;; transients — mutable backing per collection kind, snapshotted to the immutable
;; collection on persistent!. conj!/assoc!/dissoc!/disj!/pop! mutate in place
;; (amortized O(1)); persistent! converts back to a pvec / pmap / pset once.
;;
;;   vec : a growable Scheme vector (capacity) + a fill count `n`. conj!/pop! are
;;         O(1) amortized — the old copy-on-write rebuilt the whole vector per op,
;;         so building an N-vector was O(N^2).
;;   map : a Chez hashtable keyed by key-hash / jolt= (value-equality, nil-safe —
;;         a jolt-nil key stores fine here).
;;   set : a Chez hashtable of elements.
;;   cow : fallback for anything else (e.g. a sorted coll) — copy-on-write over
;;         the persistent ops, preserving jolt's superset of Clojure's transients.
;;
;; get/count/contains?/nth see THROUGH a transient (frequencies/group-by read a
;; transient map; a transient is callable). vector? on a transient is false (it's
;; this record, not a pvec), which group-by relies on. Loaded after collections.ss
;; (persistent ops + key-hash) and converters.ss.

(define-record-type jolt-transient
  (fields kind (mutable buf) (mutable n) (mutable active))
  (nongenerative jolt-transient-v2))

(define tvec-min-cap 8)

(define (jolt-transient-new coll)
  (cond
    ((pvec? coll)
     (let* ((v (pvec-v coll)) (cnt (vector-length v)) (cap (fxmax tvec-min-cap cnt))
            (buf (make-vector cap jolt-nil)))
       (let loop ((i 0)) (when (fx<? i cnt) (vector-set! buf i (vector-ref v i)) (loop (fx+ i 1))))
       (make-jolt-transient 'vec buf cnt #t)))
    ((pmap? coll)
     (let ((ht (make-hashtable key-hash jolt=2)))
       (pmap-fold coll (lambda (k v acc) (hashtable-set! ht k v) acc) 0)
       (make-jolt-transient 'map ht 0 #t)))
    ((pset? coll)
     (let ((ht (make-hashtable key-hash jolt=2)))
       (pset-fold coll (lambda (e acc) (hashtable-set! ht e #t) acc) 0)
       (make-jolt-transient 'set ht 0 #t)))
    (else (make-jolt-transient 'cow coll 0 #t))))

(define (jolt-trans-check t who)
  (unless (jolt-transient? t) (error #f (string-append who ": not a transient") t))
  (unless (jolt-transient-active t) (error #f (string-append who ": transient used after persistent!"))))

;; --- persistent! : snapshot back to the immutable collection -----------------
(define (jolt-persistent! t)
  (jolt-trans-check t "persistent!")
  (jolt-transient-active-set! t #f)
  (case (jolt-transient-kind t)
    ((vec)
     (let ((buf (jolt-transient-buf t)) (cnt (jolt-transient-n t)))
       ;; exact fit: hand off the buffer (no other reference exists, and the
       ;; transient is now inactive so it can't mutate it). Else trim to size —
       ;; a pvec's backing length must equal its count.
       (if (fx=? cnt (vector-length buf))
           (make-pvec buf)
           (let ((out (make-vector cnt)))
             (let loop ((i 0))
               (if (fx<? i cnt) (begin (vector-set! out i (vector-ref buf i)) (loop (fx+ i 1)))
                   (make-pvec out)))))))
    ((map)
     (let ((ht (jolt-transient-buf t)) (m empty-pmap))
       (vector-for-each (lambda (k) (set! m (pmap-assoc m k (hashtable-ref ht k jolt-nil)))) (hashtable-keys ht))
       m))
    ((set)
     (let ((ht (jolt-transient-buf t)) (s empty-pset))
       (vector-for-each (lambda (e) (set! s (pset-conj s e))) (hashtable-keys ht))
       s))
    (else (jolt-transient-buf t))))

;; --- in-place mutation -------------------------------------------------------
(define (tvec-ensure! t need)            ; grow capacity to >= need by doubling
  (let ((buf (jolt-transient-buf t)))
    (when (fx>? need (vector-length buf))
      (let* ((ncap (let grow ((c (fxmax tvec-min-cap (vector-length buf)))) (if (fx>=? c need) c (grow (fx* 2 c)))))
             (nbuf (make-vector ncap jolt-nil)) (cnt (jolt-transient-n t)))
        (let loop ((i 0)) (when (fx<? i cnt) (vector-set! nbuf i (vector-ref buf i)) (loop (fx+ i 1))))
        (jolt-transient-buf-set! t nbuf)))))
(define (tvec-conj1! t x)
  (let ((cnt (jolt-transient-n t)))
    (tvec-ensure! t (fx+ cnt 1))
    (vector-set! (jolt-transient-buf t) cnt x)
    (jolt-transient-n-set! t (fx+ cnt 1))))
(define (tvec-assoc1! t i x)
  (let ((i (->idx i)) (cnt (jolt-transient-n t)))
    (cond ((and (fixnum? i) (fx>=? i 0) (fx<? i cnt)) (vector-set! (jolt-transient-buf t) i x))
          ((and (fixnum? i) (fx=? i cnt)) (tvec-conj1! t x))
          (else (error #f "assoc!: index out of bounds")))))
;; conj! onto a transient map: a [k v] pair (vector/map-entry) or a whole map.
(define (tmap-conj-entry! t x)
  (cond
    ((jolt-nil? x) #t)
    ((pvec? x) (hashtable-set! (jolt-transient-buf t) (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil)))
    ((pmap? x) (pmap-fold x (lambda (k v acc) (hashtable-set! (jolt-transient-buf t) k v) acc) 0))
    (else (error #f "conj!: a transient map takes a map entry or a map" x))))

;; (conj!) -> fresh transient vector; (conj! coll) -> the 1-arity transducer-
;; completion identity (JVM: no transient check). (conj! t x ...) mutates t.
(define (jolt-conj! . args)
  (cond
    ((null? args) (jolt-transient-new (jolt-vector)))
    ((null? (cdr args)) (car args))
    (else
      (let ((t (car args)) (xs (cdr args)))
        (jolt-trans-check t "conj!")
        (case (jolt-transient-kind t)
          ((vec) (for-each (lambda (x) (tvec-conj1! t x)) xs))
          ((set) (for-each (lambda (x) (hashtable-set! (jolt-transient-buf t) x #t)) xs))
          ((map) (for-each (lambda (x) (tmap-conj-entry! t x)) xs))
          (else (jolt-transient-buf-set! t (apply jolt-conj (jolt-transient-buf t) xs))))
        t))))

;; assoc! is variadic. JVM: a complete first key/val pair present (>=3 kvs) with a
;; trailing lone key fills nil; a lone key alone (1 kv) is a wrong-arity throw.
(define (assoc-pad kvs) (if (and (>= (length kvs) 3) (odd? (length kvs))) (append kvs (list jolt-nil)) kvs))
(define (jolt-assoc! t . kvs0)
  (jolt-trans-check t "assoc!")
  (let ((kvs (assoc-pad kvs0)))
    (when (odd? (length kvs)) (error #f "assoc!: no value supplied for key"))
    (case (jolt-transient-kind t)
      ((map) (let lp ((xs kvs)) (unless (null? xs) (hashtable-set! (jolt-transient-buf t) (car xs) (cadr xs)) (lp (cddr xs)))))
      ((vec) (let lp ((xs kvs)) (unless (null? xs) (tvec-assoc1! t (car xs) (cadr xs)) (lp (cddr xs)))))
      (else (jolt-transient-buf-set! t (apply jolt-assoc (jolt-transient-buf t) kvs)))))
  t)
(define (jolt-dissoc! t . ks)
  (jolt-trans-check t "dissoc!")
  (case (jolt-transient-kind t)
    ((map) (for-each (lambda (k) (hashtable-delete! (jolt-transient-buf t) k)) ks))
    (else (jolt-transient-buf-set! t (apply jolt-dissoc (jolt-transient-buf t) ks))))
  t)
(define (jolt-disj! t . xs)
  (jolt-trans-check t "disj!")
  (case (jolt-transient-kind t)
    ((set) (for-each (lambda (x) (hashtable-delete! (jolt-transient-buf t) x)) xs))
    (else (jolt-transient-buf-set! t (apply jolt-disj (jolt-transient-buf t) xs))))
  t)
(define (jolt-pop! t)
  (jolt-trans-check t "pop!")
  (case (jolt-transient-kind t)
    ((vec) (let ((cnt (jolt-transient-n t)))
             (if (fx=? cnt 0) (error #f "pop!: can't pop empty transient vector")
                 (jolt-transient-n-set! t (fx- cnt 1)))))
    (else (jolt-transient-buf-set! t (jolt-pop (jolt-transient-buf t)))))
  t)

;; persistent disj over sets (pset-disj already exists in collections.ss).
(define (jolt-disj s . xs)
  (meta-carry s
    (let loop ((s s) (xs xs)) (if (null? xs) s (loop (pset-disj s (car xs)) (cdr xs))))))

;; --- see-through accessors ---------------------------------------------------
(define (tvec-in-bounds? t i) (and (fixnum? i) (fx>=? i 0) (fx<? i (jolt-transient-n t))))
(define (t-get t k d)
  (case (jolt-transient-kind t)
    ((vec) (let ((i (->idx k))) (if (tvec-in-bounds? t i) (vector-ref (jolt-transient-buf t) i) d)))
    ((map) (hashtable-ref (jolt-transient-buf t) k d))
    ((set) (if (hashtable-contains? (jolt-transient-buf t) k) k d))
    (else (%prev-jolt-get (jolt-transient-buf t) k d))))
(define (t-count t)
  (case (jolt-transient-kind t)
    ((vec) (jolt-transient-n t))
    ((map set) (hashtable-size (jolt-transient-buf t)))
    (else (%prev-jolt-count (jolt-transient-buf t)))))
(define (t-contains? t k)
  (case (jolt-transient-kind t)
    ((vec) (tvec-in-bounds? t (->idx k)))
    ((map set) (hashtable-contains? (jolt-transient-buf t) k))
    (else (%prev-jolt-contains? (jolt-transient-buf t) k))))

;; Redefine the native get/count/contains?/nth (captured first) so the existing
;; emit lowerings unwrap a transient; non-transients are untouched.
(register-get-arm! jolt-transient? (lambda (coll k d) (t-get coll k d)))
(define %prev-jolt-count jolt-count)
(set! jolt-count (lambda (coll) (if (jolt-transient? coll) (t-count coll) (%prev-jolt-count coll))))
(define %prev-jolt-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k) (if (jolt-transient? coll) (t-contains? coll k) (%prev-jolt-contains? coll k))))
(define %prev-jolt-nth jolt-nth)
(set! jolt-nth
  (case-lambda
    ((coll i)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i)))
               (if (tvec-in-bounds? coll idx) (vector-ref (jolt-transient-buf coll) idx) (error 'nth "index out of bounds")))
             (%prev-jolt-nth (jolt-transient-buf coll) i))
         (%prev-jolt-nth coll i)))
    ((coll i d)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i))) (if (tvec-in-bounds? coll idx) (vector-ref (jolt-transient-buf coll) idx) d))
             (%prev-jolt-nth (jolt-transient-buf coll) i d))
         (%prev-jolt-nth coll i d)))))

(def-var! "clojure.core" "transient" jolt-transient-new)
(def-var! "clojure.core" "transient?" jolt-transient?)
(def-var! "clojure.core" "persistent!" jolt-persistent!)
(def-var! "clojure.core" "conj!" jolt-conj!)
(def-var! "clojure.core" "assoc!" jolt-assoc!)
(def-var! "clojure.core" "dissoc!" jolt-dissoc!)
(def-var! "clojure.core" "disj!" jolt-disj!)
(def-var! "clojure.core" "pop!" jolt-pop!)
(def-var! "clojure.core" "disj" jolt-disj)
