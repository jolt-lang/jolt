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

;; For a transient MAP, `n` holds the array-mode capacity (entries it can hold
;; before promoting to hash order) and `ord` the reverse insertion-order key list;
;; for a vector `n` is the element count. A transient array map promotes to hash
;; at max(8, source-count) entries (TransientArrayMap, array sized max(16, len)),
;; with no keyword exception — unlike the persistent assoc growth rule.
(define-record-type jolt-transient
  (fields kind (mutable buf) (mutable n) (mutable active) (mutable ord))
  (nongenerative jolt-transient-v3))

(define tvec-min-cap 8)
(define tmap-min-cap 8)

(define (jolt-transient-new coll)
  (cond
    ((pvec? coll)
     (let* ((v (pvec-v coll)) (cnt (vector-length v)) (cap (fxmax tvec-min-cap cnt))
            (buf (make-vector cap jolt-nil)))
       (let loop ((i 0)) (when (fx<? i cnt) (vector-set! buf i (vector-ref v i)) (loop (fx+ i 1))))
       (make-jolt-transient 'vec buf cnt #t #f)))
    ((pmap? coll)
     ;; the source's mode rides along: an array-mode map keeps `ord` (and comes
     ;; back an array map from persistent!); a hash-mode map carries ord = #f
     ;; and stays hash-ordered, like the JVM's TransientArrayMap/TransientHashMap.
     (let ((ht (make-hashtable key-hash jolt=2)) (ord (if (pmap-order coll) '() #f)) (cnt 0))
       ;; visit in iteration order so `ord` ends up reverse-insertion (persistent! reverses it back)
       (pmap-fold-fwd coll (lambda (k v acc) (hashtable-set! ht k v) (when ord (set! ord (cons k ord))) (set! cnt (fx+ cnt 1)) acc) 0)
       (make-jolt-transient 'map ht (fxmax tmap-min-cap cnt) #t ord)))
    ((pset? coll)
     (let ((ht (make-hashtable key-hash jolt=2)))
       (pset-fold coll (lambda (e acc) (hashtable-set! ht e #t) acc) 0)
       (make-jolt-transient 'set ht 0 #t #f)))
    ;; a deftype implementing clojure.lang.IEditableCollection.asTransient
    ;; (flatland's OrderedMap/OrderedSet) returns its OWN transient type, which
    ;; drives its declared ITransient* methods — not the copy-on-write wrapper.
    ;; find-method-any-protocol is a forward ref to records.ss (bound by call time).
    ((and (jrec? coll) (find-method-any-protocol (jrec-tag coll) "asTransient"))
     => (lambda (m) (jolt-invoke m coll)))
    ;; RFC 0003: any COLLECTION transients (the sorted/list/seq superset rides
    ;; the copy-on-write fallback); a non-collection is the JVM's cast failure.
    ((or (cseq? coll) (empty-list-t? coll) (jolt-lazyseq? coll)
         (htable? coll) (jrec? coll))
     (make-jolt-transient 'cow coll 0 #t #f))
    (else
     (jolt-throw (jolt-host-throwable
                  "java.lang.ClassCastException"
                  (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                 " cannot be cast to class clojure.lang.IEditableCollection"))))))

;; map put/delete that maintain the reverse insertion-order list in `ord`.
(define (tmap-put! t k v)
  (let ((ht (jolt-transient-buf t)))
    (unless (or (not (jolt-transient-ord t)) (hashtable-contains? ht k))
      (jolt-transient-ord-set! t (cons k (jolt-transient-ord t))))
    (hashtable-set! ht k v)))
(define (tmap-del! t k)
  (let ((ht (jolt-transient-buf t)))
    (when (and (jolt-transient-ord t) (hashtable-contains? ht k))
      (jolt-transient-ord-set! t (remove-key (jolt-transient-ord t) k)))
    (hashtable-delete! ht k)))

(define (jolt-trans-check t who)
  (unless (jolt-transient? t) (throw-jvm (quote ClassCastException) (string-append who ": not a transient")))
  (unless (jolt-transient-active t)
    (jolt-throw (jolt-host-throwable "java.lang.IllegalAccessError"
                  (string-append who ": transient used after persistent!")))))

;; --- persistent! : snapshot back to the immutable collection -----------------
;; A deftype implementing the clojure.lang.ITransient* interfaces (flatland's
;; TransientOrderedMap/Set) is a plain jrec, not a jolt-transient — the transient
;; ops route to its declared methods. jrec?/find-method-any-protocol/jolt-invoke
;; are forward refs bound by call time.
(define (jrec-trans-method t name) (and (jrec? t) (find-method-any-protocol (jrec-tag t) name)))

(define (jolt-persistent! t)
  (cond
    ((jrec-trans-method t "persistent") => (lambda (m) (jolt-invoke m t)))
    (else
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
       (let* ((ht (jolt-transient-buf t)) (cnt (hashtable-size ht)) (cap (jolt-transient-n t))
              ;; Clojure 1.13: a keyword-only map stays an array map up to 64 entries,
              ;; so a keyword map built through a transient (into {} …) keeps insertion
              ;; order to 64, matching the literal/assoc paths.
              (cap (if (and (jolt-transient-ord t) (all-keywords? (jolt-transient-ord t)))
                       (fxmax array-map-limit-kw cap) cap)))
         (if (or (not (jolt-transient-ord t)) (fx>? cnt cap))
            ;; promoted past the array capacity: hash order
            (let ((m empty-pmap-hash))
              (vector-for-each (lambda (k) (set! m (pmap-put-hash m k (hashtable-ref ht k jolt-nil)))) (hashtable-keys ht))
              m)
            ;; array map: rebuild in insertion order
            (let ((m empty-pmap))
              (for-each (lambda (k) (set! m (pmap-put-ordered m k (hashtable-ref ht k jolt-nil))))
                        (reverse (jolt-transient-ord t)))
              m))))
    ((set)
     (let ((ht (jolt-transient-buf t)) (s empty-pset))
       (vector-for-each (lambda (e) (set! s (pset-conj s e))) (hashtable-keys ht))
       s))
    (else (jolt-transient-buf t))))))

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
          (else (throw-jvm (quote IndexOutOfBoundsException) "assoc!: index out of bounds")))))
;; conj! onto a transient map: a [k v] pair (vector/map-entry) or a whole map.
(define (tmap-conj-entry! t x)
  (cond
    ((jolt-nil? x) #t)
    ((pvec? x) (tmap-put! t (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil)))
    ((pmap? x) (pmap-fold-fwd x (lambda (k v acc) (tmap-put! t k v) acc) 0))
    (else (throw-jvm (quote IllegalArgumentException) "conj!: a transient map takes a map entry or a map"))))

;; (conj!) -> fresh transient vector; (conj! coll) -> the 1-arity transducer-
;; completion identity (JVM: no transient check). (conj! t x ...) mutates t.
(define (jolt-conj! . args)
  (cond
    ((null? args) (jolt-transient-new (jolt-vector)))
    ((null? (cdr args)) (car args))
    (else
      (let ((t (car args)) (xs (cdr args)))
        (cond
          ((jrec-trans-method t "conj")
           => (lambda (m) (fold-left (lambda (acc x) (jolt-invoke m acc x)) t xs)))
          (else
        (jolt-trans-check t "conj!")
        (case (jolt-transient-kind t)
          ((vec) (for-each (lambda (x) (tvec-conj1! t x)) xs))
          ((set) (for-each (lambda (x) (hashtable-set! (jolt-transient-buf t) x #t)) xs))
          ((map) (for-each (lambda (x) (tmap-conj-entry! t x)) xs))
          (else (jolt-transient-buf-set! t (apply jolt-conj (jolt-transient-buf t) xs))))
        t))))))

;; assoc! is variadic. JVM: a complete first key/val pair present (>=3 kvs) with a
;; trailing lone key fills nil; a lone key alone (1 kv) is a wrong-arity throw.
(define (assoc-pad kvs) (if (and (>= (length kvs) 3) (odd? (length kvs))) (append kvs (list jolt-nil)) kvs))
(define (jolt-assoc! t . kvs0)
  (cond
    ((jrec-trans-method t "assoc")
     => (lambda (m) (let lp ((xs (assoc-pad kvs0)))
                      (if (null? xs) t (begin (jolt-invoke m t (car xs) (cadr xs)) (lp (cddr xs)))))))
    (else
  (jolt-trans-check t "assoc!")
  (let ((kvs (assoc-pad kvs0)))
    (when (odd? (length kvs)) (throw-jvm (quote IllegalArgumentException) "assoc!: no value supplied for key"))
    (case (jolt-transient-kind t)
      ((map) (let lp ((xs kvs)) (unless (null? xs) (tmap-put! t (car xs) (cadr xs)) (lp (cddr xs)))))
      ((vec) (let lp ((xs kvs)) (unless (null? xs) (tvec-assoc1! t (car xs) (cadr xs)) (lp (cddr xs)))))
      (else (jolt-transient-buf-set! t (apply jolt-assoc (jolt-transient-buf t) kvs)))))
  t)))
(define (jolt-dissoc! t . ks)
  (cond
    ((jrec-trans-method t "without")
     => (lambda (m) (fold-left (lambda (acc k) (jolt-invoke m acc k)) t ks)))
    (else
  (jolt-trans-check t "dissoc!")
  (case (jolt-transient-kind t)
    ((map) (for-each (lambda (k) (tmap-del! t k)) ks))
    (else (jolt-transient-buf-set! t (apply jolt-dissoc (jolt-transient-buf t) ks))))
  t)))
(define (jolt-disj! t . xs)
  (cond
    ((jrec-trans-method t "disjoin")
     => (lambda (m) (fold-left (lambda (acc x) (jolt-invoke m acc x)) t xs)))
    (else
  (jolt-trans-check t "disj!")
  (case (jolt-transient-kind t)
    ((set) (for-each (lambda (x) (hashtable-delete! (jolt-transient-buf t) x)) xs))
    (else (jolt-transient-buf-set! t (apply jolt-disj (jolt-transient-buf t) xs))))
  t)))
(define (jolt-pop! t)
  (cond
    ((jrec-trans-method t "pop") => (lambda (m) (jolt-invoke m t)))
    (else
  (jolt-trans-check t "pop!")
  (case (jolt-transient-kind t)
    ((vec) (let ((cnt (jolt-transient-n t)))
             (if (fx=? cnt 0) (throw-jvm (quote IllegalStateException) "pop!: can't pop empty transient vector")
                 (jolt-transient-n-set! t (fx- cnt 1)))))
    (else (jolt-transient-buf-set! t (jolt-pop (jolt-transient-buf t)))))
  t)))

;; persistent disj over sets (pset-disj already exists in collections.ss).
(define (jolt-disj s . xs)
  ;; (disj nil ...) is nil on the JVM (disj is otherwise set-only).
  (if (jolt-nil? s)
      jolt-nil
      (cond
        ((pset? s)
         (meta-carry s
           (let loop ((s s) (xs xs)) (if (null? xs) s (loop (pset-disj s (car xs)) (cdr xs))))))
        ;; a deftype implementing clojure.lang.IPersistentSet.disjoin (flatland's
        ;; OrderedSet) disjoins through its own method. jrec?/jrec-cl are forward
        ;; refs to records.ss (loaded after this file, bound by call time).
        ((and (jrec? s) (jrec-cl s "disjoin"))
         => (lambda (m) (meta-carry s (fold-left (lambda (acc x) (jolt-invoke m acc x)) s xs))))
        (else
         (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                       (string-append "class " (guard (e (#t "?")) (jolt-class-name s))
                                      " cannot be cast to class clojure.lang.IPersistentSet")))))))

;; --- see-through accessors ---------------------------------------------------
;; The copy-on-write ('cow) transient kind delegates reads to the plain collection
;; ops on its wrapped immutable coll (never a transient itself, so no recursion
;; through the transient get/count/contains? arms). collections.ss defines these
;; before this file loads.
(define %prev-jolt-get jolt-get)
(define %prev-jolt-count jolt-count)
(define %prev-jolt-contains? jolt-contains?)
(define (tvec-in-bounds? t i) (and (fixnum? i) (fx>=? i 0) (fx<? i (jolt-transient-n t))))
(define (t-get t k d)
  (jolt-trans-check t "get")
  (case (jolt-transient-kind t)
    ((vec) (let ((i (->idx k))) (if (tvec-in-bounds? t i) (vector-ref (jolt-transient-buf t) i) d)))
    ((map) (hashtable-ref (jolt-transient-buf t) k d))
    ((set) (if (hashtable-contains? (jolt-transient-buf t) k) k d))
    (else (%prev-jolt-get (jolt-transient-buf t) k d))))
(define (t-count t)
  (jolt-trans-check t "count")
  (case (jolt-transient-kind t)
    ((vec) (jolt-transient-n t))
    ((map set) (hashtable-size (jolt-transient-buf t)))
    (else (%prev-jolt-count (jolt-transient-buf t)))))
(define (t-contains? t k)
  (jolt-trans-check t "contains?")
  (case (jolt-transient-kind t)
    ((vec) (tvec-in-bounds? t (->idx k)))
    ((map set) (hashtable-contains? (jolt-transient-buf t) k))
    (else (%prev-jolt-contains? (jolt-transient-buf t) k))))

;; Redefine the native get/count/contains?/nth (captured first) so the existing
;; emit lowerings unwrap a transient; non-transients are untouched.
;; count/contains?/nth wrappers are collapsed into records.ss (loaded later) —
;; only the get-arm registration lives here.
(register-get-arm! jolt-transient? (lambda (coll k d) (t-get coll k d)))

(def-var! "clojure.core" "transient" jolt-transient-new)
(def-var! "clojure.core" "transient?" jolt-transient?)
(def-var! "clojure.core" "persistent!" jolt-persistent!)
(def-var! "clojure.core" "conj!" jolt-conj!)
(def-var! "clojure.core" "assoc!" jolt-assoc!)
(def-var! "clojure.core" "dissoc!" jolt-dissoc!)
(def-var! "clojure.core" "disj!" jolt-disj!)
(def-var! "clojure.core" "pop!" jolt-pop!)
(def-var! "clojure.core" "disj" jolt-disj)
