;; transients (jolt-kl2l) — copy-on-write over the persistent collections.
;;
;; A first cut: each transient op rebuilds the persistent collection (no in-place
;; mutation perf), but the SEMANTICS match — the overlay's transient users (into,
;; frequencies, group-by) get correct results. get/count/contains? are extended to
;; see THROUGH a transient to its held collection (frequencies/group-by do
;; (get tm k) on a transient map). vector? on a transient vector is false (it's a
;; jolt-transient record, not a pvec), which group-by relies on. Loaded after
;; collections.ss (the persistent ops it delegates to) and converters.ss.

(define-record-type jolt-transient (fields (mutable coll) (mutable active))
  (nongenerative jolt-transient-v1))

(define (jolt-transient-new coll) (make-jolt-transient coll #t))

(define (jolt-trans-check t who)
  (unless (jolt-transient? t) (error #f (string-append who ": not a transient") t))
  (unless (jolt-transient-active t)
    (error #f (string-append who ": transient used after persistent!"))))

(define (jolt-persistent! t)
  (jolt-trans-check t "persistent!")
  (jolt-transient-active-set! t #f)
  (jolt-transient-coll t))

;; (conj!) with no args -> a fresh transient vector (Clojure); otherwise mutate t.
(define (jolt-conj! . args)
  (if (null? args)
      (jolt-transient-new (jolt-vector))
      (let ((t (car args)) (xs (cdr args)))
        (jolt-trans-check t "conj!")
        (jolt-transient-coll-set! t (apply jolt-conj (jolt-transient-coll t) xs))
        t)))
;; (assoc! t k v & kvs): variadic like Clojure (jolt-assoc already folds pairs).
(define (jolt-assoc! t . kvs)
  (jolt-trans-check t "assoc!")
  (jolt-transient-coll-set! t (apply jolt-assoc (jolt-transient-coll t) kvs))
  t)
(define (jolt-dissoc! t . ks)
  (jolt-trans-check t "dissoc!")
  (jolt-transient-coll-set! t (apply jolt-dissoc (jolt-transient-coll t) ks))
  t)
(define (jolt-disj! t . xs)
  (jolt-trans-check t "disj!")
  (jolt-transient-coll-set! t (apply jolt-disj (jolt-transient-coll t) xs))
  t)
(define (jolt-pop! t)
  (jolt-trans-check t "pop!")
  (jolt-transient-coll-set! t (jolt-pop (jolt-transient-coll t)))
  t)

;; persistent disj over sets (pset-disj already exists in collections.ss).
(define (jolt-disj s . xs)
  (let loop ((s s) (xs xs)) (if (null? xs) s (loop (pset-disj s (car xs)) (cdr xs)))))

;; get / count / contains? see through a transient. Redefine the native procedures
;; (captured first) so the existing emit lowerings (jolt-get/jolt-count/
;; jolt-contains?) unwrap a transient before delegating; non-transients are
;; untouched.
(define (jolt-deref-transient x) (if (jolt-transient? x) (jolt-transient-coll x) x))
(define %prev-jolt-get jolt-get)
(set! jolt-get
  (case-lambda
    ((coll k) (%prev-jolt-get (jolt-deref-transient coll) k))
    ((coll k d) (%prev-jolt-get (jolt-deref-transient coll) k d))))
(define %prev-jolt-count jolt-count)
(set! jolt-count (lambda (coll) (%prev-jolt-count (jolt-deref-transient coll))))
(define %prev-jolt-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k) (%prev-jolt-contains? (jolt-deref-transient coll) k)))
(define %prev-jolt-nth jolt-nth)
(set! jolt-nth
  (case-lambda
    ((coll i) (%prev-jolt-nth (jolt-deref-transient coll) i))
    ((coll i d) (%prev-jolt-nth (jolt-deref-transient coll) i d))))

(def-var! "clojure.core" "transient" jolt-transient-new)
(def-var! "clojure.core" "persistent!" jolt-persistent!)
(def-var! "clojure.core" "conj!" jolt-conj!)
(def-var! "clojure.core" "assoc!" jolt-assoc!)
(def-var! "clojure.core" "dissoc!" jolt-dissoc!)
(def-var! "clojure.core" "disj!" jolt-disj!)
(def-var! "clojure.core" "pop!" jolt-pop!)
(def-var! "clojure.core" "disj" jolt-disj)
