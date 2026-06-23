;; volatiles + sequence / transduce — the transducer application surface.
;;
;; `sequence` and `transduce` are seed natives. The stateful transducer arities
;; (take-nth/map-indexed/partition-by/dedupe/distinct, all overlay) use
;; volatile!/vswap!/vreset!/deref, shimmed here.
;;
;; Volatiles are a native mutable box (jvol) — the overlay vreset!/vswap! drive a
;; volatile through jolt.host/ref-put!+get, but a Chez volatile is a record, not a
;; tagged table, so those overlay versions are overridden natively in
;; post-prelude.ss. transduce/sequence build on the existing into-xform / reduce-
;; seq machinery (natives-seq.ss / seq.ss). Loaded after those + atoms.ss (deref).

;; --- volatiles ---------------------------------------------------------------
(define-record-type jvol (fields (mutable v)) (nongenerative chez-jvol-v1))
(define (jolt-volatile! x) (make-jvol x))
(define (jolt-vreset! vol x) (jvol-v-set! vol x) x)
(define (jolt-vswap! vol f . args)
  (let ((nv (apply jolt-invoke f (jvol-v vol) args))) (jvol-v-set! vol nv) nv))
(define (jolt-volatile-pred? x) (jvol? x))
;; deref reads a volatile too (partition-all/-by transducers @-deref their box).
(define %xf-deref jolt-deref)
(set! jolt-deref (lambda (x) (if (jvol? x) (jvol-v x) (%xf-deref x))))

(def-var! "clojure.core" "volatile!" jolt-volatile!)
(def-var! "clojure.core" "deref" jolt-deref)

;; --- transduce / sequence ----------------------------------------------------
;; (transduce xform f coll) / (transduce xform f init coll): build the transformed
;; reducing fn (xform f), reduce it over coll (reduce-seq honors `reduced`), then
;; run the completion (1-arg) arity. The 3-arg init defaults to (f) — the rf's
;; 0-arity, e.g. (+) = 0, (conj) = [].
(define jolt-transduce
  (case-lambda
    ((xform f coll) (jolt-transduce xform f (jolt-invoke f) coll))
    ((xform f init coll)
     (let* ((xf (jolt-invoke xform f))
            (res (reduce-seq xf init (jolt-seq coll))))
       (jolt-invoke xf res)))))

;; (sequence coll) -> a seq; (sequence xform coll) -> coll transformed by xform.
;; Materialized eagerly through into-xform then seq'd (corpus inputs are finite; a
;; fully-lazy pull is future work). Honors reduced via into-xform/reduce-seq.
(define jolt-sequence
  (case-lambda
    ((coll) (jolt-seq coll))
    ((xform coll) (jolt-seq (into-xform (jolt-vector) xform coll)))))

(def-var! "clojure.core" "transduce" jolt-transduce)
(def-var! "clojure.core" "sequence" jolt-sequence)
