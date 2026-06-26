;; natives-transduce.ss — the transducer surface: volatiles, the `cat` transducer,
;; and sequence / transduce application.
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

;; (sequence coll) -> a seq; (sequence xform coll) -> a LAZY seq of coll transformed
;; by xform. A transformer iterator (mirrors clojure.core's TransformerIterator):
;; pull one input at a time through (xform rf), where rf buffers each emitted value;
;; emit the buffer lazily, pulling more input only when it drains. So an infinite or
;; expensive source is consumed incrementally — (first (sequence (map inc) (range)))
;; returns at once. Honors `reduced` (stop pulling) and runs the 1-arg completion to
;; flush a stateful xform (partition-all / dedupe / a trailing partition).
(define (sequence-xf xform coll)
  (let* ((buf (box '()))                  ; emitted values for the current step, reversed
         (rf (case-lambda
               (() jolt-nil)
               ((acc) acc)
               ((acc x) (set-box! buf (cons x (unbox buf))) acc)))
         (xrf (jolt-invoke xform rf)))
    ;; advance the source until buf holds output or the input is drained+completed.
    (define (fill src acc completed)
      (let loop ((src src) (acc acc) (completed completed))
        (cond
          ((pair? (unbox buf)) (values src acc completed))
          (completed (values src acc #t))
          ((jolt-reduced? acc)
           (jolt-invoke xrf (jolt-reduced-val acc))      ; completion may flush
           (loop src (jolt-reduced-val acc) #t))
          (else
           (let ((s (jolt-seq src)))
             (if (jolt-nil? s)
                 (begin (jolt-invoke xrf acc) (loop src acc #t))   ; complete -> flush
                 (loop (seq-more s) (jolt-invoke xrf acc (seq-first s)) completed)))))))
    ;; Resolve the next chunk now (one fill pulls just enough input to emit or to
    ;; exhaust), so the result is a real cseq | empty — `empty` is jolt-empty-list
    ;; at the top (so an empty result still prints "()") and jolt-nil inside a tail
    ;; (the cseq terminator). The TAILS stay lazy, so an infinite source is fine.
    (define (step src acc completed empty)
      (let-values (((src2 acc2 comp2) (fill src acc completed)))
        (let ((out (reverse (unbox buf))))
          (set-box! buf '())
          (if (null? out)
              empty
              (let build ((o out))
                (if (null? (cdr o))
                    (cseq-lazy (car o) (lambda () (step src2 acc2 comp2 jolt-nil)))
                    (cseq-lazy (car o) (lambda () (build (cdr o))))))))))
    (step coll jolt-nil #f jolt-empty-list)))

(define jolt-sequence
  (case-lambda
    ((coll) (jolt-seq coll))
    ((xform coll) (sequence-xf xform coll))))

(def-var! "clojure.core" "transduce" jolt-transduce)
(def-var! "clojure.core" "sequence" jolt-sequence)

;; --- cat ---------------------------------------------------------------------
;; cat transducer: each input item is itself a collection, concatenated into the
;; downstream reducing fn.
(define (jolt-cat rf)
  (lambda a
    (cond
      ((null? a) (jolt-invoke rf))
      ((null? (cdr a)) (jolt-invoke rf (car a)))
      (else
       (let loop ((xs (seq->list (jolt-seq (cadr a)))) (acc (car a)))
         (if (null? xs) acc (loop (cdr xs) (jolt-invoke rf acc (car xs)))))))))
(def-var! "clojure.core" "cat" jolt-cat)
