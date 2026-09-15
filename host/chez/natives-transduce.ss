;; natives-transduce.ss — the transducer surface: volatiles, the `cat` transducer,
;; sequence / transduce application, and the chunked-seq builder API.
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
;; Fixed 2/3/4-arity clauses for the same reason swap! has them (atoms.ss): every
;; stateful transducer step is a (vswap! n inc) or (vswap! buf conj x), and a
;; single `. args` signature charged each one a rest list plus an apply. The
;; reference's vswap! is a macro and pays neither.
(define jolt-vswap!
  (case-lambda
    ((vol f) (let ((nv (jolt-invoke1 f (jvol-v vol)))) (jvol-v-set! vol nv) nv))
    ((vol f x) (let ((nv (jolt-invoke2 f (jvol-v vol) x))) (jvol-v-set! vol nv) nv))
    ((vol f x y) (let ((nv (jolt-invoke3 f (jvol-v vol) x y))) (jvol-v-set! vol nv) nv))
    ((vol f . args)
     (let ((nv (apply jolt-invoke f (jvol-v vol) args))) (jvol-v-set! vol nv) nv))))
(define (jolt-volatile-pred? x) (jvol? x))
;; deref reads a volatile too (partition-all/-by transducers @-deref their box).
(define %xf-deref jolt-deref)
(set! jolt-deref (lambda (x) (if (jvol? x) (jvol-v x) (%xf-deref x))))

(def-var! "clojure.core" "volatile!" jolt-volatile!)
(def-var! "clojure.core" "deref" jolt-deref)

;; --- sequence ----------------------------------------------------------------
;; transduce lives in the overlay (clojure/core/22-coll.clj): it's a pure
;; composition (xf (reduce xf init coll)) over reduce, so the Clojure version
;; lowers to the same code the native shim did. sequence stays native (below):
;; its transformer iterator drives the reduced box + lazy realization directly.

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

;; The 1-arity is `seq` except that an empty source yields () rather than nil, and
;; an argument that is already a seq is handed back untouched.
(define jolt-sequence
  (case-lambda
    ((coll) (if (jolt-seq? coll)
                coll
                (let ((s (jolt-seq coll)))
                  (if (jolt-nil? s) jolt-empty-list s))))
    ((xform coll) (sequence-xf xform coll))))

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

;; --- chunked seqs -----------------------------------------------------------
;; The chunked-seq accessors (chunked-seq? / chunk-first / chunk-rest / chunk-next)
;; live in seq.ss with the cseq core they read; here we only bind them plus the
;; chunk-builder API (clojure.lang.ChunkBuffer + chunk-cons). chunk-buffer
;; collects appended items, chunk seals them into a pvec chunk, and chunk-cons
;; prepends that chunk onto a rest seq as a real ChunkedCons (cseq-chunked) —
;; empty chunk == just the rest, like clojure.core/chunk-cons.
;; Here rather than in java/natives-array.ss: core's chunked map/filter/keep
;; read these vars, and this file is shared with the Gambit boot where the
;; java/ tree is not — bound there, every `defn` on Gambit died on chunk-first.
;; The buffer is a vector sized by cap (32 everywhere in core) + a fill count, so
;; an append is one vector-set! — it was a per-item (append items (list x)) list
;; copy, O(n^2) over a chunk's life with cap ignored (`make chunkscaling` gates
;; the shape). Appends past cap grow the vector: the JVM ChunkBuffer throws
;; there, and growing is the documented jolt superset (test/chez/unit.edn
;; "chunk-builder overflow"). Sealing copies, so the buffer stays appendable
;; after chunk — matching the list implementation, where the JVM nulls it.
(define-record-type jolt-chunkbuf (fields (mutable vec) (mutable cnt)) (nongenerative jolt-chunkbuf-v2))
(define (na-chunk-buffer cap)
  (make-jolt-chunkbuf (make-vector (if (and (fixnum? cap) (fx>? cap 0)) cap 32)) 0))
(define (na-chunk-append b x)
  (let ((v (jolt-chunkbuf-vec b)) (n (jolt-chunkbuf-cnt b)))
    (let ((v (if (fx=? n (vector-length v))
                 (let ((w (make-vector (fx* 2 n))))
                   (let copy ((i 0)) (when (fx<? i n) (vector-set! w i (vector-ref v i)) (copy (fx+ i 1))))
                   (jolt-chunkbuf-vec-set! b w)
                   w)
                 v)))
      (vector-set! v n x)
      (jolt-chunkbuf-cnt-set! b (fx+ n 1))))
  b)
(define (na-chunk b)
  (let* ((n (jolt-chunkbuf-cnt b)) (v (jolt-chunkbuf-vec b)) (out (make-vector n)))
    (let copy ((i 0)) (when (fx<? i n) (vector-set! out i (vector-ref v i)) (copy (fx+ i 1))))
    (make-pvec out)))
(define (na-chunk-cons chunk rest)
  (if (fx=? 0 (pvec-count chunk)) rest (cseq-chunked chunk 0 rest)))
;; the buffer is clojure.lang.ChunkBuffer, a Counted: count reads its fill
(register-class-arm! jolt-chunkbuf? (lambda (b) "clojure.lang.ChunkBuffer"))
(register-count-arm! jolt-chunkbuf? (lambda (b) (jolt-chunkbuf-cnt b)))
(let ((d! (lambda (n v) (def-var! "clojure.core" n v))))
  (d! "chunk-buffer" na-chunk-buffer) (d! "chunk-append" na-chunk-append)
  (d! "chunk" na-chunk) (d! "chunk-cons" na-chunk-cons)
  (d! "chunk-first" na-chunk-first) (d! "chunk-rest" na-chunk-rest)
  (d! "chunk-next" na-chunk-next) (d! "chunked-seq?" na-chunked-seq?))
