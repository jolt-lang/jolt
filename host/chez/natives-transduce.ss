;; natives-transduce.ss — the transducer surface: volatiles, the `cat` transducer,
;; sequence / transduce application, and the chunked-seq builder API.
;;
;; `sequence` and `transduce` are seed natives. The stateful transducer arities
;; (take-nth/map-indexed/partition-by/dedupe/distinct, all overlay) use
;; volatile!/vreset!/deref, shimmed here (vswap! is a macro over them).
;;
;; Volatiles are a native mutable box (jvol) — the overlay vreset! drives a
;; volatile through jolt.host/ref-put!+get, but a Chez volatile is a record, not a
;; tagged table, so those overlay versions are overridden natively in
;; post-prelude.ss. transduce/sequence build on the existing into-xform / reduce-
;; seq machinery (natives-seq.ss / seq.ss). Loaded after those + atoms.ss (deref).

;; --- volatiles ---------------------------------------------------------------
(define-record-type jvol (fields (mutable v)) (nongenerative chez-jvol-v1))
(define (jolt-volatile! x) (make-jvol x))
(define (jolt-vreset! vol x) (jvol-v-set! vol x) x)
(define (jolt-volatile-pred? x) (jvol? x))
(register-deref-print! jvol? (lambda (v) (cons "ready" (jvol-v v))))
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
;; SRC is one collection, or with MULTI? a list of them. The single source keeps
;; its own loop, so the common (sequence xform coll) pays nothing for the lockstep
;; walk's per-element lists.
(define (sequence-xf xform src0 multi?)
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
          ((not multi?)
           (let ((s (jolt-seq src)))
             (if (jolt-nil? s)
                 (begin (jolt-invoke xrf acc) (loop src acc #t))   ; complete -> flush
                 (loop (seq-more s) (jolt-invoke xrf acc (seq-first s)) completed))))
          (else
           ;; several sources, stepped in lockstep; the first one to run dry
           ;; completes the whole thing, so the shortest collection wins.
           (let ((ss (map jolt-seq src)))
             (if (let any-dry ((l ss)) (and (pair? l) (or (jolt-nil? (car l)) (any-dry (cdr l)))))
                 (begin (jolt-invoke xrf acc) (loop src acc #t))   ; complete -> flush
                 (loop (map seq-more ss)
                       (apply jolt-invoke xrf acc (map seq-first ss))
                       completed)))))))
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
    (step src0 jolt-nil #f jolt-empty-list)))

;; The 1-arity is `seq` except that an empty source yields () rather than nil, and
;; an argument that is already a seq is handed back untouched.
(define jolt-sequence
  (case-lambda
    ((coll) (if (jolt-seq? coll)
                coll
                (let ((s (jolt-seq coll)))
                  (if (jolt-nil? s) jolt-empty-list s))))
    ((xform coll) (sequence-xf xform coll #f))
    ;; (sequence xform coll & colls): the xform's step fn takes one arg per coll.
    ((xform coll . colls) (sequence-xf xform (cons coll colls) #t))))

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
;; doseq's chunk walk (00-syntax.clj): the block a chunked seq cell heads, read
;; in place. The block is a run of one 32-slot leaf of the cell's backing vector
;; (a vector seq's blocks are leaf-aligned, a ChunkedCons's chunk is one small
;; vector), so __chunk-vec answers that leaf and [__chunk-start, __chunk-end)
;; the run within it, and __chunk-nth is a slot read: indexing the whole trie
;; per element cost a descent at every step of a large vector. __chunk-after is
;; the seq that follows the block, unrealized (a ChunkedCons's rest, or the
;; vector seq at the block's end). nil from __chunk-vec — the cell is not
;; chunked, or a rare block crosses a leaf — walks it with first/next instead.
;; Nothing is copied: chunk-first's block copy cost more than the walk it bought
;; on a short vector.
(define (doseq-block-end s)
  (let ((v (cseq-cvec s)))
    (if (cseq-crest s) (pvec-count v) (fxmin (fx+ (cseq-ci s) seq-chunk-size) (pvec-count v)))))
;; Asked once of every seq a doseq walks, so the sealed cseqv? goes first: a
;; list's cell (a cseqn) or a lazy seq answers nil on one record-type compare,
;; where cseq? walked the parent chain and the kind test ran before it.
(define (doseq-chunk-vec s)
  (if (and (cseqv? s) (sk-chunked? (cseq-kind s)))
      (let ((v (cseq-cvec s)) (i (cseq-ci s)))
        (if (fx>=? i (pv-tailoff-of v))
            (pvec-tail v)               ; a short vector is all tail: no descent
            (let-values (((node off) (pv-leaf-for v i)))
              (if (fx<=? (fx+ off (fx- (doseq-block-end s) i)) (vector-length node)) node jolt-nil))))
      jolt-nil))
;; where the block starts in its leaf, as pv-leaf-for finds it, without the
;; descent: the tail starts at the tail offset, a classic trie's leaf on a 32
;; boundary; a relaxed (RRB) trie's leaves are anywhere, so it asks
(define (doseq-chunk-start s)
  (let* ((v (cseq-cvec s)) (i (cseq-ci s)) (t (pv-tailoff-of v)))
    (cond ((fx>=? i t) (fx- i t))
          ((rrbnode? (pvec-root v)) (let-values (((node off) (pv-leaf-for v i))) off))
          (else (fxand i pv-mask)))))
(define (doseq-chunk-end s) (fx+ (doseq-chunk-start s) (fx- (doseq-block-end s) (cseq-ci s))))
(define (doseq-chunk-after s)
  (let ((r (cseq-crest s)))
    (if r r (vec->seq/k (cseq-cvec s) (doseq-block-end s) (cseq-kind s)))))
(define (doseq-chunk-nth node i) (vector-ref node i))
;; private core names: doseq's expansion is their only caller, and the compiler
;; lowers each to its procedure (op-registry.clj), so a walk pays no var
(let ((d! (lambda (n v) (def-var! "clojure.core" n v))))
  (d! "__chunk-vec" doseq-chunk-vec) (d! "__chunk-start" doseq-chunk-start)
  (d! "__chunk-end" doseq-chunk-end) (d! "__chunk-after" doseq-chunk-after)
  (d! "__chunk-nth" doseq-chunk-nth))
;; and private: out of ns-publics, as the reference's chunk walk names no public
;; helper of its own (host-contract.ss lets the expansion's call through)
(for-each (lambda (n) (set-var-meta! "clojure.core" n (jolt-hash-map (keyword #f "private") #t)))
  '("__chunk-vec" "__chunk-start" "__chunk-end" "__chunk-after" "__chunk-nth"))
(let ((d! (lambda (n v) (def-var! "clojure.core" n v))))
  (d! "chunk-buffer" na-chunk-buffer) (d! "chunk-append" na-chunk-append)
  (d! "chunk" na-chunk) (d! "chunk-cons" na-chunk-cons)
  (d! "chunk-first" na-chunk-first) (d! "chunk-rest" na-chunk-rest)
  (d! "chunk-next" na-chunk-next) (d! "chunked-seq?" na-chunked-seq?))
