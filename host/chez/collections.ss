;; persistent collections on the Chez RT.
;;
;; The vector / map / set the emitted programs construct from literals and
;; operate on via the lowered leaf ops (conj/get/nth/count/assoc/...). Loaded by
;; rt.ss after values.ss; jolt=2 / jolt-hash (values.ss) call into the
;; jolt-coll? / jolt-coll=? / jolt-coll-hash hooks defined here (forward refs,
;; resolved at run time — nothing is CALLED during load).
;;
;; The persistent vector is a copy-on-write Scheme vector and the map/set are a
;; bitmap HAMT. They live in Scheme; correctness, not perf, is the gate.

;; ============================================================================
;; small immutable-vector helpers
;; ============================================================================
;; No copy here is a checked Scheme loop: the runtime compiles at
;; optimize-level 2, where such a loop pays a type and bounds check per element
;; (a 64-slot copy measured 390ns). Ranges the caller COMPUTES (vec-copy-range,
;; vec-insert, vec-remove — trie leaves and nodes) go through the target's
;; checked bulk move; the whole-vector copies below it (a node update, an
;; array-map assoc/dissoc) run an unchecked loop over 0..n with n the source's
;; own length, which on Chez beats the bulk-move primitive at every size (10
;; slots: 14 vs 19ns, 64: 62 vs 72). Not vector-copy: the vendored irregex
;; redefines that name at top level as a one-argument loop.
(define (vec-copy-range v start end)
  (let ((out (make-vector (fx- end start))))
    (sa-vector-copy-range! out 0 v start end)
    out))
(define (vec-insert v i x)            ; copy of v with x spliced in at index i
  (let* ((n (vector-length v)) (out (make-vector (fx+ n 1))))
    (sa-vector-copy-range! out 0 v 0 i)
    (vector-set! out i x)
    (sa-vector-copy-range! out (fx+ i 1) v i n)
    out))
(define (vec-remove v i)              ; copy of v with index i dropped
  (let* ((n (vector-length v)) (out (make-vector (fx- n 1))))
    (sa-vector-copy-range! out 0 v 0 i)
    (sa-vector-copy-range! out i v (fx+ i 1) n)
    out))
;; slots 0..n-1 of v into out, unchecked: out was just made at least n long
(define (vec-fill-prefix! out v n)
  (let loop ((j 0))
    (when (sa-ufx<? j n)
      (sa-uvector-set! out j (sa-uvector-ref v j))
      (loop (sa-ufx+ j 1)))))
(define (vec-set v i x)               ; functional update at index i
  (let* ((n (vector-length v)) (out (make-vector n)))
    (vec-fill-prefix! out v n)
    (vector-set! out i x)
    out))
;; the two below serve the array-mode map's k/v slot vector
(define (vec-append2 v k x)           ; copy of v with k and x appended
  (let* ((n (vector-length v)) (out (make-vector (fx+ n 2))))
    (vec-fill-prefix! out v n)
    (vector-set! out n k) (vector-set! out (fx+ n 1) x)
    out))
(define (vec-remove2 v i)             ; copy of v with slots i and i+1 dropped
  (let* ((n (vector-length v)) (out (make-vector (fx- n 2))))
    (vec-fill-prefix! out v i)
    ;; the tail, shifted down two: j runs i+2..n-1 into i..n-3
    (let loop ((j (fx+ i 2)))
      (when (sa-ufx<? j n)
        (sa-uvector-set! out (sa-ufx- j 2) (sa-uvector-ref v j))
        (loop (sa-ufx+ j 1))))
    out))

;; ============================================================================
;; persistent vector — 32-way trie + tail (Clojure's PersistentVector)
;; ============================================================================
;; cnt elements live in a trie of 32-wide nodes (root, height = shift bits) plus a
;; trailing `tail` chunk of 1..32. conj appends to the tail and, when it fills,
;; pushes it into the trie by path-copy — so conj is O(1) amortized and a linear
;; build is O(n), not the O(n^2) of a flat copy-on-write array. nth/assoc/pop are
;; O(log32 n). Trie nodes are Scheme vectors holding only their live children
;; (grown left-to-right), so a node's length is its child count.
;;
;; `ent` #t marks a MAP ENTRY (the [k v] pair seq'd out of a map). An entry has 2
;; elements (all in the tail), equals its [k v] vector and walks like one, and is
;; both vector? (Clojure's MapEntry implements IPersistentVector) and map-entry?.
;; Modifying an entry (conj/assoc/pop) yields a plain vector (ent #f).
;;
;; make-pvec and pvec-v keep the old flat-vector API: make-pvec builds a trie from
;; a Scheme vector (every existing caller still passes one) and pvec-v materializes
;; it back, so only this file's internals change.
(define pv-bits 5)
(define pv-width 32)
(define pv-mask 31)
(define pv-empty-node (vector))
(define-record-type (pvec %mk-pvec pvec?)
  (fields cnt shift root tail ent (mutable hasheq)) (nongenerative chez-pvec-v3))
(define (mk-pvec cnt shift root tail ent)
  (%mk-pvec cnt shift root tail ent 0))

;; `ent` is the vector's KIND, not just the entry flag: #f = plain vector,
;; #t = map entry (above), 'subvec = a subvec view (class
;; clojure.lang.APersistentVector$SubVector — issue #629). One shared
;; representation; only the class answer and its jch tags differ. Readers must
;; test the kind explicitly, never truthiness.
;;
;; The kind a modifying op's result carries: an entry decays to a plain vector
;; (documented above), a subvec stays a subvec — the JVM's SubVector answers
;; cons/assocN/pop with a SubVector.
(define (pv-derived-ent p) (let ((e (pvec-ent p))) (if (eq? e #t) #f e)))
;; same structure, same cached hash, a different kind (fresh identity: the meta
;; side-table keys by identity, and a subvec is a fresh nil-meta view anyway)
(define (pvec-with-ent p e)
  (if (eq? (pvec-ent p) e)
      p
      (%mk-pvec (pvec-cnt p) (pvec-shift p) (pvec-root p) (pvec-tail p) e (pvec-hasheq p))))
;; the public subvec stamp: a non-empty slice is a SubVector; an empty one is
;; RT.subvec's PersistentVector.EMPTY, so it stays plain
(define (pvec-as-subvec r)
  (if (and (pvec? r) (fx>? (pvec-cnt r) 0)) (pvec-with-ent r 'subvec) r))

;; trailing helpers over Scheme vectors used by the trie
(define (vec-snoc v x)                 ; copy v with x appended
  (let* ((n (vector-length v)) (out (make-vector (fx+ n 1))))
    (sa-vector-copy-range! out 0 v 0 n)
    (vector-set! out n x) out))
(define (vec-drop-last v) (vec-copy-range v 0 (fx- (vector-length v) 1)))
(define (vec-take v n) (vec-copy-range v 0 n))
(define (vec-set-or-snoc v i x)        ; replace index i, or append when i = length
  (let ((n (vector-length v))) (if (fx<? i n) (vec-set v i x) (vec-snoc v x))))

(define (pv-tailoff cnt)
  (if (fx<? cnt pv-width) 0 (fxsll (fxsra (fx- cnt 1) pv-bits) pv-bits)))

;; --- RRB relaxed nodes -------------------------------------------------------
;; catvec/slice (below) produce RELAXED branch nodes: children needn't be full,
;; so radix addressing gives way to a size-table search. A relaxed node is a
;; dedicated record — a regular branch is a Scheme vector of children and a leaf
;; is a chunk of ELEMENTS (which can themselves be Scheme vectors), so a record
;; is the only representation whose discrimination is total: rrbnode? and
;; vector? are disjoint, and a value is only ever treated as a node at level>0.
;; sizes are CUMULATIVE subtree counts (sizes[k] = elements in children 0..k).
;;
;; Invariants:
;;  - a pvec whose root is a plain vector is a CLASSIC PersistentVector trie
;;    (full 32-wide leaves, 32-aligned tailoff) — every pre-RRB invariant holds
;;    and every pre-RRB code path runs unchanged;
;;  - a pvec whose root is an rrbnode may be arbitrary: the size tables are
;;    authoritative and the tail is the last 0..32 elements (tailoff is
;;    cnt - tail length, NOT the aligned formula);
;;  - inside an RRB trie a plain-vector branch means LEFTWISE DENSE (all but
;;    the last child fully dense), so radix addressing on a subtree-relative
;;    index is valid from that node down.
(define-record-type rrbnode (fields sizes children) (nongenerative chez-rrbnode-v1))

;; first child whose cumulative size exceeds i (linear over <=32 slots)
(define (rrb-find-child sizes i)
  (let loop ((k 0))
    (if (fx<=? (vector-ref sizes k) i) (loop (fx+ k 1)) k)))

(define (pv-tailoff-of p) (fx- (pvec-cnt p) (vector-length (pvec-tail p))))

;; leaf resolution: the chunk holding index i and i's offset within it. The
;; classic trie keeps its radix descent (one rrbnode? test per level is the only
;; added cost); a relaxed node switches to its size table and subtree-relative
;; addressing from there down. At level 0 the low 5 bits are the offset for
;; both absolute (classic) and relative (leftwise-dense subtree) indices.
(define (pv-leaf-for p i)
  (let ((tailoff (pv-tailoff-of p)))
    (if (fx>=? i tailoff)
        (values (pvec-tail p) (fx- i tailoff))
        (let loop ((node (pvec-root p)) (level (pvec-shift p)) (i i))
          (cond
            ((fx=? level 0) (values node (fxand i pv-mask)))
            ((rrbnode? node)
             ;; a relaxed node below dense ancestors receives an unmasked index;
             ;; its in-subtree part is the low level+5 bits (dense ancestors are
             ;; aligned), and masking a subtree-relative index is the identity.
             (let* ((i (fxand i (fx- (fxsll 1 (fx+ level pv-bits)) 1)))
                    (sizes (rrbnode-sizes node))
                    (k (rrb-find-child sizes i))
                    (sub (if (fx=? k 0) i (fx- i (vector-ref sizes (fx- k 1))))))
               (loop (vector-ref (rrbnode-children node) k) (fx- level pv-bits) sub)))
            (else (loop (vector-ref node (fxand (fxsra i level) pv-mask))
                        (fx- level pv-bits) i)))))))
;; the 32-chunk Scheme vector holding index i (the tail or a trie leaf)
(define (pv-chunk-for p i)
  (let-values (((chunk off) (pv-leaf-for p i))) chunk))

;; jolt models every number as a double, so vector indices arrive as flonums —
;; coerce an integer-valued index to a Scheme fixnum before bounds math.
(define (->idx i) (if (fixnum? i) i (if (flonum? i) (exact (floor i)) i)))
(define (pvec-count p) (pvec-cnt p))
;; jolt-nth's throwing pvec read as its own entry: the base definition's pvec
;; arm and the fast path hoisted in front of the set! wrapper chain
;; (natives-array.ss, the outermost link) share this ONE copy. The nil-index
;; raise and ->idx coercion lived in jolt-nth ahead of type dispatch; a
;; hoisted pvec case never reaches them, so both happen here, inlined — this
;; is the hot read, every call frame in front of pv-leaf-for costs.
;; The tail is tested HERE, before pv-leaf-for: every vector of 32 or fewer
;; elements is all tail, and a tail index is one subtraction and a vector-ref,
;; where pv-leaf-for's two-value return through let-values costs 9 of the 13 ns
;; a small-vector nth used to take. The trie path is unchanged.
(define (pvec-nth-in-range p i)
  (let* ((tail (pvec-tail p)) (tailoff (fx- (pvec-cnt p) (vector-length tail))))
    (if (fx>=? i tailoff)
        (vector-ref tail (fx- i tailoff))
        (let-values (((chunk off) (pv-leaf-for p i))) (vector-ref chunk off)))))
(define (pvec-nth! p i)
  (if (jolt-nil? i)
      (jolt-throw (jolt-host-throwable "java.lang.NullPointerException" "nth index"))
      (let ((i (if (fixnum? i) i (if (flonum? i) (exact (floor i)) i))))
        (if (and (fixnum? i) (fx>=? i 0) (fx<? i (pvec-cnt p)))
            (pvec-nth-in-range p i)
            (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "index out of bounds"))))))
(define (pvec-nth-d p i d)
  (let ((i (->idx i)))
    (if (and (fixnum? i) (fx>=? i 0) (fx<? i (pvec-cnt p)))
        (pvec-nth-in-range p i)
        d)))

;; new-path: wrap a node in single-child nodes up `level` bits.
(define (pv-new-path level node)
  (if (fx=? level 0) node (vector (pv-new-path (fx- level pv-bits) node))))
;; push a full tail chunk into the trie under `parent` at `level`.
(define (pv-push-tail cnt level parent tail-node)
  (let ((subidx (fxand (fxsra (fx- cnt 1) level) pv-mask)))
    (if (fx=? level pv-bits)
        (vec-set-or-snoc parent subidx tail-node)
        (let ((child (and (fx<? subidx (vector-length parent)) (vector-ref parent subidx))))
          (vec-set-or-snoc parent subidx
            (if child (pv-push-tail cnt (fx- level pv-bits) child tail-node)
                      (pv-new-path (fx- level pv-bits) tail-node)))))))
(define (pvec-conj p x)
  (let ((cnt (pvec-cnt p)) (shift (pvec-shift p)))
    (cond
      ((fx<? (vector-length (pvec-tail p)) pv-width)
       ;; room in the tail (classic and RRB alike)
       (mk-pvec (fx+ cnt 1) shift (pvec-root p) (vec-snoc (pvec-tail p) x) (pv-derived-ent p)))
      ((rrbnode? (pvec-root p))
       ;; tail full under a relaxed root: push it down the rightmost spine
       (let* ((tail-node (pvec-tail p))
              (pushed (rrb-push-leaf (pvec-root p) shift tail-node)))
         (if pushed
             (mk-pvec (fx+ cnt 1) shift pushed (vector x) (pv-derived-ent p))
             (let ((trie-cnt (fx- cnt pv-width)))   ; count already in the trie
               (mk-pvec (fx+ cnt 1) (fx+ shift pv-bits)
                        (make-rrbnode (vector trie-cnt cnt)
                                      (vector (pvec-root p) (pv-new-path shift tail-node)))
                        (vector x) (pv-derived-ent p))))))
      (else
       ;; tail full: push it into the classic trie, start a fresh tail
       (let ((tail-node (pvec-tail p)))
         (if (fx>? (fxsra cnt pv-bits) (fxsll 1 shift))
             ;; root overflow: grow the trie a level
             (mk-pvec (fx+ cnt 1) (fx+ shift pv-bits)
                      (vector (pvec-root p) (pv-new-path shift tail-node))
                      (vector x) (pv-derived-ent p))
             (mk-pvec (fx+ cnt 1) shift
                      (pv-push-tail cnt shift (pvec-root p) tail-node)
                      (vector x) (pv-derived-ent p))))))))

(define (pv-assoc-trie level node i x)
  (cond
    ((fx=? level 0) (vec-set node (fxand i pv-mask) x))
    ((rrbnode? node)
     (let* ((i (fxand i (fx- (fxsll 1 (fx+ level pv-bits)) 1)))   ; see pv-leaf-for
            (sizes (rrbnode-sizes node))
            (k (rrb-find-child sizes i))
            (sub (if (fx=? k 0) i (fx- i (vector-ref sizes (fx- k 1)))))
            (children (rrbnode-children node)))
       (make-rrbnode sizes
                     (vec-set children k
                              (pv-assoc-trie (fx- level pv-bits) (vector-ref children k) sub x)))))
    (else
     (let ((subidx (fxand (fxsra i level) pv-mask)))
       (vec-set node subidx (pv-assoc-trie (fx- level pv-bits) (vector-ref node subidx) i x))))))
(define (pvec-assoc p i x)            ; i in [0,count]; =count appends
  (let ((i (->idx i)) (cnt (pvec-cnt p)))
    (cond
      ((fx=? i cnt) (pvec-conj p x))
      ((and (fx>=? i 0) (fx<? i cnt))
       (let ((tailoff (pv-tailoff-of p)))
         (if (fx>=? i tailoff)
             (mk-pvec cnt (pvec-shift p) (pvec-root p)
                      (vec-set (pvec-tail p) (fx- i tailoff) x) (pv-derived-ent p))
             (mk-pvec cnt (pvec-shift p)
                      (pv-assoc-trie (pvec-shift p) (pvec-root p) i x) (pvec-tail p) (pv-derived-ent p)))))
      (else (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "vector index out of bounds"))))))
(define (pvec-peek p)
  (let ((n (pvec-cnt p))) (if (fx=? n 0) jolt-nil (pvec-nth-d p (fx- n 1) jolt-nil))))
;; pop the last trie chunk back into the tail; #f means the subtree emptied.
(define (pv-pop-tail cnt level node)
  (let ((subidx (fxand (fxsra (fx- cnt 2) level) pv-mask)))
    (cond
      ((fx>? level pv-bits)
       (let ((newchild (pv-pop-tail cnt (fx- level pv-bits) (vector-ref node subidx))))
         (cond ((and (not newchild) (fx=? subidx 0)) #f)
               (newchild (vec-set node subidx newchild))
               (else (vec-take node subidx)))))
      ((fx=? subidx 0) #f)
      (else (vec-take node subidx)))))
(define (pvec-pop p)
  (let ((cnt (pvec-cnt p)) (shift (pvec-shift p)))
    (cond
      ((fx=? cnt 0) (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "Can't pop empty vector")))
      ((fx=? cnt 1) empty-pvec)
      ((fx>? (vector-length (pvec-tail p)) 1)
       (mk-pvec (fx- cnt 1) shift (pvec-root p) (vec-drop-last (pvec-tail p)) (pv-derived-ent p)))
      ((rrbnode? (pvec-root p))
       ;; relaxed trie: pop is a slice — O(log n) via the take machinery;
       ;; re-stamp the kind the slice result should carry (slice is unstamped)
       (pvec-with-ent (pvec-slice p 0 (fx- cnt 1)) (pv-derived-ent p)))
      (else
       (let* ((new-tail (pv-chunk-for p (fx- cnt 2)))
              (popped (pv-pop-tail cnt shift (pvec-root p)))
              (new-root (or popped pv-empty-node)))
         (if (and (fx>? shift pv-bits) (fx<? (vector-length new-root) 2))
             (mk-pvec (fx- cnt 1) (fx- shift pv-bits)
                      (if (fx=? 0 (vector-length new-root)) pv-empty-node (vector-ref new-root 0))
                      new-tail (pv-derived-ent p))
             (mk-pvec (fx- cnt 1) shift new-root new-tail (pv-derived-ent p))))))))

(define empty-pvec (mk-pvec 0 pv-bits pv-empty-node (vector) #f))
;; build a trie pvec from a flat Scheme vector (the public constructor).
(define make-pvec
  (case-lambda
    ((v) (make-pvec v #f))
    ((v ent)
     (let ((n (vector-length v)))
       (if (fx<=? n pv-width)
           (mk-pvec n pv-bits pv-empty-node v ent)   ; fits in the tail
           (let loop ((p empty-pvec) (i 0))
             (if (fx=? i n) p (loop (pvec-conj p (vector-ref v i)) (fx+ i 1)))))))))
;; materialize the trie back to a flat Scheme vector (compatibility for callers
;; that read the backing array — all one-shot conversions, not hot loops).
(define (pvec-v p)
  (let* ((cnt (pvec-cnt p)) (out (make-vector cnt)))
    (let loop ((i 0))
      (if (fx<? i cnt)
          (let-values (((chunk off) (pv-leaf-for p i)))
            (let ((run (fxmin (fx- (vector-length chunk) off) (fx- cnt i))))
              (let cloop ((j 0))
                (if (fx<? j run)
                    (begin (vector-set! out (fx+ i j) (vector-ref chunk (fx+ off j))) (cloop (fx+ j 1)))
                    (loop (fx+ i run))))))
          out))))
(define (jolt-vector . xs) (make-pvec (list->vector xs)))
(define (make-map-entry k v) (make-pvec (vector k v) #t))
(define (jolt-map-entry? x) (and (pvec? x) (eq? (pvec-ent x) #t)))
(define (jolt-subvec-view? x) (and (pvec? x) (eq? (pvec-ent x) (quote subvec))))

;; ============================================================================
;; RRB concat / slice — O(log n) pvec-catvec and pvec-slice
;;
;; The concat/rebalance plan, size-table search, and take/drop node surgery are
;; ported from Racket's treelist (racket/collects/racket/treelist.rkt, MIT or
;; Apache-2.0), adapted to this file's node shapes: leaves are element chunks,
;; regular branches are Scheme vectors, relaxed branches are rrbnode records,
;; and the vector keeps Clojure's tail (treelist has none; the tail seam
;; follows clojure/core.rrb-vector, with L'orange's RRB thesis arbitrating).
;; Levels are in SHIFT BITS like the rest of this file: leaves at 0, a node's
;; children one pv-bits step down.
;; ============================================================================

;; children of a slot, viewing a leaf as a chunk of "children" (its elements)
(define (rrb-slot-children n) (if (rrbnode? n) (rrbnode-children n) n))
(define (rrb-nslots n) (vector-length (rrb-slot-children n)))
(define (rrb-first-child n) (vector-ref (rrb-slot-children n) 0))
(define (rrb-last-child n)
  (let ((cs (rrb-slot-children n))) (vector-ref cs (fx- (vector-length cs) 1))))

(define (vec-append a b)
  (let* ((na (vector-length a)) (nb (vector-length b)) (out (make-vector (fx+ na nb))))
    (let loop ((i 0)) (when (fx<? i na) (vector-set! out i (vector-ref a i)) (loop (fx+ i 1))))
    (let loop ((i 0)) (when (fx<? i nb) (vector-set! out (fx+ na i) (vector-ref b i)) (loop (fx+ i 1))))
    out))
(define (vec-drop-first v) (vec-copy-range v 1 (vector-length v)))

;; elements in a subtree
(define (rrb-subtree-count node level)
  (cond
    ((fx=? level 0) (vector-length node))
    ((rrbnode? node)
     (let ((sizes (rrbnode-sizes node))) (vector-ref sizes (fx- (vector-length sizes) 1))))
    (else
     ;; leftwise dense: all but the last child are full for their level
     (fx+ (fxsll (fx- (vector-length node) 1) level)
          (rrb-subtree-count (rrb-last-child node) (fx- level pv-bits))))))

;; build a branch at `level` from a children vector: leftwise-dense children
;; stay a plain radix-addressed vector, anything else gets a size table.
(define (rrb-mk-node children level)
  (if (fx=? level 0)
      children
      (let* ((n (vector-length children))
             (sizes (make-vector n))
             (mask (fx- (fxsll 1 level) 1)))
        (let loop ((i 0) (sum 0) (dense? #t))
          (if (fx<? i n)
              (let ((new-sum (fx+ sum (rrb-subtree-count (vector-ref children i) (fx- level pv-bits)))))
                (vector-set! sizes i new-sum)
                (loop (fx+ i 1) new-sum (and dense? (fx=? 0 (fxand sum mask)))))
              (if dense? children (make-rrbnode sizes children)))))))

;; push a full 32-wide tail leaf down the rightmost spine of a relaxed trie;
;; #f when every node on the spine is full (caller grows the root).
(define (rrb-push-leaf node level leaf)
  (cond
    ((fx=? level pv-bits)
     (if (fx<? (rrb-nslots node) pv-width)
         (rrb-mk-node (vec-snoc (rrb-slot-children node) leaf) level)
         #f))
    (else
     (let* ((cs (rrb-slot-children node)) (n (vector-length cs))
            (pushed (and (fx>? n 0)
                         (rrb-push-leaf (vector-ref cs (fx- n 1)) (fx- level pv-bits) leaf))))
       (cond
         (pushed (rrb-mk-node (vec-set cs (fx- n 1) pushed) level))
         ((fx<? n pv-width)
          (rrb-mk-node (vec-snoc cs (pv-new-path (fx- level pv-bits) leaf)) level))
         (else #f))))))

;; --- concat ------------------------------------------------------------------
(define (rrb-merge-nodes left center-slots right)
  (vec-append (if left (vec-drop-last (rrb-slot-children left)) (vector))
              (vec-append center-slots
                          (if right (vec-drop-first (rrb-slot-children right)) (vector)))))

;; redistribution plan over slots that temporarily exceed 32 children:
;; #f when already within ceil(count/32)+2 slots (the RRB search-step bound).
(define (rrb-concat-plan slots)
  (let* ((n (vector-length slots))
         (plan (make-vector n))
         (child-count (let loop ((i 0) (c 0))
                        (if (fx<? i n)
                            (let ((sz (rrb-nslots (vector-ref slots i))))
                              (vector-set! plan i sz)
                              (loop (fx+ i 1) (fx+ c sz)))
                            c)))
         (optimal (fxquotient (fx+ child-count pv-width -1) pv-width))
         (target (fx+ optimal 2)))
    (if (fx>=? target n) #f (rrb-distribute plan target n))))

(define (rrb-distribute plan target count)
  (let loop ((count count) (node-idx 0))
    (if (fx>=? target count)
        (vec-take plan count)
        (let* ((init-i (let short ((i node-idx))
                         (if (fx<? (vector-ref plan i) (fx- pv-width 1)) i (short (fx+ i 1)))))
               (i (let dist ((i init-i) (r (vector-ref plan init-i)))
                    (if (fx=? r 0)
                        i
                        (let ((min-size (fxmin (fx+ r (vector-ref plan (fx+ i 1))) pv-width)))
                          (vector-set! plan i min-size)
                          (dist (fx+ i 1)
                                (fx- (fx+ r (vector-ref plan (fx+ i 1))) min-size)))))))
          ;; one slot was absorbed: close the gap
          (let move ((j i))
            (when (fx<? j (fx- count 1))
              (vector-set! plan j (vector-ref plan (fx+ j 1))) (move (fx+ j 1))))
          (loop (fx- count 1) (fxmax 0 (fx- i 1)))))))

(define (rrb-exec-plan slots plan sl)     ; sl = the slots' own level
  (if (not plan)
      slots
      (let* ((ns (vector-length slots))
             (flat-size (let loop ((i 0) (s 0))
                          (if (fx<? i ns) (loop (fx+ i 1) (fx+ s (rrb-nslots (vector-ref slots i)))) s)))
             (flat (make-vector flat-size)))
        (let fill ((i 0) (k 0))
          (when (fx<? i ns)
            (let* ((cs (rrb-slot-children (vector-ref slots i))) (n (vector-length cs)))
              (let cp ((j 0)) (when (fx<? j n) (vector-set! flat (fx+ k j) (vector-ref cs j)) (cp (fx+ j 1))))
              (fill (fx+ i 1) (fx+ k n)))))
        (let* ((np (vector-length plan)) (out (make-vector np)))
          (let build ((i 0) (sum 0))
            (if (fx<? i np)
                (let ((w (vector-ref plan i)))
                  (vector-set! out i (rrb-mk-node (vec-copy-range flat sum (fx+ sum w)) sl))
                  (build (fx+ i 1) (fx+ sum w)))
                out))))))

;; merge `left`'s children (but its last), `center`'s, and `right`'s (but its
;; first) into <=32-slot nodes; -> (values node level), growing a level when
;; the redistributed slots still exceed one node.
(define (rrb-rebalance left center right level level-c)
  (let* ((sl (fx- level pv-bits))
         (center-slots (if (fx<? level-c level) (vector center) (rrb-slot-children center)))
         (all-slots (rrb-merge-nodes left center-slots right))
         (plan (rrb-concat-plan all-slots))
         (slots (rrb-exec-plan all-slots plan sl)))
    (if (fx<=? (vector-length slots) pv-width)
        (values (rrb-mk-node slots level) level)
        (let ((nl (vec-take slots pv-width))
              (nr (vec-copy-range slots pv-width (vector-length slots))))
          (values (rrb-mk-node (vector (rrb-mk-node nl level) (rrb-mk-node nr level))
                               (fx+ level pv-bits))
                  (fx+ level pv-bits))))))

;; concatenate two subtrees; -> (values node level)
(define (rrb-concat-subtree left ls right rs)
  (cond
    ((fx>? ls rs)
     (let-values (((mid ml) (rrb-concat-subtree (rrb-last-child left) (fx- ls pv-bits) right rs)))
       (rrb-rebalance left mid #f ls ml)))
    ((fx<? ls rs)
     (let-values (((mid ml) (rrb-concat-subtree left ls (rrb-first-child right) (fx- rs pv-bits))))
       (rrb-rebalance #f mid right rs ml)))
    ((fx=? ls 0)
     (if (fx<=? (fx+ (vector-length left) (vector-length right)) pv-width)
         (values (vec-append left right) 0)
         (values (rrb-mk-node (vector left right) pv-bits) pv-bits)))
    (else
     (let-values (((mid ml) (rrb-concat-subtree (rrb-last-child left) (fx- ls pv-bits)
                                                (rrb-first-child right) (fx- rs pv-bits))))
       (rrb-rebalance left mid right ls ml)))))

;; --- take / drop -------------------------------------------------------------
;; keep elements [0, i] of the subtree (i relative, inclusive)
(define (rrb-take-node node level i)
  (cond
    ((fx=? level 0) (vec-take node (fx+ (fxand i pv-mask) 1)))
    ((rrbnode? node)
     (let* ((i (fxand i (fx- (fxsll 1 (fx+ level pv-bits)) 1)))   ; see pv-leaf-for
            (sizes (rrbnode-sizes node))
            (k (rrb-find-child sizes i))
            (sub (if (fx=? k 0) i (fx- i (vector-ref sizes (fx- k 1)))))
            (children (rrbnode-children node))
            (new-child (rrb-take-node (vector-ref children k) (fx- level pv-bits) sub))
            (new-children (vec-take children (fx+ k 1))))
       (vector-set! new-children k new-child)
       (if (fx=? (vector-length new-children) 1)
           new-children                       ; a single child radix-addresses as slot 0
           (let ((new-sizes (vec-take sizes (fx+ k 1))))
             (vector-set! new-sizes k (fx+ i 1))
             (make-rrbnode new-sizes new-children)))))
    (else
     (let* ((k (fxand (fxsra i level) pv-mask))
            (new-child (rrb-take-node (vector-ref node k) (fx- level pv-bits) i))
            (new-children (vec-take node (fx+ k 1))))
       (vector-set! new-children k new-child)
       new-children))))                       ; prefix children untouched => stays leftwise dense

;; drop the first i elements of the subtree (i relative)
(define (rrb-drop-node node level i)
  (cond
    ((fx=? level 0) (vec-copy-range node (fxand i pv-mask) (vector-length node)))
    ((rrbnode? node)
     (let* ((i (fxand i (fx- (fxsll 1 (fx+ level pv-bits)) 1)))   ; see pv-leaf-for
            (sizes (rrbnode-sizes node))
            (k (rrb-find-child sizes i))
            (sub (if (fx=? k 0) i (fx- i (vector-ref sizes (fx- k 1)))))
            (children (rrbnode-children node))
            (new-child (rrb-drop-node (vector-ref children k) (fx- level pv-bits) sub))
            (new-children (vec-copy-range children k (vector-length children))))
       (vector-set! new-children 0 new-child)
       (if (fx=? (vector-length new-children) 1)
           new-children
           (let* ((old-len (vector-length sizes))
                  (new-len (fx- old-len k))
                  (new-sizes (make-vector new-len)))
             (let loop ((j 0))
               (when (fx<? j new-len)
                 (vector-set! new-sizes j (fx- (vector-ref sizes (fx+ k j)) i))
                 (loop (fx+ j 1))))
             (make-rrbnode new-sizes new-children)))))
    (else
     (let* ((k (fxand (fxsra i level) pv-mask))
            (new-child (rrb-drop-node (vector-ref node k) (fx- level pv-bits) i))
            (new-children (vec-copy-range node k (vector-length node)))
            (new-len (vector-length new-children)))
       (vector-set! new-children 0 new-child)
       (if (fx=? new-len 1)
           new-children
           (let ((size0 (rrb-subtree-count new-child (fx- level pv-bits)))
                 (step (fxsll 1 level)))
             (if (fx=? size0 step)
                 new-children                 ; dropped whole subtrees: still leftwise dense
                 (let ((new-sizes (make-vector new-len)))
                   (let loop ((j 0))
                     (when (fx<? j (fx- new-len 1))
                       (vector-set! new-sizes j (fx+ size0 (fx* j step)))
                       (loop (fx+ j 1))))
                   (vector-set! new-sizes (fx- new-len 1)
                                (fx+ size0 (fx* (fx- new-len 2) step)
                                     (rrb-subtree-count (vector-ref new-children (fx- new-len 1))
                                                        (fx- level pv-bits))))
                   (make-rrbnode new-sizes new-children)))))))))

;; collapse single-child levels; -> (values node level)
(define (rrb-squash node level)
  (if (and (fx>? level 0) (fx=? (rrb-nslots node) 1))
      (rrb-squash (rrb-first-child node) (fx- level pv-bits))
      (values node level)))

;; --- pvec boundary -----------------------------------------------------------
;; the whole vector (tail folded in) as one subtree; -> (values root level)
(define (pvec->rrb-tree p)
  (let ((tail (pvec-tail p)) (tailoff (pv-tailoff-of p)))
    (cond
      ((fx=? tailoff 0) (values tail 0))
      ((fx=? (vector-length tail) 0) (values (pvec-root p) (pvec-shift p)))
      (else (rrb-concat-subtree (pvec-root p) (pvec-shift p) tail 0)))))

;; is this subtree a full classic trie (32-aligned count, plain rightmost
;; spine)? Plain multi-child nodes are leftwise dense by construction, so a
;; plain spine + aligned total means every leaf is full and radix-addressed.
(define (rrb-classic-tree? node level cnt)
  (and (fx=? 0 (fxand cnt pv-mask))
       (let spine ((n node) (l level))
         (cond ((fx=? l 0) #t)
               ((rrbnode? n) #f)
               (else (spine (rrb-last-child n) (fx- l pv-bits)))))))

;; classic emission: pull the rightmost leaf back out as the tail so every
;; classic invariant (tail 1..32, aligned tailoff, full leaves) holds.
(define (rrb-tree->classic-pvec root level cnt)
  (let* ((new-tail (let last ((n root) (l level))
                     (if (fx=? l 0) n (last (rrb-last-child n) (fx- l pv-bits)))))
         (popped (pv-pop-tail cnt level root))
         (new-root (or popped pv-empty-node)))
    (if (and (fx>? level pv-bits) (fx<? (vector-length new-root) 2))
        (mk-pvec cnt (fx- level pv-bits)
                 (if (fx=? 0 (vector-length new-root)) pv-empty-node (vector-ref new-root 0))
                 new-tail #f)
        (mk-pvec cnt level new-root new-tail #f))))

;; force a size table onto a plain (leftwise-dense) root: a plain-rooted pvec
;; promises the FULL classic invariants, which a partial last leaf breaks.
(define (rrb-force-relaxed root level)
  (if (rrbnode? root)
      root
      (let* ((n (vector-length root)) (sizes (make-vector n)))
        (let loop ((i 0) (sum 0))
          (if (fx<? i n)
              (let ((s (fx+ sum (rrb-subtree-count (vector-ref root i) (fx- level pv-bits)))))
                (vector-set! sizes i s) (loop (fx+ i 1) s))
              (make-rrbnode sizes root))))))

(define (rrb-tree->pvec root level cnt)
  (cond
    ((fx=? cnt 0) empty-pvec)
    ((fx=? level 0) (mk-pvec cnt pv-bits pv-empty-node root #f))   ; bare leaf: tail-only
    ((rrb-classic-tree? root level cnt) (rrb-tree->classic-pvec root level cnt))
    (else (mk-pvec cnt level (rrb-force-relaxed root level) (vector) #f))))

;; --- public entry points -----------------------------------------------------
(define (pvec-copy-into! out at p)
  (let ((cnt (pvec-cnt p)))
    (let loop ((i 0))
      (when (fx<? i cnt)
        (let-values (((chunk off) (pv-leaf-for p i)))
          (let ((run (fxmin (fx- (vector-length chunk) off) (fx- cnt i))))
            (let cp ((j 0))
              (when (fx<? j run)
                (vector-set! out (fx+ at (fx+ i j)) (vector-ref chunk (fx+ off j)))
                (cp (fx+ j 1))))
            (loop (fx+ i run))))))))

;; O(log n) structural concatenation
(define (pvec-catvec a b)
  (let ((ca (pvec-cnt a)) (cb (pvec-cnt b)))
    (cond
      ((fx=? ca 0) b)
      ((fx=? cb 0) a)
      ((fx<=? (fx+ ca cb) pv-width)
       (let ((out (make-vector (fx+ ca cb))))
         (pvec-copy-into! out 0 a)
         (pvec-copy-into! out ca b)
         (mk-pvec (fx+ ca cb) pv-bits pv-empty-node out #f)))
      (else
       (let*-values (((ra la) (pvec->rrb-tree a))
                     ((rb lb) (pvec->rrb-tree b))
                     ((root level) (rrb-concat-subtree ra la rb lb)))
         (rrb-tree->pvec root level (fx+ ca cb)))))))

;; O(log n) structural slice [start, end)
(define (pvec-slice p start end)
  (let ((start (->idx start)) (end (->idx end)) (cnt (pvec-cnt p)))
    (cond
      ((or (not (fixnum? start)) (not (fixnum? end))
           (fx<? start 0) (fx>? end cnt) (fx>? start end))
       (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "slice index out of bounds")))
      ((and (fx=? start 0) (fx=? end cnt)) p)
      ((fx=? start end) empty-pvec)
      ((fx<=? (fx- end start) pv-width)
       ;; small windows flatten to a tail-only classic vector
       (let* ((n (fx- end start)) (out (make-vector n)))
         (let loop ((i start))
           (when (fx<? i end)
             (let-values (((chunk off) (pv-leaf-for p i)))
               (let ((run (fxmin (fx- (vector-length chunk) off) (fx- end i))))
                 (let cp ((j 0))
                   (when (fx<? j run)
                     (vector-set! out (fx+ (fx- i start) j) (vector-ref chunk (fx+ off j)))
                     (cp (fx+ j 1))))
                 (loop (fx+ i run))))))
         (mk-pvec n pv-bits pv-empty-node out #f)))
      (else
       (let-values (((root level) (pvec->rrb-tree p)))
         (let*-values (((root level) (if (fx=? end cnt)
                                         (values root level)
                                         (let ((r (rrb-take-node root level (fx- end 1))))
                                           (rrb-squash r level))))
                       ((root level) (if (fx=? start 0)
                                         (values root level)
                                         (let ((r (rrb-drop-node root level start)))
                                           (rrb-squash r level)))))
           (rrb-tree->pvec root level (fx- end start))))))))

;; ============================================================================
;; bitmap HAMT — keys hashed by jolt-hash, leaves compared by jolt=
;;   arr slot is one of: leaf (cons k v) | hnode (branch) | hcoll (hash bucket)
;; ============================================================================
(define-record-type hnode (fields bm arr) (nongenerative chez-hnode-v1))
(define-record-type hcoll (fields hash alist) (nongenerative chez-hcoll-v1))
(define empty-hnode (make-hnode 0 (vector)))
(define hmask #xFFFFFFFF)                ; 32-bit unsigned hash window (JVM int range)
(define max-shift 30)                     ; 7 levels × 5 bits = 35 > 32; last level shift 30

;; A hash and a node bitmap each span all 32 bits, so on a 32-bit Chez (tpb32l)
;; they are bignums, not fixnums — see hasheq.ss's define-width-op for the width
;; invariant and why the arm is chosen at expand time. These are the SAFE fx
;; forms on the wide path, as they have always been: the bitmap arithmetic is
;; not the measured inner chain the hash engine's #3% forms were tuned for, and
;; a checked op here fails loudly rather than corrupting a node.
(define-width-op hamt=?       fx=?        =)
(define-width-op hamt-sub     fx-         -)
(define-width-op hamt-and     fxand       bitwise-and)
(define-width-op hamt-ior     fxior       bitwise-ior)
(define-width-op hamt-not     fxnot       bitwise-not)
(define-width-op hamt-sll     fxsll       bitwise-arithmetic-shift-left)
(define-width-op hamt-sra     fxsra       bitwise-arithmetic-shift-right)
(define-width-op hamt-popcount fxbit-count bitwise-bit-count)

;; The runtime fixnum? test stays, on both arms: an extension type can return a
;; bignum here via equal-hash whatever the machine's width is, and that case has
;; to reach bitwise-and even on a 64-bit build. hamt-and covers the other half —
;; on a 32-bit machine an ORDINARY int hash is a bignum too, so the test is not
;; the only thing keeping fxand off a non-fixnum there.
(define (key-hash k)
  ;; jolt-hasheq now has a flat-inlined fixnum fast path (murmur3-hash-long-flat)
  ;; and a keyword cached-field read — no re-dispatch needed here.
  ;; Mask to unsigned 32 bits for the HAMT's fx ops.
  (let ((h (jolt-hasheq k)))
    (if (fixnum? h) (hamt-and h hmask) (bitwise-and h hmask))))
(define (chunk h shift) (hamt-and (hamt-sra h shift) 31))
(define (bitpos h shift) (hamt-sll 1 (chunk h shift)))
(define (popcount n) (hamt-popcount n))
(define (arr-index bm bit) (popcount (hamt-and bm (hamt-sub bit 1))))

;; jolt= alist ops (for hash-collision buckets)
(define (assoc-jolt k al) (cond ((null? al) #f) ((jolt= (caar al) k) (car al)) (else (assoc-jolt k (cdr al)))))
;; Replacing a key's value KEEPS the key already stored, dropping the equal one
;; assoc was handed (JVM PersistentHashMap.assoc only clones the value slot). The
;; two are jolt=, but only the stored one carries the metadata that seq/keys/find
;; hand back.
(define (alist-replace k v al) (if (jolt= (caar al) k) (cons (cons (caar al) v) (cdr al)) (cons (car al) (alist-replace k v (cdr al)))))
(define (alist-remove k al) (cond ((null? al) '()) ((jolt= (caar al) k) (cdr al)) (else (cons (car al) (alist-remove k (cdr al))))))

;; split two leaves that collided at `shift` into a subtree (or hcoll if the
;; full hashes are equal / the hash is exhausted).
(define (split-leaf shift ek ev h k v)
  (let ((eh (key-hash ek)))
    (if (or (fx>? shift max-shift) (hamt=? eh h))
        (make-hcoll h (list (cons ek ev) (cons k v)))
        (let ((ei (chunk eh shift)) (ni (chunk h shift)))
          (if (fx=? ei ni)
              (make-hnode (hamt-sll 1 ei) (vector (split-leaf (fx+ shift 5) ek ev h k v)))
              (let ((eb (hamt-sll 1 ei)) (nb (hamt-sll 1 ni)))
                (if (fx<? ei ni)
                    (make-hnode (hamt-ior eb nb) (vector (cons ek ev) (cons k v)))
                    (make-hnode (hamt-ior eb nb) (vector (cons k v) (cons ek ev))))))))))

;; Answers the node ITSELF when the key is present with that very value — the
;; reference's `if(val == valOrNode) return this` — so pmap-assoc can hand the
;; caller's map back untouched (the eq? test at the root), the way both
;; PersistentArrayMap.assoc and PersistentHashMap.assoc do.
(define (node-assoc node shift h k v added)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)) (arr (hnode-arr node)))
    (if (hamt=? 0 (hamt-and bm bit))
        (begin (set-box! added #t)
               (make-hnode (hamt-ior bm bit) (vec-insert arr (arr-index bm bit) (cons k v))))
        (let* ((i (arr-index bm bit)) (child (vector-ref arr i)))
          (cond
            ((hnode? child)
             (let ((nc (node-assoc child (fx+ shift 5) h k v added)))
               (if (eq? nc child) node (make-hnode bm (vec-set arr i nc)))))
            ((hcoll? child)
             (let* ((al (hcoll-alist child)) (p (assoc-jolt k al)))
               (cond ((not p)
                      (set-box! added #t)
                      (make-hnode bm (vec-set arr i (make-hcoll (hcoll-hash child) (append al (list (cons k v)))))))
                     ((eq? (cdr p) v) node)
                     (else (make-hnode bm (vec-set arr i (make-hcoll (hcoll-hash child) (alist-replace k v al))))))))
            ;; replace: the leaf keeps ITS key, not the equal one handed in
            ((jolt= (car child) k)
             (if (eq? (cdr child) v) node (make-hnode bm (vec-set arr i (cons (car child) v)))))
            (else (set-box! added #t)
                  (make-hnode bm (vec-set arr i (split-leaf (fx+ shift 5) (car child) (cdr child) h k v)))))))))

(define (node-get node shift h k default)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)))
    (if (hamt=? 0 (hamt-and bm bit)) default
        (let ((child (vector-ref (hnode-arr node) (arr-index bm bit))))
          (cond ((hnode? child) (node-get child (fx+ shift 5) h k default))
                ((hcoll? child) (let ((p (assoc-jolt k (hcoll-alist child)))) (if p (cdr p) default)))
                ((jolt= (car child) k) (cdr child))
                (else default))))))

;; node-get, but answering with the whole stored (key . value) pair — #f when the
;; key is absent. The pair's car is the key the map HOLDS, which is what `find`
;; must put in its entry (see pmap-entry-at).
(define (node-entry node shift h k)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)))
    (if (hamt=? 0 (hamt-and bm bit)) #f
        (let ((child (vector-ref (hnode-arr node) (arr-index bm bit))))
          (cond ((hnode? child) (node-entry child (fx+ shift 5) h k))
                ((hcoll? child) (assoc-jolt k (hcoll-alist child)))
                ((jolt= (car child) k) child)
                (else #f))))))

(define (node-dissoc node shift h k removed)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)) (arr (hnode-arr node)))
    (if (hamt=? 0 (hamt-and bm bit)) node
        (let* ((i (arr-index bm bit)) (child (vector-ref arr i)))
          (cond
            ((hnode? child) (make-hnode bm (vec-set arr i (node-dissoc child (fx+ shift 5) h k removed))))
            ((hcoll? child)
             (if (assoc-jolt k (hcoll-alist child))
                 (begin (set-box! removed #t)
                        (let ((nal (alist-remove k (hcoll-alist child))))
                          (cond ((null? nal) (make-hnode (hamt-and bm (hamt-not bit)) (vec-remove arr i)))
                                ((null? (cdr nal)) (make-hnode bm (vec-set arr i (car nal))))   ; collapse to leaf
                                (else (make-hnode bm (vec-set arr i (make-hcoll (hcoll-hash child) nal)))))))
                 node))
            ((jolt= (car child) k)
             (set-box! removed #t) (make-hnode (hamt-and bm (hamt-not bit)) (vec-remove arr i)))
            (else node))))))

(define (node-fold node proc acc)     ; (proc k v acc) over every leaf, JVM (ascending) order
  (let ((arr (hnode-arr node)))
    (let loop ((i (fx- (vector-length arr) 1)) (acc acc))
      (if (fx<? i 0)
          acc
          (let ((child (vector-ref arr i)))
            (loop (fx- i 1)
                  (cond ((hnode? child) (node-fold child proc acc))
                        ((hcoll? child)
                         (let cl ((al (hcoll-alist child)) (a acc))
                           (if (null? al) a (cl (cdr al) (proc (caar al) (cdar al) a)))))
                        (else (proc (car child) (cdr child) acc)))))))))

;; ============================================================================
;; editable HAMT nodes — reachable only from a live transient
;; ============================================================================
;; A transient claims a node by copying it into an `enode` and then writes into
;; that copy in place, so building through a transient stops path-copying the
;; trie per entry.
;;
;; PersistentHashMap.java does this by giving every node an edit token and
;; mutating when the token matches. jolt cannot: `hnode`'s layout is
;; image-format surface. state-image.ss keeps a CLEAN pmap by pointer and only
;; rebuilds it when a key or value had to be substituted, so an ordinary map's
;; whole node tree is fasl-written raw — a dumped map's bytes contain
;; `chez-hnode-v1`. Adding a field to it stops released images restoring.
;;
;; So the editable node is a separate type, and "is this node mine?" is just
;; `enode?` — no token needed. That is a stronger guarantee than the JVM's: an
;; enode is only ever reachable from the transient that created it (claiming
;; COPIES, and persistent! freezes every enode back into an hnode), so two
;; transients can never share one. The immutable node functions above therefore
;; never see an enode, and nothing on the persistent map path changes.
;;
;; `arr` carries SLACK — (vector-length arr) >= (popcount bm) — so an insert
;; that fits shifts the tail right in place instead of reallocating.
(define-record-type enode (fields (mutable bm) (mutable arr)) (nongenerative chez-enode-v1))
(define enode-slack 4)
(define enode-max 32)                   ; a bitmap node holds at most 32 slots

(define (enode-used nd) (popcount (enode-bm nd)))

;; Claim a node: an enode is already ours; an hnode is copied once, with room to
;; grow. Its CHILDREN and its (k . v) leaves stay shared with the source — see
;; enode-assoc!'s leaf case.
(define (hnode->enode nd)
  (let* ((arr (hnode-arr nd)) (used (vector-length arr))
         (buf (make-vector (fxmin enode-max (fx+ used enode-slack)) #f)))
    (let loop ((i 0)) (when (fx<? i used) (vector-set! buf i (vector-ref arr i)) (loop (fx+ i 1))))
    (make-enode (hnode-bm nd) buf)))
(define (enode-claim nd) (if (enode? nd) nd (hnode->enode nd)))

;; insert x at slot i, shifting the tail right; grows the array when full.
(define (enode-insert! nd i x)
  (let* ((used (enode-used nd)) (arr0 (enode-arr nd))
         (arr (if (fx<? used (vector-length arr0))
                  arr0
                  (let ((w (make-vector (fxmin enode-max (fx+ (fx* used 2) enode-slack)) #f)))
                    (let loop ((j 0)) (when (fx<? j used) (vector-set! w j (vector-ref arr0 j)) (loop (fx+ j 1))))
                    (enode-arr-set! nd w)
                    w))))
    (let loop ((j used)) (when (fx>? j i) (vector-set! arr j (vector-ref arr (fx- j 1))) (loop (fx- j 1))))
    (vector-set! arr i x)))

;; remove slot i, shifting the tail left. Call BEFORE clearing the bit, so that
;; (enode-used nd) still counts the slot being dropped.
(define (enode-remove! nd i)
  (let ((used (enode-used nd)) (arr (enode-arr nd)))
    (let loop ((j i)) (when (fx<? j (fx- used 1)) (vector-set! arr j (vector-ref arr (fx+ j 1))) (loop (fx+ j 1))))
    (vector-set! arr (fx- used 1) #f)))

;; `nd` must already be claimed. Mutates in place; `added` is boxed like
;; node-assoc's so the transient can keep its count.
(define (enode-assoc! nd shift h k v added)
  (let ((bit (bitpos h shift)) (bm (enode-bm nd)))
    (if (hamt=? 0 (hamt-and bm bit))
        (begin (set-box! added #t)
               (enode-insert! nd (arr-index bm bit) (cons k v))
               (enode-bm-set! nd (hamt-ior bm bit)))
        (let* ((i (arr-index bm bit)) (arr (enode-arr nd)) (child (vector-ref arr i)))
          (cond
            ((enode? child) (enode-assoc! child (fx+ shift 5) h k v added))
            ((hnode? child)
             (let ((c (hnode->enode child)))
               (vector-set! arr i c)
               (enode-assoc! c (fx+ shift 5) h k v added)))
            ;; collisions are rare, so the bucket stays immutable and is replaced
            ((hcoll? child)
             (let ((al (hcoll-alist child)))
               (if (assoc-jolt k al)
                   (vector-set! arr i (make-hcoll (hcoll-hash child) (alist-replace k v al)))
                   (begin (set-box! added #t)
                          (vector-set! arr i (make-hcoll (hcoll-hash child) (append al (list (cons k v)))))))))
            ;; Replace: cons a FRESH pair. A claimed node's array is a shallow
            ;; copy, so this leaf is very likely the source map's own pair —
            ;; set-cdr! here would rewrite a value inside the persistent map the
            ;; transient was built from. Keeps the STORED key, like node-assoc.
            ((jolt= (car child) k) (vector-set! arr i (cons (car child) v)))
            (else (set-box! added #t)
                  (vector-set! arr i (split-leaf (fx+ shift 5) (car child) (cdr child) h k v))))))))

;; Mirrors node-dissoc, including leaving an emptied interior node in place
;; rather than collapsing it (node-dissoc does the same).
(define (enode-dissoc! nd shift h k removed)
  (let ((bit (bitpos h shift)) (bm (enode-bm nd)))
    (unless (hamt=? 0 (hamt-and bm bit))
      (let* ((i (arr-index bm bit)) (arr (enode-arr nd)) (child (vector-ref arr i)))
        (cond
          ((or (enode? child) (hnode? child))
           (let ((c (enode-claim child)))
             (vector-set! arr i c)
             (enode-dissoc! c (fx+ shift 5) h k removed)))
          ((hcoll? child)
           (when (assoc-jolt k (hcoll-alist child))
             (set-box! removed #t)
             (let ((nal (alist-remove k (hcoll-alist child))))
               (cond ((null? nal) (enode-remove! nd i) (enode-bm-set! nd (hamt-and bm (hamt-not bit))))
                     ((null? (cdr nal)) (vector-set! arr i (car nal)))   ; collapse to leaf
                     (else (vector-set! arr i (make-hcoll (hcoll-hash child) nal)))))))
          ((jolt= (car child) k)
           (set-box! removed #t)
           (enode-remove! nd i)
           (enode-bm-set! nd (hamt-and bm (hamt-not bit)))))))))

;; Reads work over a MIXED tree: the part the transient has written to is
;; enodes, everything else is still the source map's hnodes. This is on the hot
;; path for read-heavy transient code (group-by does one get per element), so
;; the cases are ordered by frequency rather than by kind: a leaf is a pair and
;; nothing else here is, so one `pair?` settles the common case, and an
;; unclaimed subtree hands off to node-get, which carries no enode tests at all.
(define (enode-get nd shift h k default)
  (if (enode? nd)
      (let ((bit (bitpos h shift)) (bm (enode-bm nd)))
        (if (hamt=? 0 (hamt-and bm bit))
            default
            (let ((child (vector-ref (enode-arr nd) (arr-index bm bit))))
              (cond ((pair? child) (if (jolt= (car child) k) (cdr child) default))
                    ((enode? child) (enode-get child (fx+ shift 5) h k default))
                    ((hcoll? child) (let ((p (assoc-jolt k (hcoll-alist child)))) (if p (cdr p) default)))
                    (else (node-get child (fx+ shift 5) h k default))))))
      (node-get nd shift h k default)))

;; Freeze an edited tree: every enode becomes an hnode whose array is trimmed to
;; exactly its used slots. An hnode subtree was never claimed, so it is already
;; immutable and is kept BY POINTER — that is the structural sharing with the
;; map the transient started from. Cost is O(claimed nodes), not O(entries).
(define (enode-freeze nd)
  (if (enode? nd)
      (let* ((used (enode-used nd)) (arr (enode-arr nd)) (out (make-vector used)))
        (let loop ((i 0))
          (if (fx<? i used)
              (begin (vector-set! out i (enode-freeze (vector-ref arr i)))
                     (loop (fx+ i 1)))
              (make-hnode (enode-bm nd) out))))
      nd))

(define (enode-empty) (make-enode 0 (make-vector enode-slack #f)))

;; ============================================================================
;; persistent map / set over the HAMT
;; ============================================================================
;; A map is one of two representations behind one record type, told apart by
;; its root — an hnode is the trie, anything else the slot vector (the test is
;; hnode?, never vector?: a target may represent records as vectors):
;;
;;   ARRAY mode — root is a Scheme vector of alternating key/value slots
;;     #(k0 v0 k1 v1 ...) in INSERTION order (Clojure's PersistentArrayMap).
;;     Lookup is a linear scan specialized on the probe key's kind; assoc and
;;     dissoc copy the vector. That is the whole structure: a 3-entry map is a
;;     record and a 6-slot vector, where a trie-backed small map was a record,
;;     a node, a leaf pair per entry and an order list on top.
;;   HASH mode — root is an hnode, the bitmap HAMT above (PersistentHashMap).
;;
;; The mode is observable only through iteration order and (class m): equality
;; and hashing fold over the entries order-independently. A map moves from
;; array to hash mode when assoc would grow it past the thresholds below, and
;; never back (PersistentHashMap.without does not demote).
;;
;; Image-format surface: chez-pmap-v5. Instances travel raw in a state image,
;; so the layout (root cnt hasheq) is frozen; the previous generation
;; (chez-pmap-v4: root cnt order hasheq all-kw, a trie root plus an order list)
;; restores through state-image.ss's legacy arm.
(define-record-type pmap (fields root cnt (mutable hasheq)) (nongenerative chez-pmap-v5))
(define make-pmap
  (let ((raw (record-constructor (record-type-descriptor pmap))))
    (lambda (root cnt) (raw root cnt 0))))
(define (pmap-array? m) (not (hnode? (pmap-root m))))
(define amap-no-slots (vector))
(define empty-pmap (make-pmap amap-no-slots 0))            ; {} = the shared empty array map
(define (fresh-empty-pmap) (make-pmap amap-no-slots 0))   ; an empty result a caller may attach meta to
(define empty-pmap-hash (make-pmap empty-hnode 0))        ; hash-order backing (sets)
(define pmap-absent (list 'absent))    ; unique missing-key sentinel
;; PersistentArrayMap thresholds: assoc of a new key promotes to hash mode once
;; the map holds 8 entries (HASHTABLE_THRESHOLD = 16 array slots), unless the
;; ADDED key is a keyword, which rides array mode to 64 entries
;; (KW_HASHTABLE_THRESHOLD = 128 slots). Only the new key's type is consulted —
;; PersistentArrayMap.assoc's isKW check — so a keyword assoc'd onto an 8-entry
;; string-keyed array map keeps it in array mode.
(define array-map-limit 8)
(define array-map-limit-kw 64)
(define (pmap-array-keep? cnt k)
  (or (fx<? cnt array-map-limit)
      (and (keyword-t? k) (fx<? cnt array-map-limit-kw))))

;; --- the slot vector ----------------------------------------------------------
;; Slot index of k among the first n slots of arr, or -1. The compare is chosen
;; ONCE from the probe key's kind (Util.equivPred), not per slot: a keyword is
;; interned, so identity is its equality and no other kind of key equals one;
;; a fixnum only equals a fixnum (5 and 5.0 differ, a bignum never normalizes
;; into fixnum range); a string only a string; a symbol only a symbol with the
;; same name and namespace (the name first — it discriminates). Everything
;; else takes jolt=2. n is passed rather than read so a transient's buffer,
;; which carries spare capacity, scans only its used slots.
;;
;; The loops are unchecked (sa-u*): n <= the vector's length is the caller's
;; invariant — a map passes the length itself, a transient its used count,
;; which never exceeds the buffer — and i steps from 0 by 2, so every
;; vector-ref is in range and every fixnum op stays fixnum. Checked, the same
;; scan of a 64-keyword map costs 3x (92 -> 33ns at the last slot).
(define (amap-index arr n k)
  (cond
    ((keyword-t? k)
     (let lp ((i 0))
       (cond ((sa-ufx>=? i n) -1)
             ((eq? (sa-uvector-ref arr i) k) i)
             (else (lp (sa-ufx+ i 2))))))
    ((fixnum? k)
     (let lp ((i 0))
       (cond ((sa-ufx>=? i n) -1)
             ((let ((x (sa-uvector-ref arr i))) (and (fixnum? x) (sa-ufx=? x k))) i)
             (else (lp (sa-ufx+ i 2))))))
    ((string? k)
     (let lp ((i 0))
       (cond ((sa-ufx>=? i n) -1)
             ((let ((x (sa-uvector-ref arr i))) (and (string? x) (string=? x k))) i)
             (else (lp (sa-ufx+ i 2))))))
    ((symbol-t? k)
     (let* ((kn (symbol-t-name k)) (kl (string-length kn)) (kns (symbol-t-ns k)))
       (let lp ((i 0))
         (cond ((sa-ufx>=? i n) -1)
               ((let ((x (sa-uvector-ref arr i)))
                  (and (symbol-t? x)
                       (let ((xn (symbol-t-name x)))
                         (or (eq? xn kn)
                             (and (fx=? (string-length xn) kl) (string=? xn kn))))
                       (let ((xns (symbol-t-ns x)))
                         (or (eq? xns kns)
                             (and (string? xns) (string? kns) (string=? xns kns))))))
                i)
               (else (lp (sa-ufx+ i 2)))))))
    (else
     (let lp ((i 0))
       (cond ((sa-ufx>=? i n) -1)
             ((jolt=2 (sa-uvector-ref arr i) k) i)
             (else (lp (sa-ufx+ i 2))))))))
(define (amap-get arr k d)
  (let ((i (amap-index arr (vector-length arr) k)))
    (if (fx<? i 0) d (vector-ref arr (fx+ i 1)))))
;; Slots from a k v k v ... list of n pairs. A repeated key keeps its first
;; position and takes the LAST value (createAsIfByAssoc), so the result can be
;; shorter than 2n.
(define (amap-from-kvs kvs n)
  (let ((arr (make-vector (fx* 2 n))))
    (let loop ((kvs kvs) (used 0))
      (if (null? kvs)
          (if (fx=? used (vector-length arr)) arr (vec-copy-range arr 0 used))
          ;; used < 2n while pairs remain (at most one slot pair per list pair)
          (let* ((k (car kvs)) (v (cadr kvs)) (i (amap-index arr used k)))
            (if (fx>=? i 0)
                (begin (sa-uvector-set! arr (sa-ufx+ i 1) v) (loop (cddr kvs) used))
                (begin (sa-uvector-set! arr used k) (sa-uvector-set! arr (sa-ufx+ used 1) v)
                       (loop (cddr kvs) (sa-ufx+ used 2)))))))))
(define (amap-slots->pmap arr) (make-pmap arr (fxsra (vector-length arr) 1)))
;; array -> hash: the slots become a trie. Keys are distinct by construction.
(define (amap->hnode arr)
  (let ((n (vector-length arr)) (added (box #f)))
    (let loop ((i 0) (root empty-hnode))
      (if (fx>=? i n) root
          (let ((k (vector-ref arr i)))
            (loop (fx+ i 2) (node-assoc root 0 (key-hash k) k (vector-ref arr (fx+ i 1)) added)))))))

;; --- assoc / dissoc -----------------------------------------------------------
;; growth rule (PersistentArrayMap.assoc): a new key appends to the slots while
;; the map may stay in array mode; otherwise the slots become a trie first and
;; the key goes in there. Replacing a key keeps its position. Assoc of the value
;; already held is the map itself, in both modes.
(define (pmap-hash-assoc m root k v)
  (let* ((added (box #f)) (r (node-assoc root 0 (key-hash k) k v added)))
    (cond ((unbox added) (make-pmap r (fx+ (pmap-cnt m) 1)))
          ((eq? r root) m)
          (else (make-pmap r (pmap-cnt m))))))
(define (pmap-assoc m k v)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (pmap-hash-assoc m root k v)
        (let ((i (amap-index root (vector-length root) k)))
          (cond ((fx>=? i 0)
                 (if (eq? (vector-ref root (fx+ i 1)) v)
                     m
                     (make-pmap (vec-set root (fx+ i 1) v) (pmap-cnt m))))
                ((pmap-array-keep? (pmap-cnt m) k)
                 (make-pmap (vec-append2 root k v) (fx+ (pmap-cnt m) 1)))
                (else (pmap-hash-assoc m (amap->hnode root) k v)))))))
;; force-hash insert for rebuilding a map whose final mode is already decided
;; (hash-map ctor, a set's backing map).
(define (pmap-put-hash m k v)
  (let* ((added (box #f)) (r (node-assoc (pmap-root m) 0 (key-hash k) k v added)))
    (make-pmap r (if (unbox added) (fx+ (pmap-cnt m) 1) (pmap-cnt m)))))
(define (pmap-dissoc m k)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (let* ((removed (box #f)) (r (node-dissoc root 0 (key-hash k) k removed)))
          (if (unbox removed) (make-pmap r (fx- (pmap-cnt m) 1)) m))
        (let ((i (amap-index root (vector-length root) k)))
          (cond ((fx<? i 0) m)
                ;; the reference answers empty() — a fresh empty, since the caller
                ;; may carry the source's metadata onto it (never onto the shared {})
                ((fx=? (vector-length root) 2) (fresh-empty-pmap))
                (else (make-pmap (vec-remove2 root i) (fx- (pmap-cnt m) 1))))))))

;; --- lookup -------------------------------------------------------------------
(define (pmap-get m k default)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (node-get root 0 (key-hash k) k default)
        (amap-get root k default))))
;; The lookup the emitter and jolt-get route through. Array mode scans its
;; slots — the keyword scan is inline here, being the shape (:k m) lowers to.
;; Hash mode is flattened for the key types that dominate real lookups
;; (keyword, fixnum, string, symbol): the layered path costs ~40ns/call —
;; case-lambda -> dispatch -> pmap? -> pmap-get -> key-hash -> jolt-hasheq cond
;; -> node-get -> bitpos/chunk/arr-index/popcount -> VARIADIC jolt= (a rest-list
;; alloc) -> jolt=2 type walk. Everything here is one procedure so cp0 inlines
;; the local helpers (bitpos/arr-index/popcount) and the record preds/accessors;
;; the key compare is specialized (keyword eq? / fixnum = / string string=?)
;; instead of the generic jolt= walk. Anything else (bignum-keyed, symbolic,
;; heterogeneous hashes) falls to pmap-get. Semantics identical to node-get.
(define (pmap-fast-get m k d)
  (let ((root (pmap-root m)))
    (cond
      ((not (hnode? root))
       (if (keyword-t? k)
           ;; unchecked like amap-index: n is the vector's own length, i steps
           ;; from 0 by 2, and slot i+1 exists whenever slot i holds a key
           (let ((n (vector-length root)))
             (let lp ((i 0))
               (cond ((sa-ufx>=? i n) d)
                     ((eq? (sa-uvector-ref root i) k) (sa-uvector-ref root (sa-ufx+ i 1)))
                     (else (lp (sa-ufx+ i 2))))))
           (amap-get root k d)))
      (else
       (let ((h (cond ((keyword-t? k) (keyword-t-khash k))
                      ((fixnum? k) (murmur3-hash-long-flat k))
                      ((string? k) (string-hasheq k))
                      ;; A SYMBOL key belongs on this path too. The reference
                      ;; implementation makes no distinction — Util.equiv/hasheq treat a
                      ;; Symbol key exactly like a Keyword one, and Symbol caches its
                      ;; hasheq in a field the way Keyword does — but here a symbol fell
                      ;; off the fast path entirely and took the generic pmap-get, whose
                      ;; descent is not type-specialized. symbol-hasheq is already
                      ;; memoized (on the symbol, and on its name cell), so this is the
                      ;; same one-field read the keyword arm above is.
                      ;;
                      ;; It is a real shape, not a curiosity: code that looks a keyword's
                      ;; SYMBOL form up in a map does it per lookup. honeysql's clause
                      ;; walk asks (get m (kw->sym k)) for all 92 clauses on every format.
                      ((symbol-t? k) (symbol-hasheq k))
                      (else #f))))
         (if (and h (fixnum? h))
             (let ((h (hamt-and h hmask)))
               (let lp ((node root) (shift 0))
                 (let* ((bit (hamt-sll 1 (hamt-and (hamt-sra h shift) 31)))
                        (bm (hnode-bm node)))
                   (if (hamt=? 0 (hamt-and bm bit)) d
                       (let ((child (vector-ref (hnode-arr node)
                                                (hamt-popcount (hamt-and bm (hamt-sub bit 1))))))
                         (cond ((hnode? child) (lp child (fx+ shift 5)))
                               ((hcoll? child)
                                (let ((p (assoc-jolt k (hcoll-alist child)))) (if p (cdr p) d)))
                               ((let ((ck (car child)))
                                  (cond ((keyword-t? ck) (and (keyword-t? k) (eq? ck k)))
                                        ((fixnum? ck) (and (fixnum? k) (fx=? ck k)))
                                        ((string? ck) (and (string? k) (string=? ck k)))
                                        (else (jolt=2 ck k))))
                                (cdr child))
                               (else d)))))))
             (node-get root 0 (key-hash k) k d)))))))
;; the stored (key . value) pair, or #f — `find` builds its entry from this so the
;; entry's key is the map's own key object, not the equal one probed with.
(define (pmap-entry-at m k)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (node-entry root 0 (key-hash k) k)
        (let ((i (amap-index root (vector-length root) k)))
          (and (fx>=? i 0) (cons (vector-ref root i) (vector-ref root (fx+ i 1))))))))
(define (pmap-contains? m k)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (not (eq? pmap-absent (node-get root 0 (key-hash k) k pmap-absent)))
        (fx>=? (amap-index root (vector-length root) k) 0))))

;; --- folds --------------------------------------------------------------------
;; The universal fold idiom across the runtime is `(pmap-fold m (lambda (k v a)
;; (cons ... a)) '())`, which accumulates in REVERSE visitation order. So that this
;; reconstructs the map's INSERTION order, pmap-fold visits an array-mode map's
;; slots from the last pair to the first; a hash-mode map visits HAMT order (its
;; iteration order is unspecified, so reverse-of-HAMT is equivalent and matches
;; prior behaviour). Use pmap-fold-fwd when building a value directly in
;; iteration order.
(define (pmap-fold m proc acc)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (node-fold root proc acc)
        (let loop ((i (fx- (vector-length root) 2)) (acc acc))
          (if (fx<? i 0) acc
              (loop (fx- i 2) (proc (sa-uvector-ref root i) (sa-uvector-ref root (sa-ufx+ i 1)) acc)))))))
;; visit entries in iteration (insertion) order — for code that builds a new map /
;; ordered value directly rather than via cons-accumulation.
(define (pmap-fold-fwd m proc acc)
  (let ((root (pmap-root m)))
    (if (hnode? root)
        (node-fold root proc acc)
        (let ((n (vector-length root)))
          (let loop ((i 0) (acc acc))
            (if (sa-ufx>=? i n) acc
                (loop (sa-ufx+ i 2) (proc (sa-uvector-ref root i) (sa-uvector-ref root (sa-ufx+ i 1)) acc))))))))

;; --- constructors -------------------------------------------------------------
;; map LITERAL ctor ({...}): RT.map/canBePAM — array map up to 8 entries, up to
;; 64 when every entry PAST the eighth is keyword-keyed (the first 8 slots may
;; hold any key type). Decided on the entries as written, the way the reference
;; reads init.length before building: a literal whose computed keys collapse
;; onto each other still lands in the mode its shape chose. Repeated keys keep
;; the first position and the last value (the reader has already refused a
;; literal that repeats a key).
(define (pam-literal-kvs? kvs n)       ; n = pair count
  (or (fx<=? n array-map-limit)
      (and (fx<=? n array-map-limit-kw)
           (let loop ((kvs (list-tail kvs (fx* 2 array-map-limit))))
             (or (null? kvs) (and (keyword-t? (car kvs)) (loop (cddr kvs))))))))
(define (hash-from-kvs kvs)
  (let loop ((m empty-pmap-hash) (kvs kvs))
    (if (null? kvs) m (loop (pmap-put-hash m (car kvs) (cadr kvs)) (cddr kvs)))))
(define (odd-kvs-error who)
  (throw-jvm (quote IllegalArgumentException) (string-append "odd number of " who)))
;; The one- and two-entry shapes are fixed arities so a dynamic literal of that
;; size ({:a x :b y}, the commonest) builds without a rest list.
(define jolt-hash-map
  (case-lambda
    (() empty-pmap)
    ((k v) (make-pmap (vector k v) 1))
    ((k1 v1 k2 v2)
     (if (jolt=2 k1 k2) (make-pmap (vector k1 v2) 1) (make-pmap (vector k1 v1 k2 v2) 2)))
    (kvs
     (let ((n2 (length kvs)))
       (when (fxodd? n2) (odd-kvs-error "map literal entries"))
       (let ((n (fxsra n2 1)))
         (if (pam-literal-kvs? kvs n)
             (amap-slots->pmap (amap-from-kvs kvs n))
             (hash-from-kvs kvs)))))))
;; array-map ctor: insertion-ordered (PersistentArrayMap, createAsIfByAssoc).
;; Never promotes — an explicit array-map stays an array map at any size.
(define (jolt-array-map-build kvs)
  (let ((n2 (length kvs)))
    (when (fxodd? n2) (odd-kvs-error "map entries"))
    (if (fx=? n2 0) empty-pmap (amap-slots->pmap (amap-from-kvs kvs (fxsra n2 1))))))
;; hash-map-build: CL function hash-map — always hash-ordered (JVM PersistentHashMap).
(define (jolt-hash-map-build kvs)
  (when (fxodd? (length kvs)) (odd-kvs-error "map entries"))
  (hash-from-kvs kvs))

(define-record-type pset (fields m (mutable hasheq)) (nongenerative chez-pset-v2))
(define make-pset
  (let ((raw (record-constructor (record-type-descriptor pset))))
    (lambda (m) (raw m 0))))
;; sets are ALWAYS hash-ordered (JVM PersistentHashSet), backed by pmap in hash mode.
(define empty-pset (make-pset empty-pmap-hash))   ; sets are ALWAYS hash-ordered (JVM PersistentHashSet)
(define (pset-conj s e) (if (pmap-contains? (pset-m s) e) s (make-pset (pmap-assoc (pset-m s) e e))))
(define (pset-disj s e) (make-pset (pmap-dissoc (pset-m s) e)))
(define (pset-contains? s e) (pmap-contains? (pset-m s) e))
;; A lookup answers with the element the set HOLDS, not with the equal probe key
;; it was handed — only the stored one carries the element's metadata, and callers
;; read it (farolero's return-from derefs (:on-stack? (meta block))). pset-conj
;; stores each element as both key and value, so the backing map's value IS that
;; element.
(define (pset-get s e default) (pmap-get (pset-m s) e default))
(define (pset-count s) (pmap-cnt (pset-m s)))
(define (pset-fold s proc acc) (pmap-fold (pset-m s) (lambda (k v a) (proc k a)) acc))
;; Fold over the stored (element . lookup-value) PAIRS. The two are normally the
;; same element, but a transient conj! of an element equal to one already in keeps
;; the old key and stores the new value (JVM ATransientSet.conj = impl.assoc(v,v)),
;; and persistent! carries that split through — so (seq s) yields the first and
;; (get s k) the second. Anything that REBUILDS a set element-by-element loses the
;; split and silently collapses get onto seq's element; the image walker rebuilds,
;; hence these two.
(define (pset-fold-pairs s proc acc) (pmap-fold (pset-m s) proc acc))
(define (pset-from-pairs pairs)   ; pairs: list of (key . lookup-value)
  (make-pset (fold-left (lambda (m p) (pmap-put-hash m (car p) (cdr p))) empty-pmap-hash pairs)))
(define (jolt-hash-set . xs) (let loop ((s empty-pset) (xs xs)) (if (null? xs) s (loop (pset-conj s (car xs)) (cdr xs)))))

;; ============================================================================
;; leaf ops the emitter lowers core/clojure fns to (mirrors native-ops)
;; ============================================================================
;; ---- conj arms: host types register here instead of set!-wrapping jolt-conj1 ----
(define jolt-conj1-arms '())
(define (register-conj-arm! pred handler)
  (set! jolt-conj1-arms (cons (cons pred handler) jolt-conj1-arms)))

;; Assoc every entry of `x` into map `m`, in x's own seq order. jolt models a map
;; entry as a 2-element vector, so that is the shape each element must have — the
;; JVM casts to java.util.Map$Entry here and this reports the same failure.
;; jolt-seq / seq-first / seq-more live in seq.ss, which rt.ss loads after this
;; file; forward references resolved at call time (cf. jolt-get-dispatch).
(define (conj-map-entries m x)
  (let loop ((s (jolt-seq x)) (acc m))
    (if (jolt-nil? s)
        acc
        (let ((e (seq-first s)))
          (if (and (pvec? e) (fx=? 2 (pvec-count e)))
              (loop (jolt-seq (seq-more s))
                    (pmap-assoc acc (pvec-nth-d e 0 jolt-nil) (pvec-nth-d e 1 jolt-nil)))
              (jolt-throw
               (jolt-host-throwable
                "java.lang.ClassCastException"
                (string-append "class " (guard (c (#t "?")) (jolt-class-name e))
                               " cannot be cast to class java.util.Map$Entry"))))))))

(define (jolt-conj1 coll x)
  (cond ((pvec? coll) (pvec-conj coll x))   ; nil is a valid vector/set element
        ((pset? coll) (pset-conj coll x))
        ;; a list/seq conjs by PREPENDING (seq.ss: cseq / empty-list). conj onto a
        ;; list stays a list, conj onto a lazy/realized seq yields a seq cell (a
        ;; Cons) — list?-preserving.
        ((cseq? coll) (if (cseq-list? coll) (cseq-list x coll) (cseq-realized x coll)))
        ((empty-list-t? coll) (cseq-list x jolt-nil))
        ;; conj onto a map takes a map ENTRY, a [k v] pair, or anything that seqs
        ;; INTO entries — which is what makes a record, a sorted map, another
        ;; native map and a bare seq of entries all work, since each of those seqs
        ;; into entries. Same shape as the JVM's APersistentMap.cons: the entry and
        ;; vector cases, then RT.seq and assoc each entry in turn. Asking "does it
        ;; seq into entries" rather than "is it one of the map types I know" is
        ;; what keeps conj from disagreeing with map? about what a map is: a record
        ;; answers true to map? and used to be rejected here.
        ((pmap? coll)
         (cond ((jolt-nil? x) coll)                                   ; (conj m nil) = m
               ((pmap? x) (pmap-fold-fwd x (lambda (k v m) (pmap-assoc m k v)) coll))   ; fast path, same rule
               ;; a vector on the right is one ENTRY, so it must be a pair — never
               ;; a sequence of entries. The JVM says so by name.
               ((pvec? x)
                (if (fx=? 2 (pvec-count x))
                    (pmap-assoc coll (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil))
                    (throw-jvm (quote IllegalArgumentException) "Vector arg to map conj must be a pair")))
               ;; jolt-seq raises the JVM's own "Don't know how to create ISeq
               ;; from: X" for a value that is not seqable at all.
               (else (conj-map-entries coll x))))
        (else (let loop ((as jolt-conj1-arms))
                (cond ((null? as)
                       (cond ((rec-coll-method coll "cons") => (lambda (m) (jolt-invoke m coll x)))
                             (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                                (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                                               " cannot be cast to class clojure.lang.IPersistentCollection"))))))
                      (((caar as) coll) ((cdar as) coll x))
                      (else (loop (cdr as))))))))
;; (conj) -> []; (conj nil a b ...) builds a list (conj prepending -> (b a)).
(define (jolt-conj . args)
  (if (null? args)
      (jolt-vector)
      (let ((coll (car args)) (xs (cdr args)))
        (cond
          ;; 1-arity returns the coll untouched — (conj nil) is nil
          ((null? xs) coll)
          ((jolt-nil? coll) (fold-left jolt-conj1 jolt-empty-list xs))
          (else (meta-carry coll (fold-left jolt-conj1 coll xs)))))))
(define jolt-conj2 (lambda (coll x)
  (if (jolt-nil? coll)
      (jolt-conj1 jolt-empty-list x)
      (meta-carry coll (jolt-conj1 coll x)))))

;; A host shim registers a type's get via register-get-arm! (handler: (coll k d) ->
;; value) instead of set!-wrapping jolt-get — disjoint coll types, checked before the
;; base map/set/vec/string cases (cf. register-hash-arm!).
;; Shared probe values for the collection fast paths. Built on demand, not at
;; load: jolt-empty-list and list->cseq come from seq.ss and make-jrec from
;; records.ss, both of which rt.ss loads after this file. Every arm registers
;; later still. See reject-fast-type-claim! (values.ss) for why each registry
;; passes its own subset rather than sharing one list — jolt-get answers records
;; but not strings, count/empty/seq answer strings but not records.
(define (probe-pvec) (jolt-vector))
(define (probe-pmap) (jolt-hash-map))
(define (probe-pset) empty-pset)
(define (probe-cseq) (list->cseq (list 1)))
;; Spliced, not listed: records.ss builds jrec-fast-type-probe well after
;; transients.ss has already registered a get arm, so before that point there is
;; simply no record to probe against.
(define (probe-jrecs)
  (probe-if-available (lambda () jrec-fast-type-probe)))

(define (get-fast-probes)
  (append (list (probe-pmap) (probe-pvec) (probe-pset)) (probe-jrecs)))
(define (get-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who pred (get-fast-probes) "the jolt-get fast path"))
(define jolt-get-arms '())
(define (register-get-arm! pred handler)
  (get-arm-reject-fast-type! 'register-get-arm! pred)
  (set! jolt-get-arms (cons (cons pred handler) jolt-get-arms)))
(define (jolt-get-base coll k d)
  (cond ((pmap? coll) (pmap-get coll k d))
        ((pset? coll) (pset-get coll k d))
        ((pvec? coll) (pvec-nth-d coll k d))
        ((string? coll) (let ((i (->idx k)))
                          (if (and (fixnum? i) (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i) d)))
        (else d)))
;; jrec? / jrec-ref live in records.ss (loaded later); these are forward references
;; resolved at call time. Check concrete types first, then records, then arms.
(define (jolt-get-dispatch coll k d)
  (cond ((pmap? coll) (pmap-fast-get coll k d))
        ((pvec? coll) (pvec-nth-d coll k d))
        ((pset? coll) (pset-get coll k d))
        ((jrec? coll) (jrec-ref coll k d))
        (else (let loop ((as jolt-get-arms))
                (cond ((null? as) (jolt-get-base coll k d))
                      (((caar as) coll) ((cdar as) coll k d))
                      (else (loop (cdr as))))))))
(define jolt-get
  (case-lambda
    ;; pmap is the dominant receiver: keep its path to one call (the arm holds
    ;; the pmap? test inline; everything else pays jolt-get-dispatch's walk).
    ((coll k)
     (if (pmap? coll) (pmap-fast-get coll k jolt-nil) (jolt-get-dispatch coll k jolt-nil)))
    ((coll k d)
     (if (pmap? coll) (pmap-fast-get coll k d) (jolt-get-dispatch coll k d)))))

;; A deftype implementing a clojure.lang collection interface (Indexed/Counted/
;; Associative/ILookup/ISeq/IPersistentCollection) carries the interface method
;; as an inline impl; the core collection fns fall back to it. find-method-any-
;; protocol / jolt-invoke load later — resolved at call time.
(define (rec-coll-method coll name)
  (and (jrec? coll) (find-method-any-protocol (jrec-tag coll) name)))

(define (jolt-nth-nil-idx! i)
  (when (jolt-nil? i)
    (jolt-throw (jolt-host-throwable "java.lang.NullPointerException" "nth index"))))
;; RT.nth / RT.count word their refusal off getClass().getSimpleName(), not the
;; canonical name — "Keyword", not "clojure.lang.Keyword"; "core$inc", not
;; "clojure.core$inc" (a fn class is top level, so only the package is stripped).
;; simple-class-name (java/records-interop.ss) is that rule, resolved at call time
;; the way jolt-class-name below it already is.
(define (unsupported-on-type who coll)
  (throw-jvm (quote UnsupportedOperationException)
             (string-append who " not supported on this type: "
                            (simple-class-name (jolt-class-name coll)))))
(define jolt-nth
  (case-lambda
     ((coll i)
      (jolt-nth-nil-idx! i)
      (let ((i (->idx i)))
        (cond ((jolt-nil? coll) jolt-nil)          ; RT.nth(nil, i) is nil at any index
        ;; pvec is the dominant receiver (every vector destructure lowers to nth);
        ;; keep it first so a destructure pays one call, not the walk below.
        ((pvec? coll) (pvec-nth! coll i))
              ((string? coll) (if (and (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i)
                                  (jolt-throw (jolt-host-throwable "java.lang.StringIndexOutOfBoundsException"
                                                                   (string-append "Index " (number->string i)
                                                                                  " out of bounds for length "
                                                                                  (number->string (string-length coll)))))))
              ((or (cseq? coll) (empty-list-t? coll)) (seq-nth coll i #f jolt-nil))
              ((rec-coll-method coll "nth") => (lambda (m) (jolt-invoke m coll i)))
              ;; RT.nth reads a CharSequence by charAt once Indexed has missed —
              ;; jrec-charseq-method (records.ss) resolves at call time.
              ((jrec-charseq-method coll "charAt") => (lambda (m) (jolt-invoke m coll i)))
              (else (unsupported-on-type "nth" coll)))))
    ((coll i d)
     (jolt-nth-nil-idx! i)
     (let ((i (->idx i)))
       (cond ((jolt-nil? coll) d)                 ; RT.nth(nil, i, notFound) is notFound
             ((pvec? coll) (pvec-nth-d coll i d))
             ((string? coll) (if (and (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i) d))
             ((or (cseq? coll) (empty-list-t? coll)) (seq-nth coll i #t d))
             ((rec-coll-method coll "nth") => (lambda (m) (jolt-invoke m coll i d)))
             ((jrec-charseq-method coll "charAt")
              => (lambda (m) (let ((n (jolt-count coll)))
                               (if (and (fx>=? i 0) (fx<? i n)) (jolt-invoke m coll i) d))))
             ;; NOT (else d). RT.nth's notFound answers an out-of-range INDEX on a
             ;; type that HAS nth; a type with no nth raises here exactly as it does
             ;; in the two-arity above. Returning d instead turned "you cannot index
             ;; this" into "there is nothing at index 0" — which is how a vector
             ;; destructure of a set bound nil, and (distinct #{1 2 3}) answered
             ;; (nil 3 2), silently, instead of raising.
             (else (unsupported-on-type "nth" coll)))))))

;; a count is an exact integer (JVM parity: count returns a long). jolt= is
;; exactness-aware, so this must be exact to match an exact integer literal:
;; (= 2 (count m)) -> 2 vs exact 2 -> true.
;; Arm registry for host-type count extensions (lazyseq, sorted, queue, array, etc.)
;; A host shim registers its type's count via register-count-arm! instead of
;; set!-wrapping jolt-count (cf. register-hash-arm!). Arms dispatch newest-
;; registration-first, matching the precedence the set! chains had. The builtin
;; types stay inline in jolt-count itself, so the arm walk only runs for
;; extension types. jolt-seq / jolt-empty? / jolt-conj1 / jolt-nth /
;; jolt-contains? still use set! chains — migrate them to this registry the
;; same way when touched (each needs its own bench guard; seq is the hottest).
(define (count-fast-probes)
  (list (probe-pvec) (probe-pmap) (probe-pset) "s" jolt-nil jolt-empty-list (probe-cseq)))
(define (count-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who pred (count-fast-probes) "the jolt-count fast path"))
(define jolt-count-arms '())
(define (register-count-arm! pred handler)
  (count-arm-reject-fast-type! 'register-count-arm! pred)
  (set! jolt-count-arms (cons (cons pred handler) jolt-count-arms)))
(define (jolt-count-base coll)
  ;; arms exhausted: a deftype/record counts through its declared method.
  (cond ((rec-coll-method coll "count") => (lambda (m) (jolt-invoke m coll)))
        (else (unsupported-on-type "count" coll))))
(define (jolt-count coll)
  (cond ((pvec? coll) (pvec-count coll))
        ((pmap? coll) (pmap-cnt coll))
        ((pset? coll) (pset-count coll))
        ((string? coll) (string-length coll))
        ((or (jolt-nil? coll) (empty-list-t? coll)) 0)
        ;; A vector-backed cell knows how many elements remain without walking
        ;; them: its backing vector's count less its own index. The JVM says the
        ;; same thing by having PersistentVector$ChunkedSeq implement Counted,
        ;; and RT.countFrom stops the moment it reaches a Counted cell —
        ;; `if(s instanceof Counted) return i + s.count()` — which is why the
        ;; walk below mirrors that shape rather than testing only the head: a
        ;; cons onto a vector seq gives plain cells in front of a countable one,
        ;; and those few steps should still end in the O(1) answer.
        ;;
        ;; Walking instead made (count (seq v)) linear where the reference is
        ;; constant: 18.8ms against 166ns over a million elements.
        ;;
        ;; Only the crest-#f shape qualifies. A ChunkedCons (crest set) is a
        ;; standalone chunk followed by an arbitrary, possibly lazy rest, so its
        ;; length is not known without forcing — ChunkedCons is deliberately not
        ;; Counted on the JVM either.
        ((cseq? coll)
         (let loop ((s coll) (n 0))
           (cond ((jolt-nil? s) n)
                 ((and (cseq-cvec s) (not (cseq-crest s)))
                  (fx+ n (fx- (pvec-count (cseq-cvec s)) (cseq-ci s))))
                 (else (loop (jolt-seq (seq-more s)) (fx+ n 1))))))
        (else (let loop ((as jolt-count-arms))
                (cond ((null? as) (jolt-count-base coll))
                      (((caar as) coll) ((cdar as) coll))
                      (else (loop (cdr as))))))))

(define (jolt-assoc1 coll k v)
  (cond ((pmap? coll) (pmap-assoc coll k v))
        ((pvec? coll) (pvec-assoc coll k v))
        ((jolt-nil? coll) (pmap-assoc empty-pmap k v))
        ((rec-coll-method coll "assoc") => (lambda (m) (jolt-invoke m coll k v)))
        (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                           (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                          " cannot be cast to class clojure.lang.Associative"))))))
(define (jolt-assoc coll . kvs)
  (meta-carry coll
    (let loop ((coll coll) (kvs kvs))
      (cond ((null? kvs) coll)
            ((null? (cdr kvs)) (throw-jvm (quote IllegalArgumentException) "assoc expects an even number of key/vals"))
            (else (loop (jolt-assoc1 coll (car kvs) (cadr kvs)) (cddr kvs)))))))
(define jolt-assoc3 (lambda (coll k v) (meta-carry coll (jolt-assoc1 coll k v))))

(define (jolt-dissoc coll . ks)
  (cond ((jolt-nil? coll) jolt-nil)
        ((pmap? coll) (meta-carry coll (fold-left pmap-dissoc coll ks)))
        (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                           (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                          " cannot be cast to class clojure.lang.IPersistentMap"))))))
(define jolt-dissoc2 (lambda (coll k)
  (cond ((jolt-nil? coll) jolt-nil)
        ((pmap? coll) (meta-carry coll (pmap-dissoc coll k)))
        (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                           (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                          " cannot be cast to class clojure.lang.IPersistentMap")))))))

(define (jolt-contains? coll k)
  (cond ((pmap? coll) (pmap-contains? coll k))
        ((pset? coll) (pset-contains? coll k))
        ((pvec? coll) (let ((k (->idx k))) (and (fixnum? k) (fx>=? k 0) (fx<? k (pvec-count coll)))))
        ((jolt-nil? coll) #f)
        ;; a string supports contains? by INDEX only (RT.contains: CharSequence +
        ;; Number key); any other key — or any unsupported type — is the JVM's
        ;; IllegalArgumentException.
        ((string? coll)
         (if (and (number? k) (exact? k) (integer? k))
             (and (>= k 0) (< k (string-length coll)))
             (jolt-throw (jolt-host-throwable
                          "java.lang.IllegalArgumentException"
                          "contains? not supported on type: java.lang.String"))))
        ((or (cseq? coll) (empty-list-t? coll) (number? coll) (boolean? coll)
             (keyword? coll) (jolt-symbol? coll) (char? coll))
         (jolt-throw (jolt-host-throwable
                      "java.lang.IllegalArgumentException"
                      (string-append "contains? not supported on type: "
                                     (guard (e (#t "?")) (jolt-class-name coll))))))
        ;; any type with no contains arm (lazy seqs, fns, atoms, …) is not
        ;; associative: throw like the eager-seq branch above, not a silent false.
        (else (let loop ((as jolt-contains-arms))
                (cond ((null? as)
                       (jolt-throw (jolt-host-throwable
                         "java.lang.IllegalArgumentException"
                         (string-append "contains? not supported on type: "
                                        (guard (e (#t "?")) (jolt-class-name coll))))))
                      (((caar as) coll) ((cdar as) coll k))
                      (else (loop (cdr as))))))))

;; ---- contains? arms: host types register here instead of set!-wrapping jolt-contains? ---
;; Widest of the four: contains? not only answers the collection types, it THROWS
;; for the scalars (a number, boolean, keyword, symbol or char is not
;; associative) before any arm is consulted. An arm claiming one of those would
;; never run.
(define (contains-fast-probes)
  (append (list (probe-pmap) (probe-pset) (probe-pvec) jolt-nil "s"
                (probe-cseq) jolt-empty-list 0 #t #f
                (keyword #f "k") (jolt-symbol #f "s") #\a)
          (probe-jrecs)))
(define (contains-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who pred (contains-fast-probes) "the jolt-contains? fast path"))
(define jolt-contains-arms '())
(define (register-contains-arm! pred handler)
  (contains-arm-reject-fast-type! 'register-contains-arm! pred)
  (set! jolt-contains-arms (cons (cons pred handler) jolt-contains-arms)))

;; ---- empty? arms: host types register here instead of set!-wrapping jolt-empty? ---
;; Arms dispatch newest-registration-first, matching the set!-chain precedence.
;; The built-in types stay inline; arms checked before the seq-based fallback.
(define (empty-fast-probes)
  (list jolt-nil (probe-pvec) (probe-pmap) (probe-pset) "s" jolt-empty-list (probe-cseq)))
(define (empty-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who pred (empty-fast-probes) "the jolt-empty? fast path"))
(define jolt-empty-arms '())
(define (register-empty-arm! pred handler)
  (empty-arm-reject-fast-type! 'register-empty-arm! pred)
  (set! jolt-empty-arms (cons (cons pred handler) jolt-empty-arms)))

(define (jolt-empty? coll)
  (cond ((jolt-nil? coll) #t)
        ((pvec? coll) (fx=? 0 (pvec-count coll)))
        ((pmap? coll) (fx=? 0 (pmap-cnt coll)))
        ((pset? coll) (fx=? 0 (pset-count coll)))
        ((string? coll) (fx=? 0 (string-length coll)))
        ((empty-list-t? coll) #t)
        ((cseq? coll) #f)                            ; a cseq is non-empty by construction
        ;; arm dispatch before the general seq-based fallback
        (else (let loop ((as jolt-empty-arms))
                (cond ((null? as) (jolt-nil? (jolt-seq coll)))
                      (((caar as) coll) ((cdar as) coll))
                      (else (loop (cdr as))))))))

(define (jolt-stack-throw coll)
  (jolt-throw (jolt-host-throwable
               "java.lang.ClassCastException"
               (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                              " cannot be cast to class clojure.lang.IPersistentStack"))))
(define (jolt-peek coll)
  (cond ((pvec? coll) (pvec-peek coll))
        ;; list peek = first; a non-list seq (range, a rest chain) is not an
        ;; IPersistentStack on the JVM
        ((and (cseq? coll) (cseq-list? coll)) (jolt-first coll))
        ((empty-list-t? coll) (jolt-first coll))
        ((jolt-nil? coll) jolt-nil)
        (else (jolt-stack-throw coll))))
(define (jolt-pop coll)
  (cond ((jolt-nil? coll) jolt-nil)                                 ; RT.pop(nil) is nil
        ((pvec? coll) (meta-carry coll (pvec-pop coll)))
        ((and (cseq? coll) (cseq-list? coll)) (meta-carry coll (jolt-rest coll)))
        ((empty-list-t? coll) (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "Can't pop empty list")))
        (else (jolt-stack-throw coll))))

;; ============================================================================
;; equality / hash hooks called from values.ss (jolt=2 / jolt-hash)
;; ============================================================================
(define (jolt-coll? x) (or (pvec? x) (pmap? x) (pset? x)))
;; SPIKE: hash fast-REJECT. Two colls whose CACHED hasheqs differ cannot be =
;; (hasheq is =-consistent, pinned by the hasheq gate), so answer #f without
;; walking. Only ever consults hashes both sides already paid for — an unset
;; side (0) skips straight to the structural walk, so nothing ever computes a
;; hash just to compare. Equal hashes prove nothing and fall through.
(define (hasheq-rejects? ha hb)
  (and (fixnum? ha) (fixnum? hb)
       (not (fx=? ha 0)) (not (fx=? hb 0))
       (not (fx=? ha hb))))

(define (jolt-coll=? a b)
  (cond
    ((and (pvec? a) (pvec? b))
     ;; leaf-run lockstep: a's and b's leaves needn't align (RRB tries have
     ;; partial leaves), so each stride is the min of both remaining runs.
     ;; Classic vectors take full-32 strides exactly as before.
     (if (hasheq-rejects? (pvec-hasheq a) (pvec-hasheq b))
         #f
     (let ((na (pvec-count a)))
       (and (fx=? na (pvec-count b))
            (let loop ((i 0))
              (if (fx=? i na) #t
                  (let*-values (((ca oa) (pv-leaf-for a i))
                                ((cb ob) (pv-leaf-for b i)))
                    (let ((run (fxmin (fx- (vector-length ca) oa)
                                      (fx- (vector-length cb) ob)
                                      (fx- na i))))
                      (let cloop ((j 0))
                        (if (fx>=? j run) (loop (fx+ i run))
                            ;; jolt=2, not the variadic jolt=: the latter conses a
                            ;; rest list on EVERY element compare.
                            (and (jolt=2 (vector-ref ca (fx+ oa j)) (vector-ref cb (fx+ ob j)))
                                 (cloop (fx+ j 1)))))))))))))
    ((and (pmap? a) (pmap? b))
     (if (hasheq-rejects? (pmap-hasheq a) (pmap-hasheq b))
         #f
         (and (fx=? (pmap-cnt a) (pmap-cnt b))
              (let ((ra (pmap-root a)) (rb (pmap-root b)))
                (if (not (or (hnode? ra) (hnode? rb)))
                    ;; two slot vectors: look each of a's keys up in b's slots
                    ;; directly — no closure per entry, the typed scan for the key
                    (let ((n (vector-length ra)) (nb (vector-length rb)))
                      (let loop ((i 0))
                        (or (fx>=? i n)
                            (let ((j (amap-index rb nb (vector-ref ra i))))
                              (and (fx>=? j 0)
                                   (jolt=2 (vector-ref ra (fx+ i 1)) (vector-ref rb (fx+ j 1)))
                                   (loop (fx+ i 2)))))))
                    (pmap-fold a (lambda (k v ok) (and ok (jolt=2 (pmap-fast-get b k pmap-absent) v))) #t))))))
    ((and (pset? a) (pset? b))
     (if (hasheq-rejects? (pset-hasheq a) (pset-hasheq b))
         #f
         (and (fx=? (pset-count a) (pset-count b))
              (pset-fold a (lambda (e ok) (and ok (pset-contains? b e))) #t))))
    (else #f)))
(define (jolt-coll-hash x)
  (cond
    ;; cached + leaf-run (hasheq.ss, runtime forward ref like hash-ordered)
    ((pvec? x) (pvec-hasheq-cached x))
    ;; maps hash as hashUnordered of entries; each entry contributes hash-ordered of [k v]
    ;; (APersistentMap.mapHasheq = Murmur3.hashUnordered, MapEntry.hasheq = ordered [k v])
    ((pmap? x)
     (or (and (not (= 0 (pmap-hasheq x))) (pmap-hasheq x))
         (let* ((result (pmap-fold x
                        (lambda (k v acc)
                          (cons (add32 (car acc) (entry-hasheq k v))
                                (fx+ (cdr acc) 1)))
                        (cons 0 0)))
                (h (mix-coll-hash (car result) (cdr result))))
           (pmap-hasheq-set! x h)
           h)))
    ;; sets hash as hashUnordered of elements
    ((pset? x)
     (or (and (not (= 0 (pset-hasheq x))) (pset-hasheq x))
         (let* ((result (pset-fold x
                        (lambda (e acc) (cons (+ (car acc) (jolt-hasheq e)) (fx+ (cdr acc) 1)))
                        (cons 0 0)))
                (h (mix-coll-hash (car result) (cdr result))))
           (pset-hasheq-set! x h)
           h)))
    (else (equal-hash x))))
