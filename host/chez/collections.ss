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
;; small immutable-vector helpers (manual; avoid stdlib arg-order ambiguity)
;; ============================================================================
(define (vec-copy-range v start end)
  (let ((out (make-vector (fx- end start))))
    (let loop ((i start))
      (when (fx<? i end) (vector-set! out (fx- i start) (vector-ref v i)) (loop (fx+ i 1))))
    out))
(define (vec-insert v i x)            ; copy of v with x spliced in at index i
  (let* ((n (vector-length v)) (out (make-vector (fx+ n 1))))
    (let loop ((j 0)) (when (fx<? j i) (vector-set! out j (vector-ref v j)) (loop (fx+ j 1))))
    (vector-set! out i x)
    (let loop ((j i)) (when (fx<? j n) (vector-set! out (fx+ j 1) (vector-ref v j)) (loop (fx+ j 1))))
    out))
(define (vec-set v i x)               ; functional update at index i
  (let ((out (vec-copy-range v 0 (vector-length v)))) (vector-set! out i x) out))
(define (vec-remove v i)              ; copy of v with index i dropped
  (let* ((n (vector-length v)) (out (make-vector (fx- n 1))))
    (let loop ((j 0)) (when (fx<? j i) (vector-set! out j (vector-ref v j)) (loop (fx+ j 1))))
    (let loop ((j (fx+ i 1))) (when (fx<? j n) (vector-set! out (fx- j 1) (vector-ref v j)) (loop (fx+ j 1))))
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
(define-record-type (pvec mk-pvec pvec?)
  (fields cnt shift root tail ent) (nongenerative chez-pvec-v2))

;; trailing helpers over Scheme vectors used by the trie
(define (vec-snoc v x)                 ; copy v with x appended
  (let* ((n (vector-length v)) (out (make-vector (fx+ n 1))))
    (let loop ((i 0)) (when (fx<? i n) (vector-set! out i (vector-ref v i)) (loop (fx+ i 1))))
    (vector-set! out n x) out))
(define (vec-drop-last v) (vec-copy-range v 0 (fx- (vector-length v) 1)))
(define (vec-take v n) (vec-copy-range v 0 n))
(define (vec-set-or-snoc v i x)        ; replace index i, or append when i = length
  (let ((n (vector-length v))) (if (fx<? i n) (vec-set v i x) (vec-snoc v x))))

(define (pv-tailoff cnt)
  (if (fx<? cnt pv-width) 0 (fxsll (fxsra (fx- cnt 1) pv-bits) pv-bits)))
;; the 32-chunk Scheme vector holding index i (the tail or a trie leaf)
(define (pv-chunk-for p i)
  (if (fx>=? i (pv-tailoff (pvec-cnt p)))
      (pvec-tail p)
      (let loop ((node (pvec-root p)) (level (pvec-shift p)))
        (if (fx>? level 0)
            (loop (vector-ref node (fxand (fxsra i level) pv-mask)) (fx- level pv-bits))
            node))))

;; jolt models every number as a double, so vector indices arrive as flonums —
;; coerce an integer-valued index to a Scheme fixnum before bounds math.
(define (->idx i) (if (fixnum? i) i (if (flonum? i) (exact (floor i)) i)))
(define (pvec-count p) (pvec-cnt p))
(define (pvec-nth-d p i d)
  (let ((i (->idx i)))
    (if (and (fixnum? i) (fx>=? i 0) (fx<? i (pvec-cnt p)))
        (vector-ref (pv-chunk-for p i) (fxand i pv-mask))
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
    (if (fx<? (fx- cnt (pv-tailoff cnt)) pv-width)
        ;; room in the tail
        (mk-pvec (fx+ cnt 1) shift (pvec-root p) (vec-snoc (pvec-tail p) x) #f)
        ;; tail full: push it into the trie, start a fresh tail
        (let ((tail-node (pvec-tail p)))
          (if (fx>? (fxsra cnt pv-bits) (fxsll 1 shift))
              ;; root overflow: grow the trie a level
              (mk-pvec (fx+ cnt 1) (fx+ shift pv-bits)
                       (vector (pvec-root p) (pv-new-path shift tail-node))
                       (vector x) #f)
              (mk-pvec (fx+ cnt 1) shift
                       (pv-push-tail cnt shift (pvec-root p) tail-node)
                       (vector x) #f))))))

(define (pv-assoc-trie level node i x)
  (if (fx=? level 0)
      (vec-set node (fxand i pv-mask) x)
      (let ((subidx (fxand (fxsra i level) pv-mask)))
        (vec-set node subidx (pv-assoc-trie (fx- level pv-bits) (vector-ref node subidx) i x)))))
(define (pvec-assoc p i x)            ; i in [0,count]; =count appends
  (let ((i (->idx i)) (cnt (pvec-cnt p)))
    (cond
      ((fx=? i cnt) (pvec-conj p x))
      ((and (fx>=? i 0) (fx<? i cnt))
       (if (fx>=? i (pv-tailoff cnt))
           (mk-pvec cnt (pvec-shift p) (pvec-root p)
                    (vec-set (pvec-tail p) (fxand i pv-mask) x) #f)
           (mk-pvec cnt (pvec-shift p)
                    (pv-assoc-trie (pvec-shift p) (pvec-root p) i x) (pvec-tail p) #f)))
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
      ((fx=? cnt 0) (error 'pop "can't pop empty vector"))
      ((fx=? cnt 1) empty-pvec)
      ((fx>? (fx- cnt (pv-tailoff cnt)) 1)
       (mk-pvec (fx- cnt 1) shift (pvec-root p) (vec-drop-last (pvec-tail p)) #f))
      (else
       (let* ((new-tail (pv-chunk-for p (fx- cnt 2)))
              (popped (pv-pop-tail cnt shift (pvec-root p)))
              (new-root (or popped pv-empty-node)))
         (if (and (fx>? shift pv-bits) (fx<? (vector-length new-root) 2))
             (mk-pvec (fx- cnt 1) (fx- shift pv-bits)
                      (if (fx=? 0 (vector-length new-root)) pv-empty-node (vector-ref new-root 0))
                      new-tail #f)
             (mk-pvec (fx- cnt 1) shift new-root new-tail #f)))))))

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
          (let* ((chunk (pv-chunk-for p i)) (clen (vector-length chunk)))
            (let cloop ((j 0) (k i))
              (if (and (fx<? j clen) (fx<? k cnt))
                  (begin (vector-set! out k (vector-ref chunk j)) (cloop (fx+ j 1) (fx+ k 1)))
                  (loop k))))
          out))))
(define (jolt-vector . xs) (make-pvec (list->vector xs)))
(define (make-map-entry k v) (make-pvec (vector k v) #t))
(define (jolt-map-entry? x) (and (pvec? x) (pvec-ent x) #t))

;; ============================================================================
;; bitmap HAMT — keys hashed by jolt-hash, leaves compared by jolt=
;;   arr slot is one of: leaf (cons k v) | hnode (branch) | hcoll (hash bucket)
;; ============================================================================
(define-record-type hnode (fields bm arr) (nongenerative chez-hnode-v1))
(define-record-type hcoll (fields hash alist) (nongenerative chez-hcoll-v1))
(define empty-hnode (make-hnode 0 (vector)))
(define hmask #x3FFFFFFFFFFFFFF)       ; 58-bit non-negative hash window
(define max-shift 55)
;; bitwise-and (not fxand): jolt-hash is set!-decorated per type (records/inst/
;; sorted return their own hash) and Chez's equal-hash can yield a BIGNUM, so a
;; key's hash isn't guaranteed to be a fixnum. Masking with the 58-bit window via
;; the generic bitwise-and always lands in fixnum range for the HAMT's fx slicing.
(define (key-hash k) (bitwise-and (jolt-hash k) hmask))
(define (chunk h shift) (fxand (fxsra h shift) 31))
(define (bitpos h shift) (fxsll 1 (chunk h shift)))
(define (popcount n) (let loop ((n n) (c 0)) (if (fx=? n 0) c (loop (fxand n (fx- n 1)) (fx+ c 1)))))
(define (arr-index bm bit) (popcount (fxand bm (fx- bit 1))))

;; jolt= alist ops (for hash-collision buckets)
(define (assoc-jolt k al) (cond ((null? al) #f) ((jolt= (caar al) k) (car al)) (else (assoc-jolt k (cdr al)))))
(define (alist-replace k v al) (if (jolt= (caar al) k) (cons (cons k v) (cdr al)) (cons (car al) (alist-replace k v (cdr al)))))
(define (alist-remove k al) (cond ((null? al) '()) ((jolt= (caar al) k) (cdr al)) (else (cons (car al) (alist-remove k (cdr al))))))

;; split two leaves that collided at `shift` into a subtree (or hcoll if the
;; full hashes are equal / the hash is exhausted).
(define (split-leaf shift ek ev h k v)
  (let ((eh (key-hash ek)))
    (if (or (fx>? shift max-shift) (fx=? eh h))
        (make-hcoll h (list (cons ek ev) (cons k v)))
        (let ((ei (chunk eh shift)) (ni (chunk h shift)))
          (if (fx=? ei ni)
              (make-hnode (fxsll 1 ei) (vector (split-leaf (fx+ shift 5) ek ev h k v)))
              (let ((eb (fxsll 1 ei)) (nb (fxsll 1 ni)))
                (if (fx<? ei ni)
                    (make-hnode (fxior eb nb) (vector (cons ek ev) (cons k v)))
                    (make-hnode (fxior eb nb) (vector (cons k v) (cons ek ev))))))))))

(define (node-assoc node shift h k v added)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)) (arr (hnode-arr node)))
    (if (fx=? 0 (fxand bm bit))
        (begin (set-box! added #t)
               (make-hnode (fxior bm bit) (vec-insert arr (arr-index bm bit) (cons k v))))
        (let* ((i (arr-index bm bit)) (child (vector-ref arr i)))
          (cond
            ((hnode? child) (make-hnode bm (vec-set arr i (node-assoc child (fx+ shift 5) h k v added))))
            ((hcoll? child)
             (let ((al (hcoll-alist child)))
               (if (assoc-jolt k al)
                   (make-hnode bm (vec-set arr i (make-hcoll (hcoll-hash child) (alist-replace k v al))))
                   (begin (set-box! added #t)
                          (make-hnode bm (vec-set arr i (make-hcoll (hcoll-hash child) (cons (cons k v) al))))))))
            ((jolt= (car child) k) (make-hnode bm (vec-set arr i (cons k v))))   ; replace
            (else (set-box! added #t)
                  (make-hnode bm (vec-set arr i (split-leaf (fx+ shift 5) (car child) (cdr child) h k v)))))))))

(define (node-get node shift h k default)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)))
    (if (fx=? 0 (fxand bm bit)) default
        (let ((child (vector-ref (hnode-arr node) (arr-index bm bit))))
          (cond ((hnode? child) (node-get child (fx+ shift 5) h k default))
                ((hcoll? child) (let ((p (assoc-jolt k (hcoll-alist child)))) (if p (cdr p) default)))
                ((jolt= (car child) k) (cdr child))
                (else default))))))

(define (node-dissoc node shift h k removed)
  (let* ((bit (bitpos h shift)) (bm (hnode-bm node)) (arr (hnode-arr node)))
    (if (fx=? 0 (fxand bm bit)) node
        (let* ((i (arr-index bm bit)) (child (vector-ref arr i)))
          (cond
            ((hnode? child) (make-hnode bm (vec-set arr i (node-dissoc child (fx+ shift 5) h k removed))))
            ((hcoll? child)
             (if (assoc-jolt k (hcoll-alist child))
                 (begin (set-box! removed #t)
                        (let ((nal (alist-remove k (hcoll-alist child))))
                          (cond ((null? nal) (make-hnode (fxand bm (fxnot bit)) (vec-remove arr i)))
                                ((null? (cdr nal)) (make-hnode bm (vec-set arr i (car nal))))   ; collapse to leaf
                                (else (make-hnode bm (vec-set arr i (make-hcoll (hcoll-hash child) nal)))))))
                 node))
            ((jolt= (car child) k)
             (set-box! removed #t) (make-hnode (fxand bm (fxnot bit)) (vec-remove arr i)))
            (else node))))))

(define (node-fold node proc acc)     ; (proc k v acc) over every leaf
  (let ((arr (hnode-arr node)))
    (let loop ((i 0) (acc acc))
      (if (fx<? i (vector-length arr))
          (let ((child (vector-ref arr i)))
            (loop (fx+ i 1)
                  (cond ((hnode? child) (node-fold child proc acc))
                        ((hcoll? child)
                         (let cl ((al (hcoll-alist child)) (a acc))
                           (if (null? al) a (cl (cdr al) (proc (caar al) (cdar al) a)))))
                        (else (proc (car child) (cdr child) acc)))))
          acc))))

;; ============================================================================
;; persistent map / set over the HAMT
;; ============================================================================
;; A small map keeps its keys in INSERTION order (Clojure's PersistentArrayMap),
;; converting to hash order past a threshold (PersistentHashMap). The HAMT root
;; always backs the values; `order` is the auxiliary insertion-order key list when
;; the map is in array mode, or #f once it has grown into hash mode. Equality and
;; hashing fold over the entries order-independently, so this only affects
;; iteration order (seq/keys/vals/print), matching the JVM.
(define-record-type pmap (fields root cnt order) (nongenerative chez-pmap-v2))
(define empty-pmap (make-pmap empty-hnode 0 '()))          ; {} = empty array map
(define empty-pmap-hash (make-pmap empty-hnode 0 #f))      ; hash-order backing (sets)
(define pmap-absent (list 'absent))    ; unique missing-key sentinel
;; PersistentArrayMap threshold: assoc of a new key promotes to hash mode once the
;; map already holds 8 entries (array.length >= 16 in the reference).
(define array-map-limit 8)
(define (append-key ord k) (append ord (list k)))
(define (remove-key ord k) (let loop ((o ord)) (cond ((null? o) '()) ((jolt= (car o) k) (cdr o)) (else (cons (car o) (loop (cdr o)))))))

;; growth rule (PersistentArrayMap.assoc): a new key appends to the order while in
;; array mode under the limit; otherwise the result is hash-ordered. Replacing an
;; existing key (or assoc onto an already-hash map) keeps the current order.
(define (pmap-assoc m k v)
  (let* ((added (box #f)) (r (node-assoc (pmap-root m) 0 (key-hash k) k v added))
         (cnt (pmap-cnt m)) (ord (pmap-order m)))
    (if (unbox added)
        (if (and ord (fx<? cnt array-map-limit))
            (make-pmap r (fx+ cnt 1) (append-key ord k))
            (make-pmap r (fx+ cnt 1) #f))
        (make-pmap r cnt ord))))
;; force-ordered / force-hash inserts for rebuilding a map whose final mode is
;; already decided (array-map ctor, transient persistent!).
(define (pmap-put-ordered m k v)
  (let* ((added (box #f)) (r (node-assoc (pmap-root m) 0 (key-hash k) k v added)))
    (if (unbox added)
        (make-pmap r (fx+ (pmap-cnt m) 1) (append-key (or (pmap-order m) '()) k))
        (make-pmap r (pmap-cnt m) (pmap-order m)))))
(define (pmap-put-hash m k v)
  (let* ((added (box #f)) (r (node-assoc (pmap-root m) 0 (key-hash k) k v added)))
    (make-pmap r (if (unbox added) (fx+ (pmap-cnt m) 1) (pmap-cnt m)) #f)))
(define (pmap->hash m) (if (pmap-order m) (make-pmap (pmap-root m) (pmap-cnt m) #f) m))
(define (pmap-dissoc m k)
  (let* ((removed (box #f)) (r (node-dissoc (pmap-root m) 0 (key-hash k) k removed))
         (ord (pmap-order m)))
    (if (unbox removed)
        (make-pmap r (fx- (pmap-cnt m) 1) (if ord (remove-key ord k) #f))
        m)))
(define (pmap-get m k default) (node-get (pmap-root m) 0 (key-hash k) k default))
(define (pmap-contains? m k) (not (eq? pmap-absent (node-get (pmap-root m) 0 (key-hash k) k pmap-absent))))
;; The universal fold idiom across the runtime is `(pmap-fold m (lambda (k v a)
;; (cons ... a)) '())`, which accumulates in REVERSE visitation order. So that this
;; reconstructs the map's INSERTION order, pmap-fold visits an array-mode map's keys
;; in reverse insertion order; a hash-mode map visits HAMT order (its iteration
;; order is unspecified, so reverse-of-HAMT is equivalent and matches prior
;; behaviour). Use pmap-fold-fwd when building a value directly in iteration order.
(define (pmap-fold m proc acc)
  (let ((ord (pmap-order m)))
    (if ord
        (fold-right (lambda (k a) (proc k (pmap-get m k jolt-nil) a)) acc ord)  ; visits last->first
        (node-fold (pmap-root m) proc acc))))
;; visit entries in iteration (insertion) order — for code that builds a new map /
;; ordered value directly rather than via cons-accumulation.
(define (pmap-fold-fwd m proc acc)
  (let ((ord (pmap-order m)))
    (if ord
        (let loop ((ks ord) (a acc))
          (if (null? ks) a (loop (cdr ks) (proc (car ks) (pmap-get m (car ks) jolt-nil) a))))
        (node-fold (pmap-root m) proc acc))))
;; map LITERAL ({...}): array map up to 8 entries, hash map beyond (RT.map).
(define (jolt-hash-map . kvs)
  (let loop ((m empty-pmap) (kvs kvs))
    (cond ((null? kvs) (if (fx>? (pmap-cnt m) array-map-limit) (pmap->hash m) m))
          ((null? (cdr kvs)) (error 'hash-map "odd number of map literal entries"))
          (else (loop (pmap-put-ordered m (car kvs) (cadr kvs)) (cddr kvs))))))
;; array-map ctor: insertion-ordered regardless of size (createAsIfByAssoc).
(define (jolt-array-map-build kvs)
  (let loop ((m empty-pmap) (kvs kvs))
    (cond ((null? kvs) m)
          ((null? (cdr kvs)) (error 'array-map "odd number of map entries"))
          (else (loop (pmap-put-ordered m (car kvs) (cadr kvs)) (cddr kvs))))))
;; hash-map ctor: hash order (PersistentHashMap).
(define (jolt-hash-map-build kvs)
  (let loop ((m empty-pmap-hash) (kvs kvs))
    (cond ((null? kvs) m)
          ((null? (cdr kvs)) (error 'hash-map "odd number of map entries"))
          (else (loop (pmap-put-hash m (car kvs) (cadr kvs)) (cddr kvs))))))

(define-record-type pset (fields m) (nongenerative chez-pset-v1))
(define empty-pset (make-pset empty-pmap-hash))            ; sets are hash-ordered
(define (pset-conj s e) (if (pmap-contains? (pset-m s) e) s (make-pset (pmap-assoc (pset-m s) e e))))
(define (pset-disj s e) (make-pset (pmap-dissoc (pset-m s) e)))
(define (pset-contains? s e) (pmap-contains? (pset-m s) e))
(define (pset-count s) (pmap-cnt (pset-m s)))
(define (pset-fold s proc acc) (pmap-fold (pset-m s) (lambda (k v a) (proc k a)) acc))
(define (jolt-hash-set . xs) (let loop ((s empty-pset) (xs xs)) (if (null? xs) s (loop (pset-conj s (car xs)) (cdr xs)))))

;; ============================================================================
;; leaf ops the emitter lowers core/clojure fns to (mirrors native-ops)
;; ============================================================================
(define (jolt-conj1 coll x)
  (cond ((pvec? coll) (pvec-conj coll x))   ; nil is a valid vector/set element
        ((pset? coll) (pset-conj coll x))
        ;; a list/seq conjs by PREPENDING (seq.ss: cseq / empty-list). conj onto a
        ;; list stays a list, conj onto a lazy/realized seq yields a seq cell (a
        ;; Cons) — list?-preserving.
        ((cseq? coll) (if (cseq-list? coll) (cseq-list x coll) (cseq-realized x coll)))
        ((empty-list-t? coll) (cseq-list x jolt-nil))
        ((pmap? coll)
         (cond ((jolt-nil? x) coll)                                   ; (conj m nil) = m
               ((pmap? x) (pmap-fold-fwd x (lambda (k v m) (pmap-assoc m k v)) coll))   ; merge in x's order
               ((and (pvec? x) (fx=? 2 (pvec-count x)))
                (pmap-assoc coll (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil)))
               (else (error 'conj "conj on a map expects a [k v] pair or a map"))))
        ((rec-coll-method coll "cons") => (lambda (m) (jolt-invoke m coll x)))
        (else (error 'conj "unsupported collection"))))
;; (conj) -> []; (conj nil a b ...) builds a list (conj prepending -> (b a)).
(define (jolt-conj . args)
  (if (null? args)
      (jolt-vector)
      (let ((coll (car args)) (xs (cdr args)))
        (if (jolt-nil? coll)
            (fold-left jolt-conj1 jolt-empty-list xs)
            (meta-carry coll (fold-left jolt-conj1 coll xs))))))

;; A host shim registers a type's get via register-get-arm! (handler: (coll k d) ->
;; value) instead of set!-wrapping jolt-get — disjoint coll types, checked before the
;; base map/set/vec/string cases (cf. register-hash-arm!).
(define jolt-get-arms '())
(define (register-get-arm! pred handler)
  (set! jolt-get-arms (cons (cons pred handler) jolt-get-arms)))
(define (jolt-get-base coll k d)
  (cond ((pmap? coll) (pmap-get coll k d))
        ((pset? coll) (if (pset-contains? coll k) k d))
        ((pvec? coll) (pvec-nth-d coll k d))
        ((string? coll) (let ((i (->idx k)))
                          (if (and (fixnum? i) (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i) d)))
        (else d)))
;; jrec? / jrec-ref live in records.ss (loaded later); these are forward references
;; resolved at call time. A record field read is the hottest get, so check it first
;; and skip the get-arm walk.
(define (jolt-get-dispatch coll k d)
  (if (jrec? coll)
      (jrec-ref coll k d)
      (let loop ((as jolt-get-arms))
        (cond ((null? as) (jolt-get-base coll k d))
              (((caar as) coll) ((cdar as) coll k d))
              (else (loop (cdr as)))))))
(define jolt-get
  (case-lambda
    ((coll k) (jolt-get-dispatch coll k jolt-nil))
    ((coll k d) (jolt-get-dispatch coll k d))))

;; A deftype implementing a clojure.lang collection interface (Indexed/Counted/
;; Associative/ILookup/ISeq/IPersistentCollection) carries the interface method
;; as an inline impl; the core collection fns fall back to it. find-method-any-
;; protocol / jolt-invoke load later — resolved at call time.
(define (rec-coll-method coll name)
  (and (jrec? coll) (find-method-any-protocol (jrec-tag coll) name)))

(define jolt-nth
  (case-lambda
    ((coll i)
     (let ((i (->idx i)))
       (cond ((pvec? coll) (let ((v (pvec-v coll)))
                             (if (and (fx>=? i 0) (fx<? i (vector-length v))) (vector-ref v i)
                                 (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "index out of bounds")))))
             ((string? coll) (if (and (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i)
                                 (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "index out of bounds"))))
             ((or (cseq? coll) (empty-list-t? coll)) (seq-nth coll i #f jolt-nil))
             ((rec-coll-method coll "nth") => (lambda (m) (jolt-invoke m coll i)))
             (else (error 'nth "unsupported collection")))))
    ((coll i d)
     (let ((i (->idx i)))
       (cond ((pvec? coll) (pvec-nth-d coll i d))
             ((string? coll) (if (and (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i) d))
             ((or (cseq? coll) (empty-list-t? coll)) (seq-nth coll i #t d))
             ((rec-coll-method coll "nth") => (lambda (m) (jolt-invoke m coll i d)))
             (else d))))))

;; a count is an exact integer (JVM parity: count returns a long). jolt= is
;; exactness-aware, so this must be exact to match an exact integer literal:
;; (= 2 (count m)) -> 2 vs exact 2 -> true.
(define (jolt-count coll)
  (begin
    (cond ((pvec? coll) (pvec-count coll))
          ((pmap? coll) (pmap-cnt coll))
          ((pset? coll) (pset-count coll))
          ((string? coll) (string-length coll))
          ((jolt-nil? coll) 0)
          ((empty-list-t? coll) 0)
          ((cseq? coll) (let loop ((s coll) (n 0))   ; walk (forces a finite seq)
                          (if (jolt-nil? s) n (loop (jolt-seq (seq-more s)) (fx+ n 1)))))
          ((rec-coll-method coll "count") => (lambda (m) (jolt-invoke m coll)))
          (else (error 'count "uncountable")))))

(define (jolt-assoc1 coll k v)
  (cond ((pmap? coll) (pmap-assoc coll k v))
        ((pvec? coll) (pvec-assoc coll k v))
        ((jolt-nil? coll) (pmap-assoc empty-pmap k v))
        ((rec-coll-method coll "assoc") => (lambda (m) (jolt-invoke m coll k v)))
        (else (error 'assoc "unsupported collection"))))
(define (jolt-assoc coll . kvs)
  (meta-carry coll
    (let loop ((coll coll) (kvs kvs))
      (cond ((null? kvs) coll)
            ((null? (cdr kvs)) (error 'assoc "assoc expects an even number of key/vals"))
            (else (loop (jolt-assoc1 coll (car kvs) (cadr kvs)) (cddr kvs)))))))

(define (jolt-dissoc coll . ks)
  (cond ((jolt-nil? coll) jolt-nil)
        ((pmap? coll) (meta-carry coll (fold-left pmap-dissoc coll ks)))
        (else (error 'dissoc "unsupported collection"))))

(define (jolt-contains? coll k)
  (cond ((pmap? coll) (pmap-contains? coll k))
        ((pset? coll) (pset-contains? coll k))
        ((pvec? coll) (let ((k (->idx k))) (and (fixnum? k) (fx>=? k 0) (fx<? k (pvec-count coll)))))
        ((jolt-nil? coll) #f)
        (else #f)))

(define (jolt-empty? coll)
  (cond ((jolt-nil? coll) #t)
        ((pvec? coll) (fx=? 0 (pvec-count coll)))
        ((pmap? coll) (fx=? 0 (pmap-cnt coll)))
        ((pset? coll) (fx=? 0 (pset-count coll)))
        ((string? coll) (fx=? 0 (string-length coll)))
        ((empty-list-t? coll) #t)
        ((cseq? coll) #f)                            ; a cseq is non-empty by construction
        (else (error 'empty? "unsupported collection"))))

(define (jolt-peek coll)
  (cond ((pvec? coll) (pvec-peek coll))
        ((or (cseq? coll) (empty-list-t? coll)) (jolt-first coll))   ; list peek = first
        ((jolt-nil? coll) jolt-nil) (else (error 'peek "unsupported collection"))))
(define (jolt-pop coll)
  (cond ((pvec? coll) (meta-carry coll (pvec-pop coll)))
        ((cseq? coll) (meta-carry coll (jolt-rest coll)))            ; list pop = rest
        ((empty-list-t? coll) (error 'pop "can't pop empty list"))
        (else (error 'pop "unsupported collection"))))

;; ============================================================================
;; equality / hash hooks called from values.ss (jolt=2 / jolt-hash)
;; ============================================================================
(define (jolt-coll? x) (or (pvec? x) (pmap? x) (pset? x)))
(define (jolt-coll=? a b)
  (cond
    ((and (pvec? a) (pvec? b))
     (let ((va (pvec-v a)) (vb (pvec-v b)))
       (and (fx=? (vector-length va) (vector-length vb))
            (let loop ((i 0))
              (or (fx=? i (vector-length va))
                  (and (jolt= (vector-ref va i) (vector-ref vb i)) (loop (fx+ i 1))))))))
    ((and (pmap? a) (pmap? b))
     (and (fx=? (pmap-cnt a) (pmap-cnt b))
          (pmap-fold a (lambda (k v ok) (and ok (jolt= (pmap-get b k pmap-absent) v))) #t)))
    ((and (pset? a) (pset? b))
     (and (fx=? (pset-count a) (pset-count b))
          (pset-fold a (lambda (e ok) (and ok (pset-contains? b e))) #t)))
    (else #f)))
(define (jolt-coll-hash x)
  (cond
    ((pvec? x)
     (let ((v (pvec-v x)))
       (let loop ((i 0) (h 1))
         (if (fx=? i (vector-length v)) (bitwise-and h hmask)
             (loop (fx+ i 1) (bitwise-and (+ (* 31 h) (key-hash (vector-ref v i))) hmask))))))
    ;; maps/sets hash order-independently (sum), consistent with unordered =
    ((pmap? x) (bitwise-and (pmap-fold x (lambda (k v a) (+ a (fxxor (key-hash k) (key-hash v)))) 0) hmask))
    ((pset? x) (bitwise-and (pset-fold x (lambda (e a) (+ a (key-hash e))) 0) hmask))))
