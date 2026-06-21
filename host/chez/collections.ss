;; Phase 1 (jolt-cf1q.2, inc 3a) — persistent collections on the Chez RT.
;;
;; The vector / map / set the emitted programs construct from literals and
;; operate on via the lowered leaf ops (conj/get/nth/count/assoc/...). Loaded by
;; rt.ss after values.ss; jolt=2 / jolt-hash (values.ss) call into the
;; jolt-coll? / jolt-coll=? / jolt-coll-hash hooks defined here (forward refs,
;; resolved at run time — nothing is CALLED during load).
;;
;; Phase note: the persistent vector is a copy-on-write Scheme vector and the
;; map/set are a bitmap HAMT (the structure 0c measured self-hostable). They live
;; in Scheme for the Phase-1 bootstrap; the 0c decision is to SELF-HOST them in
;; Clojure once core is up on Chez (Phase 3 shim shrink). Correctness, not perf,
;; is the Phase-1 gate.

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
;; persistent vector — copy-on-write over a Scheme vector
;; ============================================================================
;; A pvec carries an `ent` flag: #t marks a MAP ENTRY (the [k v] pair seq'd out
;; of a map). A map entry equals its [k v] vector and walks like one (nth/count/
;; seq/=/hash/print all read only `v`), but is NOT `vector?` and IS `map-entry?`
;; — matching Clojure's MapEntry (jolt-agw6). The flag defaults #f, so every
;; existing `(make-pvec v)` builds a plain vector; modifying an entry (conj/assoc)
;; likewise yields a plain vector.
(define-record-type pvec
  (fields v ent)
  (protocol (lambda (new) (case-lambda ((v) (new v #f)) ((v e) (new v e)))))
  (nongenerative chez-pvec-v1))
(define empty-pvec (make-pvec (vector)))
(define (jolt-vector . xs) (make-pvec (list->vector xs)))
(define (make-map-entry k v) (make-pvec (vector k v) #t))
(define (jolt-map-entry? x) (and (pvec? x) (pvec-ent x) #t))
(define (pvec-count p) (vector-length (pvec-v p)))
;; jolt models every number as a double, so vector indices arrive as flonums —
;; coerce an integer-valued index to a Scheme fixnum before bounds math.
(define (->idx i) (if (fixnum? i) i (if (flonum? i) (exact (floor i)) i)))
(define (pvec-nth-d p i d)
  (let ((v (pvec-v p)) (i (->idx i)))
    (if (and (fixnum? i) (fx>=? i 0) (fx<? i (vector-length v))) (vector-ref v i) d)))
(define (pvec-conj p x)
  (let* ((v (pvec-v p)) (n (vector-length v)) (out (make-vector (fx+ n 1))))
    (let loop ((i 0)) (when (fx<? i n) (vector-set! out i (vector-ref v i)) (loop (fx+ i 1))))
    (vector-set! out n x)
    (make-pvec out)))
(define (pvec-assoc p i x)            ; i in [0,count]; =count appends
  (let* ((v (pvec-v p)) (n (vector-length v)) (i (->idx i)))
    (cond ((and (fx>=? i 0) (fx<? i n)) (make-pvec (vec-set v i x)))
          ((fx=? i n) (pvec-conj p x))
          (else (error 'assoc "vector index out of bounds")))))
(define (pvec-peek p)
  (let ((n (pvec-count p))) (if (fx=? n 0) jolt-nil (vector-ref (pvec-v p) (fx- n 1)))))
(define (pvec-pop p)
  (let ((n (pvec-count p)))
    (if (fx=? n 0) (error 'pop "can't pop empty vector")
        (make-pvec (vec-copy-range (pvec-v p) 0 (fx- n 1))))))

;; ============================================================================
;; bitmap HAMT — keys hashed by jolt-hash, leaves compared by jolt=
;;   arr slot is one of: leaf (cons k v) | hnode (branch) | hcoll (hash bucket)
;; ============================================================================
(define-record-type hnode (fields bm arr) (nongenerative chez-hnode-v1))
(define-record-type hcoll (fields hash alist) (nongenerative chez-hcoll-v1))
(define empty-hnode (make-hnode 0 (vector)))
(define hmask #x3FFFFFFFFFFFFFF)       ; 58-bit non-negative hash window
(define max-shift 55)
(define (key-hash k) (fxand (jolt-hash k) hmask))
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
(define-record-type pmap (fields root cnt) (nongenerative chez-pmap-v1))
(define empty-pmap (make-pmap empty-hnode 0))
(define pmap-absent (list 'absent))    ; unique missing-key sentinel
(define (pmap-assoc m k v)
  (let* ((added (box #f)) (r (node-assoc (pmap-root m) 0 (key-hash k) k v added)))
    (make-pmap r (if (unbox added) (fx+ (pmap-cnt m) 1) (pmap-cnt m)))))
(define (pmap-dissoc m k)
  (let* ((removed (box #f)) (r (node-dissoc (pmap-root m) 0 (key-hash k) k removed)))
    (make-pmap r (if (unbox removed) (fx- (pmap-cnt m) 1) (pmap-cnt m)))))
(define (pmap-get m k default) (node-get (pmap-root m) 0 (key-hash k) k default))
(define (pmap-contains? m k) (not (eq? pmap-absent (node-get (pmap-root m) 0 (key-hash k) k pmap-absent))))
(define (pmap-fold m proc acc) (node-fold (pmap-root m) proc acc))
(define (jolt-hash-map . kvs)
  (let loop ((m empty-pmap) (kvs kvs))
    (cond ((null? kvs) m)
          ((null? (cdr kvs)) (error 'hash-map "odd number of map literal entries"))
          (else (loop (pmap-assoc m (car kvs) (cadr kvs)) (cddr kvs))))))

(define-record-type pset (fields m) (nongenerative chez-pset-v1))
(define empty-pset (make-pset empty-pmap))
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
               ((pmap? x) (pmap-fold x (lambda (k v m) (pmap-assoc m k v)) coll))   ; merge
               ((and (pvec? x) (fx=? 2 (pvec-count x)))
                (pmap-assoc coll (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil)))
               (else (error 'conj "conj on a map expects a [k v] pair or a map"))))
        (else (error 'conj "unsupported collection"))))
;; (conj) -> []; (conj nil a b ...) builds a list (conj prepending -> (b a)).
(define (jolt-conj . args)
  (if (null? args)
      (jolt-vector)
      (let ((coll (car args)) (xs (cdr args)))
        (if (jolt-nil? coll)
            (fold-left jolt-conj1 jolt-empty-list xs)
            (fold-left jolt-conj1 coll xs)))))

(define jolt-get
  (case-lambda
    ((coll k) (jolt-get coll k jolt-nil))
    ((coll k d)
     (cond ((pmap? coll) (pmap-get coll k d))
           ((pset? coll) (if (pset-contains? coll k) k d))
           ((pvec? coll) (pvec-nth-d coll k d))
           ((string? coll) (let ((i (->idx k)))
                             (if (and (fixnum? i) (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i) d)))
           (else d)))))

(define jolt-nth
  (case-lambda
    ((coll i)
     (let ((i (->idx i)))
       (cond ((pvec? coll) (let ((v (pvec-v coll)))
                             (if (and (fx>=? i 0) (fx<? i (vector-length v))) (vector-ref v i)
                                 (error 'nth "index out of bounds"))))
             ((string? coll) (string-ref coll i))
             ((or (cseq? coll) (empty-list-t? coll)) (seq-nth coll i #f jolt-nil))
             (else (error 'nth "unsupported collection")))))
    ((coll i d)
     (let ((i (->idx i)))
       (cond ((pvec? coll) (pvec-nth-d coll i d))
             ((string? coll) (if (and (fx>=? i 0) (fx<? i (string-length coll))) (string-ref coll i) d))
             ((or (cseq? coll) (empty-list-t? coll)) (seq-nth coll i #t d))
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
          (else (error 'count "uncountable")))))

(define (jolt-assoc1 coll k v)
  (cond ((pmap? coll) (pmap-assoc coll k v))
        ((pvec? coll) (pvec-assoc coll k v))
        ((jolt-nil? coll) (pmap-assoc empty-pmap k v))
        (else (error 'assoc "unsupported collection"))))
(define (jolt-assoc coll . kvs)
  (let loop ((coll coll) (kvs kvs))
    (cond ((null? kvs) coll)
          ((null? (cdr kvs)) (error 'assoc "assoc expects an even number of key/vals"))
          (else (loop (jolt-assoc1 coll (car kvs) (cadr kvs)) (cddr kvs))))))

(define (jolt-dissoc coll . ks)
  (cond ((jolt-nil? coll) jolt-nil)
        ((pmap? coll) (fold-left pmap-dissoc coll ks))
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
  (cond ((pvec? coll) (pvec-pop coll))
        ((cseq? coll) (jolt-rest coll))                              ; list pop = rest
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
