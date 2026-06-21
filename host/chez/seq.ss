;; Phase 1 (jolt-cf1q.2, inc 3b) — the seq tier on the Chez RT.
;;
;; One lazy-capable node (cseq) models Clojure's list, cons, and lazy seq — all
;; print as (...), all sequential-= to each other AND to vectors. `jolt-seq`
;; coerces any seqable (vector/map/set/string/list/seq/nil) to a cseq or nil.
;; The empty seq is a distinct value (jolt-empty-list) that prints "()" — Clojure
;; (rest [1]) is () not nil, (seq []) is nil. The higher-order fns
;; (map/filter/reduce/into/remove) apply their fn argument through `jolt-invoke`,
;; so a procedure, a keyword, or a collection all work as the fn (IFn dispatch).
;;
;; Loaded by rt.ss after collections.ss. values.ss / collections.ss reach the
;; jolt-sequential? / seq=? / seq-hash hooks defined here as forward refs (nothing
;; is CALLED during load).

;; ============================================================================
;; the seq node + the empty-seq sentinel
;; ============================================================================
;; head : the realized first element. tail : EITHER a realized seq (cseq |
;; jolt-nil) when forced? is #t, OR a 0-arg thunk producing one when forced? is
;; #f. Forcing memoizes (set the tail to the produced seq, flip forced?).
;; list? : #t when this cell is a PersistentList node (list literal / (list ...)
;; / cons / reverse / conj-onto-list) vs a lazy or vector-backed seq cell — the
;; only thing that distinguishes a list from any other realized seq on this host,
;; since one record type backs both (clojure.core/list? — jolt-75sv). The marker
;; lives on the cell, so (rest a-list) / (seq a-vector) / (map …) yield plain seq
;; cells and are not list?.
(define-record-type cseq (fields head (mutable tail) (mutable forced?) list?) (nongenerative chez-cseq-v2))
(define (cseq-realized head tail) (make-cseq head tail #t #f))   ; tail already a seq
(define (cseq-lazy head tail-thunk) (make-cseq head tail-thunk #f #f))
(define (cseq-list head tail) (make-cseq head tail #t #t))       ; a PersistentList node
(define (seq-first s) (cseq-head s))
(define (seq-more s)                  ; force the tail; returns a seq (cseq | jolt-nil)
  (if (cseq-forced? s) (cseq-tail s)
      (let ((t ((cseq-tail s)))) (cseq-tail-set! s t) (cseq-forced?-set! s #t) t)))

;; The empty seq (Clojure's empty list ()), distinct from nil.
(define-record-type empty-list-t (fields) (nongenerative empty-list-v1))
(define jolt-empty-list (make-empty-list-t))

;; reduced (jolt-y6mv): a box a reducing fn returns to stop reduce early. The
;; reduce machinery below unwraps it; (deref a-reduced) / unreduced also read it.
;; reduced?/reduced are def-var!'d into clojure.core in natives-seq.ss.
(define-record-type jolt-reduced (fields val) (nongenerative jolt-reduced-v1))

;; ============================================================================
;; jolt-seq — coerce a seqable to a non-empty seq, or jolt-nil when empty
;; ============================================================================
(define (list->cseq xs)               ; Scheme list -> realized cseq chain (jolt-nil if empty)
  (if (null? xs) jolt-nil (cseq-realized (car xs) (list->cseq (cdr xs)))))
(define (vec->seq v i)                 ; lazy index seq over a persistent vector
  (if (fx>=? i (pvec-count v)) jolt-nil
      (cseq-lazy (pvec-nth-d v i jolt-nil) (lambda () (vec->seq v (fx+ i 1))))))
(define (str->seq s i)
  (if (fx>=? i (string-length s)) jolt-nil
      (cseq-lazy (string-ref s i) (lambda () (str->seq s (fx+ i 1))))))
(define (jolt-seq x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((empty-list-t? x) jolt-nil)
    ((cseq? x) x)
    ((pvec? x) (vec->seq x 0))
    ((pmap? x) (list->cseq (pmap-fold x (lambda (k v a) (cons (make-map-entry k v) a)) '())))
    ((pset? x) (list->cseq (pset-fold x cons '())))
    ((string? x) (str->seq x 0))
    (else (error 'seq "not seqable" x))))

(define (jolt-sequential? x) (or (pvec? x) (cseq? x) (empty-list-t? x)))
(define (seq->list s)                  ; force a finite seq to a Scheme list
  (let loop ((s (jolt-seq s)) (acc '()))
    (if (jolt-nil? s) (reverse acc) (loop (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))

;; ============================================================================
;; the seq leaf ops the emitter lowers core fns to
;; ============================================================================
(define (jolt-first x) (let ((s (jolt-seq x))) (if (jolt-nil? s) jolt-nil (seq-first s))))
(define (jolt-rest x)                  ; () when the seq has 0/1 elements (NOT nil)
  (let ((s (jolt-seq x)))
    (if (jolt-nil? s) jolt-empty-list
        (let ((m (seq-more s))) (if (jolt-nil? m) jolt-empty-list m)))))
(define (jolt-next x)                  ; nil when the rest is empty
  ;; next = (seq (rest x)): the rest must be RE-SEQ'd so an empty tail collapses to
  ;; nil. seq-more on a lazy seq (e.g. map's) forces to jolt-empty-list, which is
  ;; truthy — returning it raw made (next 1-elem-lazy-seq) non-nil, so butlast and
  ;; other (if (next s) ...) loops over a lazy seq ran one step too far.
  (let ((s (jolt-seq x))) (if (jolt-nil? s) jolt-nil (jolt-seq (seq-more s)))))
;; Only the HEAD cell carries the list marker — (rest a-list)/(next a-list) return
;; the unmarked tail, so they are seqs and not list? (rest-of-a-list is a non-list
;; seq). cons/list/reverse/conj therefore mark
;; just the cell they create.
;;
;; cons always yields a list — (list? (cons x anything)) is true (cons
;; onto a vector/seq/nil all report list?).
(define (jolt-cons x coll) (cseq-list x (jolt-seq coll)))
;; Scheme list -> a jolt PersistentList: head is a list cell, the tail chain is
;; plain seq cells. For (list …) and quoted list literals (the emitter lowers
;; '(a b) to (jolt-list a b)).
(define (jolt-list . xs)
  (if (null? xs) jolt-empty-list (cseq-list (car xs) (list->cseq (cdr xs)))))
;; reverse yields a list (seed: (list? (reverse coll)) is always true). Build a
;; plain seq chain, then mark its head as a list cell.
(define (jolt-reverse coll)
  (let loop ((s (jolt-seq coll)) (acc jolt-empty-list))
    (if (jolt-nil? s)
        (if (empty-list-t? acc) acc (cseq-list (seq-first acc) (seq-more acc)))
        (loop (jolt-seq (seq-more s)) (cseq-realized (seq-first s) (if (empty-list-t? acc) jolt-nil acc))))))
(define (jolt-last coll) (let loop ((s (jolt-seq coll)) (last jolt-nil))
                           (if (jolt-nil? s) last (loop (jolt-seq (seq-more s)) (seq-first s)))))
;; nth over a seq (walks; forces lazily). default? selects the 3-arg behavior.
(define (seq-nth coll i default? d)
  (if (fx<? i 0) (if default? d (error 'nth "index out of bounds"))
      (let loop ((s (jolt-seq coll)) (i i))
        (cond ((jolt-nil? s) (if default? d (error 'nth "index out of bounds")))
              ((fx=? i 0) (seq-first s))
              (else (loop (jolt-seq (seq-more s)) (fx- i 1)))))))

;; value-position arithmetic (the higher-order forms: (reduce + []), (apply * xs)).
;; Scheme's +/-/*// already implement the JVM-parity numeric tower: exact+exact ->
;; exact, exact/exact -> Ratio, any flonum -> flonum. Identities (+)=0 / (*)=1 are
;; exact, matching exact integer arithmetic. The hot path uses the inlined native
;; ops, not these.
(define (jolt-add . xs) (apply + xs))
(define (jolt-sub . xs) (apply - xs))
(define (jolt-mul . xs) (apply * xs))
(define (jolt-div . xs) (apply / xs))

;; ============================================================================
;; IFn dispatch — the dynamic "value as fn" fallback. A callee that the emitter
;; can't statically resolve to a procedure (a keyword/coll/proc held in a local)
;; routes here. Off the arithmetic/self-recursion hot path by construction.
;; ============================================================================
(define (jolt-invoke f . args)
  (cond
    ((procedure? f) (apply f args))
    ((keyword? f) (apply jolt-get (car args) f (cdr args)))   ; (:k m [d]) -> (get m :k [d])
    ((jolt-coll? f) (apply jolt-get f args))                  ; (coll k [d]) -> (get coll k [d])
    ((jolt-transient? f) (apply jolt-get f args))             ; a transient vec/map/set is callable on the JVM
    (else (error 'invoke "not a fn" f))))

;; ============================================================================
;; map / filter / reduce / into / remove + range / take / concat / apply
;; ============================================================================
(define (any-nil? seqs) (and (pair? seqs) (or (jolt-nil? (car seqs)) (any-nil? (cdr seqs)))))
;; An EMPTY seq result is () (jolt-empty-list), NOT nil — Clojure's (map f []) is
;; an empty seq, so (= () (map f [])) is true and (nil? (map f [])) is false.
;; jolt-empty-list seqs back to nil, so it stays a valid lazy-tail terminator for
;; the non-empty case (printing / seq= / reduce all walk via jolt-seq).
(define (map-seq f s)
  (if (jolt-nil? s) jolt-empty-list
      (cseq-lazy (jolt-invoke f (seq-first s)) (lambda () (map-seq f (jolt-seq (seq-more s)))))))
(define (map-seq* f seqs)              ; multi-collection map; stops at the shortest
  (if (any-nil? seqs) jolt-empty-list
      (cseq-lazy (apply jolt-invoke f (map seq-first seqs))
                 (lambda () (map-seq* f (map (lambda (s) (jolt-seq (seq-more s))) seqs))))))
(define (jolt-map f . colls)
  (if (null? (cdr colls))
      (map-seq f (jolt-seq (car colls)))
      (map-seq* f (map jolt-seq colls))))

(define (filter-seq pred s keep)
  (let loop ((s s))
    (cond ((jolt-nil? s) jolt-empty-list)   ; empty result is () (see map-seq)
          ((eq? keep (jolt-truthy? (jolt-invoke pred (seq-first s))))
           (cseq-lazy (seq-first s) (lambda () (filter-seq pred (jolt-seq (seq-more s)) keep))))
          (else (loop (jolt-seq (seq-more s)))))))
(define (jolt-filter pred coll) (filter-seq pred (jolt-seq coll) #t))
(define (jolt-remove pred coll) (filter-seq pred (jolt-seq coll) #f))

;; honors `reduced`: a reducing fn that returns (reduced x) stops the fold and
;; unwraps to x (so does a reduced INIT). Checked at entry, so the value returned
;; by the last step is unwrapped on the next turn before the seq is consulted.
(define (reduce-seq f acc s)
  (cond
    ((jolt-reduced? acc) (jolt-reduced-val acc))
    ((jolt-nil? s) acc)
    (else (reduce-seq f (jolt-invoke f acc (seq-first s)) (jolt-seq (seq-more s))))))
(define jolt-reduce
  (case-lambda
    ((f coll) (let ((s (jolt-seq coll)))
                (if (jolt-nil? s) (jolt-invoke f)          ; (reduce f []) -> (f)
                    (reduce-seq f (seq-first s) (jolt-seq (seq-more s))))))
    ((f init coll) (reduce-seq f init (jolt-seq coll)))))

(define (jolt-into to from) (reduce-seq (lambda (acc x) (jolt-conj1 acc x)) to (jolt-seq from)))

(define (range-from n) (cseq-lazy n (lambda () (range-from (+ n 1)))))
(define (range-bounded n end step)
  (if (if (> step 0.0) (< n end) (> n end))
      (cseq-lazy n (lambda () (range-bounded (+ n step) end step)))
      jolt-nil))
;; numeric tower (jolt-n6al): exact 0/1 defaults so (range 3) yields exact ints
;; (= JVM longs); flonum args still produce flonums (Scheme arithmetic preserves).
(define jolt-range
  (case-lambda
    (() (range-from 0))
    ((end) (range-bounded 0 end 1))
    ((start end) (range-bounded start end 1))
    ((start end step) (range-bounded start end step))))

(define (jolt-take n coll)
  (let ((n (->idx n)))
    (let loop ((n n) (s (jolt-seq coll)))
      (if (or (fx<=? n 0) (jolt-nil? s)) jolt-nil
          (cseq-lazy (seq-first s) (lambda () (loop (fx- n 1) (jolt-seq (seq-more s)))))))))
(define (jolt-drop n coll)
  (let loop ((n (->idx n)) (s (jolt-seq coll)))
    (if (or (fx<=? n 0) (jolt-nil? s)) (if (jolt-nil? s) jolt-empty-list s)
        (loop (fx- n 1) (jolt-seq (seq-more s))))))

;; lazily append seq a then the seqable produced by the thunk `brest` — the rest
;; is NOT forced until a is exhausted, so concat is fully lazy (Clojure semantics).
;; This matters for a self-referential lazy-cat (fib = (lazy-cat [0 1] (map + (rest
;; fib) fib))): forcing the rest eagerly at construction would read fib before its
;; def binds, memoizing the tail as empty.
(define (concat2 a brest)
  (if (jolt-nil? a) (jolt-seq (brest))
      (cseq-lazy (seq-first a) (lambda () (concat2 (jolt-seq (seq-more a)) brest)))))
(define (jolt-concat . colls)
  (cond ((null? colls) jolt-empty-list)
        ((null? (cdr colls)) (jolt-seq (car colls)))
        (else (concat2 (jolt-seq (car colls)) (lambda () (apply jolt-concat (cdr colls)))))))

;; (apply f a b ... coll): spread the trailing seqable into the call.
(define (jolt-apply f . args)
  (let* ((r (reverse args)) (spread (seq->list (jolt-seq (car r)))) (fixed (reverse (cdr r))))
    (apply jolt-invoke f (append fixed spread))))

;; ============================================================================
;; numeric predicates / identity — usable in fn AND value position (map/filter).
;; Return Scheme #t/#f (= jolt true/false). All-flonum model: coerce to an exact
;; integer for the parity tests.
;; ============================================================================
(define (jolt-even? n) (fx=? 0 (fxand (->idx n) 1)))
(define (jolt-odd? n) (fx=? 1 (fxand (->idx n) 1)))
(define (jolt-pos? n) (> n 0))
(define (jolt-neg? n) (< n 0))
(define (jolt-zero? n) (= n 0))
(define (jolt-identity x) x)

;; ============================================================================
;; keys / vals — return seqs (nil on the empty map), HAMT-iteration order
;; ============================================================================
(define (jolt-keys m) (if (jolt-nil? m) jolt-nil (list->cseq (pmap-fold m (lambda (k v a) (cons k a)) '()))))
(define (jolt-vals m) (if (jolt-nil? m) jolt-nil (list->cseq (pmap-fold m (lambda (k v a) (cons v a)) '()))))

;; ============================================================================
;; sequential equality + hash (hooks called from values.ss / collections.ss);
;; consistent with the persistent vector's element-wise =/hash so a vector and a
;; list of the same elements are jolt= and hash alike.
;; ============================================================================
(define (seq=? a b)
  (let loop ((sa (jolt-seq a)) (sb (jolt-seq b)))
    (cond ((and (jolt-nil? sa) (jolt-nil? sb)) #t)
          ((or (jolt-nil? sa) (jolt-nil? sb)) #f)
          ((jolt= (seq-first sa) (seq-first sb)) (loop (jolt-seq (seq-more sa)) (jolt-seq (seq-more sb))))
          (else #f))))
(define (seq-hash x)
  (let loop ((s (jolt-seq x)) (h 1))
    (if (jolt-nil? s) (bitwise-and h hmask)
        (loop (jolt-seq (seq-more s)) (bitwise-and (+ (* 31 h) (key-hash (seq-first s))) hmask)))))
