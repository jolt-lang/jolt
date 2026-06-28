;; The seq tier on the Chez RT.
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
;; since one record type backs both (clojure.core/list?). The marker
;; lives on the cell, so (rest a-list) / (seq a-vector) / (map …) yield plain seq
;; cells and are not list?.
;; cvec/ci: for a vector-backed seq cell, the backing vector and this cell's
;; element index — so it is a real chunked-seq (chunked-seq? true, chunk-first
;; hands out a 32-element block, chunk-rest is the seq at the next block) and
;; reduce iterates the vector directly with no per-element cells.
;; cvec is #f for every other seq; stored as two fields (not a cons) so a vector
;; seq cell costs no extra allocation. The rest of the seq layer ignores them, so
;; first/rest/count/printing are unchanged.
(define-record-type cseq (fields head (mutable tail) (mutable forced?) list? cvec ci) (nongenerative chez-cseq-v3))
(define (cseq-realized head tail) (make-cseq head tail #t #f #f 0))   ; tail already a seq
(define (cseq-lazy head tail-thunk) (make-cseq head tail-thunk #f #f #f 0))
(define (cseq-list head tail) (make-cseq head tail #t #t #f 0))       ; a PersistentList node
(define (cseq-vec head tail-thunk v i) (make-cseq head tail-thunk #f #f v i)) ; vector-backed
(define (seq-first s) (cseq-head s))
(define (seq-more s)                  ; force the tail; returns a seq (cseq | jolt-nil)
  (if (cseq-forced? s) (cseq-tail s)
      (let ((t ((cseq-tail s)))) (cseq-tail-set! s t) (cseq-forced?-set! s #t) t)))

;; The empty seq (Clojure's empty list ()), distinct from nil. The (unused) field
;; defeats Chez's interning of fieldless records, so an empty list carrying
;; metadata (an `empty`/`pop`/`with-meta` result) is a distinct identity from the
;; shared jolt-empty-list — otherwise its meta would leak onto every ().
(define-record-type empty-list-t (fields _) (nongenerative empty-list-v2))
(define (fresh-empty-list) (make-empty-list-t #f))
(define jolt-empty-list (fresh-empty-list))

;; reduced: a box a reducing fn returns to stop reduce early. The
;; reduce machinery below unwraps it; (deref a-reduced) / unreduced also read it.
;; reduced?/reduced are def-var!'d into clojure.core in natives-seq.ss.
(define-record-type jolt-reduced (fields val) (nongenerative jolt-reduced-v1))

;; ============================================================================
;; jolt-seq — coerce a seqable to a non-empty seq, or jolt-nil when empty
;; ============================================================================
(define (list->cseq xs)               ; Scheme list -> realized cseq chain (jolt-nil if empty)
  (if (null? xs) jolt-nil (cseq-realized (car xs) (list->cseq (cdr xs)))))
(define (vec->seq v i)                 ; chunked index seq over a persistent vector
  (if (fx>=? i (pvec-count v)) jolt-nil
      (cseq-vec (pvec-nth-d v i jolt-nil) (lambda () (vec->seq v (fx+ i 1))) v i)))
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
;; rest = Clojure's more(): the tail as a (possibly empty) seq, NOT nil, and
;; WITHOUT realizing it. A forced cseq (list / realized chain) hands back its tail
;; directly. An UNFORCED tail (vector / string / lazy-seq cell) is returned as a
;; deferred seq so (rest s) does not realize the next node — matching Clojure,
;; where (rest (iterate f x)) does not call f and a side-effecting lazy seq is
;; realized one element at a time. next = (seq (rest s)) still realizes one.
;; jolt-make-lazy-seq (lazy-bridge.ss) resolves at call time.
(define (jolt-rest x)
  (let ((s (jolt-seq x)))
    (cond
      ((jolt-nil? s) jolt-empty-list)
      ((cseq-forced? s) (let ((m (cseq-tail s))) (if (jolt-nil? m) jolt-empty-list m)))
      ;; the lazyseq forces to a seq (cseq | nil); an empty realized lazyseq is
      ;; still a sequence value, printing "()" (see lazy-bridge.ss), so (rest s)
      ;; is never nil even when the tail is empty. jolt-seq coerces seq-more's
      ;; result (which may be jolt-empty-list, e.g. map's tail) back to cseq | nil,
      ;; the contract force-lazyseq relies on — else (seq (rest s)) of an empty
      ;; tail yields a truthy empty-list and walkers (distinct, dedupe) overrun.
      (else (jolt-make-lazy-seq (lambda () (jolt-seq (seq-more s))))))))
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
(define (jolt-min . xs) (apply min xs))
(define (jolt-max . xs) (apply max xs))

;; --- unchecked (Java long) arithmetic: wrap to signed 64 bits ----------------
;; Clojure's unchecked-* (and +/-/* under *unchecked-math*) are long ops that
;; WRAP on overflow; jolt's checked arithmetic is arbitrary-precision. These
;; truncate to the low 64 bits as a two's-complement signed long. Chez fixnums are
;; 61-bit, so wrapping uses bignum bit ops + a mask (no fx fast path). The backend
;; emits the binary jolt-unc* for :long-typed unchecked ops; the variadic
;; clojure.core/unchecked-* fns reduce through them.
(define unc-mask64 #xFFFFFFFFFFFFFFFF)
(define unc-2^63   #x8000000000000000)
(define unc-2^64   #x10000000000000000)
(define (jolt-wrap64 x)
  (let ((m (bitwise-and (if (and (number? x) (exact? x) (integer? x)) x (exact (floor x))) unc-mask64)))
    (if (>= m unc-2^63) (- m unc-2^64) m)))
;; unchecked-* only WRAP integer (long) math; on a flonum OR ratio operand they
;; are an ordinary numeric op, since *unchecked-math* never wraps a non-long —
;; Clojure's unchecked-add falls back to regular arithmetic for non-primitives:
;; (unchecked-multiply 1.5 2.0) => 3.0, (unchecked-add 2/3 2/3) => 4/3, not a
;; truncated long. (test.check's rand-double is (* double-unit shifted), and
;; gen/ratio sums ratios, both under *unchecked-math*.) Wrap iff both are exact
;; integers.
(define (unc-int? x) (and (exact? x) (integer? x)))
(define (jolt-uncadd2 a b) (if (and (unc-int? a) (unc-int? b)) (jolt-wrap64 (+ a b)) (+ a b)))
(define (jolt-uncsub2 a b) (if (and (unc-int? a) (unc-int? b)) (jolt-wrap64 (- a b)) (- a b)))
(define (jolt-uncmul2 a b) (if (and (unc-int? a) (unc-int? b)) (jolt-wrap64 (* a b)) (* a b)))
(define (jolt-uncinc x)    (if (unc-int? x) (jolt-wrap64 (+ x 1)) (+ x 1)))
(define (jolt-uncdec x)    (if (unc-int? x) (jolt-wrap64 (- x 1)) (- x 1)))
(define (jolt-uncneg x)    (if (unc-int? x) (jolt-wrap64 (- x)) (- x)))
(define (jolt-unchecked-add . xs) (if (null? xs) 0 (fold-left jolt-uncadd2 (car xs) (cdr xs))))
(define (jolt-unchecked-mul . xs) (if (null? xs) 1 (fold-left jolt-uncmul2 (car xs) (cdr xs))))
(define (jolt-unchecked-sub . xs)
  (cond ((null? xs) 0) ((null? (cdr xs)) (jolt-uncneg (car xs))) (else (fold-left jolt-uncsub2 (car xs) (cdr xs)))))
(define (jolt-unchecked-div a b) (quotient (jolt-wrap64 a) (jolt-wrap64 b)))
(define (jolt-unchecked-rem a b) (remainder (jolt-wrap64 a) (jolt-wrap64 b)))
;; the clojure.core/unchecked-* vars are def-var!'d in natives-seq.ss (def-var! is
;; defined after this file loads).

;; --- ^long ops that tolerate a full 64-bit value -----------------------------
;; A ^long is 64-bit but a Chez fixnum is only 61-bit, so the backend's fast fx
;; ops would raise on a value past 2^60 (e.g. a long from the PRNG / wrapping
;; arithmetic). These take the fx fast path when the operands ARE fixnums and fall
;; back to the generic op otherwise — so ^long comparisons / quot / min etc. on a
;; full-width long stay correct. Macros (define-syntax) so the fast path inlines.
(define-syntax define-l-binop
  (syntax-rules ()
    ((_ name fxop genop)
     (define-syntax name
       (syntax-rules ()
         ((_ a b) (let ((x a) (y b))
                    (if (and (fixnum? x) (fixnum? y)) (fxop x y) (genop x y)))))))))
(define-l-binop jolt-l<  fx<?  <)
(define-l-binop jolt-l<= fx<=? <=)
(define-l-binop jolt-l>  fx>?  >)
(define-l-binop jolt-l>= fx>=? >=)
(define-l-binop jolt-l=  fx=?  =)
(define-l-binop jolt-l-min fxmin min)
(define-l-binop jolt-l-max fxmax max)
(define-l-binop jolt-l-quot fxquotient quotient)
(define-l-binop jolt-l-rem  fxremainder remainder)
(define-l-binop jolt-l-mod  fxmodulo modulo)
(define-syntax jolt-l-inc (syntax-rules () ((_ a) (let ((x a)) (if (fixnum? x) (fx1+ x) (+ x 1))))))
(define-syntax jolt-l-dec (syntax-rules () ((_ a) (let ((x a)) (if (fixnum? x) (fx1- x) (- x 1))))))

;; ============================================================================
;; IFn dispatch — the dynamic "value as fn" fallback. A callee that the emitter
;; can't statically resolve to a procedure (a keyword/coll/proc held in a local)
;; routes here. Off the arithmetic/self-recursion hot path by construction.
;; ============================================================================
(define (jolt-invoke f . args)
  (cond
    ((procedure? f) (apply f args))
    ((keyword? f) (apply jolt-get (car args) f (cdr args)))   ; (:k m [d]) -> (get m :k [d])
    ((jolt-symbol? f) (apply jolt-get (car args) f (cdr args)))   ; ('s m [d]) -> (get m 's [d])
    ((jolt-coll? f) (apply jolt-get f args))                  ; (coll k [d]) -> (get coll k [d])
    ((jolt-transient? f) (apply jolt-get f args))             ; a transient vec/map/set is callable on the JVM
    ;; a record/reify implementing clojure.lang.IFn is callable: dispatch to its
    ;; inline `invoke` method with the value itself as the leading `this`.
    ((and (jrec? f) (find-method-any-protocol (jrec-tag f) "invoke"))
     => (lambda (m) (apply jolt-invoke m f args)))
    ((and (reified-methods f) (hashtable-ref (reified-methods f) "invoke" #f))
     => (lambda (m) (apply jolt-invoke m f args)))
    ;; calling a non-fn: a ClassCastException naming the operator, thrown via
    ;; jolt-throw so it is catchable and carries the throw-site continuation for a
    ;; stack trace.
    (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                        (string-append (guard (e (#t "value")) (jolt-pr-str f))
                                       " cannot be cast to clojure.lang.IFn"))))))

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
;; reduce a vector's backing store directly by index from element i — no per-
;; element seq cells. Honors `reduced`. The chunked-seq fast path.
(define (vec-reduce f acc v i)
  (let ((n (pvec-count v)) (raw (pvec-v v)))
    (let loop ((i i) (acc acc))
      (cond ((jolt-reduced? acc) (jolt-reduced-val acc))
            ((fx>=? i n) acc)
            (else (loop (fx+ i 1) (jolt-invoke f acc (vector-ref raw i))))))))
(define (reduce-seq f acc s)
  (cond
    ((jolt-reduced? acc) (jolt-reduced-val acc))
    ((jolt-nil? s) acc)
    ;; a vector-backed (chunked) seq reduces its vector directly, in a tight loop.
    ((and (cseq? s) (cseq-cvec s)) (vec-reduce f acc (cseq-cvec s) (cseq-ci s)))
    (else (reduce-seq f (jolt-invoke f acc (seq-first s)) (jolt-seq (seq-more s))))))
(define jolt-reduce
  (case-lambda
    ((f coll) (let ((s (jolt-seq coll)))
                (if (jolt-nil? s) (jolt-invoke f)          ; (reduce f []) -> (f)
                    (reduce-seq f (seq-first s) (jolt-seq (seq-more s))))))
    ((f init coll)
     ;; IReduceInit: a reify/record with its own `reduce` method drives the
     ;; reduction (reduce f init (reify clojure.lang.IReduceInit (reduce [_ f i] ...))).
     (cond
       ((and (jreify? coll) (reified-methods coll)
             (hashtable-ref (reified-methods coll) "reduce" #f))
        => (lambda (m) (let ((r (jolt-invoke m coll f init)))
                         (if (jolt-reduced? r) (jolt-reduced-val r) r))))
       (else (reduce-seq f init (jolt-seq coll)))))))

;; Fold through a transient so a pvec/pmap/pset target is built in O(n): a
;; persistent pvec-conj copies its whole backing vector each step, making a naive
;; fold O(n^2) (and into/vec/mapv/filterv all route here). jolt-transient-new
;; falls back to a copy-on-write wrapper for other targets (lists, sorted colls,
;; nil), so those keep the old per-step jolt-conj behaviour.
(define (jolt-into to from)
  (meta-carry to
    (jolt-persistent! (reduce-seq (lambda (t x) (jolt-conj! t x)) (jolt-transient-new to) (jolt-seq from)))))

(define (range-from n) (cseq-lazy n (lambda () (range-from (+ n 1)))))
;; An empty range is () (jolt-empty-list), NOT nil — (range 0) and (range 5 5) are
;; empty seqs in Clojure, so (= () (range 0)) holds. The same () terminates the
;; lazy tail of a non-empty range (jolt-empty-list seqs back to nil, see jolt-take).
(define (range-bounded n end step)
  (if (if (> step 0.0) (< n end) (> n end))
      (cseq-lazy n (lambda () (range-bounded (+ n step) end step)))
      jolt-empty-list))
;; numeric tower: exact 0/1 defaults so (range 3) yields exact ints
;; (= JVM longs); flonum args still produce flonums (Scheme arithmetic preserves).
(define jolt-range
  (case-lambda
    (() (range-from 0))
    ((end) (range-bounded 0 end 1))
    ((start end) (range-bounded start end 1))
    ((start end step) (range-bounded start end step))))

;; An empty take result is () (jolt-empty-list), NOT nil — (take 0 coll) and
;; (take n []) are empty seqs in Clojure, so (= () (take 0 [:a])) and printing
;; "()" hold. jolt-empty-list seqs back to nil, so it also terminates the lazy
;; tail when n hits 0 mid-stream (see map-seq).
;; The LAST element (n=1) terminates without touching the rest, so (take n s)
;; realizes exactly n elements of a side-effecting seq — matching Clojure, where
;; (take 0 (rest s)) never seqs coll. Realizing one more, as forcing seq-more at
;; the boundary would, over-runs the source by one (medley's sequence-padded).
(define (jolt-take n coll)
  ;; (take Double/POSITIVE_INFINITY coll) takes the whole coll on the JVM (the
  ;; count never reaches 0); test.check's rose-tree unchunk relies on it. Coercing
  ;; +inf.0 to a fixnum index would throw, so take all up front.
  (if (and (flonum? n) (infinite? n))
      (if (> n 0.0) (jolt-seq coll) jolt-empty-list)
      (let ((n (->idx n)))
        (let loop ((n n) (s (jolt-seq coll)))
          (cond
            ((or (fx<=? n 0) (jolt-nil? s)) jolt-empty-list)
            ((fx=? n 1) (cseq-lazy (seq-first s) (lambda () jolt-empty-list)))
            (else (cseq-lazy (seq-first s) (lambda () (loop (fx- n 1) (jolt-seq (seq-more s)))))))))))
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

;; Lazily concatenate a (possibly infinite) SEQ of colls — what (apply concat ss)
;; means, but without realizing ss. Pulls one coll at a time, concatenating it with
;; a lazy tail, so mapcat / (apply concat …) over an infinite source stays lazy.
(define (lazy-concat-seq ss)
  (let ((s (jolt-seq ss)))
    (if (jolt-nil? s)
        jolt-empty-list
        (jolt-concat (seq-first s)
                     (jolt-make-lazy-seq (lambda () (lazy-concat-seq (seq-more s))))))))

;; (apply f a b ... coll): spread the trailing seqable into the call. concat is
;; special-cased: it produces a LAZY result, so spreading an infinite tail through
;; a Scheme variadic (which must realize it) would hang — route to lazy-concat-seq,
;; prepending any fixed leading colls.
(define (jolt-apply f . args)
  (let* ((r (reverse args)) (tail (car r)) (fixed (reverse (cdr r))))
    (if (eq? f jolt-concat)
        (lazy-concat-seq (fold-right jolt-cons (jolt-seq tail) fixed))
        (apply jolt-invoke f (append fixed (seq->list (jolt-seq tail)))))))

;; ============================================================================
;; numeric predicates / identity — usable in fn AND value position (map/filter).
;; Return Scheme #t/#f (= jolt true/false). All-flonum model: coerce to an exact
;; integer for the parity tests.
;; ============================================================================
;; Parity over the full integer range (JVM even?/odd? accept any integer,
;; bignums included); a fixnum-only fxand crashes on a large value (e.g. a hash).
(define (parity-int n) (if (flonum? n) (exact (floor n)) n))
(define (jolt-even? n) (even? (parity-int n)))
(define (jolt-odd? n) (odd? (parity-int n)))
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
