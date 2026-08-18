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
;; kind : WHICH flavor of seq this cell is (one of the sk-* constants below) —
;; the only thing that distinguishes a list from a vector's seq from a map's seq
;; on this host, since one record type backs them all. See the sk-* block.
;; cvec/ci: for a vector-backed seq cell, the backing vector and this cell's
;; element index — so it is a real chunked-seq (chunked-seq? true, chunk-first
;; hands out a 32-element block, chunk-rest is the seq at the next block) and
;; reduce iterates the vector directly with no per-element cells.
;; cvec is #f for every other seq; stored as two fields (not a cons) so a vector
;; seq cell costs no extra allocation. The rest of the seq layer ignores them, so
;; first/rest/count/printing are unchanged.
;; crest: the ChunkedCons case — cvec holds a STANDALONE chunk pvec (<=32 already-
;; realized elements), ci the offset within it, and crest the seq AFTER the whole
;; chunk (the clojure.lang.ChunkedCons _more). This is what map/filter/range emit
;; so their result is itself a chunked-seq (chained chunked transforms each batch
;; by 32, like the JVM). crest is #f for a plain vector-backed seq (whose "rest"
;; is the next 32-block of the SAME cvec) and for every non-chunked cell.
;; ---- the seq FLAVOR tag (sk = seq kind) ---------------------------------------
;; jolt backs Clojure's list, cons, lazy seq, chunked seq AND every collection's
;; seq view with this ONE record, so nothing on the cell said which of them it
;; was, and the class arms had no choice but to answer PersistentList for all of
;; them: (seq [1 2]) reported clojure.lang.PersistentList where the JVM says
;; PersistentVector$ChunkedSeq, and so did a map's seq, a set's, an array's, a
;; string's, a range and an rseq. `kind` is that missing bit.
;;
;; It costs nothing. The tag is a FIXNUM in the field the boolean list? marker
;; used to occupy, so a cell is exactly the size it was — no extra field, no
;; extra allocation on the hottest record in the runtime — and list? became one
;; fixnum compare (cseq-list? below) in place of a boolean field read.
;;
;; This tier stays JVM-free on purpose: the flavor is jolt's own taxonomy, and
;; the kind -> clojure.lang.* name mapping lives in java/host-class.ss next to
;; (class …), where every other JVM name mapping already lives. Adding a flavor
;; is one constant here, one row in that table, and one line in
;; java/class-hierarchy.ss — nothing else in the seq tier looks at kind.
;;
;; The flavor PROPAGATES along the tail, because that is what the JVM does: each
;; of its seq classes returns its own type from next(), so (next (seq "abc")) is
;; a StringSeq and (next (rseq v)) an RSeq. Every builder below therefore passes
;; its own kind down when it builds the following cell.
;;
;; sk-cons is the default, and deliberately so — a generic realized-or-lazy cell
;; with no more specific flavor is exactly clojure.lang.Cons on the JVM, which is
;; what (next (take-while odd? [1 3 5])) and (next (partition 1 [1 2 3])) report
;; there.
;; The three CHUNKED flavors are kept adjacent (2..4) so sk-chunked? is a range
;; test rather than three separate compares — it sits on the map/filter batching
;; path, which asks it once per chunk.
(define sk-cons          0)   ; generic cell: clojure.lang.Cons
(define sk-list          1)   ; a PersistentList node (list literal / (list …) / reverse)
(define sk-chunked-seq   2)   ; a vector's own seq (cvec = whole pvec, crest #f)
(define sk-chunked-cons  3)   ; a standalone chunk + an after-chunk rest (crest set)
(define sk-long-range    4)   ; a bounded (range …)
(define sk-string-seq    5)   ; the characters of a string
(define sk-rseq          6)   ; a vector's reverse view (rseq)
(define sk-arraymap-seq  7)   ; an array-mode map's entry seq
(define sk-hashmap-seq   8)   ; a hash-mode map's entry seq
(define sk-treemap-seq   9)   ; a sorted map's / sorted set's entry seq
(define sk-key-seq      10)   ; keys of a map, and a set's own seq (RT.keys)
(define sk-val-seq      11)   ; vals of a map
(define sk-iterate      12)   ; (iterate f x) and the unbounded (range)
;; An array's seq, one flavor per element type — the JVM has a distinct ArraySeq
;; subclass per primitive (ArraySeq$ArraySeq_int …) and the plain ArraySeq for an
;; object array. natives-array.ss maps a jolt-array's `kind` onto these.
(define sk-array-seq    13)   ; Object[]
(define sk-array-int    14)
(define sk-array-long   15)
(define sk-array-short  16)
(define sk-array-double 17)
(define sk-array-float  18)
(define sk-array-bool   19)
(define sk-array-byte   20)
(define sk-array-char   21)
;; How many there are — the java layer sizes its kind-indexed tables from this,
;; so adding a flavor above needs no second edit to keep them in step.
(define sk-count        22)

(define-record-type cseq (fields head (mutable tail) (mutable forced?) kind cvec ci crest (mutable lock)) (nongenerative chez-cseq-v6))
;; tail already a seq. The /k variants take the flavor; the bare ones are the
;; generic cell, which is the overwhelming majority of call sites.
(define (cseq-realized head tail) (make-cseq head tail #t sk-cons #f 0 #f #f))
(define (cseq-realized/k head tail kind) (make-cseq head tail #t kind #f 0 #f #f))
(define (cseq-lazy head tail-thunk) (make-cseq head tail-thunk #f sk-cons #f 0 #f #f))
(define (cseq-lazy/k head tail-thunk kind) (make-cseq head tail-thunk #f kind #f 0 #f #f))
(define (cseq-list head tail) (make-cseq head tail #t sk-list #f 0 #f #f))   ; a PersistentList node
;; clojure.core/list? — Clojure's (instance? IPersistentList x), which among the
;; seq flavors only PersistentList is.
(define (cseq-list? s) (fx=? (cseq-kind s) sk-list))
;; Which flavors are chunked, i.e. hand out a whole block through chunk-first —
;; stated ONCE, here, because this tier is what drives chunk-first/chunk-rest.
;; host-class.ss grafts clojure.lang.IChunkedSeq onto exactly these flavors'
;; classes, so (chunked-seq? x) and (instance? IChunkedSeq x) are the same question
;; asked twice and cannot drift apart — which is what Clojure means by
;; (defn chunked-seq? [s] (instance? IChunkedSeq s)).
;;
;; Being chunked is a property of the FLAVOR, not of carrying cvec: a sorted coll's
;; seq is vector-backed here because the :seq op materializes the tree, but it is a
;; PersistentTreeMap$Seq and is no more a chunked seq than the JVM's is.
;; The chunked flavors are the contiguous run sk-chunked-seq..sk-long-range, so
;; this is two fixnum compares.
(define (sk-chunked? k)
  (and (fx>=? k sk-chunked-seq) (fx<=? k sk-long-range)))
;; vector-backed: like a ChunkedCons, its tail follows from (v, i+1), so it
;; carries no thunk either — see cseq-cvec-more.
;; The flavor is passed in rather than assumed from cvec: cvec/ci is a
;; REPRESENTATION (vector-backed, ascending from ci) and more than one flavor uses
;; it — a vector's own seq is a ChunkedSeq, while a sorted map materialized into a
;; pvec is a PersistentTreeMap$Seq that happens to be vector-backed. Keeping the
;; two independent is what lets the class answer stay right while the O(1)
;; count/chunk fast paths keep keying off cvec alone.
(define (cseq-vec head v i kind) (make-cseq head #f #f kind v i #f #f))
;; A ChunkedCons cell over a standalone chunk pvec: head is chunk[i], walking
;; (seq-more) advances within the chunk and then continues into `rest`. `rest` is
;; the already-coerced after-chunk seq (cseq | jolt-nil | a jolt-lazyseq), held in
;; crest for chunk-rest/chunk-next and forced lazily by the tail thunk at the chunk
;; boundary so a chunked map over an infinite chunked source stays productive.
;; NO TAIL THUNK. A cell that carries cvec knows its own tail from its own
;; fields — the next index in the same chunk, or the after-chunk rest at the
;; boundary — and every element of that chunk was already realized when the
;; chunk was built. So the tail is a pure function of the cell, computed on
;; demand by cseq-cvec-more below, and the lazy-seq machinery is skipped
;; entirely: nothing to allocate for the tail, nothing to memoize, nothing for a
;; concurrent reader to observe half-published.
;;
;; This is the granularity Clojure uses. Its chunked map wraps one Object[32] in
;; a single ChunkedCons under a single LazySeq node, and stepping WITHIN the
;; chunk is ChunkedCons.next() -> new ChunkedCons(chunk.dropFirst(), _more), a
;; pure computation. jolt used to allocate a closure per element and then have
;; seq-more publish it back through two mutable-field writes — i.e. it paid
;; lazy-seq memoization per ELEMENT where Clojure pays it per CHUNK, which is
;; why chunking bought nothing here (a chunkable source cost the same 86 ns/elem
;; as an unchunkable one, where the JVM gets 2.9x out of it).
;; A ChunkedCons by default; `kind` lets a producer that chunks say what it really
;; is instead (a bounded range chunks, and is a LongRange).
(define (cseq-chunked chunk i rest)
  (make-cseq (pvec-nth-d chunk i jolt-nil) #f #f sk-chunked-cons chunk i rest #f))
(define (cseq-chunked/k chunk i rest kind)
  (make-cseq (pvec-nth-d chunk i jolt-nil) #f #f kind chunk i rest #f))
;; The tail of a cvec-bearing cell. Two shapes share the field: a ChunkedCons
;; (crest set — cvec is a standalone <=32 chunk, and the after-chunk seq follows)
;; and a vector-backed index seq (crest #f — cvec is the whole backing vector).
;; FORCE? distinguishes Clojure's next() from its more(): at a chunk boundary
;; next() runs chunkedMore().seq() and realizes what follows, while more()
;; returns _more untouched. Getting that wrong makes (rest s) realize the next
;; chunk, which is a laziness regression a value test would never catch.
;; The vector-backed branch builds its next cell INLINE rather than calling
;; vec->seq/k. This is the tail of every (seq a-vector) walk — the hottest step in
;; the file — and the call it replaces was there before the flavor existed, so
;; folding it away pays for the flavor field this function now has to carry.
(define (cseq-cvec-more s force?)
  (let ((v (cseq-cvec s)) (i1 (fx+ (cseq-ci s) 1)) (cr (cseq-crest s)))
    (if cr
        (if (fx<? i1 (pvec-count v))
            (cseq-chunked/k v i1 cr (cseq-kind s))
            (if force? (jolt-seq cr) cr))
        (if (fx>=? i1 (pvec-count v))
            jolt-nil
            (make-cseq (pvec-nth-d v i1 jolt-nil) #f #f (cseq-kind s) v i1 #f #f)))))
(define (seq-first s) (cseq-head s))
;; guards lazy creation of a cell's tail mutex on the multi-threaded path (mirrors
;; force-lazyseq's lock-init). A cseq cell is shared across threads once its owning
;; lazyseq node is realized (every future/agent walking the same seq reads the SAME
;; cell), so without serialization two threads can both see forced?#f, both run the
;; tail thunk, and publish tail/forced? non-atomically — a third reader can then see
;; forced?#t with tail still the thunk-procedure, leaking a closure out as a seq.
;; Like force-lazyseq this stays lock-free until jolt-mt? flips (fork-thread shadow).
(define cseq-lock-init (make-mutex))
(define (cseq-ensure-lock! s)
  (jolt-with-mutex cseq-lock-init
    (or (cseq-lock s)
        (let ((m (make-mutex))) (cseq-lock-set! s m) m))))
(define (seq-more s)                  ; force the tail; returns a seq (cseq | jolt-nil)
  ;; Single-threaded: the fast forced? read is safe. Multi-threaded: it is NOT — the
  ;; tail/forced? stores below are unordered, so a lock-free reader on ARM64 can see
  ;; forced?#t with tail still the thunk-procedure. So once jolt-mt? flips, every
  ;; access goes through the per-cell mutex, reads included (mirrors force-lazyseq).
  ;; A cvec cell has no thunk: its tail follows from its own fields, so it is
  ;; COMPUTED here rather than run. It is still memoized, which is where this
  ;; beats the reference implementation — Clojure's ChunkedCons.next() rebuilds a
  ;; ChunkedCons + an ArrayChunk on every traversal, so re-walking a retained seq
  ;; costs it the same as the first walk. Dropping the closure without dropping
  ;; the memo gets the first walk's saving AND keeps re-walks cheap: measured
  ;; over 100k, first walk 80 -> 58 ns/elem, re-walk unchanged at ~23 (going
  ;; memo-free the way Clojure does took re-walks to ~42).
  (cond
    ((not jolt-mt?)
     (if (cseq-forced? s) (cseq-tail s)
         (let ((t (if (cseq-cvec s) (cseq-cvec-more s #t) ((cseq-tail s)))))
           (cseq-tail-set! s t) (cseq-forced?-set! s #t) t)))
    (else (jolt-with-mutex (cseq-ensure-lock! s)     ; multi-threaded: always lock
            (if (cseq-forced? s) (cseq-tail s)
                (let ((t (if (cseq-cvec s) (cseq-cvec-more s #t) ((cseq-tail s)))))
                  (cseq-tail-set! s t) (cseq-forced?-set! s #t) t))))))

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
;; …with every cell of the chain carrying one flavor. A collection's seq view is
;; the same class all the way down on the JVM ((next (keys m)) is another KeySeq),
;; so the kind goes on the whole chain and not just its head.
(define (list->cseq/k xs kind)
  (if (null? xs) jolt-nil (cseq-realized/k (car xs) (list->cseq/k (cdr xs) kind) kind)))

;; ---- variadic rest: passing a LAZY tail through a Chez rest parameter --------
;; A Chez rest parameter must be a proper list, so `apply` would have to realize
;; its trailing seqable to call a variadic fn — and an infinite tail then never
;; terminates. The JVM does not realize: RestFn.applyTo hands the seq straight to
;; the variadic arity. jolt gets the same behaviour by letting jolt-apply pass a
;; ONE-element rest list holding a lazy-rest box, which the arity's rest binding
;; (jolt-rest-seq, emitted by the back end) unwraps back into the seq.
;;
;; Only a jolt-EMITTED variadic arity binds its rest through jolt-rest-seq, so
;; only those may be handed a box: the ~100 hand-written Scheme variadics in the
;; runtime read their rest list directly and would see the box as a value. The
;; back end registers each emitted variadic closure here as it is created, which
;; is what makes the box safe — and gives a runtime variadic that IS lazy in its
;; rest (jolt-concat) one general way to opt in instead of a special case in
;; jolt-apply.
;;
;; The registered value is V, the variadic arity's fixed-parameter count. It is
;; recorded rather than derived from procedure-arity-mask because the mask cannot
;; recover it: (fn ([a] …) ([a b & r] …)) accepts {1,2,3,…}, from which V reads as
;; 1, while the variadic arity's real fixed count is 2 — and calling it with one
;; real argument plus a box would bind b to the box and r to nil.
(define-record-type lazy-rest (fields seq) (nongenerative jolt-lazy-rest-v1))
;; Written on every variadic closure creation and read by jolt-apply, from
;; whatever threads the program runs — and a Chez hashtable is not thread-safe.
;; An unsynchronized write racing a read corrupts the table's internals, and the
;; damage shows up later as a SIGSEGV inside the collector or as a hang, never as
;; an error naming this table. Both sides take the mutex; on the read side that is
;; the apply path, where a mutex is noise next to the list building around it.
(define variadic-fixed-arity-tbl (make-weak-eq-hashtable))
(define variadic-tbl-mu (make-mutex))
(define (jolt-register-variadic! v proc)
  (jolt-with-mutex variadic-tbl-mu (hashtable-set! variadic-fixed-arity-tbl proc v))
  proc)
(define (variadic-fixed-arity-of proc)
  (jolt-with-mutex variadic-tbl-mu (hashtable-ref variadic-fixed-arity-tbl proc #f)))
;; The rest binding emitted for every variadic arity. A boxed rest is the seq
;; itself; anything else is the ordinary Chez rest list.
(define (jolt-rest-seq xs)
  (if (and (pair? xs) (null? (cdr xs)) (lazy-rest? (car xs)))
      (lazy-rest-seq (car xs))
      (list->cseq xs)))
(define (vec->seq v i)                 ; chunked index seq over a persistent vector
  (vec->seq/k v i sk-chunked-seq))
;; …as some other flavor that is nonetheless vector-backed (a sorted coll's seq,
;; materialized once into a pvec, still walks it with the O(1) count and the
;; chunk-at-a-time fast paths — it just isn't a ChunkedSeq).
(define (vec->seq/k v i kind)
  (if (fx>=? i (pvec-count v)) jolt-nil
      (cseq-vec (pvec-nth-d v i jolt-nil) v i kind)))
;; A string's characters. clojure.lang.StringSeq, and the tail is one too.
(define (str->seq s i)
  (if (fx>=? i (string-length s)) jolt-nil
      (cseq-lazy/k (string-ref s i) (lambda () (str->seq s (fx+ i 1))) sk-string-seq)))
;; ---- seq arms: host types register here instead of set!-wrapping jolt-seq ----
;; Arms dispatch newest-registration-first (cons front, walk head-first), matching
;; the precedence the set! chains produced. The built-in types stay inline in
;; jolt-seq itself.
;; Records are NOT on this fast path — jolt-seq walks the arms for them — so the
;; probe list deliberately omits jrec even though jolt-get's includes it. See
;; reject-fast-type-claim! (values.ss).
(define (seq-fast-probes)
  (list jolt-nil jolt-empty-list (probe-cseq) (probe-pvec) (probe-pmap) (probe-pset) "s"))
(define (seq-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who pred (seq-fast-probes) "the jolt-seq fast path"))
(define jolt-seq-arms '())
(define (register-seq-arm! pred handler)
  (seq-arm-reject-fast-type! 'register-seq-arm! pred)
  (set! jolt-seq-arms (cons (cons pred handler) jolt-seq-arms)))

(define (jolt-seq x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((empty-list-t? x) jolt-nil)
    ((cseq? x) x)
    ((pvec? x) (vec->seq x 0))
    ;; array mode and hash mode are different classes on the JVM, the same split
    ;; (class …) already reports for the map itself.
    ((pmap? x) (list->cseq/k (pmap-fold x (lambda (k v a) (cons (make-map-entry k v) a)) '())
                             (if (pmap-order x) sk-arraymap-seq sk-hashmap-seq)))
    ;; a set's seq is RT.keys over its backing map, i.e. an APersistentMap$KeySeq
    ((pset? x) (list->cseq/k (pset-fold x cons '()) sk-key-seq))
    ((string? x) (str->seq x 0))
    (else (let loop ((as jolt-seq-arms))
            (cond ((null? as) (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                          (string-append "Don't know how to create ISeq from: "
                                                         (guard (e (#t "?")) (jolt-class-name x))))))
                  (((caar as) x) ((cdar as) x))
                  (else (loop (cdr as))))))))

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
      ;; A cvec cell's tail is already realized, so rest hands it back directly
      ;; rather than wrapping a lazy-seq around a value that is simply present.
      ;; force? #f is the whole point: at a chunk boundary this returns the
      ;; after-chunk seq WITHOUT seq-ing it, which is Clojure's more() (next()
      ;; forces, more() does not) — forcing here would make (rest s) realize the
      ;; following chunk.
      ((cseq-cvec s) (let ((m (cseq-cvec-more s #f)))
                       (if (jolt-nil? m) jolt-empty-list m)))
      ((cseq-forced? s) (let ((m (cseq-tail s))) (if (jolt-nil? m) jolt-empty-list m)))
      ;; A string seq's tail is a pure O(1) step to the next index of the same
      ;; string, so force it instead of wrapping a lazy-seq around it: (rest "abc")
      ;; is a StringSeq on the JVM, not a LazySeq, and there is no laziness to
      ;; protect here — no side effect and no unbounded work can hide behind a
      ;; string-ref. Below the forced? arm so the common path pays nothing.
      ((fx=? (cseq-kind s) sk-string-seq)
       (let ((m (seq-more s))) (if (jolt-nil? m) jolt-empty-list m)))
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
;; EVERY cell of a list is a list cell, not just its head. PersistentList._rest is
;; itself a PersistentList on the JVM, so (rest '(1 2)) is a PersistentList and
;; (list? (rest '(1 2))) is true there; marking only the head made both answers
;; wrong, and made the tail of a list uncounted when the JVM counts it in O(1).
;;
;; cons has RT.cons's two outcomes: onto nil a PersistentList, onto anything else a
;; Cons. A Cons is an ASeq but NOT an IPersistentList, so it is not list?, not
;; counted?, and not a stack — (peek (cons 1 '(2))) is a ClassCastException there,
;; which falls out of this for free because peek/pop already require a list cell.
;;
;; The split is on the ARGUMENT being nil, NOT on its seq being empty: RT.cons tests
;; (coll == null), so (cons 1 []) is a Cons whose tail is nil while (cons 1 nil) is a
;; one-element PersistentList. Testing (jolt-seq coll) for emptiness instead would
;; quietly turn every cons-onto-empty into a list.
;;
;; NOTE what this means for the rest of the runtime: a cons-built form is no longer
;; list?, and ~20 places that recognize a form — the eval path, the build driver's
;; ns/require scanners, the image emitter — used to spell that recognition
;; (and (cseq? f) (cseq-list? f)). The reference compiler tests ISeq, not
;; list-ness, because a macro that assembles its expansion with cons produces a form
;; whose head symbol is identical and only whose provenance differs; requiring a list
;; would have made every such expansion invisible. natives-reader.ss already hit that
;; exact bug with macroexpand-1. Those sites now test cseq? alone, which is both the
;; ISeq question and precisely what they answered for a cons before this change.
;; Both cells are built straight through make-cseq rather than via cseq-list /
;; cseq-realized, since cons is a primitive hot enough to notice. That does not buy
;; the whole cost back: knowing WHICH cell to build needs a nil test the single-shape
;; version did not, and a cons measures 25.8 -> 27.4 ns over 200k against the branch
;; before this one (folding the constructor call away recovered 0.4 of the 2.0).
;; The remainder is the test itself and is the price of the distinction.
(define (jolt-cons x coll)
  (if (jolt-nil? coll)
      (make-cseq x jolt-nil #t sk-list #f 0 #f #f)
      (make-cseq x (jolt-seq coll) #t sk-cons #f 0 #f #f)))
;; Scheme list -> a jolt PersistentList. For (list …) and quoted list literals
;; (the emitter lowers '(a b) to (jolt-list a b)).
(define (jolt-list . xs)
  (if (null? xs) jolt-empty-list (cseq-list (car xs) (list->cseq/k (cdr xs) sk-list))))
;; reverse yields a list (seed: (list? (reverse coll)) is always true). The chain
;; is built list-flavored as it goes, so the head needs no second cell to re-mark
;; it — one allocation less per reverse than re-wrapping did.
(define (jolt-reverse coll)
  (let loop ((s (jolt-seq coll)) (acc jolt-empty-list))
    (if (jolt-nil? s)
        acc
        (loop (jolt-seq (seq-more s))
              (cseq-realized/k (seq-first s) (if (empty-list-t? acc) jolt-nil acc) sk-list)))))
(define (jolt-last coll) (let loop ((s (jolt-seq coll)) (last jolt-nil))
                           (if (jolt-nil? s) last (loop (jolt-seq (seq-more s)) (seq-first s)))))
;; nth over a seq (walks; forces lazily). default? selects the 3-arg behavior.
(define (seq-nth coll i default? d)
  (if (fx<? i 0) (if default? d (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "index out of bounds")))
      (let loop ((s (jolt-seq coll)) (i i))
        (cond ((jolt-nil? s) (if default? d (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "index out of bounds"))))
              ((fx=? i 0) (seq-first s))
              (else (loop (jolt-seq (seq-more s)) (fx- i 1)))))))

;; --- checked arithmetic: JVM Numbers.ops-style category dispatch -------------
;; Every arithmetic/comparison site (the inlined jolt-n* macros in call position,
;; the variadic shims in value position) funnels a binary op through ONE dispatch:
;; both operands inside Chez's tower take the native op with JVM contagion rules
;; patched in (a double operand wins — Chez's exact-zero shortcut must not leak:
;; (* 1.5 0) is 0.0, not 0; an exact zero divisor throws ArithmeticException, a
;; double zero divisor yields ##Inf/##NaN); an operand OUTSIDE the tower (e.g.
;; BigDecimal) falls to a slow hook the numeric shim extends (java/bigdec.ss).
;; A non-numeric operand is a ClassCastException, like the JVM.
(define (jolt-num-cast-throw x)
  (if (jolt-nil? x)
      (jolt-throw (jolt-host-throwable "java.lang.NullPointerException" ""))
      (jolt-throw (jolt-host-throwable
                   "java.lang.ClassCastException"
                   (string-append "class " (jolt-class-name x)
                                  " cannot be cast to class java.lang.Number")))))
(define (jolt-div0-throw)
  (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))

;; Checked coercions for host operations that would otherwise hand a wrong-typed
;; value straight to a Chez primitive. The condition Chez raises carries no class,
;; so it escapes as #object[:object] and no catch clause can select it; these name
;; the class the JVM names instead. nil is the JVM's NullPointerException.
(define (jolt-cast-throw x target)
  (if (jolt-nil? x)
      (jolt-throw (jolt-host-throwable "java.lang.NullPointerException" ""))
      (jolt-throw (jolt-host-throwable
                   "java.lang.ClassCastException"
                   (string-append "class " (jolt-class-name x)
                                  " cannot be cast to class " target)))))
(define (jolt-need-num x) (if (number? x) x (jolt-num-cast-throw x)))
(define (jolt-need-str x) (if (string? x) x (jolt-cast-throw x "java.lang.CharSequence")))
(define (jolt-need-string x) (if (string? x) x (jolt-cast-throw x "java.lang.String")))

;; slow hooks: one per op, taking over when an operand is outside Chez's tower.
;; A numeric shim (java/bigdec.ss) set!-extends them; the base case is the JVM's:
;; not a number -> ClassCastException. The hooks are BINARY and never re-enter
;; the variadic shims, so extension order can't recurse.
(define (jolt-add-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
(define (jolt-sub-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
(define (jolt-mul-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
(define (jolt-div-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
;; comparison of operands outside the Chez tower: numeric shims extend this to a
;; 3-way compare; anything left over is not a number.
(define (jolt-num-cmp-slow a b)
  (jolt-num-cast-throw (if (number? a) b a)))

(define (jolt-add2 a b)
  (if (and (number? a) (number? b)) (+ a b) (jolt-add-slow a b)))
(define (jolt-sub2 a b)
  (if (and (number? a) (number? b)) (- a b) (jolt-sub-slow a b)))
(define (jolt-mul2 a b)
  (if (and (number? a) (number? b))
      (if (or (flonum? a) (flonum? b))
          (fl* (real->flonum a) (real->flonum b))
          (* a b))
      (jolt-mul-slow a b)))
(define (jolt-div2 a b)
  (if (and (number? a) (number? b))
      (if (or (flonum? a) (flonum? b))
          (fl/ (real->flonum a) (real->flonum b))
          (if (eqv? b 0) (jolt-div0-throw) (/ a b)))
      (jolt-div-slow a b)))
(define (jolt-lt2 a b)
  (if (and (number? a) (number? b)) (< a b) (< (jolt-num-cmp-slow a b) 0)))
(define (jolt-gt2 a b)
  (if (and (number? a) (number? b)) (> a b) (> (jolt-num-cmp-slow a b) 0)))
(define (jolt-le2 a b)
  (if (and (number? a) (number? b)) (<= a b) (<= (jolt-num-cmp-slow a b) 0)))
(define (jolt-ge2 a b)
  (if (and (number? a) (number? b)) (>= a b) (>= (jolt-num-cmp-slow a b) 0)))
;; min/max return the ORIGINAL operand (type and exactness kept, like
;; Numbers.min): (min 1 2.0) is 1, not 1.0. A NaN operand wins.
(define (jolt-min2 a b)
  (cond ((and (flonum? a) (nan? a)) a)
        ((and (flonum? b) (nan? b)) b)
        (else (if (jolt-lt2 a b) a b))))
(define (jolt-max2 a b)
  (cond ((and (flonum? a) (nan? a)) a)
        ((and (flonum? b) (nan? b)) b)
        (else (if (jolt-gt2 a b) a b))))

;; quot/rem/mod over the full tower: truncating division; a double operand makes
;; the result a double; mod has floor semantics (result takes the divisor's
;; sign). A zero divisor throws ArithmeticException in both worlds (JVM double
;; quot/rem check the divisor before dividing). Non-tower operands hit the
;; set!-extensible slow hooks.
(define (jolt-quot-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
(define (jolt-rem-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
(define (jolt-mod-slow a b) (jolt-num-cast-throw (if (number? a) b a)))
(define (jolt-quot a b)
  (cond ((not (and (number? a) (number? b))) (jolt-quot-slow a b))
        ((or (flonum? a) (flonum? b))
         (let ((n (real->flonum a)) (d (real->flonum b)))
           (if (fl= d 0.0) (jolt-div0-throw)
               (let ((q (fl/ n d)))
                 (when (or (nan? q) (infinite? q))
                   (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                                                    "Infinite or NaN")))
                 (fltruncate q)))))
        ((eqv? b 0) (jolt-div0-throw))
        ((and (integer? a) (integer? b)) (quotient a b))
        (else (truncate (/ a b)))))
(define (jolt-rem a b)
  (cond ((not (and (number? a) (number? b))) (jolt-rem-slow a b))
        ((or (flonum? a) (flonum? b))
         (let ((n (real->flonum a)) (d (real->flonum b)))
           (if (fl= d 0.0) (jolt-div0-throw)
               (let ((q (fl/ n d)))
                 (when (or (nan? q) (infinite? q))
                   (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                                                    "Infinite or NaN")))
                 (fl- n (fl* d (fltruncate q)))))))
        ((eqv? b 0) (jolt-div0-throw))
        ((and (integer? a) (integer? b)) (remainder a b))
        (else (- a (* b (truncate (/ a b)))))))
(define (jolt-mod a b)
  (cond ((not (and (number? a) (number? b))) (jolt-mod-slow a b))
        ((and (integer? a) (integer? b) (not (flonum? a)) (not (flonum? b)))
         (if (eqv? b 0) (jolt-div0-throw) (modulo a b)))
        (else
         (let ((m (jolt-rem a b)))
           (if (or (zero? m) (eq? (negative? m) (negative? b))) m (jolt-add2 m b))))))

;; value-position arithmetic (the higher-order forms: (reduce + []), (apply * xs)).
;; Folded through the binary dispatch so contagion/edge rules hold; identities
;; (+)=0 / (*)=1 are exact, matching exact integer arithmetic. The hot path uses
;; the inlined native ops, not these.
;; recognizer for slow-path numeric types; numeric shims extend it.
(define (jolt-num-slow? x) #f)

;; The one sanctioned way for a numeric shim outside the core (java/bigdec.ss)
;; to extend these hooks — the core owns the hook storage and the op-name
;; contract, the shim hands over (lambda (prev) handler) and never touches a
;; core var directly. Same convention as register-eq-arm!/register-compare-arm!.
;; `handler` must decline operands it can't handle by calling prev.
;;
;; OP NAMING RULE: an op is the runtime var it extends with the `jolt-` prefix
;; stripped, no abbreviating and no dropping the `-slow` suffix — 'add-slow is
;; jolt-add-slow, 'num-cmp-slow is jolt-num-cmp-slow. Nothing here has to be
;; memorized: read off the var name. Registrations are top level, so a name that
;; doesn't follow the rule hits the else below when the shim loads, not later.
;; The vars live in three files; the comments mark where each group is defined.
(define (register-num-arm! op extend)
  (case op
    ;; seq.ss (this file)
    ((add-slow)      (set! jolt-add-slow      (extend jolt-add-slow)))
    ((sub-slow)      (set! jolt-sub-slow      (extend jolt-sub-slow)))
    ((mul-slow)      (set! jolt-mul-slow      (extend jolt-mul-slow)))
    ((div-slow)      (set! jolt-div-slow      (extend jolt-div-slow)))
    ((quot-slow)     (set! jolt-quot-slow     (extend jolt-quot-slow)))
    ((rem-slow)      (set! jolt-rem-slow      (extend jolt-rem-slow)))
    ((mod-slow)      (set! jolt-mod-slow      (extend jolt-mod-slow)))
    ((num-cmp-slow)  (set! jolt-num-cmp-slow  (extend jolt-num-cmp-slow)))
    ((num-slow?)     (set! jolt-num-slow?     (extend jolt-num-slow?)))
    ((zero?)         (set! jolt-zero?         (extend jolt-zero?)))
    ((pos?)          (set! jolt-pos?          (extend jolt-pos?)))
    ((neg?)          (set! jolt-neg?          (extend jolt-neg?)))
    ;; rt.ss
    ((inc)           (set! jolt-inc           (extend jolt-inc)))
    ((dec)           (set! jolt-dec           (extend jolt-dec)))
    ;; predicates.ss
    ((number?)       (set! jolt-number?       (extend jolt-number?)))
    ((decimal?)      (set! jolt-decimal?      (extend jolt-decimal?)))
    ((num-equiv-slow) (set! jolt-num-equiv-slow (extend jolt-num-equiv-slow)))
    ;; converters.ss
    ((double-slow)   (set! jolt-double-slow   (extend jolt-double-slow)))
    ((cast-truncate-slow) (set! jolt-cast-truncate-slow (extend jolt-cast-truncate-slow)))
    (else (error 'register-num-arm!
                 "unknown numeric extension point (an op is its jolt- var minus the prefix)"
                 op))))
(define (jolt-num-check1 x)   ; (+ x)/(* x) return x but still type-check it
  (if (or (number? x) (jolt-num-slow? x)) x (jolt-num-cast-throw x)))
(define (jolt-add . xs)
  (cond ((null? xs) 0)
        ((null? (cdr xs)) (jolt-num-check1 (car xs)))
        (else (fold-left jolt-add2 (car xs) (cdr xs)))))
(define (jolt-arity0-throw name)
  (jolt-throw (jolt-host-throwable
               "clojure.lang.ArityException"
               (string-append "Wrong number of args (0) passed to: clojure.core/" name))))
(define (jolt-sub . xs)
  (cond ((null? xs) (jolt-arity0-throw "-"))
        ((null? (cdr xs)) (jolt-sub2 0 (car xs)))
        (else (fold-left jolt-sub2 (car xs) (cdr xs)))))
(define (jolt-mul . xs)
  (cond ((null? xs) 1)
        ((null? (cdr xs)) (jolt-num-check1 (car xs)))
        (else (fold-left jolt-mul2 (car xs) (cdr xs)))))
(define (jolt-div . xs)
  (cond ((null? xs) (jolt-arity0-throw "/"))
        ((null? (cdr xs)) (jolt-div2 1 (car xs)))
        (else (fold-left jolt-div2 (car xs) (cdr xs)))))
(define (jolt-min x . xs) (fold-left jolt-min2 x xs))
(define (jolt-max x . xs) (fold-left jolt-max2 x xs))
;; variadic comparison chains for value position ((apply < xs)).
(define (jolt-cmp-chain op2)
  (lambda (x . xs)
    (let loop ((a x) (rest xs))
      (cond ((null? rest) #t)
            ((op2 a (car rest)) (loop (car rest) (cdr rest)))
            (else #f)))))
(define jolt-lt (jolt-cmp-chain jolt-lt2))
(define jolt-gt (jolt-cmp-chain jolt-gt2))
(define jolt-le (jolt-cmp-chain jolt-le2))
(define jolt-ge (jolt-cmp-chain jolt-ge2))

;; call-position arithmetic: inlined macros with the both-Chez-numbers fast path
;; open-coded; anything else falls to the binary dispatch above. Comparisons
;; return a genuine Scheme boolean (the backend's truthy elision relies on it).
(define-syntax jolt-n+
  (syntax-rules ()
    ((_) 0)
    ((_ a) (jolt-add a))
    ((_ ea eb) (let ((a ea) (b eb))
                 (if (and (number? a) (number? b)) (+ a b) (jolt-add a b))))
    ((_ a b c ...) (jolt-n+ (jolt-n+ a b) c ...))))
(define-syntax jolt-n-
  (syntax-rules ()
    ((_) (jolt-sub))
    ((_ a) (jolt-sub a))
    ((_ ea eb) (let ((a ea) (b eb))
                 (if (and (number? a) (number? b)) (- a b) (jolt-sub a b))))
    ((_ a b c ...) (jolt-n- (jolt-n- a b) c ...))))
(define-syntax jolt-n*
  (syntax-rules ()
    ((_) 1)
    ((_ a) (jolt-mul a))
    ((_ ea eb) (let ((a ea) (b eb))
                 (if (and (number? a) (number? b))
                     (if (or (flonum? a) (flonum? b))
                         (fl* (real->flonum a) (real->flonum b))
                         (* a b))
                     (jolt-mul a b))))
    ((_ a b c ...) (jolt-n* (jolt-n* a b) c ...))))
(define-syntax jolt-n-div
  (syntax-rules ()
    ((_) (jolt-div))
    ((_ a) (jolt-div a))
    ((_ a b) (jolt-div2 a b))
    ((_ a b c ...) (jolt-n-div (jolt-div2 a b) c ...))))
(define-syntax define-n-cmp
  (syntax-rules ()
    ((_ name op op2)
     (define-syntax name
       (syntax-rules ()
         ((_) (op2))
         ((_ a) (begin a #t))
         ((_ ea eb) (let ((a ea) (b eb))
                      (if (and (number? a) (number? b)) (op a b) (op2 a b))))
         ((_ ea eb c (... ...)) (let ((a ea) (b eb))
                                  (and (name a b) (name b c (... ...))))))))))
(define-n-cmp jolt-n<  <  jolt-lt2)
(define-n-cmp jolt-n>  >  jolt-gt2)
(define-n-cmp jolt-n<= <= jolt-le2)
(define-n-cmp jolt-n>= >= jolt-ge2)
(define-syntax jolt-n-min
  (syntax-rules ()
    ((_) (jolt-min))
    ((_ a) (jolt-min a))
    ((_ a b) (jolt-min2 a b))
    ((_ a b c ...) (jolt-n-min (jolt-min2 a b) c ...))))
(define-syntax jolt-n-max
  (syntax-rules ()
    ((_) (jolt-max))
    ((_ a) (jolt-max a))
    ((_ a b) (jolt-max2 a b))
    ((_ a b c ...) (jolt-n-max (jolt-max2 a b) c ...))))
;; inc/dec in call position: open-coded number fast path (generic + promotes past
;; the fixnum boundary, keeps flonum/ratio contagion); anything else falls to the
;; jolt-inc/jolt-dec procedures, whose top-level cells carry the bigdec arms.
(define-syntax jolt-n-inc
  (syntax-rules ()
    ((_ ea) (let ((a ea)) (if (number? a) (+ a 1) (jolt-inc a))))))
(define-syntax jolt-n-dec
  (syntax-rules ()
    ((_ ea) (let ((a ea)) (if (number? a) (- a 1) (jolt-dec a))))))

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
(define unc-neg-2^63 (- unc-2^63))
;; Wrap to a signed 64-bit value. Fast path: an exact integer already in
;; [-2^63, 2^63) is its own wrap — skip the bignum mask, which on Chez (61-bit
;; fixnums) allocates for any value past 2^60. Only an out-of-range result (a
;; multiply overflowing into 128 bits) needs the mask + sign fixup.
(define (jolt-wrap64 x)
  (if (and (exact? x) (integer? x) (>= x unc-neg-2^63) (< x unc-2^63))
      x
      (let ((m (bitwise-and (if (and (number? x) (exact? x) (integer? x)) x (exact (floor x))) unc-mask64)))
        (if (>= m unc-2^63) (- m unc-2^64) m))))
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
;; arithmetic). Everything below keeps a ^long correct across its whole range, in
;; one of two ways depending on the op. Macros (define-syntax) so the fast path
;; inlines.
;;
;; Ops whose RESULT can leave the fixnum range even when the operands are inside it
;; (+ - * inc dec) compute generically and bound the result here, throwing
;; ArithmeticException past a true long exactly as the reference does. This is a
;; macro rather than a procedure because it wraps every one of those ops, where a
;; call would cost more than the arithmetic; inlined, the common case is a single
;; fixnum? test, since every fixnum is a long and needs no bounds check.
;; (Wrapping instead of throwing is what unchecked-math is for — see jolt-uncadd2.)
(define l-long-min -9223372036854775808)
(define l-long-max 9223372036854775807)
(define (jolt-l-overflow) (throw-jvm 'ArithmeticException "long overflow"))
(define-syntax jolt-l-checked
  (syntax-rules ()
    ((_ e) (let ((r e))
             (if (fixnum? r)
                 r
                 (if (and (>= r l-long-min) (<= r l-long-max))
                     r
                     (jolt-l-overflow)))))))

;; The rest — comparisons, quot/rem/mod, min/max — return a fixnum whenever both
;; operands are fixnums, so testing the operands settles it: fx fast path when they
;; are, generic op when they aren't.
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
(define-syntax jolt-l-inc (syntax-rules () ((_ a) (jolt-l-checked (+ a 1)))))
(define-syntax jolt-l-dec (syntax-rules () ((_ a) (jolt-l-checked (- a 1)))))

;; Chez dispatches fixnum-first inside +/-/*, so going generic here costs the
;; dispatch, not boxing.
(define-syntax jolt-l+ (syntax-rules () ((_ a b) (jolt-l-checked (+ a b)))))
(define-syntax jolt-l- (syntax-rules () ((_ a b) (jolt-l-checked (- a b)))
                                       ((_ a)   (jolt-l-checked (- a)))))
(define-syntax jolt-l* (syntax-rules () ((_ a b) (jolt-l-checked (* a b)))))

;; ============================================================================
;; IFn dispatch — the dynamic "value as fn" fallback. A callee that the emitter
;; can't statically resolve to a procedure (a keyword/coll/proc held in a local)
;; routes here. Off the arithmetic/self-recursion hot path by construction.
;; ============================================================================
;; (pred . handler) arms making a host type invocable; handler gets (f args).
(define jolt-invoke-arms '())
(define (register-invoke-arm! pred handler)
  (set! jolt-invoke-arms (cons (cons pred handler) jolt-invoke-arms)))
(define (jolt-invoke-arm-for f)
  (let loop ((as jolt-invoke-arms))
    (cond ((null? as) #f)
          (((caar as) f) (cdar as))
          (else (loop (cdr as))))))

;; prefix arms: predicates consulted BEFORE the built-in cond below, so a later-
;; loaded module folds its dispatch into this single jolt-invoke instead of
;; set!-wrapping it (which cost an extra lambda + rest-list + apply per call).
;; vars.ss registers var-cell? (invoke a var -> invoke its root) and
;; multimethods.ss registers jolt-multifn?. Those types are never raw procedures,
;; so checking them after the procedure? fast path is safe and keeps the hot path
;; (a raw procedure) first. Handler signature: (lambda (f args) ...).
(define jolt-invoke-prefix-arms '())
(define (register-invoke-prefix-arm! pred handler)
  (set! jolt-invoke-prefix-arms (cons (cons pred handler) jolt-invoke-prefix-arms)))
(define (jolt-invoke-prefix-arm-for f)
  (let loop ((as jolt-invoke-prefix-arms))
    (cond ((null? as) #f)
          (((caar as) f) (cdar as))
          (else (loop (cdr as))))))

;; --- arity pre-check: throw typed ArityException BEFORE applying a proc or
;; invokable, so the site is classified structurally, not by matching Chez's
;; error message text after the fact.
(define (jolt-proc-arity-name f)
  (let ((p (proc-name-of f)))
    (if p (string-append (car p) "/" (cdr p)) "fn")))
(define (jolt-arity-error-name name nargs)
  (jolt-throw (jolt-host-throwable "clojure.lang.ArityException"
                (string-append "Wrong number of args ("
                               (number->string nargs)
                               ") passed to: " name))))
(define (jolt-proc-arity-error f nargs)
  (jolt-arity-error-name (jolt-proc-arity-name f) nargs))
;; check one-arg-or-two: the common arity rule for keywords, symbols, maps.
(define (jolt-check-arity-1or2 name nargs)
  (unless (or (fx=? nargs 1) (fx=? nargs 2))
    (jolt-arity-error-name name nargs)))
;; check exactly-one-arg: vectors and sets on the JVM accept exactly 1 arg.
(define (jolt-check-arity-1 name nargs)
  (unless (fx=? nargs 1)
    (jolt-arity-error-name name nargs)))

(define (jolt-invoke f . args)
  (cond
    ((procedure? f) (let ((n (length args)))
                      (unless (fxlogbit? n (procedure-arity-mask f))
                        (jolt-proc-arity-error f n))
                      (apply f args)))
    ((jolt-invoke-prefix-arm-for f) => (lambda (h) (h f args)))
    ((keyword? f) (let ((n (length args)))
                    (jolt-check-arity-1or2 (if (keyword-t-ns f) 
                                              (string-append ":" (keyword-t-ns f) "/" (keyword-t-name f))
                                              (string-append ":" (keyword-t-name f)))
                                           n)
                    (apply jolt-get (car args) f (cdr args))))   ; (:k m [d]) -> (get m :k [d])
    ((jolt-symbol? f) (let ((n (length args)))
                        (jolt-check-arity-1or2 (symbol-t-name f) n)
                        (apply jolt-get (car args) f (cdr args))))   ; ('s m [d]) -> (get m 's [d])
    ;; a VECTOR invokes as nth, exactly one arg (a bad index throws, like
    ;; IPersistentVector.invoke); 0 or 2+ args are an ArityException.
    ((pvec? f) (let ((n (length args)))
                 (jolt-check-arity-1 (jolt-class-name f) n)
                 (jolt-nth f (car args))))
    ;; a map invokes as get with 1 or 2 args; a set invokes as get with exactly 1.
    ((pmap? f) (let ((n (length args)))
                 (jolt-check-arity-1or2 (jolt-class-name f) n)
                 (apply jolt-get f args)))
    ((pset? f) (let ((n (length args)))
                 (jolt-check-arity-1 (jolt-class-name f) n)
                 (apply jolt-get f args)))
    ((jolt-transient? f) (let ((n (length args)))
                           (jolt-check-arity-1or2 (jolt-class-name f) n)
                           (apply jolt-get f args)))             ; a transient vec/map/set is callable on the JVM
    ;; a record/reify implementing clojure.lang.IFn is callable: dispatch to its
    ;; inline `invoke` method with the value itself as the leading `this`.
    ((and (jrec? f) (find-method-any-protocol (jrec-tag f) "invoke"))
     => (lambda (m) (apply jolt-invoke m f args)))
    ((and (reified-methods f) (hashtable-ref (reified-methods f) "invoke" #f))
     => (lambda (m) (apply jolt-invoke m f args)))
    ;; host types registered as callable (promise delivers, …): consulted only
    ;; after every built-in case missed, so the hot dispatch pays nothing.
    ((jolt-invoke-arm-for f) => (lambda (h) (h f args)))
    ;; calling an unbound var (a declared-but-undefined var's root sentinel) is an
    ;; IllegalStateException on the JVM, not a cast error: "Attempting to call
    ;; unbound fn: #'ns/name".
    ((jolt-var-unbound? f)
     (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException"
                  (string-append "Attempting to call unbound fn: #'"
                                 (jolt-var-unbound-ns f) "/" (jolt-var-unbound-name f)))))
    ;; calling a non-fn: a ClassCastException naming the operator's CLASS (like
    ;; the JVM's "class clojure.lang.LazySeq cannot be cast to ... IFn" — never
    ;; the value, whose printed form may be unbounded: ((range)) must throw, not
    ;; hang rendering an infinite seq). Thrown via jolt-throw so it is catchable
    ;; and carries the throw-site continuation for a stack trace.
    (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                        (string-append
                          "class "
                          (guard (e (#t "value"))
                            (let ((c (jolt-class-name f)))
                              (if (string? c) c (jolt-pr-str f))))
                          " cannot be cast to class clojure.lang.IFn"))))))

;; Fixed-arity entry points: the fast path is a raw-procedure application with NO
;; rest-list allocation (the overwhelmingly common case — a compiled call to a
;; clojure.core/app fn). Anything else (a var-cell, multimethod, keyword, map/vec/
;; set callable) delegates to the full variadic jolt-invoke above, which after the
;; prefix-arm collapse is one lambda, one rest-list, no re-apply. The back end
;; picks jolt-invokeN by arg count (N<=4); apply/variadic cases keep jolt-invoke.
;; Each embeds an arity pre-check — one fxlogbit? test, predicted taken — so a
;; wrong-arity call throws a typed ArityException BEFORE Chez raises a raw condition.
;;
;; Past the procedure? test they also answer the LOOKUP-SHAPED callables — a
;; keyword, symbol, map, set or vector in head position — instead of dropping to
;; variadic jolt-invoke. That fallback is expensive out of proportion to what it
;; does: a rest list per call, (length args), an `apply`, and an arity-name string
;; built by jolt-check-arity-1or2 whether or not the arity is wrong. Measured on a
;; 4-key map, (m k) cost 256 ns against 38.5 ns for the (get m k) it decays to,
;; and (k m) with k a LOCAL cost 71 ns against 33 ns for a literal :k head (which
;; the back end lowers straight to jolt-get and never routes here). honeysql's
;; format-dsl walks 92 clauses per format call as (k leftover) and (k
;; @clause-format), so the local-keyword shape is the one that matters.
;;
;; Ordering: these clauses sit ahead of jolt-invoke's prefix arms (var-cell?,
;; jolt-multifn?), which is safe because those arms own RECORD types — a var cell
;; is not a pmap and a multifn is not a keyword — so no predicate here can claim a
;; value a prefix arm would have. Everything else, records/reifies implementing
;; IFn, transients, sorted maps, unbound vars, still falls through unchanged.
;;
;; Only the arities each type actually accepts are answered here. A vector or set
;; takes exactly one argument and a map or keyword takes one or two, so
;; jolt-invoke2 answers fewer types than jolt-invoke1 and the rest fall through to
;; get their typed ArityException from jolt-invoke, unchanged.
(define (jolt-invoke0 f) (if (procedure? f) (if (fxlogbit? 0 (procedure-arity-mask f)) (f) (jolt-proc-arity-error f 0)) (jolt-invoke f)))
(define (jolt-invoke1 f a)
  (cond
    ((procedure? f) (if (fxlogbit? 1 (procedure-arity-mask f)) (f a) (jolt-proc-arity-error f 1)))
    ((keyword? f) (jolt-get a f))            ; (:k m) -> (get m :k)
    ((pmap? f) (jolt-get f a))               ; (m k)  -> (get m k)
    ((jolt-symbol? f) (jolt-get a f))        ; ('s m) -> (get m 's)
    ((pvec? f) (jolt-nth f a))               ; (v i)  -> (nth v i)
    ((pset? f) (jolt-get f a))               ; (s x)  -> (get s x)
    (else (jolt-invoke f a))))
(define (jolt-invoke2 f a b)
  (cond
    ((procedure? f) (if (fxlogbit? 2 (procedure-arity-mask f)) (f a b) (jolt-proc-arity-error f 2)))
    ((keyword? f) (jolt-get a f b))          ; (:k m d) -> (get m :k d)
    ((pmap? f) (jolt-get f a b))             ; (m k d)  -> (get m k d)
    ((jolt-symbol? f) (jolt-get a f b))      ; ('s m d) -> (get m 's d)
    (else (jolt-invoke f a b))))
(define (jolt-invoke3 f a b c) (if (procedure? f) (if (fxlogbit? 3 (procedure-arity-mask f)) (f a b c) (jolt-proc-arity-error f 3)) (jolt-invoke f a b c)))
(define (jolt-invoke4 f a b c d) (if (procedure? f) (if (fxlogbit? 4 (procedure-arity-mask f)) (f a b c d) (jolt-proc-arity-error f 4)) (jolt-invoke f a b c d)))

;; ============================================================================
;; chunked-seq accessors — the host side of the Clojure IChunkedSeq contract
;; (chunk-first ++ chunk-rest == the seq). Two chunked shapes share the cseq
;; record: a vector-backed seq (cvec = whole pvec, ci = absolute index, crest #f,
;; rest = next 32-block of cvec) and a ChunkedCons (cvec = standalone chunk pvec,
;; crest = the after-chunk seq). natives-array.ss binds these into clojure.core and
;; the chunk-buffer/chunk/chunk-cons builder API on top of them.
;; ============================================================================
(define seq-chunk-size 32)
;; (chunk-pvec . end-index) for a chunked cell, else #f. A ChunkedCons block is the
;; whole remaining chunk (crest carries what comes after); a vector seq block is the
;; next <=32 elements within cvec.
(define (na-vblock s)
  (and (cseq? s) (cseq-cvec s)
       (let ((v (cseq-cvec s)) (i (cseq-ci s)))
         (cons v (if (cseq-crest s) (pvec-count v) (fxmin (fx+ i seq-chunk-size) (pvec-count v)))))))
;; Clojure's chunked-seq? IS (instance? IChunkedSeq s), so this asks the flavor and
;; not just the representation. Both halves are needed: sk-chunked? says the class
;; claims the interface, na-vblock that this cell can actually produce a block. A
;; sorted coll's seq is vector-backed here (the :seq op materializes the tree) yet is
;; a PersistentTreeMap$Seq, which is no more chunked than the JVM's is — answering
;; true for it made chunked-seq? disagree with instance? on the same value.
;; The flavor test is first: it is a fixnum compare, na-vblock allocates a pair.
(define (na-chunked-seq? x)
  (and (cseq? x) (sk-chunked? (cseq-kind x)) (na-vblock x) #t))
;; Copy the block [i, end) straight out of the pvec trie's 32-element leaf node
;; (pv-chunk-for is O(log n)). seq-chunk-size == pv-width and vector-seq blocks are
;; 32-aligned, so a block is exactly one leaf; the rare non-aligned window crossing
;; a leaf boundary falls back to per-index reads. Flattening the whole backing
;; vector per block (pvec-v) made chunk-first O(n), so walking chunk-by-chunk was
;; O(n^2). A ChunkedCons chunk is a small tail-only pvec, so the leaf IS the chunk.
(define (na-chunk-first s)
  (let ((vb (na-vblock s)))
    (if vb
        (let*-values (((pv) (car vb)) ((i) (cseq-ci s)) ((end) (cdr vb)) ((len) (fx- end i))
                      ((node off) (pv-leaf-for pv i)))
          (if (fx<=? (fx+ off len) (vector-length node))
              (make-pvec (vec-copy-range node off (fx+ off len)))
              (let ((out (make-vector len)))
                (let loop ((j 0))
                  (if (fx<? j len)
                      (begin (vector-set! out j (pvec-nth-d pv (fx+ i j) jolt-nil)) (loop (fx+ j 1)))
                      (make-pvec out))))))
        (jolt-first s))))               ; eager-buffer fallback
;; chunk-rest / chunk-next: drop the whole current chunk. For a ChunkedCons that is
;; crest (the after-chunk seq); for a vector seq it is the seq at the next block.
(define (na-chunk-rest s)
  (cond
    ((and (cseq? s) (cseq-crest s))
     (let ((r (jolt-seq (cseq-crest s)))) (if (jolt-nil? r) jolt-empty-list r)))
    ((na-vblock s) => (lambda (vb)
       (if (fx>=? (cdr vb) (pvec-count (car vb))) jolt-empty-list
           (vec->seq/k (car vb) (cdr vb) (cseq-kind s)))))
    (else (jolt-rest s))))
(define (na-chunk-next s)
  (cond
    ((and (cseq? s) (cseq-crest s)) (jolt-seq (cseq-crest s)))
    ((na-vblock s) => (lambda (vb)
       (if (fx>=? (cdr vb) (pvec-count (car vb))) jolt-nil
           (vec->seq/k (car vb) (cdr vb) (cseq-kind s)))))
    (else (jolt-next s))))

;; na-chunk-map-first / na-chunk-filter-first: transform a chunked seq's current
;; block straight out of the source pvec leaf into a fresh chunk pvec, without the
;; intermediate copy na-chunk-first makes (map/filter used to na-chunk-first into a
;; buffer and then re-read it element by element — one wasted pvec alloc + copy per
;; stage per chunk, and for filter also a Scheme list + reverse + list->vector).
;; Leaf resolution mirrors na-chunk-first exactly: one contiguous leaf when the
;; block is 32-aligned, per-index reads for the rare window crossing a leaf.
(define (na-chunk-map-first s g)
  (let*-values (((vb) (na-vblock s)) ((pv) (car vb)) ((i) (cseq-ci s)) ((end) (cdr vb))
                ((len) (fx- end i)) ((out) (make-vector len))
                ((node off) (pv-leaf-for pv i)))
    (if (fx<=? (fx+ off len) (vector-length node))
        (let loop ((j 0))
          (if (fx<? j len)
              (begin (vector-set! out j (g (vector-ref node (fx+ off j)))) (loop (fx+ j 1)))
              (make-pvec out)))
        (let loop ((j 0))
          (if (fx<? j len)
              (begin (vector-set! out j (g (pvec-nth-d pv (fx+ i j) jolt-nil))) (loop (fx+ j 1)))
              (make-pvec out))))))
;; Returns the kept-elements chunk pvec, or #f when the whole block is rejected
;; (so the caller recurses straight into chunk-rest, emitting no empty cell).
(define (na-chunk-filter-first s tp keep)
  (let*-values (((vb) (na-vblock s)) ((pv) (car vb)) ((i) (cseq-ci s)) ((end) (cdr vb))
                ((len) (fx- end i)) ((out) (make-vector len))
                ((node off) (pv-leaf-for pv i))
                ((aligned?) (fx<=? (fx+ off len) (vector-length node))))
    (let loop ((j 0) (w 0))
      (if (fx<? j len)
          (let ((x (if aligned? (vector-ref node (fx+ off j)) (pvec-nth-d pv (fx+ i j) jolt-nil))))
            (if (eq? keep (tp x))
                (begin (vector-set! out w x) (loop (fx+ j 1) (fx+ w 1)))
                (loop (fx+ j 1) w)))
          (cond ((fx=? w 0) #f)
                ((fx=? w len) (make-pvec out))
                (else (make-pvec (vec-copy-range out 0 w))))))))

;; ============================================================================
;; map / filter / reduce / into / remove + range / take / concat / apply
;; ============================================================================
(define (any-nil? seqs) (and (pair? seqs) (or (jolt-nil? (car seqs)) (any-nil? (cdr seqs)))))
;; An EMPTY seq result is () (jolt-empty-list), NOT nil — Clojure's (map f []) is
;; an empty seq, so (= () (map f [])) is true and (nil? (map f [])) is false.
;; jolt-empty-list seqs back to nil, so it stays a valid lazy-tail terminator for
;; the non-empty case (printing / seq= / reduce all walk via jolt-seq).
;; Single-coll map (core.clj's [f coll] arity). Chunk-preserving: when the source
;; seq is chunked, realize the WHOLE first chunk — apply f to every element eagerly
;; into a fresh chunk — and chunk-cons it onto a lazy map of chunk-rest, so the
;; result is itself a chunked-seq. A non-chunked source maps one element at a time.
(define (map-seq f s)
  ;; g: f itself when it's a raw procedure (the common case), so the per-element
  ;; step is a direct (g x); a wrapper applying jolt-invoke otherwise. Bound once
  ;; per call (a local, not a per-element closure) — chunked map amortizes it
  ;; across a whole 32-element chunk.
  (let ((g (if (procedure? f) f (lambda (x) (jolt-invoke f x)))))
    (cond
      ((jolt-nil? s) jolt-empty-list)
      ((na-chunked-seq? s)
       (cseq-chunked (na-chunk-map-first s g) 0
                     (jolt-make-lazy-seq (lambda () (jolt-seq (map-seq f (jolt-seq (na-chunk-rest s))))))))
      (else
       (cseq-lazy (g (seq-first s)) (lambda () (map-seq f (jolt-seq (seq-more s)))))))))
(define (map-seq* f seqs)              ; multi-collection map; stops at the shortest
  (if (any-nil? seqs) jolt-empty-list
      (cseq-lazy (apply jolt-invoke f (map seq-first seqs))
                 (lambda () (map-seq* f (map (lambda (s) (jolt-seq (seq-more s))) seqs))))))
;; map is fully lazy: Clojure's (map f coll) is a LazySeq whose body — including
;; (f (first coll)) — runs only when forced, so a side-effecting f does not fire
;; at construction. Wrap the (eager-headed) map-seq in a lazy-seq node; forcing it
;; once yields the cseq chain, which then iterates with no per-element overhead.
;; jolt-seq coerces map-seq's result (cseq | jolt-empty-list) to cseq | nil, the
;; contract force-lazyseq relies on (see jolt-rest).
(define (jolt-map f . colls)
  (if (null? (cdr colls))
      (jolt-make-lazy-seq (lambda () (jolt-seq (map-seq f (jolt-seq (car colls))))))
      (jolt-make-lazy-seq (lambda () (jolt-seq (map-seq* f (map jolt-seq colls)))))))

;; Chunk-preserving, like core.clj filter: a chunked source has pred applied to the
;; whole chunk, the kept elements packed into a fresh (possibly smaller) chunk, and
;; that chunk-cons'd onto a lazy filter of chunk-rest. An all-rejected chunk emits
;; no empty cell — it recurses straight into chunk-rest (chunk-cons of an empty
;; chunk == its rest). A non-chunked source filters one element at a time.
(define (filter-seq pred s keep)
  ;; tp: a per-element test returning a Scheme boolean. pred itself when raw (the
  ;; common case), wrapped in jolt-truthy?; else jolt-invoke then jolt-truthy?.
  ;; Bound once per call, consulted in both loops below.
  (let ((tp (if (procedure? pred)
                (lambda (x) (jolt-truthy? (pred x)))
                (lambda (x) (jolt-truthy? (jolt-invoke pred x))))))
    (cond
      ((jolt-nil? s) jolt-empty-list)         ; empty result is () (see map-seq)
      ((na-chunked-seq? s)
       (let ((c (na-chunk-filter-first s tp keep)))
         (if (not c)
             (filter-seq pred (jolt-seq (na-chunk-rest s)) keep)
             (cseq-chunked
              c 0
              (jolt-make-lazy-seq
               (lambda ()
                 (jolt-seq (filter-seq pred (jolt-seq (na-chunk-rest s)) keep))))))))
      (else
       (let walk ((s s))
         (cond ((jolt-nil? s) jolt-empty-list)
               ((eq? keep (tp (seq-first s)))
                (cseq-lazy (seq-first s) (lambda () (filter-seq pred (jolt-seq (seq-more s)) keep))))
               (else (walk (jolt-seq (seq-more s))))))))))
;; filter/remove are fully lazy (LazySeq): defer the predicate and the source seq
;; until forced, like Clojure. (lazy-seq* = a 0-arg lazy node coercing to cseq|nil.)
(define (jolt-filter pred coll)
  (jolt-make-lazy-seq (lambda () (jolt-seq (filter-seq pred (jolt-seq coll) #t)))))
(define (jolt-remove pred coll)
  (jolt-make-lazy-seq (lambda () (jolt-seq (filter-seq pred (jolt-seq coll) #f)))))

;; honors `reduced`: a reducing fn that returns (reduced x) stops the fold and
;; unwraps to x (so does a reduced INIT). Checked at entry, so the value returned
;; by the last step is unwrapped on the next turn before the seq is consulted.
;; reduce a vector's backing store directly by index from element i — no per-
;; element seq cells. Honors `reduced`. The chunked-seq fast path.
;; Reduce a chunk pvec from index i. Returns the accumulator RAW — a `reduced` box
;; is returned unwrapped-by reduce-seq, not here — so a ChunkedCons continuation can
;; see early termination instead of folding it back into the running value.
(define (vec-reduce f acc v start)
  (let ((n (pvec-count v)))
    (if (procedure? f)
        (let outer ((i start) (acc acc))
          (cond ((jolt-reduced? acc) acc)
                ((fx>=? i n) acc)
                (else
                 (let*-values (((chunk offset) (pv-leaf-for v i))
                        ((clen) (vector-length chunk)))
                   (let inner ((j offset) (k i) (acc acc))
                     (cond ((jolt-reduced? acc) acc)
                           ((fx>=? j clen) (outer k acc))
                           (else (inner (fx+ j 1) (fx+ k 1) (f acc (vector-ref chunk j))))))))))
        (let outer ((i start) (acc acc))
          (cond ((jolt-reduced? acc) acc)
                ((fx>=? i n) acc)
                (else
                 (let*-values (((chunk offset) (pv-leaf-for v i))
                        ((clen) (vector-length chunk)))
                   (let inner ((j offset) (k i) (acc acc))
                     (cond ((jolt-reduced? acc) acc)
                           ((fx>=? j clen) (outer k acc))
                           (else (inner (fx+ j 1) (fx+ k 1) (jolt-invoke f acc (vector-ref chunk j)))))))))))))
(define (reduce-seq f acc s)
  ;; direct? is bound once (a boolean, no allocation) and consulted in the
  ;; non-chunked step so a raw fn steps with (f acc x) instead of jolt-invoke.
  ;; The chunked branch already hoists via vec-reduce.
  (let ((direct? (procedure? f)))
    (let rec ((acc acc) (s s))
      (cond
        ((jolt-reduced? acc) (jolt-reduced-val acc))
        ((jolt-nil? s) acc)
        ;; a chunked seq reduces its chunk pvec directly, in a tight loop. A vector seq
        ;; (crest #f) reduces the whole backing vector and is then done; a ChunkedCons
        ;; reduces this chunk and continues into its after-chunk rest.
        ((and (cseq? s) (cseq-cvec s))
         (let ((acc2 (vec-reduce f acc (cseq-cvec s) (cseq-ci s))))
           (cond ((jolt-reduced? acc2) (jolt-reduced-val acc2))
                 ((cseq-crest s) (rec acc2 (jolt-seq (cseq-crest s))))
                 (else acc2))))
        (else (rec (if direct? (f acc (seq-first s)) (jolt-invoke f acc (seq-first s)))
                   (jolt-seq (seq-more s))))))))
(define jolt-reduce
  (case-lambda
    ((f coll) (let ((s (jolt-seq coll)))
                (if (jolt-nil? s) (jolt-invoke f)          ; (reduce f []) -> (f)
                    (reduce-seq f (seq-first s) (jolt-seq (seq-more s))))))
    ((f init coll)
     ;; IReduceInit: a deftype/record OR reify with its own `reduce` method drives
     ;; the reduction, e.g. (reduce f init (reify clojure.lang.IReduceInit
     ;; (reduce [_ f i] ...))) or the same on a deftype.
     (cond
       ((iface-method coll "reduce" 3)
        => (lambda (m) (let ((r (jolt-invoke m coll f init)))
                         (if (jolt-reduced? r) (jolt-reduced-val r) r))))
       (else (reduce-seq f init (jolt-seq coll)))))))

;; Fold through a transient so a pvec/pmap/pset target is built in O(n): a
;; persistent pvec-conj copies its whole backing vector each step, making a naive
;; fold O(n^2) (and into/vec/mapv/filterv all route here). jolt-transient-new
;; falls back to a copy-on-write wrapper for other targets (lists, sorted colls,
;; nil), so those keep the old per-step jolt-conj behaviour.
(define (jolt-into to from)
  (cond
    ;; two non-empty vectors: O(log n) RRB concatenation instead of a linear
    ;; element fold. Empty operands fall through — pvec-catvec answers those
    ;; with an INPUT identity, which would surface `from` (and any metadata
    ;; riding on it) as the result instead of `to`'s value with `to`'s meta.
    ((and (pvec? to) (pvec? from)
          (fx>? (pvec-cnt to) 0) (fx>? (pvec-cnt from) 0))
     (meta-carry to (pvec-catvec to from)))
    ;; only an editable collection rides the transient path; anything else
    ;; (PersistentQueue, sorted colls, seqs) folds through conj, like RT's
    ;; instanceof IEditableCollection split.
    ((or (pvec? to) (pmap? to) (pset? to))
     (meta-carry to
       (jolt-persistent! (reduce-seq (lambda (t x) (jolt-conj! t x)) (jolt-transient-new to) (jolt-seq from)))))
    (else
     (meta-carry to
       (reduce-seq (lambda (acc x) (jolt-conj1 acc x)) to (jolt-seq from))))))

;; (range) with no bound is clojure.lang.Iterate on the JVM — it IS (iterate inc' 0)
;; there — so it wears the same flavor jolt-iterate does.
(define (range-from n) (cseq-lazy/k n (lambda () (range-from (+ n 1))) sk-iterate))
;; A bounded range is a real chunked-seq, like clojure.lang.LongRange: eager, with
;; chunk-first handing out a block of up to 32 consecutive values. Each block is
;; materialized into a pvec and chunk-cons'd onto a lazy continuation, so a chunked
;; map/filter over a range batches by 32 (the JVM's observable realization), while a
;; huge range still produces its tail one block at a time.
;; An empty range is () (jolt-empty-list), NOT nil — (range 0) and (range 5 5) are
;; empty seqs in Clojure, so (= () (range 0)) holds, and () seqs back to nil so it
;; also terminates the chunked tail (see jolt-take).
(define (range-chunked n end step)
  (cond
    ((= step 0)
     ;; JVM: (range start end 0) repeats start infinitely
     (cseq-lazy/k n (lambda () (range-chunked n end step)) sk-long-range))
    ((if (> step 0.0) (< n end) (> n end))
     (let loop ((i 0) (v n) (acc '()))
       (if (and (fx<? i seq-chunk-size) (if (> step 0.0) (< v end) (> v end)))
           (loop (fx+ i 1) (+ v step) (cons v acc))
           ;; chunked in REPRESENTATION but a LongRange by class, which is exactly
           ;; the JVM's own arrangement (LongRange implements IChunkedSeq).
           (cseq-chunked/k (make-pvec (list->vector (reverse acc))) 0
                           (jolt-make-lazy-seq (lambda () (jolt-seq (range-chunked v end step))))
                           sk-long-range))))
    (else jolt-empty-list)))
;; numeric tower: exact 0/1 defaults so (range 3) yields exact ints
;; (= JVM longs); flonum args still produce flonums (Scheme arithmetic preserves).
;; (range) with no bound is the lazy, NON-chunked (iterate inc' 0) form.
(define jolt-range
  (case-lambda
    (() (range-from 0))
    ((end) (range-chunked 0 (jolt-need-num end) 1))
    ((start end) (range-chunked (jolt-need-num start) (jolt-need-num end) 1))
    ((start end step) (range-chunked (jolt-need-num start) (jolt-need-num end)
                                     (jolt-need-num step)))))

;; An empty take result is () (jolt-empty-list), NOT nil — (take 0 coll) and
;; (take n []) are empty seqs in Clojure, so (= () (take 0 [:a])) and printing
;; "()" hold. jolt-empty-list seqs back to nil, so it also terminates the lazy
;; tail when n hits 0 mid-stream (see map-seq).
;; The LAST element (n=1) terminates without touching the rest, so (take n s)
;; realizes exactly n elements of a side-effecting seq — matching Clojure, where
;; (take 0 (rest s)) never seqs coll. Realizing one more, as forcing seq-more at
;; the boundary would, over-runs the source by one (medley's sequence-padded).
(define (jolt-take n coll)
  ;; lazy (LazySeq): realize exactly n elements, none at construction. (take
  ;; Double/POSITIVE_INFINITY coll) takes the whole coll on the JVM (the count
  ;; never reaches 0); test.check's rose-tree unchunk relies on it. Coercing +inf.0
  ;; to a fixnum index would throw, so take all up front in that case.
  (jolt-make-lazy-seq
   (lambda ()
     (jolt-seq
      (if (and (flonum? n) (infinite? n))
          (if (> n 0.0) (jolt-seq coll) jolt-empty-list)
          (let ((n (->idx n)))
            (if (fx<=? n 0)
                jolt-empty-list                  ; (take 0 coll) must not seq its source
                (let loop ((n n) (s (jolt-seq coll)))
                  (cond
                    ((or (fx<=? n 0) (jolt-nil? s)) jolt-empty-list)
                    ((fx=? n 1) (cseq-lazy (seq-first s) (lambda () jolt-empty-list)))
                    (else (cseq-lazy (seq-first s) (lambda () (loop (fx- n 1) (jolt-seq (seq-more s)))))))))))))))
;; Dropping from a vector-backed cell is an index jump, not a walk: the result is
;; the same backing vector seen from further along. PersistentVector$ChunkedSeq
;; implements IDrop on the JVM for exactly this, and it is the difference between
;; 625ns and 18.5ms when dropping a million elements. As with count, the step
;; loop re-checks per cell so a few plain cells in front of a vector-backed one
;; still reach the jump.
(define (jolt-drop n coll)
  (jolt-make-lazy-seq
   (lambda ()
     (jolt-seq
      (let loop ((n (->idx n)) (s (jolt-seq coll)))
        (cond
          ((jolt-nil? s) jolt-empty-list)
          ((fx<=? n 0) s)
          ((and (cseq-cvec s) (not (cseq-crest s)))
           (let ((v (cseq-cvec s)) (i (fx+ (cseq-ci s) n)))
             (if (fx>=? i (pvec-count v)) jolt-empty-list (vec->seq v i))))
          (else (loop (fx- n 1) (jolt-seq (seq-more s))))))))))

;; (iterate f x) — x, (f x), (f (f x)), … as ONE lazy cell per element.
;; The overlay spelling, (cons x (lazy-seq (iterate f (f x)))), costs two records
;; and two closures per element: lazy-seq allocates a closure + a jolt-lazyseq
;; node, then cons-over-lazyseq wraps THAT in a cseq cell + another closure to
;; keep the tail unforced. Building the cell directly collapses both layers —
;; head is x and f runs only when the tail is forced, which is the property the
;; overlay spelling existed to protect ((first (iterate f x)) must not call f).
;; Not chunked, matching clojure.lang.Iterate: chunking would realize up to 31
;; elements ahead and break the laziness contract callers depend on.
(define (jolt-iterate f x)
  (cseq-lazy/k x (lambda () (jolt-iterate f (jolt-invoke1 f x))) sk-iterate))

;; lazily append seq a then the seqable produced by the thunk `brest` — the rest
;; is NOT forced until a is exhausted, so concat is fully lazy (Clojure semantics).
;; This matters for a self-referential lazy-cat (fib = (lazy-cat [0 1] (map + (rest
;; fib) fib))): forcing the rest eagerly at construction would read fib before its
;; def binds, memoizing the tail as empty.
(define (concat2 a brest)
  (if (jolt-nil? a) (jolt-seq (brest))
      (cseq-lazy (seq-first a) (lambda () (concat2 (jolt-seq (seq-more a)) brest)))))
(define (jolt-concat . colls)
  (jolt-make-lazy-seq
   (lambda ()
     (jolt-seq
      (cond ((null? colls) jolt-empty-list)
            ((null? (cdr colls)) (jolt-seq (car colls)))
            (else (concat2 (jolt-seq (car colls)) (lambda () (apply jolt-concat (cdr colls))))))))))

;; Lazily concatenate a (possibly infinite) SEQ of colls — what (apply concat ss)
;; means, but without realizing ss. Pulls one coll at a time, so mapcat /
;; (apply concat …) over an infinite source stays lazy.
;;
;; Emits ONE cseq cell per ELEMENT and nothing per collection boundary. Routing
;; through jolt-concat instead — (jolt-concat inner (make-lazy-seq …)) — cost a
;; lazyseq node + closure for the tail, a second node + closure inside
;; jolt-concat (plus its variadic rest-args list), and a third through
;; `apply jolt-concat` when forced: ~3 nodes and ~5 closures per inner
;; collection, which dominates when the inner colls are small (mapcat over
;; 2-element lists is the common shape).
;;
;; An empty inner coll is skipped without emitting a cell, and the outer seq is
;; advanced only at a boundary — so f runs once per inner collection, lazily,
;; exactly as before.
(define (lazy-concat-seq ss)
  (let outer ((s (jolt-seq ss)))
    (if (jolt-nil? s)
        jolt-empty-list
        (let inner ((cur (jolt-seq (seq-first s))))
          (if (jolt-nil? cur)
              (outer (jolt-seq (seq-more s)))          ; empty inner: skip, no cell
              (cseq-lazy (seq-first cur)
                         (lambda ()
                           (let ((nx (jolt-seq (seq-more cur))))
                             (if (jolt-nil? nx)
                                 (outer (jolt-seq (seq-more s)))   ; boundary
                                 (inner nx))))))))))

;; (apply f a b ... coll): spread the trailing seqable into the call.
;;
;; A registered variadic callee (see jolt-register-variadic!) takes its rest as a
;; seq, so the tail is NOT realized: peel exactly V+1 elements off fixed++tail and
;; call with the first V plus a lazy-rest box holding the remainder. That is the
;; JVM's RestFn.applyTo, and it makes (apply f (range)) over a variadic f return
;; instead of allocating until the process dies.
;;
;; V+1, not V, because Clojure ACCEPTS a fixed arity equal to the variadic
;; threshold and the fixed arity wins there:
;;   (apply (fn ([a b] :fixed2) ([a b & r] :var2)) [1 2])  =>  :fixed2
;; Calling with V args plus a box would be V+1 args and would wrongly select the
;; variadic arity. At V+1 real elements the variadic arity is unambiguous, because
;; a fixed arity with MORE params than the variadic one is a compile error.
;; Fewer than V+1 available means the call is one of the small fixed arities:
;; fall through to the eager path, which is finite and settles arity selection
;; and any ArityException exactly as before.
;;
;; jolt-concat keeps its own arm rather than registering: it is lazy in its rest
;; but hand-written, and lazy-concat-seq is a better lowering than a boxed rest
;; would be (it walks the colls lazily as well as the tail).
;;
;; Peel exactly n elements off a seq: (peeled-list . rest-seq), or #f if the seq
;; is shorter than n. Only n elements are ever forced.
(define (jolt-peel n s)
  (let loop ((i 0) (s (jolt-seq s)) (acc '()))
    (cond ((fx=? i n) (cons (reverse acc) s))
          ((jolt-nil? s) #f)
          (else (loop (fx+ i 1) (jolt-seq (seq-more s)) (cons (seq-first s) acc))))))
(define (jolt-apply f . args)
  (let* ((r (reverse args)) (tail (car r)) (fixed (reverse (cdr r)))
         (v (and (procedure? f) (variadic-fixed-arity-of f))))
    (cond
      ((eq? f jolt-concat)
       (lazy-concat-seq (fold-right jolt-cons (jolt-seq tail) fixed)))
      ;; registered variadic: peel V+1 off fixed++tail, pass V + a boxed rest
      ((and v (jolt-peel (fx+ v 1) (fold-right cseq-realized (jolt-seq tail) fixed)))
       => (lambda (peeled)
            (let ((head (list-head (car peeled) v))
                  (spill (list-tail (car peeled) v)))   ; exactly one element
              (apply jolt-invoke f
                     (append head
                             (list (make-lazy-rest
                                    (fold-right cseq-realized (cdr peeled) spill))))))))
      (else (apply jolt-invoke f (append fixed (seq->list (jolt-seq tail))))))))

;; ============================================================================
;; numeric predicates / identity — usable in fn AND value position (map/filter).
;; Return Scheme #t/#f (= jolt true/false). All-flonum model: coerce to an exact
;; integer for the parity tests.
;; ============================================================================
;; Parity over the full integer range (JVM even?/odd? accept any integer,
;; bignums included); a fixnum-only fxand crashes on a large value (e.g. a hash).
(define (parity-int n) (if (flonum? n) (exact (floor n)) n))
(define (jolt-parity-check n)
  (unless (and (number? n) (exact? n) (integer? n))
    (jolt-throw (jolt-host-throwable
                 "java.lang.IllegalArgumentException"
                 (string-append "Argument must be an integer: "
                                (guard (e (#t "?")) (jolt-str n)))))))
(define (jolt-even? n) (jolt-parity-check n) (even? (parity-int n)))
(define (jolt-odd? n) (jolt-parity-check n) (odd? (parity-int n)))
(define (jolt-pos? n) (> (jolt-need-num n) 0))
(define (jolt-neg? n) (< (jolt-need-num n) 0))
(define (jolt-zero? n) (= (jolt-need-num n) 0))
(define (jolt-identity x) x)

;; ============================================================================
;; keys / vals — return seqs (nil on the empty map), HAMT-iteration order
;; ============================================================================
;; keys/vals of anything empty is nil (RT.keys over a nil seq); a non-empty
;; non-map still fails (its elements are not MapEntries).
;; Like RT.keys/vals, a non-map argument is any seq of map entries — (keys
;; (filter pred a-map)) walks the entries.
;; An element the JVM could not cast to Map.Entry is a ClassCastException naming
;; its class — never a nil, and never a stray index into whatever the element
;; happened to be ((keys ["ab"]) must throw, not hand back the strings' first
;; characters). A plain 2-element vector is accepted where the JVM casts: jolt
;; reads a vector of pairs as a seq of entries, a documented superset.
(define (entry-like? e)
  (and (pvec? e) (= 2 (pvec-count e))))
(define (entry-cast-error e)
  (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                (string-append
                  "class "
                  (guard (x (#t "value"))
                    (let ((c (jolt-class-name e)))
                      (if (string? c) c (jolt-pr-str e))))
                  " cannot be cast to class java.util.Map$Entry"))))
;; idx picks the half; the flavor says which half, since the JVM has a class for
;; each (APersistentMap$KeySeq / $ValSeq).
(define (entry-seq-part m idx kind)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s)
        (list->cseq/k (reverse acc) kind)
        (let ((e (seq-first s)))
          (unless (entry-like? e) (entry-cast-error e))
          (loop (jolt-seq (seq-more s)) (cons (jolt-nth e idx jolt-nil) acc))))))
(define (jolt-keys m)
  (cond ((jolt-nil? m) jolt-nil)
        ((pmap? m) (list->cseq/k (pmap-fold m (lambda (k v a) (cons k a)) '()) sk-key-seq))
        ((jolt-nil? (jolt-seq m)) jolt-nil)
        ((pmap? (jolt-seq m)) (list->cseq/k (pmap-fold m (lambda (k v a) (cons k a)) '()) sk-key-seq))
        (else (entry-seq-part m 0 sk-key-seq))))
(define (jolt-vals m)
  (cond ((jolt-nil? m) jolt-nil)
        ((pmap? m) (list->cseq/k (pmap-fold m (lambda (k v a) (cons v a)) '()) sk-val-seq))
        ((jolt-nil? (jolt-seq m)) jolt-nil)
        ((pmap? (jolt-seq m)) (list->cseq/k (pmap-fold m (lambda (k v a) (cons v a)) '()) sk-val-seq))
        (else (entry-seq-part m 1 sk-val-seq))))

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
  ;; JVM-compatible ordered-collection hash (Murmur3.hashOrdered via mixCollHash).
  (hash-ordered (jolt-seq x)))
