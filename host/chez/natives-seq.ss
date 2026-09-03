;; seq-native shims — native seq fns the overlay assumes are clojure.core
;; natives. Each is a pure fn over the existing seq layer (seq.ss) — collection
;; arities only; the 1-arg transducer arities follow below. Loaded last (after
;; converters.ss for jolt-compare and seq.ss for the reduced record).

;; reduced / reduced? — the box itself is the jolt-reduced record from seq.ss
;; (so the reduce machinery there can see it); these just expose the constructor
;; and predicate. (deref a-reduced) is handled in atoms.ss.
(define (jolt-reduced-new x) (make-jolt-reduced x))
(define (jolt-reduced-pred x) (jolt-reduced? x))
(define (ensure-reduced x) (if (jolt-reduced? x) x (make-jolt-reduced x)))

;; ============================================================================
;; transducers — the 1-arg arity of map/filter/take/... returns a
;; transducer (fn [rf] rf') where rf' is a reducing fn with arities
;; []=init, [acc]=complete, [acc x]=step. rf and the mapping/predicate fns are jolt values, so every
;; call routes through jolt-invoke. A `reduced` step stops the fold — reduce-seq
;; (seq.ss) already short-circuits on a jolt-reduced.
;; ============================================================================
;; Each stage rf is a case-lambda with the exact transducer arities — []=init,
;; [acc]=complete, [acc x]=step — instead of a variadic (lambda a (case (length a)
;; …)). The variadic form allocated a rest-list and walked (length a) on EVERY
;; element, then forwarded through general jolt-invoke; case-lambda dispatches on
;; arity with no rest-list, and the step calls the downstream rf via jolt-invoke2
;; and the mapping/predicate fn via jolt-invoke1 (fixed-arity fast paths). A
;; `reduced` step stops the fold — reduce-seq (seq.ss) short-circuits on it.
;; The map transducer additionally supports multiple inputs ([result input &
;; inputs]) — a multi-collection sequence/transduce, or medley's sequence-padded
;; calling (f acc i1 i2 …) — via a trailing variadic clause, so the single-input
;; hot path stays allocation-free while (rf result (apply f inputs)) still works.
(define (td-map f)
  (lambda (rf)
    (case-lambda
      (() (jolt-invoke rf))
      ((acc) (jolt-invoke1 rf acc))
      ((acc x) (jolt-invoke2 rf acc (jolt-invoke1 f x)))
      ((acc . xs) (jolt-invoke2 rf acc (apply jolt-invoke f xs))))))
(define (td-filter pred)
  (lambda (rf)
    (case-lambda
      (() (jolt-invoke rf))
      ((acc) (jolt-invoke1 rf acc))
      ((acc x) (if (jolt-truthy? (jolt-invoke1 pred x))
                   (jolt-invoke2 rf acc x)
                   acc)))))
(define (td-remove pred) (td-filter (lambda (x) (jolt-not (jolt-invoke1 pred x)))))
(define (td-take n)
  (lambda (rf)
    (let ((left n))
      (case-lambda
        (() (jolt-invoke rf))
        ((acc) (jolt-invoke1 rf acc))
        ((acc x) (if (<= left 0)
                     (make-jolt-reduced acc)
                     (let ((r (jolt-invoke2 rf acc x)))
                       (set! left (- left 1))
                       (if (<= left 0) (ensure-reduced r) r))))))))
(define (td-drop n)
  (lambda (rf)
    (let ((left n))
      (case-lambda
        (() (jolt-invoke rf))
        ((acc) (jolt-invoke1 rf acc))
        ((acc x) (if (> left 0) (begin (set! left (- left 1)) acc)
                     (jolt-invoke2 rf acc x)))))))
(define (td-take-while pred)
  (lambda (rf)
    (case-lambda
      (() (jolt-invoke rf))
      ((acc) (jolt-invoke1 rf acc))
      ((acc x) (if (jolt-truthy? (jolt-invoke1 pred x))
                   (jolt-invoke2 rf acc x)
                   (make-jolt-reduced acc))))))
(define (td-drop-while pred)
  (lambda (rf)
    (let ((dropping #t))
      (case-lambda
        (() (jolt-invoke rf))
        ((acc) (jolt-invoke1 rf acc))
        ((acc x) (begin
                   (when (and dropping (not (jolt-truthy? (jolt-invoke1 pred x))))
                     (set! dropping #f))
                   (if dropping acc (jolt-invoke2 rf acc x))))))))
;; (mapcat f) transducer: map f, then splice (cat) f's result into rf, honoring a
;; mid-splice `reduced`.
(define (td-mapcat f)
  (lambda (rf)
    (case-lambda
      (() (jolt-invoke rf))
      ((acc) (jolt-invoke1 rf acc))
      ((acc x) (let loop ((acc acc)
                          (xs (seq->list (jolt-seq (jolt-invoke1 f x)))))
                 (if (or (null? xs) (jolt-reduced? acc)) acc
                     (loop (jolt-invoke2 rf acc (car xs)) (cdr xs))))))))

;; (into to xform from): transduce `from` through `xform` with conj as the rf.
(define (into-xform to xform from)
  ;; conj onto a nil accumulator starts a list, as (conj nil x) does — into nil
  ;; is (reduce conj nil from) on the JVM, and an empty source leaves nil.
  (let* ((conj-rf (lambda a (if (fx=? (length a) 1) (car a)   ; completion = identity
                               (jolt-conj1 (if (jolt-nil? (car a)) jolt-empty-list (car a)) (cadr a)))))
         (xrf (jolt-invoke xform conj-rf))
         ;; into-fold, not reduce-seq: an IReduce(Init) source drives its own
         ;; reduce here too (seq.ss).
         (res (into-fold xrf to from)))
    (jolt-invoke xrf res)))

;; mapcat: (mapcat f) -> transducer; (mapcat f coll & colls) -> map f across the
;; colls (stops at shortest), then concat the results.
(define (jolt-mapcat f . colls)
  (if (null? colls)
      (td-mapcat f)
      ;; lazily concat the per-element results — no seq->list, so mapcat over an
      ;; infinite source stays lazy; the outer lazy-seq node defers the first
      ;; element so a side-effecting f does not fire at construction (LazySeq).
      (jolt-make-lazy-src lz-mapcat f colls)))

;; take-while / drop-while: 1-arg -> transducer; 2-arg -> a seq over the coll.
(define lz-mapcat
  (register-lazy-src! 'mapcat
    (lambda (f colls) (jolt-seq (lazy-concat-seq (apply jolt-map f colls))))))
(define lz-take-while-top
  (register-lazy-src! 'take-while-top
    (lambda (pred coll) (jolt-seq (take-while-seq pred (jolt-seq coll))))))
(define lz-drop-while
  (register-lazy-src! 'drop-while
    (lambda (pred coll) (jolt-seq (drop-while-seq pred coll)))))
(define lz-take-while
  (register-lazy-src! 'take-while
    (lambda (pred s) (take-while-seq pred (jolt-seq (seq-more s))))))
(define (take-while-seq pred s)
  (if (jolt-nil? s) jolt-empty-list
      (let ((x (seq-first s)))
        (if (jolt-truthy? (jolt-invoke pred x))
            (cseq-lazy x (make-lazy-src lz-take-while pred s))
            jolt-empty-list))))
(define jolt-take-while
  (case-lambda
    ((pred) (td-take-while pred))
    ((pred coll) (jolt-make-lazy-src lz-take-while-top pred coll))))
(define (drop-while-seq pred coll)
  (let loop ((s (jolt-seq coll)))
    (if (and (not (jolt-nil? s)) (jolt-truthy? (jolt-invoke pred (seq-first s))))
        (loop (jolt-seq (seq-more s)))
        (if (jolt-nil? s) jolt-empty-list s))))
(define jolt-drop-while
  (case-lambda
    ((pred) (td-drop-while pred))
    ((pred coll) (jolt-make-lazy-src lz-drop-while pred coll))))

;; partition: (partition n coll), (partition n step coll), or
;; (partition n step pad coll). Only complete partitions of size n are kept;
;; with pad, a short final partition is padded from pad (and may be < n if pad
;; runs out). Each partition is a seq; the whole result is a lazy seq of seqs.
(define lz-partition
  (register-lazy-src! 'partition
    (lambda (shape coll)
      (jolt-seq (partition* (car shape) (cadr shape) (caddr shape) (cadddr shape) coll)))))
(define lz-partition-more
  (register-lazy-src! 'partition-more
    (lambda (shape s)
      (partition-walk (car shape) (cadr shape) (caddr shape) (cadddr shape)
                      (jolt-seq (advance-by (cadr shape) s))))))
(define jolt-partition
  (case-lambda
    ((n coll) (jolt-make-lazy-src lz-partition (list (->idx n) (->idx n) #f #f) coll))
    ((n step coll) (jolt-make-lazy-src lz-partition (list (->idx n) (->idx step) #f #f) coll))
    ((n step pad coll) (jolt-make-lazy-src lz-partition (list (->idx n) (->idx step) #t pad) coll))))
(define (take-n n s)               ; -> (values list-of-first-n remaining-seq taken-count)
  (let loop ((n n) (s s) (acc '()))
    (if (or (fx<=? n 0) (jolt-nil? s))
        (values (reverse acc) s (length acc))
        (loop (fx- n 1) (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))
(define (partition* n step has-pad pad coll) (partition-walk n step has-pad pad (jolt-seq coll)))
;; a top-level walk, not a named let, so a cell's tail can name it. The four
;; shape values ride in one slot as a list -- lazy-src carries three.
(define (partition-walk n step has-pad pad s0)
  (let ((shape (list n step has-pad pad)))   ; once per walk, not once per cell
   (let loop ((s s0))
    (if (jolt-nil? s) jolt-empty-list
        (let-values (((part rest taken) (take-n n s)))
          (cond
            ;; full partition: emit it, advance `step` from its START
            ((fx=? taken n)
             (cseq-lazy (list->cseq part)
                        (make-lazy-src lz-partition-more shape s)))
            ;; short final partition with pad: top up to n from pad, then stop
            ((and has-pad (fx>? taken 0))
             (let ((padded (append part (take-list (- n taken) (jolt-seq pad)))))
               (cseq-lazy (list->cseq padded) (make-lazy-src lz-empty #f #f))))
            ;; short final partition, no pad: dropped (Clojure keeps only full ones)
            (else jolt-empty-list)))))))
(define (advance-by step s)        ; drop `step` elements from s (seq), returns a seq
  (let loop ((step step) (s s))
    (if (or (fx<=? step 0) (jolt-nil? s)) s
        (loop (fx- step 1) (jolt-seq (seq-more s))))))
(define (take-list n s)            ; up to n elements of seq s as a Scheme list
  (let loop ((n n) (s s) (acc '()))
    (if (or (fx<=? n 0) (jolt-nil? s)) (reverse acc)
        (loop (fx- n 1) (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))

;; A comparator VALUE -> a 2-arg compare procedure. A fn (or any IFn: keyword,
;; var, map) is invoked; a deftype/reify/record implementing java.util.Comparator
;; is NOT IFn on the JVM, so its `compare` METHOD is called instead. The test is
;; whether the value HAS that method — through the shared iface-method lookup,
;; which covers deftype and reify alike — rather than how the value was built:
;; keying on jreify? silently left every deftype Comparator throwing
;; ClassCastException. THE seam for comparators, so sort / sort-by /
;; sorted-map-by / sorted-set-by cannot drift apart on which values they accept.
;; iface-method + record-method-dispatch live in records.ss (loaded later);
;; resolved at call time.
;; A host-shim Comparator object (String/CASE_INSENSITIVE_ORDER) keeps its
;; `compare` in the jhost method table, not the deftype/reify one. The java host
;; layer (host-static.ss, loaded later) set!s this to look there; the Gambit
;; boot, which has no jhost, keeps the #f default.
(define jhost-compare-method? (lambda (x) #f))
(define (jolt-comparator-fn cmp)
  (if (or (iface-method cmp "compare" #f) (jhost-compare-method? cmp))
      (lambda (a b) (record-method-dispatch cmp "compare" (jolt-list a b)))
      (lambda (a b) (jolt-invoke cmp a b))))
(def-var! "clojure.core" "__comparator-fn" jolt-comparator-fn)

;; sort: (sort coll) uses compare; (sort cmp coll) uses cmp, whose result may be
;; a 3-way number (<0 / 0 / >0) OR a boolean (a Clojure-style less-than pred).
(define (cmp->less cmp)
  (let ((f (jolt-comparator-fn cmp)))
    (lambda (a b)
      (let ((r (f a b)))
        (if (number? r) (< r 0) (jolt-truthy? r))))))
(define jolt-sort
  (case-lambda
    ((coll) (jolt-sort* (cmp->less jolt-compare) coll))
    ((cmp coll) (jolt-sort* (cmp->less cmp) coll))))
;; clojure.lang.ArraySeq: RT.sort copies into an Object[], sorts it in place and
;; hands back that array's seq, so (class (sort …)) is an ArraySeq on the JVM.
(define (jolt-sort* less? coll)
  (let ((s (jolt-seq coll)))
    (if (jolt-nil? s) jolt-empty-list
        (list->cseq/k (list-sort less? (seq->list s)) sk-array-seq))))

;; identical?: reference identity (Clojure ==). eq? gives pointer identity over
;; the value model — interned keywords/fixnums/nil compare equal, distinct
;; collections do not. Must NOT be value equality: a deftype whose .equals calls
;; (identical? this o) to short-circuit (e.g. core.logic's Substitutions) would
;; otherwise recur forever (identical? -> = -> equiv -> .equals -> identical?).
;; Spliced, like the predicates in values.ss (see jolt-nil? there for why). The
;; reference compiler inlines identical? too (:inline -> Util/identical), and a
;; var call here already cost ~100ns per use in cell-less seed code.
(define (jolt-identical?-fn a b) (eq? a b))
(define-syntax jolt-identical?
  (syntax-rules ()
    ((_ a b) (eq? a b))
    ((_ e ...) (jolt-identical?-fn e ...))))

;; Give the seq.ss native procedures their transducer (1-arg) arity — the emitter
;; lowers (map f)/(filter p)/(take n) at the wrong arity to the bare procedure
;; (value-position path), so widening the procedures is what makes the 1-arg form
;; work. Capture the originals (collection arities) first, then redefine.
(define %prev-jolt-map jolt-map)
(set! jolt-map (lambda (f . colls)
                 (if (null? colls) (td-map f) (apply %prev-jolt-map f colls))))
(define %prev-jolt-filter jolt-filter)
(set! jolt-filter (case-lambda ((pred) (td-filter pred))
                               ((pred coll) (%prev-jolt-filter pred coll))))
(define %prev-jolt-remove jolt-remove)
(set! jolt-remove (case-lambda ((pred) (td-remove pred))
                               ((pred coll) (%prev-jolt-remove pred coll))))
(define %prev-jolt-take jolt-take)
(set! jolt-take (case-lambda ((n) (td-take n))
                             ((n coll) (%prev-jolt-take n coll))))
(define %prev-jolt-drop jolt-drop)
(set! jolt-drop (case-lambda ((n) (td-drop n))
                             ((n coll) (%prev-jolt-drop n coll))))
;; into: add the 3-arg (into to xform from). The 2-arg stays the seq.ss fold.
(define %prev-jolt-into jolt-into)
(set! jolt-into (case-lambda ((to from) (%prev-jolt-into to from))
                             ((to xform from) (into-xform to xform from))))

(def-var! "clojure.core" "reduced" jolt-reduced-new)
(def-var! "clojure.core" "reduced?" jolt-reduced-pred)
(def-var! "clojure.core" "mapcat" jolt-mapcat)
(def-var! "clojure.core" "zipmap" jolt-zipmap)
(def-var! "clojure.core" "reduce-kv" jolt-reduce-kv)
(def-var! "clojure.core" "take-while" jolt-take-while)
(def-var! "clojure.core" "drop-while" jolt-drop-while)
(def-var! "clojure.core" "partition" jolt-partition)
(def-var! "clojure.core" "sort" jolt-sort)
(def-var! "clojure.core" "identical?" jolt-identical?-fn)

;; rseq: vectors + sorted colls only (Clojure), the reverse of the ascending seq.
;; Clojure's contract is explicit that this is CONSTANT time ("Returns, in
;; constant time, a seq of the items in rev ... in reverse order") — it hands back
;; a reverse view, APersistentVector$RSeq. jolt materialized the whole collection
;; into a list, reversed it and rebuilt a seq: O(n) time and O(n) garbage for an
;; operation whose whole point is that it is free. Over 200k elements that was
;; 19.8ms against 209ns.
;;
;; A vector walks its indices downward instead, one lazy cell at a time. NOTE the
;; cell is a plain lazy cell and deliberately NOT a cvec-backed one: cvec/ci mean
;; "vector-backed, ASCENDING from ci" and drive the O(1) count/drop and the
;; vec-reduce fast paths, so a descending seq wearing those fields would make all
;; three answer for the wrong direction.
(define lz-vec-rseq
  (register-lazy-src! 'vec-rseq (lambda (v i) (vec->rseq v (fx- i 1)))))
(define (vec->rseq v i)
  (if (fx<? i 0)
      jolt-nil
      (cseq-lazy/k (pvec-nth-d v i jolt-nil) (make-lazy-src lz-vec-rseq v i) sk-rseq)))
(define (jolt-rseq coll)
  (cond
    ((pvec? coll)
     (let ((n (pvec-count coll)))
       (if (fx=? n 0) jolt-nil (vec->rseq coll (fx- n 1)))))
    ;; a sorted coll's descending seq is still a PersistentTreeMap$Seq on the JVM
    ;; (the same class with ascending=false), not an RSeq — that one is the vector's.
    ((htable-sorted? coll)
     (list->cseq/k (reverse (seq->list (jolt-seq coll)))
                   (if (htable-sorted-set? coll) sk-key-seq sk-treemap-seq)))
    ;; a deftype/record implementing clojure.lang.Reversible (rseq) — e.g.
    ;; data.priority-map — drives rseq through its own method.
    ((and (jrec? coll) (find-method-any-protocol (jrec-tag coll) "rseq"))
     => (lambda (f) (jolt-invoke f coll)))
    (else (jolt-cast-throw coll "clojure.lang.Reversible"))))
(def-var! "clojure.core" "rseq" jolt-rseq)

;; clojure.core/unchecked-* — host-defined wrapping (Java long) arithmetic from
;; seq.ss. def-var!'d here because def-var! isn't bound when seq.ss loads.
;; The -int variants are INT-width: they wrap at 32 bits, not 64. Aliasing them to
;; the long ops let a value climb past 2^31 and then blow up at the next (int x) —
;; instaparse's hash mixing (unchecked-multiply-int in a loop) is exactly that
;; shape. jolt-unchecked-int does the wrap-and-sign-fold, so each -int op is its
;; long counterpart folded back to 32 bits, which is what the JVM's int overflow is.
(let ((d! (lambda (n v) (def-var! "clojure.core" n v)))
      (as-int (lambda (f) (lambda args (jolt-unchecked-int (apply f args))))))
  (d! "unchecked-add" jolt-unchecked-add)        (d! "unchecked-add-int" (as-int jolt-unchecked-add))
  (d! "unchecked-subtract" jolt-unchecked-sub)   (d! "unchecked-subtract-int" (as-int jolt-unchecked-sub))
  (d! "unchecked-multiply" jolt-unchecked-mul)   (d! "unchecked-multiply-int" (as-int jolt-unchecked-mul))
  (d! "unchecked-negate" jolt-uncneg)            (d! "unchecked-negate-int" (as-int jolt-uncneg))
  (d! "unchecked-inc" jolt-uncinc)               (d! "unchecked-inc-int" (as-int jolt-uncinc))
  (d! "unchecked-dec" jolt-uncdec)               (d! "unchecked-dec-int" (as-int jolt-uncdec))
  ;; quotient/remainder of two ints is already in range; the fold is a no-op except
  ;; at Integer/MIN_VALUE / -1, where the JVM also wraps back to MIN_VALUE.
  (d! "unchecked-divide-int" (as-int jolt-unchecked-div))
  (d! "unchecked-remainder-int" (as-int jolt-unchecked-rem)))
