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
;; The map transducer's step fn supports multiple inputs ([result input & inputs]),
;; so a multi-collection sequence/transduce — or medley's sequence-padded, which
;; calls (f acc i1 i2 …) — applies f across all of them: (rf result (apply f inputs)).
(define (td-map f)
  (lambda (rf)
    (lambda a
      (case (length a)
        ((0) (jolt-invoke rf))
        ((1) (jolt-invoke rf (car a)))
        (else (jolt-invoke rf (car a) (apply jolt-invoke f (cdr a))))))))
(define (td-filter pred)
  (lambda (rf)
    (lambda a
      (case (length a)
        ((0) (jolt-invoke rf))
        ((1) (jolt-invoke rf (car a)))
        (else (if (jolt-truthy? (jolt-invoke pred (cadr a)))
                  (jolt-invoke rf (car a) (cadr a))
                  (car a)))))))
(define (td-remove pred) (td-filter (lambda (x) (jolt-not (jolt-invoke pred x)))))
(define (td-take n)
  (lambda (rf)
    (let ((left n))
      (lambda a
        (case (length a)
          ((0) (jolt-invoke rf))
          ((1) (jolt-invoke rf (car a)))
          (else (if (<= left 0)
                    (make-jolt-reduced (car a))
                    (let ((r (jolt-invoke rf (car a) (cadr a))))
                      (set! left (- left 1))
                      (if (<= left 0) (ensure-reduced r) r)))))))))
(define (td-drop n)
  (lambda (rf)
    (let ((left n))
      (lambda a
        (case (length a)
          ((0) (jolt-invoke rf))
          ((1) (jolt-invoke rf (car a)))
          (else (if (> left 0) (begin (set! left (- left 1)) (car a))
                    (jolt-invoke rf (car a) (cadr a)))))))))
(define (td-take-while pred)
  (lambda (rf)
    (lambda a
      (case (length a)
        ((0) (jolt-invoke rf))
        ((1) (jolt-invoke rf (car a)))
        (else (if (jolt-truthy? (jolt-invoke pred (cadr a)))
                  (jolt-invoke rf (car a) (cadr a))
                  (make-jolt-reduced (car a))))))))
(define (td-drop-while pred)
  (lambda (rf)
    (let ((dropping #t))
      (lambda a
        (case (length a)
          ((0) (jolt-invoke rf))
          ((1) (jolt-invoke rf (car a)))
          (else (begin
                  (when (and dropping (not (jolt-truthy? (jolt-invoke pred (cadr a)))))
                    (set! dropping #f))
                  (if dropping (car a) (jolt-invoke rf (car a) (cadr a))))))))))
;; (mapcat f) transducer: map f, then splice (cat) f's result into rf, honoring a
;; mid-splice `reduced`.
(define (td-mapcat f)
  (lambda (rf)
    (lambda a
      (case (length a)
        ((0) (jolt-invoke rf))
        ((1) (jolt-invoke rf (car a)))
        (else (let loop ((acc (car a))
                         (xs (seq->list (jolt-seq (jolt-invoke f (cadr a))))))
                (if (or (null? xs) (jolt-reduced? acc)) acc
                    (loop (jolt-invoke rf acc (car xs)) (cdr xs)))))))))

;; (into to xform from): transduce `from` through `xform` with conj as the rf.
(define (into-xform to xform from)
  (let* ((conj-rf (lambda a (if (fx=? (length a) 1) (car a)   ; completion = identity
                               (jolt-conj1 (car a) (cadr a)))))
         (xrf (jolt-invoke xform conj-rf))
         (res (reduce-seq xrf to (jolt-seq from))))
    (jolt-invoke xrf res)))

;; mapcat: (mapcat f) -> transducer; (mapcat f coll & colls) -> map f across the
;; colls (stops at shortest), then concat the results.
(define (jolt-mapcat f . colls)
  (if (null? colls)
      (td-mapcat f)
      ;; lazily concat the per-element results — no seq->list, so mapcat over an
      ;; infinite source stays lazy; the outer lazy-seq node defers the first
      ;; element so a side-effecting f does not fire at construction (LazySeq).
      (jolt-make-lazy-seq (lambda () (jolt-seq (lazy-concat-seq (apply jolt-map f colls)))))))

;; take-while / drop-while: 1-arg -> transducer; 2-arg -> a seq over the coll.
(define (take-while-seq pred s)
  (if (jolt-nil? s) jolt-empty-list
      (let ((x (seq-first s)))
        (if (jolt-truthy? (jolt-invoke pred x))
            (cseq-lazy x (lambda () (take-while-seq pred (jolt-seq (seq-more s)))))
            jolt-empty-list))))
(define jolt-take-while
  (case-lambda
    ((pred) (td-take-while pred))
    ((pred coll) (jolt-make-lazy-seq (lambda () (jolt-seq (take-while-seq pred (jolt-seq coll))))))))
(define (drop-while-seq pred coll)
  (let loop ((s (jolt-seq coll)))
    (if (and (not (jolt-nil? s)) (jolt-truthy? (jolt-invoke pred (seq-first s))))
        (loop (jolt-seq (seq-more s)))
        (if (jolt-nil? s) jolt-empty-list s))))
(define jolt-drop-while
  (case-lambda
    ((pred) (td-drop-while pred))
    ((pred coll) (jolt-make-lazy-seq (lambda () (jolt-seq (drop-while-seq pred coll)))))))

;; partition: (partition n coll), (partition n step coll), or
;; (partition n step pad coll). Only complete partitions of size n are kept;
;; with pad, a short final partition is padded from pad (and may be < n if pad
;; runs out). Each partition is a seq; the whole result is a lazy seq of seqs.
(define jolt-partition
  (case-lambda
    ((n coll) (jolt-make-lazy-seq (lambda () (jolt-seq (partition* (->idx n) (->idx n) #f #f coll)))))
    ((n step coll) (jolt-make-lazy-seq (lambda () (jolt-seq (partition* (->idx n) (->idx step) #f #f coll)))))
    ((n step pad coll) (jolt-make-lazy-seq (lambda () (jolt-seq (partition* (->idx n) (->idx step) #t pad coll)))))))
(define (take-n n s)               ; -> (values list-of-first-n remaining-seq taken-count)
  (let loop ((n n) (s s) (acc '()))
    (if (or (fx<=? n 0) (jolt-nil? s))
        (values (reverse acc) s (length acc))
        (loop (fx- n 1) (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))
(define (partition* n step has-pad pad coll)
  (let loop ((s (jolt-seq coll)))
    (if (jolt-nil? s) jolt-empty-list
        (let-values (((part rest taken) (take-n n s)))
          (cond
            ;; full partition: emit it, advance `step` from its START
            ((fx=? taken n)
             (cseq-lazy (list->cseq part)
                        (lambda () (loop (jolt-seq (advance-by step s))))))
            ;; short final partition with pad: top up to n from pad, then stop
            ((and has-pad (fx>? taken 0))
             (let ((padded (append part (take-list (- n taken) (jolt-seq pad)))))
               (cseq-lazy (list->cseq padded) (lambda () jolt-empty-list))))
            ;; short final partition, no pad: dropped (Clojure keeps only full ones)
            (else jolt-empty-list))))))
(define (advance-by step s)        ; drop `step` elements from s (seq), returns a seq
  (let loop ((step step) (s s))
    (if (or (fx<=? step 0) (jolt-nil? s)) s
        (loop (fx- step 1) (jolt-seq (seq-more s))))))
(define (take-list n s)            ; up to n elements of seq s as a Scheme list
  (let loop ((n n) (s s) (acc '()))
    (if (or (fx<=? n 0) (jolt-nil? s)) (reverse acc)
        (loop (fx- n 1) (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))

;; sort: (sort coll) uses compare; (sort cmp coll) uses cmp, whose result may be
;; a 3-way number (<0 / 0 / >0) OR a boolean (a Clojure-style less-than pred).
(define (cmp->less cmp)
  (lambda (a b)
    (let ((r (jolt-invoke cmp a b)))
      (if (number? r) (< r 0) (jolt-truthy? r)))))
(define jolt-sort
  (case-lambda
    ((coll) (jolt-sort* (cmp->less jolt-compare) coll))
    ((cmp coll) (jolt-sort* (cmp->less cmp) coll))))
(define (jolt-sort* less? coll)
  (let ((s (jolt-seq coll)))
    (if (jolt-nil? s) jolt-empty-list
        (list->cseq (list-sort less? (seq->list s))))))

;; identical?: reference identity (Clojure ==). eq? gives pointer identity over
;; the value model — interned keywords/fixnums/nil compare equal, distinct
;; collections do not. Must NOT be value equality: a deftype whose .equals calls
;; (identical? this o) to short-circuit (e.g. core.logic's Substitutions) would
;; otherwise recur forever (identical? -> = -> equiv -> .equals -> identical?).
(define (jolt-identical? a b) (eq? a b))

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
(def-var! "clojure.core" "take-while" jolt-take-while)
(def-var! "clojure.core" "drop-while" jolt-drop-while)
(def-var! "clojure.core" "partition" jolt-partition)
(def-var! "clojure.core" "sort" jolt-sort)
(def-var! "clojure.core" "identical?" jolt-identical?)

;; rseq: vectors + sorted colls only (Clojure), the reverse of the ascending seq.
(define (jolt-rseq coll)
  (cond
    ((or (pvec? coll) (htable-sorted? coll))
     (list->cseq (reverse (seq->list (jolt-seq coll)))))
    ;; a deftype/record implementing clojure.lang.Reversible (rseq) — e.g.
    ;; data.priority-map — drives rseq through its own method.
    ((and (jrec? coll) (find-method-any-protocol (jrec-tag coll) "rseq"))
     => (lambda (f) (jolt-invoke f coll)))
    (else (jolt-throw (jolt-ex-info "rseq requires a vector or sorted collection" (jolt-hash-map))))))
(def-var! "clojure.core" "rseq" jolt-rseq)

;; clojure.core/unchecked-* — host-defined wrapping (Java long) arithmetic from
;; seq.ss. def-var!'d here because def-var! isn't bound when seq.ss loads.
(let ((d! (lambda (n v) (def-var! "clojure.core" n v))))
  (d! "unchecked-add" jolt-unchecked-add)        (d! "unchecked-add-int" jolt-unchecked-add)
  (d! "unchecked-subtract" jolt-unchecked-sub)   (d! "unchecked-subtract-int" jolt-unchecked-sub)
  (d! "unchecked-multiply" jolt-unchecked-mul)   (d! "unchecked-multiply-int" jolt-unchecked-mul)
  (d! "unchecked-negate" jolt-uncneg)            (d! "unchecked-negate-int" jolt-uncneg)
  (d! "unchecked-inc" jolt-uncinc)               (d! "unchecked-inc-int" jolt-uncinc)
  (d! "unchecked-dec" jolt-uncdec)               (d! "unchecked-dec-int" jolt-uncdec)
  (d! "unchecked-divide-int" jolt-unchecked-div) (d! "unchecked-remainder-int" jolt-unchecked-rem))
