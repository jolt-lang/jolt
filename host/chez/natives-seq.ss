;; seq-native shims (jolt-y6mv) — seed-native seq fns the overlay assumes are
;; clojure.core natives but which live in the Janet seed (src/jolt/core_coll.janet),
;; so they have no def-var! in the assembled prelude and resolve to jolt-nil on
;; Chez. This was the dominant prelude-parity crash bucket ('apply jolt-nil').
;; Each is a pure fn over the existing seq layer (seq.ss) — collection arities
;; only; the 1-arg transducer arities are jolt-kxsr. Loaded last (after
;; converters.ss for jolt-compare and seq.ss for the reduced record).

;; reduced / reduced? — the box itself is the jolt-reduced record from seq.ss
;; (so the reduce machinery there can see it); these just expose the constructor
;; and predicate. (deref a-reduced) is handled in atoms.ss.
(define (jolt-reduced-new x) (make-jolt-reduced x))
(define (jolt-reduced-pred x) (jolt-reduced? x))

;; mapcat: (mapcat f coll & colls) — map f across the colls (stops at shortest),
;; then concat the results. Collection arity only.
(define (jolt-mapcat f . colls)
  (apply jolt-concat (seq->list (apply jolt-map f colls))))

;; take-while / drop-while over the seq layer.
(define (take-while-seq pred s)
  (if (jolt-nil? s) jolt-empty-list
      (let ((x (seq-first s)))
        (if (jolt-truthy? (jolt-invoke pred x))
            (cseq-lazy x (lambda () (take-while-seq pred (jolt-seq (seq-more s)))))
            jolt-empty-list))))
(define (jolt-take-while pred coll) (take-while-seq pred (jolt-seq coll)))
(define (jolt-drop-while pred coll)
  (let loop ((s (jolt-seq coll)))
    (if (and (not (jolt-nil? s)) (jolt-truthy? (jolt-invoke pred (seq-first s))))
        (loop (jolt-seq (seq-more s)))
        (if (jolt-nil? s) jolt-empty-list s))))

;; partition: (partition n coll), (partition n step coll), or
;; (partition n step pad coll). Only complete partitions of size n are kept;
;; with pad, a short final partition is padded from pad (and may be < n if pad
;; runs out). Each partition is a seq; the whole result is a lazy seq of seqs.
(define jolt-partition
  (case-lambda
    ((n coll) (partition* (->idx n) (->idx n) #f #f coll))
    ((n step coll) (partition* (->idx n) (->idx step) #f #f coll))
    ((n step pad coll) (partition* (->idx n) (->idx step) #t pad coll))))
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

;; identical?: jolt reference identity. The seed defines it as (= a b) over its
;; value model (core_types.janet core-identical?), where interned keywords/small
;; values compare equal — so jolt= is the faithful port.
(define (jolt-identical? a b) (jolt= a b))

(def-var! "clojure.core" "reduced" jolt-reduced-new)
(def-var! "clojure.core" "reduced?" jolt-reduced-pred)
(def-var! "clojure.core" "mapcat" jolt-mapcat)
(def-var! "clojure.core" "take-while" jolt-take-while)
(def-var! "clojure.core" "drop-while" jolt-drop-while)
(def-var! "clojure.core" "partition" jolt-partition)
(def-var! "clojure.core" "sort" jolt-sort)
(def-var! "clojure.core" "identical?" jolt-identical?)
