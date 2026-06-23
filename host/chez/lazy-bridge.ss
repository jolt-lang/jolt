;; lazy-seq bridge — make-lazy-seq / coll->cells.
;;
;; The `lazy-seq` macro (00-syntax.clj) expands to
;;   (make-lazy-seq (fn* [] (coll->cells (do body))))
;; and `lazy-cat` to (concat (lazy-seq c) ...). These back every overlay fn
;; built on lazy-seq — repeat / iterate / cycle / dedupe / take-nth / keep /
;; interpose / reductions / tree-seq (-> flatten) / lazy-cat.
;;
;; Bridge to the cseq model (seq.ss): a `jolt-lazyseq` is a deferred seq — a 0-arg
;; thunk that, when forced once, yields a seq (cseq | nil). coll->cells coerces the
;; body result to a seq (= jolt-seq), so the thunk already returns a seq; jolt-seq
;; is extended to force a lazyseq. The one trap: (cons x (a-lazy-seq)) must NOT
;; force the tail (else (repeat x) = (lazy-seq (cons x (repeat x))) loops forever),
;; so jolt-cons defers a lazyseq tail into a lazy cseq cell.
;;
;; Loaded LAST (after host-table.ss): %ls-seq then captures the fully-extended
;; jolt-seq (sorted-aware), so a lazy body returning a sorted coll still seqs.

(define-record-type jolt-lazyseq
  (fields (mutable thunk) (mutable val) (mutable realized?))
  (nongenerative jolt-lazyseq-v1))

(define (jolt-make-lazy-seq thunk) (make-jolt-lazyseq thunk jolt-nil #f))

;; force once and memoize. The thunk is (fn [] (coll->cells body)); coll->cells
;; already coerced the body to a seq (cseq | nil) via the live jolt-seq, so the
;; result needs no further coercion (a nested lazyseq was forced by coll->cells).
(define (force-lazyseq x)
  (if (jolt-lazyseq-realized? x)
      (jolt-lazyseq-val x)
      (let ((r (jolt-invoke (jolt-lazyseq-thunk x))))
        (jolt-lazyseq-val-set! x r)
        (jolt-lazyseq-realized?-set! x #t)
        (jolt-lazyseq-thunk-set! x #f)
        r)))

;; coll->cells: coerce the body result to the cell representation = a seq | nil.
(define (jolt-coll->cells c) (jolt-seq c))

;; extend jolt-seq to force a lazyseq (a lazyseq is seqable -> its realized seq).
(define %ls-seq jolt-seq)
(set! jolt-seq (lambda (x) (if (jolt-lazyseq? x) (force-lazyseq x) (%ls-seq x))))

;; (cons x lazyseq): keep the tail lazy — force it only when the cseq cell is
;; walked, so an infinite (repeat/iterate/cycle) stays productive.
(define %ls-cons jolt-cons)
(set! jolt-cons (lambda (x coll)
  (if (jolt-lazyseq? coll)
      (cseq-lazy x (lambda () (force-lazyseq coll)))
      (%ls-cons x coll))))

;; A lazyseq is a NEW value type, so the dispatchers that DON'T route through
;; jolt-seq must learn it or a raw (unrealized) lazyseq escapes — e.g. the corpus
;; compares (= [1 3 5] (take-nth 2 …)) against the raw lazyseq, and jolt=2 would
;; see an unknown type and return false. Recognizing it as sequential is enough
;; for equality + hash (seq=? / seq-hash coerce via jolt-seq); count / empty? /
;; nth / the printers don't, so coerce those explicitly.
(define %ls-sequential? jolt-sequential?)
(set! jolt-sequential? (lambda (x) (or (jolt-lazyseq? x) (%ls-sequential? x))))
(define %ls-count jolt-count)
(set! jolt-count (lambda (x) (if (jolt-lazyseq? x) (%ls-count (jolt-seq x)) (%ls-count x))))
(define %ls-empty? jolt-empty?)
(set! jolt-empty? (lambda (x) (if (jolt-lazyseq? x) (%ls-empty? (jolt-seq x)) (%ls-empty? x))))
(define %ls-nth jolt-nth)
(set! jolt-nth (case-lambda
  ((coll i)   (if (jolt-lazyseq? coll) (%ls-nth (jolt-seq coll) i)   (%ls-nth coll i)))
  ((coll i d) (if (jolt-lazyseq? coll) (%ls-nth (jolt-seq coll) i d) (%ls-nth coll i d)))))
(define %ls-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (jolt-lazyseq? x) (%ls-pr-str (jolt-seq x)) (%ls-pr-str x))))
(define %ls-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (jolt-lazyseq? x) (%ls-pr-readable (jolt-seq x)) (%ls-pr-readable x))))
(register-str-render! jolt-lazyseq? (lambda (x) (jolt-str-render-one (jolt-seq x))))

;; seq? — a lazy seq IS a seq (predicates.ss's jolt-seq? predates the lazyseq
;; record). Unlike the native-op dispatchers above (called via a direct top-level
;; reference, so the set! is enough), seq? is reached through var-deref, which
;; reads the var-cell root — so the patched closure must be re-def-var!'d, not just
;; set!. (Exposed once dynamic binding let with-in-str/line-seq reach seq?.)
(define %ls-seq? jolt-seq?)
(set! jolt-seq? (lambda (x) (or (jolt-lazyseq? x) (%ls-seq? x))))
(def-var! "clojure.core" "seq?" jolt-seq?)

(def-var! "clojure.core" "make-lazy-seq" jolt-make-lazy-seq)
(def-var! "clojure.core" "coll->cells" jolt-coll->cells)
