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
  (fields (mutable thunk) (mutable val) (mutable realized?) (mutable error?) (mutable lock))
  (nongenerative jolt-lazyseq-v2))

;; Thread-safety for lazy realization is only needed once a second OS thread can
;; touch a shared, not-yet-realized node. In single-threaded programs — all of ys
;; and the overwhelming majority of code — a lazy node needs neither a per-node
;; mutex (allocated eagerly) nor a lock on force. Because iterate/repeat/cycle and
;; every map/filter chunk tail is a lazy node, paying a mutex alloc + acquire per
;; node (per element for iterate) was the dominant cost of idiomatic seq pipelines.
;;
;; `jolt-mt?` starts #f and flips to #t the first time a real OS thread is spawned
;; (fork-thread is shadowed below). This is race-free: a single thread is either
;; forking or forcing, never both, so no node is being realized on the lock-free
;; path at the instant the flag turns on; and fork-thread establishes happens-
;; before, so the spawned child observes the flip. Once multi-threaded, force takes
;; a per-node mutex created lazily under a shared init lock, restoring the original
;; double-checked-locking behavior.
(define jolt-mt? #f)
(define (jolt-mark-mt!) (set! jolt-mt? #t))

;; guards lazy creation of a node's mutex on the multi-threaded path
(define jolt-lazyseq-lock-init (make-mutex))
(define (jolt-lazyseq-ensure-lock! x)
  (with-mutex jolt-lazyseq-lock-init
    (or (jolt-lazyseq-lock x)
        (let ((m (make-mutex))) (jolt-lazyseq-lock-set! x m) m))))

(define (jolt-make-lazy-seq thunk) (make-jolt-lazyseq thunk jolt-nil #f #f #f))

;; force once and memoize. The thunk is (fn [] (coll->cells body)); coll->cells
;; already coerced the body to a seq (cseq | nil) via the live jolt-seq, so the
;; result needs no further coercion (a nested lazyseq was forced by coll->cells).
;; A thrown failure is cached and re-raised on every later force, like the JVM (the
;; body runs exactly once; a failed force rethrows). The captured Chez condition is
;; re-raised verbatim, so a downstream catch unwraps the original jolt value.
(define (force-lazyseq x)
  (define (deliver)
    (if (jolt-lazyseq-error? x) (raise (jolt-lazyseq-val x)) (jolt-lazyseq-val x)))
  (define (run!)
    (guard (e (#t
               (jolt-lazyseq-val-set! x e)
               (jolt-lazyseq-error?-set! x #t)
               (jolt-lazyseq-realized?-set! x #t)
               (jolt-lazyseq-thunk-set! x #f)
               (raise e)))
      (let ((r (jolt-invoke (jolt-lazyseq-thunk x))))
        (jolt-lazyseq-val-set! x r)
        (jolt-lazyseq-realized?-set! x #t)
        (jolt-lazyseq-thunk-set! x #f)
        r)))
  ;; Single-threaded: no lock, and the fast realized? read is safe. Once a second
  ;; thread exists, the fast read is NOT safe: run! stores val then realized? with
  ;; no barrier between them, so on a weak memory model (ARM64) a lock-free reader
  ;; can see realized?#t while val is still the thunk and leak a closure out as a
  ;; seq. So on the multi-threaded path every access — reads included — goes through
  ;; the per-node mutex, whose acquire/release order the writer and reader against.
  (cond
    ((not jolt-mt?) (if (jolt-lazyseq-realized? x) (deliver) (run!)))
    (else                                          ; multi-threaded: always lock
     (with-mutex (jolt-lazyseq-ensure-lock! x)     ; locking on a lazily-made mutex
       (if (jolt-lazyseq-realized? x) (deliver) (run!))))))

;; Shadow fork-thread so any spawn (future/agent/core.async/process, all loaded
;; after this file) flips jolt-mt? on. Captured in a prior define so the RHS sees
;; the primitive, not the top-level binding being defined (Chez top-level letrec*).
(define %ls-orig-fork-thread fork-thread)
(define (fork-thread thunk) (jolt-mark-mt!) (%ls-orig-fork-thread thunk))

;; coll->cells: coerce the body result to the cell representation = a seq | nil.
(define (jolt-coll->cells c) (jolt-seq c))

;; extend jolt-seq to force a lazyseq (a lazyseq is seqable -> its realized seq).
(register-seq-arm! jolt-lazyseq? force-lazyseq)

;; (cons x lazyseq): keep the tail lazy — force it only when the cseq cell is
;; walked, so an infinite (repeat/iterate/cycle) stays productive.
(define %ls-cons jolt-cons)
(set! jolt-cons (lambda (x coll)
  (if (jolt-lazyseq? coll)
      (cseq-lazy x (lambda () (force-lazyseq coll)))
      (%ls-cons x coll))))

;; (conj lazyseq x): conj onto a seq prepends, like any seq — (conj (rest xs) y).
;; rest returns a lazyseq, so this is a common path; without it conj reports the
;; lazyseq as an "unsupported collection".
(register-conj-arm! jolt-lazyseq? (lambda (coll x) (jolt-cons x coll)))

;; A lazyseq is a NEW value type, so the dispatchers that DON'T route through
;; jolt-seq must learn it or a raw (unrealized) lazyseq escapes — e.g. the corpus
;; compares (= [1 3 5] (take-nth 2 …)) against the raw lazyseq, and jolt=2 would
;; see an unknown type and return false. Recognizing it as sequential is enough
;; for equality + hash (seq=? / seq-hash coerce via jolt-seq); count / empty? /
;; nth / the printers don't, so coerce those explicitly.
(define %ls-sequential? jolt-sequential?)
(set! jolt-sequential? (lambda (x) (or (jolt-lazyseq? x) (%ls-sequential? x))))
(register-count-arm! jolt-lazyseq?
  (lambda (x) (jolt-count (jolt-seq x))))
(register-empty-arm! jolt-lazyseq? (lambda (x) (jolt-empty? (jolt-seq x))))
(define %ls-nth jolt-nth)
(set! jolt-nth (case-lambda
  ((coll i)   (if (jolt-lazyseq? coll) (%ls-nth (jolt-seq coll) i)   (%ls-nth coll i)))
  ((coll i d) (if (jolt-lazyseq? coll) (%ls-nth (jolt-seq coll) i d) (%ls-nth coll i d)))))
;; a lazy seq prints as its realized seq — force, then re-dispatch through the
;; printer. An empty realized lazy seq is still a sequence, printing "()" (like a
;; JVM LazySeq), not "nil" — so (lazy-seq nil) and (rest '(1)) render "()".
(register-pr-str-arm! jolt-lazyseq?
  (lambda (x) (let ((s (jolt-seq x))) (if (jolt-nil? s) "()" (jolt-pr-str s)))))
(register-pr-readable-arm! jolt-lazyseq?
  (lambda (x) (let ((s (jolt-seq x))) (if (jolt-nil? s) "()" (jolt-pr-readable s)))))
(register-str-render! jolt-lazyseq?
  (lambda (x) (let ((s (jolt-seq x))) (if (jolt-nil? s) "()" (jolt-str-render-one s)))))

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
