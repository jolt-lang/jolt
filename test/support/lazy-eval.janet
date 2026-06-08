# Worker: evaluate a Clojure equality check in a fresh Jolt ctx and print
# @@RESULT true or @@RESULT false. Used by lazy-infinite-test under a wall-clock
# deadline so infinite-seq hangs are caught as test failures.
(use ../../src/jolt/api)
(use ../../src/jolt/reader)

(def expected (get (dyn :args) 1))
(def actual   (get (dyn :args) 2))

(when (and expected actual)
  (def ctx (init {}))
  (def prog (string "(= " expected " " actual ")"))
  (def [ok val] (protect (eval-string ctx prog)))
  (if ok
    (printf "@@RESULT %q" val)
    (printf "@@ERROR %q" val)))
