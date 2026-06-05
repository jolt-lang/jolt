# One-file worker for the clojure-test-suite battery. Loads the clojure.test
# shim, evaluates a single suite .cljc file, runs its deftests, and prints
# "pass fail error" to stdout. Used by the discovery pass to find files that
# hang under Jolt's eager evaluation (run under an external timeout).
(use ../../src/jolt/api)
(use ../../src/jolt/reader)
(use ../../src/jolt/evaluator)

(defn- parse-forms [src]
  (var s src) (def fs @[]) (var go true)
  (while (and go (> (length (string/trim s)) 0))
    (def r (protect (parse-next s)))
    (if (not (r 0)) (set go false)
      (let [p (r 1)] (set s (p 1)) (when (not (nil? (p 0))) (array/push fs (p 0))))))
  fs)

# A helper, not a standalone test: it needs a .cljc path argument. When `jpm
# test` runs it with no args, no-op cleanly so it doesn't count as a failure.
(def path (get (dyn :args) 1))

(when path
  (def ctx (init))
  (each f (parse-forms (slurp "test/support/clojure_test.clj")) (eval-form ctx @{} f))

  # Pre-load the suite's own clojure.core-test.number-range helper ns if present
  # (35 files require it for r/max-int, r/max-double, … — its :default branches are
  # plain numeric literals Jolt can read). Its `ns` form sets the namespace; the
  # test file's own `ns` form switches back afterwards.
  (let [dir (string/slice path 0 (- (length path) (length (last (string/split "/" path)))))
        nr (string dir "number_range.cljc")]
    (when (os/stat nr)
      (each f (parse-forms (slurp nr)) (protect (eval-form ctx @{} f)))))

  (eval-string ctx "(clojure.test/reset-report!)")
  (each form (parse-forms (slurp path)) (protect (eval-form ctx @{} form)))
  (protect (eval-string ctx "(clojure.test/run-registered)"))
  (def p (eval-string ctx "(clojure.test/n-pass)"))
  (def f (eval-string ctx "(clojure.test/n-fail)"))
  (def e (eval-string ctx "(clojure.test/n-error)"))
  # A "dump" 2nd arg (or SUITE_DUMP env) also prints each failure/error message
  # (one DUMP line each) for triage.
  (when (or (os/getenv "SUITE_DUMP") (= "dump" (get (dyn :args) 2)))
    (eval-string ctx "(doseq [m (clojure.test/failures)] (println (str \"DUMP \" m)))"))
  # Counts on a sentinel line so parsers find it even if a test body printed to
  # stdout (e.g. with-out-str / println-str tests).
  (printf "@@COUNTS %d %d %d" (if (number? p) p 0) (if (number? f) f 0) (if (number? e) e 0)))
