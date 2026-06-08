# clojure-test-suite conformance: runs the external, cross-dialect
# clojure-test-suite (https://github.com/lread/clojure-test-suite, EPL) against
# Jolt and asserts the number of passing per-function test files stays at/above
# a baseline. Like the jank battery, this does NOT vendor the suite — it
# references ~/src/clojure-test-suite if present and SKIPS cleanly when absent.
#
# Each suite file is a `clojure.test` namespace (one per clojure.core/string
# function). A minimal clojure.test + portability shim (test/support/clojure_test.clj)
# lets Jolt load them; `when-var-exists` auto-skips fns Jolt doesn't implement.
#
# Files are run in a one-shot worker subprocess (test/integration/suite-worker.janet)
# under a wall-clock deadline. Some suite tests build infinite sequences
# (cycle/range/transducers-over-infinite) that Jolt's eager evaluator can't
# truncate and so HANG rather than fail; the deadline contains them — a timed-out
# file is reported as :timeout and contributes nothing, no manual skip-list needed.

(def suite-dir (string (os/getenv "HOME") "/src/clojure-test-suite/test/clojure"))

# Baseline: assertions Jolt currently passes across the suite. Raise as Jolt
# improves so a regression (previously-passing assertion breaking) is caught.
# Lowered 3915 -> 3913 when futures landed: `realized?`/realized_qmark.cljc has a
# `(when-var-exists future ...)` block that was skipped while `future` was
# unresolved. With futures implemented the block now runs, but it depends on JVM
# `Thread/sleep` (jolt has no JVM interop) and on `future-cancel` interrupting a
# running thread (Janet OS threads can't be interrupted), so `(deref (future
# (sleep 1)))` re-raises the unresolved-`Thread/sleep` error — a documented
# platform gap, not a regression in any previously-working behavior.
# Raised 3913 -> 3916 with the staged-bootstrap kernel tier (ns-restore-on-throw
# + faithful subvec coercion), then 3916 -> 3919 moving juxt/every-pred/some-fn to
# Clojure (the canonical defs are more correct than the prior Janet ones). Raised
# 3919 -> 3926 preserving nil map values (jolt-c7h): a nil value is a present key,
# which several suite tests assert. Runs read 3927 consistently, occasionally 3926
# when a timeout-prone test (of the 9 that can time out) doesn't finish; floor at
# the consistent-minus-one 3926.
(def baseline-pass 3971)
# A file is "clean" when it ran with zero failures AND zero errors.
(def baseline-clean-files 45)
# Per-file wall-clock budget (seconds). Normal files finish in well under 1s;
# this only fires on infinite-sequence hangs.
(def per-file-timeout 6)

(defn- walk [dir acc]
  (each e (os/dir dir)
    (def p (string dir "/" e))
    (case ((os/stat p) :mode)
      :directory (walk p acc)
      :file (when (and (string/has-suffix? ".cljc" p)
                       (not (string/has-suffix? "portability.cljc" p)))
              (array/push acc p))))
  acc)

# Run one file in a worker subprocess; return its "pass fail error" stdout, or
# nil if it exceeded the deadline (hang) or crashed.
(defn- run-file [path]
  (def proc (os/spawn ["janet" "test/integration/suite-worker.janet" path] :p {:out :pipe}))
  (def out (proc :out))
  (var data nil)
  (def ok
    (try
      (ev/with-deadline per-file-timeout
        (set data (ev/read out 0x10000))   # workers print a single short line
        (os/proc-wait proc)
        true)
      ([err] false)))
  (when (not ok)
    (protect (os/proc-kill proc true))
    (protect (ev/with-deadline 2 (os/proc-wait proc))))
  (protect (:close out))
  (if (and ok data) (string data) nil))

(defn- parse-counts [s]
  # Find the "@@COUNTS p f e" sentinel line (a test body may have printed other
  # lines to stdout, e.g. with-out-str tests).
  (var result nil)
  (each line (string/split "\n" s)
    (when (string/has-prefix? "@@COUNTS " line)
      (let [parts (string/split " " (string/trim line))]
        (when (= 4 (length parts))
          (set result [(scan-number (parts 1)) (scan-number (parts 2)) (scan-number (parts 3))])))))
  result)

(if (not (os/stat suite-dir))
    (print "clojure-test-suite: ~/src/clojure-test-suite not present — skipped")
    (do
      (def progress? (os/getenv "SUITE_PROGRESS"))
      (def files (sort (walk suite-dir @[])))
      (var total-pass 0)
      (var total-fail 0)
      (var total-error 0)
      (var clean-files 0)
      (var ran-files 0)
      (var timeouts 0)
      (def worst @[])

      (each path files
        (def rel (string/slice path (+ 1 (length suite-dir))))
        (when progress? (eprintf "  %s" rel) (eflush))
        (def out (run-file path))
        (def counts (and out (parse-counts out)))
        (cond
          (nil? out) (do (++ timeouts) (when progress? (eprint " TIMEOUT")))
          (nil? counts) (when progress? (eprint " (no counts)"))
          (let [[pn fn* en] counts]
            (++ ran-files)
            (+= total-pass pn)
            (+= total-fail fn*)
            (+= total-error en)
            (when (and (= 0 fn*) (= 0 en) (> pn 0)) (++ clean-files))
            (when (> (+ fn* en) 0) (array/push worst [(+ fn* en) rel pn fn* en])))))

      (def total (+ total-pass total-fail total-error))
      (printf "\nclojure-test-suite: %d files ran (%d timed out), %d assertions — %d pass / %d fail / %d error"
              ran-files timeouts total total-pass total-fail total-error)
      (printf "\n  clean files (0 fail/error, >0 pass): %d" clean-files)
      (sort-by (fn [x] (- (x 0))) worst)
      (when (> (length worst) 0)
        (print "  top files by fail+error:")
        (each w (slice worst 0 (min 15 (length worst)))
          (printf "    %-40s pass=%d fail=%d err=%d" (w 1) (w 2) (w 3) (w 4))))

      (assert (>= total-pass baseline-pass)
              (string/format "regression: total-pass %d < baseline %d" total-pass baseline-pass))
      (assert (>= clean-files baseline-clean-files)
              (string/format "regression: clean-files %d < baseline %d" clean-files baseline-clean-files))
      (printf "\nclojure-test-suite: OK (>= %d pass, >= %d clean files)\n" baseline-pass baseline-clean-files)))
