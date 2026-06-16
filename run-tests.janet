#!/usr/bin/env janet
# Parallel test runner — the gate, faster.
#
# `jpm test` runs every file under test/ serially; this runs the same set across
# a pool of worker processes (each test file is an independent `janet FILE`).
# Build the executable FIRST (`jpm build`) so the binary-spawning tests don't
# skip:  jpm build && janet run-tests.janet
#
# Worker count is the detected CPU count; override with JOLT_TEST_JOBS. Per-file
# stdout+stderr is captured to a temp file and only printed for failures (and,
# with -v / JOLT_TEST_VERBOSE, for everything). Exits non-zero if any file fails
# and prints the literal "All tests passed." on success (CI greps for it).

(defn- detect-jobs []
  (or (when-let [j (os/getenv "JOLT_TEST_JOBS")] (scan-number j))
      (try
        (let [p (os/spawn ["sh" "-c" "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null"]
                          :p {:out :pipe})
              out (:read (p :out) :all)]
          (os/proc-wait p)
          (scan-number (string/trim (or out ""))))
        ([_] nil))
      4))

(defn- find-tests [dir acc]
  (each name (sorted (os/dir dir))
    (def full (string dir "/" name))
    (case (os/stat full :mode)
      # support/ holds shared libraries (harness, shims), not standalone tests —
      # jpm doesn't run them and neither do we.
      :directory (unless (= name "support") (find-tests full acc))
      :file (when (string/has-suffix? ".janet" name) (array/push acc full))))
  acc)

(def verbose? (or (has-value? (dyn :args) "-v") (os/getenv "JOLT_TEST_VERBOSE")))
(def janet-bin (dyn :executable))
(def tmpdir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-test-" (os/getpid)))
(os/mkdir tmpdir)

(def files (find-tests "test" @[]))
(def total (length files))
(def jobs (max 1 (min 16 (detect-jobs))))

(def results @[])
(var done 0)

(defn- run-one [file]
  (def t0 (os/clock))
  (def of (string tmpdir "/" (string/replace-all "/" "_" file) ".out"))
  # let the shell own redirection — avoids pipe-buffer deadlock on chatty tests.
  (def cmd (string/format "exec '%s' '%s' > '%s' 2>&1" janet-bin file of))
  (def rc (os/proc-wait (os/spawn ["sh" "-c" cmd] :p)))
  (def dt (- (os/clock) t0))
  (array/push results {:file file :rc rc :out of :dt dt})
  (++ done)
  (printf "[%d/%d] %s %6.2fs  %s%s" done total (if (zero? rc) "ok  " "FAIL")
          dt file (if (zero? rc) "" (string/format " (rc=%d)" rc)))
  (flush))

# A :stop sentinel per worker signals end-of-queue — ev/chan-close discards
# buffered items (Janet 1.41), so we can't just close and drain.
(def q (ev/chan (+ total jobs)))
(each f files (ev/give q f))
(loop [_ :range [0 jobs]] (ev/give q :stop))

(defn- worker [&]
  (forever
    (def f (ev/take q))
    (if (= f :stop) (break) (run-one f))))

(printf "running %d test files across %d workers...\n" total jobs)
(flush)
(def wall0 (os/clock))
(def super (ev/chan))
(loop [_ :range [0 jobs]] (ev/go worker nil super))
(loop [_ :range [0 jobs]] (ev/take super))
(def wall (- (os/clock) wall0))

(def failed (filter |(not (zero? ($ :rc))) results))
(def slow (->> results (sorted-by |(- ($ :dt))) (take 5)))

(print "\n--- slowest ---")
(each r slow (printf "  %6.2fs  %s" (r :dt) (r :file)))

(when verbose?
  (each r results
    (printf "\n===== %s (rc=%d, %.2fs) =====" (r :file) (r :rc) (r :dt))
    (print (slurp (r :out)))))

(unless (empty? failed)
  (print "\n--- FAILURES ---")
  (each r failed
    (printf "\n===== %s (rc=%d) =====" (r :file) (r :rc))
    (print (slurp (r :out)))))

(printf "\n%d files, %d failed, %.1fs wall (%d workers)"
        total (length failed) wall jobs)
(if (empty? failed)
  (do (print "\nAll tests passed.") (os/exit 0))
  (do (printf "\n%d test files FAILED." (length failed)) (os/exit 1)))
