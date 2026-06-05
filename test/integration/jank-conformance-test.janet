# jank conformance: runs the Clojure-language pass-tests from a local jank
# checkout (https://github.com/jank-lang/jank, MPL-2.0) against Jolt and asserts
# the number that pass stays at/above a baseline. This does NOT vendor jank's
# sources (license differs) — it references ~/src/jank if present and SKIPS
# cleanly when absent, so it is a local/dev regression aid.
#
# Each jank pass-test is an assertion script ending in `:success`. We load it in
# a fresh context and count it as passing when it returns :success.
(use ../../src/jolt/api)

(def jank-dir (string (os/getenv "HOME") "/src/jank/compiler+runtime/test/jank"))

# Baseline: the number of pass-tests Jolt currently handles. Raise this as Jolt
# gains features so regressions (a previously-passing test breaking) are caught.
(def baseline 120)

# Tests that loop forever under Jolt's eager evaluation (skipped to avoid hangs;
# tracked as known gaps — variadic-recur arity selection and var-quote calls).
(def skip-patterns ["/cpp/" "var-quote" "fn/recur/pass-variadic-position"])

(defn- skip? [path]
  (var s false)
  (each p skip-patterns (when (string/find p path) (set s true)))
  s)

(defn- walk [dir acc]
  (each e (os/dir dir)
    (def p (string dir "/" e))
    (case ((os/stat p) :mode)
      :directory (walk p acc)
      :file (when (and (string/has-suffix? ".jank" p)
                       (string/find "/pass-" p)
                       (not (skip? p)))
              (array/push acc p))))
  acc)

(if (not (os/stat jank-dir))
  (print "jank-conformance: ~/src/jank not present — skipped")
  (do
    (def files (sort (walk jank-dir @[])))
    (var pass 0)
    (def fails @[])
    (each f files
      # A pass-test passes when no assertion throws (assert now errors on failure).
      (def res (protect (load-string (init) (slurp f))))
      (if (= (res 0) true)
        (++ pass)
        (array/push fails (string/slice f (+ 1 (length jank-dir))))))
    (printf "jank-conformance: %d/%d pass-tests pass (baseline %d)" pass (length files) baseline)
    (when (< pass baseline)
      (print "--- regressions (now failing, were within baseline) ---")
      (each rel fails (print "  " rel))
      (error (string "jank conformance dropped to " pass " (baseline " baseline ")")))
    (printf "jank conformance OK (%d known gaps)" (length fails))))
