# Conformance inc1 (jolt-xsfe) — gate wrapper for the JVM corpus certifier.
#
# test/conformance/certify.clj evaluates every test/chez/corpus.edn row through
# reference JVM Clojure and checks jolt's hand-written :expected against what real
# Clojure produces. Divergences are classified in test/conformance/known-divergences.edn
# (deliberate jolt-specific / host-model deltas + tracked bugs); the certifier exits
# nonzero only on a NEW (unclassified) divergence or a stale allowlist entry.
#
# This wrapper runs it in the Janet gate and skips cleanly when `clojure` (JVM) is
# not installed — same pattern as the chez tests skipping without `chez`.
#
#   janet test/conformance/certify-test.janet

(defn- have-clojure? []
  (def p (try (os/spawn ["clojure" "--version"] :p {:out :pipe :err :pipe}) ([_] nil)))
  (if (nil? p) false
    (do (def out (ev/read (p :out) :all)) (def err (ev/read (p :err) :all))
        (zero? (os/proc-wait p)))))

(unless (have-clojure?)
  (print "clojure (JVM) not on PATH — skipping corpus certification")
  (os/exit 0))

(def proc (os/spawn ["clojure" "-M" "test/conformance/certify.clj"] :p {:out :pipe :err :pipe}))
(def out (string (ev/read (proc :out) :all)))
(def err (string (or (ev/read (proc :err) :all) "")))
(def code (os/proc-wait proc))

# Echo the summary lines so the gate log shows the certification status.
(each line (string/split "\n" out)
  (when (or (string/find "certif" line) (string/find "allowlist" line)
            (string/find "NEW" line) (string/find "STALE" line) (string/find "DIVERGENT" line))
    (print line)))

(when (not= code 0)
  (eprint "corpus certification FAILED (new/stale divergence vs reference Clojure):")
  (eprint out)
  (unless (= "" err) (eprint err))
  (os/exit 1))

(print "corpus certification: OK (all divergences classified)")
(os/exit 0)
