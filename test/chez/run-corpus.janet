# Phase 0b — host-neutral parity runner.
#
# Feeds each corpus case to a TARGET jolt binary as a fresh subprocess (fresh
# ctx = the per-case isolation the in-process harness gives), checking
# (= expected actual) prints `true`, or that a :throws case exits non-zero. The
# target is pluggable so the SAME corpus gates every host:
#   JOLT_BIN=build/jolt           janet test/chez/run-corpus.janet   # Janet ref
#   JOLT_BIN=build/jolt-chez ...  (Phase 1+)                         # Chez host
# Env: JOLT_CORPUS_LIMIT=N caps the run (every-Nth stride) for fast iteration.
(def corpus (parse (slurp "test/chez/corpus.edn")))
(def jolt-bin (or (os/getenv "JOLT_BIN") "build/jolt"))
(def known (let [k @{}] (each l (parse (slurp "test/chez/known-divergences.edn")) (put k l true)) k))

(def cases
  (if-let [n (os/getenv "JOLT_CORPUS_LIMIT")]
    (let [stride (max 1 (math/floor (/ (length corpus) (scan-number n))))]
      (seq [i :range [0 (length corpus) stride]] (in corpus i)))
    corpus))

(defn run-capture [args]
  (def proc (os/spawn [jolt-bin ;args] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (ev/read (proc :err) 0x100000)
  (def code (os/proc-wait proc))
  # take the LAST non-empty line: a case may print side effects before its value
  (def lines (filter (fn [l] (not (empty? l))) (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines))])

(var pass 0)
(def fails @[])          # NEW divergences — these fail the gate
(def known-hit @[])      # expected (allowlisted) divergences
(defn record-fail [l m]
  (if (known l) (array/push known-hit l) (array/push fails [l m])))
(each row cases
  (def {:expected e :actual a :label l} row)
  (if (= e :throws)
    (let [[code _] (run-capture ["-e" a])]
      (if (not= code 0) (++ pass)
        (record-fail l "expected an error, exited 0")))
    (let [[code out] (run-capture ["-e" (string "(= " e " " a ")")])]
      (cond
        (not= code 0) (record-fail l (string "errored (exit " code ")"))
        (= out "true") (++ pass)
        (record-fail l (string "want true, got " out))))))

(printf "\ncorpus parity [%s]: %d/%d passed  (%d known divergences)"
        jolt-bin pass (length cases) (length known-hit))
(when (> (length fails) 0)
  (printf "%d NEW divergence(s) — gate FAILED:" (length fails))
  (each [l m] (slice fails 0 (min 20 (length fails)))
    (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (> (length fails) 0) 1 0))
