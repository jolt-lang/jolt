# Phase 1 (jolt-cf1q.2) — FIRST parity number for the Chez back end.
#
# The full 0b gate (test/chez/run-corpus.janet) drives an `-e`-capable jolt
# binary; that needs all of clojure.core bootstrapped onto Chez, which is Phase 2.
# Until then, this probe reports parity for the subset the back end can ALREADY
# compile: each corpus case `(= expected actual)` is run through the live
# analyzer -> Scheme emitter -> Chez. Cases that reference unimplemented core fns
# fail to EMIT (a clean compile-time signal) and are counted "out of subset",
# not as divergences. The number to watch is parity WITHIN the compiled subset.
#   janet test/chez/run-corpus-chez.janet
#   JOLT_CORPUS_LIMIT=400 …    (every-Nth stride, fast)
(import ../../host/chez/driver :as d)

# Slow reporting tool (~20s: a Chez subprocess per compiled case), not a pass/fail
# unit test — gate it out of the default suite like the benches (JOLT_BENCH).
(unless (os/getenv "JOLT_CHEZ_CORPUS")
  (print "skip: set JOLT_CHEZ_CORPUS=1 to run the Chez subset parity probe")
  (os/exit 0))
(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(def corpus (parse (slurp "test/chez/corpus.edn")))
(def cases
  (if-let [n (os/getenv "JOLT_CORPUS_LIMIT")]
    (let [stride (max 1 (math/floor (/ (length corpus) (scan-number n))))]
      (seq [i :range [0 (length corpus) stride]] (in corpus i)))
    corpus))

# Known subset divergences: cases that compile but need a feature beyond the
# current increment. Dynamic IFn dispatch — a keyword/vector held in a LOCAL or
# var then called as a fn ((let [k :a] (k m))) — is runtime dispatch on the
# invoke mechanism, deferred to the IFn/protocol increment. The STATIC literal
# forms ((:a m), ({:a 1} :a)) ARE supported. Allowlisted by label; the gate fails
# only on a NEW divergence.
(def known-divergences
  {"param holding a keyword (IFn leftover)" true
   "vector-in-local as fn" true
   "keyword-in-local as fn" true})

(def ctx (d/make-ctx))
(var compiled 0) (var pass 0) (var out-of-subset 0)
(def diverged @[])
(def known-hit @[])

(each row cases
  (def {:expected e :actual a :label l} row)
  # :throws cases need error-semantics we don't model yet — skip.
  (if (= e :throws)
    (++ out-of-subset)
    (let [src (string "(= " e " " a ")")
          # compile-program can throw (unsupported op/core fn) or the analyzer can
          # punt; either way the case is outside the compilable subset.
          res (try (d/run-on-chez ctx src) ([err] :uncompilable))]
      (if (= res :uncompilable)
        (++ out-of-subset)
        (let [[code out] res]
          (++ compiled)
          (defn record-div [m] (if (known-divergences l) (array/push known-hit l) (array/push diverged [l m])))
          (cond
            (not= code 0) (record-div (string "exit " code))
            (= out "true") (++ pass)
            (record-div (string "got " out))))))))

(printf "\nChez subset parity: %d/%d compiled cases pass  (%d/%d out of subset, %d known divergences)"
        pass compiled out-of-subset (length cases) (length known-hit))
(when (> (length diverged) 0)
  (printf "%d NEW divergence(s) within the compiled subset:" (length diverged))
  (each [l m] (slice diverged 0 (min 25 (length diverged)))
    (printf "  [%s] %s" l m)))
(flush)
(os/exit (if (> (length diverged) 0) 1 0))
