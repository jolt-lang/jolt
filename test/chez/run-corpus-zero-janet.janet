# Phase 3 inc7 (jolt-qjr0) — FULL corpus on the ZERO-JANET spine.
#
# run-corpus-prelude.janet measures RUNTIME parity: it analyzes each case with the
# JANET-hosted analyzer (the oracle) and runs the emitted Scheme on Chez. This
# runner closes the last gap: it analyzes each case with the CHEZ-HOSTED analyzer
# (jolt.analyzer cross-compiled to Scheme, run on Chez over host-contract.ss) —
# read -> analyze -> IR -> emit -> eval, NO Janet in the loop (eval-zero-janet).
#
# So this is the real test of self-hosting: where run-corpus-prelude proves the
# RUNTIME is faithful, this proves the COMPILER-on-Chez is faithful. A case that
# the Janet analyzer compiles but the Chez analyzer can't surfaces here as a crash
# (analyzer/emitter raised) or a divergence (ran, wrong value). The buckets form
# the inc7 punch-list; genuinely host-coupled cases (Java interop, runtime eval)
# are deferred to Phase 4 / jolt-r8ku and allowlisted, like the prelude gate.
#
#   JOLT_CHEZ_ZEROJANET_CORPUS=1 janet test/chez/run-corpus-zero-janet.janet
#   JOLT_CORPUS_LIMIT=200 …    (every-Nth stride, fast iteration)
(import ../../host/chez/driver :as d)
(import ../../host/chez/jolt-chez :as jc)

(unless (os/getenv "JOLT_CHEZ_ZEROJANET_CORPUS")
  (print "skip: set JOLT_CHEZ_ZEROJANET_CORPUS=1 to run the zero-Janet corpus gate")
  (os/exit 0))
(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(def ctx (d/make-ctx))
(def prelude-path (jc/ensure-prelude ctx))

# Compiler image (jolt.ir + jolt.analyzer + jolt.backend-scheme cross-compiled to
# Scheme), cached by the same source fingerprint the spine-test uses.
(defn- image-fingerprint []
  (string/slice (string (hash (string/join
    (map slurp ["jolt-core/jolt/ir.clj" "jolt-core/jolt/analyzer.clj"
                "jolt-core/jolt/backend_scheme.clj" "host/chez/host-contract.ss"
                "host/chez/compile-eval.ss"])))) 0))
(def image-path
  (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-compiler-image-" (image-fingerprint) ".ss"))
(def t0 (os/clock))
(d/ensure-compiler-image ctx image-path)
(printf "prelude + compiler image ready (%.1fs)" (- (os/clock) t0))
(flush)

(def corpus (parse (slurp "test/chez/corpus.edn")))
(def cases
  (if-let [n (os/getenv "JOLT_CORPUS_LIMIT")]
    (let [stride (max 1 (math/floor (/ (length corpus) (scan-number n))))]
      (seq [i :range [0 (length corpus) stride]] (in corpus i)))
    corpus))

# Known divergences/crashes: cases the Chez-hosted compiler can't yet handle that
# are tracked elsewhere (NOT analyzer-faithfulness bugs). Tolerated so the gate
# fails only on a NEW regression. Keyed by label.
#  - host interop (Java classes / constructors / .method on host types): Phase 4
#    jolt-cf1q.7. Same family the prelude gate buckets as crashes.
#  - eval / load-string / read->eval: the jolt-r8ku tail (runtime compiler entry).
(def known-fail
  # Same deferred set the prelude (oracle) gate allowlists — NOT Chez-analyzer
  # faithfulness bugs, but runtime gaps tracked elsewhere:
  #  - print-method multimethod integration: a user (defmethod print-method ...)
  #    isn't consulted by the Chez printer, so pr-str/prn of an overridden type
  #    uses the built-in form (Phase 2 deferred).
  #  - atom?: (instance? clojure.lang.Atom (atom 0)) — host class not mapped on
  #    Chez (Phase 4 host interop, jolt-cf1q.7).
  @{"defmethod overrides a record, top level" true
    "defmethod fires nested in a map" true
    "defmethod fires through prn" true
    "direct builtin override" true
    "methods table inspectable" true
    "atom override fires nested" true
    "atom?" true
    # (instance? clojure.lang.Atom (atom 1)) — atom class identity isn't mapped on
    # Chez (same Phase-4 host-interop gap as "atom?"). JVM-certifies fine.
    "instance? Atom" true
    # str of an Infinity/NaN INSIDE a collection renders the flonum form, not
    # "Infinity" — the prelude gate allowlists this too (Phase-2 recursive str).
    "inf inside coll" true
    # #?@ reader-conditional splicing into the enclosing seq isn't supported yet
    # (plain #? works); a single niche corpus case.
    "reader cond splice" true})

(var pass 0)
(def crashes @[])      # nonzero chez exit (analyzer/emitter raised, or runtime gap)
(def diverged @[])     # ran, wrong value (a real Chez-compiler divergence)
(def known-hit @[])
(def crash-keys @{})
(defn- bucket [tbl k] (put tbl k (+ 1 (or (get tbl k) 0))))

# Group a chez stderr message into a coarse reason for the punch-list.
(defn- crash-reason [m]
  (def m (string/trim m))
  (cond
    (string/find "unsupported stdlib" m) "emit: unsupported stdlib fn"
    (string/find "unsupported host" m) "emit: unsupported host call"
    (string/find "host-static" m) "emit: host-static"
    (string/find "syntax-quote" m) "form-syntax-quote-lower"
    (string/find "uncompil" m) "analyzer: uncompilable"
    (string/find "Unknown class" m) "runtime: unknown class"
    (string/find "No constructor" m) "runtime: no constructor"
    (string/find "No method" m) "runtime: no method"
    (string/find "not a fn" m) "runtime: not a fn"
    (string/find "not seqable" m) "runtime: not seqable"
    (string/find "not a transient" m) "runtime: not a transient"
    (string/find "integer->char" m) "runtime: integer->char"
    (string/find "non-condition value" m)
      (let [i (string/find "non-condition value" m)]
        (string "raised: " (string/slice m (+ i 20) (min (length m) (+ i 60)))))
    (string/slice m 0 (min 56 (length m)))))

(def t1 (os/clock))
(var throws 0)

# Build the evaluable case list (skip :throws), keyed by index (labels aren't
# unique across suites). idx -> row, idx -> "(= EXPECTED ACTUAL)".
(def rows-by-idx @{})
(def pairs @[])
(eachp [i row] cases
  (def {:expected e :actual a} row)
  (if (= e :throws)
    (++ throws)
    (let [key (string i)]
      (put rows-by-idx key row)
      (array/push pairs [key (string "(= " e " " a ")")]))))

(defn- handle [key verdict]
  (def row (get rows-by-idx key))
  (def l (get row :label))
  (case (first verdict)
    :pass (++ pass)
    :crash (let [k (crash-reason (get verdict 1))] (bucket crash-keys k) (array/push crashes [l k]))
    :diverge (if (known-fail l) (array/push known-hit l)
               (array/push diverged [l (string "got " (get verdict 1))]))))

(if (os/getenv "JOLT_ZJ_PERCASE")
  # slow per-case path (each case its own chez process) — for isolating a hang/crash
  (each [key src] pairs
    (def [code out err] (d/eval-zero-janet prelude-path image-path src))
    (handle key (cond (not= code 0) [:crash err] (= out "true") [:pass] [:diverge out])))
  # fast batched path: one chez process loads the runtime once, runs all cases
  (let [{:results r :code c :stderr se :count n} (d/eval-corpus-zero-janet prelude-path image-path pairs)]
    (when (< n (length pairs))
      (printf "WARNING: batched runner returned %d/%d results (chez exit %d): %s"
              n (length pairs) c (string/slice se 0 (min 200 (length se)))))
    (each [key _] pairs
      (handle key (or (get r key) [:crash (string "no result (batch aborted) " se)])))))

(def n-eval (+ pass (length crashes) (length diverged) (length known-hit)))
(printf "\nZero-Janet corpus parity: %d/%d evaluated cases pass  (%.1fs)" pass n-eval (- (os/clock) t1))
(printf "  crash: %d   NEW divergence: %d   known: %d   (throws skipped: %d)"
        (length crashes) (length diverged) (length known-hit) throws)

(defn- report [title tbl]
  (when (> (length tbl) 0)
    (printf "\n%s:" title)
    (each k (sort-by (fn [k] (- (get tbl k))) (keys tbl))
      (printf "  %4d x  %s" (get tbl k) k))))
(report "crash reasons" crash-keys)
(when (> (length diverged) 0)
  (printf "\nNEW divergences (ran, wrong value) — gate FAILS:")
  (each [l m] (slice diverged 0 (min 40 (length diverged)))
    (printf "  [%s] %s" l m)))
(when (> (length known-hit) 0)
  (printf "\n%d known (allowlisted) failures tolerated." (length known-hit)))
(flush)

# Regression floor: raise as the Chez-hosted compiler closes gaps. The gate fails
# on any NEW divergence or if pass drops below the floor. Strided runs scale to 0.
(def base-floor (scan-number (or (os/getenv "JOLT_CHEZ_ZJ_FLOOR") "2544")))
(def floor (if (os/getenv "JOLT_CORPUS_LIMIT") 0 base-floor))
(when (or (> (length diverged) 0) (< pass floor))
  (printf "REGRESSION: pass %d < floor %d or %d new divergence(s)" pass floor (length diverged)))
(os/exit (if (or (> (length diverged) 0) (< pass floor)) 1 0))
