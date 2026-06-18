# Phase 1 (jolt-cf1q.2, inc 3j) — FULL parity against the -e-capable jolt-chez.
#
# run-corpus-chez.janet measures parity for the SUBSET the back end can compile
# without any clojure.core present (most cases are "out of subset" because they
# call core fns). This runner closes that gap: it assembles the ENTIRE non-macro
# clojure.core as a Scheme prelude (driver/emit-core-prelude — def-var! forms in
# tier dependency order), loads it before each case, and emits the case in
# prelude mode so every core ref resolves via var-deref. That is exactly the
# -e-capable jolt-chez the milestone calls for, measured in-process (one ctx,
# prelude assembled once) rather than via a spawned binary per case.
#
# With all of core available, "out of subset" collapses to genuine emit failures
# (host interop / unsupported IR). The new signal is RUNTIME parity: a case that
# emits but crashes (a missing/blank runtime prim — a lazy gap) or returns a
# wrong value (a divergence). The report buckets these so the gaps form a
# punch-list for the next increments.
#   JOLT_CHEZ_PRELUDE_CORPUS=1 janet test/chez/run-corpus-prelude.janet
#   JOLT_CORPUS_LIMIT=200 …    (every-Nth stride, fast)
(import ../../host/chez/driver :as d)

(unless (os/getenv "JOLT_CHEZ_PRELUDE_CORPUS")
  (print "skip: set JOLT_CHEZ_PRELUDE_CORPUS=1 to run the prelude parity gate")
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

# Known divergences: cases that emit + run on Chez but yield a non-canonical
# value because of a gap beyond this increment. The gate fails only on a NEW
# (un-allowlisted) divergence — a real Chez correctness regression. Each here is
# a deferred Phase-2 / dynamic-var / eval-order gap, NOT a wrong shim:
#   - class names ((class x), .getName) — no Chez class system yet (Phase 2)
#   - *ns* / *clojure-version* / *unchecked-math* — dynamic vars not bound on Chez
#   - eval-order probes — assert left-to-right side-effect order via host state
#     the emitted Scheme doesn't yet reproduce
#   - close on throw — with-open/finally resource close semantics
(def known-divergences
  {"class name evaluates to canonical string" true
   "dispatch-only class name" true
   "inside class" true
   "values evaluate in source order" true
   "keys evaluate before their values, pairwise" true
   "source order with a nil value (phm form)" true
   "close on throw" true
   # *ns* now a namespace value (jolt-yxqm): str/ns-name of *ns* + the var str
   # case ("ns-name of *ns*" / "str of *ns*" / "*ns* user" / "str of a var") pass.
   # *clojure-version* / *unchecked-math* now bound by dynamic-vars.ss (inc 3r)
   # (str [##Inf]) wants "[Infinity]" but the Chez -e printer (jolt-pr-str, which
   # str falls back to for collections) renders inf as "inf" — str needs a
   # recursive long-form renderer, the Phase-2 canonical printer. Top-level
   # (str ##Inf) -> "Infinity" already works (jolt-str-render-one).
   "inf inside coll" true
   # Phase 2 inc B (jolt-9zhh) made pr-str run; these now render via the readable
   # FALLBACK instead of the seed's print-method / var semantics — separate gaps:
   #   - a custom (defmethod print-method ...) isn't consulted by the Chez printer
   #     (print-method multimethod integration is deferred)
   # (pr-str of a var / defn now PASS — inc I made def return the var #'ns/name.)
   "atom override fires nested" true
   # Phase 2 inc D (jolt-jgoc) made records run; pr-str of a record uses the
   # built-in #ns.Name{...} form, not a user (defmethod print-method 'ns.Name …)
   # — the printer doesn't consult the print-method multimethod yet (deferred).
   "defmethod overrides a record, top level" true
   "defmethod fires nested in a map" true
   "defmethod fires through prn" true
   # var def-time metadata (^:private / ^Type tag / docstring) is now captured on
   # the Chez var-cell (jolt-zikh), so those three cases pass.
   "methods table inspectable" true})

(def ctx (d/make-ctx))

# Assemble the prelude once and write it to a file the per-case programs `load`.
(def t0 (os/clock))
(def [prelude-scm emitted total] (d/emit-core-prelude ctx))
(def prelude-path (string "/tmp/jolt-chez-prelude-" (os/getpid) ".ss"))
(spit prelude-path prelude-scm)
(printf "prelude: %d/%d non-macro core forms emitted (%.1fs, %d bytes) -> %s"
        emitted total (- (os/clock) t0) (length prelude-scm) prelude-path)
(flush)

(var pass 0)
(def emit-errs @[])    # user form can't emit (host interop / unsupported IR)
(def crashes @[])      # emitted, chez exited non-zero (lazy runtime gap or bug)
(def diverged @[])     # emitted + ran, NEW wrong value (fails the gate)
(def known-hit @[])    # emitted + ran, allowlisted wrong value (tolerated)
(def crash-keys @{})   # grouped crash reason -> count
(def emit-keys @{})    # grouped emit-failure reason -> count

(defn- bucket [tbl k] (put tbl k (+ 1 (or (get tbl k) 0))))
(defn- crash-reason [err]
  (def e (string err))
  (if-let [i (string/find "Exception" e)]
    (string/slice e i (min (length e) (+ i 70)))
    (string/slice e 0 (min 60 (length e)))))
(defn- emit-reason [msg]
  (def m (string msg))
  (cond
    (string/find "out of subset" m) (let [i (string/find "`" m)]
                                      (if i (string "core fn: " (string/slice m (inc i) (or (string/find "`" m (inc i)) (length m)))) "out-of-subset"))
    (string/find "host" m) "host-interop"
    (string/find "unhandled op" m) (string/slice m (max 0 (- (length m) 28)))
    (string/slice m 0 (min 48 (length m)))))

(def t1 (os/clock))
(each row cases
  (def {:expected e :actual a :label l} row)
  (if (= e :throws)
    nil  # :throws error-semantics aren't modeled here; skip (counted out of run)
    (let [src (string "(= " e " " a ")")
          res (d/eval-e-with-prelude ctx src prelude-path)]
      (cond
        (= (get res 0) :emit-err)
          (let [k (emit-reason (get res 1))] (bucket emit-keys k) (array/push emit-errs [l k]))
        (not= (get res 0) 0)
          (let [k (crash-reason (get res 2))] (bucket crash-keys k) (array/push crashes [l k]))
        (= (get res 1) "true") (++ pass)
        (known-divergences l) (array/push known-hit l)
        (array/push diverged [l (string "got " (get res 1))])))))

(def n-eval (+ pass (length emit-errs) (length crashes) (length diverged) (length known-hit)))
(printf "\nPrelude parity: %d/%d evaluated cases pass  (%.1fs)" pass n-eval (- (os/clock) t1))
(printf "  emit-fail (out of subset): %d   runtime crash: %d   NEW divergence: %d   known divergence: %d"
        (length emit-errs) (length crashes) (length diverged) (length known-hit))

(defn- report [title tbl]
  (when (> (length tbl) 0)
    (printf "\n%s:" title)
    (each k (sort-by (fn [k] (- (get tbl k))) (keys tbl))
      (printf "  %4d x  %s" (get tbl k) k))))
(report "emit-failure reasons" emit-keys)
(report "runtime-crash reasons" crash-keys)
(when (> (length diverged) 0)
  (printf "\nNEW divergences (emit+ran, wrong value) — gate FAILS:")
  (each [l m] (slice diverged 0 (min 30 (length diverged)))
    (printf "  [%s] %s" l m)))
(when (> (length known-hit) 0)
  (printf "\n%d known (allowlisted) divergences tolerated." (length known-hit)))
(flush)

# Regression floor (inc 3j baseline): raise as runtime gaps close, like the probe
# reach-floor and the suite baseline. The gate fails if parity drops below it, or
# on any NEW (un-allowlisted) divergence — a real Chez correctness regression.
# Full-corpus baseline: inc 3j 1220/2497; 3k (converters) 1326; 3l (transients)
# 1382; 3m (numeric-edge emit + variadic assoc!) 1407; 3n (seq-native shims +
# reduced) 1467; 3o (transducer arities) 1493; 3p (misc seq/regex gaps) 1506;
# 3q (multimethod dispatch + late-bind) 1530; 3r (dynamic-var constants) 1532;
# 3x (non-ASCII string literals, jolt-x0os) + 3y (seed assoc! odd-args -> :throws,
# jolt-ea9k) 1534 (total evaluated drops as the 3 odd-arg rows become :throws).
# Phase 2 inc A (jolt-agw6: collection ctors set/hash-map/hash-set/array-map +
# rand + real map entries / key / val / map-entry?) 1593.
# Phase 2 inc B (jolt-9zhh: readable printer + __pr-str1/__write/__with-out-str
# -> pr-str/pr/prn/print/println/*-str family) + inc C (bit ops + parse-long/
# parse-double) 1652.
# Phase 2 inc D (jolt-jgoc: records + protocols — defrecord/deftype/defprotocol/
# extend-type/reify; jrec type + protocol registry/dispatch; emit routes record
# .method dot-calls to runtime dispatch) 1701.
# Phase 2 inc E (jolt-rkbc: meta / with-meta over an identity-keyed side-table +
# symbol reader-meta carried through quote emit) 1723.
# Phase 2 inc F (jolt-0zoy: jolt.host/tagged-table/ref-put!/ref-get over a Chez
# mutable htable + the 25-sorted tier routed through each value's :ops table —
# sorted-map/sorted-set/subseq/rsubseq + sorted equality; unblocks sorted? and
# every fn that calls it: empty/ifn?/reversible?/map?/set?/coll?. Also an emit fix
# routing a computed call operator ((sorted-map …) k) through jolt-invoke) 1837.
# Phase 2 inc G (jolt-dmw9: lazy-seq bridge — make-lazy-seq / coll->cells over the
# cseq model + a jolt-lazyseq arm on the non-jolt-seq dispatchers (sequential?/=/
# hash/count/empty?/nth/printers); jolt-concat made fully lazy so a self-
# referential lazy-cat (fib) stays productive. Unblocks repeat/iterate/cycle/
# dedupe/take-nth/keep/interpose/reductions/map-indexed/distinct/interleave/
# tree-seq->flatten/partition-all/lazy-cat) 1886.
# Phase 2 inc H (jolt-xjx6: native volatiles (jvol) + sequence/transduce over the
# existing into-xform/reduce-seq — unblocks (sequence xform coll), (transduce
# xform f coll), and the stateful transducer xforms take-nth/map-indexed/
# partition-by that drive a volatile) 1898.
# Phase 2 inc I (jolt-n7rz: first-class vars — emit :the-var to the rt.ss var-cell
# + var?/var-get/deref/invoke/=/pr-str + bound?; def now RETURNS the var (#'ns/name)
# matching Clojure, which also un-allowlists pr-str-of-var/defn) and inc J
# (jolt-snry: scalar natives — UUID random-uuid/parse-uuid/uuid?, format/printf,
# tagged-literal, bigint) 1951.
# jolt-yxqm (namespace value model — find-ns/ns-name/all-ns/resolve/ns-publics/
# in-ns/*ns* over the var-table + a jns ns value; native-op var cells so
# (resolve '+) is a var; *ns* bound to the user ns) 1969.
# jolt-zikh (var def-time metadata — :def emit passes reader meta to
# def-var-with-meta!; (meta (var v)) is {:ns :name} + ^:private/^Type tag/
# docstring) 1972.
# jolt-2o7x (dynamic var binding — the per-thread binding stack +
# binding/with-bindings*/var-set/thread-bound?/with-local-vars/with-redefs/
# bound-fn*/get-thread-bindings/alter-var-root; var-deref + jolt-var-get chained
# onto the stack. Also fixed seq? to recognize a lazy-seq, which unblocked
# with-in-str/line-seq) 2000.
# Strided runs scale down.
(def base-floor (scan-number (or (os/getenv "JOLT_CHEZ_PRELUDE_FLOOR") "2000")))
(def floor (if (os/getenv "JOLT_CORPUS_LIMIT") 0 base-floor))
(when (or (> (length diverged) 0) (< pass floor))
  (printf "REGRESSION: pass %d < floor %d or %d new divergence(s)" pass floor (length diverged)))
(os/exit (if (or (> (length diverged) 0) (< pass floor)) 1 0))
