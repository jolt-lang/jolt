# Phase 1 (jolt-cf1q.2, inc 3d) — clojure.core prelude emission probe.
#
# The path to an `-e`-capable jolt-chez: emit the clojure.core tiers
# (jolt-core/clojure/core/NN-*.clj) through the SAME live Janet analyzer ->
# host/chez/emit pipeline, as a Scheme PRELUDE of `def-var!` forms. User code's
# `(var-deref "clojure.core" "<fn>")` then resolves the fn at runtime.
#
# Most core fns are NOT native-ops, so they must be emitted; the ones that
# reference host interop / native Janet ops / unimplemented primitives can't be
# emitted yet (each a clean "out of subset" emit error). This probe reports how
# far the emit gets per tier and aggregates the gap list — the punch-list the
# next increments chase down. Measurement tool, gated out of the default suite.
#   JOLT_CHEZ_PRELUDE=1 janet test/chez/core-prelude-probe.janet
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../src/jolt/types_ctx :as tctx)
(import ../../host/chez/emit :as emit)

(unless (os/getenv "JOLT_CHEZ_PRELUDE")
  (print "skip: set JOLT_CHEZ_PRELUDE=1 to run the core-prelude emission probe")
  (os/exit 0))

# load order — same as api/core-tiers (the kernel tier is bootstrap-compiled in
# the live system; here we just measure emit reach, so treat it like the rest).
(def tier-files
  ["00-syntax" "00-kernel" "10-seq" "20-coll" "25-sorted" "30-macros" "40-lazy" "50-io"])

(defn- parse-all [src]
  (def out @[])
  (var s src)
  (while (> (length (string/trim s)) 0)
    (def parsed (r/parse-next s))
    (set s (in parsed 1))
    (def f (in parsed 0))
    (unless (nil? f) (array/push out f)))
  out)

# jolt reader forms are arrays of jolt VALUES; a symbol is a struct
# {:jolt/type :symbol :name "..."} (jolt symbols aren't Janet symbols).
(defn- sym-name [x]
  (when (and (struct? x) (= :symbol (get x :jolt/type))) (get x :name)))

# A short label for a top-level form: the defn/def name, or the form head.
(defn- form-label [f]
  (if (and (indexed? f) (> (length f) 1))
    (let [head (or (sym-name (in f 0)) "?") nm (sym-name (in f 1))]
      (if nm (string head " " nm) head))
    (string/slice (string/format "%p" f) 0 40)))

# Pull the unsupported fn/op name out of an emit error message for aggregation.
(defn- gap-key [msg]
  (def m (string msg))
  (cond
    (string/find "stdlib fn" m) (let [i (string/find "`" m)] (string "stdlib: " (string/slice m (inc i) (string/find "`" m (inc i)))))
    (string/find "stdlib ref" m) (let [i (string/find "`" m)] (string "stdlib: " (string/slice m (inc i) (string/find "`" m (inc i)))))
    (string/find "host call" m) "host-call"
    (string/find "host ref" m) "host-ref"
    (string/find "unhandled op" m) (string/slice m (max 0 (- (length m) 30)))
    (string/find "unsupported literal" m) "unsupported-literal"
    (string/slice m 0 (min 50 (length m)))))

# Macros are analyze-time only (the Janet analyzer expands them away before emit),
# so they don't belong in a RUNTIME prelude — skip them, don't count as gaps.
(defn- macro-form? [f]
  (and (indexed? f) (> (length f) 0)
       (let [h (sym-name (in f 0))] (and h (or (= h "defmacro") (= h "definline"))))))

(emit/set-prelude-mode! true)
(def ctx (api/init {:compile? true}))
(tctx/ctx-set-current-ns ctx "clojure.core")

(var total 0) (var compiled 0)
(def gaps @{})        # gap-key -> count
(def gap-examples @{}) # gap-key -> first form label that hit it

(each tf tier-files
  (def src (slurp (string "jolt-core/clojure/core/" tf ".clj")))
  (def forms (parse-all src))
  (var t-total 0) (var t-ok 0)
  (each f forms
    (unless (macro-form? f)
    (++ total) (++ t-total)
    (def res (protect (emit/emit (backend/analyze-form ctx f))))
    (if (res 0)
      (do (++ compiled) (++ t-ok))
      (let [k (gap-key (res 1))]
        (put gaps k (+ 1 (or (get gaps k) 0)))
        (unless (get gap-examples k) (put gap-examples k (form-label f)))))))
  (printf "  %-10s %3d/%-3d forms emit" tf t-ok t-total))

(printf "\nCore prelude emit reach: %d/%d top-level forms compile to Scheme" compiled total)
(printf "%d distinct gaps (fn/op the emit back end can't lower yet):" (length gaps))
(def sorted-gaps (sort-by (fn [k] (- (get gaps k))) (keys gaps)))
(each k sorted-gaps
  (printf "  %4d x  %-34s  e.g. %s" (get gaps k) k (get gap-examples k)))
(flush)

# Regression floor (raise it as new IR ops / RT shims land, like the suite
# baseline). Fails if prelude emit reach drops below the recorded baseline.
(def reach-floor 348)
(when (< compiled reach-floor)
  (printf "REGRESSION: prelude emit reach %d < floor %d" compiled reach-floor)
  (os/exit 1))
