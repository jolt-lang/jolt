# Staged-bootstrap soundness (jolt-vcx, under epic jolt-tzo).
#
# The self-hosted compiler's structural deps — second/peek/subvec/mapv/update —
# now come from the Clojure kernel tier (jolt-core/clojure/core/00-kernel.clj),
# bootstrap-compiled into clojure.core BEFORE the analyzer is built. This pins
# the two properties that make that safe:
#
#   1. Compile mode: the analyzer (which itself calls second/peek/subvec/mapv)
#      compiles analyzer-exercising forms correctly — the exact case that broke
#      when `second` was a plain overlay fn: (first {:a 1}) / (key (first ...)).
#   2. Bootstrap FIXPOINT: rebuilding the compiler (rebuild-compiler!) against the
#      now Clojure-defined core still yields a correct compiler. This is the
#      soundness gate for every future fractal turn (S2 -> S3).

(use ../../src/jolt/api)
(import ../../src/jolt/backend :as backend)

(var failures 0)

# Each probe is a jolt boolean expression; compared with jolt's own `=`.
(def probes
  ["(= 2 (second [1 2 3]))"
   "(= nil (second [1]))"
   "(= 3 (peek [1 2 3]))"
   "(= 1 (peek (list 1 2 3)))"
   "(= nil (peek []))"
   "(= [2 3] (subvec [1 2 3 4 5] 1 3))"
   "(= [3 4 5] (subvec [1 2 3 4 5] 2))"
   "(= [2 3 4] (mapv inc [1 2 3]))"
   "(= [11 22 33] (mapv + [1 2 3] [10 20 30]))"
   "(= {:a 2} (update {:a 1} :a inc))"
   "(= {:a 1 :b 1} (update {:a 1} :b (fnil inc 0)))"
   # Regression: these run the analyzer's own second/map-pair path in compile mode.
   "(= [:a 1] (first {:a 1}))"
   "(= :a (key (first {:a 1})))"
   "(= 1 (val (first {:a 1})))"
   "(= 3 (let [[a b] [1 2]] (+ a b)))"
   "(= 3 (loop [i 0 acc 0] (if (< i 3) (recur (inc i) (+ acc i)) acc)))"])

(defn- run-probes [ctx label]
  (each prog probes
    (def got (protect (eval-string ctx prog)))
    (unless (and (got 0) (= (got 1) true))
      (++ failures)
      (printf "FAIL [%s] %s => %s" label prog
              (if (got 0) (string/format "%q" (got 1)) (string "ERR:" (got 1)))))))

# Interpret mode: kernel tier interpreted, no analyzer involved.
(run-probes (init {}) "interpret")

# Compile mode: kernel tier bootstrap-compiled, analyzer built against it.
(def cctx (init {:compile? true}))
(run-probes cctx "compile")

# Fixpoint: rebuild the compiler against the current (Clojure-defined) core and
# re-run. A correct compiler recompiled on the language it just defined stays
# correct.
(backend/rebuild-compiler! cctx)
(run-probes cctx "compile+rebuilt")

(if (pos? failures)
  (do (printf "staged-bootstrap: %d failure(s)" failures) (os/exit 1))
  (print "staged-bootstrap: all probes passed (interpret, compile, compile+rebuilt)"))
