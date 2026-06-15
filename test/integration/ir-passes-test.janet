# IR pass pipeline (jolt-2om, nanopass-lite): jolt.passes/run-passes applies
# pure IR->IR rewrites between the analyzer and the back end. The first pass
# is constant folding — it computes with the ACTUAL jolt fns, so folded
# results match runtime semantics by construction.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "IR passes (constant folding)...")
(def ctx (api/init-cached {:compile? true}))
(defn ir [src] (backend/analyze-form ctx (reader/parse-string src)))

(defn check-const [src want]
  (def n (ir src))
  (assert (= :const (n :op)) (string src " folds to a constant"))
  (assert (= want (n :val)) (string src " folds to " (string/format "%q" want))))

(check-const "(+ 1 2 3)" 6)
(check-const "(* 2 (+ 3 4))" 14)
(check-const "(quot 7 2)" 3)
(check-const "(mod -7 3)" 2)
(check-const "(if (< 1 2) :yes :no)" :yes)
# dead-branch elimination: the untaken branch never evaluates (it must still
# RESOLVE — unresolved symbols are analysis errors as in Clojure, jolt-2o7.3)
(check-const "(if false (throw (ex-info \"boom\" {})) 2)" 2)
# an unresolvable symbol errors even in a dead branch (analysis precedes folding)
(assert (not ((protect (ir "(if false (this-would-not-resolve) 2)")) 0))
        "unresolved symbol errors even in a dead branch")

# non-constants stay calls; folding must be conservative. `xq` is a real var
# (defined here) so it resolves, but its VALUE must not be folded in.
(api/eval-string ctx "(def xq 1)")
(assert (= :invoke ((ir "(+ xq 2)") :op)) "var ref stays a call")
(assert (= :invoke ((ir "(mod xq 0)") :op)) "non-const args stay calls")
# a fold that would THROW is left for runtime
(assert (= :invoke ((ir "(mod 5 0)") :op)) "throwing fold left to runtime")

# and the folded code evaluates identically (3-mode conformance covers the
# broader matrix; this pins a couple end-to-end)
(assert (= 6 (api/eval-string ctx "(+ 1 2 3)")) "folded eval")
(assert (= :yes (api/eval-string ctx "(if (< 1 2) :yes :no)")) "folded if eval")

# Folding reaches into every op's children via map-ir-children (phase 3a) — it is
# now total, so a constant nested in a fn arity / loop / try body folds too
# (const-fold previously passed :try through unfolded). Sound: folding preserves
# runtime results regardless of position, so these eval end-to-end (3-mode
# conformance covers the broader matrix).
(assert (= 3 ((api/eval-string ctx "(fn [x] (+ 1 2))") 0)) "folded fn arity body eval")
(assert (= 10 (api/eval-string ctx "(loop [i 0] (if (< i (* 2 5)) (recur (inc i)) i))")) "folded loop eval")
(assert (= 5 (api/eval-string ctx "(try (+ 2 3) (catch Throwable e 0))")) "folded try eval")
(print "IR passes passed!")
