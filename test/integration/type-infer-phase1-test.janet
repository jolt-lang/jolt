# Inter-procedural collection-type inference, Phase 1 (jolt-767): closed-world.
# A whole-unit fixpoint propagates collection types through the call graph — a
# fn's param types become the lub of its in-unit call-site arg types — so a
# param that always receives a struct map gets typed and its lookups specialize,
# with no hint. Fns whose var escapes as a value keep :any params (their callers
# aren't all visible). Sound under source distribution + whole-program compile.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/types :as types)
(import ../../src/jolt/reader :as reader)

(print "Type inference Phase 1 (jolt-767)...")

(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init {:compile? true}))
(api/eval-string ctx "(ns p1)")
# closed-world unit. mk is small (inlined away). rd is RECURSIVE, so it survives
# inlining and is called via its var — exactly the shape (big/recursive fn with
# escaping-from-the-caller params) that inter-procedural inference targets. Its
# param v flows from mk's struct-map literal (after mk inlines into drv).
(each s ["(defn mk [a b] {:r a :g b})"
         "(defn rd [v n] (if (< n 1) (:r v) (rd v (dec n))))"
         "(defn drv [] (rd (mk 1 2) 3))"
         # esc's var is used as a VALUE (passed to mapv) -> params must stay :any
         "(defn esc [w] (:r w))"
         "(defn use-esc [xs] (mapv esc xs))"]
  (api/eval-string ctx s))

(def report (backend/infer-unit! ctx "p1"))

# --- the fixpoint computed the right param types -----------------------------
# rd's param v flows from mk's struct result (mk inlines to a struct literal in
# drv) and stays struct across the recursive self-call -> a {:struct ...} type
(defn struct-type? [t] (truthy? (get t :struct)))
(assert (struct-type? (in (get report "p1/rd") 0)) (string "rd param v: " (in (get report "p1/rd") 0)))
# esc escaped (passed to mapv) -> param stays unknown (:any / nil), NOT struct
(assert (not (struct-type? (in (get report "p1/esc") 0))) "escaping fn param not inferred struct")

# --- the seeded re-inference drops the guard for a struct param --------------
# (on a FRESH analysis, since infer-unit! re-stashes the already-specialized body)
(def pns (types/ctx-find-ns ctx "jolt.passes"))
(def reinfer (types/ns-find pns "reinfer-def"))
(def rd-def (backend/analyze-form ctx (reader/parse-string "(defn rdx [v n] (if (< n 1) (:r v) (rdx v (dec n))))")))
(defn guards-seeded [ptmap]
  (length (string/find-all ":jolt/type" (string/format "%p" (backend/emit-ir ctx ((types/var-get reinfer) rd-def ptmap))))))
(assert (= 0 (guards-seeded @{"v" {:struct {}}})) "struct param -> bare lookup")
(assert (= 1 (guards-seeded @{})) "no param type -> guard kept")

# --- correctness: recompiled unit still computes the same --------------------
(assert (= 1 (api/eval-string ctx "(p1/drv)")) "drv correct after recompile")
(assert (= 7 (api/eval-string ctx "(p1/rd {:r 7 :g 8} 0)")) "rd correct on a struct")
(assert (= nil (api/eval-string ctx "(p1/rd (hash-map :r nil) 0)")) "rd correct on a phm (key present, nil)")
(assert (deep= [1 1] (api/normalize-pvecs (api/eval-string ctx "(p1/use-esc [{:r 1} {:r 1}])"))) "escaping fn still correct")

(print "Type inference Phase 1 passed!")
