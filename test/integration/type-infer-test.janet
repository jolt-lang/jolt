# Static collection-type inference, Phase 0 (jolt-6sr): intra-procedural.
# The pass infers an expression's collection type from literals/arithmetic and
# flows it through let bindings and if-joins. Where a keyword-lookup subject is
# PROVEN to be a plain struct map it auto-drops the :jolt/type guard (the
# inference output is the same ^:struct channel as a manual hint); where the
# type is unknown it stays :any and keeps the dynamic guard (sound fallback).
#
# Note: Route 1 scalar-replacement already eliminates NON-escaping let-bound
# maps outright, so these cases force the map to ESCAPE (pass it to `sink`) to
# isolate what inference adds — typing a map that survives and is then looked up.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "Type inference Phase 0 (jolt-6sr)...")

(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init-cached {:compile? true}))
(api/eval-string ctx "(ns ti)")

(defn guards [src]
  (length (string/find-all ":jolt/type"
                           (string/format "%p" (backend/emit-ir ctx (backend/analyze-form ctx (reader/parse-string src)))))))
(defn ev [src] (api/eval-string ctx src))

# --- guard auto-removal where the type is proven, no hint -------------------
# escaping struct-map literal (scalar keys, truthy values) is proven struct
(assert (= 0 (guards "(fn [sink] (let [v {:r 1 :g 2 :b 3}] (sink v) (:r v)))")) "inferred struct-map literal -> bare lookup")
# arithmetic values are provably non-nil/non-false -> still a struct
(assert (= 0 (guards "(fn [sink a b] (let [v {:r (+ a 1) :g (* b 2) :b 7}] (sink v) (:r v)))")) "arithmetic-valued map inferred struct")
# the inferred type flows through a rebinding
(assert (= 0 (guards "(fn [sink] (let [v {:r 1 :g 2} w v] (sink w) (:r w)))")) "inferred type flows through a rebinding")
# both if-branches struct -> join is struct
(assert (= 0 (guards "(fn [sink c] (let [v (if c {:a 1} {:a 2})] (sink v) (:a v)))")) "if-join of two struct literals stays struct")

# --- sound fallback to the guard where the type is NOT proven ---------------
# a param is unknown (Phase 1 handles params) -> guard kept, exactly as today
(assert (= 1 (guards "(fn [m] (:r m))")) "unknown param keeps the guard")
# a value that could be nil/false makes the literal maybe-phm -> :any -> guard
(assert (= 1 (guards "(fn [sink x] (let [v {:r x}] (sink v) (:r v)))")) "maybe-nil value -> not proven struct -> guard")
# join of a struct and a phm is :any -> guard
(assert (>= (guards "(fn [sink c] (let [v (if c {:a 1} (hash-map :a nil))] (sink v) (:a v)))") 1) "struct/phm join -> :any -> guard")

# --- correctness: every shape evaluates to the same as the guarded path -----
(def snk "(fn [_] nil)")
(assert (= 1 (ev (string "((fn [sink] (let [v {:r 1 :g 2 :b 3}] (sink v) (:r v))) " snk ")"))) "struct literal value")
(assert (= 6 (ev (string "((fn [sink a] (let [v {:r (+ a 1)}] (sink v) (:r v))) " snk " 5)"))) "arithmetic-valued struct")
(assert (= 2 (ev (string "((fn [sink] (let [v {:r 1 :g 2} w v] (sink w) (:g w))) " snk ")"))) "flowed type value")
(assert (= 1 (ev (string "((fn [sink c] (let [v (if c {:a 1} {:a 2})] (sink v) (:a v))) " snk " true)"))) "if-join value")
(assert (= nil (ev (string "((fn [sink x] (let [v {:r x}] (sink v) (:r v))) " snk " nil)"))) "maybe-nil map reads correctly (nil)")
(assert (= nil (ev (string "((fn [sink c] (let [v (if c {:a 1} (hash-map :a nil))] (sink v) (:a v))) " snk " false)"))) "phm branch reads nil correctly")
(assert (= 1 (ev (string "((fn [sink c] (let [v (if c {:a 1} (hash-map :a nil))] (sink v) (:a v))) " snk " true)"))) "struct branch reads correctly")

(print "Type inference Phase 0 passed!")
