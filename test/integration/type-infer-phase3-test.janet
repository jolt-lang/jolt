# Collection-element types + HOF awareness, Phase 3 (jolt-d6u). A vector carries
# its element type ({:vec ELEM}); a reduce/map/filter closure over it gets that
# element type on its element param. So a lookup inside a reduce closure over a
# vector-of-structs specializes — no hint — WHEN the element type is provable.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/types :as types)
(import ../../src/jolt/reader :as reader)

(print "Type inference Phase 3 (jolt-d6u)...")

(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init {:compile? true}))
(api/eval-string ctx "(ns p3)")
(def pns (types/ctx-find-ns ctx "jolt.passes"))
(def reinfer (types/var-get (types/ns-find pns "reinfer-def")))
# helper: analyze a defn, reinfer with seeded param types, count guards
(defn guards [src ptmap]
  (def d (backend/analyze-form ctx (reader/parse-string src)))
  (length (string/find-all ":jolt/type" (string/format "%p" (backend/emit-ir ctx (reinfer d ptmap))))))

# a reduce closure's element param gets the vector's element type
(def red "(defn f [coll] (reduce (fn [acc h] (+ acc (:r h))) 0 coll))")
(assert (= 0 (guards red @{"coll" {:vec :struct-map}})) "reduce element typed -> bare lookup in closure")
(assert (= 1 (guards red @{"coll" {:vec :any}})) "reduce over vector of unknown -> guard kept")
(assert (= 1 (guards red @{})) "untyped coll -> guard kept")

# mapv over a vector-of-structs types the closure element too
(def mp "(defn g [coll] (mapv (fn [h] (:r h)) coll))")
(assert (= 0 (guards mp @{"coll" {:vec :struct-map}})) "mapv element typed -> bare lookup")
(assert (= 1 (guards mp @{"coll" {:vec :any}})) "mapv over unknown element -> guard")

# element type is DERIVED, not just seeded: a vector literal of structs, reduced
(def derived "(defn h2 [] (reduce (fn [acc x] (+ acc (:r x))) 0 [{:r 1 :g 2} {:r 3 :g 4}]))")
(assert (= 0 (guards derived @{})) "vector literal of structs -> element struct -> bare lookup")

# correctness: the specialized closures compute the same
(assert (= 4 (api/eval-string ctx "((fn [coll] (reduce (fn [acc h] (+ acc (:r h))) 0 coll)) [{:r 1} {:r 3}])")) "reduce value")
(assert (= 4 (api/eval-string ctx "(reduce (fn [acc x] (+ acc (:r x))) 0 [{:r 1 :g 2} {:r 3 :g 4}])")) "derived value")

(print "Type inference Phase 3 passed!")
