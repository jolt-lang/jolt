# Vector op specialization, Phase 2 (jolt-d6u): a value the inference proved to
# be a vector ({:vec ...}) gets count -> pv-count (skip core-count's dispatch)
# and 3-arg nth -> pv-nth. 2-arg nth is NOT specialized: it errors on
# out-of-bounds where pv-nth returns nil.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/types :as types)
(import ../../src/jolt/reader :as reader)
(print "Type inference Phase 2 (vector ops)...")
(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init-cached {:compile? true}))
(api/eval-string ctx "(ns p2)")
(def reinfer (types/var-get (types/ns-find (types/ctx-find-ns ctx "jolt.passes") "reinfer-def")))
(defn estr [src ptmap]
  (string/format "%p" (backend/emit-ir ctx (reinfer (backend/analyze-form ctx (reader/parse-string src)) ptmap))))
(assert (string/find "pv-count" (estr "(defn f [v] (count v))" @{"v" {:vec :any}})) "count on vector -> pv-count")
(assert (not (string/find "pv-count" (estr "(defn f [v] (count v))" @{}))) "count on unknown not specialized")
(assert (string/find "pv-nth" (estr "(defn f [v i] (nth v i 0))" @{"v" {:vec :any}})) "3-arg nth on vector -> pv-nth")
(assert (not (string/find "pv-nth" (estr "(defn f [v i] (nth v i))" @{"v" {:vec :any}}))) "2-arg nth NOT specialized")
# correctness
(assert (= 3 (api/eval-string ctx "(count [1 2 3])")) "count value")
(assert (= 2 (api/eval-string ctx "(nth [1 2 3] 1 9)")) "nth 3-arg in-bounds")
(assert (= 9 (api/eval-string ctx "(nth [1 2 3] 5 9)")) "nth 3-arg default")
(print "Type inference Phase 2 passed!")
