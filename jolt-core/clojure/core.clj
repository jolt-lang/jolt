;; The Clojure portion of clojure.core. Loaded into the clojure.core namespace at
;; init (api/init), AFTER the Janet primitives are interned by core/init-core!,
;; and compiled by the self-hosted pipeline (analyzer -> IR -> Janet back end).
;;
;; This is the Phase 4 kernel-shrink seam: fns expressible in plain Clojure on top
;; of the remaining Janet primitives move here from core.janet, one at a time,
;; each compiled by the prior stage. Anything here must depend only on core vars
;; already interned by init-core! (and on other overlay fns defined above it).

(defn ffirst [coll] (first (first coll)))
(defn fnext  [coll] (first (next coll)))
(defn nnext  [coll] (next (next coll)))
