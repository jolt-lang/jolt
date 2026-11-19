(ns jolt.passes.transducers
  "Recognize explicit core transducer stages while their resolved calls are intact.")

(defn- core-call? [node nm arity]
  (let [f (:fn node)]
    (and (= :invoke (:op node)) (= :var (:op f))
         (= "clojure.core" (:ns f)) (= nm (:name f))
         (= arity (count (:args node))))))

(defn filter-map-operands [node]
  (when (core-call? node "into" 3)
    (let [[dest xf source] (:args node)]
      (when (and (= :vector (:op dest)) (empty? (:items dest))
                 (core-call? xf "comp" 2))
        (let [[filter-stage map-stage] (:args xf)]
          (when (and (core-call? filter-stage "filter" 1) (core-call? map-stage "map" 1))
            [dest (first (:args filter-stage)) (first (:args map-stage)) source]))))))
