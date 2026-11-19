(ns jolt.passes.transducers
  "Recognize explicit core transducer stages while their resolved calls are intact.")

(defn- core-call? [node nm arity]
  (let [f (:fn node)]
    (and (= :invoke (:op node)) (= :var (:op f))
         (= "clojure.core" (:ns f)) (= nm (:name f))
         (= arity (count (:args node))))))

(defn- empty-vector-comp [node]
  (when (core-call? node "into" 3)
    (let [[dest xf] (:args node)]
      (when (and (= :vector (:op dest)) (empty? (:items dest))
                 (core-call? xf "comp" 2))
        xf))))

(defn filter-map-operands [node]
  (when-let [xf (empty-vector-comp node)]
    (let [[dest _ source] (:args node)
          [filter-stage map-stage] (:args xf)]
      (when (and (core-call? filter-stage "filter" 1) (core-call? map-stage "map" 1))
        [dest (first (:args filter-stage)) (first (:args map-stage)) source]))))

(defn map-optional-take [node]
  (when-let [xf (empty-vector-comp node)]
    (let [[map-stage choice] (:args xf)
          {:keys [then else]} choice]
      (when (and (core-call? map-stage "map" 1) (= :if (:op choice))
                 (core-call? then "take" 1)
                 (= :var (:op else)) (= "clojure.core" (:ns else))
                 (= "identity" (:name else)))
        xf))))
