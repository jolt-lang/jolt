(ns app.core)

(def a 1)
(def b 2)
(def c 3)

(defn -main [& _]
  ;; enumerate the ns at runtime: these defs are referenced by no :var node, so
  ;; a tree-shake that doesn't bail on ns-publics wrongly prunes them.
  (doseq [[sym v] (sort-by (fn [[s _]] (str s)) (ns-publics 'app.core))]
    (let [val (var-get v)]
      (when (number? val)
        (println (str sym " = " val))))))
