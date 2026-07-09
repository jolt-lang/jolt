(ns app.core)

(def a 1)
(def b 2)
(def c 3)

(defn -main [& _]
  (doseq [[sym var] (ns-publics *ns*)]
    (println (str sym " = " (var-get var)))))
