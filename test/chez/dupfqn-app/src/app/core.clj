(ns app.core)

(defn mk-h [] 41)
(def h (mk-h))
(def h (inc h))

(defn -main [& _]
  (println h))