(ns app.core)

;; defonce keeps first value; the dead def below should be pruned by tree-shake
(defonce x 1)
(defonce x 2)

(def alive "alive")
(def dead "should-be-pruned")  ;; never referenced

(defn -main [& _]
  (println x)
  (println alive))
