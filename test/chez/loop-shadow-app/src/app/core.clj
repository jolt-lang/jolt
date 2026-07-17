(ns app.core)

(defrecord P [x y])

(defn -main [& _]
  ;; loop var p shadows the record-typed outer p. Under --opt the bug typed the
  ;; loop p as the record, devirtualizing (:x p) to a record slot read that
  ;; crashed on the vector [3 4]. The fix keeps the loop p :any, so (:x p) is a
  ;; generic keyword lookup -> nil (the JVM value). The second line carries the
  ;; record straight through to prove field reads still devirtualize.
  (println (let [p (->P 1.0 2.0)]
             (loop [p [3 4]]
               (:x p))))
  (println (:x (->P 1.0 2.0))))
