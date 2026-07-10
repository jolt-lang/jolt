(ns app.core)

(defn -main [& _]
  ;; scalar-replace folds (:a {:a 1 :b (/ 1 0)}) -> 1 under --opt --direct-link,
  ;; discarding the throwing sibling. The divisor must still evaluate: / is not a
  ;; pure fn, so the map is kept, the ArithmeticException fires, and the catch
  ;; prints THROW OK instead of the folded 1.
  (println
    (try
      (:a {:a 1 :b (/ 1 0)})
      (catch ArithmeticException _ "THROW OK"))))
