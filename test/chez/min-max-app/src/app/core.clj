(ns app.core)

(defn -main [& _]
  ;; min/max return an operand unchanged; a --opt/inference double-contagion bug
  ;; used to coerce the int operand to a flonum, so (min 2.5 1) printed 1.0 and
  ;; (max 2.5 3) printed 3.0. They must preserve the int.
  (println (min 2.5 1))
  (println (max 2.5 3)))
