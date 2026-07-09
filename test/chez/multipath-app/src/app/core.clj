(ns app.core)

;; Two code paths selected by argv: a def wrongly shaken on EITHER path shows
;; up as an output diff for that invocation — the smoke runs both.
(defn summarize [xs] (str "sum=" (reduce + 0 xs)))
(defn tabulate [xs] (str "table=" (vec (map inc xs))))

(defn -main [& args]
  (if (= (first args) "alt")
    (println (tabulate [1 2 3]))
    (println (summarize [1 2 3]))))
