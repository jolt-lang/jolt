(ns xtool
  "Exec-fn fixture for the -X gate: prints the args map it is handed.")
(defn hello [m] (println "exec:" (pr-str m)))
