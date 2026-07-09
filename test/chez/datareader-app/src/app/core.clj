(ns app.core)

(defn -main [& _]
  (println (read-string "#my/rev \"hello\"")))
