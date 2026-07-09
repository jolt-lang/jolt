(ns app.core)

(defn reverse-str [s]
  (apply str (reverse s)))

(defn -main [& _]
  (println (read-string "#my/rev \"hello\"")))
