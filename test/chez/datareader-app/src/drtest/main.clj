(ns drtest.main
  (:require drtest.reader))
(defn -main [& _]
  (println #code [:ignored])
  (println (read-string "#my/rev \"hello\"")))