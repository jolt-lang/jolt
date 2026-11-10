(ns app.later)

;; Requires the library at run time: its native loads then, not at startup.
(defn -main [& _]
  (println "pending before" (count (jolt.host/pending-native-roots)))
  (require 'jbt.core)
  (println "pending after" (count (jolt.host/pending-native-roots)))
  (println "answer" ((resolve 'jbt.core/answer))))
