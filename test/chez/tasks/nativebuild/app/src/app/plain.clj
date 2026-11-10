(ns app.plain)

;; Requires nothing of the library: its native is never opened, so a run builds
;; nothing and needs nothing on disk.
(defn -main [& _] (println "plain" (vec (jolt.host/pending-native-roots))))
