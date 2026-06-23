(ns app.core
  (:require [app.util :as util]))

(defn -main [& args]
  (util/twice (println (util/shout "hello from a built binary")))
  (println "args:" (vec args))
  (println "sum:" (reduce + (map count args))))
