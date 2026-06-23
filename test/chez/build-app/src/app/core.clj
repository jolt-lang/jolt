(ns app.core
  (:require [app.util :as util]
            [clojure.java.io :as io]))

(defn -main [& args]
  ;; the resource is baked into the binary (deps.edn :jolt/build :embed), so this
  ;; resolves with no resources/ dir on disk, run from any cwd.
  (println (slurp (io/resource "greeting.txt")))
  (util/twice (println (util/shout "hello from a built binary")))
  (println "args:" (vec args))
  (println "sum:" (reduce + (map count args))))
