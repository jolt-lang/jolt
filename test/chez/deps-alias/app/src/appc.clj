(ns appc (:require [libc.core :as c]))
(defn -main [& args] (println "libc" c/version))
