(ns app.go
  (:require [jbt.core :as jbt]))

(defn -main [& _] (println "answer" (jbt/answer)))
