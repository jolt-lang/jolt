(ns gx.main1 (:require [gx.b :as b] [gx.c :as c]))
(defn -main [& _]
  (println "main1" (b/call-f 1) ((b/ref-f) 2) (c/call-g 3) ((c/ref-g) 4)))
