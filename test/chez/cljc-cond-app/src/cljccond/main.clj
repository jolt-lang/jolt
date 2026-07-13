(ns cljccond.main (:require [cljccond.lib :as lib]))
(defn -main [& _] (println "CLJC-COND" (lib/before) (lib/after)))
