(ns fix.main
  (:require [clojure.string :as ss]
            [fix.lib :as lib]))
(defn -main [& _]
  (println (ss/upper-case "hi") (lib/u)))
