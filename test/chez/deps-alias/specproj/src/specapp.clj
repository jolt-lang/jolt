(ns specapp
  (:require [clojure.spec.alpha :as s]))

(s/def ::small (s/and int? #(< % 10)))

(defn -main [& _]
  (println "spec:" (s/valid? ::small 3) (s/valid? ::small 30)))
