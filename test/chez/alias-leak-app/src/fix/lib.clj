(ns fix.lib
  (:require [clojure.set :as ss]))
(defn u [] (ss/union #{1} #{2}))
