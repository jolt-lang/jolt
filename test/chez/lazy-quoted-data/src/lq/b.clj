(ns lq.b (:require [lq.a :as a]))
(defn literals [] [:a 'sym '^{:tag String} hinted-sym [1 2] {:k :v} #{1}])
(def ^{:doc "b's" :from-b true} bvar :b)
