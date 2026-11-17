(ns gx.b (:require [gx.a :as a]))
(defn call-f [x] (a/f x))
(defn ref-f [] a/f)
