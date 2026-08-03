(ns pa.core)

(defprotocol Greet (greet [this]))

(extend-protocol Greet
  Object
  (greet [_] :from-A))
