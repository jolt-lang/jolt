(ns pa.core)

(defprotocol Greet (greet [this]))

(extend-protocol Greet
  Object
  (greet [_] :from-A))

(defn reified [] (reify Greet (greet [_] :reified-A)))
