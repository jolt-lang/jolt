(ns input)

(defprotocol Shape (area [s]))

(deftype Square [n]
  Shape
  (area [_] (* n n))
  (area [this] (* n n 1.0)))
