(ns app.readers
  (:require app.util))

(defn reverse-str [s]
  (app.util/shout (apply str (reverse s))))