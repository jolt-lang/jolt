(ns app.util
  (:require [clojure.string :as str]))

(defn shout [s]
  (str/upper-case (str s "!")))

(defmacro twice [x]
  `(do ~x ~x))
