(ns app.util
  (:require [clojure.string :as str]))

(defn shout [s]
  (str/upper-case (str s "!")))

(defmacro twice [x]
  `(do ~x ~x))

;; A multimethod with a :default method. The AOT build must set the per-ns
;; current ns before these forms run, or the defmethod registers app.util/greet
;; under the wrong ns and a dispatch to :default crashes (not a fn nil). app.core
;; adds an aliased method (util/greet :loud) — see there.
(defmulti greet (fn [kind] kind))
(defmethod greet :default [_] "greet:default")
