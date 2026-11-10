(ns input
  (:require [clojure.string :as s]))

;; The var is missing from a namespace the alias names: a compile error when the
;; file loads, as on the JVM, not "Unknown class s" when -main finally runs.
(defn -main []
  (s/nope 1))
