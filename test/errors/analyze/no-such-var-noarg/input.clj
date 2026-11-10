(ns input
  (:require [clojure.string :as s]))

;; With no arguments the call was read as a static member of a class named by
;; the alias: "Unknown class s" when -main ran. It is the same compile error as
;; (s/nope 1), as on the JVM.
(defn -main []
  (s/nope))
