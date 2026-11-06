(ns plain
  "A docstring holding an escaped quote, \"like this\"."
  {:author "someone"}
  (:require [clojure.string :as str])
  [:import [java.io File]]
  (:gen-class))

;; An alias this file never loaded, past the ns form, does not count.
(def later ::not-required/keyword)
