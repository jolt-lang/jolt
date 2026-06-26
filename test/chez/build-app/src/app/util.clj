(ns app.util
  (:require [clojure.string :as str]))

(defn shout [s]
  (str/upper-case (str s "!")))

;; A two-deep non-tail call chain that throws — exercises native stack traces in a
;; direct-link build (build-smoke runs -main with a --boom sentinel arg).
(defn deep-boom [x]
  (assert (number? x) "needs a number")
  (* x 2))

(defn mid-boom [x]
  (inc (deep-boom x)))

(defmacro twice [x]
  `(do ~x ~x))

;; A multimethod with a :default method. The AOT build must set the per-ns
;; current ns before these forms run, or the defmethod registers app.util/greet
;; under the wrong ns and a dispatch to :default crashes (not a fn nil). app.core
;; adds an aliased method (util/greet :loud) — see there.
(defmulti greet (fn [kind] kind))
(defmethod greet :default [_] "greet:default")
