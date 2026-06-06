;; clojure.edn — reading EDN data. Delegates to the Jolt reader via
;; clojure.core/read-string (which parses, never evaluates — safe for EDN), and
;; adds the opts-map arity with :eof plus nil/blank-input handling.
(ns clojure.edn
  "Reading EDN data."
  (:require [clojure.string :as cstr]))

;; Private helper, NOT named read-string: an unqualified (read-string …) call
;; dispatches the core read-string SPECIAL FORM (by name, regardless of ns), so
;; the 1-arity can't delegate to the 2-arity through that name.
(defn- read-edn [opts s]
  (if (or (nil? s) (cstr/blank? s))
    (get opts :eof nil)
    (clojure.core/read-string s)))

(defn read-string
  "Reads one object from the string s. Returns the :eof option value (default
  nil) for nil or blank input. opts is an options map; :eof sets the value
  returned at end of input."
  ([s] (read-edn {} s))
  ([opts s] (read-edn opts s)))

(defn read
  "Reads the next line from reader and parses one EDN object from it."
  [reader]
  (let [line ((get (dyn :current-env) (symbol "file/read")) reader :line)]
    (when line (read-string line))))
