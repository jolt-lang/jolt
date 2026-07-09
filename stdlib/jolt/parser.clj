;; Top-level parse driver, adapted from rm-hull/jasentaa (MIT).

(ns jolt.parser
  (:refer-clojure :exclude [apply])
  (:require
   [jolt.parser.combinators :refer [spaces]]
   [jolt.parser.monad :as m :refer [>>=]]
   [jolt.parser.position :refer [augment-location parse-exception]]))

(defn apply
  "Apply a parser, throwing away any leading space:"
  [parser input]
  (m/bind
   (augment-location input)
   (m/do*
    spaces
    parser)))

(def ^:private first-error
  (comp first second first))

(defn parse-all
  "Attempts to fully consume the input using the supplied parser.
   Throws on input that cannot be fully parsed."
  [parser input]
  (let [result (apply parser input)
        parsed (ffirst (filter (comp empty? second) result))]
    (or
     parsed
     (throw (parse-exception (first-error result))))))
