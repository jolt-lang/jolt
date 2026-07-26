(ns umapp.main
  (:require [umapp.wrapping :as w]
            [umapp.checked :as c]))

;; Three things, in order: the sibling file's top-level (set! *unchecked-math* …)
;; was legal and took effect (Long/MAX_VALUE + 1 wrapped to Long/MIN_VALUE); the
;; effect did not escape that file (the same expression next door still widens);
;; and the root binding is untouched. jolt widens where the JVM throws on long
;; overflow — un-hinted integers keep arbitrary precision — so the middle value
;; differs from Clojure's, but either way it is the not-wrapped answer.
(defn -main [& _]
  (println "UNCHECKED-MATH" (w/add-max) (c/add-max) *unchecked-math*))
