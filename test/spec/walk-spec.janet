# Specification: clojure.walk must descend lists and seqs, not just vectors/maps
# (jolt-khk). postwalk-replace with symbol keys over a quoted list silently
# no-op'd because walk only handled vector?/map? and fell through for list?/seq?
# — which broke clojure.template/apply-template (found during reitit work).
(use ../support/harness)

(defspec "clojure.walk / lists + seqs"
  ["postwalk-replace symbol keys in a list" "(quote (+ 2 2))"
   "(do (require (quote [clojure.walk :as w])) (w/postwalk-replace {(quote x) 2} (quote (+ x x))))"]
  ["postwalk descends a list" "(quote (:a :a))"
   "(do (require (quote [clojure.walk :as w])) (w/postwalk (fn [n] (if (symbol? n) :a n)) (quote (x y))))"]
  ["prewalk-replace in a list" "(quote (* 3 3))"
   "(do (require (quote [clojure.walk :as w])) (w/prewalk-replace {(quote *) (quote *) (quote y) 3} (quote (* y y))))"]
  ["nested list + vector" "(quote (1 [2 1]))"
   "(do (require (quote [clojure.walk :as w])) (w/postwalk-replace {:a 1 :b 2} (quote (:a [:b :a]))))"]
  # vectors/maps still work (regression guard for the existing behavior)
  ["postwalk-replace in a vector" "[:one 2 :one]"
   "(do (require (quote [clojure.walk :as w])) (w/postwalk-replace {1 :one} [1 2 1]))"]
  ["keywordize-keys still works" "{:a 1}"
   "(do (require (quote [clojure.walk :as w])) (w/keywordize-keys {\"a\" 1}))"]
  # clojure.template/apply-template (the real-world trigger) substitutes now
  ["apply-template substitutes" "(quote (+ 1 2))"
   "(do (require (quote [clojure.template :as t])) (t/apply-template (quote [x y]) (quote (+ x y)) (quote (1 2))))"])
