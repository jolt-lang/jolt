# Specification: host (Janet) interop — the `.` forms and jolt.interop.
(use ../support/harness)

(defspec "interop / dot forms"
  ["method call"        "\"v=41\""
   "(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)"]
  ["method with args"   "\"Hello Alice\""
   "(. {:greet (fn [self n] (str \"Hello \" n))} greet \"Alice\")"]
  ["field access .-"    "41"        "(.-value {:value 41})"]
  ["dot field keyword"  "41"        "(. {:value 41} :value)"])

(defspec "interop / jolt.interop"
  ["janet-type quoted list" ":array" "(do (require (quote [jolt.interop :as j])) (j/janet-type (quote (1 2))))"]
  ["janet-type list"    ":array"    "(do (require (quote [jolt.interop :as j])) (j/janet-type (list 1 2)))"]
  ["janet-type string"  ":string"   "(do (require (quote [jolt.interop :as j])) (j/janet-type \"x\"))"]
  ["janet-type number"  ":number"   "(do (require (quote [jolt.interop :as j])) (j/janet-type 1))"]
  ["janet-type keyword" ":keyword"  "(do (require (quote [jolt.interop :as j])) (j/janet-type :a))"])
