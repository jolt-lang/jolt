# Specification: host (Janet) interop — the `.` forms and jolt.interop.
(use ../support/harness)

(defspec "interop / dot forms"
  ["method call"        "\"v=41\""
   "(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)"]
  ["method with args"   "\"Hello Alice\""
   "(. {:greet (fn [self n] (str \"Hello \" n))} greet \"Alice\")"]
  ["field access .-"    "41"        "(.-value {:value 41})"]
  ["dot field keyword"  "41"        "(. {:value 41} :value)"])

# The `janet` namespace segment is the explicit Janet-stdlib bridge added for
# the networking layer (and used by jolt.nrepl). `janet/<name>` resolves a Janet
# root binding; `janet.<module>/<name>` resolves a module binding. The boundary
# is explicit so it's visible where host semantics take over.
(defspec "interop / janet bridge"
  ["root builtin janet/<name>"   "\"123\"" "(janet/string 1 2 3)"]
  ["root builtin janet/type"     ":string" "(janet/type \"x\")"]
  ["module fn janet.<mod>/<name>" "4"      "(janet.math/sqrt 16)"]
  ["janet.string module fn"      "\"HI\""  "(janet.string/ascii-upper \"hi\")"]
  ["janet.os/clock is a number"  "true"    "(number? (janet.os/clock))"]
  # crossing the boundary uses Janet representations: a Jolt vector is a table
  ["jolt vector crosses as a janet table" ":table" "(janet/type [1 2])"]
  # interop is explicit-only: an unprefixed Janet module is not auto-exposed
  ["unprefixed janet module not exposed" :throws "net/server"]
  ["unknown janet symbol throws"         :throws "(janet.os/definitely-not-a-real-fn)"])

(defspec "interop / jolt.interop"
  ["janet-type quoted list" ":array" "(do (require (quote [jolt.interop :as j])) (j/janet-type (quote (1 2))))"]
  ["janet-type list"    ":array"    "(do (require (quote [jolt.interop :as j])) (j/janet-type (list 1 2)))"]
  ["janet-type string"  ":string"   "(do (require (quote [jolt.interop :as j])) (j/janet-type \"x\"))"]
  ["janet-type number"  ":number"   "(do (require (quote [jolt.interop :as j])) (j/janet-type 1))"]
  ["janet-type keyword" ":keyword"  "(do (require (quote [jolt.interop :as j])) (j/janet-type :a))"])

(defspec "interop / arrays (aget/aset/alength)"
  ["alength"            "3"      "(alength (object-array [1 2 3]))"]
  ["aget"               "20"     "(aget (object-array [10 20 30]) 1)"]
  ["aset returns val"   "9"      "(aset (object-array [1 2 3]) 1 9)"]
  ["aset mutates"       "[7 2 3]" "(let [a (object-array [1 2 3])] (aset a 0 7) (vec a))"]
  ["aget 2d"            "4"      "(aget (to-array-2d [[1 2] [3 4]]) 1 1)"])

# java.lang.String surface + .method sugar (clj-compat: what portable cljc
# libraries call — landed for the cuerdas acceptance run). ASCII case mapping.
(defspec "interop / String methods"
  [".toLowerCase"   "\"hi\""  "(.toLowerCase \"HI\")"]
  [".toUpperCase"   "\"HI\""  "(.toUpperCase \"hi\")"]
  ["dot-form"       "\"hi\""  "(. \"HI\" toLowerCase)"]
  [".trim"          "\"x\""   "(.trim \"  x  \")"]
  [".length"        "3"       "(.length \"abc\")"]
  [".isEmpty"       "[true false]" "[(.isEmpty \"\") (.isEmpty \"a\")]"]
  [".indexOf hit"   "1"       "(.indexOf \"abc\" \"b\")"]
  [".indexOf miss is -1" "-1" "(.indexOf \"abc\" \"z\")"]
  [".lastIndexOf"   "3"       "(.lastIndexOf \"abab\" \"b\")"]
  [".substring"     "\"bc\""  "(.substring \"abc\" 1)"]
  [".substring end" "\"b\""   "(.substring \"abc\" 1 2)"]
  [".startsWith"    "true"    "(.startsWith \"abc\" \"ab\")"]
  [".endsWith"      "true"    "(.endsWith \"abc\" \"bc\")"]
  [".contains"      "true"    "(.contains \"abc\" \"b\")"]
  [".replace"       "\"axc\"" "(.replace \"abc\" \"b\" \"x\")"]
  [".charAt"        "\\b"     "(.charAt \"abc\" 1)"]
  [".equalsIgnoreCase" "true" "(.equalsIgnoreCase \"AbC\" \"aBc\")"]
  ["Long/MAX_VALUE" "true"    "(pos? Long/MAX_VALUE)"]
  ["unsupported method throws" :throws "(.frobnicate \"abc\")"])
