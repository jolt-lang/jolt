# Specification: functions & higher-order combinators.
(use ../support/harness)

(defspec "functions / definition"
  ["fn literal"         "3"      "((fn [a b] (+ a b)) 1 2)"]
  ["fn shorthand"       "3"      "(#(+ %1 %2) 1 2)"]
  ["fn shorthand %"     "2"      "(#(inc %) 1)"]
  ["defn"               "5"      "(do (defn f [x] (+ x 2)) (f 3))"]
  ["multi-arity"        "[1 5]"  "(do (defn f ([x] x) ([x y] (+ x y))) [(f 1) (f 2 3)])"]
  ["variadic"           "[1 2 3]" "(do (defn f [& xs] xs) (f 1 2 3))"]
  ["variadic with fixed" "[1 [2 3]]" "(do (defn f [a & xs] [a xs]) (f 1 2 3))"]
  ["closure captures"   "8"      "(do (defn adder [n] (fn [x] (+ x n))) ((adder 5) 3))"]
  ["recursion"          "120"    "(do (defn fact [n] (if (< n 2) 1 (* n (fact (dec n))))) (fact 5))"]
  ["named fn self-ref"  "120"    "((fn fact [n] (if (< n 2) 1 (* n (fact (dec n))))) 5)"])

(defspec "functions / application"
  ["apply"              "6"      "(apply + [1 2 3])"]
  ["apply with leading" "10"     "(apply + 1 2 [3 4])"]
  ["apply keyword"      "1"      "(apply :a [{:a 1}])"]
  ["partial"            "7"      "((partial + 5) 2)"]
  ["partial multi"      "10"     "((partial + 1 2) 3 4)"]
  ["comp"               "4"      "((comp inc inc) 2)"]
  ["comp order"         "5"      "((comp inc (fn [x] (* x 2))) 2)"]
  ["comp identity"      "3"      "((comp) 3)"]
  ["complement"         "true"   "((complement even?) 3)"]
  ["constantly"         "5"      "((constantly 5) 1 2 3)"]
  ["identity"           "7"      "(identity 7)"])

(defspec "functions / combinators"
  ["juxt"               "[1 3]"  "((juxt first last) [1 2 3])"]
  ["fnil"               "1"      "((fnil inc 0) nil)"]
  ["fnil passes value"  "6"      "((fnil inc 0) 5)"]
  ["every-pred true"    "true"   "((every-pred pos? even?) 4)"]
  ["every-pred false"   "false"  "((every-pred pos? even?) 3)"]
  ["some-fn"            "true"   "((some-fn even? neg?) 3 4)"]
  ["memoize"            "2"      "(do (def c (atom 0)) (def f (memoize (fn [x] (swap! c inc) x))) (f 1) (f 1) (f 2) @c)"]
  ["trampoline"         "10"     "(trampoline (fn f [n acc] (if (zero? n) acc (fn [] (f (dec n) (+ acc 2))))) 5 0)"])

# Phase 2 leaf batch (jolt-ded): moved from the Janet seed to 20-coll.clj.
(defspec "clojure.core / leaf batch (complement fnil munge etc.)"
  ["complement true"     "true"     "((complement pos?) -1)"]
  ["complement false"    "false"    "((complement pos?) 1)"]
  ["complement multi"    "true"     "((complement <) 3 2)"]
  ["fnil patches nil"    "1"        "((fnil inc 0) nil)"]
  ["fnil passes non-nil" "6"        "((fnil inc 0) 5)"]
  ["fnil two defaults"   "8"        "((fnil + 1 2) nil nil 5)"]
  ["fnil only first 3"   "[:a :b :c nil]" "((fnil vector :a :b :c) nil nil nil nil)"]
  ["fnil in update"      "{:k 1}"   "(update {} :k (fnil inc 0))"]
  ["clojure-version"     "true"     "(string? (clojure-version))"]
  ["bigdec"              "3"        "(bigdec 3)"]
  ["numerator throws"    :throws    "(numerator 1)"]
  ["denominator throws"  :throws    "(denominator 1)"]
  ["supers empty set"    "#{}"      "(supers 1)"]
  ["munge dashes"        "\"a_b\""  "(munge \"a-b\")"]
  ["munge symbol"        "\"x_y\""  "(munge (quote x-y))"]
  ["test no-test"        ":no-test" "(test (quote foo))"])
