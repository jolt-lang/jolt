# Specification: keyword-invoke and map-literal semantics that the compiled
# fast paths (jolt-4vr: inlined keyword lookup, inlined struct construction)
# must preserve. Written BEFORE the optimization; every row passes on the
# generic jolt-call/build-map-literal paths and must keep passing on the
# inlined ones.
(use ../support/harness)

(defspec "maps / keyword invoke"
  ["hit"                 "1"     "(:a {:a 1 :b 2})"]
  ["miss"                "nil"   "(:z {:a 1})"]
  ["miss with default"   ":d"    "(:z {:a 1} :d)"]
  ["hit with default"    "1"     "(:a {:a 1} :d)"]
  ["on nil"              "nil"   "(:a nil)"]
  ["on nil with default" ":d"    "(:a nil :d)"]
  ["nil value is present" "nil"  "(:a {:a nil} :d)"]
  ["false value is present" "false" "(:a {:a false} :d)"]
  ["on a vector"         "nil"   "(:a [1 2 3])"]
  ["on a number"         "nil"   "(:a 42)"]
  ["on a sorted map"     "2"     "(:b (sorted-map :a 1 :b 2))"]
  ["on assoc result"     "3"     "(:c (assoc {:a 1} :c 3))"]
  ["on a record field"   "5"     "(do (defrecord KFP [x]) (:x (->KFP 5)))"]
  ["qualified keyword"   "1"     "(:n/a {:n/a 1})"]
  ["nested in expr"      "6"     "(+ (:a {:a 1}) (:b {:b 2}) (:c {:c 3}))"]
  ["evaluates map expr once" "[2 1]"
   "(do (def cnt (atom 0)) (let [v (:a (do (swap! cnt inc) {:a 2}))] [v @cnt]))"])

(defspec "maps / literal construction"
  ["basic"            "{:a 1, :b 2}"  "{:a 1 :b 2}"]
  ["empty"            "{}"            "{}"]
  ["computed values"  "{:a 3}"        "{:a (+ 1 2)}"]
  ["nil value kept"   "true"          "(contains? {:a nil} :a)"]
  ["nil value lookup" "nil"           "(get {:a nil} :a :d)"]
  ["string key"       "1"             "(get {\"k\" 1} \"k\")"]
  ["number key"       ":one"          "(get {1 :one} 1)"]
  ["collection key"   ":v"            "(get {[1 2] :v} [1 2])"]
  ["collection value-equal key" ":v"  "(get {[1 2] :v} (vector 1 2))"]
  ["computed key"     "1"             "(get {(keyword \"a\") 1} :a)"]
  # NOTE: entry ORDER is reader-hash order today, not source order (Clojure
  # evaluates array-map literals left-to-right) — pinned loosely; see jolt-p3c
  ["values evaluate exactly once each" "[1 2 3]"
   "(do (def log (atom [])) {:a (swap! log conj 1) :b (swap! log conj 2) :c (swap! log conj 3)} (vec (sort (deref log))))"]
  ["count"            "3"             "(count {:a 1 :b 2 :c 3})"]
  ["equality with phm" "true"         "(= {:a 1 :b 2} (assoc {:a 1} :b 2))"]
  ["keys work after assoc" "2"        "(:b (assoc {:a 1 :b 2} :c 3))"]
  ["literal in fn body"  "12"         "(do (defn mfp-mk [x] {:v (* x 2)}) (:v (mfp-mk 6)))"])

(defspec "clojure.math (jolt-h79)"
  ["sqrt"        "true"  "(< 1.4142 (clojure.math/sqrt 2) 1.4143)"]
  ["pow"         "1024"  "(long (clojure.math/pow 2 10))"]
  ["tan of 0"    "0"     "(long (clojure.math/tan 0))"]
  ["round"       "3"                "(clojure.math/round 2.6)"]
  ["floor"       "2"                "(clojure.math/floor 2.9)"]
  ["signum"      "-1"               "(clojure.math/signum -7.2)"]
  ["to-radians"  "true"             "(< 3.14 (clojure.math/to-radians 180) 3.15)"]
  ["PI"          "true"             "(< 3.14 clojure.math/PI 3.15)"]
  ["require + alias" "5"            "(do (require '[clojure.math :as m]) (long (m/hypot 3 4)))"]
  ["as a value"  "[1 2]"            "(mapv (comp long clojure.math/sqrt) [1 4])"])
