# Specification: numbers & arithmetic.
(use ../support/harness)

(defspec "numbers / arithmetic"
  ["add"                "6"      "(+ 1 2 3)"]
  ["add zero args"      "0"      "(+)"]
  ["subtract"           "5"      "(- 10 3 2)"]
  ["negate"             "-5"     "(- 5)"]
  ["multiply"           "24"     "(* 2 3 4)"]
  ["multiply zero args" "1"      "(*)"]
  ["divide"             "2"      "(/ 10 5)"]
  ["divide to fraction" "0.5"    "(/ 1 2)"]
  ["inc"                "6"      "(inc 5)"]
  ["dec"                "4"      "(dec 5)"]
  ["quot"               "3"      "(quot 10 3)"]
  ["rem"                "1"      "(rem 10 3)"]
  ["mod"                "2"      "(mod -1 3)"]
  ["rem negative"       "-1"     "(rem -1 3)"]
  ["max"                "9"      "(max 3 9 1)"]
  ["min"                "1"      "(min 3 9 1)"]
  ["abs"                "5"      "(abs -5)"]
  ["promoting + alias"  "3"      "(+' 1 2)"]
  ["inc' alias"         "6"      "(inc' 5)"])

(defspec "numbers / comparison"
  ["less than"          "true"   "(< 1 2 3)"]
  ["less than false"    "false"  "(< 1 3 2)"]
  ["greater than"       "true"   "(> 3 2 1)"]
  ["<="                 "true"   "(<= 1 1 2)"]
  [">="                 "true"   "(>= 3 3 2)"]
  ["= numbers"          "true"   "(= 2 2)"]
  ["= different"        "false"  "(= 2 3)"]
  ["== numeric"         "true"   "(== 2 2)"]
  ["not="               "true"   "(not= 1 2)"]
  ["compare less"       "-1"     "(compare 1 2)"]
  ["compare equal"      "0"      "(compare 1 1)"]
  ["compare greater"    "1"      "(compare 2 1)"])

(defspec "numbers / predicates"
  ["zero?"              "true"   "(zero? 0)"]
  ["pos?"               "true"   "(pos? 5)"]
  ["neg?"               "true"   "(neg? -5)"]
  ["even?"              "true"   "(even? 4)"]
  ["odd?"               "true"   "(odd? 3)"]
  ["number?"            "true"   "(number? 5)"]
  ["number? false"      "false"  "(number? :a)"]
  ["int?"               "true"   "(int? 5)"]
  ["pos-int?"           "true"   "(pos-int? 5)"]
  ["neg-int?"           "true"   "(neg-int? -5)"]
  ["nat-int? zero"      "true"   "(nat-int? 0)"]
  ["nat-int? neg"       "false"  "(nat-int? -1)"]
  ["ratio? false"       "false"  "(ratio? 5)"])

# Symbolic float values and float/double predicates. NOTE: Janet represents
# integers and integer-valued doubles identically, so (float? 1.0) is false
# (1.0 is indistinguishable from 1) — a documented divergence. Fractional and
# non-finite values ARE recognized as floats.
(defspec "numbers / floats & symbolic values"
  ["read ##Inf"         "true"   "(= ##Inf (/ 1.0 0.0))"]
  ["read ##-Inf"        "true"   "(< ##-Inf 0)"]
  ["##NaN not= itself"  "true"   "(not (== ##NaN ##NaN))"]
  ["float? fractional"  "true"   "(float? 1.5)"]
  ["double? fractional" "true"   "(double? 0.25)"]
  ["float? integer"     "false"  "(float? 3)"]
  ["float? ##Inf"       "true"   "(float? ##Inf)"]
  ["double? ##NaN"      "true"   "(double? ##NaN)"]
  ["infinite? ##Inf"    "true"   "(infinite? ##Inf)"]
  ["infinite? ##-Inf"   "true"   "(infinite? ##-Inf)"]
  ["infinite? finite"   "false"  "(infinite? 1.5)"]
  ["NaN? ##NaN"         "true"   "(NaN? ##NaN)"]
  ["NaN? number"        "false"  "(NaN? 1.0)"]
  ["int? ##Inf false"   "false"  "(int? ##Inf)"]
  ["pos-int? ##Inf"     "false"  "(pos-int? ##Inf)"])

# Numeric literal syntaxes. Jolt has no true bignum/ratio/bigdec types, so the
# N (bigint) and M (bigdec) suffixes read as the plain number, ratios as the
# double quotient; radix integers (NrDDD) are parsed by base.
(defspec "numbers / literal syntax"
  ["bigint suffix N"    "42"     "42N"]
  ["bigint zero"        "0"      "0N"]
  ["bigdec suffix M"    "1.5"    "1.5M"]
  ["bigdec int M"       "0"      "0.0M"]
  ["ratio -> double"    "0.5"    "1/2"]
  ["ratio 3/4"          "0.75"   "3/4"]
  ["neg ratio"          "-0.5"   "-1/2"]
  ["radix binary"       "10"     "2r1010"]
  ["radix hex-ish"      "255"    "16rFF"]
  ["radix base36"       "35"     "36rZ"]
  ["hex"                "255"    "0xFF"]
  ["exponent"           "1000.0" "1e3"]
  ["exponent neg"       "0.015"  "1.5e-2"])

# Strictness: numeric ops reject non-numbers like Clojure; the integer
# predicates reject non-integers; quot/rem/mod reject zero/non-finite.
(defspec "numbers / strictness (throws like Clojure)"
  ["odd? nil"           :throws  "(odd? nil)"]
  ["odd? fractional"    :throws  "(odd? 1.5)"]
  ["even? inf"          :throws  "(even? ##Inf)"]
  ["zero? nil"          :throws  "(zero? nil)"]
  ["pos? false"         :throws  "(pos? false)"]
  ["neg? keyword"       :throws  "(neg? :a)"]
  ["< nil"              :throws  "(< nil 1)"]
  ["> with nil"         :throws  "(> 1 nil)"]
  ["max non-number"     :throws  "(max 1 nil)"]
  ["quot by zero"       :throws  "(quot 10 0)"]
  ["quot inf"           :throws  "(quot ##Inf 1)"]
  ["< arity-1 any"      "true"   "(< :anything)"]
  ["odd? ok"            "true"   "(odd? 3)"]
  ["< ok"               "true"   "(< 1 2 3)"]
  ["quot ok"            "3"      "(quot 10 3)"])

(defspec "numbers / printing of inf & nan"
  ["str Infinity"       "\"Infinity\""  "(str ##Inf)"]
  ["str -Infinity"      "\"-Infinity\"" "(str ##-Inf)"]
  ["str NaN"            "\"NaN\""       "(str ##NaN)"]
  ["pr-str Infinity"    "\"Infinity\""  "(pr-str ##Inf)"]
  ["inf inside coll"    "\"[Infinity]\"" "(str [##Inf])"])

(defspec "numbers / bit-ops & math"
  ["bit-and"            "4"      "(bit-and 12 6)"]
  ["bit-or"             "14"     "(bit-or 12 6)"]
  ["bit-xor"            "10"     "(bit-xor 12 6)"]
  ["bit-shift-left"     "8"      "(bit-shift-left 1 3)"]
  ["bit-shift-right"    "2"      "(bit-shift-right 8 2)"]
  ["bit-set"            "8"      "(bit-set 0 3)"]
  ["bit-clear"          "13"     "(bit-clear 15 1)"]
  ["bit-test true"      "true"   "(bit-test 4 2)"]
  ["bigint 64-bit"      "\"9000000000\"" "(str (bigint 9000000000))"])

(defspec "numbers / random (invariants — non-deterministic)"
  ["rand-int in range"  "true" "(let [r (rand-int 5)] (and (integer? r) (>= r 0) (< r 5)))"]
  ["rand-int zero"      "0"    "(rand-int 1)"]
  ["rand in [0,1)"      "true" "(let [r (rand)] (and (>= r 0) (< r 1)))"]
  ["rand n in [0,n)"    "true" "(let [r (rand 10)] (and (>= r 0) (< r 10)))"]
  ["rand-nth member"    "true" "(contains? #{:a :b :c} (rand-nth [:a :b :c]))"]
  ["rand-nth single"    ":x"   "(rand-nth [:x])"])

# Clojure 1.11 string->scalar parsers: nil on malformed, throw on non-string.
(defspec "numbers / parse fns (1.11)"
  ["parse-long"          "42"     "(parse-long \"42\")"]
  ["parse-long negative" "-7"     "(parse-long \"-7\")"]
  ["parse-long plus"     "7"      "(parse-long \"+7\")"]
  ["parse-long float nil" "nil"   "(parse-long \"1.5\")"]
  ["parse-long hex nil"  "nil"    "(parse-long \"0x10\")"]
  ["parse-long empty nil" "nil"   "(parse-long \"\")"]
  ["parse-long junk nil" "nil"    "(parse-long \"12ab\")"]
  ["parse-long throws"   :throws  "(parse-long 42)"]
  ["parse-double"        "1.5"    "(parse-double \"1.5\")"]
  ["parse-double int"    "4.0"    "(parse-double \"4\")"]
  ["parse-double sci"    "1500.0" "(parse-double \"1.5e3\")"]
  ["parse-double neg"    "-0.5"   "(parse-double \"-0.5\")"]
  ["parse-double junk"   "nil"    "(parse-double \"abc\")"]
  ["parse-double trail"  "nil"    "(parse-double \"1.5x\")"]
  ["parse-double throws" :throws  "(parse-double :k)"]
  ["parse-boolean true"  "true"   "(parse-boolean \"true\")"]
  ["parse-boolean false" "false"  "(parse-boolean \"false\")"]
  ["parse-boolean case"  "nil"    "(parse-boolean \"True\")"]
  ["parse-boolean junk"  "nil"    "(parse-boolean \"yes\")"]
  ["parse-boolean throws" :throws "(parse-boolean true)"])
