# Specification: reader syntax & literals.
(use ../support/harness)

(defspec "reader / scalar literals"
  ["integer"            "42"        "42"]
  ["negative"           "-7"        "-7"]
  ["float"              "1.5"       "1.5"]
  ["string"             "\"hi\""    "\"hi\""]
  ["boolean true"       "true"      "true"]
  ["nil"                "nil"       "nil"]
  ["keyword"            ":a"        ":a"]
  ["namespaced keyword" "true"      "(= :a/b :a/b)"]
  ["char"               "\\a"       "\\a"]
  ["char newline"       "true"      "(= \\newline (first \"\\n\"))"]
  # single non-symbol chars are one-char literals (\{ \( \, \% etc.)
  ["char open-brace"    "123"       "(int \\{)"]
  ["char open-paren"    "40"        "(int \\()"]
  ["char comma"         "44"        "(int \\,)"]
  ["char percent"      "37"        "(int \\%)"]
  ["char unicode"       "65"        "(int \\u0041)"]
  ["hex literal"        "255"       "0xff"]
  ["hex uppercase"      "31"        "0X1F"]
  ["bigint suffix N"    "42"        "42N"]
  ["bigdec suffix M"    "1.5"       "1.5M"]
  ["ratio -> double"    "0.75"      "3/4"]
  ["radix integer"     "255"        "16rFF"]
  ["exponent"           "1500.0"    "1.5e3"]
  ["symbolic Infinity"  "true"      "(infinite? ##Inf)"]
  ["symbolic NaN"       "true"      "(NaN? ##NaN)"]
  ["symbol via quote"   "'foo"       "'foo"])

(defspec "reader / collection literals"
  ["vector"             "[1 2 3]"   "[1 2 3]"]
  ["list quoted"        "[1 2 3]"   "'(1 2 3)"]
  ["map"                "{:a 1}"    "{:a 1}"]
  ["set"                "#{1 2 3}"  "#{1 2 3}"]
  ["nested"             "{:a [1 {:b 2}]}" "{:a [1 {:b 2}]}"]
  ["empty vector"       "[]"        "[]"]
  ["empty map"          "{}"        "{}"]
  ["empty set"          "#{}"       "#{}"])

(defspec "reader / dispatch & sugar"
  ["anon fn #()"        "3"         "(#(+ %1 %2) 1 2)"]
  ["anon fn single %"   "2"        "(#(inc %) 1)"]
  ["anon fn %&"         "[2 3]"     "(#(vec %&) 2 3)"]
  ["discard #_"         "[1 3]"     "[1 #_2 3]"]
  ["regex literal"      "true"      "(= \"abc\" (re-find #\"abc\" \"xabcx\"))"]
  ["reader conditional" "1"         "#?(:clj 1 :cljs 2 :default 3)"]
  ["reader cond splice" "[1 2 3]"   "[#?@(:clj [1 2 3] :cljs [4 5])]"]
  ["inst literal reads" "true"      "(some? #inst \"2020-01-01T00:00:00Z\")"]
  ["uuid literal"       "\"550e8400-e29b-41d4-a716-446655440000\"" "(str #uuid \"550e8400-e29b-41d4-a716-446655440000\")"]
  ["tagged literal var" "true"      "(var? #'+)"]
  ["deref sugar"        "5"         "(let [a (atom 5)] @a)"]
  ["meta sugar"         "{:t 1}"    "(meta ^{:t 1} [])"])
