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
  ["ratio not supported but reads ints" "3" "3"]
  ["hex literal"        "255"       "0xff"]
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
  ["tagged literal var" "true"      "(var? #'+)"]
  ["deref sugar"        "5"         "(let [a (atom 5)] @a)"]
  ["meta sugar"         "{:t 1}"    "(meta ^{:t 1} [])"])
