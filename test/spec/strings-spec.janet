# Specification: strings (str + clojure.string).
(use ../support/harness)

(defspec "string / str & basics"
  ["str concat"         "\"abc\""    "(str \"a\" \"b\" \"c\")"]
  ["str of numbers"     "\"12\""     "(str 1 2)"]
  ["str nil is empty"   "\"\""       "(str nil)"]
  ["str mixed"          "\"a1:b\""   "(str \"a\" 1 :b)"]
  ["str of coll"        "\"[1 2]\""  "(str [1 2])"]
  ["count"              "3"          "(count \"abc\")"]
  ["subs from"          "\"bc\""     "(subs \"abc\" 1)"]
  ["subs range"         "\"b\""      "(subs \"abc\" 1 2)"]
  ["string? true"       "true"       "(string? \"x\")"]
  ["seq of string"      "[\\a \\b]"  "(seq \"ab\")"])

(defspec "clojure.string"
  ["join"               "\"a,b,c\""  "(do (require (quote [clojure.string :as s])) (s/join \",\" [\"a\" \"b\" \"c\"]))"]
  ["join no sep"        "\"abc\""    "(do (require (quote [clojure.string :as s])) (s/join [\"a\" \"b\" \"c\"]))"]
  ["split"              "[\"a\" \"b\"]" "(do (require (quote [clojure.string :as s])) (s/split \"a,b\" #\",\"))"]
  ["split-lines"        "[\"a\" \"b\"]" "(do (require (quote [clojure.string :as s])) (s/split-lines \"a\\nb\"))"]
  ["upper-case"         "\"ABC\""    "(do (require (quote [clojure.string :as s])) (s/upper-case \"abc\"))"]
  ["lower-case"         "\"abc\""    "(do (require (quote [clojure.string :as s])) (s/lower-case \"ABC\"))"]
  ["capitalize"         "\"Abc\""    "(do (require (quote [clojure.string :as s])) (s/capitalize \"abc\"))"]
  ["trim"               "\"x\""      "(do (require (quote [clojure.string :as s])) (s/trim \"  x  \"))"]
  ["triml"              "\"x  \""    "(do (require (quote [clojure.string :as s])) (s/triml \"  x  \"))"]
  ["blank? true"        "true"       "(do (require (quote [clojure.string :as s])) (s/blank? \"   \"))"]
  ["blank? false"       "false"      "(do (require (quote [clojure.string :as s])) (s/blank? \"x\"))"]
  ["includes?"          "true"       "(do (require (quote [clojure.string :as s])) (s/includes? \"hello\" \"ell\"))"]
  ["starts-with?"       "true"       "(do (require (quote [clojure.string :as s])) (s/starts-with? \"hello\" \"he\"))"]
  ["ends-with?"         "true"       "(do (require (quote [clojure.string :as s])) (s/ends-with? \"hello\" \"lo\"))"]
  ["replace"            "\"hexxo\""  "(do (require (quote [clojure.string :as s])) (s/replace \"hello\" \"l\" \"x\"))"]
  ["reverse"            "\"cba\""    "(do (require (quote [clojure.string :as s])) (s/reverse \"abc\"))"]
  ["index-of"           "2"          "(do (require (quote [clojure.string :as s])) (s/index-of \"hello\" \"l\"))"])
