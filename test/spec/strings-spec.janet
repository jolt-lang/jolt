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
  ["pr-str vector"      "\"[1 2 3]\"" "(pr-str [1 2 3])"]
  ["pr-str quotes str"  "\"\\\"hi\\\"\"" "(pr-str \"hi\")"]
  # a var's :meta/:ns refs are cyclic — pr-str/str render it as #'ns/name
  # rather than recursing into (and looping on) the var's fields.
  ["pr-str of a var"    "\"#'user/vv\"" "(pr-str (def vv 1))"]
  ["str of a var"       "\"#'user/ww\"" "(str (def ww 2))"]
  ["pr-str of a defn"   "\"#'user/gg\"" "(pr-str (defn gg [x] x))"]
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

# subs validates bounds like Clojure (no Janet from-end/clamping).
(defspec "string / subs strictness"
  ["subs basic"         "\"bcd\""  "(subs \"abcde\" 1 4)"]
  ["subs to end"        "\"cde\""  "(subs \"abcde\" 2)"]
  ["subs start>end"     :throws    "(subs \"abcde\" 2 1)"]
  ["subs negative"      :throws    "(subs \"abcde\" -1)"]
  ["subs end past len"  :throws    "(subs \"abcde\" 1 6)"]
  ["subs nil start"     :throws    "(subs \"abcde\" nil 2)"]
  ["subs on nil"        :throws    "(subs nil 1 2)"])
