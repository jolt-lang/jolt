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

(defspec "string as a seqable of chars (jolt-dl4s)"
  ["vec of string"       "[\\a \\b]" "(vec \"ab\")"]
  ["into [] of string"   "[\\a \\b]" "(into [] \"ab\")"]
  ["set of string"       "true"      "(= #{\\a \\b} (set \"ab\"))"]
  ["into #{} of string"  "true"      "(= #{\\a \\b} (into #{} \"ab\"))"]
  ["set dedups chars"    "2"         "(count (set \"aab\"))"]
  ["mapv over string"    "[\\a \\b]" "(mapv identity \"ab\")"])

(defspec "clojure.string / split limit (jolt-ik3a)"
  ["neg keeps trailing"  "[\"a\" \"\" \"\"]" "(do (require (quote [clojure.string :as s])) (s/split \"a,,\" #\",\" -1))"]
  ["zero trims trailing" "[\"a\"]"           "(do (require (quote [clojure.string :as s])) (s/split \"a,,\" #\",\" 0))"]
  ["omitted trims"       "[\"a\"]"           "(do (require (quote [clojure.string :as s])) (s/split \"a,,\" #\",\"))"]
  ["positive caps parts" "[\"a\" \"b,c\"]"   "(do (require (quote [clojure.string :as s])) (s/split \"a,b,c\" #\",\" 2))"]
  ["empty string"        "[\"\"]"            "(do (require (quote [clojure.string :as s])) (s/split \"\" #\",\"))"]
  ["interior empties kept" "[\"a\" \"\" \"b\"]" "(do (require (quote [clojure.string :as s])) (s/split \"a,,b\" #\",\"))"])

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

(defspec "string / namespace-munge"
  ["hyphens to underscores" "\"a_b_c\"" "(namespace-munge \"a-b-c\")"]
  ["from a symbol"          "\"foo_bar\"" "(namespace-munge (quote foo-bar))"]
  ["no hyphens unchanged"   "\"ok\""    "(namespace-munge \"ok\")"])

# (get s i) indexes a string and returns the char, like Clojure (nth already
# did; get did not — reitit's path parser relies on it).
(defspec "strings / get indexes a string"
  ["get returns the char"  "true"  "(= (get \"a:b\" 1) \\:)"]
  ["get first char"        "\\a"    "(get \"abc\" 0)"]
  ["get out of range nil"  "nil"   "(get \"abc\" 9)"]
  ["get negative nil"      "nil"   "(get \"abc\" -1)"]
  ["get default honored"   ":none" "(get \"abc\" 9 :none)"])

# clojure.string/trim-newline (ported from clojure.string): strips trailing \n/\r.
(defspec "clojure.string / trim-newline"
  ["trailing newline"   "\"x\""  "(do (require (quote [clojure.string :as s])) (s/trim-newline \"x\\n\"))"]
  ["trailing \\r\\n"      "\"x\""  "(do (require (quote [clojure.string :as s])) (s/trim-newline \"x\\r\\n\"))"]
  ["no trailing"        "\"ab\"" "(do (require (quote [clojure.string :as s])) (s/trim-newline \"ab\"))"]
  ["only newlines"      "\"\""   "(do (require (quote [clojure.string :as s])) (s/trim-newline \"\\n\\n\"))"]
  ["interior kept"      "\"a\\nb\"" "(do (require (quote [clojure.string :as s])) (s/trim-newline \"a\\nb\\n\"))"])
