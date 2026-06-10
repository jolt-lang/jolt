# Specification: printing / output (print/println/pr/prn, *-str, format, str).
# Output is captured with with-out-str (jolt-rfw); the *-str fns return strings.
(use ../support/harness)

(defspec "io / with-out-str captures"
  ["println"         "\"hi\\n\""   "(with-out-str (println \"hi\"))"]
  ["print spaces"    "\"a b\""     "(with-out-str (print \"a\" \"b\"))"]
  ["prn quotes"      "\"[1 2]\\n\"" "(with-out-str (prn [1 2]))"]
  ["pr no newline"   "\"5\""       "(with-out-str (pr 5))"]
  ["multiple writes" "\"12\""      "(with-out-str (print 1) (print 2))"]
  ["no output"       "\"\""        "(with-out-str 42)"]
  ["println no args" "\"\\n\""     "(with-out-str (println))"])

(defspec "io / *-str builders"
  ["print-str"       "\"a b\""     "(print-str \"a\" \"b\")"]
  ["println-str"     "\"x\\n\""    "(println-str \"x\")"]
  ["prn-str"         "\"[1 2]\\n\"" "(prn-str [1 2])"]
  ["pr-str quotes"   "\"\\\"s\\\"\"" "(pr-str \"s\")"]
  ["pr-str keyword"  "\":a\""      "(pr-str :a)"])

(defspec "io / str & format"
  ["str concat"      "\"1:ab\""    "(str 1 :a \"b\")"]
  ["str nil"         "\"\""        "(str nil)"]
  ["str of coll"     "\"[1 2]\""   "(str [1 2])"]
  ["format d/s"      "\"5-x\""     "(format \"%d-%s\" 5 \"x\")"]
  ["format float"    "\"3.14\""    "(format \"%.2f\" 3.14159)"])

# The *in* reader family (jolt-0d9): *in* is a dynamic var holding a reader;
# with-in-str rebinds it to a string reader over one shared buffer, so read
# (consumes exactly one form) and read-line (rest of that line) interleave
# correctly, as in Clojure.
(defspec "io / *in* + with-in-str + read-line"
  ["read-line one line"   "\"hello\""        "(with-in-str \"hello\" (read-line))"]
  ["read-line strips nl"  "\"a\""            "(with-in-str \"a\\nb\" (read-line))"]
  ["read-line sequential" "[\"a\" \"b\"]"    "(with-in-str \"a\\nb\" [(read-line) (read-line)])"]
  ["read-line EOF nil"    "nil"              "(with-in-str \"\" (read-line))"]
  ["read-line after last" "[\"x\" nil]"      "(with-in-str \"x\" [(read-line) (read-line)])"]
  ["empty line"           "[\"\" \"y\"]"     "(with-in-str \"\\ny\" [(read-line) (read-line)])"]
  ["*in* is bound"        "true"             "(with-in-str \"\" (map? *in*))"])

(defspec "io / read"
  ["read a form"          "42"               "(with-in-str \"42\" (read))"]
  ["read a list form"     "(quote (+ 1 2))"  "(with-in-str \"(+ 1 2)\" (read))"]
  ["read two forms"       "[1 2]"            "(with-in-str \"1 2\" [(read) (read)])"]
  ["read then read-line"  "[1 \" rest\"]"    "(with-in-str \"1 rest\\nnext\" [(read) (read-line)])"]
  ["read vector"          "[1 2]"            "(with-in-str \"[1 2]\" (read))"]
  ["read nil literal"     "nil"              "(with-in-str \"nil\" (read))"]
  ["read EOF throws"      :throws            "(with-in-str \"\" (read))"]
  ["read EOF value"       ":done"            "(with-in-str \"\" (read *in* false :done))"]
  ["read eval data"       "3"                "(with-in-str \"(+ 1 2)\" (eval (read)))"])

(defspec "io / line-seq"
  ["line-seq"             "[\"a\" \"b\" \"c\"]" "(with-in-str \"a\\nb\\nc\" (vec (line-seq *in*)))"]
  ["line-seq empty"       "nil"              "(with-in-str \"\" (seq (line-seq *in*)))"]
  ["line-seq is lazy seq" "true"             "(with-in-str \"a\\nb\" (seq? (line-seq *in*)))"]
  ["line-seq count"       "3"                "(with-in-str \"1\\n2\\n3\" (count (line-seq *in*)))"])
