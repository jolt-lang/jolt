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

# The print family is overlay now (seed-shrink round 6), over the __write /
# __pr-str1 host seams: pr is readable, print is str semantics, *-ln appends.
(defspec "io / print family (overlay)"
  ["pr-str multi-arg spacing" "\"\\\"a\\\" [1 2] :k\"" "(pr-str \"a\" [1 2] :k)"]
  ["pr-str zero args"   "\"\""        "(pr-str)"]
  ["pr-str escapes"     "\"\\\"a\\\\\\\"b\\\"\"" "(pr-str \"a\\\"b\")"]
  ["print is unreadable" "\"a b\""    "(with-out-str (print \"a\" \"b\"))"]
  ["println appends newline" "\"x 1\\n\"" "(with-out-str (println \"x\" 1))"]
  ["prn is readable + newline" "\"[1 \\\"s\\\"]\\n\"" "(with-out-str (prn [1 \"s\"]))"]
  ["pr writes no newline" "\"\\\\a\"" "(with-out-str (pr \\a))"]
  ["print nil arg"      "\"nil\""     "(with-out-str (print nil))"]
  ["prn keyword"        "\":k\\n\""   "(with-out-str (prn :k))"])

# print-method is a real multimethod (jolt-g1r): canonical dispatch on
# (:type meta) keyword else (type x); records print as #ns.Type{...} by
# default, and a user (defmethod print-method 'ns.Type ...) overrides record
# rendering everywhere — top level AND nested, through pr/prn/pr-str — via
# the host renderer's callback. Builtin overrides apply only on direct
# print-method calls (documented divergence; pr keeps the native fast path).
(defspec "io / print-method multimethod"
  ["records print canonically" "\"#user.Pt{:x 1, :y 2}\""
   "(do (defrecord Pt [x y]) (pr-str (->Pt 1 2)))"]
  ["records nested in colls" "\"[#user.Pt{:x 1, :y 2}]\""
   "(do (defrecord Pt [x y]) (pr-str [(->Pt 1 2)]))"]
  ["defmethod overrides a record, top level" "\"<3,4>\""
   "(do (defrecord Pt [x y]) (defmethod print-method (quote user.Pt) [r w] (.write w (str \"<\" (:x r) \",\" (:y r) \">\"))) (pr-str (->Pt 3 4)))"]
  ["defmethod fires nested in a map" "\"{:p <5,6>}\""
   "(do (defrecord Pt [x y]) (defmethod print-method (quote user.Pt) [r w] (.write w (str \"<\" (:x r) \",\" (:y r) \">\"))) (pr-str {:p (->Pt 5 6)}))"]
  ["defmethod fires through prn" "\"[<1,2>]\\n\""
   "(do (defrecord Pt [x y]) (defmethod print-method (quote user.Pt) [r w] (.write w (str \"<\" (:x r) \",\" (:y r) \">\"))) (with-out-str (prn [(->Pt 1 2)])))"]
  ["direct call uses :default" "\"42\""
   "(let [w (StringWriter.)] (print-method 42 w) (.toString w))"]
  ["direct builtin override" "\"#42#\""
   "(do (defmethod print-method :number [n w] (.write w (str \"#\" n \"#\"))) (let [w (StringWriter.)] (print-method 42 w) (.toString w)))"]
  ["print-dup routes to print-method" "\"[1 2]\""
   "(let [w (StringWriter.)] (print-dup [1 2] w) (.toString w))"]
  ["StringWriter accumulates" "\"ab\""
   "(let [w (StringWriter.)] (.write w \"a\") (.append w \\b) (.toString w))"]
  ["methods table inspectable" "true"
   "(do (defrecord Pt [x y]) (defmethod print-method (quote user.Pt) [r w] r) (contains? (methods print-method) (quote user.Pt)))"])

# Cold tagged-type printing now lives in io-tier defmethods (the host
# renderer dispatches any remaining :jolt/* tagged value through the
# print-method hook). Outputs unchanged from the old host branches; any
# tagged type is now user-overridable, atoms included.
(defspec "io / cold tagged types via print-method"
  ["uuid"           "\"#uuid \\\"b6883c0a-0342-4007-9966-bc2dfa6b109e\\\"\""
   "(pr-str (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\"))"]
  ["uuid nested"    "\"[#uuid \\\"b6883c0a-0342-4007-9966-bc2dfa6b109e\\\"]\""
   "(pr-str [(parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")])"]
  ["regex"          "\"#\\\"a+b\\\"\""  "(pr-str #\"a+b\")"]
  ["transient vector" "\"#<transient vector>\"" "(pr-str (transient [1]))"]
  ["transient map"  "\"#<transient map>\""      "(pr-str (transient {:a 1}))"]
  ["atom override fires nested" "\"{:a #atom[7]}\""
   "(do (defmethod print-method :jolt/atom [a w] (.write w (str \"#atom[\" (deref a) \"]\"))) (pr-str {:a (atom 7)}))"]
  ["uuid through str unchanged" "\"b6883c0a-0342-4007-9966-bc2dfa6b109e\""
   "(str (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\"))"])
