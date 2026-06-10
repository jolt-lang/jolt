# Specification: core special forms (case/fn/let/letfn/loop/if/do/def/call).
#
# Adapted from the jank test corpus (compiler+runtime/test/jank/form/**, /call) —
# we base our coverage on jank's to close gaps, but maintain our own copies since
# jank may diverge. jank-isms are translated to Jolt/Clojure: letfn* -> letfn,
# (catch jank.runtime.object_ref …) -> (catch :default …). Platform-specific cases
# (bigdecimal M, biginteger, ratios, unicode char edges) are intentionally omitted.
#
# Multi-form jank files (def + asserts, ending in :success) are wrapped in a single
# (do … :success) so they run as one expression and assert :success.
(use ../support/harness)

(defspec "forms / case"
  ["bool"            ":yes"   "(case true true :yes false :no :default)"]
  ["keyword match"   ":b"     "(case :a :x :wrong :a :b :default)"]
  ["number match"    ":two"   "(case 2 1 :one 2 :two :default)"]
  ["string match"    ":hit"   "(case \"x\" \"y\" :miss \"x\" :hit :default)"]
  ["nil match"       ":nada"  "(case nil nil :nada :default)"]
  ["default"         ":def"   "(case 99 1 :one 2 :two :def)"]
  ["list of consts"  ":vowel" "(case \\a (\\a \\e \\i \\o \\u) :vowel :consonant)"]
  ["no match no default" :throws "(case 5 1 :one)"]
  ["duplicate keys"  :throws  "(case 1 1 :one 1 :dup :default)"]
  ["duplicate in or-group" :throws "(case 2 (1 2) :a (2 3) :b :default)"])

(defspec "forms / fn"
  ["named fn nil"    "nil"    "((fn* foo-bar []))"]
  ["immediate call"  "1"      "((fn* [] 1))"]
  ["args"            "[:a :b]" "((fn* [a b] [a b]) :a :b)"]
  ["multi-arity 0"   "0"      "(do (def add (fn* ([] 0) ([a] a) ([a b] (+ a b)))) (add))"]
  ["multi-arity 1"   "-500"   "(do (def add (fn* ([] 0) ([a] a) ([a b] (+ a b)))) (add -500))"]
  ["multi-arity 2"   "-450"   "(do (def add (fn* ([] 0) ([a] a) ([a b] (+ a b)))) (add -500 50))"]
  ["variadic rest"   "[3 4]"  "(do (def v (fn* ([a b & args] args) ([] 0))) (v 1 2 3 4))"]
  ["variadic empty"  "0"      "(do (def v (fn* ([a b & args] args) ([] 0))) (v))"]
  ["variadic collect" "[{} nil :m]" "((fn* [a b & args] args) 'w 't {} nil :m)"]
  ["closure capture" "8"      "(do (def adder (fn* [n] (fn* [x] (+ x n)))) ((adder 5) 3))"]
  ["recur countdown" "0"      "(do (def cd (fn* [n] (if (< 0 n) (recur (+ n -1)) n))) (cd 10))"]
  ["named self-recur" "120"   "(do (def f (fn* fact [n] (if (= n 0) 1 (* n (fact (dec n)))))) (f 5))"]
  ["no param vector" :throws  "(fn* foo)"]
  ["non-symbol param" :throws "(fn* [1] 1)"])

(defspec "forms / let"
  ["literal"         "1"      "(let* [a 1] a)"]
  ["multiple"        "[1 2]"  "(let* [a 1 b 2] [a b])"]
  ["previous ref"    ":bee"   "(let* [a 1 b (if (= 1 a) :bee :uh-oh)] b)"]
  ["nested let"      "3"      "(let* [a 5 b (let* [c -2] (+ a c))] b)"]
  ["fn value bound"  "\":foo\"" "(let* [kw->str (fn* [kw] (str kw))] (kw->str :foo))"]
  ["shadowing"       "2"      "(let* [a 1 a 2] a)"])

(defspec "forms / letfn"
  ["mutual top"      "[1 2]"  "(letfn [(a [] 1) (b [] 2)] [(a) (b)])"]
  ["mutual recursion" ":done" "(letfn [(ev? [n] (if (= 0 n) :done (od? (dec n)))) (od? [n] (ev? n))] (ev? 4))"]
  ["nested letfn"    "3"      "(letfn [(a [] 5) (b [] (letfn [(c [] -2)] (+ (a) (c))))] (b))"])

(defspec "forms / loop"
  ["sum"             "55"     "(loop* [sum 0 cnt 10] (if (= cnt 0) sum (recur (+ cnt sum) (dec cnt))))"]
  ["multi binding"   "[4 2]"  "(loop* [a 1 b 2 n 0] (if (< n 3) (recur (inc a) b (inc n)) [a b]))"]
  ["init sees prior" "[1 2 3]" "(loop* [a 1 b (+ a 1) c (+ b 1)] [a b c])"])

(defspec "forms / try"
  ["immediate throw caught" ":caught" "(try (throw :boom) (catch :default e :caught))"]
  ["first throw wins"   "\"a\""  "(try (throw (ex-info \"a\" {})) (throw (ex-info \"b\" {})) (catch :default e (ex-message e)))"]
  ["catch ex-data"      "7"      "(try (throw (ex-info \"e\" {:v 7})) (catch :default e (:v (ex-data e))))"]
  ["finally runs"       "9"      "(let [a (atom 0)] (try 1 (finally (reset! a 9))) @a)"]
  ["body value w/ finally" "1"   "(try 1 (finally 2))"]
  ["catch value w/ finally" ":h" "(try (throw (ex-info \"x\" {})) (catch :default e :h) (finally :ignored))"]
  ["no throw skips catch" "5"    "(try 5 (catch :default e :nope))"])

(defspec "forms / if-do-def-call"
  ["if truthy vec"   ":fine"  "(if [:ok] :fine :no)"]
  ["if truthy str"   ":fine"  "(if \"good?\" :fine :no)"]
  ["if nil = false"  ":else"  "(if nil :then :else)"]
  ["if no else"      "nil"    "(if false 1)"]
  ["do nested"       "1"      "(do (do (do (do 1))))"]
  ["do returns last" "3"      "(do 1 2 3)"]
  ["def + deref var" "true"   "(var? (def one 1))"]
  ["def redefine"    "100"    "(do (def one 1) (def one 100) one)"]
  ["def in fn mutates" "[:default :meow]" "(do (def a :default) (def set-a (fn* [v] (def a v))) (let* [before a] (set-a :meow) [before a]))"]
  ["call literal fn" "1"      "((fn* [] 1))"]
  ["call nested"     "6"      "(+ ((fn* [] 1)) ((fn* [] 2)) ((fn* [] 3)))"]
  ["call nil"        :throws  "(nil)"])
