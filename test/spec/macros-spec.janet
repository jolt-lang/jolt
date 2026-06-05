# Specification: macros, quoting and syntax-quote.
(use ../support/harness)

(defspec "macros / quoting"
  ["quote symbol"       "(quote a)"         "(quote a)"]
  ["quote list"         "[1 2 3]"     "(quote (1 2 3))"]
  ["quote nested"       "[1 [2 3]]"   "(quote (1 (2 3)))"]
  ["quote sugar"        "'a"          "'a"]
  ["syntax-quote literal" "[1 2]"     "`[1 2]"]
  ["unquote"            "[1 2 3]"     "(let [x 2] `[1 ~x 3])"]
  ["unquote-splicing"   "[1 2 3 4]"   "(let [xs [2 3]] `[1 ~@xs 4])"]
  ["unquote in list"    "[3]"         "(let [x 3] `(~x))"]
  ["syntax-quote symbol qualifies" "true" "(symbol? `foo)"])

(defspec "macros / defmacro"
  ["simple macro"       "3"
   "(do (defmacro m [a b] `(+ ~a ~b)) (m 1 2))"]
  ["macro unless"       "1"
   "(do (defmacro unless [c body] `(if ~c nil ~body)) (unless false 1))"]
  ["macro with body splice" "6"
   "(do (defmacro msum [& xs] `(+ ~@xs)) (msum 1 2 3))"]
  ["macroexpand-1"      "true"
   "(do (defmacro m [x] `(inc ~x)) (list? (macroexpand-1 '(m 5))))"]
  ["gensym unique"      "false"
   "(= (gensym) (gensym))"]
  ["gensym# in template" "true"
   "(do (defmacro m [] `(let [x# 1] x#)) (= 1 (m)))"])
