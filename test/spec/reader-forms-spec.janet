# Specification: reader forms + syntax-quote + metadata.
#
# Adapted from the jank test corpus (test/jank/{syntax-quote,metadata,reader-macro,
# call}); we keep our own copies since jank may diverge. Syntax-quoted symbols are
# qualified to clojure.core (matching jank/Clojure). Platform-specific reader forms
# (#uuid, #inst, ##Inf/##NaN, bigdecimal/biginteger/ratio) are omitted.
(use ../support/harness)

(defspec "reader / anonymous fn #()"
  ["no args"         "3"      "(#(+ 1 2))"]
  ["one arg %"       "6"      "(#(* % 2) 3)"]
  ["positional %1 %2" "[1 2]" "(#(do [%1 %2]) 1 2)"]
  ["rest %&"         "[1 2 3]" "(#(do %&) 1 2 3)"]
  ["fixed + rest"    "[2 3]"  "(#(do % %&) 1 2 3)"]
  ["%2 + rest"       "[3]"    "(#(do %2 %&) 1 2 3)"]
  ["%2 only (placeholder p1)" "20" "(#(* %2 2) 1 10)"]
  ["% and %1 same"   "10"     "(#(+ % %1) 5)"])

(defspec "reader / var-quote #'"
  ["var-quote = var" "true"   "(= (var str) #'str)"]
  ["is a var"        "true"   "(var? #'str)"]
  ["deref var-quote" "5"      "(do (def w 5) (deref #'w))"])

(defspec "reader / metadata ^"
  ["meta on map"     "true"   "(:foo (meta ^:foo {}))"]
  ["meta on vector"  "true"   "(:foo (meta ^:foo [1 2]))"]
  ["meta on set"     "true"   "(:foo (meta ^:foo #{}))"]
  ["meta map form"   "1"      "(:a (meta ^{:a 1} {}))"]
  ["meta on quoted sym" "true" "(:foo (meta (quote ^:foo bar)))"]
  ["with-meta map"   "true"   "(:k (meta (with-meta {} {:k true})))"]
  ["with-meta vector" "true"  "(:k (meta (with-meta [] {:k true})))"]
  ["non-metadatable num" "nil" "(meta 100)"]
  ["non-metadatable str" "nil" "(meta \"\")"]
  ["non-metadatable bool" "nil" "(meta true)"])

(defspec "reader / syntax-quote"
  ["plain literal"   "[1 2 3]" "`[1 2 3]"]
  ["gensym distinct" "true"   "(not= `meow# `meow#)"]
  ["gensym stable"   "true"   "(let [s `[meow# meow#]] (= (first s) (second s)))"]
  ["qualifies unresolved" "(quote user/foo)" "`foo"]
  # jolt-265 (fixed): resolved core symbols fully-qualify to clojure.core/.
  ["qualifies core sym" "(quote clojure.core/str)" "`str"]
  ["unquote value"   "[1 2 3]" "(let [a [1 2 3]] `~a)"]
  ["unquote in call" "(quote (clojure.core/str [1 2 3]))" "(let [a [1 2 3]] `(str ~a))"]
  ["splice empty"    "(quote (clojure.core/str))" "(let [e []] `(str ~@e))"]
  ["splice values"   "(quote (clojure.core/str 1 2 3))" "(let [a [1 2 3]] `(str ~@a))"]
  ["splice in vector" "[1 2 3 0 1 2 3]" "(let [b [0] a [1 2 3] e []] `[~@e ~@a ~@b ~@a ~@e])"]
  # jolt-edb (fixed): ~/~@ inside set literals.
  ["splice in set"   "#{0 1 2 3}" "(let [b [0] a [1 2 3] e []] `#{~@e ~@a ~@b})"]
  ["unquote in set"  "#{5 9}"   "(let [x 5] `#{~x 9})"])

# Spec 02-reader S17/S19/S18/S13a/S22: discard, symbolic values, conditionals,
# var-quote identity, gensym stability (jank corpus derived).
(defspec "reader / discard, symbolic values, conditionals (spec 2.3)"
  ["discard simple"      "2"     "(do #_1 2)"]
  ["discard in vector"   "[1 3]" "[1 #_2 3]"]
  ["discard stacks"      "3"     "(do #_ #_ 1 2 3)"]
  ["##Inf"               "true"  "(= ##Inf (/ 1.0 0.0))"]
  ["##-Inf"              "true"  "(= ##-Inf (/ -1.0 0.0))"]
  ["##NaN not self-equal" "false" "(= ##NaN ##NaN)"]
  ["##NaN is NaN?"       "true"  "(NaN? ##NaN)"]
  ["conditional :default reachable" "2" "#?(:no-such-dialect 1 :default 2)"]
  ["var-quote qualified" "true"  "(= (var clojure.core/str) #'clojure.core/str)"]
  ["gensym stable in template" "true" "(let [syms `[meow# meow#]] (= (first syms) (second syms)))"]
  ["gensym fresh across templates" "false" "(= `meow# `meow#)"])

# Spec 02-reader S25: syntax-quote of a self-evaluating literal is the literal
# (read-time collapse), so adjacent/nested backticks over literals are inert.
(defspec "reader / syntax-quote literal collapse (spec 2.4 S25)"
  ["string once"        "true"  "(= \"meow\" `\"meow\")"]
  ["string nested"      "true"  "(= \"meow\" ``\"meow\")"]
  ["string triple"      "true"  "(= \"meow\" ```\"meow\")"]
  ["number nested"      "true"  "(= 42 ``42)"]
  ["keyword nested"     "true"  "(= :k ``:k)"]
  ["nil nested"         "true"  "(nil? ``nil)"]
  ["char nested"        "true"  "(= \\a ``\\a)"]
  ["bool nested"        "true"  "(= true ``true)"]
  # collapse must NOT apply to symbols (they qualify) or collections (templates)
  ["symbol still qualifies" "true" "(= (quote clojure.core/map) `map)"]
  ["vector still templates" "true" "(= [1 2] `[1 ~(inc 1)])"])
