# Specification: multimethods & hierarchies.
(use ../support/harness)

(defspec "multimethods / dispatch"
  ["dispatch on value"  "\"two\""
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (defmethod f 2 [_] \"two\") (f 2))"]
  ["dispatch on keyword fn" "\"circle\""
   "(do (defmulti area :shape) (defmethod area :circle [_] \"circle\") (area {:shape :circle}))"]
  [":default method"    "\"other\""
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (defmethod f :default [_] \"other\") (f 99))"]
  ["no match throws"    :throws
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (f 99))"]
  ["multiple args"      "5"
   "(do (defmulti g (fn [a b] a)) (defmethod g :add [_ b] b) (g :add 5))"]
  ["get-method"         "\"one\""
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") ((get-method f 1) 1))"]
  ["remove-method"      :throws
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (remove-method f 1) (f 1))"])

(defspec "multimethods / hierarchies"
  ["derive + isa?"      "true"   "(do (derive ::child ::parent) (isa? ::child ::parent))"]
  ["isa? reflexive"     "true"   "(isa? ::x ::x)"]
  ["isa? unrelated"     "false"  "(do (derive ::a ::b) (isa? ::a ::c))"]
  ["parents"            "true"   "(do (derive ::c ::p) (contains? (parents ::c) ::p))"]
  ["ancestors"          "true"   "(do (derive ::c ::p) (derive ::p ::g) (contains? (ancestors ::c) ::g))"]
  ["descendants"        "true"   "(do (derive ::c ::p) (contains? (descendants ::p) ::c))"]
  ["dispatch via hierarchy" "\"animal\""
   "(do (derive ::dog ::animal) (defmulti speak identity) (defmethod speak ::animal [_] \"animal\") (speak ::dog))"])
