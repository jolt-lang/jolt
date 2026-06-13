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
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (remove-method f 1) (f 1))"]
  ["methods"            "\"one\""
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") ((get (methods f) 1) 1))"]
  ["methods count"      "2"
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (defmethod f 2 [_] \"two\") (count (methods f)))"]
  ["remove-all-methods" :throws
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (defmethod f 2 [_] \"two\") (remove-all-methods f) (f 1))"]
  ["remove-all-methods empties the table" "0"
   "(do (defmulti f identity) (defmethod f 1 [_] \"one\") (remove-all-methods f) (count (methods f)))"])

(defspec "multimethods / hierarchies"
  ["derive + isa?"      "true"   "(do (derive ::child ::parent) (isa? ::child ::parent))"]
  ["isa? reflexive"     "true"   "(isa? ::x ::x)"]
  ["isa? unrelated"     "false"  "(do (derive ::a ::b) (isa? ::a ::c))"]
  ["parents"            "true"   "(do (derive ::c ::p) (contains? (parents ::c) ::p))"]
  ["ancestors"          "true"   "(do (derive ::c ::p) (derive ::p ::g) (contains? (ancestors ::c) ::g))"]
  ["descendants"        "true"   "(do (derive ::c ::p) (contains? (descendants ::p) ::c))"]
  ["dispatch via hierarchy" "\"animal\""
   "(do (derive ::dog ::animal) (defmulti speak identity) (defmethod speak ::animal [_] \"animal\") (speak ::dog))"]
  ["custom :default key"  ":unknown"
   "(do (defmulti classify :type :default :other) (defmethod classify :a [_] :alpha) (defmethod classify :other [_] :unknown) (classify {:type :zzz}))"]
  ["explicit :hierarchy"  "\"a\""
   "(do (def h (derive (make-hierarchy) ::dog ::animal)) (defmulti snd identity :hierarchy h) (defmethod snd ::animal [_] \"a\") (snd ::dog))"])

# prefer-method breaks isa-dispatch ties; ambiguity without a preference is
# an ERROR (jolt-heo — this used to silently take an arbitrary method).
(defspec "multimethods / prefer-method"
  ["preference picks the winner" ":rect"
   "(do (derive :p/sq :p/rect) (derive :p/sq :p/shape) (defmulti pm1 identity) (defmethod pm1 :p/rect [x] :rect) (defmethod pm1 :p/shape [x] :shape) (prefer-method pm1 :p/rect :p/shape) (pm1 :p/sq))"]
  ["reverse preference" ":shape"
   "(do (derive :q/sq :q/rect) (derive :q/sq :q/shape) (defmulti pm2 identity) (defmethod pm2 :q/rect [x] :rect) (defmethod pm2 :q/shape [x] :shape) (prefer-method pm2 :q/shape :q/rect) (pm2 :q/sq))"]
  ["ambiguity throws" :throws
   "(do (derive :r/sq :r/rect) (derive :r/sq :r/shape) (defmulti pm3 identity) (defmethod pm3 :r/rect [x] :rect) (defmethod pm3 :r/shape [x] :shape) (pm3 :r/sq))"]
  ["isa dominance needs no preference" ":child"
   "(do (derive :s/c :s/p) (defmulti pm4 identity) (defmethod pm4 :s/c [x] :child) (defmethod pm4 :s/p [x] :parent) (pm4 :s/c))"]
  ["prefers map shape" "true"
   "(do (defmulti pm5 identity) (defmethod pm5 :a [x] 1) (defmethod pm5 :b [x] 2) (prefer-method pm5 :a :b) (contains? (get (prefers pm5) :a) :b))"]
  ["exact match needs no preference" ":exact"
   "(do (derive :t/sq :t/rect) (defmulti pm6 identity) (defmethod pm6 :t/sq [x] :exact) (defmethod pm6 :t/rect [x] :parent) (pm6 :t/sq))"])

# defmulti drops a leading docstring/attr-map (jolt-es4 — it used to be taken as
# the dispatch fn). methods/get-method take the multimethod VALUE and recover
# the table, so a bare multifn ref works even when defmethods live elsewhere
# (jolt-9pu).
(defspec "multimethods / docstring & value-based table ops"
  ["defmulti docstring"   "\"A\""
   "(do (defmulti gd \"the dispatcher\" identity) (defmethod gd :a [_] \"A\") (gd :a))"]
  ["defmulti doc+default" "\"d\""
   "(do (defmulti gx \"doc\" identity) (defmethod gx :default [_] \"d\") (gx :anything))"]
  ["methods on value"     "2"
   "(do (defmulti gm identity) (defmethod gm 1 [_] :one) (defmethod gm 2 [_] :two) (count (methods gm)))"]
  ["get-method on value"  "true"
   "(do (defmulti gg identity) (defmethod gg :a [_] :x) (fn? (get-method gg :a)))"])
