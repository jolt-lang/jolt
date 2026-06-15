# Specification: a deftype's custom Object/toString is honored by both .toString
# and str (jolt-rt6n). Before: object-methods' generic toString intercepted the
# record's .toString (the record isn't a tagged shim), and str rendered the
# #Type{...} repr instead of routing through toString. Needed by hiccup's
# RawString (a deftype with toString).
(use ../support/harness)

(defspec "deftype / custom toString"
  [".toString uses the method" "\"hi\""
   "(do (deftype Foo [s] Object (toString [_] s)) (.toString (->Foo \"hi\")))"]
  ["str uses the method" "\"hi\""
   "(do (deftype Foo [s] Object (toString [_] s)) (str (->Foo \"hi\")))"]
  ["str concatenation uses it" "\"<hi>\""
   "(do (deftype Foo [s] Object (toString [_] s)) (str \"<\" (->Foo \"hi\") \">\"))"]
  ["computed toString" "\"v=7\""
   "(do (deftype Boxed [v] Object (toString [_] (str \"v=\" v))) (str (->Boxed 7)))"]
  # a record WITHOUT a custom toString keeps the #Type{...} repr (regression guard)
  ["defrecord without toString keeps repr" "true"
   "(do (defrecord Bar [x]) (boolean (re-find #\"Bar\" (str (->Bar 1)))))"]
  # pr-str of a defrecord is unaffected (still the data repr)
  ["pr-str of a defrecord is the repr" "true"
   "(do (defrecord Baz [x]) (boolean (re-find #\"\\{\" (pr-str (->Baz 1)))))"])
