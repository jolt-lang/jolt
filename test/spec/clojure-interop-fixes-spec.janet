# Specification: Clojure-compat fixes that landed with HTTP-client support
# (jolt-lang/http-client). All are general language behaviours, exercised here
# across interpret + compile modes by the harness.
(use ../support/harness)

(defspec "interop-fixes / deprecated #^ metadata reader"
  ["#^ type hint on a param"   "\"x\""
   "(do (defn f1 [#^String s] s) (f1 \"x\"))"]
  ["#^\"[B\" array hint"        "[1 2]"
   "(do (defn f2 [#^\"[B\" b] b) (f2 [1 2]))"]
  ["#^ is equivalent to ^"      "true"
   "(= (meta (with-meta [] {:tag (quote String)})) {:tag (quote String)})"])

(defspec "interop-fixes / (str pattern) yields raw source"
  ["str of a regex"            "\"abc\""        "(str #\"abc\")"]
  ["compose patterns via str"  "true"
   "(boolean (re-matches (re-pattern (str #\"<\" \"(.*)\" \">\")) \"<hi>\"))"])

(defspec "interop-fixes / into onto a map"
  ["merges map items"          "true"   "(= {:a 1 :b 2} (into {} [{:a 1} {:b 2}]))"]
  ["accepts [k v] pairs"       "true"   "(= {:a 1} (into {} [[:a 1]]))"]
  ["map item onto empty {}"    "true"   "(= {:x 1} (into {} (list {:x 1})))"]
  ["conj a map onto {}"        "true"   "(= {:a 1} (conj {} {:a 1}))"])

(defspec "interop-fixes / a var is callable as its value"
  ["call a var directly"       "42"
   "(do (def vf (fn [x] (inc x))) ((var vf) 41))"]
  ["var bound as a client fn"  "\"ok\""
   "(do (def base (fn [_] \"ok\")) (def mw (fn [client] (fn [req] (client req)))) ((mw (var base)) {}))"])
