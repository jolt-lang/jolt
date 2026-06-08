# Specification: type & value predicates.
(use ../support/harness)

(defspec "predicates / nil & boolean"
  ["nil? true"          "true"   "(nil? nil)"]
  ["nil? false"         "false"  "(nil? 0)"]
  ["some? true"         "true"   "(some? 0)"]
  ["some? on nil"       "false"  "(some? nil)"]
  ["true?"              "true"   "(true? true)"]
  ["false?"             "true"   "(false? false)"]
  ["boolean? true"      "true"   "(boolean? false)"]
  ["not nil"            "true"   "(not nil)"]
  ["not 0 is false"     "false"  "(not 0)"]
  ["boolean of nil"     "false"  "(boolean nil)"]
  ["boolean of value"   "true"   "(boolean 5)"])

(defspec "predicates / types"
  ["string?"            "true"   "(string? \"x\")"]
  ["number?"            "true"   "(number? 1)"]
  ["keyword?"           "true"   "(keyword? :a)"]
  ["symbol?"            "true"   "(symbol? (quote a))"]
  ["char?"              "true"   "(char? \\a)"]
  ["fn? on fn"          "true"   "(fn? inc)"]
  ["ifn? on keyword"    "true"   "(ifn? :a)"]
  ["vector?"            "true"   "(vector? [1])"]
  ["list?"              "true"   "(list? (list 1))"]
  ["map?"               "true"   "(map? {:a 1})"]
  ["set?"               "true"   "(set? #{1})"]
  ["coll? vector"       "true"   "(coll? [1])"]
  ["coll? map"          "true"   "(coll? {:a 1})"]
  ["coll? on number"    "false"  "(coll? 1)"]
  ["seq? list"          "true"   "(seq? (list 1))"]
  ["seq? vector"        "false"  "(seq? [1])"]
  ["sequential? vector" "true"   "(sequential? [1])"]
  ["associative? map"   "true"   "(associative? {:a 1})"]
  ["associative? vec"   "true"   "(associative? [1])"]
  ["associative? list"  "false"  "(associative? '(1 2))"]
  ["associative? set"   "false"  "(associative? #{1})"]
  ["reversible? vec"    "true"   "(reversible? [1 2])"]
  ["reversible? list"   "false"  "(reversible? '(1 2))"]
  ["reversible? smap"   "true"   "(reversible? (sorted-map :a 1))"]
  ["reversible? hmap"   "false"  "(reversible? (hash-map :a 1))"]
  ["indexed? vector"    "true"   "(indexed? [1])"]
  ["counted? vector"    "true"   "(counted? [1])"])

(defspec "predicates / idents"
  ["ident? keyword"     "true"   "(ident? :a)"]
  ["ident? symbol"      "true"   "(ident? (quote a))"]
  ["simple-keyword?"    "true"   "(simple-keyword? :a)"]
  ["qualified-keyword?" "true"   "(qualified-keyword? :a/b)"]
  ["simple-symbol?"     "true"   "(simple-symbol? (quote a))"]
  ["qualified-symbol?"  "true"   "(qualified-symbol? (quote a/b))"]
  ["name of keyword"    "\"a\""  "(name :a)"]
  ["name of qualified"  "\"b\""  "(name :a/b)"]
  ["namespace"          "\"a\""  "(namespace :a/b)"]
  ["namespace simple"   "nil"    "(namespace :a)"]
  ["keyword constructor" ":foo"  "(keyword \"foo\")"]
  ["keyword ns + name"  ":a/b"   "(keyword \"a\" \"b\")"]
  ["symbol constructor" "(quote x)" "(symbol \"x\")"]
  ["name of string"     "\"s\""  "(name \"s\")"])

# Predicates moved from Janet to the Clojure overlay (jolt-1j0). Jolt has no
# ratio/bigdecimal types (so ratio?/decimal? are always false, rational?=int?),
# and no distinct host object/undefined types (object?/undefined? always false).
(defspec "predicates / overlay-migrated"
  ["not-any? true"      "true"   "(not-any? even? [1 3 5])"]
  ["not-any? false"     "false"  "(not-any? even? [1 2 3])"]
  ["not-every? true"    "true"   "(not-every? even? [2 4 5])"]
  ["not-every? false"   "false"  "(not-every? even? [2 4 6])"]
  ["ident? number"      "false"  "(ident? 1)"]
  ["qualified-ident?"   "true"   "(qualified-ident? :a/b)"]
  ["qualified-ident? no" "false" "(qualified-ident? :a)"]
  ["simple-ident?"      "true"   "(simple-ident? :a)"]
  ["ratio?"             "false"  "(ratio? 3)"]
  ["decimal?"           "false"  "(decimal? 3)"]
  ["rational? int"      "true"   "(rational? 3)"]
  ["rational? float"    "false"  "(rational? 3.5)"]
  ["nat-int? zero"      "true"   "(nat-int? 0)"]
  ["nat-int? neg"       "false"  "(nat-int? -1)"]
  ["pos-int?"           "true"   "(pos-int? 5)"]
  ["neg-int?"           "true"   "(neg-int? -3)"]
  ["NaN? on nan"        "true"   "(NaN? (/ 0.0 0.0))"]
  ["NaN? on number"     "false"  "(NaN? 5)"]
  ["abs negative"       "3"      "(abs -3)"]
  ["abs positive"       "2.5"    "(abs 2.5)"]
  ["object?"            "false"  "(object? 1)"]
  ["undefined?"         "false"  "(undefined? 1)"]
  ["keyword-identical?" "true"   "(keyword-identical? :a :a)"]
  ["keyword-identical? no" "false" "(keyword-identical? :a :b)"])

# Tagged-value predicates moved to the overlay in Phase 4 (read the value's
# :jolt/type via get). The constructors stay native.
(defspec "predicates / tagged-value (Phase 4)"
  ["atom? yes"          "true"   "(atom? (atom 1))"]
  ["atom? no"           "false"  "(atom? 1)"]
  ["volatile? yes"      "true"   "(volatile? (volatile! 1))"]
  ["volatile? no"       "false"  "(volatile? (atom 1))"]
  ["record? yes"        "true"   "(do (defrecord Rp [a]) (record? (->Rp 1)))"]
  ["record? no map"     "false"  "(record? {:a 1})"]
  ["record? no nil"     "false"  "(record? nil)"]
  ["tagged-literal? yes" "true"  "(tagged-literal? (tagged-literal (quote inst) \"2020\"))"]
  ["tagged-literal? no" "false"  "(tagged-literal? 1)"]
  ["reader-conditional? no" "false" "(reader-conditional? 1)"]
  ["chunked-seq? always false" "false" "(chunked-seq? (seq [1 2 3]))"])

(defspec "predicates / equality & identity"
  ["= same"             "true"   "(= 1 1)"]
  ["= vectors"          "true"   "(= [1 2] [1 2])"]
  ["= vector & list"    "true"   "(= [1 2] (list 1 2))"]
  ["= maps"             "true"   "(= {:a 1} {:a 1})"]
  ["= sets"             "true"   "(= #{1 2} #{2 1})"]
  ["= nested"           "true"   "(= {:a [1 2]} {:a [1 2]})"]
  ["not= differs"       "true"   "(not= [1 2] [1 3])"]
  ["identical? same kw" "true"   "(identical? :a :a)"]
  ["compare strings"    "-1"     "(compare \"a\" \"b\")"])
