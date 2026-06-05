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
