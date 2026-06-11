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
  # No first-class Class objects on this host (class names are symbols handled
  # in instance?/new positions), so class? is always false — like Clojure's
  # class? of a symbol. Selmer's `exception` macro calls it at expansion time.
  ["class? of value"    "false"  "(class? \"s\")"]
  ["class? of symbol"   "false"  "(class? 'java.lang.String)"]
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
# map?/coll? are STRICT (jolt-6s2 cleanup): tagged structs (symbols, chars,
# uuids) are values, not collections; sorted maps/sets and records ARE
# collections (and sorted-map/record are map?), matching Clojure.
(defspec "predicates / map? & coll? strictness"
  ["map? symbol"        "false"  "(map? (quote sym))"]
  ["map? char"          "false"  "(map? \\a)"]
  ["map? uuid"          "false"  "(map? (random-uuid))"]
  ["map? literal"       "true"   "(map? {:a 1})"]
  ["map? hash-map"      "true"   "(map? (hash-map :a 1))"]
  ["map? sorted-map"    "true"   "(map? (sorted-map :a 1))"]
  ["map? record"        "true"   "(do (defrecord Mr [a]) (map? (->Mr 1)))"]
  ["map? sorted-set"    "false"  "(map? (sorted-set 1))"]
  ["map? vector"        "false"  "(map? [1])"]
  ["coll? symbol"       "false"  "(coll? (quote sym))"]
  ["coll? char"         "false"  "(coll? \\a)"]
  ["coll? uuid"         "false"  "(coll? (random-uuid))"]
  ["coll? keyword"      "false"  "(coll? :k)"]
  ["coll? string"       "false"  "(coll? \"s\")"]
  ["coll? map literal"  "true"   "(coll? {:a 1})"]
  ["coll? sorted-map"   "true"   "(coll? (sorted-map :a 1))"]
  ["coll? sorted-set"   "true"   "(coll? (sorted-set 1))"]
  ["coll? record"       "true"   "(do (defrecord Cr [a]) (coll? (->Cr 1)))"]
  ["coll? vector"       "true"   "(coll? [1])"]
  ["coll? list"         "true"   "(coll? (list 1))"]
  ["coll? set"          "true"   "(coll? #{1})"]
  ["coll? lazy seq"     "true"   "(coll? (map inc [1]))"])

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

(defspec "predicates / seqable, reduced & emptiness"
  ["seqable? vector"   "true"   "(seqable? [1])"]
  ["seqable? map"      "true"   "(seqable? {:a 1})"]
  ["seqable? string"   "true"   "(seqable? \"s\")"]
  ["seqable? nil"      "true"   "(seqable? nil)"]
  ["seqable? number"   "false"  "(seqable? 5)"]
  ["integer? int"      "true"   "(integer? 5)"]
  ["integer? fraction" "false"  "(integer? 5.5)"]
  ["reduced? wrapped"  "true"   "(reduced? (reduced 1))"]
  ["reduced? plain"    "false"  "(reduced? 1)"]
  ["deref reduced"     "9"      "(deref (reduced 9))"]
  ["unreduced wrapped" "9"      "(unreduced (reduced 9))"]
  ["unreduced plain"   "9"      "(unreduced 9)"]
  ["not-empty full"    "[1]"    "(not-empty [1])"]
  ["not-empty empty"   "nil"    "(not-empty [])"]
  ["not-empty string"  "nil"    "(not-empty \"\")"])

# Stage 3 turn 2a: the implicit Janet root-env fallback is GONE — these are now
# proper interned clojure.core vars with Clojure semantics (compare's total
# order, meta-aware type, any?, gensym returning jolt symbols).
(defspec "predicates / compare, type, any? (stage 3)"
  ["compare ="          "0"     "(compare 1 1)"]
  ["compare <"          "-1"    "(compare 1 2)"]
  ["compare nil first"  "-1"    "(compare nil 1)"]
  ["compare nil nil"    "0"     "(compare nil nil)"]
  ["compare strings"    "-1"    "(compare \"a\" \"b\")"]
  ["compare keywords"   "-1"    "(compare :a :b)"]
  ["compare symbols"    "-1"    "(compare (quote a) (quote b))"]
  ["compare bools"      "-1"    "(compare false true)"]
  ["compare vec length" "-1"    "(compare [1 2] [1 2 3])"]
  ["compare vec elems"  "-1"    "(compare [1 2] [1 3])"]
  ["compare cross-type throws" :throws "(compare 1 \"a\")"]
  ["sort with compare"  "[nil 1 3]" "(sort compare [3 nil 1])"]
  ["type meta override" ":custom" "(type (with-meta [1] {:type :custom}))"]
  ["type of record"     "true"  "(do (defrecord TyR [a]) (= (symbol (str (type (->TyR 1)))) (type (->TyR 1))))"]
  ["any? value"         "true"  "(any? 5)"]
  ["any? nil"           "true"  "(any? nil)"]
  ["gensym is symbol"   "true"  "(symbol? (gensym))"]
  ["gensym prefix"      "true"  "(do (require (quote [clojure.string :as s])) (s/starts-with? (str (gensym \"p_\")) \"p_\"))"]
  ["gensym distinct"    "false" "(= (gensym) (gensym))"]
  ["int? Inf false"     "false" "(int? ##Inf)"]
  ["integer? Inf false" "false" "(integer? ##Inf)"]
  ["integer? NaN false" "false" "(integer? ##NaN)"])

# ifn? is the canonical IFn set (jolt-1vx): lists are NOT IFn.
(defspec "predicates / ifn?"
  ["fn"        "true"  "(ifn? inc)"]
  ["keyword"   "true"  "(ifn? :k)"]
  ["symbol"    "true"  "(ifn? (quote s))"]
  ["map"       "true"  "(ifn? {})"]
  ["sorted map" "true" "(ifn? (sorted-map))"]
  ["set"       "true"  "(ifn? #{1})"]
  ["vector"    "true"  "(ifn? [1])"]
  ["var"       "true"  "(ifn? (var first))"]
  ["list NOT"  "false" "(ifn? (list 1 2))"]
  ["lazy NOT"  "false" "(ifn? (map inc [1]))"]
  ["string NOT" "false" "(ifn? \"s\")"]
  ["number NOT" "false" "(ifn? 5)"]
  ["nil NOT"   "false" "(ifn? nil)"])

# zero?/pos? throw on non-numbers (Numbers.isZero/isPos), as in Clojure;
# every? short-circuits on the first falsey pred result, so an infinite seq
# with an early counterexample terminates. char? is the tagged-value check.
(defspec "predicates / numeric guards & every? (overlay moves)"
  ["zero? zero"          "true"   "(zero? 0)"]
  ["zero? nonzero"       "false"  "(zero? 3)"]
  ["zero? throws"        :throws  "(zero? :a)"]
  ["zero? throws on nil" :throws  "(zero? nil)"]
  ["pos? positive"       "true"   "(pos? 2)"]
  ["pos? zero"           "false"  "(pos? 0)"]
  ["pos? throws"         :throws  "(pos? \"x\")"]
  ["neg? throws"         :throws  "(neg? \"x\")"]
  ["every? all pass"     "true"   "(every? odd? [1 3 5])"]
  ["every? one fails"    "false"  "(every? odd? [1 2 5])"]
  ["every? vacuous"      "true"   "(every? odd? [])"]
  ["every? nil coll"     "true"   "(every? odd? nil)"]
  ["every? infinite short-circuit" "false" "(every? pos? (range))"]
  ["char? char"          "true"   "(char? \\x)"]
  ["char? string"        "false"  "(char? \"x\")"]
  ["char? number"        "false"  "(char? 97)"]
  ["char? nil"           "false"  "(char? nil)"])
