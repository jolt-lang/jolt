# Specification: sorted collections (sorted-map / sorted-set, subseq/rsubseq).
#
# Sorted collections are pure Clojure (stage 3, jolt-0lj): the entries live in
# a comparator-ordered vector, all ops are overlay Clojure attached to the
# value, and the Janet seed only dispatches to them. Semantics match Clojure:
# lookup/membership go through the COMPARATOR ((contains? (sorted-set 1) 1.0)
# is true), equality is representation-agnostic ((= (sorted-map :a 1) {:a 1})),
# empty?/empty see the collection (not the host wrapper) and (empty sc) keeps
# the comparator. (vec coerces a seq to a vector so expecteds are vector
# literals, not quoted lists.)
(use ../support/harness)

(defspec "sorted / construction & ordering"
  ["sorted-set orders"   "[1 2 3]" "(vec (seq (sorted-set 3 1 2)))"]
  ["sorted-set dedupes"  "[1 2 3]" "(vec (seq (sorted-set 3 1 2 1 3)))"]
  ["sorted-set numeric"  "[1 2 10]" "(vec (seq (sorted-set 10 1 2)))"]
  ["sorted-map ordered entries" "[[:a 1] [:b 2] [:c 3]]" "(vec (seq (sorted-map :c 3 :a 1 :b 2)))"]
  ["first is min"        "1"       "(first (sorted-set 5 3 9 1))"])

(defspec "sorted / sorted?"
  ["sorted-set"      "true"   "(sorted? (sorted-set 1))"]
  ["sorted-map"      "true"   "(sorted? (sorted-map :a 1))"]
  ["plain set"       "false"  "(sorted? #{1})"]
  ["plain map"       "false"  "(sorted? {:a 1})"]
  ["vector"          "false"  "(sorted? [1 2])"])

(defspec "sorted / map ops"
  ["get hit"         "2"        "(get (sorted-map :a 1 :b 2) :b)"]
  ["get miss default" ":none"   "(get (sorted-map :a 1) :z :none)"]
  ["contains? yes"   "true"     "(contains? (sorted-map :a 1) :a)"]
  ["contains? no"    "false"    "(contains? (sorted-map :a 1) :z)"]
  ["assoc keeps order" "[[:a 1] [:b 2] [:c 3]]" "(vec (seq (assoc (sorted-map :c 3 :a 1) :b 2)))"]
  ["dissoc"          "[[:a 1] [:c 3]]" "(vec (seq (dissoc (sorted-map :a 1 :b 2 :c 3) :b)))"]
  ["conj entry"      "[[:a 1] [:z 9]]" "(vec (seq (conj (sorted-map :a 1) [:z 9])))"]
  ["keys sorted"     "[:a :b :c]" "(vec (keys (sorted-map :c 3 :a 1 :b 2)))"]
  ["vals by key"     "[1 2 3]"  "(vec (vals (sorted-map :c 3 :a 1 :b 2)))"]
  ["map as fn"       "2"        "((sorted-map :a 1 :b 2) :b)"]
  ["map as fn miss"  ":d"       "((sorted-map :a 1) :z :d)"])

(defspec "sorted / set ops"
  ["get present"     "2"        "(get (sorted-set 1 2 3) 2)"]
  ["get absent"      ":none"    "(get (sorted-set 1 2 3) 9 :none)"]
  ["contains? yes"   "true"     "(contains? (sorted-set 1 2 3) 2)"]
  ["contains? no"    "false"    "(contains? (sorted-set 1 2 3) 9)"]
  ["conj keeps order" "[0 1 2 3 5]" "(vec (seq (conj (sorted-set 1 2 3) 5 0)))"]
  ["disj"            "[1 3]"    "(vec (seq (disj (sorted-set 1 2 3) 2)))"]
  ["set as fn"       "3"        "((sorted-set 1 2 3) 3)"]
  ["set as fn miss"  "nil"      "((sorted-set 1 2 3) 9)"])

(defspec "sorted / by comparator"
  ["sorted-set-by desc"  "[10 3 2 1]" "(vec (seq (sorted-set-by > 1 3 2 10)))"]
  ["sorted-map-by desc"  "[[3 :c] [2 :b] [1 :a]]" "(vec (seq (sorted-map-by > 1 :a 3 :c 2 :b)))"]
  ["conj keeps comparator" "[5 3 2 1 0]" "(vec (seq (conj (sorted-set-by > 1 3 2) 5 0)))"]
  ["assoc keeps comparator" "[3 2 1]" "(vec (keys (assoc (sorted-map-by > 1 :a 3 :c) 2 :b)))"]
  ["disj keeps comparator" "[3 1]"   "(vec (seq (disj (sorted-set-by > 1 2 3) 2)))"]
  ["by-comparator is sorted?" "true" "(sorted? (sorted-set-by > 1 2))"])

(defspec "sorted / subseq & rsubseq"
  ["subseq >="       "[3 4 5]" "(vec (subseq (sorted-set 1 2 3 4 5) >= 3))"]
  ["subseq <"        "[1 2]"   "(vec (subseq (sorted-set 1 2 3 4 5) < 3))"]
  ["subseq range"    "[2 3 4]" "(vec (subseq (sorted-set 1 2 3 4 5) > 1 < 5))"]
  ["rsubseq <="      "[3 2 1]" "(vec (rsubseq (sorted-set 1 2 3 4 5) <= 3))"]
  ["subseq on map"   "[[2 :b] [3 :c]]" "(vec (subseq (sorted-map 1 :a 2 :b 3 :c) > 1))"]
  ["subseq empty result" "nil" "(subseq (sorted-set 1 2) > 5)"]
  ["rsubseq on map"  "[[2 :b] [1 :a]]" "(vec (rsubseq (sorted-map 1 :a 2 :b 3 :c) < 3))"])

(defspec "sorted / predicates"
  ["sorted-map? true"   "true"  "(sorted-map? (sorted-map 1 :a))"]
  ["sorted-map? false"  "false" "(sorted-map? {:a 1})"]
  ["sorted-set? true"   "true"  "(sorted-set? (sorted-set 1))"]
  ["sorted-set? false"  "false" "(sorted-set? #{1})"]
  ["map? sorted-map"    "true"  "(map? (sorted-map 1 :a))"]
  ["coll? sorted-set"   "true"  "(coll? (sorted-set 1))"])

(defspec "sorted / lookup + membership use the comparator"
  ["get cross-numeric"       ":a"   "(get (sorted-map 1 :a) 1.0)"]
  ["contains? cross-numeric" "true" "(contains? (sorted-set 1) 1.0)"]
  ["conj equal elem no-op"   "1"    "(count (conj (sorted-set 1) 1.0))"]
  ["assoc equal key replaces" "[[1 :z]]" "(vec (seq (assoc (sorted-map 1 :a) 1.0 :z)))"]
  ["first sorted-map"        "[1 :a]" "(first (sorted-map 2 :b 1 :a))"]
  ["dissoc missing no-op"    "2"    "(count (dissoc (sorted-map 1 :a 2 :b) 9))"]
  ["conj map merges"         "3"    "(count (conj (sorted-map 1 :a) {2 :b 3 :c}))"]
  ["conj nil no-op"          "1"    "(count (conj (sorted-map 1 :a) nil))"]
  ["into sorted-map"         "[[1 :a] [2 :b]]" "(vec (seq (into (sorted-map) [[2 :b] [1 :a]])))"]
  ["source unchanged"        "[1 2]" "(let [s (sorted-set 1 2)] (conj s 9) (vec (seq s)))"]
  ["sorted-map odd kvs throws" :throws "(sorted-map 1 :a 2)"])

(defspec "sorted / equality is representation-agnostic"
  ["sorted-map = literal"    "true"  "(= (sorted-map :a 1 :b 2) {:a 1 :b 2})"]
  ["literal = sorted-map"    "true"  "(= {:a 1 :b 2} (sorted-map :a 1 :b 2))"]
  ["sorted-map = hash-map"   "true"  "(= (sorted-map :a 1) (hash-map :a 1))"]
  ["sorted-map != more keys" "false" "(= (sorted-map :a 1) {:a 1 :b 2})"]
  ["sorted-set = literal"    "true"  "(= (sorted-set 1 2) #{1 2})"]
  ["literal = sorted-set"    "true"  "(= #{1 2} (sorted-set 2 1))"]
  ["sorted-set != diff"      "false" "(= (sorted-set 1 2) #{1 3})"]
  ["two sorted-maps"         "true"  "(= (sorted-map 1 :a 2 :b) (sorted-map 2 :b 1 :a))"]
  ["cmp irrelevant to ="     "true"  "(= (sorted-map-by > 1 :a 2 :b) (sorted-map 1 :a 2 :b))"]
  ["sorted-map as map key"   ":hit"  "(get {(sorted-map :a 1) :hit} {:a 1})"]
  ["sorted-set as map key"   ":hit"  "(get {(sorted-set 1 2) :hit} #{2 1})"])

(defspec "sorted / empty + empty? + rseq + printing"
  ["empty? empty map"     "true"  "(empty? (sorted-map))"]
  ["empty? non-empty"     "false" "(empty? (sorted-map 1 :a))"]
  ["empty? empty set"     "true"  "(empty? (sorted-set))"]
  ["empty keeps sortedness" "true" "(sorted? (empty (sorted-map 1 :a)))"]
  ["empty keeps cmp"      "[3 1]" "(vec (seq (into (empty (sorted-set-by > 1 2)) [1 3])))"]
  ["empty set kind"       "true"  "(sorted-set? (empty (sorted-set 1)))"]
  ["rseq map"             "[[2 :b] [1 :a]]" "(vec (rseq (sorted-map 1 :a 2 :b)))"]
  ["rseq set"             "[3 2 1]" "(vec (rseq (sorted-set 1 2 3)))"]
  ["pr-str sorted-map"    "\"{1 :a, 2 :b}\"" "(pr-str (sorted-map 2 :b 1 :a))"]
  ["pr-str sorted-set"    "\"#{1 2 3}\""     "(pr-str (sorted-set 3 1 2))"])

(defspec "sorted / seq fn interop"
  ["map over sorted-map"  "[1 2 3]"  "(vec (map first (sorted-map 2 :b 1 :a 3 :c)))"]
  ["map over sorted-set"  "[2 3 4]"  "(vec (map inc (sorted-set 3 1 2)))"]
  ["filter entries"       "[[2 :b]]" "(vec (filter (fn [[k v]] (even? k)) (sorted-map 1 :a 2 :b)))"]
  ["reduce over set"      "6"        "(reduce + (sorted-set 1 2 3))"]
  ["vec of sorted-set"    "[1 2 3]"  "(vec (sorted-set 3 1 2))"]
  ["into vec"             "[[1 :a] [2 :b]]" "(into [] (sorted-map 2 :b 1 :a))"]
  ["sorted-map-by 3way cmp" "[3 2 1]" "(vec (keys (sorted-map-by (fn [a b] (- b a)) 1 :a 2 :b 3 :c)))"])
