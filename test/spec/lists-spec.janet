# Specification: lists (persistent singly-linked).
(use ../support/harness)

(defspec "list / construct & predicate"
  ["list"                   "[1 2 3]"   "(list 1 2 3)"]
  ["empty list"             "[]"        "(list)"]
  ["quoted list"            "[1 2 3]"   "(quote (1 2 3))"]
  ["list? true"             "true"      "(list? (list 1 2))"]
  ["list? on conj result"   "true"      "(list? (conj (list 1) 0))"]
  ["count"                  "3"         "(count (list 1 2 3))"]
  ["empty? true"            "true"      "(empty? (list))"]
  ["list = vector elts"     "true"      "(= (list 1 2 3) [1 2 3])"])

(defspec "list / access & update"
  ["first"                  "1"         "(first (list 1 2 3))"]
  ["rest"                   "[2 3]"     "(rest (list 1 2 3))"]
  ["peek is first"          "1"         "(peek (list 1 2 3))"]
  ["pop drops first"        "[2 3]"     "(pop (list 1 2 3))"]
  ["conj prepends"          "[0 1 2]"   "(conj (list 1 2) 0)"]
  ["conj many prepends"     "[4 3 1 2]" "(conj (list 1 2) 3 4)"]
  ["cons prepends"          "[0 1 2]"   "(cons 0 (list 1 2))"]
  ["nth"                    "20"        "(nth (list 10 20 30) 1)"])

(defspec "list / immutability & performance"
  ["conj does not mutate"   "true"      "(let [l (list 1 2 3) m (conj l 0)] (and (= l [1 2 3]) (= m [0 1 2 3])))"]
  ["reduce conj builds"     "[2 1 0]"   "(reduce conj (list) (range 3))"]
  ["O(1) conj at scale"     "200000"    "(count (reduce conj (list) (range 200000)))"]
  ["scale head correct"     "199999"    "(first (reduce conj (list) (range 200000)))"])
