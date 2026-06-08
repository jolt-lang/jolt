# Specification: vectors (persistent, indexed).
(use ../support/harness)

(defspec "vector / construct & predicate"
  ["literal"                "[1 2 3]"   "[1 2 3]"]
  ["vector"                 "[1 2 3]"   "(vector 1 2 3)"]
  ["vector zero args"       "[]"        "(vector)"]
  ["vec from list"          "[1 2 3]"   "(vec (list 1 2 3))"]
  ["vec from range"         "[0 1 2]"   "(vec (range 3))"]
  ["vec of map yields entries" "[[:a 1]]" "(vec {:a 1})"]
  ["vector? true"           "true"      "(vector? [1])"]
  ["vector? false on list"  "false"     "(vector? (list 1))"]
  ["vector = list elts"     "true"      "(= [1 2 3] (list 1 2 3))"])

(defspec "vector / access"
  ["nth"                    ":b"        "(nth [:a :b :c] 1)"]
  ["nth default"            ":x"        "(nth [:a] 5 :x)"]
  ["get by index"           ":b"        "(get [:a :b] 1)"]
  ["get out of range nil"   "nil"       "(get [:a] 5)"]
  ["get default"            ":x"        "(get [:a] 5 :x)"]
  ["first"                  "1"         "(first [1 2 3])"]
  ["last"                   "3"         "(last [1 2 3])"]
  ["peek is last"           "3"         "(peek [1 2 3])"]
  ["count"                  "3"         "(count [1 2 3])"]
  ["contains? index"        "true"      "(contains? [:a :b] 1)"]
  ["contains? past end"     "false"     "(contains? [:a] 3)"]
  ["vector as fn"           ":b"        "([:a :b :c] 1)"]
  # An IFn collection held in a binding (not just a literal) must dispatch as IFn,
  # not as a host call: applies to vectors, keywords, and meta-bearing vectors.
  ["vector-in-local as fn"  "20"        "(let [v [10 20 30]] (v 1))"]
  ["keyword-in-local as fn" "7"         "(let [k :a] (k {:a 7}))"]
  ["meta vector as fn"      "10"        "((with-meta [10 20] {:k 1}) 0)"])

(defspec "vector / update (persistent)"
  ["conj appends"           "[1 2 3]"   "(conj [1 2] 3)"]
  ["conj many"              "[1 2 3 4]" "(conj [1 2] 3 4)"]
  ["assoc index"            "[1 9 3]"   "(assoc [1 2 3] 1 9)"]
  ["assoc at count appends" "[1 2 3]"   "(assoc [1 2] 2 3)"]
  ["update"                 "[1 3 3]"   "(update [1 2 3] 1 inc)"]
  ["pop drops last"         "[1 2]"     "(pop [1 2 3])"]
  ["subvec start end"       "[2 3]"     "(subvec [1 2 3 4] 1 3)"]
  ["subvec to end"          "[3 4]"     "(subvec [1 2 3 4] 2)"]
  ["mapv"                   "[2 3 4]"   "(mapv inc [1 2 3])"]
  ["filterv"                "[2 4]"     "(filterv even? [1 2 3 4])"])

(defspec "vector / immutability & nesting"
  ["conj does not mutate"   "true"      "(let [v [1 2] w (conj v 3)] (and (= v [1 2]) (= w [1 2 3])))"]
  ["assoc does not mutate"  "true"      "(let [v [1 2 3] w (assoc v 0 9)] (and (= v [1 2 3]) (= w [9 2 3])))"]
  ["get-in"                 "2"         "(get-in [[1 2] [3 4]] [0 1])"]
  ["assoc-in"               "[[1 9]]"   "(assoc-in [[1 2]] [0 1] 9)"]
  ["update-in"              "[[1 3]]"   "(update-in [[1 2]] [0 1] inc)"]
  ["large vector nth"       "1500"      "(nth (vec (range 2000)) 1500)"]
  ["large vector count"     "2000"      "(count (vec (range 2000)))"]
  ["large conj immutable"   "true"      "(let [v (vec (range 1000)) w (conj v :end)] (and (= 1000 (count v)) (= 1001 (count w))))"])
