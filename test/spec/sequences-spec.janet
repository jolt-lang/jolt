# Specification: the sequence abstraction (clojure.core).
# Sequential expecteds use vector literals — Jolt's `=` treats vectors and lists
# with the same elements as equal, so [2 3 4] matches a (2 3 4) seq result.
(use ../support/harness)

(defspec "seq / access"
  ["first of vector"        "1"        "(first [1 2 3])"]
  ["first of list"          "1"        "(first (list 1 2 3))"]
  ["first of empty is nil"  "nil"      "(first [])"]
  ["first of nil is nil"    "nil"      "(first nil)"]
  ["second"                 "2"        "(second [1 2 3])"]
  ["last"                   "3"        "(last [1 2 3])"]
  ["rest of vector"         "[2 3]"    "(rest [1 2 3])"]
  ["rest of single"         "[]"       "(rest [1])"]
  ["rest of empty"          "[]"       "(rest [])"]
  ["next of single is nil"  "nil"      "(next [1])"]
  ["next of empty is nil"   "nil"      "(next [])"]
  ["nth"                    "30"       "(nth [10 20 30] 2)"]
  ["nth with default"       "99"       "(nth [10] 5 99)"]
  ["nth out of range"       :throws    "(nth [10] 5)"]
  ["ffirst"                 "1"        "(ffirst [[1 2] [3 4]])"]
  ["fnext"                  "2"        "(fnext [1 2 3])"]
  ["nnext"                  "[3 4]"    "(nnext [1 2 3 4])"])

(defspec "seq / construction"
  ["cons onto list"         "[0 1 2]"  "(cons 0 (list 1 2))"]
  ["cons onto vector"       "[0 1 2]"  "(cons 0 [1 2])"]
  ["cons onto nil"          "[0]"      "(cons 0 nil)"]
  ["conj vector appends"    "[1 2 3]"  "(conj [1 2] 3)"]
  ["conj list prepends"     "[0 1 2]"  "(conj (list 1 2) 0)"]
  ["conj multiple on vec"   "[1 2 3 4]" "(conj [1 2] 3 4)"]
  ["conj multiple on list"  "[4 3 1 2]" "(conj (list 1 2) 3 4)"]
  ["seq of empty is nil"    "nil"      "(seq [])"]
  ["seq of nil is nil"      "nil"      "(seq nil)"]
  ["seq of string"          "[\\a \\b]" "(seq \"ab\")"]
  ["empty?"                 "true"     "(empty? [])"]
  ["not empty?"             "false"    "(empty? [1])"]
  ["count"                  "3"        "(count [1 2 3])"]
  ["count of nil"           "0"        "(count nil)"]
  ["count of string"        "3"        "(count \"abc\")"])

(defspec "seq / map filter reduce"
  ["map"                    "[2 3 4]"      "(map inc [1 2 3])"]
  ["map two colls"          "[5 7 9]"      "(map + [1 2 3] [4 5 6])"]
  ["map stops at shortest"  "[5 7]"        "(map + [1 2] [4 5 6])"]
  ["map-indexed"            "[[0 :a] [1 :b]]" "(map-indexed vector [:a :b])"]
  ["mapv"                   "[2 3 4]"      "(mapv inc [1 2 3])"]
  ["filter"                 "[2 4]"        "(filter even? [1 2 3 4])"]
  ["filterv"                "[2 4]"        "(filterv even? [1 2 3 4])"]
  ["remove"                 "[1 3]"        "(remove even? [1 2 3 4])"]
  ["reduce"                 "10"           "(reduce + [1 2 3 4])"]
  ["reduce with init"       "20"           "(reduce + 10 [1 2 3 4])"]
  ["reduce empty with init" "0"           "(reduce + 0 [])"]
  ["reduce single no init"  "5"           "(reduce + [5])"]
  ["reduced short-circuits" "3"           "(reduce (fn [a x] (if (> a 2) (reduced a) (+ a x))) 0 [1 2 3 4 5])"]
  ["reduce-kv"              "6"           "(reduce-kv (fn [a k v] (+ a v)) 0 {:a 1 :b 2 :c 3})"]
  ["reductions"             "[1 3 6]"     "(reductions + [1 2 3])"]
  ["mapcat"                 "[1 1 2 2]"   "(mapcat (fn [x] [x x]) [1 2])"]
  ["keep"                   "[1 3]"       "(keep (fn [x] (if (odd? x) x nil)) [1 2 3 4])"]
  ["some truthy"            "true"        "(some even? [1 2 3])"]
  ["some nil"              "nil"          "(some even? [1 3 5])"]
  ["every? true"            "true"        "(every? pos? [1 2 3])"]
  ["every? false"           "false"       "(every? pos? [1 -2 3])"])

(defspec "seq / take drop slice"
  ["take"                   "[1 2 3]"     "(take 3 [1 2 3 4 5])"]
  ["take more than size"    "[1 2]"       "(take 5 [1 2])"]
  ["drop"                   "[4 5]"       "(drop 3 [1 2 3 4 5])"]
  ["take-while"             "[1 2]"       "(take-while (fn [x] (< x 3)) [1 2 3 1])"]
  ["drop-while"             "[3 1]"       "(drop-while (fn [x] (< x 3)) [1 2 3 1])"]
  ["take-last"              "[4 5]"       "(take-last 2 [1 2 3 4 5])"]
  ["drop-last"              "[1 2 3]"     "(drop-last [1 2 3 4])"]
  ["take-nth"               "[1 3 5]"     "(take-nth 2 [1 2 3 4 5])"]
  ["partition"             "[[1 2] [3 4]]" "(partition 2 [1 2 3 4 5])"]
  ["partition-all"         "[[1 2] [3]]"  "(partition-all 2 [1 2 3])"]
  ["split-at"              "[[1 2] [3 4]]" "(split-at 2 [1 2 3 4])"])

(defspec "seq / transform"
  ["reverse"                "[3 2 1]"     "(reverse [1 2 3])"]
  ["sort"                   "[1 2 3]"     "(sort [3 1 2])"]
  ["sort with comparator"   "[3 2 1]"     "(sort > [1 3 2])"]
  ["sort-by"                "[[1] [2 2]]" "(sort-by count [[2 2] [1]])"]
  ["distinct"               "[1 2 3]"     "(distinct [1 1 2 3 3])"]
  ["dedupe"                 "[1 2 1]"     "(dedupe [1 1 2 1])"]
  ["interpose"              "[1 0 2 0 3]" "(interpose 0 [1 2 3])"]
  ["interleave"             "[1 :a 2 :b]" "(interleave [1 2] [:a :b])"]
  ["flatten"                "[1 2 3 4]"   "(flatten [1 [2 [3 [4]]]])"]
  ["concat"                 "[1 2 3 4]"   "(concat [1 2] [3 4])"]
  ["into vector"            "[1 2 3 4]"   "(into [1 2] [3 4])"]
  ["into list"              "[3 2 1]"     "(into (list) [1 2 3])"]
  ["frequencies"            "{1 2, 2 1}"  "(frequencies [1 1 2])"]
  ["group-by"               "{false [1 3], true [2 4]}" "(group-by even? [1 2 3 4])"]
  ["zipmap"                 "{:a 1, :b 2}" "(zipmap [:a :b] [1 2])"]
  ["mapcat seqs"            "[1 2 3 4]"   "(mapcat identity [[1 2] [3 4]])"])

(defspec "seq / generators"
  ["range n"                "[0 1 2 3]"   "(range 4)"]
  ["range from to"          "[2 3 4]"     "(range 2 5)"]
  ["range with step"        "[0 2 4]"     "(range 0 6 2)"]
  ["take repeat"            "[:x :x :x]"  "(take 3 (repeat :x))"]
  ["repeat n"               "[5 5]"       "(repeat 2 5)"]
  ["take iterate"           "[1 2 4 8]"   "(take 4 (iterate (fn [x] (* x 2)) 1))"]
  ["take cycle"             "[1 2 1 2 1]" "(take 5 (cycle [1 2]))"]
  ["take repeatedly"        "3"           "(count (take 3 (repeatedly (fn [] 1))))"]
  ["take-last of range"     "[8 9]"       "(take-last 2 (range 10))"])

# Clojure IFn values used as the function arg to higher-order fns: a keyword or
# symbol looks up a key, a set tests membership, a map looks up a key.
(defspec "seq / IFn values as functions"
  ["map keyword"        "[1 2 3]"        "(map :a [{:a 1} {:a 2} {:a 3}])"]
  ["filter keyword"     "[{:ok true}]"   "(filter :ok [{:ok true} {:ok false}])"]
  ["remove keyword"     "[{:ok false}]"  "(remove :ok [{:ok true} {:ok false}])"]
  ["sort-by keyword"    "[{:a 1} {:a 2} {:a 3}]" "(sort-by :a [{:a 3} {:a 1} {:a 2}])"]
  ["sort-by key + cmp"  "[{:a 3} {:a 2} {:a 1}]" "(sort-by :a > [{:a 3} {:a 1} {:a 2}])"]
  ["filter set"         "[2 4]"          "(filter #{2 4} [1 2 3 4 5])"]
  ["remove set"         "[1 3 5]"        "(remove #{2 4} [1 2 3 4 5])"]
  ["group-by keyword"   "{1 [{:n 1}], 2 [{:n 2}]}" "(group-by :n [{:n 1} {:n 2}])"]
  ["map a map"          "[1 nil 2]"      "(map {:a 1 :b 2} [:a :z :b])"]
  ["take-nth transducer" "[0 2 4 6 8]"   "(into [] (take-nth 2) (range 10))"]
  ["interpose transducer" "[1 :x 2]"     "(into [] (interpose :x) [1 2])"])

# conj edge cases: 0-arg, conj onto nil (builds a list), conj a map into a map.
(defspec "seq / conj edge cases"
  ["conj no args"       "[]"        "(conj)"]
  ["conj nil one"       "[3]"       "(conj nil 3)"]
  ["conj nil many"      "[2 1]"     "(conj nil 1 2)"]
  ["conj vector"        "[1 2 3]"   "(conj [1 2] 3)"]
  ["conj list prepend"  "[0 1 2]"   "(conj '(1 2) 0)"]
  ["conj map + map"     "{:a 0, :b 1}" "(conj {:a 0} {:b 1})"]
  ["conj map + pair"    "{:a 0, :b 1}" "(conj {:a 0} [:b 1])"]
  ["conj map merge wins" "{:a 2}"   "(conj {:a 0} {:a 1} {:a 2})"])
