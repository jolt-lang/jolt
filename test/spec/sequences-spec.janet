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
  # nil elements are values, not end-of-seq: multi-coll map must not truncate.
  ["map keeps nil elements" "[[1 :a] [nil :b] [3 nil]]" "(map vector [1 nil 3] [:a :b nil])"]
  ["map 3 colls"            "[12 15 18]"   "(map + [1 2 3] [4 5 6] [7 8 9])"]
  ["map 3 colls shortest"   "[12 15]"      "(map + [1 2] [4 5 6] [7 8 9])"]
  ["map 4 colls"            "[16 20]"      "(map + [1 2] [3 4] [5 6] [7 8])"]
  ["map 3 colls nils"       "[[1 :a 10] [nil :b 20] [3 nil 30]]" "(map vector [1 nil 3] [:a :b nil] [10 20 30])"]
  ["map empty coll"         "()"           "(map + [] [1 2 3] [4 5 6])"]
  ["map lazy+concrete"      "[11 22 33]"   "(map + (map identity [1 2 3]) [10 20 30])"]
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
  ["reduce-kv on vector"    "[[0 :a] [1 :b]]" "(reduce-kv (fn [a i v] (conj a [i v])) [] [:a :b])"]
  ["reduce-kv honors reduced" "[:a]"      "(reduce-kv (fn [a i v] (if (= i 1) (reduced a) (conj a v))) [] [:a :b :c])"]
  ["reduce-kv on nil"       "0"           "(reduce-kv (fn [a k v] (+ a v)) 0 nil)"]
  ["reductions"             "[1 3 6]"     "(reductions + [1 2 3])"]
  ["mapcat"                 "[1 1 2 2]"   "(mapcat (fn [x] [x x]) [1 2])"]
  ["mapcat two colls"       "[1 3 2 4]"   "(mapcat vector [1 2] [3 4])"]
  ["mapcat three colls"     "[1 2 3]"     "(mapcat vector [1] [2] [3])"]
  ["mapcat empty coll"      "()"          "(mapcat vector [] [1 2] [3 4])"]
  ["mapcat seqs"            "[1 2 3 4]"   "(mapcat identity [[1 2] [3 4]])"]
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

# Strictness: these reject malformed arguments like Clojure.
(defspec "seq / strictness (throws like Clojure)"
  ["cons non-seqable num" :throws "(cons 1 42)"]
  ["cons non-seqable kw"  :throws "(cons 1 :k)"]
  ["cons onto nil ok"     "[1]"   "(cons 1 nil)"]
  ["cons onto seq ok"     "[0 1 2]" "(cons 0 [1 2])"]
  ["num non-number"       :throws "(num \"x\")"]
  ["num ok"               "5"     "(num 5)"]
  ["realized? on number"  :throws "(realized? 1)"]
  ["realized? on nil"     :throws "(realized? nil)"]
  ["symbol from nil"      :throws "(symbol nil)"]
  ["symbol bad 2-arg"     :throws "(symbol :a \"b\")"]
  ["symbol from keyword"  "\"x\"" "(name (symbol :x))"]
  ["keyword bad 2-arg"    :throws "(keyword \"abc\" nil)"]
  ["keyword from symbol"  "\"x\"" "(name (keyword 'x))"])

# Stack/accessor strictness: peek/pop are stack-only; vec needs a seqable;
# key/val need a map entry.
(defspec "seq / accessor strictness"
  ["peek vector"        "3"      "(peek [1 2 3])"]
  ["peek list"          "1"      "(peek '(1 2 3))"]
  ["peek empty vec"     "nil"    "(peek [])"]
  ["peek on set"        :throws  "(peek #{1 2})"]
  ["peek on number"     :throws  "(peek 42)"]
  ["pop empty vec"      :throws  "(pop [])"]
  ["pop on number"      :throws  "(pop 0)"]
  ["pop vector"         "[1 2]"  "(pop [1 2 3])"]
  ["vec on number"      :throws  "(vec 42)"]
  ["vec on keyword"     :throws  "(vec :a)"]
  ["vec ok"             "[1 2]"  "(vec '(1 2))"]
  ["key on nil"         :throws  "(key nil)"]
  ["key on map"         :throws  "(key {})"]
  ["val on number"      :throws  "(val 0)"]
  ["key of entry"       ":a"     "(key (first {:a 1}))"]
  ["val of entry"       "1"      "(val (first {:a 1}))"])

# More strictness: first/rseq on the right shapes, assoc even-arg requirement.
(defspec "seq / more strictness"
  ["first on number"    :throws "(first 42)"]
  ["first on keyword"   :throws "(first :a)"]
  ["first ok vec"       "1"     "(first [1 2])"]
  ["first ok nil"       "nil"   "(first nil)"]
  ["rseq vector"        "[3 2 1]" "(rseq [1 2 3])"]
  ["rseq on string"     :throws "(rseq \"ab\")"]
  ["rseq on map"        :throws "(rseq {:a 1})"]
  ["rseq on number"     :throws "(rseq 0)"]
  ["assoc odd args"     :throws "(assoc {:a 1} :b)"]
  ["assoc on number"    :throws "(assoc 5 :a 1)"]
  ["assoc on set"       :throws "(assoc #{} :a 1)"])

# Strictness on more core fns: seq/shuffle need seqables, NaN? needs a number,
# nthrest/nthnext need a numeric count (and clamp negatives / accept nil coll).
(defspec "seq / strictness round 3"
  ["seq on number"      :throws "(seq 1)"]
  ["seq on fn"          :throws "(seq (fn [] 1))"]
  ["seq vector ok"      "[1 2]" "(seq [1 2])"]
  ["NaN? on nil"        :throws "(NaN? nil)"]
  ["NaN? on number ok"  "false" "(NaN? 1.0)"]
  ["shuffle on number"  :throws "(shuffle 1)"]
  ["shuffle on string"  :throws "(shuffle \"abc\")"]
  ["shuffle vec ok"     "3"     "(count (shuffle [1 2 3]))"]
  ["nthrest nil count"  :throws "(nthrest [0 1 2] nil)"]
  ["nthrest negative"   "[0 1 2]" "(nthrest [0 1 2] -1)"]
  ["nthrest nil coll"   "nil"   "(nthrest nil 0)"]
  ["nthnext nil count"  :throws "(nthnext [0 1 2] nil)"]
  ["update vec oob"     :throws "(update [] 1 identity)"]
  ["update vec kw key"  :throws "(update [1 2 3] :k identity)"])

# Regression cases for clojure.core fns moved from Janet to the Clojure overlay
# (jolt-1j0), plus two bugs fixed in the process: nthrest returns () (not nil)
# for an exhausted n>0 walk, and distinct? compares by VALUE (equal collections
# are not distinct).
(defspec "seq / overlay-migrated fns"
  ["nthrest exhausted -> ()"   "()"     "(nthrest nil 100)"]
  ["nthrest vec exhausted"     "()"     "(nthrest [1 2 3] 100)"]
  ["nthrest n<=0 keeps coll"   "[1 2 3]" "(nthrest [1 2 3] 0)"]
  ["nthrest drops n"           "[3 4 5]" "(nthrest [1 2 3 4 5] 2)"]
  ["nthnext exhausted -> nil"  "nil"    "(nthnext [1 2] 5)"]
  ["nthnext surprising nil"    "nil"    "(nthnext nil nil)"]
  ["nthnext drops n"           "[3 4]"  "(nthnext [1 2 3 4] 2)"]
  ["distinct? distinct"        "true"   "(distinct? 1 2 3)"]
  ["distinct? dup"             "false"  "(distinct? 1 2 1)"]
  ["distinct? equal colls"     "false"  "(distinct? [1 2] [1 2])"]
  ["distinct? single"          "true"   "(distinct? 5)"]
  ["replace maps elements"     "[:a 2 :c 2]" "(replace {1 :a 3 :c} [1 2 3 2])"]
  ["replace preserves nil val" "[1 nil 3]"   "(replace {2 nil} [1 2 3])"]
  ["take-last"                 "[3 4]"  "(take-last 2 [1 2 3 4])"]
  ["take-last empty -> nil"    "nil"    "(take-last 2 [])"]
  ["take-last n>len"           "[1 2]"  "(take-last 9 [1 2])"]
  ["drop-last default 1"       "[1 2]"  "(drop-last [1 2 3])"]
  ["drop-last n"               "[1 2]"  "(drop-last 2 [1 2 3 4])"]
  ["split-with"                "[[2 4] [5 6]]" "(split-with even? [2 4 5 6])"]
  ["replicate"                 "[:x :x :x]" "(replicate 3 :x)"]
  ["bounded-count"             "5"      "(bounded-count 3 [1 2 3 4 5])"]
  ["bounded-count uncounted"   "3"      "(bounded-count 3 (filter odd? (range 100)))"]
  ["run! side effects"         "6"      "(let [a (atom 0)] (run! (fn [x] (swap! a + x)) [1 2 3]) @a)"]
  ["completing wraps rf"       "3"      "((completing +) 1 2)"]
  ["comparator <"              "[1 2 3]" "(sort (comparator <) [3 1 2])"]
  ["comparator >"              "[3 2 1]" "(sort (comparator >) [3 1 2])"]
  ["reductions"                "[1 3 6 10]" "(reductions + [1 2 3 4])"]
  ["reductions with init"      "[10 11 13 16]" "(reductions + 10 [1 2 3])"]
  ["reductions empty calls f"  "[0]"    "(reductions + [])"]
  ["reductions empty + init"   "[5]"    "(reductions + 5 [])"]
  ["tree-seq pre-order"        "[[1 [2] 3] 1 [2] 2 3]" "(tree-seq sequential? seq [1 [2] 3])"]
  ["some found"                "true"   "(some even? [1 3 4])"]
  ["some none -> nil"          "nil"    "(some even? [1 3 5])"]
  ["some keyword pred"         "7"      "(some :a [{:b 1} {:a 7}])"]
  ["some returns value"        "4"      "(some (fn [x] (when (even? x) x)) [1 3 4 5])"]
  ["flatten nested"            "[1 2 3 4 5]" "(flatten [1 [2 [3 4]] 5])"]
  ["flatten lists too"         "[1 2 3]" "(flatten [1 (list 2 3)])"]
  ["flatten scalar -> empty"   "[]"     "(flatten 5)"]
  ["interleave"                "[1 :a 2 :b]" "(interleave [1 2 3] [:a :b])"]
  ["interleave empty"          "[]"     "(interleave)"]
  ["rationalize identity"      "5"      "(rationalize 5)"]
  ["dedupe consecutive"        "[1 2 3 1]" "(dedupe [1 1 2 2 3 1 1])"]
  ["dedupe empty"              "[]"     "(dedupe [])"]
  ["dedupe no dups"            "[1 2 3]" "(dedupe [1 2 3])"])

# Clojure 1.11 vector-returning partition/split variants.
(defspec "seq / partitionv & splitv-at (1.11)"
  ["partitionv"         "[[1 2] [3 4]]"  "(partitionv 2 [1 2 3 4 5])"]
  ["partitionv elems are vectors" "true" "(every? vector? (partitionv 2 [1 2 3 4]))"]
  ["partitionv step"    "[[1 2] [3 4]]"  "(partitionv 2 2 [1 2 3 4 5])"]
  ["partitionv pad"     "[[1 2] [3 :p]]" "(partitionv 2 2 [:p] [1 2 3])"]
  ["partitionv-all"     "[[1 2] [3]]"    "(partitionv-all 2 [1 2 3])"]
  ["partitionv-all vectors" "true"       "(every? vector? (partitionv-all 2 [1 2 3]))"]
  ["splitv-at"          "[[1 2] [3 4]]"  "(splitv-at 2 [1 2 3 4])"]
  ["splitv-at first is vector" "true"    "(vector? (first (splitv-at 2 [1 2 3])))"]
  ["splitv-at past end" "[[1 2] []]"     "(splitv-at 5 [1 2])"])

# Linear seq walks (flatiron review find): cell chains over concrete
# collections walk by INDEX. Slicing the remainder per step made every full
# walk O(n^2) — mapv over 40k elements took ten seconds, a 20k first/rest
# loop took two. rest/next of an indexed collection is an O(1) lazy view —
# and therefore a SEQ, as in Clojure (it was a vector-typed slice).
(defspec "sequences / linear walks over concrete collections"
  ["rest of vector is a seq"   "true"  "(seq? (rest [1 2 3]))"]
  ["rest of vector not vector" "false" "(vector? (rest [1 2 3]))"]
  ["rest values"               "(quote (2 3))" "(rest [1 2 3])"]
  ["next of vector"            "(quote (2 3))" "(next [1 2 3])"]
  ["next exhausts to nil"      "nil"   "(next [1])"]
  ["rest exhausts to ()"       "()"    "(rest [1])"]
  ["rest-loop scales (20k linear)" "20000"
   "(loop [xs (seq (vec (range 20000))) n 0] (if xs (recur (next xs) (inc n)) n))"]
  ["mapv scales (50k linear)"  "50000" "(count (mapv inc (vec (range 50000))))"]
  ["nested walk"               "[2 3]" "(vec (rest (mapv inc [0 1 2])))"])

# rest/next over sets, maps, and sorted colls used to fall into the indexed
# fall-through and walk the wrapper table's internal fields ((next #{1 2})
# was (nil nil)) — exposed when the canonical every? started seq-walking.
(defspec "sequences / rest & next over set-like colls"
  ["next of set"        "true"  "(let [n (next #{1 2})] (and (= 1 (count n)) (contains? #{1 2} (first n))))"]
  ["rest of set count"  "1"     "(count (rest #{1 2}))"]
  ["next of singleton set" "nil" "(next #{1})"]
  ["rest of empty set"  "0"     "(count (rest #{}))"]
  ["next of map"        "true"  "(let [n (next {:a 1 :b 2})] (and (= 1 (count n)) (map-entry? (first n))))"]
  ["next of singleton map" "nil" "(next {:a 1})"]
  ["rest of sorted-set" "(quote (2 3))" "(rest (sorted-set 3 1 2))"]
  ["next of sorted-map" "(quote ([2 :b]))" "(next (sorted-map 1 :a 2 :b))"]
  ["every? over set"    "true"  "(every? pos? #{1 2 3})"]
  ["every? over set false" "false" "(every? odd? #{1 2})"]
  ["every? over sorted-set" "true" "(every? pos? (sorted-set 1 2 3))"]
  ["every? over map entries" "true" "(every? map-entry? (seq {:a 1 :b 2}))"])
