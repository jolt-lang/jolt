# Specification: transients (mutable scratch collections frozen by persistent!).
(use ../support/harness)

(defspec "transient / vector"
  ["conj! then persistent!" "[1 2]"     "(persistent! (conj! (conj! (transient []) 1) 2))"]
  ["reduce conj!"           "[0 1 2 3 4]" "(persistent! (reduce conj! (transient []) (range 5)))"]
  ["conj! many args"        "[1 2 3]"    "(persistent! (conj! (transient [1]) 2 3))"]
  ["assoc! existing"        "[1 9 3]"    "(persistent! (assoc! (transient [1 2 3]) 1 9))"]
  ["assoc! at count grows"  "[1 2 3]"    "(persistent! (assoc! (transient [1 2]) 2 3))"]
  ["pop!"                   "[1 2]"      "(persistent! (pop! (transient [1 2 3])))"]
  ["from existing vector"   "[1 2 3 4]"  "(persistent! (conj! (transient [1 2 3]) 4))"]
  ["count"                  "3"          "(count (transient [1 2 3]))"]
  ["nth"                    "2"          "(nth (transient [1 2 3]) 1)"]
  ["get"                    "2"          "(get (transient [1 2 3]) 1)"]
  ["persistent! is a vector" "true"      "(vector? (persistent! (transient [1])))"]
  ["transient? true"        "true"       "(transient? (transient []))"]
  ["transient? false"       "false"      "(transient? [1 2])"])

(defspec "transient / map"
  ["assoc! then persistent!" "{:a 1, :b 2}" "(persistent! (assoc! (assoc! (transient {}) :a 1) :b 2))"]
  ["assoc! many"            "{:a 1, :b 2}" "(persistent! (assoc! (transient {}) :a 1 :b 2))"]
  ["dissoc!"                "{:b 2}"     "(persistent! (dissoc! (transient {:a 1 :b 2}) :a))"]
  ["conj! map entry"        "{:a 1}"     "(persistent! (conj! (transient {}) [:a 1]))"]
  ["from existing map"      "{:a 1, :b 2}" "(persistent! (assoc! (transient {:a 1}) :b 2))"]
  ["get"                    "1"          "(get (transient {:a 1}) :a)"]
  ["get missing default"    ":x"         "(get (transient {:a 1}) :z :x)"]
  ["contains?"              "true"       "(contains? (transient {:a 1}) :a)"]
  ["count"                  "2"          "(count (transient {:a 1 :b 2}))"]
  ["collection key by value" ":v"        "(get (persistent! (assoc! (transient {}) [1 2] :v)) [1 2])"]
  ["persistent! is a map"   "true"       "(map? (persistent! (transient {:a 1})))"]
  ["reduce build"           "{0 0, 1 1, 2 2}" "(persistent! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 3)))"])

(defspec "transient / set"
  ["conj! dedups"           "#{1 2 3}"   "(persistent! (conj! (transient #{}) 1 2 2 3))"]
  ["disj!"                  "#{1 3}"     "(persistent! (disj! (transient #{1 2 3}) 2))"]
  ["from existing set"      "#{1 2 3}"   "(persistent! (conj! (transient #{1 2}) 3))"]
  ["contains?"              "true"       "(contains? (transient #{1 2}) 1)"]
  ["count"                  "2"          "(count (transient #{1 2}))"]
  ["persistent! is a set"   "true"       "(set? (persistent! (transient #{1})))"]
  ["map elements by value"  "1"          "(count (persistent! (conj! (transient #{}) {:a 1} (hash-map :a 1))))"])

(defspec "transient / immutability of source"
  ["source vector unchanged" "true"
   "(let [v [1 2 3] _ (persistent! (conj! (transient v) 4))] (= v [1 2 3]))"]
  ["source map unchanged"    "true"
   "(let [m {:a 1} _ (persistent! (assoc! (transient m) :b 2))] (= m {:a 1}))"])

# Transients are invokable for read-only lookup, like their persistent forms.
(defspec "transient / invokable lookup"
  ["vector index"        "20"   "((transient [10 20 30]) 1)"]
  ["map key as fn"       "7"    "((transient {:x 7}) :x)"]
  ["map key default"     "99"   "((transient {:x 7}) :z 99)"]
  ["keyword on transient" "7"   "(:x (transient {:x 7}))"]
  ["set membership"      "2"    "((transient #{1 2 3}) 2)"]
  ["set miss default"    ":no"  "((transient #{1 2 3}) 42 :no)"]
  ["collection key"      ":v"   "((transient {[1 2] :v}) [1 2])"])

# assoc! requires an even key/val arg count, like assoc and Clojure's assoc!: a
# dangling key with no value throws (jolt-ea9k fixed the former lenient
# nil-fill, which was non-Clojure and inconsistent with jolt's own plain assoc).
(defspec "transient / assoc! odd args throw"
  ["map dangling key"   :throws "(persistent! (assoc! (transient {}) :a 1 :b))"]
  ["map lone key"       :throws "(persistent! (assoc! (transient {}) :a))"]
  ["vector dangling"    :throws "(persistent! (apply assoc! (transient []) [0 9 1]))"]
  ["even args still ok" "true"
   "(= {:a 1, :b 2} (persistent! (assoc! (transient {}) :a 1 :b 2)))"])

# Using a transient after persistent! (or popping an empty one) throws.
(defspec "transient / invalidation"
  ["conj! after persistent!" :throws
   "(let [t (transient [])] (persistent! t) (conj! t 1))"]
  ["assoc! after persistent!" :throws
   "(let [t (transient {})] (persistent! t) (assoc! t :a 1))"]
  ["persistent! twice"       :throws
   "(let [t (transient [])] (persistent! t) (persistent! t))"]
  ["pop! empty"              :throws "(pop! (transient []))"])

# The bang ops require an appropriate transient (Clojure throws otherwise);
# conj! has the special 0-/1-arg identity arities.
(defspec "transient / strictness"
  ["conj! on persistent"   :throws "(conj! [1 2] 3)"]
  ["assoc! on persistent"  :throws "(assoc! {:a 1} :b 2)"]
  ["persistent! on vector" :throws "(persistent! [1 2])"]
  ["persistent! on nil"    :throws "(persistent! nil)"]
  ["pop! on transient map" :throws "(pop! (transient {:a 1}))"]
  ["dissoc! on tset"       :throws "(dissoc! (transient #{1}) 1)"]
  ["conj! map bad item"    :throws "(conj! (transient {}) #{:a 1})"]
  ["conj! no args"         "[]"    "(persistent! (conj!))"]
  ["conj! identity"        "[1 2]" "(conj! [1 2])"]
  ["conj! map merges map"  "{:a 1, :b 2}" "(persistent! (conj! (transient {:a 1}) {:b 2}))"])

(defspec "transient / assoc! bounds"
  ["assoc! existing idx"  "[1 9 3]" "(persistent! (assoc! (transient [1 2 3]) 1 9))"]
  ["assoc! at count grows" "[1 2 3]" "(persistent! (assoc! (transient [1 2]) 2 3))"]
  ["assoc! out of bounds" :throws   "(assoc! (transient [0 1 2]) 4 4)"]
  ["assoc! negative"      :throws   "(assoc! (transient []) -1 0)"])
