# Specification: maps (associative).
(use ../support/harness)

(defspec "map / construct & predicate"
  ["literal"                "{:a 1}"          "{:a 1}"]
  ["hash-map"               "{:a 1, :b 2}"    "(hash-map :a 1 :b 2)"]
  ["empty"                  "{}"              "{}"]
  ["map? true"              "true"            "(map? {:a 1})"]
  ["map? false on vector"   "false"           "(map? [1 2])"]
  ["count"                  "2"               "(count {:a 1 :b 2})"]
  ["empty? true"            "true"            "(empty? {})"]
  ["equality order-indep"   "true"            "(= {:a 1 :b 2} {:b 2 :a 1})"])

(defspec "map / access"
  ["get"                    "1"               "(get {:a 1} :a)"]
  ["get missing nil"        "nil"             "(get {:a 1} :z)"]
  ["get default"            ":x"              "(get {:a 1} :z :x)"]
  ["keyword as fn"          "1"               "(:a {:a 1})"]
  ["keyword fn default"     ":x"              "(:z {:a 1} :x)"]
  ["map as fn"              "1"               "({:a 1} :a)"]
  ["get-in"                 "2"               "(get-in {:a {:b 2}} [:a :b])"]
  ["get-in missing"         "nil"             "(get-in {:a {}} [:a :b])"]
  ["contains? key"          "true"            "(contains? {:a 1} :a)"]
  ["contains? missing"      "false"           "(contains? {:a 1} :z)"]
  ["find returns entry"     "[:a 1]"          "(find {:a 1} :a)"]
  ["keys"                   "true"            "(= #{:a :b} (set (keys {:a 1 :b 2})))"]
  ["vals"                   "true"            "(= #{1 2} (set (vals {:a 1 :b 2})))"])

(defspec "map / update"
  ["assoc adds"             "{:a 1, :b 2}"    "(assoc {:a 1} :b 2)"]
  ["assoc overwrites"       "{:a 9}"          "(assoc {:a 1} :a 9)"]
  ["assoc many"             "{:a 1, :b 2}"    "(assoc {} :a 1 :b 2)"]
  ["dissoc"                 "{:a 1}"          "(dissoc {:a 1 :b 2} :b)"]
  ["dissoc many"            "{:a 1}"          "(dissoc {:a 1 :b 2 :c 3} :b :c)"]
  ["merge"                  "{:a 1, :b 2}"    "(merge {:a 1} {:b 2})"]
  ["merge overwrites"       "{:a 2}"          "(merge {:a 1} {:a 2})"]
  ["merge-with"             "{:a 3}"          "(merge-with + {:a 1} {:a 2})"]
  ["update"                 "{:a 2}"          "(update {:a 1} :a inc)"]
  ["update missing w/ fnil" "{:a 1}"          "(update {} :a (fnil inc 0))"]
  ["update-in"              "{:a {:b 2}}"     "(update-in {:a {:b 1}} [:a :b] inc)"]
  ["assoc-in"              "{:a {:b 1}}"      "(assoc-in {} [:a :b] 1)"]
  ["select-keys"            "{:a 1}"          "(select-keys {:a 1 :b 2} [:a])"]
  ["into onto map"          "{:a 1, :b 2}"    "(into {:a 1} [[:b 2]])"]
  ["zipmap"                 "{:a 1, :b 2}"    "(zipmap [:a :b] [1 2])"])

(defspec "map / iteration & entries"
  ["map over entries"       "true"            "(= #{1 2} (set (map val {:a 1 :b 2})))"]
  ["map keys"               "true"            "(= #{:a :b} (set (map key {:a 1 :b 2})))"]
  ["reduce over entries"    "6"               "(reduce (fn [a e] (+ a (val e))) 0 {:a 1 :b 2 :c 3})"]
  ["reduce-kv"              "6"               "(reduce-kv (fn [a k v] (+ a v)) 0 {:a 1 :b 2 :c 3})"]
  ["destructure entry"      "true"            "(= [[:a 2]] (into [] (map (fn [[k v]] [k (inc v)]) {:a 1})))"]
  ["first of map is entry"  "true"            "(let [e (first {:a 1})] (and (= (key e) :a) (= (val e) 1)))"]
  ["map-entry?"             "true"            "(map-entry? (first {:a 1}))"]
  ["count of nil map"       "0"               "(count nil)"]
  ["get from nil"           "nil"             "(get nil :a)"]
  ["immutability"           "true"            "(let [m {:a 1} n (assoc m :b 2)] (and (= m {:a 1}) (= n {:a 1 :b 2})))"])
