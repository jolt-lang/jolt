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
  ["merge lattermost wins"  "{:a 3}"          "(merge {:a 1} {:a 2} {:a 3})"]
  ["merge no args -> nil"   "nil"             "(merge)"]
  ["merge all nil -> nil"   "nil"             "(merge nil nil)"]
  ["merge nil arg no-op"    "{:a 1}"          "(merge {:a 1} nil)"]
  ["merge nil then map"     "{:a 1}"          "(merge nil {:a 1})"]
  ["merge empty + nil"      "{}"              "(merge {} nil)"]
  ["merge map-entry (conj)" "{:a 1}"          "(merge {} (first {:a 1}))"]
  ["merge [k v] vector"     "{:foo 1}"        "(merge {} [:foo 1])"]
  ["merge collection key"   "true"            "(= {[2 3] :foo} (merge {[2 3] :foo} nil {}))"]
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

(defspec "map / collection keys (by value)"
  ["vector key literal"     ":v"              "(get {[1 2] :v} [1 2])"]
  ["map key literal"        ":v"              "(get {(hash-map :a 1) :v} {:a 1})"]
  ["assoc vector key"       ":v"              "(get (assoc {} [1 2] :v) [1 2])"]
  ["key across repr"        ":v"              "(get (assoc {} (vec [1 2]) :v) [1 2])"]
  ["frequencies of maps"    "2"               "(get (frequencies [{:a 1} (hash-map :a 1)]) {:a 1})"]
  ["group-by collection key" "1"              "(count (group-by identity [{:a 1} (hash-map :a 1)]))"])

# Strictness: assoc bounds-checks vector indices; dissoc requires a map;
# count rejects scalars; numerator/denominator have no ratio type.
(defspec "map / strictness (throws like Clojure)"
  ["assoc vec out of bounds" :throws "(assoc [0 1 2] 4 4)"]
  ["assoc vec negative"      :throws "(assoc [] -1 0)"]
  ["assoc vec at count ok"   "[1 2 3]" "(assoc [1 2] 2 3)"]
  ["dissoc on number"        :throws "(dissoc 42 :a)"]
  ["dissoc on vector"        :throws "(dissoc [1 2] 0)"]
  ["dissoc on set"           :throws "(dissoc #{:a} :a)"]
  ["dissoc nil ok"           "nil"   "(dissoc nil :a)"]
  ["count on number"         :throws "(count 1)"]
  ["count on keyword"        :throws "(count :a)"]
  ["count string ok"         "3"     "(count \"abc\")"]
  ["numerator throws"        :throws "(numerator 1)"]
  ["denominator throws"      :throws "(denominator 2)"]
  ["subvec out of range"     :throws "(subvec [0 1 2 3] 1 5)"]
  ["subvec start>end"        :throws "(subvec [0 1 2 3] 3 2)"]
  ["subvec ok"               "[1 2]" "(subvec [0 1 2 3] 1 3)"]
  ["min-key empty"           :throws "(apply min-key identity [])"]
  ["merge empty vector"      :throws "(merge {} [])"]
  ["merge 1-elem vector"     :throws "(merge {} [:foo])"]
  ["merge atomic arg"        :throws "(merge {} :foo)"]
  ["merge [k v] ok"          "{:foo 1}" "(merge {} [:foo 1])"]
  ["merge maps ok"           "{:a 1, :b 2}" "(merge {:a 1} {:b 2})"])

# Map entries are distinct from plain vectors (key/val/map-entry? reject a
# vector); min-key/max-key follow Clojure's NaN-aware ordering; subvec coerces
# float/NaN indices like (int ...).
(defspec "map / map-entry & key ordering"
  ["key of entry"        ":a"     "(key (first {:a 1}))"]
  ["val of entry"        "1"      "(val (first {:a 1}))"]
  ["key rejects vector"  :throws  "(key [:a 1])"]
  ["val rejects vector"  :throws  "(val [:a 1])"]
  ["map-entry? entry"    "true"   "(map-entry? (first {:a 1}))"]
  ["map-entry? vector"   "false"  "(map-entry? [:a 1])"]
  ["min-key NaN first"   "1"      "(min-key identity ##NaN 1)"]
  ["min-key NaN last"    "true"   "(NaN? (min-key identity 1 ##NaN))"]
  ["min-key NaN three"   "true"   "(infinite? (min-key identity ##NaN ##-Inf 1))"]
  ["min-key keys nonnum" :throws  "(min-key identity \"x\" \"y\")"]
  ["max-key picks max"   "[1 2 3]" "(max-key count [1] [1 2 3] [1 2])"]
  ["subvec float trunc"  "[0]"    "(subvec [0 1 2] 0.5 1.33)"]
  ["subvec NaN start"    "[0 1 2]" "(subvec [0 1 2] ##NaN 3)"]
  ["subvec NaN end"      "[]"     "(subvec [0 1 2] 0 ##NaN)"])

# A nil value is a PRESENT key in Clojure (distinct from a missing key); Janet
# structs drop nil, so jolt builds these maps as a phm. Tested via literals (the
# reader path) and the construction/op surface, in every spec mode.
(defspec "map / nil values preserved"
  ["literal contains"      "true"   "(contains? {:b nil} :b)"]
  ["literal not= empty"    "false"  "(= {:b nil} {})"]
  ["literal get nil"       "nil"    "(get {:b nil} :b :x)"]
  ["literal keys incl nil" "true"   "(= #{:a :b} (set (keys {:a nil :b 1})))"]
  ["literal count"         "2"      "(count {:a nil :b 1})"]
  ["literal vals incl nil" "2"      "(count (vals {:a nil :b 1}))"]
  ["eval values w/ nil"    "3"      "(:a {:a (+ 1 2) :b nil})"]
  ["nil key present"       "true"   "(contains? {nil :v} nil)"]
  ["assoc nil present"     "true"   "(contains? (assoc {:a 1} :b nil) :b)"]
  ["assoc nil get"         "nil"    "(get (assoc {:a 1} :b nil) :b :x)"]
  ["assoc overwrite nil"   "nil"    "(get (assoc {:a 1} :a nil) :a :x)"]
  ["hash-map nil"          "true"   "(contains? (hash-map :b nil) :b)"]
  ["merge new nil"         "true"   "(contains? (merge {:a 1} {:b nil}) :b)"]
  ["merge overwrite nil"   "nil"    "(get (merge {:a 1} {:a nil}) :a :x)"]
  ["merge-with present nil" "true"  "(= [nil 1] (get (merge-with (fn [a b] [a b]) {:a nil} {:a 1}) :a))"]
  ["into nil val"          "true"   "(contains? (into {} [[:a nil]]) :a)"]
  ["conj map nil"          "true"   "(contains? (conj {:x 1} {:a nil}) :a)"]
  ["zipmap nil"            "true"   "(contains? (zipmap [:a] [nil]) :a)"]
  ["select-keys nil"       "true"   "(contains? (select-keys {:a nil} [:a]) :a)"]
  ["get-in present nil"    "nil"    "(get-in {:a nil} [:a] :x)"]
  ["get-in through nil"    ":x"     "(get-in {:a nil} [:a :b] :x)"]
  ["dissoc keeps nil"      "true"   "(contains? (dissoc {:a nil :b 1} :b) :a)"]
  ["reduce-kv sees nil"    "true"   "(= #{:a :b} (reduce-kv (fn [acc k v] (conj acc k)) #{} {:a nil :b 2}))"]
  ["nil-free stays fast"   "true"   "(= {:a 1 :b 2} {:b 2 :a 1})"])

# Clojure 1.11 map transformers.
(defspec "map / update-keys & update-vals (1.11)"
  ["update-keys"        "{\"a\" 1, \"b\" 2}" "(update-keys {:a 1 :b 2} name)"]
  ["update-keys empty"  "{}"      "(update-keys {} inc)"]
  ["update-keys nil"    "{}"      "(update-keys nil str)"]
  ["update-keys collide last wins" "1" "(count (update-keys {:a 1 :b 2} (fn [_] :k)))"]
  ["update-vals"        "{:a 2, :b 3}" "(update-vals {:a 1 :b 2} inc)"]
  ["update-vals empty"  "{}"      "(update-vals {} inc)"]
  ["update-vals nil"    "{}"      "(update-vals nil inc)"]
  ["update-vals keeps keys" "[:a :b]" "(sort (keys (update-vals {:a 1 :b 2} inc)))"])

# keys/vals/empty? are 00-syntax overlay fns now (jolt-4j3) — expander-called,
# so they live in the first tier and get the staged defn recompile.
(defspec "maps / keys-vals-empty? as overlay fns"
  ["keys"            "(quote (:a))"   "(keys {:a 1})"]
  ["keys empty map"  "nil"            "(keys {})"]
  ["keys nil"        "nil"            "(keys nil)"]
  ["vals"            "(quote (1))"    "(vals {:a 1})"]
  ["vals empty"      "nil"            "(vals {})"]
  ["keys sorted order" "[1 2 3]"      "(vec (keys (sorted-map 2 :b 1 :a 3 :c)))"]
  ["vals sorted order" "[:a :b :c]"   "(vec (vals (sorted-map 2 :b 1 :a 3 :c)))"]
  ["keys/vals zip"   "{:a 1 :b 2}"    "(zipmap (keys {:a 1 :b 2}) (vals {:a 1 :b 2}))"]
  ["empty? map"      "true"           "(empty? {})"]
  ["empty? vec"      "[true false]"   "[(empty? []) (empty? [1])]"]
  ["empty? list"     "[true false]"   "[(empty? ()) (empty? (list 1))]"]
  ["empty? string"   "[true false]"   "[(empty? \"\") (empty? \"a\")]"]
  ["empty? nil"      "true"           "(empty? nil)"]
  ["empty? set"      "[true false]"   "[(empty? #{}) (empty? #{1})]"]
  ["empty? lazy"     "[true false]"   "[(empty? (filter pos? [-1])) (empty? (map inc [1]))]"]
  ["empty? lazy nil elem" "false"     "(empty? (cons nil nil))"]
  ["empty? sorted"   "[true false]"   "[(empty? (sorted-map)) (empty? (sorted-set 1))]"]
  ["empty? number throws" :throws     "(empty? 5)"])

# (assoc nil k v) yields a real immutable map, not a raw host table, so
# assoc-in into absent keys nests countable/seqable maps (jolt-w4s).
(defspec "map / assoc on nil"
  ["assoc nil is a map"        "{:a 1}" "(assoc nil :a 1)"]
  ["count of assoc nil"        "1"      "(count (assoc nil :a 1))"]
  ["assoc-in nested countable" "1"      "(count (:a (assoc-in {} [:a :b] 1)))"]
  ["assoc-in deep get"         "9"      "(get-in (assoc-in {} [:a :b :c] 9) [:a :b :c])"]
  ["seq over assoc-nil map"    ":a"     "(ffirst (seq (assoc nil :a 1)))"]
  ["keys of assoc-nil map"     "[:a]"   "(vec (keys (assoc nil :a 1)))"])
