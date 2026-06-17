# Specification: sets, including clojure.set.
(use ../support/harness)

(defspec "set / construct & predicate"
  ["literal"                "#{1 2 3}"  "#{1 2 3}"]
  ["hash-set"               "#{1 2 3}"  "(hash-set 1 2 3)"]
  ["set from vector"        "#{1 2 3}"  "(set [1 2 3 1])"]
  ["empty"                  "#{}"       "#{}"]
  ["set? true"              "true"      "(set? #{1})"]
  ["set? false on vector"   "false"     "(set? [1])"]
  ["count dedups"           "3"         "(count (set [1 1 2 3]))"]
  ["equality order-indep"   "true"      "(= #{1 2 3} #{3 2 1})"]
  # jolt-h86: into-conj had no set branch and returned the set unchanged
  ["into set"               "#{:a :b}"  "(into #{} [:a :b])"]
  ["into non-empty set"     "#{1 2 3}"  "(into #{1} [2 3 2])"])

(defspec "set / operations"
  ["conj adds"              "#{1 2 3}"  "(conj #{1 2} 3)"]
  ["conj dup no-op"         "#{1 2}"    "(conj #{1 2} 1)"]
  ["disj removes"           "#{1 2}"    "(disj #{1 2 3} 3)"]
  ["disj missing no-op"     "#{1 2}"    "(disj #{1 2} 9)"]
  ["contains?"              "true"      "(contains? #{1 2} 1)"]
  ["contains? missing"      "false"     "(contains? #{1 2} 9)"]
  ["get present"            "1"         "(get #{1 2} 1)"]
  ["get missing nil"        "nil"       "(get #{1 2} 9)"]
  ["set as fn present"      "2"         "(#{1 2 3} 2)"]
  ["set as fn missing"      "nil"       "(#{1 2 3} 9)"])

(defspec "set / literals & value elements"
  ["literal evaluates elements" "#{2 4}" "#{(inc 1) (* 2 2)}"]
  ["map elements by value"  "true"      "(= #{{:a 1}} #{(hash-map :a 1)})"]
  ["contains? map by value" "true"      "(contains? #{(hash-map :x 1)} {:x 1})"]
  ["dedup equal maps"       "1"         "(count (set [{:a 1} (hash-map :a 1)]))"]
  ["vector elements"        "true"      "(contains? #{[1 2]} (vec [1 2]))"])

(defspec "set / nil element (jolt-bn2p)"
  # canon-key returns nil for nil and Janet tables drop a nil key, so a nil
  # member used to be silently lost while count/contains? disagreed.
  ["set keeps nil"            "2"     "(count (set [nil 1 nil]))"]
  ["contains? nil true"       "true"  "(contains? (set [nil 1]) nil)"]
  ["contains? nil false"      "false" "(contains? #{1} nil)"]
  ["seq includes nil"         "true"  "(some nil? (seq (set [nil 1])))"]
  ["disj nil"                 "#{1}"  "(disj (set [nil 1]) nil)"]
  ["disj nil count"           "1"     "(count (disj (set [nil 1]) nil))"]
  ["conj nil count"           "2"     "(count (conj #{1} nil))"]
  ["conj nil contains?"       "true"  "(contains? (conj #{1} nil) nil)"]
  ["into #{} keeps nil"       "2"     "(count (into #{} [nil 1]))"]
  ["into #{} contains? nil"   "true"  "(contains? (into #{} [nil 1]) nil)"]
  ["into keeps existing nil"  "true"  "(contains? (into #{nil} [1]) nil)"]
  # transient set path: tr-conj!/persistent!/disj!/contains?
  ["transient conj! nil"      "2"     "(count (persistent! (conj! (transient #{}) nil 1)))"]
  ["transient contains? nil"  "true"  "(contains? (persistent! (conj! (transient #{}) nil 1)) nil)"]
  ["transient disj! nil cnt"  "1"     "(count (persistent! (disj! (conj! (transient #{}) nil 1) nil)))"]
  ["transient disj! removes"  "false" "(contains? (persistent! (disj! (conj! (transient #{}) nil 1) nil)) nil)"]
  ["transient of set w/ nil"  "true"  "(contains? (persistent! (transient (set [nil 1]))) nil)"])

(defspec "clojure.set"
  ["union"                  "#{1 2 3 4}" "(do (require (quote [clojure.set :as s])) (s/union #{1 2} #{3 4}))"]
  ["intersection"           "#{2}"       "(do (require (quote [clojure.set :as s])) (s/intersection #{1 2} #{2 3}))"]
  ["difference"             "#{1}"       "(do (require (quote [clojure.set :as s])) (s/difference #{1 2} #{2 3}))"]
  ["subset? true"           "true"      "(do (require (quote [clojure.set :as s])) (s/subset? #{1} #{1 2}))"]
  ["superset? true"         "true"      "(do (require (quote [clojure.set :as s])) (s/superset? #{1 2} #{1}))"]
  ["select"                 "#{2 4}"    "(do (require (quote [clojure.set :as s])) (s/select even? #{1 2 3 4}))"]
  ["join"                   "#{{:a 1, :b 2, :c 3}}" "(do (require (quote [clojure.set :as s])) (s/join #{{:a 1 :b 2}} #{{:b 2 :c 3}}))"]
  ["map-invert"             "{1 :a}"     "(do (require (quote [clojure.set :as s])) (s/map-invert {:a 1}))"]
  ["rename-keys"            "{:b 1}"     "(do (require (quote [clojure.set :as s])) (s/rename-keys {:a 1} {:a :b}))"])

# set? recognizes every set representation (jolt-dpn: sorted sets are tagged
# tables the host set? predicate missed).
(defspec "set / set? across representations"
  ["literal"        "true"  "(set? #{1})"]
  ["empty literal"  "true"  "(set? #{})"]
  ["sorted-set"     "true"  "(set? (sorted-set 1 2))"]
  ["sorted-set-by"  "true"  "(set? (sorted-set-by > 1 2))"]
  ["empty sorted"   "true"  "(set? (sorted-set))"]
  ["map is not"     "false" "(set? {})"]
  ["vector is not"  "false" "(set? [1])"]
  ["coll? still true" "true" "(coll? (sorted-set 1))"]
  ["ifn? sorted-set" "true" "(ifn? (sorted-set 1))"])

# set / into #{} bulk-build the backing HAMT in one pass (phs-from-seq), instead
# of a phs-conj per element (jolt-5vsp collections). Cross the promotion
# boundary, check dedup, collection members, and conj after a bulk build.
(defspec "set / bulk build boundaries"
  ["set dedup count"   "3"    "(count (set [1 1 2 3 3 2]))"]
  ["set big count"     "1000" "(count (set (range 1000)))"]
  ["into #{} count"    "500"  "(count (into #{} (range 500)))"]
  ["into #{} onto base" "3"   "(count (into #{:a} [:a :b :c]))"]
  ["set contains"      "true" "(contains? (set (range 1000)) 777)"]
  ["set missing"       "false" "(contains? (set (range 1000)) 5000)"]
  ["set coll members"  "true" "(contains? (set [[1 2] [3 4]]) [1 2])"]
  ["conj after bulk"   "true" "(contains? (conj (set (range 100)) :x) :x)"]
  ["disj after bulk"   "false" "(contains? (disj (set (range 100)) 50) 50)"]
  ["set = literal"     "true" "(= #{0 1 2} (set (range 3)))"])
