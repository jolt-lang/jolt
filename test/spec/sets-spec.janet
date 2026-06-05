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
  ["equality order-indep"   "true"      "(= #{1 2 3} #{3 2 1})"])

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
