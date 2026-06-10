# Specification: UUID support — random-uuid, parse-uuid, uuid?, #uuid literal.
# UUIDs are immutable tagged structs {:jolt/type :jolt/uuid :str "<lowercase>"}:
# value equality, usable as map keys, case-normalized at construction.
(use ../support/harness)

(defspec "uuid / random-uuid"
  ["returns a uuid"      "true"  "(uuid? (random-uuid))"]
  ["str is 36 chars"     "36"    "(count (str (random-uuid)))"]
  ["8-4-4-4-12 shape"    "[8 4 4 4 12]" "(do (require (quote [clojure.string :as s])) (mapv count (s/split (str (random-uuid)) #\"-\")))"]
  ["version nibble is 4" "\\4"   "(nth (str (random-uuid)) 14)"]
  ["variant nibble 8-b"  "true"  "(contains? #{\\8 \\9 \\a \\b} (nth (seq (str (random-uuid))) 19))"]
  ["distinct"            "10"    "(count (set (repeatedly 10 random-uuid)))"]
  ["all hex digits"      "true"  "(every? (fn [c] (contains? (set (seq \"0123456789abcdef-\")) c)) (seq (str (random-uuid))))"])

(defspec "uuid / parse-uuid"
  ["valid round-trips"   "\"b6883c0a-0342-4007-9966-bc2dfa6b109e\""
   "(str (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\"))"]
  ["parses to uuid"      "true"  "(uuid? (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\"))"]
  ["case-insensitive ="  "true"
   "(= (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\") (parse-uuid \"B6883C0A-0342-4007-9966-BC2DFA6B109E\"))"]
  ["empty -> nil"        "nil"   "(parse-uuid \"\")"]
  ["short -> nil"        "nil"   "(parse-uuid \"0\")"]
  ["garbage -> nil"      "nil"   "(parse-uuid \"df0993\")"]
  ["too long -> nil"     "nil"   "(parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109eb\")"]
  ["leading extra -> nil" "nil"  "(parse-uuid \"ab6883c0a-0342-4007-9966-bc2dfa6b109e\")"]
  ["non-hex -> nil"      "nil"   "(parse-uuid \"g6883c0a-0342-4007-9966-bc2dfa6b109e\")"]
  ["bad dashes -> nil"   "nil"   "(parse-uuid \"b6883c0a00342-4007-9966-bc2dfa6b109e\")"]
  ["non-string throws"   :throws "(parse-uuid 1000)"]
  ["keyword throws"      :throws "(parse-uuid :key)"]
  ["map throws"          :throws "(parse-uuid {})"])

(defspec "uuid / value semantics"
  ["equal by value"      "true"
   "(= (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\") (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\"))"]
  ["unequal differs"     "false"
   "(= (random-uuid) (random-uuid))"]
  ["works as map key"    ":v"
   "(let [u (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")] (get {u :v} (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")))"]
  ["works in a set"      "true"
   "(contains? #{(parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")} (parse-uuid \"B6883C0A-0342-4007-9966-BC2DFA6B109E\"))"]
  ["uuid? false on string" "false" "(uuid? \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")"]
  ["uuid? false on nil"  "false" "(uuid? nil)"])

(defspec "uuid / #uuid reader literal"
  ["reads to uuid"       "true"  "(uuid? #uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")"]
  ["= parse-uuid"        "true"
   "(= #uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\" (parse-uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\"))"]
  ["str of literal"      "\"b6883c0a-0342-4007-9966-bc2dfa6b109e\""
   "(str #uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")"]
  ["pr-str round-trips"  "\"#uuid \\\"b6883c0a-0342-4007-9966-bc2dfa6b109e\\\"\""
   "(pr-str #uuid \"b6883c0a-0342-4007-9966-bc2dfa6b109e\")"])
