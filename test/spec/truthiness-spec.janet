# Specification: truthiness & boolean logic.
# The core Clojure rule: ONLY nil and false are logically false; every other
# value — including 0, 0.0, "", and empty collections — is logically true.
(use ../support/harness)

(defspec "truthiness / if (only nil & false are falsy)"
  ["nil is falsy"       ":f"   "(if nil :t :f)"]
  ["false is falsy"     ":f"   "(if false :t :f)"]
  ["zero is truthy"     ":t"   "(if 0 :t :f)"]
  ["zero float truthy"  ":t"   "(if 0.0 :t :f)"]
  ["empty string truthy" ":t"  "(if \"\" :t :f)"]
  ["empty list truthy"  ":t"   "(if (list) :t :f)"]
  ["empty vector truthy" ":t"  "(if [] :t :f)"]
  ["empty map truthy"   ":t"   "(if {} :t :f)"]
  ["empty set truthy"   ":t"   "(if #{} :t :f)"]
  ["number truthy"      ":t"   "(if 42 :t :f)"]
  ["string truthy"      ":t"   "(if \"x\" :t :f)"]
  ["keyword truthy"     ":t"   "(if :kw :t :f)"]
  ["symbol truthy"      ":t"   "(if (quote abc) :t :f)"]
  ["coll truthy"        ":t"   "(if [1 2] :t :f)"]
  ["map truthy"         ":t"   "(if {:a 1} :t :f)"]
  ["if no else -> nil"  "nil"  "(if false :t)"])

(defspec "truthiness / not"
  ["not nil"            "true"  "(not nil)"]
  ["not false"          "true"  "(not false)"]
  ["not zero"           "false" "(not 0)"]
  ["not empty vector"   "false" "(not [])"]
  ["not empty string"   "false" "(not \"\")"]
  ["not number"         "false" "(not 42)"]
  ["not true"           "false" "(not true)"])

(defspec "truthiness / and"
  ["empty is true"      "true"  "(and)"]
  ["single value"       "5"     "(and 5)"]
  ["all truthy -> last" "3"     "(and 1 2 3)"]
  ["stops at false"     "false" "(and 1 false 3)"]
  ["stops at nil"       "nil"   "(and 1 nil 3)"]
  ["false alone"        "false" "(and false)"]
  ["nil alone"          "nil"   "(and nil)"]
  ["zero is truthy"     "0"     "(and 1 0)"])

(defspec "truthiness / or"
  ["empty is nil"       "nil"   "(or)"]
  ["first truthy"       "1"     "(or 1 2)"]
  ["skips nil/false"    "5"     "(or nil false 5)"]
  ["all falsy -> last"  "false" "(or nil false)"]
  ["nil chain -> false" "false" "(or nil nil nil false)"]
  ["zero is truthy"     "0"     "(or 0 1)"]
  ["false alone"        "false" "(or false)"])

(defspec "truthiness / if-not & boolean"
  ["if-not false"       ":yes"  "(if-not false :yes :no)"]
  ["if-not truthy"      ":no"   "(if-not 0 :yes :no)"]
  ["when-not nil"       "1"     "(when-not nil 1)"]
  ["when-not truthy"    "nil"   "(when-not 5 1)"]
  ["boolean of nil"     "false" "(boolean nil)"]
  ["boolean of false"   "false" "(boolean false)"]
  ["boolean of 0"       "true"  "(boolean 0)"]
  ["boolean of value"   "true"  "(boolean :x)"]
  ["true?/false?"       "true"  "(and (true? true) (false? false) (not (true? 1)))"])
