# Specification: control flow & binding forms.
(use ../support/harness)

(defspec "control / conditionals"
  ["if true"            "1"      "(if true 1 2)"]
  ["if false"           "2"      "(if false 1 2)"]
  ["if nil is false"    "2"      "(if nil 1 2)"]
  ["if no else"         "nil"    "(if false 1)"]
  ["when true"          "3"      "(when true 1 2 3)"]
  ["when false"         "nil"    "(when false 1)"]
  ["when-not"           "1"      "(when-not false 1)"]
  ["cond"               ":b"     "(cond false :a true :b :else :c)"]
  ["cond :else"         ":c"     "(cond false :a false :b :else :c)"]
  ["cond no match"      "nil"    "(cond false :a)"]
  ["condp"              "\"two\"" "(condp = 2 1 \"one\" 2 \"two\" \"other\")"]
  ["case"               ":b"     "(case 2 1 :a 2 :b :default)"]
  ["case default"       ":d"     "(case 9 1 :a 2 :b :d)"]
  ["case multi"         ":ab"    "(case 2 (1 2) :ab 3 :c)"]
  ["case symbol const"  ":s"     "(case 'foo foo :s :default)"]
  ["case vector const"  ":v"     "(case [1 2] [1 2] :v :default)"]
  ["case map const"     ":m"     "(case {:a 1} {:a 1} :m :default)"]
  ["case list const"    ":l"     "(case '(a b) (quote (a b)) :l :default)"]
  ["case keyword"       ":k"     "(case :x :x :k :default)"])

(defspec "control / logic"
  ["and all true"       "3"      "(and 1 2 3)"]
  ["and short circuits" "nil"    "(and 1 nil 3)"]
  ["and empty"          "true"   "(and)"]
  ["or first truthy"    "1"      "(or nil 1 2)"]
  ["or all false"       "false"  "(or nil false)"]
  ["or empty"           "nil"    "(or)"]
  ["not"                "false"  "(not true)"])

(defspec "control / let & loop"
  ["let"                "3"      "(let [a 1 b 2] (+ a b))"]
  ["let sequential"     "3"      "(let [a 1 b (+ a 2)] b)"]
  ["let shadowing"      "2"      "(let [a 1] (let [a 2] a))"]
  ["letfn mutual"       "true"   "(letfn [(ev? [n] (if (zero? n) true (od? (dec n)))) (od? [n] (if (zero? n) false (ev? (dec n))))] (ev? 10))"]
  ["loop/recur"         "15"     "(loop [i 1 acc 0] (if (> i 5) acc (recur (inc i) (+ acc i))))"]
  ["when-let"           "2"      "(when-let [x 1] (inc x))"]
  ["when-let nil"       "nil"    "(when-let [x nil] (inc x))"]
  ["if-let"             "2"      "(if-let [x 1] (inc x) :none)"]
  ["if-let else"        ":none"  "(if-let [x nil] (inc x) :none)"]
  ["if-some zero"       "1"      "(if-some [x 0] (inc x) :none)"]
  ["when-some nil"      "nil"    "(when-some [x nil] x)"])

# Regression: if-let/when-let/if-some/when-some bind the name ONLY in the
# then/body branch. The else branch (and a falsy when-let body, which there is
# none of) must see the surrounding scope, not the binding — so the else of
# (let [x 5] (if-let [x nil] ...)) sees x=5, like Clojure. (Previously the macros
# wrapped the whole `if` in the binding's let*, leaking it into the else.)
(defspec "control / conditional-binding scope"
  ["if-let else sees outer"    "5"   "(let [x 5] (if-let [x nil] :then x))"]
  ["if-let then binds"         "7"   "(let [x 5] (if-let [x 7] x :else))"]
  ["if-some else sees outer"   "5"   "(let [x 5] (if-some [x nil] :then x))"]
  ["if-some binds false"       "false" "(if-some [x false] x :else)"]
  ["when-let else via or"      "5"   "(let [x 5] (or (when-let [x nil] x) x))"]
  ["when-let multi-form body"  "14"  "(when-let [x 7] (inc x) (* x 2))"]
  ["if-let in fn param"        "9"   "((fn [xs] (if-let [xs nil] :then xs)) 9)"]
  ["when-some binds zero"      "1"   "(when-some [x 0] (inc x))"]
  ["if-let evals test once"    "1"   "(let [c (atom 0)] (if-let [v (do (swap! c inc) :v)] @c :none))"])

(defspec "control / iteration"
  ["dotimes side-effect" "5"     "(let [a (atom 0)] (dotimes [i 5] (swap! a inc)) @a)"]
  ["while"              "5"      "(let [a (atom 0)] (while (< @a 5) (swap! a inc)) @a)"]
  ["for"                "[0 1 2]" "(for [x (range 3)] x)"]
  ["for nested"         "[[0 :a] [0 :b] [1 :a] [1 :b]]" "(for [x (range 2) y [:a :b]] [x y])"]
  ["for :when"          "[0 2 4]" "(for [x (range 6) :when (even? x)] x)"]
  ["for :while"         "[0 1 2]" "(for [x (range 10) :while (< x 3)] x)"]
  ["for :let"           "[0 1 4]" "(for [x (range 3) :let [sq (* x x)]] sq)"]
  ["for :let+:when"     "[4 6 8]" "(for [x (range 5) :let [y (* x 2)] :when (> y 3)] y)"]
  ["for multi :when"    "[[1 :a] [1 :b]]" "(for [x [0 1] :when (odd? x) y [:a :b]] [x y])"]
  ["for destructure"    "[3 7]"   "(for [[a b] [[1 2] [3 4]]] (+ a b))"]
  ["doseq side-effect"  "6"      "(let [a (atom 0)] (doseq [x [1 2 3]] (swap! a (fn [v] (+ v x)))) @a)"]
  ["doseq nested"       "4"      "(let [c (atom 0)] (doseq [x [1 2] y [10 20]] (swap! c inc)) @c)"]
  ["doseq :when"        "[1 3]"  "(let [a (atom [])] (doseq [x [1 2 3] :when (odd? x)] (swap! a conj x)) @a)"]
  ["doseq :while"       "6"      "(let [a (atom 0)] (doseq [x (range 10) :while (< x 4)] (swap! a + x)) @a)"]
  ["doseq :let"         "[0 1 4]" "(let [a (atom [])] (doseq [x (range 3) :let [sq (* x x)]] (swap! a conj sq)) @a)"]
  ["doseq returns nil"  "nil"    "(doseq [x [1 2 3]] x)"])

(defspec "control / threading"
  ["->"                 "6"      "(-> 1 inc (+ 4))"]
  ["-> with forms"      "[1 2 3]" "(-> [] (conj 1) (conj 2) (conj 3))"]
  ["->>"                "9"      "(->> [1 2 3] (map inc) (reduce +))"]
  ["as->"               "2"      "(as-> [0 1] x (map inc x) (reverse x) (first x))"]
  ["some->"             "2"      "(some-> 1 inc)"]
  ["some-> nil stops"   "nil"    "(some-> nil inc)"]
  ["some->>"            "[2 3]"  "(some->> [1 2] (map inc))"]
  ["cond->"             "2"      "(cond-> 1 true inc false inc)"]
  ["cond->>"            "[1 2]"  "(cond->> [2] true (cons 1))"]
  ["doto returns subject" "5"    "(let [a (doto (atom 0) (reset! 5))] @a)"])
