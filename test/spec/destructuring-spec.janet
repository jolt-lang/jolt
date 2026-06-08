# Specification: destructuring (in let, fn, loop, doseq, for).
(use ../support/harness)

(defspec "destructure / sequential"
  ["basic vector"       "3"      "(let [[a b] [1 2]] (+ a b))"]
  ["skip with _"        "3"      "(let [[_ b] [1 2]] (+ b 1))"]
  ["rest with &"        "[3 4]"  "(let [[a & more] [1 3 4]] more)"]
  [":as whole"          "[1 2]"  "(let [[a :as v] [1 2]] v)"]
  ["nested"             "3"      "(let [[[a b]] [[1 2]]] (+ a b))"]
  ["fewer values nil"   "nil"    "(let [[a b c] [1 2]] c)"]
  ["over a list"        "1"      "(let [[a] (list 1 2)] a)"]
  ["over a seq"         "2"      "(let [[a b] (rest [9 1 2])] b)"]
  ["string chars"       "\\a"    "(let [[a] (seq \"ab\")] a)"])

(defspec "destructure / associative"
  ["keys"               "3"      "(let [{:keys [a b]} {:a 1 :b 2}] (+ a b))"]
  [":as map"            "{:a 1}" "(let [{:as m} {:a 1}] m)"]
  [":or default"        "9"      "(let [{:keys [a] :or {a 9}} {}] a)"]
  [":or present"        "1"      "(let [{:keys [a] :or {a 9}} {:a 1}] a)"]
  ["explicit binding"   "1"      "(let [{x :a} {:a 1}] x)"]
  ["nested map"         "2"      "(let [{{b :b} :a} {:a {:b 2}}] b)"]
  ["keys + as"          "[1 {:a 1}]" "(let [{:keys [a] :as m} {:a 1}] [a m])"]
  ["map in vector"      "1"      "(let [[{:keys [a]}] [{:a 1}]] a)"])

(defspec "destructure / in forms"
  ["fn params"          "3"      "((fn [[a b]] (+ a b)) [1 2])"]
  ["fn map param"       "1"      "((fn [{:keys [a]}] a) {:a 1})"]
  ["defn destructure"   "3"      "(do (defn f [[a b]] (+ a b)) (f [1 2]))"]
  ["loop destructure"   "3"      "(loop [[a b] [1 2]] (+ a b))"]
  ["doseq destructure"  "12"     "(let [s (atom 0)] (doseq [[k v] {:a 4 :b 8}] (swap! s (fn [x] (+ x v)))) @s)"]
  ["for destructure"    "[3 7]"  "(for [[a b] [[1 2] [3 4]]] (+ a b))"]
  ["& rest in fn"       "[2 3]"  "((fn [a & more] more) 1 2 3)"])

(defspec "destructure / associative extras"
  [":strs"              "7"      "(let [{:strs [a]} {\"a\" 7}] a)"]
  [":syms"              "8"      "(let [{:syms [a]} {(quote a) 8}] a)"]
  ["namespaced :keys"   "3"      "(let [{:keys [x/y]} {:x/y 3}] y)"]
  ["namespaced :syms"   "4"      "(let [{:syms [p/q]} {(quote p/q) 4}] q)"]
  # :keys also accepts keyword elements ({:keys [:a :b]}), binding bare locals.
  ["keyword :keys"      "3"      "(let [{:keys [:a :b]} {:a 1 :b 2}] (+ a b))"]
  ["keyword :keys ns"   "3"      "(let [{:keys [:x/y]} {:x/y 3}] y)"])

(defspec "destructure / keyword args (& {:keys})"
  ["fn kwargs"          "[1 2]"  "(do (defn f [& {:keys [a b]}] [a b]) (f :a 1 :b 2))"]
  ["fn kwargs + fixed"  "[0 5]"  "(do (defn g [x & {:keys [a]}] [x a]) (g 0 :a 5))"]
  ["fn kwargs :or"      "9"      "(do (defn h [& {:keys [a] :or {a 9}}] a) (h))"]
  ["fn kwargs trailing map" "7"  "(do (defn k [& {:keys [a]}] a) (k {:a 7}))"])

(defspec "destructure / fn params & loop"
  ["fn vector param"    "7"      "((fn [[a b]] (+ a b)) [3 4])"]
  ["fn map param"       "30"     "((fn [{:keys [x y]}] (* x y)) {:x 5 :y 6})"]
  ["fn :or param"       "7"      "((fn [{:keys [x] :or {x 7}}] x) {})"]
  ["fn multi-arity destr" "15"   "((fn ([[a]] a) ([[a] b] (+ a b))) [10] 5)"]
  ["loop vector binding" "[4 2]" "(loop [[a b] [1 2] n 0] (if (< n 3) (recur [(inc a) b] (inc n)) [a b]))"]
  ["loop map binding"   "4"      "(loop [{:keys [v]} {:v 1} n 0] (if (< n 2) (recur {:v (* v 2)} (inc n)) v))"]
  ["loop init sees destr" "[1 2 3]" "(loop [[a b] [1 2] c (+ a b)] [a b c])"])

(defspec "destructure / macro params"
  ["macro & [a & more :as all]"
   "[1 [2 3] [1 2 3]]"
   "(do (defmacro m [& [a & more :as all]] (list (quote quote) [a (vec more) (vec all)])) (m 1 2 3))"]
  ["macro fixed destructure" "[2 1]"
   "(do (defmacro mm [[a b]] (list (quote quote) [b a])) (mm [1 2]))"]
  ["macro & {:keys}" "5"
   "(do (defmacro mk [& {:keys [x]}] (list (quote quote) x)) (mk :x 5))"])
