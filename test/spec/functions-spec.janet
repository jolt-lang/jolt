# Specification: functions & higher-order combinators.
(use ../support/harness)

(defspec "functions / definition"
  ["fn literal"         "3"      "((fn [a b] (+ a b)) 1 2)"]
  ["fn shorthand"       "3"      "(#(+ %1 %2) 1 2)"]
  ["fn shorthand %"     "2"      "(#(inc %) 1)"]
  ["defn"               "5"      "(do (defn f [x] (+ x 2)) (f 3))"]
  ["multi-arity"        "[1 5]"  "(do (defn f ([x] x) ([x y] (+ x y))) [(f 1) (f 2 3)])"]
  ["variadic"           "[1 2 3]" "(do (defn f [& xs] xs) (f 1 2 3))"]
  ["variadic with fixed" "[1 [2 3]]" "(do (defn f [a & xs] [a xs]) (f 1 2 3))"]
  ["closure captures"   "8"      "(do (defn adder [n] (fn [x] (+ x n))) ((adder 5) 3))"]
  ["recursion"          "120"    "(do (defn fact [n] (if (< n 2) 1 (* n (fact (dec n))))) (fact 5))"]
  ["named fn self-ref"  "120"    "((fn fact [n] (if (< n 2) 1 (* n (fact (dec n))))) 5)"]
  # A param whose name collides with a Janet builtin the back end emits in
  # generated code (here `in`, used to deref vars / index shape-recs) must not
  # shadow it — malli's explainer binds `[value in acc]` (jolt-fjb1).
  ["param named `in`"   "1"       "((fn [in] (first in)) [1 2 3])"]
  ["param `in` via core fn" "3"   "((fn [in] (count in)) [1 2 3])"]
  ["local `in` in let body" "2"   "(let [in [2 3]] (first in))"])

(defspec "functions / application"
  ["apply"              "6"      "(apply + [1 2 3])"]
  ["apply with leading" "10"     "(apply + 1 2 [3 4])"]
  ["apply keyword"      "1"      "(apply :a [{:a 1}])"]
  ["partial"            "7"      "((partial + 5) 2)"]
  ["partial multi"      "10"     "((partial + 1 2) 3 4)"]
  ["comp"               "4"      "((comp inc inc) 2)"]
  ["comp order"         "5"      "((comp inc (fn [x] (* x 2))) 2)"]
  ["comp identity"      "3"      "((comp) 3)"]
  ["complement"         "true"   "((complement even?) 3)"]
  ["constantly"         "5"      "((constantly 5) 1 2 3)"]
  ["identity"           "7"      "(identity 7)"])

(defspec "functions / combinators"
  ["juxt"               "[1 3]"  "((juxt first last) [1 2 3])"]
  ["fnil"               "1"      "((fnil inc 0) nil)"]
  ["fnil passes value"  "6"      "((fnil inc 0) 5)"]
  ["every-pred true"    "true"   "((every-pred pos? even?) 4)"]
  ["every-pred false"   "false"  "((every-pred pos? even?) 3)"]
  ["some-fn"            "true"   "((some-fn even? neg?) 3 4)"]
  ["memoize"            "2"      "(do (def c (atom 0)) (def f (memoize (fn [x] (swap! c inc) x))) (f 1) (f 1) (f 2) @c)"]
  ["trampoline"         "10"     "(trampoline (fn f [n acc] (if (zero? n) acc (fn [] (f (dec n) (+ acc 2))))) 5 0)"])

# Phase 2 leaf batch (jolt-ded): moved from the Janet seed to 20-coll.clj.
(defspec "clojure.core / leaf batch (complement fnil munge etc.)"
  ["complement true"     "true"     "((complement pos?) -1)"]
  ["complement false"    "false"    "((complement pos?) 1)"]
  ["complement multi"    "true"     "((complement <) 3 2)"]
  ["fnil patches nil"    "1"        "((fnil inc 0) nil)"]
  ["fnil passes non-nil" "6"        "((fnil inc 0) 5)"]
  ["fnil two defaults"   "8"        "((fnil + 1 2) nil nil 5)"]
  ["fnil only first 3"   "[:a :b :c nil]" "((fnil vector :a :b :c) nil nil nil nil)"]
  ["fnil in update"      "{:k 1}"   "(update {} :k (fnil inc 0))"]
  ["clojure-version"     "true"     "(string? (clojure-version))"]
  ["bigdec"              "3"        "(bigdec 3)"]
  ["numerator throws"    :throws    "(numerator 1)"]
  ["denominator throws"  :throws    "(denominator 1)"]
  ["supers empty set"    "#{}"      "(supers 1)"]
  ["munge dashes"        "\"a_b\""  "(munge \"a-b\")"]
  ["munge symbol"        "\"x_y\""  "(munge (quote x-y))"]
  ["test no-test"        ":no-test" "(test (quote foo))"])

# Phase 2 leaf batch 2 (jolt-ded): canonical ports of key/val/select-keys/
# zipmap/merge/merge-with/get-in/memoize/partial/trampoline/some?/true?/false?/
# max/min/reverse, plus find (previously missing entirely).
(defspec "clojure.core / leaf batch 2"
  ["key"                "1"        "(key (first {1 :a}))"]
  ["val"                ":a"       "(val (first {1 :a}))"]
  ["key non-entry throws" :throws  "(key 5)"]
  ["find hit"           "[:a 1]"   "(find {:a 1} :a)"]
  ["find miss"          "nil"      "(find {:a 1} :b)"]
  ["find nil value"     "[:a nil]" "(find {:a nil} :a)"]
  ["find on vector"     "[0 :x]"   "(find [:x :y] 0)"]
  ["select-keys"        "{:a 1}"   "(select-keys {:a 1 :b 2} [:a])"]
  ["select-keys nil val" "{:a nil}" "(select-keys {:a nil :b 2} [:a])"]
  ["select-keys missing" "{}"      "(select-keys {:a 1} [:z])"]
  ["zipmap"             "{:a 1 :b 2}" "(zipmap [:a :b] [1 2])"]
  ["zipmap uneven"      "{:a 1}"   "(zipmap [:a :b] [1])"]
  ["zipmap nil val"     "{:a nil}" "(zipmap [:a] [nil])"]
  ["merge"              "{:a 1 :b 2}" "(merge {:a 1} {:b 2})"]
  ["merge later wins"   "{:a 2}"   "(merge {:a 1} {:a 2})"]
  ["merge nil arg"      "{:a 1}"   "(merge {:a 1} nil)"]
  ["merge nil first"    "{:a 1}"   "(merge nil {:a 1})"]
  ["merge all nil"      "nil"      "(merge nil nil)"]
  ["merge empty"        "nil"      "(merge)"]
  ["merge entry pair"   "{:a 1 :b 2}" "(merge {:a 1} [:b 2])"]
  ["merge-with"         "{:a 3}"   "(merge-with + {:a 1} {:a 2})"]
  ["merge-with disjoint" "{:a 1 :b 2}" "(merge-with + {:a 1} {:b 2})"]
  ["merge-with nil-val present" "{:a 1}" "(merge-with (fn [a b] (or a b)) {:a nil} {:a 1})"]
  ["get-in"             "1"        "(get-in {:a {:b 1}} [:a :b])"]
  ["get-in missing"     ":nf"      "(get-in {:a 1} [:z :y] :nf)"]
  ["get-in nil value present" "nil" "(get-in {:a {:b nil}} [:a :b] :nf)"]
  ["get-in empty path"  "{:a 1}"   "(get-in {:a 1} [])"]
  ["memoize"            "2"        "(do (def c (atom 0)) (def f (memoize (fn [x] (swap! c inc) x))) (f 1) (f 1) (f 2) (deref c))"]
  ["memoize caches nil" "1"        "(do (def c (atom 0)) (def f (memoize (fn [x] (swap! c inc) nil))) (f 1) (f 1) (deref c))"]
  ["partial"            "6"        "((partial + 1 2) 3)"]
  ["partial no extra"   "3"        "((partial + 1 2))"]
  ["partial many fixed" "15"       "((partial + 1 2 3 4) 5)"]
  ["trampoline"         "10"       "(trampoline (fn f [n acc] (if (zero? n) acc (fn [] (f (dec n) (+ acc 2))))) 5 0)"]
  ["some? true"         "true"     "(some? 0)"]
  ["some? false"        "false"    "(some? nil)"]
  ["true?/false?"       "[true false false]" "[(true? true) (true? 1) (false? nil)]"]
  ["max"                "3"        "(max 1 3 2)"]
  ["min"                "1"        "(min 3 1 2)"]
  ["max single"         "5"        "(max 5)"]
  ["max non-number throws" :throws "(max 1 :a)"]
  ["reverse"            "(quote (3 2 1))" "(reverse [1 2 3])"]
  ["reverse empty"      "()"       "(reverse nil)"]
  ["conj nil onto map"  "{:a 1}"   "(conj {:a 1} nil)"])

# Phase 2 leaf batch 3 (jolt-ded): empty/assoc-in/update-in (20-coll) and
# interpose/take-nth (40-lazy, with canonical transducer arities). keys/vals/
# empty? are expander-coupled (00-syntax macros call them) and stay in the
# seed until the fast macro-expansion path lands.
(defspec "clojure.core / leaf batch 3"
  ["empty vector"        "[]"        "(empty [1 2])"]
  ["empty list"          "()"        "(empty (list 1))"]
  ["empty map"           "{}"        "(empty {:a 1})"]
  ["empty set"           "#{}"       "(empty #{1})"]
  ["empty nil"           "nil"       "(empty nil)"]
  ["empty string"        "nil"       "(empty \"abc\")"]
  ["empty lazy is ()"    "()"        "(empty (map inc [1 2]))"]
  ["empty sorted keeps cmp" "[3 1]"  "(vec (seq (into (empty (sorted-set-by > 1 2)) [1 3])))"]
  ["assoc-in"            "{:a {:b 1}}" "(assoc-in {} [:a :b] 1)"]
  ["assoc-in deep"       "{:a {:b {:c 2}}}" "(assoc-in {:a {:b {:c 1}}} [:a :b :c] 2)"]
  ["assoc-in keeps siblings" "{:a {:b 1 :c 2}}" "(assoc-in {:a {:b 1}} [:a :c] 2)"]
  ["assoc-in vector idx" "[1 9]"     "(assoc-in [1 2] [1] 9)"]
  ["assoc-in nested vec" "[{:a 9}]"  "(assoc-in [{:a 1}] [0 :a] 9)"]
  ["update-in"           "{:a {:b 2}}" "(update-in {:a {:b 1}} [:a :b] inc)"]
  ["update-in extra args" "{:a {:b 111}}" "(update-in {:a {:b 1}} [:a :b] + 10 100)"]
  ["update-in fnil"      "{:a {:b 1}}" "(update-in {} [:a :b] (fnil inc 0))"]
  ["update-in single key" "{:a 2}"   "(update-in {:a 1} [:a] inc)"]
  ["interpose"           "(quote (1 :s 2 :s 3))" "(interpose :s [1 2 3])"]
  ["interpose empty"     "()"        "(interpose :s [])"]
  ["interpose one"       "(quote (1))" "(interpose :s [1])"]
  ["interpose is lazy"   "(quote (0 :s 1))" "(take 3 (interpose :s (range)))"]
  ["interpose xform"     "[\"a\" \",\" \"b\"]" "(vec (sequence (interpose \",\") [\"a\" \"b\"]))"]
  ["take-nth"            "(quote (1 3 5))" "(take-nth 2 [1 2 3 4 5 6])"]
  ["take-nth lazy"       "(quote (0 3 6))" "(take 3 (take-nth 3 (range)))"]
  ["take-nth xform"      "[1 3 5]"   "(vec (sequence (take-nth 2) [1 2 3 4 5 6]))"]
  ["take-nth into"       "[1 4]"     "(into [] (take-nth 3) [1 2 3 4 5])"])

# Phase 2 leaf batch 4 (jolt-ded): sort-by (canonical: compare-defaulted, over
# the host sort seam), rand-int (canonical truncation via int), pure
# Fisher-Yates shuffle, random-uuid over parse-uuid, char tables as
# char-keyed Clojure maps. rand and sort stay: they ARE the host seams.
(defspec "clojure.core / leaf batch 4"
  ["sort-by keyfn"        "[[1 :b] [2 :a]]" "(sort-by first [[2 :a] [1 :b]])"]
  ["sort-by string keys"  "(quote (\"a\" \"bb\" \"ccc\"))" "(sort-by count [\"ccc\" \"a\" \"bb\"])"]
  ["sort-by comparator"   "[3 2 1]"   "(sort-by identity > [1 3 2])"]
  ["sort-by 3way cmp"     "[3 2 1]"   "(sort-by identity (fn [a b] (- b a)) [1 3 2])"]
  ["sort-by mixed nil"    "[nil 1 2]" "(sort-by identity [2 nil 1])"]
  ["sort-by empty"        "()"        "(sort-by first [])"]
  ["sort-by nil coll"     "()"        "(sort-by first nil)"]
  ["rand-int range"       "true"      "(every? (fn [_] (let [r (rand-int 5)] (and (int? r) (<= 0 r 4)))) (range 50))"]
  ["rand-int zero"        "0"         "(rand-int 1)"]
  ["shuffle is permutation" "true"    "(= (sort (shuffle [5 3 1 4 2])) [1 2 3 4 5])"]
  ["shuffle returns vector" "true"    "(vector? (shuffle [1 2 3]))"]
  ["shuffle empty"        "[]"        "(shuffle [])"]
  ["shuffle non-coll throws" :throws  "(shuffle 5)"]
  ["random-uuid is uuid"  "true"      "(uuid? (random-uuid))"]
  ["random-uuid v4 shape" "true"      "(boolean (re-matches #\"[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}\" (str (random-uuid))))"]
  ["random-uuid distinct" "true"      "(not= (random-uuid) (random-uuid))"]
  ["char-escape newline"  "\"\\\\n\"" "(char-escape-string \\newline)"]
  ["char-escape quote"    "true"      "(= 2 (count (char-escape-string \\\")))"]
  ["char-escape none"     "nil"       "(char-escape-string \\a)"]
  ["char-name space"      "\"space\"" "(char-name-string \\space)"]
  ["char-name newline"    "\"newline\"" "(char-name-string \\newline)"]
  ["char-name none"       "nil"       "(char-name-string \\a)"])

# recur into a VARIADIC fn arity binds the LAST recur arg directly as the
# rest seq (Clojure: recur to a variadic head takes n-fixed + 1 args, no
# re-collection). The interpreter used to re-enter through the varargs
# collector, wrapping the seq in a fresh 1-element rest list — xs never
# emptied and the loop hung (jolt-4df).
(defspec "functions / recur into variadic arity"
  ["counts rest via recur"  "3"
   "((fn cnt [acc & xs] (if (seq xs) (recur (inc acc) (rest xs)) acc)) 0 :a :b :c)"]
  ["zero-fixed variadic"    "4"
   "((fn f [& xs] (if (< (count xs) 4) (recur (cons :x xs)) (count xs))) :a)"]
  ["rest empties to nil"    "(quote (:done))"
   "((fn f [& xs] (if xs (recur (next xs)) (list :done))) 1 2)"]
  ["multi-arity variadic recur" "6"
   "((fn ma ([a] a) ([a & xs] (if (seq xs) (recur (+ a (first xs)) (rest xs)) a))) 1 2 3)"]
  ["recur passes nil rest"  ":empty"
   "((fn f [acc & xs] (if (seq xs) (recur acc (rest xs)) :empty)) 0 1)"]
  ["fixed-arity recur untouched" "10"
   "((fn f [n acc] (if (pos? n) (recur (dec n) (+ acc 2)) acc)) 5 0)"])

# An EMPTY rest arg binds nil, never () — Clojure semantics. (f) with [& r]
# giving a truthy () sent yogthos/config's (or (.exists f) required) down the
# wrong branch and broke its load.
(defspec "functions / empty rest arg is nil"
  ["no args"            ":nil"  "((fn [& r] (if r :truthy :nil)))"]
  ["after fixed"        ":nil"  "((fn [a & r] (if r :truthy :nil)) 1)"]
  ["via apply"          ":nil"  "(apply (fn [& r] (if r :truthy :nil)) [])"]
  ["defn no args"       ":nil"  "(do (defn er-f [& r] (if r :truthy :nil)) (er-f))"]
  ["non-empty unchanged" "'(1 2)" "((fn [& r] r) 1 2)"]
  ["one extra"          "'(2)"   "((fn [a & r] r) 1 2)"]
  ["rest destructure with no args" ":nil"
   "((fn [& [a]] (if a :truthy :nil)))"])

# Arity enforcement (jolt-6xn): fixed arities throw on any mismatch, variadic
# arities on fewer than the fixed params — Clojure's ArityException, in both
# the interpreter and the compiled path.
(defspec "functions / arity enforcement"
  ["fixed extra args"      :throws "((fn [x] x) 1 2)"]
  ["fixed missing args"    :throws "((fn [x y] x) 1)"]
  ["fixed zero of one"     :throws "((fn [x] x))"]
  ["named defn extra"      :throws "(do (defn af1 [x] x) (af1 1 2))"]
  ["overlay fn extra"      :throws "(identity 1 2)"]
  ["through apply"         :throws "(apply (fn [x] x) [1 2])"]
  ["through update"        :throws "(update {:k 1} :k identity 1 2 3 4)"]
  ["variadic below min"    :throws "((fn [x & r] x))"]
  ["variadic at min"       "nil"   "((fn [x & r] r) 1)"]
  ["variadic above min"    "(quote (2 3))" "((fn [x & r] r) 1 2 3)"]
  ["multi-arity no match"  :throws "((fn ([x] x) ([x y] y)) 1 2 3)"]
  ["multi-arity variadic below min" :throws "((fn ([x] x) ([x y & r] r)))"]
  ["destructured param counts as one" "3" "((fn [[a b] c] c) [1 2] 3)"]
  ["destructured extra throws" :throws "((fn [[a b]] a) [1 2] [3 4])"]
  ["hof exact arity ok"    "[2 4]" "(mapv (fn [x] (* 2 x)) [1 2])"]
  ["zero-arity fn ok"      "7"     "((fn [] 7))"])
