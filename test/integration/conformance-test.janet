# Clojure conformance harness (phase 1: extracted assertion pairs).
#
# Each case is [name expected-clj actual-clj]. The harness evaluates the
# single Clojure program  (= <expected> <actual>)  inside a fresh jolt ctx
# and asserts it returns boolean true. Comparison therefore uses jolt's OWN
# `=`, which implements Clojure sequential/collection equality -- so results
# reflect real Clojure semantics rather than Janet-level identity.
#
# `actual` may be a multi-form body; wrap such cases in (do ...).
#
# Source of truth: ~/src/clojure/test/clojure/test_clojure/*.clj
# These pairs are hand-extracted from those files (and canonical idioms)
# until a minimal clojure.test lets us load the real files directly.

(use ../../src/jolt/api)
(import ../../src/jolt/backend :as selfhost)
(use ../../src/jolt/reader)

(def cases
  [
   ### ---- CRITICAL: lazy sequences ----
   ["self-ref lazy-cat fib"
    "(quote (0 1 1 2 3 5 8 13 21 34))"
    "(do (def fib-seq (lazy-cat [0 1] (map + (rest fib-seq) fib-seq))) (take 10 fib-seq))"]
   ["self-ref lazy-seq ones"
    "(quote (1 1 1 1 1))"
    "(do (def ones (lazy-seq (cons 1 ones))) (take 5 ones))"]
   ["self-ref lazy-seq nats"
    "(quote (0 1 2 3 4))"
    "(do (def nats (lazy-cat [0] (map inc nats))) (take 5 nats))"]

   ### ---- CRITICAL: multi-collection map ----
   ["map two colls"        "(quote (11 22 33))"      "(map + [1 2 3] [10 20 30])"]
   ["map three colls"      "(quote (12 24 36))"      "(map + [1 2 3] [10 20 30] [1 2 3])"]
   ["map uneven (shortest)" "(quote ([1 :a] [2 :b]))" "(map vector [1 2 3] [:a :b])"]
   ["map over range+vec"   "(quote (1 3 5))"         "(map + (range 3) [1 2 3])"]
   ["map fn list arg"      "(quote (2 3 4))"         "(map inc (list 1 2 3))"]

   ### ---- CRITICAL: iterate / infinite seqs ----
   ["iterate"        "(quote (0 1 2 3 4))"  "(take 5 (iterate inc 0))"]
   ["iterate double" "(quote (1 2 4 8 16))" "(take 5 (iterate (fn [x] (* 2 x)) 1))"]
   ["range over inf map" "(quote (1 2 3))"  "(take 3 (map inc (range)))"]
   ["count of take"  "100"                  "(count (take 100 (range)))"]
   ["last of take"   "5"                    "(last (take 5 (iterate inc 1)))"]

   ### ---- CRITICAL: collections as IFn ----
   ["vector as fn"  ":b"  "([:a :b :c] 1)"]
   ["map as fn"     "1"   "({:a 1} :a)"]
   ["map as fn miss" "nil" "({:a 1} :z)"]
   ["map as fn default" "99" "({:a 1} :z 99)"]
   ["set as fn"     "2"   "(#{1 2 3} 2)"]
   ["set as fn miss" "nil" "(#{1 2 3} 9)"]
   ["keyword as fn" "1"   "(:a {:a 1})"]
   ["map fn over coll" "(quote (1 3))" "(map {:a 1 :b 3} [:a :b])"]

   ### ---- CRITICAL: vec / into over lazy + maps ----
   ["vec of map-result"  "[2 3 4]"          "(vec (map inc [1 2 3]))"]
   ["vec of range"       "[0 1 2 3 4]"      "(vec (range 5))"]
   ["into vec"           "[1 2 3 4 5 6]"    "(into [1 2 3] [4 5 6])"]
   ["into vec from lazy" "[2 3 4]"          "(into [] (map inc [1 2 3]))"]
   ["into map pairs"     "{:a 1 :b 2}"      "(into {} [[:a 1] [:b 2]])"]
   ["into map onto map"  "{:a 1 :b 2 :c 3}" "(into {:a 1} [[:b 2] [:c 3]])"]
   ["into list"          "(quote (3 2 1))"  "(into (list) [1 2 3])"]

   ### ---- Option A: lazy transformers return seqs, not vectors ----
   # map/filter/take/take-while over a concrete vector yield a lazy seq, matching
   # Clojure: (seq? (map ...)) is true, (vector? (map ...)) is false.
   ["map vec is seq"      "true"   "(seq? (map inc [1 2 3]))"]
   ["map vec not vector"  "false"  "(vector? (map inc [1 2 3]))"]
   ["filter vec is seq"   "true"   "(seq? (filter odd? [1 2 3]))"]
   ["take vec is seq"     "true"   "(seq? (take 2 [1 2 3]))"]
   ["map over set"        "true"   "(= #{2 3 4} (set (map inc #{1 2 3})))"]
   ["filter over map ev"  "(quote ([:b 2]))" "(filter (fn [[k v]] (> v 1)) {:a 1 :b 2})"]
   # cons of cons over a lazy tail must not leak the rest-thunk
   ["cons cons lazy"      "(quote (1 2 3))" "(cons 1 (cons 2 (lazy-seq (cons 3 nil))))"]
   ["juxt fns in vec"     "[1 3]"  "((juxt first last) [1 2 3])"]
   ["last of lazy take"   "5"      "(last (take 5 (iterate inc 1)))"]
   ["next empty lazy"     "nil"    "(next (take 1 [1]))"]
   # drop/distinct/partition/map-indexed/take-nth/interpose/keep are lazy too
   ["drop vec is seq"     "true"   "(seq? (drop 1 [1 2 3]))"]
   ["distinct vec is seq" "true"   "(seq? (distinct [1 1 2]))"]
   ["map-indexed is seq"  "true"   "(seq? (map-indexed vector [1 2]))"]
   ["partition vec lazy"  "(quote ((1 2) (3 4)))" "(partition 2 [1 2 3 4 5])"]
   # nth over a lazy seq must not treat a false/nil element as end-of-seq
   ["nth lazy false elem" "false"  "(nth (map identity [false 1 2]) 0)"]
   ["nth lazy past false" "2"      "(nth (drop 1 (list false 1 2)) 1)"]
   ["cond-> false clause" "2"      "(cond-> 1 true inc false inc)"]

   ### ---- HIGH: destructuring ----
   ["destr nested seq"   "[1 2 3]"   "(let [[a [b c]] [1 [2 3]]] [a b c])"]
   ["destr rest+as"      "[1 (quote (2 3)) [1 2 3]]" "(let [[a & r :as all] [1 2 3]] [a r all])"]
   ["destr map :keys"    "[1 2]"     "(let [{:keys [a b]} {:a 1 :b 2}] [a b])"]
   ["destr map :or"      "[1 99]"    "(let [{:keys [a b] :or {b 99}} {:a 1}] [a b])"]
   ["destr map :strs"    "[1 2]"     "(let [{:strs [a b]} {\"a\" 1 \"b\" 2}] [a b])"]
   ["destr map :as"      "[1 {:a 1}]" "(let [{:keys [a] :as m} {:a 1}] [a m])"]
   ["destr nested map"   "5"         "(let [{{:keys [x]} :pos} {:pos {:x 5}}] x)"]
   ["destr fn-param seq" "7"         "((fn [[a b]] (+ a b)) [3 4])"]
   ["destr fn-param map" "3"         "((fn [{:keys [a b]}] (+ a b)) {:a 1 :b 2})"]
   ["destr let map key"  "1"         "(let [{a :a} {:a 1}] a)"]

   ### ---- HIGH: update / assoc-in on map literals ----
   ["update inc"         "{:a 2}"            "(update {:a 1} :a inc)"]
   ["update extra args"  "{:a 111}"          "(update {:a 1} :a + 10 100)"]
   ["update-in"          "{:a {:b 2}}"        "(update-in {:a {:b 1}} [:a :b] inc)"]
   ["assoc-in"           "{:a {:b 1 :c 2}}"   "(assoc-in {:a {:b 1}} [:a :c] 2)"]
   ["assoc-in create"    "{:a {:b 1}}"        "(assoc-in {} [:a :b] 1)"]
   ["update-in fnil"     "{:a {:b 1}}"        "(update-in {} [:a :b] (fnil inc 0))"]
   ["get-in"             "1"                  "(get-in {:a {:b {:c 1}}} [:a :b :c])"]

   ### ---- HIGH: str semantics ----
   ["str nil empty"      "\"\""       "(str nil)"]
   ["str concat nil"     "\"a1\""     "(str \"a\" 1 nil)"]
   ["str keyword"        "\":b\""     "(str :b)"]
   ["str symbol"         "\"foo\""    "(str (quote foo))"]
   ["str mixed"          "\"a:b1\""   "(str \"a\" :b 1)"]
   ["str seq"            "\"[1 2 3]\"" "(str [1 2 3])"]

   ### ---- HIGH: dispatch ----
   ["multimethod"        "9"   "(do (defmulti area :shape) (defmethod area :sq [s] (* (:s s) (:s s))) (area {:shape :sq :s 3}))"]
   ["multimethod default" ":def" "(do (defmulti f identity) (defmethod f :default [x] :def) (f 99))"]
   ["protocol on record" "16"  "(do (defprotocol Sh (ar [s])) (defrecord Sq [side] Sh (ar [_] (* side side))) (ar (->Sq 4)))"]
   ["reify dispatch"     "42"  "(do (defprotocol P (m [_])) (m (reify P (m [_] 42))))"]

   ### ---- HIGH: aliased namespace calls ----
   ["require :as alias"  "\"1,2,3\"" "(do (require (quote [clojure.string :as s])) (s/join \",\" [1 2 3]))"]

   ### ---- MED: missing core fns ----
   ["peek vec"        "3"             "(peek [1 2 3])"]
   ["peek list"       "1"             "(peek (list 1 2 3))"]
   ["pop vec"         "[1 2]"         "(pop [1 2 3])"]
   ["pop list"        "(quote (2 3))" "(pop (list 1 2 3))"]
   ["subvec"          "[2 3]"         "(subvec [1 2 3 4 5] 1 3)"]
   ["subvec to-end"   "[3 4 5]"       "(subvec [1 2 3 4 5] 2)"]
   ["reduce-kv"       "{:a 2 :b 3}"   "(reduce-kv (fn [m k v] (assoc m k (inc v))) {} {:a 1 :b 2})"]
   ["reduce-kv vector idx" "(quote ([0 :a] [1 :b]))" "(reduce-kv (fn [a i v] (conj a [i v])) [] [:a :b])"]

   ### ---- iterating maps yields entries ----
   ["map over map"      "true"  "(= #{1 2} (set (map val {:a 1 :b 2})))"]
   ["map keys over map" "true"  "(= #{:a :b} (set (map key {:a 1 :b 2})))"]
   ["first of map"      "true"  "(let [e (first {:a 1})] (and (= (key e) :a) (= (val e) 1)))"]
   ["vec of map"        "[[:a 1]]" "(vec {:a 1})"]
   ["reduce over map"   "6"     "(reduce (fn [a [k v]] (+ a v)) 0 {:a 1 :b 2 :c 3})"]
   ["into transform map" "{:a 2 :b 3}" "(into {} (map (fn [[k v]] [k (inc v)]) {:a 1 :b 2}))"]
   ["filter over map"   "true"  "(= [[:b 2]] (filterv (fn [[k v]] (> v 1)) {:a 1 :b 2}))"]
   ["doall realizes"    "(quote (2 3 4))" "(doall (map inc [1 2 3]))"]
   ["tree-seq"          "(quote (1 2 3))" "(map (fn [x] x) (filter (complement coll?) (tree-seq coll? seq [1 [2 [3]]])))"]
   ["key/val"           "true"  "(let [e (first {:k 9})] (and (= :k (key e)) (= 9 (val e))))"]
   ["nat-int?"          "true"  "(and (nat-int? 0) (nat-int? 5) (not (nat-int? -1)))"]
   ["list* prepend"     "(quote (1 2 3 4))" "(list* 1 2 [3 4])"]
   ["cycle"           "(quote (1 2 3 1 2 3 1))" "(take 7 (cycle [1 2 3]))"]
   ["partition-all"   "(quote ((1 2) (3 4) (5)))" "(partition-all 2 [1 2 3 4 5])"]
   ["reductions"      "(quote (1 3 6 10))" "(reductions + [1 2 3 4])"]
   ["reductions init" "(quote (0 1 3 6))" "(reductions + 0 [1 2 3])"]
   ["dedupe"          "(quote (1 2 3 1))" "(dedupe [1 1 2 3 3 1])"]
   ["keep-indexed"    "(quote (:b :d))" "(keep-indexed (fn [i x] (if (odd? i) x)) [:a :b :c :d])"]
   ["map-indexed"     "(quote ([0 :a] [1 :b]))" "(map-indexed (fn [i x] [i x]) [:a :b])"]
   ["trampoline"      ":done"         "(do (defn a [n] (if (zero? n) :done (fn [] (a (dec n))))) (trampoline a 5))"]
   ["format"          "\"1-x\""       "(format \"%d-%s\" 1 \"x\")"]
   ["read-string"     "(quote (+ 1 2))" "(read-string \"(+ 1 2)\")"]
   ["letfn mutual"    "true"          "(letfn [(ev? [n] (if (= n 0) true (od? (dec n)))) (od? [n] (if (= n 0) false (ev? (dec n))))] (ev? 10))"]
   ["doseq side"      "[1 2 3]"       "(do (def a (atom [])) (doseq [x [1 2 3]] (swap! a conj x)) @a)"]
   ["doseq nested"    "4"             "(do (def c (atom 0)) (doseq [x [1 2] y [10 20]] (swap! c inc)) @c)"]

   ### ---- MED: lazy filter / take-while over infinite seqs ----
   ["lazy filter inf"     "(quote (1 3 5 7 9))" "(take 5 (filter odd? (range)))"]
   ["lazy take-while inf" "(quote (0 1 2 3 4))" "(take-while (fn [x] (< x 5)) (range))"]
   ["lazy remove inf"     "(quote (0 2 4 6 8))" "(take 5 (remove odd? (range)))"]
   ["filter finite"       "(quote (2 4))"       "(filter even? [1 2 3 4 5])"]

   ### ==== atoms (full support) ====
   ["swap! args"        "7"     "(do (def a (atom 1)) (swap! a + 2 4) @a)"]
   ["reset! ret"        "9"     "(do (def a (atom 1)) (reset! a 9))"]
   ["compare-and-set!"  "true"  "(do (def a (atom 1)) (compare-and-set! a 1 2))"]
   ["compare-and-set! no" "false" "(do (def a (atom 1)) (compare-and-set! a 5 2))"]
   ["swap-vals!"        "[1 2]" "(do (def a (atom 1)) (swap-vals! a inc))"]
   ["reset-vals!"       "[1 9]" "(do (def a (atom 1)) (reset-vals! a 9))"]
   ["atom map swap"     "{:a 1 :b 2}" "(do (def a (atom {:a 1})) (swap! a assoc :b 2) @a)"]
   ["add-watch"         "[:k 1 2]" "(do (def lg (atom nil)) (def a (atom 1)) (add-watch a :k (fn [k r o n] (reset! lg [k o n]))) (swap! a inc) @lg)"]
   ["atom validator"    "5"     "(do (def a (atom 1 :validator pos?)) (reset! a 5) @a)"]
   ["instance? Atom"    "true"  "(instance? clojure.lang.Atom (atom 1))"]

   ### ==== volatiles / delays ====
   ["volatile"          "2"     "(do (def v (volatile! 1)) (vreset! v 2) @v)"]
   ["vswap!"            "2"     "(do (def v (volatile! 1)) (vswap! v inc) @v)"]
   ["volatile?"         "true"  "(volatile? (volatile! 1))"]
   ["delay force"       "3"     "(force (delay (+ 1 2)))"]
   ["delay deref once"  "1"     "(do (def c (atom 0)) (def d (delay (swap! c inc))) @d @d @c)"]
   ["realized? delay"   "true"  "(do (def d (delay 1)) @d (realized? d))"]
   ["realized? not"     "false" "(realized? (delay 1))"]

   ### ==== numbers / math ====
   ["quot neg"          "-2"    "(quot -7 3)"]
   ["rem neg"           "-1"    "(rem -7 3)"]
   ["mod neg"           "2"     "(mod -7 3)"]
   ["bit ops"           "[4 14 10]" "[(bit-and 12 6) (bit-or 12 6) (bit-xor 12 6)]"]
   ["bit-shift"         "[8 2]" "[(bit-shift-left 1 3) (bit-shift-right 8 2)]"]
   ["Math/sqrt"         "3.0"   "(Math/sqrt 9)"]
   ["Math/pow"          "8.0"   "(Math/pow 2 3)"]
   ["min-key"           "1"     "(min-key abs 1 -2 3)"]
   ["max-key"           "-4"    "(max-key abs 1 -2 -4 3)"]

   ### ==== strings (clojure.string) ====
   ["str/trim"          "\"hi\"" "(do (require (quote [clojure.string :as s])) (s/trim \"  hi  \"))"]
   ["str/split regex"   "[\"a\" \"b\" \"c\"]" "(do (require (quote [clojure.string :as s])) (s/split \"a,b,c\" #\",\"))"]
   ["str/split ws"      "[\"a\" \"b\" \"c\"]" "(do (require (quote [clojure.string :as s])) (s/split \"a  b   c\" #\"\\s+\"))"]
   ["str/replace"       "\"hexxo\"" "(do (require (quote [clojure.string :as s])) (s/replace \"hello\" \"ll\" \"xx\"))"]
   ["str/replace regex" "\"ab\""  "(do (require (quote [clojure.string :as s])) (s/replace \"a1b2\" #\"[0-9]\" \"\"))"]
   ["str/includes?"     "true"  "(do (require (quote [clojure.string :as s])) (s/includes? \"hello\" \"ell\"))"]
   ["str/reverse"       "\"cba\"" "(do (require (quote [clojure.string :as s])) (s/reverse \"abc\"))"]
   ["subs"              "\"ell\"" "(subs \"hello\" 1 4)"]

   ### ==== regex ====
   ["re-find"           "\"123\"" "(re-find #\"[0-9]+\" \"abc123def\")"]
   ["re-matches"        "\"abc\"" "(re-matches #\"a.c\" \"abc\")"]
   ["re-matches no"     "nil"   "(re-matches #\"a.c\" \"abcd\")"]
   ["re-seq"            "(quote (\"12\" \"34\"))" "(re-seq #\"[0-9]+\" \"a12b34\")"]

   ### ==== sequences ====
   ["split-at"          "[[1 2] [3 4 5]]" "(split-at 2 [1 2 3 4 5])"]
   ["split-with"        "[[1 2] [3 4 1]]" "(split-with (fn [x] (< x 3)) [1 2 3 4 1])"]
   ["interpose"         "(quote (1 0 2 0 3))" "(interpose 0 [1 2 3])"]
   ["partition step"    "(quote ((1 2) (3 4)))" "(partition 2 2 [1 2 3 4 5])"]
   ["not-every?"        "true"  "(not-every? pos? [1 -2 3])"]
   ["not-any?"          "true"  "(not-any? neg? [1 2 3])"]
   ["take-nth"          "(quote (0 2 4))" "(take-nth 2 [0 1 2 3 4])"]
   ["butlast"           "(quote (1 2))" "(butlast [1 2 3])"]
   ["filterv"           "[2 4]" "(filterv even? [1 2 3 4])"]
   ["mapv"              "[2 3 4]" "(mapv inc [1 2 3])"]
   ["reduced early"     "3"     "(reduce (fn [a x] (if (> a 2) (reduced a) (+ a x))) 0 [1 2 3 4 5])"]
   ["sort cmp"          "[3 2 1]" "(sort > [1 3 2])"]
   ["frequencies"       "{1 2 2 1}" "(frequencies [1 1 2])"]
   ["empty"             "[]"    "(empty [1 2 3])"]
   ["not-empty"         "nil"   "(not-empty [])"]
   ["rseq"              "(quote (3 2 1))" "(rseq [1 2 3])"]
   ["replace map"       "[:a :b :a]" "(replace {1 :a 2 :b} [1 2 1])"]

   ### ==== data structures ====
   ["sorted-map seq"    "(quote ([:a 1] [:b 2] [:c 3]))" "(seq (sorted-map :c 3 :a 1 :b 2))"]
   ["sorted-set seq"    "(quote (1 2 3))" "(seq (sorted-set 3 1 2))"]
   ["assoc vector"      "[1 9 3]" "(assoc [1 2 3] 1 9)"]
   ["update vector"     "[1 3 3]" "(update [1 2 3] 1 inc)"]
   ["coll? set"         "true"  "(coll? #{1 2})"]
   ["find entry"        "[:a 1]" "(find {:a 1} :a)"]
   ["conj map entry"    "{:a 1 :b 2}" "(conj {:a 1} [:b 2])"]
   ["conj list prepend" "(quote (0 1 2))" "(conj (list 1 2) 0)"]

   ### ==== keywords / symbols ====
   ["keyword ns"        ":a/b"  "(keyword \"a\" \"b\")"]
   ["name ns-kw"        "\"b\""  "(name :a/b)"]
   ["namespace"         "\"a\""  "(namespace :a/b)"]
   ["namespace none"    "nil"   "(namespace :a)"]

   ### ==== metadata / vars ====
   ["vary-meta"         "{:x 2}" "(meta (vary-meta (with-meta [1] {:x 1}) update :x inc))"]
   ["defonce no-redef"  "1"     "(do (defonce dv1 1) (defonce dv1 2) dv1)"]
   ["binding dynamic"   "10"    "(do (def ^:dynamic *x* 1) (binding [*x* 10] *x*))"]

   ### ==== try / catch ====
   ["try catch"         ":caught" "(try (throw (ex-info \"e\" {})) (catch :default e :caught))"]
   ["ex-data"           "{:a 1}" "(try (throw (ex-info \"m\" {:a 1})) (catch :default e (ex-data e)))"]
   ["ex-message"        "\"m\""  "(try (throw (ex-info \"m\" {})) (catch :default e (ex-message e)))"]

   ### ==== macros ====
   ["macroexpand-1"     "true"  "(do (defmacro mm [x] (list (quote inc) x)) (= (quote (inc 5)) (macroexpand-1 (quote (mm 5)))))"]
   ["doto"              "{:a 1}" "(deref (doto (atom {}) (swap! assoc :a 1)))"]

   ### ==== printing ====
   ["pr-str vec"        "\"[1 2 3]\"" "(pr-str [1 2 3])"]
   ["prn-str"           "\"1\\n\"" "(prn-str 1)"]

   ### ==== characters ====
   ["char?"             "true"  "(char? \\a)"]
   ["char not string"   "false" "(= \\a \"a\")"]
   ["char eq"           "true"  "(= \\a \\a)"]
   ["int of char"       "97"    "(int \\a)"]
   ["char of int"       "true"  "(= \\A (char 65))"]
   ["str of chars"      "\"abc\"" "(str \\a \\b \\c)"]
   ["seq of string"     "(quote (\\a \\b))" "(seq \"ab\")"]
   ["first of string"   "\\h"   "(first \"hello\")"]
   ["nth of string"     "\\e"   "(nth \"hello\" 1)"]
   ["char newline"      "10"    "(int \\newline)"]
   ["char space"        "32"    "(int \\space)"]
   ["char unicode"      "65"    "(int \\u0041)"]
   ["pr-str char"       "\"\\\\a\"" "(pr-str \\a)"]
   ["chars in vec"      "[\\a \\b]" "[\\a \\b]"]
   ["apply str chars"   "\"hi\"" "(apply str [\\h \\i])"]

   ### ==== transducers ====
   ["transduce map"     "9"     "(transduce (map inc) + 0 [1 2 3])"]
   ["transduce comp"    "12"    "(transduce (comp (map inc) (filter even?)) + 0 [1 2 3 4 5])"]
   ["transduce conj"    "[2 3 4]" "(transduce (map inc) conj [] [1 2 3])"]
   ["into xform"        "[2 3 4]" "(into [] (map inc) [1 2 3])"]
   ["into comp xform"   "[1 9 25]" "(into [] (comp (filter odd?) (map (fn [x] (* x x)))) [1 2 3 4 5])"]
   ["into take xform"   "[0 1 2]" "(into [] (take 3) (range 100))"]
   ["sequence xform"    "(quote (2 3 4))" "(sequence (map inc) [1 2 3])"]
   ["transduce no-init" "6"     "(transduce (map inc) + [0 1 2])"]
   ["transduce drop"    "[3 4 5]" "(into [] (drop 2) [1 2 3 4 5])"]
   ["transduce remove"  "[1 3 5]" "(into [] (remove even?) [1 2 3 4 5])"]
   ["transduce take-while" "[1 2]" "(into [] (take-while (fn [x] (< x 3))) [1 2 3 4 1])"]
   ["transduce map-indexed" "[[0 :a] [1 :b]]" "(into [] (map-indexed (fn [i x] [i x])) [:a :b])"]

   ### ==== regex (capturing groups, backtracking, flags, lookahead) ====
   ["re-find groups"    "[\"12-34\" \"12\" \"34\"]" "(re-find #\"(\\d+)-(\\d+)\" \"x12-34y\")"]
   ["re-find no-groups" "\"123\"" "(re-find #\"\\d+\" \"ab123\")"]
   ["re-matches groups" "[\"1.2\" \"1\" \"2\"]" "(re-matches #\"(\\d+)\\.(\\d+)\" \"1.2\")"]
   ["re-matches no"     "nil"   "(re-matches #\"a.c\" \"abcd\")"]
   ["re-seq"            "[\"foo\" \"bar\"]" "(re-seq #\"\\w+\" \"foo bar\")"]
   ["greedy backtrack"  "\"xxfoo\"" "(re-find #\".*foo\" \"xxfoo\")"]
   ["greedy thru group" "[\"a,b,c\" \"a,b\" \"c\"]" "(re-find #\"(.*),(.*)\" \"a,b,c\")"]
   ["lazy quantifier"   "[\"<a>\" \"a\"]" "(re-find #\"<(.+?)>\" \"<a><b>\")"]
   ["flag case-insens"  "\"CAT\"" "(re-find #\"(?i)cat\" \"a CAT\")"]
   ["lookahead"         "\"foo\"" "(re-find #\"foo(?=bar)\" \"foobar\")"]
   ["neg-lookahead"     "\"foo\"" "(re-find #\"foo(?!bar)\" \"foobaz\")"]
   ["word-boundary"     "\"word\"" "(re-find #\"\\bword\\b\" \"a word!\")"]
   ["word-boundary no"  "nil"   "(re-find #\"\\bword\\b\" \"swordfish\")"]
   ["optional group"    "[\"1.2.3\" \"1\" \"2\" \"3\" nil]" "(re-find #\"(\\d+)\\.(\\d+)\\.(\\d+)(?:-([a-z]+))?\" \"1.2.3\")"]
   ["alternation"       "\"dog\"" "(re-find #\"cat|dog\" \"a dog cat\")"]
   ["str/replace $1"    "\"he[ll]o\"" "(do (require (quote [clojure.string :as s])) (s/replace \"hello\" #\"(l+)\" \"[$1]\"))"]
   ["str/replace regex" "\"X-X\"" "(do (require (quote [clojure.string :as s])) (s/replace \"a-b\" #\"[a-z]\" \"X\"))"]

   ### ==== map literals evaluate their values ====
   ["map literal expr"  "{:a 3}"   "{:a (+ 1 2)}"]
   ["map literal var"   "{:k 5}"   "(let [x 5] {:k x})"]
   ["map literal nested" "{:a {:b 2}}" "(let [y 2] {:a {:b y}})"]
   ["map literal keyfn"  "{:x 1}"  "(let [k :x] {k 1})"]
   ["map literal in fn"  "6"       "(do (defn mk [a b] {:sum (+ a b)}) (:sum (mk 2 4)))"]

   ### ---- overlay migration (jolt-1j0): run in all 3 modes ----
   # if-let/when-let bind only in the taken branch (else sees outer scope)
   ["if-let else outer scope" "5"   "(let [x 5] (if-let [x nil] :then x))"]
   ["if-some else outer"     "5"    "(let [x 5] (if-some [x nil] :then x))"]
   ["when-let body multi"    "14"   "(when-let [x 7] (inc x) (* x 2))"]
   # nthrest returns () (not nil) for an exhausted n>0 walk; coll for n<=0
   ["nthrest exhausted"      "(quote ())"  "(nthrest nil 100)"]
   ["nthrest n=0 keeps coll" "[1 2 3]"     "(nthrest [1 2 3] 0)"]
   ["nthnext surprising nil" "nil"         "(nthnext nil nil)"]
   # distinct? compares by value
   ["distinct? equal colls"  "false" "(distinct? [1 2] [1 2])"]
   ["not-any?"               "true"  "(not-any? even? [1 3 5])"]
   ["take-last"              "[3 4]" "(take-last 2 [1 2 3 4])"]
   ["replace nil val"        "[1 nil 3]" "(replace {2 nil} [1 2 3])"]
  ])

# Run every case under a given context factory and return the failures. The same
# cases run under both the interpreter and the compiler: results must match real
# Clojure semantics either way, so the compile path (hybrid: hot compiles,
# unsupported forms fall back to the interpreter) must not diverge.
# mode: {} interpret, {:compile? true} bootstrap compiler, {:selfhost true} the
# self-hosted pipeline (portable Clojure analyzer -> IR -> Janet back end).
(defn- run-cases [mode]
  (def selfhost? (get mode :selfhost))
  (def init-opts (if selfhost? {} mode))
  (defn ev [ctx prog]
    (if selfhost? (selfhost/compile-and-eval ctx (parse-string prog)) (eval-string ctx prog)))
  (def fails @[])
  (each [name expected actual] cases
    (def ctx (init init-opts))
    (def prog (string "(= " expected " " actual ")"))
    (def res (protect (ev ctx prog)))
    (cond
      (not= (res 0) true)
      (array/push fails [name "ERROR" (string (res 1))])
      (= (res 1) true)
      nil
      (let [got (protect (ev (init init-opts) actual))]
        (array/push fails [name "MISMATCH"
                           (string "want=" expected
                                   " got=" (if (= (got 0) true) (string/format "%q" (got 1)) (string "ERR:" (got 1))))]))))
  fails)

(defn- report [label fails]
  (printf "=== CONFORMANCE (%s): %d/%d passed ===" label (- (length cases) (length fails)) (length cases))
  (unless (empty? fails)
    (print "--- Failures ---")
    (each [name kind detail] fails
      (printf "[%s] %s: %s" kind name detail))))

(def interp-fails (run-cases {}))
(report "interpret" interp-fails)
(def compile-fails (run-cases {:compile? true}))
(report "compile" compile-fails)
(def selfhost-fails (run-cases {:selfhost true}))
(report "self-host" selfhost-fails)
(print)
(when (or (pos? (length interp-fails)) (pos? (length compile-fails))
          (pos? (length selfhost-fails)))
  (os/exit 1))
