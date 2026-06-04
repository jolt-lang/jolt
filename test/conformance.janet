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

(use ../src/jolt/api)

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
  ])

(var pass 0)
(def fails @[])
(each [name expected actual] cases
  (def ctx (init))
  (def prog (string "(= " expected " " actual ")"))
  (def res (protect (eval-string ctx prog)))
  (cond
    (not= (res 0) true)
    (array/push fails [name "ERROR" (string (res 1))])
    (= (res 1) true)
    (++ pass)
    # not equal: re-eval actual alone to show what we got
    (let [got (protect (eval-string (init) actual))]
      (array/push fails [name "MISMATCH"
                         (string "want=" expected
                                 " got=" (if (= (got 0) true) (string/format "%q" (got 1)) (string "ERR:" (got 1))))]))))

(printf "\n=== CONFORMANCE: %d/%d passed ===" pass (length cases))
(unless (empty? fails)
  (print "\n--- Failures ---")
  (each [name kind detail] fails
    (printf "[%s] %s: %s" kind name detail)))
(print)
(when (pos? (length fails)) (os/exit 1))
