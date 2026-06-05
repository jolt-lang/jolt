# Specification: stateful reference types (atoms, volatiles, delays, promises).
(use ../support/harness)

(defspec "state / atoms"
  ["deref @"            "0"      "(let [a (atom 0)] @a)"]
  ["deref fn"           "0"      "(deref (atom 0))"]
  ["reset!"             "5"      "(let [a (atom 0)] (reset! a 5) @a)"]
  ["reset! returns new" "5"      "(let [a (atom 0)] (reset! a 5))"]
  ["swap!"              "1"      "(let [a (atom 0)] (swap! a inc) @a)"]
  ["swap! with args"    "10"     "(let [a (atom 1)] (swap! a + 2 3 4) @a)"]
  ["swap! returns new"  "1"      "(let [a (atom 0)] (swap! a inc))"]
  ["swap-vals!"         "[0 1]"  "(let [a (atom 0)] (swap-vals! a inc))"]
  ["reset-vals!"        "[0 9]"  "(let [a (atom 0)] (reset-vals! a 9))"]
  ["compare-and-set! ok" "true"  "(let [a (atom 0)] (compare-and-set! a 0 1))"]
  ["compare-and-set! no" "false" "(let [a (atom 0)] (compare-and-set! a 9 1))"]
  ["atom?"              "true"   "(do (require (quote [clojure.core])) (instance? clojure.lang.Atom (atom 0)))"])

(defspec "state / watches & validators"
  ["add-watch fires"    "1"      "(let [a (atom 0) seen (atom 0)] (add-watch a :k (fn [k r o n] (reset! seen 1))) (reset! a 5) @seen)"]
  ["remove-watch"       "0"      "(let [a (atom 0) seen (atom 0)] (add-watch a :k (fn [k r o n] (swap! seen inc))) (remove-watch a :k) (reset! a 5) @seen)"]
  ["set-validator! ok"  "5"      "(let [a (atom 0)] (set-validator! a number?) (reset! a 5) @a)"]
  ["set-validator! rejects" :throws "(let [a (atom 0)] (set-validator! a pos?) (reset! a -1))"])

(defspec "state / volatiles & delays"
  ["volatile! deref"    "0"      "(let [v (volatile! 0)] @v)"]
  ["vreset!"            "5"      "(let [v (volatile! 0)] (vreset! v 5) @v)"]
  ["vswap!"             "1"      "(let [v (volatile! 0)] (vswap! v inc) @v)"]
  ["delay not forced"   "0"      "(let [c (atom 0) d (delay (swap! c inc))] @c)"]
  ["delay force once"   "1"      "(let [c (atom 0) d (delay (swap! c inc))] (force d) (force d) @c)"]
  ["delay value"        "5"      "(let [d (delay 5)] @d)"]
  ["realized? before"   "false"  "(let [d (delay 5)] (realized? d))"]
  ["realized? after"    "true"   "(let [d (delay 5)] (force d) (realized? d))"])

(defspec "state / promises"
  ["promise deliver"    "5"      "(let [p (promise)] (deliver p 5) @p)"]
  ["promise undelivered" "nil"   "(let [p (promise)] @p)"])
