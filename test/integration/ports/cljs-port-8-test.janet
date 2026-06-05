(use ../../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "=== CLJS Ported Part 8 ===")

(print "35: range and repeat...")
(let [ctx (init)]
  (assert (= 5 (ct-eval ctx "(count (range 5))")) "range")
  (assert (= 4 (ct-eval ctx "(count (repeat 4 :x))")) "repeat")
  (assert (= 3 (ct-eval ctx "(count (repeatedly 3 (constantly :y)))")) "repeatedly"))
(print "  ok")

(print "36: concat and into...")
(let [ctx (init)]
  (assert (= 4 (ct-eval ctx "(count (concat [1 2] [3 4]))")) "concat")
  (assert (= 4 (ct-eval ctx "(count (into [] (range 4)))")) "into"))
(print "  ok")

(print "37: take-while/drop-while...")
(let [ctx (init)]
  (assert (= 2 (ct-eval ctx "(count (take-while even? [2 4 3 5]))")) "take-while")
  (assert (= 2 (ct-eval ctx "(count (drop-while even? [2 4 3 5]))")) "drop-while"))
(print "  ok")

(print "38: partition...")
(let [ctx (init)]
  (assert (= 2 (ct-eval ctx "(count (partition 2 [1 2 3 4]))")) "partition 2"))
(print "  ok")

(print "39: sorting...")
(let [ctx (init)]
  (assert (= [1 2 3] (ct-eval ctx "(sort [3 1 2])")) "sort")
  (assert (= [1 2 3] (ct-eval ctx "(distinct [1 2 1 3 2])")) "distinct"))
(print "  ok")

(print "\nAll CLJS Ported Part 8 tests passed!\n")
