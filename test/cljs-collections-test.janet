(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "CLJS Collections Ported Tests")

(print "1: dissoc...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= {:a :b} (dissoc {:a :b :c :d} :c))")) "dissoc")
  (assert (= true (ct-eval ctx "(= {} (dissoc {1 2 3 4} 1 3))")) "dissoc multi"))
(print "  ok")

(print "2: assoc...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= {1 2 3 4} (assoc {} 1 2 3 4))")) "assoc multi")
  (assert (= true (ct-eval ctx "(= {1 2} (assoc {} 1 2))")) "assoc single"))
(print "  ok")

(print "3: set operations...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(set? (set []))")) "set empty")
  (assert (= true (ct-eval ctx "(= #{\"foo\"} (set [\"foo\"]))")) "set from vec")
  (assert (= true (ct-eval ctx "(= #{1 2 3} #{1 3 2})")) "set order")
  (assert (= true (ct-eval ctx "(= #{1 2 3} (disj #{1 2 3}))")) "disj none")
  (assert (= true (ct-eval ctx "(= #{1 2} (disj #{1 2 3} 3))")) "disj one")
  (assert (= true (ct-eval ctx "(= #{1} (disj #{1 2 3} 2 3))")) "disj multi")
  (assert (= true (ct-eval ctx "(= 4 (get #{1 2 3 4} 4))")) "get set")
  (assert (= true (ct-eval ctx "(contains? #{1 2 3 4} 4)")) "contains? set"))
(print "  ok")

(print "4: vector nth...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= :a (nth [:a :b :c :d] 0))")) "nth")
  (assert (= true (ct-eval ctx "(= :c (nth [:a :b :c :d] 2 0.1))")) "nth float"))
(print "  ok")

(print "5: range...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= 0 (count (range 10 0 1)))")) "range empty")
  (assert (= true (ct-eval ctx "(= 4 (count (range 0 10 3)))")) "range count")
  (assert (= true (ct-eval ctx "(= 1 (count (range 0 1 1)))")) "range single"))
(print "  ok")

(print "\nAll CLJS Collections Ported tests passed!")
