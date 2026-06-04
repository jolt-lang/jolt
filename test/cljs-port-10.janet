(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "=== CLJS Ported Part 10 ===")

(print "43: when/when-not...")
(let [ctx (init)]
  (assert (= 42 (ct-eval ctx "(when true 42)")) "when true")
  (assert (= nil (ct-eval ctx "(when false 42)")) "when false")
  (assert (= 42 (ct-eval ctx "(when-not false 42)")) "when-not false"))
(print "  ok")

(print "44: if-let/when-let...")
(let [ctx (init)]
  (assert (= 2 (ct-eval ctx "(if-let [x 1] (inc x) 0)")) "if-let true")
  (assert (= 0 (ct-eval ctx "(if-let [x nil] (inc x) 0)")) "if-let nil")
  (assert (= 2 (ct-eval ctx "(when-let [x 1] (inc x))")) "when-let"))
(print "  ok")

(print "45: doto...")
(let [ctx (init)]
  (ct-eval ctx "(def x (atom []))")
  (assert (= nil (ct-eval ctx "(doto nil)")) "doto nil returns nil"))
(print "  ok")

(print "\nAll CLJS Ported Part 10 tests passed!\n")
