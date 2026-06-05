(use ../../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "=== CLJS Ported Part 7 ===")

(print "32: seq destructuring...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(let [[x] [1 2 3]] x)")) "seq destructure")
  (assert (= 2 (ct-eval ctx "(let [[_ y] [1 2 3]] y)")) "seq destructure skip")
  (assert (= 3 (ct-eval ctx "(let [[_ _ z] [1 2 3]] z)")) "seq destructure end"))
(print "  ok")

(print "33: & rest args...")
(let [ctx (init)]
  (ct-eval ctx "(defn sum [& xs] (apply + xs))")
  (assert (= 6 (ct-eval ctx "(sum 1 2 3)")) "& rest sum")
  (ct-eval ctx "(defn first-two [a b & rest] (count rest))")
  (assert (= 2 (ct-eval ctx "(first-two 1 2 3 4)")) "& rest after fixed"))
(print "  ok")

(print "\nAll CLJS Ported Part 7 tests passed!\n")
