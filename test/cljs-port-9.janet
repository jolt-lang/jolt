(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "=== CLJS Ported Part 9 ===")

(print "40: seq predicates...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(empty? [])")) "empty? true")
  (assert (= false (ct-eval ctx "(empty? [1])")) "empty? false")
  (assert (= true (ct-eval ctx "(every? pos? [1 2 3])")) "every? pos"))
(print "  ok")

(print "41: complement...")
(let [ctx (init)]
  (assert (= false (ct-eval ctx "((complement pos?) 1)")) "complement pos")
  (assert (= true (ct-eval ctx "((complement pos?) -1)")) "complement neg"))
(print "  ok")

(print "\nAll CLJS Ported Part 9 tests passed!\n")
