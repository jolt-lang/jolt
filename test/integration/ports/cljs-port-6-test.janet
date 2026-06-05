(use ../../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "=== CLJS Ported Part 6 ===")

(print "28: anon fn #()...")
(let [ctx (init)]
  (assert (= 6 (ct-eval ctx "(#(+ % 5) 1)")) "#() %")
  (assert (= 0 (ct-eval ctx "(#(do 0))")) "#() do body"))
(print "  ok")

(print "29: symbol operations...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(symbol? 'foo)")) "symbol?")
  (assert (= "foo" (ct-eval ctx "(name 'foo)")) "name symbol")
  (assert (= "bar" (ct-eval ctx "(name :bar)")) "name keyword"))
(print "  ok")

(print "30: keyword operations...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(keyword? :foo)")) "keyword?")
  (assert (= :foo (ct-eval ctx "(keyword \"foo\")")) "keyword string"))
(print "  ok")

(print "31: list operations...")
(let [ctx (init)]
  (assert (= 3 (ct-eval ctx "(count (list 3 2 1))")) "list count")
  (assert (= 1 (ct-eval ctx "(first '(1 2 3))")) "first list"))
(print "  ok")

(print "\nAll CLJS Ported Part 6 tests passed!\n")
