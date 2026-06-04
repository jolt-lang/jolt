(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "Eval Tests")

(print "1: eval literal...")
(let [ctx (init)]
  (assert (= 42 (ct-eval ctx "(eval 42)")) "eval literal")
  (assert (= 3 (ct-eval ctx "(eval '(+ 1 2))")) "eval quoted form")
  (assert (= 3 (ct-eval ctx "(eval (eval '(+ 1 2)))")) "eval nested")
  (ct-eval ctx "(eval '(def ex 99))")
  (assert (= 99 (ct-eval ctx "ex")) "eval defines var"))
(print "  ok")

(print "\nAll Eval tests passed!")
