(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "Janet Interop Tests")

(print "1: field access on tables...")
(let [ctx (init)]
  (ct-eval ctx "(def t {:a 1 :b 2})")
  (assert (= 1 (ct-eval ctx "(. t :a)")) ". field access table")
  (assert (= 2 (ct-eval ctx "(. t :b)")) ". field access table 2")
  (assert (= nil (ct-eval ctx "(. t :z)")) ". field access missing key"))
(print "  ok")

(print "2: field access on structs...")
(let [ctx (init)]
  (ct-eval ctx "(def s {:x 10 :y 20})")
  (assert (= 10 (ct-eval ctx "(. s :x)")) ". field access struct")
  (assert (= 20 (ct-eval ctx "(. s :y)")) ". field access struct 2"))
(print "  ok")

(print "3: method calls on tables...")
(let [ctx (init)]
  (ct-eval ctx "(def obj {:greet (fn [self name] (str \"Hello \" name))})")
  (assert (= "Hello Alice" (ct-eval ctx "(. obj greet \"Alice\")")) ". method call on table"))
(print "  ok")

(print "4: field access via .- reader sugar...")
(let [ctx (init)]
  (ct-eval ctx "(def t {:x 42 :len 5})")
  (assert (= 42 (ct-eval ctx "(.-x t)")) ".-x reader sugar")
  (assert (= 5 (ct-eval ctx "(.-len t)")) ".-len reader sugar"))
(print "  ok")

(print "5: . field access still works on deftypes...")
(let [ctx (init)]
  (ct-eval ctx "(deftype Point [x y])")
  (assert (= 3 (ct-eval ctx "(. (Point. 3 4) x)")) ". field access deftype"))
(print "  ok")

(print "6: method call with multiple args...")
(let [ctx (init)]
  (ct-eval ctx "(def calc {:add (fn [_ a b] (+ a b)) :mul (fn [_ a b] (* a b))})")
  (assert (= 7 (ct-eval ctx "(. calc add 3 4)")) ". method add")
  (assert (= 12 (ct-eval ctx "(. calc mul 3 4)")) ". method mul"))
(print "  ok")

(print "\nAll Janet Interop tests passed!")
