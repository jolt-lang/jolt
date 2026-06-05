(use ../../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "=== CLJS Ported Part 4 ===")

(print "23: deftype/defrecord...")
(let [ctx (init)]
  (ct-eval ctx "(deftype Point [x y])")
  (assert (= true (ct-eval ctx "(instance? Point (Point. 3 4))")) "instance? true")
  (assert (= 3 (ct-eval ctx "(. (Point. 3 4) x)")) ".field access")
  (ct-eval ctx "(defrecord Person [name age])")
  (assert (= true (ct-eval ctx "(map? (Person. \"A\" 30))")) "record is map?")
  (assert (= "Alice" (ct-eval ctx "(:name (Person. \"Alice\" 30))")) "record keyword access"))
(print "  ok")

(print "24: multimethods...")
(let [ctx (init)]
  (ct-eval ctx "(defmulti greet (fn [x] (:lang x)))")
  (ct-eval ctx "(defmethod greet :en [_] \"hello\")")
  (ct-eval ctx "(defmethod greet :fr [_] \"bonjour\")")
  (assert (= "hello" (ct-eval ctx "(greet {:lang :en})")) "dispatch :en")
  (assert (= "bonjour" (ct-eval ctx "(greet {:lang :fr})")) "dispatch :fr"))
(print "  ok")

(print "25: protocols...")
(let [ctx (init)]
  (ct-eval ctx "(defprotocol Greet (g [this]))")
  (ct-eval ctx "(deftype Dog [name])")
  (ct-eval ctx "(extend-type Dog Greet (g [this] (str \"woof \" (.-name this))))")
  (assert (= "woof Rex" (ct-eval ctx "(g (Dog. \"Rex\"))")) "extend-type"))
(print "  ok")

(print "26: var system...")
(let [ctx (init)]
  (ct-eval ctx "(def xv 42)")
  (assert (= true (ct-eval ctx "(var? (var xv))")) "var?")
  (assert (= 42 (ct-eval ctx "(var-get (var xv))")) "var-get")
  (ct-eval ctx "(var-set (var xv) 99)")
  (assert (= 99 (ct-eval ctx "(var-get (var xv))")) "var-set"))
(print "  ok")

(print "27: range/into/concat...")
(let [ctx (init)]
  (assert (= 5 (ct-eval ctx "(count (range 5))")) "range count")
  (assert (= [0 1 2 3 4] (ct-eval ctx "(into [] (range 5))")) "range into vec")
  (assert (= 4 (ct-eval ctx "(count (concat [1 2] [3 4]))")) "concat count"))
(print "  ok")

(print "\nAll CLJS Ported Part 4 tests passed!")
