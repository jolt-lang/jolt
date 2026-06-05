# Phase 12: Protocol System Tests
(use ../../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

(print "35: defprotocol...")
(let [ctx (init)]
  (ct-eval ctx "(defprotocol Greet (greet [this]))")
  (let [p (ct-eval ctx "Greet")]
    (assert (not (nil? p)) "protocol var exists")
    (assert (= :jolt/protocol (get p :jolt/type)) "protocol type tag")
    (assert (get (get p :methods) :greet) "protocol has greet method"))
  (assert (or (function? (ct-eval ctx "greet")) (cfunction? (ct-eval ctx "greet"))) "method fn exists"))
(print "  passed")

(print "36: extend-type...")
(let [ctx (init)]
  (ct-eval ctx "(deftype Person [name])")
  (ct-eval ctx "(defprotocol Namable (get-name [this]))")
  (ct-eval ctx "(extend-type Person Namable (get-name [this] (.-name this)))")
  (assert (= "Alice" (ct-eval ctx "(get-name (Person. \"Alice\"))")) "extend-type works"))
(print "  passed")

(print "37: extend-protocol...")
(let [ctx (init)]
  (ct-eval ctx "(deftype Dog [breed])")
  (ct-eval ctx "(deftype Cat [color])")
  (ct-eval ctx "(defprotocol Animal (speak [this]))")
  (ct-eval ctx "(extend-protocol Animal
    Dog (speak [this] (str \"woof from \" (.-breed this)))
    Cat (speak [this] (str \"meow from \" (.-color this))))")
  (assert (= "woof from poodle" (ct-eval ctx "(speak (Dog. \"poodle\"))")) "dog speak")
  (assert (= "meow from black" (ct-eval ctx "(speak (Cat. \"black\"))")) "cat speak"))
(print "  passed")

(print "38: satisfies?...")
(let [ctx (init)]
  (ct-eval ctx "(deftype Point [x y])")
  (ct-eval ctx "(defprotocol Locatable (location [this]))")
  (ct-eval ctx "(extend-type Point Locatable (location [this] [(.-x this) (.-y this)]))")
  (assert (= true (ct-eval ctx "(satisfies? Locatable (Point. 3 4))")) "satisfies? true")
  (assert (= false (ct-eval ctx "(satisfies? Locatable {:x 1})")) "satisfies? false"))
(print "  passed")

(print "39: reify...")
(let [ctx (init)]
  (ct-eval ctx "(defprotocol Stringable (to-str [this]))")
  (assert (= "works" (ct-eval ctx "(to-str (reify Stringable (to-str [this] \"works\")))")) "reify single method"))
(print "  passed")

(print "\nAll Phase 12 tests passed!")
