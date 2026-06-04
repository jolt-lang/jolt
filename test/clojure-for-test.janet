# Ported from clojure/test_clojure/for.clj
(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

(print "Ported For Tests")

# --- When ---
(print "test-for :when...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(= (for [x (range 10) :when (odd? x)] x)
        (quote (1 3 5 7 9)))")) "for :when odd?")
  (assert (= true (ct-eval ctx
    "(= (for [x (range 4) y (range 4) :when (odd? y)] [x y])
        (quote ([0 1] [0 3] [1 1] [1 3] [2 1] [2 3] [3 1] [3 3])))")) "for nested :when"))
(print "  ok")

# --- Let ---
(print "test-for :let...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(= (for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])
        (quote ([0 1 1] [1 0 1] [1 2 3] [2 1 3])))")) "for :let :when"))
(print "  ok")

# --- Nesting ---
(print "test-for nesting...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(= (for [x (quote (a b)) y (interpose x (quote (1 2))) z (list x y)] [x y z])
        (quote ([a 1 a] [a 1 1] [a a a] [a a a] [a 2 a] [a 2 2]
                [b 1 b] [b 1 1] [b b b] [b b b] [b 2 b] [b 2 2])))")) "for nested interpose"))
(print "  ok")

(print "\nAll Ported For tests passed!")
