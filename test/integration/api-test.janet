(use ../../src/jolt/api)
(use ../../src/jolt/types)

(print "1: init creates context...")
(let [ctx (init)]
  (assert (ctx? ctx) "init returns context")
  (let [ns (ctx-find-ns ctx "clojure.core")]
    (assert (ns? ns) "clojure.core namespace exists")
    (assert (ns-find ns "nil?") "nil? is interned")
    (assert (ns-find ns "+") "+ is interned")))
(print "  passed")

(print "2: eval-string basics...")
(let [ctx (init)]
  (assert (= 42 (eval-string ctx "42")) "eval integer")
  (assert (= true (eval-string ctx "true")) "eval bool")
  (assert (= 3 (eval-string ctx "(+ 1 2)")) "eval list"))
(print "  passed")

(print "3: eval-string with core fns...")
(let [ctx (init)]
  (assert (= true (eval-string ctx "(nil? nil)")) "nil?")
  (assert (deep= [2 3 4] (normalize-pvecs (eval-string ctx "(map inc [1 2 3])"))) "map+inc")
  (assert (= 6 (eval-string ctx "(reduce + [1 2 3])")) "reduce"))
(print "  passed")

(print "4: eval-string with def...")
(let [ctx (init)]
  (eval-string ctx "(def x 42)")
  (assert (= 42 (eval-string ctx "x")) "def then resolve"))
(print "  passed")

(print "5: eval-string* with bindings...")
(let [ctx (init)]
  (assert (= 99 (eval-string* ctx "y" @{"y" 99})) "bound variable"))
(print "  passed")

(print "\nAll API tests passed!")
