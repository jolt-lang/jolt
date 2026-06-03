(use ../src/jolt/api)
(use ../src/jolt/reader)
(use ../src/jolt/evaluator)

(defn ct-eval [ctx s] (eval-string ctx s))

(defn load-clj [ctx filepath]
  (def source (slurp filepath))
  (var remaining source)
  (while (> (length (string/trim remaining)) 0)
    (def fr (parse-next remaining))
    (def form (fr 0))
    (set remaining (fr 1))
    (when (not (nil? form))
      (eval-form ctx @{} form))))

(print "40: clojure.string...")
(let [ctx (init)]
  (load-clj ctx "src/jolt/clojure/string.clj")
  (assert (= true (ct-eval ctx "(blank? nil)")) "blank? nil")
  (assert (= true (ct-eval ctx "(blank? \"   \")")) "blank? whitespace")
  (assert (= false (ct-eval ctx "(blank? \"a\")")) "blank? non-empty")
  (assert (= "Abc" (ct-eval ctx "(capitalize \"abc\")")) "capitalize")
  (assert (= "hello" (ct-eval ctx "(lower-case \"HELLO\")")) "lower-case")
  (assert (= "HELLO" (ct-eval ctx "(upper-case \"hello\")")) "upper-case")
  (assert (= true (ct-eval ctx "(includes? \"hello\" \"ell\")")) "includes? true")
  (assert (= "foo" (ct-eval ctx "(trim \"foo\")")) "trim")
  (assert (= true (ct-eval ctx "(starts-with? \"hello\" \"he\")")) "starts-with? true")
  (assert (= true (ct-eval ctx "(ends-with? \"hello\" \"lo\")")) "ends-with? true"))
(print "  passed")

(print "41: clojure.set...")
(let [ctx (init)]
  (load-clj ctx "src/jolt/clojure/set.clj")
  (assert (= #{1 2 3} (ct-eval ctx "(union #{1 2} #{2 3})")) "union")
  (assert (= #{2} (ct-eval ctx "(intersection #{1 2} #{2 3})")) "intersection")
  (assert (= #{1} (ct-eval ctx "(difference #{1 2} #{2 3})")) "difference")
  (assert (= true (ct-eval ctx "(subset? #{1} #{1 2 3})")) "subset? true")
  (assert (= true (ct-eval ctx "(superset? #{1 2 3} #{1})")) "superset? true"))
(print "  passed")

(print "All Phase 10 tests passed!")
