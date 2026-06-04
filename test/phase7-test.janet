# Phase 7: LazySeq + PersistentHashSet completion
(use ../src/jolt/api)
(defn ct-eval [ctx s] (eval-string ctx s))

(print "32: lazy-seq...")
(let [ctx (init)]
  (let [ls (ct-eval ctx "(lazy-seq (cons 1 (lazy-seq (cons 2 nil))))")]
    (assert (not (nil? ls)) "lazy-seq returns non-nil")
    (assert (= 1 (ct-eval ctx "(first (lazy-seq (cons 1 nil)))")) "first of lazy"))
  (assert (= true (ct-eval ctx "(= [1 2 3] (seq (lazy-seq [1 2 3])))")) "seq forces lazy")
  (eval-string ctx "(def counter (atom 0))")
  (def val (ct-eval ctx "(let [ls (lazy-seq (do (swap! counter inc) [1 2 3]))] (seq ls) (seq ls) @counter)"))
  (assert (= 1 val) "realized once"))
(print "  passed")

(print "33: PersistentHashSet...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(set? #{1 2 3})")) "set? true")
  (assert (= false (ct-eval ctx "(set? [1 2 3])")) "set? false")
  (assert (= 4 (ct-eval ctx "(count (conj #{1 2 3} 4))")) "conj add")
  (assert (= 2 (ct-eval ctx "(count (disj #{1 2 3} 3))")) "disj")
  (assert (= 3 (ct-eval ctx "(count #{1 2 3})")) "count")
  (assert (= true (ct-eval ctx "(= #{1 2 3} #{3 2 1})")) "= order-independent"))

(print "\nAll Phase 7 tests passed!")
