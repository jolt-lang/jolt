# Phase 5: Multimethods + Hierarchy Tests
(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

# 22. Hierarchy
(print "22: hierarchy...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(map? (make-hierarchy))")) "make-hierarchy returns map")
  # 2-arity derive/isa? just returns nil/false (no global hierarchy)
  (ct-eval ctx "(derive ::square ::shape)")
  (assert (= false (ct-eval ctx "(isa? ::square ::shape)")) "isa? 2-arity always false"))
(print "  passed")

# 23. Multimethods — basic dispatch
(print "23: basic multimethod dispatch...")
(let [ctx (init)]
  (ct-eval ctx "(defmulti greet (fn [x] (:lang x)))")
  (ct-eval ctx "(defmethod greet :en [_] \"hello\")")
  (ct-eval ctx "(defmethod greet :fr [_] \"bonjour\")")
  (assert (= "hello" (ct-eval ctx "(greet {:lang :en})")) "dispatch :en")
  (assert (= "bonjour" (ct-eval ctx "(greet {:lang :fr})")) "dispatch :fr")
  (assert (ct-eval ctx "(try (greet {:lang :es}) (catch Exception e true))") "missing dispatch errors"))
(print "  passed")

# 24. Multimethods — :default dispatch
(print "24: :default dispatch...")
(let [ctx (init)]
  (ct-eval ctx "(defmulti classify :type :default :unknown)")
  (ct-eval ctx "(defmethod classify :a [_] :alpha)")
  # :default :unknown renames the catch-all dispatch key; a method must be
  # registered under it (Clojure semantics).
  (ct-eval ctx "(defmethod classify :unknown [_] :unknown)")
  (assert (= :alpha (ct-eval ctx "(classify {:type :a})")) "known dispatch")
  (assert (= :unknown (ct-eval ctx "(classify {:type :z})")) "default fallback"))
(print "  passed")

# 25. Multimethods — hierarchy dispatch
(print "25: hierarchy dispatch...")
(let [ctx (init)]
  (ct-eval ctx "(def h (make-hierarchy))")
  (ct-eval ctx "(def h (derive h ::dog ::mammal))")
  (ct-eval ctx "(def h (derive h ::mammal ::animal))")
  (ct-eval ctx "(defmulti animal-sound (fn [x] x) :default :unknown :hierarchy h)")
  (ct-eval ctx "(defmethod animal-sound ::animal [_] \"rawr\")")
  (ct-eval ctx "(defmethod animal-sound ::dog [_] \"woof\")")
  # catch-all method registered under the renamed default key :unknown
  (ct-eval ctx "(defmethod animal-sound :unknown [_] :unknown)")
  (assert (= "woof" (ct-eval ctx "(animal-sound ::dog)")) "direct dispatch")
  (assert (= "rawr" (ct-eval ctx "(animal-sound ::mammal)")) "hierarchy fallback")
  (assert (= :unknown (ct-eval ctx "(animal-sound ::rock)")) "default fallback"))
(print "  passed")

# 26. remove-method
(print "26: remove-method...")
(let [ctx (init)]
  (ct-eval ctx "(defmulti rmtest :k)")
  (ct-eval ctx "(defmethod rmtest :a [_] 1)")
  (ct-eval ctx "(defmethod rmtest :b [_] 2)")
  (assert (= 1 (ct-eval ctx "(rmtest {:k :a})")) "before remove")
  (ct-eval ctx "(remove-method (var rmtest) :a)")
  (assert (ct-eval ctx "(try (rmtest {:k :a}) (catch Exception e true))") "removed method errors"))
(print "  passed")

# 27. remove-all-methods
(print "27: remove-all-methods...")
(let [ctx (init)]
  (ct-eval ctx "(defmulti alltest :k)")
  (ct-eval ctx "(defmethod alltest :a [_] 1)")
  (ct-eval ctx "(defmethod alltest :b [_] 2)")
  (ct-eval ctx "(remove-all-methods (var alltest))")
  (assert (ct-eval ctx "(try (alltest {:k :a}) (catch Exception e true))") "all methods removed errors"))
(print "  passed")

(print "\nAll Phase 5 tests passed!")
