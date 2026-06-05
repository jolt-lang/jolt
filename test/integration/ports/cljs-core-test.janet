(use ../../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))
(print "CLJS Core Ported Tests")

(print "1: metadata on maps...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= {:foo \"bar\"} (meta (with-meta {:a 1} {:foo \"bar\"})))")) "with-meta on map"))
(print "  ok")

(print "2: atoms...")
(let [ctx (init)]
  (ct-eval ctx "(def a (atom 0))")
  (assert (= true (ct-eval ctx "(= 0 (deref a))")) "deref")
  (assert (= true (ct-eval ctx "(= 1 (swap! a inc))")) "swap! inc")
  (ct-eval ctx "(def b (atom 0))")
  (assert (= true (ct-eval ctx "(= 1 (swap! b + 1))")) "swap! + 1")
  (assert (= true (ct-eval ctx "(= 4 (swap! b + 1 2))")) "swap! + 1 2")
  (assert (= true (ct-eval ctx "(= 10 (swap! b + 1 2 3))")) "swap! + 1 2 3")
  (assert (= true (ct-eval ctx "(= 20 (swap! b + 1 2 3 4))")) "swap! + 1 2 3 4")
  (assert (= true (ct-eval ctx "(atom? (atom 0))")) "atom?")
  (assert (= true (ct-eval ctx "(nil? (meta (atom 0)))")) "atom meta nil"))
(print "  ok")

(print "3: contains?...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(contains? {:a 1 :b 2} :a)")) "contains? map key")
  (assert (= true (ct-eval ctx "(not (contains? {:a 1 :b 2} :z))")) "contains? missing")
  (assert (= true (ct-eval ctx "(contains? [5 6 7] 1)")) "contains? vector index")
  (assert (= true (ct-eval ctx "(contains? [5 6 7] 2)")) "contains? vector index 2")
  (assert (= true (ct-eval ctx "(not (contains? [5 6 7] 3))")) "contains? vector oob")
  (assert (= true (ct-eval ctx "(not (contains? nil 42))")) "contains? nil"))
(print "  ok")

(print "4: get-in...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= 1 (get-in {:foo 1 :bar 2} [:foo]))")) "get-in flat")
  (assert (= true (ct-eval ctx "(= 2 (get-in {:foo {:bar 2}} [:foo :bar]))")) "get-in nested"))
(print "  ok")

(print "5: multimethods...")
(let [ctx (init)]
  (ct-eval ctx "(defmulti greet (fn [x] (:lang x)))")
  (ct-eval ctx "(defmethod greet :en [_] \"hello\")")
  (ct-eval ctx "(defmethod greet :fr [_] \"bonjour\")")
  (assert (= true (ct-eval ctx "(= \"hello\" (greet {:lang :en}))")) "dispatch :en")
  (assert (= true (ct-eval ctx "(= \"bonjour\" (greet {:lang :fr}))")) "dispatch :fr"))
(print "  ok")

(print "6: sequential equality...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= (list 3 2 1) [3 2 1])")) "list = vector")
  (assert (= true (ct-eval ctx "(= () (rest nil))")) "rest nil")
  (assert (= true (ct-eval ctx "(= () (rest [1]))")) "rest [1]")
  (assert (= true (ct-eval ctx "(= () (rest ()))")) "rest empty"))
(print "  ok")

(print "7: seq operations...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(nil? (seq []))")) "seq empty vec"))
(print "  ok")

(print "8: empty and empty?...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(empty? nil)")) "empty? nil")
  (assert (= true (ct-eval ctx "(empty? ())")) "empty? ()")
  (assert (= true (ct-eval ctx "(empty? [])")) "empty? []")
  (assert (= true (ct-eval ctx "(empty? {})")) "empty? {}")
  (assert (= true (ct-eval ctx "(empty? #{})")) "empty? #{}")
  (assert (= true (ct-eval ctx "(empty? \"\")")) "empty? empty string")
  (assert (= true (ct-eval ctx "(not (empty? [1 2]))")) "empty? non-empty")
  (assert (= true (ct-eval ctx "(not (empty? {:a 1}))")) "empty? non-empty map"))
(print "  ok")

(print "9: distinct...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= 0 (count (distinct ())))")) "distinct empty")
  (assert (= true (ct-eval ctx "(= 1 (count (distinct '(1))))")) "distinct single")
  (assert (= true (ct-eval ctx "(= 3 (count (distinct '(1 2 3 1 1 1))))")) "distinct multi count")
  (assert (= true (ct-eval ctx "(= 1 (count (distinct [42 42])))")) "distinct nums count"))
(print "  ok")

(print "10: some and some?...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(some? 1)")) "some? 1")
  (assert (= true (ct-eval ctx "(not (some? nil))")) "some? nil")
  (assert (= true (ct-eval ctx "(some even? [1 2 3])")) "some even?")
  (assert (= true (ct-eval ctx "(nil? (some even? [1 3 5]))")) "some even? nil"))
(print "  ok")

(print "\nAll CLJS Core Ported tests passed!")
