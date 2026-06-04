# Ported from clojure/test_clojure/atoms.clj + systematic atom tests
(use ../src/jolt/api)
(defn ct-eval [ctx s] (eval-string ctx s))

(print "Ported Atom Tests")

# --- atom creation ---
(print "test atom creation...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(atom? (atom 0))")) "atom?")
  (assert (= false (ct-eval ctx "(atom? nil)")) "atom? nil")
  (assert (= false (ct-eval ctx "(atom? 42)")) "atom? number")
  (assert (= false (ct-eval ctx "(atom? \"x\")")) "atom? string")
  (assert (= 42 (ct-eval ctx "(deref (atom 42))")) "deref atom")
  (assert (= 99 (ct-eval ctx "(let [a (atom 99)] @a)")) "@ deref macro"))
(print "  ok")

# --- deref on non-atoms ---
(print "test deref...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(deref 1)")) "deref non-atom passes through")
  (assert (= "x" (ct-eval ctx "(deref \"x\")")) "deref string passes through")
  (assert (= nil (ct-eval ctx "(deref nil)")) "deref nil"))
(print "  ok")

# --- reset! ---
(print "test reset!...")
(let [ctx (init)]
  (assert (= :b (ct-eval ctx "(let [a (atom :a)] (reset! a :b))")) "reset! returns new")
  (assert (= 42 (ct-eval ctx "(let [a (atom 0)] (reset! a 42) @a)")) "reset! updates value")
  (assert (= true (ct-eval ctx "(= [1 1] (let [a (atom 0)] [(reset! a 1) @a]))")) "reset! returns new, value changed"))
(print "  ok")

# --- swap! ---
(print "test swap!...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(let [a (atom 0)] (swap! a inc))")) "swap! inc returns new")
  (assert (= 2 (ct-eval ctx "(let [a (atom 0)] (swap! a + 2))")) "swap! + 2")
  (assert (= 3 (ct-eval ctx "(let [a (atom 0)] (swap! a + 1 2) @a)")) "swap! + 1 2")
  (assert (= 6 (ct-eval ctx "(let [a (atom 0)] (swap! a + 1 2 3) @a)")) "swap! + 1 2 3")
  (assert (= 10 (ct-eval ctx "(let [a (atom 0)] (swap! a + 1 2 3 4) @a)")) "swap! + 1 2 3 4"))
(print "  ok")

# --- swap-vals! (returns [old new]) ---
(print "test swap-vals!...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(= [0 1] (let [a (atom 0)] (swap-vals! a inc)))")) "swap-vals! inc")
  (assert (= true (ct-eval ctx
    "(= [1 2] (let [a (atom 1)] (swap-vals! a inc)))")) "swap-vals! inc from 1")
  (assert (= 2 (ct-eval ctx
    "(let [a (atom 1)] (swap-vals! a inc) @a)")) "swap-vals! updates value"))
(print "  ok")

# --- swap-vals! with extra args ---
(print "test swap-vals! arities...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(= [0 1] (let [a (atom 0)] (swap-vals! a + 1)))")) "swap-vals! + 1")
  (assert (= true (ct-eval ctx
    "(= [1 3] (let [a (atom 0)] (swap-vals! a + 1) (swap-vals! a + 1 1)))")) "swap-vals! + 1 1")
  (assert (= true (ct-eval ctx
    "(= [3 6] (let [a (atom 0)] (swap-vals! a + 1) (swap-vals! a + 1 1) (swap-vals! a + 1 1 1)))")) "swap-vals! + 1 1 1")
  (assert (= true (ct-eval ctx
    "(= [6 10] (let [a (atom 0)] (swap-vals! a + 1) (swap-vals! a + 1 1) (swap-vals! a + 1 1 1) (swap-vals! a + 1 1 1 1)))")) "swap-vals! + 1 1 1 1"))
(print "  ok")

# --- reset-vals! (returns [old new]) ---
(print "test reset-vals!...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(= [0 :b] (let [a (atom 0)] (reset-vals! a :b)))")) "reset-vals! returns old new")
  (assert (= true (ct-eval ctx
    "(= [:b 42] (let [a (atom 0)] (reset-vals! a :b) (reset-vals! a 42)))")) "reset-vals! chain")
  (assert (= 42 (ct-eval ctx
    "(let [a (atom 0)] (reset-vals! a :b) (reset-vals! a 42) @a)")) "reset-vals! updates value"))
(print "  ok")

# --- compare-and-set! ---
(print "test compare-and-set!...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(let [a (atom 0)] (compare-and-set! a 0 1))")) "CAS true match")
  (assert (= true (ct-eval ctx
    "(= [true 1] (let [a (atom 0)] [(compare-and-set! a 0 1) @a]))")) "CAS true + value changed")
  (assert (= true (ct-eval ctx
    "(= [false 0] (let [a (atom 0)] [(compare-and-set! a 1 2) @a]))")) "CAS false no match")
  (assert (= true (ct-eval ctx
    "(= [false 1] (let [a (atom 0)] (compare-and-set! a 0 1) [(compare-and-set! a 0 2) @a]))")) "CAS false after change"))
(print "  ok")

# --- validator ---
(print "test validator...")
(let [ctx (init)]
  (assert (= 42 (ct-eval ctx
    "(let [a (atom 0 :validator pos?)] (reset! a 42) @a)")) "validator passes")
  (assert (= true (ct-eval ctx
    "(= false (try (let [a (atom 0 :validator pos?)] (reset! a -1) true) (catch Exception e false)))")) "validator blocks invalid reset!")
  (assert (= true (ct-eval ctx
    "(= false (try (let [a (atom 0 :validator pos?)] (swap! a (fn [x] -1)) true) (catch Exception e false)))")) "validator blocks invalid swap!")
  (assert (= nil (ct-eval ctx "(set-validator! (atom 0) pos?)")) "set-validator! returns nil")
  (assert (= nil (ct-eval ctx "(get-validator (atom 0))")) "get-validator nil default")
  (assert (= true (ct-eval ctx
    "(= even? (do (def a (atom 0)) (set-validator! a even?) (get-validator a)))")) "get-validator returns set fn"))
(print "  ok")

# --- watches ---
(print "test watches...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx
    "(empty? (:watches (atom 0)))")) "atom starts with empty watches")
  (assert (= true (ct-eval ctx
    "(= [0 42] (let [a (atom 0)
                 w (atom nil)]
             (add-watch a :my-key (fn [k ref old new] (reset! w [old new])))
             (swap! a + 42)
             @w))")) "add-watch triggers on swap!")
  (assert (= true (ct-eval ctx
    "(= :unchanged (let [a (atom 0)
                       w (atom :unchanged)]
                   (add-watch a :x (fn [_ _ _ _] (reset! w :fired)))
                   (remove-watch a :x)
                   (swap! a inc)
                   @w))")) "remove-watch stops notification")
  (assert (= true (ct-eval ctx
    "(= 2 (let [a (atom 0)]
           (add-watch a :foo (fn [_ _ _ _] nil))
           (add-watch a :bar (fn [_ _ _ _] nil))
           (count (:watches a))))")) "multiple watches")
  (assert (= true (ct-eval ctx
    "(= 0 (let [a (atom 0)]
           (add-watch a :foo (fn [_ _ _ _] nil))
           (remove-watch a :foo)
           (count (:watches a))))")) "remove-watch clears count")
  (assert (= true (ct-eval ctx
    "(= 2 (let [a (atom 0)
               fired (atom [])]
           (add-watch a :w1 (fn [_ _ o n] (swap! fired conj [:w1 o n])))
           (add-watch a :w2 (fn [_ _ o n] (swap! fired conj [:w2 o n])))
           (reset! a 99)
           (count @fired)))")) "multiple watches both fire"))
(print "  ok")

# --- metadata on atoms ---
(print "test atom metadata...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(meta (atom 0))")) "atom meta nil by default")
  (assert (= true (ct-eval ctx
    "(= {:foo \"bar\"} (meta (atom 0 :meta {:foo \"bar\"})))")) "atom with :meta")
  (assert (= true (ct-eval ctx
    "(= {:validated true} (meta (atom 0 :validator pos? :meta {:validated true})))")) "atom with validator and meta"))
(print "  ok")

(print "\nAll Ported Atom tests passed!")
