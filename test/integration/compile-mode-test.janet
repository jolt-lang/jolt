(use ../../src/jolt/api)

(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

(print "Phase 6: comprehensive compile-mode tests...")
(let [ctx (init {:compile? true})]

  (print "  collections...")
  (assert (= :a (ct-eval ctx "(nth [:a :b :c :d] 0)")) "nth")
  (assert (= true (ct-eval ctx "(vector? [1 2])")) "vector?")
  (assert (= true (ct-eval ctx "(map? {:a 1})")) "map?")
  (assert (= true (ct-eval ctx "(fn? inc)")) "fn?")
  (assert (= [1 2 3 4] (ct-eval ctx "(conj [1 2 3] 4)")) "conj")
  (assert (= 1 (ct-eval ctx "(first [1 2 3])")) "first")
  (assert (= [2 3] (ct-eval ctx "(rest [1 2 3])")) "rest")
  (assert (= 1 (ct-eval ctx "(get {:a 1 :b 2} :a)")) "get map")
  (assert (= nil (ct-eval ctx "(get {:a 1} :z)")) "get missing")
  (assert (= 3 (ct-eval ctx "(count {:a 1 :b 2 :c 3})")) "count map")
  (assert (= [1 2 3] (ct-eval ctx "(into [1] [2 3])")) "into")

  (print "  core math...")
  (assert (= 3 (ct-eval ctx "(+ 1 2)")) "+")
  (assert (= 1 (ct-eval ctx "(- 3 2)")) "-")
  (assert (= 6 (ct-eval ctx "(* 2 3)")) "*")
  (assert (= 2 (ct-eval ctx "(/ 4 2)")) "/")
  (assert (= 3 (ct-eval ctx "(inc 2)")) "inc")
  (assert (= 1 (ct-eval ctx "(dec 2)")) "dec")
  (assert (= 1 (ct-eval ctx "(quot 5 3)")) "quot")
  (assert (= 2 (ct-eval ctx "(rem 5 3)")) "rem")
  (assert (= 2 (ct-eval ctx "(mod 5 3)")) "mod")
  (assert (= 3 (ct-eval ctx "(max 1 2 3)")) "max")
  (assert (= 1 (ct-eval ctx "(min 1 2 3)")) "min")

  (print "  predicates...")
  (assert (= true (ct-eval ctx "(nil? nil)")) "nil?")
  (assert (= false (ct-eval ctx "(nil? 1)")) "nil? false")
  (assert (= true (ct-eval ctx "(zero? 0)")) "zero?")
  (assert (= true (ct-eval ctx "(pos? 5)")) "pos?")
  (assert (= true (ct-eval ctx "(neg? -1)")) "neg?")
  (assert (= true (ct-eval ctx "(even? 4)")) "even?")
  (assert (= true (ct-eval ctx "(odd? 3)")) "odd?")
  (assert (= false (ct-eval ctx "(not true)")) "not")
  (assert (= true (ct-eval ctx "(some? 1)")) "some?")
  (assert (= true (ct-eval ctx "(string? \"hello\")")) "string?")
  (assert (= true (ct-eval ctx "(number? 42)")) "number?")
  (assert (= true (ct-eval ctx "(keyword? :foo)")) "keyword?")
  (assert (= true (ct-eval ctx "(= 1 1)")) "=")
  (assert (= true (ct-eval ctx "(< 1 2)")) "<")
  (assert (= true (ct-eval ctx "(> 2 1)")) ">")
  (assert (= true (ct-eval ctx "(<= 1 1)")) "<=")
  (assert (= true (ct-eval ctx "(>= 2 2)")) ">=")

  (print "  seq operations...")
  (assert (= [2 3 4] (ct-eval ctx "(map inc [1 2 3])")) "map")
  (assert (= [2 4] (ct-eval ctx "(filter even? [1 2 3 4])")) "filter")
  (assert (= [1 3] (ct-eval ctx "(remove even? [1 2 3 4])")) "remove")
  (assert (= 6 (ct-eval ctx "(reduce + [1 2 3])")) "reduce")
  (assert (= [1 2 3] (ct-eval ctx "(take 3 [1 2 3 4 5])")) "take")
  (assert (= [4 5] (ct-eval ctx "(drop 3 [1 2 3 4 5])")) "drop")

  (print "  special forms...")
  (assert (= 30 (ct-eval ctx "(let [x 10 y 20] (+ x y))")) "let")
  (assert (= :a (ct-eval ctx "(if true :a :b)")) "if true")
  (assert (= :b (ct-eval ctx "(if false :a :b)")) "if false")
  (assert (= 3 (ct-eval ctx "(loop [x 0] (if (< x 3) (recur (inc x)) x))")) "loop")
  (assert (= "caught" (ct-eval ctx "(try (throw 42) (catch Exception e \"caught\"))")) "try")
  (assert (= 42 (ct-eval ctx "'42")) "quote literal")

  (print "  macros...")
  (ct-eval ctx "(defn add [a b] (+ a b))")
  (assert (= 7 (ct-eval ctx "(add 3 4)")) "defn")
  (assert (= 42 (ct-eval ctx "(when true 42)")) "when true")
  (assert (= 3 (ct-eval ctx "(and 1 2 3)")) "and")
  (assert (= 1 (ct-eval ctx "(or 1 2 3)")) "or")
  (assert (= 49 (ct-eval ctx "((fn [x] (* x x)) 7)")) "fn macro")
  (assert (= 2 (ct-eval ctx "(if-let [x 1] (inc x) 0)")) "if-let")

  (print "  complex...")
  (assert (= 6 (ct-eval ctx "(let [f (fn [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc i)) acc)))] (f 4))")) "nested")
  (assert (= 15 (ct-eval ctx "(reduce + (map inc [0 1 2 3 4]))")) "reduce+map")

  # Phase 1 wiring: compiled defns persist across forms (the per-context Janet
  # env) and recurse correctly (named-fn self-reference).
  (print "  cross-form defns + recursion...")
  (eval-string ctx "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
  (assert (= 832040 (ct-eval ctx "(fib 30)")) "recursive fib across forms")
  (eval-string ctx "(defn sq [x] (* x x))")
  (eval-string ctx "(defn sum-sq [a b] (+ (sq a) (sq b)))")
  (assert (= 25 (ct-eval ctx "(sum-sq 3 4)")) "defn calling earlier defn")
  (eval-string ctx "(def base 100)")
  (assert (= 142 (ct-eval ctx "(+ base 42)")) "compiled def referenced later")

  # Phase 2: native ops are emitted directly (fast), but IFn values in call
  # position (keyword/map/set) still dispatch via the runtime.
  (print "  native ops + IFn dispatch...")
  (assert (= 10 (ct-eval ctx "(+ 1 2 3 4)")) "n-ary +")
  (assert (= true (ct-eval ctx "(< 1 2 3)")) "n-ary <")
  (assert (= 1 (ct-eval ctx "(:a {:a 1})")) "keyword as fn")
  (assert (= 1 (ct-eval ctx "({:a 1} :a)")) "map as fn")
  (assert (= 2 (ct-eval ctx "(#{1 2 3} 2)")) "set as fn")
  (assert (= true (ct-eval ctx "(= [1 2] [1 2])")) "= is value equality, not core-= bypass")

  # Phase 2: hybrid fallback. Forms the compiler can't compile (destructuring,
  # multi-arity, named fns) interpret instead of erroring or miscompiling. The
  # result is the same — compilation is a transparent speedup.
  (print "  hybrid fallback (destructuring / multi-arity)...")
  (assert (= 3 (ct-eval ctx "(let [[a b] [1 2]] (+ a b))")) "vector destructuring let")
  (assert (= 6 (ct-eval ctx "(let [{:keys [x y z]} {:x 1 :y 2 :z 3}] (+ x y z))")) "map destructuring let")
  (assert (= 3 (ct-eval ctx "((fn [[a b]] (+ a b)) [1 2])")) "destructuring fn param")
  (assert (= 5 (ct-eval ctx "(let [[a & more] [1 2 3 4 5]] (+ a (count more)))")) "rest destructuring")
  (ct-eval ctx "(defn arity ([a] a) ([a b] (+ a b)))")
  (assert (= 5 (ct-eval ctx "(arity 5)")) "multi-arity 1")
  (assert (= 7 (ct-eval ctx "(arity 3 4)")) "multi-arity 2")
  (assert (= 10 (ct-eval ctx "((fn self [n] (if (zero? n) 0 (+ n (self (dec n))))) 4)")) "named fn recursion")
  (assert (= 6 (ct-eval ctx "(loop [[x & xs] [1 2 3] acc 0] (if x (recur xs (+ acc x)) acc))")) "destructuring loop binding")
  # A runtime error in compiled code must propagate, not silently fall back to a
  # second (interpreted) evaluation.
  (assert (= :threw (try (do (ct-eval ctx "(inc nil)") :no-throw) ([_] :threw)))
          "runtime error in compiled code propagates"))

# Context isolation: a def in one compiled context is invisible in another. With
# var-indirection each context has its own var cells, so b's `secret` is a
# distinct, unbound var (nil) rather than a's 7.
(let [a (init {:compile? true}) b (init {:compile? true})]
  (eval-string a "(def secret 7)")
  (assert (= 7 (ct-eval a "secret")) "def visible in its own ctx")
  (assert (nil? (ct-eval b "secret")) "def isolated to its ctx"))

# Redefinition is visible to already-compiled callers (var-indirection).
(let [c (init {:compile? true})]
  (eval-string c "(defn g [] 1)")
  (eval-string c "(defn calls-g [] (g))")
  (eval-string c "(defn g [] 2)")
  (assert (= 2 (ct-eval c "(calls-g)")) "compiled caller sees redefined global"))

(print "\nAll Phase 6 tests passed!")
