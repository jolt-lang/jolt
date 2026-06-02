# Jolt Compiler Tests — Phase 2
# Tests for source-to-source Clojure→Janet compilation.
# Core ops: const, do, if, def, fn, let, invoke
# Phase 2 adds: symbol classification with binding awareness

(use ../src/jolt/compiler)
(use ../src/jolt/reader)

(defn compile-str [s]
  (let [form (parse-string s)]
    (compile-form form)))

# ============================================================
# 1. Literals (const)
# ============================================================
(print "1: literal constants...")
(assert (= "42" (compile-str "42")) "integer")
(assert (= "nil" (compile-str "nil")) "nil")
(assert (= "true" (compile-str "true")) "true")
(assert (= "false" (compile-str "false")) "false")
(assert (= "\"hello\"" (compile-str "\"hello\"")) "string")
(assert (= ":foo" (compile-str ":foo")) "keyword")
(print "  passed")

# ============================================================
# 2. do
# ============================================================
(print "2: do...")
(assert (= "(do 1 2)" (compile-str "(do 1 2)")) "do two exprs")
(assert (= "(do 42)" (compile-str "(do 42)")) "do single expr")
(assert (= "(do (core-inc 1) (core-inc 2))" (compile-str "(do (inc 1) (inc 2))")) "do with fn calls")
(print "  passed")

# ============================================================
# 3. if
# ============================================================
(print "3: if...")
(assert (= "(if true 1 2)" (compile-str "(if true 1 2)")) "if three-arg")
(assert (= "(if false 1 nil)" (compile-str "(if false 1)")) "if two-arg")
(print "  passed")

# ============================================================
# 4. def
# ============================================================
(print "4: def...")
(assert (= "(def x 42)" (compile-str "(def x 42)")) "def constant")
(assert (= "(def f (fn [x] (core-inc x)))" (compile-str "(def f (fn* [x] (inc x)))")) "def with fn")
(print "  passed")

# ============================================================
# 5. fn
# ============================================================
(print "5: fn...")
(assert (= "(fn [x] (core-inc x))" (compile-str "(fn* [x] (inc x))")) "fn single arity")
(assert (= "(fn [] 42)" (compile-str "(fn* [] 42)")) "fn no args")
(assert (= "(fn [x] (do (core-print x) (core-inc x)))"
           (compile-str "(fn* [x] (print x) (inc x))")) "fn multi-expr body")
(print "  passed")

# ============================================================
# 6. let
# ============================================================
(print "6: let...")
(assert (= "(let [x 1] (core-inc x))" (compile-str "(let* [x 1] (inc x))")) "let single binding")
(assert (= "(let [x 1 y 2] (core-+ x y))" (compile-str "(let* [x 1 y 2] (+ x y))")) "let two bindings")
(assert (= "(let [x (core-inc 1)] (core-inc x))" (compile-str "(let* [x (inc 1)] (inc x))")) "let with fn in binding")
(print "  passed")

# ============================================================
# 7. invoke (function calls)
# ============================================================
(print "7: invoke...")
(assert (= "(core-inc 1)" (compile-str "(inc 1)")) "inc call")
(assert (= "(core-+ 1 2)" (compile-str "(+ 1 2)")) "+ call")
(assert (= "(core-+ (core-inc 1) 2)" (compile-str "(+ (inc 1) 2)")) "nested calls")
(assert (= "(core-map core-inc (core-vec 1 2 3))"
           (compile-str "(map inc (vec 1 2 3))")) "multi-arg call")
(print "  passed")

# ============================================================
# 8. Local symbol classification (Phase 2)
# ============================================================
(print "8: local classification...")
# Shadowing: local inc should NOT be rewritten to core-inc
(assert (= "(let [inc 5] (inc inc))"
           (compile-str "(let* [inc 5] (inc inc))")) "local shadows core fn")
# fn params are locals, not core symbols
(assert (= "(fn [map] (core-vec map))"
           (compile-str "(fn* [map] (vec map))")) "fn param shadows core map")
# nested let with shadowing
(assert (= "(let [x 1] (let [inc x] (inc x)))"
           (compile-str "(let* [x 1] (let* [inc x] (inc x)))")) "nested let local")
(print "  passed")

(print "\nAll compiler Phase 2 tests passed!")

# ============================================================
# 9. Compile-and-eval round-trip (Phase 3)
# ============================================================
(print "9: compile-and-eval...")
(use ../src/jolt/core)  # need core fns in scope for eval

(defn compile-eval-str [s]
  (let [form (parse-string s)]
    (compile-and-eval form nil)))

(assert (= 42 (compile-eval-str "42")) "eval literal")
(assert (= 2 (compile-eval-str "(inc 1)")) "eval inc")
(assert (= 3 (compile-eval-str "(+ 1 2)")) "eval +")
(assert (= 6 (compile-eval-str "(+ (inc 1) (inc 3))")) "eval nested")
(assert (= 2 (compile-eval-str "(do 1 2)")) "eval do")
(assert (= 1 (compile-eval-str "(if true 1 2)")) "eval if true")
(assert (= 2 (compile-eval-str "(if false 1 2)")) "eval if false")
(assert (= 2 (compile-eval-str "(let* [x 1] (inc x))")) "eval let")
(let [f (compile-eval-str "(fn* [x] (inc x))")]
  (assert (function? f) "eval fn returns fn")
  (assert (= 6 (f 5)) "eval fn works"))
(print "  passed")

# ============================================================
# 10. Compile flag in context (Phase 3)
# ============================================================
(print "10: compile flag...")
(use ../src/jolt/api)

# Without compile flag
(let [ctx (init)]
  (assert (= 2 (eval-string ctx "(inc 1)")) "no-compile flag: inc works"))

# With compile flag: pure expressions use compile-and-eval
(let [ctx (init {:compile? true})]
  (assert (= 2 (eval-string ctx "(inc 1)")) "compile flag: inc works")
  (assert (= 3 (eval-string ctx "(+ 1 2)")) "compile flag: + works")
  (assert (= 6 (eval-string ctx "(+ (inc 1) (inc 3))")) "compile flag: nested works"))

# With compile flag: stateful forms fall back to interpreter
(let [ctx (init {:compile? true})]
  (eval-string ctx "(def foo 99)")
  (assert (= 99 (eval-string ctx "foo")) "compile flag: def works"))

(print "  passed")

(print "\nAll compiler Phase 3 tests passed!")

# ============================================================
# 11. Macro expansion (Phase 4)
# ============================================================
(print "11: macro expansion...")
(use ../src/jolt/api)

(let [ctx (init {:compile? true})]
  # defn expands via compiler, produces Janet def
  (eval-string ctx "(defn square [n] (* n n))")
  (assert (= 25 (eval-string ctx "(square 5)")) "defn via compiler")

  # when macro
  (assert (= 42 (eval-string ctx "(when true 42)")) "when true")
  (assert (= nil (eval-string ctx "(when false 42)")) "when false")

  # let macro
  (assert (= 30 (eval-string ctx "(let [x 10 y 20] (+ x y))")) "let macro")

  # fn macro
  (assert (= 49 (eval-string ctx "((fn [x] (* x x)) 7)")) "fn macro")

  # and/or
  (assert (= 3 (eval-string ctx "(and 1 2 3)")) "and")
  (assert (= 99 (eval-string ctx "(or nil false 99)")) "or"))

(print "  passed")

(print "\nAll compiler Phase 4 tests passed!")

# ============================================================
# 12. throw, try, loop*/recur (Phase 5)
# ============================================================
(print "12: throw/try/loop...")
(use ../src/jolt/api)

(let [ctx (init {:compile? true})]
  # throw/catch via compiler
  (assert (= "caught"
             (eval-string ctx "(try (throw 42) (catch Exception e \"caught\"))"))
          "try/catch")

  # try without catch returns body
  (assert (= 1 (eval-string ctx "(try 1 (catch Exception e 2))")) "try no throw")

  # throw in nested context
  (assert (= "ok"
             (eval-string ctx "(try (do (throw 99) 1) (catch Exception e \"ok\"))"))
          "throw in do")

  # loop*/recur
  (assert (= 3 (eval-string ctx "(loop* [x 0] (if (< x 3) (recur (inc x)) x))"))
          "loop count up")
  (assert (= 3
             (eval-string ctx "(loop* [i 0 acc 0] (if (< i 3) (recur (inc i) (+ acc i)) acc))"))
          "loop with acc"))

(print "  passed")

(print "\nAll compiler Phase 5 tests passed!")

# ============================================================
# 13. defn/def integration (Phase 0 fix)
# ============================================================
(print "13: defn/def integration...")
(use ../src/jolt/api)

(let [ctx (init {:compile? true})]
  # defn produces a resolvable var
  (eval-string ctx "(defn identity-fn [x] x)")
  (assert (= 1 (eval-string ctx "(identity-fn 1)")) "defn works")
  (let [f (eval-string ctx "identity-fn")]
    (assert (function? f) "bare defn symbol returns fn"))

  # def produces a resolvable var
  (eval-string ctx "(def answer 42)")
  (assert (= 42 (eval-string ctx "answer")) "def bare symbol")
  (assert (= 43 (eval-string ctx "(inc answer)")) "def in call"))

(print "  passed")

(print "\nAll compiler tests passed!")
