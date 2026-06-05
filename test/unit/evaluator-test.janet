(use ../../src/jolt/evaluator)
(use ../../src/jolt/types)
(use ../../src/jolt/reader)
(use ../../src/jolt/api)

# Helper: create a Jolt symbol
(defn sym [name]
  (let [slash (string/find "/" name)]
    (if slash
      {:jolt/type :symbol :ns (string/slice name 0 slash)
       :name (string/slice name (+ slash 1))}
      {:jolt/type :symbol :ns nil :name name})))

# Helper: parse and eval
(defn eval-str [s]
  (let [ctx (make-ctx)
        form (parse-string s)]
    (eval-form ctx @{} form)))

(print "1: literals...")
(assert (= 42 (eval-str "42")) "integer")
(assert (= "hello" (eval-str "\"hello\"")) "string")
(assert (= true (eval-str "true")) "true")
(assert (= false (eval-str "false")) "false")
(assert (= nil (eval-str "nil")) "nil")
(print "  passed")

(print "2: quote...")
(assert (deep= (sym "x") (eval-str "'x")) "quote returns symbol")
(assert (deep= @[1 2 3] (eval-str "'(1 2 3)")) "quote list")
(print "  passed")

(print "3: do...")
(assert (= 2 (eval-str "(do 1 2)")) "do returns last")
(print "  passed")

(print "4: if...")
(assert (= 1 (eval-str "(if true 1 2)")) "if true")
(assert (= 2 (eval-str "(if false 1 2)")) "if false")
(assert (= :b (eval-str "(if nil :a :b)")) "if nil = false")
(assert (= nil (eval-str "(if false 1)")) "if with no else")
(print "  passed")

(print "5: def...")
(assert (= 42 (eval-str "(do (def x 42) x)")) "def in do")
(print "  passed")

(print "6: fn*...")
(let [f (eval-str "(fn* [x] (inc x))")]
  (assert (function? f) "fn* returns function")
  (assert (= 42 (f 41)) "fn* fn works"))
# nested function
(let [f (eval-str "(fn* [x] (inc (inc x)))")]
  (assert (= 43 (f 41)) "nested inc"))
(print "  passed")

(print "7: let*...")
(assert (= 2 (eval-str "(let* [x 1 y 2] y)")) "let* binds")
(assert (= 3 (eval-str "(let* [x 1] (inc (inc x)))")) "let* with expr")
(print "  passed")

(print "8: loop*/recur...")
(assert (= 5 (eval-str "(loop* [x 0] (if (< x 5) (recur (inc x)) x))"))
        "loop counts up")
(assert (= 10 (eval-str "(loop* [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))"))
        "loop with multiple bindings")
(print "  passed")

(print "9: recur in fn*...")
(let [countdown (eval-str "(fn* [n] (if (< n 1) 0 (recur (dec n))))")]
  (assert (= 0 (countdown 5)) "recur in fn"))
(print "  passed")

(print "10: throw/try/catch/finally...")
# throw + catch
(let [result (eval-str "(try (throw \"boom\") (catch Exception e \"caught\"))")]
  (assert (= "caught" result) "catch catches throw"))

# try with finally — body returns, finally runs
(let [result (eval-str "(try 1 (finally 2))")]
  (assert (= 1 result) "try returns body even with finally"))

# try/catch/finally — catch returns, finally runs  
(let [result (eval-str "(try (throw \"err\") (catch Exception e \"handled\") (finally :cleanup))")]
  (assert (= "handled" result) "catch + finally returns catch value"))
(print "  passed")

(print "11: set!...")
# set! on a var
(assert (= 99 (eval-str "(do (def x 1) (set! x 99) x)")) "set! on var")
# set! re-evaluates
(assert (= 3 (eval-str "(do (def a 1) (def b 2) (set! a (+ a b)) a)")) "set! with expression")
(print "  passed")

(print "12: var...")
# (var x) returns the var itself, not its value
(let [v (eval-str "(do (def x 42) (var x))")]
  (assert (var? v) "(var x) returns a var")
  (assert (= 42 (var-get v)) "var holds value"))
(print "  passed")

(print "13: locking...")
# locking is a no-op in single-threaded Janet — just executes body
(assert (= 42 (eval-str "(locking :lock 42)")) "locking returns body result")
(print "  passed")

(print "14: instance?...")
# instance? checks type
(assert (= true (eval-str "(instance? Number 42)")) "instance? Number matches number")
(assert (= false (eval-str "(instance? Number \"hello\")")) "instance? Number doesn't match string")
(print "  passed")

(print "15: defmulti/defmethod...")
(let [ctx (make-ctx)]
  (eval-form ctx @{} (parse-string "(defmulti my-dispatch (fn* [x] (x :type)))"))
  (eval-form ctx @{} (parse-string "(defmethod my-dispatch :foo [_] :got-foo)"))
  (eval-form ctx @{} (parse-string "(defmethod my-dispatch :bar [_] :got-bar)"))
  (assert (= :got-foo (eval-form ctx @{} (parse-string "(my-dispatch {:type :foo})"))) "defmethod :foo dispatches")
  (assert (= :got-bar (eval-form ctx @{} (parse-string "(my-dispatch {:type :bar})"))) "defmethod :bar dispatches"))
(print "  passed")

(print "16: deftype...")
(let [ctx (make-ctx)
      _ (eval-form ctx @{} (parse-string "(deftype Point [x y])"))
      _ (eval-form ctx @{} (parse-string "(def p (Point. 10 20))"))
      p-val (eval-form ctx @{} (parse-string "p"))
      x-val (eval-form ctx @{} (parse-string "(p :x)"))
      y-val (eval-form ctx @{} (parse-string "(p :y)"))
      result [x-val y-val]]
  (printf "  p-val: %q" p-val)
  (printf "  x-val: %q, y-val: %q" x-val y-val)
  (printf "  result: %q" result)
  (assert (deep= [10 20] result) "deftype creates tagged instances with fields"))
(print "  passed")

(print "17: defmacro...")
# define a macro using defmacro special form
# init loads clojure.core so `list` is available
(let [ctx (init)
      _ (eval-form ctx @{} (parse-string "(defmacro my-when [test body] (list 'if test body nil))"))
      result (eval-form ctx @{} (parse-string "(my-when true 2)"))]
  (assert (= 2 result) "defmacro defines callable macro"))
# verify the var is marked :macro
(let [ctx (make-ctx)
      _ (eval-form ctx @{} (parse-string "(defmacro m [x] (list 'quote x))"))
      v (resolve-var ctx @{} (parse-string "m"))]
  (assert v "macro var exists")
  (assert (v :macro) "macro var has :macro true"))
(print "  passed")

(print "\nAll evaluator tests passed!")
