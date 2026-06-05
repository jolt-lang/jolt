(use ../../src/jolt/reader)
(use ../../src/jolt/types)
(use ../../src/jolt/evaluator)

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

# ============================================================
# 1. syntax-quote — literals pass through
# ============================================================
(print "1: syntax-quote literals...")
(assert (= 42 (eval-str "`42")) "syntax-quote number")
(assert (= "hello" (eval-str "`\"hello\"")) "syntax-quote string")
(assert (= :foo (eval-str "`:foo")) "syntax-quote keyword")
(assert (= nil (eval-str "`nil")) "syntax-quote nil")
(assert (= true (eval-str "`true")) "syntax-quote true")
(assert (= false (eval-str "`false")) "syntax-quote false")
(print "  passed")

# ============================================================
# 2. syntax-quote — qualify symbols
# ============================================================
(print "2: syntax-quote qualifies symbols...")
# In the 'user namespace, `x → user/x
(let [form (eval-str "`x")]
  (assert (deep= {:jolt/type :symbol :ns "user" :name "x"} form)
          "qualifies bare symbol to current ns"))

# Already qualified symbols stay as-is
(let [form (eval-str "`foo/bar")]
  (assert (deep= {:jolt/type :symbol :ns "foo" :name "bar"} form)
          "qualified symbol unchanged"))
(print "  passed")

# ============================================================
# 3. syntax-quote — lists: qualify symbols, literal items as-is
# ============================================================
(print "3: syntax-quote lists...")
# `(+ 1 2) → (user/+ 1 2) — but 1 and 2 are numbers, stay as-is
(let [form (eval-str "`(+ 1 2)")]
  (assert (array? form) "syntax-quote list is array")
  (assert (= 3 (length form)) "has 3 elements")
  (assert (deep= {:jolt/type :symbol :ns "user" :name "+"} (in form 0))
          "operator qualified")
  (assert (= 1 (in form 1)) "number stays")
  (assert (= 2 (in form 2)) "number stays"))
(print "  passed")

# ============================================================
# 4. unquote inside syntax-quote
# ============================================================
(print "4: unquote...")
# `~x inside (let* [x 10] ...) → 10
(let [form (eval-str "(let* [x 10] `~x)")]
  (assert (= 10 form) "unquote evaluates"))
# `(~x) produces a list containing x
(let [form2 (eval-str "(let* [x 10] `(~x))")]
  (assert (deep= @[10] form2) "unquote in list"))

# `(+ ~x ~y) with x=1 y=2 → (+ 1 2)
(let [form (eval-str "(let* [x 1 y 2] `(+ ~x ~y))")]
  (assert (array? form) "result is list")
  (assert (= 1 (in form 1)) "first unquoted value")
  (assert (= 2 (in form 2)) "second unquoted value"))
(print "  passed")

# ============================================================
# 5. unquote-splicing inside syntax-quote
# ============================================================
(print "5: unquote-splicing...")
# `[1 2 ~@xs] with xs = (3 4) → [1 2 3 4]
(let [form (eval-str "(let* [xs '(3 4)] `[1 2 ~@xs])")]
  (assert (tuple? form) "result is vector")
  (assert (= 4 (length form)) "spliced items merged")
  (assert (deep= [1 2 3 4] form) "correct items"))

# `(1 ~@xs) with xs = (2 3) → (1 2 3)
(let [form (eval-str "(let* [xs '(2 3)] `(1 ~@xs))")]
  (assert (array? form) "result is list")
  (assert (= 3 (length form)) "spliced into list")
  (assert (deep= @[1 2 3] form) "correct items"))
(print "  passed")

# ============================================================
# 6. Macro function application
# ============================================================
(print "6: macro application...")

# Define a simple macro: (my-when test body) → (if test body nil)
(def ctx (make-ctx))
(def macro-fn-form (parse-string "(fn* [test body] (if test body nil))"))
(def macro-fn (eval-form ctx @{} macro-fn-form))
# intern it in user namespace with string key
(let [ns (ctx-find-ns ctx "user")]
  (ns-intern ns "my-when" macro-fn)
  (put (ns-find ns "my-when") :macro true))

# (my-when true 42) should expand to (if true 42 nil), evaluating to 42
(let [form (parse-string "(my-when true 42)")]
  (assert (= 42 (eval-form ctx @{} form)) "macro application returns correct value"))

# (my-when false 99) should expand to (if false 99 nil), evaluating to nil
(let [form (parse-string "(my-when false 99)")]
  (assert (= nil (eval-form ctx @{} form)) "macro returns nil when false"))
(print "  passed")

# ============================================================
# 7. Nested syntax-quote
# ============================================================
(print "7: nested syntax-quote...")
# ``x → (syntax-quote user/x)
(let [form (eval-str "``x")]
  (assert (array? form) "nested syntax-quote produces list")
  (assert (deep= {:jolt/type :symbol :ns nil :name "syntax-quote"} (in form 0))
          "outer is syntax-quote"))
(print "  passed")

(print "\nAll macro tests passed!")
