(use ../src/jolt/types)
(use ../src/jolt/pv)
(use ../src/jolt/plist)
(use ../src/jolt/reader)
(use ../src/jolt/evaluator)
(use ../src/jolt/core)

# Normalize jolt collection results to Janet tuples so Janet-level deep=/= can
# compare against tuple literals regardless of the (pvec/plist) representation.
(defn norm [x]
  (cond
    (pvec? x) (tuple ;(map norm (pv->array x)))
    (plist? x) (tuple ;(map norm (pl->array x)))
    (tuple? x) (tuple ;(map norm x))
    (array? x) (tuple ;(map norm x))
    x))

# Helper: create a fresh bootstrapped context
(defn make-boot-ctx []
  (let [ctx (make-ctx)]
    (init-core! ctx)
    ctx))

# Helper: parse + eval
(defn eval-str [ctx s]
  (let [form (parse-string s)]
    (norm (eval-form ctx @{} form))))

(print "1: predicates...")
(let [ctx (make-boot-ctx)]
  (assert (= true (eval-str ctx "(nil? nil)")) "nil?")
  (assert (= false (eval-str ctx "(nil? 1)")) "nil? false")
  (assert (= true (eval-str ctx "(string? \"hello\")")) "string?")
  (assert (= true (eval-str ctx "(number? 42)")) "number?")
  (assert (= true (eval-str ctx "(fn? inc)")) "fn?")
  (assert (= true (eval-str ctx "(keyword? :foo)")) "keyword?")
  (assert (= false (eval-str ctx "(keyword? 1)")) "keyword? false")
  (assert (= true (eval-str ctx "(zero? 0)")) "zero?")
  (assert (= true (eval-str ctx "(pos? 1)")) "pos?")
  (assert (= true (eval-str ctx "(neg? -1)")) "neg?")
  (assert (= true (eval-str ctx "(even? 2)")) "even?")
  (assert (= true (eval-str ctx "(odd? 1)")) "odd?")
  (assert (= true (eval-str ctx "(empty? [])")) "empty? vector")
  (assert (= false (eval-str ctx "(empty? [1])")) "empty? non-empty"))
(print "  passed")

(print "2: math...")
(let [ctx (make-boot-ctx)]
  (assert (= 0 (eval-str ctx "(+)")) "+ 0 args")
  (assert (= 5 (eval-str ctx "(+ 2 3)")) "+ 2 args")
  (assert (= 10 (eval-str ctx "(+ 1 2 3 4)")) "+ varargs")
  (assert (= -5 (eval-str ctx "(- 5)")) "- unary")
  (assert (= 2 (eval-str ctx "(- 5 3)")) "- binary")
  (assert (= 6 (eval-str ctx "(* 2 3)")) "*")
  (assert (= 1 (eval-str ctx "(*)")) "* 0 args")
  (assert (= 42 (eval-str ctx "(inc 41)")) "inc")
  (assert (= 40 (eval-str ctx "(dec 41)")) "dec")
  (assert (= 4 (eval-str ctx "(max 1 4 2)")) "max"))
(print "  passed")

(print "3: comparison...")
(let [ctx (make-boot-ctx)]
  (assert (= true (eval-str ctx "(= 1 1)")) "= same")
  (assert (= false (eval-str ctx "(= 1 2)")) "= diff")
  (assert (= true (eval-str ctx "(= 1 1 1)")) "= multi same")
  (assert (= false (eval-str ctx "(= 1 2 1)")) "= multi diff")
  (assert (= true (eval-str ctx "(not= 1 2)")) "not="))
(print "  passed")

(print "4: collections...")
(let [ctx (make-boot-ctx)]
  (assert (= 3 (eval-str ctx "(count [1 2 3])")) "count vector")
  (assert (= 1 (eval-str ctx "(first [1 2 3])")) "first")
  (assert (deep= [2 3] (eval-str ctx "(rest [1 2 3])")) "rest")
  (assert (= nil (eval-str ctx "(next [1])")) "next singleton")
  (assert (deep= [1 2 3] (eval-str ctx "(conj [1 2] 3)")) "conj vector")
  (assert (= 1 (eval-str ctx "(get {:a 1} :a)")) "get map")
  (assert (= 2 (eval-str ctx "(get [1 2 3] 1)")) "get vector")
  (assert (= :default (eval-str ctx "(get {:a 1} :b :default)")) "get default")
  (assert (deep= {:a 1 :c 3} (eval-str ctx "(assoc {:a 1} :c 3)")) "assoc")
  (assert (deep= {:a 1} (eval-str ctx "(dissoc {:a 1 :b 2} :b)")) "dissoc"))
(print "  passed")

(print "5: seq ops...")
(let [ctx (make-boot-ctx)]
  (assert (deep= [2 3 4] (eval-str ctx "(map inc [1 2 3])")) "map")
  (assert (deep= [2 4] (eval-str ctx "(filter even? [1 2 3 4])")) "filter")
  (assert (= 6 (eval-str ctx "(reduce + [1 2 3])")) "reduce")
  (assert (= 10 (eval-str ctx "(reduce + 4 [1 2 3])")) "reduce with val")
  (assert (deep= [1 2] (eval-str ctx "(take 2 [1 2 3 4])")) "take")
  (assert (deep= [3 4] (eval-str ctx "(drop 2 [1 2 3 4])")) "drop")
  (assert (deep= [1 2] (eval-str ctx "(take-while (fn* [x] (<= x 2)) [1 2 3 4])")) "take-while"))
(print "  passed")

(print "6: range...")
(let [ctx (make-boot-ctx)]
  (assert (deep= [0 1 2 3 4] (eval-str ctx "(range 5)")) "range end")
  (assert (deep= [2 3 4] (eval-str ctx "(range 2 5)")) "range start end"))
(print "  passed")

(print "7: higher-order...")
(let [ctx (make-boot-ctx)]
  (assert (= 42 (eval-str ctx "(identity 42)")) "identity")
  (assert (= 42 (eval-str ctx "(let* [f (constantly 42)] (f))")) "constantly")
  (assert (= 3 (eval-str ctx "(let* [f (comp inc inc)] (f 1))")) "comp")
  (assert (deep= [2 0] (eval-str ctx "(let* [f (juxt inc dec)] (f 1))")) "juxt"))
(print "  passed")

(print "8: str...")
(let [ctx (make-boot-ctx)]
  (assert (= "hello" (eval-str ctx "(str \"hello\")")) "str")
  (assert (= "hello42" (eval-str ctx "(str \"hello\" 42)")) "str concat"))
(print "  passed")

(print "9: atom...")
(let [ctx (make-boot-ctx)]
  (assert (= 1 (eval-str ctx "(let* [a (atom 1)] (deref a))")) "atom + deref")
  (assert (= 42 (eval-str ctx "(let* [a (atom 1)] (reset! a 42) (deref a))")) "reset!")
  (assert (= 2 (eval-str ctx "(let* [a (atom 1)] (swap! a inc) (deref a))")) "swap!"))
(print "  passed")

(print "\nAll core tests passed!")
