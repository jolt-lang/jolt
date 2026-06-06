# End-to-end proof of the self-hosting pipeline: a reader form is analyzed by the
# PORTABLE Clojure analyzer (jolt.analyzer, in jolt-core) into host-neutral IR,
# then the Janet back end lowers the IR to a Janet form and evaluates it. No use
# of compiler.janet's analyzer — this is the Clojure-in-Clojure front end.
(import ../../src/jolt/backend :as backend)
(use ../../src/jolt/api)
(use ../../src/jolt/reader)
(use ../../src/jolt/types)

(defn ce [ctx s] (normalize-pvecs (backend/compile-and-eval ctx (parse-string s))))

(print "self-host pipeline (Clojure analyzer -> IR -> Janet)...")
(let [ctx (init)]
  # primitives + control flow
  (assert (= 3 (ce ctx "(+ 1 2)")) "+")
  (assert (= 6 (ce ctx "(* 2 3)")) "*")
  (assert (= :a (ce ctx "(if true :a :b)")) "if true")
  (assert (= :b (ce ctx "(if false :a :b)")) "if false")
  (assert (= 10 (ce ctx "(let [x 4 y 6] (+ x y))")) "let")
  (assert (= 6 (ce ctx "(do 1 2 6)")) "do")

  # literals
  (assert (= [2 3 4] (ce ctx "(map inc [1 2 3])")) "vector literal + core fn")
  (assert (= 1 (ce ctx "(get {:a 1 :b 2} :a)")) "map literal")
  (assert (= 42 (ce ctx "(quote 42)")) "quote literal")

  # def + global reference (name-based var resolution)
  (ce ctx "(def base 100)")
  (assert (= 142 (ce ctx "(+ base 42)")) "def + later ref")

  # fn / defn (defn is a macro -> expand -> def of fn*)
  (ce ctx "(defn add [a b] (+ a b))")
  (assert (= 7 (ce ctx "(add 3 4)")) "defn")
  (assert (= 49 (ce ctx "((fn [x] (* x x)) 7)")) "anon fn")

  # recursion through the var cell (no recur needed)
  (ce ctx "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
  (assert (= 55 (ce ctx "(fib 10)")) "recursive fib via var")

  # multi-arity + variadic
  (ce ctx "(defn arity ([a] a) ([a b] (+ a b)) ([a b & more] (apply + a b more)))")
  (assert (= 5 (ce ctx "(arity 5)")) "multi-arity 1")
  (assert (= 7 (ce ctx "(arity 3 4)")) "multi-arity 2")
  (assert (= 15 (ce ctx "(arity 1 2 3 4 5)")) "multi-arity variadic")

  # loop / recur
  (assert (= 15 (ce ctx "(loop [i 0 acc 0] (if (< i 6) (recur (inc i) (+ acc i)) acc))")) "loop/recur")
  # recur directly in a fixed-arity fn
  (assert (= 15 (ce ctx "((fn [n acc] (if (zero? n) acc (recur (dec n) (+ acc n)))) 5 0)")) "recur in fn")
  # try / catch / finally
  (assert (= "caught" (ce ctx "(try (throw 42) (catch Exception e \"caught\"))")) "try/catch")
  (assert (= 7 (ce ctx "(try 7 (finally 0))")) "try/finally")

  # higher-order + nesting
  (assert (= 15 (ce ctx "(reduce + (map inc [0 1 2 3 4]))")) "reduce+map"))

# eval-toplevel routing: :compile? IS the self-hosted pipeline now — the only
# compile path. Forms the analyzer can't handle (stateful / destructuring) fall
# back to the interpreter, with the same observable results.
(print "self-host via eval-toplevel routing...")
(let [ctx (init {:compile? true})]
  (defn ev [s] (normalize-pvecs (eval-one ctx (parse-string s))))
  (assert (= 3 (ev "(+ 1 2)")) "tl +")
  (ev "(defn sq [x] (* x x))")                 # def via self-host
  (assert (= 81 (ev "(sq 9)")) "tl defn")
  (ev "(defmacro twice [x] (list (quote do) x x))")  # stateful -> interp fallback
  (assert (= 5 (ev "(do (twice 1) 5)")) "tl macro fallback")
  (assert (= [1 2 3] (ev "(let [{:keys [a b c]} {:a 1 :b 2 :c 3}] [a b c])")) "tl destructuring fallback")
  (assert (= 15 (ev "(reduce + (range 6))")) "tl reduce/range")
  # Proof the self-hosted pipeline actually ran: only backend/ensure-analyzer
  # populates jolt.analyzer. An interpret-only ctx never loads it.
  (assert (pos? (length ((ctx-find-ns ctx "jolt.analyzer") :mappings))) "analyzer loaded under :compile?"))
(let [ctx (init {})]
  (eval-one ctx (parse-string "(+ 1 2)"))
  (assert (zero? (length ((ctx-find-ns ctx "jolt.analyzer") :mappings))) "analyzer NOT loaded when interpreting"))

# clojure.core overlay: fns moved from core.janet to jolt-core/clojure/core.clj
# load into clojure.core at init and work the same compiled or interpreted.
(print "clojure.core overlay (Clojure-defined core fns)...")
(each opts [{:compile? true} {}]
  (let [ctx (init opts)]
    (defn ev [s] (normalize-pvecs (eval-one ctx (parse-string s))))
    (assert (= 1 (ev "(ffirst [[1 2] [3 4]])")) "ffirst")
    (assert (= [2] (ev "(nfirst [[1 2] [3 4]])")) "nfirst")
    (assert (= 2 (ev "(fnext [1 2 3])")) "fnext")
    (assert (= [3 4] (ev "(nnext [1 2 3 4])")) "nnext")))

(print "self-host pipeline passed!")
