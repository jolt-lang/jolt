# Bootstrap fixpoint (jolt-d0r).
#
# Soundness gate for self-hosting: the self-hosted compiler, rebuilt by compiling
# its OWN source through itself (stage2), must behave identically to the compiler
# built by the Janet bootstrap (stage1). We test this BEHAVIORALLY — run a corpus
# of programs through each stage and compare results — rather than by comparing
# emitted code, because emitted forms embed live setter/getter closures and the IR
# carries representation-level gensyms; behavioral parity is the property that
# actually matters and is representation-independent.
#
# stage1 = analyzer as built by the bootstrap.
# stage2 = analyzer rebuilt by compiling jolt.ir + jolt.analyzer through stage1
#          (self-host, the fractal turn) and installing the result over itself.
# stage3 = the same self-rebuild applied again, on top of stage2.
# All three must produce identical results on the corpus.

(use ../../src/jolt/types)
(use ../../src/jolt/api)
(use ../../src/jolt/reader)
(import ../../src/jolt/backend :as be)
(import ../../src/jolt/stdlib_embed :as se)

(defn- forms [src]
  (var s src) (def fs @[])
  (while (> (length (string/trim s)) 0)
    (def p (parse-next s)) (set s (p 1))
    (when (p 0) (array/push fs (p 0))))
  fs)

# Programs exercising the compiled constructs (fn/multi-arity/recur/loop/if/let/
# map+vector literals/closures/higher-order/protocol dispatch). Each is a single
# expression evaluated through the compile pipeline; we compare printed results.
(def corpus
  ["(let [f (fn [x] (* x x))] (map f [1 2 3 4]))"
   "(loop [i 0 acc 0] (if (< i 10) (recur (inc i) (+ acc i)) acc))"
   "((fn fact [n] (if (zero? n) 1 (* n (fact (dec n))))) 6)"
   "(reduce + 0 (filter even? (range 20)))"
   "(let [[a b & r] [1 2 3 4 5]] [a b r])"
   "(mapv (juxt identity inc dec) [10 20])"
   "(frequencies (concat [:a :a] [:b]))"
   "(group-by odd? (range 8))"
   "(get-in {:a {:b {:c 42}}} [:a :b :c])"
   "(first {:x 1})"
   "(into {} (map (fn [k] [k (* k k)]) (range 5)))"
   "((comp inc inc) 10)"
   "(apply max [3 1 4 1 5 9 2 6])"
   "(str (reverse \"hello\") (count [1 2 3]))"
   "(let [m {:a 1}] (assoc m :b (+ (:a m) 1)))"])

(defn- run-corpus [ctx]
  (map (fn [p] (def r (protect (eval-string ctx p)))
               (if (r 0) (string/format "%j" (normalize-pvecs (r 1))) (string "ERR:" (r 1))))
       corpus))

# Rebuild the analyzer through the self-hosted pipeline, in place.
(defn- self-rebuild! [ctx]
  (def saved (ctx-current-ns ctx))
  (each nsn ["jolt.ir" "jolt.analyzer"]
    (ctx-set-current-ns ctx nsn)
    (each f (forms (get se/sources nsn)) (protect (be/compile-and-eval ctx f))))
  (ctx-set-current-ns ctx saved))

(def ctx (init {:compile? true}))
(def r1 (run-corpus ctx))          # stage1 (bootstrap-built)
(self-rebuild! ctx)
(def r2 (run-corpus ctx))          # stage2 (self-built)
(self-rebuild! ctx)
(def r3 (run-corpus ctx))          # stage3 (self-built from stage2)

(var failures 0)
(for i 0 (length corpus)
  (unless (and (= (r1 i) (r2 i)) (= (r2 i) (r3 i)))
    (++ failures)
    (printf "FAIL [%s]\n  stage1=%s\n  stage2=%s\n  stage3=%s" (corpus i) (r1 i) (r2 i) (r3 i)))
  # also guard against everything silently erroring
  (when (string/has-prefix? "ERR:" (r1 i))
    (++ failures) (printf "FAIL [%s] stage1 errored: %s" (corpus i) (r1 i))))

(if (pos? failures)
  (do (printf "bootstrap-fixpoint: %d failure(s)" failures) (os/exit 1))
  (printf "bootstrap-fixpoint: stage1 == stage2 == stage3 on %d programs\n" (length corpus)))
