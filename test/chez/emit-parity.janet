# Chez Phase 3 inc 1 (jolt-hg7z) — value-parity gate for the PORTABLE Clojure
# emitter (jolt.backend-scheme) vs the Janet host oracle.
#
# The new emitter is jolt-core Clojure; here it runs interpreted ON THE JANET HOST
# (loaded via bootstrap-load-source) as a drop-in for host/chez/emit.janet. Each
# case is analyzed to IR, emitted to Scheme by the CLOJURE emitter, run on Chez,
# and compared to the same program evaluated by the Janet host (jolt's own oracle).
# This isolates "is the translation correct" from "does it run on Chez" — the
# emitter's logic is validated before it has to execute on Chez itself.
#
#   janet test/chez/emit-parity.janet      (from repo root)
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../src/jolt/evaluator :as evlr)
(import ../../host/chez/driver :as d)
(import ../../host/chez/emit :as emit)
(import ../../src/jolt/types_ctx :as tctx)
(import ../../src/jolt/types_ns :as tns)
(import ../../src/jolt/types_var :as tvar)

(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

# ctx with the analyzer pipeline + late-bind (same as the driver), plus the
# Clojure emitter loaded interpreted so we can call jolt.backend-scheme/emit.
(def ctx (d/make-ctx))
(def bs-src (get (get (ctx :env) :embedded-sources) "jolt.backend-scheme"))
(assert bs-src "jolt.backend-scheme not embedded — check stdlib_embed collect")
(backend/bootstrap-load-source ctx "jolt.backend-scheme" bs-src)
(def emit-clj-var (tns/ns-find (tctx/ctx-find-ns ctx "jolt.backend-scheme") "emit"))
(assert emit-clj-var "jolt.backend-scheme/emit not found after load")
(defn emit-clj [ir] (string ((tvar/var-get emit-clj-var) ir)))

# Janet host oracle, via the real CLI (-e), exactly like run-corpus.janet: take the
# last non-empty stdout line so collection values use jolt's real printer.
(defn cli-oracle [src]
  (def proc (os/spawn ["build/jolt" "-e" src] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (ev/read (proc :err) 0x100000)
  (os/proc-wait proc)
  (def lines (filter (fn [l] (not (empty? l))) (string/split "\n" (string/trim (if out (string out) "")))))
  (if (empty? lines) "" (last lines)))

(defn- parse-all [src]
  (def out @[])
  (var s src)
  (while (> (length (string/trim s)) 0)
    (def parsed (r/parse-next s))
    (set s (in parsed 1))
    (def f (in parsed 0))
    (unless (nil? f) (array/push out f)))
  out)

# Drain a pipe to EOF (a stdout side effect can flush in >1 write).
(defn- drain [pipe]
  (def b @"")
  (var c (ev/read pipe 0x10000))
  (while c (buffer/push b c) (set c (ev/read pipe 0x10000)))
  (string b))

# Compile `src` to a Chez program using the CLOJURE emitter, run it, return
# [code stdout stderr]. Mirrors driver/compile-program + run-on-chez but swaps
# emit/emit -> emit-clj.
(defn run-clj [src]
  (def forms (parse-all src))
  (def n (length forms))
  (def def-scm @[])
  (for i 0 (- n 1)
    (def f (in forms i))
    (array/push def-scm (emit-clj (backend/analyze-form ctx f)))
    (evlr/eval-form ctx @{} f))
  (def final-scm (emit-clj (backend/analyze-form ctx (in forms (- n 1)))))
  (def prog (emit/program def-scm final-scm))
  (def path (string "/tmp/jolt-chez-parity-" (os/getpid) ".ss"))
  (spit path prog)
  (def proc (os/spawn ["chez" "--script" path] :p {:out :pipe :err :pipe}))
  (def out (drain (proc :out)))
  (def err (drain (proc :err)))
  (def code (os/proc-wait proc))
  [code (string/trim out) (string/trim err)])

# A case passes when the Clojure emitter's Chez output equals the Janet oracle.
(defn check [name src]
  (def want (cli-oracle src))
  (def [code out err] (run-clj src))
  (ok name (and (= code 0) (= out want))
      (string "chez=" out " oracle=" want " code=" code " | " err)))

# --- inc 1 subset: const/local/var/if/do/let/loop/recur/invoke/fn/def ----------

(check "(+ 1 2)" "(+ 1 2)")
(check "arith mixed" "(- (* 3 4) (/ 10 2))")
(check "nested let" "(let [a 1 b (+ a 10) c (* b 2)] (- c a))")
(check "let sequential" "(loop [a 1 b (+ a 10)] (+ a b))")
(check "if comparison" "(if (< 3 5) 100 200)")
(check "if =" "(if (= 2 2) :y :n)")
(check "do side-effect ret" "(do 1 2 3)")
(check "fib 30" "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)")
(check "factorial loop" "(defn fact [n] (loop [i n acc 1] (if (< i 2) acc (recur (- i 1) (* acc i))))) (fact 10)")
(check "multi-arity" "(defn g ([x] (g x 10)) ([x y] (+ x y))) (g 5)")
(check "variadic" "(defn s [& xs] (reduce + 0 xs)) (s 1 2 3 4)")
(check "higher-order inc" "(reduce + 0 (map inc (range 5)))")
(check "anon fn invoke" "((fn [x] (* x x)) 7)")
(check "shorthand fn" "(#(+ %1 %2) 3 4)")
(check "truthy local" "(defn t [x] (if x 1 2)) (t false)")
(check "mod rem quot" "(+ (mod 17 5) (rem 17 5) (quot 17 5))")
(check "min max" "(+ (min 3 1 2) (max 3 1 2))")
(check "mandelbrot run(20)"
  (string ``
(defn count-point [cr ci cap]
  (loop [i 0 zr 0.0 zi 0.0]
    (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
      i
      (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))
(defn run [n]
  (let [cap 200 nd (* 1.0 n)]
    (loop [y 0 acc 0]
      (if (< y n)
        (let [ci (- (/ (* 2.0 y) nd) 1.0)
              row (loop [x 0 a 0]
                    (if (< x n)
                      (let [cr (- (/ (* 2.0 x) nd) 1.5)]
                        (recur (inc x) (+ a (count-point cr ci cap))))
                      a))]
          (recur (inc y) (+ acc row)))
        acc))))
`` "\n(run 20)"))

(printf "\n%d/%d ok" (- total fails) total)
(when (> fails 0) (os/exit 1))
