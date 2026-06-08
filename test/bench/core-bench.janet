# Performance baseline for the clojure.core migration (jolt-1j0).
#
# Times representative core operations end-to-end (compile path) so a phase that
# moves fns from native Janet to the self-hosted Clojure overlay can be checked
# for regressions. Same programs before/after a phase -> relative delta is the
# migration's perf impact. Run: janet test/bench/core-bench.janet
#
# Each program carries its own internal iteration so the measured work dominates
# parse/compile overhead. Reports the min of N runs (least noisy).

(import ../../src/jolt/api :as api)

(def runs 5)

(def benches
  [[:fib       "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 28)"]
   [:seq-pipe  "(loop [i 0 a 0] (if (< i 300) (recur (inc i) (+ a (reduce + 0 (map inc (filter even? (range 200)))))) a))"]
   [:reduce    "(loop [i 0 a 0] (if (< i 2000) (recur (inc i) (+ a (reduce + 0 (range 500)))) a))"]
   [:into-vec  "(loop [i 0 a 0] (if (< i 1000) (recur (inc i) (+ a (count (into [] (map inc (range 100)))))) a))"]
   [:map-build "(loop [i 0 a 0] (if (< i 2000) (recur (inc i) (+ a (count (reduce (fn [m k] (assoc m k k)) {} (range 50))))) a))"]
   [:map-read  "(let [m (zipmap (range 100) (range 100))] (loop [i 0 a 0] (if (< i 5000) (recur (inc i) (+ a (get m (mod i 100) 0))) a)))"]
   [:str-join  "(loop [i 0 a 0] (if (< i 1000) (recur (inc i) (+ a (count (apply str (map str (range 100)))))) a))"]
   [:hof       "(loop [i 0 a 0] (if (< i 2000) (recur (inc i) (+ a (reduce + 0 (map (comp inc inc) (range 200))))) a))"]])

(defn time-bench [ctx src]
  (var best math/inf)
  (for _ 0 runs
    (def t0 (os/clock))
    (api/load-string ctx src)
    (def dt (* 1000 (- (os/clock) t0)))
    (when (< dt best) (set best dt)))
  best)

(defn main [&]
  (def ctx (api/init {:compile? true}))
  (print "bench (compile mode), min of " runs " runs, ms:")
  (var total 0)
  (each [name src] benches
    (def ms (time-bench ctx src))
    (+= total ms)
    (printf "  %-10s %8.2f ms" name ms))
  (printf "  %-10s %8.2f ms" "TOTAL" total))
