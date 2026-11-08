;; Compiling a namespace must cost time LINEAR in its source, and a quoted form
;; must not cost dramatically more than the construction it is.
;;
;; Nothing measured either until v0.8.6, and both were broken at once.
;;
;; SHAPE. Chez's compile is quadratic in the size of one lexical scope, and jolt
;; hands it a def's whole constant pool in a few flat let layers (it was one let*,
;; one scope per binding, until pool-layers). That residue is jolt-wt6v: this arm
;; measures ~1.2 where linear is 1.0, and it is real rather than noise. The ceiling is a guard against gross growth, not a
;; claim that the complexity is linear; poolshape pins the pool's shape exactly.
;;
;; Keying the pool by form identity (what Compiler.registerConstant does with its
;; IdentityHashMap) cut the CONSTANT and not the class: clojure.test/is names the
;; tested form five times, splicing the SAME object into each, and the pool kept a
;; separate construction per mention, so one deftest holding 800 (is …) emitted
;; 6410 bindings of which 812 were distinct. Halving the pool quarters a quadratic
;; term, which is most of malli.core-test going 154s -> 16.9s against the JVM's
;; 1.3s — but this ratio reads the same before and after, which is exactly why the
;; overhead half below exists.
;;
;; OVERHEAD. A per-quoted-form cost regression is invisible to the shape half —
;; it is linear, just linear and slow — and that is exactly what shipped once:
;; embed-plan probed the inspector inside a guard for every quoted form, and
;; sa-procedure-info raises on anything that is not a procedure, so an ordinary
;; quoted list paid for an exception. Compiling 500 forms holding five quoted
;; forms each went 0.61s to 10.41s and `make test` stayed green. Measured on that
;; binary this ratio is 35.4; healthy it is 1.15.
;;
;; Both assertions are ratios taken inside ONE process, microseconds apart, so
;; neither depends on how fast the machine is and neither can drift against a
;; recorded constant. (Gate timing lesson: an absolute ms ceiling and a cross-run
;; ratio both flake on CI; a property measured within one run does not.)

(ns compile-scaling-test
  (:require [clojure.string :as str]
            [clojure.test]))

(defn- body
  "A fn literal whose body is n independent statements built by f."
  [n f]
  (str "(clojure.core/fn [] " (str/join " " (map f (range n))) ")"))

(defn- compile! [src] (load-string src))

;; --- timing ---------------------------------------------------------------------
;; The harness of read_scaling_test.clj (and test/complexity_test.clj before it).
;; Both arms are sampled in the same rounds, one sample of each per round, the
;; order swapped every round, so a busy stretch lands on both instead of on one
;; arm's whole best-of block. Each arm is judged on its FASTEST sample over all
;; rounds: interference only ever makes a compile slower. Each sample starts from
;; a collected heap, and is a batch sized to clear 1ms on the nanosecond clock.
;; Where one arm does k times the other's work, the smaller arm's batch is k
;; times as long, so the two take the same wall time when the cost is linear and
;; a burst is as likely to land on either.
;;
;; batch-size runs each arm before anything is timed, which is also the warm-up:
;; a fresh process's first compiles pay one-time costs, and the arm timed first
;; took all of them — cold, the shape row read 4.8 or 5.2 on the same binary
;; depending on the run.
(def ^:private rounds 5)
;; ...and at least min-rounds, after which a measurement that has spent
;; budget-ns sampling stops. A linear arm never comes near the budget; a
;; regressed one is seconds a sample, and it is regressed in every sample, so
;; three rounds of it decide as surely as seven and the gate fails in seconds
;; rather than minutes.
(def ^:private min-rounds 3)
(def ^:private budget-ns 8e9)

;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- run-ns [f b]
  (let [t (cpu-ns)]
    (dotimes [_ b] (f))
    (- (cpu-ns) t)))

(defn- batch-size [f]
  (loop [b 1]
    (if (or (>= (run-ns f b) 1000000) (>= b 4096)) b (recur (* 2 b)))))

(defn- paired
  "Cost of f4 over cost of f1, each arm its fastest sample over the rounds. f1
  does 1/k of f4's work, so its batch is k times f4's."
  [f1 f4 k]
  (f1)
  (let [b4 (batch-size f4) b1 (* k b4)
        one (fn [f b] (System/gc) (/ (double (run-ns f b)) b))]
    (loop [i 0 t1 ##Inf t4 ##Inf spent 0.0]
      (if (or (= i rounds) (and (>= i min-rounds) (> spent budget-ns)))
        (/ t4 (max 1.0 t1))
        (let [[x y] (if (even? i)
                      (let [x (one f1 b1)] [x (one f4 b4)])
                      (let [y (one f4 b4)] [(one f1 b1) y]))]
          (recur (inc i) (min t1 x) (min t4 y) (+ spent (* x b1) (* y b4))))))))

;; --- shape -----------------------------------------------------------------
;; The same statements compiled as one fn body against the same statements split
;; across `factor` fn bodies of n1 each: the same source, the same total work and
;; allocation, so linear reads 1.0 and quadratic in one scope reads `factor`.
;;
;; It used to compare n1 statements against factor*n1 (linear 4.0, quadratic 16,
;; ceiling 7.0). Its 4x arm is the bigger, memory-heavier compile, and on a busy CI
;; runner that arm slowed more than the small one even in CPU time: readings ran
;; 4.5-10.3 for one binary and failed #1543 at 10.28 then 8.48. Comparing equal
;; work cancels most of that: under 40 spinners locally it read 1.18-1.35. Not all
;; of it, though. The one-body arm is still the bigger single compile, and its
;; first CI run read 2.42 then 1.63 on a healthy tree.
;;
;; Both arms sit in the same regime, and small: CI runs the suite as
;; `make -j$(nproc) test`, so a gate that hogs a core for seconds starves the
;; other timing gates beside it. 4 x 50 statements against 200 measures ~555ms an arm.
(def ^:private n1 50)
(def ^:private factor 4)
;; It reads ~1.2 at this sizing, which is jolt-wt6v: Chez's compile is quadratic in
;; the size of one lexical scope, and a def's constant pool lands in one. The
;; regression that emitted a bare top-level fn's pool as one let* read 1.55-1.72
;; here, inside CI's noise, so no timing ceiling can tell it from a healthy run;
;; poolshape (test/chez/pool-shape-test.ss) pins that one by the emitted nesting
;; instead. This ceiling only catches gross growth: compile time quadratic in one
;; body reads `factor`, 4.0.
(def ^:private max-shape 3.0)

(defn- is-forms [n] (body n (fn [i] (str "(clojure.test/is (= " i " " i "))"))))

(defn- shape-ratio []
  (let [split (str "(do " (apply str (repeat factor (is-forms n1))) ")")
        one   (is-forms (* n1 factor))]
    (paired #(compile! split) #(compile! one) 1)))

;; --- per-quoted-form overhead ----------------------------------------------
;; Same statement count, same shape, one arm quoting and one constructing. A
;; quoted form IS a construction, so the honest ratio is near 1.
(def ^:private n-quoted 300)
;; Measured 1.15 here and 1.17 before the pool change; 35.4 on the binary that
;; regressed it. 4.0 leaves 3.5x headroom and is nowhere near the failure.
(def ^:private max-quoted 4.0)

(defn- quoted-ratio []
  (let [q (body n-quoted (fn [i] (str "(quote (= " i " " i "))")))
        p (body n-quoted (fn [i] (str "(clojure.core/list " i " " i ")")))]
    (paired #(compile! p) #(compile! q) 1)))

;; A ratio over the ceiling is measured once more before it counts, which costs
;; no POWER: a real regression is over it on both measurements, while a burst
;; long enough to spoil every round of one rarely spoils the next. There is no
;; fail-on-sight threshold: one reading is never enough to fail on.
(defn- check [label f max-ratio]
  (let [r (f)
        r (if (<= r max-ratio)
            r
            (do (println (format "  %-22s %.2f  over %.1f, measuring again" label r max-ratio))
                (f)))]
    (if (<= r max-ratio)
      (do (println (format "  %-22s %.2f  (ceiling %.1f)" label r max-ratio)) true)
      (do (println (format "  %-22s %.2f  FAIL — over %.1f on both measurements" label r max-ratio))
          false))))

(defn -main [& _]
  (println "compile-scaling:")
  (let [ok (reduce (fn [acc [label f mx]] (and (check label f mx) acc))
                   true
                   [["shape 1 body/4" shape-ratio max-shape]
                    ["quoted/plain" quoted-ratio max-quoted]])]
    (println (if ok "compile-scaling: passed" "compile-scaling: FAILED"))
    (System/exit (if ok 0 1))))

(-main)
