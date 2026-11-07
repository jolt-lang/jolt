;; (into vec vec) and (subvec v s e) must cost time LOGARITHMIC in the vector
;; size, not linear: both are backed by the RRB tree (pvec-catvec / pvec-slice
;; in host/chez/collections.ss), and this guards that the CORE FNS still reach
;; that backing — the raw ops have their own gate (test/chez/rrb-scaling-test.ss),
;; which cannot notice core falling back to an element-by-element rebuild.
;;
;; What this asserts is the SHAPE, measured in one run: quadrupling the vectors
;; must not quadruple the per-call cost. The structural op holds the ratio near
;; 1; a linear rebuild lands near 4. Nothing here is an absolute time, so it
;; does not care how fast the machine is — the two numbers come from the same
;; process, microseconds apart, and only their ratio is judged.
;;
;; The behavioural half — that the results are element- and metadata-correct —
;; is in unit/corpus rows and test/chez/rrb-property-test.ss. This file is only
;; about cost.

(ns vec-scaling-test)

(defn- vec-of [n] (into [] (range n)))

;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- timed [b f]
  (let [t (cpu-ns)]
    (dotimes [_ b] (f))
    (- (cpu-ns) t)))

;; Timed the way test/complexity_test.clj times its rows. A fixed rep count
;; cannot serve both states: 15000 reps put the fixed arms at ~18ms, and put a
;; regressed linear into at 20s and 84s per sample, so the regression this gate
;; exists for took ~10 minutes to report. Instead each arm runs in batches
;; sized to clear 1ms, its cost is its fastest batch (interference only ever
;; slows one), the arms alternate order over 5 rounds that each start from a
;; full collection, and a batch is nanosecond-timed.
(defn- batch-size [f]
  (loop [b 1]
    (if (or (>= (timed b f) 1000000) (>= b 1048576)) b (recur (* 2 b)))))

(defn- per-call-ns
  "ns per call of the fastest b-call batch within a 20ms budget, at least five."
  [b f]
  (loop [spent 0 best Long/MAX_VALUE k 0]
    (if (and (>= spent 20000000) (>= k 5))
      (/ (double best) b)
      (let [t (timed b f)]
        (recur (+ spent t) (min best t) (inc k))))))

(defn- best-pair [f1 f4]
  (f1) (f4)
  (let [b1 (batch-size f1) b4 (batch-size f4)]
    (reduce (fn [[x1 x4] i]
              (System/gc)
              (if (even? i)
                (let [y1 (per-call-ns b1 f1) y4 (per-call-ns b4 f4)] [(min x1 y1) (min x4 y4)])
                (let [y4 (per-call-ns b4 f4) y1 (per-call-ns b1 f1)] [(min x1 y1) (min x4 y4)])))
            [Double/MAX_VALUE Double/MAX_VALUE]
            (range 5))))

(def ^:private n1 50000)
(def ^:private factor 4)
;; Logarithmic measures ~1 and a linear rebuild ~4, so the line goes between
;; them, with room for a loaded machine.
(def ^:private max-ratio 2.5)

(defn- report [label [t1 t4] note]
  (let [ratio (/ t4 (max 1.0 t1))]
    (println (format "vec-scaling %s: %.0fns vs %.0fns per call (x%d size) ratio %.2f (ceiling %.1f)%s"
                     label t1 t4 factor ratio max-ratio note))
    ratio))

;; A miss is measured once more before it counts: a linear rebuild reads ~4
;; on both measurements, a burst rarely spoils two in a row.
(defn- judge [label f1 f4]
  (let [m (best-pair f1 f4)
        ratio (if (<= (/ (second m) (max 1.0 (first m))) max-ratio)
                (report label m "")
                (do (report label m " — over the ceiling, measuring again")
                    (report label (best-pair f1 f4) "")))]
    (when (> ratio max-ratio)
      (println (str "FAIL vec-scaling: " label " scales with the vector size — "
                    "core is rebuilding element by element instead of using the "
                    "RRB op (jolt-into's catvec path in host/chez/seq.ss, or "
                    "subvec's jolt.host/slice call in clojure/core/00-kernel.clj)."))
      (System/exit 1))))

(defn -main [& _]
  (let [a1 (vec-of n1) b1 (vec-of n1)
        a4 (vec-of (* factor n1)) b4 (vec-of (* factor n1))]
    ;; sanity before cost: the fast paths must produce the right values
    (when-not (and (= (count (into a1 b1)) (* 2 n1))
                   (= (nth (into a1 b1) n1) 0)
                   (= (subvec a1 10 12) [10 11])
                   (= (count (subvec a4 0 (count a4))) (* factor n1)))
      (println "FAIL vec-scaling: wrong values before any timing — fix that first")
      (System/exit 1))
    (into a1 b1) (into a4 b4)          ; warm both sizes
    (judge "into vec+vec" #(into a1 b1) #(into a4 b4))
    (let [s1 (quot n1 3) e1 (* 2 s1)
          s4 (quot (* factor n1) 3) e4 (* 2 s4)]
      (subvec a1 s1 e1) (subvec a4 s4 e4)
      (judge "subvec" #(subvec a1 s1 e1) #(subvec a4 s4 e4)))
    (println "vec-scaling: passed")))

(-main)
