;; Rendering a collection must cost time LINEAR in the printed length.
;;
;; jolt-str-join / jolt-str-join-comma (host/chez/rt.ss) were right-folds of
;; string-append, so element i's copy cost was the length of the entire
;; remaining suffix — O(n*L) over the whole render, on EVERY collection print:
;; both printers (str-style and pr-readable), record printing, sorted-coll
;; printing. The fix joins through a string output port, one pass. The same
;; right-fold lived in host/gambit/rt-core.ss, and sorted-map rendering
;; (host-table.ss) carried its own accumulator-first string-append.
;;
;; Same judgment as read_scaling_test.clj: the SHAPE, in one run — quadrupling
;; the element count must not quadruple the per-element cost. Linear lands near
;; 4, the old right-fold near 16; the arms alternate and only the in-process
;; ratio is judged. Correctness (exact rendering) is asserted on
;; small values here and pinned broadly by the corpus; this file is about cost.

(ns print-scaling-test)

(defn- render [n]
  (let [v (vec (range n))
        s (pr-str v)]
    ;; the count comes back with the render so verifying costs no extra pass
    (count s)))

;; a record's #ns.Name{...} form joins its entries the same one-pass way; the
;; extension map is unbounded (any non-field key assoc'd on lands there), so a
;; per-entry append to a growing accumulator (jrec-field-pr, records.ss) would
;; be quadratic in the entry count.
(defrecord ExtRec [])

(defn- render-rec [n]
  (let [r (reduce (fn [r i] (assoc r (keyword (str "k" i)) i)) (->ExtRec) (range n))]
    (count (pr-str r))))

;; --- timing ---------------------------------------------------------------------
;; The harness of read_scaling_test.clj (and test/complexity_test.clj before it):
;; one sample of each arm per round, the order swapped every round, so a busy
;; stretch lands on both arms rather than on one arm's whole best-of block; each
;; arm judged on its FASTEST sample over all rounds, since interference only ever
;; makes a run slower; and the two arms' samples take the same wall time when the
;; render is linear (the 1x batch renders `factor` times per 4x render), so a
;; burst is as likely to land on either.
;;
;; Each sample starts from a collected heap, as in hotpath_scaling_test.clj.
;; Without it a collection owed by earlier allocation can land in every run of
;; one arm, which a minimum cannot filter: a CI runner measured the 16000 vector
;; arm at 2x its usual per-element cost on all three attempts (ratio 8.2-8.3)
;; while the same commit read 4.2 on the next run. Both arms of the record row
;; allocate ~n intermediate records (each assoc conses a new one), which is the
;; same exposure.
;;
;; A sample is a batch sized to clear 1ms on the nanosecond clock: the 1x vector
;; arm renders in ~0.7ms, which a millisecond clock quantized to ±33% straight
;; into the ratio's denominator.
(def ^:private rounds 7)
;; ...and at least min-rounds, after which a measurement that has spent
;; budget-ns sampling stops. A linear arm never comes near the budget; a
;; regressed one is seconds a sample, and it is regressed in every sample, so
;; three rounds of it decide as surely as seven and the gate fails in seconds
;; rather than minutes.
(def ^:private min-rounds 3)
(def ^:private budget-ns 4e9)

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
  "[ms-per-call at 1x, at 4x, ratio], each arm its fastest sample over the rounds.
  f1 does 1/k of f4's work, so its batch is k times f4's."
  [f1 f4 k]
  (let [b4 (batch-size f4) b1 (* k b4)
        one (fn [f b] (System/gc) (/ (double (run-ns f b)) b))]
    (loop [i 0 t1 ##Inf t4 ##Inf spent 0.0]
      (if (or (= i rounds) (and (>= i min-rounds) (> spent budget-ns)))
        [(/ t1 1e6) (/ t4 1e6) (/ t4 (max 1.0 t1))]
        (let [[x y] (if (even? i)
                      (let [x (one f1 b1)] [x (one f4 b4)])
                      (let [y (one f4 b4)] [(one f1 b1) y]))]
          (recur (inc i) (min t1 x) (min t4 y) (+ spent (* x b1) (* y b4))))))))

;; n1 sized so the REGRESSED right-fold (~7.5e8 char copies in the 4x arm)
;; still finishes and fails rather than hanging the gate.
;;
;; Per-element cost grows with n across this whole range (record: 1.77us at 4000,
;; 4.10 at 16000, 5.44 at 32000, 5.93 at 64000; vector: 0.61, 0.63, 1.56, 1.84),
;; so there is no flat regime to move onto — 16000 -> 64000 measures 9.05 on the
;; vector arm and would fail outright. These sizes are the best window available;
;; the record ratio measures ~4.6-5.2 against the ceiling of 8.0.
(def ^:private n1 4000)
(def ^:private factor 4)
(def ^:private max-ratio 8.0)

(def ^:private failures (atom 0))

(defn- report [label [t1 t4 ratio] note]
  (println (format "print-scaling: %s%d %.2fms, %d %.2fms, ratio %.2f (linear ~%.1f, quadratic ~%.1f, ceiling %.1f)%s"
                   label n1 t1 (* factor n1) t4 ratio
                   (double factor) (double (* factor factor)) max-ratio note)))

;; A miss is measured once more before it counts. That costs no POWER: a
;; quadratic render measures ~16 in every round of both measurements, while a
;; burst long enough to spoil all seven rounds of one rarely spoils the next.
(defn- judge [label f1 f4 fail-msg]
  (let [m (paired f1 f4 factor)
        m (if (<= (nth m 2) max-ratio)
            m
            (do (report label m " — over the ceiling, measuring again")
                (paired f1 f4 factor)))]
    (report label m "")
    (when (> (nth m 2) max-ratio)
      (println (str "FAIL print-scaling: " fail-msg))
      (swap! failures inc))))

(defn -main [& _]
  ;; exact small renders: separators, map commas, sorted-map arm
  (when-not (and (= (pr-str [1 2 3]) "[1 2 3]")
                 (= (pr-str {:a 1}) "{:a 1}")
                 (= (pr-str (sorted-map :a 1 :b 2)) "{:a 1, :b 2}")
                 (= (pr-str (sorted-set 3 1 2)) "#{1 2 3}")
                 (= (str [1 2 3]) "[1 2 3]"))
    (println "FAIL print-scaling: wrong rendering on small values")
    (System/exit 1))
  (let [c1 (render n1)
        c4 (render (* factor n1))]
    ;; a ratio over the wrong output would be meaningless
    (when-not (and (> c1 (* 4 n1)) (> c4 (* 4 factor n1)))
      (println (str "FAIL print-scaling: rendered lengths look wrong — " c1 " and " c4))
      (System/exit 1))
    (judge "" #(render n1) #(render (* factor n1))
           (str "rendering scaled worse than linearly in the element count. "
                "jolt-str-join is re-copying the joined suffix per element "
                "(host/chez/rt.ss, host/gambit/rt-core.ss, or sorted-map-render in host-table.ss).")))
  ;; the record arm, judged the same way
  (when-not (= (pr-str (assoc (->ExtRec) :b 2 :a 1)) "#print_scaling_test.ExtRec{:b 2, :a 1}")
    (println (str "FAIL print-scaling: wrong record rendering — " (pr-str (assoc (->ExtRec) :b 2 :a 1))))
    (System/exit 1))
  (judge "record " #(render-rec n1) #(render-rec (* factor n1))
         (str "record rendering scaled worse than linearly in the entry count. "
              "jrec-field-pr is appending each entry to a growing accumulator (host/chez/records.ss)."))
  (if (pos? @failures)
    (System/exit 1)
    (println "print-scaling: passed")))

(-main)
