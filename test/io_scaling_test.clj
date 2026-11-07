;; Draining a string reader — by lines or by forms — must cost time LINEAR in
;; the input, not quadratic.
;;
;; The IReader implementations in clojure.core (50-io.clj) held the UNREAD REST
;; of the input in an atom: every read-line re-copied the whole remaining
;; buffer ((subs cur (inc i))), and every read re-parsed and re-copied it
;; through __parse-next, which returned [form rest-of-string]. Draining S chars
;; over L lines/forms cost O(S*L) — (line-seq (with-in-str ...)) and read loops
;; are the standard way tests feed input. The fix keeps a [string offset]
;; cursor (str-find takes a start, __parse-next-from returns [form next-index])
;; so nothing re-copies the tail.
;;
;; Same judgment as read_scaling_test.clj: the SHAPE, in one run, alternated
;; arms, only the in-process 1x-vs-4x ratio judged — linear ~4, the old copy ~16.
;; n1 is sized so a REGRESSED implementation still finishes and fails.

(ns io-scaling-test)

(def ^:private line-text "the quick brown fox jumps over the lazy dog etc")

(defn- lines-src [n] (apply str (repeat n (str line-text "\n"))))

(defn- drain-lines [s]
  (with-in-str s
    (loop [c 0]
      (if (nil? (read-line)) c (recur (inc c))))))

(def ^:private form-text "(def some-name-here [1 2 3 :a :b \"str\"])\n")

(defn- forms-src [n] (apply str (repeat n form-text)))

(defn- drain-forms [s]
  (with-in-str s
    (loop [c 0]
      (if (= :eof (read {:eof :eof} *in*)) c (recur (inc c))))))

;; --- timing ---------------------------------------------------------------------
;; The harness of read_scaling_test.clj (and test/complexity_test.clj before it):
;; one sample of each arm per round, the order swapped every round, each sample
;; from a collected heap, each arm judged on its FASTEST sample over all rounds.
;; Measured as a best-of-3 of one arm and then of the other, the 4x arm — four
;; times as long — was the likelier to overlap a busy stretch, a bias toward a
;; HIGH ratio and not just noise around it: it read 8.22 on a run whose sibling
;; workflow measured 2.74 on the same commit. So the arms also take the same wall
;; time per sample when the drain is linear — the 1x batch drains its input
;; `factor` times per drain of the 4x input — and a burst is as likely to land on
;; either. Batches clear 1ms on the nanosecond clock.
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

;; 2000 items. This was 8000 while each arm was one drain on a millisecond-ish
;; reading, where at 2000 a single scheduler blip was most of the measurement.
;; A sample is a batch sized past 1ms now, so the small size costs no precision,
;; and it keeps a REGRESSED drain cheap: copying the rest of the input per line
;; at 32000 lines took 25s a drain on a loaded machine, at 8000 it is 1.5s.
;; Healthy, the drain sits ~4.0-4.5 against a linear 4.0; the quadratic bug this
;; gates sat ~16.
(def ^:private n1 2000)
(def ^:private factor 4)
(def ^:private max-ratio 8.0)

(defn- report [label [t1 t4 ratio] note]
  (println (format "io-scaling %s: %d items %.2fms, %d items %.2fms, ratio %.2f (linear ~%.1f, quadratic ~%.1f, ceiling %.1f)%s"
                   label n1 t1 (* factor n1) t4 ratio
                   (double factor) (double (* factor factor)) max-ratio note)))

(def ^:private failures (atom 0))

;; A miss is measured once more before it counts. That costs no POWER: a
;; quadratic drain measures ~16 in every round of both measurements, while a
;; burst long enough to spoil all seven rounds of one rarely spoils the next.
;; There is no fail-on-sight threshold: one reading is never enough to fail on,
;; and a clear regression reads high on the re-measure too.
(defn- judge [label f1 f4]
  (let [m (paired f1 f4 factor)
        m (if (<= (nth m 2) max-ratio)
            m
            (do (report label m " — over the ceiling, measuring again")
                (paired f1 f4 factor)))]
    (report label m "")
    (when (> (nth m 2) max-ratio)
      (println (str "FAIL io-scaling: " label " drain scaled worse than linearly in the input. "
                    "The IReader in jolt-core/clojure/core/50-io.clj is re-copying or re-parsing "
                    "the remaining buffer per item instead of advancing a cursor."))
      (swap! failures inc))))

(defn -main [& _]
  (let [ls1 (lines-src n1) ls4 (lines-src (* factor n1))
        fs1 (forms-src n1) fs4 (forms-src (* factor n1))]
    ;; the drains must have read what they claim before their cost is judged;
    ;; also pin the read/read-line interleave (read consumes exactly its form)
    (when-not (and (= (drain-lines ls1) n1) (= (drain-lines ls4) (* factor n1))
                   (= (drain-forms fs1) n1) (= (drain-forms fs4) (* factor n1))
                   (= (with-in-str "(+ 1 2) tail-text\nnext"
                        [(read) (read-line) (read-line)])
                      ['(+ 1 2) " tail-text" "next"]))
      (println "FAIL io-scaling: wrong lines/forms through the reader")
      (System/exit 1))
    (judge "read-line" #(drain-lines ls1) #(drain-lines ls4))
    (judge "read" #(drain-forms fs1) #(drain-forms fs4))
    (if (pos? @failures)
      (System/exit 1)
      (println "io-scaling: passed"))))

(-main)
