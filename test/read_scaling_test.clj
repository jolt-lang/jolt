;; Reading a source file form by form off a host reader must cost time LINEAR in
;; the source, not quadratic.
;;
;; It was quadratic until v0.7.7: every (read rdr) drained the whole remaining
;; reader, parsed one form, and pushed the tail back, so a caller walking a file
;; paid a full copy of the rest of it per FORM. Reading clojure/core.clj (263KB,
;; 697 forms) took 37s against the JVM's 0.06s. Nothing noticed for a long time
;; because the loop was unreachable — (read opts stream) did not exist, so it
;; threw on the first call instead of running.
;;
;; What this asserts is the SHAPE, measured in one run: quadrupling the input
;; must not quadruple the cost per form. A ratio near 4 is linear; a quadratic
;; implementation lands near 16. Nothing here is an absolute time, so it does not
;; care how fast the machine is, and it does not compare against a calibration
;; constant that could drift — the two numbers come from the same process,
;; microseconds apart, and only their ratio is judged. (Gate timing lesson: an
;; absolute ns ceiling and a cross-run ratio both flake on CI; a property
;; measured within one run does not.)
;;
;; The behavioural half of this — that a read advances the reader correctly, that
;; the unconsumed tail survives, that each form carries its own line — is in the
;; corpus under "interop / read over a host reader". This file is only about cost.

(ns read-scaling-test)

;; A list NESTED in a list, not just a flat one. A list's :line/:column is taken
;; through rdr-line-col-at's forward-only cursor, and a list that asked for its
;; position after reading its children asked for an EARLIER index than they had,
;; which sent the cursor back to the start of the string on every enclosing list.
;; A flat form never asks twice, so it read linear while every real source file,
;; all nested lists, read quadratic: clojure/core.clj took 1s where the JVM takes
;; 20ms, and twice core.clj took 4s.
(def ^:private form-text "(defn some-name-here [x] (let [y (inc x)] (str y \"str\" [1 2 :a])))\n")

(defn- source [n] (apply str (repeat n form-text)))

(defn- read-all
  "Read every form off a PushbackReader over s, the way a file walker does."
  [s]
  (let [r (java.io.PushbackReader. (java.io.StringReader. s))]
    (count (take-while #(not= :eof %) (repeatedly #(read {:eof :eof} r))))))

;; --- timing ---------------------------------------------------------------------
;; The two arms ALTERNATE: one sample of each per round, the order swapped every
;; round, so a loaded stretch of the machine lands on both arms instead of on one
;; arm's whole block (best-of-3 of one arm, then best-of-3 of the other, read a
;; burst as a superlinear 4x arm). Each arm is judged on its FASTEST sample over
;; all rounds: interference only ever makes a run slower, so the minimum is the
;; estimate of what the read costs, and a quadratic read is quadratic in every
;; sample. Every sample starts from a collected heap, so a collection owed by the
;; previous sample is not billed to this one. A sample is a batch of reads sized
;; to clear 1ms on the nanosecond clock (one read-all already does at these sizes).
;; This is the harness of test/complexity_test.clj, which explains each choice.
;;
;; And the two arms' samples take the same wall time when the read is linear: the
;; 1x arm's batch reads its source `factor` times for each read of the 4x source
;; (hotpath_scaling_test's judge-scaling). A 4x arm that takes four times as long
;; per sample is four times as likely to overlap a burst, and on a busy machine
;; its fastest of seven still carried one: this read 7.27 under load, unbalanced,
;; on a linear reader. Balanced, a burst is as likely to land on either arm.
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

;; Linear measures ~4.0 and quadratic ~16, so the line goes between them. 8 is
;; twice the linear cost — room for a slow or loaded machine, while still an
;; enormous distance from quadratic.
(def ^:private max-ratio 8.0)

;; BOTH arms must sit in the same GC regime, which is a stronger requirement than
;; "above timer noise".
;;
;; Per-form cost here is flat at ~4.7-5.0us up to 8000 forms, steps to ~8.2us at
;; 16000, and is flat again above that (9.46us at 32000, 9.70 at 64000, i.e.
;; linear). Sizing 8000 -> 32000 straddled the step, so it measured the step and
;; not the reader (an honest ratio of ~7.4 against the 8.0 ceiling). 2000 -> 8000
;; puts both arms on the flat side below the step, and is an order of magnitude
;; less work than above it.
;;
;; Cost is a correctness property for this gate, not just a nicety: CI runs the
;; whole suite as `make -j$(nproc) test`, so a gate that hogs a core for seconds
;; starves the other timing gates running beside it and fails THEM (16000 ->
;; 64000 did that to the complexity gate). Staying small also keeps a REGRESSED
;; implementation failing in seconds: the old drain-and-push-back bug is
;; quadratic, so the 4x arm's cost grows with n^2.
(def ^:private n1 2000)
(def ^:private factor 4)

(defn- report [[t1 t4 ratio] note]
  (println (format "read-scaling: %d forms %.2fms, %d forms %.2fms, ratio %.2f (linear ~%.1f, quadratic ~%.1f, ceiling %.1f)%s"
                   n1 t1 (* factor n1) t4 ratio
                   (double factor) (double (* factor factor)) max-ratio note)))

(defn -main [& _]
  (let [s1 (source n1)
        s4 (source (* factor n1))
        c1 (read-all s1)
        c4 (read-all s4)]
    ;; a ratio over the wrong number of forms would be meaningless, so check the
    ;; reads did what they claim before judging what they cost
    (when (or (not= c1 n1) (not= c4 (* factor n1)))
      (println (str "FAIL read-scaling: read the wrong number of forms — "
                    c1 " (want " n1 ") and " c4 " (want " (* factor n1) ")"))
      (System/exit 1))
    ;; A miss is measured once more before it counts. That costs no POWER: a
    ;; quadratic read measures ~16 in every round of both measurements, while a
    ;; burst long enough to spoil all seven rounds of one rarely spoils the next.
    ;; There is no fail-on-sight threshold: a clear regression reads high on the
    ;; re-measure too, and one reading is never enough to fail on.
    (let [f1 #(read-all s1)
          f4 #(read-all s4)
          m (paired f1 f4 factor)
          m (if (<= (nth m 2) max-ratio)
              m
              (do (report m " — over the ceiling, measuring again")
                  (paired f1 f4 factor)))]
      (report m "")
      (if (<= (nth m 2) max-ratio)
        (println "read-scaling: passed")
        (do (println (str "FAIL read-scaling: reading scaled worse than linearly in the source size. "
                          "A read off a host reader is walking the whole remaining input again per form "
                          "(host-reader-read-form / host-reader-string-cursor in host/chez/java/io.ss, "
                          "or the position cursor in rdr-line-col-at no longer being reused)."))
            (System/exit 1))))))

(-main)
