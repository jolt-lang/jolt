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

(defn- timed
  "[elapsed-ms result] — the count comes back with the time so verifying what was
  read costs no extra pass."
  [f]
  (let [t (System/nanoTime)
        v (f)]
    [(/ (- (System/nanoTime) t) 1e6) v]))

;; best-of, not a single run. Both arms allocate heavily (a PushbackReader over a
;; multi-megabyte string, a form's worth of collections per read), so a GC episode
;; or a loaded machine lands in ONE arm and moves the ratio on its own. This gate
;; took exactly one measurement per arm, which is the most flake-prone shape there
;; is: nothing could absorb a single bad sample. Interference can only ever make a
;; run slower, so the minimum of a few runs is the robust estimator.
(def ^:private samples 3)
;; ...and if the ratio still exceeds the ceiling, re-measure before failing. This
;; costs no POWER: a quadratic implementation measures ~16 and is over the ceiling
;; on every attempt, so it still fails deterministically. Only a one-off
;; interference episode passes on retry, and that was never a real failure. This
;; gate did fail once that way, under CPU contention from a concurrent build.
(def ^:private tries 3)

;; BOTH arms must sit in the same GC regime, which is a stronger requirement than
;; "above timer noise" and is what this was missing.
;;
;; Per-form cost here is flat at ~4.7-5.0us up to 8000 forms, steps to ~8.2us at
;; 16000, and is flat again above that (9.46us at 32000, 9.70 at 64000, i.e.
;; linear). The old sizing, 8000 -> 32000, straddled the step, so it measured the
;; step and not the reader: the honest interference-free ratio was ~7.4 against a
;; ceiling of 8.0, and the 2.88 it used to report was an imprecise 1x arm
;; flattering the result.
;;
;; 2000 -> 8000 puts both arms on the flat side BELOW the step, which is the
;; cheap side. Measured 4.15-4.36 over repeated attempts against a linear
;; expectation of 4.0 — tighter than the region above the step gives
;; (16000 -> 64000 measures 4.19-5.32) and an order of magnitude less work.
;;
;; Cost is a correctness property for this gate, not just a nicety: CI runs the
;; whole suite as `make -j$(nproc) test`, so a gate that hogs a core for seconds
;; starves the other timing gates running beside it and fails THEM. An earlier
;; revision of this file used 16000 -> 64000 and did exactly that to the
;; complexity gate, whose ceiling sits at 2.0 over 3ms measurements. Staying small
;; also keeps a REGRESSED implementation failing fast rather than hanging: the old
;; drain-and-push-back bug is quadratic, so the 4x arm's cost grows with n^2.
(def ^:private n1 2000)
(def ^:private factor 4)

;; Linear measures ~4.0 and quadratic ~16, so the line goes between them. 8 is
;; twice the linear cost — room for a slow or loaded machine, while still an
;; enormous distance from quadratic.
(def ^:private max-ratio 8.0)

;; A ratio at or above this is not ambiguous — it is most of the way to quadratic,
;; so it fails on the spot with no re-measuring. Re-measuring a genuine regression
;; only multiplies a slow arm by the retry count.
(def ^:private clear-regression 12.0)

(defn- best-of [k f]
  (reduce min (map first (repeatedly k #(timed f)))))

(defn -main [& _]
  (let [s1 (source n1)
        s4 (source (* factor n1))
        [w1 c1] (timed #(read-all s1))  ; also warms: one-time costs stay out of the ratio
        [w4 c4] (timed #(read-all s4))]
    ;; a ratio over the wrong number of forms would be meaningless, so check the
    ;; reads did what they claim before judging what they cost
    (when (or (not= c1 n1) (not= c4 (* factor n1)))
      (println (str "FAIL read-scaling: read the wrong number of forms — "
                    c1 " (want " n1 ") and " c4 " (want " (* factor n1) ")"))
      (System/exit 1))
    ;; The verification runs above were timed, so screen with them before paying
    ;; for best-of: an unambiguous regression fails here, after one run per arm.
    (let [screen (/ w4 (max 0.001 w1))]
      (when (>= screen clear-regression)
        (println (format "read-scaling: %d forms %.2fms, %d forms %.2fms, ratio %.2f (linear ~%.1f, quadratic ~%.1f, ceiling %.1f)"
                         n1 w1 (* factor n1) w4 screen
                         (double factor) (double (* factor factor)) max-ratio))
        (println (str "FAIL read-scaling: reading scaled worse than linearly in the source size. "
                      "A read off a host reader is walking the whole remaining input again per form "
                      "(host-reader-read-form / host-reader-string-cursor in host/chez/java/io.ss, "
                      "or the position cursor in rdr-line-col-at no longer being reused)."))
        (System/exit 1)))
    (loop [attempt 1 seen []]
      (let [t1 (max 0.001 (best-of samples #(read-all s1)))
            t4 (best-of samples #(read-all s4))
            ratio (/ t4 t1)
            seen (conj seen ratio)]
        (println (format "read-scaling: %d forms %.2fms, %d forms %.2fms, ratio %.2f (linear ~%.1f, quadratic ~%.1f, ceiling %.1f)"
                         n1 t1 (* factor n1) t4 ratio
                         (double factor) (double (* factor factor)) max-ratio))
        (cond
          (<= ratio max-ratio) (println "read-scaling: passed")
          ;; unambiguous: don't spend more runs confirming it
          (>= ratio clear-regression)
          (do (println (str "FAIL read-scaling: reading scaled worse than linearly in the source size. "
                            "A read off a host reader is walking the whole remaining input again per form "
                            "(host-reader-read-form / host-reader-string-cursor in host/chez/java/io.ss, "
                            "or the position cursor in rdr-line-col-at no longer being reused)."))
              (System/exit 1))
          (< attempt tries)
          (do (println (format "read-scaling: ratio %.2f over ceiling %.1f — re-measuring (attempt %d of %d)"
                               ratio max-ratio (inc attempt) tries))
              (recur (inc attempt) seen))
          :else
          (do (println (str "FAIL read-scaling: reading scaled worse than linearly in the source size. "
                            "A read off a host reader is walking the whole remaining input again per form "
                            "(host-reader-read-form / host-reader-string-cursor in host/chez/java/io.ss, "
                            "or the position cursor in rdr-line-col-at no longer being reused)."))
              (println (str "  ratios over " tries " attempts: "
                            (clojure.string/join ", " (map #(format "%.2f" %) seen))))
              (System/exit 1)))))))

(-main)
