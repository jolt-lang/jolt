;; Complexity gates: operations that must NOT be linear in the collection's size.
;;
;; Each of these was O(n) in jolt while the reference answers it from the shape,
;; and every one is invisible to a value test — the results were correct all
;; along, just derived by walking. Over 200k elements:
;;
;;   (count (seq v))        18.8ms   JVM 166ns   PersistentVector$ChunkedSeq is Counted
;;   (drop k (seq v))       18.5ms   JVM 625ns   ...and IDrop
;;   (rseq v)               19.8ms   JVM 209ns   rseq is documented as constant time
;;   (first sorted-map)      190ms   JVM 416ns   PersistentTreeMap.min() walks one spine
;;   (first sorted-set)       94ms   JVM 542ns
;;
;; The gate is the SHAPE, measured in one process: run each op at n and at 4n and
;; compare. A constant-time op holds near 1.0 and an O(log n) one barely moves
;; (4x the elements is two more levels of a ~17-deep tree); the linear versions
;; these replaced all sat near 4.0, so the ceiling has a wide margin either side
;; and does not depend on absolute timings, which differ per machine and flake
;; under parallel CI.
;;
;; What is compared is THROUGHPUT over a fixed time budget, not the time for a
;; fixed number of reps, because a fixed rep count cannot serve both ends of this
;; gate at once. The ops here are ~40ns when correct and ~20ms when broken — a
;; 500,000x spread. A rep count small enough to keep the BROKEN case from running
;; for an hour leaves the correct one measuring tens of microseconds, which is
;; under the noise floor of a shared CI runner: `rseq vector` failed on main at
;; ratio 3.22 with both arms reading 0ms, on a build where rseq is O(1).
;; Measuring how many ops fit in a budget instead makes every arm cost the same
;; wall time whatever its speed, and the batch self-calibrates past the ~1us
;; timer granularity, so neither end needs a hand-tuned constant.

(ns complexity-test)

(def ^:private n1 50000)
(def ^:private n2 200000)
(def ^:private max-ratio 2.0)
(def ^:private budget-ns 30000000)        ; 30ms of measurement per arm
(def ^:private floor-ns   1000000)        ; a batch must clear 1ms to be timed

(defn- timed [f]
  (let [t (System/nanoTime)]
    (f)
    (- (System/nanoTime) t)))

(defn- batch-size
  "The smallest power-of-two batch whose single run clears floor-ns, so what gets
  timed is the work and not the clock. A ~40ns op lands around 32k, a ~20ms one
  at 1 — which is the whole point: the caller below never picks a rep count."
  [f]
  (loop [b 1]
    (if (or (>= (timed #(dotimes [_ b] (f))) floor-ns) (>= b 1048576))
      b
      (recur (* 2 b)))))

(defn- rate
  "Operations per second, measured over at least budget-ns of wall time."
  [f]
  (let [b (batch-size f)]
    (loop [n 0 elapsed 0]
      (if (>= elapsed budget-ns)
        (/ (* (double n) 1e9) (double (max 1 elapsed)))
        (recur (+ n b) (+ elapsed (timed #(dotimes [_ b] (f)))))))))

(def ^:private rounds 3)

(defn- paired
  "Measure both arms in the SAME round, k times, and keep the round whose ratio
  is smallest — that is, the least contaminated pairing. Returns [ratio r1 r4].

  Measuring the arms separately and dividing their bests is not equivalent, and
  the difference is not academic: the two bests can come from different moments,
  and a loaded runner does not slow the arms equally. The 4n arm holds a working
  set four times larger, so it loses more to cache pressure and to GC — one CI
  run read n only 1.2x slower than a quiet machine while 4n was 2.2x slower, and
  the row failed at 2.15 against a 2.0 ceiling on a build where the operation is
  flat. Pairing cancels whatever both arms share in a round; what survives is the
  part that scales with n, which is the whole question. A genuinely linear
  operation still reads ~4.0 in every round, so nothing is masked."
  [f1 f4]
  (f1) (f4)                               ; warm both
  (reduce (fn [a b] (if (< (first b) (first a)) b a))
          (map (fn [_]
                 (let [r1 (rate f1)
                       r4 (rate f4)]
                   [(/ r1 (max 1.0 r4)) r1 r4]))
               (range rounds))))

(def ^:private failures (atom 0))

;; Takes the two arms as THUNKS, not as rates: the pairing above is the point,
;; and a caller that measured them itself could hand over two numbers from
;; different moments without it being visible here.
(defn- judge [label f1 f4 detail]
  ;; Rates, so the SLOWER arm is the smaller number and the ratio keeps the same
  ;; sense it always had: flat ~1.0, linear ~4.0.
  (let [[ratio r1 r4] (paired f1 f4)]
    (println (format "complexity %-22s %10.0f ops/s at n, %10.0f at 4n, ratio %5.2f (flat ~1.0, linear ~4.0, ceiling %.1f)"
                     label r1 r4 ratio max-ratio))
    (when (> ratio max-ratio)
      (println (str "FAIL complexity " label ": " detail))
      (swap! failures inc))))

(defn -main [& _]
  (let [v1 (vec (range n1))            v2 (vec (range n2))
        ;; source-shaped text: the window rows read a span out of the middle
        src1 (apply str (repeat (quot n1 8) "(foo bar)"))
        src2 (apply str (repeat (quot n2 8) "(foo bar)"))
        anchored #"^[ ,\n\r\t]+"
        s1 (seq v1)                    s2 (seq v2)
        sm1 (into (sorted-map) (map (fn [i] [i i]) (range n1)))
        sm2 (into (sorted-map) (map (fn [i] [i i]) (range n2)))
        ss1 (into (sorted-set) (range n1))
        ss2 (into (sorted-set) (range n2))
        chain (fn [d] (reduce (fn [acc i] [i acc]) [] (range d)))
        c1 (chain 500)                 c4 (chain 2000)]

    ;; values first — a ratio over wrong answers would mean nothing
    (when-not (and (= (count s1) n1) (= (count s2) n2)
                   (= (first (drop (- n1 2) s1)) (- n1 2))
                   (= (first (rseq v1)) (dec n1))
                   (= (last (rseq v1)) 0)
                   (= (first sm1) [0 0]) (= (first ss1) 0)
                   (= (count (tree-seq coll? seq c4)) 4001)
                   (= (take 3 (tree-seq coll? seq c1)) [c1 499 (second c1)])
                   (= (first (sorted-map)) nil) (= (first (sorted-set)) nil)
                   (= (clojure.string/index-of src1 "bar)" 4) 5)
                   (= (clojure.string/index-of src1 "(foo" 1) 9)
                   (= (clojure.string/index-of src1 "zzz" 4) nil)
                   (= (clojure.string/index-of src1 "(foo" -5) 0)
                   (= (clojure.string/index-of src1 "(foo" 999999) nil)
                   (= (clojure.string/last-index-of src1 "bar)") (- (count src1) 4))
                   (= (clojure.string/last-index-of src1 "(foo" (- (count src1) 1)) (- (count src1) 9)))
      (println "FAIL complexity: wrong values before timing")
      (System/exit 1))

;; mapcat and (apply concat ...) hand back their LAST collection as it is, as
    ;; clojure.lang's concat does, instead of copying it one cell per element.
    ;; tree-seq nests one mapcat per level of depth, so a copy at every level
    ;; costs each element its depth: a proof trace 325k nodes deep in places
    ;; took 31s to walk, JVM 30ms. Both arms walk 4d elements -- four chains of
    ;; depth d, one of depth 4d -- so linear reads ~1.0 and the copy ~4.0.
    (judge "tree-seq deep chain"
           #(dotimes [_ 4] (count (tree-seq coll? seq c1)))
           #(count (tree-seq coll? seq c4))
           "mapcat/apply concat is copying its last collection instead of returning it, so nested concats cost each element its depth (lazy-concat-outer, seq.ss)")

    (judge "count vector-seq"
           #(count s1)
           #(count s2)
           "count is walking a vector-backed seq instead of subtracting its index from the backing vector's count (collections.ss)")

    ;; String.lastIndexOf is a backward scan: a needle at the tail is found in
    ;; constant time whatever precedes it. The wrapper once reversed BOTH strings
    ;; (list->string of the whole subject) and ran index-of forward, which is
    ;; linear in the subject for every call — 2% of a formatter's format pass
    ;; from the one call per line it makes.
    (judge "last-index-of tail"
           #(clojure.string/last-index-of src1 "bar)")
           #(clojure.string/last-index-of src2 "bar)")
           "last-index-of is reversing the subject instead of scanning backward (natives-str.ss str-last-index-of-from)")

    ;; ...and index-of's from arity searches the tail in place: it once copied
    ;; the tail out with subs before scanning it, linear in what follows `from`.
    (judge "index-of from"
           #(clojure.string/index-of src1 "bar)" 4)
           #(clojure.string/index-of src2 "bar)" 4)
           "index-of's from arity copies the tail (subs) instead of scanning from `from` in place (str-find's start index)")

    (judge "last-index-of from"
           #(clojure.string/last-index-of src1 "(foo" (- (count src1) 1))
           #(clojure.string/last-index-of src2 "(foo" (- (count src2) 1))
           "the from arity of last-index-of copies and reverses the prefix instead of scanning backward from `from`")

    (judge "drop vector-seq"
           #(drop (- n1 5) s1)
           #(drop (- n2 5) s2)
           "drop is stepping instead of jumping to the index (jolt-drop, seq.ss)")

    (judge "rseq vector"
           #(rseq v1)
           #(rseq v2)
           "rseq is materializing the vector — Clojure documents it as constant time (jolt-rseq, natives-seq.ss)")

    (judge "first sorted-map"
           #(first sm1)
           #(first sm2)
           "first on a sorted map is materializing the tree instead of walking to its leftmost node (25-sorted.clj :first, routed via host-table.ss)")

    (judge "first sorted-set"
           #(first ss1)
           #(first ss2)
           "first on a sorted set is materializing the tree instead of walking to its leftmost node (25-sorted.clj :first)")

    ;; A parser reads its input through a WINDOW: it cuts a fixed span out of the
    ;; source at the position it has reached, over and over, and the source is the
    ;; whole file. Both ways of doing that must cost the window, not the file.
    ;;
    ;;   subs  — `(subs txt pos (+ pos 2048))`. Chez's `substring` is already
    ;;           O(span); what this pins is that it stays that way through
    ;;           jolt-substr's block copy (converters.ss) and through any future
    ;;           string representation. A shared-slice or rope representation that
    ;;           normalized on the way out would land here at ~4.0.
    ;;   re-find — a `^`-anchored pattern over the REST of the input. The engine
    ;;           must refuse the whole subject at the anchor rather than try each
    ;;           position, which is what makes matching at an index without
    ;;           copying viable at all (irx-search-from, regex.ss).
    ;;
    ;; bench/cst-format measures both as throughput; these two rows are the shape.
    (judge "subs window"
           #(subs src1 (- n1 4096) (- n1 2048))
           #(subs src2 (- n2 4096) (- n2 2048))
           "subs is copying (or re-deriving) the whole source string rather than the requested span (jolt-substr, converters.ss)")

    (judge "re-find anchored"
           #(re-find anchored src1)
           #(re-find anchored src2)
           "an anchored re-find is scanning every position of the subject instead of failing at the anchor (regex.ss)")

    ;; nth's values, but deliberately NOT its cost.
    ;;
    ;; RT.nth tests Indexed first and returns, so a vector nth must not pay for
    ;; the extension-type probes the jolt-nth wrapper chain runs in front of the
    ;; pvec arm. That is a real property and it is worth watching — but it is a
    ;; CONSTANT factor, not a complexity shape, so the only in-process way to
    ;; state it is to calibrate nth against some other operation, and this file
    ;; used to bill it against count on the same vector.
    ;;
    ;; That gate flaked, and the numbers say it cannot be repaired by moving the
    ;; ceiling. On one commit, two CI runners measured 2.79x and 5.14x against a
    ;; 5.0 ceiling. Scaled by the same machines, the wrapper-chain regression it
    ;; exists to catch lands around 6x on the fast runner and 11x on the slow
    ;; one — so the broken and fixed ranges OVERLAP, and any ceiling is either
    ;; flaky on slow runners or vacuous on fast ones. A gate that cannot
    ;; separate the two states is worse than none: it spends CI failures without
    ;; buying information.
    ;;
    ;; The measurement lives in bench/nth_access.clj instead, where a number
    ;; that moves is read by a person. Reference figures, one machine, forced
    ;; rebuilds both arms: small vector 34.34ns with the chain, 15.95 hoisted;
    ;; with a default 27.22 against 9.68.
    (let [v (vec [10 20 30 40 50])]
      (when-not (and (= 30 (nth v 2)) (= 30 (nth v 2 :none)) (= :none (nth v 99 :none))
                     (= 50 (nth v 4)) (nil? (nth nil 3)) (= :d (nth nil 3 :d)))
        (println "FAIL complexity nth-dispatch: wrong nth values")
        (System/exit 1)))

    ;; persistent! costs what the transient WROTE, not what the map holds. A
    ;; transient shares its source's nodes and claims only the ones a write
    ;; descends through, so writing 10 entries into a transient of a 200k map
    ;; freezes ~10 paths — the same work as writing 10 into a transient of a 50k
    ;; one. The hashtable transient this replaced copied every entry in at
    ;; transient() and folded every entry back through pmap-put-hash at
    ;; persistent!, so both ends were linear in the map and this sat at ~4.0.
    (let [m1 (into {} (map (fn [i] [i i]) (range n1)))
          m2 (into {} (map (fn [i] [i i]) (range n2)))
          touch (fn [m] (let [t (transient m)]
                          (dotimes [i 10] (assoc! t (- -1 i) i))
                          (count (persistent! t))))]
      (when-not (and (= (+ n1 10) (touch m1)) (= (+ n2 10) (touch m2))
                     (= (dec n1) (get m1 (dec n1))) (nil? (get m1 -1)))
        (println "FAIL complexity transient-write-few: wrong values before timing")
        (System/exit 1))
      ;; This row is the one that keeps finding the harness's weak spots, because
      ;; its arms differ in WORKING SET (a 50k map against a 200k one) and not
      ;; just in iteration count. First it was sized by a rep count, and the small
      ;; arm measured ~1ms — under the noise floor — so one GC pause read 2.06
      ;; against the 2.0 ceiling; the arms are sized by TIME now. Then, still,
      ;; a loaded runner read 2.15, because the bigger arm loses more to a busy
      ;; machine than the smaller one and the arms were measured at different
      ;; moments; `paired` above measures them together. Fixed sits ~1.1 (~0.8
      ;; unloaded), broken ~4.0.
      (judge "transient write-few"
             #(touch m1)
             #(touch m2)
             "persistent! is rebuilding the whole map instead of freezing only the nodes the writes claimed (transients.ss jolt-persistent!, collections.ss enode-freeze)"))

    (if (pos? @failures)
      (do (println (str "complexity: " @failures " section(s) failed"))
          (System/exit 1))
      (println "complexity: passed"))))

(-main)
