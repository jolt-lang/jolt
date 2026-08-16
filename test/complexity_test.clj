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
;; Ops are repeated so the fast cases clear jolt's ~1us timer granularity —
;; several of them now measure as zero for a single call.

(ns complexity-test)

(def ^:private n1 50000)
(def ^:private n2 200000)
(def ^:private reps 2000)
(def ^:private max-ratio 2.0)

(defn- timed [f]
  (let [t (System/nanoTime)]
    (f)
    (- (System/nanoTime) t)))

(defn- best-of [k f]
  (f)                                     ; warm
  (reduce min (map (fn [_] (timed f)) (range k))))

(def ^:private failures (atom 0))

(defn- judge [label t1 t4 detail]
  (let [ratio (double (/ (max 1 t4) (max 1 t1)))]
    (println (format "complexity %-22s %5dms at n, %5dms at 4n, ratio %5.2f (flat ~1.0, linear ~4.0, ceiling %.1f)"
                     label (quot t1 1000000) (quot t4 1000000) ratio max-ratio))
    (when (> ratio max-ratio)
      (println (str "FAIL complexity " label ": " detail))
      (swap! failures inc))))

(defn -main [& _]
  (let [v1 (vec (range n1))            v2 (vec (range n2))
        s1 (seq v1)                    s2 (seq v2)
        sm1 (into (sorted-map) (map (fn [i] [i i]) (range n1)))
        sm2 (into (sorted-map) (map (fn [i] [i i]) (range n2)))
        ss1 (into (sorted-set) (range n1))
        ss2 (into (sorted-set) (range n2))]

    ;; values first — a ratio over wrong answers would mean nothing
    (when-not (and (= (count s1) n1) (= (count s2) n2)
                   (= (first (drop (- n1 2) s1)) (- n1 2))
                   (= (first (rseq v1)) (dec n1))
                   (= (last (rseq v1)) 0)
                   (= (first sm1) [0 0]) (= (first ss1) 0)
                   (= (first (sorted-map)) nil) (= (first (sorted-set)) nil))
      (println "FAIL complexity: wrong values before timing")
      (System/exit 1))

    (judge "count vector-seq"
           (best-of 3 #(dotimes [_ reps] (count s1)))
           (best-of 3 #(dotimes [_ reps] (count s2)))
           "count is walking a vector-backed seq instead of subtracting its index from the backing vector's count (collections.ss)")

    (judge "drop vector-seq"
           (best-of 3 #(dotimes [_ reps] (drop (- n1 5) s1)))
           (best-of 3 #(dotimes [_ reps] (drop (- n2 5) s2)))
           "drop is stepping instead of jumping to the index (jolt-drop, seq.ss)")

    (judge "rseq vector"
           (best-of 3 #(dotimes [_ reps] (rseq v1)))
           (best-of 3 #(dotimes [_ reps] (rseq v2)))
           "rseq is materializing the vector — Clojure documents it as constant time (jolt-rseq, natives-seq.ss)")

    (judge "first sorted-map"
           (best-of 3 #(dotimes [_ reps] (first sm1)))
           (best-of 3 #(dotimes [_ reps] (first sm2)))
           "first on a sorted map is materializing the tree instead of walking to its leftmost node (25-sorted.clj :first, routed via host-table.ss)")

    (judge "first sorted-set"
           (best-of 3 #(dotimes [_ reps] (first ss1)))
           (best-of 3 #(dotimes [_ reps] (first ss2)))
           "first on a sorted set is materializing the tree instead of walking to its leftmost node (25-sorted.clj :first)")

    ;; nth dispatch shape: RT.nth tests Indexed (jolt's pvec) FIRST and
    ;; returns, so a vector nth must not pay for the extension-type probes the
    ;; jolt-nth set! wrapper chain runs in front of the base pvec arm
    ;; (jolt-array?, the al/sb shim, lazyseq, transient — four layers with the
    ;; natives-array wrapper outermost). nth on a 5-element vector (the
    ;; tree-node/tuple shape) must sit near count on the SAME vector: both are
    ;; a type test plus a small read out of the same record, and count keeps
    ;; its builtin pvec arm inline. The chain billed nth ~6.9x count here; the
    ;; hoisted pvec case sits at ~3.2. Absolute ns flakes across machines; the
    ;; ratio rides the machine's own speed.
    (let [v (vec [10 20 30 40 50])
          nth-reps 1000000
          nth-t (best-of 5 #(dotimes [_ nth-reps] (nth v 2)))
          cnt-t (best-of 5 #(dotimes [_ nth-reps] (count v)))
          ratio (double (/ nth-t (max 1 cnt-t)))]
      (when-not (and (= 30 (nth v 2)) (= 30 (nth v 2 :none)) (= :none (nth v 99 :none))
                     (= 50 (nth v 4)) (nil? (nth nil 3)) (= :d (nth nil 3 :d)))
        (println "FAIL complexity nth-dispatch: wrong values before timing")
        (System/exit 1))
      (println (format "complexity %-22s %4.2fx count (%4dms nth vs %4dms count, ceiling 5.0; hoisted ~3.2, wrapper chain ~6.9)"
                       "nth-dispatch" ratio (quot nth-t 1000000) (quot cnt-t 1000000)))
      (when (> ratio 5.0)
        (println "FAIL complexity nth-dispatch: a vector nth is paying for the extension-type wrapper chain again (jolt-nth set! chain) — RT.nth tests Indexed first, keep the pvec case hoisted in the outermost wrapper")
        (swap! failures inc)))

    (if (pos? @failures)
      (do (println (str "complexity: " @failures " section(s) failed"))
          (System/exit 1))
      (println "complexity: passed"))))

(-main)
