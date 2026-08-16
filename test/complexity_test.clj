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
      (judge "transient write-few"
             (best-of 3 #(dotimes [_ 200] (touch m1)))
             (best-of 3 #(dotimes [_ 200] (touch m2)))
             "persistent! is rebuilding the whole map instead of freezing only the nodes the writes claimed (transients.ss jolt-persistent!, collections.ss enode-freeze)"))

    (if (pos? @failures)
      (do (println (str "complexity: " @failures " section(s) failed"))
          (System/exit 1))
      (println "complexity: passed"))))

(-main)
