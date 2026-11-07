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
;; What is compared is THROUGHPUT, not the time for a fixed number of reps,
;; because a fixed rep count cannot serve both ends of this gate at once. The
;; ops here are ~40ns when correct and ~20ms when broken — a 500,000x spread. A
;; rep count small enough to keep the BROKEN case from running for an hour
;; leaves the correct one measuring tens of microseconds, which is under the
;; noise floor of a shared CI runner: `rseq vector` failed on main at ratio 3.22
;; with both arms reading 0ms, on a build where rseq is O(1). So each op is
;; timed in batches sized to clear 1ms, and an arm spends the same wall time
;; whatever its speed.
;;
;; An arm's rate is its FASTEST batch, not the mean over its budget.
;; Interference — a neighbour's burst, a preemption, a collection — only ever
;; makes a batch slower, so the minimum is the estimate of what the op costs.
;; The mean took in every interrupt the budget overlapped, and a loaded runner
;; read the write-few row 2.04 against 2.0 (fixed ~1.1) with the mean in place.
;; A genuinely linear op is linear in every batch, so the minimum masks nothing.

(ns complexity-test
  (:require [clojure.set]))

(def ^:private n1 50000)
(def ^:private n2 200000)
(def ^:private max-ratio 2.0)
(def ^:private budget-ns 20000000)        ; 20ms of batches per arm per round
(def ^:private floor-ns   1000000)        ; a batch must clear 1ms to be timed

;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- timed [f]
  (let [t (cpu-ns)]
    (f)
    (- (cpu-ns) t)))

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
  "Operations per second of the fastest b-call batch run within budget-ns, and
  of at least five batches: a broken op is one ~20ms batch, and the fastest of
  two filters nothing."
  [f b]
  (loop [spent 0 best Long/MAX_VALUE k 0]
    (if (and (>= spent budget-ns) (>= k 5))
      (/ (* (double b) 1e9) (double (max 1 best)))
      (let [t (timed #(dotimes [_ b] (f)))]
        (recur (+ spent t) (min best t) (inc k))))))

(def ^:private rounds 5)

(defn- paired
  "Measure both arms in alternation, `rounds` rounds of each, and judge each
  arm by its fastest batch over all of them. Returns [ratio r1 r4].

  The arms run in the SAME rounds, back to back, so a loaded stretch of a shared
  runner lands on both, and they swap order every round, so neither always runs
  first (the first arm of a round read systematically slower: 0.55-0.67 on rows
  whose two arms do identical work). Each round starts from a full collection,
  so a collection owed by the previous row or round is not billed to whichever
  arm trips it. Batch sizes are fixed once per arm.

  This used to keep the round with the smallest ratio, which cancels what both
  arms share within a round — but it can only ever err toward PASSING, and
  under load it does: with every core busy, the linear control row below read
  0.14 and 0.73 that way (one round's n arm starved for its whole budget, the 4n
  arm not), and a regressed catvec in test/chez/rrb-scaling-test.ss read 1.45.
  The fastest batch per arm needs only one clean batch per arm in ~50, which a
  loaded machine still yields, and a linear op is linear in every batch."
  [f1 f4]
  (f1) (f4)                               ; warm both
  (let [b1 (batch-size f1) b4 (batch-size f4)
        [r1 r4] (reduce (fn [[x1 x4] i]
                          (System/gc)
                          (if (even? i)
                            (let [y1 (rate f1 b1) y4 (rate f4 b4)] [(max x1 y1) (max x4 y4)])
                            (let [y4 (rate f4 b4) y1 (rate f1 b1)] [(max x1 y1) (max x4 y4)])))
                        [0.0 0.0]
                        (range rounds))]
    [(/ r1 (max 1.0 r4)) r1 r4]))

(def ^:private failures (atom 0))

(defn- report [label [ratio r1 r4] note]
  (println (format "complexity %-22s %10.0f ops/s at n, %10.0f at 4n, ratio %5.2f (flat ~1.0, linear ~4.0, ceiling %.1f)%s"
                   label r1 r4 ratio max-ratio note)))

;; Takes the two arms as THUNKS, not as rates: the alternation above is the
;; point, and a caller that measured them itself could hand over two numbers
;; from different moments without it being visible here.
;;
;; A miss is measured once more before it counts, as hotpath_scaling_test's
;; judge-staged does. That costs no power: a linear op reads ~4.0 in every
;; round of both measurements, while a burst long enough to spoil all five
;; rounds of one rarely spoils the next five too.
(defn- judge [label f1 f4 detail]
  ;; Rates, so the SLOWER arm is the smaller number and the ratio keeps the same
  ;; sense it always had: flat ~1.0, linear ~4.0.
  (let [m (paired f1 f4)
        m (if (<= (first m) max-ratio)
            m
            (do (report label m " — over the ceiling, measuring again")
                (paired f1 f4)))]
    (report label m "")
    (when (> (first m) max-ratio)
      (println (str "FAIL complexity " label ": " detail))
      (swap! failures inc))))

;; The harness's own power, checked every run: an op that IS linear must read
;; over the ceiling. Every row below fails by turning linear, so if the
;; estimator ever stopped seeing a linear op (a batch that stops scaling, a
;; min that picks a broken sample) this row says so rather than every row
;; passing vacuously. Re-measured on a miss, as judge is.
(defn- judge-control [label f1 f4]
  (let [m (paired f1 f4)
        m (if (> (first m) max-ratio) m (paired f1 f4))]
    (report label m " (control: must read OVER the ceiling)")
    (when-not (> (first m) max-ratio)
      (println (str "FAIL complexity " label ": a linear op read under the ceiling, so this "
                    "harness cannot see the regressions the rows below exist to catch"))
      (swap! failures inc))))

;; A fn over a `case` of n string constants ("s0" -> 0 ...), default -1: the
;; case row below dispatches to its LAST arm at 50 and at 200 constants.
(defmacro ^:private string-case-fn [n]
  (let [x (gensym "x")]
    `(fn [~x] (case ~x ~@(mapcat (fn [i] [(str "s" i) i]) (range n)) -1))))

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
                   (= (nthrest v1 (- n1 2)) [(- n1 2) (- n1 1)])
                   (= (nthnext v1 n1) nil)
                   (= (first (drop (- n1 2) (range n1))) (- n1 2))
                   (= (first (drop n1 (range))) n1)
                   (= (first (drop (- n1 2) src1)) (nth src1 (- n1 2)))
                   (= (first (nthnext (range 0 (* 3 n1) 3) (- n1 1))) (* 3 (- n1 1)))
                   (= (first (rseq v1)) (dec n1))
                   (= (last (rseq v1)) 0)
                   (= (first sm1) [0 0]) (= (first ss1) 0)
                   (= (count (tree-seq coll? seq c4)) 4001)
                   (= (take 3 (tree-seq coll? seq c1)) [c1 499 (second c1)])
                   (= (first (sorted-map)) nil) (= (first (sorted-set)) nil)
                   (= (take 3 sm2) [[0 0] [1 1] [2 2]]) (= (first (rseq ss2)) (dec n2))
                   (= (first (keys sm1)) 0) (= (hash sm1) (hash (into {} sm1))) (not= ss1 ss2)
                   (= (clojure.string/index-of src1 "bar)" 4) 5)
                   (= (clojure.string/index-of src1 "(foo" 1) 9)
                   (= (clojure.string/index-of src1 "zzz" 4) nil)
                   (= (clojure.string/index-of src1 "(foo" -5) 0)
                   (= (clojure.string/index-of src1 "(foo" 999999) nil)
                   (= (clojure.string/last-index-of src1 "bar)") (- (count src1) 4))
                   (= (clojure.string/last-index-of src1 "(foo" (- (count src1) 1)) (- (count src1) 9)))
      (println "FAIL complexity: wrong values before timing")
      (System/exit 1))

    (judge-control "control: reduce vector" #(reduce + v1) #(reduce + v2))

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

    ;; Clojure 1.12's nthrest/nthnext hand an IDrop coll the count, as drop does.
    ;; drop on a range and on a string's seq skip to the target (seq.ss drop-walk):
    ;; a bounded range computes the 32-element block the target falls in, the
    ;; unbounded (range) and a StringSeq re-enter at the target position.
    (judge "nthrest vector"
           #(nthrest v1 (- n1 5))
           #(nthrest v2 (- n2 5))
           "nthrest is stepping instead of handing the vector the count (jolt-nthrest, seq.ss)")

    (judge "nthnext vector"
           #(nthnext v1 (- n1 5))
           #(nthnext v2 (- n2 5))
           "nthnext is stepping instead of handing the vector the count (jolt-nthnext, seq.ss)")

    ;; To the middle, not to n-5: the last block of (range n1) holds 16 elements
    ;; and of (range n2) 32, so the arms realized different-sized chunks and the
    ;; row read ~1.25 with nothing walked (2.2 on one CI runner, failing a PR).
    ;; Mid-range both arms build a full block; a walking drop still reads ~4.0.
    (judge "drop range"
           #(first (drop (quot n1 2) (range n1)))
           #(first (drop (quot n2 2) (range n2)))
           "drop on a bounded range is walking its blocks instead of computing the target's (range-skip, seq.ss)")

    (judge "drop unbounded range"
           #(first (drop n1 (range)))
           #(first (drop n2 (range)))
           "drop on (range) is stepping instead of re-entering at the target (drop-walk, seq.ss)")

    (judge "drop string"
           #(first (drop (- n1 5) src1))
           #(first (drop (- n2 5) src2))
           "drop on a string's seq is stepping instead of re-entering at the target index (drop-walk, seq.ss)")

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

    ;; the seq views themselves: a stack walk, O(log n) to the head (jolt-iz4)
    (judge "take 3 sorted-map"
           #(doall (take 3 sm1))
           #(doall (take 3 sm2))
           "seq on a sorted map is materializing the tree instead of walking it lazily (host-table.ss sc-tree-seq)")

    (judge "first rseq sorted-set"
           #(first (rseq ss1))
           #(first (rseq ss2))
           "rseq on a sorted set is materializing the tree instead of walking it lazily (host-table.ss sc-tree-seq)")

    (judge "first keys sorted-map"
           #(first (keys sm1))
           #(first (keys sm2))
           "keys of a sorted map is materializing the tree instead of walking it lazily (host-table.ss sc-tree-seq)")

    ;; hash is cached on the value, and = of different sizes is a count compare
    (judge "hash sorted-map"
           #(hash sm1)
           #(hash sm2)
           "hash of a sorted map is not cached (host-table.ss sorted-hash)")

    (judge "= sorted-sets of different sizes"
           #(= ss1 ss2)
           #(= ss2 ss1)
           "= of two sorted sets is not comparing counts first (host-table.ss sorted-fast=)")

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
      ;; moments; `paired` above measures them together. Then a loaded runner
      ;; read 2.04 (PR #1350's CI, nothing touched transients), because an arm's
      ;; rate was the mean over its budget and took in every interrupt; it is
      ;; each arm's fastest batch now, over rounds that swap order. Fixed sits
      ;; ~1.05-1.16. Broken by enode-freeze copying every hnode, it reads ~3.0
      ;; (2.85 under the old mean): node count, not entry count, is what grows
      ;; 50k -> 200k at this depth. Folding every entry back, as the hashtable
      ;; transient did, read ~4.0.
      (judge "transient write-few"
             #(touch m1)
             #(touch m2)
             "persistent! is rebuilding the whole map instead of freezing only the nodes the writes claimed (transients.ss jolt-persistent!, collections.ss enode-freeze)"))

    ;; A PersistentQueue's seq walks its front list and then its rear, and hands
    ;; back the first cell without touching the rest. Building the whole element
    ;; list up front made (first q) -- and every seq/rest/next over a queue --
    ;; linear in the queue: a BFS that peeks via first slowed with its frontier.
    ;; Both shapes: conj'd only (one front element, the rest in the rear) and
    ;; popped once (everything moved to the front).
    (let [q-of (fn [n] (into clojure.lang.PersistentQueue/EMPTY (range n)))
          q1 (q-of n1)         q2 (q-of n2)
          p1 (pop (conj q1 n1)) p2 (pop (conj q2 n2))]
      (when-not (and (= 0 (first q1)) (= 1 (first p1)) (= 1 (second q1))
                     (= (range n1) (seq q1)) (= (range 1 (inc n1)) (seq p1))
                     (= n1 (count (seq q1))) (= (range 1 n1) (rest q1))
                     (nil? (seq clojure.lang.PersistentQueue/EMPTY))
                     (= [3] (seq (conj clojure.lang.PersistentQueue/EMPTY 3)))
                     (= (next (conj clojure.lang.PersistentQueue/EMPTY 3)) nil))
        (println "FAIL complexity queue-seq: wrong values before timing")
        (System/exit 1))
      (judge "first queue"
             #(first q1)
             #(first q2)
             "seq on a PersistentQueue is building every element before returning the first (queue->seq, java/natives-queue.ss)")
      (judge "first popped queue"
             #(first (next p1))
             #(first (next p2))
             "seq on a PersistentQueue is building every element before returning the first (queue->seq, java/natives-queue.ss)")
      ;; q1 is the boundary shape: one front element, the rest in the rear, so
      ;; popping it moves the rear to the front. The JVM's rear is a vector that
      ;; becomes the front in O(1); a reversed-list rear had to be reversed, and
      ;; a persistent q1 paid that again on every pop. Its hash is cached
      ;; (PersistentQueue._hasheq), so a repeat hash must not re-walk it.
      (when-not (and (= (range 1 n1) (seq (pop q1))) (= (hash q1) (hash (vec (range n1)))))
        (println "FAIL complexity queue-pop: wrong values before timing")
        (System/exit 1))
      (judge "pop queue at boundary"
             #(pop q1)
             #(pop q2)
             "pop at the front/rear boundary is copying the rear instead of making it the front (queue-pop, java/natives-queue.ss)")
      (judge "hash queue"
             #(hash q1)
             #(hash q2)
             "a queue's hash is recomputed on every call instead of cached (seq-hasheq-cached wrapper, java/natives-queue.ss)"))

    ;; clojure.set/difference walks the SMALLER side: taking a big set out of a
    ;; small one costs the small one, as the reference's two-way branch does. A
    ;; plain (reduce disj s1 s2) walked all of s2.
    (let [b1 (set (range n1)) b2 (set (range n2))]
      (when-not (and (= #{-1} (clojure.set/difference #{1 2 -1} b1))
                     (= #{1 3} (clojure.set/difference #{1 2 3} #{2 9 10 11})))
        (println "FAIL complexity set-difference: wrong values before timing")
        (System/exit 1))
      (judge "difference small big"
             #(clojure.set/difference #{1 2 -1} b1)
             #(clojure.set/difference #{1 2 -1} b2)
             "difference is disj-ing every element of the bigger second set instead of filtering the smaller first one (stdlib/clojure/set.clj)"))

    ;; A large `case` dispatches through one lookup in a constant map, not a chain
    ;; of = tests, one per arm. The chain made a dispatch to the last of 400
    ;; string constants ~8 us; the reference's case is a hashed tableswitch.
    ;; Each arm dispatches 200 times over ALL its keys (the 50-constant case
    ;; cycles its 50 four times), so the chain reads ~4.0 on the mean arm index,
    ;; 24.5 against 99.5. It used to time one key per case, "s49" against "s199",
    ;; and one key's cost is where its hash lands in the constant map's trie:
    ;; "s199" shares its low 10 bits with another key, so it sits a level deeper
    ;; than its siblings (~41 ns against ~33), and the row read 1.35-1.6, once
    ;; 1.98 against the 2.0 ceiling, with nothing scanned.
    (let [c1 (string-case-fn 50) c4 (string-case-fn 200)
          ks1 (vec (take 200 (cycle (map #(str "s" %) (range 50)))))
          ks4 (vec (map #(str "s" %) (range 200)))
          dispatch-all (fn [c ks] (reduce (fn [acc k] (+ acc (c k))) 0 ks))]
      (when-not (and (= 49 (c1 "s49")) (= 199 (c4 "s199")) (= 0 (c4 (str "s" 0)))
                     (= -1 (c4 "s200")) (= -1 (c4 nil)) (= -1 (c4 'x)) (= -1 (c1 "s199"))
                     (= (* 4 (reduce + (range 50))) (dispatch-all c1 ks1))
                     (= (reduce + (range 200)) (dispatch-all c4 ks4)))
        (println "FAIL complexity case-dispatch: wrong values before timing")
        (System/exit 1))
      (judge "case every arm"
             #(dispatch-all c1 ks1)
             #(dispatch-all c4 ks4)
             "case is testing its constants one = at a time instead of looking the value up (00-syntax.clj case)"))

    ;; (String. bytes offset length cs) copies only its slice. A byte array became a
    ;; bytevector WHOLE before the slice was cut, so 30 bytes out of a 1.5 MB
    ;; array took 47 us against 0.4 us out of a 40-byte one. Both arms take the
    ;; same 30 bytes, so the copy reads ~1.0 and the whole-array copy ~4.0.
    (let [b1 (byte-array n1 (byte 65)) b4 (byte-array n2 (byte 65))]
      (when-not (and (= "AAA" (String. b1 (int 7) (int 3) "UTF-8"))
                     (= "AAA" (String. b4 (int (- n2 3)) (int 3) "UTF-8")))
        (println "FAIL complexity string-slice: wrong values before timing")
        (System/exit 1))
      (judge "String byte slice"
             #(String. b1 (int 1000) (int 30) "UTF-8")
             #(String. b4 (int 1000) (int 30) "UTF-8")
             "(String. bytes off len) copies the whole array before cutting its slice (java/host-static-classes.ss bytes-slice-for-string)"))

    ;; remove on a HashMap / HashSet is O(1) as the JDK's is. The key-order list
    ;; was rebuilt without the key on every removal, so taking one key out of a
    ;; 20k map cost ~112us (jolt-lfm8p). Each arm removes a middle key and puts
    ;; it back, so the map keeps its size; the order is still insertion order.
    (let [hm-of (fn [n] (let [m (java.util.HashMap.)] (dotimes [i n] (.put m i i)) m))
          hs-of (fn [n] (let [s (java.util.HashSet.)] (dotimes [i n] (.add s i)) s))
          lhm-of (fn [n] (let [m (java.util.LinkedHashMap.)] (dotimes [i n] (.put m i i)) m))
          m1 (hm-of n1) m4 (hm-of n2) s1 (hs-of n1) s4 (hs-of n2) l1 (lhm-of n1) l4 (lhm-of n2)
          churn-map (fn [m k] (.remove m k) (.put m k k))
          churn-set (fn [s k] (.remove s k) (.add s k))
          k1 (quot n1 2) k4 (quot n2 2)
          probe (doto (java.util.HashMap.) (.put :a 1) (.put :b 2) (.put :c 3))]
      (churn-map probe :a) (.remove probe :b) (.put probe :d 4) (churn-map probe :c)
      (when-not (and (= [:a :d :c] (vec (.keySet probe)))
                     (= 3 (.size probe))
                     (do (churn-map m1 k1) (= n1 (.size m1)))
                     (= k1 (last (.keySet m1)))
                     (do (churn-set s1 k1) (= n1 (.size s1)))
                     (do (churn-map l1 k1) (= k1 (last (.keySet l1)))))
        (println "FAIL complexity hashmap-remove: wrong values before timing")
        (System/exit 1))
      (judge "HashMap remove"
             #(churn-map m1 k1)
             #(churn-map m4 k4)
             "HashMap.remove rebuilds the key-order list instead of leaving a stale entry (java/host-static-classes.ss hm-drop-key!)")
      (judge "HashSet remove"
             #(churn-set s1 k1)
             #(churn-set s4 k4)
             "HashSet.remove rebuilds the key-order list instead of leaving a stale entry (java/host-static-classes.ss hm-drop-key!)")
      (judge "LinkedHashMap remove"
             #(churn-map l1 k1)
             #(churn-map l4 k4)
             "LinkedHashMap.remove is linear in the map (java/linked-hash.ss)"))

    (if (pos? @failures)
      (do (println (str "complexity: " @failures " section(s) failed"))
          (System/exit 1))
      (println "complexity: passed"))))

(-main)
