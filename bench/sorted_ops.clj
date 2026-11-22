;; sorted-ops — every operation a sorted collection answers, timed one by one.
;;
;; sorted-build and sorted-set-build time construction; sorted-access the
;; constant-time ends. This one covers the rest: point lookups (get, contains?,
;; find), removal (dissoc, disj, and the first/disj priority-queue loop), walks
;; (seq, reduce, rseq, keys/vals), bounded navigation (subseq/rsubseq) and
;; java.util.TreeMap built on the same tree. Each op prints its own line so a
;; change to the tree can be held to a per-op ceiling, not just the total.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh sorted-ops 20000
(ns sorted-ops)

(defn- scrambled [n] (mapv (fn [i] (mod (* i 7919) n)) (range n)))

(defn- build [n]
  (let [ks (scrambled n)]
    {:ks ks
     :sm (into (sorted-map) (map (fn [k] [k k])) ks)
     :ss (into (sorted-set) ks)
     :sb (into (sorted-map-by (fn [a b] (compare b a))) (map (fn [k] [k k])) ks)
     :str (into (sorted-set) (map (fn [k] (str "k" k))) ks)}))

(defn- op-get [{:keys [sm ks]}]
  (reduce (fn [a k] (+ a (get sm k))) 0 ks))
(defn- op-get-miss [{:keys [sm ks]}]
  (reduce (fn [a k] (if (get sm (- -1 k)) (inc a) a)) 0 ks))
(defn- op-contains [{:keys [ss ks]}]
  (reduce (fn [a k] (if (contains? ss k) (inc a) a)) 0 ks))
(defn- op-find [{:keys [sm ks]}]
  (reduce (fn [a k] (+ a (val (find sm k)))) 0 ks))
(defn- op-get-by [{:keys [sb ks]}]
  (reduce (fn [a k] (+ a (get sb k))) 0 ks))
(defn- op-get-str [{:keys [str ks]}]
  (reduce (fn [a k] (if (contains? str (clojure.core/str "k" k)) (inc a) a)) 0 ks))
(defn- op-dissoc-all [{:keys [sm ks]}]
  (count (reduce dissoc sm ks)))
(defn- op-disj-half [{:keys [ss ks]}]
  (count (reduce disj ss (take-nth 2 ks))))
(defn- op-pop-min [{:keys [ss]}]
  (loop [s ss n 0] (if (seq s) (recur (disj s (first s)) (inc n)) n)))
(defn- op-pop-max [{:keys [ss]}]
  (loop [s ss n 0] (if (seq s) (recur (disj s (first (rseq s))) (inc n)) n)))
(defn- op-seq-walk [{:keys [sm]}]
  (loop [s (seq sm) n 0] (if s (recur (next s) (+ n (key (first s)))) n)))
(defn- op-reduce [{:keys [ss]}]
  (reduce + 0 ss))
(defn- op-reduce-kv [{:keys [sm]}]
  (reduce-kv (fn [a k v] (+ a k v)) 0 sm))
(defn- op-rseq [{:keys [ss]}]
  (reduce + 0 (rseq ss)))
(defn- op-keys-vals [{:keys [sm]}]
  (+ (reduce + 0 (keys sm)) (reduce + 0 (vals sm))))
(defn- op-subseq [{:keys [ss ks]}]
  (let [n (count ks)]
    (loop [i 0 acc 0]
      (if (< i 200)
        (let [lo (mod (* i 977) n)]
          (recur (inc i) (+ acc (count (subseq ss >= lo < (+ lo 50))))))
        acc))))
(defn- op-rsubseq [{:keys [ss ks]}]
  (let [n (count ks)]
    (loop [i 0 acc 0]
      (if (< i 200)
        (let [hi (mod (* i 977) n)]
          (recur (inc i) (+ acc (count (rsubseq ss > (- hi 50) <= hi)))))
        acc))))
(defn- op-subseq-first [{:keys [ss ks]}]
  (reduce (fn [a k] (+ a (first (subseq ss >= k)))) 0 ks))
(defn- op-treemap [{:keys [ks]}]
  (let [tm (java.util.TreeMap.)]
    (doseq [k ks] (.put tm k k))
    (let [hits (reduce (fn [a k] (+ a (.get tm k))) 0 ks)
          fl (reduce (fn [a k] (+ a (.floorKey tm k))) 0 ks)]
      (doseq [k (take-nth 2 ks)] (.remove tm k))
      (+ hits fl (.size tm)))))

(def ops
  [["get" op-get] ["get-miss" op-get-miss] ["contains?" op-contains]
   ["find" op-find] ["get-by-cmp" op-get-by] ["contains-str" op-get-str]
   ["dissoc-all" op-dissoc-all] ["disj-half" op-disj-half]
   ["pop-min" op-pop-min] ["pop-max" op-pop-max]
   ["seq-walk" op-seq-walk] ["reduce" op-reduce] ["reduce-kv" op-reduce-kv]
   ["rseq" op-rseq] ["keys+vals" op-keys-vals]
   ["subseq" op-subseq] ["rsubseq" op-rsubseq] ["subseq-first" op-subseq-first]
   ["treemap" op-treemap]])

(defn- time-ms [f state]
  (let [t0 (System/nanoTime) r (f state)]
    [(/ (- (System/nanoTime) t0) 1000000.0) r]))

(defn- r1 [x] (/ (Math/round (* x 10.0)) 10.0))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 20000)
        state (build n)
        small (build (quot n 4))
        runs 3]
    (doseq [[_ f] ops] (dotimes [_ 2] (f small)))        ; warmup
    (let [per-op (mapv (fn [[nm f]]
                         (let [ts (mapv (fn [_] (time-ms f state)) (range runs))]
                           [nm (mapv first ts) (second (first ts))]))
                       ops)
          totals (apply mapv + (map second per-op))
          mean (/ (reduce + totals) runs)]
      (doseq [[nm mss r] per-op]
        (println (format "  %-13s %8.2f ms  (result %s)" nm (/ (reduce + mss) runs) r)))
      (println "sorted-ops n" n "result" (reduce + (map #(nth % 2) per-op)))
      (println "runs:" (mapv r1 totals))
      (println "mean:" (r1 mean) "ms"))))
