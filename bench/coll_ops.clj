;; coll-ops — every operation the unsorted persistent collections answer,
;; timed one by one: hash map, hash set, vector, list.
;;
;; sorted-ops does this for sorted colls. This one covers the HAMT behind
;; hash-map/hash-set (get, miss, assoc new/replace, dissoc, walk, reduce-kv,
;; =, hash, merge, transient build), the 32-way trie behind vectors (conj,
;; nth, assoc, pop, walk, reduce, subvec/into) and the cons list. Each op
;; prints its own line so a representation change can be held to a per-op
;; ceiling.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh coll-ops 20000
(ns coll-ops)

(defn- scrambled [n] (mapv (fn [i] (mod (* i 7919) n)) (range n)))

(defn- build [n]
  (let [ks (scrambled n)
        skeys (mapv (fn [k] (str "k" k)) ks)]
    {:ks ks
     :skeys skeys
     :hm (into {} (map (fn [k] [k k])) ks)
     :hm2 (into {} (map (fn [k] [k k])) (reverse ks))
     :hs (into #{} ks)
     :hsm (zipmap skeys ks)
     :v (vec ks)
     :l (apply list ks)
     ;; many small maps: the array-map / tiny-HAMT regime
     :smalls (mapv (fn [i] {:a i :b (inc i) :c (+ i 2) :d (+ i 3)}) (range (quot n 4)))}))

;; ---- hash map ----
(defn- op-hm-get [{:keys [hm ks]}] (reduce (fn [a k] (+ a (get hm k))) 0 ks))
(defn- op-hm-miss [{:keys [hm ks]}] (reduce (fn [a k] (if (get hm (- -1 k)) (inc a) a)) 0 ks))
(defn- op-hm-get-str [{:keys [hsm skeys]}] (reduce (fn [a k] (+ a (get hsm k))) 0 skeys))
(defn- op-hm-build [{:keys [ks]}] (count (reduce (fn [m k] (assoc m k k)) {} ks)))
(defn- op-hm-transient [{:keys [ks]}] (count (persistent! (reduce (fn [m k] (assoc! m k k)) (transient {}) ks))))
(defn- op-hm-replace [{:keys [hm ks]}] (count (reduce (fn [m k] (assoc m k (inc k))) hm ks)))
(defn- op-hm-dissoc [{:keys [hm ks]}] (count (reduce dissoc hm ks)))
(defn- op-hm-seq [{:keys [hm]}] (loop [s (seq hm) n 0] (if s (recur (next s) (+ n (key (first s)))) n)))
(defn- op-hm-reduce-kv [{:keys [hm]}] (reduce-kv (fn [a k v] (+ a k v)) 0 hm))
(defn- op-hm-keys [{:keys [hm]}] (reduce + 0 (keys hm)))
(defn- op-hm-eq [{:keys [hm hm2]}] (if (= hm hm2) 1 0))
(defn- op-hm-hash [{:keys [ks]}] (hash (into {} (map (fn [k] [k k])) ks)))
(defn- op-hm-merge [{:keys [hm smalls]}] (reduce (fn [a m] (+ a (count (merge m {:e 1})))) 0 smalls))
(defn- op-small-get [{:keys [smalls]}] (reduce (fn [a m] (+ a (:c m) (get m :d))) 0 smalls))
(defn- op-small-assoc [{:keys [smalls]}] (reduce (fn [a m] (+ a (count (assoc m :b 0 :z 1)))) 0 smalls))

;; ---- hash set ----
(defn- op-hs-contains [{:keys [hs ks]}] (reduce (fn [a k] (if (contains? hs k) (inc a) a)) 0 ks))
(defn- op-hs-build [{:keys [ks]}] (count (reduce conj #{} ks)))
(defn- op-hs-into [{:keys [ks]}] (count (into #{} ks)))
(defn- op-hs-disj [{:keys [hs ks]}] (count (reduce disj hs (take-nth 2 ks))))
(defn- op-hs-reduce [{:keys [hs]}] (reduce + 0 hs))

;; ---- vector ----
(defn- op-v-conj [{:keys [ks]}] (count (reduce conj [] ks)))
(defn- op-v-nth [{:keys [v ks]}] (reduce (fn [a k] (+ a (nth v k))) 0 ks))
(defn- op-v-assoc [{:keys [v ks]}] (count (reduce (fn [v k] (assoc v k 0)) v ks)))
(defn- op-v-pop [{:keys [v]}] (loop [v v n 0] (if (pos? (count v)) (recur (pop v) (inc n)) n)))
(defn- op-v-seq [{:keys [v]}] (loop [s (seq v) n 0] (if s (recur (next s) (+ n (first s))) n)))
(defn- op-v-reduce [{:keys [v]}] (reduce + 0 v))
(defn- op-v-into [{:keys [ks]}] (count (into [] ks)))
(defn- op-v-eq [{:keys [v ks]}] (if (= v ks) 1 0))

;; ---- list ----
(defn- op-l-conj [{:keys [ks]}] (count (reduce conj () ks)))
(defn- op-l-walk [{:keys [l]}] (loop [s l n 0] (if (seq s) (recur (rest s) (+ n (first s))) n)))
(defn- op-l-reduce [{:keys [l]}] (reduce + 0 l))

(def ops
  [["hm-get" op-hm-get] ["hm-miss" op-hm-miss] ["hm-get-str" op-hm-get-str]
   ["hm-build" op-hm-build] ["hm-transient" op-hm-transient]
   ["hm-replace" op-hm-replace] ["hm-dissoc" op-hm-dissoc]
   ["hm-seq" op-hm-seq] ["hm-reduce-kv" op-hm-reduce-kv] ["hm-keys" op-hm-keys]
   ["hm-eq" op-hm-eq] ["hm-hash" op-hm-hash] ["small-merge" op-hm-merge]
   ["small-get" op-small-get] ["small-assoc" op-small-assoc]
   ["hs-contains" op-hs-contains] ["hs-build" op-hs-build] ["hs-into" op-hs-into]
   ["hs-disj" op-hs-disj] ["hs-reduce" op-hs-reduce]
   ["v-conj" op-v-conj] ["v-nth" op-v-nth] ["v-assoc" op-v-assoc] ["v-pop" op-v-pop]
   ["v-seq" op-v-seq] ["v-reduce" op-v-reduce] ["v-into" op-v-into] ["v-eq" op-v-eq]
   ["l-conj" op-l-conj] ["l-walk" op-l-walk] ["l-reduce" op-l-reduce]])

;; a collection first, so one op's garbage is not collected on the next op's clock
(defn- time-ms [f state]
  (System/gc)
  (let [t0 (System/nanoTime) r (f state)]
    [(/ (- (System/nanoTime) t0) 1000000.0) r]))

(defn- r1 [x] (/ (Math/round (* x 10.0)) 10.0))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 20000)
        state (build n)
        small (build (quot n 4))
        runs 3]
    (doseq [[_ f] ops] (dotimes [_ 3] (f small)))        ; warmup
    (let [per-op (mapv (fn [[nm f]]
                         (let [ts (mapv (fn [_] (time-ms f state)) (range runs))]
                           [nm (mapv first ts) (second (first ts))]))
                       ops)
          totals (apply mapv + (map second per-op))
          mean (/ (reduce + totals) runs)]
      (doseq [[nm mss r] per-op]
        (println (format "  %-13s %8.2f ms  (result %s)" nm (apply min mss) r)))
      (println "coll-ops n" n "result" (reduce + (map #(nth % 2) per-op)))
      (println "runs:" (mapv r1 totals))
      (println "mean:" (r1 mean) "ms"))))
