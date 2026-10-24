;; atom-ops — the state-container paths an app hits per event: a counter
;; `swap!`, a `deref` of a small map read by key, a `swap! assoc` into a
;; 1k-key map, a `get` through a deref of that map, and a deref whose value is
;; then read whole (reduce-kv over the 1k keys).
;;
;; What it watches: swap!'s compare-and-set loop and the validator/watch checks
;; around it, the deref fast path, and keyword-keyed lookup/assoc on a hash map
;; past the array-map threshold. It prints each phase's time above `mean:`, so a
;; win or loss can be pinned to the path that moved; `mean:` is the row.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh atom-ops 200000
(ns atom-ops)

(def ^:private big-keys (mapv #(keyword (str "k" %)) (range 1000)))

(defn- big-map [] (zipmap big-keys (range 1000)))

(defn counter-swap [n]
  (let [c (atom 0)]
    (dotimes [_ n] (swap! c inc))
    @c))

(defn deref-small [n]
  (let [a (atom {:x 1 :y 2 :z 3})]
    (loop [i 0 acc 0]
      (if (< i n)
        (let [m @a] (recur (inc i) (+ acc (:x m) (:y m) (:z m))))
        acc))))

(defn swap-1k [n]
  (let [a (atom (big-map))]
    (dotimes [i n] (swap! a assoc (nth big-keys (rem i 1000)) i))
    (count @a)))

(defn get-1k [n]
  (let [a (atom (big-map))]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (inc i) (+ acc (get @a (nth big-keys (rem i 1000)))))
        acc))))

;; whole-value read: n/100 derefs, each walked across its 1000 entries
(defn deref-1k [n]
  (let [a (atom (big-map))]
    (loop [i 0 acc 0]
      (if (< i (quot n 100))
        (recur (inc i) (+ acc (reduce-kv (fn [s _ v] (+ s v)) 0 @a)))
        acc))))

(def ^:private phases
  [["counter-swap" counter-swap] ["deref-small" deref-small] ["swap-1k" swap-1k]
   ["get-1k" get-1k] ["deref-1k" deref-1k]])

(defn run [n]
  (reduce (fn [acc [_ f]] (+ acc (f n))) 0 phases))

(defn- ms-since [t0] (/ (- (System/nanoTime) t0) 1000000.0))
(defn- r1 [x] (/ (Math/round (* x 10.0)) 10.0))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 200000)]
    (dotimes [_ 2] (run (quot n 4)))                     ; warmup
    (let [runs 3
          ;; the phase split, for reading by hand — `mean:` below is the row
          split (mapv (fn [[nm f]] (let [t0 (System/nanoTime)] (f n) [nm (ms-since t0)])) phases)
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)]
                          [(ms-since t0) r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "atom-ops n" n "result" (second (first times)))
      (println "phases:" (apply str (interpose "  " (map (fn [[nm ms]] (str nm " " (r1 ms) " ms")) split))))
      (println "runs:" (mapv r1 mss))
      (println "mean:" (r1 mean) "ms"))))
