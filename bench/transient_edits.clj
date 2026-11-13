;; transient-edits — a few writes to a transient of a large populated map:
;; one replacement, one removal, and two replacements of different keys, each a
;; full `transient` .. `persistent!` round trip on a 32k-entry map.
;;
;; The shape of `update-in`-style code that goes transient for a handful of
;; edits, and of `into` onto a big map with a small source. A transient that
;; claims an editable copy of every node on the write path pays for expanding
;; and then freezing that spine even for one write; the first few writes take a
;; compact path-copying edit instead, so the cost should stay near a persistent
;; `assoc`, not a bulk build. `transients` and `transient-builds` measure long
;; runs of writes, where claiming nodes is the right trade.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh transient-edits 32768
(ns transient-edits)

(defn- edit-all [m n k]
  (+ (count (persistent! (assoc! (transient m) k -1)))
     (count (persistent! (dissoc! (transient m) k)))
     (count (persistent! (-> (transient m) (assoc! k 1) (assoc! (mod (+ k 317) n) 2))))))

(defn run [n]
  (let [m (zipmap (range n) (range n))]
    (loop [i 0 total 0]
      (if (== i n) total
          (recur (inc i) (+ total (edit-all m n (mod (* 859 i) n))))))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 32768)]
    (dotimes [_ 2] (run (quot n 4)))                     ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "transient-edits n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
