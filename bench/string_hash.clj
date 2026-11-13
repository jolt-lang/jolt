;; string-hash — `hash` of strings: a 2,048-string working set scanned over
;; and over, a larger working set scanned the same way, and strings built,
;; hashed once and dropped.
;;
;; The JVM caches String.hashCode in the object. A jolt string is a plain Chez
;; string with nowhere to put it, so the hash lives in a per-thread weak table
;; that admits a string on its second sighting. The small set is the control:
;; it always fit. The large set is the regime a bounded cache lost entirely
;; (one key past the cap cleared it and every scan recomputed every hash), and
;; the one-shot strings are what admission has to keep out of the table.
;; `keyed-lookup` hashes strings too, but a handful of them inside map lookups.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh string-hash 50000
(ns string-hash)

(defn- scan [xs passes]
  (loop [p passes total 0]
    (if (zero? p) total
        (recur (dec p) (reduce (fn [s x] (+ s (hash x))) total xs)))))

(defn run [n]
  (let [small (mapv (fn [i] (str "account-" (+ 1000000 i))) (range 2048))
        large (mapv (fn [i] (str "account-" (+ 2000000 i))) (range n))
        fresh (reduce (fn [s i] (+ s (hash (str "order-" i)))) 0 (range n))]
    (+ (scan small (quot (* 8 n) 2048)) (scan large 8) fresh)))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 50000)]
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
      (println "string-hash n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
