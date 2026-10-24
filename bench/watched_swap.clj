;; watched-swap — a disk-backed atom: every `swap!` fires a watch that prints
;; the new state with `pr-str` and writes it to a file with `spit`. The shape of
;; a persisted state domain (duratom and friends), where each update pays the
;; swap, the watch dispatch, the printer and a small file write.
;;
;; What it watches: watch notification on swap!, pr-str of a small nested map,
;; and the open/write/close round trip through the java.io shim. The file write
;; is the same syscalls on both hosts, so the gap is the jolt side of the trip.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh watched-swap 4000
(ns watched-swap)

(def ^:private path
  (str (System/getProperty "java.io.tmpdir") "/jolt-bench-watched-swap.edn"))

(defn run [n]
  (let [a (atom {:count 0 :user {:name "ada" :roles #{:admin :dev}} :items [1 2 3]})]
    (add-watch a :persist (fn [_ _ _ new] (spit path (pr-str new))))
    (dotimes [i n] (swap! a update :count + i))
    (remove-watch a :persist)
    (+ (:count @a) (count (slurp path)))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 4000)]
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
      (println "watched-swap n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
