(require '[workload.main :as workload])

;; The runner stops its startup/load clock before releasing the workload.
(let [[input iterations] (workload/arguments *command-line-args*)]
  (println "workload-ready")
  (flush)
  (when-not (= "run" (read-line))
    (throw (ex-info "Expected run signal from the benchmark runner" {})))
  (let [start (System/nanoTime)
        checksum (workload/run input iterations)
        elapsed (- (System/nanoTime) start)]
    (prn [checksum elapsed])))
