(require '[workload.main :as workload])

;; Load once. The parent stops its startup clock before sending the first run.
(let [[input iterations] (workload/arguments *command-line-args*)]
  (println "workload-ready")
  (flush)
  (loop []
    (when-let [signal (read-line)]
      (when-not (= "run" signal) (throw (ex-info "Unknown benchmark runner signal" {})))
      (let [start (System/nanoTime)
            checksum (workload/run input iterations)
            elapsed (- (System/nanoTime) start)]
        (prn [checksum elapsed])
        (flush))
      (recur))))
