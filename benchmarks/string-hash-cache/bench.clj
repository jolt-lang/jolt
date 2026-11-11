;; One working set per Jolt process. Each pass visits the same string objects in order.
(defn hash-batch [accounts passes]
  (loop [remaining passes total 0]
    (if (zero? remaining)
      total
      (recur (dec remaining)
             (reduce (fn [sum account] (+ sum (hash account))) total accounts)))))

(let [args (mapv parse-long *command-line-args*)]
  (when-not (and (= 3 (count args)) (every? #(and % (pos? %)) args))
    (throw (ex-info "Usage: jolt bench.clj KEY-COUNT PASSES BATCHES" {})))
  (let [[n passes batches] args
        accounts (mapv #(str "account-" (+ 1000000 %)) (range n))]
    (prn (mapv (fn [_]
                 (let [start (System/nanoTime)
                       checksum (hash-batch accounts passes)
                       elapsed (- (System/nanoTime) start)]
                   [checksum elapsed]))
               (range batches)))))
