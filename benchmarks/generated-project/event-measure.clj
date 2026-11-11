(require '[workload.event-index :as workload]
         '[clojure.edn :as edn])

(println "workload-ready")
(flush)

(let [{:keys [ingest updates queries]} (edn/read-string (slurp "events.edn"))
      expected (edn/read-string (read-line))]
  (loop []
    (when-let [signal (read-line)]
      (when-not (= "run" signal) (throw (ex-info "Unknown runner signal" {:signal signal})))
      (let [start (System/nanoTime)
            initial (workload/ingest ingest)
            ingested (System/nanoTime)
            index (workload/update-index initial updates)
            updated (System/nanoTime)
            answers (workload/query index queries)
            queried (System/nanoTime)]
        (prn (cond
               (not= (:index expected) index) :index-mismatch
               (not (and (vector? answers) (= (:answers expected) answers))) :query-mismatch
               :else [(- ingested start) (- updated ingested) (- queried updated)]))
        (flush))
      (recur))))
