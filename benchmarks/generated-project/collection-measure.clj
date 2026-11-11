(require '[workload.collections :as workload]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def profiles (:profiles (edn/read-string (slurp "manifest.edn"))))

(defn prepare [{:keys [op size stride offset bulk]} input budget]
  (let [kind (second (str/split op #"/"))
        items (case kind
                "map" (mapv (fn [k] [k (inc k)]) (range size))
                "vector" (mapv inc (range size))
                "set" (vec (range size))
                nil)
        base (case kind "map" (into {} items) "set" (set items) "vector" items nil)
        calls (if bulk (max 1 (quot budget size)) budget)
        probes (long-array calls)
        f (workload/kernels op)]
    (loop [i 0 k (mod (+ input offset) size)]
      (when (< i calls)
        (aset-long probes i k)
        (recur (inc i) (mod (+ k stride) size))))
    (fn [] (f base items size probes calls))))

(let [[input budget] (mapv parse-long *command-line-args*)]
  (println "workload-ready")
  (flush)
  (loop [batch nil]
    (when-let [signal (read-line)]
      (cond
        (str/starts-with? signal "prepare ")
        (let [f (prepare (nth profiles (parse-long (subs signal 8))) input budget)]
          (println "prepared")
          (flush)
          (recur f))
        (= signal "run")
        (let [start (System/nanoTime)
              result (batch)
              elapsed (- (System/nanoTime) start)]
          (prn [result elapsed])
          (flush)
          (recur batch))
        :else (throw (ex-info "Unknown runner signal" {:signal signal}))))))
