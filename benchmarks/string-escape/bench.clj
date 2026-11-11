(require '[clojure.string :as string])

(def replacements {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"})
(def alphabet "az09_bcDéß東京")
(def specials "&<>\"")
(def scenarios
  [["short-sparse" 32 32 replacements]
   ["no-match" 256 0 replacements]
   ["sparse" 256 32 replacements]
   ["dense" 256 2 replacements]
   ["long-sparse" 4096 32 replacements]
   ["fn-sparse" 256 32 (fn [c] (replacements c))]])

(defn input [size every variant]
  (let [buf (StringBuilder.)]
    (dotimes [i size]
      (.append buf
               (if (and (pos? every) (zero? (mod (+ i variant) every)))
                 (.charAt specials (mod (+ (quot i every) variant) 4))
                 (.charAt alphabet (mod (+ i variant (quot variant 7)) (count alphabet))))))
    (.toString buf)))

(defn oracle [^String s]
  (-> s (.replace "&" "&amp;") (.replace "<" "&lt;")
        (.replace ">" "&gt;") (.replace "\"" "&quot;")))

(defn run-batch [inputs iterations transform]
  (loop [i 0 checksum 0]
    (if (< i iterations)
      (let [^String result (transform (nth inputs (bit-and i 63)))]
        (recur (inc i) (+ checksum (.length result) (int (.charAt result (bit-and i 15))))))
      checksum)))

(defn measure [[label size every cmap]]
  (let [inputs (mapv #(input size every %) (range 64))
        expected (mapv oracle inputs)
        iterations (quot 524288 size)
        expected-checksum (run-batch expected iterations identity)
        escape #(string/escape % cmap)]
    (when-not (= expected (mapv escape inputs))
      (throw (ex-info "Escaped text differs from the oracle" {:profile label})))
    (dotimes [n 17]
      (let [start (System/nanoTime)
            result (run-batch inputs iterations escape)
            elapsed (- (System/nanoTime) start)]
        (when-not (= expected-checksum result)
          (throw (ex-info "Checksum mismatch" {:profile label :iteration n})))
        (when (>= n 10)
          (println label result elapsed))))))

(println "ready")
(flush)
(doseq [scenario scenarios] (measure scenario))
