#!/usr/bin/env bb
(require '[babashka.fs :as fs]
         '[babashka.process :as process]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def bench (fs/path (fs/parent (fs/absolutize *file*)) "bench.clj"))
(def target-calls 1000000)
(def warmups 5)
(def samples 7)
(def launches 3)
(def usage "Usage: bb run.bb [KEY-COUNT ...]   (default 2047 2048 2049 10000)")

(defn workload [n]
  (let [passes (quot target-calls n)
        hash-sum (reduce + 0 (map #(hash (str "account-" (+ 1000000 %))) (range n)))]
    {:n n :passes passes :calls (* passes n) :checksum (* passes hash-sum)}))

;; Both the sample count and the launch count are odd.
(defn median [xs] (nth (sort xs) (quot (count xs) 2)))

(defn measure [jolt {:keys [n passes calls checksum]}]
  (let [batches (+ warmups samples)
        result (process/shell {:cmd [jolt (str bench) (str n) (str passes) (str batches)]
                               :dir (str (fs/parent bench)) :out :string})
        rows (edn/read-string (:out result))]
    (when-not (and (vector? rows) (= batches (count rows)))
      (throw (ex-info "Incomplete Jolt measurements" {:keys n})))
    (doseq [[i row] (map-indexed vector rows)]
      (when-not (and (vector? row) (= 2 (count row)) (= checksum (first row))
                     (integer? (second row)) (pos? (second row)))
        (throw (ex-info "Invalid Jolt sample" {:keys n :batch (inc i) :expected-checksum checksum :row row}))))
    (/ (double (median (map second (drop warmups rows)))) calls)))

(defn run [args]
  (let [sizes (if (seq args) (mapv parse-long args) [2047 2048 2049 10000])]
    (when-not (every? #(and % (<= 1 % target-calls)) sizes)
      (throw (ex-info (str usage "\nKEY-COUNT must be in 1.." target-calls) {})))
    (let [binary (or (System/getenv "JOLT_BIN") "jolt")
          jolt (or (some-> (fs/which binary) fs/absolutize str)
                   (throw (ex-info (str "Cannot find Jolt executable: " binary) {})))
          cases (mapv workload (distinct sizes))]
      (println (str/trim (:out (process/shell {:cmd [jolt "--version"] :out :string}))))
      (printf "%d fresh launches per size, %d warmups and %d measured batches per launch.%n" launches warmups samples)
      (flush)
      (let [results (group-by first
                      (for [launch (range launches)
                            c (take (count cases) (drop launch (cycle cases)))]
                        [(:n c) (measure jolt c)]))
            per-launch (fn [n] (mapv second (results n)))]
        (printf "%8s %14s %15s   %s%n" "Keys" "Hashes/batch" "Median ns/hash" "Launch medians (ns/hash)")
        (doseq [{:keys [n calls]} cases]
          (let [times (per-launch n)]
            (printf "%8d %14d %15.2f   %s%n" n calls (median times)
                    (str/join "  " (map #(format "%.2f" %) times)))))
        (when (and (results 2048) (results 2049))
          (printf "2049 / 2048 elapsed time per hash: %.2fx%n"
                  (/ (median (per-launch 2049)) (median (per-launch 2048)))))))))

(if (= ["--help"] (vec *command-line-args*))
  (println usage)
  (run *command-line-args*))
