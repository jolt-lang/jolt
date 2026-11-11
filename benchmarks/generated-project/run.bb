#!/usr/bin/env bb
(ns generated-project.runner
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def usage "Usage: bb run.bb PROJECT [INPUT [ITERATIONS]] [--warmup N] [--samples N] [--launches N] [--csv FILE]")

(defn options [[dir & args]]
  (when-not dir (throw (ex-info usage {})))
  (loop [[arg & more] args opts {:dir dir :args [] :warmup 0 :samples 1 :launches 1}]
    (cond
      (nil? arg) opts
      (= "--csv" arg)
      (if (seq (first more)) (recur (rest more) (assoc opts :csv (first more))) (throw (ex-info usage {})))
      (#{"--warmup" "--samples" "--launches"} arg)
      (let [n (parse-long (or (first more) ""))]
        (when-not (and n (>= n (if (= arg "--warmup") 0 1))) (throw (ex-info usage {})))
        (recur (rest more) (assoc opts (keyword (subs arg 2)) n)))
      (or (str/starts-with? arg "--") (= 2 (count (:args opts)))) (throw (ex-info usage {}))
      :else (recur more (update opts :args conj arg)))))

(defn runtimes [dir]
  (let [deps (edn/read-string (slurp (fs/file dir "deps.edn")))]
    [["Jolt" ["jolt"]]
     ["JVM Clojure" ["clojure" "-Srepro" (if (get-in deps [:aliases :bench-jvm]) "-M:bench-jvm" "-M")]]
     ["Babashka" ["bb" "-cp" "src"]]]))

(defn read-sample [out]
  (let [line (.readLine out)
        [_ checksum nanos] (re-matches #"\[(\d+) (\d+)\]" (or line ""))]
    (when-not checksum (throw (ex-info (str "Invalid measurement " (pr-str line)) {})))
    [(parse-long checksum) (/ (parse-long nanos) 1e6)]))

(defn measure-profile [in out {:keys [warmup samples]} {:keys [index checksum op size] :as profile}]
  (when (some? index)
    (.write in (str "prepare " index "\n"))
    (.flush in)
    (when-not (= "prepared" (.readLine out))
      (throw (ex-info (str "Failed to prepare " op " at size " size) {}))))
  (assoc profile :times
         (reduce (fn [times i]
                   (.write in "run\n")
                   (.flush in)
                   (let [[actual ms] (read-sample out)]
                     (when-not (= checksum actual)
                       (throw (ex-info (str "MISMATCH on run " (inc i) " " op " " size
                                            ", got " actual ", expected " checksum) {})))
                     (if (< i warmup) times (conj times ms))))
                 [] (range (+ warmup samples)))))

(defn sample [dir command {:keys [args] :as opts} profiles]
  (let [argv (into (conj command "measure.clj") args)
        start (System/nanoTime)
        child (process/process {:cmd argv :dir dir :in :pipe :err :inherit :shutdown process/destroy-tree})]
    (try
      (with-open [out (io/reader (:out child))]
        (let [ready (.readLine out)
              load-ms (/ (- (System/nanoTime) start) 1e6)]
          (when-not (= "workload-ready" ready)
            (throw (ex-info (str "Missing readiness marker, received " (pr-str ready)) {})))
          (let [results (with-open [in (io/writer (:in child))]
                          (mapv #(measure-profile in out opts %) profiles))]
            (when (.readLine out) (throw (ex-info "Unexpected extra benchmark output" {})))
            (when-not (zero? (:exit @child)) (throw (ex-info (str "Process exited " (:exit @child)) {})))
            {:load-ms load-ms :results results})))
      (finally (when (.isAlive (:proc child)) (process/destroy-tree child))))))

(defn median [xs]
  (let [ordered (vec (sort xs)) n (count xs)]
    (/ (+ (ordered (quot (dec n) 2)) (ordered (quot n 2))) 2.0)))

(defn ordered-profiles [profiles launch]
  (let [ordered (java.util.ArrayList. profiles)]
    (java.util.Collections/shuffle ordered (java.util.Random. (long launch)))
    (vec ordered)))

(defn report-result [csv launch runtime load-ms {:keys [op size calls times checksum]}]
  (let [mid (median times)]
    (when op (printf "%-37s %7d " op size))
    (printf "%-13s %16.3f %16.3f %10d%s%n" runtime load-ms mid checksum
            (if (> (count times) 1)
              (str "  samples ms [" (str/join " " (map #(format "%.3f" %) times)) "]") ""))
    (when csv
      (spit csv (str (str/join "," (concat [launch runtime op size calls checksum load-ms mid] times))
                     "\n") :append true))
    {:op op :size size :runtime runtime :ms mid}))

(defn run-launch [dir opts profiles launch]
  (let [commands (runtimes dir)
        offset (mod (dec launch) (count commands))
        profiles (ordered-profiles profiles launch)
        run (fn [[runtime command]]
              (try
                (let [{:keys [load-ms results]} (sample dir command opts profiles)
                      medians (mapv #(report-result (:csv opts) launch runtime load-ms %) results)]
                  (flush)
                  medians)
                (catch Exception e
                  (binding [*out* *err*] (println runtime "failed:" (ex-message e)))
                  nil)))
        results (mapv run (concat (drop offset commands) (take offset commands)))]
    (when (some nil? results) (throw (ex-info "Benchmark launch failed" {:launch launch})))
    (into [] cat results)))

(defn report-comparison [results]
  (println "\nWarm execution, median of process medians in ms per batch")
  (printf "%-37s %7s %10s %10s %10s %10s %10s%n" "Operation" "Size" "Jolt" "Babashka" "JVM" "Jolt/bb" "Jolt/JVM")
  (let [ratio (fn [a b] (if (pos? b) (format "%.2fx" (/ a b)) "n/a"))]
    (doseq [[[op size] rows] (sort-by key (group-by (juxt :op :size) results))]
      (let [times (into {} (for [[rt samples] (group-by :runtime rows)] [rt (median (map :ms samples))]))
            {j "Jolt" bb "Babashka" jvm "JVM Clojure"} times]
        (printf "%-37s %7d %10.3f %10.3f %10.3f %10s %10s%n" op size j bb jvm (ratio j bb) (ratio j jvm))))))

(defn main [argv]
  (let [{:keys [dir args warmup samples launches csv] :as opts} (options argv)
        dir (str (fs/canonicalize dir))
        manifest (edn/read-string (slurp (fs/file dir "manifest.edn")))
        matrix? (= :collection-sizes-v1 (:format manifest))
        args (if matrix? [(get args 0 "1") (get args 1 "32768")] args)
        expected (edn/read-string (:out (apply process/shell {:dir dir :out :string} "bb" "oracle.bb" args)))
        profiles (if matrix?
                   (mapv (fn [i profile answer] (assoc (merge profile answer) :index i))
                         (range) (:profiles manifest) expected)
                   [{:checksum expected}])]
    (when-not (and (seq profiles) (every? #(integer? (:checksum %)) profiles)
                   (or (not matrix?) (= (count expected) (count (:profiles manifest)))))
      (throw (ex-info "Oracle returned incomplete checksums" {})))
    (when-not matrix? (println "Oracle checksum" expected))
    (printf "Warmups %d, measured samples %d per process%n" warmup samples)
    (when matrix? (printf "%-37s %7s " "Operation" "Size"))
    (printf "%-13s %16s %16s %10s%n" "Runtime" "Startup/load ms" "Exec median ms" "Checksum")
    (when csv
      (spit csv (str "launch,runtime,operation,size,calls,checksum,startup_ms,exec_median_ms,"
                     (str/join "," (map #(str "sample_" % "_ms") (range 1 (inc samples)))) "\n")))
    (flush)
    (let [results (into [] (mapcat (fn [launch]
                                     (when (> launches 1) (println "Launch" launch))
                                     (run-launch dir (assoc opts :args args) profiles launch)))
                        (range 1 (inc launches)))]
      (when matrix? (report-comparison results)))))

(try (main *command-line-args*)
     (catch Exception e
       (binding [*out* *err*] (println (ex-message e)))
       (System/exit 1)))
