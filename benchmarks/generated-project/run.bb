#!/usr/bin/env bb
(ns generated-project.runner
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def usage "Usage: bb run.bb PROJECT [INPUT [ITERATIONS]] [--warmup N] [--samples N]")

(defn options [[dir & args]]
  (when-not dir (throw (ex-info usage {})))
  (loop [[arg & more] args opts {:dir dir :args [] :warmup 0 :samples 1}]
    (cond
      (nil? arg) opts
      (#{"--warmup" "--samples"} arg)
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

(defn sample [dir command {:keys [args warmup samples]} expected]
  (let [argv (into (conj command "measure.clj") args)
        start (System/nanoTime)
        child (process/process {:cmd argv :dir dir :in :pipe :err :inherit :shutdown process/destroy-tree})]
    (try
      (with-open [out (io/reader (:out child))]
        (let [ready (.readLine out)
              load-ms (/ (- (System/nanoTime) start) 1e6)]
          (when-not (= "workload-ready" ready)
            (throw (ex-info (str "Missing readiness marker, received " (pr-str ready)) {})))
          (let [times (with-open [in (io/writer (:in child))]
                        (reduce (fn [times i]
                                  (.write in "run\n")
                                  (.flush in)
                                  (let [[checksum ms] (read-sample out)]
                                    (when-not (= expected checksum)
                                      (throw (ex-info (str "MISMATCH on run " (inc i) ", got " checksum
                                                           ", expected " expected) {})))
                                    (if (< i warmup) times (conj times ms))))
                                [] (range (+ warmup samples))))]
            (when (.readLine out) (throw (ex-info "Unexpected extra benchmark output" {})))
            (when-not (zero? (:exit @child)) (throw (ex-info (str "Process exited " (:exit @child)) {})))
            {:load-ms load-ms :times times})))
      (finally (when (.isAlive (:proc child)) (process/destroy-tree child))))))

(defn median [xs]
  (let [ordered (vec (sort xs)) n (count xs)]
    (/ (+ (ordered (quot (dec n) 2)) (ordered (quot n 2))) 2.0)))

(defn main [argv]
  (let [{:keys [dir args warmup samples] :as opts} (options argv)
        dir (str (fs/canonicalize dir))
        expected (parse-long (.trim (:out (apply process/shell {:dir dir :out :string}
                                                 "bb" "oracle.bb" args))))]
    (when-not (integer? expected) (throw (ex-info "Oracle did not return an integer" {})))
    (println "Oracle checksum" expected)
    (printf "Warmups %d, measured samples %d per process%n" warmup samples)
    (printf "%-13s %16s %16s %10s%n" "Runtime" "Startup/load ms" "Exec median ms" "Checksum")
    (flush)
    (let [ok (mapv (fn [[runtime command]]
                     (try
                       (let [{:keys [load-ms times]} (sample dir command opts expected)]
                         (printf "%-13s %16.3f %16.3f %10d%s%n" runtime load-ms (median times) expected
                                 (if (> samples 1)
                                   (str "  samples ms [" (str/join " " (map #(format "%.3f" %) times)) "]") ""))
                         (flush)
                         true)
                       (catch Exception e
                         (binding [*out* *err*] (println runtime "failed:" (ex-message e)))
                         false)))
                   (runtimes dir))]
      (when-not (every? true? ok) (System/exit 1)))))

(try (main *command-line-args*)
     (catch Exception e
       (binding [*out* *err*] (println (ex-message e)))
       (System/exit 1)))
