#!/usr/bin/env bb
(ns generated-project.runner
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]))

(def runtimes
  [["Jolt" ["jolt"]]
   ["JVM Clojure" ["clojure" "-Srepro" "-M"]]
   ["Babashka" ["bb" "-cp" "src"]]])

(defn sample [dir command args]
  (let [argv (into (conj command "measure.clj") args)
        start (System/nanoTime)
        child (process/process {:cmd argv :dir dir :in :pipe :err :inherit :shutdown process/destroy-tree})]
    (try
      (with-open [out (io/reader (:out child))]
        (let [ready (.readLine out)
              load-ns (- (System/nanoTime) start)]
          (when-not (= "workload-ready" ready)
            (throw (ex-info (str "Missing readiness marker, received " (pr-str ready)) {})))
          (with-open [in (io/writer (:in child))] (.write in "run\n"))
          (let [line (.readLine out)
                [_ checksum nanos] (re-matches #"\[(\d+) (\d+)\]" (or line ""))]
            (when-not (and checksum (nil? (.readLine out)))
              (throw (ex-info (str "Invalid measurement " (pr-str line)) {})))
            (when-not (zero? (:exit @child)) (throw (ex-info (str "Process exited " (:exit @child)) {})))
            {:checksum (parse-long checksum) :load-ms (/ load-ns 1e6) :exec-ms (/ (parse-long nanos) 1e6)})))
      (finally (when (.isAlive (:proc child)) (process/destroy-tree child))))))

(defn main [[dir & args]]
  (when (or (nil? dir) (> (count args) 2))
    (throw (ex-info "Usage: bb run.bb PROJECT [INPUT [ITERATIONS]]" {})))
  (let [dir (str (fs/canonicalize dir))
        ;; Run the project's own oracle once, outside all measured intervals.
        expected (parse-long (.trim (:out (apply process/shell {:dir dir :out :string}
                                                 "bb" "oracle.bb" args))))]
    (when-not (integer? expected) (throw (ex-info "Oracle did not return an integer" {})))
    (println "Oracle checksum" expected)
    (printf "%-13s %16s %15s %10s%n" "Runtime" "Startup/load ms" "Execution ms" "Checksum")
    (flush)
    (let [ok (mapv (fn [[runtime command]]
                     (try
                       (let [{:keys [checksum load-ms exec-ms]} (sample dir command args)]
                         (printf "%-13s %16.3f %15.3f %10d%s%n" runtime load-ms exec-ms checksum
                                 (if (= checksum expected) "" "  MISMATCH"))
                         (flush)
                         (= checksum expected))
                       (catch Exception e
                         (binding [*out* *err*] (println runtime "failed:" (ex-message e)))
                         false)))
                   runtimes)]
      (when-not (every? true? ok) (System/exit 1)))))

(try (main *command-line-args*)
     (catch Exception e
       (binding [*out* *err*] (println (ex-message e)))
       (System/exit 1)))
