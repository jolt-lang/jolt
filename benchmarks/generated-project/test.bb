#!/usr/bin/env bb
(ns generated-project.test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def kit (fs/parent (fs/absolutize *file*)))
(load-file (str (fs/path kit "oracle.bb")))

;; Include a leading zero and a suffix that already occurs in the magnitude.
(let [evaluate (resolve 'generated-project.oracle/ev)]
  (doseq [[x salt expected] [[12 12 16] [1245 12 123318]]]
    (assert (= expected (evaluate {:op :java-mix :x {:op :lit :v x} :salt salt} {})))))

(defn files [root]
  (into {} (for [path (fs/glob root "**" {:hidden true}) :when (fs/regular-file? path)]
             [(str (fs/relativize root path)) (slurp (str path))])))

(defn compare-project [project]
  (process/shell {:dir (str project) :out :string :continue true}
                 "bb" "run.bb" "." "42" "3"))

(fs/with-temp-dir [tmp {}]
  (let [a (fs/path tmp "first project")
        b (fs/path tmp "second project")]
    (doseq [dir [a b]]
      (process/shell {:out :string} "bb" (str (fs/path kit "generate.bb"))
                     (str dir) "--seed" "42" "--lines" "5k"))
    (assert (= (files a) (files b)))
    (let [manifest (edn/read-string (slurp (fs/file a "manifest.edn")))]
      (assert (every? #(str/ends-with? (:path %) ".clj") (:files manifest)))
      (assert (= (:worker-namespaces manifest) (get-in manifest [:operation-counts :live :java-mix]))))
    (let [{:keys [out exit]} (compare-project a)]
      (print out)
      (assert (zero? exit) "All three runtimes must match the model"))
    ;; All three implementations will agree on the corrupted program. The
    ;; independent model must still reject every result.
    (spit (fs/file b "src/workload/main.clj")
          "\n(def original-run run)\n(defn run [x n] (inc (original-run x n)))\n"
          :append true)
    (let [{:keys [out exit]} (compare-project b)]
      (assert (and (not (zero? exit)) (= 3 (count (re-seq #"MISMATCH" out))))
              (str "Expected a checksum mismatch for every runtime.\n" out)))))
(println "Portability and mismatch checks passed.")
