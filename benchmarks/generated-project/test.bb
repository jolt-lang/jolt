#!/usr/bin/env jolt
(ns generated-project.test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def kit (fs/parent (fs/absolutize *file*)))
;; Loaded for their functions; their entry points must not run (see the guard
;; at the end of each).
(System/setProperty "generated-project.loading" "true")
(load-file (str (fs/path kit "generate.bb")))
(load-file (str (fs/path kit "oracle.bb")))
(alias 'gen 'generated-project.generate)
(alias 'features 'generated-project.features)
(alias 'oracle 'generated-project.oracle)

(defn generate [dir & args]
  (apply process/shell {:out :string} "jolt" (str (fs/path kit "generate.bb")) (str dir) args)
  (edn/read-string (slurp (fs/file dir "manifest.edn"))))

(defn files [root]
  (into {} (for [path (fs/glob root "**" {:hidden true}) :when (fs/regular-file? path)]
             [(str (fs/relativize root path)) (slurp (str path))])))

(defn compare-project [project & options]
  (apply process/shell {:dir (str project) :out :string :err :string :continue true}
         "jolt" "run.bb" "." "42" "3" options))

(defn check-pool [dir]
  ;; Call every emitted method directly. A random workload is deliberately
  ;; free to miss methods, and a single final checksum could mask a bad result.
  (let [ids (keys features/calls)
        inputs [[0 1] [1 997] [11111 1] [65535 2] [65536 3] [1000002 997]]
        expected (into {} (for [id ids] [id (mapv #(apply oracle/api-result id %) inputs)]))
        {:keys [path source]} (gen/api-file ids)
        file (fs/path dir path)]
    (fs/create-dirs (fs/parent file))
    (spit (str file) source)
    (spit (fs/file dir "deps.edn") (pr-str {:paths ["src"] :aliases {:bench-jvm {:extra-deps features/fs-dependency}}}))
    (spit (fs/file dir "probe.clj")
          (str "(require '[workload.api :as api])\n"
               (str/join "\n" (for [id ids :let [f (symbol "api" (str (features/call-name id)))]]
                                  (pr-str (list 'prn [id (mapv #(apply list f %) inputs)]))))))
    (doseq [launcher [["jolt"] ["bb" "-cp" "src"] ["clojure" "-Srepro" "-M:bench-jvm"]]]
      (let [out (:out (apply process/shell {:dir (str dir) :out :string} (conj launcher "probe.clj")))
            actual (into {} (map edn/read-string (str/split-lines out)))]
        (doseq [id ids]
          (assert (= (expected id) (actual id)) (pr-str [launcher id (expected id) (actual id)])))))))

(let [ops (mapcat identity (vals features/expression-groups))
      entries (map first (mapcat identity (vals gen/tables)))]
  (assert (= (frequencies ops) (frequencies entries)) "Categorize every optional expression exactly once"))
(assert (= ["babashka/fs/parent"] (features/selection ["babashka/fs/parent"] [])))
(assert (= (features/selection ["jvm"] ["jvm/math"]) (features/selection ["jvm" "jvm/math"] ["jvm/math"])))

(let [methods (fn [seed only]
                (-> (gen/project {:seed seed :lines 1000 :namespace-lines 400 :categories (features/selection only [])})
                    :model :workers first :api-methods))]
  (assert (= (methods 42 ["jvm"]) (methods 42 ["jvm" "core/control"])))
  (assert (not= (methods 42 ["jvm"]) (methods 43 ["jvm"]))))

;; A core name raw-word can spell has to be reserved on every runtime alike, or
;; the same seed generates different programs (see core-words in generate.bb).
(let [missing (remove (set gen/core-words)
                      (filter gen/spellable? (map str (keys (ns-publics 'clojure.core)))))]
  (assert (empty? missing) (str "add to core-words in generate.bb: " (vec (sort missing)))))

(fs/with-temp-dir [tmp {}]
  (let [a (fs/path tmp "first project")
        b (fs/path tmp "second project")
        opts ["--seed" "42" "--lines" "5k"]
        manifest (apply generate a opts)]
    (apply generate b opts)
    (assert (= (files a) (files b)))
    (assert (every? #(str/ends-with? (:path %) ".clj") (:files manifest)))
    (assert (> (count (set (vals (:worker-api-methods manifest)))) 1))
    (assert (= (set (mapcat val (:worker-api-methods manifest))) (set (keys (:api-call-sites manifest)))))
    (let [{:keys [out err exit]} (compare-project a "--warmup" "1" "--samples" "3")]
      (print out)
      (assert (zero? exit) err)
      (assert (= 3 (count (re-seq #"samples ms \[[0-9.]+ [0-9.]+ [0-9.]+\]" out)))))
    (check-pool (fs/path tmp "method probes"))
    ;; One wrong run, followed by correct runs, must still fail (including warmup).
    (spit (fs/file b "src/workload/main.clj")
          "\n(def original-run run)\n(def first-run (atom true))\n(defn run [x n] (let [v (original-run x n)] (if (compare-and-set! first-run true false) (inc v) v)))\n"
          :append true)
    (let [{:keys [err exit]} (compare-project b "--warmup" "1" "--samples" "2")]
      (assert (and (not (zero? exit)) (= 3 (count (re-seq #"MISMATCH on run 1" err)))) err)))
  (doseq [[label args expected-methods]
          [["one method" ["--only" "jvm/byte-buffer" "--exclude" "jvm/byte-buffer/flip"] #{"jvm/byte-buffer/get-short"}]
           ["filesystem" ["--only" "babashka/fs/parent"] #{"babashka/fs/parent"}]
           ["core only" ["--only" "core,dispatch" "--exclude" "core/state,dispatch"] #{}]]]
    (let [dir (fs/path tmp label)
          manifest (apply generate dir "--seed" "-8" "--lines" "1k" args)
          model (edn/read-string (slurp (fs/file dir "model.edn")))]
      (assert (= expected-methods (set (keys (:api-call-sites manifest)))))
      (assert (= (= label "filesystem") (boolean (:jvm-deps manifest))))
      (assert (every? empty? ((juxt :protocols :methods :macros) model)))
      (assert (empty? (:waste-markers manifest)))
      (let [{:keys [exit err]} (compare-project dir)] (assert (zero? exit) err))))
  (let [dir (fs/path tmp "typo")
        result (process/shell {:continue true :err :string} "jolt" (str (fs/path kit "generate.bb"))
                              (str dir) "--only" "jvm/typo")]
    (assert (and (pos? (:exit result)) (str/includes? (:err result) "Unknown category") (not (fs/exists? dir))))))
(println "Method pool, category selection, timing and checksum checks passed.")
