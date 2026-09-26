;; Per-test timing of the library-conformance suites on jolt against JVM Clojure.
;; Runs each manifest library's suites on both hosts through the portable
;; runner (runner/lib_conformance_timing.clj) and lists the tests that are more
;; than THRESHOLD times slower on jolt. Everything it measured goes to
;; target/libperf/report.edn and report.tsv.
;;
;; The JVM classpath comes from the library's own build files: the :dependencies
;; of its project.clj with the :dev and :test profiles, and the :deps of its
;; deps.edn with the :dev and :test aliases (deps.edn wins a coordinate both
;; name). Clojure itself is pinned (clojure-version below).
;; What jolt swaps in does not go on the JVM side: `~` first-party jolt libraries,
;; `!` shims and :local-deps are jolt's stand-ins for things the JVM has. A
;; library whose JVM recipe needs more says so under :jvm in the manifest:
;;   :jvm {:paths [...]        replaces :paths
;;         :extra-deps {...}    added coordinates
;;         :exclude-deps [...]  coordinates dropped from the build file's
;;         :skip "why"}         not run on the JVM
;;
;; Usage (the Makefile's `make libperf` passes JOLT_BIN):
;;   jolt run test/conformance/libs/timing.clj [lib ...]
;; Env: JOLT_LIBPERF_THRESHOLD (5), JOLT_LIBPERF_REPS (3),
;;      JOLT_LIBPERF_FLOOR_MS (1: a test faster than this on jolt is not flagged),
;;      JOLT_LIBPERF_TIMEOUT (seconds per library per host, 900).
;;
;; Times are the minimum over the reps: steady state on both hosts. The first
;; rep is kept in the report too; on the JVM it carries the JIT warmup.
(ns lib-conformance-timing-driver
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [jolt.process :as p]))

(load-file (str (or (System/getenv "JOLT_REPO_ROOT") (System/getProperty "user.dir"))
                "/test/conformance/libs/common.clj"))
(alias 'c 'lib-conformance-common)

(def ^:private clojure-version "1.12.4")

(defn- env-num [k default]
  (if-let [v (System/getenv k)] (parse-double v) default))

(def ^:private threshold (env-num "JOLT_LIBPERF_THRESHOLD" 5.0))
(def ^:private reps (long (env-num "JOLT_LIBPERF_REPS" 3)))
(def ^:private floor-ms (env-num "JOLT_LIBPERF_FLOOR_MS" 1.0))
(def ^:private timeout-s (long (env-num "JOLT_LIBPERF_TIMEOUT" 900)))

(def ^:private outdir (str c/repo-root "/target/libperf"))

;; ---------------------------------------------------------------- runner output

(defn- parse-output [out]
  (reduce
    (fn [acc line]
      (let [ws (str/split (str/trim line) #"\s+")]
        (case (first ws)
          "LOAD" (if (= 3 (count ws))
                   (assoc-in acc [:load (nth ws 1)] (parse-double (nth ws 2)))
                   acc)
          "LOAD-FAIL" (update acc :load-fail (fnil conj []) (nth ws 1 "?"))
          "TEST" (if (= 7 (count ws))
                   (let [[_ k first-ms min-ms pa f e] ws]
                     (assoc-in acc [:tests k]
                               {:first (parse-double first-ms) :min (parse-double min-ms)
                                :tally [(parse-long pa) (parse-long f) (parse-long e)]}))
                   acc)
          "DONE" (assoc acc :done true)
          "TIMEOUT" (assoc acc :timeout true)
          acc)))
    {}
    (str/split-lines out)))

(defn- run-cmd [dir env cmd]
  (let [r (apply p/sh {:out :string :err :string :dir dir :extra-env env} cmd)]
    (str (:out r) (:err r))))

(defn- runner-args [preload nses]
  (concat ["-m" "lib-conformance-timing" (str (* 1000 timeout-s)) (str reps)
           (if (seq preload) (str/join "," preload) "-")]
          nses))

;; ---------------------------------------------------------------- jolt side

(defn- run-jolt [{:keys [preload] :as entry} {:keys [cmd-prefix nses dir env]}]
  (run-cmd dir env (vec (concat cmd-prefix (runner-args preload nses)))))

;; ---------------------------------------------------------------- JVM side

(defn- lein-coords
  "project.clj [group/artifact \"version\" & opts] vectors as deps.edn coordinates."
  [deps]
  (into {}
        (keep (fn [d]
                (when (and (vector? d) (symbol? (first d)) (string? (second d)))
                  (let [[a v & opts] d
                        a (if (namespace a) a (symbol (str a) (str a)))
                        {:keys [exclusions]} (apply hash-map opts)]
                    [a (cond-> {:mvn/version v}
                         (seq exclusions)
                         (assoc :exclusions (mapv #(if (vector? %) (first %) %) exclusions)))]))))
        deps))

(defn- read-project-clj [f]
  (let [form (try (binding [*read-eval* false] (read-string (slurp f)))
                  (catch Exception _ nil))]
    (when (and (seq? form) (= 'defproject (first form)))
      (let [kvs (apply hash-map (drop 3 form))
            profiles (:profiles kvs)
            prof-deps (mapcat #(get-in profiles [% :dependencies]) [:dev :test])]
        {:deps (lein-coords (concat (:dependencies kvs) prof-deps))}))))

(defn- absolutize-local-roots [lib-root deps]
  (into {} (map (fn [[k v]]
                  [k (if-let [r (:local/root v)]
                       (assoc v :local/root (if (str/starts-with? r "/") r (str lib-root "/" r)))
                       v)])
                deps)))

(defn- build-file-deps
  "Every coordinate the library's builds name for running its tests: project.clj's
  :dependencies with its :dev and :test profiles, then deps.edn's :deps with its
  :dev and :test aliases over those. Either file may be the one that lists a test
  dependency, so both are read."
  [lib-root]
  (let [de (java.io.File. (str lib-root "/deps.edn"))
        pc (java.io.File. (str lib-root "/project.clj"))
        from-edn (when (.exists de)
                   (when-let [m (try (edn/read-string (slurp de)) (catch Exception _ nil))]
                     (apply merge (:deps m)
                            (map #(get-in m [:aliases % :extra-deps]) [:dev :test]))))
        from-lein (when (.exists pc) (:deps (read-project-clj pc)))]
    (absolutize-local-roots lib-root (merge from-lein from-edn))))

(defn- jvm-classpath
  "Resolve the JVM classpath for ENTRY in its own scratch dir, so the tools.deps
  cache never lands in the library checkout."
  [{:keys [name paths deps extra-deps jvm] :as entry}]
  (let [lib-root (c/lib-root entry)
        ;; spec.alpha's source cannot go on the JVM classpath: clojure.core loads
        ;; the AOT'd spec.alpha that Clojure depends on at startup, and a source
        ;; copy ahead of it redefines the specs half way and fails the boot.
        jvm-portable? (fn [p] (not (or (str/starts-with? p "~") (str/starts-with? p "!")
                                       (str/starts-with? p "@spec.alpha"))))
        roots (concat (:paths jvm (or paths ["src" "test"]))
                      (filter jvm-portable? (or deps [])))
        dir (str outdir "/" name)
        coords (-> (apply dissoc (build-file-deps lib-root) (:exclude-deps jvm))
                   (merge extra-deps (:extra-deps jvm))
                   (assoc 'org.clojure/clojure {:mvn/version clojure-version}))
        sdeps {:paths (vec (cons (str c/here "/runner")
                                 (map #(c/resolve-path lib-root %) roots)))
               :deps coords}]
    (.mkdirs (java.io.File. dir))
    (spit (str dir "/deps.edn") (pr-str sdeps))
    ;; `clojure` has been seen to block before it ever execs java; bound it.
    (let [r (p/sh {:out :string :err :string :dir dir}
                  "timeout" "600" "clojure" "-Spath")]
      (if (zero? (:exit r))
        {:cp (str/trim (:out r))}
        {:error (str "clojure -Spath failed: " (str/trim (str (:out r) (:err r))))}))))

(defn- run-jvm [{:keys [preload jvm] :as entry} {:keys [nses dir]}]
  (if (:skip jvm)
    {:error (str "skipped on the JVM: " (:skip jvm))}
    (let [{:keys [cp error]} (jvm-classpath entry)]
      (if error
        {:error error}
        {:out (run-cmd dir {} (vec (concat ["java" "-cp" cp "clojure.main"]
                                           (runner-args (remove #(str/starts-with? % "jolt") preload)
                                                        nses))))}))))

;; ---------------------------------------------------------------- report

(defn- join-tests [lib jolt jvm]
  (for [[k jt] (:tests jolt)
        :let [vt (get-in jvm [:tests k])]
        :when vt]
    {:lib lib :test k
     :jolt-ms (:min jt) :jvm-ms (:min vt)
     :jolt-first-ms (:first jt) :jvm-first-ms (:first vt)
     :ratio (/ (:min jt) (max (:min vt) 0.001))
     :jolt-tally (:tally jt) :jvm-tally (:tally vt)}))

(defn- fmt-ms [x] (format "%.1f" (double x)))

(defn- lib-line [name jolt jvm rows]
  (let [sum #(reduce + 0.0 (map % rows))
        jt (sum :jolt-ms) vt (sum :jvm-ms)
        load-sum (fn [r] (reduce + 0.0 (vals (:load r))))]
    (format "%-20s tests=%d(jolt %d, jvm %d)  run %sms vs %sms (%.1fx)  load %sms vs %sms%s"
            name (count rows) (count (:tests jolt)) (count (:tests jvm))
            (fmt-ms jt) (fmt-ms vt) (/ jt (max vt 0.001))
            (fmt-ms (load-sum jolt)) (fmt-ms (load-sum jvm))
            (str (when (:timeout jolt) "  JOLT-TIMEOUT")
                 (when (:timeout jvm) "  JVM-TIMEOUT")
                 (when (seq (:load-fail jolt)) (str "  jolt-load-fail=" (count (:load-fail jolt))))
                 (when (seq (:load-fail jvm)) (str "  jvm-load-fail=" (count (:load-fail jvm))))))))

(defn -main [& args]
  (when-not (c/exists? c/libs-root)
    (println (str "FAIL: no library checkout at " c/libs-root " (set JOLT_CONFORMANCE_LIBS)"))
    (System/exit 1))
  (.mkdirs (java.io.File. outdir))
  (let [manifest (edn/read-string (slurp (str c/here "/manifest.edn")))
        wanted (set args)
        entries (cond->> (remove :skip (:libs manifest))
                  (seq wanted) (filter #(wanted (:name %))))
        results
        (doall
          (for [{:keys [name] :as e} entries
                :let [pl (c/plan e)]]
            (cond
              (:missing pl) (do (println (format "%-20s MISSING %s" name (str/join " " (:missing pl))))
                                {:name name})
              (:no-tests pl) (do (println (format "%-20s NO-TESTS" name)) {:name name})
              :else
              (let [jolt-out (run-jolt e pl)
                    _ (spit (str outdir "/" name ".jolt.log") jolt-out)
                    jvm-r (run-jvm e pl)
                    _ (spit (str outdir "/" name ".jvm.log") (or (:out jvm-r) (:error jvm-r)))
                    jolt (parse-output jolt-out)
                    jvm (if (:out jvm-r) (parse-output (:out jvm-r)) {})
                    rows (join-tests name jolt jvm)]
                (if (:error jvm-r)
                  (println (format "%-20s JVM-ERROR %s" name
                                   (subs (:error jvm-r) 0 (min 160 (count (:error jvm-r))))))
                  (println (lib-line name jolt jvm rows)))
                (flush)
                {:name name :rows rows
                 :jolt-load (:load jolt) :jvm-load (:load jvm)}))))
        rows (mapcat :rows results)
        flagged (->> rows
                     (filter #(and (>= (:ratio %) threshold) (>= (:jolt-ms %) floor-ms)))
                     (sort-by :ratio >))]
    (println)
    (println (format "%d tests timed on both hosts; %d are >= %.0fx slower on jolt (and >= %sms):"
                     (count rows) (count flagged) threshold (fmt-ms floor-ms)))
    (doseq [{:keys [lib test jolt-ms jvm-ms ratio jolt-tally jvm-tally]} flagged]
      (println (format "  %7.1fx  %10sms %9sms  %-18s %s%s"
                       (double ratio) (fmt-ms jolt-ms) (fmt-ms jvm-ms) lib test
                       (if (= jolt-tally jvm-tally) ""
                           (str "  (tally jolt " jolt-tally " jvm " jvm-tally ")")))))
    (spit (str outdir "/report.edn")
          (pr-str {:threshold threshold :reps reps :floor-ms floor-ms
                   :clojure clojure-version
                   :libs (mapv #(dissoc % :rows) results)
                   :tests (vec rows)}))
    (spit (str outdir "/report.tsv")
          (str/join "\n"
                    (cons "lib\ttest\tjolt_ms\tjvm_ms\tratio\tjolt_first_ms\tjvm_first_ms\tjolt_tally\tjvm_tally"
                          (for [r (sort-by :ratio > rows)]
                            (str/join "\t" [(:lib r) (:test r) (fmt-ms (:jolt-ms r)) (fmt-ms (:jvm-ms r))
                                            (format "%.2f" (double (:ratio r)))
                                            (fmt-ms (:jolt-first-ms r)) (fmt-ms (:jvm-first-ms r))
                                            (str/join "/" (:jolt-tally r)) (str/join "/" (:jvm-tally r))])))))
    (println "wrote" (str outdir "/report.edn") "and report.tsv")
    (System/exit 0)))

(apply -main *command-line-args*)
