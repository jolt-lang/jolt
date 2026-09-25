;; Library-conformance driver: replay third-party Clojure libraries' own
;; clojure.test suites on jolt and compare the tallies against the recorded
;; standings in manifest.edn.
;;
;; The library checkouts are NOT vendored — they are ordinary upstream clones
;; under $JOLT_CONFORMANCE_LIBS (default: conformance-libraries beside the main
;; jolt checkout, also from a worktree). This gate is therefore opt-in
;; (`make libconformance`) and fails when the checkout is absent. What lives in this repo is the recipe (which paths, which deps,
;; which namespaces) and the expected tally, so a jolt change that regresses a
;; library is caught here instead of being noticed months later.
;;
;; Usage:
;;   jolt run test/conformance/libs/run.clj [lib ...]      ; all, or the named ones
;;   JOLT_LIBCONF_REPORT=<file> jolt run ...               ; dump {name tally} edn
(ns lib-conformance-driver
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [jolt.process :as p]))

(load-file (str (or (System/getenv "JOLT_REPO_ROOT") (System/getProperty "user.dir"))
                "/test/conformance/libs/common.clj"))
(alias 'c 'lib-conformance-common)

(def ^:private libs-root c/libs-root)
(def ^:private here c/here)
(def ^:private repo-root c/repo-root)
(def ^:private exists? c/exists?)

;; ---------------------------------------------------------------- run one lib

(def ^:private result-re
  #"TOTAL tests=(-?\d+) pass=(-?\d+) fail=(-?\d+) error=(-?\d+) load-fail=(-?\d+)")

(defn- parse-total [out]
  (when-let [m (re-find result-re out)]
    (let [[_ t pa f e lf] m]
      {:tests (parse-long t) :pass (parse-long pa)
       :fail (parse-long f) :error (parse-long e) :load-fail (parse-long lf)})))

(defn- run-lib [{:keys [preload timeout] :as entry}]
  (let [{:keys [missing no-tests cmd-prefix nses dir env]} (c/plan entry)]
    (cond
      missing {:status :missing-paths :detail missing}
      no-tests {:status :no-tests}
      :else
      (let [cmd (vec (concat cmd-prefix
                             ["-m" "lib-conformance-run" (str (* 1000 (or timeout 300)))
                              (if (seq preload) (str/join "," preload) "-")]
                             nses))
            r (apply p/sh {:out :string :err :string :dir dir :extra-env env} cmd)
            out (str (:out r) (:err r))]
        (merge {:status :ran :exit (:exit r) :out out :nses nses}
               (or (parse-total out) {:status :no-result}))))))

;; ---------------------------------------------------------------- reporting

;; A property-based suite draws random cases, so its assertion count drifts run to
;; run — :tolerance is how far the recorded pass/fail/error counts may move before
;; it counts as a regression. It covers error as well as fail because which of the
;; two a generated case lands in is itself unstable: test.chuck's regex suite feeds
;; random patterns to re-pattern, and whether one is rejected or blows up inside
;; the engine moves run to run while the total stays put. Everything without a
;; :tolerance is pinned exactly.
(defn- worse? [{:keys [pass fail error load-fail]} exp tolerance]
  (or (< pass (- (:pass exp 0) (or tolerance 0)))
      (> fail (+ (:fail exp 0) (or tolerance 0)))
      (> error (+ (:error exp 0) (or tolerance 0)))
      (> load-fail (:load-fail exp 0))))

;; worse?'s mirror: a counter that moved the GOOD way by more than the tolerance.
;; Reported as BETTER rather than as a failure, so recording the improvement is a
;; one-line manifest edit.
;;
;; Two things were wrong here (jolt-8a8). Only `pass` was consulted, so a fix that
;; turns failing assertions into absent ones — a load-fail that starts loading, an
;; error the suite stops reaching — moved fail/error/load-fail down without moving
;; pass up and read as plain `ok`. All four are mirrored now.
;;
;; And :tolerance is a SYMMETRIC noise band, which means it suppresses BETTER
;; exactly as it suppresses WORSE. That is deliberate, not the leftover: inside the
;; band a move is a different draw and not a result, so re-recording it would only
;; re-centre the band on whatever the last run happened to generate. test.check
;; (:tolerance 40) came back pass=245 against a recorded 236 and reports ok for
;; that reason. Above the band the move is real and says so; a library without a
;; :tolerance is pinned exactly, so any rise there is BETTER.
(defn- better? [{:keys [pass fail error load-fail]} exp tolerance]
  (let [t (or tolerance 0)]
    (or (> pass (+ (:pass exp 0) t))
        (< fail (- (:fail exp 0) t))
        (< error (- (:error exp 0) t))
        (< load-fail (:load-fail exp 0)))))

(defn- tally-str [{:keys [tests pass fail error load-fail]}]
  (str "tests=" tests " pass=" pass " fail=" fail " error=" error
       (when (and load-fail (pos? load-fail)) (str " load-fail=" load-fail))))

(defn -main [& args]
  ;; A missing checkout fails: exiting 0 here reported a gate that ran nothing
  ;; as green.
  (when-not (exists? libs-root)
    (println (str "FAIL: no library checkout at " libs-root
                  " (set JOLT_CONFORMANCE_LIBS)"))
    (System/exit 1))
  (let [manifest (edn/read-string (slurp (str here "/manifest.edn")))
        wanted (set args)
        entries (cond->> (:libs manifest)
                  (seq wanted) (filter #(wanted (:name %))))
        logdir (str repo-root "/target/libconformance")]
    (.mkdirs (java.io.File. logdir))
    (let [rows (doall
                 (for [{:keys [name skip expect tolerance] :as e} entries]
                   (if skip
                     (do (println (format "%-20s SKIP  %s" name skip))
                         {:name name :verdict :skip})
                     (let [r (run-lib e)]
                       (when (:out r)
                         (spit (str logdir "/" name ".log") (:out r)))
                       (case (:status r)
                         :missing-paths
                         (do (println (format "%-20s MISSING  %s" name
                                              (str/join " " (:detail r))))
                             {:name name :verdict :missing})
                         :no-tests
                         (do (println (format "%-20s NO-TESTS" name))
                             {:name name :verdict :no-tests})
                         :no-result
                         (do (println (format "%-20s NO-RESULT  exit=%s (see %s/%s.log)"
                                              name (:exit r) logdir name))
                             {:name name :verdict :fail})
                         (let [bad (and expect (worse? r expect tolerance))
                               better (and expect (better? r expect tolerance))]
                           (println (format "%-20s %-6s %s%s"
                                            name
                                            (cond bad "WORSE" better "BETTER" :else "ok")
                                            (tally-str r)
                                            (if expect
                                              (str "   expected " (tally-str expect))
                                              "   (no recorded expectation)")))
                           {:name name :verdict (cond bad :fail better :better :else :ok)
                            :got (select-keys r [:tests :pass :fail :error :load-fail])}))))))
          bad (filter #(= :fail (:verdict %)) rows)]
      (println)
      (println (format "%d libraries: %d ok, %d better, %d regressed, %d skipped/missing"
                       (count rows)
                       (count (filter #(= :ok (:verdict %)) rows))
                       (count (filter #(= :better (:verdict %)) rows))
                       (count bad)
                       (count (filter #(#{:skip :missing :no-tests} (:verdict %)) rows))))
      (when (seq bad)
        (println "REGRESSED:" (str/join " " (map :name bad))))
      ;; JOLT_LIBCONF_REPORT=<file> dumps {name tally} so the manifest's :expect
      ;; entries can be refreshed from a run instead of retyped.
      (when-let [out (System/getenv "JOLT_LIBCONF_REPORT")]
        (spit out (pr-str (into {} (keep (fn [r] (when (:got r) [(:name r) (:got r)])) rows))))
        (println "wrote" out))
      (System/exit (if (seq bad) 1 0)))))

;; run on load so `jolt run test/conformance/libs/run.clj [lib ...]` executes.
(apply -main *command-line-args*)
