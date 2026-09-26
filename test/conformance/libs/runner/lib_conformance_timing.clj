;; Per-test timing runner for a library's clojure.test suite. Portable: the same
;; file runs on jolt and on JVM Clojure, so timing.clj can put the two side by
;; side test by test.
;;
;; Invoked as `-m lib-conformance-timing <timeout-ms> <reps> <preload-csv> <ns>...`
;; (preload-csv is "-" for none). Each namespace is loaded once and timed, then
;; run REPS times through test-ns, so :once fixtures and a test-ns-hook behave as
;; they do in a normal run. A test is timed between its :begin-test-var and
;; :end-test-var reports, which is inside its :each fixtures. The first rep is
;; reported separately from the minimum: on the JVM it carries the JIT warmup,
;; on jolt it runs code that was compiled at load.
;;
;; Suite output is discarded. Lines the driver reads:
;;   LOAD <ns> <ms>
;;   LOAD-FAIL <ns> <message>
;;   TEST <ns>/<name> <first-ms> <min-ms> <pass> <fail> <error>
;;   DONE
(ns lib-conformance-timing
  (:require [clojure.string]
            [clojure.test :as t]))

(defn- ms-since [t0] (/ (- (System/nanoTime) t0) 1e6))

;; The outer message and the root cause's: a load failure on the JVM is a
;; CompilerException whose own message only names the file.
(defn- msg-of [e]
  (let [root (last (take-while some? (iterate ex-cause e)))]
    (str (or (ex-message e) e)
         (when-not (identical? root e) (str " <- " (or (ex-message root) root))))))

(defn- watchdog! [ms]
  (when (pos? ms)
    (future
      (Thread/sleep ms)
      (println "TIMEOUT after" ms "ms")
      (flush)
      (System/exit 3))))

(defn- var-key [v]
  (let [m (meta v)]
    (str (ns-name (:ns m)) "/" (:name m))))

(defn- run-ns-once
  "Run NS's tests once. Returns {var-key {:ms :pass :fail :error}}. A test that
  runs another test by calling it nests begin/end reports; the time goes to the
  outer one and the assertions to whichever test was innermost."
  [ns-sym]
  (let [results (atom {})
        stack (atom ())
        bump (fn [k]
               (when-let [[vk] (first @stack)]
                 (swap! results update-in [vk k] (fnil inc 0))))]
    (binding [t/report (fn [m]
                         (case (:type m)
                           :begin-test-var
                           (swap! stack conj [(var-key (:var m)) (System/nanoTime)])
                           :end-test-var
                           (let [[vk t0] (first @stack)]
                             (swap! stack rest)
                             (when (empty? @stack)
                               (swap! results update-in [vk :ms] (fnil + 0) (ms-since t0))))
                           :pass (bump :pass)
                           :fail (bump :fail)
                           :error (bump :error)
                           nil))
              *out* (java.io.StringWriter.)
              t/*test-out* (java.io.StringWriter.)]
      (try (t/test-ns ns-sym)
           (catch Throwable e
             (when-let [[vk] (first @stack)]
               (swap! results update-in [vk :error] (fnil inc 0))))))
    @results))

(defn -main [& args]
  (let [[t-ms reps preload & nses] args
        reps (Long/parseLong reps)]
    (watchdog! (Long/parseLong t-ms))
    (doseq [p (clojure.string/split (or preload "-") #",")
            :when (and (seq p) (not= p "-"))]
      (try (require (symbol p))
           (catch Throwable e (println "PRELOAD" p "FAILED" (msg-of e)))))
    (let [loaded (reduce (fn [acc n]
                           (let [t0 (System/nanoTime)]
                             (try (require (symbol n))
                                  (println "LOAD" n (format "%.3f" (ms-since t0)))
                                  (conj acc (symbol n))
                                  (catch Throwable e
                                    (println "LOAD-FAIL" n
                                             (clojure.string/replace (str (msg-of e)) #"\s+" " "))
                                    acc))))
                         [] nses)]
      (doseq [n loaded]
        (let [runs (vec (repeatedly reps #(run-ns-once n)))]
          (doseq [vk (sort (distinct (mapcat keys runs)))]
            (let [ms (keep #(get-in % [vk :ms]) runs)
                  r (get (first runs) vk)]
              (when (seq ms)
                (println "TEST" vk
                         (format "%.3f" (double (first ms)))
                         (format "%.3f" (double (apply min ms)))
                         (:pass r 0) (:fail r 0) (:error r 0))))))
        (flush))
      (println "DONE")
      (flush)
      (System/exit 0))))
