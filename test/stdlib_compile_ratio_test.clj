;; A stdlib function must run about as fast as the same source compiled from a
;; user file, measured as a RATIO in one process.
;;
;; A release binary carries the stdlib precompiled: the seven seed-embedded
;; namespaces (clojure.string, clojure.set, ...) inside the image, and every other
;; install-owned namespace (jolt.time.*, clojure.data, babashka.process, ...) as an
;; embedded fasl that build-jolt captured from source. Neither path is the one a
;; user file takes, so nothing else notices when one of them compiles worse.
;;
;; One did, from the start of the embedded fasls until 0.8.24: build-jolt
;; captured them after the CLI AOT section had published a fresh compilation unit
;; and switched its var-cache off, so every var reference in every embedded
;; stdlib namespace resolved by name on each call. jolt.time.util/hmsn->nano took
;; 143 ns against 20 ns for the same defn in a user file; clojure.data/diff and
;; babashka.process/tokenize 2.0x and 1.5x. A `jolt build` binary was unaffected
;; (it recompiles the stdlib with the app).
;;
;; Each arm loads a verbatim copy of the stdlib namespace's source under another
;; name through load-string (the runtime spine every user file goes through) and
;; times the stdlib fn against the copy's, same arguments, samples alternating,
;; best of N. Fixed, every ratio reads ~1.0. A reading over the ceiling is
;; sampled up to twice more, judging each arm's minimum over every sample, and
;; the Makefile runs this gate after the rest of its shard (QUIET-GATES), as it
;; does fastpathratio: beside the shard a neighbour's burst put blank? at 1.74
;; on a commit whose rerun read 0.95.
(ns stdlib-compile-ratio-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]
            [clojure.data]
            [jolt.time.util]
            [babashka.process]))

(def ^:private samples 7)

;; Load the namespace's own source again as copy.<ns>, from the resource the
;; loader would read, so the two arms compile the same text.
(defn- copy-ns! [ns-sym]
  (let [base (-> (name ns-sym) (str/replace "-" "_") (str/replace "." "/"))
        res (or (io/resource (str base ".clj")) (io/resource (str base ".cljc")))
        _ (when-not res (throw (ex-info (str "no source for " ns-sym) {})))
        src (slurp res)
        copy (str "copy." (name ns-sym))
        here (ns-name *ns*)]
    (load-string (str/replace-first src (str "(ns " (name ns-sym)) (str "(ns " copy)))
    (in-ns here)
    (symbol copy)))

;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- timed [f]
  (let [t (cpu-ns)] (f) (/ (- (cpu-ns) t) 1e6)))

;; Best-of per arm. The arms alternate, and which one goes first alternates
;; too, so a neighbour's burst under make -j lands on both; each sample starts
;; from a fresh collection so neither arm inherits the other's garbage (see
;; fastpath_ratio_test.clj).
(defn- sample [f] (System/gc) (timed f))
(defn- best-of-pair [a b]
  (let [ps (vec (for [i (range samples)]
                  (if (even? i)
                    (let [x (sample a) y (sample b)] [x y])
                    (let [y (sample b) x (sample a)] [x y]))))]
    [(reduce min (map first ps)) (reduce min (map second ps))]))

(def ^:private failures (atom []))

(defn- judge! [label ns-sym fname n call ceiling]
  (let [cns (copy-ns! ns-sym)
        sf @(ns-resolve ns-sym fname)
        uf @(ns-resolve cns fname)
        _ (when (identical? sf uf) (throw (ex-info "copy resolved to the stdlib fn" {})))
        stdlib-arm #(dotimes [_ n] (call sf))
        user-arm #(dotimes [_ n] (call uf))
        ;; the best of every sample taken so far, per arm: a re-measure adds
        ;; samples, it does not throw the first ones away
        best (atom [Double/MAX_VALUE Double/MAX_VALUE])
        ratio (fn []
                (let [[s u] (best-of-pair stdlib-arm user-arm)]
                  (swap! best (fn [[s0 u0]] [(min s0 s) (min u0 u)]))
                  (let [[s u] @best] (/ s (max u 0.05)))))
        ;; over the ceiling, up to two more rounds, as fastpathratio does: a
        ;; burst from outside the process can sit on one arm for a whole round
        ;; (blank? read 1.74 twice in one CI process, 0.95 on its rerun), and a
        ;; real regression reads high on every round, so re-measuring costs the
        ;; gate no power. No reading is too high to re-measure.
        r (loop [r (ratio) left 2]
            (if (and (pos? left) (> r ceiling))
              (do (println (format "  %s: %.2f — over the ceiling, sampling again" label r))
                  (recur (ratio) (dec left)))
              r))
        [sb ub] @best]
    (println (format "  %-30s stdlib/user %5.2f  (%.1f / %.1f ms, ceiling %.1f)%s"
                     label r sb ub ceiling (if (> r ceiling) "  <-- REGRESSED" "")))
    (when (> r ceiling)
      (swap! failures conj (format "%s: ratio %.2f exceeds ceiling %.1f" label r ceiling)))))

(defn -main [& _]
  (println "stdlib fn vs the same source compiled from user code:")
  ;; embedded-fasl namespaces (build-jolt's stdlib capture)
  (judge! "jolt.time.util/hmsn->nano" 'jolt.time.util 'hmsn->nano 400000 #(% 1 2 3 4) 1.4)
  (judge! "jolt.time.util/iso-time-str" 'jolt.time.util 'iso-time-str 40000 #(% 45296789000000) 1.4)
  (judge! "clojure.data/diff" 'clojure.data 'diff 20000 #(% {:a 1 :b [1 2]} {:a 2 :b [1 3]}) 1.3)
  (judge! "babashka.process/tokenize" 'babashka.process 'tokenize 20000 #(% "ls -la \"a b\" c") 1.3)
  ;; seed-embedded namespaces (compiled into the image by the mint)
  (judge! "clojure.set/union" 'clojure.set 'union 200000 #(% #{1 2} #{3}) 1.3)
  (judge! "clojure.string/blank?" 'clojure.string 'blank? 400000 #(% "  x ") 1.3)
  (if (seq @failures)
    (do (println "\nFAIL — stdlib code compiles worse than user code:")
        (doseq [f @failures] (println "  -" f))
        (println "\nSee the header of test/stdlib_compile_ratio_test.clj.")
        (System/exit 1))
    (println "\nok — stdlib runs at user-code speed")))

;; A run that dies before its verdict says why, on stdout beside the arms that
;; did run, rather than leaving make's "Error 1" as the only line.
(try (apply -main *command-line-args*)
     (catch Throwable e
       (println "\nFAIL — stdlibratio stopped:" (or (ex-message e) (str e)))
       (when-let [d (ex-data e)] (println "  data:" (pr-str d)))
       (System/exit 1)))
