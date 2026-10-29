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
;; best of N. Fixed, every ratio reads ~1.0.
(ns stdlib-compile-ratio-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]
            [clojure.data]
            [jolt.time.util]
            [babashka.process]))

(def ^:private samples 5)

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

(defn- timed [f]
  (let [t (System/nanoTime)] (f) (/ (- (System/nanoTime) t) 1e6)))

;; best-of per arm, the two arms' samples alternating so a neighbour's burst
;; lands on both (see fastpath_ratio_test.clj).
(defn- best-of-pair [a b]
  (let [ps (vec (repeatedly samples #(vector (timed a) (timed b))))]
    [(reduce min (map first ps)) (reduce min (map second ps))]))

(def ^:private failures (atom []))

(defn- judge! [label ns-sym fname n call ceiling clear]
  (let [cns (copy-ns! ns-sym)
        sf @(ns-resolve ns-sym fname)
        uf @(ns-resolve cns fname)
        _ (when (identical? sf uf) (throw (ex-info "copy resolved to the stdlib fn" {})))
        stdlib-arm #(dotimes [_ n] (call sf))
        user-arm #(dotimes [_ n] (call uf))
        ratio (fn [] (let [[s u] (best-of-pair stdlib-arm user-arm)] (/ s (max u 0.05))))
        r1 (ratio)
        r (if (and (> r1 ceiling) (< r1 clear))
            (do (println (format "  %s: %.2f — in the re-measure band, sampling again" label r1))
                (ratio))
            r1)]
    (println (format "  %-30s stdlib/user %5.2f  (ceiling %.1f)%s"
                     label r ceiling (if (> r ceiling) "  <-- REGRESSED" "")))
    (when (> r ceiling)
      (swap! failures conj (format "%s: ratio %.2f exceeds ceiling %.1f" label r ceiling)))))

(defn -main [& _]
  (println "stdlib fn vs the same source compiled from user code:")
  ;; embedded-fasl namespaces (build-jolt's stdlib capture)
  (judge! "jolt.time.util/hmsn->nano" 'jolt.time.util 'hmsn->nano 400000 #(% 1 2 3 4) 1.4 3.0)
  (judge! "jolt.time.util/iso-time-str" 'jolt.time.util 'iso-time-str 40000 #(% 45296789000000) 1.4 3.0)
  (judge! "clojure.data/diff" 'clojure.data 'diff 20000 #(% {:a 1 :b [1 2]} {:a 2 :b [1 3]}) 1.3 2.0)
  (judge! "babashka.process/tokenize" 'babashka.process 'tokenize 20000 #(% "ls -la \"a b\" c") 1.3 2.0)
  ;; seed-embedded namespaces (compiled into the image by the mint)
  (judge! "clojure.set/union" 'clojure.set 'union 200000 #(% #{1 2} #{3}) 1.3 2.0)
  (judge! "clojure.string/blank?" 'clojure.string 'blank? 400000 #(% "  x ") 1.3 2.0)
  (if (seq @failures)
    (do (println "\nFAIL — stdlib code compiles worse than user code:")
        (doseq [f @failures] (println "  -" f))
        (println "\nSee the header of test/stdlib_compile_ratio_test.clj.")
        (System/exit 1))
    (println "\nok — stdlib runs at user-code speed")))

(apply -main *command-line-args*)
