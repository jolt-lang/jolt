(ns cts-run
  "Runner for the vendored jank-lang/clojure-test-suite (vendor/clojure-test-suite):
  requires each test namespace given on the command line and runs its clojure.test
  tests, printing one machine-readable result line per namespace. Driven per-process
  by host/chez/cts.sh so a hang or crash in one namespace can't take out the run."
  (:require [clojure.test :as t]))

(defn -main [& nses]
  (doseq [n nses]
    (let [ns-sym (symbol n)]
      (try
        (require ns-sym)
        (let [r (t/run-tests ns-sym)]
          (println "CTS-RESULT" n (:pass r 0) (:fail r 0) (:error r 0)))
        (catch Throwable e
          (println "CTS-RESULT" n 0 0 1
                   (str "LOAD: " (.getName (class e)) ": " (.getMessage e)))))))
  (System/exit 0))
