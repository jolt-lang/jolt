(ns process-run
  "Runner for the vendored babashka/process upstream test suite
  (vendor/process/test). Requires each test namespace given on the command line
  and runs its clojure.test tests, printing one machine-readable result line per
  namespace. Many upstream tests shell out to babashka itself and skip gracefully
  when bb is absent (BABASHKA_TEST_ENV=jvm) — those count as neither pass nor
  fail. Driven by host/chez/process-suite.sh."
  (:require [clojure.test :as t]))

(defn -main [& nses]
  (doseq [n nses]
    (let [ns-sym (symbol n)]
      (try
        (require ns-sym)
        (let [r (t/run-tests ns-sym)]
          (println "PROC-RESULT" n (:pass r 0) (:fail r 0) (:error r 0)))
        (catch Throwable e
          (println "PROC-RESULT" n 0 0 1
                   (str "LOAD: " (.getName (class e)) ": " (.getMessage e)))))))
  (System/exit 0))
