;; Smoke fixture: a built binary must have the vendored babashka.process (via
;; jolt.process) available and runnable — including spawning a real sub-process.
;; The exit-code and signal paths go through libc waitpid / kill resolved by
;; jolt-foreign-proc-safe, so the tree-shaken (petite-only) build proves those
;; compiled foreign-procedures resolve without the compiler boot.
(ns procapp.main
  (:require [jolt.process :as p]
            [clojure.string :as str]))
(defn -main [& _]
  (let [r (p/sh ["echo" "hi"])                 ; spawn + waitpid: exit 0
        sleeper (p/process ["sleep" "5"])]
    (p/destroy sleeper)                          ; kill: SIGTERM -> exit 143
    (println "PROC-APP" (str/trim (:out r)) (:exit r) (:exit @sleeper))))
