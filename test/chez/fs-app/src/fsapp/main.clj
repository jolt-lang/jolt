;; Smoke fixture: a built binary must have the vendored babashka.fs (via jolt.fs)
;; available — including functions defined after babashka.fs's cljs-only reader
;; conditionals (directory?/cwd/which), which the emission must not truncate.
(ns fsapp.main (:require [jolt.fs :as fs]))
(defn -main [& _]
  (println "FS-APP"
           (str (fs/path "a" "b"))
           (fs/directory? (fs/cwd))
           (some? (fs/which "sh"))))
