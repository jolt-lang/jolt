;; Smoke fixture: a built binary must have the vendored babashka.fs (via jolt.fs)
;; available — including functions defined after babashka.fs's cljs-only reader
;; conditionals (directory?/cwd/which), which the emission must not truncate.
;; The perms round-trip goes through chmod/stat resolved by jolt-foreign-proc-safe,
;; so the tree-shaken (petite-only) build proves compiled foreign-procedures
;; resolve without the compiler boot.
(ns fsapp.main (:require [jolt.fs :as fs]))
(defn -main [& _]
  (let [tmp (str (fs/create-temp-file))]
    (fs/set-posix-file-permissions tmp "rw-------")
    (let [perms (fs/posix->str (fs/posix-file-permissions tmp))]
      (fs/delete tmp)
      (println "FS-APP"
               (str (fs/path "a" "b"))
               (fs/directory? (fs/cwd))
               (some? (fs/which "sh"))
               perms))))
