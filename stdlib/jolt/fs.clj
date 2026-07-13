(ns jolt.fs
  "File-system utilities: paths, files, directories, globbing, copy/move,
  timestamps, POSIX permissions, and symbolic links. The implementation is the
  vendored babashka.fs; jolt.fs is the public surface and exposes only the
  operations Jolt fully supports on this host.

  Path-valued results are java.nio.file.Path values. See
  https://github.com/babashka/fs for the API of each function."
  (:require [babashka.fs]))

;; zip / gzip need java.util.zip, which Jolt does not shim yet — keep them out
;; of the public surface rather than exposing operations that fail.
(def ^:private unsupported '#{zip unzip gzip gunzip})

;; Re-export every supported public function of babashka.fs at compile time as a
;; thin delegating fn. Static defs (an AOT-built binary bakes them) that resolve
;; babashka.fs at call time, so load order does not matter. Macros are
;; re-exported explicitly below.
(defmacro ^:private reexport-babashka-fs []
  (cons 'do
        (for [[sym v] (ns-publics 'babashka.fs)
              :when (and (not (contains? unsupported sym)) (not (:macro (meta v))))]
          `(defn ~sym [& args#] (apply ~(symbol "babashka.fs" (name sym)) args#)))))

(reexport-babashka-fs)

(defmacro with-temp-dir
  "Evaluate body with a fresh temporary directory bound. See babashka.fs/with-temp-dir."
  [binding & body]
  `(babashka.fs/with-temp-dir ~binding ~@body))
