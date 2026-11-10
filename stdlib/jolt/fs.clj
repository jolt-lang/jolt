(ns jolt.fs
  "File-system utilities: paths, files, directories, globbing, copy/move,
  timestamps, POSIX permissions, and symbolic links. The implementation is the
  vendored babashka.fs; jolt.fs is the public surface and exposes only the
  operations Jolt fully supports on this host.

  Path-valued results are java.nio.file.Path values. See
  https://github.com/babashka/fs for the API of each function."
  (:require [babashka.fs]
            [jolt.util :refer [import-vars]])
  (:refer-clojure :exclude [exists? slurp spit]))

;; The whole surface, zip/unzip/gzip/gunzip included: they run on the runtime's
;; java.util.zip (host/chez/java/zip-*.ss), which every jolt binary carries.
(import-vars babashka.fs)
