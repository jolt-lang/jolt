(ns jolt.fs
  "File-system utilities: paths, files, directories, globbing, copy/move,
  timestamps, POSIX permissions, and symbolic links. The implementation is the
  vendored babashka.fs; jolt.fs is the public surface and exposes only the
  operations Jolt fully supports on this host.

  Path-valued results are java.nio.file.Path values. See
  https://github.com/babashka/fs for the API of each function."
  (:require [babashka.fs]
            [jolt.util :refer [import-vars]]))

;; zip / gzip need java.util.zip, which Jolt does not shim yet — keep them out
;; of the public surface rather than exposing operations that fail.
(import-vars babashka.fs :exclude #{zip unzip gzip gunzip})
