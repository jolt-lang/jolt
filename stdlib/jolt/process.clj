(ns jolt.process
  "Shelling out and spawning sub-processes: run a command, stream or capture its
  output, pipe processes together, check exit codes. The implementation is the
  vendored babashka.process, driven by Jolt's java.lang.ProcessBuilder / Process
  shims (host/chez/java/process.ss) over the operating system's process API;
  jolt.process is the public surface.

  The main entry points are `process` (spawn, returns a process record you can
  deref for the result), `sh` (run and block, capturing :out/:err strings),
  `shell` (run inheriting stdio, throw on failure), the `$` macro (convenience
  with ~ interpolation), `check` (throw on non-zero exit), and `pipeline` /
  threading with `->` for connecting processes. See
  https://github.com/babashka/process for the API of each function.

  `exec` (replace the current image, GraalVM/babashka-only) is not re-exported:
  it is unsupported outside a native image and would only ever throw here."
  (:require [babashka.process]
            ;; babashka.process loads its pprint method conditionally at runtime
            ;; (when clojure.pprint is present); require it here so it is in the
            ;; static closure and gets embedded in an AOT-built binary.
            [babashka.process.pprint]
            [jolt.util :refer [import-vars]]))

;; Excluded from the public surface:
;;   *defaults* / null-file / Process / ProcessBuilder / ProcessBuilder$Redirect /
;;   print-method  — non-fn vars (dynamic var, delay, record/class, multimethod)
;;                    that import-vars cannot re-export as delegating fns.
;;   if-before-jdk8 / if-has-exec — internal implementation macros.
;;   exec — GraalVM-only; always throws on this host.
(import-vars babashka.process
  :exclude #{*defaults* null-file Process ProcessBuilder ProcessBuilder$Redirect
             print-method if-before-jdk8 if-has-exec exec})
