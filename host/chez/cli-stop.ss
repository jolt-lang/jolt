;; cli.ss — the jolt runtime.
;;
;; Loads the checked-in seed (host/chez/seed/{prelude,image}.ss — the bootstrap
;; compiler) and the spine, then either evaluates a -e expression or dispatches a
;; CLI command (run/-M/repl/path/task) through jolt.main. The loader
;; (loader.ss) turns `require` into real file loading off the source roots, so a
;; multi-file project with deps.edn dependencies runs end to end.
;;
;; Run from the repo root (bin/jolt cd's there); the project dir is JOLT_PWD.
(import (chezscheme))

;; Fail early and actionably when the vendored submodules aren't checked out —
;; a plain `git clone` or GitHub's auto-generated "Source code" release archive
;; lacks them, and the raw failure ("load failed for vendor/irregex/irregex.scm")
;; doesn't say how to fix it. (The self-contained jolt binary embeds these and
;; never runs this file.)
(unless (file-exists? "vendor/irregex/irregex.scm")
  (display "jolt: vendor submodules are missing (vendor/irregex).
" (current-error-port))
  (display "GitHub's 'Source code' release archives don't include submodules.
" (current-error-port))
  (display "Clone the repo instead:
" (current-error-port))
  (display "  git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
" (current-error-port))
  (display "or, in an existing checkout:
" (current-error-port))
  (display "  git submodule update --init --recursive
" (current-error-port))
  (exit 1))

(load "host/chez/scheme-adapter-runtime.ss")  ; before rt.ss: macros + top-levels in rt.ss/java/*.ss call sa-*
(when (equal? (getenv "STOP") "1") (exit 0))
(load "host/chez/rt.ss")
(when (equal? (getenv "STOP") "2") (exit 0))
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(when (equal? (getenv "STOP") "3") (exit 0))
(load "host/chez/post-prelude.ss")
(when (equal? (getenv "STOP") "4") (exit 0))
(load "host/chez/post-prelude-str.ss")
(when (equal? (getenv "STOP") "5") (exit 0))
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(when (equal? (getenv "STOP") "6") (exit 0))
(load "host/chez/seed/image.ss")
(when (equal? (getenv "STOP") "7") (exit 0))
(load "host/chez/compile-eval.ss")
(when (equal? (getenv "STOP") "8") (exit 0))
(load "host/chez/cli-core.ss")
(when (equal? (getenv "STOP") "9") (exit 0))
(load "host/chez/png.ss")          ; jolt.png — a baked namespace before the snapshot
(when (equal? (getenv "STOP") "10") (exit 0))
(load "host/chez/loader.ss")
(when (equal? (getenv "STOP") "11") (exit 0))
;; The diagnostic snippet renderer. AFTER the loader, whose read-file-string it
;; uses to pull the offending lines back off disk; cli-core.ss calls into it from
;; inside the report path, which runs long after every load has finished.
(load "host/chez/diagnostic-render.ss")
(when (equal? (getenv "STOP") "12") (exit 0))
;; jolt.ffi host primitives (memory / library loading) load AFTER the loader's
;; baked-ns snapshot, so a library's (require '[jolt.ffi]) still loads jolt.ffi's
;; Clojure side (the foreign-fn / defcfn macros, stdlib/jolt/ffi.clj).
(load "host/chez/java/ffi.ss")          ; jolt.ffi (FFI: a library binds native code)
(when (equal? (getenv "STOP") "13") (exit 0))

;; jolt-report-uncaught / drop-end-of-options / the -e arm live in cli-core.ss,
;; shared with the standalone binary's launcher (build-jolt.ss). The entry
;; tail (source roots, trace init, jolt-cli-run) is shared with the devcache
;; entry (cli-devcache.ss) via cli-tail.ss.
(load "host/chez/cli-tail.ss")
(when (equal? (getenv "STOP") "14") (exit 0))
