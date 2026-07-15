;; cli.ss — the jolt runtime.
;;
;; Loads the checked-in seed (host/chez/seed/{prelude,image}.ss — the bootstrap
;; compiler) and the spine, then either evaluates a -e expression or dispatches a
;; CLI command (run/-M/repl/path/task) through jolt.main. The loader
;; (loader.ss) turns `require` into real file loading off the source roots, so a
;; multi-file project with deps.edn dependencies runs end to end.
;;
;; Run from the repo root (bin/joltc cd's there); the project dir is JOLT_PWD.
(import (chezscheme))

(define cli-args (cdr (command-line)))   ; drop the script name

;; Fail early and actionably when the vendored submodules aren't checked out —
;; a plain `git clone` or GitHub's auto-generated "Source code" release archive
;; lacks them, and the raw failure ("load failed for vendor/irregex/irregex.scm")
;; doesn't say how to fix it. (The self-contained joltc binary embeds these and
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

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")          ; jolt.png — a baked namespace before the snapshot
(load "host/chez/loader.ss")
;; jolt.ffi host primitives (memory / library loading) load AFTER the loader's
;; baked-ns snapshot, so a library's (require '[jolt.ffi]) still loads jolt.ffi's
;; Clojure side (the foreign-fn / defcfn macros, stdlib/jolt/ffi.clj).
(load "host/chez/java/ffi.ss")          ; jolt.ffi (FFI: a library binds native code)

;; jolt.main + jolt.deps live under jolt-core; keep them (and stdlib) on the
;; roots so the CLI's own namespaces — and any jolt.* an app pulls in — resolve.
;; A project's resolved deps roots are prepended to these by jolt.main.
(set-source-roots! ldr-install-roots)

;; jolt-report-uncaught / drop-end-of-options / the -e arm live in cli-core.ss,
;; shared with the standalone binary's launcher (build-joltc.ss).

;; JOLT_TRACE opt-in, at runtime (before any app ns compiles) so the app is traced.
(jolt-trace-init-from-env!)


(jolt-cli-run cli-args
  ;; `build` AOT-compiles an app to a standalone binary — load the build driver
  ;; (the cross-compiler emitter) on demand so a normal run never pays for it.
  ;; It defines jolt.host/build-binary, which jolt.main's build cmd calls.
  (lambda () (load "host/chez/build.ss")))
