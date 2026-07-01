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

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/png.ss")          ; jolt.png — a baked namespace before the snapshot
(load "host/chez/loader.ss")
;; jolt.ffi host primitives (memory / library loading) load AFTER the loader's
;; baked-ns snapshot, so a library's (require '[jolt.ffi]) still loads jolt.ffi's
;; Clojure side (the foreign-fn / defcfn macros, stdlib/jolt/ffi.clj).
(load "host/chez/java/ffi.ss")          ; jolt.ffi (FFI: a library binds native code)

;; jolt.main + jolt.deps live under jolt-core; keep them (and stdlib) on the
;; roots so the CLI's own namespaces — and any jolt.* an app pulls in — resolve.
;; A project's resolved deps roots are prepended to these by jolt.main.
(set-source-roots! (list "jolt-core" "stdlib"))

;; Render an uncaught jolt throw (any value, not just a Chez condition) to stderr
;; and exit non-zero, instead of Chez's opaque "non-condition value" dump. The
;; message/ex-data/cause + a mapped Clojure backtrace come from the shared
;; renderer (source-registry.ss); the cli adds the top-level source location.
(define (jolt-report-uncaught raw)
  (let ((v (jolt-unwrap-throw raw))
        (port (current-error-port)))
    (jolt-render-throwable v port)
    ;; The top-level form that was evaluating when this propagated (file:line:col).
    (let ((loc (jolt-current-source-string)))
      (when loc (display "  at " port) (display loc port) (newline port)))
    (let ((bt (jolt-backtrace-string v)))
      (when bt (display "  trace:\n" port) (display bt port)))
    (exit 1)))

(guard (v (#t (jolt-report-uncaught v)))
  (cond
    ;; -e EXPR — evaluate one expression and print it (blank for nil). Wrapped in
    ;; (do …) so a multi-form string evaluates every form and returns the last.
    ((and (= (length cli-args) 2) (string=? (car cli-args) "-e"))
     (let ((result (jolt-final-str
                     (jolt-compile-eval (string-append "(do " (cadr cli-args) ")") "user"))))
       (unless (string=? result "")
         (display result) (newline))))
    ;; otherwise dispatch the argv through jolt.main/-main
    (else
     ;; `build` AOT-compiles an app to a standalone binary — load the build
     ;; driver (the cross-compiler emitter) on demand so a normal run never pays
     ;; for it. It defines jolt.host/build-binary, which jolt.main's build cmd calls.
     (when (and (pair? cli-args) (string=? (car cli-args) "build"))
       (load "host/chez/build.ss"))
     (load-namespace "jolt.main")
     (let ((mainv (var-deref "jolt.main" "-main")))
       (apply jolt-invoke mainv cli-args)))))
