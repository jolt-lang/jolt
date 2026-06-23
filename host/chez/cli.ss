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
(load "host/chez/ffi.ss")          ; jolt.ffi (FFI: a library binds native code)

;; jolt.main + jolt.deps live under jolt-core; keep them (and stdlib) on the
;; roots so the CLI's own namespaces — and any jolt.* an app pulls in — resolve.
;; A project's resolved deps roots are prepended to these by jolt.main.
(set-source-roots! (list "jolt-core" "stdlib"))

;; Render an uncaught jolt throw (any value, not just a Chez condition) to stderr
;; and exit non-zero, instead of Chez's opaque "non-condition value" dump. An
;; ex-info shows its message + ex-data; anything else is pr-str'd.
(define (jolt-report-uncaught v)
  (let ((port (current-error-port)))
    (if (and (jolt=2 (jolt-get v jolt-kw-ex-type jolt-nil) jolt-kw-ex-info))
        (begin
          (display "Unhandled exception: " port)
          (display (jolt-str-render-one (jolt-get v jolt-kw-message jolt-nil)) port)
          (newline port)
          (let ((data (jolt-get v jolt-kw-data jolt-nil)))
            (unless (jolt-nil? data)
              (display "  ex-data: " port) (display (jolt-pr-str data) port) (newline port)))
          (let ((cause (jolt-get v jolt-kw-cause jolt-nil)))
            (when (condition? cause)
              (display "  cause: " port)
              (display (with-output-to-string (lambda () (display-condition cause))) port)
              (newline port))))
        (begin
          (display "Unhandled exception: " port)
          (display (if (condition? v) (with-output-to-string (lambda () (display-condition v))) (jolt-pr-str v)) port)
          (newline port)))
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
     (load-namespace "jolt.main")
     (let ((mainv (var-deref "jolt.main" "-main")))
       (apply jolt-invoke mainv cli-args)))))
