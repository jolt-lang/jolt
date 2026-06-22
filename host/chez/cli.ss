;; cli.ss (jolt-9phg / jolt-90sp) — the pure-Chez jolt runtime. NO Janet.
;;
;; Loads the checked-in seed (host/chez/seed/{prelude,image}.ss — the bootstrap
;; compiler) and the zero-Janet spine, then either evaluates a -e expression or
;; dispatches a CLI command (run/-M/repl/path/task) through jolt.main. The loader
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
(load "host/chez/loader.ss")

;; jolt.main + jolt.deps live under jolt-core; keep them (and src/jolt) on the
;; roots so the CLI's own namespaces — and any jolt.* an app pulls in — resolve.
;; A project's resolved deps roots are prepended to these by jolt.main.
(set-source-roots! (list "jolt-core" "src/jolt"))

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
     (apply jolt-invoke mainv cli-args))))
