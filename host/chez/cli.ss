;; cli.ss (jolt-9phg, Phase 3 inc9b) — the pure-Chez jolt runtime. NO Janet.
;;
;; This is the zero-Janet runtime counterpart to bootstrap.ss (the zero-Janet
;; build). It loads the checked-in seed (host/chez/seed/{prelude,image}.ss — the
;; bootstrap compiler) and the zero-Janet spine, reads a Clojure expression, and
;; compiles+evals it ON CHEZ (read -> analyze -> IR -> emit -> eval). With the seed
;; checked in, a clone of the repo runs jolt with only Chez installed — no Janet at
;; build or run time.
;;
;; Run from the repo root:
;;   chez --script host/chez/cli.ss -e "EXPR"
(import (chezscheme))

(define cli-args (cdr (command-line)))   ; drop the script name
(unless (and (= (length cli-args) 2) (string=? (car cli-args) "-e"))
  (display "usage: cli.ss -e EXPR\n")
  (exit 2))
(define cli-expr (cadr cli-args))

;; Assemble the zero-Janet spine from the checked-in seed, exactly as
;; driver/program-zero-janet does — but with no Janet generating the program.
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

;; Compile + eval on Chez; print the result (blank for nil, like the spine). Wrap
;; in (do ...) so a multi-form -e string evaluates every form and returns the last,
;; matching Clojure's -e (jolt-compile-eval reads a single form).
(let ((result (jolt-final-str
                (jolt-compile-eval (string-append "(do " cli-expr ")") "user"))))
  (unless (string=? result "")
    (display result) (newline)))
