;; Shared CLI dispatch for the two joltc entry points: the script-mode driver
;; (cli.ss under `chez --script`) and the standalone binary's launcher
;; (build-joltc.ss scheme-start). Both call jolt-cli-run so the -e handling,
;; end-of-options rule, and uncaught-throw reporting cannot drift apart — the
;; binary once carried a stale copy of the -e arm.

;; Render an uncaught jolt throw (any value, not just a Chez condition) to stderr
;; and exit non-zero, instead of Chez's opaque "non-condition value" dump. The
;; message/ex-data/cause + a mapped Clojure backtrace come from the shared
;; renderer (source-registry.ss); this adds the top-level source location.
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

;; POSIX end-of-options: drop the first standalone "--" in an argv list; any
;; later "--" stays literal program data. Returns a Scheme list.
(define (drop-end-of-options args)
  (let loop ((in args) (acc '()))
    (cond
      ((null? in) (reverse acc))
      ((string=? (car in) "--") (append (reverse acc) (cdr in)))
      (else (loop (cdr in) (cons (car in) acc))))))

;; Dispatch a joltc argv. prepare-build! runs before jolt.main dispatches a
;; `build` — the script driver loads the build driver from the repo, the
;; standalone binary materializes its bundled boots/stub (build.ss itself is
;; already inlined there).
(define (jolt-cli-run cli-args prepare-build!)
  (guard (v (#t (jolt-report-uncaught v)))
    (cond
      ;; -e EXPR [args…] — evaluate one expression and print it (blank for nil).
      ;; Wrapped in (do …) so a multi-form string evaluates every form and returns
      ;; the last. The argv after EXPR are *command-line-args* (nil when empty),
      ;; with the first standalone "--" consumed as POSIX end-of-options.
      ((and (pair? cli-args) (string=? (car cli-args) "-e")
            (pair? (cdr cli-args)))
       (let* ((expr (cadr cli-args))
              (app-args (drop-end-of-options (cddr cli-args)))
              (cla (if (null? app-args) jolt-nil (list->cseq app-args))))
         (jolt-push-thread-bindings
           (jolt-hash-map (jolt-var "clojure.core" "*command-line-args*") cla))
         (let ((result (jolt-final-str
                         (jolt-compile-eval (string-append "(do " expr ")") "user"))))
           (jolt-pop-thread-bindings)
           (unless (string=? result "")
             (display result) (newline)))))
      ;; otherwise dispatch the argv through jolt.main/-main
      (else
       (when (and (pair? cli-args) (string=? (car cli-args) "build"))
         (prepare-build!))
       (load-namespace "jolt.main")
       (let ((mainv (var-deref "jolt.main" "-main")))
         (apply jolt-invoke mainv cli-args))))))
