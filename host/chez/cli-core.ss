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
;; Read all of stdin as a string (a `-` program / expression source).
(define (jolt-read-all-stdin)
  (let ((out (open-output-string)) (in (current-input-port)))
    (let loop ()
      (let ((c (read-char in)))
        (if (eof-object? c)
            (get-output-string out)
            (begin (write-char c out) (loop)))))))

;; Evaluate EXPR (a string of one-or-more forms) with *command-line-args* bound
;; to app-args. print? echoes the final value (blank for nil), as `-e` does; a
;; `-` stdin PROGRAM runs as a script and suppresses it.
;; Binds *allow-unresolved-vars* to #f so bare symbols that don't resolve in the
;; global scope throw, matching JVM. Inside fn bodies the analyzer still
;; late-binds so defmulti/defmethod forward references work.
(define (jolt-run-expr-string expr app-args print?)
  (let ((cla (if (null? app-args) jolt-nil (list->cseq app-args)))
        (av-cell (jolt-var "jolt.analyzer" "*allow-unresolved-vars*")))
    (jolt-push-thread-bindings
      (if av-cell
          (jolt-hash-map (jolt-var "clojure.core" "*command-line-args*") cla
                         av-cell #f)
          (jolt-hash-map (jolt-var "clojure.core" "*command-line-args*") cla)))
    (let ((result (jolt-final-str
                    (jolt-compile-eval (string-append "(do " expr ")") "user"))))
      (jolt-pop-thread-bindings)
      (when (and print? (not (string=? result "")))
        (display result) (newline)))))

(define (jolt-cli-run cli-args prepare-build!)
  (guard (v (#t (jolt-report-uncaught v)))
    (cond
      ;; -e EXPR [args…] — evaluate one expression and print it (blank for nil).
      ;; Wrapped in (do …) so a multi-form string evaluates every form and returns
      ;; the last. The argv after EXPR are *command-line-args* (nil when empty),
      ;; with the first standalone "--" consumed as POSIX end-of-options. `-e -`
      ;; reads the expression from stdin.
      ((and (pair? cli-args) (string=? (car cli-args) "-e")
            (pair? (cdr cli-args)))
       (let ((expr (if (string=? (cadr cli-args) "-") (jolt-read-all-stdin) (cadr cli-args))))
         (jolt-run-expr-string expr (drop-end-of-options (cddr cli-args)) #t)))
      ;; `-` [args…] — read a PROGRAM from stdin and run it as a script (the final
      ;; value is not echoed, like `clojure -M -`); args after it are the argv.
      ((and (pair? cli-args) (string=? (car cli-args) "-"))
       (jolt-run-expr-string (jolt-read-all-stdin) (drop-end-of-options (cdr cli-args)) #f))
      ;; otherwise dispatch the argv through jolt.main/-main
      (else
       (when (and (pair? cli-args) (string=? (car cli-args) "build"))
         (prepare-build!))
       (load-namespace "jolt.main")
       (let ((mainv (var-deref "jolt.main" "-main")))
         (apply jolt-invoke mainv cli-args))))))
