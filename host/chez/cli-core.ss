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
;; Reads, compiles, and evals each top-level form in sequence — NOT batch-wrapped
;; in (do …) — so each form is visible to the next, matching JVM and file-load
;; semantics. *allow-unresolved-vars* defaults to false; unresolved bare symbols
;; throw, matching JVM. Inside fn bodies the analyzer still late-binds so
;; defmulti/defmethod forward references work.
;;
;; The CLI auto-quotes require/use vector/list args (but NOT symbols — a plain
;; (require sym) evaluates sym normally) so `(require [my.lib :as m])` works
;; without an explicit quote, matching the convenience of JVM Clojure's ns macro.
(define (jolt-run-expr-string expr app-args print?)
  (let ((cla (if (null? app-args) jolt-nil (list->cseq app-args)))
        (end (string-length expr))
        (quote-sym (jolt-symbol #f "quote")))
    (define (already-quoted? a)
      (and (cseq? a) (cseq-list? a)
           (let ((ah (car (seq->list a))))
             (and (symbol-t? ah)
                  (string=? (symbol-t-name ah) "quote")))))
    (define (maybe-quote-require-args form)
      (if (and (cseq? form) (cseq-list? form)
               (let ((items (seq->list form)))
                 (and (pair? items)
                      (let ((h (car items)))
                        (and (symbol-t? h)
                             (let ((hn (symbol-t-name h)))
                               (or (string=? hn "require")
                                   (string=? hn "use"))))))))
          (let ((items (seq->list form)))
            (list->cseq
              (cons (car items)
                    (map (lambda (a)
                           (if (and (or (cseq? a) (jolt-vector? a))
                                    (not (already-quoted? a)))
                               (list->cseq (list quote-sym a))
                               a))
                         (cdr items)))))
          form))
    (jolt-push-thread-bindings
      (jolt-hash-map (jolt-var "clojure.core" "*command-line-args*") cla))
    (let ((result (let loop ((i 0) (result jolt-nil))
                    (if (>= i end)
                        result
                        (let-values (((form j) (rdr-read-form expr i end)))
                          (if (> j i)
                              (loop j (if (rdr-eof? form)
                                          result
                                          (jolt-compile-eval-form
                                            (maybe-quote-require-args form)
                                            "user")))
                              result))))))
      (jolt-pop-thread-bindings)
      (let ((s (jolt-final-str result)))
        (when (and print? (not (string=? s "")))
          (display s) (newline))))))

(define (jolt-cli-run cli-args prepare-build!)
  (guard (v (#t (jolt-report-uncaught v)))
    (cond
      ;; -e EXPR [args…] — evaluate one expression and print it (blank for nil).
      ;; Each top-level form is read, compiled, and evaled in sequence so each
      ;; form is visible to the next, matching JVM load semantics. The argv after
      ;; EXPR are *command-line-args* (nil when empty),
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
