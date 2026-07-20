;; Shared CLI dispatch for the two joltc entry points: the script-mode driver
;; (cli.ss under `chez --script`) and the standalone binary's launcher
;; (build-joltc.ss scheme-start). Both call jolt-cli-run so the -e handling,
;; end-of-options rule, and uncaught-throw reporting cannot drift apart — the
;; binary once carried a stale copy of the -e arm.

;; Flush stdout/stderr on EVERY exit path. Buffered stdout is otherwise lost
;; when the process ends while helper threads are winding down (the throwing
;; (thread …) smoke flake: pr writes "nil" with no newline, so *flush-on-newline*
;; never fires, and the exit-time console flush raced the async thread's
;; teardown on loaded CI runs — stderr survived only because the __eprint seam
;; flushes each write). Installing it as the exit handler covers explicit
;; (exit n) calls from jolt.main as well as the normal return path. Each flush
;; is guarded: a broken pipe must not turn exit into a second crash.
(let ((base (exit-handler)))
  (exit-handler
    (lambda args
      (guard (_ (#t #f)) (flush-output-port (current-output-port)))
      (guard (_ (#t #f)) (flush-output-port (current-error-port)))
      (apply base args))))

;; --- machine-readable diagnostics (JOLT_DIAG=edn) ---------------------------
;; When JOLT_DIAG=edn, an uncaught error is emitted as a single-line EDN map to
;; stderr instead of the human report, so editors/tooling get structured data.
;; An analyzer diagnostic (e.g. unresolved symbol) attaches a :jolt/error map
;; {:type :symbol :suggestions :ns}; those fields are lifted to the top of the
;; diagnostic, alongside the human :message and the current form's :line/:column/
;; :file. A plain error still yields {:message ... :line ... :column ...}.
(define diag-kw-jolt-error (keyword "jolt" "error"))
(define diag-kw-message (keyword #f "message"))
(define diag-kw-line (keyword #f "line"))
(define diag-kw-column (keyword #f "column"))
(define diag-kw-file (keyword #f "file"))

(define (jolt-diag-machine?)
  (let ((e (getenv "JOLT_DIAG")))
    (and e (string? e) (string-ci=? e "edn"))))

;; Build the EDN diagnostic map for an unwrapped throw value.
(define (jolt-diagnostic-map v)
  (let* ((msg (cond ((jolt-ex-info-record? v)
                     (jolt-str-render-one (jolt-ex-info-record-message v)))
                    ((condition? v)
                     (with-output-to-string (lambda () (display-condition v))))
                    (else (jolt-pr-str v))))
         (data (and (jolt-ex-info-record? v) (jolt-ex-info-record-data v)))
         (err (and data (pmap? data) (jolt-get data diag-kw-jolt-error jolt-nil)))
         (base (if (and err (pmap? err)) err (jolt-hash-map)))
         (pos (jolt-current-source))
         (m (jolt-assoc base diag-kw-message msg)))
    (if (pmap? pos)
        (let ((line (jolt-get pos diag-kw-line jolt-nil))
              (col (jolt-get pos diag-kw-column jolt-nil))
              (file (jolt-get pos diag-kw-file jolt-nil)))
          (let* ((m (if (jolt-nil? line) m (jolt-assoc m diag-kw-line line)))
                 (m (if (jolt-nil? col) m (jolt-assoc m diag-kw-column col)))
                 (m (if (jolt-nil? file) m (jolt-assoc m diag-kw-file file))))
            m))
        m)))

;; Render an uncaught jolt throw (any value, not just a Chez condition) to stderr
;; and exit non-zero, instead of Chez's opaque "non-condition value" dump. The
;; message/ex-data/cause + a mapped Clojure backtrace come from the shared
;; renderer (source-registry.ss); this adds the top-level source location. Under
;; JOLT_DIAG=edn the human report is replaced by one EDN diagnostic line.
(define (jolt-report-uncaught raw)
  (let ((v (jolt-unwrap-throw raw))
        (port (current-error-port)))
    (if (jolt-diag-machine?)
        ;; jolt-pr-readable, not jolt-pr-str: strings must be quoted so the line
        ;; is valid EDN a tool can read back.
        (begin (display (jolt-pr-readable (jolt-diagnostic-map v)) port) (newline port))
        (begin
          (jolt-render-throwable v port)
          ;; The top-level form that was evaluating when this propagated (file:line:col).
          (let ((loc (jolt-current-source-string)))
            (when loc (display "  at " port) (display loc port) (newline port)))
          (let ((bt (jolt-backtrace-string v)))
            (when bt (display "  trace:\n" port) (display bt port)))))
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
                                          ;; Compile each form in the CURRENT ns, re-read
                                          ;; per form (like load-jolt-file) — an (ns …) form
                                          ;; switches it, so a later (refer …)/def and its
                                          ;; use land in the same namespace. Hardcoding
                                          ;; "user" lost mappings a runtime refer added to
                                          ;; the switched-to ns (jolt#… stdin ns-switch bug).
                                          (jolt-compile-eval-form
                                            (maybe-quote-require-args form)
                                            (chez-current-ns))))
                              result))))))
      (jolt-pop-thread-bindings)
      (let ((s (jolt-final-str result)))
        (when (and print? (not (string=? s "")))
          (display s) (newline))))))

(define (jolt-cli-run cli-args prepare-build!)
  (guard (v (#t (jolt-report-uncaught v)))
    (jolt-cli-dispatch cli-args prepare-build!)
    ;; normal-return twin of the exit-handler flush above
    (guard (_ (#t #f)) (flush-output-port (current-output-port)))))

(define (jolt-cli-dispatch cli-args prepare-build!)
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
         (apply jolt-invoke mainv cli-args)))))
