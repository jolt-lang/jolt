;; Shared CLI dispatch for the two jolt entry points: the script-mode driver
;; (cli.ss under `chez --script`) and the standalone binary's launcher
;; (build-jolt.ss scheme-start). Both call jolt-cli-run so the -e handling,
;; end-of-options rule, and uncaught-throw reporting cannot drift apart — the
;; binary once carried a stale copy of the -e arm. jolt.main's own (project-aware)
;; -e arm calls back into jolt-run-expr-string here for the same reason.

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
(define diag-kw-message (keyword #f "message"))
(define diag-kw-line (keyword #f "line"))
(define diag-kw-column (keyword #f "column"))
(define diag-kw-file (keyword #f "file"))

(define (jolt-diag-machine?)
  (let ((e (getenv "JOLT_DIAG")))
    (and e (string? e) (string-ci=? e "edn"))))

;; Build the EDN diagnostic map for an unwrapped throw value.
;;
;; The diagnostic's own keys are already flat and namespaced, so they pass
;; straight through and only :message and a fallback position are added. The
;; loader's per-top-level-form position FILLS IN only what the diagnostic did not
;; carry: a diagnostic knows the innermost form it failed in, and letting the
;; coarser position overwrite it is what reported a defn's opening line for a
;; symbol hundreds of lines inside it.
(define (jolt-diagnostic-map raw v)
  (let* ((msg (cond ((jolt-ex-info-record? v)
                     (jolt-str-render-one (jolt-ex-info-record-message v)))
                    ((condition? v)
                     (with-output-to-string (lambda () (display-condition v))))
                    (else (jolt-pr-str v))))
         (data (and (jolt-ex-info-record? v) (jolt-ex-info-record-data v)))
         (base (if (pmap? data) data (jolt-hash-map)))
         (pos (or (jolt-throw-source-position raw) (jolt-current-source)))
         (m (jolt-assoc base diag-kw-message msg)))
    (if (pmap? pos)
        (let ((fill (lambda (m src-k dst-k)
                      (let ((v (jolt-get pos src-k jolt-nil)))
                        (if (or (jolt-nil? v)
                                (not (jolt-nil? (jolt-get m dst-k jolt-nil))))
                            m
                            (jolt-assoc m dst-k v))))))
          (fill (fill (fill m diag-kw-line diag-kw-err-line)
                      diag-kw-column diag-kw-err-column)
                diag-kw-file diag-kw-err-file))
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
        (begin (display (jolt-pr-readable (jolt-diagnostic-map raw v)) port) (newline port))
        (let ((diag (jolt-analyzer-diagnostic v)))
          (jolt-render-throwable v port)
          ;; Where it failed: the diagnostic's own position, the form a file load
          ;; was at when this crossed it, or the top-level form evaluating now
          ;; (jolt-throwable-source-string, compile-eval.ss).
          (let ((loc (jolt-throwable-source-string raw)))
            (when loc (display "  at " port) (display loc port) (newline port)))
          ;; No trace for a compile-time diagnostic. It is raised while ANALYZING,
          ;; so the frames are jolt's own analyzer recursing into the form
          ;; (analyze-list, analyze-seq, map-seq, …) — thirty lines of internals
          ;; that bury the message and the location, and name nothing the reader
          ;; can act on. A runtime error's trace is unchanged.
          (unless diag
            (let ((bt (jolt-backtrace-string v)))
              (when bt (display "  trace:\n" port) (display bt port))))))
    (exit 1)))

;; POSIX end-of-options: drop the first standalone "--" in an argv list; any
;; later "--" stays literal program data. Returns a Scheme list.
(define (drop-end-of-options args)
  (let loop ((in args) (acc '()))
    (cond
      ((null? in) (reverse acc))
      ((string=? (car in) "--") (append (reverse acc) (cdr in)))
      (else (loop (cdr in) (cons (car in) acc))))))

;; Dispatch a jolt argv. prepare-build! runs before jolt.main dispatches a
;; `build` — the script driver loads the build driver from the repo, the
;; standalone binary materializes its bundled boots/stub (build.ss itself is
;; already inlined there).
;; Read all of stdin as a string (a `-` program / expression source). Through the
;; one port jolt opens on fd 0 (io-streams.ss), the same one System/in and
;; read-line read: a second port on that descriptor buffers ahead on its own, and
;; the program source would take input the program then went looking for.
(define (jolt-read-all-stdin)
  (let ((bv (get-bytevector-all (jolt-stdin-binary-port))))
    (if (eof-object? bv) "" (utf8->string bv))))

;; Evaluate EXPR (a string of one-or-more forms) with *command-line-args* bound
;; to app-args. print? echoes the final value (blank for nil), as `-e` does; a
;; `-` stdin PROGRAM runs as a script and suppresses it.
;; Reads, compiles, and evals each top-level form in sequence — NOT batch-wrapped
;; in (do …) — so each form is visible to the next, matching JVM and file-load
;; semantics. *allow-unresolved-vars* defaults to false; an unresolved bare symbol
;; throws wherever it appears — top level or nested body — matching JVM. A forward
;; reference needs a declare, as it does on the JVM.
;;
;; The CLI auto-quotes require/use vector/list args (but NOT symbols — a plain
;; (require sym) evaluates sym normally) so `(require [my.lib :as m])` works
;; without an explicit quote, matching the convenience of JVM Clojure's ns macro.
;;
;; The convenience belongs to clojure.core's require/use, not to the SPELLING of
;; the head symbol. Forms are read and evaluated one at a time, so a dialect that
;; defines its own `use` (or `require`) macro earlier in the same stdin program
;; has already interned it by the time the next form is rewritten — and it owns
;; the quoting of its own arguments. So the head is resolved the way the compiler
;; will resolve it (a locally defined var, then a :refer, then clojure.core) and
;; auto-quoting applies only when that lands on clojure.core's own var.
(define (jolt-run-expr-string expr app-args print?)
  (let ((cla (if (null? app-args) jolt-nil (list->cseq app-args)))
        (end (string-length expr))
        (quote-sym (jolt-symbol #f "quote")))
    (define (already-quoted? a)
      (and (cseq? a)
           (let ((ah (car (seq->list a))))
             (and (symbol-t? ah)
                  (string=? (symbol-t-name ah) "quote")))))
    ;; Does HEAD name clojure.core's require/use? Resolved to the (ns . name) pair
    ;; the compiler would reach — a locally DEFINED var shadows, then a :refer
    ;; (which carries the name it was renamed FROM), else the implicit core refer.
    ;; Deliberately compares the resolved pair rather than dereferencing
    ;; clojure.core/require by name: a literal (var-cell-lookup "clojure.core"
    ;; "require") here would be a runtime shim reference that has to be pinned as
    ;; a dce-runtime-core-roots entry, which would keep require/use (and the whole
    ;; loader behind them) unshakeable in every tree-shaken app.
    (define (core-require-head? h)
      (and (symbol-t? h)
           (let* ((nm (symbol-t-name h))
                  (sns (symbol-t-ns h))
                  (qualified (and sns (not (jolt-nil? sns)) (not (null? sns)) sns))
                  (here (chez-current-ns))
                  (target
                    (if qualified
                        ;; a qualified ns may be a require :as alias
                        (cons (or (chez-resolve-alias here qualified) qualified) nm)
                        (cond ((let ((c (var-cell-lookup here nm)))
                                 (and c (var-cell-defined? c)))
                               (cons here nm))
                              ((chez-resolve-refer here nm) => values)
                              (else (cons "clojure.core" nm))))))
             (and (string=? (car target) "clojure.core")
                  (or (string=? (cdr target) "require")
                      (string=? (cdr target) "use"))))))
    (define (maybe-quote-require-args form)
      (if (and (cseq? form)
               (let ((items (seq->list form)))
                 (and (pair? items) (core-require-head? (car items)))))
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
    ;; clojure.main wraps every entry — repl, -e, -m — in with-bindings, so a
    ;; top-level (set! *warn-on-reflection* true) has a thread-local slot to
    ;; write. jolt's REPL already binds them; -e (and the `-` stdin script, which
    ;; comes through here too) bound only *command-line-args*, so the same
    ;; expression that works from a file or the REPL raised "Can't
    ;; change/establish root binding" here.
    ;; Both frames are scoped, not bracketed by hand: a form that throws mid-loop
    ;; used to skip the pops and leave them standing for the rest of the thread.
    ;; That is invisible from `-e`, which exits through the uncaught handler, but
    ;; run-expr-string is also the -M/-A/-Sdeps re-dispatch path, which does not.
    (jolt-with-thread-bindings
      (jolt-hash-map (jolt-var "clojure.core" "*command-line-args*") cla)
      (lambda ()
       (jolt-with-ns-load-vars
        (lambda ()
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
      (let ((s (jolt-repl-str result)))
        (when (and print? (not (string=? s "")))
          (display s) (newline))))))))))

;; Expose the evaluator (and the stdin reader it pairs with) to Clojure. jolt.main's
;; -e arm — the project-aware path, reached from -Sdeps/-A/-M, an alias's
;; :main-opts ["-e" …], or a project dir that has a deps.edn — evaluates through
;; this same primitive, so the launcher's -e and jolt.main's -e cannot drift the
;; way the binary's forked copy of the -e arm once did.
(def-var! "jolt.host" "run-expr-string"
  (lambda (expr app-args print?)
    (jolt-run-expr-string expr (seq->list app-args) (jolt-truthy? print?))
    jolt-nil))
(def-var! "jolt.host" "read-all-stdin" (lambda () (jolt-read-all-stdin)))

;; Does the project dir have a config file — a deps.edn, or a bb.edn, which
;; jolt.deps reads for the same :paths / :deps / :tasks? The launcher's -e / -
;; arms below skip jolt.main entirely — no deps chain, no project source roots,
;; no natives — which is only equivalent to the real thing when there is no
;; project to resolve. With one present the argv falls through to jolt.main
;; instead, so `jolt -e "(require 'my.app)"` sees the project's paths and deps
;; like every other command.
(define (jolt-project-deps-edn?)
  (let* ((pwd (getenv "JOLT_PWD"))
         (dir (if (and pwd (string? pwd) (not (string=? pwd ""))) pwd ".")))
    (or (file-exists? (string-append dir "/deps.edn"))
        (file-exists? (string-append dir "/bb.edn")))))

;; Is this argv a `build`? The build driver has to be loaded before jolt.main
;; runs, and the command can sit behind the global options that re-dispatch the
;; rest of the argv through -main (-Sdeps '<edn>', -A:aliases), so peel those off
;; the same way -main does instead of only testing the head — `jolt -Sdeps '…'
;; build -m app.core` used to reach cmd-build with jolt.host/build-binary unbound.
(define (jolt-cli-build-cmd? args)
  (cond
    ((null? args) #f)
    ((string=? (car args) "build") #t)
    ((string=? (car args) "-Sdeps")
     (and (pair? (cdr args)) (jolt-cli-build-cmd? (cddr args))))
    ((and (>= (string-length (car args)) 2) (string=? (substring (car args) 0 2) "-A"))
     (jolt-cli-build-cmd? (cdr args)))
    (else #f)))

(define (jolt-cli-run cli-args prepare-build!)
  ;; Every entry starts in user, as clojure.main's does. The image bakes jolt.main
  ;; and jolt.deps at heap build, and loading a namespace leaves it current, so
  ;; without this a bare -e or a REPL evaluated in jolt.main: (str *ns*) said so,
  ;; and a REPL def landed as #'jolt.main/x under a prompt that said user. The
  ;; script driver arrives with user already current; here it costs nothing.
  (set-chez-ns! "user")
  ;; On the main thread, before anything user code can reach: Chez's exit-handler
  ;; is a thread parameter, so the shutdown-hook wrapper has to be installed on
  ;; the thread that will call (exit), and this is that thread for both CLI
  ;; entries. Without it a hook registered from a worker would be invisible to a
  ;; System/exit here.
  (jolt-install-exit-handler!)
  (guard (v (#t (jolt-report-uncaught v)))
    ;; Host faults (a condition raised outside jolt-throw) get their k / marks /
    ;; site captured HERE: a with-exception-handler runs before the stack
    ;; unwinds, where the frames still exist — the guard above runs after, when
    ;; they are gone. jolt throws skip the capture (jolt-capture-fault! tests),
    ;; warnings pass through untouched, and raise-continuable preserves a
    ;; continuable raise's resume semantics.
    (with-exception-handler
      (lambda (c)
        (when (serious-condition? c) (jolt-capture-fault! c))
        (raise-continuable c))
      (lambda () (jolt-cli-dispatch cli-args prepare-build!)))
    ;; normal-return twin of the exit-handler above. A `chez --script` that
    ;; returns instead of calling (exit) ends the process without ever running
    ;; the exit handler, so the shutdown hooks have to be run from here as well —
    ;; the runner is once-per-process, so whichever path gets there first wins.
    (guard (_ (#t #f)) (jolt-run-shutdown-hooks!))
    (guard (_ (#t #f)) (flush-output-port (current-output-port)))))

(define (jolt-cli-dispatch cli-args prepare-build!)
    (cond
      ;; -e EXPR [args…] — evaluate one expression and print it (blank for nil).
      ;; Each top-level form is read, compiled, and evaled in sequence so each
      ;; form is visible to the next, matching JVM load semantics. The argv after
      ;; EXPR are *command-line-args* (nil when empty),
      ;; with the first standalone "--" consumed as POSIX end-of-options. `-e -`
      ;; reads the expression from stdin.
      ;; Taken only when there is no project deps.edn to resolve — otherwise
      ;; jolt.main's -e arm handles it, resolving the project first.
      ((and (pair? cli-args) (string=? (car cli-args) "-e")
            (pair? (cdr cli-args)) (not (jolt-project-deps-edn?)))
       (let ((expr (if (string=? (cadr cli-args) "-") (jolt-read-all-stdin) (cadr cli-args))))
         (jolt-run-expr-string expr (drop-end-of-options (cddr cli-args)) #t)))
      ;; `-` [args…] — read a PROGRAM from stdin and run it as a script (the final
      ;; value is not echoed, like `clojure -M -`); args after it are the argv.
      ((and (pair? cli-args) (string=? (car cli-args) "-")
            (not (jolt-project-deps-edn?)))
       (jolt-run-expr-string (jolt-read-all-stdin) (drop-end-of-options (cdr cli-args)) #f))
      ;; otherwise dispatch the argv through jolt.main/-main
      (else
       (when (jolt-cli-build-cmd? cli-args)
         (prepare-build!))
       (load-namespace "jolt.main")
       (let ((mainv (var-deref "jolt.main" "-main")))
         (apply jolt-invoke mainv cli-args)))))
