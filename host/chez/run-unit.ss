;; run-unit.ss — host-specific unit gate.
;;
;; Loads the checked-in seed + spine, reads test/chez/unit.edn, and for each case
;; evaluates :expr (wrapped in (do ...), as `jolt -e` does) and compares its PRINTED
;; value (jolt-repl-str, the readable `-e` printer — an :expected string for a
;; string result therefore carries its quotes) to the literal :expected string.
;; :expected :throws asserts
;; the case raises. These cover host-specific behavior (dot-forms, java statics, io,
;; reader, walk, …) that isn't in the JVM-portable corpus. Global state is reset
;; between cases for per-case isolation.
;;
;;   chez --script host/chez/run-unit.ss
(import (chezscheme))

(load "host/chez/run-gate-harness.ss")

;; JOLT_GATE_TRACE=1 compiles every case traced, the way `jolt run` compiles
;; user code (jolt-enable-trace!, which also honours JOLT_KEEP_FRAMES). Unset,
;; the gate runs untraced as it always has.
(when (let ((v (getenv "JOLT_GATE_TRACE"))) (and v (not (string=? v "")) (not (string=? v "0"))))
  (jolt-enable-trace!))
;; The loader + install source roots, so a row's (require 'stdlib.ns) resolves the
;; .clj overlay from source exactly like jolt — without this, a require of a ns
;; whose NATIVE vars exist (e.g. clojure.core.async) silently no-ops via the
;; ns-has-vars? arm and the overlay fns are missing at analysis.
(load "host/chez/loader.ss")
;; jolt.ffi's HOST half, in cli.ss's position (after the loader, per its note
;; there). Without it the gate booted a world holding jolt.ffi's Clojure half and
;; not its host half, so stdlib/jolt/ffi.clj's fully-qualified reaches for the
;; primitives (jolt.ffi/__cfn, __remember-size!) resolved to nothing — and the
;; ffi row, which asserts only :throws, passed on "Unknown class jolt.ffi"
;; instead of on the bad-type error it is written to provoke.
(load "host/chez/java/ffi.ss")
(set-source-roots! ldr-install-roots)
;; The base java.time API (stdlib/jolt/time) autoloads on first java.time.* use at
;; runtime; here we load it once BEFORE the per-case snapshot below so its
;; value-semantics arms (impl/install-seams!) and class registrations are part of
;; the stable base world. Otherwise the first java.time case would install those
;; arms after the snapshot, and zj-reset! — which prunes vars but can't unregister
;; a Scheme-level arm — would leave them dangling and break later cases. (Cold
;; autoload is covered per-process by the smoke gate.)
(load-namespace "jolt.time.base")

(load "host/chez/gate-private-tmpdir.ss")

(define (slurp path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((cs '()) (c (read-char p)))
        (if (eof-object? c) (list->string (reverse cs))
            (loop (cons c cs) (read-char p)))))))

(define cases (jolt-read-string (slurp "test/chez/unit.edn")))
(define kw-suite    (keyword #f "suite"))
(define kw-expr     (keyword #f "expr"))
(define kw-expected (keyword #f "expected"))
(define kw-throws   (keyword #f "throws"))

(load "host/chez/run-case-isolation.ss")

;; --- run ------------------------------------------------------------------------
(define pass 0)
(define fails '())              ; (suite expr msg)
(define suite-pass (make-hashtable string-hash string=?))
(define suite-total (make-hashtable string-hash string=?))
(define (bump! ht k) (hashtable-set! ht k (+ 1 (hashtable-ref ht k 0))))

;; A row that never returns used to stall the whole gate at 100% CPU with nothing
;; printed — in CI, a job that runs to its timeout with no clue which row hung. A
;; watchdog thread watches the row in flight and, past the limit, names it and
;; exits nonzero. It never interrupts a row (that could leave the runtime's locks
;; held mid-update); the process ends instead. JOLT_UNIT_ROW_TIMEOUT overrides the
;; limit in seconds.
(define row-limit-secs
  (or (let ((v (getenv "JOLT_UNIT_ROW_TIMEOUT"))) (and v (string->number v))) 120))
(define row-in-flight #f)      ; (index expr start-seconds) or #f
(fork-thread
  (lambda ()
    (let loop ()
      (sleep (make-time 'time-duration 0 1))
      (let ((r row-in-flight))
        (when (and r (> (- (time-second (current-time)) (caddr r)) row-limit-secs))
          (let ((p (current-error-port)))
            (fprintf p "\nunit gate: row ~a did not finish within ~as — hung?\n  ~a\n"
                     (car r) row-limit-secs (cadr r))
            (flush-output-port p))
          ;; exit from this thread would only unwind this thread; rt.ss's
          ;; jolt-exit-process ends the process from any thread
          (jolt-exit-process 2)))
      (loop))))

;; What a row raised, rendered: "raised" alone left a failure that happened only
;; under load with nothing to go on.
(define (raised-msg e)
  (string-append "raised "
    (guard (_ (#t "(unprintable)"))
      (jolt-repl-str (jolt-unwrap-throw e)))))

(let loop ((i 0))
  (when (< i (pvec-count cases))
    (let* ((row (pvec-nth-d cases i jolt-nil))
           (suite (jolt-get row kw-suite))
           (expr (jolt-get row kw-expr))
           (expected (jolt-get row kw-expected))
           (throws? (eq? expected kw-throws))
           (sink (open-output-string)))
      (bump! suite-total suite)
      (set! row-in-flight (list i expr (time-second (current-time))))
      (guard (e (#t (if throws?
                        (begin (set! pass (+ pass 1)) (bump! suite-pass suite))
                        (set! fails (cons (list suite expr (raised-msg e)) fails)))))
        (let ((got (jolt-repl-str
                     (parameterize ((current-output-port sink))
                       (jolt-compile-eval (string-append "(do " expr ")") "user")))))
          (cond
            (throws? (set! fails (cons (list suite expr (string-append "expected throw; got " got)) fails)))
            ((string=? got expected) (begin (set! pass (+ pass 1)) (bump! suite-pass suite)))
            (else (set! fails (cons (list suite expr
                    (string-append "want `" expected "` got `" got "`")) fails))))))
      (zj-reset!)
      (set! row-in-flight #f))
    (loop (+ i 1))))

(printf "\nunit gate: ~a/~a passed\n" pass (pvec-count cases))
(let-values (((ks vs) (hashtable-entries suite-total)))
  (for-each (lambda (p)
              (printf "  ~a/~a  ~a\n" (hashtable-ref suite-pass (car p) 0) (cdr p) (car p)))
            (list-sort (lambda (a b) (string<? (car a) (car b)))
                       (vector->list (vector-map cons ks vs)))))
(when (> (length fails) 0)
  (printf "\n~a FAIL(s):\n" (length fails))
  (for-each (lambda (f) (printf "  [~a] ~a\n    ~a\n" (car f) (caddr f) (cadr f)))
            (list-head (reverse fails) (min 40 (length fails)))))
(flush-output-port)
(exit (if (> (length fails) 0) 1 0))
