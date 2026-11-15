;; measure.ss -- time Chez compiling a directory of emitted .scm files the way
;; Jolt's AOT cache does (loader.ss aot-publish-artifact!: compile-file, under
;; the compiler parameters the jolt binary runs with), inside a Chez process that
;; has Jolt's whole runtime loaded (build-boot.ss, the preamble build-jolt.ss
;; compiles the stdlib fasls under), so the runtime's macros -- image-fn-form-src
;; among them -- expand exactly as in a real build.
;;
;; Run from the repo root (build-boot.ss loads by relative path):
;;   chez --script benchmarks/chez-primitives/compact-ceiling/measure.ss \
;;        LABEL SRCDIR OUTDIR INSPECTOR(#t|#f|source) RUNS
;;
;; Prints one line per run and a summary line:
;;   LABEL ii=#t files=N fail=F real_ms=MEDIAN cpu_ms=MEDIAN (runs) so_bytes=B
;; The defaults (optimize-level 0, inspector information on) reproduce the
;; cache's .so byte for byte; INSPECTOR #f turns inspector information off.
(import (chezscheme))
(load "host/chez/build-boot.ss")

(define args (cdr (command-line)))
(define label (list-ref args 0))
(define srcdir (list-ref args 1))
(define outdir (list-ref args 2))
(define mode (list-ref args 3))
(define ii (string=? mode "#t"))
(define rp (guard (e (#t #f)) (eval 'generate-return-point-source-information)))
(when (and (string=? mode "source") (not rp))
  (error 'measure "source mode requires Chez patch 0015"))
(when rp (rp (string=? mode "source")))
;; $uncprep runs outside CC_PASS_TIMING. Time its calls separately when asked;
;; use nanoseconds because most individual calls take less than a millisecond.
(define uncprep-calls 0)
(define uncprep-ns 0)
(define (thread-ns)
  (let ((t (current-time 'time-thread)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))
(when (getenv "CC_UNCPREP_TIMING")
  (let ((original (#%$top-level-value '$uncprep)))
    (#%$set-top-level-value! '$uncprep
      (lambda args
        (let* ((start (thread-ns)) (result (apply original args)))
          (set! uncprep-ns (+ uncprep-ns (- (thread-ns) start)))
          (set! uncprep-calls (+ uncprep-calls 1))
          result)))))
(define runs (string->number (list-ref args 4)))

(define (scm? f) (let ((n (string-length f))) (and (> n 4) (string=? (substring f (- n 4) n) ".scm"))))
(define files (list-sort string<? (filter scm? (directory-list srcdir))))
(define (so-of f) (string-append outdir "/" (substring f 0 (- (string-length f) 4)) ".so"))
(define (file-size p) (let* ((ip (open-file-input-port p)) (n (port-length ip))) (close-port ip) n))

(when (getenv "CC_PASS_TIMING") (#%$enable-pass-timing #t))
(define fails 0)
(define (one-run)
  (set! fails 0)
  (collect (collect-maximum-generation))
  (let ((r0 (real-time)) (c0 (cpu-time)))
    (parameterize ((generate-inspector-information ii)
                   (current-output-port (open-output-string)))
      (for-each (lambda (f)
                  (guard (e (#t (set! fails (+ fails 1))
                                (fprintf (current-error-port) "FAIL ~a: ~a\n" f
                                         (if (condition? e)
                                             (call-with-string-output-port (lambda (p) (display-condition e p)))
                                             e))))
                    (compile-file (string-append srcdir "/" f) (so-of f))))
                files))
    (cons (- (real-time) r0) (- (cpu-time) c0))))

(define results
  (let loop ((i 0) (acc '()))
    (if (= i runs)
        (reverse acc)
        (let ((r (one-run)))
          (printf "  ~a run ~a: real ~a ms, cpu ~a ms\n" label (+ i 1) (car r) (cdr r))
          (loop (+ i 1) (cons r acc))))))
(define (median xs) (let ((s (list-sort < xs))) (list-ref s (quotient (length s) 2))))
;; CC_PASS_TIMING=1: Chez's per-pass compiler timings over all runs (to stderr,
;; slows the compile; for finding where the time goes, not for the table)
(when (getenv "CC_PASS_TIMING")
  (parameterize ((current-output-port (current-error-port))) (#%$print-pass-stats)))
(when (getenv "CC_UNCPREP_TIMING")
  (fprintf (current-error-port) "uncprep: calls=~a cpu_ms=~a\n"
           uncprep-calls (/ uncprep-ns 1000000.0)))
(printf "~a ii=~a files=~a fail=~a real_ms=~a cpu_ms=~a ~s so_bytes=~a\n"
        label ii (length files) fails (median (map car results)) (median (map cdr results))
        (map car results)
        (apply + (map (lambda (f) (if (file-exists? (so-of f)) (file-size (so-of f)) 0)) files)))
