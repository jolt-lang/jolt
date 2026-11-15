;; site-compile.ss -- time Chez compiling one AOT cache's emitted .scm with the
;; non-tail site restore respelled, holding everything else fixed (jolt-4).
;;
;; The input is a cold cache built with JOLT_TRACE_EMIT=macro, where every
;; non-tail site is (jolt-site-inline CALL). After Jolt's runtime is loaded
;; (build-boot.ss, as compact-ceiling/measure.ss does) the macro is redefined:
;;   M  as rt.ss defines it: the inline restore (vregs 2 and 12 saved around CALL)
;;   P  (#%$jolt-site-call (lambda () CALL)): the restore as one primitive,
;;      open-coded by a Chez carrying host/chez/chez-patches/0008
;;   A  (#%$call-setting-continuation-attachment 0 (lambda () CALL)): the
;;      continuation-attachment push of JOLT_TRACE_EMIT=attach (stock Chez)
;;   N  CALL alone: no restore (the norestore ceiling, not trace-identical)
;; then every file is compiled with compile-file under the cache's parameters.
;;
;; Run from the repo root:
;;   chez --script benchmarks/chez-primitives/site-compile.ss VARIANT SRCDIR OUTDIR [LIMIT]
;; Prints: VARIANT files=N fail=F real_ms=R cpu_ms=C so_bytes=B
(import (chezscheme))
(load "host/chez/build-boot.ss")

(define args (cdr (command-line)))
(define variant (list-ref args 0))
(define srcdir (list-ref args 1))
(define outdir (list-ref args 2))
(define limit (if (> (length args) 3) (string->number (list-ref args 3)) #f))
(cond
  ((string=? variant "M") #f)
  ((string=? variant "P")
   (eval '(define-syntax jolt-site-inline
            (syntax-rules () ((_ call) (#%$jolt-site-call (lambda () call)))))))
  ((string=? variant "A")
   (eval '(define-syntax jolt-site-inline
            (syntax-rules () ((_ call) (#%$call-setting-continuation-attachment 0 (lambda () call)))))))
  ((string=? variant "N")
   (eval '(define-syntax jolt-site-inline (syntax-rules () ((_ call) call)))))
  (else (error 'site-compile "unknown variant" variant)))

(define (scm? f) (let ((n (string-length f))) (and (> n 4) (string=? (substring f (- n 4) n) ".scm"))))
(define files
  (let ((l (list-sort string<? (filter scm? (directory-list srcdir)))))
    (if (and limit (< limit (length l))) (list-head l limit) l)))
(define (so-of f) (string-append outdir "/" (substring f 0 (- (string-length f) 4)) ".so"))
(define (file-size p) (let* ((ip (open-file-input-port p)) (n (port-length ip))) (close-port ip) n))

(when (getenv "CC_PASS_TIMING") (#%$enable-pass-timing #t))
(collect (collect-maximum-generation))
(define fails 0)
(define r0 (real-time))
(define c0 (cpu-time))
(parameterize ((current-output-port (open-output-string)))
  (for-each (lambda (f)
              (guard (e (#t (set! fails (+ fails 1))
                            (fprintf (current-error-port) "FAIL ~a: ~a\n" f
                                     (if (condition? e)
                                         (call-with-string-output-port (lambda (p) (display-condition e p)))
                                         e))))
                (compile-file (string-append srcdir "/" f) (so-of f))))
            files))
(define real (- (real-time) r0))
(define cpu (- (cpu-time) c0))
(when (getenv "CC_PASS_TIMING")
  (parameterize ((current-output-port (current-error-port))) (#%$print-pass-stats)))
(printf "~a files=~a fail=~a real_ms=~a cpu_ms=~a so_bytes=~a\n" variant (length files) fails real cpu
        (apply + (map (lambda (f) (if (file-exists? (so-of f)) (file-size (so-of f)) 0)) files)))
