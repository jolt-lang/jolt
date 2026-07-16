;; run-gate-harness.ss — shared gate harness: boot preamble, check counter, substring scan, summary/exit.
;;
;; Loaded by every run-*.ss gate to avoid duplicating the runtime boot preamble
;; and check/fails harness. Usage:
;;   (import (chezscheme))
;;   (load "host/chez/run-gate-harness.ss")
;;   … gate-specific code …
;;   (gate-summary "name")

;; --- boot preamble ---------------------------------------------------------------
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

;; --- check counter ---------------------------------------------------------------
(define gate-fails 0)
(define gate-total 0)
(define (gate-check label actual expected)
  (set! gate-total (+ gate-total 1))
  (unless (equal? actual expected)
    (set! gate-fails (+ gate-fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

;; --- substring scan --------------------------------------------------------------
(define (gate-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))

;; --- summary / exit --------------------------------------------------------------
(define (gate-summary name)
  (if (= gate-fails 0)
      (begin (printf "~a gate: ~a/~a passed\n" name gate-total gate-total) (exit 0))
      (begin (printf "~a gate: ~a/~a passed (~a failed)\n" name (- gate-total gate-fails) gate-total gate-fails) (exit 1))))
