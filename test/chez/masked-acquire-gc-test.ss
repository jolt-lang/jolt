;; test/chez/masked-acquire-gc-test.ss — a lock taken with interrupts disabled
;; must not stall a collection (locks.ss jolt-mutex-acquire!).
;; Run: chez --script test/chez/masked-acquire-gc-test.ss
;;
;; Chez gives up a blocked thread's place in a collect rendezvous only when its
;; interrupts are enabled (prim5.c s_mutex_acquire). A fiber park commits with
;; interrupts disabled and takes jolt mutexes there, so a commit blocked on a
;; mutex whose holder joined a collection kept the collection waiting on it while
;; it waited on the holder: the process stopped (the fibers-io gate, about one
;; run in a hundred). This builds that shape on purpose: HOLDER takes the mutex
;; and collects inside it once WAITER, interrupts disabled, is blocked on it.
;; Before the fix the collect never returns; the main thread polls with a
;; deadline so the gate fails instead of hanging.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(define (mono-secs)
  (let ((t (current-time 'time-monotonic)))
    (+ (time-second t) (/ (time-nanosecond t) 1e9))))
(define (wait-until pred secs)
  (let ((deadline (+ (mono-secs) secs)))
    (let loop ()
      (cond ((pred) #t)
            ((> (mono-secs) deadline) #f)
            (else (sleep (make-time 'time-duration 1000000 0)) (loop))))))

(define (one-round)
  (let ((mu (make-mutex))
        (held? #f) (waiter-trying? #f) (collected? #f) (waiter-got? #f))
    (fork-thread
      (lambda ()
        (jolt-with-mutex mu
          (set! held? #t)
          (wait-until (lambda () waiter-trying?) 5.0)
          ;; let the waiter reach its blocking acquire
          (sleep (make-time 'time-duration 20000000 0))
          (collect-rendezvous)
          (set! collected? #t))))
    (wait-until (lambda () held?) 5.0)
    (fork-thread
      (lambda ()
        (disable-interrupts)
        (set! waiter-trying? #t)
        (jolt-with-mutex mu (set! waiter-got? #t))
        (enable-interrupts)))
    (and (wait-until (lambda () (and collected? waiter-got?)) 10.0) #t)))

(printf "== masked-acquire: a lock taken with interrupts off does not stall a collection ==\n")
(let loop ((i 0))
  (when (< i 5)
    (let ((r (one-round)))
      (ok (format "round ~a: the holder's collect finished and the waiter got the lock" i) r)
      (if r (loop (+ i 1)) (begin (printf "masked-acquire: ~a checks, ~a failures\n" total fails) (exit 1))))))
(printf "masked-acquire: ~a checks, ~a failures\n" total fails)
(exit (if (= fails 0) 0 1))
