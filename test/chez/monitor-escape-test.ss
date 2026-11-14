;; An interrupt that lands inside a monitor operation must not leave it held.
;; Run from repo root:
;;   chez --script test/chez/monitor-escape-test.ss
;;
;; run-interruptible ends its body by jumping out of a timer interrupt, so any
;; event check can be where a `locking` stops. This test interrupts a body with the
;; token already set and moves the tick its handler fires on across the whole
;; enter, body and exit, so every check point in between is the escape point once.
;; Afterwards the monitor must be free (or held at the depth an enclosing section
;; still owns), the wait set empty, and this thread's interrupt-disable and
;; counted-lock depths back where they started.
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "FAIL: ~a~n" name)))

(define sweep-ticks 400)

;; Run thunk under run-interruptible, interrupted from the start, with its handler
;; due after t ticks. 'interrupted when it escaped, else thunk's value.
(define (interrupt-at t thunk)
  (let ((token (jolt-make-interrupt)))
    (jolt-interrupt! token)
    (guard (e ((let ((v (jolt-unwrap-throw e)))
                 (and (jolt-ex-info-record? v)
                      (string=? "Evaluation interrupted" (jolt-ex-info-record-message v))))
               'interrupted))
      (jolt-run-interruptible token (lambda () (set-timer t) (thunk))))))

(define (owner m) (vector-ref m monitor-i-owner))
(define (depth m) (vector-ref m monitor-i-count))
(define (free? m) (not (owner m)))

;; Sweep body over every tick. setup prepares the monitor and answers a thunk that
;; waits for whatever it started; check is the state the monitor must be in after.
;; Answers #t, or reports the first tick that broke it and answers #f.
(define (sweep setup body check)
  (let loop ((t 1) (interrupted 0))
    (if (> t sweep-ticks)
        (or (positive? interrupted)
            (begin (printf "  never interrupted~n") #f))
        (let* ((m (make-monitor))
               (d0 (sa-disable-count))
               (l0 (jolt-locks-held))
               (settle (setup m))
               (r (guard (e (#t (list 'raised (condition/report-string* e))))
                    (interrupt-at t (lambda () (body m)))))
               (_ (settle)))
          (if (and (not (and (pair? r) (eq? (car r) 'raised)))
                   (check m)
                   (null? (vector-ref m monitor-i-waiters))
                   (= d0 (sa-disable-count))
                   (= l0 (jolt-locks-held)))
              (loop (+ t 1) (if (eq? r 'interrupted) (+ interrupted 1) interrupted))
              (begin
                (printf "  tick ~a: owner ~s depth ~a waiters ~a disable ~a->~a locks ~a->~a result ~s~n"
                        t (owner m) (depth m) (length (vector-ref m monitor-i-waiters))
                        d0 (sa-disable-count) l0 (jolt-locks-held) r)
                #f))))))

(define (condition/report-string* e)
  (let ((v (jolt-unwrap-throw e)))
    (if (condition? v)
        (call-with-string-output-port (lambda (p) (display-condition v p)))
        v)))

(define (no-setup m) void)

;; 1. Uncontended: the fast claim and the release.
(ok "uncontended locking never leaks the monitor"
    (sweep no-setup
           (lambda (m) (jolt-call-with-monitor m (lambda () 'body)))
           free?))

;; 2. Reentrant: the interrupt lands in an inner section and is caught inside the
;; outer one, which must still hold the monitor exactly once.
(ok "reentrant locking keeps the outer hold"
    (sweep no-setup
           (lambda (m)
             (jolt-call-with-monitor m
               (lambda ()
                 (interrupt-at (random 200) (lambda () (jolt-call-with-monitor m (lambda () 'body))))
                 (unless (and (eq? (owner m) (monitor-self)) (= 1 (depth m)))
                   (error 'reentrant "outer hold lost" (owner m) (depth m))))))
           free?))

;; 3. Contended: another thread holds the monitor for a moment, so the enter takes
;; the slow path (mark the word, wait under bk, claim on the wake).
(define (hold-briefly m)
  (let ((mu (make-mutex)) (cv (make-condition)) (stage 0))
    (define (await n) (with-mutex mu (let wait () (unless (>= stage n) (condition-wait cv mu) (wait)))))
    (define (advance! n) (with-mutex mu (set! stage n) (condition-broadcast cv)))
    (fork-thread
      (lambda ()
        (monitor-enter! m)
        (advance! 1)
        (sleep (make-time 'time-duration 300000 0))
        (monitor-exit! m)
        (advance! 2)))
    (await 1)
    (lambda () (await 2))))
(ok "contended locking never leaks the monitor"
    (sweep hold-briefly
           (lambda (m) (jolt-call-with-monitor m (lambda () 'body)))
           free?))

;; 3b. Contended with a second waiter. A release signals ONE waiting thread
;; (monitor-wake-entry!), so whoever claims next must leave the word marked for
;; the waiter still behind it, and an escape swept across this thread's enter,
;; body and exit must not take that wake away. The other waiter must finish
;; within its deadline on every tick.
(define second-waiter-done? #f)
(define (hold-with-second-waiter m)
  (let ((mu (make-mutex)) (cv (make-condition)) (stage 0) (v-done #f))
    (define (await n) (with-mutex mu (let wait () (unless (>= stage n) (condition-wait cv mu) (wait)))))
    (define (advance! n) (with-mutex mu (set! stage (max stage n)) (condition-broadcast cv)))
    (fork-thread
      (lambda ()
        (monitor-enter! m)
        (advance! 1)
        (sleep (make-time 'time-duration 1000000 0))
        (monitor-exit! m)
        (advance! 2)))
    (await 1)
    (fork-thread
      (lambda ()
        (monitor-enter! m)
        (monitor-exit! m)
        (with-mutex mu (set! v-done #t) (condition-broadcast cv))))
    (lambda ()
      (await 2)
      (let ((deadline (add-duration (current-time) (make-time 'time-duration 0 2))))
        (with-mutex mu
          (let wait ()
            (unless (or v-done (time>=? (current-time) deadline))
              (condition-wait cv mu (make-time 'time-duration 50000000 0))
              (wait))))
        (set! second-waiter-done? (with-mutex mu v-done))))))
(ok "the next claim keeps the wake for the waiter behind it"
    (sweep hold-with-second-waiter
           (lambda (m) (jolt-call-with-monitor m (lambda () 'body)))
           (lambda (m) (and second-waiter-done? (free? m)))))

;; 4. Object.wait: released inside the wait and taken back at the saved depth.
;; Held twice, so a wait that comes back at the wrong depth shows up as a held
;; monitor or an IllegalMonitorState from the outer section's exit.
(ok "Object.wait under nested locking never leaks or loses the monitor"
    (sweep no-setup
           (lambda (m)
             (jolt-call-with-monitor m
               (lambda ()
                 (jolt-call-with-monitor m
                   (lambda () (monitor-object-wait! m 1))))))
           free?))

;; Chez mutexes are recursive, so only another thread can tell whether this one
;; still holds mu.
(define (free-from-another-thread? mu)
  (let ((done (make-mutex)) (cv (make-condition)) (answer 'pending))
    (fork-thread
      (lambda ()
        (let ((got (mutex-acquire mu #f)))
          (when got (mutex-release mu))
          (with-mutex done (set! answer got) (condition-broadcast cv)))))
    (with-mutex done
      (let wait () (when (eq? answer 'pending) (condition-wait cv done) (wait))))
    answer))

;; 5. The lock under everything: an interrupt is held off while this thread holds
;; a counted lock, so jolt-with-mutex neither keeps the count up nor the mutex held.
(ok "jolt-with-mutex is never left half-entered or half-exited"
    (let ((mu (make-mutex)))
      (sweep no-setup
             (lambda (m) (jolt-with-mutex mu 'body))
             (lambda (m) (free-from-another-thread? mu)))))

(printf "~a/~a monitor escape assertions passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
