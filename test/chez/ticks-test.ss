;; The per-thread tick scheduler (host/chez/ticks.ss): one owner for the engine
;; timer, shared by the sampler, run-interruptible and fiber preemption. Run from
;; the repo root:
;;   chez --script test/chez/ticks-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

;; Enough calls to cross many timer intervals. The loop's calls are the polls.
(define (spin n)
  (let loop ((i 0) (acc 0))
    (if (fx=? i n) acc (loop (fx+ i 1) (fx+ acc (spin-step i))))))
(define spin-step (lambda (i) (fxand i 7)))

;; A client that counts its fires and re-registers every `period` ticks until
;; it has fired `limit` times, then cancels.
(define (counting-client slot period limit)
  (let ((n 0))
    (jolt-tick-client-set! slot
      (lambda ()
        (set! n (fx+ n 1))
        (jolt-tick-set! slot (if (fx<? n limit) period 0))))
    (lambda () n)))

(define (restore-clients!)
  (jolt-tick-client-set! jolt-tick-slot-sample (lambda () (void)))
  (jolt-tick-client-set! jolt-tick-slot-poll (lambda () (void))))

(printf "== tick scheduler ==\n")

;; --- 1. a fresh thread is solo ------------------------------------------------
(ok "1. the main thread starts solo" (eq? 0 (jolt-tick-state)))
(ok "1. cancelling a demand that is not there stays solo"
    (begin (jolt-tick-set! jolt-tick-slot-poll 0) (eq? 0 (jolt-tick-state))))

;; --- 2. one client fires on its period and leaves -----------------------------
(let ((count (counting-client jolt-tick-slot-sample 1000 50)))
  (jolt-tick-set! jolt-tick-slot-sample 1000)
  (ok "2. registering a demand leaves solo" (vector? (jolt-tick-state)))
  (spin 2000000)
  (ok "2. the client fired until it cancelled" (fx=? 50 (count)))
  (ok "2. the last demand leaving makes the thread solo again"
      (eq? 0 (jolt-tick-state)))
  (ok "2. and leaves only the watch armed"
      (let ((rem (set-timer 0)))
        (set-timer rem)
        (and (fx>? rem 0) (fx<=? rem jolt-tick-watch-ticks)))))
(restore-clients!)

;; --- 2b. the watch joins a sampler that started since, and costs nothing else ---
(let ((joined 0) (old jolt-tick-join-sampling!))
  (jolt-tick-watch!)
  (ok "2b. the watch leaves the thread solo" (eq? 0 (jolt-tick-state)))
  (spin 3000000)
  (ok "2b. it fires and re-arms itself with sampling off"
      (let ((rem (set-timer 0)))
        (set-timer rem)
        (and (eq? 0 (jolt-tick-state)) (fx>? rem 0))))
  (jolt-tick-sampling-set! #t (lambda () (set! joined (fx+ joined 1)) (set-timer jolt-tick-watch-ticks)))
  (spin 3000000)
  (jolt-tick-sampling-set! #f old)
  (ok "2b. with sampling on it joins" (fx>? joined 0))
  (let ((rem (set-timer 0)))
    (set-timer rem)
    (ok "2b. a running timer is left alone"
        (begin (jolt-tick-watch!)
               (let ((now (set-timer 0))) (set-timer now) (fx<=? now rem))))))
(set-timer 0)
(restore-clients!)

;; --- 3. two clients keep their own cadence ------------------------------------
;; The sample client at 1000 ticks and the poll at 10000: over the same run the
;; sample client fires about ten times as often. Neither is starved by the other.
(let ((fast (counting-client jolt-tick-slot-sample 1000 1000000))
      (slow (counting-client jolt-tick-slot-poll 10000 1000000)))
  (jolt-tick-set! jolt-tick-slot-sample 1000)
  (jolt-tick-set! jolt-tick-slot-poll 10000)
  (spin 3000000)
  (let ((f (fast)) (s (slow)))
    (jolt-tick-set! jolt-tick-slot-sample 0)
    (jolt-tick-set! jolt-tick-slot-poll 0)
    (ok "3. both clients fired" (and (fx>? f 0) (fx>? s 0)))
    (ok (format "3. the cadences are about ten to one (~a:~a)" f s)
        (let ((r (/ f s))) (and (> r 7) (< r 13))))
    (ok "3. cancelling both returns to solo" (eq? 0 (jolt-tick-state)))))
(restore-clients!)

;; --- 4. off a fiber there is no preempt demand --------------------------------
;; the timer holds the watch by now, which would carry over as one
(set-timer 0)
(jolt-tick-set! jolt-tick-slot-poll 1000000000)
(ok "4. leaving solo off a fiber takes no preempt demand"
    (not (jolt-tick-demand jolt-tick-slot-preempt)))
(jolt-tick-set! jolt-tick-slot-poll 0)

;; --- 5. a preempt demand survives the move to multi and back ------------------
;; Off a fiber nothing arms the timer raw, so arm it as dispatch would, then
;; register another client: the remaining ticks carry over as the preempt demand.
;; The poll demand is far off: compiling each top-level form of this script runs
;; ticks too, and a poll that fell due would end the multi state early.
(jolt-tick-preempt! 777777)
(jolt-tick-set! jolt-tick-slot-poll 1000000000)
(let ((d (jolt-tick-demand jolt-tick-slot-preempt)))
  (ok (format "5. the raw timer became the preempt demand (~a)" d)
      (and (fixnum? d) (fx<=? d 777777) (fx>? d 700000))))
(jolt-tick-set! jolt-tick-slot-poll 0)
(ok "5. back to solo" (eq? 0 (jolt-tick-state)))
(let ((rem (set-timer 0)))
  (ok (format "5. the preempt demand went back into the timer (~a)" rem)
      (and (fx<=? rem 777777) (fx>? rem 700000))))

;; --- 6. a client re-registering from inside its own fire ----------------------
;; The sample client re-arms while the dispatcher is running it; the demand
;; lands once the dispatcher re-arms, not in the middle of it.
(let ((count (counting-client jolt-tick-slot-sample 1 200)))
  (jolt-tick-set! jolt-tick-slot-sample 1)
  (spin 100000)
  (ok "6. a one-tick client fires and terminates" (fx=? 200 (count)))
  (ok "6. and leaves the thread solo" (eq? 0 (jolt-tick-state))))
(restore-clients!)

;; --- 7. the state is per thread -----------------------------------------------
;; A demand on this thread is not visible to a thread forked meanwhile, and a
;; demand there fires there.
(jolt-tick-set! jolt-tick-slot-poll 1000000000)
(let ((m (make-mutex)) (c (make-condition)) (result #f))
  (fork-thread
    (lambda ()
      (let* ((fresh (eq? 0 (jolt-tick-state)))
             (fired 0))
        (jolt-tick-client-set! jolt-tick-slot-sample
          (lambda ()
            (set! fired (fx+ fired 1))
            (jolt-tick-set! jolt-tick-slot-sample (if (fx<? fired 20) 1000 0))))
        (jolt-tick-set! jolt-tick-slot-sample 1000)
        (spin 1000000)
        (with-mutex m
          (set! result (list fresh fired (eq? 0 (jolt-tick-state))))
          (condition-broadcast c)))))
  (with-mutex m (let wait () (unless result (condition-wait c m) (wait))))
  (ok "7. a forked thread starts solo" (car result))
  (ok "7. a demand on the forked thread fires there" (fx=? 20 (cadr result)))
  (ok "7. and the forked thread returns to solo" (caddr result))
  (ok "7. this thread's demand is untouched"
      (let ((d (jolt-tick-demand jolt-tick-slot-poll)))
        (and (fixnum? d) (fx>? d 0)))))
(jolt-tick-set! jolt-tick-slot-poll 0)
(restore-clients!)

;; --- 8. a client that escapes ----------------------------------------------
;; The sample client runs with interrupts on, and anything it does can raise: it
;; allocates, and the collect-request handler raises OutOfMemoryError past the
;; heap cap. An escape must not leave the dispatcher's firing flag set, which
;; made every later demand on the thread a write nobody armed the timer for.
(let ((raised #f) (polls 0))
  (jolt-tick-client-set! jolt-tick-slot-sample
    (lambda () (unless raised (set! raised #t) (error 'sampler "out of memory"))))
  (guard (e (#t #t))
    (jolt-tick-set! jolt-tick-slot-sample 1000)
    (spin 1000000))
  (ok "8. the escaping client ran" raised)
  (jolt-tick-set! jolt-tick-slot-sample 0)
  (jolt-tick-client-set! jolt-tick-slot-poll
    (lambda ()
      (set! polls (fx+ polls 1))
      (jolt-tick-set! jolt-tick-slot-poll (if (fx<? polls 20) 1000 0))))
  (jolt-tick-set! jolt-tick-slot-poll 1000)
  (spin 2000000)
  (ok "8. demands still fire after a client escaped" (fx=? polls 20))
  (ok "8. and the thread settles back to solo" (eq? 0 (jolt-tick-state))))
(restore-clients!)

;; --- 9. the sampler's demand ------------------------------------------------
;; Ticks per nanosecond as measured, bounded: a short reading may double the
;; demand but not jump it to the ceiling.
(ok "9. a steady rate keeps the demand" (= 10000 (jolt-sample-next-demand 10000 1000000 1000000)))
(ok "9. half the time left halves it" (= 5000 (jolt-sample-next-demand 10000 500000 1000000)))
(ok "9. a clock that did not move doubles it" (= 20000 (jolt-sample-next-demand 10000 1000000 0)))
(ok "9. a reading 100x short doubles it, no more" (= 20000 (jolt-sample-next-demand 10000 1000000 10000)))
(ok "9. never under the floor" (= jolt-sample-min-ticks (jolt-sample-next-demand 1000 1 1000000)))

(printf "ticks-test: ~a checks, ~a failure(s)\n" total fails)
(exit (if (fx=? fails 0) 0 1))
