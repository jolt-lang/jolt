;; Fibers: an object monitor across a fiber switch (jolt-3a87).
;;
;; Every lock in the runtime routes through the counting wrapper in locks.ss, and
;; the scheduler refuses to preempt a fiber that holds one. locks.ss states the
;; premise that makes that sound: such regions are SHORT — measured at ~55ns mean
;; against a 0.45ms quantum — and they never span a park, because either
;; jolt-with-mutex rewinds the acquire or the site releases by hand before parking.
;;
;; That premise is true of every lock in the runtime and false of exactly one, the
;; object monitor behind clojure.core/locking and the bare (monitor-enter x) /
;; (monitor-exit x) special forms. A monitor wraps arbitrary user code: the region is
;; as long as the body, and the body may park. Treating it as one of the short ones
;; failed three ways, and this gate is the three.
;;
;; It also pins what the fix rests on, which is that ownership is a FIELD and not
;; the OS mutex. A field survives a switch; an OS mutex cannot, because it has thread
;; granularity and fibers multiplex one thread — and Chez mutexes are recursive per
;; thread, so a sibling fiber's acquire SUCCEEDS and walks into the section rather
;; than failing where it could be noticed.
;;
;; Loads the full runtime: the monitors live in java/concurrency.ss and the gate
;; needs real channels to park on.
(import (chezscheme))
(load "host/chez/rt.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(define (mono-secs)
  (let ((t (current-time 'time-monotonic)))
    (+ (exact->inexact (time-second t)) (/ (time-nanosecond t) 1e9))))
(define (wait-until pred secs what)
  (let ((deadline (+ (mono-secs) secs)))
    (let loop ()
      (cond ((pred) #t)
            ((> (mono-secs) deadline)
             (set! fails (+ fails 1))
             (printf "  FAIL: ~a (timed out)\n" what)
             #f)
            (else (sleep (make-time 'time-duration 2000000 0)) (loop))))))

(printf "== object monitors across a fiber switch ==\n")

;; ONE carrier throughout the synchronous sections: two fibers on one carrier is the
;; only configuration in which a fiber can observe another fiber's monitor at all,
;; because same-carrier fibers are the only ones that share a thread. sa-fiber-run-all
;; drains on THIS thread, which makes the interleaving exact instead of timed.
(jolt-fiber-carrier-count-set! 1)

;; An occupancy counter, which is the property under test said directly: how many
;; executions are between enter and exit of ONE monitor at the same moment. Mutual
;; exclusion is "this never exceeds 1", and nothing weaker is worth asserting — a
;; monitor that is merely acquired and released in some order proves nothing.
(define occ 0)
(define occ-max 0)
(define (occ-in!) (set! occ (+ occ 1)) (when (> occ occ-max) (set! occ-max occ)))
(define (occ-out!) (set! occ (- occ 1)))
(define (occ-reset!) (set! occ 0) (set! occ-max 0))

(define trace '())
(define (note! x) (set! trace (cons x trace)))
(define (trace-reset!) (set! trace '()))
(define (trace-out) (reverse trace))

;; --- 1. `locking` around a park keeps the monitor -----------------------------
;; jolt-with-monitor used to be a plain dynamic-wind, so a park unwound it: the
;; monitor was released mid-body and re-taken on resume. Two fibers on one carrier
;; then both got in — traced (a-in b-in a-resumed a-out b-resumed b-out), occupancy
;; 2 — and when the second one's resume re-entered, monitor-enter! found the owner
;; equal to its own identity, because the owner WAS the carrier thread, which both
;; fibers share. So it took the reentrant arm and never noticed.
(occ-reset!)
(trace-reset!)
(define l1-obj (vector 'l1))
(define l1-a-ch (ac-make 1 'fixed #f))
(define l1-b-ch (ac-make 1 'fixed #f))
(define (l1-body tag ch)
  (lambda ()
    (jolt-with-monitor l1-obj
      (lambda ()
        (occ-in!) (note! (string->symbol (string-append tag "-in")))
        (jolt-fiber-<! ch)
        (note! (string->symbol (string-append tag "-resumed")))
        (occ-out!) (note! (string->symbol (string-append tag "-out")))))))
(define l1-a (sa-fiber-spawn (l1-body "a" l1-a-ch)))
(define l1-b (sa-fiber-spawn (l1-body "b" l1-b-ch)))

;; A enters and parks. B wants the monitor and must WAIT for it, so B never reaches
;; its own channel and the drain ends with both fibers parked.
(sa-fiber-run-all)
(ok "1. only one fiber is inside the monitor while the other waits" (= 1 occ-max))
(ok "1. B never entered while A held it" (equal? '(a-in) (trace-out)))

;; Feed A. It resumes inside the body, leaves, releases — and only then does B get in.
(jolt-async-give l1-a-ch 1)
(sa-fiber-run-all)
(jolt-async-give l1-b-ch 2)
(sa-fiber-run-all)
(ok "1. both fibers completed"
    (and (eq? 'done (jolt-fiber-state l1-a)) (eq? 'done (jolt-fiber-state l1-b))))
(ok "1. exclusion held across both parks" (= 1 occ-max))
(ok "1. and the bodies did not interleave"
    (equal? '(a-in a-resumed a-out b-in b-resumed b-out) (trace-out)))
(ok "1. the monitor is free afterwards" (= 0 occ))

;; --- 2. the bare halves are the same monitor, and the same rule ---------------
;; (monitor-enter x) / (monitor-exit x) have no dynamic-wind at all, so a park in
;; between could not even release the monitor by accident — it simply kept the OS
;; mutex across the switch. Two things followed and both are checked here: the next
;; fiber on the carrier saw jolt-locks-held stuck above zero, which makes the whole
;; carrier unpreemptible for as long as the first fiber stays parked, and its own
;; monitor-enter recursed on the shared thread and let it straight in.
(occ-reset!)
(trace-reset!)
(define l2-obj (vector 'l2))
(define l2-ch (ac-make 1 'fixed #f))
(define l2-locks-seen 'unset)
(define l2-a
  (sa-fiber-spawn
   (lambda ()
     (jolt-monitor-enter l2-obj)
     (occ-in!) (note! 'a-in)
     (jolt-fiber-<! l2-ch)
     (occ-out!) (note! 'a-out)
     (jolt-monitor-exit l2-obj))))
(define l2-b
  (sa-fiber-spawn
   (lambda ()
     ;; read BEFORE contending: this is the carrier's count as the next fiber finds
     ;; it, which is what the preempt handler reads.
     (set! l2-locks-seen (jolt-locks-held))
     (jolt-monitor-enter l2-obj)
     (occ-in!) (note! 'b-in)
     (occ-out!) (note! 'b-out)
     (jolt-monitor-exit l2-obj))))
(sa-fiber-run-all)
(ok "2. the carrier holds no lock while a fiber is parked inside a monitor"
    (eqv? 0 l2-locks-seen))
(ok "2. the sibling fiber did not re-enter the held monitor" (equal? '(a-in) (trace-out)))
(jolt-async-give l2-ch 1)
(sa-fiber-run-all)
(ok "2. it got in once the holder left"
    (equal? '(a-in a-out b-in b-out) (trace-out)))
(ok "2. exclusion held for the bare halves too" (= 1 occ-max))
(ok "2. both fibers completed"
    (and (eq? 'done (jolt-fiber-state l2-a)) (eq? 'done (jolt-fiber-state l2-b))))

;; --- 3. a monitor body is preemptible -----------------------------------------
;; The other half of routing a monitor through the counting wrapper: the count stayed
;; up for the whole body, so the scheduler refused to preempt anywhere inside it.
;; With one carrier, a fiber spinning inside (locking o …) starved everything queued
;; behind it — 0 preemptions, and the queued fiber ran only once the monitor was
;; released. That is the unbounded starvation window fibers.ss says no setting opens.
;; Real carrier threads here: the point is that the SCHEDULER takes the carrier away.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! 20000)

(define l3-obj (vector 'l3))
(define l3-stop (box #f))
(define l3-b-ran (box #f))
(define l3-before (jolt-fiber-preempts))
(define (l3-spin-until b) (let loop () (unless (unbox b) (loop))))
(define l3-a
  (sa-fiber-spawn
   (lambda () (jolt-with-monitor l3-obj (lambda () (l3-spin-until l3-stop))) 'a)))
(define l3-b (sa-fiber-spawn (lambda () (set-box! l3-b-ran #t) 'b)))
(jolt-fiber-ensure-carrier!)
(define l3-ran-while-held?
  (wait-until (lambda () (unbox l3-b-ran)) 5.0
              "3. the queued fiber runs while the monitor is held"))
(ok "3. a queued fiber is not starved by a long monitor body"
    (and l3-ran-while-held? (not (unbox l3-stop))))
(ok "3. the scheduler actually preempted the holder" (> (jolt-fiber-preempts) l3-before))
(set-box! l3-stop #t)
(wait-until (lambda () (eq? 'done (jolt-fiber-state l3-a))) 5.0 "3. the holder completes")
(ok "3. the holder still completed normally" (eq? 'done (jolt-fiber-state l3-a)))

;; --- 4. a preempted monitor body still excludes -------------------------------
;; The check that says the fix is not "stop protecting the region": with the body now
;; preemptible, a contended monitor under a floor quantum must still serialize. Four
;; fibers on ONE carrier, each incrementing an unsynchronized counter inside the
;; monitor, with a burn either side so preemptions fall due mid-body. Losing exclusion
;; loses increments.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min)

(define L4-N 4)
(define L4-PER 500)
(define l4-obj (vector 'l4))
(define l4-count 0)
(define l4-occ-max 0)
(define l4-occ 0)
(define l4-done 0)
(define l4-before (jolt-fiber-preempts))
(define (l4-burn n) (let loop ((i 0) (a 0)) (if (fx=? i n) a (loop (fx+ i 1) (fx+ a i)))))
(do ((i 0 (+ i 1))) ((= i L4-N))
  (sa-fiber-spawn
   (lambda ()
     (let loop ((j 0))
       (if (< j L4-PER)
           (begin
             (jolt-with-monitor l4-obj
               (lambda ()
                 (set! l4-occ (+ l4-occ 1))
                 (when (> l4-occ l4-occ-max) (set! l4-occ-max l4-occ))
                 (l4-burn 300)
                 (set! l4-count (+ l4-count 1))
                 (set! l4-occ (- l4-occ 1))))
             (l4-burn 100)
             (loop (+ j 1)))
           (set! l4-done (+ l4-done 1)))))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (= l4-done L4-N)) 120.0 "4. every fiber finished")
(ok "4. no monitored update lost under preemption" (= l4-count (* L4-N L4-PER)))
(ok "4. never two fibers inside the monitor at once" (= 1 l4-occ-max))
(ok "4. preemption fired during the run" (> (jolt-fiber-preempts) l4-before))
(ok "4. no lock left held on this thread" (= 0 (jolt-locks-held)))

;; --- 5. a thread and a fiber contend the same monitor -------------------------
;; Cross-thread is what the OS mutex was there for, and it has to keep working now
;; that ownership is a field. A fiber takes the monitor and parks inside it; a plain
;; thread then asks for the same monitor and must wait, not walk in — and must not
;; wait forever once the fiber leaves.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! #f)

(define l5-obj (vector 'l5))
(define l5-ch (ac-make 1 'fixed #f))
(define l5-in (box #f))
(define l5-thread-got (box #f))
(define l5-thread-saw-occ (box 'unset))
(define l5-occ (box 0))
(define l5-a
  (sa-fiber-spawn
   (lambda ()
     (jolt-with-monitor l5-obj
       (lambda ()
         (set-box! l5-occ (+ 1 (unbox l5-occ)))
         (set-box! l5-in #t)
         (jolt-fiber-<! l5-ch)
         (set-box! l5-occ (- (unbox l5-occ) 1)))))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (unbox l5-in)) 5.0 "5. the fiber took the monitor")
(define l5-t
  (fork-thread
   (lambda ()
     (jolt-with-monitor l5-obj
       (lambda ()
         (set-box! l5-thread-saw-occ (unbox l5-occ))
         (set-box! l5-thread-got #t))))))
(sleep (make-time 'time-duration 100000000 0))   ; 100ms: long enough to prove it waits
(ok "5. a thread does not enter a monitor a parked fiber holds" (not (unbox l5-thread-got)))
(jolt-async-give l5-ch 1)
(wait-until (lambda () (unbox l5-thread-got)) 5.0 "5. the thread gets in once the fiber leaves")
(thread-join l5-t)
(ok "5. and it found the monitor empty when it did" (eqv? 0 (unbox l5-thread-saw-occ)))
;; waited for rather than asserted outright, for the reason 9h spells out: an OS
;; thread waiter runs alongside the carrier, so its progress does not mean the
;; fiber that let it in has been published 'done yet.
(define l5-done
  (wait-until (lambda () (eq? 'done (jolt-fiber-state l5-a))) 5.0 "5. the fiber completed"))
(ok "5. the fiber completed" l5-done)

;; --- 6. the properties a monitor already had ----------------------------------
;; Regressions, all of them on the fiber that the sections above changed the
;; implementation for. Reentrancy is the one that matters most: it is what keeps a
;; nested (locking x …) on one object from deadlocking on itself, and the owner
;; identity moved, which is exactly the field that decides it.
(define l6-obj (vector 'l6))
(define l6-nested (box #f))
(define l6-threw (box #f))
(define l6-released (box #f))
(define l6-f
  (sa-fiber-spawn
   (lambda ()
     ;; reentrant on the same object, from the same fiber
     (jolt-with-monitor l6-obj
       (lambda ()
         (jolt-with-monitor l6-obj (lambda () (set-box! l6-nested #t)))))
     ;; a throw inside the body is a real exit, so the monitor is released
     (guard (e (#t (set-box! l6-threw #t)))
       (jolt-with-monitor l6-obj (lambda () (error 'l6 "boom"))))
     (jolt-with-monitor l6-obj (lambda () (set-box! l6-released #t)))
     'done)))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (memq (jolt-fiber-state l6-f) '(done dead))) 5.0 "6. the fiber finishes")
(ok "6. a nested locking on one object re-enters" (unbox l6-nested))
(ok "6. a throw inside the body still propagates" (unbox l6-threw))
(ok "6. and still releases the monitor" (unbox l6-released))
(ok "6. the fiber completed rather than died" (eq? 'done (jolt-fiber-state l6-f)))

;; monitor-exit by something that does not own it is still an error, on a fiber as
;; on a thread — the bare halves are reachable from user code and unbalanced use has
;; to be reported rather than corrupt the count.
(define l7-obj (vector 'l7))
(ok "6. monitor-exit without the monitor throws"
    (guard (e (#t #t)) (jolt-monitor-exit l7-obj) #f))

;; --- 7. a fiber's own unwind releases, even past its vreg ---------------------
;; Ownership by FIBER has one consequence that is not obvious and does not fail
;; loudly. jolt-fiber-done!/dead! clear the current-fiber vreg BEFORE escaping, and
;; the escape is what runs the fiber's winders — so a wind belonging to the fiber
;; arrives at monitor-exit! off-fiber even though it is the owner.
;;
;; java/sm.ss makes that reachable on purpose: it handles a throwing CPS'd body with
;; with-exception-handler rather than guard, so the handler runs at the raise point
;; and jolt-fiber-dead! runs BEFORE the winders instead of after them. Refusing there
;; left the monitor held for the life of the process and raised
;; IllegalMonitorStateException out of an after-thunk, mid-escape, on top of a fiber
;; that was already dying — so the next fiber to want that monitor simply waited
;; forever with nothing to say why.
;;
;; The shape below is jolt-sm-drive's, written out rather than driven through the
;; compiler, because what is under test is the handler discipline and not the CPS
;; pass. fibers-sm-test.ss and run-gosm.ss cover the pass.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(occ-reset!)
(define l8-obj (vector 'l8))
(define l8-later (box #f))
(define l8-a
  (sa-fiber-spawn
   (lambda ()
     (let ((f (jolt-current-fiber)))
       (with-exception-handler
         (lambda (e) (jolt-fiber-dead! f e))
         (lambda ()
           (jolt-with-monitor l8-obj (lambda () (occ-in!) (error 'l8 "boom")))))))))
(guard (e (#t #f)) (sa-fiber-run-all))
(ok "7. the fiber died" (eq? 'dead (jolt-fiber-state l8-a)))
(define l8-b
  (sa-fiber-spawn (lambda () (jolt-with-monitor l8-obj (lambda () (set-box! l8-later #t))))))
(let loop ((i 0))
  (when (and (< i 3) (not (unbox l8-later)))
    (guard (e (#t #f)) (sa-fiber-run-all))
    (loop (+ i 1))))
(ok "7. and its unwind released the monitor it was holding" (unbox l8-later))
(ok "7. so a later fiber gets it" (eq? 'done (jolt-fiber-state l8-b)))
;; The narrowness is the point: the arm that accepts a terminal fiber's own unwind
;; must not accept a LIVE fiber's monitor being released by anything else.
(define l9-obj (vector 'l9))
(define l9-held (box #f))
(define l9-ch (ac-make 1 'fixed #f))
(define l9-a
  (sa-fiber-spawn
   (lambda () (jolt-monitor-enter l9-obj) (set-box! l9-held #t) (jolt-fiber-<! l9-ch)
              (jolt-monitor-exit l9-obj))))
(sa-fiber-run-all)
(ok "7. a live fiber's monitor cannot be released off-fiber"
    (and (unbox l9-held)
         (guard (e (#t #t)) (jolt-monitor-exit l9-obj) #f)))
(jolt-async-give l9-ch 1)
(sa-fiber-run-all)
(ok "7. and the holder still releases it itself" (eq? 'done (jolt-fiber-state l9-a)))

(jolt-fiber-pool-reset!)

;; --- 8. the OTHER two locks that wrap user code -------------------------------
;; Sections 1-7 are about the object monitor, which is the lock this file was
;; written for. It is not the only one whose region is a user body: jolt-sync
;; (refs.ss) and jolt-delay-force (java/concurrency.ss) both ran theirs inside
;; jolt-with-mutex, and locks.ss's premise — regions are SHORT and never span a
;; park — is false of both for exactly the reason it was false of the monitor.
;;
;; So they are the same three failures, and they belong here rather than beside
;; the STM and delay unit tests, because what is under test is the lock and not
;; the transaction or the memoization.
;;
;; 8a. THE TRANSACTION'S ISOLATION (jolt-pb2s). jolt's STM has no MVCC and no
;; retry: the global lock IS the isolation, so a park that drops it mid-body drops
;; the only mechanism there is, and nothing afterwards can detect the conflict.
;; Unfixed, the second transaction below ran to completion inside the first one's
;; extent and the first then committed a value derived from a read that predated
;; it — r = 1, where the only serializable outcomes are 100 (a then b) and 101
;; (b then a).
(jolt-fiber-carrier-count-set! 1)
(define l10-r (jolt-ref-new 0))
(define l10-ch (ac-make 1 'fixed #f))
(define l10-read (box #f))
;; a: read the ref, park mid-transaction, then write read+1.
(define l10-a
  (sa-fiber-spawn
   (lambda ()
     (jolt-sync
      (lambda ()
        (let ((v (jolt-ref-deref l10-r)))
          (set-box! l10-read v)
          (jolt-fiber-<! l10-ch)
          (jolt-ref-set l10-r (+ v 1))))))))
(sa-fiber-run-all)
(ok "8a. the first transaction parked inside its body"
    (and (eqv? 0 (unbox l10-read)) (eq? 'parked (jolt-fiber-state l10-a))))
;; b: a whole transaction, start to finish, while a is parked.
(define l10-b (sa-fiber-spawn (lambda () (jolt-sync (lambda () (jolt-ref-set l10-r 100))))))
(sa-fiber-run-all)
(ok "8a. a second transaction cannot run inside the first one's extent"
    (eq? 'parked (jolt-fiber-state l10-b)))
(ok "8a. and cannot have committed" (eqv? 0 (jolt-ref-val l10-r)))
;; let a finish; b then gets the lock and commits after it.
(jolt-async-give l10-ch 1)
(sa-fiber-run-all)
(let loop ((i 0))
  (when (and (< i 5) (not (eq? 'done (jolt-fiber-state l10-b))))
    (sa-fiber-run-all)
    (loop (+ i 1))))
(ok "8a. both transactions finished"
    (and (eq? 'done (jolt-fiber-state l10-a)) (eq? 'done (jolt-fiber-state l10-b))))
(ok "8a. the outcome is serializable (a then b)" (eqv? 100 (jolt-ref-val l10-r)))

;; 8a'. AND THE TRANSACTION ENDS WHEN IT ENDS (jolt-49ay). The lock is only half of
;; it: *txn* has to come back to nil too, and it did not. Chez's parameterize is a
;; SWAP — one thunk is both the wind's before and its after, exchanging the
;; parameter with a saved slot — and jolt-fiber-slice-restore! writes *txn* BEFORE
;; the rewind runs that swap, so the saved slot came back holding the transaction
;; instead of the outer nil, and the way out then restored the transaction. The
;; fiber left its dosync still inside one, and its next dosync took the NESTED arm,
;; joined the dead transaction, took no lock at all and wrote into a log nobody
;; would ever commit. Three parked transactions in a row committed once.
;;
;; Pre-existing and reachable with no preemption at all, which is why the quantum is
;; pinned long here: the park is the whole mechanism.
(jolt-fiber-preempt-ticks-set! 100000000)
(define l10b-r (jolt-ref-new 0))
(define l10b-seen '())
(define l10b-f
  (sa-fiber-spawn
   (lambda ()
     (let loop ((i 0))
       (when (< i 3)
         (set! l10b-seen (cons (if (*txn*) 'in-a-txn 'clean) l10b-seen))
         (jolt-sync
          (lambda ()
            (let ((v (jolt-ref-deref l10b-r)))
              (jolt-fiber-park!)                 ; park INSIDE the transaction
              (jolt-ref-set l10b-r (+ v 1)))))
         (loop (+ i 1)))))))
(let loop ((i 0))
  (when (and (< i 20) (not (memq (jolt-fiber-state l10b-f) '(done dead))))
    (sa-fiber-run-all)
    (when (eq? 'parked (jolt-fiber-state l10b-f)) (sa-fiber-resume l10b-f))
    (loop (+ i 1))))
(ok "8a'. the fiber finished its three transactions"
    (eq? 'done (jolt-fiber-state l10b-f)))
(ok "8a'. each dosync started with no transaction running"
    (equal? (reverse l10b-seen) '(clean clean clean)))
(ok "8a'. so each one committed" (eqv? 3 (jolt-ref-val l10b-r)))
;; The contended version: distinct transactions, and no update lost. This is the
;; shape that measured 4 of 400 with the leak in place.
(jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min)
(define l10c-r (jolt-ref-new 0))
(define l10c-txns '())
(define l10c-N 40)
(define (l10c-work)
  (let loop ((i 0))
    (when (< i l10c-N)
      (jolt-sync
       (lambda ()
         (let ((t (*txn*)))
           (unless (memq t l10c-txns) (set! l10c-txns (cons t l10c-txns))))
         (let ((v (jolt-ref-deref l10c-r)))
           ;; burn ticks so a preemption lands inside the transaction
           (let spin ((k 0)) (when (fx<? k 2000) (spin (fx+ k 1))))
           (jolt-ref-set l10c-r (+ v 1)))))
      (loop (+ i 1)))))
(define l10c-fs
  (list (sa-fiber-spawn l10c-work) (sa-fiber-spawn l10c-work)
        (sa-fiber-spawn l10c-work) (sa-fiber-spawn l10c-work)))
(define l10c-before (jolt-fiber-preempts))
(let loop ((i 0))
  (when (and (< i 20000)
             (not (for-all (lambda (f) (memq (jolt-fiber-state f) '(done dead))) l10c-fs)))
    (sa-fiber-run-all)
    (loop (+ i 1))))
(ok "8a'. control: preemptions did land inside the transactions"
    (> (jolt-fiber-preempts) l10c-before))
(ok "8a'. every transaction was its own" (= (* 4 l10c-N) (length l10c-txns)))
(ok "8a'. and no update was lost" (= (* 4 l10c-N) (jolt-ref-val l10c-r)))
(jolt-fiber-preempt-ticks-set! #f)

;; 8b. AND THE BODY IS PREEMPTIBLE (jolt-d7l5). Reason 3 of the monitor's three:
;; a counted lock held for the body makes the fiber unpreemptible for the body,
;; and a transaction body is arbitrary user code. Asserted on the count directly
;; rather than by timing a starving sibling — jolt-locks-held is what the
;; scheduler reads, so it is the property itself.
(define l11-held (box #f))
(define l11-f
  (sa-fiber-spawn
   (lambda () (jolt-sync (lambda () (set-box! l11-held (jolt-locks-held)))))))
(sa-fiber-run-all)
(ok "8b. a transaction body holds no counted lock" (eqv? 0 (unbox l11-held)))
(ok "8b. and the transaction still ran" (eq? 'done (jolt-fiber-state l11-f)))

;; 8c. THE DELAY BODY RUNS ONCE (jolt-232k). Same shape: a park inside the body
;; released the delay's mutex, the second forcer found realized? still false and
;; ran the body again, and the two forcers came back with different values from
;; one delay. Delay.deref is `synchronized` on the JVM, so the second one waits.
(define l12-runs (box 0))
(define l12-ch (ac-make 1 'fixed #f))
(define l12-d
  (jolt-make-delay
   (lambda ()
     (set-box! l12-runs (+ 1 (unbox l12-runs)))
     (jolt-fiber-<! l12-ch))))
(define l12-va (box 'none))
(define l12-vb (box 'none))
(define l12-a (sa-fiber-spawn (lambda () (set-box! l12-va (jolt-delay-force l12-d)))))
(sa-fiber-run-all)
(ok "8c. the first forcer parked inside the delay body"
    (and (eqv? 1 (unbox l12-runs)) (eq? 'parked (jolt-fiber-state l12-a))))
(define l12-b (sa-fiber-spawn (lambda () (set-box! l12-vb (jolt-delay-force l12-d)))))
(sa-fiber-run-all)
(ok "8c. a second forcer does not enter the body" (eqv? 1 (unbox l12-runs)))
(ok "8c. it waits" (eq? 'parked (jolt-fiber-state l12-b)))
(jolt-async-give l12-ch 7)
(sa-fiber-run-all)
(let loop ((i 0))
  (when (and (< i 5) (not (eq? 'done (jolt-fiber-state l12-b))))
    (sa-fiber-run-all)
    (loop (+ i 1))))
(ok "8c. the body ran exactly once" (eqv? 1 (unbox l12-runs)))
(ok "8c. and both forcers see the same value"
    (and (eqv? 7 (unbox l12-va)) (eqv? 7 (unbox l12-vb))))
;; The delay contract the fix must not break: a throwing body is realized WITH its
;; condition, re-throws on every deref, and never re-runs.
(define l13-runs (box 0))
(define l13-d
  (jolt-make-delay (lambda () (set-box! l13-runs (+ 1 (unbox l13-runs))) (error 'l13 "boom"))))
(define (l13-force) (guard (e (#t 'threw)) (jolt-delay-force l13-d)))
(ok "8c. a throwing delay body throws" (eq? 'threw (l13-force)))
(ok "8c. re-throws on the next deref" (eq? 'threw (l13-force)))
(ok "8c. and did not re-run" (eqv? 1 (unbox l13-runs)))
(ok "8c. a delay that threw still reads as realized" (jolt-delay-realized? l13-d))
;; And the body still holds no counted lock, so a long delay body is preemptible.
(define l14-held (box #f))
(define l14-d (jolt-make-delay (lambda () (set-box! l14-held (jolt-locks-held)) 'v)))
(define l14-f (sa-fiber-spawn (lambda () (jolt-delay-force l14-d))))
(sa-fiber-run-all)
(ok "8c. a delay body holds no counted lock" (eqv? 0 (unbox l14-held)))
(ok "8c. and the delay still forced"
    (and (eq? 'done (jolt-fiber-state l14-f)) (eq? 'v (jolt-delay-force l14-d))))

;; --- 9. the FOURTH lock that wraps user code: ReentrantLock (jolt-ga8o) --------
;; java.util.concurrent.locks.ReentrantLock is the last lock in the runtime that
;; both spans a user body and recorded its owner as the OS THREAD. Sections 1-8
;; swept the other three; this one was left holding a Chez mutex from .lock to
;; .unlock with (current-interrupt-box) as the owner, which is precisely the
;; trapdoor those sections are about, so it failed in the same two ways:
;;
;;   - the owner two fibers on one carrier read is the SAME box, so the second
;;     one's .lock took the reentrant arm and walked into the section. Measured
;;     occupancy 2, and a fiber could .unlock a lock a sibling took.
;;   - the counted acquire was held for the whole body, and indefinitely if the
;;     holder parked inside it, so the carrier's lock count never came back to
;;     zero and every later fiber on it became unpreemptible.
;;
;; A ReentrantLock is NOT the object's monitor — (locking lk …) and (.lock lk) are
;; distinct locks on the JVM and stay distinct here — so it gets its own monitor
;; record rather than reusing object-monitor's. What it shares is the mechanism:
;; ownership is a field, the bookkeeping mutex is held only across the decision,
;; and a fiber contender parks.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! #f)

(define (rl-method nm)
  (or (host-method-ref "reentrant-lock" nm)
      (error 'rl-method "no such reentrant-lock method" nm)))
(define (rl-new) ((hashtable-ref class-ctors-tbl "ReentrantLock" #f)))
(define rl-lock (rl-method "lock"))
(define rl-unlock (rl-method "unlock"))
(define rl-trylock (rl-method "tryLock"))
(define rl-locked? (rl-method "isLocked"))
(define rl-hold-count (rl-method "getHoldCount"))
(define rl-mine? (rl-method "isHeldByCurrentThread"))

;; 9a. The exclusion property said directly, in the shape section 1 uses for
;; `locking`: A takes the lock, parks inside the section, and B must not get in.
(occ-reset!)
(trace-reset!)
(define r1-lk (rl-new))
(define r1-a-ch (ac-make 1 'fixed #f))
(define r1-b-ch (ac-make 1 'fixed #f))
(define (r1-body tag ch)
  (lambda ()
    (rl-lock r1-lk)
    (occ-in!) (note! (string->symbol (string-append tag "-in")))
    (jolt-fiber-<! ch)
    (note! (string->symbol (string-append tag "-resumed")))
    (occ-out!) (note! (string->symbol (string-append tag "-out")))
    (rl-unlock r1-lk)))
(define r1-a (sa-fiber-spawn (r1-body "a" r1-a-ch)))
(define r1-b (sa-fiber-spawn (r1-body "b" r1-b-ch)))
(sa-fiber-run-all)
(ok "9a. only one fiber is inside the lock while the other waits" (= 1 occ-max))
(ok "9a. B never entered while A held it" (equal? '(a-in) (trace-out)))
(ok "9a. and B WAITED by parking, not by blocking the carrier"
    (eq? 'parked (jolt-fiber-state r1-b)))
;; 9b. The count the scheduler reads is back to zero while A is parked holding the
;; lock — otherwise every later fiber on this carrier is unpreemptible for as long
;; as A stays away.
(ok "9b. a parked holder leaves no counted lock on the carrier" (= 0 (jolt-locks-held)))
;; Let A out; only then does B get in.
(jolt-async-give r1-a-ch 1)
(sa-fiber-run-all)
(jolt-async-give r1-b-ch 2)
(sa-fiber-run-all)
(ok "9a. both fibers completed"
    (and (eq? 'done (jolt-fiber-state r1-a)) (eq? 'done (jolt-fiber-state r1-b))))
(ok "9a. exclusion held across both parks" (= 1 occ-max))
(ok "9a. and the bodies did not interleave"
    (equal? '(a-in a-resumed a-out b-in b-resumed b-out) (trace-out)))
(ok "9a. the lock is free afterwards" (not (rl-locked? r1-lk)))

;; 9c. A fiber must not be able to release a sibling's lock. Under thread
;; ownership the sibling's unlock passed the owner test, cleared the owner and
;; released the OS mutex the holder was relying on — silently.
(define r2-lk (rl-new))
(define r2-ch (ac-make 1 'fixed #f))
(define r2-a (sa-fiber-spawn (lambda () (rl-lock r2-lk) (jolt-fiber-<! r2-ch) (rl-unlock r2-lk))))
(define r2-threw (box 'unset))
(define r2-b
  (sa-fiber-spawn
   (lambda () (set-box! r2-threw (guard (e (#t 'threw)) (rl-unlock r2-lk) 'released)))))
(sa-fiber-run-all)
(ok "9c. a sibling fiber cannot unlock the holder's lock" (eq? 'threw (unbox r2-threw)))
(ok "9c. so the lock is still held" (rl-locked? r2-lk))
(jolt-async-give r2-ch 1)
(sa-fiber-run-all)
(ok "9c. and the holder still releases it itself"
    (and (eq? 'done (jolt-fiber-state r2-a)) (not (rl-locked? r2-lk))))

;; 9d/9e. The properties the rewrite must not break, plus the two the JVM answers
;; differently: getHoldCount is the CURRENT context's count (0 for anyone else)
;; and isHeldByCurrentThread has to mean the current FIBER.
(define r3-lk (rl-new))
(define r3-ch (ac-make 1 'fixed #f))
(define r3-counts (box '()))
(define (r3-note! v) (set-box! r3-counts (cons v (unbox r3-counts))))
(define r3-a
  (sa-fiber-spawn
   (lambda ()
     (rl-lock r3-lk) (r3-note! (list 'depth1 (rl-hold-count r3-lk) (rl-mine? r3-lk)))
     (rl-lock r3-lk) (r3-note! (list 'depth2 (rl-hold-count r3-lk)))
     (rl-unlock r3-lk) (r3-note! (list 'back-to-1 (rl-hold-count r3-lk) (rl-locked? r3-lk)))
     (jolt-fiber-<! r3-ch)
     (rl-unlock r3-lk) (r3-note! (list 'released (rl-hold-count r3-lk) (rl-locked? r3-lk))))))
(sa-fiber-run-all)
(ok "9d. reentrant for the same fiber"
    (equal? '((depth1 1 #t) (depth2 2) (back-to-1 1 #t)) (reverse (unbox r3-counts))))
(define r3-other (box 'unset))
(define r3-b
  (sa-fiber-spawn
   (lambda () (set-box! r3-other (list (rl-hold-count r3-lk) (rl-mine? r3-lk))))))
(sa-fiber-run-all)
(ok "9e. getHoldCount is 0 for a non-owner and isHeldByCurrentThread is per-fiber"
    (equal? '(0 #f) (unbox r3-other)))
(jolt-async-give r3-ch 1)
(sa-fiber-run-all)
(ok "9d. and it unwinds to free" (not (rl-locked? r3-lk)))

;; 9f. tryLock with no timeout: #f while a sibling fiber holds it, and a failed
;; attempt leaves no counted lock behind on the carrier.
(define r4-lk (rl-new))
(define r4-ch (ac-make 1 'fixed #f))
(define r4-a (sa-fiber-spawn (lambda () (rl-lock r4-lk) (jolt-fiber-<! r4-ch) (rl-unlock r4-lk))))
(define r4-got (box 'unset))
(define r4-held-after (box 'unset))
(define r4-b
  (sa-fiber-spawn
   (lambda () (set-box! r4-got (rl-trylock r4-lk))
              (set-box! r4-held-after (jolt-locks-held)))))
(sa-fiber-run-all)
(ok "9f. tryLock refuses a lock a sibling fiber holds" (eq? #f (unbox r4-got)))
(ok "9f. and a refused tryLock counts no lock" (eqv? 0 (unbox r4-held-after)))
(jolt-async-give r4-ch 1)
(sa-fiber-run-all)
(define r4-c (sa-fiber-spawn (lambda () (rl-trylock r4-lk))))
(sa-fiber-run-all)
(ok "9f. and takes it once free" (eq? #t (jolt-fiber-result r4-c)))

;; 9g. A BOUNDED wait must be able to succeed when the holder is a sibling fiber
;; on the same carrier. Real carrier threads, because the point is that waiting
;; for the timeout must not stop the carrier from running the very fiber whose
;; release the waiter is waiting for. Polling with (sleep) did exactly that, so
;; this always answered false however long the timeout was.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(define r5-lk (rl-new))
(define r5-ch (ac-make 1 'fixed #f))
(define r5-in (box #f))
(define r5-got (box 'unset))
(define r5-a
  (sa-fiber-spawn
   (lambda () (rl-lock r5-lk) (set-box! r5-in #t) (jolt-fiber-<! r5-ch) (rl-unlock r5-lk))))
(define r5-b
  (sa-fiber-spawn (lambda () (set-box! r5-got (rl-trylock r5-lk 3000 (host-static-ref "TimeUnit" "MILLISECONDS"))))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (unbox r5-in)) 5.0 "9g. the holder took the lock")
(sleep (make-time 'time-duration 100000000 0))   ; 100ms inside the bounded wait
(ok "9g. the bounded waiter has not got it yet" (eq? 'unset (unbox r5-got)))
(jolt-async-give r5-ch 1)
(wait-until (lambda () (not (eq? 'unset (unbox r5-got)))) 10.0 "9g. the bounded wait ends")
(ok "9g. a timed tryLock succeeds when a sibling fiber releases in time"
    (eq? #t (unbox r5-got)))
(ok "9g. the holder completed" (eq? 'done (jolt-fiber-state r5-a)))

;; 9h. Cross-thread still works, which is what the OS mutex was there for: a
;; plain thread must wait for a lock a parked fiber holds, and must get it once
;; the fiber leaves.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(define r6-lk (rl-new))
(define r6-ch (ac-make 1 'fixed #f))
(define r6-in (box #f))
(define r6-thread-got (box #f))
(define r6-a
  (sa-fiber-spawn
   (lambda () (rl-lock r6-lk) (set-box! r6-in #t) (jolt-fiber-<! r6-ch) (rl-unlock r6-lk))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (unbox r6-in)) 5.0 "9h. the fiber took the lock")
(define r6-t
  (fork-thread
   (lambda () (rl-lock r6-lk) (set-box! r6-thread-got #t) (rl-unlock r6-lk))))
(sleep (make-time 'time-duration 100000000 0))
(ok "9h. a thread does not enter a lock a parked fiber holds" (not (unbox r6-thread-got)))
(jolt-async-give r6-ch 1)
(wait-until (lambda () (unbox r6-thread-got)) 5.0 "9h. the thread gets it once the fiber leaves")
(thread-join r6-t)
;; WAITED FOR, not asserted outright: the waiter here is an OS thread, so it runs
;; concurrently with the carrier rather than behind it. The fiber's rl-unlock is
;; what lets the thread in, and the fiber still has to return from its body and be
;; published 'done after that — a window the thread can win, and did on CI.
(define r6-done
  (wait-until (lambda () (eq? 'done (jolt-fiber-state r6-a))) 5.0
              "9h. the fiber completed"))
(ok "9h. the fiber completed" r6-done)
(ok "9h. and the lock is free" (not (rl-locked? r6-lk)))
(ok "9h. no lock left counted on this thread" (= 0 (jolt-locks-held)))

(jolt-fiber-pool-reset!)

;; --- 10. the rule the four sections above are instances of (jolt-h9nq) ---------
;; Everything above fixes one lock at a time. This section is the rule itself:
;;
;;   a fiber never leaves the CPU while its carrier holds a counted lock.
;;
;; It used to have an exception for a voluntary park, licensed by a precondition
;; about every OTHER user of the mutex on the same carrier — which no site can check
;; and no reviewer can see. Three bugs arrived through it (jolt-3a87 the monitor,
;; jolt-dfuo again, jolt-04ee the loader). What the exception was for is waiting on
;; state a lock guards, and that never needed one: commit under the lock, switch
;; outside it, retake the decision. jolt-lock-wait is that, and 10d/10e are it
;; working for both kinds of contender.
;;
;; WHY THIS IS ASSERTED AS A RAISE. The failure it replaces is a process that stops
;; dead with no error and no output on some fraction of runs. So the check is not
;; "the wedge no longer happens" (unobservable, and how jolt-04ee sat unreproduced);
;; it is that the violation is REPORTED, at the park, naming the rule.
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! #f)

;; 10a. the check itself, on this thread, where the count is readable.
(define (string-contains? s sub)
  (let ((ls (string-length s)) (lb (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i lb) ls) #f)
            ((string=? (substring s i (+ i lb)) sub) #t)
            (else (loop (+ i 1)))))))
(define (raises-lock-rule? thunk)
  (guard (e (#t (let ((m (guard (_ (#t "")) (condition-message e))))
                  (and (string? m) (string-contains? m "cannot leave the CPU")))))
    (thunk)
    #f))
(define l10-mu (make-mutex))
(ok "10a. the assertion passes when no lock is held"
    (not (raises-lock-rule? (lambda () (jolt-locks-assert-none! 'test)))))
(ok "10a. and raises while one is"
    (jolt-with-mutex l10-mu
      (raises-lock-rule? (lambda () (jolt-locks-assert-none! 'test)))))
(ok "10a. the region still released it" (mutex-acquire l10-mu #f))
(mutex-release l10-mu)
(ok "10a. and left nothing counted on this thread" (= 0 (jolt-locks-held)))

;; 10b. the shape itself: a fiber parking inside a counted region. This is the
;; pre-fix loader and the pre-fix monitor, reduced to the two lines they had in
;; common. It must raise rather than wedge, and the carrier must be usable
;; afterwards — the raise unwinds through the region, so the mutex is released and
;; the count given back, and 10c is that stated as behaviour rather than inferred.
(define l10-mu2 (make-mutex))
(define l10-ch (ac-make 1 'fixed #f))
(define l10-bad
  (sa-fiber-spawn
   (lambda ()
     (jolt-with-mutex l10-mu2
       (jolt-fiber-<! l10-ch)))))
(jolt-fiber-ensure-carrier!)
(define l10-died
  (wait-until (lambda () (memq (jolt-fiber-state l10-bad) '(done dead))) 5.0
              "10b. a fiber parking inside a counted lock ended"))
(ok "10b. a fiber parking inside a counted lock does not wedge" l10-died)
(ok "10b. it died rather than completing" (eq? 'dead (jolt-fiber-state l10-bad)))
(ok "10b. and the error names the rule"
    (and (condition? (jolt-fiber-error l10-bad))
         (raises-lock-rule? (lambda () (raise (jolt-fiber-error l10-bad))))))
(ok "10b. the mutex it parked inside is free" (mutex-acquire l10-mu2 #f))
(mutex-release l10-mu2)

;; 10c. the carrier recovered. A stale lock count would leave EVERY later fiber on
;; this carrier unable to park (and unpreemptible), so a fiber that parks and is
;; resumed normally afterwards is the check that the failure was contained.
(define l10-ch2 (ac-make 1 'fixed #f))
(define l10-after
  (sa-fiber-spawn (lambda () (jolt-fiber-<! l10-ch2))))
(sleep (make-time 'time-duration 50000000 0))
(jolt-async-give l10-ch2 'ok)
(define l10-recovered
  (wait-until (lambda () (eq? 'done (jolt-fiber-state l10-after))) 5.0
              "10c. the carrier still runs fibers afterwards"))
(ok "10c. the carrier still parks and resumes fibers afterwards" l10-recovered)

;; 10d. jolt-lock-wait from a FIBER: the sanctioned way to do what 10b cannot.
;; Asserted on the three properties the hand-rolled copies each got wrong somewhere:
;; the decision is retaken (decide runs more than once), the lock is FREE while the
;; fiber is parked (this is the property whose absence deadlocks the carrier), and
;; the answer comes back from the retake.
(define l10-wmu (make-mutex))
(define l10-ready (box #f))
(define l10-runs (box 0))
(define l10-waiter (box #f))
(define l10-free-while-parked (box 'unset))
(define l10-lw
  (sa-fiber-spawn
   (lambda ()
     (jolt-lock-wait l10-wmu
       (lambda ()
         (set-box! l10-runs (+ 1 (unbox l10-runs)))
         (cond
           ((unbox l10-ready) 'got-it)
           (else (set-box! l10-waiter (jolt-current-fiber))
                 (jolt-fiber-state-set! (jolt-current-fiber) 'parked)
                 jolt-lock-parked)))))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (eq? 'parked (jolt-fiber-state l10-lw))) 5.0
            "10d. the fiber committed to its park")
;; the whole point: another context can take the mutex while the waiter is parked
(set-box! l10-free-while-parked (and (mutex-acquire l10-wmu #f) #t))
(when (eq? #t (unbox l10-free-while-parked))
  (set-box! l10-ready #t)
  (mutex-release l10-wmu))
(sa-fiber-resume (unbox l10-waiter))
(define l10-lw-done
  (wait-until (lambda () (eq? 'done (jolt-fiber-state l10-lw))) 5.0
              "10d. the resumed fiber retook the decision"))
(ok "10d. jolt-lock-wait releases the lock while the fiber is parked"
    (eq? #t (unbox l10-free-while-parked)))
(ok "10d. the resumed fiber came back and finished" l10-lw-done)
(ok "10d. it retook the whole decision rather than resuming into it"
    (>= (unbox l10-runs) 2))
(ok "10d. and returned the decision" (eq? 'got-it (jolt-fiber-result l10-lw)))

;; 10e. jolt-lock-wait from a THREAD, which must never reach the sentinel: it waits
;; on a condition variable inside decide, loops there, and answers a real value. One
;; function, both contenders, and the difference is one branch.
(define l10-tmu (make-mutex))
(define l10-tcv (make-condition))
(define l10-tstate (box #f))
(define l10-tanswer (box 'unset))
(define l10-tparked (box #f))
(define l10-t
  (fork-thread
   (lambda ()
     (set-box! l10-tanswer
       (jolt-lock-wait l10-tmu
         (lambda ()
           (let loop ()
             (cond
               ((unbox l10-tstate) 'thread-got-it)
               (else (set-box! l10-tparked #t)
                     (condition-wait l10-tcv l10-tmu)
                     (loop))))))))))
(wait-until (lambda () (unbox l10-tparked)) 5.0 "10e. the thread reached its wait")
(jolt-with-mutex l10-tmu
  (set-box! l10-tstate #t)
  (condition-broadcast l10-tcv))
(thread-join l10-t)
(ok "10e. a thread through the same primitive answers without parking"
    (eq? 'thread-got-it (unbox l10-tanswer)))
(ok "10e. and left nothing counted on this thread" (= 0 (jolt-locks-held)))

;; 10f. a refused switch commits NOTHING. A fiber that catches the lock rule's
;; raise and carries on must not be left queued (yield), marked parked (park), or
;; registered as a channel waiter (take): each of those used to happen BEFORE the
;; check, so the fiber ran on while queued or registered, a waker or the scheduler
;; dispatched it a second time after it finished ("fiber in unexpected state"),
;; and a value given to the channel went to the dead registration. The yield also
;; left interrupts disabled one level deeper than it found them.
(define l10f-mu (make-mutex))
(define l10f-ch (ac-make 1 'fixed #f))
(define l10f-yield (box 'unset))
(define l10f-take (box 'unset))
(define l10f-depth (box 'unset))
(define l10f
  (sa-fiber-spawn
   (lambda ()
     (let ((d0 (jolt-current-disable-count)))
       (jolt-with-mutex l10f-mu
         (set-box! l10f-yield (guard (e (#t 'refused)) (sa-fiber-yield) 'switched))
         (set-box! l10f-depth (- (jolt-current-disable-count) d0))
         (set-box! l10f-take (guard (e (#t 'refused)) (jolt-fiber-<! l10f-ch) 'took))))
     'finished)))
(jolt-fiber-ensure-carrier!)
(ok "10f. the fiber finished after catching both refusals"
    (wait-until (lambda () (memq (jolt-fiber-state l10f) '(done dead))) 5.0
                "10f. the fiber ended"))
(ok "10f. it finished rather than died" (eq? 'done (jolt-fiber-state l10f)))
(ok "10f. the yield was refused" (eq? 'refused (unbox l10f-yield)))
(ok "10f. and left the interrupt depth where it was" (eqv? 0 (unbox l10f-depth)))
(ok "10f. the take was refused" (eq? 'refused (unbox l10f-take)))
(jolt-async-give l10f-ch 'v)
(ok "10f. the refused take left no taker behind: a later give stays buffered"
    (eq? 'v (ac-poll! l10f-ch)))
(define l10f-ch2 (ac-make 1 'fixed #f))
(define l10f-next (sa-fiber-spawn (lambda () (jolt-fiber-<! l10f-ch2))))
(sleep (make-time 'time-duration 50000000 0))
(jolt-async-give l10f-ch2 'ok)
(ok "10f. the carrier still parks and resumes fibers afterwards"
    (wait-until (lambda () (eq? 'done (jolt-fiber-state l10f-next))) 5.0
                "10f. a fiber after the refusals ran"))

(jolt-fiber-pool-reset!)

(printf "\nfibers-lock-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "fibers-lock-test: PASS — monitors across a fiber switch\n") (exit 0))
    (exit 1))
