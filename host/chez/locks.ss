;; host/chez/locks.ss — every lock in the runtime, and the count the scheduler
;; reads to decide whether a fiber may be preempted right now.
;;
;; WHY THIS FILE EXISTS
;;
;; An OS mutex has THREAD granularity. Fibers multiplex one OS thread. So an OS
;; mutex held across a fiber switch is broken either way, and there is no way to
;; fix the switch:
;;
;;   - if the switch UNWINDS, the lock is released mid-section. Chez's with-mutex
;;     is a dynamic-wind and a park is a nonlocal exit, so this is what happens
;;     by default. Measured: (swap! a inc) through a with-mutex'd compare-and-set
;;     loses an update, 11999 of 12000, on every run.
;;   - if the switch does NOT unwind, the fiber keeps the mutex while another
;;     fiber on the SAME carrier runs. Chez mutexes are recursive per thread, so
;;     that fiber's acquire succeeds and walks straight into the section.
;;
;; Both lose exclusion. The only sound answer is not to switch there at all,
;; which is what every runtime that has faced this does: Go's canPreemptM is
;; `mp.locks == 0 && ...`, a counter bumped by acquirem/releasem; Caladan wraps
;; non-reentrant regions in preempt_disable/preempt_enable; Linux PREEMPT_RT
;; keeps raw_spinlock for the sections that must not be preempted. All of them
;; on the same precondition, that such regions are SHORT — jolt's are, measured
;; at ~55ns mean against a 0.45ms quantum.
;;
;; The alternative architecture is to make the locks themselves fiber-aware so a
;; fiber CAN hold one across a switch. That is what Java did in JEP 491, and it
;; took a reimplementation of HotSpot's object monitors to get there. Not this.
;;
;; WHAT MUST NOT BE COPIED FROM THE INTERRUPT DEPTH
;;
;; Two per-carrier quantities behave OPPOSITELY across a park, and conflating
;; them is what broke the previous attempt:
;;
;;   interrupt depth   must NOT rewind. It is carried across the switch in
;;                     jolt-fiber-sic and restored at dispatch. Putting a
;;                     disable in a dynamic-wind before-thunk double-counts it.
;;   lock depth        MUST rewind, because it is a fact ABOUT THE LOCK: the
;;                     after-thunk really did release it and the before-thunk
;;                     really does re-acquire it.
;;
;; So the count below is bumped INSIDE the dynamic-wind, next to the acquire. It
;; has to agree with reality DURING a park, not merely at the ends, because the
;; carrier is handed to another fiber that reads the same register while this one
;; is parked.

;; Slot 7: how many locks this carrier currently holds. Per thread, like the
;; other virtual registers, and a fresh thread starts it at fixnum 0 — which is
;; the correct answer for a thread that never runs fibers.

(define jolt-vreg-locks 7)
;; Syntax, like the pair below: the masked winds read it at their edges, where a
;; call would itself be a place Chez checks for an interrupt.
(define-syntax jolt-locks-held (syntax-rules () ((_) (virtual-register 7))))
;; Always ERR TOWARDS HELD. enter! runs BEFORE the acquire and exit! runs AFTER
;; the release, so the window on each side reads "a lock is held" when one is not
;; quite held yet or no longer is. That costs at most a deferred preemption; the
;; other order would leave a real window where the lock is held and the count
;; says otherwise, which is the bug this file exists to prevent.
;; Syntax and not procedures: the object monitor's masked enter and exit
;; (java/concurrency.ss) take the count on every `locking`, where a call each way
;; was a measurable share of the section.
(define-syntax jolt-locks-enter!
  (syntax-rules ()
    ((_) (set-virtual-register! 7 (fx+ 1 (virtual-register 7))))))
(define-syntax jolt-locks-exit!
  (syntax-rules ()
    ((_) (set-virtual-register! 7 (fx- (virtual-register 7) 1)))))

;; --- winds an interrupt cannot split -------------------------------------------
;; run-interruptible leaves its body by jumping out of a timer interrupt, and a
;; fiber interrupt is raised where a preemption resumed, so any event check can be
;; an escape point. Chez has them at the edges of a dynamic-wind: one between the
;; body's return and the after-thunk (the winder already popped), and one between
;; state set up OUTSIDE the wind and the winder's push. An escape at either
;; skipped the cleanup: a lazy seq's claim stayed held and every other forcer
;; waited on it, a namespace load's claim did the same to every require, and a
;; binding frame, *txn* or *ns* stayed set for whatever ran next on the thread.
;;
;; Both escapes hold off while this thread's counted-lock depth is above zero, so
;; the depth is the mask: raised before the setup and dropped as the body starts,
;; raised again as the body returns and dropped once the after-thunk is done. The
;; after-thunk tells a normal exit from a raise, an escape or a park by the depth,
;; since only the normal exit left it above the level at entry, so no flag is
;; allocated. State set INSIDE the before-thunk needs no mask: Chez pushes the
;; winder as the before-thunk returns, with no check in between (swept at both
;; optimize levels), so a resume's rewind of it is safe too. Only state set up
;; before the wind is called, and the far edge, need the mask.
;;
;; A cleanup runs masked on a normal exit and drops the mask last, so a raise out
;; of it would leave the depth up for good: no interrupt would land on the thread
;; again, and a carrier would never preempt. jolt-masked-after is for cleanups
;; that cannot raise (a store, a CAS); jolt-masked-cleanup, which jolt-wind* uses,
;; drops the mask on the way out of a raise too.
;;
;; The depth is a counted lock and a park under one is refused, so these are for
;; the runtime's own winds, whose setup and after-thunk are short and never park.
;; A user `finally` is masked the same way but drops the depth before its body
;; runs (the back end's emit-try). Every piece is inline register reads and
;; writes: a call is itself a place Chez checks for the interrupt.
;; The body, run unmasked, masked again as it returns (single value).
(define-syntax jolt-masked-body
  (syntax-rules ()
    ((_ thunk) (lambda () (jolt-locks-exit!) (let ((r (thunk))) (jolt-locks-enter!) r)))))
;; The after-thunk: E ..., then the mask dropped if a normal exit raised it above
;; LV, the depth before the wind. E must not raise.
(define-syntax jolt-masked-after
  (syntax-rules ()
    ((_ lv e ...) (lambda () e ... (when (fx>? (jolt-locks-held) lv) (jolt-locks-exit!))))))
;; The same for a cleanup that might raise: on a normal exit E runs in a wind
;; whose after-thunk drops the mask, so a raise out of E drops it as it unwinds.
;; That wind's own far edge is still masked, since the drop is its after-thunk.
(define-syntax jolt-masked-cleanup
  (syntax-rules ()
    ((_ lv e ...)
     (lambda ()
       (if (fx>? (jolt-locks-held) lv)
           (dynamic-wind void (lambda () e ...) (lambda () (jolt-locks-exit!)))
           (begin e ...))))))
;; (jolt-wind* before setup body after-expr): SETUP and the push masked, BODY
;; unmasked, AFTER-EXPR masked on a normal exit (and allowed to raise). BEFORE is the before-thunk:
;; jolt-finally-in for a cleanup a park must not run (fibers.ss drops those
;; winders before a park escapes), (lambda () #f) for one it must.
(define-syntax jolt-wind*
  (syntax-rules ()
    ((_ before setup body after)
     (let ((lv (jolt-locks-held)))
       (jolt-locks-enter!)
       setup
       (dynamic-wind before (jolt-masked-body body) (jolt-masked-cleanup lv after))))))

;; NOTE on how a refused preemption is remembered. It is NOT remembered here.
;; The obvious design — a pending flag, honoured when the outermost region exits
;; — would have to park from inside a dynamic-wind's after-thunk, since that is
;; where the release happens, and escaping from an after-thunk is its own hazard.
;; The timer is a better memory than a flag: the scheduler's handler simply
;; re-arms on a short retry when it finds a lock held, so the preemption lands
;; just after the region ends without anything here knowing about fibers. That is
;; also what Go does — a goroutine interrupted at an unsafe point is resumed and
;; retried later.

;; (jolt-with-mutex m body ...) — the replacement for Chez's with-mutex.
;; Deliberately a DIFFERENT NAME rather than a shadow: a shadow would make every
;; site silently safe, which is pleasant right up until the gate cannot tell
;; whether a site was considered or merely inherited the shadow. host/chez/
;; lock-check.sh requires the explicit name, so the migration is visible and the
;; count can only go down.
;;
;; A FIBER MAY NOT PARK INSIDE THE BODY, and that is a rule rather than a
;; guideline: the count above is what the preempt handler reads to refuse an
;; INVOLUNTARY switch, and a voluntary one is not different in kind. So there is
;; one rule, and it covers both:
;;
;;   a fiber never leaves the CPU while its carrier holds a counted lock.
;;
;; jolt-locks-assert-none! below is that rule, and every switch point calls it —
;; jolt-fiber-to-scheduler! (which is yield, park, and the preemption) and
;; jolt-sm-park! (the cheap park). So a violation raises at the park instead of
;; wedging a process with no error and no output.
;;
;; IT USED TO BE AN EXCEPTION, which is how the same bug arrived three times
;; (jolt-3a87, jolt-dfuo, jolt-04ee). Parking inside the body was legal,
;; licensed by dynamic-wind: the after-thunk releases on the way out and the
;; before-thunk re-acquires when the continuation is resumed, so the lock is not
;; held ACROSS the park. What that reading leaves out is WHERE the re-acquire
;; runs. It runs from Chez's rewind, on the carrier thread, at the interrupt
;; depth the fiber parked at, where the preemption timer is not polled, the
;; carrier can do nothing else until it succeeds, and nothing can make it give
;; up. So a park does not merely release a lock and retake it. It attaches a
;; blocking acquire to a point in the SCHEDULER, and the wait edge that creates
;; belongs to every fiber on that carrier, not to the one that parked.
;;
;; The precondition that makes that safe is non-local to match: no fiber on the
;; carrier may be holding m while this one is off the CPU. A parking site cannot
;; check that, and a reviewer cannot see it, because it is a statement about
;; every OTHER user of m that shares the carrier. For the object monitor it was
;; read as the weaker "no fiber is parked while HOLDING m", which is true and
;; insufficient; one run in twelve of a contended monitor wedged the whole
;; process, and one of those was a ninety-minute `make test` (jolt-8tma). The
;; rule above is the same guarantee with nothing left to read wrong, and unlike
;; the precondition it replaces it is checkable — at the switch, and statically
;; over the whole runtime (host/chez/park-lock-check.ss).
;;
;; ACQUIRING WITH INTERRUPTS DISABLED. A park commits inside a region with
;; interrupts disabled (fibers.ss jolt-fiber-park!, fiber-park-commit!, the
;; channel waiter), and takes the carrier's and the waiter's mutexes there. Chez
;; gives up a blocked thread's place in a collect rendezvous only when interrupts
;; are ENABLED (prim5.c s_mutex_acquire, like condition-wait and sleep:
;; DISABLECOUNT(tc) == 0). So a commit that blocked on a mutex whose holder had
;; reached a safe point and joined a collection stayed counted as active: the
;; collection waited for it, it waited for the holder, and the process stopped
;; with every other thread in the rendezvous (the fibers-io gate, about one run
;; in a hundred). jolt-mutex-acquire! tries first, which is the whole cost when
;; the lock is free; contended with interrupts off, it waits in a collect-safe
;; foreign call between tries, which deactivates the thread whatever its
;; disable count, so the collection finishes and the holder can release.
;;
;; Both come from the adapter (sa-disable-count, sa-collect-safe-pause). The
;; fibers gates load this file without one, and get the derived count and a
;; plain spin, as fibers.ss's jolt-current-disable-count does.
(define jolt-locks-disable-count
  (guard (e (#t (lambda () (let ((n (disable-interrupts))) (enable-interrupts) (fx- n 1)))))
    sa-disable-count))
(define jolt-collect-safe-pause
  (guard (e (#t (lambda () 0))) sa-collect-safe-pause))
(define-syntax jolt-mutex-acquire!
  (syntax-rules ()
    ((_ m) (let ((jma-mu m))
             (unless (mutex-acquire jma-mu #f) (jolt-mutex-acquire-contended! jma-mu))))))
(define (jolt-mutex-acquire-contended! mu)
  (if (fx=? 0 (jolt-locks-disable-count))
      (mutex-acquire mu)
      (let retry ()
        (jolt-collect-safe-pause)
        (unless (mutex-acquire mu #f) (retry)))))

;; WAITING FOR STATE THIS LOCK GUARDS is what the exception existed for, and it
;; never needed one: commit under the lock, switch outside it. That is
;; jolt-lock-wait, below.
;;
;; A CHEAP park (java/sm.ss) was never allowed here even under the old reading —
;; it does not rewind, so the lock would be released and never retaken. The CPS
;; pass keeps them apart by treating every form that takes a thunk as opaque;
;; see jolt-sm-park!.
(define-syntax jolt-with-mutex
  (syntax-rules ()
    ((_ m e1 e2 ...)
     (let ((jwm-mu m))
       (dynamic-wind
         (lambda () (jolt-locks-enter!) (jolt-mutex-acquire! jwm-mu))
         (lambda () e1 e2 ...)
         (lambda () (mutex-release jwm-mu) (jolt-locks-exit!)))))))

;; The same pair for the paths that CANNOT use the macro because they must
;; release before parking and re-acquire after — the fiber channel ops, which
;; hold the channel mutex by hand for exactly that reason.
;; The optional second argument is Chez's: (mutex-acquire mu #f) TRIES and
;; answers #f rather than blocking. The count is claimed before the attempt and
;; given back if it fails, so a failed try never leaves the carrier looking like
;; it holds something, and a successful one is never briefly uncounted.
(define jolt-lock!
  (case-lambda
    ((mu) (jolt-locks-enter!) (jolt-mutex-acquire! mu))
    ((mu block?)
     (jolt-locks-enter!)
     (let ((got (if block? (begin (jolt-mutex-acquire! mu) #t) (mutex-acquire mu #f))))
       (unless got (jolt-locks-exit!))
       got))))
(define (jolt-unlock! mu) (mutex-release mu) (jolt-locks-exit!))

;; --- the invariant, checked where it can be broken ---------------------------
;; (jolt-locks-assert-none! who) — raise unless this carrier holds no counted
;; lock. Called by every switch point rather than by the sites that park, and
;; that placement is the point: there are two switch points and a growing number
;; of parking sites, and the ones that go wrong are the ones nobody thought of as
;; parking sites at all (a `locking` body, a validator, a load that waits).
;;
;; Complete for parks that HAPPEN, in a way the static check cannot be: it does
;; not care whether the park is lexically inside the region, one call away
;; (jolt-04ee), or inside user code the lock never wrote (jolt-3a87). If a fiber
;; is about to leave the CPU with a lock held, this is on the path.
;;
;; It RAISES, and the alternative is worth naming: the failure it replaces is a
;; process that stops dead with no error and no output, on some fraction of runs,
;; needing a sampling profiler to diagnose. Raising costs the fiber (its guard
;; marks it dead) and reports the invariant, the count, and a stack. That trade
;; is not close. The check runs before the caller's first mutation at both sites,
;; so the raise leaves the switch untaken rather than half-taken.
;;
;; Always on, never behind a flag: a check that is off in the build people ship is
;; not a check, and the cost is one virtual-register read and a fixnum compare
;; against a switch that costs ~136 ns. Measured rather than asserted, on
;; bench/fibers: the scheduler yield+slice figure spans 135.8-137.7 ns over three
;; runs with the check and reads 136.6 ns without it, so it is inside the
;; run-to-run spread; channel ping-pong and fan-in move the same way.
(define (jolt-locks-assert-none! who)
  (let ((n (jolt-locks-held)))
    (when (fx>? n 0)
      (error who
        (string-append
         "a fiber cannot leave the CPU while its carrier holds a counted lock ("
         (number->string n)
         " held). Commit under the lock and switch outside it — jolt-lock-wait,"
         " host/chez/locks.ss.")))))

;; The same check, made where a wait BEGINS rather than where it switches. A park
;; is a sequence: commit (mark the fiber parked or ready, enqueue it, register it
;; as a channel or condition waiter), then switch. The assertion above sits at the
;; switch, so by the time it raises the commit has happened, and the fiber runs on
;; while marked parked, queued, or registered with a waker that will dispatch it a
;; second time ("fiber in unexpected state"). Every sequence calls this first; the
;; park/lock gate checks that it comes before the commit. On a plain thread it is
;; nothing: a thread may wait while holding a lock.
(define (jolt-fiber-may-park! who)
  (when (jolt-current-fiber) (jolt-locks-assert-none! who)))

;; (jolt-fiber-wait-turn! n) -> #t when this fiber gave its carrier away for a
;; while, #f when it could not (not a fiber, or a counted lock is held) and the
;; caller must wait its own way. For a waiter that POLLS -- a lazy seq's forcer
;; waiting on the cell's runner (seq.ss force-claimed!) -- where the thing it
;; waits for may be a fiber on this same carrier that runs only once this one
;; leaves. The first rounds yield, which is enough when the runner is merely
;; queued; later ones pause with a growing deadline (1ms to 8ms), for a runner
;; that is itself parked on something slow.
;;
;; The park is guarded rather than unconditional, and that is what the park/lock
;; gate is told (park-lock-check.ss guarded-parks): with a lock held it returns #f
;; instead of parking, so calling it from inside a region cannot leave the CPU.
(define (jolt-fiber-wait-turn! n)
  (if (and (jolt-current-fiber) (fx=? (jolt-locks-held) 0))
      (begin
        (if (fx<? n 16)
            (sa-fiber-yield)
            (jolt-pause-ms (fxsll 1 (fxmin 3 (fxsrl (fx- n 16) 2)))))
        #t)
      #f))

;; --- waiting for state a lock guards ----------------------------------------
;; (jolt-lock-wait mu decide) -> whatever decide returns
;;
;; The one sanctioned way for a fiber to wait on state that a mutex guards, and
;; the protocol five sites had hand-rolled between them: the channel waiters
;; (java/fibers-async.ss, java/sm.ss), the object monitor and ReentrantLock
;; (java/concurrency.ss), jolt.io-poller/wait-fiber, and the load claims in
;; loader.ss — which is the one that got it wrong (jolt-04ee). Four correct
;; copies of an unnamed protocol are four chances to write a fifth.
;;
;; decide runs with mu HELD and answers either
;;
;;   jolt-lock-parked   "I have registered myself where my waker will look and
;;                       set my own state to 'parked. Switch me out, and call me
;;                       again when something resumes me."
;;   anything else       the decision, returned to the caller as-is.
;;
;; Three properties, and each one is where a hand-rolled copy can go wrong:
;;
;;   THE SWITCH IS OUTSIDE mu, so no resume carries a mutex re-acquire and the
;;   invariant above holds by construction. It is also lexically outside the
;;   jolt-with-mutex below, which is what the static check reads, so every caller
;;   inherits a shape that check can see through.
;;
;;   NO WAKEUP CAN BE LOST, because registering and committing to 'parked both
;;   happen under the same mu the waker must take. A resume landing in the window
;;   between the release and the switch finds the fiber 'parked, moves it to
;;   'ready and enqueues it; the switch then stores its continuation and the
;;   carrier dispatches it. A preemption in that window is refused, because
;;   jolt-fiber-preempt-handler refuses to preempt a fiber that is not 'running —
;;   which is why this needs no interrupt disable of its own.
;;
;;   THE DECISION IS RETAKEN, not resumed into. A wakeup means something changed,
;;   never "it is yours", so decide runs again from the top with mu held. That is
;;   also why decide must be safe to run more than once.
;;
;; A THREAD needs none of this and is not sent a different way: inside decide it
;; waits on a condition variable, which releases mu atomically with blocking and
;; holds it again on return, loops there, and answers a real value — so it never
;; reaches the branch below. One function serves both contenders and the entire
;; difference between them is that one branch.
(define jolt-lock-parked (list 'jolt-lock-parked))   ; unique; never a decision

(define (jolt-lock-wait mu decide) (jolt-lock-wait/abandon mu decide #f))

;; An interrupt (fibers.ss) raised out of a park is the one exit decide never
;; sees, so a registration decide removes on its own exits would outlive the
;; wait. With ABANDON? true, decide also takes one argument, and is called with
;; it (outside mu) just before that raise, to drop such a registration. It is
;; decide itself rather than a separate thunk because the caller that needs it
;; (jolt-cv-wait/ibox) sits on the settled-deref path, where one more closure
;; per call is measurable.
(define (jolt-lock-wait/abandon mu decide abandon?)
  ;; before decide, which registers the waiter under mu
  (jolt-fiber-may-park! 'jolt-lock-wait)
  (let retake ()
    (let ((r (jolt-with-mutex mu (decide))))
      (if (eq? r jolt-lock-parked)
          ;; mu is released here. jolt-current-fiber still answers this fiber —
          ;; the switch is what clears that register — and the state it needs is
          ;; already 'parked, set by decide under mu.
          (let ((f (jolt-current-fiber)))
            ;; An interrupt (fibers.ss) ends the wait instead of letting the
            ;; decision be retaken: its wake is not "something changed".
            (when (jolt-fiber-switch-for-park? f)
              (jolt-fiber-to-scheduler! f))
            (jolt-fiber-wstate-set! f #f)
            (when (and abandon? (jolt-fiber-interrupt f) (fx=? 0 (jolt-fiber-mask f)))
              (decide 'abandon))
            (jolt-fiber-check-interrupt! f)
            (retake))
          r))))

;; --- blocking, and who is allowed to do it ----------------------------------
;; The rule above is about a fiber that leaves the CPU while holding a lock. This
;; is its mirror image, and it was a bug of its own (jolt-x1no):
;;
;;   a fiber must never block its carrier.
;;
;; condition-wait blocks the THREAD. On a carrier that is every fiber placed on it,
;; and a fiber cannot migrate away, so the wait also blocks whatever the carrier
;; would have run — including, often enough, the very thing that would have ended
;; the wait. Measured on one carrier: fiber A does (deref a-promise), fiber B
;; delivers it, and neither ever finishes. With more carriers the same code is a
;; stall rather than a deadlock, which is worse to diagnose, not better.
;;
;; So the runtime has two waits, and they are not interchangeable:
;;
;;   jolt-condition-wait   for a THREAD. Refuses on a fiber, by name, instead of
;;                         quietly taking its carrier away.
;;   jolt-cv-wait          for either. A thread blocks; a fiber parks and is
;;                         resumed by the wake.
;;
;; The loader and the object monitor already chose per waiter by hand, which is why
;; those two were not affected. Everything else in the runtime blocked
;; unconditionally: promise and future deref, agent await, Thread.join,
;; CountDownLatch.await, a task Future's get, a piped stream read, and waiting on a
;; subprocess. All of those are reachable from a go block on the fiber backend.
(define (jolt-blocking-refuse who)
  (error who
    (string-append
     "a fiber cannot block its carrier on a condition variable: it would stop every"
     " other fiber placed on that carrier, including whatever would have ended this"
     " wait. Use jolt-cv-wait (host/chez/locks.ss), which parks a fiber and blocks"
     " a thread.")))

;; Chez's condition-wait for the paths that are only ever reached by a real thread —
;; a carrier's own idle wait, the timer thread, an executor worker, the thread arm
;; inside a jolt-cv-wait or jolt-lock-wait decision. The check is what makes "only
;; ever reached by a thread" a fact rather than an intention: every one of these was
;; documented as thread-only, and the four that were wrong about it were found by
;; asking rather than by reading.
;; --- what a THREAD is doing, for Thread.getState ------------------------------
;; Every thread that waits records it in a box of its own: WAITING or
;; TIMED_WAITING while it is in a condition wait (below), BLOCKED while it waits to
;; enter a monitor (java/concurrency.ss), nothing — RUNNABLE — otherwise. Another
;; thread reads it by the thread's id. The box is made on a thread's first wait and
;; found again through a thread parameter; the parameter carries the owning id
;; because a forked thread inherits it. Writing it costs a box store on a wait that
;; is about to block anyway. A fiber never waits here; its state is the fiber
;; layer's to report (jolt-thread-state-hook, java/concurrency.ss).
(define thread-state-boxes (make-eqv-hashtable))          ; thread id -> box
(define thread-state-mu (make-mutex))
(define thread-state-cell (make-thread-parameter #f))     ; (thread id . box)
(define (thread-state-box)
  (let ((c (thread-state-cell)) (id (get-thread-id)))
    (if (and (pair? c) (eqv? (car c) id))
        (cdr c)
        (let ((b (box #f)))
          (jolt-with-mutex thread-state-mu (hashtable-set! thread-state-boxes id b))
          (thread-state-cell (cons id b))
          b))))
;; The recorded state of thread ID — WAITING, TIMED_WAITING or BLOCKED — or #f.
(define (thread-wait-state id)
  (let ((b (jolt-with-mutex thread-state-mu (hashtable-ref thread-state-boxes id #f))))
    (and b (unbox b))))
(define (thread-state-forget! id)
  (jolt-with-mutex thread-state-mu (hashtable-delete! thread-state-boxes id)))
;; Run THUNK recorded as STATE, for a wait that is not a condition wait (a polled
;; lock acquire) or that is one of a more specific kind (entering a monitor). An
;; outer state wins over the inner condition wait's.
(define (call-with-thread-state state thunk)
  (let* ((b (thread-state-box)) (old (unbox b)))
    (set-box! b (or old state))
    (dynamic-wind void thunk (lambda () (set-box! b old)))))
;;
;; cv is a jolt-cv or a raw Chez condition. A jolt-cv counts the thread as its
;; waiter for the length of the wait, so a wake with nobody waiting can skip the
;; broadcast (jolt-cv-wake!); mu is held at both ends, which is what the count needs.
;; A raw condition is for the waits no fiber can reach and no value carries: the
;; carriers' idle wait, the timer, the loader, the main queue.
(define jolt-condition-wait
  (case-lambda
    ((cv mu)
     (when (jolt-current-fiber) (jolt-blocking-refuse 'jolt-condition-wait))
     ;; dynamic-wind: an interrupt handler that escapes the wait must not leave
     ;; the thread reading WAITING
     (if (jolt-cv? cv)
         (begin
           (jolt-cv-threads-set! cv (fx+ (jolt-cv-threads cv) 1))
           (call-with-thread-state 'WAITING (lambda () (condition-wait (jolt-cv-cond cv) mu)))
           (jolt-cv-threads-set! cv (fx- (jolt-cv-threads cv) 1)))
         (call-with-thread-state 'WAITING (lambda () (condition-wait cv mu)))))
    ((cv mu abs-time)
     (when (jolt-current-fiber) (jolt-blocking-refuse 'jolt-condition-wait))
     (if (jolt-cv? cv)
         (begin
           (jolt-cv-threads-set! cv (fx+ (jolt-cv-threads cv) 1))
           (let ((r (call-with-thread-state 'TIMED_WAITING
                      (lambda () (condition-wait (jolt-cv-cond cv) mu abs-time)))))
             (jolt-cv-threads-set! cv (fx- (jolt-cv-threads cv) 1))
             r))
         (call-with-thread-state 'TIMED_WAITING (lambda () (condition-wait cv mu abs-time)))))))

;; --- the wait beneath the fiber layer ---------------------------------------
;; (jolt-stop-the-world-wait cv mu timeout) -> #t signalled | #f timed out
;;
;; Chez's condition-wait, for ONE caller: the collection rendezvous the adapter
;; installs (scheme-adapter-runtime.ss sa-gc-install-stall-watch!), waiting on
;; the collector's own conditions under the tc mutex. A collect request stops
;; the whole OS thread — every fiber on the carrier with it — until the
;; collection has run: parking here would run other Scheme code on a carrier
;; the collector is waiting to stop, so this wait blocks the carrier whatever
;; is on it, and the fiber refusal in jolt-condition-wait does not apply. It is
;; reached with interrupts disabled (the rendezvous mirrors with-tc-mutex), so
;; no preemption can land inside it either way. Named here rather than
;; allowlisted so the gate keeps reading "every other condition-wait chose".
(define (jolt-stop-the-world-wait cv mu timeout)
  (condition-wait cv mu timeout))

;; --- waiting for a condition, from a thread OR a fiber ----------------------
;; (jolt-cv-wait mu cv deadline decide) -> whatever decide returns
;;
;; decide runs with mu HELD and is passed one argument, whether the deadline has
;; passed. It answers jolt-cv-again to wait for a change, or any other value to
;; finish. deadline is in mono-millis, or a timeout not yet started (jolt-timeout
;; below), or #f for an unbounded wait.
;;
;; This is jolt-lock-wait with the two waiter kinds filled in, so it inherits the
;; properties that primitive exists for: the switch happens with mu released, the
;; registration and the 'parked commit happen under the mu the waker must take so
;; no wakeup is lost, and the decision is RETAKEN rather than resumed into — which
;; is what makes a spurious wake harmless and is why decide must be re-runnable.
;;
;; WHERE THE WAITERS LIVE. On the condition itself: a jolt-cv (below) is a Chez
;; condition with the fibers parked on it and a count of the threads blocked on it,
;; both read and written only under the mu its waiters and wakers already hold. So a
;; wake takes no lock beyond that mu, finds its fibers without a lookup, and skips
;; the broadcast when no thread waits — what the JDK's queues do by signalling only
;; when a waiter exists. They used to live in one table keyed by the condition,
;; under one process-wide mutex, which every wake of every condition took whether
;; or not anything waited (#1311).
;;
;; THE DEADLINE IS THE CLOCK, NOT A FLAG, and that is what closes the race a timed
;; park would otherwise have. A fiber has nothing to wake it at the deadline, so one
;; is registered with the shared timer (java/async.ss) to poke this condition. If
;; that poke lands before the fiber parks, there is nothing to resume — which would
;; be a fiber parked forever if "timed out" were a flag the timer set. It is not:
;; decide reads the clock, and the timer fires at the deadline, so any decide that
;; runs after the timer has fired already sees the deadline as passed and does not
;; park at all. The poke is only ever a wakeup, never information.
;;
;; A thread needs no such registration — its condition-wait takes the deadline
;; directly — so it does not pay for one.
(define jolt-cv-again (list 'jolt-cv-again))

;; (jolt-make-cv) -> a condition a thread or a fiber can wait on.
;;
;; Every condition passed to jolt-cv-wait, jolt-cv-wake! or jolt-cv-signal-one! is
;; one of these, and so is any condition the runtime keeps inside a value: the image
;; writer stands one in for every 'condition it substitutes (state-image.ss). The
;; raw Chez condition is reachable only here — host/chez/lock-check.sh refuses
;; jolt-cv-cond anywhere else — because the thread count is only right if every
;; thread wait goes through jolt-condition-wait, which keeps it.
;;
;; fibers: parked fibers, newest first. threads: threads blocked in condition-wait.
;; Both change only with the waiters' mu held. A thread that leaves its wait by a
;; non-local exit leaves its count behind: it cannot take it back without mu, and a
;; count that is too high only costs a broadcast nobody hears, where one that is too
;; low would lose a wake.
(define-record-type (jolt-cv make-jolt-cv jolt-cv?)
  (fields (immutable cond jolt-cv-cond)
          (mutable fibers jolt-cv-fibers jolt-cv-fibers-set!)
          (mutable threads jolt-cv-threads jolt-cv-threads-set!))
  (nongenerative jolt-cv-v1)
  (sealed #t))
(define (jolt-make-cv) (make-jolt-cv (make-condition) '() 0))

;; Called by the fiber about to park, inside decide, with mu held.
(define (jolt-cv-register! cv f)
  (jolt-cv-fibers-set! cv (cons f (jolt-cv-fibers cv))))

;; (jolt-cv-wake! cv) — call with mu HELD, after changing the state decide reads.
;; The replacement for a bare condition-broadcast on any condition a fiber can wait
;; on: it wakes both kinds of waiter, and a waker that forgets the fiber half would
;; leave them parked with nothing to resume them.
;;
;; Broadcast and not signal, for the reason the loader gives: waiters re-check a
;; condition that may be true for only one of them, so exactly one of them getting
;; the wake is not something the waker can decide.
;;
;; Resuming while mu is held is deliberate and safe. sa-fiber-resume only enqueues,
;; on the resumed fiber's own carrier, taking that carrier's run-queue mutex, which
;; is last in the order. The resumed fiber's retake will block on mu for as long as
;; this critical section lasts — and no longer, because a lock in this runtime can
;; no longer be held across a park at all, so every holder of mu is running and
;; releases in bounded time.
;;
;; The fiber list is drained as it is read, so a fiber that goes on to wait again
;; registers itself afresh and no resume is ever delivered twice.
(define (jolt-cv-wake! cv)
  (unless (fx=? 0 (jolt-cv-threads cv)) (condition-broadcast (jolt-cv-cond cv)))
  (let ((fs (jolt-cv-fibers cv)))
    (unless (null? fs)
      (jolt-cv-fibers-set! cv '())
      (for-each sa-fiber-resume fs))))

;; (jolt-cv-signal-one! cv) — wake exactly ONE thread waiting on cv, with mu held.
;;
;; The narrow counterpart of jolt-cv-wake!, for a condition that carries a QUEUE of
;; interchangeable items to a crowd of interchangeable waiters: one item arrives,
;; one waiter can take it, and any of them will do. The executor's task condition is
;; that and is the only one in the runtime (java/concurrency.ss).
;;
;; The two preconditions, both of which that condition meets and neither of which
;; the caller can be trusted to remember, so they are stated where the call is:
;;
;;   NO FIBER MAY WAIT ON cv. There is no fiber half here — a parked fiber is on
;;   the fiber list, which this does not read, so signalling would leave it parked. The executor's
;;   workers are threads, and jolt-condition-wait REFUSES on a fiber, so the pool
;;   cannot acquire a fiber waiter by accident; the pool's other waiters (its
;;   awaitTermination, which a fiber can reach) wait on a second condition of their
;;   own and are woken with jolt-cv-wake!.
;;
;;   EVERY WAITER MUST WANT THE SAME THING. Broadcast is the default for the reason
;;   given above — when the state is true for only one waiter, the waker cannot pick
;;   which — and a signal here is right only because they are all waiting for "a
;;   task, any task", so the one that wakes can always use what arrived.
;;
;; What it buys, measured on the pool it was written for: an enqueue used to
;; broadcast, so a pool with 130 idle workers woke all 130 for one task, 129 of
;; which took the queue mutex, found the task gone and parked again. Dispatching a
;; no-op task cost 152us of that; signalling one took it to 8.9us, and the pool it
;; needs for the same work from 134 threads to 7. Reference JVM Clojure dispatches
;; the same task in 5.2us.
(define (jolt-cv-signal-one! cv)
  (unless (fx=? 0 (jolt-cv-threads cv)) (condition-signal (jolt-cv-cond cv))))

;; A mono-millis deadline -> the time left until it, as the time-duration Chez's
;; condition-wait takes for a relative timeout. Relative and not an absolute
;; time-utc: Chez's absolute form is the wall clock, and the deadline is not.
;; Measured from now at full precision (mono-duration-until), not from the
;; truncated millisecond, or every timed wait runs past its deadline.
(define (jolt-deadline->timeout deadline)
  (mono-duration-until deadline))

;; (jolt-timeout ms) — a timeout of ms (exact, maybe fractional) that starts when
;; the wait first has to wait: the deadline argument of a one-shot timed wait. A
;; wait that answers on its first decide -- a settled deref, a non-empty poll --
;; never reads the clock: its timeout cannot have run out before it started, so
;; that decide is told timed-out? exactly when ms is zero or less. An exact
;; deadline costs a rational division to build and another to test, which the
;; non-blocking path would otherwise pay on every call. A wait that keeps one
;; deadline over several waits (await-for over many agents, invokeAll) builds it
;; up front with mono-deadline instead.
(define (jolt-timeout ms) (box ms))
(define (jolt-deadline-start deadline)
  (if (box? deadline) (mono-deadline (unbox deadline)) deadline))
(define (jolt-deadline-due? deadline)
  (if (box? deadline) (<= (unbox deadline) 0) (mono-due? deadline)))

;; (jolt-pause-ms ms) — pause THIS execution context for ms, whatever it is: a
;; thread sleeps, and a fiber parks until the deadline and gives its carrier up in
;; the meantime.
;;
;; For the runtime's own POLL LOOPS, which wait on something no condition variable
;; can be attached to — a waitpid, a char-ready? on stdin. A poll that sleeps stops
;; every fiber on the carrier for each nap, and since these loops run until an
;; external event happens, that is unbounded: a (.waitFor p) from a go block
;; occupied its carrier for the whole life of the subprocess. ReentrantLock's
;; bounded tryLock had the same shape and was already fixed to yield instead, for
;; the same reason; this is that fix with the carrier actually released rather than
;; merely handed on.
;;
;; NOT for Thread/sleep, which is a user-facing request to sleep a thread, matches
;; a JVM core.async go block as it stands, and has a gate asserting it.
;;
;; The mutex and condition are private and fresh, so nothing else can signal them:
;; the timer's wake is the only one, and the retake's own clock read is what decides
;; the pause is over. That makes this a park with a deadline expressed in the one
;; wait protocol the runtime has, rather than a second one written by hand.
(define (jolt-pause-ms ms)
  (if (jolt-current-fiber)
      (let ((mu (make-mutex)) (cv (jolt-make-cv)))
        (jolt-cv-wait mu cv (mono-deadline ms)
          (lambda (timed-out?) (if timed-out? #t jolt-cv-again)))
        (void))
      (sleep (ms->duration ms))))

(define (jolt-cv-wait mu cv deadline decide)
  (jolt-cv-wait/ibox mu cv deadline decide #f #f))

;; The wait itself. ibox is the caller's interrupt box, or #f for a wait that is
;; NOT interruptible — which is the default and what jolt-cv-wait above passes,
;; because the runtime's own plumbing waits here too (the carrier idle wait, the
;; load barrier, the main-queue pump, the tap queue, the channel internals) and
;; interrupting any of those breaks the runtime rather than the caller's code.
;; jolt-cv-wait-interruptibly at the bottom of this file is the opt-in door.
;;
;; The interrupt arm reads as a parameter rather than a wrapper around `decide`
;; for a measured reason: a wrapper allocated a closure and a box on EVERY call,
;; and most calls through here never wait at all — a deref of an already-delivered
;; promise or a settled future runs decide once and returns. That cost a settled
;; deref 1.15x against a 1.1x ceiling (release binary, A/B/A). Threaded through,
;; the fast path is an `(if ibox ...)` that falls through (jolt-a0f1).
(define (jolt-cv-wait/ibox mu cv deadline decide ibox who)
  ;; The deadline wake is registered only for a fiber; a thread waits on cv with
  ;; the deadline itself. It is cancelled as the wait ends, however it ends, so a
  ;; timed deref answered in a millisecond does not leave its wake — and, through
  ;; mu and cv, whatever those keep reachable — pending on the timer until the
  ;; deadline. Arming and cancelling only take timeout-mu, a leaf (async.ss: no
  ;; thunk runs under it), so the handler below may do either at the raise point,
  ;; under mu or not.
  ;;
  ;; A raise passing through is not necessarily the end of the wait: a handler
  ;; further out may resume it. So the handler cancels, and if the raise comes
  ;; back, re-arms — otherwise the resumed wait parks with nothing to wake it at
  ;; its deadline. A wake that had already been collected to run is not re-armed;
  ;; it is on its way. The park's own may-park! assertion (the park/lock gate
  ;; wants it in jolt-cv-wait/park, next to the commit) runs after arming; when it
  ;; refuses, its raise passes through the handler and the wake is cancelled.
  (if (and deadline (jolt-current-fiber))
      (let* ((deadline (jolt-deadline-start deadline))
             (arm (lambda ()
                    (jolt-timer-at! deadline (lambda () (jolt-with-mutex mu (jolt-cv-wake! cv))))))
             (timer (arm))
             (r (with-exception-handler
                  (lambda (e)
                    (let* ((pending? (jolt-timer-cancel! timer))
                           (v (raise-continuable e)))
                      (when pending? (set! timer (arm)))
                      v))
                  (lambda () (jolt-cv-wait/park mu cv deadline decide ibox who)))))
        (jolt-timer-cancel! timer)
        r)
      (jolt-cv-wait/park mu cv deadline decide ibox who)))

(define (jolt-cv-wait/park mu cv deadline decide ibox who)
  (jolt-fiber-may-park! who)
  ;; The (mu . cv) this wait is findable by while it is willing to be interrupted,
  ;; or #f. It lives OUT here and not inside the thunk below because jolt-lock-wait
  ;; RETAKES that thunk when a parked fiber resumes: a loop-local would forget a
  ;; registration the park had already made, and the entry would outlive the wait.
  (let ((entry #f))
    (jolt-lock-wait/abandon mu
      (case-lambda
       ((_)
        ;; abandoned by a fiber interrupt: the wait is over, deregister. The
        ;; interrupt resumed the fiber directly, so no wake drained it from cv's
        ;; list; left there, cv would keep this dead wait reachable until its
        ;; next wake.
        (let ((f (jolt-current-fiber)))
          (jolt-with-mutex mu
            (jolt-cv-fibers-set! cv (remq f (jolt-cv-fibers cv)))
            (when entry (jolt-interrupt-wait-remove! ibox entry))))
        (set! entry #f))
       (()
        (let loop ()
          ;; The flag FIRST, on every round. That ordering is what gives the
          ;; already-set case for free — a thread whose flag is set when it calls a
          ;; blocking op throws immediately and never waits, as on the JVM — and
          ;; because the decision here is retaken rather than resumed into, the
          ;; same check covers every wake for nothing extra. The read CLEARS, which
          ;; is java.lang.Thread's own rule: "the interrupted status is cleared and
          ;; an InterruptedException is thrown."
          (when ibox
            (when (unbox ibox)
              (set-box! ibox #f)
              (when entry (jolt-interrupt-wait-remove! ibox entry) (set! entry #f))
              (jolt-interrupted-throw! who)))
          (let ((r (if entry
                       ;; REGISTERED. decide throws on its own account (a failed
                       ;; agent is the live case), so that exit has to deregister
                       ;; too, or the entry outlives the wait and a later interrupt
                       ;; pokes a condition nobody is on.
                       ;;
                       ;; with-exception-handler and not guard, for java/sm.ss's
                       ;; reason: the handler runs AT THE RAISE POINT, so
                       ;; jolt-throw's captured continuation and site still describe
                       ;; where the throw came from. A guard would unwind first and
                       ;; the report would name this wait instead.
                       (with-exception-handler
                         (lambda (e) (jolt-interrupt-wait-remove! ibox entry) (raise-continuable e))
                         (lambda () (decide (and deadline (jolt-deadline-due? deadline)))))
                       ;; NOT REGISTERED — nothing to clean up, so decide runs with
                       ;; nothing wrapped around it. This is the arm every
                       ;; non-interruptible wait takes, and the one a wait that
                       ;; answers on its first run takes.
                       (decide (and deadline (jolt-deadline-due? deadline))))))
            (cond
              ((not (eq? r jolt-cv-again))
               (when entry (jolt-interrupt-wait-remove! ibox entry) (set! entry #f))
               r)
              (else
               ;; About to wait for the first time: become findable, then RE-READ
               ;; the flag. The window between the read at the top and this
               ;; registration is exactly what the second read closes — the
               ;; interrupter sets the flag BEFORE it reads the registry, so either
               ;; it found us, and its wake is already on its way to a mutex we
               ;; still hold, or it did not and its flag is set for this read.
               (when (and ibox (not entry))
                 (let ((e (cons mu cv)))
                   (set! entry e)
                   (jolt-interrupt-wait-add! ibox e)
                   (when (unbox ibox)
                     (set-box! ibox #f)
                     (jolt-interrupt-wait-remove! ibox e)
                     (set! entry #f)
                     (jolt-interrupted-throw! who))))
               (cond
                 ((jolt-current-fiber)
                  => (lambda (f)
                       (jolt-cv-register! cv f)
                       (when deadline (jolt-fiber-wstate-set! f 'TIMED_WAITING))
                       (jolt-fiber-state-set! f 'parked)
                       jolt-lock-parked))
                 (else
                  (if deadline
                      (begin
                        ;; the timeout starts here, at the first wait
                        (set! deadline (jolt-deadline-start deadline))
                        (jolt-condition-wait cv mu (jolt-deadline->timeout deadline)))
                      (jolt-condition-wait cv mu))
                  (loop)))))))))
      #t)))

;; --- interruptible waiting --------------------------------------------------
;; (jolt-cv-wait-interruptibly who mu cv deadline decide)
;;
;; jolt-cv-wait with one thing added: a thread parked in it is thrown out by
;; .interrupt, the way every interruptible wait on the JVM is. It is the OPT-IN
;; door onto jolt-cv-wait/ibox above; jolt-cv-wait passes #f and stays exactly as
;; uninterruptible as it was, because the runtime's own plumbing waits through the
;; same seam — the carrier idle wait, the load barrier, the main-queue pump, the
;; tap queue, the channel internals — and interrupting any of those breaks the
;; runtime rather than the caller's code. The interruptible sites are the ones a
;; JVM caller can already name: promise and future deref, agent await,
;; Thread.join, Thread.sleep, CountDownLatch.await, a task Future's get, a
;; blocking queue take/put, awaitTermination, waitFor.
;;
;; TWO PIECES, and only the second one is new work. The first is that the wait
;; RETAKES its decision on every wake rather than resuming into it, so one check
;; at the top of the loop covers every wake and needs no second wait protocol.
;;
;; The second is that the interrupter has to WAKE a waiter blocked on a condition
;; variable it has never heard of. It cannot know which future or latch its target
;; happens to be sitting on, so the waiter says: it registers its (mu . cv) against
;; its own interrupt identity for as long as it is willing to wait, and .interrupt
;; sets the flag and then pokes every condition registered against that identity.
;;
;; WHY THE RACE CLOSES, and it closes for a reason worth not breaking. The whole
;; loop above runs with mu HELD — decide is called under it — and the interrupter takes
;; mu to wake (jolt-cv-wake! is documented "call with mu HELD"). So an interrupt
;; cannot land in the gap between deciding to wait and actually parking. The one
;; remaining window is between the check and the registration, and the re-check
;; after registering is what closes it: the interrupter sets the flag BEFORE it
;; reads the registry, so either it read us — and its wake is already on its way to
;; a mutex we still hold — or it did not, and its flag is already set for the
;; re-read. Both orders end in the same throw.
;;
;; The registry is touched ONLY by a wait that actually waits — a deref of an
;; already-delivered promise takes no lock this file did not already take, and
;; allocates nothing this file did not already allocate.
;;
;; THE IDENTITY IS THE INTERRUPT BOX. A thread's box is its own; a FIBER's is the
;; fiber's own too (current-interrupt-box answers it while the fiber runs), since a
;; fiber is a virtual thread with its own Thread object and flag. So a fiber
;; waiting here registers under its own box and is woken when IT is interrupted,
;; and no other fiber on its carrier is disturbed. Every waiter woken re-checks,
;; and the check CLEARS — one interrupt, one InterruptedException, as on the JVM.
(define jolt-interrupt-waits (make-weak-eq-hashtable))   ; interrupt box -> entry set
(define jolt-interrupt-waits-mu (make-mutex))

;; An ENTRY is what the interrupter wakes: a (mu . cv) pair, poked as
;; (jolt-with-mutex mu (jolt-cv-wake! cv)), or a procedure of no arguments that
;; does its own waking — the cheap park's, which resumes one fiber directly rather
;; than listing it on a condition (java/sm.ss jolt-sm-commit!/intr).
;;
;; Per box the entries are a SET (an eq table), not a list, so a removal does not
;; scan whatever else a box has listed. The set is kept when it empties: it is
;; weak-keyed with its box, so it goes when the thread or fiber does, and one that
;; waits again reuses it.
;;
;; Both called with the waiter's mu held. The table's own mutex is a leaf — taken
;; around one hashtable operation with nothing inside it — so it cannot be part of
;; a cycle, the same argument object-monitor's table lock rests on.
;;
;; jolt-lock! / jolt-unlock! by hand rather than jolt-with-mutex: nothing between
;; them can raise, and a go body parked in <!! pays for both on every park, where
;; the dynamic-wind was a measurable share.
(define (jolt-interrupt-wait-add! b entry)
  (jolt-lock! jolt-interrupt-waits-mu)
  (let ((es (or (hashtable-ref jolt-interrupt-waits b #f)
                (let ((t (make-eq-hashtable)))
                  (hashtable-set! jolt-interrupt-waits b t)
                  t))))
    (hashtable-set! es entry #t))
  (jolt-unlock! jolt-interrupt-waits-mu))

(define (jolt-interrupt-wait-remove! b entry)
  (jolt-lock! jolt-interrupt-waits-mu)
  (let ((es (hashtable-ref jolt-interrupt-waits b #f)))
    (when es (hashtable-delete! es entry)))
  (jolt-unlock! jolt-interrupt-waits-mu))

;; (jolt-interrupt-wake-waits! b) — poke every condition the thread owning b is
;; willing to be interrupted out of. Call AFTER setting the flag: the flag is what
;; a woken waiter reads, and a wake that arrives before it is set says nothing.
;;
;; READ AND NOT DRAINED, unlike a jolt-cv's fiber list. An entry is owned by the wait
;; that made it and is removed by that wait when it stops waiting, so deleting it
;; here would unregister a waiter that is still waiting, and the next interrupt
;; would not reach it. The entries are woken OUTSIDE the table's mutex, so this path holds one lock
;; at a time.
;;
;; A box can also have an OWNER, registered once for its life rather than per
;; wait: a fiber owns its box (fibers.ss jolt-fiber-ibox!), and its channel waits
;; register nowhere and are resumed through the fiber itself. So whoever holds a
;; fiber's box — a FutureTask's cancel(true), a pool's shutdownNow, Thread.interrupt
;; — reaches those waits by this same call, with nothing to know about fibers:
;; jolt-interrupt-owner-wake, set by fibers.ss, is handed the owner. Ephemeron-
;; keyed, so an owner that holds its own box (a fiber does) is not kept by it.
(define jolt-interrupt-owners (make-ephemeron-eq-hashtable))  ; interrupt box -> owner
(define jolt-interrupt-owner-wake #f)
(define (jolt-interrupt-owner-set! b owner)
  (jolt-lock! jolt-interrupt-waits-mu)
  (hashtable-set! jolt-interrupt-owners b owner)
  (jolt-unlock! jolt-interrupt-waits-mu))
(define (jolt-interrupt-wake-waits! b)
  (let-values (((es owner)
                (jolt-with-mutex jolt-interrupt-waits-mu
                  (let ((t (hashtable-ref jolt-interrupt-waits b #f)))
                    (values (if t (hashtable-keys t) '#())
                            (hashtable-ref jolt-interrupt-owners b #f))))))
    (vector-for-each
      (lambda (e)
        (if (procedure? e)
            (e)
            (jolt-with-mutex (car e) (jolt-cv-wake! (cdr e)))))
      es)
    (when (and owner jolt-interrupt-owner-wake) (jolt-interrupt-owner-wake owner))))

;; The flag, read-and-cleared — java.lang.Thread's own rule for a wait that throws:
;; "the interrupted status is cleared and an InterruptedException is thrown."
(define (jolt-interrupt-take! b) (and (unbox b) (begin (set-box! b #f) #t)))

(define (jolt-interrupted-throw! who)
  (jolt-throw (jolt-host-throwable "java.lang.InterruptedException" who)))

;; For a POLLED wait that has no condition variable to register (the subprocess
;; reap loop): same flag, same clear, same throw, checked once per round.
(define (jolt-interrupt-poll-check! who)
  (let ((b (current-interrupt-box)))
    (when (jolt-interrupt-take! b) (jolt-interrupted-throw! who))))

;; `who` is the InterruptedException's message. The JVM's is null for most of these
;; and "sleep interrupted" for Thread.sleep; jolt names the op instead, which is
;; strictly more information and is what its other host throwables do.
(define (jolt-cv-wait-interruptibly who mu cv deadline decide)
  (jolt-cv-wait/ibox mu cv deadline decide (current-interrupt-box) who))
