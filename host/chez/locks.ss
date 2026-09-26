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
(define (jolt-locks-held) (virtual-register jolt-vreg-locks))
;; Always ERR TOWARDS HELD. enter! runs BEFORE the acquire and exit! runs AFTER
;; the release, so the window on each side reads "a lock is held" when one is not
;; quite held yet or no longer is. That costs at most a deferred preemption; the
;; other order would leave a real window where the lock is held and the count
;; says otherwise, which is the bug this file exists to prevent.
(define (jolt-locks-enter!)
  (set-virtual-register! jolt-vreg-locks (fx+ 1 (virtual-register jolt-vreg-locks))))
(define (jolt-locks-exit!)
  (set-virtual-register! jolt-vreg-locks (fx- (virtual-register jolt-vreg-locks) 1)))

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
         (lambda () (jolt-locks-enter!) (mutex-acquire jwm-mu))
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
    ((mu) (jolt-locks-enter!) (mutex-acquire mu))
    ((mu block?)
     (jolt-locks-enter!)
     (let ((got (mutex-acquire mu block?)))
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

(define (jolt-lock-wait mu decide)
  ;; before decide, which registers the waiter under mu
  (jolt-fiber-may-park! 'jolt-lock-wait)
  (let retake ()
    (let ((r (jolt-with-mutex mu (decide))))
      (if (eq? r jolt-lock-parked)
          ;; mu is released here. jolt-current-fiber still answers this fiber —
          ;; the switch is what clears that register — and the state it needs is
          ;; already 'parked, set by decide under mu.
          (begin (jolt-fiber-to-scheduler! (jolt-current-fiber))
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
(define jolt-condition-wait
  (case-lambda
    ((cv mu)
     (when (jolt-current-fiber) (jolt-blocking-refuse 'jolt-condition-wait))
     (condition-wait cv mu))
    ((cv mu abs-time)
     (when (jolt-current-fiber) (jolt-blocking-refuse 'jolt-condition-wait))
     (condition-wait cv mu abs-time))))

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
;; finish. deadline is epoch milliseconds, or #f for an unbounded wait.
;;
;; This is jolt-lock-wait with the two waiter kinds filled in, so it inherits the
;; properties that primitive exists for: the switch happens with mu released, the
;; registration and the 'parked commit happen under the mu the waker must take so
;; no wakeup is lost, and the decision is RETAKEN rather than resumed into — which
;; is what makes a spurious wake harmless and is why decide must be re-runnable.
;;
;; WHERE THE PARKED FIBERS LIVE. In a table keyed by the condition variable, not in
;; a field on each waitable, because the waitables are a future record, a promise
;; record, an agent record, four different jhost vectors, a pipe and a process
;; latch. Keying on the condition they already have means none of those shapes
;; change. The table's own mutex is a leaf — taken around one hashtable operation
;; with nothing inside it — so it cannot be part of a cycle, the same argument
;; object-monitor's table lock rests on.
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

(define jolt-cv-waiters (make-weak-eq-hashtable))   ; condition -> parked fibers
(define jolt-cv-waiters-mu (make-mutex))

(define (jolt-cv-register! cv f)
  (jolt-with-mutex jolt-cv-waiters-mu
    (hashtable-set! jolt-cv-waiters cv
                    (cons f (hashtable-ref jolt-cv-waiters cv '())))))

;; Drained as it is read, so a fiber that goes on to wait again registers itself
;; afresh and no resume is ever delivered twice.
(define (jolt-cv-take-waiters! cv)
  (jolt-with-mutex jolt-cv-waiters-mu
    (let ((fs (hashtable-ref jolt-cv-waiters cv '())))
      (unless (null? fs) (hashtable-delete! jolt-cv-waiters cv))
      fs)))

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
(define (jolt-cv-wake! cv)
  (condition-broadcast cv)
  (let ((fs (jolt-cv-take-waiters! cv)))
    (unless (null? fs) (for-each sa-fiber-resume fs))))

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
;;   NO FIBER MAY WAIT ON cv. There is no fiber half here — a parked fiber lives in
;;   a table this does not read, so signalling would leave it parked. The executor's
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
(define (jolt-cv-signal-one! cv) (condition-signal cv))

;; Absolute epoch millis -> the absolute time object Chez's condition-wait wants.
;; Floored to an exact integer first: make-time will not take a flonum field, and a
;; deadline arriving as one is a caller's arithmetic, not something to trust.
(define (jolt-millis->time ms)
  (let ((ms (exact (floor ms))))
    (make-time 'time-utc (* 1000000 (mod ms 1000)) (div ms 1000))))

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
      (let ((mu (make-mutex)) (cv (make-condition)))
        (jolt-cv-wait mu cv (+ (now-millis) ms)
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
  ;; before the deadline timer below registers a wake for this wait
  (jolt-fiber-may-park! who)
  ;; Registered OUTSIDE mu, and only for a fiber. Outside because the timer's
  ;; thunks run with timeout-mu released but registering takes it, so doing this
  ;; under mu would order mu above timeout-mu here and below it there.
  (when (and deadline (jolt-current-fiber))
    (jolt-timer-at! deadline (lambda () (jolt-with-mutex mu (jolt-cv-wake! cv)))))
  ;; The (mu . cv) this wait is findable by while it is willing to be interrupted,
  ;; or #f. It lives OUT here and not inside the thunk below because jolt-lock-wait
  ;; RETAKES that thunk when a parked fiber resumes: a loop-local would forget a
  ;; registration the park had already made, and the entry would outlive the wait.
  (let ((entry #f))
    (jolt-lock-wait mu
      (lambda ()
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
                         (lambda () (decide (and deadline (>= (now-millis) deadline)))))
                       ;; NOT REGISTERED — nothing to clean up, so decide runs with
                       ;; nothing wrapped around it. This is the arm every
                       ;; non-interruptible wait takes, and the one a wait that
                       ;; answers on its first run takes.
                       (decide (and deadline (>= (now-millis) deadline))))))
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
                       (jolt-fiber-state-set! f 'parked)
                       jolt-lock-parked))
                 (else
                  (if deadline
                      (jolt-condition-wait cv mu (jolt-millis->time deadline))
                      (jolt-condition-wait cv mu))
                  (loop)))))))))))

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
;; THE IDENTITY IS THE INTERRUPT BOX, not the fiber, and that is a decision rather
;; than an oversight. The flag lives in the box, .interrupt is handed a box and
;; nothing else, and jolt has no per-fiber interrupt flag to key on — so a fiber
;; waiting here registers under the box of the carrier it is running on (a fiber
;; cannot migrate, so that box is stable for its lifetime) and is woken when that
;; thread is interrupted. Every waiter woken re-checks, and the check CLEARS, so
;; exactly one of them consumes the interrupt and throws while the rest go back to
;; waiting — one interrupt, one InterruptedException, as on the JVM. The reachable
;; shape is a go block that interrupts (Thread/currentThread), which is its carrier;
;; test/chez/unit.edn pins it and known-divergences.edn records it.
(define jolt-interrupt-waits (make-weak-eq-hashtable))   ; interrupt box -> (mu . cv) list
(define jolt-interrupt-waits-mu (make-mutex))

;; Both called with the waiter's mu held. The table's own mutex is a leaf — taken
;; around one hashtable operation with nothing inside it — so it cannot be part of
;; a cycle, the same argument jolt-cv-waiters-mu rests on.
(define (jolt-interrupt-wait-add! b entry)
  (jolt-with-mutex jolt-interrupt-waits-mu
    (hashtable-set! jolt-interrupt-waits b
                    (cons entry (hashtable-ref jolt-interrupt-waits b '())))))

(define (jolt-interrupt-wait-remove! b entry)
  (jolt-with-mutex jolt-interrupt-waits-mu
    (let ((es (remq entry (hashtable-ref jolt-interrupt-waits b '()))))
      (if (null? es)
          (hashtable-delete! jolt-interrupt-waits b)
          (hashtable-set! jolt-interrupt-waits b es)))))

;; (jolt-interrupt-wake-waits! b) — poke every condition the thread owning b is
;; willing to be interrupted out of. Call AFTER setting the flag: the flag is what
;; a woken waiter reads, and a wake that arrives before it is set says nothing.
;;
;; READ AND NOT DRAINED, unlike jolt-cv-take-waiters!. An entry is owned by the wait
;; that made it and is removed by that wait when it stops waiting, so deleting it
;; here would unregister a waiter that is still waiting — the one that did not win
;; the flag, when several share a carrier — and the next interrupt would not reach
;; it. The entries are woken OUTSIDE the table's mutex, so this path holds one lock
;; at a time.
(define (jolt-interrupt-wake-waits! b)
  (let ((es (jolt-with-mutex jolt-interrupt-waits-mu
              (hashtable-ref jolt-interrupt-waits b '()))))
    (for-each (lambda (e) (jolt-with-mutex (car e) (jolt-cv-wake! (cdr e)))) es)))

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
