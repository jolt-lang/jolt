;; host/chez/java/fibers-async.ss — fiber-aware <! / >! over the ONE channel
;; waiter protocol (R3, epic jolt-nvpr.4). Loaded by rt.ss AFTER async.ss and
;; fibers.ss.
;;
;; The bet: fibers consume core.async's existing callback protocol; channels
;; are not rewritten. A fiber's <! is a take that registers an alt-taker
;; handler whose `wake` is the fiber (alt-handler-alloc f); a thread's pending
;; op keeps the condvar wake. The channel core commits via claim+mailbox
;; (async.ss) and alt-deliver! decides how to wake — it never learns what a
;; fiber is. The alt-takers/alt-putters lists are the shared waiter lists.
;;
;; The immediate-completion path captures NO continuation: if the non-blocking
;; poll (take) or give (put) succeeds — a buffered value, a waiting putter, a
;; waiting taker — the fiber returns without parking, and the park counter
;; below (what the R3 gate asserts on) does not move.
;;
;; The park path closes the deliver-vs-park race with the handler's wmu: the
;; commit ("set 'parked") happens under the SAME wmu that alt-deliver! writes
;; the mailbox and resumes under. A deliver that beat the commit is seen by the
;; commit's mailbox check (no park, no capture); one that follows it observes
;; 'parked and enqueues. The capture+switch happens after releasing the wmu and
;; never re-consults the fiber state — the resume's 'ready flip only means
;; "already on the queue", and the one-shot continuation is set before the
;; switch, so run-all picks it up correctly whether it was enqueued before or
;; after the capture.
;;
;; The channel mutex is released before the park, always — never yield while
;; holding it (the R3 invariant; a fiber that parks holding the channel mutex
;; deadlocks its carrier).
;;
;; alts! is unchanged this round; R4 replaces it with a wait set.

;; The capture counter the gate asserts on — how many continuation captures
;; happened in fiber channel ops — is (jolt-fiber-chan-parks), summed over the
;; carriers in fibers.ss. Bumped here through jolt-fiber-bump-chan-parks!, which
;; touches only the parking fiber's own carrier. The immediate path never
;; touches it.

;; A waiter handler whose wake is fiber f — the fiber-wake strategy.
(define (jolt-fiber-waiter f) (alt-handler-alloc f))

;; ac-try-give!/locked can THROW — a nil value, or a transducer step raising — and
;; this path holds the channel mutex BY HAND, because it has to be able to release
;; it before parking and with-mutex cannot do that. A throw would otherwise escape
;; with the mutex held and deadlock every later op on that channel.
;; jolt-async-give is unaffected: its with-mutex releases on the unwind.
;;
;; The guard is CONDITIONAL because guard costs a call/cc per entry and this sits
;; on every fiber put. Both callers run async-check-put! before taking the mutex
;; (that hoist is what makes this safe — do not remove it), so with the nil check
;; already done the only thrower left inside ac-try-give!/locked is the transducer
;; step; the rest is a buffer push and a notify. A channel with no xform cannot
;; raise here and does not pay for the frame. The redundant async-check-put! that
;; ac-try-give!/locked still does is left alone: it guards the OTHER callers.
;; --- holding a channel mutex against preemption -----------------------------
;; async.ss states the R3 invariant: never yield while holding a channel mutex,
;; because a fiber that suspends holding it strands every later op on that
;; channel. The fiber ops honour it by hand — they release before they park.
;; PREEMPTION BREAKS THAT, since the timer suspends a fiber wherever it happens
;; to be, including mid-section.
;;
;; And it does not fail as a clean deadlock, which is what makes it dangerous.
;; Fibers on one carrier share an OS thread and Chez mutexes are recursive per
;; thread, so the next fiber's acquire SUCCEEDS and walks straight into a section
;; another fiber is halfway through. Measured before this guard existed: a
;; 4-producer/1-consumer run on one carrier stalled at 802 of 1600 values with a
;; short quantum, and hung outright with a shorter one.
;;
;; So these ops disable interrupts for exactly as long as they hold the mutex.
;; Chez defers a timer raised in here and delivers it at the enable, so the
;; preemption is postponed rather than lost. Release BEFORE re-enabling: the
;; other order reopens the window.
;; No interrupt manipulation here any more. jolt-lock! counts the lock, and the
;; scheduler refuses to preempt a fiber while the count is non-zero, so the
;; region is protected by the same mechanism as every other lock in the runtime
;; rather than by a second one bolted onto this path.
(define (jolt-chan-lock! ch) (jolt-lock! (async-chan-mu ch)))
(define (jolt-chan-unlock! ch) (jolt-unlock! (async-chan-mu ch)))

(define (jolt-chan-locked-give! ch v)
  (if (async-chan-xrf ch)
      (guard (e (#t (jolt-chan-unlock! ch) (raise e)))
        (ac-try-give!/locked ch v))
      (ac-try-give!/locked ch v)))

;; (jolt-fiber-<! ch) -> value | nil (closed). Fiber-side take: a buffered
;; value, a waiting putter, or a closed channel complete immediately (no
;; capture); an empty open channel registers an alt-taker and parks.
(define (jolt-fiber-<! ch)
  (jolt-fiber-may-park! 'clojure.core.async/<!)   ; before registering as a taker
  (jolt-chan-lock! ch)
  (let ((r (ac-poll!/locked ch)))
    (if (eq? r ac-poll-empty)
        (let ((h (jolt-fiber-waiter (jolt-current-fiber))))
          (async-chan-alt-takers-set! ch (append (async-chan-alt-takers ch) (list h)))
          (ac-notify! ch)
          (if (vector-ref (alt-handler-mailbox h) 0)
              (let ((v (vector-ref (alt-handler-mailbox h) 1)))
                (jolt-chan-unlock! ch)
                v)
              (begin
                (jolt-chan-unlock! ch)
                (vector-ref (jolt-fiber-waiter-wait! h) 1))))
        (begin
          (jolt-chan-unlock! ch)
          r))))

;; (jolt-fiber->! ch v) -> #t | #f (closed). Fiber-side put: room or a waiting
;; taker completes immediately (no capture); a full channel registers an
;; alt-putter and parks.
(define (jolt-fiber->! ch v)
  (jolt-fiber-may-park! 'clojure.core.async/>!)   ; before registering as a putter
  (async-check-put! v)                   ; throws — keep it outside the mutex
  (jolt-chan-lock! ch)
  (let ((r (jolt-chan-locked-give! ch v)))
    (cond
      ((eq? r 'ok) (jolt-chan-unlock! ch) #t)
      ((eq? r 'closed) (jolt-chan-unlock! ch) #f)
      (else
       (let ((h (jolt-fiber-waiter (jolt-current-fiber))))
         (async-chan-alt-putters-set! ch
           (append (async-chan-alt-putters ch) (list (cons h v))))
         (ac-notify! ch)
           (if (vector-ref (alt-handler-mailbox h) 0)
               (let ((ok (vector-ref (alt-handler-mailbox h) 1)))
                 (jolt-chan-unlock! ch)
                 ok)
              (begin
                (jolt-chan-unlock! ch)
                (vector-ref (jolt-fiber-waiter-wait! h) 1))))))))

;; The fiber wakeup strategy — alt-deliver! dispatches through this hook so
;; async.ss (loaded before fibers.ss) never forward-references a fiber
;; primitive. Installed here, after both files are loaded; no channel op can
;; run before the boot finishes loading, so the hook is always live in use.
(set! jolt-fiber-wake-fn sa-fiber-resume)

;; --- R4: go on fibers, and alts! as a wait set (epic jolt-nvpr.5) -------------
;;
;; jolt-fiber-go-spawn is the :fiber backend of clojure.core.async/go-spawn
;; (the dispatcher lives in async.ss; thread stays the :thread backend), and
;; since jolt-579 it is also what clojure.core.async/fiber-spawn — io-thread's
;; carrier — reaches unconditionally, no dispatch involved. It
;; spawns the body as a fiber on the R5 carrier pool — N OS threads, each
;; looping drain-then-park (fibers.ss). Parking inside the body works ACROSS
;; function boundaries, which the JVM's state-machine go structurally cannot
;; do: any <! / >! the body (or a function it calls) hits dispatches through
;; the redefs below to jolt-fiber-<! / jolt-fiber->!, which park the fiber via
;; the R3 handler protocol.
;;
;; The pool's size is clojure.core.async/*fiber-carrier-count*, defined and
;; read in fibers.ss (host setter jolt-fiber-carrier-count-set! for tests);
;; jolt-fiber-ensure-carrier! starts it at the first :fiber go spawn.

;; (jolt-fiber-go-spawn thunk) -> buffered(1) channel. Conveys the parent's
;; dynamic slice (sa-fiber-spawn reads the spawner's bindings; *txn* is never
;; conveyed — a child spawned inside a dosync cannot join the parent's txn,
;; the same rule async-go-spawn-thread enforces for threads).
(define (jolt-fiber-go-spawn thunk)
  (let ((w (ac-make 1 'fixed #f)))
    (go-chan-register! w)                     ; before the spawn — see async.ss
    (sa-fiber-spawn
     (lambda ()
       (*txn* #f)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (if (car r)
             (when (not (jolt-nil? (cdr r))) (jolt-async-give w (cdr r)))
             (begin
               (async-report-uncaught! "go/fiber body (channel closed)" (cdr r))
               ;; Record it on the FIBER as well as on the channel. This guard is
               ;; what keeps a throwing body from killing the fiber, which is
               ;; right — the channel still has to be closed — but it also means
               ;; the fiber reaches jolt-fiber-done! looking like a success, and
               ;; a gate asking the fiber directly would read the failure as a
               ;; clean run. The sm backend (sm.ss jolt-sm-drive) reaches
               ;; jolt-fiber-dead! instead, which sets the same field.
               (jolt-fiber-error-set! (jolt-current-fiber) (cdr r))))
         ;; closes w, after publishing the outcome to the channel's monitors
         (go-chan-finish! w (and (not (car r)) (cdr r))))))
    (jolt-fiber-ensure-carrier!)
    w))

;; Monitoring a go block is backend-neutral and lives with the go surface in
;; async.ss (go-chan-register! / go-chan-finish! / the go-monitor var). It
;; used to be here, keyed channel -> FIBER, which is why it had nothing to say
;; about a thread-backed body. jolt-fiber-monitor! (fibers.ss) is still the
;; fiber-level primitive and is what the scheduler gates read.

;; The R3 park, generalized to return the handler's mailbox (value + port) —
;; the alts! fiber await needs the port; <! / >! need only the value.
;; Contract unchanged otherwise: call with the channel mutex RELEASED; the
;; commit-to-park decision is atomic with alt-deliver!'s mailbox write.
(define (jolt-fiber-waiter-wait! h)
  (let ((f (jolt-current-fiber)))
    (unless f
      (error 'jolt-fiber-waiter-wait! "channel wait outside a fiber"))
    ;; the take/put ops checked before registering; alts! registers its shared
    ;; handler in async.ss and arrives here first
    (jolt-fiber-may-park! 'jolt-fiber-waiter-wait!)
    ;; Commit and park are ONE region with interrupts disabled — see
    ;; jolt-sm-commit!. The park records the depth (swish's pcb-sic) and the
    ;; resume is restored to it, so the resumed path must NOT enable again; only
    ;; the no-park path does.
    (disable-interrupts)
    (let ((park?
           (jolt-with-mutex (alt-handler-wmu h)
             (if (vector-ref (alt-handler-mailbox h) 0)
                 #f
                 (begin (jolt-fiber-state-set! f 'parked) #t)))))
      (when park?
        (jolt-fiber-bump-chan-parks! f)
        (jolt-fiber-to-scheduler! f))
      ;; Balances the disable above on BOTH paths: the park returns here when the
      ;; fiber is resumed (restored to the depth it parked at), so it owes the
      ;; same enable the no-park path does.
      (enable-interrupts)
      (alt-handler-mailbox h))))

;; The fiber alts! await: park on the already-registered shared handler and
;; return [val port]. Registered by async.ss's __do-alts with wake = the
;; fiber, so alt-deliver! resumes the fiber; this is the mirror of the thread
;; waiter's condition-wait on the same mailbox.
(define (jolt-fiber-alt-await h)
  (let ((mb (jolt-fiber-waiter-wait! h)))
    (jolt-vector (vector-ref mb 1) (vector-ref mb 2))))

;; <! / >! / <!! / >!! dispatch on "am I on a fiber?" — the vreg read (R0's 2ns
;; dispatch). On a fiber they park (the R3 primitives, and — R5's decision —
;; <!! / >!! park exactly the same way: parking a blocking take preserves its
;; observable semantics without holding the OS thread, so on a fiber there is
;; no difference between <! and <!!, or between >! and >!!); on a plain thread
;; they are the blocking ops of today, so :thread-backend go bodies (real
;; threads), bare <!! on a thread, and the conformance gate's expectations are
;; byte-for-byte unchanged.
(cca-def! "<!" (lambda (ch) (if (jolt-current-fiber) (jolt-fiber-<! ch) (jolt-async-take ch))))
(cca-def! ">!" (lambda (ch v) (if (jolt-current-fiber) (jolt-fiber->! ch v) (jolt-async-give ch v))))
(cca-def! "<!!" (lambda (ch) (if (jolt-current-fiber) (jolt-fiber-<! ch) (jolt-async-take ch))))
(cca-def! ">!!" (lambda (ch v) (if (jolt-current-fiber) (jolt-fiber->! ch v) (jolt-async-give ch v))))

;; (fiber-execute runnable) -> nil. Run RUNNABLE on a fiber and forget it: no
;; result channel, no join. This is the spawn behind the :io Executor that
;; clojure.core.async.impl.dispatch hands to core.async.flow, and it is
;; deliberately leaner than fiber-spawn — a flow process never reads the channel
;; fiber-spawn would allocate, and the executor contract (java.util.concurrent
;; .Executor/execute returns void) has nothing to hand one to.
;;
;; Uncaught throws are REPORTED and swallowed, matching the executor-service
;; shim's `execute` (concurrency.ss) rather than fiber-spawn's convey-to-channel:
;; nobody is holding a channel to convey to. runnable->thunk accepts a FutureTask
;; (what futurize submits) as well as a plain thunk, so the two executors take the
;; same arguments.
(define (async-fiber-execute r)
  (let ((thunk (runnable->thunk r)))
    (sa-fiber-spawn
     (lambda ()
       (*txn* #f)
       (guard (e (#t (async-report-uncaught! "fiber-execute task" e)))
         (jolt-invoke thunk))))
    ;; a carrier may not exist yet on the very first spawn (fiber-spawn's own
    ;; postlude does this too) — without it the fiber sits ready and unrun.
    (jolt-fiber-ensure-carrier!)
    jolt-nil))
(cca-def! "fiber-execute" async-fiber-execute)

;; Install the alts! fiber-await hook (see async.ss).
(set! jolt-fiber-alt-await-fn jolt-fiber-alt-await)

;; --- R8: the IO-parking host seams (epic jolt-nvpr.8) -------------------------
;; jolt.socket (stdlib, Clojure over jolt.ffi) parks a fiber on an EAGAIN by
;; asking "am I on a fiber?", registering readiness with its poller (also
;; Clojure), committing to park under the poller's table lock, then switching.
;; These names are the entire fiber surface the Clojure layer needs, and the
;; discipline is exactly the channel waiters':
;;   - the 'parked commit (fiber-park-commit!) and the wake's state read inside
;;     sa-fiber-resume are serialized by the CALLER's lock — for the poller
;;     that is its table lock, a new leaf in the lock chain (nothing the
;;     fiber-park path does takes the run-queue mutex; the poller's wake runs
;;     sa-fiber-resume AFTER releasing the table lock, so pm -> carrier-mu and
;;     the channel chain wmu -> carrier-mu share no cycle).
;;   - fiber-to-scheduler! runs OUTSIDE that lock (a fiber that parks holding
;;     the table lock deadlocks its carrier), mirroring the R3 "release before
;;     park" invariant.
(def-var! "jolt.host" "fiber?" (lambda () (if (jolt-current-fiber) #t #f)))
(def-var! "jolt.host" "current-fiber" (lambda () (or (jolt-current-fiber) jolt-nil)))
;; The commit DISABLES INTERRUPTS before marking 'parked, and the switch seam
;; below re-enables after the fiber resumes — the exact shape jolt-fiber-park!
;; uses, for the exact reason its comment gives: a preemption timer landing
;; between the 'parked mark and the switch runs with the fiber already marked.
;; The bare mark lost ONE wakeup in a way that took a stress gate to see: a
;; poller event can fire the instant its EV_ADD applies, so the wake's
;; sa-fiber-resume raced into that window, read 'parked, and spent the resume
;; on a fiber still running toward its switch — which then parked for real with
;; nobody left to wake it. One fiber of eight lost, rarely, under load
;; (poller-registration's LOST 1 of 8). Chez defers a timer that fires in a
;; disabled region, and jolt-fiber-to-scheduler! saves/restores the disable
;; count across the switch (jolt-fiber-sic), so the pairing below is safe on
;; both sides of the park. These two seams are a COMMIT/SWITCH PAIR: a caller
;; that commits must switch (wait-fiber does; nothing else calls them).
(def-var! "jolt.host" "fiber-park-commit!"
  (lambda ()
    (jolt-fiber-may-park! 'jolt.host/fiber-park-commit!)
    (disable-interrupts)
    (jolt-fiber-state-set! (jolt-current-fiber) 'parked)))
;; jolt-fiber-to-scheduler! takes the fiber (it clears the current-fiber vreg
;; before capturing, so the record has to be passed in, not read afterwards).
(def-var! "jolt.host" "fiber-to-scheduler!"
  (lambda ()
    ;; the commit seam above checked already, and a caller takes no counted lock
    ;; between the two; this is the gate's rule for a switching definition
    (jolt-fiber-may-park! 'jolt.host/fiber-to-scheduler!)
    (jolt-fiber-to-scheduler! (jolt-current-fiber))
    ;; balances fiber-park-commit!'s disable, on resume — see jolt-fiber-park!.
    (enable-interrupts)))
(def-var! "jolt.host" "fiber-resume" sa-fiber-resume)
;; Unguarded full collect for the R8 gate: System/gc swallows Chez's
;; "cannot collect when multiple threads are active" refusal (the JVM-faithful
;; guarded no-op), but the gate must SEE that refusal when the poller's blocking
;; wait is not collect-safe — a collect that fails proves it.
(def-var! "jolt.host" "gc-full!" (lambda () (sa-gc-collect)))

;; --- jolt.fibers: the public lower-level API (epic jolt-of08.1) ---------------
;; stdlib/jolt/fibers.clj is a thin veneer over these seams. spawn mirrors
;; jolt-fiber-go-spawn — convey the slice, never *txn*, ensure the pool — but
;; returns the FIBER, not a channel, and runs the body UNGUARDED: a throw lands
;; in jolt-fiber-dead! per the R1 contract (state 'dead, error recorded,
;; monitors fired), which is exactly what join/monitor! read. The raw park
;; protocol (fiber-park-commit!/fiber-to-scheduler!) stays out of the public
;; namespace — its commit/switch discipline belongs to the poller.
(def-var! "jolt.host" "fiber-spawn"
  (lambda (f)
    (let ((fib (sa-fiber-spawn (lambda () (*txn* #f) (jolt-invoke f)))))
      (jolt-fiber-ensure-carrier!)
      fib)))
(def-var! "jolt.host" "fiber-instance?" (lambda (x) (if (jolt-fiber? x) #t #f)))
(def-var! "jolt.host" "fiber-yield" (lambda () (sa-fiber-yield) jolt-nil))
;; The callback gets what a jolt catch would bind: the unwrapped error for a
;; fiber that died, nil for a clean completion.
(def-var! "jolt.host" "fiber-monitor!"
  (lambda (fib cb)
    (jolt-fiber-monitor! fib
      (lambda (err) (jolt-invoke cb (if err (jolt-unwrap-throw err) jolt-nil))))
    jolt-nil))
(def-var! "jolt.host" "fiber-state"
  (lambda (fib) (jolt-keyword (symbol->string (jolt-fiber-state fib)))))
;; Meaningful only once the fiber is terminal — join reads it from inside the
;; monitor callback, after jolt-fiber-finish! published state and payload
;; together.
(def-var! "jolt.host" "fiber-result" (lambda (fib) (jolt-fiber-result fib)))
(def-var! "jolt.host" "fiber-carrier-count" (lambda () (jolt-fiber-carrier-count)))
;; nil restores the machine default; validated here (like the preempt seam) so
;; the caller gets a catchable ex-info, not a Scheme error.
(def-var! "jolt.host" "fiber-carrier-count-set!"
  (lambda (n)
    (let ((n (if (jolt-nil? n) #f n)))
      (unless (or (not n) (and (fixnum? n) (fx>? n 0)))
        (jolt-throw (jolt-ex-info "carrier count must be nil or a positive int"
                                  (jolt-hash-map (jolt-keyword "given") (if n n jolt-nil)))))
      (jolt-fiber-carrier-count-set! n)
      jolt-nil)))
(def-var! "jolt.host" "fiber-preempt-ticks" (lambda () jolt-fiber-preempt-ticks-global))
;; Validated here rather than deferring to jolt-fiber-preempt-ticks-set!'s
;; Scheme error so the caller gets a catchable ex-info naming the floor.
;; nil restores the default quantum — there is no "off" (fibers.ss).
(def-var! "jolt.host" "fiber-preempt-ticks-set!"
  (lambda (n)
    (let ((n (if (jolt-nil? n) #f n)))
      (unless (or (not n) (and (fixnum? n) (fx>=? n jolt-fiber-preempt-ticks-min)))
        (jolt-throw (jolt-ex-info
                     (string-append "preempt ticks must be nil or an int >= "
                                    (number->string jolt-fiber-preempt-ticks-min))
                     (jolt-hash-map (jolt-keyword "floor") jolt-fiber-preempt-ticks-min
                                    (jolt-keyword "given") (if n n jolt-nil)))))
      (jolt-fiber-preempt-ticks-set! n)
      jolt-nil)))
