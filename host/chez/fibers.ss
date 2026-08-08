;; host/chez/fibers.ss — the fiber primitive and the carrier pool
;; (R1, epic jolt-nvpr.2; carrier pool + blocking policy R5, jolt-nvpr.6).
;;
;; A fiber is a green thread. R1 built the primitive and a one-shot scheduler;
;; R4 wrapped it in exactly ONE carrier (an OS thread looping drain-then-park);
;; R5 makes it a POOL of N carriers and settles the blocking policy (<!! on a
;; fiber parks — see fibers-async.ss).
;;
;; The design is pinned by R0 (fibers-r0-findings.md, corrections included):
;;   - the per-fiber slice rides in ONE Chez VIRTUAL REGISTER holding the
;;     current fiber record (a thread-parameter write is 33 ns vs 2 ns for a
;;     vreg — three writes per switch measured 8.6x and would drop the 3.4M
;;     switches/sec design point to ~350k). The slot index is
;;     jolt-vreg-current-fiber, allocated with the other vregs in rt.ss; this
;;     file re-defines the same value so the gate test can load it standalone
;;     (the duplicate define is the harmless re-define pattern rt.ss already
;;     uses for scheme-adapter-runtime.ss).
;;   - the run queue is INTRUSIVE: the fiber record carries its own next link,
;;     so the queue costs zero extra per fiber.
;;   - a fiber costs ~3.5 KB from the moment it PARKS (one Chez stack segment),
;;     not once scheduled — spawn-heavy workloads are not cheap; that is the
;;     representation, not a bug to design around.
;;   - exceptions are isolated PER FIBER: a raise inside a fiber kills that
;;     fiber (state 'dead) and never reaches the scheduler loop or the
;;     sa-fiber-run-all caller. R0(b) proved guard handler chains ride the
;;     continuation correctly on Chez, so the catch lives in the resume path.
;;   - call/1cc is the primitive (measured identical to call/cc; one-shot for
;;     the discipline it documents — a continuation is captured fresh per park
;;     and invoked exactly once per resume, so the multi-shot re-entry trap
;;     cannot happen).
;;   - R0(d): a continuation captured on carrier A raises `attempt to return
;;     to stale foreign context` when resumed on B. A FIBER IS BOUND TO ITS
;;     CARRIER FOR LIFE: the record carries its carrier, placement happens
;;     once at spawn (round-robin), there is no work stealing, and a park
;;     resumes on the same carrier's thread, always. The consequences are
;;     written down where the pool is sized (below): growing the pool does not
;;     rescue fibers stranded behind a blocked carrier — a JVM carrier can be
;;     compensated by remounting the continuation, ours cannot.
;;
;; The slice: the record carries a `slice` field holding the fiber's per-fiber
;; dynamic state (a jolt-dslice record — the dyn-binding-stack value, the
;; current namespace, and the STM *txn*). R2 owns the dynamic-binding work
;; (per the round split, dyn-binding.ss is NOT touched here; the swap lives in
;; the switch below).
;;
;; Loaded from rt.ss in the usual place AND from scheme-adapter-runtime.ss
;; (which loads first, so the gate-time adaptercheck, which loads only that
;; file, sees the sa-fiber-* names bound). Self-contained: uses nothing beyond
;; Chez natives (guarded references to the runtime's var machinery and
;; processor probe), so the gate test loads it directly.

;; --- the fiber record -------------------------------------------------------
;; state: 'ready (on the run queue) | 'running | 'parked (waiting on
;; sa-fiber-resume) | 'done | 'dead (raised; error field holds the condition).
;; thunk: the fiber body (immutable). k: the one-shot continuation captured at
;; the last park (unconsumed while 'parked). result/error: completion payload.
;; next: intrusive run-queue link. slice: R2's per-fiber dynamic slice (a
;; jolt-dslice: dyn-binding-stack value, current ns, *txn* — see below).
;; carrier: the fiber's carrier record, fixed at spawn, never changed — R0(d)
;; pins a fiber to its carrier for life (a continuation captured on carrier A
;; cannot resume on B).
(define-record-type jolt-fiber
  (fields (mutable state)
          thunk
          (mutable k)
          (mutable result)
          (mutable error)
          (mutable next)
          (mutable slice)
          carrier
          ;; R7: the state-machine resume continuation (sm.ss). For a state-
          ;; machine go the fiber NEVER captures a continuation (k stays #f —
          ;; the scheduler re-runs the thunk, the driver, on resume) and this
          ;; field holds the closure to run next. #f for a continuation fiber.
          (mutable sm-k))
  (nongenerative jolt-fiber-v1))

;; --- the per-fiber dynamic slice ---------------------------------------------
;; R2 (jolt-nvpr.3). jolt's `binding` macro pushes by calling the
;; dyn-binding-stack thread parameter as a SETTER, and a setter write is not
;; undone by a continuation escape (R0(a)): a fiber that parks inside a binding
;; leaves its frames on the carrier, visible to the scheduler and to every
;; other fiber, and a second fiber popping its own frame can pop the parked
;; fiber's. `set-chez-ns!` leaks the same way. The swap below saves the fiber's
;; slice on switch-out and restores the incoming party's on switch-in.
;;
;; The scheduler's own slice is captured per drain entry, so the carrier
;; reverts to the CALLER's state between fibers (the parked-fiber leak
;; regression). *txn* is parameterize-managed inside dosync, so it unwinds on
;; park on its own (R0(a)) — its two jobs in the slice are: a fiber parked
;; inside a dosync resumes INSIDE its txn, and sa-fiber-spawn does NOT convey
;; it (async-go-spawn parity: a child whose first dosync joined the parent's
;; txn would write into the parent's log).
;;
;; Writes are diffed with eq?: a thread-parameter WRITE is ~33 ns vs ~2 ns to
;; read (R0(c)), so a swap between two parties with identical slices — the
;; common case — costs a few reads and zero writes.
(define-record-type jolt-dslice
  (fields (mutable stack) (mutable ns) (mutable txn))
  (nongenerative jolt-dslice-v1))

;; The virtual-register slot holding the current fiber record (0 = not on a
;; fiber — a fresh thread starts every slot at fixnum 0, NOT #f). Allocated
;; with the other vregs in rt.ss (jolt-vreg-site 2 / catch-line 3 /
;; print-readably 4); this duplicate definition keeps the file self-contained
;; for the standalone gate and is a harmless re-define under the full boot.
(define jolt-vreg-current-fiber 0)
;; Slot 1: non-zero while a PARK escape is unwinding THIS carrier. Read by the
;; try/finally after-thunk (values.ss jolt-park-unwinding?) so a park does not
;; run cleanup that belongs to the real exit. A vreg and not a global BECAUSE
;; R5 runs several carriers, each of which can be mid-park independently: a
;; global would be written by one carrier's park and cleared by another's,
;; letting the second park's finally run at the wrong time (the R5 gate check
;; 5). Virtual registers are per thread, so each carrier owns its flag.
(define jolt-vreg-park-unwinding 1)
(define (jolt-park-unwinding-set! on?)
  (set-virtual-register! jolt-vreg-park-unwinding (if on? 1 0)))
;; Installed only when the full runtime is present; a standalone load of this
;; file (the R1 gate) has no values.ss, and the guard keeps that working — the
;; same probe pattern this file already uses for the slice parameters.
(guard (e (#t #f))
  (set! jolt-park-unwinding?-hook
        (lambda () (eqv? 1 (virtual-register jolt-vreg-park-unwinding)))))

;; --- the carrier and the pool ------------------------------------------------
;; A carrier is an OS thread running the R4 loop shape: drain its run queue,
;; then park on its condition when empty — the emptiness check and the wait
;; under the SAME mutex, so an enqueue cannot slip between them (the wake
;; signals the condition; a signal cannot be lost between the check and the
;; wait).
;;
;; Everything per-carrier lives in THIS record, not in globals:
;;   - the queue (head/tail) is guarded by mu, and enqueues can come from ANY
;;     thread (a channel delivery to a fiber-waiter on a different carrier, or
;;     a plain thread), while dequeues happen on this carrier's thread. Lock
;;     order: ... → run-queue mu is ALWAYS the last lock acquired; the
;;     enqueue/dequeue paths never acquire anything else, so the order never
;;     cycles.
;;   - sched-k is the scheduler's resume continuation. R4 kept this in a
;;     global and argued it was safe "because a fiber can only park while ITS
;;     carrier's scheduler is running it" — under a POOL that argument dies:
;;     two carriers running fibers CONCURRENTLY would overwrite each other's
;;     global, and a fiber would park into the other carrier's scheduler
;;     continuation (invoking a continuation captured on another thread —
;;     exactly the stale-context failure R0(d) rules out). It lives here, in
;;     the carrier each fiber points at, so a park always finds its own.
;;   - sched-slice is the caller's dynamic state at drain entry. R4 kept one
;;     shared record; under a pool, carrier A's capture would be overwritten
;;     by carrier B's, and A's restore would set A's THREAD PARAMETERS to B's
;;     values (thread parameters are per thread). Per carrier, allocated once.
;; The park-unwinding flag is a vreg (slot 1) for exactly this reason — per
;; thread — so it needs no carrier record at all.
(define-record-type jolt-carrier
  (fields mu cv (mutable head) (mutable tail)
          (mutable sched-k) sched-slice (mutable thread) (mutable stop?))
  (nongenerative jolt-carrier-v1))

;; The pool. jolt-fiber-carriers is the vector of carrier records, built at
;; the FIRST spawn with the current count; carrier THREADS start lazily at the
;; first :fiber go spawn (jolt-fiber-ensure-carrier!, called from
;; jolt-fiber-go-spawn in fibers-async.ss). The split matters: the R1-R3 gates
;; spawn raw fibers and drain them SYNCHRONOUSLY with sa-fiber-run-all on the
;; calling thread, and must not get carrier threads racing them.
;;
;; Placement is round-robin at spawn: jolt-fiber-rr is read and advanced under
;; jolt-fiber-rr-mu, so concurrent spawns from any thread get a strict,
;; predictable rotation. No work stealing: an idle carrier cannot take
;; another's queued fibers, because a fiber cannot move carriers.
;;
;; THE POOL IS A THROUGHPUT KNOB, NOT A RESCUE MECHANISM. A fiber queued
;; behind a blocked carrier stays queued, however big the pool gets — the JVM
;; can compensate for pinning by adding a carrier because its continuations
;; remount; ours cannot (R0(d)). Do not "fix" stranding by growing the pool;
;; the plan's rule is to keep carriers from pinning in the first place (R8:
;; parkable IO + offload), with `thread` as the documented escape.
(define jolt-fiber-carriers #f)           ; vector of jolt-carrier, or #f
(define jolt-fiber-rr 0)                  ; round-robin cursor (under rr-mu)
(define jolt-fiber-rr-mu (make-mutex))
(define jolt-fiber-pool-mu (make-mutex))  ; guards pool start/reset
(define jolt-fiber-pool-started? #f)

;; The pool size. N defaults to the machine's processor count (jolt's
;; established probe, rt.ss jolt-available-processors — affinity first, then
;; sysctl/sysconf/env; a standalone load has no runtime and falls back to 1,
;; which is also the value the R1-R3 gates pin explicitly for determinism).
;; Overridable by the jolt var clojure.core.async/*fiber-carrier-count* (a
;; program pins it, e.g. to 1, BEFORE the first :fiber go spawn) or by the
;; host setter (tests, embedding). #f at any level means "unset". Read once,
;; at the first pool start; change it via jolt-fiber-pool-reset! + a new
;; count.
(define jolt-fiber-carrier-count-global #f)
(define (jolt-fiber-probe-count)
  (guard (e (#t 1)) (jolt-available-processors)))
(define (jolt-fiber-carrier-count)
  (let ((v (guard (e (#t #f))
             (let ((cell (var-cell-lookup "clojure.core.async" "*fiber-carrier-count*")))
               (if (and cell (var-cell-defined? cell)) (var-cell-root cell) #f)))))
    (or (and (fixnum? v) (fx>? v 0) v)
        (and (fixnum? jolt-fiber-carrier-count-global)
             (fx>? jolt-fiber-carrier-count-global 0)
             jolt-fiber-carrier-count-global)
        (jolt-fiber-probe-count))))
;; (jolt-fiber-carrier-count-set! n | #f) — set the pool size for the NEXT
;; pool start; #f restores the machine default. Writes both the host global
;; and the var's root cell, so the two knobs never disagree.
(define (jolt-fiber-carrier-count-set! n)
  (when (and n (not (and (fixnum? n) (fx>? n 0))))
    (error 'jolt-fiber-carrier-count-set!
           "carrier count must be a positive fixnum or #f" n))
  (set! jolt-fiber-carrier-count-global n)
  (guard (e (#t #f))
    (let ((cell (var-cell-lookup "clojure.core.async" "*fiber-carrier-count*")))
      (when (and cell (var-cell-defined? cell))
        (var-cell-root-set! cell n)))))

;; Build the carrier vector at the current count, exactly once. Double-build
;; is guarded under rr-mu (double-checked): two threads spawning for the first
;; time concurrently must not each build a vector and place fibers on
;; carriers that then get orphaned.
(define (jolt-fiber-ensure-carriers!)
  (unless jolt-fiber-carriers
    (mutex-acquire jolt-fiber-rr-mu)
    (unless jolt-fiber-carriers
      (let* ((n (jolt-fiber-carrier-count))
             (v (make-vector n)))
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (vector-set! v i
            (make-jolt-carrier (make-mutex) (make-condition) #f #f #f
                               (make-jolt-dslice #f #f #f) #f #f)))
        (set! jolt-fiber-carriers v)))
    (mutex-release jolt-fiber-rr-mu)))

;; Pick the next carrier by round-robin. Runs ONCE at spawn — the fiber never
;; changes carrier (R0(d)). The read-and-advance of jolt-fiber-rr happens
;; under rr-mu, so concurrent spawns from any thread get a strict, predictable
;; rotation (no two spawns reserve the same slot).
(define (jolt-fiber-pick!)
  (jolt-fiber-ensure-carriers!)
  (let ((v jolt-fiber-carriers)
        (n (vector-length jolt-fiber-carriers)))
    (mutex-acquire jolt-fiber-rr-mu)
    (let ((c (vector-ref v (mod jolt-fiber-rr n))))
      (set! jolt-fiber-rr (fx+ jolt-fiber-rr 1))
      (mutex-release jolt-fiber-rr-mu)
      c)))

;; The intrusive per-carrier queue. `next` lives in each fiber record.
;; jolt-fiber-enqueue!/locked: mu must already be held. The empty→non-empty
;; transition signals the carrier's condition so a parked carrier wakes (the
;; wake is exactly the R4 design: check and wait hold the same mutex).
(define (jolt-fiber-enqueue! c f)
  (mutex-acquire (jolt-carrier-mu c))
  (if (jolt-carrier-tail c)
      (begin (jolt-fiber-next-set! (jolt-carrier-tail c) f)
             (jolt-carrier-tail-set! c f))
      (begin (condition-signal (jolt-carrier-cv c))
             (jolt-carrier-head-set! c f)
             (jolt-carrier-tail-set! c f)))
  (mutex-release (jolt-carrier-mu c)))

(define (jolt-fiber-dequeue! c)
  (mutex-acquire (jolt-carrier-mu c))
  (let ((f (jolt-carrier-head c)))
    (when f
      (jolt-carrier-head-set! c (jolt-fiber-next f))
      (unless (jolt-carrier-head c) (jolt-carrier-tail-set! c #f))
      ;; clear the link so a completed fiber does not retain the queue
      (jolt-fiber-next-set! f #f))
    (mutex-release (jolt-carrier-mu c))
    f))

;; The three thread parameters that make up a fiber's dynamic slice live in
;; other host files (dyn-binding.ss's dyn-binding-stack, multimethods.ss's
;; chez-current-ns-param, refs.ss's *txn*). The full boot defines all three
;; BEFORE this file's last load (rt.ss loads fibers.ss last), so these
;; references capture the real parameters there; a standalone load (the R1
;; gate, or scheme-adapter-runtime.ss before the rest of rt.ss) sees them
;; unbound and gets a private fallback parameter instead — behaviorally
;; identical for the R1 semantics, and rt.ss's later re-load of this file
;; re-captures the real ones (the harmless re-define pattern this file already
;; uses for jolt-vreg-current-fiber). The probe is a guard on the reference,
;; not top-level-bound? (blocklisted: fibers.ss is not a target-owned file).
(define jolt-slice-stack-param
  (guard (e (#t (make-thread-parameter '())))
    dyn-binding-stack))
(define jolt-slice-ns-param
  (guard (e (#t (make-thread-parameter "user")))
    chez-current-ns-param))
(define jolt-slice-txn-param
  (guard (e (#t (make-thread-parameter #f)))
    *txn*))

;; The scheduler's own slice — the caller's dynamic state at drain entry —
;; kept per carrier in one mutable record so the per-run capture allocates
;; nothing. NOT a global: a shared record would be written by every carrier's
;; drain, and a park's restore could put another carrier's thread parameters
;; onto this one (thread parameters are per thread).
(define (jolt-carrier-sched-slice-capture! c)
  (let ((s (jolt-carrier-sched-slice c)))
    (jolt-dslice-stack-set! s (jolt-slice-stack-param))
    (jolt-dslice-ns-set! s (jolt-slice-ns-param))
    (jolt-dslice-txn-set! s (jolt-slice-txn-param))))

;; Save the CURRENT carrier values into fiber f's slice record. Runs in f's own
;; dynamic context, BEFORE the switch invokes the scheduler continuation — the
;; parameterize unwind fires as part of that invocation, so reading earlier is
;; the only way to capture a txn a fiber is parked inside.
(define (jolt-fiber-slice-save! f)
  (let ((s (jolt-fiber-slice f)))
    (jolt-dslice-stack-set! s (jolt-slice-stack-param))
    (jolt-dslice-ns-set! s (jolt-slice-ns-param))
    (jolt-dslice-txn-set! s (jolt-slice-txn-param))))

;; Restore the carrier to slice s's values. Writes are diffed with eq?: a
;; thread-parameter WRITE is ~33 ns vs ~2 ns to read (R0(c)), so a swap between
;; two fibers with identical slices (the common case — empty stacks, same ns,
;; no txn) costs the reads and zero writes. eq? can only skip a write when the
;; carrier already holds the exact object, so it can never miss a change.
(define (jolt-fiber-slice-restore! s)
  (when s
    (let ((v (jolt-dslice-stack s)))
      (unless (eq? v (jolt-slice-stack-param)) (jolt-slice-stack-param v)))
    (let ((v (jolt-dslice-ns s)))
      (unless (eq? v (jolt-slice-ns-param)) (jolt-slice-ns-param v)))
    (let ((v (jolt-dslice-txn s)))
      (unless (eq? v (jolt-slice-txn-param)) (jolt-slice-txn-param v)))))

(define (jolt-current-fiber)
  (let ((r (virtual-register jolt-vreg-current-fiber)))
    (if (eq? r 0) #f r)))

;; --- the switch -------------------------------------------------------------
;; Symmetric two-party switch over call/1cc. Each side captures a fresh
;; continuation per park; the parked continuation is invoked exactly once by
;; the scheduler's resume path, so no continuation is ever invoked twice (the
;; one-shot discipline — a multi-shot re-entry would return into the caller's
;; half-finished expression and re-run it, the exact trap the plan warns
;; about). Chez represents a continuation as a lazily-split stack segment, so
;; capture is O(1) and depth-independent (R0: identical cost at 1 and 40
;; frames).

;; Park the CURRENT fiber: capture its continuation, hand control to the
;; scheduler (invoking the continuation ITS carrier's scheduler captured when
;; it started this fiber — the carrier is read from the fiber, so two carriers
;; can never hand a fiber to the wrong scheduler). The fiber's state is set by
;; the caller BEFORE the switch (yield -> 'ready + enqueue; park -> 'parked).
(define (jolt-fiber-to-scheduler! f)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  (call/1cc
    (lambda (k)
      (jolt-fiber-k-set! f k)
      (jolt-fiber-slice-save! f)
      ;; The dynamic-wind after-thunks between here and the scheduler are about
      ;; to fire as this continuation unwinds. They belong to forms the fiber is
      ;; still inside, so flag the escape as a park and let them skip.
      (jolt-park-unwinding-set! #t)
      ((jolt-carrier-sched-k (jolt-fiber-carrier f))))))

;; (sa-fiber-yield) -> void. Park the current fiber and move it to the back of
;; its carrier's run queue (round-robin); returns when the scheduler resumes
;; it. An error outside a fiber — the vreg read is the "am I on a fiber?"
;; dispatch R0's design calls out.
(define (sa-fiber-yield)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-fiber-state-set! f 'ready)
               (jolt-fiber-enqueue! (jolt-fiber-carrier f) f)
               (jolt-fiber-to-scheduler! f))
        (error 'sa-fiber-yield "yield called outside a fiber"))))

;; Park WITHOUT re-enqueueing: the fiber is not runnable until sa-fiber-resume.
;; Internal for R1 — this is the park shape R3's channel waiters use (a take!
;; whose callback resumes the fiber) — and what makes sa-fiber-resume real.
(define (jolt-fiber-park!)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-fiber-state-set! f 'parked)
               (jolt-fiber-to-scheduler! f))
        (error 'jolt-fiber-park! "park called outside a fiber"))))

;; (sa-fiber-resume f) -> void. Make a PARKED fiber runnable again (enqueue on
;; ITS carrier — the resume can come from any thread, and the enqueue lands on
;; the fiber's own carrier, never another's). A no-op when the fiber is
;; already runnable — a double wakeup (a value and a timeout both firing is
;; exactly the R4 alts! commit race) must not corrupt the queue.
(define (sa-fiber-resume f)
  (when (eq? (jolt-fiber-state f) 'parked)
    (jolt-fiber-state-set! f 'ready)
    (jolt-fiber-enqueue! (jolt-fiber-carrier f) f)))

;; (sa-fiber-spawn thunk) -> fiber. Create a fiber running THUNK, place it
;; round-robin on a carrier, and make it runnable; return the record.
;; Spawning inside a fiber is legal. The child's slice CONVEYS the parent's
;; current dynamic state (the carrier's live values at spawn — reading the
;; params in the parent's context), exactly as async-go-spawn snapshots
;; (dyn-binding-stack) for a thread today; *txn* is always #f so a child
;; spawned inside a dosync cannot join the parent's transaction (ref-sets into
;; the parent's log would be committed by the parent, not the child).
(define (sa-fiber-spawn thunk)
  (let ((c (jolt-fiber-pick!)))
    (let ((f (make-jolt-fiber
              'ready thunk #f #f #f #f
              (make-jolt-dslice (jolt-slice-stack-param)
                                (jolt-slice-ns-param)
                                #f)
              c #f)))
      (jolt-fiber-enqueue! c f)
      f)))

;; --- the scheduler ----------------------------------------------------------
;; Resume (or first-run) fiber f on ITS carrier's thread, returning to the
;; loop when f parks, finishes, or dies.
(define (jolt-fiber-run f)
  (let ((c (jolt-fiber-carrier f)))
    (set-virtual-register! jolt-vreg-current-fiber f)
    (call/1cc
      (lambda (k)
        (jolt-carrier-sched-k-set! c k)
        ;; scheduler -> fiber: restore the incoming fiber's slice BEFORE running
        ;; it (for a resume, before its continuation re-enters — the dynamic-wind
        ;; before-thunks then re-fire over the restored values)
        (jolt-fiber-slice-restore! (jolt-fiber-slice f))
        (jolt-fiber-resume* f)))
    ;; Back on the scheduler: whatever escape brought us here is over.
    (jolt-park-unwinding-set! #f)
    ;; The fiber parked, finished, or died: its setter-written dynamic state
    ;; (binding frames, current ns) is still live on the carrier — a continuation
    ;; escape does not undo a setter write (R0(a)). fiber -> scheduler: revert to
    ;; the scheduler's slice so the carrier between fibers is the CALLER's state.
    (jolt-fiber-slice-restore! (jolt-carrier-sched-slice c))))

;; Per-fiber exception isolation: the guard frame sits BELOW the fiber's own
;; frames and is part of the fiber's captured continuation, so it catches a
;; raise whether the fiber is on its first run or resumed from a park — and a
;; raise the fiber's own handlers do not catch kills the fiber, not the
;; scheduler. R0(b) verified guard chains ride the continuation correctly.
;; The discriminator is the continuation, not the state: a fiber that yielded
;; is 'ready AND holds a captured k, so it must be resumed at the park point —
;; re-applying the thunk would re-run it from scratch (an infinite loop). Only
;; a 'ready fiber with NO k is a first run. 'parked fibers are never dequeued:
;; sa-fiber-resume moves them to 'ready before enqueue.
(define (jolt-fiber-resume* f)
  (case (jolt-fiber-state f)
    ((ready)
     (if (jolt-fiber-k f)
         ((jolt-fiber-k f))
         (begin
           (jolt-fiber-state-set! f 'running)
           (let ((r (guard (e (#t (jolt-fiber-dead! f e)))
                       ((jolt-fiber-thunk f)))))
             (jolt-fiber-done! f r)))))
    (else (error 'jolt-fiber-run "fiber in unexpected state"
                 (jolt-fiber-state f)))))

;; Completion paths: mark the fiber, drop the consumed continuation, clear the
;; current-fiber vreg (the scheduler owns the CPU now — a stale vreg would make
;; a later yield from a non-fiber context enqueue a dead fiber and invoke the
;; consumed sched-k), then hand control back to the scheduler — again via the
;; fiber's OWN carrier, the only one that can be running it. The carrier field
;; is deliberately NOT cleared: placement is fixed at spawn (R0(d)) and the
;; gates assert on it after completion.
(define (jolt-fiber-done! f r)
  (jolt-fiber-state-set! f 'done)
  (jolt-fiber-result-set! f r)
  (jolt-fiber-k-set! f #f)
  (jolt-fiber-slice-set! f #f)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  ((jolt-carrier-sched-k (jolt-fiber-carrier f))))

(define (jolt-fiber-dead! f e)
  (jolt-fiber-state-set! f 'dead)
  (jolt-fiber-error-set! f e)
  (jolt-fiber-k-set! f #f)
  (jolt-fiber-slice-set! f #f)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  ((jolt-carrier-sched-k (jolt-fiber-carrier f))))

;; (jolt-fiber-drain! c) -> void. Run carrier c's run queue until it drains —
;; the scheduler shape the plan names ("run ready fibers until the queue
;; drains, then block in the poller"); the poll step is R8's. A fiber that
;; parks (jolt-fiber-park!) stops the drain until resumed. The scheduler's
;; slice is captured at entry — the caller's dynamic state, which every park
;; restores onto the carrier.
(define (jolt-fiber-drain! c)
  (jolt-carrier-sched-slice-capture! c)
  (let loop ()
    (let ((f (jolt-fiber-dequeue! c)))
      (when f
        (jolt-fiber-run f)
        (loop)))))

;; (sa-fiber-run-all) -> void. Drain carrier 0's run queue on the calling
;; thread — the R1-R3 gate API (spawn + run-all synchronously, no carrier
;; threads). Those gates pin the pool to 1 carrier, so every fiber lands here;
;; nothing calls run-all once a pool is live (a manual pump would race carrier
;; 0's thread over its queue — the R4+ gates wait on fiber state instead).
(define (sa-fiber-run-all)
  (jolt-fiber-ensure-carriers!)
  (jolt-fiber-drain! (vector-ref jolt-fiber-carriers 0)))

;; --- the carrier loop and the pool lifecycle (R4 carrier, generalized R5) ----
;; R3 found that sa-fiber-run-all is a ONE-SHOT drain, not a scheduler: a
;; cross-thread wake lands a fiber on the queue after the drain returned and
;; nothing runs it (the R3 gate had to pump). R4's go-on-fibers therefore
;; needs a carrier that LOOPS; R5 gives every carrier the same loop. Each
;; carrier thread is started lazily at the first :fiber go spawn
;; (jolt-fiber-ensure-carrier!, called from jolt-fiber-go-spawn in
;; fibers-async.ss) and parks on its own condition when its queue is empty —
;; never a spin; a wake (enqueue) signals it.
;;
;; The loop is run-all-then-park: drain the queue, and if it is empty, wait on
;; the condition. The check and the wait both hold the carrier's mutex, so an
;; enqueue cannot slip between them: it either lands before the check (the
;; carrier sees a non-empty queue and does not wait) or signals the condition
;; the carrier is (or is about to be) waiting on. stop? (pool reset) is also
;; checked under the mutex AFTER a drain, so a stopped carrier finishes its
;; queue first.
(define (jolt-fiber-carrier-loop c)
  (let loop ()
    (jolt-fiber-drain! c)
    (mutex-acquire (jolt-carrier-mu c))
    (cond
      ((jolt-carrier-stop? c) (mutex-release (jolt-carrier-mu c)) (void))
      ((jolt-carrier-head c) (mutex-release (jolt-carrier-mu c)) (loop))
      (else (condition-wait (jolt-carrier-cv c) (jolt-carrier-mu c))
            (mutex-release (jolt-carrier-mu c))
            (loop)))))

;; Start the pool exactly once, on the first :fiber go spawn. Double-start is
;; guarded by its own mutex (the started? flag); each carrier thread inherits
;; the spawner's thread parameters at fork, which is irrelevant here — every
;; drain re-captures the scheduler slice at entry.
(define (jolt-fiber-ensure-carrier!)
  (mutex-acquire jolt-fiber-pool-mu)
  (unless jolt-fiber-pool-started?
    (set! jolt-fiber-pool-started? #t)
    (jolt-fiber-ensure-carriers!)
    (let ((v jolt-fiber-carriers) (n (vector-length jolt-fiber-carriers)))
      (do ((i 0 (fx+ i 1))) ((fx=? i n))
        (let ((c (vector-ref v i)))
          (jolt-carrier-thread-set! c
            (fork-thread (lambda () (jolt-fiber-carrier-loop c))))))))
  (mutex-release jolt-fiber-pool-mu))

;; (jolt-fiber-pool-reset!) -> void. Stop every carrier thread (each finishes
;; its queue, then exits on the stop flag), join them, and drop the pool so
;; the next spawn rebuilds it — with a possibly different count (set via
;; jolt-fiber-carrier-count-set! between a reset and the next pool start).
;; Test/embedding API: a program pins the count for determinism; resetting is
;; how the count changes. Call only when the fibers are quiescent — a carrier
;; mid-fiber finishes that fiber before exiting (the join waits), but a
;; PARKED fiber is abandoned: no thread is left to resume it.
(define (jolt-fiber-pool-reset!)
  (mutex-acquire jolt-fiber-pool-mu)
  (let ((v jolt-fiber-carriers))
    (when v
      (let ((n (vector-length v)))
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (let ((c (vector-ref v i)))
            (mutex-acquire (jolt-carrier-mu c))
            (jolt-carrier-stop?-set! c #t)
            (condition-broadcast (jolt-carrier-cv c))
            (mutex-release (jolt-carrier-mu c))))
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (let ((t (jolt-carrier-thread (vector-ref v i))))
            (when t (thread-join t)))))))
  (mutex-acquire jolt-fiber-rr-mu)
  (set! jolt-fiber-carriers #f)
  (set! jolt-fiber-rr 0)
  (mutex-release jolt-fiber-rr-mu)
  (set! jolt-fiber-pool-started? #f)
  (mutex-release jolt-fiber-pool-mu))
