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
;; file, sees the sa-fiber-* names bound). Nearly self-contained: beyond Chez
;; natives it needs ONLY host/chez/locks.ss, which every loader of this file
;; loads first, because every lock in the runtime routes through the counting
;; wrapper there and jolt-with-mutex is a macro rather than something that can
;; be captured at run time. Everything else it borrows from the runtime (the var
;; machinery, the processor probe, the adapter seams) is a guarded reference
;; with a working fallback, so a standalone load still runs.

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
;; sm: the pending step of a fiber whose body was CPS'd (java/sm.ss), #f for
;; every other fiber. A cheap park stores the rest of the computation here
;; instead of capturing a continuation, clears k, and lets the thunk — a
;; re-entrant driver — pick the step up on the next run.
(define-record-type jolt-fiber
  (fields (mutable state)
          thunk
          (mutable k)
          (mutable result)
          (mutable error)
          (mutable next)
          (mutable slice)
          carrier
          (mutable sm)
          (mutable monitors)
          (mutable sic)
          (mutable interrupt)
          (mutable parked-on)
          (mutable mask)
          ;; the handler of this fiber's latest interruptible channel wait
          ;; (<!!, >!!, alts!!), or #f if it never made one — see
          ;; jolt-fiber-commit-park/ibox!
          (mutable iwait)
          ;; A fiber is a VIRTUAL THREAD: its own interrupt flag, which every
          ;; interruptible wait and Thread/interrupted read while it runs
          ;; (host-static-methods.ss current-interrupt-box), and its own
          ;; java.lang.Thread object, made the first time someone asks for it
          ;; (java/io.ss current-thread-handle). The box is made on first use
          ;; (jolt-fiber-ibox!), so a fiber that never meets an interruptible
          ;; wait or its own Thread object does not carry one.
          (mutable ibox)
          (mutable thread)
          ;; what a park through jolt-lock-wait is, for Thread.getState when it
          ;; is not plain WAITING: TIMED_WAITING for a wait with a deadline,
          ;; BLOCKED for entering a monitor. #f otherwise, cleared on the resume.
          (mutable wstate)
          ;; where a kill (jolt-fiber-kill!) leaves to: the continuation of the
          ;; fiber's entry, and of its outermost unmasked region while it is in
          ;; one (#f otherwise). A kill escapes rather than raises, so no guard
          ;; -- no try/catch -- between the safe point and here sees it.
          (mutable exit-k)
          (mutable edge-k))
  (nongenerative jolt-fiber-v8))

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

;; --- dropping the finally winders on a park ----------------------------------
;; A park escape unwinds the fiber's dynamic-wind chain on its way to the
;; scheduler. Most of that chain SHOULD unwind: a parameterize has to put the
;; carrier back to the scheduler's value or the next fiber inherits it, and a
;; with-mutex has to release, which is what lets loader.ss park inside its load
;; lock. What must NOT run is a jolt `finally`, because a park is not an exit.
;;
;; So instead of suppressing the finally from inside the after-thunk (which cost
;; two procedure calls on EVERY finally exit, park or not), the park removes
;; those winders from the chain before escaping. Chez then never runs them.
;;
;; The chain itself is reached through the adapter (sa-winder-in,
;; sa-current-winders, sa-current-winders-set!), because $primitive access is
;; the adapter's job and this file is not target-owned. They are captured by
;; GUARDED REFERENCE for the same reason the slice parameters below are: the R1
;; gate loads this file on its own, with no adapter present, and the header's
;; promise that it is self-contained has to keep holding. The fallbacks make the
;; filter a no-op rather than an error.
;;
;; The marker is captured the same way this file captures the slice parameters
;; below: a guarded reference, because scheme-adapter-runtime.ss loads this file
;; BEFORE values.ss defines jolt-finally-in. That load gets #f here, which makes
;; the predicate false everywhere and the filter a no-op — exactly the
;; pre-existing behaviour, which is all the R1 gate needs. rt.ss re-loads this
;; file after values.ss and re-captures the real procedure, the same harmless
;; re-define pattern used for jolt-vreg-current-fiber.
(define jolt-finally-marker
  (guard (e (#t #f)) jolt-finally-in))
(define jolt-sa-winders
  (guard (e (#t (lambda () '()))) sa-current-winders))
(define jolt-sa-winders-set!
  (guard (e (#t (lambda (w) #f))) sa-current-winders-set!))
;; The rtd and its `in` accessor, not the sa-winder-in wrapper: this sits in a
;; loop over the whole chain on every park, and going through the wrapper cost
;; two indirect calls per element (measured 32 ns/park against 12 ns for the
;; hoisted form on a 6-winder chain). record-rtd and record-accessor are plain
;; R6RS, so naming them here does not put a $primitive in this file.
(define jolt-winder-rtd
  (guard (e (#t #f)) sa-winder-rtd))
(define jolt-winder-in-ref
  (guard (e (#t #f)) (record-accessor jolt-winder-rtd 0)))

;; A winder the back end emitted for a `finally` — recognised by its `in` thunk
;; being the one shared marker (values.ss jolt-finally-in). Every other winder,
;; including with-mutex, parameterize and any host dynamic-wind, answers #f and
;; is left alone. Kept as a named predicate for the gates; the park path below
;; inlines it with the globals hoisted out of the loop.
(define (jolt-finally-winder? r)
  (and jolt-winder-rtd
       jolt-finally-marker
       (eq? (record-rtd r) jolt-winder-rtd)
       (eq? (jolt-winder-in-ref r) jolt-finally-marker)))

;; Slot 6: the winder chain as it stood when this carrier dispatched the running
;; fiber — the fiber's BASE. Set once per dispatch in jolt-fiber-run, read only
;; by the walk below. A vreg for the same reason slot 1 is one: it is per
;; carrier thread, and several carriers dispatch independently. A fresh thread
;; starts the slot at fixnum 0, which is not a list, so the walk simply runs to
;; the end — the correct answer, just without the shortcut.
(define jolt-vreg-fiber-winder-base 6)

;; --- the interrupt discipline (swish erlang.ss, ported) ----------------------
;; A region that must not be preempted disables interrupts. Chez DEFERS a timer
;; raised while they are off and delivers it at the enable, so this covers every
;; region so marked without the scheduler enumerating them — which a hand-rolled
;; "am I in a critical section" counter cannot do, and which is why the earlier
;; one kept missing regions (a channel op, a commit-to-park, a yield transition,
;; each found only by tightening the quantum until it broke).
;;
;; The piece that makes it work, and the piece that was skipped the first time:
;; the disable COUNT has to ride across a context switch. This handler escapes to
;; the scheduler rather than returning, so an (enable-interrupts) it interrupts
;; never completes and the count would stay raised on that carrier forever. Swish
;; carries it in pcb-sic and rebuilds it on the far side (erlang.ss:958-968);
;; jolt-fiber-sic is that field, and jolt-adjust-interrupts! is that loop.
;;
;; The count is READ from the thread context, never derived. A disable/enable
;; pair also answers the question — both primitives return the new count — but a
;; pair is not a read: the enable is a delivery point for anything deferred
;; while interrupts were off, and every caller here is on a park path that has
;; already dropped its finally winders and committed to leaving. Swish reads it
;; too (erlang.ss:792). The fallback keeps the R1 gate's standalone load of this
;; file working, where no adapter has been loaded. It is the old derivation, so
;; it carries the old delivery window — that gate arms a timer like any other
;; drain does — but it is what this file did everywhere until now, so the
;; standalone path is no worse than it was and every path with an adapter is
;; better. Section 10 of the preempt gate is what holds that line.
(define jolt-current-disable-count
  (guard (e (#t (lambda () (let ((n (disable-interrupts))) (enable-interrupts) (fx- n 1)))))
    sa-disable-count))
(define (jolt-adjust-interrupts! from to)
  (let loop ((n from))
    (cond ((fx>? n to) (enable-interrupts) (loop (fx- n 1)))
          ((fx<? n to) (disable-interrupts) (loop (fx+ n 1)))
          (else (void)))))

;; The chain with the finally winders removed, stopping at BASE. Returns the
;; ARGUMENT ITSELF when nothing is dropped — the overwhelmingly common case — so
;; a park with no finally in scope allocates nothing, and the tail is shared when
;; the dropped winders are outermost. The loop-invariant globals are passed in
;; rather than read per element; see the note on jolt-winder-rtd.
;;
;; Stopping there is not just an optimisation, it is the exact boundary: a park
;; escapes to the carrier's sched-k, whose own chain IS base, so Chez unwinds
;; only the winders ABOVE base. Anything at or below it is untouched by the
;; escape, so filtering it could not change what runs — and walking it is pure
;; cost, which at depth 0 is the entire cost (the fiber's chain and the base are
;; then the same list and this returns on the first test).
(define (jolt-park-winders* w base rtd in-ref marker)
  (cond ((null? w) w)
        ((eq? w base) w)
        ((let ((r (car w)))
           (and (eq? (record-rtd r) rtd) (eq? (in-ref r) marker)))
         (jolt-park-winders* (cdr w) base rtd in-ref marker))
        (else
         (let ((rest (jolt-park-winders* (cdr w) base rtd in-ref marker)))
           (if (eq? rest (cdr w)) w (cons (car w) rest))))))

;; (jolt-park-drop-finallys!) — called at every park, just before the escape.
;;
;; DELIBERATELY NOT WRAPPED IN `guard`, and nothing that mutates the chain may
;; be: `guard` saves the winder chain on entry and RESTORES it on exit, so the
;; write below would be silently undone and every finally would run mid-park
;; again — with no error to say so. That is not hypothetical, it is how the
;; first version of this function failed. The same goes for dynamic-wind,
;; with-mutex and parameterize: the mutation has to happen in the park's own
;; frame, on the way out.
;;
;; It needs no guard anyway. Reading and writing the chain cannot fail, and when
;; jolt-finally-marker is #f (the adapter-time load, before values.ss)
;; jolt-finally-winder? is false everywhere, jolt-park-winders returns its
;; argument, and no write happens at all — exactly the pre-existing behaviour.
(define (jolt-park-drop-finallys!)
  (let ((rtd jolt-winder-rtd)
        (marker jolt-finally-marker))
    ;; No rtd or no marker means nothing can ever match, so skip the walk
    ;; entirely rather than traverse the chain to learn that.
    (when (and rtd marker)
      (let ((w (jolt-sa-winders))
            (base (virtual-register jolt-vreg-fiber-winder-base)))
        (unless (or (null? w) (eq? w base))
          (let ((kept (jolt-park-winders* w base rtd jolt-winder-in-ref marker)))
            (unless (eq? kept w) (jolt-sa-winders-set! kept))))))))
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
;; sm-parks / chan-parks: the two park counters the gates and benches assert on —
;; a cheap park (java/sm.ss) and a continuation capture (java/fibers-async.ss).
;; They live on the CARRIER, not in a global, because a carrier only ever bumps
;; its own: as one global `set!` from N carrier threads the increments are a
;; read-modify-write with no lock, so they are lost updates waiting to happen, and
;; run-gosm.ss asserts EXACT deltas on them. Per-carrier keeps the bump free (no
;; lock on the park path this round exists to make cheap) and the totals exact.
;; Summed by jolt-sm-parks / jolt-fiber-chan-parks below; a pool reset zeroes
;; them along with the carriers, which is what the benches want.
(define-record-type jolt-carrier
  (fields mu cv (mutable head) (mutable tail)
          (mutable sched-k) sched-slice (mutable thread) (mutable stop?)
          (mutable sm-parks) (mutable chan-parks) (mutable preempts)
          (mutable sic)
          ;; pool: the jolt-pool this carrier belongs to, or #f for the
          ;; default pool. live: how many fibers spawned onto it have not
          ;; finished, counted under mu for a pool carrier only, so a shut-down
          ;; pool's carrier knows when it may stop (see jolt-fiber-pool-make).
          pool (mutable live))
  (nongenerative jolt-carrier-v7))

;; (jolt-fiber-bump-sm-parks! f) / (jolt-fiber-bump-chan-parks! f) — called by the
;; parking fiber, on its own carrier's field, so no two threads touch one field.
(define (jolt-fiber-bump-sm-parks! f)
  (let ((c (jolt-fiber-carrier f)))
    (jolt-carrier-sm-parks-set! c (+ 1 (jolt-carrier-sm-parks c)))))
(define (jolt-fiber-bump-chan-parks! f)
  (let ((c (jolt-fiber-carrier f)))
    (jolt-carrier-chan-parks-set! c (+ 1 (jolt-carrier-chan-parks c)))))
(define (jolt-carrier-total get)
  (let ((v jolt-fiber-carriers))
    (if (not v)
        0
        (let loop ((i 0) (n 0))
          (if (fx=? i (vector-length v)) n (loop (fx+ i 1) (+ n (get (vector-ref v i)))))))))
;; Cheap parks (the CPS'd bodies) and continuation captures (every other park),
;; across the whole pool. Procedures, not variables — the totals are summed.
(define (jolt-sm-parks) (jolt-carrier-total jolt-carrier-sm-parks))
(define (jolt-fiber-chan-parks) (jolt-carrier-total jolt-carrier-chan-parks))

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
;;
;; #f writes jolt-nil, not #f: the var is SEEDED with jolt-nil for "unset"
;; (async.ss), and a reset through here has to leave it reading the way it read
;; before anyone set it. Writing Scheme #f would work for every reader in the
;; runtime — they all test (fixnum? v) — but it makes (nil? *fiber-carrier-count*)
;; answer false for a knob nobody set, so a program cannot ask whether it is set.
(define (jolt-fiber-carrier-count-set! n)
  (when (and n (not (and (fixnum? n) (fx>? n 0))))
    (error 'jolt-fiber-carrier-count-set!
           "carrier count must be a positive fixnum or #f" n))
  (set! jolt-fiber-carrier-count-global n)
  (guard (e (#t #f))
    (let ((cell (var-cell-lookup "clojure.core.async" "*fiber-carrier-count*")))
      (when (and cell (var-cell-defined? cell))
        (var-root-set! cell (or n jolt-nil))))))

;; Build the carrier vector at the current count, exactly once. Double-build
;; is guarded under rr-mu (double-checked): two threads spawning for the first
;; time concurrently must not each build a vector and place fibers on
;; carriers that then get orphaned.
(define (jolt-fiber-ensure-carriers!)
  (unless jolt-fiber-carriers
    (jolt-lock! jolt-fiber-rr-mu)
    (unless jolt-fiber-carriers
      (let* ((n (jolt-fiber-carrier-count))
             (v (make-vector n)))
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (vector-set! v i
            (make-jolt-carrier (make-mutex) (make-condition) #f #f #f
                               (make-jolt-dslice #f #f #f) #f #f 0 0 0 0 #f 0)))
        (set! jolt-fiber-carriers v)))
    (jolt-unlock! jolt-fiber-rr-mu)))

;; Pick the next carrier by round-robin. Runs ONCE at spawn — the fiber never
;; changes carrier (R0(d)). The read-and-advance of jolt-fiber-rr happens
;; under rr-mu, so concurrent spawns from any thread get a strict, predictable
;; rotation (no two spawns reserve the same slot).
;; ONE read of the global, and n measured from THAT vector. Two reads is what this
;; was, and jolt-fiber-pool-reset! landing between them makes the second one #f, so
;; (vector-length #f) raises — or, after a rebuild at a different count, leaves n
;; disagreeing with v and the vector-ref below out of range. Reset documents "call
;; only when the fibers are quiescent", so a program that honours the contract cannot
;; get there; jolt-carrier-total already reads it once, and so does this now.
(define (jolt-fiber-pick!)
  (jolt-fiber-ensure-carriers!)
  (let* ((v jolt-fiber-carriers)
         (n (vector-length v)))
    (jolt-lock! jolt-fiber-rr-mu)
    (let ((c (vector-ref v (mod jolt-fiber-rr n))))
      (set! jolt-fiber-rr (fx+ jolt-fiber-rr 1))
      (jolt-unlock! jolt-fiber-rr-mu)
      c)))

;; The intrusive per-carrier queue. `next` lives in each fiber record.
;; jolt-fiber-enqueue!/locked: mu must already be held. The empty→non-empty
;; transition signals the carrier's condition so a parked carrier wakes (the
;; wake is exactly the R4 design: check and wait hold the same mutex).
;; Both hold a COUNTED lock, which is what keeps preemption out of them. Two
;; reasons it must, and the second is the sharper:
;;
;;   - the empty->non-empty enqueue writes head and then tail, and the dequeue
;;     reads both. A timer firing between the two writes leaves head set with
;;     tail stale, which breaks (head = #f) <=> (tail = #f) and loses fibers.
;;   - these use explicit mutex-acquire / mutex-release and NOT with-mutex, so
;;     there is no dynamic-wind to release the lock. A preemption in the middle
;;     parks the fiber still holding the carrier's queue mutex, and the carrier
;;     that must dequeue it is the one now blocked on that mutex. That is a
;;     deadlock, not a lost update.
;;
;; The scheduler refuses to preempt while the count is non-zero and retries
;; shortly after, so nothing is lost: the preemption lands just past the region.
;;
;; ENQUEUEING A FIBER THAT IS ALREADY QUEUED IS NOT A DUPLICATE, IT IS A CYCLE.
;; If f is the only queued fiber then tail is f, so the first branch writes
;; f.next = f and the queue never drains again — the carrier dispatches the same
;; fiber forever. That is why sa-fiber-resume decides and enqueues under this
;; mutex rather than checking the state first and enqueueing after.
(define (jolt-fiber-enqueue!/locked c f)
  (if (jolt-carrier-tail c)
      (begin (jolt-fiber-next-set! (jolt-carrier-tail c) f)
             (jolt-carrier-tail-set! c f))
      (begin (condition-signal (jolt-carrier-cv c))
             (jolt-carrier-head-set! c f)
             (jolt-carrier-tail-set! c f))))

(define (jolt-fiber-enqueue! c f)
  (begin
    (jolt-lock! (jolt-carrier-mu c))
    (jolt-fiber-enqueue!/locked c f)
    (jolt-unlock! (jolt-carrier-mu c))))

(define (jolt-fiber-dequeue! c)
  (begin
    (jolt-lock! (jolt-carrier-mu c))
    (let ((f (jolt-carrier-head c)))
      (when f
        (jolt-carrier-head-set! c (jolt-fiber-next f))
        (unless (jolt-carrier-head c) (jolt-carrier-tail-set! c #f))
        ;; clear the link so a completed fiber does not retain the queue
        (jolt-fiber-next-set! f #f))
      (jolt-unlock! (jolt-carrier-mu c))
      f)))

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
  ;; The invariant locks.ss states: no counted lock may be held here. Checked at
  ;; the switch and not at the parking sites, because this is where every one of
  ;; them arrives — yield, park, the preemption, the channel waiters, the object
  ;; monitor, the poller, a load that waits — and because the sites that break it
  ;; are the ones nobody counted as parking sites. Before the first mutation, so
  ;; a violation leaves the switch untaken.
  (jolt-locks-assert-none! 'jolt-fiber-to-scheduler!)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  (call/1cc
    (lambda (k)
      (jolt-fiber-k-set! f k)
      (jolt-fiber-slice-save! f)
      ;; The dynamic-wind after-thunks between here and the scheduler are about
      ;; to fire as this continuation unwinds. The ones the back end emitted for
      ;; a `finally` belong to forms the fiber is still inside, so drop them
      ;; from the chain outright. The rest — parameterize, with-mutex — SHOULD
      ;; unwind, and do. The flag stays for the host dynamic-winds that want
      ;; exit-only cleanup but need a before-thunk of their own (loader.ss).
      (jolt-park-drop-finallys!)
      (jolt-park-unwinding-set! #t)
      ;; swish's pcb-sic: remember how deep in disabled-interrupt regions this
      ;; fiber was, so its resume can be put back exactly there. Without it the
      ;; count a parking fiber leaves behind would be inherited by whatever the
      ;; carrier runs next.
      (jolt-fiber-sic-set! f (jolt-current-disable-count))
      ((jolt-carrier-sched-k (jolt-fiber-carrier f))))))

;; (sa-fiber-yield) -> void. Park the current fiber and move it to the back of
;; its carrier's run queue (round-robin); returns when the scheduler resumes
;; it. An error outside a fiber — the vreg read is the "am I on a fiber?"
;; dispatch R0's design calls out.
;; The state change, the enqueue and the switch are ONE transition and must not
;; be preempted partway. A timer landing between the enqueue and the switch runs
;; the handler, which enqueues this same fiber AGAIN — it is then on the run
;; queue twice, gets dispatched twice, and the second dispatch finds it in a
;; state the scheduler says is impossible. That is the "fiber in unexpected
;; state" failure. jolt-fiber-to-scheduler! clears the region as it escapes.
(define (sa-fiber-yield)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-fiber-may-park! 'sa-fiber-yield)   ; before the enqueue commits
               (disable-interrupts)
               (jolt-fiber-state-set! f 'ready)
               (jolt-fiber-enqueue! (jolt-fiber-carrier f) f)
               (jolt-fiber-to-scheduler! f)
               ;; The restart point, and it must BALANCE the disable above —
               ;; swish's @check-and-enable-interrupts. Without it every yield
               ;; leaves the depth one higher than it found it, the stored sic
               ;; grows without bound, and jolt-adjust-interrupts! turns a drain
               ;; quadratic: 10k yields still finished, 200k hung.
               (enable-interrupts)
               (jolt-fiber-check-interrupt! f))
        (error 'sa-fiber-yield "yield called outside a fiber"))))

;; Park WITHOUT re-enqueueing: the fiber is not runnable until sa-fiber-resume.
;; Internal for R1 — this is the park shape R3's channel waiters use (a take!
;; whose callback resumes the fiber) — and what makes sa-fiber-resume real.
;; Same transition rule as sa-fiber-yield: a preemption between marking 'parked
;; and switching would queue a fiber that is about to park.
(define (jolt-fiber-park!)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-fiber-may-park! 'jolt-fiber-park!)  ; before 'parked commits
               (disable-interrupts)
               (when (jolt-fiber-commit-park! f #f)
                 (jolt-fiber-to-scheduler! f))
               (enable-interrupts)           ; balance, see sa-fiber-yield
               (jolt-fiber-check-interrupt! f))
        (error 'jolt-fiber-park! "park called outside a fiber"))))

;; (sa-fiber-resume f) -> void. Make a PARKED fiber runnable again (enqueue on
;; ITS carrier — the resume can come from any thread, and the enqueue lands on
;; the fiber's own carrier, never another's). A no-op when the fiber is
;; already runnable — a double wakeup (a value and a timeout both firing is
;; exactly the R4 alts! commit race) must not corrupt the queue.
;;
;; The test and the transition are ONE step, under the carrier's queue mutex.
;; Written as a check and then an enqueue they are not: two threads that both
;; read 'parked both enqueue, and a second enqueue of a fiber that is already the
;; sole queued one writes f.next = f (see jolt-fiber-enqueue!/locked). That is a
;; queue that never drains, not a fiber dispatched twice.
;;
;; Nothing reachable does that today — a channel wake goes through alt-claim!,
;; which hands a handler to exactly one deliverer, and ldr-end-load! drains its
;; waiter list under ldr-load-mu — but this is an `sa-` seam that reads as a
;; general primitive, and the failure it would produce is a silent hang. The
;; queue mutex is the LAST lock in the order and this path takes nothing else, so
;; holding it across the decision closes no cycle and costs an acquire the
;; enqueue was going to pay anyway.
(define (sa-fiber-resume f)
  (let ((c (jolt-fiber-carrier f)))
    (jolt-lock! (jolt-carrier-mu c))
    (when (eq? (jolt-fiber-state f) 'parked)
      (jolt-fiber-state-set! f 'ready)
      (jolt-fiber-enqueue!/locked c f))
    (jolt-unlock! (jolt-carrier-mu c))))

;; (sa-fiber-spawn thunk) -> fiber. Create a fiber running THUNK, place it
;; round-robin on a carrier, and make it runnable; return the record.
;; Spawning inside a fiber is legal. The child's slice CONVEYS the parent's
;; current dynamic state (the carrier's live values at spawn — reading the
;; params in the parent's context), exactly as async-go-spawn snapshots
;; (dyn-binding-stack) for a thread today; *txn* is always #f so a child
;; spawned inside a dosync cannot join the parent's transaction (ref-sets into
;; the parent's log would be committed by the parent, not the child).
(define (sa-fiber-spawn thunk) (jolt-fiber-spawn* thunk #f #f))

;; F's interrupt box, made the first time it is needed. Only F's own code or a
;; spawn that binds a Thread object before F runs reaches here, so no two threads
;; race to make it.
(define (jolt-fiber-ibox! f)
  (or (jolt-fiber-ibox f)
      (let ((b (box #f))) (jolt-fiber-ibox-set! f b) (jolt-fiber-own-ibox! f b) b)))

;; Make F the owner of interrupt box B (locks.ss jolt-interrupt-owner-set!): an
;; interrupt delivered through the box resumes F from an interruptible channel
;; wait (jolt-fiber-iwait-wake!, installed below as the owners' wake), whoever
;; holds the box.
(define (jolt-fiber-own-ibox! f b) (jolt-interrupt-owner-set! b f))

;; The same, with the fiber's interrupt box given and a hook run on the new fiber
;; BEFORE it can run: a virtual thread started from a Thread object (java/
;; concurrency.ss) binds that object to the fiber there, so the fiber's own
;; Thread/currentThread can never see it unbound.
(define (jolt-fiber-spawn* thunk ibox before-run)
  (jolt-fiber-spawn-on* (jolt-fiber-pick!) thunk ibox before-run))

;; The same, onto carrier c: the default pool's pick or a pool's.
(define (jolt-fiber-spawn-on* c thunk ibox before-run)
  (let ((f (make-jolt-fiber
            'ready thunk #f #f #f #f
            (make-jolt-dslice (jolt-slice-stack-param)
                              (jolt-slice-ns-param)
                              #f)
            c #f '() 0 #f #f 0 #f ibox #f #f #f #f)))
    (when ibox (jolt-fiber-own-ibox! f ibox))
    (when before-run (before-run f))
    ;; listed before it can run, so it can't finish (and be unlisted) first
    (jolt-fiber-live-add! f)
    (jolt-fiber-enqueue! c f)
    f))

;; --- preemption (swish's quantum, erlang.ss:872 and @yield) ------------------
;; Without this a fiber only ever leaves its carrier at a channel op, so a
;; compute-bound go block pins that carrier for as long as it runs and every
;; fiber queued behind it is stranded. Growing the pool does not help — R0(d)
;; pins a fiber to its carrier, so nothing can migrate the stranded work.
;;
;; Swish solves it with a quantum: set-timer at every restart point, and the
;; timer handler forces a yield. Chez polls the engine timer at procedure calls
;; and loop back-edges, so even a tight Scheme loop is preemptible — the same
;; mechanism jolt already uses for thread interrupt (concurrency.ss
;; jolt-run-interruptible).
;;
;; OFF BY DEFAULT, and that is an evidence-based call rather than caution.
;;
;; Cooperative-only really is an unbounded starvation window, so this SHOULD be
;; the only path. It was, briefly. Then a channel stress test (4 producers and a
;; consumer pinned to one carrier) was run with the quantum tightened, and it
;; found two real races: a preemption inside a channel op's critical section
;; (jolt-chan-lock!) and one inside the commit-to-park window
;; (jolt-fiber-waiter-wait!, jolt-sm-commit!). Both are fixed below, and both
;; were quantum-INDEPENDENT — a short quantum only made them near-certain
;; instead of rare.
;;
;; With those fixed the test is clean from ~200 ticks up, and the default here is
;; 1,000,000. But it still stalls at 5 ticks and violates the fiber state machine
;; at 2 ("fiber in unexpected state with irritant running"). By the same argument
;; that made the first two bugs real, those are races too, and a large quantum
;; makes them rare rather than absent. A rare race in a scheduler is the worst
;; kind to ship: it surfaces as an inexplicable hang under load.
;;
;; It was off while preemption could still split a lock region. That is fixed:
;; every lock in the runtime routes through the counting wrapper in locks.ss,
;; and the handler below refuses to switch a fiber that holds one, retrying
;; shortly after instead. The reproduction that used to lose an atom update on
;; every run — four fibers on one carrier, 3000 increments each — is clean at the
;; quantum floor and at quanta twenty times tighter.
;;
;; There is no off switch. Cooperative-only is not a milder setting, it is an
;; unbounded starvation window, and a second scheduling path would get a
;; fraction of the exercise the default one does. Something that wants
;; effectively cooperative behaviour asks for a very long quantum.
;;
;; The quantum is measured, not guessed: ticks are polled at calls and loop
;; back-edges, so on this machine ~100k ticks is 45us and the default below is
;; about 0.45ms — the sub-millisecond range swish uses. A queued fiber therefore
;; waits at most one quantum, and a compute-bound fiber pays one park per
;; quantum, which at this length is noise.
;;
;; Turning it on costs about 13% on a yield (215 -> 243 ns), all of it arming and
;; disarming the timer and independent of the quantum.
;;
;; What preemption does NOT fix: a fiber inside a blocking foreign call cannot
;; be preempted, because the timer is only polled in Scheme. Parkable IO
;; (stdlib/jolt/io_poller.clj) is the answer there, not this.
;; The tick count is CACHED in a plain global, and every hot path reads that and
;; nothing else. This is not premature: the first version consulted the var on
;; each dispatch and wrapped the queue ops unconditionally, which took a yield
;; from 211 ns to 556 ns — a 2.6x tax paid by every program for a feature that
;; is off by default. Reading one global costs a load and a branch, so with
;; preemption off the whole mechanism is invisible.
;;
;; The cost is that setting the VAR directly only takes effect at the next
;; refresh (pool start, or jolt-fiber-preempt-ticks-set!). That is the same
;; contract *fiber-carrier-count* already documents: set it before the first
;; :fiber go, or between a pool reset and the next one. The host setter updates
;; both and is always immediate.
;; A quantum shorter than a dispatch cannot make progress: the timer expires
;; again before the fiber executes anything, so the carrier spends all its time
;; preempting and re-dispatching. Measured: 2 ticks completes the channel stress
;; (slowly, 24k preemptions), 1 tick never finishes. The floor is set well above
;; that boundary rather than at it, since dispatch cost varies by machine, and it
;; is still four orders of magnitude below the default.
;;
;; THE FLOOR IS ALSO THE WHOLE STORY ABOUT TURNING PREEMPTION OFF: there is no
;; value that does it. 0 would be the obvious spelling — (set-timer 0) disarms —
;; and it is refused along with everything else below the floor, because
;; cooperative-only is not a milder setting, it is an unbounded starvation
;; window, and a second scheduling path would get a fraction of the exercise the
;; default one does. Something that wants effectively cooperative behaviour asks
;; for a very long quantum. The var's own documentation (async.ss
;; *fiber-preempt-ticks*) says the same thing, and used to say the opposite.
(define jolt-fiber-preempt-ticks-min 100)
(define jolt-fiber-preempt-ticks-default 1000000)   ; ~0.45ms, measured
(define jolt-fiber-preempt-ticks-global jolt-fiber-preempt-ticks-default)
(define (jolt-fiber-preempt-ticks) jolt-fiber-preempt-ticks-global)
(define (jolt-fiber-preempt-refresh!)
  (let ((v (guard (e (#t #f))
             (let ((cell (var-cell-lookup "clojure.core.async" "*fiber-preempt-ticks*")))
               (if (and cell (var-cell-defined? cell)) (var-cell-root cell) #f)))))
    ;; A fixnum at or above the floor pins the quantum. Anything else — jolt-nil
    ;; (the unset var), a non-fixnum, or a value below the floor including 0 —
    ;; leaves the default alone. Silently, because this runs at pool start where
    ;; there is nobody to report to; the host setter below is the path that
    ;; refuses out loud.
    (when (and (fixnum? v) (fx>=? v jolt-fiber-preempt-ticks-min))
      (set! jolt-fiber-preempt-ticks-global v))))
(define (jolt-fiber-preempt-ticks-set! n)
  ;; #f restores the default quantum rather than turning preemption off. There
  ;; is no "off": cooperative-only is an unbounded starvation window.
  (when (and n (not (and (fixnum? n) (fx>=? n jolt-fiber-preempt-ticks-min))))
    (error 'jolt-fiber-preempt-ticks-set!
           "preempt ticks must be #f or a fixnum >= jolt-fiber-preempt-ticks-min"
           n jolt-fiber-preempt-ticks-min))
  (set! jolt-fiber-preempt-ticks-global (or n jolt-fiber-preempt-ticks-default))
  ;; #f writes jolt-nil for the same reason the carrier-count setter does: the
  ;; var is seeded with jolt-nil and an unset knob has to read as nil however it
  ;; came to be unset, not as false.
  (guard (e (#t #f))
    (let ((cell (var-cell-lookup "clojure.core.async" "*fiber-preempt-ticks*")))
      (when (and cell (var-cell-defined? cell))
        (var-root-set! cell (or n jolt-nil))))))

;; How many times the pool has preempted a fiber. Like the park counters this
;; is per carrier so no two threads touch one field; summed by
;; jolt-fiber-preempts. Used by the gate to prove preemption actually fired
;; rather than the test merely finishing.
(define (jolt-fiber-bump-preempts! f)
  (let ((c (jolt-fiber-carrier f)))
    (jolt-carrier-preempts-set! c (+ 1 (jolt-carrier-preempts c)))))
(define (jolt-fiber-preempts) (jolt-carrier-total jolt-carrier-preempts))

;; The handler. Runs on the carrier thread when its quantum expires.
;;
;; The off-fiber case is not just a guard, it is what makes the park paths safe:
;; jolt-fiber-to-scheduler! and jolt-sm-park! both clear the current-fiber vreg
;; BEFORE they touch the winder chain and escape, so a timer landing anywhere in
;; that window sees no fiber and does nothing. Without it, a preemption inside
;; jolt-park-drop-finallys! would be a park that re-enters the chain
;; read-modify-write and clobbers it with a stale value when the fiber resumes.
;;
;; A preempted fiber always takes the CONTINUATION path, never the sm cheap
;; park: the cheap park stores a pending step, and a timer fires at an arbitrary
;; point where there is no step to store. It also does not bump the sm/chan park
;; counters — run-gosm.ss asserts exact deltas on those, and a preemption is
;; neither kind of park.
;; The retry interval used when a preemption falls due at a moment it cannot be
;; taken. Short, because the regions it waits on are short — measured ~55ns mean
;; — so this lands the preemption just after the region instead of a whole
;; quantum later.
(define (jolt-fiber-preempt-retry-ticks) jolt-fiber-preempt-ticks-min)

(define (jolt-fiber-preempt-handler)
  (let ((f (jolt-current-fiber)))
    (cond
      ;; Off a fiber: mid-park, or between dispatches. A region that must run to
      ;; completion needs no test here — it disabled interrupts, so Chez has not
      ;; delivered this timer yet and will not until the region ends.
      ;; Deliberately does NOT re-arm. The next dispatch arms, and re-arming here
      ;; would leave a timer running over the scheduler itself, where every
      ;; delivery is a no-op that costs a handler call.
      ((not f) (void))
      ;; NOT 'running. resume* sets 'running on every entry, and every transition
      ;; that leaves a fiber in some other state while it is still executing runs
      ;; inside disable-interrupts — so a handler that gets here on a fiber which is
      ;; not 'running is looking at a fiber that has COMMITTED to a transition it has
      ;; not finished, and taking it apart breaks the commit three different ways:
      ;;
      ;;   'parked    the commit landed and the switch has not. This is
      ;;              stdlib/jolt/io_poller.clj wait-fiber, and it is the reason this
      ;;              arm belongs in the scheduler rather than in the seam: the
      ;;              channel ops bracket commit-and-switch in disable-interrupts,
      ;;              and that seam is CLOJURE and cannot. Preempting it queues the
      ;;              fiber, dispatches it, and lets it run on to its own
      ;;              jolt.host/fiber-to-scheduler! — which does not set the state,
      ;;              because the commit did. So it escapes neither queued nor
      ;;              'parked, and sa-fiber-resume (which acts only on 'parked) is a
      ;;              silent no-op for the rest of the process.
      ;;   'ready     the same window, one step later: the wake arrived first, so the
      ;;              fiber is on the run queue AND still running. Enqueueing it
      ;;              again is not a duplicate, it is a cycle — see
      ;;              jolt-fiber-enqueue!/locked, which writes f.next := f when f is
      ;;              the sole entry and leaves the carrier dispatching it forever.
      ;;   'done/'dead  jolt-fiber-finish! has already published the terminal state
      ;;              and is running monitors, which are user code. Resuming past
      ;;              that point means the state is never published again, so the
      ;;              fiber never reads as finished.
      ;;
      ;; Refused rather than dropped, and re-armed short like the lock arm: a seam
      ;; may in principle sit in the gap for a while, and the promise at the head of
      ;; this section is that no setting opens an unbounded starvation window.
      ((not (eq? (jolt-fiber-state f) 'running))
       (jolt-tick-preempt! (jolt-fiber-preempt-retry-ticks)))
      ;; A LOCK IS HELD. Switching here loses mutual exclusion whichever way it
      ;; goes: unwinding releases the mutex mid-section, and not unwinding leaves
      ;; this fiber holding an OS mutex while another fiber on the SAME carrier
      ;; runs — and Chez mutexes are recursive per thread, so that fiber's
      ;; acquire succeeds anyway. So do not switch; come back shortly. Dropping
      ;; the request instead would quietly restore starvation for a fiber that is
      ;; inside a region whenever the timer fires.
      ((fx>? (jolt-locks-held) 0)
       (jolt-tick-preempt! (jolt-fiber-preempt-retry-ticks)))
      ;; The same transition sa-fiber-yield makes, under the same rule: the
      ;; state change, the enqueue and the switch are one step. The timer this
      ;; handler answered has fired, but the tick scheduler may have armed it
      ;; again for another client (a sample, a run-interruptible poll), and a
      ;; fire between the enqueue and the switch would queue this fiber twice.
      (else
       (disable-interrupts)
       (jolt-fiber-bump-preempts! f)
       (jolt-fiber-state-set! f 'ready)
       (jolt-fiber-enqueue! (jolt-fiber-carrier f) f)
       ;; re-armed by jolt-fiber-arm-preempt! when this fiber is next dispatched
       (jolt-fiber-to-scheduler! f)
       (enable-interrupts)           ; balance, see sa-fiber-yield
       ;; Resumed. A compute-bound fiber that was interrupted meets it here, on
       ;; its next quantum, the only safe point it ever reaches.
       (jolt-fiber-check-interrupt! f)))))

;; Arm around a dispatch, disarm on the way back to the drain loop, so the
;; scheduler itself is never preempted. The timer belongs to the tick scheduler
;; (ticks.ss), and the quantum is its preempt demand: on a carrier with nothing
;; else registered that is a bare set-timer, and with a sampler or a
;; run-interruptible borrow registered too it is one demand among them, charged
;; the same ticks. So a borrow no longer hides the quantum and nothing has to
;; hand the timer back afterwards.
;; Installing the HANDLER is per drain, not per dispatch: it is a parameter
;; write, and paying it on every fiber dispatch showed up as a third of the cost
;; of a yield. It has to be installed here at all because a solo carrier arms
;; the raw timer, which never passes through the place ticks.ss installs it.
(define (jolt-fiber-install-preempt-handler!)
  (timer-interrupt-handler jolt-tick-fire))
(define (jolt-fiber-arm-preempt!)
  (jolt-tick-preempt! jolt-fiber-preempt-ticks-global))
(define (jolt-fiber-disarm-preempt!) (jolt-tick-preempt! 0))

;; --- interrupts (an Erlang exit signal: raise in a fiber from outside) -------
;; A fiber otherwise stops only when its body returns or raises. An interrupt
;; asks another fiber to raise a given throwable, the way an Erlang process that
;; receives an exit signal dies wherever it is -- in a receive, in a long
;; computation, parked on a channel.
;;
;; The fiber raises it ITSELF, at points where it is already safe for it to be
;; switched out: the tail of a yield or a park, a park that finds the interrupt
;; pending as it commits, the resume from a preemption, and the entry of a CPS'd
;; body's driver. So a raise never lands inside a runtime critical section -- a
;; preemption is refused while a counted lock is held, and every park asserts
;; none is -- and never between a commit and its switch. A compute-bound fiber
;; raises within one quantum; a parked one at once, since interrupt! wakes it.
;;
;; Waking a parked fiber must not lose a value. A fiber parked on a channel wait
;; is woken by claiming the wait's alt handler -- the one-shot claim an alts!
;; timeout makes -- so no channel can deliver into it afterwards; if a delivery
;; won the claim, it wakes the fiber itself, and the value is the last thing the
;; fiber took. Any other park (a deref, the poller) is woken with sa-fiber-resume;
;; whatever it waited for finds a fiber that is not 'parked when it comes, which
;; sa-fiber-resume ignores.
;;
;; The race a wake could lose -- an interrupt landing just before the fiber
;; commits to a park, so the wake finds it running and the park then waits
;; forever -- is closed by the carrier's run-queue mutex: a park commits under
;; it (jolt-fiber-commit-park!), reading the pending interrupt, and interrupt!
;; reads the fiber's state and wait under it after setting the interrupt. One of
;; the two sees the other. The run-queue mutex is the last lock in the order
;; (async.ss), so taking it inside a channel wait's wmu closes no cycle.
;;
;; The pending field is set under the monitor mutex, the lock jolt-fiber-finish!
;; publishes the terminal state through: an interrupt either reaches a fiber that
;; has not finished, or is refused.

;; Commit fiber F to a park on alt handler H (#f for a park that is not a channel
;; wait), unless an interrupt is pending. #t when committed ('parked); #f when an
;; interrupt is pending, in which case nothing changed and the caller must not
;; park but raise (jolt-fiber-check-interrupt!) once its region is closed.
;; The same commit for a wait a Thread.interrupt of this FIBER may end (<!!, >!!,
;; alts!!). IBOX is the fiber's own interrupt box. 'interrupted when its flag was
;; set: consumed here, and nothing committed. Read under the carrier's run-queue
;; mutex, the lock jolt-fiber-iwait-wake! takes, so an interrupt either set the
;; flag before this read and is seen, or finds the fiber already 'parked and
;; resumes it.
;;
;; HOW THE INTERRUPT FINDS THE FIBER. A fiber is a virtual thread with its own
;; Thread object and flag (java/io.ss), so .interrupt knows the fiber it means and
;; wakes it directly: the park records its handler in the fiber's iwait field, and
;; the wake resumes the fiber only while it is 'parked on that very handler. There
;; is no registry to add to or take out of, so the park writes one field under a
;; mutex it already holds.
(define (jolt-fiber-commit-park/ibox! f h ibox)
  (let ((mu (jolt-carrier-mu (jolt-fiber-carrier f))))
    (jolt-lock! mu)
    (let ((r (cond
               ((and (jolt-fiber-interrupt f) (fx=? 0 (jolt-fiber-mask f))) #f)
               ((unbox ibox) (set-box! ibox #f) 'interrupted)
               (else
                (jolt-fiber-iwait-set! f h)
                (jolt-fiber-parked-on-set! f h)
                (jolt-fiber-state-set! f 'parked)
                #t))))
      (jolt-unlock! mu)
      r)))

;; The interrupt's wake for F (java/concurrency.ss, Thread.interrupt on a virtual
;; thread): resume F if it is parked in an interruptible channel wait. It
;; re-commits, reads its flag and throws. A handler belongs to one op, so a wake
;; that lands after that op ended finds parked-on and iwait disagreeing and does
;; nothing.
(define (jolt-fiber-iwait-wake! f)
  (let* ((c (jolt-fiber-carrier f)) (mu (jolt-carrier-mu c)))
    (jolt-lock! mu)
    (when (and (eq? (jolt-fiber-state f) 'parked)
               (jolt-fiber-iwait f)
               (eq? (jolt-fiber-parked-on f) (jolt-fiber-iwait f)))
      (jolt-fiber-state-set! f 'ready)
      (jolt-fiber-enqueue!/locked c f))
    (jolt-unlock! mu)))

(set! jolt-interrupt-owner-wake jolt-fiber-iwait-wake!)

(define (jolt-fiber-commit-park! f h)
  (let ((mu (jolt-carrier-mu (jolt-fiber-carrier f))))
    (jolt-lock! mu)
    (let ((park? (not (and (jolt-fiber-interrupt f) (fx=? 0 (jolt-fiber-mask f))))))
      (when park?
        (jolt-fiber-parked-on-set! f h)
        (jolt-fiber-state-set! f 'parked))
      (jolt-unlock! mu)
      park?)))

;; For a park committed by a decision taken under some other lock (jolt-lock-wait:
;; a monitor, a condition variable, the load barrier) -- the fiber is 'parked
;; and registered as a waiter, the lock released, the switch not yet made. #t
;; means switch. #f means an interrupt is pending and the park is taken back:
;; the fiber is 'running again, and a later wake of its stale registration finds
;; it not 'parked and does nothing. When a wake already queued it, it must still
;; switch, and raises on its way back.
(define (jolt-fiber-switch-for-park? f)
  (let ((mu (jolt-carrier-mu (jolt-fiber-carrier f))))
    (jolt-lock! mu)
    (let ((take-back? (and (jolt-fiber-interrupt f) (fx=? 0 (jolt-fiber-mask f))
                           (eq? (jolt-fiber-state f) 'parked))))
      (when take-back? (jolt-fiber-state-set! f 'running))
      (jolt-unlock! mu)
      (not take-back?))))

;; (jolt-fiber-interrupt! f throwable) -> #t if F will raise THROWABLE, #f if F
;; had already finished. Callable from any thread or fiber.
;; A kill is an interrupt that leaves by escape: pending, it is this record in
;; the fiber's interrupt field, which every "is an interrupt pending" test reads
;; as true; only jolt-fiber-check-interrupt! tells it apart.
(define-record-type jolt-kill
  (fields throwable)
  (nongenerative jolt-kill-v1))

(define (jolt-fiber-interrupt! f throwable)
  (jolt-fiber-deliver-interrupt! f throwable))

;; Set F's pending interrupt to PAYLOAD -- a throwable, or a jolt-kill -- and
;; wake F if it is parked. A kill already pending is not replaced: it is final.
(define (jolt-fiber-deliver-interrupt! f payload)
  (let ((live? (jolt-with-mutex jolt-fiber-monitor-mu
                 (let ((st (jolt-fiber-state f)))
                   (and (not (eq? st 'done)) (not (eq? st 'dead))
                        (begin
                          (unless (jolt-kill? (jolt-fiber-interrupt f))
                            (jolt-fiber-interrupt-set! f payload))
                          #t))))))
    (when live?
      (let* ((mu (jolt-carrier-mu (jolt-fiber-carrier f)))
             (parked-on (begin (jolt-lock! mu)
                               (let ((r (and (eq? (jolt-fiber-state f) 'parked)
                                             ;; masked: raised when it unmasks, not woken now
                                             (fx=? 0 (jolt-fiber-mask f))
                                             (or (jolt-fiber-parked-on f) 'park))))
                                 (jolt-unlock! mu)
                                 r))))
        (cond
          ((not parked-on) (void))          ; running or queued: it raises at its next safe point
          ((eq? parked-on 'park) (sa-fiber-resume f))
          ((alt-claim! parked-on) (sa-fiber-resume f))
          (else (void)))))                  ; a delivery won the claim and wakes it
    live?))

;; Kill F with THROWABLE, as Erlang's kill: delivered as an interrupt is, but at
;; its safe point F escapes instead of raising, so no try/catch of F's sees it
;; and only its finally blocks (dynamic-wind exits) run. It lands at F's
;; outermost unmasked region, and is raised there, in the masked code around
;; it -- the cleanup the mask exists for -- or, outside any, at F's entry, and F
;; dies with it. A CPS'd go body has no stack to escape and is raised into as an
;; interrupt is. #f when F had already finished.
(define (jolt-fiber-kill! f throwable)
  (jolt-fiber-deliver-interrupt! f (make-jolt-kill throwable)))

;; Raise F's pending interrupt, on F. The raise leaves the interrupt depth where
;; the fiber's own code runs -- the depth its carrier starts every fiber at --
;; which is what the park site whose tail this is would have restored had it
;; returned, since a raise does not unwind the count.
(define (jolt-fiber-check-interrupt! f)
  (let ((e (and f (fx=? 0 (jolt-fiber-mask f)) (jolt-fiber-interrupt f))))
    (when e
      (jolt-fiber-parked-on-set! f #f)
      (jolt-adjust-interrupts! (jolt-current-disable-count)
                               (jolt-carrier-sic (jolt-fiber-carrier f)))
      (if (jolt-kill? e)
          ;; A kill stays pending until it lands, so a finally that reaches a
          ;; safe point on the way out escapes again, to the same place.
          (let ((out (or (jolt-fiber-edge-k f) (jolt-fiber-exit-k f))))
            (if out
                (out e)
                (begin (jolt-fiber-interrupt-set! f #f)
                       (jolt-throw (jolt-kill-throwable e)))))
          (begin (jolt-fiber-interrupt-set! f #f)
                 (jolt-throw e))))))

;; What an escape target does with what reached it: a kill is raised here, where
;; it landed, and is no longer pending; anything else is the value it returned.
(define (jolt-fiber-landed f v)
  (if (jolt-kill? v)
      (begin (when (eq? v (jolt-fiber-interrupt f)) (jolt-fiber-interrupt-set! f #f))
             (jolt-throw (jolt-kill-throwable v)))
      v))

;; --- masking ----------------------------------------------------------------
;; A region that must not be torn by an interrupt -- a process telling its links
;; it died, a cleanup that must finish -- runs masked, as Haskell's mask does: an
;; interrupt arriving then stays pending, and is raised when the region is left.
;; unmasked opens an interruptible region inside a masked one, so the shape
;;
;;   (masked (fn [] (let [r (try (unmasked body) (catch ...))] (cleanup r))))
;;
;; lets body be interrupted and never cleanup. The mask is a per-fiber count,
;; written only by the fiber itself, so it needs no lock; it survives a park, and
;; a masked fiber parked when an interrupt comes is left parked (interrupt!
;; skips the wake), since it would only defer the raise.
;;
;; A park inside either region escapes through its winds and a resume rewinds
;; them, so both keep the count across a park, as the loader's claim does: the
;; before-thunk acts on the first entry only, the after-thunk not while a park
;; unwinds (jolt-park-unwinding?).
(define (jolt-fiber-masked thunk)
  (let ((f (jolt-current-fiber)))
    (if (not f)
        (thunk)
        ;; The wind's own edges are masked against an escape (jolt-wind*,
        ;; locks.ss); the counted-lock mask is dropped before the pending
        ;; interrupt is raised, which would otherwise leave it held.
        (let ((entered #f) (lv (jolt-locks-held)))
          (jolt-locks-enter!)
          (dynamic-wind
            (lambda ()
              (unless entered
                (set! entered #t)
                (jolt-fiber-mask-set! f (fx+ 1 (jolt-fiber-mask f)))))
            (jolt-masked-body thunk)
            (lambda ()
              (unless (jolt-park-unwinding?)
                (jolt-fiber-mask-set! f (fx- (jolt-fiber-mask f) 1))
                (when (fx>? (jolt-locks-held) lv) (jolt-locks-exit!))
                ;; leaving the outermost masked region: a pending interrupt lands
                (when (fx=? 0 (jolt-fiber-mask f))
                  (jolt-fiber-check-interrupt! f)))))))))

;; The outermost unmasked region is where a kill lands: its edge-k is set on
;; entry and cleared on exit (a park's unwind and rewind leave it, as they leave
;; the mask), and the kill is raised once the mask is back, after the region.
(define (jolt-fiber-unmasked thunk)
  (let ((f (jolt-current-fiber)))
    (if (not f)
        (thunk)
        (jolt-fiber-landed f
          (call/cc
            (lambda (out)
              (let ((saved #f) (outermost? #f) (lv (jolt-locks-held)))
                (jolt-locks-enter!)
                (dynamic-wind
                  (lambda ()
                    (unless saved
                      (set! saved (jolt-fiber-mask f))
                      (jolt-fiber-mask-set! f 0)
                      (unless (jolt-fiber-edge-k f)
                        (set! outermost? #t)
                        (jolt-fiber-edge-k-set! f out))))
                  (jolt-masked-body
                    (lambda ()
                      ;; an interrupt that waited for the mask to open lands at once
                      (jolt-fiber-check-interrupt! f)
                      (thunk)))
                  (jolt-masked-after lv
                    (unless (jolt-park-unwinding?)
                      (when outermost? (jolt-fiber-edge-k-set! f #f))
                      (jolt-fiber-mask-set! f saved)))))))))))

;; --- monitors (the observable half of swish's, erlang.ss:434) ----------------
;; A fiber that dies is otherwise unobservable. fibers-async.ss and sm.ss both
;; handle a throwing go body the same way — report it and close the result
;; channel — so a reader gets nil, which is exactly what a body returning nil
;; gives. The condition lands on jolt-fiber-error and nothing can reach it.
;;
;; A monitor is a procedure called exactly once when the fiber finishes, with
;; the condition if it died and #f if it completed normally. Swish delivers this
;; as a DOWN message through a process mailbox; jolt has no mailboxes, so only
;; the shape carries over and the callback is the primitive. The jolt-level
;; channel surface is built on it in fibers-async.ss.
;;
;; The list is written by whoever registers, from any thread, and read by the
;; dying fiber's own carrier, so it is a shared side table and takes the same
;; treatment as the others (jolt-3907): one mutex, held only to swap the list,
;; never while a monitor runs.
(define jolt-fiber-monitor-mu (make-mutex))

;; --- the live fibers (jolt#1606) ---------------------------------------------
;; Every fiber that has been spawned and not finished, whoever spawned it:
;; jolt.fibers/spawn, a pool, io-thread, a :fiber go block, a flow process. They
;; all come through jolt-fiber-spawn-on*, which adds the fiber, and they all end
;; in jolt-fiber-finish!, which drops it under the mutex it already holds to
;; publish the terminal state. So a fiber listed is one whose state was not yet
;; terminal, and the spawn path pays one lock, the same one finish takes.
;;
;; Weak, so the table never keeps a fiber alive. A parked fiber that nothing
;; else references (waiting on a channel nobody holds any more) can never be
;; resumed, and it goes when the collector finds it, as it would without the
;; table.
(define jolt-fiber-live-tbl (make-weak-eq-hashtable))
(define (jolt-fiber-live-add! f)
  (jolt-with-mutex jolt-fiber-monitor-mu
    (eq-hashtable-set! jolt-fiber-live-tbl f #t)))
;; (jolt-fiber-live) -> list of fibers, a snapshot. A fiber in it may finish
;; before the caller looks at it, so state is the caller's to read.
(define (jolt-fiber-live)
  (jolt-with-mutex jolt-fiber-monitor-mu
    (vector->list (hashtable-keys jolt-fiber-live-tbl))))

;; (jolt-fiber-monitor! f proc) -> void. Register PROC on fiber F.
;;
;; The FIBER-level primitive. clojure.core.async's go-monitor does not come
;; through here: it is keyed on the go CHANNEL (async.ss go-chan-monitor!) so it
;; can answer for a thread-backed body too, which this cannot — there is no fiber
;; to register on. Gated directly by fibers-test.ss 7c, both arms.
;;
;; A fiber that has ALREADY finished calls PROC inline rather than dropping it.
;; Without that, monitoring is a race nobody can win: the caller cannot check
;; the state and register atomically from outside, so a fiber that died between
;; the two would never notify and the caller would wait forever. This is the
;; job swish's demonitor&flush does at the other end.
(define (jolt-fiber-monitor! f proc)
  (let ((now
         (jolt-with-mutex jolt-fiber-monitor-mu
           (let ((st (jolt-fiber-state f)))
             (cond
               ;; Already finished: deliver inline, reading the SAME field
               ;; jolt-fiber-finish! writes, under the SAME mutex it publishes
               ;; the state through — so "finished" and "with this outcome"
               ;; are one observation. 'done must not be assumed to mean
               ;; success: a go body that threw is caught inside the fiber
               ;; thunk and reaches 'done with its error field set.
               ((or (eq? st 'done) (eq? st 'dead)) (list (jolt-fiber-error f)))
               (else
                (jolt-fiber-monitors-set! f (cons proc (jolt-fiber-monitors f)))
                #f))))))
    (when now (proc (car now)))))

;; (jolt-fiber-finish! f state err) — publish the fiber's completion and hand its
;; monitors the outcome. Called from jolt-fiber-done! / jolt-fiber-dead!, BEFORE
;; they escape to the scheduler — those never return, so anything after the
;; escape would not run.
;;
;; THE TERMINAL STATE IS SET HERE, under the same mutex jolt-fiber-monitor! reads
;; it through, and that is the point of this function rather than a detail of it.
;; The state is what tells a late registration to deliver inline and the error is
;; what it delivers, so the two have to become visible together. Set outside the
;; lock they were two steps in the wrong order: jolt-fiber-dead! marked the fiber
;; 'dead and only then wrote the condition, so a registration from another
;; thread landing between them read a finished fiber with no error and reported a
;; body that threw as a clean completion — which is the one thing monitoring
;; exists to make visible. Every other field a monitor or a waiting gate reads
;; (result, error) is written by the caller BEFORE it gets here, for the same
;; reason: the state publishes them.
;;
;; err is the condition for a fiber that DIED, or #f to keep whatever the body
;; already recorded — jolt-fiber-go-spawn catches a throwing body itself and
;; writes the field before it returns, so a fiber can reach 'done with an error
;; and the monitor has to see it.
;;
;; The list is taken and cleared under the mutex so a monitor cannot fire twice,
;; and the monitors themselves run OUTSIDE it: they are user code, they run on
;; the dying fiber's carrier, and holding a lock across them would let one
;; monitor deadlock every other fiber's registration. A monitor that raises is
;; contained so it cannot stop the remaining ones or corrupt the completion —
;; the fiber is already finishing and there is nowhere left to report to.
(define (jolt-fiber-finish! f state err)
  (let ((ms (jolt-with-mutex jolt-fiber-monitor-mu
              (when err (jolt-fiber-error-set! f err))
              (jolt-fiber-state-set! f state)
              (eq-hashtable-delete! jolt-fiber-live-tbl f)
              (let ((ms (jolt-fiber-monitors f)))
                (jolt-fiber-monitors-set! f '())
                ms))))
    ;; Read AFTER the lock, which is safe for the same reason it was safe to
    ;; write inside it: the fiber is terminal now, this is the only thread that
    ;; ever writes the field, and a jolt-fiber-monitor! that got in first took
    ;; the list with it. So this is the value that call saw, or the one it would
    ;; have seen.
    (let ((out (jolt-fiber-error f)))
      (let loop ((ms ms))
        (unless (null? ms)
          (guard (e (#t #f)) ((car ms) out))
          (loop (cdr ms)))))))

;; --- the scheduler ----------------------------------------------------------
;; Resume (or first-run) fiber f on ITS carrier's thread, returning to the
;; loop when f parks, finishes, or dies.
(define (jolt-fiber-run f)
  (let* ((c (jolt-fiber-carrier f))
         (base (jolt-carrier-sic c)))
    (call/1cc
      (lambda (k)
        (jolt-carrier-sched-k-set! c k)
        ;; The chain sched-k was captured with: the boundary a park unwinds down
        ;; to, and therefore the point the finally walk can stop at.
        (set-virtual-register! jolt-vreg-fiber-winder-base (jolt-sa-winders))
        ;; scheduler -> fiber: restore the incoming fiber's slice BEFORE running
        ;; it (for a resume, before its continuation re-enters — the dynamic-wind
        ;; before-thunks then re-fire over the restored values)
        (jolt-fiber-slice-restore! (jolt-fiber-slice f))
        ;; Put the carrier back to the interrupt depth this fiber parked at. A
        ;; first run starts at the carrier's own baseline.
        ;;
        ;; ORDER MATTERS FROM HERE. The adjust ENABLES whenever it is walking the
        ;; count down (the carrier sits deeper than the fiber parked), and an
        ;; enable that reaches 0 delivers whatever the previous fiber deferred.
        ;; So the current-fiber register is published only after the depth is
        ;; settled: a timer arriving during the adjust then finds no fiber and
        ;; does nothing, instead of preempting a fiber that has not started. That
        ;; case does not merely lose a quantum — the handler would park through a
        ;; continuation captured mid-dispatch, and re-entering jolt-fiber-resume*
        ;; would invoke an already-consumed one-shot continuation.
        ;; Reading the count is not itself a delivery point (see
        ;; jolt-current-disable-count); the adjust is, which is why the order is
        ;; measure, adjust, THEN publish rather than anything else.
        (jolt-adjust-interrupts! (jolt-current-disable-count)
                                 (if (jolt-fiber-k f) (jolt-fiber-sic f) base))
        (set-virtual-register! jolt-vreg-current-fiber f)
        ;; Armed LAST, so the only poll between arming and entering the body is
        ;; the call to jolt-fiber-resume* itself. One poll cannot exhaust a
        ;; quantum, because jolt-fiber-preempt-ticks-min is far above 1.
        (jolt-fiber-arm-preempt!)
        (jolt-fiber-resume* f)))
    ;; Disarm FIRST: everything below runs on the scheduler, which must never be
    ;; preempted (a timer here would park the carrier's own loop).
    (jolt-fiber-disarm-preempt!)
    ;; However the fiber left the interrupt depth — parked inside a region, or
    ;; simply finished — the scheduler resumes at its own baseline.
    (jolt-adjust-interrupts! (jolt-current-disable-count) base)
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
;; re-applying the thunk would re-run it from scratch (an infinite loop). A
;; 'ready fiber with NO k enters through its THUNK. That is a first run for an
;; ordinary fiber, and for a CPS'd body (java/sm.ss) it is also every resume: a
;; cheap park leaves k clear and stashes the pending step in the sm field, so
;; the thunk is a re-entrant driver that runs the step — which is what puts this
;; guard back around the body on each resume. 'parked fibers are never dequeued:
;; sa-fiber-resume moves them to 'ready before enqueue.
(define (jolt-fiber-resume* f)
  (case (jolt-fiber-state f)
    ((ready)
     ;; 'running on every entry, not just the first. A resume used to leave it
     ;; 'ready, so 'ready meant both "on the run queue" and "executing" — the
     ;; state machine at the top of this file and alt-deliver!'s race argument
     ;; (async.ss) both name 'running for the second. Nothing reads it: the
     ;; deliver race turns on the commit under the handler's wmu, and
     ;; sa-fiber-resume acts only on 'parked. The comments should still be true.
     (jolt-fiber-state-set! f 'running)
     (cond
       ((jolt-fiber-k f) ((jolt-fiber-k f)))
       ;; An sm RESUME (a pending step, not #f/'running — a procedure, or an sm-wait
       ;; record for an interruptible <!! / >!! park): the thunk is java/sm.ss's
       ;; driver, which installs its own handler and escapes to the scheduler
       ;; itself. Guarding it here would be redundant AND wrong — this handler
       ;; marks the fiber dead without closing the body's go channel, so a reader
       ;; of that channel would wait forever. Skipping it also keeps the guard's
       ;; call/cc off the resume path the cheap park exists to make cheap.
       ((let ((sm (jolt-fiber-sm f))) (and sm (not (eq? sm 'running))))
        ((jolt-fiber-thunk f))
        ;; Unreachable: jolt-sm-drive parks, finishes, or dies, and each of those
        ;; escapes to the scheduler through the carrier's sched-k. But it is the
        ;; ONE arm here that does not end in an escape or a jolt-fiber-done!, so
        ;; if the driver ever did return, the fiber would sit in 'running with
        ;; nothing left to run it, its go channel never closed, and every reader
        ;; of that channel waiting forever — with no error naming any of it. Kill
        ;; the fiber instead, which closes nothing but at least records why.
        (jolt-fiber-dead! f
          (condition (make-error)
                     (make-message-condition
                      "jolt-fiber-resume*: the sm driver returned without completing the fiber"))))
       (else
        ;; WHY THERE IS NO STACK TRUNCATION HERE.
        ;;
        ;; This fiber's continuation is rooted in the drain iteration that
        ;; happened to dispatch it, so a parked fiber retains those frames and a
        ;; LATER iteration resumes it by reinstating them. It is correct only
        ;; because jolt-fiber-done! and jolt-fiber-dead! ESCAPE through the
        ;; carrier's sched-k instead of returning: the stale frames are returned
        ;; through, but never returned INTO. Nothing enforces that, so anything
        ;; added below which completes a fiber by returning rather than escaping
        ;; would resume a drain iteration that already finished.
        ;;
        ;; Swish makes this structural by cutting the stack at process entry
        ;; (erlang.ss @thunk->cont, #%$current-stack-link #%$null-continuation).
        ;; That was tried here and does not port: with the cut, every fiber
        ;; shape — including one that merely returns a value — dies with
        ;; "attempt to invoke shot one-shot continuation". Swish can do it
        ;; because it builds a process's continuation ONCE at spawn in a frame
        ;; made for it, and its scheduler resumes a stored continuation rather
        ;; than calling a thunk inside its own dispatch frame, which is what
        ;; happens here. Matching that shape means capturing each fiber's
        ;; continuation eagerly at spawn, which gives up the property R0 chose
        ;; deliberately and this file's header states: a fiber costs a stack
        ;; segment from the moment it PARKS, not from the moment it is created.
        ;;
        ;; So the escape discipline above is the invariant. Keep it.
        (let ((r (guard (e (#t (jolt-fiber-dead! f e)))
                    ;; the entry is where a kill outside any unmasked region
                    ;; lands, raised inside this guard so the fiber dies of it
                    (jolt-fiber-landed f
                      (call/cc
                        (lambda (exit)
                          (jolt-fiber-exit-k-set! f exit)
                          ;; interrupted before it ever ran: it dies on entry
                          (jolt-fiber-check-interrupt! f)
                          ((jolt-fiber-thunk f))))))))
          (jolt-fiber-done! f r)))))
    (else (error 'jolt-fiber-run "fiber in unexpected state"
                 (jolt-fiber-state f)))))

;; Completion paths: settle the fiber's payload, PUBLISH the terminal state
;; (jolt-fiber-finish!, which also runs the monitors), clear the current-fiber vreg
;; (the scheduler owns the CPU now — a stale vreg would make a later yield from a
;; non-fiber context enqueue a dead fiber and invoke the consumed sched-k), drop the
;; consumed continuation and the dynamic slice, then hand control back to the
;; scheduler — again via the fiber's OWN carrier, the only one that can be running it.
;;
;; WHAT MUST HAPPEN BEFORE WHAT, and it is not one rule but two (jolt-kdq7).
;;
;; The PAYLOAD — result and error — is written before the state, because the state is
;; what publishes it: a monitor registering concurrently, and every gate that waits on
;; a fiber by polling its state and then reading its result, both read the payload
;; through the state.
;;
;; The SCHEDULER'S OWN FIELDS — k, sm, slice — are cleared AFTER it, and that is the
;; opposite rule for the opposite reason. They publish nothing: only jolt-fiber-run and
;; resume* read them, on this carrier's own thread. What they are is state a park needs,
;; so clearing them while the fiber can still be parked is a trap. It was one: cleared
;; first, the fiber sat 'running with no slice, and a timer landing in that window
;; parked it and made jolt-fiber-slice-save! write into #f. The raise is inside the
;; handler and this function is called outside resume*'s guard, so it took the CARRIER
;; thread with it and stranded every fiber pinned to it — a wedged program with no
;; error pointing anywhere near the fiber that completed.
;;
;; So the clears sit where nothing can park the fiber, and TWO independent things put
;; them there rather than one. The vreg goes to 0 first, so a timer arriving from here
;; on finds no fiber at all and the handler's off-fiber arm does nothing. And the state
;; is already terminal, so even with the vreg set the handler refuses (jolt-9d3m).
;; Belt and braces on purpose: this failure does not look like its cause.
;;
;; The remaining window — after result-set!, before finish! — is preemptible, and that
;; is fine and not an oversight: the slice is still live there, so the park saves and
;; the resume restores, and the fiber comes back and finishes. Nothing in the payload
;; write is order-sensitive against being re-entered.
;;
;; The carrier field is deliberately NOT cleared: placement is fixed at spawn (R0(d))
;; and the gates assert on it after completion.
(define (jolt-fiber-done! f r)
  (jolt-fiber-result-set! f r)
  ;; #f and not a condition: jolt-fiber-go-spawn catches a throwing body inside
  ;; the fiber thunk (it has to, so it can report and close the go channel), so
  ;; that fiber finishes NORMALLY and only its error field says otherwise. #f
  ;; means "keep what the body recorded", which is that condition, or nothing for
  ;; a fiber that genuinely succeeded.
  (jolt-fiber-finish! f 'done #f)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  (jolt-fiber-k-set! f #f)
  (jolt-fiber-sm-set! f #f)
  (jolt-fiber-slice-set! f #f)
  ((jolt-carrier-sched-k (jolt-fiber-carrier f))))

(define (jolt-fiber-dead! f e)
  (jolt-fiber-finish! f 'dead e)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  (jolt-fiber-k-set! f #f)
  (jolt-fiber-sm-set! f #f)
  (jolt-fiber-slice-set! f #f)
  ((jolt-carrier-sched-k (jolt-fiber-carrier f))))

;; (jolt-fiber-drain! c) -> void. Run carrier c's run queue until it drains —
;; the scheduler shape the plan names ("run ready fibers until the queue
;; drains, then block in the poller"); the poll step is R8's. A fiber that
;; parks (jolt-fiber-park!) stops the drain until resumed. The scheduler's
;; slice is captured at entry — the caller's dynamic state, which every park
;; restores onto the carrier.
(define (jolt-fiber-drain! c)
  (jolt-fiber-install-preempt-handler!)
  ;; The carrier's own interrupt depth, measured once per drain. Every fiber is
  ;; put back to this before the scheduler runs again, and a first run starts
  ;; here. Measured rather than assumed 0, because a drain can be entered from
  ;; user code (sa-fiber-run-all) that is itself inside a disabled region.
  (jolt-carrier-sic-set! c (jolt-current-disable-count))
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
    (jolt-lock! (jolt-carrier-mu c))
    (cond
      ((jolt-carrier-stop? c) (jolt-unlock! (jolt-carrier-mu c)) (void))
      ((jolt-carrier-head c) (jolt-unlock! (jolt-carrier-mu c)) (loop))
      (else (jolt-condition-wait (jolt-carrier-cv c) (jolt-carrier-mu c))
            (jolt-unlock! (jolt-carrier-mu c))
            (loop)))))

;; Start the pool exactly once, on the first :fiber go spawn. Double-start is
;; guarded by its own mutex (the started? flag); each carrier thread inherits
;; the spawner's thread parameters at fork, which is irrelevant here — every
;; drain re-captures the scheduler slice at entry.
(define (jolt-fiber-ensure-carrier!)
  (jolt-lock! jolt-fiber-pool-mu)
  (unless jolt-fiber-pool-started?
    (set! jolt-fiber-pool-started? #t)
    (jolt-fiber-preempt-refresh!)
    (jolt-fiber-ensure-carriers!)
    (let ((v jolt-fiber-carriers) (n (vector-length jolt-fiber-carriers)))
      (do ((i 0 (fx+ i 1))) ((fx=? i n))
        (let ((c (vector-ref v i)))
          (jolt-carrier-thread-set! c
            ;; daemon, as the JVM's go dispatch threads are
            (fork-thread/daemon #t (lambda () (rdr-default-modes!) (jolt-fiber-carrier-loop c))))))))
  (jolt-unlock! jolt-fiber-pool-mu))

;; (jolt-fiber-pool-reset!) -> void. Stop every carrier thread (each finishes
;; its queue, then exits on the stop flag), join them, and drop the pool so
;; the next spawn rebuilds it — with a possibly different count (set via
;; jolt-fiber-carrier-count-set! between a reset and the next pool start).
;; Test/embedding API: a program pins the count for determinism; resetting is
;; how the count changes. Call only when the fibers are quiescent — a carrier
;; mid-fiber finishes that fiber before exiting (the join waits), but a
;; PARKED fiber is abandoned: no thread is left to resume it.
(define (jolt-fiber-pool-reset!)
  (jolt-lock! jolt-fiber-pool-mu)
  (let ((v jolt-fiber-carriers))
    (when v
      (let ((n (vector-length v)))
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (let ((c (vector-ref v i)))
            (jolt-lock! (jolt-carrier-mu c))
            (jolt-carrier-stop?-set! c #t)
            (condition-broadcast (jolt-carrier-cv c))
            (jolt-unlock! (jolt-carrier-mu c))))
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (let ((t (jolt-carrier-thread (vector-ref v i))))
            (when t (thread-join t)))))))
  (jolt-lock! jolt-fiber-rr-mu)
  (set! jolt-fiber-carriers #f)
  (set! jolt-fiber-rr 0)
  (jolt-unlock! jolt-fiber-rr-mu)
  (set! jolt-fiber-pool-started? #f)
  (jolt-unlock! jolt-fiber-pool-mu))

;; --- pools besides the default one --------------------------------------------
;; A pool is a set of carriers of its own, made explicitly and sized when made,
;; so fibers that may pin a carrier (a blocking foreign call) or hog one can be
;; kept off the carriers that everything else runs on. The default pool above
;; is untouched by this: no field it reads changed, and its hot paths do not
;; know pools exist.
;;
;; Nothing else needs to know either. Placement is still once, at spawn, onto
;; one of the pool's carriers by round-robin, and from then on every park,
;; resume, interrupt and kill goes through the fiber's own carrier record, as
;; it does in the default pool. So a fiber in one pool parks on a channel that
;; a fiber in another puts to, and is woken onto its own carrier.
;;
;; Shutting a pool down refuses new spawns at once, but each carrier keeps
;; running until every fiber placed on it has finished, parked ones included,
;; and only then lets its thread end. The default pool's reset abandons parked
;; fibers instead; that is a test knob, and this is not. The count of fibers
;; still to finish (the carrier's live field) is kept under the carrier's mu,
;; raised at spawn and lowered by a monitor on each fiber, so the default pool
;; pays nothing for it.
;;
;; Lock order: pool mu, then carrier mu, which keeps the run-queue mu last.
(define-record-type jolt-pool
  (fields name (mutable carriers) (mutable rr) mu (mutable stopped?))
  (nongenerative jolt-pool-v1))

;; A pool carrier's loop: the default carrier loop, except a stop only ends it
;; once nothing placed on it is left to run.
(define (jolt-fiber-pool-carrier-loop c)
  (let loop ()
    (jolt-fiber-drain! c)
    (jolt-lock! (jolt-carrier-mu c))
    (cond
      ((jolt-carrier-head c) (jolt-unlock! (jolt-carrier-mu c)) (loop))
      ((and (jolt-carrier-stop? c) (fx=? 0 (jolt-carrier-live c)))
       (jolt-unlock! (jolt-carrier-mu c)) (void))
      (else (jolt-condition-wait (jolt-carrier-cv c) (jolt-carrier-mu c))
            (jolt-unlock! (jolt-carrier-mu c))
            (loop)))))

;; (jolt-fiber-pool-make name n) -> pool. n carriers, their threads started now.
;; Reads *fiber-preempt-ticks* as the default pool's start does, since a pool
;; made first would otherwise run on the default quantum whatever it says.
(define (jolt-fiber-pool-make name n)
  (jolt-fiber-preempt-refresh!)
  (let ((p (make-jolt-pool name #f 0 (make-mutex) #f))
        (v (make-vector n)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n))
      (vector-set! v i
        (make-jolt-carrier (make-mutex) (make-condition) #f #f #f
                           (make-jolt-dslice #f #f #f) #f #f 0 0 0 0 p 0)))
    (jolt-pool-carriers-set! p v)
    (do ((i 0 (fx+ i 1))) ((fx=? i n))
      (let ((c (vector-ref v i)))
        (jolt-carrier-thread-set! c
          (fork-thread/daemon #t (lambda () (rdr-default-modes!) (jolt-fiber-pool-carrier-loop c))))))
    p))

(define (jolt-fiber-pool-size p) (vector-length (jolt-pool-carriers p)))

;; The pool fiber f runs in, or #f for the default pool.
(define (jolt-fiber-pool-of f) (jolt-carrier-pool (jolt-fiber-carrier f)))

;; Reserve a carrier of p for one more fiber, or #f if p is shut down. The
;; reservation is made under p's mu, so a shutdown cannot let the carrier stop
;; between the pick and the fiber's arrival on its queue.
(define (jolt-fiber-pool-reserve! p)
  (jolt-lock! (jolt-pool-mu p))
  (if (jolt-pool-stopped? p)
      (begin (jolt-unlock! (jolt-pool-mu p)) #f)
      (let* ((v (jolt-pool-carriers p))
             (c (vector-ref v (mod (jolt-pool-rr p) (vector-length v)))))
        (jolt-pool-rr-set! p (fx+ (jolt-pool-rr p) 1))
        (jolt-lock! (jolt-carrier-mu c))
        (jolt-carrier-live-set! c (fx+ (jolt-carrier-live c) 1))
        (jolt-unlock! (jolt-carrier-mu c))
        (jolt-unlock! (jolt-pool-mu p))
        c)))

;; A pool fiber has finished: one fewer for its carrier, and a stopped carrier
;; with none left is woken to end.
(define (jolt-fiber-pool-release! c)
  (jolt-lock! (jolt-carrier-mu c))
  (jolt-carrier-live-set! c (fx- (jolt-carrier-live c) 1))
  (when (jolt-carrier-stop? c) (condition-signal (jolt-carrier-cv c)))
  (jolt-unlock! (jolt-carrier-mu c)))

;; (jolt-fiber-pool-spawn p thunk) -> fiber, or #f if p is shut down.
(define (jolt-fiber-pool-spawn p thunk)
  (let ((c (jolt-fiber-pool-reserve! p)))
    (and c
         (jolt-fiber-spawn-on* c thunk #f
           (lambda (f) (jolt-fiber-monitor! f (lambda (err) (jolt-fiber-pool-release! c))))))))

;; (jolt-fiber-pool-shutdown! p) -> void. No new spawns; each carrier ends once
;; the fibers already on it have finished. Does not wait for them.
(define (jolt-fiber-pool-shutdown! p)
  (jolt-lock! (jolt-pool-mu p))
  (unless (jolt-pool-stopped? p)
    (jolt-pool-stopped?-set! p #t)
    (vector-for-each
      (lambda (c)
        (jolt-lock! (jolt-carrier-mu c))
        (jolt-carrier-stop?-set! c #t)
        (condition-broadcast (jolt-carrier-cv c))
        (jolt-unlock! (jolt-carrier-mu c)))
      (jolt-pool-carriers p)))
  (jolt-unlock! (jolt-pool-mu p)))
