;; test/chez/fibers-test.ss — R1 gate for the fiber primitive + single-carrier
;; scheduler (epic jolt-nvpr.2). Run: chez --script test/chez/fibers-test.ss
;; (wired in as `make fibers`; detection-free, no jolt boot needed).
;;
;; Correctness: spawn/yield/resume round trip; a fiber that finishes; a fiber
;; that raises with the scheduler surviving and the other fibers still running;
;; N fibers round-robin in order; a fiber yielding from 40 frames down.
;;
;; And two properties that only show up with a second thread involved, both
;; deterministic rather than sampled (sections 7b and 7c): concurrent resumes of
;; one parked fiber put it on the run queue exactly once, and a fiber's
;; completion is published under the monitor mutex so its state and its error
;; can never be observed apart.
;;
;; Numbers as assertions with generous ceilings (pinned by R0's findings):
;;   spawn  < 5us     (measured 0.84us)
;;   switch: depth-independent (40 frames within 3x of 1) — the property,
;;            not the speed; two absolute forms flaked on CI, see below
;;   per-fiber live < 8KB (measured ~3.5KB)
;; Memory is measured the R0-corrected way: retain the fibers in a global,
;; force (collect (collect-maximum-generation)), read ABSOLUTE live bytes.
;; Per-phase bytes-allocated deltas produced both of R0's wrong findings and
;; are never used here. The switch bench raises the GC trip threshold for the
;; timed region so segment churn (3.5KB per park) does not pollute the number.

(import (chezscheme))
(load "host/chez/locks.ss")   ; fibers.ss locks through it; jolt-with-mutex is a macro
(load "host/chez/ticks.ss")   ; fibers.ss arms the timer through it
(load "host/chez/fibers.ss")
;; R5 (jolt-nvpr.6): fibers now live on a POOL of N carriers (N defaults to
;; the processor count). This gate drives the scheduler SYNCHRONOUSLY with
;; sa-fiber-run-all on one carrier, so it pins the pool to exactly one — the
;; documented "pin it to 1 for determinism" use of the count knob.
(jolt-fiber-carrier-count-set! 1)

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(define (mono-nanos)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000000000 (time-second t)) (time-nanosecond t))))

(define (all-done? fs)
  (or (null? fs) (and (eq? (jolt-fiber-state (car fs)) 'done) (all-done? (cdr fs)))))
(define (all? pred ls)
  (or (null? ls) (and (pred (car ls)) (all? pred (cdr ls)))))
;; spawn N fibers via (mk i) for i = 0..N-1, IN THAT ORDER, returning them in
;; order (an explicit loop, so the enqueue order is unambiguous).
(define (spawn-n n mk)
  (let loop ((i 0) (acc '()))
    (if (fx<? i n)
        (loop (fx+ i 1) (cons (mk i) acc))
        (reverse acc))))

(printf "== correctness ==\n")

;; 1. spawn/yield/resume round trip: 'a before the yield, 'b after it
(define rt-log '())
(define rt-f
  (sa-fiber-spawn
    (lambda ()
      (set! rt-log (cons 'a rt-log))
      (sa-fiber-yield)
      (set! rt-log (cons 'b rt-log))
      'rt-done)))
(sa-fiber-run-all)
(ok "round trip: body order" (equal? rt-log '(b a)))
(ok "round trip: fiber done" (eq? (jolt-fiber-state rt-f) 'done))
(ok "round trip: result captured" (eq? (jolt-fiber-result rt-f) 'rt-done))

;; 2. a fiber that finishes without ever yielding
(define done-f (sa-fiber-spawn (lambda () 42)))
(sa-fiber-run-all)
(ok "finish: state done" (eq? (jolt-fiber-state done-f) 'done))
(ok "finish: result" (eq? (jolt-fiber-result done-f) 42))

;; 3. a fiber that raises: the scheduler survives, the others still run, and
;; the error is recorded on the dead fiber
(define raise-log '())
(define raise-fibers
  (spawn-n 6
    (lambda (i)
      (sa-fiber-spawn
        (lambda ()
          (when (fx=? i 2)
            (raise (condition (make-message-condition "boom"))))
          (set! raise-log (cons i raise-log)))))))
(sa-fiber-run-all)
(ok "raise: did not propagate to the caller" #t)
(ok "raise: non-raising fibers ran" (= (length raise-log) 5))
(ok "raise: raising fiber is dead"
    (eq? (jolt-fiber-state (list-ref raise-fibers 2)) 'dead))
(ok "raise: condition recorded"
    (string=? "boom"
              (condition-message (jolt-fiber-error (list-ref raise-fibers 2)))))
(define after-f (sa-fiber-spawn (lambda () 'ok)))
(sa-fiber-run-all)
(ok "raise: scheduler reusable after a dead fiber"
    (eq? (jolt-fiber-result after-f) 'ok))

;; 4. a raise AFTER a park: the resume-path guard must ride the continuation
(define parked-raise-f
  (sa-fiber-spawn (lambda () (sa-fiber-yield) (error 'fiber "late boom"))))
(sa-fiber-run-all)
(ok "raise after park: fiber dead" (eq? (jolt-fiber-state parked-raise-f) 'dead))

;; 5. N fibers round-robin in strict order
(define N 8)
(define M 6)
(define rr-log '())
(define rr-fibers
  (spawn-n N
    (lambda (i)
      (sa-fiber-spawn
        (lambda ()
          (let loop ((m M))
            (when (fx>? m 0)
              (set! rr-log (append rr-log (list i)))
              (sa-fiber-yield)
              (loop (fx- m 1)))))))))
(sa-fiber-run-all)
(ok "round-robin: strict rotation"
    (let loop ((k 0))
      (or (fx>=? k (fx* N M))
          (and (fx=? (list-ref rr-log k) (mod k N)) (loop (fx+ k 1))))))
(ok "round-robin: all fibers done" (all-done? rr-fibers))

;; 6. a fiber yielding from 40 frames down resumes correctly
(define (deep-yield n)
  (if (fx=? n 0) (sa-fiber-yield) (deep-yield (fx- n 1))))
(define deep-finished #f)
(define deep-f
  (sa-fiber-spawn (lambda () (deep-yield 40) (set! deep-finished #t) 'deep-ok)))
(sa-fiber-run-all)
(ok "deep yield: resumed correctly" deep-finished)
(ok "deep yield: result" (eq? (jolt-fiber-result deep-f) 'deep-ok))

;; 7. sa-fiber-resume: a fiber parked without self-enqueue wakes on resume; a
;; double wakeup is a no-op (state 'ready -> skip, the queue stays intact)
(define wake-f
  (sa-fiber-spawn (lambda () (jolt-fiber-park!) 'woke)))
(sa-fiber-run-all)
(ok "resume: parked after the drain" (eq? (jolt-fiber-state wake-f) 'parked))
(sa-fiber-resume wake-f)
(ok "resume: runnable after resume" (eq? (jolt-fiber-state wake-f) 'ready))
(sa-fiber-resume wake-f)
(ok "resume: double wakeup is a no-op" (eq? (jolt-fiber-state wake-f) 'ready))
(sa-fiber-run-all)
(ok "resume: completed" (eq? (jolt-fiber-result wake-f) 'woke))

;; 7b. the SEQUENTIAL double wakeup above is the easy half. The one that matters
;; is concurrent: two threads that both read 'parked before either writes 'ready
;; both enqueue, and a second enqueue of a fiber that is already the sole queued
;; one writes f.next = f. That is not a fiber dispatched twice, it is a run queue
;; that never drains — the carrier walks the same fiber forever and nothing says
;; why. So sa-fiber-resume decides and enqueues under the carrier's queue mutex,
;; in one step.
;;
;; The walk below is bounded rather than trusting to terminate, because the bug
;; is precisely a cycle.
;;
;; The resumers are STARTED ONCE and released on a spin barrier, not forked per
;; round. Forking per round does not reproduce anything: fork-thread costs
;; microseconds and the window is a couple of instructions, so the first thread
;; is long finished before the second exists, and the mutation below passes
;; clean. Pre-started threads spinning on a generation counter do overlap.
;;
;; Mutation-checked in that shape: putting the test back outside the lock does
;; not fail these two checks, it kills the gate — the self-linked queue hands the
;; same fiber back after it completed and the drain dies with "fiber in
;; unexpected state with irritant done". Which is the point: unfixed, the symptom
;; is a scheduler that reports something impossible, with nothing to connect it
;; to a resume that happened two dispatches earlier.
(define (rq-length c)
  (let loop ((f (jolt-carrier-head c)) (n 0))
    (cond ((not f) n)
          ((fx>? n 8) n)                      ; a cycle — report it, do not spin
          (else (loop (jolt-fiber-next f) (fx+ n 1))))))

(define RACE-N 4)
(define RACE-ROUNDS 400)
(define race-c (vector-ref jolt-fiber-carriers 0))
(define race-gen (box 0))
(define race-target (box #f))
(define race-done (box 0))
(define race-done-mu (make-mutex))
(define race-stop (box #f))

;; THE GENERATION A WORKER CARRIES IS THE ONE IT ACTED ON, never a fresh read.
;; This used to end the round with (spin (unbox race-gen)), and that loses rounds.
;; Main cannot finish round g until every worker has reported it, so the last
;; report releases main straight into round g+1 and the bump that opens it — and a
;; worker still between its own report and that re-read then reads g+1 and stores
;; it as `seen`. Round g+1 is invisible to that worker, race-done never reaches
;; RACE-N, and the await below spins forever with RACE-N threads spinning beside
;; it. Seen as a CI wedge: 85 minutes on this gate at 499% CPU with no output and
;; nothing to end it. Reproduced deterministically by widening that exact window.
(define race-threads
  (let loop ((i 0) (acc '()))
    (if (fx=? i RACE-N)
        acc
        (loop (fx+ i 1)
              (cons (fork-thread
                     (lambda ()
                       (let spin ((seen 0))
                         (cond
                           ((unbox race-stop) (void))
                           ((fx>? (unbox race-gen) seen)
                            ;; pairs with main's release before the bump: the
                            ;; target and the zeroed count are read after it
                            (memory-order-acquire)
                            (let ((g (unbox race-gen)))   ; the round being served
                              (let ((f (unbox race-target)))
                                (when f (sa-fiber-resume f)))
                              (jolt-with-mutex race-done-mu
                                (set-box! race-done (fx+ 1 (unbox race-done))))
                              (spin g)))
                           (else (spin seen))))))
                    acc)))))

(define race-bad 0)
(define race-woke 0)
(define race-stuck #f)
;; BOUNDED, because a gate that hangs is worse than one that fails: a hang says
;; nothing about what broke and burns the CI slot until somebody notices. The
;; bound is enormous relative to RACE-N resumes — reaching it means the barrier
;; lost a worker, which is a bug in the barrier or in sa-fiber-resume, and either
;; way it should be reported and not spun on.
(define RACE-AWAIT-LIMIT 2000000000)
(do ((r 0 (fx+ r 1))) ((or race-stuck (fx=? r RACE-ROUNDS)))
  (let ((f (sa-fiber-spawn (lambda () (jolt-fiber-park!) 'woke))))
    (sa-fiber-run-all)                        ; runs it up to the park
    (set-box! race-target f)
    (set-box! race-done 0)
    ;; THE BUMP PUBLISHES THE TWO STORES ABOVE, so it goes after a release fence.
    ;; x86 keeps stores in order and never needed it; arm64 does not, and there a
    ;; worker could see the new generation and still read the target or the count
    ;; from the round before. A stale count is a report lost or doubled, so
    ;; race-done never lands on RACE-N and the await runs out: the Android
    ;; (bionic) shard's occasional "every round released every resumer" failure,
    ;; and 1 run in 5 on an M1 Pro with RACE-ROUNDS at 200000.
    (memory-order-release)
    (set-box! race-gen (fx+ 1 (unbox race-gen)))   ; release all RACE-N at once
    (let await ((n 0))
      (cond ((fx=? RACE-N (unbox race-done)) (void))
            ((fx>? n RACE-AWAIT-LIMIT) (set! race-stuck #t))
            (else (await (fx+ n 1)))))
    (memory-order-acquire)                    ; the workers' resumes, before rq-length
    (unless (fx=? 1 (rq-length race-c)) (set! race-bad (fx+ race-bad 1)))
    (sa-fiber-run-all)
    (if (eq? 'done (jolt-fiber-state f))
        (set! race-woke (fx+ race-woke 1))
        (set! race-bad (fx+ race-bad 1)))))
(ok "concurrent resume: every round released every resumer" (not race-stuck))
(set-box! race-stop #t)
(set-box! race-gen (fx+ 1 (unbox race-gen)))     ; one last release so they exit
(for-each thread-join race-threads)
(ok "concurrent resume: exactly one queue entry, every round" (fx=? 0 race-bad))
(ok "concurrent resume: every fiber ran once and finished" (fx=? RACE-ROUNDS race-woke))

;; 7c. a fiber's completion is PUBLISHED UNDER THE MONITOR MUTEX.
;;
;; jolt-fiber-monitor! reads the state and, when it is already terminal,
;; delivers the error field inline without looking further. So the two have to
;; become visible together. They were two unlocked writes in the wrong order —
;; state first, error second — and a registration landing between them read a
;; finished fiber with no error and reported a body that threw as a clean
;; completion, which is the one thing monitoring exists to make visible.
;;
;; Racing it is hopeless: the window is two instructions and a stress that spawns
;; and monitors 400 dying go blocks never lands in it, even with the gap widened
;; to a millisecond (tried; fibers-monitor-test section 7 says so). So do not
;; race it — HOLD THE LOCK. If publication really happens under it, a fiber that
;; dies while this thread owns the mutex cannot become terminal at all; the
;; carrier blocks until the lock is free and then publishes both fields at once.
;; That is an assertion about the mechanism rather than a sampling of it, and it
;; is deterministic.
;;
;; Mutation-checked: moving the state write back outside the mutex fails the
;; first of these three on every run, and would fail it just as surely if the
;; gap were a single instruction.
(jolt-lock! jolt-fiber-monitor-mu)
(define pub-f (sa-fiber-spawn (lambda () (error 'fiber "pub boom"))))
(define pub-th (fork-thread (lambda () (sa-fiber-run-all))))
;; let the carrier thread get all the way to the publication point and block
(let spin ((n 0)) (when (fx<? n 4000000) (spin (fx+ n 1))))
(ok "publication: not terminal while the monitor lock is held"
    (not (memq (jolt-fiber-state pub-f) '(done dead))))
;; Registered from inside the held region, which is the QUEUEING arm of
;; jolt-fiber-monitor! and deterministic here: the fiber provably cannot be
;; terminal yet, because the check above just said so and the carrier is blocked
;; on this very mutex. Chez mutexes are recursive per thread, so the registration
;; takes the lock this thread already owns. Covers the fiber-level primitive
;; directly — clojure.core.async's go-monitor is keyed on the go CHANNEL now
;; (async.ss go-chan-monitor!, so it can answer for a thread-backed body too) and
;; no longer reaches this one.
(define pub-seen (box 'unset))
(jolt-fiber-monitor! pub-f (lambda (err) (set-box! pub-seen err)))
(ok "publication: a monitor registered before the finish has not fired yet"
    (eq? 'unset (unbox pub-seen)))
(jolt-unlock! jolt-fiber-monitor-mu)
(thread-join pub-th)
(ok "publication: terminal once the lock is released" (eq? 'dead (jolt-fiber-state pub-f)))
(ok "publication: and the error arrived with the state"
    (condition? (jolt-fiber-error pub-f)))
(ok "publication: the queued monitor fired with that same condition"
    (eq? (unbox pub-seen) (jolt-fiber-error pub-f)))
;; The already-finished arm: registering on a terminal fiber delivers inline.
(define pub-late (box 'unset))
(jolt-fiber-monitor! pub-f (lambda (err) (set-box! pub-late err)))
(ok "publication: a monitor registered after the finish delivers inline"
    (eq? (unbox pub-late) (jolt-fiber-error pub-f)))

;; 8. yield outside a fiber is a clean error (the vreg is the dispatcher)
(ok "yield outside a fiber raises"
    (guard (e (#t (string=? (condition-message e) "yield called outside a fiber")))
      (sa-fiber-yield)
      #f))

;; 9. nested spawn: spawning a fiber from inside a fiber is legal
(define inner-result #f)
(define outer-f
  (sa-fiber-spawn
    (lambda ()
      (define inner (sa-fiber-spawn (lambda () 'inner-done)))
      (sa-fiber-yield)
      (set! inner-result (jolt-fiber-result inner)))))
(sa-fiber-run-all)
(ok "nested spawn: inner fiber done" (eq? inner-result 'inner-done))
(ok "nested spawn: outer fiber done" (eq? (jolt-fiber-state outer-f) 'done))

(printf "\n== numbers (generous ceilings) ==\n")

;; --- spawn: per-spawn < 5us (measured 0.2-0.84us) ----------------------------
;; An absolute ceiling, kept absolute on purpose: spawn is record + enqueue, and
;; no control op in this process shares its cost (the switch section below
;; records two calibrations that failed for exactly that reason). What made the
;; old form flaky was one 200000-spawn window decided by whether it was
;; descheduled. Now the cost is the fastest of 5 batches of 20000 spawns (~4ms
;; each), each from a full collection with the previous batch drained, and a
;; miss is measured once more. Load only ever slows a batch, so the minimum is
;; the batch nearest spawn's own cost, while a spawn that really costs 5us is
;; that slow in every batch.
(define SPAWN-N 20000)
(define (spawn-batch-us)
  (collect (collect-maximum-generation))
  (let* ((t0 (mono-nanos))
         (fs (let loop ((i 0) (acc '()))
               (if (fx<? i SPAWN-N)
                   (loop (fx+ i 1) (cons (sa-fiber-spawn (lambda () #f)) acc))
                   acc)))
         (us (/ (exact->inexact (- (mono-nanos) t0)) 1000.0 SPAWN-N)))
    ;; the spawned-but-never-run fibers complete immediately; drain the queue
    (sa-fiber-run-all)
    (ok "spawn: batch drained" (all-done? fs))
    us))
(define (spawn-best-us)
  (let loop ((i 0) (best #f))
    (if (fx=? i 5) best (loop (fx+ i 1) (let ((us (spawn-batch-us))) (if best (min best us) us))))))
(define spawn-us (let ((us (spawn-best-us))) (if (< us 5.0) us (spawn-best-us))))
(printf "  spawn:  ~a us/fiber (assert < 5us) -- record+enqueue only; the body\n" spawn-us)
(printf "          has not run, so the continuation capture and about 3.5\n")
(printf "          KB segment is NOT in this number (first park pays those)\n")
(ok "spawn < 5us" (< spawn-us 5.0))

;; --- switch: depth-independence (the property, not the speed) -----------------
;; N fibers round-robin, each yielding M times: every park costs one switch out
;; and one switch in, so switches = 2*N*M. The trip threshold is raised for the
;; timed region so the ~3.5KB per captured segment does not trigger GC mid-run.
;; Warm up the machinery first, then settle the nursery with a full collect.
(define SW-N 32)
(define SW-M 500)
(define (sw-yield-at-depth d)                ; yield d frames down
  (if (fx=? d 0) (sa-fiber-yield) (begin (sw-yield-at-depth (fx- d 1)) (void))))
(define (sw-bench-n) (sw-bench-depth 0))
(define (sw-bench-depth depth)
  (define fibers
    (spawn-n SW-N
      (lambda (i)
        (sa-fiber-spawn
          (lambda ()
            (let loop ((m SW-M))
              (when (fx>? m 0)
                (sw-yield-at-depth depth)
                (loop (fx- m 1)))))))))
  (sa-fiber-run-all)
  (all-done? fibers))
(ok "switch: warmup run" (sw-bench-n))
(ok "switch: warmup run (40 frames)" (sw-bench-depth 40))

;; What is asserted here is the PROPERTY, not the speed: a continuation capture
;; on Chez is O(1), so yielding 40 frames down must cost the same as yielding 1
;; frame down. Both halves are measured in this same process on this same
;; machine, so the comparison is immune to how fast or busy the runner is.
;;
;; Two absolute forms were tried and both were flakes. A 100ns ceiling passed
;; locally at 53ns and failed CI at 124ns. A ratio against a bare-procedure-call
;; calibration then failed CI at 73x against a 60x ceiling — because a tight loop
;; calling a trivial procedure is cache-resident and does NOT slow down on a
;; shared runner (2.14ns on CI vs 2.39ns locally) while continuation capture
;; allocates and touches memory and does (157ns vs 53ns). Wrong proxy: the
;; calibration has to share the workload's characteristics, and nothing simple
;; does. Depth-independence needs no proxy.
;;
;; This still catches the regression that matters — a capture that starts copying
;; the stack becomes O(depth) and the ratio explodes. The absolutes are printed
;; for the record and asserted only against a ceiling loose enough to be
;; meaningless as noise (a 20x blowup, not a 2x one).
;;
;; The two depths are timed alternately, five rounds, the order flipping every
;; round, and each keeps its fastest run (~4ms a run, from a full collection).
;; Interference from other work on the machine only ever makes a run slower, so
;; the minimum is the run closest to the switch's own cost, and alternating
;; spreads a burst over both arms instead of handing it to whichever ran then.
;; One descheduled run used to decide the ratio (8.7x once under a loaded full
;; gate, 1.2x when rerun). A miss is measured once more: an O(depth) capture
;; reads high on both.
(define old-trip (collect-trip-bytes))
(collect-trip-bytes 100000000000)          ; no gen-0 during the timed region
(define (sw-run-ns depth label)
  (collect (collect-maximum-generation))
  (let ((t0 (mono-nanos)))
    (ok label (sw-bench-depth depth))
    (/ (exact->inexact (- (mono-nanos) t0)) (* 2.0 SW-N SW-M))))
(define (sw-measure)            ; -> (ns at 1 frame . ns at 40 frames)
  (let loop ((i 0) (s #f) (d #f))
    (if (fx=? i 5)
        (cons s d)
        (let* ((pair (if (even? i)
                         (let* ((a (sw-run-ns 0 "switch: timed run (1 frame)"))
                                (b (sw-run-ns 40 "switch: timed run (40 frames)")))
                           (cons a b))
                         (let* ((b (sw-run-ns 40 "switch: timed run (40 frames)"))
                                (a (sw-run-ns 0 "switch: timed run (1 frame)")))
                           (cons a b)))))
          (loop (fx+ i 1)
                (if s (min s (car pair)) (car pair))
                (if d (min d (cdr pair)) (cdr pair)))))))
(define (sw-ratio m) (/ (cdr m) (max 1.0 (car m))))
(define sw-m (let ((m (sw-measure)))
               (if (and (< (sw-ratio m) 3.0) (< (car m) 5000.0)) m (sw-measure))))
(define switch-ns (car sw-m))
(define deep-switch-ns (cdr sw-m))
(collect-trip-bytes old-trip)
(define depth-ratio (/ deep-switch-ns (max 1.0 switch-ns)))
(printf "  switch: ~a ns at 1 frame, ~a ns at 40 frames (depth ratio ~a)\n"
        switch-ns deep-switch-ns depth-ratio)
(ok "capture is depth-independent (40 frames within 3x of 1)" (< depth-ratio 3.0))
(ok "switch not catastrophically slow (< 5us)" (< switch-ns 5000.0))

;; --- per-fiber memory: < 8KB, absolute-live-bytes method ----------------------
;; R0's corrected measurement: retain the parked fibers in a global, force a
;; maximum-generation collect, and read ABSOLUTE live bytes before and after.
;; Per-phase bytes-allocated deltas (which produced both of R0's wrong
;; findings) are never used.
(define MEM-N 20000)
(define mem-held '())
(define mem-baseline (begin (collect (collect-maximum-generation)) (bytes-allocated)))
(define (make-mem-fibers)
  (let loop ((i 0) (acc '()))
    (if (fx<? i MEM-N)
        (loop (fx+ i 1)
              (cons (sa-fiber-spawn (lambda () (jolt-fiber-park!) 'x)) acc))
        acc)))
(set! mem-held (make-mem-fibers))
(sa-fiber-run-all)     ; every fiber runs once and parks (no self-enqueue)
(collect (collect-maximum-generation))
(define per-fiber-bytes
  (exact->inexact (/ (max 0 (- (bytes-allocated) mem-baseline)) MEM-N)))
(ok "memory: all parked" (all? (lambda (f) (eq? (jolt-fiber-state f) 'parked)) mem-held))
(printf "  per-fiber: ~a bytes live (assert < 8192)\n" per-fiber-bytes)
(ok "per-fiber live < 8KB" (< per-fiber-bytes 8192))

;; --- what this gate does NOT cover, asserted so it cannot be misread ---------
;; This file loads fibers.ss ON ITS OWN, with no adapter and no value layer, to
;; keep that file honestly self-contained. The consequence is easy to forget:
;; every adapter-backed feature in it is captured by guarded reference and
;; DEGRADES TO A NO-OP here. So a pass from this gate says nothing whatever
;; about the winder-drop path — jolt-park-drop-finallys! does not do anything in
;; this process, and could be arbitrarily broken without a single check moving.
;;
;; That is not hypothetical. A stack-truncation attempt (jolt-atc.3) was wrong
;; enough to kill every fiber under the full boot and this gate passed clean,
;; because the primitive it needed was absent and the call degraded to nothing.
;;
;; So the degradation is asserted rather than assumed. Two things follow: the
;; no-op path stays deliberately exercised (a target without these primitives
;; must still run fibers), and if fibers.ss ever gains a HARD dependency on the
;; adapter these checks fail loudly instead of the gate quietly covering less
;; than it appears to.
;;
;; The real coverage for this path is fibers-state-test.ss section 7, which
;; loads rt.ss and asserts the behaviour (a finally skipped on a park, a
;; with-mutex released, a parameterize unwound). Mutation-checked: disabling the
;; predicate there fails 4 of its checks.
(ok "degradation: no marker without the value layer" (not jolt-finally-marker))
(ok "degradation: no winder rtd without the adapter" (not jolt-winder-rtd))
(ok "degradation: the chain reader answers empty" (null? (jolt-sa-winders)))
(ok "degradation: dropping finallys is inert here, and does not raise"
    (begin (jolt-park-drop-finallys!) #t))
;; A fiber still has to run and park with the whole mechanism absent — that is
;; the portability claim the fallbacks exist to make.
(define degr-f (sa-fiber-spawn (lambda () (sa-fiber-yield) 'ok)))
(sa-fiber-run-all)
(ok "degradation: a fiber still completes with no winder support"
    (eq? 'done (jolt-fiber-state degr-f)))

(printf "\nfibers-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "fibers-test: PASS — fiber primitive + scheduler\n") (exit 0))
    (exit 1))
