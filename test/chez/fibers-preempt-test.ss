;; Fibers: preemptive scheduling (jolt-atc.2, swish's quantum — erlang.ss:872).
;;
;; Cooperatively scheduled fibers only leave their carrier at a channel op, so a
;; compute-bound go block pins that carrier and starves everything queued behind
;; it. R0(d) pins a fiber to its carrier for life, so growing the pool cannot
;; rescue the stranded work — the only fix is to take the carrier away.
;;
;; The gate pins the pool to ONE carrier so starvation is deterministic: fiber A
;; spins without ever yielding, fiber B is queued behind it. Cooperatively B can
;; never run. With a quantum it does.
;;
;; Loads the full runtime (rt.ss) because the preempt knob is read off a jolt
;; var and the handler needs the real carrier pool.
(import (chezscheme))
(load "host/chez/rt.ss")

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
(define (now-secs) (/ (exact->inexact (mono-nanos)) 1000000000.0))

(define (wait-until pred secs what)
  (let ((deadline (+ (now-secs) secs)))
    (let loop ()
      (cond ((pred) #t)
            ((> (now-secs) deadline)
             (set! fails (+ fails 1))
             (printf "  FAIL: ~a (timed out)\n" what)
             #f)
            (else (sleep (make-time 'time-duration 2000000 0)) (loop))))))

(printf "== fiber preemption ==\n")

;; --- 1. off by default ------------------------------------------------------
;; ON, and the only scheduling path. There is no off switch: cooperative-only is
;; an unbounded starvation window, and a second path would get a fraction of the
;; exercise the default one does.
(ok "1. preemption is on, at the default quantum"
    (eqv? (jolt-fiber-preempt-ticks) jolt-fiber-preempt-ticks-default))
(jolt-fiber-preempt-ticks-set! 20000)
(ok "1. host setter pins a quantum" (eqv? 20000 (jolt-fiber-preempt-ticks)))
(jolt-fiber-preempt-ticks-set! #f)
(ok "1. #f restores the default rather than disabling"
    (eqv? (jolt-fiber-preempt-ticks) jolt-fiber-preempt-ticks-default))
;; A quantum below the floor cannot make progress, so it is refused rather than
;; allowed to livelock the carrier.
(ok "1. a quantum below the floor is refused"
    (guard (e (#t #t))
      (jolt-fiber-preempt-ticks-set! (fx- jolt-fiber-preempt-ticks-min 1)) #f))
(ok "1. the floor itself is accepted"
    (guard (e (#t #f))
      (jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min) #t))

;; --- 1b. an unset knob reads NIL, not false (jolt-dpye) ----------------------
;; Both vars are seeded with jolt-nil for "unset", and both host setters take #f
;; to mean "unset it again". #f used to go straight into the root, which every
;; reader in the runtime survives — they all test (fixnum? v) — but which makes
;; (nil? *fiber-preempt-ticks*) answer false for a knob nobody set, so a program
;; cannot ask whether it is set. Asserted on the ROOT, since that is where the
;; two spellings differ.
(define (knob-root name)
  (let ((cell (var-cell-lookup "clojure.core.async" name)))
    (and cell (var-cell-defined? cell) (var-cell-root cell))))
(jolt-fiber-preempt-ticks-set! 20000)
(ok "1b. a pinned quantum reaches the var" (eqv? 20000 (knob-root "*fiber-preempt-ticks*")))
(jolt-fiber-preempt-ticks-set! #f)
(ok "1b. a #f reset leaves the quantum var nil"
    (jolt-nil? (knob-root "*fiber-preempt-ticks*")))
(jolt-fiber-carrier-count-set! 2)
(ok "1b. a pinned carrier count reaches the var" (eqv? 2 (knob-root "*fiber-carrier-count*")))
(jolt-fiber-carrier-count-set! #f)
(ok "1b. a #f reset leaves the carrier-count var nil"
    (jolt-nil? (knob-root "*fiber-carrier-count*")))

;; --- 2. cooperatively, a spinner starves its carrier ------------------------
;; ONE carrier, so B has nowhere else to go. A spins on a box the driver flips;
;; the flag is how the test stops it without ever letting it park.
;; Section 2 wants the cooperative failure mode on purpose.
(jolt-fiber-preempt-ticks-set! 10000000000)
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)

(define (spin-until stop-box) (let loop () (unless (unbox stop-box) (loop))))

(define coop-stop (box #f))
(define coop-b-ran (box #f))
(define coop-a
  (sa-fiber-spawn (lambda () (spin-until coop-stop))))
(define coop-b
  (sa-fiber-spawn (lambda () (set-box! coop-b-ran #t))))
(jolt-fiber-ensure-carrier!)
(sleep (make-time 'time-duration 300000000 0))       ; 0.3s
(ok "2. cooperatively the queued fiber is starved" (not (unbox coop-b-ran)))
(set-box! coop-stop #t)
(wait-until (lambda () (unbox coop-b-ran)) 5.0 "2. B runs once A finishes")
(ok "2. B ran after A released the carrier" (unbox coop-b-ran))

;; --- 3. with a quantum the same fiber does NOT starve ------------------------
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! 20000)

(define pre-stop (box #f))
(define pre-b-ran (box #f))
(define preempts-before (jolt-fiber-preempts))
(define pre-a
  (sa-fiber-spawn (lambda () (spin-until pre-stop))))
(define pre-b
  (sa-fiber-spawn (lambda () (set-box! pre-b-ran #t))))
(jolt-fiber-ensure-carrier!)
(define b-ran-while-a-spins?
  (wait-until (lambda () (unbox pre-b-ran)) 5.0
              "3. queued fiber runs while the spinner still holds the carrier"))
(ok "3. the spinner was actually preempted"
    (> (jolt-fiber-preempts) preempts-before))
(ok "3. the queued fiber ran WITHOUT the spinner ever yielding"
    (and b-ran-while-a-spins? (not (unbox pre-stop))))
(set-box! pre-stop #t)
(wait-until (lambda () (eq? 'done (jolt-fiber-state pre-a))) 5.0 "3. spinner completes")
(ok "3. the preempted fiber still completed normally"
    (eq? 'done (jolt-fiber-state pre-a)))

;; --- 4. a preemption is not counted as a park -------------------------------
;; run-gosm.ss asserts EXACT deltas on the two park counters, so a preemption
;; must not land in either. It takes the continuation path, which is neither an
;; sm cheap park nor a channel park.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! 20000)
(define sm-before (jolt-sm-parks))
(define chan-before (jolt-fiber-chan-parks))
(define p4-before (jolt-fiber-preempts))
(define p4-stop (box #f))
(define p4-f (sa-fiber-spawn (lambda () (spin-until p4-stop))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (> (jolt-fiber-preempts) p4-before)) 5.0 "4. a preemption fired")
(set-box! p4-stop #t)
(wait-until (lambda () (eq? 'done (jolt-fiber-state p4-f))) 5.0 "4. fiber completes")
(ok "4. preemption did not bump the sm-park counter"
    (= sm-before (jolt-sm-parks)))
(ok "4. preemption did not bump the chan-park counter"
    (= chan-before (jolt-fiber-chan-parks)))

;; --- 5. a long quantum starves again ----------------------------------------
;; The knob is read per dispatch rather than latched at pool start.
(jolt-fiber-preempt-ticks-set! 10000000000)
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(define off-stop (box #f))
(define off-b-ran (box #f))
(define off-a (sa-fiber-spawn (lambda () (spin-until off-stop))))
(define off-b (sa-fiber-spawn (lambda () (set-box! off-b-ran #t))))
(jolt-fiber-ensure-carrier!)
(sleep (make-time 'time-duration 300000000 0))
(ok "5. with a long quantum the queued fiber starves again" (not (unbox off-b-ran)))
(set-box! off-stop #t)
(wait-until (lambda () (unbox off-b-ran)) 5.0 "5. B runs after A finishes")

;; --- 6. channel ops are not preempted mid-critical-section ------------------
;; The regression test for the two races preemption introduced, both found by
;; running this with the quantum tightened until it broke.
;;
;; ONE carrier on purpose. Fibers on a carrier share an OS thread and Chez
;; mutexes are recursive per thread, so a fiber suspended holding a channel mutex
;; does NOT announce itself as a deadlock — the next fiber's acquire succeeds and
;; walks into a section another fiber is halfway through. Before the guards, this
;; stalled at 802 of 1600 values; before the commit-window guard, at 0 of 1600.
;;
;; Runs at the FLOOR, the tightest quantum the scheduler accepts, which is four
;; orders of magnitude below the default. Anything looser stops exercising the
;; window at all: at the default a channel-bound fiber parks long before its
;; quantum expires and no preemption ever lands inside an op.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min)

(define NPROD 4)
(define NPER 300)
(define st-ch (jolt-async-chan 4))
(define st-seen (make-eqv-hashtable))
(define st-got 0)
(define st-before (jolt-fiber-preempts))

;; Each producer BURNS a quantum between puts. Without that the test proves
;; nothing: a purely channel-bound fiber parks long before its quantum expires,
;; so no preemption ever fires and the run is trivially clean. The spin is what
;; puts a preemption near, and sometimes inside, a channel op.
(define (st-burn n) (let loop ((i 0) (acc 0)) (if (fx=? i n) acc (loop (fx+ i 1) (fx+ acc i)))))
(do ((i 0 (+ i 1))) ((= i NPROD))
  (let ((id i))
    (sa-fiber-spawn
     (lambda ()
       (let loop ((j 0))
         (when (< j NPER)
           (st-burn 400)
           (jolt-fiber->! st-ch (+ (* id 100000) j))
           (loop (+ j 1))))))))
(sa-fiber-spawn
 (lambda ()
   (let loop ()
     (let ((v (jolt-fiber-<! st-ch)))
       (unless (jolt-nil? v)
         (hashtable-set! st-seen v (+ 1 (hashtable-ref st-seen v 0)))
         (set! st-got (+ st-got 1))
         (loop))))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (>= st-got (* NPROD NPER))) 60.0
            "6. every value delivered under a tight quantum")
(ok "6. no value lost" (= st-got (* NPROD NPER)))
(ok "6. no value duplicated" (= (hashtable-size st-seen) (* NPROD NPER)))
(ok "6. preemption actually fired during the stress"
    (> (jolt-fiber-preempts) st-before))
;; --- 7. a long synchronous drain stays linear -------------------------------
;; The interrupt depth a park records has to be BALANCED when the fiber resumes.
;; It was not, at first: every yield left the depth one higher than it found it,
;; so the stored count grew without bound and the restore loop turned a drain
;; quadratic. 10,000 yields still finished and 200,000 hung, which is exactly the
;; shape of bug the other sections cannot see — they all use short bodies.
(jolt-fiber-preempt-ticks-set! #f)
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(define drain-f
  (sa-fiber-spawn
   (lambda () (let loop ((i 0)) (when (< i 200000) (sa-fiber-yield) (loop (+ i 1)))))))
(define drain-t0 (now-secs))
(jolt-fiber-drain! (jolt-fiber-carrier drain-f))
(ok "7. 200k yields complete on a synchronous drain"
    (eq? 'done (jolt-fiber-state drain-f)))
;; Linear, not quadratic. The quadratic version did not finish in minutes, so any
;; sane bound catches it; 30s leaves room for a slow machine.
(ok "7. and in linear time" (< (- (now-secs) drain-t0) 30.0))

;; --- 9. preemption never lands inside a lock --------------------------------
;; The bug this gate exists for. An OS mutex has THREAD granularity and fibers
;; multiplex one thread, so a fiber suspended holding one loses exclusion either
;; way: unwinding releases it mid-section, and not unwinding lets the next fiber
;; on the SAME carrier acquire it anyway, because Chez mutexes are recursive per
;; thread. Measured before the gate existed: (swap! a inc) through the
;; with-mutex'd compare-and-set lost an update, 11999 of 12000, on every run.
;;
;; ONE carrier, so nothing else can paper over it, and each fiber burns a quantum
;; between updates so preemptions actually fall due mid-region rather than while
;; the fiber is parked.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min)

(define LK-N 4)
(define LK-PER 2000)
(define lk-atom (jolt-atom-new 0))
(define lk-obj (jolt-atom-new 'monitored))
(define lk-mon 0)
(define lk-done 0)
(define lk-before (jolt-fiber-preempts))
(define (lk-burn n) (let loop ((i 0) (a 0)) (if (fx=? i n) a (loop (fx+ i 1) (fx+ a i)))))
(define (lk-inc x) (+ x 1))

(do ((i 0 (+ i 1))) ((= i LK-N))
  (sa-fiber-spawn
   (lambda ()
     (let loop ((j 0))
       (if (< j LK-PER)
           (begin
             (lk-burn 200)
             (jolt-swap! lk-atom lk-inc)            ; with-mutex'd CAS
             ;; a monitor too: jolt's user-facing `locking`, the class a
             ;; with-mutex wrapper alone would miss
             (jolt-with-monitor lk-obj (lambda () (set! lk-mon (+ lk-mon 1))))
             (loop (+ j 1)))
           (set! lk-done (+ lk-done 1)))))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (= lk-done LK-N)) 90.0 "9. every fiber finished")
(ok "9. no atom update lost under preemption"
    (= (jolt-atom-val lk-atom) (* LK-N LK-PER)))
(ok "9. no monitored update lost under preemption"
    (= lk-mon (* LK-N LK-PER)))
(ok "9. preemption actually fired during the run"
    (> (jolt-fiber-preempts) lk-before))
(ok "9. no lock left held on the carrier" (= 0 (jolt-locks-held)))

;; --- 10. reading the interrupt depth is a QUERY, not a delivery point --------
;; The scheduler asks for the current disable-interrupts depth on every switch,
;; to carry it across the park (swish's pcb-sic). It used to DERIVE the answer:
;; both primitives return the new count, so a disable/enable pair tells you what
;; you started at. But that pair is not a read. An enable that brings the count
;; back to 0 delivers whatever was deferred while interrupts were off, and every
;; caller of this is on a park path that has already dropped its finally winders
;; and committed to leaving — the worst place in the runtime to run an arbitrary
;; handler. It now reads the thread-context field instead, as swish does.
;;
;; The re-entrancy check below is the one that distinguishes the two. A Chez
;; timer handler runs at disable-count 0 (probed, not assumed), so a handler that
;; re-arms and then asks for the depth is exactly the shape where the old pair
;; delivers: the disable defers the new tick and the enable hands it straight
;; back, re-entering the handler. Mutation-verified by restoring the pair.
(jolt-fiber-preempt-ticks-set! #f)
(jolt-fiber-pool-reset!)

(define (derived-depth) (let ((n (disable-interrupts))) (enable-interrupts) (fx- n 1)))

(ok "10. depth reads 0 with interrupts on" (= 0 (jolt-current-disable-count)))
(ok "10. agrees with the derivation at every depth"
    (let loop ((k 0) (agree #t))
      (if (= k 4)
          (begin (do ((i 0 (+ i 1))) ((= i 4)) (enable-interrupts)) agree)
          (begin (disable-interrupts)
                 (loop (+ k 1) (and agree (= (jolt-current-disable-count)
                                             (derived-depth)
                                             (+ k 1))))))))
(ok "10. reading does not disturb the depth"
    (let ((before (jolt-current-disable-count)))
      (do ((i 0 (+ i 1))) ((= i 1000)) (jolt-current-disable-count))
      (and (= before (jolt-current-disable-count)) (= 0 before))))

;; Re-entrancy: arm a fresh tick from inside the handler, THEN read the depth.
(define re-depth 0)
(define re-entered #f)
(define re-in #f)
(define re-saved (timer-interrupt-handler))
(timer-interrupt-handler
 (lambda ()
   (when re-in (set! re-entered #t))
   (set! re-in #t)
   (set! re-depth (+ re-depth 1))
   (when (< re-depth 3) (set-timer 1))     ; a tick falls due immediately
   (jolt-current-disable-count)            ; the pair would hand it back here
   (set! re-in #f)))
(set-timer 1)
(let loop ((i 0)) (when (and (fx< i 20000000) (fx< re-depth 3)) (loop (fx+ i 1))))
(set-timer 0)
(timer-interrupt-handler re-saved)
(ok "10. the timer handler actually ran" (>= re-depth 1))
(ok "10. reading the depth did not re-enter the handler" (not re-entered))
(ok "10. depth back to 0 after the handler storm" (= 0 (jolt-current-disable-count)))

;; --- 11. handing the timer back leaves the fiber preemptible ----------------
;; jolt.host/run-interruptible (concurrency.ss) borrows the timer: it saves the
;; handler, installs one that escapes when an interrupt token is set, and arms a
;; tick of its own. Restoring the HANDLER on the way out is enough on a thread and
;; not on a fiber, because the timer is the other half — a bare (set-timer 0)
;; leaves the fiber running with nothing to preempt it, and only its next dispatch
;; arms again. So a fiber that calls it and then never parks pins its carrier for
;; as long as it likes, which is precisely the starvation window the top of
;; fibers.ss says no setting can open (jolt-ly62).
;;
;; ONE carrier, so B has nowhere else to go, and A borrows the timer with a token
;; nobody ever sets and then spins without parking. B runs only if A was preempted
;; AFTER run-interruptible returned. Unfixed, B never runs at all.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! 20000)

(define ri-stop (box #f))
(define ri-b-ran (box #f))
(define ri-returned (box #f))
(define ri-before (jolt-fiber-preempts))
(define ri-a
  (sa-fiber-spawn
   (lambda ()
     ;; a token nobody sets: the thunk simply returns and the borrow ends
     (jolt-run-interruptible (jolt-make-interrupt) (lambda () 42))
     (set-box! ri-returned #t)
     (spin-until ri-stop))))
(define ri-b (sa-fiber-spawn (lambda () (set-box! ri-b-ran #t))))
(jolt-fiber-ensure-carrier!)
(define ri-b-ran-while-a-spins?
  (wait-until (lambda () (unbox ri-b-ran)) 5.0
              "11. the queued fiber runs after the timer is handed back"))
(ok "11. A did return from run-interruptible" (unbox ri-returned))
(ok "11. the spinner was preempted after the borrow ended"
    (> (jolt-fiber-preempts) ri-before))
(ok "11. and the queued fiber ran without the spinner yielding"
    (and ri-b-ran-while-a-spins? (not (unbox ri-stop))))
(set-box! ri-stop #t)
(wait-until (lambda () (eq? 'done (jolt-fiber-state ri-a))) 5.0 "11. spinner completes")
(ok "11. the spinner still completed normally" (eq? 'done (jolt-fiber-state ri-a)))
;; The borrow must still WORK, or the fix above could be "never install the
;; handler". A token set before the call makes the first tick escape, and the
;; escape reaches the fiber as the ordinary interrupted ex-info.
(define ri-tok (jolt-make-interrupt))
(jolt-interrupt! ri-tok)
(ok "11. an interrupt still aborts the thunk on a fiber"
    (let ((caught (box #f)))
      (let ((f (sa-fiber-spawn
                (lambda ()
                  (guard (e (#t (set-box! caught #t)))
                    (jolt-run-interruptible
                     ri-tok
                     (lambda () (let loop ((i 0)) (if (fx=? i 100000000) i (loop (fx+ i 1)))))))))))
        (jolt-fiber-ensure-carrier!)
        (wait-until (lambda () (memq (jolt-fiber-state f) '(done dead))) 20.0
                    "11. the interrupted fiber finishes")
        (unbox caught))))

;; --- 11b. a PARK inside the borrow gives the timer back too (jolt-1rod) -------
;; Section 11 covers the borrow ENDING. This one covers the borrow being
;; suspended, which is a different exit and was not one at all.
;;
;; run-interruptible hands the timer back on a normal return, on a raise (its
;; guard) and on its own interrupt escape (its call/cc). A fiber park is none of
;; those: it escapes through the carrier's sched-k, and timer-interrupt-handler is
;; a setter write on a per-thread parameter, so nothing undoes it. The handler is
;; installed per DRAIN, not per dispatch, so the borrower's handler then stays live
;; over every later fiber on that carrier.
;;
;; That handler answers a quantum by re-arming its own tick and returning, so
;; nothing on the carrier is ever preempted again — the unbounded starvation
;; window the head of fibers.ss says no setting can open, reachable from ordinary
;; jolt code through jolt.host/run-interruptible.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! 20000)

(define rp-ch (ac-make 1 'fixed #f))
(define rp-stop (box #f))
(define rp-b-handler (box 'unset))
(define rp-b-ran (box #f))
(define rp-returned (box #f))
(define rp-a
  (sa-fiber-spawn
   (lambda ()
     ;; a token nobody sets; the thunk PARKS instead of returning
     (jolt-run-interruptible (jolt-make-interrupt) (lambda () (jolt-fiber-<! rp-ch)))
     (set-box! rp-returned #t))))
(define rp-b
  (sa-fiber-spawn
   (lambda ()
     ;; read on entry: the carrier hands the NEXT fiber its own handler and no
     ;; poll left over from A's borrow, which would run A's check here
     (set-box! rp-b-handler
               (if (and (eq? (timer-interrupt-handler) jolt-tick-fire)
                        (not (jolt-tick-demand jolt-tick-slot-poll)))
                   'scheduler 'borrowed))
     (set-box! rp-b-ran #t)
     (spin-until rp-stop))))
(jolt-fiber-ensure-carrier!)
(wait-until (lambda () (unbox rp-b-ran)) 5.0 "11b. the queued fiber runs while A is parked")
(ok "11b. A parked inside the borrow" (eq? 'parked (jolt-fiber-state rp-a)))
(ok "11b. the carrier got its own handler back and no poll" (eq? 'scheduler (unbox rp-b-handler)))
(define rp-before (jolt-fiber-preempts))
(sleep (make-time 'time-duration 300000000 0))   ; 0.3s: many quanta at 20k ticks
(ok "11b. and the spinner behind it is still preemptible"
    (> (jolt-fiber-preempts) rp-before))

;; The nastier half. The borrowed handler closes over the call/cc continuation
;; captured in A's stack, so left installed it fires under B and invokes it: A
;; returns from a park it never came back from and B is abandoned mid-computation,
;; state 'running for the life of the process with no error naming any of it.
;;
;; An interrupt raised while A is parked belongs to A, and A is not on the CPU, so
;; it must land when A comes back and nowhere else.
(define rp-tok2 (jolt-make-interrupt))
(define rp-ch2 (ac-make 1 'fixed #f))
(define rp-stop2 (box #f))
(define rp-c-done (box #f))
(define rp-a2-caught (box #f))
(define rp-a2
  (sa-fiber-spawn
   (lambda ()
     (guard (e (#t (set-box! rp-a2-caught #t)))
       ;; parks, then spins — the spin is what the re-established borrow has to
       ;; interrupt. A thunk that returned straight after the park would prove
       ;; only that nothing crashed: there would be nothing left to abort.
       (jolt-run-interruptible
        rp-tok2
        (lambda ()
          (jolt-fiber-<! rp-ch2)
          (let loop ((i 0)) (if (fx=? i 1000000000) i (loop (fx+ i 1))))))))))
(define rp-c
  (sa-fiber-spawn
   (lambda () (spin-until rp-stop2) (set-box! rp-c-done #t))))
(set-box! rp-stop #t)                       ; let B off the carrier first
(wait-until (lambda () (eq? 'done (jolt-fiber-state rp-b))) 5.0 "11b. B completes")
(wait-until (lambda () (eq? 'parked (jolt-fiber-state rp-a2))) 5.0 "11b. A2 parks in the borrow")
(jolt-interrupt! rp-tok2)                   ; fires while C owns the carrier
(sleep (make-time 'time-duration 300000000 0))
(ok "11b. an interrupt for a parked fiber does not resume it"
    (eq? 'parked (jolt-fiber-state rp-a2)))
(set-box! rp-stop2 #t)
;; Both halves are waited for: C sets the box as its body's last act and the
;; scheduler publishes 'done after the body returns, so a check of the state as
;; soon as the box flips raced that publish (seen on a loaded CI runner).
(wait-until (lambda () (and (unbox rp-c-done) (eq? 'done (jolt-fiber-state rp-c)))) 5.0
            "11b. the running fiber is not abandoned")
(ok "11b. the fiber that owned the carrier still finished"
    (and (unbox rp-c-done) (eq? 'done (jolt-fiber-state rp-c))))
;; and the interrupt is not lost — the resume re-takes BOTH halves of the borrow,
;; so the tick falls due inside the borrower's own spin and aborts it there. That
;; is also what says the before-thunk really re-established the handler: with only
;; the tick back, the scheduler's handler would preempt the spin forever instead.
(jolt-async-give rp-ch2 1)
(wait-until (lambda () (memq (jolt-fiber-state rp-a2) '(done dead))) 20.0 "11b. A2 finishes")
(ok "11b. the interrupt reaches the borrower on its own resume" (unbox rp-a2-caught))

;; A's own borrow still ends properly once it is fed.
(jolt-async-give rp-ch 1)
(wait-until (lambda () (unbox rp-returned)) 5.0 "11b. A returns from the borrow")
(ok "11b. the parked borrower returned normally"
    (and (unbox rp-returned) (eq? 'done (jolt-fiber-state rp-a))))

;; --- 12. a fiber that has committed to park is not preempted (jolt-9d3m) -----
;; The channel ops bracket the commit and the switch in disable-interrupts and say
;; why: they are ONE transition and a timer landing between them finds the fiber
;; already marked and takes it apart. The IO-parking seam CANNOT do that, because
;; it is Clojure — stdlib/jolt/io_poller.clj wait-fiber commits under the poller
;; lock and switches after releasing it, so the gap is preemptible and no amount
;; of care in that file can close it. The scheduler has to.
;;
;; The states a fiber can be observed in are the whole argument. resume* sets
;; 'running on every entry, and both other transitions run with interrupts off, so
;; a handler that fires on a fiber which is NOT 'running is looking at a fiber that
;; has committed to something and is a few instructions from handing the carrier
;; over. There are two such states and each fails differently.
;;
;; A drain on THIS thread rather than a carrier pool: the two failures are a lost
;; fiber and a self-linked run queue, and both are easier to see synchronously than
;; through a timeout.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min)

(define (cw-spin n) (let loop ((i 0) (a 0)) (if (fx=? i n) a (loop (fx+ i 1) (fx+ a i)))))
(define CW-SPIN 400000)     ; many quanta at the floor, so the timer certainly falls due

;; The control, and the test is worth nothing without it: the SAME spin in a fiber
;; that is plainly 'running must be preempted, or "not preempted" below would only
;; be saying the timer never fired.
(define cw-control-before (jolt-fiber-preempts))
(define cw-control (sa-fiber-spawn (lambda () (cw-spin CW-SPIN) 'ok)))
(sa-fiber-run-all)
(ok "12. control: the same spin IS preempted while the fiber is 'running"
    (> (jolt-fiber-preempts) cw-control-before))

;; 12a. committed, not yet switched. Unfixed: the handler flips it to 'ready,
;; queues it and parks through a continuation of its own; the fiber is dispatched,
;; runs on, and reaches the real switch with state 'running — so it is neither
;; queued nor 'parked, and sa-fiber-resume (which acts only on 'parked) can never
;; bring it back. The wake is a silent no-op and the fiber is gone.
(define cw-a-resumed #f)
(define cw-a-before (jolt-fiber-preempts))
(define cw-a
  (sa-fiber-spawn
   (lambda ()
     (let ((f (jolt-current-fiber)))
       ;; jolt.host/fiber-park-commit!
       (jolt-fiber-state-set! f 'parked)
       (cw-spin CW-SPIN)                    ; the gap the poller lock has just left open
       ;; jolt.host/fiber-to-scheduler!
       (jolt-fiber-to-scheduler! f)
       (set! cw-a-resumed #t)
       'ok))))
(sa-fiber-run-all)
(ok "12a. a committed fiber is not preempted in the gap"
    (= (jolt-fiber-preempts) cw-a-before))
(ok "12a. it reaches the switch still 'parked" (eq? 'parked (jolt-fiber-state cw-a)))
(sa-fiber-resume cw-a)
(sa-fiber-run-all)
(ok "12a. so the poller's wake brings it back" cw-a-resumed)
(ok "12a. and it completes" (eq? 'done (jolt-fiber-state cw-a)))

;; 12b. committed AND already woken. The poller thread can call fiber-resume the
;; moment the pipe write lands, which is before the switch: the fiber is then
;; 'ready and ON the run queue while still running. Preempting it enqueues a fiber
;; that is already queued, and jolt-fiber-enqueue!/locked spells out what that is —
;; not a duplicate, a CYCLE (f.next := f when f is the sole entry), so the carrier
;; dispatches the same fiber forever. Here the second dispatch finds it 'done and
;; the drain dies with "fiber in unexpected state".
(define cw-b-resumed #f)
(define cw-b-before (jolt-fiber-preempts))
(define cw-b
  (sa-fiber-spawn
   (lambda ()
     (let ((f (jolt-current-fiber)))
       (jolt-fiber-state-set! f 'parked)    ; commit
       (sa-fiber-resume f)                  ; the poller got in first: 'ready + queued
       (cw-spin CW-SPIN)
       (jolt-fiber-to-scheduler! f)
       (set! cw-b-resumed #t)
       'ok))))
(ok "12b. a woken-but-not-yet-switched fiber drains without error"
    (guard (e (#t #f)) (sa-fiber-run-all) #t))
(ok "12b. it was not preempted while queued" (= (jolt-fiber-preempts) cw-b-before))
(ok "12b. it ran to completion" (and cw-b-resumed (eq? 'done (jolt-fiber-state cw-b))))
(ok "12b. and left the run queue empty, not self-linked"
    (let ((c (vector-ref jolt-fiber-carriers 0)))
      (and (not (jolt-carrier-head c)) (not (jolt-carrier-tail c)))))

;; --- 13. the completion path is a transition too (jolt-kdq7) ------------------
;; Section 12 is about a fiber that has committed to PARK. This is the other
;; multi-write transition that reads as atomic and is not: jolt-fiber-done! settled
;; the payload, dropped the scheduler's own fields, and published the terminal state
;; last — so between the slice going and the state arriving the fiber was still
;; 'running with no slice, and a preemption there parked it and made
;; jolt-fiber-slice-save! write into #f. That raise is inside the timer handler and
;; jolt-fiber-done! is called outside resume*'s guard, so it killed the CARRIER, and
;; every fiber pinned to it was stranded — a wedged program with no error pointing at
;; the fiber that completed.
;;
;; AIMED, not waited for. A quantum landing in a three-poll window is a coin flip
;; nobody should gate on, so the fiber's last act is (set-timer n) and a return: the
;; tick then falls due n poll points into the completion path, and sweeping n walks
;; the whole path. 3 of the first 40 offsets parked a slice-less fiber before the fix.
;;
;; The probe is a wrapper on jolt-fiber-slice-save!, which is where the write happened
;; and therefore the only place that can see it — a fiber that survives tells you
;; nothing, because a park with no slice restores nothing and often gets away with it.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! #f)

(define cp-bad 0)
(define cp-orig jolt-fiber-slice-save!)
(set! jolt-fiber-slice-save!
  (lambda (f)
    (if (jolt-dslice? (jolt-fiber-slice f))
        (cp-orig f)
        ;; do NOT write, so the sweep can finish and report every offset rather than
        ;; dying on the first
        (set! cp-bad (+ cp-bad 1)))))

(define CP-N 40)
(define cp-before (jolt-fiber-preempts))
(define cp-unfinished 0)
(let loop ((n 1))
  (when (<= n CP-N)
    (let ((f (sa-fiber-spawn (lambda () (set-timer n) 'v))))
      (sa-fiber-run-all)
      ;; a preempted fiber goes back on the queue, so keep draining until it settles
      (let drain ((k 0))
        (cond ((memq (jolt-fiber-state f) '(done dead)) #t)
              ((< k 50) (sa-fiber-run-all) (drain (+ k 1)))
              (else (set! cp-unfinished (+ cp-unfinished 1)))))
      (loop (+ n 1)))))
(set-timer 0)
(set! jolt-fiber-slice-save! cp-orig)

;; The control: without preemptions landing in that path the sweep proves nothing.
(ok "13. the aimed ticks did preempt inside the completion path"
    (> (jolt-fiber-preempts) cp-before))
(ok "13. no offset parks a fiber whose slice has been cleared" (= 0 cp-bad))
(ok "13. every fiber in the sweep finished" (= 0 cp-unfinished))

;; And the fields really are cleared by the time the fiber is finished — the fix moves
;; the clears, it does not drop them. A retained continuation or slice on a completed
;; fiber is a stack segment and a dynamic slice that never get collected.
(define cp-f (sa-fiber-spawn (lambda () 'v)))
(sa-fiber-run-all)
(ok "13. a completed fiber still drops its continuation, sm step and slice"
    (and (eq? 'done (jolt-fiber-state cp-f))
         (not (jolt-fiber-k cp-f))
         (not (jolt-fiber-sm cp-f))
         (not (jolt-fiber-slice cp-f))
         (eq? 'v (jolt-fiber-result cp-f))))
;; The same for the dying path, which has the identical shape.
(define cp-d (sa-fiber-spawn (lambda () (error 'cp "boom"))))
(sa-fiber-run-all)
(ok "13. and so does one that died, with its condition kept"
    (and (eq? 'dead (jolt-fiber-state cp-d))
         (not (jolt-fiber-k cp-d))
         (not (jolt-fiber-slice cp-d))
         (jolt-fiber-error cp-d)))

;; --- 14. the borrowed timer still preempts (jolt-449m) ------------------------
;; jolt-run-interruptible BORROWS the timer: it installs its own handler and arms
;; its own tick. jolt-ly62 made the borrow give both halves back on every way out,
;; the park included, so it can no longer outlive itself. What was still open is
;; the window DURING the borrow: the borrowed handler answered a quantum by
;; re-arming its own tick and RETURNING, so nothing on the carrier was preempted
;; for as long as the body ran — and the body is arbitrary user code reached
;; through the public jolt.host/run-interruptible seam (a whole eval, in the
;; interrupt use it exists for). Measured unfixed, one carrier, 20,000 ticks: a
;; spinning fiber inside a borrow ran 36,983,637 iterations with 0 preemptions
;; and a sibling queued behind it never ran; the same spin outside a borrow was
;; preempted 46 times.
;;
;; Asserted on the preempt COUNT and not on a starving sibling, because the count
;; is the mechanism and a sibling is a race. The control beside it is what makes
;; the count mean something: the same spin, same length, no borrow.
(jolt-fiber-pool-reset!)
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-preempt-ticks-set! jolt-fiber-preempt-ticks-min)
(define (rb-spin n) (let loop ((i 0)) (if (fx<? i n) (loop (fx+ i 1)) i)))
(define RB-N 2000000)

;; control: a plain spin of this length is preempted, so the length is enough.
(define rb-c0 (jolt-fiber-preempts))
(define rb-plain (sa-fiber-spawn (lambda () (rb-spin RB-N))))
(sa-fiber-run-all)
(ok "14. control: a plain spin of this length is preempted"
    (> (jolt-fiber-preempts) rb-c0))
(ok "14. control: and it finished" (eq? 'done (jolt-fiber-state rb-plain)))

;; the same spin inside a borrow
(define rb-c1 (jolt-fiber-preempts))
(define rb-borrowed
  (sa-fiber-spawn
   (lambda ()
     (jolt-run-interruptible (jolt-make-interrupt) (lambda () (rb-spin RB-N))))))
(sa-fiber-run-all)
(ok "14. a fiber inside run-interruptible is preempted too"
    (> (jolt-fiber-preempts) rb-c1))
(ok "14. and the body still ran to completion"
    (and (eq? 'done (jolt-fiber-state rb-borrowed))
         (eqv? RB-N (jolt-fiber-result rb-borrowed))))

;; the borrow's own job is unchanged: a token set mid-body still aborts, and the
;; abort is an EXIT, so it must not be confused with the preemption escape.
(define rb-tok (jolt-make-interrupt))
(define rb-aborted (box #f))
(define rb-int
  (sa-fiber-spawn
   (lambda ()
     (guard (e (#t (set-box! rb-aborted (jolt-interrupted? rb-tok))))
       (jolt-run-interruptible rb-tok
         (lambda () (jolt-interrupt! rb-tok) (rb-spin RB-N) 'not-interrupted))))))
(sa-fiber-run-all)
(ok "14. an interrupt inside a fiber still aborts the body" (unbox rb-aborted))
(ok "14. and the fiber survives it" (eq? 'done (jolt-fiber-state rb-int)))

;; An interrupt that escapes on a quantum's fire must leave the quantum behind.
;; The token is set before the borrow, so its first check aborts, and with the
;; quantum shorter than the poll interval that check runs riding the quantum's
;; fire. The escape used to skip the preemption the fire had already taken off
;; the books, and the borrow's exit then left the timer disarmed: the fiber ran
;; on unpreempted until it next parked.
(define rb-tok2 (jolt-make-interrupt))
(define rb-after (box #f))
(define rb-int2
  (sa-fiber-spawn
   (lambda ()
     (jolt-interrupt! rb-tok2)
     (guard (e (#t #t))
       (jolt-run-interruptible rb-tok2 (lambda () (rb-spin RB-N))))
     (set-box! rb-after (jolt-fiber-preempts))
     (rb-spin RB-N))))
(sa-fiber-run-all)
(ok "14. a fiber is still preempted after an interrupt escaped its borrow"
    (and (unbox rb-after) (> (jolt-fiber-preempts) (unbox rb-after))))

;; The poll is gone once the borrow is over, on the REAL exit as well as on a
;; park, and the handler is still the tick scheduler's.
(ok "14. the borrow leaves no poll and the scheduler's handler on the real exit"
    (and (eq? (timer-interrupt-handler) jolt-tick-fire)
         (not (jolt-tick-demand jolt-tick-slot-poll))))

;; Off a fiber nothing changes: there is no quantum to hand back, and the borrow
;; leaves the timer disarmed.
(define rb-off (jolt-run-interruptible (jolt-make-interrupt) (lambda () (rb-spin 1000) 'off)))
(ok "14. off a fiber the borrow is unchanged" (eq? 'off rb-off))

(jolt-fiber-preempt-ticks-set! #f)
(jolt-fiber-pool-reset!)

(printf "\nfibers-preempt-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "fibers-preempt-test: PASS — preemptive scheduling\n") (exit 0))
    (exit 1))
