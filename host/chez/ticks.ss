;; host/chez/ticks.ss — one owner for a thread's engine timer.
;;
;; Chez gives each thread one engine timer (set-timer, timer-interrupt-handler),
;; polled at procedure calls and loop back-edges. It is the only interrupt that
;; reaches every thread: a signal handler runs on the main thread whichever
;; thread the signal landed on, and the keyboard interrupt carries SIGINT. Three
;; things want it:
;;
;;   sample    the sampling profiler (sampler.ss): records the stack and
;;             always returns.
;;   poll      jolt-run-interruptible (java/concurrency.ss): checks the
;;             innermost borrow's token and may escape out of the body.
;;   preempt   fiber preemption (fibers.ss): may switch the fiber out.
;;
;; Each is a SLOT holding a demand: how many ticks until it is next due, or #f.
;; The timer is armed for the nearest demand, and when it fires every demand is
;; charged the ticks that passed and the due clients run in slot order. The
;; sampler goes first because it returns, then the poll, which may escape, then
;; the preemption, which may switch. A client that cannot act yet re-registers a
;; short retry, as the preempt handler already did.
;;
;; WHY A SCHEDULER AND NOT HAND-OFFS. run-interruptible used to borrow the timer
;; from the fiber scheduler: save the handler, install its own, count the ticks
;; it borrowed so a quantum still fell due, and give everything back through a
;; dynamic-wind. Three bugs were found in that handoff (jolt-ly62, jolt-1rod,
;; jolt-449m), and a sampler written the same way would have needed a third copy
;; of the accounting. Here it is done once, for everyone.
;;
;; THE COST RULE. Fiber dispatch arms the timer on every switch, and a yield is
;; ~240 ns, so the preempt client must cost what a bare set-timer costs when it
;; is alone. It does: a thread with nothing but a preemption registered is SOLO,
;; its register (slot 13) reads 0, and jolt-tick-preempt! is that read plus the
;; set-timer. The preempt demand then lives in the hardware timer itself, which
;; nobody else arms. Registering any other client moves the thread to a state
;; vector and takes the preempt demand out of the timer (jolt-tick-go-multi!);
;; when the last other client leaves it goes back (jolt-tick-arm!).
;;
;; The state is per thread through a virtual register, never a thread parameter:
;; a forked thread inherits parameters, so a child would share its parent's
;; vector. A fresh thread's register is fixnum 0, which reads as solo.
;;
;; Every change to the state runs with interrupts disabled. The timer cannot fire
;; inside, and a fire that fell due inside is delivered after the region, where
;; the dispatcher recomputes from the timer itself, so a late or spurious
;; delivery runs nothing that is not due.

(define jolt-vreg-ticks 13)

;; state vector layout
(define jolt-tick-armed 0)      ; the ticks the timer was last armed with, 0 = disarmed
(define jolt-tick-slot-sample 1)
(define jolt-tick-slot-poll 2)
(define jolt-tick-slot-preempt 3)
(define jolt-tick-firing 4)     ; #t while the dispatcher runs the sampler: re-arm at the end
(define jolt-tick-sampler 5)    ; sampler.ss's per-thread record, or #f
(define jolt-tick-state-size 6)

;; The clients, by slot. Assigned by the files that define them, which load after
;; this one: sampler.ss sets the sample client, java/concurrency.ss the poll
;; client. The preempt client is read at call time, so fibers.ss's handler is
;; found however late it is defined.
(define jolt-tick-clients
  (vector #f
          (lambda () (void))
          (lambda () (void))
          (lambda () (jolt-fiber-preempt-handler))))
(define (jolt-tick-client-set! slot proc) (vector-set! jolt-tick-clients slot proc))

;; Set by sampler.ss while sampling is on, so a thread that is already running
;; joins at its next timer event. Read on every fire, so a plain global.
;;
;; A thread with nothing registered would have no timer event, so it keeps a slow
;; WATCH armed: the timer fires every jolt-tick-watch-ticks and does nothing unless
;; sampling started since. It is not a client and has no slot, because it stands
;; for no demand: it lives in the timer of a solo thread off a fiber, and anything
;; that wants the timer simply overwrites it. A dispatch's quantum replaces it, and
;; jolt-tick-go-multi! carries the rest of it as a preempt demand, whose fire off a
;; fiber does nothing.
(define jolt-tick-sampling? #f)
(define (jolt-tick-join-sampling!) (void))   ; redefined by sampler.ss
(define (jolt-tick-sampling-set! on? join)
  (set! jolt-tick-join-sampling! join)
  (set! jolt-tick-sampling? on?))

(define-syntax jolt-tick-state
  (syntax-rules () ((_) (virtual-register 13))))

(define jolt-tick-watch-ticks 1000000)
;; Arm the watch on this thread, leaving a timer that is already running alone.
(define (jolt-tick-watch!)
  (disable-interrupts)
  (when (and (eq? (jolt-tick-state) 0) (not (jolt-current-fiber)))
    (let ((rem (set-timer 0)))
      (unless (eq? (timer-interrupt-handler) jolt-tick-fire)
        (timer-interrupt-handler jolt-tick-fire))
      (set-timer (if (fx>? rem 0) rem jolt-tick-watch-ticks))))
  (enable-interrupts)
  (void))

;; Arm (n > 0) or cancel (n = 0) the preempt demand. Syntax, so the solo case is
;; the register read and the set-timer the fiber scheduler always paid.
(define-syntax jolt-tick-preempt!
  (syntax-rules ()
    ((_ n) (let ((s (jolt-tick-state)) (m n))
             (if (eq? s 0)
                 (begin (set-timer m) (void))
                 (jolt-tick-demand! s jolt-tick-slot-preempt m))))))

;; Arm (n > 0) or cancel (n = 0) the demand in a slot.
(define (jolt-tick-set! slot n)
  (let ((s (jolt-tick-state)))
    (cond
      ((not (eq? s 0)) (jolt-tick-demand! s slot n))
      ((fx=? slot jolt-tick-slot-preempt) (set-timer n) (void))
      ;; solo has no other demand to cancel
      ((fx=? n 0) (void))
      (else (jolt-tick-demand! s slot n)))))

;; The demand in a slot, #f when there is none. For tests and the sampler.
(define (jolt-tick-demand slot)
  (let ((s (jolt-tick-state)))
    (if (eq? s 0) #f (vector-ref s slot))))

;; The state is read again once interrupts are off. The caller's read came
;; before this call, which is a timer poll point, and a fire there can move the
;; thread between solo and multi: a demand written into the vector it left would
;; be lost, and a second go-multi! would drop the sampler's record.
(define (jolt-tick-demand! s0 slot n)
  (disable-interrupts)
  (let* ((s (jolt-tick-state))
         (s (if (eq? s 0) (jolt-tick-go-multi!) s)))
    (if (vector-ref s jolt-tick-firing)
        (vector-set! s slot (if (fx>? n 0) n #f))
        (begin
          (jolt-tick-account! s)
          (vector-set! s slot (if (fx>? n 0) n #f))
          (jolt-tick-arm! s))))
  (enable-interrupts)
  (void))

;; Leave solo: take the preempt demand out of the timer. A fiber is never running
;; with its timer disarmed (dispatch arms it, the handler re-arms or switches),
;; so a fiber whose timer reads 0 has a preemption that fell due and is waiting
;; for interrupts to come back on. It is kept due rather than dropped, which
;; would leave the fiber unpreemptible until its next dispatch. Off a fiber
;; there is nothing to preempt.
(define (jolt-tick-go-multi!)
  (let* ((rem (set-timer 0))
         (pre (cond ((fx>? rem 0) rem)
                    ((jolt-current-fiber) 1)
                    (else #f)))
         (s (make-vector jolt-tick-state-size #f)))
    (vector-set! s jolt-tick-armed 0)
    (vector-set! s jolt-tick-slot-preempt pre)
    (unless (eq? (timer-interrupt-handler) jolt-tick-fire)
      (timer-interrupt-handler jolt-tick-fire))
    (set-virtual-register! 13 s)
    s))

;; Charge every demand the ticks that passed since the timer was armed, and leave
;; it disarmed. A demand at or below zero is due.
(define (jolt-tick-account! s)
  (let* ((rem (set-timer 0))
         (el (fxmax 0 (fx- (vector-ref s jolt-tick-armed) rem))))
    (vector-set! s jolt-tick-armed 0)
    (unless (fx=? el 0)
      (do ((i jolt-tick-slot-sample (fx+ i 1))) ((fx>? i jolt-tick-slot-preempt))
        (let ((d (vector-ref s i)))
          (when d (vector-set! s i (fx- d el))))))))

;; Arm the timer for the nearest demand. With no sample or poll demand left the
;; thread goes back to solo, and the preempt demand, if any, goes back into the
;; timer.
(define (jolt-tick-arm! s) (jolt-tick-arm*! s #t))
(define (jolt-tick-arm*! s may-go-solo?)
  (let ((a (vector-ref s jolt-tick-slot-sample))
        (b (vector-ref s jolt-tick-slot-poll))
        (c (vector-ref s jolt-tick-slot-preempt)))
    (if (and may-go-solo? (not a) (not b) (not (vector-ref s jolt-tick-sampler)))
        (begin
          (set-virtual-register! 13 0)
          (set-timer (if c (fxmax 1 c) jolt-tick-watch-ticks)))
        (let* ((m (if (and b (or (not a) (fx<? b a))) b a))
               (m (if (and c (or (not m) (fx<? c m))) c m)))
          (when m
            (let ((n (fxmax 1 m)))
              (vector-set! s jolt-tick-armed n)
              (set-timer n)))))))

(define (jolt-tick-due? s slot)
  (let ((d (vector-ref s slot)))
    (and d (fx<=? d 0)
         (begin (vector-set! s slot #f) #t))))

;; The handler, installed on a thread the first time anything other than the
;; fiber scheduler registers there, and by the scheduler for every drain.
(define (jolt-tick-fire)
  (let ((s (jolt-tick-state)))
    (cond
      ((not (eq? s 0)) (jolt-tick-fire-multi s))
      ;; A solo thread's timer is the preemption's, so this is a quantum. If
      ;; sampling started since, join it instead; that leaves solo with the
      ;; preemption still due, and the next tick runs both.
      (jolt-tick-sampling? (jolt-tick-join-sampling!))
      ((jolt-current-fiber) ((vector-ref jolt-tick-clients jolt-tick-slot-preempt)))
      ;; Off a fiber it was the watch. On a carrier between dispatches it was a
      ;; quantum that fell due mid-park, and the next dispatch overwrites this.
      (else (set-timer jolt-tick-watch-ticks) (void)))))

(define (jolt-tick-fire-multi s)
  (when (and jolt-tick-sampling? (not (vector-ref s jolt-tick-sampler)))
    (jolt-tick-join-sampling!))
  (disable-interrupts)
  (jolt-tick-account! s)
  (let* ((ds (jolt-tick-due? s jolt-tick-slot-sample))
         (dq (jolt-tick-due? s jolt-tick-slot-preempt))
         ;; A pending poll runs with a preemption even before it is due. A
         ;; preemption parks the fiber, the park cancels the poll and the
         ;; resume registers it afresh, so on a fiber preempted every quantum
         ;; a poll that waited for its own deadline would never get one. An
         ;; early check costs a token read.
         (dp (or (jolt-tick-due? s jolt-tick-slot-poll)
                 (and dq (vector-ref s jolt-tick-slot-poll)
                      (begin (vector-set! s jolt-tick-slot-poll #f) #t)))))
    ;; The sampler runs with interrupts on, and it can raise (it allocates, and
    ;; the collect-request handler raises past the heap cap). An escape must not
    ;; leave firing set, which turns every later demand into a write nobody arms
    ;; for, nor take the poll and preemption this fire took off the books.
    (when ds
      (vector-set! s jolt-tick-firing #t)
      (enable-interrupts)
      (let ((done #f))
        (dynamic-wind
          void
          (lambda () ((vector-ref jolt-tick-clients jolt-tick-slot-sample)) (set! done #t))
          (lambda ()
            (unless done
              (disable-interrupts)
              (vector-set! s jolt-tick-firing #f)
              (jolt-tick-account! s)
              (when dp (vector-set! s jolt-tick-slot-poll 1))
              (when dq (vector-set! s jolt-tick-slot-preempt 1))
              (jolt-tick-arm! s)
              (enable-interrupts)))))
      (disable-interrupts)
      (vector-set! s jolt-tick-firing #f))
    ;; Armed before the clients that may escape run, so an escape leaves the
    ;; remaining demands armed. It stays multi even with nothing left but a
    ;; preemption, because a client that re-registers from solo would go back
    ;; through jolt-tick-go-multi!, and on a fiber that reads a disarmed timer as
    ;; a preemption already due. Here it is not due, it is the one about to run,
    ;; and arming it again lands a second preemption inside the first: the fiber
    ;; is queued twice. The next change of demand settles the thread back to solo.
    (jolt-tick-arm*! s (not (or dp dq)))
    (enable-interrupts)
    ;; A poll that escapes (run-interruptible's token was set) skips the
    ;; preemption after it, so with one due the escape puts it back, due at once.
    (when dp
      (if dq
          (let ((done #f))
            (dynamic-wind
              void
              (lambda () ((vector-ref jolt-tick-clients jolt-tick-slot-poll)) (set! done #t))
              (lambda () (unless done (jolt-tick-preempt! 1)))))
          ((vector-ref jolt-tick-clients jolt-tick-slot-poll))))
    (when dq ((vector-ref jolt-tick-clients jolt-tick-slot-preempt)))))
