;; concurrency.ss — real OS-thread futures + promises for the Chez host.
;;
;; SHARED-HEAP semantics, like JVM Clojure: a future body runs on a native thread
;; (fork-thread) over the SAME heap, so a captured atom is shared and the body's
;; mutations are visible to the parent. deref blocks on a mutex+condition latch.
;;
;; future / future-call / future-cancel / future? / future-done? / future-cancelled?
;; promise / deliver, and the deref extension for both, are bound here (some
;; re-asserted in post-prelude.ss over the overlay's versions).
;;
;; pmap / pcalls / pvalues live in the clojure.core overlay (40-lazy) expressed
;; over `future`, so they light up for free once future-call exists.
;;
;; Loaded near the end of rt.ss — after atoms.ss (jolt-deref, the atom lock) and
;; dyn-binding.ss (the thread-local binding stack we convey into the worker).
;; Requires a threaded Chez build (fork-thread / make-mutex / make-condition).

;; --- time helpers -----------------------------------------------------------
;; A relative duration / absolute deadline from a millisecond count (a jolt number).
(define (ms->duration ms)
  (let* ((ms* (exact (floor ms)))
         (secs (quotient ms* 1000))
         (nanos (* (remainder ms* 1000) 1000000)))
    (make-time 'time-duration nanos secs)))
(define (ms->deadline ms) (add-duration (current-time 'time-utc) (ms->duration ms)))

;; --- futures ----------------------------------------------------------------
;; A future is a mutable cell guarded by `mu`; workers/derefs coordinate on `cv`.
;;   done?       — result (or cancellation) is final; derefs may proceed
;;   cancelled?  — future-cancel won before the body finished
;;   ok?         — payload is a value (else payload is a raised condition/value)
;;   payload     — the result value, or the captured throw
(define-record-type jolt-future
  (fields (mutable done?) (mutable cancelled?) (mutable ok?) (mutable payload) mu cv)
  (nongenerative jolt-future-v1))

;; (future-call thunk): spawn a thread running (thunk). The dynamic bindings in
;; effect now are conveyed into the worker (Chez inherits thread-parameters at
;; fork; we also install an explicit snapshot for certainty). The result — value
;; or thrown condition — is latched and broadcast; a cancel that already finalized
;; the future makes the late result a no-op.
(define (jolt-future-call thunk)
  (let ((f (make-jolt-future #f #f #f jolt-nil (make-mutex) (make-condition)))
        (snap (dyn-binding-stack)))
    (fork-thread
     (lambda ()
       (dyn-binding-stack snap)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (with-mutex (jolt-future-mu f)
           (unless (jolt-future-done? f)            ; not already cancelled
             (jolt-future-ok?-set! f (car r))
             (jolt-future-payload-set! f (cdr r))
             (jolt-future-done?-set! f #t))
           (condition-broadcast (jolt-future-cv f))))))
    f))

;; Final value of a settled future (called OUTSIDE the lock): re-raise a captured
;; throw, signal a cancellation, else the value.
(define (jolt-future-finish f)
  (cond
    ((jolt-future-cancelled? f)
     (jolt-throw (jolt-ex-info "Future cancelled" (jolt-hash-map))))
    ((jolt-future-ok? f) (jolt-future-payload f))
    (else (raise (jolt-future-payload f)))))

(define (jolt-future-deref f)
  (with-mutex (jolt-future-mu f)
    (let loop ()
      (unless (jolt-future-done? f)
        (condition-wait (jolt-future-cv f) (jolt-future-mu f))
        (loop))))
  (jolt-future-finish f))

;; (deref f timeout-ms timeout-val): wait up to timeout-ms; return timeout-val if
;; it has not settled by the absolute deadline.
(define (jolt-future-deref-timed f ms timeout-val)
  (let* ((deadline (ms->deadline ms))
         (settled (with-mutex (jolt-future-mu f)
                    (let loop ()
                      (cond ((jolt-future-done? f) #t)
                            ((condition-wait (jolt-future-cv f) (jolt-future-mu f) deadline)
                             (loop))                       ; woken — recheck
                            (else (jolt-future-done? f))))))) ; timed out: final check
    (if settled (jolt-future-finish f) timeout-val)))

;; future-cancel: the running thread can't be interrupted, but the future object
;; reflects the cancellation — if not already settled, mark it cancelled+done so
;; derefs raise and the predicates flip. Returns true iff this call cancelled it.
(define (jolt-future-cancel f)
  (let ((cancelled (with-mutex (jolt-future-mu f)
                     (if (jolt-future-done? f)
                         #f
                         (begin (jolt-future-cancelled?-set! f #t)
                                (jolt-future-done?-set! f #t)
                                (condition-broadcast (jolt-future-cv f))
                                #t)))))
    cancelled))

(define (jolt-native-future-done? x)
  (if (jolt-future? x) (jolt-future-done? x)
      (jolt-throw (jolt-ex-info "future-done? requires a future" (jolt-hash-map)))))
(define (jolt-native-future-cancelled? x)
  (and (jolt-future? x) (jolt-future-cancelled? x)))

;; --- promises ---------------------------------------------------------------
;; A blocking promise (like the JVM): deref parks until deliver, then caches the
;; value. deliver wins once; later delivers return nil.
(define-record-type jolt-promise
  (fields (mutable delivered?) (mutable value) mu cv)
  (nongenerative jolt-promise-v1))

(define (jolt-promise-new) (make-jolt-promise #f jolt-nil (make-mutex) (make-condition)))

(define (jolt-deliver p v)
  (if (jolt-promise? p)
      (let ((won (with-mutex (jolt-promise-mu p)
                   (if (jolt-promise-delivered? p)
                       #f
                       (begin (jolt-promise-value-set! p v)
                              (jolt-promise-delivered?-set! p #t)
                              (condition-broadcast (jolt-promise-cv p))
                              #t)))))
        (if won p jolt-nil))
      (jolt-throw (jolt-ex-info "deliver requires a promise" (jolt-hash-map)))))

(define (jolt-promise-deref p)
  (with-mutex (jolt-promise-mu p)
    (let loop ()
      (unless (jolt-promise-delivered? p)
        (condition-wait (jolt-promise-cv p) (jolt-promise-mu p))
        (loop))))
  (jolt-promise-value p))

(define (jolt-promise-deref-timed p ms timeout-val)
  (let* ((deadline (ms->deadline ms))
         (got (with-mutex (jolt-promise-mu p)
                (let loop ()
                  (cond ((jolt-promise-delivered? p) #t)
                        ((condition-wait (jolt-promise-cv p) (jolt-promise-mu p) deadline)
                         (loop))
                        (else (jolt-promise-delivered? p)))))))
    (if got (jolt-promise-value p) timeout-val)))

;; --- agents (async, per-agent serialized dispatch) --------------------------
;; JVM semantics: send/send-off enqueue an action and a single worker thread
;; applies them to the state IN ORDER; deref reads the
;; (possibly not-yet-updated) state without blocking; await blocks until the queue
;; drains. An action error is captured (agent-error) and stops the queue.
(define-record-type jolt-agent
  (fields (mutable state) (mutable err) (mutable validator)
          (mutable queue) (mutable running?) mu cv)
  (nongenerative jolt-agent-v1))

;; (agent state) / (agent state :validator f :error-mode m :meta x): only :validator
;; has runtime behaviour here; other opts are accepted/ignored.
(define (jolt-agent-new state . opts)
  (let loop ((o opts) (validator jolt-nil))
    (cond
      ((or (null? o) (null? (cdr o)))
       (make-jolt-agent state jolt-nil validator (vector '() '()) #f (make-mutex) (make-condition)))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o)))
      (else (loop (cddr o) validator)))))

;; The action queue is an amortized-O(1) FIFO held as a mutable #(out in): `out` is
;; the front, `in` holds sends reversed onto it (an append-to-a-list send was O(n)).
;; All three helpers run under the agent mutex.
(define (jagent-q-empty? a)
  (let ((q (jolt-agent-queue a))) (and (null? (vector-ref q 0)) (null? (vector-ref q 1)))))
(define (jagent-q-push! a entry)
  (let ((q (jolt-agent-queue a))) (vector-set! q 1 (cons entry (vector-ref q 1)))))
(define (jagent-q-pop! a)
  (let ((q (jolt-agent-queue a)))
    (when (null? (vector-ref q 0))
      (vector-set! q 0 (reverse (vector-ref q 1))) (vector-set! q 1 '()))
    (let ((out (vector-ref q 0))) (vector-set! q 0 (cdr out)) (car out))))

;; Drain the queue, applying each action (f state arg*) outside the lock (an action
;; may send/deref the same agent). A validator rejection or a thrown action puts the
;; agent in an error state and halts the queue (JVM :fail mode).
(define (jolt-agent-worker a)
  (let loop ()
    (let ((act (with-mutex (jolt-agent-mu a)
                 (if (or (not (jolt-nil? (jolt-agent-err a))) (jagent-q-empty? a))
                     (begin (jolt-agent-running?-set! a #f)
                            (condition-broadcast (jolt-agent-cv a)) #f)
                     (jagent-q-pop! a)))))
      (when act
        (guard (e (#t (with-mutex (jolt-agent-mu a)
                        (jolt-agent-err-set! a e)
                        (condition-broadcast (jolt-agent-cv a)))))
          (let ((nv (apply jolt-invoke (car act) (jolt-agent-state a) (cdr act))))
            (let ((vf (jolt-agent-validator a)))
              (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf nv)))
                (error #f "Invalid reference state")))
            (jolt-agent-state-set! a nv)))
        (loop)))))

;; send / send-off: enqueue the action, start the worker if idle. (jolt treats them
;; identically — one serialized worker per agent — which is observably a superset of
;; the JVM's fixed/cached pool split.)
(define (jolt-agent-send a f . args)
  (with-mutex (jolt-agent-mu a)
    (jagent-q-push! a (cons f args))
    (unless (jolt-agent-running? a)
      (jolt-agent-running?-set! a #t)
      (fork-thread (lambda () (jolt-agent-worker a)))))
  a)

;; (await & agents): block until each agent's queue has drained.
(define (jolt-agent-await . agents)
  (for-each
   (lambda (a)
     (with-mutex (jolt-agent-mu a)
       (let loop ()
         (when (or (jolt-agent-running? a) (not (jagent-q-empty? a)))
           (condition-wait (jolt-agent-cv a) (jolt-agent-mu a)) (loop)))))
   agents)
  jolt-nil)

(define (jolt-agent-error a) (jolt-agent-err a))
(define (jolt-agent-restart a new-state . _opts)
  (jolt-agent-err-set! a jolt-nil)
  (jolt-agent-state-set! a new-state)
  a)

;; --- delay (lazy once-forced computation) -----------------------------------
;; (delay body) -> (make-delay (fn [] body)) (overlay macro); force/deref run the
;; thunk once under a lock and cache the value (JVM delays are thread-safe). force
;; (overlay) is (if (delay? x) (deref x) x), so it works once delay?/deref do.
(define-record-type jolt-delay (fields thunk (mutable realized?) (mutable value) (mutable exn) mu)
  (nongenerative jolt-delay-v1))
(define (jolt-make-delay thunk) (make-jolt-delay thunk #f jolt-nil #f (make-mutex)))
;; run the thunk once, like Clojure's Delay: if it throws, cache the exception
;; (the delay IS realized) and re-throw it on every deref — do NOT re-run the
;; body (so value-fns memoize and there is no cache-stampede / retried side
;; effect). Store the exception inside the lock, re-raise outside it so the mutex
;; is always released.
(define (jolt-delay-force d)
  (with-mutex (jolt-delay-mu d)
    (unless (jolt-delay-realized? d)
      (guard (e (#t (jolt-delay-exn-set! d e) (jolt-delay-realized?-set! d #t)))
        (jolt-delay-value-set! d (jolt-invoke (jolt-delay-thunk d)))
        (jolt-delay-realized?-set! d #t))))
  (if (jolt-delay-exn d) (raise (jolt-delay-exn d)) (jolt-delay-value d)))

;; --- deref extension --------------------------------------------------------
;; Chain the fully-built jolt-deref (atoms/vars/volatiles/reduced) with futures,
;; promises, agents, and delays; accept the timed (deref ref ms val) arity for the
;; blocking ref types.
(define %pre-conc-deref jolt-deref)
(set! jolt-deref
  (lambda (x . opts)
    (cond
      ((jolt-future? x)
       (if (null? opts) (jolt-future-deref x)
           (jolt-future-deref-timed x (car opts) (cadr opts))))
      ((jolt-promise? x)
       (if (null? opts) (jolt-promise-deref x)
           (jolt-promise-deref-timed x (car opts) (cadr opts))))
      ((jolt-agent? x) (jolt-agent-state x))
      ((jolt-delay? x) (jolt-delay-force x))
      ;; a record/reify implementing clojure.lang.IDeref: @x calls its `deref`
      ;; method with the value itself as the leading `this`.
      ((and (jrec? x) (find-method-any-protocol (jrec-tag x) "deref"))
       => (lambda (m) (jolt-invoke m x)))
      ((and (reified-methods x) (hashtable-ref (reified-methods x) "deref" #f))
       => (lambda (m) (jolt-invoke m x)))
      (else (apply %pre-conc-deref x opts)))))

;; realized? for a future/promise/delay. Wrapped over the overlay version in
;; post-prelude.ss.
(define (jolt-conc-realized? x)
  (cond ((jolt-future? x) (jolt-future-done? x))
        ((jolt-promise? x) (jolt-promise-delivered? x))
        ((jolt-delay? x) (jolt-delay-realized? x))
        (else #f)))

;; --- bind into clojure.core -------------------------------------------------
(def-var! "clojure.core" "future-call" jolt-future-call)
(def-var! "clojure.core" "future-cancel" jolt-future-cancel)
(def-var! "clojure.core" "future?" jolt-future?)
(def-var! "clojure.core" "future-done?" jolt-native-future-done?)
(def-var! "clojure.core" "future-cancelled?" jolt-native-future-cancelled?)
(def-var! "clojure.core" "promise" jolt-promise-new)
(def-var! "clojure.core" "deliver" jolt-deliver)
(def-var! "clojure.core" "agent" jolt-agent-new)
(def-var! "clojure.core" "agent?" jolt-agent?)
(def-var! "clojure.core" "send" jolt-agent-send)
(def-var! "clojure.core" "send-off" jolt-agent-send)
(def-var! "clojure.core" "await" jolt-agent-await)
(def-var! "clojure.core" "agent-error" jolt-agent-error)
(def-var! "clojure.core" "restart-agent" jolt-agent-restart)
(def-var! "clojure.core" "make-delay" jolt-make-delay)
(def-var! "clojure.core" "delay?" jolt-delay?)
(def-var! "clojure.core" "deref" jolt-deref)

;; --- object monitors (locking) ----------------------------------------------
;; (locking obj body…) takes obj's monitor for the body — a real per-object lock
;; now that futures/agents/threads share one heap. Each object gets a recursive
;; Chez mutex (a thread may re-enter a monitor it already holds, like the JVM),
;; held in an identity-keyed weak table so monitors are reclaimed with their
;; objects. dynamic-wind releases on normal, exceptional, and continuation exit.
(define monitor-table (make-weak-eq-hashtable))
(define monitor-table-lock (make-mutex))
(define (object-monitor obj)
  (with-mutex monitor-table-lock
    (or (hashtable-ref monitor-table obj #f)
        (let ((m (make-mutex))) (hashtable-set! monitor-table obj m) m))))
(define (jolt-with-monitor obj thunk)
  (let ((m (object-monitor obj)))
    (dynamic-wind
      (lambda () (mutex-acquire m))
      thunk
      (lambda () (mutex-release m)))))
(def-var! "jolt.host" "with-monitor" jolt-with-monitor)

;; --- cooperative thread interrupt -------------------------------------------
;; Chez has no force-kill, but its engine timer (set-timer + timer-interrupt-
;; handler, thread-local) is polled at procedure-call / loop back-edges — so a
;; running computation, even a tight Scheme loop, can be aborted from another
;; thread. An interrupt TOKEN is a shared box; run-interruptible arms a periodic
;; timer in the eval thread whose handler escapes (via call/cc) when the token is
;; set; interrupt! sets the token from any thread. The aborted eval throws a jolt
;; ex-info {:jolt/interrupted true}, so the thread is REUSED, not abandoned.
;;
;; Caveat: a thread blocked in a __collect_safe foreign call (socket recv/accept,
;; sleep) only sees the interrupt when it returns to Scheme — like the JVM not
;; killing native code.
(define interrupt-check-ticks 100000)   ; ~poll interval; responsive + low overhead
(define interrupt-sentinel (cons 'jolt 'interrupted))
(define jolt-kw-interrupted (keyword "jolt" "interrupted"))
(define (jolt-make-interrupt) (box #f))
(define (jolt-interrupt! token) (when (box? token) (set-box! token #t)) jolt-nil)
(define (jolt-interrupted? token) (and (box? token) (unbox token) #t))
(define (jolt-run-interruptible token thunk)
  (let ((prev-handler (timer-interrupt-handler)))
    (let ((r (call/cc
               (lambda (k)
                 (timer-interrupt-handler
                   (lambda ()
                     (if (and (box? token) (unbox token))
                         (k interrupt-sentinel)
                         (begin (set-timer interrupt-check-ticks) (void)))))
                 (set-timer interrupt-check-ticks)
                 (let ((v (thunk))) (set-timer 0) v)))))
      ;; restore the prior timer state regardless of outcome.
      (set-timer 0)
      (timer-interrupt-handler prev-handler)
      (if (eq? r interrupt-sentinel)
          (jolt-throw (jolt-ex-info "Evaluation interrupted" (jolt-hash-map jolt-kw-interrupted #t)))
          r))))
(def-var! "jolt.host" "make-interrupt" jolt-make-interrupt)
(def-var! "jolt.host" "interrupt!" jolt-interrupt!)
(def-var! "jolt.host" "interrupted?" jolt-interrupted?)
(def-var! "jolt.host" "run-interruptible" jolt-run-interruptible)

;; --- java.lang.Thread / java.util.concurrent.CountDownLatch -----------------
;; Real OS threads over Chez fork-thread (shared heap — a captured atom/var is
;; shared). A Thread runs its Runnable thunk; start forks, join waits on a
;; condition latched at completion. CountDownLatch is a counting barrier.
(define (make-jthread thunk) (make-jhost "user-thread" (vector thunk #f (make-mutex) (make-condition))))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (thunk . _) (make-jthread thunk))))
          '("Thread" "java.lang.Thread"))
(register-host-methods! "user-thread"
  (list (cons "start" (lambda (self)
          (let ((st (jhost-state self)) (snap (dyn-binding-stack)))
            (fork-thread (lambda ()
              (dyn-binding-stack snap)
              (guard (e (#t #f)) (jolt-invoke (vector-ref st 0)))
              (with-mutex (vector-ref st 2)
                (vector-set! st 1 #t)
                (condition-broadcast (vector-ref st 3)))))
            jolt-nil)))
        (cons "run" (lambda (self) (jolt-invoke (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "join" (lambda (self . _)
          (let ((st (jhost-state self)))
            (with-mutex (vector-ref st 2)
              (let loop () (unless (vector-ref st 1) (condition-wait (vector-ref st 3) (vector-ref st 2)) (loop)))))
          jolt-nil))
        (cons "isAlive" (lambda (self) (not (vector-ref (jhost-state self) 1))))
        (cons "interrupt" (lambda (self . _) jolt-nil))
        (cons "setDaemon" (lambda (self . _) jolt-nil))))

(define (make-jlatch n) (make-jhost "count-down-latch" (vector n (make-mutex) (make-condition))))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (n . _) (make-jlatch (jnum->exact n)))))
          '("CountDownLatch" "java.util.concurrent.CountDownLatch"))
(register-host-methods! "count-down-latch"
  (list (cons "countDown" (lambda (self)
          (let ((st (jhost-state self)))
            (with-mutex (vector-ref st 1)
              (when (> (vector-ref st 0) 0) (vector-set! st 0 (- (vector-ref st 0) 1)))
              (when (= (vector-ref st 0) 0) (condition-broadcast (vector-ref st 2)))))
          jolt-nil))
        (cons "await" (lambda (self . _)
          (let ((st (jhost-state self)))
            (with-mutex (vector-ref st 1)
              (let loop () (when (> (vector-ref st 0) 0) (condition-wait (vector-ref st 2) (vector-ref st 1)) (loop)))))
          jolt-nil))
         (cons "getCount" (lambda (self) (vector-ref (jhost-state self) 0)))))

;; --- main-thread executor ---------------------------------------------------
;; Lets a worker thread (e.g. an nREPL eval future) run a thunk on the thread
;; that owns the GUI main loop. On macOS GTK quartz, g_application_run must run
;; on the process main thread or AppKit aborts (setMainMenu off-main → SIGABRT).
;; Under `joltc nrepl` the accept loop is backgrounded in a future and the
;; primordial thread enters jolt-run-main-pump; glimmer's run marshals its
;; startup through jolt-call-on-main-thread.
;;
;; - With no pump running (`joltc -M:run` calls run directly on the main thread),
;;   call-on-main-thread runs the thunk INLINE — unchanged behaviour.
;; - A call from a thunk already executing on the pump runs inline too, so the
;;   pump can't deadlock on itself.
;; - Otherwise the thunk is enqueued; the caller blocks until the pump runs it,
;;   then receives the value, or the thrown condition is re-raised.
;;
;; stop-main-pump is the graceful-shutdown / external API: it tells the pump to
;; drain whatever is queued and return. The pump-active flag is flipped to #f
;; under jolt-main-queue-mu in the same critical section that decides to exit, and
;; call-on-main-thread reads that flag and enqueues under the SAME mutex, so a job
;; can never slip in after the pump has decided to leave — a call that loses the
;; race simply runs inline instead of blocking forever on a pump that is gone.

(define jolt-main-queue-mu (make-mutex))
(define jolt-main-queue-cv (make-condition))
(define jolt-main-queue '())            ; FIFO of jolt-main-job, guarded by mu
(define jolt-main-pump-active (box #f)) ; #t while run-main-pump owns this thread
(define jolt-main-pump-stop (box #f))   ; set by stop-main-pump to drain + exit
;; thread-local: this thread is the pump, mid-thunk → nested calls run inline.
(define jolt-in-main-pump? (make-thread-parameter #f))

(define-record-type jolt-main-job
  (fields thunk (mutable done?) (mutable ok?) (mutable val) mu cv)
  (nongenerative jolt-main-job-v1))

(define (jolt-call-on-main-thread thunk)
  (if (jolt-in-main-pump?)              ; reentrant — already on the pump
      (jolt-invoke thunk)
      ;; Decide-and-enqueue atomically: read pump-active and (if active) push the
      ;; job under jolt-main-queue-mu, the same lock the pump holds when it flips
      ;; active to #f on exit. So we either get queued before the pump leaves, or
      ;; we see #f and fall through to inline — never enqueue onto a dead pump.
      (let ((job (with-mutex jolt-main-queue-mu
                   (and (unbox jolt-main-pump-active)
                        (let ((j (make-jolt-main-job thunk #f #f jolt-nil
                                                     (make-mutex) (make-condition))))
                          (set! jolt-main-queue (append jolt-main-queue (list j)))
                          (condition-signal jolt-main-queue-cv)
                          j)))))
        (if (not job)
            (jolt-invoke thunk)         ; no pump (or stopped) — inline, like -M:run
            (begin
              (with-mutex (jolt-main-job-mu job)
                (let wait ()
                  (unless (jolt-main-job-done? job)
                    (condition-wait (jolt-main-job-cv job) (jolt-main-job-mu job))
                    (wait))))
              (if (jolt-main-job-ok? job)
                  (jolt-main-job-val job)
                  (raise (jolt-main-job-val job))))))))

(define jolt-pump-kih
  (lambda ()
    (for-each (lambda (th) (guard (e (#t #f)) (th)))
              (reverse (unbox jolt-shutdown-hooks)))
    (exit 0)))

;; Park the calling thread until a keyboard interrupt (^C), then run the shutdown
;; hooks and exit. Unlike run-main-pump (whose tight recursive condition-wait
;; loop elides Chez's interrupt poll points, so the handler never fires), this
;; uses a single condition-wait — the form Chez reliably interrupts. The nREPL
;; server parks here; SIGINT is unblocked in this thread first (it was masked by
;; jolt-block-sigint so the accept loop inherited a blocked mask and couldn't
;; absorb ^C in its foreign accept() call).
(define jolt-park-mu (make-mutex))
(define jolt-park-cv (make-condition))
(define (jolt-park-until-interrupt)
  (keyboard-interrupt-handler jolt-pump-kih)
  (jolt-set-sigint-blocked #f)
  (with-mutex jolt-park-mu (condition-wait jolt-park-cv jolt-park-mu))
  jolt-nil)

(define (jolt-run-main-pump)
  (with-mutex jolt-main-queue-mu
    (set-box! jolt-main-pump-stop #f)
    (set-box! jolt-main-pump-active #t))
  ;; dynamic-wind guarantees active is cleared even if the pump escapes abnormally,
  ;; so a later run-main-pump starts clean and call-on-main-thread never sees a
  ;; stale #t. The clean-exit path below also clears it under the mutex (the flip
  ;; that races call-on-main-thread); this is the belt-and-suspenders for escapes.
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (let loop ()
        (let ((job (with-mutex jolt-main-queue-mu
                     (let wait ()
                       (cond
                         ((not (null? jolt-main-queue))
                          (let ((j (car jolt-main-queue)))
                            (set! jolt-main-queue (cdr jolt-main-queue))
                            j))
                         ((unbox jolt-main-pump-stop)
                          ;; drain done, told to exit — clear active in the same
                          ;; critical section so no job can be enqueued after.
                          (set-box! jolt-main-pump-active #f)
                          #f)
                         (else (condition-wait jolt-main-queue-cv jolt-main-queue-mu)
                               (wait)))))))
          (when job
            (let ((r (dynamic-wind
                       (lambda () (jolt-in-main-pump? #t))
                       (lambda ()
                         (guard (e (#t (cons #f e)))
                           (cons #t (jolt-invoke (jolt-main-job-thunk job)))))
                       (lambda () (jolt-in-main-pump? #f)))))
              (with-mutex (jolt-main-job-mu job)
                (jolt-main-job-ok?-set! job (car r))
                (jolt-main-job-val-set! job (cdr r))
                (jolt-main-job-done?-set! job #t)
                (condition-broadcast (jolt-main-job-cv job))))
            (loop)))))
    (lambda ()
      (with-mutex jolt-main-queue-mu (set-box! jolt-main-pump-active #f))))
  jolt-nil)

(define (jolt-stop-main-pump)
  (with-mutex jolt-main-queue-mu
    (set-box! jolt-main-pump-stop #t)
    (condition-broadcast jolt-main-queue-cv))
  jolt-nil)

;; Shutdown hooks run by jolt-pump-kih (the keyboard-interrupt-handler installed by
;; park-until-interrupt) before (exit 0), so a foreground server (nREPL) can close
;; its socket and drop .nrepl-port on ^C instead of Chez's default mutex-corrupting
;; abort. Newest-first; each hook is isolated so one failing hook can't block the exit.
(define jolt-shutdown-hooks (box '()))
(define (jolt-add-shutdown-hook thunk)
  (set-box! jolt-shutdown-hooks (cons thunk (unbox jolt-shutdown-hooks)))
  jolt-nil)

;; Per-thread SIGINT mask. A worker thread parked in a foreign call (the nREPL
;; accept loop in c-accept, or a conn handler) can't run Chez's keyboard-interrupt
;; handler on ^C, so if SIGINT is delivered there the process hangs. Block SIGINT
;; in the primordial thread BEFORE forking such workers (they inherit the mask),
;; then park-until-interrupt unblocks it in the primordial once its handler is
;; installed, so ^C is always delivered to the parked thread. pthread_sigmask/
;; sigaddset are libc/libpthread symbols, resolvable once the process object is
;; loaded (as the socket fns already are). 128 bytes covers Linux's 1024-bit
;; sigset_t and is larger than macOS's 4-byte one.
;; foreign-procedure resolves its symbol eagerly, and these POSIX signal fns don't
;; exist on Windows — resolving them unguarded aborted startup ("no entry for
;; pthread_sigmask"). Guard so a non-POSIX host yields #f; jolt-set-sigint-blocked
;; then no-ops (Windows delivers ^C through the console, not a per-thread mask).
(define c-pthread-sigmask
  (guard (e (#t #f)) (foreign-procedure "pthread_sigmask" (int u8* u8*) int)))
(define c-sigemptyset (guard (e (#t #f)) (foreign-procedure "sigemptyset" (u8*) int)))
(define c-sigaddset (guard (e (#t #f)) (foreign-procedure "sigaddset" (u8* int) int)))
;; POSIX SIG_BLOCK/SIG_UNBLOCK numerics differ by platform: Linux/glibc 0/1,
;; Darwin/macOS 1/2 (SIG_UNBLOCK is SIG_BLOCK+1 on both). Resolve SIG_BLOCK for
;; this host from the machine-type symbol — macOS builds contain "osx".
(define jolt-sig-block-how
  (let* ((s (symbol->string (machine-type)))
         (n (string-length s)))
    (let loop ((i 0))
      (cond
        ((> (+ i 3) n) 0)                              ; default: Linux/glibc
        ((string=? (substring s i (+ i 3)) "osx") 1)   ; Darwin/macOS
        (else (loop (+ i 1)))))))
(define (jolt-set-sigint-blocked block?)
  (when (and c-pthread-sigmask c-sigemptyset c-sigaddset)
    (let ((set (make-bytevector 128 0))
          (old (make-bytevector 128 0)))
      (c-sigemptyset set)
      (c-sigaddset set 2)                          ; SIGINT = 2
      (c-pthread-sigmask (if block? jolt-sig-block-how (+ jolt-sig-block-how 1)) set old)))
  jolt-nil)

(def-var! "jolt.host" "call-on-main-thread" jolt-call-on-main-thread)
(def-var! "jolt.host" "run-main-pump" jolt-run-main-pump)
(def-var! "jolt.host" "stop-main-pump" jolt-stop-main-pump)
(def-var! "jolt.host" "add-shutdown-hook" jolt-add-shutdown-hook)
(def-var! "jolt.host" "block-sigint" (lambda () (jolt-set-sigint-blocked #t)))
(def-var! "jolt.host" "park-until-interrupt" jolt-park-until-interrupt)
(def-var! "jolt.host" "delete-file" delete-file)
