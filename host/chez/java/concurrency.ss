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
;; The duration is ms to the nanosecond, rounded up: ms may be a fraction (what is
;; left of an exact deadline, a TimeUnit finer than a millisecond), and flooring it
;; turned a pause of 0.4ms into none at all and a poll loop into a spin.
(define (ms->duration ms)
  (wait-ns->duration (exact (ceiling (* ms 1000000)))))
;; ms from now as a deadline on mono-millis, the form jolt-cv-wait and the timer
;; take: a fiber's deadline has to be comparable against the clock and against the
;; entries in the shared timer, and a thread's wait is converted from it. One
;; clock, so the two cannot drift apart — and the monotonic one, so a wall-clock
;; step does not move a deadline.
(define (ms->deadline-millis ms) (mono-deadline ms))
;; ms as the timeout of ONE timed wait (jolt-timeout, host/chez/locks.ss): it
;; starts when the wait first has to wait, so a wait that answers at once reads no
;; clock. A plain number of milliseconds is a long to the JVM, so a double is
;; truncated; tu->ms's exact answer for a finer TimeUnit is kept.
(define (ms->timeout ms)
  (jolt-timeout (if (and (number? ms) (exact? ms)) ms (jnum->exact ms))))

;; --- java.util.concurrent.TimeUnit ------------------------------------------
;; A TimeUnit is a scale, so each constant is an object carrying its name and its
;; size in nanoseconds; every method taking a (timeout, unit) pair converts through
;; tu->ms. Nothing modeled TimeUnit before, so none of those methods could be CALLED
;; with a unit at all — which is how each of them went so long with its timeout
;; argument quietly discarded. Adding the type makes them reachable, so they are all
;; fixed below rather than left as newly-reachable silent no-ops.
(define time-unit-nanos
  '(("NANOSECONDS" . 1) ("MICROSECONDS" . 1000) ("MILLISECONDS" . 1000000)
    ("SECONDS" . 1000000000) ("MINUTES" . 60000000000)
    ("HOURS" . 3600000000000) ("DAYS" . 86400000000000)))
(define (make-time-unit nm nanos) (make-jhost "time-unit" (vector nm nanos)))
(define (time-unit? x) (and (jhost? x) (string=? (jhost-tag x) "time-unit")))
(define (time-unit-scale u) (vector-ref (jhost-state u) 1))
;; JVM conversions TRUNCATE toward zero (SECONDS.toMinutes(90) is 1) and
;; saturate at Long's range (DAYS.toMicros(Long.MAX_VALUE) is Long.MAX_VALUE);
;; tu-saturate is below, read only when a conversion runs.
(define (tu-convert self n per)
  (tu-saturate (quotient (* (jnum->exact n) (time-unit-scale self)) per)))

;; (amount, unit) -> milliseconds. A missing or non-TimeUnit unit leaves the amount
;; alone, which is the plain-milliseconds shape awaitTermination has always taken.
;; EXACT, not truncated to a whole millisecond: (.poll q 500 MICROSECONDS) is half
;; a millisecond, where the quotient made it no wait at all.
(define (tu->ms amount unit)
  (let ((n (if (number? amount) (jnum->exact amount) 0)))
    (if (time-unit? unit)
        ;; a unit of a millisecond or more scales by a whole number, which skips
        ;; the rational division the polls with a timeout would pay on every call
        (let ((scale (time-unit-scale unit)))
          (if (fx>= scale 1000000)
              (* n (fxquotient scale 1000000))
              (/ (* n scale) 1000000)))
        n)))

;; The timeout of a (… timeout unit) argument tail, in ms, or #f when absent — the
;; untimed overload of the same method.
(define (tu-args->ms args)
  (if (null? args) #f (tu->ms (car args) (if (null? (cdr args)) #f (cadr args)))))

(define time-unit-constants
  (map (lambda (p) (cons (car p) (make-time-unit (car p) (cdr p)))) time-unit-nanos))
(register-enum-class! "java.util.concurrent.TimeUnit" time-unit-constants)
;; TimeUnit <-> java.time.temporal.ChronoUnit (the java.time base, autoloaded)
(define time-unit-chrono-names
  '(("NANOSECONDS" . "NANOS") ("MICROSECONDS" . "MICROS") ("MILLISECONDS" . "MILLIS")
    ("SECONDS" . "SECONDS") ("MINUTES" . "MINUTES") ("HOURS" . "HOURS") ("DAYS" . "DAYS")))
(define (time-unit-of-chrono cu)
  (when (jolt-nil? cu) (throw-jvm 'NullPointerException "chronoUnit"))
  (let* ((nm (jolt-final-str (record-method-dispatch cu "name" jolt-nil)))
         (hit (find (lambda (p) (string=? (cdr p) nm)) time-unit-chrono-names)))
    (if hit
        (cdr (assoc (car hit) time-unit-constants))
        ;; named by the ChronoUnit's toString (Weeks), as TimeUnit.of words it
        (throw-jvm 'IllegalArgumentException
                   (string-append "No TimeUnit equivalent for " (jolt-final-str (record-method-dispatch cu "toString" jolt-nil)))))))
;; convert(duration, unit) and convert(Duration): truncated toward zero and
;; saturated at Long's range, as TimeUnit.convert is
(define (tu-saturate n)
  (cond ((> n 9223372036854775807) 9223372036854775807)
        ((< n -9223372036854775808) -9223372036854775808)
        (else n)))
(define (tu-from-nanos self nanos) (tu-saturate (truncate (/ nanos (time-unit-scale self)))))
(register-class-statics! "java.util.concurrent.TimeUnit" (list (cons "of" time-unit-of-chrono)))
(register-host-methods! "time-unit"
  (list (cons "toNanos"   (lambda (self n) (tu-convert self n 1)))
        (cons "toMicros"  (lambda (self n) (tu-convert self n 1000)))
        (cons "toMillis"  (lambda (self n) (tu-convert self n 1000000)))
        (cons "toSeconds" (lambda (self n) (tu-convert self n 1000000000)))
        (cons "toMinutes" (lambda (self n) (tu-convert self n 60000000000)))
        (cons "toHours"   (lambda (self n) (tu-convert self n 3600000000000)))
        (cons "toDays"    (lambda (self n) (tu-convert self n 86400000000000)))
        ;; the same interruptible sleep Thread/sleep takes (java/host-static-methods.ss)
        ;; exact, not tu->ms's whole milliseconds: a sub-millisecond sleep is
        ;; rounded up by the sleep, never down to nothing. A timeout <= 0 does
        ;; nothing at all, as TimeUnit.sleep's own (timeout > 0) guard has it: no
        ;; yield, and no InterruptedException for an already-interrupted thread.
        (cons "sleep"     (lambda (self n)
                            (let ((n (jnum->exact n)))
                              (if (positive? n)
                                  (jolt-thread-sleep-ms
                                    (if (time-unit? self)
                                        (/ (* n (time-unit-scale self)) 1000000)
                                        n))
                                  jolt-nil))))
        (cons "name"      (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "convert"
              (case-lambda
                ((self d)
                 (let ((secs (jnum->exact (record-method-dispatch d "getSeconds" jolt-nil)))
                       (nano (jnum->exact (record-method-dispatch d "getNano" jolt-nil))))
                   (tu-from-nanos self (+ (* secs 1000000000) nano))))
                ((self n unit)
                 (tu-from-nanos self (* (jnum->exact n) (time-unit-scale unit))))))
        ;; timedJoin / timedWait: Thread.join / Object.wait for a timeout in
        ;; this unit; one of zero or less does nothing, as the JDK's guard has it
        (cons "timedJoin" (lambda (self th n)
                            (let ((ms (tu-convert self n 1000000)))
                              (when (positive? (jnum->exact n))
                                (record-method-dispatch th "join" (jolt-list (max ms 1))))
                              jolt-nil)))
        (cons "timedWait" (lambda (self o n)
                            (let ((ms (tu-convert self n 1000000)))
                              (when (positive? (jnum->exact n))
                                (record-method-dispatch o "wait" (jolt-list (max ms 1))))
                              jolt-nil)))
        (cons "toChronoUnit"
              (lambda (self)
                (host-static-ref "java.time.temporal.ChronoUnit"
                                 (cdr (assoc (vector-ref (jhost-state self) 0) time-unit-chrono-names)))))))

;; --- futures ----------------------------------------------------------------
;; A future is a mutable cell guarded by `mu`; workers/derefs coordinate on `cv`.
;;   done?       — result (or cancellation) is final; derefs may proceed
;;   cancelled?  — future-cancel won before the body finished
;;   ok?         — payload is a value (else payload is a raised condition/value)
;;   payload     — the result value, or the captured throw
;;   ibox        — the WORKER's interrupt box, so future-cancel can interrupt the
;;                 running body the way the JVM's cancel(true) does. Allocated by
;;                 future-call and adopted by the worker, not read out of it after
;;                 the fork: a cancel that lands before the worker has run a line
;;                 must still reach it.
;;
;; The tag is jolt-future-v2 and not -v1 because a jolt-future is IMAGE-FORMAT
;; surface (jolt.image round-trips record values by nongenerative tag), so adding a
;; field to the old tag would let a new image read as an old record. `make
;; stateimage` is the gate.
(define-record-type jolt-future
  (fields (mutable done?) (mutable cancelled?) (mutable ok?) (mutable payload) mu cv ibox)
  (nongenerative jolt-future-v2))

;; (future-call thunk): run (thunk) on the solo pool (below). The dynamic
;; bindings and current namespace in effect now are conveyed into the task as a
;; snapshot, and the worker gets its own back afterwards, so an in-ns inside one
;; future does not leak into the next task the pooled thread runs. The result —
;; value or thrown condition — is latched and broadcast; a cancel that already
;; finalized the future makes the late result a no-op, and a cancel that lands
;; before a worker takes the task keeps the body from running at all, as a
;; FutureTask cancelled before run() never runs.
;; A future runs on the agent system's cached pool on the JVM, so after
;; shutdown-agents it is refused.
(define (jolt-future-call thunk)
  (when (jolt-agents-shutdown?)
    (jolt-throw (jolt-host-throwable "java.util.concurrent.RejectedExecutionException" jolt-nil)))
  (let* ((ibox (box #f))
         (f (make-jolt-future #f #f #f jolt-nil (make-mutex) (jolt-make-cv) ibox))
         (snap (dyn-binding-stack))
         (ns (chez-current-ns-param)))
    (solo-pool-run!
     (lambda ()
       (unless (jolt-future-settled? f)              ; cancelled before it started
         (*txn* #f)                          ; the task must not inherit a txn
         (rdr-default-modes!)                ; and not the reader modes of a read it forked from
         ;; The worker's flag is the future's flag while the body runs:
         ;; future-cancel sets it, and (Thread/currentThread) inside the body
         ;; hands back a handle onto the same box, so .isInterrupted /
         ;; Thread/interrupted inside the worker and the cancel from outside are
         ;; ONE flag (the shape jolt#727 fixed for Thread.). The worker takes its
         ;; own flag back when the body is done.
         (let ((own (current-os-thread-box))
               (own-ns (chez-current-ns-param)))
           (adopt-interrupt-box! ibox)
           (dyn-binding-stack snap)
           (chez-current-ns-param ns)
           (let* ((slot (agent-pool-take! 'send-off))
                  (_ (jolt-thread-name-set! (get-thread-id) (agent-pool-thread-name 'send-off slot)))
                  (r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
             (agent-pool-give! 'send-off slot)
             (dyn-binding-stack '())
             (chez-current-ns-param own-ns)
             (adopt-interrupt-box! own)
             (jolt-with-mutex (jolt-future-mu f)
               (unless (jolt-future-done? f)            ; not already cancelled
                 (jolt-future-ok?-set! f (car r))
                 (jolt-future-payload-set! f (cdr r))
                 (jolt-future-done?-set! f #t))
               (jolt-cv-wake! (jolt-future-cv f))))))))
    f))

;; The STM's waits (refs.ss): a transaction blocked by another one's claim waits
;; on that one's latch, a mutex and a jolt-cv, for a bounded time; the owner
;; releases it when it finishes or is killed. The clock times barging.
(set! stm-now-ms (lambda () (quotient (mono-nanos) 1000000)))
(set! stm-raise jolt-raise-thrown)
(set! stm-make-latch (lambda () (vector (make-mutex) (jolt-make-cv) #f)))
(set! stm-latch-wait
  (lambda (l ms)
    (jolt-cv-wait (vector-ref l 0) (vector-ref l 1) (mono-deadline ms)
      (lambda (timed-out?) (if (or timed-out? (vector-ref l 2)) #t jolt-cv-again)))))
(set! stm-latch-release!
  (lambda (l)
    (jolt-with-mutex (vector-ref l 0)
      (vector-set! l 2 #t)
      (jolt-cv-wake! (vector-ref l 1)))))

;; Wrap a task's captured throw in an ExecutionException, the original as the
;; cause so ex-cause and .getCause both reach it. One helper for every place a
;; task's throw surfaces — a clojure `future`'s deref just below, and the .get of
;; either Future shim (the ExecutorService's and FutureTask's, further down) —
;; because the JVM makes a caller catch ExecutionException at all of them.
(define (task-execution-exception e)
  (let ((cause (jolt-unwrap-throw e)))
    (jolt-host-throwable "java.util.concurrent.ExecutionException"
                         (jolt-str-render-one cause)
                         cause)))

;; Final value of a settled future (called OUTSIDE the lock): wrap a captured
;; throw in an ExecutionException (JVM semantics), signal a cancellation, else
;; the value. The original exception is stored as the cause so ex-cause works.
(define (jolt-future-finish f)
  (cond
    ;; CancellationException, certified against JVM Clojure 1.12 — @a-cancelled-future
    ;; raises java.util.concurrent.CancellationException there, where jolt raised a
    ;; bare ExceptionInfo, so (catch java.util.concurrent.CancellationException _ …)
    ;; around a deref did not catch on jolt.
    ((jolt-future-cancelled? f)
     (jolt-throw (jolt-host-throwable "java.util.concurrent.CancellationException"
                                      "Future cancelled")))
    ((jolt-future-ok? f) (jolt-future-payload f))
    (else (jolt-throw (task-execution-exception (jolt-future-payload f))))))

;; jolt-cv-wait and not a bare condition-wait, here and at every other wait in this
;; file: @a-future from a go block used to block the fiber's CARRIER, so every other
;; fiber on it stopped until the future settled, and if the thing that would settle
;; it was itself a fiber on that carrier the two deadlocked (jolt-x1no). A fiber
;; parks now and the settle resumes it; a thread still blocks, which is what a
;; thread should do.
;;
;; SPIN BRIEFLY FIRST. A deref of a future still running, or one a worker has
;; not reached yet, is usually answered within a few microseconds, and parking
;; costs more than that: the waker's broadcast, the kernel's wake and the
;; reschedule. The JVM's FutureTask yields before it parks for the same reason,
;; and its handoff queues spin. task-spin-until reads the flag WITHOUT the mutex,
;; so it is only a hint of when to take it: a #t from it is followed by
;; jolt-future-settled? under the mutex, which is what orders the payload's
;; read after the worker's write. A fiber does not spin; it parks, so its carrier
;; runs the other fibers instead.
(define task-spin-rounds 64)
(define (task-spin-until done?)
  (and (not (jolt-current-fiber))
       (let loop ((i 0))
         (cond ((done?) #t)
               ((fx=? i task-spin-rounds) #f)
               (else (thread-yield!) (loop (fx+ i 1)))))))

;; A settled one answers without the wait. The wait begins by checking that the
;; caller may park (jolt-fiber-may-park!, which must precede the commit), and a
;; fiber holding a counted lock may not, so @ on a future or promise that had
;; long since settled threw there, from inside a channel's close! (a
;; transducer's completion) say, although nothing would wait. The settled
;; check takes only the value's own mutex, which never parks.
(define (jolt-future-settled? f)
  (jolt-with-mutex (jolt-future-mu f) (jolt-future-done? f)))

(define (jolt-future-deref f)
  (unless (begin (task-spin-until (lambda () (jolt-future-done? f)))
                 (jolt-future-settled? f))
    (jolt-cv-wait-interruptibly "future deref" (jolt-future-mu f) (jolt-future-cv f) #f
      (lambda (_timed-out?)
        (if (jolt-future-done? f) #t jolt-cv-again))))
  (jolt-future-finish f))

;; (deref f timeout-ms timeout-val): wait up to timeout-ms; return timeout-val if
;; it has not settled by the absolute deadline.
(define (jolt-future-deref-timed f ms timeout-val)
  (let ((settled (or (jolt-future-settled? f)
                     (jolt-cv-wait-interruptibly "future deref"
                                                 (jolt-future-mu f) (jolt-future-cv f)
                                                 (ms->timeout ms)
                       (lambda (timed-out?)
                         ;; done? FIRST on both paths: a future that settled as the
                         ;; deadline passed is not a timeout, which is the re-check the
                         ;; old (else (jolt-future-done? f)) arm was there for.
                         (cond ((jolt-future-done? f) #t)
                               (timed-out? #f)
                               (else jolt-cv-again)))))))
    (if settled (jolt-future-finish f) timeout-val)))

;; future-cancel: mark the future cancelled+done if it has not settled — so derefs
;; raise and the predicates flip — and INTERRUPT the worker, which is what
;; clojure.core/future-cancel does on the JVM (it calls cancel(true)). A worker
;; blocked in an interruptible wait is thrown out of it; one running compute sees
;; the flag through Thread/interrupted or .isInterrupted, the same as on the JVM,
;; which cannot stop a computation either. Returns true iff this call cancelled it.
;;
;; The interrupt is delivered OUTSIDE the future's mutex. jolt-interrupt-wake-waits!
;; takes the registry's mutex and then each waiter's, and the worker may itself be
;; waiting on something; doing that under mu here would order this future's mutex
;; above the whole set of them for no reason.
(define (jolt-future-cancel f . interrupt?)
  (let ((cancelled (jolt-with-mutex (jolt-future-mu f)
                     (if (jolt-future-done? f)
                         #f
                         (begin (jolt-future-cancelled?-set! f #t)
                                (jolt-future-done?-set! f #t)
                                (jolt-cv-wake! (jolt-future-cv f))
                                #t)))))
    ;; Future.cancel(false) cancels without interrupting the worker
    (when (and cancelled (or (null? interrupt?) (car interrupt?)))
      (let ((b (jolt-future-ibox f)))
        (set-box! b #t)
        (jolt-interrupt-wake-waits! b)))
    cancelled))


;; future? / future-done? / future-cancelled? / future-cancel over ANY
;; java.util.concurrent.Future, as clojure.core defines them on the JVM:
;; (instance? Future x), .isDone, .isCancelled and .cancel(true). A jolt future
;; answers from its record; a FutureTask, an executor's future, a
;; ScheduledFutureTask or a CompletableFuture (or a reify of Future) through its
;; own methods. They used to accept the jolt future only, so (future? a-FutureTask)
;; was false and future-done? threw on one. Anything that is not a Future is the
;; JVM's failed cast.
(define future-type-sym (jolt-symbol #f "java.util.concurrent.Future"))
(define (jolt-java-future? x)
  (let ((r (instance-check future-type-sym x))) (and r (not (jolt-nil? r)))))
(define (jolt-any-future? x) (or (jolt-future? x) (jolt-java-future? x)))
(define (future-cast-error x)
  (jolt-throw
   (jolt-host-throwable "java.lang.ClassCastException"
     (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                    " cannot be cast to class java.util.concurrent.Future"))))
(define (future-method x name . args)
  (record-method-dispatch x name (if (null? args) jolt-nil (list->cseq args))))
(define (jolt-native-future-done? x)
  (cond ((jolt-future? x) (jolt-future-done? x))
        ((jolt-java-future? x) (jolt-truthy? (future-method x "isDone")))
        (else (future-cast-error x))))
(define (jolt-native-future-cancelled? x)
  (cond ((jolt-future? x) (jolt-future-cancelled? x))
        ((jolt-java-future? x) (jolt-truthy? (future-method x "isCancelled")))
        (else (future-cast-error x))))
(define (jolt-any-future-cancel x)
  (cond ((jolt-future? x) (jolt-future-cancel x))
        ((jolt-java-future? x) (jolt-truthy? (future-method x "cancel" #t)))
        (else (future-cast-error x))))

;; --- promises ---------------------------------------------------------------
;; A blocking promise (like the JVM): deref parks until deliver, then caches the
;; value. deliver wins once; later delivers return nil.
(define-record-type jolt-promise
  (fields (mutable delivered?) (mutable value) mu cv)
  (nongenerative jolt-promise-v1))

(define (jolt-promise-new) (make-jolt-promise #f jolt-nil (make-mutex) (jolt-make-cv)))

;; (class a-future)/(class a-promise): both are anonymous reify instances of a
;; clojure.core fn on the JVM — clojure.core$future_call$reify__N /
;; clojure.core$promise$reify__N. The __N counter is unstable per eval; jolt
;; matches the stable enclosing-fn prefix and pins __0 (rather than :object).
(register-class-arm! jolt-future? (lambda (f) "clojure.core$future_call$reify__0"))
(register-class-arm! jolt-promise? (lambda (p) "clojure.core$promise$reify__0"))
;; ...and what those reifies implement. A row is needed: to the class graph a
;; $-name with no row is a fn (AFunction), which neither of these is.
(jch-register-supers! "clojure.core$future_call$reify__0"
  '("clojure.lang.IDeref" "clojure.lang.IBlockingDeref" "clojure.lang.IPending"
    "java.util.concurrent.Future"))
(jch-register-supers! "clojure.core$promise$reify__0"
  '("clojure.lang.IDeref" "clojure.lang.IBlockingDeref" "clojure.lang.IPending"
    "clojure.lang.IFn"))

(define (jolt-deliver p v)
  (if (jolt-promise? p)
      (let ((won (jolt-with-mutex (jolt-promise-mu p)
                   (if (jolt-promise-delivered? p)
                       #f
                       (begin (jolt-promise-value-set! p v)
                              (jolt-promise-delivered?-set! p #t)
                              (jolt-cv-wake! (jolt-promise-cv p))
                              #t)))))
        (if won p jolt-nil))
      (jolt-throw (jolt-ex-info "deliver requires a promise" (jolt-hash-map)))))

;; A delivered promise answers without the wait, as a settled future does
;; (jolt-future-settled?, above).
(define (jolt-promise-settled? p)
  (jolt-with-mutex (jolt-promise-mu p) (jolt-promise-delivered? p)))

(define (jolt-promise-deref p)
  (unless (jolt-promise-settled? p)
    (jolt-cv-wait-interruptibly "promise deref" (jolt-promise-mu p) (jolt-promise-cv p) #f
      (lambda (_timed-out?)
        (if (jolt-promise-delivered? p) #t jolt-cv-again))))
  (jolt-promise-value p))

(define (jolt-promise-deref-timed p ms timeout-val)
  (let ((got (or (jolt-promise-settled? p)
                 (jolt-cv-wait-interruptibly "promise deref"
                                             (jolt-promise-mu p) (jolt-promise-cv p)
                                             (ms->timeout ms)
                   (lambda (timed-out?)
                     (cond ((jolt-promise-delivered? p) #t)
                           (timed-out? #f)
                           (else jolt-cv-again)))))))
    (if got (jolt-promise-value p) timeout-val)))

;; --- agents (async, per-agent serialized dispatch) --------------------------
;; JVM semantics: send/send-off enqueue an action and a single worker thread
;; applies them to the state IN ORDER; deref reads the (possibly not-yet-updated)
;; state without blocking; await blocks until the queue drains. An action error
;; is handled per the agent's error-mode (:fail halts and stores the error;
;; :continue swallows it and keeps going), with an optional error-handler fired
;; in either mode. Sends made from inside an action are held until it completes.
;; After shutdown-agents, new sends throw RejectedExecutionException.
(define-record-type jolt-agent
  (fields (mutable state) (mutable err) (mutable validator)
          (mutable queue) (mutable running?) mu cv
          (mutable err-mode) (mutable err-handler))
  (nongenerative jolt-agent-v2))

;; A global gate: once shutdown-agents runs, new sends are rejected (running
;; workers still drain their queues). Mirrors the JVM executor shutdown.
(define agents-shutdown? (box #f))
(define (jolt-agents-shutdown?) (unbox agents-shutdown?))
(define (jolt-shutdown-agents)
  (set-box! agents-shutdown? #t)
  (solo-pool-shutdown!)
  (exit-release-pools!)
  jolt-nil)

;; --- the process's exit wait ----------------------------------------------------
;; The JVM ends when the program's main returns only once every live non-daemon
;; thread has finished; clojure.main does nothing else, so a script whose last act
;; starts work on a non-daemon thread waits for it. The live count is kept where
;; every thread is forked (lazy-bridge.ss, exit-nondaemon-live), and a thread's
;; daemon flag is the one Thread.isDaemon answers, so the two cannot disagree.
;;
;; The agent system's pools keep a JVM up after their last task as well,
;; certified with timing probes on JDK 21:
;;
;;   future / future-call / pmap  the cached "solo" pool, which jolt runs (below):
;;       its idle workers are live threads until their 60s keep-alive runs out,
;;       so the process ends 60s after the last task finishes.
;;   send-off  the same pool on the JVM; here an agent's actions get a thread of
;;       their own, so exit-linger-after-task! stands in for the idle worker. It
;;       is called as each run of actions ends, on that thread, and only a
;;       non-daemon one counts (a pool thread made on a daemon thread is a daemon).
;;   send / await / await-for  the fixed pool: its workers never time out, so
;;       after one of these the process never ends by itself.
;;
;; shutdown-agents ends both at once; what is still running is waited for, as the
;; JVM's shutdown lets running tasks finish. System/exit, Runtime.halt and an
;; uncaught error end the process without any of this, as there.
;; A babashka task (`jolt run`) is the one entry that is not clojure.main: bb's
;; agent pools are made of DAEMON threads (certified on bb: a future's, a
;; send-off's isDaemon is true), so a task that leaves a future running ends
;; without it, and nothing lingers. jolt.main switches this on before a task runs.
;; A plain Thread and an Executors pool behave as on the JVM under bb too.
(define agent-threads-daemon? #f)
(def-var! "jolt.host" "agent-threads-daemon!"
  (lambda () (set! agent-threads-daemon? #t) jolt-nil))
;; Otherwise an agent pool thread is never a daemon, whatever thread starts it,
;; as clojure.core's pools are not, so the exit waits for it.
(define (fork-agent-thread thunk)
  (fork-thread/daemon agent-threads-daemon? thunk))
(define solo-pool-keep-alive-ms 60000)     ; Executors.newCachedThreadPool's 60L, SECONDS

;; The solo pool: what a future runs on, as clojure.lang.Agent/soloExecutor is on
;; the JVM — a cached pool (0, unbounded, 60s keep-alive), so a worker that has
;; finished a task takes the next one instead of a thread being forked per future
;; (~25us a fork, against ~1us for a handoff to a parked worker). Its idle workers
;; are live threads, so they hold the process up until their keep-alive runs out,
;; which is the exit the JVM has after a future; shutdown-agents shuts the pool
;; down, which retires them at once and lets running tasks finish.
;;
;; Its workers are never daemons, whatever thread's submit starts one, as
;; clojure.core's pools are not (exit-wait-smoke pins a future started from a
;; daemon thread holding the exit); under bb's semantics (agent-threads-daemon!)
;; every worker is one. Made on first use, after jolt.main has settled that.
(define solo-pool #f)
(define solo-pool-mu (make-mutex))
;; Built OUTSIDE the mutex and published under it, so nothing that may park runs
;; with the lock held (park-lock-check.ss); a pool that loses the race to publish
;; is dropped, and with no core workers it has forked nothing.
(define (solo-pool-state)
  (or solo-pool
      (let ((p (make-executor* "executor-service" 0 executor-unbounded-workers
                               solo-pool-keep-alive-ms #f
                               agent-threads-daemon? #f)))
        (jolt-with-mutex solo-pool-mu
          (unless solo-pool (set! solo-pool p))
          solo-pool))))
(define (solo-pool-run! job)
  (executor-enqueue! (solo-pool-state) job))
(define (solo-pool-shutdown!)
  (let ((p (jolt-with-mutex solo-pool-mu solo-pool)))
    (when p (executor-shutdown! (jhost-state p)))))
(define (exit-linger-after-task!)
  (unless (jolt-thread-daemon? (get-thread-id))
    (let ((until (mono-deadline solo-pool-keep-alive-ms)))
      (jolt-with-mutex exit-mu
        (unless exit-pools-shut?
          (when (or (not exit-linger-until) (> until exit-linger-until))
            (set! exit-linger-until until)))))))
(define (exit-hold-pooled!)
  (unless (or agent-threads-daemon? (jolt-thread-daemon? (get-thread-id)))
    (jolt-with-mutex exit-mu
      (unless exit-pools-shut? (set! exit-pooled-held? #t)))))
(define (exit-release-pools!)
  (jolt-with-mutex exit-mu
    (set! exit-pools-shut? #t)
    (set! exit-pooled-held? #f)
    (set! exit-linger-until #f)
    (condition-broadcast exit-cv)))
;; Blocks the calling THREAD until nothing holds the process up. Called at
;; process end on the main thread (the CLI's normal return and a built binary's
;; launcher), never from a fiber.
(define (jolt-await-user-threads!)
  (unless exit-wait-skipped? (await-exit-holds!)))
;; The tooling commands — `jolt build` and its compile workers, the AOT cache
;; worker — are the compiler, not the program: they load the program's
;; namespaces to compile them, and a namespace whose top level starts a future
;; or a pool would otherwise hold the compiler up for that pool's keep-alive
;; (60s per build for test/chez/build-app, whose app.core derefs a future at
;; load). They end when their work is done, as a build tool's compile process
;; does. Set by the CLI dispatch before the command runs.
(define exit-wait-skipped? #f)
(define (jolt-skip-exit-wait!) (set! exit-wait-skipped? #t))
(define (await-exit-holds!)
  (jolt-with-mutex exit-mu
    (let loop ()
      (cond
        ((fx>? exit-nondaemon-live 0) (jolt-condition-wait exit-cv exit-mu) (loop))
        (exit-pooled-held? (jolt-condition-wait exit-cv exit-mu) (loop))
        ((and exit-linger-until (not (mono-due? exit-linger-until)))
         (jolt-condition-wait exit-cv exit-mu (jolt-deadline->timeout exit-linger-until))
         (loop))
        (else (void))))))

;; The agent system's pool threads carry the names the JVM's thread factories give
;; them: send runs on clojure-agent-send-pool-N, send-off and future on
;; clojure-agent-send-off-pool-N, each pool numbering its own threads from 0. A
;; worker takes the lowest number no running worker of its pool holds and gives
;; it back when it ends, as a pool hands work to an idle thread, so the numbers
;; stay as low as the pool's concurrency.
(define agent-pool-slots (vector (vector 0 '()) (vector 0 '())))   ; #(next free)
(define agent-pool-mu (make-mutex))
(define (agent-pool-of pool) (vector-ref agent-pool-slots (if (eq? pool 'send) 0 1)))
(define (agent-pool-take! pool)
  (jolt-with-mutex agent-pool-mu
    (let* ((slot (agent-pool-of pool)) (free (vector-ref slot 1)))
      (if (pair? free)
          (begin (vector-set! slot 1 (cdr free)) (car free))
          (let ((n (vector-ref slot 0))) (vector-set! slot 0 (+ n 1)) n)))))
(define (agent-pool-give! pool n)
  (jolt-with-mutex agent-pool-mu
    (let ((slot (agent-pool-of pool)))
      (vector-set! slot 1 (let ins ((l (vector-ref slot 1)))
                            (if (or (null? l) (< n (car l))) (cons n l) (cons (car l) (ins (cdr l)))))))))
(define (agent-pool-thread-name pool n)
  (string-append (if (eq? pool 'send) "clojure-agent-send-pool-" "clojure-agent-send-off-pool-")
                 (number->string n)))

;; Thread-local list of (agent pool f . args) sent from within the action currently
;; running on this thread (#f outside an action). Holds nested sends until the
;; action completes, like the JVM's ThreadLocal `nested`. Its box-ness is also
;; the signal that *agent* is bound (an action is in flight on this thread).
(define *agent-nested* (make-thread-parameter #f))
(define (jolt-in-agent-action?) (box? (*agent-nested*)))

;; (agent state :meta m :validator f :error-handler h :error-mode e): the ARef
;; ctor contract like atom's — the validator runs against the initial state,
;; :meta must be a map. Default error-mode is :fail, unless an :error-handler is
;; given (then :continue), matching clojure.core.
(define (jolt-agent-new state . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f) (handler jolt-nil) (mode #f))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let* ((em (or mode (if (jolt-nil? handler) 'fail 'continue)))
              (a (make-jolt-agent state jolt-nil validator (vector '() '()) #f
                                  (make-mutex) (jolt-make-cv) em handler)))
         (when (and (not (jolt-nil? validator)) (jolt-not (jolt-invoke validator state)))
           (jolt-iref-state-throw))
         (when (and m (not (jolt-nil? m)))
           (unless (jolt-map? m)
             (jolt-throw (jolt-host-throwable
                          "java.lang.ClassCastException"
                          (string-append "class " (jolt-class-name m)
                                         " cannot be cast to class clojure.lang.IPersistentMap"))))
           (meta-table-set! a m))
         a))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o) m handler mode))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "meta"))
       (loop (cddr o) validator (cadr o) handler mode))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "error-handler"))
       (loop (cddr o) validator m (cadr o) mode))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "error-mode"))
       (loop (cddr o) validator m handler (kw->mode (cadr o))))
      (else (loop (cddr o) validator m handler mode)))))
(define (kw->mode k)
  (let ((n (keyword-t-name k))) (if (string=? n "continue") 'continue 'fail)))
;; agents are watchable IRefs; the worker notifies on each state change.
(register-iref-arm! jolt-agent?)

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
(define (jagent-q-clear! a)
  (jolt-agent-queue-set! a (vector '() '())))
(define (jagent-q-count a)
  (let ((q (jolt-agent-queue a)))
    (fx+ (length (vector-ref q 0)) (length (vector-ref q 1)))))

;; Each action runs with *agent* bound to its agent, like the JVM's action
;; binding frame — (send a (fn [s] (send *agent* …))) works. The cell resolves
;; lazily (dynamic-var-defaults.ss loads after this file).
(define agent-star-cell #f)
(define (with-agent-binding a thunk)
  (let ((cell (or agent-star-cell
                  (let ((c (var-cell-lookup "clojure.core" "*agent*")))
                    (set! agent-star-cell c) c))))
    (if (not cell)
        (thunk)
        (dyn-with-frame (list (cons cell a)) thunk))))

;; Enqueue an action and start the worker if the agent is idle. No precondition
;; checks — used by the direct send path (after checks), by nested-send release,
;; and by restart resuming a held queue. A queue entry is (pool f . args): pool is
;; send, send-off, or #f for an executor of the caller's (send-via). An action
;; that reaches send's fixed pool holds the process up for good, as that pool's
;; worker does.
(define (jolt-agent-enqueue! a pool f args)
  (when (eq? pool 'send) (exit-hold-pooled!))
  (jolt-with-mutex (jolt-agent-mu a)
    (jagent-q-push! a (cons* pool f args))
    (unless (jolt-agent-running? a)
      (jolt-agent-running?-set! a #t)
      (fork-agent-thread (lambda () (*txn* #f) (rdr-default-modes!) (jolt-agent-worker a)))))
  a)

;; Dispatch the held nested sends accumulated on this thread, returning the count
;; dispatched (0 outside an action). Release empties the list first so the
;; dispatched sends (which call jolt-agent-enqueue! directly, bypassing the hold)
;; are not re-held.
(define (jolt-release-pending-sends)
  (let ((nested (*agent-nested*)))
    (if (not (box? nested))
        0
        (let ((sends (unbox nested)))
          (set-box! nested '())
          (for-each (lambda (e) (jolt-agent-enqueue! (car e) (cadr e) (caddr e) (cdddr e)))
                    (reverse sends))
          (length sends)))))

;; Drain the queue, applying each action (f state arg*) outside the lock (an
;; action may send/deref the same agent). A successful action flushes any nested
;; sends it accumulated; a thrown action invokes the error-handler (if any, its
;; throws swallowed) and then either continues (:continue, state left untouched)
;; or fails the agent (:fail, error stored, queue halted).
;; The worker takes the name of the pool each action was sent to, one number per
;; pool for the worker's life (names: pool -> number, cur: the pool it is named
;; for), and gives the numbers back when it goes idle.
(define (jolt-agent-worker a)
  (*txn* #f)                          ; agent worker must not inherit parent's txn
  (let loop ((names '()) (cur #f))
    (let ((act (jolt-with-mutex (jolt-agent-mu a)
                 (if (or (not (jolt-nil? (jolt-agent-err a))) (jagent-q-empty? a))
                     (begin (jolt-agent-running?-set! a #f)
                            (jolt-cv-wake! (jolt-agent-cv a)) #f)
                     (jagent-q-pop! a)))))
      ;; idle: the pool worker this stands for would linger (exit wait, below)
      (unless act
        (for-each (lambda (pn) (agent-pool-give! (car pn) (cdr pn))) names)
        (exit-linger-after-task!))
      (when act
        (let* ((pool (car act))
               (names (if (or (not pool) (eq? pool cur) (assq pool names))
                          names
                          (cons (cons pool (agent-pool-take! pool)) names))))
          (when (and pool (not (eq? pool cur)))
            (jolt-thread-name-set! (get-thread-id) (agent-pool-thread-name pool (cdr (assq pool names)))))
        (parameterize ((*agent-nested* (box '())))
          (let ((err #f))
            (guard (e (#t (set! err (jolt-unwrap-throw e))))
              (let* ((old (jolt-agent-state a))
                     (nv (with-agent-binding a
                           (lambda () (apply jolt-invoke (cadr act) old (cddr act))))))
                (let ((vf (jolt-agent-validator a)))
                  (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf nv)))
                    (jolt-iref-state-throw)))
                (jolt-agent-state-set! a nv)
                (iref-notify a old nv)))
            ;; post-action handling runs while *agent-nested* is still the box, so
            ;; the success flush sees the held sends.
            (if err
                (let ((handler (jolt-agent-err-handler a)))
                  (when (not (jolt-nil? handler))
                    ;; the handler runs as if outside the action: its sends go direct
                    (parameterize ((*agent-nested* #f))
                      (guard (_ (#t #f)) (jolt-invoke handler a err))))
                  (when (eq? (jolt-agent-err-mode a) 'fail)
                    (jolt-with-mutex (jolt-agent-mu a)
                      (jolt-agent-err-set! a err)
                      (jolt-cv-wake! (jolt-agent-cv a)))))
                (jolt-release-pending-sends))))   ; success: flush nested sends
        (loop names (or pool cur)))))))

;; send / send-off: enqueue the action, start the worker if idle. (One serialized
;; worker per agent runs both, observably a superset of the JVM fixed/cached pool
;; split; the pool names the worker's thread.) A send after shutdown-agents, to a failed
;; agent, inside a transaction, or from within an action is handled specially.
;; A send after shutdown-agents is the pool REJECTING the action, which the JVM's
;; Agent catches and hands to the agent's error handler: the send itself returns
;; the agent and the action never runs (certified: (send a inc) after
;; shutdown-agents answers the agent). It used to throw RejectedExecutionException.
;;
;; send and send-off differ only in the pool the JVM runs the action on, and so in
;; what keeps the process up after it: send's fixed pool for good, send-off's cached
;; pool for its keep-alive (the exit wait, below). send-via takes the caller's
;; executor and holds nothing of the agent system's.
;; The fixed pool gains a worker only once an action reaches it (jolt-agent-enqueue!):
;; a send that throws (a failed agent) or is rejected holds nothing up.
(define (jolt-agent-send a f . args)
  (apply jolt-agent-dispatch 'send a f args))
(define (jolt-agent-send-off a f . args)
  (apply jolt-agent-dispatch 'send-off a f args))
;; A send inside a transaction is held as a thunk the commit runs (refs.ss).
(define (jolt-agent-dispatch pool a f . args)
  (cond
    ((jolt-agents-shutdown?)
     (let ((handler (jolt-agent-err-handler a)))
       (unless (jolt-nil? handler)
         (guard (_ (#t #f))
           (jolt-invoke handler a (jolt-host-throwable
                                   "java.util.concurrent.RejectedExecutionException" jolt-nil))))))
    ((not (jolt-nil? (jolt-agent-err a)))
     (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                      "Agent is failed, needs restart")))
    ((*txn*)
     (let ((txn (*txn*)))
       (jolt-txn-pending-sends-set! txn
         (cons (lambda () (apply jolt-agent-dispatch pool a f args))
               (jolt-txn-pending-sends txn)))))
    ((jolt-in-agent-action?)
     (let ((nested (*agent-nested*)))
       (set-box! nested (cons (cons* a pool f args) (unbox nested)))))
    (else (jolt-agent-enqueue! a pool f args)))
  a)

;; (await & agents) / (await-for ms & agents): block until each agent's queue has
;; drained. Illegal inside a transaction or an agent action; a failed agent
;; rethrows its stored error (the JVM dispatches a sentinel action that would
;; throw on a failed agent). await-for returns false on timeout.
(define (jolt-agent-await-check)
  (when (*txn*)
    (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "await in transaction")))
  (when (jolt-in-agent-action?)
    (jolt-throw (jolt-host-throwable "java.lang.Exception" "Can't await in agent action"))))
;; An already-failed agent rejects the await like it rejects a send — the JVM's
;; await dispatches a latch action to each agent and the send throws (verified
;; against JVM Clojure 1.12). The same throw fires when an agent fails DURING
;; the wait: the loop below exits on the failure broadcast (the worker halts and
;; broadcasts), and the post-loop check re-throws — (await a) / (await-for ms a)
;; never return normally on a failed agent.
(define (jolt-agent-failed-throw a)
  (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                   "Agent is failed, needs restart"
                                   (jolt-agent-err a))))
;; The err check runs on every pass rather than once before the wait and once after
;; it, which is the same two cases the retake makes one: the agent failed before we
;; got here, or it failed while we waited (the worker halts and wakes us). Either
;; way the JVM throws on a failed agent rather than returning normally.
;; await and await-for dispatch their latch action with send on the JVM, so they
;; hold the process up as a send does.
(define (jolt-agent-await . agents)
  (jolt-agent-await-check)
  (unless (null? agents) (exit-hold-pooled!))
  (for-each
    (lambda (a)
      (jolt-cv-wait-interruptibly "agent await" (jolt-agent-mu a) (jolt-agent-cv a) #f
        (lambda (_timed-out?)
          (cond
            ((not (jolt-nil? (jolt-agent-err a))) (jolt-agent-failed-throw a))
            ((or (jolt-agent-running? a) (not (jagent-q-empty? a))) jolt-cv-again)
            (else #t)))))
    agents)
  jolt-nil)
(define (jolt-agent-await-for ms . agents)
  (when (*txn*)
    (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "await-for in transaction")))
  (when (jolt-in-agent-action?)
    (jolt-throw (jolt-host-throwable "java.lang.Exception" "Can't await in agent action")))
  (unless (null? agents) (exit-hold-pooled!))
  (let ((deadline (ms->deadline-millis ms)) (ok #t))
    (for-each
      (lambda (a)
        (when ok
          (unless
            (jolt-cv-wait-interruptibly "agent await-for"
                                        (jolt-agent-mu a) (jolt-agent-cv a) deadline
              (lambda (timed-out?)
                (cond
                  ;; DRAINED is tested before the deadline on purpose: a drain that
                  ;; completed as the deadline hit is not a timeout. Failure during
                  ;; the wait throws here, as it did before (JVM parity).
                  ((not (or (jolt-agent-running? a) (not (jagent-q-empty? a))))
                   (if (jolt-nil? (jolt-agent-err a)) #t (jolt-agent-failed-throw a)))
                  ;; ...and a genuine timeout answers #f without throwing, even for
                  ;; an agent that failed, which is what the old arm ordering did.
                  (timed-out? #f)
                  ((not (jolt-nil? (jolt-agent-err a))) (jolt-agent-failed-throw a))
                  (else jolt-cv-again))))
            (set! ok #f))))
      agents)
    ok))

(define (jolt-agent-error a) (jolt-agent-err a))
(define (jolt-agent-get-error-mode a)
  (keyword #f (symbol->string (jolt-agent-err-mode a))))

;; Agent.setErrorMode and setErrorHandler are VOID, so set-error-mode! and
;; set-error-handler! answer nil rather than the agent — these used to hand the
;; agent back, which threads but is not what (set-error-mode! a :continue)
;; evaluates to on the JVM.
(define (jolt-agent-set-error-mode! a k)
  (jolt-agent-err-mode-set! a (kw->mode k)) jolt-nil)
(define (jolt-agent-get-error-handler a) (jolt-agent-err-handler a))
(define (jolt-agent-set-error-handler! a f)
  (jolt-agent-err-handler-set! a f) jolt-nil)

;; --- clojure.lang.Agent's own method surface --------------------------------
;; The watch/validator half an agent shares with atom/ref/var is answered in
;; records-dispatch.ss's base; these are the methods only an Agent has. They are
;; registered as a method ARM rather than written into that base because the
;; natives are here, this file loads long after it, and the Gambit host does not
;; include this file at all — an agent does not exist there to have methods.
;;
;; Two of them are not just the native under another name:
;;   restart(newState, clearActions) is positional where restart-agent takes
;;   :clear-actions. Both answer the new state.
;;   dispatch(fn, args, exec) is the send/send-off kernel with the pool named
;;   explicitly. jolt runs ONE serialized worker per agent — send and send-off
;;   are already the same call here — so the executor is unobservable and the
;;   third argument is accepted and ignored. This is the one method here that is
;;   a superset: the JVM refuses a null executor by never running the action.
;; getQueueCount reads the pending queue under the agent mutex: it is two list
;; traversals over a structure a worker mutates, so unlike getError (a field
;; read) it cannot be taken unsynchronized.
(define (jolt-agent-queue-count a)
  (jolt-with-mutex (jolt-agent-mu a) (jagent-q-count a)))
(define (jagent-restart-2 a new-state clear?)
  (jolt-agent-restart a new-state (keyword #f "clear-actions") (jolt-truthy? clear?)))
;; The action runs on jolt's agent worker whatever executor is named; it holds
;; the exit as that worker does (the 60s linger), not as the fixed send pool.
(define (jagent-dispatch-3 a f args _exec)
  (apply jolt-agent-dispatch #f a f (rd-args->list args)))
(define (jagent-method name argc)
  (cond ((string=? name "getError")        (and (fx=? argc 0) jolt-agent-error))
        ((string=? name "getErrorMode")    (and (fx=? argc 0) jolt-agent-get-error-mode))
        ((string=? name "getErrorHandler") (and (fx=? argc 0) jolt-agent-get-error-handler))
        ((string=? name "setErrorMode")    (and (fx=? argc 1) jolt-agent-set-error-mode!))
        ((string=? name "setErrorHandler") (and (fx=? argc 1) jolt-agent-set-error-handler!))
        ((string=? name "getQueueCount")   (and (fx=? argc 0) jolt-agent-queue-count))
        ((string=? name "restart")         (and (fx=? argc 2) jagent-restart-2))
        ((string=? name "dispatch")        (and (fx=? argc 3) jagent-dispatch-3))
        (else #f)))
(register-method-arm! arm-priority-agent
  (lambda (obj method-name rest-args)
    (if (jolt-agent? obj)
        (let* ((rest (if (jolt-nil? rest-args) (quote ()) (seq->list rest-args)))
               (f (jagent-method method-name (length rest))))
          (if f (apply f obj rest) (quote pass)))
        (quote pass))))

;; --- java.util.concurrent.Future on a clojure.core future --------------------
;; A future is a reify of IDeref, IBlockingDeref, IPending and Future on the JVM,
;; and records-dispatch.ss answers the IDeref half (.deref, the no-arg .get). The
;; Future half was missing: (.get f 100 TimeUnit/MILLISECONDS), .isDone,
;; .isCancelled and .cancel all failed with "No matching method". Same tier as the
;; agent arm below it, on a disjoint type.
(define jfuture-timeout (list 'jfuture-timeout))
(define (jfuture-get-2 f amount unit)
  (let ((r (jolt-future-deref-timed f (tu->ms amount unit) jfuture-timeout)))
    (if (eq? r jfuture-timeout) (future-timeout-throw) r)))
(define (jfuture-method name argc)
  (cond ((string=? name "get")         (and (fx=? argc 2) jfuture-get-2))
        ((string=? name "isDone")      (and (fx=? argc 0) jolt-future-done?))
        ((string=? name "isCancelled") (and (fx=? argc 0) jolt-future-cancelled?))
        ((string=? name "cancel")
         (and (fx=? argc 1) (lambda (f b) (jolt-future-cancel f (jolt-truthy? b)))))
        (else #f)))
(register-method-arm! arm-priority-agent
  (lambda (obj method-name rest-args)
    (if (jolt-future? obj)
        (let* ((rest (if (jolt-nil? rest-args) (quote ()) (seq->list rest-args)))
               (f (jfuture-method method-name (length rest))))
          (if f (apply f obj rest) (quote pass)))
        (quote pass))))

;; The other four IDeref types: records-dispatch.ss answers .deref / .get and the
;; getAs* bridges for every deref-able reference, but it can only SEE the ones
;; defined before it. These four are defined here, so this is where they join —
;; and the split it asks for is the JVM's: a promise and a future are
;; IBlockingDeref, so they carry the two-argument (ms, timeout-val) deref, while
;; an agent and a delay are plain IDeref and have no such method to reflect onto.
(set-rd-extra-deref-hook!
  (lambda (x)
    (cond ((or (jolt-future? x) (jolt-promise? x)) (quote iblocking))
          ((or (jolt-agent? x) (jolt-delay? x)) (quote ideref))
          (else #f))))
;; Deprecated JVM helpers: agent-errors is a seq of the error or nil; clear-agent
;; -errors restarts with the current state (so it throws on a healthy agent, as on
;; the JVM).
(define (jolt-agent-errors a)
  (let ((e (jolt-agent-err a))) (if (jolt-nil? e) jolt-nil (list e))))
(define (jolt-clear-agent-errors a) (jolt-agent-restart a (jolt-agent-state a)))

;; restart-agent: un-fail the agent with new-state. Throws if not failed; the
;; new-state must pass the validator (else the agent stays failed); :clear-actions
;; discards the held queue, otherwise the queued actions resume. Watchers are NOT
;; notified (per the JVM contract).
;; THE VALIDATOR IS USER CODE, so it does not run under the bookkeeping mutex.
;; Agent.restart is `synchronized` on the JVM and its whole body — the error check,
;; the validate, and the writes — is one critical section. A mutex cannot be that
;; section here: a validator that parks would have it released mid-way by the unwind
;; (and now raises instead, see locks.ss). So the section is the agent's own OBJECT
;; MONITOR, which is what `synchronized` is, and which tolerates a park because
;; ownership is a field rather than an OS mutex. The bookkeeping mutex stays for what
;; it is for — the field reads and writes, with no user code between an acquire and
;; its release.
;;
;; Both properties the one big mutex was providing survive. Two concurrent restarts
;; still serialize, on the monitor, so the JVM's "the second is told the agent does
;; not need a restart" still holds; and the error check still precedes the validate,
;; so which of the two errors a healthy agent with an invalid value reports is
;; unchanged. Validating before taking anything would have reversed that, and
;; re-checking under a re-taken mutex would have let two restarts through.
;;
;; Same choice and the same reason as the object monitor (jolt-3a87), the delay
;; (jolt-232k) and the transaction (jolt-pb2s): the locks in this runtime that wrap
;; code they did not write are the ones that cannot be OS mutexes.
(define (jolt-agent-restart a new-state . opts)
  (let ((clear? (and (pair? opts) (keyword-t? (car opts))
                     (string=? (keyword-t-name (car opts)) "clear-actions")
                     (pair? (cdr opts)) (eq? (cadr opts) #t))))
    (jolt-with-monitor a
      (lambda ()
        (jolt-with-mutex (jolt-agent-mu a)
          (when (jolt-nil? (jolt-agent-err a))
            (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                             "Agent does not need a restart"))))
        (let ((vf (jolt-agent-validator a)))
          (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf new-state)))
            (jolt-iref-state-throw)))
        (jolt-with-mutex (jolt-agent-mu a)
          (jolt-agent-state-set! a new-state)
          (jolt-agent-err-set! a jolt-nil)
          (cond (clear? (jagent-q-clear! a))
                ((and (not (jagent-q-empty? a)) (not (jolt-agent-running? a)))
                 (jolt-agent-running?-set! a #t)
                 (fork-agent-thread (lambda () (*txn* #f) (rdr-default-modes!) (jolt-agent-worker a)))))))))
  ;; Agent.restart answers the NEW STATE, not the agent (and clear-agent-errors,
  ;; which is restart-agent over the current state, answers that state in turn).
  new-state)

;; --- taps (tap>/add-tap/remove-tap) -----------------------------------------
;; Mirrors the JVM tap system: a bounded (1024) FIFO of queued values plus a
;; single daemon delivery thread. tap> offers without blocking (true if the value
;; was enqueued, false if the queue is full); the delivery thread blocks on take
;; and applies every registered fn to the value, catching throws so a failing tap
;; can't kill the loop. nil is queued as a private sentinel so a real nil
;; round-trips through the queue.
(define tapq-capacity 1024)
(define tapq-sentinel (list 'jolt 'tapq-nil))   ; unique; never a user value

(define-record-type jolt-tap-queue
  (fields (mutable out) (mutable in) (mutable len) cap mu cv)
  (nongenerative jolt-tap-queue-v1))

(define tapq (make-jolt-tap-queue '() '() 0 tapq-capacity (make-mutex) (make-condition)))

;; The registered taps are a set (a fn registers once). Held as a Scheme list
;; under a lock; identity comparison matches the JVM's set semantics for fns.
(define tapset-mu (make-mutex))
(define tapset (box '()))
(define (tapset-add! f)
  (jolt-with-mutex tapset-mu
    (unless (memq f (unbox tapset))
      (set-box! tapset (cons f (unbox tapset))))))
(define (tapset-remove! f)
  (jolt-with-mutex tapset-mu
    (set-box! tapset (filter (lambda (x) (not (eq? x f))) (unbox tapset)))))
(define (tapset-snapshot)
  (jolt-with-mutex tapset-mu (unbox tapset)))

(define (tapq-offer! v)
  (jolt-with-mutex (jolt-tap-queue-mu tapq)
    (if (>= (jolt-tap-queue-len tapq) tapq-capacity)
        #f
        (begin
          (jolt-tap-queue-in-set! tapq (cons v (jolt-tap-queue-in tapq)))
          (jolt-tap-queue-len-set! tapq (fx+ 1 (jolt-tap-queue-len tapq)))
          (condition-broadcast (jolt-tap-queue-cv tapq))
          #t))))

(define (tapq-take!)
  (jolt-with-mutex (jolt-tap-queue-mu tapq)
    (let loop ()
      (cond
        ((fx> (jolt-tap-queue-len tapq) 0)
         (when (null? (jolt-tap-queue-out tapq))
           (jolt-tap-queue-out-set! tapq (reverse (jolt-tap-queue-in tapq)))
           (jolt-tap-queue-in-set! tapq '()))
         (let ((v (car (jolt-tap-queue-out tapq))))
           (jolt-tap-queue-out-set! tapq (cdr (jolt-tap-queue-out tapq)))
           (jolt-tap-queue-len-set! tapq (fx- (jolt-tap-queue-len tapq) 1))
           v))
        (else (jolt-condition-wait (jolt-tap-queue-cv tapq) (jolt-tap-queue-mu tapq)) (loop))))))

;; The delivery thread starts lazily on the first add-tap/tap>, like the JVM's
;; `(delay (doto (Thread. …) (.start)))`.
(define tap-thread-started? (box #f))
(define (start-tap-thread!)
  (unless (unbox tap-thread-started?)
    (set-box! tap-thread-started? #t)
    (fork-thread/daemon #t                 ; clojure.core's tap-loop thread is a daemon
     (lambda ()
       (*txn* #f)
       (rdr-default-modes!)                ; and not the reader modes of a read it forked from
       (let loop ()
         (let* ((t (tapq-take!))
                (x (if (eq? t tapq-sentinel) jolt-nil t))
                (taps (tapset-snapshot)))
           (for-each
             (lambda (tap)
               (guard (e (#t #f)) (jolt-invoke tap x)))
             taps)
           (loop)))))))

(define (jolt-add-tap f)
  (start-tap-thread!)
  (tapset-add! f)
  jolt-nil)
(define (jolt-remove-tap f)
  (tapset-remove! f)
  jolt-nil)
(define (jolt-tap> x)
  (start-tap-thread!)
  (tapq-offer! (if (jolt-nil? x) tapq-sentinel x)))

;; --- delay (lazy once-forced computation) -----------------------------------
;; (delay body) -> (make-delay (fn [] body)) (overlay macro); force/deref run the
;; thunk once under a lock and cache the value (JVM delays are thread-safe). force
;; (overlay) is (if (delay? x) (deref x) x), so it works once delay?/deref do.
;; No `mu` field: the delay's lock is its own object monitor, for the reason set out
;; at jolt-delay-force. A per-delay mutex nothing takes would be worse than none.
(define-record-type jolt-delay (fields thunk (mutable realized?) (mutable value) (mutable exn))
  (nongenerative jolt-delay-v2))
(define (jolt-make-delay thunk) (make-jolt-delay thunk #f jolt-nil #f))

;; How futures, promises, delays and agents print (printing.ss register-deref-print!):
;; pending until realized, failed with the throwable a deref would raise, else
;; ready with the value. A failed agent prints its state.
(register-deref-print! jolt-future?
  (lambda (f)
    (cond ((not (jolt-future-done? f)) (cons "pending" jolt-nil))
          ((jolt-future-cancelled? f)
           (cons "failed" (jolt-host-throwable "java.util.concurrent.CancellationException" jolt-nil)))
          ((jolt-future-ok? f) (cons "ready" (jolt-future-payload f)))
          (else (cons "failed" (task-execution-exception (jolt-future-payload f)))))))
(register-deref-print! jolt-promise?
  (lambda (p) (if (jolt-promise-delivered? p) (cons "ready" (jolt-promise-value p)) (cons "pending" jolt-nil))))
(register-deref-print! jolt-delay?
  (lambda (d)
    (cond ((not (jolt-delay-realized? d)) (cons "pending" jolt-nil))
          ((jolt-delay-exn d) (cons "failed" (jolt-unwrap-throw (jolt-delay-exn d))))
          (else (cons "ready" (jolt-delay-value d))))))
(register-deref-print! jolt-agent?
  (lambda (a) (cons (if (jolt-nil? (jolt-agent-err a)) "ready" "failed") (jolt-agent-state a))))
;; run the thunk once, like Clojure's Delay: if it throws, cache the exception
;; (the delay IS realized) and re-throw it on every deref — do NOT re-run the
;; body (so value-fns memoize and there is no cache-stampede / retried side
;; effect). Store the exception inside the lock, re-raise outside it so the lock
;; is always released.
;;
;; ONCE IS THE CONTRACT, AND A MUTEX CANNOT KEEP IT (jolt-232k).
;;
;; This was jolt-with-mutex on a per-delay mutex around the body. jolt-with-mutex is
;; a dynamic-wind, so a fiber that PARKS in the body releases the mutex while still
;; lexically inside it; the next forcer acquired the free mutex, found realized?
;; still #f, and ran the body AGAIN. Both then wrote the value slot, so @d answered
;; whichever finished last and the two forcers could come back with different values
;; from one delay. Measured on (delay (swap! runs inc) (<!! ch)): runs = 2, and the
;; forcers got 2 and 1. Delay.deref is `synchronized` on the JVM, so the second one
;; waits — the same reason jolt-3a87 made ownership a field for `locking`.
;;
;; So the unrealized path takes the delay's own object monitor. Ownership is a field
;; and survives the park, so the second forcer waits; a fiber contender PARKS rather
;; than blocking the carrier the holder may need to finish; and no COUNTED lock is
;; held while the body runs, so a long delay body is preemptible instead of pinning
;; its carrier for its whole extent.
;;
;; The realized? FAST PATH is not just an optimisation for the monitor's cost, it is
;; sound on its own and was worth having anyway: the writes are ordered value-then-
;; realized? and exn-then-realized?, so realized? = #t implies the payload is
;; published, and a reader that sees it needs no lock at all. Every deref of an
;; already-forced delay used to take the mutex; now none do.
;;
;; Reentrancy is unchanged in observable terms: a delay whose body derefs itself
;; re-enters the monitor, finds realized? #f and recurses, which is what the JVM does
;; too (synchronized is reentrant and fn is still non-null).
(define (jolt-delay-force d)
  (unless (jolt-delay-realized? d)
    (jolt-with-monitor d
      (lambda ()
        (unless (jolt-delay-realized? d)
          (guard (e (#t (jolt-delay-exn-set! d e) (jolt-delay-realized?-set! d #t)))
            (jolt-delay-value-set! d (jolt-invoke (jolt-delay-thunk d)))
            (jolt-delay-realized?-set! d #t))))))
  (if (jolt-delay-exn d) (raise (jolt-delay-exn d)) (jolt-delay-value d)))

;; --- deref extension --------------------------------------------------------
;; Chain the fully-built jolt-deref (atoms/vars/volatiles/reduced) with futures,
;; promises, agents, and delays; accept the timed (deref ref ms val) arity for the
;; blocking ref types. This is the GENERAL body — the entry point installed into
;; jolt-deref is the case-lambda below it, which takes the atom case first.
(define %pre-conc-deref jolt-deref)
(define %conc-deref-general
  (lambda (x . opts)
    (cond
      ((jolt-future? x)
       (if (null? opts) (jolt-future-deref x)
           (jolt-future-deref-timed x (car opts) (cadr opts))))
      ((jolt-promise? x)
       (if (null? opts) (jolt-promise-deref x)
           (jolt-promise-deref-timed x (car opts) (cadr opts))))
      ;; An agent and a delay are IDeref but NOT IBlockingDeref, so the timed
      ;; arity is a failed interface cast on the JVM, not a silent 1-arity call
      ;; that drops the timeout.
      ((jolt-agent? x)
       (if (null? opts) (jolt-agent-state x) (jolt-throw (deref-cast-error x opts))))
      ((jolt-delay? x)
       (if (null? opts) (jolt-delay-force x) (jolt-throw (deref-cast-error x opts))))
      ;; java.util.concurrent.Future — a FutureTask, and what an ExecutorService's
      ;; submit hands back. Neither is IDeref on the JVM either; clojure.core/deref
      ;; falls THROUGH to deref-future for anything that is not, which is .get, and
      ;; for the timed arity .get(ms, MILLISECONDS) with a TimeoutException answered
      ;; by the timeout value rather than thrown. Without this arm @a-future-task
      ;; raised "deref: unsupported reference type", so @(.submit pool f) — and
      ;; core.async.flow's own @(flow/inject …) and @(futurize …) — did not work.
      ;; The ms goes through tu-args->ms so the timed arity normalizes its amount
      ;; exactly as the .get(timeout, unit) overload does.
      ;;
      ;; ONE jhost? test and then the tag, rather than an arm per shim: anything
      ;; that is not one of these shims falls past the whole cond, so what a
      ;; non-shim pays for them is a single record-type predicate. (An ATOM never
      ;; gets here at all — the case-lambda entry below answers it first.)
      ((and (jhost? x) (future-shim-get* x))
       => (lambda (get*)
            (if (null? opts)
                (get* x #f future-timeout-throw)
                (get* x (tu-args->ms (list (car opts))) (lambda () (cadr opts))))))
      ;; a record/reify implementing clojure.lang.IDeref: @x calls its `deref`
      ;; method with the value itself as the leading `this`. The timed arity
      ;; passes its opts through — (deref r ms val) reaches the IBlockingDeref
      ;; 3-arity method. A deref method that exists only at the other arity is
      ;; the JVM's failed interface cast: throw ClassCastException naming the
      ;; interface the requested arity belongs to.
      ((and (jrec? x)
            (jrec-method-arity x "deref" (if (null? opts) 1 3)))
       ;; the arity lookup falls back to any same-name method, so verify the
       ;; chosen impl really accepts this call's arity before invoking.
       => (lambda (m)
            (if (and (procedure? m)
                     (not (bitwise-bit-set? (procedure-arity-mask m)
                                            (+ 1 (length opts)))))
                (jolt-throw (deref-cast-error x opts))
                (apply jolt-invoke m x opts))))
      ((reify-method-ref x "deref")
       => (lambda (m)
            (if (and (procedure? m)
                     (not (bitwise-bit-set? (procedure-arity-mask m)
                                            (+ 1 (length opts)))))
                (jolt-throw (deref-cast-error x opts))
                (apply jolt-invoke m x opts))))
      ;; Everything else (atom, ref, var, …) is IDeref at most: the timed arity
      ;; is the same failed cast. Without this it reached %pre-conc-deref, which
      ;; raised a host error whose (class e) was the opaque :object sentinel.
      ((null? opts) (%pre-conc-deref x))
      (else (jolt-throw (deref-cast-error x opts))))))

;; The ATOM and VOLATILE arms, ahead of the whole chain. Reaching an atom used
;; to mean a rest-arg list allocated on every single call, then a walk down four
;; chained lambdas (futures -> vars -> volatiles -> atoms) with a predicate at
;; each — ~60ns to read one record field, and every loop that keeps mutable
;; state in an atom pays it per iteration (bench/cst-format derefs a per-node
;; atom several times per node). A volatile is the same read one step earlier
;; in that chain, and every stateful transducer (partition-by, take, distinct)
;; derefs its box per input. A `case-lambda` fixed clause allocates nothing and
;; answers each with one record-type test; every other reference type and arity
;; reaches the general body above completely unchanged.
(set! jolt-deref
  (case-lambda
    ((x) (cond ((jolt-atom? x) (jolt-atom-val x))
               ((jvol? x) (jvol-v x))
               (else (%conc-deref-general x))))
    ((x ms val) (%conc-deref-general x ms val))
    ((x . opts) (apply %conc-deref-general x opts))))

(define (deref-cast-error x opts)
  (jolt-host-throwable
   "java.lang.ClassCastException"
   (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                  " cannot be cast to class "
                  (if (null? opts) "clojure.lang.IDeref" "clojure.lang.IBlockingDeref"))))

;; realized? for a future/promise/delay. Wrapped over the overlay version in
;; post-prelude.ss.
(define (jolt-conc-realized? x)
  (cond ((jolt-future? x) (jolt-future-done? x))
        ((jolt-promise? x) (jolt-promise-delivered? x))
        ((jolt-delay? x) (jolt-delay-realized? x))
        (else #f)))

;; --- bind into clojure.core -------------------------------------------------
(def-var! "clojure.core" "future-call" jolt-future-call)
(def-var! "clojure.core" "future-cancel" jolt-any-future-cancel)
(def-var! "clojure.core" "future?" jolt-any-future?)
(def-var! "clojure.core" "future-done?" jolt-native-future-done?)
(def-var! "clojure.core" "future-cancelled?" jolt-native-future-cancelled?)
(def-var! "clojure.core" "promise" jolt-promise-new)
(def-var! "clojure.core" "deliver" jolt-deliver)
;; a promise is an IFn on the JVM: (p val) delivers. Registered as a cold
;; invoke arm; callable-host? feeds the ifn? overlay (multimethods included).
(register-invoke-arm! jolt-promise?
  (lambda (p args)
    (if (and (pair? args) (null? (cdr args)))
        (jolt-deliver p (car args))
        (jolt-throw (jolt-host-throwable "clojure.lang.ArityException"
                                         "Wrong number of args passed to a promise")))))
(def-var! "jolt.host" "callable-host?"
  (lambda (x) (if (or (jolt-multifn? x) (jolt-promise? x)) #t jolt-nil)))
(def-var! "clojure.core" "agent" jolt-agent-new)
(def-var! "clojure.core" "agent?" jolt-agent?)
(def-var! "clojure.core" "send" jolt-agent-send)
(def-var! "clojure.core" "send-off" jolt-agent-send-off)
;; send-via takes an executor jolt has no model for; behave as send and ignore it.
(def-var! "clojure.core" "send-via"
  (lambda (_exec a f . args) (apply jolt-agent-dispatch #f a f args)))
;; Documented superset no-ops: jolt has no executor pool, so these accept and
;; ignore their argument, returning nil (as the JVM setters would).
(def-var! "clojure.core" "set-agent-send-executor!" (lambda (_e) jolt-nil))
(def-var! "clojure.core" "set-agent-send-off-executor!" (lambda (_e) jolt-nil))
(def-var! "clojure.core" "await" jolt-agent-await)
(def-var! "clojure.core" "await-for" jolt-agent-await-for)
(def-var! "clojure.core" "release-pending-sends" (lambda () (jolt-release-pending-sends)))
(def-var! "clojure.core" "agent-error" jolt-agent-error)
(def-var! "clojure.core" "agent-errors" jolt-agent-errors)
(def-var! "clojure.core" "clear-agent-errors" jolt-clear-agent-errors)
(def-var! "clojure.core" "error-mode" jolt-agent-get-error-mode)
(def-var! "clojure.core" "set-error-mode!" jolt-agent-set-error-mode!)
(def-var! "clojure.core" "error-handler" jolt-agent-get-error-handler)
(def-var! "clojure.core" "set-error-handler!" jolt-agent-set-error-handler!)
(def-var! "clojure.core" "restart-agent" jolt-agent-restart)
(def-var! "clojure.core" "shutdown-agents" jolt-shutdown-agents)
(def-var! "clojure.core" "tap>" jolt-tap>)
(def-var! "clojure.core" "add-tap" jolt-add-tap)
(def-var! "clojure.core" "remove-tap" jolt-remove-tap)
(def-var! "clojure.core" "make-delay" jolt-make-delay)
;; (clojure.lang.Delay. thunk) — the class (delay …) already reports, so a
;; library spelling its own delay macro as the constructor (fully-satisfies'
;; safe-locals-clearing does, to control when locals clear) builds the same
;; object core's delay does and derefs through the same force.
(register-class-ctor! "clojure.lang.Delay" (lambda (thunk) (jolt-make-delay thunk)))
(register-class-ctor! "Delay" (lambda (thunk) (jolt-make-delay thunk)))
;; (clojure.lang.LazySeq. thunk) — the cell core's lazy-seq builds, forced once
;; through the same bridge (lazy-bridge.ss: the thunk yields the body coerced to
;; cells). fully-satisfies' safe-locals-clearing spells its lazy-seq as this
;; constructor, exactly as it spells its delay above.
(define (lazyseq-ctor f)
  (jolt-make-lazy-seq (lambda () (jolt-coll->cells (jolt-invoke0 f)))))
(register-class-ctor! "clojure.lang.LazySeq" lazyseq-ctor)
(register-class-ctor! "LazySeq" lazyseq-ctor)
(def-var! "clojure.core" "delay?" jolt-delay?)
(def-var! "clojure.core" "deref" jolt-deref)

;; --- object monitors (locking) ----------------------------------------------
;; (locking obj body…) takes obj's monitor for the body — a real per-object lock
;; now that futures/agents/threads share one heap. Monitors live in an
;; identity-keyed weak table so they are reclaimed with their objects, and a
;; monitor is REENTRANT for its holder, like a JVM intrinsic lock: a nested
;; (locking x …) on the same object re-enters instead of deadlocking.
;;
;; OWNERSHIP IS A FIELD, NOT THE OS MUTEX, AND THAT IS THE WHOLE DESIGN (jolt-3a87).
;;
;; This used to be the obvious thing: one Chez mutex per object, acquired through
;; jolt-lock! for the length of the body, with the owner recorded as the thread's
;; interrupt box. Every lock in the runtime is held that way, and locks.ss explains
;; why it is sound — the scheduler refuses to preempt a fiber that holds a counted
;; lock, and such regions are SHORT (~55ns mean) and never span a park.
;;
;; A monitor is the one lock in the runtime that wraps code it did not write, so
;; neither half of that premise holds, and it failed three ways at once:
;;
;;   1. `locking` was a dynamic-wind, so a park UNWOUND it: the monitor was released
;;      mid-body and re-taken on resume, and two fibers on one carrier were inside
;;      one body at the same time. Worse, the second one's re-entry found the owner
;;      equal to its own identity, because the owner was the CARRIER THREAD and
;;      same-carrier fibers share it — so it took the reentrant arm silently.
;;   2. the bare (monitor-enter x) / (monitor-exit x) halves have no wind at all, so
;;      a park in between kept the OS mutex across the switch. jolt-locks-held then
;;      stayed up on the carrier for as long as the fiber was parked, which makes
;;      every LATER fiber on that carrier unpreemptible, and a sibling's acquire
;;      succeeded anyway because Chez mutexes are recursive per thread.
;;   3. holding the mutex for the body meant jolt-locks-held was up for the body, so
;;      a long (locking o …) could not be preempted at all and starved everything
;;      queued behind it — the unbounded starvation window fibers.ss says no setting
;;      can open.
;;
;; All three come through the same trapdoor: an OS mutex has THREAD granularity and a
;; fiber is not a thread. So the mutex below is no longer the monitor. It is a
;; BOOKKEEPING lock, held only across the enter/exit decision — which really is one
;; of the short regions locks.ss is about — and mutual exclusion is carried by the
;; owner field, which a context switch cannot disturb because it is not a wind.
;;
;; What that buys, in the order the symptoms above appear: ownership survives a park
;; and a preemption, so nothing else can be inside the body; the bare halves become
;; correct rather than merely diagnosable; and the body is preemptible again, because
;; no counted lock is held while it runs.
;;
;; A contender WAITS, and how it waits depends on what it is. A fiber parks on the
;; monitor's own waiter list and the release resumes it — never a condition-wait,
;; which would block the carrier that may be the very thing the holder needs in order
;; to reach its release. A thread waits on the condition variable. Both are woken by
;; the release, and neither can lose a wakeup, because the registration happens under
;; the bookkeeping lock that the release also takes.
;;
;; WHERE THE FIBER'S SWITCH HAPPENS, and why it is not inside bk (jolt-dfuo). The
;; fiber registers itself and commits to 'parked under bk, and then bk is RELEASED and
;; the switch runs outside it. It used to park inside the region and lean on
;; jolt-with-mutex being a dynamic-wind to release on the way out and re-acquire on
;; the resume. That reading of locks.ss's precondition was too weak: it is not "no
;; fiber is parked while HOLDING bk", it is that no fiber on the carrier may be
;; holding bk while this one is off the CPU, because the re-acquire runs from Chez's
;; rewind, on the carrier thread, at the interrupt depth the fiber parked at, and the
;; carrier can do nothing else until it succeeds. A monitor's bk does not satisfy
;; that: several fibers on one carrier, plus any number of threads, all pass through
;; this region for the same monitor. Measured: one OS thread and eight fibers taking
;; the same monitor in a loop wedged the whole process in 1 run of 12, with bk left
;; held by a carrier whose fiber had parked in the wait and every other carrier
;; blocked in that re-acquire (the process it wedged was `make test`, jolt-8tma).
;;
;; Committing under the lock and switching outside it is what the channel waiters
;; (java/fibers-async.ss jolt-fiber-<!) and jolt.io-poller/wait-fiber already do, for
;; this reason. It also needs no interrupt disable: the window between the release and
;; the switch belongs to a fiber that is already 'parked, and
;; jolt-fiber-preempt-handler refuses to preempt a fiber that is not 'running.
;;
;; It is not open-coded here any more. Five sites needed this protocol and four of
;; them had written it out, which is four chances to write a fifth — and loader.ss's
;; load claims were that fifth (jolt-04ee). It is now jolt-lock-wait in
;; host/chez/locks.ss, where the rule it keeps is also stated: a fiber never leaves
;; the CPU while its carrier holds a counted lock, checked at both switch points, so
;; breaking it raises at the park instead of wedging the process.
;;
;; NOT the JEP 491 reimplementation locks.ss rules out. That paragraph is about the
;; runtime's own locks, of which there are many and all short. The locks that wrap
;; code they did not write are countable — the object monitor here, the STM lock
;; (refs.ss), the delay's force, and ReentrantLock below — and they all reach for the
;; mechanism in this section. Making those four fiber-aware is not making all of them
;; so, which is the line that keeps the rest of the runtime on jolt-with-mutex.
(define monitor-table (make-weak-eq-hashtable))
(define monitor-table-lock (make-mutex))
;; #(bk owner count cv fibers box wcv waiters)
;;   bk     the bookkeeping mutex — held across a decision, never across a body
;;   owner  the OWNER WORD: the FIBER when a fiber holds it, else the thread's
;;          interrupt box (current-interrupt-box, an identity that is safe under
;;          fork-inheritance), or #f when free — or that identity wrapped in a
;;          monitor-contended record once some contender is waiting for it. The
;;          uncontended enter and exit are one compare-and-swap on this word and
;;          never take bk; see "the owner word" below
;;   count  reentrancy depth for the owner
;;   cv     thread waiters; condition-wait releases bk atomically with blocking
;;   fibers parked fiber waiters, resumed by the release
;;   box    the interrupt box of the OS thread the owner took it on — the THREAD
;;          identity (current-os-thread-box, not the fiber's own box), which is
;;          what monitor-owner? needs when the owner is a fiber
;;   wcv    the WAIT SET's condition — Object.wait/notify, not monitor entry
;;   waiters the wait set itself, FIFO. The last two are the condition-variable
;;          half and are documented at monitor-object-wait! below; every monitor
;;          carries them, but a monitor nobody waits on never touches them.
(define monitor-i-bk 0)
(define monitor-i-owner 1)
(define monitor-i-count 2)
(define monitor-i-cv 3)
(define monitor-i-fibers 4)
(define monitor-i-box 5)
(define monitor-i-wcv 6)
(define monitor-i-waiters 7)
;; A fresh monitor. Named rather than inlined at the one use site, because
;; object-monitor is no longer the only thing that needs one:
;; java.util.concurrent.locks.ReentrantLock is a lock in its own right — (locking
;; lk …) and (.lock lk) are DIFFERENT locks on the JVM and stay different here,
;; so it gets its own record rather than the object's — but the hard part it needs
;; is exactly this (jolt-ga8o). One implementation, two locks.
;; Both conditions are jolt-cvs, like every condition a value carries: a monitor
;; travels in an image inside a ReentrantLock, and an image written before format
;; 11 restores every condition as a jolt-cv. A raw entry condition here came back
;; from such an image as a jolt-cv and the first contended exit's broadcast raised.
;; Its fibers are in the fibers slot, not on the cv, so a wake of it only reaches
;; the blocked threads -- and skips the broadcast when there are none.
(define (make-monitor)
  (vector (make-mutex) #f 0 (jolt-make-cv) '() #f (jolt-make-cv) '()))

;; --- the owner word -------------------------------------------------------------
;; A thin lock. An uncontended enter is CAS #f -> me and an uncontended exit is
;; CAS me -> #f, with no bk and no wake. What makes the exit safe without bk is
;; that "somebody is waiting" lives in the SAME word: a contender, under bk and
;; before it waits, swaps owner X for (monitor-contended X), so the owner's
;; me -> #f CAS fails and it takes the slow exit, which frees the word and wakes
;; the waiters under bk. A waiter cannot be missed, because it marks the word
;; before it waits and the slow exit needs the bk the waiter holds until its wait
;; releases it; no store-load fence is needed, because both sides agree through
;; one CAS'd location.
;;
;; Model-checked in tools/monitor-model.pl (`make monitormodel`; every interleaving
;; of three contexts, no spurious wakeups so a lost one cannot be rescued by a
;; retry): mutual exclusion, no deadlock, every waiter can leave its wait and is
;; always covered by the contended mark or a pending broadcast, and with two
;; contexts each can always re-enter. The mutant whose fast exit frees a contended
;; word leaves a waiter uncovered, and the run fails if the model stops seeing it.
;; Change the protocol here, change the model there.
;;
;; vector-cas! is a WEAK compare-and-swap on arm64 (measured: 34 spurious failures
;; in 10^8 uncontended attempts). A spurious failure only moves an enter or an exit
;; onto its slow path, which handles every word it can find; the model allows the
;; slow release from an uncontended word for that reason.
;;
;; count and box belong to the owner: only the context holding the word writes
;; them, so the reentrant enter and exit touch them without bk too.
(define-record-type monitor-contended (fields id) (nongenerative jolt-monitor-contended-v1))
(define (monitor-word-id w) (if (monitor-contended? w) (monitor-contended-id w) w))
(define (monitor-owner-id m) (monitor-word-id (vector-ref m monitor-i-owner)))
;; Take a free word for me: CAS #f -> me, then the owner's own fields.
(define (monitor-claim! m me)
  (and (vector-cas! m monitor-i-owner #f me)
       (begin
         (memory-order-acquire)
         (vector-set! m monitor-i-box (current-os-thread-box))
         (vector-set! m monitor-i-count 1)
         #t)))
;; Free the word outright (bk held): the slow exit and Object.wait's release.
(define (monitor-release-word! m)
  (vector-set! m monitor-i-box #f)
  (memory-order-release)
  (vector-set! m monitor-i-owner #f))

(define (object-monitor obj)
  (jolt-with-mutex monitor-table-lock
    (or (hashtable-ref monitor-table obj #f)
        (let ((m (make-monitor)))
          (hashtable-set! monitor-table obj m) m))))

;; Who is asking. The FIBER when there is one, and this is the field that decides
;; reentrancy, so it is also what stopped a sibling fiber walking into a held
;; section: two fibers on one carrier share a thread and therefore shared the old
;; answer. A fiber is only ever compared and used as a list element here, so nothing
;; depends on the record's shape.
(define (monitor-self) (or (jolt-current-fiber) (current-interrupt-box)))

;; Does `me` own m? Normally one eq?, and the second arm is not a loosening of it but
;; the completion of it.
;;
;; A fiber's own winders run DURING its escape, and jolt-fiber-done! / jolt-fiber-dead!
;; clear the current-fiber vreg before escaping. So a wind that belongs to the fiber —
;; jolt-with-monitor's release, or a jolt `finally` calling the bare (monitor-exit x) —
;; arrives here off-fiber even though it is the owner. java/sm.ss makes that reachable
;; on purpose: it handles a throwing body with with-exception-handler rather than
;; guard, precisely so the handler runs at the raise point, which puts dead! before the
;; winders instead of after them. Refusing there left the monitor held for the life of
;; the process and raised IllegalMonitorState out of an after-thunk, mid-escape, on top
;; of an already-dying fiber.
;;
;; Narrow on purpose: the owner is a fiber, that fiber is TERMINAL, we are on the
;; thread it ran on, and no fiber is mounted. A terminal fiber will never release the
;; monitor itself, so the only alternative to accepting this is leaking it. A different
;; fiber on the same carrier does not match (it has a current fiber), and neither does
;; another thread (its interrupt box differs).
(define (monitor-owner? m me)
  (let ((owner (monitor-owner-id m)))
    (or (eq? owner me)
        (and (jolt-fiber? owner)
             (not (jolt-current-fiber))
             (eq? (vector-ref m monitor-i-box) (current-os-thread-box))
             (memq (jolt-fiber-state owner) '(done dead))
             #t))))

;; Call with bk HELD. Two different answers, because the two contenders wait in
;; different places:
;;
;;   a THREAD waits here, on the condition variable, which releases bk atomically
;;   with blocking and holds it again on return. Answers #f: bk is held, and the
;;   caller re-checks under it — a wakeup means "something changed", never "the
;;   monitor is yours".
;;
;;   a FIBER must not touch the condition variable (condition-wait would block the
;;   carrier, and the holder may be a fiber on that same carrier). It registers on the
;;   monitor's waiter list and COMMITS to 'parked, both under bk so monitor-exit!
;;   cannot run between them and no wakeup can be lost. It answers jolt-lock-parked,
;;   which is jolt-lock-wait's instruction to release bk and switch it out THERE: the
;;   park must not happen inside this region, or the resume's re-acquire deadlocks the
;;   carrier (see the header, jolt-dfuo).
(define (monitor-wait! m)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin
          (vector-set! m monitor-i-fibers (cons f (vector-ref m monitor-i-fibers)))
          (jolt-fiber-wstate-set! f (thread-enter-state))
          (jolt-fiber-state-set! f 'parked)
          jolt-lock-parked)
        (begin
          ;; BLOCKED, as the JVM reports a thread entering a monitor; a
          ;; ReentrantLock's acquire says WAITING instead (thread-enter-state)
          (call-with-thread-state (thread-enter-state)
            (lambda ()
              (jolt-condition-wait (vector-ref m monitor-i-cv) (vector-ref m monitor-i-bk))))
          #f))))
;; What a thread waiting in monitor-wait! is: BLOCKED for a monitor (locking), and
;; WAITING for a ReentrantLock, which parks on the JVM rather than blocking.
(define thread-enter-state (make-thread-parameter 'BLOCKED))

;; The decision is made under bk; a fiber's SWITCH is made outside it, and then the
;; whole decision is retaken, because a resume says only that something changed. All
;; three of those are jolt-lock-wait (host/chez/locks.ss), which is this protocol
;; named once rather than open-coded at each of the sites that needs it.
;;
;; THE MASKED ENTER. It returns with this thread's counted-lock depth one higher
;; (jolt-locks-enter!, host/chez/locks.ss), and the caller drops it once whatever
;; releases the monitor on an escape is in place. run-interruptible leaves its body
;; by jumping out of a timer interrupt, and a fiber interrupt is raised where a
;; preemption resumed, so any event check could be the last thing this thread does
;; in here: one between the claim and the caller's wind left the monitor held for
;; the life of the process, and one inside the claim left it held at depth 0. Both
;; of those handlers hold off while the depth is above zero, so from before the
;; claim until the wind is installed no escape can land
;; (test/chez/monitor-escape-test.ss). The count rather than disable-interrupts
;; because it is what those two handlers already test, and a virtual-register
;; increment where disable/enable-interrupts cost every `locking` a fifth of its time.
;; Never across a wait: the slow path drops it before it blocks or parks, and takes
;; it again only to commit.
(define (monitor-enter-masked! m)
  (jolt-locks-enter!)
  (let* ((me (monitor-self)) (w (vector-ref m monitor-i-owner)))
    (cond
      ((and (not w) (monitor-claim! m me)) (void))
      ;; reentrant: the word is mine (a contender may have wrapped it), and the
      ;; count is the owner's alone
      ((eq? (monitor-word-id w) me)
       (vector-set! m monitor-i-count (fx+ 1 (vector-ref m monitor-i-count))))
      (else (jolt-locks-exit!) (monitor-enter-slow! m me)))))
(define (monitor-enter! m)
  (monitor-enter-masked! m)
  (jolt-locks-exit!))
(define (monitor-enter-slow! m me)
  (jolt-lock-wait (vector-ref m monitor-i-bk)
    (lambda ()
      (let loop ()
        (let ((w (vector-ref m monitor-i-owner)))
          (cond
            ((not w)
             (jolt-locks-enter!)
             (if (monitor-claim! m me) #f (begin (jolt-locks-exit!) (loop))))
            ((eq? (monitor-word-id w) me)
             (jolt-locks-enter!)
             (vector-set! m monitor-i-count (fx+ 1 (vector-ref m monitor-i-count)))
             #f)
            ;; held by someone else: mark the word contended BEFORE waiting, so the
            ;; holder's fast exit fails and it wakes us. A failed mark means the
            ;; word moved; decide again. A thread's wait ends under this same bk,
            ;; so it loops HERE; a fiber answers jolt-lock-parked and jolt-lock-wait
            ;; retakes this decision from the top once something has resumed it.
            ((monitor-contended? w) (or (monitor-wait! m) (loop)))
            ((vector-cas! m monitor-i-owner w (make-monitor-contended w))
             (or (monitor-wait! m) (loop)))
            (else (loop)))))))
  (void))

;; The same decision without the wait: #t if this context now holds the monitor,
;; #f if something else does. ReentrantLock.tryLock is the caller, and the reason
;; it is a variant of monitor-enter! rather than a peek followed by an enter is
;; that a peek-then-enter is two steps — the monitor could be taken in between
;; and the "try" would block.
;;
;; Note what it does NOT do: take a Chez mutex. That is the whole difference from
;; the ReentrantLock this replaces, whose failed try had to give a counted lock
;; back and whose successful one held one for the length of the caller's critical
;; section. Here bk is dropped before the caller runs a line of its body, so
;; jolt-locks-held is zero inside the section and the section is preemptible.
(define (monitor-try-enter! m)
  (let* ((me (monitor-self)) (w (vector-ref m monitor-i-owner)))
    (cond
      ((not w) (monitor-claim! m me))
      ((eq? (monitor-word-id w) me)
       (vector-set! m monitor-i-count (fx+ 1 (vector-ref m monitor-i-count)))
       #t)
      (else #f))))

;; The three questions ReentrantLock exposes. All strict eq? against monitor-self,
;; NOT monitor-owner?: that predicate's second arm exists so a TERMINAL fiber's own
;; unwind can release what it was holding, and answering a query #t on that basis
;; would tell a live thread it holds a lock a dead fiber took. Release is lenient
;; because the alternative is leaking the monitor; a query has no such excuse.
(define (monitor-held-by-self? m)
  (jolt-with-mutex (vector-ref m monitor-i-bk)
    (eq? (monitor-owner-id m) (monitor-self))))
;; getHoldCount is "holds by the CURRENT thread" on the JVM — 0 for anyone else,
;; not the raw depth.
(define (monitor-self-count m)
  (jolt-with-mutex (vector-ref m monitor-i-bk)
    (if (eq? (monitor-owner-id m) (monitor-self))
        (vector-ref m monitor-i-count)
        0)))
(define (monitor-locked? m)
  (jolt-with-mutex (vector-ref m monitor-i-bk)
    (and (vector-ref m monitor-i-owner) #t)))

;; One round of a POLLED wait — the bounded and interruptible acquires, which
;; cannot use monitor-wait! because that has no deadline and no interrupt check.
;;
;; On a fiber this yields, and that is the load-bearing part rather than a
;; politeness: the holder may be a sibling fiber on this very carrier, and
;; sleeping the carrier is then the one thing that guarantees the wait cannot
;; succeed — the holder never gets the CPU it needs to reach its release, so the
;; call runs to its deadline every time however long the deadline is. That is what
;; a (sleep) poll did here.
;;
;; It naps only when the yield came back to an empty run queue, so the nap costs no
;; queued fiber its turn and the poll does not spin a core for the length of the
;; timeout. The queue read is unlocked on purpose: it is a hint, and being wrong
;; either way costs one 1ms nap or one extra yield.
(define lock-poll-nap (make-time 'time-duration 1000000 0))   ; 1ms
;; With a DEADLINE the nap is cut to what is left of it, so a bounded acquire gives
;; up at its deadline and not up to a nap past it.
(define monitor-poll-round!
  (case-lambda
    (() (monitor-poll-round* lock-poll-nap))
    ((deadline)
     (let ((left (mono-duration-until deadline)))
       (monitor-poll-round* (if (time<? left lock-poll-nap) left lock-poll-nap))))))
(define (monitor-poll-round* nap)
  (let ((f (jolt-current-fiber)))
    (cond
      ((not f) (sleep nap))
      (else
       (sa-fiber-yield)
       (unless (jolt-carrier-head (jolt-fiber-carrier f))
         (sleep nap))))))

;; The waiters are taken and cleared under bk so no resume is delivered twice, and
;; they are woken OUTSIDE it: sa-fiber-resume takes the carrier's run-queue mutex,
;; and keeping the two apart means this path closes no cycle at all rather than
;; relying on that mutex being last in the order. Broadcast and not signal, for
;; loader.ss's reason: the waiters re-check a condition that will be true for exactly
;; one of them.
;;
;; MASKED? is the release a masked enter pairs with: the caller raised the
;; counted-lock depth before it, and this drops it once the monitor is released —
;; and before the IllegalMonitorState throw, which must not leave it raised.
(define (monitor-exit! m) (monitor-exit* m #f))
(define (monitor-exit* m masked?)
  (let* ((me (monitor-self)) (w (vector-ref m monitor-i-owner)) (n (vector-ref m monitor-i-count)))
    (cond
      ;; reentrant exit by the owner: its own count, no word change
      ((and (eq? (monitor-word-id w) me) (fx>? n 1))
       (vector-set! m monitor-i-count (fx- n 1)))
      ;; uncontended release: me -> #f. Fails only if a contender marked the word
      ;; since, and then the slow exit wakes it.
      ((and (eq? w me) (begin (memory-order-release) (vector-cas! m monitor-i-owner me #f)))
       (void))
      (else (monitor-exit-slow! m me masked?)))
    (when masked? (jolt-locks-exit!))))
(define (monitor-exit-slow! m me masked?)
  (let ((wake
         (jolt-with-mutex (vector-ref m monitor-i-bk)
           (unless (monitor-owner? m me)
             (when masked? (jolt-locks-exit!))
             (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException"
                                              "not lock owner")))
           (vector-set! m monitor-i-count (fx- (vector-ref m monitor-i-count) 1))
           (if (fx=? 0 (vector-ref m monitor-i-count))
               (let ((fs (vector-ref m monitor-i-fibers)))
                 (monitor-release-word! m)
                 (vector-set! m monitor-i-fibers '())
                 (jolt-cv-wake! (vector-ref m monitor-i-cv))
                 fs)
               '()))))
    (for-each sa-fiber-resume wake)))

;; The enter happens OUTSIDE the dynamic-wind, and the exit asks whether this is a
;; real exit. Both halves matter and neither is the obvious spelling.
;;
;; A before-thunk that entered would re-enter on every resume, because a park escapes
;; through this wind and the rewind re-runs it — the same trap dyn-with-frame
;; describes for a binding frame, and the same answer: do it once, outside.
;;
;; A park is not an exit, so the after-thunk must not release: the monitor is meant to
;; still be held when the fiber comes back, which is the entire point of the field
;; above. jolt-park-unwinding? is the seam that tells the two apart, and it is the
;; same one load-namespace* uses to keep its claim across a park. A raise and
;; run-interruptible's escape are REAL exits and answer #f, so those still release.
;;
;; WHAT THIS AFTER-THUNK RESTS ON, and where it is checked. A CHEAP park (java/sm.ss)
;; never rewinds: it escapes, skips this after-thunk on the jolt-park-unwinding? test,
;; and comes back through the fiber thunk with the wind gone — so the release would
;; never run and the monitor would be held for the life of the process, with every later
;; contender waiting and no error anywhere. The reason that cannot happen is that
;; `locking` hands its body over as a THUNK and `fn*` is opaque to the CPS pass, so a
;; park in there falls back to a capture, which rewinds properly.
;;
;; It is the same thing parameterize and binding rely on and sm.ss's invariant names all
;; three — but this one is invisible to the check sm.ss cites. run-gosm.ss section 1c
;; scans the EMITTED Scheme for a rewritten park site inside a dynamic-wind, and the wind
;; here is inside a host procedure the emission merely calls, so nothing textual can see
;; it. Section 1d is the one that covers this shape: a park inside `locking` is not
;; rewritten, with controls for the parks either side of the monitor that must stay
;; cheap, and section 2 checks that the monitor is actually released afterwards by having
;; a second go block take it.
;;
;; NEITHER END MAY BE ESCAPED, and that is why the release on a normal return is in
;; the body. run-interruptible leaves by jumping out of a timer interrupt, so any
;; event check can be the escape point (see monitor-enter-masked!):
;;
;;   the enter leaves the counted-lock depth raised and the body drops it, so the
;;   wind is in place before an escape can land after the claim. The body and not the
;;   before-thunk, because a resume re-runs the before-thunk.
;;
;;   dynamic-wind pops the winder BEFORE it calls the after-thunk, and the
;;   after-thunk's own entry is a check, so an escape there skipped the release
;;   that the after-thunk was about to make. So a normal return releases inside the
;;   body, still wound and masked across the release and the flag that tells the
;;   after-thunk it is done. The after-thunk is left the raises and
;;   escapes, which are already under way when it runs.
;;
;; A body that let go of the monitor itself (a bare monitor-exit) gets the exit's
;; IllegalMonitorStateException, as a synchronized block's own monitorexit does on
;; the JVM; monitor-exit* drops the mask before it throws.
(define (jolt-call-with-monitor m thunk)
  (monitor-enter-masked! m)
  (let ((held #t))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (jolt-locks-exit!)
        (let ((r (thunk)))
          (jolt-locks-enter!)
          (set! held #f)
          (monitor-exit* m #t)
          r))
      (lambda ()
        (when (and held (not (jolt-park-unwinding?)))
          (set! held #f)
          (monitor-exit! m))))))
;; A caller that holds an object's monitor already (a StringBuffer resolves its
;; own once, at construction) enters it through jolt-call-with-monitor directly;
;; the table lookup is the same monitor either way.
(define (jolt-with-monitor obj thunk) (jolt-call-with-monitor (object-monitor obj) thunk))
(def-var! "jolt.host" "with-monitor" jolt-with-monitor)

;; The bare halves of the same monitor, for the (monitor-enter x) /
;; (monitor-exit x) special forms. Code that expands its own locking macro
;; instead of calling clojure.core/locking emits these directly — dynaload does,
;; and it sits under malli — so they take and release the very same per-object
;; monitor `locking` uses, and the two compose. Holding one across a park is now
;; simply correct: there is no wind to unwind and ownership is a field, so the
;; monitor is still held when the fiber comes back and a sibling waits for it.
;; Both yield nil: the JVM emits a NIL after the monitor op rather than the object.
(define (jolt-monitor-enter obj)
  (monitor-enter! (object-monitor obj))
  jolt-nil)
(define (jolt-monitor-exit obj)
  (monitor-exit! (object-monitor obj))
  jolt-nil)
(def-var! "jolt.host" "monitor-enter" jolt-monitor-enter)
(def-var! "jolt.host" "monitor-exit" jolt-monitor-exit)

;; --- Object.wait / .notify / .notifyAll -------------------------------------
;; The condition-variable half of the object monitor. `locking` is the exclusion
;; half and was already here; this is the part that lets a holder GIVE THE MONITOR
;; BACK while it waits for someone else to change the state it guards, which is the
;; portable primitive every hand-rolled condition variable on the JVM is built from
;; (jolt#1011).
;;
;; THE WAIT SET IS NOT THE ENTRY SET, and that is the whole shape of this code. A
;; monitor already had one queue — the contenders blocked in monitor-enter!, on the
;; cv and the fibers list — and a waiter is not one of them: it is not trying to
;; take the monitor, it is waiting to be told. So there is a second queue (waiters,
;; woken through wcv) and a notify moves an entry from it to the entry set; mixing
;; the two would make every monitor release wake every waiter and every notify wake
;; every contender.
;;
;; WHAT MAKES IT ATOMIC. "Releases the monitor and enters the wait set" has to be
;; one step or a notify lands in the gap and is lost. Both structures are guarded by
;; bk, so one critical section covers both — and the notification is a FIELD on the
;; waiter's own entry, not the wake itself, so a wake that arrives early (or a
;; spurious one, which is allowed and which jolt-cv-wake! delivers freely) is
;; re-checked against it rather than believed.
;;
;; THE RELEASE IS FULL, the re-acquire restores the DEPTH. wait releases a reentrant
;; monitor completely however deep it was held — the JVM's rule, and the reason a
;; (locking o (locking o (.wait o))) does not deadlock its own notifier — so the
;; saved count is what makes the return indistinguishable from never having waited.
;;
;; The wait itself is jolt-cv-wait-interruptibly over bk and wcv, so it inherits
;; everything that carrier already settles: a thread blocks, a FIBER parks and gives
;; its carrier up, the decision is retaken on every wake (which is what makes a
;; spurious wake harmless), and .interrupt throws InterruptedException and clears
;; the flag. The one thing the carrier cannot know about is the monitor, so the
;; re-acquire is done here on BOTH exits — the guard below is not cleanup, it is the
;; JVM's rule that an interrupted wait reacquires before it throws.

;; Enter the wait set and release the monitor outright; answers the hold count to
;; restore, masked (the counted-lock depth one higher) so the caller can record it
;; before an escape could land — see monitor-object-wait!. The fiber contenders are resumed OUTSIDE bk for monitor-exit!'s reason
;; — sa-fiber-resume takes a carrier's run-queue mutex, and keeping the two apart
;; means this path closes no cycle at all.
;;
;; Appended rather than consed: the wait set is FIFO, so a notify wakes the waiter
;; that has been waiting longest. The JVM picks arbitrarily and HotSpot is FIFO by
;; default; arbitrary permits fair, and a wait set is short enough that the append
;; is not worth a second data structure.
(define (monitor-wait-enter! m me w)
  (let ((r (jolt-with-mutex (vector-ref m monitor-i-bk)
             (unless (monitor-owner? m me)
               (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException"
                                                "current thread is not owner")))
             ;; masked until the caller has recorded the count it must restore
             (jolt-locks-enter!)
             (let ((saved (vector-ref m monitor-i-count))
                   (fs (vector-ref m monitor-i-fibers)))
               (vector-set! m monitor-i-waiters
                            (append (vector-ref m monitor-i-waiters) (list w)))
               (monitor-release-word! m)
               (vector-set! m monitor-i-count 0)
               (vector-set! m monitor-i-fibers '())
               (jolt-cv-wake! (vector-ref m monitor-i-cv))
               (cons saved fs)))))
    (for-each sa-fiber-resume (cdr r))
    (car r)))

;; Mark the next waiter notified, or all of them. Call with bk HELD. A notified
;; entry LEAVES the wait set here rather than on its own way out: it is in the
;; entry set from this moment, so a second notify reaches the next waiter instead
;; of the same one twice. Answers whether anything was marked, so a notify with no
;; waiters costs no wake.
(define (monitor-mark-notified! m all?)
  (let ((ws (vector-ref m monitor-i-waiters)))
    (cond
      ((null? ws) #f)
      (all? (for-each (lambda (w) (set-box! w #t)) ws)
            (vector-set! m monitor-i-waiters '())
            #t)
      (else (set-box! (car ws) #t)
            (vector-set! m monitor-i-waiters (cdr ws))
            #t))))

;; Take the monitor again at the depth it was released at, and leave the wait set.
;; Returns masked, like monitor-enter-masked!; the caller drops it.
;;
;; threw? is the interrupted exit, and the one case that has to hand a notification
;; ON: a waiter that was notified and then interrupted before it could return has
;; consumed a notify that was meant to move SOMEBODY to the entry set, so it
;; notifies the next waiter in its place. That is HotSpot's own behaviour and the
;; alternative is a lost wakeup on a wait set that may still hold the only thread
;; able to make progress.
(define (monitor-wait-exit! m saved w threw?)
  (monitor-enter-masked! m)
  (jolt-with-mutex (vector-ref m monitor-i-bk)
    (vector-set! m monitor-i-count saved)
    (let ((ws (vector-ref m monitor-i-waiters)))
      (cond
        ;; still queued — a timeout or an interrupt that no notify reached
        ((memq w ws) (vector-set! m monitor-i-waiters (remq w ws)))
        ((and threw? (unbox w))
         (when (monitor-mark-notified! m #f)
           (jolt-cv-wake! (vector-ref m monitor-i-wcv))))
        (else (void))))))

;;
;; Between the release and the re-acquire the monitor is NOT held, and every way out
;; has to take it back first or the enclosing section's exit throws
;; IllegalMonitorState. The guard covers a raise and the wind covers an escape
;; (run-interruptible jumps out of a timer interrupt; a guard does not see that). The
;; release and each re-acquire run masked (see monitor-enter-masked!) up to the point
;; where `saved` says which state this is in, so no escape lands between the two
;; (test/chez/monitor-escape-test.ss). A park unwinds the wind too, and is not an exit.
(define (monitor-object-wait! m ms)
  (let* ((me (monitor-self))
         (w (box #f))
         (deadline (and ms (mono-deadline ms)))
         (saved #f))
    (define (retake! threw?)
      (monitor-wait-exit! m saved w threw?)
      (set! saved #f)
      (jolt-locks-exit!))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (set! saved (monitor-wait-enter! m me w))
        (jolt-locks-exit!)
        (guard (e (#t (retake! #t) (raise e)))
          (jolt-cv-wait-interruptibly "Object.wait"
            (vector-ref m monitor-i-bk) (vector-ref m monitor-i-wcv) deadline
            (lambda (timed-out?)
              (cond ((unbox w) #t)
                    ;; the deadline is read by decide, never signalled, so a timeout
                    ;; leaving the wait set HERE is atomic with the decision: a notify
                    ;; that had already marked us takes the arm above instead.
                    (timed-out?
                     (vector-set! m monitor-i-waiters
                                  (remq w (vector-ref m monitor-i-waiters)))
                     #t)
                    (else jolt-cv-again)))))
        (retake! #f)
        jolt-nil)
      (lambda ()
        (when (and saved (not (jolt-park-unwinding?)))
          (retake! #t))))))

(define (monitor-notify! m all?)
  (let ((me (monitor-self)))
    (jolt-with-mutex (vector-ref m monitor-i-bk)
      (unless (monitor-owner? m me)
        (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException"
                                         "current thread is not owner")))
      ;; jolt-cv-wake! is documented "call with mu HELD": it broadcasts to the
      ;; thread waiters and resumes the parked fibers, and every one of them
      ;; re-reads its own entry, so the broadcast is a wake and not a decision.
      (when (monitor-mark-notified! m all?)
        (jolt-cv-wake! (vector-ref m monitor-i-wcv)))))
  jolt-nil)

;; (.wait o) / (.wait o ms) / (.wait o ms ns) -> the timeout in ms, or #f for an
;; unbounded wait. The JVM's own arithmetic and its own two refusals, which it
;; makes BEFORE the ownership check (they are argument validation in Object.java,
;; above the native): a negative timeout and a nanosecond argument outside its
;; range are IllegalArgumentException, and any positive nanos rounds the
;; millisecond up — so (.wait o 0 1) waits about a millisecond rather than forever.
;;
;; A non-number where a timeout belongs is a ClassCastException and not a silent
;; forever-wait: wait(long) is a declared signature, so reference Clojure compiles
;; the argument through a Number cast and (.wait o :x) is "class
;; clojure.lang.Keyword cannot be cast to class java.lang.Number" there (probed on
;; 1.12.5). A fourth argument is no overload at all — the same "No matching method"
;; every other arity miss answers.
(define (object-wait-timeout args)
  ;; TRUNCATED toward zero, like the cast to long the parameter type forces: the
  ;; JVM takes (.wait o 10.5) as ten milliseconds rather than refusing it.
  (define (num-arg x)
    (unless (number? x) (jolt-num-cast-throw x))
    (truncate (jnum->exact x)))
  (define (ms-arg x)
    (let ((n (num-arg x)))
      (when (negative? n)
        (throw-jvm (quote IllegalArgumentException) "timeout value is negative"))
      n))
  (let ((ms (cond ((null? args) 0)
                  ((null? (cdr args)) (ms-arg (car args)))
                  (else
                   (let ((ms (ms-arg (car args)))
                         (ns (num-arg (cadr args))))
                     (when (or (negative? ns) (> ns 999999))
                       (throw-jvm (quote IllegalArgumentException)
                                  "nanosecond timeout value out of range"))
                     (if (positive? ns) (+ ms 1) ms))))))
    ;; zero is "wait forever", not "do not wait" — the JVM's one piece of
    ;; overloading in this API, and what the no-arg form means.
    (and (positive? ms) ms)))

(define (jolt-object-wait obj . args)
  (when (> (length args) 2) (no-method-throw "wait" obj (length args)))
  (monitor-object-wait! (object-monitor obj) (object-wait-timeout args)))
(define (jolt-object-notify obj) (monitor-notify! (object-monitor obj) #f))
(define (jolt-object-notify-all obj) (monitor-notify! (object-monitor obj) #t))

;; wait / notify / notifyAll are FINAL on java.lang.Object: no class can define one,
;; and every object has a monitor. So they are a universal arm in the tier .getClass
;; already occupies rather than a per-type entry, and no type arm below can shadow
;; them — which is what keeps (.wait s) on a string or a record working, the same
;; values `locking` already takes a monitor for.
;;
;; The arm is consulted on EVERY .method call in the process, so the reject has to
;; be cheap: three string=? would be three string walks per call, while the first
;; character rules out all but a handful of method names in one char compare. Priced
;; on bench/char_scan_unhinted.clj, whose .charAt and .length reach the generic
;; dispatcher: 203-207ms with this arm ahead of the string tier and 204-206ms with
;; it demoted below every arm (two runs of three each way, release binary), so the
;; universal placement is inside the run-to-run spread.

;; notify and notifyAll take no arguments, so a call that passes one is the arity
;; miss the JVM reports rather than a silently ignored argument. The count is read
;; only once a name has matched: ".next" and ".name" reach this arm too.
(define (object-monitor-notify obj name rest-args f)
  (let ((argc (if (jolt-nil? rest-args) 0 (jolt-count rest-args))))
    (if (fx=? argc 0) (f obj) (no-method-throw name obj argc))))

(register-method-arm! arm-priority-monitor
  (lambda (obj method-name rest-args)
    (let ((c (and (fx>? (string-length method-name) 0) (string-ref method-name 0))))
      (cond
        ((eqv? c #\w)
         (if (string=? method-name "wait")
             (apply jolt-object-wait obj (method-rest-args->list rest-args))
             (quote pass)))
        ((eqv? c #\n)
         (cond ((string=? method-name "notify")
                (object-monitor-notify obj "notify" rest-args jolt-object-notify))
               ((string=? method-name "notifyAll")
                (object-monitor-notify obj "notifyAll" rest-args jolt-object-notify-all))
               (else (quote pass))))
        (else (quote pass))))))

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
;;
;; The timer is shared with fiber preemption and the sampler, so this does not
;; own it: it is the POLL client of the tick scheduler (ticks.ss), which keeps
;; one demand per client and charges them all the same ticks. The borrow
;; registers a poll every interrupt-check-ticks and cancels it on the way out;
;; the quantum of a fiber it runs on keeps falling due as it would without the
;; borrow, and is taken by the preempt client. It used to install its own
;; handler and count what it borrowed so the quantum still fell due, and three
;; bugs were found in that hand-off (jolt-ly62, jolt-1rod, jolt-449m).
;;
;; Borrows nest. The poll client runs the INNERMOST borrow's check, kept on
;; interrupt-poll-stack, and an inner borrow's exit leaves the poll registered
;; when an outer one is still live, so the outer token keeps being polled.
;;
;; WHY THE BORROW IS STILL A dynamic-wind (jolt-1rod). A fiber PARK is a way
;; out of the body too, through the carrier's sched-k, and the poll must not
;; outlive the extent: a poll left registered over the next fiber on the carrier
;; would run this borrow's check there, and its escape would invoke a
;; continuation captured in the PARKED fiber's stack under whichever fiber owned
;; the carrier. The after-thunk cancels the poll on every way out, the park
;; included, and the before-thunk re-registers it on resume, so an interrupt
;; raised while the borrower is parked still lands on the fiber that asked for
;; it.
;;
;; WHAT THIS RESTS ON, the same thing jolt-with-monitor's after-thunk rests on: a
;; CHEAP park (java/sm.ss) never rewinds, so it would fire this after-thunk and
;; never run the before-thunk again — the borrow would end with the computation
;; still inside it. It cannot happen because run-interruptible takes its body as a
;; THUNK and fn* is opaque to the CPS pass, so a park in there falls back to a
;; capture. run-gosm.ss section 1d is where that argument is checked.
(define interrupt-check-ticks 100000)   ; ~poll interval; responsive + low overhead
(define interrupt-sentinel (cons 'jolt 'interrupted))
(define jolt-kw-interrupted (keyword "jolt" "interrupted"))
(define (jolt-make-interrupt) (box #f))
(define (jolt-interrupt! token) (when (box? token) (set-box! token #t)) jolt-nil)
(define (jolt-interrupted? token) (and (box? token) (unbox token) #t))
;; Active borrows form a dynamic stack per application thread. Chez child
;; threads inherit thread-parameter values, so owner-tag it: an inherited stack
;; is empty in the child and cannot make the child poll a parent's token.
(define interrupt-poll-stack (make-thread-parameter (cons #f '())))
(define (current-interrupt-poll-stack)
  (let ((state (interrupt-poll-stack)) (owner (get-thread-id)))
    (if (and (pair? state) (eqv? (car state) owner)) (cdr state) '())))
;; The poll client: the innermost borrow's check. With no borrow live (one that
;; exited between the fire and this read) there is nothing to poll, and the
;; demand stays cancelled.
(jolt-tick-client-set! jolt-tick-slot-poll
  (lambda ()
    (let ((st (current-interrupt-poll-stack)))
      (when (pair? st) ((car st))))))
(define (jolt-run-interruptible token thunk)
  (let* ((outer-bindings (dyn-binding-stack))
         ;; Captured once, outside the wind: the stack this borrow sits on, the
         ;; same on the way in after a resume as on the first entry.
         (owner (get-thread-id))
         (outer-stack (current-interrupt-poll-stack))
         ;; This wind's own edges are masked (jolt-wind*, locks.ss). A deferred
         ;; interrupt re-arms a short retry, and when the body returned normally
         ;; that retry could land at this after-thunk's entry, the winder already
         ;; popped: the escape then jumped back into an extent that had already
         ;; returned.
         (lv (jolt-locks-held))
         (r (call/cc
              (lambda (k)
                (jolt-locks-enter!)
                (dynamic-wind
                  (lambda ()
                    (let ((check
                           (lambda ()
                             (cond
                               ((not (and (box? token) (unbox token)))
                                (jolt-tick-set! jolt-tick-slot-poll interrupt-check-ticks))
                               ;; Not while this thread holds a counted lock. The
                               ;; escape skips any cleanup that has not started, and
                               ;; jolt-with-mutex's edges are the place that costs: an
                               ;; escape between the count and the wind left the count
                               ;; up for good (no preemption on that carrier again,
                               ;; every park refused), and one at the after-thunk's
                               ;; entry — the winder already popped — left the mutex
                               ;; held. The count goes up first and comes down last
                               ;; (host/chez/locks.ss), so a zero count is outside
                               ;; both edges. The preempt handler refuses on the same
                               ;; test and retries the same way.
                               ((fx=? 0 (jolt-locks-held)) (k interrupt-sentinel))
                               (else (jolt-tick-set! jolt-tick-slot-poll
                                                     (jolt-fiber-preempt-retry-ticks)))))))
                      ;; Push is the poll-ownership linearization point.
                      (interrupt-poll-stack (cons owner (cons check outer-stack)))
                      (jolt-tick-set! jolt-tick-slot-poll interrupt-check-ticks)))
                  (jolt-masked-body thunk)
                  ;; Runs on every way out of the body, the park included. Pop,
                  ;; then poll for the enclosing borrow if there is one: a poll
                  ;; that fires between the two runs the enclosing check, which
                  ;; is the right one either way. On a park the enclosing
                  ;; borrow's own after-thunk runs next and cancels it.
                  (lambda ()
                    (interrupt-poll-stack (cons owner outer-stack))
                    (jolt-tick-set! jolt-tick-slot-poll
                                    (if (pair? outer-stack) interrupt-check-ticks 0))
                    (when (fx>? (jolt-locks-held) lv) (jolt-locks-exit!))))))))
    (if (eq? r interrupt-sentinel)
        (begin
          ;; The bindings the interrupted body left. Its winds are masked at their
          ;; edges, but a `binding` pops in a user finally, and the escape can land
          ;; in that finally's code before the pop. Everything inside the extent was
          ;; abandoned, so the stack goes back to what it was on the way in.
          (dyn-binding-stack outer-bindings)
          (jolt-throw (jolt-ex-info "Evaluation interrupted" (jolt-hash-map jolt-kw-interrupted #t))))
        r)))
(def-var! "jolt.host" "make-interrupt" jolt-make-interrupt)
(def-var! "jolt.host" "interrupt!" jolt-interrupt!)
(def-var! "jolt.host" "interrupted?" jolt-interrupted?)
(def-var! "jolt.host" "run-interruptible" jolt-run-interruptible)

;; --- java.lang.Thread / java.util.concurrent.CountDownLatch -----------------
;; Real OS threads over Chez fork-thread (shared heap — a captured atom/var is
;; shared). A Thread runs its Runnable thunk; start forks, join waits on a
;; condition latched at completion. CountDownLatch is a counting barrier.
;; Slot 6 is the thread's NAME, in a box so .setName after .start reaches the
;; running thread through the same box the start published. Unnamed threads get
;; the JVM's construction-order default rather than nothing, so .getName has an
;; answer before the thread has an id to be named after.
(define jthread-name-counter 0)
(define jthread-name-mutex (make-mutex))
(define (next-jthread-name)
  (jolt-with-mutex jthread-name-mutex
    (let ((n jthread-name-counter))
      (set! jthread-name-counter (+ n 1))
      (string-append "Thread-" (number->string n)))))
;; Slots 8 and 9, daemon and priority, start as the CREATING thread's, which is
;; the JVM's rule: a Thread made on a daemon thread is a daemon, and one made on a
;; thread of priority 3 has priority 3.
(define (make-jthread thunk name)
  (let ((me (get-thread-id)))
    (let ((t (make-thread-object thunk (or name (next-jthread-name)) #f #f
                                 (jolt-thread-daemon? me) (jolt-thread-priority me) (box #f))))
      ;; its creator's group, as on the JVM
      (vector-set! (jhost-state t) 12 (thread-group-of (current-thread-handle)))
      t)))
;; slot 7: the id of the thread the start forked, #f until then. A rename needs
;; it to reach the id-keyed name table the handles read.
(define (jthread-id st) (vector-ref st 7))
;; slot 8: daemon. The JVM keeps the process alive until every non-daemon thread
;; has finished, and exits regardless of the daemon ones; a thread is what its
;; flag said when it STARTED, and setDaemon on a live thread is an
;; IllegalThreadStateException. jolt used to exit the moment -main returned,
;; live threads or not, and accept setDaemon silently, so a program whose work
;; ran on a Thread it started lost that work at exit. The fork counts a non-
;; daemon thread in and its end counts it out (lazy-bridge.ss), whatever started
;; it; the CLI's normal-return path and a built binary's launcher wait before the
;; shutdown hooks run (jolt-await-user-threads!). System/exit does not wait --
;; neither does the JVM's.
(define (jthread-daemon? st) (vector-ref st 8))
;; The runtime id of a thread that is running, else #f. While it runs its name,
;; daemon status and priority are the id-keyed tables' (every object for it reads
;; the same); once it has finished they are the object's own slots, which the exit
;; hook (io.ss) filled before the tables let go.
(define (jthread-running-id st) (and (not (vector-ref st 1)) (jthread-id st)))
;; Read one of those: the table's answer, unless the thread finished while it was
;; read — the hook sets done before it deletes the entry, so a read that missed
;; the entry for that reason sees done and takes the slot.
(define (jthread-attr st table-ref slot-ref)
  (let ((id (jthread-running-id st)))
    (if id
        (let ((v (table-ref id)))
          (if (vector-ref st 1) (slot-ref st) v))
        (slot-ref st))))
;; alive = started and not yet completed. join waits on exactly this, so a thread
;; that was never started is not waited for at all (JVM: isAlive is false before
;; .start, so join returns at once).
(define (jthread-alive? st) (and (vector-ref st 5) (not (vector-ref st 1))))
;; Under st's mutex: the thread is done. The object takes the name, priority and
;; daemon status the id-keyed tables held, is marked done and its joiners woken,
;; in that order (jthread-attr relies on it) — ONCE. The body's end and the exit
;; hook (io.ss) both finish a thread, and the hook runs after the body has
;; already woken join; copying the tables again there overwrote whatever the
;; joiner had set since. Thread.setDaemon is legal on a finished thread, so
;; (.join t) (.setDaemon t true) (.isDaemon t) answered false whenever the hook
;; landed between the two.
(define (jthread-finish! st id)
  (unless (vector-ref st 1)
    (set-box! (vector-ref st 6) (jolt-thread-name id))
    (vector-set! st 9 (jolt-thread-priority id))
    (vector-set! st 8 (jolt-thread-daemon? id))
    (vector-set! st 1 #t)
    (jolt-cv-wake! (vector-ref st 3))))
;; JVM Thread() is legal: the target is null and run/start are no-ops.
;; Thread(), Thread(runnable) and Thread(runnable, name) — the name argument used
;; to be accepted and dropped, so a thread the caller had named answered with a
;; generated one.
;; The group forms too — Thread(group, target), Thread(group, target, name),
;; Thread(group, target, name, stackSize) and Thread(group, name) — and
;; Thread(String name), whose lone string is the name and not a target.
(define (thread-ctor-args args)            ; -> (group target name)
  (let* ((group (and (pair? args) (thread-group? (car args)) (car args)))
         (rest (if group (cdr args) args))
         (str-arg (lambda (x) (if (jolt-nil? x) #f (jolt-final-str x)))))
    (cond ((null? rest) (list group #f #f))
          ((string? (car rest)) (list group #f (car rest)))           ; (name)
          (else (list group (if (jolt-nil? (car rest)) #f (car rest))
                      (if (pair? (cdr rest)) (str-arg (cadr rest)) #f))))))
(for-each (lambda (nm)
            (register-class-ctor! nm
              (lambda args
                (let* ((a (thread-ctor-args args))
                       (t (make-jthread (cadr a) (caddr a))))
                  (when (car a) (vector-set! (jhost-state t) 12 (car a)))
                  t))))
          '("Thread" "java.lang.Thread"))
(register-host-methods! "user-thread"
  ;; another thread's frames live on its own continuation, which this thread
  ;; cannot walk: an empty StackTraceElement[]
  (list
        ;; the calling thread's frames, reconstructed the way an uncaught error's
        ;; backtrace is (source-registry.ss); another thread's stack is not
        ;; reachable, so it answers an empty array
        ;; Self is the running fiber's own object on a fiber (a virtual thread's
        ;; object has no thread id), the thread's on a thread.
        (cons "getStackTrace" (lambda (self)
          (let* ((st (jhost-state self)) (f (jolt-current-fiber)))
            (if (if f (eq? (jthread-fiber st) f) (eqv? (jthread-id st) (get-thread-id)))
                (jolt-current-stack-trace)
                (jolt-vector)))))
        (cons "getId" (lambda (self) (vector-ref (jhost-state self) 10)))
        (cons "threadId" (lambda (self) (vector-ref (jhost-state self) 10)))
        (cons "getContextClassLoader" (lambda (self) (thread-context-class-loader)))
        (cons "start" (lambda (self)
          (let ((st (jhost-state self)))
            (when (vector-ref st 5)
              (jolt-throw (jolt-host-throwable "java.lang.IllegalThreadStateException"
                                               "Thread already started")))
            (vector-set! st 5 #t)  ; mark started before forking
            (if (jthread-fiber st)
                (start-virtual-thread! self)
            ;; born with the object's daemon flag; the object is registered as
            ;; the thread's by the parent once the fork returns (below) and by the
            ;; child as its first act, so it is what every way of asking finds
            (let ((t (fork-thread/daemon (jthread-daemon? st) (lambda ()
               (*txn* #f)                          ; child thread must not inherit parent's txn
               (rdr-default-modes!)                ; and not the reader modes of a read it forked from
               ;; Adopt the Thread object's own interrupt flag, so .interrupt from
               ;; outside and this thread's Thread/currentThread view are ONE flag.
               (adopt-interrupt-box! (vector-ref st 4))
               ;; publish the name under this thread's id, so .getName agrees
               ;; whether it is asked of the Thread object or of the handle
               ;; Thread/currentThread (and getAllStackTraces) hands out
               (vector-set! st 7 (get-thread-id))
               (jolt-thread-name-set! (get-thread-id) (unbox (vector-ref st 6)))
               (jolt-thread-priority-set! (get-thread-id) (vector-ref st 9))
               (register-thread-object! (get-thread-id) self)
               (thread-handle-cell (cons (get-thread-id) self))
               ;; Thread.start conveys NOTHING: the new thread begins with no
               ;; thread bindings on the JVM, and bound-fn is how a body carries
               ;; the caller's in. Chez hands a forked thread the forking thread's
               ;; parameter values, so the empty stack is installed explicitly —
               ;; a snapshot restored here used to make a body read the caller's
               ;; binding where the JVM reads the root.
               (dyn-binding-stack '())
               ;; surface a thread body's throw like the JVM's default uncaught-
              ;; exception handler; the thread still completes (isAlive/join
              ;; semantics unchanged). Reporting failures are swallowed.
              (guard (e (#t (jolt-thread-uncaught! self e "Exception in Thread body:\n")))
                ;; runnable->thunk and not a bare jolt-invoke: Thread(Runnable)
                ;; accepts any Runnable on the JVM, and a FutureTask is one (it is
                ;; registered as such in the class graph). Invoking it directly
                ;; hung — a FutureTask is a shim value, not a procedure.
                (let ((th (vector-ref st 0))) (when th (jolt-invoke (runnable->thunk th)))))
              (jolt-with-mutex (vector-ref st 2)
                 (jthread-finish! st (get-thread-id)))))))
              (let ((id (sa-thread-id-of t)))
                (when id
                  ;; the id-keyed name and priority first: getName and
                  ;; getPriority read them as soon as slot 7 is set
                  ;; ...unless the child already has, and may have renamed itself
                  ;; Nothing is registered for a thread that has already
                  ;; finished: its exit hook has run, and an entry made now
                  ;; would outlive it (ids are never reused).
                  (jolt-with-mutex thread-handles-mutex
                    (when (jolt-started-thread? id)
                      (unless (hashtable-contains? thread-names-by-id id)
                        (hashtable-set! thread-names-by-id id (unbox (vector-ref st 6)))
                        (unless (eqv? 5 (vector-ref st 9))
                          (hashtable-set! thread-priorities-by-id id (vector-ref st 9))))
                      (hashtable-set! thread-handles-by-id id self)))
                  (jolt-with-mutex (vector-ref st 2)
                    (unless (vector-ref st 7) (vector-set! st 7 id)))))))
            jolt-nil)))
        (cons "run" (lambda (self) (let ((th (vector-ref (jhost-state self) 0))) (when th (jolt-invoke th))) jolt-nil))
        ;; join() and join(0) wait indefinitely; join(ms) waits at most ms and
        ;; returns whether or not the thread finished. The timeout used to be
        ;; discarded, which turned every bounded join into an unbounded one — a
        ;; caller that joined a long-lived worker with a timeout deadlocked instead
        ;; of giving up, with nothing in the code to suggest why. A negative timeout
        ;; is an IllegalArgumentException on the JVM, not an unbounded wait.
        (cons "join" (lambda (self . args)
          (let* ((st (jhost-state self))
                 (a (if (null? args) #f (car args)))
                 (ms (if (or (not a) (jolt-nil? a)) #f (jnum->exact a))))
            (when (and ms (negative? ms))
              (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                               "timeout value is negative")))
            ;; An absolute deadline, so a spurious wakeup resumes waiting for what
            ;; is LEFT of the timeout rather than restarting it. A joining FIBER
            ;; parks: blocking its carrier would stop every other fiber on it until
            ;; the thread it joins exits.
            (jolt-cv-wait-interruptibly "Thread.join"
                          (vector-ref st 2) (vector-ref st 3)
                          (and ms (not (= ms 0)) (ms->timeout ms))
              (lambda (timed-out?)
                (cond ((not (jthread-alive? st)) #t)
                      (timed-out? #f)
                      (else jolt-cv-again)))))
          jolt-nil))
        (cons "isAlive" (lambda (self) (jthread-alive? (jhost-state self))))
        ;; flag then poke, so a wait already parked on a condition this thread
        ;; registered is thrown out of it rather than merely told afterwards. A
        ;; virtual thread's fiber owns its box, so the poke also resumes it from a
        ;; channel wait (locks.ss jolt-interrupt-owner-set!).
        (cons "interrupt" (lambda (self . _)
          (let* ((st (jhost-state self)) (b (vector-ref st 4)))
            (set-box! b #t)
            (jolt-interrupt-wake-waits! b))
          jolt-nil))
        (cons "isInterrupted" (lambda (self) (and (unbox (vector-ref (jhost-state self) 4)) #t)))
        ;; once running, the thread's name is the id-keyed table's, which is what
        ;; an executor's ThreadFactory and a rename from inside write
        (cons "getName" (lambda (self)
          (let ((st (jhost-state self)))
            (jthread-attr st jolt-thread-name (lambda (st) (unbox (vector-ref st 6)))))))
        ;; A rename after .start has to reach the running thread too, or the two
        ;; spellings of the same thread's name disagree.
        (cons "setName" (lambda (self nm)
          (let* ((st (jhost-state self)) (s (jolt-final-str nm)))
            (set-box! (vector-ref st 6) s)
            (when (jthread-running-id st) (jolt-thread-name-set! (jthread-id st) s)))
          jolt-nil))
        ;; refused while the thread is ALIVE, as Thread.setDaemon checks isAlive():
        ;; before start and after it has finished the flag can still be set.
        ;; A virtual thread is always a daemon: false is refused before the
        ;; liveness check, as Thread.setDaemon orders them.
        (cons "setDaemon" (lambda (self flag)
          (let ((st (jhost-state self)))
            (when (and (not (jolt-truthy? flag))
                       (let ((f (jthread-fiber st))) (or (eq? f 'virtual) (jolt-fiber? f))))
              (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                               "'false' not legal for virtual threads")))
            (when (jthread-alive? st)
              (jolt-throw (jolt-host-throwable "java.lang.IllegalThreadStateException" jolt-nil)))
            (vector-set! st 8 (and (jolt-truthy? flag) #t)))
          jolt-nil))
        (cons "isDaemon" (lambda (self)
          (let ((st (jhost-state self)))
            (jthread-attr st jolt-thread-daemon? (lambda (st) (and (jthread-daemon? st) #t))))))
        ;; Priority: advisory, as the JVM documents it, and jolt schedules no
        ;; differently for it — but it is stored, validated and inherited as there,
        ;; and once started it is the thread's, so the handle reads the same value.
        (cons "getPriority" (lambda (self)
          (let ((st (jhost-state self)))
            (jthread-attr st jolt-thread-priority (lambda (st) (vector-ref st 9))))))
        ;; a virtual thread's priority is always NORM_PRIORITY; the argument is
        ;; still checked, and then ignored, as the JVM does
        (cons "setPriority" (lambda (self p)
          (let ((st (jhost-state self)) (p (jolt-thread-priority-arg p)))
            (unless (jthread-fiber st)
              (vector-set! st 9 p)
              (when (jthread-running-id st) (jolt-thread-priority-set! (jthread-id st) p))))
          jolt-nil))
        (cons "isVirtual" (lambda (self) (and (jthread-fiber (jhost-state self)) #t)))
        ;; its group while it lives; nil once it has terminated, as on the JVM
        (cons "getThreadGroup" (lambda (self) (thread-group-of self)))
        ;; NEW before start, TERMINATED after; while it runs, what it is waiting in
        ;; (locks.ss records it), else RUNNABLE. A thread object the fiber layer
        ;; answers for (jolt-thread-state-hook) reports what that says.
        (cons "getState" (lambda (self)
          (let ((st (jhost-state self)))
            (thread-state-constant
              (cond ((not (vector-ref st 5)) "NEW")
                    ((vector-ref st 1) "TERMINATED")
                    ((jolt-thread-state-hook self) => values)
                    (else (let ((w (and (jthread-id st) (thread-wait-state (jthread-id st)))))
                            (if w (symbol->string w) "RUNNABLE"))))))))
        (cons "setUncaughtExceptionHandler" (lambda (self h)
          (vector-set! (jhost-state self) 11 (if (jolt-nil? h) #f h))
          jolt-nil))
        ;; nil once the thread has terminated, whatever was set; before that the
        ;; handler set on it, else its ThreadGroup (itself a handler on the JVM)
        (cons "getUncaughtExceptionHandler" (lambda (self)
          (let ((st (jhost-state self)))
            (cond ((vector-ref st 1) jolt-nil)
                  ((vector-ref st 11) => values)
                  (else (thread-group-of self))))))))

;; Thread.toString as JDK 21 writes it: Thread[#id,name,priority,group], the
;; group left empty once the thread has terminated; a virtual thread is
;; VirtualThread[#id,name]/state (the JDK adds @carrier while it is mounted,
;; which jolt's carriers have no name for). Through the methods, so a thread
;; whose name or priority changed reports what they answer now.
(define (jthread-to-string self)
  (let ((id (number->string (jnum->exact (record-method-dispatch self "threadId" jolt-nil))))
        (nm (jolt-final-str (record-method-dispatch self "getName" jolt-nil))))
    (if (jthread-fiber (jhost-state self))
        (string-append "VirtualThread[#" id (if (string=? nm "") "" (string-append "," nm)) "]/"
                       (string-downcase (jolt-final-str (record-method-dispatch (record-method-dispatch self "getState" jolt-nil) "name" jolt-nil))))
        (let ((g (thread-group-of self)))
          (string-append "Thread[#" id "," nm ","
                         (number->string (jnum->exact (record-method-dispatch self "getPriority" jolt-nil))) ","
                         (if (jolt-nil? g) "" (vector-ref (jhost-state g) 0)) "]")))))
(register-host-methods! "user-thread" (list (cons "toString" jthread-to-string)))

;; --- uncaught exceptions ---------------------------------------------------
;; A throw that ends a thread goes to that thread's handler if it has one, else
;; to the default handler (Thread/setDefaultUncaughtExceptionHandler), else it is
;; reported — which is the JVM's dispatch through the thread's ThreadGroup. A
;; handler's own throw is ignored, as the JVM ignores it.
(define default-uncaught-handler #f)
(define (jolt-thread-uncaught! th e label)
  (let* ((x (jolt-unwrap-throw e))
         (h (or (and th (jhost? th) (vector-ref (jhost-state th) 11))
                default-uncaught-handler)))
    (if h
        (guard (_ (#t #f)) (jolt-fi-call h "uncaughtException" th x))
        (guard (_ (#t #f))
          (display label (current-error-port))
          (jolt-report-throwable e (current-error-port))))))
(let ((statics
       (list (cons "setDefaultUncaughtExceptionHandler"
                   (lambda (h) (set! default-uncaught-handler (if (jolt-nil? h) #f h)) jolt-nil))
             (cons "getDefaultUncaughtExceptionHandler"
                   (lambda () (or default-uncaught-handler jolt-nil))))))
  (register-class-statics! "Thread" statics)
  (register-class-statics! "java.lang.Thread" statics))

;; --- java.lang.ThreadGroup --------------------------------------------------
;; A minimal, consistent model: the JVM's two built-in groups, "system" and its
;; child "main" (every thread jolt runs that no one placed elsewhere is in main,
;; as the JVM's main thread, its pools' and its futures' threads are), groups a
;; program makes, and a thread's membership, which a new Thread takes from its
;; creator. activeCount counts live threads in a group and its subgroups.
;; State: #(name parent max-priority subgroups).
(define (make-thread-group name parent)
  (let ((g (make-jhost "thread-group" (vector name parent 10 '()))))
    (when parent
      (let ((pst (jhost-state parent)))
        (jolt-with-mutex thread-groups-mu
          (vector-set! pst 3 (cons g (vector-ref pst 3))))))
    g))
(define thread-groups-mu (make-mutex))
(define (thread-group? x) (and (jhost? x) (string=? (jhost-tag x) "thread-group")))
(define system-thread-group (make-thread-group "system" #f))
(define main-thread-group (make-thread-group "main" system-thread-group))
(define (thread-group-parent g) (vector-ref (jhost-state g) 1))
;; A thread's group: the one it was made in, main when nobody placed it, and nil
;; once it has terminated.
(define (thread-group-of th)
  (let ((st (jhost-state th)))
    (if (vector-ref st 1) jolt-nil (or (vector-ref st 12) main-thread-group))))
(define (thread-group-within? g ancestor)
  (let loop ((g g)) (and g (or (eq? g ancestor) (loop (thread-group-parent g))))))
(define (live-thread-objects)
  (let loop ((ids (cons (get-thread-id) (cons jolt-boot-thread-id (live-thread-ids)))) (seen '()) (acc '()))
    (cond ((null? ids) acc)
          ((memv (car ids) seen) (loop (cdr ids) seen acc))
          (else (loop (cdr ids) (cons (car ids) seen) (cons (thread-handle-for-id (car ids)) acc))))))
(define (thread-group-active-count g)
  (let loop ((ts (live-thread-objects)) (n 0))
    (if (null? ts)
        n
        (let ((tg (thread-group-of (car ts))))
          (loop (cdr ts) (if (and (thread-group? tg) (thread-group-within? tg g)) (+ n 1) n))))))
(define (thread-group-subgroups g)
  (jolt-with-mutex thread-groups-mu (vector-ref (jhost-state g) 3)))
(define (thread-group-group-count g)
  (let loop ((gs (thread-group-subgroups g)) (n 0))
    (if (null? gs) n (loop (cdr gs) (+ n 1 (thread-group-group-count (car gs)))))))
;; ThreadGroup(name) is a child of the current thread's group; ThreadGroup(parent, name).
(for-each (lambda (nm)
            (register-class-ctor! nm
              (case-lambda
                ((name) (make-thread-group (jolt-final-str name)
                                           (let ((g (thread-group-of (current-thread-handle))))
                                             (if (thread-group? g) g main-thread-group))))
                ((parent name)
                 (unless (thread-group? parent) (throw-jvm 'NullPointerException jolt-nil))
                 (make-thread-group (jolt-final-str name) parent)))))
          '("ThreadGroup" "java.lang.ThreadGroup"))
(register-host-methods! "thread-group"
  (list (cons "getName" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "getParent" (lambda (self) (or (thread-group-parent self) jolt-nil)))
        (cons "getMaxPriority" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "setMaxPriority" (lambda (self p)
          (let ((n (jnum->exact p)))
            (when (and (integer? n) (<= 1 n 10)) (vector-set! (jhost-state self) 2 n)))
          jolt-nil))
        ;; true of this group and of any group under it
        (cons "parentOf" (lambda (self g) (and (thread-group? g) (thread-group-within? g self) #t)))
        (cons "activeCount" (lambda (self) (thread-group-active-count self)))
        (cons "activeGroupCount" (lambda (self) (thread-group-group-count self)))
        (cons "isDaemon" (lambda (self) #f))
        ;; a ThreadGroup is its threads' uncaught-exception handler by default
        (cons "uncaughtException" (lambda (self th e)
          (jolt-thread-uncaught! #f e "Exception in Thread body:\n") jolt-nil))
        (cons "toString" (lambda (self)
          (string-append "java.lang.ThreadGroup[name=" (vector-ref (jhost-state self) 0)
                         ",maxpri=" (number->string (vector-ref (jhost-state self) 2)) "]")))))
(register-str-render! thread-group?
  (lambda (g) (string-append "java.lang.ThreadGroup[name=" (vector-ref (jhost-state g) 0)
                             ",maxpri=" (number->string (vector-ref (jhost-state g) 2)) "]")))
;; Thread/activeCount: the live threads in the current thread's group.
(let ((statics (list (cons "activeCount"
                           (lambda () (let ((g (thread-group-of (current-thread-handle))))
                                        (if (thread-group? g) (thread-group-active-count g) 0)))))))
  (register-class-statics! "Thread" statics)
  (register-class-statics! "java.lang.Thread" statics))

;; --- java.lang.Thread$State --------------------------------------------------
;; The enum getState answers, one object per constant so = compares them.
(define thread-state-names '("NEW" "RUNNABLE" "BLOCKED" "WAITING" "TIMED_WAITING" "TERMINATED"))
(define thread-state-constants
  (map (lambda (nm) (cons nm (make-jhost "thread-state" nm))) thread-state-names))
(define (thread-state-constant nm) (cdr (assoc nm thread-state-constants)))
;; For a thread the runtime does not track by id — a fiber's own Thread object —
;; the fiber layer answers its state name here ("WAITING" while it is parked, say),
;; or #f to fall through to the OS-thread answer.
(define jolt-thread-state-hook (lambda (th) #f))
(register-enum-class! "java.lang.Thread$State" thread-state-constants)

;; --- java.lang.management.ThreadMXBean (the current thread's CPU clock) -----
;; ManagementFactory/getThreadMXBean, for what callers time with it: the CPU time
;; of the CALLING thread, which — unlike wall time — does not grow while other
;; processes hold the CPU. Chez's thread clock counts user and system time
;; together, so getCurrentThreadUserTime answers the same total (the JVM's
;; excludes system time; it is never larger than the CPU time on either).
(define (current-thread-cpu-nanos)
  (let ((t (current-time 'time-thread)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))
(define the-thread-mx-bean (make-jhost "thread-mx-bean" #f))
;; How many mutexes the runtime has allocated so far (locks.ss).
(def-var! "jolt.host" "mutex-allocations" (lambda () (unbox jolt-mutex-allocations)))
(register-host-methods! "thread-mx-bean"
  (list (cons "getCurrentThreadCpuTime" (lambda (self) (current-thread-cpu-nanos)))
        (cons "getCurrentThreadUserTime" (lambda (self) (current-thread-cpu-nanos)))
        (cons "isCurrentThreadCpuTimeSupported" (lambda (self) #t))
        (cons "isThreadCpuTimeSupported" (lambda (self) #t))
        (cons "isThreadCpuTimeEnabled" (lambda (self) #t))))
(let ((statics (list (cons "getThreadMXBean" (lambda () the-thread-mx-bean)))))
  (register-class-statics! "ManagementFactory" statics)
  (register-class-statics! "java.lang.management.ManagementFactory" statics))

;; --- virtual threads --------------------------------------------------------
;; getState for a virtual thread is its fiber's: RUNNABLE while it runs or waits
;; for a turn, WAITING while parked, or what the park recorded (TIMED_WAITING for a
;; wait with a deadline, BLOCKED entering a monitor). NEW and TERMINATED are the
;; object's own, answered before the hook.
(set! jolt-thread-state-hook
  (lambda (th)
    (let ((f (jthread-fiber (jhost-state th))))
      (and (jolt-fiber? f)
           (case (jolt-fiber-state f)
             ((parked) (symbol->string (or (jolt-fiber-wstate f) 'WAITING)))
             ((done dead) "TERMINATED")
             (else "RUNNABLE"))))))
;; A virtual thread's group while it lives, as the JVM's: "VirtualThreads", under
;; "system".
(define virtual-thread-group (make-thread-group "VirtualThreads" system-thread-group))
;; A virtual thread is a fiber (fibers.ss), and its Thread object is the one
;; Thread/currentThread answers inside it (io.ss virtual-thread-object). Its fiber
;; slot is 'virtual on an object made by a builder and not started yet, then the fiber.
;; The body's throw is reported as an uncaught exception, as the JVM's default
;; handler does, and the fiber ends normally.
(define (start-virtual-thread! obj)
  (let* ((st (jhost-state obj))
         (r (vector-ref st 0))
         (thunk (lambda ()
                  (*txn* #f)
                  (guard (e (#t (jolt-thread-uncaught! obj e "Exception in virtual thread:\n")))
                    (when r (jolt-invoke (runnable->thunk r))))
                  jolt-nil)))
    (jolt-fiber-spawn* thunk (vector-ref st 4) (lambda (f) (bind-virtual-thread! obj f)))
    (jolt-fiber-ensure-carrier!)
    obj))
(define (unstarted-virtual-thread r name)
  (let ((obj (make-thread-object r name #f #f #t 5 (box #f))))
    (jthread-fiber-set! (jhost-state obj) 'virtual)
    (vector-set! (jhost-state obj) 12 virtual-thread-group)
    obj))
;; Thread.ofVirtual(): a builder. name(String) names every thread it makes;
;; name(prefix, start) numbers them from start. unstarted/start/factory as on the
;; JVM. State #(name counter mutex). A factory is a SNAPSHOT of its builder's name
;; and counter, counting on its own from there, and is thread-safe as the JDK's
;; is (the mutex); a builder is not, and has none.
(define (vbuilder-next-name! st)
  (define (next!)
    (let ((n (vector-ref st 1)))
      (if n
          (begin (vector-set! st 1 (+ n 1))
                 (string-append (vector-ref st 0) (number->string n)))
          (vector-ref st 0))))
  (let ((mu (vector-ref st 2)))
    (if mu (jolt-with-mutex mu (next!)) (next!))))
(define (vbuilder-unstarted st r) (unstarted-virtual-thread r (vbuilder-next-name! st)))
(register-host-methods! "vthread-builder"
  (list (cons "name" (lambda (self nm . start)
          (let ((st (jhost-state self)))
            (vector-set! st 0 (jolt-final-str nm))
            (vector-set! st 1 (if (pair? start) (jnum->exact (car start)) #f)))
          self))
        (cons "unstarted" (lambda (self r) (vbuilder-unstarted (jhost-state self) r)))
        (cons "start" (lambda (self r)
          (let ((t (vbuilder-unstarted (jhost-state self) r)))
            (vector-set! (jhost-state t) 5 #t)
            (start-virtual-thread! t))))
        (cons "factory" (lambda (self)
          (let ((st (jhost-state self)))
            (make-jhost "vthread-factory" (vector (vector-ref st 0) (vector-ref st 1) (make-mutex))))))))
(register-host-methods! "vthread-factory"
  (list (cons "newThread" (lambda (self r) (vbuilder-unstarted (jhost-state self) r)))))
(let ((statics (list (cons "ofVirtual" (lambda () (make-jhost "vthread-builder" (vector "" #f #f))))
                     (cons "startVirtualThread"
                           (lambda (r)
                             (let ((t (unstarted-virtual-thread r "")))
                               (vector-set! (jhost-state t) 5 #t)
                               (start-virtual-thread! t)))))))
  (register-class-statics! "Thread" statics)
  (register-class-statics! "java.lang.Thread" statics))

(define (make-jlatch n) (make-jhost "count-down-latch" (vector n (make-mutex) (jolt-make-cv))))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (n . _) (make-jlatch (jnum->exact n)))))
          '("CountDownLatch" "java.util.concurrent.CountDownLatch"))
(register-host-methods! "count-down-latch"
  (list (cons "countDown" (lambda (self)
          (let ((st (jhost-state self)))
            (jolt-with-mutex (vector-ref st 1)
              (when (> (vector-ref st 0) 0) (vector-set! st 0 (- (vector-ref st 0) 1)))
              (when (= (vector-ref st 0) 0) (jolt-cv-wake! (vector-ref st 2)))))
          jolt-nil))
        ;; await() waits indefinitely and is void; await(timeout, unit) waits at most
        ;; that long and answers whether the count reached zero. The timeout used to
        ;; be discarded, so the bounded form was an unbounded one — the same bug
        ;; Thread.join had, unreachable until TimeUnit existed to write the call with.
        (cons "await" (lambda (self . args)
          (let ((st (jhost-state self))
                (ms (tu-args->ms args)))
            ;; absolute deadline, so a spurious wakeup resumes waiting for what is
            ;; left of the timeout rather than restarting it
            (let ((reached (jolt-cv-wait-interruptibly "CountDownLatch.await"
                                         (vector-ref st 1) (vector-ref st 2)
                                         (and ms (ms->timeout ms))
                             (lambda (timed-out?)
                               (cond ((<= (vector-ref st 0) 0) #t)
                                     (timed-out? #f)
                                     (else jolt-cv-again))))))
              ;; await() is void; await(timeout, unit) answers whether it reached zero
              (if ms reached jolt-nil)))))
         (cons "getCount" (lambda (self) (vector-ref (jhost-state self) 0)))
         (cons "toString" (lambda (self)
           (string-append (host-object-tostring self) "[Count = " (number->string (vector-ref (jhost-state self) 0)) "]")))))

;; --- java.util.concurrent.ExecutorService / Executors ----------------------
;; A real task QUEUE served by a fixed number of worker threads (FIFO). A single
;; worker (newSingleThreadExecutor) runs tasks strictly in submission order —
;; code relies on that ordering (claxon dispatches handlers on a single-thread
;; executor and a later empty task acts as a barrier). submit returns a Future
;; whose .get waits for the result (re-raising the task's throw, like the JVM).
;; Future.get reports a task's throw as an ExecutionException wrapping it, on the
;; JVM and here — the raw throw used to come straight back out, so a caller
;; catching ExecutionException (what the JVM makes them catch) caught nothing and
;; .getCause had nothing to read. task-execution-exception (above) is the wrap,
;; the same one a clojure `future` already did on deref; the two now agree.
;; j-future state: #(status result error mutex condition); status is one of
;; new/running/done/cancelled, the same four a FutureTask carries (further down)
;; and for the same reason — cancel has to win over a task that has not started
;; and lose to one that has. It used to be a done? boolean with .cancel answering
;; #f unconditionally, which left invokeAll's deadline and invokeAny's losers with
;; nothing to cancel WITH.
;;
;; Cancelling a task that is already RUNNING finalizes the future as cancelled —
;; .get raises and the late result is dropped — and cancel(true) also interrupts
;; the thread running it (task-runners, below), as the JVM's does.
(define (make-j-future) (make-jhost "j-future" (vector 'new jolt-nil #f (make-mutex) (jolt-make-cv))))
(define (j-future-settled? status) (and (memq status '(done cancelled)) #t))
;; A missed deadline, as a value the waiter can test: eq?-unique, so no task
;; result can be mistaken for one.
(define j-future-timed-out (list 'j-future-timed-out))
;; THE RUNNER, for cancel(true). A task future records the interrupt box of the
;; thread running it while it runs — under the future's own mutex, the one its
;; claim and its completion take — so a cancel that finds it still unsettled
;; finds the runner too, and one that finds it settled interrupts nobody. Kept
;; aside rather than in the futures' vectors because three layouts share this
;; (j-future, the scheduled future that extends it, FutureTask).
(define task-runners (make-weak-eq-hashtable))
(define task-runners-mu (make-mutex))
(define (task-runner-set! fut)
  (jolt-with-mutex task-runners-mu (hashtable-set! task-runners fut (current-interrupt-box))))
(define (task-runner-clear! fut)
  (jolt-with-mutex task-runners-mu (hashtable-delete! task-runners fut)))
(define (task-runner fut)
  (jolt-with-mutex task-runners-mu (hashtable-ref task-runners fut #f)))
;; cancel(true)'s second half: the runner's flag is SET under the future's mutex
;; (so it cannot land after the task has completed and the worker moved on), and
;; the poke that throws it out of an interruptible wait comes after, outside it.
;; A stale poke reaching the worker's next task finds the flag cleared and waits
;; on (executor-worker-loop* clears it before each task, as the JVM's runWorker
;; does).
(define (task-interrupt-runner! b)
  (when b (jolt-interrupt-wake-waits! b)))
(define (j-future-complete! self thunk)
  (let ((st (jhost-state self)))
    ;; claim the task, or leave it alone: a cancelled future's thunk never runs.
    (when (jolt-with-mutex (vector-ref st 3)
            (and (eq? (vector-ref st 0) 'new)
                 (begin (vector-set! st 0 'running) (task-runner-set! self) #t)))
      (let ((r (guard (e (#t (vector-set! st 2 e) #f)) (jolt-invoke thunk))))
        (jolt-with-mutex (vector-ref st 3)
          (task-runner-clear! self)
          ;; a cancel that landed while this ran keeps the future cancelled
          (unless (eq? (vector-ref st 0) 'cancelled)
            (unless (vector-ref st 2) (vector-set! st 1 r))
            (vector-set! st 0 'done))
          (jolt-cv-wake! (vector-ref st 4)))))))
;; Future.cancel(mayInterruptIfRunning): wins over a task that has not settled,
;; running or not, and with MAY set interrupts the thread running it — which is
;; what cancel(true), future-cancel, invokeAll's deadline and invokeAny's losers
;; all mean on the JVM. It used to only mark the future, and the task ran on.
(define (j-future-cancel! self . may)
  (let* ((st (jhost-state self))
         (may? (and (pair? may) (car may)))
         (r (jolt-with-mutex (vector-ref st 3)
              (if (j-future-settled? (vector-ref st 0))
                  #f
                  (let ((b (and may? (task-runner self))))
                    (vector-set! st 0 'cancelled)
                    (when b (set-box! b #t))
                    (jolt-cv-wake! (vector-ref st 4))
                    (or b #t))))))
    (when (box? r) (task-interrupt-runner! r))
    (and r #t)))
;; get() waits for the task; get(timeout, unit) gives up at the deadline and throws
;; TimeoutException, like the JVM. The timeout used to be discarded, so the bounded
;; overload waited forever on a task that never finished.
;;
;; ON-TIMEOUT is what a missed deadline produces. Future.get throws there;
;; `deref` with a timeout value returns it instead (Clojure's deref-future
;; swallows exactly TimeoutException), so both spellings share one wait rather
;; than one of them catching what the other just threw.
;; The wait, against an ABSOLUTE deadline (or #f for none): invokeAll shares one
;; deadline across every future it waits on, so the bound is the whole call's, not
;; each task's in turn.
(define (j-future-get-until self deadline on-timeout)
  (let* ((st (jhost-state self))
         ;; the deref's short spin (task-spin-until), for an untimed get: a hint
         ;; only, the wait below reads the status again under the mutex. A timed
         ;; one does not spin, so a zero-timeout poll still answers at once.
         (_ (unless deadline (task-spin-until (lambda () (j-future-settled? (vector-ref st 0))))))
         (status (jolt-cv-wait-interruptibly "Future.get"
                             (vector-ref st 3) (vector-ref st 4) deadline
                   (lambda (timed-out?)
                     (cond ((j-future-settled? (vector-ref st 0)) (vector-ref st 0))
                           (timed-out? #f)
                           (else jolt-cv-again))))))
    (cond ((not status) (on-timeout))
          ((eq? status 'cancelled)
           (jolt-throw (jolt-host-throwable "java.util.concurrent.CancellationException"
                                            "task was cancelled")))
          ((vector-ref st 2) (jolt-throw (task-execution-exception (vector-ref st 2))))
          (else (vector-ref st 1)))))
(define (j-future-get* self ms on-timeout)
  (j-future-get-until self (and ms (ms->timeout ms)) on-timeout))
(define (future-timeout-throw)
  ;; no message: FutureTask.get's TimeoutException carries none on the JVM
  (jolt-throw (jolt-host-throwable "java.util.concurrent.TimeoutException" jolt-nil)))
(define (j-future-get self . args)
  (j-future-get* self (tu-args->ms args) future-timeout-throw))
(register-host-methods! "j-future"
  (list (cons "get" j-future-get)
        (cons "isDone" (lambda (self) (j-future-settled? (vector-ref (jhost-state self) 0))))
        (cons "isCancelled" (lambda (self) (eq? (vector-ref (jhost-state self) 0) 'cancelled)))
        (cons "cancel" (lambda (self may) (j-future-cancel! self (jolt-truthy? may))))))
;; executor-service state: #(shutdown? queue-box queue-mutex task-cond
;; live-workers advisory-queue-capacity core-workers max-workers keep-alive-ms
;; idle-workers queue-depth term-cond starting-workers delayed lead seq) — the
;; capacity is #f except for a ThreadPoolExecutor built with an ArrayBlockingQueue;
;; .getQueue's view subtracts the live depth from it. starting-workers is the
;; forked-but-not-yet-arrived count the growth rule adds to idle-workers; see it
;; for why. The last three belong to the scheduled pools (further down): the
;; delayed tasks in due order, the token of the idle worker timing the head of
;; that list, and the submission counter that orders equal due times.
;;
;; TWO CONDITIONS, one mutex. task-cond carries "a task is queued, or the pool is
;; shutting down" to the WORKERS, which are threads; term-cond carries "shut down
;; and the last worker is gone" to awaitTermination, which a fiber can reach. They
;; were one condition, and an enqueue therefore broadcast to every waiter on it:
;; with 130 idle workers, one task woke all 130, and 129 of them took the queue
;; mutex, found the task already claimed and parked again. Fire-and-forget cost
;; 152us per no-op task that way, against 5.2us on the JVM (and 26.5us with the
;; fixed 32-worker pool this replaced, which woke 32), and the herd fed the
;; growth rule below — workers that slow to return look like workers that are not
;; coming, so a 200k-task burst grew the pool to 134 threads, which made the herd
;; bigger still. Split, an enqueue signals exactly ONE worker (jolt-cv-signal-one!,
;; locks.ss, where the preconditions this relies on are written out): the same
;; no-op task costs 8.9us and the same burst needs 7 threads, which is the pool the
;; JVM grows for it too.
;; queue-box holds a pair (out . in) — out is the dequeue head-list, in is the
;; enqueue tail-list (reversed). Enqueue conses onto in (O(1)); dequeue pops from
;; out, reversing in into out when out is empty (amortized O(1)). queue-depth is
;; that pair's length, maintained rather than walked: the growth decision below
;; reads it on every enqueue.
;;
;; A POOL THAT GROWS, because two of the factories on the JVM have no fixed size
;; at all. The shape is java.util.concurrent.ThreadPoolExecutor's, with the three
;; numbers that decide when a thread is created and when one is retired:
;;
;;   core-workers   forked at construction and never retired.
;;   max-workers    the ceiling live-workers may grow to.
;;   keep-alive-ms  how long a worker ABOVE core may sit idle before it exits,
;;                  or #f for a pool whose workers never expire.
;;
;; A fixed pool is core = max, keep-alive #f — every worker eager, none retired,
;; which is what newFixedThreadPool and newSingleThreadExecutor were already and
;; still are (single-thread submission ORDER depends on it: one worker, forever).
;; Eager is jolt's own and predates this: the JVM creates even CORE threads on
;; demand, so a fixed pool's getPoolSize is 0 there until the first task. Forking
;; them up front costs the threads the caller named and nothing more, and it is
;; the behaviour every pool here has had, so it stays.
;;
;; newCachedThreadPool is core 0, max unbounded, keep-alive 60s — the JVM's own
;; (0, Integer.MAX_VALUE, 60L, SECONDS), and newVirtualThreadPerTaskExecutor gets
;; the same pool.
;;
;; WHEN A THREAD IS CREATED is the part worth stating exactly, because the JVM
;; expresses it through a data structure jolt does not have. There, a cached pool
;; hands tasks over a SynchronousQueue, whose offer succeeds only if a worker is
;; already parked in a take; when it fails the executor starts a thread. So the
;; rule is "a task that no idle worker is waiting to accept starts one", and that
;; is what executor-enqueue! tests directly: queue-depth > idle-workers, decided
;; under the queue mutex, so an idle worker cannot have claimed the task between
;; the test and the fork. Tasks still go through the one unbounded queue either
;; way, which is why the test is a depth against a count rather than a handoff.
(define executor-unbounded-workers 2147483647)   ; Integer.MAX_VALUE, as the JVM passes
(define cached-pool-keep-alive-ms 60000)         ; 60L, TimeUnit.SECONDS, as the JVM passes
;; SPEC is (daemon factory), both optional: slots 16 and 17, what the pool's
;; workers are born as. A pool Executors builds with its defaultThreadFactory has
;; non-daemon workers, which is the default; a ForkJoin-style pool's are daemons;
;; and a ThreadFactory the caller passed decides for itself, per worker, through
;; the Thread its newThread answers (executor-spawn-worker!).
(define (make-executor* tag core-n max-n keep-alive-ms cap . spec)
  (let ((self (make-jhost tag
                          (vector #f (box (cons '() '())) (make-mutex) (jolt-make-cv) 0
                                  cap core-n max-n keep-alive-ms 0 0 (jolt-make-cv) 0
                                  '() #f 0
                                  (and (pair? spec) (car spec) #t)
                                  (and (pair? spec) (pair? (cdr spec)) (cadr spec))
                                  '() #f #f 0))))
    (let ((st (jhost-state self)))
      ;; The core workers, eagerly. Above core, a worker appears when a task
      ;; arrives with nobody idle to take it, and not before: a cached pool that
      ;; is never used costs no threads.
      (let spawn ((k core-n))
        (when (fx>? k 0)
          (when (jolt-with-mutex (vector-ref st 2) (executor-claim-worker! st))
            (executor-spawn-worker! st))
          (spawn (fx- k 1))))
      self)))
;; newCachedThreadPool / newVirtualThreadPerTaskExecutor. SPEC as make-executor*.
(define (make-cached-executor . spec)
  (apply make-executor* "executor-service" 0 executor-unbounded-workers cached-pool-keep-alive-ms #f spec))
;; newScheduledThreadPool / newSingleThreadScheduledExecutor / the
;; ScheduledThreadPoolExecutor ctor: a fixed pool (the JVM's grows to
;; Integer.MAX_VALUE on paper and never does in practice, its queue being
;; unbounded) whose tag adds the three schedule methods.
(define (make-scheduled-executor n-workers . spec)
  (apply make-executor* "scheduled-executor" n-workers n-workers #f #f spec))

;; The ThreadFactory argument of an Executors factory or a pool constructor, or
;; #f: anything answering newThread. (A RejectedExecutionHandler in the same
;; position of the other ThreadPoolExecutor overload does not.)
(define (thread-factory-arg x)
  (and (not (jolt-nil? x)) (not (procedure? x))
       (or (iface-method x "newThread" 2)
           (and (jhost? x) (host-method-ref (jhost-tag x) "newThread")))
       x))
(define (thread-factory-in args)
  (let loop ((as args)) (cond ((null? as) #f) ((thread-factory-arg (car as)) => values) (else (loop (cdr as))))))

;; Claim a worker slot, or answer #f because the pool is at max. Called with the
;; queue mutex HELD, and the slot is claimed BEFORE the fork rather than counted
;; by the new thread on arrival: live-workers is what isTerminated and
;; awaitTermination read, so a count that dipped between the decision and the
;; thread's first instruction would report a pool terminated while a task it
;; accepted was still on its way to a worker. It is also what keeps two
;; concurrent enqueues from both spawning past max.
;;
;; STARTING (st 12) is claimed here too, and dropped on the new worker's first trip
;; through executor-take-job!. It is what keeps the growth rule below from counting
;; a worker that is on its way as a worker that is not coming — see the rule for
;; what that costs when it is missing.
(define (executor-claim-worker! st)
  (and (fx<? (vector-ref st 4) (vector-ref st 7))
       (begin (vector-set! st 4 (fx+ (vector-ref st 4) 1))
              (vector-set! st 12 (fx+ (vector-ref st 12) 1))
              #t)))
;; Fork the thread for a slot already claimed, OUTSIDE the mutex. A fork that
;; fails gives the slot back; whether that is the caller's problem depends on
;; whether anything is left to run the task — with other workers live it waits
;; for one of them, with none it would wait forever, and a submit whose task can
;; never run is a failure the caller has to see (the JVM's own answer to a thread
;; it cannot create is to reject the task, not to queue it silently).
(define (executor-spawn-worker! st)
  (guard (e (#t (let ((none-left? (jolt-with-mutex (vector-ref st 2)
                                    (vector-set! st 4 (fx- (vector-ref st 4) 1))
                                    (vector-set! st 12 (fx- (vector-ref st 12) 1))
                                    (jolt-cv-wake! (vector-ref st 11))
                                    (fx=? 0 (vector-ref st 4)))))
                  (when none-left? (raise e)))))
    ;; A pool with a ThreadFactory hands it the worker, as the JVM's does —
    ;; newThread(worker) — and STARTS the Thread it answers: the worker runs on
    ;; that Thread, so Thread/currentThread in a task is it (its name, daemon
    ;; flag, priority and uncaught-exception handler), and a factory that wraps
    ;; the Runnable it was handed sees its wrapper run. Without a factory, the
    ;; worker is a thread of the pool's own daemon status (a factory answering
    ;; nil gets the same).
    (let* ((worker (lambda ()
                     (*txn* #f)                ; not the creating thread's txn
                     (rdr-default-modes!)      ; nor the reader modes of a read it forked from
                     (executor-worker-loop st)
                     jolt-nil))
           (factory (vector-ref st 17))
           (t (and factory
                   (let ((t (record-method-dispatch factory "newThread" (jolt-list worker))))
                     (and (not (jolt-nil? t)) t)))))
      (if t
          (record-method-dispatch t "start" jolt-nil)
          (fork-thread/daemon (vector-ref st 16)
            (lambda ()
              (guard (e (#t (jolt-thread-uncaught! (current-thread-handle) e
                                                   "Exception in executor task:\n")))
                (worker))))))))

;; Dequeue, with the mutex held. Callers test queue-depth first.
(define (executor-dequeue! st)
  (let ((q (unbox (vector-ref st 1))))
    (when (null? (car q))                       ; amortized reverse of the tail
      (set-car! q (reverse (cdr q)))
      (set-cdr! q '()))
    (let ((out (car q)))
      (set-car! q (cdr out))
      (vector-set! st 10 (fx- (vector-ref st 10) 1))
      (car out))))
;; Park until something changes: a task arrives, the pool shuts down, a delayed
;; task comes due, or (for a worker above core) the keep-alive deadline DL (mono
;; ms, or #f) passes. The idle count is raised before the wait releases the mutex
;; and dropped after it retakes it, so the growth test in executor-enqueue! sees
;; exactly the workers that are available to take a task.
;;
;; ONE LEADER times the delayed head; the rest wait untimed. With every idle worker
;; timing it, a due task woke all of them for the one that takes it — the herd the
;; two conditions above exist to avoid, once per due time. This is the JVM's own
;; DelayedWorkQueue leader/follower: the first idle worker to find the head
;; unclaimed claims it (st 14 holds its token) and waits until the head is due;
;; every later one, seeing a leader, waits for a signal. The claim is released on
;; the way out ONLY IF IT IS STILL THIS WORKER'S — a schedule that landed an
;; earlier head demoted the leader (executor-delayed-add!) and signalled a
;; replacement, and that replacement's claim must not be cleared by the stale
;; leader waking on the deadline it was timing.
(define (executor-idle-wait! st dl)
  (let* ((head (executor-delayed-head st))
         (token (and head (not (vector-ref st 14)) (list 'lead)))
         (wake (cond ((not token) dl)
                     ((and dl (< dl (sf-due head))) dl)
                     (else (sf-due head)))))
    (when token (vector-set! st 14 token))
    (vector-set! st 9 (fx+ (vector-ref st 9) 1))
    (if wake
        (jolt-condition-wait (vector-ref st 3) (vector-ref st 2) (jolt-deadline->timeout wake))
        (jolt-condition-wait (vector-ref st 3) (vector-ref st 2)))
    (vector-set! st 9 (fx- (vector-ref st 9) 1))
    (when (and token (eq? (vector-ref st 14) token)) (vector-set! st 14 #f))))
;; With the mutex held, by a worker leaving the idle set for a job or for good: if
;; delayed tasks remain with nobody timing the head, hand the timed wait to one
;; parked worker. The signalled worker re-polls, finds the head unclaimed and
;; claims it; signalling with nobody parked would be a wake nobody hears, and the
;; next worker to finish its job claims the head on its own way through
;; executor-take-job!.
(define (executor-lead-handoff! st)
  (when (and (pair? (vector-ref st 13)) (not (vector-ref st 14)) (fx>? (vector-ref st 9) 0))
    (jolt-cv-signal-one! (vector-ref st 3))))
;; Leave the pool: drop out of the live count and announce it, since
;; awaitTermination waits on exactly this reaching zero. Answers #f, the "no job,
;; you are done" answer executor-take-job! returns.
;;
;; UNDER THE SAME MUTEX HOLD AS THE DECISION TO GO, which is the whole reason it
;; is a procedure and not two lines in the worker loop. Released in between, the
;; count says a worker is live for a moment after it has stopped taking work, and
;; an enqueue in that moment reads it and declines to grow — with max reached it
;; declines for good, and the task waits for a worker that has already left. A TPE
;; of (0, 1, 0ms) strands a task that way on the first idle gap. Deciding and
;; decrementing under one hold makes the two orders the only two: an enqueue
;; before it sees this worker idle and waiting (and needs no new one, because the
;; wake sends it back to the queue), one after sees the count without it.
(define (executor-worker-exit! st)
  (vector-set! st 4 (fx- (vector-ref st 4) 1))
  (executor-lead-handoff! st)
  ;; Announced on term-cond only, and only to a waiter that can care: what
  ;; awaitTermination waits for is live 0 with the pool shut down, so a keep-alive
  ;; retirement in a RUNNING pool wakes nobody at all. (isTerminated polls.)
  (when (or (vector-ref st 0) (fx=? 0 (vector-ref st 4)))
    (jolt-cv-wake! (vector-ref st 11)))
  #f)
;; The next task for this worker, or #f meaning it has exited — because the pool
;; is shut down and drained, or because this worker is above core and has been
;; idle for keep-alive. Runs with the mutex held, and only ever exits with the
;; queue empty, so no task is left with nobody to run it. Drained means BOTH
;; queues: a delayed one-shot still runs after shutdown (the JVM's default
;; executeExistingDelayedTasksAfterShutdownPolicy), so a worker waits out its
;; delay rather than leaving it behind; shutdown has already dropped the periodic
;; tasks, which are the ones that would otherwise keep the pool alive forever.
;;
;; The keep-alive deadline is absolute and survives the loop, so a broadcast that
;; woke every idle worker for one task resumes the losers waiting for what is LEFT
;; of their keep-alive rather than restarting it — the same reason Thread.join and
;; the waits above carry deadlines rather than durations.
(define (executor-take-job! st)
  (let poll ((deadline #f))
    (cond ((executor-stopping? st) (executor-worker-exit! st))    ; shutdownNow: take nothing
          ((fx>? (vector-ref st 10) 0) (executor-lead-handoff! st) (executor-dequeue! st))
          ((executor-delayed-due? st)                      ; the head's time has come
           (let ((sf (executor-delayed-pop! st)))
             (executor-lead-handoff! st)
             (lambda () (dyn-binding-stack '())          ; no conveyance, as submit
                        (scheduled-future-run! sf st))))
          ((and (vector-ref st 0) (null? (vector-ref st 13)))
           (executor-worker-exit! st))                     ; shutdown + drained
          ;; above core, or any worker once allowCoreThreadTimeOut is on
          ((and (vector-ref st 8) (or (vector-ref st 19) (fx>? (vector-ref st 4) (vector-ref st 6))))
           (let ((dl (or deadline (mono-deadline (vector-ref st 8)))))
             (cond ((mono-due? dl) (executor-worker-exit! st))   ; idle past keep-alive
                   ((executor-idle-spin! st) (poll dl))
                   (else (executor-idle-wait! st dl) (poll dl)))))
          ((executor-idle-spin! st) (poll deadline))
          (else (executor-idle-wait! st #f) (poll deadline)))))
;; Before an untimed park, a short spin with the mutex RELEASED, watching the queue
;; depth and the shutdown flag: a burst of tasks (a fan-out, a submit/get loop)
;; refills the queue within microseconds, and a worker that is still spinning takes
;; the next task without the condition's wake, which costs the waker a signal and
;; this thread a kernel wake and a reschedule. The JVM's cached pool hands over
;; through a SynchronousQueue, which spins before it parks for the same reason.
;;
;; The spinner counts as IDLE (st 9), so the growth rule in executor-enqueue! sees
;; it as a taker and forks no worker for a task it is about to take. An enqueue's
;; signal finds no thread parked in the condition on its account (the condition's
;; own count is of parked threads), so a lone spinner costs the enqueue nothing; with
;; others parked, one of them may also wake, find the task taken, and park again,
;; which is the spurious wake every waiter here already re-checks for. The depth is
;; read without the mutex, as a hint: whatever it says, the caller re-polls under
;; the mutex. Called and returning with the mutex HELD.
;;
;; NOT while delayed tasks are pending (a scheduled pool's st 13), and a schedule
;; landing during the spin ends it. An idle worker is what times the delayed head
;; (executor-idle-wait!'s leader), and executor-lead-handoff! / a schedule signal
;; the condition for one when the idle count says one is parked: a spinner in
;; that count is not parked, so the signal reached nobody and the head went
;; untimed — periodic tasks skipped runs and missed their cancellation at
;; shutdown (the corpus's scheduleAtFixedRate rows).
(define (executor-idle-spin! st)
  (and (null? (vector-ref st 13))
       (let ((mu (vector-ref st 2))
             (ready? (lambda () (or (fx>? (vector-ref st 10) 0) (vector-ref st 0)
                                    (pair? (vector-ref st 13))))))
         (vector-set! st 9 (fx+ (vector-ref st 9) 1))
         (jolt-unlock! mu)
         (let ((found (task-spin-until ready?)))
           (jolt-lock! mu)
           (vector-set! st 9 (fx- (vector-ref st 9) 1))
           ;; Asked again under the mutex: an enqueue between the spin's last look
           ;; and the lock counted this worker idle, so it neither grew the pool
           ;; nor reached a parked waiter, and parking now would strand its task.
           (or found (ready?))))))
;; A worker that dies must not take the pool's accounting with it. Everything a
;; TASK can throw is already caught at the task (submit's future keeps it, execute
;; reports it), so reaching this handler means the queue mechanics threw — which
;; nothing in them does today. Unhandled, it would leave live-workers counting a
;; thread that is gone: isTerminated would never answer true and awaitTermination
;; would wait out its whole deadline on a pool that is finished. The bookkeeping
;; here is the same one a retiring worker does, and the next enqueue replaces the
;; worker for free, because a live count below max with a task waiting is exactly
;; what the growth rule starts one on (the JVM replaces an abruptly-dead worker
;; too, for the same reason).
(define (executor-worker-loop st)
  ;; STARTING (st 12) is this worker's from the claim until it first reaches the
  ;; queue. The flag lives out here so the guard can give the count back for a
  ;; worker that dies before it ever arrives: a LEAKED start is worse than the
  ;; over-spawn it prevents, because the growth rule would then read a taker that
  ;; is never coming and decline to grow for good — a pool that strands the task
  ;; instead of one that runs it on a thread too many.
  ;; Slot 18 lists the live workers' interrupt boxes, so shutdownNow can
  ;; interrupt the tasks they are running, as the JVM's does.
  (let ((box (current-interrupt-box)))
    (jolt-with-mutex (vector-ref st 2) (vector-set! st 18 (cons box (vector-ref st 18))))
    (dynamic-wind
      void
      (lambda () (executor-worker-loop* st))
      (lambda ()
        (jolt-with-mutex (vector-ref st 2) (vector-set! st 18 (remq box (vector-ref st 18))))))))
(define (executor-worker-loop* st)
  (let ((arriving? #t))
    (define (arrived!)
      (when arriving?
        (set! arriving? #f)
        (vector-set! st 12 (fx- (vector-ref st 12) 1))))
    ;; A worker that ends ABRUPTLY — an executed task threw — gives its slot
    ;; back and, unless the pool is stopping, starts its replacement, as the
    ;; JVM's processWorkerExit does; then the throw goes on to end this thread,
    ;; where its uncaught-exception handler sees it (executor-spawn-worker!).
    (guard (e (#t (executor-task-done! st)
                  (let ((again? (jolt-with-mutex (vector-ref st 2)
                                  (arrived!) (executor-worker-exit! st)
                                  (and (not (executor-stopping? st))
                                       (executor-claim-worker! st)))))
                    (when again? (executor-spawn-worker! st))
                    (raise e))))
      (let loop ()
        (let ((job (jolt-with-mutex (vector-ref st 2)
                     ;; Out of STARTING and into whatever executor-take-job! decides,
                     ;; under that ONE hold: this worker leaves it either running the
                     ;; job it dequeues or counted in idle-workers by
                     ;; executor-idle-wait!, and is never absent from all three.
                     (arrived!)
                     (executor-take-job! st))))
          ;; the JVM's runWorker clears the worker's interrupt before each task
          ;; (unless the pool is stopping), so a cancel(true) that reached the
          ;; last task is not carried into the next one
          (when job
            (unless (executor-stopping? st) (set-box! (current-interrupt-box) #f))
            (job)
            (executor-task-done! st)
            (loop)))))))

;; shutdown: stop accepting, let what is queued drain — the delayed one-shots
;; included, which the workers wait out (executor-take-job!). The periodic tasks
;; are cancelled and dropped here, as ScheduledThreadPoolExecutor.onShutdown does
;; under its default policies, and so are the entries a cancel already settled:
;; the JVM leaves a cancelled entry in the queue until then (removeOnCancel is off
;; by default), and a pool that waited out a cancelled task's whole delay before
;; it could terminate would be waiting for nothing.
(define (executor-shutdown! st)
  (vector-set! st 0 #t)
  (jolt-with-mutex (vector-ref st 2)
    (let ((delayed (vector-ref st 13)))
      (unless (null? delayed)
        (for-each (lambda (sf) (when (sf-periodic? sf) (j-future-cancel! sf))) delayed)
        (vector-set! st 13 (filter (lambda (sf) (not (sf-settled? sf))) delayed))))
    (jolt-cv-wake! (vector-ref st 3))       ; every idle worker: leave, or re-time the head
    (jolt-cv-wake! (vector-ref st 11))))    ; awaitTermination: re-read the flag

;; shutdownNow: stop accepting AND drop what is queued, answering the dropped
;; tasks in queue order — which is the whole difference between the two methods and
;; the whole point of this one. It used to set the flag, answer an empty vector, and
;; leave the queue to drain, so the method that exists to say "do not run the rest"
;; ran the rest: a caller shutting a pool down hard because its work had become
;; irrelevant (a cancelled request, a failing import) got every queued task
;; executed anyway, and an empty list claiming nothing had been pending.
;;
;; A dropped task's Future never completes, so a .get on it waits forever. That is
;; the JVM's behaviour too — the tasks it hands back are the pending FutureTasks
;; themselves, and nothing runs them unless the caller does — which is why they are
;; returned rather than discarded: the returned procedures are callable, and calling
;; one runs that task on the caller's thread, the same recovery .run gives there.
;;
;; The other half of shutdownNow, interrupting the tasks already running, is in
;; executor-shutdown-now! below: each worker registers its interrupt box.
(define (executor-drain-queue! st)
  (let* ((q (unbox (vector-ref st 1)))
         (jobs (append (car q) (reverse (cdr q)))))
    (set-car! q '())
    (set-cdr! q '())
    (vector-set! st 10 0)
    jobs))
;; Slot 20: STOP, which only shutdownNow reaches — the state in which a worker
;; keeps its interrupt rather than having it cleared before a task.
(define (executor-stopping? st) (vector-ref st 20))
;; Slot 21: the tasks the workers have finished, normally or not, which is
;; ThreadPoolExecutor's completedTaskCount. Counted without the queue mutex, by
;; a CAS retried until it lands (vector-cas! may fail spuriously on arm64).
(define (executor-task-done! st)
  (let loop ()
    (let ((n (vector-ref st 21)))
      (unless (vector-cas! st 21 n (fx+ n 1)) (loop)))))
(define (executor-shutdown-now! st)
  ;; STOP, and the queue drained, under ONE hold of the queue mutex, and the
  ;; workers interrupted only after it. A worker takes its next task under that
  ;; same mutex and takes none once it reads STOP (executor-take-job!), which is
  ;; the JVM's getTask; so no queued task can slip past the drain and run. The
  ;; interrupt used to come first: a worker it woke could take the mutex ahead
  ;; of the drain and run what was queued, and shutdownNow handed back nothing
  ;; for tasks that had run anyway (unit row executor-shutdown, seen on CI).
  (let* ((boxes #f)
         (dropped (jolt-with-mutex (vector-ref st 2)
                    (vector-set! st 0 #t)
                    (vector-set! st 20 #t)
                    ;; the delayed tasks come back too, in due order after the
                    ;; immediate ones, and come back AS THEY ARE — the JVM hands the
                    ;; ScheduledFutureTasks over neither cancelled nor done, and each
                    ;; still answers .run.
                    (let ((jobs (append (executor-drain-queue! st) (vector-ref st 13))))
                      (vector-set! st 13 '())
                      (set! boxes (vector-ref st 18))
                      (jolt-cv-wake! (vector-ref st 3))
                      (jolt-cv-wake! (vector-ref st 11))
                      jobs))))
    ;; ...then interrupt what is running: each worker's flag, then a poke at
    ;; whatever interruptible wait it is in (the protocol Thread.interrupt uses).
    ;; A task in Thread/sleep or a deref is thrown out with InterruptedException,
    ;; and one that polls Thread/interrupted sees it; the worker then finds the
    ;; pool stopped and exits.
    (for-each (lambda (b) (set-box! b #t) (jolt-interrupt-wake-waits! b)) boxes)
    (apply jolt-vector dropped)))

;; The wait awaitTermination and close share. DEADLINE #f waits for as long as it
;; takes, which is what close does.
(define (executor-await-termination st deadline)
  (jolt-cv-wait-interruptibly "ExecutorService.awaitTermination"
                              (vector-ref st 2) (vector-ref st 11) deadline
    (lambda (timed-out?)
      (cond ((and (vector-ref st 0) (fx=? 0 (vector-ref st 4))) #t)
            (timed-out? #f)
            (else jolt-cv-again)))))

;; A submit or execute after shutdown is REJECTED, as on the JVM, whose default
;; handler is AbortPolicy. jolt used to accept it and queue it, which is a promise
;; the pool cannot keep: the workers leave as soon as the queue they are draining
;; runs dry, so the task ran only if one happened to still be there, and otherwise
;; sat in the queue for good — with a Future whose .get waits forever and an
;; isTerminated that answers false about a pool with nothing left to run it. A
;; throw at the submit says the same thing the JVM says, at the only point where
;; the caller can still do something about it.
;;
;; Decided under the queue mutex, which is what makes the answer honest either way:
;; shutdown sets the flag and then takes this mutex, so a task that gets in ahead of
;; the flag is enqueued with a worker still live, and a worker drains the queue
;; before it leaves (executor-take-job! reads the depth before it reads the flag).
;; Checked outside, the window between reading the flag and enqueueing is exactly
;; the one where the task can be both accepted and abandoned.
(define (executor-reject-task! st)
  (jolt-throw (jolt-host-throwable
               "java.util.concurrent.RejectedExecutionException"
               "task rejected from executor: it is shut down")))
(define (executor-enqueue! self job)
  (let* ((st (jhost-state self))
         (action (jolt-with-mutex (vector-ref st 2)
                   (cond
                     ((vector-ref st 0) 'reject)
                     (else
                      (let ((q (unbox (vector-ref st 1))))
                        (set-cdr! q (cons job (cdr q))))
                      (vector-set! st 10 (fx+ (vector-ref st 10) 1))
                      ;; One task, one taker: signal a single parked worker, and
                      ;; none at all when none is parked — a worker that is running
                      ;; or waiting on this mutex re-reads the queue before it
                      ;; parks, under this same hold, so there is no wake for it to
                      ;; miss.
                      (when (fx>? (vector-ref st 9) 0)
                        (jolt-cv-signal-one! (vector-ref st 3)))
                      ;; Grow if this task has no worker to take it — one parked in
                      ;; idle-wait, or one already forked and on its way to the queue.
                      ;;
                      ;; STARTING (st 12) belongs in that count for the reason the herd
                      ;; did (see the two conditions, above): a worker slow to ARRIVE
                      ;; also looks like a worker that is not coming. Counting only the
                      ;; parked ones, a fork that has not landed yet is invisible, so on
                      ;; a contended machine every submit inside that window forked
                      ;; another worker — and each one lengthened the window by
                      ;; contending for this mutex, which forked more still. A
                      ;; SEQUENTIAL submit/get loop, concurrency one throughout, took
                      ;; ~20 threads that way (one per task, all the work on the first
                      ;; of them) where it needs one; jolt-jml has the measurements.
                      ;; A starting worker is a taker, so counting it costs no
                      ;; legitimate growth: a burst that really does need n workers
                      ;; keeps depth ahead of idle+starting and still grows to n.
                      ;;
                      ;; This is Go's rule. Its scheduler counts the threads that are
                      ;; on their way to look for work (sched.nmspinning) and wakep()
                      ;; declines to start another while one exists — added there for
                      ;; the same symptom, "lots of unnecessary thread wake ups". The
                      ;; JVM does NOT do this: a SynchronousQueue offer succeeds only
                      ;; against a worker already parked in take, so a cached pool
                      ;; there counts parked workers exactly as this rule used to, and
                      ;; ThreadPoolExecutor's own javadoc grants that direct handoffs
                      ;; "admit the possibility of unbounded thread growth". So this
                      ;; is jolt being tighter than the JVM, not catching up to it.
                      ;;
                      ;; Go carries one more rule that this does not need: the last
                      ;; spinner to find work must wake a replacement. Go compares
                      ;; against "is there ANY spinner", because a spinner scans every
                      ;; queue and can absorb arbitrary work, so the invariant needs
                      ;; re-establishing by hand. The comparison here is against the
                      ;; COUNT of takers and each queued task needs its own, so it
                      ;; maintains itself: idle+starting >= depth already means one
                      ;; taker per queued task, and a worker cannot park while work is
                      ;; queued (executor-take-job! re-reads the depth under this same
                      ;; mutex before it waits).
                      (if (and (fx>? (vector-ref st 10)
                                     (fx+ (vector-ref st 9) (vector-ref st 12)))
                               (executor-claim-worker! st))
                          'spawn
                          #f))))))
    ;; Both of these happen with the mutex RELEASED: a fork takes ~80us and the
    ;; queue cannot be shut for that long, and a throw has no business unwinding
    ;; through a lock region it does not need to hold.
    (case action
      ((spawn) (executor-spawn-worker! st))
      ((reject) (executor-reject-task! st))
      (else (void)))))
(let ((single (lambda args (make-executor* "executor-service" 1 1 #f #f #f (thread-factory-in args))))
      (fixed  (lambda (n . args)
                (let ((n (max 1 (jnum->exact n))))
                  (make-executor* "executor-service" n n #f #f #f (thread-factory-in args)))))
      ;; the two scheduled factories: the same fixed pools under the tag that
      ;; answers schedule (jolt-hgjo). newSingleThreadScheduledExecutor's class
      ;; is a private delegating wrapper on the JVM, which jolt does not model —
      ;; it reports the pool it wraps, as newSingleThreadExecutor already does.
      (scheduled-single (lambda args (make-scheduled-executor 1 #f (thread-factory-in args))))
      (scheduled-fixed  (lambda (n . args)
                          (make-scheduled-executor (max 1 (jnum->exact n)) #f (thread-factory-in args))))
      ;; cached / virtual-thread-per-task: the two factories that are UNBOUNDED on
      ;; the JVM. A cached pool is (0, Integer.MAX_VALUE, 60s, SynchronousQueue)
      ;; there, and a virtual thread per task is a thread per task with no pool at
      ;; all; both answer a burst of n concurrent tasks with n carriers. These were
      ;; one fixed 32-worker pool, which meant task 33 of a burst did not start
      ;; until an earlier one finished — invisible while tasks are short, and a
      ;; deadlock when they are not: 32 tasks that block waiting for the 33rd (a
      ;; fan-out whose children hand results back through a channel, say) never let
      ;; it run. Growing on demand takes that ceiling away.
      ;;
      ;; A grown worker is REUSED for a later task and retired after 60s idle, so a
      ;; steady stream of short tasks costs a thread or two rather than one per
      ;; task, and a burst that has passed does not leave its threads behind. That
      ;; is exactly a cached pool; for the virtual-thread executor it is a
      ;; substitution — a pooled thread for a fresh virtual one, which nothing here
      ;; can tell apart, since jolt has no thread-locals and a task's identity is
      ;; its own.
      (cached (lambda args (make-cached-executor #f (thread-factory-in args))))
      ;; ...whose workers are daemons, as every virtual thread is: a script that
      ;; never closes the executor exits when it returns, not 60s after its last
      ;; task.
      (vcached (lambda _ (make-cached-executor #t)))
      ;; newWorkStealingPool is NOT one of those and stays a fixed pool: a
      ;; ForkJoinPool is sized at availableProcessors, because work stealing is for
      ;; CPU-bound tasks that would only contend if there were more of them than
      ;; cores. It does grow past that, but only to REPLACE a worker the JVM can
      ;; see is blocked in a join, which is a thing jolt cannot see; a flat 32 sits
      ;; between the two bounds and errs toward not stranding a blocking task.
      ;; ForkJoinPool's workers are daemons.
      (stealing (lambda _ (make-executor* "executor-service" 32 32 #f #f #t))))
  (for-each (lambda (nm) (register-class-statics! nm
              (list (cons "newSingleThreadExecutor" single)
                    (cons "newSingleThreadScheduledExecutor" scheduled-single)
                    (cons "newFixedThreadPool" fixed) (cons "newScheduledThreadPool" scheduled-fixed)
                    (cons "newVirtualThreadPerTaskExecutor" vcached)
                    (cons "newCachedThreadPool" cached) (cons "newWorkStealingPool" stealing))))
            '("Executors" "java.util.concurrent.Executors")))
;; submit, as a named procedure: invokeAll and invokeAny below are defined in
;; terms of it, exactly as the JVM's AbstractExecutorService defines them.
;; A task runs with NO thread bindings, as on the JVM: ExecutorService conveys
;; nothing (future, send, pmap and core.async do, each through
;; binding-conveyor-fn, and each of those shims restores a snapshot). The empty
;; stack is installed per task rather than left to the worker's own, which Chez
;; seeded from whoever forked the worker.
(define (executor-submit self thunk*)
  (let ((fut (make-j-future)) (thunk (runnable->thunk thunk*)))
    (executor-enqueue! self (lambda () (dyn-binding-stack '()) (j-future-complete! fut thunk)))
    fut))
(define (executor-task-list tasks)
  (let ((s (jolt-seq tasks))) (if (jolt-nil? s) '() (seq->list s))))
;; Submit every task, IN LIST ORDER, and answer the futures in that order. Not
;; `map`: its element order is unspecified, and the compiled runtime evaluated it
;; right-to-left, so a three-task invokeAll reached the queue as (c a b) and a
;; single-worker pool ran it that way — while .submit and .execute, which enqueue
;; one at a time, were FIFO. Submission order is observable (it is the order a
;; serial executor runs them in), so it has to be spelled out.
(define (executor-submit-all self ts)
  (let loop ((ts ts) (acc '()))
    (if (null? ts)
        (reverse acc)
        (let ((f (executor-submit self (car ts))))     ; forced: bound before the recursion
          (loop (cdr ts) (cons f acc))))))

(register-host-methods! "executor-service"
  (list (cons "submit" executor-submit)
        (cons "execute" (lambda (self thunk*)
          (let ((thunk (runnable->thunk thunk*)))
            ;; A task's throw is NOT caught here: on the JVM it ends the worker
            ;; running it, goes to that thread's uncaught-exception handler, and
            ;; the pool starts a replacement (executor-worker-loop*).
            (executor-enqueue! self (lambda () (dyn-binding-stack '())   ; no conveyance, as submit
              (jolt-invoke thunk)))
          jolt-nil)))
        ;; Shutdown wakes BOTH conditions, and every waiter on each: task-cond so
        ;; that all the idle workers see the flag and leave (the one place a
        ;; broadcast there is the point — the news is for all of them, not for
        ;; whichever one a signal would pick), term-cond because a pool with no
        ;; live worker is already terminated and awaitTermination has to re-read
        ;; the flag to find out.
        ;; invokeAll submits every task and BLOCKS until each has finished, then
        ;; answers their futures — so a caller that maps .get over the result is
        ;; waiting on work that has already run, and one that does not still gets
        ;; the JVM's guarantee that the tasks are done when it returns. A task
        ;; that threw is DONE with its failure held in its own future (.get raises
        ;; ExecutionException); invokeAll itself does not raise.
        (cons "invokeAll" (lambda (self tasks . timeout)
          (let ((futs (executor-submit-all self (executor-task-list tasks)))
                (deadline (let ((ms (tu-args->ms timeout))) (and ms (ms->deadline-millis ms)))))
            ;; ONE deadline for the whole call, not one per task, and a miss
            ;; cancels the rest rather than waiting them out — a caller that gave
            ;; a bound gets it. The deadline used to be discarded entirely.
            ;; A task that THREW is finished, so its ExecutionException is
            ;; swallowed here (it stays in its own future, where .get raises it);
            ;; only a missed deadline stops the wait.
            (let loop ((fs futs))
              (unless (null? fs)
                (if (eq? j-future-timed-out
                         (guard (e (#t #f))
                           (j-future-get-until (car fs) deadline (lambda () j-future-timed-out))))
                    (for-each (lambda (f) (j-future-cancel! f #t)) fs)
                    (loop (cdr fs)))))
            (apply jolt-vector futs))))
        ;; invokeAny answers the first task that succeeded, skipping the ones that
        ;; threw, and raises only when every one of them failed (the JVM re-raises
        ;; the last failure as an ExecutionException, which is what j-future-get
        ;; already builds). An empty task list is IllegalArgumentException there.
        ;; Every OTHER task is CANCELLED once one has succeeded, as the JVM does:
        ;; a caller relies on that to stop work whose answer it no longer needs,
        ;; and jolt used to leave them all running. The deadline (when given) is
        ;; the whole call's — nothing having succeeded by then is TimeoutException,
        ;; not a late answer.
        (cons "invokeAny" (lambda (self tasks . timeout)
          (let ((ts (executor-task-list tasks))
                (deadline (let ((ms (tu-args->ms timeout))) (and ms (ms->deadline-millis ms)))))
            (when (null? ts)
              (throw-jvm (quote IllegalArgumentException) "tasks is empty"))
            (let* ((all (executor-submit-all self ts))
                   (cancel-rest! (lambda (fs) (for-each (lambda (f) (j-future-cancel! f #t)) fs))))
              (let loop ((futs all) (err #f))
                (if (null? futs)
                    (raise err)                       ; every task failed
                    (let ((r (guard (e (#t (cons 'err e)))
                               (cons 'ok (j-future-get-until (car futs) deadline
                                                             (lambda () j-future-timed-out))))))
                      (cond ((and (eq? (car r) 'ok) (eq? (cdr r) j-future-timed-out))
                             (cancel-rest! all)
                             (jolt-throw (jolt-host-throwable
                                          "java.util.concurrent.TimeoutException"
                                          "timed out waiting for a task")))
                            ((eq? (car r) 'ok) (cancel-rest! (cdr futs)) (cdr r))
                            (else (loop (cdr futs) (cdr r)))))))))))
        (cons "shutdown" (lambda (self) (executor-shutdown! (jhost-state self)) jolt-nil))
        (cons "shutdownNow" (lambda (self) (executor-shutdown-now! (jhost-state self))))
        ;; ThreadPoolExecutor.allowCoreThreadTimeOut: core workers retire after the
        ;; keep-alive too, so an idle pool stops holding the process up. The JVM
        ;; refuses it for a pool whose keep-alive is not positive.
        (cons "allowCoreThreadTimeOut" (lambda (self on)
          (let ((st (jhost-state self)) (on (jolt-truthy? on)))
            (when (and on (not (and (vector-ref st 8) (> (vector-ref st 8) 0))))
              (throw-jvm 'IllegalArgumentException "Core threads must have nonzero keep alive times"))
            (jolt-with-mutex (vector-ref st 2)
              (vector-set! st 19 on)
              (jolt-cv-wake! (vector-ref st 3))))   ; idle core workers: start timing
          jolt-nil))
        (cons "allowsCoreThreadTimeOut" (lambda (self) (and (vector-ref (jhost-state self) 19) #t)))
        ;; close is shutdown plus an unbounded awaitTermination, and it BLOCKS —
        ;; "blocks until all tasks have completed execution", as the JVM has it
        ;; since 19. It used to return the moment the flag was set, so the one
        ;; spelling of shutdown that promises the work is finished when it returns
        ;; was the one that did not wait: (with-open [ex …] …) left its tasks
        ;; running behind it and the body's cleanup ran against a live pool.
        (cons "close" (lambda (self)
          (let ((st (jhost-state self)))
            (executor-shutdown! st)
            (executor-await-termination st #f))
          jolt-nil))
        (cons "isShutdown" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "isTerminated" (lambda (self) (let ((st (jhost-state self)))
          (and (vector-ref st 0) (fx=? 0 (vector-ref st 10)) (null? (vector-ref st 13))
               (fx=? 0 (vector-ref st 4))))))
        ;; (timeout, unit) on the JVM. The unit used to be dropped and the amount read
        ;; as milliseconds outright, so (.awaitTermination ex 5 TimeUnit/SECONDS)
        ;; waited five MILLISECONDS and reported the pool still running.
        ;; A WAIT AND NOT A POLL (executor-await-termination above). This used to
        ;; check under the mutex and sleep outside it in 10-100ms steps, because
        ;; sleeping while HOLDING the mutex starves the worker exit it waits for. A
        ;; condition wait releases the mutex atomically with blocking, so it needs
        ;; neither the poll nor the care: the workers already broadcast term-cond as
        ;; each one exits, and shutdown broadcasts it too. It also stops a fiber
        ;; calling this from sleeping its carrier in 100ms chunks. term-cond and not
        ;; task-cond: a wait that can belong to a FIBER must not sit on the
        ;; condition the enqueue path signals one thread on.
        (cons "awaitTermination" (lambda (self ms . rest)
          (executor-await-termination
            (jhost-state self)
            (ms->timeout (tu->ms ms (if (null? rest) #f (car rest)))))))))

;; --- ArrayBlockingQueue / FutureTask / ThreadPoolExecutor --------------------
;; The construction Grain's SQLite write coordinator does directly:
;; (ThreadPoolExecutor. 1 1 keepAlive unit (ArrayBlockingQueue. n)) plus
;; FutureTask. ArrayBlockingQueue is a real bounded blocking queue (fiber-aware
;; through jolt-cv-wait); FutureTask a run-once future; the ThreadPoolExecutor
;; ctor builds on make-executor* above, sized by maximumPoolSize, its queue
;; argument contributing capacity to .getQueue's view. Tasks flow through the
;; executor's own unbounded queue — the JVM REJECTS a submit when the bounded
;; queue is full, jolt queues it; that is the deliberate divergence here.
;;
;; abq state: #(capacity mutex cond queue-pair count); queue-pair is (out . in)
;; like the executor's. One condition serves takers and putters (broadcast). The
;; jhost TAG is what names the class — "abq" or "lbq" (see LinkedBlockingQueue
;; below) — so everything from here down is written against the tag it is given.
(define (make-abq* tag cap)
  ;; Both classes reject a capacity below 1 at CONSTRUCTION on the JVM, with a
  ;; bare IllegalArgumentException (no message — the JDK's ctor throws the
  ;; no-arg one). jolt used to build the queue: its put then blocked forever and
  ;; its offer always answered false, so a bad argument surfaced as a hang with
  ;; nothing pointing at the ctor that caused it (jolt-eyi).
  (when (< cap 1) (throw-jvm (quote IllegalArgumentException) jolt-nil))
  (make-jhost tag (vector cap (make-mutex) (jolt-make-cv) (cons '() '()) 0)))
(define (make-abq cap) (make-abq* "abq" cap))
(define (abq? x) (and (jhost? x) (member (jhost-tag x) '("abq" "lbq")) #t))
;; the mutators run under the mutex (jolt-cv-wait's decide, or jolt-with-mutex)
(define (abq-enq! st v)
  (let ((q (vector-ref st 3)))
    (set-cdr! q (cons v (cdr q)))
    (vector-set! st 4 (fx+ (vector-ref st 4) 1))
    (jolt-cv-wake! (vector-ref st 2))))
(define (abq-norm! st)
  (let ((q (vector-ref st 3)))
    (when (and (null? (car q)) (pair? (cdr q)))
      (set-car! q (reverse (cdr q)))
      (set-cdr! q '()))))
;; the elements head-first, copied out under the mutex
(define (abq-snapshot self)
  (let ((st (jhost-state self)))
    (jolt-with-mutex (vector-ref st 1)
      (abq-norm! st)
      (let ((q (vector-ref st 3))) (append (car q) (reverse (cdr q)))))))
(define (abq-deq! st)
  (abq-norm! st)
  (let* ((q (vector-ref st 3)) (out (car q)) (v (car out)))
    (set-car! q (cdr out))
    (vector-set! st 4 (fx- (vector-ref st 4) 1))
    (jolt-cv-wake! (vector-ref st 2))
    v))
;; blocking take/put; timeout is the (n unit) argument list, or #f to wait
;; forever. take answers (v . #t) so a #f ELEMENT stays distinct from a timeout's
;; #f.
;;
;; Each first tries the op outright, and only builds the wait (deadline, ibox,
;; handler, interrupt registration) when that fails: a timed poll on a queue with
;; data in it was paying all of that to run decide once, 1.5-5x an untimed poll
;; (#1303). The JDK makes the same split — lockInterruptibly, then the count
;; check, then the wait. Its lockInterruptibly throws on a set flag even when an
;; element is ready, so the fast path stands aside for a set flag and the wait,
;; which checks the flag first, throws and clears it.
(define (abq-take-now st)
  (and (not (unbox (current-interrupt-box)))
       (jolt-with-mutex (vector-ref st 1)
         (and (fx>? (vector-ref st 4) 0) (cons (abq-deq! st) #t)))))
(define (abq-put-now st v)
  (and (not (unbox (current-interrupt-box)))
       (jolt-with-mutex (vector-ref st 1)
         (and (fx<? (vector-ref st 4) (vector-ref st 0)) (begin (abq-enq! st v) #t)))))
(define (abq-take-wait self timeout)
  (let ((st (jhost-state self)))
    (or (abq-take-now st)
        (jolt-cv-wait-interruptibly "BlockingQueue.take"
                                    (vector-ref st 1) (vector-ref st 2)
                                    (and timeout (ms->timeout (tu-args->ms timeout)))
          (lambda (timed-out?)
            (cond ((fx>? (vector-ref st 4) 0) (cons (abq-deq! st) #t))
                  (timed-out? #f)
                  (else jolt-cv-again)))))))
(define (abq-put-wait self v timeout)
  (let ((st (jhost-state self)))
    (or (abq-put-now st v)
        (jolt-cv-wait-interruptibly "BlockingQueue.put"
                                    (vector-ref st 1) (vector-ref st 2)
                                    (and timeout (ms->timeout (tu-args->ms timeout)))
          (lambda (timed-out?)
            (cond ((fx<? (vector-ref st 4) (vector-ref st 0)) (abq-enq! st v) #t)
                  (timed-out? #f)
                  (else jolt-cv-again)))))))
(for-each (lambda (nm) (register-class-ctor! nm
            ;; (cap) / (cap fair) — fairness is accepted and ignored
            (lambda (cap . _) (make-abq (jnum->exact cap)))))
          '("ArrayBlockingQueue" "java.util.concurrent.ArrayBlockingQueue"))
;; LinkedBlockingQueue is the same queue with a different bound: its no-arg ctor
;; is unbounded (Integer.MAX_VALUE capacity, which is literally what the JDK
;; stores and what .remainingCapacity answers), (int) is the bounded form, and
;; (Collection) fills an unbounded one. It gets its OWN tag rather than a second
;; ctor onto "abq" because jhost-tag->fqn is the single value -> class map:
;; sharing the tag would make (class (LinkedBlockingQueue.)) answer
;; ArrayBlockingQueue. The methods themselves are identical, so the tag aliases
;; abq's table instead of copying it (jolt#951).
(define lbq-unbounded-capacity 2147483647)   ; Integer.MAX_VALUE, as the JDK stores it
(define (lbq-fill! q xs) (for-each (lambda (v) (abq-enq! (jhost-state q) v)) xs) q)
(for-each (lambda (nm) (register-class-ctor! nm
            (lambda args
              (cond ((null? args) (make-abq* "lbq" lbq-unbounded-capacity))
                    ((number? (car args)) (make-abq* "lbq" (jnum->exact (car args))))
                    ;; (LinkedBlockingQueue. coll) — seeded and unbounded
                    (else (lbq-fill! (make-abq* "lbq" lbq-unbounded-capacity)
                                     (let ((s (jolt-seq (car args))))
                                       (if (jolt-nil? s) '() (seq->list s)))))))))
          '("LinkedBlockingQueue" "java.util.concurrent.LinkedBlockingQueue"))
(register-host-methods! "abq"
  (list (cons "offer" (lambda (self v . args)
          (if (null? args)
              (let ((st (jhost-state self)))
                (jolt-with-mutex (vector-ref st 1)
                  (if (fx<? (vector-ref st 4) (vector-ref st 0))
                      (begin (abq-enq! st v) #t)
                      #f)))
              (abq-put-wait self v args))))
        (cons "put" (lambda (self v) (abq-put-wait self v #f) jolt-nil))
        (cons "take" (lambda (self) (car (abq-take-wait self #f))))
        (cons "poll" (lambda (self . args)
          (if (null? args)
              (let ((st (jhost-state self)))
                (jolt-with-mutex (vector-ref st 1)
                  (if (fx>? (vector-ref st 4) 0) (abq-deq! st) jolt-nil)))
              (let ((r (abq-take-wait self args)))
                (if r (car r) jolt-nil)))))
        (cons "peek" (lambda (self)
          (let ((st (jhost-state self)))
            (jolt-with-mutex (vector-ref st 1)
              (if (fx>? (vector-ref st 4) 0)
                  (begin (abq-norm! st) (car (car (vector-ref st 3))))
                  jolt-nil)))))
        (cons "size" (lambda (self) (vector-ref (jhost-state self) 4)))
        (cons "remainingCapacity" (lambda (self)
          (let ((st (jhost-state self))) (fx- (vector-ref st 0) (vector-ref st 4)))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (vector-ref (jhost-state self) 4))))
        (cons "clear" (lambda (self)
          (let ((st (jhost-state self)))
            (jolt-with-mutex (vector-ref st 1)
              (let ((q (vector-ref st 3)))
                (set-car! q '()) (set-cdr! q '())
                (vector-set! st 4 0)
                (jolt-cv-wake! (vector-ref st 2)))))
          jolt-nil))
        ;; Collection's half: add is offer-or-throw (Queue.add on a full bounded
        ;; queue is IllegalStateException "Queue full"), and the read-only walks
        ;; take a SNAPSHOT under the mutex — the JVM's iterators are weakly
        ;; consistent too, so a walk never sees a half-moved element.
        (cons "add" (lambda (self v)
          (let ((st (jhost-state self)))
            (jolt-with-mutex (vector-ref st 1)
              (if (fx<? (vector-ref st 4) (vector-ref st 0))
                  (begin (abq-enq! st v) #t)
                  (throw-jvm (quote IllegalStateException) "Queue full"))))))
        (cons "contains" (lambda (self v)
          (and (memp (lambda (a) (jolt=2 a v)) (abq-snapshot self)) #t)))
        (cons "iterator" (lambda (self) (make-jiterator (list->cseq (abq-snapshot self)))))
        (cons "toArray" (lambda (self . _) (na-to-array (list->cseq (abq-snapshot self)))))
        ;; the tag, not a literal: "lbq" shares this table and must not print as
        ;; an ArrayBlockingQueue.
        ;; AbstractCollection.toString: the elements, head first
        (cons "toString" (lambda (self) (host-elems-tostring self (abq-snapshot self))))))
(alias-host-methods! "lbq" "abq")
;; a blocking queue is a java.util Collection: str, seq and the image see its
;; elements (jutil-colls.ss)
(register-jutil-coll! "abq" 'coll abq-snapshot)
(register-jutil-coll! "lbq" 'coll abq-snapshot)
;; (seq q), (vec q) over a queue: the same snapshot, head first; (count q) is
;; the size, without one.
(register-seq-arm! abq? (lambda (q) (list->cseq (abq-snapshot q))))
(register-count-arm! abq? (lambda (q) (vector-ref (jhost-state q) 4)))

;; FutureTask — a run-once task with a blocking get. State:
;; #(status override-flag override value error mutex cond thunk); status is one
;; of new/running/done/cancelled. cancel wins until the task settles, and
;; cancel(true) interrupts the thread running it, as on the JVM.
(define (make-future-task thunk override-flag override)
  (make-jhost "future-task"
              ;; a reified Callable or Runnable is called through its method, as
              ;; submit calls one (it was invoked as a fn and failed the task)
              (vector 'new override-flag override jolt-nil #f (make-mutex) (jolt-make-cv)
                      (runnable->thunk thunk) thunk)))
(define (future-task? x) (and (jhost? x) (string=? (jhost-tag x) "future-task")))
(define (future-task-run! self)
  (let ((st (jhost-state self)))
    (when (jolt-with-mutex (vector-ref st 5)
            (and (eq? (vector-ref st 0) 'new)
                 (begin (vector-set! st 0 'running) (task-runner-set! self) #t)))
      (let ((r (guard (e (#t (vector-set! st 4 e) jolt-nil)) (jolt-invoke (vector-ref st 7)))))
        (jolt-with-mutex (vector-ref st 5)
          (task-runner-clear! self)
          ;; a cancel that landed while it ran keeps it cancelled, as the JVM's
          ;; does (this used to overwrite it with the result)
          (unless (eq? (vector-ref st 0) 'cancelled)
            (vector-set! st 3 (if (vector-ref st 1) (vector-ref st 2) r))
            (vector-set! st 0 'done))
          (jolt-cv-wake! (vector-ref st 6)))))
    jolt-nil))
(for-each (lambda (nm) (register-class-ctor! nm
            (lambda (thunk . rest)
              (make-future-task thunk (pair? rest) (if (pair? rest) (car rest) jolt-nil)))))
          '("FutureTask" "java.util.concurrent.FutureTask"))
(define (future-task-get* self ms on-timeout)
  (let* ((st (jhost-state self))
         (r (jolt-cv-wait-interruptibly "FutureTask.get"
                          (vector-ref st 5) (vector-ref st 6)
                          (and ms (ms->timeout ms))
              (lambda (timed-out?)
                (cond ((memq (vector-ref st 0) '(done cancelled)) (vector-ref st 0))
                      (timed-out? #f)
                      (else jolt-cv-again))))))
    (cond ((not r) (on-timeout))
          ((eq? r 'cancelled)
           (jolt-throw (jolt-host-throwable "java.util.concurrent.CancellationException"
                                            "task was cancelled")))
          ((vector-ref st 4) (jolt-throw (task-execution-exception (vector-ref st 4))))
          (else (vector-ref st 3)))))
(define (future-task-get self . args)
  (future-task-get* self (tu-args->ms args) future-timeout-throw))

;; The get* of a Future-shaped shim value, or #f. Both spellings of the deref
;; arity go through it: ms #f waits forever (Future.get()), an ms with a timeout
;; thunk is the bounded overload.
(define (future-shim-get* x)
  (let ((tag (jhost-tag x)))
    (cond ((string=? tag "future-task") future-task-get*)
          ((string=? tag "j-future") j-future-get*)
          ((string=? tag "scheduled-future") j-future-get*)   ; the same five slots (below)
          ((string=? tag "completable-future") cf-get*)      ; CompletableFuture (below)
          (else #f))))
(register-host-methods! "future-task"
  (list (cons "run" (lambda (self) (future-task-run! self)))
        (cons "get" future-task-get)
        (cons "isDone" (lambda (self) (and (memq (vector-ref (jhost-state self) 0) '(done cancelled)) #t)))
        (cons "isCancelled" (lambda (self) (eq? (vector-ref (jhost-state self) 0) 'cancelled)))
        ;; cancel wins while the task has not settled, running or not, and
        ;; cancel(true) interrupts the thread running it (j-future-cancel!)
        (cons "cancel" (lambda (self may)
          (let* ((st (jhost-state self))
                 (r (jolt-with-mutex (vector-ref st 5)
                      (if (memq (vector-ref st 0) '(new running))
                          (let ((b (and (jolt-truthy? may) (task-runner self))))
                            (vector-set! st 0 'cancelled)
                            (when b (set-box! b #t))
                            (jolt-cv-wake! (vector-ref st 6))
                            (or b #t))
                          #f))))
            (when (box? r) (task-interrupt-runner! r))
            (and r #t))))
        ;; FutureTask.toString: Object's, then the state
        (cons "toString" (lambda (self)
          (let ((st (jhost-state self)))
            (string-append (host-object-tostring self) "["
                           (case (vector-ref st 0)
                             ((done) (if (vector-ref st 4)
                                         (string-append "Completed exceptionally: "
                                                        (jolt-str-render-one (vector-ref st 4)))
                                         "Completed normally"))
                             ((cancelled) "Cancelled")
                             (else (string-append "Not completed, task = "
                                                  (jolt-str-render-one (vector-ref st 8)))))
                           "]"))))))
;; submit/execute above route a FutureTask through its own run (a Runnable on
;; the JVM), and a reify or deftype implementing Callable or Runnable through its
;; call or run — invoking one directly was "cannot be cast to clojure.lang.IFn",
;; so (.submit pool (reify Callable (call [_] …))) failed its future. Anything
;; else is an invokable thunk.
(define (runnable->thunk x)
  (cond ((future-task? x) (lambda () (future-task-run! x) jolt-nil))
        ((procedure? x) x)
        ((iface-method x "call" 1) (lambda () (record-method-dispatch x "call" jolt-nil)))
        ((iface-method x "run" 1) (lambda () (record-method-dispatch x "run" jolt-nil) jolt-nil))
        (else x)))

;; ThreadPoolExecutor ctor: (core max keepAlive unit [queue] [factory]
;; [handler]) — core, max and keepAlive all reach the pool now that the pool can
;; grow and retire; a factory decides each worker's daemon flag and name
;; (executor-spawn-worker!), and a handler is accepted and ignored. The workers up
;; to core are eager, the rest appear as tasks arrive with nobody idle, and an
;; above-core worker retires after keepAlive.
;;
;; WHEN a thread past core appears is the one place this still diverges, and it
;; diverges in the direction of the caller's stated maximum: the JVM grows only
;; once the work queue is FULL, so a TPE with an unbounded queue never passes
;; core at all, and jolt has one unbounded queue for every pool. Growing on the
;; handoff test instead means max means what it says. The pool is at least 1 for
;; the same reason as newFixedThreadPool: a pool with no worker runs nothing.
(for-each (lambda (nm) (register-class-ctor! nm
            (lambda (core-n max-n . rest)
              (let* ((q (and (>= (length rest) 3) (abq? (list-ref rest 2)) (list-ref rest 2)))
                     (max-n (max 1 (jnum->exact max-n)))
                     (core-n (min max-n (max 0 (jnum->exact core-n))))
                     ;; #f only when the ctor was called without them at all; a
                     ;; keepAlive of 0 is the JVM's "retire the moment you are
                     ;; idle" and stays 0.
                     (keep (and (>= (length rest) 2) (tu->ms (car rest) (cadr rest)))))
                (make-executor* "executor-service" core-n max-n keep
                                (and q (vector-ref (jhost-state q) 0))
                                #f (thread-factory-in (if (>= (length rest) 3) (cddr rest) '())))))))
          '("ThreadPoolExecutor" "java.util.concurrent.ThreadPoolExecutor"))
;; .getQueue answers a live VIEW of the executor's internal queue — size reads
;; the real depth, remainingCapacity subtracts it from the advisory capacity
;; (Integer/MAX_VALUE without one). Honest for monitoring; not the caller's
;; ArrayBlockingQueue instance, which the executor does not use.
(define (executor-queue-depth ex)
  (let ((st (jhost-state ex)))
    (fx+ (vector-ref st 10) (length (vector-ref st 13)))))   ; the delayed tasks are queued too
(register-host-methods! "executor-queue-view"
  (list (cons "size" (lambda (self) (executor-queue-depth (vector-ref (jhost-state self) 0))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (executor-queue-depth (vector-ref (jhost-state self) 0)))))
        (cons "remainingCapacity" (lambda (self)
          (let* ((ex (vector-ref (jhost-state self) 0))
                 (cap (vector-ref (jhost-state ex) 5)))
            (if cap (max 0 (- cap (executor-queue-depth ex))) 2147483647))))
        ;; the work queue's tasks, head first, as the queue's own toString lists them
        (cons "toString" (lambda (self)
                           (let* ((st (jhost-state (vector-ref (jhost-state self) 0)))
                                  (q (jolt-with-mutex (vector-ref st 2)
                                       (let ((p (unbox (vector-ref st 1))))
                                         (append (car p) (reverse (cdr p)))))))
                             (host-elems-tostring self q))))))
(register-host-methods! "executor-service"
  (list (cons "getQueue" (lambda (self) (make-jhost "executor-queue-view" (vector self))))
        ;; ThreadPoolExecutor's size accessors, which are how a caller (and the
        ;; test suite) can see the pool grow and retire at all. getActiveCount is
        ;; documented as approximate on the JVM and is approximate here for the
        ;; same reason: it is a live count read without stopping the pool.
        ;; getMaximumPoolSize of a cached pool answers Integer/MAX_VALUE, which is
        ;; the number the JVM's own newCachedThreadPool passes.
        (cons "getPoolSize" (lambda (self) (vector-ref (jhost-state self) 4)))
        ;; neither an idle worker nor one still starting (st 12, not yet at the
        ;; queue) is running a task
        (cons "getActiveCount" (lambda (self)
          (let ((st (jhost-state self)))
            (max 0 (fx- (vector-ref st 4) (fx+ (vector-ref st 9) (vector-ref st 12)))))))
        (cons "getCorePoolSize" (lambda (self) (vector-ref (jhost-state self) 6)))
        (cons "getMaximumPoolSize" (lambda (self) (vector-ref (jhost-state self) 7)))
        (cons "getCompletedTaskCount" (lambda (self) (vector-ref (jhost-state self) 21)))
        (cons "toString" (lambda (self) (executor-to-string self)))))
;; ThreadPoolExecutor.toString: Object's, then the run state and the four counts
(define (executor-to-string self)
  (let ((st (jhost-state self)))
    (string-append (host-object-tostring self)
                   "[" (cond ((not (vector-ref st 0)) "Running")
                             ((jolt-truthy? (record-method-dispatch self "isTerminated" jolt-nil)) "Terminated")
                             (else "Shutting down"))
                   ", pool size = " (number->string (vector-ref st 4))
                   ", active threads = " (number->string (record-method-dispatch self "getActiveCount" jolt-nil))
                   ", queued tasks = " (number->string (executor-queue-depth self))
                   ", completed tasks = " (number->string (vector-ref st 21)) "]")))

;; --- ScheduledExecutorService ------------------------------------------------
;; schedule, scheduleAtFixedRate and scheduleWithFixedDelay on the scheduled pools
;; (jolt-hgjo). Executors/newSingleThreadScheduledExecutor and newScheduledThreadPool
;; answered plain pools, so (.schedule ex f 1 TimeUnit/MILLISECONDS) was "No
;; matching method schedule found taking 3 args".
;;
;; THE POOL'S OWN WORKERS RUN THE DELAYED TASKS, as on the JVM, where a scheduled
;; pool's work queue IS the delay queue (ScheduledThreadPoolExecutor.DelayedWorkQueue)
;; and the workers block on its head. Here the delayed tasks are a second list on
;; the executor state (st 13), in due order, that executor-take-job! reads after
;; the immediate queue and before it parks: a due head is the worker's next job, a
;; head not yet due is what one idle worker's wait is timed against
;; (executor-idle-wait!, which explains the leader). No timer thread, no sleeping
;; thread per task, and the consequences the JVM's design has fall out of it here
;; too — shutdown lets a delayed one-shot run because the worker attending it does
;; not leave until the list is empty, and a pool with every worker busy runs a due
;; task when one frees up, late, rather than growing for it.
;;
;; THE FUTURE is the j-future's five slots — status, value, error, mutex, condition
;; — with five more behind them, so get, deref, cancel, isDone and isCancelled are
;; the j-future's procedures unchanged:
;;   5 due      the mono-millis the task is next due
;;   6 period   0 for a one-shot; >0 for fixed-rate; <0 for fixed-delay — the JVM's
;;              own encoding of the two periodic kinds, in ms
;;   7 thunk
;;   8 seq      the pool's submission number, ordering equal due times FIFO (the
;;              JVM's sequencer does the same)
;;   9 pool     the executor state a periodic run re-queues into
;; A periodic future RUNS AND RESETS: the status goes new -> running -> new around
;; each run and the future never completes normally — .get on it waits until a
;; cancel (CancellationException) or a run that threw (ExecutionException), which
;; also ends the repetition, all as FutureTask.runAndReset has it. cancel therefore
;; wins against a periodic task that is mid-run (its status is new on the JVM
;; while running; here the cancel lands on 'running and the reset honours it),
;; and the run that was in flight finishes and is not re-queued.
;;
;; MILLISECONDS are the pool's clock (mono-millis, the deadline every wait in this
;; file takes), so a period is floored at 1ms after conversion: a 500us period on
;; the JVM runs about two thousand times a second; here it would run without bound.
(define (make-scheduled-future thunk due period seq pool-st)
  (make-jhost "scheduled-future"
              (vector 'new jolt-nil #f (make-mutex) (jolt-make-cv) due period thunk seq pool-st)))
(define (scheduled-future? x) (and (jhost? x) (string=? (jhost-tag x) "scheduled-future")))
(define (sf-due sf) (vector-ref (jhost-state sf) 5))
(define (sf-periodic? sf) (not (eqv? 0 (vector-ref (jhost-state sf) 6))))
(define (sf-settled? sf)
  (let ((st (jhost-state sf)))
    (jolt-with-mutex (vector-ref st 3) (j-future-settled? (vector-ref st 0)))))
;; Queue order: due time, then submission.
(define (sf-before? a b)
  (let ((da (sf-due a)) (db (sf-due b)))
    (or (< da db)
        (and (= da db) (< (vector-ref (jhost-state a) 8) (vector-ref (jhost-state b) 8))))))
;; The delayed list (st 13), every operation with the pool's queue mutex HELD.
(define (executor-delayed-head st)
  (let ((d (vector-ref st 13))) (and (pair? d) (car d))))
(define (executor-delayed-due? st)
  (let ((head (executor-delayed-head st))) (and head (mono-due? (sf-due head)) #t)))
(define (executor-delayed-pop! st)
  (let ((d (vector-ref st 13))) (vector-set! st 13 (cdr d)) (car d)))
;; Insert in due order. A NEW HEAD demotes whoever is timing the old one (their
;; wait is against a later deadline than this task's) and signals one parked worker
;; to claim it; with nobody parked, the next worker through executor-take-job!
;; claims it. The demoted leader wakes on its stale deadline eventually, finds the
;; claim is not its own, and re-polls like anyone else.
(define (executor-delayed-add! st sf)
  (vector-set! st 13
    (let ins ((d (vector-ref st 13)))
      (if (or (null? d) (sf-before? sf (car d))) (cons sf d) (cons (car d) (ins (cdr d))))))
  (when (eq? (car (vector-ref st 13)) sf)
    (vector-set! st 14 #f)
    (when (fx>? (vector-ref st 9) 0) (jolt-cv-signal-one! (vector-ref st 3)))))
;; The next due time of a periodic task after a run: fixed-rate advances from the
;; time it WAS due (a run that overran is followed by the next at once, the JVM's
;; catch-up), fixed-delay from the time the run finished.
(define (sf-advance! sf)
  (let* ((st (jhost-state sf)) (p (vector-ref st 6)))
    (vector-set! st 5 (if (> p 0) (+ (vector-ref st 5) p) (mono-deadline (- p))))))
;; Run SF on the calling thread: claim it (a cancelled one is skipped, as
;; FutureTask.run on a cancelled task does nothing), run, then settle or reset.
;; The worker installs the empty binding stack before calling this; the .run
;; member below runs on the caller's own bindings, as FutureTask.run does.
;; A periodic task re-queues itself UNLESS the pool has shut down meanwhile,
;; in which case it is cancelled instead — what a periodic task that was mid-run
;; when shutdown dropped the rest of them from the queue gets on the JVM too
;; (canRunInCurrentRunState).
(define (scheduled-future-run! sf pool-st)
  (let ((st (jhost-state sf)))
    (when (jolt-with-mutex (vector-ref st 3)
            (and (eq? (vector-ref st 0) 'new)
                 (begin (vector-set! st 0 'running) (task-runner-set! sf) #t)))
      (let* ((r (guard (e (#t (vector-set! st 2 e) #f)) (jolt-invoke (vector-ref st 7))))
             (again? (jolt-with-mutex (vector-ref st 3)
                       (task-runner-clear! sf)
                       (let ((again? (cond ((eq? (vector-ref st 0) 'cancelled) #f)
                                           ((vector-ref st 2) (vector-set! st 0 'done) #f)
                                           ((sf-periodic? sf) (vector-set! st 0 'new) #t)
                                           (else (vector-set! st 1 r) (vector-set! st 0 'done) #f))))
                         (jolt-cv-wake! (vector-ref st 4))
                         again?))))
        (when again?
          (jolt-with-mutex (vector-ref pool-st 2)
            (cond ((vector-ref pool-st 0) (j-future-cancel! sf))
                  ;; queued at most once, at the due time it was queued with: a
                  ;; .run from the caller's thread on a periodic task the pool
                  ;; still holds neither adds a twin nor re-times the entry in
                  ;; place, which would break the list's order
                  ((memq sf (vector-ref pool-st 13)) (void))
                  (else (sf-advance! sf) (executor-delayed-add! pool-st sf)))))))))
;; The three methods' shared entry: reject after shutdown (the JVM's delayedExecute
;; does, with the same AbortPolicy exception submit gets), else build the future
;; under the mutex — its sequence number is the pool's counter — and queue it. A
;; negative delay is now, as on the JVM; a nil task is its NullPointerException.
(define (executor-schedule! self task delay unit period-ms)
  (when (jolt-nil? task) (throw-jvm (quote NullPointerException) jolt-nil))
  (let* ((st (jhost-state self))
         (thunk (runnable->thunk task))
         (due (mono-deadline (max 0 (tu->ms delay unit))))
         (sf (jolt-with-mutex (vector-ref st 2)
               (and (not (vector-ref st 0))
                    (let ((sf (make-scheduled-future thunk due period-ms (vector-ref st 15) st)))
                      (vector-set! st 15 (fx+ (vector-ref st 15) 1))
                      (executor-delayed-add! st sf)
                      sf)))))
    (or sf (executor-reject-task! st))))
;; The period, validated on the amount as GIVEN (the JVM throws a bare
;; IllegalArgumentException for period <= 0) and floored at the clock's 1ms after
;; conversion; SIGN is +1 for fixed-rate and -1 for fixed-delay.
(define (scheduled-period-ms period unit sign)
  (unless (and (number? period) (> period 0))
    (throw-jvm (quote IllegalArgumentException) jolt-nil))
  (* sign (max 1 (tu->ms period unit))))
(derive-host-methods! "scheduled-executor" "executor-service"
  (list (cons "schedule" (lambda (self task delay unit)
          (executor-schedule! self task delay unit 0)))
        (cons "scheduleAtFixedRate" (lambda (self task initial period unit)
          (executor-schedule! self task initial unit (scheduled-period-ms period unit 1))))
        (cons "scheduleWithFixedDelay" (lambda (self task initial delay unit)
          (executor-schedule! self task initial unit (scheduled-period-ms delay unit -1))))))
;; ScheduledThreadPoolExecutor ctor: (corePoolSize [factory] [handler]) — at least
;; one worker, as every pool here (a pool with no worker runs nothing; the JVM's
;; ensurePrestart starts one on demand for core 0).
(for-each (lambda (nm) (register-class-ctor! nm
            (lambda (core-n . rest)
              (make-scheduled-executor (max 1 (jnum->exact core-n)) #f (thread-factory-in rest)))))
          '("ScheduledThreadPoolExecutor" "java.util.concurrent.ScheduledThreadPoolExecutor"))
;; The future's own members over the j-future's: Delayed's getDelay (remaining
;; time in UNIT, negative once overdue, truncated toward zero as TimeUnit.convert
;; truncates), Comparable's compareTo (queue order), isPeriodic, and
;; RunnableScheduledFuture's run — the task on the caller's thread, which is how a
;; task shutdownNow handed back is run, and which re-queues a periodic one only
;; into a pool that is still open.
(derive-host-methods! "scheduled-future" "j-future"
  (list (cons "getDelay" (lambda (self unit)
          (let ((ms (mono-ms-until (sf-due self))))
            (truncate (if (time-unit? unit) (/ (* ms 1000000) (time-unit-scale unit)) ms)))))
        (cons "compareTo" (lambda (self other)
          (unless (scheduled-future? other)
            (throw-jvm (quote ClassCastException)
                       "cannot compare a ScheduledFutureTask with a non-ScheduledFutureTask"))
          (cond ((eq? self other) 0) ((sf-before? self other) -1) ((sf-before? other self) 1) (else 0))))
        (cons "isPeriodic" (lambda (self) (sf-periodic? self)))
        (cons "run" (lambda (self) (scheduled-future-run! self (vector-ref (jhost-state self) 9)) jolt-nil))))

;; --- java.util.concurrent.CompletableFuture ----------------------------------
;; A settable Future whose completion drives a graph of dependent stages. State:
;;   #(result mutex condition dependents)
;; result is cf-pending until the future settles, then the value itself (nil is
;; jolt-nil, so false and nil results need no box) or a cf-alt carrying the
;; throwable — the JVM's AltResult, and for the same reason: a result slot that
;; holds either a value or a failure, written once under the mutex.
;;
;; DEPENDENTS are thunks of one argument, the result, pushed onto a list while
;; the future is pending and run by whichever call settles it, AFTER the mutex is
;; released. A thunk registered once the future has settled runs at once in the
;; registering thread instead. Both decisions are taken under the mutex, so every
;; thunk runs exactly once: either it was on the list when the settle took it, or
;; the registrar saw the result and ran it itself. The list is a stack and runs
;; newest first, which is the order the JVM's Treiber stack of completions runs
;; them in (certified: five thenRuns registered 0..4 run 4 3 2 1 0).
;;
;; THE EXCEPTION WRAPPING is the JVM's, and it is what most code catching around
;; a CompletableFuture depends on, so it is spelled out:
;;   - completeExceptionally / failedFuture / orTimeout store the throwable RAW;
;;   - a dependent stage stores a failure WRAPPED in CompletionException, once
;;     (a failure that already is one is reused, not wrapped again), and so does
;;     supplyAsync's own failure;
;;   - get throws ExecutionException over the underlying cause (unwrapping one
;;     CompletionException), join and getNow throw CompletionException (wrapping
;;     a raw failure), and a CancellationException is rethrown as is by both;
;;   - exceptionally / handle / whenComplete hand their function the stored
;;     throwable: raw on the stage that failed, the CompletionException on a
;;     stage downstream of it.
;;
;; get is interruptible, as every jolt wait a JVM caller can name is (locks.ss,
;; jolt-cv-wait-interruptibly); join is NOT, on the JVM or here: it keeps waiting
;; and leaves the interrupt flag set. Both park a fiber rather than block its
;; carrier, through the same seam.
(define-record-type cf-alt (fields ex) (nongenerative jolt-cf-alt-v1))
(define cf-pending (list 'cf-pending))
(define cf-timed-out (list 'cf-timed-out))
(define (cf-pending? r) (eq? r cf-pending))
(define (make-cf* r) (make-jhost "completable-future" (vector r (make-mutex) (jolt-make-cv) '())))
(define (make-cf) (make-cf* cf-pending))
(define (cf? x) (and (jhost? x) (string=? (jhost-tag x) "completable-future")))
;; The result, read under the mutex: it is published by another thread, and the
;; lock is what orders the read after the write on a weakly-ordered machine.
(define (cf-result c)
  (let ((st (jhost-state c)))
    (jolt-with-mutex (vector-ref st 1) (vector-ref st 0))))

;; Class tests on a stored throwable, by the class graph like a catch clause.
(define (cf-exc-isa? x simple)
  (and (jolt-ex-info-record? x) (exception-isa? (last-dot (ex-info-class x)) simple)))
(define (cf-cancellation? x) (cf-exc-isa? x "CancellationException"))
(define (cf-completion? x) (cf-exc-isa? x "CompletionException"))
(define (cf-cause x) (if (jolt-ex-info-record? x) (jolt-ex-info-record-cause x) jolt-nil))
(define (cf-npe) (throw-jvm 'NullPointerException jolt-nil))

;; new CompletionException(x): the message is x.toString(), as Throwable(cause) makes it.
(define (cf-completion-exception x)
  (jolt-host-throwable "java.util.concurrent.CompletionException" (jolt-str-render-one x) x))
(define (cf-new-cancellation)
  (jolt-host-throwable "java.util.concurrent.CancellationException" jolt-nil))
;; encodeThrowable(x): a failure a dependent stage stores.
(define (cf-encode-throwable x)
  (make-cf-alt (if (cf-completion? x) x (cf-completion-exception x))))
;; encodeRelay(r): a source's result carried into a dependent — a value as is, a
;; raw failure wrapped, an already-wrapped one reused (the same AltResult).
(define (cf-encode-relay r)
  (if (and (cf-alt? r) (not (cf-completion? (cf-alt-ex r))))
      (make-cf-alt (cf-completion-exception (cf-alt-ex r)))
      r))

;; Settle C with R — a value or a cf-alt. #t iff this call did it. FORCE? is
;; obtrude's overwrite of a result already there. Dependents run after the
;; mutex is released, in the settling thread.
(define (cf-settle! c r force?)
  (let* ((st (jhost-state c))
         (deps (jolt-with-mutex (vector-ref st 1)
                 (and (or force? (cf-pending? (vector-ref st 0)))
                      (let ((ds (vector-ref st 3)))
                        (vector-set! st 0 r)
                        (vector-set! st 3 '())
                        (jolt-cv-wake! (vector-ref st 2))
                        ds)))))
    (and deps
         (begin (for-each (lambda (d) (d r)) deps) #t))))

;; Run (thunk result) once C has settled: now, or from whichever call settles it.
(define (cf-when-done! c thunk)
  (let* ((st (jhost-state c))
         (r (jolt-with-mutex (vector-ref st 1)
              (let ((r (vector-ref st 0)))
                (when (cf-pending? r)
                  (vector-set! st 3 (cons thunk (vector-ref st 3))))
                r))))
    (unless (cf-pending? r) (thunk r))))

;; Settle D with what THUNK answers, or with the failure it throws, wrapped.
(define (cf-complete-with! d thunk)
  (cf-settle! d (guard (e (#t (cf-encode-throwable (jolt-unwrap-throw e)))) (thunk)) #f))

;; The functional-interface argument of a stage method — a Clojure fn, or a
;; reified Function/Supplier/Consumer/Runnable/BiFunction/BiConsumer — goes
;; through jolt-fi-call (host-static-classes.ss), named for its one method.
(define cf-fn-call jolt-fi-call)
(define (cf-require! x) (when (jolt-nil? x) (cf-npe)) x)

;; The async pool: the JVM's is ForkJoinPool.commonPool. jolt's is a cached pool
;; (the one newCachedThreadPool builds), made on first use, whose workers do not
;; hold the process open — the commonPool's are daemons too.
(define cf-default-pool #f)
(define cf-default-pool-mu (make-mutex))
(define (cf-default-executor)
  (jolt-with-mutex cf-default-pool-mu
    ;; daemon workers, as the commonPool's are
    (unless cf-default-pool (set! cf-default-pool (make-cached-executor #t)))
    cf-default-pool))
;; Hand THUNK to an Executor: 'default is the async pool, anything else answers
;; execute(Runnable) — a pool shim, a delayedExecutor, a reified Executor. The
;; thunk is a jolt fn, so it is a Runnable to code that calls (.run r).
(define (cf-execute! exec thunk)
  (let ((e (if (eq? exec 'default) (cf-default-executor) exec)))
    (record-method-dispatch e "execute" (jolt-list (lambda () (thunk) jolt-nil)))))
;; Run a dependent's work inline (EXEC #f) or on EXEC. An executor that refuses
;; the task (a shut-down pool) fails the dependent with the rejection, as the
;; JVM's claim() does.
(define (cf-fire! d exec thunk)
  (if exec
      (guard (e (#t (cf-settle! d (cf-encode-throwable (jolt-unwrap-throw e)) #f)))
        (cf-execute! exec thunk))
      (thunk)))
;; The two overloads of an *Async method: without an Executor it is the async
;; pool, with one it is that (a nil one is a NullPointerException). G takes the
;; executor ahead of the function; a two-stage method passes the other stage.
(define (cf-async-1 g)
  (case-lambda ((self f) (g self 'default f))
               ((self f e) (g self (cf-require! e) f))))
(define (cf-async-2 g)
  (case-lambda ((self o f) (g self o 'default f))
               ((self o f e) (g self o (cf-require! e) f))))

;; Another CompletionStage argument, as the CompletableFuture its methods read.
(define (cf-as-cf x)
  (cond ((cf? x) x)
        ((jolt-nil? x) (cf-npe))
        ((iface-method x "toCompletableFuture" 1)
         (cf-as-cf (record-method-dispatch x "toCompletableFuture" jolt-nil)))
        (else (throw-jvm 'ClassCastException
                         (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                                        " cannot be cast to class java.util.concurrent.CompletionStage")))))

;; reportGet / reportJoin: a settled result as get and join answer it.
(define (cf-report-get r)
  (if (cf-alt? r)
      (let ((x (cf-alt-ex r)))
        (if (cf-cancellation? x)
            (jolt-throw x)
            (let ((x (if (and (cf-completion? x) (not (jolt-nil? (cf-cause x)))) (cf-cause x) x)))
              (jolt-throw (jolt-host-throwable "java.util.concurrent.ExecutionException"
                                               (jolt-str-render-one x) x)))))
      r))
(define (cf-report-join r)
  (if (cf-alt? r)
      (let ((x (cf-alt-ex r)))
        (jolt-throw (if (or (cf-cancellation? x) (cf-completion? x)) x (cf-completion-exception x))))
      r))

;; The wait. The result is read FIRST: a future that has settled answers even to
;; a thread whose interrupt flag is set (JVM: get and join both return it and
;; leave the flag alone), where the wait seam would throw before looking.
(define (cf-await c who interruptible? deadline)
  (let ((r (cf-result c)))
    (if (not (cf-pending? r))
        r
        (let* ((st (jhost-state c))
               (decide (lambda (timed-out?)
                         (let ((r (vector-ref st 0)))
                           (cond ((not (cf-pending? r)) r)
                                 (timed-out? cf-timed-out)
                                 (else jolt-cv-again))))))
          (if interruptible?
              (jolt-cv-wait-interruptibly who (vector-ref st 1) (vector-ref st 2) deadline decide)
              (jolt-cv-wait (vector-ref st 1) (vector-ref st 2) deadline decide))))))
(define (cf-timeout-throw)
  (jolt-throw (jolt-host-throwable "java.util.concurrent.TimeoutException" jolt-nil)))
;; get and its timed overload, in the shape future-shim-get* hands deref.
(define (cf-get* c ms on-timeout)
  (let ((r (cf-await c "CompletableFuture.get" #t (and ms (ms->timeout ms)))))
    (if (eq? r cf-timed-out) (on-timeout) (cf-report-get r))))

(define (cf-cancelled? c)
  (let ((r (cf-result c))) (and (cf-alt? r) (cf-cancellation? (cf-alt-ex r)))))

;; --- the stage methods --------------------------------------------------------
;; thenApply / thenAccept / thenRun and their Async forms. A failure is carried
;; to the dependent inline, without the executor — the JVM propagates before it
;; claims — and only the function call goes to the executor.
(define (cf-uni src exec f kind)
  (cf-require! f)
  (let ((d (make-cf)))
    (cf-when-done! src
      (lambda (r)
        (if (cf-alt? r)
            (cf-settle! d (cf-encode-relay r) #f)
            (cf-fire! d exec
              (lambda ()
                (cf-complete-with! d
                  (lambda ()
                    (case kind
                      ((apply) (cf-fn-call f "apply" r))
                      ((accept) (cf-fn-call f "accept" r) jolt-nil)
                      (else (cf-fn-call f "run") jolt-nil)))))))))
    d))

;; thenCombine / thenAcceptBoth / runAfterBoth: both sides settle first, then the
;; first failure in (this, other) order wins, or the function runs.
(define (cf-both a other exec f kind)
  (cf-require! f)
  (let ((b (cf-as-cf other)) (d (make-cf)))
    (cf-when-done! a
      (lambda (r)
        (cf-when-done! b
          (lambda (s)
            (cond ((cf-alt? r) (cf-settle! d (cf-encode-relay r) #f))
                  ((cf-alt? s) (cf-settle! d (cf-encode-relay s) #f))
                  (else
                   (cf-fire! d exec
                     (lambda ()
                       (cf-complete-with! d
                         (lambda ()
                           (case kind
                             ((apply) (cf-fn-call f "apply" r s))
                             ((accept) (cf-fn-call f "accept" r s) jolt-nil)
                             (else (cf-fn-call f "run") jolt-nil))))))))))))
    d))

;; applyToEither / acceptEither / runAfterEither: whichever side settles first,
;; this one if both already have. The claim box makes the loser a no-op.
(define (cf-either a other exec f kind)
  (cf-require! f)
  (let* ((b (cf-as-cf other)) (d (make-cf)) (claimed (box #f))
         (fire (lambda (r)
                 (when (sa-box-cas! claimed #f #t)
                   (if (cf-alt? r)
                       (cf-settle! d (cf-encode-relay r) #f)
                       (cf-fire! d exec
                         (lambda ()
                           (cf-complete-with! d
                             (lambda ()
                               (case kind
                                 ((apply) (cf-fn-call f "apply" r))
                                 ((accept) (cf-fn-call f "accept" r) jolt-nil)
                                 (else (cf-fn-call f "run") jolt-nil)))))))))))
    (cf-when-done! a fire)
    (cf-when-done! b fire)
    d))

;; Settle D with whatever stage G settles with, relayed — thenCompose's second half.
(define (cf-relay-stage! d g)
  (let ((g (guard (e (#t (cf-settle! d (cf-encode-throwable (jolt-unwrap-throw e)) #f) #f))
             (if (jolt-nil? g)
                 (throw-jvm 'NullPointerException "the function answered null where a CompletionStage is required")
                 (cf-as-cf g)))))
    (when g
      (cf-when-done! g (lambda (s) (cf-settle! d (cf-encode-relay s) #f))))))
;; Call F for the stage it answers, or settle D with the failure: #f then.
(define (cf-call-for-stage d f method arg)
  (guard (e (#t (cf-settle! d (cf-encode-throwable (jolt-unwrap-throw e)) #f) #f))
    (cons 'stage (cf-fn-call f method arg))))

(define (cf-compose src exec f)
  (cf-require! f)
  (let ((d (make-cf)))
    (cf-when-done! src
      (lambda (r)
        (if (cf-alt? r)
            (cf-settle! d (cf-encode-relay r) #f)
            (cf-fire! d exec
              (lambda ()
                (let ((g (cf-call-for-stage d f "apply" r)))
                  (when g (cf-relay-stage! d (cdr g)))))))))
    d))

(define (cf-exceptionally-compose src exec f)
  (cf-require! f)
  (let ((d (make-cf)))
    (cf-when-done! src
      (lambda (r)
        (if (cf-alt? r)
            (cf-fire! d exec
              (lambda ()
                (let ((g (cf-call-for-stage d f "apply" (cf-alt-ex r))))
                  (when g (cf-relay-stage! d (cdr g))))))
            (cf-settle! d r #f))))
    d))

;; handle: the function sees (value, nil) or (nil, the stored throwable) and its
;; answer is the dependent's value.
(define (cf-handle src exec f)
  (cf-require! f)
  (let ((d (make-cf)))
    (cf-when-done! src
      (lambda (r)
        (cf-fire! d exec
          (lambda ()
            (cf-complete-with! d
              (lambda ()
                (if (cf-alt? r)
                    (cf-fn-call f "apply" jolt-nil (cf-alt-ex r))
                    (cf-fn-call f "apply" r jolt-nil))))))))
    d))

;; whenComplete: the action sees what handle's function sees; the dependent
;; keeps the source's result, unless the action throws over a NORMAL result,
;; which then fails the dependent. (The JVM also records the action's throw as
;; suppressed on an earlier failure; jolt throwables carry no suppressed list.)
(define (cf-when-complete src exec f)
  (cf-require! f)
  (let ((d (make-cf)))
    (cf-when-done! src
      (lambda (r)
        (cf-fire! d exec
          (lambda ()
            (let ((thrown (guard (e (#t (jolt-unwrap-throw e)))
                            (if (cf-alt? r)
                                (cf-fn-call f "accept" jolt-nil (cf-alt-ex r))
                                (cf-fn-call f "accept" r jolt-nil))
                            cf-pending)))
              (cf-settle! d
                          (if (or (eq? thrown cf-pending) (cf-alt? r))
                              (cf-encode-relay r)
                              (cf-encode-throwable thrown))
                          #f))))))
    d))

;; exceptionally: a value passes through inline; a failure goes to the function.
(define (cf-exceptionally src exec f)
  (cf-require! f)
  (let ((d (make-cf)))
    (cf-when-done! src
      (lambda (r)
        (if (cf-alt? r)
            (cf-fire! d exec
              (lambda () (cf-complete-with! d (lambda () (cf-fn-call f "apply" (cf-alt-ex r))))))
            (cf-settle! d r #f))))
    d))

;; supplyAsync / runAsync / completeAsync. An executor that refuses the task
;; raises to the caller here, as it does on the JVM.
(define (cf-async-supply d s exec kind)
  (cf-require! s)
  (cf-execute! exec
    (lambda ()
      (cf-complete-with! d
        (lambda ()
          (if (eq? kind 'supply) (cf-fn-call s "get") (begin (cf-fn-call s "run") jolt-nil))))))
  d)

(define (cf-stage-list args)
  (let ((xs (if (and (= 1 (length args)) (not (cf? (car args))))
                (let ((s (jolt-seq (cf-require! (car args)))))
                  (if (jolt-nil? s) '() (seq->list s)))
                args)))
    (let loop ((xs xs) (acc '()))
      (if (null? xs) (reverse acc) (loop (cdr xs) (cons (cf-as-cf (car xs)) acc))))))

;; allOf: settles once every stage has, with nil, or with the first failure in
;; argument order (the JVM's tree of pairwise relays prefers the left side at
;; every node, which is the same thing).
(define (cf-all-of cfs)
  (let ((d (make-cf)) (left (box (length cfs))))
    (if (null? cfs)
        (cf-settle! d jolt-nil #f)
        (for-each
          (lambda (c)
            (cf-when-done! c
              (lambda (_)
                (when (let retry ()
                        (let ((n (unbox left)))
                          (if (box-cas! left n (fx- n 1)) (fx=? n 1) (retry))))
                  (cf-settle! d
                              (let loop ((cs cfs))
                                (if (null? cs)
                                    jolt-nil
                                    (let ((r (cf-result (car cs))))
                                      (if (cf-alt? r) (cf-encode-relay r) (loop (cdr cs))))))
                              #f)))))
          cfs))
    d))
;; anyOf: the first stage to settle, and of stages already settled the first in
;; argument order (each registers in turn, and one already settled fires as it
;; registers). Of none, a future that never completes.
(define (cf-any-of cfs)
  (let ((d (make-cf)))
    (for-each (lambda (c) (cf-when-done! c (lambda (r) (cf-settle! d (cf-encode-relay r) #f)))) cfs)
    d))

;; orTimeout / completeOnTimeout. The deadline is the shared timer's (async.ss),
;; and the settle is handed to the async pool rather than run on the timer
;; thread, so dependents never run there; settling the future cancels the timer.
(define (cf-on-timeout! self amount unit r)
  (when (cf-pending? (cf-result self))
    (let ((e (jolt-timer-at! (ms->deadline-millis (tu->ms amount unit))
               (lambda () (cf-execute! 'default (lambda () (cf-settle! self (r) #f)))))))
      (cf-when-done! self (lambda (_) (jolt-timer-cancel! e)))))
  self)

;; CompletableFuture.delayedExecutor(delay, unit[, executor]): an Executor that
;; hands each task to the base executor once the delay has passed.
(define (make-cf-delayed-executor amount unit base)
  (make-jhost "cf-delayed-executor" (vector (tu->ms amount unit) base)))
(register-host-methods! "cf-delayed-executor"
  (list (cons "execute" (lambda (self r)
          (cf-require! r)
          (let ((st (jhost-state self)))
            (jolt-timer-at! (ms->deadline-millis (vector-ref st 0))
              (lambda () (cf-execute! (vector-ref st 1) (lambda () (cf-fn-call r "run"))))))
          jolt-nil))
        (cons "toString" host-object-tostring)))

;; java.util.concurrent.Future.State, what state() answers: an enum, so each
;; constant is one object and = compares them by identity.
(define future-state-constants
  (map (lambda (nm) (cons nm (make-jhost "future-state" nm)))
       '("RUNNING" "SUCCESS" "FAILED" "CANCELLED")))
(define (future-state nm) (cdr (assoc nm future-state-constants)))
(register-enum-class! "java.util.concurrent.Future$State" future-state-constants)

(define (cf-to-string self)
  (let* ((st (jhost-state self))
         (snap (jolt-with-mutex (vector-ref st 1)
                 (cons (vector-ref st 0) (length (vector-ref st 3)))))
         (r (car snap)) (n (cdr snap)))
    (string-append
      "java.util.concurrent.CompletableFuture@"
      ;; Integer.toHexString of the identity hash: unsigned, lowercase
      (string-downcase (number->string (bitwise-and (jolt-identity-hasheq self) #xFFFFFFFF) 16))
      (cond ((cf-pending? r)
             (if (fx=? n 0)
                 "[Not completed]"
                 (string-append "[Not completed, " (number->string n) " dependents]")))
            ((cf-alt? r) (string-append "[Completed exceptionally: " (jolt-str-render-one (cf-alt-ex r)) "]"))
            (else "[Completed normally]")))))
(register-str-render! cf? cf-to-string)

(for-each (lambda (nm) (register-class-ctor! nm (case-lambda (() (make-cf)))))
          '("CompletableFuture" "java.util.concurrent.CompletableFuture"))
(let ((statics
       (list
         (cons "completedFuture" (lambda (v) (make-cf* v)))
         (cons "failedFuture" (lambda (x) (make-cf* (make-cf-alt (cf-require! x)))))
         (cons "supplyAsync" (case-lambda
           ((s) (cf-async-supply (make-cf) s 'default 'supply))
           ((s exec) (cf-require! s) (cf-async-supply (make-cf) s (cf-require! exec) 'supply))))
         (cons "runAsync" (case-lambda
           ((r) (cf-async-supply (make-cf) r 'default 'run))
           ((r exec) (cf-require! r) (cf-async-supply (make-cf) r (cf-require! exec) 'run))))
         (cons "allOf" (lambda cfs (cf-all-of (cf-stage-list cfs))))
         (cons "anyOf" (lambda cfs (cf-any-of (cf-stage-list cfs))))
         (cons "delayedExecutor" (case-lambda
           ((amount unit) (make-cf-delayed-executor amount (cf-require! unit) 'default))
           ((amount unit exec) (make-cf-delayed-executor amount (cf-require! unit) (cf-require! exec))))))))
  (register-class-statics! "CompletableFuture" statics)
  (register-class-statics! "java.util.concurrent.CompletableFuture" statics))

(register-host-methods! "completable-future"
  (list
    (cons "complete" (lambda (self v) (cf-settle! self v #f)))
    (cons "completeExceptionally" (lambda (self x) (cf-settle! self (make-cf-alt (cf-require! x)) #f)))
    ;; cancel(mayInterruptIfRunning): the flag is ignored on the JVM too — a
    ;; CompletableFuture has no thread of its own to interrupt. It answers true
    ;; for a future that is cancelled, whichever call cancelled it.
    (cons "cancel" (lambda (self _may-interrupt)
      (or (cf-settle! self (make-cf-alt (cf-new-cancellation)) #f)
          (cf-cancelled? self))))
    (cons "isDone" (lambda (self) (not (cf-pending? (cf-result self)))))
    (cons "isCancelled" (lambda (self) (cf-cancelled? self)))
    (cons "isCompletedExceptionally" (lambda (self) (cf-alt? (cf-result self))))
    (cons "get" (case-lambda
      ((self) (cf-get* self #f cf-timeout-throw))
      ((self amount unit) (cf-get* self (tu->ms amount unit) cf-timeout-throw))))
    (cons "join" (lambda (self) (cf-report-join (cf-await self "CompletableFuture.join" #f #f))))
    (cons "getNow" (lambda (self v)
      (let ((r (cf-result self))) (if (cf-pending? r) v (cf-report-join r)))))
    (cons "resultNow" (lambda (self)
      (let ((r (cf-result self)))
        (if (or (cf-pending? r) (cf-alt? r))
            (throw-jvm 'IllegalStateException jolt-nil)
            r))))
    (cons "exceptionNow" (lambda (self)
      (let ((r (cf-result self)))
        (if (and (cf-alt? r) (not (cf-cancellation? (cf-alt-ex r))))
            (let ((x (cf-alt-ex r)))
              (if (and (cf-completion? x) (not (jolt-nil? (cf-cause x)))) (cf-cause x) x))
            (throw-jvm 'IllegalStateException jolt-nil)))))
    (cons "state" (lambda (self)
      (let ((r (cf-result self)))
        (future-state (cond ((cf-pending? r) "RUNNING")
                            ((not (cf-alt? r)) "SUCCESS")
                            ((cf-cancellation? (cf-alt-ex r)) "CANCELLED")
                            (else "FAILED"))))))
    (cons "obtrudeValue" (lambda (self v) (cf-settle! self v #t) jolt-nil))
    (cons "obtrudeException" (lambda (self x) (cf-settle! self (make-cf-alt (cf-require! x)) #t) jolt-nil))
    (cons "getNumberOfDependents" (lambda (self)
      (let ((st (jhost-state self)))
        (jolt-with-mutex (vector-ref st 1) (length (vector-ref st 3))))))
    (cons "toCompletableFuture" (lambda (self) self))
    (cons "copy" (lambda (self)
      (let ((d (make-cf)))
        (cf-when-done! self (lambda (r) (cf-settle! d (cf-encode-relay r) #f)))
        d)))
    (cons "newIncompleteFuture" (lambda (self) (make-cf)))
    (cons "toString" cf-to-string)
    (cons "orTimeout" (lambda (self amount unit)
      (cf-on-timeout! self amount (cf-require! unit)
        (lambda () (make-cf-alt (jolt-host-throwable "java.util.concurrent.TimeoutException" jolt-nil))))))
    (cons "completeOnTimeout" (lambda (self v amount unit)
      (cf-on-timeout! self amount (cf-require! unit) (lambda () v))))
    (cons "completeAsync" (case-lambda
      ((self s) (cf-async-supply self s 'default 'supply))
      ((self s exec) (cf-require! s) (cf-async-supply self s (cf-require! exec) 'supply))))
    (cons "thenApply" (lambda (self f) (cf-uni self #f f 'apply)))
    (cons "thenApplyAsync" (cf-async-1 (lambda (self exec f) (cf-uni self exec f 'apply))))
    (cons "thenAccept" (lambda (self f) (cf-uni self #f f 'accept)))
    (cons "thenAcceptAsync" (cf-async-1 (lambda (self exec f) (cf-uni self exec f 'accept))))
    (cons "thenRun" (lambda (self f) (cf-uni self #f f 'run)))
    (cons "thenRunAsync" (cf-async-1 (lambda (self exec f) (cf-uni self exec f 'run))))
    (cons "thenCombine" (lambda (self o f) (cf-both self o #f f 'apply)))
    (cons "thenCombineAsync" (cf-async-2 (lambda (self o exec f) (cf-both self o exec f 'apply))))
    (cons "thenAcceptBoth" (lambda (self o f) (cf-both self o #f f 'accept)))
    (cons "thenAcceptBothAsync" (cf-async-2 (lambda (self o exec f) (cf-both self o exec f 'accept))))
    (cons "runAfterBoth" (lambda (self o f) (cf-both self o #f f 'run)))
    (cons "runAfterBothAsync" (cf-async-2 (lambda (self o exec f) (cf-both self o exec f 'run))))
    (cons "applyToEither" (lambda (self o f) (cf-either self o #f f 'apply)))
    (cons "applyToEitherAsync" (cf-async-2 (lambda (self o exec f) (cf-either self o exec f 'apply))))
    (cons "acceptEither" (lambda (self o f) (cf-either self o #f f 'accept)))
    (cons "acceptEitherAsync" (cf-async-2 (lambda (self o exec f) (cf-either self o exec f 'accept))))
    (cons "runAfterEither" (lambda (self o f) (cf-either self o #f f 'run)))
    (cons "runAfterEitherAsync" (cf-async-2 (lambda (self o exec f) (cf-either self o exec f 'run))))
    (cons "thenCompose" (lambda (self f) (cf-compose self #f f)))
    (cons "thenComposeAsync" (cf-async-1 (lambda (self exec f) (cf-compose self exec f))))
    (cons "handle" (lambda (self f) (cf-handle self #f f)))
    (cons "handleAsync" (cf-async-1 (lambda (self exec f) (cf-handle self exec f))))
    (cons "whenComplete" (lambda (self f) (cf-when-complete self #f f)))
    (cons "whenCompleteAsync" (cf-async-1 (lambda (self exec f) (cf-when-complete self exec f))))
    (cons "exceptionally" (lambda (self f) (cf-exceptionally self #f f)))
    (cons "exceptionallyAsync" (cf-async-1 (lambda (self exec f) (cf-exceptionally self exec f))))
    (cons "exceptionallyCompose" (lambda (self f) (cf-exceptionally-compose self #f f)))
    (cons "exceptionallyComposeAsync" (cf-async-1 (lambda (self exec f) (cf-exceptionally-compose self exec f))))))

;; java.util.concurrent.locks.ReentrantLock — a reentrant mutual-exclusion lock.
;; State: #(monitor), one MONITOR record of its own (make-monitor above), not the
;; object's — (locking lk …) and (.lock lk) are different locks on the JVM and stay
;; different here.
;;
;; A MONITOR AND NOT A CHEZ MUTEX, for the reasons set out at jolt-with-monitor and
;; for the third time in this file (jolt-ga8o, after jolt-3a87 for the monitor and
;; jolt-pb2s for the STM lock). This held a Chez mutex from .lock to .unlock with
;; (current-interrupt-box) as the owner, and a ReentrantLock's region is a USER body:
;; it is as long as the caller's critical section and the caller may park in it. So
;; both halves of locks.ss's premise are false of it, and it failed both ways.
;;
;;   Two fibers on one carrier share a thread and therefore shared that owner, so
;;   the second one's .lock read the owner as ITS OWN identity, took the reentrant
;;   arm, and walked into a section the first was halfway through. Measured with one
;;   carrier and two fibers that lock, park on a channel and unlock: occupancy inside
;;   the section reached 2. The owner test in .unlock is the same test, so a fiber
;;   could also release a lock a sibling took, dropping the mutex the sibling was
;;   still relying on with nothing raised to say so.
;;
;;   And the acquire was COUNTED, so jolt-locks-held stayed above zero for the whole
;;   body and forever if the holder parked in it. The scheduler refuses to preempt
;;   while that count is non-zero, so every later fiber on that carrier became
;;   unpreemptible too — the unbounded starvation window the head of fibers.ss says
;;   no setting can open, reachable from ordinary jolt code.
;;
;; Ownership as a field fixes both at once, and it is not a coincidence that it fixes
;; both: they are one bug seen from two sides, which is that an OS mutex has THREAD
;; granularity and a fiber is not a thread. A field survives a switch, and no counted
;; lock is held while the body runs.
;;
;; What the bounded and interruptible acquires add on top is a deadline and an
;; interrupt check, which monitor-wait! has neither of, so those two poll — through
;; monitor-poll-round!, which yields on a fiber. Sleeping there was the reason a timed
;; tryLock against a sibling fiber could never succeed: the carrier it slept is the one
;; the holder needs in order to reach its release.
(define (make-reentrant-lock) (make-jhost "reentrant-lock" (vector (make-monitor))))
(define (rlock-monitor self) (vector-ref (jhost-state self) 0))
(for-each (lambda (nm) (register-class-ctor! nm (lambda _ (make-reentrant-lock))))
          '("ReentrantLock" "java.util.concurrent.locks.ReentrantLock"))
(define (rlock-interrupted-check! me)
  (when (unbox me)
    (set-box! me #f)
    (jolt-throw (jolt-host-throwable "java.lang.InterruptedException" "lock interrupted"))))
(register-host-methods! "reentrant-lock"
  ;; An uninterruptible acquire: a fiber contender parks on the monitor's waiter
  ;; list and a thread waits on its condition. monitor-exit! wakes both.
  (list (cons "lock" (lambda (self)
          (parameterize ((thread-enter-state 'WAITING)) (monitor-enter! (rlock-monitor self)))
          jolt-nil))
        ;; monitor-exit! is where "unlock from a non-owner throws" already lives, and
        ;; it throws the IllegalMonitorStateException the JVM does.
        (cons "unlock" (lambda (self) (monitor-exit! (rlock-monitor self)) jolt-nil))
        ;; tryLock() gives up at once; tryLock(timeout, unit) keeps trying until the
        ;; deadline. The timeout used to be discarded, so the bounded form gave up
        ;; immediately on any lock that happened to be held at that instant; then it
        ;; was honoured but polled with (sleep), which on a fiber guaranteed it would
        ;; run to the deadline whenever the holder was a sibling.
        (cons "tryLock" (lambda (self . args)
          ;; The deadline is built only once the first try has failed, so an
          ;; uncontended tryLock(timeout) reads no clock. It is exact, and the
          ;; poll gives up once it has passed: (<= (mono-millis) deadline) kept
          ;; trying through the whole millisecond after it, and the 1ms naps on
          ;; top made tryLock(1ms) take 3.5ms. The timed form is interruptible,
          ;; as the JDK's: a set flag throws before the first try, free lock or
          ;; not, and an interrupt during the poll throws too. tryLock() is not.
          (let ((m (rlock-monitor self))
                (ms (tu-args->ms args)))
            (if (not ms)
                (monitor-try-enter! m)
                (let ((me (current-interrupt-box)))
                  (rlock-interrupted-check! me)
                  (or (monitor-try-enter! m)
                      (and (> ms 0)
                           (let ((deadline (ms->deadline-millis ms)))
                             (call-with-thread-state 'TIMED_WAITING
                               (lambda ()
                                 (let attempt ()
                                   ;; Chez has no timed acquire, so poll. The round
                                   ;; yields first so the holder can run.
                                   (cond ((mono-due? deadline) (monitor-try-enter! m))
                                         (else (monitor-poll-round! deadline)
                                               (rlock-interrupted-check! me)
                                               (or (monitor-try-enter! m) (attempt)))))))))))))))
        (cons "lockInterruptibly" (lambda (self)
          (let ((m (rlock-monitor self))
                (me (current-interrupt-box)))
            (rlock-interrupted-check! me)
            (call-with-thread-state 'WAITING
              (lambda ()
                (let loop ()
                  (unless (monitor-try-enter! m)
                    (monitor-poll-round!)
                    (rlock-interrupted-check! me)
                    (loop)))))
            jolt-nil)))
        ;; isLocked is "held by ANY thread"; getHoldCount and isHeldByCurrentThread are
        ;; about the CURRENT one, and the count is 0 rather than the raw depth for a
        ;; caller that does not hold it. The old versions read the raw fields, so
        ;; getHoldCount reported the holder's depth to everybody.
        (cons "isLocked" (lambda (self) (monitor-locked? (rlock-monitor self))))
        (cons "getHoldCount" (lambda (self) (monitor-self-count (rlock-monitor self))))
        (cons "isHeldByCurrentThread"
              (lambda (self) (monitor-held-by-self? (rlock-monitor self))))
        ;; ReentrantLock.toString: Object's, then [Unlocked] or [Locked by thread
        ;; <owner's name>]. The owner word is read as the JDK's getOwner reads it,
        ;; with no lock: a snapshot.
        (cons "toString" (lambda (self)
          (let ((o (monitor-owner-id (rlock-monitor self))))
            (string-append (host-object-tostring self)
                           (if o (string-append "[Locked by thread " (monitor-owner-name o) "]") "[Unlocked]")))))))
;; The name of the thread a monitor's owner word stands for: a fiber's own
;; Thread, or the platform thread whose interrupt box it is.
(define (monitor-owner-name o)
  (let ((th (cond ((jolt-fiber? o) (fiber-thread-object o))
                  ((box? o)
                   (let ((id (jolt-with-mutex thread-boxes-mu
                               (let-values (((ks vs) (hashtable-entries thread-boxes-by-id)))
                                 (let loop ((i 0))
                                   (cond ((fx=? i (vector-length ks)) #f)
                                         ((eq? (vector-ref vs i) o) (vector-ref ks i))
                                         (else (loop (fx+ i 1)))))))))
                     (and id (thread-handle-for-id id))))
                  (else #f))))
    (if th (jolt-final-str (record-method-dispatch th "getName" jolt-nil)) "")))

;; --- main-thread executor ---------------------------------------------------
;; Lets a worker thread (e.g. an nREPL eval future) run a thunk on the thread
;; that owns the process's main loop. This is for main-thread-affine work: many
;; native UI toolkits require their event loop and widget mutation to run on the
;; process main thread (on macOS, AppKit aborts if the main menu is set off the
;; main thread), so a library that runs such a loop cannot just start it on
;; whatever thread happened to call it.
;;
;; Under `jolt nrepl-server` the accept loop is backgrounded in a future and the
;; primordial thread parks in jolt-park-until-interrupt, which doubles as the main
;; pump (see below). A library that must run on the main thread marshals its work
;; through jolt-call-on-main-thread(-async) so it lands there.
;;
;; - With no pump running (e.g. `jolt -M:run` calls straight through on the main
;;   thread), call-on-main-thread(-async) runs the thunk INLINE — unchanged.
;; - A call from a thunk already executing on the pump runs inline too, so the
;;   pump can't deadlock on itself.
;; - Otherwise the thunk is enqueued; call-on-main-thread blocks for the result,
;;   call-on-main-thread-async returns right away (fire-and-forget).
;;
;; jolt-run-main-pump / stop-main-pump remain the external blocking-pump API (a
;; pump that drains queued jobs and returns on stop). The nREPL server uses the
;; interruptible park variant instead, since run-main-pump's bare condition-wait
;; can't be broken by ^C. The pump-active flag is flipped under jolt-main-queue-mu
;; in the same critical section that decides to exit, and call-on-main-thread reads
;; that flag and enqueues under the SAME mutex, so a job can never slip in after
;; the pump has left — a losing call runs inline instead of blocking on a dead pump.

(define jolt-main-queue-mu (make-mutex))
(define jolt-main-queue-cv (make-condition))
;; FIFO of jolt-main-job as a front/rear two-list queue, guarded by mu: enqueue
;; is one cons where a single list re-copied per (append q (list j)) cost
;; O(depth) under the mutex. Callers hold jolt-main-queue-mu for both ops.
(define jolt-main-queue '())            ; dequeue end, oldest first
(define jolt-main-queue-rear '())       ; enqueue end, newest first
(define (jolt-main-enqueue! j)
  (set! jolt-main-queue-rear (cons j jolt-main-queue-rear)))
;; next job or #f, rebalancing rear->front when the front drains.
(define (jolt-main-dequeue!)
  (when (and (null? jolt-main-queue) (pair? jolt-main-queue-rear))
    (set! jolt-main-queue (reverse jolt-main-queue-rear))
    (set! jolt-main-queue-rear '()))
  (and (not (null? jolt-main-queue))
       (let ((j (car jolt-main-queue)))
         (set! jolt-main-queue (cdr jolt-main-queue))
         j)))
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
      (let ((job (jolt-with-mutex jolt-main-queue-mu
                   (and (unbox jolt-main-pump-active)
                        (let ((j (make-jolt-main-job thunk #f #f jolt-nil
                                                     (make-mutex) (jolt-make-cv))))
                          (jolt-main-enqueue! j)
                          (condition-signal jolt-main-queue-cv)
                          j)))))
        (if (not job)
            (jolt-invoke thunk)         ; no pump (or stopped) — inline, like -M:run
            (begin
              ;; A fiber parks here: the pump is another thread, but blocking the
              ;; carrier would stop every other fiber on it for as long as the main
              ;; thread takes to run the job.
              (jolt-cv-wait (jolt-main-job-mu job) (jolt-main-job-cv job) #f
                (lambda (_timed-out?)
                  (if (jolt-main-job-done? job) #t jolt-cv-again)))
              (if (jolt-main-job-ok? job)
                  (jolt-main-job-val job)
                  (raise (jolt-main-job-val job))))))))

;; Fire-and-forget variant: schedule `thunk` on the pump and return immediately,
;; without waiting for it to finish. This suits a main-thread event loop started
;; from nREPL — the thunk blocks the pump for the loop's whole lifetime, but the
;; eval that started it returns, so the REPL session stays live. Errors in the
;; thunk have no waiter to re-raise into (the caller has already returned). With no
;; pump active it runs inline (like -M:run, where the caller is already on the main
;; thread). A reentrant call (already on the pump) also runs inline.
(define (jolt-call-on-main-thread-async thunk)
  (if (jolt-in-main-pump?)
      (begin (jolt-invoke thunk) jolt-nil)
      (let ((enq (jolt-with-mutex jolt-main-queue-mu
                   (and (unbox jolt-main-pump-active)
                        (let ((j (make-jolt-main-job thunk #f #f jolt-nil
                                                     (make-mutex) (jolt-make-cv))))
                          (jolt-main-enqueue! j)
                          (condition-signal jolt-main-queue-cv)
                          #t)))))
        (unless enq (jolt-invoke thunk))
        jolt-nil)))

;; (exit …) rather than calling _exit: the exit handler runs the hooks, so ^C
;; while parked reaches exactly the same cleanup every other exit path does.
;; 130 = 128+SIGINT, the status a shell reports for a process killed by ^C and
;; what the shutdown watcher exits with — the two ^C paths must not disagree on
;; the status just because of which one got there first.
(define jolt-pump-kih (lambda () (exit 130)))

;; Park the calling thread until a keyboard interrupt (^C), running the shutdown
;; hooks and exiting when it arrives — AND own the main-thread pump while parked.
;; The nREPL server parks the primordial thread here: it must both be interruptible
;; for ^C and be the thread call-on-main-thread(-async) marshals onto, so a worker
;; that must run on the main thread (e.g. a UI toolkit's event loop) runs on THIS
;; (main) thread instead of inline off-main (on macOS an off-main native UI call
;; can abort the process).
;;
;; Interruptibility is subtle. A keyboard interrupt raised while a thread blocks
;; in condition-wait is only *delivered* at the next Scheme safe point OUTSIDE the
;; wait, and the ordinary dequeue primitives (jolt-with-mutex / null? / car) are NOT
;; reliable interrupt-check points — only a real syscall is. run-main-pump idles
;; in a bare condition-wait with no such safe point, so a pending ^C is never
;; delivered (it also holds jolt-main-queue-mu across the wait — the "thread does
;; not own mutex" hazard). This loop idles in (sleep …) instead: sleep is a
;; syscall Chez interrupt-checks around, so a pending ^C fires jolt-pump-kih there
;; (shutdown hooks -> exit) within one poll interval. jolt-park-poll-ms is short
;; enough that the latency to pick up a job after an nREPL eval is imperceptible.
;;
;; SIGINT is unblocked in this thread first (it was masked by jolt-block-sigint so
;; the accept loop inherited a blocked mask and couldn't absorb ^C in its foreign
;; accept() call). pump-active is set so call-on-main-thread(-async) enqueues onto
;; us rather than running inline. While a job runs (e.g. an event loop that blocks
;; for its lifetime) the mutex is NOT held, so other threads keep enqueueing; ^C
;; during that foreign call is uninterruptible, exactly as under -M:run.
(define jolt-park-poll-ms 50)
(define (jolt-park-until-interrupt)
  (keyboard-interrupt-handler jolt-pump-kih)
  ;; …unless the shutdown watcher already owns SIGINT (it does whenever a hook was
  ;; registered, which is the nREPL server's own case). Unblocking here would let
  ;; the kernel deliver ^C to this thread instead of leaving it pending for
  ;; sigwait, and then the two paths race for the same shutdown. The watcher's is
  ;; the one that works from a foreign call, so leave the signal to it; the
  ;; handler above stays installed for the unarmed case.
  (unless (jolt-shutdown-watcher-running?)
    (jolt-set-sigint-blocked #f))
  (jolt-with-mutex jolt-main-queue-mu (set-box! jolt-main-pump-active #t))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (let loop ()
        (let ((job (jolt-with-mutex jolt-main-queue-mu (jolt-main-dequeue!))))
          (if job
              ;; run the job on THIS (main) thread — a UI event loop blocks here
              ;; for its lifetime; the mutex is released so other threads keep
              ;; enqueueing. jolt-in-main-pump? makes a reentrant call-on-main-thread
              ;; run inline instead of self-deadlocking.
              (let ((r (dynamic-wind
                         (lambda () (jolt-in-main-pump? #t))
                         (lambda ()
                           (guard (e (#t (cons #f e)))
                             (cons #t (jolt-invoke (jolt-main-job-thunk job)))))
                         (lambda () (jolt-in-main-pump? #f)))))
                (jolt-with-mutex (jolt-main-job-mu job)
                  (jolt-main-job-ok?-set! job (car r))
                  (jolt-main-job-val-set! job (cdr r))
                  (jolt-main-job-done?-set! job #t)
                  (jolt-cv-wake! (jolt-main-job-cv job))))
              ;; idle: sleep — an interrupt-checked syscall — so a pending ^C fires.
              (sleep (ms->duration jolt-park-poll-ms)))
          (loop))))
    (lambda ()
      (jolt-with-mutex jolt-main-queue-mu (set-box! jolt-main-pump-active #f))))
  jolt-nil)

(define (jolt-run-main-pump)
  (jolt-with-mutex jolt-main-queue-mu
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
        (let ((job (jolt-with-mutex jolt-main-queue-mu
                     (let wait ()
                       (cond
                         ((jolt-main-dequeue!) => values)
                         ((unbox jolt-main-pump-stop)
                          ;; drain done, told to exit — clear active in the same
                          ;; critical section so no job can be enqueued after.
                          (set-box! jolt-main-pump-active #f)
                          #f)
                         (else (jolt-condition-wait jolt-main-queue-cv jolt-main-queue-mu)
                               (wait)))))))
          (when job
            (let ((r (dynamic-wind
                       (lambda () (jolt-in-main-pump? #t))
                       (lambda ()
                         (guard (e (#t (cons #f e)))
                           (cons #t (jolt-invoke (jolt-main-job-thunk job)))))
                       (lambda () (jolt-in-main-pump? #f)))))
              (jolt-with-mutex (jolt-main-job-mu job)
                (jolt-main-job-ok?-set! job (car r))
                (jolt-main-job-val-set! job (cdr r))
                (jolt-main-job-done?-set! job #t)
                (jolt-cv-wake! (jolt-main-job-cv job))))
            (loop)))))
    (lambda ()
      (jolt-with-mutex jolt-main-queue-mu (set-box! jolt-main-pump-active #f))))
  jolt-nil)

(define (jolt-stop-main-pump)
  (jolt-with-mutex jolt-main-queue-mu
    (set-box! jolt-main-pump-stop #t)
    (condition-broadcast jolt-main-queue-cv))
  jolt-nil)

;; --- POSIX signal masks ------------------------------------------------------
;; pthread_sigmask/sigaddset are libc/libpthread symbols, resolvable once the
;; process object is loaded (as the socket fns already are). foreign-procedure
;; resolves its symbol eagerly, and these POSIX signal fns don't exist on
;; Windows — resolving them unguarded aborted startup ("no entry for
;; pthread_sigmask"). Guard so a non-POSIX host yields #f; every caller below
;; then no-ops (Windows delivers ^C through the console, not a per-thread mask).
;;
;; The sets live in FOREIGN memory, not bytevectors: the watcher below hands one
;; to a __collect_safe sigwait and stays parked in it, where the collector is
;; free to move a bytevector out from under the pointer C is holding.
(define c-pthread-sigmask
  (jolt-foreign-proc-safe "pthread_sigmask" '(int void* void*) 'int))
(define c-sigemptyset (jolt-foreign-proc-safe "sigemptyset" '(void*) 'int))
(define c-sigaddset (jolt-foreign-proc-safe "sigaddset" '(void* int) 'int))
;; POSIX SIG_BLOCK/SIG_UNBLOCK numerics differ by platform: Linux/glibc 0/1,
;; Darwin/macOS 1/2 (SIG_UNBLOCK is SIG_BLOCK+1 on both). Resolve SIG_BLOCK for
;; this host from the os-family property.
(define jolt-sig-block-how (if (eq? (sa-os-family) 'macos) 1 0))
(define jolt-sig-unblock-how (+ jolt-sig-block-how 1))
(define jolt-sig-setmask-how (+ jolt-sig-block-how 2))
(define jolt-sigmask-ok? (and c-pthread-sigmask c-sigemptyset c-sigaddset #t))
;; 128 bytes covers Linux's 1024-bit sigset_t and is larger than macOS's 4-byte
;; one. The caller owns the block and frees it (or keeps it for the process's
;; life, as the watcher does).
(define (jolt-make-sigset sigs)
  (let ((s (sa-foreign-alloc 128)))
    (c-sigemptyset s)
    (for-each (lambda (n) (c-sigaddset s n)) sigs)
    s))

;; Run THUNK with (HOW, SET) applied to THIS thread's mask and put the mask back
;; exactly as it was afterwards. Both of jolt's mask windows are this shape: the
;; spawn window that hands a child a default mask, and the fork window that hands
;; a new thread jolt's. The restore is SIG_SETMASK from the saved old set rather
;; than the inverse operation, because the caller's own mask is whatever it is and
;; "unblock what we blocked" would unblock a signal the caller had blocked for its
;; own reasons.
(define (jolt-with-sigmask how set thunk)
  (if (not (and jolt-sigmask-ok? set))
      (thunk)
      (let* ((old (sa-foreign-alloc 128))
             (restore (lambda ()
                        (c-pthread-sigmask jolt-sig-setmask-how old 0)
                        (sa-foreign-free old))))
        (c-pthread-sigmask how set old)
        ;; not dynamic-wind: its after-thunk can run more than once, and this one
        ;; frees. Normal return and throw both restore exactly once; multiple
        ;; values survive the round trip.
        (let ((vals (guard (e (#t (restore) (raise e))) (call-with-values thunk list))))
          (restore)
          (apply values vals)))))

;; Per-thread SIGINT mask. A worker thread parked in a foreign call (the nREPL
;; accept loop in c-accept, or a conn handler) can't run Chez's keyboard-interrupt
;; handler on ^C, so if SIGINT is delivered there the process hangs — which the
;; fork guard further down rules out for every thread jolt forks. What is left for
;; this to say is the PRIMORDIAL thread's mask, the one no fork can set: a server
;; blocks SIGINT here before it starts accepting, and park-until-interrupt unblocks
;; it once its handler is installed, so ^C lands on the parked thread and nowhere
;; else.
(define (jolt-set-sigint-blocked block?)
  (when jolt-sigmask-ok?
    (let ((set (jolt-make-sigset '(2)))              ; SIGINT = 2
          (old (sa-foreign-alloc 128)))
      (c-pthread-sigmask (if block? jolt-sig-block-how jolt-sig-unblock-how) set old)
      (sa-foreign-free set)
      (sa-foreign-free old)))
  jolt-nil)

;; --- shutdown hooks ----------------------------------------------------------
;; ONE registry for every "run this before the process ends" hook, whichever API
;; asked for it: jolt.host/add-shutdown-hook (nREPL closing its socket and
;; dropping .nrepl-port on ^C) and java.lang.Runtime's addShutdownHook, which is
;; what babashka.process's `:shutdown` option registers. Those were two separate
;; lists with one runner between them, and the Runtime one was written and never
;; read — so `:shutdown destroy-tree` cleaned up nothing at all, on any exit path
;; (#571).
;;
;; The hooks run:
;;   - on every (exit n), via the exit handler the arming below installs — a
;;     -main that returns, an explicit (System/exit n), and an uncaught throw all
;;     end there;
;;   - on SIGINT (^C), SIGTERM and SIGHUP, through the watcher thread further down;
;;   - on ^C while parked in park-until-interrupt without the watcher armed
;;     (jolt-pump-kih exits) — the fallback for a hook-free park.
;; Once per process, in registration order, each hook isolated so one that
;; throws cannot keep the rest from running.
;;
;; Entries are (key . thunk): the key is what removeShutdownHook matches on (the
;; JVM hands back the same Thread object), and is the thunk itself for the
;; anonymous jolt.host form.
(define jolt-shutdown-hooks (box '()))
(define jolt-shutdown-mutex (make-mutex))
(define jolt-shutdown-ran (box #f))

(define (jolt-register-shutdown-hook! key thunk)
  ;; A program's hook is a second mutator: on a signal the watcher thread runs it
  ;; while the program's own threads are still running, and it can touch whatever
  ;; jolt value they can. That is what jolt-mt? guards. Flipping HERE is race-free
  ;; for the same reason the fork does it: the registering thread is either
  ;; registering or forcing a cell, never both, and if any other thread exists the
  ;; flag is already on. Flipping later, as the watcher wakes, is not: the program
  ;; may be halfway through realizing a cell the hook goes on to force, and what it
  ;; published on the single-threaded path went out without a release fence.
  (jolt-mark-mt!)
  (jolt-enlist-shutdown-hook! key thunk))

(define (jolt-add-shutdown-hook thunk) (jolt-register-shutdown-hook! thunk thunk))

;; A hook of the runtime's own: the JOLT_PROFILE reports and the block-count dump
;; a --profile-gen binary writes (sampler.ss, cli-core.ss). Every built binary's
;; launcher registers the dump, so marking the process multi-threaded for it put
;; every such program's lazy seqs on the claim path, up to 1.45x slower. These
;; read only what the runtime keeps for them (sample tables, block counters) and
;; values they build themselves, never a cell or atom the program could be in the
;; middle of, so they are not a second mutator of anything the flag guards. They
;; run on the exiting thread, or on the watcher, which marks the process
;; multi-threaded as it wakes so that its own reads take the guarded paths.
(define (jolt-add-runtime-shutdown-hook thunk) (jolt-enlist-shutdown-hook! thunk thunk))

(define (jolt-enlist-shutdown-hook! key thunk)
  (jolt-with-mutex jolt-shutdown-mutex
    (set-box! jolt-shutdown-hooks (cons (cons key thunk) (unbox jolt-shutdown-hooks))))
  ;; outside the mutex: arming takes it to test the watcher's flag
  (jolt-arm-shutdown!)
  jolt-nil)

;; #t when a hook keyed by KEY was registered and is now removed, matching
;; Runtime.removeShutdownHook's boolean.
(define (jolt-remove-shutdown-hook! key)
  (jolt-with-mutex jolt-shutdown-mutex
    (let* ((before (unbox jolt-shutdown-hooks))
           (after (remp (lambda (e) (eq? (car e) key)) before)))
      (set-box! jolt-shutdown-hooks after)
      (not (= (length before) (length after))))))

;; Runs the hooks exactly once per process, whichever exit path gets here first.
;; The once-flag is flipped under the mutex together with taking the list, so a
;; SIGTERM landing while the main thread is already exiting cannot run them twice.
(define (jolt-run-shutdown-hooks!)
  (let ((hooks (jolt-with-mutex jolt-shutdown-mutex
                 (and (not (unbox jolt-shutdown-ran))
                      (begin (set-box! jolt-shutdown-ran #t)
                             (reverse (unbox jolt-shutdown-hooks)))))))
    (when hooks
      (for-each (lambda (e) (guard (_ (#t #f)) ((cdr e)))) hooks)))
  jolt-nil)

;; The Runnable a java.lang.Thread was constructed with, or #f (Thread() with no
;; target is legal). A shutdown hook is registered AS a Thread — that is the JVM's
;; API — and the runner invokes its body directly on the thread doing the
;; shutdown, rather than forking one thread per hook only to wait for it.
(define (jolt-thread-body th)
  (and (jhost? th)
       (string=? (jhost-tag th) "user-thread")
       (vector-ref (jhost-state th) 0)))

;; Every exit runs the hooks first, then flushes — a hook that prints must reach
;; the console like any other output. Guarded: a hook that throws, or a broken
;; pipe on the flush, must not turn exit into a second crash.
;;
;; Installed at RUN time, not as a top-level form. A standalone binary boots by
;; loading a boot file and then calling Sscheme_start, and whatever the boot
;; file's top-level forms left in exit-handler is not what is in force by the time
;; the program runs — a wrapper installed at load time is simply never called
;; there, which is why hooks ran under `chez --script` and not in a built binary.
;;
;; PER THREAD, because Chez's exit-handler is a thread parameter: a wrapper
;; installed on one thread is invisible to (exit) called on another. So every
;; entry point installs it on the main thread (jolt-cli-run, and the launcher a
;; `jolt build` emits), and arming installs it on whatever thread registered the
;; hook — which is the thread most likely to be the one that later exits.
(define jolt-exit-handler-installed (make-thread-parameter #f))
(define (jolt-install-exit-handler!)
  (unless (jolt-exit-handler-installed)
    (jolt-exit-handler-installed #t)
    (let ((base (exit-handler)))
      (exit-handler
        (lambda args
          (guard (_ (#t #f)) (jolt-run-shutdown-hooks!))
          (guard (_ (#t #f)) (flush-output-port (current-output-port)))
          (guard (_ (#t #f)) (flush-output-port (current-error-port)))
          (apply base args)))))
  jolt-nil)

;; --- SIGTERM / SIGHUP / SIGINT -----------------------------------------------
;; The signals a JVM runs shutdown hooks for, and jolt takes the same three:
;; SIGTERM (the default `kill`, what a supervisor sends), SIGHUP, and SIGINT (^C).
;; Same numbers on Linux and macOS.
;;
;; A JVM answers all three alike, whatever the program is doing and whichever
;; thread registered the hook: it runs the hooks and exits 128+signal. Measured on
;; OpenJDK 20 / Clojure 1.12.6, for a hook registered on the main thread, a hook
;; registered on a worker, and no hook at all: 130 / 143 / 129 in all nine cells.
;; It can answer that uniformly because a dedicated signal dispatcher thread owns
;; the three from VM startup.
;;
;; jolt's watcher is that thread, and the CLI arms it at entry (cli-core.ss, and
;; the launcher a `jolt build` emits) for the same reason the JVM does it at
;; startup rather than on demand: nothing but a thread itself can mask a signal in
;; it, so arming later — on the first hook, possibly from a worker — leaves the
;; PRIMORDIAL thread unmasked, and the kernel delivers there. That meant SIG_DFL
;; for TERM/HUP, a hard kill with the hooks unrun, and for INT Chez's
;; keyboard-interrupt handler, which unwinds to Chez's top level and exits 255
;; without ever reaching the exit handler (jolt-na7, and #1098 for the worker
;; case).
;;
;; Arming is idempotent and still happens on the first registration too, for a
;; runtime embedded without jolt's CLI entry (a gate harness loading rt.ss).
;;
;; A CHILD process inherits none of this mask (jolt-with-empty-sigmask, further
;; down), so ^C on the foreground process group still kills subprocesses outright.
(define jolt-shutdown-signals '(15 1 2))
(define jolt-shutdown-sigset (and jolt-sigmask-ok? (jolt-make-sigset jolt-shutdown-signals)))
;; #t once the sigwait watcher owns the signals above — park-until-interrupt reads
;; it to decide whether SIGINT is still Chez's to deliver.
(define jolt-shutdown-armed (box #f))
(define (jolt-shutdown-watcher-running?) (unbox jolt-shutdown-armed))

;; EVERY thread jolt forks is born with those signals blocked, armed or not: the
;; fork guard below is what lazy-bridge.ss's fork-thread shadow runs around the
;; spawn. Blocking them on the arming thread alone is not enough. A new thread
;; inherits its creator's mask, so that covers the threads forked AFTER the first
;; hook and none of the ones forked before it — and a dependency that starts a
;; thread at namespace-load time (a server's accept loop, a pool) starts it long
;; before -main can register anything. The kernel picks a thread that does not
;; block the signal, so it skipped the masked arming thread and delivered to one
;; of those: ^C landed where Chez's keyboard-interrupt handler is a no-op and did
;; nothing at all — no exit, no hooks — and SIGTERM landed there at SIG_DFL, a
;; hard kill with the hooks unrun. Minimal programs have no load-time threads,
;; which is why every ^C case in the gate passed while a real app's ^C did not
;; (#1098).
;;
;; Blocked in the PARENT for the length of the spawn, not by the child on its way
;; in: the child then has the mask from its first instruction, leaving no window
;; in which a brand-new thread is the one unmasked thread in the process. The
;; parent's own mask goes back exactly as it was.
;;
;; The one thread this cannot reach is the one jolt did not fork — the primordial
;; — and that is the point: it stays the kernel's delivery target until a hook is
;; registered, so an unarmed program still dies on SIGTERM and still takes ^C
;; through Chez. Arming blocks the signals on the thread that registers the hook,
;; which is the primordial in every case jolt controls (a hook registered from a
;; forked thread leaves the primordial unmasked, and the signal goes there, just
;; as it did before any of this).
(define (jolt-fork-with-shutdown-blocked fork)
  (jolt-with-sigmask jolt-sig-block-how jolt-shutdown-sigset fork))
(set! jolt-fork-sigmask-guard jolt-fork-with-shutdown-blocked)
(define c-sigwait (jolt-foreign-proc-blocking "sigwait" '(void* void*) 'int))
;; The watcher cannot leave through Chez's (exit): called off the main thread it
;; never returns. It has already run the hooks and flushed by then, so _exit is
;; the whole of what is left to do.
(define c-underscore-exit (jolt-foreign-proc-safe "_exit" '(int) 'void))

;; ^C during a REPL evaluation interrupts the evaluation instead of the process.
;; While a target is installed here, SIGINT trips it and the process carries on;
;; with none installed SIGINT is the shutdown it always was. A target is the
;; evaluation's run-interruptible token, which stops a computation, and the
;; evaluating thread's interrupt box, set and poked as Thread.interrupt does, which
;; throws it out of a sleep or a deref the token's timer cannot reach. Where the
;; watcher is not armed (no POSIX signal masks, as on Windows) ^C reaches Chez's
;; keyboard-interrupt handler on the installing thread instead, so that handler is
;; swapped for the duration.
(define jolt-sigint-target (box #f))       ; (token . interrupt-box) or #f
(define jolt-sigint-saved-kih (box #f))
;; Tripping and uninstalling take this mutex, so the watcher cannot read a target,
;; lose the race to the uninstall, and then set the REPL thread's interrupt box
;; for the NEXT entry. Only the boxes are set under it; the wake is outside.
(define jolt-sigint-mu (make-mutex))
;; #f when the target was already uninstalled: the signal is then the shutdown.
(define (jolt-sigint-trip! target)
  (and (jolt-with-mutex jolt-sigint-mu
         (and (eq? (unbox jolt-sigint-target) target)
              (begin (set-box! (car target) #t)
                     (set-box! (cdr target) #t)
                     #t)))
       (begin (jolt-interrupt-wake-waits! (cdr target)) #t)))
(define (jolt-interrupt-on-sigint! token)
  (cond
    ((box? token)
     (let ((target (cons token (current-interrupt-box))))
       (unless (unbox jolt-sigint-saved-kih)
         (set-box! jolt-sigint-saved-kih (keyboard-interrupt-handler)))
       (keyboard-interrupt-handler (lambda () (jolt-sigint-trip! target)))
       (jolt-with-mutex jolt-sigint-mu (set-box! jolt-sigint-target target))))
    (else
     (jolt-with-mutex jolt-sigint-mu
       (let ((target (unbox jolt-sigint-target)))
         (set-box! jolt-sigint-target #f)
         ;; an interrupt the evaluation did not consume must not reach the next one
         (when (and target (unbox (car target)))
           (set-box! (cdr target) #f))))
     (when (unbox jolt-sigint-saved-kih)
       (keyboard-interrupt-handler (unbox jolt-sigint-saved-kih))
       (set-box! jolt-sigint-saved-kih #f))))
  jolt-nil)

;; Taking these signals over means their default "terminate now" disposition no
;; longer applies, so a program with nothing to clean up has to stay killable
;; even when its main thread is somewhere Scheme cannot be resumed from — parked
;; in Chez's own blocking stdin read, say, where no Scheme thread runs at all.
;; The watcher is what keeps that true: it is a DEDICATED THREAD parked in
;; sigwait, and it exits the process itself. Arming can therefore happen up front,
;; before anyone has asked for cleanup, which is what lets the primordial thread
;; be masked in time.
;;
;; sigwait rather than (register-signal-handler): Chez runs a registered handler
;; on the main thread at its next safe point, and the two ways a program most
;; often waits — condition-wait (deref of a promise) and a blocking foreign call —
;; are not safe points. The handler would never run and the process would then
;; ignore the signal entirely, which is worse than not handling it.
;;
;; Blocking the signals here puts the calling thread where every thread jolt forked
;; already is (the fork guard above), so no thread is left for the kernel to
;; deliver to and the signal stays pending for sigwait to take. The watcher itself
;; is forked DORMANT: it runs no jolt code until a signal arrives, and a program
;; that never registers a hook must not pay the multi-threaded lazy path for a
;; thread asleep in a foreign call (lazy-bridge.ss; registration is what flips the
;; flag).
(define (jolt-arm-shutdown!)
  (jolt-install-exit-handler!)
  (when (and jolt-sigmask-ok? jolt-shutdown-sigset c-sigwait c-underscore-exit)
    (let ((start? (jolt-with-mutex jolt-shutdown-mutex
                    (and (not (unbox jolt-shutdown-armed))
                         (begin (set-box! jolt-shutdown-armed #t) #t)))))
      (when start?
        (let ((old (sa-foreign-alloc 128)))
          (c-pthread-sigmask jolt-sig-block-how jolt-shutdown-sigset old)
          (sa-foreign-free old))
        (fork-thread-dormant
          (lambda ()
            (let ((sigbuf (sa-foreign-alloc 8)))
              (let loop ()
                (if (= 0 (c-sigwait jolt-shutdown-sigset sigbuf))
                    (let ((sig (sa-foreign-ref 'int sigbuf 0))
                          (target (unbox jolt-sigint-target)))
                      (if (and (= sig 2) target (jolt-sigint-trip! target))
                          (loop)
                          (begin
                            ;; this thread now runs jolt code alongside the
                            ;; program's (jolt-add-runtime-shutdown-hook)
                            (jolt-mark-mt!)
                            (guard (_ (#t #f)) (jolt-run-shutdown-hooks!))
                            (guard (_ (#t #f)) (flush-output-port (current-output-port)))
                            (guard (_ (#t #f)) (flush-output-port (current-error-port)))
                            ;; 128+signal is the status a shell reports for a process
                            ;; killed by that signal, and what the JVM exits with here.
                            (c-underscore-exit (+ 128 sig)))))
                    (loop))))))))) ; EINTR — ask again
  jolt-nil)

;; --- a child must not inherit jolt's signal mask -----------------------------
;; Both spawn paths hand the calling thread's mask straight to the child —
;; posix_spawn with a NULL attrp, and Chez's own fork — and jolt blocks signals
;; in threads for its own reasons: SIGINT so ^C reaches the thread parked in
;; park-until-interrupt rather than an nREPL worker sitting in a foreign call, and
;; SIGINT/SIGTERM/SIGHUP so the watcher above can take them. Either one inherited
;; is wrong in a way that bites at once: a child with SIGTERM blocked survives the
;; very destroy the shutdown hook exists to call, and a child with SIGINT blocked
;; ignores ^C (jolt-e5sb). A JVM gives its children a default mask; so does this.
;;
;; The mask is emptied on THIS thread for the length of the spawn and put back
;; exactly as it was, so the whole of jolt's private masking is undone at once
;; rather than a named subset of it. That leaves one fork+exec in which jolt
;; itself is unmasked: a SIGTERM landing there kills it without running hooks, as
;; it did before there were hooks, and a ^C landing there may be taken by this
;; thread instead of the parked one. Both are pre-existing behavior. The
;; window-free alternative, posix_spawnattr with POSIX_SPAWN_SETSIGMASK, reaches
;; only one of the two paths — Chez's fork takes no attributes — and two
;; mechanisms for one rule is worse than one honest window.
(define jolt-empty-sigset (and jolt-sigmask-ok? (jolt-make-sigset '())))
(define (jolt-with-empty-sigmask thunk)
  (jolt-with-sigmask jolt-sig-setmask-how jolt-empty-sigset thunk))

(def-var! "jolt.host" "call-on-main-thread" jolt-call-on-main-thread)
(def-var! "jolt.host" "call-on-main-thread-async" jolt-call-on-main-thread-async)
(def-var! "jolt.host" "run-main-pump" jolt-run-main-pump)
(def-var! "jolt.host" "stop-main-pump" jolt-stop-main-pump)
(def-var! "jolt.host" "add-shutdown-hook" jolt-add-shutdown-hook)
(def-var! "jolt.host" "block-sigint" (lambda () (jolt-set-sigint-blocked #t)))
(def-var! "jolt.host" "park-until-interrupt" jolt-park-until-interrupt)
(def-var! "jolt.host" "interrupt-on-sigint!" jolt-interrupt-on-sigint!)

;; reference types report their JVM classes ((class (agent 1)) is
;; clojure.lang.Agent). The IDeref/IRef/IPending/IFn/IBlockingDeref answers come
;; from the class graph's rows for these names (class-hierarchy.ss, and the two
;; reify rows above): instance? and protocol dispatch both read the class a
;; value reports and take its ancestry from there, so there is no second list
;; of "which values are derefable" to keep in step with the graph.
(register-class-arm! jolt-agent? (lambda (x) "clojure.lang.Agent"))
(register-class-arm! jolt-delay? (lambda (x) "clojure.lang.Delay"))
(register-class-arm! (lambda (x) (jvol? x)) (lambda (x) "clojure.lang.Volatile"))
(register-class-arm! (lambda (x) (var-cell? x)) (lambda (x) "clojure.lang.Var"))
;; ...and instance? memoizes these four by kind (records-interop.ss ref-kind-key).
;; The future probe is a bare record: making one through future-call starts a thread.
(set-ref-kind-extra-hook!
  (lambda (x)
    (cond ((jolt-delay? x) "clojure.lang.Delay")
          ((jolt-promise? x) "clojure.core$promise$reify__0")
          ((jolt-future? x) "clojure.core$future_call$reify__0")
          ((jolt-agent? x) "clojure.lang.Agent")
          (else #f)))
  (lambda ()
    (list (jolt-make-delay (lambda () jolt-nil))
          (jolt-promise-new)
          (make-jolt-future #f #f #f jolt-nil (make-mutex) (jolt-make-cv) #f)
          (jolt-agent-new jolt-nil))))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "user-thread"))) (lambda (x) "java.lang.Thread"))
