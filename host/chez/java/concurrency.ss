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
;; The same deadline as an absolute epoch millisecond count, which is the form
;; jolt-cv-wait takes: a thread's condition-wait wants a Chez time object, but a
;; fiber's deadline has to be comparable against the clock, and against the entries
;; in the shared timer. One conversion, so the two cannot drift apart.
(define (ms->deadline-millis ms) (+ (now-millis) (exact (floor ms))))

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

;; JVM conversions TRUNCATE toward zero (SECONDS.toMinutes(90) is 1).
(define (tu-convert self n per)
  (quotient (* (jnum->exact n) (time-unit-scale self)) per))

;; (amount, unit) -> milliseconds. A missing or non-TimeUnit unit leaves the amount
;; alone, which is the plain-milliseconds shape awaitTermination has always taken.
(define (tu->ms amount unit)
  (let ((n (if (number? amount) (jnum->exact amount) 0)))
    (if (time-unit? unit) (quotient (* n (time-unit-scale unit)) 1000000) n)))

;; The timeout of a (… timeout unit) argument tail, in ms, or #f when absent — the
;; untimed overload of the same method.
(define (tu-args->ms args)
  (if (null? args) #f (tu->ms (car args) (if (null? (cdr args)) #f (cadr args)))))

(define time-unit-constants
  (map (lambda (p) (cons (car p) (make-time-unit (car p) (cdr p)))) time-unit-nanos))
(register-class-statics! "TimeUnit" time-unit-constants)
(register-class-statics! "java.util.concurrent.TimeUnit" time-unit-constants)
(register-host-methods! "time-unit"
  (list (cons "toNanos"   (lambda (self n) (tu-convert self n 1)))
        (cons "toMicros"  (lambda (self n) (tu-convert self n 1000)))
        (cons "toMillis"  (lambda (self n) (tu-convert self n 1000000)))
        (cons "toSeconds" (lambda (self n) (tu-convert self n 1000000000)))
        (cons "toMinutes" (lambda (self n) (tu-convert self n 60000000000)))
        (cons "toHours"   (lambda (self n) (tu-convert self n 3600000000000)))
        (cons "toDays"    (lambda (self n) (tu-convert self n 86400000000000)))
        ;; the same interruptible sleep Thread/sleep takes (java/host-static-methods.ss)
        (cons "sleep"     (lambda (self n) (jolt-thread-sleep-ms (tu->ms n self))))
        (cons "name"      (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "toString"  (lambda (self) (vector-ref (jhost-state self) 0)))))
;; ...and the same for (str TimeUnit/SECONDS): an enum constant's toString is its
;; name on the JVM, where the generic host-object fallback prints #object[…].
(register-str-render! time-unit? (lambda (u) (vector-ref (jhost-state u) 0)))

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

;; (future-call thunk): spawn a thread running (thunk). The dynamic bindings in
;; effect now are conveyed into the worker (Chez inherits thread-parameters at
;; fork; we also install an explicit snapshot for certainty). The result — value
;; or thrown condition — is latched and broadcast; a cancel that already finalized
;; the future makes the late result a no-op.
(define (jolt-future-call thunk)
  (let* ((ibox (box #f))
         (f (make-jolt-future #f #f #f jolt-nil (make-mutex) (make-condition) ibox))
         (snap (dyn-binding-stack)))
    (fork-thread
     (lambda ()
       (*txn* #f)                          ; child thread must not inherit parent's txn
       ;; The worker's flag is the future's flag: future-cancel sets it, and
       ;; (Thread/currentThread) inside the body hands back a handle onto the same
       ;; box, so .isInterrupted / Thread/interrupted inside the worker and the
       ;; cancel from outside are ONE flag (the shape jolt#727 fixed for Thread.).
       (adopt-interrupt-box! ibox)
       (dyn-binding-stack snap)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (jolt-with-mutex (jolt-future-mu f)
           (unless (jolt-future-done? f)            ; not already cancelled
             (jolt-future-ok?-set! f (car r))
             (jolt-future-payload-set! f (cdr r))
             (jolt-future-done?-set! f #t))
           (jolt-cv-wake! (jolt-future-cv f))))))
    f))

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
(define (jolt-future-deref f)
  (jolt-cv-wait-interruptibly "future deref" (jolt-future-mu f) (jolt-future-cv f) #f
    (lambda (_timed-out?)
      (if (jolt-future-done? f) #t jolt-cv-again)))
  (jolt-future-finish f))

;; (deref f timeout-ms timeout-val): wait up to timeout-ms; return timeout-val if
;; it has not settled by the absolute deadline.
(define (jolt-future-deref-timed f ms timeout-val)
  (let ((settled (jolt-cv-wait-interruptibly "future deref"
                               (jolt-future-mu f) (jolt-future-cv f)
                               (ms->deadline-millis ms)
                   (lambda (timed-out?)
                     ;; done? FIRST on both paths: a future that settled as the
                     ;; deadline passed is not a timeout, which is the re-check the
                     ;; old (else (jolt-future-done? f)) arm was there for.
                     (cond ((jolt-future-done? f) #t)
                           (timed-out? #f)
                           (else jolt-cv-again))))))
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
(define (jolt-future-cancel f)
  (let ((cancelled (jolt-with-mutex (jolt-future-mu f)
                     (if (jolt-future-done? f)
                         #f
                         (begin (jolt-future-cancelled?-set! f #t)
                                (jolt-future-done?-set! f #t)
                                (jolt-cv-wake! (jolt-future-cv f))
                                #t)))))
    (when cancelled
      (let ((b (jolt-future-ibox f)))
        (set-box! b #t)
        (jolt-interrupt-wake-waits! b)))
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

;; (class a-future)/(class a-promise): both are anonymous reify instances of a
;; clojure.core fn on the JVM — clojure.core$future_call$reify__N /
;; clojure.core$promise$reify__N. The __N counter is unstable per eval; jolt
;; matches the stable enclosing-fn prefix and pins __0 (rather than :object).
(register-class-arm! jolt-future? (lambda (f) "clojure.core$future_call$reify__0"))
(register-class-arm! jolt-promise? (lambda (p) "clojure.core$promise$reify__0"))

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

(define (jolt-promise-deref p)
  (jolt-cv-wait-interruptibly "promise deref" (jolt-promise-mu p) (jolt-promise-cv p) #f
    (lambda (_timed-out?)
      (if (jolt-promise-delivered? p) #t jolt-cv-again)))
  (jolt-promise-value p))

(define (jolt-promise-deref-timed p ms timeout-val)
  (let ((got (jolt-cv-wait-interruptibly "promise deref"
                           (jolt-promise-mu p) (jolt-promise-cv p)
                           (ms->deadline-millis ms)
               (lambda (timed-out?)
                 (cond ((jolt-promise-delivered? p) #t)
                       (timed-out? #f)
                       (else jolt-cv-again))))))
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
(define (jolt-shutdown-agents) (set-box! agents-shutdown? #t) jolt-nil)

;; Thread-local list of (agent f . args) sent from within the action currently
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
                                  (make-mutex) (make-condition) em handler)))
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
;; and by restart resuming a held queue.
(define (jolt-agent-enqueue! a f args)
  (jolt-with-mutex (jolt-agent-mu a)
    (jagent-q-push! a (cons f args))
    (unless (jolt-agent-running? a)
      (jolt-agent-running?-set! a #t)
      (fork-thread (lambda () (*txn* #f) (jolt-agent-worker a)))))
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
          (for-each (lambda (e) (jolt-agent-enqueue! (car e) (cadr e) (cddr e)))
                    (reverse sends))
          (length sends)))))

;; Drain the queue, applying each action (f state arg*) outside the lock (an
;; action may send/deref the same agent). A successful action flushes any nested
;; sends it accumulated; a thrown action invokes the error-handler (if any, its
;; throws swallowed) and then either continues (:continue, state left untouched)
;; or fails the agent (:fail, error stored, queue halted).
(define (jolt-agent-worker a)
  (*txn* #f)                          ; agent worker must not inherit parent's txn
  (let loop ()
    (let ((act (jolt-with-mutex (jolt-agent-mu a)
                 (if (or (not (jolt-nil? (jolt-agent-err a))) (jagent-q-empty? a))
                     (begin (jolt-agent-running?-set! a #f)
                            (jolt-cv-wake! (jolt-agent-cv a)) #f)
                     (jagent-q-pop! a)))))
      (when act
        (parameterize ((*agent-nested* (box '())))
          (let ((err #f))
            (guard (e (#t (set! err (jolt-unwrap-throw e))))
              (let* ((old (jolt-agent-state a))
                     (nv (with-agent-binding a
                           (lambda () (apply jolt-invoke (car act) old (cdr act))))))
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
        (loop)))))

;; send / send-off: enqueue the action, start the worker if idle. (jolt treats
;; them identically — one serialized worker per agent — observably a superset of
;; the JVM fixed/cached pool split.) A send after shutdown-agents, to a failed
;; agent, inside a transaction, or from within an action is handled specially.
(define (jolt-agent-send a f . args)
  (cond
    ((jolt-agents-shutdown?)
     (jolt-throw (jolt-host-throwable
                  "java.util.concurrent.RejectedExecutionException"
                  "Agent pool has been shut down")))
    ((not (jolt-nil? (jolt-agent-err a)))
     (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                      "Agent is failed, needs restart")))
    ((*txn*)
     (let ((txn (*txn*)))
       (jolt-txn-pending-sends-set! txn
         (cons (apply list a f args) (jolt-txn-pending-sends txn)))))
    ((jolt-in-agent-action?)
     (let ((nested (*agent-nested*)))
       (set-box! nested (cons (cons* a f args) (unbox nested)))))
    (else (jolt-agent-enqueue! a f args)))
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
(define (jolt-agent-await . agents)
  (jolt-agent-await-check)
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
(define (jolt-agent-set-error-mode! a k)
  (jolt-agent-err-mode-set! a (kw->mode k)) a)
(define (jolt-agent-get-error-handler a) (jolt-agent-err-handler a))
(define (jolt-agent-set-error-handler! a f) (jolt-agent-err-handler-set! a f) a)
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
                 (fork-thread (lambda () (*txn* #f) (jolt-agent-worker a)))))))))
  a)

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
    (fork-thread
     (lambda ()
       (*txn* #f)
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
      ;; ONE jhost? test and then the tag, rather than an arm per shim: every deref
      ;; of an ATOM falls past this whole cond to %pre-conc-deref, so what the
      ;; common case pays for these two shims is a single record-type predicate.
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
            (find-method-any-protocol-arity (jrec-tag x) "deref"
                                            (if (null? opts) 1 3)))
       ;; the arity lookup falls back to any same-name method, so verify the
       ;; chosen impl really accepts this call's arity before invoking.
       => (lambda (m)
            (if (and (procedure? m)
                     (not (bitwise-bit-set? (procedure-arity-mask m)
                                            (+ 1 (length opts)))))
                (jolt-throw (deref-cast-error x opts))
                (apply jolt-invoke m x opts))))
      ((and (reified-methods x) (hashtable-ref (reified-methods x) "deref" #f))
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
(def-var! "clojure.core" "future-cancel" jolt-future-cancel)
(def-var! "clojure.core" "future?" jolt-future?)
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
(def-var! "clojure.core" "send-off" jolt-agent-send)
;; send-via takes an executor jolt has no model for; behave as send and ignore it.
(def-var! "clojure.core" "send-via"
  (lambda (_exec a f . args) (apply jolt-agent-send a f args)))
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
;; #(bk owner count cv fibers box)
;;   bk     the bookkeeping mutex — held across a decision, never across a body
;;   owner  the FIBER when a fiber holds it, else the thread's interrupt box
;;          (current-interrupt-box, an identity that is safe under
;;          fork-inheritance), or #f when free
;;   count  reentrancy depth for the owner
;;   cv     thread waiters; condition-wait releases bk atomically with blocking
;;   fibers parked fiber waiters, resumed by the release
;;   box    the interrupt box of the thread the owner took it on — the THREAD
;;          identity, which is what monitor-owner? needs when the owner is a fiber
(define monitor-i-bk 0)
(define monitor-i-owner 1)
(define monitor-i-count 2)
(define monitor-i-cv 3)
(define monitor-i-fibers 4)
(define monitor-i-box 5)
;; A fresh monitor. Named rather than inlined at the one use site, because
;; object-monitor is no longer the only thing that needs one:
;; java.util.concurrent.locks.ReentrantLock is a lock in its own right — (locking
;; lk …) and (.lock lk) are DIFFERENT locks on the JVM and stay different here,
;; so it gets its own record rather than the object's — but the hard part it needs
;; is exactly this (jolt-ga8o). One implementation, two locks.
(define (make-monitor) (vector (make-mutex) #f 0 (make-condition) '() #f))

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
  (let ((owner (vector-ref m monitor-i-owner)))
    (or (eq? owner me)
        (and (jolt-fiber? owner)
             (not (jolt-current-fiber))
             (eq? (vector-ref m monitor-i-box) (current-interrupt-box))
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
          (jolt-fiber-state-set! f 'parked)
          jolt-lock-parked)
        (begin
          (jolt-condition-wait (vector-ref m monitor-i-cv) (vector-ref m monitor-i-bk))
          #f))))

;; The decision is made under bk; a fiber's SWITCH is made outside it, and then the
;; whole decision is retaken, because a resume says only that something changed. All
;; three of those are jolt-lock-wait (host/chez/locks.ss), which is this protocol
;; named once rather than open-coded at each of the sites that needs it.
(define (monitor-enter! m)
  (let ((me (monitor-self)))
    (jolt-lock-wait (vector-ref m monitor-i-bk)
      (lambda ()
        (let loop ()
          (let ((owner (vector-ref m monitor-i-owner)))
            (cond
              ((eq? owner me)
               (vector-set! m monitor-i-count (fx+ 1 (vector-ref m monitor-i-count)))
               #f)
              ((not owner)
               (vector-set! m monitor-i-owner me)
               (vector-set! m monitor-i-box (current-interrupt-box))
               (vector-set! m monitor-i-count 1)
               #f)
              ;; a thread's wait ends under this same bk, so it loops HERE; a fiber
              ;; answers jolt-lock-parked and jolt-lock-wait retakes this decision
              ;; from the top once something has resumed it.
              (else (or (monitor-wait! m) (loop))))))))
    (void)))

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
  (let ((me (monitor-self)))
    (jolt-with-mutex (vector-ref m monitor-i-bk)
      (let ((owner (vector-ref m monitor-i-owner)))
        (cond
          ((eq? owner me)
           (vector-set! m monitor-i-count (fx+ 1 (vector-ref m monitor-i-count)))
           #t)
          ((not owner)
           (vector-set! m monitor-i-owner me)
           (vector-set! m monitor-i-box (current-interrupt-box))
           (vector-set! m monitor-i-count 1)
           #t)
          (else #f))))))

;; The three questions ReentrantLock exposes. All strict eq? against monitor-self,
;; NOT monitor-owner?: that predicate's second arm exists so a TERMINAL fiber's own
;; unwind can release what it was holding, and answering a query #t on that basis
;; would tell a live thread it holds a lock a dead fiber took. Release is lenient
;; because the alternative is leaking the monitor; a query has no such excuse.
(define (monitor-held-by-self? m)
  (jolt-with-mutex (vector-ref m monitor-i-bk)
    (eq? (vector-ref m monitor-i-owner) (monitor-self))))
;; getHoldCount is "holds by the CURRENT thread" on the JVM — 0 for anyone else,
;; not the raw depth.
(define (monitor-self-count m)
  (jolt-with-mutex (vector-ref m monitor-i-bk)
    (if (eq? (vector-ref m monitor-i-owner) (monitor-self))
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
(define (monitor-poll-round!)
  (let ((f (jolt-current-fiber)))
    (cond
      ((not f) (sleep lock-poll-nap))
      (else
       (sa-fiber-yield)
       (unless (jolt-carrier-head (jolt-fiber-carrier f))
         (sleep lock-poll-nap))))))

;; The waiters are taken and cleared under bk so no resume is delivered twice, and
;; they are woken OUTSIDE it: sa-fiber-resume takes the carrier's run-queue mutex,
;; and keeping the two apart means this path closes no cycle at all rather than
;; relying on that mutex being last in the order. Broadcast and not signal, for
;; loader.ss's reason: the waiters re-check a condition that will be true for exactly
;; one of them.
(define (monitor-exit! m)
  (let ((me (monitor-self)))
    (let ((wake
           (jolt-with-mutex (vector-ref m monitor-i-bk)
             (unless (monitor-owner? m me)
               (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException"
                                                "not lock owner")))
             (vector-set! m monitor-i-count (fx- (vector-ref m monitor-i-count) 1))
             (if (fx=? 0 (vector-ref m monitor-i-count))
                 (let ((fs (vector-ref m monitor-i-fibers)))
                   (vector-set! m monitor-i-owner #f)
                   (vector-set! m monitor-i-box #f)
                   (vector-set! m monitor-i-fibers '())
                   (condition-broadcast (vector-ref m monitor-i-cv))
                   fs)
                 '()))))
      (for-each sa-fiber-resume wake))))

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
(define (jolt-with-monitor obj thunk)
  (let ((m (object-monitor obj)))
    (monitor-enter! m)
    (dynamic-wind
      (lambda () #f)
      thunk
      (lambda () (unless (jolt-park-unwinding?) (monitor-exit! m))))))
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
;; This BORROWS the timer, and on a fiber the timer belongs to the scheduler: it
;; is what preempts a compute-bound fiber so the ones queued behind it on the same
;; carrier are not starved. So both halves have to go back, the handler and the
;; tick. It used to end with a bare (set-timer 0), which returns the handler and
;; keeps the tick, and only the fiber's NEXT dispatch arms again — so a fiber that
;; borrowed the timer once and then never parked pinned its carrier (jolt-ly62).
;; jolt-fiber-rearm-preempt! puts it back, and does nothing off a fiber, where
;; there was never anything armed to restore.
;;
;; It lives in fibers.ss, which rt.ss loads AFTER this file; the reference resolves
;; at call time, the same forward reference async.ss makes to jolt-fiber-go-spawn
;; and for the same reason. No fiber can exist before the boot finishes loading.
;;
;; WHY THE BORROW IS A dynamic-wind AND NOT THREE HAND-WRITTEN EXITS (jolt-1rod).
;;
;; It was three: a normal return, a guard for a raise, and the call/cc for its own
;; interrupt escape. A fiber PARK is a fourth way out and it was none of them — it
;; escapes through the carrier's sched-k, and timer-interrupt-handler is a setter
;; write on a per-thread parameter, so a continuation escape does not undo it.
;; jolt-fiber-install-preempt-handler! runs once per DRAIN rather than per
;; dispatch, so the borrower's handler then stayed live over every later fiber on
;; that carrier, and it failed two ways at once:
;;
;;   - that handler answers a quantum by re-arming its own tick and returning, so
;;     nothing on the carrier was preempted again. Measured at a 20,000 tick
;;     quantum: 0 preemptions in 0.5s behind a fiber parked inside a borrow. That
;;     is the unbounded starvation window the head of fibers.ss says no setting can
;;     open, reachable from ordinary jolt code.
;;   - the handler closes over the call/cc continuation captured in the PARKED
;;     fiber's stack. A later interrupt! fired it under whichever fiber owned the
;;     carrier and invoked that continuation: the parked fiber returned from a park
;;     it never came back from, and the running fiber was abandoned mid-computation
;;     — 'running for the life of the process, its go channel never closed, every
;;     reader of it waiting, and no error naming any of it.
;;
;; A wind is the answer the rest of the runtime already gives for state that must
;; survive a park: the after-thunk hands the timer back on the way out and the
;; before-thunk re-takes it on the way in, so the borrow is SUSPENDED across a park
;; rather than leaking past it. It is jolt-with-mutex's rule and the opposite of
;; jolt-with-monitor's, and the difference is what each thing is: a monitor must
;; still be held when the fiber comes back, the carrier's timer must not.
;;
;; The other three exits come out of the same after-thunk, so they are no longer
;; written separately. And an interrupt raised while the borrower is parked is not
;; lost — the resume re-arms the borrowed tick, so it lands on the fiber that asked
;; for it, on its own carrier time.
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
;; Active timer borrows form a dynamic stack per application thread. Chez child
;; threads inherit thread-parameter values, so owner-tag it: an inherited stack
;; is empty in the child and cannot make the child poll a parent's token.
(define interrupt-poll-stack (make-thread-parameter (cons #f '())))
(define (current-interrupt-poll-stack)
  (let ((state (interrupt-poll-stack)) (owner (get-thread-id)))
    (if (and (pair? state) (eqv? (car state) owner)) (cdr state) '())))
(define (jolt-run-interruptible token thunk)
  ;; Captured ONCE, outside the wind: a before-thunk that re-read it would, on a
  ;; resume, save the handler the SCHEDULER had put back and restore that on the
  ;; real exit — the borrow would hand the carrier its own handler a second time
  ;; and lose whatever an enclosing borrow had installed.
  (let* ((prev-handler (timer-interrupt-handler))
         ;; Captured once, like prev-handler. On a real off-fiber exit from a
         ;; nested borrow the restored outer handler needs a fresh poll tick.
         (owner (get-thread-id))
         (outer-stack (current-interrupt-poll-stack))
         ;; --- handing the quantum back DURING the borrow (jolt-449m) -----------
         ;; jolt-ly62 stopped the borrow outliving itself. What was still open is
         ;; the extent of the borrow: the handler below answered a quantum by
         ;; re-arming its own tick and RETURNING, so nothing on the carrier was
         ;; preempted for as long as the body ran. Measured, one carrier and a
         ;; 20,000 tick quantum: a spinning fiber inside a borrow managed
         ;; 36,983,637 iterations and 0 preemptions while a sibling queued behind
         ;; it never ran; the same spin outside a borrow was preempted 46 times.
         ;; That is the unbounded starvation window the head of fibers.ss says no
         ;; setting can open, and the body reached through jolt.host/run-interruptible
         ;; is arbitrary user code — a whole eval, in the interrupt use it exists for.
         ;;
         ;; The borrow polls far more often than the quantum, so the fix is to
         ;; COUNT what it consumes and let the scheduler decide once it adds up.
         ;; `borrowed` is ticks since the fiber last had the CPU handed to it; the
         ;; before-thunk zeroes it, because a rewind means the fiber has just been
         ;; dispatched and its quantum restarts.
         (borrowed 0)
         ;; What the borrow last armed, so the accounting adds real ticks. It arms
         ;; the TIGHTER of the two demands, so a quantum shorter than the poll
         ;; interval is still honoured instead of being rounded up to it.
         (armed interrupt-check-ticks)
         (arm!
          (lambda ()
            (let ((n (if (jolt-current-fiber)
                         (fxmin interrupt-check-ticks (jolt-fiber-preempt-ticks))
                         interrupt-check-ticks)))
              (set! armed n)
              (set-timer n))))
         (r (call/cc
              (lambda (k)
                (dynamic-wind
                  (lambda ()
                    (set! borrowed 0)
                    (let ((handler
                           (lambda ()
                        (cond
                          ((and (box? token) (unbox token)) (k interrupt-sentinel))
                          (else
                           (set! borrowed (fx+ borrowed armed))
                           (cond
                             ;; The quantum has fallen due. Hand it to the
                             ;; scheduler's handler, which either escapes through
                             ;; sched-k — and the after-thunk below gives the timer
                             ;; back, the before-thunk re-takes it on resume — or
                             ;; refuses (a lock held, a transition committed) and
                             ;; re-arms short to retry. When it refuses, `armed`
                             ;; still names the old tick, so the next count is high
                             ;; and the retry lands early. That is the only safe
                             ;; direction: the guarantee is an upper bound on the
                             ;; window, and early only tightens it.
                             ;;
                             ;; The jolt-current-fiber test is load-bearing, not a
                             ;; nicety: off a fiber the scheduler's handler does
                             ;; nothing AND does not re-arm, so calling it there
                             ;; would leave the borrow with no timer and stop the
                             ;; interrupt from ever being noticed.
                              ((and (jolt-current-fiber)
                                    (fx>=? borrowed (jolt-fiber-preempt-ticks)))
                               (set! borrowed 0)
                               (jolt-fiber-preempt-handler))
                              (else (arm!) (void))))))))
                      ;; Push is the timer-frame ownership linearization point.
                      (interrupt-poll-stack (cons owner (cons handler outer-stack)))
                      (timer-interrupt-handler handler)
                      (arm!)))
                  (lambda () (thunk))
                  ;; Runs on every way out of the body, the park included. Disarm
                  ;; BEFORE restoring the handler: the other order leaves a window
                  ;; where the borrowed tick can fall due on the scheduler's
                  ;; handler, which answers it by preempting.
                  (lambda ()
                    (set-timer 0)
                    ;; Pop before restoring the enclosing handler. Every terminal
                    ;; path (return, throw, interrupt, and park unwind) comes here.
                    (interrupt-poll-stack (cons owner outer-stack))
                    (timer-interrupt-handler prev-handler)
                    ;; Off a fiber, and on a park (the current-fiber vreg is
                    ;; already cleared by then), this is a plain disarm — which is
                    ;; what the scheduler wants for the carrier it is about to take
                    ;; back. On a real exit from a fiber it re-arms the quantum.
                    ;; On a park, enclosing winds immediately disarm this tick;
                    ;; on a real off-fiber nested exit it keeps the outer token
                    ;; polling. Fiber exits retain the scheduler rearm behavior.
                    (cond
                      ((jolt-current-fiber) (jolt-fiber-rearm-preempt!))
                      ((pair? outer-stack) (set-timer interrupt-check-ticks))
                      (else (jolt-fiber-rearm-preempt!)))))))))
    (if (eq? r interrupt-sentinel)
        (jolt-throw (jolt-ex-info "Evaluation interrupted" (jolt-hash-map jolt-kw-interrupted #t)))
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
(define (make-jthread thunk name)
  (make-jhost "user-thread"
              (vector thunk #f (make-mutex) (make-condition) (box #f) #f
                      (box (or name (next-jthread-name))) #f)))
;; slot 7: the id of the thread the start forked, #f until then. A rename needs
;; it to reach the id-keyed name table the handles read.
(define (jthread-id st) (vector-ref st 7))
;; alive = started and not yet completed. join waits on exactly this, so a thread
;; that was never started is not waited for at all (JVM: isAlive is false before
;; .start, so join returns at once).
(define (jthread-alive? st) (and (vector-ref st 5) (not (vector-ref st 1))))
;; JVM Thread() is legal: the target is null and run/start are no-ops.
;; Thread(), Thread(runnable) and Thread(runnable, name) — the name argument used
;; to be accepted and dropped, so a thread the caller had named answered with a
;; generated one.
(for-each (lambda (nm)
            (register-class-ctor! nm
              (lambda args
                (make-jthread (if (null? args) #f (car args))
                              (if (or (null? args) (null? (cdr args)) (jolt-nil? (cadr args)))
                                  #f
                                  (jolt-final-str (cadr args)))))))
          '("Thread" "java.lang.Thread"))
(register-host-methods! "user-thread"
  (list (cons "start" (lambda (self)
          (let ((st (jhost-state self)) (snap (dyn-binding-stack)))
            (vector-set! st 5 #t)  ; mark started before forking
            (fork-thread (lambda ()
               (*txn* #f)                          ; child thread must not inherit parent's txn
               ;; Adopt the Thread object's own interrupt flag, so .interrupt from
               ;; outside and this thread's Thread/currentThread view are ONE flag.
               (adopt-interrupt-box! (vector-ref st 4))
               ;; publish the name under this thread's id, so .getName agrees
               ;; whether it is asked of the Thread object or of the handle
               ;; Thread/currentThread (and getAllStackTraces) hands out
               (vector-set! st 7 (get-thread-id))
               (jolt-thread-name-set! (get-thread-id) (unbox (vector-ref st 6)))
               ;; and register that handle now, so a handle someone else takes for
               ;; this thread before it first asks who it is is the live one
               (current-thread-handle)
               (dyn-binding-stack snap)
               ;; surface a thread body's throw like the JVM's default uncaught-
              ;; exception handler; the thread still completes (isAlive/join
              ;; semantics unchanged). Reporting failures are swallowed.
              (guard (e (#t (guard (_ (#t #f))
                              (display "Exception in Thread body:\n" (current-error-port))
                              (jolt-report-throwable e (current-error-port)))))
                ;; runnable->thunk and not a bare jolt-invoke: Thread(Runnable)
                ;; accepts any Runnable on the JVM, and a FutureTask is one (it is
                ;; registered as such in the class graph). Invoking it directly
                ;; hung — a FutureTask is a shim value, not a procedure.
                (let ((th (vector-ref st 0))) (when th (jolt-invoke (runnable->thunk th)))))
              (jolt-with-mutex (vector-ref st 2)
                 (vector-set! st 1 #t)
                 (jolt-cv-wake! (vector-ref st 3)))))
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
                          (and ms (not (= ms 0)) (ms->deadline-millis ms))
              (lambda (timed-out?)
                (cond ((not (jthread-alive? st)) #t)
                      (timed-out? #f)
                      (else jolt-cv-again)))))
          jolt-nil))
        (cons "isAlive" (lambda (self) (jthread-alive? (jhost-state self))))
        ;; flag then poke, so a wait already parked on a condition this thread
        ;; registered is thrown out of it rather than merely told afterwards
        (cons "interrupt" (lambda (self . _)
          (let ((b (vector-ref (jhost-state self) 4)))
            (set-box! b #t)
            (jolt-interrupt-wake-waits! b))
          jolt-nil))
        (cons "isInterrupted" (lambda (self) (and (unbox (vector-ref (jhost-state self) 4)) #t)))
        (cons "getName" (lambda (self) (unbox (vector-ref (jhost-state self) 6))))
        ;; A rename after .start has to reach the running thread too, or the two
        ;; spellings of the same thread's name disagree.
        (cons "setName" (lambda (self nm)
          (let* ((st (jhost-state self)) (s (jolt-final-str nm)))
            (set-box! (vector-ref st 6) s)
            (when (jthread-id st) (jolt-thread-name-set! (jthread-id st) s)))
          jolt-nil))
        (cons "setDaemon" (lambda (self . _) jolt-nil))))

(define (make-jlatch n) (make-jhost "count-down-latch" (vector n (make-mutex) (make-condition))))
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
                                         (and ms (ms->deadline-millis ms))
                             (lambda (timed-out?)
                               (cond ((<= (vector-ref st 0) 0) #t)
                                     (timed-out? #f)
                                     (else jolt-cv-again))))))
              ;; await() is void; await(timeout, unit) answers whether it reached zero
              (if ms reached jolt-nil)))))
         (cons "getCount" (lambda (self) (vector-ref (jhost-state self) 0)))))

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
;; j-future state: #(done? result error mutex condition)
(define (make-j-future) (make-jhost "j-future" (vector #f jolt-nil #f (make-mutex) (make-condition))))
(define (j-future-complete! self thunk)
  (let ((st (jhost-state self)))
    (let ((r (guard (e (#t (vector-set! st 2 e) #f)) (jolt-invoke thunk))))
      (jolt-with-mutex (vector-ref st 3)
        (unless (vector-ref st 2) (vector-set! st 1 r))
        (vector-set! st 0 #t)
        (jolt-cv-wake! (vector-ref st 4))))))
(define (j-future? x) (and (jhost? x) (string=? (jhost-tag x) "j-future")))
;; get() waits for the task; get(timeout, unit) gives up at the deadline and throws
;; TimeoutException, like the JVM. The timeout used to be discarded, so the bounded
;; overload waited forever on a task that never finished.
;;
;; ON-TIMEOUT is what a missed deadline produces. Future.get throws there;
;; `deref` with a timeout value returns it instead (Clojure's deref-future
;; swallows exactly TimeoutException), so both spellings share one wait rather
;; than one of them catching what the other just threw.
(define (j-future-get* self ms on-timeout)
  (let* ((st (jhost-state self))
         (done (jolt-cv-wait-interruptibly "Future.get"
                             (vector-ref st 3) (vector-ref st 4)
                             (and ms (ms->deadline-millis ms))
                 (lambda (timed-out?)
                   (cond ((vector-ref st 0) #t)
                         (timed-out? #f)
                         (else jolt-cv-again))))))
    (cond ((not done) (on-timeout))
          ((vector-ref st 2) (jolt-throw (task-execution-exception (vector-ref st 2))))
          (else (vector-ref st 1)))))
(define (future-timeout-throw)
  (jolt-throw (jolt-host-throwable "java.util.concurrent.TimeoutException"
                                   "timed out waiting for the task")))
(define (j-future-get self . args)
  (j-future-get* self (tu-args->ms args) future-timeout-throw))
(register-host-methods! "j-future"
  (list (cons "get" j-future-get)
        (cons "isDone" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "isCancelled" (lambda (self) #f))
        (cons "cancel" (lambda (self . _) #f))))
;; executor-service state: #(shutdown? queue-box queue-mutex task-cond
;; live-workers advisory-queue-capacity core-workers max-workers keep-alive-ms
;; idle-workers queue-depth term-cond worker-interrupt-boxes) — the capacity is #f
;; except for a ThreadPoolExecutor built with an ArrayBlockingQueue; .getQueue's
;; view subtracts the live depth from it. The last slot is one interrupt flag per
;; live worker, which is what lets shutdownNow reach a task that is already
;; running (see executor-shutdown-now!).
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
(define (make-executor* core-n max-n keep-alive-ms cap)
  (let ((self (make-jhost "executor-service"
                          (vector #f (box (cons '() '())) (make-mutex) (make-condition) 0
                                  cap core-n max-n keep-alive-ms 0 0 (make-condition) '()))))
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
;; A fixed pool: n eager workers that never retire, and the advisory capacity a
;; ThreadPoolExecutor's queue argument contributes to .getQueue's view.
(define (make-executor n-workers . cap)
  (make-executor* n-workers n-workers #f (if (null? cap) #f (car cap))))
;; newCachedThreadPool / newVirtualThreadPerTaskExecutor.
(define (make-cached-executor)
  (make-executor* 0 executor-unbounded-workers cached-pool-keep-alive-ms #f))

;; Claim a worker slot, or answer #f because the pool is at max. Called with the
;; queue mutex HELD, and the slot is claimed BEFORE the fork rather than counted
;; by the new thread on arrival: live-workers is what isTerminated and
;; awaitTermination read, so a count that dipped between the decision and the
;; thread's first instruction would report a pool terminated while a task it
;; accepted was still on its way to a worker. It is also what keeps two
;; concurrent enqueues from both spawning past max.
(define (executor-claim-worker! st)
  (and (fx<? (vector-ref st 4) (vector-ref st 7))
       (begin (vector-set! st 4 (fx+ (vector-ref st 4) 1)) #t)))
;; Fork the thread for a slot already claimed, OUTSIDE the mutex. A fork that
;; fails gives the slot back; whether that is the caller's problem depends on
;; whether anything is left to run the task — with other workers live it waits
;; for one of them, with none it would wait forever, and a submit whose task can
;; never run is a failure the caller has to see (the JVM's own answer to a thread
;; it cannot create is to reject the task, not to queue it silently).
(define (executor-spawn-worker! st)
  ;; The worker's interrupt flag, made and REGISTERED here rather than by the
  ;; thread itself: a shutdownNow landing between the claim and the new thread's
  ;; first instruction has to find this worker, and a flag the worker allocates on
  ;; arrival is not there to be found. It is the same box the worker adopts as its
  ;; own, so a task's (Thread/currentThread) .isInterrupted and the pool's shutdown
  ;; are ONE flag — the shape future-call and Thread.start already use.
  (let ((ibox (box #f)))
    (jolt-with-mutex (vector-ref st 2)
      (vector-set! st 12 (cons ibox (vector-ref st 12))))
    (guard (e (#t (let ((none-left? (jolt-with-mutex (vector-ref st 2)
                                      (vector-set! st 12 (remq ibox (vector-ref st 12)))
                                      (vector-set! st 4 (fx- (vector-ref st 4) 1))
                                      (jolt-cv-wake! (vector-ref st 11))
                                      (fx=? 0 (vector-ref st 4)))))
                    (when none-left? (raise e)))))
      (fork-thread (lambda ()
        (*txn* #f)      ; worker must not inherit the creating thread's txn
        (adopt-interrupt-box! ibox)
        (executor-worker-loop st ibox))))))

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
;; Park until something changes: a task arrives, the pool shuts down, or (for a
;; worker above core) the keep-alive deadline passes. The idle count is raised
;; before the wait releases the mutex and dropped after it retakes it, so the
;; growth test in executor-enqueue! sees exactly the workers that are available
;; to take a task.
(define (executor-idle-wait! st abs-time)
  (vector-set! st 9 (fx+ (vector-ref st 9) 1))
  (if abs-time
      (jolt-condition-wait (vector-ref st 3) (vector-ref st 2) abs-time)
      (jolt-condition-wait (vector-ref st 3) (vector-ref st 2)))
  (vector-set! st 9 (fx- (vector-ref st 9) 1)))
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
(define (executor-worker-exit! st ibox)
  (vector-set! st 12 (remq ibox (vector-ref st 12)))
  (vector-set! st 4 (fx- (vector-ref st 4) 1))
  ;; Announced on term-cond only, and only to a waiter that can care: what
  ;; awaitTermination waits for is live 0 with the pool shut down, so a keep-alive
  ;; retirement in a RUNNING pool wakes nobody at all. (isTerminated polls.)
  (when (or (vector-ref st 0) (fx=? 0 (vector-ref st 4)))
    (jolt-cv-wake! (vector-ref st 11)))
  #f)
;; The next task for this worker, or #f meaning it has exited — because the pool
;; is shut down and drained, or because this worker is above core and has been
;; idle for keep-alive. Runs with the mutex held, and only ever exits with the
;; queue empty, so no task is left with nobody to run it.
;;
;; The keep-alive deadline is absolute and survives the loop, so a broadcast that
;; woke every idle worker for one task resumes the losers waiting for what is LEFT
;; of their keep-alive rather than restarting it — the same reason Thread.join and
;; the waits above carry deadlines rather than durations.
(define (executor-take-job! st ibox)
  (let poll ((deadline #f))
    (cond ((fx>? (vector-ref st 10) 0)
           ;; Taking a task CLEARS the flag, unless the pool is stopping: an
           ;; interrupt aimed at the last task is not for this one, and the JVM's
           ;; runWorker clears it at the same point and for the same reason.
           (set-box! ibox #f)
           (executor-dequeue! st))
          ((vector-ref st 0) (executor-worker-exit! st ibox))   ; shutdown + drained
          ((and (vector-ref st 8) (fx>? (vector-ref st 4) (vector-ref st 6)))
           (let ((dl (or deadline (+ (now-millis) (vector-ref st 8)))))
             (if (>= (now-millis) dl)
                 (executor-worker-exit! st ibox)               ; idle past keep-alive
                 (begin (executor-idle-wait! st (jolt-millis->time dl))
                        (poll dl)))))
          (else (executor-idle-wait! st #f) (poll deadline)))))
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
(define (executor-worker-loop st ibox)
  (guard (e (#t (jolt-with-mutex (vector-ref st 2) (executor-worker-exit! st ibox))
                (guard (_ (#t #f))
                  (display "Exception in executor worker:\n" (current-error-port))
                  (jolt-report-throwable e (current-error-port)))))
    (let loop ()
      (let ((job (jolt-with-mutex (vector-ref st 2) (executor-take-job! st ibox))))
        (when job (job) (loop))))))

;; shutdown: stop accepting, let what is queued drain.
(define (executor-shutdown! st)
  (vector-set! st 0 #t)
  (jolt-with-mutex (vector-ref st 2)
    (jolt-cv-wake! (vector-ref st 3))       ; every idle worker: leave
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
;; The other half of shutdownNow is INTERRUPTING the tasks already running, and it
;; does that too: every live worker carries an interrupt flag (executor-spawn-worker!
;; above), so a task parked in Thread/sleep, a channel op, a blocking queue or a
;; Future.get is thrown out of it with InterruptedException, exactly as the JVM
;; throws one there. The flag first and the poke second, which is the order every
;; interrupt in this runtime uses: the flag is what a woken waiter reads, and a wake
;; that arrives before it is set says nothing.
;;
;; A task in a computation the runtime cannot see — a tight loop, a blocking FFI
;; call — still runs to the end. The JVM's interrupt is no different: it unblocks
;; the waits it knows about and sets a flag for everything else to notice.
(define (executor-interrupt-workers! st)
  (let ((boxes (jolt-with-mutex (vector-ref st 2) (vector-ref st 12))))
    (for-each (lambda (b) (set-box! b #t)) boxes)
    ;; poked OUTSIDE the queue mutex: waking a wait takes the waiter's own mutex,
    ;; and a task blocked on something that wants this pool's mutex would deadlock
    ;; against a poke that held it.
    (for-each jolt-interrupt-wake-waits! boxes)))
(define (executor-drain-queue! st)
  (let* ((q (unbox (vector-ref st 1)))
         (jobs (append (car q) (reverse (cdr q)))))
    (set-car! q '())
    (set-cdr! q '())
    (vector-set! st 10 0)
    jobs))
(define (executor-shutdown-now! st)
  (vector-set! st 0 #t)
  (let ((dropped (jolt-with-mutex (vector-ref st 2)
                   (let ((jobs (executor-drain-queue! st)))
                     (jolt-cv-wake! (vector-ref st 3))
                     (jolt-cv-wake! (vector-ref st 11))
                     jobs))))
    (executor-interrupt-workers! st)
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
;; GROWING ON A HANDOFF THAT FAILED, which is the JVM's rule and not the one a
;; queue-depth test gives you on its own.
;;
;; A cached pool grows there when offering the task to a SynchronousQueue fails,
;; and it fails only when no worker is at the handoff point to take it. jolt has an
;; asynchronous queue, so the same test written directly — is the depth above the
;; idle count — answers yes in a case the JVM's never sees: a worker that is
;; running, or is queued behind the producer on this very mutex, is a worker about
;; to take the task, and it counts as idle in neither. A producer in a tight loop
;; wins that mutex race most of the time, so the depth outruns the idle count for
;; reasons that have nothing to do with the pool being short of workers, and the
;; pool grows threads which then contend for the same mutex and slow the workers
;; down further — the feedback the wake fix removed one source of, arriving by
;; another road.
;;
;; Measured, once the dispatch shortcut made the producer 2.4x faster and made it
;; worse: 200k no-op tasks peaked at 25-37 workers and cost 10.6us each, against 7
;; workers and 8.9us before the producer sped up. A single-worker pool ran the same
;; tasks at 1.8us. The pool was paying for threads that were in its way.
;;
;; So the decision is made the way the JVM makes it: OFFER FIRST. When the depth
;; test says grow, the enqueue drops the mutex, yields the CPU to whoever wants it,
;; and asks again. A worker that was one mutex acquisition away from this task now
;; has it, and there is nothing to grow for; a pool whose workers are all blocked
;; still has the task sitting there, and grows. The yield costs a syscall on the
;; path that was about to fork a thread for ~80us, and nothing at all on the path
;; that was not.
(define (executor-grow? st)
  (jolt-with-mutex (vector-ref st 2)
    (and (not (vector-ref st 0))
         (fx>? (vector-ref st 10) (vector-ref st 9))
         (executor-claim-worker! st))))
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
                      ;; No idle worker waiting to take this task — which is a
                      ;; reason to grow only if it is still true once the workers
                      ;; have had their turn at the mutex, and only if there is
                      ;; room to grow at all. A pool at its maximum (every fixed
                      ;; pool, always) must not pay the yield to be told what its
                      ;; own size already says: that check costs a fixed pool
                      ;; nothing and it was costing it 0.9us a task without it.
                      (if (and (fx<? (vector-ref st 4) (vector-ref st 7))
                               (fx>? (vector-ref st 10) (vector-ref st 9)))
                          'offer
                          #f))))))
    ;; All of these happen with the mutex RELEASED: a fork takes ~80us and the
    ;; queue cannot be shut for that long, a yield holding it would hand the CPU on
    ;; while keeping the workers out, and a throw has no business unwinding through
    ;; a lock region it does not need to hold.
    (case action
      ((offer) (thread-yield!)
               (when (executor-grow? st) (executor-spawn-worker! st)))
      ((reject) (executor-reject-task! st))
      (else (void)))))
(let ((single (lambda _ (make-executor 1)))
      (fixed  (lambda (n . _) (make-executor (max 1 (jnum->exact n)))))
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
      (cached (lambda _ (make-cached-executor)))
      ;; newWorkStealingPool is NOT one of those and stays a fixed pool: a
      ;; ForkJoinPool is sized at availableProcessors, because work stealing is for
      ;; CPU-bound tasks that would only contend if there were more of them than
      ;; cores. It does grow past that, but only to REPLACE a worker the JVM can
      ;; see is blocked in a join, which is a thing jolt cannot see; a flat 32 sits
      ;; between the two bounds and errs toward not stranding a blocking task.
      (stealing (lambda _ (make-executor 32))))
  (for-each (lambda (nm) (register-class-statics! nm
              (list (cons "newSingleThreadExecutor" single)
                    (cons "newSingleThreadScheduledExecutor" single)
                    (cons "newFixedThreadPool" fixed) (cons "newScheduledThreadPool" fixed)
                    (cons "newVirtualThreadPerTaskExecutor" cached)
                    (cons "newCachedThreadPool" cached) (cons "newWorkStealingPool" stealing))))
            '("Executors" "java.util.concurrent.Executors")))
(register-host-methods! "executor-service"
  (list (cons "submit" (lambda (self thunk)
          (let ((fut (make-j-future)) (snap (dyn-binding-stack)) (thunk (runnable->thunk thunk)))
            (executor-enqueue! self (lambda () (dyn-binding-stack snap) (j-future-complete! fut thunk)))
            fut)))
        (cons "execute" (lambda (self thunk*)
          (let ((thunk (runnable->thunk thunk*)))
          (let ((snap (dyn-binding-stack)))
            (executor-enqueue! self (lambda () (dyn-binding-stack snap)
              (guard (e (#t (guard (_ (#t #f))
                              (display "Exception in executor task:\n" (current-error-port))
                              (jolt-report-throwable e (current-error-port)))))
                (jolt-invoke thunk)))))
          jolt-nil)))
        ;; Shutdown wakes BOTH conditions, and every waiter on each: task-cond so
        ;; that all the idle workers see the flag and leave (the one place a
        ;; broadcast there is the point — the news is for all of them, not for
        ;; whichever one a signal would pick), term-cond because a pool with no
        ;; live worker is already terminated and awaitTermination has to re-read
        ;; the flag to find out.
        (cons "shutdown" (lambda (self) (executor-shutdown! (jhost-state self)) jolt-nil))
        (cons "shutdownNow" (lambda (self) (executor-shutdown-now! (jhost-state self))))
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
          (and (vector-ref st 0) (fx=? 0 (vector-ref st 10)) (fx=? 0 (vector-ref st 4))))))
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
            (+ (now-millis) (tu->ms ms (if (null? rest) #f (car rest)))))))))

;; --- ArrayBlockingQueue / FutureTask / ThreadPoolExecutor --------------------
;; The construction Grain's SQLite write coordinator does directly:
;; (ThreadPoolExecutor. 1 1 keepAlive unit (ArrayBlockingQueue. n)) plus
;; FutureTask. ArrayBlockingQueue is a real bounded blocking queue (fiber-aware
;; through jolt-cv-wait); FutureTask a run-once future; the ThreadPoolExecutor
;; ctor builds on make-executor above, sized by maximumPoolSize, its queue
;; argument contributing capacity to .getQueue's view. Tasks flow through the
;; executor's own unbounded queue — the JVM REJECTS a submit when the bounded
;; queue is full, jolt queues it; that is the deliberate divergence here.
;;
;; abq state: #(capacity mutex cond queue-pair count); queue-pair is (out . in)
;; like the executor's. One condition serves takers and putters (broadcast).
(define (make-abq cap)
  (make-jhost "abq" (vector cap (make-mutex) (make-condition) (cons '() '()) 0)))
(define (abq? x) (and (jhost? x) (string=? (jhost-tag x) "abq")))
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
(define (abq-deq! st)
  (abq-norm! st)
  (let* ((q (vector-ref st 3)) (out (car q)) (v (car out)))
    (set-car! q (cdr out))
    (vector-set! st 4 (fx- (vector-ref st 4) 1))
    (jolt-cv-wake! (vector-ref st 2))
    v))
;; blocking take/put; deadline #f waits forever. take answers (v . #t) so a #f
;; ELEMENT stays distinct from a timeout's #f.
(define (abq-take-wait self deadline)
  (let ((st (jhost-state self)))
    (jolt-cv-wait-interruptibly "BlockingQueue.take"
                                (vector-ref st 1) (vector-ref st 2) deadline
      (lambda (timed-out?)
        (cond ((fx>? (vector-ref st 4) 0) (cons (abq-deq! st) #t))
              (timed-out? #f)
              (else jolt-cv-again))))))
(define (abq-put-wait self v deadline)
  (let ((st (jhost-state self)))
    (jolt-cv-wait-interruptibly "BlockingQueue.put"
                                (vector-ref st 1) (vector-ref st 2) deadline
      (lambda (timed-out?)
        (cond ((fx<? (vector-ref st 4) (vector-ref st 0)) (abq-enq! st v) #t)
              (timed-out? #f)
              (else jolt-cv-again))))))
(for-each (lambda (nm) (register-class-ctor! nm
            ;; (cap) / (cap fair) — fairness is accepted and ignored
            (lambda (cap . _) (make-abq (jnum->exact cap)))))
          '("ArrayBlockingQueue" "java.util.concurrent.ArrayBlockingQueue"))
(register-host-methods! "abq"
  (list (cons "offer" (lambda (self v . args)
          (if (null? args)
              (let ((st (jhost-state self)))
                (jolt-with-mutex (vector-ref st 1)
                  (if (fx<? (vector-ref st 4) (vector-ref st 0))
                      (begin (abq-enq! st v) #t)
                      #f)))
              (abq-put-wait self v (ms->deadline-millis (tu-args->ms args))))))
        (cons "put" (lambda (self v) (abq-put-wait self v #f) jolt-nil))
        (cons "take" (lambda (self) (car (abq-take-wait self #f))))
        (cons "poll" (lambda (self . args)
          (if (null? args)
              (let ((st (jhost-state self)))
                (jolt-with-mutex (vector-ref st 1)
                  (if (fx>? (vector-ref st 4) 0) (abq-deq! st) jolt-nil)))
              (let ((r (abq-take-wait self (ms->deadline-millis (tu-args->ms args)))))
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
        (cons "toString" (lambda (self)
          (string-append "ArrayBlockingQueue(" (number->string (vector-ref (jhost-state self) 4)) ")")))))

;; FutureTask — a run-once task with a blocking get. State:
;; #(status override-flag override value error mutex cond thunk); status is one
;; of new/running/done/cancelled. cancel wins only before run starts (the JVM
;; can interrupt a RUNNING task; jolt cannot, so cancel answers #f there).
(define (make-future-task thunk override-flag override)
  (make-jhost "future-task"
              (vector 'new override-flag override jolt-nil #f (make-mutex) (make-condition) thunk)))
(define (future-task? x) (and (jhost? x) (string=? (jhost-tag x) "future-task")))
(define (future-task-run! self)
  (let ((st (jhost-state self)))
    (when (jolt-with-mutex (vector-ref st 5)
            (and (eq? (vector-ref st 0) 'new)
                 (begin (vector-set! st 0 'running) #t)))
      (let ((r (guard (e (#t (vector-set! st 4 e) jolt-nil)) (jolt-invoke (vector-ref st 7)))))
        (jolt-with-mutex (vector-ref st 5)
          (vector-set! st 3 (if (vector-ref st 1) (vector-ref st 2) r))
          (vector-set! st 0 'done)
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
                          (and ms (ms->deadline-millis ms))
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
          (else #f))))
(register-host-methods! "future-task"
  (list (cons "run" (lambda (self) (future-task-run! self)))
        (cons "get" future-task-get)
        (cons "isDone" (lambda (self) (and (memq (vector-ref (jhost-state self) 0) '(done cancelled)) #t)))
        (cons "isCancelled" (lambda (self) (eq? (vector-ref (jhost-state self) 0) 'cancelled)))
        (cons "cancel" (lambda (self . _)
          (let ((st (jhost-state self)))
            (jolt-with-mutex (vector-ref st 5)
              (if (eq? (vector-ref st 0) 'new)
                  (begin (vector-set! st 0 'cancelled)
                         (jolt-cv-wake! (vector-ref st 6))
                         #t)
                  #f)))))
        (cons "toString" (lambda (self)
          (string-append "FutureTask[" (symbol->string (vector-ref (jhost-state self) 0)) "]")))))
;; submit/execute above route a FutureTask through its own run (a Runnable on
;; the JVM); anything else is an invokable thunk.
(define (runnable->thunk x)
  (if (future-task? x)
      (lambda () (future-task-run! x) jolt-nil)
      x))

;; ThreadPoolExecutor ctor: (core max keepAlive unit [queue] [factory]
;; [handler]) — core, max and keepAlive all reach the pool now that the pool can
;; grow and retire; factory and handler are accepted and ignored. The workers up
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
                (make-executor* core-n max-n keep
                                (and q (vector-ref (jhost-state q) 0)))))))
          '("ThreadPoolExecutor" "java.util.concurrent.ThreadPoolExecutor"))
;; .getQueue answers a live VIEW of the executor's internal queue — size reads
;; the real depth, remainingCapacity subtracts it from the advisory capacity
;; (Integer/MAX_VALUE without one). Honest for monitoring; not the caller's
;; ArrayBlockingQueue instance, which the executor does not use.
(define (executor-queue-depth ex)
  (vector-ref (jhost-state ex) 10))
(register-host-methods! "executor-queue-view"
  (list (cons "size" (lambda (self) (executor-queue-depth (vector-ref (jhost-state self) 0))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (executor-queue-depth (vector-ref (jhost-state self) 0)))))
        (cons "remainingCapacity" (lambda (self)
          (let* ((ex (vector-ref (jhost-state self) 0))
                 (cap (vector-ref (jhost-state ex) 5)))
            (if cap (max 0 (- cap (executor-queue-depth ex))) 2147483647))))
        (cons "toString" (lambda (self) "ExecutorQueueView"))))
(register-host-methods! "executor-service"
  (list (cons "getQueue" (lambda (self) (make-jhost "executor-queue-view" (vector self))))
        ;; ThreadPoolExecutor's size accessors, which are how a caller (and the
        ;; test suite) can see the pool grow and retire at all. getActiveCount is
        ;; documented as approximate on the JVM and is approximate here for the
        ;; same reason: it is a live count read without stopping the pool.
        ;; getMaximumPoolSize of a cached pool answers Integer/MAX_VALUE, which is
        ;; the number the JVM's own newCachedThreadPool passes.
        (cons "getPoolSize" (lambda (self) (vector-ref (jhost-state self) 4)))
        (cons "getActiveCount" (lambda (self)
          (let ((st (jhost-state self)))
            (max 0 (fx- (vector-ref st 4) (vector-ref st 9))))))
        (cons "getCorePoolSize" (lambda (self) (vector-ref (jhost-state self) 6)))
        (cons "getMaximumPoolSize" (lambda (self) (vector-ref (jhost-state self) 7)))))

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
  (list (cons "lock" (lambda (self) (monitor-enter! (rlock-monitor self)) jolt-nil))
        ;; monitor-exit! is where "unlock from a non-owner throws" already lives, and
        ;; it throws the IllegalMonitorStateException the JVM does.
        (cons "unlock" (lambda (self) (monitor-exit! (rlock-monitor self)) jolt-nil))
        ;; tryLock() gives up at once; tryLock(timeout, unit) keeps trying until the
        ;; deadline. The timeout used to be discarded, so the bounded form gave up
        ;; immediately on any lock that happened to be held at that instant; then it
        ;; was honoured but polled with (sleep), which on a fiber guaranteed it would
        ;; run to the deadline whenever the holder was a sibling.
        (cons "tryLock" (lambda (self . args)
          (let* ((m (rlock-monitor self))
                 (ms (tu-args->ms args))
                 (deadline (and ms (ms->deadline ms))))
            (let attempt ()
              (cond ((monitor-try-enter! m) #t)
                    ;; Chez has no timed acquire, so poll. The wait is bounded by the
                    ;; deadline; the round yields first so the holder can run.
                    ((and deadline (time<=? (current-time 'time-utc) deadline))
                     (monitor-poll-round!)
                     (attempt))
                    (else #f))))))
        (cons "lockInterruptibly" (lambda (self)
          (let ((m (rlock-monitor self))
                (me (current-interrupt-box)))
            (rlock-interrupted-check! me)
            (let loop ()
              (unless (monitor-try-enter! m)
                (monitor-poll-round!)
                (rlock-interrupted-check! me)
                (loop)))
            jolt-nil)))
        ;; isLocked is "held by ANY thread"; getHoldCount and isHeldByCurrentThread are
        ;; about the CURRENT one, and the count is 0 rather than the raw depth for a
        ;; caller that does not hold it. The old versions read the raw fields, so
        ;; getHoldCount reported the holder's depth to everybody.
        (cons "isLocked" (lambda (self) (monitor-locked? (rlock-monitor self))))
        (cons "getHoldCount" (lambda (self) (monitor-self-count (rlock-monitor self))))
        (cons "isHeldByCurrentThread"
              (lambda (self) (monitor-held-by-self? (rlock-monitor self))))))

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
                                                     (make-mutex) (make-condition))))
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
                                                     (make-mutex) (make-condition))))
                          (jolt-main-enqueue! j)
                          (condition-signal jolt-main-queue-cv)
                          #t)))))
        (unless enq (jolt-invoke thunk))
        jolt-nil)))

;; (exit 0) rather than calling _exit: the exit handler runs the hooks, so ^C
;; while parked reaches exactly the same cleanup every other exit path does.
(define jolt-pump-kih (lambda () (exit 0)))

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
  (jolt-set-sigint-blocked #f)
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

;; Per-thread SIGINT mask. A worker thread parked in a foreign call (the nREPL
;; accept loop in c-accept, or a conn handler) can't run Chez's keyboard-interrupt
;; handler on ^C, so if SIGINT is delivered there the process hangs. Block SIGINT
;; in the primordial thread BEFORE forking such workers (they inherit the mask),
;; then park-until-interrupt unblocks it in the primordial once its handler is
;; installed, so ^C is always delivered to the parked thread.
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
;;   - on ^C while parked in park-until-interrupt (jolt-pump-kih exits);
;;   - on SIGTERM / SIGHUP, through the watcher thread further down.
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
  (jolt-with-mutex jolt-shutdown-mutex
    (set-box! jolt-shutdown-hooks (cons (cons key thunk) (unbox jolt-shutdown-hooks))))
  ;; outside the mutex: arming takes it to test the watcher's flag
  (jolt-arm-shutdown!)
  jolt-nil)

(define (jolt-add-shutdown-hook thunk) (jolt-register-shutdown-hook! thunk thunk))

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

;; --- SIGTERM / SIGHUP --------------------------------------------------------
;; The signals a JVM runs shutdown hooks for and jolt can take over: SIGTERM (the
;; default `kill`, what a supervisor sends) and SIGHUP. Same numbers on Linux and
;; macOS. SIGINT is deliberately NOT here — Chez owns it through
;; keyboard-interrupt-handler, and ^C reaches a child through the foreground
;; process group anyway.
(define jolt-shutdown-signals '(15 1))
(define jolt-shutdown-sigset #f)
(define c-sigwait (jolt-foreign-proc-blocking "sigwait" '(void* void*) 'int))
;; The watcher cannot leave through Chez's (exit): called off the main thread it
;; never returns. It has already run the hooks and flushed by then, so _exit is
;; the whole of what is left to do.
(define c-underscore-exit (jolt-foreign-proc-safe "_exit" '(int) 'void))

;; Arming happens on the FIRST shutdown hook, never before. Taking a signal over means
;; its default "terminate now" disposition no longer applies, and a program with
;; nothing to clean up must stay killable even when its main thread is somewhere
;; Scheme cannot be resumed from — parked in Chez's own blocking stdin read, say,
;; where no Scheme thread runs at all. A program that registered a hook has asked
;; for the cleanup and takes that trade; one that never did is left exactly as it
;; was.
;;
;; A DEDICATED THREAD parked in sigwait, not (register-signal-handler): Chez runs
;; a registered handler on the main thread at its next safe point, and the two
;; ways a program most often waits — condition-wait (deref of a promise) and a
;; blocking foreign call — are not safe points. The handler would never run and
;; the process would then ignore the signal entirely, which is worse than not
;; handling it. Blocking the signals here also blocks them in every thread forked
;; afterwards (a new thread inherits its creator's mask), so the kernel has no
;; thread to deliver to and leaves the signal pending for sigwait to take.
(define (jolt-arm-shutdown!)
  (jolt-install-exit-handler!)
  (when (and jolt-sigmask-ok? c-sigwait c-underscore-exit)
    (let ((start? (jolt-with-mutex jolt-shutdown-mutex
                    (and (not jolt-shutdown-sigset)
                         (begin (set! jolt-shutdown-sigset
                                      (jolt-make-sigset jolt-shutdown-signals))
                                #t)))))
      (when start?
        (let ((old (sa-foreign-alloc 128)))
          (c-pthread-sigmask jolt-sig-block-how jolt-shutdown-sigset old)
          (sa-foreign-free old))
        (fork-thread
          (lambda ()
            (let ((sigbuf (sa-foreign-alloc 8)))
              (let loop ()
                (if (= 0 (c-sigwait jolt-shutdown-sigset sigbuf))
                    (let ((sig (sa-foreign-ref 'int sigbuf 0)))
                      (guard (_ (#t #f)) (jolt-run-shutdown-hooks!))
                      (guard (_ (#t #f)) (flush-output-port (current-output-port)))
                      (guard (_ (#t #f)) (flush-output-port (current-error-port)))
                      ;; 128+signal is the status a shell reports for a process
                      ;; killed by that signal, and what the JVM exits with here.
                      (c-underscore-exit (+ 128 sig)))
                    (loop))))))))) ; EINTR — ask again
  jolt-nil)

;; --- a child must not inherit jolt's signal mask -----------------------------
;; Both spawn paths hand the calling thread's mask straight to the child —
;; posix_spawn with a NULL attrp, and Chez's own fork — and jolt blocks signals
;; in threads for its own reasons: SIGINT so ^C reaches the thread parked in
;; park-until-interrupt rather than an nREPL worker sitting in a foreign call,
;; SIGTERM/SIGHUP so the watcher above can take them. Either one inherited is
;; wrong in a way that bites at once: a child with SIGTERM blocked survives the
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
  (if (not jolt-empty-sigset)
      (thunk)
      (let* ((old (sa-foreign-alloc 128))
             (restore (lambda ()
                        (c-pthread-sigmask jolt-sig-setmask-how old 0)
                        (sa-foreign-free old))))
        (c-pthread-sigmask jolt-sig-setmask-how jolt-empty-sigset old)
        ;; not dynamic-wind: its after-thunk can run more than once, and this one
        ;; frees. Normal return and throw both restore exactly once; multiple
        ;; values survive the round trip.
        (let ((vals (guard (e (#t (restore) (raise e))) (call-with-values thunk list))))
          (restore)
          (apply values vals)))))

(def-var! "jolt.host" "call-on-main-thread" jolt-call-on-main-thread)
(def-var! "jolt.host" "call-on-main-thread-async" jolt-call-on-main-thread-async)
(def-var! "jolt.host" "run-main-pump" jolt-run-main-pump)
(def-var! "jolt.host" "stop-main-pump" jolt-stop-main-pump)
(def-var! "jolt.host" "add-shutdown-hook" jolt-add-shutdown-hook)
(def-var! "jolt.host" "block-sigint" (lambda () (jolt-set-sigint-blocked #t)))
(def-var! "jolt.host" "park-until-interrupt" jolt-park-until-interrupt)

;; reference types report their JVM classes and answer the IDeref/IRef taxonomy
;; ((class (agent 1)) is clojure.lang.Agent; derefables are IDeref; the mutable
;; references — Atom/Ref/Agent/Var — are IRef; Ref and Var are also IFn).
(register-class-arm! jolt-agent? (lambda (x) "clojure.lang.Agent"))
(register-class-arm! jolt-delay? (lambda (x) "clojure.lang.Delay"))
(register-class-arm! (lambda (x) (jvol? x)) (lambda (x) "clojure.lang.Volatile"))
(register-class-arm! (lambda (x) (var-cell? x)) (lambda (x) "clojure.lang.Var"))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "user-thread"))) (lambda (x) "java.lang.Thread"))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (symbol-t? type-sym)
        (let ((tn (symbol-t-name type-sym)))
          (cond
            ((or (string=? tn "IDeref") (string=? tn "clojure.lang.IDeref"))
             (if (or (jolt-atom? val) (jolt-ref? val) (jolt-agent? val) (var-cell? val)
                     (jvol? val) (jolt-delay? val) (jolt-future? val) (jolt-promise? val))
                 #t 'pass))
            ((or (string=? tn "IRef") (string=? tn "clojure.lang.IRef"))
             (if (or (jolt-atom? val) (jolt-ref? val) (jolt-agent? val) (var-cell? val))
                 #t 'pass))
            ((or (string=? tn "IFn") (string=? tn "clojure.lang.IFn"))
             (if (or (jolt-ref? val) (var-cell? val)) #t 'pass))
            ((or (string=? tn "IPending") (string=? tn "clojure.lang.IPending"))
             (if (or (jolt-delay? val) (jolt-future? val) (jolt-promise? val)) #t 'pass))
            (else 'pass)))
        'pass)))
