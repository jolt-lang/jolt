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
       (*txn* #f)                          ; child thread must not inherit parent's txn
       (dyn-binding-stack snap)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (with-mutex (jolt-future-mu f)
           (unless (jolt-future-done? f)            ; not already cancelled
             (jolt-future-ok?-set! f (car r))
             (jolt-future-payload-set! f (cdr r))
             (jolt-future-done?-set! f #t))
           (condition-broadcast (jolt-future-cv f))))))
    f))

;; Final value of a settled future (called OUTSIDE the lock): wrap a captured
;; throw in an ExecutionException (JVM semantics), signal a cancellation, else
;; the value. The original exception is stored as the cause so ex-cause works.
(define (jolt-future-finish f)
  (cond
    ((jolt-future-cancelled? f)
     (jolt-throw (jolt-ex-info "Future cancelled" (jolt-hash-map))))
    ((jolt-future-ok? f) (jolt-future-payload f))
    (else (jolt-throw (jolt-host-throwable "java.util.concurrent.ExecutionException"
                        (jolt-str-render-one (jolt-future-payload f))
                        (jolt-future-payload f))))))

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

;; (class a-future)/(class a-promise): both are anonymous reify instances of a
;; clojure.core fn on the JVM — clojure.core$future_call$reify__N /
;; clojure.core$promise$reify__N. The __N counter is unstable per eval; jolt
;; matches the stable enclosing-fn prefix and pins __0 (rather than :object).
(register-class-arm! jolt-future? (lambda (f) "clojure.core$future_call$reify__0"))
(register-class-arm! jolt-promise? (lambda (p) "clojure.core$promise$reify__0"))

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
           (hashtable-set! meta-table a m))
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
        (dynamic-wind
          (lambda () (dyn-binding-stack (cons (list (cons cell a)) (dyn-binding-stack))))
          thunk
          (lambda () (dyn-binding-stack (cdr (dyn-binding-stack))))))))

;; Enqueue an action and start the worker if the agent is idle. No precondition
;; checks — used by the direct send path (after checks), by nested-send release,
;; and by restart resuming a held queue.
(define (jolt-agent-enqueue! a f args)
  (with-mutex (jolt-agent-mu a)
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
    (let ((act (with-mutex (jolt-agent-mu a)
                 (if (or (not (jolt-nil? (jolt-agent-err a))) (jagent-q-empty? a))
                     (begin (jolt-agent-running?-set! a #f)
                            (condition-broadcast (jolt-agent-cv a)) #f)
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
                    (with-mutex (jolt-agent-mu a)
                      (jolt-agent-err-set! a err)
                      (condition-broadcast (jolt-agent-cv a)))))
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
;; await dispatches a latch action to each agent and the send throws. (An agent
;; that fails DURING the await returns once the queue halts — friendlier than
;; the JVM, whose latch action never runs so await blocks forever.)
(define (jolt-agent-failed-throw a)
  (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                   "Agent is failed, needs restart"
                                   (jolt-agent-err a))))
(define (jolt-agent-await . agents)
  (jolt-agent-await-check)
  (for-each
    (lambda (a)
      (with-mutex (jolt-agent-mu a)
        (unless (jolt-nil? (jolt-agent-err a)) (jolt-agent-failed-throw a))
        (let loop ()
          (when (or (jolt-agent-running? a) (not (jagent-q-empty? a)))
            (condition-wait (jolt-agent-cv a) (jolt-agent-mu a)) (loop)))))
    agents)
  jolt-nil)
(define (jolt-agent-await-for ms . agents)
  (when (*txn*)
    (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "await-for in transaction")))
  (when (jolt-in-agent-action?)
    (jolt-throw (jolt-host-throwable "java.lang.Exception" "Can't await in agent action")))
  (let ((deadline (ms->deadline ms)) (ok #t))
    (for-each
      (lambda (a)
        (when ok
          (with-mutex (jolt-agent-mu a)
            (unless (jolt-nil? (jolt-agent-err a)) (jolt-agent-failed-throw a))
            (let loop ()
              (when (or (jolt-agent-running? a) (not (jagent-q-empty? a)))
                (if (condition-wait (jolt-agent-cv a) (jolt-agent-mu a) deadline)
                    (loop)
                    (when (or (jolt-agent-running? a) (not (jagent-q-empty? a)))
                      (set! ok #f))))))))
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
(define (jolt-agent-restart a new-state . opts)
  (let ((clear? (and (pair? opts) (keyword-t? (car opts))
                     (string=? (keyword-t-name (car opts)) "clear-actions")
                     (pair? (cdr opts)) (eq? (cadr opts) #t))))
    (with-mutex (jolt-agent-mu a)
      (when (jolt-nil? (jolt-agent-err a))
        (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                         "Agent does not need a restart")))
      (let ((vf (jolt-agent-validator a)))
        (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf new-state)))
          (jolt-iref-state-throw)))
      (jolt-agent-state-set! a new-state)
      (jolt-agent-err-set! a jolt-nil)
      (cond (clear? (jagent-q-clear! a))
            ((and (not (jagent-q-empty? a)) (not (jolt-agent-running? a)))
             (jolt-agent-running?-set! a #t)
             (fork-thread (lambda () (*txn* #f) (jolt-agent-worker a)))))))
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
  (with-mutex tapset-mu
    (unless (memq f (unbox tapset))
      (set-box! tapset (cons f (unbox tapset))))))
(define (tapset-remove! f)
  (with-mutex tapset-mu
    (set-box! tapset (filter (lambda (x) (not (eq? x f))) (unbox tapset)))))
(define (tapset-snapshot)
  (with-mutex tapset-mu (unbox tapset)))

(define (tapq-offer! v)
  (with-mutex (jolt-tap-queue-mu tapq)
    (if (>= (jolt-tap-queue-len tapq) tapq-capacity)
        #f
        (begin
          (jolt-tap-queue-in-set! tapq (cons v (jolt-tap-queue-in tapq)))
          (jolt-tap-queue-len-set! tapq (fx+ 1 (jolt-tap-queue-len tapq)))
          (condition-broadcast (jolt-tap-queue-cv tapq))
          #t))))

(define (tapq-take!)
  (with-mutex (jolt-tap-queue-mu tapq)
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
        (else (condition-wait (jolt-tap-queue-cv tapq) (jolt-tap-queue-mu tapq)) (loop))))))

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
                 ;; guard ensures timer+handler are disarmed on EVERY exit from
                 ;; the thunk — normal return, exception raise, and escape-continuation
                 ;; jump (the outer set-timer/handler handles the interrupt case).
                 (guard (e (#t (set-timer 0) (timer-interrupt-handler prev-handler) (raise e)))
                   (let ((v (thunk))) (set-timer 0) v))))))
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
(define (make-jthread thunk) (make-jhost "user-thread" (vector thunk #f (make-mutex) (make-condition) (box #f) #f)))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (thunk . _) (make-jthread thunk))))
          '("Thread" "java.lang.Thread"))
(register-host-methods! "user-thread"
  (list (cons "start" (lambda (self)
          (let ((st (jhost-state self)) (snap (dyn-binding-stack)))
            (vector-set! st 5 #t)  ; mark started before forking
            (fork-thread (lambda ()
               (*txn* #f)                          ; child thread must not inherit parent's txn
               (dyn-binding-stack snap)
               ;; surface a thread body's throw like the JVM's default uncaught-
              ;; exception handler; the thread still completes (isAlive/join
              ;; semantics unchanged). Reporting failures are swallowed.
              (guard (e (#t (guard (_ (#t #f))
                              (display "Exception in Thread body:\n" (current-error-port))
                              (jolt-report-throwable e (current-error-port)))))
                (jolt-invoke (vector-ref st 0)))
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
        ;; alive = started and not yet completed (JVM: false before .start)
        (cons "isAlive" (lambda (self) (let ((st (jhost-state self)))
          (and (vector-ref st 5) (not (vector-ref st 1))))))
        (cons "interrupt" (lambda (self . _) (set-box! (vector-ref (jhost-state self) 4) #t) jolt-nil))
        (cons "isInterrupted" (lambda (self) (and (unbox (vector-ref (jhost-state self) 4)) #t)))
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

;; --- java.util.concurrent.ExecutorService / Executors ----------------------
;; A real task QUEUE served by a fixed number of worker threads (FIFO). A single
;; worker (newSingleThreadExecutor) runs tasks strictly in submission order —
;; code relies on that ordering (claxon dispatches handlers on a single-thread
;; executor and a later empty task acts as a barrier). submit returns a Future
;; whose .get waits for the result (re-raising the task's throw, like the JVM).
;; j-future state: #(done? result error mutex condition)
(define (make-j-future) (make-jhost "j-future" (vector #f jolt-nil #f (make-mutex) (make-condition))))
(define (j-future-complete! self thunk)
  (let ((st (jhost-state self)))
    (let ((r (guard (e (#t (vector-set! st 2 e) #f)) (jolt-invoke thunk))))
      (with-mutex (vector-ref st 3)
        (unless (vector-ref st 2) (vector-set! st 1 r))
        (vector-set! st 0 #t)
        (condition-broadcast (vector-ref st 4))))))
(register-host-methods! "j-future"
  (list (cons "get" (lambda (self . _)
          (let ((st (jhost-state self)))
            (with-mutex (vector-ref st 3)
              (let loop () (unless (vector-ref st 0) (condition-wait (vector-ref st 4) (vector-ref st 3)) (loop))))
            (if (vector-ref st 2) (jolt-throw (vector-ref st 2)) (vector-ref st 1)))))
        (cons "isDone" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "isCancelled" (lambda (self) #f))
        (cons "cancel" (lambda (self . _) #f))))
;; executor-service state: #(shutdown? queue-box queue-mutex queue-cond worker-count)
;; queue-box holds a pair (out . in) — out is the dequeue head-list, in is the
;; enqueue tail-list (reversed). Enqueue conses onto in (O(1)); dequeue pops from
;; out, reversing in into out when out is empty (amortized O(1)).
(define (make-executor n-workers)
  (let ((self (make-jhost "executor-service" (vector #f (box (cons '() '())) (make-mutex) (make-condition) n-workers))))
    (let ((st (jhost-state self)))
      (let spawn ((k n-workers))
        (when (> k 0)
          (fork-thread (lambda ()
            (let loop ()
              (let ((job (with-mutex (vector-ref st 2)
                           (let poll ()
                             (let ((q (unbox (vector-ref st 1))))
                               (cond ((pair? (car q))
                                      (let ((out (car q)))
                                        (set-car! q (cdr out))
                                        (car out)))
                                     ((pair? (cdr q))
                                      (set-car! q (reverse (cdr q)))
                                      (set-cdr! q '())
                                      (let ((out (car q)))
                                        (set-car! q (cdr out))
                                        (car out)))
                                     ((vector-ref st 0) #f)   ; shutdown + empty -> exit
                                     (else (condition-wait (vector-ref st 3) (vector-ref st 2)) (poll))))))))
                (if job
                    (begin (job) (loop))
                    (with-mutex (vector-ref st 2)
                      (vector-set! st 4 (fx- (vector-ref st 4) 1))
                      (condition-broadcast (vector-ref st 3))))))))
          (spawn (- k 1))))
      self)))
(define (executor-enqueue! self job)
  (let ((st (jhost-state self)))
    (with-mutex (vector-ref st 2)
      (let ((q (unbox (vector-ref st 1))))
        (set-cdr! q (cons job (cdr q))))
      (condition-broadcast (vector-ref st 3)))))
(let ((single (lambda _ (make-executor 1)))
      (fixed  (lambda (n . _) (make-executor (max 1 (jnum->exact n)))))
      ;; per-task / cached / virtual: enough workers to not serialize; a generous
      ;; fixed pool preserves concurrency without unbounded thread growth.
      (many   (lambda _ (make-executor 32))))
  (for-each (lambda (nm) (register-class-statics! nm
              (list (cons "newSingleThreadExecutor" single)
                    (cons "newSingleThreadScheduledExecutor" single)
                    (cons "newFixedThreadPool" fixed) (cons "newScheduledThreadPool" fixed)
                    (cons "newVirtualThreadPerTaskExecutor" many)
                    (cons "newCachedThreadPool" many) (cons "newWorkStealingPool" many))))
            '("Executors" "java.util.concurrent.Executors")))
(register-host-methods! "executor-service"
  (list (cons "submit" (lambda (self thunk)
          (let ((fut (make-j-future)) (snap (dyn-binding-stack)))
            (executor-enqueue! self (lambda () (dyn-binding-stack snap) (j-future-complete! fut thunk)))
            fut)))
        (cons "execute" (lambda (self thunk)
          (let ((snap (dyn-binding-stack)))
            (executor-enqueue! self (lambda () (dyn-binding-stack snap)
              (guard (e (#t (guard (_ (#t #f))
                              (display "Exception in executor task:\n" (current-error-port))
                              (jolt-report-throwable e (current-error-port)))))
                (jolt-invoke thunk)))))
          jolt-nil))
        (cons "shutdown" (lambda (self) (let ((st (jhost-state self)))
          (vector-set! st 0 #t) (with-mutex (vector-ref st 2) (condition-broadcast (vector-ref st 3)))) jolt-nil))
        (cons "shutdownNow" (lambda (self) (let ((st (jhost-state self)))
          (vector-set! st 0 #t) (with-mutex (vector-ref st 2) (condition-broadcast (vector-ref st 3)))) (jolt-vector)))
        (cons "close" (lambda (self) (let ((st (jhost-state self)))
          (vector-set! st 0 #t) (with-mutex (vector-ref st 2) (condition-broadcast (vector-ref st 3)))) jolt-nil))
        (cons "isShutdown" (lambda (self) (vector-ref (jhost-state self) 0)))
        (cons "isTerminated" (lambda (self) (let* ((st (jhost-state self)) (q (unbox (vector-ref st 1))))
          (and (vector-ref st 0) (null? (car q)) (null? (cdr q)) (fx=? 0 (vector-ref st 4))))))
        (cons "awaitTermination" (lambda (self ms . _)
          (let* ((st (jhost-state self))
                 (deadline (+ (now-millis) (if (number? ms) (jnum->exact ms) 0))))
            ;; check under the mutex, but SLEEP OUTSIDE it — a worker's exit
            ;; decrement needs this mutex, so sleeping while holding it starves
            ;; the very transition awaited (the wait always rode to deadline).
            (let waiting ()
              (let ((done (with-mutex (vector-ref st 2)
                            (and (vector-ref st 0) (fx=? 0 (vector-ref st 4))))))
                (cond (done #t)
                      ((> (now-millis) deadline) #f)
                      (else
                       (let ((remaining (- deadline (now-millis))))
                         (sleep (ms->duration (max 10 (min remaining 100))))
                         (waiting)))))))))))

;; java.util.concurrent.locks.ReentrantLock — a reentrant mutual-exclusion lock.
;; State: #(mutex owner-box hold-count). owner-box is the owning thread's interrupt
;; box (nil if unlocked). The same thread may acquire multiple times (hold count
;; incremented); each unlock balances one lock — unlock from a non-owner throws.
(define (make-reentrant-lock) (make-jhost "reentrant-lock" (vector (make-mutex) #f 0)))
(for-each (lambda (nm) (register-class-ctor! nm (lambda _ (make-reentrant-lock))))
          '("ReentrantLock" "java.util.concurrent.locks.ReentrantLock"))
(register-host-methods! "reentrant-lock"
  (list (cons "lock" (lambda (self)
          (let* ((st (jhost-state self)) (mu (vector-ref st 0)) (me (current-interrupt-box)))
            (if (eq? (vector-ref st 1) me)
                (vector-set! st 2 (fx+ (vector-ref st 2) 1))
                (begin (mutex-acquire mu) (vector-set! st 1 me) (vector-set! st 2 1)))
            jolt-nil)))
        (cons "unlock" (lambda (self)
          (let* ((st (jhost-state self)) (me (current-interrupt-box)))
            (unless (eq? (vector-ref st 1) me)
              (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException" "not lock owner")))
            (vector-set! st 2 (fx- (vector-ref st 2) 1))
            (when (fx=? 0 (vector-ref st 2))
              (vector-set! st 1 #f)
              (mutex-release (vector-ref st 0)))
            jolt-nil)))
        (cons "tryLock" (lambda (self . _)
          (let* ((st (jhost-state self)) (mu (vector-ref st 0)) (me (current-interrupt-box)))
            (cond ((eq? (vector-ref st 1) me) (vector-set! st 2 (fx+ (vector-ref st 2) 1)) #t)
                  ((mutex-acquire mu #f) (vector-set! st 1 me) (vector-set! st 2 1) #t)
                  (else #f)))))
        (cons "lockInterruptibly" (lambda (self)
          (let* ((st (jhost-state self)) (mu (vector-ref st 0)) (me (current-interrupt-box)))
            (when (unbox me)
              (set-box! me #f)
              (jolt-throw (jolt-host-throwable "java.lang.InterruptedException" "lock interrupted")))
            (if (eq? (vector-ref st 1) me)
                (vector-set! st 2 (fx+ (vector-ref st 2) 1))
                (let loop ()
                  (when (unbox me)
                    (set-box! me #f)
                    (jolt-throw (jolt-host-throwable "java.lang.InterruptedException" "lock interrupted")))
                  (if (mutex-acquire mu #f)
                      (begin (vector-set! st 1 me) (vector-set! st 2 1))
                      (begin (sleep (make-time 'time-duration 10000000 0)) (loop)))))
            jolt-nil)))
        (cons "isLocked" (lambda (self) (fx>? (vector-ref (jhost-state self) 2) 0)))
        (cons "getHoldCount" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "isHeldByCurrentThread" (lambda (self)
          (eq? (vector-ref (jhost-state self) 1) (current-interrupt-box))))))

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
  (jolt-foreign-proc-safe "pthread_sigmask" '(int u8* u8*) 'int))
(define c-sigemptyset (jolt-foreign-proc-safe "sigemptyset" '(u8*) 'int))
(define c-sigaddset (jolt-foreign-proc-safe "sigaddset" '(u8* int) 'int))
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

;; reference types report their JVM classes and answer the IDeref/IRef taxonomy
;; ((class (agent 1)) is clojure.lang.Agent; derefables are IDeref; the mutable
;; references — Atom/Ref/Agent/Var — are IRef; Ref and Var are also IFn).
(register-class-arm! jolt-agent? (lambda (x) "clojure.lang.Agent"))
(register-class-arm! jolt-delay? (lambda (x) "clojure.lang.Delay"))
(register-class-arm! (lambda (x) (jvol? x)) (lambda (x) "clojure.lang.Volatile"))
(register-class-arm! (lambda (x) (var-cell? x)) (lambda (x) "clojure.lang.Var"))
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
