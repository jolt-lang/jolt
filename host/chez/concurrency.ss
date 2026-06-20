;; concurrency.ss (jolt-byjr) — real OS-thread futures + promises for the Chez host.
;;
;; SHARED-HEAP semantics (JVM Clojure), NOT Janet's isolated-heap snapshot: a
;; future body runs on a native thread (fork-thread) over the SAME heap, so a
;; captured atom is shared and the body's mutations are visible to the parent —
;; matching `clojure.core` on the JVM. deref blocks on a mutex+condition latch.
;;
;; future / future-call / future-cancel / future? / future-done? / future-cancelled?
;; promise / deliver, and the deref extension for both, are bound here (some
;; re-asserted in post-prelude.ss over the overlay's Janet-shaped versions).
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

(define (jolt-future-done?* f) (and (jolt-future? f) (jolt-future-done? f)))
(define (jolt-native-future-done? x)
  (if (jolt-future? x) (jolt-future-done? x)
      (jolt-throw (jolt-ex-info "future-done? requires a future" (jolt-hash-map)))))
(define (jolt-native-future-cancelled? x)
  (and (jolt-future? x) (jolt-future-cancelled? x)))

;; --- promises ---------------------------------------------------------------
;; A blocking promise (JVM), not Janet's non-blocking atom shim: deref parks until
;; deliver, then caches the value. deliver wins once; later delivers return nil.
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

;; --- deref extension --------------------------------------------------------
;; Chain the fully-built jolt-deref (atoms/vars/volatiles/reduced) with futures +
;; promises, and accept the timed (deref ref ms val) arity for both.
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
      (else (apply %pre-conc-deref x opts)))))

;; realized? for a Chez future/promise (the overlay reads Janet map keys). Wrap the
;; overlay version in post-prelude.ss; here just the future/promise predicate.
(define (jolt-conc-realized? x)
  (cond ((jolt-future? x) (jolt-future-done? x))
        ((jolt-promise? x) (jolt-promise-delivered? x))
        (else #f)))

;; --- bind into clojure.core -------------------------------------------------
(def-var! "clojure.core" "future-call" jolt-future-call)
(def-var! "clojure.core" "future-cancel" jolt-future-cancel)
(def-var! "clojure.core" "future?" jolt-future?)
(def-var! "clojure.core" "future-done?" jolt-native-future-done?)
(def-var! "clojure.core" "future-cancelled?" jolt-native-future-cancelled?)
(def-var! "clojure.core" "promise" jolt-promise-new)
(def-var! "clojure.core" "deliver" jolt-deliver)
(def-var! "clojure.core" "deref" jolt-deref)
