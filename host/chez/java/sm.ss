;; R7 state-machine runtime — host/chez/java/sm.ss
;;
;; The cheap go representation. A go body that parks ONLY lexically is CPS'd by
;; the compiler into a single step function `sm` of no arguments; every park op
;; inside it calls `(k value)` where k is the continuation of the park point.
;; The step itself never captures a continuation: a park that has to WAIT stores
;; its resume closure in the fiber's sm-k field and switches to the scheduler
;; (jolt-sm-yield!) without call/1cc — the scheduler's resume path re-runs the
;; fiber's thunk, which is this file's driver: it runs whatever sm-k holds.
;;
;; Memory model (R7 gate 4): a parked SM go holds (a) the step closure and any
;; captured locals, (b) the sm-k resume closure, and (c) NO stack segment — the
;; driver frame is O(1) and unwinds into the scheduler on every park. The
;; continuation fiber keeps its whole (lazily split) stack segment live, ~3995 B
;; in R6. The SM path is exactly the R4 headline — a park can run through any
;; number of normal Scheme calls, because the park is a bare store + switch, not
;; a capture of the frame the park expression sits in.
;;
;; SEMANTIC CONTRACT (gate 3): same values, same exception behaviour, same
;; finally semantics as the continuation fiber. A park must not run a finally —
;; in the SM there is no continuation escape at all, so no dynamic-wind
;; after-thunks can fire between the park and the switch; the switch itself
;; (jolt-sm-yield!) mirrors jolt-fiber-to-scheduler!'s park-unwinding flag so
;; the fibers.ss guard around the fiber body still skips the park unwind.

;; --- the driver ---------------------------------------------------------------
;; The fiber's thunk is this driver; the CURRENT STEP is the fiber's sm-k field
;; (a continuation fiber would instead hold a captured continuation k). The
;; driver is invoked on first run and on every resume, so the scheduler needs no
;; SM knowledge: jolt-fiber-resume*'s "thunk or k" choice just finds the thunk
;; set. There is no continuation anywhere in the parked state.
(define (jolt-sm-driver)
  ((jolt-fiber-sm-k (jolt-current-fiber))))

;; Store the resume and park WITHOUT capturing a continuation. Mirrors the R3
;; waiter commit (state 'parked under the handler's wmu, so a deliver that raced
;; the commit runs the resume inline instead) and jolt-fiber-to-scheduler!'s
;; switch, minus the call/1cc. The scheduler resumes us by re-running the driver
;; thunk, which runs the stored resume.
(define (jolt-sm-commit! f h resume)
  (let ((park?
         (with-mutex (alt-handler-wmu h)
           (if (vector-ref (alt-handler-mailbox h) 0)
               #f
               (begin (jolt-fiber-state-set! f 'parked) #t)))))
    (if park?
        (begin (jolt-fiber-sm-k-set! f resume) (jolt-sm-yield! f))
        (resume))))

;; The switch: slice + park flag + scheduler hand-off, exactly
;; jolt-fiber-to-scheduler! minus the capture. The park-unwinding flag matters:
;; the fibers.ss dynamic-wind guard around the fiber body fires as this unwind
;; happens; the after-thunks belong to forms the SM body is still inside, so
;; they must skip (the continuation fiber flags the same escape).
(define (jolt-sm-yield! f)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  (jolt-fiber-slice-save! f)
  (jolt-park-unwinding-set! #t)
  ((jolt-carrier-sched-k (jolt-fiber-carrier f))))

;; --- park ops -----------------------------------------------------------------
;; Every op is `(op args... k)` where k is the step continuation. On a thread
;; (go body running with the :thread backend) the op BLOCKS and k runs inline —
;; identical to today's <! / >! / alts!. On a fiber, a ready channel delivers
;; inline; a channel that would block stores `(lambda () (k value))` in sm-k and
;; switches. <!! / >!! are registered to the same functions (R5 semantics: they
;; differ only on the thread path, where <!! on a thread is the same blocking
;; take; on a fiber the SM treats them identically to <! / >! — the channel op
;; itself is the only blocking point, and it is always a park).

(define (jolt-sm-<! ch k)
  (if (jolt-current-fiber)
      (jolt-sm-fiber-<! ch k)
      (k (jolt-async-take ch))))

(define (jolt-sm-fiber-<! ch k)
  (mutex-acquire (async-chan-mu ch))
  (let ((r (ac-poll!/locked ch)))
    (if (eq? r ac-poll-empty)
        (let* ((f (jolt-current-fiber))
               (h (jolt-fiber-waiter f)))
          (async-chan-alt-takers-set! ch (append (async-chan-alt-takers ch) (list h)))
          (ac-notify! ch)
          (if (vector-ref (alt-handler-mailbox h) 0)
              (let ((v (vector-ref (alt-handler-mailbox h) 1)))
                (mutex-release (async-chan-mu ch))
                (k v))
              (begin
                (mutex-release (async-chan-mu ch))
                (jolt-sm-commit! f h (lambda () (k (vector-ref (alt-handler-mailbox h) 1)))))))
        (begin (mutex-release (async-chan-mu ch)) (k r)))))

(define (jolt-sm->! ch v k)
  (if (jolt-current-fiber)
      (jolt-sm-fiber->! ch v k)
      (k (jolt-async-give ch v))))

(define (jolt-sm-fiber->! ch v k)
  (mutex-acquire (async-chan-mu ch))
  (let ((r (ac-try-give!/locked ch v)))
    (cond
      ((eq? r 'ok) (mutex-release (async-chan-mu ch)) (k #t))
      ((eq? r 'closed) (mutex-release (async-chan-mu ch)) (k #f))
      (else
       (let* ((f (jolt-current-fiber))
              (h (jolt-fiber-waiter f)))
         (async-chan-alt-putters-set! ch (append (async-chan-alt-putters ch) (list (cons h v))))
         (ac-notify! ch)
         (if (vector-ref (alt-handler-mailbox h) 0)
             (let ((ok (vector-ref (alt-handler-mailbox h) 1)))
               (mutex-release (async-chan-mu ch))
               (k ok))
             (begin
               (mutex-release (async-chan-mu ch))
               (jolt-sm-commit! f h (lambda () (k (vector-ref (alt-handler-mailbox h) 1)))))))))))

(define (jolt-sm-alts! ports priority? k)
  (if (jolt-current-fiber)
      (k (jolt-async-do-alts* ports priority?
                              (lambda (h f unregister!) (jolt-sm-await h f unregister! k))))
      (k (jolt-async-do-alts ports priority?))))

;; The SM alts! await: mirror of the fiber condvar-free await, but the park
;; stores a resume instead of capturing. On the immediate path (the mailbox was
;; already written — a deliver that beat the commit) unregister + return the
;; result, exactly like the thread/fiber awaits so jolt-async-do-alts*'s tail
;; call hands it back to jolt-sm-alts!'s k. On the park path, never returns.
(define (jolt-sm-await h f unregister! k)
  (let ((park?
         (with-mutex (alt-handler-wmu h)
           (if (vector-ref (alt-handler-mailbox h) 0)
               #f
               (begin (jolt-fiber-state-set! f 'parked) #t)))))
    (if park?
        (begin
          (jolt-fiber-sm-k-set! f
            (lambda ()
              (unregister!)
              (let ((mb (alt-handler-mailbox h)))
                (k (jolt-vector (vector-ref mb 1) (vector-ref mb 2))))))
          (jolt-sm-yield! f))
        (begin
          (unregister!)
          (let ((mb (alt-handler-mailbox h)))
            (jolt-vector (vector-ref mb 1) (vector-ref mb 2)))))))

;; --- spawn --------------------------------------------------------------------
;; The step is the whole CPS'd go body: (lambda () initial-step). It is stored
;; in the fiber's sm-k BEFORE the fiber is enqueued, so the carrier can never
;; run the driver against a nil step. Mirrors jolt-fiber-go-spawn's wrapper
;; (fibers-async.ss): result on the channel, exceptions reported, close at the
;; end. The sm-k resume closures also flow through the same wrapper's guard, so
;; an exception thrown inside a resumed step is caught exactly where a
;; continuation fiber's would be.
(define (jolt-fiber-sm-go-spawn step)
  (let ((w (ac-make 1 'fixed #f))
        (c (jolt-fiber-pick!)))
    (let ((f (make-jolt-fiber
              'ready
              (lambda ()
                (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke jolt-sm-driver)))))
                  (if (car r)
                      (when (not (jolt-nil? (cdr r))) (jolt-async-give w (cdr r)))
                      (async-report-uncaught! "go/fiber body (channel closed)" (cdr r)))
                  (jolt-async-close! w)))
              #f #f #f #f
              (make-jolt-dslice (jolt-slice-stack-param)
                                (jolt-slice-ns-param)
                                #f)
              c step)))
      (jolt-fiber-enqueue! c f)
      (jolt-fiber-ensure-carrier!)
      w)))

;; The R7 dispatcher — replaces async-go-spawn for eligible go bodies. On the
;; :thread backend the step runs as a plain thread body (parks block the thread,
;; exactly today's go); on the :fiber backend it runs as an SM fiber. The
;; go-backend dynvar is read at SPAWN time, matching R4's go-spawn contract.
(define (jolt-go-sm-spawn step)
  (if (eq? (go-backend-current) jolt-go-backend-fiber)
      (jolt-fiber-sm-go-spawn step)
      (async-go-spawn-thread step)))

;; --- registration -------------------------------------------------------------
;; The step and the park ops are global names the compiler emits: the CPS'd body
;; closes over nothing but its free vars, and the emitted step calls these exact
;; host functions. Register them under jolt.host (the backend namespace the
;; compiler trusts; rt.ss does the same for jolt-fiber). The <!! / >!! aliases
;; point at the same functions (see the park-ops comment).
(define (jolt-sm-<!! ch k) (jolt-sm-<! ch k))
(define (jolt-sm->!! ch v k) (jolt-sm->! ch v k))

;; The compiler emits (clojure.core.async/go-spawn-sm step) for eligible go
;; bodies (and clojure.core.async/sm-<! sm->! sm-<!! sm->!! sm-alts! for the
;; park ops). async.ss loads before this file, so cca-def! and its var machinery
;; are available; the def! runs here, after both files exist.
(cca-def! "go-spawn-sm" jolt-go-sm-spawn)
(cca-def! "sm-<!" jolt-sm-<!)
(cca-def! "sm->!" jolt-sm->!)
(cca-def! "sm-<!!" jolt-sm-<!!)
(cca-def! "sm->!!" jolt-sm->!!)
(cca-def! "sm-alts!" jolt-sm-alts!)
