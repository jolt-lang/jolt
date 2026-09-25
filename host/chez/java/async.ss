;; async.ss — clojure.core.async channel primitives on real OS threads.
;;
;; A `go` block is an OS thread and a channel is a Chez mutex+condition blocking
;; queue: <! / >! are the blocking <!! / >!! (they "park" by blocking the thread),
;; and work ANYWHERE — no CPS transform, no go-only restriction. Real parallelism,
;; shared heap. This is a superset of the JVM model: it has no fixed go-block
;; thread pool, no MAX-QUEUE-SIZE on pending ops, and parking ops are legal outside
;; a go block. One OS thread per go block (fine for typical use).
;;
;; Channel: an unbuffered channel is a rendezvous (the putter blocks until its
;; value is taken); a buffered (chan n) put blocks only when full; dropping/sliding
;; buffers never block the putter. A transducer is applied on the put side; an
;; optional ex-handler catches a throw from the transducer step.
;;
;; This file provides the primitives; the higher-level dataflow API (mult, mix,
;; pub/sub, pipeline, map, merge, reduce, …) is a Clojure overlay over them.
;; `thread` is a macro here (mark-macro!) expanding to thread-spawn. `go` and
;; `go-loop` are NOT: they live in the overlay, because the CPS pass that decides
;; a park's representation per site needs &env, macroexpand and resolve, which a
;; Scheme expander over reader forms does not have. go-spawn — the runtime the
;; overlay's fallback path emits — stays here. Loaded after concurrency.ss
;; (reuses ms->duration). Requires a threaded Chez build.

;; --- buffers ----------------------------------------------------------------
(define-record-type async-buffer (fields n kind) (nongenerative async-buffer-v1))
(define (jolt-async-buffer n)          (make-async-buffer n 'fixed))
(define (jolt-async-dropping-buffer n) (make-async-buffer n 'dropping))
(define (jolt-async-sliding-buffer n)  (make-async-buffer n 'sliding))
(define (jolt-async-unblocking-buffer? b)
  (if (and (async-buffer? b) (memq (async-buffer-kind b) '(dropping sliding promise))) #t #f))

;; --- alt-handler: the ONE waiter notion (alts!, plus R3 fiber waiters) --------
;; A pending channel operation is a handler with active?/claim/commit semantics
;; and a wakeup strategy. The `wake` field is #f for a thread-waiter (the
;; blocked <!!/>!! waiter: alt-deliver! signals wcv, exactly as before) or a
;; fiber record for a fiber-waiter (alt-deliver! resumes it — enqueues it on
;; its carrier's run queue). The channel core never inspects the wake: it
;; commits via claim+mailbox and lets the handler decide how to wake. This is
;; the R3 waiter protocol — one handler notion, two wakeup strategies, no fork
;; of the channel code into fiber and thread paths. The name stays alt-handler
;; because the alt-takers/alt-putters lists ARE the shared waiter lists: a
;; fiber's <! registers as an alt-taker, exactly as alts! does.
(define-record-type alt-handler
  (fields fmu (mutable active?) wmu wcv mailbox (mutable wake))
  (nongenerative alt-handler-v2))
(define (alt-handler-alloc . wake)
  ((record-constructor (record-type-descriptor alt-handler))
   (make-mutex) #t (make-mutex) (make-condition) (vector #f #f #f)
   (if (pair? wake) (car wake) #f)))

;; --- channels ---------------------------------------------------------------
;; items: an amortized-O(1) FIFO held as a mutable #(out in len) — `out` is the
;; front (pop from its head), `in` holds pushed entries reversed onto it, `len` is
;; the count (an append-to-a-list FIFO is O(n) per push and O(n) to measure).
;; Each entry is (value . box); box is #f for a buffered value or a 1-slot vector
;; for an unbuffered rendezvous put (set #t when taken, waking the putter).
;; cap 0 + kind 'unbuffered = rendezvous; cap>0 with kind fixed/dropping/sliding.
;; takew counts threads parked in a blocking take (so a non-blocking offer! to an
;; unbuffered channel can tell a taker is waiting). alt-takers/alt-putters are
;; pending alt-handler registrations (alts! ops parked on this channel). xrf is the
;; transducer reducing fn (or #f); exh the ex-handler (or #f).
(define-record-type async-chan
  (fields mu cv (mutable items) cap kind (mutable closed?) (mutable xrf) (mutable takew)
          exh (mutable alt-takers) (mutable alt-putters))
  (nongenerative async-chan-v3))

(define (ac-qnew) (vector '() '() 0))
(define (ac-qlen ch) (vector-ref (async-chan-items ch) 2))
(define (ac-qempty? ch) (fx=? 0 (vector-ref (async-chan-items ch) 2)))
(define (ac-qpush! ch entry)
  (let ((q (async-chan-items ch)))
    (vector-set! q 1 (cons entry (vector-ref q 1)))
    (vector-set! q 2 (fx+ 1 (vector-ref q 2)))))
(define (ac-qfront! q)                  ; ensure `out` is non-empty: out := reverse in
  (when (null? (vector-ref q 0))
    (vector-set! q 0 (reverse (vector-ref q 1)))
    (vector-set! q 1 '())))
(define (ac-qpop! ch)
  (let ((q (async-chan-items ch)))
    (ac-qfront! q)
    (let ((out (vector-ref q 0)))
      (vector-set! q 0 (cdr out))
      (vector-set! q 2 (fx- (vector-ref q 2) 1))
      (car out))))
(define (ac-qdrop-oldest! ch)
  (let ((q (async-chan-items ch)))
    (ac-qfront! q)
    (vector-set! q 0 (cdr (vector-ref q 0)))
    (vector-set! q 2 (fx- (vector-ref q 2) 1))))

;; enqueue honoring the buffer kind (used by the transducer step + buffered puts).
(define (ac-buf-give! ch v)
  (case (async-chan-kind ch)
    ((dropping) (when (< (ac-qlen ch) (async-chan-cap ch)) (ac-qpush! ch (cons v #f))))
    ((sliding)  (when (>= (ac-qlen ch) (async-chan-cap ch)) (ac-qdrop-oldest! ch))
                (ac-qpush! ch (cons v #f)))
    (else       (ac-qpush! ch (cons v #f))))      ; fixed: caller ensured room
  (ac-notify! ch))

;; --- alt handler claim/deliver -----------------------------------------------
;; alt-claim! returns #t exactly once per handler (first claim wins).
;; LOCK ORDER: channel mu → fmu → wmu → run-queue mu (a fiber wake's enqueue).
;; Never hold two channel mutexes at once. Never YIELD while holding a channel
;; mutex: a waiter registers under the channel mutex and releases it before it
;; suspends — a fiber that parks holding the mutex deadlocks its carrier and
;; every other fiber on it (the R3 invariant; the fiber-side registration in
;; fibers-async.ss releases before parking).
(define (alt-claim! h)
  (jolt-with-mutex (alt-handler-fmu h)
    (and (alt-handler-active? h)
         (begin (alt-handler-active?-set! h #f) #t))))

;; alt-deliver! — call ONLY after alt-claim! returned #t. The mailbox write and
;; the wake decision happen under wmu so a fiber-waiter's park (which checks the
;; mailbox and sets its state under the SAME wmu, see jolt-fiber-waiter-wait!)
;; cannot race the delivery: a deliver that observes the fiber 'running means it
;; has not finished parking and will see the mailbox before it decides to park;
;; one that observes 'parked means the fiber is committed and must be enqueued.
;; The resume runs under wmu and takes the run-queue mutex — a leaf lock never
;; acquired by the fiber park path, so the order above has no cycle.
(define jolt-fiber-wake-fn #f)  ; set by fibers-async.ss (loads after this file)
;; R4: the fiber alts! await — (jolt-fiber-alt-await-fn h) -> [val port], parked.
;; Set by fibers-async.ss (loads after this file); #f until then, which is fine
;; because no alts! can run before the boot finishes loading.
(define jolt-fiber-alt-await-fn #f)
(define (alt-deliver! h val port)
  (jolt-with-mutex (alt-handler-wmu h)
    (let ((mb (alt-handler-mailbox h)))
      (vector-set! mb 1 val) (vector-set! mb 2 port) (vector-set! mb 0 #t))
    (let ((w (alt-handler-wake h)))
      (if w
          (if jolt-fiber-wake-fn
              (jolt-fiber-wake-fn w)
              (condition-signal (alt-handler-wcv h)))
          (condition-signal (alt-handler-wcv h))))))

;; ac-notify! — drain pending alt registrations after any channel state mutation.
;; Called with the channel mutex held. Loops steps 1→2→3 until a full pass makes
;; no progress.
(define (ac-notify! ch)
  (let loop ()
    (let ((progress #f))
      ;; Step 1: drain queue → alt-takers
      (let ((q (async-chan-items ch)))
        (let drain-takers ()
          (when (and (fx>? (vector-ref q 2) 0) (pair? (async-chan-alt-takers ch)))
            (let ((h (car (async-chan-alt-takers ch))))
              (async-chan-alt-takers-set! ch (cdr (async-chan-alt-takers ch)))
              (if (alt-claim! h)
                  (begin (alt-deliver! h (ac-take-head! ch) ch) (set! progress #t))
                  (set! progress #t)) ; dead registration — dropped
              (drain-takers)))))
      ;; Step 2: drain alt-putters → capacity
      (let drain-putters ()
        (when (pair? (async-chan-alt-putters ch))
          (let* ((hp (car (async-chan-alt-putters ch)))
                 (h (car hp)) (v (cdr hp)))
            (cond
              ;; can accept right now?
              ((case (async-chan-kind ch)
                 ((dropping sliding) #t)
                 ((promise) (and (ac-qempty? ch) #t))
                 (else
                  (if (> (async-chan-cap ch) 0)
                      (< (ac-qlen ch) (async-chan-cap ch))
                      (> (async-chan-takew ch) 0))))
               (async-chan-alt-putters-set! ch (cdr (async-chan-alt-putters ch)))
               (if (alt-claim! h)
                   (begin
                     ;; Same acceptance logic as ac-try-give!'s body
                     (cond
                       ((async-chan-xrf ch)
                        (let ((r (ac-xrf-apply ch v)))
                          (when (jolt-reduced? r) (ac-close! ch))))
                       (else
                        (case (async-chan-kind ch)
                          ((dropping sliding)
                           (ac-buf-give! ch v))
                          ((promise)
                           (ac-qpush! ch (cons v #f))
                           (condition-broadcast (async-chan-cv ch)))
                          (else
                           (if (> (async-chan-cap ch) 0)
                               (begin (ac-qpush! ch (cons v #f))
                                      (condition-broadcast (async-chan-cv ch)))
                               (let ((box (vector #f)))
                                 (ac-qpush! ch (cons v box))
                                 (condition-broadcast (async-chan-cv ch))))))))
                     (alt-deliver! h #t ch)
                     (set! progress #t))
                   (set! progress #t)) ; dead registration
               (drain-putters))
              (else #f)))))
      ;; Step 3: pair alt-putters with alt-takers directly (unbuffered channels
      ;; where a putter and taker are both parked). Only when a blocking taker
      ;; or active alt-taker exists to consume the value.
      (let pair-loop ()
        (when (and (pair? (async-chan-alt-putters ch))
                   (pair? (async-chan-alt-takers ch))
                   (or (> (async-chan-takew ch) 0)
                       (ormap (lambda (h) (alt-handler-active? h))
                              (async-chan-alt-takers ch))))
          (let* ((hp (car (async-chan-alt-putters ch)))
                 (h (car hp)) (v (cdr hp)))
            (if (alt-claim! h)
                (begin
                  (async-chan-alt-putters-set! ch (cdr (async-chan-alt-putters ch)))
                  ;; Commit the value: unbuffered rendezvous push
                  (let ((box (vector #f)))
                    (ac-qpush! ch (cons v box))
                    (condition-broadcast (async-chan-cv ch)))
                  (alt-deliver! h #t ch)
                  (set! progress #t)
                  (pair-loop))
                (begin
                  (async-chan-alt-putters-set! ch (cdr (async-chan-alt-putters ch)))
                  (set! progress #t)
                  (pair-loop))))))
      (when progress (loop))))
  (condition-broadcast (async-chan-cv ch)))

;; A transducer is a jolt fn (xform); (xform add-rf) yields the channel's reducing
;; fn. add-rf: 0-arg init, 1-arg completion, 2-arg step (enqueue the output). A
;; `reduced` step result closes the channel.
(define (ac-make-add-rf ch)
  (lambda args
    (cond ((null? args) ch)                                   ; init
          ((null? (cdr args)) (car args))                     ; completion
          (else (ac-buf-give! ch (cadr args)) (car args)))))  ; step

;; run the transducer step (or completion) guarded by the channel's ex-handler:
;; if the xform throws and exh returns non-nil, that value is added to the buffer.
(define (ac-xrf-apply ch . v)
  (let ((xrf (async-chan-xrf ch)) (exh (async-chan-exh ch)))
    ;; The handler is jolt code and takes a THROWABLE, so the raised condition
    ;; has to be unwrapped exactly as a catch clause would — jolt-throw raises a
    ;; &jolt-throw condition wrapping the value, and handing that straight to the
    ;; handler delivers an opaque #object[:object] whose ex-data, ex-message and
    ;; class are all nil. The future path (concurrency.ss) already unwraps; this
    ;; was the one site that did not.
    (guard (e (#t (if exh
                      (let ((else (jolt-invoke exh (jolt-unwrap-throw e))))
                        (unless (jolt-nil? else) (ac-buf-give! ch else))
                        (async-chan-xrf ch))   ; treat as non-reduced
                      (raise e))))
      (apply jolt-invoke xrf ch v))))

(define (ac-make cap kind xrf) (make-async-chan (make-mutex) (make-condition) (ac-qnew) cap kind #f xrf 0 #f '() '()))
(define (ac-make/exh cap kind exh) (make-async-chan (make-mutex) (make-condition) (ac-qnew) cap kind #f #f 0 exh '() '()))

;; (chan) | (chan n) | (chan buf) | (chan n|buf xform) | (chan n|buf xform exh)
(define (jolt-async-chan . args)
  (let ((buf (if (pair? args) (car args) jolt-nil))
        (xform (if (and (pair? args) (pair? (cdr args))) (cadr args) jolt-nil))
        (exh (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args))) (caddr args) jolt-nil)))
    (let-values (((cap kind)
                  (cond ((async-buffer? buf) (values (async-buffer-n buf) (async-buffer-kind buf)))
                        ((and (number? buf) (> buf 0)) (values buf 'fixed))
                        (else (values 0 'unbuffered)))))
      (let ((ch (ac-make/exh cap kind (if (jolt-nil? exh) #f exh))))
        (unless (jolt-nil? xform)
          (async-chan-xrf-set! ch (jolt-invoke xform (ac-make-add-rf ch))))
        ch))))

;; close! (idempotent): mark closed, flush a stateful transducer's completion,
;; notify pending alt handlers, and wake everyone. ac-close! assumes the lock is
;; held; the public form takes it.
(define (ac-close! ch)
  (unless (async-chan-closed? ch)
    (async-chan-closed?-set! ch #t)
    (when (async-chan-xrf ch)
      (guard (e (#t (async-report-uncaught! "transducer completion on close!" e)))
        (ac-xrf-apply ch)))
    (ac-notify! ch)
    ;; claim+deliver to every remaining alt-taker (nil) and alt-putter (#f)
    (for-each (lambda (h) (when (alt-claim! h) (alt-deliver! h jolt-nil ch)))
              (async-chan-alt-takers ch))
    (async-chan-alt-takers-set! ch '())
    (for-each (lambda (hp) (when (alt-claim! (car hp)) (alt-deliver! (car hp) #f ch)))
              (async-chan-alt-putters ch))
    (async-chan-alt-putters-set! ch '())
    (condition-broadcast (async-chan-cv ch)))
  jolt-nil)
(define (jolt-async-close! ch) (jolt-with-mutex (async-chan-mu ch) (ac-close! ch)))

;; >! / >!! — put, blocking. false if closed; nil may not be put. With a
;; transducer the value is run through it (one put -> zero or more channel values);
;; a `reduced` result closes the channel.
(define (jolt-async-give ch v)
  (async-check-put! v)
  (jolt-with-mutex (async-chan-mu ch)
    (cond
      ((async-chan-closed? ch) #f)
      ((async-chan-xrf ch)
       (if (> (async-chan-cap ch) 0)
           ;; Fixed buffered with xform: wait for room, then apply xform.
           ;; The xform step may overfill transiently (e.g. mapcat); the NEXT put
           ;; will wait again.
           (let loop ()
             (cond ((async-chan-closed? ch) #f)
                   ((< (ac-qlen ch) (async-chan-cap ch))
                    (let ((r (ac-xrf-apply ch v)))
                      (when (jolt-reduced? r) (ac-close! ch))
                      #t))
                   (else (jolt-condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))
           ;; Unbuffered with xform: apply immediately (output goes to rendezvous queue)
           (let ((r (ac-xrf-apply ch v)))
             (when (jolt-reduced? r) (ac-close! ch))
             #t)))
      (else
       (case (async-chan-kind ch)
         ((dropping sliding) (ac-buf-give! ch v) #t)
          ;; a promise channel takes ONE value, delivered to every taker; further
          ;; puts are dropped. Never blocks.
          ((promise) (when (ac-qempty? ch)
                       (ac-qpush! ch (cons v #f)))
                     (ac-notify! ch)
                     #t)
          (else
           (if (> (async-chan-cap ch) 0)
               (let loop ()                                    ; buffered fixed: wait for room
                 (cond ((async-chan-closed? ch) #f)
                       ((< (ac-qlen ch) (async-chan-cap ch))
                        (ac-qpush! ch (cons v #f)) (ac-notify! ch) #t)
                       (else (jolt-condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))
               (let ((box (vector #f)))                        ; unbuffered: rendezvous
                 (ac-qpush! ch (cons v box))
                 (ac-notify! ch)
                  (let loop ()
                   (if (vector-ref box 0)
                       #t
                       (begin (jolt-condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))))))))))

;; remove + return the head value, waking a parked rendezvous putter.
(define (ac-take-head! ch)
  (let* ((entry (ac-qpop! ch)) (v (car entry)) (box (cdr entry)))
    (when box (vector-set! box 0 #t))
    (ac-notify! ch)
    v))

;; peek the front value without removing it (promise channels keep their value).
(define (ac-peek ch)
  (let ((q (async-chan-items ch)))
    (ac-qfront! q)
    (car (car (vector-ref q 0)))))

;; <! / <!! — take, blocking. Drains buffered values, then nil once closed + empty.
;; A promise channel PEEKS — its one value stays for every taker.
;; When the queue is empty, drains pending alt-putters before parking.
(define (jolt-async-take ch)
  (jolt-with-mutex (async-chan-mu ch)
    (let loop ()
      (cond ((eq? (async-chan-kind ch) 'promise)
             (cond ((not (ac-qempty? ch)) (ac-peek ch))
                   ((async-chan-closed? ch) jolt-nil)
                   (else (ac-take-wait ch) (loop))))
            ((not (ac-qempty? ch)) (ac-take-head! ch))
            ((async-chan-closed? ch) jolt-nil)
            ;; drain an alt-putter if one is parked (no xform chans — those
            ;; complete immediately into the buffer via ac-buf-give!)
            ((and (pair? (async-chan-alt-putters ch))
                  (not (async-chan-xrf ch)))
             (let* ((hp (car (async-chan-alt-putters ch)))
                    (h (car hp)) (v (cdr hp)))
               (async-chan-alt-putters-set! ch (cdr (async-chan-alt-putters ch)))
               (if (alt-claim! h)
                   (begin
                     (alt-deliver! h #t ch)
                     ;; commit value (unbuffered rendezvous)
                     (let ((box (vector #f)))
                       (ac-qpush! ch (cons v box))
                       (condition-broadcast (async-chan-cv ch)))
                     (ac-take-head! ch))
                   (loop))))  ; dead registration, retry
            (else (ac-take-wait ch) (loop))))))

;; park in a take, tracking the waiter count so a concurrent offer! to an
;; unbuffered channel can see that a taker is ready.
(define (ac-take-wait ch)
  (async-chan-takew-set! ch (fx+ 1 (async-chan-takew ch)))
  (jolt-condition-wait (async-chan-cv ch) (async-chan-mu ch))
  (async-chan-takew-set! ch (fx- (async-chan-takew ch) 1)))

;; non-blocking take for alts!/poll!: a value, jolt-nil (closed+empty), or ac-poll-empty.
;; Drains pending alt-putters when the queue is empty (same drain path as jolt-async-take).
;; ac-poll!/locked: mutex must already be held.
(define ac-poll-empty (list 'empty))
(define (ac-poll!/locked ch)
  (cond ((and (eq? (async-chan-kind ch) 'promise) (not (ac-qempty? ch))) (ac-peek ch))
        ((not (ac-qempty? ch)) (ac-take-head! ch))
        ((async-chan-closed? ch) jolt-nil)
        ((and (pair? (async-chan-alt-putters ch)) (not (async-chan-xrf ch)))
         (let* ((hp (car (async-chan-alt-putters ch)))
                (h (car hp)) (v (cdr hp)))
           (async-chan-alt-putters-set! ch (cdr (async-chan-alt-putters ch)))
           (if (alt-claim! h)
               (begin
                 (alt-deliver! h #t ch)
                 (let ((box (vector #f)))
                   (ac-qpush! ch (cons v box))
                   (condition-broadcast (async-chan-cv ch)))
                 (ac-take-head! ch))
               ac-poll-empty)))
        (else ac-poll-empty)))
(define (ac-poll! ch)
  (jolt-with-mutex (async-chan-mu ch) (ac-poll!/locked ch)))

;; non-blocking give: 'ok (accepted), 'full (would block), or 'closed.
;; ac-try-give!/locked: mutex must already be held.
(define (ac-try-give!/locked ch v)
  (async-check-put! v)
  (cond
    ((async-chan-closed? ch) 'closed)
    ((async-chan-xrf ch) (if (and (> (async-chan-cap ch) 0)
                            (>= (ac-qlen ch) (async-chan-cap ch)))
                       'full
                       (let ((r (ac-xrf-apply ch v)))
                         (when (jolt-reduced? r) (ac-close! ch)) 'ok)))
    (else
     (case (async-chan-kind ch)
       ((dropping sliding) (ac-buf-give! ch v) 'ok)
       ((promise) (when (ac-qempty? ch) (ac-qpush! ch (cons v #f))) (ac-notify! ch) 'ok)
       (else
        (cond
          ((> (async-chan-cap ch) 0)
           (if (< (ac-qlen ch) (async-chan-cap ch))
               (begin (ac-qpush! ch (cons v #f)) (ac-notify! ch) 'ok)
               'full))
          ;; a waiting taker makes the rendezvous possible: a thread parked in a
          ;; blocking take (takew), or a fiber parked as an alt-taker (R3 — the
          ;; fiber's <! registers an alt-handler, invisible to takew). Without
          ;; the alt-taker clause, offer!/put! to an unbuffered channel would
          ;; report 'full while a fiber waited, and put! would fork a thread
          ;; instead of completing on the caller.
          ((or (> (async-chan-takew ch) 0)
               (ormap (lambda (h) (alt-handler-active? h))
                      (async-chan-alt-takers ch)))
           (let ((box (vector #f)))
             (ac-qpush! ch (cons v box))
             (ac-notify! ch)
             'ok))
          (else 'full)))))))
(define (ac-try-give! ch v)
  (async-check-put! v)
  (jolt-with-mutex (async-chan-mu ch) (ac-try-give!/locked ch v)))

;; offer! / poll! — never block. offer! returns #t/#f(closed) on completion, nil if
;; it would block; poll! returns a value, nil (closed+empty), or the ::none sentinel.
(define cca-none (keyword "clojure.core.async" "none"))
(define (jolt-async-offer! ch v)
  (case (ac-try-give! ch v) ((ok) #t) ((closed) #f) (else jolt-nil)))
(define (jolt-async-poll! ch)
  (let ((r (ac-poll! ch))) (if (eq? r ac-poll-empty) cca-none r)))

;; --- the shared timer -------------------------------------------------------
;; ONE timer thread serves every deadline in the runtime — no per-call OS thread.
;; Pending entries are kept as a sorted list of (deadline-ms . thunk) by ascending
;; deadline. The timer runs every thunk whose deadline has passed, then waits for
;; the nearest remaining one; an insert that becomes the HEAD signals timeout-cv,
;; because that is exactly when the deadline it is waiting for changed.
;;
;; A THUNK AND NOT A CHANNEL. (timeout ms) is one caller and closes a channel;
;; the other is a fiber in a timed wait on a condition variable, which needs
;; waking at the deadline (jolt-cv-wait, host/chez/locks.ss). Nothing about a
;; deadline is specific to channels, and a second timer thread for the second
;; kind of deadline would be a second thing to get wrong — this one already had
;; two bugs of its own (jolt-pe84).
;;
;; THE THUNKS RUN OUTSIDE timeout-mu. They used to run under it, which was
;; survivable while the only thunk was a channel close, and is not survivable in
;; general: a thunk that takes a lock while the timer holds timeout-mu puts
;; timeout-mu above that lock in the order, and any code that registers a deadline
;; while holding it closes a cycle. Collecting the due entries under the lock and
;; running them after it is released means the timer holds nothing while calling
;; out, so registering a deadline from inside any critical section stays safe.
;;
;; THE WAIT IS A TIMED condition-wait HELD UNDER timeout-mu, AND THE THREAD IS
;; FORKED ONCE. Both were otherwise, and each was a bug (jolt-pe84):
;;
;;   * The timer used to release timeout-mu and `sleep` to the nearest deadline,
;;     so it was not on timeout-cv when a nearer deadline arrived. The signal
;;     went nowhere and the new timeout did not fire until the deadline the timer
;;     was already sleeping to: a (timeout 100) created while a (timeout 3000)
;;     was pending took 3000ms to close. condition-wait releases the mutex
;;     atomically with committing to the wait, so an insert can no longer slip
;;     between the deadline read and the wait on it.
;;   * timeout-running? used to be cleared before the idle wait, but a timer that
;;     finds the list empty waits rather than exiting — so the next insert both
;;     signalled the live thread AND forked another one, and every timer ever
;;     forked stays alive forever. 100 sequential (timeout 1) calls left 100
;;     timer threads. The flag now means "the one timer thread exists", which
;;     once true never stops being true.
(define timeout-mu (make-mutex))
(define timeout-cv (make-condition))
;; Pending timeouts as a binary min-heap on deadline — #(deadline . thunk)
;; entries in timeout-heap[0..n), guarded by timeout-mu like the sorted list it
;; replaces. The list insert walked O(k) per (timeout ms) with k pending —
;; O(k^2) to arm a burst — and the consumer only ever takes the MIN, which is
;; the heap's O(log k) case. Entries with EQUAL deadlines pop in arbitrary
;; order; they were already batched into one collect, so nothing promised an
;; order between them.
(define timeout-heap (make-vector 64 #f))
(define timeout-heap-n 0)
(define timeout-running? #f)       ; the one timer thread has been forked

(define (theap-less? a b) (< (car a) (car b)))
(define (theap-min) (and (fx>? timeout-heap-n 0) (vector-ref timeout-heap 0)))
(define (theap-insert! entry)
  (when (fx=? timeout-heap-n (vector-length timeout-heap))
    (let ((w (make-vector (fx* 2 timeout-heap-n) #f)))
      (do ((i 0 (fx+ i 1))) ((fx=? i timeout-heap-n)) (vector-set! w i (vector-ref timeout-heap i)))
      (set! timeout-heap w)))
  (let sift ((i timeout-heap-n))
    (if (fx=? i 0)
        (vector-set! timeout-heap 0 entry)
        (let* ((p (fxquotient (fx- i 1) 2)) (pv (vector-ref timeout-heap p)))
          (if (theap-less? entry pv)
              (begin (vector-set! timeout-heap i pv) (sift p))
              (vector-set! timeout-heap i entry)))))
  (set! timeout-heap-n (fx+ timeout-heap-n 1)))
(define (theap-pop-min!)
  (let ((top (vector-ref timeout-heap 0))
        (n (fx- timeout-heap-n 1)))
    (set! timeout-heap-n n)
    (let ((item (vector-ref timeout-heap n)))
      (vector-set! timeout-heap n #f)
      (when (fx>? n 0)
        (let sift ((i 0))
          (let* ((l (fx+ (fx* 2 i) 1))
                 (r (fx+ l 1))
                 (m (if (and (fx<? l n) (theap-less? (vector-ref timeout-heap l) item)) l i))
                 (m (if (and (fx<? r n)
                             (theap-less? (vector-ref timeout-heap r)
                                          (if (fx=? m i) item (vector-ref timeout-heap m))))
                        r m)))
            (if (fx=? m i)
                (vector-set! timeout-heap i item)
                (begin (vector-set! timeout-heap i (vector-ref timeout-heap m)) (sift m)))))))
    top))

;; -> #t iff the new entry became the earliest deadline, i.e. the caller must
;; signal the timer to re-read its wake time (same contract as the old list:
;; empty, or strictly earlier than the previous minimum).
(define (timeout-insert! deadline-ms thunk)
  (let ((prev-min (theap-min)))
    (theap-insert! (cons deadline-ms thunk))
    (or (not prev-min) (< deadline-ms (car prev-min)))))

;; Everything due, removed from the list, newest deadline last. Called with
;; timeout-mu held; answers '() after waiting when nothing is due yet, so the
;; caller's loop is "collect, release, run, repeat".
(define (timeout-collect-due!)
  (let due ((acc '()))
    (cond
      ((let ((m (theap-min))) (and m (<= (car m) (now-millis))))
       (due (cons (cdr (theap-pop-min!)) acc)))
      ((pair? acc) (reverse acc))
      (else
       (let ((m (theap-min)))
         (if (not m)
             (jolt-condition-wait timeout-cv timeout-mu)
             (let ((wait-ms (- (car m) (now-millis))))
               (when (> wait-ms 0)
                 (jolt-condition-wait timeout-cv timeout-mu (ms->duration wait-ms))))))
       ;; Either deadline or signal, the answer is the same: come back and re-read
       ;; the min. A spurious wake costs one empty trip.
       '()))))

(define (timeout-thread)
  (let loop ()
    (let ((due (jolt-with-mutex timeout-mu (timeout-collect-due!))))
      (for-each (lambda (fire)
                  ;; One thunk must not be able to stop the timer for everything
                  ;; else. A channel close cannot raise; a wake takes a lock it does
                  ;; not own the discipline of, so this is the seam where a bug in
                  ;; somebody else's deadline stays theirs.
                  (guard (e (#t #f)) (fire)))
                due)
      (loop))))

;; (jolt-timer-at! deadline-ms thunk) — run thunk on the timer thread once the
;; epoch-millisecond deadline has passed. The one deadline facility in the runtime.
(define (jolt-timer-at! deadline-ms thunk)
  (jolt-lock! timeout-mu)
  (when (timeout-insert! deadline-ms thunk)
    (condition-signal timeout-cv))
  (unless timeout-running?
    (set! timeout-running? #t)
    (fork-thread timeout-thread))
  (jolt-unlock! timeout-mu))

;; (timeout ms) — a channel that closes after ms milliseconds.
(define (jolt-async-timeout ms)
  (let ((w (ac-make 0 'unbuffered #f)))
    (jolt-timer-at! (+ (now-millis) (exact (floor ms)))
                    (lambda () (jolt-async-close! w)))
    w))

;; (put! ch v [cb [on-caller?]]) — async put, optional completion callback. If the
;; put completes immediately and on-caller? (default #t), the callback runs on the
;; calling thread; otherwise on another thread. Returns true unless already closed.
(define (jolt-async-put! ch v . rest)
  (let* ((cb (if (pair? rest) (car rest) jolt-nil))
         (on-caller? (if (and (pair? rest) (pair? (cdr rest))) (jolt-truthy? (cadr rest)) #t))
         (call-cb (lambda (ok) (unless (jolt-nil? cb) (jolt-invoke cb ok)))))
    (case (ac-try-give! ch v)
      ((ok) (if on-caller? (call-cb #t) (fork-thread (lambda () (*txn* #f) (call-cb #t)))) #t)
      ((closed) (if on-caller? (call-cb #f) (fork-thread (lambda () (*txn* #f) (call-cb #f)))) #f)
      (else (fork-thread (lambda () (*txn* #f) (call-cb (jolt-async-give ch v)))) #t))))

;; (take! ch cb [on-caller?]) — async take. Same on-caller? rule as put!.
(define (jolt-async-take! ch cb . rest)
  (let* ((on-caller? (if (pair? rest) (jolt-truthy? (car rest)) #t))
         (call-cb (lambda (v) (unless (jolt-nil? cb) (jolt-invoke cb v))))
         (r (ac-poll! ch)))
    (cond
      ((eq? r ac-poll-empty) (fork-thread (lambda () (*txn* #f) (call-cb (jolt-async-take ch)))))
      (on-caller? (call-cb r))
      (else (fork-thread (lambda () (*txn* #f) (call-cb r)))))
    jolt-nil))

;; (go-spawn thunk) — run thunk on a thread; return a buffered(1) channel that
;; conveys its value once then closes (a nil result just closes). Dynamic bindings
;; are conveyed (Chez inherits the thread-parameter at fork; we install explicitly).

;; Print an uncaught-exception report to stderr — the JVM routes a thread body's
;; throw to the default uncaught-exception handler; silence here made a throwing
;; worker indistinguishable from one that returned nil. Reporting failures are
;; themselves swallowed (a worker must never die reporting).
(define (async-report-uncaught! where e)
  (guard (_ (#t #f))
    (display (string-append "Exception in " where ":\n") (current-error-port))
    (jolt-report-throwable e (current-error-port)))
  #f)

;; shared nil-put guard (was pasted at three put sites)
;; The message is the reference's, word for word: channels.clj raises
;; IllegalArgumentException "Can't put nil on channel" (no article).
(define (async-check-put! v)
  (when (jolt-nil? v)
    (throw-jvm (quote IllegalArgumentException) "Can't put nil on channel")))

;; clojure.core.async/*go-backend* — the R4 opt-in seam (epic jolt-nvpr.5):
;; :thread (default) is byte-for-byte today's go (a real OS thread); :fiber
;; spawns the body on the R4 fiber carrier. Read at SPAWN time — a runtime
;; call, not a macroexpansion — so (binding [*go-backend* :fiber] …) covers
;; every go that runs inside the scope, including ones in functions it calls.
;; The default stays :thread for the whole epic; R6 decides whether it flips.
(define jolt-go-backend-thread (keyword #f "thread"))
(define jolt-go-backend-fiber (keyword #f "fiber"))
(def-dynvar! "clojure.core.async" "*go-backend*" jolt-go-backend-thread)
;; R5: the fiber carrier-pool size (fibers.ss reads this root; jolt-nil = the
;; machine's processor count, a positive fixnum pins the pool — set it BEFORE
;; the first :fiber go, or between a jolt-fiber-pool-reset! and the next go).
;; The host setter jolt-fiber-carrier-count-set! writes this same root, so the
;; two knobs never disagree. The pool starts once per process.
(def-var! "clojure.core.async" "*fiber-carrier-count*" jolt-nil)
;; Preemption quantum, in Chez engine ticks (fibers.ss reads this root; the host
;; setter jolt-fiber-preempt-ticks-set! writes it, so the two never disagree).
;; jolt-nil means the built-in default, which is ON at roughly 0.45ms: a
;; compute-bound go block yields instead of pinning its carrier for as long as
;; it runs. A fixnum at or above jolt-fiber-preempt-ticks-min pins a different
;; quantum; anything below the floor, 0 included, is ignored and the default
;; stands.
;;
;; THERE IS NO VALUE THAT TURNS PREEMPTION OFF. Cooperative-only is not a milder
;; setting, it is an unbounded starvation window: one fiber that never reaches a
;; channel op starves every other fiber on its carrier, and nothing can migrate
;; them because a fiber is pinned to its carrier for life. Ask for a very long
;; quantum if that is what you want. Read at pool start, so set it before the
;; first :fiber go or between a pool reset and the next one; the host setter
;; jolt-fiber-preempt-ticks-set! is immediate and refuses an out-of-range value
;; out loud.
(def-var! "clojure.core.async" "*fiber-preempt-ticks*" jolt-nil)
(define (go-backend-current)
  (let ((cell (var-cell-lookup "clojure.core.async" "*go-backend*")))
    (if (and cell (var-cell-defined? cell))
        (let ((bv (dyn-binding-value cell)))
          (if (eq? bv dyn-no-binding) (var-cell-root cell) bv))
        jolt-go-backend-thread)))

;; The R4 dispatcher: go/go-loop/go-spawn go through here and honor the var.
;; Nothing else does — thread is always an OS thread and io-thread is always a
;; fiber, because both name their carrier at the call site (see async-fiber-spawn
;; below and the workload table in the overlay's thread-call). jolt-fiber-go-spawn
;; is defined in fibers-async.ss, which loads after this file; the reference
;; resolves at call time, before any :fiber spawn can happen.
(define (async-go-spawn thunk)
  (if (eq? (go-backend-current) jolt-go-backend-fiber)
      (jolt-fiber-go-spawn thunk)
      (async-go-spawn-thread thunk)))

;; (fiber-spawn thunk) — ALWAYS a fiber, whatever *go-backend* says. This is the
;; other half of the thread-spawn bargain: thread-spawn asks for an OS thread by
;; name and gets one, so asking for a fiber by name has to work the same way, and
;; io-thread (the overlay's thread-call :io) is what asks. A dispatcher would make
;; io-thread mean "a fiber, unless someone above me bound *go-backend* :thread",
;; and there is nothing a caller could do with that.
;;
;; No CPS pass here — the body parks by capturing a continuation, which is exactly
;; what a blocking-shaped body wants: a park works anywhere, including inside a
;; called function, a try or a loop the pass cannot rewrite. That is the
;; difference from go, not an omission.
;;
;; Same forward reference as async-go-spawn: jolt-fiber-go-spawn is defined in
;; fibers-async.ss, which loads after this file, and resolves at call time.
(define (async-fiber-spawn thunk)
  (jolt-fiber-go-spawn thunk))

(define (async-go-spawn-thread thunk)
  (let ((w (ac-make 1 'fixed #f)) (snap (dyn-binding-stack)))
    ;; BEFORE the fork: a body that finishes first would otherwise publish into a
    ;; registry with no entry for it, and every later monitor would read the
    ;; missing entry as "nothing to report" — the clean-completion answer.
    (go-chan-register! w)
    (fork-thread
     (lambda ()
       (*txn* #f)                          ; go/thread body must not inherit parent's txn
       (dyn-binding-stack snap)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (if (car r)
             (when (not (jolt-nil? (cdr r))) (jolt-async-give w (cdr r)))
             (async-report-uncaught! "go/thread body (channel closed)" (cdr r)))
         (go-chan-finish! w (and (not (car r)) (cdr r))))))
    w))

;; --- monitoring a go block ----------------------------------------------------
;; A go body that threw and a go body that returned nil are indistinguishable on
;; the result channel: both close it and both hand the reader nil. This is what
;; separates them, and it is keyed on the CHANNEL because that is what go returns
;; — changing go's return value would break every program that reads it.
;;
;; ONE REGISTRY FOR EVERY BACKEND, and that is the point rather than a detail.
;; *go-backend* is read at spawn time off a dynamic binding, so which backend ran
;; a body is a property of the caller's binding, not of the code that spawned it;
;; and within the fiber backend, whether the CPS pass could rewrite the body
;; decides between two spawn paths, which is a property of the body. A monitor
;; that reported on some of those and answered "clean" for the rest would be
;; worse than no monitor, because a caller cannot tell from the jolt side which
;; one it got. sm.ss makes the same argument for its own two spawn paths.
;;
;; Completion is published HERE, by every path that terminally closes a go
;; channel, before the close: a reader woken by the close then finds the monitor
;; already settled.
;;
;; WEAK, and keyed by the channel: a strong table would pin every go channel and
;; its outcome for the life of the process, which on a server spawning a go per
;; request is an unbounded leak. A dropped channel takes its entry with it, and
;; the only thing lost is the ability to monitor a go block nobody holds any more.
;;
;; #(done? error monitors) — allocated by the spawn, so a monitor registered on a
;; channel with no entry (a plain chan, or a go channel whose entry the collector
;; took) reads as "nothing to report" rather than raising.
(define go-completions (make-weak-eq-hashtable))
(define go-completions-mu (make-mutex))
(define (go-chan-register! ch)
  (jolt-with-mutex go-completions-mu
    (hashtable-set! go-completions ch (vector #f #f '()))))

;; Publish the outcome and hand it to whatever is already waiting, then close the
;; channel. ERR is the throwable, or #f for a body that completed.
;;
;; The list is taken and cleared under the lock so a monitor cannot fire twice,
;; and the monitors themselves run OUTSIDE it: each gives to a channel, taking
;; that channel's mutex, and the lock order is registry -> channel.
;;
;; Each monitor is CONTAINED, and the close is what it protects. These run on the
;; finishing body's own thread or carrier, and an escape here would take the
;; jolt-async-close! below with it — leaving the result channel open forever, so
;; every reader of a go block whose monitor happened to raise would hang. One
;; monitor must not be able to stop the others either. Nothing left to report to
;; at this point: the body is already finishing.
(define (go-chan-finish! ch err)
  (let ((waiting
         (jolt-with-mutex go-completions-mu
           (let ((c (hashtable-ref go-completions ch #f)))
             (and c
                  (let ((ms (vector-ref c 2)))
                    (vector-set! c 0 #t)
                    (vector-set! c 1 err)
                    (vector-set! c 2 '())
                    ms))))))
    (for-each (lambda (m) (guard (e (#t #f)) (m err))) (or waiting '())))
  (jolt-async-close! ch))

;; Register PROC on CH's completion. Delivered inline when the body has already
;; finished — a caller cannot check the state and register in one step from
;; outside, so without that a body that finished in between would never notify
;; and the caller would wait forever. The read and the insert are one step under
;; the mutex go-chan-finish! publishes through, so "finished" and "with this
;; outcome" are one observation.
(define (go-chan-monitor! ch proc)
  (let ((now
         (jolt-with-mutex go-completions-mu
           (let ((c (hashtable-ref go-completions ch #f)))
             (cond
               ((not c) (list #f))            ; not a go channel: nothing to report
               ((vector-ref c 0) (list (vector-ref c 1)))
               (else (vector-set! c 2 (cons proc (vector-ref c 2))) #f))))))
    (when now (proc (car now)))))

;; (go-monitor ch) -> channel. Yields the throwable if the body died, and
;; CLOSES (nil) if it completed normally. A promise-style buffered(1) channel, so
;; the value is there whether the caller takes before or after the body finishes.
(define (jolt-go-monitor-chan ch)
  (let ((m (ac-make 1 'fixed #f)))
    (go-chan-monitor! ch
      (lambda (err)
        (when err (jolt-async-give m (jolt-unwrap-throw err)))
        (jolt-async-close! m)))
    m))
(def-var! "clojure.core.async" "go-monitor" jolt-go-monitor-chan)

;; --- alts! entry point -------------------------------------------------------
;; (__do-alts ports priority?) — ports is a jolt vector of channels or [ch val]
;; put specs. Returns a jolt vector [val port]. priority? is a boolean: #t
;; starts scanning at index 0 (declared order); #f picks a random start.
;; LOCK ORDER: channel mu → fmu → wmu. Never hold two channel mutexes at once.
(define (jolt-async-do-alts ports priority?)
  (let* ((n (pvec-count ports))
         (start (if (jolt-truthy? priority?) 0 (jolt-random n)))
         (idx-of (lambda (k) (let ((m (fx+ start k))) (if (fx<? m n) m (fx- m n))))))
    ;; FAST PASS: one non-blocking attempt per op, no handler. Consumption here IS
    ;; the alts result, so consuming directly is correct on this pass only.
    (let fast-loop ((k 0))
      (if (fx<? k n)
          (let ((port (pvec-nth-d ports (idx-of k) jolt-nil)))
            (if (pvec? port)
                (let ((ch (pvec-nth-d port 0 jolt-nil)) (v (pvec-nth-d port 1 jolt-nil)))
                  (case (ac-try-give! ch v)
                    ((ok) (jolt-vector #t ch))
                    ((closed) (jolt-vector #f ch))
                    (else (fast-loop (fx+ k 1)))))
                (let ((r (ac-poll! port)))
                  (if (eq? r ac-poll-empty)
                      (fast-loop (fx+ k 1))
                      (jolt-vector r port)))))
          ;; No fast hit — one shared handler across all ports. Per op, under the
          ;; channel mutex: compute readiness with NO side effects; if ready, CLAIM
          ;; FIRST, then consume (a failed claim means a deliverer on an earlier
          ;; registration won — stop and read the mailbox). If not ready, register
          ;; and ac-notify! under the same lock so a fresh taker pairs with parked
          ;; alt-putters (and vice versa) via notify's pairing step.
          ;;
          ;; The handler's wake is the current FIBER when alts! runs on one (R4):
          ;; alt-deliver! then resumes the fiber instead of signalling the condvar,
          ;; and the await below parks instead of blocking the carrier — a fiber
          ;; waiter is exactly the R3 one-waiter protocol, no second mechanism.
          ;; On a plain thread wake is #f and the await is the condvar wait.
          ;;
          ;; PRUNING (which side): the winner prunes. finish/await call
          ;; unregister!, which removes the handler from EVERY port it registered
          ;; on — so a handler that lost on ports B..N is taken off their waiter
          ;; lists the moment the alts! completes on A; a long-lived channel never
          ;; accumulates dead handlers from lost alts! calls. ac-notify!'s scan is
          ;; the backstop for a registration that dies by claim-race mid-notify
          ;; ("dead registration — dropped" in the drain steps).
          ;; A fiber registers on every port below and then parks, so the lock
          ;; check comes first (locks.ss jolt-fiber-may-park!). The gate cannot
          ;; see this one: the await is reached through jolt-fiber-alt-await-fn.
          (let* ((_ (jolt-fiber-may-park! 'clojure.core.async/alts!))
                 (f (jolt-current-fiber))
                 (h (alt-handler-alloc f))
                 (registered '()))
            (let* ((unregister!
                    (lambda ()
                      (for-each
                        (lambda (entry)
                          (let ((ch (car entry)) (is-put (cdr entry)))
                            (jolt-with-mutex (async-chan-mu ch)
                              (if is-put
                                  (async-chan-alt-putters-set! ch
                                    (remp (lambda (hp) (eq? (car hp) h))
                                          (async-chan-alt-putters ch)))
                                  (async-chan-alt-takers-set! ch
                                    (remp (lambda (x) (eq? x h))
                                          (async-chan-alt-takers ch)))))))
                        registered)))
                   (finish (lambda (val port) (unregister!) (jolt-vector val port)))
                   (await
                    (lambda ()
                      (if (and f jolt-fiber-alt-await-fn)
                          ;; fiber waiter: park, never block the carrier (the
                          ;; handler's channel mutexes are all released here)
                          (let ((r (jolt-fiber-alt-await-fn h)))
                            (unregister!)
                            r)
                          ;; thread waiter: condvar
                          (begin
                            (jolt-with-mutex (alt-handler-wmu h)
                              (let ((mb (alt-handler-mailbox h)))
                                (let wait-loop ()
                                  (unless (vector-ref mb 0)
                                    (jolt-condition-wait (alt-handler-wcv h) (alt-handler-wmu h))
                                    (wait-loop)))))
                            (unregister!)
                            (let ((mb (alt-handler-mailbox h)))
                              (jolt-vector (vector-ref mb 1) (vector-ref mb 2))))))))
              (let reg-loop ((j 0))
                (if (fx=? j n)
                    (await)
                    (let ((port (pvec-nth-d ports (idx-of j) jolt-nil)))
                      (if (pvec? port)
                          ;; put spec [ch val]
                          (let* ((ch (pvec-nth-d port 0 jolt-nil))
                                 (v (pvec-nth-d port 1 jolt-nil))
                                 (res
                                  (jolt-with-mutex (async-chan-mu ch)
                                    (let ((ready?
                                           (or (async-chan-closed? ch)
                                               (async-chan-xrf ch)
                                               (memq (async-chan-kind ch) '(dropping sliding promise))
                                               (and (> (async-chan-cap ch) 0)
                                                    (< (ac-qlen ch) (async-chan-cap ch)))
                                               (and (fx=? (async-chan-cap ch) 0)
                                                    (> (async-chan-takew ch) 0)))))
                                      (cond
                                        ((not ready?)
                                         (async-chan-alt-putters-set! ch
                                           (append (async-chan-alt-putters ch) (list (cons h v))))
                                         (set! registered (cons (cons ch #t) registered))
                                         (ac-notify! ch)
                                         'registered)
                                        ((alt-claim! h)
                                         ;; ready? holds under this lock — the locked
                                         ;; give cannot return 'full here.
                                         (case (ac-try-give!/locked ch v)
                                           ((closed) (cons #f ch))
                                           (else (cons #t ch))))
                                        (else 'lost))))))
                            (cond
                              ((eq? res 'registered) (reg-loop (fx+ j 1)))
                              ((eq? res 'lost) (await))
                              (else (finish (car res) (cdr res)))))
                          ;; take from bare channel
                          (let* ((ch port)
                                 (res
                                  (jolt-with-mutex (async-chan-mu ch)
                                    (let ((ready?
                                           (or (not (ac-qempty? ch))
                                               (async-chan-closed? ch))))
                                      (cond
                                        ((not ready?)
                                         (async-chan-alt-takers-set! ch
                                           (append (async-chan-alt-takers ch) (list h)))
                                         (set! registered (cons (cons ch #f) registered))
                                         (ac-notify! ch)
                                         'registered)
                                        ((alt-claim! h)
                                         ;; ready? holds — the locked poll cannot
                                         ;; return the empty sentinel.
                                         (cons (ac-poll!/locked ch) ch))
                                        (else 'lost))))))
                            (cond
                              ((eq? res 'registered) (reg-loop (fx+ j 1)))
                              ((eq? res 'lost) (await))
                              (else (finish (car res) (cdr res)))))))))))))))

;; --- macros (expander fns over the reader forms) ----------------------------
;; go / go-loop are deliberately absent. They used to expand here, to a bare
;; (go-spawn (fn* [] body…)), and the overlay's defmacro then redefined the same
;; two vars — so which expansion a form got depended on whether the overlay had
;; been loaded, and the native pair was dead in every configuration that matters
;; (the loader drops clojure.core.async from loaded-ns precisely so a require
;; always pulls the overlay). One definition now, in the overlay, where the CPS
;; pass can reach &env / macroexpand / resolve.
(define cca-fn*-sym (jolt-symbol #f "fn*"))

;; (thread body...) — a real OS thread, ALWAYS: unlike go/go-loop it does NOT
;; honor *go-backend*, so a blocking body does not silently pin the fiber
;; carrier when a :fiber binding is in scope. thread is the documented escape
;; for blocking work (fibers-plan.md).
(define cca-thread-spawn-sym (jolt-symbol "clojure.core.async" "thread-spawn"))
(define (cca-thread-macro _form _env . body)
  (jolt-list cca-thread-spawn-sym (apply jolt-list cca-fn*-sym empty-pvec body)))

;; --- install clojure.core.async ---------------------------------------------
(define (cca-def! name v) (def-var! "clojure.core.async" name v))
(cca-def! "chan" jolt-async-chan)
(cca-def! "promise-chan" (lambda args (ac-make 1 'promise #f)))
(cca-def! "chan?" async-chan?)
(cca-def! "buffer" jolt-async-buffer)
(cca-def! "dropping-buffer" jolt-async-dropping-buffer)
(cca-def! "sliding-buffer" jolt-async-sliding-buffer)
(cca-def! "__promise-buffer" (lambda () (make-async-buffer 1 'promise)))
(cca-def! "unblocking-buffer?" jolt-async-unblocking-buffer?)
(cca-def! "close!" jolt-async-close!)
(cca-def! "<!" jolt-async-take)   (cca-def! "<!!" jolt-async-take)
(cca-def! ">!" jolt-async-give)   (cca-def! ">!!" jolt-async-give)
(cca-def! "timeout" jolt-async-timeout)
(cca-def! "put!" jolt-async-put!)
(cca-def! "take!" jolt-async-take!)
(cca-def! "offer!" jolt-async-offer!)
(cca-def! "go-spawn" async-go-spawn)
;; thread-spawn: always a real OS thread, whatever *go-backend* says (thread
;; is the documented escape; the go macro dispatches, thread must not).
(cca-def! "thread-spawn" async-go-spawn-thread)
;; fiber-spawn: always a fiber, whatever *go-backend* says — what the overlay's
;; io-thread / (thread-call f :io) spawns through.
(cca-def! "fiber-spawn" async-fiber-spawn)
;; non-blocking primitives also used by the Clojure overlay and external callers.
(cca-def! "__poll!" jolt-async-poll!)
(cca-def! "__offer!" jolt-async-offer!)
;; alts! entry point — handler-registration, not poll loop
(cca-def! "__do-alts" jolt-async-do-alts)
(cca-def! "thread" cca-thread-macro)   (mark-macro! "clojure.core.async" "thread")

;; go / go-loop are defined by the overlay, but the primitives above pre-seed this
;; namespace, so a bare (clojure.core.async/chan) resolves with no require and a
;; bare (clojure.core.async/go …) would report "No such var" from a namespace that
;; visibly exists. Reserve the two names with a stub that says what to do instead.
;; MARKED a macro, so it raises from the EXPANDER with the body still unevaluated.
;; As a plain fn the stub was reached as an ordinary call, which evaluates the body
;; as arguments first: (clojure.core.async/go (swap! a conj :x)) ran the swap! and
;; then reported that nothing had, and a body that parks —
;; (clojure.core.async/go (clojure.core.async/<! ch)) on an empty channel — blocked
;; forever instead of reporting anything at all, which is worse than the "No such
;; var" this exists to improve on. natives-reader.ss reserves `letfn` with an
;; unmarked fn, but that stub is unreachable (the analyzer lowers every letfn form
;; before any macro runs) and this one is not. The overlay's defmacro replaces both
;; roots and re-marks them.
;;
;; The message names a BARE require, not a :refer. Requiring the namespace at all
;; is what loads the overlay, and whoever reads this wrote the qualified call, so
;; telling them to refer the name asks them to rewrite a call site that is already
;; right. (require 'clojure.core.async) makes the very form that raised work.
(let ((needs-overlay
       (lambda (nm)
         (lambda args
           (jolt-throw
            (jolt-ex-info
             (string-append "clojure.core.async/" nm
                            " is defined by the clojure.core.async overlay: "
                            "(require 'clojure.core.async) first")
             (jolt-hash-map)))))))
  (cca-def! "go" (needs-overlay "go"))           (mark-macro! "clojure.core.async" "go")
  (cca-def! "go-loop" (needs-overlay "go-loop")) (mark-macro! "clojure.core.async" "go-loop"))

;; A channel is opaque, but it should still name itself: without these it fell to
;; the :object catch-all, so (class ch) was :object and pr printed #object[:object].
;; The tag is what print-method's :jolt/chan method (50-io.clj) dispatches on.
(register-type-arm! async-chan? (lambda (x) (keyword "jolt" "chan")))
(register-class-arm! async-chan?
  (lambda (x) "clojure.core.async.impl.channels.ManyToManyChannel"))
