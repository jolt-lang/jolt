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
;; go/go-loop/thread are macros (mark-macro!) expanding to go-spawn. Loaded after
;; concurrency.ss (reuses ms->duration). Requires a threaded Chez build.

;; --- buffers ----------------------------------------------------------------
(define-record-type async-buffer (fields n kind) (nongenerative async-buffer-v1))
(define (jolt-async-buffer n)          (make-async-buffer n 'fixed))
(define (jolt-async-dropping-buffer n) (make-async-buffer n 'dropping))
(define (jolt-async-sliding-buffer n)  (make-async-buffer n 'sliding))
(define (jolt-async-unblocking-buffer? b)
  (if (and (async-buffer? b) (memq (async-buffer-kind b) '(dropping sliding promise))) #t #f))

;; --- alt-handler (one per alts! call, shared across ports) -------------------
(define-record-type alt-handler
  (fields fmu (mutable active?) wmu wcv mailbox)
  (nongenerative alt-handler-v1))
(define (make-alt-handler)
  (make-alt-handler (make-mutex) #t (make-mutex) (make-condition) (vector #f #f #f)))

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
;; LOCK ORDER: channel mu → fmu → wmu. Never hold two channel mutexes at once.
(define (alt-claim! h)
  (with-mutex (alt-handler-fmu h)
    (and (alt-handler-active? h)
         (begin (alt-handler-active?-set! h #f) #t))))

;; alt-deliver! — call ONLY after alt-claim! returned #t.
(define (alt-deliver! h val port)
  (with-mutex (alt-handler-wmu h)
    (let ((mb (alt-handler-mailbox h)))
      (vector-set! mb 1 val) (vector-set! mb 2 port) (vector-set! mb 0 #t))
    (condition-signal (alt-handler-wcv h))))

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
    (guard (e (#t (if exh
                      (let ((else (jolt-invoke exh e)))
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
(define (jolt-async-close! ch) (with-mutex (async-chan-mu ch) (ac-close! ch)))

;; >! / >!! — put, blocking. false if closed; nil may not be put. With a
;; transducer the value is run through it (one put -> zero or more channel values);
;; a `reduced` result closes the channel.
(define (jolt-async-give ch v)
  (when (jolt-nil? v) (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException" "Can't put nil on a channel")))
  (with-mutex (async-chan-mu ch)
    (cond
      ((async-chan-closed? ch) #f)
      ((async-chan-xrf ch)
       (let ((r (ac-xrf-apply ch v)))
         (when (jolt-reduced? r) (ac-close! ch))
         #t))
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
                       (else (condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))
               (let ((box (vector #f)))                        ; unbuffered: rendezvous
                 (ac-qpush! ch (cons v box))
                 (ac-notify! ch)
                 (let loop ()
                  (cond ((vector-ref box 0) #t)
                        ((async-chan-closed? ch) #f)
                        (else (condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))))))))))

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
  (with-mutex (async-chan-mu ch)
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
  (condition-wait (async-chan-cv ch) (async-chan-mu ch))
  (async-chan-takew-set! ch (fx- (async-chan-takew ch) 1)))

;; non-blocking take for alts!/poll!: a value, jolt-nil (closed+empty), or ac-poll-empty.
;; Drains pending alt-putters when the queue is empty (same drain path as jolt-async-take).
(define ac-poll-empty (list 'empty))
(define (ac-poll! ch)
  (with-mutex (async-chan-mu ch)
    (cond ((and (eq? (async-chan-kind ch) 'promise) (not (ac-qempty? ch))) (ac-peek ch))
          ((not (ac-qempty? ch)) (ac-take-head! ch))
          ((async-chan-closed? ch) jolt-nil)
          ;; drain an alt-putter if parked (no xform chans)
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
                 ac-poll-empty)))  ; dead registration
          (else ac-poll-empty))))

;; non-blocking give: 'ok (accepted), 'full (would block), or 'closed.
(define (ac-try-give! ch v)
  (when (jolt-nil? v) (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException" "Can't put nil on a channel")))
  (with-mutex (async-chan-mu ch)
    (cond
      ((async-chan-closed? ch) 'closed)
      ((async-chan-xrf ch) (let ((r (ac-xrf-apply ch v)))
                             (when (jolt-reduced? r) (ac-close! ch)) 'ok))
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
            ;; unbuffered: only immediate if a taker is parked to receive it.
            ((> (async-chan-takew ch) 0)
             (let ((box (vector #f)))
               (ac-qpush! ch (cons v box))
               (ac-notify! ch)
               'ok))
            (else 'full))))))))

;; offer! / poll! — never block. offer! returns #t/#f(closed) on completion, nil if
;; it would block; poll! returns a value, nil (closed+empty), or the ::none sentinel.
(define cca-none (keyword "clojure.core.async" "none"))
(define (jolt-async-offer! ch v)
  (case (ac-try-give! ch v) ((ok) #t) ((closed) #f) (else jolt-nil)))
(define (jolt-async-poll! ch)
  (let ((r (ac-poll! ch))) (if (eq? r ac-poll-empty) cca-none r)))

;; (timeout ms) — a channel that closes after ms milliseconds.
(define (jolt-async-timeout ms)
  (let ((w (ac-make 0 'unbuffered #f)))
    (fork-thread (lambda () (sleep (ms->duration ms)) (jolt-async-close! w)))
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

(define (async-go-spawn thunk)
  (let ((w (ac-make 1 'fixed #f)) (snap (dyn-binding-stack)))
    (fork-thread
     (lambda ()
       (*txn* #f)                          ; go/thread body must not inherit parent's txn
       (dyn-binding-stack snap)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (if (car r)
             (when (not (jolt-nil? (cdr r))) (jolt-async-give w (cdr r)))
             (async-report-uncaught! "go/thread body (channel closed)" (cdr r)))
          (jolt-async-close! w))))
    w))

;; --- alts! entry point -------------------------------------------------------
;; (__do-alts ports priority?) — ports is a jolt vector of channels or [ch val]
;; put specs. Returns a jolt vector [val port]. priority? is a boolean: #t
;; starts scanning at index 0 (declared order); #f picks a random start.
;; LOCK ORDER: channel mu → fmu → wmu. Never hold two channel mutexes at once.
(define (jolt-async-do-alts ports priority?)
  (let* ((n (pvec-count ports))
         (start (if (jolt-truthy? priority?) 0 (random n)))
         (boxed-val (lambda (v) (if (jolt-nil? v) v (box v)))))
    ;; Normalize to a Scheme list of (ch . (box v)) for puts, (ch . #f) for takes.
    (let loop ((i 0) (acc '()))
      (if (fx=? i n)
          (let ((ops (reverse acc)))
            ;; FAST PASS: poll each op without registering a handler.
            (let fast-loop ((k 0))
              (if (fx=? k n)
                  ;; No fast hit — register a handler and wait.
                  (let* ((h (make-alt-handler))
                         (registered '()))
                    ;; REGISTRATION PASS with re-check under lock
                    (let reg-loop ((j 0))
                      (if (fx=? j n)
                          (begin
                            ;; WAIT
                            (with-mutex (alt-handler-wmu h)
                              (let ((mb (alt-handler-mailbox h)))
                                (let wait-loop ()
                                  (unless (vector-ref mb 0)
                                    (condition-wait (alt-handler-wcv h) (alt-handler-wmu h))
                                    (wait-loop)))))
                            ;; UNREGISTER from every channel
                            (for-each
                              (lambda (entry)
                                (let ((ch (car entry)) (is-put (cdr entry)))
                                  (with-mutex (async-chan-mu ch)
                                    (if is-put
                                        (async-chan-alt-putters-set! ch
                                          (remove (lambda (hp) (eq? (car hp) h))
                                                  (async-chan-alt-putters ch)))
                                        (async-chan-alt-takers-set! ch
                                          (remove (lambda (x) (eq? x h))
                                                  (async-chan-alt-takers ch)))))
                                  (ac-notify! ch)))
                              registered)
                            ;; Return result
                            (let ((mb (alt-handler-mailbox h)))
                              (jolt-vector (vector-ref mb 1) (vector-ref mb 2))))
                          (let* ((idx (let ((m (fx+ start j)))
                                        (if (fx<? m n) m (fx- m n))))
                                 (port (pvec-nth-d ports idx jolt-nil)))
                            (if (pvec? port)
                                ;; put spec [ch val]
                                (let ((ch (pvec-nth-d port 0 jolt-nil)) (v (pvec-nth-d port 1 jolt-nil)))
                                  (with-mutex (async-chan-mu ch)
                                    ;; re-check readiness under lock
                                    (case (ac-try-give! ch v)
                                      ((ok)
                                       (if (alt-claim! h)
                                           (jolt-vector #t ch)
                                           ;; concurrent deliver won — wait
                                           (begin (set! registered (cons (cons ch #t) registered))
                                                  (reg-loop (fx+ j 1)))))
                                      ((closed)
                                       (jolt-vector #f ch))
                                      (else
                                       ;; not ready — register
                                       (async-chan-alt-putters-set! ch
                                         (append (async-chan-alt-putters ch) (list (cons h (boxed-val v)))))
                                       (set! registered (cons (cons ch #t) registered))
                                       (reg-loop (fx+ j 1))))))
                                ;; take from bare channel
                                (let ((ch port))
                                  (with-mutex (async-chan-mu ch)
                                    (let ((r (ac-poll! ch)))
                                      (if (eq? r ac-poll-empty)
                                          ;; not ready — register
                                          (begin
                                            (async-chan-alt-takers-set! ch
                                              (append (async-chan-alt-takers ch) (list h)))
                                            (set! registered (cons (cons ch #f) registered))
                                            (reg-loop (fx+ j 1)))
                                          ;; ready — claim and return
                                          (if (alt-claim! h)
                                              (jolt-vector r ch)
                                              (begin
                                                (set! registered (cons (cons ch #f) registered))
                                                (reg-loop (fx+ j 1)))))))))))))
                  (let* ((idx (let ((m (fx+ start k)))
                                (if (fx<? m n) m (fx- m n))))
                         (port (pvec-nth-d ports idx jolt-nil)))
                    (if (pvec? port)
                        (let ((ch (pvec-nth-d port 0 jolt-nil)) (v (pvec-nth-d port 1 jolt-nil)))
                          (case (ac-try-give! ch v)
                            ((ok) (jolt-vector #t ch))
                            ((closed) (jolt-vector #f ch))
                            (else (fast-loop (fx+ k 1)))))
                        (let ((r (ac-poll! port)))
                          (if (eq? r ac-poll-empty)
                              (fast-loop (fx+ k 1))
                              (jolt-vector r port))))))))
          (let ((port (pvec-nth-d ports i jolt-nil)))
            (loop (fx+ i 1)
                  (if (pvec? port)
                      (let ((ch (pvec-nth-d port 0 jolt-nil)) (v (pvec-nth-d port 1 jolt-nil)))
                        (cons (cons ch (boxed-val v)) acc))
                      (cons (cons port #f) acc))))))))

;; --- macros (expander fns over the reader forms) ----------------------------
(define cca-go-spawn-sym (jolt-symbol "clojure.core.async" "go-spawn"))
(define cca-go-sym (jolt-symbol "clojure.core.async" "go"))
(define cca-fn*-sym (jolt-symbol #f "fn*"))
(define cca-loop-sym (jolt-symbol #f "loop"))

;; (go body...) -> (clojure.core.async/go-spawn (fn* [] body...))
(define (cca-go-macro . body)
  (jolt-list cca-go-spawn-sym (apply jolt-list cca-fn*-sym empty-pvec body)))
;; (go-loop bindings body...) -> (go (loop bindings body...))
(define (cca-go-loop-macro bindings . body)
  (jolt-list cca-go-sym (apply jolt-list cca-loop-sym bindings body)))
;; (thread body...) — a real OS thread (same shape as go here).
(define (cca-thread-macro . body)
  (jolt-list cca-go-spawn-sym (apply jolt-list cca-fn*-sym empty-pvec body)))

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
;; non-blocking primitives also used by the Clojure overlay and external callers.
(cca-def! "__poll!" jolt-async-poll!)
(cca-def! "__offer!" jolt-async-offer!)
;; alts! entry point — handler-registration, not poll loop
(cca-def! "__do-alts" jolt-async-do-alts)
(cca-def! "go" cca-go-macro)           (mark-macro! "clojure.core.async" "go")
(cca-def! "go-loop" cca-go-loop-macro) (mark-macro! "clojure.core.async" "go-loop")
(cca-def! "thread" cca-thread-macro)   (mark-macro! "clojure.core.async" "thread")
