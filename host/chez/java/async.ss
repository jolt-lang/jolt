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

;; --- channels ---------------------------------------------------------------
;; items: an amortized-O(1) FIFO held as a mutable #(out in len) — `out` is the
;; front (pop from its head), `in` holds pushed entries reversed onto it, `len` is
;; the count (an append-to-a-list FIFO is O(n) per push and O(n) to measure).
;; Each entry is (value . box); box is #f for a buffered value or a 1-slot vector
;; for an unbuffered rendezvous put (set #t when taken, waking the putter).
;; cap 0 + kind 'unbuffered = rendezvous; cap>0 with kind fixed/dropping/sliding.
;; takew counts threads parked in a blocking take (so a non-blocking offer! to an
;; unbuffered channel can tell a taker is waiting). xrf is the transducer reducing
;; fn (or #f); exh the ex-handler (or #f).
(define-record-type async-chan
  (fields mu cv (mutable items) cap kind (mutable closed?) (mutable xrf) (mutable takew) exh)
  (nongenerative async-chan-v2))

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

(define (ac-make cap kind xrf) (make-async-chan (make-mutex) (make-condition) (ac-qnew) cap kind #f xrf 0 #f))
(define (ac-make/exh cap kind exh) (make-async-chan (make-mutex) (make-condition) (ac-qnew) cap kind #f #f 0 exh))

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

;; close! (idempotent): mark closed, flush a stateful transducer's completion, and
;; wake everyone. ac-close! assumes the lock is held; the public form takes it.
(define (ac-close! ch)
  (unless (async-chan-closed? ch)
    (async-chan-closed?-set! ch #t)
    (when (async-chan-xrf ch)
      (guard (e (#t (async-report-uncaught! "transducer completion on close!" e)))
        (ac-xrf-apply ch)))
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
                      (ac-qpush! ch (cons v #f)) (condition-broadcast (async-chan-cv ch)))
                    #t)
         (else
          (if (> (async-chan-cap ch) 0)
              (let loop ()                                    ; buffered fixed: wait for room
                (cond ((async-chan-closed? ch) #f)
                      ((< (ac-qlen ch) (async-chan-cap ch))
                       (ac-qpush! ch (cons v #f)) (condition-broadcast (async-chan-cv ch)) #t)
                      (else (condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))
              (let ((box (vector #f)))                        ; unbuffered: rendezvous
                (ac-qpush! ch (cons v box))
                (condition-broadcast (async-chan-cv ch))
                (let loop ()
                  (cond ((vector-ref box 0) #t)
                        ((async-chan-closed? ch) #f)
                        (else (condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))))))))))

;; remove + return the head value, waking a parked rendezvous putter.
(define (ac-take-head! ch)
  (let* ((entry (ac-qpop! ch)) (v (car entry)) (box (cdr entry)))
    (when box (vector-set! box 0 #t))
    (condition-broadcast (async-chan-cv ch))
    v))

;; peek the front value without removing it (promise channels keep their value).
(define (ac-peek ch)
  (let ((q (async-chan-items ch)))
    (ac-qfront! q)
    (car (car (vector-ref q 0)))))

;; <! / <!! — take, blocking. Drains buffered values, then nil once closed + empty.
;; A promise channel PEEKS — its one value stays for every taker.
(define (jolt-async-take ch)
  (with-mutex (async-chan-mu ch)
    (let loop ()
      (cond ((eq? (async-chan-kind ch) 'promise)
             (cond ((not (ac-qempty? ch)) (ac-peek ch))
                   ((async-chan-closed? ch) jolt-nil)
                   (else (ac-take-wait ch) (loop))))
            ((not (ac-qempty? ch)) (ac-take-head! ch))
            ((async-chan-closed? ch) jolt-nil)
            (else (ac-take-wait ch) (loop))))))

;; park in a take, tracking the waiter count so a concurrent offer! to an
;; unbuffered channel can see that a taker is ready.
(define (ac-take-wait ch)
  (async-chan-takew-set! ch (fx+ 1 (async-chan-takew ch)))
  (condition-wait (async-chan-cv ch) (async-chan-mu ch))
  (async-chan-takew-set! ch (fx- (async-chan-takew ch) 1)))

;; non-blocking take for alts!/poll!: a value, jolt-nil (closed+empty), or ac-poll-empty.
(define ac-poll-empty (list 'empty))
(define (ac-poll! ch)
  (with-mutex (async-chan-mu ch)
    (cond ((and (eq? (async-chan-kind ch) 'promise) (not (ac-qempty? ch))) (ac-peek ch))
          ((not (ac-qempty? ch)) (ac-take-head! ch))
          ((async-chan-closed? ch) jolt-nil)
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
         ((promise) (when (ac-qempty? ch) (ac-qpush! ch (cons v #f))
                          (condition-broadcast (async-chan-cv ch))) 'ok)
         (else
          (cond
            ((> (async-chan-cap ch) 0)
             (if (< (ac-qlen ch) (async-chan-cap ch))
                 (begin (ac-qpush! ch (cons v #f)) (condition-broadcast (async-chan-cv ch)) 'ok)
                 'full))
            ;; unbuffered: only immediate if a taker is parked to receive it.
            ((> (async-chan-takew ch) 0)
             (let ((box (vector #f)))
               (ac-qpush! ch (cons v box))
               (condition-broadcast (async-chan-cv ch))
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
      ((ok) (if on-caller? (call-cb #t) (fork-thread (lambda () (call-cb #t)))) #t)
      ((closed) (if on-caller? (call-cb #f) (fork-thread (lambda () (call-cb #f)))) #f)
      (else (fork-thread (lambda () (call-cb (jolt-async-give ch v)))) #t))))

;; (take! ch cb [on-caller?]) — async take. Same on-caller? rule as put!.
(define (jolt-async-take! ch cb . rest)
  (let* ((on-caller? (if (pair? rest) (jolt-truthy? (car rest)) #t))
         (call-cb (lambda (v) (unless (jolt-nil? cb) (jolt-invoke cb v))))
         (r (ac-poll! ch)))
    (cond
      ((eq? r ac-poll-empty) (fork-thread (lambda () (call-cb (jolt-async-take ch)))))
      (on-caller? (call-cb r))
      (else (fork-thread (lambda () (call-cb r)))))
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
;; non-blocking primitives the Clojure overlay's do-alts polls over.
(cca-def! "__poll!" jolt-async-poll!)
(cca-def! "__offer!" jolt-async-offer!)
(cca-def! "go" cca-go-macro)           (mark-macro! "clojure.core.async" "go")
(cca-def! "go-loop" cca-go-loop-macro) (mark-macro! "clojure.core.async" "go-loop")
(cca-def! "thread" cca-thread-macro)   (mark-macro! "clojure.core.async" "thread")
