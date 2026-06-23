;; async.ss — clojure.core.async on real OS threads for the Chez host.
;;
;; A `go` block is an OS thread and a channel is a mutex+condition blocking
;; queue: <! / >! are the blocking <!! / >!! (they "park" by blocking the thread).
;; <! / >! work ANYWHERE — no CPS transform — because they are ordinary blocking
;; calls. Real parallelism, shared heap. Trade-off: one OS thread per go block
;; (fine for typical use, not for thousands of simultaneous go blocks).
;;
;; Channel: an unbuffered channel is a rendezvous (the putter blocks until its
;; value is taken); a buffered (chan n) put blocks only when full; dropping/sliding
;; buffers never block the putter. A transducer is applied on the put side.
;;
;; The fns are def-var!'d into clojure.core.async; go/go-loop/thread are macros
;; (mark-macro!) expanding to go-spawn. Loaded after
;; concurrency.ss (reuses ms->duration). Requires a threaded Chez build.

;; --- buffers ----------------------------------------------------------------
(define-record-type async-buffer (fields n kind) (nongenerative async-buffer-v1))
(define (jolt-async-buffer n)          (make-async-buffer n 'fixed))
(define (jolt-async-dropping-buffer n) (make-async-buffer n 'dropping))
(define (jolt-async-sliding-buffer n)  (make-async-buffer n 'sliding))

;; --- channels ---------------------------------------------------------------
;; items: an amortized-O(1) FIFO held as a mutable #(out in len) — `out` is the
;; front (pop from its head), `in` holds pushed entries reversed onto it, `len` is
;; the count (an append-to-a-list FIFO is O(n) per push and O(n) to measure).
;; Each entry is (value . box); box is #f for a buffered value or a 1-slot vector
;; for an unbuffered rendezvous put (set #t when taken, waking the putter).
;; cap 0 + kind 'unbuffered = rendezvous; cap>0 with kind fixed/dropping/sliding.
(define-record-type async-chan
  (fields mu cv (mutable items) cap kind (mutable closed?) (mutable xrf))
  (nongenerative async-chan-v1))

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

(define (ac-make cap kind xrf) (make-async-chan (make-mutex) (make-condition) (ac-qnew) cap kind #f xrf))

;; (chan) | (chan n) | (chan buf) | (chan n|buf xform)
(define (jolt-async-chan . args)
  (let ((buf (if (pair? args) (car args) jolt-nil))
        (xform (if (and (pair? args) (pair? (cdr args))) (cadr args) jolt-nil)))
    (let-values (((cap kind)
                  (cond ((async-buffer? buf) (values (async-buffer-n buf) (async-buffer-kind buf)))
                        ((and (number? buf) (> buf 0)) (values buf 'fixed))
                        (else (values 0 'unbuffered)))))
      (let ((ch (ac-make cap kind #f)))
        (unless (jolt-nil? xform)
          (async-chan-xrf-set! ch (jolt-invoke xform (ac-make-add-rf ch))))
        ch))))

;; close! (idempotent): mark closed, flush a stateful transducer's completion, and
;; wake everyone. ac-close! assumes the lock is held; the public form takes it.
(define (ac-close! ch)
  (unless (async-chan-closed? ch)
    (async-chan-closed?-set! ch #t)
    (when (async-chan-xrf ch) (guard (e (#t #f)) (jolt-invoke (async-chan-xrf ch) ch)))
    (condition-broadcast (async-chan-cv ch)))
  jolt-nil)
(define (jolt-async-close! ch) (with-mutex (async-chan-mu ch) (ac-close! ch)))

;; >! / >!! — put, blocking. false if closed; nil may not be put. With a
;; transducer the value is run through it (one put -> zero or more channel values);
;; a `reduced` result closes the channel.
(define (jolt-async-give ch v)
  (when (jolt-nil? v) (jolt-throw (jolt-ex-info "Can't put nil on a channel" (jolt-hash-map))))
  (with-mutex (async-chan-mu ch)
    (cond
      ((async-chan-closed? ch) #f)
      ((async-chan-xrf ch)
       (let ((r (jolt-invoke (async-chan-xrf ch) ch v)))
         (when (jolt-reduced? r) (ac-close! ch))
         #t))
      (else
       (case (async-chan-kind ch)
         ((dropping sliding) (ac-buf-give! ch v) #t)
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

;; <! / <!! — take, blocking. Drains buffered values, then nil once closed + empty.
(define (jolt-async-take ch)
  (with-mutex (async-chan-mu ch)
    (let loop ()
      (cond ((not (ac-qempty? ch)) (ac-take-head! ch))
            ((async-chan-closed? ch) jolt-nil)
            (else (condition-wait (async-chan-cv ch) (async-chan-mu ch)) (loop))))))

;; non-blocking take for alts!: a value, jolt-nil (closed+empty), or ac-poll-empty.
(define ac-poll-empty (list 'empty))
(define (ac-poll! ch)
  (with-mutex (async-chan-mu ch)
    (cond ((not (ac-qempty? ch)) (ac-take-head! ch))
          ((async-chan-closed? ch) jolt-nil)
          (else ac-poll-empty))))

;; (alts! [ch ...]) — take from whichever channel is ready first; returns
;; [value channel] (value nil if that channel closed). Take-only: every port must
;; be a channel — put specs [ch val] and the :default option are not supported, so
;; reject them with a clear error instead of crashing inside ac-poll!.
;; Polls with a 1ms backoff — no cross-channel wait-set yet.
(define ac-1ms (make-time 'time-duration 1000000 0))
(define (jolt-async-alts chans)
  (let ((cs (seq->list (jolt-seq chans))))
    (for-each (lambda (c)
                (unless (async-chan? c)
                  (jolt-throw (jolt-ex-info
                                "alts! supports channel ports only (put specs [ch val] and :default are not supported)"
                                (jolt-hash-map)))))
              cs)
    (let loop ()
      (let try ((rest cs))
        (if (null? rest)
            (begin (sleep ac-1ms) (loop))
            (let ((r (ac-poll! (car rest))))
              (if (eq? r ac-poll-empty)
                  (try (cdr rest))
                  (jolt-vector r (car rest)))))))))

;; (timeout ms) — a channel that closes after ms milliseconds.
(define (jolt-async-timeout ms)
  (let ((w (ac-make 0 'unbuffered #f)))
    (fork-thread (lambda () (sleep (ms->duration ms)) (jolt-async-close! w)))
    w))

;; (put! ch v [cb]) / (take! ch cb) — async put/take on a thread, optional callback.
(define (jolt-async-put! ch v . cb)
  (fork-thread (lambda ()
                 (let ((ok (jolt-async-give ch v)))
                   (when (and (pair? cb) (not (jolt-nil? (car cb)))) (jolt-invoke (car cb) ok)))))
  jolt-nil)
(define (jolt-async-take! ch cb)
  (fork-thread (lambda ()
                 (let ((v (jolt-async-take ch)))
                   (unless (jolt-nil? cb) (jolt-invoke cb v)))))
  jolt-nil)

;; (go-spawn thunk) — run thunk on a thread; return a buffered(1) channel that
;; conveys its value once then closes (a nil result just closes). Dynamic bindings
;; are conveyed (Chez inherits the thread-parameter at fork; we install explicitly).
(define (async-go-spawn thunk)
  (let ((w (ac-make 1 'fixed #f)) (snap (dyn-binding-stack)))
    (fork-thread
     (lambda ()
       (dyn-binding-stack snap)
       (let ((r (guard (e (#t (cons #f e))) (cons #t (jolt-invoke thunk)))))
         (when (and (car r) (not (jolt-nil? (cdr r)))) (jolt-async-give w (cdr r)))
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
(cca-def! "chan?" async-chan?)
(cca-def! "buffer" jolt-async-buffer)
(cca-def! "dropping-buffer" jolt-async-dropping-buffer)
(cca-def! "sliding-buffer" jolt-async-sliding-buffer)
(cca-def! "close!" jolt-async-close!)
(cca-def! "<!" jolt-async-take)   (cca-def! "<!!" jolt-async-take)
(cca-def! ">!" jolt-async-give)   (cca-def! ">!!" jolt-async-give)
(cca-def! "alts!" jolt-async-alts) (cca-def! "alts!!" jolt-async-alts)
(cca-def! "timeout" jolt-async-timeout)
(cca-def! "put!" jolt-async-put!)
(cca-def! "take!" jolt-async-take!)
(cca-def! "go-spawn" async-go-spawn)
(cca-def! "go" cca-go-macro)           (mark-macro! "clojure.core.async" "go")
(cca-def! "go-loop" cca-go-loop-macro) (mark-macro! "clojure.core.async" "go-loop")
(cca-def! "thread" cca-thread-macro)   (mark-macro! "clojure.core.async" "thread")
