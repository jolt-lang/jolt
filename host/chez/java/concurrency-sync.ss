;; concurrency-sync.ss — java.util.concurrent.Semaphore,
;; java.util.concurrent.locks.ReentrantReadWriteLock and
;; java.util.concurrent.CyclicBarrier (JDK 21 Semaphore.java,
;; ReentrantReadWriteLock.java, CyclicBarrier.java).
;;
;; Both are a little state under one bookkeeping mutex, waited on through
;; jolt-cv-wait (host/chez/locks.ss): a thread blocks on the condition, a fiber
;; registers on it and parks, and the decision is retaken on every wake. That is
;; the protocol CountDownLatch and the blocking queues already use, so neither
;; class holds a counted lock across a caller's body and a fiber that waits gives
;; its carrier up. Neither is a Chez mutex held from acquire to release: a lock
;; region is a USER body, and an OS mutex has thread granularity where a fiber
;; does not (see ReentrantLock in concurrency.ss for what that cost there).
;;
;; THE QUEUE. Every acquire that has to wait takes a TICKET, a fresh pair, and
;; puts it at the tail of the object's queue; it leaves when the acquire succeeds,
;; times out or is interrupted. The queue is what getQueueLength counts, and it is
;; what fairness reads: a fair acquire (and a reader behind a queued writer, which
;; the JDK's non-fair lock also holds back so writers are not starved) goes only
;; when nothing is ahead of it. An untimed tryAcquire / tryLock barges, as the
;; JDK's does, fair or not. A leaving ticket wakes the rest, since whoever was
;; behind it may now be first.
;;
;; A ticket that leaves by a throw (InterruptedException, a fiber interrupt) is
;; taken out by a guard around the wait. Not a dynamic-wind: a fiber park escapes
;; through the wait and comes back, and an after-thunk would dequeue the ticket on
;; every park.

(define (sync-ticket kind) (cons kind 'ticket))
(define (sync-ticket-kind t) (car t))
;; The queue without T, and whether anything is AHEAD of T (or, for a ticket not
;; queued yet, whether anything is queued at all).
(define (sync-queue-remove q t) (remq t q))
(define (sync-predecessor? q t) (and (pair? q) (not (eq? (car q) t))))

;; Run (wait ticket-box) and take the ticket out again if the wait throws. The
;; wait's decide queues the ticket the first time it has to wait and dequeues it
;; when it is done, both under MU, so only a throw can leave one behind.
(define (sync-with-ticket mu cv dequeue! wait)
  (let ((tb (box #f)))
    (guard (e (#t (when (unbox tb)
                    (jolt-with-mutex mu (dequeue! (unbox tb)) (jolt-cv-wake! cv)))
                  (raise e)))
      (wait tb))))

;; The wait itself: WHO names the operation for InterruptedException, MS is the
;; timeout in milliseconds or #f, INTERRUPTIBLE? whether a set flag throws.
(define (sync-wait who mu cv ms interruptible? decide)
  (if interruptible?
      (jolt-cv-wait-interruptibly who mu cv (and ms (ms->timeout ms)) decide)
      (jolt-cv-wait mu cv (and ms (ms->timeout ms)) decide)))

;; Thread.interrupted() then throw, the check an interruptible acquire makes
;; before it looks at the state at all, so a set flag throws even when the
;; acquire could have succeeded.
(define (sync-interrupt-check! who)
  (when (jolt-interrupt-take! (current-interrupt-box))
    (jolt-interrupted-throw! who)))

(define (sync-max-error! msg)
  (jolt-throw (jolt-host-throwable "java.lang.Error" msg)))

;; An int argument, as the compiled call casts it.
(define (sync-int-arg x) (jolt-int-cast (jnum->exact x)))

;; ============================================================================
;; java.util.concurrent.Semaphore
;; ============================================================================
;; State #(mu cv permits fair? queue). permits may be negative: the constructor
;; takes any int, and release adds to whatever is there.
(define sem-i-mu 0)
(define sem-i-cv 1)
(define sem-i-permits 2)
(define sem-i-fair 3)
(define sem-i-queue 4)
(define (make-semaphore permits fair?)
  (make-jhost "semaphore" (vector (make-mutex) (jolt-make-cv) permits fair? '())))
(define (sem-permits st) (vector-ref st sem-i-permits))
(define (sem-queue st) (vector-ref st sem-i-queue))

;; Can an acquire of N holding ticket T (or #f) take them now? A fair one only
;; with nobody ahead of it; BARGE? is the untimed tryAcquire, which never looks.
(define (sem-can-take? st t n barge?)
  (and (>= (sem-permits st) n)
       (or barge? (not (vector-ref st sem-i-fair))
           (not (sync-predecessor? (sem-queue st) t)))))
;; Take N, under mu. A ticket that is done leaves the queue, and if anyone is
;; still queued they are woken: permits may remain for the next one.
(define (sem-take! st t n)
  (vector-set! st sem-i-permits (- (sem-permits st) n))
  (when t
    (vector-set! st sem-i-queue (sync-queue-remove (sem-queue st) t))
    (when (pair? (sem-queue st)) (jolt-cv-wake! (vector-ref st sem-i-cv))))
  #t)

;; acquire / acquireUninterruptibly / tryAcquire(timeout): #t once N permits are
;; taken, #f when MS ran out first.
(define (sem-acquire! self n ms interruptible? who)
  (let* ((st (jhost-state self))
         (mu (vector-ref st sem-i-mu))
         (cv (vector-ref st sem-i-cv)))
    (when interruptible? (sync-interrupt-check! who))
    (or (jolt-with-mutex mu (and (sem-can-take? st #f n #f) (sem-take! st #f n)))
        (and (not (and ms (<= ms 0)))
             (sync-with-ticket mu cv
               (lambda (t) (vector-set! st sem-i-queue (sync-queue-remove (sem-queue st) t)))
               (lambda (tb)
                 (sync-wait who mu cv ms interruptible?
                   (lambda (timed-out?)
                     (let ((t (unbox tb)))
                       (cond
                         ((sem-can-take? st t n #f) (sem-take! st t n))
                         (timed-out?
                          (when t
                            (vector-set! st sem-i-queue (sync-queue-remove (sem-queue st) t))
                            (set-box! tb #f)
                            (jolt-cv-wake! cv))
                          #f)
                         (else
                          (unless t
                            (let ((t (sync-ticket 'shared)))
                              (set-box! tb t)
                              (vector-set! st sem-i-queue (append (sem-queue st) (list t)))))
                          jolt-cv-again)))))))))))

(define (sem-permit-arg x)
  (let ((n (sync-int-arg x)))
    (when (< n 0) (throw-jvm 'IllegalArgumentException jolt-nil))
    n))

(define (sem-release! self n)
  (let ((st (jhost-state self)))
    (jolt-with-mutex (vector-ref st sem-i-mu)
      (let ((next (+ (sem-permits st) n)))
        (when (> next 2147483647) (sync-max-error! "Maximum permit count exceeded"))
        (vector-set! st sem-i-permits next)
        (jolt-cv-wake! (vector-ref st sem-i-cv))))
    jolt-nil))

;; Semaphore(int permits) | Semaphore(int permits, boolean fair)
(define (semaphore-ctor . args)
  (unless (<= 1 (length args) 2)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.concurrent.Semaphore"))
  (make-semaphore (sync-int-arg (car args))
                  (and (pair? (cdr args)) (jolt-truthy? (cadr args)))))
(for-each (lambda (nm) (register-class-ctor! nm semaphore-ctor))
          '("Semaphore" "java.util.concurrent.Semaphore"))
(register-host-methods! "semaphore"
  (list
   ;; acquire() | acquire(int)
   (cons "acquire" (lambda (self . args)
     (let ((n (if (pair? args) (sem-permit-arg (car args)) 1)))
       (sem-acquire! self n #f #t "Semaphore.acquire")
       jolt-nil)))
   (cons "acquireUninterruptibly" (lambda (self . args)
     (let ((n (if (pair? args) (sem-permit-arg (car args)) 1)))
       (sem-acquire! self n #f #f "Semaphore.acquireUninterruptibly")
       jolt-nil)))
   ;; tryAcquire() | tryAcquire(int) barge and never wait; tryAcquire(timeout,
   ;; unit) | tryAcquire(int, timeout, unit) wait, in order when fair, and are
   ;; interruptible.
   (cons "tryAcquire" (lambda (self . args)
     (case (length args)
       ((0 1)
        (let ((n (if (pair? args) (sem-permit-arg (car args)) 1))
              (st (jhost-state self)))
          (jolt-with-mutex (vector-ref st sem-i-mu)
            (and (sem-can-take? st #f n #t) (sem-take! st #f n)))))
       ((2) (sem-acquire! self 1 (tu-args->ms args) #t "Semaphore.tryAcquire"))
       (else
        (let ((n (sem-permit-arg (car args))))
          (sem-acquire! self n (tu-args->ms (cdr args)) #t "Semaphore.tryAcquire"))))))
   (cons "release" (lambda (self . args)
     (sem-release! self (if (pair? args) (sem-permit-arg (car args)) 1))))
   (cons "availablePermits" (lambda (self) (sem-permits (jhost-state self))))
   ;; drainPermits: whatever is there, negative too, and leaves zero
   (cons "drainPermits" (lambda (self)
     (let ((st (jhost-state self)))
       (jolt-with-mutex (vector-ref st sem-i-mu)
         (let ((n (sem-permits st)))
           (vector-set! st sem-i-permits 0)
           n)))))
   (cons "isFair" (lambda (self) (vector-ref (jhost-state self) sem-i-fair)))
   (cons "hasQueuedThreads" (lambda (self) (pair? (sem-queue (jhost-state self)))))
   (cons "getQueueLength" (lambda (self) (length (sem-queue (jhost-state self)))))
   (cons "toString" (lambda (self)
     (string-append (host-object-tostring self) "[Permits = "
                    (number->string (sem-permits (jhost-state self))) "]")))))

;; ============================================================================
;; java.util.concurrent.locks.ReentrantReadWriteLock
;; ============================================================================
;; State #(mu cv writer wcount wbox wthread readers rtotal fair? queue rlock wlock)
;;   writer   the OWNER of the write lock — monitor-self, so the FIBER when a fiber
;;            holds it, else the thread's interrupt box — or #f
;;   wcount   its reentrant depth
;;   wbox     the OS thread box it was taken on (the terminal-fiber release below)
;;   wthread  the Thread it was taken on, for toString's owner name
;;   readers  one #(owner count box) per context holding the read lock
;;   rtotal   the read holds of all of them together (getReadLockCount)
(define rw-i-mu 0)
(define rw-i-cv 1)
(define rw-i-writer 2)
(define rw-i-wcount 3)
(define rw-i-wbox 4)
(define rw-i-wthread 5)
(define rw-i-readers 6)
(define rw-i-rtotal 7)
(define rw-i-fair 8)
(define rw-i-queue 9)
(define rw-i-rlock 10)
(define rw-i-wlock 11)
;; ReentrantReadWriteLock's hold counts are 16 bits each (SHARED_UNIT / MAX_COUNT)
(define rw-max-count 65535)

(define (make-rwlock fair?)
  (let* ((st (vector (make-mutex) (jolt-make-cv) #f 0 #f #f '() 0 fair? '() #f #f))
         (self (make-jhost "rw-lock" st)))
    (vector-set! st rw-i-rlock (make-jhost "rw-read-lock" (vector self)))
    (vector-set! st rw-i-wlock (make-jhost "rw-write-lock" (vector self)))
    self))
(define (rw-of lock) (vector-ref (jhost-state lock) 0))   ; a Read/WriteLock's lock
(define (rw-ref rw i) (vector-ref (jhost-state rw) i))
(define (rw-set! rw i v) (vector-set! (jhost-state rw) i v))

;; ME's reader entry, or #f.
(define (rw-reader rw me)
  (let loop ((rs (rw-ref rw rw-i-readers)))
    (cond ((null? rs) #f)
          ((eq? (vector-ref (car rs) 0) me) (car rs))
          (else (loop (cdr rs))))))

;; The release-side owner test, which is monitor-owner?'s (concurrency.ss) for the
;; same reason: a fiber's own unwind can arrive here off-fiber once the fiber is
;; terminal, and refusing it would leave the lock held for the life of the process.
;; Narrow: the owner is a done or dead fiber, it took the lock on THIS thread, and
;; no fiber is mounted now.
(define (rw-owner-releasing? owner box me)
  (or (eq? owner me)
      (and (jolt-fiber? owner)
           (not (jolt-current-fiber))
           (eq? box (current-os-thread-box))
           (memq (jolt-fiber-state owner) '(done dead))
           #t)))

;; Can ME take the write lock now? The writer itself always can (reentrancy);
;; anyone else needs it free of readers and writers, and, fair, nobody ahead.
;; BARGE? is the untimed tryLock. A reader asking for the write lock waits for
;; its own read hold too, as on the JDK: there is no upgrade.
(define (rw-write-ok? rw me t barge?)
  (let ((w (rw-ref rw rw-i-writer)))
    (or (eq? w me)
        (and (not w)
             (= 0 (rw-ref rw rw-i-rtotal))
             (or barge? (not (rw-ref rw rw-i-fair))
                 (not (sync-predecessor? (rw-ref rw rw-i-queue) t)))))))
;; Can ME take the read lock now? A reentrant reader, and the writer itself, always
;; can (the JDK lets both through whatever is queued: blocking them would
;; deadlock). Anyone else waits for a writer, and, fair, for anyone ahead; the
;; non-fair lock still holds a new reader back behind a queued WRITER at the head
;; of the queue, so a stream of readers cannot starve writers.
(define (rw-read-ok? rw me t barge?)
  (let ((w (rw-ref rw rw-i-writer)))
    (cond ((and w (not (eq? w me))) #f)
          ((or (eq? w me) (rw-reader rw me)) #t)
          (barge? #t)
          (else
           (let ((q (rw-ref rw rw-i-queue)))
             (not (if (rw-ref rw rw-i-fair)
                      (sync-predecessor? q t)
                      (and (sync-predecessor? q t) (eq? 'exclusive (sync-ticket-kind (car q)))))))))))

;; Take it, under mu; both check the 16-bit count first, as the JDK does.
(define (rw-take-write! rw me t)
  (if (eq? (rw-ref rw rw-i-writer) me)
      (begin
        (when (>= (rw-ref rw rw-i-wcount) rw-max-count) (sync-max-error! "Maximum lock count exceeded"))
        (rw-set! rw rw-i-wcount (+ 1 (rw-ref rw rw-i-wcount))))
      (begin
        (rw-set! rw rw-i-writer me)
        (rw-set! rw rw-i-wcount 1)
        (rw-set! rw rw-i-wbox (current-os-thread-box))
        (rw-set! rw rw-i-wthread (current-thread-handle))))
  (rw-dequeue! rw t)
  #t)
(define (rw-take-read! rw me t)
  (when (>= (rw-ref rw rw-i-rtotal) rw-max-count) (sync-max-error! "Maximum lock count exceeded"))
  (let ((e (rw-reader rw me)))
    (if e
        (vector-set! e 1 (+ 1 (vector-ref e 1)))
        (rw-set! rw rw-i-readers (cons (vector me 1 (current-os-thread-box)) (rw-ref rw rw-i-readers)))))
  (rw-set! rw rw-i-rtotal (+ 1 (rw-ref rw rw-i-rtotal)))
  (rw-dequeue! rw t)
  #t)
;; A finished ticket leaves the queue and wakes the rest: a reader behind it may
;; go now too, as the JDK propagates a shared acquire down the queue.
(define (rw-dequeue! rw t)
  (when t
    (rw-set! rw rw-i-queue (sync-queue-remove (rw-ref rw rw-i-queue) t))
    (when (pair? (rw-ref rw rw-i-queue)) (jolt-cv-wake! (rw-ref rw rw-i-cv)))))

;; One acquire, of either kind. KIND is 'exclusive or 'shared; MS a timeout or #f.
(define (rw-acquire! rw kind ms interruptible? who)
  (let* ((mu (rw-ref rw rw-i-mu))
         (cv (rw-ref rw rw-i-cv))
         (me (monitor-self))
         (ok? (if (eq? kind 'exclusive) rw-write-ok? rw-read-ok?))
         (take! (if (eq? kind 'exclusive) rw-take-write! rw-take-read!)))
    (when interruptible? (sync-interrupt-check! who))
    (or (jolt-with-mutex mu (and (ok? rw me #f #f) (take! rw me #f)))
        (and (not (and ms (<= ms 0)))
             (sync-with-ticket mu cv
               (lambda (t) (rw-set! rw rw-i-queue (sync-queue-remove (rw-ref rw rw-i-queue) t)))
               (lambda (tb)
                 (sync-wait who mu cv ms interruptible?
                   (lambda (timed-out?)
                     (let ((t (unbox tb)))
                       (cond
                         ((ok? rw me t #f) (take! rw me t))
                         (timed-out?
                          (when t
                            (rw-set! rw rw-i-queue (sync-queue-remove (rw-ref rw rw-i-queue) t))
                            (set-box! tb #f)
                            (jolt-cv-wake! cv))
                          #f)
                         (else
                          (unless t
                            (let ((t (sync-ticket kind)))
                              (set-box! tb t)
                              (rw-set! rw rw-i-queue (append (rw-ref rw rw-i-queue) (list t)))))
                          jolt-cv-again)))))))))))

;; tryLock(): barge, never wait.
(define (rw-try! rw kind)
  (let ((me (monitor-self)))
    (jolt-with-mutex (rw-ref rw rw-i-mu)
      (if (eq? kind 'exclusive)
          (and (rw-write-ok? rw me #f #t) (rw-take-write! rw me #f))
          (and (rw-read-ok? rw me #f #t) (rw-take-read! rw me #f))))))

(define (rw-unlock-write! rw)
  (let ((me (monitor-self)))
    (jolt-with-mutex (rw-ref rw rw-i-mu)
      (unless (and (rw-ref rw rw-i-writer)
                   (rw-owner-releasing? (rw-ref rw rw-i-writer) (rw-ref rw rw-i-wbox) me))
        (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException" jolt-nil)))
      (let ((n (- (rw-ref rw rw-i-wcount) 1)))
        (rw-set! rw rw-i-wcount n)
        (when (= n 0)
          (rw-set! rw rw-i-writer #f)
          (rw-set! rw rw-i-wbox #f)
          (rw-set! rw rw-i-wthread #f)
          (jolt-cv-wake! (rw-ref rw rw-i-cv)))))
    jolt-nil))
(define (rw-unlock-read! rw)
  (let ((me (monitor-self)))
    (jolt-with-mutex (rw-ref rw rw-i-mu)
      (let ((e (or (rw-reader rw me)
                   (find (lambda (e) (rw-owner-releasing? (vector-ref e 0) (vector-ref e 2) me))
                         (rw-ref rw rw-i-readers)))))
        (unless e
          (jolt-throw (jolt-host-throwable "java.lang.IllegalMonitorStateException"
                                           "attempt to unlock read lock, not locked by current thread")))
        (vector-set! e 1 (- (vector-ref e 1) 1))
        (when (= 0 (vector-ref e 1))
          (rw-set! rw rw-i-readers (remq e (rw-ref rw rw-i-readers))))
        (rw-set! rw rw-i-rtotal (- (rw-ref rw rw-i-rtotal) 1))
        (when (= 0 (rw-ref rw rw-i-rtotal))
          (jolt-cv-wake! (rw-ref rw rw-i-cv)))))
    jolt-nil))

;; The queries. "Current thread" is monitor-self, so a fiber is its own thread
;; here, as a virtual thread is on the JVM.
(define (rw-write-held-by-me? rw)
  (let ((w (rw-ref rw rw-i-writer))) (and w (eq? w (monitor-self)))))
(define (rw-write-hold-count rw)
  (if (rw-write-held-by-me? rw) (rw-ref rw rw-i-wcount) 0))
(define (rw-read-hold-count rw)
  (let ((e (jolt-with-mutex (rw-ref rw rw-i-mu) (rw-reader rw (monitor-self)))))
    (if e (vector-ref e 1) 0)))
(define (rw-write-count rw) (if (rw-ref rw rw-i-writer) (rw-ref rw rw-i-wcount) 0))
(define (rw-write-lock-tostring self)
  (let* ((rw (rw-of self))
         (owner (jolt-with-mutex (rw-ref rw rw-i-mu)
                  (and (rw-ref rw rw-i-writer) (rw-ref rw rw-i-wthread)))))
    (string-append (host-object-tostring self)
                   (if owner
                       (string-append "[Locked by thread "
                                      (jolt-str-render-one (record-method-dispatch owner "getName" jolt-nil))
                                      "]")
                       "[Unlocked]"))))

(define (rwlock-ctor . args)
  (unless (<= (length args) 1)
    (throw-jvm 'IllegalArgumentException
               "No matching ctor found for class java.util.concurrent.locks.ReentrantReadWriteLock"))
  (make-rwlock (and (pair? args) (jolt-truthy? (car args)))))
(for-each (lambda (nm) (register-class-ctor! nm rwlock-ctor))
          '("ReentrantReadWriteLock" "java.util.concurrent.locks.ReentrantReadWriteLock"))

;; The four Lock members both views share, over KIND.
(define (rw-lock-members kind)
  (list
   (cons "lock" (lambda (self)
     (rw-acquire! (rw-of self) kind #f #f "ReentrantReadWriteLock.lock")
     jolt-nil))
   (cons "lockInterruptibly" (lambda (self)
     (rw-acquire! (rw-of self) kind #f #t "ReentrantReadWriteLock.lockInterruptibly")
     jolt-nil))
   ;; tryLock() barges; tryLock(timeout, unit) waits its turn and is interruptible
   (cons "tryLock" (lambda (self . args)
     (if (null? args)
         (rw-try! (rw-of self) kind)
         (rw-acquire! (rw-of self) kind (tu-args->ms args) #t "ReentrantReadWriteLock.tryLock"))))
   (cons "unlock" (lambda (self)
     (if (eq? kind 'exclusive) (rw-unlock-write! (rw-of self)) (rw-unlock-read! (rw-of self)))))))

(register-host-methods! "rw-read-lock"
  (append
   (rw-lock-members 'shared)
   (list
    (cons "newCondition" (lambda (self) (throw-jvm 'UnsupportedOperationException jolt-nil)))
    (cons "toString" (lambda (self)
      (string-append (host-object-tostring self) "[Read locks = "
                     (number->string (rw-ref (rw-of self) rw-i-rtotal)) "]"))))))
(register-host-methods! "rw-write-lock"
  (append
   (rw-lock-members 'exclusive)
   (list
    (cons "isHeldByCurrentThread" (lambda (self) (rw-write-held-by-me? (rw-of self))))
    (cons "getHoldCount" (lambda (self) (rw-write-hold-count (rw-of self))))
    (cons "toString" rw-write-lock-tostring))))
(register-host-methods! "rw-lock"
  (list
   (cons "readLock" (lambda (self) (rw-ref self rw-i-rlock)))
   (cons "writeLock" (lambda (self) (rw-ref self rw-i-wlock)))
   (cons "isFair" (lambda (self) (rw-ref self rw-i-fair)))
   (cons "getReadLockCount" (lambda (self) (rw-ref self rw-i-rtotal)))
   (cons "getReadHoldCount" rw-read-hold-count)
   (cons "isWriteLocked" (lambda (self) (and (rw-ref self rw-i-writer) #t)))
   (cons "isWriteLockedByCurrentThread" rw-write-held-by-me?)
   (cons "getWriteHoldCount" rw-write-hold-count)
   (cons "hasQueuedThreads" (lambda (self) (pair? (rw-ref self rw-i-queue))))
   (cons "getQueueLength" (lambda (self) (length (rw-ref self rw-i-queue))))
   (cons "toString" (lambda (self)
     (let-values (((w r) (jolt-with-mutex (rw-ref self rw-i-mu)
                           (values (rw-write-count self) (rw-ref self rw-i-rtotal)))))
       (string-append (host-object-tostring self)
                      "[Write locks = " (number->string w)
                      ", Read locks = " (number->string r) "]"))))))

;; ============================================================================
;; java.util.concurrent.CyclicBarrier
;; ============================================================================
;; State #(mu cv parties action gen count). GEN is the current generation, a
;; fresh #(tripped? broken? tripping?) per trip or reset, so a waiter woken after
;; its generation tripped returns normally even when a later generation has
;; already broken. COUNT is the parties still to arrive in it.
;;
;; The JDK runs the barrier action with its lock held, and every other entry
;; (await, reset, isBroken, getNumberWaiting) takes that lock first. Here the
;; action is user code, so the last arrival marks the generation TRIPPING under mu
;; and runs the action with mu released. While a generation is tripping:
;;   - arrivals, reset, isBroken and getNumberWaiting wait for it to finish,
;;     uninterruptibly, which is what blocking on lock() does;
;;   - a waiter whose timeout runs out, or that is interrupted, waits for the
;;     outcome instead of breaking the barrier. It returns its index (and keeps
;;     its interrupt flag, as the JDK re-sets it) or gets BrokenBarrierException
;;     when the action threw.
;; The generation tripped? flag is set once the action has returned, so nobody is
;; released before it has run. The one thing the JDK's reentrant lock allows and
;; this does not: an action that calls await() on its own barrier deadlocks here
;; where the JDK lets it in. That is pathological and not handled.
(define cb-i-mu 0)
(define cb-i-cv 1)
(define cb-i-parties 2)
(define cb-i-action 3)
(define cb-i-gen 4)
(define cb-i-count 5)
(define (cb-ref st i) (vector-ref st i))
(define (cb-gen-tripped? g) (vector-ref g 0))
(define (cb-gen-broken? g) (vector-ref g 1))
(define (cb-gen-tripping? g) (vector-ref g 2))
(define (cb-new-gen! st)
  (vector-set! st cb-i-gen (vector #f #f #f))
  (vector-set! st cb-i-count (cb-ref st cb-i-parties)))
;; Break generation G, if it is still open, and wake everyone waiting in it. The
;; barrier starts a fresh generation only on a trip or reset(), so a broken
;; current generation stays broken until reset.
(define (cb-break! st g)
  (unless (or (cb-gen-tripped? g) (cb-gen-broken? g))
    (vector-set! g 1 #t)
    (when (eq? g (cb-ref st cb-i-gen))
      (vector-set! st cb-i-count (cb-ref st cb-i-parties)))
    (jolt-cv-wake! (cb-ref st cb-i-cv))))
(define (cb-throw! name)
  (jolt-throw (jolt-host-throwable name jolt-nil)))
(define (cb-bbe!) (cb-throw! "java.util.concurrent.BrokenBarrierException"))

;; mu NOT held (jolt-cv-wait takes it): wait, uninterruptibly, until no
;; generation is tripping.
(define (cb-wait-idle! st)
  (jolt-cv-wait (cb-ref st cb-i-mu) (cb-ref st cb-i-cv) #f
    (lambda (_) (if (cb-gen-tripping? (cb-ref st cb-i-gen)) jolt-cv-again #t))))
;; mu NOT held: wait, uninterruptibly, until G has tripped or broken.
(define (cb-wait-outcome! st g)
  (jolt-cv-wait (cb-ref st cb-i-mu) (cb-ref st cb-i-cv) #f
    (lambda (_) (if (or (cb-gen-tripped? g) (cb-gen-broken? g)) #t jolt-cv-again))))
;; Run (f) under mu once no generation is tripping, the JDK's lock(). F must not
;; answer 'busy.
(define (cb-with-lock st f)
  (let retry ()
    (let ((r (jolt-with-mutex (cb-ref st cb-i-mu)
               (if (cb-gen-tripping? (cb-ref st cb-i-gen)) 'busy (f)))))
      (if (eq? r 'busy)
          (begin (cb-wait-idle! st) (retry))
          r))))

(define (cb-interrupted-exception? e)
  (let ((v (jolt-unwrap-throw e)))
    (and (jolt-ex-info-record? v)
         (equal? (jolt-ex-info-record-class-name v) "java.lang.InterruptedException"))))

;; A waiter of G in the middle of a trip, interrupted or timed out: wait for the
;; outcome. FLAG? re-sets the interrupt flag, as the JDK does when it swallows the
;; interrupt of a generation that is already decided.
(define (cb-finish-waiter st g index flag?)
  (cb-wait-outcome! st g)
  (when flag? (set-box! (current-interrupt-box) #t))
  (if (cb-gen-broken? g) 'broken index))

;; await() / await(timeout, unit): the arrival index, parties-1 for the first
;; arrival and 0 for the last.
(define (cb-await! self ms)
  (let* ((st (jhost-state self))
         (mu (cb-ref st cb-i-mu))
         (cv (cb-ref st cb-i-cv))
         (who "CyclicBarrier.await")
         (r (cb-with-lock st
              (lambda ()
                (let ((g (cb-ref st cb-i-gen)))
                  (cond
                    ((cb-gen-broken? g) 'broken)
                    ((jolt-interrupt-take! (current-interrupt-box))
                     (cb-break! st g) 'interrupted)
                    (else
                     (let ((index (- (cb-ref st cb-i-count) 1)))
                       (vector-set! st cb-i-count index)
                       (cond ((= index 0) (vector-set! g 2 #t) (list 'trip g))
                             ((and ms (<= ms 0)) (cb-break! st g) 'timeout)
                             (else (list 'wait g index)))))))))))
    (cond
      ((eq? r 'broken) (cb-bbe!))
      ((eq? r 'interrupted) (jolt-interrupted-throw! who))
      ((eq? r 'timeout) (cb-throw! "java.util.concurrent.TimeoutException"))
      ((eq? (car r) 'trip)
       (let ((g (cadr r))
             (action (cb-ref st cb-i-action)))
         (when action
           (guard (e (#t (jolt-with-mutex mu (vector-set! g 2 #f) (cb-break! st g))
                         (raise e)))
             ((runnable->thunk action))))
         (jolt-with-mutex mu
           (vector-set! g 2 #f)
           (vector-set! g 0 #t)
           (cb-new-gen! st)
           (jolt-cv-wake! cv))
         0))
      (else
       (let* ((g (cadr r))
              (index (caddr r))
              (res (guard (e (#t
                              (let ((act (jolt-with-mutex mu
                                           (cond ((cb-interrupted-exception? e)
                                                  (if (or (cb-gen-tripping? g) (cb-gen-tripped? g)
                                                          (cb-gen-broken? g))
                                                      'defer
                                                      (begin (cb-break! st g) 'raise)))
                                                 (else
                                                  (unless (cb-gen-tripping? g) (cb-break! st g))
                                                  'raise)))))
                                (if (eq? act 'defer)
                                    (cb-finish-waiter st g index #t)
                                    (raise e)))))
                     (let ((r (sync-wait who mu cv ms #t
                                (lambda (timed-out?)
                                  (cond ((cb-gen-broken? g) 'broken)
                                        ((cb-gen-tripped? g) index)
                                        (timed-out?
                                         (if (cb-gen-tripping? g)
                                             'defer
                                             (begin (cb-break! st g) 'timeout)))
                                        (else jolt-cv-again))))))
                       (if (eq? r 'defer) (cb-finish-waiter st g index #f) r)))))
         (cond ((eq? res 'broken) (cb-bbe!))
               ((eq? res 'timeout) (cb-throw! "java.util.concurrent.TimeoutException"))
               (else res)))))))

;; The action must be a Runnable: a fn, a reify or deftype with run (or call), a
;; FutureTask. Anything else is the JVM's ClassCastException at construction.
(define (cb-check-action! x)
  (unless (or (procedure? x) (future-task? x)
              (iface-method x "run" 1) (iface-method x "call" 1)
              (let ((r (instance-check (jolt-symbol #f "java.lang.Runnable") x)))
                (and r (not (jolt-nil? r)))))
    (jolt-throw
     (jolt-host-throwable "java.lang.ClassCastException"
       (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                      " cannot be cast to class java.lang.Runnable")))))

;; CyclicBarrier(int parties) | CyclicBarrier(int parties, Runnable action)
(define (cyclic-barrier-ctor . args)
  (unless (<= 1 (length args) 2)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.concurrent.CyclicBarrier"))
  (let ((n (sync-int-arg (car args)))
        (action (and (pair? (cdr args)) (not (eq? (cadr args) jolt-nil)) (cadr args))))
    (when (<= n 0) (throw-jvm 'IllegalArgumentException jolt-nil))
    (when action (cb-check-action! action))
    (make-jhost "cyclic-barrier"
                (vector (make-mutex) (jolt-make-cv) n action (vector #f #f #f) n))))
(for-each (lambda (nm) (register-class-ctor! nm cyclic-barrier-ctor))
          '("CyclicBarrier" "java.util.concurrent.CyclicBarrier"))
(register-host-methods! "cyclic-barrier"
  (list
   (cons "await" (lambda (self . args) (cb-await! self (tu-args->ms args))))
   (cons "getParties" (lambda (self) (cb-ref (jhost-state self) cb-i-parties)))
   (cons "getNumberWaiting" (lambda (self)
     (let ((st (jhost-state self)))
       (cb-with-lock st (lambda () (- (cb-ref st cb-i-parties) (cb-ref st cb-i-count)))))))
   (cons "isBroken" (lambda (self)
     (let ((st (jhost-state self)))
       (cb-with-lock st (lambda () (cb-gen-broken? (cb-ref st cb-i-gen)))))))
   ;; reset: break the current generation, then start a fresh one
   (cons "reset" (lambda (self)
     (let ((st (jhost-state self)))
       (cb-with-lock st (lambda ()
         (cb-break! st (cb-ref st cb-i-gen))
         (cb-new-gen! st)
         #t)))
     jolt-nil))))

;; --- class rows ---------------------------------------------------------------
(for-each (lambda (p) (hashtable-set! jhost-tag->fqn (car p) (cdr p)))
          '(("semaphore" . "java.util.concurrent.Semaphore")
            ("cyclic-barrier" . "java.util.concurrent.CyclicBarrier")
            ("rw-lock" . "java.util.concurrent.locks.ReentrantReadWriteLock")
            ("rw-read-lock" . "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock")
            ("rw-write-lock" . "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock")))
(jch-register-supers! "java.util.concurrent.Semaphore" '("java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.CyclicBarrier" '())
(jch-register-supers! "java.util.concurrent.locks.ReentrantReadWriteLock"
                      '("java.util.concurrent.locks.ReadWriteLock" "java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                      '("java.util.concurrent.locks.Lock" "java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"
                      '("java.util.concurrent.locks.Lock" "java.io.Serializable"))
