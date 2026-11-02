;; concurrency-sync.ss — java.util.concurrent.Semaphore and
;; java.util.concurrent.locks.ReentrantReadWriteLock (JDK 21 Semaphore.java,
;; ReentrantReadWriteLock.java).
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

;; --- class rows ---------------------------------------------------------------
(for-each (lambda (p) (hashtable-set! jhost-tag->fqn (car p) (cdr p)))
          '(("semaphore" . "java.util.concurrent.Semaphore")
            ("rw-lock" . "java.util.concurrent.locks.ReentrantReadWriteLock")
            ("rw-read-lock" . "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock")
            ("rw-write-lock" . "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock")))
(jch-register-supers! "java.util.concurrent.Semaphore" '("java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.locks.ReentrantReadWriteLock"
                      '("java.util.concurrent.locks.ReadWriteLock" "java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                      '("java.util.concurrent.locks.Lock" "java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"
                      '("java.util.concurrent.locks.Lock" "java.io.Serializable"))
