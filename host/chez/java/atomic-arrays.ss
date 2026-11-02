;; atomic-arrays.ss — java.util.concurrent.atomic.AtomicIntegerArray and
;; AtomicLongArray (JDK 21 AtomicIntegerArray.java, AtomicLongArray.java).
;;
;; The array counterpart of the scalar atomics in host-static-classes.ss, and
;; built the same way: a Scheme vector guarded by one mutex, every element an
;; exact integer of the class's width. The read-modify-write members hold the
;; mutex only across the read and the write; updateAndGet and its kin run the
;; caller's function OUTSIDE it, in a compare-and-set retry loop, so the function
;; may touch the same array (a mutex-held call deadlocked the scalar atomics on
;; exactly that, PSL R4 cluster 4). Arithmetic wraps as Java's does; a value
;; handed in is cast to the element width at the call, as the compiled call
;; casts it.
;;
;; The JDK's memory-order variants (getPlain/getOpaque/getAcquire, setPlain/
;; setOpaque/setRelease/lazySet, the weakCompareAndSet* family and
;; compareAndExchange*) are the strong operations here: every access is already
;; ordered by the mutex or by the release barrier of a plain set, which is
;; stronger than each of them promises.

;; State #(vector mutex kind), kind 'integer or 'long.
(define (atomic-array-tag kind) (if (eq? kind 'long) "atomic-long-array" "atomic-integer-array"))
(define (aa-vec self) (vector-ref (jhost-state self) 0))
(define (aa-mutex self) (vector-ref (jhost-state self) 1))
(define (aa-kind self) (vector-ref (jhost-state self) 2))
(define (aa-cast self x)
  (if (eq? (aa-kind self) 'long) (jolt-long-cast x) (jolt-int-cast x)))
(define (aa-wrap self x)
  (if (eq? (aa-kind self) 'long) (jolt-wrap64 x) (jolt-unchecked-int x)))
;; IntUnaryOperator.applyAsInt / LongUnaryOperator.applyAsLong, and the binary
;; operators' method of the same name.
(define (aa-fi-method self) (if (eq? (aa-kind self) 'long) "applyAsLong" "applyAsInt"))

;; The index, checked as the VarHandle checks it.
(define (aa-index self i)
  (let ((i (jolt-int-cast (jnum->exact i)))
        (n (vector-length (aa-vec self))))
    (unless (and (>= i 0) (< i n))
      (throw-jvm 'ArrayIndexOutOfBoundsException
                 (string-append "Index " (number->string i) " out of bounds for length "
                                (number->string n))))
    i))

;; AtomicIntegerArray(int length) | AtomicIntegerArray(int[] array), and the long
;; pair: a fresh array of zeros, or a copy of the given one.
(define (atomic-array-ctor kind)
  (lambda args
    (unless (= (length args) 1)
      (throw-jvm 'IllegalArgumentException
                 (string-append "No matching ctor found for class java.util.concurrent.atomic."
                                (if (eq? kind 'long) "AtomicLongArray" "AtomicIntegerArray"))))
    (let* ((x (car args))
           (cast (if (eq? kind 'long) jolt-long-cast jolt-int-cast))
           (v (cond
                ((number? x)
                 (let ((n (jolt-int-cast (jnum->exact x))))
                   (when (< n 0) (throw-jvm 'NegativeArraySizeException (number->string n)))
                   (make-vector n 0)))
                ((jolt-nil? x) (throw-jvm 'NullPointerException jolt-nil))
                ((jolt-array? x)
                 (let* ((n (ja-len x)) (v (make-vector n 0)))
                   (do ((i 0 (fx+ i 1))) ((fx= i n) v)
                     (vector-set! v i (cast (ja-ref x i))))))
                (else
                 (throw-jvm 'ClassCastException
                            (string-append "class " (jolt-class-name x) " cannot be cast to class "
                                           (if (eq? kind 'long) "[J" "[I")))))))
      (make-jhost (atomic-array-tag kind) (vector v (make-mutex) kind)))))

;; Under the mutex: element I becomes (F old), and the answer is old or new.
(define (aa-transition! self i f return-old?)
  (jolt-with-mutex (aa-mutex self)
    (let* ((v (aa-vec self))
           (old (vector-ref v i))
           (new (f old)))
      (vector-set! v i new)
      (if return-old? old new))))
(define (aa-add! self i d return-old?)
  (let ((i (aa-index self i)))
    (aa-transition! self i (lambda (old) (aa-wrap self (+ old d))) return-old?)))
;; compareAndSet: the old value when it was EXPECT (and is now NEW), else the
;; value that is there — compareAndExchange's answer; compareAndSet tests it.
(define (aa-exchange! self i expect new)
  (jolt-with-mutex (aa-mutex self)
    (let* ((v (aa-vec self)) (cur (vector-ref v i)))
      (when (= cur expect) (vector-set! v i new))
      cur)))
(define (aa-cas! self i expect new)
  (= expect (aa-exchange! self i expect new)))
;; updateAndGet & co: F runs outside the mutex and is retried until the element
;; it saw is still there.
(define (aa-update! self i f return-old?)
  (let loop ((old (vector-ref (aa-vec self) i)))
    (let ((new (aa-cast self (f old))))
      (if (aa-cas! self i old new)
          (if return-old? old new)
          (loop (vector-ref (aa-vec self) i))))))

(define (aa-get self i) (vector-ref (aa-vec self) (aa-index self i)))
(define (aa-set! self i x)
  (let ((i (aa-index self i)) (x (aa-cast self x)))
    ;; a volatile write: what was built before it is visible to a get elsewhere
    (memory-order-release)
    (vector-set! (aa-vec self) i x)
    jolt-nil))
(define (aa-tostring self)
  (let ((v (aa-vec self)))
    (let loop ((i 0) (acc '()))
      (if (fx= i (vector-length v))
          (string-append "[" (apply string-append (reverse acc)) "]")
          (loop (fx+ i 1)
                (cons (string-append (if (fx= i 0) "" ", ") (number->string (vector-ref v i))) acc))))))

(let* ((getter (lambda (self i) (aa-get self i)))
       (setter (lambda (self i x) (aa-set! self i x)))
       (cas (lambda (self i e n)
              (let ((i (aa-index self i)))
                (aa-cas! self i (aa-cast self e) (aa-cast self n)))))
       (cax (lambda (self i e n)
              (let ((i (aa-index self i)))
                (aa-exchange! self i (aa-cast self e) (aa-cast self n)))))
       (methods
        (list
         (cons "length" (lambda (self) (vector-length (aa-vec self))))
         (cons "get" getter) (cons "getPlain" getter) (cons "getOpaque" getter) (cons "getAcquire" getter)
         (cons "set" setter) (cons "lazySet" setter) (cons "setPlain" setter)
         (cons "setOpaque" setter) (cons "setRelease" setter)
         (cons "getAndSet" (lambda (self i x)
           (let ((i (aa-index self i)) (x (aa-cast self x)))
             (aa-transition! self i (lambda (old) x) #t))))
         (cons "compareAndSet" cas)
         (cons "weakCompareAndSet" cas) (cons "weakCompareAndSetPlain" cas)
         (cons "weakCompareAndSetVolatile" cas) (cons "weakCompareAndSetAcquire" cas)
         (cons "weakCompareAndSetRelease" cas)
         (cons "compareAndExchange" cax) (cons "compareAndExchangeAcquire" cax)
         (cons "compareAndExchangeRelease" cax)
         (cons "getAndIncrement" (lambda (self i) (aa-add! self i 1 #t)))
         (cons "getAndDecrement" (lambda (self i) (aa-add! self i -1 #t)))
         (cons "incrementAndGet" (lambda (self i) (aa-add! self i 1 #f)))
         (cons "decrementAndGet" (lambda (self i) (aa-add! self i -1 #f)))
         (cons "getAndAdd" (lambda (self i d) (aa-add! self i (aa-cast self d) #t)))
         (cons "addAndGet" (lambda (self i d) (aa-add! self i (aa-cast self d) #f)))
         (cons "getAndUpdate" (lambda (self i f)
           (aa-update! self (aa-index self i) (lambda (x) (jolt-fi-call f (aa-fi-method self) x)) #t)))
         (cons "updateAndGet" (lambda (self i f)
           (aa-update! self (aa-index self i) (lambda (x) (jolt-fi-call f (aa-fi-method self) x)) #f)))
         (cons "getAndAccumulate" (lambda (self i y f)
           (let ((y (aa-cast self y)))
             (aa-update! self (aa-index self i) (lambda (x) (jolt-fi-call f (aa-fi-method self) x y)) #t))))
         (cons "accumulateAndGet" (lambda (self i y f)
           (let ((y (aa-cast self y)))
             (aa-update! self (aa-index self i) (lambda (x) (jolt-fi-call f (aa-fi-method self) x y)) #f))))
         (cons "toString" aa-tostring))))
  (register-host-methods! "atomic-integer-array" methods)
  (register-host-methods! "atomic-long-array" methods))

(for-each (lambda (nm) (register-class-ctor! nm (atomic-array-ctor 'integer)))
          '("AtomicIntegerArray" "java.util.concurrent.atomic.AtomicIntegerArray"))
(for-each (lambda (nm) (register-class-ctor! nm (atomic-array-ctor 'long)))
          '("AtomicLongArray" "java.util.concurrent.atomic.AtomicLongArray"))
(hashtable-set! jhost-tag->fqn "atomic-integer-array" "java.util.concurrent.atomic.AtomicIntegerArray")
(hashtable-set! jhost-tag->fqn "atomic-long-array" "java.util.concurrent.atomic.AtomicLongArray")
(jch-register-supers! "java.util.concurrent.atomic.AtomicIntegerArray" '("java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.atomic.AtomicLongArray" '("java.io.Serializable"))
