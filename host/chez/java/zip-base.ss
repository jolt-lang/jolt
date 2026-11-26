;; zip-base.ss — what the java.util.zip classes share: the JDK's array range
;; check and its message, byte[] access, and the two checksums (JDK 21
;; CRC32.java, Adler32.java). The class rows, including the two exception
;; classes, are in class-hierarchy.ss.

;; --- range check ------------------------------------------------------------
;; Preconditions.checkFromIndexSize with the AIOOBE formatter: fail when any of
;; the three is negative or the range runs past the end (JDK 21
;; jdk/internal/util/Preconditions.java lines 245-249, 395-397).
(define (zip-check-from-index-size off len length)
  (when (or (< length 0) (< off 0) (< len 0) (> len (- length off)))
    (jolt-throw
     (jolt-host-throwable "java.lang.ArrayIndexOutOfBoundsException"
                          (format "Range [~a, ~a + ~a) out of bounds for length ~a" off off len length)))))

;; --- byte[] access ----------------------------------------------------------
;; A byte[] argument (or a Chez bytevector) as a bytevector, for read-only use.
;; A byte array's backing is a bytevector (natives-array.ss na-make-backing), so
;; this is normally the array's own storage; the copy covers a boxed backing
;; restored from an image written before typed backings (natives-array.ss
;; lines 316-324). nil is the JDK's NullPointerException with no message.
(define (zip-bytes who x)
  (cond
    ((bytevector? x) x)
    ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte))
     (let ((v (jolt-array-vec x)))
       (if (bytevector? v) v (na-bytearray->bv x))))
    ((jolt-nil? x)
     (jolt-throw (jolt-host-throwable "java.lang.NullPointerException" jolt-nil)))
    (else
     (throw-jvm 'ClassCastException (string-append who ": argument is not a byte[]")))))

(define (zip-byte-array? x)
  (or (bytevector? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte))))

;; An integer a Java int parameter accepts in overload resolution: a Long, Integer,
;; Short or Byte. A double or a char is not one (Clojure 1.12
;; Reflector.paramArgTypeMatch). A BigInt is not one either, but Jolt cannot tell
;; a BigInt from a Long, so a BigInt in the long range passes.
(define (zip-long? x)
  (and (number? x) (exact? x) (integer? x)
       (<= -9223372036854775808 x 9223372036854775807)))

;; --- method arguments -------------------------------------------------------
;; JDK 21 casts a method's arguments one of two ways (records-interop.ss). A call
;; the compiler resolved, on a receiver whose class it knows, casts and unboxes
;; each argument at the call site, which the back end emits from the zip
;; signatures there, so the arguments arrive here already converted. A call it
;; did not resolve reaches the method through the Reflector, which is what the
;; checks below are: Class.cast for a reference parameter, Number.intValue for an
;; int one, a refusal by name for anything else. A constructor is compiled
;; against the overload its argument count picks, so its arguments take the
;; resolved casts (zip-ctor-int-arg, zip-ctor-class-cast). A wrong argument count
;; has no matching method.
(define (zip-class-cast x to) (jolt-reflect-cast-fail x to))
(define (zip-ctor-class-cast x to) (jolt-checkcast-fail x to))
(define (zip-int-arg x) (jolt-box-int x))
(define (zip-long-arg x) (jolt-box-long x))
;; A constructor's int argument, cast as the compiled call casts it: nil is
;; NullPointerException; a double is truncated, NaN is 0, an infinity is out of
;; range for long, and past the int range is "integer overflow"; a long, ratio or
;; BigDecimal goes through Clojure's int; anything else, a char included, is not
;; a Number.
(define (zip-ctor-int-arg x) (jolt-unbox-int x))

;; A boolean argument of the overload the call picked.
(define (zip-boolean-arg x)
  (cond ((jolt-nil? x)
         (throw-jvm 'NullPointerException
                    "Cannot invoke \"java.lang.Boolean.booleanValue()\" because \"null\" is null"))
        ((boolean? x) x)
        (else (zip-ctor-class-cast x "java.lang.Boolean"))))

;; A method WHO that takes one of ARITIES arguments after self; any other count
;; has no matching method.
(define (zip-method who arities f)
  (host-arity-of arities #t
    (lambda (self . args)
      (if (memv (length args) arities)
          (apply f self args)
          (no-method-throw who self (length args))))))

;; The arguments of X(byte[]) | X(byte[], int off, int len, int ...) as
;; (values bv off len more), where MORE holds the int arguments after LEN.
;; X(ByteBuffer) is out of scope, so one argument that is not a byte[] or nil
;; has no matching method. With more than one, every argument is cast, then the
;; null check and the range check run. NAME, when given, is the JDK's name for
;; the array parameter, which its NullPointerException message names; CRC32 and
;; Adler32 throw theirs with no message.
(define (zip-array-args who self args . opt)
  (let ((b (car args))
        (name (if (and (pair? opt) (car opt)) (list (car opt)) '()))
        (nil1 (and (pair? opt) (pair? (cdr opt)) (cadr opt))))
    (if (null? (cdr args))
        (begin
          (unless (or (zip-byte-array? b) (jolt-nil? b))
            (no-method-throw who self 1))
          ;; nil fits every overload, and the reflector settles on one whose
          ;; own NullPointerException the caller names (NIL1)
          (when (and nil1 (jolt-nil? b))
            (jolt-throw (jolt-host-throwable "java.lang.NullPointerException" nil1)))
          (let ((bv (zip-bytes who b)))
            (values bv 0 (bytevector-length bv) '())))
        (let* ((bv (cond ((jolt-nil? b) #f)
                         ((zip-byte-array? b) (zip-bytes who b))
                         (else (zip-class-cast b "[B"))))
               (off (zip-int-arg (cadr args)))
               (len (zip-int-arg (caddr args)))
               (more (let loop ((xs (cdddr args)) (acc '()))
                       (if (null? xs)
                           (reverse acc)
                           (loop (cdr xs) (cons (zip-int-arg (car xs)) acc))))))
          (unless bv
            (if (pair? name)
                (throw-jvm 'NullPointerException
                           (string-append "Cannot read the array length because \""
                                          (car name) "\" is null"))
                (zip-bytes who b)))
          (zip-check-from-index-size off len (bytevector-length bv))
          (values bv off len more)))))

;; --- CRC32 and Adler32 ------------------------------------------------------
;; state #(value): the unsigned 32-bit checksum. UPDATE is zlib-crc32 or
;; zlib-adler32, (value bv start count) -> value.
;;
;; update resolves its overloads as the method arguments section above says.
;; With one argument the argument picks the method: an integer is update(int), a
;; byte[] is update(byte[]), nil is the ByteBuffer overload, and anything else
;; has no matching method. With three there is one method, update(byte[], int,
;; int) (zip-array-args). The reflector wraps a long past the int range; a call
;; the compiler resolved (a long literal picks update(int)) has already been
;; unboxed with the overflow check.
(define (zip-checksum-methods initial update)
  (list
   (cons "update"
         (lambda (self . args)
           (let ((st (jhost-state self))
                 (b (if (pair? args) (car args) jolt-nil))
                 (rest (if (pair? args) (cdr args) '())))
             (cond
               ((null? args) (no-method-throw "update" self 0))
               ((null? rest)
                (cond
                  ((zip-long? b)
                   (vector-set! st 0 (update (vector-ref st 0)
                                             (make-bytevector 1 (bitwise-and (zip-int-arg b) #xff))
                                             0 1)))
                  ((jolt-nil? b)
                   (throw-jvm 'NullPointerException
                              "Cannot invoke \"java.nio.ByteBuffer.position()\" because \"buffer\" is null"))
                  ((zip-byte-array? b)
                   (let ((bv (zip-bytes "update" b)))
                     (vector-set! st 0 (update (vector-ref st 0) bv 0 (bytevector-length bv)))))
                  (else (no-method-throw "update" self 1))))
               ((and (pair? (cdr rest)) (null? (cddr rest)))
                (let-values (((bv off len more) (zip-array-args "update" self args)))
                  (vector-set! st 0 (update (vector-ref st 0) bv off len))))
               (else (no-method-throw "update" self (+ 1 (length rest))))))
           jolt-nil))
   (cons "reset" (lambda (self) (vector-set! (jhost-state self) 0 initial) jolt-nil))
   (cons "getValue" (lambda (self) (->num (vector-ref (jhost-state self) 0))))))

(hashtable-set! jhost-tag->fqn "zip-crc32" "java.util.zip.CRC32")
(hashtable-set! jhost-tag->fqn "zip-adler32" "java.util.zip.Adler32")
(register-host-methods! "zip-crc32" (zip-checksum-methods 0 zlib-crc32))
(register-host-methods! "zip-adler32" (zip-checksum-methods 1 zlib-adler32))
(reg-ctor! '("CRC32" "java.util.zip.CRC32")
           (lambda () (make-jhost "zip-crc32" (vector 0))))
(reg-ctor! '("Adler32" "java.util.zip.Adler32")
           (lambda () (make-jhost "zip-adler32" (vector 1))))

;; --- ByteBuffer arguments ----------------------------------------------------
;; The ByteBuffer overloads of Inflater and Deflater (setInput(ByteBuffer),
;; inflate(ByteBuffer), deflate(ByteBuffer[, flush])) run on the byte[] engine:
;; the buffer's remaining bytes are copied into an input array at setInput, and
;; output is produced into an array of the buffer's remaining room and put
;; through the buffer, which advances its position by what was produced. The
;; INPUT buffer's position advances as the engine consumes the array, after
;; each call, as the JDK advances it during one (java/byte-buffer.ss holds the
;; buffer's own layout).

;; The remaining bytes of buffer B as a jolt byte-array (a copy).
(define (zip-buffer-remaining-array b)
  (let* ((n (max 0 (- (bb-limit b) (bb-pos b))))
         (arr (na-byte-array n)))
    (bb-bulk-ref! b (bb-pos b) arr 0 n)
    arr))

;; Put N bytes of ARR into B at its position and advance the position.
(define (zip-buffer-put! b arr n)
  (bb-bulk-set! b (bb-pos b) arr 0 n)
  (bb-pos! b (+ (bb-pos b) n))
  n)

;; The input buffer's position after the engine consumed down to REMAINING
;; bytes of the array copied from it.
(define (zip-buffer-consumed! b remaining)
  (bb-pos! b (- (bb-limit b) remaining)))

