;; zip-inflater.ss — java.util.zip.Inflater (JDK 21 Inflater.java, Inflater.c),
;; and what zip-deflater.ss shares with it: the input window handed to zlib, the
;; copy of zlib's output into a byte[], and raising a JDK exception with zlib's
;; message. Method arguments are zip-base.ss's.

;; --- shared with zip-deflater.ss --------------------------------------------

;; The most input one call hands zlib. The JDK hands zlib the whole remaining
;; range without a copy; here each call copies its input into foreign memory,
;; so a large setInput followed by many small calls would copy the same bytes
;; again and again. zlib takes what it can and the caller calls again, as it
;; must while needsInput() is false.
(define zip-input-window 65536)

(define zip-version-error-message
  "zlib returned Z_VERSION_ERROR: compile time and runtime zlib implementations differ")

;; Raise CLASS with MESSAGE; #f is the JDK's null message.
(define (zip-throw class message)
  (jolt-throw (jolt-host-throwable class (or message jolt-nil))))

;; The next input window of ARRAY (a byte[] or bytevector) from POS to LIM.
(define (zip-window array pos lim)
  (let ((n (min (- lim pos) zip-input-window)))
    (if (<= n 0)
        (make-bytevector 0)
        (let ((out (make-bytevector n)))
          (bytevector-copy! (zip-bytes "input" array) pos out 0 n)
          out))))

;; Copy the first N bytes of BV into DEST (a byte[] or bytevector) at OFF.
(define (zip-store! dest off bv n)
  (when (> n 0)
    (if (bytevector? dest)
        (bytevector-copy! bv 0 dest off n)
        (ja-bv->bytes! bv 0 dest off n))))

;; --- Inflater ---------------------------------------------------------------
;; The fields of JDK 21 Inflater. INPUT is the caller's array, kept by
;; reference as the JDK keeps it; POS and LIM bound what is not yet consumed.
(define-record-type zinflater
  (fields zs mu
          (mutable input) (mutable pos) (mutable lim)
          (mutable input-buffer)          ; the ByteBuffer INPUT was copied from, or #f
          (mutable finished) (mutable pending) (mutable need-dict)
          (mutable read) (mutable written))
  (nongenerative jolt-zinflater-v2))

;; The input ByteBuffer's position follows what the engine consumed.
(define (zinflater-sync-buffer! st)
  (let ((b (zinflater-input-buffer st)))
    (when b (zip-buffer-consumed! b (- (zinflater-lim st) (zinflater-pos st))))))

;; Run F on the state under the object's lock, as the JDK synchronizes on zsRef.
(define (inflater-locked self f)
  (let ((st (jhost-state self)))
    (jolt-with-mutex (zinflater-mu st) (f st))))

;; The same, after the JDK's ensureOpen.
(define (inflater-open self f)
  (inflater-locked self
    (lambda (st)
      (unless (zstream-open? (zinflater-zs st))
        (zip-throw "java.lang.NullPointerException" "Inflater has been closed"))
      (f st))))

;; Inflater() | Inflater(boolean nowrap)
(define (make-inflater . args)
  (let ((nowrap
         (cond
           ((null? args) #f)
           ((pair? (cdr args))
            (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.zip.Inflater"))
           (else (zip-boolean-arg (car args))))))
    (let-values (((zs code msg) (zstream-open 'inflate (if nowrap -15 15) 0 0)))
      (unless zs
        (if (= code z-mem-error)
            (zip-throw "java.lang.OutOfMemoryError" #f)
            (zip-throw "java.lang.InternalError"
                       (or msg
                           (cond ((= code z-version-error) zip-version-error-message)
                                 ((= code z-stream-error) "inflateInit2 returned Z_STREAM_ERROR")
                                 (else "unknown error initializing zlib library"))))))
      (let* ((st (make-zinflater zs (make-mutex) (make-bytevector 0) 0 0 #f #f #f #f 0 0))
             (self (make-jhost "zip-inflater" st)))
        (zstream-guard! self zs (zinflater-mu st))
        self))))

;; setInput(byte[]) | setInput(byte[], off, len)
(define (inflater-set-input! self . args)
  (if (and (null? (cdr args)) (bb? (car args)))
      ;; setInput(ByteBuffer): the remaining bytes, and the buffer to advance
      (let ((arr (zip-buffer-remaining-array (car args))))
        (inflater-locked self
          (lambda (st)
            (zinflater-input-set! st arr)
            (zinflater-pos-set! st 0)
            (zinflater-lim-set! st (ja-len arr))
            (zinflater-input-buffer-set! st (car args))))
        jolt-nil)
      (let-values (((bv off len more) (zip-array-args "setInput" self args "input" "Cannot read the array length because \"input\" is null")))
        (inflater-locked self
          (lambda (st)
            (zinflater-input-set! st (car args))
            (zinflater-pos-set! st off)
            (zinflater-lim-set! st (+ off len))
            (zinflater-input-buffer-set! st #f)))
        jolt-nil)))

;; setDictionary(byte[]) | setDictionary(byte[], off, len); the result check is
;; Inflater.c checkSetDictionaryResult.
(define (inflater-set-dictionary! self . args)
  (let-values (((bv off len more) (zip-array-args "setDictionary" self args "dictionary" "Cannot invoke \"java.nio.ByteBuffer.position()\" because \"dictionary\" is null")))
    (inflater-open self
      (lambda (st)
        (let* ((zs (zinflater-zs st))
               (dict (let ((d (make-bytevector len))) (bytevector-copy! bv off d 0 len) d))
               (code (zstream-set-dictionary! zs dict)))
          (cond ((= code z-ok) (zinflater-need-dict-set! st #f))
                ((or (= code z-stream-error) (= code z-data-error))
                 (zip-throw "java.lang.IllegalArgumentException" (zstream-message zs)))
                (else (zip-throw "java.lang.InternalError" (zstream-message zs)))))))
    jolt-nil))

;; inflate(byte[]) | inflate(byte[], off, len): Inflater.inflate over
;; Inflater.c doInflate (Z_PARTIAL_FLUSH) and checkInflateStatus. zlib gets at
;; most one window per zlib call, so one Java call offers the next window while
;; zlib took all of the last one, output room remains, and the stream goes on,
;; as the JDK's one call over all the input would.
(define (inflater-inflate self . args)
  (cond
    ;; inflate(ByteBuffer): into an array of the buffer's remaining room, then
    ;; through the buffer, whose position advances by what came out
    ;; (Inflater.java lines 400-460)
    ((and (= (length args) 1) (bb? (car args)))
     (let* ((b (car args))
            (arr (na-byte-array (max 0 (- (bb-limit b) (bb-pos b)))))
            (n (jnum->exact (inflater-inflate self arr (->num 0) (->num (ja-len arr))))))
       (zip-buffer-put! b arr n)
       (->num n)))
    (else
     (let ((n (apply inflater-inflate-array self args)))
       (inflater-locked self zinflater-sync-buffer!)
       n))))
(define (inflater-inflate-array self . args)
  (let-values (((out-bv off len more) (zip-array-args "inflate" self args "output" "Cannot read the array length because \"output\" is null")))
    (let ((out (car args)))
      (inflater-open self
        (lambda (st)
          (let ((zs (zinflater-zs st)))
            (define (count! pos read written)
              (zinflater-pos-set! st (+ pos read))
              (zinflater-read-set! st (+ (zinflater-read st) read))
              (zinflater-written-set! st (+ (zinflater-written st) written)))
            (define (done total)
              (zinflater-pending-set! st (and (= total len) (not (zinflater-finished st))))
              (->num total))
            (let loop ((total 0))
              (let* ((pos (zinflater-pos st))
                     (lim (zinflater-lim st))
                     (in (zip-window (zinflater-input st) pos lim))
                     (last? (<= (- lim pos) zip-input-window)))
                (let-values (((code consumed produced bytes)
                              (zstream-step! zs z-partial-flush in (- len total))))
                  (cond
                    ((or (= code z-ok) (= code z-stream-end) (= code z-need-dict))
                     (zip-store! out (+ off total) bytes produced)
                     (when (= code z-stream-end) (zinflater-finished-set! st #t))
                     (when (= code z-need-dict) (zinflater-need-dict-set! st #t))
                     (count! pos consumed produced)
                     (let ((total (+ total produced)))
                       (if (and (= code z-ok)
                                (not last?)
                                (= consumed (bytevector-length in))
                                (< total len))
                           (loop total)
                           (done total))))
                    ((= code z-buf-error)
                     ;; the JDK counts nothing used, so written is 0
                     (if (= total 0)
                         (begin
                           (zinflater-pending-set! st (and (= len 0) (not (zinflater-finished st))))
                           (->num 0))
                         (done total)))
                    ((= code z-data-error)
                     ;; the JDK keeps what was used and produced before the error
                     (zip-store! out (+ off total) bytes produced)
                     (count! pos consumed produced)
                     (zip-throw "java.util.zip.DataFormatException" (zstream-message zs)))
                    ((= code z-mem-error) (zip-throw "java.lang.OutOfMemoryError" #f))
                    (else (zip-throw "java.lang.InternalError" (zstream-message zs)))))))))))))

(define (inflater-remaining self)
  (inflater-locked self (lambda (st) (- (zinflater-lim st) (zinflater-pos st)))))
(define (inflater-needs-input? self)
  (inflater-locked self (lambda (st) (= (zinflater-lim st) (zinflater-pos st)))))
(define (inflater-needs-dictionary? self) (inflater-locked self zinflater-need-dict))
(define (inflater-finished? self) (inflater-locked self zinflater-finished))
(define (inflater-bytes-read self) (inflater-open self zinflater-read))
(define (inflater-bytes-written self) (inflater-open self zinflater-written))

(define (inflater-reset! self)
  (inflater-open self
    (lambda (st)
      (unless (= (zstream-reset! (zinflater-zs st)) z-ok)
        (zip-throw "java.lang.InternalError" #f))
      (zinflater-input-set! st (make-bytevector 0))
      (zinflater-pos-set! st 0)
      (zinflater-lim-set! st 0)
      (zinflater-finished-set! st #f)
      (zinflater-need-dict-set! st #f)
      (zinflater-read-set! st 0)
      (zinflater-written-set! st 0)))
  jolt-nil)

;; end(): frees the stream once; a second end() does nothing.
(define (inflater-end! self)
  (inflater-locked self
    (lambda (st)
      (let ((code (zstream-close! (zinflater-zs st))))
        (zinflater-input-set! st (make-bytevector 0))
        (zinflater-pos-set! st 0)
        (zinflater-lim-set! st 0)
        (when (= code z-stream-error) (zip-throw "java.lang.InternalError" #f)))))
  jolt-nil)

(hashtable-set! jhost-tag->fqn "zip-inflater" "java.util.zip.Inflater")
(register-host-methods! "zip-inflater"
  (list
   (cons "setInput" (zip-method "setInput" '(1 3) inflater-set-input!))
   (cons "setDictionary" (zip-method "setDictionary" '(1 3) inflater-set-dictionary!))
   (cons "getRemaining" (zip-method "getRemaining" '(0) (lambda (self) (->num (inflater-remaining self)))))
   (cons "needsInput" (zip-method "needsInput" '(0) inflater-needs-input?))
   (cons "needsDictionary" (zip-method "needsDictionary" '(0) inflater-needs-dictionary?))
   (cons "finished" (zip-method "finished" '(0) inflater-finished?))
   (cons "inflate" (zip-method "inflate" '(1 3) inflater-inflate))
   (cons "getAdler"
         (zip-method "getAdler" '(0)
           (lambda (self)
             (inflater-open self (lambda (st) (->num (jolt-s32 (zstream-adler (zinflater-zs st)))))))))
   (cons "getTotalIn" (zip-method "getTotalIn" '(0) (lambda (self) (->num (jolt-s32 (inflater-bytes-read self))))))
   (cons "getBytesRead" (zip-method "getBytesRead" '(0) (lambda (self) (->num (inflater-bytes-read self)))))
   (cons "getTotalOut" (zip-method "getTotalOut" '(0) (lambda (self) (->num (jolt-s32 (inflater-bytes-written self))))))
   (cons "getBytesWritten" (zip-method "getBytesWritten" '(0) (lambda (self) (->num (inflater-bytes-written self)))))
   (cons "reset" (zip-method "reset" '(0) inflater-reset!))
   (cons "end" (zip-method "end" '(0) inflater-end!))))
(reg-ctor! '("Inflater" "java.util.zip.Inflater") make-inflater)
