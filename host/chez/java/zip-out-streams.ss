;; zip-out-streams.ss — java.util.zip.DeflaterOutputStream (JDK 21
;; DeflaterOutputStream.java), and the frame GZIPOutputStream builds on.
;;
;; Each stream is jolt's own "out-stream" (io-streams.ss): a jhost over a Chez
;; custom binary output port whose write! runs the body of the JDK's
;; write(byte[], off, len). write flushes the port after each call, so the
;; Deflater sees each write when the caller makes it, as in the JDK; io/copy,
;; transferTo and a Writer put bytes into the port directly, and those reach the
;; Deflater on flush, finish or close. write, flush, finish and close are
;; overridden for these streams, so the JDK's checks run when the caller calls,
;; and they take arguments as zip-base.ss does. with-open closes a stream through
;; its close method (io-streams.ss jolt-close), and io/writer's writer closes it
;; as well, so the end of the compressed data is written there. Slot 4 of the
;; out-stream state holds the zout record.

;; --- the frame --------------------------------------------------------------
(define-record-type zout
  (fields fqn               ; the class name the stream reports
          inner             ; the wrapped OutputStream
          codec             ; the Deflater
          owned             ; #t when the stream made CODEC (usesDefaultDeflater)
          buf               ; byte[]: the JDK's output buffer
          sync-flush        ; the constructor's syncFlush
          (mutable closed)
          write-proc        ; (zout bv start count): bytes a write! received
          finish-proc       ; (zout): the class's finish(), after the port flush
          (mutable extra))  ; per-class state (GZIPOutputStream)
  (nongenerative jolt-zout-v1))

;; The zout record of X, or #f for any other value.
(define (zout-of x)
  (let ((st (and (out-stream? x) (jhost-state x))))
    (and (vector? st) (fx>? (vector-length st) 4)
         (let ((v (vector-ref st 4))) (and (zout? v) v)))))

;; An out-stream over Z. State #(port extract acc piped zout): not a
;; ByteArrayOutputStream, not piped.
(define (make-zout-stream z)
  (make-jhost "out-stream"
              (vector (make-custom-binary-output-port
                       (zout-fqn z)
                       (lambda (bv start count) ((zout-write-proc z) z bv start count) count)
                       #f #f #f)
                      #f #f #f z)))

;; Call METHOD with no arguments on the wrapped stream X. A reify or proxy that
;; does not define it has the OutputStream default, which does nothing.
(define (zout-inner-call x method)
  (when (or (not (jreify? x)) (obj-has-method? x method 0))
    (record-method-dispatch x method jolt-nil)))

;; deflate(): one Deflater call into BUF, written to the wrapped stream
;; (lines 281-286).
(define (zout-deflate! z)
  (let* ((buf (zout-buf z))
         (n (jnum->exact (deflater-deflate (zout-codec z) buf))))
    (when (> n 0)
      (record-method-dispatch (zout-inner z) "write"
        (list->cseq (list buf (->num 0) (->num n)))))))

;; The body of write(byte[], off, len) (lines 220-235). zout-write checks first
;; for writes through the method; io/copy, transferTo and a Writer put bytes
;; into the port directly, so a finished Deflater is refused here too, as the
;; JDK refuses every write after finish. BV is the port's buffer or, for a large
;; put, the caller's bytevector; the Deflater keeps a reference to it but has
;; used all of it before this returns.
(define (zout-deflate-write z bv start count)
  (let ((def (zout-codec z)))
    (when (and (fx> count 0) (deflater-finished? def))
      (zip-throw "java.io.IOException" "write beyond end of stream"))
    (deflater-set-input! def bv (->num start) (->num count))
    (let loop ()
      (unless (deflater-needs-input? def)
        (zout-deflate! z)
        (loop)))))

;; finish(): lines 243-256. On an IOException, a Deflater the stream made ends.
(define (zout-deflate-finish z)
  (let ((def (zout-codec z)))
    (unless (deflater-finished? def)
      (guard (e ((zip-thrown? e "IOException")
                 (when (zout-owned z) (deflater-end! def))
                 (raise e)))
        (deflater-finish! def)
        (let loop ()
          (unless (deflater-finished? def)
            (zout-deflate! z)
            (loop)))))))

;; Flush the port so the Deflater has every buffered byte. A write the Deflater
;; or the wrapped stream refuses drops its bytes, as a failing JDK write does.
(define (zout-flush-port! port)
  (unless (port-closed? port)
    (guard (e (#t (clear-output-port port) (raise e)))
      (flush-output-port port))))

(define (zip-deflater? x) (and (jhost? x) (string=? (jhost-tag x) "zip-deflater")))

;; --- the constructor --------------------------------------------------------
;; DeflaterOutputStream(out) | (out, syncFlush) | (out, def) | (out, def, syncFlush)
;; | (out, def, size) | (out, def, size, syncFlush), resolved as the compiled
;; call resolves them. With several overloads for a count, each argument must
;; fit its parameter (nil fits an object, not a boolean or an int), or there is
;; no matching constructor. The one four-argument overload casts each argument.
;; Then the JDK's null check and size check run.
(define (zout-out-fits? x) (or (jolt-nil? x) (out-stream? x) (user-out-stream? x)))
(define (zout-def-fits? x) (or (jolt-nil? x) (zip-deflater? x)))

(define (make-deflater-output-stream . args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException
               "No matching ctor found for class java.util.zip.DeflaterOutputStream"))
  (define (cast-out x) (if (zout-out-fits? x) x (zip-ctor-class-cast x "java.io.OutputStream")))
  (let-values
      (((out given def size sync)
        (case (length args)
          ((1) (let ((out (cast-out (car args))))
                 (values out #f #f 512 #f)))
          ((2) (let ((out (car args)) (b (cadr args)))
                 (cond ((not (zout-out-fits? out)) (no-ctor))
                       ((boolean? b) (values out #f #f 512 b))
                       ((zout-def-fits? b) (values out #t b 512 #f))
                       (else (no-ctor)))))
          ((3) (let ((out (car args)) (d (cadr args)) (c (caddr args)))
                 (cond ((not (and (zout-out-fits? out) (zout-def-fits? d))) (no-ctor))
                       ((boolean? c) (values out #t d 512 c))
                       ((zip-long? c) (values out #t d (zip-ctor-int-arg c) #f))
                       (else (no-ctor)))))
          ((4) (let* ((out (cast-out (car args)))
                      (d (let ((x (cadr args)))
                           (if (zout-def-fits? x) x (zip-ctor-class-cast x "java.util.zip.Deflater"))))
                      (n (zip-ctor-int-arg (caddr args)))
                      (s (zip-boolean-arg (cadddr args))))
                 (values out #t d n s)))
          (else (no-ctor)))))
    (let ((def (cond (given def)
                     ((jolt-nil? out) jolt-nil)
                     (else (make-deflater)))))
      (when (or (jolt-nil? out) (jolt-nil? def))
        (zip-throw "java.lang.NullPointerException" #f))
      (when (<= size 0)
        (zip-throw "java.lang.IllegalArgumentException" "buffer size <= 0"))
      (make-zout-stream
       (make-zout "java.util.zip.DeflaterOutputStream" out def (not given)
                  (na-byte-array size) sync #f
                  zout-deflate-write zout-deflate-finish #f)))))

;; --- out-stream methods for these streams -----------------------------------
;; Each override answers for a zip stream and hands any other out-stream to the
;; method it replaces.
(define (zout-prior name)
  (hashtable-ref (hashtable-ref host-methods-tbl "out-stream" #f) name #f))
(define zout-prior-write (zout-prior "write"))
(define zout-prior-flush (zout-prior "flush"))
(define zout-prior-close (zout-prior "close"))

(define (zout-method name arities prior f)
  (host-arity-over prior arities
    (lambda (self . args)
      (let ((z (zout-of self)))
        (cond ((not z) (apply prior self args))
              ((memv (length args) arities) (apply f self z args))
              (else (no-method-throw name self (length args))))))))

(define (zout-null-array)
  (throw-jvm 'NullPointerException "Cannot read the array length because \"b\" is null"))

;; write(int) | write(byte[]) | write(byte[], off, len): the arguments are cast,
;; then the JDK's finished check, null check and range check run (lines 205-235),
;; then the bytes go into the port, and the port is flushed.
(define (zout-write self z . args)
  (define (finished-check)
    (when (deflater-finished? (zout-codec z))
      (zip-throw "java.io.IOException" "write beyond end of stream")))
  (define (put . xs)
    (apply zout-prior-write self xs)
    (zout-flush-port! (out-stream-port self))
    jolt-nil)
  (if (null? (cdr args))
      (let ((x (car args)))
        (cond ((zip-long? x)
               (let ((v (zip-int-arg x)))
                 (finished-check)
                 (put (->num v))))
              ((jolt-nil? x) (zout-null-array))
              ((zip-byte-array? x)
               (finished-check)
               (put x))
              (else (no-method-throw "write" self 1))))
      (let* ((b (let ((x (car args)))
                  (if (or (jolt-nil? x) (zip-byte-array? x)) x (zip-class-cast x "[B"))))
             (off (zip-int-arg (cadr args)))
             (len (zip-int-arg (caddr args))))
        (finished-check)
        (when (jolt-nil? b) (zout-null-array))
        (let ((whole (bytevector-length (zip-bytes "write" b))))
          (when (or (< off 0) (< len 0) (> (+ off len) whole))
            (zip-throw "java.lang.IndexOutOfBoundsException" #f)))
        (if (= len 0) jolt-nil (put b (->num off) (->num len))))))

;; flush(): lines 304-321. With syncFlush, deflate with SYNC_FLUSH after the
;; port's bytes are in; then flush the wrapped stream.
(define (zout-flush self z)
  (let ((port (out-stream-port self))
        (def (zout-codec z)))
    (zout-flush-port! port)
    (when (and (zout-sync-flush z) (not (deflater-finished? def)))
      (let* ((buf (zout-buf z))
             (fbuf (if (fx< (ja-len buf) 7) (na-byte-array 512) buf))
             (flen (ja-len fbuf)))
        (let loop ()
          (let ((n (jnum->exact (deflater-deflate def fbuf (->num 0) (->num flen)
                                                  (->num z-sync-flush)))))
            (when (> n 0)
              (record-method-dispatch (zout-inner z) "write"
                (list->cseq (list fbuf (->num 0) (->num n))))
              (when (fx= n flen) (loop)))))))
    (zout-inner-call (zout-inner z) "flush")
    jolt-nil))

;; finish(): flush the port so the Deflater has every byte, then the class's
;; own finish. Only java.util.zip output streams have it.
(define (zout-finish self . args)
  (let ((z (zout-of self)))
    (cond ((not z) (no-method-throw "finish" self (length args)))
          ((pair? args) (no-method-throw "finish" self (length args)))
          (else
           (zout-flush-port! (out-stream-port self))
           ((zout-finish-proc z) z)
           jolt-nil))))

;; close(): lines 264-275. finish; end a Deflater the stream made even when
;; finish throws; then close the wrapped stream. A failed finish leaves the
;; stream open, as in the JDK.
(define (zout-close self z)
  (unless (zout-closed z)
    (dynamic-wind
      (lambda () #f)
      (lambda () (zout-finish self))
      (lambda () (when (zout-owned z) (deflater-end! (zout-codec z)))))
    (zout-inner-call (zout-inner z) "close")
    (close-port (out-stream-port self))
    (zout-closed-set! z #t))
  jolt-nil)

(register-host-methods! "out-stream"
  (list (cons "write" (zout-method "write" '(1 3) zout-prior-write zout-write))
        (cons "flush" (zout-method "flush" '(0) zout-prior-flush zout-flush))
        (cons "finish" zout-finish)
        (cons "close" (zout-method "close" '(0) zout-prior-close zout-close))))

;; class and instance? answer from the record's class name and the hierarchy: a
;; known class the stream's class does not extend is #f, not the out-stream's
;; general answer.
(register-class-arm! (lambda (x) (and (zout-of x) #t))
                     (lambda (x) (zout-fqn (zout-of x))))
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((z (and (symbol-t? type-sym) (zout-of val))))
      (if z
          (let* ((n (symbol-t-name type-sym))
                 (name (or (resolve-class-hint n) n)))
            (cond ((or (string=? name "java.lang.Object") (jch-isa? (zout-fqn z) name)) #t)
                  ((jch-known? name) #f)
                  (else 'pass)))
          'pass))))

;; io/writer over a zip stream: the writer's close and flush reach the stream's
;; own methods, so closing the writer finishes the compressed data, as closing
;; an OutputStreamWriter over it does on the JDK.
(let ((prev jolt-io-writer))
  (set! jolt-io-writer
        (lambda (x . opts)
          (if (zout-of x)
              (begin
                (io-opts-check! opts)
                (make-charset-writer (out-stream-sink-port x) (io-encoding opts) x #f
                                     "java.io.BufferedWriter"))
              (apply prev x opts))))
  (def-var! "clojure.java.io" "writer" jolt-io-writer))

(reg-ctor! '("DeflaterOutputStream" "java.util.zip.DeflaterOutputStream") make-deflater-output-stream)
