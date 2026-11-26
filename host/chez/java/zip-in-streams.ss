;; zip-in-streams.ss — java.util.zip.InflaterInputStream and DeflaterInputStream
;; (JDK 21 InflaterInputStream.java, DeflaterInputStream.java), and the frame
;; GZIPInputStream and ZipInputStream build on.
;;
;; Each stream is jolt's own "in-stream" (io-streams.ss): a jhost over a Chez
;; custom binary input port whose read! runs the JDK's read(byte[], off, len).
;; So read, readAllBytes, skip, slurp, io/copy, InputStreamReader and with-open
;; work as they do for any stream. A bulk read hands read! the caller's own
;; buffer; a single-byte read fills the port's buffer first. After read!
;; answers 0 (the end), a later read calls read! again (Chez s/io.ss
;; binary-custom-port-get-some). Slot 5 of the in-stream state holds the zin
;; record, which marks the stream for class, instance? and the method overrides
;; at the end of this file. Those take arguments as zip-base.ss does, and a
;; single-byte read() with nothing in the port's buffer reads one byte through
;; the class's own read, as the JDK's read() does, not a buffer's worth.

;; --- the frame --------------------------------------------------------------
(define-record-type zin
  (fields fqn               ; the class name the stream reports
          inner             ; the wrapped InputStream
          codec             ; the Inflater or Deflater
          owned             ; #t when the stream made CODEC (the JDK's usesDefault...)
          buf               ; byte[]: the JDK's input buffer
          (mutable len)     ; bytes the last fill read into BUF (the JDK's len)
          (mutable reach-eof)
          read-proc         ; (zin bv start count) -> n > 0, or 0 at the end
          available-proc    ; (zin) -> 0 or 1: the class's available()
          close-proc        ; (zin): runs when the port closes
          (mutable extra))  ; per-class state (GZIPInputStream, ZipInputStream)
  (nongenerative jolt-zin-v1))

;; The zin record of X, or #f for any other value.
(define (zin-of x)
  (let ((st (and (in-stream? x) (jhost-state x))))
    (and (vector? st) (fx>? (vector-length st) 5) (vector-ref st 5))))

;; An in-stream over Z. State #(port markable? mark-pos piped pushback zin):
;; not markable, not piped, no pushback.
(define (make-zin-stream z)
  (make-jhost "in-stream"
              (vector (make-custom-binary-input-port
                       (zin-fqn z)
                       (lambda (bv start count) ((zin-read-proc z) z bv start count))
                       #f #f
                       (lambda () ((zin-close-proc z) z)))
                      #f 0 #f #f z)))

;; in.read(buf, 0, buf.length) on the wrapped stream; -1 at its end.
(define (zin-read-inner z)
  (let ((buf (zin-buf z)))
    (jnum->exact (record-method-dispatch (zin-inner z) "read"
                   (list->cseq (list buf (->num 0) (->num (ja-len buf))))))))

;; Is condition E a thrown Java exception whose class is, or extends, SIMPLE?
(define (zip-thrown? e simple)
  (and (jolt-throw-condition? e)
       (let ((v (jolt-throw-condition-value e)))
         (and (ex-info-map? v) (exception-isa? (ex-info-class v) simple)))))

;; The message of the thrown Java exception in condition E, or #f.
(define (zip-thrown-message e)
  (let ((m (jolt-ex-info-record-message (jolt-throw-condition-value e))))
    (and (string? m) m)))

;; --- InflaterInputStream ----------------------------------------------------

;; fill(): read more input into BUF and hand it to the Inflater.
(define (zin-fill! z)
  (let ((n (zin-read-inner z)))
    (when (< n 0)
      (zip-throw "java.io.EOFException" "Unexpected end of ZLIB input stream"))
    (zin-len-set! z n)
    (inflater-set-input! (zin-codec z) (zin-buf z) (->num 0) (->num n))))

;; read(byte[], off, len): InflaterInputStream.java lines 153-183.
(define (zin-inflate-read z bv start count)
  (if (fx= count 0)
      0
      (guard (e ((zip-thrown? e "DataFormatException")
                 (zip-throw "java.util.zip.ZipException"
                            (or (zip-thrown-message e) "Invalid ZLIB data format"))))
        (let ((inf (zin-codec z)))
          (let loop ()
            (if (or (inflater-finished? inf) (inflater-needs-dictionary? inf))
                (begin (zin-reach-eof-set! z #t) 0)
                (begin
                  ;; the Inflater may hold output that did not fit last time
                  (when (and (inflater-needs-input? inf)
                             (not (inflater-locked inf zinflater-pending)))
                    (zin-fill! z))
                  (let ((n (jnum->exact (inflater-inflate inf bv (->num start) (->num count)))))
                    (if (fx= n 0) (loop) n)))))))))

;; available(): lines 195-206.
(define (zin-inflate-available z)
  (cond ((zin-reach-eof z) 0)
        ((inflater-finished? (zin-codec z)) (zin-reach-eof-set! z #t) 0)
        (else 1)))

;; close(): lines 243-250. The Inflater ends only if the stream made it.
(define (zin-inflate-close z)
  (when (zin-owned z) (inflater-end! (zin-codec z)))
  (record-method-dispatch (zin-inner z) "close" jolt-nil))

(define (zip-inflater? x) (and (jhost? x) (string=? (jhost-tag x) "zip-inflater")))

;; Constructor arguments, cast as the compiled call casts them before the
;; constructor runs: nil passes every cast. An InputStream is jolt's own
;; in-stream or a reify / proxy declared as one (io-streams.ss).
(define (zin-stream-arg x)
  (if (or (jolt-nil? x) (in-stream? x) (user-in-stream? x))
      x
      (zip-ctor-class-cast x "java.io.InputStream")))
(define (zin-codec-arg x ok? class)
  (if (or (jolt-nil? x) (ok? x)) x (zip-ctor-class-cast x class)))
(define (zin-ctor-arity who args)
  (unless (<= 1 (length args) 3)
    (throw-jvm 'IllegalArgumentException (string-append "No matching ctor found for class " who))))

;; InflaterInputStream(in) | (in, inf) | (in, inf, size)
(define (make-inflater-input-stream . args)
  (zin-ctor-arity "java.util.zip.InflaterInputStream" args)
  (let* ((in (zin-stream-arg (car args)))
         (given (and (pair? (cdr args))
                     (zin-codec-arg (cadr args) zip-inflater? "java.util.zip.Inflater")))
         (size (if (and (pair? (cdr args)) (pair? (cddr args))) (zip-ctor-int-arg (caddr args)) 512))
         (owned (null? (cdr args)))
         (inf (cond (given given)
                    ((jolt-nil? in) jolt-nil)
                    (else (make-inflater)))))
    (when (or (jolt-nil? in) (jolt-nil? inf))
      (zip-throw "java.lang.NullPointerException" #f))
    (when (<= size 0)
      (zip-throw "java.lang.IllegalArgumentException" "buffer size <= 0"))
    (make-zin-stream
     (make-zin "java.util.zip.InflaterInputStream" in inf owned (na-byte-array size) 0 #f
               zin-inflate-read zin-inflate-available zin-inflate-close #f))))

;; --- DeflaterInputStream ----------------------------------------------------

;; read(byte[], off, len): DeflaterInputStream.java lines 171-210.
(define (zin-deflate-read z bv start count)
  (let ((def (zin-codec z)))
    (let loop ((off start) (len count) (cnt 0))
      (if (and (fx> len 0) (not (deflater-finished? def)))
          (begin
            (when (deflater-needs-input? def)
              (let ((n (zin-read-inner z)))
                (cond ((< n 0) (deflater-finish! def))
                      ((> n 0) (deflater-set-input! def (zin-buf z) (->num 0) (->num n))))))
            (let ((n (jnum->exact (deflater-deflate def bv (->num off) (->num len)))))
              (loop (fx+ off n) (fx- len n) (fx+ cnt n))))
          (if (and (fx= cnt 0) (deflater-finished? def))
              (begin (zin-reach-eof-set! z #t) 0)
              cnt)))))

;; available(): lines 259-265.
(define (zin-deflate-available z) (if (zin-reach-eof z) 0 1))

;; close(): lines 126-139. The Deflater ends only if the stream made it. The
;; JDK clears the stream in a finally, so it is closed even when the wrapped
;; close throws. EXTRA marks the stream closed before the wrapped close runs, and
;; the throw goes to whatever closed the port: close, slurp or a reader. The port
;; stays open when the throw leaves this, so the next use closes it quietly
;; (zin-live-port).
(define (zin-deflate-close z)
  (unless (eq? (zin-extra z) 'closed)
    (zin-extra-set! z 'closed)
    (when (zin-owned z) (deflater-end! (zin-codec z)))
    (record-method-dispatch (zin-inner z) "close" jolt-nil)))

(define (zip-deflater-arg? x) (and (jhost? x) (string=? (jhost-tag x) "zip-deflater")))

;; DeflaterInputStream(in) | (in, defl) | (in, defl, bufLen)
(define (make-deflater-input-stream . args)
  (zin-ctor-arity "java.util.zip.DeflaterInputStream" args)
  (let* ((in (zin-stream-arg (car args)))
         (given (and (pair? (cdr args))
                     (zin-codec-arg (cadr args) zip-deflater-arg? "java.util.zip.Deflater")))
         (size (if (and (pair? (cdr args)) (pair? (cddr args))) (zip-ctor-int-arg (caddr args)) 512))
         (owned (null? (cdr args)))
         (def (cond (given given)
                    ((jolt-nil? in) jolt-nil)
                    (else (make-deflater)))))
    (when (jolt-nil? in) (zip-throw "java.lang.NullPointerException" "Null input"))
    (when (jolt-nil? def) (zip-throw "java.lang.NullPointerException" "Null deflater"))
    (when (< size 1) (zip-throw "java.lang.IllegalArgumentException" "Buffer size < 1"))
    (make-zin-stream
     (make-zin "java.util.zip.DeflaterInputStream" in def owned (na-byte-array size) 0 #f
               zin-deflate-read zin-deflate-available zin-deflate-close #f))))

;; --- in-stream methods for these streams ------------------------------------
;; Each override answers for a zip stream and hands any other in-stream to the
;; method it replaces.
(define (zin-prior name)
  (hashtable-ref (hashtable-ref host-methods-tbl "in-stream" #f) name #f))
(define zin-prior-available (zin-prior "available"))
(define zin-prior-skip (zin-prior "skip"))
(define zin-prior-read (zin-prior "read"))
(define zin-prior-read-n (zin-prior "readNBytes"))
(define zin-prior-mark (zin-prior "mark"))
(define zin-prior-reset (zin-prior "reset"))
(define zin-prior-mark-supported (zin-prior "markSupported"))
(define zin-prior-close (zin-prior "close"))

;; A method NAME of zip streams that takes one of ARITIES arguments; F gets the
;; stream, its zin record and the arguments.
(define (zin-method name arities prior f)
  (host-arity-over prior arities
    (lambda (self . args)
      (let ((z (zin-of self)))
        (cond ((not z) (apply prior self args))
              ((memv (length args) arities) (apply f self z args))
              (else (no-method-throw name self (length args))))))))

;; A byte[] argument of a call that took its overload: nil passes.
(define (zin-bytes-arg b)
  (if (or (jolt-nil? b) (zip-byte-array? b)) b (zip-class-cast b "[B")))

;; The port of a stream that is still open, or IOException "Stream closed". A
;; DeflaterInputStream whose wrapped close threw is closed, but its port is not
;; yet: closing it now runs the close procedure again, which sees the mark.
(define (zin-live-port self z)
  (when (eq? (zin-extra z) 'closed)
    (close-port (in-stream-port self)))
  (in-stream-live-port self))

;; A long argument (skip): the reflector's Number.longValue (zip-base.ss).
(define (zin-long-arg x) (zip-long-arg x))

;; available(): the class's own answer, or 1 when it says 0 while bytes still
;; wait in the port's buffer (the stream is not at its end). A ZipFile entry
;; stream answers a count of what is left, which already includes them. A
;; closed stream is IOException "Stream closed".
(define (zin-available self z)
  (let* ((port (zin-live-port self z))
         (n ((zin-available-proc z) z)))
    (->num (if (and (eqv? n 0) (fx> (port-buffered port) 0)) 1 n))))

;; skip(n): a negative count is IllegalArgumentException; then read and drop up
;; to n bytes (at most Integer.MAX_VALUE), 512 at a time (lines 215-236).
(define (zin-skip self z n)
  (let ((n (zin-long-arg n)))
    (when (< n 0)
      (zip-throw "java.lang.IllegalArgumentException" "negative skip length"))
    (let ((port (zin-live-port self z))
          (limit (min n 2147483647)))
      (let loop ((total 0))
        (if (>= total limit)
            (->num total)
            (let ((bv (get-bytevector-n port (min 512 (- limit total)))))
              (if (eof-object? bv)
                  (begin (zin-reach-eof-set! z #t) (->num total))
                  (loop (+ total (bytevector-length bv))))))))))

;; read() | read(byte[]) | read(byte[], off, len): the arguments are cast, then
;; ensureOpen, the null check and Objects.checkFromIndexSize run before the port
;; reads (InflaterInputStream.java lines 153-161, DeflaterInputStream.java lines
;; 171-180). A bad range is IndexOutOfBoundsException (Preconditions.java lines
;; 92-100, 394-397). Only DeflaterInputStream's null message is set. read() with
;; nothing in the port's buffer reads one byte through the class's read, so it
;; never asks the wrapped stream for more than the JDK's read() does.
(define (zin-read self z . args)
  (case (length args)
    ((0)
     (let ((port (zin-live-port self z)))
       (if (fx> (port-buffered port) 0)
           (zin-prior-read self)
           (let* ((bv (make-bytevector 1))
                  (n ((zin-read-proc z) z bv 0 1)))
             (if (fx= n 0) -1 (->num (bytevector-u8-ref bv 0)))))))
    ((1)
     (let ((b (zin-bytes-arg (car args))))
       ;; read(byte[] b) is read(b, 0, b.length): the array's length is read
       ;; first, so a nil array throws before the stream is checked, and the
       ;; check comes next. Without it a closed stream whose port outlived a
       ;; failed close would read on (DeflaterInputStream, zin-deflate-close).
       (when (jolt-nil? b)
         (throw-jvm 'NullPointerException "Cannot read the array length because \"b\" is null"))
       (zin-live-port self z)
       (zin-prior-read self b)))
    (else
     (let* ((b (zin-bytes-arg (car args)))
            (off (zip-int-arg (cadr args)))
            (len (zip-int-arg (caddr args))))
       (zin-live-port self z)
       (when (jolt-nil? b)
         (zip-throw "java.lang.NullPointerException"
                    (and (not (zip-inflater? (zin-codec z))) "Null buffer for read")))
       (let ((whole (bytevector-length (zip-bytes "read" b))))
         (when (or (< off 0) (< len 0) (> len (- whole off)))
           (zip-throw "java.lang.IndexOutOfBoundsException"
                      (format "Range [~a, ~a + ~a) out of bounds for length ~a" off off len whole))))
       (zin-prior-read self b (->num off) (->num len))))))

;; Up to COUNT bytes from the port into a new bytevector, reading until COUNT or
;; the end. get-bytevector-some! takes the end of file it meets, so a later read
;; asks the stream again, as the JDK's readNBytes loop over read does.
(define (zin-read-up-to port count)
  (let loop ((chunks '()) (total 0))
    (if (>= total count)
        (zin-join chunks total)
        (let* ((bv (make-bytevector (min 16384 (- count total))))
               (n (get-bytevector-some! port bv 0 (bytevector-length bv))))
          (if (eof-object? n)
              (zin-join chunks total)
              (loop (cons (if (fx= n (bytevector-length bv))
                              bv
                              (let ((c (make-bytevector n))) (bytevector-copy! bv 0 c 0 n) c))
                          chunks)
                    (+ total n)))))))
(define (zin-join chunks total)
  (let ((out (make-bytevector total)))
    (let loop ((cs (reverse chunks)) (off 0))
      (if (null? cs)
          out
          (begin (bytevector-copy! (car cs) 0 out off (bytevector-length (car cs)))
                 (loop (cdr cs) (+ off (bytevector-length (car cs)))))))))

;; readNBytes(len) | readNBytes(byte[], off, len) (InputStream.java).
(define (zin-read-n self z . args)
  (if (null? (cdr args))
      (let ((len (zip-int-arg (car args))))
        (when (< len 0) (throw-jvm 'IllegalArgumentException "len < 0"))
        (let ((port (zin-live-port self z)))
          (na-byte-array (if (= len 0) (make-bytevector 0) (zin-read-up-to port len)))))
      (let* ((b (zin-bytes-arg (car args)))
             (off (zip-int-arg (cadr args)))
             (len (zip-int-arg (caddr args))))
        (when (jolt-nil? b)
          (throw-jvm 'NullPointerException "Cannot read the array length because \"b\" is null"))
        (let ((whole (bytevector-length (zip-bytes "readNBytes" b))))
          (when (or (< off 0) (< len 0) (> len (- whole off)))
            (zip-throw "java.lang.IndexOutOfBoundsException"
                       (format "Range [~a, ~a + ~a) out of bounds for length ~a" off off len whole))))
        (if (= len 0)
            (->num 0)
            (let* ((bv (zin-read-up-to (zin-live-port self z) len))
                   (n (bytevector-length bv)))
              (ja-bv->bytes! bv 0 b off n)
              (->num n))))))

;; close(): the port's close runs the class's close procedure, and a throw from
;; the wrapped close comes out of it.
(define (zin-close self z)
  (zin-prior-close self)
  jolt-nil)

(register-host-methods! "in-stream"
  (list (cons "available" (zin-method "available" '(0) zin-prior-available zin-available))
        (cons "read" (zin-method "read" '(0 1 3) zin-prior-read zin-read))
        (cons "readNBytes" (zin-method "readNBytes" '(1 3) zin-prior-read-n zin-read-n))
        (cons "skip" (zin-method "skip" '(1) zin-prior-skip zin-skip))
        (cons "mark" (zin-method "mark" '(1) zin-prior-mark
                                 (lambda (self z limit) (zip-int-arg limit) (zin-prior-mark self limit))))
        (cons "reset" (zin-method "reset" '(0) zin-prior-reset (lambda (self z) (zin-prior-reset self))))
        (cons "markSupported" (zin-method "markSupported" '(0) zin-prior-mark-supported
                                          (lambda (self z) (zin-prior-mark-supported self))))
        (cons "close" (zin-method "close" '(0) zin-prior-close zin-close))))

;; class and instance? answer from the record's class name and the hierarchy: a
;; known class the stream's class does not extend is #f, not the in-stream's
;; general answer.
(register-class-arm! (lambda (x) (and (zin-of x) #t))
                     (lambda (x) (zin-fqn (zin-of x))))
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((z (and (symbol-t? type-sym) (zin-of val))))
      (if z
          (let* ((n (symbol-t-name type-sym))
                 (name (or (resolve-class-hint n) n)))
            (cond ((or (string=? name "java.lang.Object") (jch-isa? (zin-fqn z) name)) #t)
                  ((jch-known? name) #f)
                  (else 'pass)))
          'pass))))

(reg-ctor! '("InflaterInputStream" "java.util.zip.InflaterInputStream") make-inflater-input-stream)
(reg-ctor! '("DeflaterInputStream" "java.util.zip.DeflaterInputStream") make-deflater-input-stream)
