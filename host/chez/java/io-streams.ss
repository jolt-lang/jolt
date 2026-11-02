;; java.io byte/char streams over Chez ports. Each stream is a jhost wrapping a
;; Chez port, so buffering, EOF and binary<->char transcoding come from Chez
;; rather than a hand-rolled buffer.
;;
;;   in-stream    #(binary-input-port)            FileInputStream / ByteArrayInputStream
;;   out-stream   #(binary-output-port extract acc) FileOutputStream / ByteArrayOutputStream
;;   char-reader  #(textual-input-port pending-lf?) FileReader / InputStreamReader
;;   char-writer  #(textual-output-port)           FileWriter / OutputStreamWriter
;;
;; Buffered{Reader,Writer,Input,Output}Stream are buffering wrappers; Chez ports
;; are already buffered, so their constructors return the wrapped stream.
;;
;; Loaded after io.ss + natives-array.ss (uses make-jfile/slurp helpers + the
;; byte-array <-> bytevector bridge), and extends io.ss's reader-jhost? / slurp /
;; __close so the new readers/streams flow through slurp / line-seq / with-open.

;; --- byte input stream ------------------------------------------------------
;; The port is resolved through here rather than read straight out of the slot so
;; System/in can hold the SYMBOL 'stdin and open the process's standard input on
;; first use — the same live-resolution port-writer uses for 'out / 'err. A
;; program that never touches System/in never opens a handle on fd 0.
;;
;; This is the ONLY port jolt opens on fd 0, and everything that reads standard
;; input goes through it: System/in itself, the *in* reader below it, and the `-`
;; program source in cli-core.ss. A second port on the same descriptor buffers
;; ahead on its own, and whichever read first would eat input the other never
;; sees. Memoized under a mutex for the same reason across threads.
(define jolt-stdin-port-mu (make-mutex))
(define jolt-stdin-port-memo #f)
(define (jolt-stdin-binary-port)
  (unless jolt-stdin-port-memo
    (jolt-with-mutex jolt-stdin-port-mu
      (unless jolt-stdin-port-memo
        (set! jolt-stdin-port-memo (standard-input-port (buffer-mode block))))))
  jolt-stdin-port-memo)
(define (in-stream-port self)
  (let ((p (vector-ref (jhost-state self) 0)))
    (if (eq? p 'stdin) (jolt-stdin-binary-port) p)))
;; state: #(port mark-supported? mark-pos). mark/reset are only honoured for a
;; source that declares support, which on the JVM means ByteArrayInputStream
;; (and BufferedInputStream) but NOT FileInputStream — a Chez file port is
;; seekable, so "can I seek" is the wrong test and would answer true where the
;; JVM answers false. The constructor says.
(define (make-in-stream port . opt)
  (make-jhost "in-stream" (vector port (and (pair? opt) (eq? (car opt) 'markable)) 0)))
(define (in-stream-markable? self) (vector-ref (jhost-state self) 1))
(define (make-in-stream-markable port) (make-in-stream port 'markable))
(define (in-stream? x) (and (jhost? x) (string=? (jhost-tag x) "in-stream")))
;; The JDK class a stream or reader was made as. One tag serves every byte
;; input stream (and one every output stream, one every port-backed Reader), so
;; the class is carried in the state, at a slot past every optional one: a
;; constructor names its class as it builds the value, before anything else
;; holds the state, and (class x) answers it (jolt-iad8f). A stream nothing
;; classed keeps the generic answer. The padding is #f, which is what each
;; optional slot reads as when absent.
(define stream-class-slot 9)
(define (stream-classed! x cls)
  (let* ((st (jhost-state x)) (n (vector-length st)))
    (if (fx>? n stream-class-slot)
        (vector-set! st stream-class-slot cls)
        (let ((v (make-vector (fx+ stream-class-slot 1) #f)))
          (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! v i (vector-ref st i)))
          (vector-set! v stream-class-slot cls)
          (jhost-state-set! x v))))
  x)
(define (stream-class x)
  (let ((st (jhost-state x)))
    (and (vector? st) (fx>? (vector-length st) stream-class-slot)
         (vector-ref st stream-class-slot))))
;; Two optional slots follow, each #f or absent on an ordinary stream: slot 3 is
;; a piped stream's shared pipe (piped-cell, below), slot 4 a
;; PushbackInputStream's pushback state (in-stream-pushback, below). Slot 5 is
;; a zip stream's record (zip-in-streams.ss); slot 6, on a SequenceInputStream
;; only, closes its streams (in-stream-sequence-close).

;; The port behind a stream, or the JVM's IOException when it has been closed.
;; Reading a closed Chez port raises a classless host error instead, which no
;; (catch java.io.IOException …) can see and which prints with no class on it.
;; A piped stream does not close its port (it marks the shared pipe), so this
;; leaves those to pipe-read!'s own "Pipe closed".
(define (in-stream-live-port self)
  (let ((port (in-stream-port self)))
    (if (port-closed? port) (io-throw "Stream closed") port)))

;; InputStream.available: how many bytes can be read without blocking. jolt
;; answered 0, which the JVM's contract permits ("an estimate") but which leaves
;; (pos? (.available in)) false forever, so a loop written to drain what has
;; arrived never runs a single iteration.
;;
;; A seekable source knows its remainder exactly, and that is what
;; FileInputStream and ByteArrayInputStream answer. A pipe or a terminal has no
;; length, so the count is what Chez has buffered — and when the buffer is empty
;; but the descriptor is ready, one lookahead fills it: that consumes nothing and
;; turns the kernel's read into a real count. A port that cannot say whether it
;; is ready keeps whatever it has buffered, because a lookahead there would
;; block, and blocking is the one thing available must not do.
;;
;; The lookahead is the LAST resort. A descriptor is asked directly first —
;; ioctl(FIONREAD), the same syscall the JVM's available() makes — which is both
;; exact and free of side effects. Filling the buffer is a real read(2): the
;; bytes move from the kernel into the port, still readable through this stream
;; but no longer there for a descriptor handed to a subprocess, and the answer
;; is capped by the buffer. That is where a target without ioctl ends up.
(define (port-buffered port)
  (fx- (port-input-size port) (port-input-index port)))

;; FIONREAD — the kernel's own count of what can be read from a descriptor, the
;; same question the JVM's available() asks. It is the only way to answer for a
;; pipe or a terminal: those have no length, so everything else jolt can see is
;; what it has already buffered.
;;
;; ioctl is (int fd, unsigned long request, ...), and the binding says so. A
;; fixed-arity one puts the third argument in a register where Apple arm64's
;; callee reads the stack, and the call then returns SUCCESS with the
;; out-parameter untouched — a wrong count that looks like a right one.
;;
;; Resolved once and lazily, and #f when there is no ioctl to call: available
;; falls back to what it can see itself, which is what every platform without
;; this syscall gets.
(define fionread-request
  (if (eq? (sa-os-family) 'macos) #x4004667F #x541B))
(define fionread-fn 'unresolved)
(define (fionread-proc)
  (when (eq? fionread-fn 'unresolved)
    ;; a duplicate on a race is the same procedure, so this needs no lock
    (set! fionread-fn
          (and (not (eq? (sa-os-family) 'windows))
               (jolt-foreign-proc-safe (__varargs_after 2) "ioctl" '(int unsigned-long void*) 'int))))
  fionread-fn)

;; The count for PORT's descriptor, or #f when it cannot be had — no ioctl, no
;; descriptor behind the port, or a descriptor that does not answer FIONREAD (a
;; regular file on macOS says ENOTTY).
(define (port-fionread port)
  (let ((f (fionread-proc))
        (fd (guard (_ (#t #f)) (port-file-descriptor port))))
    (and f fd (fixnum? fd) (fx>=? fd 0)
         (let* ((out (sa-foreign-alloc 4))
                (n (guard (_ (#t #f))
                     (sa-foreign-set! 'int out 0 0)
                     (and (>= (f fd fionread-request out) 0)
                          (let ((c (sa-foreign-ref 'int out 0)))
                            (and (>= c 0) c))))))
           (sa-foreign-free out)
           n))))
;; What is left of a seekable source, or #f when it is not one. The two
;; predicates are not a promise: a port over a SOCKET descriptor answers #t to
;; both and then raises ESPIPE ("illegal seek") from port-position, so this asks
;; by trying rather than by asking.
(define (port-remaining port)
  (guard (_ (#t #f))
    (and (port-has-port-length? port)
         (port-has-port-position? port)
         (max 0 (- (port-length port) (port-position port))))))
(define (in-stream-available self)
  (let ((port (in-stream-live-port self))
        (p (piped-pipe self)))
    (cond
      ;; a pushback stream: what its port holds — the pushed-back bytes and any
      ;; block already pulled from the wrapped stream — plus the wrapped
      ;; stream's own count, asked through ITS available (a proxy's included)
      ((in-stream-pushback self)
       => (lambda (pb)
            (+ (port-buffered port)
               (jnum->exact (record-method-dispatch (pushback-inner pb) "available" jolt-nil)))))
      ;; a piped stream holds its bytes in two places: what the writer has queued
      ;; and what an earlier read already pulled into the port's buffer
      (p (+ (port-buffered port) (pipe-available p)))
      (else (port-available port)))))
;; The same count for a bare byte port, whatever stream (if any) owns it.
(define (port-available port)
  (cond
    ((port-remaining port) => values)
    ;; the kernel's count plus whatever was already pulled out of it into the
    ;; port's buffer — the bytes live in two places, as they do for a pipe
    ((port-fionread port) => (lambda (n) (+ (port-buffered port) n)))
    ((fx>? (port-buffered port) 0) (port-buffered port))
    ((guard (_ (#t #f)) (input-port-ready? port))
     (lookahead-u8 port)
     (port-buffered port))
    (else 0)))

;; --- will a read block? -------------------------------------------------------
;; Reader.ready() has to see through the layers a Reader is made of: the
;; decoder's own read-ahead, the byte port under it, and — for InputStreamReader
;; — the java.io stream under THAT. A custom Chez port keeps that state in a
;; closure nothing outside can ask (input-port-ready? on one raises), so each
;; custom port jolt builds for a Reader registers how to ask it here.
(define port-ready-probes (make-weak-eq-hashtable))
(define port-ready-probes-mu (make-mutex))
(define (port-ready-probe! port probe)
  (jolt-with-mutex port-ready-probes-mu (hashtable-set! port-ready-probes port probe))
  port)
(define (port-ready-probe port)
  (jolt-with-mutex port-ready-probes-mu (hashtable-ref port-ready-probes port #f)))
;; Would reading a byte from BP answer without blocking?
(define (byte-port-ready? bp)
  (guard (_ (#t #f))
    (cond ((fx>? (port-buffered bp) 0) #t)
          ((port-ready-probe bp) => (lambda (probe) (probe)))
          (else (> (port-available bp) 0)))))

;; InputStream.skip over a file moves the position instead of reading the bytes
;; through: to the end of a 20MB file that is one lseek, where reading took 12ms.
;; FileInputStream.skip is lseek(cur + n) and answers n, whether or not that is
;; past the end (the next read then answers -1), and a target before the start
;; of the file is IOException "Invalid argument", the errno text (JDK 25).
;;
;; A target the file system will not take (Long/MAX_VALUE on ext4, past its
;; s_maxbytes) is the same IOException there; macOS takes it. tmpfs and xfs on
;; Linux take it too, and the next read is then EINVAL, which reaches the caller
;; as the same "Invalid argument" (host-faults.ss port-errno-fault-message).
;;
;; Only a file stream takes this path: a port with no descriptor
;; (ByteArrayInputStream, a pipe's or a zip entry's custom port) or one that
;; cannot say where it is (a pipe or terminal on fd 0, which raises ESPIPE from
;; port-position) answers #f and keeps the reading loop. A pushback stream has
;; bytes of its own ahead of the port, so it stays on the loop too. A
;; BufferedInputStream over a file (the markable stream) hands a forward skip to
;; the file the way the JDK's does, and answers 0 for n <= 0.
;; #f when the stream is not seekable, else the count skipped.
(define (in-stream-skip-by-seek! self port n)
  (and (not (and (in-stream-markable? self) (<= n 0)))
       (not (in-stream-pushback self))
       (not (piped-pipe self))
       (let ((pos (guard (_ (#t #f))
                    (and (port-file-descriptor port) (port-position port)))))
         (and pos
              (let ((target (+ pos n)))
                (when (< target 0) (io-throw "Invalid argument"))
                (guard (_ (#t (io-throw "Invalid argument")))
                  (set-port-position! port target))
                (->num n))))))

;; InputStream.skip by reading: n <= 0 is 0, and the bytes go through a bounded
;; buffer, so a count past the end (Long/MAX_VALUE) stops at end of stream.
(define (in-stream-skip-by-reading! port n)
  (if (<= n 0)
      0
      (let ((buf (make-bytevector (min n 8192))))
        (let loop ((left n) (done 0))
          (if (zero? left)
              done
              (let ((k (get-bytevector-n! port buf 0 (min left 8192))))
                (if (eof-object? k)
                    done
                    (loop (- left k) (+ done k)))))))))

(register-host-methods! "in-stream"
  (list
   (cons "read"
         (lambda (self . rest)
           (let ((port (in-stream-live-port self)))
             (if (null? rest)
                 ;; InputStream.read() returns the byte as an UNSIGNED int 0..255
                 ;; (-1 at EOF) — the one place a byte is not signed, because the
                 ;; return has to distinguish 0xff from end-of-stream.
                 ;;
                 ;; A pushback stream with nothing buffered asks the wrapped
                 ;; stream's OWN read(), as the JVM's does (buffer, else
                 ;; super.read()): a refill through the port would ask for a
                 ;; whole block, and a reify's read(byte[],int,int) may well
                 ;; block until it has all of it — the JVM's own default does.
                 (let ((pb (in-stream-pushback self)))
                   (if (and pb (fx=? (port-buffered port) 0))
                       (record-method-dispatch (pushback-inner pb) "read" jolt-nil)
                       (let ((b (get-u8 port))) (if (eof-object? b) -1 (->num b)))))
                 ;; read(buf …) fills a byte-array, whose elements ARE signed.
                 ;; Returns as soon as ANY byte is there, which is the JVM's
                 ;; contract ("blocks until at least one byte is available") and
                 ;; not "until the buffer is full": filling it first hung a read
                 ;; over a pipe or a terminal, where the rest of the buffer only
                 ;; arrives after whatever the program does with these bytes.
                 (let* ((buf (car rest))
                        (off (if (>= (length rest) 3) (jnum->exact (cadr rest)) 0))
                        (len (if (>= (length rest) 3) (jnum->exact (caddr rest)) (ja-len buf)))
                        (v (and (jolt-array? buf) (jolt-array-vec buf))))
                   (cond
                     ((<= len 0) (->num 0))
                     ;; STRAIGHT INTO THE ARRAY'S OWN BACKING. A byte array is
                     ;; backed by a bytevector, which is exactly what the port
                     ;; fills, so the read needs no buffer of its own: this used
                     ;; to allocate (make-bytevector len) on EVERY call and then
                     ;; block-copy it into the array. Under a decoded read that
                     ;; callback runs once per block for the whole stream, so a
                     ;; large read allocated and copied the entire input a second
                     ;; time on its way through.
                     ((and (bytevector? v) (fixnum? off) (fixnum? len)
                           (fx>=? off 0) (fx<=? (fx+ off len) (bytevector-length v)))
                      (let ((n (in-stream-read-some! self port v off len)))
                        (if (eof-object? n) -1 (->num n))))
                     ;; a boxed backing (a promoted array, or an older image) or
                     ;; a range the caller got wrong keeps the buffered path,
                     ;; where ja-bv->bytes! raises at the offending index
                     (else
                      (let* ((tmp (make-bytevector (max len 1)))
                             (n (in-stream-read-some! self port tmp 0 len)))
                        (if (eof-object? n) -1
                            (begin (ja-bv->bytes! tmp 0 buf off n) (->num n)))))))))))
   (cons "readAllBytes" (lambda (self) (let ((bv (get-bytevector-all (in-stream-live-port self))))
                                         (na-byte-array (if (eof-object? bv) (make-bytevector 0) bv)))))
   (cons "skip" (lambda (self n)
                  (let ((port (in-stream-live-port self)) (n (jnum->exact n)))
                    (or (in-stream-skip-by-seek! self port n)
                        (->num (in-stream-skip-by-reading! port n))))))
   (cons "available" (lambda (self) (->num (in-stream-available self))))
   ;; PushbackInputStream.unread — see the pushback section below. Any other
   ;; stream has no such method, and says so the way every missing method does.
   (cons "unread" (lambda (self b . rest)
                    (if (in-stream-pushback self)
                        (in-stream-unread! self b rest)
                        (dispatch-miss self "unread" (cons b rest)))))
   ;; A piped stream marks its shared pipe instead of closing the Chez port: a
   ;; read after close has to raise the JVM's IOException, and a closed Chez port
   ;; raises a classless host error before the port's own reader is consulted.
   ;; A SequenceInputStream closes its streams and leaves its own port open:
   ;; a read after close is the end of input on the JVM, not an IOException.
   (cons "close" (lambda (self)
                   (let ((p (piped-pipe self)) (sq (in-stream-sequence-close self)))
                     (cond (p (pipe-close-read! p))
                           (sq (sq))
                           (else (close-port (in-stream-port self)))))
                   jolt-nil))
   (cons "connect" (lambda (self other) (pipe-connect! self other) jolt-nil))
   ;; readNBytes(len) reads UP TO len bytes and answers what it got — unlike
   ;; read(byte[],off,len), it does not stop at the first short read, and it
   ;; answers an empty array at EOF rather than -1.
   (cons "readNBytes"
         (lambda (self . args)
           (if (= 1 (length args))
               (let ((len (jnum->exact (car args))))
                 (when (< len 0) (throw-jvm (quote IllegalArgumentException) "len < 0"))
                 (let ((bv (get-bytevector-n (in-stream-live-port self) len)))
                   (na-byte-array (if (eof-object? bv) (make-bytevector 0) bv))))
               ;; (b off len) -> the count actually read, 0 at EOF (never -1)
               (let* ((buf (car args))
                      (off (jnum->exact (cadr args)))
                      (len (jnum->exact (caddr args))))
                 (when (or (< off 0) (< len 0) (> (+ off len) (ja-len buf)))
                   (throw-jvm (quote IndexOutOfBoundsException) "readNBytes range"))
                 (if (<= len 0) (->num 0)
                     (let ((bv (get-bytevector-n (in-stream-live-port self) len)))
                       (if (eof-object? bv) (->num 0)
                           (let ((n (bytevector-length bv)))
                             (ja-bv->bytes! bv 0 buf off n)
                             (->num n)))))))))
   ;; transferTo drains this stream into an OutputStream and answers the count.
   (cons "transferTo"
         (lambda (self out)
           (let ((bv (get-bytevector-all (in-stream-live-port self))))
             (if (eof-object? bv) (->num 0)
                 (let ((n (bytevector-length bv)))
                   (if (out-stream? out)
                       (put-bytevector (out-stream-port out) bv)
                       ;; any other shape of output stream (a library's shim):
                       ;; go through its own write(byte[],int,int), the way the JVM does
                       (record-method-dispatch out "write" (list->cseq (list (na-byte-array bv) (->num 0) (->num n)))))
                   (->num n))))))
   ;; mark/reset only where the source declares support (make-in-stream). The
   ;; JVM throws from reset() on a stream that does not support it; answering a
   ;; silent no-op there let a caller believe it had rewound.
   (cons "mark" (lambda (self . _)
                  (when (in-stream-markable? self)
                    (vector-set! (jhost-state self) 2 (port-position (in-stream-port self))))
                  jolt-nil))
   (cons "reset" (lambda (self)
                   (unless (in-stream-markable? self)
                     (io-throw "mark/reset not supported"))
                   (set-port-position! (in-stream-port self) (vector-ref (jhost-state self) 2))
                   jolt-nil))
   (cons "markSupported" (lambda (self) (if (in-stream-markable? self) #t #f)))
   (cons "toString" host-object-tostring)))

;; --- byte output stream -----------------------------------------------------
;; state #(port extract acc): extract/acc are #f for a file/passthrough stream;
;; a ByteArrayOutputStream carries the R6RS extraction proc + an accumulator
;; bytevector (Chez's extract resets the port, so snapshot on demand, not per write).
(define (out-stream-port self) (vector-ref (jhost-state self) 0))
(define (out-stream? x) (and (jhost? x) (string=? (jhost-tag x) "out-stream")))
;; The port to write, or the JVM's IOException once closed — the mirror of
;; in-stream-live-port: a closed Chez port raised a classless "put-u8: not
;; permitted on closed port" that no (catch java.io.IOException …) could see.
;; flush and a second close are no-ops after close on the JVM, so those two
;; ask port-closed? themselves rather than come through here.
(define (out-stream-live-port self)
  (let ((port (out-stream-port self)))
    (if (port-closed? port) (io-throw "Stream closed") port)))
(define (make-out-stream port) (make-jhost "out-stream" (vector port #f #f)))
(define (bv-concat a b)
  (if (= 0 (bytevector-length b)) a
      (let ((m (make-bytevector (+ (bytevector-length a) (bytevector-length b)))))
        (bytevector-copy! a 0 m 0 (bytevector-length a))
        (bytevector-copy! b 0 m (bytevector-length a) (bytevector-length b))
        m)))
;; all bytes written to a ByteArrayOutputStream so far (folds the latest extract
;; into the accumulator).
(define (baos-bytes self)
  (let* ((st (jhost-state self)) (port (vector-ref st 0)) (extract (vector-ref st 1)) (acc (vector-ref st 2)))
    (flush-output-port port)
    (let ((merged (bv-concat acc (extract))))
      (vector-set! st 2 merged) merged)))
(register-host-methods! "out-stream"
  (list
   (cons "write"
         (lambda (self x . rest)
           (let ((port (out-stream-live-port self)))
             (cond
               ((number? x) (put-u8 port (bitwise-and (jnum->exact x) #xff)))
               ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte))
                (let ((bv (na-bytearray->bv x)))
                  (if (pair? rest)
                      (put-bytevector port bv (jnum->exact (car rest)) (jnum->exact (cadr rest)))
                      (put-bytevector port bv))))
               ((bytevector? x) (put-bytevector port x))
               (else (throw-jvm (quote IllegalArgumentException) "OutputStream/write: unsupported argument")))
             ;; a pipe's whole point is that the reader sees the write; Chez
             ;; buffers a custom output port, so a producer streaming into a
             ;; response body would otherwise stall until close.
             (when (piped-pipe self) (flush-output-port port))
             jolt-nil)))
   (cons "flush" (lambda (self)
                   (unless (port-closed? (out-stream-port self))
                     (flush-output-port (out-stream-port self))
                     (let ((inner (out-stream-filter-inner self)))
                       (when inner (record-method-dispatch inner "flush" jolt-nil))))
                   jolt-nil))
   (cons "close" (lambda (self)
                   (unless (port-closed? (out-stream-port self))
                     (flush-output-port (out-stream-port self))
                     ;; a ByteArrayOutputStream's close is a no-op (toByteArray stays valid);
                     ;; a piped stream signals end-of-stream to its reader; a file
                     ;; stream's port is closed; a filter closes its port and the
                     ;; stream it wraps, as FilterOutputStream's close does.
                     (let ((p (piped-pipe self)) (inner (out-stream-filter-inner self)))
                       (cond (p (pipe-close-write! p))
                             ((vector-ref (jhost-state self) 1))
                             (inner (close-port (out-stream-port self))
                                    (record-method-dispatch inner "flush" jolt-nil)
                                    (record-method-dispatch inner "close" jolt-nil))
                             (else (close-port (out-stream-port self))))))
                   jolt-nil))
   (cons "connect" (lambda (self other) (pipe-connect! self other) jolt-nil))
   (cons "toByteArray" (lambda (self) (na-byte-array (bytevector-copy (baos-bytes self)))))
   (cons "size" (lambda (self) (->num (bytevector-length (baos-bytes self)))))
   (cons "reset" (lambda (self) (baos-bytes self) (vector-set! (jhost-state self) 2 (make-bytevector 0)) jolt-nil))
   (cons "toString" (lambda (self . cs) (decode-bytevector (baos-bytes self)
                                          (if (pair? cs) (list (jolt-str-render-one (car cs))) '()))))))
;; (str baos) is its toString — the collected bytes as text — as str is on the
;; JVM. It rendered as #object[java.io.OutputStream]; transit clients read the
;; payload back with (str baos).
(register-str-render! (lambda (x) (and (out-stream? x) (vector-ref (jhost-state x) 1) #t))
  (lambda (x) (decode-bytevector (baos-bytes x) '())))

;; --- the decoder every Reader reads through -----------------------------------
;; R6RS transcoded-port is Chez's UTF-8 codec, and that codec is not java.nio's:
;; it replaces malformed input per SEQUENCE where Java replaces per RUN, and it
;; swallows a leading BOM where Java hands back U+FEFF (natives-str.ss has the
;; table and the rules). Every Reader jolt hands out used to decode that way, so
;; the same bytes read through a Reader and through (String. bytes) came back
;; different, and a BOM'd file quietly lost its first character.
;;
;; Decoding is BULK, not character at a time. The obvious shape -- one
;; %utf8-java-step per character -- is a Scheme loop where the transcoder is C,
;; and it measured 573ms -> 808ms for line-seq over 8.1MB x3 and 224ms -> 526ms
;; for (slurp (io/reader f)). So each fill instead cuts the buffered bytes at a
;; sequence boundary and hands the whole run to utf8-bytes->string, which is the
;; same C decoder behind the same guard the whole-buffer path uses. The
;; character-at-a-time arm survives for the one case bulk cannot serve: a
;; sequence split across two reads, where there is nothing complete to cut.
;;
;; It must not read ahead further than it has to: io/reader over a fifo, a pipe
;; or a socket has to hand back a line as soon as the line is there (jolt-0nk),
;; so a fill blocks only while it has NOTHING to return and answers short
;; otherwise. get-bytevector-some! is what makes that possible -- plain
;; get-bytevector-n! blocks until the whole request is filled.
(define java-utf8-chunk 65536)

;; The end of the last COMPLETE sequence in bv[lo..end), or END when they are all
;; complete. Walks back to the last byte that could begin a sequence -- at most
;; four, since nothing is longer -- and asks the decoder itself whether that one
;; is finished, so the lead-byte special cases (E0/ED/F0/F4) are not restated
;; here. Trailing bytes with no lead among them are strays, and a stray is
;; complete on its own.
(define (%utf8-chunk-boundary bv lo end)
  (let loop ((k (fx- end 1)) (n 0))
    (cond
      ((or (fx<? k lo) (fx>=? n 4)) end)
      ((fx=? (fxand (bytevector-u8-ref bv k) #xC0) #x80) (loop (fx- k 1) (fx+ n 1)))
      (else
       (let-values (((c next) (%utf8-java-step bv k end #t)))
         (if c end k))))))

;; REPORT? (optional): malformed input is MalformedInputException, as a
;; reporting CharsetDecoder answers it, rather than U+FFFD. The check costs
;; nothing on the bulk path for well-formed input -- %utf8-java-plain? is already
;; asked there -- and only a run it turns down is decoded again, strictly.
(define (open-java-utf8-input-port bp id . report?)
  (let ((buf (make-bytevector java-utf8-chunk))
        (report? (and (pair? report?) (car report?)))
        (lo 0)
        (hi 0)
        (src-eof? #f))
    ;; Keep the unconsumed tail -- a sequence split across two reads lives there
    ;; -- and read in behind it. #t when at least one byte arrived.
    (define (refill!)
      (let ((left (fx- hi lo)))
        (when (fx>? lo 0)
          (bytevector-copy! buf lo buf 0 left)
          (set! lo 0)
          (set! hi left))
        (and (not src-eof?)
             (let ((got (get-bytevector-some! bp buf hi (fx- (bytevector-length buf) hi))))
               (cond ((eof-object? got) (set! src-eof? #t) #f)
                     ((fx=? got 0) #f)
                     (else (set! hi (fx+ hi got)) #t))))))
    (define (step-one! str start o terminal?)
      (let-values (((c next) (%utf8-java-step buf lo hi (not terminal?))))
        (when (and c report? (fx=? c #xFFFD))
          (let ((seq (make-bytevector (fx- next lo))))
            (bytevector-copy! buf lo seq 0 (fx- next lo))
            (charset-decode-report seq "UTF-8")))
        (and c
             (begin (string-set! str (fx+ start o) (integer->char c))
                    (set! lo next)
                    #t))))
    (define (read! str start count)
      (let loop ((o 0))
        (cond
          ((fx=? o count) o)
          ((fx>=? lo hi)
           ;; Nothing buffered. Block for more only while the caller has
           ;; received nothing at all; past that a short answer is the right
           ;; answer, and waiting would be the eager read jolt-0nk removed.
           (cond (src-eof? o)
                 ((fx>? o 0) o)
                 ((refill!) (loop o))
                 (else o)))
          (else
           ;; At most one character per byte, so a window of WANT bytes can
           ;; never overrun a request for WANT characters.
           (let* ((want (fx- count o))
                  (end (fx+ lo (fxmin want (fx- hi lo))))
                  (cut (if (and src-eof? (fx=? end hi))
                           end                     ; end of input: no sequence is pending
                           (%utf8-chunk-boundary buf lo end))))
             (cond
               ((fx>? cut lo)
                (let* ((len (fx- cut lo))
                       (slice (make-bytevector len)))
                  (bytevector-copy! buf lo slice 0 len)
                  (let* ((s (if report? (charset-decode-report slice "UTF-8") (utf8-bytes->string slice)))
                         (n (string-length s)))
                    ;; sa-string-copy-range!, not string-copy!: Chez's takes
                    ;; (from from-start to to-start count) and R7RS's the same
                    ;; five in the opposite direction, so the raw one compiles
                    ;; on both and copies backwards on one (CONTRACT.txt).
                    (sa-string-copy-range! str (fx+ start o) s 0 n)
                    (set! lo cut)
                    (loop (fx+ o n)))))
               ;; Nothing complete in the window: either the request has room
               ;; for fewer bytes than this sequence needs, or the sequence is
               ;; cut off at the buffer edge and the rest may still arrive.
               ((step-one! str start o #f) (loop (fx+ o 1)))
               ((fx>? o 0) o)
               ((refill!) (loop o))
               ;; the source really is finished, so the remainder is terminal
               (else (step-one! str start o #t) (loop (fx+ o 1)))))))))
    ;; Positions are BYTES of the underlying port -- where the next character
    ;; starts -- since lo..hi is read ahead that has not been handed out. Only
    ;; offered when the source has them: a socket does not, and advertising them
    ;; would make the Reader's mark/reset lie.
    (define (get-pos) (- (port-position bp) (fx- hi lo)))
    (define (set-pos! p)
      (set-port-position! bp p)
      (set! lo 0) (set! hi 0) (set! src-eof? #f))
    ;; Ready when a whole character already sits in lo..hi (or the source is
    ;; finished, and the tail is answered as it stands), or when the byte
    ;; source itself has more.
    (port-ready-probe!
     (make-custom-textual-input-port
      id read!
      (and (port-has-port-position? bp) get-pos)
      (and (port-has-set-port-position!? bp) set-pos!)
      (lambda () (close-port bp)))
     (lambda ()
       (or (and (fx<? lo hi) (or src-eof? (fx>? (%utf8-chunk-boundary buf lo hi) lo)))
           (byte-port-ready? bp))))))

;; --- char input (Reader) ----------------------------------------------------
;; state #(port pending-lf? scratch mark replay): pending-lf? is the \n owed by
;; a \r that ended the last line — see char-reader-get-char.
(define (char-reader-port self) (vector-ref (jhost-state self) 0))
(define (char-reader-pending-lf? self) (vector-ref (jhost-state self) 1))
(define (char-reader-pending-lf! self v) (vector-set! (jhost-state self) 1 v))
(define (char-reader? x) (and (jhost? x) (string=? (jhost-tag x) "char-reader")))
;; Slot 2 is the block-read scratch string (see char-reader-read-block!), grown
;; on demand and reused across calls so a streaming read allocates once rather
;; than once per call.
;;
;; Slots 3 and 4 are BufferedReader's mark/reset, kept above the port rather
;; than as a port position because the port is as often a pipe, a socket or a
;; terminal as a file, and the JVM's mark works over all of them: it is the
;; buffer that remembers, not the source. The mark is #f or
;; #(pending-lf-at-mark limit recorded count) — every unit read from the port
;; after mark() is pushed onto `recorded` (reversed) — and replay is the list
;; of units reset() put back, delivered before the port is read again. The
;; recording is of RAW port units, below the CR-LF skip, and the skip flag is
;; saved with the mark (BufferedReader's markedSkipLF), so a reset re-reads
;; exactly what the reader saw. A read past the limit keeps the mark up to
;; the JVM's default buffer size (8192): its readAheadLimit is the size it
;; guarantees, and it does not invalidate a mark the buffer still holds.
;;
;; Until 0.8.11 the streamed file reader took mark as a no-op and reset as
;; "seek to 0" — a peek written as mark/read/reset restarted the file every
;; time, and Selmer's template parser never reached EOF.
(define (make-char-reader port) (make-jhost "char-reader" (vector port #f #f #f '())))
(define (char-reader-mark self) (vector-ref (jhost-state self) 3))
(define (char-reader-mark! self m) (vector-set! (jhost-state self) 3 m))
(define (char-reader-replay self) (vector-ref (jhost-state self) 4))
(define (char-reader-replay! self l) (vector-set! (jhost-state self) 4 l))
(define char-reader-mark-floor 8192)
;; Is a mark or a replay in force? Then the block reads take the one-unit path
;; through char-reader-raw-get, which is what records and replays; a reader
;; nobody has marked keeps its get-string-n! fill.
(define (char-reader-buffered? self)
  (or (char-reader-mark self) (pair? (char-reader-replay self))))
;; One raw unit: the replay first, then the port; recorded when a mark is on.
(define (char-reader-raw-get self)
  (let* ((replay (char-reader-replay self))
         (c (if (pair? replay)
                (begin (char-reader-replay! self (cdr replay)) (car replay))
                (get-char (char-reader-port self))))
         (m (char-reader-mark self)))
    ;; `vector? m`, not `m`: an invalidated mark is the symbol 'invalid, and the
    ;; reader goes on being read from after one — the JVM drops the mark and
    ;; keeps delivering characters, and only reset() raises. Testing `m` here
    ;; walked into (vector-ref 'invalid 3) on the next unit.
    (when (and (vector? m) (not (eof-object? c)))
      (let ((n (fx+ (vector-ref m 3) 1)))
        (if (fx>? n (fxmax (vector-ref m 1) char-reader-mark-floor))
            (char-reader-mark! self 'invalid)         ; read past what a mark keeps
            (begin (vector-set! m 2 (cons c (vector-ref m 2)))
                   (vector-set! m 3 n)))))
    c))
(define (char-reader-mark-set! self limit)
  (char-reader-mark! self (vector (char-reader-pending-lf? self)
                                  (if (and (fixnum? limit) (fx>=? limit 0)) limit 0)
                                  '() 0)))
(define (char-reader-reset! self)
  (let ((m (char-reader-mark self)))
    (cond
      ((not m) (io-throw "Stream not marked"))
      ((eq? m 'invalid) (io-throw "Mark invalid"))
      (else
       ;; what was read since the mark goes back in front of whatever a
       ;; previous reset already put back and is not yet consumed
       (char-reader-replay! self (append (reverse (vector-ref m 2)) (char-reader-replay self)))
       (vector-set! m 2 '())
       (vector-set! m 3 0)
       (char-reader-pending-lf! self (vector-ref m 0))))))
;; One character, minus the \n owed by a \r that ended the last line. Chez's
;; get-line splits on \n ALONE, so every line of CRLF input came back with its
;; \r still attached and a line-oriented protocol (HTTP headers, SMTP, RESP)
;; never recognized its own blank-line terminator.
;;
;; A \r ends a line at once and the \n that may follow is left for the next read
;; to skip rather than peeked for: over a pipe or a socket a peek blocks, and a
;; terminal's last line would not come back until the user typed something more.
;; That is why java.io.BufferedReader keeps this flag, and it is the same flag
;; System/in's hand-written loop below keeps, for the same reason.
(define (char-reader-get-char self)
  (let* ((plain? (not (char-reader-buffered? self)))
         (c (if plain? (get-char (char-reader-port self)) (char-reader-raw-get self))))
    (if (not (char-reader-pending-lf? self))
        c
        (begin (char-reader-pending-lf! self #f)
               (if (eqv? c #\newline)
                   (if plain? (get-char (char-reader-port self)) (char-reader-raw-get self))
                   c)))))
;; The next line without its terminator, or the eof object at end of input. A
;; line that ends at eof is still a line; only an immediate eof is the end.
(define (char-reader-line self)
  ;; The first unit goes through char-reader-get-char for the LF a \r left
  ;; owed; after it nothing within this line can set that flag, so the rest of
  ;; an unmarked reader's line is read straight off the port — the mark and
  ;; replay tests are per line here, not per character.
  (let ((out (open-output-string))
        (port (char-reader-port self))
        (plain? (not (char-reader-buffered? self))))
    (let loop ((any? #f) (first? #t))
      (let ((c (if (and plain? (not first?)) (get-char port) (char-reader-get-char self))))
        (cond
          ((eof-object? c) (if any? (get-output-string out) c))
          ((char=? c #\newline) (get-output-string out))
          ((char=? c #\return) (char-reader-pending-lf! self #t) (get-output-string out))
          (else (write-char c out) (loop #t #f)))))))
;; 64 KB: the knee measured on this path. Below it the per-call overhead starts
;; to show again; above it the scratch stops paying for itself.
(define char-reader-block-size 65536)
;; …but a drain starts here and doubles up to it, so reading a small file does
;; not allocate a 64 KB buffer to hold 200 bytes.
(define char-reader-scratch-initial 4096)
(define (char-reader-scratch self want)
  (let ((cur (vector-ref (jhost-state self) 2)))
    (if (and (string? cur) (fx>=? (string-length cur) want))
        cur
        (let ((s (make-string want)))
          (vector-set! (jhost-state self) 2 s)
          s))))

;; Will the next read answer without blocking? Something reset() put back, a
;; character Chez already holds, or whatever the decoder under the port says
;; (port-ready-probe!) — a string port answers for itself. A \r that ended the
;; last line owes a \n that is skipped rather than returned, so a buffered \n
;; standing behind it is dropped first and the question asked again, as
;; java.io.BufferedReader's ready does.
(define (char-reader-ready? self)
  (let ((port (char-reader-port self)))
    (when (port-closed? port) (io-throw "Stream closed"))
    (when (and (char-reader-pending-lf? self)
               (not (char-reader-buffered? self))
               (fx>? (port-buffered port) 0))
      (when (eqv? (lookahead-char port) #\newline) (get-char port))
      (char-reader-pending-lf! self #f))
    (or (pair? (char-reader-replay self))
        (fx>? (port-buffered port) 0)
        (cond ((port-ready-probe port) => (lambda (probe) (probe)))
              (else (guard (_ (#t #f)) (input-port-ready? port)))))))

;; One character at a time, through the generic array setter — the shape every
;; unusual call keeps: a mark or a replay in force (each unit has to be
;; recorded or replayed), a \n owed by the last readLine, a backing that is not
;; a plain string or vector, or a range the caller got wrong, where ja-set!
;; raises the JVM's own ArrayIndexOutOfBoundsException at the offending index.
(define (char-reader-read-elementwise! self buf off len)
  (reader-fill-available! buf off len
                          (lambda () (char-reader-get-char self))
                          (lambda () (char-reader-ready? self))))

;; read(char[], off, len) in ONE port operation instead of one get-char per
;; character, and with the JVM's contract: block until at least one character
;; is there, then answer what is — not "until the buffer is full". That is
;; get-string-some!, the textual twin of the get-bytevector-some! in-stream's
;; read is built on. Filling the buffer with get-string-n! instead hung a
;; read over a pipe, a socket or a terminal until the sender had said LEN
;; characters' worth, so an SSE stream delivering a few dozen bytes at a time
;; looked dead (jolt-lang/jolt#1364).
;;
;; The per-char loop this replaced was the SLOWEST way to read: a megabyte cost
;; a million get-char and ja-set! calls, ~31 ms per MB against babashka's ~1.
;; A char array's backing IS a Chez string (natives-array.ss), so the read goes
;; straight into it: no scratch, no copy. An array on the older boxed backing —
;; promoted by a store no character can hold, or restored from an image written
;; before the string backing — reads through a reused scratch and copies out.
(define (char-reader-read-block! self buf off len)
  (let ((v (and (jolt-array? buf) (jolt-array-vec buf))))
    (cond
      ((not (and (fixnum? off) (fixnum? len) (fx>=? off 0) (fx>=? len 0)))
       (char-reader-read-elementwise! self buf off len))
      ((fx=? len 0) (->num 0))
      ;; the pending \n sits ABOVE the port: without resolving it first, a
      ;; readLine that stopped on \r followed by a read(char[]) would hand back
      ;; the \n that readLine had already accounted for
      ((or (char-reader-buffered? self) (char-reader-pending-lf? self))
       (char-reader-read-elementwise! self buf off len))
      ((and (string? v) (fx<=? (fx+ off len) (string-length v)))
       (let ((got (get-string-some! (char-reader-port self) v off len)))
         (if (eof-object? got) -1 (->num got))))
      ((and (vector? v) (fx<=? (fx+ off len) (vector-length v)))
       ;; a REUSED scratch rather than one the size of the request: a 1 MB
       ;; buffer allocated per call cost more than the copy it fed
       (let* ((s (char-reader-scratch self (fxmin len char-reader-block-size)))
              (got (get-string-some! (char-reader-port self) s 0 (fxmin len (string-length s)))))
         (if (eof-object? got)
             -1
             (do ((i 0 (fx+ i 1))) ((fx=? i got) (->num got))
               (vector-set! v (fx+ off i) (string-ref s i))))))
      (else (char-reader-read-elementwise! self buf off len)))))

(register-host-methods! "char-reader"
  (list
   (cons "read"
         (with-charbuffer-read
         (lambda (self . rest)
           (if (null? rest)
               (let ((c (char-reader-get-char self))) (if (eof-object? c) -1 (->num (char->integer c))))
               (let* ((buf (car rest))
                      (off (if (>= (length rest) 3) (jnum->exact (cadr rest)) 0))
                      (len (if (>= (length rest) 3) (jnum->exact (caddr rest)) (ja-len buf))))
                 (char-reader-read-block! self buf off len))))))
   (cons "readLine" (lambda (self) (let ((l (char-reader-line self))) (if (eof-object? l) jolt-nil l))))
   (cons "lines" (lambda (self)
                   (let loop ((acc '()))
                     (let ((l (char-reader-line self)))
                       (if (eof-object? l) (list->cseq (reverse acc)) (loop (cons l acc)))))))
   (cons "ready" char-reader-ready?)
   (cons "skip" (lambda (self n) (let loop ((i 0) (k (jnum->exact n)))
                                   (if (or (>= i k) (eof-object? (char-reader-get-char self))) (->num i)
                                       (loop (+ i 1) k)))))
   (cons "close" (lambda (self) (close-port (char-reader-port self)) jolt-nil))
   (cons "markSupported" (lambda (self) #t))
   (cons "mark" (lambda (self . rest)
                  (char-reader-mark-set! self (if (pair? rest) (jnum->exact (car rest)) 0))
                  jolt-nil))
   (cons "reset" (lambda (self) (char-reader-reset! self) jolt-nil))
   (cons "toString" host-object-tostring)))

;; --- java.io.BufferedReader -------------------------------------------------
;; BufferedReader's constructor was the identity over jolt's own readers (each
;; wraps a Chez port, already buffered, already carrying the method table), and
;; over a Reader the USER wrote it was a delegating wrapper reading one
;; character at a time. Neither reads ahead, and the JVM's does: what it has
;; buffered is gone from the reader it wraps, so a program reading both saw
;; different input on jolt (jolt-kjlp), and (class (BufferedReader. sr)) was
;; StringReader.
;;
;; So: java.io.BufferedReader, ported from the JDK's. It fills a char buffer of
;; its own from the wrapped object's .read -- the one method java.io.Reader
;; leaves abstract -- asking for the whole buffer in one call, so what it has
;; read ahead is gone from the wrapped reader, as on the JVM: (io/reader *in*)
;; then read-line on *in* finds the lines the BufferedReader already holds.
;; read / readLine / skip / ready / mark / reset work off that buffer.
;;
;; state #(inner skip-lf? cb next n mark limit mark-skip-lf?): cb is the char
;; array (#f once closed), cb[next..n) the unread chars, mark the marked index,
;; -1 unmarked or -2 invalidated, and skip-lf? that a readLine ended on \r so a
;; \n next is the same line end.
(define (reader-adapter? x) (and (jhost? x) (string=? (jhost-tag x) "reader-adapter")))
(define (reader-adapter-inner self) (vector-ref (jhost-state self) 0))
(define (make-reader-adapter inner . size)
  (let ((sz (if (pair? size) (car size) 8192)))
    (when (<= sz 0) (throw-jvm (quote IllegalArgumentException) "Buffer size <= 0"))
    (make-jhost "reader-adapter" (vector inner #f (na-char-array sz) 0 0 -1 0 #f))))
;; Does obj implement `name` at n arguments? A reify's method table is keyed by
;; NAME alone, so iface-method cannot answer the arity question for one; the
;; stored method is a plain procedure whose arity mask includes its own `self`,
;; so ask the procedure. A deftype's table is keyed by arity and iface-method
;; already answered.
(define (obj-has-method? obj name n)
  (let ((m (iface-method obj name n)))
    (and m (or (not (procedure? m))
               (logbit? (+ n 1) (procedure-arity-mask m))))))
;; A reify/proxy records the interfaces it was written against, and a
;; java.io.InputStream has a .read too — one that hands back BYTES. Decoding
;; those as characters would be a silent mojibake, so a value that declares
;; itself a byte stream is left to the coercion error it already got rather than
;; quietly read as latin-1.
(define (declares-class? x fqn)
  (and (jreify? x) (member fqn (jreify-host-tags x)) #t))
;; A stdlib class built on a host tagged-table (jolt.socket's SocketInputStream
;; and SocketOutputStream) says the same with a marker key, :jolt/in-stream or
;; :jolt/out-stream: it answers the whole java.io contract, read(byte[],int,int)
;; or write(byte[],int,int) included, so every coercion site can drive it the
;; way it drives a reify. Without it (io/reader (.getInputStream sock)) — the
;; ordinary way to read a socket in Clojure — and slurp, io/copy, io/writer and
;; io/output-stream over a socket's streams all raised "Cannot open".
;; :jolt/input-stream is an older, narrower marker (a read()-only byte shim that
;; slurp and io/copy drain a byte at a time) and is left as it is.
(define (marked-stream? x k)
  (and (htable? x) (jolt-truthy? (jolt-ref-get x k))))
(define in-stream-marker (keyword "jolt" "in-stream"))
(define out-stream-marker (keyword "jolt" "out-stream"))
(define (user-in-stream? x)
  (or (declares-class? x "java.io.InputStream") (marked-stream? x in-stream-marker)))
;; The concrete method NAME of java.io.InputStream / OutputStream for a marked
;; stream, or #f: what a reify declaring the class inherits (host-static.ss
;; abstract-class-method), for a tagged-table stream that defines only the
;; read / write the class leaves abstract. jolt.socket's SocketInputStream had
;; no readAllBytes.
(define (stream-contract-method obj name)
  (let ((h (cond ((marked-stream? obj in-stream-marker)
                  (hashtable-ref abstract-methods-tbl "java.io.InputStream" #f))
                 ((marked-stream? obj out-stream-marker)
                  (hashtable-ref abstract-methods-tbl "java.io.OutputStream" #f))
                 (else #f))))
    (and h (hashtable-ref h name #f))))
;; …and the other three java.io roots a reify/proxy extends. Each is the class it
;; declares to every coercion site — slurp, spit, io/reader, io/writer, io/copy,
;; io/input-stream, io/output-stream — which drive it through the read / write
;; the class leaves abstract, as the JVM's IOFactory does; the methods the class
;; supplies for free come from the tables below.
(define (user-out-stream? x)
  (or (declares-class? x "java.io.OutputStream") (marked-stream? x out-stream-marker)))
(define (user-writer? x) (declares-class? x "java.io.Writer"))
;; A value io/reader and BufferedReader should adapt rather than reject: not one
;; of jolt's own reader jhosts (those pass through), but something that answers
;; .read — the java.io.Reader contract as far as either of them needs it.
(define (user-reader? x)
  (and (not (jhost? x))
       (or (obj-has-method? x "read" 3) (obj-has-method? x "read" 0))
       (not (user-in-stream? x))))
;; One read of the wrapped reader into buf: the count, or -1 at end of input.
;; read(char[],int,int) is the abstract method, so a Reader has it -- every one
;; of jolt's does; a reify that wrote only read() gets a character and then the
;; rest while it says it is ready, which is how java.io.Reader's own
;; read(char[],int,int) would drive it.
(define (reader-adapter-inner-read inner buf off len)
  (if (or (jhost? inner) (obj-has-method? inner "read" 3))
      (let ((n (record-method-dispatch inner "read" (list->cseq (list buf (->num off) (->num len))))))
        (if (jolt-nil? n) -1 (jnum->exact n)))
      (jnum->exact
       (reader-fill-available! buf off len
         (lambda ()
           (let ((c (record-method-dispatch inner "read" jolt-nil)))
             (if (or (jolt-nil? c) (< (jnum->exact c) 0)) (eof-object) (integer->char (jnum->exact c)))))
         (lambda () (reader-ready? inner))))))
(define (ra-open! self)
  (or (vector-ref (jhost-state self) 2) (throw-jvm 'java.io.IOException "Stream closed")))
(define (ra-avail st) (- (vector-ref st 4) (vector-ref st 3)))
;; cb is a char[]; its backing is a Scheme string, which the scans and copies
;; below use directly
(define (ra-str st) (jolt-array-vec (vector-ref st 2)))
(define (ra-peek st) (string-ref (ra-str st) (vector-ref st 3)))
(define (ra-advance! st k) (vector-set! st 3 (+ k (vector-ref st 3))))
;; BufferedReader.fill: keep what a live mark still covers, then one read of the
;; wrapped reader for the rest of the buffer (again while it answers 0).
(define (ra-fill! self)
  (let* ((st (jhost-state self))
         (mark (vector-ref st 5))
         (dst (if (< mark 0)
                  0
                  (let ((delta (- (vector-ref st 3) mark))
                        (limit (vector-ref st 6)))
                    (if (>= delta limit)
                        (begin (vector-set! st 5 -2) (vector-set! st 6 0) 0)
                        (let* ((kept (substring (ra-str st) mark (+ mark delta))))
                          (when (> limit (ja-len (vector-ref st 2)))
                            (vector-set! st 2 (na-char-array limit)))
                          (sa-string-copy-range! (ra-str st) 0 kept 0 delta)
                          (vector-set! st 5 0)
                          (vector-set! st 3 delta)
                          (vector-set! st 4 delta)
                          delta)))))
         (cb (vector-ref st 2))
         (n (let loop ()
              (let ((n (reader-adapter-inner-read (vector-ref st 0) cb dst (- (ja-len cb) dst))))
                (if (= n 0) (loop) n)))))
    (when (> n 0)
      (vector-set! st 4 (+ dst n))
      (vector-set! st 3 dst))))
;; read(): one character code, -1 at end of input. The common case -- open, a
;; character buffered, no \n owed -- is one string-ref.
(define (ra-read-char self)
  (let* ((st (jhost-state self)) (cb (vector-ref st 2)) (i (vector-ref st 3)))
    (if (and cb (fx<? i (vector-ref st 4)) (not (vector-ref st 1)))
        (begin (vector-set! st 3 (fx+ i 1))
               (char->integer (string-ref (jolt-array-vec cb) i)))
        (ra-read-char-slow self))))
(define (ra-read-char-slow self)
  (ra-open! self)
  (let ((st (jhost-state self)))
    (let loop ()
      (when (<= (ra-avail st) 0) (ra-fill! self))
      (if (<= (ra-avail st) 0)
          -1
          (let ((c (char->integer (ra-peek st))))
            (ra-advance! st 1)
            (cond ((not (vector-ref st 1)) c)
                  (else (vector-set! st 1 #f) (if (= c 10) (loop) c))))))))
;; BufferedReader.read1: from the buffer, or straight into buf when the request
;; is at least a buffer's worth and nothing (a mark, an owed \n) needs the buffer
(define (ra-read1-into self buf off len)
  (let ((st (jhost-state self)))
    (define (copy)
      (if (<= (ra-avail st) 0)
          -1
          (let ((k (min len (ra-avail st))) (src (ra-str st)) (nx (vector-ref st 3))
                (v (and (jolt-array? buf) (jolt-array-vec buf))))
            (if (and (string? v) (<= (+ off k) (string-length v)))
                (sa-string-copy-range! v off src nx (+ nx k))
                (do ((i 0 (+ i 1))) ((= i k)) (ja-set! buf (+ off i) (string-ref src (+ nx i)))))
            (ra-advance! st k)
            k)))
    (define (after-fill)
      (when (and (vector-ref st 1) (> (ra-avail st) 0))
        (vector-set! st 1 #f)
        (when (char=? (ra-peek st) #\newline)
          (ra-advance! st 1)
          (when (<= (ra-avail st) 0) (ra-fill! self))))
      (copy))
    (cond ((> (ra-avail st) 0) (after-fill))
          ((and (>= len (ja-len (vector-ref st 2))) (< (vector-ref st 5) 0) (not (vector-ref st 1)))
           (reader-adapter-inner-read (vector-ref st 0) buf off len))
          (else (ra-fill! self) (after-fill)))))
;; read(cbuf, off, len): what the buffer has, then more only while the wrapped
;; reader says the next read would not block
(define (ra-read-into self buf off len)
  (ra-open! self)
  (if (= len 0)
      0
      (let ((n (ra-read1-into self buf off len)))
        (if (<= n 0)
            n
            (let loop ((n n))
              (if (and (< n len) (reader-ready? (reader-adapter-inner self)))
                  (let ((n1 (ra-read1-into self buf (+ off n) (- len n))))
                    (if (<= n1 0) n (loop (+ n n1))))
                  n))))))
;; readLine: \n, \r and \r\n end a line; a \r leaves skip-lf? set so the \n of
;; the pair is not the next line. The line, or #f at end of input.
(define (ra-read-line self)
  (ra-open! self)
  (let ((st (jhost-state self)) (out #f))
    (define (port!) (or out (begin (set! out (open-output-string)) out)))
    (let loop ((omit-lf? (vector-ref st 1)) (any? #f))
      (when (<= (ra-avail st) 0) (ra-fill! self))
      (if (<= (ra-avail st) 0)
          (and any? (let ((s (get-output-string out))) (and (> (string-length s) 0) s)))
          (begin
            (when (and omit-lf? (char=? (ra-peek st) #\newline)) (ra-advance! st 1))
            (vector-set! st 1 #f)
            (let ((cb (ra-str st)) (start (vector-ref st 3)) (end (vector-ref st 4)))
              (let scan ((i start))
                (cond
                  ((fx=? i end)
                   (put-string (port!) cb start (fx- end start))
                   (vector-set! st 3 end)
                   (loop #f #t))
                  ((let ((c (string-ref cb i))) (or (char=? c #\newline) (char=? c #\return)))
                   (vector-set! st 3 (fx+ i 1))
                   (when (char=? (string-ref cb i) #\return) (vector-set! st 1 #t))
                   ;; the common line sits whole in the buffer: no port at all
                   (if any?
                       (begin (put-string (port!) cb start (fx- i start)) (get-output-string out))
                       (substring cb start i)))
                  (else (scan (fx+ i 1)))))))))))
;; What is left, as a string: the buffered rest, then the wrapped reader's own
;; drain -- only over one of jolt's readers, so slurp keeps its fast path.
(define (ra-drain self drain-inner)
  (ra-open! self)
  (let ((st (jhost-state self)) (inner (reader-adapter-inner self)))
    (when (and (vector-ref st 1) (<= (ra-avail st) 0)) (ra-fill! self))
    (when (and (vector-ref st 1) (> (ra-avail st) 0))
      (when (char=? (ra-peek st) #\newline) (ra-advance! st 1)))
    (vector-set! st 1 #f)
    (if (>= (vector-ref st 3) (vector-ref st 4))
        (drain-inner inner)
        (let ((head (substring (ra-str st) (vector-ref st 3) (vector-ref st 4))))
          (vector-set! st 3 (vector-ref st 4))
          (string-append head (drain-inner inner))))))
(define (ra-skip self n)
  (when (< n 0) (throw-jvm (quote IllegalArgumentException) "skip value is negative"))
  (ra-open! self)
  (let ((st (jhost-state self)))
    (let loop ((r n))
      (if (<= r 0)
          n
          (begin
            (when (<= (ra-avail st) 0) (ra-fill! self))
            (if (<= (ra-avail st) 0)
                (- n r)
                (begin
                  (when (vector-ref st 1)
                    (vector-set! st 1 #f)
                    (when (char=? (ra-peek st) #\newline) (ra-advance! st 1)))
                  (let ((d (ra-avail st)))
                    (if (<= r d)
                        (begin (ra-advance! st r) n)
                        (begin (ra-advance! st d) (loop (- r d))))))))))))
(define (ra-ready? self)
  (ra-open! self)
  (let ((st (jhost-state self)) (inner (reader-adapter-inner self)))
    (when (vector-ref st 1)
      (when (and (<= (ra-avail st) 0) (reader-ready? inner)) (ra-fill! self))
      (when (> (ra-avail st) 0)
        (when (char=? (ra-peek st) #\newline) (ra-advance! st 1))
        (vector-set! st 1 #f)))
    (or (> (ra-avail st) 0) (reader-ready? inner))))
(register-host-methods! "reader-adapter"
  (list
   (cons "read"
         (with-charbuffer-read
         (lambda (self . rest)
           (if (null? rest)
               (ra-read-char self)
               (let-values (((buf off len) (reader-read-buf-args rest)))
                 (->num (ra-read-into self buf off len)))))))
   (cons "readLine" (lambda (self) (or (ra-read-line self) jolt-nil)))
   (cons "lines" (lambda (self)
                   (let loop ((acc '()))
                     (let ((l (ra-read-line self)))
                       (if l (loop (cons l acc)) (list->cseq (reverse acc)))))))
   (cons "skip" (lambda (self n) (->num (ra-skip self (jnum->exact n)))))
   (cons "ready" (lambda (self) (ra-ready? self)))
   (cons "markSupported" (lambda (self) #t))
   (cons "mark" (lambda (self limit)
                  (let ((limit (jnum->exact limit)) (st (jhost-state self)))
                    (when (< limit 0) (throw-jvm (quote IllegalArgumentException) "Read-ahead limit < 0"))
                    (ra-open! self)
                    (vector-set! st 6 limit)
                    (vector-set! st 5 (vector-ref st 3))
                    (vector-set! st 7 (vector-ref st 1))
                    jolt-nil)))
   (cons "reset" (lambda (self)
                   (ra-open! self)
                   (let* ((st (jhost-state self)) (mark (vector-ref st 5)))
                     (when (< mark 0)
                       (throw-jvm 'java.io.IOException (if (= mark -2) "Mark invalid" "Stream not marked")))
                     (vector-set! st 3 mark)
                     (vector-set! st 1 (vector-ref st 7))
                     jolt-nil)))
   ;; the wrapped reader's close, when it has one -- a Reader that implements
   ;; none is still a legal argument here, and closing it does nothing
   (cons "close" (lambda (self)
                   (let ((st (jhost-state self)) (inner (reader-adapter-inner self)))
                     (when (vector-ref st 2)
                       (dynamic-wind
                         void
                         (lambda () (when (obj-has-method? inner "close" 0)
                                      (record-method-dispatch inner "close" jolt-nil)))
                         (lambda () (vector-set! st 2 #f)))))
                   jolt-nil))
   (cons "toString" host-object-tostring)))

;; --- the java.io classes a reify/proxy extends ------------------------------
;; (proxy [java.io.InputStream] [] (read …)) inherits, on the JVM, every method
;; of InputStream it does not name; the proxy macro's fn stands in for ALL
;; overloads of a name it does name, and a method the class leaves abstract
;; that the body omits is a stub raising UnsupportedOperationException. These
;; tables are the four java.io roots' inherited surface (host-static.ss
;; register-abstract-methods!): each method takes the reify as self and reaches
;; the abstract read / write by dispatching on it, so the override runs, and the
;; abstract methods are the stubs. The JDK's own bodies, where they show:
;; InputStream's readers loop read(byte[],int,int) in blocks, Reader.skip reads
;; into a scratch buffer, Writer.append is write(String) / write(int).
(define (abstract-stub name)
  (lambda (self . _) (throw-jvm (quote UnsupportedOperationException) name)))
;; one read(b,off,len) on self -> the count, or -1
(define (user-read-into! self buf off len)
  (jnum->exact (record-method-dispatch self "read" (list->cseq (list buf (->num off) (->num len))))))
;; read(b,off,len) until LEN bytes or end of stream -> the count read
(define (user-read-fully! self buf off len)
  (let loop ((got 0))
    (if (fx>=? got len) got
        (let ((n (user-read-into! self buf (fx+ off got) (fx- len got))))
          (if (< n 0) got (loop (fx+ got n)))))))
;; everything left in the stream, read in 8K blocks like readAllBytes
(define (user-read-all self)
  (let ((block (na-byte-array 8192)))
    (let loop ((acc (make-bytevector 0)))
      (let ((n (user-read-into! self block 0 8192)))
        (if (< n 0)
            acc
            (let ((chunk (make-bytevector n)))
              (ja-bytes->bv! block 0 chunk 0 n)
              (loop (bv-concat acc chunk))))))))
(register-abstract-methods! "java.io.InputStream"
  (list
   (cons "read" (abstract-stub "read"))
   (cons "readAllBytes" (lambda (self) (na-bv->bytearray (user-read-all self))))
   (cons "readNBytes"
         (lambda (self . args)
           (if (= 1 (length args))
               (let ((len (jnum->exact (car args))))
                 (when (< len 0) (throw-jvm (quote IllegalArgumentException) "len < 0"))
                 (let* ((buf (na-byte-array len))
                        (n (user-read-fully! self buf 0 len)))
                   (if (= n len) buf
                       (let ((out (make-bytevector n)))
                         (ja-bytes->bv! buf 0 out 0 n)
                         (na-bv->bytearray out)))))
               (let ((buf (car args)) (off (jnum->exact (cadr args))) (len (jnum->exact (caddr args))))
                 (when (or (< off 0) (< len 0) (> (+ off len) (ja-len buf)))
                   (throw-jvm (quote IndexOutOfBoundsException) "readNBytes range"))
                 (->num (user-read-fully! self buf off len))))))
   (cons "skip"
         (lambda (self n)
           (let ((wanted (max 0 (jnum->exact n)))
                 (scratch (na-byte-array 2048)))
             (let loop ((done 0))
               (if (fx>=? done wanted) (->num done)
                   (let ((got (user-read-into! self scratch 0 (min 2048 (fx- wanted done)))))
                     (if (< got 0) (->num done) (loop (fx+ done got)))))))))
   (cons "transferTo"
         (lambda (self out)
           (let ((block (na-byte-array 8192)))
             (let loop ((total 0))
               (let ((n (user-read-into! self block 0 8192)))
                 (if (< n 0) (->num total)
                     (begin
                       (record-method-dispatch out "write" (list->cseq (list block (->num 0) (->num n))))
                       (loop (fx+ total n)))))))))
   (cons "available" (lambda (self) (->num 0)))
   (cons "close" (lambda (self) jolt-nil))
   (cons "mark" (lambda (self . _) jolt-nil))
   (cons "reset" (lambda (self) (io-throw "mark/reset not supported")))
   (cons "markSupported" (lambda (self) #f))))
(register-abstract-methods! "java.io.OutputStream"
  (list
   (cons "write" (abstract-stub "write"))
   (cons "flush" (lambda (self) jolt-nil))
   (cons "close" (lambda (self) jolt-nil))))
;; one read(cbuf,off,len) on a Reader -> the count, or -1
(define (user-read-chars! self buf off len)
  (jnum->exact (record-method-dispatch self "read" (list->cseq (list buf (->num off) (->num len))))))
(register-abstract-methods! "java.io.Reader"
  (list
   (cons "read" (abstract-stub "read"))
   (cons "close" (abstract-stub "close"))
   (cons "skip"
         (lambda (self n)
           (let ((wanted (max 0 (jnum->exact n)))
                 (scratch (na-char-array 1024)))
             (let loop ((done 0))
               (if (fx>=? done wanted) (->num done)
                   (let ((got (user-read-chars! self scratch 0 (min 1024 (fx- wanted done)))))
                     (if (< got 0) (->num done) (loop (fx+ done got)))))))))
   (cons "transferTo"
         (lambda (self out)
           (let ((block (na-char-array 1024)))
             (let loop ((total 0))
               (let ((n (user-read-chars! self block 0 1024)))
                 (if (< n 0) (->num total)
                     (begin
                       (record-method-dispatch out "write" (list->cseq (list block (->num 0) (->num n))))
                       (loop (fx+ total n)))))))))
   (cons "ready" (lambda (self) #f))
   (cons "markSupported" (lambda (self) #f))
   (cons "mark" (lambda (self . _) (io-throw "mark() not supported")))
   (cons "reset" (lambda (self) (io-throw "reset() not supported")))))
(register-abstract-methods! "java.io.Writer"
  (list
   (cons "write" (abstract-stub "write"))
   (cons "flush" (abstract-stub "flush"))
   (cons "close" (abstract-stub "close"))
   ;; append(csq) is write(String); append(csq,start,end) the subsequence's;
   ;; append(char) is write(int)
   (cons "append"
         (lambda (self x . rest)
           (record-method-dispatch self "write"
             (jolt-list (if (and (char? x) (null? rest))
                            (->num (char->integer x))
                            (append-text x rest))))
           self))))

;; --- char output (Writer) ---------------------------------------------------
(define (char-writer-port self) (vector-ref (jhost-state self) 0))
(define (char-writer? x) (and (jhost? x) (string=? (jhost-tag x) "char-writer")))
;; Every jhost on the CHARACTER side of the io split, the mirror of reader-jhost?:
;; char-writer here, and StringWriter / *out* / io/writer-of-a-file over in io.ss
;; and host-static-classes.ss. A print-stream is deliberately absent -- it is a
;; java.io.OutputStream, so it is a legitimate thing to wrap in a writer.
(define (writer-jhost? x)
  (and (jhost? x)
       (member (jhost-tag x) '("char-writer" "writer" "port-writer"))
       #t))
;; state #(port downstream sink class): downstream is the stream this writer
;; wraps, when it wraps one; class is the java.io class it reports, #f for a
;; plain java.io.Writer. flush/close have to reach it — java.io.OutputStreamWriter.flush
;; flushes its own encoder AND the stream underneath, which is what makes a
;; PrintStream over a logging proxy emit on (println …), since println flushes.
(define (make-char-writer port . down)
  (make-jhost "char-writer" (vector port (and (pair? down) (car down)) #f #f)))
(define (char-writer-downstream self) (vector-ref (jhost-state self) 1))
;; The byte port a charset writer encodes into (make-charset-writer), or #f. A
;; flush has to reach it: the encoder hands it whole blocks and leaves them in
;; its buffer, rather than flushing it once per block.
(define (char-writer-sink self) (vector-ref (jhost-state self) 2))
(define (char-writer-class self) (vector-ref (jhost-state self) 3))
(define (char-writer-classed! w cls) (vector-set! (jhost-state w) 3 cls) w)
(define (buffered-char-writer? x)
  (and (char-writer? x) (equal? (char-writer-class x) "java.io.BufferedWriter")))
;; The port to write, or the JVM's IOException once closed, as out-stream-live-port
;; does for the byte side: a closed Chez port raised a classless "put-string: not
;; permitted on closed port". A second close is a no-op on the JVM, so close asks
;; port-closed? itself.
(define (char-writer-live-port self)
  (let ((port (char-writer-port self)))
    (if (port-closed? port) (io-throw "Stream closed") port)))
;; write and append take what every other Writer here takes (writer-piece-range,
;; append-text): an int as its character, a char[] or String with an (off len)
;; region, and append's (csq start end) subsequence with nil as "null".
(register-host-methods! "char-writer"
  (list
   (cons "write" (lambda (self x . rest)
                   (put-string (char-writer-live-port self) (writer-piece-range x rest))
                   jolt-nil))
   (cons "append" (lambda (self x . rest)
                    (put-string (char-writer-live-port self)
                                (if (and (null? rest) (char? x)) (string x) (append-text x rest)))
                    self))
   (cons "newLine" (lambda (self) (put-char (char-writer-live-port self) #\newline) jolt-nil))
   (cons "flush" (lambda (self)
                   (flush-output-port (char-writer-live-port self))
                   (let ((bp (char-writer-sink self)))
                     (when (and bp (not (port-closed? bp))) (flush-output-port bp)))
                   (let ((d (char-writer-downstream self)))
                     (when d (record-method-dispatch d "flush" jolt-nil)))
                   jolt-nil))
   (cons "close" (lambda (self)
                   (let ((port (char-writer-port self)))
                     (unless (port-closed? port)
                       (flush-output-port port)
                       ;; before the downstream close, which may close the
                       ;; stream the sink's buffered bytes are bound for
                       (let ((bp (char-writer-sink self)))
                         (when (and bp (not (port-closed? bp))) (flush-output-port bp)))
                       (let ((d (char-writer-downstream self)))
                         (when d (record-method-dispatch d "close" jolt-nil)))
                       (unless (port-closed? port) (close-port port))))
                   jolt-nil))
   (cons "toString" host-object-tostring)))

;; --- constructors -----------------------------------------------------------
(define utf8-tx (make-transcoder (utf-8-codec)))

;; --- text over bytes in a named charset ----------------------------------------
;; What io/reader and io/writer build when given :encoding, and what the
;; InputStreamReader / OutputStreamWriter / FileReader / FileWriter constructors
;; build when given a charset. Every one of those used to accept the charset and
;; read or write UTF-8 regardless. UTF-8 keeps the ports it always had; the
;; charset is checked by name first, as the JVM does when the reader or writer is
;; constructed.
;;
;; Writing: each block the port hands over is encoded through charset-encode-bv
;; (natives-str.ss), the same encoder .getBytes uses. UTF-16 marks its byte order
;; once, at the start of the stream, and is plain big-endian after that -- else
;; the mark would reappear at every block boundary.
;;
;; The encoded bytes stay in bp's buffer until the writer flushes or closes,
;; which is why only make-charset-writer calls this: it records bp so the
;; writer's flush reaches it. Flushing bp once per block made every non-UTF-8
;; write cost a write(2) per block, 8x the UTF-8 writer's time.
;;
;; REPORT? encodes as a CharsetEncoder does by default: a character the charset
;; cannot hold is UnmappableCharacterException when its block is encoded -- at a
;; flush or close, as the JDK's StreamEncoder raises it -- rather than "?".
(define (charset-output-port bp name . report?)
  (io-check-encoding! name)
  (if (io-utf8? name)
      (transcoded-port bp utf8-tx)
      (let ((enc name) (utf16? (string=? (charset-canonical-down name) "utf-16"))
            (encode (if (and (pair? report?) (car report?)) charset-encode-report charset-encode-bv)))
        (make-custom-textual-output-port "charset-writer"
          (lambda (str start count)
            (when (fx>? count 0)
              (put-bytevector bp (encode (substring str start (fx+ start count)) enc))
              (when utf16? (set! utf16? #f) (set! enc "UTF-16BE")))
            count)
          #f #f
          (lambda () (close-port bp))))))
;; A char-writer encoding into byte port BP in charset ENC. DOWN is the stream
;; object a flush and close also go to, or #f. OWN?: BP was opened for this
;; writer, so a charset the host cannot write closes it again; a stream the
;; caller handed in is left open, as the JVM's OutputStreamWriter leaves it.
(define (make-charset-writer bp enc down own? . cls)
  (make-reporting-charset-writer bp enc down own? #f (and (pair? cls) (car cls))))
;; …and one that encodes with REPORT? (charset-output-port).
(define (make-reporting-charset-writer bp enc down own? report? cls)
  (make-jhost "char-writer"
    (vector (if own?
                (guard (e (#t (close-port bp) (raise e))) (charset-output-port bp enc report?))
                (charset-output-port bp enc report?))
            down
            bp
            cls)))
;; Reading: a one-byte charset decodes as the bytes arrive, so a pipe still
;; streams. A multi-byte or stateful one decodes the whole input on the first
;; read, through decode-bytevector -- the decoder (String. bytes cs) and slurp
;; use -- because a block boundary can fall inside a character, and inside an
;; iconv shift state, and only the full input decodes the way the JVM's does.
;;
;; REPORT? decodes as a CharsetDecoder does by default: malformed input is
;; MalformedInputException at the read that reaches it, where the plain reader
;; answers U+FFFD.
(define (charset-input-port bp name id . report?)
  (io-check-encoding! name)
  (let ((cs (charset-canonical-down name))
        (report? (and (pair? report?) (car report?))))
    (cond
      ((string=? cs "utf-8") (open-java-utf8-input-port bp id report?))
      ;; a charset with no streaming reporter here: the whole input, decoded
      ;; through the reporting CharsetDecoder at the first read
      ((and report? (not (or (string=? cs "iso-8859-1") (string=? cs "us-ascii"))))
       (let ((text #f) (pos 0))
         (make-custom-textual-input-port id
           (lambda (str start count)
             (unless text
               (let ((bv (get-bytevector-all bp)))
                 (set! text (charset-decode-report (if (eof-object? bv) (make-bytevector 0) bv) name))))
             (let ((n (fxmin count (fx- (string-length text) pos))))
               (sa-string-copy-range! str start text pos (fx+ pos n))
               (set! pos (fx+ pos n))
               n))
           #f #f
           (lambda () (close-port bp)))))
      ((or (string=? cs "iso-8859-1") (string=? cs "us-ascii"))
       (let ((top (if (string=? cs "us-ascii") 127 255))
             (buf (make-bytevector 4096)))
         (port-ready-probe!
          (make-custom-textual-input-port id
           (lambda (str start count)
             (let ((n (get-bytevector-some! bp buf 0 (fxmin count 4096))))
               (if (eof-object? n)
                   0
                   (do ((i 0 (fx+ i 1))) ((fx=? i n) n)
                     (let ((b (bytevector-u8-ref buf i)))
                       ;; past 127 is not ASCII: U+FFFD, as decode-bytevector
                       ;; answers, or malformed to a reporting decoder
                       (when (and report? (fx>? b top)) (charset-report-malformed 1))
                       (string-set! str (fx+ start i) (if (fx<=? b top) (integer->char b) #\xFFFD)))))))
           #f #f
           (lambda () (close-port bp)))
          (lambda () (byte-port-ready? bp)))))
      (else
       ;; Decoded as the bytes arrive (make-java-decoder, natives-str.ss): a
       ;; read blocks only while nothing at all is decoded, so a reader over a
       ;; pipe or a socket answers as soon as a character is complete.
       (let* ((dec (or (make-java-decoder cs name) (unsupported-encoding-throw name)))
              (step (car dec))
              (finish (cdr dec))
              (buf (make-bytevector 4096))
              (lo 0) (hi 0) (src-eof? #f) (done? #f)
              (text "") (pos 0))
         ;; what is buffered, as text; #t when any came out
         (define (decode!)
           (let-values (((s next) (step buf lo hi src-eof?)))
             (set! lo next)
             (when src-eof? (set! done? #t) (finish))
             (set! text s)
             (set! pos 0)
             (fx>? (string-length s) 0)))
         (define (refill!)
           (when (fx>? lo 0)
             (bytevector-copy! buf lo buf 0 (fx- hi lo))
             (set! hi (fx- hi lo))
             (set! lo 0))
           (when (fx=? hi (bytevector-length buf))
             (let ((big (make-bytevector (fx* 2 hi))))
               (bytevector-copy! buf 0 big 0 hi)
               (set! buf big)))
           (let ((got (get-bytevector-some! bp buf hi (fx- (bytevector-length buf) hi))))
             (if (eof-object? got) (set! src-eof? #t) (set! hi (fx+ hi got)))))
         (port-ready-probe!
          (make-custom-textual-input-port id
           (lambda (str start count)
             (let loop ()
               (cond
                 ((fx<? pos (string-length text))
                  (let ((n (fxmin count (fx- (string-length text) pos))))
                    (sa-string-copy-range! str start text pos (fx+ pos n))
                    (set! pos (fx+ pos n))
                    n))
                 (done? 0)
                 ((and (or (fx<? lo hi) src-eof?) (decode!)) (loop))
                 (done? 0)
                 (else (refill!) (loop)))))
           #f #f
           (lambda () (finish) (close-port bp)))
          (lambda ()
            (or (fx<? pos (string-length text))
                (and src-eof? (not done?))
                (byte-port-ready? bp)))))))))
;; The byte port a stream's text is read from: jolt's own stream is its port; a
;; reify/proxy one is read through its own read method.
(define (stream-byte-port s)
  (if (in-stream? s) (in-stream-port s) (in-stream-source-port s)))
;; A charset argument to one of the constructors below: a name or a Charset.
(define (ctor-charset? x)
  (or (string? x) (and (jhost? x) (string=? (jhost-tag x) "charset"))))
(define (path-of x) (project-relative (file-path-of x)))
(define (src-bytevector x)   ; a byte[] or Chez bytevector -> bytevector
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        (else (throw-jvm (quote ClassCastException) "expected a byte array"))))

(define (reg-ctor! names ctor) (for-each (lambda (n) (register-class-ctor! n ctor)) names))

;; Every file-opening constructor here opens through this, so a failure arrives
;; as the JVM's java.io.FileNotFoundException rather than a raw Chez condition.
;; The guard and the classification live in io.ss, which loads first, because
;; slurp reaches a file WITHOUT passing through here (read-file-bytes-on-disk)
;; and the two have to answer alike; the note above open-path-guarded there says
;; what each reason means.
(define (open-file-guarded src thunk)
  (open-path-guarded (file-path-of src) (path-of src) thunk))

;; (io/reader path) and (io/reader (io/file path)): the file's own port, read on
;; demand.  open-file-guarded gives the same FileNotFoundException/`Is a directory`
;; reporting every other file-opening ctor here gives, so a missing path still
;; fails at io/reader time and with the same message it always did.
(define (jio-file-char-reader path . given)   ; GIVEN: the caller's spelling, for the message
  (io-note-file-read! path)
  (open-path-guarded (if (pair? given) (car given) path) path
    (lambda (p)
      (make-char-reader (open-java-utf8-input-port
                          (open-file-input-port p (file-options) (buffer-mode block))
                          p)))))

(reg-ctor! '("FileInputStream" "java.io.FileInputStream")
  (lambda (src . _)
    (open-file-guarded src
      (lambda (p) (stream-classed! (make-in-stream (open-file-input-port p (file-options) (buffer-mode block)))
                                   "java.io.FileInputStream")))))
(reg-ctor! '("FileOutputStream" "java.io.FileOutputStream")
  (lambda (src . rest)
    (let ((append? (and (pair? rest) (jolt-truthy? (car rest)))))
      (open-file-guarded src
        (lambda (p)
          (stream-classed!
            (make-out-stream (open-file-output-port p
                               (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                               (buffer-mode block)))
            "java.io.FileOutputStream"))))))
(reg-ctor! '("ByteArrayInputStream" "java.io.ByteArrayInputStream")
  (lambda (bytes . rest)
    (let ((bv (src-bytevector bytes)))
      (stream-classed!
        (make-in-stream-markable (open-bytevector-input-port
                         (if (>= (length rest) 2)
                             (let ((off (jnum->exact (car rest))) (len (jnum->exact (cadr rest))))
                               (let ((sub (make-bytevector len))) (bytevector-copy! bv off sub 0 len) sub))
                             bv)))
        "java.io.ByteArrayInputStream"))))
;; --- java.io.RandomAccessFile -------------------------------------------------
;; state #(port writable?). One Chez file port serves both directions: "r" opens
;; an input port, "rw" (and "rws"/"rwd", which differ only in how eagerly the JDK
;; syncs) an input/output port that creates a missing file and keeps an existing
;; one's contents. The port is unbuffered because the JDK's every read and write
;; is a system call: a second handle on the file, or another process, sees a
;; write the moment it returns, and a read sees what is on disk now.
;;
;; Messages are the JDK 25 ones, taken from a Java probe against the same cases.
;; Every method on a closed file raises IOException "Stream Closed", including
;; getFilePointer and length. A write on an "r" file is "Bad file descriptor" and
;; its setLength "Invalid argument", the errno text of the system call that fails.
(define (raf-port self)
  (let ((p (vector-ref (jhost-state self) 0)))
    (if (port-closed? p) (io-throw "Stream Closed") p)))
(define (raf-writable? self) (vector-ref (jhost-state self) 1))
(define (raf-writable-port self)
  (let ((p (raf-port self)))
    (unless (raf-writable? self) (io-throw "Bad file descriptor"))
    p))
(define (raf-mode mode)
  (cond ((jolt-nil? mode)
         (throw-jvm 'NullPointerException
                    "Cannot invoke \"String.equals(Object)\" because \"mode\" is null"))
        ((and (string? mode) (string=? mode "r")) 'r)
        ((and (string? mode) (member mode '("rw" "rws" "rwd"))) 'rw)
        (else (throw-jvm 'IllegalArgumentException
                         (string-append "Illegal mode \"" (jolt-str-render-one mode)
                                        "\" must be one of \"r\", \"rw\", \"rws\", or \"rwd\"")))))
(define (raf-null-array!)
  (throw-jvm 'NullPointerException "Cannot read the array length because \"b\" is null"))
;; (buf) or (buf off len) -> buf, off, len; a range outside the array is the
;; JDK's IndexOutOfBoundsException, which carries no message
(define (raf-buffer-args who args)
  (cond ((and (pair? args) (null? (cdr args)))
         (when (jolt-nil? (car args)) (raf-null-array!))
         (values (car args) 0 (ja-len (car args))))
        ((and (pair? args) (pair? (cdr args)) (pair? (cddr args)) (null? (cdddr args)))
         (when (jolt-nil? (car args)) (raf-null-array!))
         (let ((off (jnum->exact (cadr args))) (len (jnum->exact (caddr args))))
           (when (or (< off 0) (< len 0) (> (+ off len) (ja-len (car args))))
             (throw-jvm 'IndexOutOfBoundsException jolt-nil))
           (values (car args) off len)))
        (else (throw-jvm 'IllegalArgumentException
                         (string-append "No matching method " who " found taking "
                                        (number->string (length args)) " args for class java.io.RandomAccessFile")))))
;; Read up to LEN bytes into BUF at OFF, as many as the file has. The count, or
;; #f at end of file; LEN 0 is 0 even there.
(define (raf-read-into! self buf off len)
  (let ((port (raf-port self)))
    (if (fx=? len 0)
        0
        (let* ((tmp (make-bytevector len))
               (n (get-bytevector-n! port tmp 0 len)))
          (if (or (eof-object? n) (fx=? n 0))
              #f
              (begin (ja-bv->bytes! tmp 0 buf off n) n))))))
(define (raf-length self)
  (let ((port (raf-port self)))
    (when (raf-writable? self) (flush-output-port port))
    (port-length port)))

(hashtable-set! jhost-tag->fqn "random-access-file" "java.io.RandomAccessFile")
(register-host-methods! "random-access-file"
  (list
   (cons "read"
         (lambda (self . args)
           (if (null? args)
               (let ((b (get-u8 (raf-port self)))) (if (eof-object? b) -1 (->num b)))
               (let-values (((buf off len) (raf-buffer-args "read" args)))
                 (->num (or (raf-read-into! self buf off len) -1))))))
   (cons "readFully"
         (lambda (self . args)
           (let-values (((buf off len) (raf-buffer-args "readFully" args)))
             (let ((n (raf-read-into! self buf off len)))
               (when (and (fx>? len 0) (or (not n) (fx<? n len)))
                 (throw-jvm 'EOFException jolt-nil))
               jolt-nil))))
   (cons "write"
         (lambda (self x . rest)
           (let ((port (raf-writable-port self)))
             (if (number? x)
                 (put-u8 port (bitwise-and (jnum->exact x) #xff))
                 (let-values (((buf off len) (raf-buffer-args "write" (cons x rest))))
                   (put-bytevector port (na-bytearray->bv buf) off len)))
             (flush-output-port port)
             jolt-nil)))
   (cons "seek"
         (lambda (self pos)
           (let ((port (raf-port self)) (pos (jnum->exact pos)))
             (when (< pos 0) (io-throw "Negative seek offset"))
             (set-port-position! port pos)
             jolt-nil)))
   (cons "getFilePointer" (lambda (self) (->num (port-position (raf-port self)))))
   (cons "length" (lambda (self) (->num (raf-length self))))
   (cons "setLength"
         (lambda (self new-len)
           (let ((port (raf-port self)) (n (jnum->exact new-len)))
             (when (or (< n 0) (not (raf-writable? self)))
               (io-throw "Invalid argument"))
             (let ((pos (port-position port)))
               (set-port-length! port n)
               ;; a pointer past the new end moves back to it, as the JDK does
               (when (> pos n) (set-port-position! port n)))
             jolt-nil)))
   (cons "skipBytes"
         (lambda (self n)
           (let* ((len (raf-length self)) (port (raf-port self)) (n (jnum->exact n)))
             (if (<= n 0)
                 (->num 0)
                 ;; the JDK's arithmetic: from a pointer past the end this
                 ;; moves back to the end and answers a negative count
                 (let* ((pos (port-position port))
                        (new (min (+ pos n) len)))
                   (set-port-position! port new)
                   (->num (- new pos)))))))
   (cons "close"
         (lambda (self)
           (let ((p (vector-ref (jhost-state self) 0)))
             (unless (port-closed? p) (close-port p)))
           jolt-nil))))
(jch-register-supers! "java.io.RandomAccessFile" '("java.io.Closeable"))
(reg-ctor! '("RandomAccessFile" "java.io.RandomAccessFile")
  (lambda args
    (unless (= (length args) 2)
      (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.io.RandomAccessFile"))
    (let ((mode (raf-mode (cadr args))))
      (when (jolt-nil? (car args)) (throw-jvm 'NullPointerException jolt-nil))
      (open-file-guarded (car args)
        (lambda (p)
          (if (eq? mode 'r)
              (make-jhost "random-access-file"
                          (vector (open-file-input-port p (file-options) (buffer-mode none)) #f))
              (make-jhost "random-access-file"
                          (vector (open-file-input/output-port p (file-options no-fail no-truncate) (buffer-mode none))
                                  #t))))))))

;; --- java.io.PrintStream ------------------------------------------------------
;; A byte stream that renders values as text. state #(target autoflush): the target
;; is anything answering write/flush — an out-stream, a port-writer, or a PROXY
;; over one, which is what clojure.tools.logging's log-stream builds. Writes go
;; through record-method-dispatch rather than a direct call so a proxy's override
;; is the thing that runs.
(define (ps-target self) (vector-ref (jhost-state self) 0))
(define (ps-autoflush? self) (vector-ref (jhost-state self) 1))
(define (print-stream? x) (and (jhost? x) (string=? (jhost-tag x) "print-stream")))
(define (ps-emit self str)
  (let ((t (ps-target self)))
    ;; a port-writer is jolt's process stream and takes text; every other target
    ;; is a byte stream, so encode.
    (if (and (jhost? t) (string=? (jhost-tag t) "port-writer"))
        (display str (port-writer-port t))
        ;; through write(byte[],int,int), the overload the JVM's PrintStream
        ;; hands its encoded text to
        (let ((bv (string->utf8 str)))
          (record-method-dispatch t "write"
            (list->cseq (list (na-byte-array bv) (->num 0) (->num (bytevector-length bv)))))))
    jolt-nil))
(define (ps-flush! self)
  (record-method-dispatch (ps-target self) "flush" jolt-nil)
  jolt-nil)
(define (ps-emit-line self str)
  (ps-emit self str)
  ;; autoFlush flushes on println, which is what makes a PrintStream over a
  ;; logging proxy emit one record per line.
  (when (ps-autoflush? self) (ps-flush! self))
  jolt-nil)
(register-host-methods! "print-stream"
  (list
   (cons "print" (lambda (self x) (ps-emit self (writer-piece x))))
   (cons "println" (lambda (self . xs)
                     (ps-emit-line self (if (null? xs) "\n" (string-append (writer-piece (car xs)) "\n")))))
   ;; printf and format are the same call, and both answer the stream
   (cons "printf" (lambda (self a . rest)
                    (jvm-format-pieces a rest (lambda (piece) (ps-emit self piece)))
                    self))
   (cons "format" (lambda (self a . rest) (jvm-format-pieces a rest (lambda (piece) (ps-emit self piece))) self))
   (cons "append" (lambda (self x . rest) (ps-emit self (append-text x rest)) self))
   (cons "write" (lambda (self x . rest)
                   (let ((t (ps-target self)))
                     ;; write(byte[]) is write(b, 0, b.length) on the JVM's
                     ;; PrintStream; the int and the ranged forms pass through
                     (record-method-dispatch t "write"
                       (list->cseq (if (and (null? rest) (jolt-array? x) (eq? (jolt-array-kind x) 'byte))
                                       (list x (->num 0) (->num (ja-len x)))
                                       (cons x rest)))))
                   ;; a written newline autoflushes too, as on the JVM
                   (when (and (ps-autoflush? self) (number? x)
                              (= (jnum->exact x) 10))
                     (ps-flush! self))
                   jolt-nil))
   (cons "flush" (lambda (self) (ps-flush! self)))
   (cons "close" (lambda (self) (ps-flush! self)
                   (record-method-dispatch (ps-target self) "close" jolt-nil) jolt-nil))
   ;; flush, then the target's own answer when it has one (System/out's
   ;; port-writer says whether the process stream has failed)
   (cons "checkError" (lambda (self)
                        (ps-flush! self)
                        (let ((t (ps-target self)))
                          (and (jhost? t) (host-method-ref (jhost-tag t) "checkError")
                               (jolt-truthy? (record-method-dispatch t "checkError" jolt-nil))))))
   (cons "toString" host-object-tostring)))
;; (PrintStream. out) / (PrintStream. out autoFlush). The charset arity is
;; accepted and ignored: jolt renders UTF-8.
(reg-ctor! '("PrintStream" "java.io.PrintStream")
  (lambda (out . rest)
    (make-jhost "print-stream"
                (vector out (and (pair? rest) (jolt-truthy? (car rest)))))))

;; --- System/in, System/out, System/err ---------------------------------------
;; The three streams the JVM hands every program. jolt had out and err as bare
;; port-writers and no `in` at all, so a namespace that read stdin the ordinary
;; ported way — (slurp System/in), (io/reader System/in), (io/copy System/in
;; System/out) — failed to LOAD: "No matching field or method: System/in".
;;
;; System.in is a java.io.InputStream, so it is an in-stream over a binary port on
;; fd 0 and not a transcoded view of (current-input-port): an InputStream is
;; bytes, and (io/copy System/in out) has to come out byte-identical for input
;; that isn't UTF-8 text. It is also the one place standard input is read from —
;; see the *in* seam below.
;;
;; System.out and System.err are java.io.PrintStreams, NOT the java.io.PrintWriter
;; *out* is; ported code branches on the class and hands them to anything taking
;; an OutputStream. They wrap the process port-writers registered in
;; host-static-classes.ss, which is where the underlying ports resolve.  autoFlush
;; is on, as it is for the JVM's two.
;; NOT markable, and a known divergence: the JVM's System.in is a
;; BufferedInputStream (markSupported true); honouring a mark on interactive
;; stdin would need replay buffering over fd 0 (see stdin-one-port-on-fd0), so
;; jolt answers false and reset throws cleanly. See known-divergences.edn.
(define system-in-stream (stream-classed! (make-jhost "in-stream" (vector 'stdin #f 0)) "java.io.BufferedInputStream"))
(define (sys-set-in! v)
  (for-each (lambda (c) (vector-set! (mutable-static-cell c "in" #t) 0 v))
            '("System" "java.lang.System"))
  jolt-nil)
(register-class-statics! "System"
  (list (cons "in" system-in-stream)
        (cons "setIn" (lambda (v) (sys-set-in! v)))))
(sys-set-in! system-in-stream)
(for-each (lambda (p)
            (sys-set-stream! (car p) (make-jhost "print-stream" (vector (cdr p) #t))))
          (list (cons "out" system-out-writer) (cons "err" system-err-writer)))

;; --- *in*: the reader over System/in ----------------------------------------
;; The JVM stacks the two rather than putting them side by side — System.in is the
;; byte stream, and *in* is a Reader built over it (RT.in is a
;; LineNumberingPushbackReader over an InputStreamReader over System.in) — so
;; read-line ultimately pulls its bytes out of System.in. jolt has the same shape:
;; the clojure.core *in* reader (50-io.clj) drives read-line / read / read+string
;; through this one seam, and this seam reads System/in.
;;
;; Standard input is therefore buffered in exactly one place, the Chez port on
;; fd 0, which is the layer the JVM's BufferedInputStream is. What jolt does not
;; add is the JVM's SECOND buffer: an InputStreamReader decodes into 8K of its
;; own, which is why (.read System/in) after a (read-line) answers -1 there with
;; the rest of stdin sitting inside the reader. Nothing here reads past the line
;; it returns, so that same read answers the next byte of the input.
;;
;; The other difference is that System/setIn redirects read-line, because the
;; stream is read out of the static on every line. The JVM builds *in* over
;; whatever System.in was at RT class-init, so a later setIn does not reach it and
;; with-in-str is the only way to feed read-line. Both differences are supersets:
;; on the JVM, mixing the two loses input and redirecting read-line is impossible.
;;
;; Waits in a nap, NOT inside the read. Chez's blocking read holds the whole
;; Scheme world while it waits: no other thread runs at all, so a future stopped
;; ticking and an agent stopped draining the moment the main thread reached a
;; prompt, and the SIGTERM watcher (concurrency.ss) could not wake to run the
;; shutdown hooks either. On the JVM those all keep running while a thread sits in
;; System.in.read(). The nap is collect-safe, so parking THERE and reading only
;; once the port has something leaves every other thread alive (jolt-p9ua). Backs
;; off 1ms -> 20ms, the shape proc-wait-blocking uses.
;;
;; Whether a port can answer input-port-ready? at all is settled ONCE per port
;; rather than under a guard per line — the guard was most of the cost when this
;; was measured. A port that cannot answer (a custom port, e.g. a pipe) reads the
;; blocking way. input-port-ready? and not char-ready?, which is textual only.
(define jolt-stdin-poll-step0 1)              ; 1ms
(define jolt-stdin-poll-step-max 20)          ; 20ms
(define jolt-stdin-poll-port (box #f))        ; the port the answer below is about
(define jolt-stdin-poll-ok (box #f))
(define (jolt-stdin-wait-ready! in)
  (unless (eq? (unbox jolt-stdin-poll-port) in)
    (set-box! jolt-stdin-poll-ok (guard (_ (#t #f)) (input-port-ready? in) #t))
    (set-box! jolt-stdin-poll-port in))
  (when (unbox jolt-stdin-poll-ok)
    (let loop ((step jolt-stdin-poll-step0))
      (unless (input-port-ready? in)
        ;; jolt-pause-ms and not (sleep): on a fiber this parks for the step and
        ;; gives the carrier up, so (read-line) from a go block no longer stops
        ;; every other fiber placed on that carrier until input arrives.
        (jolt-pause-ms step)
        (loop (min jolt-stdin-poll-step-max (* step 2)))))))

;; readLine ends a line on \n, on \r, or on \r\n. A \r ends the line at once; the
;; \n that may follow is taken then and there when the stream already has it, and
;; otherwise left for the next read to skip. Waiting to see whether one follows
;; would keep a terminal's last line from being returned until the user typed
;; something more, which is why BufferedReader keeps this flag; taking the byte
;; when it IS there keeps a stray \n out of what (.read System/in) sees next. The
;; flag names the stream it belongs to, so a System/setIn between the two halves
;; of a CRLF cannot make the new stream swallow a leading newline.
(define stdin-pending-lf (box #f))
(define stdin-cell (mutable-static-cell "System" "in" #t))
(define stdin-line-cap0 128)

;; The first byte of a line, minus the \n owed by a \r that ended the last one.
(define (stdin-first-byte v get)
  (if (eq? (unbox stdin-pending-lf) v)
      (begin (set-box! stdin-pending-lf #f)
             (let ((b (get))) (if (eqv? b 10) (get) b)))
      (get)))
;; utf8-bytes->string, not utf8->string: this is the decoder seam for
;; (read-line) off the process's own stdin, and it answers to the same rules as
;; every other Reader (natives-str.ss) -- a leading BOM is U+FEFF and not a
;; signature to swallow, and a malformed run costs what java.nio charges it.
(define (stdin-line-string buf i)
  (let ((out (make-bytevector i)))
    (bytevector-copy! buf 0 out 0 i)
    (utf8-bytes->string out)))

;; The \n of a CRLF, when the port can say it is already sitting there. A port
;; that cannot answer, or that has nothing yet, leaves it to the flag: peeking
;; would block, and blocking here is what this must not do.
(define (stdin-take-crlf! v port)
  (if (and (unbox jolt-stdin-poll-ok)
           (eq? (unbox jolt-stdin-poll-port) port)
           (input-port-ready? port))
      (when (eqv? (lookahead-u8 port) 10) (get-u8 port))
      (set-box! stdin-pending-lf v)))

;; System/in is an ordinary in-stream, so its bytes come straight off the port.
;; The dispatched twin below is the same loop over .read; kept apart because this
;; one is the one a piped read-line loop runs a million times, and reading the
;; byte through a passed-in procedure instead of inline costs it a quarter.
(define (stdin-line-from-port v port)
  (jolt-stdin-wait-ready! port)
  (let loop ((b (stdin-first-byte v (lambda () (get-u8 port))))
             (buf (make-bytevector stdin-line-cap0))
             (cap stdin-line-cap0)
             (i 0))
    (cond
      ((eof-object? b) (if (fx=? i 0) jolt-nil (stdin-line-string buf i)))
      ((fx=? b 10) (stdin-line-string buf i))
      ((fx=? b 13) (stdin-take-crlf! v port) (stdin-line-string buf i))
      ((fx=? i cap)
       (let ((grown (make-bytevector (fx* cap 2))))
         (bytevector-copy! buf 0 grown 0 cap)
         (bytevector-u8-set! grown i b)
         (loop (get-u8 port) grown (fx* cap 2) (fx+ i 1))))
      (else
        (bytevector-u8-set! buf i b)
        (loop (get-u8 port) buf cap (fx+ i 1))))))

;; Anything else System/setIn was handed — a proxy, a library's stream shim — is
;; read through its own .read, so an override is seen. One byte at a time, so the
;; line takes nothing the caller did not ask for.
(define (stream-read-byte v)
  (let ((b (record-method-dispatch v "read" jolt-nil)))
    (if (or (jolt-nil? b) (not (number? b)) (< (jnum->exact b) 0))
        (eof-object)
        (bitwise-and (jnum->exact b) #xff))))
(define (stdin-line-from-stream v)
  (let ((get (lambda () (stream-read-byte v))))
    (let loop ((b (stdin-first-byte v get))
               (buf (make-bytevector stdin-line-cap0))
               (cap stdin-line-cap0)
               (i 0))
      (cond
        ((eof-object? b) (if (fx=? i 0) jolt-nil (stdin-line-string buf i)))
        ((fx=? b 10) (stdin-line-string buf i))
        ((fx=? b 13) (set-box! stdin-pending-lf v) (stdin-line-string buf i))
        ((fx=? i cap)
         (let ((grown (make-bytevector (fx* cap 2))))
           (bytevector-copy! buf 0 grown 0 cap)
           (bytevector-u8-set! grown i b)
           (loop (get) grown (fx* cap 2) (fx+ i 1))))
        (else
          (bytevector-u8-set! buf i b)
          (loop (get) buf cap (fx+ i 1)))))))

;; The line of `s` starting at `from`, as [line next-index], or nil when `from`
;; is already at the end. Backs the IReader over a string (with-in-str's reader,
;; clojure/core/50-io.clj) — which knew only \n and so handed back a trailing \r
;; on CRLF input, the same divergence char-reader-line above fixes for the
;; port-backed readers. The rule lives here, beside the other two line readers in
;; this file, so the three cannot drift apart.
;;
;; It returns the resume index rather than just the terminator's so the caller
;; needs ONE scan: searching for \n and \r separately re-scans the whole tail for
;; whichever is absent, which is quadratic over \n-only input — the exact shape
;; test/io_scaling_test.clj gates.
(define (string-line-from s from)
  (let ((n (string-length s)))
    (if (fx>=? from n)
        jolt-nil
        (let loop ((i from))
          (cond
            ((fx=? i n) (jolt-vector (substring s from n) n))
            ((char=? (string-ref s i) #\newline) (jolt-vector (substring s from i) (fx+ i 1)))
            ((char=? (string-ref s i) #\return)
             (jolt-vector (substring s from i)
                          (if (and (fx<? (fx+ i 1) n) (char=? (string-ref s (fx+ i 1)) #\newline))
                              (fx+ i 2)
                              (fx+ i 1))))
            (else (loop (fx+ i 1))))))))
(def-var! "clojure.core" "__string-line-from"
  (lambda (s from) (string-line-from (jolt-str-render-one s) (jnum->exact from))))

;; The next line of System/in, its terminator stripped, or nil at end of input.
;; Without this seam (read-line) and the REPL call nil.
(def-var! "clojure.core" "__stdin-read-line"
  (lambda ()
    (let ((v (vector-ref stdin-cell 0)))
      (if (in-stream? v)
          (stdin-line-from-port v (in-stream-port v))
          (stdin-line-from-stream v)))))

(reg-ctor! '("ByteArrayOutputStream" "java.io.ByteArrayOutputStream")
  (lambda _
    (call-with-values open-bytevector-output-port
      (lambda (port extract)
        (stream-classed! (make-jhost "out-stream" (vector port extract (make-bytevector 0)))
                         "java.io.ByteArrayOutputStream")))))
(reg-ctor! '("FileReader" "java.io.FileReader")
  (lambda (src . rest)   ; (FileReader. f) | (FileReader. f charset)
    (let ((cs (if (and (pair? rest) (ctor-charset? (car rest))) (charset-arg-name (car rest)) "UTF-8")))
      (open-file-guarded src
        (lambda (p)
          (let ((bp (open-file-input-port p (file-options) (buffer-mode block))))
            (stream-classed!
              (make-char-reader (guard (e (#t (close-port bp) (raise e)))
                                  (charset-input-port bp cs p)))
              "java.io.FileReader")))))))
;; (FileWriter. f), (FileWriter. f append), (FileWriter. f charset) and
;; (FileWriter. f charset append). The append flag was read as "the first extra
;; argument", so a Charset there opened the file for APPEND.
(reg-ctor! '("FileWriter" "java.io.FileWriter")
  (lambda (src . rest)
    (let* ((cs? (and (pair? rest) (ctor-charset? (car rest))))
           (cs (if cs? (charset-arg-name (car rest)) "UTF-8"))
           (flag (if cs? (cdr rest) rest))
           (append? (and (pair? flag) (jolt-truthy? (car flag)))))
      (open-file-guarded src
        (lambda (p)
          (let ((bp (open-file-output-port p
                      (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                      (buffer-mode block))))
            (make-charset-writer bp cs #f #t "java.io.FileWriter")))))))
;; InputStreamReader / OutputStreamWriter decode / encode the wrapped byte stream
;; (UTF-8 default; an explicit charset is honored only as UTF-8 here).
;;
;; A byte port that pulls each block through the stream's OWN read method,
;; whatever kind of stream it is. Dispatching rather than transcoding the
;; stream's port directly is what lets a proxy's override see the read, and it
;; leaves the wrapped stream OPEN — R6RS transcoded-port takes ownership of the
;; port it is given, so (io/reader System/in) used to take standard input away
;; from System/in, and from read-line with it, the moment it was called.
;;
;; THE BUFFER IS REUSED, and each byte is copied ONCE. This callback sits under
;; every decoded read in the host — (io/reader stream), line-seq, slurp of a
;; stream, .read(char[]) — and it used to move every byte THREE times and
;; allocate twice per block: (na-byte-array (make-bytevector count)) built a
;; bytevector and then COPIED it into a fresh byte array (na-byte-array copies a
;; bytevector argument, because the caller usually keeps writing into its own),
;; na-bytearray->bv copied the array back out to a second bytevector, and
;; bytevector-copy! then moved it into the port's buffer. Only the last of those
;; was the port's actual business.
;;
;; IT BUFFERS, and that is the whole point. A transcoded port reads from the
;; binary port underneath it in 1024-BYTE units — measured, and not tunable:
;; custom-port-buffer-size has no effect on it — so serving each of those
;; requests with its own dispatch meant 7912 round trips per 8.1 MB, each one
;; consing an argument seq, boxing two numbers, hashing the jhost tag and then
;; hashing the method NAME. That is ~158k dispatches to read that file twenty
;; times, and it was the single largest remaining cost in every decoded read.
;;
;; So one dispatch fills a 64 KB byte array and the 1024-byte requests are
;; served out of it — 64x fewer crossings into the method layer. The bytes move
;; once either way: ja-bytes->bv! copies from the array's backing (a bytevector)
;; straight into the port's buffer at its offset.
;;
;; BLOCKING IS UNCHANGED. The refill asks for 64 KB but the stream's own read
;; answers as soon as ANY bytes are there — that is InputStream.read(byte[],
;; int,int)'s contract and what in-stream's get-bytevector-some! implements — so
;; a pipe, socket or terminal still delivers as data arrives rather than waiting
;; for a full buffer.
;; …but it STARTS SMALL and doubles, exactly as char-reader-scratch does, and
;; for the same reason: a buffer sized to the block rather than to the input
;; makes every stream pay 64 KB to hold a few hundred bytes. Reading one large
;; file cannot tell the difference, and a suite that opens many small ones
;; regresses — that regression has already shipped here once, from the
;; char-reader drain, and the small-file-drain arm of the fastpath ratio gate
;; caught this one (1.01 -> 1.57) before it left the branch.
(define stream-source-block 65536)
(define stream-source-initial 4096)
(define (in-stream-source-port in)
  (let ((arr #f) (cap 0) (pos 0) (lim 0))
   ;; ready when a block is still here, or when the stream's own available says
   ;; more is — a PipedInputStream's queue, a socket's kernel count
   (port-ready-probe!
    (make-custom-binary-input-port
     "stream-source"
     (lambda (bv start count)
       (when (fx=? pos lim)
         ;; grown only between refills, when the buffer holds nothing
         (let ((want (if (fx=? cap 0)
                         stream-source-initial
                         (fxmin stream-source-block (fx* cap 2)))))
           (when (fx>? want cap)
             (set! arr (na-byte-array want))
             (set! cap want)))
         (let ((n (jnum->exact (record-method-dispatch in "read"
                                 (list->cseq (list arr (->num 0) (->num cap)))))))
           (set! pos 0)
           (set! lim (if (and (fixnum? n) (fx>? n 0)) n 0))))
       (if (fx=? pos lim)
           0                                   ; the custom-port way of saying EOF
           (let ((n (fxmin count (fx- lim pos))))
             (ja-bytes->bv! arr pos bv start n)
             (set! pos (fx+ pos n))
             n)))
     #f #f
     ;; close reaches the wrapped stream, as InputStreamReader's does: a text
     ;; reader over a pipe that stopped here left the pipe open for good. Nothing
     ;; closes this port except a caller that owns the stream -- a copy or a
     ;; drain through it leaves it open.
     (lambda () (record-method-dispatch in "close" jolt-nil)))
    (lambda ()
      (or (fx<? pos lim)
          (> (jnum->exact (record-method-dispatch in "available" jolt-nil)) 0))))))
;; …and one dispatch that fills a caller-supplied range, for the pushback
;; port below, which asks the wrapped stream for exactly what its own buffer
;; wants rather than reading ahead of it.
(define (stream-read-into! in bv start count)
  (let* ((arr (na-byte-array (make-bytevector count)))
         (n (jnum->exact (record-method-dispatch in "read"
                           (list->cseq (list arr (->num 0) (->num count)))))))
    (if (<= n 0)
        0                                   ; the custom-port way of saying EOF
        (begin (bytevector-copy! (na-bytearray->bv arr) 0 bv start n) n))))
;; the rest of a reify/proxy InputStream, through its own read
(define (user-in-stream-bytes in)
  (let ((bv (get-bytevector-all (in-stream-source-port in))))
    (if (eof-object? bv) (make-bytevector 0) bv)))
;; BV into a reify/proxy OutputStream, through its write(byte[],int,int)
(define (user-out-stream-write! out bv)
  (record-method-dispatch out "write"
    (list->cseq (list (na-bv->bytearray bv) (->num 0) (->num (bytevector-length bv))))))
(reg-ctor! '("InputStreamReader" "java.io.InputStreamReader")
  (lambda (in . rest)
    ;; A Reader is already decoded characters, so there is nothing for an
    ;; InputStreamReader to decode and the JVM has no such constructor. Say so
    ;; here: the byte port below drives the wrapped stream's read(byte[],int,int),
    ;; which a Reader answers by writing CHARACTERS into the array, and the
    ;; failure then surfaces from whatever finally reads a char as a number --
    ;; far from the call that was actually wrong.
    (when (reader-jhost? in)
      (throw-jvm (quote IllegalArgumentException)
                 "InputStreamReader wraps an InputStream, not a Reader (it is already decoded)"))
    ;; (InputStreamReader. in decoder) decodes as the CharsetDecoder is set to:
    ;; REPORT, its default, refuses malformed input, where a charset replaces it.
    (let ((decoder (and (pair? rest) (charset-decoder-jhost? (car rest)) (car rest))))
      (stream-classed!
        (make-char-reader (charset-input-port (in-stream-source-port in)
                                              (cond (decoder (charset-name (decoder-charset decoder)))
                                                    ((and (pair? rest) (ctor-charset? (car rest)))
                                                     (charset-arg-name (car rest)))
                                                    (else "UTF-8"))
                                              "InputStreamReader"
                                              (and decoder (decoder-reports-malformed? decoder))))
        "java.io.InputStreamReader"))))
;; A byte port that hands each encoded block to the stream's OWN write method,
;; whatever kind of stream it is — a port-backed out-stream, a PrintStream, or a
;; proxy over one. Dispatching rather than writing to the stream's port directly
;; is what lets a proxy's override see the bytes; it also leaves the wrapped
;; stream open, where transcoding its port would close it (R6RS transcoded-port
;; takes ownership) and a later (.toString baos) would fail on a closed port.
(define (out-stream-sink-port out)
  (make-custom-binary-output-port
   "stream-sink"
   (lambda (bv start count)
     (let ((chunk (make-bytevector count)))
       (bytevector-copy! bv start chunk 0 count)
       ;; write(byte[],int,int), the overload the JVM's encoder calls — a
       ;; proxy's fn sees three arguments there, not one
       (record-method-dispatch out "write" (list->cseq (list (na-byte-array chunk) (->num 0) (->num count)))))
     count)
   #f #f (lambda () #f)))
(reg-ctor! '("OutputStreamWriter" "java.io.OutputStreamWriter")
  (lambda (out . rest)
    ;; The mirror of InputStreamReader above: a Writer takes characters, so there
    ;; is nothing to encode and the JVM has no such constructor.
    (when (writer-jhost? out)
      (throw-jvm (quote IllegalArgumentException)
                 "OutputStreamWriter wraps an OutputStream, not a Writer (it already takes characters)"))
    ;; (OutputStreamWriter. out encoder): a CharsetEncoder reports what its
    ;; charset cannot hold, where a charset writes "?".
    (let ((encoder (and (pair? rest) (charset-encoder-jhost? (car rest)) (car rest))))
      (make-reporting-charset-writer (out-stream-sink-port out)
                                     (cond (encoder (charset-name (vector-ref (jhost-state encoder) 0)))
                                           ((and (pair? rest) (ctor-charset? (car rest)))
                                            (charset-arg-name (car rest)))
                                           (else "UTF-8"))
                                     out #f (and encoder #t) "java.io.OutputStreamWriter"))))
;; BufferedOutputStream is its own stream, as on the JVM: (class x) is
;; BufferedOutputStream, and bytes written to it reach the wrapped stream through
;; that stream's own write when it flushes, closes or fills its 8192-byte buffer.
;; Slot 5 holds the wrapped stream, which flush and close then go on to. It used
;; to answer the wrapped stream itself (jolt-iad8f).
(define (out-stream-filter-inner self)
  (let ((st (jhost-state self)))
    (and (fx>? (vector-length st) 5) (vector-ref st 5))))
(define (make-buffered-out-stream inner . size)
  (let ((n (if (pair? size) (jnum->exact (car size)) 8192)))
    (when (<= n 0) (throw-jvm (quote IllegalArgumentException) "Buffer size <= 0"))
    (stream-classed!
      (make-jhost "out-stream"
                  (vector (parameterize ((custom-port-buffer-size n)) (out-stream-sink-port inner))
                          #f #f #f #f inner))
      "java.io.BufferedOutputStream")))
(for-each (lambda (n) (register-class-ctor! n make-buffered-out-stream))
          '("BufferedOutputStream" "java.io.BufferedOutputStream"))
;; A BufferedWriter is its own object, as on the JVM: it reports its class, and
;; what is written to it reaches the wrapped Writer when it flushes, closes or
;; fills its buffer. Over a jolt char-writer it writes into that writer's port,
;; which buffers already, and flush and close go on to the wrapped writer. Over
;; any other Writer (a StringWriter, *out*, a proxy) it is a port of its own,
;; buffering 8192 chars like the JVM's, that hands each block to the wrapped
;; writer's write.
(define (make-buffered-writer inner)
  (char-writer-classed!
   (if (char-writer? inner)
       (make-char-writer (char-writer-port inner) inner)
       (make-char-writer
        (parameterize ((custom-port-buffer-size 8192))
          (make-custom-textual-output-port "buffered-writer"
            (lambda (str start count)
              (when (fx>? count 0)
                (record-method-dispatch inner "write"
                                        (jolt-list (substring str start (fx+ start count)))))
              count)
            #f #f #f))
        inner))
   "java.io.BufferedWriter"))
(reg-ctor! '("BufferedWriter" "java.io.BufferedWriter")
  (lambda (inner . _) (make-buffered-writer inner)))
;; A BufferedReader is its own object over any Reader, jolt's or the program's:
;; the ported BufferedReader above, which reads ahead as the JDK's does.
(for-each (lambda (n)
            (register-class-ctor! n
              (lambda (inner . size)
                (let ((size (map jnum->exact size)))
                  (when (and (pair? size) (<= (car size) 0))
                    (throw-jvm (quote IllegalArgumentException) "Buffer size <= 0"))
                  (if (or (user-reader? inner) (reader-jhost? inner))
                      (apply make-reader-adapter inner size)
                      inner)))))
          '("BufferedReader" "java.io.BufferedReader"))
;; BufferedInputStream is the JVM's mark/reset provider: wrapping ANY stream
;; makes markSupported true there. jolt honours that when the underlying port
;; can actually seek (file, bytevector) — a fresh markable jhost over the SAME
;; port, so close propagates both ways like the JVM's. Over a port that cannot
;; seek (socket, pipe, stdin) the JVM would replay from its buffer; jolt has no
;; replay layer, so the wrapper stays the wrapped stream and markSupported
;; stays false — the honest answer, and a known divergence (reset throws
;; instead of silently rewinding wrong). See known-divergences.edn.
;; Over a port that cannot seek it is a stream of its own over the same port
;; (not markable), so it still reports BufferedInputStream and closing it closes
;; what it wraps; over a stream the program wrote, it reads through that
;; stream's own read.
(for-each (lambda (n)
            (register-class-ctor! n
              (lambda (inner . _)
                (cond
                  ((in-stream? inner)
                   (let ((p (vector-ref (jhost-state inner) 0)))
                     (stream-classed!
                       (if (and (port? p)
                                (port-has-port-position? p)
                                (port-has-set-port-position!? p))
                           (make-in-stream-markable p)
                           (make-in-stream p))
                       "java.io.BufferedInputStream")))
                  ((user-in-stream? inner)
                   (stream-classed! (make-in-stream (in-stream-source-port inner))
                                    "java.io.BufferedInputStream"))
                  (else inner)))))
          '("BufferedInputStream" "java.io.BufferedInputStream"))

;; --- java.io.PushbackInputStream ---------------------------------------------
;; An InputStream whose next bytes can be put back (bencode reads one byte to
;; classify a token and unreads it when it opens a netstring, which is how
;; babashka.pods and nrepl talk to a process). It is an in-stream like every
;; other: its port is a custom binary port that pulls each block through the
;; wrapped stream's OWN read — a proxy's override, a process or socket stream's
;; own read — and unread writes the bytes into the front of THAT port's buffer.
;; The port is the one place every reader of a stream goes: read / readAllBytes
;; / skip here, slurp, io/copy, an InputStreamReader over it, read-line over a
;; System/in it was setIn as. So a byte put back is the next byte for all of
;; them by construction, where a prefix buffer of its own would be honoured only
;; by the sites it was threaded through.
;;
;; pushback state #(inner capacity pb-end): capacity is the JVM's fixed buffer
;; size (unread past it is "Push back buffer is full"); pb-end the port-buffer
;; index where the pushed-back region ends, so the bytes still pending are
;; pb-end - index, and reading them back frees their room. A refill starts the
;; buffer over, and clears it.
(define (in-stream-pushback x)
  (let ((st (and (jhost? x) (jhost-state x))))
    (and (vector? st) (fx>? (vector-length st) 4) (vector-ref st 4))))
(define (pushback-inner pb) (vector-ref pb 0))
(define (pushback-pending pb port)
  (max 0 (fx- (vector-ref pb 2) (port-input-index port))))
(define (make-pushback-in-stream inner . rest)
  ;; A Reader is already decoded characters — the same guard InputStreamReader
  ;; makes, for the same reason: the byte port drives the wrapped value's
  ;; read(byte[],int,int), which a Reader answers with characters, and the
  ;; failure would surface from whatever finally reads a char as a number.
  (when (reader-jhost? inner)
    (throw-jvm (quote IllegalArgumentException)
               "PushbackInputStream wraps an InputStream, not a Reader (it is already decoded)"))
  (let ((capacity (if (pair? rest) (jnum->exact (car rest)) 1)))
    (when (<= capacity 0) (throw-jvm (quote IllegalArgumentException) "size <= 0"))
    (let* ((pb (vector inner capacity 0))
           (port (make-custom-binary-input-port
                  "pushback-source"
                  (lambda (bv start count)
                    (vector-set! pb 2 0)        ; drained: nothing pushed back is pending
                    (stream-read-into! inner bv start count))
                  #f #f
                  ;; close reaches the wrapped stream, as FilterInputStream's does
                  (lambda () (record-method-dispatch inner "close" jolt-nil)))))
      (make-jhost "in-stream" (vector port #f 0 #f pb)))))
(reg-ctor! '("PushbackInputStream" "java.io.PushbackInputStream") make-pushback-in-stream)

;; SequenceInputStream over two streams or an Enumeration of them: each is read
;; to its end, then closed and the next one taken, as the JDK's nextStream
;; does, so an empty stream in the middle is skipped rather than ending the
;; whole. close closes the current stream and every one not reached yet.
(define (make-sequence-in-stream . args)
  (let* ((two? (fx=? (length args) 2))
         (later (if two? (cdr args) '()))
         (e (if two? #f (car args)))
         (next (lambda ()
                 (cond (two? (if (null? later) #f
                                 (let ((s (car later))) (set! later '()) s)))
                       ((jolt-truthy? (record-method-dispatch e "hasMoreElements" jolt-nil))
                        (let ((s (record-method-dispatch e "nextElement" jolt-nil)))
                          (when (jolt-nil? s) (throw-jvm 'NullPointerException jolt-nil))
                          s))
                       (else #f))))
         (cur (if two? (car args) (next))))
    (define (advance!)
      (record-method-dispatch cur "close" jolt-nil)
      (set! cur (next)))
    (define (close-all!) (let loop () (when cur (advance!) (loop))))
    (make-jhost "in-stream"
      (vector (make-custom-binary-input-port
               "sequence-source"
               (lambda (bv start count)
                 (let loop ()
                   (if (not cur)
                       0
                       (let ((n (stream-read-into! cur bv start count)))
                         (if (fx>? n 0) n (begin (advance!) (loop)))))))
               #f #f close-all!)
              #f 0 #f #f #f close-all!))))
(define (in-stream-sequence-close x)
  (let ((st (and (jhost? x) (jhost-state x))))
    (and (vector? st) (fx>? (vector-length st) 6) (vector-ref st 6))))
(reg-ctor! '("SequenceInputStream" "java.io.SequenceInputStream") make-sequence-in-stream)

;; unread(int) | unread(byte[]) | unread(byte[] off len). The int is the byte's
;; low 8 bits — bencode hands back the SIGNED value read-byte narrowed to — and
;; a byte[]'s elements are signed too, so both go in as their u8 view.
(define (in-stream-unread! self b rest)
  (let* ((port (in-stream-live-port self))
         (bytes (cond
                  ((number? b) (let ((bv (make-bytevector 1)))
                                 (bytevector-u8-set! bv 0 (bitwise-and (jnum->exact b) #xff))
                                 bv))
                  ((and (jolt-array? b) (eq? (jolt-array-kind b) 'byte))
                   (let* ((bv (na-bytearray->bv b))
                          (off (if (>= (length rest) 2) (jnum->exact (car rest)) 0))
                          (len (if (>= (length rest) 2) (jnum->exact (cadr rest)) (bytevector-length bv))))
                     (when (or (< off 0) (< len 0) (> (+ off len) (bytevector-length bv)))
                       (throw-jvm (quote IndexOutOfBoundsException) "unread range"))
                     (let ((out (make-bytevector len)))
                       (bytevector-copy! bv off out 0 len)
                       out)))
                  (else (throw-jvm (quote IllegalArgumentException)
                                   "PushbackInputStream/unread: expects an int or a byte[]")))))
    (pushback-prepend! (in-stream-pushback self) port bytes)
    jolt-nil))

;; Put BYTES in front of the port's unread bytes. When the buffer has that much
;; room before its index — the bytes just read are still there — back the index
;; up and write over them; otherwise rebuild the buffer as the bytes followed by
;; what is still unread, no smaller than the block the port was reading in, so a
;; byte-at-a-time reader keeps pulling whole blocks from the wrapped stream.
(define (pushback-prepend! pb port bytes)
  (let* ((n (bytevector-length bytes))
         (idx (port-input-index port))
         (size (port-input-size port))
         (pending (pushback-pending pb port)))
    (when (fx>? (fx+ pending n) (vector-ref pb 1))
      (io-throw "Push back buffer is full"))
    (if (fx>=? idx n)
        (let ((at (fx- idx n)))
          (bytevector-copy! bytes 0 (port-input-buffer port) at n)
          (set-port-input-index! port at)
          (vector-set! pb 2 (fx+ idx pending)))
        (let* ((rest (fx- size idx))
               (buf (port-input-buffer port))
               (nb (make-bytevector (fxmax (bytevector-length buf) (fx+ n rest)))))
          (bytevector-copy! bytes 0 nb 0 n)
          (bytevector-copy! buf idx nb n rest)
          (set-port-input-buffer! port nb)
          (set-port-input-index! port 0)
          (set-port-input-size! port (fx+ n rest))
          (vector-set! pb 2 (fx+ n pending))))))

;; read(byte[] …) for any in-stream: what the port has, or one read from the
;; source when it has nothing (get-bytevector-some!). A pushback stream adds the
;; JVM's rule for the pushed-back bytes: they come first, and when they fall
;; short of LEN the wrapped stream is read ONCE for the rest — so a caller that
;; put a header back and asks for header + body in one read gets both, as it
;; does on the JVM, instead of the header alone.
;; START, not always 0: read(byte[] …) fills the caller's array's own backing in
;; place where it can (the byte-array arm above), so the destination offset is
;; the caller's off and not a scratch buffer's zero.
(define (in-stream-read-some! self port bv start len)
  (let ((pb (in-stream-pushback self)))
    (if (and pb (fx>? (pushback-pending pb port) 0))
        (let ((n (get-bytevector-some! port bv start len)))
          (if (or (eof-object? n) (fx>=? n len))
              n
              (let ((more (get-bytevector-some! port bv (fx+ start n) (fx- len n))))
                (if (eof-object? more) n (fx+ n more)))))
        (get-bytevector-some! port bv start len))))

;; --- integration: slurp / line-seq / with-open ------------------------------
;; a char-reader joins the reader-jhost set (drain-reader / line-seq read it via
;; its .read method).
(let ((prev reader-jhost?))
  (set! reader-jhost? (lambda (x) (or (char-reader? x) (reader-adapter? x) (prev x)))))

;; Drain a char-reader in BLOCKS.
;;
;; drain-reader's general arm (drain-reader-by-dispatch) asks for one character
;; at a time through record-method-dispatch — which finds the method table by
;; hashing the jhost's tag and the handler by hashing the method NAME, per
;; character — and conses each one onto a list to reverse and list->string at
;; the end. That is what every stream-backed (read rdr) paid: reading one form
;; out of a file through PushbackReader(InputStreamReader(FileInputStream))
;; measured ~70x slower than slurp + read-string over the same 332 files
;; (3909 ms vs 52 ms), and jolt was FASTER than babashka on the string form.
;;
;; Nothing about the buffering changes: the general arm drained the whole
;; reader too, and host-reader-read-form refills the pushback reader with the
;; unconsumed tail as a StringReader, so every subsequent read already took the
;; string cursor. Only the first drain was slow, and only because of how it
;; asked for the characters.
(define (char-reader-drain self)
  (let ((out (open-output-string))
        (port (char-reader-port self)))
    ;; the pending \r that char-reader-line may have left sits ABOVE the port
    (when (char-reader-pending-lf? self)
      (let ((c (char-reader-get-char self)))
        (unless (eof-object? c) (write-char c out))))
    ;; The scratch GROWS from small rather than starting at the block size.
    ;; Draining is what slurp and line-seq do, and most of those calls are small
    ;; files — a config, a fixture, a source file. Forcing a 64 KB allocation on
    ;; every one of them cost more than the per-character loop it replaced on a
    ;; suite that reads many small files (a 900 ms group of kmet namespaces went
    ;; to 1.8 s). Doubling on each full fill reaches the block size after a few
    ;; iterations, so large files pay a handful of extra rounds and small ones
    ;; allocate what they actually use.
    (let loop ((cap char-reader-scratch-initial))
      (let* ((s (char-reader-scratch self cap))
             (n (get-string-n! port s 0 cap)))
        (if (or (eof-object? n) (fx=? n 0))
            (get-output-string out)
            (begin
              (put-string out s 0 n)
              (loop (if (fx=? n cap)
                        (fxmin (fx* cap 2) char-reader-block-size)
                        cap))))))))

;; …and route drain-reader through it, for a bare char-reader and for the
;; PushbackReader-over-one that (read rdr) actually hands us. The pushback arm
;; mirrors io.ss's string-reader arm exactly — pushed characters sit above the
;; translation, so they are prepended raw and the wrapped rest is folded.
(let ((prev drain-reader))
  (set! drain-reader
        (lambda (r)
          (cond
            ((char-reader? r) (char-reader-drain r))
            ((and (jhost? r) (pushback-reader-tag? (jhost-tag r))
                  (char-reader? (vector-ref (jhost-state r) 0)))
             (let* ((st (jhost-state r))
                    (pushed (pbr-pushed-string r))
                    (rest (char-reader-drain (vector-ref st 0))))
               (vector-set! st 1 '())
               (string-append pushed (if (vector-ref st 2) (pbr-fold-and-count! st rest) rest))))
            (else (prev r))))))

;; slurp a char-reader (drain chars) or a byte in-stream (drain bytes -> decode).
;; slurp is a with-open over a reader on the JVM, so a stream or reader handed
;; to it is closed once read, or when reading it throws.
(define (slurp-closes? src)
  (or (in-stream? src) (user-in-stream? src) (char-reader? src) (reader-adapter? src)
      (user-reader? src) (reader-jhost? src)))
(let* ((prev jolt-slurp)
       (slurp-source
        (lambda (src . opts)
          (cond
            ((char-reader? src) (drain-reader src))
            ((reader-adapter? src) (drain-reader src))
            ((in-stream? src) (decode-bytevector (let ((bv (get-bytevector-all (in-stream-port src))))
                                                   (if (eof-object? bv) (make-bytevector 0) bv))
                                                 (slurp-encoding opts)))
            ;; a reify/proxy InputStream: its bytes through its own read, as the
            ;; JVM's InputStreamReader reads it; a reify/proxy Reader: through
            ;; the same adapter io/reader hands back for one
            ((user-in-stream? src) (decode-bytevector (user-in-stream-bytes src) (slurp-encoding opts)))
            ((user-reader? src) (drain-reader (make-reader-adapter src)))
            (else (apply prev src opts))))))
  (set! jolt-slurp
        (lambda (src . opts)
          (if (slurp-closes? src)
              (let ((text (guard (e (#t (jolt-close src) (raise e)))
                            (apply slurp-source src opts))))
                (jolt-close src)
                text)
              (apply slurp-source src opts))))
  (def-var! "clojure.core" "slurp" jolt-slurp))

;; spit to a stream or writer writes INTO it and closes it, as on the JVM where
;; spit wraps the target in a writer under with-open. jolt rendered the target as
;; a path, so (spit an-output-stream "x") silently created a file literally named
;; "#object[java.io.OutputStream]" and the stream stayed empty.
(let ((prev jolt-spit))
  (set! jolt-spit
        (lambda (target content . opts)
          (cond
            ((out-stream? target)
             (put-bytevector (out-stream-port target)
                             (string->utf8 (jolt-str-render-one content)))
             (jolt-close target) jolt-nil)
            ((char-writer? target)
             (put-string (char-writer-port target) (jolt-str-render-one content))
             (jolt-close target) jolt-nil)
            ;; the StringWriter / PrintWriter family (io.ss) accumulates through
            ;; its own .write method.
            ((and (jhost? target) (text-sink-tag? (jhost-tag target)))
             (record-method-dispatch target "write" (jolt-list (jolt-str-render-one content)))
             (jolt-close target) jolt-nil)
            ;; a reify/proxy OutputStream takes the UTF-8 bytes through its
            ;; write(byte[],int,int), the way the JVM's OutputStreamWriter hands
            ;; them over; a reify/proxy Writer takes the text
            ((user-out-stream? target)
             (user-out-stream-write! target (string->utf8 (jolt-str-render-one content)))
             (jolt-close target) jolt-nil)
            ((user-writer? target)
             (record-method-dispatch target "write" (jolt-list (jolt-str-render-one content)))
             (jolt-close target) jolt-nil)
            (else (apply prev target content opts)))))
  (def-var! "clojure.core" "spit" jolt-spit))

;; with-open closes a host object through its .close method: any jhost whose
;; class registers one (the streams and readers here, a ZipFile, a socket) is
;; Closeable. This used to name five tags, and each class added since had to
;; add its own arm or fail with-open, as ZipFile did.
(let ((prev jolt-close))
  (set! jolt-close
        (lambda (x)
          (if (or (and (jhost? x) (host-method-ref (jhost-tag x) "close"))
                  ;; a reify/proxy whose close is the one its class supplies
                  ;; (InputStream's and OutputStream's do nothing)
                  (and (jreify? x) (abstract-class-method x "close")))
              (begin (record-method-dispatch x "close" jolt-nil) jolt-nil)
              (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; --- clojure.java.io: byte streams + copy / make-parents / delete-file -------
;; input-stream/output-stream now yield real byte streams (were char reader/writer).
;; the file branches announce themselves to the AOT cache (io-note-file-read!,
;; io.ss): opening a resource for reading at compile time is a read like a slurp.
(define (jio-open-in-file p . given)   ; GIVEN: the caller's spelling, for the message
  (io-note-file-read! p)
  ;; The one funnel for io/input-stream's file arms -- a path, an io/file, a
  ;; file: URL -- and it opened unguarded, so a missing path came back carrying
  ;; Chez's own wording where every FileInputStream above reports the JVM's.
  (open-path-guarded (if (pair? given) (car given) p) p
    (lambda (rp) (make-in-stream (open-file-input-port rp (file-options) (buffer-mode block))))))
;; Both take io.clj's options. A byte stream has no charset, so :encoding is
;; accepted and has nothing to do, as on the JVM; :append opens the output file
;; without truncating it.
;; A java.net.Socket (jolt.socket builds it as a host tagged-table carrying its
;; :class): io.clj's IOFactory opens a socket's own streams, so io/reader,
;; io/writer, io/input-stream and io/output-stream all take one.
(define socket-class-key (keyword #f "class"))
(define (socket-value? x)
  (and (htable? x)
       (let ((c (jolt-ref-get x socket-class-key)))
         (and (string? c) (jch-isa? c "java.net.Socket")))))
(define (socket-stream x getter) (record-method-dispatch x getter jolt-nil))
;; What io/input-stream opens over a file or bytes is, on the JVM, a
;; BufferedInputStream, so mark/reset work on it; these streams' ports seek, so
;; they are made markable the way the BufferedInputStream ctor makes them.
(define (jio-open-in-file-buffered fs-path given)
  (jio-buffered-in (make-in-stream-markable (in-stream-port (jio-open-in-file fs-path given)))))
(define (jio-buffered-in s) (stream-classed! s "java.io.BufferedInputStream"))
(define (jio-input-stream x . opts)
  (io-opts-check! opts)
  (cond ((or (in-stream? x) (user-in-stream? x)) x)
        ((jfile? x) (jio-open-in-file-buffered (jfile-fs x) (jfile-path x)))
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (jio-buffered-in (make-in-stream-markable (open-bytevector-input-port (na-bytearray->bv x)))))
        ((bytevector? x) (jio-buffered-in (make-in-stream-markable (open-bytevector-input-port x))))
        ((and (jhost? x) (string=? (jhost-tag x) "url"))
         (if (jar-path? (url-spec x))
             (jar-path-stream (url-spec x))
             (let ((up (file-url->path (url-spec x)))) (jio-open-in-file-buffered up up))))
        ;; io/resource's answer for a resource baked into a built binary — a
        ;; java.net.URL with an openStream (io.ss embedded-res). Without this arm
        ;; (io/input-stream (io/resource "baked.txt")) threw in a built binary
        ;; while the same call on a file: URL worked. The content is the source
        ;; string or a bytevector, whichever the embed stored.
        ((embedded-res? x)
         (jio-buffered-in (make-in-stream (open-bytevector-input-port (embedded-res-bytes x)))))
        ;; an entry inside a jar on the roots streams out of the archive (io.ss)
        ((jar-path? x) (jar-path-stream x))
        ((string? x) (jio-open-in-file-buffered (io-source-path x) (if (file-url-string? x) (file-url->path x) x)))
        ((socket-value? x) (socket-stream x "getInputStream"))
        (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str x) "> as an InputStream.")))))
(define (jio-output-stream x . rest)
  (io-opts-check! rest)
  (cond ((or (out-stream? x) (user-out-stream? x)) x)
        ;; a "file:" string is the URL's file, as it is for the read side
        ((and (string? x) (file-url-string? x)) (apply jio-output-stream (file-url->path x) rest))
        ((or (jfile? x) (string? x))
         (let ((append? (io-append? rest)))
           ;; through the same classifier every read-side opener uses: this
           ;; opened bare, so a directory came back as Chez's "is a directory"
           ;; inside a java.io.IOException where the JVM raises
           ;; FileNotFoundException, and a missing parent carried Chez's wording
           ;; inside the right class (jolt-g81).
           (open-path-guarded (file-path-of x) (path-of x)
             (lambda (p)
               (stream-classed!
                 (make-out-stream (open-file-output-port p
                                    (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                                    (buffer-mode block)))
                 "java.io.BufferedOutputStream")))))
        ;; a file: URL writes its target, any other protocol raises (io.ss).
        ((url-jhost? x) (apply jio-output-stream (url-write-path x) rest))
        ;; System/out and System/err are already byte streams — pass them through,
        ;; the way an out-stream passes through.
        ((and (jhost? x) (text-sink-tag? (jhost-tag x))) x)
        ((socket-value? x) (socket-stream x "getOutputStream"))
        (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str x) "> as an OutputStream.")))))
(def-var! "clojure.java.io" "input-stream" jio-input-stream)
(def-var! "clojure.java.io" "output-stream" jio-output-stream)

;; io/reader and io/writer over a BYTE stream are an InputStreamReader and an
;; OutputStreamWriter on the JVM: the bytes, decoded / encoded. io.ss's coercions
;; predate the byte streams in this file and knew neither, so (io/reader
;; System/in) — the ordinary ported way to read stdin — and (io/writer
;; (FileOutputStream. f)) both raised "Cannot open <…> as a Reader/Writer".
;; A value that is already a reader/writer passes through, as io/reader and
;; io/writer do for every other reader/writer.
(let ((prev jolt-io-reader))
  (set! jolt-io-reader
        (lambda (x . opts)
          (io-opts-check! opts)
          (let ((enc (io-encoding opts)))
          ;; what io/reader opens is a BufferedReader on the JVM, and a Reader
          ;; that is not one already is wrapped in one (make-reader)
          (jio-buffered-reader x
          (cond
            ((char-reader? x)
             (if (and (stream-class x) (not (jch-isa? (stream-class x) "java.io.BufferedReader")))
                 (make-reader-adapter x)
                 x))
            ((reader-adapter? x) x)
            ((and (reader-jhost? x)
                  (not (and (string=? (jhost-tag x) "string-reader") (string-reader-buffered? x))))
             (make-reader-adapter x))
            ((in-stream? x) (make-char-reader (charset-input-port (in-stream-source-port x) enc "reader")))
            ;; a byte[] is a ByteArrayInputStream decoded as UTF-8 on the JVM.
            ;; It used to fall through to the seq arm in io.ss and read as the
            ;; TEXT of its element values, so (line-seq (io/reader (.getBytes
            ;; "a\nb"))) answered ("971098").
            ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte))
             (make-char-reader (charset-input-port (open-bytevector-input-port (na-bytearray->bv x)) enc "reader")))
            ;; a java.io.InputStream the caller wrote: decoded through its own
            ;; read, the way the in-stream arm above decodes jolt's
            ((user-in-stream? x) (make-char-reader (charset-input-port (in-stream-source-port x) enc "reader")))
            ;; a java.io.Reader the caller wrote: io/reader wraps a non-buffered
            ;; Reader in a BufferedReader on the JVM, and that is what the adapter
            ;; is. Without this arm io/reader refused every reify/proxy Reader.
            ((user-reader? x) (make-reader-adapter x))
            ;; Any other charset reads a byte source -- a file, a path, a URL, a
            ;; jar entry, an embedded resource -- through the stream
            ;; io/input-stream opens on it, the JVM's make-reader. The arms below
            ;; and io.ss's are text, read as UTF-8.
            ((and (not (io-utf8? enc))
                  (or (jfile? x) (string? x) (url-jhost? x) (embedded-res? x)))
             (let ((in (jio-input-stream x)))
               (make-char-reader (guard (e (#t (jolt-close in) (raise e)))
                                   (charset-input-port (stream-byte-port in) enc "reader")))))
            ;; A FILE is STREAMED, not slurped.  io.ss's file and path arms answer
            ;; a StringReader over the whole content, which is right for a file
            ;; sitting still and wrong for one that is not: the content had to be
            ;; read before the reader existed, so .readLine and line-seq over a
            ;; FIFO, a tailed log or a device could not return until EOF.  Against
            ;; a pipe held open for 3s, (.readLine (io/reader path)) took 3002 ms
            ;; where the JVM took 6 (jolt-0nk).  The JVM hands back a
            ;; BufferedReader over a FileReader and reads on demand; a char-reader
            ;; over the file's own port is exactly that, and is already what
            ;; (FileReader. path) builds a few hundred lines up.
            ;;
            ;; Only a real on-disk file takes this path.  A jar entry has to be
            ;; inflated out of its archive and an embedded resource is already in
            ;; memory, so both keep io.ss's StringReader arms — there is no port to
            ;; stream from.  slurp is unaffected either way: jolt-slurp answers a
            ;; UTF-8 path through slurp-path and never builds a reader at all.
            ((and (jfile? x) (not (jar-path? (jfile-fs x))))
             (jio-file-char-reader (jfile-fs x) (jfile-path x)))
            ((and (string? x) (not (jar-path? (io-source-path x))))
             (jio-file-char-reader (io-source-path x) (if (file-url-string? x) (file-url->path x) x)))
            ((socket-value? x) (apply jolt-io-reader (socket-stream x "getInputStream") opts))
            (else (prev x))))))))
(define (jio-buffered-reader x r)
  (cond ((eq? r x) r)
        ((and (char-reader? r) (not (stream-class r))) (stream-classed! r "java.io.BufferedReader"))
        ((and (jhost? r) (string=? (jhost-tag r) "string-reader")
              (fx>? (vector-length (jhost-state r)) 3))
         (vector-set! (jhost-state r) 3 #t) r)
        (else r)))
;; A File, a path or a URL is an encoding writer over the stream io/output-stream
;; opens on it, which is the JVM's make-writer, so :append reaches the open and
;; the target is truncated -- or created, or refused -- when the writer is made.
;; It used to be a buffer spat to the path at every flush, which could not
;; append: (io/writer f :append true) had no arity at all, and (io/writer f)
;; then a flush rewrote the whole file each time.
(let ((prev jolt-io-writer))
  (set! jolt-io-writer
        (lambda (x . opts)
          (io-opts-check! opts)
          (let ((enc (io-encoding opts)))
          ;; make-writer: a BufferedWriter is itself, any other Writer is
          ;; wrapped in one, and a byte stream is encoded into by one.
          (cond ((buffered-char-writer? x) x)
                ((char-writer? x) (make-buffered-writer x))
                ;; x is what flush and close reach, as OutputStreamWriter's are:
                ;; with #f the encoded bytes stopped in x's port buffer (#1379)
                ((out-stream? x)
                 (make-charset-writer (out-stream-sink-port x) enc x #f "java.io.BufferedWriter"))
                ((and (jhost? x) (text-sink-tag? (jhost-tag x))) (make-buffered-writer x))
                ;; a java.io.OutputStream the caller wrote is encoded into, the
                ;; way jolt's is
                ((user-out-stream? x)
                 (make-charset-writer (out-stream-sink-port x) enc x #f "java.io.BufferedWriter"))
                ((user-writer? x) (make-buffered-writer x))
                ((or (jfile? x) (string? x) (url-jhost? x))
                 (let ((out (apply jio-output-stream x opts)))
                   (make-charset-writer (out-stream-port out) enc #f #t "java.io.BufferedWriter")))
                ((socket-value? x) (apply jolt-io-writer (socket-stream x "getOutputStream") opts))
                (else (apply prev x opts)))))))
;; re-bound: the clojure.java.io vars hold the VALUE these names had when io.ss
;; ran, so a set! above would not reach them.
(def-var! "clojure.java.io" "writer" jolt-io-writer)

;; io/make-parents: create the parent directories of the last path segment.
(define (jio-make-parents . args)
  (let ((p (apply-make-file-path args)))
    (let loop ((i (- (string-length p) 1)))
      (cond ((<= i 0) #f)
            ((char=? (string-ref p i) #\/) (mkdirs! (substring p 0 i)))
            (else (loop (- i 1)))))))
(define (apply-make-file-path args)
  ;; the JVM's make-parents builds (-> f as-file (apply file more) ...), so it
  ;; carries io/file's as-relative-path contract: an absolute child throws
  ;; here, it is not quietly joined the way the bare constructor would. It also
  ;; inherits the nil: (io/file nil) is nil, and .getParentFile raises on it.
  (let ((f (apply jolt-io-file args)))
    (when (jolt-nil? f)
      (throw-jvm (quote NullPointerException)
                 "Cannot invoke \"java.io.File.getParentFile()\" because the return value of \"clojure.core$apply.invokeStatic(Object, Object, Object)\" is null"))
    (jfile-path f)))
(def-var! "clojure.java.io" "make-parents" jio-make-parents)

;; io/delete-file is (or (.delete (file f)) silently (throw ...)) in
;; clojure.java.io, so a delete answers true and a quiet failure answers the
;; silently value itself. The path resolves against the user's directory like
;; every other io touch; the message names it as given.
(define (jio-delete-file f . opts)
  (cond ((delete-path! (path-of f)) #t)
        ((and (pair? opts) (jolt-truthy? (car opts))) (car opts))
        (else (throw-jvm (quote java.io.IOException)
                         (string-append "Couldn't delete " (file-path-of f))))))
(def-var! "clojure.java.io" "delete-file" jio-delete-file)

;; io/copy: file/path/reader/stream/string/byte[] -> writer/stream/file/path.
;; A byte source copies byte-exact to a byte/file destination (no lossy text
;; round-trip); otherwise the content is read as text. UTF-8 bridges byte<->char.
(define (input-bytes input)   ; bytevector for a byte source, else #f
  (cond ((in-stream? input) (let ((bv (get-bytevector-all (in-stream-port input)))) (if (eof-object? bv) (make-bytevector 0) bv)))
        ((user-in-stream? input) (user-in-stream-bytes input))
        ((bytevector? input) input)
        ((and (jolt-array? input) (eq? (jolt-array-kind input) 'byte)) (na-bytearray->bv input))
        ;; a File source is a BYTE source for every byte destination, not just for
        ;; another file: (io/copy f baos) used to fall through to input-text, slurp
        ;; the file as UTF-8, and replace every non-UTF-8 byte with U+FFFD.
        ((jfile? input) (read-file-bytes (path-of input)))
        ;; a byte-input-stream shim (host tagged-table, :jolt/input-stream — e.g.
        ;; http-client's ByteArrayInputStream): drain it byte-exact, like slurp.
        ((and (htable? input) (jolt-truthy? (jolt-ref-get input (keyword "jolt" "input-stream"))))
         (drain-byte-stream input))
        (else #f)))
(define (input-text input)
  (cond ((string? input) input)
        ((or (char-reader? input) (reader-jhost? input)) (drain-reader input))
        ((jfile? input) (jolt-slurp input))
        ((input-bytes input) => (lambda (bv) (decode-bytevector bv '())))
        (else (jolt-str-render-one input))))
;; A byte STREAM source as a binary input port to read a chunk at a time, with
;; the thunk that releases it, or #f for a source that is not a stream (a byte
;; array, a string, a shim table — those are already in memory). A jolt
;; in-stream is its port; a reify/proxy InputStream reads through its own
;; read(byte[],int,int); a File opens for the copy. This is what keeps a copy
;; in constant memory: input-bytes drains a source whole, and a 100 MB unzip
;; through it held 100 MB (test/zip_memory_test.clj, the copy arms).
(define copy-chunk-bytes 8192)
(define (stream-source-port input)
  (cond ((in-stream? input) (values (in-stream-port input) (lambda () #f)))
        ((user-in-stream? input) (values (in-stream-source-port input) (lambda () #f)))
        ((jfile? input) (let ((p (open-file-input-port (path-of input))))
                          (values p (lambda () (close-port p)))))
        (else (values #f #f))))
;; Read PORT to its end a chunk at a time, handing each bytevector to SINK!;
;; the byte count.
(define (copy-port-chunks! port sink!)
  (let loop ((total 0))
    (let ((bv (get-bytevector-n port copy-chunk-bytes)))
      (if (eof-object? bv)
          total
          (begin (sink! bv) (loop (+ total (bytevector-length bv))))))))
;; INPUT's bytes into SINK! a chunk at a time when it is a stream, else its
;; bytes (or its text's UTF-8) whole; the byte count.
(define (copy-bytes-into! input sink!)
  (let-values (((port release!) (stream-source-port input)))
    (if port
        (dynamic-wind (lambda () #f)
                      (lambda () (copy-port-chunks! port sink!))
                      release!)
        (let ((bv (or (input-bytes input) (string->utf8 (input-text input)))))
          (sink! bv)
          (bytevector-length bv)))))
(define (jio-copy input output . opts)
  (cond
    ((out-stream? output)
     (copy-bytes-into! input (lambda (bv) (put-bytevector (out-stream-port output) bv))))
    ((char-writer? output) (put-string (char-writer-port output) (input-text input)))
    ;; A PrintStream is a java.io.OutputStream, so a byte source reaches it byte
    ;; for byte — (io/copy System/in System/out) is the cat. The other text sinks
    ;; are java.io.Writers and take characters, as they do on the JVM.
    ((print-stream? output)
     (let ((bv (input-bytes input)))
       (record-method-dispatch output "write"
         (list->cseq (list (if bv (na-bv->bytearray bv) (input-text input)))))))
    ((and (jhost? output) (text-sink-tag? (jhost-tag output)))
     (record-method-dispatch output "write" (list->cseq (list (input-text input)))))
    ;; a reify/proxy OutputStream takes bytes through write(byte[],int,int), as
    ;; the JVM's copy loop hands them; a reify/proxy Writer takes the text
    ((user-out-stream? output)
     (copy-bytes-into! input (lambda (bv) (user-out-stream-write! output bv))))
    ((user-writer? output)
     (record-method-dispatch output "write" (list->cseq (list (input-text input)))))
    ((or (jfile? output) (string? output))
     ;; a string INPUT is its characters (io/copy's text source), never a filename
     (let-values (((src release!) (if (string? input) (values #f #f) (stream-source-port input))))
       (cond
         (src
          (dynamic-wind (lambda () #f)
                        (lambda ()
                          (with-port (open-file-output-port (path-of output) (file-options no-fail) (buffer-mode block))
                            (lambda (port) (copy-port-chunks! src (lambda (bv) (put-bytevector port bv))))))
                        release!))
         ((and (not (string? input)) (input-bytes input))
          => (lambda (bv)
               (with-port (open-file-output-port (path-of output) (file-options no-fail) (buffer-mode block))
                 (lambda (port) (put-bytevector port bv)))))
         (else (jolt-spit output (input-text input))))))
    ;; a byte-output-stream shim (a host tagged-table with :jolt/output-stream,
    ;; e.g. http-client's ByteArrayOutputStream): write through its .write method,
    ;; byte-exact for a byte source.
    ((and (htable? output) (jolt-truthy? (jolt-ref-get output (keyword "jolt" "output-stream"))))
     (let ((bv (input-bytes input)))
       (record-method-dispatch output "write"
         (list->cseq (list (if bv (na-bv->bytearray bv) (input-text input)))))))
    (else (throw-jvm (quote IllegalArgumentException) "io/copy: unsupported output type")))
  jolt-nil)
(def-var! "clojure.java.io" "copy" jio-copy)

;; --- instance? for the java.io stream taxonomy ------------------------------
(register-class-arm! in-stream?
  (lambda (x) (cond ((stream-class x))
                    ((in-stream-pushback x) "java.io.PushbackInputStream")
                    ((in-stream-sequence-close x) "java.io.SequenceInputStream")
                    (else "java.io.InputStream"))))
(register-class-arm! out-stream? (lambda (x) (or (stream-class x) "java.io.OutputStream")))
(register-class-arm! char-reader? (lambda (x) (or (stream-class x) "java.io.Reader")))
(register-class-arm! reader-adapter? (lambda (x) "java.io.BufferedReader"))
(register-class-arm! char-writer? (lambda (x) (or (char-writer-class x) "java.io.Writer")))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (not (symbol-t? type-sym)) 'pass
    (let ((short (last-dot (symbol-t-name type-sym))))
      (cond
        ;; a PushbackInputStream is the one in-stream that IS one; every other
        ;; in-stream answers false to it, as a ByteArrayInputStream does
        ((and (string=? short "PushbackInputStream") (in-stream? val)) (and (in-stream-pushback val) #t))
        ((and (string=? short "SequenceInputStream") (in-stream? val)) (and (in-stream-sequence-close val) #t))
        ;; a classed stream or reader answers from its class, as the JVM does: a
        ;; ByteArrayInputStream is not a FileInputStream
        ((and (or (in-stream? val) (out-stream? val) (char-reader? val)) (stream-class val))
         => (lambda (cls)
              (if (or (member short '("Closeable" "AutoCloseable"))
                      (jch-isa? cls (symbol-t-name type-sym)))
                  #t
                  'pass)))
        ((and (in-stream? val) (member short '("InputStream" "FileInputStream" "ByteArrayInputStream"
                                               "BufferedInputStream" "FilterInputStream" "Closeable" "AutoCloseable"))) #t)
        ((and (out-stream? val) (member short '("OutputStream" "FileOutputStream" "ByteArrayOutputStream"
                                                "BufferedOutputStream" "FilterOutputStream" "Closeable" "AutoCloseable" "Flushable"))) #t)
        ((and (char-reader? val) (member short '("Reader" "BufferedReader" "FileReader" "InputStreamReader"
                                                 "Closeable" "AutoCloseable" "Readable"))) #t)
        ((and (reader-adapter? val) (member short '("Reader" "BufferedReader"
                                                    "Closeable" "AutoCloseable" "Readable"))) #t)
        ((and (char-writer? val) (char-writer-class val))
         (if (or (member short '("Writer" "Closeable" "AutoCloseable" "Flushable" "Appendable"))
                 (jch-isa? (char-writer-class val) (symbol-t-name type-sym)))
             #t
             'pass))
        ((and (char-writer? val) (member short '("Writer" "BufferedWriter" "FileWriter" "OutputStreamWriter"
                                                 "Closeable" "AutoCloseable" "Flushable" "Appendable"))) #t)
        (else 'pass))))))

;; --- pr / pr-str honor a user (defmethod print-method T …) -------------------
;; On the JVM print-method IS the printer, so installing one changes what pr
;; emits for that type. Here the printer is a list of native arms, and this hook
;; (printing.ss consults it before that list) restores the JVM's precedence: a
;; record renders through its method instead of the default #ns.Name{…} form, and
;; a host class renders through its method instead of the arm the library that
;; provided the class registered — for instance ring's session store, which
;; round-trips a java.time.Instant through a custom print-method and edn reader.
;;
;; Two dispatch values are tried: the type tag the multimethod dispatches on
;; itself (a record's class name, or the :jolt/… tag of a built-in), then the
;; value's class, since a method written for a host type names the class —
;; (defmethod print-method java.time.Instant …) — and jolt's tag for one of those
;; is the catch-all :object.
;;
;; Only a DIRECT method counts — the multimethod's :default falls back to
;; __pr-str1, which returns here, so a full resolve would recurse forever.
;;
;; jolt's own print-method entries are all keyed by type tag, so resolving a
;; class per printed value would be dead weight until a library installs a
;; class-keyed method. Whether any exists is settled once per multimethod epoch.
(define pm-class-keys-epoch -1)
(define pm-has-class-keys? #f)
(define (pm-class-keys? tbl)
  (unless (fx= pm-class-keys-epoch jolt-mm-epoch)
    (set! pm-class-keys-epoch jolt-mm-epoch)
    (set! pm-has-class-keys?
          (let loop ((ks (vector->list (hashtable-keys tbl))))
            (and (pair? ks) (or (jclass? (car ks)) (loop (cdr ks)))))))
  pm-has-class-keys?)

;; Resolved once: var-deref rebuilds "clojure.core/print-method" and hashes it on
;; every call, which this pays per printed value. The cell is stable — def-var!
;; mutates the root in place — so caching it is sound under redefinition.
(define pm-cell #f)
(define (user-print-method x)
  (unless pm-cell (set! pm-cell (jolt-var "clojure.core" "print-method")))
  (let ((mf (var-cell-deref pm-cell)))
    (and (jolt-multifn? mf)
         (let ((tbl (jolt-multifn-methods mf)))
           (or (hashtable-ref tbl (jolt-type x) #f)
               (and (pm-class-keys? tbl)
                    (let ((c (jolt-class-name x)))
                      (and (string? c)
                           (hashtable-ref tbl (jolt-class-for c) #f)))))))))
(set-pr-user-method-render!
  (lambda (x)
    (let ((m (user-print-method x)))
      (and m
           (let ((port (open-output-string)))
             (jolt-invoke m x (make-char-writer port))
             (get-output-string port))))))

;; --- piped streams ----------------------------------------------------------
;; A PipedOutputStream and its PipedInputStream share one buffer: the reader
;; blocks until the writer produces or closes. jolt runs futures on real OS
;; threads, which is what makes a pipe useful — ring.util.io/piped-input-stream
;; streams a response body out of one.
;;
;; Each end is an ordinary in-stream/out-stream jhost over a CUSTOM Chez port, so
;; slurp, with-open, io/copy and instance? treat it like any other stream. The
;; extra state slot holds a box carrying the shared pipe: connect makes both ends
;; point at one, and close marks it rather than closing the port.
(define-record-type jpipe
  ;; The chunk queue is a front/rear list pair — the standard amortized-O(1)
  ;; two-list queue. front holds bytevectors oldest-first (the dequeue end, with
  ;; `pos` bytes of its head already handed out); rear holds newly written chunks
  ;; newest-first and reverses onto front when front drains. A write is one cons,
  ;; where a single `chunks` list re-copied per write cost O(N^2) over N writes —
  ;; under the pipe mutex (`make pipescaling` gates the shape). nbytes counts
  ;; what is queued and unread, so available is O(1).
  (fields mu cv (mutable front) (mutable rear) (mutable pos) (mutable nbytes)
          (mutable wclosed?) (mutable rclosed?))
  (nongenerative jpipe-v2))
(define (new-jpipe) (make-jpipe (make-mutex) (jolt-make-cv) '() '() 0 0 #f #f))

;; the queue's dequeue end, rebalancing rear->front when front has drained.
(define (jpipe-front! p)
  (let ((f (jpipe-front p)))
    (if (pair? f)
        f
        (let ((f (reverse (jpipe-rear p))))
          (jpipe-front-set! p f)
          (jpipe-rear-set! p '())
          f))))

(define (io-throw msg) (jolt-throw (jolt-host-throwable "java.io.IOException" msg)))

(define (pipe-write! p bv start count)
  (jolt-with-mutex (jpipe-mu p)
    (when (jpipe-rclosed? p) (io-throw "Read end dead"))
    (when (jpipe-wclosed? p) (io-throw "Pipe closed"))
    (let ((chunk (make-bytevector count)))
      (bytevector-copy! bv start chunk 0 count)
      (jpipe-rear-set! p (cons chunk (jpipe-rear p)))
      (jpipe-nbytes-set! p (+ (jpipe-nbytes p) count))
      (jolt-cv-wake! (jpipe-cv p))))
  count)

;; Waits while the pipe is empty and the writer is still open. A THREAD blocks,
;; which releases the mutex so a writer can run; a FIBER parks, because blocking
;; its carrier would stop every other fiber on it until the writer wrote — and if
;; the writer is a fiber on that same carrier, forever (jolt-x1no).
(define (pipe-read! p bv start count)
  (jolt-cv-wait (jpipe-mu p) (jpipe-cv p) #f
    (lambda (_timed-out?)
      (cond
        ((jpipe-rclosed? p) (io-throw "Pipe closed"))
        ((pair? (jpipe-front! p))
         (let* ((head (car (jpipe-front p)))
                (avail (- (bytevector-length head) (jpipe-pos p)))
                (n (min avail count)))
           (bytevector-copy! head (jpipe-pos p) bv start n)
           (if (= n avail)
               (begin (jpipe-front-set! p (cdr (jpipe-front p))) (jpipe-pos-set! p 0))
               (jpipe-pos-set! p (+ (jpipe-pos p) n)))
           (jpipe-nbytes-set! p (- (jpipe-nbytes p) n))
           n))
        ((jpipe-wclosed? p) 0)                      ; writer done: end of stream
        (else jolt-cv-again)))))

;; PipedInputStream.available: what the writer has queued and the reader has not
;; taken yet. The queue IS the buffer here, so this is exact rather than an
;; estimate — the running byte count writes maintain and reads drain.
(define (pipe-available p)
  (jolt-with-mutex (jpipe-mu p)
    (when (jpipe-rclosed? p) (io-throw "Pipe closed"))
    (jpipe-nbytes p)))

(define (pipe-close-write! p)
  (jolt-with-mutex (jpipe-mu p)
    (jpipe-wclosed?-set! p #t)
    (jolt-cv-wake! (jpipe-cv p))))
(define (pipe-close-read! p)
  (jolt-with-mutex (jpipe-mu p)
    (jpipe-rclosed?-set! p #t)
    (jolt-cv-wake! (jpipe-cv p))))

;; The shared pipe behind a stream, or #f when it is an ordinary file/array stream.
(define (piped-cell x)
  (let ((st (and (jhost? x) (jhost-state x))))
    (and (vector? st) (fx>? (vector-length st) 3) (vector-ref st 3))))
(define (piped-pipe x) (let ((c (piped-cell x))) (and c (unbox c))))

;; connect joins the two ends onto ONE pipe. Either end may be the receiver, and
;; either may already have been connected, so both boxes are pointed at the same
;; buffer rather than one adopting the other's.
(define (pipe-connect! a b)
  (let ((ca (piped-cell a)) (cb (piped-cell b)))
    (unless (and ca cb) (io-throw "Not a piped stream"))
    (let ((shared (unbox ca)))
      (set-box! cb shared))))

(define (make-piped-in-stream . rest)
  (let* ((cell (box (new-jpipe)))
         (port (make-custom-binary-input-port
                "piped-input"
                (lambda (bv start count) (pipe-read! (unbox cell) bv start count))
                #f #f (lambda () #f)))
         (self (stream-classed! (make-jhost "in-stream" (vector port #f 0 cell)) "java.io.PipedInputStream")))
    (when (pair? rest) (pipe-connect! self (car rest)))
    self))

(define (make-piped-out-stream . rest)
  (let* ((cell (box (new-jpipe)))
         (port (make-custom-binary-output-port
                "piped-output"
                (lambda (bv start count) (pipe-write! (unbox cell) bv start count))
                #f #f (lambda () #f)))
         (self (stream-classed! (make-jhost "out-stream" (vector port #f #f cell)) "java.io.PipedOutputStream")))
    (when (pair? rest) (pipe-connect! self (car rest)))
    self))

(register-class-ctor! "PipedInputStream" make-piped-in-stream)
(register-class-ctor! "java.io.PipedInputStream" make-piped-in-stream)
(register-class-ctor! "PipedOutputStream" make-piped-out-stream)
(register-class-ctor! "java.io.PipedOutputStream" make-piped-out-stream)
