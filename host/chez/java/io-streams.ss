;; java.io byte/char streams over Chez ports. Each stream is a jhost wrapping a
;; Chez port, so buffering, EOF and binary<->char transcoding come from Chez
;; rather than a hand-rolled buffer.
;;
;;   in-stream    #(binary-input-port)            FileInputStream / ByteArrayInputStream
;;   out-stream   #(binary-output-port extract acc) FileOutputStream / ByteArrayOutputStream
;;   char-reader  #(textual-input-port)            FileReader / InputStreamReader
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
      ;; a piped stream holds its bytes in two places: what the writer has queued
      ;; and what an earlier read already pulled into the port's buffer
      (p (+ (port-buffered port) (pipe-available p)))
      ((port-remaining port) => values)
      ;; the kernel's count plus whatever was already pulled out of it into the
      ;; port's buffer — the bytes live in two places, as they do for a pipe
      ((port-fionread port) => (lambda (n) (+ (port-buffered port) n)))
      ((fx>? (port-buffered port) 0) (port-buffered port))
      ((guard (_ (#t #f)) (input-port-ready? port))
       (lookahead-u8 port)
       (port-buffered port))
      (else 0))))

(register-host-methods! "in-stream"
  (list
   (cons "read"
         (lambda (self . rest)
           (let ((port (in-stream-live-port self)))
             (if (null? rest)
                 ;; InputStream.read() returns the byte as an UNSIGNED int 0..255
                 ;; (-1 at EOF) — the one place a byte is not signed, because the
                 ;; return has to distinguish 0xff from end-of-stream.
                 (let ((b (get-u8 port))) (if (eof-object? b) -1 (->num b)))
                 ;; read(buf …) fills a byte-array, whose elements ARE signed.
                 ;; Returns as soon as ANY byte is there, which is the JVM's
                 ;; contract ("blocks until at least one byte is available") and
                 ;; not "until the buffer is full": filling it first hung a read
                 ;; over a pipe or a terminal, where the rest of the buffer only
                 ;; arrives after whatever the program does with these bytes.
                 (let* ((buf (car rest))
                        (vec (jolt-array-vec buf))
                        (off (if (>= (length rest) 3) (jnum->exact (cadr rest)) 0))
                        (len (if (>= (length rest) 3) (jnum->exact (caddr rest)) (vector-length vec)))
                        (tmp (make-bytevector (max len 1)))
                        (n (if (<= len 0) 0 (get-bytevector-some! port tmp 0 len))))
                   (cond
                     ((<= len 0) (->num 0))
                     ((eof-object? n) -1)
                     (else (let loop ((i 0))
                             (if (>= i n) (->num n)
                                 (begin (vector-set! vec (+ off i) (na-u8->byte (bytevector-u8-ref tmp i)))
                                        (loop (+ i 1))))))))))))
   (cons "readAllBytes" (lambda (self) (let ((bv (get-bytevector-all (in-stream-live-port self))))
                                         (na-byte-array (if (eof-object? bv) (make-bytevector 0) bv)))))
   (cons "skip" (lambda (self n) (let ((bv (get-bytevector-n (in-stream-live-port self) (jnum->exact n))))
                                   (->num (if (eof-object? bv) 0 (bytevector-length bv))))))
   (cons "available" (lambda (self) (->num (in-stream-available self))))
   ;; A piped stream marks its shared pipe instead of closing the Chez port: a
   ;; read after close has to raise the JVM's IOException, and a closed Chez port
   ;; raises a classless host error before the port's own reader is consulted.
   (cons "close" (lambda (self)
                   (let ((p (piped-pipe self)))
                     (if p (pipe-close-read! p) (close-port (in-stream-port self))))
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
               (let* ((vec (jolt-array-vec (car args)))
                      (off (jnum->exact (cadr args)))
                      (len (jnum->exact (caddr args))))
                 (when (or (< off 0) (< len 0) (> (+ off len) (vector-length vec)))
                   (throw-jvm (quote IndexOutOfBoundsException) "readNBytes range"))
                 (if (<= len 0) (->num 0)
                     (let ((bv (get-bytevector-n (in-stream-live-port self) len)))
                       (if (eof-object? bv) (->num 0)
                           (let ((n (bytevector-length bv)))
                             (let loop ((i 0))
                               (if (>= i n) (->num n)
                                   (begin (vector-set! vec (+ off i) (na-u8->byte (bytevector-u8-ref bv i)))
                                          (loop (+ i 1)))))))))))))
   ;; transferTo drains this stream into an OutputStream and answers the count.
   (cons "transferTo"
         (lambda (self out)
           (let ((bv (get-bytevector-all (in-stream-live-port self))))
             (if (eof-object? bv) (->num 0)
                 (let ((n (bytevector-length bv)))
                   (if (out-stream? out)
                       (put-bytevector (out-stream-port out) bv)
                       ;; any other shape of output stream (a library's shim):
                       ;; go through its own write(byte[]), the way the JVM does
                       (record-method-dispatch out "write" (jolt-list (na-byte-array bv))))
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
   (cons "toString" (lambda (self) "#<InputStream>"))))

;; --- byte output stream -----------------------------------------------------
;; state #(port extract acc): extract/acc are #f for a file/passthrough stream;
;; a ByteArrayOutputStream carries the R6RS extraction proc + an accumulator
;; bytevector (Chez's extract resets the port, so snapshot on demand, not per write).
(define (out-stream-port self) (vector-ref (jhost-state self) 0))
(define (out-stream? x) (and (jhost? x) (string=? (jhost-tag x) "out-stream")))
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
           (let ((port (out-stream-port self)))
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
   (cons "flush" (lambda (self) (flush-output-port (out-stream-port self)) jolt-nil))
   (cons "close" (lambda (self) (flush-output-port (out-stream-port self))
                   ;; a ByteArrayOutputStream's close is a no-op (toByteArray stays valid);
                   ;; a piped stream signals end-of-stream to its reader; a file
                   ;; stream's port is closed.
                   (let ((p (piped-pipe self)))
                     (cond (p (pipe-close-write! p))
                           ((vector-ref (jhost-state self) 1))
                           (else (close-port (out-stream-port self)))))
                   jolt-nil))
   (cons "connect" (lambda (self other) (pipe-connect! self other) jolt-nil))
   (cons "toByteArray" (lambda (self) (na-byte-array (bytevector-copy (baos-bytes self)))))
   (cons "size" (lambda (self) (->num (bytevector-length (baos-bytes self)))))
   (cons "reset" (lambda (self) (baos-bytes self) (vector-set! (jhost-state self) 2 (make-bytevector 0)) jolt-nil))
   (cons "toString" (lambda (self . cs) (decode-bytevector (baos-bytes self)
                                          (if (pair? cs) (list (jolt-str-render-one (car cs))) '()))))))

;; --- char input (Reader) ----------------------------------------------------
(define (char-reader-port self) (vector-ref (jhost-state self) 0))
(define (char-reader? x) (and (jhost? x) (string=? (jhost-tag x) "char-reader")))
(define (make-char-reader port) (make-jhost "char-reader" (vector port)))
(register-host-methods! "char-reader"
  (list
   (cons "read"
         (lambda (self . rest)
           (let ((port (char-reader-port self)))
             (if (null? rest)
                 (let ((c (get-char port))) (if (eof-object? c) -1 (->num (char->integer c))))
                 (let* ((buf (car rest))
                        (vec (jolt-array-vec buf))
                        (off (if (>= (length rest) 3) (jnum->exact (cadr rest)) 0))
                        (len (if (>= (length rest) 3) (jnum->exact (caddr rest)) (vector-length vec))))
                   (let loop ((i 0))
                     (if (>= i len) (->num i)
                         (let ((c (get-char port)))
                           (if (eof-object? c)
                               (if (= i 0) -1 (->num i))
                               (begin (vector-set! vec (+ off i) c) (loop (+ i 1))))))))))))
   (cons "readLine" (lambda (self) (let ((l (get-line (char-reader-port self)))) (if (eof-object? l) jolt-nil l))))
   (cons "lines" (lambda (self)
                   (let loop ((acc '()))
                     (let ((l (get-line (char-reader-port self))))
                       (if (eof-object? l) (list->cseq (reverse acc)) (loop (cons l acc)))))))
   (cons "ready" (lambda (self) #t))
   (cons "skip" (lambda (self n) (let loop ((i 0) (k (jnum->exact n)))
                                   (if (or (>= i k) (eof-object? (get-char (char-reader-port self)))) (->num i)
                                       (loop (+ i 1) k)))))
   (cons "close" (lambda (self) (close-port (char-reader-port self)) jolt-nil))
   (cons "mark" (lambda (self . _) jolt-nil))
   (cons "reset" (lambda (self) (guard (e (#t jolt-nil)) (set-port-position! (char-reader-port self) 0) jolt-nil)))
   (cons "toString" (lambda (self) "#<Reader>"))))

;; --- char output (Writer) ---------------------------------------------------
(define (char-writer-port self) (vector-ref (jhost-state self) 0))
(define (char-writer? x) (and (jhost? x) (string=? (jhost-tag x) "char-writer")))
;; Every jhost on the CHARACTER side of the io split, the mirror of reader-jhost?:
;; char-writer here, and StringWriter / *out* / io/writer-of-a-file over in io.ss
;; and host-static-classes.ss. A print-stream is deliberately absent -- it is a
;; java.io.OutputStream, so it is a legitimate thing to wrap in a writer.
(define (writer-jhost? x)
  (and (jhost? x)
       (member (jhost-tag x) '("char-writer" "writer" "port-writer" "file-writer"))
       #t))
;; state #(port downstream): downstream is the byte stream this writer wraps, when
;; it wraps one. flush/close have to reach it — java.io.OutputStreamWriter.flush
;; flushes its own encoder AND the stream underneath, which is what makes a
;; PrintStream over a logging proxy emit on (println …), since println flushes.
(define (make-char-writer port . down)
  (make-jhost "char-writer" (vector port (and (pair? down) (car down)))))
(define (char-writer-downstream self)
  (let ((st (jhost-state self))) (and (> (vector-length st) 1) (vector-ref st 1))))
(define (cw-text x) (if (number? x) (string (integer->char (jnum->exact x))) (jolt-str-render-one x)))
(register-host-methods! "char-writer"
  (list
   (cons "write" (lambda (self x . rest)
                   ;; (write str) | (write int) | (write str off len)
                   (let ((s (cw-text x)))
                     (put-string (char-writer-port self)
                                 (if (>= (length rest) 2) (substring s (jnum->exact (car rest))
                                                                     (+ (jnum->exact (car rest)) (jnum->exact (cadr rest)))) s)))
                   jolt-nil))
   (cons "append" (lambda (self x . rest) (put-string (char-writer-port self) (cw-text x)) self))
   (cons "newLine" (lambda (self) (put-char (char-writer-port self) #\newline) jolt-nil))
   (cons "flush" (lambda (self)
                   (flush-output-port (char-writer-port self))
                   (let ((d (char-writer-downstream self)))
                     (when d (record-method-dispatch d "flush" jolt-nil)))
                   jolt-nil))
   (cons "close" (lambda (self)
                   (flush-output-port (char-writer-port self))
                   (let ((d (char-writer-downstream self)))
                     (when d (record-method-dispatch d "close" jolt-nil)))
                   (close-port (char-writer-port self))
                   jolt-nil))
   (cons "toString" (lambda (self) "#<Writer>"))))

;; --- constructors -----------------------------------------------------------
(define utf8-tx (make-transcoder (utf-8-codec)))
(define (path-of x) (project-relative (file-path-of x)))
(define (src-bytevector x)   ; a byte[] or Chez bytevector -> bytevector
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        (else (throw-jvm (quote ClassCastException) "expected a byte array"))))

(define (reg-ctor! names ctor) (for-each (lambda (n) (register-class-ctor! n ctor)) names))

;; The JVM's file constructors raise java.io.FileNotFoundException when the path
;; cannot be opened, and libraries branch on that class the same way they do on
;; slurp's (io.ss slurp-path says why). A raw Chez i/o condition is not catchable
;; as that class, so the caller's fallback never runs. The message is the JVM's
;; shape -- the path AS GIVEN, then the reason in parens -- and the JVM uses this
;; one class for all three reasons, so distinguish only the parenthetical.
(define (file-open-error given resolved)
  (throw-jvm (quote java.io.FileNotFoundException)
             (string-append given " ("
                            (cond ((not (file-exists? resolved)) "No such file or directory")
                                  ((file-directory? resolved)    "Is a directory")
                                  (else                          "Permission denied"))
                            ")")))
;; Opening is guarded rather than pre-checked -- existence and permission can
;; change between a check and the open, and only the open itself is
;; authoritative. A DIRECTORY is the one case that needs the check: the JVM
;; refuses it at construction, while a Chez binary input port over a directory
;; opens fine and raises on the first read, far from the call that was wrong.
(define (open-file-guarded src thunk)
  (let ((resolved (path-of src)))
    (when (file-directory? resolved) (file-open-error (file-path-of src) resolved))
    (guard (e ((i/o-error? e) (file-open-error (file-path-of src) resolved)))
      (thunk resolved))))

(reg-ctor! '("FileInputStream" "java.io.FileInputStream")
  (lambda (src . _)
    (open-file-guarded src
      (lambda (p) (make-in-stream (open-file-input-port p (file-options) (buffer-mode block)))))))
(reg-ctor! '("FileOutputStream" "java.io.FileOutputStream")
  (lambda (src . rest)
    (let ((append? (and (pair? rest) (jolt-truthy? (car rest)))))
      (open-file-guarded src
        (lambda (p)
          (make-out-stream (open-file-output-port p
                             (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                             (buffer-mode block))))))))
(reg-ctor! '("ByteArrayInputStream" "java.io.ByteArrayInputStream")
  (lambda (bytes . rest)
    (let ((bv (src-bytevector bytes)))
      (make-in-stream-markable (open-bytevector-input-port
                       (if (>= (length rest) 2)
                           (let ((off (jnum->exact (car rest))) (len (jnum->exact (cadr rest))))
                             (let ((sub (make-bytevector len))) (bytevector-copy! bv off sub 0 len) sub))
                           bv))))))
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
        (record-method-dispatch t "write" (list->cseq (list (na-byte-array (string->utf8 str))))))
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
   (cons "printf" (lambda (self fmt . args)
                    (ps-emit self (apply jolt-format (jolt-str-render-one fmt) args))))
   (cons "format" (lambda (self fmt . args)
                    (ps-emit self (apply jolt-format (jolt-str-render-one fmt) args)) self))
   (cons "append" (lambda (self x . rest) (ps-emit self (append-text x rest)) self))
   (cons "write" (lambda (self x . rest)
                   (let ((t (ps-target self)))
                     (record-method-dispatch t "write" (list->cseq (cons x rest))))
                   ;; a written newline autoflushes too, as on the JVM
                   (when (and (ps-autoflush? self) (number? x)
                              (= (jnum->exact x) 10))
                     (ps-flush! self))
                   jolt-nil))
   (cons "flush" (lambda (self) (ps-flush! self)))
   (cons "close" (lambda (self) (ps-flush! self)
                   (record-method-dispatch (ps-target self) "close" jolt-nil) jolt-nil))
   (cons "checkError" (lambda (self) #f))
   (cons "toString" (lambda (self) "#<PrintStream>"))))
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
(define system-in-stream (make-jhost "in-stream" (vector 'stdin #f 0)))
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
(define (stdin-line-string buf i)
  (let ((out (make-bytevector i)))
    (bytevector-copy! buf 0 out 0 i)
    (utf8->string out)))

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
      (lambda (port extract) (make-jhost "out-stream" (vector port extract (make-bytevector 0)))))))
(reg-ctor! '("FileReader" "java.io.FileReader")
  (lambda (src . _)
    (open-file-guarded src
      (lambda (p) (make-char-reader (transcoded-port (open-file-input-port p (file-options) (buffer-mode block)) utf8-tx))))))
(reg-ctor! '("FileWriter" "java.io.FileWriter")
  (lambda (src . rest)
    (let ((append? (and (pair? rest) (jolt-truthy? (car rest)))))
      (open-file-guarded src
        (lambda (p)
          (make-char-writer (transcoded-port (open-file-output-port p
                              (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                              (buffer-mode block)) utf8-tx)))))))
;; InputStreamReader / OutputStreamWriter decode / encode the wrapped byte stream
;; (UTF-8 default; an explicit charset is honored only as UTF-8 here).
;;
;; A byte port that pulls each block through the stream's OWN read method,
;; whatever kind of stream it is. Dispatching rather than transcoding the
;; stream's port directly is what lets a proxy's override see the read, and it
;; leaves the wrapped stream OPEN — R6RS transcoded-port takes ownership of the
;; port it is given, so (io/reader System/in) used to take standard input away
;; from System/in, and from read-line with it, the moment it was called.
(define (in-stream-source-port in)
  (make-custom-binary-input-port
   "stream-source"
   (lambda (bv start count)
     (let* ((arr (na-byte-array (make-bytevector count)))
            (n (jnum->exact (record-method-dispatch in "read"
                              (list->cseq (list arr (->num 0) (->num count)))))))
       (if (<= n 0)
           0                                   ; the custom-port way of saying EOF
           (begin (bytevector-copy! (na-bytearray->bv arr) 0 bv start n) n))))
   #f #f (lambda () #f)))
(reg-ctor! '("InputStreamReader" "java.io.InputStreamReader")
  (lambda (in . _)
    ;; A Reader is already decoded characters, so there is nothing for an
    ;; InputStreamReader to decode and the JVM has no such constructor. Say so
    ;; here: the byte port below drives the wrapped stream's read(byte[],int,int),
    ;; which a Reader answers by writing CHARACTERS into the array, and the
    ;; failure then surfaces from whatever finally reads a char as a number --
    ;; far from the call that was actually wrong.
    (when (reader-jhost? in)
      (throw-jvm (quote IllegalArgumentException)
                 "InputStreamReader wraps an InputStream, not a Reader (it is already decoded)"))
    (make-char-reader (transcoded-port (in-stream-source-port in) utf8-tx))))
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
       (record-method-dispatch out "write" (list->cseq (list (na-byte-array chunk)))))
     count)
   #f #f (lambda () #f)))
(reg-ctor! '("OutputStreamWriter" "java.io.OutputStreamWriter")
  (lambda (out . _)
    ;; The mirror of InputStreamReader above: a Writer takes characters, so there
    ;; is nothing to encode and the JVM has no such constructor.
    (when (writer-jhost? out)
      (throw-jvm (quote IllegalArgumentException)
                 "OutputStreamWriter wraps an OutputStream, not a Writer (it already takes characters)"))
    (make-char-writer (transcoded-port (out-stream-sink-port out) utf8-tx) out)))
;; Buffered* — Chez ports are buffered already; the wrapper is the wrapped stream.
(for-each (lambda (n) (register-class-ctor! n (lambda (inner . _) inner)))
          '("BufferedReader" "java.io.BufferedReader"
            "BufferedWriter" "java.io.BufferedWriter"
            "BufferedOutputStream" "java.io.BufferedOutputStream"))
;; BufferedInputStream is the JVM's mark/reset provider: wrapping ANY stream
;; makes markSupported true there. jolt honours that when the underlying port
;; can actually seek (file, bytevector) — a fresh markable jhost over the SAME
;; port, so close propagates both ways like the JVM's. Over a port that cannot
;; seek (socket, pipe, stdin) the JVM would replay from its buffer; jolt has no
;; replay layer, so the wrapper stays the wrapped stream and markSupported
;; stays false — the honest answer, and a known divergence (reset throws
;; instead of silently rewinding wrong). See known-divergences.edn.
(for-each (lambda (n)
            (register-class-ctor! n
              (lambda (inner . _)
                (if (and (in-stream? inner)
                         (let ((p (vector-ref (jhost-state inner) 0)))
                           (and (port? p)
                                (port-has-port-position? p)
                                (port-has-set-port-position!? p))))
                    (make-in-stream-markable (in-stream-port inner))
                    inner))))
          '("BufferedInputStream" "java.io.BufferedInputStream"))

;; --- integration: slurp / line-seq / with-open ------------------------------
;; a char-reader joins the reader-jhost set (drain-reader / line-seq read it via
;; its .read method).
(let ((prev reader-jhost?))
  (set! reader-jhost? (lambda (x) (or (char-reader? x) (prev x)))))

;; slurp a char-reader (drain chars) or a byte in-stream (drain bytes -> decode).
(let ((prev jolt-slurp))
  (set! jolt-slurp
        (lambda (src . opts)
          (cond
            ((char-reader? src) (drain-reader src))
            ((in-stream? src) (decode-bytevector (let ((bv (get-bytevector-all (in-stream-port src))))
                                                   (if (eof-object? bv) (make-bytevector 0) bv))
                                                 (slurp-encoding opts)))
            (else (apply prev src opts)))))
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
            (else (apply prev target content opts)))))
  (def-var! "clojure.core" "spit" jolt-spit))

;; with-open closes the new stream jhosts via their .close method.
(let ((prev jolt-close))
  (set! jolt-close
        (lambda (x)
          (if (and (jhost? x) (member (jhost-tag x) '("in-stream" "out-stream" "char-reader" "char-writer")))
              (begin (record-method-dispatch x "close" jolt-nil) jolt-nil)
              (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; --- clojure.java.io: byte streams + copy / make-parents / delete-file -------
;; input-stream/output-stream now yield real byte streams (were char reader/writer).
;; the file branches announce themselves to the AOT cache (io-note-file-read!,
;; io.ss): opening a resource for reading at compile time is a read like a slurp.
(define (jio-open-in-file p)
  (io-note-file-read! p)
  (make-in-stream (open-file-input-port p (file-options) (buffer-mode block))))
(define (jio-input-stream x)
  (cond ((in-stream? x) x)
        ((jfile? x) (jio-open-in-file (jfile-fs x)))
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (make-in-stream (open-bytevector-input-port (na-bytearray->bv x))))
        ((bytevector? x) (make-in-stream (open-bytevector-input-port x)))
        ((and (jhost? x) (string=? (jhost-tag x) "url")) (jio-open-in-file (url-strip-scheme (url-spec x))))
        ((string? x) (jio-open-in-file (project-relative x)))
        (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str x) "> as an InputStream.")))))
(define (jio-output-stream x . rest)
  (cond ((out-stream? x) x)
        ((or (jfile? x) (string? x))
         (let ((append? (let loop ((o rest)) (cond ((or (null? o) (null? (cdr o))) #f)
                                                    ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "append") (jolt-truthy? (cadr o))) #t)
                                                    (else (loop (cddr o)))))))
           (make-out-stream (open-file-output-port (path-of x)
                              (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                              (buffer-mode block)))))
        ;; System/out and System/err are already byte streams — pass them through,
        ;; the way an out-stream passes through.
        ((and (jhost? x) (text-sink-tag? (jhost-tag x))) x)
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
        (lambda (x)
          (if (in-stream? x)
              (make-char-reader (transcoded-port (in-stream-source-port x) utf8-tx))
              (prev x)))))
(let ((prev jolt-io-writer))
  (set! jolt-io-writer
        (lambda (x)
          (cond ((char-writer? x) x)
                ((out-stream? x) (make-char-writer (transcoded-port (out-stream-sink-port x) utf8-tx)))
                ((and (jhost? x) (text-sink-tag? (jhost-tag x))) x)
                (else (prev x))))))
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
  ;; the JVM's make-parents is (.mkdirs (as-file (apply file f more))) — io/file
  ;; semantics, not the bare ctor, so an absolute child throws here too
  (jfile-path (apply jolt-io-file args)))
(def-var! "clojure.java.io" "make-parents" jio-make-parents)

;; io/delete-file: delete the file; raise unless :silently truthy.
(define (jio-delete-file f . opts)
  (let ((p (file-path-of f)))
    (if (delete-path! p) jolt-nil
        (if (and (pair? opts) (jolt-truthy? (car opts))) jolt-nil
            (throw-jvm (quote java.io.IOException) (string-append "Couldn't delete " p))))))
(def-var! "clojure.java.io" "delete-file" jio-delete-file)

;; io/copy: file/path/reader/stream/string/byte[] -> writer/stream/file/path.
;; A byte source copies byte-exact to a byte/file destination (no lossy text
;; round-trip); otherwise the content is read as text. UTF-8 bridges byte<->char.
(define (input-bytes input)   ; bytevector for a byte source, else #f
  (cond ((in-stream? input) (let ((bv (get-bytevector-all (in-stream-port input)))) (if (eof-object? bv) (make-bytevector 0) bv)))
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
(define (jio-copy input output . opts)
  (cond
    ((out-stream? output)
     (put-bytevector (out-stream-port output)
                     (or (input-bytes input) (string->utf8 (input-text input)))))
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
    ((or (jfile? output) (string? output))
     ;; a string INPUT is its characters (io/copy's text source), never a filename
     (let ((bv (and (not (string? input)) (input-bytes input))))
       (if bv
           (with-port (open-file-output-port (path-of output) (file-options no-fail) (buffer-mode block))
             (lambda (port) (put-bytevector port bv)))
           (jolt-spit output (input-text input)))))
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
(register-class-arm! in-stream? (lambda (x) "java.io.InputStream"))
(register-class-arm! out-stream? (lambda (x) "java.io.OutputStream"))
(register-class-arm! char-reader? (lambda (x) "java.io.Reader"))
(register-class-arm! char-writer? (lambda (x) "java.io.Writer"))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (not (symbol-t? type-sym)) 'pass
    (let ((short (last-dot (symbol-t-name type-sym))))
      (cond
        ((and (in-stream? val) (member short '("InputStream" "FileInputStream" "ByteArrayInputStream"
                                               "BufferedInputStream" "FilterInputStream" "Closeable" "AutoCloseable"))) #t)
        ((and (out-stream? val) (member short '("OutputStream" "FileOutputStream" "ByteArrayOutputStream"
                                                "BufferedOutputStream" "FilterOutputStream" "Closeable" "AutoCloseable" "Flushable"))) #t)
        ((and (char-reader? val) (member short '("Reader" "BufferedReader" "FileReader" "InputStreamReader"
                                                 "Closeable" "AutoCloseable" "Readable"))) #t)
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
(define (new-jpipe) (make-jpipe (make-mutex) (make-condition) '() '() 0 0 #f #f))

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
         (self (make-jhost "in-stream" (vector port #f 0 cell))))
    (when (pair? rest) (pipe-connect! self (car rest)))
    self))

(define (make-piped-out-stream . rest)
  (let* ((cell (box (new-jpipe)))
         (port (make-custom-binary-output-port
                "piped-output"
                (lambda (bv start count) (pipe-write! (unbox cell) bv start count))
                #f #f (lambda () #f)))
         (self (make-jhost "out-stream" (vector port #f #f cell))))
    (when (pair? rest) (pipe-connect! self (car rest)))
    self))

(register-class-ctor! "PipedInputStream" make-piped-in-stream)
(register-class-ctor! "java.io.PipedInputStream" make-piped-in-stream)
(register-class-ctor! "PipedOutputStream" make-piped-out-stream)
(register-class-ctor! "java.io.PipedOutputStream" make-piped-out-stream)
