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
(define (in-stream-port self) (vector-ref (jhost-state self) 0))
(define (make-in-stream port) (make-jhost "in-stream" (vector port)))
(define (in-stream? x) (and (jhost? x) (string=? (jhost-tag x) "in-stream")))
(register-host-methods! "in-stream"
  (list
   (cons "read"
         (lambda (self . rest)
           (let ((port (in-stream-port self)))
             (if (null? rest)
                 ;; InputStream.read() returns the byte as an UNSIGNED int 0..255
                 ;; (-1 at EOF) — the one place a byte is not signed, because the
                 ;; return has to distinguish 0xff from end-of-stream.
                 (let ((b (get-u8 port))) (if (eof-object? b) -1 (->num b)))
                 ;; read(buf …) fills a byte-array, whose elements ARE signed.
                 (let* ((buf (car rest))
                        (vec (jolt-array-vec buf))
                        (off (if (>= (length rest) 3) (jnum->exact (cadr rest)) 0))
                        (len (if (>= (length rest) 3) (jnum->exact (caddr rest)) (vector-length vec))))
                   (let loop ((i 0))
                     (if (>= i len) (->num i)
                         (let ((b (get-u8 port)))
                           (if (eof-object? b)
                               (if (= i 0) -1 (->num i))
                               (begin (vector-set! vec (+ off i) (na-u8->byte b)) (loop (+ i 1))))))))))))
   (cons "readAllBytes" (lambda (self) (let ((bv (get-bytevector-all (in-stream-port self))))
                                         (na-byte-array (if (eof-object? bv) (make-bytevector 0) bv)))))
   (cons "skip" (lambda (self n) (let ((bv (get-bytevector-n (in-stream-port self) (jnum->exact n))))
                                   (->num (if (eof-object? bv) 0 (bytevector-length bv))))))
   (cons "available" (lambda (self) (->num 0)))
   ;; A piped stream marks its shared pipe instead of closing the Chez port: a
   ;; read after close has to raise the JVM's IOException, and a closed Chez port
   ;; raises a classless host error before the port's own reader is consulted.
   (cons "close" (lambda (self)
                   (let ((p (piped-pipe self)))
                     (if p (pipe-close-read! p) (close-port (in-stream-port self))))
                   jolt-nil))
   (cons "connect" (lambda (self other) (pipe-connect! self other) jolt-nil))
   (cons "mark" (lambda (self . _) jolt-nil))
   (cons "reset" (lambda (self) (guard (e (#t jolt-nil)) (set-port-position! (in-stream-port self) 0) jolt-nil)))
   (cons "markSupported" (lambda (self) #f))
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

(reg-ctor! '("FileInputStream" "java.io.FileInputStream")
  (lambda (src . _) (make-in-stream (open-file-input-port (path-of src) (file-options) (buffer-mode block)))))
(reg-ctor! '("FileOutputStream" "java.io.FileOutputStream")
  (lambda (src . rest)
    (let ((append? (and (pair? rest) (jolt-truthy? (car rest)))))
      (make-out-stream (open-file-output-port (path-of src)
                         (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                         (buffer-mode block))))))
(reg-ctor! '("ByteArrayInputStream" "java.io.ByteArrayInputStream")
  (lambda (bytes . rest)
    (let ((bv (src-bytevector bytes)))
      (make-in-stream (open-bytevector-input-port
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

(reg-ctor! '("ByteArrayOutputStream" "java.io.ByteArrayOutputStream")
  (lambda _
    (call-with-values open-bytevector-output-port
      (lambda (port extract) (make-jhost "out-stream" (vector port extract (make-bytevector 0)))))))
(reg-ctor! '("FileReader" "java.io.FileReader")
  (lambda (src . _) (make-char-reader (transcoded-port (open-file-input-port (path-of src) (file-options) (buffer-mode block)) utf8-tx))))
(reg-ctor! '("FileWriter" "java.io.FileWriter")
  (lambda (src . rest)
    (let ((append? (and (pair? rest) (jolt-truthy? (car rest)))))
      (make-char-writer (transcoded-port (open-file-output-port (path-of src)
                          (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                          (buffer-mode block)) utf8-tx)))))
;; InputStreamReader / OutputStreamWriter take ownership of the wrapped byte
;; stream's port and transcode it (UTF-8 default; an explicit charset is honored
;; only as UTF-8 here).
(reg-ctor! '("InputStreamReader" "java.io.InputStreamReader")
  (lambda (in . _) (make-char-reader (transcoded-port (in-stream-port in) utf8-tx))))
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
    (make-char-writer (transcoded-port (out-stream-sink-port out) utf8-tx) out)))
;; Buffered* — Chez ports are buffered already; the wrapper is the wrapped stream.
(for-each (lambda (n) (register-class-ctor! n (lambda (inner . _) inner)))
          '("BufferedReader" "java.io.BufferedReader"
            "BufferedWriter" "java.io.BufferedWriter"
            "BufferedInputStream" "java.io.BufferedInputStream"
            "BufferedOutputStream" "java.io.BufferedOutputStream"))

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
            ((and (jhost? target)
                  (member (jhost-tag target) '("writer" "file-writer" "port-writer" "print-writer")))
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
(define (jio-input-stream x)
  (cond ((in-stream? x) x)
        ((jfile? x) (make-in-stream (open-file-input-port (jfile-fs x) (file-options) (buffer-mode block))))
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (make-in-stream (open-bytevector-input-port (na-bytearray->bv x))))
        ((bytevector? x) (make-in-stream (open-bytevector-input-port x)))
        ((and (jhost? x) (string=? (jhost-tag x) "url")) (make-in-stream (open-file-input-port (url-strip-scheme (url-spec x)) (file-options) (buffer-mode block))))
        ((string? x) (make-in-stream (open-file-input-port (project-relative x) (file-options) (buffer-mode block))))
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
        (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str x) "> as an OutputStream.")))))
(def-var! "clojure.java.io" "input-stream" jio-input-stream)
(def-var! "clojure.java.io" "output-stream" jio-output-stream)

;; io/make-parents: create the parent directories of the last path segment.
(define (jio-make-parents . args)
  (let ((p (apply-make-file-path args)))
    (let loop ((i (- (string-length p) 1)))
      (cond ((<= i 0) #f)
            ((char=? (string-ref p i) #\/) (mkdirs! (substring p 0 i)))
            (else (loop (- i 1)))))))
(define (apply-make-file-path args)
  (jfile-path (apply jolt-make-file args)))
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
    ((and (jhost? output) (member (jhost-tag output) '("writer" "file-writer" "port-writer" "print-writer")))
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
  ;; chunks is a queue of bytevectors oldest-first, with `pos` bytes of the head
  ;; already handed out.
  (fields mu cv (mutable chunks) (mutable pos) (mutable wclosed?) (mutable rclosed?))
  (nongenerative jpipe-v1))
(define (new-jpipe) (make-jpipe (make-mutex) (make-condition) '() 0 #f #f))

(define (pipe-io-throw msg) (jolt-throw (jolt-host-throwable "java.io.IOException" msg)))

(define (pipe-write! p bv start count)
  (with-mutex (jpipe-mu p)
    (when (jpipe-rclosed? p) (pipe-io-throw "Read end dead"))
    (when (jpipe-wclosed? p) (pipe-io-throw "Pipe closed"))
    (let ((chunk (make-bytevector count)))
      (bytevector-copy! bv start chunk 0 count)
      (jpipe-chunks-set! p (append (jpipe-chunks p) (list chunk)))
      (condition-broadcast (jpipe-cv p))))
  count)

;; Blocks while the pipe is empty and the writer is still open. condition-wait
;; releases the mutex and deactivates the thread, so a writer can run and the
;; collector is not held up by a parked reader.
(define (pipe-read! p bv start count)
  (with-mutex (jpipe-mu p)
    (let loop ()
      (cond
        ((jpipe-rclosed? p) (pipe-io-throw "Pipe closed"))
        ((pair? (jpipe-chunks p))
         (let* ((head (car (jpipe-chunks p)))
                (avail (- (bytevector-length head) (jpipe-pos p)))
                (n (min avail count)))
           (bytevector-copy! head (jpipe-pos p) bv start n)
           (if (= n avail)
               (begin (jpipe-chunks-set! p (cdr (jpipe-chunks p))) (jpipe-pos-set! p 0))
               (jpipe-pos-set! p (+ (jpipe-pos p) n)))
           n))
        ((jpipe-wclosed? p) 0)                      ; writer done: end of stream
        (else (condition-wait (jpipe-cv p) (jpipe-mu p)) (loop))))))

(define (pipe-close-write! p)
  (with-mutex (jpipe-mu p)
    (jpipe-wclosed?-set! p #t)
    (condition-broadcast (jpipe-cv p))))
(define (pipe-close-read! p)
  (with-mutex (jpipe-mu p)
    (jpipe-rclosed?-set! p #t)
    (condition-broadcast (jpipe-cv p))))

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
    (unless (and ca cb) (pipe-io-throw "Not a piped stream"))
    (let ((shared (unbox ca)))
      (set-box! cb shared))))

(define (make-piped-in-stream . rest)
  (let* ((cell (box (new-jpipe)))
         (port (make-custom-binary-input-port
                "piped-input"
                (lambda (bv start count) (pipe-read! (unbox cell) bv start count))
                #f #f (lambda () #f)))
         (self (make-jhost "in-stream" (vector port #f #f cell))))
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
