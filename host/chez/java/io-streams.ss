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
                 (let ((b (get-u8 port))) (if (eof-object? b) -1 (->num b)))
                 (let* ((buf (car rest))
                        (vec (jolt-array-vec buf))
                        (off (if (>= (length rest) 3) (jnum->exact (cadr rest)) 0))
                        (len (if (>= (length rest) 3) (jnum->exact (caddr rest)) (vector-length vec))))
                   (let loop ((i 0))
                     (if (>= i len) (->num i)
                         (let ((b (get-u8 port)))
                           (if (eof-object? b)
                               (if (= i 0) -1 (->num i))
                               (begin (vector-set! vec (+ off i) b) (loop (+ i 1))))))))))))
   (cons "readAllBytes" (lambda (self) (let ((bv (get-bytevector-all (in-stream-port self))))
                                         (na-byte-array (if (eof-object? bv) (make-bytevector 0) bv)))))
   (cons "skip" (lambda (self n) (let ((bv (get-bytevector-n (in-stream-port self) (jnum->exact n))))
                                   (->num (if (eof-object? bv) 0 (bytevector-length bv))))))
   (cons "available" (lambda (self) (->num 0)))
   (cons "close" (lambda (self) (close-port (in-stream-port self)) jolt-nil))
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
               (else (error #f "OutputStream/write: unsupported" x)))
             jolt-nil)))
   (cons "flush" (lambda (self) (flush-output-port (out-stream-port self)) jolt-nil))
   (cons "close" (lambda (self) (flush-output-port (out-stream-port self))
                   ;; a ByteArrayOutputStream's close is a no-op (toByteArray stays valid);
                   ;; a file stream's port is closed.
                   (unless (vector-ref (jhost-state self) 1) (close-port (out-stream-port self))) jolt-nil))
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
(define (make-char-writer port) (make-jhost "char-writer" (vector port)))
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
   (cons "flush" (lambda (self) (flush-output-port (char-writer-port self)) jolt-nil))
   (cons "close" (lambda (self) (close-port (char-writer-port self)) jolt-nil))
   (cons "toString" (lambda (self) "#<Writer>"))))

;; --- constructors -----------------------------------------------------------
(define utf8-tx (make-transcoder (utf-8-codec)))
(define (path-of x) (project-relative (file-path-of x)))
(define (src-bytevector x)   ; a byte[] or Chez bytevector -> bytevector
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        (else (error #f "expected a byte array" x))))

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
(reg-ctor! '("OutputStreamWriter" "java.io.OutputStreamWriter")
  (lambda (out . _) (make-char-writer (transcoded-port (out-stream-port out) utf8-tx))))
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
        (else (error #f "io/input-stream: don't know how to open" x))))
(define (jio-output-stream x . rest)
  (cond ((out-stream? x) x)
        ((or (jfile? x) (string? x))
         (let ((append? (let loop ((o rest)) (cond ((or (null? o) (null? (cdr o))) #f)
                                                    ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "append") (jolt-truthy? (cadr o))) #t)
                                                    (else (loop (cddr o)))))))
           (make-out-stream (open-file-output-port (path-of x)
                              (if append? (file-options no-fail no-truncate append) (file-options no-fail))
                              (buffer-mode block)))))
        (else (error #f "io/output-stream: don't know how to open" x))))
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
            (error #f (string-append "Couldn't delete " p))))))
(def-var! "clojure.java.io" "delete-file" jio-delete-file)

;; io/copy: file/path/reader/stream/string/byte[] -> writer/stream/file/path.
;; A byte source copies byte-exact to a byte/file destination (no lossy text
;; round-trip); otherwise the content is read as text. UTF-8 bridges byte<->char.
(define (input-bytes input)   ; bytevector for a byte source, else #f
  (cond ((in-stream? input) (let ((bv (get-bytevector-all (in-stream-port input)))) (if (eof-object? bv) (make-bytevector 0) bv)))
        ((bytevector? input) input)
        ((and (jolt-array? input) (eq? (jolt-array-kind input) 'byte)) (na-bytearray->bv input))
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
     (let ((bv (cond
                 ((jfile? input) (read-file-bytes (path-of input)))
                 ((not (string? input)) (input-bytes input))
                 (else #f))))
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
         (list->cseq (list (if bv (make-jolt-array (list->vector (bytevector->u8-list bv)) 'byte)
                               (input-text input)))))))
    (else (error #f "io/copy: don't know how to write to" output)))
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
