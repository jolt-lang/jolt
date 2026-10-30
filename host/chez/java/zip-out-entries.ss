;; zip-out-entries.ss — java.util.zip.ZipOutputStream (JDK 21 ZipOutputStream.java,
;; ZipUtils.java), the write side of zip archives, on the zip-out-streams.ss
;; frame: an out-stream whose port write! runs the JDK's write(byte[], off,
;; len) for the current entry, with a raw Deflater for DEFLATED entries and a
;; straight copy for STORED ones. putNextEntry writes the local header,
;; closeEntry the data descriptor (when the sizes were not known up front) and
;; checks the sizes and CRC-32 that were, finish writes the central directory
;; and the END record, and close finishes first. Every check and message is
;; the JDK's; Zip64 (a size or count past 32 bits) is refused where the JDK
;; would write it, and test/conformance/known-divergences.edn says so.

;; --- the writer's state -----------------------------------------------------
(define-record-type zipout
  (fields (mutable entries)     ; XEntries written, newest first: (entry flag . offset)
          names                 ; hashtable of the names put so far
          (mutable crc)         ; CRC-32 of the current entry's data
          (mutable written)     ; bytes written to the wrapped stream
          (mutable locoff)      ; where the current entry's data began
          (mutable comment)     ; the archive comment bytes, or #f
          (mutable method)      ; the default method for entries that set none
          (mutable finished)
          (mutable current)     ; the XEntry being written, or #f
          coder                 ; the charset's canonical name, or #f for UTF-8
          (mutable self))       ; the out-stream this belongs to, set at construction
  (nongenerative jolt-zipout-v1))

(define (zipout-of z) (zout-extra z))
(define (xentry-entry x) (car x))
(define (xentry-flag x) (cadr x))
(define (xentry-offset x) (cddr x))

;; out.write(arr[, off, len]) on the wrapped stream, counting the bytes.
(define (zipout-write! z bv)
  (let ((st (zipout-of z)))
    (record-method-dispatch (zout-inner z) "write"
      (list->cseq (list (na-byte-array bv) (->num 0) (->num (bytevector-length bv)))))
    (zipout-written-set! st (+ (zipout-written st) (bytevector-length bv)))))

;; Little-endian fields into a growing byte list (ZipUtils writeShort/writeInt/
;; writeLong).
(define (zip-le16 n) (let ((b (make-bytevector 2))) (bytevector-u16-set! b 0 (bitwise-and n #xffff) (endianness little)) b))
(define (zip-le32 n) (let ((b (make-bytevector 4))) (bytevector-u32-set! b 0 (bitwise-and n #xffffffff) (endianness little)) b))
(define (zip-cat . bvs)
  (let* ((n (apply + (map bytevector-length bvs)))
         (out (make-bytevector n)))
    (let loop ((bvs bvs) (off 0))
      (if (null? bvs)
          out
          (begin (bytevector-copy! (car bvs) 0 out off (bytevector-length (car bvs)))
                 (loop (cdr bvs) (+ off (bytevector-length (car bvs)))))))))

;; A name or comment in the archive's charset (ZipCoder.getBytes): UTF-8, or the
;; charset the constructor was given, through the String encoder.
(define (zipout-encode st s)
  (if (zipout-coder st)
      (charset-encode-bv s (zipout-coder st))
      (string->utf8 s)))

;; The entry's extra field as written: the bytes it carries, less any Zip64 or
;; timestamp block, plus the extended-timestamp block (0x5455) for an entry
;; whose time was set as an mtime (lines 490-518, 592-612; ZipUtils lines
;; 217-245). The JDK writes the block in the local header with the flag and
;; mtime only, and the same in the central directory.
(define zip-extid-zip64 #x0001)
(define zip-extid-ntfs #x000a)
(define zip-extid-extt #x5455)
(define (zipout-extra-bytes e)
  (let* ((given (let ((x (zentry-extra e)))
                  (if (jolt-nil? x) (make-bytevector 0) (zip-bytes "extra" x))))
         (kept (let loop ((off 0) (acc '()))
                 (if (> (+ off 4) (bytevector-length given))
                     (apply zip-cat (reverse acc))
                     (let* ((tag (bytevector-u16-ref given off (endianness little)))
                            (sz (bytevector-u16-ref given (+ off 2) (endianness little)))
                            (end (min (bytevector-length given) (+ off 4 sz))))
                       (loop end
                             (if (memv tag (list zip-extid-zip64 zip-extid-ntfs zip-extid-extt))
                                 acc
                                 (cons (let ((b (make-bytevector (- end off))))
                                         (bytevector-copy! given off b 0 (- end off))
                                         b)
                                       acc)))))))
         (mtime (zentry-mtime e)))
    ;; the timestamp block first, then the entry's own blocks (writeExtra)
    (if mtime
        (zip-cat (zip-le16 zip-extid-extt) (zip-le16 5) (make-bytevector 1 1)
                 (zip-le32 (div mtime 1000))
                 kept)
        kept)))

;; writeLOC (lines 475-547): the local header of X.
(define (zipout-write-loc! z x)
  (let* ((st (zipout-of z))
         (e (xentry-entry x))
         (flag (xentry-flag x))
         (name (zipout-encode st (zentry-name e)))
         (extra (zipout-extra-bytes e))
         (descriptor? (not (zero? (bitwise-and flag 8))))
         (deflated? (= (zentry-method e) zip-deflated)))
    (when (or (> (bytevector-length name) #xffff) (> (bytevector-length extra) #xffff))
      (zip-throw "java.util.zip.ZipException" "entry name or extra field too long"))
    (when (and (not descriptor?)
               (or (>= (zentry-size e) #xffffffff) (>= (zentry-csize e) #xffffffff)))
      (zip-throw "java.util.zip.ZipException" "Zip64 entries are not supported"))
    (zipout-write! z
      (zip-cat (zip-le32 zip-locsig)
               (zip-le16 (if deflated? 20 10))            ; version needed to extract
               (zip-le16 flag)
               (zip-le16 (zentry-method e))
               (zip-le32 (zentry-xdostime e))
               (if descriptor?
                   (zip-cat (zip-le32 0) (zip-le32 0) (zip-le32 0))
                   (zip-cat (zip-le32 (zentry-crc e)) (zip-le32 (zentry-csize e)) (zip-le32 (zentry-size e))))
               (zip-le16 (bytevector-length name))
               (zip-le16 (bytevector-length extra))
               name
               extra))
    (zipout-locoff-set! st (zipout-written st))))

;; writeEXT (lines 553-567): the data descriptor after a DEFLATED entry whose
;; sizes were not known up front.
(define (zipout-write-ext! z e)
  (when (or (>= (zentry-size e) #xffffffff) (>= (zentry-csize e) #xffffffff))
    (zip-throw "java.util.zip.ZipException" "Zip64 entries are not supported"))
  (zipout-write! z
    (zip-cat (zip-le32 zip-extsig) (zip-le32 (zentry-crc e))
             (zip-le32 (zentry-csize e)) (zip-le32 (zentry-size e)))))

;; writeCEN (lines 574-681): the central directory record of X.
(define (zipout-write-cen! z x)
  (let* ((st (zipout-of z))
         (e (xentry-entry x))
         (flag (xentry-flag x))
         (name (zipout-encode st (zentry-name e)))
         (extra (zipout-extra-bytes e))
         ;; an entry comment past 0xFFFF bytes is cut there, as the JDK cuts it
         (comment (if (jolt-nil? (zentry-comment e))
                      (make-bytevector 0)
                      (let ((bv (zipout-encode st (zentry-comment e))))
                        (if (> (bytevector-length bv) #xffff)
                            (let ((c (make-bytevector #xffff))) (bytevector-copy! bv 0 c 0 #xffff) c)
                            bv))))
         (deflated? (= (zentry-method e) zip-deflated)))
    (when (or (>= (zentry-size e) #xffffffff) (>= (zentry-csize e) #xffffffff)
              (>= (xentry-offset x) #xffffffff))
      (zip-throw "java.util.zip.ZipException" "Zip64 entries are not supported"))
    (zipout-write! z
      (zip-cat (zip-le32 zip-censig)
               (zip-le16 (if deflated? 20 10))            ; version made by
               (zip-le16 (if deflated? 20 10))            ; version needed to extract
               (zip-le16 flag)
               (zip-le16 (zentry-method e))
               (zip-le32 (zentry-xdostime e))
               (zip-le32 (zentry-crc e))
               (zip-le32 (zentry-csize e))
               (zip-le32 (zentry-size e))
               (zip-le16 (bytevector-length name))
               (zip-le16 (bytevector-length extra))
               (zip-le16 (bytevector-length comment))
               (zip-le16 0)                               ; starting disk number
               (zip-le16 0)                               ; internal file attributes
               (zip-le32 0)                               ; external file attributes
               (zip-le32 (xentry-offset x))
               name extra comment))))

;; writeEND (lines 687-735).
(define (zipout-write-end! z off len)
  (let* ((st (zipout-of z))
         (count (length (zipout-entries st)))
         (comment (or (zipout-comment st) (make-bytevector 0))))
    (when (or (> count #xffff) (> len #xffffffff) (> off #xffffffff))
      (zip-throw "java.util.zip.ZipException" "Zip64 archives are not supported"))
    (zipout-write! z
      (zip-cat (zip-le32 zip-endsig)
               (zip-le16 0) (zip-le16 0)                  ; this disk, the directory's disk
               (zip-le16 count) (zip-le16 count)
               (zip-le32 len) (zip-le32 off)
               (zip-le16 (bytevector-length comment))
               comment))))

(define (zipout-ensure-open z)
  (when (zout-closed z) (zip-throw "java.io.IOException" "Stream closed")))

;; putNextEntry (lines 197-243).
(define (zipout-put-next-entry! self z e)
  (let ((st (zipout-of z)))
    (zipout-ensure-open z)
    (when (zipout-current st) (zipout-close-entry! self z))
    (let ((ent (zentry-of e)))
      (when (= (zentry-xdostime ent) -1)
        (zentry-set-time! ent (now-millis)))
      (when (= (zentry-method ent) -1)
        (zentry-method-set! ent (zipout-method st)))
      (let ((flag
             (cond
               ((= (zentry-method ent) zip-deflated)
                ;; store size, compressed size and crc-32 in the data descriptor
                ;; immediately following the compressed entry data
                (if (or (= (zentry-size ent) -1) (= (zentry-csize ent) -1) (= (zentry-crc ent) -1))
                    8
                    0))
               ((= (zentry-method ent) zip-stored)
                ;; compressed size, uncompressed size, and crc-32 must all be
                ;; set for entries using STORED compression method
                (cond ((= (zentry-size ent) -1) (zentry-size-set! ent (zentry-csize ent)))
                      ((= (zentry-csize ent) -1) (zentry-csize-set! ent (zentry-size ent)))
                      ((not (= (zentry-size ent) (zentry-csize ent)))
                       (zip-throw "java.util.zip.ZipException" "STORED entry where compressed != uncompressed size")))
                (when (or (= (zentry-size ent) -1) (= (zentry-crc ent) -1))
                  (zip-throw "java.util.zip.ZipException" "STORED entry missing size, compressed size, or crc-32"))
                0)
               (else (zip-throw "java.util.zip.ZipException" "unsupported compression method")))))
        (when (hashtable-ref (zipout-names st) (zentry-name ent) #f)
          (zip-throw "java.util.zip.ZipException" (string-append "duplicate entry: " (zentry-name ent))))
        (hashtable-set! (zipout-names st) (zentry-name ent) #t)
        (let ((x (cons ent (cons (if (zipout-coder st) flag (bitwise-ior flag zip-use-utf8))
                                 (zipout-written st)))))
          (zipout-current-set! st x)
          (zipout-entries-set! st (cons x (zipout-entries st)))
          (zipout-write-loc! z x))))))

;; closeEntry (lines 249-306).
(define (zipout-close-entry! self z)
  (let ((st (zipout-of z)))
    (zipout-ensure-open z)
    ;; everything the caller put into the port belongs to this entry
    (zout-flush-port! (out-stream-port self))
    (let ((x (zipout-current st)))
      (when x
        (let ((e (xentry-entry x))
              (def (zout-codec z)))
          (cond
            ((= (zentry-method e) zip-deflated)
             (deflater-finish! def)
             (let loop ()
               (unless (deflater-finished? def)
                 (zout-deflate! z)
                 (loop)))
             ;; the deflated bytes went to the wrapped stream through
             ;; zout-deflate!, which does not count them: the Deflater does
             (zipout-written-set! st (+ (zipout-locoff st) (deflater-bytes-written def)))
             (if (zero? (bitwise-and (xentry-flag x) 8))
                 ;; verify size, compressed size, and crc-32 settings
                 (begin
                   (unless (= (zentry-size e) (deflater-bytes-read def))
                     (zip-throw "java.util.zip.ZipException"
                                (string-append "invalid entry size (expected " (number->string (zentry-size e))
                                               " but got " (number->string (deflater-bytes-read def)) " bytes)")))
                   (unless (= (zentry-csize e) (deflater-bytes-written def))
                     (zip-throw "java.util.zip.ZipException"
                                (string-append "invalid entry compressed size (expected "
                                               (number->string (zentry-csize e))
                                               " but got " (number->string (deflater-bytes-written def)) " bytes)")))
                   (unless (= (zentry-crc e) (zipout-crc st))
                     (zip-throw "java.util.zip.ZipException"
                                (string-append "invalid entry CRC-32 (expected 0x" (zip-hex (zentry-crc e))
                                               " but got 0x" (zip-hex (zipout-crc st)) ")"))))
                 (begin
                   (zentry-size-set! e (deflater-bytes-read def))
                   (zentry-csize-set! e (deflater-bytes-written def))
                   (zentry-crc-set! e (zipout-crc st))
                   (zipout-write-ext! z e)))
             (deflater-reset! def))
            ((= (zentry-method e) zip-stored)
             ;; we already know that both e.size and e.csize are the same
             (unless (= (zentry-size e) (- (zipout-written st) (zipout-locoff st)))
               (zip-throw "java.util.zip.ZipException"
                          (string-append "invalid entry size (expected " (number->string (zentry-size e))
                                         " but got " (number->string (- (zipout-written st) (zipout-locoff st)))
                                         " bytes)")))
             (unless (= (zentry-crc e) (zipout-crc st))
               (zip-throw "java.util.zip.ZipException"
                          (string-append "invalid entry crc-32 (expected 0x" (zip-hex (zentry-crc e))
                                         " but got 0x" (zip-hex (zipout-crc st)) ")"))))
            (else (zip-throw "java.util.zip.ZipException" "invalid compression method")))
          (zipout-crc-set! st 0)
          (zipout-current-set! st #f))))))

;; The body of write(byte[], off, len) (lines 322-343), run by the port's
;; write! and by the method after its checks.
(define (zout-zip-write z bv start count)
  (let ((st (zipout-of z)))
    (when (fx> count 0)
      (let ((x (zipout-current st)))
        (unless x (zip-throw "java.util.zip.ZipException" "no current ZIP entry"))
        (let ((e (xentry-entry x)))
          (cond
            ((= (zentry-method e) zip-deflated)
             (zout-deflate-write z bv start count))
            ((= (zentry-method e) zip-stored)
             (zipout-written-set! st (+ (zipout-written st) count))
             (when (> (- (zipout-written st) (zipout-locoff st)) (zentry-size e))
               (zip-throw "java.util.zip.ZipException" "attempt to write past end of STORED entry"))
             (let ((chunk (make-bytevector count)))
               (bytevector-copy! bv start chunk 0 count)
               (record-method-dispatch (zout-inner z) "write"
                 (list->cseq (list (na-byte-array chunk) (->num 0) (->num count))))))
            (else (zip-throw "java.util.zip.ZipException" "invalid compression method")))
          (zipout-crc-set! st (zlib-crc32 (zipout-crc st) bv start count)))))))

;; finish (lines 351-371): close the entry in progress, then the central
;; directory and the END record.
(define (zout-zip-finish z)
  (let ((st (zipout-of z)))
    (zipout-ensure-open z)
    (unless (zipout-finished st)
      (when (zipout-current st)
        (zipout-close-entry! (zipout-self st) z))
      (let ((off (zipout-written st)))
        (for-each (lambda (x) (zipout-write-cen! z x)) (reverse (zipout-entries st)))
        (zipout-write-end! z off (- (zipout-written st) off))
        (zipout-finished-set! st #t)))))

;; ZipOutputStream(out) | ZipOutputStream(out, charset): lines 155-172.
(define (make-zip-output-stream . args)
  (make-zip-output-stream* "java.util.zip.ZipOutputStream" args))
;; The constructor under the class FQN reports (JarOutputStream builds on it).
(define (make-zip-output-stream* fqn args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException (string-append "No matching ctor found for class " fqn)))
  (unless (<= 1 (length args) 2) (no-ctor))
  (let* ((out (let ((x (car args)))
                (if (zout-out-fits? x) x (zip-class-cast x "java.io.OutputStream"))))
         (cs (if (pair? (cdr args))
                 (let ((c (cadr args)))
                   (if (or (jolt-nil? c) (zip-charset? c)) c (zip-class-cast c "java.nio.charset.Charset")))
                 #f)))
    (when (jolt-nil? out) (zip-throw "java.lang.NullPointerException" #f))
    (when (and cs (jolt-nil? cs)) (zip-throw "java.lang.NullPointerException" "charset is null"))
    (let* ((coder (and cs
                       (let ((name (charset-canonical-down (charset-arg-name cs))))
                         (and (not (string=? name "utf-8")) name))))
           (st (make-zipout '() (make-hashtable string-hash string=?) 0 0 0 #f zip-deflated #f #f coder #f))
           (z (make-zout fqn out (make-deflater -1 #t) #t
                         (na-byte-array 512) #f #f
                         zout-zip-write zout-zip-finish st))
           (self (make-zout-stream z)))
      ;; closeEntry flushes the port through the stream, so finish needs it
      (zipout-self-set! st self)
      self)))

;; --- the methods only ZipOutputStream has ------------------------------------
(define (zipout-method-of name arities f)
  (host-arity-of arities #t
    (lambda (self . args)
      (let ((z (zout-of self)))
        (if (and z (zipout? (zout-extra z)) (memv (length args) arities))
            (apply f self z args)
            (no-method-throw name self (length args)))))))

(register-host-methods! "out-stream"
  (list
   (cons "putNextEntry"
         (zipout-method-of "putNextEntry" '(1)
           (lambda (self z e)
             (cond ((jolt-nil? e) (zip-throw "java.lang.NullPointerException" #f))
                   ((zip-entry? e) (zipout-put-next-entry! self z e) jolt-nil)
                   (else (zip-class-cast e "java.util.zip.ZipEntry"))))))
   (cons "closeEntry"
         (zipout-method-of "closeEntry" '(0)
           (lambda (self z) (zipout-close-entry! self z) jolt-nil)))
   ;; setComment (lines 121-128): the comment's bytes in the archive's charset
   (cons "setComment"
         (zipout-method-of "setComment" '(1)
           (lambda (self z c)
             (let ((st (zipout-of z)))
               (cond ((jolt-nil? c) (zipout-comment-set! st #f))
                     ((string? c)
                      (let ((bv (zipout-encode st c)))
                        (when (> (bytevector-length bv) #xffff)
                          (zip-throw "java.lang.IllegalArgumentException" "ZIP file comment too long"))
                        (zipout-comment-set! st bv)))
                     (else (zip-class-cast c "java.lang.String")))
               jolt-nil))))
   ;; setMethod (lines 135-141)
   (cons "setMethod"
         (zipout-method-of "setMethod" '(1)
           (lambda (self z m)
             (let ((m (zip-int-arg m)))
               (unless (or (= m zip-deflated) (= m zip-stored))
                 (zip-throw "java.lang.IllegalArgumentException" "invalid compression method"))
               (zipout-method-set! (zipout-of z) m)
               jolt-nil))))
   ;; setLevel (lines 148-150): the Deflater's setLevel, with its check
   (cons "setLevel"
         (zipout-method-of "setLevel" '(1)
           (lambda (self z level)
             (record-method-dispatch (zout-codec z) "setLevel" (jolt-list level))
             jolt-nil)))))

;; write(byte[], off, len) on a ZipOutputStream checks ensureOpen and the range
;; before "no current ZIP entry" (lines 322-330); the frame's write does the
;; first two, and the entry check is zout-zip-write's when the bytes arrive.
;; A zero-length write is nothing (line 331), which the frame already skips.

(hashtable-set! jhost-tag->fqn "zip-output-stream" "java.util.zip.ZipOutputStream")
(reg-ctor! '("ZipOutputStream" "java.util.zip.ZipOutputStream") make-zip-output-stream)
