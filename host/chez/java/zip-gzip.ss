;; zip-gzip.ss — java.util.zip.GZIPInputStream and GZIPOutputStream (JDK 21
;; GZIPInputStream.java, GZIPOutputStream.java), on the in-stream and
;; out-stream frames of zip-in-streams.ss and zip-out-streams.ss. Constructor
;; and method arguments are taken as zip-base.ss takes them.

;; --- reading the header and trailer -----------------------------------------

;; A byte source: BV[POS, LIM) first, then the wrapped stream. readTrailer reads
;; the Inflater's leftover input this way (the JDK's SequenceInputStream over a
;; ByteArrayInputStream and the wrapped stream); the constructor's header read
;; has no leftover.
;; SEQ is #t when readTrailer reads through the SequenceInputStream.
(define-record-type gzsrc
  (fields bv (mutable pos) lim inner seq)
  (nongenerative jolt-gzsrc-v1))

;; The next byte, 0..255, or -1 at the end.
(define (gzsrc-read src)
  (if (< (gzsrc-pos src) (gzsrc-lim src))
      (let ((b (bytevector-u8-ref (gzsrc-bv src) (gzsrc-pos src))))
        (gzsrc-pos-set! src (+ 1 (gzsrc-pos src)))
        b)
      (jnum->exact (record-method-dispatch (gzsrc-inner src) "read" jolt-nil))))

;; readUByte: EOFException with no message at the end; a value outside -1..255
;; is an IOException naming the wrapped stream's class.
(define (gzsrc-ubyte src)
  (let ((b (gzsrc-read src)))
    (when (= b -1) (zip-throw "java.io.EOFException" #f))
    (when (or (< b -1) (> b 255))
      (zip-throw "java.io.IOException"
                 (string-append (jolt-class-name (gzsrc-inner src))
                                ".read() returned value out of range -1..255: "
                                (number->string b))))
    b))

;; read(tmp, 0, n): up to N bytes into the byte[] TMP; -1 at the end. The
;; leftover answers first, then the wrapped stream. Through the
;; SequenceInputStream, a wrapped read of 0 or less is the end (JDK 21
;; SequenceInputStream.read(byte[], int, int)).
(define (gzsrc-read-into src tmp n)
  (let ((left (- (gzsrc-lim src) (gzsrc-pos src))))
    (if (> left 0)
        (let ((k (min n left)))
          (ja-bv->bytes! (gzsrc-bv src) (gzsrc-pos src) tmp 0 k)
          (gzsrc-pos-set! src (+ (gzsrc-pos src) k))
          k)
        (let ((k (jnum->exact (record-method-dispatch (gzsrc-inner src) "read"
                                (list->cseq (list tmp (->num 0) (->num n)))))))
          (if (and (gzsrc-seq src) (<= k 0)) -1 k)))))

(define (gzip-flag? flg bit) (not (zero? (bitwise-and flg bit))))

;; readHeader (lines 192-236): check the magic and method, skip MTIME/XFL/OS and
;; the optional fields, check the optional header CRC. Returns the header's
;; length in bytes. CRC is the CRC-32 of the header bytes read so far (the JDK's
;; CheckedInputStream).
(define (gzip-read-header src)
  (let ((crc 0)
        (tmp (na-byte-array 128))
        (one (make-bytevector 1)))
    (define (ubyte)
      (let ((b (gzsrc-ubyte src)))
        (bytevector-u8-set! one 0 b)
        (set! crc (zlib-crc32 crc one 0 1))
        b))
    (define (ushort)
      (let ((lo (ubyte)))
        (bitwise-ior (bitwise-arithmetic-shift-left (ubyte) 8) lo)))
    ;; skipBytes (lines 310-318): bulk reads of at most 128 bytes.
    (define (skip! count)
      (let loop ((count count))
        (when (> count 0)
          (let ((k (gzsrc-read-into src tmp (min count 128))))
            (when (= k -1) (zip-throw "java.io.EOFException" #f))
            (set! crc (zlib-crc32 crc (zip-bytes "read" tmp) 0 k))
            (loop (- count k))))))
    (unless (= (ushort) #x8b1f)
      (zip-throw "java.util.zip.ZipException" "Not in GZIP format"))
    (unless (= (ubyte) 8)
      (zip-throw "java.util.zip.ZipException" "Unsupported compression method"))
    (let ((flg (ubyte))
          (n 10))
      (skip! 6)                                  ; MTIME, XFL, OS
      (when (gzip-flag? flg 4)                   ; FEXTRA
        (let ((m (ushort)))
          (skip! m)
          (set! n (+ n m 2))))
      (when (gzip-flag? flg 8)                   ; FNAME
        (let loop () (set! n (+ n 1)) (unless (= (ubyte) 0) (loop))))
      (when (gzip-flag? flg 16)                  ; FCOMMENT
        (let loop () (set! n (+ n 1)) (unless (= (ubyte) 0) (loop))))
      (when (gzip-flag? flg 2)                   ; FHCRC
        (let ((v (bitwise-and crc #xffff)))
          (unless (= (ushort) v)
            (zip-throw "java.util.zip.ZipException" "Corrupt GZIP header"))
          (set! n (+ n 2))))
      n)))

;; readUInt: an unsigned 32-bit integer, little-endian.
(define (gzsrc-uint src)
  (let* ((b0 (gzsrc-ubyte src)) (b1 (gzsrc-ubyte src))
         (b2 (gzsrc-ubyte src)) (b3 (gzsrc-ubyte src)))
    (bitwise-ior b0
                 (bitwise-arithmetic-shift-left b1 8)
                 (bitwise-arithmetic-shift-left b2 16)
                 (bitwise-arithmetic-shift-left b3 24))))

;; --- GZIPInputStream --------------------------------------------------------
;; state: the CRC-32 of the member's data so far, and eos.
(define-record-type gzin
  (fields (mutable crc) (mutable eos))
  (nongenerative jolt-gzin-v1))

;; readTrailer (lines 243-270): check the trailer, then try the next member.
;; #t means the end of the stream; #f means another member follows.
(define (gzip-read-trailer! z)
  (let* ((g (zin-extra z))
         (inf (zin-codec z))
         (n (inflater-remaining inf))
         (len (zin-len z))
         (src (make-gzsrc (zip-bytes "buf" (zin-buf z)) (- len n) len (zin-inner z) (> n 0))))
    (when (or (not (= (gzsrc-uint src) (gzin-crc g)))
              (not (= (gzsrc-uint src) (bitwise-and (inflater-bytes-written inf) #xffffffff))))
      (zip-throw "java.util.zip.ZipException" "Corrupt GZIP trailer"))
    (let ((m (guard (e ((zip-thrown? e "IOException") #f))
               (+ 8 (gzip-read-header src)))))
      (if (not m)
          #t
          (begin
            (gzin-crc-set! g 0)
            (inflater-reset! inf)
            (when (> n m)
              (inflater-set-input! inf (zin-buf z) (->num (+ (- len n) m)) (->num (- n m))))
            #f)))))

;; read(byte[], off, len): lines 144-159.
(define (zin-gzip-read z bv start count)
  (let ((g (zin-extra z)))
    (if (gzin-eos g)
        0
        (let ((n (zin-inflate-read z bv start count)))
          (cond ((fx> n 0)
                 (gzin-crc-set! g (zlib-crc32 (gzin-crc g) bv start n))
                 n)
                ((gzip-read-trailer! z)
                 (gzin-eos-set! g #t)
                 0)
                (else (zin-gzip-read z bv start count)))))))

;; close(): lines 166-172.
(define (zin-gzip-close z)
  (zin-inflate-close z)
  (gzin-eos-set! (zin-extra z) #t))

;; GZIPInputStream(in) | GZIPInputStream(in, size). Each count has one overload,
;; so each argument is cast before the null and size checks. The header is read
;; here; on an IOException the Inflater ends (lines 77-86).
(define (make-gzip-input-stream . args)
  (zin-ctor-arity "java.util.zip.GZIPInputStream" args)
  (when (> (length args) 2)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.zip.GZIPInputStream"))
  (let* ((in (zin-stream-arg (car args)))
         (size (if (pair? (cdr args)) (zip-ctor-int-arg (cadr args)) 512)))
    (when (jolt-nil? in) (zip-throw "java.lang.NullPointerException" #f))
    (when (<= size 0) (zip-throw "java.lang.IllegalArgumentException" "buffer size <= 0"))
    (let* ((inf (make-inflater #t))
           (z (make-zin "java.util.zip.GZIPInputStream" in inf #t (na-byte-array size) 0 #f
                        zin-gzip-read zin-inflate-available zin-gzip-close
                        (make-gzin 0 #f))))
      (guard (e ((zip-thrown? e "IOException") (inflater-end! inf) (raise e)))
        (gzip-read-header (make-gzsrc (make-bytevector 0) 0 0 in #f)))
      (make-zin-stream z))))

;; --- GZIPOutputStream -------------------------------------------------------
;; state: the CRC-32 of the data written so far.
(define-record-type gzout
  (fields (mutable crc))
  (nongenerative jolt-gzout-v1))

;; out.write(arr, off, len), or out.write(arr) with no range, on the wrapped
;; stream.
(define (gzip-write! z arr . range)
  (record-method-dispatch (zout-inner z) "write"
    (list->cseq (cons arr (map ->num range)))))

;; writeTrailer: the data CRC-32, then getTotalIn(), both little-endian.
(define (gzip-trailer z)
  (let ((t (make-bytevector 8)))
    (bytevector-u32-set! t 0 (bitwise-and (gzout-crc (zout-extra z)) #xffffffff) (endianness little))
    (bytevector-u32-set! t 4 (bitwise-and (deflater-bytes-read (zout-codec z)) #xffffffff) (endianness little))
    t))

;; write(byte[], off, len): lines 145-150, the Deflater first, then the CRC.
(define (zout-gzip-write z bv start count)
  (zout-deflate-write z bv start count)
  (let ((g (zout-extra z)))
    (gzout-crc-set! g (zlib-crc32 (gzout-crc g) bv start count))))

;; finish(): lines 158-185. The trailer goes into the last buffer when it fits.
(define (zout-gzip-finish z)
  (let ((def (zout-codec z))
        (buf (zout-buf z)))
    (unless (deflater-finished? def)
      (guard (e ((zip-thrown? e "IOException")
                 (when (zout-owned z) (deflater-end! def))
                 (raise e)))
        (deflater-finish! def)
        (let loop ()
          (if (deflater-finished? def)
              (gzip-write! z (na-byte-array (gzip-trailer z)))
              (let ((len (jnum->exact (deflater-deflate def buf))))
                (if (and (deflater-finished? def) (<= len (- (ja-len buf) 8)))
                    (begin
                      (zip-store! buf len (gzip-trailer z) 8)
                      (gzip-write! z buf 0 (+ len 8)))
                    (begin
                      (when (> len 0) (gzip-write! z buf 0 len))
                      (loop))))))))))

;; GZIPOutputStream(out) | (out, size) | (out, syncFlush) | (out, size, syncFlush),
;; resolved as the compiled call resolves them: the two two-argument overloads
;; need an OutputStream (or nil) and a boolean or an integer, and the one-argument
;; and three-argument overloads cast each argument. The header is written here
;; (lines 90-99, 190-203).
(define (make-gzip-output-stream . args)
  (define (no-ctor)
    (throw-jvm 'IllegalArgumentException "No matching ctor found for class java.util.zip.GZIPOutputStream"))
  (define (cast-out x) (if (zout-out-fits? x) x (zip-ctor-class-cast x "java.io.OutputStream")))
  (let-values
      (((out size sync)
        (case (length args)
          ((1) (values (cast-out (car args)) 512 #f))
          ((2) (let ((o (car args)) (b (cadr args)))
                 (cond ((not (zout-out-fits? o)) (no-ctor))
                       ((boolean? b) (values o 512 b))
                       ((zip-long? b) (values o (zip-ctor-int-arg b) #f))
                       (else (no-ctor)))))
          ((3) (let* ((o (cast-out (car args)))
                      (n (zip-ctor-int-arg (cadr args)))
                      (s (zip-boolean-arg (caddr args))))
                 (values o n s)))
          (else (no-ctor)))))
    (when (jolt-nil? out) (zip-throw "java.lang.NullPointerException" #f))
    (let ((def (make-deflater -1 #t)))
      (when (<= size 0)
        (zip-throw "java.lang.IllegalArgumentException" "buffer size <= 0"))
      (let ((z (make-zout "java.util.zip.GZIPOutputStream" out def #t
                          (na-byte-array size) sync #f
                          zout-gzip-write zout-gzip-finish (make-gzout 0))))
        (gzip-write! z (na-byte-array (u8-list->bytevector '(31 139 8 0 0 0 0 0 0 255))))
        (make-zout-stream z)))))

;; --- in-stream read for GZIPInputStream -------------------------------------
;; read(byte[], off, len) checks eos before the null check and the length (lines
;; 145-148): after the end, a zero-length or out-of-range read is -1, not
;; InflaterInputStream's answer. The arguments are cast first; read(byte[])
;; reads the array's length before it reads, so a nil array is
;; NullPointerException even after the end. Other argument counts go to the
;; frame's read.
(define gzip-prior-read
  (hashtable-ref (hashtable-ref host-methods-tbl "in-stream" #f) "read" #f))
(define (gzip-read self . rest)
  (let ((z (zin-of self)))
    (if (and z (gzin? (zin-extra z)) (memv (length rest) '(1 3)))
        (let ((b (zin-bytes-arg (car rest))))
          (if (null? (cdr rest))
              (when (jolt-nil? b)
                (throw-jvm 'NullPointerException "Cannot read the array length because \"b\" is null"))
              (begin (zip-int-arg (cadr rest)) (zip-int-arg (caddr rest))))
          (zin-live-port self z)              ; ensureOpen: "Stream closed"
          (if (gzin-eos (zin-extra z))
              (->num -1)
              (apply gzip-prior-read self rest)))
        (apply gzip-prior-read self rest))))
(register-host-methods! "in-stream" (list (cons "read" gzip-read)))

(reg-ctor! '("GZIPInputStream" "java.util.zip.GZIPInputStream") make-gzip-input-stream)
(reg-ctor! '("GZIPOutputStream" "java.util.zip.GZIPOutputStream") make-gzip-output-stream)

;; GZIPInputStream's and GZIPOutputStream's JDK 21 signatures, every public
;; method each answers (records-interop.ss, the JDK signatures).
(register-method-sig! "java.util.zip.GZIPInputStream" "available" #f '() #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "close" #f '() #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "mark" #f '("int") #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "markSupported" #f '() #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "read" #f '() #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "read" #f '("[B") #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "read" #f '("[B" "int" "int") #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "readAllBytes" #f '() "[B")
(register-method-sig! "java.util.zip.GZIPInputStream" "readNBytes" #f '("int") "[B")
(register-method-sig! "java.util.zip.GZIPInputStream" "readNBytes" #f '("[B" "int" "int") #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "reset" #f '() #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "skip" #f '("long") #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "skipNBytes" #f '("long") #f)
(register-method-sig! "java.util.zip.GZIPInputStream" "transferTo" #f '("java.io.OutputStream") #f)
(register-method-sig! "java.util.zip.GZIPOutputStream" "close" #f '() #f)
(register-method-sig! "java.util.zip.GZIPOutputStream" "finish" #f '() #f)
(register-method-sig! "java.util.zip.GZIPOutputStream" "flush" #f '() #f)
(register-method-sig! "java.util.zip.GZIPOutputStream" "write" #f '("int") #f)
(register-method-sig! "java.util.zip.GZIPOutputStream" "write" #f '("[B") #f)
(register-method-sig! "java.util.zip.GZIPOutputStream" "write" #f '("[B" "int" "int") #f)
