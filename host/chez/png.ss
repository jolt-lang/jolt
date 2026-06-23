;; png.ss — jolt.png: a minimal PNG writer, the built-in the
;; ray-tracer-multi example renders through. Truecolor (8-bit RGB), no
;; compression: the IDAT zlib stream uses DEFLATE "stored" (uncompressed) blocks,
;; so there is no compressor to carry — just CRC-32 / Adler-32 framing over Chez
;; bytevectors. def-var!'d into the jolt.png namespace, so a (require '[jolt.png])
;; resolves it as a baked namespace (no source file).

;; --- CRC-32 (PNG chunk checksum) --------------------------------------------
(define png-crc-table
  (let ((t (make-vector 256)))
    (do ((n 0 (+ n 1))) ((= n 256) t)
      (let loop ((c n) (k 0))
        (if (= k 8)
            (vector-set! t n c)
            (loop (if (odd? c) (bitwise-xor #xedb88320 (bitwise-arithmetic-shift-right c 1))
                               (bitwise-arithmetic-shift-right c 1))
                  (+ k 1)))))))
(define (png-crc32 bv)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0) (c #xffffffff))
      (if (= i len)
          (bitwise-xor c #xffffffff)
          (loop (+ i 1)
                (bitwise-xor (bitwise-arithmetic-shift-right c 8)
                             (vector-ref png-crc-table
                               (bitwise-and (bitwise-xor c (bytevector-u8-ref bv i)) #xff))))))))

;; --- Adler-32 (zlib checksum) -----------------------------------------------
(define (png-adler32 bv)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0) (a 1) (b 0))
      (if (= i len)
          (bitwise-ior (bitwise-arithmetic-shift-left b 16) a)
          (let ((a* (modulo (+ a (bytevector-u8-ref bv i)) 65521)))
            (loop (+ i 1) a* (modulo (+ b a*) 65521)))))))

;; --- byte helpers -----------------------------------------------------------
(define (png-u32be n)
  (let ((bv (make-bytevector 4)))
    (bytevector-u8-set! bv 0 (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff))
    (bytevector-u8-set! bv 1 (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff))
    (bytevector-u8-set! bv 2 (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff))
    (bytevector-u8-set! bv 3 (bitwise-and n #xff))
    bv))
(define (png-bytes . bs) (u8-list->bytevector bs))
(define (png-cat . bvs)
  (let* ((total (apply + (map bytevector-length bvs)))
         (out (make-bytevector total)))
    (let loop ((bvs bvs) (off 0))
      (if (null? bvs) out
          (let ((n (bytevector-length (car bvs))))
            (bytevector-copy! (car bvs) 0 out off n)
            (loop (cdr bvs) (+ off n)))))))

;; one PNG chunk: length(4) + type(4) + data + crc32(type+data)(4)
(define (png-chunk type-str data)
  (let ((type (string->utf8 type-str)))
    (png-cat (png-u32be (bytevector-length data)) type data
             (png-u32be (png-crc32 (png-cat type data))))))

;; DEFLATE "stored" stream of raw: ≤65535-byte blocks, each 1 header byte
;; (BFINAL bit) + LEN(2 LE) + NLEN(2 LE) + raw. Wrapped as zlib (0x78 0x01 …
;; adler32).
(define (png-deflate-stored raw)
  (let ((len (bytevector-length raw)))
    (let loop ((off 0) (acc (list (png-bytes #x78 #x01))))
      (if (>= off len)
          (apply png-cat (reverse (cons (png-u32be (png-adler32 raw)) acc)))
          (let* ((n (min 65535 (- len off)))
                 (final (if (>= (+ off n) len) 1 0))
                 (block (png-cat (png-bytes final)
                                 (png-bytes (bitwise-and n #xff) (bitwise-arithmetic-shift-right n 8))
                                 (png-bytes (bitwise-and (bitwise-not n) #xff)
                                            (bitwise-and (bitwise-arithmetic-shift-right (bitwise-not n) 8) #xff))
                                 (let ((b (make-bytevector n))) (bytevector-copy! raw off b 0 n) b))))
            (loop (+ off n) (cons block acc)))))))

;; --- the image value --------------------------------------------------------
(define-record-type pimg (fields w h data (mutable cur)) (nongenerative jolt-png-img-v1))
(define (png-clamp-byte n)
  (let ((x (cond ((and (number? n) (exact? n) (integer? n)) n)
                 ((number? n) (exact (floor n)))
                 (else 0))))
    (cond ((< x 0) 0) ((> x 255) 255) (else x))))

(define (png-image w h) (make-pimg w h (make-bytevector (* w h 3) 0) 0))
(define (png-put! img r g b)
  (let ((d (pimg-data img)) (c (pimg-cur img)))
    (when (<= (+ c 3) (bytevector-length d))
      (bytevector-u8-set! d c (png-clamp-byte r))
      (bytevector-u8-set! d (+ c 1) (png-clamp-byte g))
      (bytevector-u8-set! d (+ c 2) (png-clamp-byte b))
      (pimg-cur-set! img (+ c 3)))
    jolt-nil))

;; scanlines with a 0 (None) filter byte per row -> raw -> zlib -> IDAT
(define (png-raw img w h)
  (let* ((stride (* w 3)) (raw (make-bytevector (* h (+ 1 stride)))) (src (pimg-data img)))
    (do ((y 0 (+ y 1))) ((= y h) raw)
      (let ((ro (* y (+ 1 stride))))
        (bytevector-u8-set! raw ro 0)                       ; filter: None
        (bytevector-copy! src (* y stride) raw (+ ro 1) stride)))))

(define png-signature (png-bytes #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a))
(define (png-ihdr w h)
  (png-cat (png-u32be w) (png-u32be h)
           (png-bytes 8 2 0 0 0)))   ; bitdepth 8, colortype 2 (RGB), deflate, filter 0, no interlace

(define (png-write img w h path)
  (let* ((idat (png-deflate-stored (png-raw img w h)))
         (bytes (png-cat png-signature
                         (png-chunk "IHDR" (png-ihdr w h))
                         (png-chunk "IDAT" idat)
                         (png-chunk "IEND" (make-bytevector 0))))
         (p (open-file-output-port path (file-options no-fail) (buffer-mode block))))
    (put-bytevector p bytes)
    (close-port p)
    jolt-nil))

(def-var! "jolt.png" "image" png-image)
(def-var! "jolt.png" "put!" png-put!)
(def-var! "jolt.png" "write" png-write)
