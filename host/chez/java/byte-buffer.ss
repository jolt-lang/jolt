;; byte-buffer.ss — java.nio's buffers: ByteBuffer, the typed buffers
;; (CharBuffer views, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer,
;; DoubleBuffer) and ByteOrder. Shared by every target: the bytes live in an
;; R7RS bytevector, the floating-point encodings are exact arithmetic, and what
;; only one host has (byte arrays, foreign memory) is reached through named
;; seams — so host/gambit/boot.ss includes this same file.
;;
;; ONE REPRESENTATION. Every buffer is a jhost whose tag names its element kind
;; ("byte-buffer", "int-buffer", ...) over the state vector below. A buffer reads
;; its elements one of two ways:
;;
;;   - 'bytes: through a STORE of octets — a bytevector, or foreign memory — at a
;;     byte offset, in a byte order. A ByteBuffer is this with width 1; a view
;;     (asIntBuffer and friends) is this with the view's width and the order the
;;     ByteBuffer had when the view was made. A write through either side is a
;;     write to the same octets, which is what makes the view a view.
;;   - 'array: over a host typed array at an element offset — IntBuffer/wrap and
;;     IntBuffer/allocate, whose backing is the int[] .array answers.
;;
;; A heap ByteBuffer's store is the bytevector INSIDE the jolt byte-array it
;; wraps (nb-host-bytes), so (.array b) is that array and a write through either
;; is visible to the other, as on the JVM. A store of foreign memory is
;; jolt.ffi/byte-buffer's direct view of a pointer: the bytes are C's, reached
;; through the sa-foreign-* seam; the buffer does not keep the memory alive.
;;
;; State vector slots:
;;   0 store   bytevector | foreign descriptor | host typed array
;;   1 pos     position
;;   2 limit
;;   3 off     where element 0 is: a byte offset ('bytes) or element offset ('array)
;;   4 cap     capacity, in elements
;;   5 mark    -1 when there is none
;;   6 order   'big | 'little
;;   7 flags   bit 0 read-only, bit 1 direct
;;   8 array   the user-visible byte array of a heap ByteBuffer, else #f
;;   9 kind    'byte 'char 'short 'int 'long 'float 'double
;;  10 mode    'bytes | 'array

;; --- the host seams ----------------------------------------------------------
;; Defined per target, next to the arrays they describe (Chez:
;; java/natives-array.ss; Gambit: rt-core.ss, where no array can be built):
;;   (nb-host-bytes a)           a byte array's octets (a bytevector) or #f
;;   (nb-host-new-bytes n)       a fresh byte array, or #f on a target with none
;;   (nb-host-array? a kind)     is A a host array of element KIND
;;   (nb-host-array-len a) (nb-host-array-ref a i) (nb-host-array-set! a i v)
;;   (nb-host-new-array kind n)  a fresh zeroed typed array
;; and from the scheme-adapter: sa-bytevector-copy-range!, sa-foreign-ref/-set!,
;; sa-foreign-bytes-ref!/-set!, sa-endian.

;; --- state -------------------------------------------------------------------
(define nb-flag-ro 1)
(define nb-flag-direct 2)
(define (nb-tag-of kind)
  (case kind
    ((byte) "byte-buffer") ((char) "nio-char-buffer") ((short) "short-buffer")
    ((int) "int-buffer") ((long) "long-buffer") ((float) "float-buffer")
    (else "double-buffer")))
(define nb-tags '("byte-buffer" "nio-char-buffer" "short-buffer" "int-buffer"
                  "long-buffer" "float-buffer" "double-buffer"))
(define (nb-width kind)
  (case kind ((byte) 1) ((char short) 2) ((int float) 4) (else 8)))

(define (make-nb kind mode store off cap pos limit order flags array)
  (make-jhost (nb-tag-of kind) (vector store pos limit off cap -1 order flags array kind mode)))
;; A ByteBuffer, which wrap/slice/duplicate make often enough to skip the walk.
(define (make-bb store off cap pos limit order flags array)
  (make-jhost "byte-buffer" (vector store pos limit off cap -1 order flags array 'byte 'bytes)))

(define (bb? x) (and (jhost? x) (string=? (jhost-tag x) "byte-buffer")))
(define (nb? x) (and (jhost? x) (member (jhost-tag x) nb-tags) #t))
(define-syntax nb-st (syntax-rules () ((_ b) (jhost-state b))))
(define (bb-backing b) (vector-ref (nb-st b) 0))
(define (bb-pos b) (vector-ref (nb-st b) 1))
(define (bb-limit b) (vector-ref (nb-st b) 2))
(define (bb-off b) (vector-ref (nb-st b) 3))
(define (bb-capacity b) (vector-ref (nb-st b) 4))
(define (bb-pos! b n) (vector-set! (nb-st b) 1 n))
(define (bb-limit! b n) (vector-set! (nb-st b) 2 n))
(define (nb-mark b) (vector-ref (nb-st b) 5))
(define (nb-mark! b n) (vector-set! (nb-st b) 5 n))
(define (nb-order b) (vector-ref (nb-st b) 6))
(define (nb-flags b) (vector-ref (nb-st b) 7))
(define (nb-array b) (vector-ref (nb-st b) 8))
(define (nb-kind b) (vector-ref (nb-st b) 9))
(define (nb-mode b) (vector-ref (nb-st b) 10))
(define (nb-ro? b) (fx=? 1 (fxand (nb-flags b) nb-flag-ro)))
(define (nb-direct? b) (fx=? 2 (fxand (nb-flags b) nb-flag-direct)))
(define (nb-remaining b) (fx- (bb-limit b) (bb-pos b)))

;; --- exceptions --------------------------------------------------------------
;; The JDK's own: these carry no message (getMessage is null) except where the
;; JDK writes one.
(define (nb-throw cls msg) (jolt-throw (jolt-host-throwable cls msg)))
(define (nb-underflow) (nb-throw "java.nio.BufferUnderflowException" jolt-nil))
(define (nb-overflow) (nb-throw "java.nio.BufferOverflowException" jolt-nil))
(define (nb-read-only) (nb-throw "java.nio.ReadOnlyBufferException" jolt-nil))
(define (nb-ioobe) (nb-throw "java.lang.IndexOutOfBoundsException" jolt-nil))
(define (nb-iae msg) (nb-throw "java.lang.IllegalArgumentException" msg))
(define (nb-n x) (number->string x))
;; Objects.checkFromIndexSize, in the JDK's words.
(define (nb-check-range from size len)
  (when (or (fx<? from 0) (fx<? size 0) (fx>? from len) (fx>? size (fx- len from)))
    (nb-throw "java.lang.IndexOutOfBoundsException"
              (string-append "Range [" (nb-n from) ", " (nb-n from) " + " (nb-n size)
                             ") out of bounds for length " (nb-n len)))))
(define (nb-check-writable b) (when (nb-ro? b) (nb-read-only)))
;; An int argument. The common case is already a fixnum.
(define-syntax nb-int
  (syntax-rules () ((_ x) (let ((v x)) (if (fixnum? v) v (jnum->exact v))))))

;; Relative access of N elements: the index to use, with position advanced, or
;; the JDK's under/overflow. Absolute access of N elements at I: the JDK's
;; checkIndex against the LIMIT.
(define (nb-next-get! b n)
  (let ((p (bb-pos b)))
    (if (fx>? n (fx- (bb-limit b) p)) (nb-underflow) (begin (bb-pos! b (fx+ p n)) p))))
(define (nb-next-put! b n)
  (let ((p (bb-pos b)))
    (if (fx>? n (fx- (bb-limit b) p)) (nb-overflow) (begin (bb-pos! b (fx+ p n)) p))))
(define (nb-check-index b i n)
  (if (and (fixnum? i) (fx>=? i 0) (fx<=? n (fx- (bb-limit b) i))) i (nb-ioobe)))

;; --- the store: octets in a bytevector or in foreign memory ------------------
(define (bb-direct-backing addr cap) (vector 'jolt-direct-buffer addr cap))
(define (nb-faddr st) (vector-ref st 1))
(define-syntax nb-u8
  (syntax-rules ()
    ((_ st i) (let ((s st) (k i))
                (if (bytevector? s) (bytevector-u8-ref s k) (sa-foreign-ref 'unsigned-8 (nb-faddr s) k))))))
(define-syntax nb-u8-set!
  (syntax-rules ()
    ((_ st i v) (let ((s st) (k i) (x v))
                  (if (bytevector? s) (bytevector-u8-set! s k x) (sa-foreign-set! 'unsigned-8 (nb-faddr s) k x))))))

;; W octets at P as an unsigned integer, in ORDER. Up to four octets stay in
;; fixnum arithmetic.
(define (nb-load st p w order)
  (if (eq? order 'big)
      (if (fx<=? w 4)
          (let loop ((i 0) (acc 0))
            (if (fx=? i w) acc (loop (fx+ i 1) (fx+ (fx* acc 256) (nb-u8 st (fx+ p i))))))
          (let loop ((i 0) (acc 0))
            (if (fx=? i w) acc (loop (fx+ i 1) (+ (* acc 256) (nb-u8 st (fx+ p i)))))))
      (if (fx<=? w 4)
          (let loop ((i (fx- w 1)) (acc 0))
            (if (fx<? i 0) acc (loop (fx- i 1) (fx+ (fx* acc 256) (nb-u8 st (fx+ p i))))))
          (let loop ((i (fx- w 1)) (acc 0))
            (if (fx<? i 0) acc (loop (fx- i 1) (+ (* acc 256) (nb-u8 st (fx+ p i)))))))))
;; Store the low W octets of the integer U at P, in ORDER.
(define (nb-store! st p w order u)
  (let ((u (if (fx<=? w 4)
               (bitwise-and u (if (fx=? w 4) #xffffffff (if (fx=? w 2) #xffff #xff)))
               (bitwise-and u #xffffffffffffffff))))
    (if (fx<=? w 4)
        (let loop ((i 0) (u u))
          (when (fx<? i w)
            (nb-u8-set! st (if (eq? order 'big) (fx+ p (fx- (fx- w 1) i)) (fx+ p i)) (fxand u #xff))
            (loop (fx+ i 1) (fxarithmetic-shift-right u 8))))
        (let loop ((i 0) (u u))
          (when (fx<? i w)
            (nb-u8-set! st (if (eq? order 'big) (fx+ p (fx- (fx- w 1) i)) (fx+ p i)) (bitwise-and u #xff))
            (loop (fx+ i 1) (bitwise-arithmetic-shift-right u 8)))))))

;; Block moves between stores. Two bytevectors are one block copy (the target's
;; own, overlap-safe); anything touching foreign memory goes through a
;; bytevector and one sa-foreign-bytes move.
(define (nb-store-copy! src soff dst doff n)
  (cond
    ((fx<=? n 0) (if #f #f))
    ((and (bytevector? src) (bytevector? dst))
     (sa-bytevector-copy-range! dst doff src soff (fx+ soff n)))
    (else
     (let ((tmp (if (bytevector? src)
                    (let ((t (make-bytevector n)))
                      (sa-bytevector-copy-range! t 0 src soff (fx+ soff n))
                      t)
                    (let ((t (make-bytevector n)))
                      (sa-foreign-bytes-ref! (fx+ (nb-faddr src) soff) t n)
                      t))))
       (if (bytevector? dst)
           (sa-bytevector-copy-range! dst doff tmp 0 n)
           (sa-foreign-bytes-set! (fx+ (nb-faddr dst) doff) tmp n))))))

;; --- one byte, signed, as a byte[] element is ---------------------------------
(define (nb-s8 u) (if (fx<? u 128) u (fx- u 256)))
(define (bb-byte-ref b i) (nb-s8 (nb-u8 (bb-backing b) (fx+ (bb-off b) i))))

;; Buffer <-> jolt byte-array block moves, by byte index into the buffer — the
;; seam zip-base.ss and charset-coding.ss share with get/put below.
(define (nb-array-octets a)
  (or (nb-host-bytes a)
      (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                       "java.nio.ByteBuffer: the argument is not a byte[]"))))
(define (bb-bulk-ref! b idx dst doff n)
  (nb-store-copy! (bb-backing b) (fx+ (bb-off b) idx) (nb-array-octets dst) doff n))
(define (bb-bulk-set! b idx src soff n)
  (nb-store-copy! (nb-array-octets src) soff (bb-backing b) (fx+ (bb-off b) idx) n))

;; --- IEEE 754 encodings ----------------------------------------------------
;; The bit patterns are math.ss's dbl->bits, bits->dbl, flt->bits, bits->flt and
;; flt-mag->bits (exact arithmetic, the same on every target; math.ss is loaded
;; on both). What
;; is this file's own is how a FLOAT reads back.
;;
;; The float a pattern holds, as jolt holds a float: a double with the float's
;; exact value, which is what (float x) rounds to (converters.ss) and what the
;; JVM's Float widens to. Then (.getFloat b) after (.putFloat b 0.1) equals
;; (float 0.1), both 0.10000000149011612.
(define (nb-bits->flt b) (bits->flt b))

;; --- element kinds -------------------------------------------------------------
;; What a stored unsigned W-octet integer reads as, and what an argument stores.
(define (nb-signed u bits)
  (if (>= u (expt 2 (- bits 1))) (- u (expt 2 bits)) u))
(define (nb-code->char u)
  (if (and (fx>=? u #xd800) (fx<=? u #xdfff))
      (nb-iae (string-append "Value out of range for char: " (nb-n u)))
      (integer->char u)))
(define (nb-char-code x)
  (if (char? x) (char->integer x) (fxand (nb-int x) #xffff)))
(define (nb-decode kind u)
  (case kind
    ((byte) (nb-s8 u))
    ((short) (if (fx<? u #x8000) u (fx- u #x10000)))
    ((int) (if (fx<? u #x80000000) u (fx- u #x100000000)))
    ((long) (nb-signed u 64))
    ((char) (nb-code->char u))
    ((float) (nb-bits->flt u))
    (else (bits->dbl u))))
(define (nb-encode kind v)
  (case kind
    ((char) (nb-char-code v))
    ((float) (flt->bits (jolt-need-num v)))
    ((double) (dbl->bits (jolt-need-num v)))
    (else (nb-int v))))
;; A value as an element of a host typed array of KIND.
(define (nb-array-elem kind v)
  (case kind
    ((char) (if (char? v) v (nb-code->char (nb-char-code v))))
    ((float double) (inexact (jolt-need-num v)))
    ((short) (nb-signed (bitwise-and (nb-int v) #xffff) 16))
    ((int) (nb-signed (bitwise-and (nb-int v) #xffffffff) 32))
    ((byte) (nb-s8 (bitwise-and (nb-int v) #xff)))
    (else (nb-int v))))

;; Element I (0..cap-1) of any buffer.
;; A third mode, 'string, is CharBuffer/wrap over a CharSequence: the JDK's
;; StringCharBuffer, read-only, reading the string itself.
;; A StringCharBuffer reads its CharSequence LIVE, as the JDK's does: a later
;; append or setCharAt on a wrapped StringBuilder shows through the buffer. A
;; String is its own text; anything else is asked for its text at each read
;; (a StringBuilder's is its cached string, so that is no copy per read).
(define (nb-live-char store k)
  (let ((s (if (string? store) store (jolt-str-render-one store))))
    (if (fx<? k (string-length s)) (string-ref s k) (nb-ioobe))))
(define (nb-live-substring store from to)
  (let ((s (if (string? store) store (jolt-str-render-one store))))
    (if (fx<=? to (string-length s)) (substring s from to) (nb-ioobe))))
(define (nb-ref b i)
  (let ((kind (nb-kind b)) (mode (nb-mode b)))
    (cond ((eq? mode 'bytes)
           (let ((w (nb-width kind)))
             (nb-decode kind (nb-load (bb-backing b) (fx+ (bb-off b) (fx* i w)) w (nb-order b)))))
          ((eq? mode 'array) (nb-host-array-ref (bb-backing b) (fx+ (bb-off b) i)))
          (else (nb-live-char (bb-backing b) (fx+ (bb-off b) i))))))
(define (nb-set! b i v)
  (let ((kind (nb-kind b)) (mode (nb-mode b)))
    (cond ((eq? mode 'bytes)
           (let ((w (nb-width kind)))
             (nb-store! (bb-backing b) (fx+ (bb-off b) (fx* i w)) w (nb-order b) (nb-encode kind v))))
          ((eq? mode 'array) (nb-host-array-set! (bb-backing b) (fx+ (bb-off b) i) (nb-array-elem kind v)))
          (else (nb-read-only)))))

;; --- ByteOrder ---------------------------------------------------------------
;; Two interned constants; a buffer holds the symbol, the constant is what
;; .order hands out.
(define (byte-order? x) (and (jhost? x) (string=? (jhost-tag x) "byte-order")))
(define nb-big-endian (make-jhost "byte-order" (vector "BIG_ENDIAN" 'big)))
(define nb-little-endian (make-jhost "byte-order" (vector "LITTLE_ENDIAN" 'little)))
(define (nb-native-order) (if (eq? (sa-endian) 'big) 'big 'little))
(define (nb-order-object sym) (if (eq? sym 'big) nb-big-endian nb-little-endian))
(define (byte-order-name o) (vector-ref (jhost-state o) 0))
(define (nb-order-of x)
  (cond ((byte-order? x) (vector-ref (jhost-state x) 1))
        ;; ByteBuffer.order(null) is little-endian: the JDK tests bo == BIG_ENDIAN
        ((jolt-nil? x) 'little)
        (else (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                               "java.nio.ByteBuffer/order takes a java.nio.ByteOrder")))))
(register-class-statics! "java.nio.ByteOrder"
  (list (cons "BIG_ENDIAN" nb-big-endian)
        (cons "LITTLE_ENDIAN" nb-little-endian)
        (cons "nativeOrder" (lambda () (nb-order-object (nb-native-order))))))
(register-host-methods! "byte-order"
  (list (cons "toString" byte-order-name)))
(register-str-render! byte-order? byte-order-name)
(register-class-arm! byte-order? (lambda (x) "java.nio.ByteOrder"))

;; --- class names -------------------------------------------------------------
;; The JDK's concrete classes, which (class b) reports: HeapByteBuffer[R],
;; DirectByteBuffer[R], HeapIntBuffer[R] over an int[], ByteBufferAsIntBuffer
;; [R]{B,L} for a view of a heap buffer (B/L its order) and DirectIntBuffer[R]
;; {S,U} for a view of a direct one (U when its order is the native one).
(define (nb-kind-name kind)
  (case kind
    ((byte) "Byte") ((char) "Char") ((short) "Short") ((int) "Int")
    ((long) "Long") ((float) "Float") (else "Double")))
(define (nb-class-name b)
  (let ((t (nb-kind-name (nb-kind b))) (r (if (nb-ro? b) "R" "")))
    (cond ((eq? (nb-kind b) 'byte)
           (string-append (if (nb-direct? b) "java.nio.DirectByteBuffer" "java.nio.HeapByteBuffer") r))
          ((eq? (nb-mode b) 'array) (string-append "java.nio.Heap" t "Buffer" r))
          ((eq? (nb-mode b) 'string) "java.nio.StringCharBuffer")
          ((nb-direct? b)
           (string-append "java.nio.Direct" t "Buffer" r (if (eq? (nb-order b) (nb-native-order)) "U" "S")))
          (else (string-append "java.nio.ByteBufferAs" t "Buffer" r (if (eq? (nb-order b) 'big) "B" "L"))))))
(define (nb-abstract-class kind) (string-append "java.nio." (nb-kind-name kind) "Buffer"))

;; The graph: every concrete class under its abstract one, under java.nio.Buffer.
(jch-register-supers! "java.nio.Buffer" '())
(jch-mark-abstract! "java.nio.Buffer")
(jch-register-supers! "java.nio.MappedByteBuffer" '("java.nio.ByteBuffer"))
(jch-mark-abstract! "java.nio.MappedByteBuffer")
(jch-register-supers! "java.nio.HeapByteBuffer" '("java.nio.ByteBuffer"))
(jch-register-supers! "java.nio.HeapByteBufferR" '("java.nio.HeapByteBuffer"))
(jch-register-supers! "java.nio.DirectByteBuffer" '("java.nio.MappedByteBuffer"))
(jch-register-supers! "java.nio.DirectByteBufferR" '("java.nio.DirectByteBuffer"))
(jch-register-supers! "java.nio.ByteOrder" '())
(jch-register-supers! "java.nio.StringCharBuffer" '("java.nio.CharBuffer"))
(jch-register-supers! "java.nio.InvalidMarkException" '("java.lang.IllegalStateException"))
(jch-register-supers! "java.nio.ReadOnlyBufferException" '("java.lang.UnsupportedOperationException"))
(for-each
  (lambda (kind)
    (let* ((t (nb-kind-name kind)) (abs (nb-abstract-class kind)))
      (unless (eq? kind 'char)
        (jch-register-supers! abs '("java.nio.Buffer" "java.lang.Comparable"))
        (jch-mark-abstract! abs))
      (jch-register-supers! (string-append "java.nio.Heap" t "Buffer") (list abs))
      (jch-register-supers! (string-append "java.nio.Heap" t "BufferR")
                            (list (string-append "java.nio.Heap" t "Buffer")))
      (for-each (lambda (sfx)
                  (jch-register-supers! (string-append "java.nio.ByteBufferAs" t "Buffer" sfx) (list abs))
                  (jch-register-supers! (string-append "java.nio.Direct" t "Buffer" sfx) (list abs)))
                '("B" "L" "RB" "RL" "S" "U" "RS" "RU"))))
  '(char short int long float double))

;; --- constructors ----------------------------------------------------------------
;; A heap ByteBuffer over N fresh zero bytes. On a target with no byte arrays
;; the store is a bare bytevector and the buffer has no array to hand out.
(define (nb-allocate-bytes n direct?)
  (let ((n (nb-int n)))
    (when (fx<? n 0)
      (nb-iae (string-append "capacity < 0: (" (nb-n n) " < 0)")))
    (let* ((arr (and (not direct?) (nb-host-new-bytes n)))
           (st (if arr (nb-host-bytes arr) (make-bytevector n 0))))
      (make-bb st 0 n 0 n 'big (if direct? nb-flag-direct 0) arr))))
(define (nb-wrap-bytes arr off len)
  (let* ((st (nb-array-octets arr)) (n (bytevector-length st)))
    (nb-check-range off len n)
    (make-bb st 0 n off (fx+ off len) 'big 0 arr)))
;; What jolt.ffi/byte-buffer answers: a direct buffer over CAP bytes at ADDR.
(define (make-direct-byte-buffer addr cap)
  (make-bb (bb-direct-backing addr cap) 0 cap 0 cap 'big nb-flag-direct #f))

(register-class-statics! "ByteBuffer"
  (list
    (cons "wrap" (case-lambda
                   ((ba) (let ((st (nb-array-octets ba)))
                           (make-bb st 0 (bytevector-length st) 0 (bytevector-length st) 'big 0 ba)))
                   ((ba off len) (nb-wrap-bytes ba (nb-int off) (nb-int len)))))
    (cons "allocate" (lambda (n) (nb-allocate-bytes n #f)))
    ;; Direct: isDirect, no array, and the class the JDK names. The bytes are on
    ;; jolt's heap — nothing a caller can observe tells the two apart.
    (cons "allocateDirect" (lambda (n) (nb-allocate-bytes n #t)))))

;; The typed buffers' own statics: allocate and wrap over a host typed array,
;; in native order, as the JDK's HeapIntBuffer and friends are.
(for-each
  (lambda (kind)
    (let ((cls (nb-abstract-class kind)))
      (define (wrap arr off len)
        (unless (nb-host-array? arr kind)
          (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                           (string-append cls "/wrap takes a " (symbol->string kind) "[]"))))
        (let ((n (nb-host-array-len arr)))
          (nb-check-range off len n)
          (make-nb kind 'array arr 0 n off (fx+ off len) (nb-native-order) 0 arr)))
      (register-class-statics! cls
        (list
          (cons "allocate"
                (lambda (n)
                  (let ((n (nb-int n)))
                    (when (fx<? n 0) (nb-iae (string-append "capacity < 0: (" (nb-n n) " < 0)")))
                    (let ((arr (nb-host-new-array kind n)))
                      (make-nb kind 'array arr 0 n 0 n (nb-native-order) 0 arr)))))
          (cons "wrap" (case-lambda
                         ((arr) (wrap arr 0 (if (nb-host-array? arr kind) (nb-host-array-len arr) 0)))
                         ((arr off len) (wrap arr (nb-int off) (nb-int len)))))))))
  '(short int long float double))

;; CharBuffer: allocate and wrap(char[]) over a char array, sharing it, as the
;; JDK's HeapCharBuffer; wrap(CharSequence) over the text, read-only, as its
;; StringCharBuffer. Both in native order, which is the JDK's for either.
(define (nb-char-allocate n)
  (let ((n (nb-int n)))
    (when (fx<? n 0) (nb-iae (string-append "capacity < 0: (" (nb-n n) " < 0)")))
    (let ((arr (nb-host-new-array 'char n)))
      (make-nb 'char 'array arr 0 n 0 n (nb-native-order) 0 arr))))
(define (nb-char-wrap x start end)
  (if (nb-host-array? x 'char)
      (let ((n (nb-host-array-len x)))
        (nb-check-range start (fx- end start) n)
        (make-nb 'char 'array x 0 n start end (nb-native-order) 0 x))
      (let ((n (nb-char-seq-len x)))
        (when (or (fx<? start 0) (fx>? start n) (fx<? end start) (fx>? end n)) (nb-ioobe))
        (make-nb 'char 'string x 0 n start end (nb-native-order) nb-flag-ro #f))))
(define (nb-char-seq-len x)
  (cond ((nb-host-array? x 'char) (nb-host-array-len x))
        ((string? x) (string-length x))
        (else (string-length (jolt-str-render-one x)))))
(register-class-statics! "java.nio.CharBuffer"
  (list
    (cons "allocate" nb-char-allocate)
    (cons "wrap" (case-lambda
                   ((x) (nb-char-wrap x 0 (nb-char-seq-len x)))
                   ;; (char[] off len), but (CharSequence start end)
                   ((x a b) (let ((a (nb-int a)) (b (nb-int b)))
                              (if (nb-host-array? x 'char)
                                  (nb-char-wrap x a (fx+ a b))
                                  (nb-char-wrap x a b))))))))

;; --- derived buffers -----------------------------------------------------------
;; slice/duplicate/asReadOnlyBuffer of a ByteBuffer start BIG_ENDIAN, as the
;; JDK's do; of a typed buffer they keep its order, which the view fixed. A
;; slice has no mark; a duplicate keeps it.
(define (nb-derived-order b) (if (eq? (nb-kind b) 'byte) 'big (nb-order b)))
(define (nb-elem-off b i)                 ; where element I of B starts in its store
  (if (eq? (nb-mode b) 'bytes)
      (fx+ (bb-off b) (fx* i (nb-width (nb-kind b))))
      (fx+ (bb-off b) i)))
(define (nb-slice b index len)
  (make-nb (nb-kind b) (nb-mode b) (bb-backing b) (nb-elem-off b index) len 0 len
           (nb-derived-order b) (nb-flags b) (nb-array b)))
(define (nb-duplicate b ro?)
  (let ((d (make-nb (nb-kind b) (nb-mode b) (bb-backing b) (bb-off b) (bb-capacity b)
                    (bb-pos b) (bb-limit b) (nb-derived-order b)
                    (if ro? (fxior (nb-flags b) nb-flag-ro) (nb-flags b)) (nb-array b))))
    (nb-mark! d (nb-mark b))
    d))
;; asXBuffer: a view over this buffer's REMAINING bytes, in its current order.
(define (nb-view b kind)
  (let* ((w (nb-width kind)) (p (bb-pos b)) (n (fxquotient (fx- (bb-limit b) p) w)))
    (make-nb kind 'bytes (bb-backing b) (fx+ (bb-off b) p) n 0 n (nb-order b) (nb-flags b) #f)))

;; --- the Buffer index protocol -------------------------------------------------
(define (nb-set-position! b p)
  (let ((p (nb-int p)))
    (cond ((fx>? p (bb-limit b))
           (nb-iae (string-append "newPosition > limit: (" (nb-n p) " > " (nb-n (bb-limit b)) ")")))
          ((fx<? p 0) (nb-iae (string-append "newPosition < 0: (" (nb-n p) " < 0)"))))
    (when (fx>? (nb-mark b) p) (nb-mark! b -1))
    (bb-pos! b p)
    b))
(define (nb-set-limit! b l)
  (let ((l (nb-int l)))
    (cond ((fx>? l (bb-capacity b))
           (nb-iae (string-append "newLimit > capacity: (" (nb-n l) " > " (nb-n (bb-capacity b)) ")")))
          ((fx<? l 0) (nb-iae (string-append "newLimit < 0: (" (nb-n l) " < 0)"))))
    (bb-limit! b l)
    (when (fx>? (bb-pos b) l) (bb-pos! b l))
    (when (fx>? (nb-mark b) l) (nb-mark! b -1))
    b))
(define (nb-compact! b)
  (nb-check-writable b)
  (let* ((p (bb-pos b)) (n (fx- (bb-limit b) p)))
    (if (eq? (nb-mode b) 'array)
        (do ((i 0 (fx+ i 1))) ((fx=? i n)) (nb-set! b i (nb-ref b (fx+ p i))))
        (let ((w (nb-width (nb-kind b))))
          (nb-store-copy! (bb-backing b) (fx+ (bb-off b) (fx* p w))
                          (bb-backing b) (bb-off b) (fx* n w))))
    (bb-pos! b n)
    (bb-limit! b (bb-capacity b))
    (nb-mark! b -1)
    b))

;; --- equals / hashCode / compareTo / mismatch ------------------------------------
;; The JDK's: over the REMAINING elements, position to limit.
(define (nb-elem-equal? kind x y)
  (case kind
    ((char) (char=? x y))
    ;; the JDK's float equality: == (so 0.0 equals -0.0), and NaN equals NaN
    ((float double) (or (= x y) (and (nan? x) (nan? y))))
    (else (= x y))))
(define (nb-elem-compare kind x y)
  (case kind
    ((byte short) (- x y))
    ((char) (fx- (char->integer x) (char->integer y)))
    ((int long) (cond ((< x y) -1) ((> x y) 1) (else 0)))
    (else (cond ((< x y) -1) ((> x y) 1) ((= x y) 0)
                ((nan? x) (if (nan? y) 0 1))
                (else -1)))))
;; index of the first differing element in the first N remaining, or -1
(define (nb-mismatch-n a b n)
  (let ((kind (nb-kind a)) (pa (bb-pos a)) (pb (bb-pos b)))
    (let loop ((i 0))
      (cond ((fx=? i n) -1)
            ((nb-elem-equal? kind (nb-ref a (fx+ pa i)) (nb-ref b (fx+ pb i))) (loop (fx+ i 1)))
            (else i)))))
(define (nb-same-kind? a b) (and (nb? b) (eq? (nb-kind a) (nb-kind b))))
(define (nb-equals? a b)
  (or (eq? a b)
      (and (nb-same-kind? a b)
           (fx=? (nb-remaining a) (nb-remaining b))
           (fx<? (nb-mismatch-n a b (nb-remaining a)) 0))))
(define (nb-compare a b)
  (unless (nb-same-kind? a b)
    (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                     (string-append "class " (nb-class-name b) " cannot be cast to class "
                                                    (nb-abstract-class (nb-kind a))))))
  (let* ((ra (nb-remaining a)) (rb (nb-remaining b))
         (i (nb-mismatch-n a b (fxmin ra rb))))
    (if (fx>=? i 0)
        (nb-elem-compare (nb-kind a) (nb-ref a (fx+ (bb-pos a) i)) (nb-ref b (fx+ (bb-pos b) i)))
        (fx- ra rb))))
(define (nb-mismatch a b)
  (unless (nb-same-kind? a b)
    (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                     (string-append (nb-abstract-class (nb-kind a)) "/mismatch: argument kind"))))
  (let* ((ra (nb-remaining a)) (rb (nb-remaining b)) (n (fxmin ra rb))
         (r (nb-mismatch-n a b n)))
    (if (and (fx=? r -1) (not (fx=? ra rb))) n r)))
;; Java int arithmetic: the low 32 bits, signed.
(define (nb-int32 x) (nb-signed (bitwise-and x #xffffffff) 32))
;; (int) of an element: a char's code unit, a long's low 32 bits, a float's
;; d2i (NaN 0, saturating, toward zero).
(define (nb-elem->int kind x)
  (case kind
    ((char) (char->integer x))
    ((long) (nb-int32 x))
    ((float double) (cond ((nan? x) 0)
                          ((>= x 2147483647.0) 2147483647)
                          ((<= x -2147483648.0) -2147483648)
                          (else (exact (truncate x)))))
    (else x)))
(define (nb-hash b)
  (let ((kind (nb-kind b)) (p (bb-pos b)))
    (let loop ((i (fx- (bb-limit b) 1)) (h 1))
      (if (fx<? i p)
          h
          (loop (fx- i 1) (nb-int32 (+ (* 31 h) (nb-elem->int kind (nb-ref b i)))))))))

;; --- toString ----------------------------------------------------------------
;; A CharBuffer is its remaining characters; every other buffer names its class
;; and its three indexes.
(define (nb-char-string b from to)        ; elements [from, to) as a string
  (case (nb-mode b)
    ((string) (nb-live-substring (bb-backing b) (fx+ (bb-off b) from) (fx+ (bb-off b) to)))
    ((array) (nb-host-chars->string (bb-backing b) (fx+ (bb-off b) from) (fx+ (bb-off b) to)))
    (else (let ((s (make-string (fx- to from))))
            (do ((i from (fx+ i 1))) ((fx=? i to) s) (string-set! s (fx- i from) (nb-ref b i)))))))
(define (nb-render b)
  (if (eq? (nb-kind b) 'char)
      (nb-char-string b (bb-pos b) (bb-limit b))
      (string-append (nb-class-name b) "[pos=" (nb-n (bb-pos b)) " lim=" (nb-n (bb-limit b))
                     " cap=" (nb-n (bb-capacity b)) "]")))

;; --- bulk transfers ---------------------------------------------------------
;; Between a buffer and a host array of its kind. A ByteBuffer moves octets in
;; one block; anything else moves element by element.
(define (nb-check-array b arr)
  (unless (nb-host-array? arr (nb-kind b))
    (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                     (string-append (nb-abstract-class (nb-kind b)) ": expected a "
                                                    (symbol->string (nb-kind b)) "[]")))))
(define (nb-array-len arr) (nb-host-array-len arr))
;; elements [idx, idx+n) of B -> ARR at off
(define (nb-get-into! b idx arr off n)
  (if (eq? (nb-kind b) 'byte)
      (nb-store-copy! (bb-backing b) (fx+ (bb-off b) idx) (nb-host-bytes arr) off n)
      (do ((i 0 (fx+ i 1))) ((fx=? i n)) (nb-host-array-set! arr (fx+ off i) (nb-ref b (fx+ idx i))))))
(define (nb-put-from! b idx arr off n)
  (if (eq? (nb-kind b) 'byte)
      (nb-store-copy! (nb-host-bytes arr) off (bb-backing b) (fx+ (bb-off b) idx) n)
      (do ((i 0 (fx+ i 1))) ((fx=? i n)) (nb-set! b (fx+ idx i) (nb-host-array-ref arr (fx+ off i))))))
;; elements [sidx, sidx+n) of SRC -> DST at didx. Two byte stores are a block
;; move, which is overlap-safe; any other pair goes through a snapshot, so a
;; buffer put into a view of itself copies as if from a temporary.
(define (nb-copy-between! src sidx dst didx n)
  (if (and (eq? (nb-mode src) 'bytes) (eq? (nb-mode dst) 'bytes)
           (eq? (nb-kind src) (nb-kind dst)) (eq? (nb-order src) (nb-order dst)))
      (let ((w (nb-width (nb-kind src))))
        (nb-store-copy! (bb-backing src) (fx+ (bb-off src) (fx* sidx w))
                        (bb-backing dst) (fx+ (bb-off dst) (fx* didx w)) (fx* n w)))
      (let ((tmp (make-vector n)))
        (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! tmp i (nb-ref src (fx+ sidx i))))
        (do ((i 0 (fx+ i 1))) ((fx=? i n)) (nb-set! dst (fx+ didx i) (vector-ref tmp i))))))
;; put(Buffer src): the source's remaining elements, both advancing
(define (nb-put-buffer! b src)
  (when (eq? src b) (nb-iae "The source buffer is this buffer"))
  (nb-check-writable b)
  (unless (nb-same-kind? b src)
    (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                                     (string-append (nb-abstract-class (nb-kind b)) "/put: argument kind"))))
  (let ((n (nb-remaining src)))
    (when (fx>? n (nb-remaining b)) (nb-overflow))
    (nb-copy-between! src (bb-pos src) b (bb-pos b) n)
    (bb-pos! src (fx+ (bb-pos src) n))
    (bb-pos! b (fx+ (bb-pos b) n))
    b))

;; get, every overload: () | (index) | (T[] dst) | (T[] dst off len) |
;; (index T[] dst) | (index T[] dst off len)
(define (nb-get b . args)
  (cond
    ((null? args)
     (nb-ref b (nb-next-get! b 1)))
    ((number? (car args))
     (let ((i (nb-int (car args))))
       (if (null? (cdr args))
           (nb-ref b (nb-check-index b i 1))
           (let* ((arr (cadr args)) (_ (nb-check-array b arr))
                  (off (if (pair? (cddr args)) (nb-int (caddr args)) 0))
                  (len (if (pair? (cddr args)) (nb-int (cadddr args)) (nb-array-len arr))))
             (nb-check-range i len (bb-limit b))
             (nb-check-range off len (nb-array-len arr))
             (nb-get-into! b i arr off len)
             b))))
    (else
     (let* ((arr (car args)) (_ (nb-check-array b arr))
            (off (if (pair? (cdr args)) (nb-int (cadr args)) 0))
            (len (if (pair? (cdr args)) (nb-int (caddr args)) (nb-array-len arr))))
       (nb-check-range off len (nb-array-len arr))
       (when (fx>? len (nb-remaining b)) (nb-underflow))
       (nb-get-into! b (bb-pos b) arr off len)
       (bb-pos! b (fx+ (bb-pos b) len))
       b))))

;; put, every overload: (x) | (Buffer src) | (T[] src) | (T[] src off len) |
;; (index x) | (index T[] src) | (index T[] src off len) |
;; (index Buffer src off len), and on a CharBuffer (String) / (String start end).
(define (nb-elem-arg? b x)
  (if (eq? (nb-kind b) 'char) (or (char? x) (number? x)) (number? x)))
(define (nb-put-string! b s start end)
  (nb-check-writable b)
  (let ((n (fx- end start)))
    (when (fx>? n (nb-remaining b)) (nb-overflow))
    (do ((i 0 (fx+ i 1))) ((fx=? i n)) (nb-set! b (fx+ (bb-pos b) i) (string-ref s (fx+ start i))))
    (bb-pos! b (fx+ (bb-pos b) n))
    b))
(define (nb-put b x . rest)
  (cond
    ((and (null? rest) (nb-elem-arg? b x))
     (nb-check-writable b)
     (nb-set! b (nb-next-put! b 1) x)
     b)
    ((and (pair? rest) (number? x))
     (let ((i (nb-int x)) (y (car rest)))
       (nb-check-writable b)
       (cond
         ((null? (cdr rest))
          (if (nb-elem-arg? b y)
              (begin (nb-set! b (nb-check-index b i 1) y) b)
              (begin (nb-check-array b y)
                     (let ((len (nb-array-len y)))
                       (nb-check-range i len (bb-limit b))
                       (nb-put-from! b i y 0 len)
                       b))))
         ((nb? y)                          ; (index src off len)
          (let ((off (nb-int (cadr rest))) (len (nb-int (caddr rest))))
            (nb-check-range i len (bb-limit b))
            (nb-check-range off len (bb-limit y))
            (unless (nb-same-kind? b y) (nb-check-array b y))
            (nb-copy-between! y off b i len)
            b))
         (else                             ; (index T[] src off len)
          (nb-check-array b y)
          (let ((off (nb-int (cadr rest))) (len (nb-int (caddr rest))))
            (nb-check-range i len (bb-limit b))
            (nb-check-range off len (nb-array-len y))
            (nb-put-from! b i y off len)
            b)))))
    ((nb? x) (nb-put-buffer! b x))
    ((and (string? x) (eq? (nb-kind b) 'char))
     (if (pair? rest)
         (let ((start (nb-int (car rest))) (end (nb-int (cadr rest))))
           (nb-check-range start (fx- end start) (string-length x))
           (nb-put-string! b x start end))
         (nb-put-string! b x 0 (string-length x))))
    (else
     (nb-check-writable b)
     (nb-check-array b x)
     (let ((off (if (pair? rest) (nb-int (car rest)) 0))
           (len (if (pair? rest) (nb-int (cadr rest)) (nb-array-len x))))
       (nb-check-range off len (nb-array-len x))
       (when (fx>? len (nb-remaining b)) (nb-overflow))
       (nb-put-from! b (bb-pos b) x off len)
       (bb-pos! b (fx+ (bb-pos b) len))
       b))))

;; --- the method table every buffer shares ------------------------------------
(define nb-common-methods
  (list
    (cons "position" (case-lambda ((b) (bb-pos b)) ((b p) (nb-set-position! b p))))
    (cons "limit" (case-lambda ((b) (bb-limit b)) ((b l) (nb-set-limit! b l))))
    (cons "capacity" bb-capacity)
    (cons "remaining" nb-remaining)
    (cons "hasRemaining" (lambda (b) (fx>? (bb-limit b) (bb-pos b))))
    (cons "mark" (lambda (b) (nb-mark! b (bb-pos b)) b))
    (cons "reset" (lambda (b)
                    (let ((m (nb-mark b)))
                      (when (fx<? m 0) (nb-throw "java.nio.InvalidMarkException" jolt-nil))
                      (bb-pos! b m)
                      b)))
    (cons "clear" (lambda (b) (bb-pos! b 0) (bb-limit! b (bb-capacity b)) (nb-mark! b -1) b))
    (cons "flip" (lambda (b) (bb-limit! b (bb-pos b)) (bb-pos! b 0) (nb-mark! b -1) b))
    (cons "rewind" (lambda (b) (bb-pos! b 0) (nb-mark! b -1) b))
    (cons "isReadOnly" nb-ro?)
    (cons "isDirect" nb-direct?)
    (cons "hasArray" (lambda (b) (and (nb-array b) (not (nb-ro? b)) #t)))
    ;; no backing array is asked before read-only, as the JDK tests hb first
    (cons "array" (lambda (b)
                    (cond ((not (nb-array b)) (nb-throw "java.lang.UnsupportedOperationException" jolt-nil))
                          ((nb-ro? b) (nb-read-only))
                          (else (nb-array b)))))
    (cons "arrayOffset" (lambda (b)
                          (cond ((not (nb-array b)) (nb-throw "java.lang.UnsupportedOperationException" jolt-nil))
                                ((nb-ro? b) (nb-read-only))
                                (else (bb-off b)))))
    (cons "order" (lambda (b) (nb-order-object (nb-order b))))
    (cons "slice" (case-lambda
                    ((b) (nb-slice b (bb-pos b) (nb-remaining b)))
                    ((b index len)
                     (let ((index (nb-int index)) (len (nb-int len)))
                       (nb-check-range index len (bb-limit b))
                       (nb-slice b index len)))))
    (cons "duplicate" (lambda (b) (nb-duplicate b #f)))
    (cons "asReadOnlyBuffer" (lambda (b) (nb-duplicate b #t)))
    (cons "compact" nb-compact!)
    (cons "equals" nb-equals?)
    (cons "hashCode" nb-hash)
    (cons "compareTo" nb-compare)
    (cons "mismatch" nb-mismatch)
    (cons "toString" nb-render)
    (cons "get" (case-lambda
                  ((b) (nb-get b))
                  ((b x) (nb-get b x))
                  ((b x y) (nb-get b x y))
                  ((b x y z) (nb-get b x y z))
                  ((b x y z w) (nb-get b x y z w))))
    (cons "put" (case-lambda
                  ((b x) (nb-put b x))
                  ((b x y) (nb-put b x y))
                  ((b x y z) (nb-put b x y z))
                  ((b x y z w) (nb-put b x y z w))))))

;; --- ByteBuffer's own ----------------------------------------------------------
;; The single-byte get/put are the hot path a codec loop runs, so they read the
;; store directly rather than through nb-get / nb-put's overload walk.
(define (bb-get1 b)
  (let ((p (bb-pos b)))
    (if (fx<? p (bb-limit b))
        (begin (bb-pos! b (fx+ p 1)) (nb-s8 (nb-u8 (bb-backing b) (fx+ (bb-off b) p))))
        (nb-underflow))))
(define (bb-get-at b i)
  (if (number? i)
      (let ((i (nb-int i)))
        (if (and (fx>=? i 0) (fx<? i (bb-limit b)))
            (nb-s8 (nb-u8 (bb-backing b) (fx+ (bb-off b) i)))
            (nb-ioobe)))
      (let ((oct (nb-host-bytes i)))
        (if oct (bb-get-bulk b i 0 (bytevector-length oct)) (nb-get b i)))))
(define (bb-put1 b x)
  (if (number? x)
      (let ((p (bb-pos b)))
        (nb-check-writable b)
        (if (fx<? p (bb-limit b))
            (begin (nb-u8-set! (bb-backing b) (fx+ (bb-off b) p) (fxand (nb-int x) #xff))
                   (bb-pos! b (fx+ p 1))
                   b)
            (nb-overflow)))
      (let ((oct (nb-host-bytes x)))
        (if oct (bb-put-bulk b x 0 (bytevector-length oct)) (nb-put b x)))))

;; put(index, byte): absolute. Anything else with two arguments
;; (index, byte[]) takes the general walk.
(define (bb-put-at b i x)
  (if (and (number? i) (number? x))
      (let ((i (nb-int i)))
        (nb-check-writable b)
        (if (and (fx>=? i 0) (fx<? i (bb-limit b)))
            (begin (nb-u8-set! (bb-backing b) (fx+ (bb-off b) i) (fxand (nb-int x) #xff)) b)
            (nb-ioobe)))
      (nb-put b i x)))
;; get(byte[]) / put(byte[]) and their (off, len) forms: one block move between
;; the array's octets and the store, which is what a codec loop moves data with.
(define (bb-get-bulk b arr off len)
  (let ((oct (nb-host-bytes arr)))
    (if oct
        (let ((off (nb-int off)) (len (nb-int len)) (p (bb-pos b)))
          (nb-check-range off len (bytevector-length oct))
          (when (fx>? len (fx- (bb-limit b) p)) (nb-underflow))
          (nb-store-copy! (bb-backing b) (fx+ (bb-off b) p) oct off len)
          (bb-pos! b (fx+ p len))
          b)
        (nb-get b arr off len))))
(define (bb-put-bulk b arr off len)
  (let ((oct (nb-host-bytes arr)))
    (if (and oct (not (nb-ro? b)))
        (let ((off (nb-int off)) (len (nb-int len)) (p (bb-pos b)))
          (nb-check-range off len (bytevector-length oct))
          (when (fx>? len (fx- (bb-limit b) p)) (nb-overflow))
          (nb-store-copy! oct off (bb-backing b) (fx+ (bb-off b) p) len)
          (bb-pos! b (fx+ p len))
          b)
        (nb-put b arr off len))))

;; getX / putX for one width: relative (from position, advancing it) and
;; absolute (at a byte index, position unchanged), in the buffer's order.
(define (bb-num-accessors nm kind)
  (let ((w (nb-width kind)))
    (list
      (cons (string-append "get" nm)
            (case-lambda
              ((b) (let ((p (nb-next-get! b w)))
                     (nb-decode kind (nb-load (bb-backing b) (fx+ (bb-off b) p) w (nb-order b)))))
              ((b i) (let ((i (nb-check-index b (nb-int i) w)))
                       (nb-decode kind (nb-load (bb-backing b) (fx+ (bb-off b) i) w (nb-order b)))))))
      (cons (string-append "put" nm)
            (case-lambda
              ((b v) (nb-check-writable b)
                     (let ((u (nb-encode kind v)) (p (nb-next-put! b w)))
                       (nb-store! (bb-backing b) (fx+ (bb-off b) p) w (nb-order b) u))
                     b)
              ((b i v) (nb-check-writable b)
                       (let ((u (nb-encode kind v)) (i (nb-check-index b (nb-int i) w)))
                         (nb-store! (bb-backing b) (fx+ (bb-off b) i) w (nb-order b) u))
                       b))))))

(register-host-methods! "byte-buffer"
  (append
    nb-common-methods
    (list
      (cons "get" (case-lambda
                    ((b) (bb-get1 b))
                    ((b x) (bb-get-at b x))
                    ((b x y) (nb-get b x y))
                    ((b x y z) (bb-get-bulk b x y z))
                    ((b x y z w) (nb-get b x y z w))))
      (cons "put" (case-lambda
                    ((b x) (bb-put1 b x))
                    ((b x y) (bb-put-at b x y))
                    ((b x y z) (bb-put-bulk b x y z))
                    ((b x y z w) (nb-put b x y z w))))
      (cons "slice" (case-lambda
                      ((b) (make-bb (bb-backing b) (fx+ (bb-off b) (bb-pos b)) (nb-remaining b)
                                    0 (nb-remaining b) 'big (nb-flags b) (nb-array b)))
                      ((b index len)
                       (let ((index (nb-int index)) (len (nb-int len)))
                         (nb-check-range index len (bb-limit b))
                         (make-bb (bb-backing b) (fx+ (bb-off b) index) len 0 len 'big (nb-flags b) (nb-array b))))))
      (cons "duplicate" (lambda (b)
                          (let ((d (make-bb (bb-backing b) (bb-off b) (bb-capacity b) (bb-pos b) (bb-limit b)
                                            'big (nb-flags b) (nb-array b))))
                            (nb-mark! d (nb-mark b))
                            d)))
      (cons "order" (case-lambda
                      ((b) (nb-order-object (nb-order b)))
                      ((b bo) (vector-set! (nb-st b) 6 (nb-order-of bo)) b)))
      (cons "asCharBuffer" (lambda (b) (nb-view b 'char)))
      (cons "asShortBuffer" (lambda (b) (nb-view b 'short)))
      (cons "asIntBuffer" (lambda (b) (nb-view b 'int)))
      (cons "asLongBuffer" (lambda (b) (nb-view b 'long)))
      (cons "asFloatBuffer" (lambda (b) (nb-view b 'float)))
      (cons "asDoubleBuffer" (lambda (b) (nb-view b 'double))))
    (bb-num-accessors "Char" 'char)
    (bb-num-accessors "Short" 'short)
    (bb-num-accessors "Int" 'int)
    (bb-num-accessors "Long" 'long)
    (bb-num-accessors "Float" 'float)
    (bb-num-accessors "Double" 'double)))

(for-each (lambda (kind) (register-host-methods! (nb-tag-of kind) nb-common-methods))
          '(short int long float double))

;; --- CharBuffer views ----------------------------------------------------------
;; One character at element I, the single-char get/put a decode loop or a
;; reader runs per character, so a char-array buffer goes straight to the array
;; (nb-host-char-ref / -set!) instead of through nb-ref's kind walk.
(define (nb-char-ref b i)
  (case (nb-mode b)
    ((array) (nb-host-char-ref (bb-backing b) (fx+ (bb-off b) i)))
    ((string) (nb-live-char (bb-backing b) (fx+ (bb-off b) i)))
    (else (nb-ref b i))))
(define (nb-char-set! b i c)
  (if (and (eq? (nb-mode b) 'array) (char? c))
      (nb-host-char-set! (bb-backing b) (fx+ (bb-off b) i) c)
      (nb-set! b i c)))
(define (nb-char-get1 b)
  (let* ((st (nb-st b)) (p (vector-ref st 1)))
    (if (fx<? p (vector-ref st 2))
        (let ((store (vector-ref st 0)) (i (fx+ (vector-ref st 3) p)))
          (vector-set! st 1 (fx+ p 1))
          (case (vector-ref st 10)
            ((array) (nb-host-char-ref store i))
            ((string) (nb-live-char store i))
            (else (nb-ref b p))))
        (nb-underflow))))
(define (nb-char-put1 b x)
  (let* ((st (nb-st b)) (p (vector-ref st 1)))
    (cond ((not (char? x)) (nb-put b x))
          ((fx=? 1 (fxand (vector-ref st 7) nb-flag-ro)) (nb-read-only))
          ((fx<? p (vector-ref st 2))
           (if (eq? (vector-ref st 10) 'array)
               (nb-host-char-set! (vector-ref st 0) (fx+ (vector-ref st 3) p) x)
               (nb-set! b p x))
           (vector-set! st 1 (fx+ p 1))
           b)
          (else (nb-overflow)))))
;; A writer for a loop that appends many characters to one CharBuffer (the
;; decode loop): (put c) answers #f when the buffer is full. Over a char array
;; whose backing is a string it resolves that string once, so a character is a
;; string-set! and a position bump, as the string-backed CharBuffer was. The
;; array cannot change representation mid-loop: only a non-char store does that.
(define (nb-char-appender b)
  (let* ((st (nb-st b))
         (raw (and (eq? (vector-ref st 10) 'array) (nb-host-char-string (vector-ref st 0))))
         (off (vector-ref st 3)))
    (cond
      ((nb-ro? b)                         ; full is still OVERFLOW; a write raises
       (lambda (c) (and (fx<? (vector-ref st 1) (vector-ref st 2)) (nb-read-only))))
      (raw
        (lambda (c)
          (let ((p (vector-ref st 1)))
            (and (fx<? p (vector-ref st 2))
                 (begin (string-set! raw (fx+ off p) c) (vector-set! st 1 (fx+ p 1)) #t)))))
      (else
       (lambda (c)
         (let ((p (vector-ref st 1)))
           (and (fx<? p (vector-ref st 2))
                (begin (nb-char-set! b p c) (vector-set! st 1 (fx+ p 1)) #t))))))))

;; A CharSequence: length/charAt/subSequence over the REMAINING characters, and
;; toString is them.
(register-host-methods! "nio-char-buffer"
  (append
    nb-common-methods
    (list
      (cons "length" nb-remaining)
      (cons "isEmpty" (lambda (b) (fx=? 0 (nb-remaining b))))
      (cons "charAt" (lambda (b i)
                       (let ((i (nb-int i)))
                         (if (and (fx>=? i 0) (fx<? i (nb-remaining b)))
                             (nb-char-ref b (fx+ (bb-pos b) i))
                             (nb-ioobe)))))
      (cons "subSequence" (lambda (b start end)
                            (let ((start (nb-int start)) (end (nb-int end)) (n (nb-remaining b)))
                              (when (or (fx<? start 0) (fx>? end n) (fx>? start end)) (nb-ioobe))
                              (let ((s (nb-slice b (bb-pos b) n)))
                                (bb-pos! s start)
                                (bb-limit! s end)
                                s))))
      (cons "append" (lambda (b x)
                       (if (char? x)
                           (nb-put b x)
                           (let ((s (jolt-str-render-one x))) (nb-put-string! b s 0 (string-length s))))))
      (cons "get" (case-lambda
                    ((b) (nb-char-get1 b))
                    ((b x) (nb-get b x))
                    ((b x y) (nb-get b x y))
                    ((b x y z) (nb-get b x y z))
                    ((b x y z w) (nb-get b x y z w))))
      (cons "put" (case-lambda
                    ((b x) (nb-char-put1 b x))
                    ((b x y) (nb-put b x y))
                    ((b x y z) (nb-put b x y z))
                    ((b x y z w) (nb-put b x y z w)))))))

;; --- the value-model arms ------------------------------------------------------
(register-class-arm! nb? nb-class-name)
(register-str-render! nb? nb-render)
(register-eq-arm! (lambda (a b) (or (nb? a) (nb? b)))
                  (lambda (a b) (and (nb? a) (nb-equals? a b))))
(register-hash-arm! nb? nb-hash)
(register-compare-arm! (lambda (a b) (and (nb? a) (nb? b))) nb-compare)
(register-instance-check-arm!
  (lambda (type-sym val)
    (cond ((nb? val)
           (if (symbol-t? type-sym) (jch-isa? (nb-class-name val) (symbol-t-name type-sym)) 'pass))
          ((byte-order? val)
           (if (symbol-t? type-sym) (jch-isa? "java.nio.ByteOrder" (symbol-t-name type-sym)) 'pass))
          (else 'pass))))
