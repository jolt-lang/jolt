;; natives-array.ss (jolt-cf1q.7) — Java-style mutable arrays for the Chez host.
;;
;; A jolt-array wraps a Chez mutable vector + a `kind` tag (for bytes?). The array
;; CONSTRUCTORS are native (they build the backing); the overlay's aget/aset/alength
;; are pure over count / nth / jolt.host/ref-put!, so we extend those dispatchers
;; to see a jolt-array (backed by a Chez vector). Loaded after host-table.ss (ref-put!),
;; transients.ss, seq.ss (the dispatchers it chains).

(define-record-type jolt-array (fields (mutable vec) kind) (nongenerative jolt-array-v1))

;; JVM array class name per element kind ((class (int-array 3)) -> "[I", like the
;; JVM's Class.getName for arrays). Object arrays use the descriptor form.
(define (na-array-class-name arr)
  (case (jolt-array-kind arr)
    ((int) "[I") ((long) "[J") ((short) "[S") ((double) "[D")
    ((float) "[F") ((boolean) "[Z") ((byte) "[B") ((char) "[C")
    (else "[Ljava.lang.Object;")))

(define (na-idx i) (if (and (number? i) (not (exact? i))) (exact (floor i)) i))
(define (na-from-seq x kind) (make-jolt-array (list->vector (seq->list (jolt-seq x))) kind))
;; (T-array size) | (T-array size init) | (T-array seq)
(define (na-num-array a rest init kind)
  (if (number? a)
      (make-jolt-array (make-vector (exact (na-idx a)) (if (pair? rest) (car rest) init)) kind)
      (na-from-seq a kind)))

;; numeric tower (jolt-n6al): array element defaults / masked bytes / count are
;; EXACT integers (= JVM byte/short/int), matching exact integer literals.
(define (na-byte-of v) (bitwise-and (exact (floor v)) #xff))

;; --- constructors -----------------------------------------------------------
(define (na-object-array a . rest)  (na-num-array a rest jolt-nil 'object))
;; integer kinds default to exact 0 (JVM int/long/short 0 -> "0", not "0.0").
(define (na-int-array a . rest)     (na-num-array a rest 0 'int))
(define (na-long-array a . rest)    (na-num-array a rest 0 'long))
(define (na-short-array a . rest)   (na-num-array a rest 0 'short))
(define (na-double-array a . rest)  (na-num-array a rest 0.0 'double))
(define (na-float-array a . rest)   (na-num-array a rest 0.0 'float))
(define (na-boolean-array a . rest) (na-num-array a rest #f 'boolean))
;; char-array is a real 'char array (instance? "[C"), seqing as chars via the
;; dispatchers below — io/reader (extended here) and str/slurp consume the seq.
(define (na-char-array a . rest)
  (cond
    ((string? a) (make-jolt-array (list->vector (string->list a)) 'char))
    ((number? a) (make-jolt-array (make-vector (exact (na-idx a)) #\nul) 'char))
    (else (make-jolt-array
           (list->vector (map (lambda (c) (if (char? c) c (integer->char (exact (truncate c)))))
                              (seq->list (jolt-seq a)))) 'char))))
;; (byte-array n [init]) | (byte-array coll). Also coerces the host's OTHER byte
;; carrier — a Chez bytevector (what String/.getBytes produce) — and a string's
;; UTF-8 bytes, so bytevector and byte-array interconvert across interop seams.
(define (na-byte-array a . rest)
  (cond
    ((number? a) (make-jolt-array (make-vector (exact (na-idx a)) (na-byte-of (if (pair? rest) (car rest) 0))) 'byte))
    ((bytevector? a) (make-jolt-array (list->vector (bytevector->u8-list a)) 'byte))
    ((string? a) (make-jolt-array (list->vector (bytevector->u8-list (string->utf8 a))) 'byte))
    (else (make-jolt-array (list->vector (map na-byte-of (seq->list (jolt-seq a)))) 'byte))))
;; jolt byte-array -> Chez bytevector (for String decode / utf8->string).
(define (na-bytearray->bv arr)
  (let* ((v (jolt-array-vec arr)) (n (vector-length v)) (bv (make-bytevector n)))
    (do ((i 0 (+ i 1))) ((= i n)) (bytevector-u8-set! bv i (bitwise-and (exact (vector-ref v i)) #xff)))
    bv))
(define (na-make-array a . rest)    ; (make-array len) | (make-array type len ...)
  (make-jolt-array (make-vector (exact (na-idx (if (number? a) a (car rest)))) jolt-nil) 'object))
(define (na-into-array a . rest)    (na-from-seq (if (pair? rest) (car rest) a) 'object))
(define (na-to-array coll)          (na-from-seq coll 'object))
(define (na-aclone arr)
  (if (jolt-array? arr)
      (make-jolt-array (vector-copy (jolt-array-vec arr)) (jolt-array-kind arr))
      (na-from-seq arr 'object)))

;; --- typed aset (return the stored value) -----------------------------------
(define (na-aset! arr i v) (vector-set! (jolt-array-vec arr) (exact (na-idx i)) v) v)
(define (na-aset-int arr i v)     (na-aset! arr i v))
(define (na-aset-long arr i v)    (na-aset! arr i v))
(define (na-aset-short arr i v)   (na-aset! arr i v))
(define (na-aset-double arr i v)  (na-aset! arr i v))
(define (na-aset-float arr i v)   (na-aset! arr i v))
(define (na-aset-char arr i v)    (na-aset! arr i v))
(define (na-aset-boolean arr i v) (na-aset! arr i v))
(define (na-aset-byte arr i v)
  (vector-set! (jolt-array-vec arr) (exact (na-idx i)) (na-byte-of v)) v)

;; --- coercions (identity on arrays; byte/short are masked scalar casts) ------
(define (na-bytes x) (if (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) x (na-byte-array x)))
(define (na-bytes? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)))
(define (na-identity x) x)
(define (na-byte x)
  (let ((b (bitwise-and (exact (floor x)) #xff))) (if (>= b 128) (- b 256) b)))
(define (na-short x)
  (let ((s (bitwise-and (exact (floor x)) #xffff))) (if (>= s #x8000) (- s #x10000) s)))

;; --- chunked seqs -----------------------------------------------------------
;; A vector's seq is a REAL chunked-seq: (seq v) carries its backing vector +
;; element index (seq.ss cseq-vec), so chunked-seq? is true and chunk-first hands
;; out a 32-element block (a pvec slice) while chunk-rest is the seq at the next
;; block boundary — the Clojure/CLJS ChunkedSeq contract (chunk-first ++
;; chunk-rest == the seq). The eager buffer model (chunk-buffer/chunk-append/
;; chunk) builds a plain cseq; chunk-cons/first/rest fall back to seq ops over it.
(define na-chunk-size 32)
(define-record-type jolt-chunkbuf (fields (mutable items)) (nongenerative jolt-chunkbuf-v1))
(define (na-chunk-buffer cap) (make-jolt-chunkbuf '()))
(define (na-chunk-append b x) (jolt-chunkbuf-items-set! b (append (jolt-chunkbuf-items b) (list x))) b)
(define (na-chunk b) (list->cseq (jolt-chunkbuf-items b)))
(define (na-chunk-cons chunk rest) (jolt-concat chunk rest))
;; backing (vector . end-of-block index) for a vector-seq cell, or #f.
(define (na-vblock s)
  (and (cseq? s) (cseq-cvec s)
       (let* ((v (cseq-cvec s)) (i (cseq-ci s)))
         (cons v (fxmin (fx+ i na-chunk-size) (pvec-count v))))))
(define (na-chunked-seq? x) (and (na-vblock x) #t))
(define (na-chunk-first s)
  (let ((vb (na-vblock s)))
    (if vb (make-pvec (vec-copy-range (pvec-v (car vb)) (cseq-ci s) (cdr vb)))
        (jolt-first s))))               ; eager-buffer fallback
(define (na-chunk-rest s)
  (let ((vb (na-vblock s)))
    (if vb (if (fx>=? (cdr vb) (pvec-count (car vb))) jolt-empty-list (vec->seq (car vb) (cdr vb)))
        (jolt-rest s))))
(define (na-chunk-next s)
  (let ((vb (na-vblock s)))
    (if vb (if (fx>=? (cdr vb) (pvec-count (car vb))) jolt-nil (vec->seq (car vb) (cdr vb)))
        (jolt-next s))))

;; --- extend the collection dispatchers to see a jolt-array ------------------
(define %na-count jolt-count)
(set! jolt-count (lambda (c) (if (jolt-array? c) (vector-length (jolt-array-vec c)) (%na-count c))))
(define %na-seq jolt-seq)
(set! jolt-seq (lambda (c) (if (jolt-array? c) (list->cseq (vector->list (jolt-array-vec c))) (%na-seq c))))
(define %na-nth jolt-nth)
(set! jolt-nth
  (case-lambda
    ((c i)   (if (jolt-array? c) (vector-ref (jolt-array-vec c) (exact (na-idx i))) (%na-nth c i)))
    ((c i d) (if (jolt-array? c)
                 (let ((v (jolt-array-vec c)) (j (exact (na-idx i))))
                   (if (and (>= j 0) (< j (vector-length v))) (vector-ref v j) d))
                 (%na-nth c i d)))))
(define %na-get jolt-get)
(set! jolt-get
  (case-lambda
    ((c k)   (if (jolt-array? c) (jolt-nth c k) (%na-get c k)))
    ((c k d) (if (jolt-array? c) (jolt-nth c k d) (%na-get c k d)))))
;; aset (overlay) writes through jolt.host/ref-put! — mutate the slot, return arr.
;; count/nth/seq/get above are NATIVE-OPS (inlined at call sites), so aget/alength/
;; array-seq/vec already use the set!-extended globals; ref-put! is a host var
;; (var-deref'd), so re-assert its cell to the array-aware closure.
(define %na-ref-put! jolt-ref-put!)
(set! jolt-ref-put!
  (lambda (t k v)
    (if (jolt-array? t) (begin (vector-set! (jolt-array-vec t) (exact (na-idx k)) v) t)
        (%na-ref-put! t k v))))
(def-var! "jolt.host" "ref-put!" jolt-ref-put!)

;; --- array identity: type / class / instance? recognize arrays ---------------
;; (type arr) / (class arr) -> the JVM array class name; (class …) delegates to
;; (jolt-type …) for arrays, so extending jolt-type covers both.
(define %na-type jolt-type)
(set! jolt-type (lambda (x) (if (jolt-array? x) (na-array-class-name x) (%na-type x))))
(def-var! "clojure.core" "type" jolt-type)

;; instance? over an array class token ([I, [C, …). The token reaches us as a
;; string (Class/forName "[C") or symbol; normalize, and pass a non-array string
;; token on as a symbol so the inner wrappers' symbol-t-name doesn't choke.
(define %na-instance-check instance-check)
(set! instance-check
  (lambda (type-sym val)
    (let ((tname (cond ((string? type-sym) type-sym)
                       ((symbol-t? type-sym) (symbol-t-name type-sym))
                       (else #f))))
      (cond
        ((and tname (> (string-length tname) 0) (char=? (string-ref tname 0) #\[))
         (and (jolt-array? val) (string=? (na-array-class-name val) tname)))
        ((string? type-sym) (%na-instance-check (jolt-symbol #f type-sym) val))
        (else (%na-instance-check type-sym val))))))
(def-var! "clojure.core" "instance-check" instance-check)

;; clojure.java.io/reader over a char-array reads its chars (the JVM char[] branch).
(def-var! "clojure.java.io" "reader"
  (lambda (x)
    (if (jolt-array? x)
        (host-new "StringReader"
                  (apply string-append (map jolt-str-render-one (seq->list (jolt-seq x)))))
        (jolt-io-reader x))))

;; --- bind into clojure.core -------------------------------------------------
(for-each (lambda (p) (def-var! "clojure.core" (car p) (cdr p)))
  (list
    (cons "object-array" na-object-array) (cons "int-array" na-int-array)
    (cons "long-array" na-long-array) (cons "short-array" na-short-array)
    (cons "double-array" na-double-array) (cons "float-array" na-float-array)
    (cons "boolean-array" na-boolean-array)
    (cons "byte-array" na-byte-array) (cons "char-array" na-char-array)
    (cons "array?" (lambda (x) (jolt-array? x)))
    (cons "make-array" na-make-array)
    (cons "into-array" na-into-array) (cons "to-array" na-to-array) (cons "aclone" na-aclone)
    (cons "aset-int" na-aset-int) (cons "aset-long" na-aset-long)
    (cons "aset-short" na-aset-short) (cons "aset-double" na-aset-double)
    (cons "aset-float" na-aset-float) (cons "aset-char" na-aset-char)
    (cons "aset-boolean" na-aset-boolean) (cons "aset-byte" na-aset-byte)
    (cons "bytes" na-bytes) (cons "bytes?" na-bytes?)
    (cons "booleans" na-identity) (cons "ints" na-identity) (cons "longs" na-identity)
    (cons "shorts" na-identity) (cons "doubles" na-identity) (cons "floats" na-identity)
    (cons "chars" na-identity) (cons "byte" na-byte) (cons "short" na-short)
    (cons "chunk-buffer" na-chunk-buffer) (cons "chunk-append" na-chunk-append)
    (cons "chunk" na-chunk) (cons "chunk-cons" na-chunk-cons)
    (cons "chunk-first" na-chunk-first) (cons "chunk-rest" na-chunk-rest)
    (cons "chunk-next" na-chunk-next) (cons "chunked-seq?" na-chunked-seq?)))

;; --- clojure.java.io/copy ---------------------------------------------------
;; Copy src -> dst, JVM-style. Raw bytes (byte-array / bytevector / string) and a
;; jhost reader write in one shot; any other source (a stream shim with a .read
;; method, e.g. jolt-lang/http-client's ByteArrayInputStream) drains via .read
;; into a byte-array buffer and .write to dst — both reached through method
;; dispatch, so a library's tagged-table streams work without the host knowing
;; their layout. Lives here (not io.ss) because io.ss loads before byte-array.
(define (jolt-io-copy src dst . _opts)
  (define (write-all! bytes)
    (record-method-dispatch dst "write" (list->cseq (list bytes 0 (vector-length (jolt-array-vec bytes))))))
  (cond
    ((or (bytevector? src) (string? src)
         (and (jolt-array? src) (eq? (jolt-array-kind src) 'byte)))
     (write-all! (na-byte-array src)))
    ((and (jhost? src) (member (jhost-tag src) '("string-reader" "pushback-reader")))
     (write-all! (na-byte-array (drain-reader src))))
    (else
     (let ((buf (na-byte-array 8192)))
       (let loop ()
         (let ((n (record-method-dispatch src "read" (list->cseq (list buf 0 8192)))))
           (when (and (number? n) (> (jnum->exact n) 0))
             (record-method-dispatch dst "write" (list->cseq (list buf 0 n)))
             (loop)))))))
  jolt-nil)
(def-var! "clojure.java.io" "copy" jolt-io-copy)
