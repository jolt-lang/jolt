;; natives-array.ss (jolt-cf1q.7) — Java-style mutable arrays for the Chez host.
;;
;; A jolt-array wraps a Chez mutable vector + a `kind` tag (for bytes?). The array
;; CONSTRUCTORS are native (they build the backing); the overlay's aget/aset/alength
;; are pure over count / nth / jolt.host/ref-put!, so we extend those dispatchers
;; to see a jolt-array (backed by a Chez vector). Loaded after host-table.ss (ref-put!),
;; transients.ss, seq.ss (the dispatchers it chains).

(define-record-type jolt-array (fields (mutable vec) kind) (nongenerative jolt-array-v1))

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
(define (na-int-array a . rest)     (na-num-array a rest 0.0 'int))
(define (na-long-array a . rest)    (na-num-array a rest 0.0 'long))
(define (na-short-array a . rest)   (na-num-array a rest 0.0 'short))
(define (na-double-array a . rest)  (na-num-array a rest 0.0 'double))
(define (na-float-array a . rest)   (na-num-array a rest 0.0 'float))
(define (na-boolean-array a . rest) (na-num-array a rest #f 'boolean))
;; char-array stays in io.ss (a char-SEQ that io/reader / str / slurp consume).
(define (na-byte-array a . rest)
  (if (number? a)
      (make-jolt-array (make-vector (exact (na-idx a)) (na-byte-of (if (pair? rest) (car rest) 0))) 'byte)
      (make-jolt-array (list->vector (map na-byte-of (seq->list (jolt-seq a)))) 'byte)))
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

;; --- chunked seqs (Jolt does not chunk; eager equivalents over a buffer) -----
(define-record-type jolt-chunkbuf (fields (mutable items)) (nongenerative jolt-chunkbuf-v1))
(define (na-chunk-buffer cap) (make-jolt-chunkbuf '()))
(define (na-chunk-append b x) (jolt-chunkbuf-items-set! b (append (jolt-chunkbuf-items b) (list x))) b)
(define (na-chunk b) (list->cseq (jolt-chunkbuf-items b)))
(define (na-chunk-cons chunk rest) (jolt-concat chunk rest))
(define (na-chunk-first s) (jolt-first s))
(define (na-chunk-rest s) (jolt-rest s))
(define (na-chunk-next s) (jolt-next s))

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

;; --- bind into clojure.core -------------------------------------------------
(for-each (lambda (p) (def-var! "clojure.core" (car p) (cdr p)))
  (list
    (cons "object-array" na-object-array) (cons "int-array" na-int-array)
    (cons "long-array" na-long-array) (cons "short-array" na-short-array)
    (cons "double-array" na-double-array) (cons "float-array" na-float-array)
    (cons "boolean-array" na-boolean-array)
    (cons "byte-array" na-byte-array) (cons "make-array" na-make-array)
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
    (cons "chunk-next" na-chunk-next)))
