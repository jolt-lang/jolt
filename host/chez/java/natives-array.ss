;; natives-array.ss — Java-style mutable arrays for the Chez host.
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

(define (na-idx i) (if (and (number? i) (not (exact? i))) (exact (floor i)) (jolt-need-num i)))

;; A double/float jolt-array is backed by a Chez FLVECTOR (unboxed flonums); every
;; other kind keeps a boxed Chez vector. These helpers let the collection
;; dispatchers (count/seq/nth/ref-put!/aset/aclone) and java.util.Arrays work over
;; either backing. Chez has flvector? / make-flvector / flvector-ref / -set! / -length.
(define (na-fl-kind? k) (or (eq? k 'double) (eq? k 'float)))
(define (ja-len v)     (if (flvector? v) (flvector-length v) (vector-length v)))
;; An out-of-range index on the generic aget/aset path throws the typed JVM
;; exception with the JVM message. The proven ^doubles fast path (jolt-flaget/
;; jolt-flaset below) skips this pre-check — it relies on flvector-ref's own
;; range check and its condition classifies at inspection time instead.
(define (na-oob-throw i n)
  (jolt-throw (jolt-host-throwable "java.lang.ArrayIndexOutOfBoundsException"
                                   (format "Index ~a out of bounds for length ~a" i n))))
(define (ja-check v i)
  (unless (and (fixnum? i) (fx>=? i 0) (fx<? i (ja-len v)))
    (if (jolt-nil? i)
        (throw-jvm 'NullPointerException "array index is nil")
        (na-oob-throw i (ja-len v)))))
(define (ja-ref v i)
  (ja-check v i)
  (if (flvector? v) (flvector-ref v i) (vector-ref v i)))
(define (ja-set! v i x)
  (ja-check v i)
  (if (flvector? v)
      (flvector-set! v i (if (flonum? x) x (exact->inexact x)))
      (vector-set! v i x)))
(define (ja->list v)
  (if (flvector? v)
      (let loop ((i (- (flvector-length v) 1)) (acc '()))
        (if (< i 0) acc (loop (- i 1) (cons (flvector-ref v i) acc))))
      (vector->list v)))
(define (ja-copy v)
  (if (flvector? v)
      (let* ((n (flvector-length v)) (r (make-flvector n 0.0)))
        (do ((i 0 (+ i 1))) ((= i n) r) (flvector-set! r i (flvector-ref v i))))
      (vector-copy v)))
(define (na-make-backing n kind init)
  (if (na-fl-kind? kind)
      (make-flvector (exact n) (if (flonum? init) init (exact->inexact init)))
      (make-vector (exact n) init)))
(define (na-list->backing lst kind)
  (if (na-fl-kind? kind)
      (let* ((n (length lst)) (fv (make-flvector n 0.0)))
        (let loop ((i 0) (l lst))
          (if (null? l) fv (begin (flvector-set! fv i (exact->inexact (car l))) (loop (+ i 1) (cdr l))))))
      (list->vector lst)))

(define (na-from-seq x kind) (make-jolt-array (na-list->backing (seq->list (jolt-seq x)) kind) kind))
;; (T-array size) | (T-array size init) | (T-array seq)
(define (na-num-array a rest init kind)
  (if (number? a)
      (make-jolt-array (na-make-backing (na-idx a) kind (if (pair? rest) (car rest) init)) kind)
      (na-from-seq a kind)))

;; numeric tower: array element defaults / narrowed bytes / count are
;; EXACT integers (= JVM byte/short/int), matching exact integer literals.

;; A byte-array element is a SIGNED byte, -128..127, like the JVM's byte[]: a value
;; above 127 reads negative, so (neg? b), a (bit-and b 0xff) mask, and arithmetic
;; over a high byte all behave as they do on the JVM. na-byte-of is the ONE place a
;; value entering a byte array is narrowed — Byte.byteValue() semantics: truncate
;; toward zero (the JVM's d2i, not floor — (byte-array [-1.9]) is -1, not -2), take
;; the low 8 bits, fold the sign bit. na-bytearray->bv masks back to 0..255 at the
;; raw-byte seam, so the two carriers round-trip byte-exactly.
(define (na-u8->byte b) (if (fx<? b 128) b (fx- b 256)))
(define (na-byte-of v) (na-u8->byte (bitwise-and (exact (truncate v)) #xff)))
;; Narrow a value being STORED into an array to its element kind. Only 'byte has a
;; range jolt must maintain (the u8 <-> s8 bridge above depends on it); the other
;; kinds hold whatever integer/flonum they are given, as they always have.
(define (na-elem-of kind v) (if (eq? kind 'byte) (na-byte-of v) v))

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
;; Chez bytevector (unsigned 0..255) -> jolt byte-array (signed -128..127). The
;; inbound half of the raw-bytes seam: every producer of a byte-array from raw bytes
;; — .getBytes, stream reads, Files/readAllBytes, Base64, FFI — funnels through here.
(define (na-bv->bytearray bv)
  (let* ((n (bytevector-length bv)) (v (make-vector n 0)))
    (do ((i 0 (+ i 1))) ((= i n)) (vector-set! v i (na-u8->byte (bytevector-u8-ref bv i))))
    (make-jolt-array v 'byte)))
;; (byte-array n [init]) | (byte-array coll). Also coerces the host's OTHER byte
;; carrier — a Chez bytevector (what the charset encoders produce) — and a string's
;; UTF-8 bytes, so bytevector and byte-array interconvert across interop seams.
(define (na-byte-array a . rest)
  (cond
    ((number? a) (make-jolt-array (make-vector (exact (na-idx a)) (na-byte-of (if (pair? rest) (car rest) 0))) 'byte))
    ((bytevector? a) (na-bv->bytearray a))
    ((string? a) (na-bv->bytearray (string->utf8 a)))
    (else (make-jolt-array (list->vector (map na-byte-of (seq->list (jolt-seq a)))) 'byte))))
;; jolt byte-array -> Chez bytevector (for String decode / utf8->string). The
;; outbound half: the #xff mask folds a signed element back to its raw byte.
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
      (make-jolt-array (ja-copy (jolt-array-vec arr)) (jolt-array-kind arr))
      (na-from-seq arr 'object)))

;; --- typed aset (return the stored value) -----------------------------------
;; Through na-array-set! (below), so a byte array narrows what it is handed rather
;; than storing it raw. The JVM would reject (aset bytes 0 200) outright — Array.set
;; can see that 200 is an Integer, not a Byte — but jolt models every integer as one
;; type, so it cannot tell (byte -1) from 255. Narrowing keeps the array's -128..127
;; invariant intact (aget / seq / (String. bytes) all agree) where a raw store
;; would break it.
(define (na-aset! arr i v) (na-array-set! arr i v))
(define (na-aset-int arr i v)     (na-aset! arr i v))
(define (na-aset-long arr i v)    (na-aset! arr i v))
(define (na-aset-short arr i v)   (na-aset! arr i v))
(define (na-aset-double arr i v)  (na-aset! arr i v))
(define (na-aset-float arr i v)   (na-aset! arr i v))
(define (na-aset-char arr i v)    (na-aset! arr i v))
(define (na-aset-boolean arr i v) (na-aset! arr i v))
(define (na-aset-byte arr i v)
  (let ((bv (jolt-array-vec arr)) (j (exact (na-idx i))) (b (na-byte-of v)))
    (ja-check bv j)
    (vector-set! bv j b) b))

;; --- coercions (identity on arrays; byte/short are masked scalar casts) ------
(define (na-bytes x) (if (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) x (na-byte-array x)))
(define (na-bytes? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)))
(define (na-identity x) x)
(define (na-byte x) (jolt-byte-cast x))
(define (na-short x) (jolt-short-cast x))

;; --- chunked seqs -----------------------------------------------------------
;; The chunked-seq accessors (chunked-seq? / chunk-first / chunk-rest / chunk-next)
;; live in seq.ss with the cseq core they read; here we only bind them plus the
;; chunk-builder API (clojure.lang.ChunkBuffer + chunk-cons). chunk-buffer collects
;; appended items, chunk seals them into a pvec chunk, and chunk-cons prepends that
;; chunk onto a rest seq as a real ChunkedCons (cseq-chunked) — empty chunk == just
;; the rest, like clojure.core/chunk-cons.
(define-record-type jolt-chunkbuf (fields (mutable items)) (nongenerative jolt-chunkbuf-v1))
(define (na-chunk-buffer cap) (make-jolt-chunkbuf '()))
(define (na-chunk-append b x) (jolt-chunkbuf-items-set! b (append (jolt-chunkbuf-items b) (list x))) b)
(define (na-chunk b) (make-pvec (list->vector (jolt-chunkbuf-items b))))
(define (na-chunk-cons chunk rest)
  (if (fx=? 0 (pvec-count chunk)) rest (cseq-chunked chunk 0 rest)))

;; --- extend the collection dispatchers to see a jolt-array ------------------
(register-count-arm! jolt-array? (lambda (c) (ja-len (jolt-array-vec c))))
(register-seq-arm! jolt-array? (lambda (c) (list->cseq (ja->list (jolt-array-vec c)))))
(define %na-nth jolt-nth)
(set! jolt-nth
  (case-lambda
    ((c i)   (if (jolt-array? c) (ja-ref (jolt-array-vec c) (exact (na-idx i))) (%na-nth c i)))
    ((c i d) (if (jolt-array? c)
                 (let ((v (jolt-array-vec c)) (j (exact (na-idx i))))
                   (if (and (>= j 0) (< j (ja-len v))) (ja-ref v j) d))
                 (%na-nth c i d)))))
(def-var! "jolt.host" "array-value?" (lambda (x) (if (jolt-array? x) #t jolt-nil)))
;; jolt-get on arrays stays as a set!-wrap rather than register-get-arm! because
;; the arm dispatch (collections.ss jolt-get-dispatch) already handles the common
;; pmap/pvec/pset cases BEFORE it reaches the arm loop — and jolt-array? extends
;; jolt-nth (not jolt-get directly). The set!-wrap here REUSES jolt-nth (which
;; itself has a count-arm registry) so arrays get the same nth semantics without
;; re-entering the get arm loop. This is the documented fast-path exception.
(define %na-get jolt-get)
(set! jolt-get
  (case-lambda
    ((c k)   (if (jolt-array? c) (jolt-nth c k jolt-nil) (%na-get c k)))
    ((c k d) (if (jolt-array? c) (jolt-nth c k d) (%na-get c k d)))))
;; aset (overlay) writes through jolt.host/ref-put! — mutate the slot, return arr.
;; count/nth/seq/get above are NATIVE-OPS (inlined at call sites), so aget/alength/
;; array-seq/vec already use the set!-extended globals; ref-put! is a host var
;; (var-deref'd), so re-assert its cell to the array-aware closure.
;; THE array write seam: the aset overlay, jolt-aset3, and any jolt.host/ref-put!
;; on an array all land here. Narrows the value to the element kind (na-elem-of) and
;; answers what was actually stored, so aset's return agrees with a following aget.
(define (na-array-set! a k v)
  (let ((sv (na-elem-of (jolt-array-kind a) v)))
    (ja-set! (jolt-array-vec a) (exact (na-idx k)) sv) sv))
(define %na-ref-put! jolt-ref-put!)
(set! jolt-ref-put!
  (lambda (t k v)
    (if (jolt-array? t) (begin (na-array-set! t k v) t)
        (%na-ref-put! t k v))))
(def-var! "jolt.host" "ref-put!" jolt-ref-put!)
;; native-op target for the 1-dim (aset arr i v): write through the array-aware
;; seam and return the stored value (JVM aset returns the val, not the array).
(define (jolt-aset3 a i v)
  (if (jolt-array? a) (na-array-set! a i v) (begin (jolt-ref-put! a i v) v)))
;; unboxed read target for (aget ^doubles a i): direct flvector-ref on the backing,
;; skipping jolt-nth's case-lambda + jolt-array?/flvector? dispatch. Emitted only
;; when jolt.passes.numeric proved the array is a ^doubles/^floats (flvector) param.
;; The index takes a fixnum?-first fast path: a proven-:long loop counter is always
;; a fixnum, and the (exact (na-idx i)) coercion (two procedure calls) was the
;; dominant per-access cost in the hot loop. Non-fixnum indices (flonum/bignum/
;; ratio) keep the coercing path unchanged; out-of-range still raises via
;; flvector-ref's range check (the array bounds contract).
(define (jolt-flaget a i)
  (flvector-ref (jolt-array-vec a) (if (fixnum? i) i (exact (na-idx i)))))
;; unboxed write target for (aset ^doubles a i v): direct flvector-set!, returning
;; the stored flonum (JVM aset returns the val). Same fixnum-first index path.
(define (jolt-flaset a i v)
  (let ((fv (if (flonum? v) v (exact->inexact v))))
    (flvector-set! (jolt-array-vec a) (if (fixnum? i) i (exact (na-idx i))) fv) fv))

;; A range condition escaping jolt-flaget/jolt-flaset IS the array bounds error
;; on the proven ^doubles path (a typed pre-check there costs ~1ns/access, ~11%
;; on an array-walking loop; wrapping in guard costs ~30ns/call). Classify the
;; raw Chez condition at inspection time instead: (class e) and instance? report
;; java.lang.ArrayIndexOutOfBoundsException, so a typed catch dispatches
;; precisely through the exception hierarchy and an unrelated class does not
;; broad-match. flvector-ref/-set! appear nowhere else in the runtime (the
;; generic ja-ref/ja-set! path pre-checks and throws typed before reaching
;; them), so the condition's who field is a precise key.
(define (na-flv-oob-condition? c)
  (and (condition? c) (who-condition? c)
       (memq (condition-who c) '(flvector-ref flvector-set!))))
(register-class-arm! na-flv-oob-condition?
  (lambda (c) "java.lang.ArrayIndexOutOfBoundsException"))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (na-flv-oob-condition? val)
        (exception-isa? "ArrayIndexOutOfBoundsException"
                        (last-dot (if (string? type-sym) type-sym (symbol-t-name type-sym))))
        'pass)))

;; --- array identity: type / class / instance? recognize arrays ---------------
;; (type arr) / (class arr) -> the JVM array class name; (class …) delegates to
;; (jolt-type …) for arrays, so extending jolt-type covers both.
(register-type-arm! jolt-array? (lambda (x) (na-array-class-name x)))

;; instance? over an array class token ([I, [C, …). An array token reaches us as
;; a string ("[C", from (Class/forName "[C")) — the dispatcher leaves it a string
;; (non-array string tokens are already normalized to symbols there); decide it
;; here, deferring everything else.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tname (cond ((string? type-sym) type-sym)
                       ((symbol-t? type-sym) (symbol-t-name type-sym))
                       (else #f))))
      (if (and tname (> (string-length tname) 0) (char=? (string-ref tname 0) #\[))
          (and (jolt-array? val) (string=? (na-array-class-name val) tname))
          'pass))))

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
    ((and (jhost? src) (or (string=? (jhost-tag src) "string-reader")
                           (pushback-reader-tag? (jhost-tag src))))
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

;; java.lang.reflect.Array — allocation and element access over an array whose
;; component type is named rather than written literally. This is not the
;; reflection API in any deep sense: newInstance with a concrete component type
;; is what make-array already does, and malli's own :bb and :cljs branches spell
;; the same call (object-array capacity). Like make-array, the component type
;; selects nothing here — jolt's arrays are object-kinded unless built by a typed
;; constructor — so a primitive component gives an object array of that length.
(define (na-need-array x)
  (if (jolt-array? x)
      x
      (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                       "Argument is not an array"))))
(let ((statics
       (list
        (cons "newInstance" (lambda (component len . dims)
                              (if (pair? dims)
                                  (throw-jvm (quote IllegalArgumentException)
                                    "Array/newInstance: multi-dimensional arrays are not supported")
                                  (na-make-array component len))))
        (cons "getLength" (lambda (arr) (->num (ja-len (jolt-array-vec (na-need-array arr))))))
        (cons "get" (lambda (arr i) (ja-ref (jolt-array-vec (na-need-array arr)) (na-idx i))))
        (cons "set" (lambda (arr i v) (na-array-set! (na-need-array arr) (na-idx i) v) jolt-nil)))))
  (register-class-statics! "Array" statics)
  (register-class-statics! "java.lang.reflect.Array" statics))

;; java.lang.reflect.Field over the modeled class registry: getDeclaredFields on
;; a Class naming a deftype/defrecord returns its declared fields, each
;; answering getName / setAccessible / get — the reflective field walk
;; (fireworks' datatype->map) works because the model already holds the field
;; list in the type's descriptor.
(define (reflect-field-name self) (vector-ref (jhost-state self) 0))
(register-host-methods! "reflect-field"
  (list (cons "getName" (lambda (self) (let ((k (reflect-field-name self)))
                                         (if (keyword? k) (keyword-t-name k) (jolt-str-render-one k)))))
        (cons "setAccessible" (lambda (self v) jolt-nil))
        (cons "get" (lambda (self obj)
                      (jolt-get obj (reflect-field-name self) jolt-nil)))
        (cons "toString" (lambda (self) (jolt-str-render-one (reflect-field-name self))))))
;; --- reflective STATIC fields -------------------------------------------------
;; class name + field name -> a thunk returning the field's value. Class.getDeclared
;; Field consults this before the deftype/defrecord descriptor, so a host-modeled
;; static field is registered as DATA rather than as another string comparison
;; inside getDeclaredField. clojure.lang.Keyword/table (below) is the first entry;
;; the next library that reads a static reflectively adds a row, not a branch.
(define static-field-tbl (make-hashtable string-hash string=?))
(define (static-field-key cls nm) (string-append cls "/" nm))
(define (register-static-field! cls nm getter)
  (hashtable-set! static-field-tbl (static-field-key cls nm) getter)
  jolt-nil)
(define (lookup-static-field cls nm) (hashtable-ref static-field-tbl (static-field-key cls nm) #f))
;; One host type serves every registered static: it carries the class/field names
;; (so getName / toString read correctly) plus the getter.
(register-host-methods! "static-field"
  (list (cons "setAccessible" (lambda (self v) jolt-nil))
        (cons "get" (lambda (self obj) ((vector-ref (jhost-state self) 2))))
        (cons "getName" (lambda (self) (vector-ref (jhost-state self) 1)))
        (cons "toString" (lambda (self)
                           (string-append "static field " (vector-ref (jhost-state self) 0)
                                          "." (vector-ref (jhost-state self) 1))))))

;; Snapshot of the keyword intern tables as a jolt map of symbol -> keyword,
;; the way the JVM's clojure.lang.Keyword.table reads (its Reference values are
;; irrelevant to consumers, which only iterate the keys).
(define (keyword-intern-table-map)
  (let ((acc '()))
    (let-values (((ks vs) (hashtable-entries keyword-table-bare)))
      (vector-for-each (lambda (name kw) (set! acc (cons (jolt-symbol #f name) (cons kw acc)))) ks vs))
    (let-values (((ks vs) (hashtable-entries keyword-table)))
      (vector-for-each
        (lambda (nk kw) (set! acc (cons (jolt-symbol (keyword-t-ns kw) (keyword-t-name kw)) (cons kw acc))))
        ks vs))
    (apply jolt-hash-map acc)))
;; clojure.lang.Keyword.table — compliment's keyword source reads it reflectively.
(register-static-field! "clojure.lang.Keyword" "table" keyword-intern-table-map)
(register-host-methods! "class"
  (list (cons "getDeclaredFields"
              (lambda (self)
                (let ((desc (hashtable-ref chez-tag-desc (jclass-name self) #f)))
                  (make-jolt-array
                   (if desc
                       (vector-map (lambda (k) (make-jhost "reflect-field" (vector k)))
                                   (jrdesc-fkeys desc))
                       (vector))
                   'objects))))
        (cons "getDeclaredField"
              (lambda (self name)
                (cond ((lookup-static-field (jclass-name self) name)
                       => (lambda (getter)
                            (make-jhost "static-field"
                                        (vector (jclass-name self) name getter))))
                      ((let ((desc (hashtable-ref chez-tag-desc (jclass-name self) #f)))
                         (and desc
                              (find (lambda (k) (string=? (jolt-str-render-one k) name))
                                    (vector->list (jrdesc-fkeys desc)))))
                       => (lambda (k) (make-jhost "reflect-field" (vector k))))
                      (else (throw-jvm 'NoSuchFieldException name)))))))
