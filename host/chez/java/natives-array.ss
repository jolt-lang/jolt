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

;; …and the matching seq flavor. The JVM gives an array's seq its own class per
;; element type (ArraySeq$ArraySeq_int for an int[], plain ArraySeq for an
;; Object[]), so the seq arm below labels the chain from the array's kind exactly
;; the way na-array-class-name labels the array itself.
(define (na-seq-kind arr)
  (case (jolt-array-kind arr)
    ((int) sk-array-int) ((long) sk-array-long) ((short) sk-array-short)
    ((double) sk-array-double) ((float) sk-array-float) ((boolean) sk-array-bool)
    ((byte) sk-array-byte) ((char) sk-array-char)
    (else sk-array-seq)))

;; The element type an array constructor was HANDED, as a backing kind. A primitive
;; class token evaluates to its own name here — Integer/TYPE is "int", Double/TYPE is
;; "double" — so on those eight names the mapping is the identity, and the type
;; argument that into-array / make-array used to discard now picks the backing. A
;; boxed or reference type (Integer, String, a record class) is one 'object array:
;; jolt models every reference array as a single kind, which is the rule arraycopy
;; states (host-static-methods.ss) and why their class is [Ljava.lang.Object;.
(define na-prim-type-kinds
  '(("int" . int) ("long" . long) ("short" . short) ("double" . double)
    ("float" . float) ("boolean" . boolean) ("byte" . byte) ("char" . char)))
(define (na-type-kind t)
  (let* ((n (cond ((string? t) t) ((jclass? t) (jclass-name t)) (else #f)))
         (hit (and n (assoc n na-prim-type-kinds))))
    (if hit (cdr hit) 'object)))
;; The JVM's zero for an element kind: an int/long/short/byte array reads 0 (not
;; 0.0), a double/float 0.0, a boolean false, a char NUL, a reference array nil.
(define (na-zero-of kind)
  (case kind
    ((int long short byte) 0)
    ((double float) 0.0)
    ((boolean) #f)
    ((char) #\nul)
    (else jolt-nil)))

(define (na-idx i) (if (and (number? i) (not (exact? i))) (exact (floor i)) (jolt-need-num i)))

;; A double/float jolt-array is backed by a Chez FLVECTOR (unboxed flonums); every
;; other kind keeps a boxed Chez vector. These helpers let the collection
;; dispatchers (count/seq/nth/ref-put!/aset/aclone) and java.util.Arrays work over
;; either backing. Chez has flvector? / make-flvector / flvector-ref / -set! / -length.
(define (na-fl-kind? k) (or (eq? k 'double) (eq? k 'float)))
(define (ja-len v)     (if (flvector? v) (flvector-length v) (vector-length v)))
;; alength in call position (op registry): the count of any array-like, but nil
;; is the NullPointerException the JVM raises rather than 0.
(define (jolt-alength a)
  (if (jolt-nil? a) (throw-jvm 'NullPointerException "") (jolt-count a)))
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

;; A byte array's elements are signed-byte-folded, the same coercion aset applies
;; through na-elem-of — so (into-array Byte/TYPE …) stores bytes rather than whatever
;; magnitude it was handed. Only that kind needs the pass, so the common path keeps
;; building the backing straight off the seq.
(define (na-from-seq x kind)
  (let ((xs (seq->list (jolt-seq x))))
    (make-jolt-array (na-list->backing (if (eq? kind 'byte) (map na-byte-of xs) xs) kind) kind)))
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
  (let* ((typed? (not (number? a)))
         (kind (if typed? (na-type-kind a) 'object))
         (len (exact (na-idx (if typed? (car rest) a)))))
    (make-jolt-array (na-make-backing len kind (na-zero-of kind)) kind)))
;; (into-array coll) | (into-array type coll). The typed form honors its element
;; type, so (into-array Integer/TYPE …) is an int[] — it used to build an Object[]
;; and report [Ljava.lang.Object; where the JVM says [I.
;; NOTE the untyped form stays an Object[] while the JVM infers the element class
;; from the first element ((into-array [1 2]) is a Long[] there); that follows from
;; jolt modelling reference arrays as one kind, same as the typed reference case.
(define (na-into-array a . rest)
  (if (pair? rest)
      (na-from-seq (car rest) (na-type-kind a))
      (na-from-seq a 'object)))
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
;; The buffer is a vector sized by cap (32 everywhere in core) + a fill count, so
;; an append is one vector-set! — it was a per-item (append items (list x)) list
;; copy, O(n^2) over a chunk's life with cap ignored (`make chunkscaling` gates
;; the shape). Appends past cap grow the vector: the JVM ChunkBuffer throws
;; there, and growing is the documented jolt superset (test/chez/unit.edn
;; "chunk-builder overflow"). Sealing copies, so the buffer stays appendable
;; after chunk — matching the list implementation, where the JVM nulls it.
(define-record-type jolt-chunkbuf (fields (mutable vec) (mutable cnt)) (nongenerative jolt-chunkbuf-v2))
(define (na-chunk-buffer cap)
  (make-jolt-chunkbuf (make-vector (if (and (fixnum? cap) (fx>? cap 0)) cap 32)) 0))
(define (na-chunk-append b x)
  (let ((v (jolt-chunkbuf-vec b)) (n (jolt-chunkbuf-cnt b)))
    (let ((v (if (fx=? n (vector-length v))
                 (let ((w (make-vector (fx* 2 n))))
                   (let copy ((i 0)) (when (fx<? i n) (vector-set! w i (vector-ref v i)) (copy (fx+ i 1))))
                   (jolt-chunkbuf-vec-set! b w)
                   w)
                 v)))
      (vector-set! v n x)
      (jolt-chunkbuf-cnt-set! b (fx+ n 1))))
  b)
(define (na-chunk b)
  (let* ((n (jolt-chunkbuf-cnt b)) (v (jolt-chunkbuf-vec b)) (out (make-vector n)))
    (let copy ((i 0)) (when (fx<? i n) (vector-set! out i (vector-ref v i)) (copy (fx+ i 1))))
    (make-pvec out)))
(define (na-chunk-cons chunk rest)
  (if (fx=? 0 (pvec-count chunk)) rest (cseq-chunked chunk 0 rest)))
;; the buffer is clojure.lang.ChunkBuffer, a Counted: count reads its fill
(register-class-arm! jolt-chunkbuf? (lambda (b) "clojure.lang.ChunkBuffer"))
(register-count-arm! jolt-chunkbuf? (lambda (b) (jolt-chunkbuf-cnt b)))

;; --- extend the collection dispatchers to see a jolt-array ------------------
(register-count-arm! jolt-array? (lambda (c) (ja-len (jolt-array-vec c))))
(register-seq-arm! jolt-array?
  (lambda (c) (list->cseq/k (ja->list (jolt-array-vec c)) (na-seq-kind c))))
(define %na-nth jolt-nth)
;; RT.nth tests Indexed first and returns; this is the OUTERMOST jolt-nth
;; wrapper (natives-array loads last of the set! chain), so a pvec receiver
;; here would otherwise pay for the jolt-array?, al/sb shim, lazyseq and
;; transient probes plus three chained calls before reaching the base pvec
;; arm. pvec first; pvec-nth!/pvec-nth-d are the same shared reads the base
;; definition's own pvec arms call, so semantics move nowhere. pvec is
;; disjoint from every type the chain below intercepts (each is its own
;; record type), so reordering cannot shadow them.
(set! jolt-nth
  (case-lambda
    ((c i)   (if (pvec? c)
                 (pvec-nth! c i)
                 (if (jolt-array? c) (ja-ref (jolt-array-vec c) (exact (na-idx i))) (%na-nth c i))))
    ((c i d) (if (pvec? c)
                 (begin (jolt-nth-nil-idx! i) (pvec-nth-d c i d))
                 (if (jolt-array? c)
                     (let ((v (jolt-array-vec c)) (j (exact (na-idx i))))
                       (if (and (>= j 0) (< j (ja-len v))) (ja-ref v j) d))
                     (%na-nth c i d))))))
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
;; The backing flvector of a ^doubles PARAM, bound once at the arity's entry by
;; the back end so a loop over the array indexes the flvector directly instead
;; of re-reading the checked record accessor on every aget/aset (bench/arrays
;; 225 -> 145 ms in a Chez probe of the emitted loop). Raising here is the JVM
;; checkcast at the call boundary: a ^doubles parameter that is not a double[]
;; raises on entry there too, whether or not the body ever reads it.
(define (jolt-array-vec-of a)
  (if (jolt-array? a)
      (jolt-array-vec a)
      (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                   (string-append "class " (guard (e (#t "?")) (jolt-class-name a))
                                  " cannot be cast to class [D")))))
(define (jolt-flaget a i)
  (flvector-ref (jolt-array-vec a) (if (fixnum? i) i (exact (na-idx i)))))
;; unboxed write target for (aset ^doubles a i v): direct flvector-set!, returning
;; the stored flonum (JVM aset returns the val). Same fixnum-first index path.
(define (jolt-flaset a i v)
  (let ((fv (if (flonum? v) v (exact->inexact v))))
    (flvector-set! (jolt-array-vec a) (if (fixnum? i) i (exact (na-idx i))) fv) fv))

;; A range condition escaping jolt-flaget/jolt-flaset IS the array bounds error
;; on the proven ^doubles path (a typed pre-check there costs ~1ns/access, ~11%
;; on an array-walking loop; wrapping in guard costs ~30ns/call). The catch
;; boundary turns that raw condition into a
;; java.lang.ArrayIndexOutOfBoundsException by its who field (host-faults.ss
;; array-index-whos), so a typed catch dispatches precisely through the
;; exception hierarchy and an unrelated class does not match.

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
;;
;; "<modifiers> <type> <declaring-class>.<name>" for a field, and the same with a
;; "(params)" tail for a method — the two shapes java.lang.reflect's own toString
;; prints, spelled once. jmod-string (java/host-static-classes.ss) names the
;; modifier bits exactly as Modifier/toString does.
(define (jreflect-member-str mods type cls nm params)
  (let ((ms (jmod-string mods)))
    (string-append (if (string=? ms "") "" (string-append ms " "))
                   type " " cls "." nm
                   (if params (string-append "(" params ")") ""))))

(define (reflect-field-name self) (vector-ref (jhost-state self) 0))
;; The field's NAME as the JVM reports it: a record's field keys are keywords, and
;; Field.getName has no leading colon. getDeclaredField used to match against
;; jolt-str-render-one instead, which renders :x as ":x", so a lookup by the name
;; getDeclaredFields had just reported could never hit and every field on every
;; record raised NoSuchFieldException. One helper now answers for both.
(define (reflect-key-name k) (if (keyword? k) (keyword-t-name k) (jolt-str-render-one k)))
(register-host-methods! "reflect-field"
  (list (cons "getName" (lambda (self) (reflect-key-name (reflect-field-name self))))
        (cons "setAccessible" (lambda (self v) jolt-nil))
        ;; the declaring class travels with the field so a caller can ask where it
        ;; came from, as clojure.reflect's field->map does
        (cons "getDeclaringClass"
              (lambda (self) (let ((c (vector-ref (jhost-state self) 1)))
                               (if c (jolt-class-for c) jolt-nil))))
        (cons "getType" (lambda (self) (jolt-class-for "java.lang.Object")))
        (cons "getModifiers" (lambda (self) (->num 1)))
        (cons "get" (lambda (self obj)
                      (jolt-get obj (reflect-field-name self) jolt-nil)))
        ;; the JVM's shape — "public java.lang.Object user.Foo.a". It used to be
        ;; the bare field name, which said neither where the field lives nor that
        ;; it is a field at all.
        (cons "toString"
              (lambda (self)
                (jreflect-member-str 1 "java.lang.Object"
                                     (let ((c (vector-ref (jhost-state self) 1))) (or c "?"))
                                     (reflect-key-name (reflect-field-name self)) #f)))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "reflect-field")))
                      (lambda (x) (record-method-dispatch x "toString" jolt-nil)))
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
;; The names registered here FOR one class, so getDeclaredFields can report them
;; alongside the class's other fields. The table is keyed "Class/field" because
;; the lookup is by pair; a listing has to take the scan.
(define (static-field-names-of cls)
  (let ((prefix (string-append cls "/"))
        (ks (hashtable-keys static-field-tbl)))
    (let loop ((i 0) (acc '()))
      (cond ((fx=? i (vector-length ks)) (reverse acc))
            ((let ((k (vector-ref ks i)))
               (and (fx>=? (string-length k) (string-length prefix))
                    (string=? (substring k 0 (string-length prefix)) prefix)
                    (substring k (string-length prefix) (string-length k))))
             => (lambda (nm) (loop (fx+ i 1) (cons nm acc))))
            (else (loop (fx+ i 1) acc))))))
;; One host type serves every registered static: it carries the class/field names
;; (so getName / toString read correctly) plus the getter.
(register-host-methods! "static-field"
  (list (cons "setAccessible" (lambda (self v) jolt-nil))
        (cons "get" (lambda (self obj) ((vector-ref (jhost-state self) 2))))
        (cons "getName" (lambda (self) (vector-ref (jhost-state self) 1)))
        (cons "getDeclaringClass" (lambda (self) (jolt-class-for (vector-ref (jhost-state self) 0))))
        (cons "getType" (lambda (self) (jolt-class-for "java.lang.Object")))
        ;; public static final — the shape every static jolt models is registered
        ;; in, and the flags clojure.reflect turns into #{:public :static :final}.
        ;; A caller filtering members by :static (typedclojure's static-members
        ;; does) sees nothing at all without this, since the default was 1.
        (cons "getModifiers" (lambda (self) (->num 25)))
        (cons "toString"
              (lambda (self)
                (jreflect-member-str 25 "java.lang.Object"
                                     (vector-ref (jhost-state self) 0)
                                     (vector-ref (jhost-state self) 1) #f)))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "static-field")))
                      (lambda (x) (record-method-dispatch x "toString" jolt-nil)))

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
;; --- the class-statics registry as MEMBERS -------------------------------------
;; class-statics-tbl (java/host-static.ss) holds a class's statics with one rule:
;; a procedure is a static METHOD, anything else a static FIELD's value. That is
;; the same split reflection asks for, so the registry answers both halves of
;; Class.getDeclaredFields / getDeclaredMethods for every host class jolt models —
;; Long/MAX_VALUE, System/out, Math/floor, clojure.lang.RT/REQUIRE_LOCK. Before
;; this, a host class reported no members at all and every reflective lookup on
;; one raised, which is what a caller with a NoSuchFieldException handler could not
;; tell apart from a field that genuinely is not there.
;;
;; Answers (name . value) pairs of one half or the other: a Method member needs the
;; procedure as well as the name, and pairing them here is one table walk instead
;; of a walk plus a lookup per hit.
(define (class-static-members cls want-methods?)
  (let ((h (lookup-class class-statics-tbl cls)))
    (if (not h) '()
        (let-values (((ks vs) (hashtable-entries h)))
          (let loop ((i 0) (acc '()))
            (cond ((fx=? i (vector-length ks)) (reverse acc))
                  ((eq? want-methods? (and (procedure? (vector-ref vs i)) #t))
                   (loop (fx+ i 1) (cons (cons (vector-ref ks i) (vector-ref vs i)) acc)))
                  (else (loop (fx+ i 1) acc))))))))
;; A Field over a registered static: the getter goes back through host-static-ref
;; rather than closing over the value, so a mutable static cell (Compiler/LINE)
;; reads its CURRENT value the way the JVM's Field.get does.
(define (class-static-field-obj cls nm)
  (make-jhost "static-field" (vector cls nm (lambda () (host-static-ref cls nm)))))
;; Does cls register nm as a static FIELD — present, and not a procedure? A field
;; may hold a falsy value (Boolean/FALSE is #f), so a bare hashtable-ref result
;; cannot answer this; the miss marker host-static.ss already has is what can.
(define (class-static-field? cls nm)
  (let ((h (lookup-class class-statics-tbl cls)))
    (and h (let ((v (hashtable-ref h nm host-static-miss)))
             (and (not (eq? v host-static-miss)) (not (procedure? v)))))))

(register-host-methods! "class"
  (list (cons "getDeclaredFields"
              (lambda (self)
                (let* ((cls (jclass-name self))
                       (desc (hashtable-ref chez-tag-desc cls #f))
                       (declared (if desc
                                     (map (lambda (k) (make-jhost "reflect-field" (vector k cls)))
                                          (vector->list (jrdesc-fkeys desc)))
                                     '()))
                       (registered (map (lambda (nm)
                                          (make-jhost "static-field"
                                                      (vector cls nm (lookup-static-field cls nm))))
                                        (static-field-names-of cls)))
                       (statics (map (lambda (p) (class-static-field-obj cls (car p)))
                                     (class-static-members cls #f))))
                  (make-jolt-array (list->vector (append declared registered statics)) 'objects))))
        (cons "getDeclaredField"
              (lambda (self name)
                (cond ((lookup-static-field (jclass-name self) name)
                       => (lambda (getter)
                            (make-jhost "static-field"
                                        (vector (jclass-name self) name getter))))
                      ((let ((desc (hashtable-ref chez-tag-desc (jclass-name self) #f)))
                         (and desc
                              (find (lambda (k) (string=? (reflect-key-name k) name))
                                    (vector->list (jrdesc-fkeys desc)))))
                       => (lambda (k) (make-jhost "reflect-field" (vector k (jclass-name self)))))
                      ((class-static-field? (jclass-name self) name)
                       (class-static-field-obj (jclass-name self) name))
                      (else (throw-jvm 'NoSuchFieldException name)))))
        ;; getFields / getField are the public-member spellings. jolt's member
        ;; sets are already flat per type (there is no separate inherited set to
        ;; add), so they answer the same as the declared pair.
        (cons "getFields"
              (lambda (self) (record-method-dispatch self "getDeclaredFields" jolt-nil)))
        (cons "getField"
              (lambda (self name) (record-method-dispatch self "getDeclaredField" (list->cseq (list name)))))
        ;; --- methods ----------------------------------------------------------
        ;; Every method jolt's registries record for this class: the protocol
        ;; registry's (whichever protocol or interface declares it), the host shim
        ;; table's for a class a jhost tag models, and the class-statics table's
        ;; procedures as static methods. A class jolt models by other means answers
        ;; with whatever of those it has rather than guessing at the JVM's full set
        ;; (recorded divergence :reflection-member-model) — String's instance
        ;; methods are a `cond` in java/natives-str.ss, not data anything can
        ;; enumerate, so String reports its statics and nothing else.
        (cons "getDeclaredMethods" (lambda (self) (class-method-array self)))
        (cons "getMethods" (lambda (self) (class-method-array self)))
        ;; getMethod / getDeclaredMethod: the JVM selects an overload by parameter
        ;; TYPES; jolt has no signatures, so it selects by parameter COUNT and
        ;; raises NoSuchMethodException when nothing matches — the exception the
        ;; callers that ask this way are already catching.
        (cons "getDeclaredMethod" (lambda (self name . params) (class-find-method self name params)))
        (cons "getMethod" (lambda (self name . params) (class-find-method self name params)))
        (cons "getConstructor" (lambda (self . params) (class-find-ctor self params)))
        (cons "getDeclaredConstructor" (lambda (self . params) (class-find-ctor self params)))))

;; How many parameters a reflective lookup was asked for. Java spells these
;; (String, Class...) so from Clojure the types arrive as ONE array — the usual
;; (into-array Class [...]) — but a caller may also spell them out, and nil is the
;; no-argument case either way.
(define (jreflect-arity params)
  (cond ((null? params) 0)
        ((and (null? (cdr params)) (jolt-nil? (car params))) 0)
        ((and (null? (cdr params)) (jolt-array? (car params)))
         (ja-len (jolt-array-vec (car params))))
        ((and (null? (cdr params)) (or (pvec? (car params)) (cseq? (car params))))
         (length (seq->list (jolt-seq (car params)))))
        (else (length params))))
(define (class-find-method cls name params)
  (let* ((want (jreflect-arity params))
         (ms (vector->list (jolt-array-vec (class-method-array cls))))
         (named (filter (lambda (m) (string=? (reflect-method-name m) name)) ms)))
    (or (find (lambda (m) (fx=? want (reflect-method-arity m))) named)
        ;; Parameter counts are a FLOOR, not a signature: most registered statics
        ;; are (lambda args …) and report 0, so an exact-count miss is far more
        ;; often jolt not knowing the arity than the method not existing. A name
        ;; that IS registered therefore answers with its member rather than
        ;; raising — the caller gets the method it asked for, with the parameter
        ;; types jolt has for everything (Object). Only an unknown NAME raises.
        (and (pair? named) (car named))
        (throw-jvm 'NoSuchMethodException
                   (string-append (jclass-name cls) "." name "(" (jreflect-param-str want) ")")))))
(define (class-find-ctor cls params)
  (let ((want (jreflect-arity params))
        (cs (seq->list (jolt-seq (class-constructors cls)))))
    (or (find (lambda (c) (= want (jnum->exact (vector-ref (jhost-state c) 1)))) cs)
        (throw-jvm 'NoSuchMethodException
                   (string-append (jclass-name cls) ".<init>(" (jreflect-param-str want) ")")))))

;; java.lang.reflect.Method, enough of one to name it, say where it was declared,
;; and call it.
(define (reflect-method-cls self) (vector-ref (jhost-state self) 0))
(define (reflect-method-name self) (vector-ref (jhost-state self) 1))
(define (reflect-method-fn self) (vector-ref (jhost-state self) 2))
;; A STATIC method's impl takes exactly its arguments; an instance method's takes
;; `this` first. The flag is what tells the two apart everywhere it matters —
;; the parameter count, the modifiers, and whether invoke passes the target — so
;; it travels in the member itself rather than being re-derived per call site.
(define (reflect-method-static? self) (vector-ref (jhost-state self) 3))
;; Lowest arity the impl accepts, less the leading `this` for an instance method —
;; the JVM's parameter count for the same method.
(define (reflect-method-param-count f static?)
  (if (procedure? f)
      (let ((mask (procedure-arity-mask f))
            (drop (if static? 0 1)))
        (let loop ((k (if static? 0 1)))
          (cond ((fx>? k 24) 0)
                ((bitwise-bit-set? mask k) (fx- k drop))
                (else (loop (fx+ k 1))))))
      0))
(define (reflect-method-arity self)
  (reflect-method-param-count (reflect-method-fn self) (reflect-method-static? self)))
(register-host-methods! "reflect-method"
  (list (cons "getName" (lambda (self) (reflect-method-name self)))
        (cons "getDeclaringClass" (lambda (self) (jolt-class-for (reflect-method-cls self))))
        (cons "setAccessible" (lambda (self v) jolt-nil))
        (cons "getParameterCount" (lambda (self) (->num (reflect-method-arity self))))
        (cons "getParameterTypes"
              (lambda (self)
                (make-jolt-array
                 (make-vector (reflect-method-arity self) (jolt-class-for "java.lang.Object"))
                 'objects)))
        ;; jolt's registries carry no return or throws signature; Object and empty
        ;; are the honest answers, and they are what the JVM reports for an
        ;; Object-returning method with no checked exceptions anyway.
        (cons "getReturnType" (lambda (self) (jolt-class-for "java.lang.Object")))
        (cons "getExceptionTypes" (lambda (self) (make-jolt-array (vector) 'objects)))
        ;; public, plus static for a class-statics entry — the bit clojure.reflect
        ;; renders as :static and every "is this a static method" filter reads.
        (cons "getModifiers" (lambda (self) (->num (if (reflect-method-static? self) 9 1))))
        ;; Method.invoke(obj, Object... args) — from Clojure the varargs arrive as
        ;; one array, so a lone trailing array IS the argument list and gets
        ;; splatted. Anything else is passed straight through, which is what a
        ;; caller spelling the arguments out means. A static ignores the target,
        ;; as the JVM does (Method.invoke takes null there).
        (cons "invoke"
              (lambda (self target . args)
                (let ((as (if (and (= 1 (length args)) (jolt-array? (car args)))
                              (vector->list (jolt-array-vec (car args)))
                              args)))
                  (if (reflect-method-static? self)
                      (apply (reflect-method-fn self) as)
                      (apply (reflect-method-fn self) target as)))))
        (cons "toString"
              (lambda (self)
                (jreflect-member-str (if (reflect-method-static? self) 9 1)
                                     "java.lang.Object"
                                     (reflect-method-cls self) (reflect-method-name self)
                                     (jreflect-param-str (reflect-method-arity self)))))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "reflect-method")))
                      (lambda (x) (record-method-dispatch x "toString" jolt-nil)))
;; The type's registered methods as a Method[]. type-registry is
;; tag -> (proto -> (method-name -> fn)); a name declared by two protocols
;; surfaces once per declaration, as it does on the JVM for two interfaces.
(define (class-method-array cls)
  (let* ((nm (jclass-name cls))
         (fqn (jch-fqn-of-simple nm))
         (ti (or (hashtable-ref type-registry nm #f)
                 (hashtable-ref type-registry fqn #f)))
         (acc '()))
    (define (add! owner name f static?)
      (set! acc (cons (make-jhost "reflect-method" (vector owner name f static?)) acc)))
    (when ti
      (let-values (((protos impls) (hashtable-entries ti)))
        (vector-for-each
         (lambda (proto pi)
           (let-values (((names fns) (hashtable-entries pi)))
             (vector-for-each (lambda (n f) (add! nm n f #f)) names fns)))
         protos impls)))
    ;; the shim's instance methods, for a class a jhost tag models. Two tags can
    ;; name one class, so both are walked; a name registered under both surfaces
    ;; twice, as an interface method inherited twice does on the JVM.
    (for-each
     (lambda (tag)
       (let ((h (hashtable-ref host-methods-tbl tag #f)))
         (when h
           (let-values (((names fns) (hashtable-entries h)))
             (vector-for-each (lambda (n f) (add! nm n f #f)) names fns)))))
     (jhost-tags-for-fqn fqn))
    (for-each (lambda (p) (add! nm (car p) (cdr p) #t)) (class-static-members nm #t))
    (make-jolt-array (list->vector (reverse acc)) 'objects)))
