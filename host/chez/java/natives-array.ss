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

;; The character a value denotes when it is STORED into a char array, or #f when
;; no character does — the test that decides whether a char array can keep its
;; string backing. A character is itself; an integer is the char it denotes,
;; which is aset-char's own rule on the JVM ((aset-char chars 0 65) puts \A
;; there). Reference Clojure's generic (aset chars 0 65) is an
;; IllegalArgumentException rather than that store, and (char-array [65]) a
;; ClassCastException; jolt accepts both as the character, here as it already
;; did in the char-array constructor, and this keeps the two spellings agreeing.
;;
;; A LONE SURROGATE (#xD800-#xDFFF) answers #f, and that is the one real gap
;; between the two representations. A JVM char is a UTF-16 CODE UNIT, so half a
;; surrogate pair is a legal char[] element; a Chez string holds Unicode SCALAR
;; values and integer->char refuses exactly that range. So an array that is
;; handed one falls back to a boxed vector (ja-promote-chars!) and goes on
;; holding the raw integer, which is what it did before char arrays had a string
;; backing. Nothing regresses — that case simply does not get the fast backing.
(define (na-char-scalar? n)
  (and (fixnum? n) (fx<=? 0 n #x10FFFF) (not (fx<=? #xD800 n #xDFFF))))
(define (na-char-of v)
  (cond ((char? v) v)
        ((na-char-scalar? v) (integer->char v))
        ;; a non-fixnum real truncates toward zero first, which is what the char
        ;; array constructor has always done with one ((char-array [65.0]) is
        ;; (\A)); a bignum or an out-of-range value answers #f and boxes.
        ((and (number? v) (real? v))
         (let ((n (guard (e (#t #f)) (exact (truncate v)))))
           (and (na-char-scalar? n) (integer->char n))))
        (else #f)))

;; --- backings ---------------------------------------------------------------
;; An element kind picks the Chez vector type that holds its elements UNBOXED:
;;
;;   double / float      flvector     (unboxed flonums)
;;   int / long / short   fxvector     (unboxed fixnums)
;;   byte                bytevector   (one signed byte per element)
;;   char / boolean / object          a boxed Chez vector
;;
;; The unboxed backings are not a memory saving alone — a bytevector is 8x
;; smaller than a vector of the same bytes, and the GC walks neither an fxvector
;; nor a bytevector at all, so a large numeric array stops being traced work on
;; every collection.
;;
;; EVERY accessor below dispatches on the BACKING, never on the kind, and a boxed
;; vector is a valid backing for any kind. That is what makes two things work:
;; the WIDENING below (ja-promote! — an int/long array handed a bignum swaps its
;; fxvector for a boxed vector and keeps going), and an array restored from an
;; image written before this change (a vector-backed byte array reads and writes
;; exactly as it did).
(define (na-fl-kind? k) (or (eq? k 'double) (eq? k 'float)))
;; int/long/short — the kinds whose elements are JVM integers, so a fixnum holds
;; one whenever jolt's own numeric tower keeps it a fixnum.
(define (na-fx-kind? k) (or (eq? k 'int) (eq? k 'long) (eq? k 'short)))

;; --- element access ---------------------------------------------------------
;; ja-ref / ja-set! / ja-len and the rest take the ARRAY, not the backing:
;; ja-set! may REPLACE the backing (the widening below), which needs the record
;; to write through. The two ja-backing-* here take the backing their caller
;; already read.
;;
;; MACROS, not procedures: these two are the innermost thing an element access
;; does, and at optimize-level 2 a call does not inline — a procedure here cost
;; ~2ns of an 8ns proven-^longs aget, which is the whole point of that path.
;;
;; The remaining tag test is what transparent widening costs: an int/long array is
;; an fxvector until a bignum store swaps in a boxed vector, so the read has to
;; ask which it is holding — about 1ns on a hinted aget, against a major
;; collection over a live 20M-element array falling from 45ms to 0.4ms because
;; the collector no longer walks it. (A ^doubles read still asks nothing: its
;; flvector is a promise that kind can keep.) fxvector is tested FIRST because
;; the hinted numeric read is the one that exists to be in a tight loop —
;; ^objects, whose backing is always a vector, pays the extra test instead.
(define-syntax ja-backing-len
  (syntax-rules ()
    ((_ b) (let ((v b))
             (cond ((fxvector? v) (fxvector-length v))
                   ((vector? v) (vector-length v))
                   ((string? v) (string-length v))
                   ((bytevector? v) (bytevector-length v))
                   (else (flvector-length v)))))))
(define (ja-len a) (ja-backing-len (jolt-array-vec a)))
;; alength in call position (op registry): the count of any array-like, but nil
;; is the NullPointerException the JVM raises rather than 0.
;; The message names what was read, as the reference's helpful NPE does
;; ("Cannot read the array length because \"xs\" is null"). An empty one reported
;; as "Unhandled exception (NullPointerException): " and stopped. Note that an
;; empty message is CORRECT for the NoSuchElementException sites elsewhere — the
;; JVM's is genuinely null there — so this is not a blanket rule.
(define (jolt-alength a)
  (if (jolt-nil? a)
      (throw-jvm 'NullPointerException "Cannot read the array length because the array is null")
      (jolt-count a)))
;; An out-of-range index on the generic aget/aset path throws the typed JVM
;; exception with the JVM message. The proven ^doubles fast path (jolt-flaget/
;; jolt-flaset below) and the unboxed jolt-vaget/jolt-vaset skip this pre-check —
;; they rely on the backing primitive's own range check, and its condition
;; classifies at inspection time instead (host-faults.ss array-index-whos).
(define (na-oob-throw i n)
  (jolt-throw (jolt-host-throwable "java.lang.ArrayIndexOutOfBoundsException"
                                   (format "Index ~a out of bounds for length ~a" i n))))
;; The bounds test takes the BACKING its caller already read: jolt-array-vec is a
;; checked record accessor (a type test per call), and reading it once per access
;; rather than once per helper is worth ~10% of an untyped aget.
(define (ja-check v i)
  (unless (and (fixnum? i) (fx>=? i 0) (fx<? i (ja-backing-len v)))
    (if (jolt-nil? i)
        (throw-jvm 'NullPointerException "array index is nil")
        (na-oob-throw i (ja-backing-len v)))))
(define-syntax ja-backing-ref
  (syntax-rules ()
    ((_ b k) (let ((v b) (i k))
               (cond ((fxvector? v) (fxvector-ref v i))
                     ((vector? v) (vector-ref v i))
                     ((string? v) (string-ref v i))
                     ((bytevector? v) (bytevector-s8-ref v i))
                     (else (flvector-ref v i)))))))
(define (ja-ref a i)
  (let ((v (jolt-array-vec a)))
    (ja-check v i)
    (ja-backing-ref v i)))
;; Widen an fxvector backing to a boxed vector, in place, and answer the new one.
;; THE one place an array's representation changes after construction: jolt's
;; integers promote to bignums past Chez's 2^60 fixnum ceiling (that is the whole
;; numeric model — see test/conformance/known-divergences.edn), so a long array
;; handed Long/MAX_VALUE has to keep it. Rather than refuse the store or lose the
;; value, the array widens once and behaves exactly as it did before this change.
;; Rare by construction: nothing that fits in a JVM int can trigger it.
(define (ja-promote! a)
  (let* ((v (jolt-array-vec a)) (n (fxvector-length v)) (w (make-vector n 0)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! w i (fxvector-ref v i)))
    (jolt-array-vec-set! a w)
    w))
;; The char backing's counterpart to ja-promote!, and it exists for the same
;; reason: a Chez string holds CHARACTERS, and jolt cannot stop a program from
;; storing something else in a char[]. The JVM cannot either — (aset chars 0 65)
;; compiles, because 65 is an int and int->char is a widening store — so this is
;; not a corner the type system rules out. A char store keeps the string; anything
;; else swaps in a boxed vector and the array behaves exactly as it did before
;; char arrays got a string backing.
;;
;; An INTEGER is the common non-char store and is NOT promoted: it is the JVM's
;; own char store, so it is narrowed to the character it denotes and the string
;; backing is kept. Promotion is for values a char[] could never hold on the JVM
;; at all (nil, a string, a record), which jolt tolerates rather than refuses.
(define (ja-promote-chars! a)
  (let* ((v (jolt-array-vec a)) (n (string-length v)) (w (make-vector n #\nul)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! w i (string-ref v i)))
    (jolt-array-vec-set! a w)
    w))
(define (ja-set! a i x)
  (let ((v (jolt-array-vec a)))
    (ja-check v i)
    (cond ((vector? v) (vector-set! v i x))
          ((string? v)
           (if (char? x)
               (string-set! v i x)
               (let ((c (na-char-of x)))
                 (if c (string-set! v i c) (vector-set! (ja-promote-chars! a) i x)))))
          ((fxvector? v)
           (if (fixnum? x) (fxvector-set! v i x) (vector-set! (ja-promote! a) i x)))
          ;; -128..127, whatever magnitude the caller spelled: na-byte-of is the
          ;; one narrowing seam and bytevector-s8-set! would refuse anything else.
          ;; A value already in range is stored as it is — the common case, and
          ;; na-byte-of's truncate/mask/fold is three operations to answer itself.
          ((bytevector? v)
           (bytevector-s8-set! v i (if (and (fixnum? x) (fx<=? -128 x 127)) x (na-byte-of x))))
          (else (flvector-set! v i (if (flonum? x) x (exact->inexact x)))))))
;; Element-wise equality over two arrays — java.util.Arrays/equals. Same backing
;; type on both sides is one `equal?` over the backing (a bytevector or fxvector
;; comparison is a block compare); a promoted array meeting an unpromoted one is
;; the mixed case, and takes the element walk rather than answering #f on a
;; representation difference the caller cannot see.
(define (ja-equal? a b)
  (let ((va (jolt-array-vec a)) (vb (jolt-array-vec b)))
    (if (or (and (vector? va) (vector? vb))
            (and (fxvector? va) (fxvector? vb))
            (and (string? va) (string? vb))
            (and (bytevector? va) (bytevector? vb))
            (and (flvector? va) (flvector? vb)))
        (equal? va vb)
        (let ((n (ja-backing-len va)))
          (and (fx=? n (ja-backing-len vb))
               (let loop ((i 0))
                 (or (fx=? i n)
                     (and (equal? (ja-backing-ref va i) (ja-backing-ref vb i))
                          (loop (fx+ i 1))))))))))
(define (ja->list a)
  (let ((v (jolt-array-vec a)))
    (cond ((vector? v) (vector->list v))
          ((fxvector? v) (fxvector->list v))
          ((string? v) (string->list v))
          (else (let loop ((i (fx- (ja-backing-len v) 1)) (acc '()))
                  (if (fx<? i 0) acc (loop (fx- i 1) (cons (ja-backing-ref v i) acc))))))))
;; A fresh backing holding the same elements — aclone's copy, and the one
;; java.util.Arrays hands its copyOf results.
(define (ja-copy a)
  (let ((v (jolt-array-vec a)))
    (cond ((vector? v) (vector-copy v))
          ((fxvector? v) (fxvector-copy v))
          ((string? v) (string-copy v))
          ((bytevector? v) (bytevector-copy v))
          (else (let* ((n (flvector-length v)) (r (make-flvector n 0.0)))
                  (do ((i 0 (fx+ i 1))) ((fx=? i n) r) (flvector-set! r i (flvector-ref v i))))))))
;; --- building a backing -----------------------------------------------------
;; An init value the kind's unboxed backing cannot hold — a bignum for an
;; fxvector, anything non-numeric — starts the array boxed rather than raising:
;; same rule as promotion, decided once at construction.
(define (na-make-backing n kind init)
  (let ((n (exact n)))
    (cond ((na-fl-kind? kind) (make-flvector n (if (flonum? init) init (exact->inexact init))))
          ((na-fx-kind? kind) (if (fixnum? init) (make-fxvector n init) (make-vector n init)))
          ((eq? kind 'byte) (make-bytevector n (na-byte-of init)))
          ;; A char array is a Chez STRING: the elements are characters and a
          ;; string is the carrier that holds them unboxed, exactly as an
          ;; fxvector holds an int array's. It also removes a conversion at
          ;; every seam a char[] actually meets — a reader fills one straight
          ;; from the port, (String. chars) is a substring of it — where a
          ;; boxed vector made each of those a per-character copy. An init the
          ;; backing cannot hold starts the array boxed, the same rule the
          ;; fxvector arm above follows.
          ((eq? kind 'char) (let ((c (na-char-of init)))
                              (if c (make-string n c) (make-vector n init))))
          (else (make-vector n init)))))
(define (na-list->backing lst kind)
  (cond ((na-fl-kind? kind)
         (let* ((n (length lst)) (fv (make-flvector n 0.0)))
           (let loop ((i 0) (l lst))
             (if (null? l) fv (begin (flvector-set! fv i (exact->inexact (car l))) (loop (+ i 1) (cdr l)))))))
        ((and (na-fx-kind? kind) (for-all fixnum? lst)) (list->fxvector lst))
        ;; every element coercible to a character, or the array starts boxed
        ((and (eq? kind 'char) (for-all (lambda (c) (na-char-of c)) lst))
         (list->string (map na-char-of lst)))
        ;; every element narrowed on the way in, so the seq of a byte array
        ;; agrees with what a raw-byte consumer reads out of it
        ((eq? kind 'byte)
         (let* ((n (length lst)) (bv (make-bytevector n)))
           (let loop ((i 0) (l lst))
             (if (null? l) bv (begin (bytevector-s8-set! bv i (na-byte-of (car l))) (loop (+ i 1) (cdr l)))))))
        (else (list->vector lst))))

;; Copy n elements from SRC at soff into DST at doff — the region move behind
;; System/arraycopy, a ByteBuffer bulk get/put and a heap slice. Two bytevector
;; backings are one block move (memmove-shaped, so an overlapping region inside
;; one byte array is correct without the backwards walk); everything else steps,
;; descending when the regions overlap forwards in the SAME backing, which is
;; what gives arraycopy's "as if through a temporary" without the temporary.
(define (ja-copy-range! src soff dst doff n)
  (let ((sv (jolt-array-vec src)) (dv (jolt-array-vec dst)))
    (cond
      ((and (bytevector? sv) (bytevector? dv)) (bytevector-copy! sv soff dv doff n))
      ;; string-copy! is specified to act as if through a temporary, so an
      ;; overlapping region inside one char array is correct without the
      ;; descending walk below — the same reason the bytevector arm is here.
      ((and (string? sv) (string? dv)) (sa-string-copy-range! dv doff sv soff (+ soff n)))
      ((and (eq? sv dv) (< soff doff))
       (let loop ((i (- n 1)))
         (when (>= i 0) (ja-set! dst (+ doff i) (ja-ref src (+ soff i))) (loop (- i 1)))))
      (else
       (let loop ((i 0))
         (when (< i n) (ja-set! dst (+ doff i) (ja-ref src (+ soff i))) (loop (+ i 1))))))))

;; --- the raw-bytes seam ------------------------------------------------------
;; A byte array and a Chez bytevector now hold their bytes the same way, so the
;; bridge in both directions is a block copy where it used to be an element loop
;; with a sign fold per byte. Both directions COPY: a bytevector handed to
;; na-bv->bytearray usually belongs to the caller (a port read buffer), and
;; na-bytearray->bv's result travels to decoders that must not see later writes.
;;
;; A boxed backing (a promoted array, or one restored from an older image) still
;; takes the loop — hence the pair of arms rather than a bare bytevector-copy.
(define (ja-bytes->bv! a off bv bvoff n)   ; byte array -> bytevector
  (let ((v (jolt-array-vec a)))
    (if (bytevector? v)
        (bytevector-copy! v off bv bvoff n)
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (bytevector-s8-set! bv (fx+ bvoff i) (na-byte-of (ja-backing-ref v (fx+ off i))))))))
(define (ja-bv->bytes! bv bvoff a off n)   ; bytevector -> byte array
  (let ((v (jolt-array-vec a)))
    (if (bytevector? v)
        (bytevector-copy! bv bvoff v off n)
        (do ((i 0 (fx+ i 1))) ((fx=? i n))
          (vector-set! v (fx+ off i) (na-u8->byte (bytevector-u8-ref bv (fx+ bvoff i))))))))

;; A byte array's elements are signed-byte-folded by na-list->backing, the same
;; coercion aset applies through na-elem-of — so (into-array Byte/TYPE …) stores
;; bytes rather than whatever magnitude it was handed.
(define (na-from-seq x kind)
  (make-jolt-array (na-list->backing (seq->list (jolt-seq x)) kind) kind))
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
;; The fixnum arm is the same answer by a shorter route, not a second rule: a
;; fixnum IS its own (exact (truncate v)), so all that is left of the general
;; case is the mask and the fold. Worth splitting because the general case spends
;; three GENERIC (bignum-capable) operations to discover that — 14.5ns — and every
;; door into a byte array comes through here: into-array, Arrays/fill,
;; na-list->backing, and the untyped aset.
(define (na-byte-of v)
  (if (fixnum? v)
      (na-u8->byte (fxand v #xff))
      (na-u8->byte (bitwise-and (exact (truncate v)) #xff))))
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
    ;; .toCharArray is now a COPY OF THE STRING, not a walk over it: the backing
    ;; and the source are the same representation, so one string-copy replaces a
    ;; character-at-a-time fill (and, before that, a cons per character plus a
    ;; second walk over the list).
    ((string? a) (make-jolt-array (string-copy a) 'char))
    ;; (char-array n) — make-string rather than make-vector. This is the whole of
    ;; the ~140x allocation gap: a boxed vector of n characters is n POINTERS the
    ;; collector must trace on every major GC, while a string of n characters is
    ;; a flat, untraced block half the size. It still initializes every element
    ;; (Chez has no lazily-zeroed allocation the way the JVM's new char[n] gets
    ;; zero pages from the OS), so parity is not the claim — but the cost falls
    ;; to what filling a string costs.
    ((number? a) (make-jolt-array (make-string (exact (na-idx a)) #\nul) 'char))
    ;; (char-array coll) coerces STRICTLY, and keeps doing so: a character is
    ;; itself, a number is the character at that code point, and anything else
    ;; raises out of truncate — which is what this constructor has always done
    ;; and what the JVM does (a ClassCastException). That is deliberately NOT
    ;; na-list->backing's rule: into-array is the lenient door and boxes instead,
    ;; also as before. Routing this through na-list->backing would silently turn
    ;; (char-array [:k]) from an error into a boxed array holding :k.
    (else (make-jolt-array
           (list->string (map (lambda (c) (if (char? c) c (integer->char (exact (truncate c)))))
                              (seq->list (jolt-seq a))))
           'char))))
;; Chez bytevector -> jolt byte-array. The inbound half of the raw-bytes seam:
;; every producer of a byte-array from raw bytes — .getBytes, stream reads,
;; Files/readAllBytes, Base64, FFI — funnels through here. One block copy now
;; that the two carriers agree on representation; the copy stays because the
;; caller's bytevector is usually a buffer it goes on writing into.
(define (na-bv->bytearray bv) (make-jolt-array (bytevector-copy bv) 'byte))
;; (byte-array n [init]) | (byte-array coll). Also coerces the host's OTHER byte
;; carrier — a Chez bytevector (what the charset encoders produce) — and a string's
;; UTF-8 bytes, so bytevector and byte-array interconvert across interop seams.
(define (na-byte-array a . rest)
  (cond
    ((number? a) (make-jolt-array (na-make-backing (na-idx a) 'byte (if (pair? rest) (car rest) 0)) 'byte))
    ((bytevector? a) (na-bv->bytearray a))
    ((string? a) (na-bv->bytearray (string->utf8 a)))
    (else (na-from-seq a 'byte))))
;; jolt byte-array -> Chez bytevector (for String decode / utf8->string), the
;; outbound half of the same seam.
(define (na-bytearray->bv arr)
  (let* ((n (ja-len arr)) (bv (make-bytevector n)))
    (ja-bytes->bv! arr 0 bv 0 n)
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
      (make-jolt-array (ja-copy arr) (jolt-array-kind arr))
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
  (let ((b (na-byte-of v)))
    (ja-set! arr (exact (na-idx i)) b) b))

;; --- coercions (identity on arrays; byte/short are masked scalar casts) ------
(define (na-bytes x) (if (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) x (na-byte-array x)))
(define (na-bytes? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)))
(define (na-identity x) x)
(define (na-byte x) (jolt-byte-cast x))
(define (na-short x) (jolt-short-cast x))

;; (The chunked-seq builder API — chunk-buffer / chunk-append / chunk /
;; chunk-cons and the accessor bindings — lives in natives-transduce.ss: it is
;; seq machinery, not arrays, and that file is shared with the Gambit boot.)

;; --- extend the collection dispatchers to see a jolt-array ------------------
(register-count-arm! jolt-array? (lambda (c) (ja-len c)))
(register-seq-arm! jolt-array?
  (lambda (c) (list->cseq/k (ja->list c) (na-seq-kind c))))
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
                 (if (jolt-array? c) (ja-ref c (exact (na-idx i))) (%na-nth c i))))
    ((c i d) (if (pvec? c)
                 (begin (jolt-nth-nil-idx! i) (pvec-nth-d c i d))
                 (if (jolt-array? c)
                     (let ((j (exact (na-idx i))))
                       (if (and (>= j 0) (< j (ja-len c))) (ja-ref c j) d))
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
;; The index takes the fixnum-first route jolt-vaget/jolt-flaget take: (exact
;; (na-idx k)) on a fixnum is two procedure calls to answer k itself, 9ns of an
;; untyped store.
(define (na-array-set! a k v)
  (let ((sv (na-elem-of (jolt-array-kind a) v)))
    (ja-set! a (if (fixnum? k) k (exact (na-idx k))) sv) sv))
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

;; The NON-flvector counterparts, for (aget ^longs a i) / (aset ^ints a i v) /
;; (aget ^bytes a i) / (aget ^objects a i). What they skip is the DISPATCH: an
;; untyped (aget a i) lowers to jolt-nth, which nil-checks the index, coerces it,
;; and then walks pvec?/string?/cseq?/rec-coll-method before it reaches the array
;; arm. What they cannot skip is the backing test — a long array is an fxvector
;; until a bignum store promotes it (ja-promote!), so the read has to ask.
;;
;; No result-type promise, unlike the flvector pair: an fxvector element IS a
;; fixnum, but a promoted array's is whatever integer was stored, so an element
;; is still not provably one and the numeric pass must not type it :long.
;;
;; 'byte reads here but writes through jolt-baset below: its elements are signed
;; 8-bit, and a store has to narrow to that range and answer what it stored,
;; neither of which this pair does. Reads need neither — the narrowing already
;; happened at the store.
;;
;; The unboxed backings need no index pre-check: their own range check IS the
;; array bounds contract here, exactly as on the ^doubles path, and host-faults.ss
;; classifies an fxvector/bytevector-s8 condition as an
;; ArrayIndexOutOfBoundsException.
;;
;; A BOXED backing is the exception, and it has to pre-check. A plain Chez vector
;; is what the runtime uses for everything, so vector-ref's range condition
;; carries nothing to tell an array apart by and cannot join that list — which
;; left (aget ^objects a oob) raising the PARENT IndexOutOfBoundsException while
;; the same read without the hint raised the array class. A hint must not decide
;; which exception a program catches. The pre-check is one fixnum compare, ~0.4ns
;; of a 5.6ns read: 4% of what the hint buys (an untyped read of the same loop is
;; 2.4x slower than the checked hinted one), which is the right side of that
;; trade. It is on the boxed arm only, so ^longs/^bytes/^doubles pay nothing.
(define (jolt-vaget a i)
  (let ((v (jolt-array-vec a)) (j (if (fixnum? i) i (exact (na-idx i)))))
    (cond ((fxvector? v) (fxvector-ref v j))
          ((vector? v) (if (and (fixnum? j) (fx<? -1 j (vector-length v)))
                           (vector-ref v j)
                           (na-oob-throw j (vector-length v))))
          ((bytevector? v) (bytevector-s8-ref v j))
          ;; A char array has a STRING backing and no hint of its own (:chars is
          ;; not a boxed-akind), so reaching here means a LYING hint — ^objects
          ;; on a char[]. Take the checked generic path rather than a raw
          ;; string-ref, whose range condition host-faults.ss cannot tell from
          ;; any other string operation's.
          ((string? v) (ja-ref a j))
          (else (flvector-ref v j)))))
(define (jolt-vaset a i v)
  (let ((bk (jolt-array-vec a)) (j (if (fixnum? i) i (exact (na-idx i)))))
    (cond ((fxvector? bk)
           (if (fixnum? v) (fxvector-set! bk j v) (vector-set! (ja-promote! a) j v)))
          ((vector? bk)
           (if (and (fixnum? j) (fx<? -1 j (vector-length bk)))
               (vector-set! bk j v)
               (na-oob-throw j (vector-length bk))))
          ;; a lying hint over a string-backed char array — see jolt-vaget. The
          ;; else arm used to assume a boxed vector and would read
          ;; (vector-length bk) off a string.
          (else (ja-set! a j v)))
    v))

;; (aset ^bytes a i v) — the byte kind's own store target, split from jolt-vaset
;; rather than folded into it because a byte array is the one kind whose store
;; NARROWS: na-elem-of folds the value to signed 8 bits and na-array-set! answers
;; what was actually stored, so a helper that returned its argument the way
;; jolt-vaset does would disagree with the following aget.
;;
;; What it skips is the generic seam's walk to the same bytevector-s8-set!:
;; jolt-aset3's array test, the kind read and the eq? on it, na-byte-of's
;; truncate/exact/bitwise-and over the numeric tower, the index coercion, then
;; ja-set!'s SECOND read of the backing, ja-check, and a four-way cond — ~34ns of
;; a 40ns store, against 7ns here.
;;
;; The guard is the whole contract. A fixnum already inside -128..127 is what
;; na-byte-of answers for it (mask to 0..255, fold the high half back), so storing
;; it directly is the same value by a shorter route. Everything else — a flonum, a
;; bignum, a value out of range, a non-fixnum index, a byte array whose backing an
;; older image left boxed, and a LYING ^bytes hint on an array of any other kind —
;; falls to na-array-set!. That fallback is not a slow path bolted on for this
;; helper; it is the generic seam every unhinted aset takes, which is why the
;; narrowing contract here is exactly the one it has always had.
;;
;; No index pre-check: bytevector-s8-set!'s own range check IS the array bounds
;; contract here, as on the ^longs and ^doubles paths, and host-faults.ss already
;; classifies a bytevector-s8-set! condition as an ArrayIndexOutOfBoundsException.
;; A non-array receiver raises out of jolt-array-vec, exactly as it does for
;; jolt-vaget/jolt-vaset.
(define (jolt-baset a i v)
  (let ((bk (jolt-array-vec a)))
    (if (and (bytevector? bk) (fixnum? i) (fixnum? v) (fx<=? -128 v 127))
        (begin (bytevector-s8-set! bk i v) v)
        (na-array-set! a i v))))

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
;; Only a CHAR array: this used to take every array, so a byte[] — a
;; ByteArrayInputStream decoded as UTF-8 on the JVM — read as the text of its
;; element values ((line-seq (io/reader (.getBytes "a\nb"))) was ("971098")).
;; Every other array kind goes to jolt-io-reader, whose byte[] arm decodes it and
;; which refuses the rest the way the JVM does.
(def-var! "clojure.java.io" "reader"
  (lambda (x)
    (if (and (jolt-array? x) (eq? (jolt-array-kind x) 'char))
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
    (cons "chars" na-identity) (cons "byte" na-byte) (cons "short" na-short)))

;; --- clojure.java.io/copy ---------------------------------------------------
;; Copy src -> dst, JVM-style. Raw bytes (byte-array / bytevector / string) and a
;; jhost reader write in one shot; any other source (a stream shim with a .read
;; method, e.g. jolt-lang/http-client's ByteArrayInputStream) drains via .read
;; into a byte-array buffer and .write to dst — both reached through method
;; dispatch, so a library's tagged-table streams work without the host knowing
;; their layout. Lives here (not io.ss) because io.ss loads before byte-array.
(define (jolt-io-copy src dst . _opts)
  (define (write-all! bytes)
    (record-method-dispatch dst "write" (list->cseq (list bytes 0 (ja-len bytes)))))
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
;; is what make-array already does, and malli's own :cljs branch spells the same
;; call (object-array capacity). Like make-array, the component type
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
        (cons "getLength" (lambda (arr) (->num (ja-len (na-need-array arr)))))
        (cons "get" (lambda (arr i) (ja-ref (na-need-array arr) (na-idx i))))
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
         (ja-len (car params)))
        ((and (null? (cdr params)) (or (pvec? (car params)) (cseq? (car params))))
         (length (seq->list (jolt-seq (car params)))))
        (else (length params))))
(define (class-find-method cls name params)
  (let* ((want (jreflect-arity params))
         (ms (ja->list (class-method-array cls)))
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
  (let ((st (jhost-state self)))
    ;; A method built from a dispatch rule rather than a proc cannot report its
    ;; parameter count off an arity mask (the rule accepts anything), so it
    ;; pins the count in a fifth slot. The members jolt enumerates keep four
    ;; slots and derive the count from the proc, as before.
    (if (and (fx>? (vector-length st) 4) (not (eq? #f (vector-ref st 4))))
        (vector-ref st 4)
        (reflect-method-param-count (reflect-method-fn self) (reflect-method-static? self)))))
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
        ;; splatted. A lone nil is the EMPTY argument list: sci.impl.reflector's
        ;; box-args answers nil for a method with no parameters, and the JVM's
        ;; Method.invoke treats a null array as none either (it would raise). A
        ;; caller spelling out arguments passes them straight through.
        (cons "invoke"
              (lambda (self target . args)
                (let ((as (if (and (= 1 (length args)) (jolt-array? (car args)))
                              (ja->list (car args))
                              (if (and (= 1 (length args)) (jolt-nil? (car args)))
                                  '()
                                  args))))
                  (if (reflect-method-static? self)
                      (apply (reflect-method-fn self) as)
                      (apply (reflect-method-fn self) target as)))))
        ;; AccessibleObject.canAccess: jolt's members carry no accessibility
        ;; state, and every method it reports is public, so a reflective caller
        ;; that asks (SCI's interpreter is one) is allowed through.
        (cons "canAccess" (lambda (self target) #t))
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
    ;; The protocol registry's methods — but only the ones the type DECLARES.
    ;; An extend/extend-type/extend-protocol implementation is filed in the same
    ;; registry and is not a member of the class: the JVM's extend writes the
    ;; protocol's method table and leaves the class alone, so reflection over a
    ;; type that was merely extended reports nothing new (and never either of
    ;; the bookkeeping marks jolt files beside those methods).
    (when ti
      (let-values (((protos impls) (hashtable-entries ti)))
        (vector-for-each
         (lambda (proto pi)
           (unless (extend-impl-table? pi)
             (let-values (((names fns) (hashtable-entries pi)))
               (vector-for-each
                (lambda (n f)
                  (unless (or (string=? n extend-mark) (string=? n inline-mark))
                    (add! nm n f #f)))
                names fns))))
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

;; ---- Reflector/getMethods (SCI's reflective lookup) -------------------------
;; SCI's interpreter (sci.impl.reflector) resolves a method call by asking
;; clojure.lang.Reflector/getMethods for the members matching (name, arity,
;; static?) and then invoking the one it got. jolt records members as data for a
;; protocol's methods, a jhost tag's shims and a class's statics — those answer
;; with the real member. String and the collections model their methods as a
;; cond over the receiver, so nothing enumerates them: those (and any name the
;; class does not have, which dispatch reports at the call the way every other
;; jolt interop miss does) answer with a stand-in that carries the dispatch
;; RULE instead of a proc. Its arity cannot come off a proc, so it pins one.
(define (make-dispatch-method cls-name method-name arity static?)
  (make-jhost
   "reflect-method"
   (vector cls-name method-name
           (if static?
               (lambda args (apply host-static-call cls-name method-name args))
               (lambda (self . args)
                 (record-method-dispatch self method-name (list->cseq args))))
           static? arity)))

(define (reflect-get-methods cls arity method-name static?)
  (let* ((want (jnum->exact arity))
         (name (jolt-str-render-one method-name))
         (stat? (if (or (eq? static? #f) (jolt-nil? static?)) #f #t))
         (ms (filter (lambda (m)
                       (and (string=? (reflect-method-name m) name)
                            (fx=? want (reflect-method-arity m))
                            (eq? stat? (reflect-method-static? m))))
                     (ja->list (class-method-array cls)))))
    ;; A java.util.List, which is what SCI's caller destructures (.size/.get).
    (make-arraylist (list (if (pair? ms)
                              (car ms)
                              (make-dispatch-method (jclass-name cls) name want stat?))))))

(register-class-statics!
 "clojure.lang.Reflector"
 (list (cons "getMethods" reflect-get-methods)))
