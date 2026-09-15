;; JVM-compatible hash engine for Jolt — Gambit target (native gsi), safe ops.
;;
;; Safe-ops port of host/chez/hasheq.ss (PSL R10 target-owned file): the SAME
;; exported 14-procedure surface, the SAME murmur3 constants and algorithm,
;; expressed with plain R7RS/Gambit arithmetic (bitwise-and masks, arithmetic-
;; shift) instead of Chez's #3% fixnum tricks. Correctness first, tuning never
;; (demo target).
;;
;; The 14 exported procedures (the rest of the host calls exactly these):
;;   jolt-hasheq                    entry point: fast paths + arms + fallback
;;   murmur3-hash-long-flat         fixnum hashing (collections.ss key-hash)
;;   murmur3-hash-int               int hashing (java/host-static-methods.ss)
;;   murmur3-hash-long              long/bignum-in-range hashing
;;   murmur3-hash-unencoded-chars   string-char hashing (host-static-methods)
;;   big-integer-hashcode           bignum hash (java/bigdec.ss)
;;   mix-coll-hash                  collection combine (collections/records/...)
;;   hash-ordered                   seq hashing (seq.ss, reader.ss, ...)
;;   hash-unordered                 map/set hashing (natives-misc, static-methods)
;;   entry-hasheq                   (key . value) pair hash (collections/records)
;;   hash-combine                   combiner (natives-reader, io, natives-str)
;;   compute-keyword-hasheq         keyword hashing (values.ss)
;;   symbol-hasheq / compute-symbol-hasheq   symbol hashing (records.ss)
;; (string-hasheq, double-hasheq, jolt-hasheq-fallback and the caches stay
;; INTERNAL — reached only through jolt-hasheq.)
;;
;; HASH VALUE PARITY: jolt hash values must match the Chez build's — corpus
;; rows compare them in G2. gambitcheck pins this with known-answer rows
;; captured from the Chez build via bin/jolt (see the rows in gambitcheck.ss).
;;
;; Shift semantics: Gambit does not bind the R6RS bitwise-arithmetic-shift-*
;; names; arithmetic-shift with a NEGATIVE count is the arithmetic right shift
;; (floor semantics, identical to R6RS). urs32 masks back to unsigned 32 bits,
;; which is the Java >>> on a 32-bit value.

;; ============================================================================
;; 32-bit signed integer helpers — all macros (syntax-rules) so they inline at
;; every call site with zero procedure-call overhead.
;; ============================================================================

;; Mask to unsigned 32 bits (0 .. 2^32-1).
(define-syntax u32
  (syntax-rules ()
    ((_ x) (bitwise-and x #xFFFFFFFF))))

;; Interpret an unsigned 32-bit value as signed 32-bit (-2^31 .. 2^31-1).
;; Input must already be ≤ 32 bits (u32-masked); already-signed values pass
;; through unchanged — same semantics as the Chez i32.
(define-syntax i32
  (syntax-rules ()
    ((_ x) (let ((u (u32 x)))
             (if (>= u #x80000000) (- u #x100000000) u)))))

;; 32-bit wrapping multiply via a 16-bit split. Plain arithmetic throughout;
;; every intermediate is ≤ 2^48, far inside Gambit's 62-bit fixnum range.
(define-syntax mul32
  (syntax-rules ()
    ((_ a b)
     (let* ((a* (i32 a))
            (b* (i32 b))
            (hi (bitwise-and (arithmetic-shift b* -16) #xFFFF))
            (lo (bitwise-and b* #xFFFF))
            (hi-part (arithmetic-shift (bitwise-and (* a* hi) #xFFFF) 16))
            (lo-part (* a* lo)))
       (i32 (bitwise-and (+ hi-part lo-part) #xFFFFFFFF))))))

;; 32-bit wrapping add. a and b each evaluated once.
(define-syntax add32
  (syntax-rules ()
    ((_ a b) (i32 (+ (i32 a) (i32 b))))))

;; Unsigned right shift (Java >>>): mask to unsigned 32 FIRST, then shift —
;; shifting a negative value first drags infinite sign bits through the mask
;; (that ordering bug corrupted every hash). Same order as the Chez build.
(define-syntax urs32
  (syntax-rules ()
    ((_ x n) (arithmetic-shift (u32 x) (- n)))))

;; Rotate left (Java Integer.rotateLeft). x and n each evaluated once.
(define-syntax rotl32
  (syntax-rules ()
    ((_ x n)
     (let ((n* (remainder n 32))
           (x* x))
       (i32 (bitwise-ior (arithmetic-shift (u32 x*) n*)
                         (urs32 x* (- 32 n*))))))))

;; ============================================================================
;; Murmur3 — exact port of clojure.lang.Murmur3.
;; ============================================================================

(define murmur3-seed 0)
(define murmur3-C1   #xcc9e2d51)   ;; -862048943
(define murmur3-C2   #x1b873593)   ;; 461845907

(define (murmur3-mix-k1 k1)
  (let* ((k1 (mul32 k1 murmur3-C1))
         (k1 (rotl32 k1 15))
         (k1 (mul32 k1 murmur3-C2)))
    k1))

(define (murmur3-mix-h1 h1 k1)
  (let* ((h1 (bitwise-xor h1 k1))
         (h1 (rotl32 h1 13))
         (h1 (add32 (mul32 h1 5) #xe6546b64)))
    h1))

(define (murmur3-fmix h1 len)
  (let* ((h1 (bitwise-xor h1 len))
         (h1 (bitwise-xor h1 (urs32 h1 16)))
         (h1 (mul32 h1 #x85ebca6b))
         (h1 (bitwise-xor h1 (urs32 h1 13)))
         (h1 (mul32 h1 #xc2b2ae35))
         (h1 (bitwise-xor h1 (urs32 h1 16))))
    h1))

;; Flat-inlined murmur3-hash-long for fixnums (Java Long.hasheq: two 32-bit
;; halves, count=8; input 0 → 0).
(define (murmur3-hash-long-flat input)
  (if (= input 0) 0
      (let* ((low (i32 input))
             (high (i32 (arithmetic-shift input -32)))
             ;; --- mixK1(low): mul32(low, C1) ---
             (k1 (mul32 low murmur3-C1))
             (k1 (rotl32 k1 15))
             (k1 (mul32 k1 murmur3-C2))
             ;; --- mixH1(seed, k1) ---
             (h1 (bitwise-xor murmur3-seed k1))
             (h1 (rotl32 h1 13))
             (h1 (add32 (mul32 h1 5) #xe6546b64))
             ;; --- mixK1(high) ---
             (k1 (mul32 high murmur3-C1))
             (k1 (rotl32 k1 15))
             (k1 (mul32 k1 murmur3-C2))
             ;; --- mixH1(h1 from low, k1 from high) ---
             (h1 (bitwise-xor h1 k1))
             (h1 (rotl32 h1 13))
             (h1 (add32 (mul32 h1 5) #xe6546b64))
             ;; --- fmix(h1, 8) ---
             (h1 (bitwise-xor h1 8))
             (h1 (bitwise-xor h1 (urs32 h1 16)))
             (h1 (mul32 h1 #x85ebca6b))
             (h1 (bitwise-xor h1 (urs32 h1 13)))
             (h1 (mul32 h1 #xc2b2ae35)))
        (i32 (bitwise-xor h1 (urs32 h1 16))))))

;; murmur3-hash-int for int32-range values (Java Murmur3.hashInt, count=4).
(define (murmur3-hash-int input)
  (if (= (i32 input) 0) 0
      (let* ((k1 (murmur3-mix-k1 (i32 input)))
             (h1 (murmur3-mix-h1 murmur3-seed k1)))
        (murmur3-fmix h1 4))))

;; murmur3-hash-long for any exact integer within 64 bits; the >64-bit bignum
;; fallback keeps the low 64 bits (Java long semantics) before mixing.
(define (murmur3-hash-long input)
  (if (= input 0) 0
      (if (fixnum? input)
          (murmur3-hash-long-flat input)
          (let* ((u64 (bitwise-and input #xFFFFFFFFFFFFFFFF))
                 (low (i32 u64))
                 (high (i32 (arithmetic-shift u64 -32)))
                 (k1 (murmur3-mix-k1 low))
                 (h1 (murmur3-mix-h1 murmur3-seed k1))
                 (k1 (murmur3-mix-k1 high))
                 (h1 (murmur3-mix-h1 h1 k1)))
            (murmur3-fmix h1 8)))))

;; ============================================================================
;; String hash — Java String.hashCode() over UTF-16 code units.
;; ============================================================================

;; Java String.hashCode(): s[0]*31^(n-1) + ... + s[n-1] with int32 wrapping at
;; every step (the i32 wrap is exact: + and * commute with mod 2^32). Iterates
;; the string's codepoints directly (jolt/Gambit strings hold astral chars as a
;; single position), computing surrogate pairs inline for codepoints >= #x10000.
(define (java-string-hashcode s)
  (let ((len (string-length s)))
    (let loop ((i 0) (h 0))
      (if (>= i len)
          (i32 h)
          (let ((cp (char->integer (string-ref s i))))
            (if (< cp #x10000)
                (loop (+ i 1) (i32 (+ (* 31 h) cp)))
                (let* ((cp2 (- cp #x10000))
                       (high (bitwise-ior #xD800 (arithmetic-shift cp2 -10)))
                       (low  (bitwise-ior #xDC00 (bitwise-and cp2 #x3FF))))
                  (let ((h* (i32 (+ (* 31 h) high))))
                    (loop (+ i 1) (i32 (+ (* 31 h*) low)))))))))))

(define (murmur3-hash-unencoded-chars s)
  ;; Java's Murmur3.hashUnencodedChars(CharSequence) over the UTF-16 code-unit
  ;; sequence: processes 2 code units at a time, mixing each pair as
  ;; k1 = c1 | (c2 << 16); a single trailing unit is mixed alone. Count tracks
  ;; complete code units so the final fmix length is 2 * total code units.
  (let ((len (string-length s)))
    (let loop ((i 0) (h1 murmur3-seed) (pending #f) (count 0))
      (if (>= i len)
          (if pending
              ;; One unpaired unit left — mix and finalize
              (let* ((k1 (murmur3-mix-k1 pending))
                     (h1 (bitwise-xor h1 k1)))
                (murmur3-fmix h1 (* 2 (+ count 1))))
              (murmur3-fmix h1 (* 2 count)))
          (let ((cp (char->integer (string-ref s i))))
            (if (< cp #x10000)
                ;; BMP: one code unit
                (if pending
                    ;; Pair pending + this unit; both consumed
                    (let* ((k1 (murmur3-mix-k1
                                (bitwise-ior pending
                                             (arithmetic-shift cp 16))))
                           (h1 (murmur3-mix-h1 h1 k1)))
                      (loop (+ i 1) h1 #f (+ count 2)))
                    ;; Hold as pending (not counted yet)
                    (loop (+ i 1) h1 cp count))
                ;; Astral: surrogate pair (high, low) — always 2 units
                (let* ((cp2 (- cp #x10000))
                       (high (bitwise-ior #xD800 (arithmetic-shift cp2 -10)))
                       (low  (bitwise-ior #xDC00 (bitwise-and cp2 #x3FF))))
                  (if pending
                      ;; Pair pending + high (consumed), low becomes new pending
                      (let* ((k1 (murmur3-mix-k1
                                  (bitwise-ior pending
                                               (arithmetic-shift high 16))))
                             (h1 (murmur3-mix-h1 h1 k1)))
                        (loop (+ i 1) h1 low (+ count 2)))
                      ;; High + low consumed together
                      (let* ((k1 (murmur3-mix-k1
                                  (bitwise-ior high
                                               (arithmetic-shift low 16))))
                             (h1 (murmur3-mix-h1 h1 k1)))
                        (loop (+ i 1) h1 #f (+ count 2)))))))))))

;; ============================================================================
;; Bignum / double hashing.
;; ============================================================================

;; Java BigInteger.hashCode over the magnitude limbs (big-endian 32-bit):
;; h = 31*h + limb with int32 wrapping, then * signum.
(define (big-integer-hashcode x)
  (let* ((signum (cond ((< x 0) -1) ((= x 0) 0) (else 1)))
         (mag (abs x)))
    (if (= mag 0)
        0
        (let ((nbits (integer-length mag)))
          (let* ((nlimbs (+ (quotient (- nbits 1) 32) 1))
                 (shift0 (* (- nlimbs 1) 32)))
            (let loop ((i 0) (h 0) (shift shift0))
              (if (>= i nlimbs)
                  (* h signum)
                  (let ((limb (u32 (arithmetic-shift mag (- shift)))))
                    (loop (+ i 1) (i32 (+ (* 31 h) limb)) (- shift 32))))))))))

;; Extract the 64-bit IEEE-754 bit pattern of a double. Gambit binds no R6RS
;; bytevector-ieee accessors; ##flonum->ieee754-64 returns the pattern
;; directly as an exact integer (probed 4.9.7: 1.5 -> #x3FF8000000000000).
(define (double-to-raw-bits x)
  (##flonum->ieee754-64 x))

;; Double hasheq — Numbers.hasheq for Double.class: ±0.0 → 0 (Gambit's fl=?
;; equates 0.0 and -0.0, so both hit the special case, matching the Chez
;; build), else Double.hashCode = (int)(bits ^ (bits >>> 32)).
(define (double-hasheq x)
  (if (and (flonum? x) (fl=? x 0.0) (fl=? (fl/ x 1.0) -0.0))
      0
      (let ((bits (double-to-raw-bits x)))
        (i32 (bitwise-xor bits (arithmetic-shift bits -32))))))

;; ============================================================================
;; Collection hash mixers — exact ports of the Murmur3 Java methods.
;; ============================================================================

(define (mix-coll-hash hash count)
  (let* ((k1 (murmur3-mix-k1 hash))
         (h1 (murmur3-mix-h1 murmur3-seed k1)))
    (murmur3-fmix h1 count)))

;; hash-ordered and hash-unordered operate over a Jolt seq (cseq/nil).
;; Called from seq.ss (seq-hash) and collections.ss (jolt-coll-hash).

(define (hash-ordered xs)
  (let loop ((xs xs) (n 0) (h 1))
    (if (jolt-nil? xs)
        (mix-coll-hash h n)
        (loop (jolt-seq (seq-more xs))
              (+ n 1)
              (i32 (+ (* 31 h) (jolt-hasheq (seq-first xs))))))))

;; hash-ordered of a 2-element sequence [k v] — MapEntry-as-vector on the JVM.
(define (entry-hasheq k v)
  (let* ((h1 (i32 (+ 31 (jolt-hasheq k))))
         (h2 (i32 (+ (* 31 h1) (jolt-hasheq v)))))
    (mix-coll-hash h2 2)))

(define (hash-unordered xs)
  (let loop ((xs xs) (n 0) (h 0))
    (if (jolt-nil? xs)
        (mix-coll-hash h n)
        (let ((e (seq-first xs)))
          (loop (jolt-seq (seq-more xs))
                (+ n 1)
                ;; add32: APersistentMap.mapHasheq/APersistentSet.setHasheq sum
                ;; in a Java int (32-bit wrap), matching the native pmap/pset
                ;; paths — so a custom coll that hashes via hash-unordered hashes
                ;; equal to a plain map/set with the same elements.
                (add32 h (if (pair? e)
                             (entry-hasheq (car e) (cdr e))
                             (jolt-hasheq e))))))))

;; ============================================================================
;; Util.hashCombine — exact port of clojure.lang.Util.hashCombine.
;; ============================================================================

(define (hash-combine seed hash)
  ;; a la boost: seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2)
  ;; Java's >> is arithmetic (sign-extending), NOT >>> (logical/unsigned).
  (let* ((seed (i32 seed))
         (hash (i32 hash))
         (sl   (i32 (arithmetic-shift seed 6)))
         (sr   (arithmetic-shift seed -2))
         (sum  (i32 (+ (i32 (+ hash #x9e3779b9)) (i32 (+ sl sr)))))
         (result (bitwise-xor seed sum)))
    (i32 result)))

;; ============================================================================
;; Keyword / Symbol hasheq (mirrors Keyword.java / Symbol.java).
;; ============================================================================

;; Keyword hasheq = symbol.hasheq() + 0x9e3779b9. Stored in the keyword-t's
;; khash field at construction time (values.ss).
(define (compute-keyword-hasheq ns name)
  ;; sym.hasheq() = hashCombine(murmur3.hashUnencodedChars(name), hash(ns))
  ;; hash(ns) = ns.hashCode() = java-string-hashcode(ns) or 0 if absent.
  ;; Then keyword hasheq = sym.hasheq() + 0x9e3779b9.
  (let ((ns-hash (if (or (not ns) (eq? ns '()))
                     0
                     (java-string-hashcode ns))))
    (i32 (+ (hash-combine (murmur3-hash-unencoded-chars name) ns-hash)
            #x9e3779b9))))

;; Symbol hasheq = Util.hashCombine(Murmur3.hashUnencodedChars(name), hash(ns)).
;; STRONG table (not weak like Chez): the js target's weak tables spin on
;; symbol/string keys (WeakRef cannot hold primitive-likes — the lookup hung
;; under node). Symbols and strings are program vocabulary; the caches are
;; bounded by it, so strength costs little.
(define symbol-hasheq-cache (make-eq-hashtable))

(define (compute-symbol-hasheq ns name)
  (let ((ns-hash (if (or (jolt-nil? ns) (not ns) (eq? ns '()))
                     0
                     (java-string-hashcode ns))))
    (hash-combine (murmur3-hash-unencoded-chars name) ns-hash)))

(define (symbol-hasheq sym)
  (or (hashtable-ref symbol-hasheq-cache sym #f)
      (let ((h (compute-symbol-hasheq (symbol-t-ns sym) (symbol-t-name sym))))
        (hashtable-set! symbol-hasheq-cache sym h)
        h)))

;; String hasheq cache — same pattern (and the same strong-table reason).
(define string-hasheq-cache (make-eq-hashtable))

(define (compute-string-hasheq s)
  (murmur3-hash-int (java-string-hashcode s)))

(define (string-hasheq s)
  (or (hashtable-ref string-hasheq-cache s #f)
      (let ((h (compute-string-hasheq s)))
        (hashtable-set! string-hasheq-cache s h)
        h)))

;; ============================================================================
;; identity hashes and the per-object caches (mirrors host/chez/hasheq.ss)
;; ============================================================================

;; A weak-eq side table hands each procedure (and each plain deftype instance —
;; records-coll.ss) a murmured id on first hash; see the Chez file for why it is
;; a table and not an address hash. jolt-identity-hasheq-seed! pins a deftype
;; token's hash to its Class's before anything asks (records.ss
;; deftype-ctor-tag-set!, a shared file — which is why these are here).
(define proc-hasheq-tbl (make-weak-eq-hashtable))
(define proc-hasheq-mu (make-mutex))
(define proc-hasheq-counter 0)
(define (jolt-identity-hasheq p)
  (jolt-with-mutex proc-hasheq-mu
    (or (hashtable-ref proc-hasheq-tbl p #f)
        (begin
          (set! proc-hasheq-counter (fx+ proc-hasheq-counter 1))
          (let ((h (murmur3-hash-long-flat proc-hasheq-counter)))
            (hashtable-set! proc-hasheq-tbl p h)
            h)))))
(define (procedure-hasheq p) (jolt-identity-hasheq p))
(define (jolt-identity-hasheq-seed! p h)
  (jolt-with-mutex proc-hasheq-mu
    (unless (hashtable-ref proc-hasheq-tbl p #f)
      (hashtable-set! proc-hasheq-tbl p h))))

;; pvec hasheq cached in the record's field, computed by leaf runs with no seq
;; cells (collections.ss's jolt-coll-hash reads this). Plain arithmetic where
;; the Chez copy uses its hash-fx operators — the documented per-host split.
(define (pvec-hasheq-cached p)
  (let ((c (pvec-hasheq p)))
    (if (and (fixnum? c) (not (= c 0)))
        c
        (let ((n (pvec-count p)))
          (let loop ((i 0) (h 1))
            (if (= i n)
                (let ((r (mix-coll-hash h n)))
                  (pvec-hasheq-set! p r)
                  r)
                (let-values (((leaf off) (pv-leaf-for p i)))
                  (let ((run (min (- (vector-length leaf) off) (- n i))))
                    (let cloop ((j 0) (h h))
                      (if (>= j run)
                          (loop (+ i run) h)
                          (cloop (+ j 1)
                                 (i32 (+ (* 31 h)
                                         (jolt-hasheq (vector-ref leaf (+ off j))))))))))))))))

;; Seq hasheq, cached per HEAD object — the JVM's ASeq._hasheq. Realized cells
;; never change, so the ordered hash of a head is fixed once computed.
(define seq-hasheq-tbl (make-weak-eq-hashtable))
(define seq-hasheq-mu (make-mutex))
(define (seq-hasheq-cached x)
  (if (or (cseq? x) (jolt-lazyseq? x))
      (or (jolt-with-mutex seq-hasheq-mu (hashtable-ref seq-hasheq-tbl x #f))
          (let ((h (hash-ordered (jolt-seq x))))
            (jolt-with-mutex seq-hasheq-mu (hashtable-set! seq-hasheq-tbl x h))
            h))
      (hash-ordered (jolt-seq x))))

;; ============================================================================
;; jolt-hasheq — the top-level dispatch (mirrors Util.hasheq).
;; ============================================================================

;; Per-type arms registered by host shims (records, dates, etc.). An arm is
;; (pred . handler); pred takes the value, handler returns int.
(define jolt-hasheq-arms '())

;; Dispatch: fast-path types first, then registered arms, then fallback.
(define (jolt-hasheq x)
  (cond
    ((jolt-nil? x) 0)
    ((keyword? x) (keyword-t-khash x))
    ;; Fixnum: all fixnums use hashLong (count=8) matching JVM's Long.hasheq.
    ((fixnum? x) (murmur3-hash-long-flat x))
    ((string? x) (string-hasheq x))
    ;; A procedure hashes by identity, ahead of the arm walk as on Chez: a
    ;; deftype token's seeded hash (deftype-ctor-tag-set!) is read here.
    ((procedure? x) (procedure-hasheq x))
    (else
     ;; New hasheq arms (jrec via records.ss, etc.)
     (let loop ((as jolt-hasheq-arms))
       (cond ((null? as)
              ;; Fall through to old jolt-hash arms (backward-compat for types
              ;; that still register via register-hash-arm!).
              (let loop2 ((bs jolt-hash-arms))
                (cond ((null? bs) (jolt-hasheq-fallback x))
                      (((caar bs) x) (i32 ((cdar bs) x)))
                      (else (loop2 (cdr bs))))))
             (((caar as) x) ((cdar as) x))
             (else (loop (cdr as))))))))

(define (jolt-hasheq-fallback x)
  ;; All types not covered by the fast path or arms.
  ;; Mirrors Util.hasheq: Number → Numbers.hasheq,
  ;; IHashEq → .hasheq(), else .hashCode().
  (cond
    ;; Numbers (excluding fixnums, already handled in fast path)
    ((number? x)
     (cond
       ((flonum? x) (double-hasheq x))
       ;; Ratio: exact non-integer. hasheq = BigInteger.hashCode(numer) ^
       ;; BigInteger.hashCode(denom)
       ((and (exact? x) (not (integer? x)))
        (i32 (bitwise-xor (big-integer-hashcode (numerator x))
                          (big-integer-hashcode (denominator x)))))
       ;; BigInt / bignum: if fits in long → hashLong, else BigInteger.hashCode
       ((and (exact? x) (integer? x)
             (>= x -9223372036854775808) (<= x 9223372036854775807))
        (murmur3-hash-long x))
       ;; Bignum > 64-bit: BigInteger.hashCode (JVM parity).
       (else (big-integer-hashcode x))))
    ((boolean? x) (if x 1231 1237))
    ((char? x) (char->integer x))    ;; Character.hashCode = (int) charValue
    ((jolt-symbol? x) (symbol-hasheq x))
    ;; Sequential (vector/list/seq) → hashOrdered (Murmur3.hashOrdered)
    ((jolt-sequential? x) (hash-ordered (jolt-seq x)))
    ;; Collections (map/set) → hashUnordered (Murmur3.hashUnordered)
    ((pmap? x)
     (or (and (not (= 0 (pmap-hasheq x))) (pmap-hasheq x))
         (let* ((result (pmap-fold x
                         (lambda (k v acc)
                           (cons (add32 (car acc) (entry-hasheq k v))
                                 (+ (cdr acc) 1)))
                         (cons 0 0)))
                (h (mix-coll-hash (car result) (cdr result))))
           (pmap-hasheq-set! x h)
           h)))
    ((pset? x)
     (or (and (not (= 0 (pset-hasheq x))) (pset-hasheq x))
         (let* ((result (pset-fold x
                         (lambda (e acc) (cons (+ (car acc) (jolt-hasheq e))
                                               (+ (cdr acc) 1)))
                         (cons 0 0)))
                (h (mix-coll-hash (car result) (cdr result))))
           (pset-hasheq-set! x h)
           h)))
    (else (equal-hash x))))
