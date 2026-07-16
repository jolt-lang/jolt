;; JVM-compatible hash engine for Jolt: Murmur3 + hasheq dispatch.
;;
;; Ports Murmur3.java, Util.hasheq/Util.hashCombine, Numbers.hasheq,
;; Keyword.hasheq/Symbol.hasheq, APersistentMap.mapHasheq,
;; APersistentVector.hasheq, APersistentSet.hasheq.
;;
;; All arithmetic is 32-bit signed wrapping — the i32/u32 helpers below
;; implement Java's int semantics on Chez's 61-bit fixnum tower.
;;
;; SOUNDNESS of #3% unsafe primitives: every primitive marked #3% below
;; operates on values that are PROVABLY fixnums — either masked to ≤32 bits
;; (fxand #xFFFFFFFF, fxand #xFFFF), the product of two ≤2^31 inputs (|p| ≤ 2^47
;; ≪ 2^60), or a fixnum loop index. The #3% prefix drops Chez's runtime fixnum?
;; check per call site, which is sound because (a) all intermediates are bounded
;; far below the 61-bit fixnum ceiling, and (b) the entry point (jolt-hasheq)
;; only reaches these paths after `fixnum?` guards or type dispatch.
;;
;; Loaded from rt.ss BEFORE collections.ss so key-hash can use jolt-hasheq.

;; ============================================================================
;; 32-bit signed integer helpers — all macros (syntax-rules) so they textually
;; inline at every call site with zero procedure-call overhead.
;; ============================================================================

;; Mask to unsigned 32 bits (0 .. 2^32-1).
(define-syntax u32
  (syntax-rules ()
    ((_ x) (#3%bitwise-and x #xFFFFFFFF))))

;; Interpret unsigned 32 bits as signed 32-bit (-2^31 .. 2^31-1).
(define-syntax i32
  (syntax-rules ()
    ((_ x) (let ((u (u32 x)))
             (if (#3%fx>=? u #x80000000) (#3%fx- u #x100000000) u)))))

;; 32-bit wrapping multiply — fixnum-pure via 16-bit split.
;; Proof no step exceeds Chez's signed 61-bit fixnum range (±2^60−1):
;;   Let a ∈ [−2^31, 2^31−1] (after i32), b likewise.
;;   hi = low 16 bits of (b >>> 16) ∈ [0, 0xFFFF]
;;   lo = low 16 bits of b          ∈ [0, 0xFFFF]
;;   (#3%fx* a hi) : |a| ≤ 2^31, hi ≤ 0xFFFF → |p| ≤ 2^47        ≪ 2^60  ✓
;;   (#3%fxand p #xFFFF) ∈ [0, 0xFFFF]                              ≪ 2^60  ✓
;;   (#3%fxsll ... 16)  ∈ [0, 0xFFFF0000] ≤ 2^32                   ≪ 2^60  ✓
;;   (#3%fx* a lo) : |a| ≤ 2^31, lo ≤ 0xFFFF → |p| ≤ 2^47          ≪ 2^60  ✓
;;   (#3%fx+ hi_part lo_part) : each ≤ max(2^32, 2^47) = 2^47 → sum ≤ 2^48 ≪ 2^60 ✓
;;   final (#3%fxand sum #xFFFFFFFF) ∈ [0, 2^32−1]                  ≪ 2^60  ✓
;; After the unsigned 32-bit result is obtained, i32 converts back to signed.
;; a and b are each evaluated exactly once (let*-bound as a*/b*).
(define-syntax mul32
  (syntax-rules ()
    ((_ a b)
     (let* ((a* (i32 a))
            (b* (i32 b))
            (hi (#3%fxand (#3%fxsra b* 16) #xFFFF))
            (lo (#3%fxand b* #xFFFF))
            (hi-part (#3%fxsll (#3%fxand (#3%fx* a* hi) #xFFFF) 16))
            (lo-part (#3%fx* a* lo)))
       (i32 (#3%fxand (#3%fx+ hi-part lo-part) #xFFFFFFFF))))))

;; 32-bit wrapping add. a and b each evaluated once.
(define-syntax add32
  (syntax-rules ()
    ((_ a b) (i32 (#3%fx+ (i32 a) (i32 b))))))

;; Unsigned right shift (Java >>>).
(define-syntax urs32
  (syntax-rules ()
    ((_ x n) (#3%bitwise-arithmetic-shift-right (u32 x) n))))

;; Rotate left (Java Integer.rotateLeft). x and n each evaluated once.
(define-syntax rotl32
  (syntax-rules ()
    ((_ x n)
     (let ((n* (remainder n 32))
           (x* x))
       (i32 (#3%bitwise-ior (#3%bitwise-arithmetic-shift-left (u32 x*) n*)
                           (urs32 x* (#3%fx- 32 n*))))))))

;; ============================================================================
;; Murmur3 — exact port of clojure.lang.Murmur3.
;;
;; murmur3-mix-k1 / murmur3-mix-h1 / murmur3-fmix are the building blocks,
;; kept for cold paths (strings, bignums). The hot fixnum paths below
;; (hash-int-flat, hash-long-flat) hand-inline them to avoid procedure-call
;; overhead — the A/B/A shows +220ns per key-hash from the layered
;; key-hash→jolt-hasheq→cond→hashLong→mixK1→mixH1→fmix chain.
;; ============================================================================

(define murmur3-seed (i32 0))
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

;; ---------------------------------------------------------------------------
;; Flat-inlined murmur3-hash-int for int32-range fixnums.
;; Every intermediate < 2^49 (well within Chez 61-bit fixnums).
;; NO calls to murmur3-mix-k1/murmur3-mix-h1/murmur3-fmix —
;; the mix logic is hand-expanded as a single let* chain of fx ops.
;; mul32/rotl32/add32/i32 are small leaf helpers (fixnum-pure, one expression).
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; Flat-inlined murmur3-hash-long for fixnums wider than int32.
;; Same hand-inlined mix logic as above, applied to two 32-bit halves;
;; the second half chains through the h1 from the first half.
;; Cold bignum path kept below in murmur3-hash-long.
;; ---------------------------------------------------------------------------
(define (murmur3-hash-long-flat input)
  ;; input: fixnum. Java Long.hasheq: (int)(input ^ (input >>> 32))
  ;; If 0 → return 0; otherwise murmur3-hash-long with count=8.
  (if (#3%fx=? input 0) 0
      (let* ((low (i32 input))
             (high (i32 (bitwise-arithmetic-shift-right input 32)))
             ;; --- mixK1(low): mul32(low, C1) ---
             (k1 (mul32 low murmur3-C1))
             (k1 (rotl32 k1 15))
             (k1 (mul32 k1 murmur3-C2))
             ;; --- mixH1(seed, k1) ---
             (h1 (#3%fxxor murmur3-seed k1))
             (h1 (rotl32 h1 13))
             (h1 (add32 (mul32 h1 5) #xe6546b64))
             ;; --- mixK1(high) ---
             (k1 (mul32 high murmur3-C1))
             (k1 (rotl32 k1 15))
             (k1 (mul32 k1 murmur3-C2))
             ;; --- mixH1(h1 from low, k1 from high) ---
             (h1 (#3%fxxor h1 k1))
             (h1 (rotl32 h1 13))
             (h1 (add32 (mul32 h1 5) #xe6546b64))
             ;; --- fmix(h1, 8) ---
             (h1 (#3%fxxor h1 8))
             (h1 (#3%fxxor h1 (urs32 h1 16)))
             (h1 (mul32 h1 #x85ebca6b))
             (h1 (#3%fxxor h1 (urs32 h1 13)))
             (h1 (mul32 h1 #xc2b2ae35))
             (h1 (#3%fxxor h1 (urs32 h1 16))))
        h1)))

;; Legacy entry points — kept for cold paths (strings, bignums).
;; The hot fixnum path in jolt-hasheq and key-hash calls the flat versions above.

(define (murmur3-hash-int input)
  (if (fx=? (i32 input) 0) 0
      (let* ((k1 (murmur3-mix-k1 (i32 input)))
             (h1 (murmur3-mix-h1 murmur3-seed k1)))
        (murmur3-fmix h1 4))))

(define (murmur3-hash-long input)
  ;; Hot fixnum path: use the hand-inlined flat version.
  ;; All fixnums use hashLong (count=8) — matching JVM's Long.hasheq.
  ;; Bignum fallback below for the rare >64-bit integer.
  (if (= input 0) 0
      (if (fixnum? input)
          (murmur3-hash-long-flat input)
          ;; Cold bignum path
          (let* ((u64 (bitwise-and input #xFFFFFFFFFFFFFFFF))
                 (low (i32 u64))
                 (high (i32 (bitwise-arithmetic-shift-right u64 32)))
                 (k1 (murmur3-mix-k1 low))
                 (h1 (murmur3-mix-h1 murmur3-seed k1))
                 (k1 (murmur3-mix-k1 high))
                 (h1 (murmur3-mix-h1 h1 k1)))
            (murmur3-fmix h1 8)))))

;; ============================================================================
;; String hash — Java String.hashCode() over UTF-16 code units
;; ============================================================================

;; Java String.hashCode(): s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]
;; over UTF-16 code units. Iterates the string's codepoints directly,
;; computing surrogate pairs inline for codepoints >= #x10000 — no
;; intermediate vector allocation.
(define (java-string-hashcode s)
  (let ((len (string-length s)))
    (let loop ((i 0) (h 0))
      (if (#3%fx>=? i len)
          (i32 h)
          (let ((cp (char->integer (string-ref s i))))
            (if (#3%fx<? cp #x10000)
                (loop (#3%fx+ i 1) (i32 (#3%fx+ (#3%fx* 31 h) cp)))
                (let* ((cp2 (#3%fx- cp #x10000))
                       (high (fxior #xD800 (fxsra cp2 10)))
                       (low  (fxior #xDC00 (fxand cp2 #x3FF))))
                  (let ((h* (i32 (#3%fx+ (#3%fx* 31 h) high))))
                    (loop (#3%fx+ i 1) (i32 (#3%fx+ (#3%fx* 31 h*) low)))))))))))

(define (murmur3-hash-unencoded-chars s)
  ;; Match Java's Murmur3.hashUnencodedChars(CharSequence) over the
  ;; UTF-16 code-unit sequence. Processes 2 code units at a time.
  ;; Iterates codepoints directly (no intermediate vector), pairing
  ;; BMP units across iterations and self-pairing astral surrogates.
  (let ((len (string-length s)))
    (let loop ((i 0) (h1 murmur3-seed) (pending #f) (count 0))
      (if (#3%fx>=? i len)
          (if pending
              ;; One unpaired unit left — mix and finalize
              (let* ((k1 (murmur3-mix-k1 pending))
                     (h1 (#3%bitwise-xor h1 k1)))
                (murmur3-fmix h1 (#3%fx* 2 (#3%fx+ count 1))))
              (murmur3-fmix h1 (#3%fx* 2 count)))
          (let ((cp (char->integer (string-ref s i))))
            (if (#3%fx<? cp #x10000)
                ;; BMP: one code unit
                (if pending
                    ;; Pair pending + this unit; both consumed
                    (let* ((k1 (murmur3-mix-k1
                                (#3%bitwise-ior pending
                                 (#3%bitwise-arithmetic-shift-left cp 16))))
                           (h1 (murmur3-mix-h1 h1 k1)))
                      (loop (#3%fx+ i 1) h1 #f (#3%fx+ count 2)))
                    ;; Hold as pending (not counted yet)
                    (loop (#3%fx+ i 1) h1 cp count))
                ;; Astral: surrogate pair (high, low) — always 2 units
                (let* ((cp2 (#3%fx- cp #x10000))
                       (high (fxior #xD800 (fxsra cp2 10)))
                       (low  (fxior #xDC00 (fxand cp2 #x3FF))))
                  (if pending
                      ;; Pair pending + high (consumed), low becomes new pending
                      (let* ((k1 (murmur3-mix-k1
                                  (#3%bitwise-ior pending
                                   (#3%bitwise-arithmetic-shift-left high 16))))
                             (h1 (murmur3-mix-h1 h1 k1)))
                        (loop (#3%fx+ i 1) h1 low (#3%fx+ count 2)))
                      ;; High + low consumed together
                      (let* ((k1 (murmur3-mix-k1
                                  (#3%bitwise-ior high
                                   (#3%bitwise-arithmetic-shift-left low 16))))
                             (h1 (murmur3-mix-h1 h1 k1)))
                        (loop (#3%fx+ i 1) h1 #f (#3%fx+ count 2)))))))))))

;; ============================================================================
;; Long.hashCode (Java): (int)(value ^ (value >>> 32))
;; Used for Ratio/BigInt hashCode where the JVM calls .hashCode() directly
;; (not Murmur3).
;; ============================================================================

(define (long-hashcode x)
  (i32 (bitwise-xor x (bitwise-arithmetic-shift-right (bitwise-and x #xFFFFFFFFFFFFFFFF) 32))))

;; ============================================================================
;; Double.hasheq — exact port of Numbers.hasheq for Double.class
;; ============================================================================

;; Extract the 64-bit IEEE-754 bit pattern of a double.
;; bytevector-ieee-double-native-set! writes in native byte order.
;; On little-endian (macOS ARM), the most significant byte is at index 7.
(define (double-to-raw-bits x)
  (let ((bv (make-bytevector 8)))
    (bytevector-ieee-double-native-set! bv 0 x)
    (let ((hi (bitwise-ior (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 7) 24)
                           (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 6) 16)
                           (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 5) 8)
                           (bytevector-u8-ref bv 4)))
          (lo (bitwise-ior (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 3) 24)
                           (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 2) 16)
                           (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 1) 8)
                           (bytevector-u8-ref bv 0))))
      (bitwise-ior (bitwise-arithmetic-shift-left hi 32) lo))))

(define (double-hasheq x)
  ;; Numbers.hasheq for Double.class: if -0.0 → 0; else hashCode.
  ;; Double.hashCode = (int)(bits ^ (bits >>> 32))
  (if (and (flonum? x) (fl=? x 0.0) (fl=? (fl/ x 1.0) -0.0))
      0
      (let ((bits (double-to-raw-bits x)))
        (i32 (bitwise-xor bits (bitwise-arithmetic-shift-right bits 32))))))

;; ============================================================================
;; Collection hash mixers — exact ports of Murmur3 Java methods
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
              (#3%fx+ n 1)
              (i32 (#3%fx+ (#3%fx* 31 h) (jolt-hasheq (seq-first xs))))))))

;; Compute hash-ordered of a 2-element sequence [k v] — exactly what
;; MapEntry-as-vector yields on the JVM. Inlined to avoid cseq allocs.
(define (entry-hasheq k v)
  (let* ((h1 (i32 (#3%fx+ 31 (jolt-hasheq k))))
         (h2 (i32 (#3%fx+ (#3%fx* 31 h1) (jolt-hasheq v)))))
    (mix-coll-hash h2 2)))

(define (hash-unordered xs)
  (let loop ((xs xs) (n 0) (h 0))
    (if (jolt-nil? xs)
        (mix-coll-hash h n)
        (let ((e (seq-first xs)))
          (loop (jolt-seq (seq-more xs))
                (#3%fx+ n 1)
                (+ h (if (pair? e)
                         (entry-hasheq (car e) (cdr e))
                         (jolt-hasheq e))))))))

;; ============================================================================
;; Util.hashCombine — exact port of clojure.lang.Util.hashCombine
;; ============================================================================

(define (hash-combine seed hash)
  ;; a la boost: seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2)
  ;; Java's >> is arithmetic (sign-extending), NOT >>> (logical/unsigned).
  (let* ((seed (i32 seed))
         (hash (i32 hash))
         (sl   (i32 (bitwise-arithmetic-shift-left seed 6)))
         (sr   (bitwise-arithmetic-shift-right seed 2))
         (sum  (i32 (+ (i32 (+ hash #x9e3779b9)) (i32 (+ sl sr)))))
         (result (bitwise-xor seed sum)))
    (i32 result)))

;; ============================================================================
;; Keyword / Symbol hasheq (mirrors Keyword.java / Symbol.java)
;; ============================================================================

;; Keyword hasheq = symbol.hasheq() + 0x9e3779b9
;; Stored in the keyword-t's khash field at construction time.

(define (compute-keyword-hasheq ns name)
  ;; sym.hasheq() = hashCombine(murmur3.hashUnencodedChars(name), hash(ns))
  ;; hash(ns) = ns.hashCode() = java-string-hashcode(ns) or 0 if null.
  ;; Then keyword hasheq = sym.hasheq() + 0x9e3779b9.
  (let ((ns-hash (if (or (not ns) (eq? ns '()))
                     0
                     (java-string-hashcode ns))))
    (i32 (+ (hash-combine (murmur3-hash-unencoded-chars name) ns-hash)
            #x9e3779b9))))

;; Symbol hasheq = Util.hashCombine(Murmur3.hashUnencodedChars(name), Util.hash(ns)).
;; JVM caches in _hasheq field. Jolt symbols aren't interned (meta varies),
;; so we cache in a weak-eq hashtable keyed by the symbol object.
(define symbol-hasheq-cache (make-weak-eq-hashtable))

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

;; String hasheq cache — same pattern as symbol cache.
;; JVM caches String.hashCode per object; Jolt strings aren't interned
;; (they're regular Chez strings), so we cache in a weak-eq hashtable.
(define string-hasheq-cache (make-weak-eq-hashtable))

(define (compute-string-hasheq s)
  (murmur3-hash-int (java-string-hashcode s)))

(define (string-hasheq s)
  (or (hashtable-ref string-hasheq-cache s #f)
      (let ((h (compute-string-hasheq s)))
        (hashtable-set! string-hasheq-cache s h)
        h)))

;; ============================================================================
;; jolt-hasheq — the top-level dispatch (mirrors Util.hasheq)
;; ============================================================================

;; Per-type arms registered by host shims (records, dates, etc.).
;; An arm is (pred . handler); pred takes the value, handler returns int.
(define jolt-hasheq-arms '())

;; Dispatch: fast-path types first, then registered arms, then fallback.
(define (jolt-hasheq x)
  ;; Fast path for the most common types (matching Util.hasheq order).
  (cond
    ((jolt-nil? x) 0)
    ((keyword? x) (keyword-t-khash x))
    ;; Fixnum: hot path for integer-keyed maps. Hand-inlined murmur to
    ;; avoid the layered dispatch chain (key-hash→jolt-hasheq→cond→
    ;; hashLong→mixK1→mixH1→fmix). All fixnums use hashLong (count=8)
    ;; matching JVM's Long.hasheq.
    ((fixnum? x) (murmur3-hash-long-flat x))
    ((string? x) (string-hasheq x))
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
       ;; Ratio: exact non-integer. hashCode = numerator.hashCode ^ denominator.hashCode
       ((and (exact? x) (not (integer? x)))
        (i32 (bitwise-xor (long-hashcode (numerator x))
                          (long-hashcode (denominator x)))))
       ;; BigInt / bignum: if fits in long → hashLong, else BigInteger.hashCode
       ;; For values within 64-bit signed range, use hashLong.
       ((and (exact? x) (integer? x)
             (>= x -9223372036854775808) (<= x 9223372036854775807))
        (murmur3-hash-long x))
       ;; Bignum > 64-bit: fallback to Chez equal-hash (not JVM BigInteger.hashCode,
       ;; but bignum map keys are exceptionally rare).
       (else (equal-hash x))))
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
                                 (fx+ (cdr acc) 1)))
                         (cons 0 0)))
                (h (mix-coll-hash (car result) (cdr result))))
           (pmap-hasheq-set! x h)
           h)))
    ((pset? x)
     (or (and (not (= 0 (pset-hasheq x))) (pset-hasheq x))
         (let* ((result (pset-fold x
                         (lambda (e acc) (cons (+ (car acc) (jolt-hasheq e)) (fx+ (cdr acc) 1)))
                         (cons 0 0)))
                (h (mix-coll-hash (car result) (cdr result))))
           (pset-hasheq-set! x h)
           h)))
    (else (equal-hash x))))

;; ============================================================================
;; Quick sanity: export a helper for the natives to rebind clojure.core/hash

