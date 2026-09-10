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
;;
;; TARGET-OWNED FILE (PSL R10): hasheq.ss is a per-target implementation a port
;; REPLACES rather than migrates — the Chez-tuned hash engine. The #3% sites
;; above are proven-sound unsafe variants (bounded intermediates, guarded
;; entry); a target port supplies its own hasheq using safe ops or its own
;; unsafe forms, keeping the SAME exported procedures. The portability gate
;; allows this file's $primitive use only because the file is target-owned.
;; Loaded by rt.ss as a plain top-level load, so the "exports" are the
;; procedures the rest of the host actually calls from here (verified against
;; call sites, not guessed). A target must reimplement exactly these:
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

;; ============================================================================
;; Fixnum-width selection for the 32-bit engine (shared with collections.ss).
;; ============================================================================
;; Every value this engine and the HAMT compute lives in the Java int window:
;; [0, 2^32-1] unsigned, [-2^31, 2^31-1] signed. On a 64-bit Chez that whole
;; window is fixnum, which is what makes the unsafe #3%fx chain below sound and
;; what the measured fast path was tuned against. On a 32-bit target — tpb32l,
;; the pb/WASM build — the positive fixnum ceiling is 2^29-1, so an ordinary int
;; hash is a bignum and an fx op applied to it is unsound, not merely slow.
;;
;; define-width-op names one operator per pair and picks between them at EXPAND
;; time, so exactly one arm reaches the compiler: on a wide target the generated
;; code is the same #3%fx chain as before, with no runtime width test, no
;; duplicated arms, and no reliance on cp0 folding the test away. It is one
;; macro-generating macro rather than a hand-written (if (fixnum? ...) a b) per
;; operator so the width question is asked in exactly ONE place — collections.ss
;; declares its own HAMT operators through this same form.
;;
;; The test runs on the COMPILING Chez, which for a native build is the target.
;; Cross-compiling to a target narrower than the host would have to consult the
;; target's fixnum width here instead.
;;
;; JOLT_NARROW_HASH=1 in the compiling environment forces the narrow arm on a
;; wide machine. That is what makes the generic arms executable under the
;; ordinary gates instead of only on 32-bit hardware: `make narrowhash` replays
;; the JVM-pinned hash suite and the collection suites with it set.
(define-syntax define-width-op
  (lambda (x)
    (syntax-case x ()
      ((_ name wide narrow)
       (if (and (fixnum? #xFFFFFFFF) (not (getenv "JOLT_NARROW_HASH")))
           #'(define-syntax name
               (syntax-rules () ((_ a (... ...)) (wide a (... ...)))))
           #'(define-syntax name
               (syntax-rules () ((_ a (... ...)) (narrow a (... ...))))))))))

;; The hash engine's operators. Unsafe #3% on the wide path: the window
;; invariant documented above is the type check, and these are the measured
;; inner chain of murmur3/mul32.
(define-width-op hash-fx=?  #3%fx=?  =)
(define-width-op hash-fx<?  #3%fx<?  <)
(define-width-op hash-fx>=? #3%fx>=? >=)
(define-width-op hash-fx+   #3%fx+   +)
(define-width-op hash-fx-   #3%fx-   -)
(define-width-op hash-fx*   #3%fx*   *)
(define-width-op hash-fxand #3%fxand bitwise-and)
(define-width-op hash-fxior #3%fxior bitwise-ior)
(define-width-op hash-fxxor #3%fxxor bitwise-xor)
(define-width-op hash-fxsll #3%fxsll bitwise-arithmetic-shift-left)
(define-width-op hash-fxsra #3%fxsra bitwise-arithmetic-shift-right)
;; fxsrl has no exact-integer twin, and needs none: every use below shifts a
;; u32-masked (non-negative) operand, where the logical and arithmetic shifts
;; agree. urs32 is the only entry point and it masks first.
(define-width-op hash-fxsrl #3%fxsrl bitwise-arithmetic-shift-right)

;; ============================================================================
;; 32-bit signed integer helpers — all macros (syntax-rules) so they textually
;; inline at every call site with zero procedure-call overhead.
;; ============================================================================

;; Mask to unsigned 32 bits (0 .. 2^32-1).
;;
;; This one MUST stay generic. Unlike every other helper here, u32 is the entry
;; point for the COLD paths too: murmur3-hash-long's bignum arm masks a 64-bit
;; value (itself a bignum on a 61-bit fixnum tower) and double-hasheq feeds it a
;; double's raw bits, so an fxand here is applied to a non-fixnum. Doing that with
;; the unsafe #3% form does not error, it silently answers wrong — the corpus
;; caught exactly that as `hash double 1.5` and `hash large long` divergences.
;; It also buys nothing: measured 2.1 ns either way, because the result is a
;; fixnum and Chez's generic bitwise-and is already fast for the fixnum case.
;;
;; The output is always in [0, 2^32-1]. hash-fx* exploits that as a fixnum
;; invariant only on wide-fixnum machines and uses generic exact arithmetic on
;; narrower targets.
(define-syntax u32
  (syntax-rules ()
    ((_ x) (#3%bitwise-and x #xFFFFFFFF))))

;; Interpret unsigned 32 bits as signed 32-bit (-2^31 .. 2^31-1).
(define-syntax i32
  (syntax-rules ()
    ((_ x) (let ((u (u32 x)))
             (if (hash-fx>=? u #x80000000) (hash-fx- u #x100000000) u)))))

;; 32-bit wrapping multiply via a 16-bit split. On a wide-fixnum target the
;; selected fast path is fixnum-pure; on a narrow target the same bounded
;; intermediates use generic exact arithmetic.
;; Proof no step exceeds Chez's signed 61-bit fixnum range (±2^60−1):
;;   Let a ∈ [−2^31, 2^31−1] (after i32), b likewise.
;;   hi = low 16 bits of (b >>> 16) ∈ [0, 0xFFFF]
;;   lo = low 16 bits of b          ∈ [0, 0xFFFF]
;;   (hash-fx* a hi) : |a| ≤ 2^31, hi ≤ 0xFFFF → |p| ≤ 2^47        ≪ 2^60  ✓
;;   (hash-fxand p #xFFFF) ∈ [0, 0xFFFF]                              ≪ 2^60  ✓
;;   (hash-fxsll ... 16)  ∈ [0, 0xFFFF0000] ≤ 2^32                   ≪ 2^60  ✓
;;   (hash-fx* a lo) : |a| ≤ 2^31, lo ≤ 0xFFFF → |p| ≤ 2^47          ≪ 2^60  ✓
;;   (hash-fx+ hi_part lo_part) : each ≤ max(2^32, 2^47) = 2^47 → sum ≤ 2^48 ≪ 2^60 ✓
;;   final (hash-fxand sum #xFFFFFFFF) ∈ [0, 2^32−1]                  ≪ 2^60  ✓
;; After the unsigned 32-bit result is obtained, i32 converts back to signed.
;; a and b are each evaluated exactly once (let*-bound as a*/b*).
(define-syntax mul32
  (syntax-rules ()
    ((_ a b)
     (let* ((a* (i32 a))
            (b* (i32 b))
            (hi (hash-fxand (hash-fxsra b* 16) #xFFFF))
            (lo (hash-fxand b* #xFFFF))
            (hi-part (hash-fxsll (hash-fxand (hash-fx* a* hi) #xFFFF) 16))
            (lo-part (hash-fx* a* lo)))
       (i32 (hash-fxand (hash-fx+ hi-part lo-part) #xFFFFFFFF))))))

;; 32-bit wrapping add. a and b each evaluated once.
(define-syntax add32
  (syntax-rules ()
    ((_ a b) (i32 (hash-fx+ (i32 a) (i32 b))))))

;; Unsigned right shift (Java >>>). fxsrl on the masked value: the operand is a
;; fixnum and already non-negative after u32, so the logical and arithmetic shifts
;; agree and the fx form skips the generic dispatch.
(define-syntax urs32
  (syntax-rules ()
    ((_ x n) (hash-fxsrl (u32 x) n))))

;; Rotate left (Java Integer.rotateLeft). x and n each evaluated once.
;;
;; This was the most expensive leaf in the engine by a factor of three — 6.3 ns
;; against mul32's 2.2 ns — for two reasons, and it is called twice per murmur
;; round so it dominated hashing: (remainder n 32) was a GENERIC procedure call on
;; what is a literal 13 or 15 at every call site, and the shifts and the ior were
;; the generic bitwise-* forms rather than fx ops. It accounted for roughly half of
;; murmur3-hash-unencoded-chars.
;;
;; The mask is what makes the fx shift safe for any n, not just the 13/15 the
;; callers use: a bare (fxsll u n) with u up to 2^32-1 and n up to 31 reaches 2^63
;; and blows past Chez's 61-bit fixnum ceiling. Masking to the low (32-n) bits
;; FIRST bounds the shifted value at 2^32.
;;   mask = #xFFFFFFFF >>> n  -> low (32-n) bits set
;;   lo   = (u & mask) << n   -> < 2^32                                        ✓
;;   hi   = u >>> (32-n)      -> < 2^n                                         ✓
;;   lo | hi                  -> < 2^32                                        ✓
;; n must be in 1..31; every call site passes a literal in range.
(define-syntax rotl32
  (syntax-rules ()
    ((_ x n)
     (let* ((x* (u32 x))
            (n* n)
            (lo (hash-fxsll (hash-fxand x* (hash-fxsrl #xFFFFFFFF n*)) n*))
            (hi (hash-fxsrl x* (hash-fx- 32 n*))))
       (i32 (hash-fxior lo hi))))))

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
  (let* ((h1 (hash-fxxor h1 k1))
         (h1 (rotl32 h1 13))
         (h1 (add32 (mul32 h1 5) #xe6546b64)))
    h1))

(define (murmur3-fmix h1 len)
  (let* ((h1 (hash-fxxor h1 len))
         (h1 (hash-fxxor h1 (urs32 h1 16)))
         (h1 (mul32 h1 #x85ebca6b))
         (h1 (hash-fxxor h1 (urs32 h1 13)))
         (h1 (mul32 h1 #xc2b2ae35))
         (h1 (hash-fxxor h1 (urs32 h1 16))))
    h1))

;; The same three mixers as MACROS, so a caller gets the flat fx-op chain the
;; fixnum paths below hand-expand without hand-expanding it again. Same technique,
;; same arithmetic, one copy of the logic: mul32/rotl32/add32/urs32/i32 are already
;; macros, so expanding these leaves a let* of fx ops and no calls at all.
;;
;; This exists because "cold path" stopped being true for strings. The procedure
;; forms above cost roughly 10-15 ns per call in Chez and hashing a 6-char symbol
;; name makes SEVEN of them (mixK1 and mixH1 per 2-char pair, plus one fmix), which
;; measured 111 ns of the 161 ns a fresh symbol's hasheq took — and honeysql's
;; format-dsl hashes 92 freshly built symbols per format call. The procedure forms
;; stay for the genuinely cold bignum paths.
(define-syntax murmur3-mix-k1-flat
  (syntax-rules ()
    ((_ k) (let* ((k1 (mul32 k murmur3-C1))
                  (k1 (rotl32 k1 15)))
             (mul32 k1 murmur3-C2)))))
(define-syntax murmur3-mix-h1-flat
  (syntax-rules ()
    ((_ h k) (let* ((h1 (hash-fxxor h k))
                    (h1 (rotl32 h1 13)))
               (add32 (mul32 h1 5) #xe6546b64)))))
(define-syntax murmur3-fmix-flat
  (syntax-rules ()
    ((_ h len) (let* ((h1 (hash-fxxor h len))
                      (h1 (hash-fxxor h1 (urs32 h1 16)))
                      (h1 (mul32 h1 #x85ebca6b))
                      (h1 (hash-fxxor h1 (urs32 h1 13)))
                      (h1 (mul32 h1 #xc2b2ae35)))
                 (hash-fxxor h1 (urs32 h1 16))))))

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
  (if (hash-fx=? input 0) 0
      (let* ((low (i32 input))
             (high (i32 (bitwise-arithmetic-shift-right input 32)))
             ;; --- mixK1(low): mul32(low, C1) ---
             (k1 (mul32 low murmur3-C1))
             (k1 (rotl32 k1 15))
             (k1 (mul32 k1 murmur3-C2))
             ;; --- mixH1(seed, k1) ---
             (h1 (hash-fxxor murmur3-seed k1))
             (h1 (rotl32 h1 13))
             (h1 (add32 (mul32 h1 5) #xe6546b64))
             ;; --- mixK1(high) ---
             (k1 (mul32 high murmur3-C1))
             (k1 (rotl32 k1 15))
             (k1 (mul32 k1 murmur3-C2))
             ;; --- mixH1(h1 from low, k1 from high) ---
             (h1 (hash-fxxor h1 k1))
             (h1 (rotl32 h1 13))
             (h1 (add32 (mul32 h1 5) #xe6546b64))
             ;; --- fmix(h1, 8) ---
             (h1 (hash-fxxor h1 8))
             (h1 (hash-fxxor h1 (urs32 h1 16)))
             (h1 (mul32 h1 #x85ebca6b))
             (h1 (hash-fxxor h1 (urs32 h1 13)))
             (h1 (mul32 h1 #xc2b2ae35))
             (h1 (hash-fxxor h1 (urs32 h1 16))))
        h1)))

;; Legacy entry points — kept for cold paths (strings, bignums).
;; The hot fixnum path in jolt-hasheq and key-hash calls the flat versions above.

(define (murmur3-hash-int input)
  (if (hash-fx=? (i32 input) 0) 0
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
      (if (hash-fx>=? i len)
          (i32 h)
          (let ((cp (char->integer (string-ref s i))))
            (if (hash-fx<? cp #x10000)
                (loop (hash-fx+ i 1) (i32 (hash-fx+ (hash-fx* 31 h) cp)))
                (let* ((cp2 (hash-fx- cp #x10000))
                       (high (fxior #xD800 (fxsra cp2 10)))
                       (low  (fxior #xDC00 (fxand cp2 #x3FF))))
                  (let ((h* (i32 (hash-fx+ (hash-fx* 31 h) high))))
                    (loop (hash-fx+ i 1) (i32 (hash-fx+ (hash-fx* 31 h*) low)))))))))))

(define (murmur3-hash-unencoded-chars s)
  ;; Match Java's Murmur3.hashUnencodedChars(CharSequence) over the
  ;; UTF-16 code-unit sequence. Processes 2 code units at a time.
  ;; Iterates codepoints directly (no intermediate vector), pairing
  ;; BMP units across iterations and self-pairing astral surrogates.
  ;;
  ;; Java reads charAt, so its input IS the code-unit sequence and its loop is a
  ;; bare `for i += 2`. A Chez string holds CODEPOINTS, so an astral char is two
  ;; UTF-16 units and the pairing cannot be a fixed stride — hence `pending`,
  ;; which carries an unpaired unit into the next iteration.
  ;;
  ;; Mixers are the -flat MACRO forms: expanding them leaves fx ops with no calls,
  ;; where the procedure forms cost 7 calls for a 6-char name (mixK1 + mixH1 per
  ;; pair, plus fmix). Same technique the file already applied to hash-int/hash-long,
  ;; extended here because a fresh symbol's hasheq is on honeysql's hot path, not a
  ;; cold one. Values are unchanged — flat-vs-layered is checked over ASCII, BMP,
  ;; astral, mixed and every length 0..64 in test/chez/hasheq-test.ss.
  (let ((len (string-length s)))
    (let loop ((i 0) (h1 murmur3-seed) (pending #f) (count 0))
      (if (hash-fx>=? i len)
          (if pending
              ;; One unpaired unit left — mix and finalize
              (let* ((k1 (murmur3-mix-k1-flat pending))
                     (h1 (hash-fxxor h1 k1)))
                (murmur3-fmix-flat h1 (hash-fx* 2 (hash-fx+ count 1))))
              (murmur3-fmix-flat h1 (hash-fx* 2 count)))
          (let ((cp (char->integer (string-ref s i))))
            (if (hash-fx<? cp #x10000)
                ;; BMP: one code unit
                (if pending
                    ;; Pair pending + this unit; both consumed
                    (let* ((k1 (murmur3-mix-k1-flat
                                (hash-fxior pending (hash-fxsll cp 16))))
                           (h1 (murmur3-mix-h1-flat h1 k1)))
                      (loop (hash-fx+ i 1) h1 #f (hash-fx+ count 2)))
                    ;; Hold as pending (not counted yet)
                    (loop (hash-fx+ i 1) h1 cp count))
                ;; Astral: surrogate pair (high, low) — always 2 units
                (let* ((cp2 (hash-fx- cp #x10000))
                       (high (fxior #xD800 (fxsra cp2 10)))
                       (low  (fxior #xDC00 (fxand cp2 #x3FF))))
                  (if pending
                      ;; Pair pending + high (consumed), low becomes new pending
                      (let* ((k1 (murmur3-mix-k1-flat
                                  (hash-fxior pending (hash-fxsll high 16))))
                             (h1 (murmur3-mix-h1-flat h1 k1)))
                        (loop (hash-fx+ i 1) h1 low (hash-fx+ count 2)))
                      ;; High + low consumed together
                      (let* ((k1 (murmur3-mix-k1-flat
                                  (hash-fxior high (hash-fxsll low 16))))
                             (h1 (murmur3-mix-h1-flat h1 k1)))
                        (loop (hash-fx+ i 1) h1 #f (hash-fx+ count 2)))))))))))

;; ============================================================================
;; Long.hashCode (Java): (int)(value ^ (value >>> 32))
;; Used for Ratio/BigInt hashCode where the JVM calls .hashCode() directly
;; (not Murmur3).
;; ============================================================================


;; BigInteger.hashCode — java.math.BigInteger.hashCode() for exact integers
;; that don't fit in 64-bit. Iterates 32-bit magnitude limbs (big-endian),
;; accumulating h = 31*h + limb with int32 wrapping, then multiplies by signum.
(define (big-integer-hashcode x)
  (let* ((signum (cond ((< x 0) -1) ((= x 0) 0) (else 1)))
         (mag (abs x)))
    (if (= mag 0)
        0
        (let ((nbits (integer-length mag)))
          (let* ((nlimbs (fx+ (fxquotient (fx- nbits 1) 32) 1))
                 (shift0 (fx* (fx- nlimbs 1) 32)))
            (let loop ((i 0) (h 0) (shift shift0))
              (if (fx>=? i nlimbs)
                  (i32 (* signum h))
                  (let ((limb (u32 (bitwise-arithmetic-shift-right mag shift))))
                    (loop (fx+ i 1)
                          (i32 (+ (* 31 h) limb))
                          (fx- shift 32))))))))))

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
              (hash-fx+ n 1)
              (i32 (hash-fx+ (hash-fx* 31 h) (jolt-hasheq (seq-first xs))))))))

;; Compute hash-ordered of a 2-element sequence [k v] — exactly what
;; MapEntry-as-vector yields on the JVM. Inlined to avoid cseq allocs.
(define (entry-hasheq k v)
  (let* ((h1 (i32 (hash-fx+ 31 (jolt-hasheq k))))
         (h2 (i32 (hash-fx+ (hash-fx* 31 h1) (jolt-hasheq v)))))
    (mix-coll-hash h2 2)))

(define (hash-unordered xs)
  (let loop ((xs xs) (n 0) (h 0))
    (if (jolt-nil? xs)
        (mix-coll-hash h n)
        (let ((e (seq-first xs)))
          (loop (jolt-seq (seq-more xs))
                (hash-fx+ n 1)
                ;; add32: APersistentMap.mapHasheq/APersistentSet.setHasheq sum in a
                ;; Java int (32-bit wrap), matching the native pmap/pset paths — so a
                ;; custom coll that hashes via hash-unordered-coll (flatland's ordered
                ;; types) hashes equal to a plain map/set with the same elements.
                (add32 h (if (pair? e)
                             (entry-hasheq (car e) (cdr e))
                             (jolt-hasheq e))))))))

;; ============================================================================
;; Util.hashCombine — exact port of clojure.lang.Util.hashCombine
;; ============================================================================

;; Util.hashCombine: seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2)
;; Java's >> is arithmetic (sign-extending), NOT >>> (logical/unsigned), so fxsra.
;; fx ops throughout rather than the generic bitwise-*/+ forms: both arguments are
;; i32-normalized fixnums, and this runs once per symbol and once per keyword hash.
;; Bounds, with seed and hash in [-2^31, 2^31-1] after i32:
;;   (fxsll seed 6)  -> |v| <= 2^37                                            ✓
;;   (fxsra seed 2)  -> |v| <= 2^29                                            ✓
;;   the two fx+ sums stay under 2^38 before each i32 masks back to 32 bits     ✓
(define (hash-combine seed hash)
  (let* ((seed (i32 seed))
         (hash (i32 hash))
         (sl   (i32 (hash-fxsll seed 6)))
         (sr   (hash-fxsra seed 2))
         (sum  (i32 (hash-fx+ (i32 (hash-fx+ hash #x9e3779b9)) (i32 (hash-fx+ sl sr)))))
         (result (hash-fxxor seed sum)))
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
;; The JVM caches it in Symbol's _hasheq field and so does jolt now, in
;; symbol-t-khash (values.ss) — see symbol-hasheq below for why that replaced a
;; table.
;; --- the per-thread hasheq caches -------------------------------------------
;; A string's hasheq is cached, because recomputing it is 8-19x the cost of a
;; lookup (measured: a 12-char key 47 ns vs 5.8 ns, a 48-char key 109 ns).
;; Keywords and symbols do not need this — they carry their hash in the record
;; (keyword-t-khash, symbol-t-khash) — but a Chez string cannot hold a field, so
;; a table it is. The car of the pair is vestigial: it held the symbol cache and
;; is kept so the vreg's shape does not change under a running image.
;;
;; The tables are per THREAD, not shared. A Chez hashtable is not thread-safe,
;; and these are written on every cache MISS from whatever thread is hashing, so
;; sharing them means unsynchronized concurrent mutation — which corrupts the
;; table's internals and surfaces later as a fault inside the collector, never as
;; an error naming the table. Serializing them instead would put a mutex on the
;; hottest read path in the collection layer.
;;
;; A virtual register holds this thread's pair of tables (slot 5, registered in
;; rt.ss's allocation comment): a vreg read is ~2 ns against ~33 ns for a
;; thread-parameter, and a freshly forked thread starts every slot at fixnum 0,
;; which is what "not allocated yet" means here.
(define jolt-vreg-hasheq-caches 5)

(define (hasheq-caches)
  (let ((c (virtual-register jolt-vreg-hasheq-caches)))
    (if (eq? c 0)
        (let ((v (cons (make-weak-eq-hashtable) (make-weak-eq-hashtable))))
          (set-virtual-register! jolt-vreg-hasheq-caches v)
          v)
        c)))

(define (compute-symbol-hasheq ns name)
  (let ((ns-hash (if (or (jolt-nil? ns) (not ns) (eq? ns '()))
                     0
                     (java-string-hashcode ns))))
    (hash-combine (murmur3-hash-unencoded-chars name) ns-hash)))

;; Read the record's own field, filling it on the first hash.
;;
;; This used to be a per-thread weak-eq hashtable keyed by the symbol object, on
;; the reasoning that symbols are not interned so there is nowhere to put the
;; hash. There is — symbol-t is a record and can carry a mutable field, which
;; keywords already did. The table was actively harmful for the case that matters
;; most: a symbol built for one lookup and dropped, which is what
;; (get m (symbol (name k))) does, MISSED and then INSERTED, so the weak table
;; grew by an entry per call and every one of them had to be traced and collected.
;; honeysql's format-dsl does that 92 times per format call. A field write costs
;; nothing and dies with the symbol.
;;
;; The write is unsynchronized on purpose: ns and name are immutable, so two
;; threads racing compute the same value and the loser's store is a no-op.
;; The murmur over the NAME is memoized on the name's pool cell, so a name the
;; process has hashed before costs a cdr instead of a 45.8 ns walk. That is the
;; whole reason symbol-t carries ncell — a fresh symbol for a familiar name is the
;; hot shape, and khash cannot help it. Only a symbol with a non-string name (no
;; cell) falls through to compute-symbol-hasheq, which must stay byte-compatible
;; with this: same combiner, same two hashes, same 0 for an absent ns.
(define (symstr-mhash! c)
  (or (symstr-mhash c)
      (let ((h (murmur3-hash-unencoded-chars (symstr-str c))))
        (symstr-mhash-set! c h)
        h)))
(define (symbol-hasheq sym)
  (or (symbol-t-khash sym)
      (let* ((c (symbol-t-ncell sym))
             (ns (symbol-t-ns sym))
             ;; one string? in place of compute-symbol-hasheq's three-way
             ;; nil/#f/() check: an ns that is not a string is exactly an absent
             ;; one, whichever of those three spellings it arrived as.
             (h (if c
                    (hash-combine (symstr-mhash! c)
                                  (if (string? ns) (java-string-hashcode ns) 0))
                    (compute-symbol-hasheq ns (symbol-t-name sym)))))
        (symbol-t-khash-set! sym h)
        h)))

;; The JVM caches String.hashCode in the object; jolt strings are plain Chez
;; strings with nowhere to put it, so they use the per-thread table above.
(define (compute-string-hasheq s)
  (murmur3-hash-int (java-string-hashcode s)))

(define (string-hasheq s)
  (let ((t (cdr (hasheq-caches))))
    (or (hashtable-ref t s #f)
        (let ((h (compute-string-hasheq s)))
          (hashtable-set! t s h)
          h))))

;; ============================================================================
;; jolt-hasheq — the top-level dispatch (mirrors Util.hasheq)
;; ============================================================================

;; Per-type arms registered by host shims (records, dates, etc.).
;; An arm is (pred . handler); pred takes the value, handler returns int.
(define jolt-hasheq-arms '())

;; Dispatch: fast-path types first, then registered arms, then fallback.
;; Procedure identity hasheq — the JVM's Object.hashCode shape for fns. The
;; old route was the (equal-hash x) fallback, and Chez's equal-hash answers ONE
;; CONSTANT for every procedure (measured 669430755266, for all fns — not even
;; 32-bit), which collapsed every fn-keyed map into a single collision bucket:
;; instaparse's GLL msg-cache keys [listener index] made the grammar build
;; quadratic in listeners, 92% of honeysql's namespace-load time.
;;
;; A weak-eq side table hands each procedure a murmured id on first hash. The
;; table MUST be weak-keyed and unbounded, unlike the intern front cache
;; (values.ss): an id has to stay stable for the object's lifetime — clearing
;; on overflow would rehash live map keys out of their maps — and stability
;; across GC is why this is a table and not an address hash. Entries die with
;; their fn, and only fns that are actually hashed (fn-keyed maps/sets) ever
;; get one, so the live-weak-entry volume stays far below the churn that made
;; a weak table pathological in the intern-cache case. Ids are per-process:
;; a procedure's hash does not survive an image dump/restore, exactly as
;; identityHashCode does not survive JVM serialization.
;; jolt-identity-hasheq serves every identity-hashed population: procedures,
;; and plain deftypes (records-coll.ss — a deftype without a declared
;; hashCode/hasheq is Object.hashCode on the JVM, identity, where jolt used to
;; hash it structurally and let equal-field instances collide as map keys).
;; One shared table: the id is per OBJECT, whatever its type.
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
;; Pin a procedure's identity hash to a value chosen by the caller, before
;; anything asks for one. A deftype/defrecord type token is = to its Class (they
;; are one object on the JVM), so it has to HASH like it too, and the fast path
;; above may not grow a probe for that — procedures are in hash-fast-probes
;; precisely so no arm can claim one, and adding a second weak-table lookup to
;; every procedure hash would tax the fn-keyed-map path to fix a rare case.
;; Seeding the table the fast path already reads costs the hot path nothing.
;; A content-derived seed also travels better than the counter: it is the same
;; number in the next process, where a counter-assigned id is not.
(define (jolt-identity-hasheq-seed! p h)
  (jolt-with-mutex proc-hasheq-mu
    (unless (hashtable-ref proc-hasheq-tbl p #f)
      (hashtable-set! proc-hasheq-tbl p h))))

;; pvec hasheq, cached in the field the record has carried since chez-pvec-v3
;; (mk-pvec inits it 0 = unset; pvec-with-ent already forwards it) but nothing
;; ever FILLED: every (hash v) re-walked the vector through a freshly allocated
;; seq. Fill it the way pmap/pset do — lazily, on first hash — and compute by
;; leaf runs (pv-leaf-for) with no seq cells, the same stride jolt-coll=? walks.
;; The 0-means-unset convention shares pmap's one-in-2^32 flaw: a content whose
;; hash IS 0 recomputes per call, harmlessly.
(define (pvec-hasheq-cached p)
  (let ((c (pvec-hasheq p)))
    (if (and (fixnum? c) (not (fx=? c 0)))
        c
        (let ((n (pvec-count p)))
          (let loop ((i 0) (h 1))
            (if (fx=? i n)
                (let ((r (mix-coll-hash h n)))
                  (pvec-hasheq-set! p r)
                  r)
                (let-values (((leaf off) (pv-leaf-for p i)))
                  (let ((run (fxmin (fx- (vector-length leaf) off) (fx- n i))))
                    (let cloop ((j 0) (h h))
                      (if (fx>=? j run)
                          (loop (fx+ i run) h)
                          (cloop (fx+ j 1)
                                 (i32 (hash-fx+ (hash-fx* 31 h)
                                              (jolt-hasheq (vector-ref leaf (fx+ off j))))))))))))))))

;; Seq hasheq, cached per HEAD object — the JVM's ASeq._hasheq. A cseq chain is
;; memoized-lazy: realized cells never change, so the ordered hash of a given
;; head is fixed once computed (hash-ordered realizes the chain to compute it,
;; exactly as ASeq.hasheq walks it). Same weak side table shape as
;; procedure-hasheq/jrec-hash-cached — no cseq layout change (chez-cseq-v6 is
;; image surface), nothing travels into a fasl, entries die with their seq.
;; Only cseq/lazyseq heads cache; other sequentials (empty list, host arrays'
;; seq views) compute directly as before. Compute outside the lock:
;; hash-ordered recurses into elements, and a seq of seqs re-enters.
(define seq-hasheq-tbl (make-weak-eq-hashtable))
(define seq-hasheq-mu (make-mutex))
(define (seq-hasheq-cached x)
  (if (or (cseq? x) (jolt-lazyseq? x))
      (or (jolt-with-mutex seq-hasheq-mu (hashtable-ref seq-hasheq-tbl x #f))
          (let ((h (hash-ordered (jolt-seq x))))
            (jolt-with-mutex seq-hasheq-mu (hashtable-set! seq-hasheq-tbl x h))
            h))
      (hash-ordered (jolt-seq x))))

(define (jolt-hasheq x)
  ;; Fast path for the most common types (matching Util.hasheq order).
  (cond
    ((jolt-nil? x) 0)
    ((keyword? x) (keyword-t-khash x))
    ;; Symbols sit up here with keywords, not down in the fallback cond behind two
    ;; arm-registry walks. A keyword-to-symbol conversion feeding a map lookup is a
    ;; hot path (honeysql's clause walk), and reaching symbol-hasheq the long way
    ;; cost 82.5 ns against 24 ns for the equivalent keyword.
    ((jolt-symbol? x) (symbol-hasheq x))
    ;; Fixnum: hot path for integer-keyed maps. Hand-inlined murmur to
    ;; avoid the layered dispatch chain (key-hash→jolt-hasheq→cond→
    ;; hashLong→mixK1→mixH1→fmix). All fixnums use hashLong (count=8)
    ;; matching JVM's Long.hasheq.
    ((fixnum? x) (murmur3-hash-long-flat x))
    ((string? x) (string-hasheq x))
    ;; Collections sit up here for the same reason symbols do, and the cost they
    ;; were paying is worse: a collection reached the fallback only after walking
    ;; BOTH registries (jolt-hasheq-arms, then jolt-hash-arms), and a map or set
    ;; already CACHES its hasheq — so the two walks were the entire cost of every
    ;; repeat hash, paid again per nested collection. Loading jolt-lang/time, whose
    ;; __register-eq!/__register-hash! arm predicates are Clojure fns called
    ;; through jolt-invoke, made (hash {:a 1 :b 2}) 8.4x slower purely from this.
    ;; Routing is copied from jolt-hasheq-fallback, so hash VALUES are unchanged.
    ;; pvec before the generic sequential walk: cached field + leaf-run compute.
    ((pvec? x) (pvec-hasheq-cached x))
    ((jolt-sequential? x) (seq-hasheq-cached x))
    ((pmap? x) (jolt-hasheq-fallback x))
    ((pset? x) (jolt-hasheq-fallback x))
    ;; jrec ahead of the walk: the hasheq slot answers a repeat hash in one
    ;; read — this is key-hash's path, so a record map key costs a field read.
    ;; A jrec probe in hash-fast-probes keeps any arm from claiming one.
    ((jrec? x) (jrec-hasheq-fast x))
    ;; Ahead of the arm walk, like keywords/symbols/collections: a procedure is
    ;; in hash-fast-probes, so no arm may claim one (values.ss guard).
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
       ;; Ratio: exact non-integer. hasheq = BigInteger.hashCode(numer) ^ BigInteger.hashCode(denom)
       ((and (exact? x) (not (integer? x)))
        (i32 (bitwise-xor (big-integer-hashcode (numerator x))
                          (big-integer-hashcode (denominator x)))))
       ;; BigInt / bignum: if fits in long → hashLong, else BigInteger.hashCode
       ;; For values within 64-bit signed range, use hashLong.
       ((and (exact? x) (integer? x)
             (>= x -9223372036854775808) (<= x 9223372036854775807))
        (murmur3-hash-long x))
       ;; Bignum > 64-bit: BigInteger.hashCode (JVM parity).
       (else (big-integer-hashcode x))))
    ((boolean? x) (if x 1231 1237))
    ((char? x) (char->integer x))    ;; Character.hashCode = (int) charValue
    ((jolt-symbol? x) (symbol-hasheq x))
    ;; Sequential (vector/list/seq) → hashOrdered (Murmur3.hashOrdered)
    ((jolt-sequential? x) (seq-hasheq-cached x))
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
    ((procedure? x) (procedure-hasheq x))   ; direct fallback callers too
    ;; equal-hash can exceed the 32-bit hasheq range (a bare procedure's was
    ;; 669430755266); clamp so every hasheq is a JVM int.
    (else (i32 (equal-hash x)))))

;; ============================================================================
;; Quick sanity: export a helper for the natives to rebind clojure.core/hash

