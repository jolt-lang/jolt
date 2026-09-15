;; misc scalar natives — UUID, tagged-literal, bigint, and the hash API. (format /
;; printf moved to natives-format.ss.)
;;
;; Loaded after the printers (pr-str of a uuid is #uuid "…") and converters
;; (jolt-str-render-one for %s / str of a uuid).

;; --- UUID --------------------------------------------------------------------
;; A uuid is a record wrapping its canonical 36-char lowercase string. str -> the
;; bare string; pr-str -> #uuid "…"; not map?/coll?.
(define-record-type juuid (fields s) (nongenerative chez-juuid-v1))
(define (jolt-uuid-pred? x) (juuid? x))

(define hexd "0123456789abcdef")
;; Position of hex digit j within the 8-4-4-4-12 layout: one extra character for
;; each hyphen already passed (they sit after hex digits 8, 12, 16 and 20).
(define (uuid-hex-pos j)
  (fx+ j
       (if (fx>=? j 8) 1 0) (if (fx>=? j 12) 1 0)
       (if (fx>=? j 16) 1 0) (if (fx>=? j 20) 1 0)))
;; v4 from 16 bytes of OS entropy (jolt-random-bytes), NOT from the seeded PRNG:
;; v4 UUIDs get used as session ids and reset tokens, where a guessable value is a
;; forgeable one. RFC 4122 then overwrites 6 of the 128 bits — version 4 in the
;; high nibble of byte 6, variant 10 in the top two bits of byte 8 — leaving 122
;; random bits.
(define (random-uuid-str)
  (let ((b (jolt-random-bytes 16))
        (cs (make-string 36)))
    (bytevector-u8-set! b 6 (fxior (fxand (bytevector-u8-ref b 6) #x0f) #x40))
    (bytevector-u8-set! b 8 (fxior (fxand (bytevector-u8-ref b 8) #x3f) #x80))
    (string-set! cs 8 #\-) (string-set! cs 13 #\-)
    (string-set! cs 18 #\-) (string-set! cs 23 #\-)
    (let loop ((i 0))
      (if (fx=? i 16)
          cs
          (let ((v (bytevector-u8-ref b i)))
            (string-set! cs (uuid-hex-pos (fx* i 2))
                         (string-ref hexd (fxarithmetic-shift-right v 4)))
            (string-set! cs (uuid-hex-pos (fx+ (fx* i 2) 1))
                         (string-ref hexd (fxand v 15)))
            (loop (fx+ i 1)))))))
(define (jolt-random-uuid) (make-juuid (random-uuid-str)))

;; #uuid literal -> a uuid value (the emitter lowers the :uuid node to this). The
;; reader already validated the shape; lowercase for value equality.
(define (jolt-uuid-from-string s) (make-juuid (string-downcase s)))

;; parse-uuid: validate the canonical shape (8-4-4-4-12 hex), lowercase, -> uuid;
;; nil if the string doesn't conform (Clojure parse-uuid), error on a non-string.
(define (hex-char? c) (or (and (char>=? c #\0) (char<=? c #\9))
                          (and (char>=? c #\a) (char<=? c #\f))
                          (and (char>=? c #\A) (char<=? c #\F))))
(define (uuid-shape? s)
  (and (string? s) (fx=? (string-length s) 36)
       (let loop ((i 0))
         (if (fx=? i 36) #t
             (let ((c (string-ref s i)))
               (cond ((or (fx=? i 8) (fx=? i 13) (fx=? i 18) (fx=? i 23)) (and (char=? c #\-) (loop (fx+ i 1))))
                     ((hex-char? c) (loop (fx+ i 1)))
                     (else #f)))))))
(define (jolt-parse-uuid s)
  (cond ((not (string? s)) (throw-jvm (quote ClassCastException) (string-append (jolt-final-str s) " cannot be cast to java.lang.String")))
        ((uuid-shape? s) (make-juuid (string-downcase s)))
        (else jolt-nil)))

;; uuid? / random-uuid / parse-uuid are OVERLAY fns (they read :jolt/type), so
;; the prelude would clobber a def-var! here — they're asserted in post-prelude.ss.

;; --- java.util.UUID bit views ------------------------------------------------
;; The canonical string is the stored representation; the JVM's two-long view is
;; derived by parsing its 32 hex digits. The longs are SIGNED, and that is
;; observable: compareTo orders by signed msb then lsb, so a uuid with the high
;; bit set sorts FIRST — before the nil uuid (UUID.compareTo). The instance
;; method surface over these lives with the ctors in java/io.ss.
(define (uuid-hexv c)
  (cond ((and (char>=? c #\0) (char<=? c #\9)) (fx- (char->integer c) 48))
        ((and (char>=? c #\a) (char<=? c #\f)) (fx- (char->integer c) 87))
        (else (fx- (char->integer c) 55))))
;; unsigned value of the 16 hex digits starting at digit index `start` (0 = msb,
;; 16 = lsb), mapped through the 8-4-4-4-12 layout by uuid-hex-pos.
(define (uuid-half-u s start)
  (let loop ((j start) (acc 0))
    (if (fx=? j (fx+ start 16))
        acc
        (loop (fx+ j 1) (+ (* acc 16) (uuid-hexv (string-ref s (uuid-hex-pos j))))))))
(define (uuid-msb-u u) (uuid-half-u (juuid-s u) 0))
(define (uuid-lsb-u u) (uuid-half-u (juuid-s u) 16))
(define (uuid-u64->s64 n) (if (>= n #x8000000000000000) (- n #x10000000000000000) n))
(define (uuid-cmp a b)
  (let ((ma (uuid-u64->s64 (uuid-msb-u a))) (mb (uuid-u64->s64 (uuid-msb-u b))))
    (cond ((< ma mb) -1) ((> ma mb) 1)
          (else (let ((la (uuid-u64->s64 (uuid-lsb-u a))) (lb (uuid-u64->s64 (uuid-lsb-u b))))
                  (cond ((< la lb) -1) ((> la lb) 1) (else 0)))))))
;; a uuid is Comparable — compare / sort / sorted colls agree with .compareTo.
(register-compare-arm! (lambda (a b) (and (juuid? a) (juuid? b)))
                       (lambda (a b) (uuid-cmp a b)))

;; str of a uuid -> the bare 36-char string; pr-str -> #uuid "…".
(register-str-render! juuid? juuid-s)
(define (juuid-pr u) (string-append "#uuid \"" (juuid-s u) "\""))
(register-pr-arm! juuid? juuid-pr)
;; two uuids are = iff same string.
(register-eq-arm! (lambda (a b) (or (juuid? a) (juuid? b)))
                  (lambda (a b) (and (juuid? a) (juuid? b) (string=? (juuid-s a) (juuid-s b)))))

;; --- bigint / biginteger -----------------------------------------------------
;; JVM bigint/biginteger coerce: string → parsed integer, double/float →
;; truncated integer, ratio → quotient, integer → exact integer.
(define (jolt-bigint x)
  (cond ((string? x) (parse-int-or-throw x 10 "big"))
        ((flonum? x)
         (if (or (finite? x) (zero? x))
             (inexact->exact (truncate x))
             (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                           (string-append "For input string: \""
                                          (jolt-str-render-one x) "\"")))))
        (else (inexact->exact (truncate x)))))
(def-var! "clojure.core" "bigint" jolt-bigint)
(def-var! "clojure.core" "biginteger" jolt-bigint)

;; --- tagged-literal ----------------------------------------------------------
;; (tagged-literal tag form): a tagged value with :tag / :form. tagged-literal? is
;; overlay (reads :jolt/type) so it's overridden in post-prelude.ss.
(define-record-type jtagged (fields tag form) (nongenerative chez-jtagged-v1))
(define (jolt-tagged-literal tag form) (make-jtagged tag form))
(define (jolt-tagged-literal-pred? x) (jtagged? x))
(define kw-tl-tag (keyword #f "tag"))
(define kw-tl-form (keyword #f "form"))
(register-get-arm! jtagged?
  (lambda (coll k d)
    (cond ((jolt=2 k kw-tl-tag) (jtagged-tag coll))
          ((jolt=2 k kw-tl-form) (jtagged-form coll))
          (else d))))
(define (jtagged-pr t) (string-append "#" (jolt-pr-str (jtagged-tag t)) " " (jolt-pr-readable (jtagged-form t))))
(register-pr-arm! jtagged? jtagged-pr)
;; two tagged literals are = iff same tag and (recursively) = form, like the JVM's
;; TaggedLiteral — so they work as map keys / set members. (jolt-hash already
;; hashes the fields structurally, so eq/hash stay consistent.)
(register-eq-arm! (lambda (a b) (or (jtagged? a) (jtagged? b)))
                  (lambda (a b) (and (jtagged? a) (jtagged? b)
                                     (jolt=2 (jtagged-tag a) (jtagged-tag b))
                                     (jolt=2 (jtagged-form a) (jtagged-form b)))))
(def-var! "clojure.core" "tagged-literal" jolt-tagged-literal)
;; tagged-literal? is OVERLAY (reads :jolt/type) — asserted in post-prelude.ss.

;; --- hash family (JVM-compatible via hasheq.ss) ------------------------------
;; Object.hashCode parity: Java's specified String hash and Clojure's Symbol hash
;; (Util.hashCombine), so (.hashCode s) / (.hashCode sym) match the JVM. 32-bit int.
;; Read by natives-str.ss (String.hashCode) and by the keyword/symbol arms in
;; records-dispatch.ss — a shared file, which is why these live here and not in
;; the java/ tree the Gambit boot excludes.
(define (jolt-u32 x) (bitwise-and x #xFFFFFFFF))
(define (jolt-s32 x) (let ((m (jolt-u32 x))) (if (>= m #x80000000) (- m #x100000000) m)))
(define (java-string-hash s)
  (let ((n (string-length s)))
    (let loop ((i 0) (h 0))
      (if (fx<? i n)
          (loop (fx+ i 1) (jolt-s32 (+ (* 31 h) (char->integer (string-ref s i)))))
          (jolt-s32 h)))))
(define (java-hash-combine seed hash)
  (let* ((su (jolt-u32 seed))
         (sl (bitwise-arithmetic-shift-left su 6))
         (sr (bitwise-arithmetic-shift-right (jolt-s32 su) 2))
         (add (+ (jolt-u32 hash) #x9e3779b9 sl sr)))
    (jolt-s32 (bitwise-xor su (jolt-u32 add)))))
(define (java-symbol-hash name ns)
  (java-hash-combine (java-string-hash name) (if ns (java-string-hash ns) 0)))

;; Java .hashCode() for a collection (java.util.Map/Set/List semantics), NOT the
;; Murmur3 hasheq that clojure.core/hash uses. A library computing .hashCode on its
;; own collection type (flatland's OrderedMap via APersistentMap/mapHash, OrderedSet
;; summing element .hashCodes) must agree with jolt's builtins, so map/set/vector
;; .hashCode go here. Recursive: a nested collection element hashes the same way; a
;; scalar routes to its own .hashCode. Sums use exact ints (jolt + is unbounded, as
;; a Clojure (reduce + …) over element hashCodes is) except the map form, which
;; mirrors APersistentMap.mapHash's 32-bit int accumulation.
(define (jolt-java-hashcode x)
  (cond
    ((jolt-nil? x) 0)
    ((pmap? x)
     (pmap-fold x (lambda (k v a)
                    (i32 (+ a (bitwise-xor (jolt-java-hashcode k) (jolt-java-hashcode v))))) 0))
    ((pset? x)
     (pset-fold x (lambda (e a) (if (jolt-nil? e) a (+ a (jolt-java-hashcode e)))) 0))
    ((pvec? x)
     (let ((n (pvec-count x)))
       (let loop ((i 0) (h 1))
         (if (fx>=? i n) h
             (loop (fx+ i 1) (i32 (+ (* 31 h) (jolt-java-hashcode (pvec-nth-d x i jolt-nil)))))))))
    ((or (cseq? x) (empty-list-t? x) (jolt-lazyseq? x))
     (let loop ((s (jolt-seq x)) (h 1))
       (if (jolt-nil? s) h
           (loop (jolt-seq (seq-more s)) (i32 (+ (* 31 h) (jolt-java-hashcode (seq-first s))))))))
    ;; a jrec is jolt-map? (so dot-coll?) — route it here directly, NOT through
    ;; record-method-dispatch, which would re-enter the .hashCode arm and loop. A
    ;; declared hashCode governs (flatland's types via APersistentMap/mapHash);
    ;; else the structural record hash.
    ((jrec? x) (let ((m (find-method-any-protocol (jrec-tag x) "hashCode")))
                 (if m (jolt-invoke m x) (jrec-hash x))))
    (else (record-method-dispatch x "hashCode" jolt-nil))))
(def-var! "jolt.host" "java-hashcode" jolt-java-hashcode)

;; Replaces the old 24-bit masked hash with JVM Murmur3 hasheq.
(define (nm-hash x) (jolt-hasheq x))
;; clojure.core/hash-combine (core_deftype.clj) is
;;   (Util/hashCombine x (Util/hash y))
;; — the first argument is a seed the caller already has as an int, the second is
;; a VALUE that gets hashed on the way in. Passing y through raw made
;; (hash-combine 0 "a") throw out of bitwise-and instead of hashing, so any
;; ported library folding hash-combine over values (compliment hashes the
;; classpath strings that way) died on the first non-integer.
(define (nm-hash-combine seed x) (hash-combine seed (jolt-java-hashcode x)))
(define (nm-hash-ordered-coll coll) (hash-ordered (jolt-seq coll)))
(define (nm-hash-unordered-coll coll) (hash-unordered (jolt-seq coll)))
(define (nm-mix-collection-hash hash-basis count) (mix-coll-hash hash-basis count))
(def-var! "clojure.core" "hash" nm-hash)
(def-var! "clojure.core" "hash-combine" nm-hash-combine)
(def-var! "clojure.core" "hash-ordered-coll" nm-hash-ordered-coll)
(def-var! "clojure.core" "mix-collection-hash" nm-mix-collection-hash)
(def-var! "clojure.core" "hash-unordered-coll" nm-hash-unordered-coll)
