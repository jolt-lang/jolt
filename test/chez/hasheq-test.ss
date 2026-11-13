;; test/chez/hasheq-test.ss — the hash engine's VALUES are a wire format.
;; Run: chez --script test/chez/hasheq-test.ss (wired into `make ci` as `hasheq`).
;;
;; hasheq.ss is performance-sensitive and gets tuned (the 32-bit leaf helpers are
;; macros precisely so they inline), and every tuning pass is a chance to change a
;; hash VALUE rather than just its cost. That is not a normal regression: a wrong
;; hash still returns an integer, maps still work, `=` still answers correctly, and
;; nothing fails until something compares a hash across the JVM boundary or a
;; serialized structure is re-read. So the values are pinned here against JVM
;; Clojure directly.
;;
;; Golden values were produced by JVM Clojure 1.13.0-alpha6 and are literal in this
;; file rather than computed, so a refactor cannot drift both sides together:
;;
;;   clj -e '(import [clojure.lang Murmur3 Util])
;;           (println (Murmur3/hashUnencodedChars "select"))
;;           (println (Util/hasheq (clojure.lang.Symbol/intern nil "select")))'
;;
;; Structure:
;;   1. Murmur3.hashUnencodedChars golden values (ASCII, BMP, astral, empty)
;;   2. Symbol.hasheq / Keyword.hasheq golden values (the hashCombine path)
;;   3. Util.hashCombine golden values
;;   4. Long/int hasheq golden values, and (4b) the COLD arms — doubles, longs
;;      past int32, bignums past int64 — which reach u32 with a non-fixnum
;;   5. an EQUIVALENCE sweep of the flat (macro) mixers against the layered
;;      (procedure) mixers over every length 0..64 and every char class — the
;;      property that made flattening safe, kept as a property rather than a
;;      one-time check, since the layered forms survive for the bignum paths
;;   6. the symbol khash cache: a cached read must equal a fresh compute
;;   7. jolt-hasheq agreement with jolt= (the HAMT's correctness condition)

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))
(define (eqv-ok name got want)
  (set! total (+ total 1))
  (unless (equal? got want)
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a — got ~a want ~a\n" name got want)))

;; --- 1. Murmur3.hashUnencodedChars, against the JVM ------------------------
(printf "== 1. Murmur3.hashUnencodedChars golden values (JVM Clojure) ==\n")
(for-each
 (lambda (p)
   (eqv-ok (string-append "hashUnencodedChars " (format "~s" (car p)))
           (murmur3-hash-unencoded-chars (car p)) (cdr p)))
 (list (cons "" 0)
       (cons "a" 1867108634)
       (cons "ab" 374890698)
       (cons "abc" 1118836419)
       (cons "select" 203636772)
       (cons "ns" -732690158)
       (cons "qual" -380830177)
       (cons "clojure.core" 878072082)
       (cons "some.qualified/entity-name" -2018759251)
       ;; BMP non-ASCII: one UTF-16 unit per char, like ASCII
       (cons "\xe9;" 1105794559)
       (cons "\x65e5;\x672c;\x8a9e;" 1004281861)
       ;; astral: ONE Chez char, TWO UTF-16 units — the surrogate expansion this
       ;; file's loop does and Java's charAt gets for free
       (cons (string (integer->char #x1F600)) 1443257913)))

;; --- 2. Symbol / Keyword hasheq --------------------------------------------
;; Symbol.hasheq = hashCombine(Murmur3.hashUnencodedChars(name), Util.hash(ns)),
;; where Util.hash(ns) is ns.hashCode() (the 31-based String.hashCode, NOT murmur)
;; and a nil ns contributes 0. Keyword.hasheq is that + 0x9e3779b9.
(printf "\n== 2. Symbol.hasheq / Keyword.hasheq golden values ==\n")
(eqv-ok "(hasheq 'select)"       (jolt-hasheq (jolt-symbol #f "select"))       -1506602266)
(eqv-ok "(hasheq 'ns/qual)"      (jolt-hasheq (jolt-symbol "ns" "qual"))        42381530)
(eqv-ok "(hasheq 'clojure.core)" (jolt-hasheq (jolt-symbol #f "clojure.core")) -189332625)
(eqv-ok "(hasheq 'a/b)"          (jolt-hasheq (jolt-symbol "a" "b"))          -1172211204)
;; a keyword is its symbol's hasheq + 0x9e3779b9, so these two must stay in step
(ok "keyword hasheq = symbol hasheq + 0x9e3779b9"
    (= (jolt-hasheq (keyword #f "select"))
       (i32 (+ (jolt-hasheq (jolt-symbol #f "select")) #x9e3779b9))))
(ok "same for a namespaced pair"
    (= (jolt-hasheq (keyword "ns" "qual"))
       (i32 (+ (jolt-hasheq (jolt-symbol "ns" "qual")) #x9e3779b9))))

;; --- 3. Util.hashCombine ----------------------------------------------------
(printf "\n== 3. Util.hashCombine golden values ==\n")
(eqv-ok "hashCombine(203636772, 0)"  (hash-combine 203636772 0)  -1506602266)
(eqv-ok "hashCombine(0, 0)"          (hash-combine 0 0)          -1640531527)
;; a NEGATIVE seed exercises the arithmetic (sign-extending) >> that Java uses and
;; a logical >>> would get wrong
(eqv-ok "hashCombine(-1, 0)"         (hash-combine -1 0)          1640531591)
(eqv-ok "hashCombine(-732690158, 0)" (hash-combine -732690158 0)  2082130287)

;; --- 4. Long / int hasheq ---------------------------------------------------
(printf "\n== 4. fixnum hasheq golden values ==\n")
(eqv-ok "(hasheq 0)"   (jolt-hasheq 0)   0)
(eqv-ok "(hasheq 1)"   (jolt-hasheq 1)   1392991556)
(eqv-ok "(hasheq 7)"   (jolt-hasheq 7)   -137604029)
(eqv-ok "(hasheq -1)"  (jolt-hasheq -1)  1651860712)
(eqv-ok "(hasheq 100)" (jolt-hasheq 100) -970256272)

;; JVM Murmur3.hashLong goldens on either side of the two common high words.
(for-each
 (lambda (p)
   (let ((k (car p)) (h (cdr p)))
     (eqv-ok (format "hasheq ~a" k) (jolt-hasheq k) h)
     (eqv-ok (format "HAMT hash ~a" k) (key-hash k) (bitwise-and h #xffffffff))))
 '((-4294967297 . -284799838) (-4294967296 . -805887626) (-4294967295 . -173965278)
   (4294967295 . 689913327) (4294967296 . 987256456) (4294967297 . 953780890)))

;; --- 4b. the COLD arms: doubles, longs past int32, bignums past int64 --------
;; These reach u32 with a NON-FIXNUM — murmur3-hash-long's bignum arm masks a
;; 64-bit value, and double-hasheq feeds it a double's raw bits — which is why u32
;; alone among the 32-bit helpers must stay generic. Replacing its bitwise-and with
;; an unsafe fxand does not error, it silently answers wrong, and the only thing
;; that noticed was the corpus (`hash double 1.5`, `hash large long`). Pinned here
;; so the hash engine's own gate catches it next time.
(printf "\n== 4b. cold arms: doubles, wide longs, bignums ==\n")
(eqv-ok "(hasheq 1.5)"     (jolt-hasheq 1.5)      1073217536)
(eqv-ok "(hasheq 0.0)"     (jolt-hasheq 0.0)      0)
(eqv-ok "(hasheq -1.5)"    (jolt-hasheq -1.5)     -1074266112)
(eqv-ok "(hasheq 3.14159)" (jolt-hasheq 3.14159)  -1340954729)
(eqv-ok "(hasheq 1.0e300)" (jolt-hasheq 1.0e300)  -164130400)
;; just past int32, both signs
(eqv-ok "(hasheq 2147483648)"  (jolt-hasheq 2147483648)  386683422)
(eqv-ok "(hasheq -2147483649)" (jolt-hasheq -2147483649) 75387528)
;; the int64 extremes — bignums on a 61-bit fixnum tower
(eqv-ok "(hasheq Long/MAX_VALUE)" (jolt-hasheq 9223372036854775807)  -2106506049)
(eqv-ok "(hasheq Long/MIN_VALUE)" (jolt-hasheq -9223372036854775808) 1366273829)
(eqv-ok "(hasheq 1234567890123)"  (jolt-hasheq 1234567890123)        1740798302)
;; past int64 -> BigInteger.hashCode, a different arm again
(eqv-ok "(hasheq a >64-bit bigint)"
        (jolt-hasheq 123456789012345678901234567890) 1915528825)

;; --- 5. flat (macro) vs layered (procedure) mixers --------------------------
;; hasheq.ss keeps BOTH forms: the -flat macros for the hot paths and the
;; procedures for the cold bignum ones. They must agree, and this is the property
;; that made it safe to flatten the string loop. Reproduced here as the layered
;; loop, so the test fails if either side is changed alone.
(printf "\n== 5. flat mixers agree with the layered mixers, every length and char class ==\n")
(define (layered-hash-unencoded-chars s)
  (let ((len (string-length s)))
    (let loop ((i 0) (h1 murmur3-seed) (pending #f) (count 0))
      (if (fx>=? i len)
          (if pending
              (let* ((k1 (murmur3-mix-k1 pending)) (h1 (bitwise-xor h1 k1)))
                (murmur3-fmix h1 (fx* 2 (fx+ count 1))))
              (murmur3-fmix h1 (fx* 2 count)))
          (let ((cp (char->integer (string-ref s i))))
            (if (fx<? cp #x10000)
                (if pending
                    (let* ((k1 (murmur3-mix-k1
                                (bitwise-ior pending (bitwise-arithmetic-shift-left cp 16))))
                           (h1 (murmur3-mix-h1 h1 k1)))
                      (loop (fx+ i 1) h1 #f (fx+ count 2)))
                    (loop (fx+ i 1) h1 cp count))
                (let* ((cp2 (fx- cp #x10000))
                       (high (fxior #xD800 (fxsra cp2 10)))
                       (low  (fxior #xDC00 (fxand cp2 #x3FF))))
                  (if pending
                      (let* ((k1 (murmur3-mix-k1
                                  (bitwise-ior pending (bitwise-arithmetic-shift-left high 16))))
                             (h1 (murmur3-mix-h1 h1 k1)))
                        (loop (fx+ i 1) h1 low (fx+ count 2)))
                      (let* ((k1 (murmur3-mix-k1
                                  (bitwise-ior high (bitwise-arithmetic-shift-left low 16))))
                             (h1 (murmur3-mix-h1 h1 k1)))
                        (loop (fx+ i 1) h1 #f (fx+ count 2)))))))))))

(define sweep-mismatches 0)
(define sweep-cases 0)
(define (sweep s)
  (set! sweep-cases (+ sweep-cases 1))
  (unless (= (layered-hash-unencoded-chars s) (murmur3-hash-unencoded-chars s))
    (set! sweep-mismatches (+ sweep-mismatches 1))
    (printf "  flat/layered mismatch on ~s: layered=~a flat=~a\n"
            s (layered-hash-unencoded-chars s) (murmur3-hash-unencoded-chars s))))
;; every length 0..64 — the ODD lengths are what exercise the trailing-unit branch
(let loop ((i 0)) (when (<= i 64) (sweep (make-string i #\a)) (loop (+ i 1))))
;; astral at every alignment: leading, trailing, adjacent, and interleaved, which is
;; where the `pending` state machine differs from Java's fixed 2-char stride
(let ((e (integer->char #x1F600)) (f (integer->char #x10000)) (g (integer->char #x10FFFF)))
  (for-each sweep
            (list "" "a" "ab" "abc" "select" "\xe9;" "\xe9;\xe9;" "a\xe9;" "\xe9;a"
                  (string e) (string e #\a) (string #\a e) (string e e)
                  (string #\a e #\b) (string e #\a e) (string #\a #\b e)
                  (string f) (string g) (string f g) (string #\a f #\b g))))
(ok (string-append "flat and layered agree over " (number->string sweep-cases) " strings")
    (= 0 sweep-mismatches))

;; --- 6. the symbol khash cache ---------------------------------------------
;; symbol-t carries its hasheq in a lazily-filled field. A cached read must equal
;; the fresh compute, and a symbol rebuilt only to change metadata must inherit the
;; cache rather than recompute (or drift).
(printf "\n== 6. the symbol khash cache ==\n")
(let ((s1 (jolt-symbol "ns" "qual")))
  (ok "khash starts unfilled" (not (symbol-t-khash s1)))
  (let ((h (jolt-hasheq s1)))
    (ok "khash is filled after the first hash" (eqv? (symbol-t-khash s1) h))
    (ok "the cached read equals the fresh compute"
        (= h (compute-symbol-hasheq "ns" "qual")))
    (ok "a second read is the same value" (= h (jolt-hasheq s1)))
    (let ((s2 (symbol-t-with-meta s1 jolt-nil)))
      (ok "with-meta inherits the cache" (eqv? (symbol-t-khash s2) h))
      (ok "and hashes equal" (= (jolt-hasheq s2) h)))))
;; a fresh symbol with no cache must hash the same as a cached one
(ok "fresh and cached symbols hash alike"
    (= (jolt-hasheq (jolt-symbol #f "select"))
       (jolt-hasheq (jolt-symbol #f "select"))))

;; --- 6b. the per-NAME murmur memo (symbol-t-ncell) --------------------------
;; The khash field above only helps a symbol that gets hashed twice. The shape that
;; matters — (get m (symbol (name k))) — builds a fresh symbol per call, so the
;; murmur is memoized on the name's POOL CELL instead and each symbol keeps a
;; pointer to it. Two things have to hold: symbols sharing a name share one cell
;; (or nothing is amortised), and the memoized answer equals the unmemoized one
;; (or every hash is silently wrong for names hashed a second time).
(printf "\n== 6b. the per-name murmur memo ==\n")
(let* ((nm (string-append "clause" "-memo"))       ; built, so not a literal's object
       (a (jolt-symbol #f nm))
       (b (jolt-symbol #f (string-append "clause" "-memo"))))
  (ok "two symbols built from equal names share one cell"
      (eq? (symbol-t-ncell a) (symbol-t-ncell b)))
  (ok "the name string is canonicalized to the cell's"
      (eq? (symbol-t-name a) (symbol-t-name b)))
  (ok "the memo starts empty" (not (symstr-mhash (symbol-t-ncell a))))
  (let ((ha (jolt-hasheq a)))
    (ok "hashing fills the memo"
        (eqv? (symstr-mhash (symbol-t-ncell a)) (murmur3-hash-unencoded-chars nm)))
    (ok "a symbol built later reads the memo and agrees" (= ha (jolt-hasheq b)))
    (ok "and both agree with the unmemoized compute"
        (= ha (compute-symbol-hasheq #f nm)))))
;; the ns half is NOT memoized, so a qualified symbol has to reach the same answer
;; through symbol-hasheq's string? test as compute-symbol-hasheq does through its
;; three-way nil/#f/() test. Every spelling of "no namespace" must agree.
(let ((h (compute-symbol-hasheq #f "bare")))
  (ok "an absent ns hashes alike however it is spelled"
      (and (= h (jolt-hasheq (jolt-symbol #f "bare")))
           (= h (jolt-hasheq (jolt-symbol jolt-nil "bare")))
           (= h (jolt-hasheq (jolt-symbol '() "bare"))))))
;; Every symbol reachable from jolt code has a cell, which is what lets the memo
;; path be the only one that matters. (symbol 42) is rejected before it gets here —
;; a non-string name has never been hashable, since both the memo path and
;; compute-symbol-hasheq end in a murmur that indexes the name as a string — so the
;; cell-less branch of make-symbol-t/pooled is reachable only by an internal caller
;; going straight to make-symbol-t.
(ok "symbols built through the public constructors all carry a cell"
    (and (symbol-t-ncell (jolt-symbol #f "bare"))
         (symbol-t-ncell (jolt-symbol "ns" "qual"))
         (symbol-t-ncell (jolt-symbol/meta #f "meta" jolt-nil))
         (symbol-t-ncell (symbol-t-with-meta (jolt-symbol #f "bare") jolt-nil))))

;; --- 7. hasheq agrees with jolt= (the HAMT's correctness condition) --------
;; Two values that are jolt= MUST hash alike, or a map lookup finds the right
;; bucket and then fails the equality check. Symbols are the interesting case
;; because they are not interned, so two distinct records must agree.
(printf "\n== 7. hasheq agrees with jolt= ==\n")
(define (agree? a b)
  (if (jolt=2 a b) (= (jolt-hasheq a) (jolt-hasheq b)) #t))
(ok "two distinct 'select records" (agree? (jolt-symbol #f "select") (jolt-symbol #f "select")))
(ok "two distinct 'ns/qual records" (agree? (jolt-symbol "ns" "qual") (jolt-symbol "ns" "qual")))
(ok "symbol with meta vs without"
    (agree? (jolt-symbol "ns" "qual") (symbol-t-with-meta (jolt-symbol "ns" "qual") jolt-nil)))
(ok "a bare-name and a ns'd symbol differ"
    (not (jolt=2 (jolt-symbol #f "qual") (jolt-symbol "ns" "qual"))))
;; and a symbol used as a real map key round-trips through a distinct record
(let ((m (jolt-assoc (jolt-hash-map) (jolt-symbol #f "select") 42)))
  (ok "get by a DISTINCT but equal symbol record"
      (eqv? 42 (jolt-get m (jolt-symbol #f "select") jolt-nil)))
  (ok "get by a with-meta copy"
      (eqv? 42 (jolt-get m (symbol-t-with-meta (jolt-symbol #f "select") jolt-nil) jolt-nil))))

(printf "\nhasheq-test: ~a checks, ~a failure(s)\n" total fails)
(if (zero? fails)
    (begin (printf "hasheq-test: PASS — hash values pinned to JVM Clojure\n") (exit 0))
    (exit 1))
