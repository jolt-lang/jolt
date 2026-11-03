;; natives-str.ss — java.lang.String method interop on Chez.
;;
;; (.method s arg*) on a string target lowers to record-method-dispatch (emit.ss),
;; which falls through to jolt-string-method here when the target is a string.
;; Covers the
;; portable java.lang.String/CharSequence methods cljc libraries actually call.
;; Case handling here is Java's, in two flavours that must not be confused:
;; toUpperCase / toLowerCase map the whole of Unicode, while the ignore-case
;; COMPARISONS use Java's own per-character upper-then-lower fold
;; (jvm-char-ci-diff), which is neither that nor Scheme's full case folding. The
;; ascii-string-up/down pair below is for neither — it serves the ASCII-only
;; lookups other files do (a month abbreviation, a charset alias, "true").
;; indexOf returns -1 on miss as on the JVM, indices come in as flonums, char
;; results are Scheme chars, and numeric results are flonums to match jolt's
;; number model.
;;
;; Loaded from rt.ss AFTER regex.ss (the regex methods reuse jolt-re-pattern /
;; regex-t-irx) and records.ss (which calls jolt-string-method).

;; --- ASCII case mapping (byte-oriented) -------
(define (ascii-down-char c)
  (if (and (char<=? #\A c) (char<=? c #\Z))
      (integer->char (fx+ (char->integer c) 32)) c))
(define (ascii-string-down s)
  (let ((n (string-length s)))
    (let check ((i 0))
      (if (fx=? i n)
          s
          (if (and (char<=? #\A (string-ref s i)) (char<=? (string-ref s i) #\Z))
              (let ((r (make-string n)))
                (do ((j 0 (fx+ j 1)))
                    ((fx=? j n) r)
                  (string-set! r j (ascii-down-char (string-ref s j)))))
              (check (fx+ i 1)))))))

;; Java's per-character case fold, as a DIFFERENCE: 0 when the two characters are
;; equal ignoring case, else the difference of the folded characters. The fold is
;; Character.toUpperCase, then Character.toLowerCase when that still differs.
;;
;; This one function is the whole ignore-case contract on the JVM:
;; equalsIgnoreCase IS regionMatches(true, 0, other, 0, length), and
;; compareToIgnoreCase and CASE_INSENSITIVE_ORDER differ from it only in
;; answering the difference rather than a boolean. So they share it here, and a
;; new ignore-case method has one place to reach for.
;;
;; It is NOT Scheme's string-ci=?, which folds the full Unicode way: the two
;; disagree on "I" vs "\x131;" (dotless i), which Java calls equal because both
;; upper-case to #\I while Unicode case folding maps them to different letters.
;; regionMatches used string-ci=? and answered #f there while equalsIgnoreCase,
;; on the same pair, answered #t — two answers for one JVM operation.
(define (jvm-char-ci-diff ca cb)
  (if (char=? ca cb)
      0
      (let ((ua (char-upcase ca)) (ub (char-upcase cb)))
        (if (char=? ua ub)
            0
            (let ((da (char-downcase ua)) (db (char-downcase ub)))
              (if (char=? da db)
                  0
                  (fx- (char->integer da) (char->integer db))))))))

;; String.compareToIgnoreCase, character for character: the first pair that does
;; not fold equal answers the DIFFERENCE of the folded chars (not merely a sign);
;; equal prefixes answer the length difference. This is also the comparison
;; contract used by String.CASE_INSENSITIVE_ORDER.
(define (jvm-string-ci-compare a b)
  (let ((la (string-length a)) (lb (string-length b)))
    (let loop ((i 0))
      (if (or (fx=? i la) (fx=? i lb))
          (fx- la lb)
          (let ((d (jvm-char-ci-diff (string-ref a i) (string-ref b i))))
            (if (fx=? d 0) (loop (fx+ i 1)) d))))))

;; String.equalsIgnoreCase / String.regionMatches(true, ...): the same fold, as
;; equality. Length first, so a prefix is not equal to what it prefixes.
(define (jvm-string-ci=? a b)
  (and (fx=? (string-length a) (string-length b))
       (fx=? 0 (jvm-string-ci-compare a b))))

;; String.compareTo, the case-sensitive twin of jvm-string-ci-compare and the
;; same shape without the fold: the first differing pair answers the DIFFERENCE
;; of the chars, and equal prefixes the length difference. The JVM's magnitude is
;; not decoration — (.compareTo "a" "c") is -2 and (.compareTo "abcd" "ab") is 2 —
;; and a sign-only answer beside a compareToIgnoreCase that reports the real
;; difference is two contracts for one pair of methods.
;; It is compare's own string order (jolt-strcmp, converters.ss), which also
;; walks a supplementary char as its UTF-16 surrogate pair.
(define (jvm-string-compare a b) (jolt-strcmp a b))

;; Two different notions of whitespace, and the JVM uses both. String.trim drops
;; anything at or below the space character; clojure.string/trim drops whatever
;; Character.isWhitespace accepts, which reaches the Unicode space separators
;; (U+3000 and friends) but NOT the non-breaking ones. Keep them apart: str-trim
;; is String.trim, str-triml / str-trimr / str-trim* back clojure.string.
;; U+001C..U+001F (the file/group/record/unit separators) are whitespace to Java but
;; carry no Unicode White_Space property, so char-whitespace? alone misses them.
(define (java-whitespace? c)
  (let ((cp (char->integer c)))
    (cond ((or (fx=? cp #xA0) (fx=? cp #x2007) (fx=? cp #x202F)) #f)
          ((and (fx>=? cp #x1C) (fx<=? cp #x1F)) #t)
          (else (char-whitespace? c)))))

;; Each of these answers S ITSELF when there is nothing to cut, exactly as
;; String.trim returns `this`. Strings are values here, so handing back the same
;; one is indistinguishable — and the case matters, because trimming a string
;; that needs no trimming is the common call, not the rare one.
(define (str-trim s)
  (let ((len (string-length s)))
    (let scan-l ((i 0))
      (cond ((fx=? i len) "")
            ((char<=? (string-ref s i) #\space) (scan-l (fx+ i 1)))
            (else (let scan-r ((j (fx- len 1)))
                    (if (char<=? (string-ref s j) #\space)
                        (scan-r (fx- j 1))
                        (if (and (fx=? i 0) (fx=? j (fx- len 1)))
                            s
                            (substring s i (fx+ j 1))))))))))
(define (str-triml s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((fx=? i len) "")
            ((java-whitespace? (string-ref s i)) (loop (fx+ i 1)))
            ((fx=? i 0) s)
            (else (substring s i len))))))
(define (str-trimr s)
  (let ((len (string-length s)))
    (let loop ((j (fx- len 1)))
      (cond ((fx<? j 0) "")
            ((java-whitespace? (string-ref s j)) (loop (fx- j 1)))
            ((fx=? j (fx- len 1)) s)
            (else (substring s 0 (fx+ j 1)))))))
;; clojure.string/trim, in ONE pass with at most ONE copy.
;;
;; This was (str-trimr (str-triml s)): two scans, and — because str-triml's
;; else-branch took (substring s i len) even when i was 0 — two FULL COPIES of
;; a string that needed no trimming at all. Trimming short strings measured
;; ~15x babashka (183 ms vs 12 ms for 100k calls) almost entirely on those
;; copies, which is why the fix is the allocation and not the scan.
(define (str-trim* s)
  (let ((len (string-length s)))
    (let scan-l ((i 0))
      (cond ((fx=? i len) "")
            ((java-whitespace? (string-ref s i)) (scan-l (fx+ i 1)))
            (else (let scan-r ((j (fx- len 1)))
                    (if (java-whitespace? (string-ref s j))
                        (scan-r (fx- j 1))
                        (if (and (fx=? i 0) (fx=? j (fx- len 1)))
                            s
                            (substring s i (fx+ j 1))))))))))

;; Java 11's strip family, over the same Character.isWhitespace as clojure.string's
;; trim. String.trim cuts at <= U+0020 — its notion of "space" predates Unicode —
;; where strip also removes the Unicode separators trim leaves behind.
(define (str-strip s left? right?)
  (let ((len (string-length s)))
    (let scan-l ((i 0))
      (cond ((fx=? i len) "")
            ((and left? (java-whitespace? (string-ref s i))) (scan-l (fx+ i 1)))
            (else (let scan-r ((j (fx- len 1)))
                    (if (and right? (fx>? j i) (java-whitespace? (string-ref s j)))
                        (scan-r (fx- j 1))
                        (substring s i (fx+ j 1)))))))))

;; --- substring search: first index of `needle` in `s` at/after `from`, or -1 --
(define (char-by-char-match? s si needle nlen)
  (let loop ((j 0))
    (cond ((fx=? j nlen) #t)
          ((char=? (string-ref s (fx+ si j)) (string-ref needle j)) (loop (fx+ j 1)))
          (else #f))))
;; --- Boyer-Moore-Horspool ----------------------------------------------------
;; The scan below tests one character per position, which is already the cheap
;; version. BMH does better by not visiting most positions at all: it compares
;; the needle's LAST character against the haystack, and on a mismatch skips
;; ahead by however far that haystack character sits from the end of the needle
;; — up to the needle's whole length. On a 6.6 MB haystack and a 6-character
;; needle that is 15.6 ms against 5.9 ms.
;;
;; THE SKIP TABLE IS AN FXVECTOR, and that is the whole difference between this
;; being worth it and not. The same algorithm with an eqv hashtable measured
;; 15.1 ms — no better than the linear scan — because a hashtable lookup per
;; mismatch costs about what the comparisons it saves cost.
;;
;; ASCII NEEDLES ONLY, and the guard is load-bearing rather than a convenience.
;; A haystack character with no entry in the table is skipped by the needle's
;; full length, which is only sound if such a character genuinely cannot appear
;; in the needle. A 256-entry table cannot answer for a needle holding U+65E5,
;; and skipping the full length past one silently misses matches — a fuzz of
;; 40k random cases over the alphabet {a b c é 日} found 174 of them before this
;; guard went in, and none after.
(define (str-bmh-table needle nlen)
  (let loop ((j 0))
    (cond
      ((fx=? j nlen)
       (let ((tbl (make-fxvector 256 nlen)) (lastj (fx- nlen 1)))
         (do ((k 0 (fx+ k 1))) ((fx=? k lastj) tbl)
           (fxvector-set! tbl (char->integer (string-ref needle k)) (fx- lastj k)))))
      ((fx>=? (char->integer (string-ref needle j)) 256) #f)
      (else (loop (fx+ j 1))))))

;; Search with a table built by str-bmh-table.
;;
;; THE HAYSTACK IS NOT ASCII JUST BECAUSE THE NEEDLE IS. str-bmh-table only
;; establishes that the NEEDLE fits the 256-entry table; the string being
;; searched can hold anything, so a character code is range-tested before it
;; indexes the table, and a character outside that range takes the full-length
;; skip (it cannot be in an ASCII needle).
;;
;; Everything here is a CHECKED primitive. The #3% unsafe reads were written
;; first and measured at 6.17ms against 6.50ms for the checked ones — 5%, on a
;; loop whose speed came from the algorithm rather than the accessor. That is
;; not worth an unsafe memory access in a path every string operation runs:
;; the first version indexed this table with an unchecked fxvector-ref and read
;; out of bounds on any haystack holding a non-ASCII character, which a fuzz run
;; surfaced as "fx+: #\nul is not a fixnum". The checked version cannot do that.
(define (str-index-of/bmh s needle from tbl)
  (let* ((nlen (string-length needle)) (slen (string-length s))
         (lastj (fx- nlen 1))
         (lastc (string-ref needle lastj))
         (last (fx- slen nlen)))
    (let loop ((i (fxmax 0 from)))
      (if (fx>? i last)
          -1
          (let ((c (string-ref s (fx+ i lastj))))
            (if (and (char=? c lastc) (char-by-char-match? s i needle nlen))
                i
                (let ((ci (char->integer c)))
                  (loop (fx+ i (if (fx<? ci 256) (fxvector-ref tbl ci) nlen))))))))))

;; Worth building a table for? It is ~256 stores, so it pays for itself over a
;; scan of any length but not over a handful of characters, and a 1-character
;; needle has nothing to skip by.
(define (str-bmh-worth? slen nlen) (and (fx>=? nlen 2) (fx>=? slen 512)))

;; The FIRST CHARACTER is tested inline, and only a hit calls the full compare.
;; This scan sits under indexOf, contains, split on a literal, and both literal
;; replaces, so it runs once per character of every string those touch — and a
;; procedure call per position is most of what it cost: scanning 6.6 MB for a
;; needle that never matches took 47 ms through char-by-char-match? alone.
;; Hoisting the length bound out of the loop matters for the same reason.
(define (str-index-of s needle from)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (if (fx=? nlen 0)
        ;; An empty needle matches AT from, CLAMPED to the string's length —
        ;; String.indexOf is explicit that "if fromIndex is greater than the
        ;; length of this String, and the target is the empty string, then the
        ;; length of this String is returned". The old loop answered -1 there,
        ;; which is the one place this scan disagreed with Java.
        (fxmin (fxmax 0 from) slen)
        (let ((tbl (and (str-bmh-worth? slen nlen) (str-bmh-table needle nlen))))
          (if tbl
              (str-index-of/bmh s needle from tbl)
              (let ((c0 (string-ref needle 0))
                    (last (fx- slen nlen)))
                (let loop ((i (fxmax 0 from)))
                  (cond ((fx>? i last) -1)
                        ((and (char=? (string-ref s i) c0)
                              (char-by-char-match? s i needle nlen)) i)
                        (else (loop (fx+ i 1)))))))))))
;; single-char search with no needle allocation — (.indexOf s (int 59)) used to
;; build a 1-char string through number->exact->truncate->integer->char->string
;; per call (~160ns); honeysql's suspicious? transducer does two per entity.
(define (str-char-index s c from)
  (let ((n (string-length s)))
    (let loop ((i (max 0 from)))
      (cond ((fx>=? i n) -1)
            ((char=? (string-ref s i) c) i)
            (else (loop (fx+ i 1)))))))
;; a needle that is a char code (fixnum) or a char scans directly
(define (str-index-of-any s needle from)
  (cond ((fixnum? needle)
         (if (and (fx>=? needle 0) (fx<=? needle #x10FFFF))
             (str-char-index s (integer->char needle) from)
             (str-index-of s (str-needle needle) from)))
        ((char? needle) (str-char-index s needle from))
        (else (str-index-of s (str-needle needle) from))))
;; The backward twin of str-index-of, and it gets the same inline first-character
;; test for the same reason: a procedure call at every position cost more than
;; the comparison it was making. An empty needle answers the string's length,
;; which is what String.lastIndexOf("") returns.
(define (str-last-index-of s needle)
  (str-last-index-of-from s needle (string-length s)))
;; The JVM's 2-arg overload: the last occurrence starting at or before FROM. An
;; empty needle answers FROM clamped into the string, which is what
;; String.lastIndexOf("", k) returns — and -1 for a negative FROM, since no start
;; position satisfies it.
(define (str-last-index-of-from s needle from)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (cond
      ((fx<? from 0) -1)
      ((fx=? nlen 0) (fxmin from slen))
      (else
       (let ((c0 (string-ref needle 0)))
         (let loop ((i (fxmin from (fx- slen nlen))))
           (cond ((fx<? i 0) -1)
                 ((and (char=? (string-ref s i) c0)
                       (char-by-char-match? s i needle nlen)) i)
                 (else (loop (fx- i 1))))))))))

;; A string argument to a String method: nil is a NullPointerException, as
;; String's own methods raise on null (they used to read as the empty string,
;; so (.indexOf "abc" nil) answered 0 and (.contains "a" nil) true).
(define (str-arg x)
  (if (jolt-nil? x) (throw-jvm 'NullPointerException "str") x))
;; A needle arg: a char value -> its 1-char string; a number -> the char at that
;; code point (JVM treats an int arg to indexOf as a char code); else a string.
(define (str-needle x)
  (cond ((char? x) (string x))
        ((number? x) (string (integer->char (exact (truncate x)))))
        ((string? x) x)
        (else (jolt-str (str-arg x)))))

;; literal replace-all (JVM String.replace(CharSequence,CharSequence)).
(define (str-replace-literal s a b)
  (let ((alen (string-length a)) (slen (string-length s)) (blen (string-length b)))
    (if (fx=? alen 0)
        ;; JVM String.replace with an empty match inserts the replacement at
        ;; every position 0..slen: "" -> "b", "aaa" -> "bababab".
        (let ((op (open-output-string)))
          (let loop ((i 0))
            (if (fx>? i slen)
                (get-output-string op)
                (begin (display b op)
                       (when (fx<? i slen) (write-char (string-ref s i) op))
                       (loop (fx+ i 1))))))
        ;; COUNT the matches, then allocate the result once and fill it with
        ;; block copies. The old loop advanced one CHARACTER at a time — a match
        ;; probe and a write-char each — which is what made an 8.7 MB replace
        ;; 5.4x babashka even after a literal pattern stopped reaching the regex
        ;; engine. Now the walk costs one block move per PART.
        ;;
        ;; The pre-sized fill is worth ~2x over writing into an output string
        ;; port, and the emit is ~74% of a replace with many matches (measured
        ;; on 6.6 MB with 240k matches: emit through a port 64 ms, the same emit
        ;; into a pre-sized string 34 ms, whole replace 91 ms). An earlier pass
        ;; measured those as equal and dropped the pre-sized version; that
        ;; measurement was taken on a Chez the project does not build with
        ;; (a system 9.5.4 rather than the pinned 10.4.1), where the two really
        ;; are within 1%. The result length is exact — slen + n*(blen-alen) —
        ;; so nothing grows and nothing is copied twice.
        ;;
        ;; The BMH table is built ONCE and threaded through both the counting
        ;; scan and the filling scan, rather than rebuilt per lookup.
        (let* ((tbl (and (str-bmh-worth? slen alen) (str-bmh-table a alen)))
               (next-at (lambda (from)
                          (if tbl (str-index-of/bmh s a from tbl) (str-index-of s a from))))
               (first-match (next-at 0)))
          (if (fx<? first-match 0)
              s                        ; nothing to replace: String.replace returns this
              (let count ((m first-match) (n 0))
                (if (fx>=? m 0)
                    (count (next-at (fx+ m alen)) (fx+ n 1))
                    (let ((out (make-string (fx+ slen (fx* n (fx- blen alen))))))
                      (let fill ((i 0) (m first-match) (o 0))
                        (cond
                         ((fx<? m 0)
                          (let ((tail (fx- slen i)))
                            (when (fx>? tail 0) (sa-string-copy-range! out o s i slen))
                            out))
                         (else
                          (let ((span (fx- m i)))
                            (when (fx>? span 0) (sa-string-copy-range! out o s i m))
                            (when (fx>? blen 0) (sa-string-copy-range! out (fx+ o span) b 0 blen))
                            (let ((nx (fx+ m alen)))
                              (fill nx (next-at nx) (fx+ o (fx+ span blen))))))))))))))))

;; A compiled irregex for a plain-string Java-regex pattern (or a jolt-regex).
(define (str-irx pat) (regex-t-irx (jolt-re-pattern pat)))

;; JVM String.split: split fully, then drop trailing empty strings.
(define (str-split-drop-trailing parts)
  (let loop ((p (reverse parts)))
    (if (and (pair? p) (string=? (car p) "")) (loop (cdr p)) (reverse p))))

;; --- charsets Chez has no codec for, through the system iconv ----------------
;; Chez encodes UTF-8/16/32 and the two single-byte Latin sets. Everything else a
;; caller can name — Shift_JIS, EUC-JP, windows-1252, KOI8-R — needs real tables,
;; and libc already carries them, so ask iconv rather than shipping our own. Where
;; iconv is missing (Windows) these are #f and the caller reports
;; UnsupportedEncodingException, which is what the JVM throws for a charset it
;; does not have. Silently returning UTF-8 bytes, as this used to, left the caller
;; no way to tell it had asked for something the host could not do.
;; Names differ by provider: glibc exports iconv_open/iconv/iconv_close, and
;; Termux's GNU libiconv exports libiconv_open/libiconv/libiconv_close. BOTH
;; pairs can exist at once on bionic: Android's libc carries a PARTIAL iconv
;; (UTF-8/16 and ASCII only) beside Termux's complete GNU library, so a symbol
;; that resolves is not a provider that can name the charset — the libc pair
;; answers (iconv_t)-1 for windows-1252/Shift_JIS. Trying one name and taking
;; its presence for capability is what left every non-Unicode charset raising
;; on bionic. So collect the pairs and let each request fall through to the
;; next provider. Both names are literal because foreign-procedure names a
;; symbol at compile time.
(define c-iconv-open  (jolt-foreign-proc-safe "iconv_open" '(string string) 'void*))
(define c-iconv-conv  (jolt-foreign-proc-safe "iconv" '(void* void* void* void* void*) 'size_t))
(define c-iconv-close (jolt-foreign-proc-safe "iconv_close" '(void*) 'int))
(define c-libiconv-open  (jolt-foreign-proc-safe "libiconv_open" '(string string) 'void*))
(define c-libiconv-conv  (jolt-foreign-proc-safe "libiconv" '(void* void* void* void* void*) 'size_t))
(define c-libiconv-close (jolt-foreign-proc-safe "libiconv_close" '(void*) 'int))
(define iconv-size-max (- (expt 2 (* 8 (sa-foreign-sizeof 'size_t))) 1))

;; (open conv close) for every iconv this host has, unprefixed first: where
;; both work the libc's is the process's own.
(define iconv-providers
  (filter (lambda (p) (and (vector-ref p 0) (vector-ref p 1) (vector-ref p 2)))
          (list (vector c-iconv-open c-iconv-conv c-iconv-close)
                (vector c-libiconv-open c-libiconv-conv c-libiconv-close))))

;; The first provider that can name FROM->TO, as (provider . descriptor), or #f
;; when none can. A provider whose open raises is skipped too, not fatal. The
;; descriptor is the caller's to close, with that provider's close.
(define (iconv-open-cd from to)
  (let loop ((ps iconv-providers))
    (and (pair? ps)
         (let ((p (car ps)))
           (guard (e (#t (loop (cdr ps))))
             (let ((cd ((vector-ref p 0) to from)))
               (if (or (= cd iconv-size-max) (= cd 0))
                   (loop (cdr ps))
                   (cons p cd))))))))

(define (iconv-known? name)
  (let ((pcd (iconv-open-cd "UTF-8" name)))
    (and pcd (begin ((vector-ref (car pcd) 2) (cdr pcd)) #t))))

;; Convert bytes between two charsets, or #f if the host cannot. The four
;; iconv arguments are pointers to a cursor pair, so they live in one 32-byte
;; block at offsets 0/8/16/24: in pointer, in remaining, out pointer, out
;; remaining. Worst case a byte grows to four (UTF-32), plus room for a BOM.
(define (iconv-bytes bv from to)
  (let ((r (iconv-convert bv from to))) (and r (car r))))
;; ...as (bytes . exact?), exact? when iconv reported no irreversible
;; conversion. iconv refuses a character its target lacks in one of two ways,
;; and which is the libc's choice: glibc stops with EILSEQ (#f here), Apple's
;; substitutes something of its own -- "gal." for 日 in ISO-8859-2 -- and counts
;; it in its return value (exact? #f). Neither is what the JDK writes, so a
;; caller that cares takes either as "not this way" (charset-encode-iconv).
(define (iconv-convert bv from to)
  (let ((pcd (iconv-open-cd from to)))
    (and pcd
         (let* ((provider (car pcd))
                (cd (cdr pcd))
                (conv (vector-ref provider 1))
                (close (vector-ref provider 2))
                (inlen (bytevector-length bv))
                (outcap (+ 32 (* 4 (max inlen 1))))
                (inbuf (sa-foreign-alloc (max inlen 1)))
                (outbuf (sa-foreign-alloc outcap))
                (cells (sa-foreign-alloc 32))
                (result
                 (guard (e (#t #f))
                   (do ((i 0 (+ i 1))) ((= i inlen))
                     (sa-foreign-set! 'unsigned-8 inbuf i (bytevector-u8-ref bv i)))
                   (sa-foreign-set! 'void* cells 0 inbuf)
                   (sa-foreign-set! 'unsigned-64 cells 8 inlen)
                   (sa-foreign-set! 'void* cells 16 outbuf)
                   (sa-foreign-set! 'unsigned-64 cells 24 outcap)
                   (let ((irreversible (conv cd cells (+ cells 8) (+ cells 16) (+ cells 24))))
                     (and (not (= iconv-size-max irreversible))
                        ;; Then reset the descriptor to its initial state, which
                        ;; POSIX spells as an iconv with a NULL input. A stateful
                        ;; charset holds a mode, and its closing shift back to
                        ;; ASCII is only emitted here — without it
                        ;; (.getBytes "い" "ISO-2022-JP") stops after the
                        ;; character and drops the trailing ESC ( B the JVM
                        ;; writes. Stateless charsets write nothing.
                        (begin (conv cd 0 0 (+ cells 16) (+ cells 24))
                               #t)
                        (let* ((n (- outcap (sa-foreign-ref 'unsigned-64 cells 24)))
                               (out (make-bytevector n)))
                          (do ((i 0 (+ i 1))) ((= i n) (cons out (= irreversible 0)))
                            (bytevector-u8-set! out i (sa-foreign-ref 'unsigned-8 outbuf i)))))))))
           (sa-foreign-free inbuf) (sa-foreign-free outbuf) (sa-foreign-free cells)
           (close cd)
           result))))

(define (unsupported-encoding-throw name)
  (jolt-throw (jolt-host-throwable "java.io.UnsupportedEncodingException" name)))

;; --- incremental decoding, for the charsets that are not UTF-8 or Latin-1 -----
;; A decoder is (step . finish). (step bv lo hi eof?) decodes everything in
;; bv[lo,hi) that can be decoded and answers (values text next): NEXT is where
;; the undecoded tail starts, a sequence that may still be completed by bytes
;; not read yet. With eof? there are none, so the tail is malformed and comes
;; out as U+FFFD. (finish) releases whatever the decoder holds; it may be
;; called more than once.
;;
;; Being incremental is what lets an InputStreamReader in one of these charsets
;; hand back text as it arrives: decoding the whole source on the first read
;; blocked a reader over a pipe or a socket until the writer closed it. The
;; same decoders serve (String. bytes cs), so the two answer alike.
;;
;; The malformed-input rules are the JDK's (sun.nio.cs.UnicodeDecoder and
;; UTF_32Coder): a high surrogate followed by anything but a low one is ONE
;; malformed unit of four bytes; a lone low surrogate, a code point past
;; U+10FFFF, and a short tail at end of input are each one U+FFFD. A UTF-32
;; surrogate code point, which the JDK hands back as a lone char, is U+FFFD
;; here: a Chez string cannot hold one.
;;
;; Chez's own utf16->string and utf32->string already follow those rules
;; (s/prims.ss: a high surrogate and a non-low unit are one U+FFFD over both,
;; a lone low one or an out-of-range unit is one, a short tail at the end is
;; one), so a step hands Chez every byte that is complete and holds back only
;; a tail that later bytes could still complete. It used to scan each unit in
;; Scheme for a malformed one first; that scan cost as much as the decode and
;; made (String. bytes "UTF-32") 4x slower than handing the buffer over.
;; The text of bv[from,to) in the order BIG?. The whole buffer goes over as it
;; is. So does one whose only skipped prefix is a byte-order mark the decoder
;; read at 0: Chez drops a leading mark itself when the order is not
;; mandatory, and it is the same mark, so it cannot pick another order.
;; Anything else is copied out first, as utf16->string takes no range.
(define (jdec-run->string decode bv from to big? marked?)
  (let ((order (if big? (endianness big) (endianness little)))
        (n (bytevector-length bv)))
    (cond ((fx=? from to) "")
          ((and (fx=? from 0) (fx=? to n)) (decode bv order #t))
          ((and marked? (fx=? to n)) (decode bv order #f))
          (else
           (let ((slice (make-bytevector (fx- to from))))
             (bytevector-copy! bv from slice 0 (fx- to from))
             (decode slice order #t))))))

;; UTF-16, UTF-16BE, UTF-16LE. Only "UTF-16" reads a byte-order mark, and only
;; at the start: it picks the order and is dropped. The BE and LE forms hand a
;; leading FEFF back as a character, as the JDK's do.
(define (make-utf16-decoder cs)
  (let ((big? (not (string=? cs "utf-16le")))
        (sniff? (string=? cs "utf-16")))
    (define (high? bv i) (fx=? (fxand (bytevector-u8-ref bv (if big? i (fx+ i 1))) #xFC) #xD8))
    ;; Where the text that is decidable now ends, short of HI: an odd byte is
    ;; held, and so is a high surrogate whose next unit is not here yet. A
    ;; high one followed by another high one is malformed as a PAIR, so
    ;; whether the last high unit waits depends on the parity of the run of
    ;; high units it ends: even, they all pair off among themselves.
    (define (decidable-end bv lo hi)
      (let ((c (fx- hi (fxand (fx- hi lo) 1))))
        (let count ((i c) (k 0))
          (if (and (fx>=? (fx- i 2) lo) (high? bv (fx- i 2)))
              (count (fx- i 2) (fx+ k 1))
              (if (fxodd? k) (fx- c 2) c)))))
    (define (step bv lo hi eof?)
      (define marked? #f)
      (when (and sniff? (or eof? (fx>=? (fx- hi lo) 2)))
        (set! sniff? #f)
        (when (fx>=? (fx- hi lo) 2)
          (let ((a (bytevector-u8-ref bv lo)) (b (bytevector-u8-ref bv (fx+ lo 1))))
            (cond ((and (fx=? a #xFE) (fx=? b #xFF))
                   (set! big? #t) (set! marked? (fx=? lo 0)) (set! lo (fx+ lo 2)))
                  ((and (fx=? a #xFF) (fx=? b #xFE))
                   (set! big? #f) (set! marked? (fx=? lo 0)) (set! lo (fx+ lo 2)))))))
      (if sniff?
          (values "" lo)                                ; the mark is not all here yet
          (let ((end (if eof? hi (decidable-end bv lo hi))))
            (values (jdec-run->string utf16->string bv lo end big? marked?) end))))
    (cons step void)))

;; UTF-32, UTF-32BE, UTF-32LE. Every form drops a leading U+FEFF in its own
;; order; "UTF-32" also reads a little-endian mark as one and switches.
(define (make-utf32-decoder cs)
  (let ((big? (not (string=? cs "utf-32le")))
        (sniff? #t))
    (define (unit bv i)
      (let ((b0 (bytevector-u8-ref bv i)) (b1 (bytevector-u8-ref bv (fx+ i 1)))
            (b2 (bytevector-u8-ref bv (fx+ i 2))) (b3 (bytevector-u8-ref bv (fx+ i 3))))
        (if big?
            (+ (* b0 #x1000000) (fxsll b1 16) (fxsll b2 8) b3)
            (+ (* b3 #x1000000) (fxsll b2 16) (fxsll b1 8) b0))))
    (define (step bv lo hi eof?)
      (define marked? #f)
      (when (and sniff? (or eof? (fx>=? (fx- hi lo) 4)))
        (set! sniff? #f)
        (when (fx>=? (fx- hi lo) 4)
          (cond ((eqv? (unit bv lo) #xFEFF) (set! marked? (fx=? lo 0)) (set! lo (fx+ lo 4)))
                ((and (string=? cs "utf-32")
                      (begin (set! big? #f) (eqv? (unit bv lo) #xFEFF)))
                 (set! marked? (fx=? lo 0)) (set! lo (fx+ lo 4)))
                (else (set! big? (not (string=? cs "utf-32le")))))))
      (if sniff?
          (values "" lo)
          (let ((end (if eof? hi (fx- hi (fxand (fx- hi lo) 3)))))
            (values (jdec-run->string utf32->string bv lo end big? marked?) end))))
    (cons step void)))

;; Everything else through the system iconv, with ONE descriptor for the life
;; of the stream: iconv carries a stateful charset's shift state (ISO-2022-JP)
;; from one call to the next, and stops short of a sequence the input cut off
;; (EINVAL), which is exactly the undecoded tail a step answers. A byte iconv
;; cannot decode (EILSEQ) is one U+FFFD and decoding goes on past it, where
;; the whole-buffer decode raised UnsupportedEncodingException over the lot.
;; The JDK sizes some malformed runs by charset (EUC-JP takes two bytes where
;; this takes one); GBK, Shift_JIS and the windows code pages agree.
(define iconv-E2BIG 7)
(define iconv-EINVAL 22)
(define iconv-EILSEQ (if (eq? (sa-os-family) 'macos) 92 84))
;; A reader dropped without close would keep its descriptor; the guardian hands
;; those back, and they are closed the next time a decoder is opened.
(define iconv-guardian (make-guardian))
(define iconv-guardian-mu (make-mutex))
(define (iconv-state-close! st)
  (when (vector-ref st 1)
    ((vector-ref (vector-ref st 0) 2) (vector-ref st 1))
    (vector-set! st 1 #f)))
(define (iconv-drain!)
  (for-each iconv-state-close!
            (jolt-with-mutex iconv-guardian-mu
              (let loop ((acc '()))
                (let ((st (iconv-guardian)))
                  (if st (loop (cons st acc)) acc))))))
;; The ISO-2022 charsets switch between single- and double-byte runs. When a
;; read ends inside a double-byte character, glibc's iconv stops before it
;; (EINVAL), but Apple's decodes the dangling byte as ASCII and loses the shift
;; state, so the next read comes out wrong. A step on these charsets therefore
;; hands iconv only whole characters and keeps the shift state itself: ESC $ ...
;; (JP) or SO (KR/CN) starts a double-byte run, ESC ( ... or SI ends it, and in
;; one, bytes 0x21-0x7E pair up while a control byte stands alone.
(define (iso2022-family name)
  (let ((n (string-downcase name)))
    (cond ((iconv-name-has? n "2022-jp") 'jp)
          ((or (iconv-name-has? n "2022-kr") (iconv-name-has? n "2022-cn")) 'so)
          (else #f))))
(define (iconv-name-has? s sub)
  (let ((n (string-length s)) (k (string-length sub)))
    (let loop ((i 0))
      (and (<= (+ i k) n)
           (or (string=? (substring s i (+ i k)) sub) (loop (+ i 1)))))))
;; Scan BV[lo,hi) from shift state DOUBLE? (#t inside a double-byte run): the
;; last position that ends a whole character or escape, and the state there.
(define (iso2022-scan family bv lo hi double?)
  (let loop ((i lo) (dbl double?) (safe lo) (safe-dbl double?))
    (if (fx>=? i hi)
        (values safe safe-dbl)
        (let ((b (bytevector-u8-ref bv i)))
          (cond
            ((fx=? b 27)                          ; ESC intermediates* final
             (let esc ((j (fx+ i 1)))
               (cond ((fx>=? j hi) (values safe safe-dbl))
                     ((fx<=? #x20 (bytevector-u8-ref bv j) #x2f) (esc (fx+ j 1)))
                     (else
                      (let ((d (if (and (eq? family 'jp) (fx>? j (fx+ i 1)))
                                   (case (integer->char (bytevector-u8-ref bv (fx+ i 1)))
                                     ((#\$) #t) ((#\() #f) (else dbl))
                                   dbl)))
                        (loop (fx+ j 1) d (fx+ j 1) d))))))
            ((and (eq? family 'so) (fx=? b 14)) (loop (fx+ i 1) #t (fx+ i 1) #t))
            ((and (eq? family 'so) (fx=? b 15)) (loop (fx+ i 1) #f (fx+ i 1) #f))
            ((and dbl (fx<=? #x21 b #x7e))
             (if (fx<? (fx+ i 1) hi)
                 (loop (fx+ i 2) dbl (fx+ i 2) dbl)
                 (values safe safe-dbl)))
            (else
             ;; a newline ends a KR/CN double-byte run
             (let ((d (if (and (eq? family 'so) (fx=? b 10)) #f dbl)))
               (loop (fx+ i 1) d (fx+ i 1) d))))))))
(define (make-iconv-decoder name)
  (iconv-drain!)
  (let ((pcd (iconv-open-cd name "UTF-8")))
    (and pcd
         (let ((st (vector (car pcd) (cdr pcd) (iso2022-family name) #f)))
           (jolt-with-mutex iconv-guardian-mu (iconv-guardian st))
           (cons (lambda (bv lo hi eof?) (iconv-step st bv lo hi eof?))
                 (lambda () (iconv-state-close! st)))))))
(define (iconv-step st bv lo hi eof?)
  (let ((family (vector-ref st 2)))
    (if (not family)
        (iconv-step* st bv lo hi eof?)
        (let-values (((safe d) (if eof? (values hi #f)
                                   (iso2022-scan family bv lo hi (vector-ref st 3)))))
          (let-values (((str next) (iconv-step* st bv lo safe eof?)))
            ;; the state where iconv stopped, which is SAFE unless it stopped short
            (vector-set! st 3 (if (fx=? next safe) d
                                  (let-values (((p d2) (iso2022-scan family bv lo next (vector-ref st 3)))) d2)))
            (values str next))))))
(define (iconv-step* st bv lo hi eof?)
  (let* ((conv (vector-ref (vector-ref st 0) 1))
         (cd (vector-ref st 1))
         (n (fx- hi lo))
         (outcap (+ 64 (* 4 n)))
         (inbuf (sa-foreign-alloc (max n 1)))
         (outbuf (sa-foreign-alloc outcap))
         (cells (sa-foreign-alloc 32))
         ;; R6RS: the port and the procedure that extracts what it collected
         (acc+ (call-with-values open-bytevector-output-port cons))
         (acc (car acc+)))
    (define (in-left) (sa-foreign-ref 'unsigned-64 cells 8))
    (define (drain-out!)              ; move what iconv wrote into ACC
      (let* ((k (- outcap (sa-foreign-ref 'unsigned-64 cells 24)))
             (chunk (make-bytevector k)))
        (sa-foreign-bytes-ref! outbuf chunk k)
        (put-bytevector acc chunk)
        (sa-foreign-set! 'void* cells 16 outbuf)
        (sa-foreign-set! 'unsigned-64 cells 24 outcap)))
    (define (replace!) (put-bytevector acc #vu8(#xEF #xBF #xBD)))
    (let ((r (dynamic-wind
      void
      (lambda ()
        (let ((slice (make-bytevector n)))
          (bytevector-copy! bv lo slice 0 n)
          (sa-foreign-bytes-set! inbuf slice n))
        (sa-foreign-set! 'void* cells 0 inbuf)
        (sa-foreign-set! 'unsigned-64 cells 8 n)
        (sa-foreign-set! 'void* cells 16 outbuf)
        (sa-foreign-set! 'unsigned-64 cells 24 outcap)
        (let loop ()
          (if (fx=? (in-left) 0)
              (drain-out!)
              (let ((r (if cd (conv cd cells (+ cells 8) (+ cells 16) (+ cells 24)) iconv-size-max)))
                (if (not (= r iconv-size-max))
                    (begin (drain-out!) (loop))
                    (let ((err (io-errno)))
                      (drain-out!)
                      (cond
                        ((= err iconv-E2BIG) (loop))
                        ;; cut off: wait for the rest, unless there is no rest
                        ((and (= err iconv-EINVAL) (not eof?)) #f)
                        ((= err iconv-EINVAL)
                         (replace!)
                         (sa-foreign-set! 'unsigned-64 cells 8 0))
                        ;; EILSEQ, or an errno this host would not give: one
                        ;; replacement for the byte, and on past it
                        (else
                         (replace!)
                         (sa-foreign-set! 'void* cells 0 (+ (sa-foreign-ref 'void* cells 0) 1))
                         (sa-foreign-set! 'unsigned-64 cells 8 (- (in-left) 1))
                         (loop))))))))
        ;; at end of input, back to the initial shift state (a decoder writes
        ;; nothing doing it, but the descriptor is then ready to be closed)
        (when (and eof? cd)
          (conv cd 0 0 (+ cells 16) (+ cells 24))
          (drain-out!))
        (cons (utf8-bytes->string ((cdr acc+)))
              (fx+ lo (fx- n (in-left)))))
      (lambda ()
        (sa-foreign-free inbuf) (sa-foreign-free outbuf) (sa-foreign-free cells)))))
      (values (car r) (cdr r)))))

;; The decoder for canonical charset CS (named NAME by the caller), or #f when
;; the host has none.
(define (make-java-decoder cs name)
  (cond
    ((member cs '("utf-16" "utf-16be" "utf-16le")) (make-utf16-decoder cs))
    ((member cs '("utf-32" "utf-32be" "utf-32le")) (make-utf32-decoder cs))
    (else (make-iconv-decoder name))))
;; All of BV at once, as (String. bytes cs) wants it.
(define (java-decode-all bv cs name)
  (let ((dec (or (make-java-decoder cs name) (unsupported-encoding-throw name))))
    (dynamic-wind
      void
      (lambda () (let-values (((s next) ((car dec) bv 0 (bytevector-length bv) #t))) s))
      (cdr dec))))

;; --- UTF-8 -> string, the way java.nio's decoder does it ---------------------
;;
;; Chez's utf8->string and Java's CharsetDecoder agree on every well-formed
;; input and disagree on malformed ones, because they disagree about how many
;; BYTES a bad sequence costs. Java replaces per malformed RUN, and the run
;; length is decided by sun.nio.cs.UTF_8's malformedN: an overlong lead is
;; rejected on its own, and the continuation bytes behind it are then each a
;; stray of their own. Chez folds the whole sequence into one replacement.
;;
;;   bytes           Java            Chez
;;   C0 AF           FFFD FFFD       FFFD
;;   E0 80 AF        FFFD FFFD FFFD  FFFD
;;   F0 80 80 AF     FFFD x4         FFFD
;;   F4 90 80 80     FFFD x4         FFFD
;;   EF BB BF        FEFF            (stripped)
;;
;; The last row is not about malformed input at all: Chez treats a leading BOM
;; as a signature and drops it, and Java hands it back as U+FEFF.
;;
;; So (String. bytes) cannot be utf8->string. It also cannot be a transcoded
;; port, which is what slurp-of-a-path uses -- that agrees with Java only on
;; the two-byte row above and is wrong on the other four, and it is twice the
;; cost besides (8.1MB x20: utf8->string 889ms, bytevector->string 1733ms).
;;
;; utf8-bytes->string is therefore Chez's decoder behind a GUARD. Well-formed
;; input with no leading BOM is exactly the case where the two cannot disagree,
;; so it goes to utf8->string; anything else is re-decoded by the hand-written
;; Java model below. Measured on the same 8.1MB x20, over utf8->string's 889ms:
;; the guard adds 185ms, and decoding by hand instead would add 968ms -- ~20%
;; to keep the C decoder against ~110% to replace it.
(define (%utf8-cont? b) (fx=? (fxand b #xC0) #x80))

;; #t when Chez's decoder and Java's cannot differ on bv: strictly well-formed
;; UTF-8, and no BOM for Chez to swallow.
(define (%utf8-java-plain? bv)
  (let* ((n (bytevector-length bv))
         (n4 (fx- n 4)))
    (and (not (and (fx>=? n 3)
                   (fx=? (bytevector-u8-ref bv 0) #xEF)
                   (fx=? (bytevector-u8-ref bv 1) #xBB)
                   (fx=? (bytevector-u8-ref bv 2) #xBF)))
         (let loop ((i 0))
           ;; ASCII runs dominate real payloads, so clear four bytes per step
           ;; while the index stays word-aligned: no high bit anywhere in the
           ;; word means four plain characters. Byte at a time this scan cost
           ;; 371ms per 8.1MB x20 against the 185ms it costs here.
           (let skip ((i i))
             (cond
               ((and (fx<=? i n4) (fx=? (fxand i 3) 0)
                     (fx=? (fxand (bytevector-u32-native-ref bv i) #x80808080) 0))
                (skip (fx+ i 4)))
               ((fx>=? i n) #t)
               (else
                (let ((b1 (bytevector-u8-ref bv i)))
                  (cond
                    ((fx<? b1 #x80) (loop (fx+ i 1)))
                    ;; C0/C1 are overlong two-byte leads and never well-formed
                    ((and (fx>=? b1 #xC2) (fx<=? b1 #xDF))
                     (and (fx<? (fx+ i 1) n)
                          (%utf8-cont? (bytevector-u8-ref bv (fx+ i 1)))
                          (loop (fx+ i 2))))
                    ((and (fx>=? b1 #xE0) (fx<=? b1 #xEF))
                     (and (fx<? (fx+ i 2) n)
                          (let ((b2 (bytevector-u8-ref bv (fx+ i 1))))
                            (and (%utf8-cont? b2)
                                 (not (and (fx=? b1 #xE0) (fx<? b2 #xA0)))   ; overlong
                                 (not (and (fx=? b1 #xED) (fx>? b2 #x9F)))   ; surrogate
                                 (%utf8-cont? (bytevector-u8-ref bv (fx+ i 2)))
                                 (loop (fx+ i 3))))))
                    ((and (fx>=? b1 #xF0) (fx<=? b1 #xF4))
                     (and (fx<? (fx+ i 3) n)
                          (let ((b2 (bytevector-u8-ref bv (fx+ i 1))))
                            (and (%utf8-cont? b2)
                                 (not (and (fx=? b1 #xF0) (fx<? b2 #x90)))   ; overlong
                                 (not (and (fx=? b1 #xF4) (fx>? b2 #x8F)))   ; > U+10FFFF
                                 (%utf8-cont? (bytevector-u8-ref bv (fx+ i 2)))
                                 (%utf8-cont? (bytevector-u8-ref bv (fx+ i 3)))
                                 (loop (fx+ i 4))))))
                    (else #f))))))))))

;; sun.nio.cs.UTF_8's decode loop, one sequence at a time.
;;
;; Decodes the sequence at I and answers two values: its code point (#xFFFD for
;; a malformed one) and the index just past what Java would have CONSUMED for
;; it. N bounds the bytes available, and MORE? says whether bytes beyond N may
;; still arrive -- a streaming reader passes #t and gets (values #f i) when the
;; sequence is cut off at the buffer edge, meaning "refill and ask again". With
;; MORE? #f, N is end of input: an incomplete but VALID prefix there is one
;; replacement for the whole remainder, because Java's decoder underflows and
;; the flush replaces once.
;;
;; Both the whole-buffer decoder below and the streaming port that java/
;; io-streams.ss hands every Reader run on this, so the two cannot drift.
(define %utf8-replacement #xFFFD)
(define (%utf8-java-step bv i n more?)
  (let ((b1 (bytevector-u8-ref bv i)))
    (cond
      ((fx<? b1 #x80) (values b1 (fx+ i 1)))
      ;; C0/C1 are overlong leads; they fall to the stray arm at the bottom
      ((and (fx>=? b1 #xC2) (fx<=? b1 #xDF))
       (if (fx>=? (fx+ i 1) n)
           (if more? (values #f i) (values %utf8-replacement n))
           (let ((b2 (bytevector-u8-ref bv (fx+ i 1))))
             (if (%utf8-cont? b2)
                 (values (fxior (fxsll (fxand b1 #x1F) 6) (fxand b2 #x3F)) (fx+ i 2))
                 (values %utf8-replacement (fx+ i 1))))))
      ((and (fx>=? b1 #xE0) (fx<=? b1 #xEF))
       (if (fx>=? (fx+ i 1) n)
           (if more? (values #f i) (values %utf8-replacement n))
           (let* ((b2 (bytevector-u8-ref bv (fx+ i 1)))
                  ;; isMalformed3_2: the lead is already wrong on its own, so
                  ;; Java consumes ONE byte and the rest become strays
                  (lead-bad? (or (and (fx=? b1 #xE0) (fx=? (fxand b2 #xE0) #x80))
                                 (not (%utf8-cont? b2)))))
             (cond
               (lead-bad? (values %utf8-replacement (fx+ i 1)))
               ((fx>=? (fx+ i 2) n)
                (if more? (values #f i) (values %utf8-replacement n)))
               (else
                (let ((b3 (bytevector-u8-ref bv (fx+ i 2))))
                  (if (not (%utf8-cont? b3))
                      (values %utf8-replacement (fx+ i 2))
                      (let ((c (fxior (fxsll (fxand b1 #x0F) 12)
                                      (fxsll (fxand b2 #x3F) 6)
                                      (fxand b3 #x3F))))
                        ;; a surrogate is malformedForLength(3) -- one
                        ;; replacement for all three bytes, unlike the overlong
                        ;; above, which costs one per byte
                        (if (and (fx>=? c #xD800) (fx<=? c #xDFFF))
                            (values %utf8-replacement (fx+ i 3))
                            (values c (fx+ i 3)))))))))))
      ;; Java's four-byte arm is F0..F7; F5..F7 always fail the lead test
      ((and (fx>=? b1 #xF0) (fx<=? b1 #xF7))
       (if (fx>=? (fx+ i 1) n)
           (if more? (values #f i) (values %utf8-replacement n))
           (let* ((b2 (bytevector-u8-ref bv (fx+ i 1)))
                  (lead-bad? (or (fx>? b1 #xF4)
                                 (and (fx=? b1 #xF0) (or (fx<? b2 #x90) (fx>? b2 #xBF)))
                                 (and (fx=? b1 #xF4) (not (fx=? (fxand b2 #xF0) #x80)))
                                 (not (%utf8-cont? b2)))))
             (cond
               (lead-bad? (values %utf8-replacement (fx+ i 1)))
               ((fx>=? (fx+ i 2) n)
                (if more? (values #f i) (values %utf8-replacement n)))
               ((not (%utf8-cont? (bytevector-u8-ref bv (fx+ i 2))))
                (values %utf8-replacement (fx+ i 2)))
               ((fx>=? (fx+ i 3) n)
                (if more? (values #f i) (values %utf8-replacement n)))
               ((not (%utf8-cont? (bytevector-u8-ref bv (fx+ i 3))))
                (values %utf8-replacement (fx+ i 3)))
               (else
                (values (fxior (fxsll (fxand b1 #x07) 18)
                               (fxsll (fxand b2 #x3F) 12)
                               (fxsll (fxand (bytevector-u8-ref bv (fx+ i 2)) #x3F) 6)
                               (fxand (bytevector-u8-ref bv (fx+ i 3)) #x3F))
                        (fx+ i 4)))))))
      ;; 80..BF (a stray continuation), C0/C1, F8..FF
      (else (values %utf8-replacement (fx+ i 1))))))

;; The whole-buffer decoder: only reached for input %utf8-java-plain? turned
;; down, so it is never on a hot path.
(define (utf8->string/java bv)
  (let* ((n (bytevector-length bv))
         (out (make-string n)))          ; one char per byte is the upper bound
    (let loop ((i 0) (o 0))
      (if (fx>=? i n)
          (if (fx=? o n) out (substring out 0 o))
          (let-values (((c next) (%utf8-java-step bv i n #f)))
            (string-set! out o (integer->char c))
            (loop next (fx+ o 1)))))))

;; Decode UTF-8 bytes the way the JVM does. Shared by decode-bytevector
;; (String., slurp of a byte source, the CharsetDecoder) -- anything that turns
;; a whole byte buffer into text.
(define (utf8-bytes->string bv)
  (if (%utf8-java-plain? bv)
      (utf8->string bv)
      (utf8->string/java bv)))

;; Encode a string to bytes (a bytevector) under a named charset. UTF-8 default;
;; ISO-8859-1/US-ASCII are one byte per char; UTF-16/UTF-32 via Chez's codecs
;; (plain "UTF-16" emits a big-endian BOM then BE, matching the JVM); anything
;; else through iconv. Names are canonicalized first, so any JVM alias works.
;; Shared by .getBytes and decode-bytevector (String.).
(define (charset-encode-bv s csname)
  ;; through charset-canonical-down (host-static-classes.ss), so every JVM alias
  ;; the Charset table knows resolves here too — (.getBytes s "l1") used to fall
  ;; past this cond's partial list and silently return UTF-8 bytes.
  (let ((name (charset-arg-name csname)))
    (let ((cs (charset-canonical-down name)))
      (cond
        ((string=? cs "utf-8") (string->utf8 s))
        ;; one byte per char, and a char the charset cannot hold is the JVM
        ;; encoder's replacement byte, `?`. Masking the code point to its low
        ;; byte wrote some other character: (.getBytes "€" "ISO-8859-1") was
        ;; [-84], the low byte of U+20AC, where the JVM answers [63].
        ((member cs '("iso-8859-1" "us-ascii"))
         (let* ((n (string-length s)) (bv (make-bytevector n))
                (top (if (string=? cs "us-ascii") 127 255)))
           (do ((i 0 (+ i 1))) ((= i n) bv)
             (let ((c (char->integer (string-ref s i))))
               (bytevector-u8-set! bv i (if (fx<=? c top) c 63))))))
        ((string=? cs "utf-16be") (string->utf16 s (endianness big)))
        ((string=? cs "utf-16le") (string->utf16 s (endianness little)))
        ((string=? cs "utf-16")
         (let ((be (string->utf16 s (endianness big))))
           (let* ((n (bytevector-length be)) (bv (make-bytevector (+ n 2))))
             (bytevector-u8-set! bv 0 #xfe) (bytevector-u8-set! bv 1 #xff)
             (bytevector-copy! be 0 bv 2 n) bv)))
        ((or (string=? cs "utf-32be") (string=? cs "utf-32"))
         (string->utf32 s (endianness big)))
        ((string=? cs "utf-32le") (string->utf32 s (endianness little)))
        (else (charset-encode-iconv s name
                                    (lambda (c) (or (iconv-bytes (string->utf8 "?") "UTF-8" name)
                                                    (bytevector 63)))))))))
;; S in an iconv charset. One conversion when every character maps, which is
;; nearly always; otherwise each character on its own, and UNMAPPABLE (a
;; procedure of the character) answers the bytes for one that does not -- the
;; charset's replacement for String.getBytes, a raise for a reporting encoder.
;; Apple's ISO-8859-2 and -3 tables go further: they substitute a spelling
;; ("gal." for 日) and still report the conversion exact. In a one-byte charset
;; that shows as more bytes than characters, which is checked instead.
(define (charset-one-byte? name)
  (let ((cs (charset-canonical-down name)))
    (exists (lambda (prefix)
              (and (>= (string-length cs) (string-length prefix))
                   (string=? (substring cs 0 (string-length prefix)) prefix)))
            '("iso-8859-" "windows-125" "koi8-"))))
(define (charset-encode-iconv s name unmappable)
  ;; the name is only looked at when the byte count is not the character count
  (let* ((exact (lambda (r n) (and r (cdr r) (or (= (bytevector-length (car r)) n)
                                                  (not (charset-one-byte? name))))))
         (whole (iconv-convert (string->utf8 s) "UTF-8" name)))
    (cond
      ((exact whole (string-length s)) (car whole))
      ((not (iconv-known? name)) (unsupported-encoding-throw name))
      (else
       (let-values (((out extract) (open-bytevector-output-port)))
         (string-for-each
          (lambda (c)
            (let ((one (iconv-convert (string->utf8 (string c)) "UTF-8" name)))
              (put-bytevector out (if (exact one 1) (car one) (unmappable c)))))
          s)
         (extract))))))

;; (Object.hashCode parity — jolt-s32, java-string-hash, java-symbol-hash — lives
;; in natives-misc.ss: records-dispatch.ss's keyword/symbol .hashCode arms read
;; it too, and that file is shared with the Gambit boot where this one is not.)

;; --- String methods as named natives -----------------------------------------
;; The back end's string-direct-emit (backend_scheme.clj) open-codes a `.method`
;; call whose receiver is PROVEN a string, and jolt-string-method below dispatches
;; the same call when it is not. Every method whose body is more than a single
;; Chez form gets its native here so those two paths are the SAME code rather than
;; two transcriptions of it — a divergence between them would show up only on the
;; hinted path, which is exactly where nobody looks.
;;
;; The `jolt-` prefix is load-bearing: munge-name (backend_scheme.clj) prefixes any
;; user local whose name starts with "jolt-", so a bare emitted head with it can
;; never be shadowed by a local, and the name needs no entry in rt-emitted-names.
(define (jolt-str-equals? s o) (and (string? o) (string=? s o)))
;; Thin wrappers over the jvm-string-* fold, NOT second implementations. Java's
;; ignore-case comparison is a per-character upper-then-lower fold and its
;; compareTo answers a character DIFFERENCE, not a sign — a downcase-and-sign
;; transcription disagrees on "É"/"é", on dotless i, and on every magnitude
;; ((.compareTo "a" "c") is -2, not -1). One implementation, reached from both the
;; generic dispatch above and the proven-receiver emission, is the only way the
;; two paths cannot drift.
(define (jolt-str-equals-ci? s o)
  (and (not (jolt-nil? o)) (jvm-string-ci=? s (jolt-need-str o))))
(define (jolt-str-compare s o)
  (jvm-string-compare s (jolt-need-str o)))
(define (jolt-str-compare-ci s o)
  (jvm-string-ci-compare s (jolt-need-str o)))
(define (jolt-str-blank? s)
  (let blank ((i 0))
    (cond ((fx=? i (string-length s)) #t)
          ((char-whitespace? (string-ref s i)) (blank (fx+ i 1)))
          (else #f))))
(define (jolt-str-repeat s n)
  (let ((n (jolt->idx n)))
    (if (fx<=? n 0) ""
        (apply string-append
               (let rep ((i n) (a (quote ()))) (if (fx=? i 0) a (rep (fx- i 1) (cons s a))))))))
(define (jolt-str-code-point-at s i) (char->integer (string-ref s (jolt->idx i))))
(define (jolt-str-last-index-of s needle) (str-last-index-of s (str-needle needle)))
(define (jolt-str-strip s left? right?) (str-strip s left? right?))
(define (jolt-str-to-char-array s) (na-char-array s))
;; String.intern: one canonical object per content, the JVM's string pool. An
;; ephemeron table, because the entry's value is its own key and an ordinary
;; weak table would keep every interned string alive through that value. Lookup
;; is unlocked and the insert re-checks under the lock, the double-check the
;; symbol-string pool (values.ss) uses for the same reason.
;;
;; Once an interned string is collected its entry's key reads #!bwp, and a later
;; lookup or resize hands that to the table's procedures, so both take any key:
;; the bare string=? threw on the first intern to land in such a bucket.
(define (str-intern-hash s) (if (string? s) (string-hash s) 0))
(define (str-intern=? a b) (and (string? a) (string? b) (string=? a b)))
(define jolt-str-intern-pool (make-ephemeron-hashtable str-intern-hash str-intern=?))
(define jolt-str-intern-mu (make-mutex))
(define (jolt-str-intern s)
  (or (hashtable-ref jolt-str-intern-pool s #f)
      (jolt-with-mutex jolt-str-intern-mu
        (or (hashtable-ref jolt-str-intern-pool s #f)
            (begin (hashtable-set! jolt-str-intern-pool s s) s)))))
(define (jolt-str-get-bytes s cs) (na-byte-array (charset-encode-bv s cs)))
(define (jolt-str-matches? s pat) (if (irregex-match (str-irx pat) s) #t #f))
(define (jolt-str-replace-all s pat repl) (re-replace (str-irx pat) s repl #t))
(define (jolt-str-replace-first s pat repl) (re-replace (str-irx pat) s repl #f))
;; re-split, not irregex-split: irregex-split collapses an empty field, so
;; ("a::b" ":") came back ("a" "b") where the JVM gives ("a" "" "b").
;; `limit` arrives raw from the direct-emit path (the JVM's 2-arg overload) and
;; already normalized from split-limit-arg on the dispatch path; normalizing here
;; is idempotent, so both callers can hand over whatever they hold.
(define (jolt-str-split s pat limit)
  (jvm-split-array (str-irx pat) s (if (number? limit) (exact (truncate limit)) 0)))
(define (jolt-str-sub-sequence s from to) (jolt-substr s (jolt->idx from) (jolt->idx to)))
(define (jolt-str-simple-name s)
  (let ((i (str-last-index-of s "."))) (if (>= i 0) (substring s (+ i 1) (string-length s)) s)))

;; --- lattice-proven clojure.core calls ---------------------------------------
;; The back end lowers (count s) / (str a b) to these when the collection lattice
;; proved every operand a string (jolt.passes.types str-prim-op).
;;
;; They tolerate NIL, and that is the whole reason they exist rather than
;; string-length / string-append being emitted directly. A :str type can come from
;; a DECLARED ^String hint, and a hint is not a nil proof — people write ^String on
;; a parameter that may be nil — while (count nil) is 0 and (str nil) is "" in
;; Clojure, which is load-bearing in real code. The nil test costs one branch
;; against the four failed type tests jolt-count runs before its string? arm, and
;; against a var-deref plus jolt-invoke plus str's own render loop.
;;
;; This mirrors the :nilable rule on the struct path: where nil is possible, keep
;; the nil-safe form. A LYING hint (a non-string, non-nil receiver) fails here, the
;; same contract every other hint-directed path has.
(define (jolt-str-count s) (if (jolt-nil? s) 0 (string-length s)))
(define (jolt-str-nil->empty x) (if (jolt-nil? x) "" x))
(define (jolt-str-cat2 a b)
  (string-append (jolt-str-nil->empty a) (jolt-str-nil->empty b)))
(define (jolt-str-cat3 a b c)
  (string-append (jolt-str-nil->empty a) (jolt-str-nil->empty b) (jolt-str-nil->empty c)))

;; The arities String's methods actually have, as a bitmask per name (bit N set =
;; N arguments), and the reason it is a table and not a check inside each arm: the
;; cond below reads its arguments POSITIONALLY, so an extra trailing one was never
;; looked at and (.length "abc" 1) answered 3 where the JVM raises "No matching
;; method length found taking 1 args". One check at entry cannot be forgotten by
;; an arm; the arm that forgets is the one that never gets written.
;;
;; A name with no entry is unchecked, which is the behaviour every name had before
;; this table existed — a method added to the cond without one is no worse off than
;; it was, and the pairing is one screen away.
(define jolt-string-method-arities
  (let ((h (make-hashtable string-hash string=?))
        (mask (lambda (ns) (fold-left (lambda (m n) (bitwise-ior m (bitwise-arithmetic-shift-left 1 n))) 0 ns))))
    (for-each (lambda (e) (hashtable-set! h (car e) (mask (cdr e))))
      '(("length" 0) ("charAt" 1) ("toString" 0) ("indexOf" 1 2) ("startsWith" 1 2)
        ("hashCode" 0) ("toLowerCase" 0 1) ("toUpperCase" 0 1) ("trim" 0)
        ("isEmpty" 0) ("isBlank" 0) ("repeat" 1) ("codePointAt" 1)
        ("codePointBefore" 1) ("codePointCount" 2) ("offsetByCodePoints" 2)
        ("chars" 0) ("codePoints" 0)
        ("substring" 1 2) ("lastIndexOf" 1 2) ("endsWith" 1) ("contains" 1)
        ("concat" 1) ("replace" 2) ("equalsIgnoreCase" 1) ("compareTo" 1)
        ("compareToIgnoreCase" 1) ("contentEquals" 1) ("regionMatches" 4 5)
        ("toCharArray" 0) ("strip" 0) ("stripLeading" 0) ("stripTrailing" 0)
        ("getBytes" 0 1) ("matches" 1) ("replaceAll" 2) ("replaceFirst" 2)
        ("split" 1 2) ("equals" 1) ("intern" 0) ("getChars" 4) ("subSequence" 2)
        ;; the Class / Throwable methods that reach a string receiver: a class
        ;; token is its name string, and a thrown string answers getMessage
        ("getMessage" 0) ("getLocalizedMessage" 0) ("getName" 0)
        ("getCanonicalName" 0) ("getSimpleName" 0) ("isArray" 0)))
    h))

(define (jolt-string-method method s rest)
  ;; Arity first, and only for a call that HAS arguments: too few is already the
  ;; JVM's miss through `arg` below, and the zero-argument calls (length, toString,
  ;; trim) are the hot ones, which this way pay nothing for the check. An arity the
  ;; method does not have takes the same miss an unknown name takes, so a class
  ;; extension registered for it still gets its say.
  (if (and (pair? rest)
           (let ((m (hashtable-ref jolt-string-method-arities method #f)))
             (and m (not (bitwise-bit-set? m (length rest))))))
      (dispatch-miss s method rest)
      (jolt-string-method-arms method s rest)))

(define (jolt-string-method-arms method s rest)
  ;; A missing argument is the JVM's reflective miss (dispatch-miss: a 0-arg read
  ;; reports as a field, more as a method of that arity), not an index fault from
  ;; reading past the argument list — that left the call uncatchable as the
  ;; IllegalArgumentException it is.
  (define (arg n)
    (let loop ((l rest) (i n))
      (cond ((null? l) (dispatch-miss s method rest))
            ((fx=? i 0) (car l))
            (else (loop (cdr l) (fx- i 1))))))
   (cond
    ;; hot-first: length/charAt/indexOf/startsWith dominate library interop
    ;; (honeysql, string codecs); a miss at the bottom of the chain cost ~100ns
    ;; per call in the string arm. Order is behavior-neutral, keep it stable.
    ((string=? method "length") (string-length s))   ; exact int (= JVM)
    ((string=? method "charAt") (jolt-char-at s (arg 0)))
    ((string=? method "toString") s)
    ((string=? method "indexOf")
     (str-index-of-any s (str-arg (arg 0))
                   (if (fx>? (length rest) 1) (jolt->idx (arg 1)) 0)))
    ;; startsWith(prefix) and the JVM's startsWith(prefix, toffset), which asks the
    ;; same question at an offset and is false — not an error — off either end.
    ((string=? method "startsWith")
     (let ((p (str-arg (arg 0)))
           (off (if (fx>? (length rest) 1) (jolt->idx (arg 1)) 0)))
       (and (fx>=? off 0)
            (fx<=? (fx+ off (string-length p)) (string-length s))
            (string=? (substring s off (fx+ off (string-length p))) p))))
    ((string=? method "hashCode") (java-string-hash s))
    ;; The 1-arg forms take a Locale, which jolt drops: case mapping here is the
    ;; Unicode default, the same choice String/format makes for its Locale.
    ((string=? method "toLowerCase") (string-downcase s))
    ((string=? method "toUpperCase") (string-upcase s))
    ((string=? method "trim") (str-trim s))
    ((string=? method "isEmpty") (fx=? (string-length s) 0))
    ((string=? method "isBlank") (jolt-str-blank? s))
    ((string=? method "repeat") (jolt-str-repeat s (arg 0)))
    ((string=? method "codePointAt") (jolt-str-code-point-at s (arg 0)))
    ((string=? method "codePointBefore")
     (let ((j (- (jolt->idx (arg 0)) 1)) (n (string-length s)))
       (unless (and (fixnum? j) (fx>=? j 0) (fx<? j n)) (char-index-oob j n))
       (char->integer (string-ref s j))))
    ;; jolt's strings hold code points, so a count or an offset in code points is
    ;; the same count in chars; the bounds are the JDK's.
    ((string=? method "codePointCount")
     (let ((b (jolt->idx (arg 0))) (e (jolt->idx (arg 1))) (n (string-length s)))
       (jvm-range-check 'IndexOutOfBoundsException n b e)
       (- e b)))
    ((string=? method "offsetByCodePoints")
     (let* ((i (jolt->idx (arg 0))) (r (+ i (jolt->idx (arg 1)))) (n (string-length s)))
       (when (or (< i 0) (> i n) (< r 0) (> r n)) (throw-jvm 'IndexOutOfBoundsException jolt-nil))
       r))
    ((or (string=? method "chars") (string=? method "codePoints")) (string->int-stream s))
    ((string=? method "substring")
     (jolt-substr s (jolt->idx (arg 0))
                  (if (fx>? (length rest) 1) (jolt->idx (arg 1)) (string-length s))))
    ((string=? method "lastIndexOf")
     (if (fx>? (length rest) 1)
         (str-last-index-of-from s (str-needle (arg 0)) (jolt->idx (arg 1)))
         (jolt-str-last-index-of s (arg 0))))
    ((string=? method "endsWith")
     (let ((p (str-arg (arg 0))) (slen (string-length s)))
       (and (fx>=? slen (string-length p))
            (string=? (substring s (fx- slen (string-length p)) slen) p))))
    ((string=? method "contains")
     (fx>=? (str-index-of s (str-needle (arg 0)) 0) 0))
    ((string=? method "concat") (string-append s (str-arg (arg 0))))
    ((string=? method "replace") (str-replace-literal s (str-needle (arg 0)) (str-needle (arg 1))))
    ;; These three go through the same jolt-str-* wrappers the PROVEN-receiver
    ;; path emits (below), so the hinted and generic paths cannot answer
    ;; differently — a divergence between them would surface only on the hinted
    ;; path, which is where nobody looks.
    ((string=? method "equalsIgnoreCase") (jolt-str-equals-ci? s (arg 0)))
    ((string=? method "compareTo") (jolt-str-compare s (arg 0)))
    ((string=? method "compareToIgnoreCase") (jolt-str-compare-ci s (arg 0)))
    ;; CharSequence content equality — the same characters, whatever the receiver's
    ;; concrete type (a StringBuilder compares equal to the String it holds).
    ((string=? method "contentEquals")
     (string=? s (jolt-str-render-one (arg 0))))
    ;; (.regionMatches s toffset other ooffset len), plus the leading-boolean
    ;; ignore-case overload the JVM also has.
    ((string=? method "regionMatches")
     (let* ((ic? (and (boolean? (arg 0)) (arg 0)))
            (base (if (boolean? (arg 0)) 1 0))
            (toff (jolt->idx (arg base)))
            (other (jolt-need-str (arg (fx+ base 1))))
            (ooff (jolt->idx (arg (fx+ base 2))))
            (len (jolt->idx (arg (fx+ base 3)))))
       (and (fx>=? toff 0) (fx>=? ooff 0)
            (fx<=? (fx+ toff len) (string-length s))
            (fx<=? (fx+ ooff len) (string-length other))
            (let ((a (substring s toff (fx+ toff len)))
                  (b (substring other ooff (fx+ ooff len))))
              (if ic? (jvm-string-ci=? a b) (string=? a b))))))
    ;; char[] of the string's characters — a real 'char array, the same value
    ;; (char-array s) builds and (String. ca) reads back.
    ((string=? method "toCharArray") (jolt-str-to-char-array s))
    ;; Java 11 strip family. Unicode-aware whitespace, where trim cuts at <= U+0020.
    ((string=? method "strip") (jolt-str-strip s #t #t))
    ((string=? method "stripLeading") (jolt-str-strip s #t #f))
    ((string=? method "stripTrailing") (jolt-str-strip s #f #t))
    ((string=? method "getBytes")
     ;; (.getBytes s) / (.getBytes s charset) -> a jolt byte-array (seqable /
     ;; countable / alength-able, like (byte-array …)); the JVM returns byte[].
     ;; hand the charset argument over UNRENDERED: charset-encode-bv resolves a
     ;; name string or a Charset object through charset-arg-name. Rendering a
     ;; Charset here produced "#object[java.nio.charset.Charset]", which matched
     ;; no arm and silently encoded as UTF-8.
     (jolt-str-get-bytes s (if (null? rest) "utf-8" (arg 0))))
    ((string=? method "matches") (jolt-str-matches? s (arg 0)))
    ((string=? method "replaceAll") (jolt-str-replace-all s (arg 0) (arg 1)))
    ((string=? method "replaceFirst") (jolt-str-replace-first s (arg 0) (arg 1)))
    ((string=? method "split") (jolt-str-split s (arg 0) (split-limit-arg rest 1)))
    ;; universal object-methods that reach a string target (seed object-methods):
    ;; a thrown string / Exception. ctor (which keeps the message string) answers
    ;; getMessage with itself; equals is value equality.
    ((or (string=? method "getMessage") (string=? method "getLocalizedMessage")) s)
    ((string=? method "equals") (jolt-str-equals? s (arg 0)))
    ((string=? method "intern") (jolt-str-intern s))
    ;; A class token is its canonical-name string, so Class methods land here:
    ;; (.getName (.getClass x)) / (.getSimpleName …) over the name string.
    ((or (string=? method "getName") (string=? method "getCanonicalName")) s)
    ((string=? method "getSimpleName") (jolt-str-simple-name s))
    ;; .getChars srcBegin srcEnd dst dstBegin — copy s[srcBegin,srcEnd) into the
    ;; char-array dst at dstBegin (used by buffered readers, e.g. data.json).
    ((string=? method "getChars")
     (let ((src-begin (jolt->idx (arg 0))) (src-end (jolt->idx (arg 1)))
           (dst (arg 2)) (dst-begin (jolt->idx (arg 3))))
       (let loop ((i src-begin) (j dst-begin))
         (when (fx<? i src-end)
           (ja-set! dst j (string-ref s i))
           (loop (fx+ i 1) (fx+ j 1)))))
     jolt-nil)
    ((string=? method "subSequence") (jolt-str-sub-sequence s (arg 0) (arg 1)))
    ;; Class.isArray over a class-name string: array classes are "[…" (e.g. "[C").
    ((string=? method "isArray") (and (fx>? (string-length s) 0) (char=? (string-ref s 0) #\[)))
    ;; the shared end of the chain, so a string reports the same way every other
    ;; value does — including "No matching field found" for a (.-x "s") read
    (else (dispatch-miss s method rest))))

;; --- clojure.core str-* primitives (the substrate clojure.string.clj calls) ---
;; clojure.string.clj is pure Clojure over these
;; natives; def-var!'d here so the emitted
;; clojure.string prelude tier's var-derefs resolve:
;; string/ascii-* (ASCII), string/find (index or nil), core-str-* (regex|literal).

;; (string/split sep s) -> parts, splitting on each non-overlapping sep.
;; The scan used to test each position with (string=? (substring s i (+ i plen))
;; sep) — a fresh substring ALLOCATED per character of the input, thrown away
;; immediately. str-index-of compares in place and skips straight to the next
;; hit, so the walk allocates only the parts it actually returns.
(define (str-literal-split s sep)
  (let* ((s (jolt-need-str s))
         (slen (string-length s))
         (plen (string-length sep)))
    (if (fx=? plen 0)
        (map string (string->list s))
        (let loop ((start 0) (acc '()))
          (let ((i (str-index-of s sep start)))
            (if (fx<? i 0)
                (reverse (cons (substring s start slen) acc))
                (loop (fx+ i plen) (cons (substring s start i) acc))))))))

;; clojure.string/upper-case and lower-case, and String's toUpperCase /
;; toLowerCase, map the whole of Unicode on the JVM — Cyrillic, Greek and the
;; accented Latin ranges included. Chez's own case mappings are the Unicode ones,
;; so use them; the ASCII pair above stays for the places that mean ASCII (a
;; charset name, a header key) and must not fold a non-ASCII character.
(define (str-upper s) (string-upcase s))
(define (str-lower s) (string-downcase s))
(define (str-reverse-b s) (list->string (reverse (string->list s))))

;; (str-find needle haystack) -> exact int index of first occurrence, or nil.
;; optional third arg: search from that index (the IReader cursors use it so a
;; line drain does not re-copy the tail just to search it).
(define (str-find needle s . opt)
  (let ((i (str-index-of s needle (if (pair? opt) (car opt) 0))))
    (if (fx<? i 0) jolt-nil i)))
;; (str-last-find needle s from) -> exact int index of the last occurrence that
;; STARTS at or before FROM, or nil: String.lastIndexOf(str, from) — a backward
;; scan, so a needle near the tail costs nothing for what precedes it (the
;; wrapper once reversed both strings and searched forward, linear in the subject
;; on every call). A char needle is the one-char string the JVM's char overload
;; answers identically for.
(define (str-last-find needle s from)
  (let ((i (str-last-index-of-from s (str-needle needle) from)))
    (if (fx<? i 0) jolt-nil i)))

;; --- native one-shots for clojure.string's hot wrappers ----------------------
;; The prelude's compiled wrappers chain overlay calls per invocation
;; (to-str -> count -> subs -> = is 4-5 var derefs plus a substring ALLOCATION),
;; ~400-500ns where the substrate is ~40ns; honeysql's format path calls
;; starts-with?/includes? several times per entity formatted. These single-proc
;; versions carry the wrapper's exact semantics (NPE on nil args, s coerced via
;; toString, substr raw) and allocate nothing. post-prelude.ss installs them
;; over the prelude-baked vars.
(define (str-coerce s)
  (cond ((string? s) s)
        ((jolt-nil? s) (throw-jvm 'NullPointerException "s"))
        (else (record-method-dispatch s "toString" jolt-nil))))
;; JVM starts-with?/ends-with? pass substr straight to .startsWith/.endsWith —
;; anything but a String is a ClassCastException (nil is an NPE).
(define (str-need-substr p)
  (cond ((string? p) p)
        ((jolt-nil? p) (throw-jvm 'NullPointerException "substr"))
        (else (throw-jvm 'ClassCastException
                         (string-append "class " (jolt-class-name p)
                                        " cannot be cast to class java.lang.String")))))
(define (str-starts-with? s p)
  (let ((p (str-need-substr p))
        (s (str-coerce s)))
    (and (fx>=? (string-length s) (string-length p))
         (let loop ((i 0))
           (or (fx=? i (string-length p))
               (and (char=? (string-ref s i) (string-ref p i))
                    (loop (fx+ i 1))))))))
(define (str-ends-with? s p)
  (let* ((p (str-need-substr p))
         (s (str-coerce s))
         (n (string-length s)))
    (let ((m (string-length p)))
      (and (fx>=? n m)
           (let loop ((i 0))
             (or (fx=? i m)
                 (and (char=? (string-ref s (fx+ (fx- n m) i)) (string-ref p i))
                      (loop (fx+ i 1)))))))))
(define (str-includes? s p)
  (fx>=? (str-index-of-any (str-coerce s) (if (jolt-nil? p) (throw-jvm 'NullPointerException "value") p) 0) 0))
(define (str-index-of* s v . opt)
  (let* ((s (str-coerce s))
         (n (string-length s))
         (from (if (pair? opt)
                   (min (max 0 (jnum->exact (car opt))) n)
                   0))
         (i (str-index-of-any s v from)))
    (if (fx<? i 0) jolt-nil i)))
(define (str-upper-c s) (str-upper (str-coerce s)))
(define (str-lower-c s) (str-lower (str-coerce s)))

;; (str-join coll [sep]) -> stringify each element (Clojure str), join by sep.
;; str-join-strs (defined below) does the join; here we just render each element.
;; One seq walk, no intermediate list when the coll is 0/1 elements (the common
;; case for entity/column joining): the old map-over-seq->list tripled the walks
;; and cost ~260ns for a single-element join.
(define (str-join coll . opt)
  (let ((sep (if (pair? opt) (jolt-str-render-one (car opt)) "")))
    (let ((s (jolt-seq coll)))
      (if (jolt-nil? s)
          ""
          (let ((f (jolt-str-render-one (seq-first s)))
                (r (jolt-seq (seq-more s))))
            (if (jolt-nil? r)
                f
                (str-join-strs
                 (cons f (let loop ((r r))
                           (if (jolt-nil? r)
                               '()
                               (cons (jolt-str-render-one (seq-first r))
                                     (loop (jolt-seq (seq-more r)))))))
                 sep)))))))

;; JVM split semantics over re-split, shared by String.split and Pattern.split:
;;   limit > 0   at most `limit` parts, the last left unsplit
;;   limit = 0   split fully, trailing empty strings dropped — the 1-arg default
;;   limit < 0   split fully, trailing empty strings KEPT
;; Both methods used to discard the limit argument entirely, so `(.split "a:b:c" ":" 2)`
;; came back three-way and every caller splitting a key from a value that may itself
;; contain the separator (a URL header, a password, a status line's description) got
;; the value truncated at its first separator.
(define (jvm-split irx s limit)
  (let ((parts (re-split irx s (and (fx>? limit 0) limit))))
    (if (fx=? limit 0) (str-split-drop-trailing parts) parts)))

;; The int limit of a .split call, defaulting to 0 (the 1-arg form).
(define (split-limit-arg rest n)
  (if (fx>? (length rest) n)
      (let ((v (list-ref rest n))) (if (number? v) (exact (truncate v)) 0))
      0))

;; .split answers a String[], so hand back a real array — seqable, countable,
;; nth-able and destructurable here exactly as an array is on the JVM. The two
;; methods used to disagree about the surrogate for it: String.split returned a
;; VECTOR and Pattern.split a SEQ, so the same split printed two different ways and
;; compared equal to a vector through one and not the other. (natives-array.ss loads
;; after this file; the reference resolves when the method runs.)
(define (jvm-split-array irx s limit)
  (make-jolt-array (na-list->backing (jvm-split irx s limit) 'object) 'object))

;; re-split's semantics over a literal separator: interior AND trailing empty
;; strings kept, a positive limit capping the parts with the tail left unsplit.
;; (The clojure.string wrapper layers the trailing-empty trim on top, exactly as
;; it does for the engine path.)
(define (literal-split s sep limit)
  (let* ((s (jolt-need-str s))
         (slen (string-length s))
         (plen (string-length sep)))
    (let loop ((start 0) (out '()) (nout 0))
      (if (and limit (fx>=? nout (fx- limit 1)))
          (reverse (cons (substring s start slen) out))
          (let ((i (str-index-of s sep start)))
            (if (fx<? i 0)
                (reverse (cons (substring s start slen) out))
                (loop (fx+ i plen) (cons (substring s start i) out) (fx+ nout 1))))))))

;; clojure.string/split-lines, which is (split s #"\r?\n") — the one line-shaped
;; pattern that is NOT a literal, so the recognizer above cannot help it and it
;; kept paying an irregex search per line (1201 ms against babashka's 234 ms).
;; Scanning for \n and dropping a \r immediately before it is the same language:
;; \r?\n matches \n with an optional \r in front, and nothing else — a BARE \r
;; is not a terminator here (that is line-seq's rule, not this one).
;; Trailing empties are dropped by the wrapper, as they are for limit 0.
(define (str-split-lines s)
  (let* ((s (jolt-need-str s))
         (len (string-length s)))
    (let loop ((start 0) (out '()))
      (let ((i (str-char-index s #\newline start)))
        (if (fx<? i 0)
            (reverse (cons (substring s start len) out))
            (let ((end (if (and (fx>? i start) (char=? (string-ref s (fx- i 1)) #\return))
                           (fx- i 1)
                           i)))
              (loop (fx+ i 1) (cons (substring s start end) out))))))))

;; (str-split pat s [limit]) -> parts. Regex or literal separator; a positive
;; limit caps the part count (the unsplit tail kept), matching core-str-split.
(define (str-split pat s . opt)
  (let ((limit (if (and (pair? opt) (not (jolt-nil? (car opt)))) (jolt->idx (car opt)) #f)))
    (if (jolt-regex? pat)
        (let ((src (regex-t-source pat)))
          (apply jolt-vector
                 (cond
                   ;; clojure.string/split-lines is (split s #"\r?\n"), the one
                   ;; line-shaped pattern that is not a literal, so it kept
                   ;; paying an irregex search per line. Recognised here rather
                   ;; than in stdlib/clojure/string.clj so the whole recognizer
                   ;; lives in one place — and so `(split s #"\r?\n")` spelled
                   ;; out by hand is just as fast as the named wrapper.
                   ((and (not limit) (string=? src "\\r?\\n")) (str-split-lines s))
                   ((regex-literal-text src) => (lambda (lit) (literal-split s lit limit)))
                   (else (re-split (regex-t-irx pat) s limit)))))
        (let ((parts (str-literal-split s pat)))
          (apply jolt-vector
            (if (and limit (fx>? limit 0) (fx>? (length parts) limit))
                (append (list-head parts (fx- limit 1))
                        (list (str-join-strs (list-tail parts (fx- limit 1)) pat)))
                parts))))))
(define (str-join-strs strs sep)
  (let loop ((xs strs) (first #t) (acc '()))
    (cond ((null? xs) (apply string-append (reverse acc)))
          (first (loop (cdr xs) #f (cons (car xs) acc)))
          (else (loop (cdr xs) #f (cons (car xs) (cons sep acc)))))))

;; (str-replace-all pat repl s) / (str-replace pat repl s) — regex or literal.
(define (str-replace-all pat repl s)
  (let ((lit (literal-replace-text pat repl)))
    (cond
      (lit (str-replace-literal s lit repl))
      ((jolt-regex? pat) (re-replace (regex-t-irx pat) s repl #t))
      ;; literal match: a char/number match or replacement (str/replace s \a \b)
      ;; coerces to a string, as on the JVM.
      (else (str-replace-literal s (str-needle pat) (str-needle repl))))))
(define (str-replace-literal-first s a b)
  (let ((alen (string-length a)) (i (str-index-of s a 0)))
    (if (fx<? i 0) s
        (string-append (substring s 0 i) b (substring s (fx+ i alen) (string-length s))))))
(define (str-replace pat repl s)
  (let ((lit (literal-replace-text pat repl)))
    (cond
      (lit (str-replace-literal-first s lit repl))
      ((jolt-regex? pat) (re-replace (regex-t-irx pat) s repl #f))
      (else (str-replace-literal-first s (str-needle pat) (str-needle repl))))))

(def-var! "clojure.core" "str-upper" str-upper)
(def-var! "clojure.core" "str-lower" str-lower)
;; the var backs clojure.string/trim and blank?, so it is the isWhitespace rule;
;; String.trim reaches the <= space one directly.
(def-var! "clojure.core" "str-trim" str-trim*)
(def-var! "clojure.core" "str-triml" str-triml)
(def-var! "clojure.core" "str-trimr" str-trimr)
(def-var! "clojure.core" "str-find" str-find)
(def-var! "clojure.core" "str-last-find" str-last-find)
(def-var! "clojure.core" "str-reverse-b" str-reverse-b)
(def-var! "clojure.core" "str-join" str-join)
(def-var! "clojure.core" "str-split" str-split)
;; str-split-lines is deliberately NOT def-var!'d: it answers a Scheme list, not
;; a jolt vector, so an overlay caller would get #object[:object]. str-split
;; above is its one entry point, and clojure.string/split-lines reaches it by
;; being (split s #"\r?\n") — the pattern the recognizer picks out.
(def-var! "clojure.core" "str-replace" str-replace)
(def-var! "clojure.core" "str-replace-all" str-replace-all)

;; (import — import-spec-of-fqn, chez-runtime-import, clojure.core/__import —
;; lives in ns.ss with the rest of the namespace model; it is shared with the
;; Gambit boot, which excludes this file.)
