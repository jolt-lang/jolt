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
(define (ascii-up-char c)
  (if (and (char<=? #\a c) (char<=? c #\z))
      (integer->char (fx- (char->integer c) 32)) c))
(define (ascii-down-char c)
  (if (and (char<=? #\A c) (char<=? c #\Z))
      (integer->char (fx+ (char->integer c) 32)) c))
(define (ascii-string-up s)
  (let ((n (string-length s)))
    (let check ((i 0))
      (if (fx=? i n)
          s
          (if (and (char<=? #\a (string-ref s i)) (char<=? (string-ref s i) #\z))
              (let ((r (make-string n)))
                (do ((j 0 (fx+ j 1)))
                    ((fx=? j n) r)
                  (string-set! r j (ascii-up-char (string-ref s j)))))
              (check (fx+ i 1)))))))
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
(define (jvm-string-compare a b)
  (let ((la (string-length a)) (lb (string-length b)))
    (let loop ((i 0))
      (if (or (fx=? i la) (fx=? i lb))
          (fx- la lb)
          (let ((ca (string-ref a i)) (cb (string-ref b i)))
            (if (char=? ca cb)
                (loop (fx+ i 1))
                (fx- (char->integer ca) (char->integer cb))))))))

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

(define (str-trim s)
  (let ((len (string-length s)))
    (let scan-l ((i 0))
      (cond ((fx=? i len) "")
            ((char<=? (string-ref s i) #\space) (scan-l (fx+ i 1)))
            (else (let scan-r ((j (fx- len 1)))
                    (if (char<=? (string-ref s j) #\space)
                        (scan-r (fx- j 1))
                        (substring s i (fx+ j 1)))))))))
(define (str-triml s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((fx=? i len) "")
            ((java-whitespace? (string-ref s i)) (loop (fx+ i 1)))
            (else (substring s i len))))))
(define (str-trimr s)
  (let loop ((j (fx- (string-length s) 1)))
    (cond ((fx<? j 0) "")
          ((java-whitespace? (string-ref s j)) (loop (fx- j 1)))
          (else (substring s 0 (fx+ j 1))))))
(define (str-trim* s) (str-trimr (str-triml s)))

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
(define (str-index-of s needle from)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (max 0 from)))
      (cond ((fx>? (fx+ i nlen) slen) -1)
            ((char-by-char-match? s i needle nlen) i)
            (else (loop (fx+ i 1)))))))
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
(define (str-last-index-of s needle)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (fx- slen nlen)) (found -1))
      (cond ((fx<? i 0) found)
            ((char-by-char-match? s i needle nlen) i)
            (else (loop (fx- i 1) found))))))

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
  (let ((alen (string-length a)) (slen (string-length s)))
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
        (let ((first-match (str-index-of s a 0)))
          (if (fx<? first-match 0) s
              (let ((op (open-output-string)))
                (let loop ((i 0))
                  (cond
                   ((fx>? (fx+ i alen) slen)
                    (display (substring s i slen) op)
                    (get-output-string op))
                   ((char-by-char-match? s i a alen)
                    (display b op)
                    (loop (fx+ i alen)))
                   (else
                    (write-char (string-ref s i) op)
                    (loop (fx+ i 1)))))))))))

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
(define c-iconv-open  (jolt-foreign-proc-safe "iconv_open" '(string string) 'void*))
(define c-iconv-conv  (jolt-foreign-proc-safe "iconv" '(void* void* void* void* void*) 'size_t))
(define c-iconv-close (jolt-foreign-proc-safe "iconv_close" '(void*) 'int))
(define iconv-size-max (- (expt 2 (* 8 (sa-foreign-sizeof 'size_t))) 1))

;; iconv_open, or #f when the host has no such charset. The descriptor must be
;; closed by the caller.
(define (iconv-open-cd from to)
  (and c-iconv-open
       (guard (e (#t #f))
         (let ((cd (c-iconv-open to from)))
           (and (not (= cd iconv-size-max)) (not (= cd 0)) cd)))))

(define (iconv-known? name)
  (let ((cd (iconv-open-cd "UTF-8" name)))
    (and cd (begin (c-iconv-close cd) #t))))

;; Convert bytes between two charsets, or #f if the host cannot. The four
;; iconv arguments are pointers to a cursor pair, so they live in one 32-byte
;; block at offsets 0/8/16/24: in pointer, in remaining, out pointer, out
;; remaining. Worst case a byte grows to four (UTF-32), plus room for a BOM.
(define (iconv-bytes bv from to)
  (and c-iconv-conv c-iconv-close
       (let ((cd (iconv-open-cd from to)))
         (and cd
              (let* ((inlen (bytevector-length bv))
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
                        (and (not (= iconv-size-max
                                     (c-iconv-conv cd cells (+ cells 8) (+ cells 16) (+ cells 24))))
                             ;; Then reset the descriptor to its initial state, which
                             ;; POSIX spells as an iconv with a NULL input. A stateful
                             ;; charset holds a mode, and its closing shift back to
                             ;; ASCII is only emitted here — without it
                             ;; (.getBytes "い" "ISO-2022-JP") stops after the
                             ;; character and drops the trailing ESC ( B the JVM
                             ;; writes. Stateless charsets write nothing.
                             (begin (c-iconv-conv cd 0 0 (+ cells 16) (+ cells 24))
                                    #t)
                             (let* ((n (- outcap (sa-foreign-ref 'unsigned-64 cells 24)))
                                    (out (make-bytevector n)))
                               (do ((i 0 (+ i 1))) ((= i n) out)
                                 (bytevector-u8-set! out i (sa-foreign-ref 'unsigned-8 outbuf i))))))))
                (sa-foreign-free inbuf) (sa-foreign-free outbuf) (sa-foreign-free cells)
                (c-iconv-close cd)
                result)))))

(define (unsupported-encoding-throw name)
  (jolt-throw (jolt-host-throwable "java.io.UnsupportedEncodingException" name)))

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
        ((member cs '("iso-8859-1" "us-ascii"))
         (let* ((n (string-length s)) (bv (make-bytevector n)))
           (do ((i 0 (+ i 1))) ((= i n) bv)
             (bytevector-u8-set! bv i (bitwise-and (char->integer (string-ref s i)) #xff)))))
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
        (else (or (iconv-bytes (string->utf8 s) "UTF-8" name)
                  (unsupported-encoding-throw name)))))))

;; Object.hashCode parity: Java's specified String hash and Clojure's Symbol hash
;; (Util.hashCombine), so (.hashCode s) / (.hashCode sym) match the JVM. 32-bit int.
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

(define (jolt-string-method method s rest)
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
    ((string=? method "charAt") (string-ref s (jolt->idx (arg 0))))
    ((string=? method "toString") s)
    ((string=? method "indexOf")
     (str-index-of-any s (str-arg (arg 0))
                   (if (fx>? (length rest) 1) (jolt->idx (arg 1)) 0)))
    ((string=? method "startsWith")
     (let ((p (str-arg (arg 0)))) (and (fx>=? (string-length s) (string-length p))
                             (string=? (substring s 0 (string-length p)) p))))
    ((string=? method "hashCode") (java-string-hash s))
    ((string=? method "toLowerCase") (string-downcase s))
    ((string=? method "toUpperCase") (string-upcase s))
    ((string=? method "trim") (str-trim s))
    ((string=? method "isEmpty") (fx=? (string-length s) 0))
    ((string=? method "isBlank")
     (let blank ((i 0))
       (cond ((fx=? i (string-length s)) #t)
             ((char-whitespace? (string-ref s i)) (blank (fx+ i 1)))
             (else #f))))
    ((string=? method "repeat")
     (let ((n (jolt->idx (arg 0))))
       (if (fx<=? n 0) ""
           (apply string-append (let rep ((i n) (a '())) (if (fx=? i 0) a (rep (fx- i 1) (cons s a))))))))
    ((string=? method "codePointAt")
     (char->integer (string-ref s (jolt->idx (arg 0)))))
    ((string=? method "substring")
     (substring s (jolt->idx (arg 0))
                (if (fx>? (length rest) 1) (jolt->idx (arg 1)) (string-length s))))
    ((string=? method "lastIndexOf")
     (str-last-index-of s (str-needle (arg 0))))
    ((string=? method "endsWith")
     (let ((p (str-arg (arg 0))) (slen (string-length s)))
       (and (fx>=? slen (string-length p))
            (string=? (substring s (fx- slen (string-length p)) slen) p))))
    ((string=? method "contains")
     (fx>=? (str-index-of s (str-needle (arg 0)) 0) 0))
    ((string=? method "concat") (string-append s (str-arg (arg 0))))
    ((string=? method "replace") (str-replace-literal s (str-needle (arg 0)) (str-needle (arg 1))))
    ((string=? method "equalsIgnoreCase")
     (let ((o (arg 0)))
       (and (not (jolt-nil? o))
            (jvm-string-ci=? s (jolt-need-str o)))))
    ;; compareTo answers an INT on the JVM, not a double — it fed straight into
    ;; (neg? …) fine but printed as -1.0, and (= -1 (.compareTo …)) was false.
    ((string=? method "compareTo")
     (jvm-string-compare s (jolt-need-str (arg 0))))
    ((string=? method "compareToIgnoreCase")
     (jvm-string-ci-compare s (jolt-need-str (arg 0))))
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
    ((string=? method "toCharArray") (na-char-array s))
    ;; Java 11 strip family. Unicode-aware whitespace, where trim cuts at <= U+0020.
    ((string=? method "strip") (str-strip s #t #t))
    ((string=? method "stripLeading") (str-strip s #t #f))
    ((string=? method "stripTrailing") (str-strip s #f #t))
    ((string=? method "getBytes")
     ;; (.getBytes s) / (.getBytes s charset) -> a jolt byte-array (seqable /
     ;; countable / alength-able, like (byte-array …)); the JVM returns byte[].
     ;; hand the charset argument over UNRENDERED: charset-encode-bv resolves a
     ;; name string or a Charset object through charset-arg-name. Rendering a
     ;; Charset here produced "#object[java.nio.charset.Charset]", which matched
     ;; no arm and silently encoded as UTF-8.
     (na-byte-array
      (charset-encode-bv s (if (null? rest) "utf-8" (arg 0)))))
    ((string=? method "matches") (if (irregex-match (str-irx (arg 0)) s) #t #f))
    ((string=? method "replaceAll") (irregex-replace/all (str-irx (arg 0)) s (arg 1)))
    ((string=? method "replaceFirst") (irregex-replace (str-irx (arg 0)) s (arg 1)))
    ;; re-split, not irregex-split: irregex-split collapses an empty field, so
    ;; ("a::b" ":") came back ("a" "b") where the JVM gives ("a" "" "b").
    ((string=? method "split")
     (jvm-split-array (str-irx (arg 0)) s (split-limit-arg rest 1)))
    ;; universal object-methods that reach a string target (seed object-methods):
    ;; a thrown string / Exception. ctor (which keeps the message string) answers
    ;; getMessage with itself; equals is value equality.
    ((or (string=? method "getMessage") (string=? method "getLocalizedMessage")) s)
    ((string=? method "equals") (and (string? (arg 0)) (string=? s (arg 0))))
    ;; String.intern: jolt strings aren't pooled, but value equality holds, so the
    ;; canonical representation is the string itself.
    ((string=? method "intern") s)
    ;; A class token is its canonical-name string, so Class methods land here:
    ;; (.getName (.getClass x)) / (.getSimpleName …) over the name string.
    ((or (string=? method "getName") (string=? method "getCanonicalName")) s)
    ((string=? method "getSimpleName")
     (let ((i (str-last-index-of s "."))) (if (>= i 0) (substring s (+ i 1) (string-length s)) s)))
    ;; .getChars srcBegin srcEnd dst dstBegin — copy s[srcBegin,srcEnd) into the
    ;; char-array dst at dstBegin (used by buffered readers, e.g. data.json).
    ((string=? method "getChars")
     (let ((src-begin (jolt->idx (arg 0))) (src-end (jolt->idx (arg 1)))
           (dv (jolt-array-vec (arg 2))) (dst-begin (jolt->idx (arg 3))))
       (let loop ((i src-begin) (j dst-begin))
         (when (fx<? i src-end)
           (vector-set! dv j (string-ref s i))
           (loop (fx+ i 1) (fx+ j 1)))))
     jolt-nil)
    ((string=? method "subSequence")
     (substring s (jolt->idx (arg 0)) (jolt->idx (arg 1))))
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
(define (str-literal-split s sep)
  (let ((slen (string-length (jolt-need-str s))) (plen (string-length sep)))
    (if (fx=? plen 0)
        (map string (string->list s))
        (let loop ((i 0) (start 0) (acc '()))
          (cond ((fx>? (fx+ i plen) slen)
                 (reverse (cons (substring s start slen) acc)))
                ((string=? (substring s i (fx+ i plen)) sep)
                 (loop (fx+ i plen) (fx+ i plen) (cons (substring s start i) acc)))
                (else (loop (fx+ i 1) start acc)))))))

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

;; (re-split irx s limit) -> parts, splitting at each match. Keeps interior AND
;; trailing empty strings (the clojure.string wrapper drops trailing for limit 0);
;; a positive limit yields at most `limit` parts (the rest kept unsplit).
;; The clojure.string.clj split wrapper
;; layers the trailing-empty trim on top.
(define (re-split irx s limit)
  (let* ((s (jolt-need-str s))
         (len (string-length s)))
    ;; nout counts out — (length out) per part made a limited split O(parts^2)
    (let loop ((start 0) (last 0) (out '()) (nout 0))
      (if (and limit (fx>=? nout (fx- limit 1)))
          (reverse (cons (substring s last len) out))
          (let ((m (and (fx<=? start len) (irx-search-from irx s start))))
            (if (not m)
                (reverse (cons (substring s last len) out))
                (let ((ms (irregex-match-start-index m 0))
                      (me (irregex-match-end-index m 0)))
                  (if (fx=? me ms)                 ; zero-width: emit single-char segment
                      (if (fx>=? start len)
                          (reverse (cons (substring s last len) out))
                          ;; Emit the segment from last to this match point, skip
                          ;; leading empty (JVM semantics for zero-width splits).
                          (let ((seg (substring s last ms)))
                            (if (and (string=? seg "") (null? out))
                                (loop (fx+ start 1) me out nout)
                                (loop (fx+ start 1) me (cons seg out) (fx+ nout 1)))))
                      (loop me me (cons (substring s last ms) out) (fx+ nout 1))))))))))

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

;; (str-split pat s [limit]) -> parts. Regex or literal separator; a positive
;; limit caps the part count (the unsplit tail kept), matching core-str-split.
(define (str-split pat s . opt)
  (let ((limit (if (and (pair? opt) (not (jolt-nil? (car opt)))) (jolt->idx (car opt)) #f)))
    (if (jolt-regex? pat)
        (apply jolt-vector (re-split (regex-t-irx pat) s limit))
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

;; Replacement-string expansion against an irregex match, with the JVM's
;; Matcher.appendReplacement syntax: $N inserts group N's text (dropped when the
;; group didn't participate) and a backslash escapes the next character — so
;; \\ inserts one backslash and \$ a literal dollar. re-quote-replacement's
;; output round-trips through this.
(define (expand-dollar repl m)
  (let ((len (string-length repl)))
    (let loop ((i 0) (acc '()))
      (if (fx>=? i len)
          (apply string-append (reverse acc))
          (let ((c (string-ref repl i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (loop (fx+ i 2) (cons (string (string-ref repl (fx+ i 1))) acc)))
              ((and (char=? c #\$) (fx<? (fx+ i 1) len)
                    (char<=? #\0 (string-ref repl (fx+ i 1)))
                    (char<=? (string-ref repl (fx+ i 1)) #\9))
               (let* ((n (fx- (char->integer (string-ref repl (fx+ i 1))) 48))
                      (g (and (fx<=? n (irregex-match-num-submatches m))
                              (irregex-match-substring m n))))
                 (loop (fx+ i 2) (if g (cons g acc) acc))))
              (else (loop (fx+ i 1) (cons (string c) acc)))))))))

;; One match's replacement text. A string gets $N expansion; a fn (jolt closure)
;; is called with the match result (whole string, or [whole g1 ...] when grouped)
;; and its result stringified.
(define (replacement-text replacement m)
  (cond
    ((string? replacement) (expand-dollar replacement m))
    ((procedure? replacement) (jolt-str-render-one (jolt-invoke replacement (irx-result m))))
    (else (jolt-str-render-one replacement))))

;; regex replace, first or all matches.
(define (re-replace irx s replacement all?)
  (let ((len (string-length s)))
    (let loop ((start 0) (last 0) (acc '()))
      (let ((m (and (fx<=? start len) (irx-search-from irx s start))))
        (if (not m)
            (apply string-append (reverse (cons (substring s last len) acc)))
            (let ((ms (irregex-match-start-index m 0))
                  (me (irregex-match-end-index m 0)))
              (if (fx=? me ms)                     ; zero-width: step past
                  (if (fx>=? start len)
                      (apply string-append (reverse (cons (substring s last len) acc)))
                      (loop (fx+ start 1) last acc))
                  (let ((acc2 (cons (replacement-text replacement m)
                                    (cons (substring s last ms) acc))))
                    (if all?
                        (loop me me acc2)
                        (apply string-append (reverse (cons (substring s me len) acc2))))))))))))

;; (str-replace-all pat repl s) / (str-replace pat repl s) — regex or literal.
(define (str-replace-all pat repl s)
  (if (jolt-regex? pat)
      (re-replace (regex-t-irx pat) s repl #t)
      ;; literal match: a char/number match or replacement (str/replace s \a \b)
      ;; coerces to a string, as on the JVM.
      (str-replace-literal s (str-needle pat) (str-needle repl))))
(define (str-replace-literal-first s a b)
  (let ((alen (string-length a)) (i (str-index-of s a 0)))
    (if (fx<? i 0) s
        (string-append (substring s 0 i) b (substring s (fx+ i alen) (string-length s))))))
(define (str-replace pat repl s)
  (if (jolt-regex? pat)
      (re-replace (regex-t-irx pat) s repl #f)
      (str-replace-literal-first s (str-needle pat) (str-needle repl))))

(def-var! "clojure.core" "str-upper" str-upper)
(def-var! "clojure.core" "str-lower" str-lower)
;; the var backs clojure.string/trim and blank?, so it is the isWhitespace rule;
;; String.trim reaches the <= space one directly.
(def-var! "clojure.core" "str-trim" str-trim*)
(def-var! "clojure.core" "str-triml" str-triml)
(def-var! "clojure.core" "str-trimr" str-trimr)
(def-var! "clojure.core" "str-find" str-find)
(def-var! "clojure.core" "str-reverse-b" str-reverse-b)
(def-var! "clojure.core" "str-join" str-join)
(def-var! "clojure.core" "str-split" str-split)
(def-var! "clojure.core" "str-replace" str-replace)
(def-var! "clojure.core" "str-replace-all" str-replace-all)

;; (require ...) / (use ...) at runtime: register each spec's :as alias + :refer
;; names into the runtime ns tables (chez-register-spec!, ns.ss), keyed by the
;; current ns. The spine also pre-registers these at analyze time (idempotent),
;; so ns-aliases/ns-resolve over an :as alias resolve. Specs arrive evaluated
;; (quoted).
(define (chez-runtime-require . specs)
  (for-each (lambda (s) (chez-register-spec! (chez-current-ns) s)) specs)
  jolt-nil)
(def-var! "clojure.core" "require" chez-runtime-require)
;; use = require + refer ALL of the target's public vars (unless an explicit
;; :only/:refer filter is given, which chez-register-spec! handles per-name).
(define (chez-runtime-use . specs)
  (for-each
    (lambda (spec)
      (chez-register-spec! (chez-current-ns) spec)
      (let* ((items (cond ((pvec? spec) (seq->list spec))
                          ((or (cseq? spec) (empty-list-t? spec)) (seq->list spec))
                          ((symbol-t? spec) (list spec))
                          (else '())))
             (target (and (pair? items) (symbol-t? (car items)) (symbol-t-name (car items))))
             (filtered (let scan ((xs (if (pair? items) (cdr items) '())))
                         (cond ((null? xs) #f)
                               ((and (keyword? (car xs))
                                     (member (keyword-t-name (car xs)) '("only" "refer"))) #t)
                               (else (scan (cdr xs))))))
             (excluded (let scan ((xs (if (pair? items) (cdr items) '())))
                         (cond ((null? xs) '())
                               ((and (keyword? (car xs))
                                     (string=? (keyword-t-name (car xs)) "exclude")
                                     (pair? (cdr xs)))
                                (map symbol-t-name (filter symbol-t? (seq->list (cadr xs)))))
                               (else (scan (cdr xs)))))))
        (when (and target (not filtered))
          (chez-register-refer-all! (chez-current-ns) target)
          (chez-register-refer-all-excludes! (chez-current-ns) target excluded))))
    specs)
  jolt-nil)
(def-var! "clojure.core" "use" chez-runtime-use)
;; import: bring a deftype/defrecord from another ns into the current one. A spec
;; [from-ns Type ...] binds each Type's ctor closure under the current ns, so its
;; (Type. ...) constructor (host-new resolves it as a var) works after :import.
;; A bare fully-qualified symbol spec — (import 'java.util.Date), or java.util.Date
;; in an ns :import clause — is the (java.util Date) list it abbreviates. A name
;; with no package (a default-package class the JVM would look up) binds nothing.
(define (import-spec-of-fqn nm)
  (let ((i (let loop ((i (fx- (string-length nm) 1)))
             (cond ((fx<? i 0) #f)
                   ((char=? (string-ref nm i) #\.) i)
                   (else (loop (fx- i 1)))))))
    (if i
        (list (jolt-symbol #f (substring nm 0 i))
              (jolt-symbol #f (substring nm (fx+ i 1) (string-length nm))))
        '())))
(define (chez-runtime-import . specs)
  (for-each
    (lambda (spec)
      (let ((items (cond ((pvec? spec) (seq->list spec))
                         ((or (cseq? spec) (empty-list-t? spec)) (seq->list spec))
                         ((symbol-t? spec) (import-spec-of-fqn (symbol-t-name spec)))
                         (else '()))))
        (when (and (pair? items) (symbol-t? (car items)))
          (let ((from (symbol-t-name (car items))))
            (for-each
              (lambda (tn)
                (when (symbol-t? tn)
                  ;; bind the short name to the interned CLASS value (java.lang.Class
                  ;; token) for its fully-qualified name — the same self-evaluating
                  ;; pattern the core Long/Integer/String tokens use. For a deftype/
                  ;; defrecord this is its "ns.Name" class, equal to (type inst) /
                  ;; (class inst), so (= SomeType (type inst)) and (instance? SomeType
                  ;; x) work; (SomeType. …) construction resolves through the ctor
                  ;; registry (host-new), not this binding.
                  (def-var! (chez-current-ns) (symbol-t-name tn)
                            (jolt-class-for (string-append from "." (symbol-t-name tn))))))
              (cdr items))))))
    specs)
  jolt-nil)
;; clojure.core/import is a macro (00-syntax.clj) expanding to this runtime fn.
(def-var! "clojure.core" "__import" chez-runtime-import)
