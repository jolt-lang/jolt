;; regex-translate.ss — Java regex pattern → irregex SRE translator.
;;
;; Parses a Java/Clojure regex pattern string ONCE via recursive descent and emits
;; an irregex SRE (s-expression AST). This is the sole compile path: regex.ss
;; compiles every pattern through (java-pattern->sre ...).
;;
;; Loaded before regex.ss; exports (java-pattern->sre pat-string) → values(sre opts).
;; The SRE is passed directly to (irregex sre . opts), bypassing irregex's PCRE
;; string reader entirely.

;; ── helpers ───────────────────────────────────────────────────────────────────

(define (oct-value ch)
  (let ((n (char->integer ch)))
    (and (>= n 48) (<= n 55) (- n 48))))

(define (oct-value? ch)
  (let ((n (char->integer ch)))
    (and (>= n 48) (<= n 55))))

(define (hex-value c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        (else #f)))

(define (str-scan-char s c start end)
  (let loop ((i start))
    (cond ((>= i end) #f)
          ((char=? (string-ref s i) c) i)
          (else (loop (+ i 1))))))

(define (jr-flag? flags f) (memq f flags))

;; Scan for the two-character sequence \E (used inside \Q...\E).
(define (scan-qe src start end)
  (let loop ((i start))
    (cond ((>= (+ i 1) end) #f)
          ((and (char=? (string-ref src i) #\\) (char=? (string-ref src (+ i 1)) #\E))
           i)
          (else (loop (+ i 1))))))

;; ── Pattern-syntax errors ─────────────────────────────────────────────────────
;;
;; A malformed pattern is a java.util.regex.PatternSyntaxException on the JVM, and
;; that exception carries two things beyond the description: the index into the
;; pattern where parsing stopped, and — derived from it — the caret line getMessage
;; renders. So a parse failure here is not a bare (error …) string; it is this
;; record, raised as-is, and regex.ss turns it into the JVM's exact message. index
;; is -1 when the JVM would name no position (its own getMessage then prints no
;; " near index" clause and no caret). A record, not an R6RS condition type: the
;; Gambit boot ##includes this file, and it has define-record-type (a shim) but
;; no define-condition-type.
(define-record-type java-pattern-error (fields desc index))

(define (java-re-error desc idx)
  (raise (make-java-pattern-error desc idx)))

;; ── Inline flag groups: (?onFlags-offFlags) and (?onFlags-offFlags: ───────────
;;
;; java.util.regex.Pattern.group0 reads a flag group with addFlag() over the
;; letters before the "-" and subFlag() over the letters after it.  "idmsuxUc" is
;; the whole alphabet, a second "-" is "Unknown inline modifier", and the group
;; ends either at ")" — the setting then runs to the end of the ENCLOSING group —
;; or at ":", where it runs to the end of this one.
;;
;; That grammar used to be written out four times over: in the parser's
;; leading-flag fast path, in its group parser, and in the two COMMENTS
;; pre-passes (one for the parser, one for the validator).  The four disagreed.
;; Three read a group naming "-" at all as switching flags OFF, so (?x-i) turned
;; COMMENTS on for the JVM and off here; the fourth applied the OFF set before
;; the ON set, so (?i-i:AB) matched "ab".  One parser now, so they cannot drift
;; apart again.
;;
;; i is at the "(" of "(?…".  Answers (on off end terminator) — the flag letters
;; before and after the "-", the index just past the terminator, and the
;; terminator itself — or #f when this is not a flag group at all (a look-around,
;; a name, an unknown letter).
(define flag-group-letters '(#\i #\d #\m #\s #\u #\x #\U #\c))

(define (parse-flag-group src i end)
  (and (< (+ i 1) end)
       (char=? (string-ref src i) #\()
       (char=? (string-ref src (+ i 1)) #\?)
       (let scan ((j (+ i 2)) (on '()) (off '()) (neg #f))
         (and (< j end)
              (let ((c (string-ref src j)))
                (cond
                 ((memv c flag-group-letters)
                  (if neg
                      (scan (+ j 1) on (cons c off) neg)
                      (scan (+ j 1) (cons c on) off neg)))
                 ((and (char=? c #\-) (not neg)) (scan (+ j 1) on off #t))
                 ((or (char=? c #\)) (char=? c #\:))
                  (list (reverse on) (reverse off) (+ j 1) c))
                 (else #f)))))))

(define (flag-group-on g) (car g))
(define (flag-group-off g) (cadr g))
(define (flag-group-end g) (caddr g))
(define (flag-group-scoped? g) (char=? (cadddr g) #\:))

;; Does this group leave COMMENTS on, starting from `now`?  addFlag and then
;; subFlag, so a letter on both sides ends up off: (?x-x) is off, (?x-i) is on.
(define (flag-group-comments? g now)
  (cond ((memv #\x (flag-group-off g)) #f)
        ((memv #\x (flag-group-on g)) #t)
        (else now)))

;; ── COMMENTS (?x): whitespace and #-comments ─────────────────────────────────
;;
;; With COMMENTS in force the JVM drops unescaped whitespace and #-to-end-of-line
;; runs at every token boundary — inside a character class as much as outside —
;; and it does so while parsing, so the mode is scoped like any other flag: an
;; unscoped (?x) runs to the end of the enclosing group, (?x:…) to the end of its
;; own, and (?-x) turns it back off.
;;
;; jolt strips in a pre-pass instead, which comes to the same thing only if the
;; pre-pass tracks the same scope.  It did not: two strippers, one for the parser
;; and one for the validator, both took "the first unscoped group naming x turns
;; stripping on for the rest of the pattern" — which gets (?x:a b)c d, (?x)a
;; (?-x)b c and (a(?x)b c)d e all wrong.  This one carries the flag through the
;; group nesting, and both callers share it.
;;
;; \Q…\E is already gone when this runs (jsc-qe-rewrite has escaped what it
;; quoted), which is what keeps (?x)\Qa b\E matching "a b" rather than "ab".
;;
;; Answers the stripped text and, for each of its positions, the position it came
;; from — a PatternSyntaxException names an index into the unstripped pattern.
;; The map is #f when nothing was stripped, the overwhelmingly common case and
;; the fast path.
(define (x-strip s)
  (let ((n (string-length s)))
    ;; Is there any group that turns COMMENTS on?  If not, nothing is stripped.
    (define (any-x?)
      (let loop ((i 0) (in-class #f))
        (cond ((>= i n) #f)
              ((char=? (string-ref s i) #\\) (loop (+ i 2) in-class))
              ((and (not in-class) (char=? (string-ref s i) #\[)) (loop (+ i 1) #t))
              ((and in-class (char=? (string-ref s i) #\])) (loop (+ i 1) #f))
              ((and (not in-class) (char=? (string-ref s i) #\()
                    (let ((g (parse-flag-group s i n)))
                      (and g (memv #\x (flag-group-on g))
                           (not (memv #\x (flag-group-off g))))))
               #t)
              (else (loop (+ i 1) in-class)))))
    (if (not (any-x?))
        (values s #f)
        (let ((out (open-output-string)) (map '()))
          (define (emit! k)
            (write-char (string-ref s k) out) (set! map (cons k map)))
          (define (emit-range! a b)
            (let lp ((k a)) (when (< k b) (emit! k) (lp (+ k 1)))))
          (let loop ((i 0) (x? #f) (in-class #f) (stack '()))
            (if (>= i n)
                (begin (set! map (cons n map))
                       (values (get-output-string out) (list->vector (reverse map))))
                (let ((c (string-ref s i)))
                  (cond
                   ;; \p{…} / \P{…}: the JVM skips comments once after the "{"
                   ;; and then reads the family name RAW to the first "}", so
                   ;; \p{ Lu } names "Lu " to it and is not the same property as
                   ;; \p{Lu}.  Copying the name through is what makes jolt say so
                   ;; too — stripping it made \p{L u} a legal spelling of \p{Lu}.
                   ;; The name is copied whether or not COMMENTS is on, because a
                   ;; (?x: inside one is name text to the JVM and must not turn
                   ;; stripping on for the rest of the pattern.
                   ((and (char=? c #\\) (< (+ i 2) n)
                         (memv (string-ref s (+ i 1)) '(#\p #\P))
                         (char=? (string-ref s (+ i 2)) #\{))
                    (emit! i) (emit! (+ i 1)) (emit! (+ i 2))
                    (let skip ((j (+ i 3)))
                      (cond ((>= j n) (loop j x? in-class stack))
                            ((and x? (memv (string-ref s j)
                                           '(#\space #\tab #\newline #\return #\x0B #\x0C)))
                             (skip (+ j 1)))
                            ((and x? (char=? (string-ref s j) #\#))
                             (let eol ((k (+ j 1)))
                               (cond ((>= k n) (loop k x? in-class stack))
                                     ((char=? (string-ref s k) #\newline) (skip (+ k 1)))
                                     (else (eol (+ k 1))))))
                            (else
                             (let copy ((k j))
                               (cond ((>= k n) (loop k x? in-class stack))
                                     ((char=? (string-ref s k) #\})
                                      (emit! k) (loop (+ k 1) x? in-class stack))
                                     (else (emit! k) (copy (+ k 1)))))))))
                   ;; An escape is two units, and neither is a token boundary.
                   ;; \x{…} and \N{…} carry a braced argument the JVM skips
                   ;; comments inside like anywhere else — (?x)\x{4 1} is 0x41 —
                   ;; so that "{" is taken here, out of reach of the quantifier
                   ;; peek below, which would otherwise have kept the unit after
                   ;; it and moved every index in a malformed \x{ or \N{.
                   ((and (char=? c #\\) (< (+ i 1) n))
                    (emit! i) (emit! (+ i 1))
                    (if (and (memv (string-ref s (+ i 1)) '(#\x #\N))
                             (< (+ i 2) n) (char=? (string-ref s (+ i 2)) #\{))
                        (begin (emit! (+ i 2)) (loop (+ i 3) x? in-class stack))
                        (loop (+ i 2) x? in-class stack)))
                   ;; A quantifier's "{" peeks the next unit raw (Pattern reads
                   ;; temp[cursor+1] there, not next()), so (?x)a{ 1,2} is an
                   ;; "Illegal repetition" on the JVM while (?x)a{1 ,2} is fine.
                   ;; Only the ONE unit, and only when it is one that would
                   ;; otherwise go: anything else still needs the normal reading
                   ;; (an escape is a pair, a "(" opens a scope).
                   ((and x? (not in-class) (char=? c #\{) (< (+ i 1) n)
                         (memv (string-ref s (+ i 1))
                               '(#\space #\tab #\newline #\return #\x0B #\x0C #\#)))
                    (emit! i) (emit! (+ i 1)) (loop (+ i 2) x? in-class stack))
                   ((and x? (memv c '(#\space #\tab #\newline #\return #\x0B #\x0C)))
                    (loop (+ i 1) x? in-class stack))
                   ((and x? (char=? c #\#))
                    (let skip ((j (+ i 1)))
                      (cond ((>= j n) (loop j x? in-class stack))
                            ((char=? (string-ref s j) #\newline)
                             (loop (+ j 1) x? in-class stack))
                            (else (skip (+ j 1))))))
                   (in-class
                    (emit! i)
                    (loop (+ i 1) x? (not (char=? c #\])) stack))
                   ((char=? c #\[) (emit! i) (loop (+ i 1) x? #t stack))
                   ((char=? c #\()
                    (let ((g (parse-flag-group s i n)))
                      (cond
                       ;; (?flags) — the setting outlives its own ")", so no
                       ;; scope is pushed; the group is copied as it stands, and
                       ;; the parser reads it again for the flags jolt applies.
                       ((and g (not (flag-group-scoped? g)))
                        (emit-range! i (flag-group-end g))
                        (loop (flag-group-end g) (flag-group-comments? g x?)
                              in-class stack))
                       ;; (?flags: — scoped, like any other ( … )
                       (g
                        (emit-range! i (flag-group-end g))
                        (loop (flag-group-end g) (flag-group-comments? g x?)
                              in-class (cons x? stack)))
                       (else (emit! i) (loop (+ i 1) x? in-class (cons x? stack))))))
                   ((char=? c #\))
                    (emit! i)
                    (if (null? stack)
                        (loop (+ i 1) x? in-class stack)
                        (loop (+ i 1) (car stack) in-class (cdr stack))))
                   (else (emit! i) (loop (+ i 1) x? in-class stack))))))))))

;; ── Leading flags ─────────────────────────────────────────────────────────────

(define (regex-flag->opt c)
  (cond ((char=? c #\s) 'single-line)
        ((char=? c #\i) 'case-insensitive)
        ((char=? c #\m) 'multi-line)
        ((char=? c #\x) 'ignore-space)
        ;; d (UNIX_LINES) narrows the terminator set DOT / ^ / $ read — see
        ;; java-nel below. It used to be accepted and dropped, which was harmless
        ;; only because the narrow set was all jolt ever applied.
        ((char=? c #\d) 'unix-lines)
        (else #f)))

;; A run of unscoped flag groups at the head of the pattern becomes irregex
;; options rather than an SRE wrapper.  Same grammar as everywhere else, and the
;; same precedence: the OFF set is applied over the ON set, not before it.
(define (parse-leading-flags src i end)
  (let loop ((i i) (opts '()))
    (let ((g (parse-flag-group src i end)))
      (if (or (not g) (flag-group-scoped? g))
          (values (reverse opts) i)
          (if (and (< (flag-group-end g) end)
                   (memv (string-ref src (flag-group-end g)) '(#\* #\+ #\?)))
              (error 'java-pattern->sre "dangling quantifier after flag group" src)
              (loop (flag-group-end g)
                    (let on ((cs (flag-group-on g)) (opts opts))
                      (if (pair? cs)
                          (let ((f (regex-flag->opt (car cs))))
                            (on (cdr cs) (if f (cons f (remq f opts)) opts)))
                          (let off ((cs (flag-group-off g)) (opts opts))
                            (if (pair? cs)
                                (let ((f (regex-flag->opt (car cs))))
                                  (off (cdr cs) (if f (remq f opts) opts)))
                                opts))))))))))

;; ── \p{...} property class → SRE char-set ─────────────────────────────────────
;; Covers the categories the current pipeline handles, extended to include
;; supplementary-plane letters (the known \p{L} above U+D7FF residual).

(define sre-Zs
  '(or #\space #\xA0 #\x1680 (/ #\x2000 #\x200A) #\x202F #\x205F #\x3000))
(define sre-Z
  '(or #\space #\xA0 #\x1680 (/ #\x2000 #\x200A) #\x2028 #\x2029 #\x202F #\x205F #\x3000))

;; \p{L}/\p{N} used to approximate with a hand-picked range ((/ #\x80
;; #\xD7FF) for L), which is nearly the whole BMP above ASCII and so
;; wrongly matched symbols/punctuation too, e.g. U+2192 → (#941). Build
;; the real Unicode ranges from Chez's own char-general-category (and, for
;; the binary properties, its R6RS char predicates) instead. ~10ms over the
;; full codepoint space, paid once per name and lazily, only if a pattern
;; actually uses it.
(define (unicode-property-ranges in?)
  (let loop ((cp 0) (start #f) (ranges '()))
    (define (close-at cp ranges)
      (if start (cons `(/ ,(integer->char start) ,(integer->char (- cp 1))) ranges) ranges))
    (cond
     ((> cp #x10FFFF) (cons 'or (reverse (close-at cp ranges))))
     ((and (>= cp #xD800) (<= cp #xDFFF)) (loop (+ cp 1) start ranges)) ; surrogates
     (else
      (let ((in? (in? (integer->char cp))))
        (cond
         ((and in? (not start)) (loop (+ cp 1) cp ranges))
         ((and (not in?) start) (loop (+ cp 1) #f (close-at cp ranges)))
         (else (loop (+ cp 1) start ranges))))))))

;; The JVM's rule, which these follow: a Unicode CATEGORY name (\p{L}, \p{Lu},
;; \p{N}, \p{Nd}, \p{P} ...) is the real category over every codepoint, while a
;; POSIX name (\p{Alpha}, \p{Digit}, \p{Upper}, \p{Punct} ...) is ASCII unless
;; UNICODE_CHARACTER_CLASS is on -- so \p{Alpha} matches "a" and not "é", and
;; \p{Nd} matches an Arabic-Indic digit but not a Roman numeral (that is \p{N}).
;;
;; Every general category and category group java.util.regex names (Pattern's
;; CharPredicates), spelled as the R6RS categories it unions. The binary
;; properties the JVM spells \p{IsAlphabetic}, \p{IsLowercase} … are the same
;; Unicode properties R6RS's char-alphabetic? / char-lower-case? … answer, and
;; Character.isLowerCase/isUpperCase/isAlphabetic (javaLowerCase …) are those
;; properties too, not the bare Ll/Lu categories. A script (\p{IsLatin}) or a
;; block (\p{InGreek}) has no data behind it here: jolt has no script or block
;; tables, so those names are refused as unsupported (known-divergences.edn).
(define unicode-category-groups
  '(("L" Lu Ll Lt Lm Lo) ("LC" Lu Ll Lt) ("Lu" Lu) ("Ll" Ll) ("Lt" Lt) ("Lm" Lm) ("Lo" Lo)
    ("M" Mn Mc Me) ("Mn" Mn) ("Mc" Mc) ("Me" Me)
    ("N" Nd Nl No) ("Nd" Nd) ("Nl" Nl) ("No" No)
    ("P" Pc Pd Ps Pe Pi Pf Po) ("Pc" Pc) ("Pd" Pd) ("Ps" Ps) ("Pe" Pe) ("Pi" Pi) ("Pf" Pf) ("Po" Po)
    ("S" Sm Sc Sk So) ("Sm" Sm) ("Sc" Sc) ("Sk" Sk) ("So" So)
    ("C" Cc Cf Cs Co Cn) ("Cc" Cc) ("Cf" Cf) ("Cs" Cs) ("Co" Co) ("Cn" Cn)))

(define unicode-binary-properties
  `(("ALPHABETIC" . ,char-alphabetic?)
    ("LOWERCASE" . ,char-lower-case?)
    ("UPPERCASE" . ,char-upper-case?)
    ("WHITE_SPACE" . ,char-whitespace?) ("WHITESPACE" . ,char-whitespace?)
    ("TITLECASE" . ,(lambda (c) (eq? (char-general-category c) 'Lt)))
    ("LETTER" . ,(lambda (c) (memq (char-general-category c) '(Lu Ll Lt Lm Lo))))
    ("DIGIT" . ,(lambda (c) (eq? (char-general-category c) 'Nd)))
    ("ALNUM" . ,(lambda (c) (or (char-alphabetic? c) (eq? (char-general-category c) 'Nd))))
    ("BLANK" . ,(lambda (c) (or (eq? (char-general-category c) 'Zs) (char=? c #\tab))))
    ("PUNCTUATION" . ,(lambda (c) (memq (char-general-category c) '(Pc Pd Ps Pe Pi Pf Po))))
    ("CONTROL" . ,(lambda (c) (eq? (char-general-category c) 'Cc)))
    ("ASSIGNED" . ,(lambda (c) (not (eq? (char-general-category c) 'Cn))))
    ;; UnicodeProp.HEXDIGIT is DIGIT.is(ch) || the ASCII and fullwidth spellings,
    ;; so EVERY Nd digit is a hex digit to java.util.regex — U+1C50, U+0660 and
    ;; U+0966 all answer \p{IsHex_Digit} on the JDK. A wider set than Unicode's
    ;; own Hex_Digit property, and the JVM's is what this has to answer.
    ("HEX_DIGIT" . ,(lambda (c) (or (eq? (char-general-category c) 'Nd)
                                    (and (char<=? #\0 c) (char<=? c #\9))
                                    (and (char<=? #\a c) (char<=? c #\f))
                                    (and (char<=? #\A c) (char<=? c #\F))
                                    (and (char<=? #\xFF10 c) (char<=? c #\xFF19))
                                    (and (char<=? #\xFF21 c) (char<=? c #\xFF26))
                                    (and (char<=? #\xFF41 c) (char<=? c #\xFF46)))))
    ("JOIN_CONTROL" . ,(lambda (c) (memv (char->integer c) '(#x200C #x200D))))
    ("NONCHARACTER_CODE_POINT" . ,(lambda (c) (let ((n (char->integer c)))
                                                (or (and (>= n #xFDD0) (<= n #xFDEF))
                                                    (= (bitwise-and n #xFFFE) #xFFFE)))))))

;; Character.isX for the javaX names, as the JDK defines them.
(define java-character-predicates
  `(("javaLowerCase" . ,char-lower-case?)
    ("javaUpperCase" . ,char-upper-case?)
    ("javaAlphabetic" . ,char-alphabetic?)
    ("javaTitleCase" . ,(lambda (c) (eq? (char-general-category c) 'Lt)))
    ("javaLetter" . ,(lambda (c) (memq (char-general-category c) '(Lu Ll Lt Lm Lo))))
    ("javaDigit" . ,(lambda (c) (eq? (char-general-category c) 'Nd)))
    ("javaLetterOrDigit" . ,(lambda (c) (memq (char-general-category c) '(Lu Ll Lt Lm Lo Nd))))
    ("javaDefined" . ,(lambda (c) (not (eq? (char-general-category c) 'Cn))))
    ("javaSpaceChar" . ,(lambda (c) (memq (char-general-category c) '(Zs Zl Zp))))
    ;; isWhitespace: a space separator that is not non-breaking, the line and
    ;; paragraph separators, and the ASCII controls \t \n \x0B \f \r \x1C-\x1F.
    ("javaWhitespace" . ,(lambda (c) (let ((n (char->integer c)))
                                       (or (and (memq (char-general-category c) '(Zs Zl Zp))
                                                (not (memv n '(#xA0 #x2007 #x202F))))
                                           (and (>= n 9) (<= n 13))
                                           (and (>= n #x1C) (<= n #x1F))))))
    ("javaISOControl" . ,(lambda (c) (let ((n (char->integer c)))
                                       (or (<= n #x1F) (and (>= n #x7F) (<= n #x9F))))))))

;; name -> built SRE, once per name (the walk over the codepoint space is the
;; cost, so a class is built the first time a pattern names it and kept). The
;; regex cache's lock already serializes the translator's callers; the table
;; takes its own so a direct caller (a gate, a REPL) is safe too — a Chez
;; hashtable written from two threads faults in the collector.
(define unicode-property-cache (make-hashtable string-hash string=?))
(define unicode-property-mutex (make-mutex 'unicode-properties))
(define (unicode-property-sre name in?)
  (or (hashtable-ref unicode-property-cache name #f)
      (jolt-with-mutex unicode-property-mutex
        (or (hashtable-ref unicode-property-cache name #f)
            (let ((sre (unicode-property-ranges in?)))
              (hashtable-set! unicode-property-cache name sre)
              sre)))))

;; The SRE for a \p{name}, or #f for a name jolt cannot build. The spellings
;; the JVM accepts for a category — L, IsL, gc=L, general_category=L — all
;; reach the same table; a binary property is Is<Property>.
(define (prop-class-sre name)
  (define (category-sre nm)
    (let ((e (assoc nm unicode-category-groups)))
      (and e (unicode-property-sre nm (lambda (c) (memq (char-general-category c) (cdr e)))))))
  (define (prefixed? pre) (and (> (string-length name) (string-length pre))
                               (string=? (substring name 0 (string-length pre)) pre)))
  (define (after pre) (substring name (string-length pre) (string-length name)))
  (cond
   ;; Pattern.family() reads the key/value split first, before any plain name —
   ;; and no plain name holds an "=", so nothing else can be reached through it.
   ((property-key-split name)
    => (lambda (kv)
         (and (member (car kv) '("gc" "general_category"))
              (category-sre (cdr kv)))))
   ;; The Unicode separator categories are a short fixed list, so spell them out
   ;; rather than settling for irregex's ASCII `blank`. Zs is the space separators
   ;; (the non-breaking ones included — \p{Z} is a category, not Java's
   ;; isWhitespace), Zl the line separator, Zp the paragraph separator.
   ((string=? name "Zs") sre-Zs)
   ((string=? name "Zl") #\x2028)
   ((string=? name "Zp") #\x2029)
   ((string=? name "Z") sre-Z)
   ((category-sre name) => values)
   ;; POSIX names: ASCII, as on the JVM
   ((string=? name "Alpha") 'alpha)
   ((string=? name "Digit") 'numeric)
   ((string=? name "Lower") 'lower)
   ((string=? name "Upper") 'upper)
   ((string=? name "ASCII") 'ascii)
   ((string=? name "Alnum") 'alphanumeric)
   ((string=? name "Punct") 'punct)
   ((string=? name "Graph") 'graph)
   ((string=? name "Print") 'print)
   ((string=? name "Blank") 'blank)
   ((string=? name "Cntrl") 'cntrl)
   ((string=? name "XDigit") 'xdigit)
   ((string=? name "Space") 'whitespace)
   ((string=? name "all") 'any)
   ((assoc name java-character-predicates)
    => (lambda (e) (unicode-property-sre name (cdr e))))
   ((prefixed? "Is")
    (let ((nm (after "Is")))
      (cond ((assoc (string-upcase nm) unicode-binary-properties)
             => (lambda (e) (unicode-property-sre name (cdr e))))
            (else (prop-class-sre nm)))))
   (else #f)))

;; ── Literal string → SRE ──────────────────────────────────────────────────────

(define (make-lit s)
  (let ((len (string-length s)))
    (cond ((= len 0) 'epsilon)
          ((= len 1) (string-ref s 0))
          (else `(seq ,@(map (lambda (i) (string-ref s i)) (iota len)))))))

;; ── Entry point ───────────────────────────────────────────────────────────────


;; ── JDK-faithful pattern-syntax validator ─────────────────────────────────────
;; Java rejects patterns the SRE translation would otherwise quietly accept: a
;; quantifier with no atom, a malformed {...}, an unfinished group, a bad
;; \p / \x / \N / \k escape.  This mirrors java.util.regex.Pattern's own scan
;; (group0 / sequence / atom / closure / clazz / range / escape) so the reported
;; description and index match the JVM (host/chez/regex.ss renders them as
;; "<desc> near index <n>" plus the caret line).  It answers only "what would
;; the JVM say about this text"; whether jolt can BUILD what it accepts is the
;; translator's question, asked next.
;;
;; The JVM's indexes are cursor arithmetic (error() reports cursor - 1), and a
;; scan that has run off the end sits one past the sentinel — which is why an
;; unclosed group after a trailing backslash is "near index n+1" while a plain
;; unclosed group is "near index n".  Every such quirk here was captured from a
;; JDK run (test/chez/regex-syntax-test.ss), not derived.

;; Pattern.RemoveQEQuoting, exactly: the JVM rewrites every \Q…\E span BEFORE it
;; parses, and the index its PatternSyntaxException names is an index into that
;; rewritten buffer, not the source. A quoted ASCII letter or non-ASCII unit is
;; copied; a quoted digit is copied too, with a \x3 prefix when it opens the
;; quote (so a \u escape before the \Q cannot absorb it); any other quoted unit
;; is backslash-escaped, a quoted backslash doubled. A quote with no \E runs to
;; the end. Everything before the first \Q, and everything outside a quote after
;; it, is copied verbatim — an \E outside a quote stays for the scanner to reject.
(define (jsc-qe-rewrite s)
  (let* ((n (string-length s))
         (start (let loop ((i 0))
                  (cond ((>= i (- n 1)) #f)
                        ((not (char=? (string-ref s i) #\\)) (loop (+ i 1)))
                        ((char=? (string-ref s (+ i 1)) #\Q) i)
                        (else (loop (+ i 2)))))))
    (if (not start)
        s
        (let loop ((i (+ start 2)) (in-quote #t) (begin-quote #t)
                   (acc (reverse (string->list (substring s 0 start)))))
          (if (>= i n)
              (list->string (reverse acc))
              (let ((c (string-ref s i)) (i (+ i 1)))
                (cond
                  ((or (> (char->integer c) 127) (jsc-latin-letter? c))
                   (loop i in-quote #f (cons c acc)))
                  ((jsc-digit? c)
                   (loop i in-quote #f
                         (cons c (if begin-quote (append '(#\3 #\x #\\) acc) acc))))
                  ((not (char=? c #\\))
                   (loop i in-quote #f (cons c (if in-quote (cons #\\ acc) acc))))
                  (in-quote
                   (if (and (< i n) (char=? (string-ref s i) #\E))
                       (loop (+ i 1) #f #f acc)
                       (loop i #t #f (cons #\\ (cons #\\ acc)))))
                  ((and (< i n) (char=? (string-ref s i) #\Q))
                   (loop (+ i 1) #t #t acc))
                  (else
                   (loop (if (< i n) (+ i 1) i) #f #f
                         (if (< i n) (cons (string-ref s i) (cons c acc)) (cons c acc)))))))))))

(define (jsc-latin-letter? c)
  (or (and (char>=? c #\A) (char<=? c #\Z))
      (and (char>=? c #\a) (char<=? c #\z))))

(define (jsc-digit? c)
  (and (char>=? c #\0) (char<=? c #\9)))

(define (jsc-latin-char? c)
  (or (jsc-latin-letter? c) (jsc-digit? c)))

(define (jsc-octal? c)
  (and (char>=? c #\0) (char<=? c #\7)))

;; The keyless \p{…} names the JVM knows: the general categories and their
;; groups, the POSIX and java* names, `all`, and the Is (script, binary property
;; or category) and In (block) spellings with a non-empty argument.  The keyed
;; gc= / sc= / blk= spellings go through property-key-split below instead.
(define jvm-property-names
  '("Lower" "Upper" "ASCII" "Alpha" "Digit" "Alnum" "Punct" "Graph" "Print"
    "Blank" "Cntrl" "XDigit" "Space" "all"
    "javaLowerCase" "javaUpperCase" "javaAlphabetic" "javaIdeographic"
    "javaTitleCase" "javaDigit" "javaDefined" "javaLetter" "javaLetterOrDigit"
    "javaJavaIdentifierStart" "javaJavaIdentifierPart"
    "javaUnicodeIdentifierStart" "javaUnicodeIdentifierPart"
    "javaIdentifierIgnorable" "javaSpaceChar" "javaWhitespace" "javaISOControl"
    "javaMirrored"
    "C" "Cc" "Cf" "Cn" "Co" "Cs" "L" "LC" "Ll" "Lm" "Lo" "Lt" "Lu" "M" "Mc" "Me"
    "Mn" "N" "Nd" "Nl" "No" "P" "Pc" "Pd" "Pe" "Pf" "Pi" "Po" "Ps" "S" "Sc" "Sk"
    "Sm" "So" "Z" "Zl" "Zp" "Zs"))

(define (jvm-property-name? name)
  (define (prefixed? pre)
    (and (> (string-length name) (string-length pre))
         (string=? (substring name 0 (string-length pre)) pre)))
  (or (and (member name jvm-property-names) #t)
      (prefixed? "Is") (prefixed? "In")))

;; Pattern.family() splits a braced \p{…} at the FIRST "=" before it looks at
;; anything else: what is left of it is a key, lowercased, and what is right of
;; it a value, kept as written.  Answers (key . value), or #f when the text holds
;; no "=" and is a plain property name.  One splitter for the validator and the
;; translator both — they used to spell the keys out separately, and
;; case-sensitively, so \p{GC=Lu} was rejected here and compiles on the JVM.
(define (rx-downcase s)
  (let* ((n (string-length s)) (out (make-string n)))
    (let loop ((i 0))
      (if (>= i n)
          out
          (begin (string-set! out i (char-downcase (string-ref s i)))
                 (loop (+ i 1)))))))

(define (property-key-split name)
  (let ((eq (str-scan-char name #\= 0 (string-length name))))
    (and eq (cons (rx-downcase (substring name 0 eq))
                  (substring name (+ eq 1) (string-length name))))))

;; The general category names \p{gc=…} takes, spelled as the JVM spells them —
;; it matches the value case-sensitively even though the key is case-insensitive.
(define unicode-gc-value-names
  '("C" "Cc" "Cf" "Cn" "Co" "Cs" "L" "LC" "Ll" "Lm" "Lo" "Lt" "Lu" "M" "Mc" "Me"
    "Mn" "N" "Nd" "Nl" "No" "P" "Pc" "Pd" "Pe" "Pf" "Pi" "Po" "Ps" "S" "Sc" "Sk"
    "Sm" "So" "Z" "Zl" "Zp" "Zs"))

;; #f when the JVM accepts this \p{…} text, otherwise the description it rejects
;; with.  A keyed spec gets a sentence of its own, naming the two halves
;; separately — jolt printed the keyless "Unknown character property name {…}"
;; for both, so \p{FOO=BAR} reported something the JVM never says.  A script or
;; block VALUE is not checked: jolt has no table to check it against, and the
;; translator refuses the ones it cannot build.
(define (jvm-property-reject name)
  (let ((kv (property-key-split name)))
    (if kv
        (and (not (if (member (car kv) '("gc" "general_category"))
                      (member (cdr kv) unicode-gc-value-names)
                      (member (car kv) '("sc" "script" "blk" "block"))))
             (string-append "Unknown Unicode property {name=<" (car kv)
                            ">, value=<" (cdr kv) ">}"))
        (and (not (jvm-property-name? name))
             (string-append "Unknown character property name {" name "}")))))

(define (java-syntax-check source)
  (let*-values (((qe) (jsc-qe-rewrite source))
                ((s index-map) (x-strip qe)))
    (java-syntax-check-stripped qe s index-map)))

;; qe is the \Q-rewritten pattern, s that text with COMMENTS whitespace stripped
;; out of it, index-map s's positions back in qe's terms.  java-pattern->sre
;; hands its own copies of all three straight in, so the text checked here and
;; the text parsed there are one object and cannot disagree.
(define (java-syntax-check-stripped qe s index-map)
   (let ((n (string-length s)))
    (define (rf k) (string-ref s k))
    ;; An index is a position in s; in COMMENTS mode it is mapped back to the
    ;; unstripped pattern. The JVM's end-relative indexes (n-1, n+1) and its
    ;; "the unit before" (Unmatched closing) are an offset from a mapped
    ;; position, so they land on a stripped unit when that is what is there.
    (define (err desc idx . delta)
      (let ((d (if (null? delta) 0 (car delta))))
        (java-re-error desc
                       (+ d (cond ((not index-map) idx)
                                  ((<= idx n) (vector-ref index-map idx))
                                  (else (+ (string-length qe) (- idx n))))))))
    ;; The JVM reports cursor - 1, and some of its errors are raised after a
    ;; next() that — with COMMENTS in force — has already skipped the whitespace
    ;; and #-comments following the unit that caused them. The index then names
    ;; the unit AFTER that run rather than the offending one: (?x)* is a dangling
    ;; '*' near index 4 and (?x)* #c\n is the same '*' near index 8. So take the
    ;; cursor's own position, which is the next kept unit, and step back one.
    ;; Without stripping the two readings coincide, which is why only a (?x)
    ;; pattern ever showed the difference.
    (define (err/past desc idx) (err desc (+ idx 1) -1))

    ;; Did stripping take a run off the END of the pattern?  Pattern's reads run
    ;; one past their own sentinel when it did, and more than one of its errors
    ;; turns on that.
    (define (trailing-run?)
      (and index-map (> n 0)
           (> (vector-ref index-map n) (+ 1 (vector-ref index-map (- n 1))))))

    (define seen '())                   ; the named groups defined so far
    (define refs 0)                     ; back-references read so far

    ;; ── escapes ──
    ;; Each reads one escape and answers (values kind code next): kind is char
    ;; (code = the code point, or #f for a \N{…} whose value jolt cannot look
    ;; up), class (\d \p{…} …), atom (an assertion or \R, quantifiable like the
    ;; JVM allows), or ref.  A class can be quantified but not end a range.

    ;; \0: one to three octal digits, the third only when the first is 0-3.
    (define (octal i)                   ; i = the first digit
      (if (or (>= i n) (not (jsc-octal? (rf i))))
          (err "Illegal octal escape sequence" i)
          (let ((d1 (- (char->integer (rf i)) 48)))
            (if (and (< (+ i 1) n) (jsc-octal? (rf (+ i 1))))
                (let ((d2 (- (char->integer (rf (+ i 1))) 48)))
                  (if (and (<= d1 3) (< (+ i 2) n) (jsc-octal? (rf (+ i 2))))
                      (values (+ (* d1 64) (* d2 8) (- (char->integer (rf (+ i 2))) 48)) (+ i 3))
                      (values (+ (* d1 8) d2) (+ i 2))))
                (values d1 (+ i 1))))))

    ;; \x: two hex digits, or {hex+} at most 0x10FFFF (Pattern.x()).
    (define (hex i)                     ; i = after the x
      (cond
        ((and (< i n) (hex-value (rf i)))
         (if (and (< (+ i 1) n) (hex-value (rf (+ i 1))))
             (values (+ (* 16 (hex-value (rf i))) (hex-value (rf (+ i 1)))) (+ i 2))
             (err "Illegal hexadecimal escape sequence" (+ i 1))))
        ((and (< i n) (char=? (rf i) #\{) (< (+ i 1) n) (hex-value (rf (+ i 1))))
         (let loop ((k (+ i 1)) (v 0))
           (cond ((>= k n) (err "Unclosed hexadecimal escape sequence" k))
                 ((hex-value (rf k))
                  (let ((v (+ (* v 16) (hex-value (rf k)))))
                    (if (> v #x10FFFF)
                        (err "Hexadecimal codepoint is too big" k)
                        (loop (+ k 1) v))))
                 ((char=? (rf k) #\}) (values v (+ k 1)))
                 (else (err "Unclosed hexadecimal escape sequence" k)))))
        ;; Pattern.x() PEEKS the first unit raw and only starts READING — which
        ;; is what skips — once it has seen a "{", so a malformed \x{…} reports
        ;; from past the run after the brace and a malformed \x41 does not.
        ((and (< i n) (char=? (rf i) #\{))
         (err/past "Illegal hexadecimal escape sequence" i))
        (else (err "Illegal hexadecimal escape sequence" i))))

    ;; \u: exactly four hex digits (Pattern.uxxxx()) …
    (define (uxxxx i)                   ; i = after the u
      (let loop ((k i) (v 0))
        (cond ((>= k (+ i 4)) (values v k))
              ((and (< k n) (hex-value (rf k)))
               (loop (+ k 1) (+ (* v 16) (hex-value (rf k)))))
              (else (err "Illegal Unicode escape sequence" k)))))
    ;; … and a high surrogate followed by \u + a low one is a single code point
    ;; (Pattern.u()); the second escape is read, and so checked, either way.
    (define (unicode i)
      (let-values (((v k) (uxxxx i)))
        (if (and (<= #xD800 v) (<= v #xDBFF)
                 (< (+ k 1) n) (char=? (rf k) #\\) (char=? (rf (+ k 1)) #\u))
            (let-values (((v2 k2) (uxxxx (+ k 2))))
              (if (and (<= #xDC00 v2) (<= v2 #xDFFF))
                  (values (+ #x10000 (* (- v #xD800) #x400) (- v2 #xDC00)) k2)
                  (values v k)))
            (values v k))))

    ;; \N{name}: the braces are checked here, and a name no character can have
    ;; (Character.codePointOf takes letters, digits, space and hyphen, any case,
    ;; and every name has a letter) is refused as the JVM refuses it; whether a
    ;; well-formed name EXISTS is the translator's answer, which is that it has
    ;; no table to say.
    (define (charname i)                ; i = after the N → next
      (if (or (>= i n) (not (char=? (rf i) #\{)))
          (err "Illegal character name escape sequence" i)
          (let ((cl (str-scan-char s #\} (+ i 1) n)))
            (cond
              ((not cl)
               ;; Pattern.N() reads towards the "}" with read(), which skips a
               ;; COMMENTS run, and then reports cursor - 1. With no "}" at all
               ;; the cursor has stopped either ON the last unit it read — the
               ;; final kept unit of the name — or past the sentinel, which is
               ;; where an empty name and a trailing stripped run both leave it.
               (if (or (= (+ i 1) n) (trailing-run?))
                   (err "Unclosed character name escape sequence" n)
                   (err "Unclosed character name escape sequence" (- n 1))))
              ((let ok ((k (+ i 1)) (letter #f))
                 (if (>= k cl)
                     letter
                     (and (or (jsc-latin-char? (rf k)) (memv (rf k) '(#\space #\-)))
                          (ok (+ k 1) (or letter (jsc-latin-letter? (rf k)))))))
               (+ cl 1))
              (else
               (err (string-append "Unknown character name [" (substring s (+ i 1) cl) "]") cl))))))

    ;; \p{name} / \pL: the shape, and that the JVM knows the name.
    (define (property i)                ; i = after the p/P → next
      (cond
        ((>= i n)
         (err (string-append "Unknown character property name {"
                             (string (integer->char 0)) "}")
              i))
        ((char=? (rf i) #\{)
         (let ((cl (str-scan-char s #\} (+ i 1) n)))
           (cond ((not cl) (err "Unclosed character family" n))
                 ((= cl (+ i 1)) (err "Empty character family" cl))
                 ((jvm-property-reject (substring s (+ i 1) cl))
                  => (lambda (desc) (err desc cl)))
                 (else (+ cl 1)))))
        ((jvm-property-reject (string (rf i))) => (lambda (desc) (err desc i)))
        (else (+ i 1))))

    ;; <name> of a named group or a \k back-reference: a Latin letter, then
    ;; Latin letters and digits, then > → (values name next)
    (define (groupname j)
      (if (or (>= j n) (not (jsc-latin-letter? (rf j))))
          (err "capturing group name does not start with a Latin letter" j)
          (let loop ((k (+ j 1)))
            (cond ((>= k n) (err "named capturing group is missing trailing '>'" n))
                  ((char=? (rf k) #\>) (values (substring s j k) (+ k 1)))
                  ((jsc-latin-char? (rf k)) (loop (+ k 1)))
                  (else (err "named capturing group is missing trailing '>'" k))))))

    (define (backref i)                 ; i = after the k → next
      (if (or (>= i n) (not (char=? (rf i) #\<)))
          (err "\\k is not followed by '<' for named capturing group" i)
          (let-values (((nm k) (groupname (+ i 1))))
            (if (member nm seen)
                k
                (err (string-append "named capturing group <" nm "> does not exist")
                     (- k 1))))))

    ;; One escape at i (the backslash). where is top, group or class: a trailing
    ;; backslash reads the JVM's end sentinel as a literal, and what then fails
    ;; is the enclosing construct.
    (define (escape i where)
      (if (>= (+ i 1) n)
          (case where
            ((class) (err "Unclosed character class" n))
            ((top) (err "Unescaped trailing backslash" n))
            (else (err "Unclosed group" n 1)))
          (let ((c (rf (+ i 1))) (inclass (eq? where 'class)))
            (define (illegal) (err "Illegal/unsupported escape sequence" (+ i 1)))
            (define (char v k) (values 'char v k))
            (case c
              ((#\0) (let-values (((v k) (octal (+ i 2)))) (char v k)))
              ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (if inclass (illegal) (begin (set! refs (+ refs 1)) (values 'ref #f (+ i 2)))))
              ((#\a) (char 7 (+ i 2)))
              ((#\e) (char 27 (+ i 2)))
              ((#\f) (char 12 (+ i 2)))
              ((#\n) (char 10 (+ i 2)))
              ((#\r) (char 13 (+ i 2)))
              ((#\t) (char 9 (+ i 2)))
              ((#\c)
               ;; Pattern.c() tests cursor < patternLength against the
               ;; UNSTRIPPED buffer and then read()s, which with COMMENTS in
               ;; force skips the run and hands back the sentinel.  So a \c with
               ;; a stripped run after it and nothing else does NOT fail here:
               ;; the JVM takes NUL as the control letter and carries on past
               ;; its own end, where an open group is "Unclosed group", an open
               ;; class is "Unclosed character class", and at the top level the
               ;; read past the sentinel is the JDK's own "Unexpected internal
               ;; error".  (?x)\c a and (?x)\c ) are the same reading, and both
               ;; compile — the letter is simply the next unit that survives.
               (cond
                 ((< (+ i 2) n)
                  (char (bitwise-xor (char->integer (rf (+ i 2))) 64) (+ i 3)))
                 ((trailing-run?)
                  (case where
                    ((class) (err "Unclosed character class" n))
                    ((top) (err "Unexpected internal error" n))
                    (else (err "Unclosed group" n 1))))
                 (else (err "Illegal control escape sequence" (+ i 1)))))
              ((#\x) (let-values (((v k) (hex (+ i 2)))) (char v k)))
              ((#\u) (let-values (((v k) (unicode (+ i 2)))) (char v k)))
              ((#\N) (char #f (charname (+ i 2))))
              ((#\p #\P) (values 'class #f (property (+ i 2))))
              ((#\d #\D #\s #\S #\w #\W #\h #\H #\v #\V) (values 'class #f (+ i 2)))
              ((#\b #\B #\A #\G #\z #\Z #\R #\X)
               (if inclass (illegal) (values 'atom #f (+ i 2))))
              ((#\k) (if inclass (illegal)
                         (begin (set! refs (+ refs 1)) (values 'ref #f (backref (+ i 2))))))
              ((#\Q #\E) (illegal))
              (else
               (if (jsc-latin-char? c)
                   (illegal)
                   (char (char->integer c) (+ i 2))))))))

    ;; ── character classes (Pattern.clazz / range) ──
    ;; One class member that is not [, && or ] → (values kind code next)
    (define (element j)
      (if (char=? (rf j) #\\)
          (escape j 'class)
          (values 'char (char->integer (rf j)) (+ j 1))))

    ;; [ at i → the index after its ]. `members` counts what the class holds
    ;; so far: a ] closes only a non-empty class, otherwise it is a member, and
    ;; an && with nothing on either side is the JVM's "Bad class syntax".
    (define (cclass i)
      (let* ((j (+ i 1))
             (j (if (and (< j n) (char=? (rf j) #\^)) (+ j 1) j)))
        (let loop ((j j) (members 0))
          (cond
            ((>= j n) (err "Unclosed character class" n -1))
            ((char=? (rf j) #\[) (loop (cclass j) (+ members 1)))
            ((and (char=? (rf j) #\&) (< (+ j 1) n) (char=? (rf (+ j 1)) #\&))
             (if (and (= members 0)
                      (or (>= (+ j 2) n) (memv (rf (+ j 2)) '(#\] #\&))))
                 (err "Bad class syntax" (+ j 1))
                 (loop (+ j 2) members)))
            ((char=? (rf j) #\])
             (if (> members 0) (+ j 1) (loop (+ j 1) 1)))
            (else
             (let-values (((kind code k) (element j)))
               (if (and (eq? kind 'char) (< k n) (char=? (rf k) #\-))
                   ;; a single unit followed by - : a range, unless the - is
                   ;; itself last, or before ] or a nested [ (then both are
                   ;; literal)
                   (cond
                     ((>= (+ k 1) n) (err "Illegal character range" n))
                     ((memv (rf (+ k 1)) '(#\] #\[)) (loop k (+ members 1)))
                     ((and (char=? (rf (+ k 1)) #\\) (< (+ k 2) n)
                           (memv (rf (+ k 2)) '(#\p #\P)))
                      ;; a \p cannot end a range; the JVM's escape() refuses it
                      (err "Illegal/unsupported escape sequence" (+ k 2)))
                     (else
                      (let-values (((kind2 code2 k2) (element (+ k 1))))
                        (if (or (not (eq? kind2 'char))
                                (and code code2 (< code2 code)))
                            (err "Illegal character range" (- k2 1))
                            (loop k2 (+ members 1))))))
                   (loop k (+ members 1)))))))))

    ;; ── can the JVM put a maximum length on this look-behind body? ──
    ;;
    ;; Not the same question as "is it bounded": JDK 21 measures (?<=a*) and
    ;; (?<=a{2,}) without complaint.  What defeats it is an unbounded LAZY or
    ;; POSSESSIVE repetition with something else ahead of it in the same
    ;; sequence — (?<!ab*+) is refused where (?<!a*+b) and (?<!ab*) both
    ;; compile, and (?<!ab{2,3}+) does too because that one is bounded.
    ;;
    ;; What counts as "ahead of it" is Java's own study(): zero-width things
    ;; contribute nothing (^, $, \b, a flag group, a look-around), and a group
    ;; holding a top-level | is a Branch, whose study() resets the TreeInfo
    ;; that Curly.study() then overflows — so such a group neither carries what
    ;; precedes it into its branches nor passes anything out: (?<!a(b*+|c)) and
    ;; (?<!a(b|c)d*+) compile, (?<!a(b*+)) does not.
    ;;
    ;; Captured from JDK 21 over 852 look-behind shapes, decision and index
    ;; both.  jolt used to ask only whether the body held a back-reference, so
    ;; it accepted every one of these.
    (define (lb-unmeasurable? start stop)
      (define (skip-class i)
        (let lp ((i (+ i 1)))
          (cond ((>= i stop) stop)
                ((char=? (rf i) #\\) (lp (+ i 2)))
                ((char=? (rf i) #\]) (+ i 1))
                (else (lp (+ i 1))))))
      (define (top-bar? i)              ; a | of this group's own, not a nested one
        (let lp ((i i) (d 0))
          (cond ((>= i stop) #f)
                ((char=? (rf i) #\\) (lp (+ i 2) d))
                ((char=? (rf i) #\[) (lp (skip-class i) d))
                ((char=? (rf i) #\() (lp (+ i 1) (+ d 1)))
                ((char=? (rf i) #\)) (and (> d 0) (lp (+ i 1) (- d 1))))
                ((and (char=? (rf i) #\|) (= d 0)) #t)
                (else (lp (+ i 1) d)))))
      ;; the quantifier at i, if any → (values next unbounded? lazy-or-possessive?)
      (define (marked j unbounded?)
        (if (and (< j stop) (memv (rf j) '(#\? #\+)))
            (values (+ j 1) unbounded? #t)
            (values j unbounded? #f)))
      (define (quantifier i)
        (if (>= i stop)
            (values i #f #f)
            (let ((c (rf i)))
              (cond
                ((memv c '(#\* #\+)) (marked (+ i 1) #t))
                ((char=? c #\?) (marked (+ i 1) #f))
                ((char=? c #\{)
                 (let ((cl (str-scan-char s #\} i stop)))
                   (if (or (not cl) (= cl i))
                       (values i #f #f)
                       (marked (+ cl 1) (char=? (rf (- cl 1)) #\,)))))
                (else (values i #f #f))))))
      (let loop ((i start) (seen #f) (stack '()))
        (and (< i stop)
             (let ((c (rf i)))
               (cond
                 ((char=? c #\\)
                  (let* ((e (and (< (+ i 1) stop) (rf (+ i 1))))
                         (zero (and e (memv e '(#\b #\B #\A #\z #\Z #\G))))
                         ;; a braced or angled argument belongs to the escape and
                         ;; is not a bound: \p{Sc}{2,}+ is ONE atom possessively
                         ;; repeated, and reading {Sc} as the repetition made the
                         ;; {2,}+ after it invisible.
                         (after
                          (cond
                            ((and (memv e '(#\p #\P #\x #\N))
                                  (< (+ i 2) stop) (char=? (rf (+ i 2)) #\{))
                             (let ((cl (str-scan-char s #\} (+ i 3) stop)))
                               (if cl (+ cl 1) stop)))
                            ((and (eqv? e #\k) (< (+ i 2) stop) (char=? (rf (+ i 2)) #\<))
                             (let ((gt (str-scan-char s #\> (+ i 3) stop)))
                               (if gt (+ gt 1) stop)))
                            ;; and so does a run of digits: \x41*+ is one atom
                            ;; repeated, not \x then 4 then 1 then the repeat.
                            ((eqv? e #\c) (min stop (+ i 3)))
                            ((eqv? e #\x) (min stop (+ i 4)))
                            ((eqv? e #\u) (min stop (+ i 6)))
                            ((eqv? e #\0)
                             (let lp ((k (+ i 2)) (d 0))
                               (if (and (< k stop) (< d 3) (jsc-octal? (rf k)))
                                   (lp (+ k 1) (+ d 1))
                                   k)))
                            ((jsc-digit? e)
                             (let lp ((k (+ i 2)))
                               (if (and (< k stop) (jsc-digit? (rf k))) (lp (+ k 1)) k)))
                            (else (+ i 2)))))
                    (let-values (((j unb mk) (quantifier after)))
                      (if (and unb mk seen)
                          #t
                          (loop j (or seen (not zero)) stack)))))
                 ((char=? c #\[)
                  (let-values (((j unb mk) (quantifier (skip-class i))))
                    (if (and unb mk seen) #t (loop j #t stack))))
                 ((memv c '(#\^ #\$)) (loop (+ i 1) seen stack))
                 ((char=? c #\|) (loop (+ i 1) #f stack))
                 ((char=? c #\))
                  (and (pair? stack)
                       (let ((outer (caar stack)) (zero (cadar stack)) (bar (caddar stack)))
                         (let-values (((j unb mk) (quantifier (+ i 1))))
                           (if (and unb mk outer)
                               #t
                               (loop j
                                     (if bar #f (or outer (not zero)))
                                     (cdr stack)))))))
                 ((char=? c #\()
                  (let ((g (parse-flag-group s i stop)))
                    (cond
                      ;; (?flags) is not a node at all
                      ((and g (not (flag-group-scoped? g))) (loop (flag-group-end g) seen stack))
                      ;; (?#…) is a comment
                      ((and (< (+ i 2) stop) (char=? (rf (+ i 1)) #\?) (char=? (rf (+ i 2)) #\#))
                       (let ((cl (str-scan-char s #\) (+ i 3) stop)))
                         (loop (if cl (+ cl 1) stop) seen stack)))
                      (else
                       (let* ((look (and (< (+ i 2) stop) (char=? (rf (+ i 1)) #\?)
                                         (or (memv (rf (+ i 2)) '(#\= #\!))
                                             (and (char=? (rf (+ i 2)) #\<)
                                                  (< (+ i 3) stop)
                                                  (memv (rf (+ i 3)) '(#\= #\!))))))
                              (body (cond (g (flag-group-end g))
                                          ((not (and (< (+ i 1) stop) (char=? (rf (+ i 1)) #\?)))
                                           (+ i 1))
                                          ((and (char=? (rf (+ i 2)) #\<)
                                                (not look))
                                           (let ((gt (str-scan-char s #\> (+ i 3) stop)))
                                             (if gt (+ gt 1) (+ i 3))))
                                          ((and (char=? (rf (+ i 2)) #\<) look) (+ i 4))
                                          (else (+ i 3))))
                              (bar (top-bar? body)))
                         (loop body
                               (if (or look bar) #f seen)
                               (cons (list seen look bar) stack)))))))
                 (else
                  (let-values (((j unb mk) (quantifier (+ i 1))))
                    (if (and unb mk seen) #t (loop j #t stack)))))))))

    ;; ── groups (Pattern.group0) ──
    ;; ( at i → (values next quantifiable?): a flags-only (?i) is not an atom,
    ;; so a quantifier after it dangles.
    (define (group i)
      (define (body j)
        (let ((k (scan j #f)))
          (if (>= k n) (err "Unclosed group" n) (values (+ k 1) #t))))
      (cond
        ((>= (+ i 1) n) (err "Unclosed group" n))
        ((not (char=? (rf (+ i 1)) #\?)) (body (+ i 1)))
        ((>= (+ i 2) n) (err "Unknown inline modifier" n))
        (else
         (let ((c2 (rf (+ i 2))))
           (case c2
             ((#\: #\= #\! #\>) (body (+ i 3)))
             ((#\<)
              (if (and (< (+ i 3) n) (memv (rf (+ i 3)) '(#\= #\!)))
                  ;; a look-behind body must have a measurable maximum length;
                  ;; a back-reference defeats it, and so does lb-unmeasurable?
                  ;; above. The check runs BEFORE the group is known to be
                  ;; closed, which is why (?<!ab*+ names the look-behind and
                  ;; (?<!a{2,}+ names the unclosed group.
                  (let* ((before refs) (k (scan (+ i 4) #f)))
                    (cond ((or (> refs before) (lb-unmeasurable? (+ i 4) k))
                           (err "Look-behind group does not have an obvious maximum length" k -1))
                          ((>= k n) (err "Unclosed group" n))
                          (else (values (+ k 1) #t))))
                  (let-values (((nm k) (groupname (+ i 3))))
                    (if (member nm seen)
                        (err (string-append "Named capturing group <" nm "> is already defined")
                             (- k 1))
                        (begin (set! seen (cons nm seen))
                               (body k))))))
             ((#\$ #\@) (err "Unknown group type" (+ i 2)))
             (else (flags (+ i 2))))))))

    ;; (?flags) or (?flags:body), j = the first flag. One - switches to the
    ;; flags being turned off; a second is not a flag.
    (define (flags j)
      (let loop ((k j) (minus #f))
        (cond ((>= k n) (err "Unknown inline modifier" n))
              ((memv (rf k) '(#\i #\m #\s #\d #\u #\x #\U #\c)) (loop (+ k 1) minus))
              ((and (char=? (rf k) #\-) (not minus)) (loop (+ k 1) #t))
              ((char=? (rf k) #\)) (values (+ k 1) #f))
              ((char=? (rf k) #\:)
               (let ((m (scan (+ k 1) #f)))
                 (if (>= m n) (err "Unclosed group" n) (values (+ m 1) #t))))
              (else (err "Unknown inline modifier" k)))))

    ;; {n}, {n,}, {n,m} at i → the index after the }
    (define (brace i)
      (let ((j (+ i 1)))
        (if (or (>= j n) (not (jsc-digit? (rf j))))
            (err "Illegal repetition" j)
            (let ((k (let loop ((k j))
                       (if (and (< k n) (jsc-digit? (rf k))) (loop (+ k 1)) k))))
              (cond
                ((>= k n) (err "Unclosed counted closure" n))
                ((char=? (rf k) #\}) (+ k 1))
                ((char=? (rf k) #\,)
                 (let ((m (let loop ((m (+ k 1)))
                            (if (and (< m n) (jsc-digit? (rf m))) (loop (+ m 1)) m))))
                   (cond
                     ((>= m n) (err "Unclosed counted closure" n))
                     ((not (char=? (rf m) #\})) (err "Unclosed counted closure" m))
                     ((and (> m (+ k 1))
                           (< (string->number (substring s (+ k 1) m))
                              (string->number (substring s j k))))
                      (err "Illegal repetition range" m))
                     (else (+ m 1)))))
                (else (err "Unclosed counted closure" k)))))))

    ;; ── sequences (Pattern.expr / sequence / closure) ──
    ;; From i to the end or, below the top, to the ) that closes the group.
    ;; `prev` says whether a quantifier has an atom to apply to.
    (define (scan i top?)
      (let loop ((i i) (prev #f))
        (if (>= i n)
            i
            (let ((c (rf i)))
              (case c
                ((#\)) (if top? (err "Unmatched closing ')'" i -1) i))
                ((#\\)
                 (let-values (((kind code k) (escape i (if top? 'top 'group))))
                   (loop k #t)))
                ((#\[) (loop (cclass i) #t))
                ((#\() (let-values (((k p) (group i))) (loop k p)))
                ((#\* #\+ #\?)
                 (if prev
                     (let ((k (+ i 1)))
                       (loop (if (and (< k n) (memv (rf k) '(#\? #\+))) (+ k 1) k) #f))
                     (err/past (string-append "Dangling meta character '" (string c) "'") i)))
                ((#\{)
                 (let ((k (brace i)))
                   (loop (if (and (< k n) (memv (rf k) '(#\? #\+))) (+ k 1) k) #f)))
                ((#\|) (loop (+ i 1) #f))
                (else (loop (+ i 1) #t)))))))

    (scan 0 #t)))

;; Pattern.RemoveQEQuoting and then COMMENTS stripping, ONCE, with the validator
;; and the parser below both reading what comes out.  They used to preprocess
;; apart: the validator rewrote \Q…\E where the parser tried to read it inline,
;; so \c\QZ(?= was accepted by the one and refused by the other with a confident
;; "Unclosed group" — \c having eaten the backslash that opened the quote.  A
;; parser that reads a different string from the one that was checked can always
;; be talked into contradicting it, so it no longer reads a different string.
(define (java-pattern->sre source)
  (let*-values (((qe) (jsc-qe-rewrite source))
                ((source index-map) (x-strip qe)))
    (java-syntax-check-stripped qe source index-map)
    (let* ((len (string-length source)))
      (let-values (((opts start) (parse-leading-flags source 0 len)))
        (let-values (((sre _end) (parse-expr source start len opts 0)))
          (values sre
                  (let lp ((opts opts) (out '()))
                    (cond ((null? opts) (reverse out))
                          ((memq (car opts) '(case-insensitive single-line multi-line))
                           (lp (cdr opts) (cons (car opts) out)))
                          (else (lp (cdr opts) out))))))))))
;; ── parse-expr: alternation level (handles |) ─────────────────────────────────

(define (parse-expr src i end flags depth)
  (let loop ((i i) (alts '()))
    (let-values (((sre i) (parse-seq src i end flags depth #f)))
      (if (and (< i end) (char=? (string-ref src i) #\|))
          (loop (+ i 1) (cons sre alts))
          (values (if (null? alts) sre `(or ,@(reverse (cons sre alts)))) i)))))

;; ── parse-seq: concatenation ──────────────────────────────────────────────────

(define (parse-seq src i end flags depth stop-at-depth)
  (let loop ((i i) (parts '()))
    (if (>= i end)
        (values (seq->sre parts) i)
        (let ((c (string-ref src i)))
          (cond
           ((and (char=? c #\)) (or (not stop-at-depth) (= depth stop-at-depth)))
            (values (seq->sre parts) i))
           ((char=? c #\|)
            (values (seq->sre parts) i))
           (else
            (let-values (((atom i) (parse-atom src i end flags depth)))
              (loop i (cons atom parts)))))))))

(define (seq->sre parts)
  (cond ((null? parts) 'epsilon)
        ((null? (cdr parts)) (car parts))
        (else `(seq ,@(reverse parts)))))

;; ── parse-atom: single atom + optional quantifier ─────────────────────────────

(define (parse-atom src i end flags depth)
  (let ((c (string-ref src i)))
    (cond
     ((char=? c #\\)  (let-values (((esc i) (parse-escape src (+ i 1) end flags)))
                         (maybe-quantifier esc src i end flags depth)))
     ((char=? c #\[)  (let-values (((cc i) (parse-cc src i end flags)))
                         (maybe-quantifier cc src i end flags depth)))
     ((char=? c #\()  (let-values (((grp i) (parse-group src i end flags depth)))
                         (maybe-quantifier grp src i end flags depth)))
     ((char=? c #\.)  (maybe-quantifier (if (jr-flag? flags 'single-line) 'any (jr-dot-sre flags))
                                        src (+ i 1) end flags depth))
     ((char=? c #\^)  (maybe-quantifier (if (jr-flag? flags 'multi-line) (jr-bol-sre flags) 'bos)
                                        src (+ i 1) end flags depth))
     ((char=? c #\$)
      (maybe-quantifier
       (if (jr-flag? flags 'multi-line) (jr-eol-sre flags) (jr-final-eol-sre flags))
       src (+ i 1) end flags depth))
      ((char=? c #\{)
       ;; A {n,m} with no preceding atom: Java still validates it and rejects
       ;; a malformed one (min > max), but treats a well-formed brace as literal.
       (let-values (((q i2) (parse-bounded c src i end flags depth)))
         (maybe-quantifier c src (+ i 1) end flags depth)))
      (else (maybe-quantifier c src (+ i 1) end flags depth)))))

;; ── Quantifiers ───────────────────────────────────────────────────────────────

(define (maybe-quantifier atom src i end flags depth)
  (if (>= i end)
      (values atom i)
      (let ((c (string-ref src i)))
        (cond
         ((char=? c #\*)
          (let ((i1 (+ i 1)))
            (cond ((and (< i1 end) (char=? (string-ref src i1) #\?))
                   (values `(*? ,atom) (+ i1 1)))
                   ((and (< i1 end) (char=? (string-ref src i1) #\+))
                    (values `(atomic (* ,atom)) (+ i1 1)))
                  (else (values `(* ,atom) i1)))))
         ((char=? c #\+)
          (let ((i1 (+ i 1)))
            (cond ((and (< i1 end) (char=? (string-ref src i1) #\?))
                   (values `(**? 1 #f ,atom) (+ i1 1)))
                  ((and (< i1 end) (char=? (string-ref src i1) #\+))
                   (values `(atomic (+ ,atom)) (+ i1 1)))
                  (else (values `(+ ,atom) i1)))))
         ((char=? c #\?)
          (let ((i1 (+ i 1)))
            (cond ((and (< i1 end) (char=? (string-ref src i1) #\?))
                   (values `(?? ,atom) (+ i1 1)))
                  ((and (< i1 end) (char=? (string-ref src i1) #\+))
                   (values `(atomic (? ,atom)) (+ i1 1)))
                  (else (values `(? ,atom) i1)))))
         ((char=? c #\{) (parse-bounded atom src i end flags depth))
         (else (values atom i))))))

(define (parse-bounded atom src i end flags depth)
  (let ((cb (str-scan-char src #\} (+ i 1) end)))
    (if (not cb)
        (values atom i)
        (let* ((body (substring src (+ i 1) cb))
               (comma (str-scan-char body #\, 0 (string-length body)))
               (n (if comma
                      (let ((s (substring body 0 comma)))
                        (if (> (string-length s) 0) (string->number s) #f))
                      (string->number body)))
               ;; {n} is EXACTLY n, so its upper bound is n — not #f, which is
               ;; irregex's "no bound" and made every {n} behave as {n,}: \d{4}
               ;; matched all of "20260729", [0-9]{2} all of "1234", and
               ;; (?:%[0-9a-f]{2})+ ran past the last percent-escape. Only a comma
               ;; opens the bound: {n,} (nothing after it) is unbounded, {n,m} is m.
               (m (if comma
                      (let ((s (substring body (+ comma 1) (string-length body))))
                        (if (> (string-length s) 0) (string->number s) #f))
                      n))
               (j (+ cb 1))
               (lazy? (and (< j end) (char=? (string-ref src j) #\?)))
               (j (if lazy? (+ j 1) j))
               (poss? (and (< j end) (char=? (string-ref src j) #\+)))
               (j (if poss? (+ j 1) j)))
          (cond
           ((not n) (values atom i))
           ((and m (< m n))
            (error 'java-pattern->sre "quantifier min greater than max" src))
           (else
            (let ((base (if lazy?
                           `(**? ,n ,(or m #f) ,atom)
                           `(** ,n ,(or m #f) ,atom))))
              (values (if poss? `(atomic ,base) base) j))))))))

;; ── Escape sequences ──────────────────────────────────────────────────────────
;;
;; An escape means the same thing in a bare pattern and inside a character class
;; unless Java says otherwise, and there are exactly three places it does:
;;   \b        a word boundary outside a class, a COMPILE ERROR inside one
;;   \R        a linebreak outside a class, an error inside one
;;   \1..\9 \k back-references, legal only outside
;; Everything else — \d \D \w \W \s \S, \p \P, \a \e \t \n \r \f, \cX, octal, \x,
;; \u — is parse-escape-shared's job, and both callers fall through to it.
;;
;; This used to be two hand-kept copies and they HAD drifted: \a was in the
;; bare-pattern copy only, so [\a] matched the letter a instead of BEL, and \c
;; was in neither, so \cA matched the letter c. Add an escape here, not there,
;; and both contexts get it.
;;
;; A miss answers (values #f i) — the caller decides what an unknown escape is.
;; No real escape value is #f (they are chars, symbols and lists), so #f is an
;; unambiguous sentinel.
(define (parse-uxxxx src i end)
  (and (< (+ i 3) end)
       (hex-value (string-ref src i)) (hex-value (string-ref src (+ i 1)))
       (hex-value (string-ref src (+ i 2))) (hex-value (string-ref src (+ i 3)))
       (+ (* 4096 (hex-value (string-ref src i)))
          (* 256 (hex-value (string-ref src (+ i 1))))
          (* 16 (hex-value (string-ref src (+ i 2))))
          (hex-value (string-ref src (+ i 3))))))

;; Java's \h and \v (JDK 8+): horizontal whitespace is
;; [ \t\xA0\u1680\u180e\u2000-\u200a\u202f\u205f\u3000], vertical is
;; [\n\x0B\f\r\x85\u2028\u2029]. Both used to fall through to the literal
;; letter.
(define horizontal-ws-sre
  '(or #\space #\tab #\xA0 #\x1680 #\x180E (/ #\x2000 #\x200A) #\x202F #\x205F #\x3000))
(define vertical-ws-sre
  `(or #\newline ,(integer->char #x0B) ,(integer->char #x0C) #\return
       ,(integer->char #x85) ,(integer->char #x2028) ,(integer->char #x2029)))

(define (parse-escape-shared src i end flags)
  (let ((c (string-ref src i)))
    (case c
      ((#\d) (values 'numeric (+ i 1)))
      ((#\D) (values '(~ numeric) (+ i 1)))
      ((#\w) (values '(or alphanumeric #\_) (+ i 1)))
      ((#\W) (values '(~ (or alphanumeric #\_)) (+ i 1)))
      ((#\s) (values 'whitespace (+ i 1)))
      ((#\S) (values '(~ whitespace) (+ i 1)))
      ((#\h) (values horizontal-ws-sre (+ i 1)))
      ((#\H) (values `(~ ,horizontal-ws-sre) (+ i 1)))
      ((#\v) (values vertical-ws-sre (+ i 1)))
      ((#\V) (values `(~ ,vertical-ws-sre) (+ i 1)))
      ((#\p #\P) (parse-prop c src i end))
      ;; Two escapes the JVM compiles and jolt cannot build, refused as syntax
      ;; errors rather than matched as the literal letter (which is what an
      ;; unknown escape falls to below): \N{NAME} needs the Unicode name table,
      ;; \X the grapheme-cluster rules. known-divergences.edn carries both.
      ((#\N)
       (java-re-error "\\N{name} is unsupported on jolt (no Unicode character name table)" i))
      ((#\X)
       (java-re-error "\\X (extended grapheme cluster) is unsupported on jolt" i))
      ((#\e) (values (integer->char #x1B) (+ i 1)))
      ((#\t) (values #\tab (+ i 1)))
      ((#\n) (values #\newline (+ i 1)))
      ((#\r) (values #\return (+ i 1)))
      ((#\f) (values (integer->char #x0C) (+ i 1)))
      ((#\a) (values (integer->char #x07) (+ i 1)))
      ;; \cX is X xor 64 on the RAW next character — no case folding first, so
      ;; \ca is 97 xor 64 = 33, not \cA's 1. A dangling \c is a pattern error.
      ((#\c)
       (if (>= (+ i 1) end)
           (error 'java-pattern->sre "incomplete \\c escape" src)
           (values (integer->char
                     (bitwise-xor (char->integer (string-ref src (+ i 1))) 64))
                   (+ i 2))))
      ((#\0) (parse-octal-escape src i end))
      ((#\x)
       (if (and (< (+ i 1) end) (char=? (string-ref src (+ i 1)) #\{))
           (let ((close (str-scan-char src #\} (+ i 2) end)))
             (if close
                 ;; a lone surrogate is its bare code point, as \u gives it
                 ;; (see below); past U+10FFFF is the JVM's error
                 (let ((v (parse-hex src (+ i 2) close)))
                   (cond
                     ((> v #x10FFFF) (java-re-error "Hexadecimal codepoint is too big" close))
                     ((and (<= #xD800 v) (<= v #xDFFF)) (values v (+ close 1)))
                     (else (values (integer->char v) (+ close 1)))))
                 (values #\x i)))
           (if (and (< (+ i 2) end) (hex-value (string-ref src (+ i 1)))
                    (hex-value (string-ref src (+ i 2))))
               (let ((v (+ (* 16 (hex-value (string-ref src (+ i 1))))
                           (hex-value (string-ref src (+ i 2))))))
                 (values (integer->char v) (+ i 3)))
               (values #\x (+ i 1)))))
      ;; \uXXXX. The JVM reads a high surrogate followed by \u + a low surrogate
      ;; as the one supplementary code point they encode (Pattern.u()), which is
      ;; how a pattern spells an emoji in Java source. A lone surrogate is a
      ;; legal Java char but not a Chez one, and a jolt string, which holds code
      ;; points, can never contain one. So it comes back as its bare code point
      ;; (an integer, not a char), and the callers make it match nothing:
      ;; jr-unit->sre outside a class, maybe-cc-range inside one. That is
      ;; exact over jolt strings, and it lets ported code that guards with
      ;; #"[\uD800-\uDBFF]" load (#1611).
      ((#\u)
       (let ((cp (parse-uxxxx src (+ i 1) end)))
         (if (not cp)
             (values #\u (+ i 1))
             (let ((cp2 (and (<= #xD800 cp) (<= cp #xDBFF)
                             (< (+ i 6) end)
                             (char=? (string-ref src (+ i 5)) #\\)
                             (char=? (string-ref src (+ i 6)) #\u)
                             (parse-uxxxx src (+ i 7) end))))
               (cond
                 ((and cp2 (<= #xDC00 cp2) (<= cp2 #xDFFF))
                  (values (integer->char (+ #x10000 (* (- cp #xD800) #x400) (- cp2 #xDC00)))
                          (+ i 11)))
                 ((and (<= #xD800 cp) (<= cp #xDFFF)) (values cp (+ i 5)))
                 (else (values (integer->char cp) (+ i 5))))))))
      (else (values #f i)))))

;; Java-compatible octal: \0 then up to 3 octal digits, value <= 0377. i points
;; at the 0.
(define (parse-octal-escape src i end)
  (let ((d1 (and (< (+ i 1) end) (oct-value? (string-ref src (+ i 1)))
                 (oct-value (string-ref src (+ i 1))))))
    (if (not d1)
        (values (integer->char 0) (+ i 1))
        (let ((d2 (and (< (+ i 2) end) (oct-value? (string-ref src (+ i 2)))
                       (oct-value (string-ref src (+ i 2))))))
          (if (not d2)
              (values (integer->char d1) (+ i 2))
              (if (<= d1 3)
                  (let ((d3 (and (< (+ i 3) end) (oct-value? (string-ref src (+ i 3)))
                                 (oct-value (string-ref src (+ i 3))))))
                    (if d3
                        (values (integer->char (+ (* d1 64) (* d2 8) d3)) (+ i 4))
                        (values (integer->char (+ (* d1 8) d2)) (+ i 3))))
                  (values (integer->char (+ (* d1 8) d2)) (+ i 3))))))))

;; Java's \R: a CRLF PAIR as one unit, or any single linebreak character. The
;; pair has to lead the alternation or "\r\n" matches as two linebreaks.
(define linebreak-sre
  `(or (seq #\return #\newline)
       #\newline ,(integer->char #x0B) ,(integer->char #x0C) #\return
       ,(integer->char #x85) ,(integer->char #x2028) ,(integer->char #x2029)))

;; ── Line terminators for DOT / ^ / $ ──────────────────────────────────────────
;; Java's terminator set is \n, \r, \r\n, NEL (U+0085), LS (U+2028) and PS
;; (U+2029); the UNIX_LINES flag ((?d)) narrows it to \n alone. irregex's own
;; `nonl`, `bol` and `eol` carry the NARROW set and nothing else, so mapping onto
;; them applied UNIX_LINES unconditionally (#956): `.` matched across a \r, (?m)^
;; did not match after one, and `(?m)^(.*)$` over CRLF input captured the \r —
;; found in an HTTP header parser, where every value came back with one attached.
;;
;; The wide forms are assertions rather than irregex ops, which costs the
;; backtracking matcher for a pattern that anchors (non-multiline `$` already
;; cost it — it has always been a look-ahead). Two rules shape them, and both are
;; Java's: a CRLF is ONE terminator, so neither anchor may sit between the \r and
;; the \n; and multiline ^ does not match at the very end of input, which is what
;; its look-ahead for one more character says.
(define java-nel (integer->char #x85))
(define java-ls (integer->char #x2028))
(define java-ps (integer->char #x2029))

(define dot-sre-wide `(~ #\newline #\return ,java-nel ,java-ls ,java-ps))
;; Multiline ^ never matches at the very end of input — not even after a final
;; terminator, and not on empty input: java.util.regex's Caret returns false at
;; endIndex before it looks at anything else (Perl's rule, which it cites). So
;; every branch, the start-of-input one included, wants one more character.
(define bol-sre-wide
  `(seq (or bos
            (look-behind (or #\newline ,java-nel ,java-ls ,java-ps))
            (seq (look-behind #\return) (look-ahead (~ #\newline))))
        (look-ahead any)))
(define bol-sre-unix `(seq bol (look-ahead any)))
(define eol-sre-wide
  `(or eos
       (look-ahead (or #\return ,java-nel ,java-ls ,java-ps))
       (seq (or bos (look-behind (~ #\return))) (look-ahead #\newline))))
;; `$` outside MULTILINE, and \Z: end of input, or just before a FINAL terminator
;; — where a CRLF is one terminator, so the position between its halves is not
;; "before a final \n" (Dollar's "No match between \r\n").
(define final-eol-sre-wide
  `(look-ahead (or eos
                   (seq #\return (? #\newline) eos)
                   (seq (or ,java-nel ,java-ls ,java-ps) eos)
                   (seq (or bos (look-behind (~ #\return))) #\newline eos))))
(define final-eol-sre-unix `(look-ahead (or eos (seq #\newline eos))))

;; Where an anchor can be emitted as a PRIMITIVE instead — each of these
;; assertions is decidable from three code units, and paying for the general
;; look-around machinery at every candidate position is what made `$` 70x the JVM
;; (#1062).  The primitives are jolt's own SRE extension, and only the Chez host
;; teaches irregex about them: host/chez/java/regex-anchors.ss registers their
;; expansions (the SREs above, which is what every other irregex walker then
;; reads them as) and regex-anchor-sre.scm compiles them.  It calls
;; `jr-use-anchor-prims!` when it loads.
;;
;; The Gambit boot ##includes THIS file and the vendored irregex and nothing
;; else — its `load` of a host/chez path is a deliberate no-op (host/gambit/
;; boot.ss) — so there the flag stays off and the SREs are emitted, exactly as
;; before #1062.  A primitive emitted into an irregex that has never heard of it
;; is an "unknown regexp" at compile.
(define jr-anchor-prims? #f)
(define (jr-use-anchor-prims!) (set! jr-anchor-prims? #t))

;; Multiline `$` under UNIX_LINES needs no primitive either way: irregex's own
;; `eol` already means "before a \n, or at the end of input", which is what
;; `(or eol eos)` said with the redundancy spelled out.
(define (jr-dot-sre flags) (if (jr-flag? flags 'unix-lines) 'nonl dot-sre-wide))
(define (jr-bol-sre flags)
  (if (jr-flag? flags 'unix-lines)
      (if jr-anchor-prims? '%java-bol-unix bol-sre-unix)
      (if jr-anchor-prims? '%java-bol bol-sre-wide)))
(define (jr-eol-sre flags)
  (if (jr-flag? flags 'unix-lines) 'eol
      (if jr-anchor-prims? '%java-eol eol-sre-wide)))
(define (jr-final-eol-sre flags)
  (if (jr-flag? flags 'unix-lines)
      (if jr-anchor-prims? '%java-final-eol-unix final-eol-sre-unix)
      (if jr-anchor-prims? '%java-final-eol final-eol-sre-wide)))

(define (parse-escape src i end flags)
  (if (>= i end)
      (values #\\ i)
      (let ((c (string-ref src i)))
        (case c
          ((#\b) (values '(or bow eow) (+ i 1)))
          ((#\B) (values 'nwb (+ i 1)))
          ((#\A) (values 'bos (+ i 1)))
          ((#\Z) (values (jr-final-eol-sre flags) (+ i 1)))
          ((#\z) (values 'eos (+ i 1)))
          ((#\R) (values linebreak-sre (+ i 1)))
          ((#\Q)
           (let* ((eos-q (scan-qe src (+ i 1) end))
                  (end-q (or eos-q end))
                  (lit (substring src (+ i 1) end-q))
                  (len (string-length lit))
                  (idx (+ end-q (if eos-q 2 0)))
                  (quant? (and (< idx end)
                               (> len 1)
                               (memv (string-ref src idx)
                                     '(#\* #\+ #\? #\{)))))
             (if quant?
                 ;; Quantifier scopes only last char in Java
                 (let* ((prefix (substring lit 0 (- len 1)))
                        (last (string-ref lit (- len 1)))
                        (prefix-chars (map (lambda (i) (string-ref prefix i))
                                           (iota (string-length prefix)))))
                   (let-values (((qm-sre qm-idx) (maybe-quantifier last src idx end flags 0)))
                     (values `(seq ,@prefix-chars ,qm-sre) qm-idx)))
                 (values (make-lit lit) idx))))
          ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (values `(backref ,(- (char->integer c) (char->integer #\0))) (+ i 1)))
          ((#\k)
           (if (and (< (+ i 1) end) (char=? (string-ref src (+ i 1)) #\<))
               (let ((gt (str-scan-char src #\> (+ i 2) end)))
                 (if (not gt)
                     (values #\k (+ i 1))
                     (let ((nm (string->symbol (substring src (+ i 2) gt))))
                       (values `(backref ,nm) (+ gt 1)))))
               (values #\k (+ i 1))))
          (else
           (let-values (((v j) (parse-escape-shared src i end flags)))
             (if v (values (jr-unit->sre v) j) (values c (+ i 1)))))))))

;; The empty character set: matches nothing, and unlike (or) it is a set, so a
;; class can complement or intersect it.
(define jr-no-char-sre '(""))
;; A lone surrogate escape answers its code point (parse-escape-shared); no jolt
;; string holds one, so as an atom or a class member it matches nothing.
(define (jr-unit->sre v) (if (fixnum? v) jr-no-char-sre v))

(define (parse-prop prefix src i end)
  ;; i points at p/P; next char is either { (braced) or property name (brace-less)
  (if (>= (+ i 1) end)
      (error 'java-pattern->sre "incomplete \\p or \\P escape" src)
      (if (char=? (string-ref src (+ i 1)) #\{)
          ;; Brace form: \p{Lu} — name between { and }
          (let ((close (str-scan-char src #\} (+ i 2) end)))
            (if (not close)
                (error 'java-pattern->sre "unterminated \\p{...} property" src)
                (let* ((name (substring src (+ i 2) close))
                       (sre (prop-class-sre name)))
                  (if sre
                      (values (if (char=? prefix #\P) `(~ ,sre) sre) (+ close 1))
                      (java-re-error (prop-unsupported-message name) close)))))
          ;; Brace-less: \pL — single char at i+1 is the property name
          (let* ((ch (string-ref src (+ i 1)))
                 (name (string ch))
                 (sre (prop-class-sre name)))
            (if sre
                (values (if (char=? prefix #\P) `(~ ,sre) sre) (+ i 2))
                (java-re-error (prop-unsupported-message name) (+ i 1)))))))

;; A \p name the validator let through is one the JVM knows (a script, a block,
;; a binary property jolt has no data for); say so rather than call it unknown.
(define (prop-unsupported-message name)
  (string-append "\\p{" name "} is unsupported on jolt (no Unicode "
                 (cond ((and (> (string-length name) 2) (string=? (substring name 0 2) "In")) "block")
                       ((and (> (string-length name) 2) (string=? (substring name 0 2) "Is")) "script or property")
                       (else "property"))
                 " table for it)"))

(define (parse-hex src start end)
  (let loop ((i start) (v 0))
    (if (>= i end) v
        (let ((h (hex-value (string-ref src i))))
          (if h (loop (+ i 1) (+ (* v 16) h)) v)))))

;; ── Character classes [...] ──────────────────────────────────────────────────

(define (parse-cc src i end flags)
  (let ((i1 (+ i 1)))
    (if (>= i1 end)
        (error 'java-pattern->sre "unterminated character class" src)
        (let ((negated? (and (char=? (string-ref src i1) #\^))))
          (let-values (((members i2)
                        (parse-cc-body src (if negated? (+ i1 1) i1) end flags #t)))
            (if (>= i2 end)
                (error 'java-pattern->sre "unterminated character class" src)
                (let ((result (cond ((null? members) 'epsilon)
                                    ((null? (cdr members)) (car members))
                                    (else `(or ,@members)))))
                  (values (if negated? `(~ ,result) result)
                          (+ i2 1)))))))))

(define (parse-cc-body src start end flags outer)
  (let loop ((i start) (members '()))
    (if (>= i end)
        (values (reverse members) i)
        (let ((c (string-ref src i)))
          (cond
           ((and outer (= i start) (char=? c #\]))
            (maybe-cc-range #\] src (+ i 1) end flags loop members))
           ((char=? c #\]) (values (reverse members) i))
           ;; nested char class [a-z&&[^b]] — parse the inner class as a member
           ((char=? c #\[)
            (let-values (((nested i2) (parse-cc src i end flags)))
              (loop i2 (cons nested members))))
           ;; intersection: a-z&&[^b]
            ((and (char=? c #\&) (< (+ i 1) end)
                  (char=? (string-ref src (+ i 1)) #\&))
             (when (and outer (null? members)
                        (< (+ i 2) end)
                        (memv (string-ref src (+ i 2)) '(#\& #\])))
               (error 'java-pattern->sre "bad class intersection syntax" src))
             (let-values (((nested i2) (parse-cc-body src (+ i 2) end flags #f)))
              (if (>= i2 end)
                  (error 'java-pattern->sre "unterminated class intersection" src)
                  (let ((prev (if (null? members) 'any (cc-members->sre (reverse members))))
                        (nested-sre (if (null? nested) 'any (cc-members->sre nested))))
                    (if (char=? (string-ref src i2) #\])
                        (values (list `(& ,prev ,nested-sre)) i2)
                        (loop (+ i2 1) (list `(& ,prev ,nested-sre))))))))
           ((char=? c #\\)
            (let-values (((atom i2) (parse-cc-escape src (+ i 1) end flags)))
              (maybe-cc-range atom src i2 end flags loop members)))
           (else
            (maybe-cc-range c src (+ i 1) end flags loop members)))))))

(define (cc-members->sre members)
  (cond ((null? members) 'epsilon)
        ((null? (cdr members)) (car members))
        (else `(or ,@members))))

;; A - after a member opens a range unless what follows is ] (the - is then a
;; literal) or a nested [ (the JVM's range() answers the single unit and the
;; - is a literal too: [a-[b]] is a, -, and the class b).
(define (maybe-cc-range atom src i end flags cont members)
  (if (and (< i end) (char=? (string-ref src i) #\-)
           (< (+ i 1) end)
           (not (memv (string-ref src (+ i 1)) '(#\] #\[))))
      (let ((i2 (+ i 1)))
        (let-values (((end-atom i3) (parse-cc-atom src i2 end flags)))
          (cond
           ((and (char? atom) (char? end-atom) (char>? atom end-atom))
            (error 'java-pattern->sre "range out of order in character class" src))
           ((and (char? atom) (char? end-atom))
            (cont i3 (cons `(/ ,atom ,end-atom) members)))
           ;; an endpoint is a lone surrogate's code point: the range is the
           ;; same code points with D800-DFFF taken out, which on jolt strings
           ;; is exactly what it matches. [\uD800-\uDBFF] is then empty.
           ((and (jr-unit-cp atom) (jr-unit-cp end-atom))
            (let ((a (jr-unit-cp atom)) (b (jr-unit-cp end-atom)))
              (when (> a b)
                (error 'java-pattern->sre "range out of order in character class" src))
              (cont i3 (cons (jr-cp-range-sre a b) members))))
           (else (cont (+ i 1) (cons #\- (cons (jr-unit->sre atom) members)))))))
      (cont i (cons (jr-unit->sre atom) members))))

;; A class member's code point when it is a single unit (a char, or a lone
;; surrogate's integer), else #f.
(define (jr-unit-cp x)
  (cond ((char? x) (char->integer x)) ((fixnum? x) x) (else #f)))
;; The code points a..b without the surrogate block, as an SRE char set.
(define (jr-cp-range-sre a b)
  (let ((lo (and (< a #xD800) `(/ ,(integer->char a) ,(integer->char (min b #xD7FF)))))
        (hi (and (> b #xDFFF) `(/ ,(integer->char (max a #xE000)) ,(integer->char b)))))
    (cond ((and lo hi) `(or ,lo ,hi))
          (lo lo)
          (hi hi)
          (else jr-no-char-sre))))

(define (parse-cc-atom src i end flags)
  (let ((c (string-ref src i)))
    (if (char=? c #\\)
        (parse-cc-escape src (+ i 1) end flags)
        (values c (+ i 1)))))

(define (parse-cc-escape src i end flags)
  (if (>= i end)
      (values #\\ i)
      (let ((c (string-ref src i)))
        (case c
          ;; The two Java rejects. A backspace for \b is PERL: java.util.regex
          ;; refuses the pattern, so refusing it here is the parity behaviour.
          ((#\b)
           (error 'java-pattern->sre "escape \\b not allowed in character class" src))
          ((#\R)
           (error 'java-pattern->sre "linebreak escape not allowed in character class" src))
          ;; \Q…\E inside a class contributes its characters as members, so the
          ;; bare-pattern copy's quantifier scoping does not apply here.
          ((#\Q)
           (let ((eos-q (scan-qe src (+ i 1) end)))
             (let* ((end-q (or eos-q end))
                    (lit (substring src (+ i 1) end-q)))
               (if (and eos-q (zero? (string-length lit)))
                   (error 'java-pattern->sre "empty quote escape in character class" src)
                   (values (make-lit lit) (+ end-q (if eos-q 2 0)))))))
          (else
           (let-values (((v j) (parse-escape-shared src i end flags)))
             (if v (values v j) (values c (+ i 1)))))))))

;; ── Groups ────────────────────────────────────────────────────────────────────

(define (parse-group src i end flags depth)
  (let ((i1 (+ i 1)))
    (if (>= i1 end)
        (java-re-error "Unclosed group" end)
        (let ((c1 (string-ref src i1)))
          (if (not (char=? c1 #\?))
              (parse-capturing-group src i1 end flags depth)
              (parse-special-group src (+ i1 1) end flags depth))))))

(define (parse-capturing-group src i end flags depth)
  (let-values (((sre i2) (parse-expr src i end flags (+ depth 1))))
    (if (and (< i2 end) (char=? (string-ref src i2) #\)))
        (values `(submatch ,sre) (+ i2 1))
        (java-re-error "Unclosed group" end))))

(define (parse-special-group src i end flags depth)
  (if (>= i end)
      (java-re-error "Unclosed group" end)
      (let ((c (string-ref src i)))
        (case c
          ((#\:)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values sre (+ i2 1))
                 (java-re-error "Unclosed group" end))))
          ((#\=)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values `(look-ahead ,sre) (+ i2 1))
                 (java-re-error "Unclosed group" end))))
          ((#\!)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values `(neg-look-ahead ,sre) (+ i2 1))
                 (java-re-error "Unclosed group" end))))
          ((#\<)
           (if (>= (+ i 1) end)
               (java-re-error "Unclosed group" end)
               (let ((c2 (string-ref src (+ i 1))))
                 (case c2
                   ((#\=)
                    (let-values (((sre i2) (parse-expr src (+ i 2) end flags (+ depth 1))))
                      (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                          (values `(look-behind ,sre) (+ i2 1))
                          (java-re-error "Unclosed group" end))))
                   ((#\!)
                    (let-values (((sre i2) (parse-expr src (+ i 2) end flags (+ depth 1))))
                      (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                          (values `(neg-look-behind ,sre) (+ i2 1))
                          (java-re-error "Unclosed group" end))))
                   (else
                    (let ((gt (str-scan-char src #\> (+ i 1) end)))
                      (if (not gt)
                          (error 'java-pattern->sre "unterminated named group" src)
                          (let ((name (string->symbol (substring src (+ i 1) gt))))
                            (let-values (((sre i2) (parse-expr src (+ gt 1) end flags (+ depth 1))))
                              (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                                  (values `(=> ,name ,sre) (+ i2 1))
                                  (error 'java-pattern->sre "unterminated named group" src)))))))))))
          ((#\>)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values `(atomic ,sre) (+ i2 1))
                 (java-re-error "Unclosed group" end))))
           ((#\c #\i #\s #\m #\x #\u #\U #\d #\- #\))
           (parse-inline-flags-group src i end flags depth))
          ((#\#)
           (let ((close (str-scan-char src #\) (+ i 1) end)))
             (if close
                 (values 'epsilon (+ close 1))
                 (error 'java-pattern->sre "unterminated comment" src))))
          (else
           (error 'java-pattern->sre "unknown (?… group type" src))))))

;; ── Inline flags: (?imsx-imsx:body) ─────────────────────────────────────────

(define (parse-inline-flags-group src i end flags depth)
  ;; i is just past the "(?"; parse-flag-group wants the "(" itself.
  (let ((g (parse-flag-group src (- i 2) end)))
    (if (not g)
        (error 'java-pattern->sre "unrecognized inline flag" src)
        (let ((fs (flag-group-opts g))
              (j (- (flag-group-end g) 1)))  ; the terminator's own index
          (if (flag-group-scoped? g)
              (let ((new-flags (apply-inline-flags flags fs)))
                (let-values (((sre i2) (parse-expr src (+ j 1) end new-flags (+ depth 1))))
                  (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                      (values (wrap-case-flag sre fs) (+ i2 1))
                      (java-re-error "Unclosed group" end))))
              ;; Unscoped toggle (?i) — parse the remainder with the new flags
              (let ((new-flags (apply-inline-flags flags fs))
                    (k (+ j 1)))
                (let ((qc (and (< k end) (string-ref src k))))
                  (cond
                   ((and qc (memv qc '(#\* #\+ #\?)))
                    (error 'java-pattern->sre "dangling quantifier after flag group" src))
                   ((and qc (char=? qc #\{))
                    (let-values (((qi i2) (parse-bounded 'epsilon src k end flags depth)))
                      (let-values (((sre i3) (parse-expr src i2 end new-flags (+ depth 1))))
                        (values (wrap-flags-sre sre fs) i3))))
                   (else
                    (let-values (((sre i2) (parse-expr src k end new-flags (+ depth 1))))
                      (values (wrap-flags-sre sre fs) i2)))))))))))

;; A flag group as the symbols apply-inline-flags understands, with the OFF set
;; resolved against the ON set rather than left to the order they are applied in.
;; A letter named on both sides is off — the JVM runs addFlag and then subFlag —
;; and resolving it here is also what keeps wrap-case-flag from being handed both
;; case-insensitive and case-sensitive and taking whichever it tests for first.
;; u, U and c (UNICODE_CASE, UNICODE_CHARACTER_CLASS, CANON_EQ) are accepted and
;; dropped: they do not change what this engine matches.
(define (flag-group-opts g)
  (let loop ((cs '(#\i #\s #\m #\x #\d)) (out '()))
    (if (null? cs)
        out
        (let ((c (car cs)))
          (loop (cdr cs)
                (cond ((memv c (flag-group-off g)) (cons (regex-flag->off-opt c) out))
                      ((memv c (flag-group-on g)) (cons (regex-flag->opt c) out))
                      (else out)))))))

(define (regex-flag->off-opt c)
  (cond ((char=? c #\i) 'case-sensitive)
        ((char=? c #\s) 'not-single-line)
        ((char=? c #\m) 'not-multi-line)
        ((char=? c #\x) 'not-ignore-space)
        ((char=? c #\d) 'not-unix-lines)
        (else #f)))

(define (apply-inline-flags flags fs)
  (let loop ((fs fs) (flags flags))
    (if (null? fs) flags
        (let ((f (car fs)))
          (cond
           ((eq? f 'case-sensitive) (loop (cdr fs) (remq 'case-insensitive flags)))
           ((eq? f 'not-single-line) (loop (cdr fs) (remq 'single-line flags)))
           ((eq? f 'not-multi-line) (loop (cdr fs) (remq 'multi-line flags)))
           ((eq? f 'not-ignore-space) (loop (cdr fs) (remq 'ignore-space flags)))
           ((eq? f 'not-unix-lines) (loop (cdr fs) (remq 'unix-lines flags)))
           (else (loop (cdr fs) (cons f flags))))))))

(define (wrap-case-flag sre fs)
  (cond
   ((memq 'case-insensitive fs) `(w/nocase ,sre))
   ((memq 'case-sensitive fs)   `(w/case ,sre))
   (else sre)))

(define (wrap-flags-sre sre fs)
  (let loop ((fs fs) (sre sre))
    (if (null? fs) sre
        (let ((f (car fs)))
          (cond
           ((eq? f 'case-insensitive) (loop (cdr fs) `(w/nocase ,sre)))
           ((eq? f 'case-sensitive)   (loop (cdr fs) `(w/case ,sre)))
           ((memq f '(not-single-line not-multi-line not-ignore-space not-unix-lines))
            (loop (cdr fs) sre))
           (else (loop (cdr fs) sre)))))))
