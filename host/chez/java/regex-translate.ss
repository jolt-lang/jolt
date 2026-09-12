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

;; ── (?x) whitespace/comment stripping (from regex.ss, duplicated for standalone use)

(define (regex-x-strip s start)
  (let ((n (string-length s)) (out (open-output-string)))
    (let loop ((i start))
      (if (>= i n)
          (get-output-string out)
          (let ((c (string-ref s i)))
            (cond
             ((and (char=? c #\\) (< (+ i 1) n))
              (write-char c out) (write-char (string-ref s (+ i 1)) out)
              (loop (+ i 2)))
             ((char=? c #\#)
              (let skip ((j (+ i 1)))
                (if (>= j n) (loop j)
                    (if (char=? (string-ref s j) #\newline)
                        (loop (+ j 1))
                        (skip (+ j 1))))))
             ((memv c '(#\space #\tab #\newline #\return #\x0B #\x0C))
              (loop (+ i 1)))
             (else (write-char c out) (loop (+ i 1)))))))))

(define (apply-global-x src)
  (let ((n (string-length src)))
    (let loop ((i 0) (in-class #f))
      (if (>= (+ i 2) n)
          src
          (let ((c (string-ref src i)))
            (cond
             ((and (char=? c #\\) (< (+ i 1) n)) (loop (+ i 2) in-class))
             ((and (not in-class) (char=? c #\[)) (loop (+ i 1) #t))
             ((and in-class (char=? c #\])) (loop (+ i 1) #f))
             ((and (not in-class) (char=? c #\()
                   (char=? (string-ref src (+ i 1)) #\?))
              (let scan ((j (+ i 2)) (fs '()))
                (if (>= j n) src
                    (let ((fc (string-ref src j)))
                      (cond
                       ((memv fc '(#\s #\i #\m #\x #\u #\d #\U))
                        (scan (+ j 1) (cons fc fs)))
                       ((and (char=? fc #\)) (pair? fs) (memv #\x fs))
                        (let ((others (reverse (remv #\x fs))))
                          (string-append
                           (substring src 0 i)
                           (apply string-append
                                  (map (lambda (f) (string #\( #\? f #\))) others))
                           (regex-x-strip src (+ j 1)))))
                       (else (loop (+ i 1) in-class)))))))
             (else (loop (+ i 1) in-class))))))))

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

(define (parse-leading-flags src i end)
  (let loop ((i i) (opts '()))
    (if (>= (+ i 3) end)
        (values (reverse opts) i)
        (let ((c0 (string-ref src i))
              (c1 (string-ref src (+ i 1))))
          (if (and (char=? c0 #\() (char=? c1 #\?))
              (let scan ((j (+ i 2)) (fs '()))
                (if (>= j end)
                    (values (reverse opts) i)
                    (let ((c (string-ref src j)))
                      (cond
                       ((memv c '(#\u #\U)) (scan (+ j 1) fs))
                       ((regex-flag->opt c) =>
                        (lambda (opt) (scan (+ j 1) (cons opt fs))))
                       ((char=? c #\))
                        (if (and (< (+ j 1) end)
                                 (memv (string-ref src (+ j 1)) '(#\* #\+ #\?)))
                            (error 'java-pattern->sre
                                   "dangling quantifier after flag group" src)
                            (loop (+ j 1) (append opts (reverse fs)))))
                       ((char=? c #\:) (values (reverse opts) i))
                       (else (values (reverse opts) i))))))
              (values (reverse opts) i))))))

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
;; the real Unicode General Category ranges from Chez's own
;; char-general-category instead. ~10ms over the full codepoint space,
;; paid once and lazily, only if a pattern actually uses \p{L}/\p{N}.
(define (unicode-category-ranges categories)
  (let loop ((cp 0) (start #f) (ranges '()))
    (define (close-at cp ranges)
      (if start (cons `(/ ,(integer->char start) ,(integer->char (- cp 1))) ranges) ranges))
    (cond
     ((> cp #x10FFFF) (cons 'or (reverse (close-at cp ranges))))
     ((and (>= cp #xD800) (<= cp #xDFFF)) (loop (+ cp 1) start ranges)) ; surrogates
     (else
      (let ((in? (memq (char-general-category (integer->char cp)) categories)))
        (cond
         ((and in? (not start)) (loop (+ cp 1) cp ranges))
         ((and (not in?) start) (loop (+ cp 1) #f (close-at cp ranges)))
         (else (loop (+ cp 1) start ranges))))))))

;; The JVM's rule, which these follow: a Unicode CATEGORY name (\p{L}, \p{Lu},
;; \p{N}, \p{Nd}, \p{P} ...) is the real category over every codepoint, while a
;; POSIX name (\p{Alpha}, \p{Digit}, \p{Upper}, \p{Punct} ...) is ASCII unless
;; UNICODE_CHARACTER_CLASS is on -- so \p{Alpha} matches "a" and not "é", and
;; \p{Nd} matches an Arabic-Indic digit but not a Roman numeral (that is \p{N}).
;; javaLowerCase/javaUpperCase are Character.isLowerCase/isUpperCase, which
;; Ll/Lu approximate far better than ASCII did.
(define sre-unicode-L  (delay (unicode-category-ranges '(Lu Ll Lt Lm Lo))))
(define sre-unicode-Lu (delay (unicode-category-ranges '(Lu))))
(define sre-unicode-Ll (delay (unicode-category-ranges '(Ll))))
(define sre-unicode-N  (delay (unicode-category-ranges '(Nd Nl No))))
(define sre-unicode-Nd (delay (unicode-category-ranges '(Nd))))
(define sre-unicode-P  (delay (unicode-category-ranges '(Pc Pd Ps Pe Pi Pf Po))))
(define sre-unicode-Ps (delay (unicode-category-ranges '(Ps))))
(define sre-unicode-Pe (delay (unicode-category-ranges '(Pe))))

(define (prop-class-sre name)
  (cond
   ((string=? name "L") (force sre-unicode-L))
   ((string=? name "Lu") (force sre-unicode-Lu))
   ((string=? name "Ll") (force sre-unicode-Ll))
   ((string=? name "N") (force sre-unicode-N))
   ((string=? name "Nd") (force sre-unicode-Nd))
   ;; POSIX names: ASCII, as on the JVM
   ((string=? name "Alpha") 'alpha)
   ((string=? name "Digit") 'numeric)
   ;; The Unicode separator categories are a short fixed list, so spell them out
   ;; rather than settling for irregex's ASCII `blank`. Zs is the space separators
   ;; (the non-breaking ones included — \p{Z} is a category, not Java's
   ;; isWhitespace), Zl the line separator, Zp the paragraph separator.
   ((string=? name "Zs") sre-Zs)
   ((string=? name "Zl") #\x2028)
   ((string=? name "Zp") #\x2029)
   ((string=? name "Z") sre-Z)
   ((string=? name "P") (force sre-unicode-P))
   ((string=? name "Ps") (force sre-unicode-Ps))
   ((string=? name "Pe") (force sre-unicode-Pe))
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
   ((string=? name "javaLowerCase") (force sre-unicode-Ll))
   ((string=? name "javaUpperCase") (force sre-unicode-Lu))
   ((string=? name "javaWhitespace") 'whitespace)
   (else #f)))

;; ── Literal string → SRE ──────────────────────────────────────────────────────

(define (make-lit s)
  (let ((len (string-length s)))
    (cond ((= len 0) 'epsilon)
          ((= len 1) (string-ref s 0))
          (else `(seq ,@(map (lambda (i) (string-ref s i)) (iota len)))))))

;; ── Entry point ───────────────────────────────────────────────────────────────

(define (java-pattern->sre source)
  (let ((source (apply-global-x source)))
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
(define (parse-escape-shared src i end flags)
  (let ((c (string-ref src i)))
    (case c
      ((#\d) (values 'numeric (+ i 1)))
      ((#\D) (values '(~ numeric) (+ i 1)))
      ((#\w) (values '(or alphanumeric #\_) (+ i 1)))
      ((#\W) (values '(~ (or alphanumeric #\_)) (+ i 1)))
      ((#\s) (values 'whitespace (+ i 1)))
      ((#\S) (values '(~ whitespace) (+ i 1)))
      ((#\p #\P) (parse-prop c src i end))
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
                 (let ((v (parse-hex src (+ i 2) close)))
                   (values (integer->char v) (+ close 1)))
                 (values #\x i)))
           (if (and (< (+ i 2) end) (hex-value (string-ref src (+ i 1)))
                    (hex-value (string-ref src (+ i 2))))
               (let ((v (+ (* 16 (hex-value (string-ref src (+ i 1))))
                           (hex-value (string-ref src (+ i 2))))))
                 (values (integer->char v) (+ i 3)))
               (values #\x (+ i 1)))))
      ((#\u)
       (if (and (< (+ i 4) end) (hex-value (string-ref src (+ i 1)))
                (hex-value (string-ref src (+ i 2)))
                (hex-value (string-ref src (+ i 3)))
                (hex-value (string-ref src (+ i 4))))
           (let ((cp (+ (* 4096 (hex-value (string-ref src (+ i 1))))
                        (* 256 (hex-value (string-ref src (+ i 2))))
                        (* 16 (hex-value (string-ref src (+ i 3))))
                        (hex-value (string-ref src (+ i 4))))))
             (values (integer->char cp) (+ i 5)))
           (values #\u (+ i 1))))
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

(define (jr-dot-sre flags) (if (jr-flag? flags 'unix-lines) 'nonl dot-sre-wide))
(define (jr-bol-sre flags) (if (jr-flag? flags 'unix-lines) bol-sre-unix bol-sre-wide))
(define (jr-eol-sre flags) (if (jr-flag? flags 'unix-lines) '(or eol eos) eol-sre-wide))
(define (jr-final-eol-sre flags)
  (if (jr-flag? flags 'unix-lines) final-eol-sre-unix final-eol-sre-wide))

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
             (if v (values v j) (values c (+ i 1)))))))))

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
                      (error 'java-pattern->sre
                             (string-append "unknown \\p property: " name) src)))))
          ;; Brace-less: \pL — single char at i+1 is the property name
          (let* ((ch (string-ref src (+ i 1)))
                 (name (string ch))
                 (sre (prop-class-sre name)))
            (if sre
                (values (if (char=? prefix #\P) `(~ ,sre) sre) (+ i 2))
                (error 'java-pattern->sre
                       (string-append "unknown \\p property: " name) src))))))

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

(define (maybe-cc-range atom src i end flags cont members)
  (if (and (< i end) (char=? (string-ref src i) #\-)
           (< (+ i 1) end)
           (not (char=? (string-ref src (+ i 1)) #\])))
      (let ((i2 (+ i 1)))
        (let-values (((end-atom i3) (parse-cc-atom src i2 end flags)))
          (cond
           ((and (char? atom) (char? end-atom) (char>? atom end-atom))
            (error 'java-pattern->sre "range out of order in character class" src))
           ((and (char? atom) (char? end-atom))
            (cont i3 (cons `(/ ,atom ,end-atom) members)))
           (else (cont (+ i 1) (cons #\- (cons atom members)))))))
      (cont i (cons atom members))))

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
        (error 'java-pattern->sre "unterminated group" src)
        (let ((c1 (string-ref src i1)))
          (if (not (char=? c1 #\?))
              (parse-capturing-group src i1 end flags depth)
              (parse-special-group src (+ i1 1) end flags depth))))))

(define (parse-capturing-group src i end flags depth)
  (let-values (((sre i2) (parse-expr src i end flags (+ depth 1))))
    (if (and (< i2 end) (char=? (string-ref src i2) #\)))
        (values `(submatch ,sre) (+ i2 1))
        (error 'java-pattern->sre "unterminated capturing group" src))))

(define (parse-special-group src i end flags depth)
  (if (>= i end)
      (error 'java-pattern->sre "unterminated group" src)
      (let ((c (string-ref src i)))
        (case c
          ((#\:)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values sre (+ i2 1))
                 (error 'java-pattern->sre "unterminated non-capturing group" src))))
          ((#\=)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values `(look-ahead ,sre) (+ i2 1))
                 (error 'java-pattern->sre "unterminated lookahead" src))))
          ((#\!)
           (let-values (((sre i2) (parse-expr src (+ i 1) end flags (+ depth 1))))
             (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                 (values `(neg-look-ahead ,sre) (+ i2 1))
                 (error 'java-pattern->sre "unterminated neg-lookahead" src))))
          ((#\<)
           (if (>= (+ i 1) end)
               (error 'java-pattern->sre "unterminated group" src)
               (let ((c2 (string-ref src (+ i 1))))
                 (case c2
                   ((#\=)
                    (let-values (((sre i2) (parse-expr src (+ i 2) end flags (+ depth 1))))
                      (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                          (values `(look-behind ,sre) (+ i2 1))
                          (error 'java-pattern->sre "unterminated lookbehind" src))))
                   ((#\!)
                    (let-values (((sre i2) (parse-expr src (+ i 2) end flags (+ depth 1))))
                      (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                          (values `(neg-look-behind ,sre) (+ i2 1))
                          (error 'java-pattern->sre "unterminated neg-lookbehind" src))))
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
                 (error 'java-pattern->sre "unterminated atomic group" src))))
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
  (let scan ((j i) (fs '()) (neg #f))
    (if (>= j end)
        (error 'java-pattern->sre "unterminated inline flags" src)
        (let ((c (string-ref src j)))
          (cond
           ((char=? c #\-) (scan (+ j 1) fs #t))
           ((char=? c #\i)
            (scan (+ j 1)
                  (cons (if neg 'case-sensitive 'case-insensitive) fs) neg))
           ((char=? c #\s)
            (scan (+ j 1)
                  (cons (if neg 'not-single-line 'single-line) fs) neg))
           ((char=? c #\m)
            (scan (+ j 1)
                  (cons (if neg 'not-multi-line 'multi-line) fs) neg))
           ((char=? c #\x)
            (scan (+ j 1)
                  (cons (if neg 'not-ignore-space 'ignore-space) fs) neg))
           ((char=? c #\d)
            (scan (+ j 1)
                  (cons (if neg 'not-unix-lines 'unix-lines) fs) neg))
            ;; u (UNICODE_CASE) and U (UNICODE_CHARACTER_CLASS): accept and
            ;; ignore — they don't change matching for our engine.
            ((memv c '(#\c #\u #\U))
             (scan (+ j 1) fs neg))
           ((char=? c #\:)
            (let ((new-flags (apply-inline-flags flags fs)))
              (let-values (((sre i2) (parse-expr src (+ j 1) end new-flags (+ depth 1))))
                (if (and (< i2 end) (char=? (string-ref src i2) #\)))
                    (values (wrap-case-flag sre fs) (+ i2 1))
                    (error 'java-pattern->sre "unterminated scoped flags group" src)))))
           ((char=? c #\))
            ;; Unscoped toggle (?i) — parse remainder with new flags
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
                    (values (wrap-flags-sre sre fs) i2)))))))
           (else
            (error 'java-pattern->sre "unrecognized inline flag" src)))))))

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
