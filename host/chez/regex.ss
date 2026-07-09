;; regex on Chez via vendored irregex.
;;
;; Chez has no regex at all. We vendor
;; Alex Shinn's irregex (vendor/irregex, BSD) — a portable Scheme regex with
;; PCRE/Java-style STRING patterns — and wrap jolt's re-* surface over it.
;;
;; irregex maps cleanly onto the Clojure fns: irregex-match is an anchored
;; whole-string match (= re-matches), irregex-search finds the first match
;; anywhere (= re-find), irregex-match-substring extracts group N (0 = whole).
;; Results follow Clojure shape: a 0-group match is the whole string; a grouped
;; match is a jolt VECTOR [whole g1 ...] (a non-participating group is nil); a nil
;; result is jolt-nil; re-seq is a jolt seq (nil when there are no matches).
;;
;; The re-* fns are def-var!'d into clojure.core so prelude / -e code resolves
;; them at runtime (they're NOT subset native-ops: irregex's Unicode/property-
;; class semantics keep them out
;; of the subset-parity corpus). Loaded from rt.ss after def-var! is defined.

;; irregex.scm is portable R[457]RS; two small adaptations for Chez's top level:
;; a cond-expand at expression position (Chez's is library-only), and `error`
;; called with a lone string (Chez's error wants who+msg). The wrapper normalizes
;; both without changing behavior for valid patterns.
(define-syntax cond-expand
  (syntax-rules (else)
    ((_ (else e ...)) (begin e ...))
    ((_ (else e ...) c ...) (begin e ...))
    ((_ (req e ...) c ...) (cond-expand c ...))
    ((_) (if #f #f))))
(define %chez-error error)
(define (error . args)
  (if (and (pair? args) (string? (car args)))
      (apply %chez-error #f args)
      (apply %chez-error args)))
(load "vendor/irregex/irregex.scm")

;; irregex rejects a quantifier applied to anything that already contains one —
;; including a GROUP like (a+)* — because sre-repeater? recurses through submatch.
;; Java only rejects a DANGLING double quantifier (a**); it allows a quantifier on
;; a group whose body is quantified. Restrict the check to a bare leading * / + so
;; a** still errors but (a+)* parses (cuerdas's format tokenizer needs this).
(set! sre-repeater?
  (lambda (sre) (and (pair? sre) (memq (car sre) '(* +)) #t)))

;; Unicode property classes \p{...}: irregex's string syntax has no
;; \p{...}, so translate a fixed set of property names
;; to ASCII char classes before compiling. ASCII-only — \p{L} would need
;; UTF-8 high bytes counted as letters, which a Unicode-char Scheme string can't
;; reproduce byte-for-byte; the corpus tests ASCII inputs, where they agree. An
;; unmapped name is left as-is (irregex errors, as before — no new behavior). The
;; ORIGINAL source is kept for printing; only the compiled pattern is translated.
(define (prop-class name)
  (cond
    ;; L/Alpha: ASCII letters + non-ASCII up to just below the UTF-16 surrogate gap
    ;; (D800). This covers essentially every real letter (Latin/Greek/Cyrillic/CJK/…
    ;; live below D800); the supplementary planes above it are rare and a range that
    ;; reaches them makes irregex's char-set construction call integer->char on a
    ;; surrogate and crash. N/Z stay ASCII-only.
    ((or (string=? name "L") (string=? name "Alpha")) "a-zA-Z\\x80-\\x{D7FF}")
    ((string=? name "Lu") "A-Z")
    ((string=? name "Ll") "a-z")
    ((or (string=? name "N") (string=? name "Nd") (string=? name "Digit")) "0-9")
    ((or (string=? name "Z") (string=? name "Zs")) " ")
    ((string=? name "Ps") "([{")
    ((string=? name "Pe") ")\\]}")
    (else #f)))
;; Tracks whether the cursor is inside a [...] char class: a \p{X} there emits the
;; class CONTENT (inlined), standalone it emits a wrapping [X]. Escapes
;; (\[, \]) don't toggle the class. \P (negation) only wraps when standalone.
(define (translate-prop-classes src)
  (let ((len (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f))
      (if (fx>=? i len)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ;; \p{Name} / \P{Name}
              ((and (char=? c #\\) (fx<? (fx+ i 2) len)
                    (let ((p (string-ref src (fx+ i 1)))) (or (char=? p #\p) (char=? p #\P)))
                    (char=? (string-ref src (fx+ i 2)) #\{))
               (let* ((close (let scan ((j (fx+ i 3)))
                               (cond ((fx>=? j len) #f)
                                     ((char=? (string-ref src j) #\}) j)
                                     (else (scan (fx+ j 1))))))
                      (cls (and close (prop-class (substring src (fx+ i 3) close)))))
                 (cond
                   ((not cls) (write-char c out) (loop (fx+ i 1) in-class))
                   (in-class (display cls out) (loop (fx+ close 1) in-class))
                   (else
                    (display "[" out)
                    (when (char=? (string-ref src (fx+ i 1)) #\P) (display "^" out))
                    (display cls out) (display "]" out)
                    (loop (fx+ close 1) in-class)))))
              ;; any other escape: copy the pair verbatim, don't toggle class state
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (write-char c out) (write-char (string-ref src (fx+ i 1)) out)
               (loop (fx+ i 2) in-class))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f))
              (else (write-char c out) (loop (fx+ i 1) in-class))))))))

;; Inside a [...] class, irregex reads backslash POSIX-style: '\]' is a literal
;; backslash and the ']' ends the class (Java reads it as an escaped ']').
;; Rewrite a class-internal '\]' to '\x5D' — accepted standalone and as a range
;; endpoint, so no reordering games. Outside a class '\]' passes through.
(define (escape-class-bracket src)
  (let ((len (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f))
      (if (fx>=? i len)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (let ((n (string-ref src (fx+ i 1))))
                 (if (and in-class (char=? n #\]))
                     (put-string out "\\x5D")
                     (begin (write-char c out) (write-char n out)))
                 (loop (fx+ i 2) in-class)))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f))
              (else (write-char c out) (loop (fx+ i 1) in-class))))))))

;; Java accepts any combination of inline flags — (?sx), (?si:...) — while
;; irregex rejects combined clusters ("unknown regex cluster modifier") and
;; accepts only i/x/u inline (s never inline; leading s/i/m are peeled into
;; constructor options by regex-parse-flags below). Normalize toward what the
;; layers downstream can express:
;;   prefix (?sx)   -> (?s)(?x), SORTED so strippable flags (s,i,m) lead and
;;                     parse-flags consumes them; x/u remain inline for irregex.
;;   scoped (?ix:B) -> (?i:(?x:B)) via nesting of inline-supported flags.
;;   scoped s       -> the s is dropped from the flags and the group body's
;;                     unescaped dots become [\s\S] (dot-all by construction).
;; '(?' inside a [...] class is literal and left alone.
(define (split-cluster-modifiers src)
  (define (flag-char? c) (memv c '(#\s #\x #\i #\m #\u)))
  (define (flag-rank c) (case c ((#\s) 0) ((#\i) 1) ((#\m) 2) ((#\x) 3) (else 4)))
  ;; index just past the ) that closes the group opening at open-idx (the char
  ;; AFTER "(?flags:"). Honors escapes and [...] classes.
  (define (group-end body-start)
    (let scan ((i body-start) (depth 1) (in-class #f))
      (if (fx>=? i (string-length src))
          #f
          (let ((c (string-ref src i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) (string-length src))) (scan (fx+ i 2) depth in-class))
              (in-class (scan (fx+ i 1) depth (not (char=? c #\]))))
              ((char=? c #\[) (scan (fx+ i 1) depth #t))
              ((char=? c #\() (scan (fx+ i 1) (fx+ depth 1) #f))
              ((char=? c #\))
               (if (fx=? depth 1) i (scan (fx+ i 1) (fx- depth 1) #f)))
              (else (scan (fx+ i 1) depth #f)))))))
  ;; a group body with each unescaped '.' outside a class replaced by [\s\S]
  (define (dot-all body)
    (let ((out (open-output-string)) (n (string-length body)))
      (let walk ((i 0) (in-class #f))
        (if (fx>=? i n)
            (get-output-string out)
            (let ((c (string-ref body i)))
              (cond
                ((and (char=? c #\\) (fx<? (fx+ i 1) n))
                 (write-char c out) (write-char (string-ref body (fx+ i 1)) out)
                 (walk (fx+ i 2) in-class))
                (in-class (write-char c out) (walk (fx+ i 1) (not (char=? c #\]))))
                ((char=? c #\[) (write-char c out) (walk (fx+ i 1) #t))
                ((char=? c #\.) (put-string out "[\\s\\S]") (walk (fx+ i 1) #f))
                (else (write-char c out) (walk (fx+ i 1) #f))))))))
  (let ((len (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f))
      (if (fx>=? i len)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (write-char c out) (write-char (string-ref src (fx+ i 1)) out)
               (loop (fx+ i 2) in-class))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f))
              ((and (not in-class) (char=? c #\() (fx<? (fx+ i 1) len)
                    (char=? (string-ref src (fx+ i 1)) #\?))
               (let scan ((j (fx+ i 2)) (flags '()))
                 (cond
                   ((and (fx<? j len) (flag-char? (string-ref src j)))
                    (scan (fx+ j 1) (cons (string-ref src j) flags)))
                   ;; prefix cluster (?fs) with 2+ flags: emit sorted singles
                   ((and (fx<? j len) (>= (length flags) 2)
                         (char=? (string-ref src j) #\)))
                    (for-each (lambda (f) (write-char #\( out) (write-char #\? out)
                                          (write-char f out) (write-char #\) out))
                              (sort (lambda (a b) (< (flag-rank a) (flag-rank b)))
                                    flags))
                    (loop (fx+ j 1) in-class))
                   ;; scoped cluster (?fs:BODY): nest inline flags; s becomes a
                   ;; dot-all rewrite of the body
                   ((and (fx<? j len) (pair? flags) (char=? (string-ref src j) #\:)
                         (or (>= (length flags) 2) (memv #\s flags)))
                    (let ((end (group-end (fx+ j 1))))
                      (if (not end)
                          (begin (write-char c out) (loop (fx+ i 1) in-class))
                          (let* ((body (substring src (fx+ j 1) end))
                                 (body (if (memv #\s flags) (dot-all body) body))
                                 (inline (sort (lambda (a b) (< (flag-rank a) (flag-rank b)))
                                               (remv #\s flags))))
                            (put-string out "(?:")
                            (for-each (lambda (f)
                                        (put-string out "(?") (write-char f out)
                                        (write-char #\: out))
                                      inline)
                            (put-string out body)
                            (for-each (lambda (_) (write-char #\) out)) inline)
                            (write-char #\) out)
                            (loop (fx+ end 1) in-class)))))
                   (else (write-char c out) (loop (fx+ i 1) in-class)))))
              (else (write-char c out) (loop (fx+ i 1) in-class))))))))

;; Inside a [...] class, irregex reads a '-' that follows a shorthand class
;; (\w \d \s \W \D \S) as the start of a range and errors ("bad char-set"); Java
;; reads it as a literal hyphen (a shorthand can't be a range endpoint). Escape
;; such a '-' to \- so the class parses. Only a '-' right after a shorthand and
;; not the class terminator is touched; a '-' after a plain char (a real range
;; like [a-z]) is left alone.
(define (escape-class-shorthand-dash src)
  (let ((len (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f) (after-shorthand #f))
      (if (fx>=? i len)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ;; an escape pair: \w-style shorthand sets after-shorthand inside a class
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (let ((n (string-ref src (fx+ i 1))))
                 (write-char c out) (write-char n out)
                 (loop (fx+ i 2) in-class
                       (and in-class (memv n '(#\w #\d #\s #\W #\D #\S)) #t))))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t #f))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f #f))
              ;; the case Java reads as a literal hyphen
              ((and in-class after-shorthand (char=? c #\-)
                    (fx<? (fx+ i 1) len) (not (char=? (string-ref src (fx+ i 1)) #\])))
               (write-char #\\ out) (write-char #\- out)
               (loop (fx+ i 1) in-class #f))
              (else (write-char c out) (loop (fx+ i 1) in-class #f))))))))

;; Java COMMENTS mode ((?x)): literal whitespace (space \t \n \v \f \r) and
;; #-to-end-of-line comments are stripped from the pattern — INCLUDING inside
;; character classes (a Java quirk; PCRE keeps class whitespace), with escaped
;; whitespace (\ ) and \# kept literal. irregex's own x only ignores spaces,
;; not newlines, so a multi-line (?x) pattern silently matches nothing there;
;; implement Java's stripping here and drop x before irregex sees the pattern.
(define (regex-x-strip s start)
  (let ((n (string-length s)) (out (open-output-string)))
    (let loop ((i start))
      (if (fx>=? i n)
          (get-output-string out)
          (let ((c (string-ref s i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) n))
               (write-char c out) (write-char (string-ref s (fx+ i 1)) out)
               (loop (fx+ i 2)))
              ((char=? c #\#)
               (let skip ((j (fx+ i 1)))
                 (cond ((fx>=? j n) (loop j))
                       ((char=? (string-ref s j) #\newline) (loop (fx+ j 1)))
                       (else (skip (fx+ j 1))))))
              ((memv c '(#\space #\tab #\newline #\return #\x0B #\x0C))
               (loop (fx+ i 1)))
              (else (write-char c out) (loop (fx+ i 1)))))))))
;; A flag cluster containing x — (?x), (?sx) — engages COMMENTS mode from its
;; position to the end of the pattern (Java allows a mid-pattern cluster; a
;; template-composed pattern like "\$(?x)\n…" puts one after a literal prefix).
;; Strip the tail, drop x, keep the cluster's other flags as singles. Scoped
;; (?x:…) groups are not rewritten (nothing in the conformance libraries scopes
;; x; extend group-end-style scanning if one does).
(define (apply-global-x src)
  (let ((n (string-length src)))
    (let loop ((i 0) (in-class #f))
      (if (fx>=? (fx+ i 2) n)
          src
          (let ((c (string-ref src i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) n)) (loop (fx+ i 2) in-class))
              ((and (not in-class) (char=? c #\[)) (loop (fx+ i 1) #t))
              ((and in-class (char=? c #\])) (loop (fx+ i 1) #f))
              ((and (not in-class) (char=? c #\()
                    (char=? (string-ref src (fx+ i 1)) #\?))
               (let scan ((j (fx+ i 2)) (flags '()))
                 (cond
                   ((fx>=? j n) src)
                   ((memv (string-ref src j) '(#\s #\i #\m #\x #\u))
                    (scan (fx+ j 1) (cons (string-ref src j) flags)))
                   ((and (char=? (string-ref src j) #\)) (pair? flags)
                         (memv #\x flags))
                    (let ((others (reverse (remv #\x flags))))
                      (string-append
                        (substring src 0 i)
                        (apply string-append
                               (map (lambda (f) (string #\( #\? f #\))) others))
                        (regex-x-strip src (fx+ j 1)))))
                   ;; not an x flag cluster — (?:…), (?=…), (?s) alone, etc.
                   (else (loop (fx+ i 1) in-class)))))
              (else (loop (fx+ i 1) in-class))))))))

;; Java reads a backslash before any punctuation as a literal escape; irregex
;; gives \< and \> word-boundary meaning, so a Java-literal \> silently never
;; matches. '<' and '>' are plain literals unescaped in both engines — drop the
;; backslash. A preceding \\ pair is consumed first, so an escaped backslash
;; followed by an angle keeps its meaning.
(define (strip-angle-escapes src)
  (let ((n (string-length src)) (out (open-output-string)))
    (let loop ((i 0))
      (if (fx>=? i n)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (if (and (char=? c #\\) (fx<? (fx+ i 1) n))
                (let ((d (string-ref src (fx+ i 1))))
                  (if (memv d '(#\< #\>))
                      (begin (write-char d out) (loop (fx+ i 2)))
                      (begin (write-char c out) (write-char d out) (loop (fx+ i 2)))))
                (begin (write-char c out) (loop (fx+ i 1)))))))))

;; Java's $ (without MULTILINE) matches at end of input OR just before a FINAL
;; line terminator ((re-find #"foo$" "foo\n") => "foo"); irregex's $ is absolute
;; end only. Rewrite each unescaped $ outside a character class to a lookahead
;; with those semantics. The emitted inner $ is irregex's own end anchor.
;; Multi-line patterns skip this (both engines treat $ as a line boundary there).
(define (rewrite-dollar-eol src)
  (let ((n (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f))
      (if (fx>=? i n)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ((and (char=? c #\\) (fx<? (fx+ i 1) n))
               (write-char c out) (write-char (string-ref src (fx+ i 1)) out)
               (loop (fx+ i 2) in-class))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f))
              ((and (not in-class) (char=? c #\$))
               (put-string out "(?=(?:\\r?\\n)?$)")
               (loop (fx+ i 1) in-class))
              (else (write-char c out) (loop (fx+ i 1) in-class))))))))

;; Java/Clojure inline flags: a leading (?imsx…) group sets a flag over the whole
;; pattern. irregex has the same semantics but as constructor OPTIONS, not inline
;; syntax (it rejects (?s)/(?s:…)), so peel any leading flag groups off the source
;; and pass the equivalent option symbols. Scoped groups ((?:…), (?=…), (?<n>…))
;; and groups with a flag irregex can't express are left untouched for irregex.
(define (regex-flag->opt c)
  (cond ((char=? c #\s) 'single-line)        ; DOTALL — . matches newline
        ((char=? c #\i) 'case-insensitive)
        ((char=? c #\m) 'multi-line)          ; ^/$ match at line boundaries
        (else #f)))
(define (regex-parse-flags src)
  (let loop ((s src) (opts '()))
    (if (and (>= (string-length s) 4)
             (char=? (string-ref s 0) #\() (char=? (string-ref s 1) #\?))
        (let scan ((i 2) (fs '()))
          (cond
            ((>= i (string-length s)) (values (reverse opts) s))
            ((char=? (string-ref s i) #\))
             (let ((mapped (map regex-flag->opt fs)))
               (if (and (pair? fs) (for-all (lambda (x) x) mapped))
                   (loop (substring s (+ i 1) (string-length s)) (append opts mapped))
                   (values (reverse opts) s))))      ; unmappable flag — leave as-is
            ((char=? (string-ref s i) #\:) (values (reverse opts) s)) ; scoped group
            (else (scan (+ i 1) (cons (string-ref s i) fs)))))
        (values (reverse opts) s))))

;; A jolt regex value: the source string (for printing / str) + the compiled
;; irregex. regex? recognizes it; the printer renders #"source".
(define-record-type regex-t (fields source irx) (nongenerative jolt-regex-v1))
;; A capturing pattern is compiled with irregex's BACKTRACKING matcher ('backtrack),
;; not its DFA. java.util.regex is itself a leftmost-first backtracking engine, so
;; this matches the JVM's submatch semantics; irregex's DFA is POSIX leftmost-longest
;; and, worse, leaks a non-participating alternation group's capture (e.g.
;; #"(?:([0-9])|([0-9])r([0-9]+))" on "2r11" left group 1 = "2"), which broke
;; tools.reader's number reader. Non-capturing patterns keep the fast DFA — with no
;; groups to read, its whole-match result is all a caller sees. The count comes from
;; a first cheap compile; a capturing pattern is recompiled once (patterns compile
;; once and cache in the regex-t).
(define (jolt-regex source)
  ;; COMMENTS mode first (strips whitespace/comments, drops x), then normalize
  ;; combined clusters so a leading (?sx) becomes (?s)(?x) and regex-parse-flags
  ;; can peel the strippable singles into options
  (let-values (((opts pat) (regex-parse-flags (split-cluster-modifiers (apply-global-x source)))))
    (let* ((pat (strip-angle-escapes pat))
           (pat (if (memq 'multi-line opts) pat (rewrite-dollar-eol pat)))
           (p (translate-prop-classes (escape-class-shorthand-dash (escape-class-bracket pat))))
           (irx (apply irregex p opts)))
      (make-regex-t source
                    (if (> (irregex-num-submatches irx) 0)
                        (apply irregex p 'backtrack opts)
                        irx)))))
(define (jolt-regex? x) (regex-t? x))
(define (jolt-re-pattern x) (if (regex-t? x) x (jolt-regex x)))

;; An irregex match -> the Clojure result: whole string (no groups) or the
;; [whole g1 ... gn] vector (nil for a non-participating group).
(define (irx-result m)
  (let ((n (irregex-match-num-submatches m)))
    (if (= n 0)
        (irregex-match-substring m 0)
        (let loop ((i n) (acc '()))
          (if (< i 0)
              (apply jolt-vector acc)
              (let ((s (irregex-match-substring m i)))
                (loop (- i 1) (cons (if s s jolt-nil) acc))))))))

(define (jolt-re-matches re s)
  (let ((m (irregex-match (regex-t-irx (jolt-re-pattern re)) s)))
    (if m (irx-result m) jolt-nil)))

;; A stateful matcher (java.util.regex.Matcher): the compiled pattern, the target
;; string, the next search position, and the last successful irregex match. re-find
;; over a matcher steps through non-overlapping matches; re-groups returns the
;; groups of the last one.
(define-record-type matcher-t
  (fields irx str (mutable pos) (mutable last))
  (nongenerative jolt-matcher-v1))
(define (jolt-re-matcher re s)
  (make-matcher-t (regex-t-irx (jolt-re-pattern re)) s 0 #f))
(define (jolt-matcher? x) (matcher-t? x))

;; re-find: stateless over (re s), or stateful over a matcher (advance + remember).
(define jolt-re-find
  (case-lambda
    ((re s)
     (let ((m (irregex-search (regex-t-irx (jolt-re-pattern re)) s)))
       (if m (irx-result m) jolt-nil)))
    ((m)
     (let* ((str (matcher-t-str m))
            (len (string-length str))
            (start (matcher-t-pos m))
            (mm (and (<= start len) (irregex-search (matcher-t-irx m) str start))))
       (if mm
           (let ((ms (irregex-match-start-index mm 0))
                 (e (irregex-match-end-index mm 0)))
             (matcher-t-last-set! m mm)
             ;; advance past this match: to its end, or one past a zero-width match
             ;; (which may sit past the search origin, e.g. a lookahead/boundary).
             (matcher-t-pos-set! m (if (> e ms) e (+ e 1)))
             (irx-result mm))
           (begin (matcher-t-last-set! m #f) jolt-nil))))))

;; re-groups: the groups of the matcher's last successful find. Throws when no
;; match has succeeded, like Clojure's IllegalStateException "No match found".
(define (jolt-re-groups m)
  (let ((last (matcher-t-last m)))
    (if last (irx-result last)
        (jolt-throw (jolt-ex-info "No match found" (jolt-hash-map))))))

;; java.util.regex.Matcher methods over a matcher-t. .matches anchors a full-region
;; match and remembers it for .group; .group n returns submatch n (0 = whole) or
;; nil; .groupCount is the pattern's capturing-group count.
(define (jolt-matcher-matches m)
  (let ((mm (irregex-match (matcher-t-irx m) (matcher-t-str m))))
    (matcher-t-last-set! m mm)
    (if mm #t #f)))
(define (jolt-matcher-group m . n)
  (let ((last (matcher-t-last m)))
    (if last
        (let ((s (irregex-match-substring last (if (pair? n) (->idx (car n)) 0))))
          (if s s jolt-nil))
        (jolt-throw (jolt-ex-info "No match available" (jolt-hash-map))))))
(define (jolt-matcher-group-count m) (irregex-num-submatches (matcher-t-irx m)))

;; All non-overlapping matches, left to right. Advance past each match end (or by
;; one on a zero-width match). nil when there are no matches (Clojure: seq-able as
;; nil, so (if-let [m (re-seq ...)] ...) works).
(define (jolt-re-seq re s)
  (let ((irx (regex-t-irx (jolt-re-pattern re)))
        (len (string-length s)))
    (let loop ((start 0) (acc '()))
      (let ((m (and (<= start len) (irregex-search irx s start))))
        (if m
            (let ((ms (irregex-match-start-index m 0))
                  (e (irregex-match-end-index m 0)))
              ;; to the match end, or one past a zero-width match (relative to its
              ;; own start, which may be past the search origin).
              (loop (if (> e ms) e (+ e 1)) (cons (irx-result m) acc)))
            (list->cseq (reverse acc)))))))

(def-var! "clojure.core" "re-pattern" jolt-re-pattern)
(def-var! "clojure.core" "re-matches" jolt-re-matches)
(def-var! "clojure.core" "re-find" jolt-re-find)
(def-var! "clojure.core" "re-seq" jolt-re-seq)
(def-var! "clojure.core" "re-matcher" jolt-re-matcher)
(def-var! "clojure.core" "re-groups" jolt-re-groups)
(def-var! "clojure.core" "regex?" jolt-regex?)
