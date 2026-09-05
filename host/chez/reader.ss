;; Chez-side Clojure data reader.
;;
;; The data half of runtime read/eval: a recursive-descent reader that parses
;; ONE Clojure form off a string and produces jolt runtime values. Two host
;; seams hang off it:
;;   read-string  : string -> first form (clojure.core seam, src 772)
;;   __parse-next : string -> [form rest] | nil  (the *in* family seam, src 801)
;; read / read+string / with-in-str / line-seq / clojure.edn are Clojure over
;; these (jolt-core/clojure/core/50-io.clj, stdlib/clojure/edn.clj).
;;
;; Form shapes:
;;   sets     -> {:jolt/type :jolt/set :value [...]}        (a FORM, not a set)
;;   #tag frm -> {:jolt/type :jolt/tagged :tag :#tag :form ...}  (NO data reader)
;;   #"src"   -> {:jolt/type :jolt/tagged :tag :regex :form "src"}
;;   'x  `x  ~x  ~@x  @x  -> (quote x)/(syntax-quote x)/(unquote x)/
;;                            (unquote-splicing x)/(clojure.core/deref x)
;;   ^meta sym -> symbol carrying meta ({:tag "Name"} | {:kw true} | the map)
;; read-string of blank / comment-only input is nil (the documented seed wart),
;; NOT an EOF throw.

;; Reader forms reuse these interned keywords for their tag structure.
(define rdr-kw-jolt-type (keyword "jolt" "type"))
(define rdr-kw-jolt-set  (keyword "jolt" "set"))
(define rdr-kw-jolt-tagged (keyword "jolt" "tagged"))
(define rdr-kw-value (keyword #f "value"))
(define rdr-kw-tag   (keyword #f "tag"))
(define rdr-kw-form  (keyword #f "form"))

;; A unique sentinel meaning "no form here" (EOF, or a close delimiter that the
;; caller — read-seq — must consume). Never a legal jolt value, so unambiguous.
(define rdr-eof (list 'reader-eof))
(define (rdr-eof? x) (eq? x rdr-eof))

;; A splicing reader conditional #?@(...) yields this wrapper; the enclosing
;; sequence reader splices its items in place (never a legal jolt value).
(define-record-type rdr-splice-t (fields items) (nongenerative rdr-splice-v1))

(define (rdr-ws? c)
  (or (char-whitespace? c) (char=? c #\,)))

;; `'` (apostrophe) is a NON-terminating macro char in Clojure (isTerminatingMacro
;; is false for it), so it's a valid symbol constituent after the first char:
;; inc'/+'/foo' read as single symbols. A LEADING ' still dispatches as quote
;; (handled before token reading begins). Omit it from the terminator set.
(define (rdr-terminator? c)
  (or (rdr-ws? c)
      (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\@ #\^ #\` #\~ #\\))))

(define (rdr-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (rdr-octal? c) (and (char>=? c #\0) (char<=? c #\7)))
(define (rdr-all-digits? s from to)
  (and (> to from)
       (let loop ((i from))
         (cond ((>= i to) #t)
               ((rdr-digit? (string-ref s i)) (loop (+ i 1)))
               (else #f)))))
;; every char of s in [from,to) is an octal digit (and the span is non-empty).
(define (rdr-all-octal? s from to)
  (and (fx<? from to)
       (let loop ((i from)) (cond ((fx=? i to) #t) ((rdr-octal? (string-ref s i)) (loop (fx+ i 1))) (else #f)))))

;; Advance past whitespace, commas, and ;-to-end-of-line comments.
;; EDN strict mode (clojure.edn): auto-resolved keywords are invalid, and each
;; discarded (#_) form is handed to rdr-discard-cb so the edn layer validates
;; its tagged elements through :readers/:default like the JVM.
(define rdr-edn-mode (make-parameter #f))
(define rdr-discard-cb (make-parameter #f))
;; Scan mode: reading source BEFORE any namespace is loaded (the build's
;; require scanner). An auto keyword whose alias isn't registered yet can't
;; resolve — in scan mode keep the alias text as the keyword's ns instead of
;; erroring; the scanner only extracts require clauses and discards every
;; other form, so the placeholder value is never observed.
(define rdr-scan-mode (make-parameter #f))
;; Suppress the :line/:column/:file metadata a list form carries. Set while
;; reading a form out of a string that is NOT the source being read — the
;; ~(…) inside a #$ interpolation, whose offsets are into the string literal.
;; Attaching them would report line 1 of the real file for a throw inside the
;; interpolated form, and would thrash rdr-line-col-at's per-string cursor back
;; and forth between the two strings. With no position of its own the form
;; inherits the enclosing form's, which is the right answer.
(define rdr-suppress-pos (make-parameter #f))

(define (rdr-skip-ws s i end)
  (let loop ((i i))
    (cond
      ((>= i end) i)
      ((rdr-ws? (string-ref s i)) (loop (+ i 1)))
      ((char=? (string-ref s i) #\;)
       (let eol ((j (+ i 1)))
         (if (or (>= j end) (char=? (string-ref s j) #\newline)
                 (char=? (string-ref s j) #\return))
             (loop j)
             (eol (+ j 1)))))
      (else i))))

;; --- numbers ----------------------------------------------------------------
;; A token is a number iff it (after an optional sign) starts with a digit and
;; parses. Ratios and big-N/M decimals use all-double rendering
;; for division; ints/bignums stay exact (Chez's tower IS Clojure's).
(define (rdr-string-index-char str c)
  (let ((n (string-length str)))
    (let loop ((i 0))
      (cond ((>= i n) #f)
            ((char=? (string-ref str i) c) i)
            (else (loop (+ i 1)))))))

;; Numeric tower (JVM parity): integer literals read as exact integers (= Long/
;; BigInt, arbitrary precision), a/b ratios as exact rationals (= Ratio), and
;; decimal/exponent literals as flonums (= double).
;; --- digit separators: 1_000_000 (issue #389) --------------------------------
;; A jolt superset: the JVM reader raises "Invalid number" on every token below,
;; so nothing that reads today changes meaning. Only a token that ALREADY starts
;; like a number is affected — a leading underscore is still an ordinary symbol
;; (_1 reads as the symbol _1 here exactly as on the JVM), because that decision
;; is made before this runs.
;;
;; The rule is Java's, which is the one someone writing 1_000_000 expects: an
;; underscore must sit BETWEEN TWO DIGITS of the literal. Never at either end,
;; never against the sign, the 0x / NrDDD radix marker, the decimal point, the
;; exponent marker, the ratio slash, or the N/M suffix. Being looser would be
;; easier — strip every underscore and let the parse decide — but that accepts
;; 0x_FF, 1e_5 and 2r_10, which reads as a second, stranger divergence to
;; explain. One rule, matching the language jolt models, is cheaper to document
;; than three special cases.
;;
;; "Digit" is per literal kind, so the marker characters are located FIRST and
;; excluded by position rather than by character class: r IS a digit in base 36
;; (36rR_Z is legal) and e IS a hex digit (0x1e_5 is legal), so a rule phrased
;; over characters alone gets both wrong.
(define (rdr-digit-sep-marker-positions body blen)
  ;; Positions in BODY that are structural markers, not digits: the x of a 0x
  ;; prefix, the r of a radix literal, and a trailing N/M suffix. The decimal
  ;; point, exponent marker, sign and slash are handled by the alphanumeric
  ;; test itself — none of them is alphanumeric except e/E, which is only a
  ;; marker when the token has no hex or radix prefix.
  (let* ((hex? (and (>= blen 2) (char=? (string-ref body 0) #\0)
                    (let ((c (string-ref body 1)))
                      (or (char=? c #\x) (char=? c #\X)))))
         (ri (and (not hex?)
                  (let loop ((i 0))
                    (cond ((>= i blen) #f)
                          ((let ((c (string-ref body i)))
                             (or (char=? c #\r) (char=? c #\R)))
                           i)
                          (else (loop (+ i 1)))))))
         (acc '()))
    (when hex? (set! acc (cons 1 acc)))
    (when (and ri (> ri 0)) (set! acc (cons ri acc)))
    ;; A trailing N (bigint) or M (bigdecimal) is a suffix, not a digit.
    (when (> blen 0)
      (let ((c (string-ref body (- blen 1))))
        (when (or (char=? c #\N) (char=? c #\M))
          (set! acc (cons (- blen 1) acc)))))
    ;; e/E is the exponent marker only in a plain decimal literal; in hex and in
    ;; base>14 radix literals it is a digit.
    (when (and (not hex?) (not ri))
      (let loop ((i 0))
        (when (< i blen)
          (let ((c (string-ref body i)))
            (when (or (char=? c #\e) (char=? c #\E)) (set! acc (cons i acc))))
          (loop (+ i 1)))))
    acc))

(define (rdr-digit-sep-alnum? c)
  (or (and (char>=? c #\0) (char<=? c #\9))
      (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))))

;; TOK with its underscores removed, or #f when any of them is misplaced. #f
;; reaches the caller's "starts like a number but does not parse" arm, which
;; raises NumberFormatException naming the original token — the same answer the
;; JVM gives, which is what a misplaced separator deserves.
(define (rdr-strip-digit-separators tok)
  (let* ((len (string-length tok))
         (c0 (string-ref tok 0))
         (start (if (or (char=? c0 #\+) (char=? c0 #\-)) 1 0))
         (body (substring tok start len))
         (blen (string-length body))
         (markers (rdr-digit-sep-marker-positions body blen)))
    (let loop ((i 0) (acc '()))
      (cond
        ((>= i blen)
         (string-append (substring tok 0 start)
                        (list->string (reverse acc))))
        ((char=? (string-ref body i) #\_)
         ;; The RUN of underscores starting here must be between two digits —
         ;; 5_______2 is legal, the same as 5_2 (JLS 3.10.1). So look past the
         ;; rest of the run for the right-hand neighbour rather than requiring
         ;; the very next character to be a digit.
         (let scan ((j i))
           (cond
             ((and (< j blen) (char=? (string-ref body j) #\_)) (scan (+ j 1)))
             (else
              ;; Both neighbours must exist, be alphanumeric, and not be one of
              ;; this literal's structural markers.
              (and (> i 0) (< j blen)
                   (rdr-digit-sep-alnum? (string-ref body (- i 1)))
                   (rdr-digit-sep-alnum? (string-ref body j))
                   (not (memv (- i 1) markers))
                   (not (memv j markers))
                   (loop j acc))))))
        (else (loop (+ i 1) (cons (string-ref body i) acc)))))))

(define (rdr-has-digit-separator? tok)
  (let ((n (string-length tok)))
    (let loop ((i 0))
      (cond ((>= i n) #f)
            ((char=? (string-ref tok i) #\_) #t)
            (else (loop (+ i 1)))))))

;; NOT in EDN. edn is an interchange format with a published grammar, and its
;; integers are [+-]?(0|[1-9][0-9]*)N? — no separators. jolt's PRINTER never
;; emits one (numbers print canonically), so jolt-written edn stays portable
;; either way; what accepting them would add is the other direction, a
;; hand-written deps.edn or config that reads here and fails in tools.deps or
;; any other edn reader. That is the trap edn strict mode already exists to
;; prevent — it is why #(), #= and auto-resolved keywords are refused too — so
;; a separator is refused with it, and the divergence stays confined to source
;; jolt reads for itself.
;; The separator retry, for a token that ALREADY failed to parse and already
;; looks like a number. It is deliberately NOT part of rdr-try-number: that runs
;; on every token in every source file, and a symbol fails the ordinary parse
;; too, so hanging the retry off failure alone made every symbol in the file pay
;; a parameter read and a scan of its own name to discover it is not a number.
;; Measured on a 2593-line file: 7.06 ms/read became 7.22.
;;
;; rdr-token->value already knows which failures are numeric-looking, so the
;; retry sits in that arm — on the path whose only other outcome is raising
;; "Invalid number". Nothing that reads today reaches it.
(define (rdr-try-separated-number tok)
  (and (not (rdr-edn-mode))
       (rdr-has-digit-separator? tok)
       (let ((stripped (rdr-strip-digit-separators tok)))
         (and stripped (rdr-try-number-raw stripped)))))

(define (rdr-try-number tok)
  (rdr-try-number-raw tok))

(define (rdr-try-number-raw tok)
  (let ((len (string-length tok)))
    (and (> len 0)
         (let* ((c0 (string-ref tok 0))
                (signed (or (char=? c0 #\+) (char=? c0 #\-)))
                (start (if signed 1 0)))
           (and (> len start)
                (rdr-digit? (string-ref tok start))
                (rdr-number-body tok start signed c0))))))

;; parse DDD in base `radix` (2..36); #f on a bad digit. Scheme string->number only
;; does radix 2/8/10/16, so Clojure's NrDDD (e.g. 36rZ) needs a manual parse.
(define (rdr-parse-radix digits radix)
  (let ((len (string-length digits)))
    (and (> len 0)
         (let loop ((i 0) (acc 0))
           (if (>= i len)
               acc
               (let* ((c (char-downcase (string-ref digits i)))
                      (d (cond ((and (char>=? c #\0) (char<=? c #\9)) (- (char->integer c) 48))
                               ((and (char>=? c #\a) (char<=? c #\z)) (+ 10 (- (char->integer c) 97)))
                               (else #f))))
                 (and d (< d radix) (loop (+ i 1) (+ (* acc radix) d)))))))))

(define (rdr-number-body tok start signed sign-ch)
  (let* ((sign (if (and signed (char=? sign-ch #\-)) -1 1))
         (len (string-length tok))
         (body (substring tok start len))
         (blen (string-length body))
         (slash (rdr-string-index-char body #\/)))
    (cond
      ;; ratio a/b -> exact rational (= JVM Ratio); reduces to an exact integer
      ;; when d divides n. Both parts must be plain digit runs (1/-1 is an
      ;; invalid token); a zero denominator is the JVM's divide error.
      (slash
       (let ((ns (substring body 0 slash))
             (ds (substring body (+ slash 1) blen)))
         (and (rdr-all-digits? ns 0 (string-length ns))
              (rdr-all-digits? ds 0 (string-length ds))
              (let ((n (string->number ns)) (d (string->number ds)))
                (when (= d 0)
                  (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))
                (* sign (/ n d))))))
       ;; hex 0x..
       ((and (>= blen 2) (char=? (string-ref body 0) #\0)
             (or (char=? (string-ref body 1) #\x) (char=? (string-ref body 1) #\X)))
        (let* ((raw (substring body 2 blen))
               (raw-len (string-length raw))
               (has-bigint (and (> raw-len 0) (char=? (string-ref raw (- raw-len 1)) #\N)))
               (digits (if has-bigint (substring raw 0 (- raw-len 1)) raw))
               (h (string->number digits 16)))
          (and h (* sign h))))
      ;; radix NrDDD (Clojure 2r1010 / 16rFF / 36rZ): N in decimal, DDD in base N
       ((let ((ri (or (rdr-string-index-char body #\r) (rdr-string-index-char body #\R))))
          (and ri (> ri 0) (< (+ ri 1) blen) ri))
        => (lambda (ri)
             (let* ((raw (substring body (+ ri 1) blen))
                    (raw-len (string-length raw))
                    (has-bigint (and (> raw-len 0) (char=? (string-ref raw (- raw-len 1)) #\N)))
                    (digits (if has-bigint (substring raw 0 (- raw-len 1)) raw))
                    (radix (string->number (substring body 0 ri))))
               (and radix (integer? radix) (>= radix 2) (<= radix 36)
                    (let ((v (rdr-parse-radix digits radix)))
                      (and v (* sign v)))))))
      ;; octal 0NNN, optionally with a bigint N suffix: a leading 0 followed by
      ;; octal digits (Clojure reads 042 as 34 and 0177N as octal 127, not the
      ;; decimal that the N-suffix branch below would otherwise produce). "0"
      ;; alone, 0x.., 0r.. and a float "0.5" are handled elsewhere or fall through.
      ((and (>= blen 2) (char=? (string-ref body 0) #\0)
            (let ((end (if (char=? (string-ref body (- blen 1)) #\N) (- blen 1) blen)))
              (and (> end 1) (rdr-all-octal? body 1 end) end)))
       => (lambda (end)
            (let ((o (rdr-parse-radix (substring body 1 end) 8))) (and o (* sign o)))))
      ;; a leading zero on a plain multi-digit integer is invalid (the octal
      ;; branch above accepted real octals; 08/09 match the JVM's trailing
      ;; "invalid number" alternative)
      ((and (>= blen 2) (char=? (string-ref body 0) #\0) (rdr-all-digits? body 1 blen))
       #f)
      ;; bigint suffix N — must be an exact integer (reject floats like 1e2N)
      ((and (> blen 1) (char=? (string-ref body (- blen 1)) #\N))
       (let ((n (string->number (substring body 0 (- blen 1)))))
         (and n (exact? n) (integer? n) (* sign n))))
      ;; bigdecimal suffix M -> a :bigdec form carrying the numeric text; the back
      ;; end lowers it to a runtime jbigdec.
      ((and (> blen 1) (char=? (string-ref body (- blen 1)) #\M))
       (let ((n (string->number (substring body 0 (- blen 1)))))
         (and n (real? n)
              (rdr-make-tagged (keyword #f "bigdec") (substring tok 0 (- len 1))))))
      (else
       (let ((n (string->number tok)))   ; tok carries its own sign
         ;; keep exactness: "42" -> exact int, "3.14"/"1e3" -> flonum.
         (and (number? n) (real? n) n))))))

;; --- string / char literals -------------------------------------------------
(define (rdr-hex->int s i n)            ; n hex digits at i -> (values int j)
  (let loop ((k 0) (acc 0) (j i))
    (if (= k n)
        (values acc j)
        (let ((d (and (< j (string-length s)) (rdr-hexdigit (string-ref s j)))))
          ;; a non-hex digit or a run shorter than n is a reader error with the
          ;; source position (ex-info), not a raw Chez "bad hex digit" condition.
          (if d
              (loop (+ k 1) (+ (* acc 16) d) (+ j 1))
              (rdr-error s (if (< j (string-length s)) j i)
                         "Invalid unicode escape (expected hex digits)"))))))

(define (rdr-hexdigit c)                 ; hex digit value, or #f if not a hex char
  (cond ((and (char>=? c #\0) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char>=? c #\a) (char<=? c #\f)) (+ 10 (- (char->integer c) 97)))
        ((and (char>=? c #\A) (char<=? c #\F)) (+ 10 (- (char->integer c) 65)))
        (else #f)))

;; opening quote already consumed; read to the closing quote, processing escapes.
(define (rdr-read-string-lit s i end)
  (let loop ((i i) (acc '()))
    (when (>= i end) (rdr-error s i "EOF while reading string"))
    (let ((c (string-ref s i)))
      (cond
        ((char=? c #\") (values (list->string (reverse acc)) (+ i 1)))
        ((char=? c #\\)
         (when (>= (+ i 1) end) (rdr-error s i "EOF while reading string"))
         (let ((e (string-ref s (+ i 1))))
           (case e
             ((#\n) (loop (+ i 2) (cons #\newline acc)))
             ((#\t) (loop (+ i 2) (cons #\tab acc)))
             ((#\r) (loop (+ i 2) (cons #\return acc)))
             ((#\\) (loop (+ i 2) (cons #\\ acc)))
             ((#\") (loop (+ i 2) (cons #\" acc)))
             ((#\b) (loop (+ i 2) (cons #\backspace acc)))
             ((#\f) (loop (+ i 2) (cons #\page acc)))
             ;; octal escape \ooo: 1-3 octal digits (Clojure's \0..\377), so \000
             ;; is one null char, not \0 + literal "00".
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
              (let oct ((j (+ i 1)) (val 0) (cnt 0))
                (if (and (fx<? cnt 3) (fx<? j end) (rdr-octal? (string-ref s j)))
                    (oct (fx+ j 1) (fx+ (fx* val 8) (fx- (char->integer (string-ref s j)) 48)) (fx+ cnt 1))
                    (begin
                      (when (> val 255)
                        (rdr-error s i "Octal escape sequence must be in range [0, 377]"))
                      (loop j (cons (integer->char val) acc))))))
              ((#\u)
               (when (>= (+ i 5) end) (rdr-error s i "EOF while reading string"))
               (let-values (((cp j) (rdr-hex->int s (+ i 2) 4)))
                 ;; A \u escape is a UTF-16 code unit. jolt chars are Unicode scalars,
                 ;; so combine a high+low surrogate pair into the one scalar char.
                 ;; Lone surrogates have no scalar — throw Invalid character constant
                 ;; (JVM-visible divergence, bead jolt-445k.34).
                 (cond
                    ((and (fx>=? cp #xD800) (fx<=? cp #xDBFF)
                          (fx<? (fx+ j 5) end)
                          (char=? (string-ref s j) #\\) (char=? (string-ref s (fx+ j 1)) #\u))
                    (let-values (((lo k) (rdr-hex->int s (+ j 2) 4)))
                      (if (and (fx>=? lo #xDC00) (fx<=? lo #xDFFF))
                          (loop k (cons (integer->char
                                         (fx+ #x10000 (fx* (fx- cp #xD800) 1024) (fx- lo #xDC00))) acc))
                          (rdr-error s i "Invalid character constant: \\u escape not followed by low surrogate"))))
                   ((and (fx>=? cp #xD800) (fx<=? cp #xDFFF))
                    (rdr-error s i "Invalid character constant: lone surrogate \\u escape"))
                   (else (loop j (cons (integer->char cp) acc))))))
             (else (rdr-error s i (string-append "Unsupported escape character: \\" (string e))
)))))
        (else (loop (+ i 1) (cons c acc)))))))

;; backslash already consumed; read a Clojure character literal.
(define (rdr-read-char s i end)
  (when (>= i end) (rdr-error s i "EOF while reading char"))
  (let ((c0 (string-ref s i)))
    (if (char-alphabetic? c0)
        ;; named / unicode / single-letter: collect the alnum run
        (let loop ((j (+ i 1)))
          (if (and (< j end)
                   (let ((c (string-ref s j)))
                     (or (char-alphabetic? c) (char-numeric? c))))
              (loop (+ j 1))
              (let ((name (substring s i j)))
                (if (= (string-length name) 1)
                    (values c0 j)
                    (values (rdr-named-char name) j)))))
        ;; any other single char (\(  \\  \;  \space-as-symbol handled above)
        (values c0 (+ i 1)))))

(define (rdr-named-char name)
  (cond
    ((string=? name "newline") #\newline)
    ((string=? name "space") #\space)
    ((string=? name "tab") #\tab)
    ((string=? name "return") #\return)
    ((string=? name "backspace") #\backspace)
    ((string=? name "formfeed") #\page)
    ((char=? (string-ref name 0) #\u)
      (let* ((hex (substring name 1 (string-length name)))
             (cp (string->number hex 16)))
        (cond
          ;; \uXXXX takes exactly 4 hex digits; a bad digit or wrong length is a
          ;; reader error (ex-info), not a raw integer->char crash on #f.
          ((not (= (string-length hex) 4))
           (rdr-error-here (keyword "read" "invalid-unicode")
                           (string-append "Invalid unicode character escape length: "
                                          (number->string (string-length hex)) ", should be: 4")))
          ((not cp)
           (rdr-error-here (keyword "read" "invalid-unicode")
                           (string-append "Invalid unicode character: \\u" hex)))
          ((and (>= cp #xD800) (<= cp #xDFFF))
           (rdr-error-here (keyword "read" "invalid-unicode")
                           "Invalid character constant: lone surrogate \\u escape"))
          (else (integer->char cp)))))
    ((char=? (string-ref name 0) #\o)
     (let ((v (string->number (substring name 1 (string-length name)) 8)))
       (when (or (not v) (> v 255))
         (rdr-error-here (keyword "read" "invalid-character")
                          "Octal escape sequence must be in range [0, 377]"))
       (integer->char v)))
    (else (rdr-error-here (keyword "read" "invalid-character")
                          (string-append "Unsupported character: \\" name)))))

;; --- token (symbol / keyword / number / nil|true|false) ---------------------
(define (rdr-read-token s i end)
  (let loop ((j i))
    (if (and (< j end) (not (rdr-terminator? (string-ref s j))))
        (loop (+ j 1))
        (values (substring s i j) j))))

;; split a "ns/name" token on the FIRST slash (a lone "/" is name "/")
(define (rdr-sym-parts tok)
  (let ((slash (rdr-string-index-char tok #\/)))
    (if (or (not slash) (= (string-length tok) 1) (= slash 0))
        (values #f tok)
        (values (substring tok 0 slash) (substring tok (+ slash 1) (string-length tok))))))

(define (rdr-numeric-lead? tok)
  (let ((len (string-length tok)))
    (and (> len 0)
         (let ((c0 (string-ref tok 0)))
           (or (rdr-digit? c0)
               (and (or (char=? c0 #\+) (char=? c0 #\-)) (> len 1)
                    (rdr-digit? (string-ref tok 1))))))))
(define (rdr-invalid-token tok)
  (rdr-error-here* "java.lang.RuntimeException"
                   (keyword "read" "invalid-token")
                   (string-append "Invalid token: " tok)))
(define (rdr-token->value tok)
  (let ((n (rdr-try-number tok)))
    (cond
      (n n)
      ;; a token that starts like a number but doesn't parse as one is an
      ;; invalid number (1a, 08, 0x2g, 2r2), never a symbol — like the JVM.
      ;; Except that a digit separator (1_000_000) lands here too, so the
      ;; retry goes in front of the raise: this is the only arm it can help,
      ;; and putting it anywhere earlier taxes tokens that can never benefit.
      ((rdr-numeric-lead? tok)
       (or (rdr-try-separated-number tok)
           (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                                            (string-append "Invalid number: " tok)))))
      ((string=? tok "nil") jolt-nil)
      ((string=? tok "true") #t)
      ((string=? tok "false") #f)
      (else
       (let ((len (string-length tok)))
         ;; a lone "/" is the division symbol, and "ns//" names it in a
         ;; namespace (clojure.core//); otherwise a leading or trailing slash
         ;; leaves an empty ns/name part — an invalid token.
         (when (and (> len 1)
                    (or (char=? (string-ref tok 0) #\/)
                        (and (char=? (string-ref tok (- len 1)) #\/)
                             (not (and (> len 2) (char=? (string-ref tok (- len 2)) #\/))))))
           (rdr-invalid-token tok))
         (let-values (((ns name) (rdr-sym-parts tok))) (jolt-symbol ns name)))))))

;; --- collections ------------------------------------------------------------
;; Read forms until the close delimiter; returns (values reversed?-no list j).
(define (rdr-read-seq s i end close)
  (let loop ((i i) (acc '()))
    (let ((i (rdr-skip-ws s i end)))
      (cond
        ((>= i end) (rdr-error s i "EOF while reading"))
        ((char=? (string-ref s i) close) (values (reverse acc) (+ i 1)))
        (else
         (let-values (((form j) (rdr-read-form s i end)))
           (cond
             ;; A close delimiter that is not the one this collection is waiting
             ;; for comes back UNCONSUMED (j = i). Treating that as a discard
             ;; re-reads the same character forever, so a file with a mismatched
             ;; delimiter hung the loader instead of failing: the only symptom was
             ;; that loading never returned, which reads as a hang, not a syntax
             ;; error. Any no-progress result is an error here, reported at its
             ;; position like the JVM's.
             ((and (rdr-eof? form) (= j i))
              (rdr-error s i (string-append "Unmatched delimiter: " (string (string-ref s i)))))
             ((rdr-eof? form) (loop j acc))   ; a #_ discard or no-match #? — re-check at j
             ((rdr-splice-t? form)            ; #?@ — splice the matched collection's items
              (loop j (append (reverse (rdr-splice-t-items form)) acc)))
             (else (loop j (cons form acc))))))))))

;; Map literals must preserve SOURCE key order so the analyzer emits the value
;; expressions in source order (Clojure guarantees left-to-right map-literal eval).
;; A pmap is hash-ordered, so record each reader-built map's (k1 v1 k2 v2 ...) form
;; sequence in a weak side-table the host contract's form-map-pairs consults.
;; Both reader side-tables are weak-eq, and a weak-eq table cannot be read
;; unlocked while another thread writes it: Chez's eq adjust! relinks live cells
;; in place with $set-tlc-next! (s/library.ss) while a reader may be walking them,
;; and the reader is unsafe primitive code, so a resize concurrent with a lookup
;; hangs or faults. Two threads reading source at once is ordinary here (an nREPL
;; session evaluating while the loader requires, two requires in flight), so both
;; go through one mutex.
;;
;; A mutex and not copy-on-write, unlike records.ss's ctor-tag table: this one is
;; WRITTEN once per map literal read, so copying per write would be quadratic in
;; the size of the source being read. The lock is noise against the read + analyze
;; it sits inside.
(define rdr-side-mu (make-mutex))
(define rdr-map-order (make-weak-eq-hashtable))
(define (rdr-map-order-ref m) (jolt-with-mutex rdr-side-mu (hashtable-ref rdr-map-order m #f)))
(define (rdr-map-order-set! m es) (jolt-with-mutex rdr-side-mu (hashtable-set! rdr-map-order m es)))
(define (rdr-make-map es)
  ;; An odd literal is the READER's error, reported here rather than left to the
  ;; map constructor. Deferring to it gave "odd number of map literal entries"
  ;; from collections.ss — a message about a constructor argument, carrying no
  ;; kind and no position, so the report pointed at the top of the file. The
  ;; reference names the literal: "Map literal must contain an even number of
  ;; forms". rdr-at at the call site turns the position in.
  (when (and (not (rdr-scan-mode)) (odd? (length es)))
    (rdr-error-here* "java.lang.RuntimeException"
                     (keyword "read" "odd-entries-in-map")
                     "Map literal must contain an even number of forms"))
  ;; the JVM reader rejects duplicate literal keys before building the map.
  (let dupchk ((kvs (and (not (rdr-scan-mode)) es)) (seen empty-pset))
    (when (and (pair? kvs) (pair? (cdr kvs)))
      (let ((k (car kvs)))
        (when (jolt-truthy? (jolt-contains? seen k))
          (rdr-error-here* "java.lang.IllegalArgumentException"
                           (keyword "read" "duplicate-key")
                           (string-append "Duplicate key: " (jolt-pr-str k))))
        (dupchk (cddr kvs) (pset-conj seen k)))))
  (let ((m (apply jolt-hash-map es)))
    (when (pair? es) (rdr-map-order-set! m es))
    m))

;; Same guard as rdr-make-map, for the same reason: the JVM reader rejects a
;; duplicate literal element before building the set (PersistentHashSet
;; .createWithCheck), so `#{1 1}` is a read error, not a one-element set. Only the
;; DATA path checked here, in rdr-form->data, so `#{1 1}` read as data threw while
;; the same literal compiled in a source file quietly became #{1} — and a build,
;; which re-reads every file as data to scan its requires, then failed on a file
;; that had loaded fine. Elements are compared as read, like the JVM: two symbols
;; are distinct, two equal lists are not.
;;
;; Never in scan mode. There an auto keyword whose alias can't resolve yet keeps
;; the ALIAS TEXT as its namespace, so `#{::o/x :o/x}` reads as two copies of
;; :o/x — a collision that exists only in the placeholder, since at load those are
;; :app.other/x and :o/x. The scanner discards every form but the require clauses,
;; so checking there rejects a program that is perfectly good; it is why a build
;; of exactly that set literal failed while the namespace loaded fine.
(define (rdr-make-set elems)
  (let dupchk ((xs (and (not (rdr-scan-mode)) elems)) (seen empty-pset))
    (when (pair? xs)
      (when (jolt-truthy? (jolt-contains? seen (car xs)))
        (rdr-error-here* "java.lang.IllegalArgumentException"
                         (keyword "read" "duplicate-key")
                         (string-append "Duplicate key: " (jolt-pr-str (car xs)))))
      (dupchk (cdr xs) (pset-conj seen (car xs)))))
  (jolt-hash-map rdr-kw-jolt-type rdr-kw-jolt-set
                 rdr-kw-value (apply jolt-vector elems)))

(define (rdr-make-tagged tag form)
  (jolt-hash-map rdr-kw-jolt-type rdr-kw-jolt-tagged
                 rdr-kw-tag tag rdr-kw-form form))

;; --- metadata ---------------------------------------------------------------
(define (rdr-meta-map m)
  (cond
    ((keyword? m) (jolt-hash-map m #t))
    ;; ^Type -> {:tag Type} with the SYMBOL (Clojure parity — core.match's
    ;; array-tag and other libs look the tag up as a symbol; jolt's tag consumers
    ;; tolerate a symbol). ^"Type" keeps the string.
    ((symbol-t? m) (jolt-hash-map rdr-kw-tag m))
    ((string? m) (jolt-hash-map rdr-kw-tag m))
    ((pmap? m) m)
    (else (jolt-hash-map rdr-kw-tag m))))

(define (rdr-merge-meta old new)
  (if (pmap? old)
      (pmap-fold-fwd new (lambda (k v acc) (jolt-assoc1 acc k v)) old)
      new))

(define (rdr-attach-meta target meta)
  (cond
    ((symbol-t? target)
     ;; symbol-t-with-meta (values.ss): carries the khash over, since ns/name are
     ;; unchanged and only the metadata differs.
     (symbol-t-with-meta target (rdr-merge-meta (symbol-t-meta target) meta)))
    ;; Lists/vectors/maps/sets attach metadata to the value itself, as Clojure's
    ;; reader does. Reading DATA (read-string, edn) then preserves it. A list form
    ;; is code: ^Type (expr) is a compile-time hint on the FORM, read off the form
    ;; for :tag and discarded at runtime (a hint on an evaluated form is dropped).
    ;; A vector/map/set LITERAL keeps it as a runtime value: the analyzer re-emits a
    ;; (with-meta form meta) for a meta-carrying collection literal in code, so
    ;; (meta ^{:tag :int} [1 2]) / ^:foo {} still works.
    (else
     ;; Merge onto any metadata the target already carries (a list form picks up
     ;; :line/:column first, then ^meta folds its keys on top).
     (let* ((old (jolt-meta target))
            (merged (rdr-merge-meta (if (jolt-nil? old) jolt-nil old) meta))
            (c (jolt-with-meta target merged)))
       ;; jolt-with-meta copies a pmap, giving it a fresh identity the rdr-map-order
       ;; side-table (source key order for left-to-right map-literal eval) loses —
       ;; carry the order entry over to the copy.
       (let ((order (and (pmap? target) (rdr-map-order-ref target))))
         (when order (rdr-map-order-set! c order)))
       ;; same for the record-literal ctor mark (^:foo #ns.Type[1] copies the form).
       (when (rdr-ctor-call? target) (rdr-mark-ctor-form c))
       c))))

;; --- source position --------------------------------------------------------
;; List forms (code) carry 1-based :line/:column, plus :file when the compiler
;; bound rdr-source-file. read-string leaves the file unset. The analyzer reads
;; this back via jolt.host/form-position to stamp :pos on call nodes; macros and
;; (meta (read-string "(…)")) see it too.
(define rdr-source-file (make-thread-parameter #f))
(define rdr-kw-line   (keyword #f "line"))
(define rdr-kw-column (keyword #f "column"))
(define rdr-kw-file   (keyword #f "file"))

;; Forms are read left-to-right, so the indices queried are non-decreasing within
;; one source string — keep a cursor and count newlines only over the delta
;; (O(n) total, not O(n^2)). A different string or a backward index resets it.
(define rdr-pos-cursor (make-thread-parameter #f))   ; #f | (vector s i line col)
(define (rdr-line-col-at s i)
  (let* ((cur (rdr-pos-cursor))
         (reuse (and (vector? cur) (eq? (vector-ref cur 0) s)
                     (fx<=? (vector-ref cur 1) i)))
         (k0 (if reuse (vector-ref cur 1) 0))
         (l0 (if reuse (vector-ref cur 2) 1))
         (c0 (if reuse (vector-ref cur 3) 1)))
    (let loop ((k k0) (line l0) (col c0))
      (if (fx>=? k i)
          (begin (rdr-pos-cursor (vector s k line col)) (values line col))
          (if (char=? (string-ref s k) #\newline)
              (loop (fx+ k 1) (fx+ line 1) 1)
              (loop (fx+ k 1) line (fx+ col 1)))))))

(define (rdr-pos-meta line col)
  (let ((f (rdr-source-file)))
    (if f
        (jolt-hash-map rdr-kw-line line rdr-kw-column col rdr-kw-file f)
        (jolt-hash-map rdr-kw-line line rdr-kw-column col))))

;; A read diagnostic, shaped like the analyzer's: the message alone, and the KIND
;; plus position, as flat :jolt.error/* keys. Three things follow from that.
;;
;; The position is no longer glued onto the message text. It used to read
;; "msg (file:line:col)" while the reporter printed its own "at file:line:col"
;; line underneath, and the two disagreed — an unmatched delimiter said 2:20 in
;; the message and 2:1 on the at line, because only one of them was the token's.
;;
;; The reporter suppresses the backtrace for anything carrying a kind, so
;; read errors stop printing ten frames of rdr-read-form / rdr-read-seq internals
;; that name nothing a reader can act on.
;;
;; And the kind is registered in test/conformance/error-kinds.edn like every
;; other, so `make errorkinds` covers the reader too.
(define rdr-kw-err-kind (keyword "jolt.error" "kind"))
(define rdr-kw-err-type (keyword "jolt.error" "type"))
(define rdr-kw-err-line (keyword "jolt.error" "line"))
(define rdr-kw-err-column (keyword "jolt.error" "column"))
(define rdr-kw-err-file (keyword "jolt.error" "file"))
(define rdr-kw-read-error (keyword #f "read-error"))

;; One FLAT namespaced shape. Namespacing, not nesting, is what keeps these from
;; colliding with the thrower's own ex-data (which the analyzer preserves), so
;; there is no wrapper map and no second copy of the position to drift out of
;; step with the first. The reference spells its own the same way
;; (:clojure.error/line).
(define (rdr-diagnostic-data kind line col)
  (let* ((f (rdr-source-file))
         (m (jolt-hash-map rdr-kw-err-kind kind
                           rdr-kw-err-type rdr-kw-read-error
                           rdr-kw-err-line line
                           rdr-kw-err-column col)))
    (if f (jolt-assoc m rdr-kw-err-file f) m)))

(define (rdr-error-kind s i kind msg)
  (let-values (((line col) (rdr-line-col-at s i)))
    (jolt-throw (jolt-ex-info msg (rdr-diagnostic-data kind line col)))))

;; The kindless spelling, kept so the 37 existing call sites read unchanged while
;; they are given kinds one at a time.
(define (rdr-error s i msg)
  (rdr-error-kind s i (keyword "read" "invalid-syntax") msg))

;; Raise a read diagnostic that has NO position, for the helpers that take a
;; parsed value rather than a source index — rdr-make-map, rdr-make-set,
;; rdr-token->value and friends never see s or i, which is why they never used
;; rdr-error. rdr-at (below) gives it one as it escapes.
;;
;; CLASS is the throwable class, kept because several of these match a JVM class
;; a program can catch: a duplicate map key is an IllegalArgumentException on both
;; runtimes, and turning it into an ExceptionInfo to carry a kind would break
;; every (catch IllegalArgumentException ...) around a read.
(define (rdr-error-here* class kind msg)
  ;; make-jolt-ex-info-record directly, NOT jolt-host-throwable: that helper's
  ;; optional third argument is the CAUSE, not the data (the record's fields are
  ;; class-name message cause data error-offset), so passing the diagnostic there
  ;; silently filed it as a cause and left data nil — the report kept falling back
  ;; to the top-level position with no kind at all.
  (jolt-throw (make-jolt-ex-info-record
               class msg jolt-nil
               (jolt-hash-map rdr-kw-err-kind kind
                              rdr-kw-err-type rdr-kw-read-error)
               0)))

(define (rdr-error-here kind msg)
  (rdr-error-here* "clojure.lang.ExceptionInfo" kind msg))

;; Run THUNK, and if it raises a read diagnostic with no position, fill in the one
;; at index I. Only the CALLER knows where the form started, so this is where the
;; position lives; threading s and i through a dozen helper signatures would put
;; the cost on every read instead of on the raise. The guard is per collection
;; literal / per token, not per form, and it does nothing until something throws:
;; the reader's hot path is untouched.
(define (rdr-at s i thunk)
  (guard (e ((rdr-positionless-read-error? e)
             (let-values (((line col) (rdr-line-col-at s i)))
               (let ((v (jolt-unwrap-throw e)))
                 ;; The CLASS is carried over, not rebuilt as an ExceptionInfo:
                 ;; adding a position must not change what a catch clause matches.
                 (jolt-throw
                  (make-jolt-ex-info-record
                   (jolt-ex-info-record-class-name v)
                   (jolt-ex-info-record-message v)
                   (jolt-ex-info-record-cause v)
                   (rdr-diagnostic-data (rdr-read-error-kind e) line col)
                   0))))))
    (thunk)))

(define (rdr-read-error-of e)
  (let ((v (jolt-unwrap-throw e)))
    (and (jolt-ex-info-record? v)
         (let ((d (jolt-ex-info-record-data v)))
           (and (pmap? d)
                (eq? (jolt-get d rdr-kw-err-type jolt-nil) rdr-kw-read-error)
                d)))))

(define (rdr-positionless-read-error? e)
  (let ((d (rdr-read-error-of e)))
    (and d (jolt-nil? (jolt-get d rdr-kw-err-line jolt-nil)))))

(define (rdr-read-error-kind e)
  (let ((d (rdr-read-error-of e)))
    (if d (jolt-get d rdr-kw-err-kind jolt-nil) jolt-nil)))

(define (rdr-attach-pos lst line col)
  (if (empty-list-t? lst)            ; () is interned, can't carry meta (= Clojure)
      lst
      (rdr-attach-meta lst (rdr-pos-meta line col))))

;; --- # dispatch -------------------------------------------------------------
;; #(...) anonymous fn shorthand: % -> p1, %N -> pN, %& -> rest. The
;; fixed arity is the MAX positional used (Clojure: #(do %2 %&) -> [p1 p2 & rest]).
;; Param names carry a trailing "#" so a #() inside a syntax-quote still reads them
;; as auto-gensyms.
;; The bump and the read are one step, like every other name counter in the
;; runtime: unlocked, two threads reading a form at once draw the same number and
;; the "#" auto-gensym these mint is no longer unique. See jolt-gensym.
(define rdr-anon-counter 0)
(define rdr-name-counter-mutex (make-mutex))
(define (rdr-anon-gensym)
  (jolt-symbol #f (string-append "p__"
                                 (number->string
                                  (jolt-with-mutex rdr-name-counter-mutex
                                    (set! rdr-anon-counter (+ rdr-anon-counter 1))
                                    rdr-anon-counter))
                                 "#")))
(define (rdr-pct-index nm)               ; % ->1, %& ->'rest, %N ->N, else #f
  (cond ((string=? nm "%") 1)
        ((string=? nm "%&") 'rest)
        ((and (> (string-length nm) 1) (char=? (string-ref nm 0) #\%))
         (let ((n (string->number (substring nm 1 (string-length nm)))))
           (if (and n (integer? n) (>= n 1)) n #f)))
        (else #f)))
(define (rdr-anon-set? f) (and (pmap? f) (eq? (jolt-get f rdr-kw-jolt-type) rdr-kw-jolt-set)))
(define (rdr-anon-scan f max-box rest-box)
  (cond
    ((symbol-t? f)
     (let ((idx (rdr-pct-index (symbol-t-name f))))
       (cond ((eq? idx 'rest) (set-box! rest-box #t))
             ((and idx (> idx (unbox max-box))) (set-box! max-box idx)))))
    ((or (pvec? f) (cseq? f) (empty-list-t? f))
     (for-each (lambda (x) (rdr-anon-scan x max-box rest-box)) (seq->list f)))
    ((rdr-anon-set? f)
     (for-each (lambda (x) (rdr-anon-scan x max-box rest-box)) (seq->list (jolt-get f rdr-kw-value))))
    ((pmap? f)
     (for-each (lambda (x) (rdr-anon-scan x max-box rest-box)) (or (rdr-map-order-ref f) '())))))
;; rdr-anon-replace REBUILDS every collection it walks, and a rebuilt collection
;; is a fresh object — so all reader metadata on a nested literal was dropped.
;; #() therefore stripped ^meta from every vector/map/set in its body at any
;; depth, which is how #((fn ^long [x] x) v) lost its return hint: the ^long
;; lives on the arglist VECTOR, and that vector got rebuilt. (meta ^{:a 1} [1 2])
;; read nil inside #() and {:a 1} outside it.
;;
;; Carry the metadata across as a FORM — unlike rdr-carry-meta, which converts it
;; to data for a value position; here the body is still code to be analyzed. A
;; pmap copy loses its rdr-map-order side-table entry (source key order for
;; left-to-right literal eval), so carry that too, the way the ^meta reader does.
(define (rdr-anon-keep-meta src dst)
  (let ((m (jolt-meta src)))
    (if (jolt-nil? m)
        dst
        (let ((c (jolt-with-meta dst m)))
          (let ((order (and (pmap? dst) (rdr-map-order-ref dst))))
            (when order (rdr-map-order-set! c order)))
          c))))

(define (rdr-anon-replace f slots rest-sym)
  (cond
    ((symbol-t? f)
     (let ((idx (rdr-pct-index (symbol-t-name f))))
       (cond ((eq? idx 'rest) rest-sym) (idx (vector-ref slots (- idx 1))) (else f))))
    ((pvec? f)
     (rdr-anon-keep-meta f (apply jolt-vector (map (lambda (x) (rdr-anon-replace x slots rest-sym)) (seq->list f)))))
    ((or (cseq? f) (empty-list-t? f))
     (rdr-anon-keep-meta f (apply jolt-list (map (lambda (x) (rdr-anon-replace x slots rest-sym)) (seq->list f)))))
    ((rdr-anon-set? f)
     (rdr-anon-keep-meta f (rdr-make-set (map (lambda (x) (rdr-anon-replace x slots rest-sym)) (seq->list (jolt-get f rdr-kw-value))))))
    ((pmap? f)
     (let ((kv (rdr-map-order-ref f)))
       (if kv (rdr-anon-keep-meta f (rdr-make-map (map (lambda (x) (rdr-anon-replace x slots rest-sym)) kv))) f)))
    (else f)))
(define (rdr-read-anon-fn s i end)       ; i at the '(' after '#'
  (let-values (((form j) (rdr-read-form s i end)))
    (let ((max-box (box 0)) (rest-box (box #f)))
      (rdr-anon-scan form max-box rest-box)
      (let* ((n (unbox max-box))
             (slots (make-vector n)))
        (let loop ((k 0)) (when (< k n) (vector-set! slots k (rdr-anon-gensym)) (loop (+ k 1))))
        (let* ((rest-sym (if (unbox rest-box) (rdr-anon-gensym) #f))
               (body (rdr-anon-replace form slots rest-sym))
               (params (append (vector->list slots)
                               (if rest-sym (list (jolt-symbol #f "&") rest-sym) '()))))
          (values (jolt-list (jolt-symbol #f "fn*") (apply jolt-vector params) body) j))))))

;; reader conditionals: jolt's feature set is {:jolt :bb :clj :default};
;; the FIRST clause whose feature key is in the set wins (clause order, like
;; Clojure). jolt is a Clojure/JVM-compatible host — it emulates clojure.lang.*
;; and java.* interop — so it reads the :clj branch of a .cljc library (the JVM
;; code path its host shims target), not the :cljs one. :bb is also in the set,
;; like babashka itself: a library's :bb branch solves the same non-JVM problems
;; jolt has (no reflection, no JVM-only classes), and libraries list it ahead of
;; :clj precisely so a bb-like host takes it. A library can still override with
;; a :jolt-specific branch (place it before :bb/:clj).
(define rdr-features '("jolt" "bb" "clj" "default"))
(define (rdr-feature? kw)
  (and (keyword? kw) (jolt-nil? (let ((n (keyword-t-ns kw))) (if n n jolt-nil)))
       (and (member (keyword-t-name kw) rdr-features) #t)))
(define (rdr-read-reader-cond s i end)   ; i is past the '?'
  (let* ((splice (and (< i end) (char=? (string-ref s i) #\@)))
         (start (if splice (+ i 1) i)))
    (let-values (((form j) (rdr-read-form s start end)))
      (when (rdr-eof? form) (rdr-error s i "EOF after #?"))
      (let ((items (cond ((pvec? form) (seq->list form))
                         ((or (cseq? form) (empty-list-t? form)) (seq->list form))
                         (else '()))))
        (let loop ((xs items))
          (cond ((or (null? xs) (null? (cdr xs))) (values rdr-eof j))  ; no match -> discard
                ((rdr-feature? (car xs))
                 (if splice
                     ;; #?@ — the matched value is a collection whose ITEMS splice
                     ;; into the enclosing sequence (read-seq expands the wrapper).
                     (let ((v (cadr xs)))
                       (values (make-rdr-splice-t
                                 (cond ((pvec? v) (seq->list v))
                                       ((or (cseq? v) (empty-list-t? v)) (seq->list v))
                                       (else (list v))))
                               j))
                     (values (cadr xs) j)))
                (else (loop (cddr xs)))))))))

(define (rdr-string-rindex-char str c)
  (let loop ((i (- (string-length str) 1)))
    (cond ((< i 0) #f) ((char=? (string-ref str i) c) i) (else (loop (- i 1))))))

;; A record/type literal tag (#ns.Type{..} / #ns.Type[..]) is any tag containing
;; a dot — Clojure routes those to a constructor instead of a data reader.
(define (rdr-record-tag? tok) (and (rdr-string-rindex-char tok #\.) #t))

;; Is v a factory-call form the reader itself built for a #ns.Type{...}/[...]
;; literal? Recorded by identity in a weak side-table, NOT recognized by the head
;; symbol's name: ordinary code calls a qualified ->name too ((u/->long n) in
;; jolt.time), and the data path applies a ctor form, so a name test would try to
;; apply an unbound var at read time.
(define rdr-ctor-forms (make-weak-eq-hashtable))   ; under rdr-side-mu, see above
(define (rdr-mark-ctor-form v) (jolt-with-mutex rdr-side-mu (hashtable-set! rdr-ctor-forms v #t)) v)
(define (rdr-ctor-call? v)
  (and (cseq? v) (jolt-with-mutex rdr-side-mu (hashtable-ref rdr-ctor-forms v #f)) #t))

;; Is v a tagged-literal pmap (#inst/#uuid/#regex/#bigdec at read time)?
(define (rdr-tagged-form? v)
  (and (pmap? v) (eq? (jolt-get v rdr-kw-jolt-type) rdr-kw-jolt-tagged)))

;; Recursively datafy a VALUE inside a record literal: quote plain data (symbols,
;; data lists), keep nested record-ctor calls and value-constructors evaluating.
(define (rdr-datafy v)
  (cond
   ((rdr-ctor-call? v) v)               ; nested record ctor — keep evaluating
   ((rdr-tagged-form? v) v)             ; #inst/#uuid/#regex/#bigdec — keep
   ((and (pmap? v) (eq? (jolt-get v rdr-kw-jolt-type) rdr-kw-jolt-set)) v)  ; #{…} set — keep
   ((pmap? v)
    (let ((kv (rdr-map-order-ref v)))
      (if kv
          (rdr-make-map
           (let loop ((kvs kv))
             (if (null? kvs) '()
                 (cons (car kvs)
                       (cons (rdr-datafy (cadr kvs))
                             (loop (cddr kvs)))))))
          (apply jolt-hash-map
                 (pmap-fold v (lambda (k val a)
                                (cons k (cons (rdr-datafy val) a)))
                            '())))))
   ((pvec? v)
    (apply jolt-list
           (jolt-symbol "clojure.core" "vector")
           (map rdr-datafy (vector->list (pvec-v v)))))
   ((or (cseq? v) (empty-list-t? v))
    (apply jolt-list
           (jolt-symbol "clojure.core" "list")
           (map rdr-datafy (seq->list v))))
   ((or (keyword? v) (string? v) (number? v) (boolean? v) (jolt-nil? v) (char? v))
    v)                                   ; self-evaluating — as-is
   (else
    (jolt-list (jolt-symbol #f "quote") v)))) ; symbol or other — quote it

;; #a.b.C{..} -> (a.b/map->C {:keys (datafy vals)...})
;; #a.b.C[..] -> (a.b/->C  (datafy val)...).  The factory call compiles like any
;; invoke; defrecord interns map->C/->C in the type's ns.
(define (rdr-record-ctor-form tok form)
  ;; The JVM's reader resolves the CLASS the literal names, and the JVM spelling
  ;; of a type in a dashed namespace is munged (#my_app.core.Foo{…} — what the
  ;; JVM printed it as). The class graph maps that back to the registered name,
  ;; whose namespace holds the factory; a name it does not know (the type's ns
  ;; not loaded yet) is taken as written, as before.
  (let* ((tok (or (jch-registered-name tok) tok))
         (di (rdr-string-rindex-char tok #\.))
         (ns (substring tok 0 di))
         (simple (substring tok (+ di 1) (string-length tok))))
    (cond
      ((pmap? form)
       (rdr-mark-ctor-form
        (jolt-list (jolt-symbol ns (string-append "map->" simple))
                   (rdr-datafy form))))
      ((pvec? form)
       (rdr-mark-ctor-form
        (apply jolt-list (jolt-symbol ns (string-append "->" simple))
               (map rdr-datafy (vector->list (pvec-v form))))))
      (else (jolt-throw (jolt-ex-info
                         (string-append "Unreadable constructor form: #" tok)
                         empty-pmap))))))

;; #:ns{…} namespaced map literal: a bare keyword/symbol key gets `ns`, a `:_/x`
;; key is un-namespaced, an already-qualified key stays. #::{…} uses the current
;; ns; #::alias{…} resolves the alias.
(define (rdr-nsmap-key mapns k)
  (cond
    ((keyword? k)
     (let ((kns (keyword-t-ns k)) (kn (keyword-t-name k)))
       (cond ((and (string? kns) (string=? kns "_")) (keyword #f kn))
             (kns k)
             (else (keyword mapns kn)))))
    ((symbol-t? k)
     (let ((kns (symbol-t-ns k)) (kn (symbol-t-name k)))
       (cond ((and (string? kns) (string=? kns "_")) (jolt-symbol #f kn))
             (kns k)
             (else (jolt-symbol mapns kn)))))
    (else k)))
(define (rdr-nsmap-kvs mapns es)
  (cond ((null? es) '())
        ((null? (cdr es)) es)
        (else (cons (rdr-nsmap-key mapns (car es))
                    (cons (cadr es) (rdr-nsmap-kvs mapns (cddr es)))))))
(define (rdr-simple-symbol-token? tok)
  (guard (e (#t #f))
    (let ((v (rdr-token->value tok)))
      (and (symbol-t? v) (not (symbol-t-ns v))))))
(define (rdr-read-ns-map s i end)        ; i points just past "#:"
  (let* ((auto? (and (< i end) (char=? (string-ref s i) #\:)))
         (i2 (if auto? (+ i 1) i))
         ;; Reader whitespace (a comma included) directly after the prefix means
         ;; no namespace token was written at all. Only `#::` may still reach a
         ;; map from there, naming the current namespace.
         (spaced? (and (< i2 end) (rdr-ws? (string-ref s i2)))))
    ;; The namespace is a token, not every byte up to the opening brace.
    ;; Whitespace may separate that token from the map; anything else at the
    ;; boundary is an error rather than part of the namespace or a non-map
    ;; payload borrowing a later opening brace.
    (let-values (((nstok j) (rdr-read-token s i2 end)))
      (when (and spaced? (not auto?))
        (rdr-error s i2 "Namespaced map must specify a namespace"))
      (let skip-boundary ((k j))
        (cond
          ((and (< k end) (rdr-ws? (string-ref s k)))
           (skip-boundary (+ k 1)))
          ((or (>= k end) (not (char=? (string-ref s k) #\{)))
           ;; A comment is a reader macro rather than whitespace at this exact
           ;; boundary on the JVM, so do not use rdr-skip-ws (which skips it).
           ;; A missing map is also reported BEFORE the namespace token is
           ;; judged, which is the order the JVM reports these two in.
           (rdr-error s k (if spaced?
                              "Namespaced map must specify a namespace"
                              "Namespaced map must specify a map")))
          (else
           ;; Both prefix forms require a simple, unqualified symbol here; only
           ;; the auto form may leave the token out, naming the current ns.
           ;; The JVM renders the offending namespace the way Java prints the
           ;; object it read, so an absent one reads as "null" there and as
           ;; "nil" here. Reading a TOKEN rather than a whole form is what
           ;; makes a non-map payload fail closed, and it is why the two
           ;; cannot agree on every degenerate spelling (`#:"s"{}` names the
           ;; string on the JVM and no token at all here) — so report the
           ;; value jolt actually has rather than imitate Java's printer.
           (when (if (string=? nstok "")
                     (not auto?)
                     (not (rdr-simple-symbol-token? nstok)))
             (rdr-error s i2 (string-append "Namespaced map must specify a valid namespace: "
                                            (if (string=? nstok "") "nil" nstok))))
           (let ((mapns (if auto?
                            (if (string=? nstok "") (chez-current-ns)
                                (let ((a (chez-resolve-alias (chez-current-ns) nstok)))
                                  (cond (a a)
                                        ;; The build's require/class-provider scans
                                        ;; read every top-level form before the ns
                                        ;; declaration has installed its aliases.
                                        ;; Preserve the alias spelling just as the
                                        ;; ::alias/keyword reader does below; scan
                                        ;; mode discards the value after extracting
                                        ;; dependency names.
                                        ((rdr-scan-mode) nstok)
                                        (else
                                         (rdr-error s i2 (string-append
                                                          "Unknown auto-resolved namespace alias: " nstok))))))
                            nstok)))
             (let-values (((es next) (rdr-read-seq s (+ k 1) end #\})))
               (values (rdr-make-map (rdr-nsmap-kvs mapns es)) next)))))))))

;; *read-eval* gate for #= — the cell is captured lazily (the var is def'd
;; after this file loads) and the value read per use, so a
;; (binding [*read-eval* false] (read-string ...)) holds.
(define rdr-read-eval-cell #f)
(define (rdr-read-eval?)
  (unless rdr-read-eval-cell
    (set! rdr-read-eval-cell (jolt-var "clojure.core" "*read-eval*")))
  (let ((v (jolt-var-get rdr-read-eval-cell)))
    (and v (not (jolt-nil? v)))))

;; --- user reader macros -----------------------------------------------------
;; Clojure's dispatch table is closed: after a `#`, the reader claims a fixed set
;; of characters, a letter starts a data-reader tag, and everything else is a read
;; error ("No dispatch macro for: $"). jolt owns its reader, so the punctuation
;; half of that table is open here — a program registers a reader for one
;; character and `#<c>` from then on reads through it. It is the seam the two
;; things Clojure never shipped both need: string interpolation (`#$"…"` below)
;; and user-defined reader macros. See stdlib/jolt/reader.clj.
;;
;; Two tiers, the same shape as jolt.host/extend-class!:
;;   form tier  (fn [form]) -> form         the next form is read normally and the
;;                                          registered fn rewrites it
;;   raw  tier  (fn [src i]) -> [form j]    the fn reads the SOURCE itself, from
;;                                          index i, and says where it stopped
;; The raw tier is for a literal whose body is not Clojure data (a raw string, a
;; heredoc); the form tier covers everything else. Either way a registration is
;; a runtime call and jolt reads a file one top-level form at a time, so a file
;; can register a macro and use it below — and `jolt build` loads the app from
;; source before it scans it, so a build reads what a run reads.
;;
;; A character the reader itself claims can never be registered, and neither can
;; a letter or a digit — those begin a data-reader tag, so a `#s` reader would
;; silently swallow every `#some/tag`. Registration throws on both rather than
;; shadowing.
;;
;; The table is an immutable alist swapped whole under a mutex. Reads happen on
;; every `#` in every file jolt reads and take no lock: a reader sees either the
;; old list or the new one, where a Chez hashtable read while another thread
;; writes it faults in the collector.
(define rdr-dispatch-mu (make-mutex))
(define rdr-dispatch-macros '())        ; ((char raw? . fn) …) — swapped, never mutated

(define (rdr-dispatch-macro-ref c)
  (let ((tbl rdr-dispatch-macros))      ; one read of the pointer, then work off it
    (and (pair? tbl) (assv c tbl))))

;; the characters rdr-read-dispatch handles itself, above the registry clause
(define rdr-dispatch-builtins '(#\{ #\( #\" #\_ #\! #\' #\^ #\# #\= #\? #\:))
(define (rdr-dispatch-char-ok? c)
  (and (char? c)
       (not (memv c rdr-dispatch-builtins))
       (not (char-alphabetic? c))       ; a data-reader tag's first character
       (not (char-numeric? c))
       (not (rdr-ws? c))                ; whitespace and the comma the reader skips
       (not (memv c '(#\; #\\ #\) #\] #\}))))) ; comment, char literal, closers

(define (rdr-dispatch-reject c why)
  (jolt-throw (jolt-ex-info (string-append "cannot register a reader macro on #"
                                           (if (char? c) (string c) (jolt-pr-str c))
                                           ": " why)
                            (jolt-hash-map (keyword #f "char") c))))

(define (rdr-set-dispatch-macro! c fn raw?)
  (unless (char? c) (rdr-dispatch-reject c "not a character"))
  (when (memv c rdr-dispatch-builtins) (rdr-dispatch-reject c "the reader claims it"))
  (unless (rdr-dispatch-char-ok? c)
    (rdr-dispatch-reject c "only punctuation can carry one (a letter or digit starts a #tag)"))
  (unless (procedure? fn)
    (jolt-throw (jolt-ex-info "a reader macro must be a function" (jolt-hash-map))))
  (jolt-with-mutex rdr-dispatch-mu
    (set! rdr-dispatch-macros
          (cons (cons c (cons (and (jolt-truthy? raw?) #t) fn))
                (filter (lambda (e) (not (eqv? (car e) c))) rdr-dispatch-macros))))
  jolt-nil)

(define (rdr-remove-dispatch-macro! c)
  (jolt-with-mutex rdr-dispatch-mu
    (set! rdr-dispatch-macros (filter (lambda (e) (not (eqv? (car e) c))) rdr-dispatch-macros)))
  jolt-nil)

;; {char fn} of what is registered — the tier is not in it: a caller re-registers
;; with the tier it wants rather than reading one back out.
(define (rdr-dispatch-macro-map)
  (let loop ((es rdr-dispatch-macros) (acc '()))
    (if (null? es)
        (apply jolt-hash-map acc)
        (loop (cdr es) (cons (caar es) (cons (cddr (car es)) acc))))))

;; Apply a registered reader macro. i points AT the dispatch character.
(define (rdr-apply-dispatch-macro entry s i end)
  (let ((c (car entry)) (raw? (cadr entry)) (fn (cddr entry)))
    (if raw?
        (let ((res (jolt-invoke fn s (+ i 1))))
          (unless (and (pvec? res) (= 2 (vector-length (pvec-v res))))
            (rdr-error s i (string-append "reader macro #" (string c)
                                          " must return [form end-index]")))
          (let ((j (vector-ref (pvec-v res) 1)))
            ;; the index has to move forward and stay inside the input, or the
            ;; read loop above either spins on the same character forever or
            ;; indexes past the end of the string.
            (unless (and (integer? j) (exact? j) (>= j (+ i 1)) (<= j end))
              (rdr-error s i (string-append "reader macro #" (string c)
                                            " returned an out-of-range end-index: "
                                            (jolt-pr-str j))))
            (values (vector-ref (pvec-v res) 0) j)))
        (let-values (((form j) (rdr-read-form s (+ i 1) end)))
          (when (rdr-eof? form)
            (rdr-error s i (string-append "EOF after #" (string c))))
          (values (jolt-invoke fn form) j)))))

;; --- #$"…" — string interpolation -------------------------------------------
;; clojure.core.strint's ~{form} / ~(form) markers, applied to a string LITERAL
;; at read time:
;;
;;   #$"a ~{x} b ~(inc x)"  ->  (clojure.core/str "a " x " b " (inc x))
;;
;; A string with no marker reads as itself, so #$"plain" IS "plain" and costs
;; nothing at runtime. A `~` that is not followed by `{` or `(` is literal, as in
;; strint; a literal `~{` is written `~{"~{"}`.
;;
;; This is registered through the table above rather than wired into
;; rdr-read-dispatch directly, so (jolt.reader/dispatch-macros) lists it and
;; jolt.reader/remove-dispatch-macro! takes it back off like any other.
(define (rdr-interp-error msg str)
  (jolt-throw (jolt-ex-info (string-append msg ": " (jolt-pr-str str)) (jolt-hash-map))))

(define (rdr-interp-parts str)
  (let ((n (string-length str)))
    (let loop ((i 0) (lit '()) (parts '()))
      (let ((flush (lambda (ps) (if (null? lit) ps (cons (list->string (reverse lit)) ps)))))
        (cond
          ((>= i n) (reverse (flush parts)))
          ((and (char=? (string-ref str i) #\~) (< (+ i 1) n)
                (memv (string-ref str (+ i 1)) '(#\{ #\()))
           (let ((brace? (char=? (string-ref str (+ i 1)) #\{)))
             ;; ~{x} delimits the form with the brace; ~(f x) IS the form, so the
             ;; read starts on the paren and consumes its own closer.
             (let-values (((form j) (parameterize ((rdr-suppress-pos #t))
                                      (rdr-read-form str (if brace? (+ i 2) (+ i 1)) n))))
               (when (rdr-eof? form)
                 (rdr-interp-error "EOF in an interpolated form" str))
               (let ((k (if brace?
                            (let ((k (rdr-skip-ws str j n)))
                              (unless (and (< k n) (char=? (string-ref str k) #\}))
                                (rdr-interp-error "unterminated ~{…} in an interpolated string" str))
                              (+ k 1))
                            j)))
                 (loop k '() (cons form (flush parts)))))))
          (else (loop (+ i 1) (cons (string-ref str i) lit) parts)))))))

(define (rdr-interp-check! str)
  (unless (string? str)
    (jolt-throw (jolt-ex-info (string-append "string interpolation reads a string literal, got "
                                             (jolt-pr-str str))
                              (jolt-hash-map)))))

;; the literal-and-form parts, in source order — what clojure.core.strint's <<
;; splices into its own (str …) so the macro and the reader macro share one
;; implementation of the ~{} / ~() grammar.
(define (rdr-interpolate-parts str)
  (rdr-interp-check! str)
  (apply jolt-vector (rdr-interp-parts str)))

(define (rdr-interpolate str)
  (rdr-interp-check! str)
  (let ((parts (rdr-interp-parts str)))
    (cond
      ((null? parts) "")
      ((and (null? (cdr parts)) (string? (car parts))) (car parts))
      (else (apply jolt-list (jolt-symbol "clojure.core" "str") parts)))))

(rdr-set-dispatch-macro! #\$ rdr-interpolate #f)

(define (rdr-read-dispatch s i end)      ; i points just past the '#'
  (when (>= i end) (rdr-error s i "EOF after #"))
  (let ((c (string-ref s i)))
    (cond
      ((char=? c #\{)                    ; #{...} set
       (let-values (((elems j) (rdr-read-seq s (+ i 1) end #\})))
         ;; i is the literal's own start, which is the only place that knows it:
         ;; rdr-make-set takes elements, not source. See rdr-at.
         (values (rdr-at s i (lambda () (rdr-make-set elems))) j)))
      ((char=? c #\()                    ; #(...) anonymous fn shorthand
       (rdr-read-anon-fn s i end))
      ((char=? c #\")                    ; #"..." -> a regex VALUE (Clojure parity:
       ;; the reader constructs the Pattern, so a macro gets a regex, not a form).
       ;; The analyzer compiles a regex value to the same :regex IR leaf via its
       ;; source string.
       (let-values (((src j) (rdr-read-regex s (+ i 1) end)))
         (values (jolt-re-pattern src) j)))
      ((char=? c #\_)                    ; #_ discard the next form
        (let-values (((d j) (rdr-read-form s (+ i 1) end)))
          (when (rdr-eof? d) (rdr-error s i "EOF after #_"))
          ;; edn validates the discarded element (its tags go through the same
          ;; :readers/:default pipeline; an unreadable one throws)
          (let ((cb (rdr-discard-cb)))
            (when cb (jolt-invoke cb d)))
          (rdr-read-form s j end)))
       ((char=? c #\!)                    ; #! shebang line comment — skip to EOL
        ;; a clojure-reader extension only: EDN rejects #! (No dispatch macro)
        (when (rdr-edn-mode) (rdr-error s i "No dispatch macro for: !"))
        (let eol ((j (+ i 1)))
          (if (or (>= j end) (char=? (string-ref s j) #\newline)
                  (char=? (string-ref s j) #\return))
              (rdr-read-form s j end)
              (eol (+ j 1)))))
      ((char=? c #\')                    ; #'x var-quote -> (var x)
       (let-values (((form j) (rdr-read-form s (+ i 1) end)))
         (values (jolt-list (jolt-symbol #f "var") form) j)))
      ((char=? c #\^)                    ; #^meta — deprecated metadata syntax = ^meta
       (let-values (((mform j) (rdr-read-form s (+ i 1) end)))
         (let-values (((target k) (rdr-read-form s j end)))
           (when (rdr-eof? target)
             (rdr-error s j "EOF after #^meta"))
           (values (rdr-attach-meta target (rdr-meta-map mform)) k))))
      ((char=? c #\#)                    ; ## symbolic value: ##Inf / ##-Inf / ##NaN
       (let-values (((tok j) (rdr-read-token s (+ i 1) end)))
         (values (cond ((string=? tok "Inf") +inf.0)
                       ((string=? tok "-Inf") -inf.0)
                       ((string=? tok "NaN") +nan.0)
                       (else (rdr-error s j (string-append "unknown ## literal: " tok)
)))
                 j)))
      ((char=? c #\=)                    ; #=form read-eval: evaluate at READ time
       ;; The clojure reader's EvalReader, gated by *read-eval* (clj-uuid
       ;; computes its bit masks with #=). EDN has no = dispatch. The var cell
       ;; and the eval entry point live in later-loaded files; both resolve at
       ;; call time, and by the time user source is read the runtime is up.
       (when (rdr-edn-mode) (rdr-error s i "No dispatch macro for: ="))
       (let-values (((form j) (rdr-read-form s (+ i 1) end)))
         (when (rdr-eof? form) (rdr-error s i "EOF after #="))
         (unless (rdr-read-eval?)
           (rdr-error s i "EvalReader not allowed when *read-eval* is false."))
         (values (jolt-compile-eval-form form (chez-current-ns)) j)))
      ((char=? c #\?)                    ; #?(...) / #?@(...) reader conditional
       (rdr-read-reader-cond s (+ i 1) end))
      ((char=? c #\:)                    ; #:ns{...} namespaced map literal
       (rdr-read-ns-map s (+ i 1) end))
      ;; a registered reader macro (see the table above). EDN has a closed
      ;; grammar and no user extension point, so clojure.edn never consults it —
      ;; a #$ there stays the unreadable tag it already was.
      ((and (not (rdr-edn-mode)) (rdr-dispatch-macro-ref c))
       => (lambda (entry) (rdr-apply-dispatch-macro entry s i end)))
      (else                              ; #tag form -> tagged {:tag :#tag :form ...}
       (let-values (((tok j) (rdr-read-token s i end)))
         (let-values (((form k) (rdr-read-form s j end)))
           (when (rdr-eof? form) (rdr-error s j "EOF after #tag"))
           (if (rdr-record-tag? tok)       ; #ns.Type{..}/[..] record literal
               (values (rdr-record-ctor-form tok form) k)
               (values (rdr-make-tagged (keyword #f (string-append "#" tok)) form) k))))))))

;; regex literal source: raw chars to the closing quote; \" is an escaped quote,
;; every other backslash sequence is kept verbatim (regex engine semantics).
(define (rdr-read-regex s i end)
  (let loop ((i i) (acc '()))
    (when (>= i end) (rdr-error s i "EOF while reading regex"))
    (let ((c (string-ref s i)))
      (cond
        ((char=? c #\") (values (list->string (reverse acc)) (+ i 1)))
        ;; \" delimits without ending the literal, and the pattern SOURCE keeps
        ;; the backslash — (pr-str #"a\"b") round-trips as #"a\"b" like the JVM.
        ((char=? c #\\)
         (when (>= (+ i 1) end) (rdr-error s i "EOF while reading regex"))
         (loop (+ i 2) (cons (string-ref s (+ i 1)) (cons #\\ acc))))
        (else (loop (+ i 1) (cons c acc)))))))

;; --- keyword ----------------------------------------------------------------
(define (rdr-read-keyword s i end)       ; i points just past the leading ':'
  ;; ::kw is auto-resolved against the current ns: ::name -> current-ns/name,
  ;; ::alias/name -> the alias's target ns / name (Clojure's reader semantics).
  (let ((auto? (and (< i end) (char=? (string-ref s i) #\:))))
    (let ((i (if auto? (+ i 1) i)))
      (let-values (((tok j) (rdr-read-token s i end)))
        (let ((len (string-length tok)))
          ;; ":" and "::" alone, a leading or trailing slash (a name of exactly
          ;; "/" is fine, :ns//), a token that STARTS with ':' (:::foo — the
          ;; leading colon(s) are already consumed, and a keyword name can't begin
          ;; with a colon, though :foo:bar is fine mid-name), or an auto-resolved
          ;; keyword in edn (no resolution context) are invalid tokens.
          (when (or (= len 0)
                    (char=? (string-ref tok 0) #\:)
                    (and (> len 1) (char=? (string-ref tok 0) #\/))
                    (and (> len 1) (char=? (string-ref tok (- len 1)) #\/)
                         (not (and (> len 2) (char=? (string-ref tok (- len 2)) #\/)))))
            (rdr-invalid-token (string-append (if auto? "::" ":") tok)))
          (when (and auto? (rdr-edn-mode))
            (rdr-invalid-token (string-append "::" tok))))
        (let-values (((ns name) (rdr-sym-parts tok)))
          (if auto?
              (let* ((cur (chez-current-ns))
                     (rns (if (string? ns)
                              (let ((a (chez-resolve-alias cur ns)))
                                (cond (a a)
                                      ((rdr-scan-mode) ns)
                                      (else (rdr-invalid-token (string-append "::" tok)))))
                              cur)))
                (values (keyword rns name) j))
              (values (keyword ns name) j)))))))

;; --- the main dispatch ------------------------------------------------------
;; Returns (values form j). form is rdr-eof at end-of-input or at an unconsumed
;; close delimiter (read-seq consumes the close itself).
(define (rdr-read-form s i end)
  (let ((i (rdr-skip-ws s i end)))
    (if (>= i end)
        (values rdr-eof i)
        (let ((c (string-ref s i)))
          (cond
            ((char=? c #\()
             (let-values (((es j) (rdr-read-seq s (+ i 1) end #\))))
               (let ((lst (apply jolt-list es)))
                 (values (if (rdr-suppress-pos)
                             lst
                             (let-values (((line col) (rdr-line-col-at s i)))
                               (rdr-attach-pos lst line col)))
                         j))))
            ((char=? c #\[) (let-values (((es j) (rdr-read-seq s (+ i 1) end #\])))
                              (values (apply jolt-vector es) j)))
            ((char=? c #\{) (let-values (((es j) (rdr-read-seq s (+ i 1) end #\})))
                              (values (rdr-at s i (lambda () (rdr-make-map es))) j)))
            ((or (char=? c #\)) (char=? c #\]) (char=? c #\}))
             (values rdr-eof i))         ; unconsumed close — read-seq handles it
            ;; i is the token's own start; the readers below take the index just
            ;; PAST the sigil and their helpers (rdr-invalid-token, the character
            ;; name table) see only a parsed token, so rdr-at is what turns a
            ;; positionless raise from one into a positioned diagnostic.
            ((char=? c #\") (rdr-at s i (lambda () (rdr-read-string-lit s (+ i 1) end))))
            ((char=? c #\\) (rdr-at s i (lambda () (rdr-read-char s (+ i 1) end))))
            ((char=? c #\:) (rdr-at s i (lambda () (rdr-read-keyword s (+ i 1) end))))
            ((char=? c #\#) (rdr-read-dispatch s (+ i 1) end))
            ((char=? c #\') (rdr-wrap s (+ i 1) end (jolt-symbol #f "quote")))
            ;; syntax-quote of a self-evaluating literal collapses to the literal at
            ;; READ time (Clojure's reader), so nested backticks over a literal are
            ;; inert: ``42 reads as 42, ```"meow" as "meow".
            ;;
            ;; The marker is clojure.core-QUALIFIED, for the same reason ~ and ~@
            ;; below are: it is a name jolt invents, and a bare one would reserve
            ;; `syntax-quote` against every program that wants it. The JVM has no
            ;; special form of this name (its reader expands ` outright), so a var
            ;; or local called syntax-quote is legal there and must stay legal here
            ;; — edamame names its own resolver that and :refers it in.
            ((char=? c #\`)
             (let-values (((form j) (rdr-read-form s (+ i 1) end)))
               (when (rdr-eof? form) (rdr-error s i "EOF after `"))
               (values (if (rdr-self-eval-literal? form)
                           form
                           (jolt-list (jolt-symbol "clojure.core" "syntax-quote") form))
                       j)))
            ((char=? c #\@) (rdr-wrap s (+ i 1) end (jolt-symbol "clojure.core" "deref")))
            ;; ~ / ~@ read as clojure.core/unquote(-splicing), like the JVM reader —
            ;; so code that inspects pattern/template data (core.logic's defne) sees
            ;; the qualified symbol it expects.
            ((char=? c #\~)
             (if (and (< (+ i 1) end) (char=? (string-ref s (+ i 1)) #\@))
                 (rdr-wrap s (+ i 2) end (jolt-symbol "clojure.core" "unquote-splicing"))
                 (rdr-wrap s (+ i 1) end (jolt-symbol "clojure.core" "unquote"))))
            ((char=? c #\^)
             (let-values (((mform j) (rdr-read-form s (+ i 1) end)))
               (let-values (((target k) (rdr-read-form s j end)))
                 (when (rdr-eof? target)
                   (rdr-error s i "EOF after ^meta"))
                 (values (rdr-attach-meta target (rdr-meta-map mform)) k))))
            (else
             (let-values (((tok j) (rdr-read-token s i end)))
               (values (rdr-token->value tok) j))))))))

;; wrap the next form in a 2-element list (READER-MACRO form)
;; self-evaluating literals (NOT symbols/collections) — syntax-quote passes these
;; through unchanged, collapsed at read time.
(define (rdr-self-eval-literal? x)
  (or (jolt-nil? x) (boolean? x) (number? x) (string? x) (keyword? x) (char? x)))

(define (rdr-wrap s i end head)
  (let-values (((form j) (rdr-read-form s i end)))
    (when (rdr-eof? form)
      (rdr-error s i "EOF while reading reader macro"))
    (values (jolt-list head form) j)))

;; --- form -> data -----------------------------------------------------------
;; read-string/read return DATA, so set literal FORMS ({:jolt/type :jolt/set
;; :value [...]}) become real sets, recursing through maps/vectors/lists. The
;; COMPILER reads via rdr-read-form and keeps the set FORM (the analyzer lowers
;; it), so this conversion runs only on the data seams. Structural sharing keeps
;; identity (and the rdr-map-order entry + metadata) for any branch with no set.
(define (rdr-set-form? x)
  (and (pmap? x) (eq? (jolt-get x rdr-kw-jolt-type) rdr-kw-jolt-set)
       (not (jolt-nil? (jolt-get x rdr-kw-value)))))

(define (rdr-conv-each xs)         ; (values converted-list changed?)
  (let loop ((xs xs) (acc '()) (changed #f))
    (if (null? xs)
        (values (reverse acc) changed)
        (let ((c (rdr-form->data (car xs))))
          (loop (cdr xs) (cons c acc) (or changed (not (eq? c (car xs)))))))))

;; carry the reader metadata, converting its nested forms too — a set/tagged
;; literal inside a ^{…} map (^{:k #{…}}) must become a value like the rest of
;; the data, not stay the tagged set-form.
(define (rdr-carry-meta src dst)
  (let ((m (jolt-meta src))) (if (jolt-nil? m) dst (jolt-with-meta dst (rdr-form->data m)))))

;; tag keyword (:#time/date) -> its *data-readers* reader fn, or #f. The fn's
;; namespace must already be loaded (the loader requires them when a project's
;; data_readers.{clj,cljc} registers a tag).
(define (rdr-data-reader-fn tag)
  (and (keyword? tag)
       (let ((nm (keyword-t-name tag)))
         (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\#)
              (let* ((bare (substring nm 1 (string-length nm)))
                     (slash (let loop ((i 0))
                              (cond ((>= i (string-length bare)) #f)
                                    ((char=? (string-ref bare i) #\/) i)
                                    (else (loop (+ i 1))))))
                     (sym (if slash
                              (jolt-symbol (substring bare 0 slash) (substring bare (+ slash 1) (string-length bare)))
                              (jolt-symbol #f bare)))
                     (dr (var-deref "clojure.core" "*data-readers*"))
                     (v (and (pmap? dr) (jolt-get dr sym))))
                ;; *data-readers* maps a tag to a var or a fn (JVM). A fn value is
                ;; applied as-is; a var yields its root fn; a qualified symbol is
                ;; resolved to its var, as before.
                (cond
                  ((or (not v) (jolt-nil? v)) #f)
                  ((procedure? v) v)
                  ((var-cell? v) (let ((fn (var-cell-root v))) (and (procedure? fn) fn)))
                  ((and (symbol-t? v) (not (jolt-nil? (symbol-t-ns v))))
                   (guard (e (#t #f))
                     (let ((fn (var-deref (symbol-t-ns v) (symbol-t-name v))))
                       (and (procedure? fn) fn))))
                  (else #f)))))))
;; the bare tag SYMBOL for a :#name / :#ns/name reader keyword (strip the leading
;; #, split a qualified tag on /). *default-data-reader-fn* receives it.
(define (rdr-tag->symbol tag)
  (let* ((nm (keyword-t-name tag))
         (bare (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\#))
                   (substring nm 1 (string-length nm)) nm)))
    (let loop ((i 0))
      (cond ((>= i (string-length bare)) (jolt-symbol #f bare))
            ((char=? (string-ref bare i) #\/)
             (jolt-symbol (substring bare 0 i) (substring bare (+ i 1) (string-length bare))))
            (else (loop (+ i 1)))))))
;; *default-data-reader-fn* — a (fn [tag value]) consulted for an unregistered
;; tag, or #f when unset/nil. Honors a `binding` (var-deref reads the stack).
(define (rdr-default-data-reader-fn)
  (guard (e (#t #f))
    (let ((v (var-deref "clojure.core" "*default-data-reader-fn*")))
      (and (not (jolt-nil? v)) (procedure? v) v))))

;; strict #inst validation: RFC-3339 calendar fields must be real (month 1-12,
;; day valid for the month incl. leap years, hour < 24, minute/second < 60).
(define (rdr-2dig s i)
  (and (< (+ i 1) (string-length s))
       (rdr-digit? (string-ref s i)) (rdr-digit? (string-ref s (+ i 1)))
       (+ (* 10 (- (char->integer (string-ref s i)) 48))
          (- (char->integer (string-ref s (+ i 1))) 48))))
(define (rdr-leap? y) (and (= 0 (modulo y 4)) (or (not (= 0 (modulo y 100))) (= 0 (modulo y 400)))))
(define (rdr-inst-throw s)
  (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                   (string-append "Unrecognized date/time syntax: " s))))
(define (rdr-validate-inst! s)
  ;; progressive RFC-3339 like clojure.instant: yyyy[-MM[-dd[Thh[:mm[:ss[.f]]]]]]
  ;; with an optional Z/±hh:mm offset; each present field must be in range
  ;; (months 1-12, day valid for the month incl. leap years, hour < 24, min < 60).
  (let* ((len (string-length s))
         (y (and (>= len 4) (rdr-all-digits? s 0 4) (string->number (substring s 0 4)))))
    (unless y (rdr-inst-throw s))
    (when (>= len 5)
      (unless (char=? (string-ref s 4) #\-) (rdr-inst-throw s))
      (let ((mo (rdr-2dig s 5)))
        (unless (and mo (>= mo 1) (<= mo 12)) (rdr-inst-throw s))
        (when (>= len 8)
          (unless (char=? (string-ref s 7) #\-) (rdr-inst-throw s))
          (let ((d (rdr-2dig s 8)))
            (unless (and d (>= d 1)
                         (<= d (vector-ref (if (rdr-leap? y)
                                               '#(31 29 31 30 31 30 31 31 30 31 30 31)
                                               '#(31 28 31 30 31 30 31 31 30 31 30 31))
                                           (- mo 1))))
              (rdr-inst-throw s))
            (when (>= len 11)
              (unless (char=? (string-ref s 10) #\T) (rdr-inst-throw s))
              (let ((h (rdr-2dig s 11)))
                (unless (and h (<= h 23)) (rdr-inst-throw s))
                (when (>= len 14)
                  (when (char=? (string-ref s 13) #\:)
                    (let ((mi (rdr-2dig s 14)))
                      (unless (and mi (<= mi 59)) (rdr-inst-throw s)))))))))))))
;; strict #uuid: canonical 8-4-4-4-12 hex groups.
(define (rdr-validate-uuid! s)
  (define (hexrun? from to)
    (let loop ((i from))
      (cond ((>= i to) #t)
            ((let ((c (char-downcase (string-ref s i))))
               (or (rdr-digit? c) (and (char>=? c #\a) (char<=? c #\f))))
             (loop (+ i 1)))
            (else #f))))
  (unless (and (= (string-length s) 36)
               (char=? (string-ref s 8) #\-) (char=? (string-ref s 13) #\-)
               (char=? (string-ref s 18) #\-) (char=? (string-ref s 23) #\-)
               (hexrun? 0 8) (hexrun? 9 13) (hexrun? 14 18) (hexrun? 19 23) (hexrun? 24 36))
    (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                     (string-append "Invalid UUID string: " s)))))

;; read-string / read data seam: construct the value for a #tag literal. #inst,
;; #uuid and #"regex" are built in; any other tag is applied from *data-readers*,
;; then *default-data-reader-fn*. An unregistered tag with no default handler stays
;; a tagged FORM (lenient — clojure.edn raises instead).
(define (rdr-construct-tag tag inner)
  (cond
    ((eq? tag (keyword #f "#inst"))
     (when (string? inner) (rdr-validate-inst! inner))
     (jolt-inst-from-string inner))
    ((eq? tag (keyword #f "#uuid"))
     (when (string? inner) (rdr-validate-uuid! inner))
     (jolt-uuid-from-string inner))
    ((eq? tag (keyword #f "regex")) (jolt-re-pattern inner))
    ;; the M-literal form: construct the BigDecimal from its numeric text
    ((eq? tag (keyword #f "bigdec")) (jolt-bigdec-from-string inner))
    (else (let ((fn (rdr-data-reader-fn tag)))
            (if fn (jolt-invoke fn inner)
                (let ((dfn (rdr-default-data-reader-fn)))
                  (if dfn (jolt-invoke dfn (rdr-tag->symbol tag) inner)
                      ;; no reader for the tag: a proper tagged-literal value, like
                      ;; Clojure's *default-data-reader-fn* (tagged-literal), so
                      ;; tagged-literal? / :tag / :form / printing all work — not the
                      ;; internal reader form. clojure.edn reads raw forms via
                      ;; __read-form-raw, so its :readers/:default path is unaffected.
                      (jolt-tagged-literal (rdr-tag->symbol tag) inner))))))))

;; --- syntax-quote lowering for the data path ---------------------------------
;; Expands `(syntax-quote FORM)` to the JVM-compatible seq/concat/list form at
;; read time, so read-string / read return the same data as clojure.core's reader.
;; Symbol resolution uses the current *ns* (chez-current-ns), auto-gensym sharing
;; is stable within one backquote, and the output is DATA (not construction IR).
;; Self-evaluating literals collapse at read time (line ~778 already does this
;; for the top-level backquote, this handles nested backquotes on non-literals).

(define rdr-sq-gensym-counter 0)
(define (rdr-sq-gensym base)
  (jolt-symbol #f (string-append base "__"
                                 (number->string
                                  (jolt-with-mutex rdr-name-counter-mutex
                                    (set! rdr-sq-gensym-counter (fx+ rdr-sq-gensym-counter 1))
                                    rdr-sq-gensym-counter))
                                 "__auto")))

;; special forms / interop heads stay bare in backquote, like the JVM reader
;; The one list of names a syntax-quote leaves BARE (not namespace-qualified):
;; the special-form heads plus the reader-macro markers. Shared by the data path
;; (rdr-sq-symbol) and the compile path (host-contract.ss hc-sq-symbol), so a
;; special added here is honored by both `read-string and a compiled `.
;; NOT here: "syntax-quote". It names no Clojure special form, so `syntax-quote
;; qualifies to the current ns like any other symbol; jolt's own ` marker is
;; already clojure.core-qualified and so is never a candidate for qualification.
(define jsq-specials
  '("quote" "unquote" "unquote-splicing" "do" "if" "def"
    "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "var" "new" "."
    "&" "catch" "finally" "case*" "letfn*" "monitor-enter" "monitor-exit"
    "reify*" "deftype*"))

;; The one syntax-quote symbol resolver, shared by both paths. `cns` is the
;; namespace to resolve against (the compile ns / the current ns); `gensym-fn`
;; makes a fresh auto-gensym for a trailing-# symbol (each path keeps its own
;; counter, so a compiled ` and a read-string ` don't share gensym numbers).
;; Resolution order (Clojure): a foo# auto-gensym; a bare special/interop/class
;; token; the ns's own interned var; a :refer'd name (SOURCE ns) — BEFORE the
;; implicit clojure.core, so an explicit :refer shadows core; then clojure.core
;; unless the name is :refer-clojure-excluded or ns-unmapped; else qualify to cns.
(define (jsq-class-symbol cell)
  ;; A var holding a Class object is jolt's model of an import — syntax-quote
  ;; renders it as the class's FQN (ns=nil), like the JVM.
  (and (var-cell? cell)
       (let ((v (var-cell-root cell)))
         (and (jclass? v) (jolt-symbol #f (vector-ref (jhost-state v) 0))))))

(define (jsq-resolve-symbol cns sym gsmap gensym-fn)
  (let ((sns (symbol-t-ns sym)) (nm (symbol-t-name sym)))
    (if (or (jolt-nil? sns) (null? sns) (not sns))
        (cond
          ((and (fx>? (string-length nm) 0)
                (char=? (string-ref nm (fx- (string-length nm) 1)) #\#))
           (or (hashtable-ref gsmap nm #f)
               (let ((g (gensym-fn (substring nm 0 (fx- (string-length nm) 1)))))
                 (hashtable-set! gsmap nm g) g)))
          ((member nm jsq-specials) sym)
          ((hc-interop-head? nm) sym)         ; interop (.method / Class. / .-field)
          ((hc-fq-class-name? nm) sym)        ; a fully-qualified class token
          ((var-cell-lookup cns nm)                              ; the ns's own var
           => (lambda (cell) (or (jsq-class-symbol cell) (jolt-symbol cns nm))))
          ((chez-resolve-refer cns nm)                             ; a :refer'd name
           => (lambda (ref)
                (let ((cell (var-cell-lookup (car ref) (cdr ref))))
                  (or (jsq-class-symbol cell)
                      (jolt-symbol (car ref) (cdr ref))))))
          ((and (not (chez-core-excluded? cns nm))                 ; else clojure.core,
                (not (eq? (hashtable-ref ns-refer-table (cons cns nm) #f) 'unmapped))
                (var-cell-lookup "clojure.core" nm))               ; unless excluded/unmapped
           => (lambda (cell) (or (jsq-class-symbol cell) (jolt-symbol "clojure.core" nm))))
          (else (jolt-symbol cns nm)))                             ; else the ns itself
        ;; qualified: resolve an :as alias in cns to the target ns, else leave as
        ;; written (a real ns or an interop class token).
        (let ((target (chez-resolve-alias cns sns)))
          (if target (jolt-symbol target nm) sym)))))

(define (rdr-sq-head-is? x nm)
  (and (cseq? x)
       (let ((h (seq-first x)))
         (and (symbol-t? h) (string=? (symbol-t-name h) nm)
              (let ((ns (symbol-t-ns h)))
                (or (jolt-nil? ns) (null? ns) (not ns)
                    (and (string? ns) (string=? ns "clojure.core"))))))))

(define (rdr-sq-literal? x)
  (or (jolt-nil? x) (boolean? x) (number? x) (string? x) (keyword? x) (char? x)))

;; data path: resolve against the current ns, via the shared resolver.
(define (rdr-sq-symbol sym gsmap)
  (jsq-resolve-symbol (chez-current-ns) sym gsmap rdr-sq-gensym))

(define (rdr-sq-lower form gsmap)
  (cond
    ((rdr-sq-head-is? form "unquote")
     (seq-first (seq-more form)))
    ((rdr-sq-head-is? form "unquote-splicing")
     (jolt-throw (jolt-ex-info "~@ used outside of a list or vector in syntax-quote"
                               empty-pmap)))
    ((rdr-sq-literal? form) form)
    ((symbol-t? form)
     (jolt-list (jolt-symbol #f "quote")
                (rdr-sq-symbol form gsmap)))
    ;; `() is (clojure.core/list) => (), NOT (clojure.core/list ()) => (()).
    ((empty-list-t? form)
     (jolt-list (jolt-symbol "clojure.core" "list")))
    ((cseq? form)
     (if (rdr-syntax-quote-form? form)
         ;; nested backquote: lower it first (with a fresh gsmap — auto-gensyms in
         ;; the nested backquote are independent), then reprocess the result through
         ;; the outer lowering.
         (rdr-sq-lower (rdr-syntax-quote-lower (seq-first (seq-more form))) gsmap)
         (jolt-list (jolt-symbol "clojure.core" "seq")
                    (apply jolt-list (jolt-symbol "clojure.core" "concat")
                           (map (lambda (it) (rdr-sq-lower-part it gsmap))
                                (seq->list form))))))
    ((pvec? form)
     (jolt-list (jolt-symbol "clojure.core" "apply")
                (jolt-symbol "clojure.core" "vector")
                (jolt-list (jolt-symbol "clojure.core" "seq")
                           (apply jolt-list (jolt-symbol "clojure.core" "concat")
                                  (map (lambda (it) (rdr-sq-lower-part it gsmap))
                                       (vector->list (pvec-v form)))))))
    ((rdr-set-form? form)
     (let ((items (jolt-get form rdr-kw-value)))
       (jolt-list (jolt-symbol "clojure.core" "apply")
                  (jolt-symbol "clojure.core" "hash-set")
                  (jolt-list (jolt-symbol "clojure.core" "seq")
                             (apply jolt-list (jolt-symbol "clojure.core" "concat")
                                    (map (lambda (it) (rdr-sq-lower-part it gsmap))
                                         (vector->list (pvec-v items))))))))
    ((pmap? form)
     (let ((order (rdr-map-order-ref form)))
       (let ((pairs (if order
                        (let r ((xs order) (acc '()))
                          (if (null? xs) (reverse acc)
                              (r (cddr xs) (cons (list (car xs) (cadr xs)) acc))))
                        (let r ((xs (pmap-fold form (lambda (k v a) (cons k (cons v a))) '()))
                                (acc '()))
                          (if (null? xs) (reverse acc)
                              (r (cddr xs) (cons (list (car xs) (cadr xs)) acc)))))))
         (jolt-list (jolt-symbol "clojure.core" "apply")
                    (jolt-symbol "clojure.core" "hash-map")
                     (jolt-list (jolt-symbol "clojure.core" "seq")
                                (apply jolt-list (jolt-symbol "clojure.core" "concat")
                                       (let loop ((ps pairs) (acc '()))
                                         (if (null? ps) (reverse acc)
                                             (loop (cdr ps)
                                                   (cons (jolt-list (jolt-symbol "clojure.core" "list")
                                                                    (rdr-sq-lower (cadar ps) gsmap))
                                                         (cons (jolt-list (jolt-symbol "clojure.core" "list")
                                                                          (rdr-sq-lower (caar ps) gsmap))
                                                               acc)))))))))))
    (else
     (jolt-list (jolt-symbol #f "quote") form))))

(define (rdr-sq-lower-part item gsmap)
  (if (rdr-sq-head-is? item "unquote-splicing")
      (seq-first (seq-more item))
      (jolt-list (jolt-symbol "clojure.core" "list")
                 (rdr-sq-lower item gsmap))))

(define (rdr-syntax-quote-lower form)
  (rdr-sq-lower form (make-hashtable string-hash string=?)))

;; Check if a cseq form is (clojure.core/syntax-quote ...) — the raw form the
;; reader emits for `. The qualification is the point: a BARE (syntax-quote x) is
;; an ordinary call to whatever the program means by that name, not a marker.
(define (rdr-syntax-quote-form? x)
  (and (cseq? x)
       (let ((h (seq-first x)))
         (and (symbol-t? h) (string=? (symbol-t-name h) "syntax-quote")
              (let ((ns (symbol-t-ns h)))
                (and (string? ns) (string=? ns "clojure.core")))))))

;; rdr-un-datafy: reverse rdr-datafy. (quote x) → x, (clojure.core/vector ...)
;; → vector, (clojure.core/list ...) → list. pmap args (from jolt-hash-map) are
;; already real values and returned as-is.
(define (rdr-un-datafy x)
  (cond
   ((pmap? x) x)
   ((cseq? x)
    (let ((lst (seq->list x)))
      (if (null? lst) x
          (let ((h (car lst)))
            (cond
             ((and (symbol-t? h) (string=? (symbol-t-name h) "quote")
                   (pair? (cdr lst)) (null? (cddr lst)))
              (cadr lst))
             ((and (symbol-t? h) (string=? (symbol-t-name h) "vector")
                   (let ((ns (symbol-t-ns h)))
                     (and ns (string=? ns "clojure.core"))))
              (apply jolt-vector (map rdr-un-datafy (cdr lst))))
             ((and (symbol-t? h) (string=? (symbol-t-name h) "list")
                   (let ((ns (symbol-t-ns h)))
                     (and ns (string=? ns "clojure.core"))))
              (apply jolt-list (map rdr-un-datafy (cdr lst))))
             (else (rdr-form->data x)))))))
   (else x)))

;; rdr-form->data*: convert the VALUE structure (set/tagged/nested forms). The
;; wrapper below adds the metadata, so the unchanged branches return x bare.
(define (rdr-form->data* x)
  (cond
    ((rdr-syntax-quote-form? x)
     ;; Lower (syntax-quote FORM) to JVM-compatible data and convert the result.
     (rdr-form->data (rdr-syntax-quote-lower (seq-first (seq-more x)))))
    ((and (pmap? x) (eq? (jolt-get x rdr-kw-jolt-type) rdr-kw-jolt-tagged))
     (rdr-construct-tag (jolt-get x rdr-kw-tag) (rdr-form->data (jolt-get x rdr-kw-form))))
    ((rdr-set-form? x)
     (let ((items (jolt-get x rdr-kw-value)))
       (let loop ((i 0) (s empty-pset))
         (if (fx>=? i (pvec-count items)) s
             (let ((v (rdr-form->data (pvec-nth-d items i jolt-nil))))
               ;; not in scan mode — see rdr-make-set: an unresolvable auto keyword
               ;; keeps its alias text there, so two placeholders can collide where
               ;; the real values do not, and the scanner discards these forms anyway.
               (when (and (not (rdr-scan-mode)) (jolt-truthy? (jolt-contains? s v)))
                 (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                                  (string-append "Duplicate key: " (jolt-pr-str v)))))
               (loop (fx+ i 1) (pset-conj s v)))))))
    ((pvec? x)
     (let-values (((items changed) (rdr-conv-each (vector->list (pvec-v x)))))
       (if changed (apply jolt-vector items) x)))
    ((pmap? x)
     (let ((order (rdr-map-order-ref x)))
       (if order
           (let-values (((kvs changed) (rdr-conv-each order)))
             (if changed (rdr-make-map kvs) x))
            (let-values (((kvs changed)
                          (rdr-conv-each (pmap-fold x (lambda (k v a) (cons k (cons v a))) '()))))
              (if changed (apply jolt-hash-map kvs) x)))))
     ((cseq? x)
      (if (rdr-ctor-call? x)
          ;; Record/type literal: resolve constructor and apply to data-converted args
          (let* ((lst (seq->list x))
                 (ctor-sym (car lst))
                 (ctor (var-deref (symbol-t-ns ctor-sym) (symbol-t-name ctor-sym)))
                 (args (map rdr-un-datafy (cdr lst))))
            (apply ctor args))
          (let-values (((items changed) (rdr-conv-each (seq->list x))))
            (if changed (apply jolt-list items) x))))
     (else x)))
;; Read DATA always carries metadata, converting its nested forms too — Clojure's
;; reader reads a ^{…} map with the same read() as any value, so a set/tagged
;; literal in metadata is a value, not a form. Carry it whether or not the value
;; itself changed (a set-form in the metadata of an otherwise-unchanged value).
(define (rdr-form->data x)
  (let ((v (rdr-form->data* x)) (m (jolt-meta x)))
    (if (jolt-nil? m) v (jolt-with-meta v (rdr-form->data m)))))

;; --- the two host seams -----------------------------------------------------
;; a top-level read: a stray close delimiter is unmatched (read-seq consumes the
;; close of an open collection; anything reaching here is unbalanced input).
(define (rdr-read-top s i end)
  (let ((k (rdr-skip-ws s i end)))
    (when (and (< k end)
                (let ((c (string-ref s k)))
                  (or (char=? c #\)) (char=? c #\]) (char=? c #\}))))
      ;; through rdr-error, so the report names the file and the line:col of the
      ;; delimiter itself. The loader reads every top-level form through here, and
      ;; "Unmatched delimiter: )" pointing at 1:1 of a 600-line file says only that
      ;; the file is unbalanced somewhere.
      (rdr-error s k (string-append "Unmatched delimiter: " (string (string-ref s k)))))
    (let-values (((form j) (rdr-read-form s k end)))
      (when (rdr-splice-t? form)
        (jolt-throw (jolt-ex-info
                     "Reader conditional splicing not allowed at the top level."
                     empty-pmap)))
      (values form j))))

;; clojure.core/read-string: first form, or nil for blank / comment-only input
;; (parse-string wart, matched deliberately). jolt-read-form-raw keeps set FORMS
;; for the compiler spine (compile-eval); the data seam converts them to sets.
(define (jolt-read-form-raw s)
  (let-values (((form j) (rdr-read-top s 0 (string-length s))))
    (if (rdr-eof? form) jolt-nil form)))

;; the edn seam: strict mode (no auto-resolved keywords), each #_ discard handed
;; to the callback for tag validation, and a distinct EOF sentinel so the edn
;; layer can honor its :eof option (nil input is a plain EOF).
(define (jolt-read-form-edn s cb)
  (if (jolt-nil? s)
      (keyword "jolt" "reader-eof")
      (parameterize ((rdr-edn-mode #t)
                     (rdr-discard-cb (if (jolt-nil? cb) #f cb)))
        (let-values (((form j) (rdr-read-top s 0 (string-length s))))
          (if (rdr-eof? form) (keyword "jolt" "reader-eof") form)))))
;; read-string: the 1-arity returns nil at end of input (the documented seed
;; wart, src 18); the (opts s) arity is the reference's, where :eof sets the
;; end-of-input value and its ABSENCE makes end of input an error. :read-cond and
;; :features are accepted and ignored: jolt's reader always resolves #?
;; conditionals, against the fixed host feature set {:jolt :clj :default} (see
;; rdr-features), so a reader-conditional string reads the same either way.
;; malli's generator-ast suite calls this arity.
(define rdr-kw-eof (keyword #f "eof"))
(define jolt-read-string
  (case-lambda
    ((s) (let ((form (jolt-read-form-raw s)))
           (if (jolt-nil? form) form (rdr-form->data form))))
    ((opts s)
     (let ((form (if (jolt-nil? s) jolt-nil (jolt-read-form-raw s))))
       (cond ((not (jolt-nil? form)) (rdr-form->data form))
             ((and (pmap? opts) (jolt-contains? opts rdr-kw-eof))
              (jolt-get opts rdr-kw-eof))
             (else (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap))))))))

;; __parse-next: [form rest-of-string] or nil when only whitespace/comments left.
(define (jolt-parse-next s)
  (let ((end (string-length s)))
    (let-values (((form j) (rdr-read-top s 0 end)))
      (if (rdr-eof? form)
          jolt-nil
          (jolt-vector (rdr-form->data form) (substring s j end))))))

;; The same read, at an INDEX into s rather than off the front: (form . next-index),
;; or #f when only whitespace/comments remain. Handing back an index instead of the
;; rest of the string is what keeps a caller that reads a source file form by form
;; linear — jolt-parse-next copies the whole remaining input on every call, and a
;; host reader read that way (java/io.ss host-reader-read-form) was quadratic.
;; Scheme-level, for that one caller: the jolt-visible __parse-next is unchanged.
(define (rdr-parse-at s i)
  (let ((end (string-length s)))
    (let-values (((form j) (rdr-read-top s i end)))
      (and (not (rdr-eof? form)) (cons (rdr-form->data form) j)))))

;; __read-tagged: apply a built-in data reader to an already-read form. The tag
;; is the :#name keyword the reader produced; #uuid/#inst reuse the inst-time ctors.
(define (jolt-read-tagged tag form)
  (cond
    ((eq? tag (keyword #f "#uuid"))
     (when (string? form) (rdr-validate-uuid! form))
     (jolt-uuid-from-string form))
    ((eq? tag (keyword #f "#inst"))
     (when (string? form) (rdr-validate-inst! form))
     (jolt-inst-from-string form))
    ((eq? tag (keyword #f "bigdec")) (jolt-bigdec-from-string form))
    ;; No registered reader: consult *default-data-reader-fn*, else throw a clean,
    ;; catchable ex-info naming the tag, like the JVM's "No reader function for tag
    ;; foobar" (empty-pmap is a VALUE — the old (empty-pmap) applied it as a
    ;; procedure and crashed the Chez VM).
    (else (let ((dfn (rdr-default-data-reader-fn)))
            (if dfn (jolt-invoke dfn (rdr-tag->symbol tag) form)
                (let* ((nm (keyword-t-name tag))
                       (bare (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\#))
                                 (substring nm 1 (string-length nm)) nm)))
                  (jolt-throw (jolt-ex-info (string-append "No reader function for tag " bare) empty-pmap))))))))

;; The jolt seam for rdr-parse-at: [form next-index] at an index into s, or nil
;; when only whitespace/comments remain. The IReader cursors in clojure.core
;; (50-io.clj) read at their offset through this, so draining a string form by
;; form stays linear — __parse-next below returns [form rest-of-string] and
;; copies the whole remaining input per call; it is kept for compatibility.
(define (jolt-parse-next-from s i)
  (let ((r (rdr-parse-at s i)))
    (if r (jolt-vector (car r) (cdr r)) jolt-nil)))


;; --- the reader-extension seam (stdlib/jolt/reader.clj wraps these) ----------
;; raw? picks the tier: #f hands the fn the next FORM, #t hands it the source
;; string and the index just past the dispatch character.
(def-var! "jolt.host" "set-dispatch-macro!" rdr-set-dispatch-macro!)
(def-var! "jolt.host" "remove-dispatch-macro!" rdr-remove-dispatch-macro!)
(def-var! "jolt.host" "dispatch-macros" rdr-dispatch-macro-map)
;; the ~{}/~() split behind #$, so clojure.core.strint's << expands through
;; exactly the same grammar instead of a second copy of it.
(def-var! "jolt.host" "interpolate-parts" rdr-interpolate-parts)
(def-var! "clojure.core" "read-string" jolt-read-string)
(def-var! "clojure.core" "__parse-next" jolt-parse-next)
(def-var! "clojure.core" "__parse-next-from" jolt-parse-next-from)
(def-var! "clojure.core" "__read-tagged" jolt-read-tagged)
;; __read-form-raw: the read form WITHOUT building values — set/tagged literals
;; stay FORMS. clojure.edn reads this so it applies a #tag through its :readers/
;; :default (a #inst can be overridden to defer), rather than read-string building
;; the built-in #inst eagerly (which fails on a non-string like #inst ^:ref […]).
(def-var! "clojure.core" "__read-form-raw" jolt-read-form-raw)
(def-var! "clojure.core" "__read-form-edn" jolt-read-form-edn)
