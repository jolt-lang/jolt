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
      ;; octal 0NNN: a leading 0 followed by octal digits (Clojure reads 042 as 34,
      ;; not decimal 42). "0" alone, 0x.., 0r.. and a float "0.5" are handled
      ;; elsewhere or fall through (a non-octal digit fails rdr-all-octal?).
      ((and (>= blen 2) (char=? (string-ref body 0) #\0) (rdr-all-octal? body 1 blen))
       (let ((o (rdr-parse-radix (substring body 1 blen) 8))) (and o (* sign o))))
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
        (loop (+ k 1) (+ (* acc 16) (rdr-hexdigit (string-ref s j))) (+ j 1)))))

(define (rdr-hexdigit c)
  (cond ((and (char>=? c #\0) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char>=? c #\a) (char<=? c #\f)) (+ 10 (- (char->integer c) 97)))
        ((and (char>=? c #\A) (char<=? c #\F)) (+ 10 (- (char->integer c) 65)))
        (else (error 'reader "bad hex digit" c))))

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
      (let ((cp (string->number (substring name 1 (string-length name)) 16)))
        (if (and cp (>= cp #xD800) (<= cp #xDFFF))
            (jolt-throw (jolt-ex-info "Invalid character constant: lone surrogate \\u escape" empty-pmap))
            (integer->char cp))))
    ((char=? (string-ref name 0) #\o)
     (let ((v (string->number (substring name 1 (string-length name)) 8)))
       (when (or (not v) (> v 255))
         (jolt-throw (jolt-ex-info "Octal escape sequence must be in range [0, 377]" empty-pmap)))
       (integer->char v)))
    (else (jolt-throw (jolt-ex-info (string-append "Unsupported character: \\" name)
                                    empty-pmap)))))

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
  (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                   (string-append "Invalid token: " tok))))
(define (rdr-token->value tok)
  (let ((n (rdr-try-number tok)))
    (cond
      (n n)
      ;; a token that starts like a number but doesn't parse as one is an
      ;; invalid number (1a, 08, 0x2g, 2r2), never a symbol — like the JVM.
      ((rdr-numeric-lead? tok)
       (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                                        (string-append "Invalid number: " tok))))
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
             ((rdr-eof? form) (loop j acc))   ; a #_ discard or no-match #? — re-check at j
             ((rdr-splice-t? form)            ; #?@ — splice the matched collection's items
              (loop j (append (reverse (rdr-splice-t-items form)) acc)))
             (else (loop j (cons form acc))))))))))

;; Map literals must preserve SOURCE key order so the analyzer emits the value
;; expressions in source order (Clojure guarantees left-to-right map-literal eval).
;; A pmap is hash-ordered, so record each reader-built map's (k1 v1 k2 v2 ...) form
;; sequence in a weak side-table the host contract's form-map-pairs consults.
(define rdr-map-order (make-weak-eq-hashtable))
(define (rdr-make-map es)
  ;; the JVM reader rejects duplicate literal keys before building the map
  (let dupchk ((kvs es) (seen empty-pset))
    (when (pair? kvs)
      (let ((k (car kvs)))
        (when (jolt-truthy? (jolt-contains? seen k))
          (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                           (string-append "Duplicate key: " (jolt-pr-str k)))))
        (dupchk (cddr kvs) (pset-conj seen k)))))
  (let ((m (apply jolt-hash-map es)))
    (when (pair? es) (hashtable-set! rdr-map-order m es))
    m))

(define (rdr-make-set elems)
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
     (make-symbol-t (symbol-t-ns target) (symbol-t-name target)
                    (rdr-merge-meta (symbol-t-meta target) meta)))
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
       (let ((order (and (pmap? target) (hashtable-ref rdr-map-order target #f))))
         (when order (hashtable-set! rdr-map-order c order)))
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

;; rdr-error: format an error with the current source position, throw ex-info.
;; The message is "msg (file:line:col)" when rdr-source-file is bound,
;; just "msg" for bare -e strings. ex-data carries :line :column and :file.
(define (rdr-error s i msg)
  (let-values (((line col) (rdr-line-col-at s i)))
    (let* ((file (rdr-source-file))
           (loc (if file (string-append " (" file ":" (number->string line) ":" (number->string col) ")") "")))
      (jolt-throw (jolt-ex-info (string-append msg loc) (rdr-pos-meta line col))))))

(define (rdr-attach-pos lst line col)
  (if (empty-list-t? lst)            ; () is interned, can't carry meta (= Clojure)
      lst
      (rdr-attach-meta lst (rdr-pos-meta line col))))

;; --- # dispatch -------------------------------------------------------------
;; #(...) anonymous fn shorthand: % -> p1, %N -> pN, %& -> rest. The
;; fixed arity is the MAX positional used (Clojure: #(do %2 %&) -> [p1 p2 & rest]).
;; Param names carry a trailing "#" so a #() inside a syntax-quote still reads them
;; as auto-gensyms.
(define rdr-anon-counter 0)
(define (rdr-anon-gensym)
  (set! rdr-anon-counter (+ rdr-anon-counter 1))
  (jolt-symbol #f (string-append "p__" (number->string rdr-anon-counter) "#")))
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
     (for-each (lambda (x) (rdr-anon-scan x max-box rest-box)) (or (hashtable-ref rdr-map-order f #f) '())))))
(define (rdr-anon-replace f slots rest-sym)
  (cond
    ((symbol-t? f)
     (let ((idx (rdr-pct-index (symbol-t-name f))))
       (cond ((eq? idx 'rest) rest-sym) (idx (vector-ref slots (- idx 1))) (else f))))
    ((pvec? f) (apply jolt-vector (map (lambda (x) (rdr-anon-replace x slots rest-sym)) (seq->list f))))
    ((or (cseq? f) (empty-list-t? f))
     (apply jolt-list (map (lambda (x) (rdr-anon-replace x slots rest-sym)) (seq->list f))))
    ((rdr-anon-set? f)
     (rdr-make-set (map (lambda (x) (rdr-anon-replace x slots rest-sym)) (seq->list (jolt-get f rdr-kw-value)))))
    ((pmap? f)
     (let ((kv (hashtable-ref rdr-map-order f #f)))
       (if kv (rdr-make-map (map (lambda (x) (rdr-anon-replace x slots rest-sym)) kv)) f)))
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

;; reader conditionals: jolt's feature set is {:jolt :clj :default};
;; the FIRST clause whose feature key is in the set wins (clause order, like
;; Clojure). jolt is a Clojure/JVM-compatible host — it emulates clojure.lang.*
;; and java.* interop — so it reads the :clj branch of a .cljc library (the JVM
;; code path its host shims target), not the :cljs one. A library can still
;; override with a :jolt-specific branch (place it before :clj).
(define rdr-features '("jolt" "clj" "default"))
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

;; Is v a cseq whose head symbol names a record constructor — "map->" or "->"
;; prefix WITH a namespace (produced by the reader for #ns.Type{...}/[...])?
;; Unqualified "->" is the threading macro, not a record ctor.
(define (rdr-ctor-call? v)
  (and (cseq? v)
       (let ((lst (seq->list v)))
         (and (pair? lst)
              (symbol-t? (car lst))
              (symbol-t-ns (car lst))               ; must be qualified
              (let* ((nm (symbol-t-name (car lst)))
                     (len (string-length nm)))
                (or (and (>= len 5) (string=? (substring nm 0 5) "map->"))
                    (and (>= len 2) (string=? (substring nm 0 2) "->"))))))))

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
    (let ((kv (hashtable-ref rdr-map-order v #f)))
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
  (let* ((di (rdr-string-rindex-char tok #\.))
         (ns (substring tok 0 di))
         (simple (substring tok (+ di 1) (string-length tok))))
    (cond
      ((pmap? form)
       (jolt-list (jolt-symbol ns (string-append "map->" simple))
                  (rdr-datafy form)))
      ((pvec? form)
       (apply jolt-list (jolt-symbol ns (string-append "->" simple))
              (map rdr-datafy (vector->list (pvec-v form)))))
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
(define (rdr-read-ns-map s i end)        ; i points just past "#:"
  (let* ((auto? (and (< i end) (char=? (string-ref s i) #\:)))
         (i2 (if auto? (+ i 1) i)))
    (let loop ((j i2))
      (cond
        ((>= j end) (rdr-error s j "EOF in namespaced map literal"))
        ((char=? (string-ref s j) #\{)
         (let* ((nstok (substring s i2 j))
                (mapns (if auto?
                           (if (string=? nstok "") (chez-current-ns)
                               (let ((a (chez-resolve-alias (chez-current-ns) nstok)))
                                 (if a a (rdr-invalid-token (string-append "::" nstok)))))
                           nstok)))
           (let-values (((es k) (rdr-read-seq s (+ j 1) end #\})))
             (values (rdr-make-map (rdr-nsmap-kvs mapns es)) k))))
        (else (loop (+ j 1)))))))

(define (rdr-read-dispatch s i end)      ; i points just past the '#'
  (when (>= i end) (rdr-error s i "EOF after #"))
  (let ((c (string-ref s i)))
    (cond
      ((char=? c #\{)                    ; #{...} set
       (let-values (((elems j) (rdr-read-seq s (+ i 1) end #\})))
         (values (rdr-make-set elems) j)))
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
      ((char=? c #\?)                    ; #?(...) / #?@(...) reader conditional
       (rdr-read-reader-cond s (+ i 1) end))
      ((char=? c #\:)                    ; #:ns{...} namespaced map literal
       (rdr-read-ns-map s (+ i 1) end))
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
          ;; "/" is fine, :ns//), or an auto-resolved keyword in edn (no
          ;; resolution context) are invalid tokens.
          (when (or (= len 0)
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
                                (if a a (rdr-invalid-token (string-append "::" tok))))
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
            ((char=? c #\() (let-values (((line col) (rdr-line-col-at s i)))
                              (let-values (((es j) (rdr-read-seq s (+ i 1) end #\))))
                                (values (rdr-attach-pos (apply jolt-list es) line col) j))))
            ((char=? c #\[) (let-values (((es j) (rdr-read-seq s (+ i 1) end #\])))
                              (values (apply jolt-vector es) j)))
            ((char=? c #\{) (let-values (((es j) (rdr-read-seq s (+ i 1) end #\})))
                              (values (rdr-make-map es) j)))
            ((or (char=? c #\)) (char=? c #\]) (char=? c #\}))
             (values rdr-eof i))         ; unconsumed close — read-seq handles it
            ((char=? c #\") (rdr-read-string-lit s (+ i 1) end))
            ((char=? c #\\) (rdr-read-char s (+ i 1) end))
            ((char=? c #\:) (rdr-read-keyword s (+ i 1) end))
            ((char=? c #\#) (rdr-read-dispatch s (+ i 1) end))
            ((char=? c #\') (rdr-wrap s (+ i 1) end (jolt-symbol #f "quote")))
            ;; syntax-quote of a self-evaluating literal collapses to the literal at
            ;; READ time (Clojure's reader), so nested backticks over a literal are
            ;; inert: ``42 reads as 42, ```"meow" as "meow".
            ((char=? c #\`)
             (let-values (((form j) (rdr-read-form s (+ i 1) end)))
               (when (rdr-eof? form) (rdr-error s i "EOF after `"))
               (values (if (rdr-self-eval-literal? form)
                           form
                           (jolt-list (jolt-symbol #f "syntax-quote") form))
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
                (and v (not (jolt-nil? v)) (symbol-t? v) (not (jolt-nil? (symbol-t-ns v)))
                     (guard (e (#t #f))
                       (let ((fn (var-deref (symbol-t-ns v) (symbol-t-name v))))
                         (and (procedure? fn) fn)))))))))
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
  (set! rdr-sq-gensym-counter (fx+ rdr-sq-gensym-counter 1))
  (jolt-symbol #f (string-append base "__" (number->string rdr-sq-gensym-counter) "__auto")))

;; special forms / interop heads stay bare in backquote, like the JVM reader
(define rdr-sq-specials
  '("quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
    "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "var" "new" "."
    "&" "catch" "finally" "case*" "letfn*" "monitor-enter" "monitor-exit"
    "reify*" "deftype*"))

(define (rdr-sq-head-is? x nm)
  (and (cseq? x)
       (let ((h (seq-first x)))
         (and (symbol-t? h) (string=? (symbol-t-name h) nm)
              (let ((ns (symbol-t-ns h)))
                (or (jolt-nil? ns) (null? ns) (not ns)
                    (and (string? ns) (string=? ns "clojure.core"))))))))

(define (rdr-sq-literal? x)
  (or (jolt-nil? x) (boolean? x) (number? x) (string? x) (keyword? x) (char? x)))

;; Resolve a bare or qualified symbol against the current ns, like Clojure's
;; syntax-quote reader. A trailing # triggers auto-gensym (stable within one `).
(define (rdr-sq-symbol sym gsmap)
  (let ((sns (symbol-t-ns sym)) (nm (symbol-t-name sym)))
    (if (or (jolt-nil? sns) (null? sns) (not sns))
        (cond
          ((and (fx>? (string-length nm) 0)
                (char=? (string-ref nm (fx- (string-length nm) 1)) #\#))
           (or (hashtable-ref gsmap nm #f)
               (let ((g (rdr-sq-gensym (substring nm 0 (fx- (string-length nm) 1)))))
                 (hashtable-set! gsmap nm g) g)))
           ((member nm rdr-sq-specials) sym)
           (else
            ;; JVM syntax-quote resolution: the ns's own interned var wins, then
            ;; refers/aliased refers, then the implicit clojure.core default,
            ;; else qualify to the current ns.
            (let* ((cns (chez-current-ns))
                   (own (let ((c (var-cell-lookup cns nm))) (and c (var-cell-defined? c))))
                   (resolved (and (not own) (chez-resolve-refer cns nm)))
                   (core (and (not own) (not resolved)
                              (not (eq? (hashtable-ref ns-refer-table (cons cns nm) #f) 'unmapped))
                              (let ((c (var-cell-lookup "clojure.core" nm)))
                                (and c (var-cell-defined? c))))))
              (jolt-symbol (cond (own cns) (resolved resolved) (core "clojure.core") (else cns)) nm))))
        (let ((target (chez-resolve-alias (chez-current-ns) sns)))
          (if target (jolt-symbol target nm) sym)))))

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
    ((empty-list-t? form)
     (jolt-list (jolt-symbol "clojure.core" "list") form))
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
     (let ((order (hashtable-ref rdr-map-order form #f)))
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

;; Check if a cseq form is (syntax-quote ...) — the raw form the reader emits for `.
(define (rdr-syntax-quote-form? x)
  (and (cseq? x)
       (let ((h (seq-first x)))
         (and (symbol-t? h) (string=? (symbol-t-name h) "syntax-quote")
              (let ((ns (symbol-t-ns h)))
                (or (jolt-nil? ns) (null? ns) (not ns)))))))

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
               (when (jolt-truthy? (jolt-contains? s v))
                 (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                                  (string-append "Duplicate key: " (jolt-pr-str v)))))
               (loop (fx+ i 1) (pset-conj s v)))))))
    ((pvec? x)
     (let-values (((items changed) (rdr-conv-each (vector->list (pvec-v x)))))
       (if changed (apply jolt-vector items) x)))
    ((pmap? x)
     (let ((order (hashtable-ref rdr-map-order x #f)))
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
      (jolt-throw (jolt-ex-info (string-append "Unmatched delimiter: "
                                               (string (string-ref s k)))
                                empty-pmap)))
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
(define (jolt-read-string s)
  (let ((form (jolt-read-form-raw s)))
    (if (jolt-nil? form) form (rdr-form->data form))))

;; __parse-next: [form rest-of-string] or nil when only whitespace/comments left.
(define (jolt-parse-next s)
  (let ((end (string-length s)))
    (let-values (((form j) (rdr-read-top s 0 end)))
      (if (rdr-eof? form)
          jolt-nil
          (jolt-vector (rdr-form->data form) (substring s j end))))))

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

(def-var! "clojure.core" "read-string" jolt-read-string)
(def-var! "clojure.core" "__parse-next" jolt-parse-next)
(def-var! "clojure.core" "__read-tagged" jolt-read-tagged)
;; __read-form-raw: the read form WITHOUT building values — set/tagged literals
;; stay FORMS. clojure.edn reads this so it applies a #tag through its :readers/
;; :default (a #inst can be overridden to defer), rather than read-string building
;; the built-in #inst eagerly (which fails on a non-string like #inst ^:ref […]).
(def-var! "clojure.core" "__read-form-raw" jolt-read-form-raw)
(def-var! "clojure.core" "__read-form-edn" jolt-read-form-edn)
