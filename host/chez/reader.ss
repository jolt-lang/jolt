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
;; every char of s in [from,to) is an octal digit (and the span is non-empty).
(define (rdr-all-octal? s from to)
  (and (fx<? from to)
       (let loop ((i from)) (cond ((fx=? i to) #t) ((rdr-octal? (string-ref s i)) (loop (fx+ i 1))) (else #f)))))

;; Advance past whitespace, commas, and ;-to-end-of-line comments.
(define (rdr-skip-ws s i end)
  (let loop ((i i))
    (cond
      ((>= i end) i)
      ((rdr-ws? (string-ref s i)) (loop (+ i 1)))
      ((char=? (string-ref s i) #\;)
       (let eol ((j (+ i 1)))
         (if (or (>= j end) (char=? (string-ref s j) #\newline))
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
      ;; when d divides n.
      (slash
       (let ((n (string->number (substring body 0 slash)))
             (d (string->number (substring body (+ slash 1) blen))))
         (and (integer? n) (integer? d) (not (= d 0))
              (* sign (/ n d)))))
      ;; hex 0x..
      ((and (>= blen 2) (char=? (string-ref body 0) #\0)
            (or (char=? (string-ref body 1) #\x) (char=? (string-ref body 1) #\X)))
       (let ((h (string->number (substring body 2 blen) 16)))
         (and h (* sign h))))
      ;; radix NrDDD (Clojure 2r1010 / 16rFF / 36rZ): N in decimal, DDD in base N
      ((let ((ri (or (rdr-string-index-char body #\r) (rdr-string-index-char body #\R))))
         (and ri (> ri 0) (< (+ ri 1) blen) ri))
       => (lambda (ri)
            (let ((radix (string->number (substring body 0 ri))))
              (and radix (integer? radix) (>= radix 2) (<= radix 36)
                   (let ((v (rdr-parse-radix (substring body (+ ri 1) blen) radix)))
                     (and v (* sign v)))))))
      ;; octal 0NNN: a leading 0 followed by octal digits (Clojure reads 042 as 34,
      ;; not decimal 42). "0" alone, 0x.., 0r.. and a float "0.5" are handled
      ;; elsewhere or fall through (a non-octal digit fails rdr-all-octal?).
      ((and (>= blen 2) (char=? (string-ref body 0) #\0) (rdr-all-octal? body 1 blen))
       (let ((o (rdr-parse-radix (substring body 1 blen) 8))) (and o (* sign o))))
      ;; bigint suffix N
      ((and (> blen 1) (char=? (string-ref body (- blen 1)) #\N))
       (let ((n (string->number (substring body 0 (- blen 1)))))
         (and n (integer? n) (* sign n))))
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
    (when (>= i end) (jolt-throw (jolt-ex-info "EOF while reading string" empty-pmap)))
    (let ((c (string-ref s i)))
      (cond
        ((char=? c #\") (values (list->string (reverse acc)) (+ i 1)))
        ((char=? c #\\)
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
                    (loop j (cons (integer->char val) acc)))))
             ((#\u)
              (let-values (((cp j) (rdr-hex->int s (+ i 2) 4)))
                ;; A \u escape is a UTF-16 code unit. jolt chars are Unicode scalars,
                ;; so combine a high+low surrogate pair (😃 -> U+1F603) into
                ;; the one scalar char. A lone surrogate has no scalar — emit U+FFFD
                ;; rather than crash (the irreducible UTF-16/scalar divergence).
                (cond
                  ((and (fx>=? cp #xD800) (fx<=? cp #xDBFF)
                        (fx<? (fx+ j 1) end)
                        (char=? (string-ref s j) #\\) (char=? (string-ref s (fx+ j 1)) #\u))
                   (let-values (((lo k) (rdr-hex->int s (+ j 2) 4)))
                     (if (and (fx>=? lo #xDC00) (fx<=? lo #xDFFF))
                         (loop k (cons (integer->char
                                        (fx+ #x10000 (fx* (fx- cp #xD800) 1024) (fx- lo #xDC00))) acc))
                         (loop j (cons #\xFFFD acc)))))
                  ((and (fx>=? cp #xD800) (fx<=? cp #xDFFF)) (loop j (cons #\xFFFD acc)))
                  (else (loop j (cons (integer->char cp) acc))))))
             (else (loop (+ i 2) (cons e acc))))))
        (else (loop (+ i 1) (cons c acc)))))))

;; backslash already consumed; read a Clojure character literal.
(define (rdr-read-char s i end)
  (when (>= i end) (jolt-throw (jolt-ex-info "EOF while reading char" empty-pmap)))
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
     (integer->char (string->number (substring name 1 (string-length name)) 16)))
    ((char=? (string-ref name 0) #\o)
     (integer->char (string->number (substring name 1 (string-length name)) 8)))
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

(define (rdr-token->value tok)
  (let ((n (rdr-try-number tok)))
    (cond
      (n n)
      ((string=? tok "nil") jolt-nil)
      ((string=? tok "true") #t)
      ((string=? tok "false") #f)
      (else (let-values (((ns name) (rdr-sym-parts tok))) (jolt-symbol ns name))))))

;; --- collections ------------------------------------------------------------
;; Read forms until the close delimiter; returns (values reversed?-no list j).
(define (rdr-read-seq s i end close)
  (let loop ((i i) (acc '()))
    (let ((i (rdr-skip-ws s i end)))
      (cond
        ((>= i end) (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap)))
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
      (when (rdr-eof? form) (jolt-throw (jolt-ex-info "EOF after #?" empty-pmap)))
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

;; #a.b.C{..} -> (a.b/map->C {..}); #a.b.C[..] -> (a.b/->C ..). The factory call
;; compiles like any invoke; defrecord interns map->C/->C in the type's ns.
(define (rdr-record-ctor-form tok form)
  (let* ((di (rdr-string-rindex-char tok #\.))
         (ns (substring tok 0 di))
         (simple (substring tok (+ di 1) (string-length tok))))
    (cond
      ((pmap? form)
       (jolt-list (jolt-symbol ns (string-append "map->" simple)) form))
      ((pvec? form)
       (apply jolt-list (jolt-symbol ns (string-append "->" simple))
              (vector->list (pvec-v form))))
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
        ((>= j end) (jolt-throw (jolt-ex-info "EOF in namespaced map literal" empty-pmap)))
        ((char=? (string-ref s j) #\{)
         (let* ((nstok (substring s i2 j))
                (mapns (if auto?
                           (if (string=? nstok "") (chez-current-ns)
                               (let ((a (chez-resolve-alias (chez-current-ns) nstok))) (if a a nstok)))
                           nstok)))
           (let-values (((es k) (rdr-read-seq s (+ j 1) end #\})))
             (values (rdr-make-map (rdr-nsmap-kvs mapns es)) k))))
        (else (loop (+ j 1)))))))

(define (rdr-read-dispatch s i end)      ; i points just past the '#'
  (when (>= i end) (jolt-throw (jolt-ex-info "EOF after #" empty-pmap)))
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
       (let-values (((_ j) (rdr-read-form s (+ i 1) end)))
         (when (rdr-eof? _) (jolt-throw (jolt-ex-info "EOF after #_" empty-pmap)))
         (rdr-read-form s j end)))
      ((char=? c #\')                    ; #'x var-quote -> (var x)
       (let-values (((form j) (rdr-read-form s (+ i 1) end)))
         (values (jolt-list (jolt-symbol #f "var") form) j)))
      ((char=? c #\^)                    ; #^meta — deprecated metadata syntax = ^meta
       (let-values (((mform j) (rdr-read-form s (+ i 1) end)))
         (let-values (((target k) (rdr-read-form s j end)))
           (when (rdr-eof? target)
             (jolt-throw (jolt-ex-info "EOF after #^meta" empty-pmap)))
           (values (rdr-attach-meta target (rdr-meta-map mform)) k))))
      ((char=? c #\#)                    ; ## symbolic value: ##Inf / ##-Inf / ##NaN
       (let-values (((tok j) (rdr-read-token s (+ i 1) end)))
         (values (cond ((string=? tok "Inf") +inf.0)
                       ((string=? tok "-Inf") -inf.0)
                       ((string=? tok "NaN") +nan.0)
                       (else (jolt-throw (jolt-ex-info (string-append "unknown ## literal: " tok)
                                                       empty-pmap))))
                 j)))
      ((char=? c #\?)                    ; #?(...) / #?@(...) reader conditional
       (rdr-read-reader-cond s (+ i 1) end))
      ((char=? c #\:)                    ; #:ns{...} namespaced map literal
       (rdr-read-ns-map s (+ i 1) end))
      (else                              ; #tag form -> tagged {:tag :#tag :form ...}
       (let-values (((tok j) (rdr-read-token s i end)))
         (let-values (((form k) (rdr-read-form s j end)))
           (when (rdr-eof? form) (jolt-throw (jolt-ex-info "EOF after #tag" empty-pmap)))
           (if (rdr-record-tag? tok)       ; #ns.Type{..}/[..] record literal
               (values (rdr-record-ctor-form tok form) k)
               (values (rdr-make-tagged (keyword #f (string-append "#" tok)) form) k))))))))

;; regex literal source: raw chars to the closing quote; \" is an escaped quote,
;; every other backslash sequence is kept verbatim (regex engine semantics).
(define (rdr-read-regex s i end)
  (let loop ((i i) (acc '()))
    (when (>= i end) (jolt-throw (jolt-ex-info "EOF while reading regex" empty-pmap)))
    (let ((c (string-ref s i)))
      (cond
        ((char=? c #\") (values (list->string (reverse acc)) (+ i 1)))
        ((and (char=? c #\\) (< (+ i 1) end) (char=? (string-ref s (+ i 1)) #\"))
         (loop (+ i 2) (cons #\" acc)))
        ((char=? c #\\)
         (loop (+ i 2) (cons (string-ref s (+ i 1)) (cons #\\ acc))))
        (else (loop (+ i 1) (cons c acc)))))))

;; --- keyword ----------------------------------------------------------------
(define (rdr-read-keyword s i end)       ; i points just past the leading ':'
  ;; ::kw is auto-resolved against the current ns: ::name -> current-ns/name,
  ;; ::alias/name -> the alias's target ns / name (Clojure's reader semantics).
  (let ((auto? (and (< i end) (char=? (string-ref s i) #\:))))
    (let ((i (if auto? (+ i 1) i)))
      (let-values (((tok j) (rdr-read-token s i end)))
        (let-values (((ns name) (rdr-sym-parts tok)))
          (if auto?
              (let* ((cur (chez-current-ns))
                     (rns (if (string? ns)
                              (let ((a (chez-resolve-alias cur ns))) (if a a ns))
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
               (when (rdr-eof? form) (jolt-throw (jolt-ex-info "EOF after `" empty-pmap)))
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
                   (jolt-throw (jolt-ex-info "EOF after ^meta" empty-pmap)))
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
      (jolt-throw (jolt-ex-info "EOF while reading reader macro" empty-pmap)))
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

;; read-string / read data seam: construct the value for a #tag literal. #inst,
;; #uuid and #"regex" are built in; any other tag is applied from *data-readers*,
;; then *default-data-reader-fn*. An unregistered tag with no default handler stays
;; a tagged FORM (lenient — clojure.edn raises instead).
(define (rdr-construct-tag tag inner)
  (cond
    ((eq? tag (keyword #f "#inst")) (jolt-inst-from-string inner))
    ((eq? tag (keyword #f "#uuid")) (jolt-uuid-from-string inner))
    ((eq? tag (keyword #f "regex")) (jolt-re-pattern inner))
    (else (let ((fn (rdr-data-reader-fn tag)))
            (if fn (jolt-invoke fn inner)
                (let ((dfn (rdr-default-data-reader-fn)))
                  (if dfn (jolt-invoke dfn (rdr-tag->symbol tag) inner)
                      (rdr-make-tagged tag inner))))))))

;; rdr-form->data*: convert the VALUE structure (set/tagged/nested forms). The
;; wrapper below adds the metadata, so the unchanged branches return x bare.
(define (rdr-form->data* x)
  (cond
    ((and (pmap? x) (eq? (jolt-get x rdr-kw-jolt-type) rdr-kw-jolt-tagged))
     (rdr-construct-tag (jolt-get x rdr-kw-tag) (rdr-form->data (jolt-get x rdr-kw-form))))
    ((rdr-set-form? x)
     (let ((items (jolt-get x rdr-kw-value)))
       (let loop ((i 0) (s empty-pset))
         (if (fx>=? i (pvec-count items)) s
             (loop (fx+ i 1) (pset-conj s (rdr-form->data (pvec-nth-d items i jolt-nil))))))))
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
     (let-values (((items changed) (rdr-conv-each (seq->list x))))
       (if changed (apply jolt-list items) x)))
    (else x)))
;; Read DATA always carries metadata, converting its nested forms too — Clojure's
;; reader reads a ^{…} map with the same read() as any value, so a set/tagged
;; literal in metadata is a value, not a form. Carry it whether or not the value
;; itself changed (a set-form in the metadata of an otherwise-unchanged value).
(define (rdr-form->data x)
  (let ((v (rdr-form->data* x)) (m (jolt-meta x)))
    (if (jolt-nil? m) v (jolt-with-meta v (rdr-form->data m)))))

;; --- the two host seams -----------------------------------------------------
;; clojure.core/read-string: first form, or nil for blank / comment-only input
;; (parse-string wart, matched deliberately). jolt-read-form-raw keeps set FORMS
;; for the compiler spine (compile-eval); the data seam converts them to sets.
(define (jolt-read-form-raw s)
  (let-values (((form j) (rdr-read-form s 0 (string-length s))))
    (if (rdr-eof? form) jolt-nil form)))
(define (jolt-read-string s)
  (let ((form (jolt-read-form-raw s)))
    (if (jolt-nil? form) form (rdr-form->data form))))

;; __parse-next: [form rest-of-string] or nil when only whitespace/comments left.
(define (jolt-parse-next s)
  (let ((end (string-length s)))
    (let-values (((form j) (rdr-read-form s 0 end)))
      (if (rdr-eof? form)
          jolt-nil
          (jolt-vector (rdr-form->data form) (substring s j end))))))

;; __read-tagged: apply a built-in data reader to an already-read form. The tag
;; is the :#name keyword the reader produced; #uuid/#inst reuse the inst-time ctors.
(define (jolt-read-tagged tag form)
  (cond
    ((eq? tag (keyword #f "#uuid")) (jolt-uuid-from-string form))
    ((eq? tag (keyword #f "#inst")) (jolt-inst-from-string form))
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
