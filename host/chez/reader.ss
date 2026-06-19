;; Chez-side Clojure data reader (jolt-r8ku, inc Y).
;;
;; The data half of runtime read/eval: a recursive-descent reader that parses
;; ONE Clojure form off a string and produces the same jolt runtime values the
;; Janet reader's parse-next yields (the analyzer/eval half — eval, load-string,
;; runtime defmacro — stays Phase-3, it needs the compiler at runtime). Two host
;; seams hang off it, matching the Janet seed (eval_runtime.janet):
;;   read-string  : string -> first form (clojure.core seam, src 772)
;;   __parse-next : string -> [form rest] | nil  (the *in* family seam, src 801)
;; read / read+string / with-in-str / line-seq / clojure.edn are Clojure over
;; these (jolt-core/clojure/core/50-io.clj, src/jolt/clojure/edn.clj).
;;
;; Form shapes are pinned to the Janet reader's output (probed against build/jolt):
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

(define (rdr-ws? c)
  (or (char-whitespace? c) (char=? c #\,)))

(define (rdr-terminator? c)
  (or (rdr-ws? c)
      (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\@ #\^ #\' #\` #\~ #\\))))

(define (rdr-digit? c) (and (char>=? c #\0) (char<=? c #\9)))

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
;; parses. Ratios and big-N/M decimals follow the seed's all-double rendering
;; for division; ints/bignums stay exact (Chez's tower IS Clojure's).
(define (rdr-string-index-char str c)
  (let ((n (string-length str)))
    (let loop ((i 0))
      (cond ((>= i n) #f)
            ((char=? (string-ref str i) c) i)
            (else (loop (+ i 1)))))))

;; jolt models EVERY number as a double (emit-const lowers integer literals to
;; flonums too), so the reader coerces every parsed number to inexact — else a
;; read int (exact) is not jolt= to a source int literal (flonum).
(define (rdr-try-number tok)
  (let ((raw (rdr-try-number-raw tok)))
    (and raw (exact->inexact raw))))

(define (rdr-try-number-raw tok)
  (let ((len (string-length tok)))
    (and (> len 0)
         (let* ((c0 (string-ref tok 0))
                (signed (or (char=? c0 #\+) (char=? c0 #\-)))
                (start (if signed 1 0)))
           (and (> len start)
                (rdr-digit? (string-ref tok start))
                (rdr-number-body tok start signed c0))))))

(define (rdr-number-body tok start signed sign-ch)
  (let* ((sign (if (and signed (char=? sign-ch #\-)) -1 1))
         (len (string-length tok))
         (body (substring tok start len))
         (blen (string-length body))
         (slash (rdr-string-index-char body #\/)))
    (cond
      ;; ratio a/b -> flonum (the seed has no exact ratios)
      (slash
       (let ((n (string->number (substring body 0 slash)))
             (d (string->number (substring body (+ slash 1) blen))))
         (and (integer? n) (integer? d) (not (= d 0))
              (* sign (exact->inexact (/ n d))))))
      ;; hex 0x..
      ((and (>= blen 2) (char=? (string-ref body 0) #\0)
            (or (char=? (string-ref body 1) #\x) (char=? (string-ref body 1) #\X)))
       (let ((h (string->number (substring body 2 blen) 16)))
         (and h (* sign h))))
      ;; bigint suffix N
      ((and (> blen 1) (char=? (string-ref body (- blen 1)) #\N))
       (let ((n (string->number (substring body 0 (- blen 1)))))
         (and n (integer? n) (* sign n))))
      ;; bigdecimal suffix M -> double
      ((and (> blen 1) (char=? (string-ref body (- blen 1)) #\M))
       (let ((n (string->number (substring body 0 (- blen 1)))))
         (and n (exact->inexact (* sign n)))))
      (else
       (let ((n (string->number tok)))   ; tok carries its own sign
         (and (number? n) (real? n)
              ;; never surface an exact non-integer ratio
              (if (and (exact? n) (not (integer? n))) (exact->inexact n) n)))))))

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
    (when (>= i end) (jolt-throw (jolt-ex-info "EOF while reading string" (empty-pmap))))
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
             ((#\0) (loop (+ i 2) (cons #\nul acc)))
             ((#\u)
              (let-values (((cp j) (rdr-hex->int s (+ i 2) 4)))
                (loop j (cons (integer->char cp) acc))))
             (else (loop (+ i 2) (cons e acc))))))
        (else (loop (+ i 1) (cons c acc)))))))

;; backslash already consumed; read a Clojure character literal.
(define (rdr-read-char s i end)
  (when (>= i end) (jolt-throw (jolt-ex-info "EOF while reading char" (empty-pmap))))
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
                                    (empty-pmap))))))

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
        ((>= i end) (jolt-throw (jolt-ex-info "EOF while reading" (empty-pmap))))
        ((char=? (string-ref s i) close) (values (reverse acc) (+ i 1)))
        (else
         (let-values (((form j) (rdr-read-form s i end)))
           (if (rdr-eof? form)
               (loop j acc)             ; a #_ discard or close — re-check at j
               (loop j (cons form acc)))))))))

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
    ((symbol-t? m) (jolt-hash-map rdr-kw-tag (symbol-t-name m)))
    ((string? m) (jolt-hash-map rdr-kw-tag m))
    ((pmap? m) m)
    (else (jolt-hash-map rdr-kw-tag m))))

(define (rdr-merge-meta old new)
  (if (pmap? old)
      (pmap-fold new (lambda (k v acc) (jolt-assoc1 acc k v)) old)
      new))

(define (rdr-attach-meta target meta)
  (if (symbol-t? target)
      (make-symbol-t (symbol-t-ns target) (symbol-t-name target)
                     (rdr-merge-meta (symbol-t-meta target) meta))
      target))                           ; non-symbol meta is dropped (corpus only hints symbols)

;; --- # dispatch -------------------------------------------------------------
(define (rdr-read-dispatch s i end)      ; i points just past the '#'
  (when (>= i end) (jolt-throw (jolt-ex-info "EOF after #" (empty-pmap))))
  (let ((c (string-ref s i)))
    (cond
      ((char=? c #\{)                    ; #{...} set
       (let-values (((elems j) (rdr-read-seq s (+ i 1) end #\})))
         (values (rdr-make-set elems) j)))
      ((char=? c #\")                    ; #"..." regex -> tagged :regex (raw source)
       (let-values (((src j) (rdr-read-regex s (+ i 1) end)))
         (values (rdr-make-tagged (keyword #f "regex") src) j)))
      ((char=? c #\_)                    ; #_ discard the next form
       (let-values (((_ j) (rdr-read-form s (+ i 1) end)))
         (when (rdr-eof? _) (jolt-throw (jolt-ex-info "EOF after #_" (empty-pmap))))
         (rdr-read-form s j end)))
      ((char=? c #\')                    ; #'x var-quote -> (var x)
       (let-values (((form j) (rdr-read-form s (+ i 1) end)))
         (values (jolt-list (jolt-symbol #f "var") form) j)))
      (else                              ; #tag form -> tagged {:tag :#tag :form ...}
       (let-values (((tok j) (rdr-read-token s i end)))
         (let-values (((form k) (rdr-read-form s j end)))
           (when (rdr-eof? form) (jolt-throw (jolt-ex-info "EOF after #tag" (empty-pmap))))
           (values (rdr-make-tagged (keyword #f (string-append "#" tok)) form) k)))))))

;; regex literal source: raw chars to the closing quote; \" is an escaped quote,
;; every other backslash sequence is kept verbatim (regex engine semantics).
(define (rdr-read-regex s i end)
  (let loop ((i i) (acc '()))
    (when (>= i end) (jolt-throw (jolt-ex-info "EOF while reading regex" (empty-pmap))))
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
  ;; ::kw auto-resolves; the seed drops the ns, so skip a second ':'
  (let ((i (if (and (< i end) (char=? (string-ref s i) #\:)) (+ i 1) i)))
    (let-values (((tok j) (rdr-read-token s i end)))
      (let-values (((ns name) (rdr-sym-parts tok)))
        (values (keyword ns name) j)))))

;; --- the main dispatch ------------------------------------------------------
;; Returns (values form j). form is rdr-eof at end-of-input or at an unconsumed
;; close delimiter (read-seq consumes the close itself).
(define (rdr-read-form s i end)
  (let ((i (rdr-skip-ws s i end)))
    (if (>= i end)
        (values rdr-eof i)
        (let ((c (string-ref s i)))
          (cond
            ((char=? c #\() (let-values (((es j) (rdr-read-seq s (+ i 1) end #\))))
                              (values (apply jolt-list es) j)))
            ((char=? c #\[) (let-values (((es j) (rdr-read-seq s (+ i 1) end #\])))
                              (values (apply jolt-vector es) j)))
            ((char=? c #\{) (let-values (((es j) (rdr-read-seq s (+ i 1) end #\})))
                              (values (apply jolt-hash-map es) j)))
            ((or (char=? c #\)) (char=? c #\]) (char=? c #\}))
             (values rdr-eof i))         ; unconsumed close — read-seq handles it
            ((char=? c #\") (rdr-read-string-lit s (+ i 1) end))
            ((char=? c #\\) (rdr-read-char s (+ i 1) end))
            ((char=? c #\:) (rdr-read-keyword s (+ i 1) end))
            ((char=? c #\#) (rdr-read-dispatch s (+ i 1) end))
            ((char=? c #\') (rdr-wrap s (+ i 1) end (jolt-symbol #f "quote")))
            ((char=? c #\`) (rdr-wrap s (+ i 1) end (jolt-symbol #f "syntax-quote")))
            ((char=? c #\@) (rdr-wrap s (+ i 1) end (jolt-symbol "clojure.core" "deref")))
            ((char=? c #\~)
             (if (and (< (+ i 1) end) (char=? (string-ref s (+ i 1)) #\@))
                 (rdr-wrap s (+ i 2) end (jolt-symbol #f "unquote-splicing"))
                 (rdr-wrap s (+ i 1) end (jolt-symbol #f "unquote"))))
            ((char=? c #\^)
             (let-values (((mform j) (rdr-read-form s (+ i 1) end)))
               (let-values (((target k) (rdr-read-form s j end)))
                 (when (rdr-eof? target)
                   (jolt-throw (jolt-ex-info "EOF after ^meta" (empty-pmap))))
                 (values (rdr-attach-meta target (rdr-meta-map mform)) k))))
            (else
             (let-values (((tok j) (rdr-read-token s i end)))
               (values (rdr-token->value tok) j))))))))

;; wrap the next form in a 2-element list (READER-MACRO form)
(define (rdr-wrap s i end head)
  (let-values (((form j) (rdr-read-form s i end)))
    (when (rdr-eof? form)
      (jolt-throw (jolt-ex-info "EOF while reading reader macro" (empty-pmap))))
    (values (jolt-list head form) j)))

;; --- the two host seams -----------------------------------------------------
;; clojure.core/read-string: first form, or nil for blank / comment-only input
;; (the seed's parse-string wart, matched deliberately).
(define (jolt-read-string s)
  (let-values (((form j) (rdr-read-form s 0 (string-length s))))
    (if (rdr-eof? form) jolt-nil form)))

;; __parse-next: [form rest-of-string] or nil when only whitespace/comments left.
(define (jolt-parse-next s)
  (let ((end (string-length s)))
    (let-values (((form j) (rdr-read-form s 0 end)))
      (if (rdr-eof? form)
          jolt-nil
          (jolt-vector form (substring s j end))))))

;; __read-tagged: apply a built-in data reader to an already-read form. The tag
;; is the :#name keyword the reader produced; #uuid/#inst reuse the inc X ctors.
(define (jolt-read-tagged tag form)
  (cond
    ((eq? tag (keyword #f "#uuid")) (jolt-uuid-from-string form))
    ((eq? tag (keyword #f "#inst")) (jolt-inst-from-string form))
    (else (jolt-throw (jolt-ex-info (string-append "No reader function for tag " (jolt-pr-str tag))
                                    (empty-pmap))))))

(def-var! "clojure.core" "read-string" jolt-read-string)
(def-var! "clojure.core" "__parse-next" jolt-parse-next)
(def-var! "clojure.core" "__read-tagged" jolt-read-tagged)
