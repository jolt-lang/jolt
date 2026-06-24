;; natives-str.ss — java.lang.String method interop on Chez.
;;
;; (.method s arg*) on a string target lowers to record-method-dispatch (emit.ss),
;; which falls through to jolt-string-method here when the target is a string.
;; Covers the
;; portable java.lang.String/CharSequence methods cljc libraries actually call.
;; Case mapping is ASCII (the whole engine is byte-oriented), indexOf returns -1
;; on miss as on the JVM, indices come in as flonums, char results are Scheme
;; chars, and numeric results are flonums to match jolt's number model.
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
(define (ascii-string-up s) (list->string (map ascii-up-char (string->list s))))
(define (ascii-string-down s) (list->string (map ascii-down-char (string->list s))))

;; --- ASCII trim: drop leading/trailing chars with code <= space (JVM .trim) ---
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
            ((char<=? (string-ref s i) #\space) (loop (fx+ i 1)))
            (else (substring s i len))))))
(define (str-trimr s)
  (let loop ((j (fx- (string-length s) 1)))
    (cond ((fx<? j 0) "")
          ((char<=? (string-ref s j) #\space) (loop (fx- j 1)))
          (else (substring s 0 (fx+ j 1))))))

;; --- substring search: first index of `needle` in `s` at/after `from`, or -1 --
(define (str-index-of s needle from)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (max 0 from)))
      (cond ((fx>? (fx+ i nlen) slen) -1)
            ((string=? (substring s i (fx+ i nlen)) needle) i)
            (else (loop (fx+ i 1)))))))
(define (str-last-index-of s needle)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (fx- slen nlen)) (found -1))
      (cond ((fx<? i 0) found)
            ((string=? (substring s i (fx+ i nlen)) needle) i)
            (else (loop (fx- i 1) found))))))

;; A needle arg: a char value -> its 1-char string; a number -> the char at that
;; code point (JVM treats an int arg to indexOf as a char code); else a string.
(define (str-needle x)
  (cond ((char? x) (string x))
        ((number? x) (string (integer->char (exact (truncate x)))))
        ((string? x) x)
        (else (jolt-str x))))

;; literal replace-all (JVM String.replace(CharSequence,CharSequence)).
(define (str-replace-literal s a b)
  (let ((alen (string-length a)) (slen (string-length s)))
    (if (fx=? alen 0) s
        (let loop ((i 0) (acc '()))
          (cond ((fx>? (fx+ i alen) slen)
                 (apply string-append (reverse (cons (substring s i slen) acc))))
                ((string=? (substring s i (fx+ i alen)) a)
                 (loop (fx+ i alen) (cons b acc)))
                (else (loop (fx+ i 1) (cons (substring s i (fx+ i 1)) acc))))))))

;; A compiled irregex for a plain-string Java-regex pattern (or a jolt-regex).
(define (str-irx pat) (regex-t-irx (jolt-re-pattern pat)))

;; JVM String.split: split fully, then drop trailing empty strings.
(define (str-split-drop-trailing parts)
  (let loop ((p (reverse parts)))
    (if (and (pair? p) (string=? (car p) "")) (loop (cdr p)) (reverse p))))

;; Encode a string to bytes (a bytevector) under a named charset. UTF-8 default;
;; ISO-8859-1/latin1/ascii are one byte per char; UTF-16/UTF-32 via Chez's codecs
;; (plain "UTF-16" emits a big-endian BOM then BE, matching the JVM). Shared by
;; .getBytes and decode-bytevector (String.).
(define (charset-encode-bv s csname)
  (let ((cs (ascii-string-down (if (string? csname) csname (jolt-str-render-one csname)))))
    (cond
      ((or (string=? cs "utf-8") (string=? cs "utf8")) (string->utf8 s))
      ((member cs '("iso-8859-1" "latin1" "iso8859-1" "us-ascii" "ascii"))
       (let* ((n (string-length s)) (bv (make-bytevector n)))
         (do ((i 0 (+ i 1))) ((= i n) bv)
           (bytevector-u8-set! bv i (bitwise-and (char->integer (string-ref s i)) #xff)))))
      ((string=? cs "utf-16be") (string->utf16 s (endianness big)))
      ((string=? cs "utf-16le") (string->utf16 s (endianness little)))
      ((or (string=? cs "utf-16") (string=? cs "utf16") (string=? cs "unicode"))
       (let ((be (string->utf16 s (endianness big))))
         (let* ((n (bytevector-length be)) (bv (make-bytevector (+ n 2))))
           (bytevector-u8-set! bv 0 #xfe) (bytevector-u8-set! bv 1 #xff)
           (bytevector-copy! be 0 bv 2 n) bv)))
      ((or (string=? cs "utf-32be") (string=? cs "utf-32") (string=? cs "utf32"))
       (string->utf32 s (endianness big)))
      ((string=? cs "utf-32le") (string->utf32 s (endianness little)))
      (else (string->utf8 s)))))

(define (jolt-string-method method s rest)
  (define (arg n) (list-ref rest n))
  (cond
    ((string=? method "toString") s)
    ((string=? method "toLowerCase") (ascii-string-down s))
    ((string=? method "toUpperCase") (ascii-string-up s))
    ((string=? method "trim") (str-trim s))
    ((string=? method "length") (string-length s))   ; exact int (= JVM)
    ((string=? method "isEmpty") (fx=? (string-length s) 0))
    ((string=? method "charAt") (string-ref s (jolt->idx (arg 0))))
    ((string=? method "substring")
     (substring s (jolt->idx (arg 0))
                (if (fx>? (length rest) 1) (jolt->idx (arg 1)) (string-length s))))
    ((string=? method "indexOf")
     (str-index-of s (str-needle (arg 0))
                   (if (fx>? (length rest) 1) (jolt->idx (arg 1)) 0)))
    ((string=? method "lastIndexOf")
     (str-last-index-of s (str-needle (arg 0))))
    ((string=? method "startsWith")
     (let ((p (arg 0))) (and (fx>=? (string-length s) (string-length p))
                             (string=? (substring s 0 (string-length p)) p))))
    ((string=? method "endsWith")
     (let ((p (arg 0)) (slen (string-length s)))
       (and (fx>=? slen (string-length p))
            (string=? (substring s (fx- slen (string-length p)) slen) p))))
    ((string=? method "contains")
     (fx>=? (str-index-of s (str-needle (arg 0)) 0) 0))
    ((string=? method "concat") (string-append s (arg 0)))
    ((string=? method "replace") (str-replace-literal s (str-needle (arg 0)) (str-needle (arg 1))))
    ((string=? method "equalsIgnoreCase")
     (string=? (ascii-string-down s) (ascii-string-down (arg 0))))
    ((string=? method "compareTo")
     (let ((o (arg 0))) (cond ((string<? s o) -1.0) ((string>? s o) 1.0) (else 0.0))))
    ((string=? method "getBytes")
     ;; (.getBytes s) / (.getBytes s charset) -> a jolt byte-array (seqable /
     ;; countable / alength-able, like (byte-array …)); the JVM returns byte[].
     (na-byte-array
      (charset-encode-bv s (if (null? rest)
                               "utf-8"
                               (if (string? (arg 0)) (arg 0) (jolt-str-render-one (arg 0)))))))
    ((string=? method "matches") (if (irregex-match (str-irx (arg 0)) s) #t #f))
    ((string=? method "replaceAll") (irregex-replace/all (str-irx (arg 0)) s (arg 1)))
    ((string=? method "replaceFirst") (irregex-replace (str-irx (arg 0)) s (arg 1)))
    ((string=? method "split")
     (apply jolt-vector (str-split-drop-trailing (irregex-split (str-irx (arg 0)) s))))
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
    (else (error #f (string-append "No method " method " for value")))))

;; --- clojure.core str-* primitives (the substrate clojure.string.clj calls) ---
;; clojure.string.clj is pure Clojure over these
;; natives; def-var!'d here so the emitted
;; clojure.string prelude tier's var-derefs resolve:
;; string/ascii-* (ASCII), string/find (index or nil), core-str-* (regex|literal).

;; (string/split sep s) -> parts, splitting on each non-overlapping sep.
(define (str-literal-split s sep)
  (let ((slen (string-length s)) (plen (string-length sep)))
    (if (fx=? plen 0)
        (map string (string->list s))
        (let loop ((i 0) (start 0) (acc '()))
          (cond ((fx>? (fx+ i plen) slen)
                 (reverse (cons (substring s start slen) acc)))
                ((string=? (substring s i (fx+ i plen)) sep)
                 (loop (fx+ i plen) (fx+ i plen) (cons (substring s start i) acc)))
                (else (loop (fx+ i 1) start acc)))))))

(define (str-upper s) (ascii-string-up s))
(define (str-lower s) (ascii-string-down s))
(define (str-reverse-b s) (list->string (reverse (string->list s))))

;; (str-find needle haystack) -> exact int index of first occurrence, or nil.
(define (str-find needle s)
  (let ((i (str-index-of s needle 0)))
    (if (fx<? i 0) jolt-nil i)))

;; (str-join coll [sep]) -> stringify each element (Clojure str), join by sep.
;; str-join-strs (defined below) does the join; here we just render each element.
(define (str-join coll . opt)
  (let ((sep (if (pair? opt) (jolt-str-render-one (car opt)) "")))
    (str-join-strs (map jolt-str-render-one (seq->list coll)) sep)))

;; (re-split irx s limit) -> parts, splitting at each match. Keeps interior AND
;; trailing empty strings (the clojure.string wrapper drops trailing for limit 0);
;; a positive limit yields at most `limit` parts (the rest kept unsplit).
;; The clojure.string.clj split wrapper
;; layers the trailing-empty trim on top.
(define (re-split irx s limit)
  (let ((len (string-length s)))
    (let loop ((start 0) (last 0) (out '()))
      (if (and limit (fx>=? (length out) (fx- limit 1)))
          (reverse (cons (substring s last len) out))
          (let ((m (and (fx<=? start len) (irregex-search irx s start))))
            (if (not m)
                (reverse (cons (substring s last len) out))
                (let ((ms (irregex-match-start-index m 0))
                      (me (irregex-match-end-index m 0)))
                  (if (fx=? me ms)                 ; zero-width: step past to avoid a stall
                      (if (fx>=? start len)
                          (reverse (cons (substring s last len) out))
                          (loop (fx+ start 1) last out))
                      (loop me me (cons (substring s last ms) out))))))))))

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

;; $0/$1... expansion in a string replacement against an irregex match (the
;; JVM/seed replacement syntax). $N -> group N's text (dropped if non-matching).
(define (expand-dollar repl m)
  (let ((len (string-length repl)))
    (let loop ((i 0) (acc '()))
      (if (fx>=? i len)
          (apply string-append (reverse acc))
          (let ((c (string-ref repl i)))
            (if (and (char=? c #\$) (fx<? (fx+ i 1) len)
                     (char<=? #\0 (string-ref repl (fx+ i 1)))
                     (char<=? (string-ref repl (fx+ i 1)) #\9))
                (let* ((n (fx- (char->integer (string-ref repl (fx+ i 1))) 48))
                       (g (and (fx<=? n (irregex-match-num-submatches m))
                               (irregex-match-substring m n))))
                  (loop (fx+ i 2) (if g (cons g acc) acc)))
                (loop (fx+ i 1) (cons (string c) acc))))))))

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
      (let ((m (and (fx<=? start len) (irregex-search irx s start))))
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
      (str-replace-literal s pat repl)))
(define (str-replace-literal-first s a b)
  (let ((alen (string-length a)) (i (str-index-of s a 0)))
    (if (fx<? i 0) s
        (string-append (substring s 0 i) b (substring s (fx+ i alen) (string-length s))))))
(define (str-replace pat repl s)
  (if (jolt-regex? pat)
      (re-replace (regex-t-irx pat) s repl #f)
      (str-replace-literal-first s pat repl)))

(def-var! "clojure.core" "str-upper" str-upper)
(def-var! "clojure.core" "str-lower" str-lower)
(def-var! "clojure.core" "str-trim" str-trim)
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
                               (else (scan (cdr xs)))))))
        (when (and target (not filtered))
          (chez-register-refer-all! (chez-current-ns) target))))
    specs)
  jolt-nil)
(def-var! "clojure.core" "use" chez-runtime-use)
;; import: bring a deftype/defrecord from another ns into the current one. A spec
;; [from-ns Type ...] binds each Type's ctor closure under the current ns, so its
;; (Type. ...) constructor (host-new resolves it as a var) works after :import.
(define (chez-runtime-import . specs)
  (for-each
    (lambda (spec)
      (let ((items (cond ((pvec? spec) (seq->list spec))
                         ((or (cseq? spec) (empty-list-t? spec)) (seq->list spec))
                         (else '()))))
        (when (and (pair? items) (symbol-t? (car items)))
          (let ((from (symbol-t-name (car items))))
            (for-each
              (lambda (tn)
                (when (symbol-t? tn)
                  (let ((c (var-cell-lookup from (symbol-t-name tn))))
                    (when (and c (var-cell-defined? c))
                      (def-var! (chez-current-ns) (symbol-t-name tn) (var-cell-root c))))))
              (cdr items))))))
    specs)
  jolt-nil)
(def-var! "clojure.core" "import" chez-runtime-import)
