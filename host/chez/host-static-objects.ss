;; host-static-objects.ss — host object classes (ArrayList, HashMap, the
;; String/Reader/Writer/Tokenizer shims, BigInteger/MapEntry ctors, URL codecs)
;; and the tagged-table method dispatch + pluggable instance? hook. Continues
;; host-static-statics.ss; loaded last of the three.

;; ---- java.util.ArrayList ----------------------------------------------------
;; A mutable list backed by a growable Scheme vector. State is #(backing count);
;; .add amortizes O(1) and .get is O(1) (a list backing made both O(n)). medley's
;; stateful transducers (window / partition-between) build one with .add / .size /
;; .toArray / .clear / .remove. (ArrayList.) | (ArrayList. n) | (ArrayList. coll).
(define al-min-cap 8)
(define (al-vec self) (vector-ref (jhost-state self) 0))
(define (al-cnt self) (vector-ref (jhost-state self) 1))
(define (al-cnt! self n) (vector-set! (jhost-state self) 1 n))
(define (make-arraylist xs)               ; xs: a Scheme list of initial elements
  (let* ((n (length xs)) (cap (fxmax al-min-cap n)) (v (make-vector cap jolt-nil)))
    (let loop ((i 0) (xs xs)) (when (pair? xs) (vector-set! v i (car xs)) (loop (fx+ i 1) (cdr xs))))
    (make-jhost "arraylist" (vector v n))))
(define (al-ensure! self need)            ; grow the backing vector (doubling) to fit `need`
  (let ((v (al-vec self)))
    (when (fx>? need (vector-length v))
      (let grow ((cap (fxmax al-min-cap (vector-length v))))
        (if (fx<? cap need) (grow (fx* cap 2))
            (let ((nv (make-vector cap jolt-nil)))
              (let cp ((i 0)) (when (fx<? i (al-cnt self)) (vector-set! nv i (vector-ref v i)) (cp (fx+ i 1))))
              (vector-set! (jhost-state self) 0 nv)))))))
(define (al-push! self x)
  (let ((n (al-cnt self))) (al-ensure! self (fx+ n 1)) (vector-set! (al-vec self) n x) (al-cnt! self (fx+ n 1))))
(define (al-insert-at! self i x)
  (let ((n (al-cnt self)))
    (al-ensure! self (fx+ n 1))
    (let ((v (al-vec self)))
      (let shift ((j n)) (when (fx>? j i) (vector-set! v j (vector-ref v (fx- j 1))) (shift (fx- j 1))))
      (vector-set! v i x) (al-cnt! self (fx+ n 1)))))
(define (al-remove-at! self i)
  (let ((n (al-cnt self)) (v (al-vec self)))
    (let shift ((j i)) (when (fx<? j (fx- n 1)) (vector-set! v j (vector-ref v (fx+ j 1))) (shift (fx+ j 1))))
    (vector-set! v (fx- n 1) jolt-nil) (al-cnt! self (fx- n 1))))
(define (al->list self)                   ; first `count` elements as a Scheme list
  (let ((v (al-vec self)))
    (let loop ((i (fx- (al-cnt self) 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (vector-ref v i) acc))))))
(register-class-ctor! "ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))   ; initial capacity, ignored
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(register-class-ctor! "java.util.ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(register-host-methods! "arraylist"
  (list
    (cons "add" (lambda (self . a)
                  ;; (.add x) -> append+true; (.add i x) -> insert at i, returns nil.
                  (if (= 1 (length a))
                      (begin (al-push! self (car a)) #t)
                      (begin (al-insert-at! self (jnum->exact (car a)) (cadr a)) jolt-nil))))
    (cons "add!" (lambda (self x) (al-push! self x) #t))
    (cons "get" (lambda (self i) (vector-ref (al-vec self) (jnum->exact i))))
    (cons "set" (lambda (self i x)
                  (let* ((idx (jnum->exact i)) (old (vector-ref (al-vec self) idx)))
                    (vector-set! (al-vec self) idx x) old)))
    (cons "size" (lambda (self) (->num (al-cnt self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (al-cnt self))))
    (cons "remove" (lambda (self i)
                     (let* ((idx (jnum->exact i)) (old (vector-ref (al-vec self) idx)))
                       (al-remove-at! self idx) old)))
    (cons "clear" (lambda (self) (vector-set! (jhost-state self) 0 (make-vector al-min-cap jolt-nil)) (al-cnt! self 0) jolt-nil))
    (cons "contains" (lambda (self x) (and (memp (lambda (e) (jolt=2 e x)) (al->list self)) #t)))
    (cons "toArray" (lambda (self . _) (apply jolt-vector (al->list self))))
    (cons "iterator" (lambda (self) (make-jiterator (list->cseq (al->list self)))))
    (cons "toString" (lambda (self) (jolt-pr-str (list->cseq (al->list self)))))))

(register-class-ctor! "StringBuilder"
  (lambda args (make-jhost "string-builder"
    ;; a numeric first arg is a CAPACITY hint, not content.
    (vector (if (and (pair? args) (not (number? (car args)))) (render-piece (car args)) "")))))
(register-host-methods! "string-builder"
  (list (cons "append" (lambda (self x) (sb-set! self (string-append (sb-str self) (render-piece x))) self))
        (cons "toString" (lambda (self) (sb-str self)))
        (cons "length" (lambda (self) (->num (string-length (sb-str self)))))
        (cons "charAt" (lambda (self i) (string-ref (sb-str self) (jnum->exact i))))
        (cons "setLength" (lambda (self n)
                            (let ((cur (sb-str self)) (n (jnum->exact n)))
                              (sb-set! self (if (< n (string-length cur))
                                                (substring cur 0 n)
                                                (string-append cur (make-string (- n (string-length cur)) #\nul)))))
                            jolt-nil))))

;; ---- StringWriter -----------------------------------------------------------
;; Writer.write(int) writes the CHAR for that code; append(char) appends the char.
(define (writer-piece x) (if (number? x) (string (integer->char (jnum->exact x))) (render-piece x)))
(register-class-ctor! "StringWriter" (lambda args (make-jhost "writer" (vector ""))))
(register-host-methods! "writer"
  (list (cons "write" (lambda (self x) (sb-set! self (string-append (sb-str self) (writer-piece x))) jolt-nil))
        (cons "append" (lambda (self x) (sb-set! self (string-append (sb-str self) (render-piece x))) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) (sb-str self)))))

;; a file-backed writer (clojure.java.io/writer of a File/path): accumulates like
;; StringWriter, then persists to the path on flush/close, so
;; (with-open [w (io/writer "f")] (.write w …)) writes the file. State #(path buf).
(define (fw-path self) (vector-ref (jhost-state self) 0))
(define (fw-buf self) (vector-ref (jhost-state self) 1))
(define (fw-append! self s) (vector-set! (jhost-state self) 1 (string-append (fw-buf self) s)))
(define (fw-flush! self) (jolt-spit (fw-path self) (fw-buf self)))  ; jolt-spit: io.ss
(register-host-methods! "file-writer"
  (list (cons "write" (lambda (self x) (fw-append! self (writer-piece x)) jolt-nil))
        (cons "append" (lambda (self x) (fw-append! self (render-piece x)) self))
        (cons "flush" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "close" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "toString" (lambda (self) (fw-buf self)))))

;; a writer over a real Chez port — the values *out* / *err* hold. write/append
;; push to the port (so (.write *out* s) and (binding [*out* *err*] …) work);
;; it isn't a buffer, so toString is empty. Lets libraries that touch *out*/*err*
;; (tools.logging, selmer) compile and run.
(register-host-methods! "port-writer"
  (list (cons "write" (lambda (self x) (display (writer-piece x) (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "append" (lambda (self x) (display (render-piece x) (vector-ref (jhost-state self) 0)) self))
        (cons "flush" (lambda (self) (flush-output-port (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))
(def-var! "clojure.core" "*out*" (make-jhost "port-writer" (vector (current-output-port))))
(def-var! "clojure.core" "*err*" (make-jhost "port-writer" (vector (current-error-port))))

;; ---- java.util.HashMap ------------------------------------------------------
;; A mutable map keyed by jolt values (jolt-hash / jolt=2). State #(chez-hashtable).
;; Constructors: () | (capacity) | (capacity load-factor) [sizing args ignored] |
;; (Map m) [copy]. Enough of the Map surface for libraries that build a fast lookup
;; (malli's fast-registry: (doto (HashMap. 1024 0.25) (.putAll m)) then .get).
(define (hm-hash k) (let ((h (jolt-hash k)))
                      (bitwise-and (if (and (integer? h) (exact? h)) (abs h) 0) #x3FFFFFFF)))
(define (hm-tbl self) (vector-ref (jhost-state self) 0))
(define (hm-hashmap? x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
(define (hm-copy-into! ht src)            ; src: a jolt map or another hashmap
  (if (hm-hashmap? src)
      (vector-for-each (lambda (k) (hashtable-set! ht k (hashtable-ref (hm-tbl src) k jolt-nil)))
                       (hashtable-keys (hm-tbl src)))
      (for-each (lambda (e) (hashtable-set! ht (jolt-nth e 0) (jolt-nth e 1)))
                (seq->list (jolt-seq src)))))
(register-class-ctor! "HashMap"
  (lambda args
    (let ((ht (make-hashtable hm-hash jolt=2)))
      (when (and (pair? args) (or (pmap? (car args)) (hm-hashmap? (car args))))
        (hm-copy-into! ht (car args)))
      (make-jhost "hashmap" (vector ht)))))
(define (hm->pmap self)
  (let ((m (jolt-hash-map)))
    (vector-for-each (lambda (k) (set! m (jolt-assoc m k (hashtable-ref (hm-tbl self) k jolt-nil))))
                     (hashtable-keys (hm-tbl self)))
    m))
(register-host-methods! "hashmap"
  (list (cons "put" (lambda (self k v) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                          (hashtable-set! (hm-tbl self) k v) old)))
        (cons "get" (lambda (self k) (hashtable-ref (hm-tbl self) k jolt-nil)))
        (cons "getOrDefault" (lambda (self k d) (hashtable-ref (hm-tbl self) k d)))
        (cons "containsKey" (lambda (self k) (if (hashtable-contains? (hm-tbl self) k) #t #f)))
        (cons "containsValue" (lambda (self v)
          (let ((found #f))
            (vector-for-each (lambda (k) (when (jolt=2 v (hashtable-ref (hm-tbl self) k jolt-nil)) (set! found #t)))
                             (hashtable-keys (hm-tbl self))) found)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "remove" (lambda (self k) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                           (hashtable-delete! (hm-tbl self) k) old)))
        (cons "clear" (lambda (self) (hashtable-clear! (hm-tbl self)) jolt-nil))
        (cons "putAll" (lambda (self m) (hm-copy-into! (hm-tbl self) m) jolt-nil))
        (cons "keySet" (lambda (self) (apply jolt-hash-set (vector->list (hashtable-keys (hm-tbl self))))))
        (cons "values" (lambda (self) (apply jolt-vector
                          (map (lambda (k) (hashtable-ref (hm-tbl self) k jolt-nil))
                               (vector->list (hashtable-keys (hm-tbl self)))))))
        (cons "entrySet" (lambda (self) (jolt-seq (hm->pmap self))))
        (cons "toString" (lambda (self) (jolt-pr-str (hm->pmap self))))))

;; ---- StringReader -----------------------------------------------------------
;; state: a vector #(string pos marked).
(register-class-ctor! "StringReader"
  ;; src is a String or a char[] ((StringReader. (char-array s)) — selmer's parser
  ;; reads templates this way); a char-array becomes the string of its chars.
  (lambda (src . _)
    (make-jhost "string-reader"
      (vector (cond ((string? src) src)
                    ((jolt-array? src) (apply string-append (map jolt-str-render-one (seq->list (jolt-seq src)))))
                    (else (jolt-str-render-one src)))
              0 0))))
(define (sr-s self) (vector-ref (jhost-state self) 0))
(define (sr-pos self) (vector-ref (jhost-state self) 1))
(define (sr-pos! self p) (vector-set! (jhost-state self) 1 p))
(register-host-methods! "string-reader"
  (list (cons "read" (lambda (self)
                       (let ((s (sr-s self)) (p (sr-pos self)))
                         (if (>= p (string-length s)) -1   ; EOF -> exact int -1 (= JVM)
                             (begin (sr-pos! self (+ p 1)) (->num (char->integer (string-ref s p))))))))
        (cons "mark" (lambda (self . _) (vector-set! (jhost-state self) 2 (sr-pos self)) jolt-nil))
        (cons "reset" (lambda (self) (sr-pos! self (vector-ref (jhost-state self) 2)) jolt-nil))
        (cons "skip" (lambda (self n) (let ((n (jnum->exact n)))
                                        (sr-pos! self (min (string-length (sr-s self)) (+ (sr-pos self) n))) (->num n))))
        ;; readLine: the next line without its terminator (\n or \r\n), nil at EOF —
        ;; what line-seq drives over a BufferedReader.
        (cons "readLine"
          (lambda (self)
            (let ((s (sr-s self)) (p (sr-pos self)) (len (string-length (sr-s self))))
              (if (>= p len) jolt-nil
                  (let scan ((i p))
                    (cond
                      ((>= i len) (sr-pos! self len) (substring s p len))
                      ((char=? (string-ref s i) #\newline)
                       (sr-pos! self (+ i 1))
                       (substring s p (if (and (> i p) (char=? (string-ref s (- i 1)) #\return)) (- i 1) i)))
                      (else (scan (+ i 1)))))))))
        (cons "close" (lambda (self) jolt-nil))))

;; ---- PushbackReader ---------------------------------------------------------
;; state: a vector #(wrapped-reader pushed-list)
(register-class-ctor! "PushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (vector rdr '()))))
(define (read-unit r)        ; read one code unit (flonum) from any reader, -1 at EOF
  (record-method-dispatch r "read" jolt-nil))
(register-host-methods! "pushback-reader"
  (list (cons "read" (lambda (self)
                       (let ((pushed (vector-ref (jhost-state self) 1)))
                         (if (pair? pushed)
                             (begin (vector-set! (jhost-state self) 1 (cdr pushed)) (car pushed))
                             (read-unit (vector-ref (jhost-state self) 0))))))
        (cons "unread" (lambda (self ch)
                         (vector-set! (jhost-state self) 1
                           (cons (if (char? ch) (->num (char->integer ch)) ch) (vector-ref (jhost-state self) 1)))
                         jolt-nil))
        (cons "close" (lambda (self) jolt-nil))))

;; ---- StringTokenizer --------------------------------------------------------
;; state: a vector #(tokens-list pos)
(define (tokenize s delims)
  (let ((dset (string->list delims)))
    (let loop ((chars (string->list s)) (cur '()) (toks '()))
      (cond ((null? chars) (reverse (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            ((memv (car chars) dset)
             (loop (cdr chars) '() (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            (else (loop (cdr chars) (cons (car chars) cur) toks))))))
(register-class-ctor! "StringTokenizer"
  (lambda (s . delims) (make-jhost "string-tokenizer"
    (vector (tokenize (if (string? s) s (jolt-str-render-one s))
                      (if (null? delims) " \t\n\r\f" (car delims))) 0))))
(register-host-methods! "string-tokenizer"
  (list (cons "hasMoreTokens" (lambda (self) (< (vector-ref (jhost-state self) 1) (length (vector-ref (jhost-state self) 0)))))
        (cons "countTokens" (lambda (self) (->num (- (length (vector-ref (jhost-state self) 0)) (vector-ref (jhost-state self) 1)))))
        (cons "nextToken" (lambda (self)
                            (let ((toks (vector-ref (jhost-state self) 0)) (p (vector-ref (jhost-state self) 1)))
                              (if (< p (length toks))
                                  (begin (vector-set! (jhost-state self) 1 (+ p 1)) (list-ref toks p))
                                  (error #f "NoSuchElementException")))))))

;; ---- String / BigInteger / MapEntry constructors ----------------------------
;; (String. bytes [charset]) decodes bytes (a bytevector OR a jolt byte-array)
;; with the named charset (UTF-8 default; ISO-8859-1/latin1/ascii = one byte per
;; char); else stringify. clj-http-lite's body coercion is (String. ^[B body cs).
(define (string-charset-name rest)
  (if (pair? rest)
      (let ((c (car rest)))
        (cond ((string? c) c)
              ((and (jhost? c) (string=? (jhost-tag c) "charset"))
               (let ((p (assq 'name (jhost-state c)))) (if p (jolt-str-render-one (cdr p)) "UTF-8")))
              (else "UTF-8")))
      "UTF-8"))
(define (decode-bytevector bv rest)
  (let ((cs (ascii-string-down (string-charset-name rest))))
    (cond
      ((or (string=? cs "utf-8") (string=? cs "utf8")) (utf8->string bv))
      ((or (string=? cs "iso-8859-1") (string=? cs "latin1") (string=? cs "iso8859-1")
           (string=? cs "us-ascii") (string=? cs "ascii"))
       (list->string (map integer->char (bytevector->u8-list bv))))
      ((or (string=? cs "utf-16") (string=? cs "utf16") (string=? cs "utf-16be") (string=? cs "unicode"))
       (utf16->string bv (endianness big)))   ; respects a leading BOM
      ((string=? cs "utf-16le") (utf16->string bv (endianness little)))
      ((or (string=? cs "utf-32") (string=? cs "utf32") (string=? cs "utf-32be"))
       (utf32->string bv (endianness big)))
      ((string=? cs "utf-32le") (utf32->string bv (endianness little)))
      (else (guard (e (#t (list->string (map integer->char (bytevector->u8-list bv))))) (utf8->string bv))))))
(register-class-ctor! "String"
  (lambda (x . rest)
    (cond ((bytevector? x) (decode-bytevector x rest))
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (decode-bytevector (na-bytearray->bv x) rest))
          ((string? x) x)
          (else (jolt-str-render-one x)))))
(register-class-ctor! "BigInteger"
  (lambda (v) (parse-int-or-throw v 10 "BigInteger")))
(register-class-ctor! "MapEntry" (lambda (k v) (make-map-entry k v)))
;; JVM exception ctors -> a typed host throwable carrying the canonical :jolt/class
;; (so class / instance? / getMessage / ex-message reflect the real type) and the
;; message. Supports (E. msg), (E. msg cause), (E. cause), and (E.).
(for-each
  (lambda (nm)
    (let ((canonical (or (resolve-class-hint nm) nm)))
      (register-class-ctor! nm
        (lambda args
          (let* ((a0 (if (pair? args) (car args) jolt-nil))
                 (rest (if (pair? args) (cdr args) '()))
                 (cause (if (pair? rest) (car rest) jolt-nil)))
            (cond
              ((string? a0) (jolt-host-throwable canonical a0 cause))
              ((jolt-nil? a0) (jolt-host-throwable canonical jolt-nil))
              ;; (E. cause): a lone throwable arg is the cause, message nil.
              ((and (null? rest) (ex-info-map? a0)) (jolt-host-throwable canonical jolt-nil a0))
              (else (jolt-host-throwable canonical (jolt-str-render-one a0) cause))))))))
  '("Throwable" "Exception" "RuntimeException" "IllegalArgumentException" "IllegalStateException"
    "InterruptedException" "UnsupportedOperationException" "IOException" "NumberFormatException"
    "ArithmeticException" "NullPointerException" "ClassCastException" "IndexOutOfBoundsException"
    "FileNotFoundException" "UnsupportedEncodingException"))

;; ---- URLEncoder / URLDecoder (www-form-urlencoded) --------------------------
(define (url-unreserved? b)
  (or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90)) (and (>= b 97) (<= b 122))
      (= b 46) (= b 42) (= b 95) (= b 45)))
(define hex-digits "0123456789ABCDEF")
(define (url-encode s . _)
  (let ((bs (string->utf8 (if (string? s) s (jolt-str-render-one s)))) (out '()))
    (let loop ((i 0))
      (if (= i (bytevector-length bs)) (list->string (reverse out))
          (let ((b (bytevector-u8-ref bs i)))
            (cond ((url-unreserved? b) (set! out (cons (integer->char b) out)))
                  ((= b 32) (set! out (cons #\+ out)))
                  (else (set! out (cons (string-ref hex-digits (bitwise-and b 15))
                                   (cons (string-ref hex-digits (bitwise-arithmetic-shift-right b 4))
                                     (cons #\% out))))))
            (loop (+ i 1)))))))
(define (hexv c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
        (else (error #f "URLDecoder: malformed escape"))))
(define (url-decode s . _)
  (let* ((str (if (string? s) s (jolt-str-render-one s))) (n (string-length str)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (utf8->string (u8-list->bytevector (reverse out)))
          (let ((c (string-ref str i)))
            (cond ((char=? c #\+) (set! out (cons 32 out)) (loop (+ i 1)))
                  ((char=? c #\%)
                   (set! out (cons (+ (* 16 (hexv (string-ref str (+ i 1)))) (hexv (string-ref str (+ i 2)))) out))
                   (loop (+ i 3)))
                  (else (set! out (cons (char->integer c) out)) (loop (+ i 1)))))))))
(define (u8-list->bytevector lst)
  (let ((bv (make-bytevector (length lst))))
    (let loop ((l lst) (i 0)) (if (null? l) bv (begin (bytevector-u8-set! bv i (car l)) (loop (cdr l) (+ i 1)))))))
(register-class-statics! "URLEncoder" (list (cons "encode" url-encode)))
(register-class-statics! "URLDecoder" (list (cons "decode" url-decode)))
;; Charset/forName yields the canonical name STRING (not an opaque object) so it
;; threads straight into (.getBytes s cs) / (String. bytes cs), which take a name.
(register-class-statics! "Charset" (list (cons "forName" (lambda (nm) (jolt-str-render-one nm)))))

;; ---- Base64 (RFC 4648) ------------------------------------------------------
(define b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define (->bytevector x)
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        ((string? x) (string->utf8 x))
        (else (string->utf8 (jolt-str-render-one x)))))
(define (b64-encode x)
  (let* ((bs (->bytevector x)) (n (bytevector-length bs)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (list->string (reverse out))
          (let* ((b0 (bytevector-u8-ref bs i))
                 (b1 (if (< (+ i 1) n) (bytevector-u8-ref bs (+ i 1)) #f))
                 (b2 (if (< (+ i 2) n) (bytevector-u8-ref bs (+ i 2)) #f)))
            (set! out (cons (string-ref b64-alphabet (bitwise-arithmetic-shift-right b0 2)) out))
            (set! out (cons (string-ref b64-alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b0 3) 4)
                                                                  (bitwise-arithmetic-shift-right (or b1 0) 4))) out))
            (set! out (cons (if b1 (string-ref b64-alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 15) 2)
                                                                         (bitwise-arithmetic-shift-right (or b2 0) 6))) #\=) out))
            (set! out (cons (if b2 (string-ref b64-alphabet (bitwise-and b2 63)) #\=) out))
            (loop (+ i 3)))))))
(define (b64-char-val c)
  (let loop ((i 0)) (cond ((= i 64) (error #f "Base64: illegal character")) ((char=? (string-ref b64-alphabet i) c) i) (else (loop (+ i 1))))))
(define (b64-decode x)
  (let* ((str (let ((s (if (string? x) x (utf8->string (->bytevector x)))))
                (list->string (filter (lambda (c) (not (char=? c #\=))) (string->list s)))))
         (out '()) (acc 0) (bits 0))
    (for-each (lambda (c)
                (set! acc (bitwise-ior (bitwise-arithmetic-shift-left acc 6) (b64-char-val c)))
                (set! bits (+ bits 6))
                (when (>= bits 8)
                  (set! bits (- bits 8))
                  (set! out (cons (bitwise-and (bitwise-arithmetic-shift-right acc bits) 255) out))))
              (string->list str))
    (u8-list->bytevector (reverse out))))
(register-host-methods! "b64-encoder"
  (list (cons "encode" (lambda (self bs) (string->utf8 (b64-encode bs))))
        (cons "encodeToString" (lambda (self bs) (b64-encode bs)))))
(register-host-methods! "b64-decoder"
  (list (cons "decode" (lambda (self s) (b64-decode s)))))
(register-class-statics! "Base64"
  (list (cons "getEncoder" (lambda () (make-jhost "b64-encoder" '())))
        (cons "getDecoder" (lambda () (make-jhost "b64-decoder" '())))))

;; ---- java.util.regex.Pattern ------------------------------------------------
;; Pattern/compile returns a jolt-regex value (regex-t), so str/replace, re-find,
;; .split etc. accept it transparently.
(define pattern-multiline 8.0)
(define (pattern-quote s)
  (let ((meta "\\.[]{}()*+-?^$|&") (s (if (string? s) s (jolt-str-render-one s))) (out '()))
    (let loop ((i 0))
      (if (= i (string-length s)) (list->string (reverse out))
          (let ((c (string-ref s i)))
            (when (memv c (string->list meta)) (set! out (cons #\\ out)))
            (set! out (cons c out))
            (loop (+ i 1)))))))
(register-class-statics! "Pattern"
  (list (cons "compile" (lambda (s . flags)
                          (if (and (pair? flags) (= (bitwise-and (jnum->exact (car flags)) 8) 8))
                              (jolt-regex (string-append "(?m)" s))
                              (jolt-regex s))))
        (cons "quote" (lambda (s) (pattern-quote s)))
        (cons "MULTILINE" pattern-multiline)))
;; record-method-dispatch already routes string? -> jolt-string-method. Add a
;; regex-t arm (Pattern .split / .matcher-less surface used by corpus) by wrapping
;; once more — a regex-t isn't a jhost.
(define %hs-rmd2 record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (if (regex-t? obj)
        (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
          (cond ((string=? method-name "split")
                 ;; .split returns a String[] — a seq (prints
                 ;; (a b c), not a vector). re-split with no limit; drop trailing
                 ;; empties (JVM default).
                 (let ((parts (re-split (regex-t-irx obj) (car rest) #f)))
                   (list->cseq (str-split-drop-trailing parts))))
                ((string=? method-name "pattern") (regex-t-source obj))
                (else (error #f (string-append "No method " method-name " on Pattern")))))
        (%hs-rmd2 obj method-name rest-args))))

;; ---- def-var! the registry entry points so emit can also reach them ---------
(def-var! "clojure.core" "host-static-ref" host-static-ref)
(def-var! "clojure.core" "host-static-call" (lambda (c m . a) (apply host-static-call c m a)))
(def-var! "clojure.core" "host-new" (lambda (c . a) (apply host-new c a)))

;; Clojure-visible class-registration hooks. A host shim (e.g. reitit.trie-jolt,
;; which mirrors the reitit.Trie Java class) registers a constructor proc or a
;; map of static members against a class token so (Class. args) / (Class/member
;; args) resolve to it. The statics argument is a jolt map {member-name -> val}.
(define (jmap->static-alist m)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s) acc
        (let ((e (jolt-first s)))
          (loop (jolt-seq (jolt-rest s)) (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc))))))
(def-var! "clojure.core" "__register-class-ctor!"
  (lambda (name proc) (register-class-ctor! name proc) jolt-nil))
(def-var! "clojure.core" "__register-class-statics!"
  (lambda (name members) (register-class-statics! name (jmap->static-alist members)) jolt-nil))

;; ---- tagged-table method dispatch + pluggable instance? --------------------
;; A jolt library can build stateful host objects with (jolt.host/tagged-table
;; tag) and dispatch (.method obj ...) to handlers registered here, keyed by the
;; table's "jolt/type" tag — the htable analogue of the jhost method registry
;; above. jolt-lang/http-client uses this to emulate java.net URL /
;; HttpURLConnection / java.io byte streams so clj-http-lite runs unchanged.
(define tagged-methods-tbl (make-hashtable string-hash string=?))   ; tag-key -> (method-ht)
(define (tag->method-key tag)
  (if (keyword-t? tag)
      (let ((ns (keyword-t-ns tag)))
        (if (and ns (not (jolt-nil? ns))) (string-append ns "/" (keyword-t-name tag)) (keyword-t-name tag)))
      (jolt-str-render-one tag)))
(define (register-tagged-methods! tag members)
  (let* ((key (tag->method-key tag))
         (h (or (hashtable-ref tagged-methods-tbl key #f)
                (let ((nh (make-hashtable string-hash string=?)))
                  (hashtable-set! tagged-methods-tbl key nh) nh))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

;; htable arm: dispatch (.method obj a*) through the table's tag method registry;
;; an unregistered method falls through (sorted colls are htables too).
(define %hs-rmd-htable record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (let ((tag (and (htable? obj) (hashtable-ref (htable-h obj) "jolt/type" #f))))
      (let* ((mh (and tag (hashtable-ref tagged-methods-tbl (tag->method-key tag) #f)))
             (f  (and mh (hashtable-ref mh method-name #f))))
        (if f
            (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
            (%hs-rmd-htable obj method-name rest-args))))))

(def-var! "clojure.core" "__register-class-methods!"
  (lambda (tag members) (register-tagged-methods! tag (jmap->static-alist members)) jolt-nil))

;; Pluggable instance? — a library registers (fn [class-name-string val] -> true
;; | false | nil); nil means "not my class, fall through". First non-nil wins.
(define user-instance-checks '())
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tname (symbol-t-name type-sym)))
      (let loop ((fs user-instance-checks))
        (if (null? fs)
            'pass
            (let ((r ((car fs) tname val)))
              (if (jolt-nil? r) (loop (cdr fs)) (if (jolt-truthy? r) #t #f))))))))
(def-var! "clojure.core" "__register-instance-check!"
  (lambda (f) (set! user-instance-checks (append user-instance-checks (list f))) jolt-nil))

;; (jolt.host/table? x) — is x a host tagged-table?
(def-var! "jolt.host" "table?" (lambda (x) (if (htable? x) #t #f)))
