;; java.io.File + host file I/O (jolt-yyud). A
;; Chez-native implementation over Chez's filesystem primitives. A File is a
;; path-backed jfile record: (instance? java.io.File f) is true, str/slurp coerce
;; it to its path, and the File method surface (getName/getPath/exists/
;; isDirectory/isFile/listFiles) dispatches through record-method-dispatch.
;;
;; Provides make-file/file?/slurp/spit/flush/dir?/
;; list-dir for the overlay file-seq (20-coll.clj), which calls __file?/__dir?/
;; __list-dir + the .isDirectory/.listFiles/.isFile method surface.
;;
;; Reader/StringReader-coupled io (io/reader, line-seq over a file, .toURL,
;; slurp over a reader) is deferred to jolt-at0a. Loaded LAST in rt.ss, after
;; dot-forms.ss (so the jfile method arm wraps the fully-built dispatch) and
;; natives-meta.ss / records.ss / printing.ss (jolt-type / instance-check /
;; jolt-str-render-one, which it extends).

(define-record-type jfile (fields path) (nongenerative jolt-jfile-v1))
(define (jolt-file? x) (jfile? x))

;; path string of any value: a jfile -> its path, else its str rendering.
(define (file-path-of x) (if (jfile? x) (jfile-path x) (jolt-str-render-one x)))

;; (io/file path) / (io/file parent child) — join children with "/".
(define (jolt-make-file path . rest)
  (let loop ((p (file-path-of path)) (cs rest))
    (if (null? cs) (make-jfile p)
        (loop (string-append p "/" (file-path-of (car cs))) (cdr cs)))))

(define (path-last-segment p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) p)
          ((char=? (string-ref p i) #\/) (substring p (+ i 1) (string-length p)))
          (else (loop (- i 1))))))

;; directory children as full paths, sorted (the __list-dir seed primitive).
(define (jolt-list-dir path)
  (let ((p (file-path-of path)))
    (map (lambda (e) (string-append p "/" e))
         (sort string<? (directory-list p)))))
(define (jolt-dir? path) (if (file-directory? (file-path-of path)) #t #f))

;; absolute path string (cwd-relative paths resolved against current-directory).
(define (jfile-abs p)
  (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)) p
      (string-append (current-directory) "/" p)))

;; --- java.net.URL (a jhost "url", state #(spec)) ----------------------------
;; A File.toURL value: .toString / .toExternalForm give the spec, .getPath /
;; .getFile strip the "file:" scheme.
(define (make-url spec) (make-jhost "url" (vector spec)))
(define (url-spec u) (vector-ref (jhost-state u) 0))
(define (url-strip-scheme spec)
  (if (and (>= (string-length spec) 5) (string=? (substring spec 0 5) "file:"))
      (substring spec 5 (string-length spec)) spec))
(register-host-methods! "url"
  (list (cons "toString"       (lambda (self) (url-spec self)))
        (cons "toExternalForm" (lambda (self) (url-spec self)))
        (cons "getPath"        (lambda (self) (url-strip-scheme (url-spec self))))
        (cons "getFile"        (lambda (self) (url-strip-scheme (url-spec self))))))

;; --- File method surface (record-method-dispatch arm) -----------------------
(define (jfile-method f name args)        ; -> boxed result, or #f to fall through
  (let ((p (jfile-path f)))
    (cond
      ((string=? name "getPath")        (list p))
      ((string=? name "getName")        (list (path-last-segment p)))
      ((string=? name "toString")       (list p))
      ((string=? name "getAbsolutePath")(list (jfile-abs p)))
      ((string=? name "getCanonicalPath")(list (jfile-abs p)))
      ((string=? name "toURI")          (list (string-append "file:" (jfile-abs p))))
      ((string=? name "toURL")          (list (make-url (string-append "file:" (jfile-abs p)))))
      ((string=? name "exists")         (list (if (file-exists? p) #t #f)))
      ((string=? name "isDirectory")    (list (if (file-directory? p) #t #f)))
      ((string=? name "isFile")         (list (if (and (file-exists? p) (not (file-directory? p))) #t #f)))
      ((string=? name "listFiles")      (list (list->cseq (map make-jfile (jolt-list-dir p)))))
      ((string=? name "getParent")
       (let loop ((i (- (string-length p) 1)))
         (cond ((< i 0) (list jolt-nil))
               ((char=? (string-ref p i) #\/) (list (if (= i 0) "/" (substring p 0 i))))
               (else (loop (- i 1))))))
      (else #f))))

(define %io-rmd record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (if (jfile? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (jfile-method obj method-name rest)))
          (if r (car r) (error #f "no File method" method-name)))
        (%io-rmd obj method-name rest-args))))

;; .isDirectory / .listFiles emit to jolt-host-call (rt.ss), not record-method-
;; dispatch — the Phase-1 shims there assume a
;; path STRING target. Make them jfile-aware so file-seq's File branch works.
(define %io-host-call jolt-host-call)
(set! jolt-host-call
  (lambda (method target . args)
    (cond
      ((and (jfile? target) (string=? method "isDirectory"))
       (if (file-directory? (jfile-path target)) #t #f))
      ((and (jfile? target) (string=? method "listFiles"))
       (list->cseq (map make-jfile (jolt-list-dir target))))
      (else (apply %io-host-call method target args)))))

;; --- slurp / spit / flush ---------------------------------------------------
(define (read-file-string path)
  (let ((p (open-input-file path)))
    (let ((s (get-string-all p))) (close-port p) (if (eof-object? s) "" s))))

;; Drain a jhost reader (StringReader / PushbackReader): read code units from the
;; current position to EOF (-1) and assemble the string. Used by slurp; advances
;; the reader, as on the JVM.
(define (drain-reader r)
  (let loop ((acc '()))
    (let ((u (record-method-dispatch r "read" jolt-nil)))
      (if (or (jolt-nil? u) (and (number? u) (< u 0)))
          (list->string (reverse acc))
          (loop (cons (integer->char (exact (truncate u))) acc))))))

(define (reader-jhost? x)
  (and (jhost? x) (member (jhost-tag x) '("string-reader" "pushback-reader"))))

;; clojure.edn/read over a reader (jolt-uicd): the overlay edn.clj's drain-reader is
;; janet/type-coupled, so on Chez we drain the jhost reader to a string and read the
;; first EDN form (read-string). Re-asserted over the prelude in post-prelude.ss.
(define (chez-edn-read reader)
  (jolt-invoke (var-deref "clojure.core" "read-string")
               (if (reader-jhost? reader) (drain-reader reader) (jolt-str-render-one reader))))

;; line-seq (jolt-0obq): the overlay line-seq reads via a Janet map-reader's
;; :read-line-fn, but a Chez io/reader is a jhost StringReader. Drain it (or take a
;; string) and split on newline; a trailing newline does NOT yield a final empty
;; line (like readLine -> nil at EOF). Re-asserted in post-prelude.ss.
(define (chez-lines s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (chez-line-seq rdr)
  (list->cseq (chez-lines (cond ((string? rdr) rdr)
                                ((reader-jhost? rdr) (drain-reader rdr))
                                (else (jolt-str-render-one rdr))))))

(define (jolt-slurp src . opts)
  (cond
    ((jfile? src) (read-file-string (jfile-path src)))
    ((reader-jhost? src) (drain-reader src))
    ((string? src) (read-file-string src))
    (else (error #f "slurp: unsupported source" src))))

(define (spit-append? opts)
  (let loop ((o opts))
    (cond ((or (null? o) (null? (cdr o))) #f)
          ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "append")
                (jolt-truthy? (cadr o))) #t)
          (else (loop (cddr o))))))

(define (jolt-spit path content . opts)
  (let* ((p (file-path-of path))
         (port (open-output-file p (if (spit-append? opts) 'append 'truncate))))
    (put-string port (jolt-str-render-one content))
    (close-port port)
    jolt-nil))

(define (jolt-flush) (flush-output-port (current-output-port)) jolt-nil)

;; --- str / type / instance? integration ------------------------------------
;; str of a jfile is its path (Clojure's File.toString).
(define %io-str-render jolt-str-render-one)
(set! jolt-str-render-one
  (lambda (v) (if (jfile? v) (jfile-path v) (%io-str-render v))))

;; (type f) -> :jolt/file (the tagged-file :jolt/type). Re-def-var!
;; "type": natives-meta.ss already bound the var to the old jolt-type value, so the
;; set! alone (which updates the symbol for internal callers) wouldn't reach it.
(define io-kw-file (keyword "jolt" "file"))
(define %io-type jolt-type)
(set! jolt-type (lambda (x) (if (jfile? x) io-kw-file (%io-type x))))
(def-var! "clojure.core" "type" jolt-type)

;; (instance? java.io.File f): the instance? macro passes the class-name symbol;
;; match "File" / "java.io.File" (and any *.File) against a jfile.
(define %io-instance-check instance-check)
(set! instance-check
  (lambda (type-sym val)
    (let ((tname (symbol-t-name type-sym)))
      (if (and (jfile? val)
               (or (string=? tname "File") (string=? tname "java.io.File")
                   (string=? (path-last-segment tname) "File")))
          #t
          (%io-instance-check type-sym val)))))
(def-var! "clojure.core" "instance-check" instance-check)

;; --- def-var! the native names the overlay file-seq + str/slurp use ----
(def-var! "clojure.core" "__make-file" jolt-make-file)
(def-var! "clojure.core" "__file?" jolt-file?)
(def-var! "clojure.core" "__dir?" jolt-dir?)
(def-var! "clojure.core" "__list-dir" (lambda (p) (list->cseq (jolt-list-dir p))))
(def-var! "clojure.core" "slurp" jolt-slurp)
(def-var! "clojure.core" "spit" jolt-spit)
(def-var! "clojure.core" "flush" jolt-flush)

;; --- char-array: a seq of chars over a string (the JVM char[]). io/reader's
;; char[] branch + selmer's (char-array template) feed on this.
;; char-array (string -> chars). A leaf array native; lives here as io/reader
;; is its only Chez consumer so far.
(define (jolt-char-array a . rest)
  (cond
    ((string? a) (list->cseq (string->list a)))
    ((number? a) (list->cseq (make-list (exact (truncate a)) #\nul)))
    (else (list->cseq (map (lambda (c) (if (char? c) c (integer->char (exact (truncate c)))))
                           (seq->list a))))))
(def-var! "clojure.core" "char-array" jolt-char-array)

;; --- with-open's close seam (__close): a map-like value closes via its :close
;; fn; a jhost reader/writer/file via its .close method (a no-op here); anything
;; else is an error.
(define (jolt-close x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((and (jhost? x) (member (jhost-tag x) '("string-reader" "pushback-reader" "writer")))
     (record-method-dispatch x "close" jolt-nil) jolt-nil)
    ((jfile? x) jolt-nil)
    (else
     (let ((closef (jolt-get x (keyword #f "close") jolt-nil)))
       (if (and (not (jolt-nil? closef)) (procedure? closef))
           (begin (jolt-invoke closef) jolt-nil)
           (error #f "with-open: don't know how to close" x))))))
(def-var! "clojure.core" "__close" jolt-close)

;; --- clojure.java.io/reader: an in-memory java.io.Reader over the source. An
;; existing reader passes through; a File / path / URL is slurped; a char[] (or
;; any seq) becomes a reader over (apply str …). Mirrors io.clj's reader. Returns
;; a StringReader (host-static.ss jhost) so .read/.mark/.reset and slurp work.
(define (seq-source->string x)
  (apply string-append (map jolt-str-render-one (seq->list x))))
(define (jolt-io-reader x)
  (cond
    ((reader-jhost? x) x)
    ((jfile? x) (host-new "StringReader" (read-file-string (jfile-path x))))
    ((and (jhost? x) (string=? (jhost-tag x) "url"))
     (host-new "StringReader" (read-file-string (url-strip-scheme (url-spec x)))))
    ((string? x) (host-new "StringReader" (read-file-string x)))
    ((or (cseq? x) (empty-list-t? x) (pvec? x))
     (host-new "StringReader" (seq-source->string x)))
    (else (host-new "StringReader" (jolt-str-render-one x)))))

;; --- clojure.java.io ns -----------------------------------------------------
(def-var! "clojure.java.io" "file" jolt-make-file)
(def-var! "clojure.java.io" "as-file" (lambda (x) (if (jfile? x) x (make-jfile (file-path-of x)))))
(def-var! "clojure.java.io" "reader" jolt-io-reader)
(def-var! "clojure.java.io" "input-stream" jolt-io-reader)
(def-var! "clojure.java.io" "as-url" (lambda (x) (if (and (jhost? x) (string=? (jhost-tag x) "url")) x (make-url (jolt-str-render-one x)))))
