;; java.io.File + host file I/O, implemented over Chez's filesystem
;; primitives. A File is a
;; path-backed jfile record: (instance? java.io.File f) is true, str/slurp coerce
;; it to its path, and the File method surface (getName/getPath/exists/
;; isDirectory/isFile/listFiles) dispatches through record-method-dispatch.
;;
;; Provides make-file/file?/slurp/spit/flush/dir?/
;; list-dir for the overlay file-seq (20-coll.clj), which calls __file?/__dir?/
;; __list-dir + the .isDirectory/.listFiles/.isFile method surface.
;;
;; Loaded LAST in rt.ss, after
;; dot-forms.ss (so the jfile method arm wraps the fully-built dispatch) and
;; natives-meta.ss / records.ss / printing.ss (jolt-type / instance-check /
;; jolt-str-render-one, which it extends).

(define-record-type jfile (fields path) (nongenerative jolt-jfile-v1))
(define (jolt-file? x) (jfile? x))

;; path string of any value: a jfile -> its path, else its str rendering.
(define (file-path-of x) (if (jfile? x) (jfile-path x) (jolt-str-render-one x)))

;; A user-facing relative path resolves against JOLT_PWD — the user's cwd before
;; the launcher cd'd to the jolt repo root — matching the JVM, where io/file is
;; cwd-relative. (io/resource builds jfiles from the source roots directly, so it
;; isn't routed through here.)
(define (project-relative p)
  (if (or (= (string-length p) 0) (char=? (string-ref p 0) #\/))
      p
      (let ((pwd (getenv "JOLT_PWD")))
        (if (and pwd (> (string-length pwd) 0)) (string-append pwd "/" p) p))))

;; (io/file path) / (io/file parent child) — join children with "/".
(define (jolt-make-file path . rest)
  (let loop ((p (project-relative (file-path-of path))) (cs rest))
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
(define (url-protocol spec)
  (let ((i (let loop ((j 0)) (cond ((>= j (string-length spec)) #f)
                                   ((char=? (string-ref spec j) #\:) j) (else (loop (+ j 1)))))))
    (if i (substring spec 0 i) "")))
;; (java.net.URL. spec) — a basic file/http URL value (a library may register a
;; richer URL shim, which overrides this).
(register-class-ctor! "URL" (lambda (spec . _) (make-url (jolt-str-render-one spec))))
(register-class-ctor! "java.net.URL" (lambda (spec . _) (make-url (jolt-str-render-one spec))))
(register-host-methods! "url"
  (list (cons "toString"       (lambda (self) (url-spec self)))
        (cons "toExternalForm" (lambda (self) (url-spec self)))
        (cons "getProtocol"    (lambda (self) (url-protocol (url-spec self))))
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
;; dispatch — the shims there assume a path STRING target. Make them jfile-aware
;; so file-seq's File branch works.
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

;; Refill a host reader so subsequent read/slurp see `s` (the unconsumed tail).
(define (reader-refill! r s)
  (cond
    ((string=? (jhost-tag r) "string-reader")
     (vector-set! (jhost-state r) 0 s) (vector-set! (jhost-state r) 1 0))
    ((string=? (jhost-tag r) "pushback-reader")
     (vector-set! (jhost-state r) 0 (host-new "StringReader" s))
     (vector-set! (jhost-state r) 1 '()))))
;; Read ONE form from a host reader (StringReader/PushbackReader): drain the
;; remaining chars, parse one form, push the tail back. -> (values form found?).
;; (read r) over a java.io reader — cuerdas' interpolation reads this way.
(define (host-reader-read-form r)
  (let* ((s (drain-reader r)) (pr (jolt-parse-next s)))
    (if (jolt-nil? pr)
        (begin (reader-refill! r "") (values jolt-nil #f))
        (begin (reader-refill! r (jolt-nth pr 1)) (values (jolt-nth pr 0) #t)))))

;; clojure.edn/read over a reader: drain the jhost reader to a string and read the
;; first EDN form (read-string). Re-asserted over the prelude in post-prelude.ss.
(define (chez-edn-read reader)
  (jolt-invoke (var-deref "clojure.core" "read-string")
               (if (reader-jhost? reader) (drain-reader reader) (jolt-str-render-one reader))))

;; line-seq: an io/reader is a jhost StringReader. Drain it (or take a string)
;; and split on newline; a trailing newline does NOT yield a final empty line
;; (like readLine -> nil at EOF). Re-asserted in post-prelude.ss.
(define (chez-lines s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (chez-line-seq rdr)
  (list->cseq (chez-lines (cond ((string? rdr) rdr)
                                ((reader-jhost? rdr) (drain-reader rdr))
                                (else (jolt-str-render-one rdr))))))

;; (slurp src :encoding "...") — pull the charset from the trailing kwargs.
(define (slurp-encoding opts)
  (let loop ((o opts))
    (cond ((or (null? o) (null? (cdr o))) '())
          ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "encoding"))
           (list (jolt-str-render-one (cadr o))))
          (else (loop (cddr o))))))
;; drain a byte input-stream shim (tagged-table) one byte at a time to a bytevector.
(define (drain-byte-stream src)
  (let loop ((acc '()))
    (let ((b (record-method-dispatch src "read" jolt-nil)))
      (if (or (jolt-nil? b) (and (number? b) (< b 0)))
          (u8-list->bytevector (reverse acc))
          (loop (cons (bitwise-and (jnum->exact b) #xff) acc))))))
(define (jolt-slurp src . opts)
  (cond
    ((jfile? src) (read-file-string (jfile-path src)))
    ((reader-jhost? src) (drain-reader src))
    ;; bytes (a bytevector or a jolt byte-array): decode with :encoding (UTF-8
    ;; default). clj-http-lite slurps response-body byte arrays.
    ((bytevector? src) (decode-bytevector src (slurp-encoding opts)))
    ((and (jolt-array? src) (eq? (jolt-array-kind src) 'byte))
     (decode-bytevector (na-bytearray->bv src) (slurp-encoding opts)))
    ;; a byte input-stream shim (e.g. clj-http-lite's :as :stream body): drain it.
    ((and (htable? src) (jolt-truthy? (jolt-ref-get src (keyword "jolt" "input-stream"))))
     (decode-bytevector (drain-byte-stream src) (slurp-encoding opts)))
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

;; stdin line seam: the clojure.core *in* reader (50-io.clj) drives read-line /
;; read / read+string through __stdin-read-line. Return the next line (newline
;; stripped) or nil at EOF. Without this, (read-line) and the REPL call nil.
(def-var! "clojure.core" "__stdin-read-line"
  (lambda () (let ((l (get-line (current-input-port)))) (if (eof-object? l) jolt-nil l))))

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

;; --- with-open's close seam (__close): a map-like value closes via its :close
;; fn; a jhost reader/writer/file via its .close method (a no-op here); anything
;; else is an error.
(define (jolt-close x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((and (jhost? x) (member (jhost-tag x) '("string-reader" "pushback-reader" "writer")))
     (record-method-dispatch x "close" jolt-nil) jolt-nil)
    ;; a library's stream shim (tagged-table) closes via its registered .close
    ;; method (a no-op for in-memory streams); absent method -> no-op.
    ((htable? x) (guard (e (#t jolt-nil)) (record-method-dispatch x "close" jolt-nil)) jolt-nil)
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

;; --- clojure.java.io/writer: an existing writer passes through; a File / path
;; gets a file-backed writer (host-static.ss "file-writer") that persists on
;; flush/close. Mirrors io.clj's writer over the host's StringWriter/file ports.
(define (jolt-io-writer x)
  (cond
    ((and (jhost? x) (string=? (jhost-tag x) "writer")) x)
    ((and (jhost? x) (string=? (jhost-tag x) "file-writer")) x)
    ((jfile? x) (make-jhost "file-writer" (vector (jfile-path x) "")))
    ((string? x) (make-jhost "file-writer" (vector x "")))
    (else (error #f "io/writer: don't know how to create a writer from" x))))

;; --- clojure.java.io ns -----------------------------------------------------
(def-var! "clojure.java.io" "file" jolt-make-file)
(def-var! "clojure.java.io" "as-file" (lambda (x) (if (jfile? x) x (make-jfile (file-path-of x)))))
;; "reader" is bound by natives-array.ss (loaded later) so a char[] argument is
;; handled; that binding delegates here via jolt-io-reader for everything else.
(def-var! "clojure.java.io" "writer" jolt-io-writer)
(def-var! "clojure.java.io" "input-stream" jolt-io-reader)
(def-var! "clojure.java.io" "output-stream" jolt-io-writer)
;; resource: jolt has no classpath, so a named resource is resolved against the
;; loader's source roots (a project's :paths, e.g. "resources"). Returns a File
;; (slurp/reader-able) for the first match, else nil. get-source-roots is the
;; loader's accessor (loader.ss), resolved at call time — the runtime CLI loads it.
(define (jolt-io-resource name)
  (let ((nm (jolt-str-render-one name)))
    (let loop ((roots (get-source-roots)))
      (cond ((null? roots) jolt-nil)
            ((file-exists? (string-append (car roots) "/" nm)) (make-jfile (string-append (car roots) "/" nm)))
            (else (loop (cdr roots)))))))
(def-var! "clojure.java.io" "resource" jolt-io-resource)
;; as-url honors a library-registered URL class (e.g. jolt-lang/http-client's full
;; java.net.URL shim) so io/as-url and (URL. spec) agree; else the file-only jhost.
(def-var! "clojure.java.io" "as-url"
  (lambda (x)
    (cond ((and (jhost? x) (string=? (jhost-tag x) "url")) x)
          ((htable? x) x)
          (else (let ((ctor (lookup-class class-ctors-tbl "URL")))
                  (if ctor (ctor (jolt-str-render-one x)) (make-url (jolt-str-render-one x))))))))

;; --- java.lang.ClassLoader --------------------------------------------------
;; jolt has no classpath; a "classloader" resolves a named resource against the
;; loader's source roots (the same model as clojure.java.io/resource), returning a
;; file: URL or nil. getSystemClassLoader / a thread's contextClassLoader both hand
;; back this loader. Libraries that probe the classpath (e.g. migratus's migration-
;; dir discovery) then fall back to the filesystem when a resource isn't a root.
(define the-classloader (make-jhost "classloader" (vector)))
(define (cl-get-resource self name)
  (let ((nm (jolt-str-render-one name)))
    (let loop ((roots (get-source-roots)))
      (cond ((null? roots) jolt-nil)
            ((file-exists? (string-append (car roots) "/" nm))
             (make-url (string-append "file:" (car roots) "/" nm)))
            (else (loop (cdr roots)))))))
(register-host-methods! "classloader"
  (list (cons "getResource" cl-get-resource)
        (cons "getResourceAsStream"
              (lambda (self name)
                (let ((u (cl-get-resource self name)))
                  (if (jolt-nil? u) jolt-nil (host-new "StringReader" (jolt-slurp (url-strip-scheme (url-spec u))))))))))
(register-class-statics! "ClassLoader" (list (cons "getSystemClassLoader" (lambda () the-classloader))))
(register-class-statics! "java.lang.ClassLoader" (list (cons "getSystemClassLoader" (lambda () the-classloader))))
;; Thread/currentThread -> a fresh thread jhost wrapping THIS thread's interrupt
;; flag (the box from current-interrupt-box, host-static.ss), so .interrupt from
;; any thread sets the target thread's flag and .isInterrupted reads it without
;; clearing (instance semantics; the static Thread/interrupted reads-and-clears).
;; getContextClassLoader hands back the loader.
(register-host-methods! "thread"
  (list (cons "getContextClassLoader" (lambda (self) the-classloader))
        (cons "getName" (lambda (self) "main"))
        (cons "interrupt" (lambda (self)
                            (when (box? (jhost-state self)) (set-box! (jhost-state self) #t))
                            jolt-nil))
        (cons "isInterrupted" (lambda (self)
                                (and (box? (jhost-state self)) (unbox (jhost-state self)) #t)))))
(define (current-thread-handle) (make-jhost "thread" (current-interrupt-box)))
(register-class-statics! "Thread" (list (cons "currentThread" current-thread-handle)))
(register-class-statics! "java.lang.Thread" (list (cons "currentThread" current-thread-handle)))

;; --- java.io.File / java.util.UUID constructors -----------------------------
;; (java.io.File. parent child) joins with "/"; (File. path) wraps the path.
(register-class-ctor! "File"
  (lambda (a . rest)
    (if (pair? rest)
        (jolt-make-file (string-append (file-path-of a) "/" (file-path-of (car rest))))
        (jolt-make-file a))))
;; UUID: randomUUID / fromString statics + a (UUID. s) string ctor.
(register-class-statics! "UUID"
  (list (cons "randomUUID" (lambda () (jolt-random-uuid)))
        (cons "fromString" (lambda (s) (jolt-parse-uuid (jolt-str-render-one s))))))
(register-class-statics! "java.util.UUID"
  (list (cons "randomUUID" (lambda () (jolt-random-uuid)))
        (cons "fromString" (lambda (s) (jolt-parse-uuid (jolt-str-render-one s))))))
(register-class-ctor! "UUID" (lambda (s) (jolt-parse-uuid (jolt-str-render-one s))))

;; --- java.net.URI -----------------------------------------------------------
;; A minimal RFC-3986 split into scheme/authority/host/port/path/query/fragment,
;; kept in a jhost "uri" carrying the original string. (str u)/(.toString u) give
;; the original; getHost is nil for a relative URI (hiccup.util/to-str branches on
;; it). instance? java.net.URI + extend-protocol dispatch work via value-host-tags.
(define (uri-index-of s ch from)
  (let ((n (string-length s)))
    (let loop ((i from)) (cond ((>= i n) #f) ((char=? (string-ref s i) ch) i) (else (loop (+ i 1)))))))
(define (uri-scheme-end s)
  ;; index of ':' that ends a scheme (letter then alnum/+-. before any /?#), or #f.
  (let ((n (string-length s)))
    (and (> n 0) (char-alphabetic? (string-ref s 0))
         (let loop ((i 1))
           (cond ((>= i n) #f)
                 ((char=? (string-ref s i) #\:) i)
                 ((let ((c (string-ref s i)))
                    (or (char-alphabetic? c) (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))
                  (loop (+ i 1)))
                 (else #f))))))
(define (uri-parse s)
  (let* ((n (string-length s))
         (se (uri-scheme-end s))
         (scheme (and se (substring s 0 se)))
         (rest-start (if se (+ se 1) 0))
         ;; fragment
         (hash (uri-index-of s #\# rest-start))
         (frag (and hash (substring s (+ hash 1) n)))
         (pre-frag-end (or hash n))
         ;; query
         (qm (uri-index-of s #\? rest-start))
         (query (and qm (< qm pre-frag-end) (substring s (+ qm 1) pre-frag-end)))
         (hp-end (cond ((and qm (< qm pre-frag-end)) qm) (else pre-frag-end)))
         ;; authority (after "//")
         (has-auth (and (<= (+ rest-start 2) n)
                        (char=? (string-ref s rest-start) #\/)
                        (char=? (string-ref s (+ rest-start 1)) #\/)))
         (auth-start (and has-auth (+ rest-start 2)))
         (auth-end (and has-auth
                        (let loop ((i auth-start))
                          (cond ((>= i hp-end) hp-end)
                                ((char=? (string-ref s i) #\/) i)
                                (else (loop (+ i 1)))))))
         (authority (and has-auth (substring s auth-start auth-end)))
         (path-start (if has-auth auth-end rest-start))
         (path (substring s path-start hp-end)))
    ;; host:port from authority (strip userinfo@)
    (let* ((at (and authority (uri-index-of authority #\@ 0)))
           (hostport (if at (substring authority (+ at 1) (string-length authority)) authority))
           (colon (and hostport (uri-index-of hostport #\: 0)))
           (host (cond ((not hostport) jolt-nil)
                       (colon (substring hostport 0 colon))
                       (else hostport)))
           (port (if (and colon (< (+ colon 1) (string-length hostport)))
                     (or (string->number (substring hostport (+ colon 1) (string-length hostport))) -1)
                     -1)))
      (make-jhost "uri"
        (list (cons 'string s)
              (cons 'scheme (or scheme jolt-nil))
              (cons 'authority (or authority jolt-nil))
              (cons 'host (if (and host (string? host) (= 0 (string-length host))) jolt-nil host))
              (cons 'port (->num port))
              (cons 'path (if (= 0 (string-length path)) (if has-auth "" jolt-nil) path))
              (cons 'query (or query jolt-nil))
              (cons 'fragment (or frag jolt-nil)))))))
(define (uri-field u k) (let ((p (assq k (jhost-state u)))) (if p (cdr p) jolt-nil)))
(register-class-ctor! "URI" (lambda (s) (uri-parse (jolt-str-render-one s))))
(register-class-ctor! "java.net.URI" (lambda (s) (uri-parse (jolt-str-render-one s))))
(register-host-methods! "uri"
  (list (cons "toString" (lambda (u) (uri-field u 'string)))
        (cons "toASCIIString" (lambda (u) (uri-field u 'string)))
        (cons "getScheme" (lambda (u) (uri-field u 'scheme)))
        (cons "getAuthority" (lambda (u) (uri-field u 'authority)))
        (cons "getHost" (lambda (u) (uri-field u 'host)))
        (cons "getPort" (lambda (u) (uri-field u 'port)))
        (cons "getPath" (lambda (u) (uri-field u 'path)))
        (cons "getRawPath" (lambda (u) (uri-field u 'path)))
        (cons "getQuery" (lambda (u) (uri-field u 'query)))
        (cons "getRawQuery" (lambda (u) (uri-field u 'query)))
        (cons "getFragment" (lambda (u) (uri-field u 'fragment)))
        (cons "isAbsolute" (lambda (u) (not (jolt-nil? (uri-field u 'scheme)))))
        (cons "hashCode" (lambda (u) (string-hash (uri-field u 'string))))
        (cons "equals" (lambda (u o) (and (jhost? o) (string=? (jhost-tag o) "uri")
                                          (string=? (uri-field u 'string) (uri-field o 'string)))))))
;; str / pr-str of a uri -> its string form.
(define %uri-str-render-one jolt-str-render-one)
(set! jolt-str-render-one
  (lambda (x) (if (and (jhost? x) (string=? (jhost-tag x) "uri")) (uri-field x 'string) (%uri-str-render-one x))))
(define %uri-pr-readable jolt-pr-readable)
(set! jolt-pr-readable
  (lambda (x) (if (and (jhost? x) (string=? (jhost-tag x) "uri"))
                  (string-append "#object[java.net.URI \"" (uri-field x 'string) "\"]")
                  (%uri-pr-readable x))))
;; class of the host value types defined by now (uri/uuid/file).
(define %uri-class jolt-class)
(set! jolt-class
  (lambda (x)
    (cond ((and (jhost? x) (string=? (jhost-tag x) "uri")) "java.net.URI")
          ((juuid? x) "java.util.UUID")
          ((jfile? x) "java.io.File")
          (else (%uri-class x)))))
(def-var! "clojure.core" "class" jolt-class)
