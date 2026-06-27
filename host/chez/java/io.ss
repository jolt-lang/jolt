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

;; Resources baked into a standalone binary by `jolt build` (deps.edn
;; :jolt/build :embed). The build emits a register-embedded-resource! per file at
;; heap-build time, so the contents live in the boot image — io/resource serves
;; them with no file on disk. An embedded hit reads through slurp/reader exactly
;; like a jfile would.
(define embedded-resources (make-hashtable equal-hash equal?))
(define (register-embedded-resource! name content)
  (hashtable-set! embedded-resources name content))
(define-record-type embedded-res (fields name content) (nongenerative jolt-embres-v1))

;; A user-facing relative path resolves against JOLT_PWD — the user's cwd before
;; the launcher cd'd to the jolt repo root — matching the JVM, where io/file is
;; cwd-relative. (io/resource builds jfiles from the source roots directly, so it
;; isn't routed through here.)
(define (project-relative p)
  (if (or (= (string-length p) 0) (char=? (string-ref p 0) #\/))
      p
      (let ((pwd (getenv "JOLT_PWD")))
        (if (and pwd (> (string-length pwd) 0)) (string-append pwd "/" p) p))))

;; (io/file path) / (io/file parent child) — join children with "/". The File
;; keeps the path AS GIVEN (like the JVM: new File("rel").getPath() is "rel");
;; a relative path resolves against JOLT_PWD only when the filesystem is touched
;; (jfile-fs / slurp / spit / the stream constructors).
(define (jolt-make-file path . rest)
  (let loop ((p (file-path-of path)) (cs rest))
    (if (null? cs) (make-jfile p)
        (loop (string-append p "/" (file-path-of (car cs))) (cdr cs)))))
;; the on-disk path of a value: a relative path resolves against JOLT_PWD.
(define (jfile-fs f) (project-relative (file-path-of f)))

(define (path-last-segment p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) p)
          ((char=? (string-ref p i) #\/) (substring p (+ i 1) (string-length p)))
          (else (loop (- i 1))))))

;; directory children as full paths, sorted (the __list-dir seed primitive).
(define (jolt-list-dir path)
  (let ((p (project-relative (file-path-of path))))
    (map (lambda (e) (string-append p "/" e))
         (sort string<? (directory-list p)))))
(define (jolt-dir? path) (if (file-directory? (project-relative (file-path-of path))) #t #f))

;; absolute path string (cwd-relative paths resolved against current-directory).
(define (jfile-abs p)
  (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)) p
      (string-append (current-directory) "/" p)))

;; --- file metadata over Chez filesystem ops ---------------------------------
;; byte size of a regular file (0 for a directory or a missing file).
(define (file-byte-size p)
  (if (or (not (file-exists? p)) (file-directory? p)) 0
      (let ((port (open-file-input-port p))) (let ((n (file-length port))) (close-port port) n))))
;; last-modified as epoch milliseconds (0 if the file is absent).
(define (file-mtime-millis p)
  (if (file-exists? p)
      (let ((t (file-modification-time p)))
        (+ (* (time-second t) 1000) (div (time-nanosecond t) 1000000)))
      0))
;; mkdir -p: create p and any missing parents. Returns #t if p ends up a dir.
(define (mkdirs! p)
  (unless (or (= 0 (string-length p)) (file-exists? p))
    (let loop ((i (- (string-length p) 1)))
      (cond ((<= i 0) #f)
            ((char=? (string-ref p i) #\/)
             (let ((parent (substring p 0 i))) (unless (file-exists? parent) (mkdirs! parent))))
            (else (loop (- i 1)))))
    (guard (e (#t #f)) (mkdir p)))
  (and (file-exists? p) (file-directory? p)))
;; delete a file or an (empty) directory; #t on success.
(define (delete-path! p)
  (guard (e (#t #f))
    (cond ((not (file-exists? p)) #f)
          ((file-directory? p) (delete-directory p) #t)
          (else (delete-file p) #t))))

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
  (let ((p (jfile-path f))               ; the path as given (display methods)
        (fp (jfile-fs f)))               ; JOLT_PWD-resolved on-disk path (FS methods)
    (cond
      ((string=? name "getPath")        (list p))
      ((string=? name "getName")        (list (path-last-segment p)))
      ((string=? name "toString")       (list p))
      ((string=? name "getAbsolutePath")(list (jfile-abs fp)))
      ((string=? name "getCanonicalPath")(list (jfile-abs fp)))
      ((string=? name "toURI")          (list (string-append "file:" (jfile-abs fp))))
      ((string=? name "toURL")          (list (make-url (string-append "file:" (jfile-abs fp)))))
      ;; io/resource returns a File where the JVM returns a file: URL; answer the
      ;; two URL methods resource-serving middleware (ring) calls on the result, so
      ;; it sees a "file" protocol and a path without changing the return type.
      ((string=? name "getProtocol")    (list "file"))
      ((string=? name "getFile")        (list (jfile-abs fp)))
      ((string=? name "exists")         (list (if (file-exists? fp) #t #f)))
      ((string=? name "isDirectory")    (list (if (file-directory? fp) #t #f)))
      ((string=? name "isFile")         (list (if (and (file-exists? fp) (not (file-directory? fp))) #t #f)))
      ((string=? name "isAbsolute")     (list (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)) #t #f)))
      ((string=? name "listFiles")      (list (list->cseq (map make-jfile (jolt-list-dir fp)))))
      ;; .list -> the child NAMES (a String[]), nil if not a directory.
      ((string=? name "list")
       (list (if (file-directory? fp)
                 (apply jolt-vector (sort string<? (directory-list fp)))
                 jolt-nil)))
      ((string=? name "length")         (list (->num (file-byte-size fp))))
      ((string=? name "lastModified")   (list (->num (file-mtime-millis fp))))
      ((string=? name "canRead")        (list (if (file-exists? fp) #t #f)))
      ((string=? name "canWrite")       (list (if (file-exists? fp) #t #f)))
      ((string=? name "canExecute")     (list (if (file-exists? fp) #t #f)))
      ((string=? name "isHidden")       (list (let ((nm (path-last-segment p)))
                                                (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\.)) #t #f))))
      ((string=? name "mkdir")          (list (guard (e (#t #f)) (and (not (file-exists? fp)) (begin (mkdir fp) #t)))))
      ((string=? name "mkdirs")         (list (if (mkdirs! fp) #t #f)))
      ((string=? name "delete")         (list (if (delete-path! fp) #t #f)))
      ((string=? name "deleteOnExit")   (list jolt-nil))
      ((string=? name "setLastModified")(list #t))
      ((string=? name "createNewFile")
       (list (if (file-exists? fp) #f
                 (guard (e (#t #f)) (close-port (open-output-file fp 'truncate)) #t))))
      ((string=? name "renameTo")
       (list (let ((dst (jfile-fs (car args)))) (guard (e (#t #f)) (rename-file fp dst) #t))))
      ((string=? name "getParentFile")
       (let loop ((i (- (string-length p) 1)))
         (cond ((< i 0) (list jolt-nil))
               ((char=? (string-ref p i) #\/) (list (make-jfile (if (= i 0) "/" (substring p 0 i)))))
               (else (loop (- i 1))))))
      ((string=? name "getAbsoluteFile")  (list (make-jfile (jfile-abs p))))
      ((string=? name "getCanonicalFile") (list (make-jfile (jfile-abs p))))
      ((string=? name "compareTo")      (list (->num (let ((o (file-path-of (car args))))
                                                       (cond ((string<? p o) -1) ((string>? p o) 1) (else 0))))))
      ((string=? name "equals")         (list (and (jfile? (car args)) (string=? p (jfile-path (car args))))))
      ((string=? name "hashCode")       (list (->num (string-hash p))))
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
       (if (file-directory? (jfile-fs target)) #t #f))
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
    ((jfile? src) (read-file-string (jfile-fs src)))
    ((embedded-res? src) (embedded-res-content src))
    ((reader-jhost? src) (drain-reader src))
    ;; bytes (a bytevector or a jolt byte-array): decode with :encoding (UTF-8
    ;; default). clj-http-lite slurps response-body byte arrays.
    ((bytevector? src) (decode-bytevector src (slurp-encoding opts)))
    ((and (jolt-array? src) (eq? (jolt-array-kind src) 'byte))
     (decode-bytevector (na-bytearray->bv src) (slurp-encoding opts)))
    ;; a byte input-stream shim (e.g. clj-http-lite's :as :stream body): drain it.
    ((and (htable? src) (jolt-truthy? (jolt-ref-get src (keyword "jolt" "input-stream"))))
     (decode-bytevector (drain-byte-stream src) (slurp-encoding opts)))
    ((string? src) (read-file-string (project-relative src)))
    (else (error #f "slurp: unsupported source" src))))

(define (spit-append? opts)
  (let loop ((o opts))
    (cond ((or (null? o) (null? (cdr o))) #f)
          ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "append")
                (jolt-truthy? (cadr o))) #t)
          (else (loop (cddr o))))))

(define (jolt-spit path content . opts)
  (let* ((p (project-relative (file-path-of path)))
         (port (open-output-file p (if (spit-append? opts) 'append 'truncate))))
    (put-string port (jolt-str-render-one content))
    (close-port port)
    jolt-nil))

(define (jolt-flush) (flush-output-port (current-output-port)) jolt-nil)

;; --- str / type / instance? integration ------------------------------------
;; str of a jfile is its path (Clojure's File.toString).
(register-str-render! jfile? jfile-path)

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

;; (instance? java.io.File f): the instance? macro passes the class-name symbol;
;; match "File" / "java.io.File" (and any *.File) against a jfile.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tname (symbol-t-name type-sym)))
      (if (and (jfile? val)
               (or (string=? tname "File") (string=? tname "java.io.File")
                   (string=? (path-last-segment tname) "File")))
          #t
          'pass))))

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
    ((and (jhost? x) (member (jhost-tag x) '("string-reader" "pushback-reader" "writer"
                                             "file-writer" "port-writer" "print-writer")))
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
;; io/reader returns an in-memory StringReader (the full Reader contract incl.
;; (read), mark/reset and pushback). The streaming java.io.FileReader /
;; BufferedReader classes (io-streams.ss) read a Chez port directly when a caller
;; wants to avoid loading the whole source.
(define (jolt-io-reader x)
  (cond
    ((reader-jhost? x) x)
    ((jfile? x) (host-new "StringReader" (read-file-string (jfile-fs x))))
    ((embedded-res? x) (host-new "StringReader" (embedded-res-content x)))
    ((and (jhost? x) (string=? (jhost-tag x) "url"))
     (host-new "StringReader" (read-file-string (url-strip-scheme (url-spec x)))))
    ((string? x) (host-new "StringReader" (read-file-string (project-relative x))))
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
  (let* ((nm (jolt-str-render-one name))
         (emb (hashtable-ref embedded-resources nm #f)))
    (if emb (make-embedded-res nm emb)
        (let loop ((roots (get-source-roots)))
          (cond ((null? roots) jolt-nil)
                ((file-exists? (string-append (car roots) "/" nm)) (make-jfile (string-append (car roots) "/" nm)))
                (else (loop (cdr roots))))))))
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
;; getResources: every source root that holds the named resource, as file: URLs
;; (enumeration-seq just calls seq, so a list serves). ring's static-resource
;; symlink check enumerates these to confirm a served file sits under a root.
(define (cl-get-resources self name)
  (let ((nm (jolt-str-render-one name)))
    (let loop ((roots (get-source-roots)) (acc '()))
      (cond ((null? roots) (list->cseq (reverse acc)))
            ((file-exists? (string-append (car roots) "/" nm))
             (loop (cdr roots) (cons (make-url (string-append "file:" (car roots) "/" nm)) acc)))
            (else (loop (cdr roots) acc))))))
(register-host-methods! "classloader"
  (list (cons "getResource" cl-get-resource)
        (cons "getResources" cl-get-resources)
        (cons "getResourceAsStream"
              (lambda (self name)
                (let ((u (cl-get-resource self name)))
                  (if (jolt-nil? u) jolt-nil (host-new "StringReader" (jolt-slurp (url-strip-scheme (url-spec u))))))))))
(register-class-statics! "ClassLoader" (list (cons "getSystemClassLoader" (lambda () the-classloader))))
(register-class-statics! "java.lang.ClassLoader" (list (cons "getSystemClassLoader" (lambda () the-classloader))))
;; clojure.lang.RT/baseLoader — the resource-resolving class loader (RT/baseLoader
;; is how libraries reach Clojure's base loader, e.g. aws-api's resources ns).
(register-class-statics! "RT" (list (cons "baseLoader" (lambda () the-classloader))))
(register-class-statics! "clojure.lang.RT" (list (cons "baseLoader" (lambda () the-classloader))))
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
;; File statics: the platform separators plus createTempFile / listRoots.
(define temp-file-counter 0)
(define (file-create-temp prefix suffix . dir)
  (let* ((d (cond ((pair? dir) (file-path-of (car dir)))
                  ((getenv "TMPDIR") => (lambda (t) t))
                  (else "/tmp")))
         (sfx (if (or (null? (list suffix)) (jolt-nil? suffix)) ".tmp" (jolt-str-render-one suffix))))
    (set! temp-file-counter (+ temp-file-counter 1))
    (let loop ((n temp-file-counter))
      (let ((p (string-append d "/" (jolt-str-render-one prefix)
                              (number->string (now-millis)) "-" (number->string n) sfx)))
        (if (file-exists? p) (loop (+ n 1))
            (begin (close-port (open-output-file p 'truncate)) (make-jfile p)))))))
(let ((statics (list (cons "separator" "/")
                     (cons "separatorChar" #\/)
                     (cons "pathSeparator" ":")
                     (cons "pathSeparatorChar" #\:)
                     (cons "createTempFile" file-create-temp)
                     (cons "listRoots" (lambda () (jolt-vector (make-jfile "/")))))))
  (register-class-statics! "File" statics)
  (register-class-statics! "java.io.File" statics))
(register-class-ctor! "java.io.File"
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
;; URI/create — the static factory, same as the (URI. s) constructor.
(register-class-statics! "URI" (list (cons "create" (lambda (s) (uri-parse (jolt-str-render-one s))))))
(register-class-statics! "java.net.URI" (list (cons "create" (lambda (s) (uri-parse (jolt-str-render-one s))))))
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
;; (= u1 u2) is value equality by string form (the .equals method above only
;; serves explicit (.equals …)); hash matches so a URI works as a map key / set
;; member (ring/hiccup compare (URI. "/") values).
(define (uri-jhost? x) (and (jhost? x) (string=? (jhost-tag x) "uri")))
(register-eq-arm! (lambda (a b) (or (uri-jhost? a) (uri-jhost? b)))
                  (lambda (a b) (and (uri-jhost? a) (uri-jhost? b)
                                     (string=? (uri-field a 'string) (uri-field b 'string)))))
(register-hash-arm! uri-jhost? (lambda (x) (string-hash (uri-field x 'string))))
;; str / pr-str of a uri -> its string form.
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "uri")))
                      (lambda (x) (uri-field x 'string)))
(register-pr-readable-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "uri")))
                           (lambda (x) (string-append "#object[java.net.URI \"" (uri-field x 'string) "\"]")))
;; class of the host value types defined by now (uri/uuid/file).
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "uri"))) (lambda (x) "java.net.URI"))
(register-class-arm! juuid? (lambda (x) "java.util.UUID"))
(register-class-arm! jfile? (lambda (x) "java.io.File"))
