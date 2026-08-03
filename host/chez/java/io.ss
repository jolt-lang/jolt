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
;; io/resource returns a java.net.URL from BOTH branches: a file: URL (a jhost
;; "url") for a hit on a source root, and this embedded-res (class java.net.URL,
;; protocol "jar") for a resource baked into a built binary. The two must answer the
;; SAME surface — getPath / getFile / getName / getProtocol / openStream — so a
;; caller that resolves a resource (orchard.namespace/canonical-source does
;; (some-> (io/resource p) .getPath)) gets the same answer whichever branch served
;; it. Registered below (after record-method-dispatch's arm registry exists)
;; rather than inline, so both branches stay in one place.
(define (embedded-res-method obj name args)
  (let ((nm (embedded-res-name obj)))
    (cond
      ((string=? name "getPath")     (list nm))
      ((string=? name "getFile")     (list nm))
      ((string=? name "getName")     (list (path-last-segment nm)))
      ((string=? name "toString")    (list nm))
      ;; embedded content has no file on disk; the JVM would report a jar: URL for
      ;; a resource inside an artifact, so "jar" is the honest protocol here.
      ((string=? name "getProtocol") (list "jar"))
      ((string=? name "exists")      (list #t))
      ((string=? name "isDirectory") (list #f))
      ((string=? name "isFile")      (list #t))
      ((string=? name "openStream")
       (let ((c (embedded-res-content obj)))
         (list (host-new "StringReader" (if (bytevector? c) (utf8->string c) c)))))
      (else #f))))

;; --- self-contained build artifacts (jolt-eaj) ------------------------------
;; A toolchain-free `jolt build` (the distributed jolt) carries the Chez
;; petite/scheme boots and a prebuilt launcher stub baked into its own boot image.
;; They live in the same table as embedded-resources, but keyed under bytevector
;; values (register-embedded-bytes!) rather than strings; resolve-on-roots /
;; io/resource only ever ask for the string-keyed source entries, so the two
;; coexist. The build driver reads them at heap-build time from files that exist
;; only on the dev machine.
(define (register-embedded-bytes! name bv) (hashtable-set! embedded-resources name bv))
(define (jolt-embedded-bytes name)
  (let ((v (hashtable-ref embedded-resources name #f)))
    (and (bytevector? v) v)))

;; --- with-port: open a port, do work, close on success or throw ----------------
(define (with-port port proc)
  (guard (e (#t (guard (_ (#t #f)) (close-port port)) (raise e)))
    (let ((result (proc port)))
      (close-port port)
      result)))

;; Read a whole file as a bytevector ("" -> empty). Used to slurp boot/stub files.
(define (read-file-bytes path)
  (with-port (open-file-input-port path)
    (lambda (p) (let ((bv (get-bytevector-all p))) (if (eof-object? bv) (bytevector) bv)))))

;; Write an embedded bytevector resource out to a path. make-boot-file needs the
;; petite/scheme boots as files, so they are spilled to scratch before the call.
(define (jolt-spill-embedded! name path)
  (let ((bv (jolt-embedded-bytes name)))
    (unless bv (error 'jolt-spill-embedded! "no embedded bytes for" name))
    (with-port (open-file-output-port path (file-options no-fail) (buffer-mode block))
      (lambda (p) (put-bytevector p bv)))))

;; Frame an app boot onto a file that already holds the stub bytes. Layout:
;; [stub][boot][boot-length:le64]["JOLTBOOT"]. The stub (host/chez/stub/launcher.c)
;; reads the trailing 16 bytes — the 8-byte magic, then the preceding 8-byte LE
;; length — to locate and register the boot, so a boot that itself contains the
;; magic bytes can't be mistaken for the frame.
(define jolt-payload-magic (string->utf8 "JOLTBOOT"))
(define (jolt-append-payload! path boot-bv)
  (let* ((head (read-file-bytes path))           ; the stub bytes already written
         (lb (make-bytevector 8 0)))
    (bytevector-u64-set! lb 0 (bytevector-length boot-bv) (endianness little))
    (with-port (open-file-output-port path (file-options no-fail) (buffer-mode block))
      (lambda (p)
        (put-bytevector p head)
        (put-bytevector p boot-bv)
        (put-bytevector p lb)
        (put-bytevector p jolt-payload-magic)))))

;; chmod 0755 via libc, so the produced binary is executable. load-shared-object
;; with #f pulls the running process's own symbols (chmod is in libc, linked into
;; every Chez binary) — no external toolchain. Falls back to /bin/sh chmod if the
;; symbol can't be resolved.
(define jolt-chmod-755
  (let ((c (jolt-foreign-proc-safe "chmod" '(string int) 'int)))
    (lambda (path)
      (cond
        (c (c path #o755))
        ;; Windows has no chmod and needs none (execute is by extension)
        ((let ((m (symbol->string (machine-type))))
           (let loop ((i 0))
             (cond ((> (+ i 2) (string-length m)) #f)
                   ((string=? (substring m i (+ i 2)) "nt") #t)
                   (else (loop (+ i 1))))))
         0)
        (else (system (string-append "chmod 755 '" path "'")))))))

;; user.dir — the project dir every user-facing relative path resolves against.
;; JOLT_PWD carries it when the launcher moved away from it (bin/jolt exports the
;; user's cwd before cd'ing to the repo root); otherwise it is the process's own
;; working directory, like the JVM's user.dir. This is the same chain
;; System/getProperty "user.dir" answers with — kept in one place so a caller
;; cannot implement half of it.
;;
;; PWD only gets a say when it agrees with that directory, where it is the
;; symlink-preserving spelling of it. It is a SHELL convention, not the process's
;; cwd: a child started in a different directory (jolt.process's :dir, or any
;; parent that chdirs) inherits the parent's PWD, and trusting it resolved every
;; relative path against the parent's directory — `jolt -m app` run with :dir set
;; read the WRONG deps.edn, or none.
(define (jolt-user-dir)
  (let ((jp (getenv "JOLT_PWD")))
    (if (and jp (> (string-length jp) 0))
        jp
        (let ((cwd (current-directory))
              (wd (getenv "PWD")))
          (cond ((and wd (> (string-length wd) 0) (string=? wd cwd)) wd)
                ((> (string-length cwd) 0) cwd)
                (else "."))))))

;; A user-facing relative path resolves against user.dir — the user's cwd before
;; the launcher cd'd to the jolt repo root — matching the JVM, where io/file is
;; cwd-relative. (io/resource builds jfiles from the source roots directly, so it
;; isn't routed through here.)
(define (project-relative p)
  (if (or (= (string-length p) 0) (char=? (string-ref p 0) #\/))
      p
      (let ((base (jolt-user-dir)))
        ;; "." adds nothing the OS won't do itself when it resolves a relative
        ;; path — leave it alone rather than prefixing "./".
        (if (string=? base ".") p (string-append base "/" p)))))

;; (io/file path) / (io/file parent child) — join children with "/". The File
;; keeps the path AS GIVEN (like the JVM: new File("rel").getPath() is "rel");
;; a relative path resolves against JOLT_PWD only when the filesystem is touched
;; (jfile-fs / slurp / spit / the stream constructors).
(define (jolt-make-file path . rest)
  (let loop ((p (file-path-of path)) (cs rest))
    (if (null? cs)
        ;; (io/file url) strips the scheme — File of url.toURI on the JVM; only a
        ;; file: url names a path. url-file-coercion is defined below; call-time ref.
        (if (and (null? rest) (jhost? path) (string=? (jhost-tag path) "url")) (url-file-coercion path) (make-jfile p))
        (loop (string-append p "/" (file-path-of (car cs))) (cdr cs)))))
;; the on-disk path of a value: a relative path resolves against JOLT_PWD.
(define (jfile-fs f) (project-relative (file-path-of f)))

(define (path-last-segment p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) p)
          ((char=? (string-ref p i) #\/) (substring p (+ i 1) (string-length p)))
          (else (loop (- i 1))))))

;; directory children, sorted (the __list-dir seed primitive). The children keep
;; the FORM OF THE PARENT, like File.listFiles(), which builds each child as
;; new File(this, name): listing a relative directory yields relative children.
;; Resolving the base to an absolute path first made every child absolute, so a
;; caller that relativized the results against the directory it passed in (a
;; classpath scanner turning files into namespace names) got ../../-prefixed
;; garbage. A trailing slash is dropped the way the File constructor normalizes
;; it away.
(define (jolt-list-dir path)
  (let* ((given (file-path-of path))
         (p (project-relative given))
         (trimmed (let loop ((n (string-length given)))
                    (if (and (> n 1) (char=? (string-ref given (- n 1)) #\/))
                        (loop (- n 1))
                        (substring given 0 n))))
         (base (if (string=? trimmed "") p trimmed)))
    (map (lambda (e) (string-append (if (string=? base "/") "" base) "/" e))
         (sort string<? (directory-list p)))))
(define (jolt-dir? path) (if (file-directory? (project-relative (file-path-of path))) #t #f))

;; absolute path string: a relative path resolves against user.dir — the same
;; base every filesystem touch uses (project-relative). Resolving against
;; (current-directory) here instead reported paths under the jolt repo root the
;; launcher cd'd into, diverging from the JVM where io/file and getAbsolutePath
;; are user.dir-relative.
(define (jfile-abs p)
  (cond ((= (string-length p) 0) (jolt-user-dir))
        ((char=? (string-ref p 0) #\/) p)
        (else (project-relative p))))

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
;; set atime+mtime from epoch milliseconds via utimes(2). struct timeval is
;; sec + usec, 16 bytes each on the 64-bit platforms Chez targets; usec fits
;; its field (< 1e6) so a signed 64-bit native-endian write covers the layout.
;; Resolved via jolt-foreign-proc-safe — a literal foreign-procedure here is a
;; fasl relocation that aborts the boot on platforms lacking the symbol.
;; Windows has no utimes; its CRT _utime64 takes {actime, modtime} as two
;; signed 64-bit seconds (16 bytes, second resolution).
(define c-utimes (jolt-foreign-proc-safe "utimes" '(string u8*) 'int))
(define c-utime64 (and (not c-utimes)
                       (jolt-foreign-proc-safe "_utime64" '(string u8*) 'int)))
(define (set-file-mtime-millis! p ms)
  (let ((sec (div ms 1000)))
    (cond
      (c-utimes
       (let ((tv (make-bytevector 32 0))
             (usec (* (mod ms 1000) 1000)))
         (bytevector-s64-set! tv 0 sec (native-endianness))
         (bytevector-s64-set! tv 8 usec (native-endianness))
         (bytevector-s64-set! tv 16 sec (native-endianness))
         (bytevector-s64-set! tv 24 usec (native-endianness))
         (= (c-utimes p tv) 0)))
      (c-utime64
       (let ((tb (make-bytevector 16 0)))
         (bytevector-s64-set! tb 0 sec (native-endianness))
         (bytevector-s64-set! tb 8 sec (native-endianness))
         (= (c-utime64 p tb) 0)))
      (else #f))))
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
          ((file-directory? p) (delete-directory p))
          (else (delete-file p) #t))))

;; --- java.net.URL (a jhost "url", state #(spec handler)) --------------------
;; A File.toURL value: .toString / .toExternalForm give the spec, .getPath /
;; .getFile strip the "file:" scheme.
;;
;; handler is a java.net.URLStreamHandler when one was supplied, else #f. A URL
;; built with one reads through it rather than off the filesystem: openConnection
;; is the handler's, and openStream is that connection's getInputStream. That is
;; how a caller serves templates from somewhere jolt has no protocol for —
;; Selmer's :url-stream-handler option is exactly this.
(define (make-url spec . h) (make-jhost "url" (vector spec (and (pair? h) (car h)))))
(define (url-spec u) (vector-ref (jhost-state u) 0))
(define (url-handler u)
  (let ((st (jhost-state u)))
    (and (> (vector-length st) 1)
         (let ((h (vector-ref st 1))) (and (not (jolt-nil? h)) h)))))
(define (url-jhost? x) (and (jhost? x) (string=? (jhost-tag x) "url")))
(define (url-strip-scheme spec)
  (if (and (>= (string-length spec) 5) (string=? (substring spec 0 5) "file:"))
      (substring spec 5 (string-length spec)) spec))
;; The path component: the spec without its scheme, and without an authority when
;; one is present. "https://example.com/a.html" -> "/a.html", "file:/a/b" -> "/a/b"
;; (a file: URL keeps giving the filesystem path callers read it for).
(define (url-path spec)
  (let* ((i (let loop ((j 0)) (cond ((>= j (string-length spec)) #f)
                                    ((char=? (string-ref spec j) #\:) j)
                                    (else (loop (+ j 1))))))
         (rest (if i (substring spec (+ i 1) (string-length spec)) spec)))
    (if (and (>= (string-length rest) 2) (string=? (substring rest 0 2) "//"))
        (let loop ((j 2))
          (cond ((>= j (string-length rest)) "")
                ((char=? (string-ref rest j) #\/) (substring rest j (string-length rest)))
                (else (loop (+ j 1)))))
        rest)))
(define (url-authority spec)
  (let* ((i (let loop ((j 0)) (cond ((>= j (string-length spec)) #f)
                                    ((char=? (string-ref spec j) #\:) j)
                                    (else (loop (+ j 1))))))
         (rest (if i (substring spec (+ i 1) (string-length spec)) spec)))
    (if (and (>= (string-length rest) 2) (string=? (substring rest 0 2) "//"))
        (let loop ((j 2))
          (cond ((>= j (string-length rest)) (substring rest 2 (string-length rest)))
                ((char=? (string-ref rest j) #\/) (substring rest 2 j))
                (else (loop (+ j 1)))))
        "")))
(define (url-protocol spec)
  (let ((i (let loop ((j 0)) (cond ((>= j (string-length spec)) #f)
                                   ((char=? (string-ref spec j) #\:) j) (else (loop (+ j 1)))))))
    (if i (substring spec 0 i) "")))
;; The JVM canonicalizes a spec on the way in: the protocol lowercases, and an
;; EMPTY authority collapses, so "file:///a/b/" and "http:///a" render "file:/a/b/"
;; and "http:/a" while "file://host/a" keeps its host. Callers compare these
;; strings (Selmer stores a resource path as one), so rendering the spec verbatim
;; diverges on the most common shape there is — a file: URL built from a path.
(define url-known-protocols '("http" "https" "file" "jar" "ftp" "mailto" "netdoc"))
(define (url-canonical spec)
  (let* ((i (let loop ((j 0)) (cond ((>= j (string-length spec)) #f)
                                    ((char=? (string-ref spec j) #\:) j)
                                    (else (loop (+ j 1))))))
         (proto (and i (string-downcase (substring spec 0 i))))
         (rest (and i (substring spec (+ i 1) (string-length spec)))))
    (unless (and proto (> (string-length proto) 0))
      (jolt-throw (jolt-host-throwable "java.net.MalformedURLException"
                                       (string-append "no protocol: " spec))))
    (unless (member proto url-known-protocols)
      (jolt-throw (jolt-host-throwable "java.net.MalformedURLException"
                                       (string-append "unknown protocol: " proto))))
    ;; An empty authority drops its "//": "file:///a/b/" renders "file:/a/b/" and
    ;; "http:///a" renders "http:/a". A FOURTH slash does not — "file:////x" stays
    ;; as written, because the path itself then begins "//" and the JVM keeps it.
    ;; Both shapes turn up: the first is File.toURL, the second is what a caller
    ;; builds by hand as "file:///" + an absolute path.
    (string-append proto ":"
                   (if (and (>= (string-length rest) 4)
                            (string=? (substring rest 0 3) "///")
                            (not (char=? (string-ref rest 3) #\/)))
                       (substring rest 2 (string-length rest))
                       rest))))
;; The constructors, told apart by argument TYPE the way the JVM's overloads are:
;;   (URL. spec)
;;   (URL. context spec)             context a URL or nil
;;   (URL. context spec handler)
;;   (URL. protocol host file)       three strings
;; A relative spec resolves against the context's directory; an absolute one
;; ignores the context, as on the JVM.
(define (url-resolve-spec context spec)
  (if (or (jolt-nil? context) (not context)
          ;; absolute: it carries its own scheme
          (let ((i (proto-colon-index spec))) (and i (> i 0))))
      spec
      (let* ((base (url-spec context))
             (cut (let loop ((j (- (string-length base) 1)))
                    (cond ((< j 0) #f)
                          ((char=? (string-ref base j) #\/) j)
                          (else (loop (- j 1)))))))
        (string-append (if cut (substring base 0 (+ cut 1)) base) spec))))
(define (proto-colon-index spec)
  (let loop ((j 0))
    (cond ((>= j (string-length spec)) #f)
          ((char=? (string-ref spec j) #\:) j)
          ;; a colon after a slash is part of the path, not a scheme
          ((char=? (string-ref spec j) #\/) #f)
          (else (loop (+ j 1))))))
(define (jolt-make-url . args)
  (cond
    ((null? args) (throw-jvm (quote IllegalArgumentException) "URL: no arguments"))
    ;; (URL. protocol host file) — first arg a string means the protocol form
    ((and (= (length args) 3) (string? (car args)) (not (url-jhost? (car args))))
     (let ((proto (jolt-str-render-one (car args)))
           (host (jolt-str-render-one (cadr args)))
           (file (jolt-str-render-one (caddr args))))
       (make-url (url-canonical (string-append proto "://" host file)))))
    ((= (length args) 1)
     (make-url (url-canonical (jolt-str-render-one (car args)))))
    ;; (URL. context spec [handler])
    (else
     (let* ((context (car args))
            (spec (jolt-str-render-one (cadr args)))
            (handler (and (>= (length args) 3) (caddr args))))
       (make-url (url-canonical (url-resolve-spec context spec)) handler)))))
(register-class-ctor! "URL" jolt-make-url)
(register-class-ctor! "java.net.URL" jolt-make-url)
;; (str url) is the spec, like the JVM — without this it renders the opaque
;; #object[java.net.URL] form and any caller that builds a path from it gets that
;; string instead.
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "url")))
                      url-spec)
(register-host-methods! "url"
  (list (cons "toString"       (lambda (self) (url-spec self)))
        (cons "toExternalForm" (lambda (self) (url-spec self)))
        (cons "getProtocol"    (lambda (self) (url-protocol (url-spec self))))
        (cons "getPath"        (lambda (self) (url-path (url-spec self))))
        (cons "getFile"        (lambda (self) (url-path (url-spec self))))
        (cons "getHost"        (lambda (self) (url-authority (url-spec self))))
        (cons "getName"        (lambda (self) (path-last-segment (url-path (url-spec self)))))
        ;; openStream / io/input-stream: a URL built with a stream handler reads
        ;; through it; a file: URL reads its target from disk; a URL of any other
        ;; protocol has no local backing and raises (the JVM would connect or read
        ;; the jar), never empty content.
        (cons "openConnection" (lambda (self . _) (url-open-connection self)))
        (cons "openStream"     (lambda (self)
                                 (let ((h (url-handler self)))
                                   (if h
                                       (record-method-dispatch (url-open-connection self)
                                                               "getInputStream" jolt-nil)
                                       (host-new "StringReader" (url-content self))))))))
;; The handler's own openConnection. Without one there is nothing to connect
;; through — say so rather than returning something that reads as empty.
(define (url-open-connection u)
  (let ((h (url-handler u)))
    (if h
        (record-method-dispatch h "openConnection" (jolt-list u))
        (throw-jvm (quote java.io.IOException)
                   (string-append "no protocol handler for: " (url-spec u))))))
;; (instance? java.net.URL x): the url jhost and an embedded-res (the jar: branch of
;; io/resource) both report java.net.URL. records-interop's case-string has no URL
;; arm, so answer it here where the two types live.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (symbol-t-name type-sym)))
      (if (or (string=? tn "URL") (string=? tn "java.net.URL"))
          (or (and (jhost? val) (string=? (jhost-tag val) "url")) (embedded-res? val))
          'pass))))

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
      ;; File.toURI returns a java.net.URI (JVM), not a String.
      ((string=? name "toURI")          (list (uri-parse (string-append "file:" (jfile-abs fp)))))
      ((string=? name "toURL")          (list (make-url (string-append "file:" (jfile-abs fp)))))
      ((string=? name "exists")         (list (if (file-exists? fp) #t #f)))
      ((string=? name "isDirectory")    (list (if (file-directory? fp) #t #f)))
      ((string=? name "isFile")         (list (if (and (file-exists? fp) (not (file-directory? fp))) #t #f)))
      ((string=? name "isAbsolute")     (list (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)) #t #f)))
      ;; listFiles builds each child from the path AS GIVEN (new File(this, name)
      ;; on the JVM), so a File made from a relative path lists relative children.
      ((string=? name "listFiles")      (list (list->cseq (map make-jfile (jolt-list-dir p)))))
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
      ((string=? name "setLastModified")
       (list (guard (e (#t #f))
               (set-file-mtime-millis! fp (exact (floor (car args)))))))
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
      ((string=? name "toPath")           (list (make-nio-path p)))  ; -> java.nio.file.Path (nio-file.ss)
      ((string=? name "getAbsoluteFile")  (list (make-jfile (jfile-abs fp))))
      ((string=? name "getCanonicalFile") (list (make-jfile (jfile-abs fp))))
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

(register-method-arm! arm-priority-file
  (lambda (obj method-name rest-args)
    (if (jfile? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (jfile-method obj method-name rest)))
          (if r (car r) (throw-jvm (quote IllegalArgumentException) (string-append "No matching method for File: " method-name))))
        'pass)))
;; An embedded resource shares the tier: io/resource returns one of these where a
;; source root would have yielded a jfile, so it has to answer the same methods.
(register-method-arm! arm-priority-file
  (lambda (obj method-name rest-args)
    (if (embedded-res? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (embedded-res-method obj method-name rest)))
          (if r (car r)
              (throw-jvm (quote IllegalArgumentException)
                         (string-append "No matching method for an embedded resource: " method-name))))
        'pass)))
(register-class-arm! embedded-res? (lambda (x) "java.net.URL"))
;; (str resource) is the resource name, like URL.toString — which also gives the
;; printer's #object[…] fallback its content.
(register-str-render! embedded-res? (lambda (x) (embedded-res-name x)))

;; File methods emitted via jolt-host-call (rt.ss) need jfile dispatch,
;; not the string-path shims in the base jolt-host-call. Route through
;; the jfile-method table so all File methods share one dispatch point.
(define %io-host-call jolt-host-call)
(set! jolt-host-call
  (lambda (method target . args)
    (if (jfile? target)
        (let ((r (jfile-method target method args)))
          (if r (car r) (apply %io-host-call method target args)))
        (apply %io-host-call method target args))))

;; --- slurp / spit / flush ---------------------------------------------------
(define (read-file-string path)
  (with-port (open-input-file path)
    (lambda (p) (let ((s (get-string-all p))) (if (eof-object? s) "" s)))))

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
  (and (jhost? x)
       (or (string=? (jhost-tag x) "string-reader")
           (pushback-reader-tag? (jhost-tag x)))))

;; Refill a host reader so subsequent read/slurp see `s` (the unconsumed tail).
(define (reader-refill! r s)
  (cond
    ((string=? (jhost-tag r) "string-reader")
     (vector-set! (jhost-state r) 0 s) (vector-set! (jhost-state r) 1 0))
    ((pushback-reader-tag? (jhost-tag r))
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
;; Reading a path that isn't there is java.io.FileNotFoundException on the JVM, and
;; libraries branch on it: instaparse decides whether its argument is a grammar or
;; a file by slurping and catching FNF. A raw Chez open-input-file condition is not
;; catchable as that class, so the caller's fallback never runs.
(define (slurp-path path)
  (unless (file-exists? path)
    (throw-jvm (quote java.io.FileNotFoundException)
               (string-append path " (No such file or directory)")))
  (read-file-string path))
;; The content a URL names, as text: a file: URL reads its target from disk (a
;; missing file is a FileNotFoundException, as on the JVM); any other protocol has
;; no local backing, so raise rather than hand back empty content. slurp /
;; io/reader / io/input-stream / .openStream all reach a URL through here.
(define (url-content u)
  (let ((spec (url-spec u)))
    (cond
      ;; a stream handler decides what this URL means, whatever its protocol
      ((url-handler u)
       (drain-any-stream (record-method-dispatch (url-open-connection u)
                                                 "getInputStream" jolt-nil)))
      ((string=? (url-protocol spec) "file") (slurp-path (url-strip-scheme spec)))
      (else (throw-jvm (quote java.io.IOException)
                       (string-append "protocol doesn't support input: " spec))))))
;; Whatever the handler handed back: a byte stream, a reader, or a value that
;; already renders as its content.
(define (drain-any-stream s)
  (cond ((reader-jhost? s) (drain-reader s))
        ((and (jhost? s) (string=? (jhost-tag s) "in-stream"))
         (utf8->string (na-bytearray->bv
                        (record-method-dispatch s "readAllBytes" jolt-nil))))
        (else (jolt-str-render-one s))))
(define (jolt-slurp src . opts)
  (cond
    ((jfile? src) (slurp-path (jfile-fs src)))
    ((embedded-res? src)
     (let ((c (embedded-res-content src)))
       (if (bytevector? c) (utf8->string c) c)))
    ((reader-jhost? src) (drain-reader src))
    ;; a file: URL reads its target (jar:/http:/… raise in url-content).
    ((and (jhost? src) (string=? (jhost-tag src) "url")) (url-content src))
    ;; bytes (a bytevector or a jolt byte-array): decode with :encoding (UTF-8
    ;; default). clj-http-lite slurps response-body byte arrays.
    ((bytevector? src) (decode-bytevector src (slurp-encoding opts)))
    ((and (jolt-array? src) (eq? (jolt-array-kind src) 'byte))
     (decode-bytevector (na-bytearray->bv src) (slurp-encoding opts)))
    ;; a byte input-stream shim (e.g. clj-http-lite's :as :stream body): drain it.
    ((and (htable? src) (jolt-truthy? (jolt-ref-get src (keyword "jolt" "input-stream"))))
     (decode-bytevector (drain-byte-stream src) (slurp-encoding opts)))
    ((string? src) (slurp-path (project-relative src)))
    (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str src) "> as a Reader.")))))

(define (spit-append? opts)
  (let loop ((o opts))
    (cond ((or (null? o) (null? (cdr o))) #f)
          ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "append")
                (jolt-truthy? (cadr o))) #t)
          (else (loop (cddr o))))))

(define io-counter-mutex (make-mutex))
(define spit-tmp-counter 0)
(define (jolt-spit path content . opts)
  ;; Render BEFORE any file is touched — a throwing toString used to leave the
  ;; target truncated. The non-append write goes to a temp file in the same
  ;; directory and renames over the target, so a mid-write failure (disk full)
  ;; never destroys the original. Append keeps writing in place.
  (let* ((p (project-relative (file-path-of path)))
         (text (jolt-str-render-one content)))
    (if (spit-append? opts)
        (with-port (open-output-file p 'append)
          (lambda (port) (put-string port text)))
        (let ((tmp (string-append p ".spit-tmp-"
                                   (number->string (real-time)) "-"
                                   (number->string (with-mutex io-counter-mutex
                                                     (begin (set! spit-tmp-counter (+ spit-tmp-counter 1))
                                                            spit-tmp-counter))))))
          (with-port (open-output-file tmp 'replace)
            (lambda (port) (put-string port text)))
          (guard (e (#t (guard (_ (#t #f)) (delete-file tmp)) (raise e)))
            (rename-file tmp p))))
    jolt-nil))

;; (flush) is (.flush *out*) on the JVM. When *out* holds a real writer — a
;; StringWriter, an OutputStreamWriter over a stream, a reify or proxy one — the
;; flush has to reach THAT, mirroring how jolt-write routes a write (printing.ss).
;; Flushing only the Chez port left a buffered writer unflushed, so text printed
;; through an OutputStreamWriter never reached the stream underneath it.
(define flush-out-cell #f)
(define (jolt-flush)
  (let ((w (begin (unless flush-out-cell
                    (set! flush-out-cell (jolt-var "clojure.core" "*out*")))
                  (var-cell-deref flush-out-cell))))
    (if (and (or (iface-method w "flush" #f)
                 (and (jhost? w)
                      (not (and (string=? (jhost-tag w) "port-writer")
                                (eq? (vector-ref (jhost-state w) 0) 'out)))))
             w)
        (record-method-dispatch w "flush" jolt-nil)
        (flush-output-port (current-output-port))))
  jolt-nil)

;; --- str / type / instance? integration ------------------------------------
;; str of a jfile is its path (Clojure's File.toString).
(register-str-render! jfile? jfile-path)

;; stdin line seam: the clojure.core *in* reader (50-io.clj) drives read-line /
;; read / read+string through __stdin-read-line. Return the next line (newline
;; stripped) or nil at EOF. Without this, (read-line) and the REPL call nil.
(def-var! "clojure.core" "__stdin-read-line"
  (lambda () (let ((l (get-line (current-input-port)))) (if (eof-object? l) jolt-nil l))))

;; (type f) -> :jolt/file (the tagged-file :jolt/type). Registered through the
;; type-arm registry (natives-meta.ss) so the dispatcher picks it up.
(define io-kw-file (keyword "jolt" "file"))
(register-type-arm! jfile? (lambda (x) io-kw-file))

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
    ((and (jhost? x) (or (pushback-reader-tag? (jhost-tag x))
                         (member (jhost-tag x) '("string-reader" "writer"
                                                 "file-writer" "port-writer" "print-writer"))))
     (record-method-dispatch x "close" jolt-nil) jolt-nil)
    ;; a library's stream shim (tagged-table) closes via its registered .close
    ;; method (a no-op for in-memory streams); absent method -> no-op.
    ((htable? x) (guard (e (#t jolt-nil)) (record-method-dispatch x "close" jolt-nil)) jolt-nil)
    ((jfile? x) jolt-nil)
    ;; a deftype/defrecord that implements a `close` method (java.io.Closeable /
    ;; AutoCloseable, e.g. tools.reader's reader types) closes through it — the
    ;; same method (.close x) would dispatch to.
    ((and (jrec? x) (jrec-cl x "close"))
     (record-method-dispatch x "close" jolt-nil) jolt-nil)
    (else
     (let ((closef (jolt-get x (keyword #f "close") jolt-nil)))
       (if (and (not (jolt-nil? closef)) (procedure? closef))
           (begin (jolt-invoke closef) jolt-nil)
           (throw-jvm (quote IllegalArgumentException) "with-open: no .close method on value"))))))
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
    ((embedded-res? x)
     (let ((c (embedded-res-content x)))
       (host-new "StringReader" (if (bytevector? c) (utf8->string c) c))))
    ((url-jhost? x) (host-new "StringReader" (url-content x)))
    ((string? x) (host-new "StringReader" (read-file-string (project-relative x))))
    ((or (cseq? x) (empty-list-t? x) (pvec? x))
     (host-new "StringReader" (seq-source->string x)))
    ;; anything else is not a source, and quietly rendering it would read as empty
    ;; content — (io/reader nil) used to hand back a reader over "" rather than say
    ;; so. Same coercion error the JVM raises, and the same one io/writer raises.
    (else (throw-jvm (quote IllegalArgumentException)
                     (string-append "Cannot open <" (jolt-pr-str x) "> as a Reader.")))))

;; --- clojure.java.io/writer: an existing writer passes through; a File / path
;; gets a file-backed writer (host-static.ss "file-writer") that persists on
;; flush/close. Mirrors io.clj's writer over the host's StringWriter/file ports.
(define (jolt-io-writer x)
  (cond
    ((and (jhost? x) (string=? (jhost-tag x) "writer")) x)
    ((and (jhost? x) (string=? (jhost-tag x) "file-writer")) x)
    ((jfile? x) (make-jhost "file-writer" (vector (jfile-path x) "")))
    ((string? x) (make-jhost "file-writer" (vector x "")))
    (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str x) "> as a Writer.")))))

;; --- clojure.java.io ns -----------------------------------------------------
(def-var! "clojure.java.io" "file" jolt-make-file)
;; io/as-file of a file: URL yields the file it points at (JVM: new
;; File(url.toURI())); a URL with any other protocol has no filesystem path —
;; IllegalArgumentException, as the JVM's File(URI) throws.
(define (url-file-coercion u)
  (if (string=? (url-protocol (url-spec u)) "file")
      (make-jfile (url-strip-scheme (url-spec u)))
      (throw-jvm 'IllegalArgumentException (string-append "Not a file: " (url-spec u)))))
(def-var! "clojure.java.io" "as-file"
  (lambda (x) (cond ((jfile? x) x)
                    ((and (jhost? x) (string=? (jhost-tag x) "url")) (url-file-coercion x))
                    (else (make-jfile (file-path-of x))))))
;; "reader" is bound by natives-array.ss (loaded later) so a char[] argument is
;; handled; that binding delegates here via jolt-io-reader for everything else.
(def-var! "clojure.java.io" "writer" jolt-io-writer)
(def-var! "clojure.java.io" "input-stream" jolt-io-reader)
(def-var! "clojure.java.io" "output-stream" jolt-io-writer)
;; resource: jolt has no classpath, so a named resource is resolved against the
;; loader's source roots (a project's :paths, e.g. "resources"). Returns a file:
;; URL for the first match (a jar:-classed embedded-res if the file is baked into a
;; built binary), else nil — matching the JVM, which returns a java.net.URL. Both
;; branches answer the same URL surface. get-source-roots is the loader's accessor
;; (loader.ss), resolved at call time — the runtime CLI loads it.
;; The file: URL for `nm` under source root `root`, ABSOLUTE as the JVM
;; classloader's always is. Source roots are usually relative ("./stdlib"), and
;; "file:./stdlib/x" is not a valid absolute URL — a consumer that resolves
;; another name against it gets MalformedURLException "no protocol". (Selmer
;; stores the URL from (io/resource "templates/…") and resolves template names
;; against it, which is where this surfaced.) Absolutize against user.dir, the
;; base every other filesystem touch uses, dropping a leading "./" so the path
;; reads like the JVM's instead of carrying a "/./" segment.
(define (resource-file-url root nm)
  (let* ((rel (string-append root "/" nm))
         (rel (if (and (>= (string-length rel) 2)
                       (char=? (string-ref rel 0) #\.)
                       (char=? (string-ref rel 1) #\/))
                  (substring rel 2 (string-length rel))
                  rel)))
    (make-url (string-append "file:" (jfile-abs rel)))))

(define (jolt-io-resource name)
  (let* ((nm (jolt-str-render-one name))
         (emb (hashtable-ref embedded-resources nm #f)))
    (if emb (make-embedded-res nm emb)
        (let loop ((roots (get-source-roots)))
          (cond ((null? roots) jolt-nil)
                ((file-exists? (string-append (car roots) "/" nm))
                 (resource-file-url (car roots) nm))
                (else (loop (cdr roots))))))))
(def-var! "clojure.java.io" "resource" jolt-io-resource)
;; as-url honors a library-registered URL class (e.g. jolt-lang/http-client's full
;; java.net.URL shim) so io/as-url and (URL. spec) agree; else the file-only jhost.
;; as-url of a File is File.toURL — a file: URL (JVM), so the spec carries the
;; scheme; a bare string keeps its spec as given.
(def-var! "clojure.java.io" "as-url"
  (lambda (x)
    (cond ((and (jhost? x) (string=? (jhost-tag x) "url")) x)
          ((htable? x) x)
          (else (let ((spec (if (jfile? x) (string-append "file:" (jfile-fs x)) (jolt-str-render-one x)))
                      (ctor (lookup-class class-ctors-tbl "URL")))
                  (if ctor (ctor spec) (make-url spec)))))))

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
             (resource-file-url (car roots) nm))
            (else (loop (cdr roots)))))))
;; getResources: every source root that holds the named resource, as file: URLs
;; (enumeration-seq just calls seq, so a list serves). ring's static-resource
;; symlink check enumerates these to confirm a served file sits under a root.
(define (cl-get-resources self name)
  (let ((nm (jolt-str-render-one name)))
    (let loop ((roots (get-source-roots)) (acc '()))
      (cond ((null? roots) (list->cseq (reverse acc)))
            ((file-exists? (string-append (car roots) "/" nm))
             (loop (cdr roots) (cons (resource-file-url (car roots) nm) acc)))
            (else (loop (cdr roots) acc))))))
(register-host-methods! "classloader"
  (list (cons "getResource" cl-get-resource)
        (cons "getResources" cl-get-resources)
        ;; jolt has a single loader, so it has no parent — the same answer the
        ;; JVM's bootstrap loader gives, which terminates the usual
        ;; (take-while identity (iterate #(.getParent %) loader)) walk.
        (cons "getParent" (lambda (self) jolt-nil))
        (cons "getResourceAsStream"
              (lambda (self name)
                (let ((u (cl-get-resource self name)))
                  (if (jolt-nil? u) jolt-nil (host-new "StringReader" (jolt-slurp (url-strip-scheme (url-spec u))))))))))
(register-class-statics! "java.lang.ClassLoader" (list (cons "getSystemClassLoader" (lambda () the-classloader))))
;; clojure.lang.RT/baseLoader — the resource-resolving class loader (RT/baseLoader
;; is how libraries reach Clojure's base loader, e.g. aws-api's resources ns).
(register-class-statics! "clojure.lang.RT" (list (cons "baseLoader" (lambda () the-classloader))))
;; java.lang.Class's loader surface: jolt loads every class through the single
;; source-root loader, so any Class reports it (on the JVM bootstrap classes
;; return null; here the loader itself answers nil for resources it can't serve,
;; which is the answer classpath-probing callers like orchard's source-file
;; resolution need). Class.getResource resolves a relative name against the
;; class's package before delegating — JVM semantics.
(define (class-resource-name class-name name)
  (if (and (> (string-length name) 0) (char=? (string-ref name 0) #\/))
      (substring name 1 (string-length name))
      (let loop ((i (- (string-length class-name) 1)))
        (cond ((< i 0) name)
              ((char=? (string-ref class-name i) #\.)
               (string-append (ns-name->rel (substring class-name 0 i)) "/" name))
              (else (loop (- i 1)))))))
(register-host-methods! "class"
  (list (cons "getClassLoader" (lambda (self) the-classloader))
        (cons "getResource"
              (lambda (self name)
                (cl-get-resource the-classloader
                                 (class-resource-name (jclass-name self) (jolt-str-render-one name)))))
        (cons "getResourceAsStream"
              (lambda (self name)
                (let ((u (cl-get-resource the-classloader
                                          (class-resource-name (jclass-name self) (jolt-str-render-one name)))))
                  (if (jolt-nil? u) jolt-nil (host-new "StringReader" (jolt-slurp (url-strip-scheme (url-spec u))))))))))
;; clojure.lang.RT/nextID — process-unique increasing id (AtomicInteger(1)
;; getAndIncrement), used by id generators such as core.logic's lvar.
(define rt-next-id-counter 1)
(define (rt-next-id)
  (with-mutex io-counter-mutex
    (let ((v rt-next-id-counter))
      (set! rt-next-id-counter (+ rt-next-id-counter 1))
      v)))
(register-class-statics! "RT" (list (cons "nextID" rt-next-id)))
(register-class-statics! "clojure.lang.RT" (list (cons "nextID" rt-next-id)))
;; clojure.lang.Util — hash/equality helpers libraries call directly (core.logic's
;; LCons.hashCode uses Util/hash). hash = Java hashCode (0 for nil); hasheq = the
;; value hash jolt's = uses; equiv = value equality; identical = reference identity.
(let ((util-statics
       (list (cons "hash" (lambda (x) (if (jolt-nil? x) 0 (record-method-dispatch x "hashCode" jolt-nil))))
             (cons "hasheq" (lambda (x) (jolt-hash x)))
             (cons "equiv" (lambda (a b) (if (jolt= a b) #t #f)))
             (cons "identical" (lambda (a b) (if (eq? a b) #t #f)))
             ;; the boost-style mixer Symbol/Keyword hash with, and that a
             ;; library folding several hashes into one calls directly
             (cons "hashCombine"
                   (lambda (seed h) (hash-combine (jolt->fx seed) (jolt->fx h)))))))
  (register-class-statics! "Util" util-statics)
  (register-class-statics! "clojure.lang.Util" util-statics))
;; Thread/currentThread -> a fresh thread jhost wrapping THIS thread's interrupt
;; flag (the box from current-interrupt-box, host-static.ss), so .interrupt from
;; any thread sets the target thread's flag and .isInterrupted reads it without
;; clearing (instance semantics; the static Thread/interrupted reads-and-clears).
;; getContextClassLoader hands back the loader.
(register-host-methods! "thread"
  (list (cons "getContextClassLoader" (lambda (self) the-classloader))
        (cons "getName" (lambda (self) "main"))
        ;; no reified call stack (jolt does TCO, so caller frames are erased) — an
        ;; empty StackTraceElement[]. clojure.spec.test.alpha's instrument reads it
        ;; to name the caller var; it degrades to no ::caller, the conform error
        ;; (the ExceptionInfo) is still thrown.
        (cons "getStackTrace" (lambda (self) (jolt-vector)))
        (cons "interrupt" (lambda (self)
                            (when (box? (jhost-state self)) (set-box! (jhost-state self) #t))
                            jolt-nil))
        (cons "isInterrupted" (lambda (self)
                                (and (box? (jhost-state self)) (unbox (jhost-state self)) #t)))))
;; ONE handle per thread, cached in a thread parameter. The JVM's
;; Thread/currentThread is identity-stable, and code relies on it: keying a map by
;; the current thread, or comparing two calls with identical?/=. Allocating a fresh
;; jhost per call made every such comparison false — tools.logging's suite tags each
;; log entry with its calling thread and then asks whether it was logged directly.
;; The cell carries the owning thread's id for the same reason current-interrupt-box
;; does: a Chez thread parameter is inherited by a forked thread, and a child must
;; not report the parent's handle as its own.
(define thread-handle-cell (make-thread-parameter #f))      ; (thread-id . handle)
;; Mirror of the per-thread cache keyed by thread id, so another thread can name
;; this one — Thread/getAllStackTraces has to hand back the SAME handle
;; currentThread does, or a caller cannot find itself in the map.
(define thread-handles-by-id (make-eqv-hashtable))
(define thread-handles-mutex (make-mutex))
(define (current-thread-handle)
  (let ((c (thread-handle-cell))
        (id (get-thread-id)))
    (if (and (pair? c) (eqv? (car c) id))
        (cdr c)
        (let ((h (make-jhost "thread" (current-interrupt-box))))
          (thread-handle-cell (cons id h))
          (with-mutex thread-handles-mutex (hashtable-set! thread-handles-by-id id h))
          h))))
;; A handle for a thread that has never asked who it is. Its interrupt box is its
;; own, so .interrupt through it does not reach that thread — the thread adopts a
;; real handle the moment it calls currentThread.
(define (thread-handle-for-id id)
  (if (eqv? id (get-thread-id))
      (current-thread-handle)              ; the caller must find ITSELF in the map
      (or (with-mutex thread-handles-mutex (hashtable-ref thread-handles-by-id id #f))
          (let ((h (make-jhost "thread" (box #f))))
            (with-mutex thread-handles-mutex (hashtable-set! thread-handles-by-id id h))
            h))))
;; Thread/getAllStackTraces: the live threads mapped to EMPTY stack traces. jolt
;; reifies no call stack (TCO erases caller frames) and .getStackTrace is already
;; an empty StackTraceElement[], so the traces are honestly empty; the thread set
;; is real, which is what the callers want — ring's suites count threads before
;; and after a request to check for leaks.
(define (all-stack-traces)
  (let loop ((ids (cons (get-thread-id) (live-thread-ids)))
             (seen '())
             (m empty-pmap))
    (cond ((null? ids) m)
          ((memv (car ids) seen) (loop (cdr ids) seen m))
          (else (loop (cdr ids) (cons (car ids) seen)
                      (jolt-assoc m (thread-handle-for-id (car ids)) (jolt-vector)))))))
(let ((statics (list (cons "currentThread" current-thread-handle)
                     (cons "getAllStackTraces" all-stack-traces))))
  (register-class-statics! "Thread" statics)
  (register-class-statics! "java.lang.Thread" statics))

;; --- java.io.File / java.util.UUID constructors -----------------------------
;; (java.io.File. parent child) joins with exactly ONE separator: File(parent,
;; child) normalizes, so a parent that already ends in "/" does not produce a
;; doubled slash (ring's resource middleware builds "assets/" + "index.html").
;; A child that starts with a separator is joined the same way, and an empty
;; child yields the parent's path alone -- all four checked against the JVM.
(define (jolt-file-join parent child)
  (let* ((p (file-path-of parent))
         (c (file-path-of child))
         (p (if (and (> (string-length p) 1)
                     (char=? (string-ref p (- (string-length p) 1)) #\/))
                (substring p 0 (- (string-length p) 1))
                p))
         (c (let strip ((i 0))
              (cond ((= i (string-length c)) c)
                    ((char=? (string-ref c i) #\/) (strip (+ i 1)))
                    (else (substring c i (string-length c)))))))
    (cond ((string=? c "") p)
          ((string=? p "/") (string-append "/" c))
          (else (string-append p "/" c)))))
(register-class-ctor! "File"
  (lambda (a . rest)
    (if (pair? rest)
        (jolt-make-file (jolt-file-join a (car rest)))
        (jolt-make-file a))))
;; File statics: the platform separators plus createTempFile / listRoots.
(define temp-file-counter 0)
(define (file-create-temp prefix suffix . dir)
  ;; the JVM rejects a prefix under three characters, so a caller that works here
  ;; works there too
  (when (< (string-length (jolt-str-render-one prefix)) 3)
    (throw-jvm (quote IllegalArgumentException)
               (string-append "Prefix string \"" (jolt-str-render-one prefix)
                              "\" too short: length must be at least 3")))
  (let* ((d (cond ((pair? dir) (file-path-of (car dir)))
                  ((getenv "TMPDIR") => (lambda (t) t))
                  (else "/tmp")))
         (sfx (if (or (null? (list suffix)) (jolt-nil? suffix)) ".tmp" (jolt-str-render-one suffix))))
    (let ((n (with-mutex io-counter-mutex
              (set! temp-file-counter (+ temp-file-counter 1))
              temp-file-counter)))
    (let loop ((n n))
      (let ((p (string-append d "/" (jolt-str-render-one prefix)
                              (number->string (now-millis)) "-" (number->string n) sfx)))
        (if (file-exists? p) (loop (+ n 1))
            (begin (close-port (open-output-file p 'truncate)) (make-jfile p))))))))
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
        (jolt-make-file (jolt-file-join a (car rest)))
        (jolt-make-file a))))
;; UUID: randomUUID / fromString statics + a (UUID. s) string ctor. Registering
;; under the FQN also registers the short name (shared member table).
(register-class-statics! "java.util.UUID"
  (list (cons "randomUUID" (lambda () (jolt-random-uuid)))
        (cons "fromString" (lambda (s) (jolt-parse-uuid (jolt-str-render-one s))))))
;; (UUID. msb lsb): build from the most/least-significant 64-bit halves (the JVM's
;; 2-long ctor), the form test.check's uuid generator uses. (UUID. s) parses a
;; string. The 128 bits format as the canonical 8-4-4-4-12 lowercase hex string.
(define (uuid-long->hex16 n)
  (let* ((u (bitwise-and (jnum->exact n) #xFFFFFFFFFFFFFFFF))
         (s (string-downcase (number->string u 16))))   ; JVM UUIDs are lowercase
    (string-append (make-string (- 16 (string-length s)) #\0) s)))
(define (uuid-from-halves msb lsb)
  (let ((h (uuid-long->hex16 msb)) (l (uuid-long->hex16 lsb)))
    (make-juuid (string-append (substring h 0 8) "-" (substring h 8 12) "-" (substring h 12 16)
                               "-" (substring l 0 4) "-" (substring l 4 16)))))
(define (uuid-ctor . args)
  (if (= (length args) 2)
      (uuid-from-halves (car args) (cadr args))
      (jolt-parse-uuid (jolt-str-render-one (car args)))))
(register-class-ctor! "UUID" uuid-ctor)
(register-class-ctor! "java.util.UUID" uuid-ctor)
;; (Long. n) / (Long. "n"): a Long is just jolt's integer; return it (parse a string).
(register-class-ctor! "Long" (lambda (x) (if (string? x) (parse-int-or-throw x 10 "Long") (->num (jnum->exact x)))))
(register-class-ctor! "java.lang.Long" (lambda (x) (if (string? x) (parse-int-or-throw x 10 "Long") (->num (jnum->exact x)))))
;; (Integer. n) / (Integer. "n"): jolt's integer, range-checked like intCast.
(define (integer-ctor x)
  (jolt-int-cast (if (string? x) (parse-int-or-throw x 10 "Integer") x)))
(register-class-ctor! "Integer" integer-ctor)
(register-class-ctor! "java.lang.Integer" integer-ctor)
;; (Double. x) / (Double. "x"): jolt's double.
(define (double-ctor x)
  (if (string? x)
      (let ((n (string->number x)))
        (if n (exact->inexact n)
            (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                                             (string-append "For input string: \"" x "\"")))))
      (jolt-double x)))
(register-class-ctor! "Double" double-ctor)
(register-class-ctor! "java.lang.Double" double-ctor)

;; (Boolean. "true") / (Boolean. b): true for the string "true" (case-insensitive,
;; anything else false) or the boolean itself — Boolean.valueOf semantics; the
;; box is jolt's plain boolean.
(define (boolean-ctor x)
  (cond ((string? x) (string-ci=? x "true"))
        ((boolean? x) x)
        (else #f)))
(register-class-ctor! "Boolean" boolean-ctor)
(register-class-ctor! "java.lang.Boolean" boolean-ctor)

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
           (user-info (and at (substring authority 0 at)))
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
              (cons 'user-info (or user-info jolt-nil))
              (cons 'port (->num port))
              (cons 'path (if (= 0 (string-length path)) (if has-auth "" jolt-nil) path))
              (cons 'query (or query jolt-nil))
              (cons 'fragment (or frag jolt-nil)))))))
(define (uri-field u k) (let ((p (assq k (jhost-state u)))) (if p (cdr p) jolt-nil)))
(register-class-ctor! "URI" (lambda (s) (uri-parse (jolt-str-render-one s))))
(register-class-ctor! "java.net.URI" (lambda (s) (uri-parse (jolt-str-render-one s))))
;; URI/create — the static factory, same as the (URI. s) constructor.
(register-class-statics! "java.net.URI" (list (cons "create" (lambda (s) (uri-parse (jolt-str-render-one s))))))
(register-host-methods! "uri"
  (list (cons "toString" (lambda (u) (uri-field u 'string)))
        (cons "toASCIIString" (lambda (u) (uri-field u 'string)))
        (cons "getScheme" (lambda (u) (uri-field u 'scheme)))
        (cons "getAuthority" (lambda (u) (uri-field u 'authority)))
        (cons "getHost" (lambda (u) (uri-field u 'host)))
        (cons "getUserInfo" (lambda (u) (uri-field u 'user-info)))
        (cons "getRawUserInfo" (lambda (u) (uri-field u 'user-info)))
        (cons "getPort" (lambda (u) (uri-field u 'port)))
        (cons "getPath" (lambda (u) (uri-field u 'path)))
        (cons "getRawPath" (lambda (u) (uri-field u 'path)))
        (cons "getQuery" (lambda (u) (uri-field u 'query)))
        (cons "getRawQuery" (lambda (u) (uri-field u 'query)))
        (cons "getFragment" (lambda (u) (uri-field u 'fragment)))
        ;; URI.toURL = new URL(toString()) (JVM); honors a library-registered
        ;; URL shim like io/as-url does.
        (cons "toURL" (lambda (u) (let ((ctor (lookup-class class-ctors-tbl "URL")))
                                    (if ctor (ctor (uri-field u 'string))
                                        (make-url (uri-field u 'string))))))
        (cons "isAbsolute" (lambda (u) (not (jolt-nil? (uri-field u 'scheme)))))
        (cons "hashCode" (lambda (u) (string-hash (uri-field u 'string))))
        (cons "equals" (lambda (u o) (and (jhost? o) (string=? (jhost-tag o) "uri")
                                          (string=? (uri-field u 'string) (uri-field o 'string)))))))
;; (= f1 f2) is value equality by pathname, like java.io.File.equals — .equals
;; and hash already agreed, so two Files built from the same path compared equal
;; through the method and unequal through =, which is how ring's resource tests
;; read (not (= #object[java.io.File "…/foo.html"] #object[java.io.File "…/foo.html"])).
(register-eq-arm! (lambda (a b) (or (jfile? a) (jfile? b)))
                  (lambda (a b) (and (jfile? a) (jfile? b)
                                     (string=? (jfile-path a) (jfile-path b)))))

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
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "url"))) (lambda (x) "java.net.URL"))
(register-class-arm! juuid? (lambda (x) "java.util.UUID"))
(register-class-arm! jfile? (lambda (x) "java.io.File"))
