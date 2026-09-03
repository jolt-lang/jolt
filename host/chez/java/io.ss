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

;; FileSystem.normalize(): runs of "/" collapse to one and a trailing "/" is
;; dropped. "." and ".." are left alone -- the JVM's constructor does not resolve
;; those, and neither does this. new File("/a/b//c").getPath() is "/a/b/c".
;;
;; Every path the ONE-argument constructor produces is normalized, and so is
;; every path built by as-file, the file: URL coercion, createTempFile,
;; getParentFile and listRoots -- nine construction sites of which only one is
;; the constructor entry point. So make-jfile normalizes and they all go through
;; it, rather than the invariant being restated nine times.
;;
;; The two-argument constructor normalizes each ARGUMENT and then resolves them.
;; That result is normal too, on every JDK from 21. See jolt-file-join.
(define (path-has-double-sep? p n)
  (let loop ((i 1))
    (and (fx<? i n)
         (or (and (char=? (string-ref p i) #\/) (char=? (string-ref p (fx- i 1)) #\/))
             (loop (fx+ i 1))))))

(define (jolt-path-normalize p)
  (let ((n (string-length p)))
    (cond
      ;; an already-normal path is the overwhelmingly common case, and a jfile is
      ;; built per entry on every directory listing: look before copying, so the
      ;; answer is p itself and nothing is allocated
      ((not (path-has-double-sep? p n))
       ;; a trailing separator goes, but "/" is a path, not an empty one
       (if (and (fx>? n 1) (char=? (string-ref p (fx- n 1)) #\/))
           (substring p 0 (fx- n 1))
           p))
      (else
       (let ((out (make-string n)))
         (let loop ((i 0) (j 0) (prev-slash? #f))
           (if (fx=? i n)
               (let ((j (if (and (fx>? j 1) (char=? (string-ref out (fx- j 1)) #\/))
                            (fx- j 1)
                            j)))
                 (substring out 0 j))
               (let ((c (string-ref p i)))
                 (cond ((and (char=? c #\/) prev-slash?) (loop (fx+ i 1) j #t))
                       (else (string-set! out j c)
                             (loop (fx+ i 1) (fx+ j 1) (char=? c #\/))))))))))))

(define-record-type jfile (fields path) (nongenerative jolt-jfile-v1)
  (protocol (lambda (new) (lambda (p) (new (jolt-path-normalize p))))))
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

;; Embedded compiled fasls for install-owned stdlib namespaces. build-jolt bakes
;; one fasl per namespace into the binary so a require loads the compiled code
;; instead of recompiling from source on every process start. Same seam and
;; locking discipline as register-embedded-resource! (written only at heap-build
;; time, single-threaded before scheme-start; read at runtime by single-key
;; hashtable-ref, safe on strong-general hashtables). Keyed by ns name, not path.
;;
;; Two ways in. register-embedded-fasl! is the explicit registry: tests and other
;; embedders can hand a bytevector directly. The built binary does NOT bake the
;; (multi-MB) fasl bytes as boot-image literals — that regresses startup, since a
;; flat.ss literal re-materializes at every Sbuild_heap. Instead build-jolt xxd's
;; one concatenated blob into the linked binary as the C array jolt_stdlib_fasls
;; (-rdynamic exports it) and records an index of (ns offset length) triples; the
;; launcher calls jolt-stdlib-fasls-attach! with that index before any require.
;; jolt-embedded-fasl then memcpy's the slice out of the C array on demand — once
;; per ns per process, never cached.
(define embedded-fasls (make-hashtable string-hash string=?))
(define (register-embedded-fasl! name bv) (hashtable-set! embedded-fasls name bv))
;; ns-name -> (offset . length) into the linked jolt_stdlib_fasls C array.
;; Populated once by the launcher's jolt-stdlib-fasls-attach!; empty in every
;; path that carries no such array (dev bin/jolt, devcache, app binaries).
(define embedded-fasl-index (make-hashtable string-hash string=?))
(define (jolt-stdlib-fasls-attach! index)
  (for-each (lambda (entry)
              (hashtable-set! embedded-fasl-index (car entry)
                              (cons (cadr entry) (caddr entry))))
            index))
;; memcpy `length` bytes at `offset` out of the jolt_stdlib_fasls C array into a
;; fresh bytevector. #f when the symbol is absent (dev/devcache/apps carry no such
;; array) or the fetch raises, so the caller falls back to today's source path.
;; sa-foreign-entry-address (foreign-entry) returns an integer address, so
;; base+offset is plain arithmetic. Same memcpy pattern as the launcher's
;; jolt-materialize-bundles!.
;;
;; No (sa-load-shared-object #f) here: the boot already loaded the process-global
;; handle once, which makes jolt_stdlib_fasls (an exported symbol of THIS binary)
;; resolvable. Re-loading re-promotes the global handle to the head of Chez's
;; foreign-entry search order (most-recently-loaded first), so it outranks every
;; explicitly loaded native — after any fetch, an EVP_* bind resolves to Apple's
;; BoringSSL in /usr/lib and jolt.ffi's defcfn caches that bad fp forever
;; (jolt-lang/crypto, Selmer's hash filter). The same hazard exists in the
;; launcher's jolt-materialize-bundles! (build-jolt.ss).
(define (jolt-stdlib-fasl-fetch offset length)
  (guard (e (else #f))
    (let* ((base (sa-foreign-entry-address "jolt_stdlib_fasls"))
           (bv (make-bytevector length))
           (memcpy (sa-foreign-procedure "memcpy" (u8* uptr uptr) void*)))
      (memcpy bv (+ base offset) length)
      bv)))
(define (jolt-embedded-fasl name)
  (let ((v (hashtable-ref embedded-fasls name #f)))
    (cond
      ((bytevector? v) v)
      (else
       (let ((ol (hashtable-ref embedded-fasl-index name #f)))
         (and ol (jolt-stdlib-fasl-fetch (car ol) (cdr ol))))))))

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
        ((eq? (sa-os-family) 'windows) 0)
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

;; --- canonical paths --------------------------------------------------------
;; getCanonicalPath is realpath(3), not "make it absolute": it resolves
;; symlinks as well as "." and "..". Answering with the absolute path -- which
;; is what this used to do -- is not a rougher version of the same answer, it
;; is a different one, and the difference is load-bearing. The containment
;; check every Java program writes,
;;
;;   (.startsWith (.getCanonicalPath child) (.getCanonicalPath root))
;;
;; then passes for a symlink inside root pointing anywhere at all, so a static
;; file server built on it serves whatever the link names. ring.middleware.file
;; is written exactly that way.
(define c-realpath (jolt-foreign-proc-safe "realpath" '(string u8*) 'iptr))

(define (jfile-cstr buf)                        ; buf up to the first NUL, as a string
  (let loop ((i 0))
    (cond ((>= i (bytevector-length buf)) (utf8->string buf))
          ((= 0 (bytevector-u8-ref buf i))
           (let ((bv (make-bytevector i)))
             (do ((j 0 (+ j 1))) ((= j i) (utf8->string bv))
               (bytevector-u8-set! bv j (bytevector-u8-ref buf j)))))
          (else (loop (+ i 1))))))

;; #f when the path does not exist (realpath fails ENOENT) or the host has no
;; realpath at all -- a Windows build, where the callers below fall back to
;; lexical folding, which is what this file could do before.
(define (jfile-realpath p)
  (and c-realpath
       (let ((buf (make-bytevector 4096 0)))     ; >= PATH_MAX
         (and (not (= 0 (c-realpath p buf))) (jfile-cstr buf)))))

;; "/a/b" -> "/a", "/a" -> "/", "/" -> #f
(define (path-parent p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) #f)
          ((char=? (string-ref p i) #\/) (if (= i 0) "/" (substring p 0 i)))
          (else (loop (- i 1))))))

;; Fold "." and ".." lexically. Only ever applied to a part of a path that does
;; NOT exist: where a component is real, realpath resolves it instead, because
;; POSIX (and the JVM) resolve ".." AFTER following the link before it, and
;; folding it lexically there would give a different -- wrong -- directory.
(define (jfile-fold-dots p)
  (let loop ((segs (let split ((i 0) (start 0) (acc (quote ())))
                     (cond ((= i (string-length p))
                            (reverse (cons (substring p start i) acc)))
                           ((char=? (string-ref p i) #\/)
                            (split (+ i 1) (+ i 1) (cons (substring p start i) acc)))
                           (else (split (+ i 1) start acc)))))
             (out (quote ())))
    (cond
      ((null? segs)
       (if (null? out)
           "/"
           (apply string-append (map (lambda (s) (string-append "/" s)) (reverse out)))))
      ((or (string=? (car segs) "") (string=? (car segs) "."))
       (loop (cdr segs) out))
      ((string=? (car segs) "..")
       (loop (cdr segs) (if (null? out) out (cdr out))))
      (else (loop (cdr segs) (cons (car segs) out))))))

(define (path-join base segs)
  (if (null? segs)
      base
      (path-join (if (string=? base "/")
                     (string-append "/" (car segs))
                     (string-append base "/" (car segs)))
                 (cdr segs))))

;; The JVM canonicalizes a path whose tail does not exist -- on a host where
;; /tmp is a link, new File("/tmp/nope").getCanonicalPath is
;; "/private/tmp/nope" -- while realpath(3) fails outright on ENOENT. So
;; resolve the longest existing ancestor and re-attach what is left.
(define (jfile-canonical p)
  (let ((abs (jfile-abs p)))
    (or (jfile-realpath abs)
        (let loop ((dir (path-parent abs)) (tail (list (path-last-segment abs))))
          (cond
            ((not dir) (jfile-fold-dots abs))
            ((jfile-realpath dir)
             => (lambda (rp) (jfile-fold-dots (path-join rp tail))))
            (else (loop (path-parent dir) (cons (path-last-segment dir) tail))))))))

;; --- file metadata over Chez filesystem ops ---------------------------------
;; byte size of a regular file (0 for a directory or a missing file).
(define (file-byte-size p)
  (if (or (not (file-exists? p)) (file-directory? p)) 0
      (let ((port (open-file-input-port p))) (let ((n (file-length port))) (close-port port) n))))
;; last-modified as epoch milliseconds (0 if the file is absent).
(define (file-mtime-millis p)
  (if (file-exists? p) (sa-file-mtime-ms p) 0))

;; access(2): may the EFFECTIVE user read / write / execute this path? This is
;; the question File.canRead/canWrite/canExecute and Files.isReadable/isWritable/
;; isExecutable ask on the JVM. All six used to answer (file-exists? p) instead,
;; which reports a read-only file as writable and every regular file as
;; executable — so a caller testing writability before a write took the wrong
;; branch and found out at the open, and babashka.fs/writable? (which routes to
;; Files/isWritable) inherited it. ONE predicate for all six: two hand-kept
;; copies is how java.io and java.nio.file start disagreeing about a path.
;;
;; Resolved through jolt-foreign-proc-safe like utimes above — a literal
;; foreign-procedure is a fasl relocation that aborts the boot where the symbol
;; is absent. Windows' CRT spells it _access and has no X_OK: mode 1 is EINVAL
;; there, so an execute test falls back to existence.
(define c-access (or (jolt-foreign-proc-safe "access" '(string int) 'int)
                     (jolt-foreign-proc-safe "_access" '(string int) 'int)))
(define access-r-ok 4)
(define access-w-ok 2)
(define access-x-ok 1)
(define (file-accessible? p mode)
  (if (and c-access
           (not (and (fx=? mode access-x-ok) (eq? (sa-os-family) 'windows))))
      (= (c-access p mode) 0)
      ;; no access(2) to ask (or X_OK on Windows): the old answer, existence.
      (if (file-exists? p) #t #f)))
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
        (cons "openStream"     (lambda (self) (url-open-stream self)))))
;; openStream hands back an InputStream, like the JVM (a file: URL there is a
;; FileInputStream behind a BufferedInputStream). It used to answer a StringReader
;; -- content-correct, but the wrong half of the io hierarchy, so the documented
;; composition (InputStreamReader. (.openStream u)) could not work: an ISR drives
;; its argument's read(byte[],int,int), and a Reader answers that by writing
;; CHARACTERS into the byte array. typedclojure reads its config through exactly
;; that chain and the failure surfaced from tools.reader as "#\{ is not a number".
(define (url-open-stream u)
  (let ((spec (url-spec u)))
    (cond
      ;; a stream handler decides what this URL means, whatever its protocol
      ((url-handler u)
       (record-method-dispatch (url-open-connection u) "getInputStream" jolt-nil))
      ;; FileInputStream resolves a relative path against user.dir and raises
      ;; java.io.FileNotFoundException for a missing one, both like the JVM.
      ((string=? (url-protocol spec) "file")
       (host-new "FileInputStream" (url-strip-scheme spec)))
      (else (throw-jvm (quote java.io.IOException)
                       (string-append "protocol doesn't support input: " spec))))))
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

;; File.getParent()/getParentFile(): the prefix up to the last separator, or nil
;; when the path names no parent. The JVM's no-parent set is wider than "no
;; separator in the path" -- the root is its own longest prefix, so "/" answers
;; null there while the scan below finds "/" and hands the path straight back.
;; Comparing the result against the input is what turns that into nil, and it
;; costs one string=? to cover the case without naming "/" anywhere: any path
;; whose parent would be itself has no parent, whatever normalization does next.
;; The loop never terminating is the visible failure -- (.getParentFile d) in a
;; walk-to-root recur is a tail call, so a parent that answers itself spins with
;; no stack growth and no exception.
(define (jfile-parent-path p)        ; -> the parent path, or #f when there is none
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) #f)
          ((char=? (string-ref p i) #\/)
           (let ((parent (if (= i 0) "/" (substring p 0 i))))
             (and (not (string=? parent p)) parent)))
          (else (loop (- i 1))))))

;; File.list()/File.listFiles(): the JVM answers null -- not an empty array, and
;; not a throw -- for a path that is not a readable directory, so the ordinary
;; (map str (.listFiles f)) over a missing path or a plain file yields () rather
;; than dying. file-directory? covers both of those; the guard covers a directory
;; the process may not read, which is an I/O error and null on the JVM too. Both
;; spellings go through here so they cannot drift apart again.
(define (jfile-listing fp produce)   ; -> the listing, or jolt-nil
  (if (file-directory? fp)
      (guard (e (#t jolt-nil)) (produce))
      jolt-nil))

;; --- File method surface (record-method-dispatch arm) -----------------------
(define (jfile-method f name args)        ; -> boxed result, or #f to fall through
  (let ((p (jfile-path f))               ; the path as given (display methods)
        (fp (jfile-fs f)))               ; JOLT_PWD-resolved on-disk path (FS methods)
    (cond
      ((string=? name "getPath")        (list p))
      ((string=? name "getName")        (list (path-last-segment p)))
      ((string=? name "toString")       (list p))
      ((string=? name "getAbsolutePath")(list (jfile-abs fp)))
      ((string=? name "getCanonicalPath")(list (jfile-canonical fp)))
      ;; File.toURI returns a java.net.URI (JVM), not a String.
      ((string=? name "toURI")          (list (uri-parse (string-append "file:" (jfile-abs fp)))))
      ((string=? name "toURL")          (list (make-url (string-append "file:" (jfile-abs fp)))))
      ((string=? name "exists")         (list (if (file-exists? fp) #t #f)))
      ((string=? name "isDirectory")    (list (if (file-directory? fp) #t #f)))
      ((string=? name "isFile")         (list (if (and (file-exists? fp) (not (file-directory? fp))) #t #f)))
      ((string=? name "isAbsolute")     (list (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)) #t #f)))
      ;; listFiles builds each child from the path AS GIVEN (new File(this, name)
      ;; on the JVM), so a File made from a relative path lists relative children.
      ((string=? name "listFiles")
       (list (jfile-listing fp (lambda () (list->cseq (map make-jfile (jolt-list-dir p)))))))
      ;; .list -> the child NAMES (a String[]), nil if not a readable directory.
      ((string=? name "list")
       (list (jfile-listing fp (lambda () (apply jolt-vector (sort string<? (directory-list fp)))))))
      ((string=? name "length")         (list (->num (file-byte-size fp))))
      ((string=? name "lastModified")   (list (->num (file-mtime-millis fp))))
      ((string=? name "canRead")        (list (file-accessible? fp access-r-ok)))
      ((string=? name "canWrite")       (list (file-accessible? fp access-w-ok)))
      ((string=? name "canExecute")     (list (file-accessible? fp access-x-ok)))
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
       (list (let ((parent (jfile-parent-path p)))
               (if parent (make-jfile parent) jolt-nil))))
      ((string=? name "toPath")           (list (make-nio-path p)))  ; -> java.nio.file.Path (nio-file.ss)
      ((string=? name "getAbsoluteFile")  (list (make-jfile (jfile-abs fp))))
      ((string=? name "getCanonicalFile") (list (make-jfile (jfile-canonical fp))))
      ((string=? name "compareTo")      (list (->num (let ((o (file-path-of (car args))))
                                                       (cond ((string<? p o) -1) ((string>? p o) 1) (else 0))))))
      ((string=? name "equals")         (list (and (jfile? (car args)) (string=? p (jfile-path (car args))))))
      ((string=? name "hashCode")       (list (->num (string-hash p))))
      ((string=? name "getParent")
       (list (or (jfile-parent-path p) jolt-nil)))
      (else #f))))

(register-method-arm! arm-priority-file
  (lambda (obj method-name rest-args)
    (if (jfile? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (jfile-method obj method-name rest)))
          (if r (car r) (dispatch-miss obj method-name rest)))
        'pass)))
;; An embedded resource shares the tier: io/resource returns one of these where a
;; source root would have yielded a jfile, so it has to answer the same methods.
(register-method-arm! arm-priority-file
  (lambda (obj method-name rest-args)
    (if (embedded-res? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (embedded-res-method obj method-name rest)))
          (if r (car r) (dispatch-miss obj method-name rest)))
        'pass)))
(register-class-arm! embedded-res? (lambda (x) "java.net.URL"))
;; (str resource) is the resource name, like URL.toString — which also gives the
;; printer's #object[…] fallback its content.
(register-str-render! embedded-res? (lambda (x) (embedded-res-name x)))

;; File methods emitted via jolt-host-call (rt.ss) need jfile dispatch, not the
;; string-path shims in the base jolt-host-call. Route through
;; record-method-dispatch — the same entry point every other (.method file)
;; call takes — so the two spellings cannot answer differently. Calling
;; jfile-method here directly was that second answer: it skipped the arm chain,
;; so a library override of File/isDirectory registered through
;; jolt.host/extend-class! applied everywhere EXCEPT the file-seq call sites the
;; backend lowers to jolt-host-call.
(define %io-host-call jolt-host-call)
(set! jolt-host-call
  (lambda (method target . args)
    (if (jfile? target)
        (record-method-dispatch target method (apply jolt-vector args))
        (apply %io-host-call method target args))))

;; --- the files a load READ ---------------------------------------------------
;; Every file the io layer opens on behalf of USER code announces itself here.
;;
;; The AOT cache (loader.ss) binds the sink while it compiles a namespace: a
;; macro that slurps an external file bakes that file's CONTENTS into the
;; artifact exactly the way it bakes a macro expansion, so the file belongs in the
;; cache key. Keying on the .clj alone left an edited SQL migration serving the
;; previous build's statements out of the cache, silently — the namespace source
;; is untouched, so its hash still matches (jolt#576).
;;
;; A file that does NOT exist is announced too. (io/resource "migrations/003.sql")
;; answering nil is a compile-time answer like any other, and it stops being the
;; right one the moment someone adds the file.
;;
;; It lives here rather than beside the cache because rt.ss is loaded in contexts
;; that never load loader.ss (bootstrap, devboot, the .ss unit tests), and the io
;; layer must not reference a name those don't have. Unbound (#f) in every
;; ordinary run — the cost off the compile path is one thread-parameter read.
(define io-file-read-sink (make-thread-parameter #f))
(define (io-note-file-read! path)
  (let ((sink (io-file-read-sink)))
    (when (and (vector? sink) (string? path)
               (not (member path (vector-ref sink 0))))
      (vector-set! sink 0 (cons path (vector-ref sink 0))))))

;; --- slurp / spit / flush ---------------------------------------------------
;; NOT announced: the loader reads namespace SOURCE through this too, and those
;; are described by the cache key already. slurp-path / io/resource / io/reader —
;; the entry points user code reaches — announce for themselves.
(define (read-file-string path)
  (with-port (open-input-file path)
    (lambda (p) (let ((s (get-string-all p))) (if (eof-object? s) "" s)))))

;; Drain a jhost reader (StringReader / PushbackReader): read code units from the
;; current position to EOF (-1) and assemble the string. Used by slurp; advances
;; the reader, as on the JVM.
;;
;; The generic way to do that is one record-method-dispatch per CODE UNIT, which
;; is a quarter-million dispatches for a 250KB source — and `read` over a host
;; reader drains, parses one form and pushes the tail back, so a caller reading a
;; file form by form pays that per FORM. Reading clojure/core.clj through
;; (read {:eof …} rdr) took 37s that way, against the JVM's 0.06s. These readers
;; are string-backed, so take the remaining text in one substring when the shape
;; allows and keep the dispatch loop for everything else (a char-reader over a
;; Chez port, a library's own reader shim).
(define (string-reader-jhost? x)
  (and (jhost? x) (string=? (jhost-tag x) "string-reader")))

;; the code units already pushed back, in the order a read would hand them out
(define (pbr-pushed-string r)
  (let loop ((ps (vector-ref (jhost-state r) 1)) (acc '()))
    (if (null? ps)
        (list->string (reverse acc))
        (loop (cdr ps) (cons (integer->char (jnum->exact (car ps))) acc)))))

;; A line-numbering reader folds \r\n and a lone \r to one \n and counts a line
;; for each, one character at a time (pbr-read-translated). A bulk drain has to
;; leave exactly the state that loop would have: same text, same line/column, and
;; the same "a \n right after this \r is already counted" flag.
(define (pbr-fold-and-count! st s)
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()) (line (vector-ref st 3)) (col (vector-ref st 4))
               (skip-lf (vector-ref st 5)))
      (if (fx>=? i n)
          (begin (vector-set! st 3 line) (vector-set! st 4 col) (vector-set! st 5 skip-lf)
                 (list->string (reverse acc)))
          (let ((c (string-ref s i)))
            (cond
              ((and skip-lf (char=? c #\newline)) (loop (fx+ i 1) acc line col #f))
              ((or (char=? c #\return) (char=? c #\newline))
               (loop (fx+ i 1) (cons #\newline acc) (fx+ line 1) 0 (char=? c #\return)))
              (else (loop (fx+ i 1) (cons c acc) line (fx+ col 1) #f))))))))

(define (drain-reader-by-dispatch r)
  (let loop ((acc '()))
    (let ((u (record-method-dispatch r "read" jolt-nil)))
      (if (or (jolt-nil? u) (and (number? u) (< u 0)))
          (list->string (reverse acc))
          (loop (cons (integer->char (exact (truncate u))) acc))))))

(define (drain-reader r)
  (cond
    ;; a StringReader: the rest of its string, in one copy
    ((string-reader-jhost? r)
     (let* ((s (sr-s r)) (p (sr-pos r)) (n (string-length s)))
       (if (fx>=? p n) "" (begin (sr-pos! r n) (substring s p n)))))
    ;; a PushbackReader over one: the pushback buffer (which sits ABOVE the
    ;; translation, so it is handed back raw) then the wrapped reader's rest
    ((and (jhost? r) (pushback-reader-tag? (jhost-tag r))
          (string-reader-jhost? (vector-ref (jhost-state r) 0)))
     (let* ((st (jhost-state r))
            (pushed (pbr-pushed-string r))
            (rest (drain-reader (vector-ref st 0))))
       (vector-set! st 1 '())
       (string-append pushed (if (vector-ref st 2) (pbr-fold-and-count! st rest) rest))))
    (else (drain-reader-by-dispatch r))))

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
;; The StringReader a host reader ultimately reads out of, when it has one and
;; nothing sits between the caller and it: -> (values string-reader ln-state),
;; where ln-state is the pushback reader's own state vector for the line-numbering
;; subclass (whose counters a read has to advance) and #f otherwise. Characters
;; pushed back sit ABOVE the string and would be skipped by an index read, so a
;; non-empty pushback buffer declines — (values #f #f).
(define (host-reader-string-cursor r)
  (cond
    ((string-reader-jhost? r) (values r #f))
    ((and (jhost? r) (pushback-reader-tag? (jhost-tag r))
          (null? (vector-ref (jhost-state r) 1))
          (string-reader-jhost? (vector-ref (jhost-state r) 0)))
     (values (vector-ref (jhost-state r) 0)
             (and (vector-ref (jhost-state r) 2) (jhost-state r))))
    (else (values #f #f))))

;; Read ONE form from a host reader (StringReader/PushbackReader), advancing it
;; past exactly that form. -> (values form found?). (read r) over a java.io reader
;; — cuerdas' interpolation reads this way, and so does anything reading a source
;; file form by form.
;;
;; A string-backed reader parses AT its current index and moves the index; the
;; drain-parse-refill fallback below re-materializes the whole remaining input per
;; form, which is quadratic over a file. The fallback still covers a char-reader
;; over a Chez port, a library's own reader shim, and a reader with pushback.
(define (host-reader-read-form r)
  (let-values (((sr lnst) (host-reader-string-cursor r)))
    (if sr
        (let* ((s (sr-s sr)) (i (sr-pos sr)) (pr (rdr-parse-at s i)))
          (if (not pr)
              (begin (sr-pos! sr (string-length s)) (values jolt-nil #f))
              (let ((j (cdr pr)))
                ;; the line-numbering reader counts what a char-by-char read would
                ;; have counted over the span this form consumed
                (when lnst (pbr-fold-and-count! lnst (substring s i j)))
                (sr-pos! sr j)
                (values (car pr) #t))))
        (let* ((s (drain-reader r)) (pr (jolt-parse-next s)))
          (if (jolt-nil? pr)
              (begin (reader-refill! r "") (values jolt-nil #f))
              (begin (reader-refill! r (jolt-nth pr 1)) (values (jolt-nth pr 0) #t)))))))

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
  (io-note-file-read! path)
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
      ;; project-relative: a relative file: URL resolves against user.dir on the
      ;; JVM, where a bare path here would resolve against the process cwd -- the
      ;; jolt repo root under the launcher, not the project the user is in.
      ((string=? (url-protocol spec) "file")
       (slurp-path (project-relative (url-strip-scheme spec))))
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
;; slurp over a clojure.core/IReader (what *in* and with-in-str hand out). The
;; protocol is line-based — -read-line, -read-form, -read+string, no char read —
;; and -read-line drops the delimiter, so whether the input ended with a newline
;; is not recoverable here: "a\nb" and "a\nb\n" both drain to "a\nb". Reading
;; source text off a pipe, which is what this is for, does not care.
(define (drain-ireader src)
  (let ((out (open-output-string)))
    (let loop ((first? #t))
      (let ((line (record-method-dispatch src "-read-line" jolt-nil)))
        (if (jolt-nil? line)
            (get-output-string out)
            (begin
              (unless first? (put-char out #\newline))
              (put-string out line)
              (loop #f)))))))
(define (jolt-slurp src . opts)
  (cond
    ((jfile? src) (slurp-path (jfile-fs src)))
    ((embedded-res? src)
     (let ((c (embedded-res-content src)))
       (if (bytevector? c) (utf8->string c) c)))
    ((reader-jhost? src) (drain-reader src))
    ((and (reified-methods src)
          (hashtable-ref (reified-methods src) "-read-line" #f))
     (drain-ireader src))
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
  ;; Only a path, a File or a host stream names a target; anything else is the
  ;; coercion error io/writer raises. nil used to render as "" and write a temp
  ;; file into the working directory before failing to rename it.
  (unless (or (string? path) (jfile? path) (jhost? path))
    (throw-jvm (quote IllegalArgumentException)
               (string-append "Cannot open <" (jolt-pr-str path) "> as a Writer.")))
  (let* ((p (project-relative (file-path-of path)))
         (text (jolt-str-render-one content)))
    (if (spit-append? opts)
        (with-port (open-output-file p 'append)
          (lambda (port) (put-string port text)))
        (let ((tmp (string-append p ".spit-tmp-"
                                   (number->string (sa-real-time-ms)) "-"
                                   (number->string (jolt-with-mutex io-counter-mutex
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

;; The stdin line seam (__stdin-read-line, the *in* reader's source) lives in
;; io-streams.ss, next to the System/in stream it reads.

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
                         (text-sink-tag? (jhost-tag x))
                         (string=? (jhost-tag x) "string-reader")))
     (record-method-dispatch x "close" jolt-nil) jolt-nil)
    ;; a library's stream shim (tagged-table) closes via its registered .close
    ;; method (a no-op for in-memory streams); absent method -> no-op.
    ((htable? x) (guard (e (#t jolt-nil)) (record-method-dispatch x "close" jolt-nil)) jolt-nil)
    ((jfile? x) jolt-nil)
    ;; a deftype/defrecord/reify that implements a `close` method (java.io.Closeable
    ;; / AutoCloseable, e.g. tools.reader's reader types, or clojure.jdbc's
    ;; connection wrapper) closes through it — the same method (.close x) would
    ;; dispatch to. Ask iface-method rather than jrec-cl: that is the shared
    ;; deftype-or-reify lookup, and a reify is the other half of it, so one
    ;; implementing Closeable used to fall through to the error below even though
    ;; (.close x) on it worked.
    ((iface-method x "close" #f)
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
    ((jfile? x) (io-note-file-read! (jfile-fs x))
                (host-new "StringReader" (read-file-string (jfile-fs x))))
    ((embedded-res? x)
     (let ((c (embedded-res-content x)))
       (host-new "StringReader" (if (bytevector? c) (utf8->string c) c))))
    ((url-jhost? x) (host-new "StringReader" (url-content x)))
    ((string? x) (let ((p (project-relative x)))
                   (io-note-file-read! p)
                   (host-new "StringReader" (read-file-string p))))
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
;; io/file is NOT the File constructor. It puts every child through
;; as-relative-path, which throws on an absolute one, so (io/file "/a/b" "/c")
;; raises where (File. "/a/b" "/c") happily answers "/a/b/c" -- both checked
;; against the JVM. jolt registered io/file as jolt-make-file, which has no
;; notion of a child, so the absolute one was silently joined.
;;
;; Normalization alone would have HIDDEN this rather than fixed it: joining
;; "/a/b" and "/c" produces "/a/b//c", which now collapses to "/a/b/c" and looks
;; like a correct answer to a call the JVM rejects.
;; as-relative-path is Clojure's own coercion, and on the JVM it goes through
;; as-file FIRST: normalize(child) is what .isAbsolute sees, so the thrown
;; message names the normalized path -- (io/file "/a/b" "//c") says
;; "/c is not a relative path", not "//c". io-file-relative-child checked the
;; raw string, so the accept/reject set was right but the message diverged.
;; Registered as io/as-relative-path too: public API in clojure.java.io on
;; the JVM, and missing here entirely before.
(define (jolt-as-relative-path x)
  ;; as-file first, so nil coerces to nil and .isAbsolute raises on it rather
  ;; than the child being read as ""
  (when (jolt-nil? x)
    (throw-jvm (quote NullPointerException)
               "Cannot invoke \"java.io.File.isAbsolute()\" because \"f\" is null"))
  (let ((p (jfile-path (make-jfile (file-path-of x)))))
    (when (and (fx>? (string-length p) 0) (char=? (string-ref p 0) #\/))
      (throw-jvm (quote IllegalArgumentException)
                 (string-append p " is not a relative path")))
    p))
(def-var! "clojure.java.io" "as-relative-path" jolt-as-relative-path)
(define (jolt-io-file a . rest)
  (cond ((pair? rest) (apply jolt-make-file a (map jolt-as-relative-path rest)))
        ;; one-arg io/file IS as-file, and as-file of nil is nil
        ((jolt-nil? a) a)
        (else (jolt-make-file a))))
(def-var! "clojure.java.io" "file" jolt-io-file)
;; io/as-file of a file: URL yields the file it points at (JVM: new
;; File(url.toURI())); a URL with any other protocol has no filesystem path —
;; IllegalArgumentException, as the JVM's File(URI) throws.
(define (url-file-coercion u)
  (if (string=? (url-protocol (url-spec u)) "file")
      (make-jfile (url-strip-scheme (url-spec u)))
      (throw-jvm 'IllegalArgumentException (string-append "Not a file: " (url-spec u)))))
(def-var! "clojure.java.io" "as-file"
  ;; Clojure extends Coercions to nil, so (io/as-file nil) is nil -- NOT a File
  ;; whose path is "". The difference is load-bearing one call downstream, where
  ;; the JVM raises on the nil and jolt was quietly reading the process's cwd.
  (lambda (x) (cond ((jolt-nil? x) x)
                    ((jfile? x) x)
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

;; The name argument, or an NPE. The JVM throws NullPointerException for a null
;; resource name from ClassLoader.getResource / getResources /
;; getResourceAsStream and Class.getResource alike (probed directly), and
;; clojure.java.io/resource is a bare .getResource, so it throws too. jolt sent
;; the name through jolt-str-render-one, the `str` coercion, which renders nil as
;; "" — and "" is a DIFFERENT question with a real answer, since the empty name is
;; the classpath root. (io/resource nil) therefore handed back a URL for the first
;; source root: a caller whose name came from a missing config key or an absent
;; optional path got a directory, and only found out when something far away tried
;; to read it. "" itself keeps answering the root, which is what the JVM does with
;; it — verified, not assumed.
(define (resource-name-arg name)
  (if (jolt-nil? name)
      (throw-jvm (quote NullPointerException) "resource name is nil")
      (jolt-str-render-one name)))

;; This is THE resource resolver: clojure.java.io/resource and every ClassLoader
;; method in the java.lang.ClassLoader section below (getResource / getResources /
;; getResourceAsStream, on the loader and on a Class) answer through it, so all of
;; them see the embedded branch and announce the same candidates. They used to
;; walk the roots themselves, which silently made the loader the weaker resolver;
;; cl-get-resource carries what that cost.
;;
;; Every candidate probed is announced to the AOT cache (io-note-file-read!),
;; not just the one that answered — including the ones that were not there. Which
;; root wins is part of the answer, so a file appearing at an EARLIER root has to
;; invalidate; and a lookup that found nothing at all has to invalidate when the
;; resource is finally added (a new migration is exactly that). An embedded
;; resource is baked into the binary and covered by the runtime fingerprint, so it
;; contributes nothing here.
(define (resolve-resource name)
  (let* ((nm (resource-name-arg name))
         (emb (hashtable-ref embedded-resources nm #f)))
    (if emb (make-embedded-res nm emb)
        (let loop ((roots (get-source-roots)))
          (if (null? roots)
              jolt-nil
              (let ((cand (string-append (car roots) "/" nm)))
                (io-note-file-read! cand)
                (if (file-exists? cand)
                    (resource-file-url (car roots) nm)
                    (loop (cdr roots)))))))))

;; (resource n) and (resource n loader). The JVM's 2-arity resolves against the
;; ClassLoader it is handed; jolt has a single "classloader" that resolves through
;; resolve-resource just as this does (see the java.lang.ClassLoader section
;; below), so every loader resolves the same resources and the argument is
;; accepted and ignored. Libraries pass it to pin resolution to one loader
;; across threads — cognitect aws-api's `cognitect.aws.resources/resource` is
;; (io/resource n (RT/baseLoader)) — and without the arity they fail to load at
;; all rather than degrading. case-lambda rather than a rest argument so the JVM's
;; two arities are the only two: (resource n loader extra) is an arity error there
;; and has to stay one here.
(define jolt-io-resource
  (case-lambda
    ((name) (resolve-resource name))
    ((name _loader) (resolve-resource name))))
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
;; Straight through to io/resource's resolver. This walked the roots itself until
;; it was found to be resolving LESS than io/resource did, in two ways that both
;; only bite where they are hardest to see:
;;
;;   - it never consulted embedded-resources, so in a `jolt build` binary with
;;     :jolt/build :embed a baked-in resource answered nil here while
;;     (io/resource n) served it. Every classpath-probing library that goes
;;     through a loader rather than io/resource — .getResourceAsStream on
;;     RT/baseLoader is the common spelling — therefore saw nothing in the built
;;     artifact and everything in the source tree it was developed against.
;;   - it announced no candidate to the AOT cache, so a compile-time lookup
;;     through a loader was not part of the cache key: exactly the staleness
;;     jolt#576 fixed for io/resource, still live on this path.
(define (cl-get-resource self name) (resolve-resource name))
;; getResources: every source root that holds the named resource, as file: URLs
;; (enumeration-seq just calls seq, so a list serves). ring's static-resource
;; symlink check enumerates these to confirm a served file sits under a root.
;; An embedded hit leads, matching the precedence the singular resolver gives it,
;; and every candidate is announced for the reason resolve-resource announces.
(define (cl-get-resources self name)
  (let* ((nm (resource-name-arg name))
         (emb (hashtable-ref embedded-resources nm #f)))
    (let loop ((roots (get-source-roots))
               (acc (if emb (list (make-embedded-res nm emb)) '())))
      (cond ((null? roots) (list->cseq (reverse acc)))
            (else
             (let ((cand (string-append (car roots) "/" nm)))
               (io-note-file-read! cand)
               (if (file-exists? cand)
                   (loop (cdr roots) (cons (resource-file-url (car roots) nm) acc))
                   (loop (cdr roots) acc))))))))
;; The stream for whatever the resolver answered. Both branches of a resolved
;; resource are java.net.URLs with an openStream — a file: URL reads its target,
;; an embedded-res hands back its baked content — so dispatching the method is
;; what makes an embedded hit readable. Stripping the scheme and slurping the
;; path, which is what this did, only ever worked for the file: branch.
(define (cl-resource-stream self name)
  (let ((u (cl-get-resource self name)))
    (if (jolt-nil? u) jolt-nil (record-method-dispatch u "openStream" jolt-nil))))
(register-host-methods! "classloader"
  (list (cons "getResource" cl-get-resource)
        (cons "getResources" cl-get-resources)
        ;; jolt has a single loader, so it has no parent — the same answer the
        ;; JVM's bootstrap loader gives, which terminates the usual
        ;; (take-while identity (iterate #(.getParent %) loader)) walk.
        (cons "getParent" (lambda (self) jolt-nil))
        (cons "getResourceAsStream" cl-resource-stream)))
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
                                 (class-resource-name (jclass-name self) (resource-name-arg name)))))
        (cons "getResourceAsStream"
              (lambda (self name)
                (cl-resource-stream the-classloader
                                    (class-resource-name (jclass-name self) (resource-name-arg name)))))))
;; clojure.lang.RT/nextID — process-unique increasing id (AtomicInteger(1)
;; getAndIncrement), used by id generators such as core.logic's lvar.
(define rt-next-id-counter 1)
(define (rt-next-id)
  (jolt-with-mutex io-counter-mutex
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
;; A handle STANDS FOR one thread, and every question asked through it is about
;; that thread — including when some other thread is holding it, which is the
;; only shape Thread/getAllStackTraces hands back. So the id travels IN the
;; handle: reading (get-thread-id) here answered about whoever was asking, so
;; every entry in that map reported the caller's id and its name was the constant
;; "main". State is (interrupt-box . thread-id).
(define (thread-handle-box h) (car (jhost-state h)))
(define (thread-handle-id h) (cdr (jhost-state h)))
;; Names live in an id-keyed table for the same reason, under the handle mutex:
;; a thread parameter is only readable by its own thread. A thread nobody named
;; answers the JVM's default shape — the boot thread is "main", anything else
;; "Thread-<id>".
(define thread-names-by-id (make-eqv-hashtable))
(define (jolt-thread-name-set! id nm)
  (jolt-with-mutex thread-handles-mutex (hashtable-set! thread-names-by-id id nm)))
(define (jolt-thread-name id)
  (or (jolt-with-mutex thread-handles-mutex (hashtable-ref thread-names-by-id id #f))
      (if (eqv? id jolt-boot-thread-id)
          "main"
          (string-append "Thread-" (number->string id)))))
(register-host-methods! "thread"
  (list (cons "getContextClassLoader" (lambda (self) the-classloader))
        (cons "getName" (lambda (self) (jolt-thread-name (thread-handle-id self))))
        (cons "setName" (lambda (self nm)
                          (jolt-thread-name-set! (thread-handle-id self) (jolt-final-str nm))
                          jolt-nil))
        (cons "getId" (lambda (self) (thread-handle-id self)))
        ;; no reified call stack (jolt does TCO, so caller frames are erased) — an
        ;; empty StackTraceElement[]. clojure.spec.test.alpha's instrument reads it
        ;; to name the caller var; it degrades to no ::caller, the conform error
        ;; (the ExceptionInfo) is still thrown.
        (cons "getStackTrace" (lambda (self) (jolt-vector)))
        ;; The flag first, then the poke: a waiter woken by the poke reads the
        ;; flag, so a wake that arrives before it is set says nothing. Waking is
        ;; what turns .interrupt from "the target will notice next time it looks"
        ;; into the JVM's "the target is thrown out of its wait now"
        ;; (jolt-cv-wait-interruptibly, host/chez/locks.ss).
        (cons "interrupt" (lambda (self)
                            (let ((b (thread-handle-box self)))
                              (when (box? b)
                                (set-box! b #t)
                                (jolt-interrupt-wake-waits! b)))
                            jolt-nil))
        (cons "isInterrupted" (lambda (self)
                                (let ((b (thread-handle-box self)))
                                  (and (box? b) (unbox b) #t))))))
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
        (let ((h (make-jhost "thread" (cons (current-interrupt-box) id))))
          (thread-handle-cell (cons id h))
          (jolt-with-mutex thread-handles-mutex (hashtable-set! thread-handles-by-id id h))
          h))))
;; A handle for a thread that has never asked who it is. Its interrupt box is its
;; own, so .interrupt through it does not reach that thread — the thread adopts a
;; real handle the moment it calls currentThread.
(define (thread-handle-for-id id)
  (if (eqv? id (get-thread-id))
      (current-thread-handle)              ; the caller must find ITSELF in the map
      (or (jolt-with-mutex thread-handles-mutex (hashtable-ref thread-handles-by-id id #f))
          (let ((h (make-jhost "thread" (cons (box #f) id))))
            (jolt-with-mutex thread-handles-mutex (hashtable-set! thread-handles-by-id id h))
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
;; (java.io.File. parent child) answers resolve(normalize(parent),
;; normalize(child)) -- it normalizes each ARGUMENT, and then:
;;
;;   resolve(p, c) = p              when c is "" or "/"
;;                 = c              when c is absolute and p is "/"
;;                 = p + c          when c is absolute
;;                 = p + c          when p is "/"
;;                 = p + "/" + c    otherwise
;;
;; So a parent that already ends in "/" does not produce a doubled slash (ring's
;; resource middleware builds "assets/" + "index.html"), a duplicate INSIDE
;; either argument collapses, and a separator-only child yields the parent alone.
;;
;; A null parent is the child by itself. An EMPTY parent is not: it resolves
;; against getDefaultParent(), which is "/" -- new File("", "c") is "/c", not
;; "c", and new File("", "") is "/", not "". Both measured against the JVM.
;;
;; Worth knowing before measuring this yourself: resolve grew its c == "/" case
;; in JDK 21. Through JDK 20, new File("/a/b", "/") answered "/a/b/" -- a path
;; carrying a trailing separator no one-argument constructor can produce, whose
;; .getName() was "". From 21 on it is "/a/b", which is what this matches.
;;
;; A null CHILD is not a null parent: the constructor null-checks it up front and
;; throws, message and all -- new File("/a", null) raises NPE rather than
;; answering "/a". jolt read it as "" and quietly answered the parent, the same
;; silently-wrong-file shape as the nil coercions above.
(define (jolt-file-join parent child)
  (when (jolt-nil? child) (throw-jvm (quote NullPointerException) jolt-nil))
  (let ((c (jolt-path-normalize (file-path-of child))))
    (if (jolt-nil? parent)
        c
        (let* ((p (jolt-path-normalize (file-path-of parent)))
               (p (if (string=? p "") "/" p)))
          (cond ((or (string=? c "") (string=? c "/")) p)
                ((char=? (string-ref c 0) #\/)
                 (if (string=? p "/") c (string-append p c)))
                ((string=? p "/") (string-append p c))
                (else (string-append p "/" c)))))))
;; new File((String)null) throws too, with a null message of its own. Only the
;; two-arg form takes a null parent, and there it means "the child alone".
(define (jolt-file-ctor a . rest)
  (cond ((pair? rest) (jolt-make-file (jolt-file-join a (car rest))))
        ((jolt-nil? a) (throw-jvm (quote NullPointerException) jolt-nil))
        (else (jolt-make-file a))))
(register-class-ctor! "File" jolt-file-ctor)
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
    (let ((n (jolt-with-mutex io-counter-mutex
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
(register-class-ctor! "java.io.File" jolt-file-ctor)
;; java.nio.charset.StandardCharsets: the constants ARE the charset names —
;; every jolt charset seam (.getBytes, String ctors, InputStreamReader) takes
;; the name string, so the constant composes with all of them (clj-uuid's v3/v5
;; digest .getBytes with StandardCharsets/UTF_8).
(register-class-statics! "java.nio.charset.StandardCharsets"
  (list (cons "UTF_8" "UTF-8") (cons "US_ASCII" "US-ASCII")
        (cons "ISO_8859_1" "ISO-8859-1") (cons "UTF_16" "UTF-16")
        (cons "UTF_16BE" "UTF-16BE") (cons "UTF_16LE" "UTF-16LE")))
;; UUID: randomUUID / fromString statics + a (UUID. s) string ctor. Registering
;; under the FQN also registers the short name (shared member table).
;;
;; fromString is the JVM's lenient 5-component parse: the canonical 36-char
;; shape takes the fast path; otherwise exactly five dash-separated hex groups,
;; each masked into its slot, so short groups zero-pad ((UUID/fromString
;; "1-1-1-1-1") is legal) and an overlong group drops its high bits. Anything
;; else throws IllegalArgumentException — returning nil here sent library code
;; down a wrong branch silently. parse-uuid stays the nil-returning Clojure
;; surface; only the java.util.UUID spellings throw.
(define (uuid-split-dashes s)
  (let ((len (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((fx=? i len) (reverse (cons (substring s start len) acc)))
            ((char=? (string-ref s i) #\-)
             (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) acc)))
            (else (loop (fx+ i 1) start acc))))))
(define (uuid-from-string-jvm s0)
  (let ((s (jolt-str-render-one s0)))
    (define (bad!)
      (throw-jvm (quote IllegalArgumentException) (string-append "Invalid UUID string: " s)))
    (define (part-u p)          ; unsigned hex value; JVM parses each group as a signed long
      (let ((n (string-length p)))
        (when (or (fx=? n 0) (fx>? n 16)) (bad!))
        (let loop ((i 0) (acc 0))
          (if (fx=? i n)
              (if (> acc #x7FFFFFFFFFFFFFFF) (bad!) acc)
              (let ((c (string-ref p i)))
                (if (hex-char? c)
                    (loop (fx+ i 1) (+ (* acc 16) (uuid-hexv (char-downcase c))))
                    (bad!)))))))
    (if (uuid-shape? s)
        (make-juuid (string-downcase s))
        (let ((parts (uuid-split-dashes s)))
          (if (not (= (length parts) 5))
              (bad!)
              (let ((p0 (part-u (car parts)))    (p1 (part-u (cadr parts)))
                    (p2 (part-u (caddr parts)))  (p3 (part-u (cadddr parts)))
                    (p4 (part-u (car (cddddr parts)))))
                (uuid-from-halves
                 (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and p0 #xFFFFFFFF) 32)
                              (bitwise-arithmetic-shift-left (bitwise-and p1 #xFFFF) 16)
                              (bitwise-and p2 #xFFFF))
                 (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and p3 #xFFFF) 48)
                              (bitwise-and p4 #xFFFFFFFFFFFF)))))))))
(register-class-statics! "java.util.UUID"
  (list (cons "randomUUID" (lambda () (jolt-random-uuid)))
        (cons "fromString" uuid-from-string-jvm)))
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
      (uuid-from-string-jvm (car args))))
(register-class-ctor! "UUID" uuid-ctor)
(register-class-ctor! "java.util.UUID" uuid-ctor)
;; a uuid's java.util.UUID method surface (record-method-dispatch arm; shares
;; the date tier — disjoint receiver types). The bit accessors answer SIGNED
;; longs (natives-misc.ss); timestamp/clockSequence/node are v1-only, like the
;; JVM. Unknown names 'pass so the base still answers toString/equals/getClass.
(define (uuid-version-of u) (uuid-hexv (string-ref (juuid-s u) 14)))
(define (uuid-method u m args)
  (define (need-v1!)
    (unless (= 1 (uuid-version-of u))
      (throw-jvm (quote UnsupportedOperationException) "Not a time-based UUID")))
  (cond
    ((string=? m "getMostSignificantBits") (uuid-u64->s64 (uuid-msb-u u)))
    ((string=? m "getLeastSignificantBits") (uuid-u64->s64 (uuid-lsb-u u)))
    ((string=? m "version") (uuid-version-of u))
    ((string=? m "variant")
     ;; top 3 bits of the lsb: 0xx -> 0 (NCS), 10x -> 2 (RFC 4122), 110 -> 6
     ;; (Microsoft), 111 -> 7 (reserved) — UUID.variant's decoding.
     (let ((top (fxarithmetic-shift-right (uuid-hexv (string-ref (juuid-s u) 19)) 1)))
       (cond ((fx<? top 4) 0) ((fx<? top 6) 2) ((fx=? top 6) 6) (else 7))))
    ((string=? m "timestamp")
     (need-v1!)
     (let ((msb (uuid-msb-u u)))
       (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and msb #xFFF) 48)
                    (bitwise-arithmetic-shift-left
                     (bitwise-and (bitwise-arithmetic-shift-right msb 16) #xFFFF) 32)
                    (bitwise-arithmetic-shift-right msb 32))))
    ((string=? m "clockSequence")
     (need-v1!)
     (bitwise-and (bitwise-arithmetic-shift-right (uuid-lsb-u u) 48) #x3FFF))
    ((string=? m "node")
     (need-v1!)
     (bitwise-and (uuid-lsb-u u) #xFFFFFFFFFFFF))
    ((string=? m "compareTo")
     (let ((o (if (pair? args) (car args) jolt-nil)))
       (if (juuid? o)
           (uuid-cmp u o)
           (throw-jvm (quote ClassCastException)
                      (string-append (jolt-final-str o) " cannot be cast to java.util.UUID")))))
    ((string=? m "hashCode")
     ;; (int)(hilo >> 32) ^ (int)hilo where hilo = msb ^ lsb — the JVM fold.
     (let* ((hilo (bitwise-xor (uuid-msb-u u) (uuid-lsb-u u)))
            (x (bitwise-xor (bitwise-arithmetic-shift-right hilo 32)
                            (bitwise-and hilo #xFFFFFFFF))))
       (if (>= x #x80000000) (- x #x100000000) x)))
    (else 'pass)))
(register-method-arm! arm-priority-date
  (lambda (obj method-name rest-args)
    (if (juuid? obj)
        (uuid-method obj method-name
                     (if (jolt-nil? rest-args) '() (seq->list rest-args)))
        'pass)))
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
