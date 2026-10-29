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
;; Scanning starts after the ROOT, whose separators are structural rather than
;; redundant: "//srv/sh" is a UNC root and collapsing its leading pair to
;; "/srv/sh" names a directory on the current drive instead. POSIX has no root
;; to protect (FROM is 0 there), so "//a/b" still folds to "/a/b" as the JVM
;; does — the row win-path-test.ss already pins.
;;
;; With NUL? the same pass also looks for a NUL anywhere in p and answers 'nul
;; for one, so a java.nio Path (which refuses a NUL as it parses) pays no second
;; scan for it; without, it stops at the first doubled separator. The walk is
;; unchecked: p is a string and i stays below its length.
(define (path-has-double-sep? p n from nul?)
  (let ((lo (fxmax 1 from)))
    (let loop ((i 0) (dbl #f) (prev-slash? #f))
      (if (sa-ufx<? i n)
          (let ((c (sa-ustring-ref p i)))
            (cond ((and nul? (eq? c #\nul)) 'nul)
                  ((and (eq? c #\/) prev-slash? (sa-ufx>=? i lo))
                   (if nul? (loop (sa-ufx+ i 1) #t #t) #t))
                  (else (loop (sa-ufx+ i 1) dbl (eq? c #\/)))))
          dbl))))

;; May the trailing separator at index N-1 be dropped? Not when it IS the root:
;; "/" is a path rather than an empty one, and on Windows so is the drive root
;; "C:/" — trimming that to "C:" names the drive's current directory instead,
;; which is a different file. File/listRoots is what found this: it builds its
;; roots through here, so every root it answered came back drive-RELATIVE
;; (jolt-lang/jolt#1074).
;;
;; POSIX is decided by the length test alone — the only POSIX path whose
;; trailing separator is its root is "/" itself — so the root scan runs on
;; Windows only, and this stays allocation-free on the hot path (a jfile is
;; built per entry on every directory listing).
(define (trailing-sep-droppable-for? windows? p n)
  (and (fx>? n 1)
       (char=? (string-ref p (fx- n 1)) #\/)
       (or (not windows?)
           (fx>=? (fx- n 1) (path-root-end #t p)))))

;; Windows spells a separator either way, and a File or Path holds ONE spelling:
;; "/", the one every scan in this file and nio-file.ss reads. What the caller
;; SEES is the native "\" — path-native below renders it at the display boundary
;; (str, toString, getPath, getAbsolutePath, getCanonicalPath, getParent, and the
;; path in an exception message), as WinNTFileSystem and WindowsPathParser do by
;; normalizing to "\" at construction. Keeping the held spelling "/" is what lets
;; the ~60 separator scans between here and the Path shim stay as they are; the
;; alternative, holding "\", would have to teach every one of them both.
(define (path-backslashes->slashes p)
  (if (let loop ((i 0)) (and (fx<? i (string-length p))
                             (or (char=? (string-ref p i) #\\) (loop (fx+ i 1)))))
      (list->string (map (lambda (c) (if (char=? c #\\) #\/ c)) (string->list p)))
      p))
;; The spelling a caller sees: "\" for "/" on Windows, the held path on POSIX.
;; A File or Path renders through this and nothing else, so File/separator, the
;; file.separator property and every rendered path agree — the one thing the JDK
;; guarantees about them, and why File/separator could not be flipped alone
;; (jolt-lang/jolt#1110).
(define (path-native-for windows? p)
  (if (and windows?
           (let loop ((i 0)) (and (fx<? i (string-length p))
                                  (or (char=? (string-ref p i) #\/) (loop (fx+ i 1))))))
      (list->string (map (lambda (c) (if (char=? c #\/) #\\ c)) (string->list p)))
      p))
(define (path-native p) (path-native-for (eq? (sa-os-family) 'windows) p))
;; A java.nio.file FileSystemException's message: "file", "file -> other", and
;; ": reason" after either, as FileSystemException.getMessage builds it. Only the
;; paths are rendered natively; the reason is strerror's text, and flipping the
;; whole string would turn "Input/output error" into "Input\output error".
(define (fs-exception-message-for windows? file other reason)
  (string-append (path-native-for windows? file)
                 (if other (string-append " -> " (path-native-for windows? other)) "")
                 (if reason (string-append ": " reason) "")))
(define (fs-exception-message file other reason)
  (fs-exception-message-for (eq? (sa-os-family) 'windows) file other reason))

(define (jolt-path-normalize-for windows? p0) (path-normalize* windows? p0 #f))
;; The same, or 'nul when p0 holds a NUL (nio-file.ss npath-held-for).
(define (jolt-path-normalize-for/nul windows? p0) (path-normalize* windows? p0 #t))
(define (path-normalize* windows? p0 nul?)
  (define (trailing-sep-droppable? p n) (trailing-sep-droppable-for? windows? p n))
  (let* ((p (if windows? (path-backslashes->slashes p0) p0))
         (n (string-length p))
         ;; POSIX classifies nothing as a root here, so its answers are exactly
         ;; what they were; only Windows has a prefix to hold back.
         (root-end (if windows? (path-root-end #t p) 0)))
    (let ((scan (path-has-double-sep? p n root-end nul?)))
    (cond
      ((eq? scan 'nul) 'nul)
      ;; an already-normal path is the overwhelmingly common case, and a jfile is
      ;; built per entry on every directory listing: look before copying, so the
      ;; answer is p itself and nothing is allocated
      ((not scan)
       (if (trailing-sep-droppable? p n)
           (substring p 0 (fx- n 1))
           p))
      (else
       (let ((out (make-string n)))
         ;; the root is copied verbatim, and the collapse picks up after it with
         ;; prev-slash? seeded from the root's last character, so a root that
         ;; ends in a separator does not then swallow the first child separator
         (let copy ((k 0))
           (when (fx<? k root-end)
             (string-set! out k (string-ref p k))
             (copy (fx+ k 1))))
         (let loop ((i root-end)
                    (j root-end)
                    (prev-slash? (and (fx>? root-end 0)
                                      (char=? (string-ref p (fx- root-end 1)) #\/))))
           (if (fx=? i n)
               ;; cut to the written prefix BEFORE asking about the trailing
               ;; separator: out is n wide and only j of it is valid, and
               ;; path-root-end measures the whole string it is handed
               (let* ((collapsed (substring out 0 j))
                      (m (string-length collapsed)))
                 (if (trailing-sep-droppable? collapsed m)
                     (substring collapsed 0 (fx- m 1))
                     collapsed))
               (let ((c (string-ref p i)))
                 (cond ((and (char=? c #\/) prev-slash?) (loop (fx+ i 1) j #t))
                       (else (string-set! out j c)
                             (loop (fx+ i 1) (fx+ j 1) (char=? c #\/)))))))))))))
(define (jolt-path-normalize p)
  (jolt-path-normalize-for (eq? (sa-os-family) 'windows) p))

(define-record-type jfile (fields path) (nongenerative jolt-jfile-v1)
  (protocol (lambda (new) (lambda (p) (new (jolt-path-normalize p))))))
(define (jolt-file? x) (jfile? x))

;; path string of any value: a jfile -> its path, else its str rendering.
(define (file-path-of x) (if (jfile? x) (jfile-path x) (jolt-str-render-one x)))

;; Resources baked into a standalone binary by `jolt build` (deps.edn
;; :jolt/build :embed). The build emits a register-public-embedded-resource! per
;; file at heap-build time, so the contents live in the boot image — io/resource serves
;; them with no file on disk. An embedded hit reads through slurp/reader exactly
;; like a jfile would.
;;
;; The PUBLIC ones -- the project's and its dependencies' resources, not jolt's
;; own runtime files -- are also an archive, the way an uberjar's are: a JarFile,
;; a read-only NIO filesystem and jar: URLs over "jolt-embedded-resources.jar"
;; (zip-file.ss, embedded-archive.ss, embedded-url.ss). Their names are kept as
;; one sorted vector, built once, which is what directory questions search.
(define embedded-resources (make-hashtable equal-hash equal?))
(define public-embedded-resources (make-hashtable string-hash string=?))
(define embedded-resource-index #f)
(define (register-embedded-resource! name content)
  (hashtable-set! embedded-resources name content))
(define (register-public-embedded-resource! name content)
  (register-embedded-resource! name content)
  (hashtable-set! public-embedded-resources name #t)
  (set! embedded-resource-index #f))
(define (embedded-resource-sorted)
  (or embedded-resource-index
      (let ((v (hashtable-keys public-embedded-resources)))
        (vector-sort! string<? v)
        (set! embedded-resource-index v)
        v)))
(define (embedded-resource-names) (vector->list (embedded-resource-sorted)))
(define (embedded-public-resource? name) (hashtable-ref public-embedded-resources name #f))
;; The first index whose name is not below KEY.
(define (embedded-resource-lower-bound key)
  (let ((v (embedded-resource-sorted)))
    (let loop ((lo 0) (hi (vector-length v)))
      (if (fx>=? lo hi)
          lo
          (let ((mid (fxsrl (fx+ lo hi) 1)))
            (if (string<? (vector-ref v mid) key) (loop (fx+ mid 1) hi) (loop lo mid)))))))
(define (embedded-string-prefix? prefix s)
  (and (fx>=? (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))
;; The names under directory PREFIX (which ends in "/"), in order.
(define (embedded-resources-under prefix)
  (let ((v (embedded-resource-sorted)))
    (let loop ((i (embedded-resource-lower-bound prefix)) (acc '()))
      (if (and (fx<? i (vector-length v)) (embedded-string-prefix? prefix (vector-ref v i)))
          (loop (fx+ i 1) (cons (vector-ref v i) acc))
          (reverse acc)))))
;; The directory NAME names, spelled with its trailing "/", when some public
;; resource lives under it. A directory exists only by holding something, as in
;; a jar written by the jar tool, so with nothing embedded there is none -- not
;; even the root, which a jar has no entry for.
(define (embedded-directory-name name)
  (let ((n (string-length name)))
    (and (fx>? n 0)
         (let* ((prefix (if (char=? (string-ref name (fx- n 1)) #\/) name (string-append name "/")))
                (v (embedded-resource-sorted))
                (i (embedded-resource-lower-bound prefix)))
           (and (fx<? i (vector-length v))
                (embedded-string-prefix? prefix (vector-ref v i))
                prefix)))))
(define (embedded-directory? name) (and (embedded-directory-name name) #t))

(define embedded-archive-name "jolt-embedded-resources.jar")
(define embedded-archive-prefix "jar:file:jolt-embedded-resources.jar!/")

;; Percent-encoding, the JDK's two ways. A class loader's resource URL is
;; built by ParseUtil.encodePath: lowercase escapes, and ";" "=" "[" "]" are
;; escaped too. A zipfs Path's toUri goes through the multi-argument URI
;; constructor, which escapes only what a URI cannot hold, in uppercase, and
;; leaves other non-ASCII characters as they are.
(define (embedded-percent-encode s escape? upper?)
  (let ((out (open-output-string))
        (hex (if upper? "0123456789ABCDEF" "0123456789abcdef")))
    (string-for-each
     (lambda (c)
       (if (escape? c)
           (embedded-bytes-for-each
            (lambda (b)
              (write-char #\% out)
              (write-char (string-ref hex (fxsrl b 4)) out)
              (write-char (string-ref hex (fxand b 15)) out))
            (string->utf8 (string c)))
           (write-char c out)))
     s)
    (get-output-string out)))
(define (embedded-bytes-for-each f bv)
  (let loop ((i 0))
    (when (fx<? i (bytevector-length bv))
      (f (bytevector-u8-ref bv i))
      (loop (fx+ i 1)))))
(define (embedded-url-escape? c)
  (let ((n (char->integer c)))
    (or (fx<=? n #x20) (fx>=? n #x7F)
        (memv c '(#\" #\# #\% #\; #\< #\= #\> #\? #\[ #\\ #\] #\^ #\` #\{ #\| #\})))))
(define (embedded-uri-escape? c)
  (let ((n (char->integer c)))
    (if (fx<? n #x80)
        (or (fx<=? n #x20) (fx=? n #x7F)
            (memv c '(#\" #\# #\% #\< #\> #\\ #\^ #\` #\{ #\| #\})))
        (or (char-whitespace? c) (eq? (char-general-category c) 'Cc)))))
(define (embedded-url-encode name) (embedded-percent-encode name embedded-url-escape? #f))
(define (embedded-uri-encode name) (embedded-percent-encode name embedded-uri-escape? #t))
;; %XX sequences back to the UTF-8 text they spell.
(define (embedded-percent-decode s)
  (let ((n (string-length s)))
    (if (not (exists (lambda (c) (char=? c #\%)) (string->list s)))
        s
        (let-values (((out extract) (open-bytevector-output-port)))
          (let loop ((i 0))
            (cond ((fx>=? i n)
                   (utf8->string (extract)))
                  ((and (char=? (string-ref s i) #\%) (fx<=? (fx+ i 3) n)
                        (string->number (substring s (fx+ i 1) (fx+ i 3)) 16))
                   => (lambda (b) (put-u8 out b) (loop (fx+ i 3))))
                  (else (put-bytevector out (string->utf8 (string (string-ref s i))))
                        (loop (fx+ i 1)))))))))

;; A resource URL in the archive: the entry NAME (as asked for, or the
;; multi-release entry the runtime version selects) and its bytes, or #f for a
;; directory. Its text is the jar: URL a class loader would answer.
(define-record-type embedded-res (fields name content) (nongenerative jolt-embres-v1))
(define (embedded-res-url r) (string-append embedded-archive-prefix (embedded-url-encode (embedded-res-name r))))
;; Its bytes; a directory entry has none, as a jar's directory entry has none.
(define (embedded-res-bytes r)
  (let ((c (embedded-res-content r)))
    (cond ((bytevector? c) c) ((string? c) (string->utf8 c)) (else (make-bytevector 0)))))
(define (embedded-resource-url name)
  (cond
    ((fx=? (string-length name) 0) #f)
    ((let ((source (embedded-runtime-resource-name name)))
       (and (embedded-public-resource? source) source))
     => (lambda (source) (make-embedded-res source (embedded-resource-ref source))))
    ((embedded-resource-ref name) => (lambda (content) (make-embedded-res name content)))
    ((embedded-directory-name name) (make-embedded-res name #f))
    (else #f)))
;; io/resource returns a java.net.URL from BOTH branches: a file: URL (a jhost
;; "url") for a hit on a source root, and this embedded-res (class java.net.URL,
;; protocol "jar") for a resource baked into a built binary. The two must answer the
;; SAME surface — getPath / getFile / getName / getProtocol / openStream — so a
;; caller that resolves a resource (orchard.namespace/canonical-source does
;; (some-> (io/resource p) .getPath)) gets the same answer whichever branch served
;; it. Registered below (after record-method-dispatch's arm registry exists)
;; rather than inline, so both branches stay in one place.
(define (embedded-res-method obj name args)
  (let* ((url (embedded-res-url obj))
         (directory? (not (embedded-res-content obj)))
         (file (substring url 4 (string-length url))))
    (cond
      ((string=? name "getPath")     (list file))
      ((string=? name "getFile")     (list file))
      ((string=? name "getName")     (list (path-last-segment (embedded-res-name obj))))
      ((string=? name "toString")    (list url))
      ((string=? name "getProtocol") (list "jar"))
      ((string=? name "exists")      (list #t))
      ((string=? name "isDirectory") (list directory?))
      ((string=? name "isFile")      (list (not directory?)))
      ((or (string=? name "equals") (string=? name "sameFile"))
       (list (and (embedded-res? (car args))
                  (string=? (embedded-res-name obj) (embedded-res-name (car args))))))
      ((string=? name "hashCode")    (list (string-hash url)))
      ((string=? name "getHost")     (list ""))
      ((string=? name "getAuthority") (list jolt-nil))
      ((or (string=? name "getPort") (string=? name "getDefaultPort")) (list -1))
      ((or (string=? name "getQuery") (string=? name "getUserInfo") (string=? name "getRef")) (list jolt-nil))
      ((string=? name "toExternalForm") (list url))
      ((string=? name "toURI")      (list (uri-ctor url)))
      ((string=? name "openConnection")
       (begin
         (when (pair? args)
           (cond ((jolt-nil? (car args))
                  (throw-jvm 'IllegalArgumentException "proxy can not be null"))
                 ((not (and (jhost? (car args)) (string=? (jhost-tag (car args)) "proxy")))
                  (throw-jvm 'ClassCastException "argument is not java.net.Proxy"))))
         (list (embedded-url-open-connection obj))))
      ((string=? name "getContent")
       (embedded-jar-connection-method (embedded-url-open-connection obj) "getContent" args))
      ((string=? name "openStream")
       ;; A byte stream, not a reader: a URL's openStream is byte-level on the
       ;; JVM, and a baked resource can be binary (the same StringReader ->
       ;; InputStream fix url-open-stream records for file: URLs). io/reader
       ;; still decodes whatever it is handed.
       (list (embedded-connection-stream (embedded-url-open-connection obj))))
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
  (let ((v (embedded-resource-ref name)))
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
;; flat.ss literal re-materializes at every Sbuild_heap. Instead build-jolt links
;; one concatenated blob into the binary as the C array jolt_stdlib_fasls
;; (-rdynamic exports it), and the launcher calls jolt-stdlib-fasls-attach! with
;; the namespace names in blob order before any require. jolt-embedded-fasl then
;; memcpy's the slice out of the C array on demand — once per ns per process,
;; never cached.
;;
;; The blob starts with a table of (offset, length) pairs, one per name, as
;; little-endian u64s, offsets from the start of the blob. The table lives in
;; the blob rather than in the launcher's attach call so flat.ss does not wait
;; on the fasls being compiled: build-jolt compiles the two in parallel.
(define embedded-fasls (make-hashtable string-hash string=?))
;; ns-name -> its entry number in the linked jolt_stdlib_fasls C array's table.
;; Populated once by the launcher's jolt-stdlib-fasls-attach!; empty in every
;; path that carries no such array (dev bin/jolt, devcache, app binaries).
(define embedded-fasl-index (make-hashtable string-hash string=?))
(define (jolt-stdlib-fasls-attach! names)
  (let loop ((ns names) (i 0))
    (when (pair? ns)
      (hashtable-set! embedded-fasl-index (car ns) i)
      (loop (cdr ns) (fx+ i 1)))))
;; memcpy entry I's fasl out of the jolt_stdlib_fasls C array into a fresh
;; bytevector. #f when the symbol is absent (dev/devcache/apps carry no such
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
(define (jolt-stdlib-fasl-fetch i)
  (guard (e (else #f))
    (let* ((base (sa-foreign-entry-address "jolt_stdlib_fasls"))
           (memcpy (sa-foreign-procedure "memcpy" (u8* uptr uptr) void*))
           (entry (make-bytevector 16)))
      (memcpy entry (+ base (* 16 i)) 16)
      (let* ((offset (bytevector-u64-ref entry 0 (endianness little)))
             (length (bytevector-u64-ref entry 8 (endianness little)))
             (bv (make-bytevector length)))
        (memcpy bv (+ base offset) length)
        bv))))
(define (jolt-embedded-fasl name)
  (let ((v (hashtable-ref embedded-fasls name #f)))
    (cond
      ((bytevector? v) v)
      (else
       (let ((i (hashtable-ref embedded-fasl-index name #f)))
         (and i (jolt-stdlib-fasl-fetch i)))))))

;; --- embedded SOURCE, the same treatment as the fasls above -----------------
;; jolt-core/ and stdlib/ source is embedded so a built binary can load a
;; namespace that is neither in the runtime image nor carries a fasl. It used to
;; be emitted as one (register-embedded-resource! "<path>" (string->utf8 "…"))
;; per file, which is precisely the boot-image-literal shape the comment above
;; warns about: every start re-materialized each string literal AND allocated a
;; fresh utf8 bytevector from it. Measured on `jolt --version` at 59ms and +47MB
;; of heap, and that heap was then paid for a second time by the Scompact_heap
;; at the end of Sbuild_heap.
;;
;; The same answer applies, and build-jolt.ss had already reached it twice —
;; once for the stdlib fasls above, once for the build subsystem's own .ss
;; embeds (deferred into a thunk). So the bytes go into one concatenated C array
;; (jolt_source_blob) and only the index is baked. Same locking discipline as
;; the fasl index: written once by the launcher before scheme-start, read
;; afterwards by single-key hashtable-ref.
;;
;; Empty in every path that carries no such array — dev bin/jolt, devcache, and
;; `jolt build` app binaries, which register their own embeds eagerly — so the
;; fetch returning #f is a normal answer there, not a failure. Written once while
;; the heap is built, single-threaded, and read afterwards by single-key
;; hashtable-ref, which is the same discipline the fasl index above keeps.
(define embedded-source-index (make-hashtable equal-hash equal?))
(define (jolt-source-blob-attach! index)
  (for-each (lambda (entry)
              (hashtable-set! embedded-source-index (car entry)
                              (cons (cadr entry) (caddr entry))))
            index))
(define (jolt-source-blob-fetch offset length)
  (guard (e (else #f))
    (let* ((base (sa-foreign-entry-address "jolt_source_blob"))
           (bv (make-bytevector length))
           (memcpy (sa-foreign-procedure "memcpy" (u8* uptr uptr) void*)))
      (memcpy bv (+ base offset) length)
      ;; Each slice is one bytevector-compress frame. Compressing pays here in a
      ;; way it does not for the boot image: the boot is read start to finish on
      ;; every single start, where unpacking outruns readahead and serializes a
      ;; read that overlapped the parse, but this is read only when a namespace
      ;; loads from source — never on the boot path — so the bytes come off the
      ;; binary lazily and one small inflate costs nothing measurable. The frame
      ;; records its own format and size, so nothing here has to agree with the
      ;; build about either.
      (bytevector-uncompress bv))))

;; The lookup every reader of embedded-resources goes through: the eager table
;; first — `jolt build`'s embed dirs, the runtime .ss thunk and the materialized
;; bundles all still register directly — then the blob index. Readers already
;; accepted a bytevector here (the old literals were string->utf8), so what comes
;; back has the same shape it always did.
(define (embedded-resource-ref name)
  (or (hashtable-ref embedded-resources name #f)
      (let ((ol (hashtable-ref embedded-source-index name #f)))
        (and ol (jolt-source-blob-fetch (car ol) (cdr ol))))))

;; Presence WITHOUT the bytes. resolve-on-roots probes several candidate paths on
;; every require and ldr-install-file? asks about one on every load; when the
;; answer lives in the blob, fetching it to test existence would memcpy a whole
;; file to throw it away. Those callers ask this instead.
(define (embedded-resource-has? name)
  (or (and (hashtable-ref embedded-resources name #f) #t)
      (and (hashtable-ref embedded-source-index name #f) #t)))

;; --- with-port: open a port, do work, close on success or throw ----------------
(define (with-port port proc)
  (guard (e (#t (guard (_ (#t #f)) (close-port port)) (raise e)))
    (let ((result (proc port)))
      (close-port port)
      result)))

;; --- entries of an archive as paths ------------------------------------------
;; A namespace source or a resource inside a jar on the roots is spelled as the
;; JVM spells its URL: "jar:file:<absolute jar path>!/<entry name>". Every read
;; of such a path — read-file-string, read-file-bytes, slurp, io/reader,
;; io/input-stream, the resource resolver and the loader — goes through the
;; archive's central directory (java/zip-file.ss, which loads later; its names
;; are reached at call time), never through an extraction (jolt issue #1005).
;; *file* carries the spelling for a namespace loaded from a jar, so an error
;; report reads its source lines the same way. A whole entry read checks the
;; entry's CRC-32 (zipdir-entry-bytes), so a damaged jar is refused where it is
;; read, with the jar and the entry in the message.
(define jar-path-prefix "jar:file:")
(define (jar-path-split p)
  (let ((pn (string-length jar-path-prefix)))
    (and (string? p)
         (> (string-length p) pn)
         (string=? (substring p 0 pn) jar-path-prefix)
         (let loop ((i pn))
           (cond ((>= (+ i 1) (string-length p)) #f)
                 ((and (char=? (string-ref p i) #\!) (char=? (string-ref p (+ i 1)) #\/))
                  (cons (substring p pn i) (substring p (+ i 2) (string-length p))))
                 (else (loop (+ i 1))))))))
(define (jar-path? p) (and (jar-path-split p) #t))
(define (make-jar-path jar entry) (string-append jar-path-prefix (file-uri-path jar) "!/" entry))
;; The archive index and the entry a jar path names, or (values #f #f) when the
;; archive is not readable or has no such entry.
(define (jar-path-entry p)
  (let ((parts (jar-path-split p)))
    (if (not parts)
        (values #f #f)
        ;; the archive is a file: URL's path, the entry a URL path segment:
        ;; both may be escaped, and the jar may be spelled "/C:/…" (#1118)
        (let ((d (zipdir-for (file-url->path (car parts)))))
          (if (not d)
              (values #f #f)
              ;; a directory answers to its name without the "/", as
              ;; ZipFile.getEntry looks it up
              (let ((ent (zipdir-lookup d (uri-decode-lenient (cdr parts)))))
                (if ent (values d ent) (values #f #f))))))))
;; A jar path into the image's own archive ("jar:file:jolt-embedded-resources.jar
;; !/name", the URL io/resource hands out in a built binary) names an embedded
;; resource, not a file: its resource URL, or #f.
(define (embedded-jar-path-res p)
  (let ((parts (jar-path-split p)))
    (and parts
         (string=? (car parts) embedded-archive-name)
         (embedded-resource-url (embedded-percent-decode (cdr parts))))))
(define (jar-path-exists? p)
  (or (and (embedded-jar-path-res p) #t)
      (let-values (((d ent) (jar-path-entry p))) (and ent #t))))
;; The entry's bytes, or #f when the path names no entry. A damaged entry raises
;; the ZipException the read finds, with the path in it.
(define (jar-path-bytes p)
  (let-values (((d ent) (jar-path-entry p)))
    (if (not ent)
        (let ((r (embedded-jar-path-res p))) (and r (embedded-res-bytes r)))
         (guard (e ((jolt-throw-condition? e)
                    (let ((v (jolt-throw-condition-value e)))
                      (if (ex-info-map? v)
                          (jolt-throw (jolt-host-throwable
                                       (ex-info-class v)
                                       (string-append p ": "
                                                      (let ((m (jolt-ex-info-record-message v)))
                                                        (if (string? m) m "unreadable entry")))))
                          (raise e)))))
           (zipdir-entry-bytes d ent)))))
;; A java.io.FileNotFoundException for a jar path with no such entry, the class
;; a missing file raises.
(define (jar-path-missing p)
  (throw-jvm (quote java.io.FileNotFoundException)
             (string-append p " (No such file or directory)")))

;; Read a whole file as a bytevector ("" -> empty). Used to slurp boot/stub files.
(define (read-file-bytes path)
  (if (jar-path? path)
      (or (jar-path-bytes path) (jar-path-missing path))
      (with-port (open-file-input-port path)
        (lambda (p) (let ((bv (get-bytevector-all p))) (if (eof-object? bv) (bytevector) bv))))))

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
;; every Chez binary) — no external toolchain, and no program: this used to fall
;; back to a `chmod 755` through the shell, the one thing the runtime ran off
;; PATH besides git, and a fallback that could never be reached on a POSIX host
;; (libc always has chmod) while hiding a broken FFI. A libc without it is an
;; error that says so.
(define jolt-chmod-755
  (let ((c (jolt-foreign-proc-safe "chmod" '(string int) 'int)))
    (lambda (path)
      (cond
        (c (c path #o755))
        ;; Windows has no chmod and needs none (execute is by extension)
        ((eq? (sa-os-family) 'windows) 0)
        (else (error 'jolt-chmod-755 "chmod does not resolve in this process's libc" path))))))

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
(define (path-separator-char? c)
  (or (char=? c #\/) (char=? c #\\)))

(define (ascii-drive-letter? c)
  (or (and (char>=? c #\A) (char<=? c #\Z))
      (and (char>=? c #\a) (char<=? c #\z))))

(define (windows-drive-prefix? p)
  (and (>= (string-length p) 2)
       (ascii-drive-letter? (string-ref p 0))
       (char=? (string-ref p 1) #\:)))

(define (windows-root-relative-for? windows? p)
  (and windows?
       (> (string-length p) 0)
       (path-separator-char? (string-ref p 0))
       (or (= (string-length p) 1)
           (not (path-separator-char? (string-ref p 1))))))
(define (windows-root-relative? p)
  (windows-root-relative-for? (eq? (sa-os-family) 'windows) p))

(define (trim-trailing-path-separator p)
  (let ((n (string-length p)))
    (if (and (> n 2) (path-separator-char? (string-ref p (- n 1))))
        (substring p 0 (- n 1))
        p)))

;; java.io.File.isAbsolute is host-platform-specific. POSIX has one absolute
;; prefix (/). Windows has drive-rooted paths (C:\x or C:/x) and UNC paths
;; (\\server\share); drive-relative C:x and current-drive-rooted \x are not
;; absolute in the JVM sense. Drive-rooted and UNC recognition is shared by
;; filesystem resolution, getAbsolutePath, and isAbsolute. The rooted-but-
;; relative case is resolved separately by project-relative below. `C:child`
;; still depends on Windows' process-local current directory for that drive and
;; remains an older File-shim compatibility gap.
;;
;; Split on the platform the way jfile-fold-dots-for is, so the rows a Linux
;; runner can never reach are still pinned (test/chez/win-platform-test.ss), and
;; so ProcessBuilder's program resolver can ask the same question this does
;; instead of keeping a second, POSIX-only answer of its own (#1074).
(define (jfile-path-absolute-for? windows? p)
  (let ((n (string-length p)))
    (if windows?
        (or (and (>= n 3)
                 (windows-drive-prefix? p)
                 (path-separator-char? (string-ref p 2)))
            (and (>= n 2)
                 (path-separator-char? (string-ref p 0))
                 (path-separator-char? (string-ref p 1))))
        (and (> n 0) (char=? (string-ref p 0) #\/)))))
(define (jfile-path-absolute? p)
  (jfile-path-absolute-for? (eq? (sa-os-family) 'windows) p))

;; BASE joined to the relative P with one separator: a base that already ends in
;; one -- the root directory "/" -- does not get a second, which made "/src" into
;; "//src" and its URL file://src/, a host named "src".
(define (path-under base p)
  (let ((n (string-length base)))
    (if (and (> n 0) (memv (string-ref base (- n 1)) '(#\/ #\\)))
        (string-append base p)
        (string-append base "/" p))))
(define (project-relative p)
  (cond
    ((or (= (string-length p) 0) (jfile-path-absolute? p)) p)
    ;; an entry inside a jar is already absolute (the jar's path is)
    ((jar-path? p) p)
    ;; A single leading separator is rooted on the current drive but is not an
    ;; absolute File pathname on Windows. The JVM resolves it against user.dir's
    ;; drive; the process cwd is Jolt's source tree, so leaving it to the OS can
    ;; select the wrong drive after the launcher changes directory.
    ((windows-root-relative? p)
     (let ((base (jolt-user-dir)))
       (cond
         ;; The base names a drive, so the current-drive-rooted path takes it.
         ((windows-drive-prefix? base) (string-append (substring base 0 2) p))
         ;; A UNC base has no drive letter; its \\server\share IS the root.
         ((jfile-path-absolute? base)
          (string-append (trim-trailing-path-separator base) p))
         ;; Nothing to root against: JOLT_PWD is unset, so jolt-user-dir is ".".
         ;; Prefixing that turns a ROOTED path into a relative one, which is
         ;; strictly worse than leaving the OS to resolve it against the process
         ;; drive — the drive is at least a plausible answer, "./\x" is not.
         ;; jolt.deps/root-relative-for keeps the same arm for the same reason,
         ;; and its (absolute true "." "\project") case pins it; without this
         ;; the two classifiers disagree on the one input neither can resolve.
         (else p))))
    (else
     (let ((base (jolt-user-dir)))
       ;; "." adds nothing the OS won't do itself when it resolves a relative
       ;; path — leave it alone rather than prefixing "./".
       (if (string=? base ".") p (path-under base p))))))

;; (io/file path) / (io/file parent child) — join children with "/". The File
;; keeps the path AS GIVEN (like the JVM: new File("rel").getPath() is "rel");
;; a relative path resolves against JOLT_PWD only when the filesystem is touched
;; (jfile-fs / slurp / spit / the stream constructors).
(define (jolt-make-file path . rest)
  (let loop ((p (file-path-of path)) (cs rest))
    (if (null? cs)
        ;; (io/file url) strips the scheme — File of url.toURI on the JVM; only a
        ;; file: url names a path. url-file-coercion is defined below; call-time ref.
        (cond ((null? rest)
               (cond ((url-jhost? path) (url-file-coercion path))
                     ;; as-file of a URI is as-file of its URL (clojure.java.io)
                     ((uri-jhost? path) (url-spec-file-coercion (uri-field path 'string)))
                     (else (make-jfile p))))
              (else (make-jfile p)))
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
;; project-relative answers an absolute path with itself, so asking it twice —
;; once here to decide, once inside — only pays the host-specific classification
;; twice per call. The empty path is the one case it does not cover.
(define (jfile-abs p)
  (if (= (string-length p) 0) (jolt-user-dir) (project-relative p)))

;; java.io.File.slashify, the path File.toURI and File.toURL are built from: an
;; EXISTING directory's URL ends in "/". That trailing slash is not cosmetic —
;; it is what tells a consumer of the URL that the thing is a container, and
;; what relative resolution against the URL keys on: resolved against
;; "file:/root" a name replaces the last segment, against "file:/root/" it lands
;; inside. The JVM asks the filesystem (File.isDirectory), so a path that is not
;; there, or is a plain file, gets no slash.
;; The directory question is asked of the RESOLVED path: (File. "") is the
;; working directory, whose raw path "" is no directory to file-directory?.
(define (jfile-uri-path p)
  (let ((abs (jfile-abs p)))
    (if (and (file-directory? abs)
             (> (string-length abs) 0)
             (not (char=? (string-ref abs (- (string-length abs) 1)) #\/)))
        (string-append abs "/")
        abs)))
;; The two edges between a filesystem path and the path of a file: URL, per
;; platform (jolt-lang/jolt#1118). Everything between them is written once over
;; the URL spelling; the platform is a parameter so the Windows rows are pinned
;; from any host (test/chez/win-platform-test.ss).
;;
;; OUT, java.io.File.slashify over an absolute path: on Windows the separators
;; become "/" and the path gains the "/" a drive does not start with, so
;; "C:\a\b" is "/C:/a/b" and the URL "file:/C:/a/b" — the JDK's spelling, and
;; the one every consumer of a file URL expects. A UNC path keeps its host as the
;; JDK does: "//srv/sh" becomes "////srv/sh". POSIX paths are already in URL
;; form. Characters are NOT encoded here; File.toURI encodes, File.toURL and
;; jolt's own file:/jar:file: spellings do not.
(define (file-uri-path-for windows? abs)
  (if windows?
      (let* ((p (list->string (map (lambda (c) (if (char=? c #\\) #\/ c)) (string->list abs))))
             (p (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/))
                    p
                    (string-append "/" p))))
        (if (and (>= (string-length p) 2) (string=? (substring p 0 2) "//"))
            (string-append "//" p)
            p))
      abs))
(define (file-uri-path abs) (file-uri-path-for (win32?) abs))

;; IN, the filesystem path a file: URL names — what the JDK's file: handler
;; opens. An empty or "localhost" authority is dropped ("file:///a" is "/a"),
;; any other is a UNC host ("file://srv/sh/x" is "//srv/sh/x"); %hh escapes are
;; decoded, since File.toURI writes them; and on Windows the "/" in front of a
;; drive is dropped, since "/C:/a" is a path on the current drive's root that
;; names nothing. The JVM rejects a malformed escape; a "%" that does not start
;; one is left as a literal here, because jolt's own file: spellings carry the
;; path unencoded and a "%" in a file name must still open.
(define (file-url->path-for windows? spec)
  (let* ((rest (if (and (>= (string-length spec) 5) (string-ci=? (substring spec 0 5) "file:"))
                   (substring spec 5 (string-length spec))
                   spec))
         (rest (if (and (>= (string-length rest) 2) (string=? (substring rest 0 2) "//"))
                   (let* ((n (string-length rest))
                          (slash (let loop ((j 2)) (cond ((>= j n) n)
                                                         ((char=? (string-ref rest j) #\/) j)
                                                         (else (loop (+ j 1))))))
                          (host (substring rest 2 slash))
                          (path (substring rest slash n)))
                     (if (or (string=? host "") (string-ci=? host "localhost"))
                         path
                         (string-append "//" host path)))
                   rest))
         (p (uri-decode-lenient rest)))
    (uri-path->drive-path windows? p)))
(define (file-url->path spec) (file-url->path-for (win32?) spec))

;; A URI path names a drive as "/C:/..."; on Windows the path is "C:/...".
(define (uri-path->drive-path windows? p)
  (if (and windows?
           (>= (string-length p) 3)
           (char=? (string-ref p 0) #\/)
           (windows-drive-prefix? (substring p 1 (string-length p)))
           (or (= (string-length p) 3) (path-separator-char? (string-ref p 3))))
      (substring p 1 (string-length p))
      p))

;; new File(URI), and the default filesystem's getPath(URI) under Paths.get and
;; Path.of (jolt-lang/jolt#1198). Unlike a file: URL, which clojure.java.io reads
;; leniently above, the JDK takes only an absolute, hierarchical file: URI with
;; no authority, query or fragment, and says which rule failed, in this order.
;; A localhost authority is refused here, not dropped. Both callers used to hand
;; the URI's string on as the path, so "file:///tmp/a%20b" became the relative
;; path "file:/tmp/a%20b".
(define (uri->file-path-for windows? u)
  (define (bad msg) (throw-jvm 'IllegalArgumentException msg))
  (let ((scheme (uri-field u 'scheme)))
    (when (jolt-nil? scheme) (bad "URI is not absolute"))
    (when (uri-opaque? u) (bad "URI is not hierarchical"))
    (unless (string-ci=? scheme "file") (bad "URI scheme is not \"file\""))
    (unless (jolt-nil? (uri-field u 'authority)) (bad "URI has an authority component"))
    (unless (jolt-nil? (uri-field u 'fragment)) (bad "URI has a fragment component"))
    (unless (jolt-nil? (uri-field u 'query)) (bad "URI has a query component"))
    (let ((p (uri-field u 'dec-path)))
      (when (or (jolt-nil? p) (string=? p "")) (bad "URI path component is empty"))
      (uri-path->drive-path windows? p))))
(define (uri->file-path u) (uri->file-path-for (win32?) u))
;; Path.of(URI): the scheme picks the filesystem provider first, and only the
;; default one ("file") is installed.
(define (nio-uri->path u)
  (let ((scheme (uri-field u 'scheme)))
    (cond ((jolt-nil? scheme) (throw-jvm 'IllegalArgumentException "Missing scheme"))
          ((string-ci=? scheme "file") (uri->file-path u))
          (else (jolt-throw (jolt-host-throwable
                              "java.nio.file.FileSystemNotFoundException"
                              (string-append "Provider \"" scheme "\" not installed")))))))

;; The path a STRING names as a clojure.java.io source or sink. Its Coercions
;; try (URL. s) before (File. s), so "file:/a/b" is the file /a/b rather than a
;; relative path whose first segment is "file:"; any other string is a path.
(define (file-url-string? s)
  (and (>= (string-length s) 5) (string-ci=? (substring s 0 5) "file:")))
(define (io-source-path s)
  (project-relative (if (file-url-string? s) (file-url->path s) s)))

;; %hh decoding when every "%" starts a well-formed escape, else the text as it
;; is (see file-url->path-for).
(define (uri-decode-lenient s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((>= i n) (uri-decode s))
            ((char=? (string-ref s i) #\%)
             (if (and (<= (+ i 3) n) (uri-hex? (string-ref s (+ i 1))) (uri-hex? (string-ref s (+ i 2))))
                 (loop (+ i 3))
                 s))
            (else (loop (+ i 1)))))))

;; File.toURI / Path.toUri: a java.net.URI over the file: form of the path, its
;; characters percent-encoded and an existing directory's ending in a slash.
(define (jfile->uri-spec p)
  (string-append "file:" (uri-quote-path (file-uri-path (jfile-uri-path p)))))
(define (jfile->uri p) (uri-parse (jfile->uri-spec p)))
;; File.toURL: the same URL unencoded, as the JDK's (deprecated) toURL spells it.
(define (jfile->url-spec p)
  (string-append "file:" (file-uri-path (jfile-uri-path p))))

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
;; --- why realpath failed ------------------------------------------------------
;; The walk below answers a best-effort path when realpath fails, which is right
;; for a path that merely does not exist yet: the JVM does the same. It is wrong
;; for a failure meaning the path can NEVER name a file, where the JVM raises.
;; Answering a string there lets a path that cannot be opened travel on as though
;; it could.
;;
;; The JVM's split, measured against Clojure 1.12 rather than assumed:
;;
;;   ENOENT / ENOTDIR / EACCES    best-effort path, no error
;;   ELOOP / ENAMETOOLONG         java.io.IOException, in strerror's own wording
;;
;; Errno comes from the location accessor rather than Chez's native-error
;; convention, which needs a literal foreign-procedure whose load-time relocation
;; aborts the boot where the symbol is absent -- exactly the Windows build
;; realpath is already missing from. Same three spellings process.ss uses:
;; Darwin/BSD, glibc/musl, then bionic.
(define io-errno-loc
  (or (jolt-foreign-proc-safe "__error" '() 'void*)
      (jolt-foreign-proc-safe "__errno_location" '() 'void*)
      (jolt-foreign-proc-safe "__errno" '() 'void*)))
(define (io-errno)
  (if io-errno-loc (guard (e (#t 0)) (sa-foreign-ref 'int (io-errno-loc) 0)) 0))

;; macOS values from <sys/errno.h>; Linux from asm-generic/errno.h. The same
;; os-family split process.ss uses for EAGAIN and io_poller.clj for EINPROGRESS.
;; A wrong value here degrades to today's behavior (no raise) rather than
;; misfiring, and the gate exercises both platforms.
(define io-ELOOP        (if (eq? (sa-os-family) 'macos) 62 40))
(define io-ENAMETOOLONG (if (eq? (sa-os-family) 'macos) 63 36))

(define (jfile-realpath* p)                 ; -> (values path-or-#f errno)
  (cond
    (c-realpath
     (let ((buf (make-bytevector 4096 0)))
       (if (= 0 (c-realpath p buf))
           (values #f (io-errno))
           (values (jfile-cstr buf) 0))))
    ((win32?) (values (win32-long-path p) 0))
    (else (values #f 0))))

(define (jfile-realpath p)
  (let-values (((rp e) (jfile-realpath* p))) rp))

;; realpath for an ANCESTOR of the path being canonicalized -- a component that
;; has to be resolved to descend through. A failure that can never name a file
;; raises HERE, where the same failure on the FINAL component does not: the JVM
;; leaves a trailing symlink loop or over-long name unresolved and answers,
;; and raises only when it had to walk through one. Both directions measured.
(define (jfile-realpath-ancestor p)
  (let-values (((rp e) (jfile-realpath* p)))
    (cond (rp rp)
          ((= e io-ELOOP)
           (throw-jvm (quote java.io.IOException)
                      "Too many levels of symbolic links"))
          ((= e io-ENAMETOOLONG)
           (throw-jvm (quote java.io.IOException) "File name too long"))
          (else #f))))

;; A Java String can hold a NUL and a C path cannot, so a path carrying one can
;; never name a file. The JVM refuses it in the CANONICALIZING route
;; specifically: File.exists answers false rather than raising, and
;; getAbsolutePath hands the NUL straight back. So this belongs here and not in
;; jfile-abs, which those two go through.
;; It runs ahead of every File query and file open, so the walk is unchecked:
;; the string? test leaves only the index, which stays below the length, and
;; the checked string-ref made it ~4x slower.
(define (jfile-nul? p)
  (and (string? p)
       (let ((n (string-length p)))
         (let loop ((i 0))
           (and (sa-ufx<? i n)
                (or (eq? (sa-ustring-ref p i) #\nul)
                    (loop (sa-ufx+ i 1))))))))
(define (jfile-nul-free! p)
  (when (jfile-nul? p)
    (throw-jvm (quote java.io.IOException) "Invalid file path")))

;; What a File whose path holds a NUL answers to the methods that ask the file
;; system, File.isInvalid's answers: nothing exists, nothing changes. Asking the
;; OS instead asked about the path's text BEFORE the NUL, where a C string ends,
;; so (.delete (File. "notes.txt\0x")) deleted notes.txt. createNewFile raises
;; (jfile-nul-free!), as the JVM's does.
(define jfile-invalid-answers
  (let ((t (make-hashtable string-hash string=?)))
    (for-each (lambda (m) (hashtable-set! t m #f))
              '("exists" "isDirectory" "isFile" "isHidden" "canRead" "canWrite"
                "canExecute" "mkdir" "mkdirs" "delete" "renameTo" "setReadable"
                "setWritable" "setExecutable" "setReadOnly" "setLastModified"))
    (for-each (lambda (m) (hashtable-set! t m 0)) '("length" "lastModified"))
    (for-each (lambda (m) (hashtable-set! t m jolt-nil)) '("list" "listFiles"))
    t))

;; The directory half of an output path: "/a/b" -> "/a", "/a" -> "/", and #f for
;; a root or a bare name, which have nothing above them to create. Its callers
;; walk up with it until something exists (build.ss bld-mkdir-p, loader.ss
;; aot-mkdir-p) or take an executable's directory (build.ss, build-jolt.ss).
;;
;; Those paths are not only ones jolt composed with "/": `jolt build -o` takes
;; whatever the user typed, and on Windows that is "C:\proj\out\app.exe". The
;; POSIX-only version found no "/" in it and answered #f for its parent, which
;; the walk then handed to string=? (jolt-lang/jolt#1207). So it splits on the
;; platform's separators and stops at the platform's root: "C:\a" -> "C:\",
;; "\\srv\sh\a" -> "\\srv\sh", and a drive or share root is terminal. The
;; spelling is kept — a parent is a prefix of the path, never a re-rendering.
;; The platform is a parameter so the Windows rows are pinned from a POSIX host
;; (test/chez/win-path-test.ss).
(define (path-parent-for windows? p)
  (let ((root (path-root-end windows? p)))
    (let loop ((i (- (string-length p) 1)))
      (cond ((< i root)
             (and (> root 0) (< root (string-length p)) (substring p 0 root)))
            ((path-sep-for? windows? (string-ref p i)) (substring p 0 i))
            (else (loop (- i 1)))))))
(define (path-parent p) (path-parent-for (win32?) p))

;; --- the lexical half of canonicalization ------------------------------------
;; Everything below splits a path ONCE into its root and the segments under it,
;; and rebuilds from that pair. The root is what the POSIX-only version could
;; not express: it rejoined every segment as "/" + segment, so on Windows a
;; drive-absolute path came back as "/C:/Users/x/a.txt" — a path resolved
;; against the CURRENT drive, so reading or writing the canonicalized value
;; failed as "C:/C:/Users/x/…" (jolt-lang/jolt#991).
;;
;; The platform is a parameter rather than a call to sa-os-family so the Windows
;; rows are gated from a POSIX host (test/chez/win-path-test.ss) — the Windows
;; build is exactly where realpath is missing and this fallback is the whole of
;; getCanonicalPath.

;; Is C a separator for this platform? POSIX has one; "\" is an ordinary
;; filename character there and must stay one. Windows accepts either, and the
;; fallback receives either — a File built from "C:\Users\x\a.txt" reached
;; jfile-fold-dots as a single unsplittable segment.
(define (path-sep-for? windows? c)
  (or (char=? c #\/) (and windows? (char=? c #\\))))

;; Is P already spelled in the native Windows style — backslashes and no forward
;; slash? A join onto such a path uses ITS separator rather than handing back a
;; mixed spelling: Windows accepts either, but a %TEMP%- or PATH-derived
;; directory is native, and the joined path is what the caller then sees in an
;; error message or hands to a child process. Callers: the java.nio.file Path
;; resolve/join (nio-file.ss) and ProcessBuilder's program resolver
;; (process.ss). Always false on POSIX, where a backslash is an ordinary
;; character in a filename and nothing about it says "separator".
(define (path-backslash-style? windows? p)
  (and windows?
       (let loop ((i 0) (bs #f))
         (cond ((= i (string-length p)) bs)
               ((char=? (string-ref p i) #\/) #f)
               ((char=? (string-ref p i) #\\) (loop (+ i 1) #t))
               (else (loop (+ i 1) bs))))))

;; The separator a join adds after P: the one P already uses, else "/".
(define (path-join-sep windows? p) (if (path-backslash-style? windows? p) "\\" "/"))

;; --- the Win32 native surface ------------------------------------------------
;; The handful of kernel32 entry points the shims need where POSIX has no answer:
;; the DOS file attributes behind java.nio.file.Files/isHidden (nio-file.ss), and
;; the UTF-16 marshalling every W entry point takes, which the ProcessBuilder
;; spawn path (process.ss) shares. Lives here because io.ss is the file both of
;; those already load — and already the home of the other platform-parameterized
;; path helpers above.
;;
;; Everything is resolved LAZILY and only on Windows: on POSIX nothing here ever
;; loads a library or looks up an entry, so a jolt that never calls one pays
;; nothing and a host without the entry degrades rather than failing to boot.
;;
;; kernel32 is loaded explicitly, as sa-windows-env-entries does for
;; GetEnvironmentStringsW and jolt.nrepl does for ws2_32: -lkernel32 being linked
;; does not put its symbols in jolt.exe's own export table, so the process handle
;; alone does not resolve them. load-shared-object PREPENDS to Chez's lookup list
;; and every later foreign-entry walks it, so each library is loaded at most once.
(define win32? (lambda () (eq? (sa-os-family) 'windows)))

;; File.compareTo is the file system's compare on the two paths: String.compareTo
;; on Unix, compareToIgnoreCase on Windows, where names are case-insensitive. Its
;; argument is cast to a File, so the caller refuses anything else first.
(define (jfile-compare-for windows? a b)
  (if windows? (jvm-string-ci-compare a b) (jvm-string-compare a b)))

(define win32-loaded-libs '())
;; Load first, mark after: marking first let a second thread skip the load and
;; ask foreign-entry? before the library was there, then remember its #f (the
;; #1331 race again, one level down). Two first callers may both load; a second
;; load only puts the same handle on the lookup list again.
(define (win32-load-lib! name)
  (unless (member name win32-loaded-libs)
    (guard (e (#t #f)) (sa-load-shared-object name))
    (set! win32-loaded-libs (cons name win32-loaded-libs))))

;; A Win32 entry point, or #f: #f off Windows, #f when the library or the symbol
;; is missing. Resolved through sa-foreign-procedure-runtime for the reason
;; jolt-foreign-proc-safe takes that branch on Windows — a compiled foreign
;; reference is a load-time fasl relocation there, and a missing symbol aborts
;; the boot before any guard can run.
(define (win32-proc lib name args res blocking?)
  (and (win32?)
       (begin
         (win32-load-lib! lib)
         (and (sa-foreign-entry? name)
              (guard (e (#t #f)) (sa-foreign-procedure-runtime name args res blocking?))))))

;; Resolve on first use and remember the answer (including #f). The answer and
;; the "resolved" mark are ONE store: a separate done? flag set before the answer
;; let a thread arriving in between read #f, so threads starting their first
;; child process together failed with "no Windows spawn path" (#1331). Two first
;; callers may now both resolve, which is harmless — they get the same answer.
(define win32-unresolved (list 'win32-unresolved))
(define-syntax define-win32-memo
  (syntax-rules ()
    ((_ id resolve)
     (define id
       (let ((memo win32-unresolved))
         (lambda ()
           (when (eq? memo win32-unresolved)
             (set! memo resolve))
           memo))))))
(define-syntax define-win32-proc
  (syntax-rules ()
    ((_ id lib name args res)
     (define-win32-memo id (win32-proc lib name (quote args) (quote res) #f)))))
;; Same, for an entry point that may block (a pipe read): the call is
;; __collect_safe, so a thread parked in it does not hold up the collector.
(define-syntax define-win32-proc-blocking
  (syntax-rules ()
    ((_ id lib name args res)
     (define-win32-memo id (win32-proc lib name (quote args) (quote res) #t)))))

;; A NUL-terminated UTF-16LE copy of S in foreign memory — what every W entry
;; point takes. The caller owns it and must sa-foreign-free it. Surrogate pairs
;; are string->utf16's to get right, which is the reason not to hand-roll it.
(define (win32-wstr s)
  (let* ((bv (string->utf16 s (endianness little)))
         (n (bytevector-length bv))
         (p (sa-foreign-alloc (+ n 2))))
    (sa-foreign-bytes-set! p bv n)
    (sa-foreign-set! 'unsigned-8 p n 0)
    (sa-foreign-set! 'unsigned-8 p (+ n 1) 0)
    p))

;; Run BODY with the wide copy of S, freeing it however BODY leaves.
(define (win32-with-wstr s proc)
  (let ((w (win32-wstr s)))
    (dynamic-wind (lambda () #f) (lambda () (proc w)) (lambda () (sa-foreign-free w)))))

(define win32-INVALID-FILE-ATTRIBUTES #xFFFFFFFF)
(define win32-FILE-ATTRIBUTE-HIDDEN   #x2)
(define win32-FILE-ATTRIBUTE-DIRECTORY #x10)

(define-win32-proc win32-get-file-attributes-w
  "kernel32.dll" "GetFileAttributesW" (void*) unsigned-32)

;; The DOS attribute word for PATH, or #f when it cannot be read (the path does
;; not exist, or this is not Windows). Win32 takes "/" as a separator as happily
;; as "\\", so the "/"-rendered paths the Path shim hands out need no rewriting.
(define (win32-file-attributes path)
  (let ((f (win32-get-file-attributes-w)))
    (and f
         (let ((a (win32-with-wstr path
                    (lambda (w) (guard (e (#t win32-INVALID-FILE-ATTRIBUTES)) (f w))))))
           (and (not (= a win32-INVALID-FILE-ATTRIBUTES)) a)))))

;; A FILETIME: 100-nanosecond intervals since 1601-01-01 UTC, which is
;; 11644473600 seconds before the Unix epoch. Pure, so the conversion is pinned
;; from any host (test/chez/win-platform-test.ss).
;; Converted at the FILETIME's own resolution, which is what a FileTime carries:
;; a nanosecond count loses only its last two digits on the way out.
(define win32-epoch-offset-ticks 116444736000000000)
(define (unix-ns->filetime ns) (+ (div ns 100) win32-epoch-offset-ticks))
(define (filetime->unix-ns ft) (* (- ft win32-epoch-offset-ticks) 100))

(define win32-FILE-READ-ATTRIBUTES       #x80)
(define win32-FILE-WRITE-ATTRIBUTES      #x100)
(define win32-FILE-SHARE-ALL             #x7)          ; read | write | delete
(define win32-OPEN-EXISTING              3)
(define win32-FILE-FLAG-BACKUP-SEMANTICS #x02000000)   ; what opens a directory
(define win32-FILE-FLAG-OPEN-REPARSE-POINT #x00200000) ; the link itself, not its target
(define win32-FILE-ATTRIBUTE-REPARSE-POINT #x400)
(define win32-INVALID-HANDLE-VALUE       -1)

;; CreateFileW follows a symbolic link unless told not to, so the handle a time
;; is read or set through names the link's TARGET by default. NOFOLLOW_LINKS asks
;; for the link itself: FILE_FLAG_OPEN_REPARSE_POINT, as the JDK's
;; WindowsPath.openFor*AttributeAccess(followLinks=false) passes.
(define (win32-attr-open-flags follow?)
  (if follow?
      win32-FILE-FLAG-BACKUP-SEMANTICS
      (bitwise-ior win32-FILE-FLAG-BACKUP-SEMANTICS win32-FILE-FLAG-OPEN-REPARSE-POINT)))
;; GetFileAttributesExW never follows a reparse point: on a link it answers the
;; link's own times. That is the NOFOLLOW answer, and for any path that is not a
;; reparse point the only answer; a FOLLOW read of a link has to open it.
(define (win32-times-need-handle? attrs follow?)
  (and follow? (not (= 0 (bitwise-and attrs win32-FILE-ATTRIBUTE-REPARSE-POINT)))))

(define-win32-proc win32-create-file-w
  "kernel32.dll" "CreateFileW" (void* unsigned-32 unsigned-32 void* unsigned-32 unsigned-32 void*) iptr)
(define-win32-proc win32-set-file-time
  "kernel32.dll" "SetFileTime" (iptr u8* u8* u8*) int)
(define-win32-proc win32-close-handle
  "kernel32.dll" "CloseHandle" (iptr) int)
(define-win32-proc win32-get-file-time
  "kernel32.dll" "GetFileTime" (iptr u8* u8* u8*) int)

;; Open PATH for ACCESS with the link-following flags, run PROC on the handle and
;; close it. #f when the open fails or this is not Windows.
(define (win32-with-attr-handle path access follow? proc)
  (let ((create (win32-create-file-w)) (close (win32-close-handle)))
    (and create close
         (let ((h (win32-with-wstr path
                    (lambda (w)
                      (create w access win32-FILE-SHARE-ALL 0
                              win32-OPEN-EXISTING (win32-attr-open-flags follow?) 0)))))
           (and (not (= h win32-INVALID-HANDLE-VALUE))
                (dynamic-wind (lambda () #f) (lambda () (proc h)) (lambda () (close h))))))))

;; Files.setLastModifiedTime / setAttribute on Windows, as WindowsFileAttributeViews
;; does it: open the path for FILE_WRITE_ATTRIBUTES — with
;; FILE_FLAG_BACKUP_SEMANTICS, the flag that lets CreateFile open a directory at
;; all, and FILE_FLAG_OPEN_REPARSE_POINT when not following a link — and set just
;; the times given. Each of CREATION, ACCESS and WRITE is epoch NANOSECONDS or #f,
;; and a #f slot passes NULL, which SetFileTime leaves alone. Answers whether they
;; were set; #f off Windows.
(define (win32-set-file-times! path creation access write follow?)
  (let ((set-time (win32-set-file-time)))
    (define (ft ns)
      (and ns (let ((b (make-bytevector 8 0)))
                (bytevector-u64-set! b 0 (unix-ns->filetime ns) (endianness little))
                b)))
    (and set-time
         (win32-with-attr-handle path win32-FILE-WRITE-ATTRIBUTES follow?
           (lambda (h) (not (= 0 (set-time h (ft creation) (ft access) (ft write)))))))))

;; Files.createLink on Windows: CreateHardLinkW(new, existing, NULL). Answers
;; whether the link was made; #f off Windows.
(define-win32-proc win32-create-hard-link-w
  "kernel32.dll" "CreateHardLinkW" (void* void* void*) int)
(define (win32-create-hard-link! link existing)
  (let ((f (win32-create-hard-link-w)))
    (and f
         (win32-with-wstr link
           (lambda (l) (win32-with-wstr existing
                         (lambda (e) (not (= 0 (f l e 0))))))))))

;; The three times of PATH as a vector #(creation access write) of epoch
;; NANOSECONDS (100ns resolution), or #f. WIN32_FILE_ATTRIBUTE_DATA is the
;; attribute word and then three FILETIMEs, each two DWORDs — at 4, 12 and 20, so
;; not 8-aligned, and read as two halves. FOLLOW? on a reparse point reads the
;; target's through a handle (GetFileTime), since the attribute data describes
;; the link itself.
(define-win32-proc win32-get-file-attributes-ex-w
  "kernel32.dll" "GetFileAttributesExW" (void* int u8*) int)
(define (win32-filetime-at buf off)
  (filetime->unix-ns
   (+ (bytevector-u32-ref buf off (endianness little))
      (* (bytevector-u32-ref buf (+ off 4) (endianness little)) #x100000000))))
(define (win32-file-times path follow?)
  (let ((f (win32-get-file-attributes-ex-w)))
    (and f
         (let ((buf (make-bytevector 36 0)))
           (and (win32-with-wstr path
                  (lambda (w) (guard (e (#t #f)) (not (= 0 (f w 0 buf))))))  ; GetFileExInfoStandard
                (if (win32-times-need-handle? (bytevector-u32-ref buf 0 (endianness little)) follow?)
                    (let ((get-time (win32-get-file-time)))
                      (and get-time
                           (win32-with-attr-handle path win32-FILE-READ-ATTRIBUTES #t
                             (lambda (h)
                               (let ((c (make-bytevector 8 0)) (a (make-bytevector 8 0))
                                     (m (make-bytevector 8 0)))
                                 (and (not (= 0 (get-time h c a m)))
                                      (vector (win32-filetime-at c 0) (win32-filetime-at a 0)
                                              (win32-filetime-at m 0))))))))
                    (vector (win32-filetime-at buf 4) (win32-filetime-at buf 12)
                            (win32-filetime-at buf 20))))))))

;; The ROOT of P — the prefix that is not a segment and must be reproduced
;; verbatim — and the index the segments start at. Rendered with "/" separators,
;; the spelling getAbsolutePath and babashka.fs/absolutize already answer with
;; on Windows, so canonicalize agrees with its neighbours and a path's identity
;; no longer depends on which separator the caller typed.
;;
;;   POSIX    "/a/b"                -> "/"                 UNC   "//srv/sh/a" -> "//srv/sh/"
;;   drive    "C:/a"  "C:\a"        -> "C:/"               rooted "/a"        -> "/"
;;   drive-relative "C:a"           -> "C:"                relative "a/b"     -> ""
;;
;; A drive-relative path keeps its "C:" and gains no separator: it names the
;; per-drive current directory, which this process cannot see, so the honest
;; answer is to hand back the same relative meaning the caller passed in rather
;; than to invent a root. jolt.deps rejects that form outright because it has to
;; produce a path it can then read; the JVM's canonicalizer resolves it against
;; the drive, and leaving it alone is the closest we can get to that.
(define (path-root-end windows? p)
  (let ((n (string-length p)))
    (define (sep? i) (and (< i n) (path-sep-for? windows? (string-ref p i))))
    (cond
      ((not windows?) (if (sep? 0) 1 0))
      ((and (>= n 2) (windows-drive-prefix? p)) (if (sep? 2) 3 2))
      ;; UNC or device: "\\server\share", "\\?\C:\x". The first two segments
      ;; after the leading pair are part of the root, not children of it.
      ((and (sep? 0) (sep? 1))
       (let* ((seg-end (lambda (i)
                         (let loop ((j i)) (if (or (>= j n) (sep? j)) j (loop (+ j 1))))))
              (skip-seps (lambda (i) (let loop ((j i)) (if (sep? j) (loop (+ j 1)) j))))
              (a (seg-end (skip-seps 2)))
              (b (seg-end (skip-seps a))))
         b))
      ((sep? 0) 1)
      (else 0))))

;; The root as a string, separators normalized to "/" and one trailing "/" kept
;; when the root is a directory prefix ("C:/", "//srv/sh/", "/") rather than a
;; drive-relative "C:".
(define (path-root-from windows? p end)
  (let ((raw (substring p 0 end)))
    (cond
      ((= end 0) "")
      ((and windows? (= end 2) (windows-drive-prefix? p)) raw)  ; "C:" — drive-relative
      (else
       (let ((out (make-string (string-length raw))))
         (do ((i 0 (+ i 1))) ((= i (string-length raw)))
           (string-set! out i (if (path-sep-for? windows? (string-ref raw i))
                                  #\/
                                  (string-ref raw i))))
         (let ((s (if (char=? (string-ref out (- (string-length out) 1)) #\/)
                      out
                      (string-append out "/"))))
           s))))))
(define (path-root windows? p)
  (path-root-from windows? p (path-root-end windows? p)))

;; The non-empty segments under the root. Empty ones (a doubled separator) are
;; dropped here, which is what the JVM's normalize does to them anyway.
(define (path-segments-from windows? p from)
  (let ((n (string-length p)))
    (let loop ((i from) (start from) (acc '()))
      (cond
        ((= i n) (reverse (if (> i start) (cons (substring p start i) acc) acc)))
        ((path-sep-for? windows? (string-ref p i))
         (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring p start i) acc) acc)))
        (else (loop (+ i 1) start acc))))))
(define (path-segments windows? p)
  (path-segments-from windows? p (path-root-end windows? p)))

;; A path PARSED once: its root, its segments, and the platform they were read
;; for. Every helper below used to take the string and re-derive both, and
;; path-root / path-segments each begin with their own path-root-end scan — so a
;; single Path method could scan the same string up to seven times (endsWith
;; against a rooted other normalizes both sides, and each normalize is a root
;; plus a segment split). The scan runs once here and the parts are read as
;; fields instead (jolt-2sp).
;;
;; This deliberately does NOT reach jolt-path-normalize, which every jfile runs
;; through and which a directory listing runs per entry: that one is a character
;; walk that allocates nothing for an already-normal path, it never asks for
;; segments, and turning it into a parse-and-render would allocate a record and
;; a segment list per file. It keeps its own shape for that reason.
;;
;; The platform is NOT a field. It reads like it should be one -- the parse is
;; platform-specific, so the result "belongs to" a platform -- but nothing would
;; ever read it back: every helper below already takes `windows?` as a parameter,
;; which is what lets a Linux runner pin the Windows rows
;; (test/chez/win-platform-test.ss). A field no caller reads is weight on every
;; parse and one more thing to keep true, so the parser takes the platform and
;; the value keeps only what the platform decided.
(define-record-type ppath
  (fields root segs)
  (nongenerative jolt-ppath-v1))

(define (path-parse windows? p)
  (let ((end (path-root-end windows? p)))
    (make-ppath (path-root-from windows? p end)
                (path-segments-from windows? p end))))

(define (ppath-rooted? pp) (not (string=? (ppath-root pp) "")))

;; Re-render, optionally over a different segment list — the shape every
;; consumer wants: parse, transform the segments, render.
(define (ppath-render pp segs) (path-rebuild (ppath-root pp) segs))

(define (path-rebuild root segs)
  (cond
    ((null? segs) (if (string=? root "") "." root))
    (else
     ;; ONE allocation for the whole path. This appended per segment, and each
     ;; append copies the answer built so far -- quadratic in the number of
     ;; segments, on the function every getter below ends in. A path is short
     ;; enough that the constant hid it, but rebuilding was measurably the most
     ;; expensive thing in the Path algebra, more than the scanning above it.
     ;;
     ;; No separator before the FIRST segment: a root either ends in one ("/",
     ;; "C:/", "//srv/sh/") or must not gain one ("C:" is drive-relative, and
     ;; "C:a" names a different file from "C:/a"). The old spelling also tested
     ;; (string=? out "") for that, which could only ever be true on the first
     ;; segment and so said the same thing twice.
     (apply string-append root
            (cons (car segs)
                  (let loop ((ss (cdr segs)) (acc '()))
                    (if (null? ss)
                        (reverse acc)
                        (loop (cdr ss) (cons (car ss) (cons "/" acc))))))))))

;; Fold "." and ".." lexically. Only ever applied to a part of a path that does
;; NOT exist: where a component is real, realpath resolves it instead, because
;; POSIX (and the JVM) resolve ".." AFTER following the link before it, and
;; folding it lexically there would give a different -- wrong -- directory.
(define (fold-dot-segments segs)
  (let loop ((ss segs) (out '()))
    (cond
      ((null? ss) (reverse out))
      ((string=? (car ss) ".") (loop (cdr ss) out))
      ((string=? (car ss) "..") (loop (cdr ss) (if (null? out) out (cdr out))))
      (else (loop (cdr ss) (cons (car ss) out))))))

(define (jfile-fold-dots-for windows? p)
  (let ((pp (path-parse windows? p)))
    (ppath-render pp (fold-dot-segments (ppath-segs pp)))))

;; The JVM canonicalizes a path whose tail does not exist -- on a host where
;; /tmp is a link, new File("/tmp/nope").getCanonicalPath is
;; "/private/tmp/nope" -- while realpath(3) fails outright on ENOENT. So
;; resolve the longest existing ancestor and re-attach what is left.
;; REALPATH is a parameter (#f-answering, like jfile-realpath) so the walk can
;; be driven from a test without a filesystem, and so the Windows rows -- where
;; the host has no realpath at all and this is the entire implementation -- are
;; reachable from a POSIX host.
;; ANCESTOR-REALPATH resolves the components the walk descends through, and is
;; where a can-never-name-a-file failure raises; REALPATH answers for the whole
;; path, where such a failure is not an error on the JVM. The 3-argument form
;; uses one procedure for both, which is what a driver with no errno to read
;; wants (win-path-test.ss) and what the Windows fallback is.
(define jfile-canonical-for
  (case-lambda
    ((windows? realpath p)
     (jfile-canonical-for windows? realpath realpath p))
    ((windows? realpath ancestor-realpath p)
     (or (realpath p)
         (let* ((pp (path-parse windows? p))
                (root (ppath-root pp))
                (segs (ppath-segs pp)))
           (let loop ((n (- (length segs) 1)))
             (cond
               ((< n 0) (jfile-fold-dots-for windows? p))
               (else
                (let ((rp (ancestor-realpath (path-rebuild root (list-head segs n)))))
                  (if rp
                      (let ((rpp (path-parse windows? rp)))
                        (jfile-fold-dots-for
                         windows?
                         (ppath-render rpp (append (ppath-segs rpp) (list-tail segs n)))))
                      (loop (- n 1))))))))))))

;; --- getCanonicalPath and toRealPath on Windows -----------------------------
;; Windows has no realpath, and the two JDK operations it stands for differ
;; there. File.getCanonicalPath (WinNTFileSystem's canonicalize) folds "." and
;; ".." lexically and then replaces each existing component with the name the
;; directory lists for it, which is what turns an 8.3 alias like MARKO~1.KOC back
;; into Marko.Kocic and fixes the case; it does not follow links. Path.toRealPath
;; asks the filesystem for the final path of an opened handle, which does follow
;; them. Either way a path holding a short name is spelled long, so two spellings
;; of one directory compare equal (jolt-lang/jolt#1281).

;; The canonical spelling of absolute path P, or #f when some component does not
;; exist. FIND answers the listed name of the last component of the path it is
;; given, or #f. A parameter so the walk is pinned from a POSIX host
;; (test/chez/win-path-test.ss); win32-long-path passes FindFirstFileW. The drive
;; letter is upper-cased, as the JDK spells it. A "*" or "?" in a component is a
;; FindFirstFileW wildcard and names no single file.
(define (win32-long-path-with find p)
  (let* ((pp (path-parse #t (jfile-fold-dots-for #t p)))
         (root (let ((r (ppath-root pp)))
                 (if (windows-drive-prefix? r)
                     (string-append (string (char-upcase (string-ref r 0))) (substring r 1 (string-length r)))
                     r))))
    (and (not (string=? root ""))
         (let loop ((segs (ppath-segs pp)) (done '()))
           (if (null? segs)
               (path-rebuild root (reverse done))
               (let ((seg (car segs)))
                 (and (not (exists (lambda (c) (memv c '(#\* #\?))) (string->list seg)))
                      (let ((nm (find (path-rebuild root (reverse (cons seg done))))))
                        (and nm (loop (cdr segs) (cons nm done)))))))))))

;; WIN32_FIND_DATAW: cFileName, MAX_PATH UTF-16 units, starts at 44.
(define win32-find-data-size 592)
(define win32-find-data-name-offset 44)
(define-win32-proc win32-find-first-file-w
  "kernel32.dll" "FindFirstFileW" (void* void*) iptr)
(define-win32-proc win32-find-close
  "kernel32.dll" "FindClose" (iptr) int)

;; A NUL-terminated UTF-16LE string at foreign address P, offset OFF, at most
;; MAX units.
(define (win32-wstr-at p off max)
  (let loop ((i 0))
    (if (or (= i max) (= 0 (sa-foreign-ref 'unsigned-16 p (+ off (* 2 i)))))
        (let ((bv (make-bytevector (* 2 i))))
          (do ((j 0 (+ j 1))) ((= j (* 2 i)))
            (bytevector-u8-set! bv j (sa-foreign-ref 'unsigned-8 p (+ off j))))
          (utf16->string bv (endianness little)))
        (loop (+ i 1)))))

;; The name the directory lists for the last component of PATH, or #f.
(define (win32-listed-name path)
  (let ((ff (win32-find-first-file-w)) (fc (win32-find-close)))
    (and ff fc
         (let ((data (sa-foreign-alloc win32-find-data-size)))
           (dynamic-wind
             (lambda () #f)
             (lambda ()
               (let ((h (win32-with-wstr path (lambda (w) (ff w data)))))
                 (and (not (= h win32-INVALID-HANDLE-VALUE))
                      (let ((nm (win32-wstr-at data win32-find-data-name-offset 260)))
                        (fc h)
                        (and (not (string=? nm "")) nm)))))
             (lambda () (sa-foreign-free data)))))))

(define (win32-long-path p) (win32-long-path-with win32-listed-name p))

;; "\\?\C:\x" -> "C:\x" and "\\?\UNC\srv\sh" -> "\\srv\sh": the
;; prefixes GetFinalPathNameByHandleW reports a DOS path under.
(define (win32-strip-final-prefix s)
  (cond ((and (>= (string-length s) 8) (string=? (substring s 0 8) "\\\\?\\UNC\\"))
         (string-append "\\\\" (substring s 8 (string-length s))))
        ((and (>= (string-length s) 4) (string=? (substring s 0 4) "\\\\?\\"))
         (substring s 4 (string-length s)))
        (else s)))

(define-win32-proc win32-get-final-path-name-by-handle-w
  "kernel32.dll" "GetFinalPathNameByHandleW" (iptr void* unsigned-32 unsigned-32) unsigned-32)

;; Path.toRealPath on Windows: the final path of PATH opened (a directory too,
;; through FILE_FLAG_BACKUP_SEMANTICS) with its links followed, or #f.
(define (win32-final-path path)
  (let ((gf (win32-get-final-path-name-by-handle-w)))
    (and gf
         (win32-with-attr-handle path 0 #t
           (lambda (h)
             (let retry ((units 1024))
               (let ((buf (sa-foreign-alloc (* 2 units))))
                 (let ((n (gf h buf units 0)))
                   (cond ((= n 0) (sa-foreign-free buf) #f)
                         ((>= n units) (sa-foreign-free buf) (retry (+ n 1)))
                         (else
                          (let ((s (win32-wstr-at buf 0 n)))
                            (sa-foreign-free buf)
                            (win32-strip-final-prefix s))))))))))))

(define (jfile-canonical p)
  (let ((abs (jfile-abs p)))
    (jfile-nul-free! abs)
    (jfile-canonical-for (eq? (sa-os-family) 'windows)
                         jfile-realpath jfile-realpath-ancestor abs)))

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
;; utimes(2) is the fallback for a host without utimensat: struct timeval is
;; sec + usec, 16 bytes each on the 64-bit platforms Chez targets. Resolved via jolt-foreign-proc-safe — a literal foreign-procedure here is a
;; fasl relocation that aborts the boot on platforms lacking the symbol.
;; Windows has no utimes, and its CRT's _utime64 is no substitute: it opens the
;; path without FILE_FLAG_BACKUP_SEMANTICS, so it cannot open a DIRECTORY and a
;; directory's mtime was never set (jolt-lang/jolt#1119) — and it has second
;; resolution. win32-set-file-times! (SetFileTime) is what the JDK's Windows
;; provider does. Answers whether the time was set.
(define c-utimes (jolt-foreign-proc-safe "utimes" '(string u8*) 'int))
;; utimensat(2) sets either time alone (UTIME_OMIT in the other slot) at
;; nanosecond resolution, and can leave a symbolic link's target alone
;; (AT_SYMLINK_NOFOLLOW). Setting the mtime through utimes moved the access time
;; to it as well, where the JDK keeps the access time as it was, for
;; File.setLastModified and Files.setLastModifiedTime both. The constants are
;; per-OS, measured with cc/gcc: #(AT_FDCWD UTIME_OMIT AT_SYMLINK_NOFOLLOW).
(define c-utimensat (jolt-foreign-proc-safe "utimensat" '(int string u8* int) 'int))
(define utimensat-consts
  (case (sa-os-family)
    ((linux) '#(-100 1073741822 #x100))
    ((macos) '#(-2 -2 #x20))
    (else #f)))
;; A struct timespec of epoch NS at OFF: seconds floored, so a time before the
;; epoch keeps its nanoseconds field in [0, 1e9).
(define (timespec-bytes! bv off ns)
  (bytevector-s64-set! bv off (div ns 1000000000) (native-endianness))
  (bytevector-s64-set! bv (+ off 8) (mod ns 1000000000) (native-endianness)))
;; Set P's access and/or modification time, each epoch NS or #f to leave it as
;; it is. FOLLOW? #f sets a symbolic link's own. Answers whether it was set.
(define (set-file-times-ns! p atime mtime follow?)
  (cond
    ((eq? (sa-os-family) 'windows) (win32-set-file-times! p #f atime mtime follow?))
    ((and c-utimensat utimensat-consts)
     (let ((ts (make-bytevector 32 0)) (k utimensat-consts))
       (if atime (timespec-bytes! ts 0 atime)
           (bytevector-s64-set! ts 8 (vector-ref k 1) (native-endianness)))
       (if mtime (timespec-bytes! ts 16 mtime)
           (bytevector-s64-set! ts 24 (vector-ref k 1) (native-endianness)))
       (= 0 (c-utimensat (vector-ref k 0) p ts (if follow? 0 (vector-ref k 2))))))
    ;; no utimensat: utimes, which can only set both, so both get the one given
    ((and c-utimes follow? (or mtime atime))
     (let ((tv (make-bytevector 32 0)) (t (or mtime atime)))
       (define (tv! off ns)
         (bytevector-s64-set! tv off (div ns 1000000000) (native-endianness))
         (bytevector-s64-set! tv (+ off 8) (div (mod ns 1000000000) 1000) (native-endianness)))
       (tv! 0 (or atime t))
       (tv! 16 (or mtime t))
       (= (c-utimes p tv) 0)))
    (else #f)))
(define (set-file-mtime-millis! p ms)
  (set-file-times-ns! p #f (* (exact (floor ms)) 1000000) #t))
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
;; delete a file or an (empty) directory; #t on success, #f on failure. Without
;; their error? argument, delete-file and delete-directory answer that boolean
;; themselves and raise nothing (CSUG 9.16), so both branches pass it on.
;; A symbolic link is deleted itself, as File.delete does: the checks do not
;; follow it, or a dangling link read as absent and a link to a directory went
;; to rmdir, and both stayed on disk.
(define (delete-path! p)
  (guard (e (#t #f))
    (cond ((not (file-exists? p #f)) #f)
          ((file-directory? p #f) (delete-directory p))
          (else (delete-file p)))))

;; rename(2) REPLACES an existing destination. Chez's rename-file is MoveFile on
;; Windows, which refuses one — "cannot rename A to B: file exists" — so every
;; publish-by-rename here failed the moment its target already existed: the
;; SECOND spit to a path, the first spit to a File/createTempFile target, and
;; every AOT or classpath artifact written a second time (jolt-lang/jolt#1074).
;; Those callers all mean the POSIX semantic, so drop the destination first
;; there. That opens a window where the target is gone and the new content is
;; not in place yet; it is Windows-only, and still far narrower than the
;; truncate-in-place write the staged rename replaced.
;;
;; java.io.File.renameTo keeps the bare rename-file: the JVM documents it as
;; platform-dependent and it fails over an existing destination on Windows too,
;; so matching it IS the shim's job.
;;
;; On Windows the rename is also retried. Real-time AV (Defender's MsMpEng) opens
;; a freshly closed file to scan it, and while that handle is open MoveFile on it
;; fails with ERROR_ACCESS_DENIED — "cannot rename ...: permission denied". A
;; publish-by-rename runs microseconds after its close, the worst moment for that
;; race, so spit failed intermittently and left its temp file behind
;; (jolt-lang/jolt#1263). The scan finishes in milliseconds; this is the bounded
;; retry write-file-atomic settled on for the same failure. It retries only while
;; the source is still there and the destination is not a directory, so a rename
;; that can never succeed fails as fast as it did.
(define fs-retry-delays-ms '(10 25 50 100 200 400))
(define (call-with-fs-retry thunk retry?)
  (let loop ((delays fs-retry-delays-ms))
    (if (null? delays)
        (thunk)
        (guard (e ((retry? e)
                   (sleep (make-time 'time-duration (* (car delays) 1000000) 0))
                   (loop (cdr delays))))
          (thunk)))))
(define (rename-replace! from to)
  (if (eq? (sa-os-family) 'windows)
      (call-with-fs-retry
       (lambda ()
         (when (file-exists? to) (delete-file to #t))
         (rename-file from to))
       (lambda (e) (and (file-exists? from) (not (file-directory? to)))))
      (rename-file from to)))
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
;; The filesystem path a URL names for WRITING. Reading a URL is broad — a stream
;; handler decides, and url-content knows several protocols — but there is nowhere
;; to write anything except a file: one, so every other protocol is the JVM's
;; IllegalArgumentException. Without this a URL reached the path coercions as its
;; SPEC, and (spit (.toURL f) …) created a file literally named "file:/…/f" under
;; the working directory instead of writing f.
(define (url-write-path u)
  (let ((spec (url-spec u)))
    (if (string=? (url-protocol spec) "file")
        (file-url->path spec)
        (throw-jvm (quote IllegalArgumentException)
                   (string-append "Can not write to non-file URL <" spec ">")))))

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
;; A relative spec resolves against the context, as the protocol's handler
;; parses it (URLStreamHandler.parseURL, and the jar handler's own for a jar:
;; context); an absolute one ignores the context, as on the JVM. The context is
;; either kind of java.net.URL: a "url" jhost or a resource in the image's
;; archive.
(define (url-resolve-spec context spec)
  (if (or (jolt-nil? context) (not context)
          ;; absolute: it carries its own scheme
          (let ((i (proto-colon-index spec))) (and i (> i 0))))
      spec
      (let* ((base (cond ((url-jhost? context) (url-spec context))
                         ((embedded-res? context) (embedded-res-url context))
                         (else (throw-jvm 'ClassCastException
                                          (string-append "class " (jolt-class-name context)
                                                         " cannot be cast to class java.net.URL")))))
             (hash (url-char-index spec #\# 0))
             (ref (and hash (substring spec (+ hash 1) (string-length spec))))
             (spec (if hash (substring spec 0 hash) spec))
             (base-hash (url-char-index base #\# 0))
             (bare (if base-hash (substring base 0 base-hash) base))
             (resolved (cond ((and (string=? spec "") ref) bare)       ; a new ref, the rest kept
                             ((string=? (url-protocol base) "jar") (url-resolve-jar bare spec))
                             ((string=? spec "") base)                   ; the context, ref and all
                             (else (url-resolve-hierarchical bare spec)))))
        (if ref (string-append resolved "#" ref) resolved))))
(define (url-char-index s c from)
  (let loop ((i from))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) c) i)
          (else (loop (+ i 1))))))
(define (url-last-index s c)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) #f) ((char=? (string-ref s i) c) i) (else (loop (- i 1))))))
(define (url-string-index s sub from)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i from))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) i)
            (else (loop (+ i 1)))))))
;; The "/" of the last "!/" (the jar handler's indexOfBangSlash), or #f.
(define (url-bang-slash s)
  (let loop ((i (- (string-length s) 2)))
    (cond ((< i 0) #f)
          ((and (char=? (string-ref s i) #\!) (char=? (string-ref s (+ i 1)) #\/)) (+ i 1))
          (else (loop (- i 1))))))
(define (url-ends-with? s suffix)
  (let ((n (string-length s)) (m (string-length suffix)))
    (and (>= n m) (string=? (substring s (- n m) n) suffix))))
;; jar: (the jar handler's parseContextSpec, then ParseUtil.canonizeString over
;; what follows the "!/"): a spec starting "/" replaces the entry, anything
;; else is resolved against the entry's directory, and "." and ".." segments go.
(define (url-resolve-jar base spec)
  (define (malformed msg) (jolt-throw (jolt-host-throwable "java.net.MalformedURLException" msg)))
  (let* ((ctx (substring base 4 (string-length base)))
         (ctx (if (and (> (string-length spec) 0) (char=? (string-ref spec 0) #\/))
                  (let ((bang (url-bang-slash ctx)))
                    (unless bang (malformed (string-append "malformed context url:" base ": no !/")))
                    (substring ctx 0 bang))
                  (let ((slash (url-last-index ctx #\/)))
                    (unless slash (malformed (string-append "malformed context url:" base)))
                    (substring ctx 0 (+ slash 1)))))
         (file (string-append ctx spec))
         (bang (or (url-bang-slash file) (malformed "no !/ in spec"))))
    (string-append "jar:" (substring file 0 bang)
                   (url-canonize-jar (substring file bang (string-length file))))))
(define (url-canonize-jar file)
  (let* ((file (let loop ((file file))
                 (let ((i (url-string-index file "/../" 0)))
                   (if (not i)
                       file
                       (let ((lim (and (> i 0) (url-last-index (substring file 0 i) #\/))))
                         (loop (if lim
                                   (string-append (substring file 0 lim) (substring file (+ i 3) (string-length file)))
                                   (substring file (+ i 3) (string-length file)))))))))
         (file (let loop ((file file))
                 (let ((i (url-string-index file "/./" 0)))
                   (if i
                       (loop (string-append (substring file 0 i) (substring file (+ i 2) (string-length file))))
                       file))))
         (file (let loop ((file file))
                 (if (url-ends-with? file "/..")
                     (let* ((i (url-string-index file "/.." 0))
                            (lim (and (> i 0) (url-last-index (substring file 0 i) #\/))))
                       (loop (if lim (substring file 0 (+ lim 1)) (substring file 0 i))))
                     file))))
    (if (url-ends-with? file "/.") (substring file 0 (- (string-length file) 1)) file)))
;; Every other protocol (URLStreamHandler.parseURL): "//" brings its own
;; authority, a spec starting "/" replaces the path, "?" only the query, and a
;; relative path joins the context's directory; then "." segments go and each
;; ".." takes the segment before it, as far as there is one.
(define (url-resolve-hierarchical base spec)
  (let* ((colon (url-char-index base #\: 0))
         (scheme (substring base 0 (+ colon 1)))
         (rest (substring base (+ colon 1) (string-length base)))
         (auth-end (if (and (>= (string-length rest) 2) (string=? (substring rest 0 2) "//"))
                       (let loop ((i 2))
                         (cond ((>= i (string-length rest)) i)
                               ((memv (string-ref rest i) '(#\/ #\?)) i)
                               (else (loop (+ i 1)))))
                       0))
         (authority (substring rest 0 auth-end))
         (rest (substring rest auth-end (string-length rest)))
         (q (url-char-index rest #\? 0))
         (base-path (if q (substring rest 0 q) rest))
         (base-query (and q (substring rest q (string-length rest))))
         (sq (url-char-index spec #\? 0))
         (spec-path (if sq (substring spec 0 sq) spec))
         (spec-query (and sq (substring spec sq (string-length spec)))))
    (cond
      ((and (>= (string-length spec) 2) (string=? (substring spec 0 2) "//"))
       (string-append scheme spec))
      ;; a query alone keeps the context's directory
      ((string=? spec-path "")
       (let ((slash (url-last-index base-path #\/)))
         (string-append scheme authority (if slash (substring base-path 0 (+ slash 1)) base-path)
                        (or spec-query base-query ""))))
      (else
       (let ((path (if (char=? (string-ref spec-path 0) #\/)
                       spec-path
                       (let ((slash (url-last-index base-path #\/)))
                         (string-append (if slash (substring base-path 0 (+ slash 1))
                                            (if (string=? authority "") "" "/"))
                                        spec-path)))))
         (string-append scheme authority (url-normalize-path path) (or spec-query "")))))))
(define (url-normalize-path path)
  (let* ((path (let loop ((path path))
                 (let ((i (url-string-index path "/./" 0)))
                   (if i
                       (loop (string-append (substring path 0 i) (substring path (+ i 2) (string-length path))))
                       path))))
         (path (let loop ((path path) (from 0))
                 (let ((i (url-string-index path "/../" from)))
                   (if (not i)
                       path
                       (let ((lim (and (> i 0) (url-last-index (substring path 0 i) #\/))))
                         ;; the JDK's test, verbatim: only a "/../" at the very start stays
                         (if (and lim (not (eqv? (url-string-index path "/../" lim) 0)))
                             (loop (string-append (substring path 0 lim) (substring path (+ i 3) (string-length path))) 0)
                             (loop path (+ i 3))))))))
         (path (let loop ((path path))
                 (if (url-ends-with? path "/..")
                     (let* ((i (url-string-index path "/.." 0))
                            (lim (and (> i 0) (url-last-index (substring path 0 i) #\/))))
                       (if lim (loop (substring path 0 (+ lim 1))) path))
                     path)))
         (path (if (and (> (string-length path) 2) (string=? (substring path 0 2) "./"))
                   (substring path 2 (string-length path))
                   path)))
    (if (url-ends-with? path "/.") (substring path 0 (- (string-length path) 1)) path)))
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
     (let ((spec (jolt-str-render-one (car args))))
       ;; a URL into the image's own archive is the resource it names
       (or (embedded-jar-path-res spec)
           (make-url (url-canonical spec)))))
    ;; (URL. context spec [handler])
    (else
     (let* ((context (car args))
            (spec (jolt-str-render-one (cadr args)))
            (handler (and (>= (length args) 3) (caddr args)))
            (resolved (url-resolve-spec context spec)))
       ;; a URL into the image's own archive is the resource it names, as the
       ;; one-argument form makes it
       (or (and (not handler) (embedded-jar-path-res resolved))
           (make-url (url-canonical resolved) handler))))))
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
        (cons "equals"         (lambda (self o) (and (url-jhost? o) (string=? (url-spec self) (url-spec o)))))
        (cons "hashCode"       (lambda (self) (java-string-hashcode (url-spec self))))
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
        (cons "openStream"     (lambda (self) (url-open-stream self)))
        ;; URL.toURI: the URI its text parses to
        (cons "toURI"          (lambda (self) (uri-ctor (url-spec self))))))
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
       (host-new "FileInputStream" (file-url->path spec)))
      ;; an entry of a jar on the roots streams out of the archive
      ((jar-path? spec) (jar-path-stream spec))
      (else (throw-jvm (quote java.io.IOException)
                       (string-append "protocol doesn't support input: " spec))))))
;; An InputStream over the entry a jar path names, as ZipFile.getInputStream
;; opens it; a path with no such entry is java.io.FileNotFoundException.
(define (jar-path-stream p)
  (let-values (((d ent) (jar-path-entry p)))
    (cond (ent (zipdir-entry-stream-owned d ent))
          ((embedded-jar-path-res p)
           => (lambda (r) (make-in-stream (open-bytevector-input-port (embedded-res-bytes r)))))
          (else (jar-path-missing p)))))
;; The handler's own openConnection. Without one there is nothing to connect
;; through — say so rather than returning something that reads as empty.
(define (url-open-connection u)
  (let ((h (url-handler u)))
    (cond
      (h (record-method-dispatch h "openConnection" (jolt-list u)))
      ;; a jar: URL is a JarURLConnection onto the jar (embedded-url.ss)
      ((jar-path? (url-spec u)) (embedded-url-open-connection u))
      (else (throw-jvm (quote java.io.IOException)
                       (string-append "no protocol handler for: " (url-spec u)))))))
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

;; File.setReadable/setWritable/setExecutable(enable [, ownerOnly]) and
;; setReadOnly, as the JDK's UnixFileSystem.setPermission does them: BIT is the
;; permission's "other" bit, widened to the owner's alone (ownerOnly, the
;; default) or to all three classes, then or'd in or masked out with chmod.
;; Answers whether the mode was changed; false for a missing file. On Windows
;; only the write bit means anything (the read-only attribute, which Chez's
;; chmod sets through _wchmod), and the JDK answers a read or execute change
;; with ENABLE itself. None of these existed, so every call raised.
(define (jfile-set-permission! fp bit args)
  (let ((enable? (and (pair? args) (jolt-truthy? (car args))))
        (owner-only? (or (not (pair? args)) (null? (cdr args)) (jolt-truthy? (cadr args)))))
    (guard (e (#t #f))
      (cond
        ((and (eq? (sa-os-family) 'windows) (not (= bit 2))) enable?)
        (else
         (let* ((m (bitwise-and (get-mode fp) #o7777))
                (a (if (and owner-only? (not (eq? (sa-os-family) 'windows)))
                       (* bit #o100)
                       (* bit #o111))))
           (chmod fp (if enable? (bitwise-ior m a) (bitwise-and m (bitwise-not a))))
           #t))))))

;; --- File method surface (record-method-dispatch arm) -----------------------
(define (jfile-method f name args)        ; -> boxed result, or #f to fall through
  (let ((p (jfile-path f))               ; the path as given (display methods)
        (fp (jfile-fs f)))               ; JOLT_PWD-resolved on-disk path (FS methods)
    (cond
      ((string=? name "getPath")        (list (path-native p)))
      ((string=? name "getName")        (list (path-last-segment p)))
      ((string=? name "toString")       (list (path-native p)))
      ((string=? name "getAbsolutePath")(list (path-native (jolt-path-normalize (jfile-abs fp)))))
      ((string=? name "getCanonicalPath")(list (path-native (jfile-canonical fp))))
      ;; File.toURI returns a java.net.URI (JVM), not a String.
      ((string=? name "toURI")          (list (jfile->uri fp)))
      ((string=? name "toURL")          (list (make-url (jfile->url-spec fp))))
      ;; a method that asks the file system, on a path holding a NUL
      ;; (jfile-invalid-answers); the path-only methods above skip the lookup
      ((and (jfile-nul? fp) (hashtable-contains? jfile-invalid-answers name))
       (list (hashtable-ref jfile-invalid-answers name #f)))
      ((string=? name "exists")         (list (if (file-exists? fp) #t #f)))
      ((string=? name "isDirectory")    (list (if (file-directory? fp) #t #f)))
      ((string=? name "isFile")         (list (if (and (file-exists? fp) (not (file-directory? fp))) #t #f)))
      ((string=? name "isAbsolute")     (list (if (jfile-path-absolute? p) #t #f)))
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
      ((string=? name "setReadable")    (list (jfile-set-permission! fp 4 args)))
      ((string=? name "setWritable")    (list (jfile-set-permission! fp 2 args)))
      ((string=? name "setExecutable")  (list (jfile-set-permission! fp 1 args)))
      ((string=? name "setReadOnly")    (list (jfile-set-permission! fp 2 (list #f #f))))
      ((string=? name "setLastModified")
       (list (guard (e (#t #f))
               (set-file-mtime-millis! fp (exact (floor (car args)))))))
      ((string=? name "createNewFile")
       (jfile-nul-free! fp)
       (list (if (file-exists? fp) #f
                 (guard (e (#t #f)) (close-port (open-output-file fp 'truncate)) #t))))
      ((string=? name "renameTo")
       (list (let ((dst (jfile-fs (car args))))
               (and (not (jfile-nul? dst)) (guard (e (#t #f)) (rename-file fp dst) #t)))))
      ((string=? name "getParentFile")
       (list (let ((parent (jfile-parent-path p)))
               (if parent (make-jfile parent) jolt-nil))))
      ((string=? name "toPath")           (list (make-nio-path p)))  ; -> java.nio.file.Path (nio-file.ss)
      ((string=? name "getAbsoluteFile")  (list (make-jfile (jfile-abs fp))))
      ((string=? name "getCanonicalFile") (list (make-jfile (jfile-canonical fp))))
      ((string=? name "compareTo")
       (unless (jfile? (car args))
         (throw-jvm 'ClassCastException
                    (string-append "class " (jolt-class-name (car args))
                                   " cannot be cast to class java.io.File")))
       (list (->num (jfile-compare-for (win32?) p (jfile-path (car args))))))
      ((string=? name "equals")         (list (and (jfile? (car args)) (string=? p (jfile-path (car args))))))
      ((string=? name "hashCode")       (list (jfile-hashcode f)))
      ((string=? name "getParent")
       (list (let ((parent (jfile-parent-path p))) (if parent (path-native parent) jolt-nil))))
      (else #f))))

(register-method-arm! arm-priority-file
  (lambda (obj method-name rest-args)
    (if (jfile? obj)
        (let* ((rest (method-rest-args->list rest-args))
               (r (jfile-method obj method-name rest)))
          (if r (car r) (dispatch-miss obj method-name rest)))
        'pass)))
;; An embedded resource shares the tier: io/resource returns one of these where a
;; source root would have yielded a jfile, so it has to answer the same methods.
(register-method-arm! arm-priority-file
  (lambda (obj method-name rest-args)
    (if (embedded-res? obj)
        (let* ((rest (method-rest-args->list rest-args))
               (r (embedded-res-method obj method-name rest)))
          (if r (car r) (dispatch-miss obj method-name rest)))
        'pass)))
(register-class-arm! embedded-res? (lambda (x) "java.net.URL"))
;; (str resource) is its jar: URL, like URL.toString — which also gives the
;; printer's #object[…] fallback its content.
(register-str-render! embedded-res? embedded-res-url)
(register-eq-arm! (lambda (a b) (and (embedded-res? a) (embedded-res? b)))
                  (lambda (a b) (string=? (embedded-res-name a) (embedded-res-name b))))
(register-hash-arm! embedded-res? (lambda (x) (string-hash (embedded-res-url x))))

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
;; The BYTES of a file, read into a buffer allocated once.
;;
;; get-bytevector-all was the whole cost of slurp: it grows its result as it
;; goes, so an 8.1 MB file cost 1745 ms against 1249 ms for the same read into a
;; buffer allocated ONCE -- slurp is among the most-called IO functions in
;; ordinary Clojure, and it was paying ~40% overhead on every call. file-length
;; gives the exact size, so ask for it once.
(define (read-file-bytes-sized p n)
  (let ((out (make-bytevector n)))
    (let loop ((at 0))
      (cond
        ((fx<? at n)
         (let ((k (get-bytevector-n! p out at (fx- n at))))
           (if (or (eof-object? k) (fx=? k 0))
               ;; shorter than file-length promised: truncated under the read
               (let ((short (make-bytevector at)))
                 (bytevector-copy! out 0 short 0 at)
                 short)
               (loop (fx+ at k)))))
        ;; The bound was reached, which normally means done. A file being
        ;; APPENDED to while it is read has more, and get-bytevector-all would
        ;; have taken it, so ask once rather than silently truncating.
        (else
         (let ((more (get-bytevector-all p)))
           (if (or (eof-object? more) (fx=? (bytevector-length more) 0))
               out
               (let* ((m (bytevector-length more))
                      (both (make-bytevector (fx+ n m))))
                 (bytevector-copy! out 0 both 0 n)
                 (bytevector-copy! more 0 both n m)
                 both))))))))

;; --- why an open failure is classified, and where ----------------------------
;; The JVM raises java.io.FileNotFoundException when a path cannot be opened, and
;; libraries branch on that class: instaparse decides whether its argument is a
;; grammar or a file by slurping and catching FNF. A raw Chez i/o condition is
;; not catchable as that class -- nor as any Java class -- so the caller's
;; fallback never runs. The message is the JVM's shape: the path AS GIVEN, then
;; the reason in parens. The JVM uses this one class for all the reasons, so only
;; the parenthetical distinguishes them.
;;
;; This lives in io.ss rather than next to the stream constructors because slurp
;; reaches a file through read-file-bytes-on-disk below, not through
;; io-streams.ss -- and while that was the only opener NOT classifying, slurp of
;; a directory, of an unreadable file and of anything at all under descriptor
;; exhaustion all came back as a bare java.io.IOException carrying Chez's own
;; wording (jolt-3ah).
;;
;; --- the reason comes from the condition, not from a second look at the disk ---
;; What the JVM puts in the parentheses is strerror(errno), and Chez's i/o
;; conditions carry that very string among their IRRITANTS, as
;; ("/the/path" "No such file or directory") -- and equally "Permission denied",
;; "Is a directory", "Too many open files". Both sides are printing the same libc
;; string, so the condition is what is asked.
;;
;; A probe of the filesystem cannot stand in for it. EMFILE is the case that
;; shows why: the open failed because the PROCESS is out of descriptors and
;; nothing is wrong with the path at all, so a probe finds a readable file and
;; blames the program's own leak on the file's mode bits. A parent directory
;; without +x is the same story inverted -- the open says "Permission denied" and
;; the probe cannot so much as stat the target, so it would say "No such file or
;; directory". And whatever the reason, the filesystem can change between the
;; failure and the probe.
;;
;; The IRRITANTS, not condition/report-string: Chez's EMFILE condition carries no
;; i/o-file-name, so report-string raises trying to format it -- and it raises
;; whether or not descriptors are still exhausted, so there is no waiting it out.
;; The reason is the LAST irritant; the first is the path Chez was handed, which
;; is not always the path being reported (spit opens a temp file and names its
;; target), so position picks it out rather than identity.
(define (io-open-reason exc)
  (and (pair? exc) (condition? (car exc))
       (let ((irr (guard (e2 (#t (quote ()))) (condition-irritants (car exc)))))
         (and (pair? irr) (pair? (cdr irr))
              (let last ((l irr))
                (cond ((pair? (cdr l)) (last (cdr l)))
                      ((and (string? (car l)) (fx>? (string-length (car l)) 0)) (car l))
                      (else #f)))))))

;; EXC is the condition the open raised, when there was one. The probes are the
;; fallback for the one caller with no condition to offer: open-path-guarded's
;; directory check, which refuses before it opens.
;; GIVEN is named the way a java.io.File built from it would name it: the JDK
;; opens a File, so (slurp "a//b") reports "a/b", and path-native alone left the
;; doubled separator in.
(define (file-open-error given resolved . exc)
  (throw-jvm (quote java.io.FileNotFoundException)
             (string-append (path-native (jolt-path-normalize given)) " ("
                            (or (io-open-reason exc)
                                (cond ((not (file-exists? resolved)) "No such file or directory")
                                      ((file-directory? resolved)    "Is a directory")
                                      (else                          "Permission denied")))
                            ")")))

;; Opening is guarded rather than pre-checked -- existence and permission can
;; change between a check and the open, and only the open itself is
;; authoritative. A DIRECTORY is the one case that needs the check: the JVM
;; refuses it at construction, while a Chez port over a directory opens fine and
;; raises on the first READ, far from the call that was wrong.
;;
;; That check is the only syscall this adds, and on the slurp path it replaces
;; one: slurp-path used to stat for existence up front and now lets the open
;; report it.
(define (open-path-guarded given resolved thunk)
  ;; a C path ends at a NUL, so opening this one would open another file
  (when (jfile-nul? resolved)
    (throw-jvm (quote java.io.FileNotFoundException) "Invalid file path"))
  (when (file-directory? resolved) (file-open-error given resolved))
  (guard (e ((i/o-error? e) (file-open-error given resolved e)))
    (thunk resolved)))

;; The text of a file: read the bytes, then decode them with the one decoder
;; every other byte->text seam uses (natives-str.ss utf8-bytes->string).
;;
;; This used to read through the PORT's transcoder instead, which is Chez's
;; UTF-8 codec and not java.nio's -- so the same bytes came back differently
;; from (slurp f) and from (String. (.readAllBytes ...)), and a file with a BOM
;; quietly lost its first character. Reading bytes is also the faster of the two
;; (8.1MB x20: 1150ms against the transcoder's 1234ms); the well-formedness
;; guard inside utf8-bytes->string spends that margin and about as much again,
;; for a net ~8%.
;;
;; THE LOADER READS SOURCE THROUGH HERE, so a .clj beginning with a UTF-8 BOM
;; now fails to read, exactly as it does on the JVM and on babashka
;; ("Unable to resolve symbol: <U+FEFF>"). Chez's codec used to swallow the BOM
;; and hide that, at the price of swallowing it out of DATA files too.
;; GIVEN, when passed, is the path as the caller spelled it, for the message: a
;; missing file is reported under that name, as the JVM's FileInputStream does,
;; rather than under the user.dir-resolved PATH this opens.
(define (read-file-string path . given)
  (utf8-bytes->string
   (if (jar-path? path)
       (or (jar-path-bytes path) (jar-path-missing path))
       (apply read-file-bytes-on-disk path given))))
(define (read-file-bytes-on-disk path . given)
  (with-port (open-path-guarded (if (pair? given) (car given) path) path (lambda (p) (open-file-input-port p)))
    (lambda (p)
      ;; A port with no meaningful length — a fifo, a character device — reports
      ;; 0 or raises; both fall back to the growing read, which is correct for
      ;; anything whose size cannot be known in advance.
      (let ((n (guard (e (#t #f)) (file-length p))))
        (if (and (fixnum? n) (fx>? n 0))
            (read-file-bytes-sized p n)
            (let ((bv (get-bytevector-all p))) (if (eof-object? bv) (bytevector) bv)))))))

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
;; for each, one character at a time (pbr-read-translated, then the pushback
;; reader's own column in its read). A bulk drain has to leave exactly the state
;; that loop would have: same text, same line and column, the same "a \n right
;; after this \r is already counted" and end-of-input flags, and the same
;; atLineStart pair. Reaching the end of S is not reading the end of input —
;; pbr-fold-eof! is that.
(define (pbr-fold-and-count! st s)
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()) (line (vector-ref st 3)) (col (vector-ref st 4))
               (skip-lf (vector-ref st 5)) (pending (vector-ref st 9))
               (als (vector-ref st 6)) (prev (vector-ref st 7)))
      (if (fx>=? i n)
          (begin (vector-set! st 3 line) (vector-set! st 4 col) (vector-set! st 5 skip-lf)
                 (vector-set! st 9 pending) (vector-set! st 6 als) (vector-set! st 7 prev)
                 (list->string (reverse acc)))
          (let ((c (string-ref s i)))
            (cond
              ((and skip-lf (char=? c #\newline)) (loop (fx+ i 1) acc line col #f pending als prev))
              ((or (char=? c #\return) (char=? c #\newline))
               (loop (fx+ i 1) (cons #\newline acc) (fx+ line 1) 1 (char=? c #\return) #f #t als))
              (else (loop (fx+ i 1) (cons c acc) line (fx+ col 1) #f #t #f als))))))))

;; pbr-fold-and-count!'s counting over s[a, b) with no folded copy built: what
;; a read that parses the string in place needs, per form.
(define (pbr-count! st s a b)
  (let loop ((i a) (line (vector-ref st 3)) (col (vector-ref st 4))
             (skip-lf (vector-ref st 5)) (pending (vector-ref st 9))
             (als (vector-ref st 6)) (prev (vector-ref st 7)))
    (if (fx>=? i b)
        (begin (vector-set! st 3 line) (vector-set! st 4 col) (vector-set! st 5 skip-lf)
               (vector-set! st 9 pending) (vector-set! st 6 als) (vector-set! st 7 prev))
        (let ((c (string-ref s i)))
          (cond
            ((and skip-lf (char=? c #\newline)) (loop (fx+ i 1) line col #f pending als prev))
            ((or (char=? c #\return) (char=? c #\newline))
             (loop (fx+ i 1) (fx+ line 1) 1 (char=? c #\return) #f #t als))
            (else (loop (fx+ i 1) line (fx+ col 1) #f #t #f als)))))))

;; Reading the end of input: the pending line ends, and the column and
;; atLineStart go back to the start of a line, as a read of -1 leaves them.
(define (pbr-fold-eof! st)
  (when (vector-ref st 9)
    (vector-set! st 3 (+ 1 (vector-ref st 3)))
    (vector-set! st 9 #f))
  (vector-set! st 7 (vector-ref st 6))
  (vector-set! st 6 #t)
  (vector-set! st 4 1))

;; The numbering origin a read at index I of S starts from (reader.ss
;; rdr-read-one): the line the reader reports, its column, and the two flags.
(define (pbr-numbering-origin st s i)
  (vector s i (+ 1 (vector-ref st 3)) (vector-ref st 4) (vector-ref st 5) (vector-ref st 9) #f))

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
    ;; a BufferedReader over one of jolt's readers: its buffered rest, then that
    ;; reader's own drain (io-streams.ss ra-drain). Over a Reader the program
    ;; wrote it drains through its own read, which drives whichever read the
    ;; Reader implements.
    ((and (reader-adapter? r) (jhost? (reader-adapter-inner r)))
     (ra-drain r drain-reader))
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
;; past exactly that form. -> (values form found? text), TEXT (when CAPTURE is
;; passed, else "") the source the read
;; consumed. (read r) over a java.io reader — cuerdas' interpolation reads this
;; way, and so does anything reading a source file form by form — and
;; read+string and clojure.edn/read over one.
;;
;; EDN? and CB pick clojure.edn's grammar (reader.ss rdr-read-one); EOF-ERROR?
;; raises "EOF while reading" at end of input instead of answering found? #f.
;; Over a LineNumberingPushbackReader the read is numbered from the reader's own
;; counters, so an error escapes as the reference's ReaderException, and the
;; counters then move over exactly what the read consumed. An EOF error consumes
;; the rest of the input, as the reference's reader has by then.
;;
;; A string-backed reader parses AT its current index and moves the index; the
;; drain-parse-refill fallback below re-materializes the whole remaining input per
;; form, which is quadratic over a file. The fallback still covers a char-reader
;; over a Chez port, a library's own reader shim, and a reader with pushback.
(define (host-reader-read r edn? cb eof-error? . capture)
  (let-values (((sr lnst) (host-reader-string-cursor r)))
    (if sr
        (let* ((s (sr-s sr)) (i (sr-pos sr)) (end (string-length s)))
          (define (consume-all!)
            (when lnst
              (pbr-count! lnst s i end)
              (pbr-fold-eof! lnst))
            (sr-pos! sr end))
          ;; an error consumes through what it names (rdr-error-resume-index)
          (define (consume-through! k)
            (when lnst (pbr-count! lnst s i k))
            (sr-pos! sr k))
          (let ((pr (rdr-on-read-error
                     (lambda (e)
                       (if (rdr-eof-error? e)
                           (consume-all!)
                           (consume-through! (rdr-error-resume-index s i e))))
                     (lambda ()
                       (rdr-read-one s i (and lnst (pbr-numbering-origin lnst s i))
                                     edn? cb eof-error?)))))
            (if (not pr)
                (begin (consume-all!) (values jolt-nil #f ""))
                (let ((j (cdr pr)))
                  (when lnst (pbr-count! lnst s i j))
                  (sr-pos! sr j)
                  (values (car pr) #t (if (pair? capture) (substring s i j) ""))))))
        (if (pushback-over-user-reader? r)
            (host-reader-read-incremental r edn? cb eof-error?)
            (host-reader-read-drained r edn? cb eof-error?)))))

;; The fallback: drain, read from the front, and put back what the read did not
;; consume. Draining a line-numbering reader counts everything it drains, so the
;; counters are put back first and then moved over only the consumed text. The
;; drained text is already folded (no \r left in it), which is also why skip-lf
;; starts over; characters that came out of the pushback buffer were counted when
;; they were first read, so they move the column only.
(define (host-reader-read-drained r edn? cb eof-error?)
  (let* ((st (and (jhost? r) (pushback-reader-tag? (jhost-tag r)) (jhost-state r)))
         (lnst (and st (vector-ref st 2) st))
         (snap (and lnst (vector-copy lnst)))
         (npushed (if st (length (vector-ref st 1)) 0))
         (s (drain-reader r))
         (end (string-length s)))
    (define (count-through! j)
      (when lnst
        (let ((p (fxmin npushed j)))
          (let ((line (vector-ref lnst 3)) (pending (vector-ref lnst 9)))
            (pbr-fold-and-count! lnst (substring s 0 p))
            (vector-set! lnst 3 line)
            (vector-set! lnst 9 pending))
          (pbr-fold-and-count! lnst (substring s p j)))))
    (define (consume-all!)
      (count-through! end)
      (when lnst (pbr-fold-eof! lnst))
      (reader-refill! r ""))
    (when snap
      (for-each (lambda (k) (vector-set! lnst k (vector-ref snap k))) '(3 4 6 7 9))
      (vector-set! lnst 5 #f))
    (let ((pr (rdr-on-read-error
               (lambda (e)
                 (if (rdr-eof-error? e)
                     (consume-all!)
                     (let ((k (rdr-error-resume-index s 0 e)))
                       (count-through! k)
                       (reader-refill! r (substring s k end)))))
               (lambda ()
                 (rdr-read-one s 0 (and lnst (pbr-numbering-origin lnst s 0))
                               edn? cb eof-error?)))))
      (if (not pr)
          (begin (consume-all!) (values jolt-nil #f ""))
          (let ((j (cdr pr)))
            (count-through! j)
            (reader-refill! r (substring s j end))
            (values (car pr) #t (substring s 0 j)))))))

;; A pushback reader whose wrapped reader is the program's own -- a proxy/reify
;; java.io.Reader, or the adapter io/reader puts around one. Such a reader can
;; be an interactive source (a REPL's or an IDE's stdin) whose read blocks until
;; more input arrives, so draining it to EOF to parse one form waits for input
;; the form never needed, as the JVM's LispReader does not (jolt-lang/jolt#1137).
;; The string and char-reader cases stay on their drains: those end, and the
;; char-reader drain is the fast path loading a file depends on.
(define (pushback-over-user-reader? r)
  (and (jhost? r) (pushback-reader-tag? (jhost-tag r))
       (let loop ((w (vector-ref (jhost-state r) 0)))
         ;; a BufferedReader is as interactive as the reader it wraps
         (cond ((not (jhost? w)) #t)
               ((string=? (jhost-tag w) "reader-adapter") (loop (reader-adapter-inner w)))
               (else #f)))))

;; Read ONE form from a pushback reader through its own read/unread, taking
;; only as much input as the form needs. Characters go into a buffer while a
;; small scanner tracks bracket depth and whether it is inside a string, regex,
;; char literal or comment. The real parser runs only at a top-level boundary:
;; the bracket that closes depth back to 0, the quote that closes a top-level
;; string, whitespace or a comment after a top-level token, or a terminating
;; macro character right after one (where the reference's token read stops). An
;; EOF read error there means the form continues (a quote or #_ with nothing
;; after it yet), so reading goes on. What the parser did not consume is
;; unread, so the next read starts right after the form; a read error unreads
;; what lies past the token it names, as the string path consumes through it.
;; At end of input the buffer is parsed as it stands, so its errors are the
;; string path's errors.
;;
;; The pushback reader's own read counts lines, so a line-numbering reader's
;; counters need no bookkeeping here; the numbering origin for a ReaderException
;; is the counters as they stood before the first character was read.
(define (host-reader-read-incremental r edn? cb eof-error?)
  (let* ((st (jhost-state r))
         (snap (and (vector-ref st 2) (vector-copy st)))
         (buf (open-output-string)))
    (define (read-char!)
      (let ((u (record-method-dispatch r "read" jolt-nil)))
        (if (or (jolt-nil? u) (and (number? u) (< u 0)))
            #f
            (integer->char (exact (truncate u))))))
    (define (unread-tail! s j)
      (let loop ((k (fx- (string-length s) 1)))
        (when (fx>=? k j)
          (record-method-dispatch r "unread" (jolt-list (string-ref s k)))
          (loop (fx- k 1)))))
    ;; the read at the buffer's front; a non-EOF error unreads past its token
    ;; and propagates
    (define (read-buffer s eof-err?)
      (rdr-on-read-error
       (lambda (e)
         (unless (rdr-eof-error? e)
           (unread-tail! s (rdr-error-resume-index s 0 e))))
       (lambda ()
         (rdr-read-one s 0 (and snap (pbr-numbering-origin snap s 0)) edn? cb eof-err?))))
    (define (buffer-text)
      ;; get-output-string resets the port, so put the text back
      (let ((s (get-output-string buf))) (put-string buf s) s))
    ;; -> (form . text) when a form is complete, #f when more input is needed
    (define (attempt)
      (let* ((s (buffer-text))
             (pr (guard (e ((rdr-eof-error? e) #f)) (read-buffer s #t))))
        (and pr
             (begin (unread-tail! s (cdr pr))
                    (cons (car pr) (substring s 0 (cdr pr)))))))
    (define (finish)
      (let* ((s (buffer-text)) (pr (read-buffer s eof-error?)))
        (if pr
            (begin (unread-tail! s (cdr pr)) (values (car pr) #t (substring s 0 (cdr pr))))
            (values jolt-nil #f ""))))
    (define (done-or got otherwise)
      (if got (values (car got) #t (cdr got)) (otherwise)))
    (define (step depth mode content?)
      (let ((c (read-char!)))
        (if (not c)
            (finish)
            (begin
              (write-char c buf)
              (case mode
                ((code)
                 (if (and content? (fx<=? depth 0)
                          (memv c '(#\( #\[ #\{ #\" #\; #\\ #\@ #\^ #\` #\~)))
                     ;; a terminating macro character ends a pending token
                     (done-or (attempt) (lambda () (code-char depth c content?)))
                     (code-char depth c content?)))
                ((char-escape) (step depth 'code #t))
                ((string)
                 (cond
                   ((char=? c #\\) (step depth 'string-escape #t))
                   ((char=? c #\")
                    (if (fx<=? depth 0)
                        (done-or (attempt) (lambda () (step depth 'code #t)))
                        (step depth 'code #t)))
                   (else (step depth 'string #t))))
                ((string-escape) (step depth 'string #t))
                ((comment)
                 (if (memv c '(#\newline #\return))
                     (if (and content? (fx<=? depth 0))
                         (done-or (attempt) (lambda () (step depth 'code content?)))
                         (step depth 'code content?))
                     (step depth 'comment content?))))))))
    (define (code-char depth c content?)
      (cond
        ((char=? c #\;) (step depth 'comment content?))
        ((char=? c #\\) (step depth 'char-escape #t))
        ((char=? c #\") (step depth 'string #t))
        ((memv c '(#\( #\[ #\{)) (step (fx+ depth 1) 'code #t))
        ((memv c '(#\) #\] #\}))
         (let ((d (fx- depth 1)))
           (if (fx<=? d 0)
               (done-or (attempt) (lambda () (step d 'code #t)))
               (step d 'code #t))))
        ((or (char-whitespace? c) (char=? c #\,))
         (if (and content? (fx<=? depth 0))
             (done-or (attempt) (lambda () (step depth 'code content?)))
             (step depth 'code content?)))
        (else (step depth 'code #t))))
    (step 0 'code #f)))

(define (host-reader-read-form r)
  (let-values (((form found? text) (host-reader-read r #f #f #f)))
    (values form found?)))

;; java.lang.String.trim: every char at or below U+0020 off both ends.
;; clojure.edn/read over a reader: one EDN form off the reader, leaving the rest
;; of it for the next read, through clojure.edn's own tag pass. Absent :eof in
;; opts makes end of input an error, as on the JVM.
(define (chez-edn-read opts reader)
  (let ((edn->value (var-deref "clojure.edn" "edn->value"))
        (kw-eof (keyword #f "eof")))
    (let-values (((form found? text)
                  (host-reader-read reader #t
                                    (lambda (f) (jolt-invoke edn->value opts f) jolt-nil)
                                    (not (jolt-truthy? (jolt-contains? opts kw-eof))))))
      (if found?
          (jolt-invoke edn->value opts form)
          (jolt-get opts kw-eof)))))

;; line-seq: an io/reader is a jhost StringReader. Drain it (or take a string)
;; and split on a line terminator; a trailing terminator does NOT yield a final
;; empty line (like readLine -> nil at EOF). Re-asserted in post-prelude.ss.
;;
;; \n, \r and \r\n all terminate, because on the JVM line-seq is a (.readLine …)
;; loop over a BufferedReader and that is the rule readLine follows. Splitting on
;; \n alone left the \r of every CRLF line attached to it.
(define (chez-lines s)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((fx=? i n) (reverse (if (fx=? start i) acc (cons (substring s start i) acc))))
        ((char=? (string-ref s i) #\newline)
         (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) acc)))
        ((char=? (string-ref s i) #\return)
         (let ((next (if (and (fx<? (fx+ i 1) n) (char=? (string-ref s (fx+ i 1)) #\newline))
                         (fx+ i 2)
                         (fx+ i 1))))
           (loop next next (cons (substring s start i) acc))))
        (else (loop (fx+ i 1) start acc))))))
;; line-seq over a host reader is LAZY, one readLine per element, as it is on the
;; JVM: (when-let [line (.readLine rdr)] (cons line (lazy-seq (line-seq rdr)))).
;; Draining the reader and splitting the string is right for a file and wrong
;; for a reader over something still arriving — an SSE body, a tailed log, a
;; pipe — where the drain cannot finish until the producer stops, so the FIRST
;; line is not visible until the LAST one has been read.
;;
;; Every reader-jhost answers readLine: string-reader and pushback-reader
;; (host-static-classes.ss), char-reader and the reader-adapter over a
;; hand-written java.io.Reader (io-streams.ss). Each applies the same \n / \r /
;; \r\n rule and the same nil-at-EOF as chez-lines, so a string argument and a
;; reader argument split alike.
;;
;; The first line is read eagerly, which is what makes (line-seq empty-rdr) nil
;; rather than a lazy cell: an unrealized lazyseq that forces to nil still
;; prints "()" (lazy-bridge.ss), and nil is what the JVM's when-let yields.
(define (chez-line-seq-lazy rdr)
  (let ((l (record-method-dispatch rdr "readLine" jolt-nil)))
    (if (jolt-nil? l)
        jolt-nil
        (jolt-cons l (jolt-make-lazy-seq (lambda () (chez-line-seq-lazy rdr)))))))
(define (chez-line-seq rdr)
  (cond ((string? rdr) (list->cseq (chez-lines rdr)))
        ((reader-jhost? rdr) (chez-line-seq-lazy rdr))
        (else (list->cseq (chez-lines (jolt-str-render-one rdr))))))

;; The keyword options slurp, spit and the clojure.java.io openers take:
;; (io/writer f :append true :encoding "UTF-8"). Clojure reads them as
;; (apply hash-map opts), so a key left without a value is the
;; IllegalArgumentException hash-map raises, and a repeated key answers its
;; LAST value. Every opener checks through here; scanning the pairs ad hoc let
;; a dangling key pass silently.
(define (io-opts-check! opts)
  (let ((n (length opts)))
    (when (odd? n)
      (throw-jvm (quote IllegalArgumentException)
                 (string-append "No value supplied for key: "
                                (jolt-str-render-one (list-ref opts (- n 1))))))))
(define (io-opt opts kw)   ; the value under KW, or nil
  (let loop ((o opts) (hit jolt-nil))
    (cond ((or (null? o) (null? (cdr o))) hit)
          ((eq? (car o) kw) (loop (cddr o) (cadr o)))
          (else (loop (cddr o) hit)))))
(define io-kw-append (keyword #f "append"))
(define io-kw-encoding (keyword #f "encoding"))
;; io.clj's (boolean (:append opts)) and (or (:encoding opts) "UTF-8"): a nil or
;; false encoding is the default, not an error.
(define (io-append? opts) (jolt-truthy? (io-opt opts io-kw-append)))
(define (io-encoding opts)
  (let ((e (io-opt opts io-kw-encoding)))
    (if (jolt-truthy? e) (charset-arg-name e) "UTF-8")))
(define (io-utf8? name) (string=? (charset-canonical-down name) "utf-8"))
;; The JVM opens a writer's charset by NAME, so one it does not know is
;; UnsupportedEncodingException -- the same check Charset/forName makes, minus
;; the IllegalCharsetNameException split, which an OutputStreamWriter does not
;; draw.
(define (io-encoding-known? name) (or (charset-lookup name) (iconv-known? name)))
(define (io-check-encoding! name)
  (unless (io-encoding-known? name)
    (unsupported-encoding-throw name)))
;; (slurp src :encoding "...") as the rest list decode-bytevector takes.
(define (slurp-encoding opts) (list (io-encoding opts)))
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
(define (slurp-path path . given)
  (io-note-file-read! path)
  ;; An entry inside a jar has no open to fail, so its absence is still checked
  ;; here. A path ON DISK is not pre-checked: read-file-bytes-on-disk opens
  ;; through open-path-guarded, which reports a missing file the same way and
  ;; also reports the three this used to miss -- a directory, an unreadable
  ;; file, and an open refused because the process is out of descriptors.
  (when (and (jar-path? path) (not (jar-path-exists? path)))
    (throw-jvm (quote java.io.FileNotFoundException)
               (string-append path " (No such file or directory)")))
  (apply read-file-string path given))
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
       (slurp-path (project-relative (file-url->path spec))))
      ((jar-path? spec) (slurp-path spec))
      (else (throw-jvm (quote java.io.IOException)
                       (string-append "protocol doesn't support input: " spec))))))
;; Whatever the handler handed back: a byte stream, a reader, or a value that
;; already renders as its content.
(define (drain-any-stream s)
  (cond ((reader-jhost? s) (drain-reader s))
        ;; jolt's byte stream, or a reify/proxy InputStream (io-streams.ss),
        ;; whose readAllBytes is the class's
        ((or (and (jhost? s) (string=? (jhost-tag s) "in-stream"))
             (user-in-stream? s))
         (utf8-bytes->string (na-bytearray->bv
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
  (io-opts-check! opts)
  (cond
    ;; slurp is a drain of (apply io/reader src opts) on the JVM, so a charset
    ;; other than UTF-8 on a file, path or URL decodes the way io/reader does.
    ;; The arms below read those as UTF-8 text and ignored :encoding.
    ((and (or (jfile? src) (string? src) (url-jhost? src) (embedded-res? src))
          (not (io-utf8? (io-encoding opts))))
     (let* ((r (apply jolt-io-reader src opts))
            (text (guard (e (#t (jolt-close r) (raise e))) (drain-reader r))))
       (jolt-close r)
       text))
    ((jfile? src) (slurp-path (jfile-fs src) (jfile-path src)))
    ((embedded-res? src) (utf8->string (embedded-res-bytes src)))
    ((reader-jhost? src) (drain-reader src))
    ((reify-method-ref src "-read-line")
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
    ((string? src) (let ((fp (io-source-path src)))
                     (if (jar-path? fp)
                         (slurp-path fp)
                         (slurp-path fp (if (file-url-string? src) (file-url->path src) src)))))
    (else (throw-jvm (quote IllegalArgumentException) (string-append "Cannot open <" (jolt-pr-str src) "> as a Reader.")))))

(define io-counter-mutex (make-mutex))
(define (jolt-spit path content . opts)
  ;; Render BEFORE any file is touched — a throwing toString used to leave the
  ;; target truncated. Both writes then go to the target in place (below).
  ;; Only a path, a File or a host stream names a target; anything else is the
  ;; coercion error io/writer raises. nil used to render as "" and write a temp
  ;; file into the working directory before failing to rename it.
  (unless (or (string? path) (jfile? path) (jhost? path))
    (throw-jvm (quote IllegalArgumentException)
               (string-append "Cannot open <" (jolt-pr-str path) "> as a Writer.")))
  (io-opts-check! opts)
  (let* ((given (if (url-jhost? path) (url-write-path path) (file-path-of path)))
         (p (project-relative given))
         (enc (io-encoding opts))
         ;; encoded up front too, and :encoding was ignored outright before
         (bytes (begin
                  ;; The JVM's spit opens the target and THEN builds the writer
                  ;; that refuses the charset, so a charset it does not know
                  ;; still truncates the file (or creates it) before raising.
                  (unless (io-encoding-known? enc)
                    (close-port (open-path-guarded given p
                                  (lambda (rp)
                                    (open-file-output-port rp
                                      (if (io-append? opts)
                                          (file-options no-fail no-truncate append)
                                          (file-options no-fail))
                                      (buffer-mode none)))))
                    (io-check-encoding! enc))
                  (charset-encode-bv (jolt-str-render-one content) enc))))
    ;; The JVM opens the TARGET, so a target it cannot open fails here and names
    ;; itself. This wrote its temp file first and only discovered the target at
    ;; the rename, which came back as Chez's "cannot rename ..." inside a plain
    ;; java.io.IOException -- naming a temp path the caller never asked for
    ;; (jolt-g81).
    (when (file-directory? p) (file-open-error given p))
    (if (io-append? opts)
        (with-port (open-path-guarded given p
                     (lambda (rp) (open-file-output-port rp (file-options no-fail no-truncate append)
                                                         (buffer-mode block))))
          (lambda (port) (put-bytevector port bytes)))
        ;; In place, truncating, as the JVM's FileOutputStream does. This used
        ;; to stage a temp file and rename it over the target, which REPLACED
        ;; the file rather than writing it: a spit through a symlink turned the
        ;; link into a regular file and left its target alone, a hard link
        ;; stopped sharing the data, and the mode was reset from the umask (a
        ;; 0600 file came back 0644). It also cost four syscalls more than the
        ;; write, ~250us a spit against ~25us on the JVM.
        (with-port (open-path-guarded given p
                     (lambda (rp) (open-file-output-port rp (file-options no-fail)
                                                         (buffer-mode block))))
          (lambda (port) (put-bytevector port bytes))))
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
(register-str-render! jfile? (lambda (f) (path-native (jfile-path f))))

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
(define (jolt-io-reader x . opts)   ; OPTS: io-streams.ss's wrapper reads them
  (cond
    ((reader-jhost? x) x)
    ((jfile? x) (io-note-file-read! (jfile-fs x))
                (host-new "StringReader" (read-file-string (jfile-fs x) (jfile-path x))))
    ((embedded-res? x) (host-new "StringReader" (utf8->string (embedded-res-bytes x))))
    ((url-jhost? x) (host-new "StringReader" (url-content x)))
    ((string? x) (let ((p (project-relative x)))
                   (io-note-file-read! p)
                   (host-new "StringReader" (if (jar-path? p) (read-file-string p) (read-file-string p x)))))
    ((or (cseq? x) (empty-list-t? x) (pvec? x))
     (host-new "StringReader" (seq-source->string x)))
    ;; anything else is not a source, and quietly rendering it would read as empty
    ;; content — (io/reader nil) used to hand back a reader over "" rather than say
    ;; so. Same coercion error the JVM raises, and the same one io/writer raises.
    (else (throw-jvm (quote IllegalArgumentException)
                     (string-append "Cannot open <" (jolt-pr-str x) "> as a Reader.")))))

;; --- clojure.java.io/writer: an existing StringWriter passes through. Every
;; other arm -- a File, a path, a URL, a byte stream -- is io-streams.ss's, which
;; loads later and has the ports: a file is an encoding writer over the same
;; output stream io/output-stream opens, the JVM's make-writer.
(define (jolt-io-writer x . opts)
  (cond
    ((and (jhost? x) (string=? (jhost-tag x) "writer")) x)
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
    ;; .isAbsolute, not "starts with a separator" — the two agree on POSIX and
    ;; differ on Windows both ways round: "C:/x" IS absolute and was silently
    ;; accepted here, and "/x" is NOT (it is rooted on the current drive) yet was
    ;; rejected. The comment above already says this mirrors .isAbsolute; now it
    ;; asks it (jolt-lang/jolt#1074).
    (when (jfile-path-absolute? p)
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
(define (url-file-coercion u) (url-spec-file-coercion (url-spec u)))
(define (url-spec-file-coercion spec)
  (if (string=? (url-protocol spec) "file")
      (make-jfile (file-url->path spec))
      (throw-jvm 'IllegalArgumentException (string-append "Not a file: " spec))))
(def-var! "clojure.java.io" "as-file"
  ;; Clojure extends Coercions to nil, so (io/as-file nil) is nil -- NOT a File
  ;; whose path is "". The difference is load-bearing one call downstream, where
  ;; the JVM raises on the nil and jolt was quietly reading the process's cwd.
  (lambda (x) (cond ((jolt-nil? x) x)
                    ((jfile? x) x)
                    ((url-jhost? x) (url-file-coercion x))
                    ((uri-jhost? x) (url-spec-file-coercion (uri-field x 'string)))
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
  (make-url (string-append "file:" (file-uri-path (root-path-abs (string-append root "/" nm))))))
;; A path under a source root made absolute for a URL: the roots are spelled as
;; deps.edn spells them ("./src", "./lib.jar"), and the JVM's URL for a resource
;; carries no "./" segment, so a leading one is dropped before the cwd is put in
;; front. Directory and jar roots alike (resource-jar-url, loader.ss
;; ldr-root-file, whose spelling is *file* and the AOT cache's key).
;; A relative root is relative to the process directory: that is where
;; resource-candidate and ldr-root-file found the file. bin/jolt cd's to the
;; checkout and keeps jolt's own install roots ("stdlib", "jolt-core")
;; relative, so resolving them against user.dir (the caller's directory) named
;; a file that isn't there, and (slurp (io/resource "clojure/string.clj"))
;; threw from anywhere but the checkout. When the two directories agree (a
;; built binary) jfile-abs answers the same path, so nothing changes there.
(define (root-path-abs p)
  (let ((p (if (and (>= (string-length p) 2)
                    (char=? (string-ref p 0) #\.)
                    (char=? (string-ref p 1) #\/))
               (substring p 2 (string-length p))
               p)))
    (if (or (string=? (jolt-user-dir) (current-directory))
            (= (string-length p) 0) (jfile-path-absolute? p) (jar-path? p)
            (windows-root-relative? p))
        (jfile-abs p)
        (path-under (current-directory) p))))
;; The jar: URL for `nm` inside the jar at root JAR, absolute like the file: one.
(define (resource-jar-url jar nm)
  (make-url (make-jar-path (root-path-abs jar) nm)))
;; The candidate for NM on ROOT, as (path-or-#f . url-thunk): a directory root's
;; file, or a jar root's entry (loader.ss root-jar-index says which roots are
;; jars). The path is what the AOT cache is told about; for a jar it is the
;; entry's jar path, so the cache keys on the entry's content and re-validates
;; when the jar changes.
(define (resource-candidate root nm)
  (let ((d (root-jar-index root)))
    (if d
        (let ((p (make-jar-path (root-path-abs root) nm)))
          ;; a directory's entry answers to its name without the "/", as the
          ;; JDK's JarLoader finds it through JarFile.getJarEntry
          (cons p (and (zipdir-lookup d nm) (lambda () (resource-jar-url root nm)))))
        (let ((cand (string-append root "/" nm)))
          (cons cand (and (file-exists? cand) (lambda () (resource-file-url root nm))))))))

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
         (emb (embedded-resource-url nm)))
    (if emb emb
        (let loop ((roots (get-source-roots)))
          (if (null? roots)
              jolt-nil
              (let ((cand (resource-candidate (car roots) nm)))
                (io-note-file-read! (car cand))
                (if (cdr cand)
                    ((cdr cand))
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
;; A loader object with a getResource method — a jolt.loader context facade, or
;; the host singleton itself — answers in its OWN context: the 2-arity is the
;; JVM's "resolve against THIS ClassLoader", and a context loader depends on it
;; for isolation (a resource only the context's roots hold must not fall
;; through to the host roots). Anything else — nil, a stand-in for a thread's
;; contextClassLoader, junk — keeps the historical answer.
;; The getResource a loader object would answer with, or #f: the host's jhost
;; registry for the host singleton, the library's tagged-table registry for a
;; jolt.loader context facade.
(define (loader-object-get-resource loader)
  (cond
    ((jhost? loader)
     (let* ((mh (hashtable-ref host-methods-tbl (jhost-tag loader) #f))
            (f (and mh (hashtable-ref mh "getResource" #f))))
       f))
    ((htable? loader) (tagged-method-lookup loader "getResource"))
    (else #f)))
;; The ambient base loader (jolt.loader rebinds clojure.lang.RT/baseLoader to
;; the loader bound by with-loader; outside one it is the host singleton).
;; Read through the class-statics table so a library-registered value — the
;; Clojure fn the loader registers — is what answers, not a stale copy.
(define (current-base-loader)
  (let ((m (hashtable-ref class-statics-tbl "clojure.lang.RT" #f)))
    (and m (let ((f (hashtable-ref m "baseLoader" #f))) (and f (f))))))
(define jolt-io-resource
  (case-lambda
    ;; The 1-arity follows the ambient loader: inside `with-loader` a context's
    ;; facade answers in its own context — the resource analogue of the TCCL, and
    ;; how an extension's (io/resource "x") finds its own bundled files. Outside
    ;; one the ambient value is the host singleton, whose getResource is straight
    ;; through to resolve-resource: the historical answer, unchanged.
    ((name)
     (let ((cl (current-base-loader)))
       (if cl
           (let ((f (loader-object-get-resource cl)))
             (if f
                 (f cl (resource-name-arg name))
                 (resolve-resource name)))
           (resolve-resource name))))
    ((name loader)
     (let ((f (loader-object-get-resource loader)))
       (if f
           (f loader (resource-name-arg name))
           (resolve-resource name))))))
(def-var! "clojure.java.io" "resource" jolt-io-resource)
;; as-url honors a library-registered URL class (e.g. jolt-lang/http-client's full
;; java.net.URL shim) so io/as-url and (URL. spec) agree; else the file-only jhost.
;; as-url of a File is clojure.java.io's (.toURL (.toURI f)) — the encoded
;; file: URL File.toURI spells, "file:/C:/…" on Windows (#1118); a bare string
;; keeps its spec as given.
(def-var! "clojure.java.io" "as-url"
  (lambda (x)
    (cond ((and (jhost? x) (string=? (jhost-tag x) "url")) x)
          ((htable? x) x)
          (else (let ((spec (if (jfile? x) (jfile->uri-spec (jfile-fs x)) (jolt-str-render-one x)))
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
         (emb (embedded-resource-url nm)))
    (let loop ((roots (get-source-roots))
               (acc (if emb (list emb) '())))
      (cond ((null? roots) (list->cseq (reverse acc)))
            (else
             (let ((cand (resource-candidate (car roots) nm)))
               (io-note-file-read! (car cand))
               (if (cdr cand)
                   (loop (cdr roots) (cons ((cdr cand)) acc))
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
                   (lambda (seed h) (hash-combine (jolt->fx seed) (jolt->fx h))))
             ;; Clojure's throw-without-a-checked-signature. A caller uses it to
             ;; rethrow a caught exception and keep its type, which is exactly
             ;; what jolt-throw does — SCI's reflective invoke ends every method
             ;; call here, so without it a method that throws reports
             ;; "No matching field or method: clojure.lang.Util/sneakyThrow"
             ;; instead of the exception the method raised.
             (cons "sneakyThrow" (lambda (t) (jolt-throw t))))))
  (register-class-statics! "Util" util-statics)
  (register-class-statics! "clojure.lang.Util" util-statics))
;; java.lang.Thread — ONE object per thread, whoever asks and however. A thread
;; started from a (Thread. f) object IS that object: its Thread/currentThread, the
;; key getAllStackTraces gives for it, and a handle anyone took for it earlier are
;; all identical? to it, as on the JVM. A thread jolt did not start from a Thread
;; object (main, a future's, a pool worker, a core.async thread, a fiber carrier)
;; gets one made the first time anyone asks, kept for as long as it lives. It used
;; to be two representations — the Thread object and a separate currentThread
;; handle — which answered some members each and were never identical?.
;;
;; State (concurrency.ss reads the same slots):
;;   #(thunk done? mutex cond interrupt-box started? name-box thread-id daemon
;;     priority java-id uncaught-handler thread-group fiber)
;; fiber is the fiber a VIRTUAL thread's object stands for, #f for a platform
;; thread's (virtual-thread-object below).
;; thread-id is the runtime's (get-thread-id) of the running thread, #f before
;; start; java-id is getId/threadId, assigned at construction as the JVM assigns
;; it, 1 for the boot thread as the JVM's main is. Once a thread runs, its name,
;; daemon status and priority live in the id-keyed tables below and in
;; lazy-bridge.ss, so an object made for it from another thread reads the same.
(define java-thread-id-next 2)                 ; 1 is main's
(define java-thread-id-mu (make-mutex))
(define (next-java-thread-id!)
  (jolt-with-mutex java-thread-id-mu
    (let ((n java-thread-id-next)) (set! java-thread-id-next (+ n 1)) n)))
(define (make-thread-object thunk name started? tid daemon priority ibox)
  (make-jhost "user-thread"
              (vector thunk #f (make-mutex) (jolt-make-cv) ibox started? (box name) tid
                      daemon priority
                      (if (eqv? tid jolt-boot-thread-id) 1 (next-java-thread-id!))
                      #f #f #f)))
;; A fiber's Thread object: a JVM virtual thread's surface. Its flag is the fiber's
;; own box, it is a daemon, its name is the empty string a virtual thread gets when
;; none is given, and it has no runtime thread id (slot 7 #f), since the OS thread
;; under it changes nothing about it. It is started, and done once the fiber
;; finishes, which is what isAlive and join read; the fiber's monitor marks it.
;; The fiber slot's index, named: the thread object's slots are shared with
;; concurrency.ss and grow.
(define jthread-fiber-slot 13)
(define (jthread-fiber st) (vector-ref st jthread-fiber-slot))
(define (jthread-fiber-set! st f) (vector-set! st jthread-fiber-slot f))
(define (virtual-thread-object f)
  (let ((obj (make-thread-object #f "" #t #f #t 5 (jolt-fiber-ibox! f))))
    (bind-virtual-thread! obj f)
    obj))
;; Make OBJ fiber F's Thread: the fiber slot, the fiber's field, and done when it ends.
(define (bind-virtual-thread! obj f)
  (let ((st (jhost-state obj)))
    (jthread-fiber-set! st f)
    (vector-set! st 12 virtual-thread-group)          ; its ThreadGroup (concurrency.ss)
    (jolt-fiber-thread-set! f obj)
    (jolt-fiber-monitor! f
      (lambda (_err)
        (jolt-with-mutex (vector-ref st 2)
          (vector-set! st 1 #t)
          (jolt-cv-wake! (vector-ref st 3)))))))
;; The fiber's object, made once. Only the fiber's own code and whoever it hands
;; the object to can ask, and the fiber runs on one thread at a time, so the
;; field needs no lock when the fiber asks for itself.
(define (fiber-thread-object f)
  (or (jolt-fiber-thread f) (virtual-thread-object f)))
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
;; Priorities, id-keyed like the names and for the same reason. Only a priority
;; other than NORM_PRIORITY (5) has an entry.
(define thread-priorities-by-id (make-eqv-hashtable))
(define (jolt-thread-priority id)
  (jolt-with-mutex thread-handles-mutex (hashtable-ref thread-priorities-by-id id 5)))
(define (jolt-thread-priority-set! id p)
  (jolt-with-mutex thread-handles-mutex
    (if (eqv? p 5)
        (hashtable-delete! thread-priorities-by-id id)
        (hashtable-set! thread-priorities-by-id id p))))
;; Thread.setPriority's argument: MIN_PRIORITY..MAX_PRIORITY, else
;; IllegalArgumentException (with no message, as the JVM's).
(define (jolt-thread-priority-arg p)
  (let ((n (and (number? p) (jnum->exact p))))
    (if (and (integer? n) (<= 1 n 10))
        n
        (throw-jvm 'IllegalArgumentException jolt-nil))))
;; A handle's thread is alive while it runs: it is the caller, the boot thread
;; (whose end is the process's), or a thread jolt started that has not finished.
(define (thread-handle-alive? id)
  (or (eqv? id (get-thread-id)) (eqv? id jolt-boot-thread-id) (jolt-started-thread? id)))
;; A running thread is alive while it runs: it is the caller, the boot thread
;; (whose end is the process's), or a thread jolt started that has not finished.
(define (thread-handle-alive? id)
  (or (eqv? id (get-thread-id)) (eqv? id jolt-boot-thread-id) (jolt-started-thread? id)))
;; TCCL follows the ambient loader the way io/resource's 1-arity does: inside
;; `with-loader` it is that context's facade (so a library finding its own
;; resources the Java way gets the context's roots), outside one the host
;; singleton. `current-base-loader` answers with the facade itself, and only a
;; classloader-shaped answer (a jhost, or a tagged table like the facade) is
;; taken — a library that rebound RT/baseLoader to something else keeps the
;; historical answer, the rule the resource path above follows too. There is no
;; setContextClassLoader: the getter is ambient-derived, not per-thread state.
(define (thread-context-class-loader)
  (let ((cl (current-base-loader)))
    (if (and cl (or (jhost? cl) (htable? cl))) cl the-classloader)))
;; The object for each running thread, by id, so another thread finds the same
;; one; and this thread's own, cached in a thread parameter. The cell carries the
;; owning thread's id because a Chez thread parameter is inherited by a forked
;; thread, and a child must not report the parent's object as its own.
(define thread-handle-cell (make-thread-parameter #f))      ; (thread-id . object)
(define thread-handles-by-id (make-eqv-hashtable))
(define thread-handles-mutex (make-mutex))
;; Thread.start: the started thread IS its object. Registered by the parent right
;; after the fork and again by the child, so it is there whichever runs first.
(define (register-thread-object! id obj)
  (jolt-with-mutex thread-handles-mutex (hashtable-set! thread-handles-by-id id obj)))
(define (thread-object-for-id! id)
  (let ((mine? (eqv? id (get-thread-id))))
    (jolt-with-mutex thread-handles-mutex
      (or (hashtable-ref thread-handles-by-id id #f)
          (if (or mine? (thread-handle-alive? id))
              (let ((obj (make-thread-object #f #f #t id #f 5
                                             (if mine? (current-os-thread-box) (thread-box-for-id! id)))))
                (hashtable-set! thread-handles-by-id id obj)
                obj)
              ;; already finished, its exit hook run: an object that registers
              ;; nothing, or the entries would outlive the thread
              (let ((obj (make-thread-object #f (jolt-thread-name id) #t id #f 5 (box #f))))
                (vector-set! (jhost-state obj) 1 #t)
                obj))))))
;; On a fiber, the FIBER's object: a fiber is a virtual thread, and its carrier is
;; not something its code can see (the JVM hides a virtual thread's carrier the
;; same way).
(define (current-thread-handle)
  (let ((f (jolt-current-fiber)))
    (if f
        (fiber-thread-object f)
        (let ((c (thread-handle-cell))
              (id (get-thread-id)))
          (if (and (pair? c) (eqv? (car c) id))
              (cdr c)
              (let ((h (thread-object-for-id! id)))
                (thread-handle-cell (cons id h))
                h))))))
(define (thread-handle-for-id id)
  (if (and (eqv? id (get-thread-id)) (not (jolt-current-fiber)))
      (current-thread-handle)
      (thread-object-for-id! id)))
;; A thread that ends: its object is no longer alive (join returns), and the
;; tables keyed by its id let go. The object keeps what it knew.
(set! jolt-thread-exit-hook
  (lambda (id)
    (let ((obj (jolt-with-mutex thread-handles-mutex
                 (let ((o (hashtable-ref thread-handles-by-id id #f)))
                   (hashtable-delete! thread-handles-by-id id)
                   o))))
      (thread-box-forget! id)
      (thread-state-forget! id)
      ;; the object takes over the name, priority and daemon status the tables
      ;; held, and is marked done, BEFORE the entries go (concurrency.ss
      ;; jthread-finish!; a thread whose body already finished it keeps what
      ;; it has, which may since have been set on the object)
      (when obj
        (let ((st (jhost-state obj)))
          (jolt-with-mutex (vector-ref st 2)
            (jthread-finish! st id))))
      (jolt-with-mutex thread-handles-mutex
        (hashtable-delete! thread-names-by-id id)
        (hashtable-delete! thread-priorities-by-id id)))))
(set! jolt-thread-box-adopted-hook
  (lambda (id b)
    (let ((obj (jolt-with-mutex thread-handles-mutex (hashtable-ref thread-handles-by-id id #f))))
      (when obj (vector-set! (jhost-state obj) 4 b)))))
;; Thread/getAllStackTraces: the live threads mapped to EMPTY stack traces. jolt
;; reifies no call stack (TCO erases caller frames) and .getStackTrace is already
;; an empty StackTraceElement[], so the traces are honestly empty; the thread set
;; is real, which is what the callers want — ring's suites count threads before
;; and after a request to check for leaks. The boot thread is among them, as the
;; JVM's main is, whoever asks.
(define (all-stack-traces)
  (let loop ((ids (cons (get-thread-id) (cons jolt-boot-thread-id (live-thread-ids))))
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

;; The rules above, spelled for both platforms. Three things in them are really
;; questions about the platform rather than about "/":
;; whether a character is a separator, whether the parent already ends in one,
;; and which one a join should add. The old spelling asked (string=? p "/"),
;; which is "is the parent the root" written for the one platform that has
;; exactly one root — Windows has "C:/", "//srv/sh/" and "/". Since a normalized
;; path ends in a separator only when it IS a root, asking that directly covers
;; every root on both platforms and needs no root table.
;;
;; The default parent stays "/" for both: WinNTFileSystem.getDefaultParent() is
;; "\\", which is the same path in the "/" spelling this shim renders with.
(define (path-ends-with-sep? windows? p)
  (let ((n (string-length p)))
    (and (fx>? n 0) (path-sep-for? windows? (string-ref p (fx- n 1))))))

(define (jolt-file-join-for windows? p c)
  (let ((p (if (string=? p "") "/" p)))
    (cond
      ;; an empty child, or one that is nothing but a separator, adds nothing
      ((or (string=? c "")
           (and (fx=? (string-length c) 1) (path-sep-for? windows? (string-ref c 0))))
       p)
      ;; a child that starts with a separator supplies the join itself, so the
      ;; parent must not add a second one
      ((path-sep-for? windows? (string-ref c 0))
       (if (path-ends-with-sep? windows? p)
           (string-append p (substring c 1 (string-length c)))
           (string-append p c)))
      ((path-ends-with-sep? windows? p) (string-append p c))
      (else (string-append p (path-join-sep windows? p) c)))))

(define (jolt-file-join parent child)
  (when (jolt-nil? child) (throw-jvm (quote NullPointerException) jolt-nil))
  (let ((c (jolt-path-normalize (file-path-of child))))
    (if (jolt-nil? parent)
        c
        (jolt-file-join-for (eq? (sa-os-family) 'windows)
                            (jolt-path-normalize (file-path-of parent))
                            c))))
;; new File((String)null) throws too, with a null message of its own. Only the
;; two-arg form takes a null parent, and there it means "the child alone".
(define (jolt-file-ctor a . rest)
  (cond ((pair? rest) (jolt-make-file (jolt-file-join a (car rest))))
        ((jolt-nil? a) (throw-jvm (quote NullPointerException) jolt-nil))
        ((uri-jhost? a) (make-jfile (uri->file-path a)))
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
                  (else (host-temp-dir))))
         (sfx (if (or (null? (list suffix)) (jolt-nil? suffix)) ".tmp" (jolt-str-render-one suffix))))
    (let ((n (jolt-with-mutex io-counter-mutex
              (set! temp-file-counter (+ temp-file-counter 1))
              temp-file-counter)))
    (let loop ((n n))
      (let ((p (string-append d "/" (jolt-str-render-one prefix)
                              (number->string (now-millis)) "-" (number->string n) sfx)))
        (if (file-exists? p) (loop (+ n 1))
            (begin (close-port (open-output-file p 'truncate)) (make-jfile p))))))))
;; File.listRoots: the filesystem roots. POSIX has exactly one; Windows has one
;; per mounted drive, and answering "/" there named a directory on whichever
;; drive the process happened to be on rather than enumerating anything
;; (jolt-lang/jolt#1074). No volume-enumerating entry point is bound here, so
;; probe the 26 letters — enumeration IS what the method is for, it is called
;; rarely, and 26 stats are cheap next to a wrong answer. EXISTS? is a parameter
;; so the Windows row is reachable from a Linux runner
;; (test/chez/win-platform-test.ss). A Windows host that somehow shows no drive
;; at all still answers "C:/" rather than an empty array, because the JVM never
;; answers an empty one.
(define (file-list-roots-for windows? exists?)
  (if (not windows?)
      (list "/")
      (let loop ((i 25) (acc '()))
        (if (< i 0)
            (if (null? acc) (list "C:/") acc)
            (let ((r (string-append (string (integer->char (+ (char->integer #\A) i))) ":/")))
              (loop (- i 1) (if (exists? r) (cons r acc) acc)))))))

;; separator is "\\" on Windows, and File and Path render with it (path-native),
;; so the two agree as they do on the JDK (jolt-lang/jolt#1110). pathSeparator is
;; the PATH-LIST separator and must be ";" there, or babashka.fs/split-paths and
;; fs/which cut every drive-lettered entry in half (host-static-methods.ss
;; path-list-separator).
(let ((statics (list (cons "separator" (file-separator))
                     (cons "separatorChar" (string-ref (file-separator) 0))
                     (cons "pathSeparator" (path-list-separator))
                     (cons "pathSeparatorChar" (string-ref (path-list-separator) 0))
                     (cons "createTempFile" file-create-temp)
                     (cons "listRoots"
                           (lambda ()
                             (apply jolt-vector
                                    (map make-jfile
                                         (file-list-roots-for (eq? (sa-os-family) 'windows)
                                                              file-exists?))))))))
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
    ;; (int)(hilo >> 32) ^ (int)hilo where hilo = msb ^ lsb — the JVM fold.
    ((string=? m "hashCode") (uuid-hashcode u))
    (else 'pass)))
(register-method-arm! arm-priority-date
  (lambda (obj method-name rest-args)
    (if (juuid? obj)
        (uuid-method obj method-name (method-rest-args->list rest-args))
        'pass)))
;; (Long. n) / (Long. "n"): a Long is just jolt's integer; return it (parse a string).
(register-class-ctor! "Long" (lambda (x) (if (string? x) (parse-int-or-throw x 10 "long") (->num (jnum->exact x)))))
(register-class-ctor! "java.lang.Long" (lambda (x) (if (string? x) (parse-int-or-throw x 10 "long") (->num (jnum->exact x)))))
;; (Integer. n) / (Integer. "n"): jolt's integer, range-checked like intCast.
(define (integer-ctor x)
  (jolt-int-cast (if (string? x) (parse-int-or-throw x 10 "int") x)))
(register-class-ctor! "Integer" integer-ctor)
(register-class-ctor! "java.lang.Integer" integer-ctor)
;; (Double. x) / (Double. "x"): jolt's double. The string arity is
;; Double.parseDouble, so it takes that grammar rather than a string->number of
;; its own — which read (Double. "#xff") as 255.0 and (Double. "1/2") as 0.5.
(define (double-ctor x)
  (if (string? x) (parse-double-or-throw x) (jolt-double x)))
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
;; An RFC-2396 parse that follows java.net.URI's, because the single-argument
;; constructor VALIDATES: a space — or any character illegal in the component it
;; lands in — is a URISyntaxException, not a URI whose getHost is the garbage.
;; Callers lean on that: validation that only tries (URI. s) and catches, and
;; anything downstream that trusts getHost to be a host (jolt-oov, #904).
;;
;; The shape mirrors the JVM's parser closely enough to reproduce its messages
;; ("Illegal character in authority at index 11: …"), including the three rules
;; that are easy to miss:
;;   - a registry-based authority is legal but has NO host: "http://h_c.com/p"
;;     parses and getHost is nil, because "_" is not a hostname character. Same
;;     for a non-ASCII host and for a port that is not all digits.
;;   - a character above 0x80 that is neither a space nor an ISO control is
;;     legal UNESCAPED wherever an escape is, so "http://h.com/ä" is a valid URI.
;;   - an opaque URI ("mailto:a@b.com") has no path at all; its body is the
;;     scheme-specific part.
;; The result is kept in a jhost "uri" carrying the original string, so (str u) /
;; (.toString u) give the original. instance? java.net.URI + extend-protocol
;; dispatch work via value-host-tags.
(define (uri-index-of s ch from)
  (let ((n (string-length s)))
    (let loop ((i from)) (cond ((>= i n) #f) ((char=? (string-ref s i) ch) i) (else (loop (+ i 1)))))))

;; Character classes, ASCII-exact: Chez's char-alphabetic? spans Unicode, and a
;; letter above 0x80 is "other" to the URI grammar, not an alpha.
(define (uri-alpha? c) (or (and (char>=? c #\a) (char<=? c #\z)) (and (char>=? c #\A) (char<=? c #\Z))))
(define (uri-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (uri-alphanum? c) (or (uri-alpha? c) (uri-digit? c)))
(define (uri-hex? c) (or (uri-digit? c) (and (char>=? c #\a) (char<=? c #\f)) (and (char>=? c #\A) (char<=? c #\F))))
(define (uri-in-set? c set) (and (uri-index-of set c 0) #t))
;; unreserved = alphanum | mark
(define (uri-unreserved? c) (or (uri-alphanum? c) (uri-in-set? c "-_.!~*'()")))
;; uric = reserved | unreserved
(define (uri-uric? c) (or (uri-unreserved? c) (uri-in-set? c ";/?:@&=+$,[]")))
;; path = pchar | ";" | "/", pchar = unreserved | ":" "@" "&" "=" "+" "$" ","
(define (uri-path-char? c) (or (uri-unreserved? c) (uri-in-set? c ":@&=+$,;/")))
(define (uri-userinfo-char? c) (or (uri-unreserved? c) (uri-in-set? c ";:&=+$,")))
(define (uri-reg-name-char? c) (or (uri-unreserved? c) (uri-in-set? c "$,;:@&=+")))
;; server = userinfo | alphanum | "-" | "." ":" "@" "[" "]"
(define (uri-server-char? c) (or (uri-userinfo-char? c) (uri-in-set? c ".:@[]")))
;; …and inside a literal IPv6 address "%" is the scope-id separator, not an escape.
(define (uri-server%-char? c) (or (uri-server-char? c) (char=? c #\%)))
(define (uri-scheme-char? c) (or (uri-alphanum? c) (uri-in-set? c "+-.")))
(define (uri-alphanum-dash? c) (or (uri-alphanum? c) (char=? c #\-)))
(define (uri-digit-dot? c) (or (uri-digit? c) (char=? c #\.)))
;; Character.isISOControl / isSpaceChar over the range scanEscape can reach.
(define (uri-iso-control? c)
  (let ((i (char->integer c))) (or (<= i #x1f) (and (>= i #x7f) (<= i #x9f)))))
(define (uri-space-char? c)
  (let ((i (char->integer c)))
    (or (= i #x20) (= i #xa0) (= i #x1680) (and (>= i #x2000) (<= i #x200a))
        (= i #x2028) (= i #x2029) (= i #x202f) (= i #x205f) (= i #x3000))))
;; java.net.URI.scanEscape: "%hh", or an unescaped character above 0x80 that is
;; neither a space nor an ISO control. Answers the index past the unit,
;; 'malformed for a bad "%" pair, or the same index when neither applies.
(define (uri-scan-escape s p n)
  (let ((c (string-ref s p)))
    (cond ((char=? c #\%)
           (if (and (<= (+ p 3) n) (uri-hex? (string-ref s (+ p 1))) (uri-hex? (string-ref s (+ p 2))))
               (+ p 3)
               'malformed))
          ((and (> (char->integer c) 128) (not (uri-space-char? c)) (not (uri-iso-control? c))) (+ p 1))
          (else p))))

;; java.net.URI.decode: percent-decode a component, as the NON-raw accessors
;; answer it (getPath vs getRawPath). Three details make this more than a loop
;; over "%hh":
;;   - a run of consecutive escapes is one UTF-8 sequence — "%C3%A4" is "ä", not
;;     two characters — so the run is collected and decoded together;
;;   - a byte sequence that is not valid UTF-8 becomes U+FFFD rather than
;;     raising, since the string already parsed as a URI and the accessor has no
;;     way to report an error. The JVM decodes these through a CharsetDecoder, so
;;     the run decodes through utf8-bytes->string (natives-str.ss), which is that
;;     decoder: "%FF" is one U+FFFD and "%C0%AF" is two, where Chez's own
;;     utf8->string would answer one for both, and "%EF%BB%BF" is U+FEFF rather
;;     than a signature to swallow;
;;   - inside a BRACKETED IPv6 literal a "%" is the scope-id separator, not an
;;     escape, so "[fe80::1%25eth0]" must not decode to "[fe80::1%eth0]" — a
;;     scope id is written with the "%" escaped and stays that way. The JVM has
;;     this as a flag per accessor (JDK-8037396): the components that can hold an
;;     IPv6 literal — authority, userInfo, schemeSpecificPart — decode with it,
;;     and path/query/fragment without, which is why "?q=[%25]" reads back from
;;     getQuery as "q=[%]" but from getSchemeSpecificPart with the "%25" intact.
;; "+" is NOT a space: that is form encoding, and URLDecoder's job, not this
;; one's. A component with no "%" is returned as it is.
(define (uri-decode* x keep-scope-id?)
  (if (or (jolt-nil? x) (not (uri-index-of x #\% 0)))
      x
      (let ((n (string-length x)) (out '()))
        ;; a "[" opens the literal region and the next "]" closes it; an
        ;; unbalanced "]" outside one means nothing, as on the JVM.
        (define (bracket-state c in?)
          (cond ((char=? c #\[) #t) ((and in? (char=? c #\])) #f) (else in?)))
        (define (escape-at? i in?)
          (and (char=? (string-ref x i) #\%) (not (and in? keep-scope-id?))))
        (let loop ((i 0) (in? #f))
          (cond
            ((>= i n) (apply string-append (reverse out)))
            ;; a RUN of escapes is one UTF-8 sequence, so it decodes as one
            ((escape-at? i in?)
             (let run ((j i) (bytes '()))
               (if (and (< j n) (escape-at? j in?))
                   (run (+ j 3) (cons (+ (* 16 (hexv (string-ref x (+ j 1))))
                                         (hexv (string-ref x (+ j 2))))
                                      bytes))
                   (begin (set! out (cons (utf8-bytes->string (u8-list->bytevector (reverse bytes))) out))
                          (loop j in?)))))
            ;; everything up to the next escape passes through unchanged — but
            ;; the bracket state has to be tracked across it to know what an
            ;; escape THERE means
            (else
             (let plain ((j i) (b in?))
               (if (and (< j n) (not (escape-at? j b)))
                   (plain (+ j 1) (bracket-state (string-ref x j) b))
                   (begin (set! out (cons (substring x i j) out))
                          (loop j b))))))))))
;; The authority, the user info and the scheme-specific part can hold a bracketed
;; IPv6 literal, so they keep a scope id's escaped "%"; the path, the query and
;; the fragment cannot, so they decode every escape.
(define (uri-decode-keeping-scope-id x) (uri-decode* x #t))
(define (uri-decode x) (uri-decode* x #f))

;; The parse proper. Every failure goes to `bail` with a reason and an index
;; rather than raising, because two callers want two different exceptions —
;; the constructor a URISyntaxException, URI/create an IllegalArgumentException —
;; and parse-authority itself RETRIES a failed server parse as a registry-based
;; authority, which needs the failure as a value.
(define (uri-parse-1 s bail . opt)
  ;; opt: require-server? — the JDK's requireServerAuthority, set by the component
  ;; constructors that take a host (see uri-of-host). With it a failed
  ;; server-authority parse RAISES instead of falling back to a registry-based
  ;; authority, which is why (URI. "https" nil "h_c.com" -1 "/p" nil nil) is an
  ;; error on the JVM while the 5-arg authority form accepts the same string.
  (let ((n (string-length s))
        (cur-bail bail)
        (require-server? (and (pair? opt) (car opt)))
        (scheme jolt-nil) (ssp-start 0) (authority jolt-nil) (user-info jolt-nil)
        (host jolt-nil) (port -1) (path jolt-nil) (query jolt-nil) (fragment jolt-nil)
        (v6bytes 0))
    (define (fail reason idx) (cur-bail reason idx))
    (define (failx what idx) (cur-bail (string-append "Expected " what) idx))
    (define (at? p e ch) (and (< p e) (char=? (string-ref s p) ch)))
    (define (at2? p e a b) (and (< (+ p 1) e) (char=? (string-ref s p) a) (char=? (string-ref s (+ p 1)) b)))
    ;; scan by character class, honoring escapes when `esc`.
    (define (scan p e pred esc)
      (let loop ((i p))
        (if (>= i e) i
            (let ((c (string-ref s i)))
              (cond ((pred c) (loop (+ i 1)))
                    (esc (let ((q (uri-scan-escape s i e)))
                           (cond ((eq? q 'malformed) (fail "Malformed escape pair" i))
                                 ((> q i) (loop q))
                                 (else i))))
                    (else i))))))
    (define (check p e pred esc what)
      (let ((q (scan p e pred esc)))
        (when (< q e) (fail (string-append "Illegal character in " what) q))))
    ;; scan to the first character of `stop`; -1 if one of `err` comes first.
    (define (scan-until p e err stop)
      (let loop ((i p))
        (cond ((>= i e) i)
              ((uri-in-set? (string-ref s i) err) -1)
              ((uri-in-set? (string-ref s i) stop) i)
              (else (loop (+ i 1))))))
    ;; 1-3 digits whose value fits in a byte.
    (define (scan-byte p e)
      (let ((q (scan p e uri-digit? #f)))
        (if (<= q p) q (if (> (string->number (substring s p q)) 255) p q))))
    ;; A dotted quad. `strict` requires it to consume the whole range. `soft`
    ;; answers #f where the JVM raises "Malformed IPv4 address" — the hostname
    ;; path treats a malformed quad as simply "not an address" and tries a
    ;; hostname instead, which is what parseIPv4Address's catch amounts to.
    (define (scan-ipv4 p e strict soft)
      (let ((m (scan p e uri-digit-dot? #f)))
        (if (or (<= m p) (and strict (not (= m e))))
            #f
            (let loop ((i p) (step 0))
              (cond ((= step 7) (if (= i m) i (if soft #f (fail "Malformed IPv4 address" i))))
                    ((even? step)
                     (let ((q (scan-byte i m)))
                       (if (<= q i) (if soft #f (fail "Malformed IPv4 address" q)) (loop q (+ step 1)))))
                    (else (if (at? i m #\.)
                              (loop (+ i 1) (+ step 1))
                              (if soft #f (fail "Malformed IPv4 address" i)))))))))
    (define (take-ipv4 p e what)
      (let ((q (scan-ipv4 p e #t #f)))
        (if (or (not q) (<= q p)) (failx what p) q)))
    (define (parse-ipv4-address p e)
      (let ((m (scan-ipv4 p e #f #t)))
        (cond ((or (not m) (<= m p)) #f)
              ((and (< m e) (not (char=? (string-ref s m) #\:))) #f)
              (else (set! host (substring s p m)) m))))
    (define (scan-hex-seq p e)
      (let ((q (scan p e uri-hex? #f)))
        (cond ((<= q p) -1)
              ((at? q e #\.) -1)                       ; the start of an IPv4 address
              (else
               (when (> q (+ p 4)) (fail "IPv6 hexadecimal digit sequence too long" p))
               (set! v6bytes (+ v6bytes 2))
               (let loop ((i q))
                 (cond ((>= i e) i)
                       ((not (at? i e #\:)) i)
                       ((at2? i e #\: #\:) i)          ; "::" ends this sequence
                       ((= (+ i 1) e) (fail "Expected digits for an IPv6 address" (+ i 1)))
                       (else
                        (let* ((p2 (+ i 1)) (q2 (scan p2 e uri-hex? #f)))
                          (cond ((<= q2 p2) (failx "digits for an IPv6 address" p2))
                                ((at? q2 e #\.) i)     ; an IPv4 tail; stop at the ":"
                                (else
                                 (when (> q2 (+ p2 4)) (fail "IPv6 hexadecimal digit sequence too long" p2))
                                 (set! v6bytes (+ v6bytes 2))
                                 (loop q2)))))))))))
    (define (scan-hex-post p e)
      (if (= p e)
          p
          (let ((q (scan-hex-seq p e)))
            (if (> q p)
                (if (at? q e #\:)
                    (let ((r (take-ipv4 (+ q 1) e "hex digits or IPv4 address")))
                      (set! v6bytes (+ v6bytes 4)) r)
                    q)
                (let ((r (take-ipv4 p e "hex digits or IPv4 address")))
                  (set! v6bytes (+ v6bytes 4)) r)))))
    (define (parse-ipv6-ref start e)
      (let* ((q (scan-hex-seq start e))
             (compressed #f)
             (p (cond ((> q start)
                       (cond ((at2? q e #\: #\:) (set! compressed #t) (scan-hex-post (+ q 2) e))
                             ((at? q e #\:)
                              (let ((r (take-ipv4 (+ q 1) e "IPv4 address")))
                                (set! v6bytes (+ v6bytes 4)) r))
                             (else q)))
                      ((at2? start e #\: #\:) (set! compressed #t) (scan-hex-post (+ start 2) e))
                      (else start))))
        (when (< p e) (fail "Malformed IPv6 address" start))
        (when (> v6bytes 16) (fail "IPv6 address too long" start))
        (when (and (not compressed) (< v6bytes 16)) (fail "IPv6 address too short" start))
        (when (and compressed (= v6bytes 16)) (fail "Malformed IPv6 address" start))
        p))
    ;; hostname = domainlabel *( "." domainlabel ) [ "." ], and a multi-label
    ;; name must have an alphabetic rightmost label — "1.2.3.4.5" is neither an
    ;; address nor a hostname, so it falls back to a registry authority.
    (define (parse-hostname start e)
      (define (done p l)
        (when (and (< p e) (not (at? p e #\:))) (fail "Illegal character in hostname" p))
        (when (< l 0) (failx "hostname" start))
        (when (and (> l start) (not (uri-alpha? (string-ref s l)))) (fail "Illegal character in hostname" l))
        (set! host (substring s start p))
        p)
      (let loop ((p start) (l -1))
        (let ((q (scan p e uri-alphanum? #f)))
          (if (<= q p)
              (done p l)
              (let* ((q2 (scan q e uri-alphanum-dash? #f))
                     (p2 (if (> q2 q)
                             (begin (when (char=? (string-ref s (- q2 1)) #\-)
                                      (fail "Illegal character in hostname" (- q2 1)))
                                    q2)
                             q)))
                (if (at? p2 e #\.)
                    (let ((p3 (+ p2 1))) (if (< p3 e) (loop p3 p) (done p3 p)))
                    (done p2 p)))))))
    (define (parse-server start e)
      (let* ((q (scan-until start e "/?#" "@"))
             (p (if (and (>= q start) (at? q e #\@))
                    (begin (check start q uri-userinfo-char? #t "user info")
                           (set! user-info (substring s start q))
                           (+ q 1))
                    start))
             (p (if (at? p e #\[)
                    (let* ((b (+ p 1)) (q2 (scan-until b e "/?#" "]")))
                      (if (and (> q2 b) (at? q2 e #\]))
                          ;; A "%" splits the address from a scope id. With no
                          ;; "%" the scan lands on the closing bracket, which is
                          ;; the same thing as the whole range being the address.
                          (let ((m (scan-until b q2 "" "%")))
                            (if (> m b)
                                (begin (parse-ipv6-ref b m)
                                       (when (= (+ m 1) q2) (fail "scope id expected" -1))
                                       (check (+ m 1) q2 uri-alphanum? #f "scope id"))
                                (parse-ipv6-ref b q2))
                            (set! host (substring s p (+ q2 1)))
                            (+ q2 1))
                          (failx "closing bracket for IPv6 address" q2)))
                    (or (parse-ipv4-address p e) (parse-hostname p e))))
             (p (if (at? p e #\:)
                    (let* ((pp (+ p 1)) (q3 (scan-until pp e "" "/")))
                      (if (> q3 pp)
                          (begin (check pp q3 uri-digit? #f "port number")
                                 (let ((v (string->number (substring s pp q3))))
                                   (when (> v 2147483647) (fail "Malformed port number" pp))
                                   (set! port v))
                                 q3)
                          pp))
                    ;; A host that ran to a character other than ":" only gets
                    ;; here from the bracket branch — parse-hostname and the IPv4
                    ;; scan both refuse a trailing anything-else themselves.
                    (begin (when (< p e) (failx "port number" p)) p))))
        p))
    ;; An authority is server-based when it parses as one, and registry-based
    ;; otherwise; only a string that is neither is an error.
    (define (parse-authority start e)
      (let* ((bracket (> (scan-until start e "" "]") start))
             (server-pred (if bracket uri-server%-char? uri-server-char?))
             (server-ok (= (scan start e server-pred #t) e))
             (reg-stop (scan start e uri-reg-name-char? #t))
             (reg-ok (= reg-stop e)))
        (cond
          ((and reg-ok (not server-ok)) (set! authority (substring s start e)))
          (server-ok
           (let* ((outer cur-bail)
                  (err (call/cc (lambda (k)
                                  (set! cur-bail (lambda (r i) (k (cons r i))))
                                  (parse-server start e)
                                  #f))))
             (set! cur-bail outer)
             (cond ((not err) (set! authority (substring s start e)))
                   (else (set! user-info jolt-nil) (set! host jolt-nil) (set! port -1)
                         (if (and reg-ok (not require-server?))
                             (set! authority (substring s start e))
                             (fail (car err) (cdr err)))))))
          (else (fail "Illegal character in authority" reg-stop)))
        e))
    (define (parse-hierarchical start)
      (let* ((p (if (and (at? start n #\/) (at? (+ start 1) n #\/))
                    (let* ((p2 (+ start 2)) (q (scan-until p2 n "" "/?#")))
                      (cond ((> q p2) (parse-authority p2 q))
                            ;; an empty authority is allowed before a non-empty
                            ;; path — "file:///a/b" is the everyday shape.
                            ((< q n) p2)
                            (else (failx "authority" p2))))
                    start))
             (q (scan-until p n "" "?#")))
        (check p q uri-path-char? #t "path")
        (set! path (substring s p q))
        (if (at? q n #\?)
            (let* ((p3 (+ q 1)) (q3 (scan-until p3 n "" "#")))
              (check p3 q3 uri-uric? #t "query")
              (set! query (substring s p3 q3))
              q3)
            q)))
    (let* ((p0 (scan-until 0 n "/?#" ":"))
           (body-end
            (if (and (>= p0 0) (at? p0 n #\:))
                (begin
                  (when (= p0 0) (failx "scheme name" 0))
                  (unless (uri-alpha? (string-ref s 0)) (fail "Illegal character in scheme name" 0))
                  (check 1 p0 uri-scheme-char? #f "scheme name")
                  (set! scheme (substring s 0 p0))
                  (set! ssp-start (+ p0 1))
                  ;; "scheme:/…" is hierarchical, anything else opaque.
                  (if (at? ssp-start n #\/)
                      (parse-hierarchical ssp-start)
                      (let ((q (scan-until ssp-start n "" "#")))
                        (when (<= q ssp-start) (failx "scheme-specific part" ssp-start))
                        (check ssp-start q uri-uric? #t "opaque part")
                        q)))
                (parse-hierarchical 0)))
           (end (if (at? body-end n #\#)
                    (begin (check (+ body-end 1) n uri-uric? #t "fragment")
                           (set! fragment (substring s (+ body-end 1) n))
                           n)
                    body-end)))
      (when (< end n) (failx "end of URI" end))
      ;; Each escapable component is stored TWICE: the raw substring the parse
      ;; produced, and its percent-decoded form, because java.net.URI answers both
      ;; (getPath vs getRawPath) and they are different strings. The scheme, the
      ;; host and the port have no decoded half on the JVM either — a scheme
      ;; cannot hold an escape, and there is no getRawHost.
      (let ((ssp (substring s ssp-start body-end)))
        (make-jhost "uri"
          (list (cons 'string s)
                (cons 'scheme scheme)
                (cons 'ssp ssp)
                (cons 'dec-ssp (uri-decode-keeping-scope-id ssp))
                (cons 'authority authority)
                (cons 'dec-authority (uri-decode-keeping-scope-id authority))
                (cons 'host host)
                (cons 'user-info user-info)
                (cons 'dec-user-info (uri-decode-keeping-scope-id user-info))
                (cons 'port (->num port))
                (cons 'path path)
                (cons 'dec-path (uri-decode path))
                (cons 'query query)
                (cons 'dec-query (uri-decode query))
                (cons 'fragment fragment)
                (cons 'dec-fragment (uri-decode fragment))))))))
(define (uri-parse-either s . opt)
  (call/cc (lambda (k)
             (apply uri-parse-1 s (lambda (reason idx) (k (list 'uri-error reason idx))) opt))))
(define (uri-error? r) (and (pair? r) (eq? (car r) 'uri-error)))
(define (uri-error-message s r)
  (let ((idx (caddr r)))
    (string-append (cadr r) (if (< idx 0) "" (string-append " at index " (number->string idx))) ": " s)))
(define (uri-parse s . opt)
  (let ((r (apply uri-parse-either s opt)))
    (if (uri-error? r)
        (jolt-throw (jolt-host-throwable "java.net.URISyntaxException" (uri-error-message s r)))
        r)))
;; Percent-encode what is illegal in a URI path. File.toURI is new URI(scheme,
;; host, path, fragment) on the JVM, which QUOTES rather than rejects, so a file
;; whose name holds a space is file:/tmp/a%20b and not an invalid URI string.
;; A character above 0x80 is legal unescaped and stays as it is.
(define (uri-hex2 b)
  (let ((d "0123456789ABCDEF"))
    (string (string-ref d (quotient b 16)) (string-ref d (remainder b 16)))))
(define (uri-percent-encode c)
  (let ((bv (string->utf8 (string c))))
    (let loop ((i 0) (acc ""))
      (if (>= i (bytevector-length bv))
          acc
          (loop (+ i 1) (string-append acc "%" (uri-hex2 (bytevector-u8-ref bv i))))))))
(define (uri-quote s ok?)
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
          (apply string-append (reverse acc))
          (let ((c (string-ref s i)))
            (loop (+ i 1)
                  (cons (if (or (ok? c)
                                (and (> (char->integer c) 128)
                                     (not (uri-space-char? c)) (not (uri-iso-control? c))))
                            (string c)
                            (uri-percent-encode c))
                        acc)))))))
(define (uri-quote-path p) (uri-quote p uri-path-char?))
(define (uri-field u k) (let ((p (assq k (jhost-state u)))) (if p (cdr p) jolt-nil)))

;; --- the component constructors (URI. scheme host path fragment) & kin -------
;; The JDK builds these the long way round: compose a URI STRING out of the
;; pieces, quoting each one against the character set its component allows, then
;; run the ordinary parser over the result. That is why they QUOTE where the
;; single-string ctor REJECTS — (URI. "https" "x.example" "/a b" nil) is
;; https://x.example/a%20b, while (URI. "https://x.example/a b") is a
;; URISyntaxException — and why the exception a bad component raises reports an
;; index into the composed string. Composing rather than filling the fields in
;; directly is what keeps the two paths from drifting: every URI jolt hands back
;; came out of one parser.
;;
;; Arities are the JDK's five: (s), (scheme ssp fragment),
;; (scheme host path fragment), (scheme authority path query fragment),
;; (scheme userInfo host port path query fragment). Anything else is
;; "No matching ctor found for class java.net.URI", which is the message JVM
;; Clojure's reflector gives for the same call (jolt#949).
(define (uri-authority-char? c) (or (uri-reg-name-char? c) (uri-server-char? c)))
;; A bracketed IPv6 literal is already in its own syntax and must not be quoted;
;; only what follows the "]" is.
(define (uri-quote-authority a)
  (if (and (> (string-length a) 0) (char=? (string-ref a 0) #\[))
      (let ((end (uri-index-of a #\] 0)))
        (if (and end (uri-index-of a #\: 0))
            (string-append (substring a 0 (+ end 1))
                           (uri-quote (substring a (+ end 1) (string-length a)) uri-authority-char?))
            (uri-quote a uri-authority-char?)))
      (uri-quote a uri-authority-char?)))
;; new URI(...)'s appendAuthority: a host wins over an authority, a host holding
;; a ":" is bracketed as an IPv6 literal, and a port of -1 is "no port".
(define (uri-compose scheme opaque authority user-info host port path query fragment)
  (let ((out '()))
    (define (emit! . xs) (for-each (lambda (x) (set! out (cons x out))) xs))
    (when scheme (emit! scheme ":"))
    (if opaque
        (emit! (uri-quote opaque uri-uric?))
        (begin
          (cond
            (host
             (emit! "//")
             (when user-info (emit! (uri-quote user-info uri-userinfo-char?) "@"))
             (let ((brackets (and (> (string-length host) 0)
                                  (uri-index-of host #\: 0)
                                  (not (char=? (string-ref host 0) #\[))
                                  (not (char=? (string-ref host (- (string-length host) 1)) #\])))))
               (when brackets (emit! "["))
               (emit! host)
               (when brackets (emit! "]")))
             (when (and port (not (= port -1))) (emit! ":" (number->string port))))
            (authority (emit! "//" (uri-quote-authority authority))))
          (when path (emit! (uri-quote path uri-path-char?)))
          (when query (emit! "?" (uri-quote query uri-uric?)))))
    (when fragment (emit! "#" (uri-quote fragment uri-uric?)))
    (apply string-append (reverse out))))
;; A scheme makes the URI absolute, and an absolute URI's path must be rooted —
;; the JDK's checkPath, which catches (URI. "https" "x.example" "a" nil) before
;; the parser turns the missing "/" into a nonsense authority.
(define (uri-check-path! composed scheme path)
  (when (and scheme path (> (string-length path) 0) (not (char=? (string-ref path 0) #\/)))
    (jolt-throw (jolt-host-throwable "java.net.URISyntaxException"
                  (string-append "Relative path in absolute URI: " composed)))))
;; a nil component is absent, not the string "nil"
(define (uri-arg x) (if (or (jolt-nil? x) (not x)) #f (jolt-str-render-one x)))
(define (uri-port-arg x) (if (jolt-nil? x) -1 (jnum->exact x)))
;; (scheme ssp fragment)
(define (uri-of-ssp scheme ssp fragment)
  (uri-parse (uri-compose scheme ssp #f #f #f -1 #f #f fragment) #f))
;; (scheme userInfo host port path query fragment) — and (scheme host path
;; fragment), which the JDK defines as this one with the other three nil. Both
;; name a host, so both require a server authority.
(define (uri-of-host scheme user-info host port path query fragment)
  (let ((composed (uri-compose scheme #f #f user-info host port path query fragment)))
    (uri-check-path! composed scheme path)
    (uri-parse composed #t)))
;; (scheme authority path query fragment) — the authority is taken as given, so a
;; registry-based one ("h_c.com") is legal here where it is not in uri-of-host.
(define (uri-of-authority scheme authority path query fragment)
  (let ((composed (uri-compose scheme #f authority #f #f -1 path query fragment)))
    (uri-check-path! composed scheme path)
    (uri-parse composed #f)))
(define (uri-ctor . args)
  (let ((a (lambda (i) (uri-arg (list-ref args i)))))
    (case (length args)
      ((1) (uri-parse (jolt-str-render-one (car args))))
      ((3) (uri-of-ssp (a 0) (a 1) (a 2)))
      ((4) (uri-of-host (a 0) #f (a 1) -1 (a 2) #f (a 3)))
      ((5) (uri-of-authority (a 0) (a 1) (a 2) (a 3) (a 4)))
      ((7) (uri-of-host (a 0) (a 1) (a 2) (uri-port-arg (list-ref args 3))
                        (a 4) (a 5) (a 6)))
      (else (throw-jvm (quote IllegalArgumentException)
              "No matching ctor found for class java.net.URI")))))
(register-class-ctor! "URI" uri-ctor)
(register-class-ctor! "java.net.URI" uri-ctor)
;; URI/create — the (URI. s) constructor with the checked URISyntaxException
;; rewrapped as an unchecked IllegalArgumentException, as the JVM's does.
(define (uri-create s)
  (let ((r (uri-parse-either s)))
    (if (uri-error? r)
        (throw-jvm (quote IllegalArgumentException) (uri-error-message s r))
        r)))
(register-class-statics! "java.net.URI" (list (cons "create" (lambda (s) (uri-create (jolt-str-render-one s))))))
(register-host-methods! "uri"
  ;; The getX / getRawX pairs answer DIFFERENT strings: raw is the substring the
  ;; parse produced, getX is that percent-decoded. They used to share one field,
  ;; so getPath on "https://h.com/a%20b" answered "/a%20b" where the JVM answers
  ;; "/a b" — the last divergence the java.net.URI differential run found
  ;; (jolt-6i6). getScheme, getHost and getPort have no raw counterpart on the
  ;; JVM and are unchanged.
  (list (cons "toString" (lambda (u) (uri-field u 'string)))
        (cons "toASCIIString" (lambda (u) (uri-field u 'string)))
        (cons "getScheme" (lambda (u) (uri-field u 'scheme)))
        (cons "getSchemeSpecificPart" (lambda (u) (uri-field u 'dec-ssp)))
        (cons "getRawSchemeSpecificPart" (lambda (u) (uri-field u 'ssp)))
        (cons "getAuthority" (lambda (u) (uri-field u 'dec-authority)))
        (cons "getRawAuthority" (lambda (u) (uri-field u 'authority)))
        (cons "getHost" (lambda (u) (uri-field u 'host)))
        (cons "getUserInfo" (lambda (u) (uri-field u 'dec-user-info)))
        (cons "getRawUserInfo" (lambda (u) (uri-field u 'user-info)))
        (cons "getPort" (lambda (u) (uri-field u 'port)))
        (cons "getPath" (lambda (u) (uri-field u 'dec-path)))
        (cons "getRawPath" (lambda (u) (uri-field u 'path)))
        (cons "getQuery" (lambda (u) (uri-field u 'dec-query)))
        (cons "getRawQuery" (lambda (u) (uri-field u 'query)))
        (cons "getFragment" (lambda (u) (uri-field u 'dec-fragment)))
        (cons "getRawFragment" (lambda (u) (uri-field u 'fragment)))
        ;; URI.toURL = new URL(toString()) (JVM); honors a library-registered
        ;; URL shim like io/as-url does.
        (cons "toURL" (lambda (u) (let ((ctor (lookup-class class-ctors-tbl "URL")))
                                    (if ctor (ctor (uri-field u 'string))
                                        (make-url (uri-field u 'string))))))
        (cons "isAbsolute" (lambda (u) (not (jolt-nil? (uri-field u 'scheme)))))
        (cons "isOpaque" (lambda (u) (uri-opaque? u)))
        (cons "resolve" (lambda (u x) (uri-resolve u (uri-arg->uri x))))
        (cons "normalize" (lambda (u) (uri-normalize u)))
        (cons "relativize" (lambda (u x) (uri-relativize u (uri-arg->uri x))))
        (cons "compareTo" (lambda (u o) (let ((a (uri-field u 'string)) (b (uri-field o 'string)))
                                          (cond ((string<? a b) -1) ((string=? a b) 0) (else 1)))))
        (cons "hashCode" (lambda (u) (string-hash (uri-field u 'string))))
        (cons "equals" (lambda (u o) (and (jhost? o) (string=? (jhost-tag o) "uri")
                                          (string=? (uri-field u 'string) (uri-field o 'string)))))))

;; --- resolve / normalize / relativize: RFC 2396 §5.2, as java.net.URI does it --
;; A URI is rebuilt from its RAW components — the substrings the parse produced,
;; escapes intact — as scheme ":" ["//" authority] path ["?" query] ["#" fragment]
;; and parsed again, which is what java.net.URI.toString does for a URI it
;; constructed itself (defineString), and which keeps every URI jolt hands back
;; the product of the one parser.
(define (uri-opaque? u)
  (and (not (jolt-nil? (uri-field u 'scheme)))
       (let ((ssp (uri-field u 'ssp)))
         (or (= (string-length ssp) 0) (not (char=? (string-ref ssp 0) #\/))))))
(define (uri-nil->f x) (if (jolt-nil? x) #f x))
(define (uri-arg->uri x) (if (uri-jhost? x) x (uri-create (jolt-str-render-one x))))
(define (uri-from-parts scheme authority path query fragment)
  (uri-parse (string-append (if scheme (string-append scheme ":") "")
                            (if authority (string-append "//" authority) "")
                            (or path "")
                            (if query (string-append "?" query) "")
                            (if fragment (string-append "#" fragment) ""))))
;; RFC 2396 §5.2 (6c-f), java.net.URI.normalize(String): "." segments go, a ".."
;; removes the segment before it unless that is itself a ".." or there is none
;; (a leading ".." stays — 6g leaves the path as it is), and a kept segment
;; keeps the slash that FOLLOWED it in the original, which is how "/a/b/.."
;; normalizes to "/a/" while "/a/b/../../.." is "/.." and "a/.." is "" (the
;; JDK's join step). A RELATIVE path whose first segment holds a ":" gains a
;; "./" so it cannot be read back as a scheme.
(define (uri-normalize-path path)
  (if (or (not path) (= (string-length path) 0))
      path
      (let* ((n (string-length path))
             (absolute? (char=? (string-ref path 0) #\/))
             ;; (segment . followed-by-slash?) in order, empty segments dropped
             (segs (let loop ((i 0) (start 0) (acc '()))
                     (cond ((= i n) (reverse (if (> i start) (cons (cons (substring path start i) #f) acc) acc)))
                           ((char=? (string-ref path i) #\/)
                            (loop (+ i 1) (+ i 1) (if (> i start) (cons (cons (substring path start i) #t) acc) acc)))
                           (else (loop (+ i 1) start acc)))))
             (kept (let loop ((ss segs) (acc '()))
                     (cond ((null? ss) (reverse acc))
                           ((string=? (car (car ss)) ".") (loop (cdr ss) acc))
                           ((and (string=? (car (car ss)) "..") (pair? acc) (not (string=? (car (car acc)) "..")))
                            (loop (cdr ss) (cdr acc)))
                           (else (loop (cdr ss) (cons (car ss) acc))))))
             (body (apply string-append
                          (map (lambda (sg) (string-append (car sg) (if (cdr sg) "/" ""))) kept)))
             (body (if (and (not absolute?) (pair? kept) (uri-index-of (car (car kept)) #\: 0))
                       (string-append "./" body)
                       body)))
        (string-append (if absolute? "/" "") body))))
(define (uri-normalize u)
  (if (uri-opaque? u)
      u
      (let* ((path (uri-nil->f (uri-field u 'path)))
             (np (uri-normalize-path path)))
        (if (equal? np path)
            u
            (uri-from-parts (uri-nil->f (uri-field u 'scheme)) (uri-nil->f (uri-field u 'authority))
                            np (uri-nil->f (uri-field u 'query)) (uri-nil->f (uri-field u 'fragment)))))))
;; java.net.URI.resolve(URI base, URI child): an opaque side answers the child;
;; a lone fragment is the base with that fragment (5.2 (2)); an absolute child
;; is itself (3); a child with an authority replaces everything but the scheme
;; (4); a child path from "/" replaces the base's (5); anything else is merged
;; onto the base path's directory and normalized (6).
(define (uri-resolve base child)
  (let ((c-scheme (uri-nil->f (uri-field child 'scheme)))
        (c-auth (uri-nil->f (uri-field child 'authority)))
        (c-path (or (uri-nil->f (uri-field child 'path)) ""))
        (c-query (uri-nil->f (uri-field child 'query)))
        (c-frag (uri-nil->f (uri-field child 'fragment)))
        (b-scheme (uri-nil->f (uri-field base 'scheme)))
        (b-auth (uri-nil->f (uri-field base 'authority)))
        (b-path (or (uri-nil->f (uri-field base 'path)) ""))
        (b-query (uri-nil->f (uri-field base 'query)))
        (b-frag (uri-nil->f (uri-field base 'fragment))))
    (cond
      ((or (uri-opaque? child) (uri-opaque? base)) child)
      ((and (not c-scheme) (not c-auth) (= (string-length c-path) 0) c-frag (not c-query))
       (if (and b-frag (string=? b-frag c-frag))
           base
           (uri-from-parts b-scheme b-auth b-path b-query c-frag)))
      (c-scheme child)
      (c-auth (uri-from-parts b-scheme c-auth c-path c-query c-frag))
      ((and (> (string-length c-path) 0) (char=? (string-ref c-path 0) #\/))
       (uri-from-parts b-scheme b-auth c-path c-query c-frag))
      (else
       ;; the base path's directory, then the child; a base with an authority and
       ;; no path merges as "/" (RFC 3986 §5.2.3, and the JDK since 20), so
       ;; "a" against "https://h.com" is "https://h.com/a", not "https://h.coma"
       (let* ((i (let loop ((k (- (string-length b-path) 1)))
                   (cond ((< k 0) #f) ((char=? (string-ref b-path k) #\/) k) (else (loop (- k 1))))))
              (dir (cond (i (substring b-path 0 (+ i 1)))
                         ((and b-auth (= (string-length b-path) 0) (> (string-length c-path) 0)) "/")
                         (else "")))
              (merged (string-append dir c-path)))
         (uri-from-parts b-scheme b-auth (uri-normalize-path merged) c-query c-frag))))))
;; java.net.URI.relativize: the child, unless both are hierarchical with the same
;; scheme and authority and the base's normalized path is a prefix of the
;; child's at a segment boundary — then the remainder, with the child's query
;; and fragment.
(define (uri-relativize base child)
  (let ((same? (lambda (a b ci?) (or (and (not a) (not b))
                                     (and a b (if ci? (string-ci=? a b) (string=? a b)))))))
    (if (or (uri-opaque? base) (uri-opaque? child)
            (not (same? (uri-nil->f (uri-field base 'scheme)) (uri-nil->f (uri-field child 'scheme)) #t))
            (not (same? (uri-nil->f (uri-field base 'authority)) (uri-nil->f (uri-field child 'authority)) #f)))
        child
        (let* ((bp (uri-normalize-path (or (uri-nil->f (uri-field base 'path)) "")))
               (cp (uri-normalize-path (or (uri-nil->f (uri-field child 'path)) "")))
               ;; equal paths relativize to the empty path; otherwise the base
               ;; must be a whole-segment prefix
               (bp (cond ((string=? bp cp) bp)
                         ((and (> (string-length bp) 0)
                               (char=? (string-ref bp (- (string-length bp) 1)) #\/)) bp)
                         (else (string-append bp "/")))))
          (if (and (>= (string-length cp) (string-length bp))
                   (string=? (substring cp 0 (string-length bp)) bp))
              (uri-from-parts #f #f (substring cp (string-length bp) (string-length cp))
                              (uri-nil->f (uri-field child 'query)) (uri-nil->f (uri-field child 'fragment)))
              child)))))
;; (= f1 f2) is value equality by pathname, like java.io.File.equals — .equals
;; and hash already agreed, so two Files built from the same path compared equal
;; through the method and unequal through =, which is how ring's resource tests
;; read (not (= #object[java.io.File "…/foo.html"] #object[java.io.File "…/foo.html"])).
(register-eq-arm! (lambda (a b) (or (jfile? a) (jfile? b)))
                  (lambda (a b) (and (jfile? a) (jfile? b)
                                     (string=? (jfile-path a) (jfile-path b)))))
;; File.hashCode is the file system's: path.hashCode() ^ 1234321, over the
;; lowercased path on Windows. Chez's string-hash is not a 32-bit int, so
;; .hashCode answered numbers no JVM can produce.
(define (jfile-hashcode f)
  (let ((p (jfile-path f)))
    (bitwise-xor (java-string-hashcode (if (eq? (sa-os-family) 'windows) (string-downcase p) p))
                 1234321)))
(register-hash-arm! jfile? jfile-hashcode)

;; (= u1 u2) is value equality by string form (the .equals method above only
;; serves explicit (.equals …)); hash matches so a URI works as a map key / set
;; member (ring/hiccup compare (URI. "/") values).
(define (uri-jhost? x) (and (jhost? x) (string=? (jhost-tag x) "uri")))
(register-eq-arm! (lambda (a b) (or (uri-jhost? a) (uri-jhost? b)))
                  (lambda (a b) (and (uri-jhost? a) (uri-jhost? b)
                                     (string=? (uri-field a 'string) (uri-field b 'string)))))
(register-hash-arm! uri-jhost? (lambda (x) (string-hash (uri-field x 'string))))
;; A URL compares by its canonical spec. The JVM's URL.equals also resolves both
;; hosts and compares addresses (a blocking DNS lookup inside =); jolt does not,
;; so two names for one host stay unequal here.
(register-eq-arm! (lambda (a b) (or (url-jhost? a) (url-jhost? b)))
                  (lambda (a b) (and (url-jhost? a) (url-jhost? b)
                                     (string=? (url-spec a) (url-spec b)))))
(register-hash-arm! url-jhost? (lambda (x) (java-string-hashcode (url-spec x))))
;; (compare u1 u2) / (sort uris): URI is Comparable on the JVM, by string form
;; (its compareTo compares component-wise, which for two well-formed URIs is the
;; same order as the strings up to the first differing component).
(register-compare-arm! (lambda (a b) (and (uri-jhost? a) (uri-jhost? b)))
                       (lambda (a b) (let ((x (uri-field a 'string)) (y (uri-field b 'string)))
                                       (cond ((string<? x y) -1) ((string=? x y) 0) (else 1)))))
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
