;; nio-file.ss — java.nio.file shim, part 1: the Path value + Paths / Path / of
;; construction, FileSystems path construction and glob/regex matching, and the
;; File<->Path bridge. babashka.fs is built entirely on java.nio.file, so this
;; is the substrate it runs on.
;;
;; A Path is a jhost tagged "nio-path" whose state is (filesystem . path-string),
;; normalized as the JDK's path parsers do at construction: runs of separators
;; collapse and a trailing one is dropped, and on Windows it is held with "/" and
;; rendered with "\" (java/io.ss jolt-path-normalize, path-native). Like
;; java.nio.file, a Path is just a name until an operation touches disk —
;; resolution against the working directory happens in toAbsolutePath / the Files
;; layer, not at construction.
;;
;; Loaded from rt.ss after java/io.ss (needs make-jfile / jfile? / jfile-abs).

;; Files' member alists, collected in REVERSE and flattened once at the bottom.
;; Eleven chunks are appended over the file; growing the accumulation with
;; (append files-accum chunk) copied everything gathered so far each time. The
;; flush restores source order, which matters: the dedup below is last-wins.
(define files-accum-chunks '())

;; ---- path string algebra ----------------------------------------------------
;; A Path is a name, and a name has a ROOT: a prefix that is not a segment and
;; must be reproduced verbatim. POSIX has one ("/"); Windows has "C:/" (drive),
;; "//srv/sh/" (UNC), "/" (rooted on the process's current drive) and "C:"
;; (rooted on that drive's current directory). This file assumed the POSIX shape
;; everywhere — the root was one leading "/" and nothing else — so on Windows a
;; drive path was a RELATIVE path whose first segment happened to be "C:".
;; (fs/absolute? "C:/Windows/System32") answered false, getRoot answered nil,
;; getParent walked off the drive letter and normalize could fold a path above
;; its own root (jolt-lang/jolt#1074).
;;
;; java/io.ss already owns this split for getCanonicalPath — path-root,
;; path-segments and path-rebuild, with the platform a parameter so
;; win-path-test.ss can pin the Windows rows from a Linux runner — so the Path
;; shim asks IT rather than keeping a second, POSIX-only answer. Two hand-kept
;; copies is how java.io and java.nio.file start disagreeing about a path, which
;; is the same reason the access(2) predicate there is one function for all six
;; of its callers.
(define (nio-windows?) (eq? (sa-os-family) 'windows))

;; isAbsolute is NOT "has a root". On Windows "\x" is rooted on whichever drive
;; the process is on and "C:x" on that drive's current directory; the JVM calls
;; neither absolute, because neither names a file by itself. getRoot answers for
;; both, which is why the two questions stay separate here.
(define (npath-absolute-for? windows? s) (jfile-path-absolute-for? windows? s))

(define (npath-root-for windows? s) (path-root windows? s))

(define (npath-segs-for windows? s) (path-segments windows? s))

;; path-rebuild answers "." for a rootless path with no segments, which is what
;; a canonical path wants; java.nio.file's empty path is "".
(define (npath-rebuild root segs)
  (if (and (null? segs) (string=? root "")) "" (path-rebuild root segs)))
;; The same over a path parsed by java/io.ss's path-parse, which is how the
;; getters below read a root and its segments from one scan rather than three.
(define (npath-render pp segs) (npath-rebuild (ppath-root pp) segs))

;; java.nio.file normalize: drop ".", resolve ".." against the preceding
;; segment. A ".." that reaches past a ROOT is dropped — a rooted path stays
;; rooted — while one that reaches past a relative path survives, because
;; "../x" is a real name. (io.ss's fold-dot-segments drops it either way, which
;; is right for a canonical path, since a canonical path is always rooted.)
(define (npath-fold-dots rooted? segs)
  (let loop ((ss segs) (stack '()))
    (if (null? ss)
        (reverse stack)
        (let ((seg (car ss)))
          (cond
            ((string=? seg ".") (loop (cdr ss) stack))
            ((string=? seg "..")
             (cond
               ((and (pair? stack) (not (string=? (car stack) "..")))
                (loop (cdr ss) (cdr stack)))
               (rooted? (loop (cdr ss) stack))
               (else (loop (cdr ss) (cons ".." stack)))))
            (else (loop (cdr ss) (cons seg stack))))))))

(define (npath-normalize-for windows? s)
  (let ((pp (path-parse windows? s)))
    (npath-render pp (npath-fold-dots (ppath-rooted? pp) (ppath-segs pp)))))

;; Paths.get(first, more...) — join with the separator, no normalization. The
;; varargs `more` arrives as a jolt String[] (from into-array); spread it.
(define (npath-spread-args args)
  (apply append (map (lambda (x) (if (jolt-array? x) (ja->list x) (list x))) args)))
;; A joined path keeps the separator the left side is already spelled with
;; (java/io.ss path-join-sep), so a native "C:\\Users\\x" gains "\\" and everything
;; else gains "/"; a separator the caller already wrote — either kind, on
;; Windows — is not doubled.
(define (npath-join-for windows? parts)
  (fold-left (lambda (acc p)
               (cond ((string=? p "") acc)
                     ((string=? acc "") p)
                     ((path-sep-for? windows? (string-ref acc (- (string-length acc) 1)))
                      (string-append acc p))
                     (else (string-append acc (path-join-sep windows? acc) p))))
             "" parts))
(define (npath-get first . more)
  (if (and (null? more) (uri-jhost? first))
      (nio-uri-path first)
      (npath-get-strings first more)))
;; Path.of(URI): the provider installed for the URI's scheme answers it, as
;; FileSystemProvider.getPath(URI) does on the JDK; "file" is the host's own.
(define (nio-uri-path uri)
  (let* ((scheme (uri-field uri 'scheme))
         (provider (and (string? scheme) (nio-scheme-provider scheme))))
    (if provider
        ((nio-provider-uri->path provider) uri)
        (make-nio-path (npath-join-for (nio-windows?) (list (nio-uri->path uri)))))))
(define (npath-get-strings first more)
  (npath-get-strings-on default-nio-filesystem first more))
(define (npath-get-strings-on fs first more)
  (make-nio-path-on fs (npath-join-for (nio-fs-windows? fs)
                                       (cons (npath-string-of first)
                                             (map npath-string-of (npath-spread-args more))))))

;; Path.resolve(other): an absolute other replaces this; else concatenate.
;; A Windows other that is ROOTED but not absolute ("\b") is neither: it names
;; the current drive, so the JVM resolves it against THIS path's root — "C:/a"
;; resolve "\b" is "C:/b", not "C:/a/\b".
(define (npath-resolve-for windows? a b)
  (cond
    ((string=? b "") a)
    ((npath-absolute-for? windows? b) b)
    (else
     ;; b's root, read once and kept. This asked "is b rooted", which rendered
     ;; the root and dropped it, and the rooted branch below then read b again.
     ;; b is NOT split into segments here: the branch taken by almost every
     ;; caller concatenates and never needs them.
     (let ((broot (npath-root-for windows? b)))
       (cond
         ((not (string=? broot ""))
          (let ((aroot (npath-root-for windows? a)))
            (if (string=? aroot "")
                b
                (npath-rebuild aroot (npath-segs-for windows? b)))))
         ((string=? a "") b)
         ((path-sep-for? windows? (string-ref a (- (string-length a) 1))) (string-append a b))
         (else (string-append a (path-join-sep windows? a) b)))))))
(define (npath-same-provider! self other)
  (unless (nio-path? other)
    (throw-jvm 'ClassCastException "argument is not java.nio.file.Path"))
  (unless (eq? (nio-path-filesystem self) (nio-path-filesystem other))
    (throw-jvm 'ProviderMismatchException "paths belong to different filesystems")))
(define (npath-resolve self other)
  (when (nio-path? other) (npath-same-provider! self other))
  (npath-like self (npath-resolve-for (nio-path-windows? self)
                                      (nio-path-str self) (npath-string-of other))))

;; Path.relativize(other): the path from self to other. The JVM demands both be
;; rooted the same way and throws IllegalArgumentException otherwise; this
;; answers a best-effort path instead, as it always has — callers here (glob and
;; walk in babashka.fs) pass two paths from the same walk.
(define (npath-relativize-for windows? a b)
  (let loop ((x (npath-segs-for windows? a)) (y (npath-segs-for windows? b)))
    (if (and (pair? x) (pair? y) (string=? (car x) (car y)))
        (loop (cdr x) (cdr y))
        (npath-rebuild "" (append (map (lambda (_) "..") x) y)))))
(define (npath-relativize self other)
  (npath-same-provider! self other)
  (npath-like self (npath-relativize-for (nio-path-windows? self)
                                         (nio-path-str self) (npath-string-of other))))

;; The parent of the last segment under a root is the ROOT ("C:/a" -> "C:/"),
;; and a root has no parent. This answered "/" for every rooted path, so on
;; Windows the parent of "C:/a" was "/" — a directory on another drive.
(define (npath-parent-for windows? s)
  (let* ((pp (path-parse windows? s)) (segs (ppath-segs pp)))
    (cond
      ((null? segs) jolt-nil)
      ((null? (cdr segs)) (if (ppath-rooted? pp) (ppath-root pp) jolt-nil))
      (else (npath-render pp (reverse (cdr (reverse segs))))))))
(define (npath-parent-on fs s)
  (let ((r (npath-parent-for (nio-fs-windows? fs) s)))
    (if (string? r) (make-nio-path-on fs r) r)))

(define (npath-file-name-on fs s)
  (let ((segs (npath-segs-for (nio-fs-windows? fs) s)))
    (if (null? segs) jolt-nil (make-nio-path-on fs (car (reverse segs))))))
(define (npath-file-name s) (npath-file-name-on default-nio-filesystem s))

;; ---- the Path jhost ---------------------------------------------------------
;; Paths.get("a//b/") is "a/b" on the JDK, as new File("a//b/") already was here;
;; the Path kept the string as given, so the two disagreed about one name, and a
;; Path built from a tmpdir ending in "/" rendered "T//x".
;;
;; One place Path and File normalize differently, and it is the JDK's: a UNC root
;; alone keeps its trailing separator as a Path ("\\\\srv\\sh\\", what
;; WindowsPathParser answers) where java.io.File drops it ("\\\\srv\\sh").
(define (npath-held-for windows? s . refuse-nul)
  (let ((n (if (null? refuse-nul)
               (jolt-path-normalize-for windows? s)
               (jolt-path-normalize-for/nul windows? s))))
    (when (eq? n 'nul) (npath-refuse-nul windows? s))
    (if (and windows? (fx>? (string-length n) 2) (string=? (substring n 0 2) "//"))
        (let ((pp (path-parse #t n)))
          (if (null? (ppath-segs pp)) (ppath-root pp) n))
        n)))
(define default-nio-filesystem (make-jhost "nio-filesystem" #f))

;; ---- filesystem providers ------------------------------------------------------
;; A Path carries its FileSystem, and every FileSystem answers to one provider:
;; the record of operations that Files calls and the filesystem-specific Path
;; methods dispatch through, as java.nio.file.spi.FileSystemProvider is on the
;; JDK. The default provider is the host's own filesystem, assembled from the
;; Files chunks below; another filesystem (the embedded-resource archive,
;; embedded-archive.ss) registers its own. Nothing outside a provider asks which
;; filesystem a path is on.
;;
;;   name      the URI scheme it serves ("file", "jar")
;;   windows?  thunk: does it parse and render paths the Windows way
;;   render    path string -> what toString answers
;;   files     Files member name -> procedure taking the Files arguments
;;   path-ops  Path member name -> procedure (path . args): toUri, toFile,
;;             toAbsolutePath, toRealPath, register
;;   uri->path a URI of this scheme -> its Path (Path.of(URI))
;;   new-file-system / get-file-system  FileSystems.newFileSystem(URI, env) and
;;             getFileSystem(URI) for this scheme
;;   read-bytes  path -> its contents as a bytevector, what the text and
;;             comparison operations every provider shares read
(define-record-type nio-provider
  (fields name windows? render files path-ops uri->path new-file-system get-file-system read-bytes)
  (nongenerative jolt-nio-provider-v1))
(define (nio-alist->table alist)
  (let ((t (make-hashtable string-hash string=?)))
    (for-each (lambda (p) (hashtable-set! t (car p) (cdr p))) alist)
    t))
;; The host filesystem's provider. Its Files table is filled once, from the
;; accumulated chunks, at the bottom of this file; its Path members a few lines
;; below the Path methods.
(define default-nio-provider
  (make-nio-provider "file" nio-windows? (lambda (s) (path-native s))
                     (make-hashtable string-hash string=?) (make-hashtable string-hash string=?)
                     #f
                     (lambda (uri . env) (zip-throw "java.nio.file.FileSystemAlreadyExistsException" jolt-nil))
                     (lambda (uri) default-nio-filesystem)
                     (lambda (p) (nio-read-bv (nfp p) (nio-shown p)))))
(define nio-fs-providers (make-hashtable string-hash string=?))      ; FileSystem tag -> provider
(define nio-scheme-providers (make-hashtable string-ci-hash string-ci=?)) ; URI scheme -> provider
(hashtable-set! nio-scheme-providers "file" default-nio-provider)
(define (register-nio-provider! fs-tag provider)
  (hashtable-set! nio-fs-providers fs-tag provider)
  (hashtable-set! nio-scheme-providers (nio-provider-name provider) provider))
(define (nio-scheme-provider scheme)
  (let ((p (hashtable-ref nio-scheme-providers scheme #f)))
    (and p (not (eq? p default-nio-provider)) p)))
(define (nio-fs-provider fs)
  (if (eq? fs default-nio-filesystem)
      default-nio-provider
      (hashtable-ref nio-fs-providers (jhost-tag fs) default-nio-provider)))
(define (nio-fs-windows? fs) ((nio-provider-windows? (nio-fs-provider fs))))

;; A path string with a NUL names no file: the JDK's UnixPath (and WindowsPath,
;; in its own words) refuses it as it parses, so Paths/get, Path/of, File.toPath
;; and resolve all raise rather than hand the C layer a path it would truncate
;; at the NUL, acting on another file. The normalizing scan finds it, so a
;; well-formed path pays no second pass.
(define (npath-refuse-nul windows? s)
  (throw-jvm 'java.nio.file.InvalidPathException
             (if windows?
                 (let loop ((i 0))
                   (if (char=? (string-ref s i) #\nul)
                       (string-append "Illegal char <" (string #\nul) "> at index "
                                      (number->string i) ": " s)
                       (loop (fx+ i 1))))
                 (string-append "Nul character not allowed: " s))))
(define (make-nio-path-on fs s)
  (make-jhost "nio-path"
              (cons fs (let ((w? (nio-fs-windows? fs)) (str (if (string? s) s (npath-string-of s))))
                         (if (eq? fs default-nio-filesystem)
                             (npath-held-for w? str #t)
                             (npath-held-for w? str))))))
(define (make-nio-path s) (make-nio-path-on default-nio-filesystem s))
(define-jhost-tag-pred (nio-path? t) (string=? t "nio-path"))
(define (nio-path-str p) (cdr (jhost-state p)))
(define (nio-path-filesystem p) (car (jhost-state p)))
(define (nio-path-provider p) (nio-fs-provider (nio-path-filesystem p)))
(define (nio-path-windows? p) (nio-fs-windows? (nio-path-filesystem p)))
(define (nio-path-render p) ((nio-provider-render (nio-path-provider p)) (nio-path-str p)))
(define (npath-like self s) (make-nio-path-on (nio-path-filesystem self) s))
;; A Path member only some filesystems answer differently: the provider's own, or
;; the host filesystem's.
(define (nio-path-op p name)
  (or (hashtable-ref (nio-provider-path-ops (nio-path-provider p)) name #f)
      (hashtable-ref (nio-provider-path-ops default-nio-provider) name #f)))

;; A path string of any value: a Path -> its string, a File -> its path, else str.
(define (npath-string-of x)
  (cond ((nio-path? x) (nio-path-str x))
        ((jfile? x) (jfile-path x))
        (else (jolt-str-render-one x))))

;; The ROOTS must match, not merely the absoluteness: "C:/a" does not start with
;; "D:/", and both are rooted. Comparing the roots also makes the answer
;; independent of how the caller spelled its separators, since path-root renders
;; both kinds as "/".
(define (npath-starts-with-for windows? a b)
  ;; the empty path is a single empty component: only another empty path starts with it
  (if (string=? b "") (string=? a "")
      (let ((pa (path-parse windows? a)) (pb (path-parse windows? b)))
        (and (string=? (ppath-root pa) (ppath-root pb))
             (let loop ((sa (ppath-segs pa)) (sb (ppath-segs pb)))
               (cond ((null? sb) #t)
                     ((null? sa) #f)
                     ((string=? (car sa) (car sb)) (loop (cdr sa) (cdr sb)))
                     (else #f)))))))
(define (npath-starts-with self other)
  (and (or (not (nio-path? other))
           (eq? (nio-path-filesystem self) (nio-path-filesystem other)))
       (npath-starts-with-for (nio-path-windows? self) (nio-path-str self) (npath-string-of other))))

(define (npath-ends-with-for windows? a b)
  ;; b is parsed once and serves both branches -- the rooted one normalizes it
  ;; from the parse in hand rather than handing the string back to be re-read.
  (let ((pb (path-parse windows? b)))
    (if (ppath-rooted? pb)
        (string=? (npath-normalize-for windows? a)
                  (npath-render pb (npath-fold-dots #t (ppath-segs pb))))
        (let loop ((sa (reverse (npath-segs-for windows? a)))
                   (sb (reverse (ppath-segs pb))))
          (cond ((null? sb) #t)
                ((null? sa) #f)
                ((string=? (car sa) (car sb)) (loop (cdr sa) (cdr sb)))
                (else #f))))))
(define (npath-ends-with self other)
  (and (or (not (nio-path? other))
           (eq? (nio-path-filesystem self) (nio-path-filesystem other)))
       (npath-ends-with-for (nio-path-windows? self) (nio-path-str self) (npath-string-of other))))

;; Path.compareTo. UnixPath compares the paths' bytes, unsigned: the first
;; difference, else the length difference -- a magnitude, not a sign. WindowsPath
;; ignores case, as the file system does. Its argument is cast to a Path, so a
;; File (or a string) is a ClassCastException, not a comparison.
(define (npath-compare-for windows? s o)
  (if windows?
      (jvm-string-ci-compare s o)
      (let* ((x (string->utf8 s)) (y (string->utf8 o))
             (nx (bytevector-length x)) (ny (bytevector-length y)))
        (let loop ((i 0))
          (cond ((or (fx=? i nx) (fx=? i ny)) (fx- nx ny))
                ((fx=? (bytevector-u8-ref x i) (bytevector-u8-ref y i)) (loop (fx+ i 1)))
                (else (fx- (bytevector-u8-ref x i) (bytevector-u8-ref y i))))))))

(define (nio-path-method self name rest)   ; -> boxed result, or #f to fall through
  (let ((s (nio-path-str self))
        (windows? (nio-path-windows? self)))
    (cond
      ((string=? name "toString")      (list (nio-path-render self)))
      ((string=? name "getFileName")   (list (npath-file-name-on (nio-path-filesystem self) s)))
      ((string=? name "getParent")     (list (npath-parent-on (nio-path-filesystem self) s)))
      ((string=? name "getName")       (list (let ((segs (npath-segs-for windows? s)) (i (exact (truncate (car rest)))))
                                               (npath-like self (list-ref segs i)))))
      ((string=? name "getNameCount")  (list (length (npath-segs-for windows? s))))
      ((string=? name "getRoot")       (list (let ((r (npath-root-for windows? s)))
                                               (if (string=? r "") jolt-nil (npath-like self r)))))
      ((string=? name "getFileSystem") (list (nio-path-filesystem self)))
      ((string=? name "normalize")     (list (npath-like self (npath-normalize-for windows? s))))
      ((string=? name "resolve")       (list (fold-left npath-resolve self (npath-spread-args rest))))
      ((string=? name "resolveSibling")(list (let ((par (npath-parent-on (nio-path-filesystem self) s)))
                                               (npath-resolve (if (jolt-nil? par) (npath-like self "") par) (car rest)))))
      ((string=? name "relativize")    (list (npath-relativize self (car rest))))
      ((member name '("toAbsolutePath" "toRealPath" "toFile" "toUri" "register"))
       (list (apply (nio-path-op self name) self rest)))
      ((string=? name "startsWith")    (list (npath-starts-with self (car rest))))
      ((string=? name "endsWith")      (list (npath-ends-with self (car rest))))
      ((string=? name "isAbsolute")    (list (npath-absolute-for? windows? s)))
      ((string=? name "subpath")       (list (let ((segs (npath-segs-for windows? s))
                                                   (b (exact (truncate (car rest))))
                                                   (e (exact (truncate (cadr rest)))))
                                               (npath-like self (npath-rebuild "" (list-head (list-tail segs b) (- e b)))))))
      ((string=? name "compareTo")     (list (begin
                                               (npath-same-provider! self (car rest))
                                               (npath-compare-for windows? s (nio-path-str (car rest))))))
      ((string=? name "equals")        (list (and (nio-path? (car rest))
                                                 (eq? (nio-path-filesystem self) (nio-path-filesystem (car rest)))
                                                 (string=? s (nio-path-str (car rest))))))
      ((string=? name "hashCode")      (list (equal-hash (jhost-state self))))
      ((string=? name "iterator")      (list (list->cseq (map (lambda (segment) (npath-like self segment))
                                                              (npath-segs-for windows? s)))))
      (else #f))))

;; The host filesystem's answers to the Path members a provider owns.
(for-each
 (lambda (p) (hashtable-set! (nio-provider-path-ops default-nio-provider) (car p) (cdr p)))
 (list
  (cons "toAbsolutePath"
        (lambda (self)
          (let ((s (nio-path-str self)))
            (npath-like self (if (npath-absolute-for? (nio-path-windows? self) s) s (jfile-abs s))))))
  (cons "toRealPath"
        (lambda (self . opts)
          (let* ((s (nio-path-str self))
                 (windows? (nio-path-windows? self))
                 (abs (if (npath-absolute-for? windows? s) s (jfile-abs s)))
                 (fp (project-relative abs))
                 (rp (nio-realpath fp)))
            (cond (rp (npath-like self rp))
                  ((file-exists? fp) (npath-like self (npath-normalize-for windows? abs)))
                  (else (jolt-throw (jolt-ex-info abs empty-pmap)))))))   ; missing path throws
  (cons "toFile" (lambda (self) (make-jfile (nio-path-str self))))
  ;; a java.net.URI, as File.toURI answers (io.ss jfile->uri)
  (cons "toUri" (lambda (self) (jfile->uri (nio-path-str self))))
  (cons "register" (lambda (self . args)
                     (throw-jvm 'UnsupportedOperationException "watch service is not supported")))))

;; ---- glob / regex PathMatcher -----------------------------------------------
;; Translate a "glob:" pattern to a Java-style regex string. ** crosses a
;; separator, * and ? stay within a segment, {a,b} alternates, [..] is a char
;; class, \ escapes.
;;
;; WHICH characters separate is a platform parameter, the way jfile-fold-dots-for's
;; root shape is (io.ss) — and for the same reason: the Windows rows are
;; unreachable from the host that runs CI, so they are driven from a table.
;; jolt renders every path with "/" on every platform, but the vendored
;; babashka.fs/match derives win? from os.name rather than File/separator and
;; there rewrites every "/" in the CALLER's pattern to "\\" before handing it
;; over. That arrives here as an escaped backslash — a literal character no
;; "/"-rendered path can hold — so on Windows every pattern that spelled a
;; separator at all matched nothing, while the separator-free spelling of the
;; same query matched: (fs/glob "src" "**/*.clj") answered () where
;; (fs/glob "src" "**.clj") answered 152 files (jolt-lang/jolt#1086). "**/*.ext"
;; being the common spelling, Windows globbing silently saw an empty tree.
;;
;; So on Windows both "/" and "\" separate, which is also how the JDK's Globs
;; reads a Windows pattern. On POSIX "\" stays an ordinary filename character:
;; a file really named a\b is matched by "a\\b" and not by "a/b".
(define (nio-bad-glob msg) (jolt-throw (jolt-ex-info (string-append "invalid glob: " msg) empty-pmap)))
(define (npath-glob->regex pattern) (npath-glob->regex-for (nio-windows?) pattern))
(define (npath-glob->regex-for windows? pattern)
  (let ((n (string-length pattern))
        ;; one segment's worth of any character: everything but a separator
        (not-sep (if windows? "[^/\\\\]" "[^/]"))
        ;; an escaped "\" — the separator babashka.fs wrote on Windows, and a
        ;; literal backslash everywhere else
        (esc-backslash (if windows? "[/\\\\]" "\\\\")))
    (let loop ((i 0) (out "^") (brace #f) (class #f))   ; brace = inside {}, class = inside []
      (if (>= i n)
          (cond (brace (nio-bad-glob "missing '}'"))
                (class (nio-bad-glob "missing ']'"))
                (else (string-append out "$")))
          (let ((c (string-ref pattern i)))
             (cond
              ((and (char=? c #\*) (< (+ i 1) n) (char=? (string-ref pattern (+ i 1)) #\*))
               (loop (+ i 2) (string-append out (if class "\\*\\*" ".*")) brace class))
              ((char=? c #\*) (loop (+ i 1) (string-append out (if class "\\*" (string-append not-sep "*"))) brace class))
              ((char=? c #\?) (loop (+ i 1) (string-append out (if class "\\?" not-sep)) brace class))
              ((and class (char=? c #\!) (char=? (string-ref out (- (string-length out) 1)) #\[))
               (loop (+ i 1) (string-append out "^") brace class))
              ((char=? c #\{) (if brace (nio-bad-glob "nested '{'") (loop (+ i 1) (string-append out "(") #t class)))
              ((char=? c #\}) (loop (+ i 1) (string-append out ")") #f class))
              ((char=? c #\,) (loop (+ i 1) (string-append out (if brace "|" ",")) brace class))
              ((char=? c #\[) (loop (+ i 1) (string-append out "[") brace #t))
              ((char=? c #\]) (loop (+ i 1) (string-append out "]") brace #f))
              ((char=? c #\\)
               (if (< (+ i 1) n)
                   ;; a "\\" pair outside a character class is the one the
                   ;; Windows rewrite writes for a separator; inside a class it
                   ;; is a literal, since [/\] would name two members there
                   (let ((next (string-ref pattern (+ i 1))))
                     (loop (+ i 2)
                           (string-append out (if (and (char=? next #\\) (not class))
                                                  esc-backslash
                                                  (string-append "\\" (string next))))
                           brace class))
                   (nio-bad-glob "no character to escape after '\\'")))
              ;; a "/" in a Windows pattern is a separator, as the JDK's
              ;; Windows glob reads it, and the path it meets renders "\\"
              ((and windows? (char=? c #\/) (not class))
               (loop (+ i 1) (string-append out esc-backslash) brace class))
              ((memv c '(#\. #\( #\) #\^ #\$ #\+ #\|))
               (loop (+ i 1) (string-append out "\\" (string c)) brace class))
              (else (loop (+ i 1) (string-append out (string c)) brace class))))))))

;; getPathMatcher("glob:..."|"regex:...") -> a matcher jhost; .matches(path) is
;; a whole-string match of the compiled pattern against the path's string form.
(define (npath-make-matcher-on windows? syntax-and-pattern)
  (let* ((idx (let loop ((i 0)) (cond ((>= i (string-length syntax-and-pattern)) #f)
                                      ((char=? (string-ref syntax-and-pattern i) #\:) i)
                                      (else (loop (+ i 1))))))
         (syntax (if idx (substring syntax-and-pattern 0 idx) "glob"))
         (pat (if idx (substring syntax-and-pattern (+ idx 1) (string-length syntax-and-pattern)) syntax-and-pattern))
         (rx (jolt-re-pattern (cond ((string=? syntax "glob") (npath-glob->regex-for windows? pat))
                                    ((string=? syntax "regex") pat)
                                    (else (throw-jvm (quote UnsupportedOperationException) (string-append "unrecognized path-matcher syntax: " (jolt-final-str syntax))))))))
    (make-jhost "nio-path-matcher" rx)))
(define (npath-make-matcher syntax-and-pattern)
  (npath-make-matcher-on (nio-windows?) syntax-and-pattern))

(register-host-methods! "nio-path-matcher"
  (list (cons "matches" (lambda (self p)
                          (and (jolt-truthy?
                                (jolt-re-matches (jhost-state self)
                                                 (if (nio-path? p)
                                                     (nio-path-render p)
                                                     (path-native (npath-string-of p)))))
                               #t)))))

(register-host-methods! "nio-filesystem"
  (list (cons "getPathMatcher" (lambda (self spec) (npath-make-matcher (npath-string-of spec))))
        (cons "getPath" (lambda (self first . more) (apply npath-get first more)))
        (cons "getSeparator" (lambda (self) (file-separator)))))

;; ---- construction statics + File bridge -------------------------------------
;; FileSystems.newFileSystem / getFileSystem(URI) go to the provider installed
;; for the URI's scheme, and a scheme nobody serves is ProviderNotFoundException.
(define (nio-uri-provider uri)
  (let* ((scheme (if (uri-jhost? uri) (uri-field uri 'scheme) jolt-nil))
         (provider (and (string? scheme) (hashtable-ref nio-scheme-providers scheme #f))))
    (or provider
        (zip-throw "java.nio.file.ProviderNotFoundException"
                   (string-append "Provider \"" (if (string? scheme) scheme (jolt-str-render-one uri))
                                  "\" not found")))))
(let ((paths-statics (list (cons "get" npath-get)))
      (path-statics  (list (cons "of" npath-get)))
      (fs-statics    (list (cons "getDefault" (lambda () default-nio-filesystem))
                           (cons "newFileSystem"
                                 (lambda (uri . env)
                                   (apply (nio-provider-new-file-system (nio-uri-provider uri)) uri env)))
                           (cons "getFileSystem"
                                 (lambda (uri) ((nio-provider-get-file-system (nio-uri-provider uri)) uri))))))
  (register-class-statics! "Paths" paths-statics)
  (register-class-statics! "java.nio.file.Paths" paths-statics)
  (register-class-statics! "Path" path-statics)
  (register-class-statics! "java.nio.file.Path" path-statics)
  (register-class-statics! "FileSystems" fs-statics)
  (register-class-statics! "java.nio.file.FileSystems" fs-statics))

;; nio-path method dispatch (priority above the jfile arm).
(register-method-arm! arm-priority-nio-path
  (lambda (obj method-name rest-args)
    (if (nio-path? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (nio-path-method obj method-name rest)))
          (if r (car r) (dispatch-miss obj method-name rest)))
        'pass)))

;; (str p), value equality + hashing. instance? and (class p) come from the
;; jhost-tag->fqn rows for these three tags (class-hierarchy.ss) — the same
;; registry that gives every other shim its class, so extend-protocol on
;; java.nio.file.Path dispatches on a Path too. A tag-local arm here answered
;; instance? and class but NOT value-host-tags, which is exactly the drift the
;; registry exists to prevent: (extend-protocol P java.nio.file.Path …) then
;; threw "No method" on a value whose (class …) said java.nio.file.Path.
(register-str-render! nio-path? nio-path-render)
(register-eq-arm! (lambda (a b) (and (nio-path? a) (nio-path? b)))
                  (lambda (a b) (and (eq? (nio-path-filesystem a) (nio-path-filesystem b))
                                     (string=? (nio-path-str a) (nio-path-str b)))))
(register-hash-arm! nio-path? (lambda (p) (equal-hash (jhost-state p))))

;; ---- Files statics ----------------------------------------------------------
;; Each Files op takes Path / File / String args plus trailing varargs (options /
;; attributes) it ignores here; the on-disk path resolves against JOLT_PWD the
;; same way java.io.File does (project-relative), so File and Path see one tree.
(define (nfp x)                                ; on-disk path; "" is the cwd, nil is nothing
  (if (jolt-nil? x) ""
      (let ((s (npath-string-of x))) (project-relative (if (string=? s "") "." s)))))
(define (->path x) (if (nio-path? x) x (make-nio-path (npath-string-of x))))

;; ---- error reporting: java.nio.file's exception family, not java.io's -------
;; Chez's filesystem primitives raise &i/o-filename conditions. Left to escape,
;; they reach jolt through host-faults' generic fallback, which names them with
;; java.io classes -- FileNotFoundException for a missing path -- and renders the
;; Chez primitive's own message. java.nio.file.Files answers a different family,
;; per UnixException.translateToIOException, so every Files entry point that
;; touches the filesystem translates its own failure here.
;;
;; Two facts about the raise shape this:
;;
;;   - Only open-file-input-port / open-file-output-port attach the R6RS
;;     subconditions (&i/o-file-already-exists, &i/o-file-does-not-exist,
;;     &i/o-file-protection). mkdir, rename-file and directory-list raise a bare
;;     &i/o-filename whose only clue to the errno is the strerror text sitting in
;;     the irritants.
;;   - Reading a class off that text would tie it to libc's wording, and to the
;;     locale the process happens to run under.
;;
;; So the entry points needing a class Chez does not type stat first and name the
;; error themselves. That is a race for the error's NAME only, never for
;; correctness: no pre-check here gates a mutation. createFile -- the one place
;; where losing the race would cost data -- takes the O_EXCL open instead, which
;; does raise a typed condition, and stats nothing.
;; A message names the path AS THE CALLER GAVE IT, as the JDK's does: Files ops
;; resolve a relative path against user.dir before touching disk (nfp), and the
;; message used that resolved string, so (Files/delete (Paths/get "nope/x"))
;; reported "/home/me/proj/nope/x" where the JDK says "nope/x". The helpers
;; below take the resolved path to act on and an optional SHOWN one to report,
;; which each Files entry point passes as (nio-shown its-argument).
(define (nio-shown x) (if (jolt-nil? x) "" (npath-string-of x)))
;; FileSystemException's (file, other, reason) are kept apart until the message
;; is built (io.ss fs-exception-message), so only the paths render with "\" on
;; Windows and the reason text is left as the OS wrote it.
(define (nio-fs-throw cls fp . other+reason)
  (let ((other  (and (pair? other+reason) (car other+reason)))
        (reason (and (pair? other+reason) (pair? (cdr other+reason)) (cadr other+reason))))
    (jolt-throw (jolt-host-throwable cls (fs-exception-message fp other reason)))))
(define (nio-no-such-file fp . other)
  (nio-fs-throw "java.nio.file.NoSuchFileException" fp (and (pair? other) (car other))))
(define (nio-already-exists fp . other)
  (nio-fs-throw "java.nio.file.FileAlreadyExistsException" fp (and (pair? other) (car other))))
;; "<path>: <reason>" is the JDK's rendering for every errno without a class.
(define (nio-fs-detail fp reason)
  (nio-fs-throw "java.nio.file.FileSystemException" fp #f reason))

;; The strerror text is the LAST string irritant: an open raises (path reason),
;; rename-file raises (src dst reason). Any other shape degrades to a bare path
;; rather than promoting some other irritant into the reason slot.
(define (nio-fs-error-reason e fp)
  (and (irritants-condition? e)
       (let loop ((xs (condition-irritants e)) (last #f))
         (cond ((null? xs) (and (string? last) (not (string=? last fp)) last))
               ((string? (car xs)) (loop (cdr xs) (car xs)))
               (else (loop (cdr xs) last))))))

;; Run a Chez filesystem primitive, translating whatever it raises.
(define (nio-fs-call fp thunk . shown)
  (let ((m (if (pair? shown) (car shown) fp)))
    (guard (e
            ((i/o-file-already-exists-error? e) (nio-already-exists m))
            ((i/o-file-does-not-exist-error? e) (nio-no-such-file m))
            ((i/o-file-protection-error? e)
             (nio-fs-throw "java.nio.file.AccessDeniedException" m))
            ((i/o-filename-error? e) (nio-fs-detail m (nio-fs-error-reason e fp)))
            (else (raise e)))
      (thunk))))

;; A directory opens for reading on Linux and only fails at the first read, so
;; newInputStream handed back a stream that threw later. The JVM checks at open
;; and reports exactly this message.
(define (nio-open-input-port fp . shown)
  (let ((m (if (pair? shown) (car shown) fp)))
    (when (file-directory? fp) (nio-fs-detail m "Is a directory"))
    (nio-fs-call fp (lambda () (open-file-input-port fp)) m)))

(define (nio-size fp . shown)
  (if (not (or (file-exists? fp) (nio-is-symlink? fp)))
      (nio-no-such-file (if (pair? shown) (car shown) fp))
      ;; A directory opens for reading and fstats fine even though READING one
      ;; fails, and Files.size reports a directory's st_size like any other
      ;; entry -- so open it directly here rather than through
      ;; nio-open-input-port, which is the read path and refuses a directory
      ;; exactly as the JVM's newInputStream does.
      ;;
      ;; Chez's file-length is fstat(2).st_size (S_get_fd_length in new-io.c),
      ;; which is the number the JVM answers, so this needs none of the struct
      ;; stat offsets below -- the fd carries the layout question for us.
      (let ((port (apply nio-fs-call fp (lambda () (open-file-input-port fp)) shown)))
        (let ((n (file-length port))) (close-port port) n))))

(define (nio-read-bv fp . shown)
  (io-note-file-read! fp)          ; a compile-time read belongs in the AOT key (io.ss)
  (let ((port (apply nio-open-input-port fp shown)))
    (let ((bv (get-bytevector-all port)))
      (close-port port)
      (if (eof-object? bv) (make-bytevector 0) bv))))

(define (nio-write-bv! fp bv)
  (let ((port (open-file-output-port fp (file-options no-fail))))
    (put-bytevector port bv) (close-port port)))

;; readAllLines: split content on line terminators, drop a single trailing empty.
(define (nio-read-lines fp . shown)
  (let* ((s (utf8->string (apply nio-read-bv fp shown)))
         (n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((= i n)
         (let ((segs (reverse (if (> i start) (cons (substring s start i) acc) acc))))
           (make-pvec (list->vector
                       (map (lambda (ln)
                              (let ((k (string-length ln)))
                                (if (and (> k 0) (char=? (string-ref ln (- k 1)) #\return))
                                    (substring ln 0 (- k 1)) ln)))
                            segs)))))
        ((char=? (string-ref s i) #\newline)
         (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
        (else (loop (+ i 1) start acc))))))

(define (nio-output-data->bv data)
  (cond
    ((jolt-array? data) (na-bytearray->bv data))
    ((string? data) (string->utf8 data))
    (else                                    ; Iterable<CharSequence>: line + separator each
     (let ((body (fold-left (lambda (acc ln) (string-append acc (jolt-str-render-one ln) "\n"))
                            "" (seq->list (jolt-seq data)))))
       (string->utf8 body)))))

(define nio-ENOTEMPTY (if (eq? (sa-os-family) 'macos) 66 39))
;; A failed unlink/rmdir named the JDK's way (UnixFileSystemProvider.implDelete):
;; EEXIST or ENOTEMPTY on a directory is DirectoryNotEmpty, then EACCES
;; AccessDenied, ENOENT NoSuchFile, and any other errno a FileSystemException
;; carrying strerror's text. Windows sets no errno, so a refused delete there
;; reads as AccessDenied, which is what the JDK reports for its usual causes
;; (read-only attribute, sharing violation on an open file).
(define (nio-delete-failed fp m err dir?)
  (cond ((not (and err (> err 0)))
         (if (and dir? (pair? (guard (e (#t '())) (directory-list fp))))
             (nio-fs-throw "java.nio.file.DirectoryNotEmptyException" m)
             (nio-fs-throw "java.nio.file.AccessDeniedException" m)))
        ((and dir? (or (= err nio-ENOTEMPTY) (= err nio-EEXIST)))
         (nio-fs-throw "java.nio.file.DirectoryNotEmptyException" m))
        ((= err nio-EACCES) (nio-fs-throw "java.nio.file.AccessDeniedException" m))
        ((= err nio-ENOENT) (nio-no-such-file m))
        (else (nio-fs-detail m (and c-strerror (c-strerror err))))))

;; delete-file and delete-directory answer #f on failure rather than raising,
;; so the answer is checked here and the errno read straight after it.
(define (nio-delete1 fp missing-ok? . shown)
  (let ((m (if (pair? shown) (car shown) fp)))
    (define (attempt del dir?)
      (if (del fp) #t
          (let ((err (io-errno)))   ; before anything else can set it
            (nio-delete-failed fp m err dir?))))
    (cond ((nio-is-symlink? fp) (attempt delete-file #f))   ; the link itself, even if dangling
          ((not (file-exists? fp))
           (if missing-ok? #f (nio-no-such-file m)))
          ((file-directory? fp) (attempt delete-directory #t))
          (else (attempt delete-file #f)))))

(define nio-temp-counter 0)
(define nio-temp-mutex (make-mutex))
(define (nio-tmp-dir) (host-temp-dir))
;; A temp path in `dir` (default the system temp dir), unique across processes
;; via now-millis + a retry counter, like java.nio.file's createTemp*.
(define (nio-temp-path dir prefix suffix)
  (let ((d (let ((d (or dir (nio-tmp-dir))))
             (if (char=? (string-ref d (- (string-length d) 1)) #\/) d (string-append d "/")))))
    (let loop ()
      ;; the increment and the read are ONE step. Read after the release and two
      ;; threads see the same value, so they build the same path — and the
      ;; file-exists? retry below does not catch it, since neither has created
      ;; the file yet and the caller that creates it second clobbers the first.
      (let* ((n (jolt-with-mutex nio-temp-mutex
                  (set! nio-temp-counter (+ nio-temp-counter 1))
                  nio-temp-counter))
             (full (string-append d (if (string? prefix) prefix "")
                                  (number->string (now-millis)) "-" (number->string n)
                                  (if (string? suffix) suffix ""))))
        (if (file-exists? (project-relative full)) (loop) full)))))

;; ---- Files/isHidden ---------------------------------------------------------
;; Files.isHidden is implementation-specific, and the JDK answers it per
;; platform: UnixFileSystemProvider.isHidden tests the NAME's first character for
;; ".", while WindowsFileSystemProvider.isHidden reads the file's DOS attribute
;; word and asks for FILE_ATTRIBUTE_HIDDEN, ignoring the name entirely. So on
;; Windows a dot-prefixed file is NOT hidden unless `attrib +h` says so, and a
;; plainly-named one IS when it does.
;;
;; This tested the leading dot on every platform, which inverted the answer in
;; both directions on Windows and took everything built on it along:
;; babashka.fs/hidden? delegates straight to Files/isHidden, and fs/glob skips
;; hidden entries by asking it — so a glob over the same tree selected a
;; different set of files under jolt than under babashka (jolt-lang/jolt#1110).
;;
;; Neither side has a directory exception: a hidden DIRECTORY is hidden on
;; Windows, and a dot-prefixed one is hidden on Unix.
;;
;; The platform is a parameter, like every other Windows-shaped answer in this
;; file, so the rows are pinned from a POSIX runner (test/chez/win-platform-test.ss).
(define (nio-hidden-for? windows? attrs name)
  (if windows?
      (and attrs (not (= 0 (bitwise-and attrs win32-FILE-ATTRIBUTE-HIDDEN))) #t)
      (and (> (string-length name) 0) (char=? (string-ref name 0) #\.))))

(define (nio-hidden? p)
  (let ((windows? (nio-windows?)))
    (nio-hidden-for? windows?
                     (and windows? (win32-file-attributes (nfp p)))
                     (npath-string-of (npath-file-name (npath-string-of p))))))

(let ((files-statics
       (list
        (cons "notExists"     (lambda (p . _) (if (file-exists? (nfp p)) #f #t)))
        ;; the effective user's permission, from the one predicate java.io.File's
        ;; canRead/canWrite/canExecute uses (io.ss) — these are the same question
        ;; and must not drift into two answers for one path.
        (cons "isReadable"    (lambda (p . _) (file-accessible? (nfp p) access-r-ok)))
        (cons "isWritable"    (lambda (p . _) (file-accessible? (nfp p) access-w-ok)))
        (cons "isExecutable"  (lambda (p . _) (file-accessible? (nfp p) access-x-ok)))
        (cons "isHidden"      (lambda (p . _) (nio-hidden? p)))
        (cons "size"          (lambda (p . _) (nio-size (nfp p) (nio-shown p))))
        (cons "delete"        (lambda (p) (nio-delete1 (nfp p) #f (nio-shown p)) jolt-nil))
        (cons "deleteIfExists"(lambda (p) (nio-delete1 (nfp p) #t (nio-shown p))))
        (cons "readAllBytes"  (lambda (p) (na-bv->bytearray (nio-read-bv (nfp p) (nio-shown p)))))
        (cons "readAllLines"  (lambda (p . _) (nio-read-lines (nfp p) (nio-shown p))))
        (cons "newInputStream"(lambda (p . opts)
                                (let ((fp (nfp p)))
                                  ;; a read stream refuses the two write options
                                  (for-each (lambda (o)
                                              (when (memq (oopt-sym o) '(write append))
                                                (throw-jvm 'UnsupportedOperationException
                                                           (string-append "'" (enum-name o) "' not allowed"))))
                                            (npath-spread-args opts))
                                  (io-note-file-read! fp)
                                  (let ((port (nio-open-input-port fp (nio-shown p))))
                                    (nio-delete-on-close! fp port opts)
                                    (stream-classed! (make-in-stream port) "sun.nio.ch.ChannelInputStream")))))
        (cons "createTempFile"      (lambda args (nio-files-create-temp args #f)))
        (cons "createTempDirectory" (lambda args (nio-files-create-temp args #t))))))
  (set! files-accum-chunks (cons files-statics files-accum-chunks)))

;; createTempFile(prefix, suffix, attrs*) | createTempFile(dir, prefix, suffix, attrs*)
;; createTempDirectory(prefix, attrs*)    | createTempDirectory(dir, prefix, attrs*)
(define (nio-files-create-temp args dir?)
  (let* ((args (filter (lambda (x) (not (jolt-array? x))) args))
         (has-dir (and (pair? args) (or (nio-path? (car args)) (jfile? (car args)))))
         (base (if has-dir (npath-string-of (car args)) #f))
         (rest (if has-dir (cdr args) args))
         (prefix (if (and (pair? rest) (string? (car rest))) (car rest) ""))
         (suffix (if (and (not dir?) (pair? rest) (pair? (cdr rest)) (string? (cadr rest))) (cadr rest) ""))
         (full (nio-temp-path base prefix suffix))
         (fp (project-relative full)))
    (if dir? (mkdir fp) (close-port (open-file-output-port fp (file-options no-fail))))
    (when c-chmod (c-chmod fp (if dir? #o700 #o600)))
    (make-nio-path full)))

;; ---- walkFileTree + FileVisitor ---------------------------------------------
;; FileVisitResult values — distinct tokens; babashka.fs maps :continue etc. to
;; these and hands them back to walkFileTree, which reads them for control flow.
(define (make-fvr sym) (make-jhost "fvr" sym))
(define (fvr? x) (and (jhost? x) (string=? (jhost-tag x) "fvr")))
(define fvr-continue      (make-fvr 'continue))
(define fvr-skip-subtree  (make-fvr 'skip-subtree))
(define fvr-skip-siblings (make-fvr 'skip-siblings))
(define fvr-terminate     (make-fvr 'terminate))
(define (fvr-sym r) (if (fvr? r) (jhost-state r) 'continue))
(define fvo-follow-links  (make-jhost "fvo" 'follow-links))

;; BasicFileAttributes of a path, read through (FOLLOW? #t) or not through
;; (NOFOLLOW_LINKS) a symbolic link — every getter answers through
;; nio-attr-value, the one table readAttributes and getAttribute also read, so
;; the three cannot disagree about a link. It used to hold only the path and
;; follow always, so readAttributes(link, BasicFileAttributes, NOFOLLOW_LINKS)
;; described the target and isSymbolicLink was false for everything.
(define (make-basic-attrs fp follow?) (make-jhost "basic-attrs" (cons fp follow?)))
(define (basic-attrs-value self nm)
  (let ((st (jhost-state self))) (nio-attr-value (car st) nm (cdr st))))
(register-host-methods! "basic-attrs"
  (list (cons "isDirectory"      (lambda (self) (basic-attrs-value self "isDirectory")))
        (cons "isRegularFile"    (lambda (self) (basic-attrs-value self "isRegularFile")))
        (cons "isSymbolicLink"   (lambda (self) (basic-attrs-value self "isSymbolicLink")))
        (cons "isOther"          (lambda (self) (basic-attrs-value self "isOther")))
        (cons "size"             (lambda (self) (basic-attrs-value self "size")))
        (cons "lastModifiedTime" (lambda (self) (basic-attrs-value self "lastModifiedTime")))
        (cons "lastAccessTime"   (lambda (self) (basic-attrs-value self "lastAccessTime")))
        (cons "creationTime"     (lambda (self) (basic-attrs-value self "creationTime")))
        (cons "fileKey"          (lambda (self) (basic-attrs-value self "fileKey")))))

(define (nio-call-visitor visitor name . args)
  (let ((m (reify-method-ref visitor name)))
    (if m (fvr-sym (apply jolt-invoke m visitor args)) 'continue)))

;; Files/walkFileTree(start, opts, max-depth, visitor): pre-order directory walk
;; calling the visitor and honoring CONTINUE / SKIP_SUBTREE / SKIP_SIBLINGS /
;; TERMINATE. Returns the start path. (Symlink following via opts is not yet
;; distinct — there are no symlinks to follow on this host layer.)
(define (nio-path-join base name)
  (if (char=? (string-ref base (- (string-length base) 1)) #\/)
      (string-append base name) (string-append base "/" name)))
(define (nio-opts-follow? opts)   ; does the opts set carry FileVisitOption/FOLLOW_LINKS?
  (and opts (not (jolt-nil? opts))
       (guard (e (#t #f))
         (exists (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "fvo")))
                 (seq->list (jolt-seq opts))))))
(define (nio-walk-file-tree start opts max-depth visitor)
  (let ((md (if (number? max-depth) (exact (truncate max-depth)) 2147483647))
        (follow? (nio-opts-follow? opts)))
    (call/cc
     (lambda (stop)
       (define (walk path-obj depth ancestors)
         (let ((fp (nfp path-obj)))
           ;; a symlink is a leaf unless following (never descend its target); a
           ;; directory at max depth is visited as a file, like java.nio.file
           (if (and (file-directory? fp) (< depth md) (or follow? (not (nio-is-symlink? fp))))
               (let ((ino (and follow? (nio-stat-ino fp))))
                 ;; following a link back into an ancestor is a cycle -> visitFileFailed
                 (if (and ino (member ino ancestors))
                     (let ((r (nio-call-visitor visitor "visitFileFailed" path-obj jolt-nil)))
                       (if (eq? r 'terminate) (stop #t) r))
                     (let ((r (nio-call-visitor visitor "preVisitDirectory" path-obj (make-basic-attrs fp follow?))))
                       (cond
                         ((eq? r 'terminate) (stop #t))
                         ((eq? r 'skip-subtree) 'continue)
                         ((eq? r 'skip-siblings) 'skip-siblings)
                         (else
                          (let ((anc (if ino (cons ino ancestors) ancestors)))
                            (when (< depth md)
                              (let loop ((names (sort string<? (directory-list fp))))
                                (unless (null? names)
                                  (let ((cr (walk (make-nio-path (nio-path-join (nio-path-str path-obj) (car names)))
                                                  (+ depth 1) anc)))
                                    (unless (eq? cr 'skip-siblings) (loop (cdr names))))))))
                          (let ((pr (nio-call-visitor visitor "postVisitDirectory" path-obj jolt-nil)))
                            (if (eq? pr 'terminate) (stop #t) 'continue)))))))
               (let ((r (nio-call-visitor visitor "visitFile" path-obj (make-basic-attrs fp follow?))))
                 (if (eq? r 'terminate) (stop #t) r)))))
       (walk (->path start) 0 '())))
    (->path start)))

;; ---- newDirectoryStream: a closeable, seqable listing of a directory's kids --
(define (make-dir-stream paths) (make-jhost "dir-stream" paths))  ; state: list of Path
(define-jhost-tag-pred (dir-stream? t) (string=? t "dir-stream"))
(define (nio-new-directory-stream dir . rest)
  (let* ((base (npath-string-of dir))
         (fp (project-relative base))
         (_ (cond ((not (file-exists? fp)) (nio-no-such-file base))
                  ((not (file-directory? fp))
                   (nio-fs-throw "java.nio.file.NotDirectoryException" base))))
         (names (sort string<? (nio-fs-call fp (lambda () (directory-list fp)) base)))
         (arg (and (pair? rest) (car rest)))
         (paths (map (lambda (nm) (make-nio-path (nio-path-join base nm))) names)))
    (make-dir-stream
     (cond
       ;; a glob string filters by file name
       ((string? arg)
        (let ((rx (jolt-re-pattern (npath-glob->regex arg))))
          (filter (lambda (p) (jolt-truthy? (jolt-re-matches rx (npath-string-of (npath-file-name (nio-path-str p)))))) paths)))
       ;; a DirectoryStream$Filter reify filters by its accept method
       ((and arg (reify-method-ref arg "accept"))
        => (lambda (m) (filter (lambda (p) (jolt-truthy? (jolt-invoke m arg p))) paths)))
       (else paths)))))

;; dir-stream is seqable (its child Paths) and closeable (a no-op) so
;; (with-open [s (newDirectoryStream d)] (mapv f s)) works.
;; seq arms, not a set!-wrap of jolt-seq: a wrapper put two record tests in
;; front of EVERY seq in the program (a vector's included); an arm is consulted
;; only after jolt-seq's own types have missed.
(register-seq-arm! dir-stream? (lambda (x) (list->cseq (jhost-state x))))
(register-seq-arm! nio-path?
  (lambda (x)
    (let ((segs (npath-segs-for (nio-path-windows? x) (nio-path-str x))))
      ;; the empty path iterates as one empty component (java parity)
      (list->cseq (map (lambda (segment) (npath-like x segment))
                       (if (null? segs) '("") segs))))))
(let ((prev jolt-close))
  (set! jolt-close (lambda (x) (if (dir-stream? x) jolt-nil (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; register the Files walk/stream ops + the FileVisitResult / FileVisitOption enums.
(let ((files-walk (list (cons "walkFileTree" nio-walk-file-tree)
                        (cons "newDirectoryStream" nio-new-directory-stream))))
  (set! files-accum-chunks (cons files-walk files-accum-chunks)))
(register-enum-class! "java.nio.file.FileVisitResult"
  (list (cons "CONTINUE" fvr-continue) (cons "TERMINATE" fvr-terminate)
        (cons "SKIP_SUBTREE" fvr-skip-subtree) (cons "SKIP_SIBLINGS" fvr-skip-siblings)))
(register-enum-class! "java.nio.file.FileVisitOption" (list (cons "FOLLOW_LINKS" fvo-follow-links)))

;; ---- FileTime + attributes + POSIX permissions + symlinks -------------------
;; A FileTime is the JDK's (value, unit) pair: state (VALUE . SCALE), SCALE the
;; unit's size in nanoseconds, or SCALE 0 for one made from an Instant, whose
;; VALUE is then its epoch nanoseconds. It was epoch milliseconds, so a file's
;; time lost its sub-millisecond digits on the way in: str showed ".123Z" where
;; the JDK shows ".123456789Z", and two times a microsecond apart were equal.
;; The pair is kept rather than a nanosecond count because the JDK converts from
;; it: to(unit)/toMillis truncate toward zero from the unit held, but floor from
;; an Instant. A bare integer state is a millisecond FileTime from before this.
(define (make-file-time-in value scale) (make-jhost "file-time" (cons value scale)))
(define (make-file-time ms) (make-file-time-in ms 1000000))
(define (make-file-time-ns ns) (make-file-time-in ns 1))
(define-jhost-tag-pred (file-time? t) (string=? t "file-time"))
(define (file-time-parts x)
  (let ((st (jhost-state x))) (if (pair? st) st (cons st 1000000))))
;; epoch nanoseconds, exact
(define (file-time-ns x)
  (if (file-time? x)
      (let ((p (file-time-parts x)))
        (if (eqv? (cdr p) 0) (car p) (* (car p) (cdr p))))
      0))
(define ft-long-min (- (expt 2 63)))
(define ft-long-max (- (expt 2 63) 1))
(define (ft-saturate n) (max ft-long-min (min ft-long-max n)))
;; FileTime.to(unit), USCALE the unit's nanoseconds: TimeUnit.convert from the
;; unit held (truncating, saturating at Long's range), or seconds and nanos
;; converted apart for an Instant-made one.
(define (file-time-to x uscale)
  (let* ((p (file-time-parts x)) (v (car p)) (sc (cdr p)))
    (if (eqv? sc 0)
        (let* ((secs (floor (/ v 1000000000))) (nanos (- v (* secs 1000000000)))
               (s (ft-saturate (quotient (* secs 1000000000) uscale))))
          (if (or (= s ft-long-min) (= s ft-long-max))
              s
              (ft-saturate (+ s (quotient nanos uscale)))))
        (ft-saturate (quotient (* v sc) uscale)))))
(define (file-time-ms x) (if (file-time? x) (file-time-to x 1000000) 0))
(define (file-time-compare a b)
  (let ((x (file-time-ns a)) (y (file-time-ns b)))
    (cond ((< x y) -1) ((> x y) 1) (else 0))))
;; hashCode is toInstant().hashCode(), which equals needs: (int)(s ^ s>>>32) +
;; 51 * nanos over the instant's seconds and nanos, in int arithmetic.
(define (file-time-hash x)
  (let* ((ns (file-time-ns x))
         (secs (floor (/ ns 1000000000)))
         (nanos (- ns (* secs 1000000000)))
         (s64 (bitwise-and secs #xFFFFFFFFFFFFFFFF))
         (h (bitwise-and (+ (bitwise-and (bitwise-xor s64 (bitwise-arithmetic-shift-right s64 32))
                                         #xFFFFFFFF)
                            (* 51 nanos))
                         #xFFFFFFFF)))
    (if (>= h #x80000000) (- h #x100000000) h)))
;; A TimeUnit argument's scale; tu->ms's rule for anything else (milliseconds).
(define (file-time-unit-scale u) (if (time-unit? u) (time-unit-scale u) 1000000))
(register-host-methods! "file-time"
  (list (cons "toMillis"   (lambda (self) (file-time-ms self)))
        (cons "to"         (lambda (self unit) (file-time-to self (file-time-unit-scale unit))))
        (cons "toInstant"  (lambda (self)
                             (unless jt-instant-hook (load-namespace "jolt.time.base"))
                             (jt-instant-hook (file-time-ns self))))
        (cons "compareTo"  (lambda (self o) (file-time-compare self o)))
        (cons "equals"     (lambda (self o) (and (file-time? o) (= 0 (file-time-compare self o)))))
        (cons "hashCode"   (lambda (self) (file-time-hash self)))
        (cons "toString"   (lambda (self) (file-time->string (file-time-ns self))))))
;; FileTime.toString: ISO-8601 in UTC, the fraction (to the nanosecond) only when
;; there is one and with its trailing zeros dropped, no "+" on a five-digit year,
;; and a year at or before 0 written as "-" and 1 - year ("-0001" is year 0).
;; That is FileTime's own rendering, not Instant's.
(define (file-time->string ns)
  (let* ((secs (floor (/ ns 1000000000)))
         (frac (- ns (* secs 1000000000)))
         (f (inst-fields (* secs 1000))) (y (list-ref f 0)))
    (string-append (if (> y 0) (pad4 y) (string-append "-" (pad4 (- 1 y))))
                   "-" (pad2 (list-ref f 1)) "-" (pad2 (list-ref f 2))
                   "T" (pad2 (list-ref f 3)) ":" (pad2 (list-ref f 4)) ":" (pad2 (list-ref f 5))
                   (if (= frac 0)
                       ""
                       (let loop ((s (let ((d (number->string frac)))
                                       (string-append (make-string (- 9 (string-length d)) #\0) d))))
                         (if (char=? (string-ref s (- (string-length s) 1)) #\0)
                             (loop (substring s 0 (- (string-length s) 1)))
                             (string-append "." s))))
                   "Z")))
(register-str-render! file-time? (lambda (t) (file-time->string (file-time-ns t))))
(register-eq-arm! (lambda (a b) (and (file-time? a) (file-time? b)))
                  (lambda (a b) (= 0 (file-time-compare a b))))
(register-hash-arm! file-time? file-time-hash)
;; FileTime is Comparable, so compare / sort take two of them.
(register-compare-arm! (lambda (a b) (and (file-time? a) (file-time? b))) file-time-compare)
;; An Instant's epoch nanoseconds: the jolt.time base's own count when it is one,
;; else clojure.core/inst-ms's milliseconds, both looked up when called — a bare
;; Scheme inst-ms here named nothing, and every (fs/set-last-modified-time p
;; instant) raised "variable inst-ms is not bound" (jolt-lang/jolt#1119).
(define (instant-epoch-nanos x)
  (or (guard (e (#t #f))
        (and (jolt-truthy? (jolt-invoke (var-deref "jolt.time.instant" "inst?") x))
             (jnum->exact (jolt-invoke (var-deref "jolt.time.instant" "inst-nanos") x))))
      (* (jnum->exact (jolt-invoke (var-deref "clojure.core" "inst-ms") x)) 1000000)))
(let ((ft-statics (list (cons "fromMillis" (lambda (ms) (make-file-time (jnum->exact ms))))
                        ;; from(long, TimeUnit) keeps the pair; from(Instant) the
                        ;; instant's nanoseconds
                        (cons "from" (lambda (x . unit)
                                       (if (pair? unit)
                                           (make-file-time-in (jnum->exact x) (file-time-unit-scale (car unit)))
                                           (make-file-time-in (instant-epoch-nanos x) 0)))))))
  (register-class-statics! "FileTime" ft-statics)
  (register-class-statics! "java.nio.file.attribute.FileTime" ft-statics))

;; Files/getAttribute / setAttribute / readAttributes over the "basic:" view.
(define (nio-attr-name a)                      ; strip a "view:" prefix
  (let ((i (let loop ((j 0)) (cond ((>= j (string-length a)) #f)
                                   ((char=? (string-ref a j) #\:) j) (else (loop (+ j 1)))))))
    (if i (substring a (+ i 1) (string-length a)) a)))
;; The basic attribute NM of FP, read through a symbolic link (FOLLOW?) or of
;; the link itself. A link read as itself is a link: not a directory, not a
;; regular file, sized by its target string, with its own times and file key.
(define (nio-attr-value fp nm follow?)
  (let ((link? (and (not follow?) (nio-is-symlink? fp))))
    (cond
      ((member nm '("lastModifiedTime" "lastAccessTime" "creationTime"))
       (nio-time-attr fp nm (not link?)))
      ((string=? nm "size")
       (if link?
           (bytevector-length (string->utf8 (or (nio-readlink fp) "")))
           (nio-size fp)))
      ((string=? nm "isDirectory")      (and (not link?) (file-directory? fp) #t))
      ((string=? nm "isRegularFile")    (and (not link?) (file-regular? fp) #t))
      ((string=? nm "isSymbolicLink")   link?)
      ((string=? nm "isOther")          (and (not link?) (file-exists? fp)
                                             (not (file-directory? fp)) (not (file-regular? fp)) #t))
      ((string=? nm "fileKey")          (nio-file-key fp (not link?)))
      (else jolt-nil))))
;; readAttributes/getAttribute on a path that is not there raise, as the JDK's
;; do; a dangling link is there when read as itself.
(define (nio-attrs-require-exists! fp follow? shown)
  (unless (or (file-exists? fp) (and (not follow?) (nio-is-symlink? fp)))
    (nio-no-such-file shown)))
(define nio-basic-attr-names
  '("lastModifiedTime" "lastAccessTime" "creationTime" "size"
    "isDirectory" "isRegularFile" "isSymbolicLink" "isOther" "fileKey"))
(define (nio-split-commas s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length s))
           (reverse (if (> i start) (cons (substring s start i) acc) acc)))
          ((char=? (string-ref s i) #\,)
           (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring s start i) acc) acc)))
          (else (loop (+ i 1) start acc)))))
(define (nio-str-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))
;; LinkOption/NOFOLLOW_LINKS among OPTS describes a symbolic link as itself, in
;; both forms, as getAttribute already did.
(define (nio-read-attributes path what . opts)
  (let* ((w (npath-string-of what))
         (fp (nfp path))
         (follow? (not (nio-opts-nofollow? opts))))
    (nio-attrs-require-exists! fp follow? (nio-shown path))
    (if (nio-str-suffix? w "Attributes")   ; the Class form -> a BasicFileAttributes value
        (make-basic-attrs fp follow?)
        ;; the string form ("view:a,b" / "*" / "a") -> a map of just those attributes
        (let* ((attr-part (nio-attr-name w))
               (names (if (string=? attr-part "*") nio-basic-attr-names (nio-split-commas attr-part))))
          (fold-left (lambda (m nm) (jolt-assoc m nm (nio-attr-value fp nm follow?))) empty-pmap names)))))

;; PosixFilePermissions <-> "rwxr-xr-x" strings, and chmod-based set.
(define posix-order '("OWNER_READ" "OWNER_WRITE" "OWNER_EXECUTE"
                      "GROUP_READ" "GROUP_WRITE" "GROUP_EXECUTE"
                      "OTHERS_READ" "OTHERS_WRITE" "OTHERS_EXECUTE"))
(define posix-bits '(#o400 #o200 #o100 #o40 #o20 #o10 #o4 #o2 #o1))
(define pfp-constants (map (lambda (nm) (cons nm (make-jhost "posix-perm" nm))) posix-order))
(define (make-pfp name) (cdr (assoc name pfp-constants)))
(define-jhost-tag-pred (pfp? t) (string=? t "posix-perm"))
;; A permission set is a mutable java.util.Set (like the JVM): callers do
;; (.add perms OWNER_WRITE) before setPosixFilePermissions.
(define (make-perm-set elems)
  ((hashtable-ref class-ctors-tbl "HashSet" #f) (list->cseq elems)))
(define (posix-set->mode s)                    ; a jolt set of PosixFilePermission -> mode int
  (fold-left (lambda (acc p)
               (let ((nm (if (pfp? p) (jhost-state p) (npath-string-of p))))
                 (let loop ((os posix-order) (bs posix-bits))
                   (cond ((null? os) acc)
                         ((string=? (car os) nm) (+ acc (car bs)))
                         (else (loop (cdr os) (cdr bs)))))))
             0 (seq->list (jolt-seq s))))
(define (posix-set->str s)
  (let ((mode (posix-set->mode s)))
    (list->string
     (let loop ((bs posix-bits) (ch '(#\r #\w #\x #\r #\w #\x #\r #\w #\x)) (acc '()))
       (if (null? bs) (reverse acc)
           (loop (cdr bs) (cdr ch) (cons (if (> (bitwise-and mode (car bs)) 0) (car ch) #\-) acc)))))))
(define (posix-str->set str)                   ; "rwxr-xr-x" -> jolt set of PosixFilePermission
  (let loop ((i 0) (os posix-order) (acc '()))
    (if (or (>= i (string-length str)) (null? os)) (make-perm-set (reverse acc))
        (loop (+ i 1) (cdr os)
              (if (memv (string-ref str i) '(#\r #\w #\x #\s #\t)) (cons (make-pfp (car os)) acc) acc)))))
(let ((pfp-statics (list (cons "toString" (lambda (s) (posix-set->str s)))
                         (cons "fromString" (lambda (s) (posix-str->set (npath-string-of s)))))))
  (register-class-statics! "PosixFilePermissions" pfp-statics)
  (register-class-statics! "java.nio.file.attribute.PosixFilePermissions" pfp-statics))

;; symlinks + hard links + chmod, via libc (jolt-foreign-proc-safe resolves the
;; already-loaded process symbol; a literal foreign-procedure would be a fasl
;; relocation that aborts the boot where the symbol is absent).
(define c-symlink  (jolt-foreign-proc-safe "symlink"  '(string string) 'int))
(define c-link     (jolt-foreign-proc-safe "link"     '(string string) 'int))
(define c-readlink (jolt-foreign-proc-safe "readlink" '(string u8* unsigned-long) 'long))
(define c-chmod    (jolt-foreign-proc-safe "chmod"    '(string int) 'int))
;; The JDK's Windows filesystem provider has no PosixFileAttributeView, so
;; getPosixFilePermissions / setPosixFilePermissions raise
;; UnsupportedOperationException there, and so does a create given a
;; "posix:permissions" attribute. The shim answered #o755 for the get and did
;; nothing for the set — chmod is not bound on Windows — so a caller asking
;; whether a file was read-only was told it was writable.
(define (nio-posix-view! . msg)
  (when (win32?)
    (jolt-throw (jolt-host-throwable "java.lang.UnsupportedOperationException"
                                     (if (pair? msg) (car msg) jolt-nil)))))
(define (nio-is-symlink? fp)
  (and c-readlink (> (c-readlink fp (make-bytevector 1 0) 1) 0)))   ; readlink succeeds only on a link
(define (nio-readlink fp)
  (and c-readlink
       (let* ((buf (make-bytevector 4096 0)) (n (c-readlink fp buf 4096)))
         (and (> n 0)
              (let ((bv (make-bytevector n)))
                (do ((i 0 (+ i 1))) ((= i n) (utf8->string bv))
                  (bytevector-u8-set! bv i (bytevector-u8-ref buf i))))))))
;; A link that was not made raises, named the JDK's way (UnixException
;; .translateToIOException): EEXIST is FileAlreadyExists, ENOENT NoSuchFile,
;; EACCES AccessDenied, and any other errno a FileSystemException whose reason is
;; strerror's text — "l -> e: Operation not permitted" for a hard link to a
;; directory. A hard link's message names both paths, "link -> existing". ERR is
;; the errno read straight after the failed call; without one (Windows, whose
;; CreateHardLinkW sets no errno) the class is named by looking after the
;; failure, and the reason is unknown.
(define nio-EEXIST 17)
(define nio-ENOENT 2)
(define nio-EACCES 13)
(define c-strerror (jolt-foreign-proc-safe "strerror" '(int) 'string))
(define (nio-link-failed link existing shown-link shown-existing err)
  (let ((other (and existing shown-existing)))
    (cond ((and err (> err 0))
           (cond ((= err nio-EEXIST) (nio-already-exists shown-link other))
                 ((= err nio-ENOENT) (nio-no-such-file shown-link other))
                 ((= err nio-EACCES)
                  (nio-fs-throw "java.nio.file.AccessDeniedException" shown-link other))
                 (else
                  (let ((reason (and c-strerror (c-strerror err))))
                    (nio-fs-throw "java.nio.file.FileSystemException" shown-link other
                                  (if (and reason (= err io-ELOOP))
                                      (string-append reason " or unable to access attributes of symbolic link")
                                      reason))))))
          ((or (file-exists? link) (nio-is-symlink? link)) (nio-already-exists shown-link other))
          ((or (and existing (not (file-exists? existing)))
               (let ((parent (npath-parent-for (nio-windows?) link)))
                 (and (string? parent) (not (file-directory? parent)))))
           (nio-no-such-file shown-link other))
          (else (nio-fs-throw "java.nio.file.FileSystemException" shown-link other)))))
(define fvo-nofollow (make-jhost "link-option" 'nofollow-links))
(let ((files-attr
       (list (cons "readAttributes" nio-read-attributes)
             (cons "isSymbolicLink" (lambda (p . _) (if (nio-is-symlink? (nfp p)) #t #f)))
             (cons "createSymbolicLink" (lambda (link target . _)
                                          (if c-symlink
                                              (unless (= 0 (c-symlink (npath-string-of target) (nfp link)))
                                                (let ((err (io-errno)))   ; before anything else can set it
                                                  (nio-link-failed (nfp link) #f (nio-shown link) #f err)))
                                              (jolt-throw (jolt-ex-info "symlink unavailable" empty-pmap)))
                                          (->path link)))
             (cons "createLink" (lambda (link existing . _)
                                  (let* ((l (nfp link)) (e (nfp existing))
                                         (err (cond ((nio-windows?) (and (win32-create-hard-link! l e) 0))
                                                    ((not c-link) #f)
                                                    ((= 0 (c-link e l)) 0)
                                                    (else (io-errno)))))   ; before anything else can set it
                                    (unless (eqv? err 0)
                                      (nio-link-failed l e (nio-shown link) (nio-shown existing) err)))
                                  (->path link)))
             (cons "readSymbolicLink" (lambda (p) (let* ((fp (nfp p)) (t (nio-readlink fp)))
                                                    (cond (t (make-nio-path t))
                                                          ((not (file-exists? fp)) (nio-no-such-file (nio-shown p)))
                                                          (else (nio-fs-throw "java.nio.file.NotLinkException" (nio-shown p)))))))
             (cons "setPosixFilePermissions" (lambda (p perms . _)
                                               (nio-posix-view!)
                                               (when c-chmod (c-chmod (nfp p) (posix-set->mode perms))) (->path p))))))
  (set! files-accum-chunks (cons files-attr files-accum-chunks)))
(register-enum-class! "java.nio.file.LinkOption" (list (cons "NOFOLLOW_LINKS" fvo-nofollow)))

;; ---- option enums (CopyOption / OpenOption / PosixFilePermission) -----------
;; The constants carry a symbol the Files ops inspect; an op receives them as a
;; trailing CopyOption[] / OpenOption[] (from into-array), which is spread here.
(define (make-copt sym) (make-jhost "copy-option" sym))
(define (copt-sym x) (and (jhost? x) (string=? (jhost-tag x) "copy-option") (jhost-state x)))
(define (make-oopt sym) (make-jhost "open-option" sym))
(define (oopt-sym x) (and (jhost? x) (string=? (jhost-tag x) "open-option") (jhost-state x)))
(define (nio-opts-have? args sym-of want)
  (exists (lambda (x) (eq? (sym-of x) want)) (npath-spread-args args)))

(register-enum-class! "java.nio.file.StandardCopyOption"
  (list (cons "REPLACE_EXISTING" (make-copt 'replace-existing))
        (cons "COPY_ATTRIBUTES"  (make-copt 'copy-attributes))
        (cons "ATOMIC_MOVE"      (make-copt 'atomic-move))))
(register-enum-class! "java.nio.file.StandardOpenOption"
  (list (cons "READ"              (make-oopt 'read))
        (cons "WRITE"             (make-oopt 'write))
        (cons "APPEND"            (make-oopt 'append))
        (cons "TRUNCATE_EXISTING" (make-oopt 'truncate-existing))
        (cons "CREATE"            (make-oopt 'create))
        (cons "CREATE_NEW"        (make-oopt 'create-new))
        (cons "DELETE_ON_CLOSE"   (make-oopt 'delete-on-close))
        (cons "SPARSE"            (make-oopt 'sparse))
        (cons "SYNC"              (make-oopt 'sync))
        (cons "DSYNC"             (make-oopt 'dsync))))
(register-enum-class! "java.nio.file.attribute.PosixFilePermission" pfp-constants)

;; copy / move honor REPLACE_EXISTING; write honors APPEND. newOutputStream's
;; options need a fuller mapping because CREATE_NEW is an atomic filesystem
;; operation, not a pre-open existence check: the port returned by
;; open-file-output-port with no `no-fail` option owns the O_EXCL-style create
;; handle itself.
(define (nio-output-file-options args)
  (let* ((opts (npath-spread-args args))
         (syms (map oopt-sym opts)))
    (for-each
     (lambda (sym)
       (cond
         ((eq? sym 'read)
          (throw-jvm (quote IllegalArgumentException) "READ not allowed"))
         ((not (memq sym '(write append create create-new truncate-existing
                           delete-on-close sparse sync dsync)))
          (throw-jvm (quote UnsupportedOperationException)
                     "unsupported output option"))))
     syms)
    ;; SPARSE is a hint the JDK may ignore, and does on most filesystems; the
    ;; three others act on the port (nio-open-output-port). Any option at all,
    ;; one of these four included, replaces the CREATE + TRUNCATE_EXISTING +
    ;; WRITE default.
    (let* ((defaults? (null? syms))
           (syms (filter (lambda (s) (not (memq s '(delete-on-close sparse sync dsync)))) syms))
           (append? (memq 'append syms))
           (truncate? (or defaults? (memq 'truncate-existing syms)))
           (create-new? (memq 'create-new syms))
           (create? (or defaults? create-new? (memq 'create syms))))
      (when (and append? truncate?)
        (throw-jvm (quote IllegalArgumentException)
                   "APPEND + TRUNCATE_EXISTING not allowed"))
      (cond
        ;; No `no-fail`: Chez creates the entry and atomically fails if any
        ;; entry (including a symlink) already occupies the path.
        (create-new? (if append?
                         (file-options no-truncate append)
                         (file-options)))
        (append? (if create?
                     (file-options no-fail no-truncate append)
                     (file-options no-create no-fail no-truncate append)))
        (create? (if truncate?
                     (file-options no-fail)
                     (file-options no-fail no-truncate)))
        (truncate? (file-options no-create no-fail))
        (else (file-options no-create no-fail no-truncate))))))

;; SYNC / DSYNC: every write reaches the device before it returns, as the JDK
;; opens with O_SYNC / O_DSYNC — here an unbuffered port whose writes fsync.
(define c-fsync (jolt-foreign-proc-safe "fsync" '(int) 'int))
(define (nio-sync-port port fp opt)
  (let ((fd (and c-fsync (guard (_ (#t #f)) (port-file-descriptor port)))))
    (unless fd
      (close-port port)
      (throw-jvm 'UnsupportedOperationException (string-append opt " not supported")))
    (parameterize ((custom-port-buffer-size 1))
      (make-custom-binary-output-port fp
        (lambda (bv start n)
          (put-bytevector port bv start n)
          (flush-output-port port)
          (when (fx<? (c-fsync fd) 0)
            (throw-jvm 'IOException (string-append fp ": fsync failed")))
          n)
        #f #f
        (lambda () (close-port port))))))
;; DELETE_ON_CLOSE: unlinked as soon as it is open, which is what the JDK does on
;; a POSIX filesystem; the open port keeps the data until it is closed. Windows
;; cannot unlink an open file.
(define (nio-delete-on-close! fp port opts)
  (when (nio-opts-have? opts oopt-sym 'delete-on-close)
    (when (nio-windows?)
      (close-port port)
      (throw-jvm 'UnsupportedOperationException "DELETE_ON_CLOSE not supported"))
    (guard (_ (#t #f)) (delete-file fp))))
(define (nio-open-output-port fp options opts . shown)
  (let ((port (apply nio-fs-call fp (lambda () (open-file-output-port fp options)) shown)))
    (nio-delete-on-close! fp port opts)
    (cond ((nio-opts-have? opts oopt-sym 'sync) (nio-sync-port port fp "SYNC"))
          ((nio-opts-have? opts oopt-sym 'dsync) (nio-sync-port port fp "DSYNC"))
          (else port))))

(let ((files-opt
       (list (cons "write" (lambda (p data . opts)
                             (let* ((fp (nfp p))
                                    ;; not named `file-options`: that is the Chez
                                    ;; macro this scope still needs to mean itself
                                    (fopts (nio-output-file-options opts))
                                    (bytes (nio-output-data->bv data))
                                    (port (nio-open-output-port fp fopts opts (nio-shown p))))
                               (put-bytevector port bytes)
                               (close-port port)
                               (->path p))))
             (cons "newOutputStream" (lambda (p . opts)
                                      (stream-classed!
                                       (make-out-stream
                                        (nio-open-output-port
                                         (nfp p) (nio-output-file-options opts) opts (nio-shown p)))
                                       "sun.nio.ch.ChannelOutputStream"))))))
  (set! files-accum-chunks (cons files-opt files-accum-chunks)))

;; ---- stat-backed perms + real path (increment: what the fs suite exercises) --
;; st_mode lives at a platform-specific offset in struct stat; read only that.
(define nio-macos? (eq? (sa-os-family) 'macos))
;; struct stat field offsets are platform ABIs, not portable. Only two fields
;; move: st_mode and st_uid. st_ino@8 is identical everywhere we run, and
;; st_mtim@88 is identical on both Linux ABIs, so those readers stay unguarded.
;;
;; #(name st_mode-offset st_mode-width st_uid-offset
;;   st_atim-offset st_mtim-offset st_birthtim-offset|#f st_dev-width):
;;   darwin        mode@4  (16-bit) uid@16   -- all arches
;;   linux-x86-64  mode@24 (32-bit) uid@28   -- glibc, offsetof-measured
;;   linux-arm64   mode@16 (32-bit) uid@24   -- glibc, offsetof-measured under
;;                                              qemu-aarch64 against the arm64
;;                                              cross headers. st_mode precedes
;;                                              st_nlink here instead of
;;                                              following it, which is the only
;;                                              difference from the row above --
;;                                              and it is why aarch64 Linux is a
;;                                              row rather than sharing one.
;;
;; sizeof(struct stat) is 144 on x86-64 and 128 on aarch64. st_ino@8 and
;; st_mtim@88 were measured identical on both, which is what lets those two
;; readers stay unguarded.
;;
;; The times are timespecs (a 64-bit second, then a 64-bit nanosecond): darwin
;; has atime@32 mtime@48 birthtime@80 with a 32-bit st_dev, and both Linux ABIs
;; atime@72 mtime@88 with a 64-bit st_dev and no birth time in struct stat —
;; that comes from statx(2), which is what the JDK asks too. offsetof-measured:
;; darwin with cc for arm64 and x86_64, Linux with gcc 13 under podman for amd64
;; and arm64.
(define nio-stat-layouts
  (list (vector 'darwin        4 2 16 32 48 80 4)
        (vector 'linux-x86-64 24 4 28 72 88 #f 8)
        (vector 'linux-arm64  16 4 24 72 88 #f 8)))
(define (nio-layout-name l) (vector-ref l 0))
(define (nio-layout-atime-off l) (vector-ref l 4))
(define (nio-layout-mtime-off l) (vector-ref l 5))
(define (nio-layout-birth-off l) (vector-ref l 6))
(define (nio-layout-dev-ref l buf)
  (if (= 4 (vector-ref l 7))
      (bytevector-s32-ref buf 0 (native-endianness))
      (bytevector-u64-ref buf 0 (native-endianness))))
(define (nio-layout-mode-ref l buf)
  (if (= 2 (vector-ref l 2))
      (bytevector-u16-ref buf (vector-ref l 1) (native-endianness))
      (bytevector-u32-ref buf (vector-ref l 1) (native-endianness))))
(define (nio-layout-uid-ref l buf)
  (bytevector-u32-ref buf (vector-ref l 3) (native-endianness)))
(define (nio-layout-named n)
  (let loop ((ls nio-stat-layouts))
    (cond ((null? ls) #f)
          ((eq? (nio-layout-name (car ls)) n) (car ls))
          (else (loop (cdr ls))))))

;; The row the host's IDENTITY proposes. A proposal only -- it is what gets
;; measured below, never what gets trusted.
(define (nio-proposed-stat-layout)
  (case (sa-os-family)
    ((macos) (nio-layout-named 'darwin))
    ((linux) (case (sa-arch)
               ((x86-64) (nio-layout-named 'linux-x86-64))
               ((arm64)  (nio-layout-named 'linux-arm64))
               (else #f)))
    (else #f)))

;; MEASURE the layout instead of deducing it from the host's name, because the
;; name is not always available to be read: a portable-bytecode build's machine
;; tag carries no OS and no arch (jolt-lang/jolt#796, #798), so identity alone
;; sent every such build down the "unverified" branch even on a host whose
;; layout is one of the three above. A stat of a directory we know settles it
;; directly: S_IFDIR has to appear in the format bits of whichever field really
;; is st_mode, and it appears in no other field of any of these layouts (at the
;; competing offsets a real host has st_dev's high half, st_nlink, or st_uid,
;; none of which reach 0x4000 for a root directory). Measured on both Linux
;; ABIs -- natively on x86-64, and under qemu-aarch64 on a statically linked
;; aarch64 build -- where stat("/") is mode 040755 and exactly one row matches:
;;
;;   x86-64   darwin@4 -> 0x0  linux-x86-64@24 -> 0x41ed  linux-arm64@16 -> 0x15
;;   aarch64  darwin@4 -> 0x0  linux-x86-64@24 -> 0x0     linux-arm64@16 -> 0x41ed
;;
;; The rows that do not match land on st_nlink (0x15) and st_uid (0), which is
;; the discrimination working rather than a coincidence to rely on.
;;
;; Identity gets the first word and measurement gets the last. The proposal is
;; preferred whenever the measurement agrees with it, so a host we already know
;; reads exactly as it did before; a proposal the measurement CONTRADICTS is
;; discarded rather than used, because a contradicted proposal is a proposal
;; that would read garbage -- which is the whole failure this guard exists to
;; prevent, and throwing is what it did about it before. That is also the
;; property that makes a NEW row cheap to add: get the offsets wrong and nothing
;; verifies, so the host refuses exactly as it refuses today rather than quietly
;; answering nonsense. A row still has to be measured before it is added -- this
;; is a backstop, not a licence to guess.
;;
;; Only when nothing can be measured (no stat entry, no root directory) does
;; identity stand alone, which is the pre-#798 behaviour. Only macos/linux are
;; measured at all: a Windows _stat is a different struct that none of these rows
;; describes, so it stays unknown rather than risking a chance match.
(define (nio-layout-verifies? l buf)
  ;; bitwise-and, not fxand: a 32-bit st_mode read is not a fixnum on a 32-bit host.
  (= #x4000 (bitwise-and (nio-layout-mode-ref l buf) #xF000)))   ; S_IFDIR
(define (nio-probe-root-stat)
  (and c-stat
       (memq (sa-os-family) '(macos linux))
       (let ((buf (make-bytevector 256 0)))
         (and (= 0 (c-stat "/" buf)) buf))))
(define (nio-sole-verifying-layout buf)
  (let loop ((ls nio-stat-layouts) (hits '()))
    (cond ((null? ls) (and (pair? hits) (null? (cdr hits)) (car hits)))
          ((nio-layout-verifies? (car ls) buf) (loop (cdr ls) (cons (car ls) hits)))
          (else (loop (cdr ls) hits)))))
(define (nio-resolve-stat-layout)
  (let ((proposed (nio-proposed-stat-layout))
        (buf (nio-probe-root-stat)))
    (cond ((not buf) proposed)                                   ; nothing to measure with
          ((and proposed (nio-layout-verifies? proposed buf)) proposed)
          (else (nio-sole-verifying-layout buf)))))
(define nio-stat-layout-cache 'unresolved)
(define (nio-stat-layout)
  (when (eq? nio-stat-layout-cache 'unresolved)
    (set! nio-stat-layout-cache (nio-resolve-stat-layout)))
  nio-stat-layout-cache)
(define (nio-stat-layout-guard! who)
  (unless (nio-stat-layout)
    (jolt-throw (jolt-host-throwable "java.lang.UnsupportedOperationException"
      (string-append who " is not supported on this host: unverified struct stat layout for "
                     (sa-host-tag))))))
(define c-stat (jolt-foreign-proc-safe "stat" '(string u8*) 'int))
(define (nio-stat-mode fp)
  (and c-stat
       (begin
         (nio-stat-layout-guard! "getPosixFilePermissions")
         (let ((lay (nio-stat-layout)) (buf (make-bytevector 256 0)))
           (and (= 0 (c-stat fp buf)) (nio-layout-mode-ref lay buf))))))
;; resolve symlinks; #f if the path is absent. One binding of realpath(3) for
;; the whole runtime, in java/io.ss, which loads before this file and needs it
;; for File.getCanonicalPath.
(define (nio-realpath fp) (if (win32?) (win32-final-path fp) (jfile-realpath fp)))
(define (nio-mode->perm-set mode)
  (let ((low (bitwise-and mode #o777)))
    (make-perm-set
      (let loop ((os posix-order) (bs posix-bits) (acc '()))
        (cond ((null? os) (reverse acc))
              ((> (bitwise-and low (car bs)) 0) (loop (cdr os) (cdr bs) (cons (make-pfp (car os)) acc)))
              (else (loop (cdr os) (cdr bs) acc)))))))
(let ((files-stat
       (list (cons "getPosixFilePermissions"
                   (lambda (p . _) (nio-posix-view!) (nio-mode->perm-set (or (nio-stat-mode (nfp p)) #o755)))))))
  (set! files-accum-chunks (cons files-stat files-accum-chunks)))
;; instance? FileTime
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (symbol-t? type-sym) (file-time? val))
        (let ((n (symbol-t-name type-sym)))
          (if (or (string=? n "FileTime") (string=? n "java.nio.file.attribute.FileTime")) #t 'pass))
        'pass)))

;; ---- NOFOLLOW predicates, directory copy, owner, file-attribute perms -------
;; With NOFOLLOW_LINKS a symlink is examined as itself: it is neither a
;; directory nor a regular file, and it "exists" as long as the link is present.
(define (nio-opts-nofollow? args)
  (exists (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "link-option")))
          (npath-spread-args args)))
(let ((files-nofollow
       (list
        (cons "exists" (lambda (p . opts)
                         (let ((fp (nfp p)))
                           (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp)) #t
                               (if (file-exists? fp) #t #f)))))
        (cons "isDirectory" (lambda (p . opts)
                              (let ((fp (nfp p)))
                                (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp)) #f
                                    (if (file-directory? fp) #t #f)))))
        (cons "isRegularFile" (lambda (p . opts)
                                (let ((fp (nfp p)))
                                  (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp)) #f
                                      (if (and (file-exists? fp) (not (file-directory? fp))) #t #f))))))))
  (set! files-accum-chunks (cons files-nofollow files-accum-chunks)))
(register-host-methods! "user-principal"
  (list (cons "getName" (lambda (self) (jhost-state self)))
        (cons "toString" (lambda (self) (jhost-state self)))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "user-principal")))
                      (lambda (x) (jhost-state x)))

;; PosixFilePermissions/asFileAttribute -> a FileAttribute the create ops apply
;; by chmod after making the entry.
(define (file-attr? x) (and (jhost? x) (string=? (jhost-tag x) "file-attribute")))
(register-class-statics! "java.nio.file.attribute.PosixFilePermissions"
  (list (cons "asFileAttribute" (lambda (perms) (make-jhost "file-attribute" perms)))))


;; java.util.regex.Pattern statics (incl. quote) are registered once in
;; host-static-classes.ss — no per-file copy here.

;; ---- umask-masked create perms, symlink-aware move/copy ---------------------
(define c-umask (jolt-foreign-proc-safe "umask" '(int) 'int))
(define (nio-current-umask) (if c-umask (let ((old (c-umask 0))) (c-umask old) old) 0))
;; chmod a created entry to the requested permission file-attribute, masked by
;; the umask — exactly what java.nio.file's create* do.
(define (nio-apply-attrs-umask! fp args)
  (let ((um (nio-current-umask)))
    (when (exists file-attr? (npath-spread-args args))
      (nio-posix-view! "'posix:permissions' not supported as initial attribute"))
    (for-each (lambda (a) (when (and (file-attr? a) c-chmod)
                            (c-chmod fp (bitwise-and (posix-set->mode (jhost-state a)) (bitwise-not um)))))
              (npath-spread-args args))))
(define (nio-parent-of fp)
  (let loop ((i (- (string-length fp) 1)))
    (cond ((< i 0) "") ((char=? (string-ref fp i) #\/) (substring fp 0 i)) (else (loop (- i 1))))))
(define (nio-blocking-ancestor fp)   ; nearest existing ancestor that is not a directory
  (let loop ((p (nio-parent-of fp)))
    (cond ((or (string=? p "") (string=? p "/")) #f)
          ((file-exists? p) (and (not (file-directory? p)) p))
          (else (loop (nio-parent-of p))))))
(define (nio-missing-ancestors fp)   ; the not-yet-existing path chain, shallowest first
  (let loop ((p fp) (acc '()))
    (cond ((or (string=? p "") (string=? p "/") (file-exists? p)) acc)
          (else (loop (nio-parent-of p) (cons p acc))))))
;; is the dest present as a link (even broken) or a real file?
(define (nio-dest-present? d) (or (file-exists? d) (nio-is-symlink? d)))
(let ((files-create+move
       (list
        (cons "createDirectory" (lambda (p . attrs)
                                  (let ((fp (nfp p)))
                                    ;; mkdir's EEXIST and ENOENT come back untyped, so name them
                                    ;; here. A non-directory in the way is neither: that is
                                    ;; ENOTDIR, which nio-fs-call renders as a FileSystemException
                                    ;; exactly as the JVM does.
                                    (when (nio-dest-present? fp) (nio-already-exists (nio-shown p)))
                                    (let ((parent (nio-parent-of fp)))
                                      (when (and (not (string=? parent "")) (not (file-exists? parent)))
                                        (nio-no-such-file (nio-shown p))))
                                    (nio-fs-call fp (lambda () (mkdir fp)) (nio-shown p))
                                    (nio-apply-attrs-umask! fp attrs) (->path p))))
        ;; CREATE_NEW's open, for the same reason Files/newOutputStream takes it:
        ;; `no-fail` here made createFile TRUNCATE an existing file and return it.
        (cons "createFile" (lambda (p . attrs)
                             (let ((fp (nfp p)))
                               (close-port (nio-fs-call fp (lambda () (open-file-output-port fp (file-options))) (nio-shown p)))
                               (nio-apply-attrs-umask! fp attrs) (->path p))))
        (cons "createDirectories" (lambda (p . attrs)
                                    (let ((fp (nfp p)))
                                      ;; an existing directory is a no-op; anything else in the
                                      ;; way -- at the target or above it -- is the JVM's
                                      ;; FileAlreadyExistsException, named for what blocks
                                      (when (and (file-exists? fp) (not (file-directory? fp)))
                                        (nio-already-exists (nio-shown p)))
                                      (let ((blocked (nio-blocking-ancestor fp)))
                                        (when blocked (nio-already-exists blocked)))
                                      (let ((missing (nio-missing-ancestors fp)))
                                        (nio-fs-call fp (lambda () (mkdirs! fp)))
                                        (for-each (lambda (d) (nio-apply-attrs-umask! d attrs)) missing)))
                                    (->path p)))
        (cons "move" (lambda (src dst . opts)
                       (let ((s (nfp src)) (d (nfp dst)))
                         (cond
                           ((string=? s d) (->path dst))
                           ((not (nio-dest-present? s)) (nio-no-such-file (nio-shown src)))
                           ((and (nio-dest-present? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                            (nio-already-exists (nio-shown dst)))
                           (else
                            ;; The JDK removes the target first and reports a
                            ;; target it cannot remove naming the TARGET
                            ;; (AccessDeniedException for a held file or an
                            ;; unwritable directory), never the source
                            ;; (jolt-lang/jolt#1263). nio-delete1 raises that.
                            (when (nio-dest-present? d)
                              (nio-delete1 d #t (nio-shown dst)))
                            (nio-fs-call s (lambda () (rename-file s d)) (nio-shown src)) (->path dst)))))))))
  (set! files-accum-chunks (cons files-create+move files-accum-chunks)))

;; ---- nofollow timestamps (the link's own mtime, via lstat/lutimes) ----------
(define c-lstat (jolt-foreign-proc-safe "lstat" '(string u8*) 'int))
(define (nio-stat-buf fp follow?)                ; struct stat of FP, or #f
  (let ((f (if follow? c-stat c-lstat)))
    (and f (nio-stat-layout)
         (let ((buf (make-bytevector 256 0)))
           (and (= 0 (f fp buf)) buf)))))
;; A timespec as the JDK's UnixFileAttributes.toFileTime makes it: whole
;; seconds when there is no fraction, else nanoseconds.
(define (nio-timespec-file-time sec nsec)
  (if (= nsec 0)
      (make-file-time-in sec 1000000000)
      (make-file-time-ns (+ (* sec 1000000000) nsec))))
(define (nio-stat-file-time buf off)
  (nio-timespec-file-time (bytevector-s64-ref buf off (native-endianness))
                          (bytevector-s64-ref buf (+ off 8) (native-endianness))))

;; ---- the access and creation times (jolt-ow0x) --------------------------------
;; The shim used to keep one time per file and answer the mtime for all three,
;; so fs/last-access-time and fs/creation-time were fs/last-modified-time under
;; other names. They are read where the JDK reads them: st_atime, and the birth
;; time from struct stat on macOS, statx(2) on Linux, and GetFileAttributesEx's
;; FILETIMEs on Windows. A host or filesystem with no birth time answers the
;; mtime, which is the JDK's fallback too.
(define c-statx (jolt-foreign-proc-safe "statx" '(int string int unsigned-32 u8*) 'int))
(define (nio-statx-btime fp follow?)
  ;; statx(AT_FDCWD, fp, AT_SYMLINK_NOFOLLOW?, STATX_BTIME, buf): stx_mask@0
  ;; says whether the filesystem answered, stx_btime@80 is {s64 sec, u32 nsec}.
  (and c-statx (eq? (sa-os-family) 'linux)
       (let ((buf (make-bytevector 256 0)))
         (and (= 0 (c-statx -100 fp (if follow? 0 #x100) #x800 buf))
              (not (= 0 (bitwise-and (bytevector-u32-ref buf 0 (native-endianness)) #x800)))
              (nio-timespec-file-time (bytevector-s64-ref buf 80 (native-endianness))
                                      (bytevector-u32-ref buf 88 (native-endianness)))))))
;; The time NM ("lastModifiedTime", "lastAccessTime" or "creationTime") of FP as
;; a FileTime at the resolution the filesystem keeps — nanoseconds from struct
;; stat / statx, 100ns FILETIME ticks on Windows — through a symbolic link or,
;; FOLLOW? #f, of the link itself. #f when it cannot be read.
(define (nio-read-time fp nm follow?)
  (if (nio-windows?)
      (let ((t (win32-file-times fp follow?)))
        (and t (make-file-time-ns
                (vector-ref t (cond ((string=? nm "creationTime") 0)
                                    ((string=? nm "lastAccessTime") 1)
                                    (else 2))))))
      (let ((lay (nio-stat-layout)))
        (and lay
             (let ((off (cond ((string=? nm "lastAccessTime") (nio-layout-atime-off lay))
                              ((string=? nm "creationTime") (nio-layout-birth-off lay))
                              (else (nio-layout-mtime-off lay)))))
               (if off
                   (let ((buf (nio-stat-buf fp follow?))) (and buf (nio-stat-file-time buf off)))
                   (nio-statx-btime fp follow?)))))))
;; The same, never #f: a host or filesystem with no birth time answers the mtime,
;; which is the JDK's fallback too, and a host with no stat layout the
;; millisecond mtime Chez reads.
(define (nio-time-attr fp nm follow?)
  (or (nio-read-time fp nm follow?)
      (and (not (string=? nm "lastModifiedTime")) (nio-read-time fp "lastModifiedTime" follow?))
      (make-file-time (file-mtime-millis fp))))

;; Setting them. utimensat(2) with UTIME_OMIT in the mtime slot moves the access
;; time alone; its constants are per-OS (measured with cc/gcc as above). The
;; creation time is settable on macOS through setattrlist(ATTR_CMN_CRTIME) and on
;; Windows through SetFileTime; Linux has no way to set it and the JDK ignores
;; the set there, so this answers #t without doing anything. Both answer whether
;; the time was set.
(define c-setattrlist (jolt-foreign-proc-safe "setattrlist" '(string u8* u8* size_t unsigned-long) 'int))
;; Each setter takes epoch NANOSECONDS; io.ss set-file-times-ns! is the one
;; utimensat / SetFileTime call for the access and modification times.
(define (nio-set-mtime! fp ns follow?) (set-file-times-ns! fp #f ns follow?))
(define (nio-set-access-time! fp ns follow?) (set-file-times-ns! fp ns #f follow?))
(define (nio-set-creation-time! fp ns follow?)
  (cond ((nio-windows?) (win32-set-file-times! fp ns #f #f follow?))
        ((eq? (sa-os-family) 'macos)
         ;; struct attrlist: bitmapcount=ATTR_BIT_MAP_COUNT(5)@0, commonattr@4;
         ;; the buffer is the one timespec; options FSOPT_NOFOLLOW=1.
         (and c-setattrlist
              (let ((al (make-bytevector 24 0)) (ts (make-bytevector 16 0)))
                (bytevector-u16-set! al 0 5 (native-endianness))
                (bytevector-u32-set! al 4 #x200 (native-endianness))
                (timespec-bytes! ts 0 ns)
                (= 0 (c-setattrlist fp al ts 16 (if follow? 0 1))))))
        (else #t)))
(let ((files-nofollow-time
       (list
        (cons "getLastModifiedTime" (lambda (p . opts)
                                      (let ((fp (nfp p)) (follow? (not (nio-opts-nofollow? opts))))
                                        (nio-attrs-require-exists! fp follow? (nio-shown p))
                                        (nio-attr-value fp "lastModifiedTime" follow?))))
        (cons "getAttribute" (lambda (path attr . opts)
                               (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr)))
                                     (follow? (not (nio-opts-nofollow? opts))))
                                 (nio-attrs-require-exists! fp follow? (nio-shown path))
                                 (nio-attr-value fp nm follow?)))))))
  (set! files-accum-chunks (cons files-nofollow-time files-accum-chunks)))

;; java.nio.channels.FileChannel/open — babashka.fs/touch uses it only to create
;; a file (CREATE + WRITE) inside with-open, so support open+close of a channel.
;; State: #(path size-at-open|#f). DELETE_ON_CLOSE unlinks the file as the
;; channel opens, as the stream opens do (nio-delete-on-close!); nothing writes
;; through this channel, so the size it had then is its size.
(define (fc-close! self) jolt-nil)
(let ((fc-statics
       (list (cons "open" (lambda (path . opts)
                            (if (nio-path? path)
                                (apply (nio-path-op path "newFileChannel") path opts)
                                (apply nio-host-file-channel path opts)))))))
  (register-class-statics! "FileChannel" fc-statics)
  (register-class-statics! "java.nio.channels.FileChannel" fc-statics))
(define (nio-host-file-channel path . opts)
                                (let ((fp (nfp path)))
                                  (when (and (nio-opts-have? opts oopt-sym 'create) (not (file-exists? fp)))
                                    (close-port (open-file-output-port fp (file-options no-fail))))
                                  (if (nio-opts-have? opts oopt-sym 'delete-on-close)
                                      (begin
                                        (when (nio-windows?)
                                          (throw-jvm 'UnsupportedOperationException "DELETE_ON_CLOSE not supported"))
                                        (let ((size (nio-size fp (nio-shown path))))
                                          (guard (_ (#t #f)) (delete-file fp))
                                          (make-jhost "file-channel" (vector fp size))))
                                      (make-jhost "file-channel" (vector fp #f)))))
(hashtable-set! (nio-provider-path-ops default-nio-provider) "newFileChannel" nio-host-file-channel)
(register-host-methods! "file-channel"
  (list (cons "close" fc-close!)
        (cons "size" (lambda (self) (or (vector-ref (jhost-state self) 1)
                                        (nio-size (vector-ref (jhost-state self) 0)))))))
(let ((prev jolt-close))
  (set! jolt-close (lambda (x) (if (and (jhost? x) (string=? (jhost-tag x) "file-channel")) (fc-close! x) (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; A missing target makes the time setters throw NoSuchFileException, as
;; java.nio.file does — babashka.fs/touch relies on catching it to create the file.
(define (nio-require-exists fp shown)
  (unless (or (file-exists? fp) (nio-is-symlink? fp))
    (nio-no-such-file shown)))
;; Files.setLastModifiedTime reports a time it could not set as an IOException,
;; where java.io.File.setLastModified answers false. The setters below answer
;; whether they set it, and discarding that is how a directory's mtime on
;; Windows went unset with no sign of it (jolt-lang/jolt#1119).
(define (nio-time-set-or-raise! fp what set?)
  (unless set?
    (nio-fs-throw "java.nio.file.FileSystemException" fp #f (string-append "cannot set the " what))))
(define (nio-mtime-set-or-raise! fp set?) (nio-time-set-or-raise! fp "last modified time" set?))
(let ((files-throwing-setters
       (list
        (cons "setLastModifiedTime" (lambda (p t) (let ((fp (nfp p))) (nio-require-exists fp (nio-shown p))
                                                    (nio-mtime-set-or-raise! (nio-shown p) (nio-set-mtime! fp (file-time-ns t) #t))
                                                    (->path p))))
        (cons "setAttribute" (lambda (path attr value . opts)
                               (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
                                 (when (member nm '("lastModifiedTime" "creationTime" "lastAccessTime"))
                                   (nio-require-exists fp (nio-shown path))
                                   (let ((ns (if (file-time? value) (file-time-ns value) (* (jnum->exact value) 1000000)))
                                         (follow? (not (nio-opts-nofollow? opts))))
                                     (cond ((string=? nm "lastModifiedTime")
                                            (nio-mtime-set-or-raise! (nio-shown path) (nio-set-mtime! fp ns follow?)))
                                           ((string=? nm "lastAccessTime")
                                            (nio-time-set-or-raise! (nio-shown path) "last access time" (nio-set-access-time! fp ns follow?)))
                                           (else
                                            (nio-time-set-or-raise! (nio-shown path) "creation time" (nio-set-creation-time! fp ns follow?))))))
                                 (->path path)))))))
  (set! files-accum-chunks (cons files-throwing-setters files-accum-chunks)))

;; isSameFile compares inodes (so hard links are the same file); copy preserves
;; the source permissions by default, like java.nio.file on this host.
(define (nio-stat-ino fp)
  (and c-stat (let ((buf (make-bytevector 256 0)))
                (and (= 0 (c-stat fp buf)) (bytevector-u64-ref buf 8 (native-endianness))))))
;; BasicFileAttributes.fileKey: on POSIX the JDK's UnixFileKey, the (st_dev,
;; st_ino) pair, which is what makes two hard links to one file the same key;
;; Windows answers null, as its provider does. It answered nil everywhere.
;; toString and hashCode are UnixFileKey's: dev in unsigned hex, ino as a signed
;; long, and each folded to an int and summed.
(define (nio-file-key fp follow?)
  (let ((buf (and (not (nio-windows?)) (nio-stat-buf fp follow?))))
    (if buf
        (make-jhost "file-key"
                    (cons (bitwise-and (nio-layout-dev-ref (nio-stat-layout) buf) #xFFFFFFFFFFFFFFFF)
                          (bytevector-u64-ref buf 8 (native-endianness))))
        jolt-nil)))
(define-jhost-tag-pred (file-key? t) (string=? t "file-key"))
(define (nio-s64 u) (if (>= u #x8000000000000000) (- u #x10000000000000000) u))
(define (nio-file-key-string k)
  (let ((dev (car (jhost-state k))) (ino (cdr (jhost-state k))))
    (string-append "(dev=" (string-downcase (number->string dev 16)) ",ino=" (number->string (nio-s64 ino)) ")")))
(define (nio-file-key-hash k)
  (define (fold x) (bitwise-and (bitwise-xor x (bitwise-arithmetic-shift-right x 32)) #xFFFFFFFF))
  (let ((h (bitwise-and (+ (fold (car (jhost-state k))) (fold (cdr (jhost-state k)))) #xFFFFFFFF)))
    (if (>= h #x80000000) (- h #x100000000) h)))
(register-host-methods! "file-key"
  (list (cons "toString" (lambda (self) (nio-file-key-string self)))
        (cons "hashCode" (lambda (self) (nio-file-key-hash self)))
        (cons "equals"   (lambda (self o) (and (file-key? o) (equal? (jhost-state self) (jhost-state o)))))))
(register-str-render! file-key? nio-file-key-string)
(register-eq-arm! (lambda (a b) (and (file-key? a) (file-key? b)))
                  (lambda (a b) (equal? (jhost-state a) (jhost-state b))))
(register-hash-arm! file-key? nio-file-key-hash)
(let ((files-final
       (list
        (cons "isSameFile" (lambda (a b)
                             (or (string=? (nfp a) (nfp b))
                                 (let ((ia (nio-stat-ino (nfp a))) (ib (nio-stat-ino (nfp b))))
                                   (and ia ib (= ia ib) #t)))))
        (cons "copy" (lambda (src dst . opts)
                       (cond
                         ;; copy(InputStream, Path, CopyOption...): the stream's
                         ;; bytes become the file, replaced only with
                         ;; REPLACE_EXISTING; the count is answered. A jolt
                         ;; in-stream or a reify InputStream (io-streams.ss).
                         ((or (in-stream? src) (user-in-stream? src))
                          (let ((d (nfp dst)))
                            (when (and (nio-dest-present? d)
                                       (not (nio-opts-have? opts copt-sym 'replace-existing)))
                              (nio-already-exists (nio-shown dst)))
                            (when (nio-dest-present? d) (nio-delete1 d #t))
                            ;; a chunk at a time (io-streams.ss copy-bytes-into!),
                            ;; as the JDK copies through an 8 KiB buffer: the
                            ;; stream never has to fit in memory
                            (let ((port (open-file-output-port d (file-options no-fail) (buffer-mode block))))
                              (->num (dynamic-wind
                                       (lambda () #f)
                                       (lambda () (copy-bytes-into! src (lambda (bv) (put-bytevector port bv))))
                                       (lambda () (close-port port)))))))
                         ;; copy(Path, OutputStream): the file's bytes into the
                         ;; stream a chunk at a time; the count is answered.
                         ((or (out-stream? dst) (user-out-stream? dst))
                          (let ((s (nfp src)))
                            (unless (nio-dest-present? s) (nio-no-such-file (nio-shown src)))
                            (io-note-file-read! s)
                            (let ((port (nio-open-input-port s (nio-shown src))))
                              (->num (dynamic-wind
                                       (lambda () #f)
                                       (lambda ()
                                         (copy-port-chunks! port
                                           (lambda (bv)
                                             (record-method-dispatch dst "write"
                                               (list->cseq (list (na-byte-array bv) (->num 0) (->num (bytevector-length bv))))))))
                                       (lambda () (close-port port)))))))
                         (else
                       (let ((s (nfp src)) (d (nfp dst)))
                         (cond
                           ((string=? s d) (->path dst))
                           ((not (nio-dest-present? s)) (nio-no-such-file (nio-shown src)))
                           ((and (nio-dest-present? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                            (nio-already-exists (nio-shown dst)))
                           (else
                            (when (nio-dest-present? d) (nio-delete1 d #t))
                            (cond
                              ((and (nio-opts-nofollow? opts) (nio-is-symlink? s))
                               (when c-symlink (c-symlink (or (nio-readlink s) "") d)))
                              ((file-directory? s) (unless (file-exists? d) (mkdir d)))
                              (else
                               (nio-write-bv! d (nio-read-bv s))
                               (let ((mode (nio-stat-mode s)))            ; preserve source perms
                                 (when (and mode c-chmod) (c-chmod d (bitwise-and mode #o777))))
                               ;; COPY_ATTRIBUTES carries both times, at full resolution
                               (when (nio-opts-have? opts copt-sym 'copy-attributes)
                                 (set-file-times-ns! d (file-time-ns (nio-time-attr s "lastAccessTime" #t))
                                                     (file-time-ns (nio-time-attr s "lastModifiedTime" #t)) #t))))
                            (->path dst))))))))) ))
  (set! files-accum-chunks (cons files-final files-accum-chunks)))

;; getOwner resolves the real owning user (stat st_uid -> getpwuid -> pw_name),
;; so it distinguishes root-owned paths from user files.
(define c-getpwuid (jolt-foreign-proc-safe "getpwuid" '(unsigned-int) 'iptr))
(define (nio-cstr-at addr)                      ; a NUL-terminated C string at a raw address
  (let loop ((i 0) (acc '()))
    (let ((b (sa-foreign-ref 'unsigned-8 addr i)))
      (if (= b 0) (list->string (map integer->char (reverse acc)))
          (loop (+ i 1) (cons b acc))))))
(define (nio-stat-uid fp)
  (and c-stat (begin
                (nio-stat-layout-guard! "getOwner")
                (let ((lay (nio-stat-layout)) (buf (make-bytevector 256 0)))
                  (and (= 0 (c-stat fp buf)) (nio-layout-uid-ref lay buf))))))
(define (nio-uid->name uid)
  (and c-getpwuid (let ((pw (c-getpwuid uid))) (and (not (= 0 pw)) (nio-cstr-at (sa-foreign-ref 'iptr pw 0))))))
;; user-principal values compare and hash by name; getOwner honors NOFOLLOW.
(define-jhost-tag-pred (nio-userprin? t) (string=? t "user-principal"))
(register-eq-arm! (lambda (a b) (and (nio-userprin? a) (nio-userprin? b)))
                  (lambda (a b) (string=? (jhost-state a) (jhost-state b))))
(register-hash-arm! nio-userprin? (lambda (x) (string-hash (jhost-state x))))
(define (nio-lstat-uid fp)
  (and c-lstat (begin
                 (nio-stat-layout-guard! "getOwner")
                 (let ((lay (nio-stat-layout)) (buf (make-bytevector 256 0)))
                   (and (= 0 (c-lstat fp buf)) (nio-layout-uid-ref lay buf))))))
(let ((files-owner2
       (list (cons "getOwner" (lambda (p . opts)
                                (let* ((fp (nfp p))
                                       (uid (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp))
                                                (nio-lstat-uid fp) (nio-stat-uid fp))))
                                  (make-jhost "user-principal"
                                              (or (and uid (nio-uid->name uid)) (getenv "USER") ""))))))))
  (set! files-accum-chunks (cons files-owner2 files-accum-chunks)))

;; ---- the host filesystem's channels, stores and owners ------------------------
;; Files.newByteChannel: a SeekableByteChannel over the file. A read channel is an
;; input port; WRITE / APPEND open the file through the same option table
;; newOutputStream uses, on a port that also reads.
;; State: #(port open? append?)
(define (nio-host-byte-channel p . opts)
  (let* ((fp (nfp p))
         (shown (nio-shown p))
         (write? (or (nio-opts-have? opts oopt-sym 'write) (nio-opts-have? opts oopt-sym 'append)))
         (append? (nio-opts-have? opts oopt-sym 'append))
         (port (if write?
                   (nio-fs-call fp (lambda () (open-file-input/output-port fp (nio-output-file-options opts))) shown)
                   (nio-open-input-port fp shown))))
    (when write? (nio-delete-on-close! fp port opts))
    (make-jhost "host-byte-channel" (vector port #t append?))))
(define (nio-hbc-port self) (vector-ref (jhost-state self) 0))
(define (nio-hbc-open! self)
  (unless (vector-ref (jhost-state self) 1)
    (throw-jvm 'java.nio.channels.ClosedChannelException jolt-nil)))
(define (nio-hbc-writable! self)
  (unless (output-port? (nio-hbc-port self))
    (throw-jvm 'java.nio.channels.NonWritableChannelException jolt-nil)))
(define (nio-hbc-read self buffer)
  (nio-hbc-open! self)
  (let ((want (nb-remaining buffer)))
    (if (= want 0)
        0
        (let ((bv (get-bytevector-n (nio-hbc-port self) want)))
          (if (eof-object? bv)
              -1
              (let ((n (bytevector-length bv)))
                (nb-put buffer (na-byte-array bv) 0 n)
                n))))))
(define (nio-hbc-write self buffer)
  (nio-hbc-open! self)
  (nio-hbc-writable! self)
  (let* ((n (nb-remaining buffer))
         (arr (na-byte-array (make-bytevector n)))
         (port (nio-hbc-port self)))
    (nb-get buffer arr 0 n)
    (when (vector-ref (jhost-state self) 2)
      (set-port-position! port (file-length port)))
    (put-bytevector port (na-bytearray->bv arr))
    (flush-output-port port)
    n))
(register-host-methods! "host-byte-channel"
  (list
   (cons "read" nio-hbc-read)
   (cons "write" nio-hbc-write)
   (cons "position" (lambda (self . position)
                      (nio-hbc-open! self)
                      (if (pair? position)
                          (begin (set-port-position! (nio-hbc-port self) (jnum->exact (car position))) self)
                          (port-position (nio-hbc-port self)))))
   (cons "size" (lambda (self) (nio-hbc-open! self) (file-length (nio-hbc-port self))))
   (cons "truncate" (lambda (self size)
                      (nio-hbc-open! self)
                      (nio-hbc-writable! self)
                      (let ((size (jnum->exact size)) (port (nio-hbc-port self)))
                        (when (< size (file-length port)) (set-port-length! port size))
                        (when (> (port-position port) size) (set-port-position! port size)))
                      self))
   (cons "isOpen" (lambda (self) (vector-ref (jhost-state self) 1)))
   (cons "close" (lambda (self)
                   (when (vector-ref (jhost-state self) 1)
                     (vector-set! (jhost-state self) 1 #f)
                     (close-port (nio-hbc-port self)))
                   jolt-nil))))
(hashtable-set! jhost-tag->fqn "host-byte-channel" "java.nio.channels.SeekableByteChannel")
(let ((prev jolt-close))
  (set! jolt-close (lambda (x) (if (and (jhost? x) (string=? (jhost-tag x) "host-byte-channel"))
                                   (record-method-dispatch x "close" jolt-nil)
                                   (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; Files.getFileStore: the mounted filesystem a path lives on, read through
;; statfs(2). The struct is an ABI, so each host names its own offsets (as the
;; stat layouts above do) and a host with none refuses rather than reading
;; garbage. Linux names the mount from /proc/self/mounts, macOS from the
;; statfs record itself.
;;   #(block-size-offset block-size-width blocks bfree bavail flags-offset
;;     type-name-offset mount-on-offset mount-from-offset)
(define c-statfs (or (jolt-foreign-proc-safe "statfs$INODE64" '(string u8*) 'int)
                     (jolt-foreign-proc-safe "statfs" '(string u8*) 'int)))
(define nio-statfs-layout
  (case (sa-os-family)
    ((macos) (vector 0 4 8 16 24 64 72 88 1112))
    ((linux) (vector 72 8 16 24 32 80 #f #f #f))
    (else #f)))
(define (nio-statfs fp shown)
  (unless (and c-statfs nio-statfs-layout)
    (throw-jvm 'UnsupportedOperationException
               (string-append "getFileStore is not supported on this host: " (sa-host-tag))))
  (let ((buf (make-bytevector 4096 0)))
    (unless (= 0 (c-statfs fp buf))
      (let ((err (io-errno)))
        (if (and err (= err nio-ENOENT))
            (nio-no-such-file shown)
            (nio-fs-detail shown (and c-strerror err (c-strerror err))))))
    buf))
(define (nio-bv-cstr bv off)
  (let loop ((i off))
    (if (or (>= i (bytevector-length bv)) (= 0 (bytevector-u8-ref bv i)))
        (let ((out (make-bytevector (- i off))))
          (bytevector-copy! bv off out 0 (- i off))
          (utf8->string out))
        (loop (+ i 1)))))
;; Linux: the /proc/self/mounts row whose mount point is the longest prefix of
;; the real path; its device and type are the store's name and type.
(define (nio-split-spaces line)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length line))
           (reverse (if (> i start) (cons (substring line start i) acc) acc)))
          ((char=? (string-ref line i) #\space)
           (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring line start i) acc) acc)))
          (else (loop (+ i 1) start acc)))))
(define (nio-unescape-mount s)                 ; /proc/mounts writes " " as \040
  (let loop ((i 0) (acc '()))
    (cond ((= i (string-length s)) (list->string (reverse acc)))
          ((and (char=? (string-ref s i) #\\) (<= (+ i 4) (string-length s))
                (string->number (substring s (+ i 1) (+ i 4)) 8))
           => (lambda (n) (loop (+ i 4) (cons (integer->char n) acc))))
          (else (loop (+ i 1) (cons (string-ref s i) acc))))))
(define (nio-mount-covers? dir real)
  (or (string=? dir "/")
      (string=? dir real)
      (and (> (string-length real) (string-length dir))
           (string=? (substring real 0 (string-length dir)) dir)
           (char=? (string-ref real (string-length dir)) #\/))))
(define (nio-proc-mount-entry real)
  (let ((rows (guard (e (#t '()))
                (call-with-input-file "/proc/self/mounts"
                  (lambda (in)
                    (let loop ((acc '()))
                      (let ((l (get-line in)))
                        (if (eof-object? l) (reverse acc) (loop (cons l acc))))))))))
    (let loop ((rows rows) (best #f) (best-len -1))
      (if (null? rows)
          best
          (let* ((fields (nio-split-spaces (car rows)))
                 (dir (and (>= (length fields) 3) (nio-unescape-mount (cadr fields)))))
            (if (and dir (> (string-length dir) best-len) (nio-mount-covers? dir real))
                (loop (cdr rows) (list (nio-unescape-mount (car fields)) dir (caddr fields))
                      (string-length dir))
                (loop (cdr rows) best best-len)))))))
;; #(name type mount-point read-only? block-size blocks free available)
(define (nio-host-file-store p)
  (let* ((fp (nfp p))
         (buf (nio-statfs fp (nio-shown p)))
         (lay nio-statfs-layout)
         (u64 (lambda (off) (bytevector-u64-native-ref buf off)))
         (bsize (if (= (vector-ref lay 1) 4)
                    (bytevector-u32-native-ref buf (vector-ref lay 0))
                    (u64 (vector-ref lay 0))))
         (flags (bytevector-u32-native-ref buf (vector-ref lay 5)))
         (entry (if (vector-ref lay 6)
                    (list (nio-bv-cstr buf (vector-ref lay 8)) (nio-bv-cstr buf (vector-ref lay 7))
                          (nio-bv-cstr buf (vector-ref lay 6)))
                    (or (nio-proc-mount-entry (or (nio-realpath fp) fp)) (list "" "/" "")))))
    (make-jhost "host-file-store"
                (vector (car entry) (caddr entry) (cadr entry) (odd? flags) bsize
                        (u64 (vector-ref lay 2)) (u64 (vector-ref lay 3)) (u64 (vector-ref lay 4))))))
(define (nio-store-ref self i) (vector-ref (jhost-state self) i))
(define (nio-store-space self i) (* (nio-store-ref self 4) (nio-store-ref self i)))
(register-host-methods! "host-file-store"
  (list
   (cons "name" (lambda (self) (nio-store-ref self 0)))
   (cons "type" (lambda (self) (nio-store-ref self 1)))
   (cons "toString" (lambda (self) (string-append (nio-store-ref self 2) " (" (nio-store-ref self 0) ")")))
   (cons "isReadOnly" (lambda (self) (nio-store-ref self 3)))
   (cons "getBlockSize" (lambda (self) (nio-store-ref self 4)))
   (cons "getTotalSpace" (lambda (self) (nio-store-space self 5)))
   (cons "getUnallocatedSpace" (lambda (self) (nio-store-space self 6)))
   (cons "getUsableSpace" (lambda (self) (nio-store-space self 7)))
   (cons "supportsFileAttributeView"
         (lambda (self view) (and (member (nio-view-name view) (nio-host-attribute-views)) #t)))
   (cons "getAttribute"
         (lambda (self attribute)
           (let ((a (npath-string-of attribute)))
             (cond ((string=? a "totalSpace") (nio-store-space self 5))
                   ((string=? a "unallocatedSpace") (nio-store-space self 6))
                   ((string=? a "usableSpace") (nio-store-space self 7))
                   (else (throw-jvm 'UnsupportedOperationException
                                    (string-append "'" a "' not recognized")))))))
   (cons "getFileStoreAttributeView" (lambda (self view) jolt-nil))))
(hashtable-set! jhost-tag->fqn "host-file-store" "java.nio.file.FileStore")

;; Files.setOwner: chown(2) to the uid getpwnam(3) answers for the principal's
;; name. struct passwd keeps pw_uid at 16 on every 64-bit host here.
(define c-chown (jolt-foreign-proc-safe "chown" '(string unsigned-int unsigned-int) 'int))
(define c-getpwnam (jolt-foreign-proc-safe "getpwnam" '(string) 'iptr))
(define (nio-host-set-owner p owner)
  (unless (nio-userprin? owner)
    (throw-jvm 'ClassCastException "owner is not a java.nio.file.attribute.UserPrincipal"))
  (unless (and c-chown c-getpwnam)
    (throw-jvm 'UnsupportedOperationException "setOwner is not supported on this host"))
  (let ((pw (c-getpwnam (jhost-state owner))))
    (when (= 0 pw)
      (zip-throw "java.nio.file.attribute.UserPrincipalNotFoundException" (jhost-state owner)))
    (unless (= 0 (c-chown (nfp p) (sa-foreign-ref 'unsigned-32 pw 16) #xFFFFFFFF))
      (let ((err (io-errno)))
        (cond ((and err (= err nio-ENOENT)) (nio-no-such-file (nio-shown p)))
              ((and err (= err nio-EACCES))
               (nio-fs-throw "java.nio.file.AccessDeniedException" (nio-shown p)))
              (else (nio-fs-detail (nio-shown p) (and c-strerror err (c-strerror err)))))))
    (->path p)))

;; ---- attribute views, over any provider ---------------------------------------
;; A view reads and writes through the Files operations of its path's provider,
;; so a filesystem gets the views by answering readAttributes and setAttribute;
;; which views it offers is the list it hands nio-attribute-view, and a
;; read-only filesystem hands a REFUSE that every setter answers first, as the
;; JDK's zip filesystem checks writability before anything else.
(define (nio-view-name view)
  (let ((n (if (jclass? view) (jclass-name view) (npath-string-of view))))
    (cond ((string=? n "java.nio.file.attribute.BasicFileAttributeView") "basic")
          ((string=? n "java.nio.file.attribute.FileOwnerAttributeView") "owner")
          ((string=? n "java.nio.file.attribute.PosixFileAttributeView") "posix")
          (else n))))
(define (nio-host-attribute-views) (if (nio-windows?) '("basic" "owner") '("basic" "owner" "posix")))
(define (nio-attribute-view supported . refuse)
  (lambda (p view . opts)
    (if (member (nio-view-name view) (if (procedure? supported) (supported) supported))
        (make-jhost "nio-attribute-view" (vector (nio-view-name view) p opts (and (pair? refuse) (car refuse))))
        jolt-nil)))
(define (nio-view-writable! self what)
  (let ((refuse (vector-ref (jhost-state self) 3)))
    (when refuse (refuse what))))
(define (nio-view-path self) (vector-ref (jhost-state self) 1))
(define (nio-view-call self op . args)
  (apply nio-files-call op (nio-view-path self) (append args (vector-ref (jhost-state self) 2))))
(register-host-methods! "nio-attribute-view"
  (list
   (cons "name" (lambda (self) (vector-ref (jhost-state self) 0)))
   (cons "readAttributes"
         (lambda (self)
           (nio-view-call self "readAttributes" (jolt-class-for "java.nio.file.attribute.BasicFileAttributes"))))
   (cons "setTimes"
         (lambda (self modified access created)
           (nio-view-writable! self "setTimes")
           (for-each (lambda (attr value)
                       (unless (jolt-nil? value) (nio-view-call self "setAttribute" attr value)))
                     '("basic:lastModifiedTime" "basic:lastAccessTime" "basic:creationTime")
                     (list modified access created))
           jolt-nil))
   (cons "getOwner" (lambda (self) (nio-view-call self "getOwner")))
   (cons "setOwner" (lambda (self owner)
                      (nio-view-writable! self "setOwner")
                      (nio-files-call "setOwner" (nio-view-path self) owner) jolt-nil))
   (cons "setPermissions" (lambda (self perms)
                            (nio-view-writable! self "setPermissions")
                            (nio-files-call "setPosixFilePermissions" (nio-view-path self) perms)
                            jolt-nil))))
(hashtable-set! jhost-tag->fqn "nio-attribute-view" "java.nio.file.attribute.BasicFileAttributeView")

(set! files-accum-chunks
      (cons (list (cons "newByteChannel" nio-host-byte-channel)
                  (cons "getFileStore" nio-host-file-store)
                  (cons "setOwner" nio-host-set-owner)
                  (cons "getFileAttributeView" (nio-attribute-view nio-host-attribute-views)))
            files-accum-chunks))

;; ---- Files operations every provider shares --------------------------------------
;; Text, streams of paths, comparison and cross-filesystem copies are built from a
;; provider's primitives -- its bytes, its directory stream, its attributes --
;; the way the JDK's Files builds them on FileSystemProvider, so one
;; implementation serves every filesystem.
;; A Charset; StandardCharsets' constants are their names here.
(define (nio-charset-arg? x) (or (string? x) (and (jhost? x) (string=? (jhost-tag x) "charset"))))
(define (nio-charset-name args)
  (if (and (pair? args) (nio-charset-arg? (car args))) (string-charset-name args) "UTF-8"))
(define (nio-after-charset args)
  (if (and (pair? args) (nio-charset-arg? (car args))) (cdr args) args))
(define (nio-path-bytes p)
  ((nio-provider-read-bytes (if (nio-path? p) (nio-path-provider p) default-nio-provider)) p))
;; The JDK reads text with a REPORTING decoder: malformed input raises
;; MalformedInputException rather than decoding to U+FFFD.
(define (nio-decode-strict bv cs-name)
  (if (and (string=? (charset-canonical-down cs-name) "utf-8") (%utf8-java-plain? bv))
      (utf8->string bv)
      (let* ((decoder (make-jhost "charset-decoder" (vector (charset-for-name cs-name) #f #f "\xFFFD;")))
             (decoded (decoder-decode-all decoder
                                          (nb-wrap-bytes (na-bv->bytearray bv) 0 (bytevector-length bv)))))
        (nb-char-string decoded (bb-pos decoded) (bb-limit decoded)))))
;; BufferedReader.readLine's terminators: "\n", "\r" and "\r\n".
(define (nio-split-lines s)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((= i n) (reverse (if (> i start) (cons (substring s start i) acc) acc)))
        ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
        ((char=? (string-ref s i) #\return)
         (let ((next (if (and (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\newline)) (+ i 2) (+ i 1))))
           (loop next next (cons (substring s start i) acc))))
        (else (loop (+ i 1) start acc))))))
(define (nio-read-text p args) (nio-decode-strict (nio-path-bytes p) (nio-charset-name args)))

(define (nio-files-read-string p . args) (nio-read-text p args))
(define (nio-files-read-all-lines p . args)
  (make-pvec (list->vector (nio-split-lines (nio-read-text p args)))))
(define (nio-files-lines p . args)
  (make-stream 'ref (list->cseq (nio-split-lines (nio-read-text p args)))))
(define (nio-files-buffered-reader p . args)
  (host-new "BufferedReader"
            (host-new "InputStreamReader" (nio-files-call "newInputStream" p) (nio-charset-name args))))
(define (nio-files-buffered-writer p . args)
  (host-new "BufferedWriter"
            (host-new "OutputStreamWriter"
                      (apply nio-files-call "newOutputStream" p (nio-after-charset args))
                      (nio-charset-name args))))
(define (nio-files-write-string p text . args)
  (let ((bytes (charset-encode-bv (jolt-str-render-one text) (nio-charset-name args))))
    (apply nio-files-call "write" p (na-bv->bytearray bytes) (nio-after-charset args))))
(define (nio-files-mismatch a b)
  (let* ((x (nio-path-bytes a)) (y (nio-path-bytes b))
         (n (min (bytevector-length x) (bytevector-length y))))
    (let loop ((i 0))
      (cond ((< i n) (if (= (bytevector-u8-ref x i) (bytevector-u8-ref y i)) (loop (+ i 1)) i))
            ((= (bytevector-length x) (bytevector-length y)) -1)
            (else n)))))

;; walk / find: pre-order, a directory's children in its directory stream's
;; order; a link is followed only under FOLLOW_LINKS.
(define (nio-walk-list start depth opts)
  (let ((link-opts (if (nio-opts-follow? (list->cseq (npath-spread-args opts))) '() (list fvo-nofollow))))
    (unless (apply nio-files-call "exists" start link-opts)
      (nio-no-such-file (nio-shown start)))
    (let walk ((path start) (level 0) (acc '()))
      (let ((acc (cons path acc)))
        (if (and (< level depth) (apply nio-files-call "isDirectory" path link-opts))
            (fold-left (lambda (acc child) (walk child (+ level 1) acc))
                       acc
                       (seq->list (jolt-seq (nio-files-call "newDirectoryStream" path))))
            acc)))))
(define (nio-files-list p)
  (make-stream 'ref (jolt-seq (nio-files-call "newDirectoryStream" p))))
(define (nio-files-walk p . args)
  (let-values (((depth opts) (if (and (pair? args) (number? (car args)))
                                 (values (jnum->exact (car args)) (cdr args))
                                 (values 2147483647 args))))
    (make-stream 'ref (list->cseq (reverse (nio-walk-list p depth opts))))))
(define (nio-files-find p depth predicate . opts)
  (let ((attrs-class (jolt-class-for "java.nio.file.attribute.BasicFileAttributes")))
    (make-stream 'ref
      (list->cseq
       (filter (lambda (path)
                 (jolt-truthy? (jolt-fi-call predicate "test" path
                                             (nio-files-call "readAttributes" path attrs-class))))
               (reverse (nio-walk-list p (jnum->exact depth) opts)))))))

(define (nio-content-type-of name)
  (let* ((dot (let loop ((i (- (string-length name) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref name i) #\.) i)
                      ((char=? (string-ref name i) #\/) #f)
                      (else (loop (- i 1))))))
         (ext (if dot (ascii-string-down (substring name dot (string-length name))) "")))
    (cond ((member ext '(".txt" ".sql" ".clj" ".cljc" ".cljs" ".edn" ".md" ".csv" ".ss")) "text/plain")
          ((member ext '(".html" ".htm")) "text/html")
          ((string=? ext ".css") "text/css")
          ((string=? ext ".json") "application/json")
          ((string=? ext ".xml") "application/xml")
          ((string=? ext ".png") "image/png")
          ((member ext '(".jpg" ".jpeg")) "image/jpeg")
          ((string=? ext ".gif") "image/gif")
          ((string=? ext ".pdf") "application/pdf")
          ((string=? ext ".zip") "application/zip")
          ((string=? ext ".jar") "application/java-archive")
          (else jolt-nil))))
(define (nio-files-probe-content-type p) (nio-content-type-of (npath-string-of p)))

;; Two paths on different providers: Files.copy and move go through the
;; source's bytes and the target's writes (CopyMoveHelper.copyToForeignTarget),
;; and isSameFile is false.
(define (nio-foreign-pair? a b)
  (and (nio-path? a) (nio-path? b)
       (not (eq? (nio-path-provider a) (nio-path-provider b)))))
(define (nio-copy-foreign src dst opts)
  (if (nio-opts-have? opts copt-sym 'replace-existing)
      (nio-files-call "deleteIfExists" dst)
      (when (nio-files-call "exists" dst)
        (nio-already-exists (nio-shown dst))))
  (if (nio-files-call "isDirectory" src)
      (nio-files-call "createDirectory" dst)
      (nio-files-call "write" dst (na-bv->bytearray (nio-path-bytes src))))
  dst)
(define (nio-files-copy src dst . opts)
  (if (nio-foreign-pair? src dst)
      (nio-copy-foreign src dst opts)
      (apply nio-files-provider-call "copy" src dst opts)))
(define (nio-files-move src dst . opts)
  (if (nio-foreign-pair? src dst)
      (begin (nio-copy-foreign src dst opts)
             (nio-files-call "delete" src)
             dst)
      (apply nio-files-provider-call "move" src dst opts)))
(define (nio-files-same-file a b)
  (if (nio-foreign-pair? a b) #f (nio-files-provider-call "isSameFile" a b)))

(define nio-files-shared
  (list (cons "readString" nio-files-read-string)
        (cons "readAllLines" nio-files-read-all-lines)
        (cons "lines" nio-files-lines)
        (cons "newBufferedReader" nio-files-buffered-reader)
        (cons "newBufferedWriter" nio-files-buffered-writer)
        (cons "writeString" nio-files-write-string)
        (cons "mismatch" nio-files-mismatch)
        (cons "list" nio-files-list)
        (cons "walk" nio-files-walk)
        (cons "find" nio-files-find)
        (cons "probeContentType" nio-files-probe-content-type)
        (cons "copy" nio-files-copy)
        (cons "move" nio-files-move)
        (cons "isSameFile" nio-files-same-file)))

;; ---- Files: dispatch to the path's provider ---------------------------------------
;; Files.x(path, ...) asks the provider of its path argument, as the JDK's
;; provider(path) does; copy(InputStream, Path) has its path second.
(define (nio-args-provider args)
  (cond ((null? args) default-nio-provider)
        ((nio-path? (car args)) (nio-path-provider (car args)))
        ((and (pair? (cdr args)) (nio-path? (cadr args))) (nio-path-provider (cadr args)))
        (else default-nio-provider)))
(define (nio-provider-op provider name)
  (or (hashtable-ref (nio-provider-files provider) name #f)
      (lambda args
        (throw-jvm 'UnsupportedOperationException
                   (string-append "Files." name " is not supported by the "
                                  (nio-provider-name provider) " filesystem provider")))))
(define (nio-files-provider-call name . args)
  (apply (nio-provider-op (nio-args-provider args) name) args))
(define (nio-files-dispatcher name)
  (let ((host-op (nio-provider-op default-nio-provider name)))
    (lambda args
      (let ((provider (nio-args-provider args)))
        (if (eq? provider default-nio-provider)
            (apply host-op args)
            (apply (nio-provider-op provider name) args))))))
;; The Files statics as registered, by name: what the shared operations call.
(define nio-files-table (make-hashtable string-hash string=?))
(define (nio-files-call name . args) (apply (hashtable-ref nio-files-table name #f) args))

;; One-shot Files registration: the accumulated chunks (deduplicated, last wins)
;; are the host provider's Files table; each registered static dispatches on its
;; path's provider, and the shared operations stand over all of them.
(let ((host (nio-provider-files default-nio-provider)))
  (for-each (lambda (p) (hashtable-set! host (car p) (cdr p)))
            (apply append (reverse files-accum-chunks)))
  (vector-for-each (lambda (k) (hashtable-set! nio-files-table k (nio-files-dispatcher k)))
                   (hashtable-keys host))
  (for-each (lambda (p) (hashtable-set! nio-files-table (car p) (cdr p))) nio-files-shared)
  (let-values (((ks vs) (hashtable-entries nio-files-table)))
    (let ((live (map cons (vector->list ks) (vector->list vs))))
      (register-class-statics! "Files" live)
      (register-class-statics! "java.nio.file.Files" live))))
