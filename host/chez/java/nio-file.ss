;; nio-file.ss — java.nio.file shim, part 1: the Path value + Paths / Path / of
;; construction, FileSystems.getPathMatcher glob/regex matching, and the
;; File<->Path bridge. babashka.fs is built entirely on java.nio.file, so this
;; is the substrate it runs on.
;;
;; A Path is a jhost tagged "nio-path" whose state is the path string as given
;; (unix "/" separator on this host). Like java.nio.file, a Path is just a name
;; until an operation touches disk — resolution against the working directory
;; happens in toAbsolutePath / the Files layer, not at construction.
;;
;; Loaded from rt.ss after java/io.ss (needs make-jfile / jfile? / jfile-abs).

;; ---- path string algebra ----------------------------------------------------
(define (npath-absolute? s) (and (> (string-length s) 0) (char=? (string-ref s 0) #\/)))

;; The non-empty "/"-separated segments of a path.
(define (npath-segs s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond
      ((= i (string-length s))
       (reverse (if (> i start) (cons (substring s start i) acc) acc)))
      ((char=? (string-ref s i) #\/)
       (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring s start i) acc) acc)))
      (else (loop (+ i 1) start acc)))))

(define (npath-join-segs abs? segs)
  (let ((body (let loop ((ss segs) (out ""))
                (cond ((null? ss) out)
                      ((string=? out "") (loop (cdr ss) (car ss)))
                      (else (loop (cdr ss) (string-append out "/" (car ss))))))))
    (cond (abs? (string-append "/" body))
          ((string=? body "") "")
          (else body))))

;; java.nio.file normalize: drop ".", resolve ".." against the preceding segment.
(define (npath-normalize s)
  (let ((abs? (npath-absolute? s)))
    (let loop ((ss (npath-segs s)) (stack '()))
      (if (null? ss)
          (let ((segs (reverse stack)))
            (cond ((and (null? segs) abs?) "/")
                  ((null? segs) "")
                  (else (npath-join-segs abs? segs))))
          (let ((seg (car ss)))
            (cond
              ((string=? seg ".") (loop (cdr ss) stack))
              ((string=? seg "..")
               (cond
                 ((and (pair? stack) (not (string=? (car stack) "..")))
                  (loop (cdr ss) (cdr stack)))
                 (abs? (loop (cdr ss) stack))          ; /.. stays at root
                 (else (loop (cdr ss) (cons ".." stack)))))
              (else (loop (cdr ss) (cons seg stack)))))))))

;; Paths.get(first, more...) — join with the separator, no normalization. The
;; varargs `more` arrives as a jolt String[] (from into-array); spread it.
(define (npath-spread-args args)
  (apply append (map (lambda (x) (if (jolt-array? x) (vector->list (jolt-array-vec x)) (list x))) args)))
(define (npath-get first . more)
  (let ((s (fold-left (lambda (acc x)
                        (let ((p (npath-string-of x)))
                          (cond ((string=? p "") acc)
                                ((string=? acc "") p)
                                ((char=? (string-ref acc (- (string-length acc) 1)) #\/)
                                 (string-append acc p))
                                (else (string-append acc "/" p)))))
                      (npath-string-of first) (npath-spread-args more))))
    (make-nio-path s)))

;; Path.resolve(other): an absolute other replaces this; else concatenate.
(define (npath-resolve self other)
  (let ((a (nio-path-str self)) (b (npath-string-of other)))
    (cond ((string=? b "") self)
          ((npath-absolute? b) (make-nio-path b))
          ((string=? a "") (make-nio-path b))
          ((char=? (string-ref a (- (string-length a) 1)) #\/) (make-nio-path (string-append a b)))
          (else (make-nio-path (string-append a "/" b))))))

;; Path.relativize(other): the path from self to other (both same absoluteness).
(define (npath-relativize self other)
  (let* ((sa (npath-segs (nio-path-str self)))
         (sb (npath-segs (npath-string-of other))))
    (let loop ((a sa) (b sb))
      (if (and (pair? a) (pair? b) (string=? (car a) (car b)))
          (loop (cdr a) (cdr b))
          (make-nio-path
           (npath-join-segs #f (append (map (lambda (_) "..") a) b)))))))

(define (npath-parent s)
  (let ((abs? (npath-absolute? s)) (segs (npath-segs s)))
    (cond
      ((null? segs) jolt-nil)
      ((null? (cdr segs)) (if abs? (make-nio-path "/") jolt-nil))
      (else (make-nio-path (npath-join-segs abs? (reverse (cdr (reverse segs)))))))))

(define (npath-file-name s)
  (let ((segs (npath-segs s)))
    (if (null? segs) jolt-nil (make-nio-path (car (reverse segs))))))

;; ---- the Path jhost ---------------------------------------------------------
(define (make-nio-path s) (make-jhost "nio-path" (if (string? s) s (npath-string-of s))))
(define (nio-path? x) (and (jhost? x) (string=? (jhost-tag x) "nio-path")))
(define (nio-path-str p) (jhost-state p))

;; A path string of any value: a Path -> its string, a File -> its path, else str.
(define (npath-string-of x)
  (cond ((nio-path? x) (nio-path-str x))
        ((jfile? x) (jfile-path x))
        (else (jolt-str-render-one x))))

(define (npath-starts-with self other)
  (let ((a (nio-path-str self)) (b (npath-string-of other)))
    ;; the empty path is a single empty component: only another empty path starts with it
    (if (string=? b "") (string=? a "")
        (and (eq? (npath-absolute? a) (npath-absolute? b))
             (let loop ((sa (npath-segs a)) (sb (npath-segs b)))
               (cond ((null? sb) #t)
                     ((null? sa) #f)
                     ((string=? (car sa) (car sb)) (loop (cdr sa) (cdr sb)))
                     (else #f)))))))

(define (npath-ends-with self other)
  (let ((a (nio-path-str self)) (b (npath-string-of other)))
    (if (npath-absolute? b)
        (string=? (npath-normalize a) (npath-normalize b))
        (let loop ((sa (reverse (npath-segs a))) (sb (reverse (npath-segs b))))
          (cond ((null? sb) #t)
                ((null? sa) #f)
                ((string=? (car sa) (car sb)) (loop (cdr sa) (cdr sb)))
                (else #f))))))

(define (nio-path-method self name rest)   ; -> boxed result, or #f to fall through
  (let ((s (nio-path-str self)))
    (cond
      ((string=? name "toString")      (list s))
      ((string=? name "getFileName")   (list (npath-file-name s)))
      ((string=? name "getParent")     (list (npath-parent s)))
      ((string=? name "getName")       (list (let ((segs (npath-segs s)) (i (exact (truncate (car rest)))))
                                               (make-nio-path (list-ref segs i)))))
      ((string=? name "getNameCount")  (list (length (npath-segs s))))
      ((string=? name "getRoot")       (list (if (npath-absolute? s) (make-nio-path "/") jolt-nil)))
      ((string=? name "normalize")     (list (make-nio-path (npath-normalize s))))
      ((string=? name "resolve")       (list (npath-resolve self (car rest))))
      ((string=? name "resolveSibling")(list (let ((par (npath-parent s)))
                                               (npath-resolve (if (jolt-nil? par) (make-nio-path "") par) (car rest)))))
      ((string=? name "relativize")    (list (npath-relativize self (car rest))))
      ((string=? name "toAbsolutePath")(list (make-nio-path (if (npath-absolute? s) s (jfile-abs s)))))
      ((string=? name "toRealPath")    (list (let* ((abs (if (npath-absolute? s) s (jfile-abs s)))
                                                    (fp (project-relative abs))
                                                    (rp (nio-realpath fp)))
                                               (cond (rp (make-nio-path rp))
                                                     ((file-exists? fp) (make-nio-path (npath-normalize abs)))
                                                     (else (jolt-throw (jolt-ex-info abs empty-pmap)))))))  ; missing path throws
      ((string=? name "toFile")        (list (make-jfile s)))
      ((string=? name "toUri")         (list (string-append "file:" (jfile-abs s))))
      ((string=? name "startsWith")    (list (npath-starts-with self (car rest))))
      ((string=? name "endsWith")      (list (npath-ends-with self (car rest))))
      ((string=? name "isAbsolute")    (list (npath-absolute? s)))
      ((string=? name "subpath")       (list (let ((segs (npath-segs s))
                                                   (b (exact (truncate (car rest))))
                                                   (e (exact (truncate (cadr rest)))))
                                               (make-nio-path (npath-join-segs #f (list-head (list-tail segs b) (- e b)))))))
      ((string=? name "compareTo")     (list (let ((o (npath-string-of (car rest))))
                                               (cond ((string<? s o) -1) ((string>? s o) 1) (else 0)))))
      ((string=? name "equals")        (list (and (nio-path? (car rest)) (string=? s (nio-path-str (car rest))))))
      ((string=? name "hashCode")      (list (string-hash s)))
      ((string=? name "iterator")      (list (list->cseq (map make-nio-path (npath-segs s)))))
      (else #f))))

;; ---- glob / regex PathMatcher -----------------------------------------------
;; Translate a "glob:" pattern to a Java-style regex string. ** crosses "/", *
;; and ? stay within a segment, {a,b} alternates, [..] is a char class, \ escapes.
(define (nio-bad-glob msg) (jolt-throw (jolt-ex-info (string-append "invalid glob: " msg) empty-pmap)))
(define (npath-glob->regex pattern)
  (let ((n (string-length pattern)))
    (let loop ((i 0) (out "^") (brace #f) (class #f))   ; brace = inside {}, class = inside []
      (if (>= i n)
          (cond (brace (nio-bad-glob "missing '}'"))
                (class (nio-bad-glob "missing ']'"))
                (else (string-append out "$")))
          (let ((c (string-ref pattern i)))
            (cond
              ((and (char=? c #\*) (< (+ i 1) n) (char=? (string-ref pattern (+ i 1)) #\*))
               (loop (+ i 2) (string-append out ".*") brace class))
              ((char=? c #\*) (loop (+ i 1) (string-append out "[^/]*") brace class))
              ((char=? c #\?) (loop (+ i 1) (string-append out "[^/]") brace class))
              ((char=? c #\{) (if brace (nio-bad-glob "nested '{'") (loop (+ i 1) (string-append out "(") #t class)))
              ((char=? c #\}) (loop (+ i 1) (string-append out ")") #f class))
              ((char=? c #\,) (loop (+ i 1) (string-append out (if brace "|" ",")) brace class))
              ((char=? c #\[) (loop (+ i 1) (string-append out "[") brace #t))
              ((char=? c #\]) (loop (+ i 1) (string-append out "]") brace #f))
              ((char=? c #\\)
               (if (< (+ i 1) n)
                   (loop (+ i 2) (string-append out "\\" (string (string-ref pattern (+ i 1)))) brace class)
                   (nio-bad-glob "no character to escape after '\\'")))
              ((memv c '(#\. #\( #\) #\^ #\$ #\+ #\|))
               (loop (+ i 1) (string-append out "\\" (string c)) brace class))
              (else (loop (+ i 1) (string-append out (string c)) brace class))))))))

;; getPathMatcher("glob:..."|"regex:...") -> a matcher jhost; .matches(path) is
;; a whole-string match of the compiled pattern against the path's string form.
(define (npath-make-matcher syntax-and-pattern)
  (let* ((idx (let loop ((i 0)) (cond ((>= i (string-length syntax-and-pattern)) #f)
                                      ((char=? (string-ref syntax-and-pattern i) #\:) i)
                                      (else (loop (+ i 1))))))
         (syntax (if idx (substring syntax-and-pattern 0 idx) "glob"))
         (pat (if idx (substring syntax-and-pattern (+ idx 1) (string-length syntax-and-pattern)) syntax-and-pattern))
         (rx (jolt-re-pattern (cond ((string=? syntax "glob") (npath-glob->regex pat))
                                    ((string=? syntax "regex") pat)
                                    (else (error #f "unrecognized path-matcher syntax" syntax))))))
    (make-jhost "nio-path-matcher" rx)))

(register-host-methods! "nio-path-matcher"
  (list (cons "matches" (lambda (self p)
                          (and (jolt-truthy? (jolt-re-matches (jhost-state self) (npath-string-of p))) #t)))))

(register-host-methods! "nio-filesystem"
  (list (cons "getPathMatcher" (lambda (self spec) (npath-make-matcher (npath-string-of spec))))
        (cons "getSeparator" (lambda (self) "/"))))

;; ---- construction statics + File bridge -------------------------------------
(let ((paths-statics (list (cons "get" npath-get)))
      (path-statics  (list (cons "of" npath-get)))
      (fs-statics    (list (cons "getDefault" (lambda () (make-jhost "nio-filesystem" #f))))))
  (register-class-statics! "Paths" paths-statics)
  (register-class-statics! "java.nio.file.Paths" paths-statics)
  (register-class-statics! "Path" path-statics)
  (register-class-statics! "java.nio.file.Path" path-statics)
  (register-class-statics! "FileSystems" fs-statics)
  (register-class-statics! "java.nio.file.FileSystems" fs-statics))

;; nio-path method dispatch (priority above the jfile arm's 41).
(register-method-arm! 42
  (lambda (obj method-name rest-args)
    (if (nio-path? obj)
        (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (r (nio-path-method obj method-name rest)))
          (if r (car r) (error #f "no Path method" method-name)))
        'pass)))

;; instance? java.nio.file.Path, (class p), (str p), value equality + hashing.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (symbol-t? type-sym) (nio-path? val))
        (let ((n (symbol-t-name type-sym)))
          (if (or (string=? n "Path") (string=? n "java.nio.file.Path")) #t 'pass))
        'pass)))
(register-class-arm! nio-path? (lambda (_) "java.nio.file.Path"))
(register-str-render! nio-path? (lambda (p) (nio-path-str p)))
(register-eq-arm! (lambda (a b) (and (nio-path? a) (nio-path? b)))
                  (lambda (a b) (string=? (nio-path-str a) (nio-path-str b))))
(register-hash-arm! nio-path? (lambda (p) (string-hash (nio-path-str p))))

;; ---- Files statics ----------------------------------------------------------
;; Each Files op takes Path / File / String args plus trailing varargs (options /
;; attributes) it ignores here; the on-disk path resolves against JOLT_PWD the
;; same way java.io.File does (project-relative), so File and Path see one tree.
(define (nfp x)                                ; on-disk path; "" is the cwd, nil is nothing
  (if (jolt-nil? x) ""
      (let ((s (npath-string-of x))) (project-relative (if (string=? s "") "." s)))))
(define (->path x) (if (nio-path? x) x (make-nio-path (npath-string-of x))))

(define (nio-size fp)
  (if (or (not (file-exists? fp)) (file-directory? fp)) 0
      (let ((port (open-file-input-port fp)))
        (let ((n (file-length port))) (close-port port) n))))

(define (nio-read-bv fp)
  (let ((port (open-file-input-port fp)))
    (let ((bv (get-bytevector-all port)))
      (close-port port)
      (if (eof-object? bv) (make-bytevector 0) bv))))

(define (nio-write-bv! fp bv)
  (let ((port (open-file-output-port fp (file-options no-fail))))
    (put-bytevector port bv) (close-port port)))

;; readAllLines: split content on line terminators, drop a single trailing empty.
(define (nio-read-lines fp)
  (let* ((s (utf8->string (nio-read-bv fp)))
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

(define (nio-write! fp data)
  (cond
    ((jolt-array? data) (nio-write-bv! fp (na-bytearray->bv data)))
    ((string? data) (nio-write-bv! fp (string->utf8 data)))
    (else                                    ; Iterable<CharSequence>: line + separator each
     (let ((body (fold-left (lambda (acc ln) (string-append acc (jolt-str-render-one ln) "\n"))
                            "" (seq->list (jolt-seq data)))))
       (nio-write-bv! fp (string->utf8 body))))))

(define (nio-delete1 fp missing-ok?)
  (cond ((nio-is-symlink? fp) (delete-file fp) #t)   ; the link itself, even if dangling
        ((not (file-exists? fp))
         (if missing-ok? #f (jolt-throw (jolt-ex-info fp empty-pmap))))
        ((file-directory? fp) (delete-directory fp) #t)
        (else (delete-file fp) #t)))

(define nio-temp-counter 0)
(define (nio-tmp-dir) (or (getenv "TMPDIR") "/tmp"))
;; A temp path in `dir` (default the system temp dir), unique across processes
;; via now-millis + a retry counter, like java.nio.file's createTemp*.
(define (nio-temp-path dir prefix suffix)
  (let ((d (let ((d (or dir (nio-tmp-dir))))
             (if (char=? (string-ref d (- (string-length d) 1)) #\/) d (string-append d "/")))))
    (let loop ()
      (set! nio-temp-counter (+ nio-temp-counter 1))
      (let ((full (string-append d (if (string? prefix) prefix "")
                                 (number->string (now-millis)) "-" (number->string nio-temp-counter)
                                 (if (string? suffix) suffix ""))))
        (if (file-exists? (project-relative full)) (loop) full)))))

(let ((files-statics
       (list
        (cons "exists"        (lambda (p . _) (if (file-exists? (nfp p)) #t #f)))
        (cons "notExists"     (lambda (p . _) (if (file-exists? (nfp p)) #f #t)))
        (cons "isDirectory"   (lambda (p . _) (if (file-directory? (nfp p)) #t #f)))
        (cons "isRegularFile" (lambda (p . _) (let ((fp (nfp p))) (if (and (file-exists? fp) (not (file-directory? fp))) #t #f))))
        (cons "isReadable"    (lambda (p . _) (if (file-exists? (nfp p)) #t #f)))
        (cons "isWritable"    (lambda (p . _) (if (file-exists? (nfp p)) #t #f)))
        (cons "isExecutable"  (lambda (p . _) (if (file-exists? (nfp p)) #t #f)))
        (cons "isHidden"      (lambda (p . _) (let ((nm (npath-string-of (npath-file-name (npath-string-of p)))))
                                                (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\.)))))
        (cons "size"          (lambda (p . _) (nio-size (nfp p))))
        (cons "createDirectory"   (lambda (p . _) (mkdir (nfp p)) (->path p)))
        (cons "createDirectories" (lambda (p . _) (mkdirs! (nfp p)) (->path p)))
        (cons "createFile"    (lambda (p . _) (close-port (open-file-output-port (nfp p) (file-options no-fail))) (->path p)))
        (cons "delete"        (lambda (p) (nio-delete1 (nfp p) #f) jolt-nil))
        (cons "deleteIfExists"(lambda (p) (nio-delete1 (nfp p) #t)))
        (cons "move"          (lambda (src dst . _) (let ((d (nfp dst)))
                                                      (when (file-exists? d) (nio-delete1 d #t))
                                                      (rename-file (nfp src) d)) (->path dst)))
        (cons "copy"          (lambda (src dst . _) (nio-write-bv! (nfp dst) (nio-read-bv (nfp src))) (->path dst)))
        (cons "readAllBytes"  (lambda (p) (make-jolt-array (list->vector (bytevector->u8-list (nio-read-bv (nfp p)))) 'byte)))
        (cons "readAllLines"  (lambda (p . _) (nio-read-lines (nfp p))))
        (cons "write"         (lambda (p data . _) (nio-write! (nfp p) data) (->path p)))
        (cons "newInputStream"(lambda (p . _) (make-in-stream (open-file-input-port (nfp p)))))
        (cons "createTempFile"      (lambda args (nio-files-create-temp args #f)))
        (cons "createTempDirectory" (lambda args (nio-files-create-temp args #t))))))
  (register-class-statics! "Files" files-statics)
  (register-class-statics! "java.nio.file.Files" files-statics))

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

;; BasicFileAttributes — the subset the visitors read (times land with the
;; attributes increment).
(define (make-basic-attrs fp) (make-jhost "basic-attrs" fp))
(register-host-methods! "basic-attrs"
  (list (cons "isDirectory"    (lambda (self) (if (file-directory? (jhost-state self)) #t #f)))
        (cons "isRegularFile"  (lambda (self) (let ((fp (jhost-state self)))
                                                (if (and (file-exists? fp) (not (file-directory? fp))) #t #f))))
        (cons "isSymbolicLink" (lambda (self) #f))
        (cons "isOther"        (lambda (self) #f))
        (cons "size"           (lambda (self) (nio-size (jhost-state self))))))

(define (nio-call-visitor visitor name . args)
  (let ((m (and (reified-methods visitor) (hashtable-ref (reified-methods visitor) name #f))))
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
                     (let ((r (nio-call-visitor visitor "preVisitDirectory" path-obj (make-basic-attrs fp))))
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
               (let ((r (nio-call-visitor visitor "visitFile" path-obj (make-basic-attrs fp))))
                 (if (eq? r 'terminate) (stop #t) r)))))
       (walk (->path start) 0 '())))
    (->path start)))

;; ---- newDirectoryStream: a closeable, seqable listing of a directory's kids --
(define (make-dir-stream paths) (make-jhost "dir-stream" paths))  ; state: list of Path
(define (dir-stream? x) (and (jhost? x) (string=? (jhost-tag x) "dir-stream")))
(define (nio-new-directory-stream dir . rest)
  (let* ((base (npath-string-of dir))
         (fp (project-relative base))
         (names (sort string<? (directory-list fp)))
         (arg (and (pair? rest) (car rest)))
         (paths (map (lambda (nm) (make-nio-path (nio-path-join base nm))) names)))
    (make-dir-stream
     (cond
       ;; a glob string filters by file name
       ((string? arg)
        (let ((rx (jolt-re-pattern (npath-glob->regex arg))))
          (filter (lambda (p) (jolt-truthy? (jolt-re-matches rx (npath-string-of (npath-file-name (nio-path-str p)))))) paths)))
       ;; a DirectoryStream$Filter reify filters by its accept method
       ((and arg (reified-methods arg) (hashtable-ref (reified-methods arg) "accept" #f))
        => (lambda (m) (filter (lambda (p) (jolt-truthy? (jolt-invoke m arg p))) paths)))
       (else paths)))))

;; dir-stream is seqable (its child Paths) and closeable (a no-op) so
;; (with-open [s (newDirectoryStream d)] (mapv f s)) works.
(let ((prev jolt-seq))
  (set! jolt-seq (lambda (x)
                   (cond ((dir-stream? x) (list->cseq (jhost-state x)))
                         ((nio-path? x) (let ((segs (npath-segs (nio-path-str x))))
                                          ;; the empty path iterates as one empty component (java parity)
                                          (list->cseq (map make-nio-path (if (null? segs) '("") segs)))))
                         (else (prev x))))))
(let ((prev jolt-close))
  (set! jolt-close (lambda (x) (if (dir-stream? x) jolt-nil (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; register the Files walk/stream ops + the FileVisitResult / FileVisitOption enums.
(let ((files-walk (list (cons "walkFileTree" nio-walk-file-tree)
                        (cons "newDirectoryStream" nio-new-directory-stream))))
  (register-class-statics! "Files" files-walk)
  (register-class-statics! "java.nio.file.Files" files-walk))
(let ((fvr-statics (list (cons "CONTINUE" fvr-continue) (cons "SKIP_SUBTREE" fvr-skip-subtree)
                         (cons "SKIP_SIBLINGS" fvr-skip-siblings) (cons "TERMINATE" fvr-terminate))))
  (register-class-statics! "FileVisitResult" fvr-statics)
  (register-class-statics! "java.nio.file.FileVisitResult" fvr-statics))
(let ((fvo-statics (list (cons "FOLLOW_LINKS" fvo-follow-links))))
  (register-class-statics! "FileVisitOption" fvo-statics)
  (register-class-statics! "java.nio.file.FileVisitOption" fvo-statics))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (symbol-t? type-sym) (fvr? val))
        (let ((n (symbol-t-name type-sym)))
          (if (or (string=? n "FileVisitResult") (string=? n "java.nio.file.FileVisitResult")) #t 'pass))
        'pass)))

;; ---- FileTime + attributes + POSIX permissions + symlinks -------------------
;; FileTime carries epoch milliseconds; this host layer resolves timestamps at
;; millisecond granularity (utimes(2)).
(define (make-file-time ms) (make-jhost "file-time" ms))
(define (file-time? x) (and (jhost? x) (string=? (jhost-tag x) "file-time")))
(define (file-time-ms x) (if (file-time? x) (jhost-state x) 0))
(register-host-methods! "file-time"
  (list (cons "toMillis"   (lambda (self) (jhost-state self)))
        (cons "toInstant"  (lambda (self) (mk-instant (jhost-state self))))
        (cons "compareTo"  (lambda (self o) (let ((a (jhost-state self)) (b (file-time-ms o)))
                                              (cond ((< a b) -1) ((> a b) 1) (else 0)))))
        (cons "equals"     (lambda (self o) (and (file-time? o) (= (jhost-state self) (file-time-ms o)))))
        (cons "hashCode"   (lambda (self) (jhost-state self)))
        (cons "toString"   (lambda (self) (iso-instant-str-nanos (* (jhost-state self) 1000000))))))
(let ((ft-statics (list (cons "fromMillis" (lambda (ms) (make-file-time (jnum->exact ms))))
                        (cons "from" (lambda (inst . _) (make-file-time (inst-ms inst)))))))
  (register-class-statics! "FileTime" ft-statics)
  (register-class-statics! "java.nio.file.attribute.FileTime" ft-statics))

;; A basic-attrs value also answers the time getters as FileTimes.
(register-host-methods! "basic-attrs"
  (list (cons "lastModifiedTime" (lambda (self) (make-file-time (file-mtime-millis (jhost-state self)))))
        (cons "lastAccessTime"   (lambda (self) (make-file-time (file-mtime-millis (jhost-state self)))))
        (cons "creationTime"     (lambda (self) (make-file-time (file-mtime-millis (jhost-state self)))))
        (cons "fileKey"          (lambda (self) jolt-nil))))

;; Files/getAttribute / setAttribute / readAttributes over the "basic:" view.
(define (nio-attr-name a)                      ; strip a "view:" prefix
  (let ((i (let loop ((j 0)) (cond ((>= j (string-length a)) #f)
                                   ((char=? (string-ref a j) #\:) j) (else (loop (+ j 1)))))))
    (if i (substring a (+ i 1) (string-length a)) a)))
(define (nio-attr-value fp nm)
  (cond
    ((string=? nm "lastModifiedTime") (make-file-time (file-mtime-millis fp)))
    ((string=? nm "lastAccessTime")   (make-file-time (file-mtime-millis fp)))
    ((string=? nm "creationTime")     (make-file-time (file-mtime-millis fp)))
    ((string=? nm "size")             (nio-size fp))
    ((string=? nm "isDirectory")      (if (file-directory? fp) #t #f))
    ((string=? nm "isRegularFile")    (if (and (file-exists? fp) (not (file-directory? fp))) #t #f))
    ((string=? nm "isSymbolicLink")   (if (nio-is-symlink? fp) #t #f))
    ((string=? nm "isOther")          #f)
    ((string=? nm "fileKey")          jolt-nil)
    (else jolt-nil)))
(define (nio-get-attribute path attr . _)
  (nio-attr-value (nfp path) (nio-attr-name (npath-string-of attr))))
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
(define (nio-set-attribute path attr value . _)
  (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
    (cond
      ((or (string=? nm "lastModifiedTime") (string=? nm "creationTime") (string=? nm "lastAccessTime"))
       (set-file-mtime-millis! fp (if (file-time? value) (file-time-ms value) (jnum->exact value))))
      (else jolt-nil))
    (->path path)))
(define (nio-str-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))
(define (nio-read-attributes path what . _)
  (let ((w (npath-string-of what)))
    (if (nio-str-suffix? w "Attributes")   ; the Class form -> a BasicFileAttributes value
        (make-basic-attrs (nfp path))
        ;; the string form ("view:a,b" / "*" / "a") -> a map of just those attributes
        (let* ((fp (nfp path))
               (attr-part (nio-attr-name w))
               (names (if (string=? attr-part "*") nio-basic-attr-names (nio-split-commas attr-part))))
          (fold-left (lambda (m nm) (jolt-assoc m nm (nio-attr-value fp nm))) empty-pmap names)))))

;; PosixFilePermissions <-> "rwxr-xr-x" strings, and chmod-based set.
(define posix-order '("OWNER_READ" "OWNER_WRITE" "OWNER_EXECUTE"
                      "GROUP_READ" "GROUP_WRITE" "GROUP_EXECUTE"
                      "OTHERS_READ" "OTHERS_WRITE" "OTHERS_EXECUTE"))
(define posix-bits '(#o400 #o200 #o100 #o40 #o20 #o10 #o4 #o2 #o1))
(define (make-pfp name) (make-jhost "posix-perm" name))
(define (pfp? x) (and (jhost? x) (string=? (jhost-tag x) "posix-perm")))
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
(define (posix-str->mode str)                  ; "rwxr-xr-x" -> mode int
  (let loop ((i 0) (bs posix-bits) (acc 0))
    (if (or (>= i (string-length str)) (null? bs)) acc
        (loop (+ i 1) (cdr bs)
              (if (memv (string-ref str i) '(#\r #\w #\x #\s #\t)) (+ acc (car bs)) acc)))))
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
(register-host-methods! "posix-perm"
  (list (cons "toString" (lambda (self) (jhost-state self)))
        (cons "name"     (lambda (self) (jhost-state self)))))
(register-str-render! pfp? (lambda (p) (jhost-state p)))
(register-eq-arm! (lambda (a b) (and (pfp? a) (pfp? b))) (lambda (a b) (string=? (jhost-state a) (jhost-state b))))
(register-hash-arm! pfp? (lambda (p) (string-hash (jhost-state p))))

;; symlinks + hard links + chmod, via libc (jolt-foreign-proc-safe resolves the
;; already-loaded process symbol; a literal foreign-procedure would be a fasl
;; relocation that aborts the boot where the symbol is absent).
(define c-symlink  (jolt-foreign-proc-safe "symlink"  '(string string) 'int))
(define c-link     (jolt-foreign-proc-safe "link"     '(string string) 'int))
(define c-readlink (jolt-foreign-proc-safe "readlink" '(string u8* unsigned-long) 'long))
(define c-chmod    (jolt-foreign-proc-safe "chmod"    '(string int) 'int))
(define (nio-is-symlink? fp)
  (and c-readlink (> (c-readlink fp (make-bytevector 1 0) 1) 0)))   ; readlink succeeds only on a link
(define (nio-readlink fp)
  (and c-readlink
       (let* ((buf (make-bytevector 4096 0)) (n (c-readlink fp buf 4096)))
         (and (> n 0)
              (let ((bv (make-bytevector n)))
                (do ((i 0 (+ i 1))) ((= i n) (utf8->string bv))
                  (bytevector-u8-set! bv i (bytevector-u8-ref buf i))))))))
(define fvo-nofollow (make-jhost "link-option" 'nofollow-links))
(let ((files-attr
       (list (cons "getAttribute" nio-get-attribute)
             (cons "setAttribute" nio-set-attribute)
             (cons "readAttributes" nio-read-attributes)
             (cons "getLastModifiedTime" (lambda (p . _) (make-file-time (file-mtime-millis (nfp p)))))
             (cons "setLastModifiedTime" (lambda (p t) (set-file-mtime-millis! (nfp p) (file-time-ms t)) (->path p)))
             (cons "isSymbolicLink" (lambda (p . _) (if (nio-is-symlink? (nfp p)) #t #f)))
             (cons "createSymbolicLink" (lambda (link target . _)
                                          (if c-symlink (c-symlink (npath-string-of target) (nfp link))
                                              (jolt-throw (jolt-ex-info "symlink unavailable" empty-pmap)))
                                          (->path link)))
             (cons "createLink" (lambda (link existing . _)
                                  (when c-link (c-link (nfp existing) (nfp link))) (->path link)))
             (cons "readSymbolicLink" (lambda (p) (let ((t (nio-readlink (nfp p))))
                                                    (if t (make-nio-path t)
                                                        (jolt-throw (jolt-ex-info (npath-string-of p) empty-pmap))))))
             (cons "setPosixFilePermissions" (lambda (p perms . _)
                                               (when c-chmod (c-chmod (nfp p) (posix-set->mode perms))) (->path p))))))
  (register-class-statics! "Files" files-attr)
  (register-class-statics! "java.nio.file.Files" files-attr))
(let ((lo-statics (list (cons "NOFOLLOW_LINKS" fvo-nofollow))))
  (register-class-statics! "LinkOption" lo-statics)
  (register-class-statics! "java.nio.file.LinkOption" lo-statics))

;; ---- option enums (CopyOption / OpenOption / PosixFilePermission) -----------
;; The enum values are tokens the Files ops inspect; an op receives them as a
;; trailing CopyOption[] / OpenOption[] (from into-array), which is spread here.
(define (make-copt sym) (make-jhost "copy-option" sym))
(define (copt-sym x) (and (jhost? x) (string=? (jhost-tag x) "copy-option") (jhost-state x)))
(define (make-oopt sym) (make-jhost "open-option" sym))
(define (oopt-sym x) (and (jhost? x) (string=? (jhost-tag x) "open-option") (jhost-state x)))
(define (nio-opts-have? args sym-of want)
  (exists (lambda (x) (eq? (sym-of x) want)) (npath-spread-args args)))

(let ((sco (list (cons "REPLACE_EXISTING" (make-copt 'replace-existing))
                 (cons "COPY_ATTRIBUTES"  (make-copt 'copy-attributes))
                 (cons "ATOMIC_MOVE"      (make-copt 'atomic-move)))))
  (register-class-statics! "StandardCopyOption" sco)
  (register-class-statics! "java.nio.file.StandardCopyOption" sco))
(let ((soo (list (cons "READ"              (make-oopt 'read))
                 (cons "WRITE"             (make-oopt 'write))
                 (cons "APPEND"            (make-oopt 'append))
                 (cons "CREATE"            (make-oopt 'create))
                 (cons "CREATE_NEW"        (make-oopt 'create-new))
                 (cons "TRUNCATE_EXISTING" (make-oopt 'truncate-existing)))))
  (register-class-statics! "StandardOpenOption" soo)
  (register-class-statics! "java.nio.file.StandardOpenOption" soo))
(let ((pfp-perms (map (lambda (nm) (cons nm (make-pfp nm))) posix-order)))
  (register-class-statics! "PosixFilePermission" pfp-perms)
  (register-class-statics! "java.nio.file.attribute.PosixFilePermission" pfp-perms))

;; copy / move honor REPLACE_EXISTING; write / newOutputStream honor APPEND.
(define (nio-append! fp data)
  (let ((port (open-file-output-port fp (file-options no-fail no-truncate append))))
    (put-bytevector port (cond ((jolt-array? data) (na-bytearray->bv data))
                               ((string? data) (string->utf8 data))
                               (else (string->utf8 (fold-left (lambda (a ln) (string-append a (jolt-str-render-one ln) "\n")) "" (seq->list (jolt-seq data)))))))
    (close-port port)))
(let ((files-opt
       (list (cons "copy" (lambda (src dst . opts)
                            (let ((d (nfp dst)))
                              (when (and (file-exists? d)
                                         (not (nio-opts-have? opts copt-sym 'replace-existing)))
                                (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                              (nio-write-bv! d (nio-read-bv (nfp src))) (->path dst))))
             (cons "move" (lambda (src dst . opts)
                            (let ((d (nfp dst)))
                              (when (and (file-exists? d)
                                         (not (nio-opts-have? opts copt-sym 'replace-existing)))
                                (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                              (when (file-exists? d) (nio-delete1 d #t))
                              (rename-file (nfp src) d) (->path dst))))
             (cons "write" (lambda (p data . opts)
                             (if (nio-opts-have? opts oopt-sym 'append)
                                 (nio-append! (nfp p) data) (nio-write! (nfp p) data))
                             (->path p)))
             (cons "newOutputStream" (lambda (p . opts)
                                       (make-out-stream
                                        (open-file-output-port
                                         (nfp p) (if (nio-opts-have? opts oopt-sym 'append)
                                                     (file-options no-fail no-truncate append)
                                                     (file-options no-fail)))))))))
  (register-class-statics! "Files" files-opt)
  (register-class-statics! "java.nio.file.Files" files-opt))

;; ---- stat-backed perms + real path (increment: what the fs suite exercises) --
;; st_mode lives at a platform-specific offset in struct stat; read only that.
(define nio-macos?
  (let ((m (symbol->string (machine-type))))
    (let loop ((i 0))
      (cond ((> (+ i 3) (string-length m)) #f)
            ((string=? (substring m i (+ i 3)) "osx") #t)
            (else (loop (+ i 1)))))))
(define c-stat (jolt-foreign-proc-safe "stat" '(string u8*) 'int))
(define c-realpath (jolt-foreign-proc-safe "realpath" '(string u8*) 'iptr))
(define (nio-stat-mode fp)
  (and c-stat
       (let ((buf (make-bytevector 256 0)))
         (and (= 0 (c-stat fp buf))
              (if nio-macos?
                  (bytevector-u16-ref buf 4 (native-endianness))
                  (bytevector-u32-ref buf 24 (native-endianness)))))))
(define (nio-cstr buf)                          ; buf up to the first NUL, as a string
  (let loop ((i 0))
    (cond ((>= i (bytevector-length buf)) (utf8->string buf))
          ((= 0 (bytevector-u8-ref buf i))
           (let ((bv (make-bytevector i)))
             (do ((j 0 (+ j 1))) ((= j i) (utf8->string bv))
               (bytevector-u8-set! bv j (bytevector-u8-ref buf j)))))
          (else (loop (+ i 1))))))
(define (nio-realpath fp)                        ; resolve symlinks; #f if the path is absent
  (and c-realpath
       (let ((buf (make-bytevector 4096 0)))
         (and (not (= 0 (c-realpath fp buf))) (nio-cstr buf)))))
(define (nio-mode->perm-set mode)
  (let ((low (bitwise-and mode #o777)))
    (make-perm-set
      (let loop ((os posix-order) (bs posix-bits) (acc '()))
        (cond ((null? os) (reverse acc))
              ((> (bitwise-and low (car bs)) 0) (loop (cdr os) (cdr bs) (cons (make-pfp (car os)) acc)))
              (else (loop (cdr os) (cdr bs) acc)))))))
(let ((files-stat
       (list (cons "getPosixFilePermissions"
                   (lambda (p . _) (nio-mode->perm-set (or (nio-stat-mode (nfp p)) #o755))))
             (cons "isSameFile"
                   (lambda (a b) (let ((ra (nio-realpath (nfp a))) (rb (nio-realpath (nfp b))))
                                   (and ra rb (string=? ra rb))))))))
  (register-class-statics! "Files" files-stat)
  (register-class-statics! "java.nio.file.Files" files-stat))
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
                                      (if (and (file-exists? fp) (not (file-directory? fp))) #t #f)))))
        ;; copy(dir, dst) creates an empty directory, like java.nio.file
        (cons "copy" (lambda (src dst . opts)
                       (let ((s (nfp src)) (d (nfp dst)))
                         (when (and (file-exists? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                           (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                         (if (file-directory? s)
                             (unless (file-exists? d) (mkdir d))
                             (nio-write-bv! d (nio-read-bv s)))
                         (->path dst))))
        (cons "getOwner" (lambda (p . _) (make-jhost "user-principal" (or (getenv "USER") ""))))
        (cons "getLastModifiedTime" (lambda (p . _) (make-file-time (file-mtime-millis (nfp p))))))))
  (register-class-statics! "Files" files-nofollow)
  (register-class-statics! "java.nio.file.Files" files-nofollow))
(register-host-methods! "user-principal"
  (list (cons "getName" (lambda (self) (jhost-state self)))
        (cons "toString" (lambda (self) (jhost-state self)))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "user-principal")))
                      (lambda (x) (jhost-state x)))

;; PosixFilePermissions/asFileAttribute -> a FileAttribute the create ops apply
;; by chmod after making the entry.
(define (file-attr? x) (and (jhost? x) (string=? (jhost-tag x) "file-attribute")))
(define (nio-apply-attrs! fp args)
  (for-each (lambda (a) (when (and (file-attr? a) c-chmod) (c-chmod fp (posix-set->mode (jhost-state a)))))
            (npath-spread-args args)))
(register-class-statics! "PosixFilePermissions"
  (list (cons "asFileAttribute" (lambda (perms) (make-jhost "file-attribute" perms)))))
(register-class-statics! "java.nio.file.attribute.PosixFilePermissions"
  (list (cons "asFileAttribute" (lambda (perms) (make-jhost "file-attribute" perms)))))
(let ((files-attr-create
       (list (cons "createDirectory" (lambda (p . attrs) (mkdir (nfp p)) (nio-apply-attrs! (nfp p) attrs) (->path p)))
             (cons "createFile" (lambda (p . attrs)
                                  (close-port (open-file-output-port (nfp p) (file-options no-fail)))
                                  (nio-apply-attrs! (nfp p) attrs) (->path p))))))
  (register-class-statics! "Files" files-attr-create)
  (register-class-statics! "java.nio.file.Files" files-attr-create))

;; java.util.regex.Pattern/quote — escape regex metacharacters in a literal.
(define (nio-pattern-quote s)
  (list->string
   (fold-right (lambda (c acc)
                 (if (memv c '(#\\ #\. #\* #\+ #\? #\( #\) #\[ #\] #\{ #\} #\^ #\$ #\|))
                     (cons #\\ (cons c acc)) (cons c acc)))
               '() (string->list s))))
(register-class-statics! "Pattern" (list (cons "quote" nio-pattern-quote)))
(register-class-statics! "java.util.regex.Pattern" (list (cons "quote" nio-pattern-quote)))

;; copy/move to the same file are no-ops; isSameFile is true for equal paths
;; without touching disk (matches java.nio.file).
(let ((files-samefile
       (list (cons "isSameFile"
                   (lambda (a b) (or (string=? (nfp a) (nfp b))
                                     (let ((ra (nio-realpath (nfp a))) (rb (nio-realpath (nfp b))))
                                       (and ra rb (string=? ra rb) #t)))))
             (cons "copy" (lambda (src dst . opts)
                            (let ((s (nfp src)) (d (nfp dst)))
                              (cond
                                ((string=? s d) (->path dst))          ; same file: no-op
                                ((and (file-exists? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                                 (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                                ((file-directory? s) (unless (file-exists? d) (mkdir d)) (->path dst))
                                (else (nio-write-bv! d (nio-read-bv s))
                                      (when (nio-opts-have? opts copt-sym 'copy-attributes)  ; preserve mtime + perms
                                        (let ((mode (nio-stat-mode s)))
                                          (when (and mode c-chmod) (c-chmod d (bitwise-and mode #o777))))
                                        (set-file-mtime-millis! d (file-mtime-millis s)))
                                      (->path dst))))))
             (cons "move" (lambda (src dst . opts)
                            (let ((s (nfp src)) (d (nfp dst)))
                              (cond
                                ((string=? s d) (->path dst))          ; same file: no-op
                                ((and (file-exists? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                                 (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                                (else (when (file-exists? d) (nio-delete1 d #t))
                                      (rename-file s d) (->path dst)))))))))
  (register-class-statics! "Files" files-samefile)
  (register-class-statics! "java.nio.file.Files" files-samefile))

;; ---- umask-masked create perms, symlink-aware move/copy ---------------------
(define c-umask (jolt-foreign-proc-safe "umask" '(int) 'int))
(define (nio-current-umask) (if c-umask (let ((old (c-umask 0))) (c-umask old) old) 0))
;; chmod a created entry to the requested permission file-attribute, masked by
;; the umask — exactly what java.nio.file's create* do.
(define (nio-apply-attrs-umask! fp args)
  (let ((um (nio-current-umask)))
    (for-each (lambda (a) (when (and (file-attr? a) c-chmod)
                            (c-chmod fp (bitwise-and (posix-set->mode (jhost-state a)) (bitwise-not um)))))
              (npath-spread-args args))))
(define (nio-parent-of fp)
  (let loop ((i (- (string-length fp) 1)))
    (cond ((< i 0) "") ((char=? (string-ref fp i) #\/) (substring fp 0 i)) (else (loop (- i 1))))))
(define (nio-missing-ancestors fp)   ; the not-yet-existing path chain, shallowest first
  (let loop ((p fp) (acc '()))
    (cond ((or (string=? p "") (string=? p "/") (file-exists? p)) acc)
          (else (loop (nio-parent-of p) (cons p acc))))))
;; is the dest present as a link (even broken) or a real file?
(define (nio-dest-present? d) (or (file-exists? d) (nio-is-symlink? d)))
(let ((files-create+move
       (list
        (cons "createDirectory" (lambda (p . attrs) (mkdir (nfp p)) (nio-apply-attrs-umask! (nfp p) attrs) (->path p)))
        (cons "createFile" (lambda (p . attrs)
                             (close-port (open-file-output-port (nfp p) (file-options no-fail)))
                             (nio-apply-attrs-umask! (nfp p) attrs) (->path p)))
        (cons "createDirectories" (lambda (p . attrs)
                                    (let ((missing (nio-missing-ancestors (nfp p))))
                                      (mkdirs! (nfp p))
                                      (for-each (lambda (d) (nio-apply-attrs-umask! d attrs)) missing))
                                    (->path p)))
        (cons "copy" (lambda (src dst . opts)
                       (let ((s (nfp src)) (d (nfp dst)))
                         (cond
                           ((string=? s d) (->path dst))
                           ((and (nio-dest-present? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                            (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                           (else
                            (when (nio-dest-present? d) (nio-delete1 d #t))   ; replace a symlink dest as itself
                            (cond
                              ;; :nofollow-links on a symlink copies the link itself
                              ((and (nio-opts-nofollow? opts) (nio-is-symlink? s))
                               (when c-symlink (c-symlink (or (nio-readlink s) "") d)))
                              ((file-directory? s) (unless (file-exists? d) (mkdir d)))
                              (else (nio-write-bv! d (nio-read-bv s))
                                    (when (nio-opts-have? opts copt-sym 'copy-attributes)
                                      (let ((mode (nio-stat-mode s)))
                                        (when (and mode c-chmod) (c-chmod d (bitwise-and mode #o777))))
                                      (set-file-mtime-millis! d (file-mtime-millis s)))))
                            (->path dst))))))
        (cons "move" (lambda (src dst . opts)
                       (let ((s (nfp src)) (d (nfp dst)))
                         (cond
                           ((string=? s d) (->path dst))
                           ((and (nio-dest-present? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                            (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
                           (else (when (nio-dest-present? d) (nio-delete1 d #t))
                                 (rename-file s d) (->path dst)))))))))
  (register-class-statics! "Files" files-create+move)
  (register-class-statics! "java.nio.file.Files" files-create+move))

;; ---- nofollow timestamps (the link's own mtime, via lstat/lutimes) ----------
(define c-lstat (jolt-foreign-proc-safe "lstat" '(string u8*) 'int))
(define c-lutimes (jolt-foreign-proc-safe "lutimes" '(string u8*) 'int))
(define (nio-lstat-mtime-millis fp)              ; the symlink's own mtime
  (and c-lstat
       (let ((buf (make-bytevector 256 0)))
         (and (= 0 (c-lstat fp buf))
              (* 1000 (if nio-macos? (bytevector-s64-ref buf 48 (native-endianness))
                                     (bytevector-s64-ref buf 88 (native-endianness))))))))
(define (nio-set-lmtime! fp ms opts)             ; nofollow on a link sets the link's own time
  (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp) c-lutimes)
      (let ((tv (make-bytevector 32 0)) (sec (div ms 1000)) (usec (* (mod ms 1000) 1000)))
        (bytevector-s64-set! tv 0 sec (native-endianness)) (bytevector-s64-set! tv 8 usec (native-endianness))
        (bytevector-s64-set! tv 16 sec (native-endianness)) (bytevector-s64-set! tv 24 usec (native-endianness))
        (c-lutimes fp tv))
      (set-file-mtime-millis! fp ms)))
(define (nio-lmtime-millis fp opts)              ; read, honoring NOFOLLOW on a link
  (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp))
      (or (nio-lstat-mtime-millis fp) (file-mtime-millis fp))
      (file-mtime-millis fp)))
(let ((files-nofollow-time
       (list
        (cons "getLastModifiedTime" (lambda (p . opts) (make-file-time (nio-lmtime-millis (nfp p) opts))))
        (cons "setLastModifiedTime" (lambda (p t) (set-file-mtime-millis! (nfp p) (file-time-ms t)) (->path p)))
        (cons "getAttribute" (lambda (path attr . opts)
                               (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
                                 (if (member nm '("lastModifiedTime" "creationTime" "lastAccessTime"))
                                     (make-file-time (nio-lmtime-millis fp opts))
                                     (nio-attr-value fp nm)))))
        (cons "setAttribute" (lambda (path attr value . opts)
                               (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
                                 (when (member nm '("lastModifiedTime" "creationTime" "lastAccessTime"))
                                   (nio-set-lmtime! fp (if (file-time? value) (file-time-ms value) (jnum->exact value)) opts))
                                 (->path path)))))))
  (register-class-statics! "Files" files-nofollow-time)
  (register-class-statics! "java.nio.file.Files" files-nofollow-time))

;; java.nio.channels.FileChannel/open — babashka.fs/touch uses it only to create
;; a file (CREATE + WRITE) inside with-open, so support open+close of a channel.
(let ((fc-statics
       (list (cons "open" (lambda (path . opts)
                            (let ((fp (nfp path)))
                              (when (and (nio-opts-have? opts oopt-sym 'create) (not (file-exists? fp)))
                                (close-port (open-file-output-port fp (file-options no-fail))))
                              (make-jhost "file-channel" fp)))))))
  (register-class-statics! "FileChannel" fc-statics)
  (register-class-statics! "java.nio.channels.FileChannel" fc-statics))
(register-host-methods! "file-channel"
  (list (cons "close" (lambda (self) jolt-nil))
        (cons "size" (lambda (self) (nio-size (jhost-state self))))))
(let ((prev jolt-close))
  (set! jolt-close (lambda (x) (if (and (jhost? x) (string=? (jhost-tag x) "file-channel")) jolt-nil (prev x))))
  (def-var! "clojure.core" "__close" jolt-close))

;; A missing target makes the time setters throw NoSuchFileException, as
;; java.nio.file does — babashka.fs/touch relies on catching it to create the file.
(define (nio-require-exists fp)
  (unless (or (file-exists? fp) (nio-is-symlink? fp))
    (jolt-throw (jolt-host-throwable "java.nio.file.NoSuchFileException" fp))))
(let ((files-throwing-setters
       (list
        (cons "setLastModifiedTime" (lambda (p t) (let ((fp (nfp p))) (nio-require-exists fp)
                                                    (set-file-mtime-millis! fp (file-time-ms t)) (->path p))))
        (cons "setAttribute" (lambda (path attr value . opts)
                               (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
                                 (when (member nm '("lastModifiedTime" "creationTime" "lastAccessTime"))
                                   (nio-require-exists fp)
                                   (nio-set-lmtime! fp (if (file-time? value) (file-time-ms value) (jnum->exact value)) opts))
                                 (->path path)))))))
  (register-class-statics! "Files" files-throwing-setters)
  (register-class-statics! "java.nio.file.Files" files-throwing-setters))

;; isSameFile compares inodes (so hard links are the same file); copy preserves
;; the source permissions by default, like java.nio.file on this host.
(define (nio-stat-ino fp)
  (and c-stat (let ((buf (make-bytevector 256 0)))
                (and (= 0 (c-stat fp buf)) (bytevector-u64-ref buf 8 (native-endianness))))))
(let ((files-final
       (list
        (cons "isSameFile" (lambda (a b)
                             (or (string=? (nfp a) (nfp b))
                                 (let ((ia (nio-stat-ino (nfp a))) (ib (nio-stat-ino (nfp b))))
                                   (and ia ib (= ia ib) #t)))))
        (cons "copy" (lambda (src dst . opts)
                       (let ((s (nfp src)) (d (nfp dst)))
                         (cond
                           ((string=? s d) (->path dst))
                           ((and (nio-dest-present? d) (not (nio-opts-have? opts copt-sym 'replace-existing)))
                            (jolt-throw (jolt-ex-info (string-append d " already exists") empty-pmap)))
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
                               (when (nio-opts-have? opts copt-sym 'copy-attributes)
                                 (set-file-mtime-millis! d (file-mtime-millis s)))))
                            (->path dst))))))) ))
  (register-class-statics! "Files" files-final)
  (register-class-statics! "java.nio.file.Files" files-final))

;; getOwner resolves the real owning user (stat st_uid -> getpwuid -> pw_name),
;; so it distinguishes root-owned paths from user files.
(define c-getpwuid (jolt-foreign-proc-safe "getpwuid" '(unsigned-int) 'iptr))
(define (nio-cstr-at addr)                      ; a NUL-terminated C string at a raw address
  (let loop ((i 0) (acc '()))
    (let ((b (foreign-ref 'unsigned-8 addr i)))
      (if (= b 0) (list->string (map integer->char (reverse acc)))
          (loop (+ i 1) (cons b acc))))))
(define (nio-stat-uid fp)
  (and c-stat (let ((buf (make-bytevector 256 0)))
                (and (= 0 (c-stat fp buf)) (bytevector-u32-ref buf (if nio-macos? 16 28) (native-endianness))))))
(define (nio-uid->name uid)
  (and c-getpwuid (let ((pw (c-getpwuid uid))) (and (not (= 0 pw)) (nio-cstr-at (foreign-ref 'iptr pw 0))))))
(let ((files-owner
       (list (cons "getOwner" (lambda (p . _)
                                (let ((uid (nio-stat-uid (nfp p))))
                                  (make-jhost "user-principal"
                                              (or (and uid (nio-uid->name uid)) (getenv "USER") ""))))))))
  (register-class-statics! "Files" files-owner)
  (register-class-statics! "java.nio.file.Files" files-owner))

;; user-principal values compare and hash by name; getOwner honors NOFOLLOW.
(define (nio-userprin? x) (and (jhost? x) (string=? (jhost-tag x) "user-principal")))
(register-eq-arm! (lambda (a b) (and (nio-userprin? a) (nio-userprin? b)))
                  (lambda (a b) (string=? (jhost-state a) (jhost-state b))))
(register-hash-arm! nio-userprin? (lambda (x) (string-hash (jhost-state x))))
(define (nio-lstat-uid fp)
  (and c-lstat (let ((buf (make-bytevector 256 0)))
                 (and (= 0 (c-lstat fp buf)) (bytevector-u32-ref buf (if nio-macos? 16 28) (native-endianness))))))
(let ((files-owner2
       (list (cons "getOwner" (lambda (p . opts)
                                (let* ((fp (nfp p))
                                       (uid (if (and (nio-opts-nofollow? opts) (nio-is-symlink? fp))
                                                (nio-lstat-uid fp) (nio-stat-uid fp))))
                                  (make-jhost "user-principal"
                                              (or (and uid (nio-uid->name uid)) (getenv "USER") ""))))))))
  (register-class-statics! "Files" files-owner2)
  (register-class-statics! "java.nio.file.Files" files-owner2))
