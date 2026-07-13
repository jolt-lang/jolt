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
    (and (eq? (npath-absolute? a) (npath-absolute? b))
         (let loop ((sa (npath-segs a)) (sb (npath-segs b)))
           (cond ((null? sb) #t)
                 ((null? sa) #f)
                 ((string=? (car sa) (car sb)) (loop (cdr sa) (cdr sb)))
                 (else #f))))))

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
      ((string=? name "toRealPath")    (list (make-nio-path (npath-normalize (if (npath-absolute? s) s (jfile-abs s))))))
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
(define (npath-glob->regex pattern)
  (let ((n (string-length pattern)))
    (let loop ((i 0) (out "^"))
      (if (>= i n)
          (string-append out "$")
          (let ((c (string-ref pattern i)))
            (cond
              ((and (char=? c #\*) (< (+ i 1) n) (char=? (string-ref pattern (+ i 1)) #\*))
               (loop (+ i 2) (string-append out ".*")))
              ((char=? c #\*) (loop (+ i 1) (string-append out "[^/]*")))
              ((char=? c #\?) (loop (+ i 1) (string-append out "[^/]")))
              ((char=? c #\{) (loop (+ i 1) (string-append out "(")))
              ((char=? c #\}) (loop (+ i 1) (string-append out ")")))
              ((char=? c #\,) (loop (+ i 1) (string-append out "|")))
              ((char=? c #\[) (loop (+ i 1) (string-append out "[")))
              ((char=? c #\]) (loop (+ i 1) (string-append out "]")))
              ((char=? c #\\)
               (if (< (+ i 1) n)
                   (loop (+ i 2) (string-append out "\\" (string (string-ref pattern (+ i 1)))))
                   (loop (+ i 1) (string-append out "\\\\"))))
              ((memv c '(#\. #\( #\) #\^ #\$ #\+ #\|))
               (loop (+ i 1) (string-append out "\\" (string c))))
              (else (loop (+ i 1) (string-append out (string c))))))))))

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
(define (nfp x) (project-relative (npath-string-of x)))       ; on-disk path
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
  (cond ((not (file-exists? fp))
         (if missing-ok? #f (jolt-throw (jolt-ex-info (string-append fp) (empty-pmap)))))
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
(define (nio-walk-file-tree start opts max-depth visitor)
  (let ((md (if (number? max-depth) (exact (truncate max-depth)) 2147483647)))
    (call/cc
     (lambda (stop)
       (define (walk path-obj depth)
         (let ((fp (nfp path-obj)))
           (if (file-directory? fp)
               (let ((r (nio-call-visitor visitor "preVisitDirectory" path-obj (make-basic-attrs fp))))
                 (cond
                   ((eq? r 'terminate) (stop #t))
                   ((eq? r 'skip-subtree) 'continue)
                   ((eq? r 'skip-siblings) 'skip-siblings)
                   (else
                    (when (< depth md)
                      (let loop ((names (sort string<? (directory-list fp))))
                        (unless (null? names)
                          (let ((cr (walk (make-nio-path (nio-path-join (nio-path-str path-obj) (car names)))
                                          (+ depth 1))))
                            (unless (eq? cr 'skip-siblings) (loop (cdr names)))))))
                    (let ((pr (nio-call-visitor visitor "postVisitDirectory" path-obj jolt-nil)))
                      (if (eq? pr 'terminate) (stop #t) 'continue)))))
               (let ((r (nio-call-visitor visitor "visitFile" path-obj (make-basic-attrs fp))))
                 (if (eq? r 'terminate) (stop #t) r)))))
       (walk (->path start) 0)))
    (->path start)))

;; ---- newDirectoryStream: a closeable, seqable listing of a directory's kids --
(define (make-dir-stream paths) (make-jhost "dir-stream" paths))  ; state: list of Path
(define (dir-stream? x) (and (jhost? x) (string=? (jhost-tag x) "dir-stream")))
(define (nio-new-directory-stream dir . rest)
  (let* ((base (npath-string-of dir))
         (fp (project-relative base))
         (names (sort string<? (directory-list fp)))
         (glob (and (pair? rest) (string? (car rest)) (car rest)))
         (rx (and glob (jolt-re-pattern (npath-glob->regex glob))))
         (kept (if rx (filter (lambda (nm) (jolt-truthy? (jolt-re-matches rx nm))) names) names)))
    (make-dir-stream (map (lambda (nm) (make-nio-path (nio-path-join base nm))) kept))))

;; dir-stream is seqable (its child Paths) and closeable (a no-op) so
;; (with-open [s (newDirectoryStream d)] (mapv f s)) works.
(let ((prev jolt-seq))
  (set! jolt-seq (lambda (x) (if (dir-stream? x) (list->cseq (jhost-state x)) (prev x)))))
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
(define (nio-get-attribute path attr . _)
  (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
    (cond
      ((string=? nm "lastModifiedTime") (make-file-time (file-mtime-millis fp)))
      ((string=? nm "lastAccessTime")   (make-file-time (file-mtime-millis fp)))
      ((string=? nm "creationTime")     (make-file-time (file-mtime-millis fp)))
      ((string=? nm "size")             (nio-size fp))
      ((string=? nm "isDirectory")      (if (file-directory? fp) #t #f))
      ((string=? nm "isRegularFile")    (if (and (file-exists? fp) (not (file-directory? fp))) #t #f))
      ((string=? nm "isSymbolicLink")   (if (nio-is-symlink? fp) #t #f))
      ((string=? nm "isOther")          #f)
      (else jolt-nil))))
(define (nio-set-attribute path attr value . _)
  (let ((fp (nfp path)) (nm (nio-attr-name (npath-string-of attr))))
    (cond
      ((or (string=? nm "lastModifiedTime") (string=? nm "creationTime") (string=? nm "lastAccessTime"))
       (set-file-mtime-millis! fp (if (file-time? value) (file-time-ms value) (jnum->exact value))))
      (else jolt-nil))
    (->path path)))
(define (nio-read-attributes path what . _)
  (let ((w (npath-string-of what)))
    (if (let loop ((j 0)) (cond ((>= j (string-length w)) #f)   ; a "view:" string form
                                ((char=? (string-ref w j) #\:) #t) (else (loop (+ j 1)))))
        (let ((fp (nfp path)))                                  ; return a small attribute map
          (jolt-hash-map "lastModifiedTime" (make-file-time (file-mtime-millis fp))
                         "size" (nio-size fp)
                         "isDirectory" (if (file-directory? fp) #t #f)
                         "isRegularFile" (if (and (file-exists? fp) (not (file-directory? fp))) #t #f)))
        (make-basic-attrs (nfp path)))))                        ; the class form -> BasicFileAttributes

;; PosixFilePermissions <-> "rwxr-xr-x" strings, and chmod-based set.
(define posix-order '("OWNER_READ" "OWNER_WRITE" "OWNER_EXECUTE"
                      "GROUP_READ" "GROUP_WRITE" "GROUP_EXECUTE"
                      "OTHERS_READ" "OTHERS_WRITE" "OTHERS_EXECUTE"))
(define posix-bits '(#o400 #o200 #o100 #o40 #o20 #o10 #o4 #o2 #o1))
(define (make-pfp name) (make-jhost "posix-perm" name))
(define (pfp? x) (and (jhost? x) (string=? (jhost-tag x) "posix-perm")))
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
    (if (or (>= i (string-length str)) (null? os)) (apply jolt-hash-set (reverse acc))
        (loop (+ i 1) (cdr os)
              (if (memv (string-ref str i) '(#\r #\w #\x #\s #\t)) (cons (make-pfp (car os)) acc) acc)))))
(let ((pfp-statics (list (cons "toString" (lambda (s) (posix-set->str s)))
                         (cons "fromString" (lambda (s) (posix-str->set (npath-string-of s)))))))
  (register-class-statics! "PosixFilePermissions" pfp-statics)
  (register-class-statics! "java.nio.file.attribute.PosixFilePermissions" pfp-statics))
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
                                              (jolt-throw (jolt-ex-info "symlink unavailable" (empty-pmap))))
                                          (->path link)))
             (cons "createLink" (lambda (link existing . _)
                                  (when c-link (c-link (nfp existing) (nfp link))) (->path link)))
             (cons "readSymbolicLink" (lambda (p) (let ((t (nio-readlink (nfp p))))
                                                    (if t (make-nio-path t)
                                                        (jolt-throw (jolt-ex-info (npath-string-of p) (empty-pmap)))))))
             (cons "setPosixFilePermissions" (lambda (p perms . _)
                                               (when c-chmod (c-chmod (nfp p) (posix-set->mode perms))) (->path p))))))
  (register-class-statics! "Files" files-attr)
  (register-class-statics! "java.nio.file.Files" files-attr))
(let ((lo-statics (list (cons "NOFOLLOW_LINKS" fvo-nofollow))))
  (register-class-statics! "LinkOption" lo-statics)
  (register-class-statics! "java.nio.file.LinkOption" lo-statics))
