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
(define (nio-temp-name prefix suffix)
  (set! nio-temp-counter (+ nio-temp-counter 1))
  (string-append (let ((d (nio-tmp-dir)))
                   (if (char=? (string-ref d (- (string-length d) 1)) #\/) d (string-append d "/")))
                 (if (string? prefix) prefix "")
                 (number->string (+ (* 100000 nio-temp-counter) (modulo (* nio-temp-counter 2654435761) 100000)))
                 (if (string? suffix) suffix "")))

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
         (name (nio-temp-name prefix suffix))
         (full (if base (string-append base "/" (npath-string-of (npath-file-name name))) name))
         (fp (project-relative full)))
    (if dir? (mkdir fp) (close-port (open-file-output-port fp (file-options no-fail))))
    (make-nio-path full)))
