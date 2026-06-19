;; java.io.File + host file I/O (jolt-yyud). The seed's clojure.java.io (io.clj)
;; is a Janet-backed shim (janet.*/janet.file) — not reusable here, so this is a
;; Chez-native implementation over Chez's filesystem primitives. A File is a
;; path-backed jfile record: (instance? java.io.File f) is true, str/slurp coerce
;; it to its path, and the File method surface (getName/getPath/exists/
;; isDirectory/isFile/listFiles) dispatches through record-method-dispatch.
;;
;; Mirrors src/jolt/core_io.janet (core-make-file/file?/slurp/spit/flush/dir?/
;; list-dir) and the overlay file-seq (20-coll.clj), which calls __file?/__dir?/
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

;; --- File method surface (record-method-dispatch arm) -----------------------
(define (jfile-method f name args)        ; -> boxed result, or #f to fall through
  (let ((p (jfile-path f)))
    (cond
      ((string=? name "getPath")        (list p))
      ((string=? name "getName")        (list (path-last-segment p)))
      ((string=? name "toString")       (list p))
      ((string=? name "getAbsolutePath")(list p))
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
;; dispatch (emit.janet supported-host-methods) — the Phase-1 shims there assume a
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

(define (jolt-slurp src . opts)
  (cond
    ((jfile? src) (read-file-string (jfile-path src)))
    ((string? src) (read-file-string src))
    (else (error #f "slurp: unsupported source (reader io is jolt-at0a)" src))))

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

;; (type f) -> :jolt/file, matching the seed's tagged-file :jolt/type. Re-def-var!
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

;; --- def-var! the seed-native names the overlay file-seq + str/slurp use ----
(def-var! "clojure.core" "__make-file" jolt-make-file)
(def-var! "clojure.core" "__file?" jolt-file?)
(def-var! "clojure.core" "__dir?" jolt-dir?)
(def-var! "clojure.core" "__list-dir" (lambda (p) (list->cseq (jolt-list-dir p))))
(def-var! "clojure.core" "slurp" jolt-slurp)
(def-var! "clojure.core" "spit" jolt-spit)
(def-var! "clojure.core" "flush" jolt-flush)

;; --- clojure.java.io ns (file/as-file; reader/writer are jolt-at0a) ---------
(def-var! "clojure.java.io" "file" jolt-make-file)
(def-var! "clojure.java.io" "as-file" (lambda (x) (if (jfile? x) x (make-jfile (file-path-of x)))))
