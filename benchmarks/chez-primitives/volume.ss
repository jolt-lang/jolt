;; Code volume for the Chez-primitives experiments: how much text each side hands
;; to Chez, where it sits, and what it is made of.
;;
;;   chez --script benchmarks/chez-primitives/volume.ss [--clj DIR] [--scm DIR]
;;                                                      [--cache DIR] [--so DIR]
;;
;;   --clj DIR    Clojure sources (*.clj under DIR)
;;   --scm DIR    Scheme sources (*.sls, *.sps, *.ss under DIR) — for the
;;                hand-written reference this is also what Chez compiles
;;   --cache DIR  a jolt AOT cache (JOLT_CACHE_DIR): the emitted *.scm and the
;;                compiled *.so under it
;;   --so DIR     compiled *.so under DIR (the reference's object directory)
;;
;; For each set of text files: file count, non-whitespace bytes (what
;; `tr -d '[:space:]' | wc -c` counts, comments and jolt's #|L<n>|# line markers
;; included, since Chez reads them too), then the same bytes broken down by top
;; form, then the call heads the experiments track. A top form is a top-level
;; list labelled by its head; a `library` is opened up so its body forms are the
;; top forms, and a `begin` is labelled by its first child's head
;; (begin:image-register-fn-form! is a def carrying an anonymous fn's source).
;; Bytes outside any top form (comments and markers between forms, a library's
;; name/export/import) are "(outside top forms)".
;;
;; The scan is lexical, not a reader: strings, ; and #| |# comments, and character
;; literals (Clojure \x, Scheme #\x) are skipped so their brackets do not count;
;; (, [ and { all nest. A quoted list's first element counts as a head.

(define (ws? b) (or (fx= b 32) (fx= b 10) (fx= b 9) (fx= b 13) (fx= b 11) (fx= b 12)))
(define (open? b) (or (fx= b 40) (fx= b 91) (fx= b 123)))     ; ( [ {
(define (close? b) (or (fx= b 41) (fx= b 93) (fx= b 125)))    ; ) ] }
(define (delim? b) (or (ws? b) (open? b) (close? b) (fx= b 34) (fx= b 59)))

;; The call heads the group context lists, in its order. Each is counted as a
;; list head, except the two `(... (lambda` shapes, which are counted as text.
(define tracked-heads
  '("jolt-site-call" "virtual-register" "set-virtual-register!" "jolt-register-callsite!"
    "keyword" "jolt-symbol" "jolt-vector" "amap-slots->pmap" "image-fn-form-src"
    "image-register-fn-form!" "def-var-with-meta!" "aot-replay-def-ordinals!"
    "var-cell-deref" "var-deref" "jolt-var" "jolt-invoke1" "jolt-invoke2" "jolt-invoke-t1" "jolt-invoke-t2" "jolt-invoke"
    "let*" "let" "lambda" "define"))
(define tracked-text
  '(("(jolt-site-call (lambda" . "site-call wrappers (jolt-site-call (lambda ...))")
    ("(let ((jolt-site-call (lambda" . "local jolt-site-call closures, one per fn")))

(define (read-bytes path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

(define (count-text bv pat)
  (let* ((p (string->utf8 pat)) (pn (bytevector-length p)) (n (bytevector-length bv)))
    (if (eof-object? bv) 0
        (let loop ((i 0) (c 0))
          (if (fx> (fx+ i pn) n)
              c
              (if (let m ((k 0))
                    (or (fx= k pn)
                        (and (fx= (bytevector-u8-ref bv (fx+ i k)) (bytevector-u8-ref p k))
                             (m (fx+ k 1)))))
                  (loop (fx+ i pn) (fx+ c 1))
                  (loop (fx+ i 1) c)))))))

;; Totals for one set of files. scan! returns the file's non-ws bytes inside top
;; forms; the rest of its non-ws bytes are "(outside top forms)".
(define-record-type acc
  (fields (mutable files) (mutable bytes) (mutable nonws)
          forms                         ; label -> (count . nonws)
          heads                         ; head -> count
          (mutable texts)))             ; alist pattern -> count
(define (new-acc)
  (make-acc 0 0 0 (make-hashtable string-hash string=?) (make-hashtable string-hash string=?)
            (map (lambda (t) (cons (car t) 0)) tracked-text)))

;; A qualified head at the top level is a call of some namespace's macro; the
;; generated project has hundreds of distinct ones, so they share one row.
(define (form-label l)
  (cond ((not l) "begin")
        ((and (> (string-length l) 1)
              (memv #\/ (string->list l))
              (not (char=? (string-ref l 0) #\/)))
         "alias/macro (qualified macro call)")
        (else l)))
(define (bump-form! a label nonws)
  (hashtable-update! (acc-forms a) label
                     (lambda (p) (cons (fx+ (car p) 1) (+ (cdr p) nonws))) '(0 . 0)))

(define (scan! a path clj?)
  (let* ((bv (read-bytes path))
         (bv (if (eof-object? bv) (make-bytevector 0) bv))
         (n (bytevector-length bv))
         (heads (acc-heads a)))
    (acc-files-set! a (fx+ (acc-files a) 1))
    (acc-bytes-set! a (+ (acc-bytes a) n))
    (acc-texts-set! a (map (lambda (t) (cons (car t) (+ (cdr t) (count-text bv (car t)))))
                           (acc-texts a)))
    (let ((u8 (lambda (i) (bytevector-u8-ref bv i)))
          (in-form 0))               ; non-ws bytes inside top forms, this file
      ;; read the symbol at i; returns (values string end)
      (define (sym-at i)
        (let loop ((j i))
          (if (and (fx< j n) (not (delim? (u8 j))))
              (loop (fx+ j 1))
              (values (utf8->string (let ((o (make-bytevector (fx- j i))))
                                      (bytevector-copy! bv i o 0 (fx- j i)) o))
                      j))))
      (define (head-at i)                ; first symbol after an open bracket at i-1
        (let skip ((j i))
          (cond ((fx>= j n) #f)
                ((ws? (u8 j)) (skip (fx+ j 1)))
                ((delim? (u8 j)) #f)
                (else (let-values (((s e) (sym-at j))) s)))))
      ;; A unit is a top form: depth-0 list, or a library body form at depth 1.
      ;; unit-depth/unit-start/unit-label describe the open one; nonws counts in it.
      (let loop ((i 0) (depth 0) (lib-depth #f) (lib-child 0)
                 (unit-depth #f) (unit-label #f) (unit-nonws 0) (begin-pending #f))
        (define (nonws+ k) (if unit-depth (fx+ unit-nonws k) unit-nonws))
        (if (fx>= i n)
            (acc-nonws-set! a (+ (acc-nonws a)
                                 (let c ((k 0) (s 0)) (if (fx= k n) s (c (fx+ k 1) (if (ws? (u8 k)) s (fx+ s 1)))))))
            (let ((b (u8 i)))
              (cond
                ((ws? b) (loop (fx+ i 1) depth lib-depth lib-child unit-depth unit-label unit-nonws begin-pending))
                ;; string
                ((fx= b 34)
                 (let s ((j (fx+ i 1)) (k 1))
                   (cond ((fx>= j n) (loop j depth lib-depth lib-child unit-depth unit-label (nonws+ k) begin-pending))
                         ((fx= (u8 j) 92) (s (fx+ j 2) (fx+ k (if (and (fx< (fx+ j 1) n) (ws? (u8 (fx+ j 1)))) 1 2))))
                         ((fx= (u8 j) 34) (loop (fx+ j 1) depth lib-depth lib-child unit-depth unit-label (nonws+ (fx+ k 1)) begin-pending))
                         (else (s (fx+ j 1) (if (ws? (u8 j)) k (fx+ k 1)))))))
                ;; line comment
                ((fx= b 59)
                 (let s ((j i) (k 0))
                   (if (or (fx>= j n) (fx= (u8 j) 10))
                       (loop j depth lib-depth lib-child unit-depth unit-label (nonws+ k) begin-pending)
                       (s (fx+ j 1) (if (ws? (u8 j)) k (fx+ k 1))))))
                ;; block comment #| ... |# (not nested: jolt's markers are flat)
                ((and (fx= b 35) (fx< (fx+ i 1) n) (fx= (u8 (fx+ i 1)) 124))
                 (let s ((j (fx+ i 2)) (k 2))
                   (cond ((fx>= (fx+ j 1) n) (loop n depth lib-depth lib-child unit-depth unit-label (nonws+ k) begin-pending))
                         ((and (fx= (u8 j) 124) (fx= (u8 (fx+ j 1)) 35))
                          (loop (fx+ j 2) depth lib-depth lib-child unit-depth unit-label (nonws+ (fx+ k 2)) begin-pending))
                         (else (s (fx+ j 1) (if (ws? (u8 j)) k (fx+ k 1)))))))
                ;; character literals: Scheme #\x, Clojure \x
                ((and (not clj?) (fx= b 35) (fx< (fx+ i 2) n) (fx= (u8 (fx+ i 1)) 92))
                 (loop (fx+ i 3) depth lib-depth lib-child unit-depth unit-label (nonws+ 3) begin-pending))
                ((and clj? (fx= b 92) (fx< (fx+ i 1) n))
                 (loop (fx+ i 2) depth lib-depth lib-child unit-depth unit-label (nonws+ 2) begin-pending))
                ((open? b)
                 (let* ((h (and (fx= b 40) (head-at (fx+ i 1))))
                        (_ (when h (hashtable-update! heads h fx1+ 0)))
                        (lib-open? (and (fx= depth 0) (equal? h "library")))
                        (opens-unit? (and (not unit-depth)
                                          (if lib-depth
                                              (and (fx= depth (fx+ lib-depth 1)) (fx>= lib-child 3))
                                              (and (fx= depth 0) (not lib-open?)))))
                        (label (if (equal? h "begin") #f (or h "(non-call list)"))))
                   (cond
                     (opens-unit?
                      (loop (fx+ i 1) (fx+ depth 1) lib-depth (if lib-depth (fx+ lib-child 1) lib-child)
                            depth label 1 (equal? h "begin")))
                     ;; first child of a top-level begin names it
                     ((and begin-pending (fx= depth (fx+ unit-depth 1)))
                      (loop (fx+ i 1) (fx+ depth 1) lib-depth lib-child unit-depth
                            (string-append "begin:" (or h "?")) (nonws+ 1) #f))
                     (else
                      (loop (fx+ i 1) (fx+ depth 1) (if lib-open? depth lib-depth)
                            (if (and lib-depth (fx= depth (fx+ lib-depth 1))) (fx+ lib-child 1) lib-child)
                            unit-depth unit-label (nonws+ 1) begin-pending)))))
                ((close? b)
                 (let ((d (fx- depth 1)))
                   (cond
                     ((and unit-depth (fx= d unit-depth))
                      (bump-form! a (form-label unit-label) (fx+ unit-nonws 1))
                      (set! in-form (+ in-form unit-nonws 1))
                      (loop (fx+ i 1) d lib-depth lib-child #f #f 0 #f))
                     ((and lib-depth (fx= d lib-depth))
                      (loop (fx+ i 1) d #f 0 unit-depth unit-label (nonws+ 1) begin-pending))
                     (else (loop (fx+ i 1) (fxmax d 0) lib-depth lib-child unit-depth unit-label (nonws+ 1) begin-pending)))))
                (else
                 ;; an atom: a library's name/export/import children count by position
                 (let-values (((s e) (sym-at i)))
                   (let ((e (if (fx= e i) (fx+ i 1) e)))
                     (loop e depth lib-depth
                           (if (and lib-depth (fx= depth (fx+ lib-depth 1))) (fx+ lib-child 1) lib-child)
                           unit-depth unit-label (nonws+ (fx- e i)) begin-pending))))))))
      in-form)))

(define (files-under dir exts)
  (let walk ((d dir))
    (apply append
      (map (lambda (f)
             (let ((p (string-append d "/" f)))
               (cond ((file-directory? p) (walk p))
                     ((exists (lambda (x) (let ((nx (string-length x)) (nf (string-length f)))
                                            (and (> nf nx) (string=? x (substring f (- nf nx) nf)))))
                              exts)
                      (list p))
                     (else '()))))
           (sort string<? (directory-list d))))))

(define (commas n)
  (let* ((s (number->string n)) (k (string-length s)))
    (let loop ((i k) (out ""))
      (if (<= i 3)
          (string-append (substring s 0 i) out)
          (loop (- i 3) (string-append "," (substring s (- i 3) i) out))))))
(define (mib n) (number->string (/ (round (/ n 104857.6)) 10)))
(define (padl s n) (if (>= (string-length s) n) s (string-append (make-string (- n (string-length s)) #\space) s)))
(define (padr s n) (if (>= (string-length s) n) s (string-append s (make-string (- n (string-length s)) #\space))))
(define (pct a b) (if (zero? b) "-" (string-append (number->string (/ (round (* 1000.0 (/ a b))) 10)) "%")))

(define (report title paths clj?)
  (let ((a (new-acc)) (in-forms 0))
    (for-each (lambda (p) (set! in-forms (+ in-forms (scan! a p clj?)))) paths)
    (let ((total (acc-nonws a)))
      (hashtable-set! (acc-forms a) "(outside top forms)" (cons 0 (- total in-forms)))
      (printf "== ~a: ~a files, ~a bytes, ~a non-whitespace bytes (~a MiB)\n"
              title (acc-files a) (commas (acc-bytes a)) (commas total) (mib total))
      (printf "  by top form                          count     non-ws bytes   share\n")
      (let-values (((ks vs) (hashtable-entries (acc-forms a))))
        (define (line label n b)
          (printf "  ~a ~a ~a ~a\n" (padr label 34) (padl (commas n) 7)
                  (padl (commas b) 16) (padl (pct b total) 7)))
        ;; the 20 biggest, then the rest as one row
        (let loop ((l (sort (lambda (x y) (> (cddr x) (cddr y)))
                            (map cons (vector->list ks) (vector->list vs))))
                   (i 0))
          (cond ((null? l) #t)
                ((< i 20) (line (caar l) (cadar l) (cddar l)) (loop (cdr l) (+ i 1)))
                (else (line (string-append "(" (number->string (length l)) " other labels)")
                            (apply + (map cadr l)) (apply + (map cddr l)))))))
      (printf "  tracked forms                                   count\n")
      (for-each (lambda (t) (printf "  ~a ~a\n" (padr (cdr (assoc (car t) tracked-text)) 46) (padl (commas (cdr t)) 9)))
                (acc-texts a))
      (for-each (lambda (h) (printf "  (~a ...) ~a\n" (padr h 40) (padl (commas (hashtable-ref (acc-heads a) h 0)) 9)))
                tracked-heads)
      (printf "  most frequent list heads\n")
      (let-values (((ks vs) (hashtable-entries (acc-heads a))))
        (let loop ((l (sort (lambda (x y) (> (cdr x) (cdr y))) (map cons (vector->list ks) (vector->list vs))))
                   (i 0))
          (when (and (pair? l) (< i 25))
            (printf "  (~a ...) ~a\n" (padr (caar l) 40) (padl (commas (cdar l)) 9))
            (loop (cdr l) (+ i 1))))))))

(define (so-report title dir)
  (let ((sos (files-under dir '(".so"))))
    (let ((n (apply + (map (lambda (p) (call-with-port (open-file-input-port p) port-length)) sos))))
      (printf "== ~a: ~a .so files, ~a bytes (~a MiB)\n" title (length sos) (commas n) (mib n)))))

(let loop ((args (cdr (command-line))))
  (unless (null? args)
    (when (null? (cdr args)) (error 'volume.ss "missing directory after" (car args)))
    (let ((flag (car args)) (dir (cadr args)))
      (cond
        ((string=? flag "--clj") (report (string-append "clojure source " dir) (files-under dir '(".clj")) #t))
        ((string=? flag "--scm") (report (string-append "scheme source " dir) (files-under dir '(".sls" ".sps" ".ss")) #f))
        ((string=? flag "--cache")
         (report (string-append "emitted scheme " dir) (files-under dir '(".scm")) #f)
         (so-report (string-append "compiled " dir) dir))
        ((string=? flag "--so") (so-report (string-append "compiled " dir) dir))
        (else (error 'volume.ss "unknown flag" flag))))
    (loop (cddr args))))
