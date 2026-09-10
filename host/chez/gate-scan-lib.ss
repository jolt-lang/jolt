;; gate-scan-lib.ss — the file-walking bits the source-scanning gates share.
;;
;; (include "host/chez/gate-scan-lib.ss") from a gate script, run from the repo
;; root. Both mirror-drift-check.ss and dead-host-check.ss walk the tree, slurp
;; files that may not be valid UTF-8, and read Scheme sources that may be
;; Gambit's dialect; one copy of that, not two.
;;
;; Nothing here is loaded into a jolt binary — these are build-time gate
;; helpers, so they may use whatever Chez offers.

(define (gs-string-prefix? p s)
  (and (>= (string-length s) (string-length p))
       (string=? p (substring s 0 (string-length p)))))

(define (gs-string-suffix? p s)
  (and (>= (string-length s) (string-length p))
       (string=? p (substring s (- (string-length s) (string-length p)) (string-length s)))))

(define (gs-string-contains? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? sub (substring s i (+ i m))) #t)
            (else (loop (+ i 1)))))))

;; latin-1, not utf-8: these gates walk every file under their roots and a stray
;; non-utf-8 byte in one of them must not abort the run. Byte-for-char is right
;; here — the scans only ever compare ASCII identifier and string text.
(define (gs-slurp path)
  (let ((p (open-file-input-port path (file-options) (buffer-mode block)
                                 (make-transcoder (latin-1-codec)))))
    (let ((o (open-output-string)))
      (let loop ()
        (let ((c (read-char p)))
          (if (eof-object? c)
              (begin (close-port p) (get-output-string o))
              (begin (write-char c o) (loop))))))))

(define (gs-walk-files dir proc)
  (when (file-directory? dir)
    (for-each
      (lambda (e)
        (let ((p (string-append dir "/" e)))
          (cond ((and (file-directory? p) (not (string=? e ".git"))) (gs-walk-files p proc))
                ((file-regular? p) (proc p)))))
      (directory-list dir))))

;; Gambit spells its primitives ##name, which Chez's reader rejects outright.
;; Rewriting the prefix to a plain symbol prefix at TOKEN START (after an open
;; paren, whitespace or a quote) lets a Gambit source read as data here; a #\#
;; character literal is left alone because it is not at a token start.
;;
;; The rewrite is one-way and only ever applied to the Gambit side, so where a
;; gate compares the two hosts its failure mode is a spurious DIVERGED — which
;; an allowlist absorbs — never a spurious match.
(define (gs-normalize-gambit-sharps text)
  (let ((n (string-length text)) (out (open-output-string)))
    (let loop ((i 0) (prev #\space))
      (if (>= i n)
          (get-output-string out)
          (let ((c (string-ref text i)))
            (if (and (char=? c #\#)
                     (< (+ i 1) n)
                     (char=? (string-ref text (+ i 1)) #\#)
                     (memv prev '(#\space #\newline #\tab #\( #\' #\` #\,)))
                (begin (display "gambit-ns:" out) (loop (+ i 2) #\:))
                (begin (write-char c out) (loop (+ i 1) c))))))))

;; Every top-level datum in a Scheme source, Gambit dialect included. Raises if
;; the file cannot be read: to a gate that is a lint HOLE, not something to skip.
(define (gs-read-forms path)
  (let ((p (open-input-string (gs-normalize-gambit-sharps (gs-slurp path)))))
    (let loop ((acc '()))
      (let ((d (read p)))
        (if (eof-object? d) (reverse acc) (loop (cons d acc)))))))

;; name -> the whole (define (name . args) . body) datum, top level only. A
;; (define name value) binding is deliberately not collected: it is a value, and
;; two hosts legitimately bind different tables and parameters under one name.
(define (gs-top-procs path)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each
      (lambda (d)
        (when (and (pair? d) (eq? (car d) 'define)
                   (pair? (cdr d)) (pair? (cadr d)) (symbol? (car (cadr d))))
          (hashtable-set! h (symbol->string (car (cadr d))) d)))
      (gs-read-forms path))
    h))
