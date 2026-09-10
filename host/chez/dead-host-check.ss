;; dead-host-check.ss — top-level host procedures that nothing calls.
;;
;; Every (define (name . args) . body) at the top level of a handwritten host
;; file is a procedure something is supposed to reach. When the last caller goes
;; away the definition stays, still compiled into every binary, still read by
;; the next person trying to understand the file, and — the expensive part —
;; still copied forward by gen-records.ss into the Gambit half. Four of the
;; procedures this first found were dead in BOTH copies for that reason.
;;
;; A name counts as REFERENCED when either:
;;   - it appears as an identifier token anywhere in the repo outside its own
;;     definition line, or
;;   - some string literal in the repo is a prefix of it, at least
;;     min-stem-length characters long.
;;
;; The second rule is not a nicety. backend_scheme.clj emits calls by building
;; the name: (str "jolt-ffi-varargs-proc" k) reaches jolt-ffi-varargs-proc0
;; through jolt-ffi-varargs-proc3, and a token scan alone calls all four dead.
;; Deleting them would have broken every varargs FFI binding, and the build
;; would still have been green — nothing loads those names until a varargs
;; foreign call runs. Any gate that skips this rule is a loaded gun.
;;
;; Modes:
;;   (default)  gate — exit 1 if any definition is unreferenced
;;   --list     print what it found and why, exit 0
(import (chezscheme))
(include "host/chez/gate-scan-lib.ss")

(define min-stem-length 8)

;; Definitions come from handwritten host files only. Gate runners and one-shot
;; scripts define helpers for their own use and are not the subject here; the
;; generated Gambit halves are not either, since their content is decided by
;; gen-records.ss from the Chez originals.
(define (definition-file? path)
  (and (gs-string-suffix? ".ss" path)
       (or (gs-string-prefix? "host/chez/" path)
           (gs-string-prefix? "host/gambit/" path))
       (not (gs-string-contains? path "/seed/"))
       (not (gs-string-contains? path "/stub/"))
       ;; generated halves: what lives in them is gen-records.ss's / gen-boot.ss's
       ;; decision, taken from the Chez originals. The GENERATORS themselves are
       ;; handwritten and are scanned.
       (not (gs-string-contains? path "records-gambit.ss"))
       (not (gs-string-contains? path "prelude-shims.ss"))
       (not (gs-string-prefix? "host/gambit/boot" path))
       ;; gate runners and one-shot scripts define helpers for their own use
       (not (gs-string-contains? path "-test.ss"))
       (not (gs-string-contains? path "-check.ss"))
       (not (gs-string-contains? path "/run-"))
       (not (gs-string-contains? path "/gate-"))))

;; References come from everywhere: the seed and the generated halves included,
;; because a name can be reachable only from generated code.
(define (reference-file? path)
  (or (gs-string-suffix? ".ss" path) (gs-string-suffix? ".scm" path)
      (gs-string-suffix? ".clj" path) (gs-string-suffix? ".cljc" path)
      (gs-string-suffix? ".jolt" path) (gs-string-suffix? ".sh" path)
      (gs-string-suffix? ".edn" path) (gs-string-suffix? ".txt" path)
      (string=? path "Makefile")))

(define reference-roots '("host" "jolt-core" "stdlib" "test" "bench" "tools" "ci" "bin" "img"))

;; latin-1, not utf-8: this walks every file under the reference roots and a
;; stray non-utf-8 byte in one of them must not abort the gate. Byte-for-char is
;; fine here — the scan only ever compares ASCII identifier and string text.
(define (gs-slurp path)
  (let ((p (open-file-input-port path (file-options)
                                 (buffer-mode block)
                                 (make-transcoder (latin-1-codec)))))
    (let ((o (open-output-string)))
      (let loop ()
        (let ((c (read-char p)))
          (if (eof-object? c)
              (begin (close-port p) (get-output-string o))
              (begin (write-char c o) (loop))))))))

(define (ident-char? c)
  (or (char-alphabetic? c) (char-numeric? c)
      (memv c '(#\! #\? #\* #\< #\> #\= #\/ #\+ #\. #\: #\$ #\% #\^ #\& #\| #\_ #\-))))

;; Every identifier token in TEXT, and every string literal in it. One pass, so
;; the whole repo is walked once rather than once per candidate name.
;;
;; The contents of a string literal are tokenized TOO, not just recorded. The
;; backend emits Scheme as text: (str "(jolt-ffi-string->c " param ")") is the
;; only call site jolt-ffi-string->c has, and a scanner that treats string
;; bodies as opaque calls it dead. The whole-literal record is a separate thing,
;; for the stem case where the name is completed by concatenation.
;; Identifier tokens only — used for string bodies, which cannot themselves
;; contain a nested literal worth tracking.
;; Every top-level "(define (name" head in TEXT. A definition is not a use, and
;; the generated Gambit halves repeat the Chez ones verbatim — without this,
;; gen-records.ss copying a dead procedure forward made the copy look like a
;; caller and the pair excused itself. That is the exact case this gate exists
;; to catch, so the baseline has to count heads everywhere, not just where
;; definitions are collected from.
(define (scan-def-heads! text heads)
  (let ((n (string-length text)))
    (let loop ((i 0))
      (when (< i n)
        (let ((nl (let f ((j i)) (cond ((>= j n) n)
                                       ((char=? (string-ref text j) #\newline) j)
                                       (else (f (+ j 1)))))))
          (when (and (gs-string-prefix? "(define (" (substring text i nl)))
            (let* ((rest (substring text (+ i 9) nl)))
              (let g ((k 0))
                (if (and (< k (string-length rest)) (ident-char? (string-ref rest k)))
                    (g (+ k 1))
                    (when (> k 0)
                      (hashtable-update! heads (substring rest 0 k)
                                         (lambda (v) (+ v 1)) 0))))))
          (loop (+ nl 1)))))))

(define (scan-idents! text tokens . stem-table)
  (let ((n (string-length text)))
    (let loop ((i 0))
      (when (< i n)
        (if (ident-char? (string-ref text i))
            (let tloop ((j i))
              (if (and (< j n) (ident-char? (string-ref text j)))
                  (tloop (+ j 1))
                  (let ((tok (substring text i j)))
                    (hashtable-update! tokens tok (lambda (v) (+ v 1)) 0)
                    (when (pair? stem-table) (hashtable-set! (car stem-table) tok #t))
                    (loop j))))
            (loop (+ i 1)))))))

(define (scan-text! text tokens strings)
  (let ((n (string-length text)))
    (let loop ((i 0))
      (when (< i n)
        (let ((c (string-ref text i)))
          (cond
            ;; a string literal: record it and skip past
            ((char=? c #\")
             (let sloop ((j (+ i 1)) (o (open-output-string)))
               (cond ((>= j n) (loop j))
                     ((char=? (string-ref text j) #\\) (sloop (+ j 2) o))
                     ((char=? (string-ref text j) #\")
                      ;; identifiers inside the literal are references (emitted
                      ;; code is text) AND candidate stems for a built name
                      (scan-idents! (get-output-string o) tokens strings)
                      (loop (+ j 1)))
                     (else (write-char (string-ref text j) o) (sloop (+ j 1) o)))))
            ;; #\x — a character literal, never an identifier
            ((and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\\))
             (loop (+ i 3)))
            ;; #| … |# block comment
            ((and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\|))
             (let bloop ((j (+ i 2)))
               (cond ((>= j n) (loop j))
                     ((and (char=? (string-ref text j) #\|)
                           (< (+ j 1) n) (char=? (string-ref text (+ j 1)) #\#))
                      (loop (+ j 2)))
                     (else (bloop (+ j 1))))))
            ;; ; to end of line. A name that appears ONLY in prose is not a
            ;; reference — without this the gate excused any name its own header
            ;; happened to mention, which is every name it documents.
            ((char=? c #\;)
             (let cloop ((j i))
               (cond ((>= j n) (loop j))
                     ((char=? (string-ref text j) #\newline) (loop (+ j 1)))
                     (else (cloop (+ j 1))))))
            ((ident-char? c)
             (let tloop ((j i))
               (if (and (< j n) (ident-char? (string-ref text j)))
                   (tloop (+ j 1))
                   (let ((tok (substring text i j)))
                     (hashtable-update! tokens tok (lambda (v) (+ v 1)) 0)
                     (loop j)))))
            (else (loop (+ i 1)))))))))

;; The top-level procedure NAMES in one file. Read as data (Gambit dialect
;; included) so a name inside a comment or a string is not taken for a
;; definition. A file that cannot be read is a lint HOLE, not a skip.
(define (definitions-in path)
  (guard (e (#t (printf "dead host: CANNOT READ ~a — lint hole\n" path) (exit 1)))
    (let-values (((keys vals) (hashtable-entries (gs-top-procs path))))
      (vector->list keys))))

(define (main args)
  (let ((tokens (make-hashtable string-hash string=?))
        (strings (make-hashtable string-hash string=?))   ; identifier stems seen inside string literals
        (heads (make-hashtable string-hash string=?))    ; name -> define heads ANYWHERE
        (defs (make-hashtable string-hash string=?)))   ; name -> definition count
    (for-each
      (lambda (root)
        (gs-walk-files root
          (lambda (p)
            (when (reference-file? p)
              (let ((text (gs-slurp p)))
                (scan-text! text tokens strings)
                (scan-def-heads! text heads)))
            (when (definition-file? p)
              (for-each (lambda (n)
                          (hashtable-update! defs n (lambda (v) (+ v 1)) 0))
                        (definitions-in p))))))
      reference-roots)
    (when (file-regular? "Makefile") (scan-text! (gs-slurp "Makefile") tokens strings))
    ;; an identifier seen INSIDE a string literal that is a prefix of NAME
    (let-values (((skeys svals) (hashtable-entries strings)))
      (let ((stems (vector->list skeys)))
        (define (all-digits? s from)
          (let loop ((i from))
            (cond ((>= i (string-length s)) (> (string-length s) from))
                  ((char-numeric? (string-ref s i)) (loop (+ i 1)))
                  (else #f))))
        ;; A name built at emit time is a stem plus an INDEX: (str "…proc" k).
        ;; Requiring the tail to be all digits is what keeps the rule honest —
        ;; "is a prefix of" alone excused j-future? because the tag string
        ;; "j-future" happens to be one, and the gate then under-reported.
        (define (built-by-concat? name)
          (let loop ((ss stems))
            (cond ((null? ss) #f)
                  ((and (>= (string-length (car ss)) min-stem-length)
                        (gs-string-prefix? (car ss) name)
                        (all-digits? name (string-length (car ss))))
                   #t)
                  (else (loop (cdr ss))))))
        (let-values (((keys vals) (hashtable-entries defs)))
          (let ((dead '()) (concat 0))
            (vector-for-each
              (lambda (name ndefs)
                ;; every token occurrence that is not itself a define head
                (when (<= (hashtable-ref tokens name 0)
                          (max ndefs (hashtable-ref heads name 0)))
                  (if (built-by-concat? name)
                      (set! concat (+ concat 1))
                      (set! dead (cons name dead)))))
              keys vals)
            (set! dead (list-sort string<? dead))
            (printf "dead host: ~a top-level procedures scanned, ~a reachable only through a built name\n"
                    (vector-length keys) concat)
            (cond
              ((member "--list" args)
               (for-each (lambda (n) (printf "  ~a\n" n)) dead)
               (exit 0))
              ((pair? dead)
               (printf "\nDEAD — defined and never referenced:\n")
               (for-each (lambda (n) (printf "  ~a\n" n)) dead)
               (printf "\nDelete them, or if one is reached through a name built at emit time,\n")
               (printf "make sure the stem appears as a string literal (>= ~a chars).\n" min-stem-length)
               (exit 1))
              (else (printf "dead host: passed\n") (exit 0)))))))))

(main (cdr (command-line)))
