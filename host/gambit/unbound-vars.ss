;; unbound-vars.ss — every var the Gambit boot interns is bound (jolt-cw2p).
;;
;; Run via `make gambitvars` (detection-gated, from the repo root). The
;; Scheme-level half of this question is unbound-check.ss; this is the jolt
;; half. Emitted code references a var through its CELL — the seed hoists
;; (jolt-var "clojure.core" "chunk-first") into a let* and reads it at call
;; time — so after the boot the var table holds every var the seed and the
;; compiler image reference, defined or not. A cell whose root is the unbound
;; sentinel was referenced and never bound: on Chez the java/ tree binds it, on
;; this boot nothing does, and the first call raises "Attempting to call
;; unbound fn" — which is how chunk-first took `defn` down for two weeks while
;; every gate stayed green.
;;
;; Compared against host/gambit/unbound-vars-allowlist.txt, which names the vars
;; this target cannot bind and no path reaches (the compiler image's build and
;; image entry points, the host IO seams behind reader protocols). The list
;; only shrinks truthfully: a line whose var is bound now is STALE and fails.
;; Chez has exactly two unbound cells after the same load — the unquote markers
;; — so the honest end state of this list is short.
;;
;; Modes, chosen by JOLT_GAMBITVARS (gsi loads every argument as a file, so a
;; flag cannot ride the command line):
;;   unset      gate — exit 1 on an unallowlisted unbound var or a stale line
;;   regen      rewrite the allowlist: comments and still-valid lines kept in
;;              place, stale lines dropped, new names appended
;;   list       print every unbound var and whether it is allowlisted

(##include "boot.ss")

(define allowlist-path "host/gambit/unbound-vars-allowlist.txt")

(define (unbound-var-names)
  (let loop ((cells (vector->list (var-table-cells))) (acc '()))
    (if (null? cells)
        (list-sort string<? acc)
        (let ((c (car cells)))
          (loop (cdr cells)
                (if (jolt-var-unbound? (var-cell-root c))
                    (cons (string-append (var-cell-ns c) "/" (var-cell-name c)) acc)
                    acc))))))

(define (read-lines path)
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (p)
          (let loop ((acc '()))
            (let ((l (read-line p)))
              (if (eof-object? l) (reverse acc) (loop (cons l acc)))))))
      '()))

(define (comment-line? l)
  ;; "#" alone or "# text" — a name may start with ## (Gambit's own primitives)
  (or (string=? l "")
      (and (char=? (string-ref l 0) #\#)
           (or (= (string-length l) 1) (char=? (string-ref l 1) #\space)))))

(define (line-name l)
  (let loop ((i 0))
    (if (or (= i (string-length l)) (memv (string-ref l i) '(#\space #\tab)))
        (substring l 0 i)
        (loop (+ i 1)))))

(define (allowed-names lines)
  (map line-name (filter (lambda (l) (not (comment-line? l))) lines)))

(define header
  '("# unbound-vars-allowlist.txt — vars the Gambit boot references and never binds."
    "#"
    "# One ns/name per line, grouped under a comment that says why nothing on this"
    "# target reaches the var. host/gambit/unbound-vars.ss (make gambitvars) walks"
    "# the booted var table and pins its unbound cells against this list: a var"
    "# missing here fails the gate, and so does a line whose var is bound now —"
    "# the list only shrinks truthfully. `make gambitvars-regen` keeps comments"
    "# and still-valid lines in place, drops stale lines and appends new names at"
    "# the end for classification."
    "#"
    "# A line here is a documented gap: calling the var raises \"Attempting to"
    "# call unbound fn\". A binding in host-vars.ss — real, an answer, or a raise"
    "# naming the absent capability — is always better than a line here."))

(define (write-allowlist! lines unbound)
  (let* ((kept (filter (lambda (l) (or (comment-line? l) (member (line-name l) unbound)))
                       lines))
         (have (allowed-names kept))
         (new (filter (lambda (n) (not (member n have))) unbound)))
    (call-with-output-file allowlist-path
      (lambda (p)
        (for-each (lambda (l) (display l p) (newline p))
                  (if (null? lines) header kept))
        (when (pair? new)
          (display "#\n# NEW since the last regen — classify each under a group above, or bind it.\n" p)
          (for-each (lambda (n) (display n p) (newline p)) new))))
    (values (length kept) (length new))))

(define (main mode)
  (let* ((unbound (unbound-var-names))
         (lines (read-lines allowlist-path))
         (allow (allowed-names lines))
         (unallowed (filter (lambda (n) (not (member n allow))) unbound))
         (stale (filter (lambda (n) (not (member n unbound))) allow)))
    (printf "gambit vars: ~a var cell(s) after boot, ~a unbound\n"
            (vector-length (var-table-cells)) (length unbound))
    (cond
      ((equal? mode "list")
       (for-each (lambda (n) (printf "  ~a~a\n" n (if (member n allow) "  (allowlisted)" "")))
                 unbound)
       (exit 0))
      ((equal? mode "regen")
       (call-with-values (lambda () (write-allowlist! lines unbound))
         (lambda (kept new)
           (printf "gambit vars: wrote ~a (~a line(s) kept, ~a new)\n" allowlist-path kept new)))
       (exit 0))
      (else
       (when (pair? unallowed)
         (printf "\nUNBOUND — referenced by the seed or the compiler image, bound by nothing this\n")
         (printf "boot loads, and not allowlisted:\n")
         (for-each (lambda (n) (printf "  ~a\n" n)) unallowed)
         (printf "\nBind each one in host/gambit/host-vars.ss (real, an answer, or a raise naming\n")
         (printf "the absent capability), or record a genuinely unreachable var with\n")
         (printf "`make gambitvars-regen` and classify it in ~a.\n" allowlist-path))
       (when (pair? stale)
         (printf "\nSTALE allowlist lines — these vars are bound now. Drop them with\n")
         (printf "`make gambitvars-regen`:\n")
         (for-each (lambda (n) (printf "  ~a\n" n)) stale))
       (if (or (pair? unallowed) (pair? stale))
           (exit 1)
           (begin (printf "gambit vars: passed (~a allowlisted)\n" (length allow))
                  (exit 0)))))))

(main (or (getenv "JOLT_GAMBITVARS" #f) ""))
