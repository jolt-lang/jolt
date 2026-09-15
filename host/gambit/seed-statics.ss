;; seed-statics.ss — every Class/member call and (new Class) the seed emits
;; resolves on the Gambit boot.
;;
;; Run via `make gambitstatics` (detection-gated, from the repo root; the
;; driver is seed-statics.sh). The analyzer lowers `Class/member` to
;; (host-static-call "Class" "member" …) or
;; (host-static-ref "Class" "member"), and (Class. …) to (host-new "Class" …) —
;; Scheme-level calls in the seed, resolved at run time against the statics
;; and constructor registries. On Chez host-static-methods.ss and
;; host-static-classes.ss fill those; this target carries the members the SEED
;; reaches (host-statics.ss) and a named raise for the rest. Nothing else says
;; which is which: a remint that reaches a new static degrades silently, and
;; parse-long answered "Long/parseLong is unsupported" for two weeks with every
;; gate green.
;;
;; So: seed-statics.sh greps every class/member the emitted calls name out of
;; the two seed files, this boots and asks the registries. A miss must be a
;; line in host/gambit/seed-statics-allowlist.txt — a member this target degrades on
;; purpose (arrays, the filesystem) — and a line whose member resolves now is
;; STALE and fails; the list only shrinks truthfully.
;;
;; Modes, chosen by JOLT_GAMBITSTATICS (the driver's --regen / --list; gsi
;; loads every argument as a file):
;;   unset      gate — exit 1 on an unallowlisted miss or a stale line
;;   regen      rewrite the allowlist: comments and still-valid lines kept in
;;              place, stale lines dropped, new names appended
;;   list       print every emitted static and constructor and its status

(##include "boot.ss")

(define allowlist-path "host/gambit/seed-statics-allowlist.txt")

(define (read-lines path)
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (p)
          (let loop ((acc '()))
            (let ((l (read-line p)))
              (if (eof-object? l) (reverse acc) (loop (cons l acc)))))))
      '()))

;; The driver's list: "Class/member" per static, "new Class" per constructor,
;; sorted and unique.
(define (seed-references)
  (let ((path (getenv "JOLT_GAMBITSTATICS_REFS" #f)))
    (if (not path)
        (begin (display "gambit statics: run through host/gambit/seed-statics.sh\n") (exit 2))
        (let loop ((ls (read-lines path)) (statics '()) (ctors '()))
          (cond ((null? ls) (values (reverse statics) (reverse ctors)))
                ((and (> (string-length (car ls)) 4) (string=? (substring (car ls) 0 4) "new "))
                 (loop (cdr ls) statics (cons (substring (car ls) 4 (string-length (car ls))) ctors)))
                ((string=? (car ls) "") (loop (cdr ls) statics ctors))
                (else (loop (cdr ls) (cons (car ls) statics) ctors)))))))

(define (split-static name)
  (let loop ((i (- (string-length name) 1)))
    (cond ((< i 0) (values name ""))
          ((char=? (string-ref name i) #\/)
           (values (substring name 0 i) (substring name (+ i 1) (string-length name))))
          (else (loop (- i 1))))))

(define (static-resolves? name)
  (call-with-values (lambda () (split-static name))
    (lambda (cls member)
      (not (eq? (host-static-lookup cls member) host-static-miss)))))
;; a registered constructor, or a deftype whose type name is a var holding
;; its ctor — the two arms host-new takes before raising
(define (ctor-resolves? cls)
  (or (and (hashtable-ref class-ctors-tbl cls #f) #t)
      (let ((cell (var-cell-lookup "clojure.core" cls)))
        (and cell (var-cell-defined? cell) (procedure? (var-cell-root cell)) #t))))

(define (comment-line? l)
  (or (string=? l "")
      (and (char=? (string-ref l 0) #\#)
           (or (= (string-length l) 1) (char=? (string-ref l 1) #\space)))))

;; an entry is the whole line ("new Class" has a space in it), trailing blanks off
(define (line-name l)
  (let loop ((i (string-length l)))
    (if (or (= i 0) (not (memv (string-ref l (- i 1)) '(#\space #\tab))))
        (substring l 0 i)
        (loop (- i 1)))))

(define (allowed-names lines)
  (map line-name (filter (lambda (l) (not (comment-line? l))) lines)))

(define header
  '("# seed-statics-allowlist.txt — Class/member calls and constructors the seed"
    "# emits that the Gambit boot does not resolve."
    "#"
    "# One entry per line — Class/member for a static, new Class for a"
    "# constructor — grouped under a comment that says why this target degrades"
    "# it. host/gambit/seed-statics.ss (make gambitstatics) reads every"
    "# host-static-call / host-static-ref / host-new the seed emits and pins the"
    "# ones the booted registries miss against this list: a miss missing here"
    "# fails the gate, and so does a line whose entry resolves now — the list"
    "# only shrinks truthfully. `make gambitstatics-regen` keeps comments and"
    "# still-valid lines in place, drops stale lines and appends new entries."
    "#"
    "# A line here is a documented gap: the call raises a named"
    "# UnsupportedOperationException. A registration in host-statics.ss is"
    "# always better than a line here."))

(define (write-allowlist! lines missing)
  (let* ((kept (filter (lambda (l) (or (comment-line? l) (member (line-name l) missing)))
                       lines))
         (have (allowed-names kept))
         (new (filter (lambda (n) (not (member n have))) missing)))
    (call-with-output-file allowlist-path
      (lambda (p)
        (for-each (lambda (l) (display l p) (newline p))
                  (if (null? lines) header kept))
        (when (pair? new)
          (display "#\n# NEW since the last regen — classify each under a group above, or register it.\n" p)
          (for-each (lambda (n) (display n p) (newline p)) new))))
    (values (length kept) (length new))))

(define (main mode)
  (call-with-values seed-references
    (lambda (statics ctors)
      (let* ((entries (append (map (lambda (s) (cons s (static-resolves? s))) statics)
                              (map (lambda (c) (cons (string-append "new " c) (ctor-resolves? c))) ctors)))
             (missing (map car (filter (lambda (e) (not (cdr e))) entries)))
             (lines (read-lines allowlist-path))
             (allow (allowed-names lines))
             (unallowed (filter (lambda (n) (not (member n allow))) missing))
             (stale (filter (lambda (n) (not (member n missing))) allow)))
        (printf "gambit statics: the seed emits ~a static(s) and ~a constructor(s); ~a unresolved\n"
                (length statics) (length ctors) (length missing))
        (cond
          ((equal? mode "list")
           (for-each (lambda (e)
                       (printf "  ~a  ~a~a\n" (if (cdr e) "ok     " "MISSING") (car e)
                               (if (member (car e) allow) "  (allowlisted)" "")))
                     entries)
           (exit 0))
          ((equal? mode "regen")
           (call-with-values (lambda () (write-allowlist! lines missing))
             (lambda (kept new)
               (printf "gambit statics: wrote ~a (~a line(s) kept, ~a new)\n" allowlist-path kept new)))
           (exit 0))
          (else
           (when (pair? unallowed)
             (printf "\nUNRESOLVED — emitted by the seed, registered by nothing this boot loads,\n")
             (printf "and not allowlisted:\n")
             (for-each (lambda (n) (printf "  ~a\n" n)) unallowed)
             (printf "\nRegister each one in host/gambit/host-statics.ss, or record a deliberate\n")
             (printf "degradation with `make gambitstatics-regen` and classify it in ~a.\n" allowlist-path))
           (when (pair? stale)
             (printf "\nSTALE allowlist lines — these resolve now. Drop them with\n")
             (printf "`make gambitstatics-regen`:\n")
             (for-each (lambda (n) (printf "  ~a\n" n)) stale))
           (if (or (pair? unallowed) (pair? stale))
               (exit 1)
               (begin (printf "gambit statics: passed (~a allowlisted)\n" (length allow))
                      (exit 0)))))))))

(main (or (getenv "JOLT_GAMBITSTATICS" #f) ""))
