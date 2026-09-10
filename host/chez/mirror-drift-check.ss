;; mirror-drift-check.ss — the staleness gate for the HAND-mirrored host files.
;;
;; Most of what the two hosts share is generated and already gated:
;;   host/gambit/records-gambit.ss  <- gen-records.ss  (make gambitgencheck)
;;   host/gambit/seed/              <- gen-seed.ss     (make gambitseedcheck)
;;   host/gambit/boot*.ss           <- gen-boot.ss
;;   host/gambit/hasheq.ss VALUES   <- make gambitcheck pins captured Chez constants
;; and a few files are shared outright (java/regex-translate.ss is ##included
;; into the Gambit boot, so there is only ever one copy to drift).
;;
;; host/chez/rt.ss and host/gambit/rt-core.ss are neither. 56 of the 72
;; procedures defined in both were BYTE-IDENTICAL and kept that way by hand,
;; with nothing checking. The other 16 diverge on purpose — Chez mutexes and
;; hash-fx arithmetic against Gambit's plain operators and no-ops — which is
;; exactly why "they differ" cannot be the alarm on its own.
;;
;; So: a procedure defined in BOTH files must be identical unless the allowlist
;; says it is deliberately per-host. Comparison is on the READ DATUM, not the
;; text, so reformatting and comments are not drift. Like the portability
;; allowlist, this one only ever shrinks truthfully: a line naming a procedure
;; that no longer diverges (or no longer exists in both) is STALE and fails, so
;; folding a per-host arm back into shared code cannot leave a lie behind.
;;
;; Modes:
;;   (default)  gate — exit 1 on unallowlisted drift or a stale allowlist line
;;   --regen    rewrite the allowlist from current reality
;;   --list     print the shared/identical/diverged tallies and the names
;;
;; A file that fails to READ is a hole, not a skip: it is reported by name and
;; the gate exits non-zero.
(import (chezscheme))
(include "host/chez/gate-scan-lib.ss")

(define allowlist-path "host/chez/mirror-drift-allowlist.txt")

;; The mirrored pairs. Add one here when a third hand-kept pair appears; do not
;; add generated files, which their own generator gate already covers.
(define mirror-pairs
  '(("host/chez/rt.ss" . "host/gambit/rt-core.ss")
    ("host/chez/hasheq.ss" . "host/gambit/hasheq.ss")))

;; "chez-file::name" — one allowlist key, so the same procedure name in two
;; different pairs is two separate decisions.
(define (key-for chez-path name) (string-append chez-path "::" name))

(define (diverged-keys)
  (let loop ((ps mirror-pairs) (acc '()) (shared 0) (same 0))
    (if (null? ps)
        (values (list-sort string<? acc) shared same)
        (let* ((a (caar ps)) (b (cdar ps))
               (ha (gs-top-procs a)) (hb (gs-top-procs b)))
          (let-values (((ks vs) (hashtable-entries ha)))
            (let ((n-shared 0) (n-same 0) (found '()))
              (vector-for-each
                (lambda (k v)
                  (let ((other (hashtable-ref hb k #f)))
                    (when other
                      (set! n-shared (+ n-shared 1))
                      (if (equal? v other)
                          (set! n-same (+ n-same 1))
                          (set! found (cons (key-for a k) found))))))
                ks vs)
              (loop (cdr ps) (append found acc)
                    (+ shared n-shared) (+ same n-same))))))))

(define (read-allowlist)
  (if (not (file-exists? allowlist-path))
      '()
      (call-with-input-file allowlist-path
        (lambda (p)
          (let loop ((acc '()))
            (let ((l (get-line p)))
              (cond ((eof-object? l) (reverse acc))
                    ((or (string=? l "") (char=? (string-ref l 0) #\#)) (loop acc))
                    (else (loop (cons l acc))))))))))

(define (write-allowlist! keys)
  (call-with-output-file allowlist-path
    (lambda (p)
      (display "# mirror-drift-allowlist.txt — procedures DELIBERATELY per-host.\n" p)
      (display "#\n" p)
      (display "# One <chez-file>::<procedure> per line: defined in both halves of a\n" p)
      (display "# mirrored pair and different on purpose (Chez mutexes and hash-fx\n" p)
      (display "# arithmetic against Gambit's plain operators, capabilities one host\n" p)
      (display "# does not have). Everything else defined in both must stay identical.\n" p)
      (display "#\n" p)
      (display "# Regenerate with `make mirrordrift-regen`. A line whose procedure no\n" p)
      (display "# longer diverges is STALE and fails the gate — the list only shrinks\n" p)
      (display "# truthfully.\n" p)
      (for-each (lambda (k) (display k p) (newline p)) keys))
    'truncate))

(define (main args)
  (for-each
    (lambda (pr)
      (for-each (lambda (f)
                  (unless (file-exists? f)
                    (printf "mirror drift: MISSING file ~a\n" f)
                    (exit 1)))
                (list (car pr) (cdr pr))))
    mirror-pairs)
  (let-values (((diverged shared same) (diverged-keys)))
    (cond
      ((member "--list" args)
       (printf "mirror drift: ~a procedures defined in both, ~a identical, ~a diverged\n"
               shared same (length diverged))
       (for-each (lambda (k) (printf "  ~a\n" k)) diverged)
       (exit 0))
      ((member "--regen" args)
       (write-allowlist! diverged)
       (printf "mirror drift: wrote ~a with ~a entries\n" allowlist-path (length diverged))
       (exit 0))
      (else
       (let* ((allow (read-allowlist))
              (unallowed (filter (lambda (k) (not (member k allow))) diverged))
              (stale (filter (lambda (k) (not (member k diverged))) allow)))
         (printf "mirror drift: ~a procedures defined in both, ~a identical, ~a diverged\n"
                 shared same (length diverged))
         (when (pair? unallowed)
           (printf "\nDRIFT — defined in both and NOT identical, with no allowlist line:\n")
           (for-each (lambda (k) (printf "  ~a\n" k)) unallowed)
           (printf "\nEither make the two halves agree, or record the per-host split with\n")
           (printf "`make mirrordrift-regen` after checking each one is deliberate.\n"))
         (when (pair? stale)
           (printf "\nSTALE allowlist lines — these no longer diverge (or no longer exist\n")
           (printf "in both files). Drop them with `make mirrordrift-regen`:\n")
           (for-each (lambda (k) (printf "  ~a\n" k)) stale))
         (if (or (pair? unallowed) (pair? stale))
             (exit 1)
             (begin (printf "mirror drift: passed\n") (exit 0))))))))

(main (cdr (command-line)))
