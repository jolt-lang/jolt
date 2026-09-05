;; diagnostic-render.ss — the framed source snippet a diagnostic prints.
;;
;; A report used to be a message and a location, and the reader had to go open
;; the file to see what it was talking about:
;;
;;   Unhandled exception: First argument to def must be a Symbol
;;     at ./src/app.clj:3:1
;;
;; It now shows the code:
;;
;;   error[analyze/invalid-def]: First argument to def must be a Symbol
;;     --> ./src/app.clj:3:1
;;      |
;;    1 | (ns app)
;;    2 |
;;    3 | (def :foo 2)
;;      |      ^^^^ the name must be a symbol
;;
;; No box drawing: the rules and their ┬┼┴ joints cost roughly 250 characters a
;; report and carry no information. This shape says the same thing, and a reader
;; that is a language model — which is most of them, for a compiler — pays for
;; every one of those characters in its context.
;;
;; The caret's NOTE is what keeps it readable without counting columns: the fact
;; is written down rather than encoded in the position of a ^. jank labels its
;; carets for the same reason.
;;
;; No documentation URL row: jolt has no per-error pages yet, and a link to a
;; page that does not exist is worse than no link. It goes in when they do.
;;
;; EVERYTHING here is best-effort. A report is what a user sees when something
;; has ALREADY gone wrong, so every step that could fail — reading the file, the
;; line existing, the column being in range — falls back to printing nothing
;; rather than raising. A renderer that throws while rendering an error turns a
;; diagnosable problem into an opaque one.

;; How many lines of context to show above the offending one. Below it: none —
;; the reader's eye stops at the caret, and trailing context pushes the message
;; off a short terminal.
(define diag-context-lines 2)

(define (diag-repeat-string s n)
  (let loop ((i 0) (acc ""))
    (if (fx>=? i n) acc (loop (fx+ i 1) (string-append acc s)))))

;; Split a source string into a vector of lines, without a trailing empty line
;; for a file ending in a newline.
(define (diag-source-lines src)
  (let ((n (string-length src)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((fx>=? i n)
         (list->vector (reverse (if (fx>? i start) (cons (substring src start i) acc) acc))))
        ((char=? (string-ref src i) #\newline)
         (loop (fx+ i 1) (fx+ i 1) (cons (substring src start i) acc)))
        (else (loop (fx+ i 1) start acc))))))

;; The extent of the token starting at COL (1-based) in LINE, as a character
;; count. Whole-token carets are what make a snippet readable — a single ^ under
;; a long name says "somewhere here" — and the extent is computed with the
;; reader's own delimiter rules rather than by re-entering the reader, which must
;; not happen from inside a report.
;;
;; An OPENING delimiter spans its whole balanced form, so the caret under a
;; collection covers [1 2 3] rather than just the bracket. A closing delimiter, or
;; an unbalanced opener running off the line, falls back to one character. Falls
;; back to 1 and never 0, so the caret is always visible.
(define (diag-token-width line col)
  (let* ((n (string-length line))
         (start (fx- col 1)))
    (define (open? c) (or (char=? c #\() (char=? c #\[) (char=? c #\{)))
    (define (close? c) (or (char=? c #\)) (char=? c #\]) (char=? c #\})))
    (define (delim? c) (or (open? c) (close? c)
                           (char=? c #\') (char=? c #\`) (char=? c #\,)))
    (cond
      ((or (fx<? start 0) (fx>=? start n)) 1)
      ;; a string literal spans to its closing quote, escapes included
      ((char=? (string-ref line start) #\")
       (let loop ((j (fx+ start 1)))
         (cond ((fx>=? j n) 1)
               ((char=? (string-ref line j) #\\) (loop (fx+ j 2)))
               ((char=? (string-ref line j) #\") (fx- (fx+ j 1) start))
               (else (loop (fx+ j 1))))))
      ((open? (string-ref line start))
       (let loop ((j (fx+ start 1)) (depth 1))
         (cond ((fx>=? j n) 1)
               ((open? (string-ref line j)) (loop (fx+ j 1) (fx+ depth 1)))
               ((close? (string-ref line j))
                (if (fx=? depth 1) (fx- (fx+ j 1) start) (loop (fx+ j 1) (fx- depth 1))))
               (else (loop (fx+ j 1) depth)))))
      ((delim? (string-ref line start)) 1)
      (else
       (let loop ((i start))
         (cond
           ((fx>=? i n) (fxmax 1 (fx- i start)))
           ((let ((c (string-ref line i)))
              (or (char-whitespace? c) (delim? c) (char=? c #\")))
            (fxmax 1 (fx- i start)))
           (else (loop (fx+ i 1)))))))))

;; The column of element N (1-based) of the form opening at COL in LINE, or COL
;; when the line does not hold that many. Deliberately simple: it walks the one
;; line, skipping balanced sub-forms and strings so (def (f x) y) counts (f x) as
;; one element. A form spanning several lines falls back to the form's own
;; position rather than guessing.
(define (diag-element-col line col n)
  (let* ((len (string-length line))
         (start (fx- col 1)))
    (define (open? c) (or (char=? c #\() (char=? c #\[) (char=? c #\{)))
    (define (close? c) (or (char=? c #\)) (char=? c #\]) (char=? c #\})))
    ;; index just past the element beginning at I, or #f if it runs off the line
    (define (skip-element i)
      (cond
        ((fx>=? i len) #f)
        ((char=? (string-ref line i) #\")
         (let loop ((j (fx+ i 1)))
           (cond ((fx>=? j len) #f)
                 ((char=? (string-ref line j) #\\) (loop (fx+ j 2)))
                 ((char=? (string-ref line j) #\") (fx+ j 1))
                 (else (loop (fx+ j 1))))))
        ((open? (string-ref line i))
         (let loop ((j (fx+ i 1)) (depth 1))
           (cond ((fx>=? j len) #f)
                 ((open? (string-ref line j)) (loop (fx+ j 1) (fx+ depth 1)))
                 ((close? (string-ref line j))
                  (if (fx=? depth 1) (fx+ j 1) (loop (fx+ j 1) (fx- depth 1))))
                 (else (loop (fx+ j 1) depth)))))
        (else
         (let loop ((j i))
           (cond ((fx>=? j len) j)
                 ((let ((c (string-ref line j)))
                    (or (char-whitespace? c) (open? c) (close? c)))
                  j)
                 (else (loop (fx+ j 1))))))))
    (define (skip-space i)
      (let loop ((j i))
        (if (and (fx<? j len) (char-whitespace? (string-ref line j))) (loop (fx+ j 1)) j)))
    (if (or (fx<? start 0) (fx>=? start len) (not (open? (string-ref line start))))
        col
        (let loop ((i (skip-space (fx+ start 1))) (k 0))
          (cond
            ((fx>=? i len) col)
            ((close? (string-ref line i)) col)
            ((fx=? k n) (fx+ i 1))
            (else (let ((e (skip-element i)))
                    (if e (loop (skip-space e) (fx+ k 1)) col))))))))

;; Right-align a line number in a fixed gutter.
(define (diag-gutter s width)
  (let ((pad (fx- width (string-length s))))
    (string-append (diag-repeat-string " " (fxmax 0 pad)) s)))

;; Render the snippet for FILE at LINE/COL to PORT, or nothing at all if anything
;; is missing or unreadable.
;;
;;   error[analyze/invalid-def]: First argument to def must be a Symbol
;;     --> ./src/app.clj:3:1
;;      |
;;    1 | (ns app)
;;    2 |
;;    3 | (def :foo 2)
;;      |      ^^^^ the name must be a symbol
;;
;; NOTE, when the diagnostic carries one, is what makes the caret readable
;; without counting columns: the fact is written down rather than encoded in the
;; position of a ^. That serves a person and a model equally, and it is what jank
;; does — its carets are labelled, not bare.
(define (diag-render-snippet port file line col loc arg note)
  (guard (e (#t (void)))
    (let* ((src (read-file-string file))
           (lines (diag-source-lines src))
           (count (vector-length lines)))
      (when (and (fx>=? line 1) (fx<=? line count))
        (let* ((first-line (fxmax 1 (fx- line diag-context-lines)))
               (gutter (string-length (number->string line)))
               (pad (diag-repeat-string " " gutter))
               (text (vector-ref lines (fx- line 1)))
               (c0 (if (and (integer? col) (fx>=? col 1)) col 1))
               (c (if (and (integer? arg) (fx>? arg 0))
                      (diag-element-col text c0 arg)
                      c0)))
          (display " " port) (display pad port) (display "--> " port)
          (display (if (string? loc) loc file) port) (newline port)
          (display " " port) (display pad port) (display " |" port) (newline port)
          (let loop ((i first-line))
            (when (fx<=? i line)
              (display " " port)
              (display (diag-gutter (number->string i) gutter) port)
              (display " | " port)
              (display (vector-ref lines (fx- i 1)) port)
              (newline port)
              (loop (fx+ i 1))))
          (when (fx<=? c (fx+ (string-length text) 1))
            (display " " port) (display pad port) (display " | " port)
            (display (diag-repeat-string " " (fx- c 1)) port)
            (display (diag-repeat-string "^" (diag-token-width text c)) port)
            (when (and note (string? note) (fx>? (string-length note) 0))
              (display " " port) (display note port))
            (newline port)))))))

;; The token under the caret, for JOLT_DIAG=edn: the same text the caret spans,
;; so a tool never has to do the column arithmetic the caret encodes. Extracted
;; from the SOURCE LINE rather than by printing the offending value, which could
;; be an enormous collection or an infinite seq.
(define (diag-token-at file line col arg)
  (guard (e (#t #f))
    (let* ((lines (diag-source-lines (read-file-string file)))
           (n (vector-length lines)))
      (and (fx>=? line 1) (fx<=? line n)
           (let* ((text (vector-ref lines (fx- line 1)))
                  (c0 (if (and (integer? col) (fx>=? col 1)) col 1))
                  (c (if (and (integer? arg) (fx>? arg 0))
                         (diag-element-col text c0 arg)
                         c0))
                  (w (diag-token-width text c)))
             (and (fx<=? (fx+ (fx- c 1) w) (string-length text))
                  (substring text (fx- c 1) (fx+ (fx- c 1) w))))))))

;; The snippet lines themselves, for JOLT_DIAG=edn, so a tool never needs a
;; second read of the file to see the code the report is about.
(define (diag-source-window file line)
  (guard (e (#t #f))
    (let* ((lines (diag-source-lines (read-file-string file)))
           (n (vector-length lines)))
      (and (fx>=? line 1) (fx<=? line n)
           (let loop ((i (fxmax 1 (fx- line diag-context-lines))) (acc '()))
             (if (fx>? i line)
                 (reverse acc)
                 (loop (fx+ i 1) (cons (vector-ref lines (fx- i 1)) acc))))))))
