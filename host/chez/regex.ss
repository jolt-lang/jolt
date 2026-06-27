;; regex on Chez via vendored irregex.
;;
;; Chez has no regex at all. We vendor
;; Alex Shinn's irregex (vendor/irregex, BSD) — a portable Scheme regex with
;; PCRE/Java-style STRING patterns — and wrap jolt's re-* surface over it.
;;
;; irregex maps cleanly onto the Clojure fns: irregex-match is an anchored
;; whole-string match (= re-matches), irregex-search finds the first match
;; anywhere (= re-find), irregex-match-substring extracts group N (0 = whole).
;; Results follow Clojure shape: a 0-group match is the whole string; a grouped
;; match is a jolt VECTOR [whole g1 ...] (a non-participating group is nil); a nil
;; result is jolt-nil; re-seq is a jolt seq (nil when there are no matches).
;;
;; The re-* fns are def-var!'d into clojure.core so prelude / -e code resolves
;; them at runtime (they're NOT subset native-ops: irregex's Unicode/property-
;; class semantics keep them out
;; of the subset-parity corpus). Loaded from rt.ss after def-var! is defined.

;; irregex.scm is portable R[457]RS; two small adaptations for Chez's top level:
;; a cond-expand at expression position (Chez's is library-only), and `error`
;; called with a lone string (Chez's error wants who+msg). The wrapper normalizes
;; both without changing behavior for valid patterns.
(define-syntax cond-expand
  (syntax-rules (else)
    ((_ (else e ...)) (begin e ...))
    ((_ (else e ...) c ...) (begin e ...))
    ((_ (req e ...) c ...) (cond-expand c ...))
    ((_) (if #f #f))))
(define %chez-error error)
(define (error . args)
  (if (and (pair? args) (string? (car args)))
      (apply %chez-error #f args)
      (apply %chez-error args)))
(load "vendor/irregex/irregex.scm")

;; irregex rejects a quantifier applied to anything that already contains one —
;; including a GROUP like (a+)* — because sre-repeater? recurses through submatch.
;; Java only rejects a DANGLING double quantifier (a**); it allows a quantifier on
;; a group whose body is quantified. Restrict the check to a bare leading * / + so
;; a** still errors but (a+)* parses (cuerdas's format tokenizer needs this).
(set! sre-repeater?
  (lambda (sre) (and (pair? sre) (memq (car sre) '(* +)) #t)))

;; Unicode property classes \p{...}: irregex's string syntax has no
;; \p{...}, so translate a fixed set of property names
;; to ASCII char classes before compiling. ASCII-only — \p{L} would need
;; UTF-8 high bytes counted as letters, which a Unicode-char Scheme string can't
;; reproduce byte-for-byte; the corpus tests ASCII inputs, where they agree. An
;; unmapped name is left as-is (irregex errors, as before — no new behavior). The
;; ORIGINAL source is kept for printing; only the compiled pattern is translated.
(define (prop-class name)
  (cond
    ;; L/Alpha: ASCII letters + non-ASCII up to just below the UTF-16 surrogate gap
    ;; (D800). This covers essentially every real letter (Latin/Greek/Cyrillic/CJK/…
    ;; live below D800); the supplementary planes above it are rare and a range that
    ;; reaches them makes irregex's char-set construction call integer->char on a
    ;; surrogate and crash. N/Z stay ASCII-only.
    ((or (string=? name "L") (string=? name "Alpha")) "a-zA-Z\\x80-\\x{D7FF}")
    ((string=? name "Lu") "A-Z")
    ((string=? name "Ll") "a-z")
    ((or (string=? name "N") (string=? name "Nd") (string=? name "Digit")) "0-9")
    ((or (string=? name "Z") (string=? name "Zs")) " ")
    ((string=? name "Ps") "([{")
    ((string=? name "Pe") ")\\]}")
    (else #f)))
;; Tracks whether the cursor is inside a [...] char class: a \p{X} there emits the
;; class CONTENT (inlined), standalone it emits a wrapping [X]. Escapes
;; (\[, \]) don't toggle the class. \P (negation) only wraps when standalone.
(define (translate-prop-classes src)
  (let ((len (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f))
      (if (fx>=? i len)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ;; \p{Name} / \P{Name}
              ((and (char=? c #\\) (fx<? (fx+ i 2) len)
                    (let ((p (string-ref src (fx+ i 1)))) (or (char=? p #\p) (char=? p #\P)))
                    (char=? (string-ref src (fx+ i 2)) #\{))
               (let* ((close (let scan ((j (fx+ i 3)))
                               (cond ((fx>=? j len) #f)
                                     ((char=? (string-ref src j) #\}) j)
                                     (else (scan (fx+ j 1))))))
                      (cls (and close (prop-class (substring src (fx+ i 3) close)))))
                 (cond
                   ((not cls) (write-char c out) (loop (fx+ i 1) in-class))
                   (in-class (display cls out) (loop (fx+ close 1) in-class))
                   (else
                    (display "[" out)
                    (when (char=? (string-ref src (fx+ i 1)) #\P) (display "^" out))
                    (display cls out) (display "]" out)
                    (loop (fx+ close 1) in-class)))))
              ;; any other escape: copy the pair verbatim, don't toggle class state
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (write-char c out) (write-char (string-ref src (fx+ i 1)) out)
               (loop (fx+ i 2) in-class))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f))
              (else (write-char c out) (loop (fx+ i 1) in-class))))))))

;; Inside a [...] class, irregex reads a '-' that follows a shorthand class
;; (\w \d \s \W \D \S) as the start of a range and errors ("bad char-set"); Java
;; reads it as a literal hyphen (a shorthand can't be a range endpoint). Escape
;; such a '-' to \- so the class parses. Only a '-' right after a shorthand and
;; not the class terminator is touched; a '-' after a plain char (a real range
;; like [a-z]) is left alone.
(define (escape-class-shorthand-dash src)
  (let ((len (string-length src)) (out (open-output-string)))
    (let loop ((i 0) (in-class #f) (after-shorthand #f))
      (if (fx>=? i len)
          (get-output-string out)
          (let ((c (string-ref src i)))
            (cond
              ;; an escape pair: \w-style shorthand sets after-shorthand inside a class
              ((and (char=? c #\\) (fx<? (fx+ i 1) len))
               (let ((n (string-ref src (fx+ i 1))))
                 (write-char c out) (write-char n out)
                 (loop (fx+ i 2) in-class
                       (and in-class (memv n '(#\w #\d #\s #\W #\D #\S)) #t))))
              ((and (not in-class) (char=? c #\[))
               (write-char c out) (loop (fx+ i 1) #t #f))
              ((and in-class (char=? c #\]))
               (write-char c out) (loop (fx+ i 1) #f #f))
              ;; the case Java reads as a literal hyphen
              ((and in-class after-shorthand (char=? c #\-)
                    (fx<? (fx+ i 1) len) (not (char=? (string-ref src (fx+ i 1)) #\])))
               (write-char #\\ out) (write-char #\- out)
               (loop (fx+ i 1) in-class #f))
              (else (write-char c out) (loop (fx+ i 1) in-class #f))))))))

;; Java/Clojure inline flags: a leading (?imsx…) group sets a flag over the whole
;; pattern. irregex has the same semantics but as constructor OPTIONS, not inline
;; syntax (it rejects (?s)/(?s:…)), so peel any leading flag groups off the source
;; and pass the equivalent option symbols. Scoped groups ((?:…), (?=…), (?<n>…))
;; and groups with a flag irregex can't express are left untouched for irregex.
(define (regex-flag->opt c)
  (cond ((char=? c #\s) 'single-line)        ; DOTALL — . matches newline
        ((char=? c #\i) 'case-insensitive)
        ((char=? c #\m) 'multi-line)          ; ^/$ match at line boundaries
        (else #f)))
(define (regex-parse-flags src)
  (let loop ((s src) (opts '()))
    (if (and (>= (string-length s) 4)
             (char=? (string-ref s 0) #\() (char=? (string-ref s 1) #\?))
        (let scan ((i 2) (fs '()))
          (cond
            ((>= i (string-length s)) (values (reverse opts) s))
            ((char=? (string-ref s i) #\))
             (let ((mapped (map regex-flag->opt fs)))
               (if (and (pair? fs) (for-all (lambda (x) x) mapped))
                   (loop (substring s (+ i 1) (string-length s)) (append opts mapped))
                   (values (reverse opts) s))))      ; unmappable flag — leave as-is
            ((char=? (string-ref s i) #\:) (values (reverse opts) s)) ; scoped group
            (else (scan (+ i 1) (cons (string-ref s i) fs)))))
        (values (reverse opts) s))))

;; A jolt regex value: the source string (for printing / str) + the compiled
;; irregex. regex? recognizes it; the printer renders #"source".
(define-record-type regex-t (fields source irx) (nongenerative jolt-regex-v1))
(define (jolt-regex source)
  (let-values (((opts pat) (regex-parse-flags source)))
    (make-regex-t source
                  (apply irregex (translate-prop-classes (escape-class-shorthand-dash pat)) opts))))
(define (jolt-regex? x) (regex-t? x))
(define (jolt-re-pattern x) (if (regex-t? x) x (jolt-regex x)))

;; An irregex match -> the Clojure result: whole string (no groups) or the
;; [whole g1 ... gn] vector (nil for a non-participating group).
(define (irx-result m)
  (let ((n (irregex-match-num-submatches m)))
    (if (= n 0)
        (irregex-match-substring m 0)
        (let loop ((i n) (acc '()))
          (if (< i 0)
              (apply jolt-vector acc)
              (let ((s (irregex-match-substring m i)))
                (loop (- i 1) (cons (if s s jolt-nil) acc))))))))

(define (jolt-re-matches re s)
  (let ((m (irregex-match (regex-t-irx (jolt-re-pattern re)) s)))
    (if m (irx-result m) jolt-nil)))

;; A stateful matcher (java.util.regex.Matcher): the compiled pattern, the target
;; string, the next search position, and the last successful irregex match. re-find
;; over a matcher steps through non-overlapping matches; re-groups returns the
;; groups of the last one.
(define-record-type matcher-t
  (fields irx str (mutable pos) (mutable last))
  (nongenerative jolt-matcher-v1))
(define (jolt-re-matcher re s)
  (make-matcher-t (regex-t-irx (jolt-re-pattern re)) s 0 #f))
(define (jolt-matcher? x) (matcher-t? x))

;; re-find: stateless over (re s), or stateful over a matcher (advance + remember).
(define jolt-re-find
  (case-lambda
    ((re s)
     (let ((m (irregex-search (regex-t-irx (jolt-re-pattern re)) s)))
       (if m (irx-result m) jolt-nil)))
    ((m)
     (let* ((str (matcher-t-str m))
            (len (string-length str))
            (start (matcher-t-pos m))
            (mm (and (<= start len) (irregex-search (matcher-t-irx m) str start))))
       (if mm
           (let ((ms (irregex-match-start-index mm 0))
                 (e (irregex-match-end-index mm 0)))
             (matcher-t-last-set! m mm)
             ;; advance past this match: to its end, or one past a zero-width match
             ;; (which may sit past the search origin, e.g. a lookahead/boundary).
             (matcher-t-pos-set! m (if (> e ms) e (+ e 1)))
             (irx-result mm))
           (begin (matcher-t-last-set! m #f) jolt-nil))))))

;; re-groups: the groups of the matcher's last successful find. Throws when no
;; match has succeeded, like Clojure's IllegalStateException "No match found".
(define (jolt-re-groups m)
  (let ((last (matcher-t-last m)))
    (if last (irx-result last)
        (jolt-throw (jolt-ex-info "No match found" (jolt-hash-map))))))

;; java.util.regex.Matcher methods over a matcher-t. .matches anchors a full-region
;; match and remembers it for .group; .group n returns submatch n (0 = whole) or
;; nil; .groupCount is the pattern's capturing-group count.
(define (jolt-matcher-matches m)
  (let ((mm (irregex-match (matcher-t-irx m) (matcher-t-str m))))
    (matcher-t-last-set! m mm)
    (if mm #t #f)))
(define (jolt-matcher-group m . n)
  (let ((last (matcher-t-last m)))
    (if last
        (let ((s (irregex-match-substring last (if (pair? n) (->idx (car n)) 0))))
          (if s s jolt-nil))
        (jolt-throw (jolt-ex-info "No match available" (jolt-hash-map))))))
(define (jolt-matcher-group-count m) (irregex-num-submatches (matcher-t-irx m)))

;; All non-overlapping matches, left to right. Advance past each match end (or by
;; one on a zero-width match). nil when there are no matches (Clojure: seq-able as
;; nil, so (if-let [m (re-seq ...)] ...) works).
(define (jolt-re-seq re s)
  (let ((irx (regex-t-irx (jolt-re-pattern re)))
        (len (string-length s)))
    (let loop ((start 0) (acc '()))
      (let ((m (and (<= start len) (irregex-search irx s start))))
        (if m
            (let ((ms (irregex-match-start-index m 0))
                  (e (irregex-match-end-index m 0)))
              ;; to the match end, or one past a zero-width match (relative to its
              ;; own start, which may be past the search origin).
              (loop (if (> e ms) e (+ e 1)) (cons (irx-result m) acc)))
            (list->cseq (reverse acc)))))))

(def-var! "clojure.core" "re-pattern" jolt-re-pattern)
(def-var! "clojure.core" "re-matches" jolt-re-matches)
(def-var! "clojure.core" "re-find" jolt-re-find)
(def-var! "clojure.core" "re-seq" jolt-re-seq)
(def-var! "clojure.core" "re-matcher" jolt-re-matcher)
(def-var! "clojure.core" "re-groups" jolt-re-groups)
(def-var! "clojure.core" "regex?" jolt-regex?)
