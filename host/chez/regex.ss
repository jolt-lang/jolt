;; Phase 1 (jolt-cf1q.2) — regex on Chez via vendored irregex (jolt-i0s3).
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

;; Unicode property classes \p{...} (jolt-y1zq): irregex's string syntax has no
;; \p{...}, so translate a fixed set of property names
;; to ASCII char classes before compiling. ASCII-only — \p{L} would need
;; UTF-8 high bytes counted as letters, which a Unicode-char Scheme string can't
;; reproduce byte-for-byte; the corpus tests ASCII inputs, where they agree. An
;; unmapped name is left as-is (irregex errors, as before — no new behavior). The
;; ORIGINAL source is kept for printing; only the compiled pattern is translated.
(define (prop-class name)
  (cond
    ;; L/Alpha: ASCII letters + any non-ASCII codepoint (UTF-8 high
    ;; bytes count as letters, so ^\p{L}+$ accepts accented words). N/Z stay ASCII-only.
    ((or (string=? name "L") (string=? name "Alpha")) "a-zA-Z\\x80-\\x{10FFFF}")
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

;; A jolt regex value: the source string (for printing / str) + the compiled
;; irregex. regex? recognizes it; the printer renders #"source".
(define-record-type regex-t (fields source irx) (nongenerative jolt-regex-v1))
(define (jolt-regex source) (make-regex-t source (irregex (translate-prop-classes source))))
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

(define (jolt-re-find re s)
  (let ((m (irregex-search (regex-t-irx (jolt-re-pattern re)) s)))
    (if m (irx-result m) jolt-nil)))

;; All non-overlapping matches, left to right. Advance past each match end (or by
;; one on a zero-width match). nil when there are no matches (Clojure: seq-able as
;; nil, so (if-let [m (re-seq ...)] ...) works).
(define (jolt-re-seq re s)
  (let ((irx (regex-t-irx (jolt-re-pattern re)))
        (len (string-length s)))
    (let loop ((start 0) (acc '()))
      (let ((m (and (<= start len) (irregex-search irx s start))))
        (if m
            (let ((e (irregex-match-end-index m 0)))
              (loop (if (> e start) e (+ start 1)) (cons (irx-result m) acc)))
            (list->cseq (reverse acc)))))))

(def-var! "clojure.core" "re-pattern" jolt-re-pattern)
(def-var! "clojure.core" "re-matches" jolt-re-matches)
(def-var! "clojure.core" "re-find" jolt-re-find)
(def-var! "clojure.core" "re-seq" jolt-re-seq)
(def-var! "clojure.core" "regex?" jolt-regex?)
