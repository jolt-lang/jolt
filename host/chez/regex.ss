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


;; A jolt regex value: the source string (for printing / str) + the compiled
;; irregex. regex? recognizes it; the printer renders #"source".
(define-record-type regex-t (fields source irx) (nongenerative jolt-regex-v1))
;; A capturing pattern is compiled with irregex's BACKTRACKING matcher ('backtrack),
;; not its DFA. java.util.regex is itself a leftmost-first backtracking engine, so
;; this matches the JVM's submatch semantics; irregex's DFA is POSIX leftmost-longest
;; and, worse, leaks a non-participating alternation group's capture (e.g.
;; #"(?:([0-9])|([0-9])r([0-9]+))" on "2r11" left group 1 = "2"), which broke
;; tools.reader's number reader. Non-capturing patterns keep the fast DFA — with no
;; groups to read, its whole-match result is all a caller sees. The count comes from
;; a first cheap compile; a capturing pattern is recompiled once (patterns compile
;; once and cache in the regex-t).

;; Compile a Java/Clojure pattern string → a regex-t. The pattern is parsed into an
;; irregex SRE via regex-translate.ss's java-pattern->sre, which handles the full
;; Java regex feature set: escapes, char classes, Unicode \p{...}, quantifiers,
;; groups, flags, anchors, etc. The pattern is parsed ONCE and emitted as SRE
;; directly, so features compose correctly.
(define (sre-has-backref? sre)
  (let walk ((x sre))
    (cond ((pair? x)
           (if (memq (car x) '(backref backref-ci))
               #t
               (let lp ((xs (cdr x)))
                 (and (pair? xs) (or (walk (car xs)) (lp (cdr xs)))))))
          ((vector? x) (let lp ((i 0))
                         (and (< i (vector-length x))
                              (or (walk (vector-ref x i)) (lp (+ i 1))))))
          (else #f))))
(define regex-cache (make-hashtable string-hash string=?))
(define regex-cache-mutex (make-mutex 'regex-cache))

(define (cached-regex-entry source)
  "Return (count . irx) for source, compiling if needed."
  (mutex-acquire regex-cache-mutex)
  (let ((entry (hashtable-ref regex-cache source #f)))
    (if entry
        (begin (mutex-release regex-cache-mutex) entry)
        (let-values (((sre opts) (java-pattern->sre source)))
          (let* ((irx (apply irregex sre opts))
                 (has-caps? (sre-has-backref? sre))
                 (count (irregex-num-submatches irx))
                 (entry (if (or has-caps? (> count 0))
                           (cons count (apply irregex sre 'backtrack opts))
                           (cons 0 irx))))
            (hashtable-set! regex-cache source entry)
            (mutex-release regex-cache-mutex)
            entry)))))

(define (jolt-regex source)
  (let ((entry (cached-regex-entry source)))
    (make-regex-t source (cdr entry))))

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
