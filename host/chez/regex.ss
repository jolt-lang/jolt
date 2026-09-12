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

;; irregex.scm is portable R[457]RS; it relies on the Chez-compat preamble at
;; the top of rt.ss (expression-position cond-expand, lone-string `error`),
;; which every load path runs before this file.
(load "vendor/irregex/irregex.scm")
;; …and jolt's replacement for its NFA->DFA conversion: the vendored one is
;; quadratic in the DFA size and unbounded in work, which made a large
;; alternation take seconds (or never finish) on its first match. See the file.
(load "host/chez/regex-dfa.ss")


;; A jolt regex value: the source string (for printing / str) + the LAZILY
;; compiled irregex. regex? recognizes it; the printer renders #"source".
;; Construction parses the pattern — a malformed pattern throws
;; PatternSyntaxException at re-pattern, like the JVM — but the engine build
;; waits for the first match: a namespace full of `def`'d patterns loads for
;; the price of parsing, and a pattern that is never matched never compiles.
;; (An HTTP-client middleware regex measured at hundreds of ms of app startup
;; on one host motivated this; the cost now lands on first use or never.)
;; The irx-cell starts #f and is filled through regex-t-irx below; the fill is
;; a single store of an interned value, so a racing double-fill is benign.
(define-record-type regex-t (fields source (mutable irx-cell)) (nongenerative jolt-regex-v2))
;; A capturing pattern is compiled with irregex's BACKTRACKING matcher ('backtrack),
;; not its DFA. java.util.regex is itself a leftmost-first backtracking engine, so
;; this matches the JVM's submatch semantics; irregex's DFA is POSIX leftmost-longest
;; and, worse, leaks a non-participating alternation group's capture (e.g.
;; #"(?:([0-9])|([0-9])r([0-9]+))" on "2r11" left group 1 = "2"), which broke
;; tools.reader's number reader. Non-capturing patterns keep the fast DFA — with no
;; groups to read, its whole-match result is all a caller sees. Which engine a
;; pattern needs is read off its SRE (sre-count-submatches below), so each
;; pattern builds exactly one engine, at first use.

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

;; A pattern the engine will not compile is a PatternSyntaxException, the same
;; catchable thing the JVM throws — not the raw internal error, which surfaced as
;; an unnamed condition no (catch PatternSyntaxException …) could see.
(define (regex-syntax-error source e)
  (jolt-throw
   (jolt-host-throwable "java.util.regex.PatternSyntaxException"
     (string-append (guard (e2 (#t "Unsupported pattern")) (condition-message-of e))
                    " near index 0\n" source))))

(define (condition-message-of e)
  (if (and (condition? e) (message-condition? e)) (condition-message e) "Unsupported pattern"))

;; capturing groups in an SRE — the translator emits (submatch …) for plain
;; groups and (=> name …) for named ones; counting the SRE directly picks the
;; engine without a throwaway compile.
(define (sre-count-submatches sre)
  (let walk ((x sre) (n 0))
    (cond ((pair? x)
           (let ((n (if (memq (car x) '(submatch submatch-named =>)) (+ n 1) n)))
             (let lp ((xs (cdr x)) (n n))
               (if (pair? xs) (lp (cdr xs) (walk (car xs) n)) n))))
          ((vector? x) (let lp ((i 0) (n n))
                         (if (< i (vector-length x)) (lp (+ i 1) (walk (vector-ref x i) n)) n)))
          (else n))))

;; Two cache stages per source, both under the mutex (dynamic-wind, not a bare
;; release: a pattern that fails used to leave the mutex held, blocking every
;; later compile). 'parsed holds the validated SRE; 'irx the built engine.
(define (regex-parsed-entry source)
  (jolt-lock! regex-cache-mutex)
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (or (hashtable-ref regex-cache source #f)
          (let ((entry (guard (e (#t (regex-syntax-error source e)))
                         (let-values (((sre opts) (java-pattern->sre source)))
                           (vector 'parsed sre opts
                                   (or (sre-has-backref? sre)
                                       (> (sre-count-submatches sre) 0)))))))
            (hashtable-set! regex-cache source entry)
            entry)))
    (lambda () (jolt-unlock! regex-cache-mutex))))

;; the built engine for source, compiling once on first demand. A capturing
;; pattern gets irregex's BACKTRACKING matcher (see the engine note above); a
;; group-free one keeps the fast DFA. An engine-build failure on an SRE the
;; parser accepted still surfaces as PatternSyntaxException, just at first use.
(define (regex-compiled-irx source)
  (let ((entry (regex-parsed-entry source)))
    (if (eq? (vector-ref entry 0) 'irx)
        (vector-ref entry 1)
        (begin
          (jolt-lock! regex-cache-mutex)
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (let ((entry (hashtable-ref regex-cache source #f)))
                (if (and entry (eq? (vector-ref entry 0) 'irx))
                    (vector-ref entry 1)
                    (let* ((sre (vector-ref entry 1)) (opts (vector-ref entry 2))
                           (irx (guard (e (#t (regex-syntax-error source e)))
                                  (if (vector-ref entry 3)
                                      (apply irregex sre 'backtrack opts)
                                      (apply irregex sre opts)))))
                      (hashtable-set! regex-cache source (vector 'irx irx))
                      irx))))
            (lambda () (jolt-unlock! regex-cache-mutex)))))))

(define (jolt-regex source)
  (regex-parsed-entry source)        ; eager syntax validation, no engine build
  (make-regex-t source #f))

;; every reader of a regex's engine comes through here; first read compiles.
(define (regex-t-irx r)
  (or (regex-t-irx-cell r)
      (let ((irx (regex-compiled-irx (regex-t-source r))))
        (regex-t-irx-cell-set! r irx)
        irx)))

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
  (let* ((s (rx-charseq->string s))
         (m (irregex-match (regex-t-irx (jolt-re-pattern re)) s)))
    (if m (irx-result m) jolt-nil)))

;; A stateful matcher (java.util.regex.Matcher): the compiled pattern, the target
;; string, the next search position, the last successful irregex match, and the
;; REGION the matcher is confined to. re-find over a matcher steps through
;; non-overlapping matches; re-groups returns the groups of the last one.
;;
;; The region is [rstart, rend) and defaults to the whole string. Under the JVM's
;; default anchoring bounds ^ and $ match AT the region's edges, and that is what
;; irregex gives for free: an (irregex-search irx s from rend) chunks the string
;; as (s from rend), so bos sits at the search origin and eos at the region end.
;; The consumer guard below is what keeps ^ from re-anchoring at a resumed scan
;; position instead of the region start.
(define-record-type matcher-t
  (fields irx str (mutable pos) (mutable last) (mutable rstart) (mutable rend))
  (nongenerative jolt-matcher-v2))
;; EVERY regex entry point takes a CharSequence on the JVM, not just a String, and
;; a library matching over a WINDOW of a larger string passes its own
;; implementation rather than copying — instaparse's Segment is a deftype with
;; length/charAt/subSequence/toString. irregex works on Scheme strings, so realize
;; one: a deftype through jrec-charseq->string (records.ss), a host CharSequence
;; (StringBuilder) through the str registry that already renders its content for
;; (str sb). The class graph is what decides, so a host type that is NOT a
;; CharSequence — a StringWriter is a Writer — still gets the cast error it would
;; have got from jolt-need-str, as on the JVM. Forward refs resolve at call time.
(define (rx-host-charseq->string s)
  (let ((cls (guard (e (#t #f)) (jolt-class-name s))))
    (and (string? cls) (jch-isa? cls "java.lang.CharSequence")
         (let ((content (guard (e (#t #f)) (jolt-object-content s))))
           (and (string? content) content)))))
(define (rx-charseq->string s)
  (cond ((string? s) s)
        ((and (jrec? s) (jrec-charseq->string s)))
        ((rx-host-charseq->string s))
        (else (jolt-need-str s))))
(define (jolt-re-matcher re s)
  (let ((s (rx-charseq->string s)))
    (make-matcher-t (regex-t-irx (jolt-re-pattern re)) s 0 #f 0 (string-length s))))
(define (jolt-matcher? x) (matcher-t? x))

;; java.util.regex.Pattern.flags(). jolt compiles a pattern from its source alone,
;; so the flags it carries are the ones written inline at the front — (?i), (?is)
;; and friends. Values are the Pattern constants, so a caller comparing against
;; Pattern/CASE_INSENSITIVE sees what it expects. A flag set later in the pattern
;; is scoped to that group on the JVM too, so it correctly doesn't count here.
(define (rx-inline-flags src)
  (let ((n (string-length src)))
    (if (or (fx<? n 3)
            (not (char=? (string-ref src 0) #\())
            (not (char=? (string-ref src 1) #\?)))
        0
        (let loop ((i 2) (acc 0))
          (if (fx>=? i n)
              0                                  ; unterminated: not a flag group
              (let ((c (string-ref src i)))
                (case c
                  ((#\)) acc)
                  ((#\i) (loop (fx+ i 1) (fxlogor acc 2)))    ; CASE_INSENSITIVE
                  ((#\x) (loop (fx+ i 1) (fxlogor acc 4)))    ; COMMENTS
                  ((#\m) (loop (fx+ i 1) (fxlogor acc 8)))    ; MULTILINE
                  ((#\s) (loop (fx+ i 1) (fxlogor acc 32)))   ; DOTALL
                  ((#\u) (loop (fx+ i 1) (fxlogor acc 64)))   ; UNICODE_CASE
                  ((#\d) (loop (fx+ i 1) (fxlogor acc 1)))    ; UNIX_LINES
                  ((#\U) (loop (fx+ i 1) (fxlogor acc 256)))  ; UNICODE_CHARACTER_CLASS
                  (else 0))))))))                ; (?:, (?=, a flag we don't model

;; re-find: stateless over (re s), or stateful over a matcher (advance + remember).
(define jolt-re-find
  (case-lambda
    ((re s)
     (let ((m (irregex-search (regex-t-irx (jolt-re-pattern re)) (rx-charseq->string s))))
       (if m (irx-result m) jolt-nil)))
    ((m)
     (let* ((str (matcher-t-str m))
            (end (matcher-t-rend m))
            (start (matcher-t-pos m))
            (mm (and (<= start end)
                     (irx-search-from (matcher-t-irx m) str start (matcher-t-rstart m) end))))
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
  (let ((mm (irregex-match (matcher-t-irx m) (matcher-t-str m)
                           (matcher-t-rstart m) (matcher-t-rend m))))
    ;; like .lookingAt, anchored at the region start rather than the find cursor,
    ;; and a success moves the cursor past the match so a following .find resumes
    ;; where the JVM's would instead of re-finding what was just matched.
    (if mm (matcher-note-match! m mm) (begin (matcher-t-last-set! m #f) #f))))
;; .group before a successful match is the JVM's IllegalStateException, message
;; and all. It used to be a bare ex-info, which a (catch IllegalStateException …)
;; could not select — the same shape of bug as a raw host condition escaping.
(define (jolt-matcher-no-match)
  (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "No match found")))
(define (jolt-matcher-group m . n)
  (let ((last (matcher-t-last m)))
    (if last
        (let ((s (irregex-match-substring last (if (pair? n) (->idx (car n)) 0))))
          (if s s jolt-nil))
        (jolt-matcher-no-match))))
(define (jolt-matcher-group-count m) (irregex-num-submatches (matcher-t-irx m)))
;; .lookingAt: anchored at the region START, matching a PREFIX — the middle ground
;; between .matches (the whole region) and .find (anywhere). It does NOT resume
;; from the find cursor: on the JVM both .matches and .lookingAt anchor at the
;; region's own start, a field only reset/region move, so a .lookingAt after a
;; .find re-anchors at the beginning. jolt models no region, so that start is 0.
;;
;; irregex has no prefix-match entry point, so search from 0 and keep the result
;; only when it begins there — the engine is leftmost-first, so if any match starts
;; at 0 the search finds that one. On success the find cursor moves to the match
;; end, so a following .find continues after it the way the JVM's does.
;; (instaparse's regexp terminal is .lookingAt + .group.)
(define (matcher-note-match! m mm)
  (matcher-t-last-set! m mm)
  (let ((ms (irregex-match-start-index mm 0)) (e (irregex-match-end-index mm 0)))
    (matcher-t-pos-set! m (if (> e ms) e (+ e 1))))
  #t)
(define (jolt-matcher-looking-at m)
  (let* ((origin (matcher-t-rstart m))
         (mm (irregex-search (matcher-t-irx m) (matcher-t-str m) origin (matcher-t-rend m))))
    (if (and mm (= (irregex-match-start-index mm 0) origin))
        (matcher-note-match! m mm)
        (begin (matcher-t-last-set! m #f) #f))))

;; --- .reset / .find(int) / .region: the JVM's scan-position and region controls
;; .reset drops the last match, clears the region back to the whole input and
;; puts the scan cursor at 0. It is what .find(int) and .region are both defined
;; in terms of on the JVM, and it returns the matcher so .reset chains.
(define (matcher-reset! m)
  (matcher-t-last-set! m #f)
  (matcher-t-rstart-set! m 0)
  (matcher-t-rend-set! m (string-length (matcher-t-str m)))
  (matcher-t-pos-set! m 0)
  m)
;; .find(int from): RESET the matcher — region included, which is why the bounds
;; check is against the whole input — and then scan from `from`. The int used to
;; be dropped, so every (.find m i) answered with the first match in the string
;; and the anchored-scan idiom (.find m i) + (= (.start m) i) only ever matched
;; at 0.
(define (jolt-matcher-find-from m i)
  (let ((n (string-length (matcher-t-str m))))
    (when (or (< i 0) (> i n))
      (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" "Illegal start index")))
    (matcher-reset! m)
    (matcher-t-pos-set! m i)
    (not (jolt-nil? (jolt-re-find m)))))
;; .region(start, end): confine every subsequent match to [start, end). Resets
;; first, as the JVM does, so a region also clears the last match and puts the
;; scan cursor at the region start.
(define (jolt-matcher-region m a b)
  (let ((n (string-length (matcher-t-str m))))
    (define (oob what)
      (jolt-throw (jolt-host-throwable "java.lang.IndexOutOfBoundsException" what)))
    (when (or (< a 0) (> a n)) (oob "start"))
    (when (or (< b 0) (> b n)) (oob "end"))
    (when (> a b) (oob "start > end"))
    (matcher-reset! m)
    (matcher-t-rstart-set! m a)
    (matcher-t-rend-set! m b)
    (matcher-t-pos-set! m a)
    m))

;; Next match at or after cursor `i`.
;;
;; A pattern anchored at the start of input (`^` without (?m), or \A) can only
;; match at index 0, so once a scan has moved past 0 there is nothing left to find.
;; irregex marks such a pattern ~consumer? and its own irregex-fold stops on that
;; flag; jolt's scanning loops (re-seq, replace-all, split, matcher find) hand-roll
;; their own loop, so they have to honor it here.
;;
;; The resume index is NOT the origin. irregex-search's start argument is both
;; where the scan begins and what the pattern treats as the beginning of input:
;; it re-anchored ^ there, so (str/replace "abcabc" #"^abc" "-") replaced twice
;; and (re-seq #"^abc" "abcabc") returned two matches where the JVM does one
;; (Selmer's include-tag parser strips its tag with ^.+?include\s*, so a nested
;; {% include "a/include/head.html" %} lost everything up to the LAST "include"),
;; and look-behind could not see the character before the resume point, which is
;; what the wide line-terminator anchors need to tell a CRLF's \n from a lone one.
;;
;; irregex-search/matches takes the two separately — `init` is the origin every
;; assertion is measured from, `i` is where to start looking — so pass the origin
;; as init and the cursor as i. That origin is index 0 for a whole-string scan and
;; the REGION START for a matcher confined to one; the four-argument form takes
;; both it and the region end.
;;
;; The ~consumer? guard in front is now an optimization rather than a correction:
;; a pattern anchored at the start of input cannot match past the origin, and
;; irregex answers #f there on its own — this just saves it the scan.
(define irx-search-from
  (case-lambda
    ((irx s i) (irx-search-from irx s i 0 (string-length s)))
    ((irx s i origin end)
     (and (or (= i origin) (not (flag-set? (irregex-flags irx) ~consumer?)))
          (let ((src (list s origin end))
                (matches (irregex-new-matches irx)))
            (irregex-match-chunker-set! matches irregex-basic-string-chunker)
            (irregex-search/matches irx irregex-basic-string-chunker
                                    (cons src origin) src i matches))))))

;; All non-overlapping matches, left to right. Advance past each match end (or by
;; one on a zero-width match). nil when there are no matches (Clojure: seq-able as
;; nil, so (if-let [m (re-seq ...)] ...) works).
(define (jolt-re-seq re s)
  (let* ((s (rx-charseq->string s))
         (irx (regex-t-irx (jolt-re-pattern re)))
         (len (string-length s)))
    (let loop ((start 0) (acc '()))
      (let ((m (and (<= start len) (irx-search-from irx s start))))
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
;; test probe: has this pattern's engine been built yet? The lazy-compile gate
;; asserts a fresh pattern answers false and a matched one true.
(def-var! "jolt.host" "regex-compiled?"
  (lambda (x) (if (and (regex-t? x) (regex-t-irx-cell x)) #t #f)))
