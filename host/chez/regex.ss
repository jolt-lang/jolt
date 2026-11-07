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
;; …and java.util.regex's line anchors as O(1) zero-width primitives: expressed as
;; the look-around SREs they are equivalent to, `^`/`$`/`\Z` paid the general
;; look-around machinery at every candidate position. See the file (#1062).
(load "host/chez/java/regex-anchors.ss")
;; …and its SRE compiler, which compiles those primitives and bounds the vendored
;; look-behind: that look-behind rescans from the chunk start, which made any
;; pattern containing one quadratic. See the file (#1062).
(load "host/chez/java/regex-anchor-sre.scm")


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
;; tools.reader's number reader.
;;
;; A NON-capturing pattern used to keep the DFA unconditionally, and that was
;; wrong twice over (#1062). Leftmost-longest is visible without any groups — the
;; JVM answers "a" for (re-find #"a|ab" "ab") and the DFA answered "ab" — and the
;; DFA is not the fast engine for most patterns either: its transition lookup
;; walks a list of char sets per character, so a literal or a class repetition
;; measured 3-8x the backtracking matcher on the same input (#"zzz" 17 ms vs
;; 2.8 ms over 5k lines, #"[0-9]+" 30 ms vs 3.8 ms).
;;
;; What the DFA IS for is the one shape backtracking is bad at: an unbounded
;; repetition with more pattern after it, where a failed match retries that
;; repetition from every start position. #".*z" over a line with no z is quadratic
;; — 268 ms against the DFA's 16 ms, and babashka, itself a backtracking engine,
;; takes 234 ms. So the DFA is kept for exactly those patterns and the
;; backtracking matcher takes the rest, which is both faster and the JVM's own
;; answer. sre-linear-backtrack? below is the test, and it is a WHITELIST: a shape
;; it does not model keeps the DFA.
;;
;; Even there the DFA only answers WHERE the first match starts, which both
;; semantics agree on; the backtracking matcher irx-finish! attaches runs from
;; that start and decides where the match ends (jolt-6gkc). Keeping the DFA's own
;; end made #"[ab]|c*.+" answer "bb" on "bb" where the JVM answers "b".
;;
;; Which engine a pattern needs is read off its SRE, so each pattern builds
;; exactly one engine, at first use.

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

;; Can the backtracking matcher run this SRE in linear time per start position?
;;
;; It cannot when an UNBOUNDED repetition has consuming pattern after it: the
;; repetition takes everything, the tail fails, and it gives a unit back and
;; retries, once per start position. That is the #".*z" shape, and it is the only
;; shape jolt keeps irregex's DFA for (see the engine note above). Two sources of
;; it: a repetition followed by something that can consume, and a repetition
;; nested inside another (#"(?:a+)+"), whose retries multiply.
;;
;; A WHITELIST, deliberately: an SRE shape not modelled here answers #f and keeps
;; the DFA, so a new translator output can only cost speed, never correctness.
;;
;; A trailing ASSERTION does not count as pattern after the repetition. It can
;; still fail and make the repetition give units back, so #"\s+$" is not strictly
;; linear — but no pattern containing an assertion can have a DFA in the first
;; place (irregex's sre->nfa answers #f for `bos`, `eol`, a look-around and jolt's
;; own anchor primitives alike), so refusing those here would change nothing
;; except to make irregex build the NFA twice before giving up.
(define (sre-zero-width? x)
  (cond ((symbol? x)
         (and (memq x '(epsilon bos eos bol eol bow eow nwb commit
                        %java-bol %java-eol %java-final-eol
                        %java-bol-unix %java-final-eol-unix))
              #t))
        ((pair? x)
         (case (car x)
           ((look-ahead neg-look-ahead look-behind neg-look-behind) #t)
           ((seq : submatch $ => submatch-named atomic w/case w/nocase)
            (let lp ((xs (cdr x)))
              (or (null? xs) (and (sre-zero-width? (car xs)) (lp (cdr xs))))))
           ((or) (let lp ((xs (cdr x)))
                   (or (null? xs) (and (sre-zero-width? (car xs)) (lp (cdr xs))))))
           (else #f)))
        (else #f)))

(define (sre-unbounded-rep? x)
  (and (pair? x)
       (or (and (memq (car x) '(* + *? +? >= >=? word+)) #t)
           (and (memq (car x) '(** **?)) (not (number? (caddr x)))))))

(define (sre-has-unbounded-rep? x)
  (or (sre-unbounded-rep? x)
      (and (pair? x)
           (let lp ((xs (cdr x)))
             (and (pair? xs) (or (sre-has-unbounded-rep? (car xs)) (lp (cdr xs))))))))

(define (sre-linear-backtrack? sre)
  (let walk ((x sre))
    (cond
      ((char? x) #t)
      ((string? x) #t)
      ((symbol? x) #t)                       ; a named class or an assertion
      ((not (pair? x)) #f)
      ((string? (car x)) #t)                 ; ("abc"), an enumerated char set
      (else
       (case (car x)
         ((~ & - /) #t)                      ; a char set: one unit, no retry
         ((seq : submatch $ => submatch-named atomic w/case w/nocase)
          ;; every element safe, and nothing that can consume after a repetition
          (let lp ((xs (if (memq (car x) '(=> submatch-named)) (cddr x) (cdr x))))
            (cond ((null? xs) #t)
                  ((not (walk (car xs))) #f)
                  ((and (sre-has-unbounded-rep? (car xs))
                        (let tail ((ys (cdr xs)))
                          (and (pair? ys)
                               (or (not (sre-zero-width? (car ys))) (tail (cdr ys))))))
                   #f)
                  (else (lp (cdr xs))))))
         ((or) (let lp ((xs (cdr x)))
                 (or (null? xs) (and (walk (car xs)) (lp (cdr xs))))))
         ((? ??) (let ((body (sre-sequence (cdr x))))
                   (and (walk body) (not (sre-has-unbounded-rep? body)))))
         ((* + *? +? word+)
          ;; a repetition whose body repeats is the #"(?:a+)+" blowup
          (let ((body (sre-sequence (cdr x))))
            (and (walk body) (not (sre-has-unbounded-rep? body)))))
         ((** **?)
          (let ((body (sre-sequence (cdddr x))))
            (and (number? (cadr x)) (number? (caddr x))
                 (walk body) (not (sre-has-unbounded-rep? body)))))
         ((=) (let ((body (sre-sequence (cddr x))))
                (and (number? (cadr x)) (walk body)
                     (not (sre-has-unbounded-rep? body)))))
         (else #f))))))

(define regex-cache (make-hashtable string-hash string=?))
(define regex-cache-mutex (make-mutex 'regex-cache))

;; A pattern the engine will not compile is a PatternSyntaxException, the same
;; catchable thing the JVM throws — not the raw internal error, which surfaced as
;; an unnamed condition no (catch PatternSyntaxException …) could see.
;; The exact string java.util.regex.PatternSyntaxException.getMessage() builds:
;; the description, " near index N" ONLY when N >= 0, a newline, the whole pattern,
;; and — when N lands inside the pattern — another newline and a caret under that
;; column. The caret padding is one character per column before N: a space, or a
;; tab where the pattern has a tab (PatternSyntaxException.java). N is an index
;; into the pattern's CODE POINTS (Pattern parses those), but getMessage measures
;; and pads it against the Java String's UTF-16 units, so a supplementary
;; character before N is two columns of padding and counts two toward the
;; "inside the pattern" test — a JDK quirk, reproduced.
(define (pattern-syntax-message desc idx source)
  (define (units c) (if (> (char->integer c) #xFFFF) 2 1))
  (define utf16-length
    (let loop ((k 0) (u 0))
      (if (>= k (string-length source)) u (loop (+ k 1) (+ u (units (string-ref source k)))))))
  (string-append
   desc
   (if (and (integer? idx) (>= idx 0))
       (string-append " near index " (number->string idx))
       "")
   "\n" source
   (if (and (integer? idx) (>= idx 0) (< idx utf16-length))
       (string-append
        "\n"
        (let loop ((k 0) (u 0) (pad ""))
          (if (or (>= u idx) (>= k (string-length source)))
              pad
              (let ((c (string-ref source k)))
                (loop (+ k 1) (+ u (units c))
                      (string-append pad (if (char=? c #\tab) "\t" " ")
                                     (if (and (= (units c) 2) (< (+ u 1) idx)) " " ""))))))
        "^")
       "")))

(define (regex-syntax-error source e)
  (let ((desc (if (java-pattern-error? e)
                  (java-pattern-error-desc e)
                  (guard (e2 (#t "Unsupported pattern")) (condition-message-of e))))
        (idx (if (java-pattern-error? e) (java-pattern-error-index e) 0)))
    (jolt-throw
     (jolt-host-throwable/detail "java.util.regex.PatternSyntaxException"
       (if (java-pattern-error? e)
           (pattern-syntax-message desc idx source)
           (string-append desc " near index 0\n" source))
       (vector desc idx source)))))

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

;; Two cache stages per source. 'parsed holds the validated SRE; 'irx the built
;; engine. The HIT is read without the mutex and only a miss takes it, then
;; re-reads under it: every (jolt-regex "src") — which is what a #"…" literal
;; evaluates each time it is reached — comes through here, and with the lookup
;; inside the lock eight threads matching literals ran 24x slower per thread
;; than one. A string-keyed hashtable read racing a locked writer answers stale
;; or not-found, never a torn entry (the class-graph caches read the same way),
;; and a not-found just takes the locked path. The writer holds the mutex under
;; dynamic-wind, not a bare release: a pattern that fails used to leave it held,
;; blocking every later compile.
(define (regex-parsed-entry source)
  (or (hashtable-ref regex-cache source #f)
      (begin
        (jolt-lock! regex-cache-mutex)
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (or (hashtable-ref regex-cache source #f)
                (let ((entry (guard (e (#t (regex-syntax-error source e)))
                               (let-values (((sre opts) (java-pattern->sre source)))
                                 (vector 'parsed sre opts
                                         (or (sre-has-backref? sre)
                                             (> (sre-count-submatches sre) 0)
                                             (sre-linear-backtrack? sre)))))))
                  (hashtable-set! regex-cache source entry)
                  entry)))
          (lambda () (jolt-unlock! regex-cache-mutex))))))

;; Two adjustments to a freshly built irregex, made before it is cached.
;;
;; A DFA-compiled pattern also gets the BACKTRACKING matcher, in the irregex's
;; otherwise empty nfa slot. The DFA is POSIX leftmost-longest and only finds
;; where a match starts; regex-dfa.ss's search runs this matcher from there, so
;; the match ends where java.util.regex's would (jolt-6gkc: #"[ab]|c*.+" on "bb"
;; is "b", the first alternative, not the DFA's "bb"). Building it is cheap: the
;; matcher is closures over the SRE, not an automaton.
;;
;; And ~consumer? is kept only on a pattern anchored at the start of input.
;; irregex also sets it on one that ENDS in `.*`, on the reasoning that a match
;; runs to the end of the input so there is nothing left to find — but #"(?s).*"
;; finds "" at the end after "c", and (.find m 1) on #"(?s)b.*" over "ab" finds
;; "b". The scan loops below stop at a consumer, so both came back nil.
(define (irx-finish! sre irx opts)
  (when (irregex-dfa irx)
    (vector-set! irx 3 (irregex-nfa (apply irregex sre 'backtrack opts))))
  (when (and (flag-set? (irregex-flags irx) ~consumer?) (not (sre-bos? sre)))
    (vector-set! irx 4 (flag-clear (irregex-flags irx) ~consumer?)))
  irx)

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
                                  (irx-finish!
                                   sre
                                   (if (vector-ref entry 3)
                                       (apply irregex sre 'backtrack opts)
                                       (apply irregex sre opts))
                                   opts))))
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
;; The groups vector is built straight into its slot vector: it used to go
;; through a list and apply (a cons per group plus the list->vector copy) on
;; every capturing match.
(define (irx-result m)
  (let ((n (irregex-match-num-submatches m)))
    (if (= n 0)
        (irregex-match-substring m 0)
        (let ((out (make-vector (+ n 1))))
          (let loop ((i 0))
            (if (> i n)
                (make-pvec out)
                (let ((s (irregex-match-substring m i)))
                  (vector-set! out i (if s s jolt-nil))
                  (loop (+ i 1)))))))))

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
;; A matcher OWNS its match machinery: one irregex match vector (allocated at
;; the first search, reset before each — the way irregex-fold reuses its own)
;; and one (string start end) source triple, rebuilt only when the region
;; changes. Each find used to allocate both, plus a chunker cons, before any
;; matching: ~200 bytes per attempt on the call a tokenizer makes per token.
;; `last` is that same vector after a hit — .group/.start/.end read the LAST
;; match, as on the JVM — and #f after a miss, so a stale read is a "No match
;; found" rather than the previous match. test/chez/regex-matcher-test.ss.
(define-record-type matcher-t
  (fields irx str (mutable pos) (mutable last) (mutable rstart) (mutable rend)
          (mutable matches) (mutable src))
  (nongenerative jolt-matcher-v3))
(define (matcher-matches! m)
  (or (matcher-t-matches m)
      (let ((mm (irregex-new-matches (matcher-t-irx m))))
        (matcher-t-matches-set! m mm)
        mm)))
;; The source triple (string start end) irregex's string chunker reads, and the
;; (triple . origin) pair the search starts from, allocated once per matcher
;; and UPDATED IN PLACE when the region moves — a tokenizer sets the region
;; before every attempt, and a rebuilt triple per attempt was 64 bytes of the
;; ~200 this file's header counts. In place is sound because irregex reads the
;; triple only during a search and no search is in flight when the region is
;; set (a matcher is one thread's cursor, as on the JVM).
(define (matcher-src! m)
  (or (matcher-t-src m)
      (let* ((src (list (matcher-t-str m) (matcher-t-rstart m) (matcher-t-rend m)))
             (init (cons src (matcher-t-rstart m))))
        (matcher-t-src-set! m init)
        init)))
(define (matcher-region-set! m a b)
  (matcher-t-rstart-set! m a)
  (matcher-t-rend-set! m b)
  (let ((init (matcher-t-src m)))
    (when init
      (let ((src (car init)))
        (set-car! (cdr src) a)
        (set-car! (cddr src) b)
        (set-cdr! init a)))))
;; The matcher's own search: irx-search-from's body over the matcher's reused
;; vector and triple. Answers the match vector (the matcher's own) or #f.
(define (matcher-search! m i)
  (let ((irx (matcher-t-irx m)) (origin (matcher-t-rstart m)))
    (and (or (= i origin) (not (flag-set? (irregex-flags irx) ~consumer?)))
         (let* ((init (matcher-src! m))
                (matches (matcher-matches! m)))
           (irregex-reset-matches! matches)
           (irregex-match-chunker-set! matches irregex-basic-string-chunker)
           (irregex-search/matches irx irregex-basic-string-chunker
                                   init (car init) i matches)))))
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
    (make-matcher-t (regex-t-irx (jolt-re-pattern re)) s 0 #f 0 (string-length s) #f #f)))
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
     (let* ((end (matcher-t-rend m))
            (start (matcher-t-pos m))
            (mm (and (<= start end) (matcher-search! m start))))
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
        (let ((arg (if (pair? n) (car n) 0)))
          (if (string? arg)
              ;; JVM .group(String) is name-only: an unknown name is an error,
              ;; even when the string looks like a group number.
              (let ((sym (string->symbol arg)))
                (if (assq sym (irregex-match-names last))
                    (let ((s (irregex-match-substring last sym)))
                      (if s s jolt-nil))
                    (jolt-throw (jolt-host-throwable
                                 "java.lang.IllegalArgumentException"
                                 (string-append "No group with name <" arg ">")))))
              (let ((s (irregex-match-substring last (->idx arg))))
                (if s s jolt-nil))))
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
         (mm (matcher-search! m origin)))
    (if (and mm (= (irregex-match-start-index mm 0) origin))
        (matcher-note-match! m mm)
        (begin (matcher-t-last-set! m #f) #f))))

;; --- .reset / .find(int) / .region: the JVM's scan-position and region controls
;; .reset drops the last match, clears the region back to the whole input and
;; puts the scan cursor at 0. It is what .find(int) and .region are both defined
;; in terms of on the JVM, and it returns the matcher so .reset chains.
(define (matcher-reset! m)
  (matcher-t-last-set! m #f)
  (matcher-region-set! m 0 (string-length (matcher-t-str m)))
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
    (matcher-region-set! m a b)
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

;; ---- splitting ----------------------------------------------------------------
;; The two halves clojure.string/split and String.split share, here rather than
;; in java/natives-str.ss because the Gambit boot's string surface (rt-core.ss)
;; splits on a regex too and that file is not shared.

;; The exact text a pattern matches, when it matches exactly one string — or #f
;; when the pattern has any regex structure at all.
;;
;; A great many regex splits are not really regex splits: #"\n" is the single
;; commonest separator in line-oriented code, and it costs a full irregex search
;; per line to find a character. Splitting 20k lines on #"\n" measured ~10x
;; babashka (1086 ms vs 106 ms). Recognising the literal lets the same call take
;; the non-allocating str-index-of scan the literal-separator arm already uses.
;;
;; This is Java regex SOURCE, so a backslash escape is either a control letter or
;; a quoted punctuation character. Anything that can match more than one string —
;; a metacharacter, a quantifier, a class, a group, an anchor, a predefined class
;; like \d, an inline flag like (?i) — declines and keeps the engine. Declining
;; is always safe; only accepting wrongly would be a bug.
(define (regex-literal-text src)
  (let ((n (string-length src)))
    (and (fx>? n 0)
         (let ((out (open-output-string)))
           (let loop ((i 0))
             (if (fx>=? i n)
                 (get-output-string out)
                 (let ((c (string-ref src i)))
                   (cond
                     ((memv c '(#\. #\* #\+ #\? #\[ #\] #\( #\) #\{ #\} #\| #\^ #\$)) #f)
                     ((char=? c #\\)
                      (and (fx<? (fx+ i 1) n)
                           (let ((e (string-ref src (fx+ i 1))))
                             (cond
                               ((char=? e #\n) (write-char #\newline out) (loop (fx+ i 2)))
                               ((char=? e #\r) (write-char #\return out) (loop (fx+ i 2)))
                               ((char=? e #\t) (write-char #\tab out) (loop (fx+ i 2)))
                               ((char=? e #\f) (write-char #\page out) (loop (fx+ i 2)))
                               ;; a quoted punctuation character stands for itself;
                               ;; a quoted LETTER or DIGIT is a class or a back
                               ;; reference (\d \w \s \b \Q \p \1), never a literal
                               ((and (char>? e #\space) (char<=? e #\~)
                                     (not (char-alphabetic? e)) (not (char-numeric? e)))
                                (write-char e out) (loop (fx+ i 2)))
                               (else #f)))))
                     (else (write-char c out) (loop (fx+ i 1)))))))))))

;; (re-split irx s limit) -> parts, splitting at each match. Keeps interior AND
;; trailing empty strings (the clojure.string wrapper drops trailing for limit 0);
;; a positive limit yields at most `limit` parts (the rest kept unsplit).
;; The clojure.string.clj split wrapper
;; layers the trailing-empty trim on top.
(define (re-split irx s limit)
  (let* ((s (jolt-need-str s))
         (len (string-length s)))
    ;; nout counts out — (length out) per part made a limited split O(parts^2)
    (let loop ((start 0) (last 0) (out '()) (nout 0))
      (if (and limit (fx>=? nout (fx- limit 1)))
          (reverse (cons (substring s last len) out))
          (let ((m (and (fx<=? start len) (irx-search-from irx s start))))
            (if (not m)
                (reverse (cons (substring s last len) out))
                (let ((ms (irregex-match-start-index m 0))
                      (me (irregex-match-end-index m 0)))
                  (if (fx=? me ms)                 ; zero-width: emit single-char segment
                      (if (fx>=? start len)
                          (reverse (cons (substring s last len) out))
                          ;; Emit the segment from last to this match point, skip
                          ;; leading empty (JVM semantics for zero-width splits).
                          ;; Resume at me+1, not start+1 — start+1 can still sit at
                          ;; or before ms and re-find this same match (#940).
                          (let ((seg (substring s last ms)))
                            (if (and (string=? seg "") (null? out))
                                (loop (fx+ me 1) me out nout)
                                (loop (fx+ me 1) me (cons seg out) (fx+ nout 1)))))
                      (loop me me (cons (substring s last ms) out) (fx+ nout 1))))))))))

;; ---- replacing --------------------------------------------------------------
;; The same split of labor for clojure.string/replace and replace-first: here
;; so rt-core.ss's Gambit string surface replaces on a regex with the code
;; natives-str.ss uses.
(define (string-has-char? s c)
  (let loop ((i 0))
    (cond ((fx=? i (string-length s)) #f)
          ((char=? (string-ref s i) c) #t)
          (else (loop (fx+ i 1))))))

;; Group N's replacement text: empty when the group did not participate, and the
;; JVM's IndexOutOfBoundsException ("No group N") when N exceeds the count.
(define (group-text m ref)
  (if (fx>? ref (irregex-match-num-submatches m))
      (jolt-throw (jolt-host-throwable
                   "java.lang.IndexOutOfBoundsException"
                   (string-append "No group " (number->string ref))))
      (let ((g (irregex-match-substring m ref)))
        (if g g ""))))

;; A ${name} reference: `i` is the index of the `{`. Returns (next-index . text).
;; The braced form names a group (never a number): a missing `}`, an empty or
;; digit-leading name, and a name no group carries are each the JVM's error.
(define (parse-braced-ref repl m i)
  (let ((len (string-length repl)))
    (define (bad msg)
      (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException" msg)))
    (let close ((k (fx+ i 1)))
      (if (fx>=? k len)
          (bad "named capturing group is missing trailing '}'")
          (if (char=? (string-ref repl k) #\})
              (let ((name (substring repl (fx+ i 1) k)))
                (cond
                  ((fx=? (string-length name) 0)
                   (bad "named capturing group has 0 length name"))
                  ((char<=? #\0 (string-ref name 0) #\9)
                   (bad (string-append "capturing group name {" name
                                       "} starts with digit character")))
                  (else
                   (let ((sym (string->symbol name)))
                     (if (assq sym (irregex-match-names m))
                         (let ((g (irregex-match-substring m sym)))
                           (cons (fx+ k 1) (if g g "")))
                         (bad (string-append "No group with name {" name "}")))))))
              (close (fx+ k 1)))))))

;; The group number of a $N reference starting at index i (a digit): a greedy run
;; of digits clipped to the longest prefix that is a valid group (JVM
;; appendReplacement), so "$12" with one group is group 1 then a literal "2".
;; Returns (next-index . text).
(define (parse-group-ref repl m i)
  (let ((len (string-length repl))
        (ngroups (irregex-match-num-submatches m)))
    (define (digit? c) (char<=? #\0 c #\9))
    (define (dv c) (fx- (char->integer c) 48))
    (let consume ((j (fx+ i 1)) (ref (dv (string-ref repl i))))
      (if (and (fx<? j len)
               (digit? (string-ref repl j))
               (fx<=? (+ (* ref 10) (dv (string-ref repl j))) ngroups))
          (consume (fx+ j 1) (+ (* ref 10) (dv (string-ref repl j))))
          (cons j (group-text m ref))))))

;; Matcher.appendReplacement syntax: $N inserts group N's text and ${name} the
;; text of the named group, while a backslash escapes the next character, so \\
;; inserts one backslash and \$ a literal dollar. A dangling backslash, a `$`
;; that does not start a group reference, and a group index past the count are
;; errors, matching the JVM. Unlike $N, ${...} is a name reference and never a
;; number, so ${1} is the same error as ${y} when no group is named "1".
(define (expand-dollar repl m)
  (let ((len (string-length repl)))
    (let loop ((i 0) (acc '()))
      (if (fx>=? i len)
          (apply string-append (reverse acc))
          (let ((c (string-ref repl i)))
            (cond
              ((char=? c #\\)
               (if (fx>=? (fx+ i 1) len)
                   (jolt-throw (jolt-host-throwable
                                "java.lang.IllegalArgumentException"
                                "character to be escaped is missing"))
                   (loop (fx+ i 2)
                         (cons (string (string-ref repl (fx+ i 1))) acc))))
              ((and (char=? c #\$) (fx>=? (fx+ i 1) len))
               (jolt-throw (jolt-host-throwable
                            "java.lang.IllegalArgumentException"
                            "Illegal group reference: group index is missing")))
              ((and (char=? c #\$)
                    (char<=? #\0 (string-ref repl (fx+ i 1)) #\9))
               (let ((r (parse-group-ref repl m (fx+ i 1))))
                 (loop (car r) (cons (cdr r) acc))))
              ((and (char=? c #\$) (char=? (string-ref repl (fx+ i 1)) #\{))
               (let ((r (parse-braced-ref repl m (fx+ i 1))))
                 (loop (car r) (cons (cdr r) acc))))
              ((char=? c #\$)
               (jolt-throw (jolt-host-throwable
                            "java.lang.IllegalArgumentException"
                            "Illegal group reference")))
              (else (loop (fx+ i 1) (cons (string c) acc)))))))))


;; One match's replacement text. A string gets $N expansion; a fn (jolt closure)
;; is called with the match result (whole string, or [whole g1 ...] when grouped)
;; and its result stringified.
(define (replacement-text replacement m)
  (cond
    ((string? replacement) (expand-dollar replacement m))
    ((procedure? replacement) (jolt-str-render-one (jolt-invoke replacement (irx-result m))))
    (else (jolt-str-render-one replacement))))

;; regex replace, first or all matches.
(define (re-replace irx s replacement all?)
  (let ((len (string-length s)))
    (let loop ((start 0) (last 0) (acc '()))
      (let ((m (and (fx<=? start len) (irx-search-from irx s start))))
        (if (not m)
            (apply string-append (reverse (cons (substring s last len) acc)))
            (let ((ms (irregex-match-start-index m 0))
                  (me (irregex-match-end-index m 0)))
              (let ((acc2 (cons (replacement-text replacement m)
                                (cons (substring s last ms) acc))))
                ;; advance to the match end, or one past a zero-width match —
                ;; which is emitted like any other, then stepped over. The same
                ;; rule re-seq and the matcher's .find use.
                (if all?
                    (loop (if (fx=? me ms) (fx+ me 1) me) me acc2)
                    (apply string-append (reverse (cons (substring s me len) acc2)))))))))))

;; Matcher.replaceAll / .replaceFirst: substitute every match in the region (or
;; just the first), expanding $N in the replacement, then leave the matcher
;; reset with no match — the JVM's post-state. The region confines where matches
;; are sought; the whole input is returned with the region's replacements.
(define (jolt-matcher-replace m replacement all?)
  (let* ((s (matcher-t-str m))
         (irx (matcher-t-irx m))
         (len (string-length s))
         (a (matcher-t-rstart m))
         (b (matcher-t-rend m)))
    (let loop ((pos a) (last a) (acc '()))
      (let ((mm (and (fx<=? pos b) (irx-search-from irx s pos a b))))
        (if (not mm)
            (begin
              (matcher-reset! m)
              (apply string-append (reverse (cons (substring s last len) acc))))
            (let ((ms (irregex-match-start-index mm 0))
                  (me (irregex-match-end-index mm 0)))
              (let ((acc2 (cons (expand-dollar replacement mm)
                                (cons (substring s last ms) acc))))
                ;; same zero-width advance rule as re-replace/re-seq/.find
                (if all?
                    (loop (if (fx=? me ms) (fx+ me 1) me) me acc2)
                    (begin
                      (matcher-reset! m)
                      (apply string-append (reverse (cons (substring s me len) acc2))))))))))))

;; A regex that is really a literal, replaced by a string that is really a
;; literal, is a plain search-and-replace — the same recognition split uses.
;; (str/replace s #"abc" "xyz") ran the engine over every position and measured
;; ~10x babashka (1661 ms vs 174 ms) where THIS function's own literal arm did
;; the identical work in 171 ms.
;;
;; Answers the literal text to search for, or #f to keep the engine. It declines
;; whenever the replacement could mean more than itself: a $-group reference or
;; a backslash escape, both of which the engine path expands, or a FUNCTION,
;; which has to be called with each match. Declining is always safe.
(define (literal-replace-text pat repl)
  (and (jolt-regex? pat)
       (string? repl)
       (not (string-has-char? repl #\$))
       (not (string-has-char? repl #\\))
       (regex-literal-text (regex-t-source pat))))
