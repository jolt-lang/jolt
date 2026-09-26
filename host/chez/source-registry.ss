;; source-registry.ss — map emitted procedures back to Clojure source for native
;; stack traces, and render an uncaught throwable.
;;
;; A direct-linked def compiles to (define jv$ns/name <fn>); the back end also
;; emits (jolt-register-source! "jv$ns/name" ns name file line) once per such def
;; — at definition time, so there is zero per-call cost. On an uncaught error we
;; walk Chez's native continuation frames, read each frame's procedure name, and
;; look it up here to print a Clojure backtrace.
;;
;; CAVEATS. Names map only for stable Chez procedure names — direct-link / AOT
;; closed-world builds. The open-world -e/repl/run path stores fns in var cells
;; as anonymous lambdas, so its frames don't map (the trace falls back to the
;; top-level location compile-eval.ss tracks). Pervasive tail-call optimization
;; also erases tail-called frames, so even a mapped trace shows only the non-tail
;; spine — the immediate error site is often a tail call and won't appear.

;; Keyed by the procedure name Chez actually reports for a frame — the SHORT
;; munged fn name (the letrec self-binding emit-fn uses), e.g. "deepest", not the
;; jv$ns/name global. Two vars in different namespaces can share a short name; an
;; 'ambiguous marker then keeps the frame name in the trace but drops the
;; (now-uncertain) ns/file:line, so a trace is never misattributed.
(define source-registry (make-hashtable string-hash string=?))

;; The whole cond is ONE critical section. This runs at load, emitted by the back
;; end once per def, and namespaces now load in parallel — so two threads
;; registering the same procname under different namespaces both read #f, both
;; write their own vector, and neither writes 'ambiguous. The trace then names one
;; of them with confidence, which is exactly the misattribution the marker exists
;; to prevent (400 contended pairs missed it 3 times). Reads stay unlocked: this is
;; a strong hashtable and every read of it is single-key.
(define source-registry-mu (make-mutex))
;; The same records, keyed "ns/name" instead of by emitted procedure name. A
;; spliced call site names its callee by fqn (the trace marker carries it) and has
;; no procedure of its own to look up, so this is the index that answers it. Fed
;; from the same registration, under the same lock; unlike the procname table it
;; cannot go 'ambiguous, because a fqn identifies exactly one var.
(define source-registry-by-fqn (make-hashtable string-hash string=?))
(define (srcreg-record-for-fqn fqn)
  (and (string? fqn) (hashtable-ref source-registry-by-fqn fqn #f)))
(define (jolt-register-source! procname ns nm file line)
  (jolt-with-mutex source-registry-mu
    (let ((rec (vector ns nm file line))
          (existing (hashtable-ref source-registry procname #f)))
      (hashtable-set! source-registry-by-fqn (string-append ns "/" nm) rec)
      (cond
        ((not existing) (hashtable-set! source-registry procname rec))
        ((and (vector? existing)
              (or (not (equal? (vector-ref existing 0) ns))
                  (not (equal? (vector-ref existing 1) nm))))
         (hashtable-set! source-registry procname 'ambiguous)))))
  jolt-nil)
(def-var! "jolt.host" "register-source!" jolt-register-source!)

;; The continuation to walk for an uncaught value: the one jolt-throw captured for
;; THIS value (identity-tagged via jolt-throw-cont, so a stale entry from an
;; earlier caught throw is never reused), else a host condition's own
;; &continuation, else #f. raw may arrive as the &jolt-throw condition wrapping
;; the value (the built-binary launcher hands jolt-report-throwable the guard's
;; raw value) or already unwrapped (the cli unwraps first); unwrap here so the
;; identity match holds either way.
(define (jolt-error-continuation raw)
  (let* ((v (jolt-unwrap-throw raw))
         (tc (jolt-throw-cont)))
    (cond
      ((and (pair? tc) (eq? (car tc) v)) (cdr tc))
      ((and (condition? v) (continuation-condition? v)) (condition-continuation v))
      ;; a host fault the catch boundary turned into a throwable: the raise-time
      ;; continuation is on the condition it came from (java/host-faults.ss)
      ((jolt-fault-condition-of v)
       => (lambda (c) (and (continuation-condition? c) (condition-continuation c))))
      (else #f))))

;; An anonymous fn inside a def is emitted as jfn$<ns>/<def>$<n>: (ns def n) when
;; nm is one and the def is registered, else #f. The JVM names the same fn
;; ns$def$fn__n, which is what a StackTraceElement reports for it.
(define (srcreg-jfn-parts nm)
  (and (fx>? (string-length nm) 4)
       (string=? (substring nm 0 4) "jfn$")
       (let* ((body (substring nm 4 (string-length nm)))
              (n (string-length body))
              (dollar (let loop ((i (fx- n 1)))
                        (cond ((fx<? i 0) #f)
                              ((char=? (string-ref body i) #\$) i)
                              (else (loop (fx- i 1))))))
              (fqn (if dollar (substring body 0 dollar) body))
              (rec (srcreg-record-for-fqn fqn)))
         (and (vector? rec)
              (list (vector-ref rec 0) (vector-ref rec 1)
                    (if dollar (substring body (fx+ dollar 1) n) "0"))))))

;; A frame inspector's procedure name as a string, or #f for a non-frame / unnamed.
(define (srcreg-frame-name io)
  (and (guard (e (#t #f)) (eq? (io 'type) 'continuation))
       (let ((code (guard (e (#t #f)) (io 'code))))
         (and code
              (let ((nm (guard (e (#t #f)) (code 'name))))
                (cond ((string? nm) nm)
                      ((symbol? nm) (symbol->string nm))
                      (else #f)))))))

;; Frame names that are pure Chez / jolt-runtime plumbing — the eval boundary,
;; the var-cell trampoline, continuation/winder internals. They carry no Clojure
;; meaning, so an unmapped frame with one of these names is dropped from the trace
;; (a MAPPED frame is always kept — a jolt fn that happens to share the name still
;; resolves to its source). Any name Chez prefixes with `$` (system) or that jolt
;; prefixes with `jolt-` (host runtime) is plumbing too.
(define srcreg-plumbing-names
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (s) (hashtable-set! h s #t))
              '("dynamic-wind" "winder-dummy" "ksrc" "invoke" "apply"
                "call-with-values" "call/cc" "call-with-current-continuation"
                "raise" "raise-continuable" "with-exception-handler" "guard"
                "eval" "compile" "interpret" "expand" "read" "load"
                ;; host dispatch/coercion helpers (not `jolt-` prefixed) that carry
                ;; no Clojure meaning in a trace
                "record-method-dispatch" "protocol-resolve" "devirt-resolve"
                "list->cseq" "host-static-call" "host-call"
                ;; the CLI's own entry frames: every trace ended with these, and
                ;; they say only that jolt was started, which the reader knows.
                ;; run-script! is the one that loads a FILE (a script, a `run
                ;; FILE`, an alias's :main-opts), so it sits directly under the
                ;; program's own frames on every script trace.
                "cmd-run" "run-script!" "load-jolt-file*" "jolt-cli-run"))
    h))
;; The `jolt-` prefix rule is also what BOUNDS A FIBER BACKTRACE, which is not
;; obvious from here. A throw inside a go body has the whole scheduler below it
;; on the continuation — jolt-fiber-run, jolt-fiber-drain!, jolt-fiber-resume*,
;; jolt-fiber-carrier-loop — and every one of those is filtered by that prefix,
;; so the rendered trace ends at the user's own frames with nothing after them.
;; Swish needs an explicit boundary marker for this (erlang.ss limit-stack,
;; a recognisable frame walk-stack stops at); jolt gets it from the naming
;; convention instead, and more generally, since the rule covers any host frame
;; rather than one planted spot.
;;
;; So this is load-bearing beyond tidiness: narrowing the prefix rule, or letting
;; a scheduler procedure be named without the prefix, puts carrier internals back
;; into every go-block trace.
;;
;; The reader's own procedures (reader.ss, every one rdr-) are plumbing by the
;; same kind of rule. A read error used to print ten of them — rdr-read-form,
;; rdr-read-seq, rdr-read-form … — above the one line that said what was wrong;
;; they name the parser's shape, never anything in the program being read. A
;; list of them went stale the first time a raise moved out of tail position in
;; a helper nobody had listed.
(define (srcreg-plumbing-name? nm)
  (or (hashtable-ref srcreg-plumbing-names nm #f)
      (and (fx>? (string-length nm) 0) (char=? (string-ref nm 0) #\$))
      (and (fx>=? (string-length nm) 5) (string=? (substring nm 0 5) "jolt-"))
      (and (fx>=? (string-length nm) 4) (string=? (substring nm 0 4) "rdr-"))))

;; An inspector message that may return ZERO values (e.g. 'source-path when a
;; frame carries no source) — read it as one value or #f. `guard` alone cannot
;; catch a zero-value return, so the read goes through call-with-values with a
;; null-tolerant collector.
(define (srcreg-inspect-get io msg)
  (call-with-values (lambda () (guard (e (#t (values 'srcreg-inspect-err))) (io msg)))
    (lambda args (if (or (null? args) (eq? (car args) 'srcreg-inspect-err)) #f (car args)))))

;; A frame's source as (source-name . byte-offset), or #f. Read from the
;; inspector's 'source-object (which carries the sfd and the bfp/efp range)
;; rather than 'source-path: the latter reports only the path string for
;; eval'd code, while the source object always carries the positions.
(define (srcreg-frame-source-pair io)
  (guard (e (#t #f))
    (let ((so (srcreg-inspect-get io 'source-object)))
      (and so
           (let ((path (source-file-descriptor-path (source-object-sfd so)))
                 (bfp (source-object-bfp so)))
             (and (string? path) (fixnum? bfp) (cons path bfp)))))))

;; JOLT_DEBUG_FRAMES extra: the frame's source object, when it has one. For the
;; eval path that is (synthetic-name . offset) resolved through the eval marker
;; registry back to the ORIGINAL clj line — the surface trace-r2's gate asserts
;; on. Exception-proof: the debug print must never break the frame walk.
(define (srcreg-frame-source-debug io)
  (guard (e (#t ""))
    (let ((pair (srcreg-frame-source-pair io)))
      (if pair
          (let ((name (car pair)) (offset (cdr pair)))
            (string-append " source=" name "@" (number->string offset)
                           (if (jolt-eval-source-name? name)
                               (let ((l (jolt-eval-source-line name offset)))
                                 (if l (string-append " -> clj:L" (number->string l))
                                     " -> clj:?"))
                               "")))
          ""))))

;; A frame's OWN line — where its execution is paused, which Chez annotates with
;; the position of the call that CREATED the frame: the call site inside the
;; caller, exactly what the JVM prints for a frame. Resolve the frame's
;; (source-name . offset) through the marker lookup: a .scm path resolves via the
;; on-disk marker table (jolt-marker-line-in-file), a synthetic eval name via the
;; eval registry (jolt-eval-source-line). #f when the frame carries no source or
;; no marker precedes its offset — the renderer then falls back to the defn line.
;; Exception-proof: the reporter runs while an error is being reported.
(define (srcreg-frame-entry-from-source-pair io ns)
  (guard (e (#t #f))
    (let ((pair (srcreg-frame-source-pair io)))
      (and pair
           (let ((name (car pair)) (offset (cdr pair)))
             (cond
               ((jolt-eval-source-name? name) (jolt-eval-source-entry name offset))
               ((jolt-baked-marker-table ns) => (lambda (t) (jolt-marker-line-from-table t offset)))
               (else (jolt-marker-entry-in-file name offset))))))))

;; --- marker tables a built binary carries ------------------------------------
;; A built binary's app code was compiled from unit files in its build directory,
;; one per namespace, and a frame's line comes from the markers in that file. Read
;; off the disk, the answer depended on the directory still being there: a binary
;; copied elsewhere, or one whose .build dir was cleaned, reported its frames at
;; their defn lines and lost every spliced frame, and so did one assembled from
;; build-cache units, which name the directory of the build that first compiled
;; them. So each unit carries its own table and registers it here as it loads,
;; under the namespaces it holds (build.ss bld-append-marker-table!); a frame's
;; namespace is its fn's, which the walk already has.
(define jolt-baked-marker-tables (make-hashtable string-hash string=?))   ; ns -> table
(define (jolt-register-marker-table! nses table)
  (for-each (lambda (ns) (hashtable-set! jolt-baked-marker-tables ns table)) nses))
(define (jolt-baked-marker-table ns)
  (and (string? ns) (hashtable-ref jolt-baked-marker-tables ns #f)))


;; The logical frames a marker/site entry stands for, innermost first, as
;; srcreg-frame records. `nm`/`own` are the PHYSICAL fn (its procname and record);
;; the entry's chain names the fns whose code was spliced into it.
;;
;; Reading the chain: the entry's own line locates the first fn in it, entry i's
;; call-line locates entry i+1, and the last call-line locates the physical fn.
;; So a chain of N produces N+1 frames, which is exactly what the same code reads
;; as without inlining. An entry naming a fn nothing registered stops the walk and
;; the physical frame stands alone — incomplete, never invented.
(define (srcreg-entry-frames nm own e)
  (let ((ok (lambda (l) (and (fixnum? l) (fx>? l 0) l))))
    (let loop ((ch (jolt-marker-entry-chain e))
               (line (jolt-marker-entry-line e))
               (acc '()))
      (cond
        ((null? ch) (reverse (cons (srcreg-frame nm own (ok line)) acc)))
        (else
         (let* ((ent (car ch))
                (rec (srcreg-record-for-fqn (car ent))))
           (if (not rec)
               (reverse (cons (srcreg-frame nm own (ok line)) acc))
               (loop (cdr ch) (cdr ent)
                     (cons (srcreg-frame (string-append (vector-ref rec 0) "/"
                                                        (vector-ref rec 1))
                                         rec (ok line))
                           acc)))))))))
;; Walk a continuation, returning its frames (innermost first) as (frame-name .
;; record) pairs. record is a source vector #(ns name file line) for a frame that
;; maps to registered Clojure source, the symbol 'ambiguous for a short name shared
;; across namespaces, or #f for an unmapped-but-named frame (the common case on the
;; open-world eval path, where nothing is registered — the bare frame name is still
;; a useful trace line). Plumbing frames (host spine, eval boundary) and unnamed
;; frames are skipped; raw depth is capped. Each frame carries the line reached
;; INSIDE it, from the R1/R2 marker lookup on its source object — the caller's
;; call site — so a rendered frame points at where it was when the error hit, not
;; where its fn was defined.
(define (jolt-frame-records k)
  ;; read the env at call time, not load time: a built binary runs top-level forms
  ;; at heap-build time, where this would always be unset.
  (let ((debug? (getenv "JOLT_DEBUG_FRAMES")))
   (guard (e (#t '()))
    (let loop ((ios (sa-continuation-frames k)) (acc '()))
      (if (null? ios)
          (reverse acc)
          (let* ((io (car ios))
                 (nm (srcreg-frame-name io))
                 (src (and nm (hashtable-ref source-registry nm #f)))
                 ;; keep a frame that maps, or any named frame that isn't plumbing
                 (keep? (and nm (or src (not (srcreg-plumbing-name? nm)))))
                 ;; the marker entry at the offset this frame is stopped at (only
                 ;; a mapped frame prints a line, so only it pays the lookup)
                 (parts (and nm (not src) (srcreg-jfn-parts nm)))
                 (entry (and nm (or src parts)
                             (srcreg-frame-entry-from-source-pair
                               io (cond ((vector? src) (vector-ref src 0))
                                        ((pair? parts) (car parts))
                                        (else #f)))))
                 (line (jolt-marker-entry-line entry)))
            (when (and debug? nm)
              (display (string-append "  [frame] " nm (if src " *MAPPED*"
                                                          (if keep? "" " (skipped)"))
                                      (srcreg-frame-source-debug io) "\n")
                       (current-error-port)))
            (loop (cdr ios)
                  (if (not keep?)
                      acc
                      ;; One PHYSICAL frame can stand for several logical ones
                      ;; when code was spliced into it. srcreg-entry-frames hands
                      ;; them back innermost first, and acc is reversed on return,
                      ;; so each is consed IN ORDER — a left fold, not a recursive
                      ;; cons, which would put them on backwards.
                      (let add ((fs (srcreg-entry-frames nm src entry)) (a acc))
                        (if (null? fs) a (add (cdr fs) (cons (car fs) a))))))))))))

;; --- clj-line lookup for generated .scm offsets ---------------------------------
;;
;; The back end (backend_scheme.clj with-site) emits an inline Chez block comment
;; #|L<clj-line>|# immediately before every traced call site in the generated
;; .scm. A Chez frame's source object carries a byte offset into that file; a
;; backtrace must map it back to the ORIGINAL .clj line the site was emitted
;; from. Returns the line as a fixnum, or #f when no marker precedes the offset.
;;
;; The scan is FORWARD, tracking Scheme lexical state, and builds a sorted table
;; of (marker-start . line) pairs in ONE pass; a lookup is a binary search over
;; that table (the nearest preceding marker), O(log markers) per frame instead
;; of O(file). A backward scan could not tell a real marker from the same bytes
;; inside a user STRING LITERAL — a Clojure string like "#|L999|# oops" is
;; user-controlled, and every offset after it would resolve to a fabricated
;; line. The forward scanner skips:
;;   - "..." string literals, with \ escapes (an escaped \" does not end the
;;     string; \\ then a quote DOES — the backslash escapes exactly one char);
;;   - #| ... |# block comments, which NEST in Chez (a marker is recognized only
;;     when the comment is exactly #|L<digits>|# with an immediate close);
;;   - #\ character literals, so a #\" can't open a string or a #\| start a
;;     comment (defensive: the emitter renders chars as (integer->char N), never
;;     #\);
;;   - ; line comments (the emitter never emits one — a top-level form is one
;;     line, so a ; comment would swallow the rest of it — but they are free to
;;     skip).
;; Deliberately NOT handled (the emitter cannot produce them): #; datum
;; comments, and bar-delimited |symbol| syntax containing #|.
(define jolt-marker-cache-text (make-hashtable string-hash string=?))
(define jolt-marker-cache-file (make-hashtable string-hash string=?))

;; Forward scan -> sorted vector of (start . line) pairs, one per real marker.
(define (jolt-marker-table text)
  (define n (string-length text))
  (define (digit? c) (char<=? #\0 c #\9))
  (define (alpha? c) (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))
  (define (hex-digit? c)
    (or (digit? c) (char<=? #\a c #\f) (char<=? #\A c #\F)))
  ;; Is text[i..] a marker?  Two shapes:
  ;;   #|L<digits>|#                     -> entry is the line, a fixnum
  ;;   #|L<digits>@<fqn>@<digits>|#      -> entry is #(line fqn callsite-line)
  ;; The second is emitted for a site the inline pass copied out of another fn
  ;; (backend_scheme with-site); fqn is that fn's ns/name and the trailing number
  ;; is the line of the call in the fn it was copied into. The emitter refuses to
  ;; write the long form when the fqn holds a |, # or @, so the fqn field here can
  ;; be read up to the next @ without escaping. Returns (entry . end) or #f.
  (define (marker-at? i)
    (and (<= (+ i 3) n)
         (char=? (string-ref text i) #\#)
         (char=? (string-ref text (+ i 1)) #\|)
         (char=? (string-ref text (+ i 2)) #\L)
         (let loop ((j (+ i 3)) (acc 0) (digits? #f))
           (cond
             ((and (< j n) (digit? (string-ref text j)))
              (loop (+ j 1) (+ (* acc 10)
                               (- (char->integer (string-ref text j))
                                  (char->integer #\0)))
                    #t))
             ((and digits? (< (+ j 1) n)
                   (char=? (string-ref text j) #\|)
                   (char=? (string-ref text (+ j 1)) #\#))
              (cons acc (+ j 2)))
             ;; the inline form: one or more @<fqn>@<digits> groups, then |#
             ((and digits? (< j n) (char=? (string-ref text j) #\@))
              (let group ((p j) (chain '()))
                ;; at p: either "|#" (done) or "@<fqn>@<digits>"
                (cond
                  ((and (< (+ p 1) n)
                        (char=? (string-ref text p) #\|)
                        (char=? (string-ref text (+ p 1)) #\#))
                   (and (pair? chain) (cons (vector acc (reverse chain)) (+ p 2))))
                  ((and (< p n) (char=? (string-ref text p) #\@))
                   (let fqn ((k (+ p 1)))
                     (cond
                       ((>= k n) #f)
                       ((char=? (string-ref text k) #\@)
                        (let at ((m (+ k 1)) (a 0) (d? #f))
                          (cond
                            ((and (< m n) (digit? (string-ref text m)))
                             (at (+ m 1) (+ (* a 10)
                                            (- (char->integer (string-ref text m))
                                               (char->integer #\0)))
                                 #t))
                            ((not d?) #f)
                            (else (group m (cons (cons (substring text (+ p 1) k) a)
                                                 chain))))))
                       ;; a | or # inside the fqn field means this is not a marker
                       ;; the emitter wrote; refuse rather than guess
                       ((or (char=? (string-ref text k) #\|)
                            (char=? (string-ref text k) #\#)) #f)
                       (else (fqn (+ k 1))))))
                  (else #f))))
             (else #f)))))
  (let scan ((i 0) (out '()))
    (cond
      ((>= i n) (list->vector (reverse out)))
      ;; string literal: \" stays inside, \\ does not escape a later quote
      ((char=? (string-ref text i) #\")
       (let skip ((j (+ i 1)))
         (cond
           ((>= j n) (scan n out))
           ((char=? (string-ref text j) #\\)
            (if (< (+ j 1) n) (skip (+ j 2)) (scan n out)))
           ((char=? (string-ref text j) #\") (scan (+ j 1) out))
           (else (skip (+ j 1))))))
      ;; #\ char literal: consume it whole so #\" etc. can't mislead the scan
      ((and (char=? (string-ref text i) #\#)
            (< (+ i 1) n)
            (char=? (string-ref text (+ i 1)) #\\))
       (cond
         ((>= (+ i 2) n) (scan n out))
         ((let ((c (string-ref text (+ i 2))))
            (or (char=? c #\x) (char=? c #\u)))
          ;; hex codepoint: #\x41, #\u0041, #\x(41), #\u(41)
          (if (and (< (+ i 3) n) (char=? (string-ref text (+ i 3)) #\())
              (let inner ((k (+ i 4)))
                (cond
                  ((and (< k n) (hex-digit? (string-ref text k))) (inner (+ k 1)))
                  ((and (< k n) (char=? (string-ref text k) #\))) (scan (+ k 1) out))
                  (else (scan (+ i 3) out))))
              (let skip ((j (+ i 3)))
                (cond ((and (< j n) (hex-digit? (string-ref text j))) (skip (+ j 1)))
                      (else (scan j out))))))
         ((alpha? (string-ref text (+ i 2)))
          ;; named char: #\space, #\newline, ...
          (let skip ((j (+ i 3)))
            (cond ((and (< j n) (alpha? (string-ref text j))) (skip (+ j 1)))
                  (else (scan j out)))))
         (else (scan (+ i 3) out))))
      ;; #| block comment (nests); our markers are exactly such comments
      ((and (char=? (string-ref text i) #\#)
            (< (+ i 1) n)
            (char=? (string-ref text (+ i 1)) #\|))
       (let ((m (marker-at? i)))
         (if m
             (scan (cdr m) (cons (cons i (car m)) out))
             (let skip ((j (+ i 2)) (depth 1))
               (cond
                 ((>= j n) (scan n out))
                 ((and (char=? (string-ref text j) #\#)
                       (< (+ j 1) n)
                       (char=? (string-ref text (+ j 1)) #\|))
                  (skip (+ j 2) (+ depth 1)))
                 ((and (char=? (string-ref text j) #\|)
                       (< (+ j 1) n)
                       (char=? (string-ref text (+ j 1)) #\#))
                  (if (= depth 1) (scan (+ j 2) out) (skip (+ j 2) (- depth 1))))
                 (else (skip (+ j 1) depth)))))))
      ;; ; line comment
      ((char=? (string-ref text i) #\;)
       (let skip ((j (+ i 1)))
         (cond
           ((>= j n) (scan n out))
           ((char=? (string-ref text j) #\newline) (scan (+ j 1) out))
           (else (skip (+ j 1))))))
      (else (scan (+ i 1) out)))))

;; Rightmost marker start <= offset, else #f. Binary search over the sorted
;; table; an offset past the last marker (or past the end of the file) lands on
;; the last marker, matching the old backward scan. An offset exactly AT a
;; marker's start resolves to that marker (its bytes begin there).
(define (jolt-marker-line-from-table table offset)
  (let loop ((lo 0) (hi (- (vector-length table) 1)) (best #f))
    (cond
      ((> lo hi) best)
      (else
       (let ((mid (quotient (+ lo hi) 2)))
         (if (<= (car (vector-ref table mid)) offset)
             (loop (+ mid 1) hi (cdr (vector-ref table mid)))
             (loop lo (- mid 1) best)))))))

;; A table entry is either the clj line (a fixnum), or #(line chain) for a site
;; the inline pass copied out of another fn — chain being ((fqn . call-line) ...)
;; innermost first, the logical frames between the site and the physical fn it
;; ended up inside. Everything that only wants the line goes through
;; jolt-marker-entry-line, so the extended form is transparent to it.
(define (jolt-marker-entry-line e) (if (vector? e) (vector-ref e 0) e))
(define (jolt-marker-entry-chain e) (if (vector? e) (vector-ref e 1) '()))


(define (jolt-marker-line-at-offset text offset)
  (jolt-marker-entry-line
   (let ((tbl (hashtable-ref jolt-marker-cache-text text #f)))
    (if tbl
        (jolt-marker-line-from-table tbl offset)
        (let ((tbl (jolt-marker-table text)))
          ;; content-keyed: exact (never stale), and a backtrace resolves many
          ;; frames against the same generated text. Cap at 16 entries so a
          ;; long-lived process can't accumulate every file it ever touched.
          (when (fx>=? (hashtable-size jolt-marker-cache-text) 16)
            (hashtable-clear! jolt-marker-cache-text))
          (hashtable-set! jolt-marker-cache-text text tbl)
          (jolt-marker-line-from-table tbl offset))))))

;; --- eval-path source registry -------------------------------------------------
;; On the eval path (an AOT cache MISS) the emitted Scheme is a transient string:
;; it is gone by the time anything throws. To let a backtrace resolve a frame's
;; (source-name . offset) back to a clj line, compile-eval.ss registers the marker
;; table for the text under a SYNTHETIC source name before eval'ing it. The name
;; is never a filesystem path (jolt-eval-source-name?), so a consumer knows to
;; consult this registry instead of reading a .scm off disk. Populated only when
;; tracing is on; with tracing off the whole block costs nothing.
;;
;; Growth is bounded: jolt-eval-source-max tables, oldest evicted FIFO. A table
;; is one (marker-start . clj-line) pair per traced call site — a few hundred
;; bytes for a typical top-level form — so the registry stays under roughly
;; jolt-eval-source-max * (form size) no matter how long a REPL/nREPL session
;; runs, and the most recent forms (the set a fresh backtrace can mention) are
;; exactly the ones kept.
;;
;; Thread-safety: futures/agents compile on their own threads, so two threads
;; must not draw the same counter value or interleave table inserts. A mutex
;; guards counter increment + insert; that is one acquisition per top-level
;; form, under tracing only, negligible against the analysis+emit already paid.
(define jolt-eval-source-max 4096)
(define jolt-eval-source-counter 0)
(define jolt-eval-source-mutex (make-mutex))
(define jolt-eval-marker-registry (make-hashtable string-hash string=?))
(define jolt-eval-source-queue '())          ; names in insertion order (FIFO)
(define jolt-eval-source-queue-tail '())
(define (jolt-eval-queue-push! name)
  (let ((c (cons name '())))
    (if (null? jolt-eval-source-queue-tail)
        (begin (set! jolt-eval-source-queue c)
               (set! jolt-eval-source-queue-tail c))
        (begin (set-cdr! jolt-eval-source-queue-tail c)
               (set! jolt-eval-source-queue-tail c)))))
(define (jolt-eval-queue-pop!)
  (when (pair? jolt-eval-source-queue)
    (let ((name (car jolt-eval-source-queue)))
      (set! jolt-eval-source-queue (cdr jolt-eval-source-queue))
      (when (null? jolt-eval-source-queue) (set! jolt-eval-source-queue-tail '()))
      name)))
;; Register the marker table for one eval'd Scheme text; returns the synthetic
;; source name to pass make-source-file-descriptor. Once per top-level form.
(define (jolt-register-eval-marker-table! scm)
  (jolt-lock! jolt-eval-source-mutex)
  (let ((name (string-append "jolt-eval-src-"
                             (number->string jolt-eval-source-counter))))
    (set! jolt-eval-source-counter (+ jolt-eval-source-counter 1))
    (when (fx>=? (hashtable-size jolt-eval-marker-registry) jolt-eval-source-max)
      (let ((old (jolt-eval-queue-pop!)))
        (when old (hashtable-delete! jolt-eval-marker-registry old))))
    (hashtable-set! jolt-eval-marker-registry name (jolt-marker-table scm))
    (jolt-eval-queue-push! name)
    (jolt-unlock! jolt-eval-source-mutex)
    name))
;; Resolve an eval-path frame's (name . offset) to a clj line, or #f when the
;; name was evicted / never registered or no marker precedes the offset.
(define (jolt-eval-source-entry name offset)
  (let ((tbl (hashtable-ref jolt-eval-marker-registry name #f)))
    (and tbl (jolt-marker-line-from-table tbl offset))))
(define (jolt-eval-source-line name offset)
  (jolt-marker-entry-line (jolt-eval-source-entry name offset)))
;; Is this source name a registry key rather than a real file? The distinguisher
;; between "consult the eval registry" and "read a .scm off disk": every name
;; this registry mints is "jolt-eval-src-" followed by decimal digits, and no
;; generated artifact on disk is ever named that.
(define (jolt-eval-source-name? name)
  (and (string? name)
       (let ((n (string-length name)))
         (and (fx>=? n 14)
              (string=? (substring name 0 14) "jolt-eval-src-")
              (let loop ((i 14))
                (or (fx>=? i n)
                    (and (char<=? #\0 (string-ref name i) #\9)
                         (loop (fx+ i 1)))))))))

;; Convenience wrapper for a generated file on disk. Cached per path+mtime: the
;; same .scm serves every frame of a backtrace, so read+scan it once. Whole-
;; second mtime granularity means a file regenerated within the same second can
;; serve a stale table — acceptable on a debug path, never a correctness claim.
(define (jolt-marker-entry-in-file path offset)
  (define (read-table)
    (call-with-input-file path
      (lambda (p) (jolt-marker-table (get-string-all p)))))
  (let* ((mtime (div (sa-file-mtime-ms path) 1000))
         (entry (hashtable-ref jolt-marker-cache-file path #f)))
    (unless (and entry (= (car entry) mtime))
      (set! entry (cons mtime (read-table)))
      (hashtable-set! jolt-marker-cache-file path entry))
    (jolt-marker-line-from-table (cdr entry) offset)))
(define (jolt-marker-line-in-file path offset)
  (jolt-marker-entry-line (jolt-marker-entry-in-file path offset)))

;; Render a list of (frame-name . record) pairs (innermost/deepest first) to a
;; backtrace string. record is a source vector #(ns name file line) -> "ns/name
;; (file:line)", or 'ambiguous / #f -> the bare frame name. A run of the same
;; frame-name collapses to one "name (xN)" line (deep recursion, or a hot fn a
;; loop re-enters), and the number of distinct lines is capped.
;; A frame to render: #(frame-name record own-line). own-line is the line reached
;; INSIDE that frame (#f when unknown), which is what the JVM prints per frame and
;; what a reader needs; the record's own line is where the function was DEFINED and
;; is only the fallback.
;; The inline pass alpha-renames a spliced callee's binders, so a NAMED inner fn
;; inside a spliced body emits as `foo__il7` and Chez names its frame that. The
;; suffix is a compiler artifact -- `__il` plus the unit's fresh counter
;; (jolt.passes.inline/fresh) -- and nothing maps such a frame to a source record,
;; so it reaches the renderer as a bare name and used to print the mangled form
;; verbatim (jolt-pzos). Strip it back to what the user wrote.
;;
;; Only a trailing __il<digits> over a non-empty base is stripped, so a fn the user
;; actually named foo__il is left alone.
;;
;; A NAMED inner literal is bound under its image-registry label,
;; jfn$<ns>/<def>$<n>/<name> (backend fnsrc-name), so the registry has a key
;; that cannot collide. Shown as <ns>/<def>/<name>, the way clojure.stacktrace
;; demunges user$f$mapi__12: the counter is a compiler artifact. An anonymous
;; literal's label has one `/` and is shown as is. munge-chars never emits a
;; `/`, so the two slashes are the label's own.
;; The alpha-rename is stripped from the name part as well: a named literal
;; inside a spliced callee reaches here as jfn$…/step-boom__il22.
(define (srcreg-strip-il nm)
  (let ((n (string-length nm)))
    (let scan ((i n))
      (cond
        ;; walk back over the digit run
        ((and (fx>? i 0) (char<=? #\0 (string-ref nm (fx- i 1)) #\9)) (scan (fx- i 1)))
        ;; ...which must be non-empty, preceded by "__il", over a non-empty base
        ((and (fx<? i n) (fx>? i 4)
              (string=? (substring nm (fx- i 4) i) "__il"))
         (substring nm 0 (fx- i 4)))
        (else nm)))))
(define (srcreg-literal-display nm)
  (let ((n (string-length nm)))
    (and (fx>? n 4) (string=? (substring nm 0 4) "jfn$")
         (let* ((i1 (let find ((i 4)) (cond ((fx=? i n) #f)
                                             ((char=? (string-ref nm i) #\/) i)
                                             (else (find (fx+ i 1))))))
                (i2 (and i1 (let find ((i (fx- n 1))) (cond ((fx<=? i i1) #f)
                                                              ((char=? (string-ref nm i) #\/) i)
                                                              (else (find (fx- i 1))))))))
           (and i2
                (let* ((ns (substring nm 4 i1))
                       (mid (substring nm (fx+ i1 1) i2))
                       ;; <def>$<n>: drop the counter
                       (def (let scan ((i (string-length mid)))
                              (cond ((and (fx>? i 0) (char<=? #\0 (string-ref mid (fx- i 1)) #\9))
                                     (scan (fx- i 1)))
                                    ((and (fx>? i 0) (char=? (string-ref mid (fx- i 1)) #\$))
                                     (substring mid 0 (fx- i 1)))
                                    (else mid))))
                       (name (srcreg-strip-il (substring nm (fx+ i2 1) n))))
                  (string-append ns "/" (if (fx=? (string-length def) 0) "" (string-append def "/")) name)))))))
(define (srcreg-display-name nm)
  (or (srcreg-literal-display nm)
      (srcreg-strip-il nm)))

(define (srcreg-frame name record line) (vector name record line))
(define (srcreg-frame-nm f) (vector-ref f 0))
(define (srcreg-frame-rec f) (vector-ref f 1))
(define (srcreg-frame-line f) (vector-ref f 2))

(define (jolt-render-recs recs)
  (let ((port (open-output-string)))
    (let loop ((rs recs) (shown 0))
      (if (or (null? rs) (fx>=? shown 30))
          (get-output-string port)
          (let* ((f (car rs)) (frame-name (srcreg-frame-nm f)) (r (srcreg-frame-rec f)))
            ;; count a maximal run of the same frame-name
            (let run ((tail (cdr rs)) (cnt 1))
              (if (and (pair? tail) (string=? (srcreg-frame-nm (car tail)) frame-name))
                  (run (cdr tail) (fx+ cnt 1))
                  (begin
                    (put-string port "    ")
                    (if (vector? r)
                        (let* ((ns (vector-ref r 0)) (nm (vector-ref r 1))
                               (file (vector-ref r 2))
                               ;; the line reached in this frame, else where it was defined
                               (line (or (srcreg-frame-line f) (vector-ref r 3))))
                          (put-string port ns) (put-string port "/") (put-string port nm)
                          (when (string? file)
                            (put-string port " (") (put-string port (jolt-display-path file))
                            (put-string port ":") (put-string port (number->string line))
                            (put-string port ")")))
                        (put-string port (srcreg-display-name frame-name)))   ; 'ambiguous / unmapped: bare name
                    (when (fx>? cnt 1)
                      (put-string port " (x") (put-string port (number->string cnt)) (put-string port ")"))
                    (put-char port #\newline)
                    (loop tail (fx+ shown 1))))))))))

;; --- static chain reconstruction (R4, bead jolt-hm1p) ------------------------
;; The only runtime record tracing keeps is the throw-time site pair (rt.ss
;; jolt-throw-sitep). Erased tail chains are RECONSTRUCTED from the compile-
;; time callsite tables:
;;   backward: from the pair's fn, follow the single registered tail-entry
;;     edge (who tail-called it) outward — stopping at a fn the live
;;     continuation already reports (connected), a fork, a dynamic dead end,
;;     a cycle, or the cap. Incomplete is possible; invented is not, because
;;     only unambiguous edges are followed.
;;   forward: between two ADJACENT continuation frames, the outer frame's
;;     registered callee at its reached line names the fn its call created;
;;     when a UNIQUE single-tail-exit path leads from such a callee to the
;;     inner frame's fn, the fns on it are the TCO-erased frames between the
;;     two, spliced in deepest first.
;; Every entry is (fn . line) with the fn's OWN tail-site line — no shift.

(define jolt-chain-cap 16)

;; Render one (fn . line) entry as a LIST of frames — usually one.
;;
;; The cdr is the same shape a marker-table entry has: a line, or
;; #(line callee-fqn call-site-line) when the emitter marked the site as code the
;; inline pass copied out of another fn (backend_scheme sited-tail-call). A
;; spliced callee has no procedure of its own, so a tail site inside one would
;; otherwise be attributed to the fn it was spliced INTO and located at a line
;; that fn does not contain. Two frames then: the callee at the line reached in
;; it, then this fn at the call. Deepest first, matching the order the callers
;; splice these in.
(define (jolt-site-frame* site)
  (srcreg-entry-frames (car site)
                       (hashtable-ref source-registry (car site) #f)
                       (cdr site)))
;; (append-map jolt-site-frame* sites). Written out rather than pulled from
;; SRFI-1: (chezscheme) has no append-map, and every list here is capped
;; (jolt-chain-cap / the 30-frame render limit) so the naive shape is fine.
(define (srcreg-site-frames sites)
  (let loop ((ss sites) (acc '()))
    (if (null? ss)
        (reverse acc)
        (loop (cdr ss) (let add ((fs (jolt-site-frame* (car ss))) (a acc))
                         (if (null? fs) a (add (cdr fs) (cons (car fs) a))))))))

;; Backward walk from the throw-time pair: (values entries connected?), entries
;; innermost first starting with the pair itself.
(define (jolt-backwalk pair cont-names)
  (let loop ((fn (car pair)) (acc (list pair)) (seen (list (car pair))) (n 0))
    (let ((ents (and (fx<? n jolt-chain-cap) (jolt-callsite-tail-entries fn))))
      (if (or (not ents) (null? ents) (not (null? (cdr ents))))
          (values (reverse acc) #f)
          (let* ((e (car ents)) (caller (car e)) (line (cdr e)))
            (cond
              ((hashtable-ref cont-names caller #f) (values (reverse acc) #t))
              ((member caller seen) (values (reverse acc) #f))
              (else (loop caller (cons (cons caller line) acc)
                          (cons caller seen) (fx+ n 1)))))))))

;; Staleness validation (R2's rule over the walk): the pair's fn must be
;; absent from the live continuation, and is rejected only on POSITIVE
;; registered disagreement — the walk unconnected AND the innermost
;; continuation frame's registered callees naming none of the walked fns.
;; A returned tail site's residue fails exactly this; a genuine pair behind
;; a dynamic launcher survives via the no-evidence arm.
(define (jolt-site-valid? pair path connected? cont cont-names)
  (and (pair? pair)
       (not (hashtable-ref cont-names (car pair) #f))
       (or connected?
           (null? cont)
           ;; Two evidence tiers, because a CATCH context's frame can resolve
           ;; to an imprecise line (the guard's establishment point): the
           ;; precise line-keyed callees first, then the fn's callees at ANY
           ;; line. Reject only when the fn is registered and neither tier
           ;; names a walked fn — a returned tail site's residue fails exactly
           ;; that; a genuine chain behind an imprecise line survives the
           ;; fn-level tier.
           (let* ((ctx (car cont))
                  (fnm (srcreg-frame-nm ctx))
                  (path-hit? (lambda (expected)
                               (exists (lambda (p) (and (member (car p) expected) #t))
                                       path)))
                  (line-callees (jolt-callsite-callees fnm (srcreg-frame-line ctx)))
                  (fn-callees (jolt-callsite-fn-callees fnm)))
             (cond
               ((and line-callees (path-hit? line-callees)) #t)
               ((and fn-callees (path-hit? fn-callees)) #t)
               ((or line-callees fn-callees) #f)
               (else #t))))))

;; The positive half of jolt-site-valid?: the walk connected to a live frame, or
;; the innermost live frame's registered callees (at its line, else anywhere in
;; it) name a walked fn. No evidence is not enough here.
(define (jolt-site-evidenced? path connected? cont)
  (or connected?
      (and (pair? cont)
           (let* ((ctx (car cont))
                  (fnm (srcreg-frame-nm ctx))
                  (hit? (lambda (expected)
                          (and expected
                               (exists (lambda (p) (and (member (car p) expected) #t)) path)))))
             (or (hit? (jolt-callsite-callees fnm (srcreg-frame-line ctx)))
                 (hit? (jolt-callsite-fn-callees fnm)))))))

;; A live read's pair stands for an erased frame only while its tail call is still
;; running, and spliced innermost it claims that call reached the stack read with
;; no traced frame in between. When the pair's own site registers a static tail
;; callee, the call went there instead: had that callee still been running it would
;; be a live frame, or it would have stored a pair of its own on its way out. Either
;; way the pair is a returned call's residue, whatever the live frames' callee lists
;; say — they only show the pair's chain is CALLED somewhere, not that it still is.
;; A dynamic tail call (a fn value) is registered by line alone and counts the same.
;; A tail call through a host method (the stack read itself) registers nothing.
(define (jolt-site-exited? site)
  (let ((line (jolt-marker-entry-line (cdr site)))
        (exits (jolt-callsite-tail-exits (car site)))
        (dyn (jolt-callsite-dynamic-tail-lines (car site))))
    (or (and (pair? exits) (exists (lambda (e) (eqv? (car e) line)) exits))
        (and (pair? dyn) (memv line dyn) #t))))

;; Forward path from callee `start` through single tail exits until an exit
;; reaches `target`; returns the erased (fn . exit-line) entries DEEPEST
;; first, '() when start IS the target (nothing erased), #f when no
;; unambiguous path exists.
(define (jolt-forward-path start target)
  (if (equal? start target)
      '()
      (let loop ((fn start) (acc '()) (seen '()) (n 0))
        (let ((exits (and (fx<? n 8)
                          (not (member fn seen))
                          (jolt-callsite-tail-exits fn))))
          (if (or (not exits) (null? exits) (not (null? (cdr exits))))
              #f
              (let* ((e (car exits)) (l (car e)) (next (cdr e))
                     (acc (cons (cons fn l) acc)))
                (if (equal? next target)
                    acc
                    (loop next acc (cons fn seen) (fx+ n 1)))))))))

;; Splice reconstructed gaps between adjacent continuation frames. The gap for
;; a frame pair is inserted only when exactly ONE registered callee of the
;; outer frame's site yields a path to the inner frame's fn — ambiguity means
;; no insert, never a guess.
;;
;; A live stack (Thread.getStackTrace) can also bridge a gap with the most recent
;; tail-site pair, `site`: when no static path reaches the inner frame — the
;; erased fn's tail call went through apply or a fn value — but exactly one path
;; reaches the pair's fn, that fn is the erased caller of the inner frame. The
;; pair is used at most once. Returns (values frames site-used?).
(define (jolt-fill-gaps cont site)
  (if (or (null? cont) (null? (cdr cont)))
      (values cont #f)
      (let loop ((inner (car cont)) (rest (cdr cont)) (acc (list (car cont))) (site site) (used? #f))
        (if (null? rest)
            (values (reverse acc) used?)
            (let* ((outer (car rest))
                   (onm (srcreg-frame-nm outer)) (oln (srcreg-frame-line outer))
                   (inm (srcreg-frame-nm inner))
                   (cands (and (fixnum? oln) (fx>? oln 0)
                               (jolt-callsite-callees onm oln)))
                   (paths-to (lambda (target)
                               (if cands
                                   (filter (lambda (x) x)
                                           (map (lambda (c) (jolt-forward-path c target)) cands))
                                   '())))
                   (paths (paths-to inm))
                   (site-paths (if (and site (null? paths)) (paths-to (car site)) '()))
                   (bridge? (and (pair? site-paths) (null? (cdr site-paths))))
                   (gap (cond ((and (pair? paths) (null? (cdr paths))) (car paths))
                              (bridge? (cons site (car site-paths)))
                              (else '()))))
              (loop outer (cdr rest)
                    (cons outer (append (srcreg-site-frames (reverse gap)) acc))
                    (if bridge? #f site)
                    (or used? bridge?)))))))

;; The no-continuation / REPL fallback: the pair plus its backward walk is all
;; there is (a REPL's own continuation is just its machinery). #f when tracing
;; is off / nothing was stashed, so a caller can when-let.
(define (jolt-history-backtrace)
  (let ((site (jolt-throw-site)))
    (and (pair? site)
         (let ((none (make-hashtable string-hash string=?)))
           (call-with-values (lambda () (jolt-backwalk site none))
             (lambda (path connected?)
               (let ((recs (srcreg-site-frames path)))
                 (and (pair? recs) (jolt-render-recs recs)))))))))

;; The frames a continuation stands for, innermost first: the live continuation
;; is the spine; the most recent tail-site pair (validated) plus its backward walk
;; recovers the innermost erased chain, and forward gap-fills recover erased
;; frames between live ones. All reconstruction is from compile-time tables — the
;; runtime records exactly one pair.
;; live? is a stack read in place rather than at a throw: the pair may then
;; belong to a caller outside the live frames, so it may bridge a gap first.
(define (jolt-continuation-recs k live?)
  (jolt-continuation-recs/site k live?
    ;; a throw reads the raise-time snapshot; a live read takes the slot as
    ;; it stands, the most recent tail call made on this thread
    (if live?
        (let ((s (virtual-register jolt-vreg-site))) (and (pair? s) s))
        (jolt-throw-site))))
;; ...with the site pair given: a caught throwable's own (jolt-throwable-recs).
(define (jolt-continuation-recs/site k live? site0)
  (let* ((cont (jolt-frame-records k))
         (cont-names (let ((h (make-hashtable string-hash string=?)))
                       (for-each (lambda (f)
                                   (hashtable-set! h (srcreg-frame-nm f) #t))
                                 cont)
                       h))
         (site (and (pair? site0) (not (hashtable-ref cont-names (car site0) #f)) site0)))
    (call-with-values (lambda () (jolt-fill-gaps cont (and live? site)))
      (lambda (body used?)
        ;; At a throw the pair is the innermost call, so it splices in innermost
        ;; unless registered evidence contradicts it. A live read's pair can be
        ;; residue from anywhere along the stack, so there it splices in only on
        ;; positive evidence: the walk reaches a live frame, or the innermost
        ;; live frame's registered callees name a walked fn, and the pair's own
        ;; tail call has not visibly gone somewhere else (jolt-site-exited?).
        (if (and site (not used?))
            (call-with-values (lambda () (jolt-backwalk site cont-names))
              (lambda (path connected?)
                (if (if live?
                        (and (not (jolt-site-exited? site))
                             (jolt-site-evidenced? path connected? cont))
                        (jolt-site-valid? site path connected? cont cont-names))
                    (append (srcreg-site-frames path) body)
                    body)))
            body)))))

;; Multi-line backtrace for an uncaught value.
(define (jolt-backtrace-string v)
  (let ((k (jolt-error-continuation v)))
    (if (not k)
        (jolt-history-backtrace)
        (let ((recs (jolt-continuation-recs k #f)))
          (and (pair? recs) (jolt-render-recs recs))))))

;; --- java.lang.StackTraceElement ------------------------------------------------
;; A frame as the JVM names it: class ns$fn (munged like Compiler/munge), method
;; invoke, the source file's base name, and the line reached in the frame. Only
;; frames that map to registered Clojure source become elements; an unmapped or
;; ambiguous frame would have to be invented, so it is left out.
(define (ste-make cls method file line)
  (make-jhost "stack-trace-element" (vector cls method file line)))
(define (ste? x) (and (jhost? x) (string=? (jhost-tag x) "stack-trace-element")))
(define (ste-field x i) (vector-ref (jhost-state x) i))

(define (ste-base-name path)
  (let loop ((i (fx- (string-length path) 1)))
    (cond ((fx<? i 0) path)
          ((memv (string-ref path i) '(#\/ #\\)) (substring path (fx+ i 1) (string-length path)))
          (else (loop (fx- i 1))))))

(define (ste-of-frame f)
  (let ((r (srcreg-frame-rec f))
        (file-of (lambda (file) (if (string? file) (ste-base-name file) jolt-nil)))
        (cls (lambda (ns nm) (string-append (class-munge-name ns) "$" (class-munge-name nm)))))
    (cond
      ((vector? r)
       (let ((line (or (srcreg-frame-line f) (vector-ref r 3))))
         (ste-make (cls (vector-ref r 0) (vector-ref r 1)) "invoke"
                   (file-of (vector-ref r 2)) (if (fixnum? line) line -1))))
      ((srcreg-jfn-parts (srcreg-frame-nm f))
       => (lambda (p)
            (let ((rec (srcreg-record-for-fqn (string-append (car p) "/" (cadr p))))
                  (line (srcreg-frame-line f)))
              (ste-make (string-append (cls (car p) (cadr p)) "$fn__" (caddr p)) "invoke"
                        (file-of (vector-ref rec 2)) (if (fixnum? line) line -1)))))
      (else #f))))

;; A throwable's own frames as a StackTraceElement array, or an empty one for a
;; throwable that was never thrown (the JVM fills one in at construction; jolt
;; has no frames until the throw captures them).
;;
;; From its OWN capture (rt.ss jolt-thrown-cont: the continuation and the site pair
;; of its throw), rendered as a throw is: the live spine, and the frames a tail
;; call erased recovered from that site. Not from jolt-error-continuation and the
;; thread's site snapshot, which belong to whatever threw last on this thread.
(define (jolt-throwable-recs v)
  (let ((cap (jolt-thrown-cont (jolt-unwrap-throw v))))
    (and cap (guard (e (#t #f)) (jolt-continuation-recs/site (car cap) #f (cdr cap))))))
(define (jolt-throwable-stack-trace v)
  (let ((recs (jolt-throwable-recs v)))
    (if recs (apply jolt-vector (jolt-recs->stack-trace recs)) (jolt-vector))))
;; printStackTrace's frame lines: the same capture, rendered like a report.
(define (jolt-throwable-backtrace-string v)
  (let ((recs (jolt-throwable-recs v)))
    (if (pair? recs) (jolt-render-recs recs) (jolt-backtrace-string v))))

(define (jolt-stack-trace-list k)
  (guard (e (#t '()))
    (jolt-recs->stack-trace (jolt-continuation-recs k #t))))
(define (jolt-recs->stack-trace recs)
  (let loop ((fs recs) (acc '()))
    (cond ((null? fs) (reverse acc))
          ((ste-of-frame (car fs)) => (lambda (e) (loop (cdr fs) (cons e acc))))
          (else (loop (cdr fs) acc)))))

;; Thread.getStackTrace on the calling thread: its own frame first, as on the JVM,
;; then the caller's frames.
(define (jolt-current-stack-trace)
  (call/cc
   (lambda (k)
     (apply jolt-vector
            (ste-make "java.lang.Thread" "getStackTrace" "Thread.java" -1)
            (jolt-stack-trace-list k)))))

(define (ste-string x)
  (let ((file (ste-field x 2)) (line (ste-field x 3)))
    (string-append (ste-field x 0) "." (ste-field x 1) "("
                   (cond ((not (string? file)) "Unknown Source")
                         ((fx>=? line 0) (string-append file ":" (number->string line)))
                         (else file))
                   ")")))

(register-host-methods! "stack-trace-element"
  (list (cons "getClassName"  (lambda (self) (ste-field self 0)))
        (cons "getMethodName" (lambda (self) (ste-field self 1)))
        (cons "getFileName"   (lambda (self) (ste-field self 2)))
        (cons "getLineNumber" (lambda (self) (ste-field self 3)))
        (cons "isNativeMethod" (lambda (self) #f))
        (cons "toString"      (lambda (self) (ste-string self)))
        (cons "hashCode"      (lambda (self) (equal-hash (jhost-state self))))
        (cons "equals"        (lambda (self o) (and (ste? o) (equal? (jhost-state self) (jhost-state o)))))))
(register-str-render! ste? ste-string)
(register-eq-arm! (lambda (a b) (and (ste? a) (ste? b)))
                  (lambda (a b) (equal? (jhost-state a) (jhost-state b))))
(register-hash-arm! ste? (lambda (x) (equal-hash (jhost-state x))))
(register-class-arm! ste? (lambda (x) "java.lang.StackTraceElement"))
(for-each (lambda (nm)
            (register-class-ctor! nm (lambda (cls method file line) (ste-make cls method file line))))
          '("StackTraceElement" "java.lang.StackTraceElement"))


;; Exposed for the REPL / nREPL error paths, which catch errors themselves instead
;; of going through the uncaught reporter. Returns the "  trace:\n<frames>" block
;; from the tail-frame HISTORY only — the live continuation in a REPL is just the
;; REPL's own machinery — or nil when tracing is off (so a caller can when-let).
(def-var! "jolt.host" "backtrace-string"
  (lambda ()
    (let ((bt (jolt-history-backtrace)))
      (if bt (string-append "  trace:\n" bt) jolt-nil))))

;; A diagnostic's own keys all live in the "jolt.error" namespace — and, since
;; the same diagnostic also carries the reference's spelling of its phase and
;; position for clojure.main/ex-triage, in "clojure.error" — so the report
;; hides them by NAMESPACE rather than by naming each one. The list this
;; replaces had to be edited every time a diagnostic gained a field, and would
;; silently start leaking machinery into the user's ex-data the first time
;; someone forgot.
(define (srcreg-jolt-error-key? k)
  (and (keyword-t? k)
       (let ((ns (keyword-t-ns k)))
         (and (string? ns)
              (or (string=? ns "jolt.error") (string=? ns "clojure.error"))))))

;; The thrower's own ex-data, with jolt's diagnostic keys taken out; jolt-nil
;; when there was none. Both report shapes go through this — the plain one below
;; and the framed one in cli-core.ss — so a macro that raises
;; (ex-info "bad schema" {:schema …}) while a form is being analyzed shows that
;; map exactly as an uncaught runtime ex-info does.
(define (jolt-user-ex-data v)
  (let ((data (jolt-ex-info-record-data v)))
    (if (pmap? data)
        (jolt-reduce-kv
         (lambda (acc k _)
           (if (srcreg-jolt-error-key? k) (jolt-dissoc acc k) acc))
         data data)
        data)))

;; Render an uncaught jolt throw (any value, not just a Chez condition) to a port:
;; an ex-info shows its message + ex-data (+ a host cause); anything else is
;; pr-str'd. Shared by the cli (cli.ss) and a built binary's launcher (build.ss).
(define (jolt-render-throwable raw port)
  (let ((v (jolt-unwrap-throw raw)))
    (if (jolt-ex-info-record? v)
        (begin
          ;; The class is the headline when it says something: shown for a
          ;; typed throwable (a host fault, an ArithmeticException), omitted for
          ;; an ExceptionInfo — its message and ex-data carry the report — and
          ;; for a bare java.lang.Exception, as the reference does.
          (display "Unhandled exception" port)
          (let ((cn (jolt-ex-info-record-class-name v)))
            (unless (member cn '("clojure.lang.ExceptionInfo" "java.lang.Exception"))
              (display " (" port) (display (last-dot cn) port) (display ")" port)))
          (display ": " port)
          (display (jolt-str-render-one (jolt-ex-info-record-message v)) port)
          (newline port)
          ;; The USER's ex-data, never jolt's. A diagnostic's position and kind
          ;; live in the :jolt.error namespace and are machinery: printing them
          ;; put the position in front of a reader who is told it again on the
          ;; next line, and BURIED the data a throwing macro actually attached —
          ;; (ex-info "m" {:orig true}) reported jolt's keys and no :orig.
          ;; JOLT_DIAG=edn is where the structured form belongs, and it still
          ;; emits it (cli-core.ss).
          (let* ((data0 (jolt-ex-info-record-data v))
                 (data (jolt-user-ex-data v)))
            (unless (or (jolt-nil? data)
                        ;; A map left empty only because jolt's own keys came out
                        ;; says nothing, so the line goes away. A thrower's own
                        ;; empty map still prints, which is what the reference
                        ;; shows for (ex-info "m" {}).
                        (and (pmap? data) (= 0 (jolt-count data))
                             (> (jolt-count data0) 0)))
              (display "  ex-data: " port) (display (jolt-pr-str data) port) (newline port)))
          (let ((cause (jolt-ex-info-record-cause v)))
            (when (condition? cause)
              (display "  cause: " port)
              (display (with-output-to-string (lambda () (display-condition cause))) port)
              (newline port))))
        (begin
          (display "Unhandled exception: " port)
          (display (if (condition? v) (with-output-to-string (lambda () (display-condition v))) (jolt-pr-str v)) port)
          (newline port)))))

;; Render the throwable, then its Clojure backtrace when one maps. The caller adds
;; any top-level source location (the runtime cli does; a built binary has none).
(define (jolt-report-throwable v port)
  (jolt-render-throwable v port)
  (let ((bt (jolt-backtrace-string v)))
    (when bt (display "  trace:\n" port) (display bt port))))

;; ---- #error print form (pr/pr-str) and toString (str) for ex-info records ----

;; Walk the cause chain of a jolt-ex-info-record, returning a list of cause entries
;; (innermost first). Each entry is a vector [class-name message data].
(define (jolt-error-via-chain rec)
  (let loop ((r rec) (acc '()))
    (let ((class-name (jolt-ex-info-record-class-name r))
          (msg (jolt-ex-info-record-message r))
          (data (jolt-ex-info-record-data r))
          (cause (jolt-ex-info-record-cause r)))
      (let ((new-acc (cons (vector class-name msg data) acc)))
        (if (jolt-ex-info-record? cause)
            (loop cause new-acc)
            (reverse new-acc))))))

;; Render a single :via entry as a string map.
(define (jolt-error-render-via-entry entry)
  (let* ((class-name (vector-ref entry 0))
         (msg (vector-ref entry 1))
         (data (vector-ref entry 2))
         (type-str (jolt-pr-str (jolt-symbol #f class-name)))
         (msg-str (jolt-pr-readable msg))
         (has-data (not (jolt-nil? data))))
    (string-append " {:type " type-str
                   " :message " msg-str
                   (if has-data (string-append " :data " (jolt-pr-readable data)) "")
                   " :at nil}")))

;; Render the :via chain list.
(define (jolt-error-render-via chain)
  (string-append "[" (jolt-str-join (map jolt-error-render-via-entry chain)) "]"))

;; #error print form for jolt-ex-info-records (pr/pr-str). Matches JVM shape:
;; #error {:cause <root-msg> :data {...} :via [{...}] :trace [[...]...]}
;; :trace is always [] here — frame records are only accessible during a throw.
(register-pr-arm! jolt-ex-info-record?
  (lambda (x)
    (let* ((chain (jolt-error-via-chain x))
           (root-cause (vector-ref (car (reverse chain)) 1))
           (data (jolt-ex-info-record-data x))
           (via-str (jolt-error-render-via chain)))
      (string-append "#error {\n :cause " (jolt-pr-readable root-cause)
                     (if (jolt-nil? data) "" (string-append "\n :data " (jolt-pr-readable data)))
                     "\n :via " via-str
                     "\n :trace []"
                     "}"))))

;; toString (str/print) for throwable records: Throwable's "ClassName: message",
;; the ": message" dropped when there is none. Only ExceptionInfo's own toString
;; appends its data. A throwable of another class can carry data here too — a
;; read error keeps its diagnostic position on its RuntimeException — and
;; printing that into its toString broke the text a caller builds from it, the
;; ReaderException's message first among them.
(register-str-render! jolt-ex-info-record?
  (lambda (x)
    (let* ((class-name (jolt-ex-info-record-class-name x))
           (msg (jolt-ex-info-record-message x))
           (data (jolt-ex-info-record-data x)))
      (string-append class-name
                     (if (jolt-nil? msg) "" (string-append ": " (jolt-str-render-one msg)))
                     (if (or (jolt-nil? data) (not (equal? class-name "clojure.lang.ExceptionInfo")))
                         ""
                         (string-append " " (jolt-pr-str data)))))))

;; count on ex-info / host throwable records throws UnsupportedOperationException
;; matching JVM: "count not supported on this type: <SimpleClassName>"
(register-count-arm! jolt-ex-info-record?
  (lambda (coll)
    (jolt-throw (jolt-host-throwable "java.lang.UnsupportedOperationException"
                   (string-append "count not supported on this type: "
                                  (simple-class-name (jolt-ex-info-record-class-name coll)))))))
