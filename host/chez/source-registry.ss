;; source-registry.ss — map emitted procedures back to Clojure source for native
;; stack traces, and render an uncaught throwable.
;;
;; A direct-linked def compiles to (define jv$ns$name <fn>); the back end also
;; emits (jolt-register-source! "jv$ns$name" ns name file line) once per such def
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
;; jv$ns$name global. Two vars in different namespaces can share a short name; an
;; 'ambiguous marker then keeps the frame name in the trace but drops the
;; (now-uncertain) ns/file:line, so a trace is never misattributed.
(define source-registry (make-hashtable string-hash string=?))

(define (jolt-register-source! procname ns nm file line)
  (let ((existing (hashtable-ref source-registry procname #f)))
    (cond
      ((not existing) (hashtable-set! source-registry procname (vector ns nm file line)))
      ((and (vector? existing)
            (or (not (equal? (vector-ref existing 0) ns))
                (not (equal? (vector-ref existing 1) nm))))
       (hashtable-set! source-registry procname 'ambiguous))))
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
      (else #f))))

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
                "list->cseq" "host-static-call" "host-call"))
    h))
(define (srcreg-plumbing-name? nm)
  (or (hashtable-ref srcreg-plumbing-names nm #f)
      (and (fx>? (string-length nm) 0) (char=? (string-ref nm 0) #\$))
      (and (fx>=? (string-length nm) 5) (string=? (substring nm 0 5) "jolt-"))))

;; Walk a continuation, returning its frames (innermost first) as (frame-name .
;; record) pairs. record is a source vector #(ns name file line) for a frame that
;; maps to registered Clojure source, the symbol 'ambiguous for a short name shared
;; across namespaces, or #f for an unmapped-but-named frame (the common case on the
;; open-world eval path, where nothing is registered — the bare frame name is still
;; a useful trace line). Plumbing frames (host spine, eval boundary) and unnamed
;; frames are skipped; raw depth is capped.
(define (jolt-frame-records k)
  ;; read the env at call time, not load time: a built binary runs top-level forms
  ;; at heap-build time, where this would always be unset.
  (let ((debug? (getenv "JOLT_DEBUG_FRAMES")))
   (guard (e (#t '()))
    (let loop ((io (inspect/object k)) (n 0) (acc '()))
      (if (or (not io) (fx>=? n 400))
          (reverse acc)
          (let* ((nm (srcreg-frame-name io))
                 (src (and nm (hashtable-ref source-registry nm #f)))
                 ;; keep a frame that maps, or any named frame that isn't plumbing
                 (keep? (and nm (or src (not (srcreg-plumbing-name? nm))))))
            (when (and debug? nm)
              (display (string-append "  [frame] " nm (if src " *MAPPED*"
                                                          (if keep? "" " (skipped)")) "\n")
                       (current-error-port)))
            (loop (guard (e (#t #f)) (io 'link)) (fx+ n 1)
                  (if keep? (cons (cons nm src) acc) acc))))))))

;; Render a list of (frame-name . record) pairs (innermost/deepest first) to a
;; backtrace string. record is a source vector #(ns name file line) -> "ns/name
;; (file:line)", or 'ambiguous / #f -> the bare frame name. A run of the same
;; frame-name collapses to one "name (xN)" line (deep recursion, or a hot fn a
;; loop re-enters), and the number of distinct lines is capped.
(define (jolt-render-recs recs)
  (let ((port (open-output-string)))
    (let loop ((rs recs) (shown 0))
      (if (or (null? rs) (fx>=? shown 30))
          (get-output-string port)
          (let* ((p (car rs)) (frame-name (car p)) (r (cdr p)))
            ;; count a maximal run of the same frame-name
            (let run ((tail (cdr rs)) (cnt 1))
              (if (and (pair? tail) (string=? (car (car tail)) frame-name))
                  (run (cdr tail) (fx+ cnt 1))
                  (begin
                    (put-string port "    ")
                    (if (vector? r)
                        (let ((ns (vector-ref r 0)) (nm (vector-ref r 1))
                              (file (vector-ref r 2)) (line (vector-ref r 3)))
                          (put-string port ns) (put-string port "/") (put-string port nm)
                          (when (string? file)
                            (put-string port " (") (put-string port file)
                            (put-string port ":") (put-string port (number->string line))
                            (put-string port ")")))
                        (put-string port frame-name))   ; 'ambiguous / unmapped: bare name
                    (when (fx>? cnt 1)
                      (put-string port " (x") (put-string port (number->string cnt)) (put-string port ")"))
                    (put-char port #\newline)
                    (loop tail (fx+ shown 1))))))))))

;; Multi-line backtrace for an uncaught value. Two sources, in preference order:
;;   1. The tail-frame history ring (rt.ss), when JOLT_TRACE enabled it — an
;;      execution history of the runtime-compiled fns entered before the throw,
;;      INCLUDING ones TCO erased from the live continuation. Most-recent first.
;;   2. Otherwise the live continuation (jolt-frame-records) — the accurate but
;;      TCO-truncated non-tail spine.
;; Each frame maps to "ns/name (file:line)" when registered, else its bare name.
;; #f when neither source yields a frame (the caller then prints just the location).
;; The tail-frame history ring rendered as a backtrace, or #f when tracing is off /
;; empty. A mapped frame is kept; else drop plumbing (same rule as the continuation
;; path) so the two sources read consistently.
(define (jolt-history-backtrace)
  (let* ((hist (jolt-trace-snapshot))
         (recs (let loop ((ns hist) (acc '()))
                 (if (null? ns)
                     (reverse acc)
                     (let* ((nm (car ns)) (src (hashtable-ref source-registry nm #f)))
                       (loop (cdr ns)
                             (if (or src (not (srcreg-plumbing-name? nm)))
                                 (cons (cons nm src) acc) acc)))))))
    (and (pair? recs) (jolt-render-recs recs))))

(define (jolt-backtrace-string v)
  (or (jolt-history-backtrace)
      (let ((k (jolt-error-continuation v)))
        (and k
             (let ((recs (jolt-frame-records k)))
               (and (pair? recs) (jolt-render-recs recs)))))))

;; Exposed for the REPL / nREPL error paths, which catch errors themselves instead
;; of going through the uncaught reporter. Returns the "  trace:\n<frames>" block
;; from the tail-frame HISTORY only — the live continuation in a REPL is just the
;; REPL's own machinery — or nil when tracing is off (so a caller can when-let).
(def-var! "jolt.host" "backtrace-string"
  (lambda ()
    (let ((bt (jolt-history-backtrace)))
      (if bt (string-append "  trace:\n" bt) jolt-nil))))

;; Render an uncaught jolt throw (any value, not just a Chez condition) to a port:
;; an ex-info shows its message + ex-data (+ a host cause); anything else is
;; pr-str'd. Shared by the cli (cli.ss) and a built binary's launcher (build.ss).
(define (jolt-render-throwable raw port)
  (let ((v (jolt-unwrap-throw raw)))
    (if (jolt-ex-info-record? v)
        (begin
          (display "Unhandled exception: " port)
          (display (jolt-str-render-one (jolt-ex-info-record-message v)) port)
          (newline port)
          (let ((data (jolt-ex-info-record-data v)))
            (unless (jolt-nil? data)
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
