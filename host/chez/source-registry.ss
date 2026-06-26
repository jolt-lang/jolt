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
;; &continuation, else #f.
(define (jolt-error-continuation v)
  (let ((tc (jolt-throw-cont)))
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

;; Walk a continuation, returning the registered jolt frames (innermost first) as
;; (frame-name . record) pairs, where record is #(ns name file line) or the symbol
;; 'ambiguous. Unmapped frames (host spine, anonymous lambdas) are skipped; raw
;; depth is capped.
(define (jolt-frame-records k)
  ;; read the env at call time, not load time: a built binary runs top-level forms
  ;; at heap-build time, where this would always be unset.
  (let ((debug? (getenv "JOLT_DEBUG_FRAMES")))
   (guard (e (#t '()))
    (let loop ((io (inspect/object k)) (n 0) (acc '()))
      (if (or (not io) (fx>=? n 400))
          (reverse acc)
          (let* ((nm (srcreg-frame-name io))
                 (src (and nm (hashtable-ref source-registry nm #f))))
            (when (and debug? nm)
              (display (string-append "  [frame] " nm (if src " *MAPPED*" "") "\n")
                       (current-error-port)))
            (loop (guard (e (#t #f)) (io 'link)) (fx+ n 1)
                  (if src (cons (cons nm src) acc) acc))))))))

;; Multi-line backtrace for an uncaught value — "  ns/name (file:line)" for a
;; mapped frame, the bare frame name for an ambiguous one — or #f when no jolt
;; frame maps (the caller then prints just the top-level location). Capped to the
;; innermost frames.
(define (jolt-backtrace-string v)
  (let ((k (jolt-error-continuation v)))
    (and k
         (let ((recs (jolt-frame-records k)))
           (and (pair? recs)
                (let ((port (open-output-string)))
                  (let loop ((rs recs) (shown 0))
                    (when (and (pair? rs) (fx<? shown 30))
                      (let* ((p (car rs)) (frame-name (car p)) (r (cdr p)))
                        (put-string port "    ")
                        (if (vector? r)
                            (let ((ns (vector-ref r 0)) (nm (vector-ref r 1))
                                  (file (vector-ref r 2)) (line (vector-ref r 3)))
                              (put-string port ns) (put-string port "/") (put-string port nm)
                              (when (string? file)
                                (put-string port " (") (put-string port file)
                                (put-string port ":") (put-string port (number->string line))
                                (put-string port ")")))
                            (put-string port frame-name))   ; 'ambiguous: bare name
                        (put-char port #\newline))
                      (loop (cdr rs) (fx+ shown 1))))
                  (get-output-string port)))))))

;; Render an uncaught jolt throw (any value, not just a Chez condition) to a port:
;; an ex-info shows its message + ex-data (+ a host cause); anything else is
;; pr-str'd. Shared by the cli (cli.ss) and a built binary's launcher (build.ss).
(define (jolt-render-throwable v port)
  (if (jolt=2 (jolt-get v jolt-kw-ex-type jolt-nil) jolt-kw-ex-info)
      (begin
        (display "Unhandled exception: " port)
        (display (jolt-str-render-one (jolt-get v jolt-kw-message jolt-nil)) port)
        (newline port)
        (let ((data (jolt-get v jolt-kw-data jolt-nil)))
          (unless (jolt-nil? data)
            (display "  ex-data: " port) (display (jolt-pr-str data) port) (newline port)))
        (let ((cause (jolt-get v jolt-kw-cause jolt-nil)))
          (when (condition? cause)
            (display "  cause: " port)
            (display (with-output-to-string (lambda () (display-condition cause))) port)
            (newline port))))
      (begin
        (display "Unhandled exception: " port)
        (display (if (condition? v) (with-output-to-string (lambda () (display-condition v))) (jolt-pr-str v)) port)
        (newline port))))

;; Render the throwable, then its Clojure backtrace when one maps. The caller adds
;; any top-level source location (the runtime cli does; a built binary has none).
(define (jolt-report-throwable v port)
  (jolt-render-throwable v port)
  (let ((bt (jolt-backtrace-string v)))
    (when bt (display "  trace:\n" port) (display bt port))))
