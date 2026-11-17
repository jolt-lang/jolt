;; run-traceeval.ss — trace-r2: an eval-path (AOT cache MISS) frame carries a
;; source object, and the frame's (source-name . offset) resolves to the ORIGINAL
;; clj line through the eval marker registry. With tracing off, the eval path
;; registers nothing (the off branch is today's plain read+eval expression).
;;
;;   chez --script host/chez/run-traceeval.ss
(load "host/chez/run-gate-harness.ss")

;; The emitter must put #|L<line>|# markers in the emitted text, or there is
;; nothing for the registry to resolve against — tracing ON throughout.
(define set-trace-frames! (var-deref "jolt.backend-scheme" "set-trace-frames!"))
(set-trace-frames! #t)
(gate-check "trace frames on from the start" (jolt-ce-trace-frames?) #t)
;; JOLT_KEEP_FRAMES=1 on a Chez with patch 0012 runs the gate over keep-frames
;; emission (session.clj keep-frames), as jolt-enable-trace! would
(define keep-frames? (jolt-keep-frames-enabled?))
(when keep-frames?
  ((var-deref "jolt.backend-scheme.session" "set-keep-frames!") #t)
  (jolt-keep-frames-set! #t))

;; --- (1) an eval-path frame carries a source object and resolves ----------------
;; The source string, with the call sites on known clj lines:
;;   line 1: (defn boom [x]
;;   line 2:   (if (pos? x)
;;   line 3:     (+ 0 (boom (dec x)))               <- recursion: boom(0)'s frame
;;   line 4:     (throw (ex-info "boom" {:x x}))
;;   line 5: (defn caller [y] (+ 1 (boom y))       <- caller's (boom y) call
;; boom recurses NON-tail (inside +), so Chez cannot inline it: boom(0) gets its
;; own frame. Chez annotates a frame with the position of the call that CREATED
;; it, so the innermost frame at the throw resolves to line 3 (the recursive
;; (boom (dec x)) call), not line 4 (the throw itself — the jolt-throw
;; scaffolding carries no annotation of its own).
(define src
  "(defn boom [x]
  (if (pos? x)
      (+ 0 (boom (dec x)))
      (throw (ex-info \"boom\" {:x x}))))
(defn caller [y] (+ 1 (boom y)))
")
(jolt-load-string src)
;; the throw escapes jolt-compile-eval of a fresh form into this guard
(define boom-k
  (guard (e (#t (jolt-error-continuation e)))
    (jolt-compile-eval "(caller 1)" "user")))
(gate-check "(1) a throw was captured" (not (eq? boom-k #f)) #t)

;; walk the live continuation, collecting each frame's (source-name . offset)
(define (walk-source-pairs k)
  (guard (e (#t '()))
    (let loop ((ios (sa-continuation-frames k)) (acc '()))
      (if (null? ios)
          (reverse acc)
          (loop (cdr ios)
                (let ((sp (srcreg-frame-source-pair (car ios))))
                  (if sp (cons sp acc) acc)))))))
(define paths (walk-source-pairs boom-k))
;; every source object on the eval path names a registry key, never a file
(define reg-paths
  (filter (lambda (sp) (and (pair? sp) (jolt-eval-source-name? (car sp)))) paths))
(gate-check "(1) at least one frame carries a source object" (pair? reg-paths) #t)
;; the innermost such frame is boom(0)'s, created by the recursive call on
;; clj line 3
(define first-line
  (let loop ((rs reg-paths))
    (and (pair? rs)
         (or (jolt-eval-source-line (car (car rs)) (cdr (car rs)))
             (loop (cdr rs))))))
;; Under keep-frames boom(0) keeps its own frame across the throw in its tail,
;; so the innermost frame is boom(0) waiting on the throw, on line 4.
(gate-check "(1) innermost eval frame resolves to the throwing call site" first-line
            (if keep-frames? 4 3))
;; every registry frame resolves to a line from the source (1..5), never a
;; fabricated one
(define bad-lines
  (filter (lambda (sp)
            (let ((l (jolt-eval-source-line (car sp) (cdr sp))))
              (and l (not (and (<= 1 l) (<= l 5))))))
          reg-paths))
(gate-check "(1) every frame resolves to a real source line" bad-lines '())
;; an offset for a name that was never registered resolves to #f
(gate-check "(1) unknown name resolves to #f"
            (jolt-eval-source-line "jolt-eval-src-999999" 0) #f)

;; --- (2) tracing OFF: the eval path registers nothing --------------------------
;; The annotated read is gated on the SAME flag the emitter gates markers on;
;; with it off, compiling+evalling a form must not touch the registry.
(define reg-before (hashtable-size jolt-eval-marker-registry))
(set-trace-frames! #f)
(gate-check "(2) trace frames off is visible" (jolt-ce-trace-frames?) #f)
(gate-check "(2) plain eval still works" (jolt-compile-eval "(+ 1 2)" "user") 3)
(gate-check "(2) no registry growth with tracing off"
            (hashtable-size jolt-eval-marker-registry) reg-before)
;; ...and the traced run DID grow it, so the check above is not vacuous
(gate-check "(2) tracing grew the registry earlier" (fx>? reg-before 0) #t)

(gate-summary "trace-eval")
