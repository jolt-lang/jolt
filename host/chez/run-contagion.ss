;; run-contagion.ss — devirt-gated fl* contagion for :num record fields.
;;
;; A :num field (an integer/mixed ctor-arg join) reads :any under Option A, so
;; flonum arithmetic over it stays generic — the win Option B gave up to fix the
;; megamorphic regression. This gate pins the mechanism that recovers it, gated to
;; devirtualized call sites: contagion-specialize-arity builds a clone of an impl
;; body whose :num field reads contagion-coerce (exact->inexact) beside a proven
;; :double operand, lowering to fl*. The invariant: contagion fires ONLY where at
;; least one operand is proven :double — a pure-:num expression stays generic.
;; Stage 1 pins the types API and the runtime specialized clone registry.
;;
;;   chez --script host/chez/run-contagion.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer! (var-deref "jolt.passes.types" "wp-infer!"))
(define contagion-specialize-arity (var-deref "jolt.passes.types" "contagion-specialize-arity"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
(define (kw n) (keyword #f n))
(define (sub? s t)
  (let ((n (string-length s)) (m (string-length t)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) t) #t)
            (else (loop (+ i 1)))))))

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

;; a def's fn arity, from "(def _ (fn [a] ..body..))"
(define (fn-of src) (jolt-get (anode src) (kw "init")))
(define (arity-of src) (jolt-nth (jolt-get (fn-of src) (kw "arities")) 0))
;; emit a def whose fn carries the specialized arity — numeric-annotate (NOT
;; run-passes, which would re-infer lean and erase the contagion coerces) then emit.
(define (emit-spec base-src spar)
  (let* ((base-def (anode base-src))
         (base-fn (jolt-get base-def (kw "init")))
         (spec-fn (jolt-assoc base-fn (kw "arities") (jolt-vector spar)))
         (spec-def (jolt-assoc base-def (kw "init") spec-fn)))
    (emit (numeric-annotate spec-def))))

;; === (1) :num field beside a :double operand -> contagion (fl* + exact->inexact) ==
(evals "(defrecord IBox [n])")
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (chez-protocol-methods-map))
(define idef (anode "(defrecord IBox [n])"))
(define iuse (anode "(def iuse (fn [] (->IBox 7)))"))   ; integer ctor arg -> :n :num
(wp-infer! (jolt-vector idef iuse))
(let* ((src "(def _ (fn [a] (* 3.14159 (:n a))))")
       (res (contagion-specialize-arity (arity-of src) "IBox"))
       (spar (jolt-nth res 0))
       (eligible? (jolt-nth res 1))
       (e (emit-spec src spar)))
  (check "(1) :num beside :double is eligible" eligible? #t)
  (check "(1) contagion lowers to fl*" (sub? e "fl*") #t)
  (check "(1) contagion coerces :num operand via exact->inexact" (sub? e "exact->inexact") #t))

;; === (2) pure-:num (no :double operand) -> stays generic (the invariant) =========
(let* ((src "(def _ (fn [a] (* (:n a) (:n a))))")
       (res (contagion-specialize-arity (arity-of src) "IBox"))
       (spar (jolt-nth res 0))
       (eligible? (jolt-nth res 1))
       (e (emit-spec src spar)))
  (check "(2) pure-:num not eligible (no :double operand)" eligible? #f)
  (check "(2) pure-:num stays generic (no fl*)" (sub? e "fl*") #f))

;; === (3) genuine :double field -> shared path already unboxes; no clone worth it ==
(evals "(defrecord DBox [d])")
(set-record-shapes! (chez-record-shapes-map))
(define ddef (anode "(defrecord DBox [d])"))
(define duse (anode "(def duse (fn [] (->DBox 1.0)))"))  ; flonum ctor -> :d :double
(wp-infer! (jolt-vector ddef duse))
(let* ((src "(def _ (fn [a] (* 2.0 (:d a))))")
       (res (contagion-specialize-arity (arity-of src) "DBox"))
       (eligible? (jolt-nth res 1)))
  (check "(3) genuine :double field not eligible (shared path already unboxes)" eligible? #f))

;; === (4) runtime specialized clone registry + devirt-resolve-fl =================
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [r] Shape (area [s] (:r s)))")
(evals "(def c (->Circle 7))")
(define clone-fn (lambda (s) 'CLONE))
(register-clone "user.Circle" "Shape" "area" clone-fn)
(check "(4) devirt-resolve-fl finds the clone"
       (eq? (devirt-resolve-fl "user.Circle" "Shape" "area" (var-deref "user" "c")) clone-fn) #t)
;; Square.area has no clone -> devirt-resolve-fl falls back to devirt-resolve.
(evals "(defrecord Square [w] Shape (area [s] (* (:w s) (:w s))))")
(evals "(def sq (->Square 5))")
(check "(4) devirt-resolve-fl falls back when no clone (Square.area)"
       (eq? (devirt-resolve-fl "user.Square" "Shape" "area" (var-deref "user" "sq"))
            (devirt-resolve "user.Square" "Shape" "area" (var-deref "user" "sq"))) #t)
;; a re-extend re-registers the impl -> register-protocol-method invalidates the clone
;; for exactly (Circle/Shape/area) -> devirt-resolve-fl falls back to the fresh impl.
(evals "(extend-type Circle Shape (area [s] (:r s)))")
(check "(4) clone invalidated on re-register (epoch)"
       (eq? (devirt-resolve-fl "user.Circle" "Shape" "area" (var-deref "user" "c"))
            (devirt-resolve "user.Circle" "Shape" "area" (var-deref "user" "c"))) #t)

;; === (5) backend emits a contagion clone for an eligible impl ==================
;; A :num field beside a proven :double operand -> the register-inline-method call
;; for that impl is wrapped with a clone def + register-clone*, the clone body
;; lowering to fl* with the :num operand coerced via exact->inexact.
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define emit-top-form (var-deref "jolt.backend-scheme" "emit-top-form"))
(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(set-optimize! #t)
(set-direct-link! #t)
(evals "(defprotocol P (m [s]))")
(evals "(defrecord IBag [n] P (m [s] (* 3.14159 (:n s))))")
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (chez-protocol-methods-map))
(define bag-def (anode "(defrecord IBag [n] P (m [s] (* 3.14159 (:n s))))"))
(define bag-ctor (anode "(def bag-use (fn [] (->IBag 7)))"))
(wp-infer! (jolt-vector bag-def bag-ctor))
(let ((emitted (emit-top-form (run-passes bag-def (make-analyze-ctx "user")))))
  (check "(5) eligible impl emits a clone registration" (sub? emitted "register-clone*") #t)
  (check "(5) clone body lowers to fl*" (sub? emitted "fl*") #t)
  (check "(5) clone coerces the :num operand via exact->inexact" (sub? emitted "exact->inexact") #t))

;; === (6) a pure-:num impl (no :double operand) emits NO clone ==================
(evals "(defrecord IPlain [n] P (m [s] (* (:n s) (:n s))))")
(set-record-shapes! (chez-record-shapes-map))
(define plain-def (anode "(defrecord IPlain [n] P (m [s] (* (:n s) (:n s))))"))
(define plain-ctor (anode "(def plain-use (fn [] (->IPlain 7)))"))
(wp-infer! (jolt-vector plain-def plain-ctor))
(let ((emitted (emit-top-form (run-passes plain-def (make-analyze-ctx "user")))))
  (check "(6) pure-:num impl emits NO clone" (sub? emitted "register-clone*") #f))
(set-direct-link! #f)
(set-optimize! #f)

(if (= fails 0)
    (begin (printf "contagion gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "contagion gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
