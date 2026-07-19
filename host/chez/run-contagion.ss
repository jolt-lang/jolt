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
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer! (var-deref "jolt.passes.types" "wp-infer!"))
(define contagion-specialize-arity (var-deref "jolt.passes.types" "contagion-specialize-arity"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
(define (kw n) (keyword #f n))

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
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (chez-protocol-methods-map))
(define idef (anode "(defrecord IBox [n])"))
(define iuse (anode "(def iuse (fn [] (->IBox 7)))"))   ; integer ctor arg -> :n :num
(wp-infer! U (jolt-vector idef iuse))
(let* ((src "(def _ (fn [a] (* 3.14159 (:n a))))")
       (res (contagion-specialize-arity U (arity-of src) "IBox"))
       (spar (jolt-nth res 0))
       (eligible? (jolt-nth res 1))
       (e (emit-spec src spar)))
  (gate-check "(1) :num beside :double is eligible" eligible? #t)
  (gate-check "(1) contagion lowers to fl*" (gate-sub? e "fl*") #t)
  (gate-check "(1) contagion coerces :num operand via exact->inexact" (gate-sub? e "exact->inexact") #t))

;; === (2) pure-:num (no :double operand) -> stays generic (the invariant) =========
(let* ((src "(def _ (fn [a] (* (:n a) (:n a))))")
       (res (contagion-specialize-arity U (arity-of src) "IBox"))
       (spar (jolt-nth res 0))
       (eligible? (jolt-nth res 1))
       (e (emit-spec src spar)))
  (gate-check "(2) pure-:num not eligible (no :double operand)" eligible? #f)
  (gate-check "(2) pure-:num stays generic (no fl*)" (gate-sub? e "fl*") #f))

;; === (3) genuine :double field -> shared path already unboxes; no clone worth it ==
(evals "(defrecord DBox [d])")
(set-record-shapes! U (chez-record-shapes-map))
(define ddef (anode "(defrecord DBox [d])"))
(define duse (anode "(def duse (fn [] (->DBox 1.0)))"))  ; flonum ctor -> :d :double
(wp-infer! U (jolt-vector ddef duse))
(let* ((src "(def _ (fn [a] (* 2.0 (:d a))))")
       (res (contagion-specialize-arity U (arity-of src) "DBox"))
       (eligible? (jolt-nth res 1)))
  (gate-check "(3) genuine :double field not eligible (shared path already unboxes)" eligible? #f))

;; === (4) runtime specialized clone registry + devirt-resolve-fl =================
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [r] Shape (area [s] (:r s)))")
(evals "(def c (->Circle 7))")
(define clone-fn (lambda (s) 'CLONE))
(register-clone "user.Circle" "Shape" "area" clone-fn)
(gate-check "(4) devirt-resolve-fl finds the clone"
       (eq? (devirt-resolve-fl "user.Circle" "Shape" "area" (var-deref "user" "c")) clone-fn) #t)
;; Square.area has no clone -> devirt-resolve-fl falls back to devirt-resolve.
(evals "(defrecord Square [w] Shape (area [s] (* (:w s) (:w s))))")
(evals "(def sq (->Square 5))")
(gate-check "(4) devirt-resolve-fl falls back when no clone (Square.area)"
       (eq? (devirt-resolve-fl "user.Square" "Shape" "area" (var-deref "user" "sq"))
            (devirt-resolve "user.Square" "Shape" "area" (var-deref "user" "sq"))) #t)
;; a re-extend re-registers the impl -> register-protocol-method invalidates the clone
;; for exactly (Circle/Shape/area) -> devirt-resolve-fl falls back to the fresh impl.
(evals "(extend-type Circle Shape (area [s] (:r s)))")
(gate-check "(4) clone invalidated on re-register (epoch)"
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
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (chez-protocol-methods-map))
(define bag-def (anode "(defrecord IBag [n] P (m [s] (* 3.14159 (:n s))))"))
(define bag-ctor (anode "(def bag-use (fn [] (->IBag 7)))"))
(wp-infer! U (jolt-vector bag-def bag-ctor))
(let ((emitted (emit-top-form (run-passes bag-def (make-analyze-ctx "user") U))))
  (gate-check "(5) eligible impl emits a clone registration" (gate-sub? emitted "register-clone*") #t)
  (gate-check "(5) clone body lowers to fl*" (gate-sub? emitted "fl*") #t)
  (gate-check "(5) clone coerces the :num operand via exact->inexact" (gate-sub? emitted "exact->inexact") #t))

;; === (6) a pure-:num impl (no :double operand) emits NO clone ==================
(evals "(defrecord IPlain [n] P (m [s] (* (:n s) (:n s))))")
(set-record-shapes! U (chez-record-shapes-map))
(define plain-def (anode "(defrecord IPlain [n] P (m [s] (* (:n s) (:n s))))"))
(define plain-ctor (anode "(def plain-use (fn [] (->IPlain 7)))"))
(wp-infer! U (jolt-vector plain-def plain-ctor))
(let ((emitted (emit-top-form (run-passes plain-def (make-analyze-ctx "user") U))))
  (gate-check "(6) pure-:num impl emits NO clone" (gate-sub? emitted "register-clone*") #f))
(set-direct-link! #f)
(set-optimize! #f)

;; === (7) a devirt site over an eligible impl resolves the contagion clone ======
;; stage 3: the devirt call site emits devirt-resolve-fl (consults the clone table
;; first) when its (type/proto/method) is a clone-site, and the resolved clone returns
;; a value identical to ordinary dispatch. A pure-:num impl has no clone-site, so its
;; devirt site stays on devirt-resolve — the non-specialized path byte-identical.
(define contagion-prepass!     (var-deref "jolt.backend-scheme" "contagion-prepass!"))
(define contagion-prepass-done! (var-deref "jolt.backend-scheme" "contagion-prepass-done!"))
(define reset-clone-prepass!   (var-deref "jolt.backend-scheme" "reset-clone-prepass!"))
(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))
;; (def usem2 (fn [x] (m2 x))) with the (m2 x) invoke annotated a devirt site on `type`.
(define (devirt-def type)
  (let* ((dn (anode "(def usem2 (fn [x] (m2 x)))"))
         (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
         (inv (jolt-get ar0 (kw "body")))
         (inv2 (jolt-assoc inv (kw "devirt-type") type (kw "devirt-proto") "P2" (kw "devirt-method") "m2")))
    (jolt-assoc dn (kw "init")
                (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                            (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
(evals "(defprotocol P2 (m2 [s]))")
(evals "(defrecord IBag2 [n] P2 (m2 [s] (* 3.14159 (:n s))))")
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (chez-protocol-methods-map))
(define ibag2-def (anode "(defrecord IBag2 [n] P2 (m2 [s] (* 3.14159 (:n s))))"))
(define ibag2-ctor (anode "(def ibag2-use (fn [] (->IBag2 7)))"))
(wp-infer! U (jolt-vector ibag2-def ibag2-ctor))
;; eligible impl + a devirt site targeting it -> a clone-site.
(reset-clone-prepass! U)
(contagion-prepass! U (jolt-vector ibag2-def (devirt-def "user.IBag2")) "user")
(contagion-prepass-done! U)
(set-optimize! #t)
(set-direct-link! #t)
(let ((e (emit-top-form (run-passes (devirt-def "user.IBag2") (make-analyze-ctx "user") U))))
  (gate-check "(7) devirt site over eligible impl emits devirt-resolve-fl" (gate-sub? e "devirt-resolve-fl") #t))
;; emit + eval the defrecord (registers the clone), then the devirt site resolves it.
(let ((rec-e (emit-top-form (run-passes ibag2-def (make-analyze-ctx "user") U)))
      (site-e (emit-top-form (run-passes (devirt-def "user.IBag2") (make-analyze-ctx "user") U))))
  (run-emit rec-e)
  (run-emit site-e)
  (evals "(def an-ibag2 (->IBag2 7))")
  (gate-check "(7) devirt-resolve-fl resolves the clone, value == dispatch"
         (jolt-invoke (var-deref "user" "usem2") (var-deref "user" "an-ibag2"))
         (evals "(m2 an-ibag2)")))
;; a pure-:num impl has no clone-site -> its devirt site stays on devirt-resolve.
(define iplain2-def (anode "(defrecord IPlain2 [n] P2 (m2 [s] (* (:n s) (:n s))))"))
(define iplain2-ctor (anode "(def iplain2-use (fn [] (->IPlain2 7)))"))
(wp-infer! U (jolt-vector iplain2-def iplain2-ctor))
(reset-clone-prepass! U)
(contagion-prepass! U (jolt-vector iplain2-def (devirt-def "user.IPlain2")) "user")
(contagion-prepass-done! U)
(let ((e (emit-top-form (run-passes (devirt-def "user.IPlain2") (make-analyze-ctx "user") U))))
  (gate-check "(7) devirt site over pure-:num impl is NOT devirt-resolve-fl" (gate-sub? e "devirt-resolve-fl") #f)
  (gate-check "(7) ...it stays on the ordinary devirt-resolve" (gate-sub? e "devirt-resolve ") #t))
(set-direct-link! #f)
(set-optimize! #f)

;; === (8) caller accumulator over a clone-resolving devirt site -> fl+ ============
;; Stage 5: a devirt site whose contagion clone returns :double types its return
;; :double per-site, so a caller's accumulator add over it fires dbl-arith? and
;; lowers to fl+ — the caller-side win the bead noted ("with caller fl+"), recovered
;; WITHOUT a global pm-rets :double leak (a PIC/megamorphic site keeps returning :any).
(evals "(defprotocol P3 (m3 [s]))")
(evals "(defrecord IBag3 [n] P3 (m3 [s] (* 3.14159 (:n s))))")
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (chez-protocol-methods-map))
(define ibag3-def  (anode "(defrecord IBag3 [n] P3 (m3 [s] (* 3.14159 (:n s))))"))
(define ibag3-ctor (anode "(def ibag3-use (fn [] (->IBag3 7)))"))
(define acc8-def   (anode "(def acc8 (fn [s] (+ 0.0 (m3 s))))"))
(define acc8-use   (anode "(def acc8-use (fn [] (acc8 (->IBag3 7))))"))
;; wp-infer! seeds acc8's `s` as IBag3 (acc8-use passes a fresh ctor) so the (m3 s)
;; site devirt-annotates on user.IBag3 during run-passes.
(wp-infer! U (jolt-vector ibag3-def ibag3-ctor acc8-def acc8-use))
(reset-clone-prepass! U)
(contagion-prepass! U (jolt-vector ibag3-def acc8-def) "user")
(contagion-prepass-done! U)
(set-optimize! #t)
(set-direct-link! #t)
(let ((e (emit-top-form (run-passes acc8-def (make-analyze-ctx "user") U))))
  (gate-check "(8) caller accumulator over clone-resolving devirt site -> fl+" (gate-sub? e "fl+") #t)
  (gate-check "(8) ...and the devirt call resolves the clone (devirt-resolve-fl)" (gate-sub? e "devirt-resolve-fl") #t))
;; the SAME caller shape over a PIC/megamorphic site (unseeded `s`) does not resolve
;; the contagion clone — no devirt-resolve-fl. (Whether it lowers to fl+ depends on
;; shared rtinfo/pm-rets state and is unreliable in this synthetic gate; the real
;; guarantee — no clone path at a PIC site — is pinned in run-pic and verified in the
;; dispatch build's emitted scheme, where stage 5 leaves fl+ counts byte-identical to
;; baseline.)
(define acc8-pic (anode "(def acc8-pic (fn [s] (+ 0.0 (m3 s))))"))
(let ((e (emit-top-form (run-passes acc8-pic (make-analyze-ctx "user") U))))
  (gate-check "(8) PIC/megamorphic site does NOT resolve the contagion clone" (gate-sub? e "devirt-resolve-fl") #f))
(set-direct-link! #f)
(set-optimize! #f)

(gate-summary "contagion")
