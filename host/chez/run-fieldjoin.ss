;; run-fieldjoin.ss — whole-program record FIELD-type inference gate.
;;
;; A hint-free (untyped) record field is :any today, so a protocol-method body that
;; reads it stays generic and its return joins :any — the caller's accumulator never
;; unboxes. This gate pins the new mechanism: wp-infer! joins the ctor-argument
;; types across every (->Ctor ...) site to derive each field's type, so the same
;; emission a ^double hint reaches today (protoret/fieldnum) is reached by portable
;; hint-free code.
;;
;;   (a) every ctor site passes a flonum (a statically :double arg) -> field :double
;;       -> impl body fl*, caller accumulator fl+ (the bead's required regression).
;;   (b) ctor sites pass a record or nil   -> nilable field; a guarded read narrows
;;       to the direct accessor, an unguarded read stays nil-safe.
;;   (c) a conflicting join (one site passes a string) -> reads stay generic.
;;   (d) an integer-fed (:num-joined) field must NOT unbox: dbl contagion is restricted
;;       to genuine :double fields, so flonum arithmetic over a :num field stays generic.
;;
;;   chez --script host/chez/run-fieldjoin.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze             (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes!  (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer!           (var-deref "jolt.passes.types" "wp-infer!"))
(define run-passes          (var-deref "jolt.passes" "run-passes"))
(define emit                (var-deref "jolt.backend-scheme" "emit"))
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

;; === (a) all-flonum ctor sites -> field :double -> impl fl*, caller fl+ ==========
(evals "(defprotocol Sh (area [s]))")
(evals "(defrecord Circle [r] Sh (area [s] (* 3.14159 (:r s))))")
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (chez-protocol-methods-map))
(set-optimize! #t)
;; ctor site: a flonum literal is statically :double, so :r proves :double outright
;; (NOT via dbl contagion from a :num arg — case (d) pins that restriction).
(define cdef (anode "(defrecord Circle [r] Sh (area [s] (* 3.14159 (:r s))))"))
(define mk   (anode "(def mk (fn [] (->Circle 1.0)))"))
(define sum  (anode "(def sum (fn [cs] (reduce (fn [acc c] (+ acc (area c))) 0.0 cs)))"))
(wp-infer! (jolt-vector cdef mk sum))
;; the impl body reads :r; once :r proves :double the (* 3.14159 (:r s)) unboxes.
(define cdef-e (emit (run-passes cdef (make-analyze-ctx "user"))))
(gate-check "(a) impl body unboxes :r to fl*" (gate-sub? cdef-e "fl*") #t)
;; the caller's (+ acc (area c)) goes fl+ via the concrete protocol-method return.
(define sum-e (emit (run-passes sum (make-analyze-ctx "user"))))
(gate-check "(a) caller accumulator goes fl+" (gate-sub? sum-e "fl+") #t)

;; === (d) integer-fed (:num-joined) field must NOT unbox (Option B) ==============
;; dbl contagion is restricted to genuine :double fields. A field whose ctor sites
;; pass integers joins :num (an int literal is :num); under Option B that reads :any,
;; so flonum arithmetic over it stays generic (no fl*) — unlike the all-flonum (a).
;; iuse seeds iscale's `a` as IBox (a ctor return) so the :n read resolves.
(evals "(defrecord IBox [n])")
(set-record-shapes! (chez-record-shapes-map))
(define iscale (anode "(def iscale (fn [a] (* 3.14159 (:n a))))"))
(define iuse   (anode "(def iuse (fn [] (iscale (->IBox 7))))"))   ; integer ctor arg -> :num
(wp-infer! (jolt-vector iscale iuse))
(define iscale-e (emit (run-passes iscale (make-analyze-ctx "user"))))
(gate-check "(d) integer-fed :num-joined field stays generic (no fl*)" (gate-sub? iscale-e "fl*") #f)

;; === (b) record-or-nil ctor sites -> nilable field =============================
;; Outer's :child is filled with an Inner or nil across ctor sites -> nilable-Inner.
;; The consumer takes a seeded record param (mirrors how binary-trees' check-tree
;; reads its Node param), so the field read resolves under run-passes. Inner has two
;; fields so its :val slot (jrec2-f1) is distinct from Outer's :child slot (jrec1-f0)
;; — the signal is the INNER (:val) read.
(evals "(defrecord Inner [a val])")
(evals "(defrecord Outer [child])")
(set-record-shapes! (chez-record-shapes-map))
(define odef (anode "(defrecord Outer [child])"))
(define idef (anode "(defrecord Inner [a val])"))
(define mko  (anode "(def mko (fn [b] (if b (->Outer (->Inner 1 2)) (->Outer nil))))"))
;; guarded: (some? c) narrows the nilable local -> (:val c) direct-accesses (jrec2-f1)
(define og   (anode "(def og (fn [o] (let [c (:child o)] (if (some? c) (:val c) 0))))"))
;; unguarded: c is a nilable local -> (:val c) takes the nil-safe path (jrec-field-at)
(define ou   (anode "(def ou (fn [o] (let [c (:child o)] (:val c))))"))
;; caller seeds og/ou's `o` (mko returns Outer)
(define caller (anode "(def caller (fn [b] (+ (og (mko b)) (ou (mko b)))))"))
(wp-infer! (jolt-vector odef idef mko og ou caller))
(define og-e (emit (run-passes og (make-analyze-ctx "user"))))
(gate-check "(b) guarded nilable-field read direct-accesses (jrec2-f1)" (gate-sub? og-e "jrec2-f1") #t)
(define ou-e (emit (run-passes ou (make-analyze-ctx "user"))))
(gate-check "(b) unguarded nilable-field read stays nil-safe (no direct accessor)" (gate-sub? ou-e "jrec2-f1") #f)
(gate-check "(b) unguarded nilable-field read uses nil-safe jrec-field-at" (gate-sub? ou-e "jrec-field-at") #t)

;; === (c) conflicting join -> reads stay generic ================================
;; Box's :v: one ctor site passes a flonum, another a string -> join :any -> no unbox.
(evals "(defrecord Box [v])")
(set-record-shapes! (chez-record-shapes-map))
(define bdef (anode "(defrecord Box [v])"))
(define bmk1 (anode "(def bmk1 (fn [] (->Box 1.0)))"))   ; :double
(define bmk2 (anode "(def bmk2 (fn [] (->Box \"hi\")))")) ; :str  -> conflict
(define brd  (anode "(def brd (fn [] (* (:v (bmk1)) 2.0)))"))
(wp-infer! (jolt-vector bdef bmk1 bmk2 brd))
(define brd-e (emit (run-passes brd (make-analyze-ctx "user"))))
(gate-check "(c) conflicting field join stays generic (no fl*)" (gate-sub? brd-e "fl*") #f)

(gate-summary "fieldjoin")
