;; run-inline-body.ss — inline method body field-read gate.
;;
;; When `run-passes` re-infers inline method bodies with the receiver typed as the
;; record, (get _p :field) must emit jrec-field-at (bare index) instead of jolt-get.
;;
;;   chez --script host/chez/run-inline-body.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")
(load "host/chez/gate-private-tmpdir.ss")

(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define emit    (var-deref "jolt.backend-scheme" "emit"))
(define analyze (var-deref "jolt.analyzer" "analyze"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

;; Populate runtime tables with a protocol and a defrecord with inline method impl.
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [^double r] Shape (area [_] (* r r 3.14159)))")

;; Get shapes from the populated runtime tables.
(define shapes (chez-record-shapes-map))
(define pmethods (chez-protocol-methods-map))

;; Analyze the defrecord form.  The macro expansion populates the runtime tables
;; (register-record-type!, register-inline-method!), so shapes are available.
(let* ((ir (analyze (make-analyze-ctx "user")
                    (jolt-ce-read "(defrecord Circle [^double r] Shape (area [_] (* r r 3.14159)))")))
       (_ (set-optimize! #t))
       (_ (set-record-shapes! U shapes))
       (_ (set-protocol-methods! U pmethods))
       (passed (run-passes ir (make-analyze-ctx "user") U))
       (emitted (emit passed)))
  ;; The register-inline-method's fn body is inside the :do statements; the
  ;; reinfer pass should have seeded the receiver param so field reads emit
  ;; jrec-field-at.  Not checking jolt-get absence — the :do also contains
  ;; defs that use jolt-get for other purposes.
  (gate-check "inline method body field read uses direct accessor"
         (gate-sub? emitted "jrec1-f0") #t))

;; Also check that a deftype (non-record protocol impl) does NOT break anything.
;; deftype bodies use register-method, not register-inline-method.
(evals "(defrecord Square [s] Shape (area [_] (* s s)))")
(define shapes2 (chez-record-shapes-map))
(let* ((ir2 (analyze (make-analyze-ctx "user")
                     (jolt-ce-read "(defrecord Square [s] Shape (area [_] (* s s)))")))
       (_ (set-record-shapes! U shapes2))
       (passed2 (run-passes ir2 (make-analyze-ctx "user") U))
       (emitted2 (emit passed2)))
  (gate-check "deftype field read uses direct accessor"
         (gate-sub? emitted2 "jrec1-f0") #t))

;; jolt-ox7c.46: scalar-replace must not DISCARD a throwing sibling. A numeric op
;; throws on a non-numeric arg, so a map value like (+ x "throwme") whose key is
;; never read must NOT be dropped when the map binding is eliminated (that would
;; swallow the exception under --opt). The emitted body still contains the op.
(set-direct-link-flag! #t)
(let* ((ir (analyze (make-analyze-ctx "user")
                    (jolt-ce-read "(fn [x] (let [m {:a (+ x \"throwme\")}] (:b m)))")))
       (_ (set-optimize! #t))
       (passed (run-passes ir (make-analyze-ctx "user") U))
       (emitted (emit passed)))
  (gate-check "throwing discarded map value survives scalar-replace"
              (gate-sub? emitted "throwme") #t))
(set-direct-link-flag! #f)

;; --- splicing follows LINKAGE, not a build mode (jolt-mbcm.6) ---------------
;; hc-inline-enabled? used to be (and hc-optimize? hc-direct-link?), so the
;; DEFAULT release build emitted a real call everywhere --opt emitted a spliced
;; body -- a policy dial on a pass whose precondition is a correctness property.
;; It is hc-direct-link? alone now, and there is no second path to fall back to,
;; so this 2x2 is what stands in for the flag.
;;
;; Both axes are varied INDEPENDENTLY on purpose. Pinning only the shipped
;; configurations would leave the linkage-on/passes-off cell untested, and that
;; is the one cell that tells (and hc-optimize? hc-direct-link?) apart from
;; hc-direct-link? -- checked by mutation: restoring the old conjunction fails
;; row 1 and nothing else, and widening the gate to #t fails rows 3 and 4.
;;
;; Asserted on the callee's NAME: a spliced body has no reference left to it, an
;; un-spliced call reads its var by name whichever way the back end spells that.
(define (ilg-emit src)
  (emit (run-passes (analyze (make-analyze-ctx "user") (jolt-ce-read src))
                    (make-analyze-ctx "user") U)))
(define ilg-callee "(defn ilg-callee [a b] (+ a b 1))")
(define ilg-caller "(defn ilg-caller [x] (ilg-callee x 2))")
(define (ilg-spliced?) (not (gate-sub? (ilg-emit ilg-caller) "ilg-callee")))
(evals ilg-callee)                      ; intern the var so the ref resolves to :var

;; 1. linkage alone decides. Passes OFF, direct-linked: still spliced, because
;;    the closed world is the whole precondition. This also stashes the callee,
;;    so rows 3 and 4 below refuse a splice that is otherwise available.
(set-optimize! #f)
(set-direct-link-flag! #t)
(ilg-emit ilg-callee)
(gate-check "direct-linked, passes off: eligible callee is spliced" (ilg-spliced?) #t)

;; 2. the shipped build configuration.
(set-optimize! #t)
(gate-check "direct-linked, passes on: eligible callee is spliced" (ilg-spliced?) #t)

;; 3. --no-direct-link: the var stays redefinable, so the same stashed callee
;;    must NOT travel. This is the row that catches an inline gate widened past
;;    what linkage guarantees.
(set-direct-link-flag! #f)
(gate-check "dynamically linked, passes on: the same callee stays a call"
            (ilg-spliced?) #f)

;; 4. --dev, and the runtime compile spine.
(set-optimize! #f)
(gate-check "dynamically linked, passes off: the same callee stays a call"
            (ilg-spliced?) #f)

;; --- ^:redef / ^:dynamic are never spliced ----------------------------------
;; Direct-linking is decided PER DEF, not just by the global flag: dl-opt-out?
;; leaves a ^:dynamic or ^:redef def var-routed so `binding` and a later
;; redefinition still reach code already compiled. The stash gate has to make the
;; same per-def decision, and until jolt-mbcm.6 it did not -- with splicing gated
;; on --opt the gap was unreachable in a default build, and the moment splicing
;; followed linkage a ^:redef callee got its body copied into its caller and the
;; redefinition landed on a var nothing read any more. Verified end to end at the
;; time: a built binary printed "original" after interning a new greeting.
;;
;; Both rows assert on the callee's NAME, like the 2x2 above -- and the callers
;; are named so that neither name CONTAINS the callee's. gate-sub? is a substring
;; test, so an ilg-redef-caller would match "ilg-redef" whether or not the body
;; was spliced, and the row would pass under any mutation. It did.
(set-optimize! #t)
(set-direct-link-flag! #t)

(evals "(defn ^:redef ilg-redef [a] (+ a 1))")
(ilg-emit "(defn ^:redef ilg-redef [a] (+ a 1))")
(gate-check "^:redef callee is not spliced, even direct-linked"
            (gate-sub? (ilg-emit "(defn ilg-uses-redef [x] (ilg-redef x))") "ilg-redef") #t)

(evals "(defn ^:dynamic ilg-dyn [a] (+ a 1))")
(ilg-emit "(defn ^:dynamic ilg-dyn [a] (+ a 1))")
(gate-check "^:dynamic callee is not spliced, even direct-linked"
            (gate-sub? (ilg-emit "(defn ilg-uses-dyn [x] (ilg-dyn x))") "ilg-dyn") #t)

(set-direct-link-flag! #f)
(set-optimize! #f)

;; --- binder-carrying bodies: :fn, :loop, :recur (jolt-mbcm.5) ---------------
;; safe-op? used to refuse any body containing one of these, which was the real
;; limiter: instrumenting a grenadine build found 88 refused splices, 86 of them
;; a disallowed op and only 2 over the size budget, with :fn at 1144 blocked
;; visits and :loop at 438. subst alpha-renames their binders now, exactly as it
;; already did for :let.
;;
;; The hygiene rows EVALUATE the emitted Scheme instead of matching its text. A
;; captured variable still emits a perfectly well-formed lambda -- the bug is in
;; what it computes, not in how it reads -- so a string assertion would pass on
;; the broken output.
(define (ilg-run scm) (eval (read (open-input-string scm)) (interaction-environment)))
(define (ilg-call src) (jolt-pr-str ((ilg-run (ilg-emit src)))))

(set-optimize! #t)
(set-direct-link-flag! #t)

;; 1. capture. The callee's inner fn binds y; the caller passes ITS OWN y as the
;;    argument that lands next to it. Renaming the inner binder is the only thing
;;    keeping (+ a y) from becoming (+ y y): shadowing the name in env would not,
;;    because the danger is the substituted ARGUMENT being captured, not the
;;    callee's body seeing the wrong scope. Verified by mutation: make the :fn
;;    arm bind (:name nm) instead of (fresh nm) and (+ a y) becomes (+ y y) over
;;    the inner binder, printing [2 4 6].
(evals "(defn ilg-inner [a] (mapv (fn [y] (+ a y)) [1 2 3]))")
(ilg-emit "(defn ilg-inner [a] (mapv (fn [y] (+ a y)) [1 2 3]))")
(gate-check "inner fn binder is renamed, so the argument is not captured"
            (ilg-call "(fn [] (let [y 10] (ilg-inner y)))") "[11 12 13]")

;; 2. a loop/recur pair travels together -- the recur's target is the loop, which
;;    is inside the body being copied.
(evals "(defn ilg-sum [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc i)) acc)))")
(ilg-emit "(defn ilg-sum [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc i)) acc)))")
(gate-check "a callee's loop/recur travels with it"
            (ilg-call "(fn [] (ilg-sum 5))") "10")

;; 3. a recur targeting the callee's OWN arity does not, because that arity is
;;    not at the call site. recur-bound? refuses the splice, so the call stays a
;;    call -- asserted on the name, and on the answer still being right.
(evals "(defn ilg-down [n] (if (pos? n) (recur (dec n)) :done))")
(ilg-emit "(defn ilg-down [n] (if (pos? n) (recur (dec n)) :done))")
(gate-check "a fn-level recur refuses the splice"
            (gate-sub? (ilg-emit "(defn ilg-uses-down [] (ilg-down 3))") "ilg-down") #t)

;; 4. an inner binder that SHADOWS the callee's own param.
(evals "(defn ilg-shadow [a] ((fn [a] (* a 2)) (+ a 1)))")
(ilg-emit "(defn ilg-shadow [a] ((fn [a] (* a 2)) (+ a 1)))")
(gate-check "a shadowing inner binder still shadows after the splice"
            (ilg-call "(fn [] (let [a 100] (ilg-shadow a)))") "202")

;; 5. an ARRAY-HINTED callee splices and its copy stays on the typed path.
;;    The splice binds each ^doubles/^longs/... param under a :coerce of its
;;    kind, the same carrier a ^double/^long param and an array-hinted let use,
;;    so the numeric pass types the copy's local. Before that carrier existed
;;    these fns were refused outright: when :loop became spliceable, dot's
;;    (aget a i) emitted jolt-nth in every copy and bench/arrays went 229.7 ->
;;    1272.6ms while every behavioural gate passed. So this asserts the
;;    emission, not only the answer.
(evals "(defn ilg-adot ^double [^doubles v ^long n] (loop [i 0 acc 0.0] (if (< i n) (recur (inc i) (+ acc (aget v i))) acc)))")
(ilg-emit "(defn ilg-adot ^double [^doubles v ^long n] (loop [i 0 acc 0.0] (if (< i n) (recur (inc i) (+ acc (aget v i))) acc)))")
(let ((e (ilg-emit "(defn ilg-uses-adot [v] (ilg-adot v 4))")))
  (gate-check "an array-hinted callee is spliced" (gate-sub? e "ilg-adot") #f)
  (gate-check "...and its copy reads the flvector unboxed" (gate-sub? e "flvector-ref") #t)
  (gate-check "...never through jolt-nth" (gate-sub? e "jolt-nth") #f))
(gate-check "...and answers what the call did"
            (ilg-call "(fn [] (ilg-adot (double-array [1.0 2.0 3.0 4.0]) 4))") "10.0")

(set-direct-link-flag! #f)
(set-optimize! #f)

;; --- a declared ^Record param survives the splice (jolt-2ztv) ---------------
;; :phints are a DECLARATION, not an inference result: types.clj seeds an arity
;; from them exactly when no caller type could be inferred, which is what types a
;; record param in the open world. A spliced body has no arity, and until now the
;; stash did not carry them at all -- so a callee that bare-indexed its own field
;; read fell back to jolt-get the moment it was copied into a caller.
;;
;; The argument is deliberately opaque (a var deref), so nothing but the
;; declaration can type it: with the hint removed the same call emits jolt-get,
;; which is the control row below.
(evals "(defrecord ILGPt [x y])")
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (chez-protocol-methods-map))
(set-optimize! #t)
(set-direct-link-flag! #t)
(evals "(def ilg-pt (->ILGPt 1 2))")

(evals "(defn ilg-hinted [^ILGPt p] (:x p))")
(ilg-emit "(defn ilg-hinted [^ILGPt p] (:x p))")
;; ...but a declaration is not a proof: Clojure's (:x p) answers a map's :x, or
;; nil, whatever p is hinted. So the hinted body runs under a type test on p --
;; the proven branch bare-indexes, anything else takes the generic lookup.
(let ((e (ilg-emit "(defn ilg-hinted [^ILGPt p] (:x p))")))
  (gate-check "the hinted callee bare-indexes its own field read" (gate-sub? e "jrec2-f0") #t)
  (gate-check "...under a type test on the param" (gate-sub? e "(eq? (jrec-desc") #t)
  (gate-check "...with the generic lookup for any other value" (gate-sub? e "jolt-kw-get-site") #t))
(gate-check "a map passed to the hinted fn reads as a map"
            (ilg-call "(fn [] (ilg-hinted {:x 42}))") "42")
(gate-check "...and nil reads nil" (ilg-call "(fn [] (ilg-hinted nil))") "nil")
(let ((e (ilg-emit "(defn ilg-uses-hinted [] (ilg-hinted ilg-pt))")))
  (gate-check "the hinted callee is spliced" (gate-sub? e "ilg-hinted") #f)
  (gate-check "a spliced ^Record param still bare-indexes" (gate-sub? e "jrec2-f0") #t)
  (gate-check "...under the same test" (gate-sub? e "(eq? (jrec-desc") #t))
;; an argument inference PROVES is that record answers the test at compile time:
;; the guard folds away and only the bare-index read is left. p escapes into the
;; result so scalar-replace keeps the record (and the read) rather than folding
;; (:x (->ILGPt a 2)) to a.
(let ((e (ilg-emit "(defn ilg-uses-proven [a] (let [p (->ILGPt a 2)] [p (ilg-hinted p)]))")))
  (gate-check "a proven argument folds the test" (gate-sub? e "(eq? (jrec-desc") #f)
  (gate-check "...and bare-indexes" (gate-sub? e "jrec2-f0") #t)
  (gate-check "...with no generic lookup left" (or (gate-sub? e "jolt-get") (gate-sub? e "jolt-kw-get-site")) #f))

;; control: the SAME code without the hint. Nothing types the local, so the
;; spliced read is generic -- which is what the rows above measure.
(evals "(defn ilg-unhinted [p] (:x p))")
(ilg-emit "(defn ilg-unhinted [p] (:x p))")
(let ((e (ilg-emit "(defn ilg-uses-unhinted [] (ilg-unhinted ilg-pt))")))
  (gate-check "control: no hint, no bare index" (gate-sub? e "jrec2-f0") #f)
  ;; generic = the keyword-invoke site lookup, never a static field accessor
  (gate-check "control: no hint, generic lookup" (gate-sub? e "jolt-kw-get-site") #t))

;; --- a spliced fn literal still travels in a state image (jolt-giqc) --------
;; A closure is written to an image through its RECORDED SOURCE: the back end
;; registers (form, defining ns, free names) per anon literal, and restore
;; compiles (fn* [free-names...] form) in that ns and applies it to the values
;; recovered from the live closure by those names. A spliced copy broke both
;; halves of the name assumption -- its binders are renamed and its captures may
;; be the caller's locals or gone entirely -- so the pass used to drop the
;; registration, and a fn `jolt run` dumped fine refused in a built binary.
;;
;; The rows below do the real round trip rather than match emitted text: the
;; failure was never in how the lambda READ, it was in whether the image could
;; rebuild it, and only writing and reading one answers that.
(define (ilg-emit-ns src ns)
  (emit (run-passes (analyze (make-analyze-ctx ns) (jolt-ce-read src))
                    (make-analyze-ctx ns) U)))
(define (ilg-eval scm) (eval (read (open-input-string scm)) (interaction-environment)))
(define image-write! (var-deref "jolt.host" "image-write!"))
(define image-read   (var-deref "jolt.host" "image-read"))
(define image-scan   (var-deref "jolt.host" "image-scan"))
(define (ilg-roundtrip clo path)
  ;; guarded so a refusal reports as a failed row rather than aborting the gate
  ;; before the rows after it run
  (guard (e (#t 'image-refused))
    (jolt-invoke2 image-write! path clo)
    (jolt-invoke1 image-read path)))
(define (ilg-call-restored clo path arg)
  (let ((r (ilg-roundtrip clo path)))
    (if (procedure? r) (jolt-invoke1 r arg) r)))

;; the callee lives in its OWN namespace and its literal reads a var only that
;; namespace can resolve -- so a registration recording the CALLER's ns would
;; fail to compile at restore. That is what pins :src-ns.
(jolt-compile-eval "(do (def ilg-base 100) (defn ilg-mk [n] (fn [y] (+ ilg-base n y))))"
                   "ilglib")
(ilg-emit-ns "(def ilg-base 100)" "ilglib")
(ilg-emit-ns "(defn ilg-mk [n] (fn [y] (+ ilg-base n y)))" "ilglib")

;; 1. a live capture, renamed by the splice: the closure holds the caller's own
;;    local under a fresh name, and the registration has to say so.
(ilg-eval (ilg-emit-ns "(defn ilg-app-live [q] (ilglib/ilg-mk (+ q 1)))" "ilgapp"))
(let* ((clo (jolt-invoke1 (var-deref "ilgapp" "ilg-app-live") 9)))
  (gate-check "spliced closure over a renamed local is live-correct"
              (jolt-invoke1 clo 5) 115)
  (gate-check "spliced closure over a renamed local is writable"
              (jolt-count (jolt-invoke1 image-scan clo)) 0)
  (gate-check "and restores to the same answer"
              (ilg-call-restored clo (string-append (host-temp-dir) "/jolt-gate-ilg-live.fasl") 5) 115))

;; 2. a CONSTANT argument. Copy propagation folds it into the body, so the
;;    compiled closure captures nothing at all and there is no variable left for
;;    the inspector to report -- the value has to travel as data on the
;;    registration instead. This is the shape the bug report opened with.
(ilg-eval (ilg-emit-ns "(defn ilg-app-const [] (ilglib/ilg-mk 10))" "ilgapp"))
(let* ((clo (jolt-invoke0 (var-deref "ilgapp" "ilg-app-const"))))
  (gate-check "spliced closure over a folded constant is live-correct"
              (jolt-invoke1 clo 5) 115)
  (gate-check "spliced closure over a folded constant is writable"
              (jolt-count (jolt-invoke1 image-scan clo)) 0)
  (gate-check "and restores to the same answer"
              (ilg-call-restored clo (string-append (host-temp-dir) "/jolt-gate-ilg-const.fasl") 5) 115))

;; 3. the capture list is only emitted when the copy actually differs. An
;;    un-spliced literal registers exactly as it always did -- the registry
;;    defaults the live names to the source names, and defaulting is `eq?`, so
;;    this reads whether the fifth argument was emitted at all.
(let ((reg (image-fn-form-lookup "jfn$ilglib/ilg-mk$0")))
  (gate-check "the callee's own literal is registered" (vector? reg) #t)
  (gate-check "an un-spliced registration carries no capture list"
              (eq? (vector-ref reg 2) (vector-ref reg 3)) #t))
;; 4. a literal from a namespace the LANGUAGE owns registers like any other,
;;    spliced or not. This row used to assert the opposite: core's literals were
;;    excluded from registration so the seed prelude stayed byte-identical across
;;    a mint, and a spliced COPY of one had to stay excluded too, or a built
;;    binary would dump closures `jolt run` refused. Core registers now -- an
;;    image that cannot carry core's closures is not carrying program state --
;;    so both halves of that asymmetry are gone and a copy is treated like the
;;    original. The callee's name is what the splice assertion reads, so the
;;    caller is named so as not to contain it.
(jolt-compile-eval "(do (defn sysmk [n] (fn [y] (+ n y))))" "jolt.ilgsys")
(ilg-emit-ns "(defn sysmk [n] (fn [y] (+ n y)))" "jolt.ilgsys")
(let ((e (ilg-emit-ns "(defn ilg-uses-sys [q] (jolt.ilgsys/sysmk q))" "ilgapp")))
  ;; nothing names the callee's var: the spliced literal's registration still
  ;; carries the class its code compiles into, jolt.ilgsys$sysmk$fn__N, which
  ;; is the callee's
  (gate-check "a system-namespace callee is still spliced" (gate-sub? e "\"sysmk\"") #f)
  (gate-check "...and its literal keeps the callee's class" (gate-sub? e "jolt.ilgsys$sysmk$fn__") #t)
  (gate-check "and its literal is registered, like any other"
              (gate-sub? e "image-register-fn-form!") #t))
;;    the same shape from an ordinary namespace, so the pair reads as one rule
;;    rather than two.
(let ((e (ilg-emit-ns "(defn ilg-uses-lib [q] (ilglib/ilg-mk q))" "ilgapp")))
  (gate-check "an ordinary callee is spliced too" (gate-sub? e "ilg-mk") #f)
  (gate-check "and its literal IS registered"
              (gate-sub? e "image-register-fn-form!") #t))

(let ((reg (image-fn-form-lookup "jfn$ilgapp/ilg-app-live$0")))
  (gate-check "a spliced literal is registered at all" (vector? reg) #t)
  (when (vector? reg)
    (gate-check "a spliced registration carries one"
                (eq? (vector-ref reg 2) (vector-ref reg 3)) #f)
    (gate-check "and records the ns the form was WRITTEN in, not the call site's"
                (vector-ref reg 1) "ilglib")))

(set-direct-link-flag! #f)
(set-optimize! #f)

(gate-summary "inline-body")
