(ns jolt.backend-scheme.lambda
  "fn literals for the Scheme back end: each arity as a named-let clause with
  its numeric-hint coercions, ^doubles vector hoists, debug-local scope and recur
  target, and the whole literal as a lambda or case-lambda with the identity,
  ^:once, trace-site, variadic and source-registration machinery around it.
  Arity bodies are lowered through the `emit` the back end passes in."
  (:require [clojure.string :as str]
            [jolt.backend-scheme.names :refer [munge-chars munge-name]]
            [jolt.backend-scheme.session
             :refer [*array-vecs* *dbg-scope* *known-procs* *letrec-binders*
                     *qualifying-ns* *recur-target* *self-wrap* *site-call* *tail?*
                     *trace-self* *trace-site* *variadic-reg-suppressed?* dbg-record
                     debug-locals? fresh-label site-call-helper target trace-frames?]]
            [jolt.backend-scheme.hoist :refer [*cache-cells*]]
            [jolt.backend-scheme.once :refer [*once-clears* once-last-uses]]
            [jolt.backend-scheme.fnsrc
             :refer [*fnsrc-def-init?* *fnsrc-ns* *fnsrc-regs* fnsrc-maker-site
                     fnsrc-name fnsrc-system-ns?]]))

;; One arity -> a Scheme lambda param-list + a named-let-wrapped body. The named
;; let lets fn-level `recur` rebind this arity's params. A variadic arity takes a
;; Scheme rest arg coerced to a jolt seq (nil when empty); recur carries the rest
;; seq directly, and the named let's init only runs on first entry.
;; Coerce a numeric-hinted param at fn entry, the way the JVM coerces a primitive
;; parameter: ^double -> jolt->fl, ^long -> jolt->fx. Only the named-let init
;; (first entry) coerces — recur carries already-typed values, like a JVM goto. This
;; is what makes the hint a contract the body's fl*/fx* ops can rely on. `orig` is
;; the param's source name (the :nhints key); `munged` the emitted identifier.
;; A ^double/^long hint coercion with its NO-OP case open-coded. jolt->fl returns
;; its argument unchanged when it is already a flonum, and jolt->fx likewise for a
;; fixnum — the helpers already test that first — so hoisting the test to the call
;; site is value-identical and skips a call cp0 cannot inline across compilation
;; units. Everything that needs real conversion, or that must raise, still reaches
;; the helper; only the case that provably does nothing is short-circuited.
;;
;; This is the float-side twin of the bit-op inline in emit-invoke, and the same
;; regression: these sites emitted a bare exact->inexact until v0.5.13, which Chez
;; inlines, and moving to jolt->fl for JVM cast semantics turned every ^double
;; parameter entry, return and contagion site into a procedure call. On mandelbrot
;; that was 23.0 -> 31.7ms with nothing else changed.
;; `return?` picks the ^long RETURN coercion, which is a different operation from
;; the parameter one: a parameter range-checks and raises, a return truncates to
;; the low 64 bits (jolt->fx-ret; see rt.ss). ^double is the same either way.
(defn emit-nhint-coerce
  ([kind e] (emit-nhint-coerce kind e false))
  ([kind e return?]
   (let [[pred helper] (cond (= kind :double) ["flonum?" "jolt->fl"]
                             return?          ["fixnum?" "jolt->fx-ret"]
                             :else            ["fixnum?" "jolt->fx"])
         t (fresh-label "_nc$")]
     (str "(let ((" t " " e ")) (if (" pred " " t ") " t " (" helper " " t ")))"))))

(defn- nhint-init [nh orig munged]
  (let [k (get nh orig)]
    (cond (= k :double) (emit-nhint-coerce :double munged)
          (= k :long)   (emit-nhint-coerce :long munged)
          :else munged)))

(defn- emit-arity-clause [emit a]
  (let [orig (:params a)
        nh (into {} (:nhints a))
        params (map munge-name orig)
        restp (when-let [r (:rest a)] (munge-name r))
        label (fresh-label "fnrec")
        ret (:ret-nhint a)
        ;; a ^doubles param's backing flvector, bound once per entry — inside the
        ;; named let, so a fn-level recur that passes a DIFFERENT array rebinds
        ;; it — and every proven aget/aset on that param indexes it directly
        ;; (*array-vecs*). The params themselves shadow any outer hoist.
        ah (into {} (:ahints a))
        avecs (into {} (keep (fn [o] (when (contains? #{:doubles :floats} (get ah o))
                                       [(munge-name o) (fresh-label "_av$")]))
                             orig))
        av (merge (apply dissoc *array-vecs* (concat params (when restp [restp]))) avecs)
        ;; the body is the fn's tail position — UNLESS a ^double/^long return hint
        ;; wraps it in a coercion below, which puts the body back in non-tail.
        body-tail? (not (or (= ret :double) (= ret :long)))
        ;; debug locals: the arity's params are the root of its frame's scope
        ;; chain, bound inside the named let so a fn-level recur rebinds them; the
        ;; body is a tail region whatever the return hint (emit's leaf rule)
        dv (when (debug-locals?) (fresh-label "_dl$"))
        body (binding [*recur-target* label *tail?* (or (some? dv) body-tail?) *array-vecs* av
                       *dbg-scope* dv]
               (emit (:body a)))
        body (if dv
               (str "(let ((" dv " " (dbg-record nil (concat orig (when-let [r (:rest a)] [r]))
                                                 (concat params (when restp [restp]))) ")) "
                    body ")")
               body)
        body (if (seq avecs)
               (str "(let (" (str/join " " (map (fn [[p v]] (str "(" v " (jolt-array-vec-of " p "))")) avecs))
                    ") " body ")")
               body)
        paramlist (cond
                    (and restp (empty? params)) restp
                    restp (str "(" (str/join " " params) " . " restp ")")
                    :else (str "(" (str/join " " params) ")"))
        pbind (map (fn [o p] (str "(" p " " (nhint-init nh o p) ")")) orig params)
        ;; jolt-rest-seq, not list->cseq: it also unwraps the lazy-rest box that
        ;; jolt-apply hands a registered variadic fn, so (apply f infinite-seq)
        ;; binds the rest to the seq instead of realizing it (seq.ss).
        binds (if restp
                (concat pbind [(str "(" restp " (jolt-rest-seq " restp "))")])
                pbind)
        lett (str "(let " label " (" (str/join " " binds) ") " body ")")
        ;; a ^double/^long return hint coerces the arity's value on the way out
        ;; (jolt->fl / jolt->fx), like a JVM primitive return — so a caller's
        ;; arithmetic over the result is sound.
        ret (:ret-nhint a)]
    [paramlist (cond (= ret :double) (emit-nhint-coerce :double lett)
                     (= ret :long)   (emit-nhint-coerce :long lett true)
                     :else lett)]))

;; The unique registry name of a literal that registers its source form, or nil.
;; The namespace the literal's SOURCE was written in (SRC-NS) is this one, or the
;; callee's when the inline pass copied it here. Both are checked against
;; the system split, so a core literal spliced into user code stays
;; unregistered exactly as it is when core runs un-spliced — the language
;; owns those namespaces, and a copy of one is still one of theirs.
;;
;; A literal registers whether or not it has a NAME. It used to have to be
;; anonymous, because the registry is keyed on the name Chez reports and a
;; named fn is bound under its own munged name, which is not unique -- two
;; `mapi`s in two fns would collide. So a named one is bound under the
;; anon label plus its name, jfn$<ns>/<def>$<n>/<name>, and its short
;; name aliases that: unique for the registry, and a backtrace shows it
;; as <ns>/<def>/<name> (source-registry srcreg-display-name), the way
;; clojure.stacktrace demunges user$f$mapi__12. It was <name>$jf<n> with
;; the counter per def, so `mapi` in two defs — or in two namespaces —
;; was mapi$jf0 in both, and an image restore of the first closure came
;; back with the second's source.
;;
;; This is what map-indexed, distinct, dedupe, partition-by and tree-seq
;; needed: each closes a lazy-seq thunk over a letfn-bound fn, and that
;; captured fn had no source to rebuild from however well the thunk itself
;; travelled.
(defn- fn-registry-name [node def-init? src-ns]
  (when (and (not def-init?)
             (not (fnsrc-system-ns? *fnsrc-ns*))
             (not (fnsrc-system-ns? src-ns))
             (:src-form node))
    (if (:name node)
      (str (fnsrc-name) "/" (munge-chars (:name node)))
      (fnsrc-name))))

;; A ^:once fn's capture box: {:env :ps :marked :clears}, or nil when the fn is
;; not ^:once, captures nothing, or is a spliced copy (:live-names).
;; A single-arity body releases each capture at its last use
;; (once-last-uses), as the reference's compiler nulls a once fn's field.
;; On Chez the emptied slot holds the literal #!bwp, an immediate, so the
;; store needs no write barrier (1.4 ns against 2.6 for jolt-nil); the box
;; is read back through jolt-once-ref, which answers nil for it -- what a
;; rerun after a failure sees, as the reference's cleared fields are null.
(defn- fn-once-box [node arities]
  (when (and (:once node) (seq (:free-names node)) (nil? (:live-names node)))
    (let [env (fresh-label "_once$")
          marked (when (= 1 (count arities))
                   (let [a (first arities)
                         hidden (cond-> (set (:params a)) (:rest a) (conj (:rest a)))]
                     [(assoc a :body (once-last-uses (:body a) (remove hidden (:free-names node))))]))
          cleared (if (= :chez (target)) "#!bwp" "jolt-nil")]
      {:env env
       :ps (map munge-name (:free-names node))
       :marked marked
       :clears (when marked
                 (into {} (map-indexed
                            (fn [i nm] [nm (str "(vector-set! " env " " (inc i) " " cleared ")")])
                            (:free-names node))))})))

;; One arity clause [params body]. Each clause that has a site binds the restore
;; helper its sites call (site-restore), a nested fn's clauses their own.
(defn- emit-site-clause [emit a]
  (if (and (trace-frames?) (= :chez (target)))
    (let [used (atom false)
          c (binding [*site-call* used] (emit-arity-clause emit a))]
      (if @used
        [(nth c 0) (str "(let ((jolt-site-call " site-call-helper ")) " (nth c 1) ")")]
        c))
    (emit-arity-clause emit a)))

(defn- emit-fn-clauses [emit arities self site once-clears self-wrap]
  (binding [*known-procs* (if self (assoc *known-procs* self site) *known-procs*)
            *once-clears* once-clears
            *trace-site* site
            *trace-self* (cond-> #{} self (conj self) site (conj site))
            *self-wrap* self-wrap
            ;; An enclosing letrec's bindings are initialised by the
            ;; time anything in THIS body runs, so a literal nested
            ;; here may name one — map-indexed's lazy-seq thunk
            ;; referencing the letfn-bound mapi is the ordinary case.
            ;; The constraint applies only to a literal created
            ;; DURING the initialisation, which is this fn itself.
            *letrec-binders* #{}
            *fnsrc-def-init?* false]
    (mapv (fn [a] (emit-site-clause emit a)) arities)))

;; the self-calls' placeholder head, now that the body is known (*self-wrap*)
(defn- fill-self-wrap [clauses self-wrap]
  (if self-wrap
    (let [tok (:tok self-wrap)
          unsafe? @(:unsafe self-wrap)
          rd-from (str "(" tok "r)")
          rd-to (if unsafe? "(virtual-register 2)" "0")
          wr-from (str "(" tok "w ")
          wr-to (if unsafe? "(set-virtual-register! 2 " "(begin ")
          fix (fn [x] (if (string? x)
                        (str/replace (str/replace x rd-from rd-to) wr-from wr-to)
                        x))]
      (mapv (fn [c] (mapv fix c)) clauses))
    clauses))

;; Clojure's exact fixed arity wins over a variadic arity that also accepts
;; the count; Chez's case-lambda takes the FIRST clause that accepts, so a
;; variadic clause declared before a colliding fixed one shadows it, and a
;; call that the JVM routes to the fixed arity lands in the variadic one.
;;
;; A fixed arity can never exceed the variadic threshold (the JVM rejects
;; that outright: "Can't have fixed arity function with more params than
;; variadic function") nor equal another fixed arity, so the only legal
;; overlap is equality -- exactly the case the JVM resolves toward the fixed
;; clause. Fixed clauses first and the single variadic clause last therefore
;; reproduces JVM selection on every legal input.
;;
;; Reordered on the EMITTED clauses only: `arities` keeps declared order for
;; :arglists and the variadic registration below, and emitting in the
;; original order leaves label allocation (the jfn$/fnvar names) untouched.
;; From here on a variadic clause, if there is one, is LAST in `clauses`.
(defn- fixed-clauses-first [clauses arities]
  (let [variadic? (mapv (fn [a] (boolean (:rest a))) arities)
        fixed     (keep-indexed (fn [i c] (when-not (nth variadic? i) c))
                                clauses)]
    (into (vec fixed)
          (keep-indexed (fn [i c] (when (nth variadic? i) c)) clauses))))

;; Gambit's case-lambda expander (lib/_nonstd.scm ##case-lambda, 4.9.7
;; and 4.9.8) appends the rest parameter to the generated lambda's formals
;; only when some clause has OPTIONAL parameters — and the two-clause fn
;; (fn ([x y] …) ([x y & more] …)) has none: both clauses require exactly
;; two. Its dispatch still reads the rest variable, so every call fails
;; with "Unbound variable: #:gN" (bit-and, bit-or and four more seed fns
;; were dead that way; gambitunbound reports the uninterned names). On
;; that target the two clauses are emitted as the one rest lambda the
;; macro should have produced: an empty rest is the fixed arity. Same
;; behaviour under jolt-apply's boxed lazy rest (a one-element list, so
;; the variadic body's jolt-rest-seq unwraps it). Nothing else changes —
;; a third clause, or a variadic with more required params, expands fine.
;; The reorder above put the fixed clause first and the variadic one
;; last, whichever order they were declared in, so the clauses are read
;; by that position.
(defn- gambit-merged-lambda [clauses arities]
  (when (and (= :gambit (target)) (= 2 (count arities)))
    (let [[a b] arities
          fixed (cond (and (:rest a) (not (:rest b))) b
                      (and (:rest b) (not (:rest a))) a)
          variadic (if (= fixed a) b a)]
      (when (and fixed (= (count (:params fixed)) (count (:params variadic))))
        (let [fbody (nth (nth clauses 0) 1)
              [vformals vbody] (nth clauses 1)
              fps (map munge-name (:params fixed))
              vps (map munge-name (:params variadic))
              fbody (if (= fps vps)
                      fbody
                      (str "(let (" (str/join " " (map (fn [f v] (str "(" f " " v ")")) fps vps))
                           ") " fbody ")"))]
          (str "(lambda " vformals
               " (if (null? " (munge-name (:rest variadic)) ") " fbody " " vbody "))"))))))

;; A ^:once fn (the reference's lazy-seq thunk) lets go of what it captured
;; as it runs: the reference's compiler nulls a once-fn's closed-over
;; fields at their last use. Here the captures ride in one box the closure
;; holds; the body copies them into locals and empties the box first
;; thing, and Chez frames keep only live values, so each is released at
;; its last use (the box empties to nil, which is what a rerun after a failure
;; then sees, as the reference's cleared locals are). A plain rebinding would not do -- the optimizer
;; propagates (let ((x x)) ...) straight back to the closure's slot.
;; Without it a thunk that walks a source -- for's :when loop, which keeps
;; the closure alive for its step fn -- pinned the head of the source for
;; the whole walk (3M skipped elements out of a 256MB heap the JVM does
;; in 96MB). A literal the inline pass copied (:live-names) keeps the
;; plain closure: its captures are renamed and the box would name them
;; wrong. The tag lets the image read a boxed closure's captures in
;; order (state-image.ss image-flat-free-values).
(defn- with-once-locals [clauses once]
  (if once
    (let [once-env (:env once)
          once-ps (:ps once)
          once-marked (:marked once)
          ref (if (and once-marked (= :chez (target))) "jolt-once-ref" "vector-ref")
          binds (apply str (map-indexed (fn [i p] (str "(" p " (" ref " " once-env " " (inc i) "))")) once-ps))
          ;; a few slots cleared inline; a bigger box by the runtime loop
          ;; a marked body releases each at its last use; an
          ;; unmarked (multi-arity) one all on entry
          clear (cond
                  once-marked ""
                  (<= (count once-ps) 4)
                  (apply str (map (fn [i] (str "(vector-set! " once-env " " (inc i) " jolt-nil) ")) (range (count once-ps))))
                  :else (str "(jolt-once-clear! " once-env ") "))]
      (mapv (fn [c] [(nth c 0) (str "(let (" binds ") " clear (nth c 1) ")")])
            clauses))
    clauses))

(defn- clauses-lambda [clauses gambit-merge]
  (cond
    (= 1 (count clauses))
    (let [c (first clauses)] (str "(lambda " (nth c 0) " " (nth c 1) ")"))
    gambit-merge gambit-merge
    :else
    (str "(case-lambda "
         (str/join " " (map (fn [c] (str "(" (nth c 0) " " (nth c 1) ")")) clauses))
         ")")))

;; The lambda for the emitted clauses, with the identity capture and the ^:once
;; box around it.
;; The capture must stay LIVE. Chez removes a dead one and the sharing
;; comes back — measured for a dead reference, a captured value used
;; through begin, an assigned variable and a captured fresh pair, all of
;; which went back to eq?. A branch on an assigned top-level cannot be
;; folded, so the reference survives; jolt-fn-identity-probe is never
;; true, so no arity's behaviour changes. Body stays in tail position.
;; Wrapping in a let keeps Chez's procedure naming — it looks through the
;; let, so `#<procedure inner>` still reports, which the source registry
;; and native backtrace frames depend on. Verified before relying on it.
(defn- fn-lambda [clauses arities id-nm once]
  (let [clauses (if id-nm
                  (mapv (fn [c]
                          [(nth c 0)
                           (str "(if jolt-fn-identity-probe " id-nm " " (nth c 1) ")")])
                        clauses)
                  clauses)
        clauses (fixed-clauses-first clauses arities)
        gambit-merge (gambit-merged-lambda clauses arities)
        clauses (with-once-locals clauses once)
        lambda (clauses-lambda clauses gambit-merge)
        lambda (if id-nm
                 (str "(let ((" id-nm " jolt-fn-identity-seed)) " lambda ")")
                 lambda)]
    (if once
      (str "(let ((" (:env once) " (vector jolt-once-tag " (str/join " " (:ps once)) "))) " lambda ")")
      lambda)))

;; Record a registered literal's source row, and give it a MAKER when it can
;; have one: one (lambda (free…) <the literal>) that this site then calls, so
;; the closure comes from a code object the image can reach.
;;
;; Why: writing a closure to an image needs its captured values, and
;; those are read out of the live closure. Chez hands them back by
;; POSITION; the NAMES that say which is which live in inspector
;; information, which a release build does not generate — it costs
;; +117% on the compiled prelude, for a debugging model of every
;; procedure, to name the captures of a few hundred. So `jolt run`
;; refused every closure clojure.core makes (cycle, repeat, partial,
;; comp) while a default app build, which does generate it, wrote them
;; fine. The position order is not the source order and is not ours to
;; predict.
;;
;; A maker settles it without inspector information: every instance of
;; a site shares one code object, so the layout is a property of the
;; code and identical across instances. The image calls the maker once
;; with distinct sentinels, sees which position each landed in, and
;; reads every later instance through that permutation. The cost is one
;; closure per site at first dump and one call per closure creation.
;;
;; A SPLICED copy keeps the old emission: its captures no longer stand
;; in one-to-one correspondence with the source's free names (a binder
;; was renamed, an argument was folded to a constant), which is what
;; :live-names records, so there is no honest parameter list to give a
;; maker. Those still need inspector information, which the builds that
;; splice (an app build, not the seed) generate by default.
;;
;; A named literal gets the same treatment as an anonymous one, for the same
;; reason — a letfn-bound fn that a lazy-seq thunk closes over is exactly what
;; map-indexed, distinct and tree-seq capture.
(defn- register-fn-literal [node nm src-ns inner]
  (let [frees (:free-names node)
        lives (:live-names node)]
    (if (or lives (nil? *cache-cells*)
            ;; nothing captured, nothing to recover: the restore wrapper
            ;; binds no free names, so this closure never needs a layout
            (empty? frees)
            (some *letrec-binders* frees))
      ;; no maker: a spliced copy (see above), or a literal emitted outside
      ;; any cache-cell scope — there would be nowhere to bind one, and a
      ;; site calling an unbound maker is worse than a closure that refuses.
      (do (swap! *fnsrc-regs* conj [nm (:src-form node) src-ns frees lives])
          inner)
      (do (swap! *fnsrc-regs* conj [nm (:src-form node) src-ns frees nil])
          (fnsrc-maker-site nm frees inner)))))

;; A named fn references itself by name — the analyzer binds that name as a
;; :local in the body. letrec makes the name visible to the lambda.
;; The qualified binding is what Chez reports for the frame; the short alias
;; is what the body's self-calls emit (*known-procs* is keyed on it). Both
;; are letrec* bindings so the alias is in scope INSIDE the lambda — an
;; alias bound by an enclosing `let` is not, and a self-recursive body then
;; references an unbound name. letrec* also fixes the order: the alias is
;; initialised right after the lambda exists and long before any call.
;; the letrec BODY is where a variadic fn registers (REG, its fixed count), so
;; the binding still holds the bare lambda and Chez keeps the frame name.
(defn- bind-named-fn [node lambda qname fnsrc-nm src-ns reg]
  (let [m (munge-name (:name node))
        ret (if reg (str "(jolt-register-variadic! " reg " " m ")") m)]
    (cond
      qname
      (str "(letrec* ((" qname " " lambda ") (" m " " qname ")) " ret ")")
      ;; a registered inner literal: the lambda sits in the UNIQUE binding
      ;; (that is what names the procedure), the short name aliases it
      fnsrc-nm
      (register-fn-literal node fnsrc-nm src-ns
                           (str "(letrec* ((" fnsrc-nm " " lambda ") (" m " " fnsrc-nm ")) "
                                ret ")"))
      :else
      (str "(letrec ((" m " " lambda ")) " ret ")"))))

;; An anonymous literal. System namespaces, a def's direct init, and ad-hoc fns
;; (contagion clones) have no FNSRC-NM and keep the old emission byte-identical:
;; a bare lambda named by the enclosing define, or the fnvar ANON-LABEL for an
;; anon variadic (see emit-fn on why the lambda must sit in the binding
;; position).
;; A non-system anon literal is bound letrec under its globally unique name so
;; Chez reports that name through the inspector ((io 'code) 'name), and its
;; source form + defining ns + free locals are registered for the image. The
;; variadic registration stays in the letrec BODY so the binding holds the
;; bare lambda and the name survives (same constraint as the named path).
;; The ns is the one the FORM was written in. For a literal the analyzer
;; produced that is the namespace being compiled; for one the inline pass
;; copied here it is the callee's (:src-ns), and compiling the callee's
;; text in this namespace would resolve its private and aliased names
;; against the wrong map. :live-names says what each source free name
;; became in this copy (a renamed local, or a constant with no capture
;; left) — absent for a literal that was not spliced.
(defn- bind-anon-fn [node lambda fnsrc-nm src-ns variadic-fixed anon-label]
  (cond
    (and (not fnsrc-nm) anon-label)
    (str "(let ((" anon-label " " lambda ")) "
         "(jolt-register-variadic! " variadic-fixed " " anon-label "))")
    (not fnsrc-nm) lambda
    :else
    (register-fn-literal node fnsrc-nm src-ns
                         (if variadic-fixed
                           (str "(letrec ((" fnsrc-nm " " lambda ")) "
                                "(jolt-register-variadic! " variadic-fixed " " fnsrc-nm "))")
                           (str "(letrec ((" fnsrc-nm " " lambda ")) " fnsrc-nm ")")))))

(defn emit-fn [emit node]
  (let [;; a def's DIRECT anonymous init is named by its define, so it keeps the
        ;; bare lambda; *fnsrc-def-init?* is set only around that init's emission
        ;; and cleared for the body below, so a nested literal isn't mistaken for it.
        def-init? *fnsrc-def-init?*
        arities (:arities node)
        ;; a named fn binds its own name as a known-procedure local across ALL
        ;; arities, so self-calls emit directly rather than via jolt-invoke.
        self (when-let [nm (:name node)] (munge-name nm))
        ;; When *qualifying-ns* is set (the :def runtime-eval path), a def's
        ;; DIRECT named init binds its letrec under a qualified name (ns/name) so
        ;; Chez reports a unique per-var frame name — the source registry's key
        ;; for the def. A nested named literal is not the var's root: it binds
        ;; under its registry label below, on this path as on every other. Bound
        ;; under ns/name too, it was never registered (the qualified branch
        ;; skipped registration), so a closure over a letfn-bound fn dumped from
        ;; a build and refused from a `jolt run` session; and two defs' inner
        ;; `mapi`s shared one frame name that read as a var named mapi.
        qname (when (and self *qualifying-ns* def-init?)
                (str (munge-name *qualifying-ns*) "/" self))
        ;; the unique name is allocated BEFORE the arity bodies emit, so an
        ;; enclosing literal numbers ahead of the literals nested inside it
        ;; (document order — jfn$ns/def$0 is the outermost)
        src-ns (or (:src-ns node) *fnsrc-ns*)
        fnsrc-nm (fn-registry-name node def-init? src-ns)
        ;; the name a NAMED fn's frame carries, which is what its tail sites
        ;; record; nil for an anonymous literal, whose tail sites skip
        site (when self (or qname fnsrc-nm self))
        ;; --- fn identity -------------------------------------------------
        ;; Chez shares ONE closure object across every evaluation of a lambda
        ;; with no free variables, where Clojure allocates a fresh fn each time.
        ;; Observable, and real code depends on the Clojure answer: malli keys a
        ;; cache on validator closures (two :? branches collided, backtracking
        ;; died, m/validate returned false), and jolt's own fn meta is keyed on
        ;; the procedure, so with-meta leaked between unrelated fns.
        ;;
        ;; So such a lambda is given something to capture. Only a literal that is
        ;; EVALUATED REPEATEDLY needs it: a def's direct init runs once, so its
        ;; single shared instance is already the only one there will ever be, and
        ;; two distinct fn forms never share with each other.
        ;;
        ;; :free-names absent means the analyzer did not compute it (a node a pass
        ;; built), not that there are none — so absent is treated as "might be
        ;; shared" and gets the capture. Wrong only in costing a fn that already
        ;; allocated.
        id-nm (when (and (not def-init?) (empty? (:free-names node))) (fresh-label "_fnid$"))
        once (fn-once-box node arities)
        self-wrap (when (and self (trace-frames?))
                    {:tok (fresh-label "_srself$") :unsafe (atom false)})
        clauses (emit-fn-clauses emit (or (:marked once) arities) self site (:clears once) self-wrap)
        lambda (fn-lambda (fill-self-wrap clauses self-wrap) arities id-nm once)
        ;; A fn with a variadic arity records that arity's FIXED param count, so
        ;; jolt-apply can hand it a lazy rest instead of realizing the tail. The
        ;; count is recorded rather than read back from procedure-arity-mask,
        ;; which cannot recover it: (fn ([a] …) ([a b & r] …)) accepts {1,2,3,…},
        ;; so the mask says 1 where the variadic arity's fixed count is 2.
        ;;
        ;; The registration must NOT wrap the lambda expression: Chez names a
        ;; procedure after the variable it is bound to, and only when the lambda
        ;; sits directly in the binding position. Moving it into argument position
        ;; leaves the procedure unnamed, which silently unmaps its native backtrace
        ;; frame from the source registry (build-smoke's app.core/-main frame). So
        ;; register AFTER the binding, in the letrec body, where the name survives:
        ;;   (letrec ((m (lambda …))) (jolt-register-variadic! N m))   name = m
        ;;   (letrec ((m (jolt-register-variadic! N (lambda …)))) m)   name = #f
        ;; An ANONYMOUS fn has no binding of its own — it is emitted bare so the
        ;; enclosing (define jv$ns/name …) names it — so its def registers the
        ;; sibling call instead (emit-def-cached), and *variadic-reg-suppressed?*
        ;; tells this fn to leave it alone.
        variadic-fixed (some (fn [a] (when (:rest a) (count (:params a)))) arities)
        reg-here? (and variadic-fixed (not *variadic-reg-suppressed?*))]
    (if (:name node)
      (bind-named-fn node lambda qname fnsrc-nm src-ns (when reg-here? variadic-fixed))
      ;; a bare anonymous variadic fn still needs a binding to register through;
      ;; nothing maps its frame, so a fresh label costs no backtrace fidelity.
      (bind-anon-fn node lambda fnsrc-nm src-ns variadic-fixed
                    (when reg-here? (fresh-label "fnvar"))))))
