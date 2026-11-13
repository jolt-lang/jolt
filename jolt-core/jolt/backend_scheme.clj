(ns jolt.backend-scheme
  "Lowers the host-neutral IR (jolt.ir) to Chez Scheme source text.

  The analyzer produces IR; this emitter turns each IR op into a string of Scheme
  source, which the host compiles with (eval (read ...)). It depends only on
  clojure.core and clojure.string, so once cross-compiled it runs on Chez and can
  emit its own code — the bootstrap spine. Quoted forms are walked through the
  portable jolt.host form-* contract, the same seam the analyzer uses, so the
  emitter never touches a concrete host representation directly.

  The dispatch over IR ops and the forms that recurse through it live here; the
  pieces that do not lower child nodes themselves live in the
  jolt.backend-scheme.* namespaces it requires, and those of them that need to
  lower a child take `emit` as an argument."
  (:require [clojure.string :as str]
            [jolt.host :refer [seed-callable? form-char? form-literal?]]
            [jolt.passes.types :as types]
            [jolt.passes.numeric :as numeric]
            [jolt.op-registry :as op-registry]
            [jolt.ir :as ir]
            [jolt.backend-scheme.names :as names :refer [chez-str-lit dl-fqn dl-name]]
            [jolt.backend-scheme.session :as session
             :refer [*array-vecs* *callsites* *dbg-scope* *dl-self* *known-procs*
                     *letrec-binders* *qualifying-ns* *recur-target* *tail?*
                     *trace-site* *variadic-reg-suppressed?* clone-site-key
                     ctor-shapes cur dbg-hidden? dbg-record dbg-record1
                     direct-link-callable? direct-link-fn?
                     direct-link? direct-linkable? fresh-label marker-safe-fqn?
                     node-line note-tail-call! prelude-mode? record-shape-for-class
                     register-callsite! seed-mint? source-reg? static-callee
                     trace-callsite-reg unsafe-prefix var-cache? with-site]]
            [jolt.backend-scheme.hoist
             :refer [*const-pool* const-coll-node? distinct-const-keys?
                     emit-top-cells emit-with-cells hoist-const hoist-const-for
                     hoist-const-per-site hoist-seed-root hoist-var-cell site-const
                     unique-keyword-key-literal?]]
            [jolt.backend-scheme.calls
             :refer [bit-fx-ops direct-field-accessor emit-bit-fx-call emit-call
                     emit-devirt-call emit-fl-aget emit-fl-aset
                     emit-host-static-call emit-numeric emit-pic-call
                     emit-rec-type-check emit-record-ctor ifn-kind lookup-restore
                     native-op node-inline-chain ordered-call plain-call
                     site-literal site-restore sited-tail-call stdlib-var?
                     struct-field-index]]
            [jolt.backend-scheme.literals :as literals :refer [emit-const]]
            [jolt.backend-scheme.once :refer [once-clear-stmts with-once-clears]]
            [jolt.backend-scheme.fnsrc :as fnsrc
             :refer [*fnsrc-counter* *fnsrc-def* *fnsrc-def-init?* *fnsrc-ns*
                     *fnsrc-regs* fnsrc-flush]]
            [jolt.backend-scheme.ffi :as ffi :refer [emit-ffi-node]]
            [jolt.backend-scheme.interop :refer [host-call-emit]]
            [jolt.backend-scheme.lambda :refer [emit-fn emit-nhint-coerce]]))

;; op-registry and its derived accessor tables live in jolt.op-registry — the
;; single source of truth for per-op facts, shared with the passes. Bind them to
;; local names here so the emit clauses read the same as before. (Qualified refs,
;; not :refer, so the seed mint can compile this ns before op-registry is loaded
;; into the running image — op-registry loads first at runtime.)
(def ^:private core-value-procs op-registry/core-value-procs)
(def ^:private bool-returning-ops op-registry/bool-returning-ops)
(def ^:private cmp1-ops op-registry/cmp1-ops)

;; Vars the host reaches by name (var-deref "jolt.backend-scheme" …) whose code
;; lives in a namespace this one requires, bound here so those lookups — and the
;; emit clauses below — read the same as before the split.
(def ^:private munge-name names/munge-name)
(def ^:private trace-frames? session/trace-frames?)
(def ^:private emit-quoted literals/emit-quoted)
(def ^:private fnsrc-src fnsrc/fnsrc-src)
(def ^:private ffi-type->chez ffi/ffi-type->chez)

;; The anon-literal counter fnsrc-name numbers a namespace's top-level
;; literals by (jolt.backend-scheme.fnsrc).
(defn seed-fnsrc-ns-counter!
  "Start namespace ns's anon-literal counter at n. The build does this as it
  emits each namespace: the counter otherwise carries whatever the in-process
  load of that namespace used — a source load consumes names, a cached one does
  not — and a namespace's emitted text must not depend on which it was, since
  the build caches one compiled unit per namespace on its text (#1059). The
  build seeds far above zero so a built binary's own runtime evaluation, whose
  counters start at 0, cannot reuse a baked name."
  [ns n]
  (swap! fnsrc/fnsrc-ns-counters assoc (str ns) n))

;; --- the emit-session switches --------------------------------------------
;; The drivers (bootstrap.ss, build.ss, compile-eval.ss, the gate harnesses) set
;; these by name. Each is a flag on the compilation unit, which
;; jolt.backend-scheme.session reads and where what each one means is written
;; down. `current-unit` is the public entry run-passes uses to share the emit
;; unit as its inference context too.
(defn set-emit-unit! [u] (reset! jolt.op-registry/current-unit-box u))
(defn current-unit [] (cur))
(defn set-prelude-mode! [on] (reset! (:prelude-mode? (cur)) (boolean on)))
(defn set-direct-link! [on] (reset! (:direct-link? (cur)) (boolean on)))
(defn set-seed-mint! [on] (reset! (:seed-mint? (cur)) (boolean on)))
(defn set-var-cache! [on] (reset! (:var-cache? (cur)) (boolean on)))
(defn set-target! [t] (reset! (:target (cur)) t))
(defn set-trace-frames! [on] (reset! (:trace-frames? (cur)) (boolean on)))
(defn set-source-reg! [on] (reset! (:source-reg? (cur)) (boolean on)))
(defn set-debug-locals! [on] (reset! (:debug-locals? (cur)) (boolean on)))

;; Fully-qualified app var names ("ns/name") already emitted with a direct-link
;; binding in the current unit; and, of those, the ones whose init is a fn literal
;; (safe to call as a raw Scheme application — a non-fn value is invokable in Clojure
;; but not a Scheme procedure, so its calls still route through jolt-invoke). A
;; call/value-ref direct-links only to a name defined earlier in emission order (or
;; itself), so the Scheme binding exists by the time the reference runs. Reset per build.
(defn direct-link-reset! []
  (reset! (:direct-link-defined (cur)) #{}) (reset! (:direct-link-fns (cur)) #{}))


;; whole-program pre-pass accumulator: impl keys (ns.type|proto|method) for
;; contagion-eligible impls. The build driver calls contagion-prepass! per-ns (it
;; knows the ns) then contagion-prepass-done!, which makes clone-sites the
;; eligible-impl set. clone-sites is consulted ONLY by the :devirt-type emit clause
;; — PIC and generic protocol dispatch never look at it — so a devirt site over an
;; eligible impl resolves the clone (the win, gated to monomorphic call sites) while
;; a megamorphic site over the same impl stays on the PIC path (untouched). That is
;; the brief's design: "the gate must be the call site's dispatch mode," not the
;; impl's. We do NOT intersect with devirt-reached sites: the devirt annotation only
;; lands on nodes during the emit re-analysis (after wp-infer! has populated pm-rets),
;; so it is absent from the pre-pass's first-analysis nodes — intersecting would
;; silently drop every site (the clones would be emitted but never resolved).
;; Resolving at a devirt site over an eligible impl is value-identical by the
;; invariant, so there is no correctness cost; a clone for an eligible impl that no
;; devirt site reaches is dead (kept by DCE via register-clone*) — a size, not
;; correctness, concern, checked via the binary-size gate.
(defn reset-clone-prepass! [unit]
  (reset! (:clone-impl-keys unit) #{}) (reset! (:clone-sites unit) nil)
  (types/reset-clone-double-ret! unit))
(declare register-impl-invoke)
(defn contagion-prepass! [unit nodes ns]
  (letfn [(walk [n]
            (when-some [[type-name proto method fc] (register-impl-invoke n)]
              (let [ars (:arities fc)]
                (when (= 1 (count ars))
                  (let [res (types/contagion-specialize-arity unit (first ars) type-name)]
                    (when (nth res 1)
                      (let [tag (str ns "." type-name)]
                        (swap! (:clone-impl-keys unit) conj (clone-site-key tag proto method))
                        ;; the clone's return is :double -> the devirt sites that
                        ;; resolve it type their return :double per-site (infer-call),
                        ;; so a caller accumulator add over them lowers to fl+.
                        (when (nth res 2)
                          (types/add-clone-double-ret! unit tag proto method))))))))
            (ir/reduce-ir-children (fn [_ c] (walk c) nil) nil n))]
    (run! walk nodes)
    nil))
(defn contagion-prepass-done! [unit]
  (reset! (:clone-sites unit) (not-empty @(:clone-impl-keys unit))) nil)
;; Ops that pass their tail position through to a child the leaf rule then
;; wraps. A try is NOT one here, unlike tail-transparent-ops: a guard in tail
;; position erases the enclosing frame, so the whole try is the leaf.
(def ^:private dbg-structural-ops #{:if :do :let :loop :recur})


;; The casts the :coerce arm lowers on a proven double, each with the suffix
;; of its window constants (converters.ss: jolt-cast-lo-<s> / jolt-cast-hi-<s>).
;; The unchecked casts share the checked ones' windows: inside the window the
;; two agree, and outside it the helper does the saturating. The same seven
;; strings as jolt.passes.numeric's lowered-cast-fns.
(def ^:private cast-window-suffix
  {"jolt-long-cast" "long" "jolt-unchecked-long" "long"
   "jolt-int-cast" "int" "jolt-unchecked-int" "int"
   "jolt-short-cast" "short" "jolt-byte-cast" "byte" "jolt-char" "char"})

;; What a double or float the JVM holds primitive (:fl-prim) calls, where RT's
;; primitive overload differs from its Object one (converters.ss). The unchecked
;; casts have no check to differ on.
(def ^:private cast-prim-entry
  {"jolt-byte-cast" "jolt-byte-cast-prim" "jolt-short-cast" "jolt-short-cast-prim"
   "jolt-int-cast" "jolt-int-cast-prim" "jolt-long-cast" "jolt-long-cast-prim"
   "jolt-char" "jolt-char-prim"})
(defn- cast-entry [node]
  (if (:fl-prim node)
    (get cast-prim-entry (:cast-fn node) (:cast-fn node))
    (:cast-fn node)))


(declare emit)
(declare emit*)
;; Ops that pass tail position through to a child (the child can itself be a tail
;; call): if/do carry it to their tail branch/last form, let/loop to their body,
;; and invoke reads it to decide whether the call is tail. Every other op's
;; children are non-tail, so `emit` clears *tail?* before dispatching them — that
;; way a stray true can't leak into, say, a call sitting in a vector literal.
;; :throw is tail-transparent so the :throw emit case still sees *tail?* — a
;; TAIL throw must store the site-vreg pair (sited-tail-call) or its TCO-erased
;; frame has no name at report time (app.tailstale's thrower). :host-call for
;; the same reason: a (.concat s nil) in tail position raises from inside the
;; host, and without the site the fn it sat in is the one frame the report
;; cannot recover.
(def ^:private tail-transparent-ops #{:if :do :let :loop :invoke :throw :host-call :try})
;; A try is tail-transparent too. With neither a catch nor a finally, emit-try
;; emits it as its body and nothing else, so a tail call inside one is a real tail
;; call. With a catch or a finally the body runs inside a guard or dynamic-wind,
;; so its last call is not a Chez tail call — but the guard itself is, and it
;; erases the enclosing fn's frame all the same. The body's last call is then the
;; fn's exit: storing its site and registering it as a tail edge is what lets a
;; trace, or Thread.getStackTrace, name the fn the try sat in. Treating the try
;; as opaque lost every frame from the try outwards —
;; (defn wrapped [x] (try (boom x))) reported `boom` and then stopped. The
;; finally body is not the fn's exit; emit-try emits it non-tail.
(defn- tail-transparent? [node]
  (contains? tail-transparent-ops (:op node)))
(declare emit-plain)
(defn emit [node]
  (if (and *dbg-scope* *tail?* (not (contains? dbg-structural-ops (:op node))))
    (let [r (fresh-label "_dr$")]
      (str "(let ((" r " " (binding [*tail?* false] (emit-plain node)) ")) (keep-live "
           *dbg-scope* ") " r ")"))
    (emit-plain node)))
(defn- emit-plain [node]
  (let [s (if (and *tail?* (not (tail-transparent? node)))
            (binding [*tail?* false] (emit* node))
            (emit* node))
        s (with-once-clears node s)
        ;; A call typed from its var's declared return (:num-ret-guard, numeric
        ;; pass) checks what it answered: a stale site, after the var is
        ;; redefined, otherwise hands an unsafe fl op a string. Anything else
        ;; goes through jolt-double: a number widens, as generic arithmetic would
        ;; take it, and a non-number raises ClassCastException. A double is safe
        ;; wherever a :float is, since every consumer unboxes with jfloat-unbox. In
        ;; tail position the value leaves the fn, so no fl op here takes it.
        guard (and (not *tail?*) (:num-ret-guard node))
        ;; An fl op taking a guarded :float call reads the double straight out of
        ;; the Float, under the one test the guard makes (:fl-unbox below then has
        ;; nothing left to do).
        fused? (and (= guard :float) (:fl-unbox node))
        s (if guard
            (let [t (fresh-label "_nr$")]
              (cond
                fused?
                (str "(let ((" t " " s ")) (if (jfloat? " t ") (jfloat-fl " t ") (jolt-double " t ")))")
                (= guard :float)
                (str "(let ((" t " " s ")) (if (jfloat? " t ") " t " (jolt-double " t ")))")
                :else
                (str "(let ((" t " " s ")) (if (flonum? " t ") " t " (jolt-double " t ")))")))
            s)]
    ;; a :long operand of a :double-specialized op is tagged :fl-coerce by
    ;; jolt.passes.numeric so it widens to a flonum here (JVM long->double
    ;; widening). The obvious emit is a bare (fixnum->flonum s), and that is what
    ;; this did — on the premise that the :long contract guarantees a real fixnum.
    ;; It does not: jolt's ^long is 64-bit and a Chez fixnum is 61, so a :long
    ;; operand between 2^60 and 2^63 is a BIGNUM at runtime and fixnum->flonum
    ;; raises on it. ((fn [^double a ^long b] (+ a b)) 1.5 (bit-shift-left 1 62))
    ;; failed outright where the reference answers 4.61e18.
    ;;
    ;; Guard the fixnum case instead of widening the contract: the fast path is
    ;; the same inlined fixnum->flonum, and a bignum falls to jolt->fl, which
    ;; converts it exactly as the reference's long->double does.
    (cond
      (:fl-coerce node)
      (let [t (fresh-label "_fc$")]
        (str "(let ((" t " " s "))"
             " (if (fixnum? " t ")"
             " (fixnum->flonum " t ")"
             " (jolt->fl " t ")))"))
      ;; a :float operand of a :double op: a float cast already emitted its
      ;; double (the :coerce arm); anything else of the kind holds a jfloat
      (and (:fl-unbox node) (not (= :coerce (:op node))) (not (:fl-float node)) (not fused?))
      (str "(jfloat-unbox " s ")")
      :else s)))



;; Emit a call `(ctor a0 a1 ...)` with the args evaluated LEFT-TO-RIGHT. Chez's
;; procedure-argument evaluation order is unspecified (in practice right-to-left),
;; but Clojure evaluates collection-literal elements left to right, so a literal
;; like [(read r) (read r)] over side-effecting reads must bind in source order.
;; Delegates to ordered-call (needs-order?): wrap in a let* of fresh temps only
;; when two or more operands could have observable effects, so a literal over
;; locals/consts stays un-wrapped. `nodes` are the item IR nodes (for the
;; side-effect check); each is emitted once here.
(defn- emit-ordered [ctor nodes]
  (ordered-call nodes (mapv emit nodes)
    (fn [strs] (str "(" ctor (if (empty? strs) "" (str " " (str/join " " strs))) ")"))))

;; A def's :meta is a jolt map value. Non-empty? (a plain def carries {}).
(defn- jmeta-nonempty? [m] (and (map? m) (pos? (count m))))

;; The meta argument to def-var-with-meta!. When the analyzer attached a
;; :meta-expr (metadata with values to evaluate, e.g. ^{:a some-fn}), emit it as a
;; runtime expression; otherwise the static :meta map as quoted data.
;;
;; *fnsrc-def-init?* is cleared here. It means "this literal IS the def's direct
;; init, which the def's own define names, so the def already registers for it" --
;; true of the init and of nothing else in the def. But defn always expands to
;; (def ^{...} name (fn ...)), so the :def arm sets the flag for the WHOLE def, and
;; a literal inside the attr-map was then skipped as if the define named it. A
;; ^{:inline (fn ...)} was never registered, so a macro splicing that value out of
;; (meta #'f) had no source to rebuild it from -- "Cannot compile this value into
;; code".
;;
;; The expression gets the init's cache-cell scope. Its values are mostly fns —
;; every deftest body is the :test fn in its def's metadata — and without the scope
;; each var they reference resolved by name per call: a loop calling an aliased
;; var ran 66 ns/iter in a deftest body against 8 ns in a defn. Gated on
;; var-cache? like emit-top-cells, so the seed mint's output does not move.
(defn- emit-def-meta [node]
  (if (:meta-expr node)
    (binding [*fnsrc-def-init?* false]
      (if (var-cache?)
        (emit-with-cells #(emit (:meta-expr node)))
        (emit (:meta-expr node))))
    (emit-quoted (:meta node))))

(defn- emit-binding [b]
  (str "(" (munge-name (nth b 0)) " " (emit (nth b 1)) ")"))

;; letfn lowers to a :let flagged :letrec (mutually-recursive named local fns):
;; Scheme `letrec*` binds them so each sees its siblings. A plain let uses let*.
(declare emit-let-dbg)
(defn- emit-let [node]
  (let [kw (if (:letrec node) "letrec*" "let*")
        bs (:bindings node)
        names (map #(munge-name (nth % 0)) bs)
        ;; a binding that reuses a hoisted ^doubles param's name shadows its
        ;; flvector from there on (*array-vecs*). let* binds sequentially, so
        ;; each init is emitted under the names bound BEFORE it; letrec* binds
        ;; every name up front.
        av-body (apply dissoc *array-vecs* names)]
    (if *dbg-scope*
      (emit-let-dbg node kw names av-body)
      ;; bindings are non-tail; the body inherits the let's tail position
      (let [binds (binding [*tail?* false
                            *letrec-binders* (if (:letrec node)
                                               (into *letrec-binders*
                                                     (map #(nth % 0) bs))
                                               *letrec-binders*)]
                    (cond
                      (empty? *array-vecs*) (str/join " " (mapv emit-binding bs))
                      (:letrec node) (binding [*array-vecs* av-body]
                                       (str/join " " (mapv emit-binding bs)))
                      :else (loop [bs bs av *array-vecs* acc []]
                              (if (empty? bs)
                                (str/join " " acc)
                                (let [b (first bs)
                                      s (binding [*array-vecs* av] (emit-binding b))]
                                  (recur (rest bs) (dissoc av (munge-name (nth b 0))) (conj acc s)))))))]
        (str "(" kw " (" binds ") " (binding [*array-vecs* av-body] (emit (:body node))) ")")))))

;; emit-let under debug locals (*dbg-scope*): each binding is followed by a scope
;; record holding it, so an init sees the locals bound before it and the body all
;; of them; a letrec's names get one record after the group. The body is emitted
;; as a tail region, so its leaves keep the innermost record live.
(defn- emit-let-dbg [node kw names av-body]
  (let [bs (:bindings node)]
    (if (:letrec node)
      (let [binds (binding [*tail?* false
                            *letrec-binders* (into *letrec-binders* (map #(nth % 0) bs))
                            *array-vecs* av-body]
                    (str/join " " (mapv emit-binding bs)))
            v (fresh-label "_dl$")
            rec (dbg-record *dbg-scope* (map #(nth % 0) bs) names)]
        (str "(" kw " (" binds " (" v " " rec ")) "
             (binding [*array-vecs* av-body *dbg-scope* v *tail?* true] (emit (:body node))) ")"))
      (loop [bs bs av *array-vecs* scope *dbg-scope* acc []]
        (if (empty? bs)
          (str "(" kw " (" (str/join " " acc) ") "
               (binding [*array-vecs* av-body *dbg-scope* scope *tail?* true] (emit (:body node))) ")")
          (let [b (first bs)
                nm (munge-name (nth b 0))
                init (binding [*tail?* false *array-vecs* av *dbg-scope* scope] (emit-binding b))]
            (if (dbg-hidden? (nth b 0))
              (recur (rest bs) (dissoc av nm) scope (conj acc init))
              (let [v (fresh-label "_dl$")]
                (recur (rest bs) (dissoc av nm) v
                       (conj acc init (str "(" v " " (dbg-record1 scope (nth b 0) nm) ")")))))))))))

(defn- emit-loop [node]
  (let [label (fresh-label "loop")
        pairs (:bindings node)
        names (map #(munge-name (nth % 0)) pairs)
        ;; inits evaluate in the OUTER scope (recur-target unchanged) and, like
        ;; Clojure loop/let, SEQUENTIALLY — wrap a let* around the named let.
        ;; sequential, so a loop var reusing a hoisted ^doubles param's name
        ;; shadows its flvector for the inits after it and for the body
        ;; (*array-vecs*).
        inits (binding [*tail?* false]
                (if (empty? *array-vecs*)
                  (mapv #(emit (nth % 1)) pairs)
                  (loop [ps pairs av *array-vecs* acc []]
                    (if (empty? ps)
                      acc
                      (let [p (first ps)
                            s (binding [*array-vecs* av] (emit (nth p 1)))]
                        (recur (rest ps) (dissoc av (munge-name (nth p 0))) (conj acc s)))))))
        seq-bs (str/join " " (map (fn [n i] (str "(" n " " i ")")) names inits))
        rebinds (str/join " " (map (fn [n] (str "(" n " " n ")")) names))
        ;; the loop body inherits the loop's tail position
        dv (when *dbg-scope* (fresh-label "_dl$"))
        body (binding [*recur-target* label
                       *array-vecs* (apply dissoc *array-vecs* names)
                       *dbg-scope* (or dv *dbg-scope*)
                       *tail?* (or (some? dv) *tail?*)]
               (emit (:body node)))
        ;; under debug locals each iteration binds a record of the loop's names
        body (if dv
               (str "(let ((" dv " " (dbg-record *dbg-scope* (map #(nth % 0) pairs) names) ")) " body ")")
               body)]
    (str "(let* (" seq-bs ") (let " label " (" rebinds ") " body "))")))

(defn- emit-recur [node]
  (when-not *recur-target* (throw (ex-info "emit: recur outside a loop/fn target" {})))
  (let [arg-nodes (:args node)]
    (if *dbg-scope*
      ;; debug locals: the args go to temps and the scope is kept live past them,
      ;; so a call in an arg sees the locals; the jump itself stays a tail call
      (let [as (mapv emit arg-nodes)
            tmps (mapv (fn [_] (fresh-label "_ra$")) as)]
        (str "(let* (" (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps as)) ")"
             " (keep-live " *dbg-scope* ") (" *recur-target* " " (str/join " " tmps) "))"))
      (ordered-call arg-nodes (mapv emit arg-nodes)
                    (fn [as] (str "(" *recur-target* " " (str/join " " as) ")"))))))

;; (get m k [default]) specialized by what the receiver and key are.
(defn- emit-get-call [tail? arg-nodes order-args]
  (let [recv (first arg-nodes) key-node (second arg-nodes)
        idx (when (and (= 2 (count arg-nodes))
                       (= :struct (:hint recv))
                       (= :const (:op key-node))
                       (keyword? (:val key-node)))
              (struct-field-index (:shape recv) (:val key-node)))
        dir (when (and idx (not (:nilable recv)))
              (direct-field-accessor (:shape recv) idx))]
    (cond
      dir  (order-args (fn [as] (str "(" dir " " (first as) ")")))
      idx  (order-args (fn [as] (str "(jrec-field-at " (first as) " " idx " " (emit key-node) ")")))
      :else (do (note-tail-call! tail?)
                (order-args (fn [as] (lookup-restore tail? (str "(jolt-get " (str/join " " as) ")"))))))))

;; (:k m [default]): a keyword in call position.
(defn- emit-keyword-call [tail? fnode arg-nodes order-args defstr]
  (let [_ (note-tail-call! tail?)
        recv (first arg-nodes)
        idx (when (and (= :struct (:hint recv)) (= 1 (count arg-nodes)))
              (struct-field-index (:shape recv) (:val fnode)))
        dir (when (and idx (not (:nilable recv)))
              (direct-field-accessor (:shape recv) idx))]
    (cond
      dir  (order-args (fn [as] (str "(" dir " " (first as) ")")))
      idx  (order-args (fn [as] (str "(jrec-field-at " (first as) " " idx " " (emit fnode) ")")))
      ;; Any other receiver: the site lookup over a per-site cell that
      ;; remembers the array-map slot the key was last found at (collections.ss
      ;; jolt-kw-get-site — one eq? on a hit, jolt-get's answer on anything
      ;; else). The cell is a hoisted per-site constant, so only a site with a
      ;; constant pool (inside a def) gets one; a bare top-level form keeps
      ;; jolt-get rather than allocate a cell per evaluation.
      *const-pool*
      (let [site (hoist-const-per-site "(jolt-kw-site)")]
        (order-args (fn [as] (lookup-restore tail? (str "(jolt-kw-get-site " (first as) " " (emit fnode) " " site (defstr as) ")")))))
      :else (order-args (fn [as] (lookup-restore tail? (str "(jolt-get " (first as) " " (emit fnode) (defstr as) ")")))))))

;; A collection literal or value in call position.
(defn- emit-coll-call [tail? fnode arg-nodes args]
  (do
   (note-tail-call! tail?)
   (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                (fn [[c & as]]
                  (lookup-restore tail?
                    (str (if (= :vector (:op fnode))
                           "(jolt-vec-invoke1 "
                           "(jolt-get ")
                         c " " (str/join " " as) ")"))))))

;; An :invoke's emission context, built once by emit-invoke and read by the arms
;; below: the node and its callee, whether the call is in tail position, TWO
;; parallel vectors — :arg-nodes, the IR NODES, and :args, their EMITTED STRINGS
;; (anything that asks a question about an argument — its type, its :num-kind,
;; whether it is a constant — has to read :arg-nodes; :args can only be spliced
;; into output, and calling (:op …) on one yields nil and fails silently, which
;; is a real bug this file has already shipped once) — the trace line and inline
;; chain, the native op and IFn-literal kind of the callee at this arity, and
;; :order-args, which orders the args left-to-right (build receives the spliced
;; operand strings).
;;
;; jolt-invoke dispatch: Clojure evaluates the fn expr before the args, so
;; order [callee & args] together when ordering is observable. Pick a
;; fixed-arity entry point (jolt-invoke0..4) by arg count so the common
;; raw-procedure fast path allocates no rest-list; keep variadic jolt-invoke
;; for larger arities / apply tails.
;;
;; jolt-invokeN puts the site slot back when the fn value returns (seq.ss):
;; this code may be untraced (clojure.core) and the fn a traced callback,
;; whose tail site would otherwise outlive it. A tail call must stay one,
;; and a traced non-tail call restores the slot itself (site-restore), so
;; both take jolt-invoke-tN, which does not.
(defn- emit-jolt-invoke [c]
  (let [args (:args c)
        callee (if (<= (count args) 4)
                 (str (if (or (:tail? c) (trace-frames?)) "jolt-invoke-t" "jolt-invoke")
                      (count args))
                 "jolt-invoke")]
    (ordered-call (cons (:fnode c) (:arg-nodes c)) (cons (emit (:fnode c)) args)
                  (fn [operands] (emit-call (:tail? c) callee operands (:tl c) (:ich c))))))

;; The default value of a lookup's optional third operand, as spliced text.
(defn- default-arg-str [as] (if (> (count as) 1) (str " " (nth as 1)) ""))

;; The callee by what it is, once no annotation or native op has claimed the call.
(defn- emit-var-invoke [c]
  (let [node (:node c) fnode (:fnode c) arg-nodes (:arg-nodes c) args (:args c)
        tail? (:tail? c) tl (:tl c) ich (:ich c) order-args (:order-args c)]
    (cond
      ;; the type test guard-declared-records (jolt.passes.types) puts around a
      ;; body whose param carries a ^Record hint the program wrote: per value, the
      ;; type's descriptor from a per-site cell, then jrec? and jrec-desc, which
      ;; the adapter open-codes (record ops are syntax on their constant rtd), so
      ;; the test is a few loads with no call. The descriptor is #f until the type
      ;; is defined, and then nothing matches.
      (and (= "jolt.host" (:ns fnode)) (= "rec-type?" (:name fnode))
           (some? (:rec-guard node)) (even? (count arg-nodes)))
      (emit-rec-type-check node arg-nodes order-args)
      ;; closed-world direct call: the callee var is an app fn def already emitted
      ;; with a Scheme binding — apply it directly, no var lookup, no jolt-invoke.
      ;; Only fn-valued defs qualify; a non-fn invokable value (a map/set/keyword
      ;; held in a var) isn't a Scheme procedure, so it falls through to jolt-invoke
      ;; below (which still uses the direct binding as the invoke target).
      (and (direct-linkable? (:ns fnode) (:name fnode))
           (direct-link-fn? (:ns fnode) (:name fnode))
           (direct-link-callable? (:ns fnode) (:name fnode) (count args)))
      (order-args (fn [as] (emit-call tail? (dl-name (:ns fnode) (:name fnode)) as tl ich)))
      ;; closed-world direct call to a SEED var — clojure.core and the other
      ;; namespaces the runtime image boots with. Such a var is preloaded ahead
      ;; of every app def and never emitted by this build, so the def binds its
      ;; root procedure once at load and the site applies it directly: no
      ;; var-cell-deref (7 ns), no jolt-invokeN (5 ns) — true? went 16 -> 4 ns.
      ;; jolt.host/seed-callable? applies the same closed-world rule an app def
      ;; gets (not ^:dynamic/^:redef, not redefined by the app) plus "root is a
      ;; procedure whose arity mask admits this arity", so a keyword/map/multi-
      ;; method-valued var and a wrong-arity call keep jolt-invoke below.
      ;; seed-callable? answers the callee's jv$ binding name for a var the seed
      ;; minted direct-linked (def-var-linked!), and the site applies that
      ;; top-level variable: the same one load, and a later def / alter-var-root
      ;; / with-redefs of the var writes through to it (rt.ss var-root-set!), so
      ;; the site follows a redefinition. A seed var the runtime defined itself
      ;; (a Scheme def-var!) has no binding to name; its root is hoisted once at
      ;; load, and a redefinition is invisible to that site, as under the JVM's
      ;; direct linking. `jolt run` never direct-links. Off while MINTING the
      ;; seed: the vars this arm would bind are the ones being emitted.
      (and (direct-link?) (not (seed-mint?))
           (seed-callable? nil (:ns fnode) (:name fnode) (count args)))
      (let [sc (seed-callable? nil (:ns fnode) (:name fnode) (count args))
            head (if (string? sc) sc (hoist-seed-root (:ns fnode) (:name fnode)))]
        (order-args (fn [as] (emit-call tail? head as tl ich))))
      ;; record ctor with matching arity: inline the native per-arity ctor
      ;; (make-jrecN) directly — desc + ext + one inline slot per field —
      ;; eliminating jolt-invoke / var-deref / rest-list / ctor call / hashtable
      ;; lookup AND the field vector. After per-site desc-cell warmup the hot
      ;; path is: cell read -> make-jrecN — one allocation, no lookups, no
      ;; dispatch. The shape comes from the unit's record-shapes (set-record-shapes!).
      (let [shape (get (ctor-shapes) (str (:ns fnode) "/" (:name fnode)))]
        (and shape (= (count (get shape :fields)) (count args)) (<= (count args) 6)))
      (emit-record-ctor (get (ctor-shapes) (str (:ns fnode) "/" (:name fnode))) arg-nodes args)
      ;; a late-bound :var call head can hold a procedure OR a non-applicable
      ;; value the RT dispatches (multimethod, keyword/coll IFn) — route via
      ;; jolt-invoke (transparent for a procedure).
      :else (emit-jolt-invoke c))))

;; a :local callee: a known procedure (the letrec-bound self-name of a named
;; fn — i.e. self-recursion) is a real Scheme proc, so call it directly with
;; no jolt-invoke / arg consing; case-lambda handles arity. Any other local
;; holds an arbitrary IFn -> dynamic dispatch.
(defn- emit-local-invoke [c]
  (let [fnode (:fnode c)]
    (if (*known-procs* (munge-name (:name fnode)))
      ;; a ^:once body's last use of a captured callee: released before the
      ;; call (the local already holds it), which keeps the call in tail position
      (let [call ((:order-args c)
                  (fn [as] (emit-call (:tail? c) (munge-name (:name fnode)) as (:tl c) (:ich c))))
            st (when-let [cc (:clear-caps fnode)] (once-clear-stmts cc))]
        (if st (str "(begin " st " " call ")") call))
      (emit-jolt-invoke c))))

;; The callee by the kind of head it is.
(defn- emit-head-invoke [c]
  (let [fnode (:fnode c)]
    (cond
      (and (stdlib-var? fnode) (not (prelude-mode?)))
      (throw (ex-info (str "emit: unsupported stdlib fn `" (:ns fnode) "/" (:name fnode)
                           "` (no core on Chez yet)") {}))
      ;; static method call (Class/method arg*). With a const pool the site gets a
      ;; cache (host-static.ss host-static-proc-site): it answers the procedure the
      ;; call applies, so a warm call is one epoch compare and a plain application
      ;; instead of three string-keyed lookups and a rest list. Gated like the
      ;; instance? site, so the seed mint stays byte-identical.
      (= :host-static (:op fnode))
      (emit-host-static-call (:tail? c) fnode (:order-args c))
      (= :host (:op fnode))
      (throw (ex-info (str "emit: unsupported host call `" (:name fnode) "`") {}))
      (= :local (:op fnode)) (emit-local-invoke c)
      (= :var (:op fnode)) (emit-var-invoke c)
      ;; a computed callee can yield ANY IFn — route through jolt-invoke.
      :else (emit-jolt-invoke c))))

;; Calls to a native op, or to a keyword or collection literal.
(defn- emit-native-invoke [c]
  (let [fnode (:fnode c) arg-nodes (:arg-nodes c) args (:args c) nop (:nop c)
        tail? (:tail? c) order-args (:order-args c)]
    (cond
      (and nop (= 1 (count args)) (cmp1-ops nop)) (str "(begin " (first args) " #t)")
      ;; (get coll k [default]) with a struct-typed coll — the inference marked the
      ;; receiver with :hint :struct and a :shape matching the declared field layout.
      ;; Read the field by its static slot instead of the generic jolt-get dispatch.
      ;; A proven-NON-NIL receiver emits the direct per-arity accessor (jrecN-fI) —
      ;; zero dispatch, one load; a nilable receiver keeps jrec-field-at, which falls
      ;; back to jolt-get on nil. Only the 2-arg (no-default) form takes this path
      ;; since a declared field is always present. The key must be a compile-time
      ;; keyword literal so (struct-field-index) can resolve it.
      (and (= nop "jolt-get") (<= 2 (count arg-nodes) 3))
      (emit-get-call tail? arg-nodes order-args)
      ;; bit-and/or/xor/not: open-code the fixnum case, helper as the slow arm.
      ;; These moved off the raw Chez bitwise primitives — which Chez inlines to
      ;; native code — onto jolt-bit-* helpers so a bad operand raises a CLASSED
      ;; IllegalArgumentException in call position as well as value position. The
      ;; helper coerces both operands through ->int, and it is a cross-unit call
      ;; cp0 cannot inline, so the whole thing costs ~2x a raw bitwise-xor even
      ;; with ->int's fixnum fast path (1.28M ops: 4ms raw, 8ms through the
      ;; helper, 4ms with this inline).
      ;;
      ;; Soundness: ->int accepts exactly the exact integers in signed 64-bit
      ;; range and raises on everything else, and a Chez fixnum here is 61-bit,
      ;; so a fixnum ALWAYS took ->int's accept branch. On fixnum operands
      ;; fxand/fxior/fxxor/fxnot agree with the generic bitwise-* ops and always
      ;; yield a fixnum (the result's bit pattern is bounded by the operands'),
      ;; so the fast arm is value-identical. Every operand that COULD raise —
      ;; a non-integer, a ratio, an out-of-range bignum — still reaches the
      ;; helper, which owns the raise. The exception path is untouched.
      ;;
      ;; A proven :long operand does NOT license skipping the test: jolt's ^long
      ;; is 64-bit and a fixnum is 61, so a :long can be a bignum at runtime.
      ;; That is why this is a runtime guard and not a :lng registry entry.
      ;; Operands are let*-bound because each appears twice; let* also fixes
      ;; left-to-right evaluation, so this does not need order-args.
      (and nop (bit-fx-ops nop) (= (count args) (second (bit-fx-ops nop))))
      (emit-bit-fx-call tail? args (:tl c) (:ich c) nop)
      ;; a generic native op.
      nop (order-args (fn [as] (emit-call tail? nop as (:tl c) (:ich c))))
      ;; (:k coll [default]) -> (jolt-get coll :k [default]) — the key (fnode) is a
      ;; const, so only the coll/default args carry order. When the inference typed
      ;; the receiver as a record whose declared fields include :k (it carries the
      ;; field-order :shape), read the field by its static slot — no field-key
      ;; lookup, no jolt-get dispatch. A proven-non-nil receiver emits the direct
      ;; per-arity accessor (jrecN-fI); a nilable one keeps jrec-field-at. Only the
      ;; no-default form (a declared field is always present, so a default is never
      ;; taken).
      (= (:kind c) :keyword)
      (emit-keyword-call tail? fnode arg-nodes order-args default-arg-str)
      ;; (coll k [default]) -> lookup — coll (fnode) is the callee, evaluated
      ;; before the key/default args. A VECTOR literal invokes as nth (a bad
      ;; index throws, IPersistentVector.invoke); maps/sets invoke as get.
      (= (:kind c) :coll)
      (emit-coll-call tail? fnode arg-nodes args)
      :else (emit-head-invoke c))))

;; (instance? T x) asks the same question at nearly every call — T is a literal
;; or a type's ctor value — so the site gets an inline cache (records-interop.ss
;; jolt-instance-site): the last T by identity and the last receiver kind,
;; answered by two eq?s and an epoch compare. Without it every call interned the type name as a symbol and went
;; through instance-check's type-argument normalization and two memo tables,
;; 37-54 ns against the JVM's inlined instanceof. The site object is a per-site
;; constant, so this needs a pool; it also needs var-cache? so the seed mint
;; stays byte-identical, and a callee that IS clojure.core's — an ns that
;; defined its own instance-check resolves elsewhere and is called as written.
(defn- instance-site-call? [fnode arg-nodes]
  (and *const-pool* (var-cache?)
       (= :var (:op fnode)) (= "clojure.core" (:ns fnode)) (= "instance-check" (:name fnode))
       (= 2 (count arg-nodes))))

;; Calls an earlier pass annotated with how to lower them.
(defn- emit-annotated-invoke [c]
  (let [node (:node c) arg-nodes (:arg-nodes c) tail? (:tail? c)
        tl (:tl c) ich (:ich c) order-args (:order-args c)]
    (cond
      (instance-site-call? (:fnode c) arg-nodes)
      (let [site (hoist-const-per-site "(jolt-instance-site-make)")]
        (note-tail-call! tail?)
        (order-args (fn [as] (str "(jolt-instance-site " site " " (first as) " " (second as) ")"))))
      ;; devirtualized protocol call: the inference proved the receiver (arg 0) is
      ;; one record type, so resolve the impl by that static tag instead of routing
      ;; through the protocol var -> jolt-invoke -> protocol-resolve (which recomputes
      ;; the tag and walks the type table). devirt-resolve does the same table lookup
      ;; the dispatch would, but with no var-deref and no receiver-type computation;
      ;; it falls back to ordinary dispatch when the static tag has no direct impl (a
      ;; record satisfying the protocol via an Object/host-tag default). Fires only on
      ;; a monomorphic site (a megamorphic receiver joins to :any, no :devirt-type).
      ;; The receiver is bound once — it feeds both the resolve and the application.
      (:devirt-type node)
      (emit-devirt-call node tail? order-args)
      ;; polymorphic inline cache: a protocol call the inference recognized (:proto)
      ;; but could NOT prove monomorphic (no :devirt-type — a megamorphic / unknown
      ;; receiver). Emit a per-site cache keyed on the receiver's descriptor identity:
      ;; after warmup each call is one field read + an eq? scan over <= jolt-pic-n
      ;; cached descs + a direct apply, with no string hashing or table walk. The
      ;; cache lives in a def-closure cell (emit-with-cells); a register-protocol-
      ;; method after population bumps jolt-proto-epoch and the cache re-resolves,
      ;; so an extend-type at runtime can't strand a stale impl. Falls back to a
      ;; direct protocol-resolve (still the per-descriptor fast path) when not inside
      ;; a def. The receiver is bound once and feeds both the resolve and the apply.
      (:proto node)
      (emit-pic-call node tail? order-args)
      ;; a java.lang.Math call jolt.passes.numeric proved is over flonum operands:
      ;; emit the native Chez flonum op (flsqrt/flatan/…) instead of the generic
      ;; string-keyed host-static-call, and (via its :double result kind) keep the
      ;; surrounding arithmetic unboxed.
      ;; (aget ^doubles a i): jolt.passes.numeric proved the array is a flvector. A
      ;; proven-:long (or fixnum-literal) index (:fl-idx-long) reads the backing
      ;; flvector INLINE — (flvector-ref (jolt-array-vec A) I) — so a call-position
      ;; flonum stays unboxed through the surrounding fl+ chain instead of being boxed
      ;; at a jolt-flaget procedure boundary. An unproven index keeps (jolt-flaget A I),
      ;; which owns the fixnum?/na-idx coercion; the inline flvector-ref's own range
      ;; check is the bounds contract on the hot path (a pre-check regresses ~11%).
      ;; A ^doubles PARAM (the local is in *array-vecs*) reads the flvector its
      ;; arity bound at entry; any other proven array re-reads the accessor.
      ;; A ^floats read (:fl-float) is the Float unless an fl op takes it
      ;; (:fl-unbox), which reads the double it holds and allocates nothing.
      (:fl-aget node)
      (emit-fl-aget node tail? arg-nodes tl ich order-args)
      ;; (aset ^doubles a i v): proven index AND :double value (:fl-idx-long +
      ;; :fl-val-double) store inline — (let ((v V)) (flvector-set! (jolt-array-vec A)
      ;; I v) v) — and return the stored value (JVM contract; the let evaluates V once).
      ;; Otherwise keep (jolt-flaset A I V), which owns exact->inexact for a non-double.
      ;; ^floats (:fl-float) rounds the value to single precision as it stores it,
      ;; and answers the Float unless an fl op takes the result (:fl-unbox).
      (:fl-aset node)
      (emit-fl-aset node tail? arg-nodes tl ich order-args)
      ;; (aget ^longs/^ints/^bytes/^objects a i) and its aset twin. A boxed backing
      ;; cannot unbox, so there is no inline form and no hoisted vector — the win is
      ;; skipping jolt-nth's dispatch walk, which the call already gets.
      (:v-aget node) (order-args (fn [as] (emit-call tail? "jolt-vaget" as tl ich)))
      (:v-aset node) (order-args (fn [as] (emit-call tail? "jolt-vaset" as tl ich)))
      ;; (aset ^bytes a i v): the store's own helper, because the byte kind narrows
      ;; to signed 8 bits and must answer what it stored — jolt-vaset answers its
      ;; argument. No inline form: there is nothing to unbox on the way out.
      (:b-aset node) (order-args (fn [as] (emit-call tail? "jolt-baset" as tl ich)))
      (:fl-op node) (order-args (fn [as] (str "(" (:fl-op node) " " (str/join " " as) ")")))
      ;; the integer twin of :fl-op — a java.lang.Math member over proven fixnum
      ;; operands, lowered to its jolt-l-* macro (jolt.passes.numeric math-lng-ops).
      (:lng-op node) (order-args (fn [as] (str "(" (:lng-op node) " " (str/join " " as) ")")))
      ;; a clojure.core call the collection lattice proved reduces to a Chez
      ;; primitive — (count s) / (str a b) over proven strings (jolt.passes.types
      ;; str-prim-op). No var deref, no jolt-invoke, no type dispatch.
      (:prim-op node) (order-args (fn [as] (str "(" (:prim-op node) " " (str/join " " as) ")")))
      ;; hint-directed fast arithmetic: jolt.passes.numeric proved every operand a
      ;; flonum (^double) or fixnum (^long), so emit the Chez fl*/fx* op.
      (:num-kind node) (emit-numeric (:num-kind node) (:name (:fnode c)) (:args c) order-args)
      ;; a (/ ...) with a primitive double or float operand (jolt.passes.numeric
      ;; :div-prim, one flag per step): a primitive step divides as IEEE does, the
      ;; rest take the Object path, which raises on a zero divisor.
      (:div-prim node)
      (order-args (fn [as]
        (reduce (fn [acc [prim? a]]
                  (str "(" (if prim? "jolt-div2-prim" "jolt-div2") " " acc " " a ")"))
                (first as) (map vector (:div-prim node) (rest as)))))
      :else (emit-native-invoke c))))

(defn- emit-invoke [node]
  (let [tail? *tail?*]           ; capture: children below emit non-tail
   (binding [*tail?* false]
    (let [fnode (:fn node)
          arg-nodes (:args node)
          args (mapv emit arg-nodes)
          tl (or (node-line node) 0)]
      ;; R2: record this site's static callee for the callsite table. Runs after
      ;; the args are emitted, so when a line carries both a call and its
      ;; operand's call the OUTER (later-emitted) callee wins — the tail call's.
      ;; A dynamic TAIL callee registers as "?": the reporter needs to know the
      ;; site's call went somewhere even when it cannot say where (rt.ss
      ;; jolt-dynamic-tail-lines). A dynamic non-tail site still registers nothing.
      (when tl (register-callsite! tl (or (static-callee fnode) (when tail? "?")) tail?))
      (emit-annotated-invoke
       {:node node :fnode fnode :tail? tail? :arg-nodes arg-nodes :args args :tl tl
        ;; the same stamp with-site folds into the marker; a TAIL site needs it in
        ;; the site pair instead, because the call erases the frame the marker
        ;; would have been read against (source-registry jolt-site-frame*)
        :ich (node-inline-chain node)
        :nop (native-op fnode (count args))
        :kind (ifn-kind fnode (count args))
        :order-args (fn [build] (ordered-call arg-nodes args build))})))))

;; A try's body runs in a closure of guard's or dynamic-wind's. Under debug
;; locals it is a tail region of its own, so its leaves keep the scope live there.
(defn- emit-try-body [node]
  (binding [*tail?* (or (some? *dbg-scope*) *tail?*)] (emit (:body node))))

;; try/catch/finally. throw raises a Chez condition wrapping the jolt value
;; (jolt-throw = Scheme `raise` of a &jolt-throw condition); catch lowers to
;; `guard`, whose raw binding is unwrapped via jolt-unwrap-throw so the catch var
;; receives the jolt value (preserving ex-data/ex-message and the backtrace
;; identity tag). finally lowers to `dynamic-wind`'s after-thunk, and runs on
;; success, on catch and on a real escape, but NOT while a fiber park is
;; unwinding. A park is not an exit — the computation resumes — so running the
;; finally there would close a file still in use. Every other escape, an
;; interrupt abort included, is an exit and still runs it.
;;
;; The park case is handled by the BEFORE-thunk, which is the shared marker
;; procedure jolt-finally-in (values.ss) rather than a fresh (lambda () #f) per
;; site. A park drops exactly the winders carrying that marker before it escapes
;; (fibers.ss jolt-park-winders), so the after-thunk below never runs on a park
;; and needs no guard of its own. It used to ask jolt-park-unwinding?, which
;; cost two procedure calls on every finally exit — and `binding` expands to a
;; try/finally, so every binding form paid it too.
;;
;; This is why the marker must be emitted by NAME and not inlined: the filter
;; recognises it with eq?.
;;
;; THIS IS ALSO THE ONLY dynamic-wind THE BACK END EMITS FOR A jolt FORM, and
;; that fact is load-bearing elsewhere. The cheap park in host/chez/java/sm.ss
;; escapes the whole winder chain above the carrier's base and resumes through
;; the fiber thunk, so it does not rewind: any wind between the CPS driver and a
;; rewritten park site would have its after-thunk fire mid-computation and its
;; before-thunk never run again. clojure.core.async's pass keeps that from
;; happening by treating `try` and `fn*` as opaque — `try` because of this clause,
;; and `fn*` NOT because it winds. A bare fn* emits a plain lambda, which is what
;; lets the pass wrap its own continuations around park sites; it is opaque because
;; the pass cannot see what the thunk is handed to, and a host form that takes one
;; winds around the call. Emitting a wind for anything else means adding that head
;; to sm-opaque in the same change, and run-gosm.ss section 1c fails if it does
;; not: it scans the emission for a rewritten park site inside a wind's extent. See
;; the invariant note at jolt-sm-park!.
;; Both keys optional.
;;
;; With tracing on, a catch also puts the site slots back as they were when the
;; try began. A non-tail call restores them when it returns, but a throw does not
;; return: what the throw and the calls it left behind stored would stay there, and
;; the next host fault would report the thrower as its innermost frame.
(defn- emit-try [node]
  (let [ts (when (and (:catch-sym node) (trace-frames?)) (fresh-label "_ts$"))
        to (when ts (fresh-label "_to$"))
        core (if-let [cs (:catch-sym node)]
               (let [raw (munge-name (:catch-raw-sym node))
                     ;; Snapshot the throwing line BEFORE the handler body runs:
                     ;; the body's own calls move the current line, so by the time
                     ;; it reaches .printStackTrace the line would be the handler's
                     ;; rather than the fault's. Saved and restored, so a nested
                     ;; catch cannot leave its throw's line behind for an outer
                     ;; handler that runs later. Only under tracing — with it off
                     ;; there are no line stores to snapshot.
                     cl (when (trace-frames?) (fresh-label "_cl$"))
                     body (str "(guard (" raw " (else (let ((" (munge-name cs) " (jolt-unwrap-throw " raw "))) "
                               (if cl (str "(let ((" cl " (jolt-catch-enter!))) ") "")
                               (if ts (str "(set-virtual-register! 2 " ts ") (set-virtual-register! 12 " to ") ") "")
                               "(let ((r " (binding [*array-vecs* (dissoc *array-vecs* (munge-name cs) raw)]
                                            ;; debug locals: the handler body is a tail region.
                                            ;; cs is the analyzer's; the user's name is a let
                                            ;; inside, which records itself.
                                            (binding [*tail?* (or (some? *dbg-scope*) *tail?*)]
                                              (emit (:catch-body node)))) ")) "
                               (if cl (str "(jolt-catch-leave! " cl ") ") "")
                               "(jolt-catch-complete!) r)"
                               (if cl ")" "")
                               "))) "
                               (emit-try-body node) ")")]
                 (if ts
                   (str "(let ((" ts " (virtual-register 2)) (" to " (virtual-register 12))) " body ")")
                   body))
               (emit-try-body node))]
    (if-let [fin (:finally node)]
      ;; The edges are masked against an interrupt's escape. run-interruptible
      ;; leaves its body by jumping out of a timer interrupt, and Chez checks for
      ;; one at the call into dynamic-wind (after binding's push-thread-bindings,
      ;; before the winder is up) and between the body's return and the
      ;; after-thunk (the winder already popped), so an escape there skipped the
      ;; finally. The counted-lock depth (virtual register 7, host/chez/locks.ss)
      ;; holds the escape off: raised before the wind, dropped as the body starts,
      ;; raised again as it returns and dropped as the finally starts, which then
      ;; runs unmasked and may park. The finally tells a normal exit from a raise
      ;; or an escape by the depth: only the normal exit left it above the level
      ;; at entry. Inline register ops and not the jolt-locks syntax, because the
      ;; emission is also eval'd where only procedures are bound (Gambit's eval
      ;; sees no shim macro, which is also why this says fx> and not fx>?).
      (let [lv (fresh-label "_wl$")
            r (fresh-label "_wr$")]
        (str "(let ((" lv " (virtual-register 7))) (set-virtual-register! 7 (fx+ " lv " 1))"
             " (dynamic-wind jolt-finally-in"
             " (lambda () (set-virtual-register! 7 (fx- (virtual-register 7) 1))"
             " (let ((" r " " core ")) (set-virtual-register! 7 (fx+ (virtual-register 7) 1)) " r "))"
             " (lambda () (if (fx> (virtual-register 7) " lv ") (set-virtual-register! 7 (fx- (virtual-register 7) 1)))"
             " " (binding [*tail?* false] (emit fin)) ")))"))
      core)))

;; Does this IR node emit to an expression that yields a Scheme boolean? Used to
;; drop the redundant jolt-truthy? on an :if test. Sees through the let*/if an
;; (or ...)/(and ...) of bool-returning ops desugars to: `or` is
;; (let* [g E1] (if (truthy? g) g E2)), `and` is (let* [g E1] (if (truthy? g) E2 g))
;; — both return a Scheme boolean when E1/E2 are bool ops, since the value yielded
;; is always one of the (boolean) operand results. `bools` tracks let-bound locals
;; proven to hold a Scheme boolean.
(defn- returns-scheme-bool?
  ([node] (returns-scheme-bool? node #{}))
  ([node bools]
   (cond
     (and (= :const (:op node)) (boolean? (:val node))) true
     ;; the declared-^Record guard emits an (and ...) of jrec-desc-is? tests
     (and (= :invoke (:op node)) (some? (:rec-guard node))) true
     (= :invoke (:op node))
     (let [nop (native-op (:fn node) (count (:args node)))]
       (boolean (and nop (bool-returning-ops nop))))
     (= :local (:op node)) (contains? bools (:name node))
     (= :if (:op node))
     (and (returns-scheme-bool? (:then node) bools)
          (returns-scheme-bool? (:else node) bools))
     (= :let (:op node))
     (let [bools' (reduce (fn [s b]
                            (if (returns-scheme-bool? (nth b 1) s)
                              (conj s (nth b 0))
                              (disj s (nth b 0))))
                          bools (:bindings node))]
       (returns-scheme-bool? (:body node) bools'))
     :else false)))

;; A fn def registers its source so a backtrace maps the frame-name to
;; "ns/name (file:line)" instead of a bare name — for the tail-frame history in
;; trace mode, and for the live continuation on the ordinary eval path. Keyed by
;; the SAME munged name the entry push records (emit-fn's letrec self-binding =
;; the fn's own name). Returns "" when off / not a positioned fn def, so the seed
;; mint's output stays byte-identical. Direct-link builds already register via
;; emit-def-cached; this covers the open-world eval path.
(defn- trace-source-reg [node]
  (let [init (:init node) pos (:pos node)]
    (if (and (or (trace-frames?) (source-reg?)) (= :fn (:op init)) (:name init) pos)
      (let [nm (munge-name (:name init))
            key (if *qualifying-ns*
                  (str (munge-name *qualifying-ns*) "/" nm)
                  nm)]
        (str " (jolt-register-source! " (chez-str-lit key) " "
             (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
             (if (:file pos) (chez-str-lit (:file pos)) "jolt-nil") " "
             (or (:line pos) 0) ")"))
      "")))

;; A (register-inline-method type-name proto method fn) / register-method call whose
;; fn arg is a single-arity impl literal. Returns [type-name proto method fn-node] or
;; nil — the seed of a contagion-specialized clone.
(defn- register-impl-invoke [node]
  (types/register-impl-invoke? node))

;; Emit a contagion-specialized clone of an impl alongside its registration, when the
;; impl body has a :num field beside a proven :double operand. Re-specializes on the
;; emit (run-passes'd) IR — eligibility is deterministic, so it agrees with whatever
;; populated clone-sites. Returns "(define <sym> <fn>) (register-clone* ..)" or nil.
;; register-clone* tags via the runtime current ns, exactly like register-inline-method,
;; so the clone and impl land under the same tag the devirt site's :devirt-type names.
(defn- emit-impl-clone [node]
  (when-some [u @jolt.op-registry/current-unit-box]
   (when-some [[type-name proto method fc] (register-impl-invoke node)]
    (let [ars (:arities fc)]
      (when (= 1 (count ars))
        (let [res (types/contagion-specialize-arity u (first ars) type-name)]
          (when (nth res 1)
            (let [spar (nth res 0)
                  sym (fresh-label "_jcf$")
                  clone (emit (numeric/annotate {:op :fn :arities [spar]}))]
              (str "(define " sym " " clone ") (register-clone* "
                   (chez-str-lit type-name) " " (chez-str-lit proto) " "
                   (chez-str-lit method) " " sym ")")))))))))

;; Wrap emit-invoke so an eligible impl registration also emits its contagion clone as
;; a sibling. The non-eligible path is byte-identical to before (no clone emitted).
(defn- emit-invoke-maybe-clone [node]
  (let [base (emit-invoke node)]
    (with-site node
      (if-some [c (emit-impl-clone node)]
        (str "(begin " c " " base ")")
        base))))

;; A def: the var, its init, and the linked or cached forms it takes.
(defn- emit-def-node [node]
  (binding [*qualifying-ns* (when (source-reg?) (:ns node))
            *callsites* (when (trace-frames?) (atom {}))
            *fnsrc-ns* (:ns node)
            *fnsrc-def* (:name node)
            *fnsrc-counter* (atom 0)
            *fnsrc-regs* (atom [])
            *fnsrc-def-init?* (= :fn (:op (:init node)))]
    (let [reg (trace-source-reg node)
          d (cond
              (:no-init node)
              (if (jmeta-nonempty? (:meta node))
                ;; set-var-meta! interns the same cell declare-var! returns, so
                ;; declare-var! runs LAST — a no-init def evaluates to its var,
                ;; like the JVM ((var? (def x)) is true), not set-var-meta!'s void.
                (str "(begin (set-var-meta! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                     (emit-def-meta node) ")"
                     " (declare-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) "))")
                (str "(declare-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")"))
              (jmeta-nonempty? (:meta node))
              (str "(def-var-with-meta! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                   (emit-with-cells #(emit (:init node))) " " (emit-def-meta node) ")")
              :else
              (str "(def-var-plain! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                   (emit-with-cells #(emit (:init node))) ")"))
            creg (trace-callsite-reg)
            freg (fnsrc-flush)]
      ;; a def evaluates to its VAR ((var? (def x)) is true), so the source
      ;; and callsite registrations must not be the value of the form —
      ;; bind the def's result, register, and hand the var back. The fnsrc
      ;; registrations run BEFORE the form: they are static data, and the
      ;; form's own evaluation may already dump a closure it just created.
      (let [pre (if (= freg "") "" (str "(begin" freg " "))
            post (if (= freg "") "" ")")
            body (if (= (str reg creg) "") d
                     (let [v (fresh-label "_dv$")]
                       (str "(let ((" v " " d "))" reg creg " " v ")")))]
        (str pre body post)))))

;; throw, with the site the reporter needs.
(defn- emit-throw-node [node]
  (let [line (or (node-line node) (node-line (:expr node)))
        ch (let [c (or (get node :inline-chain) (get (:expr node) :inline-chain))]
             (when (and (seq c) (every? (fn [x] (marker-safe-fqn? (nth x 0))) c)) c))
        e (binding [*tail?* false] (emit (:expr node)))
        ;; A throw never returns, so its site goes in the slot in any
        ;; position, not only a tail one. What the slot holds otherwise is
        ;; the tail call that reached this fn, and the reporter would take
        ;; that caller for the innermost frame. Outside a named fn there
        ;; is no pair to store, so the slot is emptied instead.
        call (cond
               (not (trace-frames?)) (emit-call *tail?* "jolt-throw" [e] (or line 0) ch)
               *trace-site* (sited-tail-call *trace-site* (or line 0) "jolt-throw" [e] ch)
               :else (let [t (fresh-label "_tt$")]
                       (str "(let* ((" t " " e ")) (set-virtual-register! 2 0) "
                            (plain-call "jolt-throw" [t]) ")")))]
    ;; A macro-built throw has no :pos — `assert` expands to one, and the
    ;; app.util fixture reaches it through a user macro besides — so a
    ;; site with a chain but no line still emits a marker, with line 0.
    ;; 0 is not a line: srcreg-entry-frames drops it and the renderer
    ;; falls back to the callee's DEFINING line. That is close to, but
    ;; not always identical to, what the un-inlined build reports — there
    ;; the frame is located by the nearest marker inside the callee's own
    ;; compiled body, which for the app.util fixture is the macro's line
    ;; (64) rather than the defn's (66). The frame is named correctly
    ;; either way; only a macro-generated site whose neighbours were
    ;; const-folded away can differ, and it differs by pointing at the fn
    ;; instead of into the macro that wrote it.
    ;;
    ;; Without a chain the lineless case still emits nothing, because
    ;; there the nearest enclosing marker is a better answer than 0.
    (if (and (trace-frames?) (or line ch))
      (str "#|L" (or line 0)
           (if ch (apply str (map (fn [x] (str "@" (nth x 0) "@" (nth x 1))) ch)) "")
           "|# " call)
      call)))

;; A host method call (.m obj args).
(defn- emit-host-call-node [node]
  (let [tail? *tail?*
        sited? (and (trace-frames?) tail? *trace-site*)]
   (binding [*tail?* false]
    (when sited? (note-tail-call! tail?))
    (if sited?
      ;; A bare local or a constant runs no tail site, so it is
      ;; spliced as it is; only an operand that can call is bound
      ;; to a temp. (Keeps a proven-keyword (.sym k) at the exact
      ;; inline shape the build smoke pins.)
      (let [trivial? (fn [n] (contains? #{:local :const} (:op n)))
            bind (fn [n] (let [e (emit n)]
                           (if (trivial? n) [nil e] [(fresh-label "_hs$") e])))
            [tt t] (bind (:target node))
            bs (mapv bind (:args node))
            as (mapv (fn [[l e]] (or l e)) bs)
            binds (str/join " " (keep (fn [[l e]] (when l (str "(" l " " e ")")))
                                      (cons [tt t] bs)))
            site (site-literal *trace-site* (or (node-line node) 0)
                               (node-inline-chain node))
            call (host-call-emit emit (assoc node :sited-target (or tt t) :sited-args as))]
        (if (seq binds)
          (str "(let* (" binds ") (jolt-site! " site ") " call ")")
          (str "(begin (jolt-site! " site ") " call ")")))
      ;; A method a library type implements runs jolt code, so a
      ;; non-tail call restores the slot like any other
      ;; (returning-call?) -- unless the receiver is proven a
      ;; string, keyword or string builder, a host method.
      (let [call (host-call-emit emit node)]
        (note-tail-call! tail?)
        (if (and (trace-frames?) (not tail?)
                 (not (contains? #{:str :kw :sb :sbuf} (:target-type node))))
          (site-restore call)
          call))))))

;; A coercion the inference placed at a typed boundary.
(defn- emit-coerce-node [node]
  (let [e (emit (:expr node))]
    (cond
      (and (:fl-arg node) (contains? cast-window-suffix (:cast-fn node)))
      (let [t (fresh-label "_fa$")
            s (get cast-window-suffix (:cast-fn node))
            lane (str "(flonum->fixnum " t ")")]
        (str "(let ((" t " " e "))"
             " (if (and (" (unsafe-prefix) "fl>=? " t " jolt-cast-lo-" s ")"
             " (" (unsafe-prefix) "fl<=? " t " jolt-cast-hi-" s "))"
             " " (if (= "jolt-char" (:cast-fn node)) (str "(integer->char " lane ")") lane)
             " (" (cast-entry node) " " t ")))"))
      ;; (long x) and (unchecked-long x) both hand a fixnum back
      ;; unchanged, and a fixnum is what a loop counter or a char code
      ;; point is: test it here so the common case is a type check,
      ;; not a call. The helper still owns every other operand.
      (contains? #{"jolt-long-cast" "jolt-unchecked-long"} (:cast-fn node))
      (let [t (fresh-label "_lc$")]
        (str "(let ((" t " " e ")) (if (fixnum? " t ") " t " (" (cast-entry node) " " t ")))"))
      ;; a float cast whose value an fl op takes (:fl-unbox, numeric
      ;; pass) hands it the double it widens to, never building the jfloat
      (and (:fl-unbox node) (contains? #{"jolt-float" "jolt-unchecked-float"} (:cast-fn node)))
      (str "(" (:cast-fn node) "->flonum " e ")")
      (:cast-fn node) (str "(" (cast-entry node) " " e ")")
          (= :double (:kind node)) (emit-nhint-coerce :double e)
          (= :long (:kind node)) (emit-nhint-coerce :long e (boolean (:return? node)))
          :else e)))

;; A var in value position.
(defn- emit-var-node [node]
  (let [core-proc (and (= "clojure.core" (:ns node)) (core-value-procs (:name node)))]
    (cond
      core-proc core-proc
      ;; direct-linked app var used as a value -> reference its binding.
      ;; The def emitted it linked (def-var-linked!), so the binding and the
      ;; var cell's root are ONE value under alter-var-root / with-redefs /
      ;; a later def; it also helps DCE keep the binding live.
      (direct-linkable? (:ns node) (:name node)) (dl-name (:ns node) (:name node))
      (and (stdlib-var? node) (not (prelude-mode?)))
      (throw (ex-info (str "emit: unsupported stdlib ref `" (:ns node) "/" (:name node)
                           "` (no core on Chez yet)") {}))
      ;; inside a def, cache the interned var cell in a per-site cell so the
      ;; name lookup (string-append + hash) runs once, not per access; the
      ;; cell is stable and def-var! mutates its root in place, so this stays
      ;; correct under redefinition. Read through var-cell-deref — the
      ;; cell-based var-deref: binding-aware (a thread-bound dynamic var
      ;; resolves to its binding) AND lenient on an unbound root (the strict
      ;; jolt-var-get throws on a forward-declared var). Outside a def,
      ;; resolve per access.
      :else
      (if (and (var-cache?) *const-pool*)
        (hoist-var-cell (:ns node) (:name node))
        (str "(var-deref " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")")))))

;; The var forms: the var object, a thread-binding or field write, and a
;; non-top-level defmacro.
(defn- emit-var-form [node]
  (case (:op node)
    :the-var (str "(jolt-var " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")")
    ;; (set! *var* val) -> set the var's innermost thread binding; throws if none.
    :set-var (str "(jolt-set-var! " (emit (:the-var node)) " " (emit (:val node)) ")")
    ;; (set! (.-field obj) val) -> mutate the deftype instance field in place.
    :set-field (str "(jolt-set-field! " (emit (:obj node)) " "
                    (hoist-const (str "(keyword #f " (chez-str-lit (:field node)) ")"))
                    " " (emit (:val node)) ")")
    ;; a non-top-level defmacro -> def the expander fn + mark the var a macro at
    ;; runtime (the spine does the same for top-level forms).
    :defmacro (str "(begin (def-var-with-meta! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                   (emit (:fn node)) " "
                   (emit-def-meta node)
                   ") (mark-macro! " (chez-str-lit (:ns node)) " "
                   (chez-str-lit (:name node)) ") jolt-nil)")))

;; Host references and constructions.
(defn- emit-host-node [node]
  (case (:op node)
    :host (throw (ex-info (str "emit: unsupported host ref `" (:name node) "`") {}))
    ;; a static field read (Long/MIN_VALUE); cached per site like the call above
    :host-static (if (and *const-pool* (var-cache?))
                   (str "(host-static-ref-site " (hoist-const-per-site "(host-static-site-make)") " "
                        (chez-str-lit (:class node)) " " (chez-str-lit (:member node)) ")")
                   (str "(host-static-ref " (chez-str-lit (:class node)) " "
                        (chez-str-lit (:member node)) ")"))
    ;; (R. f …) on a record whose shape the unit knows is its positional ctor,
    ;; the one (->R f …) already inlines — same arity rule. Anything else (a host
    ;; class, the JVM's (R. f … meta ext) arity) is the runtime's class lookup.
    :host-new (let [arg-nodes (:args node)
                    s (record-shape-for-class (:class node))]
                (if (and s (= (count (get s :fields)) (count arg-nodes)) (<= (count arg-nodes) 6))
                  (emit-record-ctor s arg-nodes (mapv emit arg-nodes))
                  (str "(host-new " (chez-str-lit (:class node))
                       (let [args (map emit arg-nodes)]
                         (if (empty? args) "" (str " " (str/join " " args)))) ")")))))

;; if: the test is non-tail; then/else inherit the if's tail position.
;; do: non-last statements are non-tail; the ret inherits the do's tail position.
(defn- emit-if-or-do [node]
  (if (= :if (:op node))
    (let [test (:test node)
          t (binding [*tail?* false]
              (if (returns-scheme-bool? test) (emit test)
                  (str "(jolt-truthy? " (emit test) ")")))]
      (str "(if " t " " (emit (:then node)) " " (emit (:else node)) ")"))
    (str "(begin " (binding [*tail?* false] (str/join " " (mapv emit (:statements node))))
         (if (empty? (:statements node)) "" " ") (emit (:ret node)) ")")))

;; collection literals -> rt constructors (collections.ss). Elements are
;; already-analyzed IR nodes; evaluate LEFT-TO-RIGHT (emit-ordered, which
;; wraps only when two or more operands could have observable effects).
;;
;; An ALL-CONSTANT literal is hoisted to a per-def constant instead of being
;; rebuilt at every evaluation — what the reference compiler does by emitting
;; literal collections into the class's constant pool. Rebuilding cost real
;; time: a 12-key literal map measured 0.81 us against the JVM's 0.012, and a
;; 2-element literal set 0.12 against 0.012, because each evaluation re-ran
;; jolt-hash-map / jolt-hash-set over the same elements. A literal with a
;; non-constant element (a local, a call) still constructs per evaluation,
;; since its value is not fixed at emit time.
;;
;; A map literal whose keys are all constant keywords hands its slots to the
;; array map as one vector: the reader has already refused a repeated
;; literal key, so there is nothing for jolt-hash-map's duplicate scan over
;; a rest list to find (the reference compiler's RT.mapUniqueKeys for a
;; MapExpr with constant, distinct keys). Bounded at the keyword array
;; limit — past it the literal is hash mode, which the checked constructor
;; still decides. The slot vector is built through ordered-call like any
;; operand list, so values with effects still evaluate left to right.
(defn- emit-map-literal [node]
  (let [kvs (mapcat (fn [p] [(nth p 0) (nth p 1)]) (:pairs node))]
    (if (unique-keyword-key-literal? node)
      (ordered-call kvs (mapv emit kvs)
        (fn [strs] (str "(amap-slots->pmap (vector " (str/join " " strs) "))")))
      (emit-ordered (if (distinct-const-keys? (map first (:pairs node)))
                      "jolt-hash-map"
                      "jolt-hash-map-checked")
                    kvs))))

(defn- emit-coll-literal [node]
  (let [s (case (:op node)
            :vector (emit-ordered "jolt-vector" (:items node))
            :set (emit-ordered (if (distinct-const-keys? (:items node))
                                 "jolt-hash-set"
                                 "jolt-hash-set-checked")
                               (:items node))
            :map (emit-map-literal node))]
    (if (const-coll-node? node) (hoist-const-per-site (site-const node s)) s)))

;; Literal data the reader built: quoted forms, and the regex, #inst, #uuid,
;; bigdec and namespace leaves.
(defn- emit-data-node [node]
  (case (:op node)
    ;; A quoted scalar (form-char?/form-literal?) emits as an immediate constant
    ;; via emit-const — nothing to hoist. Every other quoted form (symbol, list,
    ;; vector, map, set, regex/inst/uuid/tagged) is a CONSTRUCTION rebuilt per
    ;; evaluation, so hoist it: built once per def, one object per source form,
    ;; distinct objects for distinct forms — the reference compiler's
    ;; ConstantExpr behavior for quoted data.
    ;;
    ;; Keyed by the FORM, so a macro that splices one form into several places in
    ;; its expansion gets one constant rather than one per mention. See
    ;; hoist-const-for.
    :quote (let [f (:form node)
                 s (emit-quoted f)]
             (if (or (form-char? f) (form-literal? f))
               s
               (hoist-const-for f (site-const node s))))
    ;; regex literal #"…" -> a jolt-regex value (regex.ss, vendored irregex),
    ;; hoisted per site like a constant collection: the reference reads #"…"
    ;; as a Pattern object and compiles it as a constant, one object per site
    ;; built once, where rebuilding here cost a cache lookup and a fresh
    ;; regex-t every time the literal was reached — measured 320 ns of a 350
    ;; ns `(re-find #"^\\(defn" s)` with the engine already compiled. Per
    ;; site, not per form, for the reason the collection literals are.
    :regex (hoist-const-per-site (site-const node (str "(jolt-regex " (chez-str-lit (:source node)) ")")))
    ;; #inst / #uuid literals -> runtime inst / uuid values.
    :inst (str "(jolt-inst-from-string " (chez-str-lit (:source node)) ")")
    :uuid (str "(jolt-uuid-from-string " (chez-str-lit (:source node)) ")")
    ;; bigdecimal literal (1.5M) -> a runtime jbigdec from its numeric text.
    :bigdec (str "(jolt-bigdec-from-string " (chez-str-lit (:source node)) ")")
    ;; a namespace value spliced into a form (~*ns*) -> reconstruct by name.
    :the-ns (str "(intern-ns! " (chez-str-lit (:name node)) ")")))

(defn emit* [node]
  (case (:op node)
    :const (emit-const (:val node))
    :local (munge-name (:name node))
    ;; late-bound var: read the cell's current root at use time. A value-position
    ;; ref to a clojure.core fn the RT provides lowers to the RT procedure.
    :var (emit-var-node node)
    (:the-var :set-var :set-field :defmacro) (emit-var-form node)
    (:host :host-static :host-new) (emit-host-node node)
    (:if :do) (emit-if-or-do node)
    :invoke (emit-invoke-maybe-clone node)
    (:vector :set :map) (emit-coll-literal node)
    (:quote :regex :inst :uuid :bigdec :the-ns) (emit-data-node node)
    ;; the thrown value is an operand (emitted non-tail); the throw itself goes
    ;; through emit-call with marks?=#f, so a TAIL throw gets the site-vreg pair
    ;; (sited-tail-call — stored after the operand is bound, so the operand's own
    ;; fixnum store cannot stomp it) and still names its TCO-erased frame. A
    ;; non-tail throw needs neither: the continuation names the frame, and the
    ;; operand's own with-site store carries the line. The analyzer stamps :pos
    ;; on call forms, not on throw special forms, so the site line falls back to
    ;; the thrown expression's — (throw (ex-info …)) sits on one line.
    ;; The chain comes from the same two places the line does — a :throw the
    ;; inline pass copied carries it, and so does the expression it throws when
    ;; the throw form itself was not stamped.
    :throw (emit-throw-node node)
     ;; numeric coercion. A :cast-fn (from a user (double x)/(long x)/… cast)
     ;; emits the checked runtime helper — clojure.core's full JVM semantics —
     ;; NOT the hint coercion (jolt->fl / jolt->fx) a proven typed-param cast uses.
     ;; The 2-arg :coerce (inlined ^double/^long param or return) has no :cast-fn
     ;; and keeps the hint coercion. A cast whose operand the numeric pass has
     ;; PROVEN a double (:fl-arg) tests the cast's window inline and converts
     ;; with flonum->fixnum, calling the helper only outside the window: NaN
     ;; fails both compares and reaches it (stays 0), and out-of-range values,
     ;; the long saturation at 2^63 and surrogate or astral chars keep their
     ;; results and messages. The window constants live in the RUNTIME
     ;; (converters.ss jolt-cast-windows), computed at load from the target's
     ;; own fixnum limits: the compiling host's fixnum width is not the
     ;; target's, so no bound is baked in here.
     :coerce (emit-coerce-node node)
    :try (emit-try node)
     ;; A :target-type direct emit skips record-method-dispatch entirely, so the
     ;; classes it fires for are the classes a library CANNOT override at runtime
     ;; (jolt.host/extend-class! rejects them, listed in class-ext-no-override,
     ;; java/class-extensions.ss). Adding a fourth :target-type here means adding
     ;; its class there, or an override on it applies at some call sites and not
     ;; others.
     ;; (.method target arg*) -> jolt-host-call for an rt-shimmed method, else
     ;; record-method-dispatch (a reify/record protocol method). A target PROVEN
     ;; a string (:target-type :str) or a keyword (:kw) on the Chez target emits
     ;; that native directly — no dispatch walk, no rest-args vector. The emitted
     ;; target is bound as `t` rather than `target` so the host predicate
     ;; `(target)` stays reachable in this scope.
     ;; In tail position with tracing on, the receiver and args are bound first
     ;; and the site pair stored before the call (the same shape as
     ;; sited-tail-call, for the same reason: a callee's own tail sites must not
     ;; stomp the slot). The host raising from inside that call — string-append
     ;; on nil — is then reported at this fn and line, TCO having erased the
     ;; frame. Untraced and non-tail emission is byte-identical to before.
     :host-call (emit-host-call-node node)
    :let (emit-let node)
    :loop (emit-loop node)
    :recur (emit-recur node)
    (:ffi-layout :ffi-fn :ffi-callable) (emit-ffi-node emit node)
    :fn (emit-fn emit node)
    ;; (def name) with no init (declare): reserve the cell. A def with non-empty
    ;; reader metadata lowers to def-var-with-meta! (ported in a later increment).
    ;; Qualify only when this def will actually register a source map — that is the
    ;; whole point of the unique name, and it ties qualification to the runtime-eval
    ;; path. The seed mint and `jolt build` emit with source-reg off, so core keeps
    ;; its short names and prelude.ss stays byte-identical across a re-mint.
    ;; *callsites* is bound HERE, not in register-callsite! — a dynamic var
    ;; must hold the atom before the init emission runs or every site record
    ;; silently no-ops and the callsite table stays empty.
    :def (emit-def-node node)
    (throw (ex-info (str "emit: op not yet ported / unhandled: " (pr-str (:op node))) {}))))

;; ^:dynamic / ^:redef on a def opts it out of direct-linking: it stays redefinable,
;; so callers must go through the var cell. m is a def's :meta (a jolt map value).
(defn- dl-opt-out? [m] (jolt.ir/closed-world-opt-out? m))

;; A fn def gets a source-registry entry so a native backtrace can map its
;; frame to ns/name (file:line). Chez names the frame by whatever emit-fn
;; binds the lambda to: a NAMED fn (defn, or (fn foo …)) gets a letrec
;; self-binding = munge-name of the fn's own name; an ANONYMOUS fn def has
;; no letrec, so the lambda sits directly under (define jv$ns/name …) and
;; takes that name (B). Register under whichever Chez will report.
;; Not in the seed mint: a registration carries the def's file, and the
;; seed must not bake this machine's paths (a core frame prints by name).
(defn- linked-def-source-reg [node b]
  (let [pos (:pos node)
        frame-name (if-let [fnm (:name (:init node))] (munge-name fnm) b)]
    (when (and pos (not (seed-mint?)))
      (str " (jolt-register-source! " (chez-str-lit frame-name) " "
           (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
           (if (get pos :file) (chez-str-lit (get pos :file)) "jolt-nil") " "
           (or (get pos :line) 0) ")"))))

;; The def's init emitted under its cache-cell scope, with the callsite table and
;; fnsrc context bound around it: [init callsite-registrations fnsrc-registrations].
;; *callsites* is bound around the init emission and the registration
;; sibling read while still bound — this is the BUILD path's def emit,
;; and without it a built binary had empty callsite tables: the
;; reporter's backwalk dead-ended after the throw-time pair and a
;; built trace showed the erased fn but never its erased callers.
;; The fnsrc context rides the same binding: the init's anon literals
;; collect into *fnsrc-regs* and flush before the binding returns.
(defn- emit-def-init [node dl-self anon-variadic fn?]
  (binding [*dl-self* dl-self
            *variadic-reg-suppressed?* (boolean anon-variadic)
            *callsites* (when (trace-frames?) (atom {}))
            *fnsrc-ns* (:ns node)
            *fnsrc-def* (:name node)
            *fnsrc-counter* (atom 0)
            *fnsrc-regs* (atom [])
            *fnsrc-def-init?* fn?]
    (let [i (emit-with-cells #(emit (:init node)))]
      [i (trace-callsite-reg) (fnsrc-flush)])))

;; An unlinked def: def-var-with-meta! or def-var-plain!. A def evaluates to its
;; var, so with registrations to make it registers first, binds, and hands the
;; var back. The binding's label is taken before the metadata emits.
(defn- emit-unlinked-def [node init creg freg]
  (let [ns (:ns node) nm (:name node)
        def-form (fn []
                   (if (jmeta-nonempty? (:meta node))
                     (str "(def-var-with-meta! " (chez-str-lit ns) " " (chez-str-lit nm) " " init " " (emit-def-meta node) ")")
                     (str "(def-var-plain! " (chez-str-lit ns) " " (chez-str-lit nm) " " init ")")))]
    (if (= (str creg freg) "")
      (def-form)
      (let [v (fresh-label "_dv$")]
        (str "(begin" freg " (let ((" v " " (def-form) "))" creg " " v "))")))))

;; Per-form entry used by the image/build emitter. In direct-link mode a TOP-LEVEL
;; def (form root, or spliced from a top-level do) without an opt-out also binds
;; jv$<fqn> and aliases the var cell to it, so app->app calls/refs bind directly.
;; Off direct-link mode this is exactly `emit`, so the seed mint and runtime eval are
;; byte-unchanged. Nested defs (a defonce's inner def) never reach a top-level branch
;; here, so they stay indirect — a `define` would be illegal in their position.
;; Emit a def, wrapping its init in a let that binds each per-site cache cell
;; (var-ref + devirt) so a hot loop's lookups resolve once into the def's closure.
;; Runs in BOTH modes; in direct-link mode a non-opt-out def also binds jv$<fqn>
;; and registers it for app->app direct linking + a source-map frame.
(defn- emit-def-cached [node]
  (let [ns (:ns node) nm (:name node)
        dl? (and (direct-link?) (not (dl-opt-out? (:meta node))))
        b (dl-name ns nm)
        fn? (= :fn (:op (:init node)))
        reg (when (and dl? fn?) (linked-def-source-reg node b))
        ;; register before emitting the init so a self-referential body direct-links.
        _ (when dl? (swap! (:direct-link-defined (cur)) conj (dl-fqn ns nm))
                    (when fn? (swap! (:direct-link-fns (cur)) conj (dl-fqn ns nm))))
        ;; An anonymous variadic fn under a direct-link define is the one shape whose
        ;; frame name comes from the define itself, so it must stay a bare lambda in
        ;; that position: emit-fn skips its registration and we emit it here against
        ;; b. Everywhere else emit-fn registers through its own binding.
        anon-variadic (when (and dl? fn? (not (:name (:init node))))
                        (some (fn [a] (when (:rest a) (count (:params a))))
                              (:arities (:init node))))
        vreg (when anon-variadic
               (str " (jolt-register-variadic! " anon-variadic " " b ")"))
        init+creg (emit-def-init node (when (and dl? fn?) [(dl-fqn ns nm) (:init node)])
                                 anon-variadic fn?)
        init (nth init+creg 0)
        creg (nth init+creg 1)
        freg (nth init+creg 2)]
    ;; fnsrc registrations run BEFORE the define/def-var!: static data, and the
    ;; init (or a form evaluated right after in the same top-level do) may dump a
    ;; closure the init just created.
    (if dl?
      ;; a direct-linked def — the seed mint's core, and every app def a
      ;; `jolt build` emits — is a LINKED def. def-var-linked! binds the var the
      ;; way def-var-with-meta!/def-var-plain! do and records the jv$ symbol with
      ;; a setter over it, so a later def / alter-var-root / with-redefs of the
      ;; var writes the new root through to the binding (rt.ss var-root-set!) —
      ;; the one every direct call site applies and every value-position ref
      ;; reads. Without it the binding and the var cell split on the first root
      ;; write: `(var-get #'x)` saw the new value while a compiled `x` kept
      ;; reading the old one, so `alter-var-root` of a plain app var was
      ;; invisible in a built binary and visible everywhere else (jolt#1009).
      ;; In TIME it is one hashtable probe per ROOT WRITE — never per call or
      ;; per read — and no call site slows down: the binding is already
      ;; assignable in a build (build.ss bld-defer-app-strs rewrites each
      ;; `(define jv$… init)` into a `(set!)` run from the launcher), so linking
      ;; costs it no further optimization. In SPACE it is one setter closure and
      ;; one eq-hashtable entry per app def, which is NOT free: measured over a
      ;; generated app, ~42-45 bytes of binary and ~0.6 KB of runtime RSS per
      ;; def (601 defs: +0.3% binary, +0.3% RSS; 2401 defs: +0.9% / +1.4%). A
      ;; pathological shape — 20k defs whose inits are all tiny constants, so
      ;; the setter dominates what it is attached to — costs more: +207 B/def
      ;; and +1.4 KB/def, +17% binary and +12% RSS. Worth knowing before
      ;; anything raises the per-def payload again. Inlining is the
      ;; separate closed-world freeze: a body spliced into a call site by the
      ;; inline pass still predates the write, and ^:dynamic/^:redef opt out of
      ;; direct-linking altogether.
      (str "(begin" freg " (define " b " " init ") (def-var-linked! "
           (chez-str-lit ns) " " (chez-str-lit nm) " '" b " " b
           " (lambda (v) (set! " b " v)) "
           (if (jmeta-nonempty? (:meta node)) (emit-def-meta node) "#f") ")"
           (or reg "") (or vreg "") creg ")")
      (emit-unlinked-def node init creg freg))))

;; FNSRC-DEF names the enclosing top-level def for a node that does not carry the
;; name itself. A macro's expander reaches the image emitter as a BARE fn form —
;; ce-defmacro->fn has already split the name off — so the caller supplies it
;; here; otherwise every macro in a namespace registers its anon fns under
;; jfn$<ns>/$<n> with the counter restarting per form, and siblings collide.
(defn emit-top-form
  ([node] (emit-top-form node nil))
  ([node fnsrc-def]
  ;; A statement of a top-level do (the direct-link arm below re-enters here
  ;; per statement) carries no :ns and no :fnsrc-ns of its own -- the analyzer
  ;; stamps only the top-level node -- so it inherits the enclosing binding.
  ;; Rebound to nil, every fn literal in a non-def statement (a deftype method
  ;; body, a defmethod's fn) was emitted unnamed and unregistered, and a reify
  ;; instance holding one refused to dump.
  (binding [*fnsrc-ns* (or (:ns node) (:fnsrc-ns node) *fnsrc-ns*)
            ;; :defmacro too, not just :def. Without it every defmacro in a
            ;; namespace emits its expander under jfn$<ns>/$<n> with the counter
            ;; restarting per top-level form, so sibling macros all claim
            ;; jfn$<ns>/$0 — last registration wins, and an image dump of a
            ;; closure over an earlier macro's expander restores a different
            ;; macro's source. A defmacro node carries :name exactly as :def
            ;; does, so naming it is all that is needed.
            *fnsrc-def* (or (when (#{:def :defmacro} (:op node)) (:name node))
                            fnsrc-def)
            *fnsrc-counter* (atom 0)
            *fnsrc-regs* (atom [])]
    (let [scm (cond
                ;; off direct-link (the seed mint + runtime-via-image) this is exactly
                ;; `emit`, whose :def case already wraps cache cells, so the seed stays
                ;; byte-unchanged. The :def cases bind their own fnsrc context over
                ;; this one (fresh counter/regs per def) and flush it themselves.
                ;; emit-top-cells declines a :def itself, so this reaches it only
                ;; for expressions.
                (not (direct-link?)) (emit-top-cells node #(emit node))
                ;; top-level do splices: each statement/ret is itself a top-level form.
                (= :do (:op node))
                (str "(begin " (str/join " " (map emit-top-form (:statements node)))
                     (if (empty? (:statements node)) "" " ") (emit-top-form (:ret node)) ")")
                (and (= :def (:op node)) (not (:no-init node)) (not (dl-opt-out? (:meta node))))
                (emit-def-cached node)
                :else (emit-top-cells node #(emit node)))
          freg (fnsrc-flush)]
      (cond
        (= freg "") scm
          ;; registrations run BEFORE the form: they are static data with no
          ;; dependency on the form's evaluation, and the form itself may dump a
          ;; closure it just created — the registration must already be there.
          ;; begin keeps the form's value as the result.
        :else (str "(begin" freg " " scm ")"))))))
