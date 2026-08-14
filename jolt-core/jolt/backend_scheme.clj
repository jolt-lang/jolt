(ns jolt.backend-scheme
  "Lowers the host-neutral IR (jolt.ir) to Chez Scheme source text.

  The analyzer produces IR; this emitter turns each IR op into a string of Scheme
  source, which the host compiles with (eval (read ...)). It depends only on
  clojure.core and clojure.string, so once cross-compiled it runs on Chez and can
  emit its own code — the bootstrap spine. Quoted forms are walked through the
  portable jolt.host form-* contract, the same seam the analyzer uses, so the
  emitter never touches a concrete host representation directly."
  (:require [clojure.string :as str]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-sym-meta
                               form-list? form-vec? form-map? form-set? form-char?
                               form-literal? form-elements form-vec-items
                               form-map-pairs form-set-items form-char-code
                               form-regex? form-regex-source
                               form-inst? form-inst-source form-uuid? form-uuid-source
                               form-class-value? form-class-value-name]]
            [jolt.passes.types :as types]
            [jolt.passes.numeric :as numeric]
            [jolt.op-registry :as op-registry]
            [jolt.ir :as ir]
            [clojure.set :as set]))

;; op-registry and its derived accessor tables live in jolt.op-registry — the
;; single source of truth for per-op facts, shared with the passes. Bind them to
;; local names here so the emit clauses read the same as before. (Qualified refs,
;; not :refer, so the seed mint can compile this ns before op-registry is loaded
;; into the running image — op-registry loads first at runtime.)
(def ^:private native-ops op-registry/native-ops)
(def ^:private core-value-procs op-registry/core-value-procs)
(def ^:private bool-returning-ops op-registry/bool-returning-ops)
(def ^:private op-arity op-registry/op-arity)
(def ^:private fixed-arity-ops op-registry/fixed-arity-ops)
(def ^:private dbl-ops op-registry/dbl-ops)
(def ^:private lng-ops op-registry/lng-ops)
(def ^:private bd-ops op-registry/bd-ops)
(def ^:private cmp1-ops op-registry/cmp1-ops)
(def ^:private registry-emitted-names op-registry/registry-emitted-names)
;; Host interop methods with a Chez RT shim (rt.ss jolt-host-call). A `.method`
;; call on any other method routes to record-method-dispatch (a reify/record
;; protocol method).
(def ^:private supported-host-methods #{"isDirectory" "listFiles"})

;; The current compilation-unit context (jolt.passes.types unit). ALL emit-session
;; state — the mode flags, the direct-link name registries, the record-ctor shape
;; registry, the gensym counter and the per-site cache cells — lives on it, read
;; here because the emitter threads no unit. The pointer lives in jolt.op-registry
;; (the leaf) so the passes reach the same unit — jolt.passes.inline can't require
;; the back end (a cycle). A driver publishes the unit (set-emit-unit!); `cur` lazily
;; installs a fresh one so a bare emit never reads nil. `current-unit` is the public
;; entry run-passes uses to share this one unit as its inference context too.
(defn set-emit-unit! [u] (reset! jolt.op-registry/current-unit-box u))
(defn- cur [] (or @jolt.op-registry/current-unit-box
                  (let [u (types/new-unit)] (reset! jolt.op-registry/current-unit-box u) u)))
(defn current-unit [] (cur))

;; PRELUDE MODE. The default (subset) mode rejects any clojure.core ref
;; that isn't a native-op — a clean "out of subset" signal for user-facing `-e`.
;; When emitting clojure.core ITSELF as a prelude, core fns reference each other
;; constantly; those lower to var-deref (resolved at runtime).
(defn set-prelude-mode! [on] (reset! (:prelude-mode? (cur)) (boolean on)))
(defn- prelude-mode? [] @(:prelude-mode? (cur)))

;; DIRECT-LINK MODE. Off for ordinary runs, the seed mint, and `-e`/repl/load-string
;; (open world — vars are redefinable). `jolt build` (release/optimized) flips it on
;; during app emission: a closed-world program where every app def is final, so an
;; app->app call binds to the def's Scheme binding directly, skipping the var-table
;; lookup and the generic jolt-invoke dispatch.
(defn set-direct-link! [on] (reset! (:direct-link? (cur)) (boolean on)))
(defn- direct-link? [] @(:direct-link? (cur)))

;; Fully-qualified app var names ("ns/name") already emitted with a direct-link
;; binding in the current unit; and, of those, the ones whose init is a fn literal
;; (safe to call as a raw Scheme application — a non-fn value is invokable in Clojure
;; but not a Scheme procedure, so its calls still route through jolt-invoke). A
;; call/value-ref direct-links only to a name defined earlier in emission order (or
;; itself), so the Scheme binding exists by the time the reference runs. Reset per build.
(defn direct-link-reset! []
  (reset! (:direct-link-defined (cur)) #{}) (reset! (:direct-link-fns (cur)) #{}))

;; Cache a resolved var cell in a per-site cell so a non-direct-linked var
;; reference skips the name lookup (string-append + hash) after the first use.
;; OFF during the seed mint (the seed must stay a byte-fixpoint, and caching the
;; compiler's own refs shifts the gensym-numbered cell names every pass); the
;; runtime eval path turns it on for user code, where it's the big win.
(defn set-var-cache! [on] (reset! (:var-cache? (cur)) (boolean on)))
(defn- var-cache? [] @(:var-cache? (cur)))

;; TARGET PRIMITIVES. The back end emits a small set of Chez-only spellings
;; directly — the #3% unsafe variants of runtime ops (vector-ref, fx=, the
;; flonum arithmetic ops) that the PIC inline-cache probe and the proven-
;; :double/:long emit sites rely on. A target port swaps THIS table, never the
;; emitter: each key is a MEANING, and all seven emission sites derive their
;; spelling from :unsafe-prefix + the safe op name (one mechanism, no per-site
;; strings). A target without unsafe variants maps :unsafe-prefix to "" — every
;; derived spelling degrades to the checked op, safe-and-slower, never wrong.
(defn set-target! [t] (reset! (:target (cur)) t))
(defn- target [] @(:target (cur)))
(def ^:private target-prims
  {:chez {:unsafe-prefix "#3%"}})
(defn- unsafe-prefix [] (get-in target-prims [(target) :unsafe-prefix]))

;; contagion clone sites (jolt devirt-gated fl* contagion for :num fields): a set of
;; "tag|proto|method" keys for impls whose body has a :num field beside a proven
;; :double operand — worth a contagion-specialized clone at the devirt site. It lives
;; on the compilation unit (:clone-sites), populated by a whole-program pre-pass
;; (contagion-prepass!) so the devirt-site resolution can consult it regardless of emit
;; order; nil in non-WP builds. The emit-time reader reaches it through the emit-unit
;; pointer (emit threads no unit) — nil off the optimize path, so clone-site? is false
;; and the non-specialized path stays byte-identical.
(defn- clone-site-key [tag proto method] (str tag "|" proto "|" method))
(defn- clone-site? [tag proto method]
  (when-some [u @jolt.op-registry/current-unit-box]
    (let [cs @(:clone-sites u)] (and cs (contains? cs (clone-site-key tag proto method))))))
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

;; Record-ctor shapes ("ns/->Name" -> {:fields (:k ..) :type tag}): the direct
;; fixed-arity ctor call emit-invoke recognizes reads the SAME record shapes the
;; inference installed on the unit (set-record-shapes!) — one registry, not a
;; separate backend copy. run-passes shares one unit for inference and emit, so it
;; is populated before each form's emit.
(defn- ctor-shapes [] (get @(:config (cur)) :record-shapes))

;; Tail-frame tracing (JOLT_TRACE): a TAIL CALL SITE stores its static
;; ('ns/fn' . line) pair in the site vreg — one store, no allocation, nothing
;; in a fn prologue (R4, bead jolt-hm1p) — and every call site registers its
;; static callee in the callsite tables at def load, from which the reporter
;; reconstructs TCO-erased chains. OFF during the seed mint (byte-determinism);
;; compile-eval.ss turns it on for runtime-eval'd user code and build.ss for
;; `jolt build` output, both honoring JOLT_TRACE=0.
(defn set-trace-frames! [on] (reset! (:trace-frames? (cur)) (boolean on)))
(defn- trace-frames? [] @(:trace-frames? (cur)))

;; The line an :invoke node sits on, or nil. The analyzer stamps :pos on every call
;; form the reader gave a position to; a macro-built form carries none.
(defn- node-line [node]
  (let [p (:pos node)] (when (map? p) (:line p))))

;; Under tracing, emit an inline Chez block comment #|L<line>|# right before every
;; call: a frame's source object gives a byte offset into the generated .scm, and
;; scanning back for the nearest marker recovers the ORIGINAL clj line
;; (source-registry.ss jolt-marker-line-at-offset). Must be a block comment — a
;; `;;` comment would comment out the rest of the one-line top-level form. R2
;; (bead jolt-knn8): the per-call (jolt-site! LINE) fixnum store is GONE — non-tail
;; code now compiles with no per-call instrumentation at all, so a continuation
;; names a non-tail frame with its exact line for free. The ('ns/fn' . line) PAIR
;; the innermost-frame splice needs is stored only by TAIL call emissions
;; (sited-tail-call / the :throw case), there AFTER the operands are temp-bound.
;; A pair now survives its chain's return, so the reporter validates the splice
;; against the compile-time callsite table (*callsites* / jolt-register-callsite!)
;; before accepting it (source-registry.ss jolt-site-splice?).
(defn- with-site [node s]
  (if-let [l (and (trace-frames?) (node-line node))]
    (str "#|L" l "|# " s)
    s))

;; Source-map registration for a fn def: one hashtable insert at definition time,
;; no per-call cost, so a backtrace can name a frame ns/name (file:line) instead of
;; falling back to its bare Chez procedure name. On for runtime eval
;; (compile-eval.ss) — a `jolt run` trace reads the same as an AOT build's. OFF for
;; the seed mint (emit-image.ss): the record carries the def's absolute path, and
;; baking this machine's paths into prelude.ss would break the byte-fixpoint
;; everywhere else.
(defn set-source-reg! [on] (reset! (:source-reg? (cur)) (boolean on)))
(defn- source-reg? [] @(:source-reg? (cur)))

;; A direct-link Scheme binding name for a var. The fqn maps to a unique identifier
;; jv$<ns>$<name>; chars that break a Scheme identifier or the `$` separator are
;; escaped so distinct vars never collide.
(defn- dl-munge [s]
  (-> s (str/replace "$" "_D_") (str/replace "#" "_H_") (str/replace "'" "_Q_")))
(defn- dl-name [ns nm] (str "jv$" (dl-munge ns) "$" (dl-munge nm)))
(defn- dl-fqn [ns nm] (str ns "/" nm))
(defn- direct-linkable? [ns nm]
  (and (direct-link?) (contains? @(:direct-link-defined (cur)) (dl-fqn ns nm))))
;; A direct-linked var whose value is a fn literal — its binding is a Scheme
;; procedure, so a call site can apply it directly.
(defn- direct-link-fn? [ns nm]
  (contains? @(:direct-link-fns (cur)) (dl-fqn ns nm)))

;; recur-target and the set of munged local names known to hold a procedure (a
;; named fn's self-recursion name) are lexically scoped — dynamic vars so the
;; recursion auto-restores them (no manual save/restore, no throw-leak).
(def ^:dynamic *recur-target* nil)
(def ^:dynamic *known-procs* #{})
;; When set (in the :def emit path), fns are emitted with a qualified letrec
;; binding (ns/name) so Chez reports a unique per-var frame name — no collisions
;; across namespaces. Nested/anonymous fns ignore it (they never register).
(def ^:dynamic *qualifying-ns* nil)
;; Set while emitting the init of a def whose value is an ANONYMOUS fn. Such a fn
;; is emitted bare so the enclosing (define jv$ns$name …) names the procedure and
;; its backtrace frame resolves; wrapping or re-binding it would drop that name.
;; emit-fn therefore skips its own variadic registration and emit-def-cached emits
;; the sibling (jolt-register-variadic! …) against the define's own binding.
(def ^:dynamic *variadic-reg-suppressed?* false)
;; R1 (bead jolt-hqpn): unique letrec names + source-form registration for
;; anonymous fn literals in NON-SYSTEM namespaces, so a live closure reports its
;; source form through Chez's inspector and a state image can rebuild it as code.
;; Bound where emit-top-form / the :def cases establish per-top-level-form
;; context: *fnsrc-ns* (the namespace being emitted), *fnsrc-def* (the enclosing
;; top-level def name, nil for a top-level expression), *fnsrc-counter* (an
;; atom: anon literals seen so far in this top-level form), *fnsrc-regs* (an
;; atom: the collected [name form ns free-names] rows, flushed as
;; image-register-fn-form! siblings after the form), and *fnsrc-def-init?* (true
;; only while emitting a def's DIRECT fn init, which the define itself names and
;; must not wrap). All nil/false outside any top-level form: emit-fn then keeps
;; the old bare lambda, so off-context emission is byte-unchanged.
(def ^:dynamic *fnsrc-ns* nil)
(def ^:dynamic *fnsrc-def* nil)
(def ^:dynamic *fnsrc-counter* nil)
(def ^:dynamic *fnsrc-regs* nil)
(def ^:dynamic *fnsrc-def-init?* false)
;; Same split as image-system-ns? in host/chez/state-image.ss: a namespace the
;; language owns (clojure.core or a clojure.* / jolt.* prefix) keeps its old
;; emission — the seed mint and the core overlay must stay byte-identical. nil
;; (no per-form context) counts as system so a bare emit never changes.
(defn- fnsrc-system-ns? [ns]
  (or (nil? ns)
      (= ns "clojure.core")
      (str/starts-with? ns "clojure.")
      (str/starts-with? ns "jolt.")))
;; True while emitting a node in TAIL position. Only used, in trace mode, to mark a
;; tail call so the runtime routes its callee into the current history rib instead
;; of a new one (rt.ss). It never affects semantics — a wrong value only mislabels
;; a debug trace line — so partial propagation is safe. `emit` (the wrapper below)
;; clears it by default; the tail-transparent forms (fn body, if/do/let/loop) pass
;; it to their tail child. Default false so a top-level form is treated non-tail.
(def ^:dynamic *tail?* false)
;; R1 (bead jolt-pfhc): the tail-site instrumentation's static site literal. While
;; emitting a named fn's body, *trace-site* is the enclosing fn's qualified name
;; (the same (or qname self) the old prologue push used) and *trace-self* is the
;; set of that fn's own binding names, so a tail call can stamp its rib entry
;; with the fn it lives in and elide direct self-tail-calls (the erased frame is
;; the enclosing fn, already named by the chain or the continuation). nil / empty
;; in code outside any named fn (top-level, anonymous fns): those tail sites skip.
(def ^:dynamic *trace-site* nil)
(def ^:dynamic *trace-self* #{})

;; R2 (bead jolt-knn8): compile-time callsite table, the staleness fix for the
;; site vreg once only tail sites write it. While emitting a def's init under
;; tracing, *callsites* holds an atom of {fn-qname {line static-callee}}; each
;; call site the emitter compiles records its static callee. The def emit renders
;; one (jolt-register-callsite! "ns/f" LINE "callee") sibling per entry, so the
;; reporter can verify the throw-stashed pair names the callee the innermost site
;; actually entered (source-registry.ss jolt-site-splice?). nil outside a def.
(def ^:dynamic *callsites* nil)
;; A line can carry SEVERAL calls ((println (kaboom 10)) — operand and outer
;; call share it), so each line maps to a VECTOR of distinct callees and the
;; validator accepts membership; a single-winner entry false-rejected genuine
;; pairs whenever calls nest on one line.
;; Entries are [callee tail?]: the reporter's chain reconstruction follows
;; TAIL edges only (a chain hop is by definition a tail call); the validator
;; consults every entry. A site seen both ways keeps tail? = true.
(defn- register-callsite! [line callee tail?]
  (when (and (trace-frames?) *callsites* *trace-site* line callee)
    (let [m @*callsites*
          sites (get m *trace-site*)
          cur (get sites line)
          seen (some (fn [e] (when (= (nth e 0) callee) e)) cur)]
      (cond
        (nil? seen)
        (swap! *callsites* assoc *trace-site*
               (assoc sites line (conj (or cur []) [callee (boolean tail?)])))
        (and tail? (not (nth seen 1)))
        (swap! *callsites* assoc *trace-site*
               (assoc sites line (mapv (fn [e] (if (= (nth e 0) callee) [callee true] e))
                                       cur)))))))
;; The static callee fqn of a call site's fn head, or nil for a genuinely dynamic
;; callee (an arbitrary IFn through jolt-invoke — register nothing for that line).
;; :var — the var's fqn, matching the qualified name a source-registered frame
;; reports; :local — a known procedure (self / a letrec-bound named fn): its
;; qualified name, or the enclosing fn's *trace-site* for a self-call; :keyword /
;; :coll / computed callees — nil.
(defn- static-callee [fnode]
  (case (:op fnode)
    ;; The registered name must be the SAME form the callee's own *trace-site*
    ;; uses, or the validator false-rejects every genuine pair: qualified
    ;; "(munged ns)/(munged name)" when source registration is on (dev, the
    ;; open-world build), the bare munged short name in a direct-link build
    ;; (qualifying is off there and sites record short names).
    :var (if (source-reg?)
           (str (munge-name (:ns fnode)) "/" (munge-name (:name fnode)))
           (munge-name (:name fnode)))
    :local (let [nm (munge-name (:name fnode))]
             (when (contains? *known-procs* nm)
               (if (contains? *trace-self* nm)
                 *trace-site*
                 (if *qualifying-ns* (str (munge-name *qualifying-ns*) "/" nm) nm))))
    nil))
;; The def-emit sibling: one (jolt-register-callsite! …) per collected (line,
;; callee) entry, or "" when tracing is off / nothing collected — the seed mint
;; and `jolt build` emit with tracing off, so they stay byte-identical. Entries
;; sorted for deterministic output; "last call emitted on a line wins" (a tail
;; call is emitted after its operands, so its callee wins a shared line).
(defn- trace-callsite-reg []
  (if (and (trace-frames?) *callsites* (seq @*callsites*))
    (str/join ""
      (for [[site sites] (sort-by key @*callsites*)
            [line callees] (sort-by key sites)
            [callee tail?] (sort-by (fn [e] (nth e 0)) callees)]
        (str " (jolt-register-callsite! " (chez-str-lit site) " " line " "
             (chez-str-lit callee) " " (if tail? "#t" "#f") ")")))
    ""))

(defn- fresh-label [prefix] (str prefix (swap! (:gensym-counter (cur)) inc)))

;; Per-site cache cells collected while emitting one top-level def. A site that
;; resolves a STABLE value — a devirtualized impl (constant tag/proto/method) or a
;; var cell (interned, so the cell never changes even when the var is redefined) —
;; resolves it once, not per call, the inline cache the JVM gets for free. When a
;; def init is being emitted this holds an atom; each site appends a fresh cell name
;; (bound to #f in a let wrapping the def, so it persists across calls and is shared
;; by every invocation) and resolves into it on first use. nil outside a def (a site
;; there falls back to a per-call resolve).
;;
;; THREAD-LOCAL, like every other piece of emit-session scratch in this file
;; (*fnsrc-counter*, *callsites*, *tail?*, …) and like the reference compiler, which
;; keeps its whole compilation state in thread-bound Vars. These two lived on the
;; unit — one object for the whole process — and emit-with-cells swapped them in and
;; out around each def. That save/restore is a read-modify-write on shared state, so
;; two threads compiling at once traded collectors: one thread's hoisted constant was
;; registered into the other's pool, the `let` that should have bound it never saw
;; it, and a namespace that compiled cleanly alone died with "variable _kc$81 is not
;; bound". Rebinding also unwinds on a throw, which the reset! pair did not — a
;; failed emit used to leave the unit pointing at the abandoned def's collector, and
;; every later def in the process appended cells to it.
;;
;; The gensym counter deliberately stays shared on the unit: swap! is atomic, and one
;; counter per process is what keeps a registered anon-fn name globally unique.
(def ^:dynamic *cache-cells* nil)
(def ^:dynamic *const-pool* nil)

;; Emit a def's init (via the supplied thunk) under a fresh cache-cell collector,
;; then wrap the result in a let binding any cells its body registered so they
;; persist in the def's closure. The binding nests for nested defs and unwinds on a
;; throw. Used by both the runtime def emit and the direct-link top-level emit.
;; Hoist a PURE CONSTANT construction out of its use site: the site emits a bare
;; variable read, and the def's wrapper binds it once. The pool is keyed by the
;; emitted expression, so a constant appearing at ten sites in one def is built
;; once and shared.
;;
;; Unlike the lazy cache cells above these bind EAGERLY — a constant has no
;; resolution to defer, so there is nothing for an (or cell …) branch to save and
;; a plain variable read is cheaper at every use.
;;
;; Only for constructions that are pure AND whose result is safe to share.
;; Keyword literals qualify on both counts: `keyword` interns, so it already
;; returns the same object for the same name and sharing changes nothing
;; observable. Symbols deliberately do NOT route through here — jolt-symbol
;; allocates a fresh symbol per call and symbols carry metadata, so sharing one
;; across sites could let meta leak between them.
;;
;; Outside a def (no pool) the raw expression is returned unchanged.
(defn- hoist-const [expr]
  (let [pool *const-pool*]
    (if pool
      (or (get @pool expr)
          (let [nm (fresh-label "_kc$")]
            (swap! pool assoc expr nm)
            nm))
      expr)))

(defn- emit-with-cells [emit-thunk]
  (let [cells (atom [])
        pool (atom {})
        raw (binding [*cache-cells* cells
                      *const-pool* pool]
              (emit-thunk))
        ;; constants bind eagerly (value first); lazy cache cells start #f. Sorted
        ;; by binding name so a build's output stays deterministic.
        consts (map (fn [p] (str "(" (val p) " " (key p) ")"))
                    (sort-by val @pool))
        lazies (map (fn [c] (str "(" c " #f)")) @cells)
        binds  (concat consts lazies)]
    (if (seq binds)
      (str "(let (" (str/join " " binds) ") " raw ")")
      raw)))

;; A cache-cell scope for a top-level EXPRESSION (jolt-g3u). Without it the
;; collector exists only inside a def, so every var / protocol / ctor site in a
;; bare top-level form resolves PER ACCESS. Measured: a var-ref call at such a
;; site costs 101-125 ns against 28 ns at a cached site and 11 ns through a local,
;; and 3M calls do 3M var-table reads against 2 at a cached site. A bare top-level
;; (loop …) went 120.8 -> 25.1 ns/op with this.
;;
;; TWO restrictions, both of which cost real measurements to find.
;;
;; Not a def. A direct-link def emits a bare (define …), which cannot sit inside a
;; let, and both :def paths already wrap their own INIT — the part that repeats.
;;
;; And only when the form contains a LOOP (or a self-recurring fn), not merely a
;; :fn. That looks over-cautious and is not. A cell has to be paid for: the emitted
;; (var-cell-deref (or c …)) is several times the text of a (var-deref …), and a
;; top-level form is compiled by Chez every time it is loaded. So the cell wins
;; only if its site actually runs again, and a loop is the one thing that proves
;; it will — a top-level fn may never be called. Wrapping on :fn too cost 14% on
;; `require clojure.test` (0.44 -> 0.50s), verified by switching the wrapper at
;; runtime inside ONE binary so no build variance was involved, and bought that
;; path nothing: a namespace being loaded is defs and defmethods whose bodies run
;; once at load. Confirmed it was the cells and not the const pool by trying a
;; cells-only variant, which was equally slow.
;;
;; Gated on var-cache? for the same reason the :var site is: the seed mint runs
;; with it OFF and must stay a byte-fixpoint, and this wrapper would otherwise
;; change mint output through the const pool (which no other flag gates) even
;; though no cache cell would be registered.
(def ^:private repeat-ops #{:loop :recur})

;; Keys whose value is PAYLOAD hanging off a node rather than IR under it. They
;; have to be skipped, and :src-form is not an optimization — it is the difference
;; between linear and quadratic. Every :fn node carries its whole source form, so a
;; fn literal written inside another is in both, and walking them all costs the sum
;; of the subtree sizes. 240 nested literals spent 89 ms of a 90 ms emit right here,
;; and it showed up as 633k reads of clojure.core/map?, sequential?, vector? and
;; seq? — this walk's own operations, called O(source size) times per fn.
;;
;; Skipping it cannot lose a loop. :src-form is the SOURCE the image rebuilds a
;; closure from; a loop written in it is already in the IR under :arities, which is
;; walked. :free-names is a vector of name strings and holds no nodes at all.
;; Checked rather than asserted: over 247 top-level forms from eight real files the
;; two walks never disagreed.
;;
;; They CAN disagree in one direction, and it is the harmless one. A source form
;; carrying a map literal that happens to look like IR — (fn [] {:op :loop}) — used
;; to answer "repeats" off the DATA, and now does not. The wrapper this drives is an
;; optimization, so the old answer only bought that form some cache cells it could
;; not use; nothing reads the cells that is not also emitted by the same pass. A
;; quoted {:op :recur} still answers true, through the :quote node rather than
;; through :src-form — pre-existing, equally harmless, and left alone.
(def ^:private node-payload-keys #{:src-form :free-names})

(defn- node-tree-any?
  "Does any node in this tree satisfy pred? Walks map values and sequential
  children, which covers every shape the analyzer builds — minus the payload keys
  above, which hold source and names rather than IR."
  [pred x]
  (cond
    (and (map? x) (:op x) (pred x)) true
    ;; a :const holds a literal, which cannot contain a loop and can be arbitrarily
    ;; large (a 10k-element vector literal at top level). Stop rather than walk it.
    (and (map? x) (= :const (:op x))) false
    (map? x) (boolean (some (fn [e] (and (not (contains? node-payload-keys (key e)))
                                         (node-tree-any? pred (val e))))
                            x))
    (sequential? x) (boolean (some (fn [v] (node-tree-any? pred v)) x))
    :else false))

;; Does this top-level form contain something whose body PROVABLY runs more than
;; once? A loop (or a self-recurring fn) does. A plain :fn does not — it may never
;; be called, and that distinction is the whole design; see emit-top-cells.
(defn- top-form-repeats? [node]
  (node-tree-any? (fn [n] (contains? repeat-ops (:op n))) node))

(defn- emit-top-cells [node emit-thunk]
  (if (and (var-cache?)
           ;; never a def: the :def cases already wrap their INIT, which is the
           ;; part that repeats. A def's remaining pieces run once.
           (not= :def (:op node))
           (top-form-repeats? node))
    (emit-with-cells emit-thunk)
    (emit-thunk)))

;; Scheme syntactic keywords. A jolt local with one of these names would, when
;; emitted verbatim, shadow the Scheme form in operator position (a local named
;; `if` would turn the special form (if …) the back end emits into a call), so
;; such locals are prefixed. Matches the spec: special-form heads are not
;; shadowable, but a value local may legally be named `if`.
(def ^:private scheme-reserved
  #{"if" "begin" "lambda" "let" "let*" "letrec" "letrec*" "quote" "quasiquote"
    "unquote" "set!" "define" "define-syntax" "cond" "case" "when" "unless"
    "and" "or" "do" "else" "guard" "parameterize" "delay" "values"})

;; clojure.core ops emitted as a BARE Scheme name (where native-ops maps the op
;; to itself: + - * / < > min max …). A local binding with one of these names
;; would otherwise shadow the emitted prim — e.g. (fn [max] (clojure.core/max …))
;; emits (max …) calling the param — so such locals are prefixed, like reserved
;; words. Derived from native-ops so the two never drift.
(def ^:private bare-native-names
  (set (keep (fn [[k v]] (when (= k v) k)) native-ops)))

;; All Scheme identifiers the back end emits bare — every :call, :dbl, :lng,
;; :fixed, :value, and :bd name from op-registry, plus a closed set of runtime
;; helpers the registry doesn't enumerate (devirt dispatchers, jolt-invokeN,
;; var-deref, list->cseq, literal/interop heads, etc.). Derived from op-registry
;; so it stays in sync with per-op facts. A local whose munged name matches ANY
;; of these would shadow the emitted runtime procedure (e.g. (let [jolt-nil 5]
;; nil) returns 5 if the local is emitted verbatim).
;; INVARIANT: any NEW bare Scheme head an emit* clause outputs — a procedure or
;; special-form name emitted as a call head that a user local could shadow —
;; MUST be added to the `helpers` set below. The op-registry derivation covers
;; registry-driven heads; everything else is enumerated there by emission site.
(def ^:private rt-emitted-names
  (let [from-registry registry-emitted-names
        helpers #{"jolt-nil" "jolt-invoke" "jolt-invoke0" "jolt-invoke1"
                  "jolt-invoke2" "jolt-invoke3" "jolt-invoke4"
                  "var-deref" "list->cseq" "cseq->list"
                  "jolt-rest-seq" "jolt-register-variadic!"
                  "make-jrec" "jrec-field-at" "jrec-field-set!"
                  "devirt-resolve" "devirt-resolve-fl"
                  "register-clone*" "protocol-resolve"
                  "protocol-dispatch1" "protocol-dispatch2" "protocol-dispatch3"
                  "jolt->fl" "jolt->fx"
                  "jolt-register-source!" "jolt-proto-epoch"
                  ;; --- bare heads emitted at sites the registry doesn't cover ---
                  ;; INVARIANT: any new bare Scheme head an emit* clause outputs
                  ;; (a head a user local could shadow) MUST be added here. Literal
                  ;; + form emission (emit-const): keyword/char construction.
                  "keyword" "integer->char"
                  ;; fn forms (emit-fn): a multi-arity fn lowers to case-lambda.
                  "case-lambda"
                  ;; binding + try/finally (emit-try): both thread through
                  ;; dynamic-wind; a binding form and a finally both emit it.
                  ;; jolt-finally-in is that dynamic-wind's before-thunk, emitted
                  ;; by name because the fiber park recognises it with eq? — a
                  ;; shadowed or inlined copy would make the finally run mid-park.
                  "dynamic-wind" "jolt-finally-in"
                  ;; host interop (emit-invoke static call, :host-new,
                  ;; :host-static field ref).
                  "host-new" "host-static-call" "host-static-ref"
                  ;; record/reify protocol-method dispatch (:host-call fallback
                  ;; for any .method not in supported-host-methods).
                  "record-method-dispatch"
                  ;; cell-cached var deref (the whole-program var-cache? path).
                  "var-cell-deref"
                  ;; devirt cached-desc lookup (emit-invoke ctor inlining).
                  "hashtable-ref"
                  ;; top-level def / forward-declare / ns-value splice
                  ;; (emit-def-cached, :forward-decl, :the-ns).
                  "define" "def-var!" "def-var-with-meta!"
                  "declare-var!" "intern-ns!"
                  ;; ffi lowering (emit-ffi-fn/emit-ffi-callable: the sa-* adapter
                  ;; syntaxes a Chez foreign-procedure/callable expands to).
                  "sa-foreign-procedure" "sa-foreign-procedure-blocking"
                  "sa-foreign-callable" "sa-foreign-callable-collect-safe"}]
    (into from-registry helpers)))

;; Most jolt names are already valid Scheme identifiers. The one that isn't is
;; `#`, which jolt auto-gensyms use as a suffix (p1__0000X4# from #(...)) — `#`
;; starts a datum in Scheme, so replace it with `_`. A name that collides with a
;; Scheme keyword, a bare-emitted native op, or ANY runtime-emitted identifier
;; (prefix "jolt-", "jv$", or in rt-emitted-names) is prefixed with `_` so it
;; can never shadow the emitted form. The "jolt-"/"jv$" prefix rules are a
;; safety net for identifiers added to the runtime that aren't yet in the
;; registry — they catch future additions without manual enumeration.
(defn- munge-name [s]
  ;; A Clojure symbol may carry chars that break a Scheme identifier or that
  ;; collide once substituted: ' is the quote reader macro (a bare f' reads as f
  ;; then 'rest), # is the auto-gensym suffix the reader puts on #() params
  ;; (p__1#) and starts a Scheme datum, and $ is the escape marker used below.
  ;; Map all three to safe tokens INJECTIVELY so two distinct Clojure locals can
  ;; never munge to the same Scheme identifier: reserve $ as the escape char and
  ;; escape it FIRST ($$ = a literal $), then ' -> $P and # -> $H. Decoding is an
  ;; unambiguous left-to-right inverse ($$ -> $, $P -> ', $H -> #): every $ in
  ;; the output is either doubled (came from a literal $) or single+P/+H (came
  ;; from ' / #), so no two inputs share an output. The same mapping applies at
  ;; the binding and at every reference, so resolution stays consistent. Only the
  ;; char-substitution step changes; the reserved/emitted-name _-prefix below is
  ;; untouched (it runs on the substituted string; a $ -> $$ still leaves the
  ;; jv$/jolt- prefix tests true, and those runtime names never reach munge-name).
  (let [s (-> s
              (str/replace "$" "$$")
              (str/replace "'" "$P")
              (str/replace "#" "$H"))]
    (if (or (contains? scheme-reserved s)
            (contains? bare-native-names s)
            (contains? rt-emitted-names s)
            (.startsWith ^String s "jolt-")
            (.startsWith ^String s "jv$"))
      (str "_" s) s)))

(declare emit)
(declare emit*)
;; Ops that pass tail position through to a child (the child can itself be a tail
;; call): if/do carry it to their tail branch/last form, let/loop to their body,
;; and invoke reads it to decide whether the call is tail. Every other op's
;; children are non-tail, so `emit` clears *tail?* before dispatching them — that
;; way a stray true can't leak into, say, a call sitting in a vector literal.
;; :throw is tail-transparent so the :throw emit case still sees *tail?* — a
;; TAIL throw must store the site-vreg pair (sited-tail-call) or its TCO-erased
;; frame has no name at report time (app.tailstale's thrower).
(def ^:private tail-transparent-ops #{:if :do :let :loop :invoke :throw})
(defn emit [node]
  (let [s (if (and *tail?* (not (tail-transparent-ops (:op node))))
            (binding [*tail?* false] (emit* node))
            (emit* node))]
    ;; a :long operand of a :double-specialized op is tagged :fl-coerce by
    ;; jolt.passes.numeric so it widens to a flonum here (JVM long->double widening
    ;; == fixnum->flonum); the :long contract guarantees a real fixnum.
    (if (:fl-coerce node)
      (str "(fixnum->flonum " s ")")
      s)))

;; A Chez string literal. Every char outside printable ASCII becomes a
;; codepoint hex escape \x<cp>; ; the named escapes (\n \t \r \" \\) match what
;; Chez's reader accepts. For pure printable ASCII this is byte-identical to %j.
(defn- char-escape [cp]
  (cond
    (= cp 34) "\\\""
    (= cp 92) "\\\\"
    (= cp 10) "\\n"
    (= cp 9) "\\t"
    (= cp 13) "\\r"
    (and (>= cp 32) (< cp 127)) (str (char cp))
    :else (str "\\x" (format "%x" cp) ";")))

(defn- chez-str-lit [s]
  (str "\"" (apply str (map (fn [c] (char-escape (int c))) s)) "\""))

(defn- emit-const [v]
  (cond
    (nil? v) "jolt-nil"
    (boolean? v) (if v "#t" "#f")
    ;; Numeric tower: emit a literal Chez re-reads as the SAME number, via the
    ;; host's own writer (jolt.host/chez-number-literal) — NOT jolt's str, whose
    ;; rendering follows the reference printer (bigint N suffix, E exponents)
    ;; and is not Chez-readable. ##Inf/##-Inf/##NaN -> Chez's flonum literals.
    (number? v) (cond
                  (= v ##Inf) "+inf.0"
                  (= v ##-Inf) "-inf.0"
                  (not= v v) "+nan.0"
                  :else (jolt.host/chez-number-literal v))
    (string? v) (chez-str-lit v)
    ;; keyword literal -> (keyword ns name), hoisted to a per-def constant so the
    ;; intern lookup runs once per def rather than at every use site — a keyword
    ;; in a hot path ((:left node), a cond's :else, a map key) was re-interned on
    ;; every evaluation, ~25% of the cost of a keyword-keyed lookup.
    (keyword? v) (hoist-const
                   (if-let [kns (namespace v)]
                     (str "(keyword " (chez-str-lit kns) " " (chez-str-lit (name v)) ")")
                     (str "(keyword #f " (chez-str-lit (name v)) ")")))
    ;; char literal -> (integer->char <codepoint>). Get the codepoint via the host
    ;; contract (form-char-code), NOT (get v :ch): on Chez (the self-hosted spine)
    ;; a char is a native char, so a struct-field read returns nil and would emit
    ;; (integer->char) with no arg.
    (form-char? v) (str "(integer->char " (form-char-code v) ")")
    :else (throw (ex-info (str "emit-const: unsupported literal " (pr-str v)) {}))))

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

;; An operand whose evaluation has no observable effect: constants, locals,
;; var/the-var reads, quoted literals.
(defn- side-effect-free? [n]
  (contains? #{:const :local :var :the-var :quote} (:op n)))

;; A var VALUE read is effect-free but order-SENSITIVE: a mutating sibling
;; (def/alter-var-root/set!) changes what it yields, so it must not move across
;; an effectful operand in either direction. A :the-var read (the var object)
;; is a stable reference — insensitive.
(defn- order-sensitive-read? [n]
  (= :var (:op n)))

;; Clojure evaluates a call's operands (and recur's args, and collection-literal
;; elements) left to right; Chez's application order is unspecified
;; (right-to-left in practice). Force source order by binding operands to fresh
;; temps in a let* — but only when an effectful operand coexists with another
;; order-sensitive one (effectful or a var read), so hot calls over
;; locals/consts stay un-wrapped.
(defn- needs-order? [nodes]
  (let [e (count (remove side-effect-free? nodes))]
    (and (>= e 1)
         (>= (+ e (count (filter order-sensitive-read? nodes))) 2))))

;; Build a call from operand strings, forcing left-to-right evaluation when
;; needed. `nodes`/`strs` are the operands (parallel); `build` receives the
;; operand strings to splice (temps when wrapped, raw otherwise) and returns the
;; call. Operands that don't need ordering are passed through inline.
(defn- ordered-call [nodes strs build]
  (if (needs-order? nodes)
    (let [tmps (mapv (fn [_] (fresh-label "_a$")) strs)
          binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps strs))]
      (str "(let* (" binds ") " (build tmps) ")"))
    (build strs)))

;; Quoted literals. A :quote node's :form is the RAW reader form;
;; reconstruct each as the matching Chez RT constructor — the runtime value of a
;; quote is just that literal data. The form is walked via the jolt.host form-*
;; contract (the portable seam the analyzer uses), NOT host-native predicates, so
;; this stays host-neutral — the contract walks the host's reader forms.
(declare emit-quoted)

;; --- sharing the quoted constructions ----------------------------------------
;; emit-quoted renders a form as the constructor calls that rebuild it, and it
;; renders a SUBTREE every time it meets one. That is fine for a lone quoted datum
;; and quadratic for the fnsrc registrations, which are the one caller that emits
;; forms NESTED INSIDE EACH OTHER: every anon fn registers its whole source form,
;; and an anon fn written inside another one is part of both, so a chain of depth
;; N emits O(N^2) text. The CPS pass in clojure.core.async builds exactly that
;; chain — one closure per park site — and a 240-park go body emitted 5.4 MB of
;; registrations, which analyze/emit/Chez then each paid for in proportion.
;;
;; So the pool hash-conses: a construction is emitted once, bound to a name, and
;; every later occurrence of the same construction is that name. Because a child's
;; construction is interned before its parent's is built, the parent is assembled
;; out of NAMES and costs O(its own arity) rather than O(its subtree) — which is
;; what turns the total linear. It also collapses the repetition inside one form
;; (every (jolt-symbol #f "x") in a body is now one object).
;;
;; Sharing the VALUES is sound because these are immutable constructions: symbols,
;; persistent collections and literals. It is the same argument hoist-const makes,
;; and the values reaching image-register-fn-form! are equal to the ones it got
;; before, structurally and by every observation the image writer makes of them.
;;
;; nil pool (every other caller, and every system namespace) is the identity: the
;; construction is returned inline, byte for byte as before, so the seed mint and
;; the core overlay are untouched.
(def ^:dynamic *quote-pool* nil)

;; Rows already emitted, as [src-form binding-name]. Interning alone made the TEXT
;; linear but left the WALK quadratic: assembling a parent's construction still
;; descended through the nested literal it contains, all the way to the leaves,
;; only to rediscover constructions the pool already had. This stops the descent —
;; a sub-form that IS an already-emitted literal is that literal's binding name.
;;
;; Matched by IDENTITY, which is exact here and costs a pointer compare: analyze-fn
;; hands a literal's :src-form back as the very object nested inside its parent's
;; :src-form. Equality would be the obvious alternative and is the wrong tool — a
;; jolt seq does not cache its hash, so asking whether two forms are equal walks
;; the subtree this exists to avoid walking. A miss (a form the analyzer had to
;; rebuild) just descends as before.
(def ^:dynamic *quote-shared* nil)

(defn- q-shared-name [form]
  (when-let [rows *quote-shared*]
    (loop [i 0]
      (when (< i (count rows))
        (let [r (nth rows i)]
          (if (identical? (nth r 0) form) (nth r 1) (recur (inc i))))))))

(defn- q-intern
  "The text to use at a USE SITE for a construction: the construction itself with
  no pool, else a binding name, recording the construction once."
  [expr]
  (let [pool *quote-pool*]
    (if (nil? pool)
      expr
      (or (get (:by-expr @pool) expr)
          (let [nm (str "_q$" (count (:order @pool)))]
            (swap! pool (fn [p] (-> p
                                    (assoc-in [:by-expr expr] nm)
                                    (update :order conj [nm expr]))))
            nm)))))

;; Both sorted renderings below order by EMITTED TEXT, because a set and a map
;; value have no source order and the host hash order is not stable across Chez
;; versions (jolt-8479). A pooled subterm renders as a binding name handed out in
;; traversal order — i.e. in host-hash order — so sorting those would reintroduce
;; exactly the instability the sort exists to remove. Emit the items unpooled, sort
;; the real text, and intern only the finished construction.
(defn- emit-quoted-map [pairs]
  ;; pairs: a jolt vector of [k-form v-form] pairs (form-map-pairs)
  (q-intern
   (str "(jolt-hash-map "
        (str/join " " (mapcat (fn [p] [(emit-quoted (nth p 0)) (emit-quoted (nth p 1))]) pairs))
        ")")))
(defn- emit-quoted-map-value [m]
  ;; A jolt map VALUE (def/symbol metadata is a value, not a reader form). (keys m)
  ;; iterates in host-hash order, which is not stable across Chez versions, so emit
  ;; the pairs sorted by their emitted Scheme text — keeps the seed byte-fixed
  ;; regardless of the host hash (jolt-8479).
  (let [pairs (binding [*quote-pool* nil]
                (vec (sort (map (fn [k] (str (emit-quoted k) " " (emit-quoted (get m k)))) (keys m)))))]
    (q-intern (str "(jolt-hash-map " (str/join " " pairs) ")"))))
;; emit-quoted reconstructs both raw reader forms (from :quote) AND plain jolt
;; values (def/symbol :meta). Reader forms are walked via the jolt.host form-*
;; contract; the native-predicate branches below catch genuine jolt collection
;; VALUES. The form-* branches come first so a reader form (a host-native struct/
;; array that a native predicate might also match) is always handled as a form.
(defn- emit-quoted [form]
  (cond
    (form-char? form) (emit-const form)
    (form-literal? form) (emit-const form)
    (form-sym? form)
    (let [m (form-sym-meta form) sns (form-sym-ns form) nm (form-sym-name form)]
      (q-intern
       (if (and m (pos? (count m)))
         ;; carry reader metadata (^:foo bar) onto the quoted symbol so (meta 'x) sees it
         (str "(jolt-symbol/meta " (if sns (chez-str-lit sns) "#f") " " (chez-str-lit nm) " "
              (emit-quoted m) ")")
         (str "(jolt-symbol " (if sns (chez-str-lit sns) "#f") " " (chez-str-lit nm) ")"))))
    ;; sort items by emitted text: a set has no source order, and host-hash order
    ;; is not stable across Chez versions (jolt-8479) — so the items are emitted
    ;; unpooled, or the sort would be over binding names handed out in that same
    ;; unstable order. See the pool comment above.
    (form-set? form)
    (q-intern (str "(jolt-hash-set "
                   (str/join " " (binding [*quote-pool* nil]
                                   (vec (sort (map emit-quoted (form-set-items form))))))
                   ")"))
    ;; a fn literal is a list, so this is the only branch that has to ask
    (form-list? form)
    (or (q-shared-name form)
        (q-intern (str "(jolt-list " (str/join " " (map emit-quoted (form-elements form))) ")")))
    (form-vec? form) (q-intern (str "(jolt-vector " (str/join " " (map emit-quoted (form-vec-items form))) ")"))
    (form-map? form) (emit-quoted-map (form-map-pairs form))
    ;; a quoted #"…" regex value -> reconstruct it (same as the :regex IR leaf).
    (form-regex? form) (str "(jolt-regex " (chez-str-lit (form-regex-source form)) ")")
    ;; quoted #inst / #uuid literals construct their value, like the JVM reader
    ;; (which builds the Date/UUID at read time, so a quoted/macro form carries the
    ;; value, not the raw tagged form). Same emit as the :inst / :uuid IR leaves.
    (form-inst? form) (str "(jolt-inst-from-string " (chez-str-lit (form-inst-source form)) ")")
    ;; a Class value inside quoted structure (a macro spliced (resolve 'C) under
    ;; a quote) reconstructs through the interner, like #inst/#uuid.
    (form-class-value? form) (str "(jolt-class-for " (chez-str-lit (form-class-value-name form)) ")")
    (form-uuid? form) (str "(jolt-uuid-from-string " (chez-str-lit (form-uuid-source form)) ")")
    ;; a quoted custom #tag with no registered reader -> a tagged-literal value
    ;; (Clojure's reader builds a TaggedLiteral), not the raw reader map. The tag is
    ;; stored as a :#name keyword; strip the leading # to the bare symbol.
    (and (map? form) (= :jolt/tagged (get form :jolt/type)))
    (let [nm (name (get form :tag))
          tsym (if (= \# (first nm)) (subs nm 1) nm)]
      (q-intern (str "(jolt-tagged-literal (jolt-symbol #f " (chez-str-lit tsym) ") "
                     (emit-quoted (get form :form)) ")")))
    ;; plain jolt VALUES (metadata maps and anything nested in them)
    (map? form) (emit-quoted-map-value form)
    (vector? form) (q-intern (str "(jolt-vector " (str/join " " (map emit-quoted form)) ")"))
    (set? form) (q-intern (str "(jolt-hash-set "
                               (str/join " " (binding [*quote-pool* nil]
                                               (vec (sort (map emit-quoted form)))))
                               ")"))
    (seq? form) (q-intern (str "(jolt-list " (str/join " " (map emit-quoted form)) ")"))
    :else (throw (ex-info (str "emit-quoted: unsupported quoted form " (pr-str form)) {}))))

;; A def's :meta is a jolt map value. Non-empty? (a plain def carries {}).
(defn- jmeta-nonempty? [m] (and (map? m) (pos? (count m))))

;; The meta argument to def-var-with-meta!. When the analyzer attached a
;; :meta-expr (metadata with values to evaluate, e.g. ^{:a some-fn}), emit it as a
;; runtime expression; otherwise the static :meta map as quoted data.
(defn- emit-def-meta [node]
  (if (:meta-expr node)
    (emit (:meta-expr node))
    (emit-quoted (:meta node))))

(defn- emit-binding [b]
  (str "(" (munge-name (nth b 0)) " " (emit (nth b 1)) ")"))

;; letfn lowers to a :let flagged :letrec (mutually-recursive named local fns):
;; Scheme `letrec*` binds them so each sees its siblings. A plain let uses let*.
(defn- emit-let [node]
  (let [kw (if (:letrec node) "letrec*" "let*")
        ;; bindings are non-tail; the body inherits the let's tail position
        binds (binding [*tail?* false] (str/join " " (mapv emit-binding (:bindings node))))]
    (str "(" kw " (" binds ") " (emit (:body node)) ")")))

(defn- emit-loop [node]
  (let [label (fresh-label "loop")
        pairs (:bindings node)
        names (map #(munge-name (nth % 0)) pairs)
        ;; inits evaluate in the OUTER scope (recur-target unchanged) and, like
        ;; Clojure loop/let, SEQUENTIALLY — wrap a let* around the named let.
        inits (binding [*tail?* false] (mapv #(emit (nth % 1)) pairs))
        seq-bs (str/join " " (map (fn [n i] (str "(" n " " i ")")) names inits))
        rebinds (str/join " " (map (fn [n] (str "(" n " " n ")")) names))
        ;; the loop body inherits the loop's tail position
        body (binding [*recur-target* label] (emit (:body node)))]
    (str "(let* (" seq-bs ") (let " label " (" rebinds ") " body "))")))

;; jolt.ffi/__cfn -> a Chez foreign-procedure (jolt-ffi). The C symbol + types are
;; compile-time literals from the analyzer, so this emits a real typed binding;
;; the resulting Scheme procedure is callable like any jolt fn. The library must
;; have loaded the shared object (jolt.ffi/load-library) before this def runs.
(def ^:private ffi-types
  {"int" "int" "uint" "unsigned-int" "long" "long" "ulong" "unsigned-long"
   "int64" "integer-64" "uint64" "unsigned-64" "size_t" "size_t" "ssize_t" "ssize_t"
   "iptr" "iptr" "uptr" "uptr" "double" "double" "float" "float"
   "pointer" "void*" "void*" "void*" "string" "string" "void" "void"
   "uint8" "unsigned-8" "u8" "unsigned-8" "byte" "unsigned-8" "char" "char"})
(defn- ffi-type->chez [t]
  (or (ffi-types t) (throw (ex-info (str "jolt.ffi: unknown foreign type :" t) {}))))
(defn- emit-ffi-fn [node]
  ;; A "varargs" marker in the argtype vector declares the binding variadic and
  ;; marks the FIXED/VARIADIC boundary: types before it are the named
  ;; parameters, types after it are the concrete variadic arguments this
  ;; binding always passes. The call is emitted with Chez's (__varargs_after n)
  ;; convention (n = the fixed-arg count = the marker's index), so the variadic
  ;; arguments travel where the callee's va_list reads them — Apple arm64
  ;; passes variadic args on the stack, and a fixed-arity binding silently
  ;; corrupts them (fcntl, ioctl, open). C requires a named parameter before
  ;; the ellipsis, and a trailing marker would declare nothing variadic, so
  ;; both malformed shapes are rejected. Only supported on the non-blocking
  ;; path: __collect_safe cannot combine with a varargs convention.
  (let [at (:argtypes node)
        vi (first (keep-indexed (fn [i t] (when (= t "varargs") i)) at))]
    (when (and vi (zero? vi))
      (throw (ex-info "jolt.ffi: :varargs needs at least one fixed argtype before it"
                      {:argtypes at})))
    (when (and vi (= vi (dec (count at))))
      (throw (ex-info "jolt.ffi: :varargs marks the boundary — the variadic argtypes follow it"
                      {:argtypes at})))
    (when (and vi (:blocking node))
      (throw (ex-info "jolt.ffi: :varargs cannot combine with :blocking" {:argtypes at})))
    (let [types (if vi (vec (concat (subvec at 0 vi) (subvec at (inc vi)))) at)
          n (count types)
          params (mapv (fn [i] (str "a" i)) (range n))
          conv (if vi (str " (__varargs_after " vi ")") "")
          fp (str "(" (if (:blocking node) "sa-foreign-procedure-blocking " "sa-foreign-procedure ")
                  conv " "
                  (chez-str-lit (:csym node))
                  " (" (str/join " " (map ffi-type->chez types)) ") "
                  (ffi-type->chez (:rettype node)) ")")]
      ;; Lazy resolution: the foreign-procedure form is deferred inside a closure.
      ;; On first call, the cell `p` is set to the FP and then invoked; subsequent
      ;; calls skip the set!. This lets a defcfn's defining form (top-level def)
      ;; evaluate to a callable closure before the shared library is loaded —
      ;; critical for :optional :jolt/native libs whose load-object runs in the
      ;; scheme-start launcher, after the heap is already built.
      ;;
      ;; Scoped resolution: dlsym the symbol against the RTLD_LOCAL handles a
      ;; :jolt/native library registered, and build the foreign-procedure FROM THE
      ;; ADDRESS on a hit (Chez accepts a runtime integer address in the entry
      ;; position). Falls back to fp (global name resolution) when no handle has
      ;; the symbol. Skipped for :varargs bindings — those are libc functions
      ;; (fcntl/ioctl) that resolve globally as process symbols, and address +
      ;; (__varargs_after n) is untested. defcfn's surface syntax is unchanged.
      (let [scoped (if vi "#f"
                     (str "(let ((a (jolt-ffi-dlsym-native " (chez-str-lit (:csym node)) "))) "
                          "(and a (foreign-procedure " (when (:blocking node) "__collect_safe ")
                          "a (" (str/join " " (map ffi-type->chez types)) ") "
                          (ffi-type->chez (:rettype node)) ")))"))]
        (str "(let ((p #f)) (lambda (" (str/join " " params) ") "
             "((or p (begin (set! p (or " scoped " " fp ")) p)) " (str/join " " params) ")))")))))

;; jolt.ffi/__ccallable -> a Chez foreign-callable wrapping the emitted jolt fn,
;; locked + registered (jolt-ffi-register-callable!, host/chez/java/ffi.ss) so the
;; collector neither moves nor reclaims it while C may still call through it. The
;; expression evaluates to the entry-point address — a jolt pointer the caller
;; hands to C. :collect-safe emits the convention that reactivates the thread on
;; entry, for callbacks invoked while it is parked in a :blocking foreign call.
(defn- emit-ffi-callable [node]
  (str "(jolt-ffi-register-callable! ("
       (if (:collect-safe node) "sa-foreign-callable-collect-safe " "sa-foreign-callable ")
       (emit (:fn node))
       " (" (str/join " " (map ffi-type->chez (:argtypes node))) ") "
       (ffi-type->chez (:rettype node)) "))"))

(defn- emit-recur [node]
  (when-not *recur-target* (throw (ex-info "emit: recur outside a loop/fn target" {})))
  (let [arg-nodes (:args node)]
    (ordered-call arg-nodes (mapv emit arg-nodes)
                  (fn [as] (str "(" *recur-target* " " (str/join " " as) ")")))))

;; One arity -> a Scheme lambda param-list + a named-let-wrapped body. The named
;; let lets fn-level `recur` rebind this arity's params. A variadic arity takes a
;; Scheme rest arg coerced to a jolt seq (nil when empty); recur carries the rest
;; seq directly, and the named let's init only runs on first entry.
;; Coerce a numeric-hinted param at fn entry, the way the JVM coerces a primitive
;; parameter: ^double -> jolt->fl, ^long -> jolt->fx. Only the named-let init
;; (first entry) coerces — recur carries already-typed values, like a JVM goto. This
;; is what makes the hint a contract the body's fl*/fx* ops can rely on. `orig` is
;; the param's source name (the :nhints key); `munged` the emitted identifier.
(defn- nhint-init [nh orig munged]
  (let [k (get nh orig)]
    (cond (= k :double) (str "(jolt->fl " munged ")")
          (= k :long)   (str "(jolt->fx " munged ")")
          :else munged)))

(defn- emit-arity-clause [a]
  (let [orig (:params a)
        nh (into {} (:nhints a))
        params (map munge-name orig)
        restp (when-let [r (:rest a)] (munge-name r))
        label (fresh-label "fnrec")
        ret (:ret-nhint a)
        ;; the body is the fn's tail position — UNLESS a ^double/^long return hint
        ;; wraps it in a coercion below, which puts the body back in non-tail.
        body-tail? (not (or (= ret :double) (= ret :long)))
        body (binding [*recur-target* label *tail?* body-tail?] (emit (:body a)))
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
    [paramlist (cond (= ret :double) (str "(jolt->fl " lett ")")
                     (= ret :long)   (str "(jolt->fx " lett ")")
                     :else lett)]))

;; The globally unique letrec name for the next anon literal:
;; jfn$<munged-ns>$<munged-def>$<counter> (counter per top-level def);
;; literals outside any def use jfn$<munged-ns>$$<counter> (counter per
;; top-level form). Deterministic: same source emits the same names.
(defn- fnsrc-name []
  (str "jfn$" (munge-name *fnsrc-ns*)
       (if *fnsrc-def* (str "$" (munge-name *fnsrc-def*) "$") "$$")
       (let [n @*fnsrc-counter*] (swap! *fnsrc-counter* inc) n)))

;; A top-level form's collected anon-fn registrations as Scheme siblings:
;;   (image-register-fn-form! "jfn$…" <quoted fn* form> "ns" <quoted free names>)
;; "" when the namespace is system or nothing was collected, so the seed mint
;; and any fn-free def emit byte-identically.
;;
;; Emitted under a *quote-pool*, which is what keeps this linear. The rows are in
;; innermost-first order (emit-fn conjes after emitting the body), so a nested
;; literal's construction is already interned by the time its enclosing literal is
;; assembled, and the enclosing one costs its own arity instead of its whole
;; subtree. Without it a chain of N nested literals emitted O(N^2) text — see the
;; pool comment at emit-quoted. The bindings come out in dependency order for the
;; same reason, so let* binds them in one pass.
;;
;; A row that throws leaves whatever it interned before throwing in the pool, so
;; the let* can carry a binding nothing references. Dead, valid, and confined to a
;; path that is already best-effort.
(defn- fnsrc-flush []
  (if (or (fnsrc-system-ns? *fnsrc-ns*) (empty? @*fnsrc-regs*))
    ""
    ;; best-effort: a macro can splice a LIVE value (a namespace, a var's
    ;; value) into a fn body, and emit-quoted has no rendering for those.
    ;; Such a literal just goes unregistered — its closure refuses at dump
    ;; like any other unregistered fn — rather than failing the whole
    ;; compilation of code that never dumps anything.
    (let [pool (atom {:by-expr {} :order []})
          ;; the rows in order, each emitted with every EARLIER row available to
          ;; stop the walk at (see *quote-shared*). Innermost first, so a nested
          ;; literal is always already there by the time its parent is emitted.
          ;; reduce and not map: each row's emission depends on the ones before it.
          out (binding [*quote-pool* pool]
                (reduce
                 (fn [acc row]
                   (let [nm (nth row 0) form (nth row 1) ns (nth row 2) frees (nth row 3)
                         q (try
                             (binding [*quote-shared* (:shared acc)]
                               (let [f (emit-quoted form)]
                                 [f (str "(image-register-fn-form! " (chez-str-lit nm) " "
                                         f " " (chez-str-lit ns) " "
                                         (emit-quoted frees) ")")]))
                             (catch Exception _ nil))]
                     (if (nil? q)
                       acc
                       (-> acc
                           (update :calls conj (nth q 1))
                           (update :shared conj [form (nth q 0)])))))
                 {:calls [] :shared []}
                 @*fnsrc-regs*))
          calls (:calls out)
          binds (:order @pool)]
      (cond
        ;; every row threw: nothing to register, and a let* with no body is not a
        ;; form at all
        (empty? calls) ""
        (empty? binds) (str " " (str/join " " calls))
        :else
        (str " (let* ("
             (str/join " " (map (fn [b] (str "(" (nth b 0) " " (nth b 1) ")")) binds))
             ") " (str/join " " calls) ")")))))

(defn- emit-fn [node]
  (let [;; a def's DIRECT anonymous init is named by its define, so it keeps the
        ;; bare lambda; *fnsrc-def-init?* is set only around that init's emission
        ;; and cleared for the body below, so a nested literal isn't mistaken for it.
        def-init? *fnsrc-def-init?*
        arities (:arities node)
        ;; a named fn binds its own name as a known-procedure local across ALL
        ;; arities, so self-calls emit directly rather than via jolt-invoke.
        self (when-let [nm (:name node)] (munge-name nm))
        ;; When *qualifying-ns* is set (the :def runtime-eval path), bind the
        ;; letrec under a qualified name (ns/name) so Chez reports a unique
        ;; per-var frame name. Nested/anonymous fns ignore it (self is nil).
        qname (when (and self *qualifying-ns*)
                (str (munge-name *qualifying-ns*) "/" self))
        ;; the unique name is allocated BEFORE the arity bodies emit, so an
        ;; enclosing literal numbers ahead of the literals nested inside it
        ;; (document order — jfn$ns$def$0 is the outermost)
        fnsrc-nm (when (and (nil? (:name node)) (not def-init?)
                            (not (fnsrc-system-ns? *fnsrc-ns*)) (:src-form node))
                   (fnsrc-name))
        clauses (binding [*known-procs* (if self (conj *known-procs* self) *known-procs*)
                          *trace-site* (or qname self)
                          *trace-self* (cond-> #{} self (conj self) qname (conj qname))
                          *fnsrc-def-init?* false]
                  (mapv emit-arity-clause arities))
        lambda (if (= 1 (count clauses))
                 (let [c (first clauses)] (str "(lambda " (nth c 0) " " (nth c 1) ")"))
                 (str "(case-lambda "
                      (str/join " " (map (fn [c] (str "(" (nth c 0) " " (nth c 1) ")")) clauses))
                      ")"))
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
        ;; enclosing (define jv$ns$name …) names it — so its def registers the
        ;; sibling call instead (emit-def-cached), and *variadic-reg-suppressed?*
        ;; tells this fn to leave it alone.
        variadic-fixed (some (fn [a] (when (:rest a) (count (:params a)))) arities)
        reg-here? (and variadic-fixed (not *variadic-reg-suppressed?*))
        ;; a bare anonymous variadic fn still needs a binding to register through;
        ;; nothing maps its frame, so a fresh label costs no backtrace fidelity.
        anon-label (when (and reg-here? (not self)) (fresh-label "fnvar"))]
    ;; A named fn references itself by name — the analyzer binds that name as a
    ;; :local in the body. letrec makes the name visible to the lambda.
    (if-let [nm (:name node)]
      (let [m (munge-name nm)]
        ;; The qualified binding is what Chez reports for the frame; the short alias
        ;; is what the body's self-calls emit (*known-procs* is keyed on it). Both
        ;; are letrec* bindings so the alias is in scope INSIDE the lambda — an
        ;; alias bound by an enclosing `let` is not, and a self-recursive body then
        ;; references an unbound name. letrec* also fixes the order: the alias is
        ;; initialised right after the lambda exists and long before any call.
        ;; the letrec BODY is where a variadic fn registers, so the binding still
        ;; holds the bare lambda and Chez keeps the frame name.
        (let [ret (if reg-here? (str "(jolt-register-variadic! " variadic-fixed " " m ")") m)]
          (if qname
            (str "(letrec* ((" qname " " lambda ") (" m " " qname ")) " ret ")")
            (str "(letrec ((" m " " lambda ")) " ret ")"))))
      (if (not fnsrc-nm)
        ;; system namespaces, a def's direct init, and ad-hoc fns (contagion
        ;; clones) keep the old emission byte-identical: a bare lambda named by
        ;; the enclosing define, or the fnvar label for an anon variadic (see
        ;; the comment above on why the lambda must sit in the binding position).
        (if anon-label
          (str "(let ((" anon-label " " lambda ")) "
               "(jolt-register-variadic! " variadic-fixed " " anon-label "))")
          lambda)
        ;; a non-system anon literal: bind it letrec under a globally unique name
        ;; so Chez reports that name through the inspector ((io 'code) 'name),
        ;; and register its source form + defining ns + free locals for the
        ;; image. The variadic registration stays in the letrec BODY so the
        ;; binding holds the bare lambda and the name survives (same constraint
        ;; as the named path above).
        (let [nm fnsrc-nm
              form (:src-form node)
              frees (:free-names node)]
          (swap! *fnsrc-regs* conj [nm form *fnsrc-ns* frees])
          (if variadic-fixed
            (str "(letrec ((" nm " " lambda ")) "
                 "(jolt-register-variadic! " variadic-fixed " " nm "))")
            (str "(letrec ((" nm " " lambda ")) " nm ")")))))))

;; If fnode is a clojure.core (or host) ref to a native-op primitive, return the
;; Scheme op string — only at an arity where the Scheme op and the jolt fn agree.
(defn- native-op [fnode nargs]
  (let [nm (case (:op fnode)
             :var (when (= "clojure.core" (:ns fnode)) (:name fnode))
             :host (:name fnode)
             nil)
        fixed (when nm (get (fixed-arity-ops nm) nargs))
        op (or fixed (when nm (native-ops nm)))
        arity-ok (when nm (op-arity nm))]
    (cond
      (nil? op) nil
      (and arity-ok (not (arity-ok nargs))) nil
      :else op)))

;; IFn dispatch for a LITERAL callee (Clojure's "value as fn"): a keyword looks
;; itself up in its arg; a map/set/vector literal looks up its arg.
(defn- ifn-kind [fnode]
  (case (:op fnode)
    :const (when (keyword? (:val fnode)) :keyword)
    (:map :set :vector) :coll
    nil))

;; Polymorphic inline-cache width. MUST match jolt-pic-n in host/chez/records.ss:
;; the emitted scan reads slots [0..2N) and the epoch slot at 2N+1 of the cache
;; vector jolt-pic-make allocates, so the two must agree.
(def ^:private pic-n 4)
(def ^:private pic-epoch-idx (+ (* 2 pic-n) 1))
;; the eq? scan over a PIC cache's N (desc . impl) pairs: each clause returns the
;; impl when its cached desc is eq? to d. Strung together with `or` so a hit short-
;; circuits; a full miss falls through to the install helper (the caller appends it).
;; v is always a length-(2N+2) jolt-pic-make vector (the cache cell starts #f and is
;; only ever set to one), and the indices are constants below its length, so the
;; vector type/bounds checks are redundant -- emit the per-site unsafe variant.
(defn- pic-scan-clauses [v d]
  (str/join " "
            (for [i (range pic-n)]
              (str "(and (eq? (" (unsafe-prefix) "vector-ref " v " " (* 2 i) ") " d ")"
                   " (" (unsafe-prefix) "vector-ref " v " " (+ (* 2 i) 1) "))"))))

;; A reference into the Clojure stdlib (clojure.*) with no impl on Chez yet.
(defn- stdlib-var? [n]
  (and (= :var (:op n)) (str/starts-with? (or (:ns n) "") "clojure.")))

;; Emit a :num-kind-tagged arithmetic call as a Chez flonum/fixnum op. inc/dec are
;; unary (fl +/- 1.0, fx1+/fx1-); the rest map through dbl-ops/lng-ops. Integer
;; literal operands and a :long var operand of a :double op were coerced to flonums
;; by jolt.passes.numeric (the literal as a compile-time flonum const, the :long var
;; via a :fl-coerce tag the emitter wraps in (fixnum->flonum ...)). A :double op is
;; only emitted once the numeric pass has PROVEN every operand is a flonum (that
;; proof gates the fl* specialization itself), so the flonum type check the safe fl
;; ops run is redundant there — emit the per-site unsafe #3% variant. There is no
;; matching unsafe variant for ^long +/-/*: a ^long is 64-bit and a Chez fixnum is
;; 61, so those map to the widening jolt-l+/-/* rather than any fx op.
(defn- emit-numeric [kind nm args order-args]
  (cond
    (and (= kind :double) (= nm "inc")) (str "(" (unsafe-prefix) "fl+ " (first args) " 1.0)")
    (and (= kind :double) (= nm "dec")) (str "(" (unsafe-prefix) "fl- " (first args) " 1.0)")
    ;; inc/dec tolerate a 64-bit operand (jolt-l-inc/dec fall back past fixnum range);
    ;; unchecked-inc/dec wrap (Java long). Neither can use the raising fx1+/fx1-.
    (and (= kind :long) (= nm "inc")) (str "(jolt-l-inc " (first args) ")")
    (and (= kind :long) (= nm "dec")) (str "(jolt-l-dec " (first args) ")")
    (and (= kind :long) (= nm "unchecked-inc")) (str "(jolt-uncinc " (first args) ")")
    (and (= kind :long) (= nm "unchecked-dec")) (str "(jolt-uncdec " (first args) ")")
    :else
    (let [op (case kind :double (dbl-ops nm) :long (lng-ops nm) :bigdec (bd-ops nm))
          op (if (= kind :double) (str (unsafe-prefix) op) op)]
      (cond
        (and (contains? #{"<" "<=" ">" ">=" "==" "="} nm) (> (count args) 2))
        ;; a chained comparison (<= a b c) means (and (<= a b) (<= b c)); the fast
        ;; binary op is 2-ary, so expand rather than pass 3+ args to it. order-args
        ;; binds each operand to a temp once, so reusing a temp across pairs is safe.
        (order-args (fn [as]
          (str "(and " (str/join " " (map (fn [pair] (str "(" op " " (first pair) " " (second pair) ")"))
                                          (partition 2 1 as))) ")")))
        ;; Every fast-path op is BINARY — jolt-l+ and friends are 2-arg macros (3
        ;; operands is a syntax error at expansion, not a runtime one) and the
        ;; unchecked ops are 2-arg procs — while +/-/*/min/max are variadic. Lower N
        ;; operands as a left fold of the binary op, which is also the reference
        ;; semantics: (+ a b c) is (+ (+ a b) c), so each step overflow-checks
        ;; separately rather than one check over the whole sum. Each operand still
        ;; appears exactly once and in source order, so this is safe whether or not
        ;; order-args bound them to temps.
        (> (count args) 2)
        (order-args (fn [as]
          (reduce (fn [acc a] (str "(" op " " acc " " a ")")) (first as) (rest as))))
        ;; The other end: ONE operand. (+ x)/(* x)/(min x)/(max x) ARE x, and a
        ;; binary op has no 1-operand form to splice it into. A specialized operand
        ;; is already coerced (^long -> fixnum, ^double -> flonum), so unlike the
        ;; generic jolt-add there is nothing left to type-check either. `-` and `/`
        ;; are NOT identities here — they negate and reciprocate — and every
        ;; fast-path op spells those out, so they keep their call.
        ;; unchecked-add/-multiply are identities the same way: jolt's overlay folds
        ;; them over any arity ((apply unchecked-add [x]) => x), so the inline path
        ;; has to agree with it — a 2-arg proc handed one operand is an arity error.
        (and (= 1 (count args))
             (contains? #{"+" "*" "min" "max" "unchecked-add" "unchecked-multiply"} nm))
        (first args)
        ;; unchecked-subtract at one operand negates, matching both `-` and jolt's
        ;; own overlay ((apply unchecked-subtract [x]) => -x). jolt-uncsub2 has no
        ;; unary form, so subtract from zero — which wraps identically.
        (and (= 1 (count args)) (= "unchecked-subtract" nm))
        (order-args (fn [as] (str "(" op " 0 " (first as) ")")))
        :else
        (order-args (fn [as] (str "(" op " " (str/join " " as) ")")))))))

;; slot of a declared field key in a record's field-order shape, or nil.
(defn- struct-field-index [shape kw]
  (when shape
    (loop [i 0]
      (cond (>= i (count shape)) nil
            (= (nth shape i) kw) i
            :else (recur (inc i))))))
;; the direct per-arity slot accessor (jrecN-fI) for a field at idx in `shape`, or
;; nil when there is no per-arity accessor (>8 fields spill into jrec*, which stores
;; fields in a vector, not inline slots). Sound only for a proven-non-nil receiver.
(defn- direct-field-accessor [shape idx]
  (let [n (count shape)]
    (when (<= n 8)
      (str "jrec" n "-f" idx))))

;; A plain Scheme application: (callee op ...).
(defn- plain-call [callee operand-strs]
  (str "(" callee (if (seq operand-strs) (str " " (str/join " " operand-strs)) "") ")"))
;; A tail call in trace mode (R4, bead jolt-hm1p): ONE virtual-register store
;; of the static ('ns/fn' . line) site pair, after the operands are temp-bound
;; — an operand's own tail site would otherwise stomp the slot before the call
;; runs. The call stays the last form, so TCO is preserved. This is the WHOLE
;; runtime instrumentation; erased chains are reconstructed at report time
;; from the callsite tables. (The R1-R3 continuation-marks design recorded
;; chains at runtime and ballooned the heap on delegation-heavy code — every
;; mark op allocated; see rt.ss.) Self-tail calls never reach here (emit-call
;; elides them), so a tight self-loop stores once, not per iteration.
(defn- sited-tail-call [site-qname line callee operand-strs]
  (let [tts (mapv (fn [_] (fresh-label "_tt$")) operand-strs)
        binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tts operand-strs))
        site (str "'(" (chez-str-lit site-qname) " . " line ")")]
    (if (seq binds)
      (str "(let* (" binds ") (jolt-site! " site ") " (plain-call callee tts) ")")
      (str "(begin (jolt-site! " site ") " (plain-call callee operand-strs) ")"))))
;; Emit a call. In tail position with tracing on, a call to a DIFFERENT fn than
;; the enclosing one stores its site pair (sited-tail-call). Everything else —
;; non-tail calls, JOLT_TRACE=0, direct self-tail-calls — is a plain
;; application, byte-identical to untraced code.
(defn- emit-call [tail? callee operand-strs line]
  (if (and (trace-frames?) tail? *trace-site*
           (not (contains? *trace-self* callee)))
    (sited-tail-call *trace-site* line callee operand-strs)
    (plain-call callee operand-strs)))

(defn- emit-invoke [node]
  (let [tail? *tail?*]           ; capture: children below emit non-tail
   (binding [*tail?* false]
    (let [fnode (:fn node)
        arg-nodes (:args node)
        args (mapv emit arg-nodes)
        tl (or (node-line node) 0)
        ;; R2: record this site's static callee for the callsite table. Runs after
        ;; the args are emitted, so when a line carries both a call and its
        ;; operand's call the OUTER (later-emitted) callee wins — the tail call's.
        _ (when tl (register-callsite! tl (static-callee fnode) tail?))
        nop (native-op fnode (count args))
        kind (ifn-kind fnode)
        ;; order args left-to-right (build receives the spliced operand strings)
        order-args (fn [build] (ordered-call arg-nodes args build))
        defstr (fn [as] (if (> (count as) 1) (str " " (nth as 1)) ""))
        ;; jolt-invoke dispatch: Clojure evaluates the fn expr before the args, so
        ;; order [callee & args] together when ordering is observable. Pick a
        ;; fixed-arity entry point (jolt-invoke0..4) by arg count so the common
        ;; raw-procedure fast path allocates no rest-list; keep variadic jolt-invoke
        ;; for larger arities / apply tails.
        invoke (fn []
                 (let [callee (if (<= (count args) 4)
                                (str "jolt-invoke" (count args))
                                "jolt-invoke")]
                   (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                                 (fn [operands] (emit-call tail? callee operands tl)))))]
    (cond
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
      (order-args (fn [as]
                     (let [r (fresh-label "_r$")
                          ;; a site whose impl has a contagion clone resolves the clone
                          ;; (fl* + exact->inexact on the :num operand) instead of the
                          ;; shared impl; otherwise the ordinary devirt-resolve. The
                          ;; non-specialized path is byte-identical (clone-sites empty
                          ;; outside a whole-program build).
                          resolver-name (if (clone-site? (:devirt-type node) (:devirt-proto node) (:devirt-method node))
                                           "devirt-resolve-fl" "devirt-resolve")
                          dv (str "(" resolver-name " " (chez-str-lit (:devirt-type node)) " "
                                  (chez-str-lit (:devirt-proto node)) " " (chez-str-lit (:devirt-method node))
                                  " " r ")")
                          cells *cache-cells*
                          ;; cache the resolved impl in a per-site cell when inside a
                          ;; def; else resolve per call. The cell carries (epoch . fn):
                          ;; each call compares its epoch against jolt-proto-epoch and
                          ;; re-resolves on mismatch, so an extend-type after warmup
                          ;; (a register-protocol-method epoch bump) invalidates this
                          ;; site like every other dispatch path, mirroring the PIC.
                          resolver (if cells
                                      (let [c (fresh-label "_dvc$")]
                                        (swap! cells conj c)
                                        (str "(if (and (pair? " c ") (fx= (car " c ") jolt-proto-epoch))"
                                             " (cdr " c ")"
                                             " (let ((_f " dv ")) (set! " c " (cons jolt-proto-epoch _f)) _f))"))
                                      dv)]
                      (str "(let* ((" r " " (first as) ")) ("
                           resolver " " (str/join " " (cons r (rest as))) "))"))))
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
      (order-args (fn [as]
                    (let [r (fresh-label "_r$")
                          d (fresh-label "_d$")
                          v (fresh-label "_v$")
                          cells *cache-cells*
                          proto (chez-str-lit (:proto node))
                          method (chez-str-lit (:method node))
                          apply-args (str/join " " (cons r (rest as)))]
                      (if cells
                        (let [c (fresh-label "_picv$")
                              scan (pic-scan-clauses v d)]
                          (swap! cells conj c)
                          ;; hot path inlined: bind the receiver, the cache vector
                          ;; (lazily allocated on first call), and its desc; then, if
                          ;; the epoch still matches, eq?-scan the cached descs and
                          ;; apply the hit impl directly — no helper call after warmup.
                          ;; A miss (no cached desc / stale epoch) resolves + (re)fills
                          ;; via the jolt-pic-install/-rebuild helpers.
                          (str "(let* ((" r " " (first as) ")"
                               " (" v " (or " c " (let ((_nv (jolt-pic-make))) (set! " c " _nv) _nv)))"
                               " (" d " (jrec-pic-desc " r ")))"
                               " ((if (and " d " (" (unsafe-prefix) "fx= (" (unsafe-prefix) "vector-ref " v " " pic-epoch-idx ") jolt-proto-epoch))"
                                " (or " scan " (jolt-pic-install " v " " d " " proto " " method " " r "))"
                                " (jolt-pic-rebuild " v " " d " " proto " " method " " r "))"
                                " " apply-args "))"))
                        (str "(let* ((" r " " (first as) "))"
                             " ((protocol-resolve " proto " " method " " r ") " apply-args "))")))))
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
      (:fl-aget node)
      (order-args
       (fn [as]
         (if (:fl-idx-long node)
           (str "(flvector-ref (jolt-array-vec " (first as) ") " (second as) ")")
           (str "(jolt-flaget " (str/join " " as) ")"))))
      ;; (aset ^doubles a i v): proven index AND :double value (:fl-idx-long +
      ;; :fl-val-double) store inline — (let ((v V)) (flvector-set! (jolt-array-vec A)
      ;; I v) v) — and return the stored value (JVM contract; the let evaluates V once).
      ;; Otherwise keep (jolt-flaset A I V), which owns exact->inexact for a non-double.
      (:fl-aset node)
      (order-args
       (fn [as]
         (if (and (:fl-idx-long node) (:fl-val-double node))
           (let [v (fresh-label "_v$")]
             (str "(let ((" v " " (nth as 2) ")) (flvector-set! (jolt-array-vec "
                  (first as) ") " (second as) " " v ") " v ")"))
           (str "(jolt-flaset " (str/join " " as) ")"))))
      (:fl-op node) (order-args (fn [as] (str "(" (:fl-op node) " " (str/join " " as) ")")))
      ;; hint-directed fast arithmetic: jolt.passes.numeric proved every operand a
      ;; flonum (^double) or fixnum (^long), so emit the Chez fl*/fx* op.
      (:num-kind node) (emit-numeric (:num-kind node) (:name fnode) args order-args)
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
          :else (order-args (fn [as] (str "(jolt-get " (str/join " " as) ")")))))
      ;; a generic native op.
      nop (order-args (fn [as] (emit-call tail? nop as tl)))
      ;; (:k coll [default]) -> (jolt-get coll :k [default]) — the key (fnode) is a
      ;; const, so only the coll/default args carry order. When the inference typed
      ;; the receiver as a record whose declared fields include :k (it carries the
      ;; field-order :shape), read the field by its static slot — no field-key
      ;; lookup, no jolt-get dispatch. A proven-non-nil receiver emits the direct
      ;; per-arity accessor (jrecN-fI); a nilable one keeps jrec-field-at. Only the
      ;; no-default form (a declared field is always present, so a default is never
      ;; taken).
      (= kind :keyword)
      (let [recv (first arg-nodes)
            idx (when (and (= :struct (:hint recv)) (= 1 (count arg-nodes)))
                  (struct-field-index (:shape recv) (:val fnode)))
            dir (when (and idx (not (:nilable recv)))
                  (direct-field-accessor (:shape recv) idx))]
        (cond
          dir  (order-args (fn [as] (str "(" dir " " (first as) ")")))
          idx  (order-args (fn [as] (str "(jrec-field-at " (first as) " " idx " " (emit fnode) ")")))
          :else (order-args (fn [as] (str "(jolt-get " (first as) " " (emit fnode) (defstr as) ")")))))
      ;; (coll k [default]) -> lookup — coll (fnode) is the callee, evaluated
      ;; before the key/default args. A VECTOR literal invokes as nth (a bad
      ;; index throws, IPersistentVector.invoke); maps/sets invoke as get.
      (= kind :coll)
      (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                    (fn [[c & as]]
                      (str (if (and (= :vector (:op fnode)) (= 1 (count as)))
                             "(jolt-nth "
                             "(jolt-get ")
                           c " " (str/join " " as) ")")))
      (and (stdlib-var? fnode) (not (prelude-mode?)))
      (throw (ex-info (str "emit: unsupported stdlib fn `" (:ns fnode) "/" (:name fnode)
                           "` (no core on Chez yet)") {}))
      ;; static method call (Class/method arg*) -> (host-static-call ...).
      (= :host-static (:op fnode))
      (order-args (fn [as]
                    (str "(host-static-call " (chez-str-lit (:class fnode)) " " (chez-str-lit (:member fnode))
                         (if (empty? as) "" (str " " (str/join " " as))) ")")))
      (= :host (:op fnode))
      (throw (ex-info (str "emit: unsupported host call `" (:name fnode) "`") {}))
      ;; a :local callee: a known procedure (the letrec-bound self-name of a named
      ;; fn — i.e. self-recursion) is a real Scheme proc, so call it directly with
      ;; no jolt-invoke / arg consing; case-lambda handles arity. Any other local
      ;; holds an arbitrary IFn -> dynamic dispatch.
      (= :local (:op fnode))
      (if (*known-procs* (munge-name (:name fnode)))
        (order-args (fn [as] (emit-call tail? (munge-name (:name fnode)) as tl)))
        (invoke))
      ;; closed-world direct call: the callee var is an app fn def already emitted
      ;; with a Scheme binding — apply it directly, no var lookup, no jolt-invoke.
      ;; Only fn-valued defs qualify; a non-fn invokable value (a map/set/keyword
      ;; held in a var) isn't a Scheme procedure, so it falls through to jolt-invoke
      ;; below (which still uses the direct binding as the invoke target).
      (and (= :var (:op fnode)) (direct-linkable? (:ns fnode) (:name fnode))
           (direct-link-fn? (:ns fnode) (:name fnode)))
      (order-args (fn [as] (emit-call tail? (dl-name (:ns fnode) (:name fnode)) as tl)))
       ;; record ctor with matching arity: inline the native per-arity ctor
       ;; (make-jrecN) directly — desc + ext + one inline slot per field —
       ;; eliminating jolt-invoke / var-deref / rest-list / ctor call / hashtable
       ;; lookup AND the field vector. After per-site desc-cell warmup the hot
       ;; path is: cell read -> make-jrecN — one allocation, no lookups, no
       ;; dispatch. The shape comes from the unit's record-shapes (set-record-shapes!).
       (let [key (str (:ns fnode) "/" (:name fnode))
             shape (get (ctor-shapes) key)]
         (and (= :var (:op fnode)) shape
              (= (count (get shape :fields)) (count args))
              (<= (count args) 6)
              ;; skip if any ^double field — the inlined path doesn't coerce
              (not-any? #{"double"} (get shape :tags))))
       (let [s (get (ctor-shapes) (str (:ns fnode) "/" (:name fnode)))
             tag (:type s)
             cells *cache-cells*
             desc-lookup (str "(hashtable-ref chez-tag-desc " (chez-str-lit tag) " #f)")
             cached-desc (if cells
                           (let [c (fresh-label "_cdesc$")]
                             (swap! cells conj c)
                             (str "(or " c " (let ((_d " desc-lookup ")) (set! " c " _d) _d))"))
                           desc-lookup)]
         (order-args (fn [as]
                       (let [n (count as)]
                         (if (<= n 8)
                           (str "(make-jrec" n " " cached-desc " jolt-nil"
                                (when (pos? n) (str " " (str/join " " as))) ")")
                           (str "(let ((v (vector " (str/join " " as) "))) (make-jrec " cached-desc " v jolt-nil))"))))))
      ;; a late-bound :var call head can hold a procedure OR a non-applicable
      ;; value the RT dispatches (multimethod, keyword/coll IFn) — route via
      ;; jolt-invoke (transparent for a procedure).
      (= :var (:op fnode))
      (invoke)
      ;; a computed callee can yield ANY IFn — route through jolt-invoke.
      :else
      (invoke))))))

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
(defn- emit-try [node]
  (let [core (if-let [cs (:catch-sym node)]
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
                               "(let ((r " (emit (:catch-body node)) ")) "
                               (if cl (str "(jolt-catch-leave! " cl ") ") "")
                               "(jolt-catch-complete!) r)"
                               (if cl ")" "")
                               "))) "
                               (emit (:body node)) ")")]
                 body)
               (emit (:body node)))]
    (if-let [fin (:finally node)]
      (str "(dynamic-wind jolt-finally-in (lambda () " core ")"
           " (lambda () " (emit fin) "))")
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

(defn emit* [node]
  (case (:op node)
    :const (emit-const (:val node))
    :local (munge-name (:name node))
    ;; late-bound var: read the cell's current root at use time. A value-position
    ;; ref to a clojure.core fn the RT provides lowers to the RT procedure.
    :var (let [core-proc (and (= "clojure.core" (:ns node)) (core-value-procs (:name node)))]
           (cond
             core-proc core-proc
             ;; direct-linked app var used as a value -> reference its binding (same
             ;; root as the var cell for a final var; helps DCE keep it live).
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
             (let [cells *cache-cells*
                   nslit (chez-str-lit (:ns node)) nmlit (chez-str-lit (:name node))]
               (if (and (var-cache?) cells)
                 (let [c (fresh-label "_vc$")]
                   (swap! cells conj c)
                   (str "(var-cell-deref (or " c " (let ((_v (jolt-var " nslit " " nmlit "))) (set! " c " _v) _v)))"))
                 (str "(var-deref " nslit " " nmlit ")")))))
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
                   (if (:meta-expr node) (emit (:meta-expr node)) (emit-quoted (:meta node)))
                   ") (mark-macro! " (chez-str-lit (:ns node)) " "
                   (chez-str-lit (:name node)) ") jolt-nil)")
    :host (throw (ex-info (str "emit: unsupported host ref `" (:name node) "`") {}))
    :host-static (str "(host-static-ref " (chez-str-lit (:class node)) " "
                      (chez-str-lit (:member node)) ")")
    :host-new (str "(host-new " (chez-str-lit (:class node))
                   (let [args (map emit (:args node))]
                     (if (empty? args) "" (str " " (str/join " " args)))) ")")
    ;; the test is non-tail; then/else inherit the if's tail position
    :if (let [test (:test node)
              t (binding [*tail?* false]
                  (if (returns-scheme-bool? test) (emit test)
                      (str "(jolt-truthy? " (emit test) ")")))]
          (str "(if " t " " (emit (:then node)) " " (emit (:else node)) ")"))
    ;; non-last statements are non-tail; the ret inherits the do's tail position
    :do (str "(begin " (binding [*tail?* false] (str/join " " (mapv emit (:statements node))))
             (if (empty? (:statements node)) "" " ") (emit (:ret node)) ")")
    :invoke (emit-invoke-maybe-clone node)
     ;; collection literals -> rt constructors (collections.ss). Elements are
     ;; already-analyzed IR nodes; evaluate LEFT-TO-RIGHT (emit-ordered, which
     ;; wraps only when two or more operands could have observable effects).
     :vector (emit-ordered "jolt-vector" (:items node))
     :set (emit-ordered "jolt-hash-set" (:items node))
     :map (emit-ordered "jolt-hash-map"
                       (mapcat (fn [p] [(nth p 0) (nth p 1)]) (:pairs node)))
    :quote (emit-quoted (:form node))
    ;; the thrown value is an operand (emitted non-tail); the throw itself goes
    ;; through emit-call with marks?=#f, so a TAIL throw gets the site-vreg pair
    ;; (sited-tail-call — stored after the operand is bound, so the operand's own
    ;; fixnum store cannot stomp it) and still names its TCO-erased frame. A
    ;; non-tail throw needs neither: the continuation names the frame, and the
    ;; operand's own with-site store carries the line. The analyzer stamps :pos
    ;; on call forms, not on throw special forms, so the site line falls back to
    ;; the thrown expression's — (throw (ex-info …)) sits on one line.
    :throw (let [line (or (node-line node) (node-line (:expr node)))
                 e (binding [*tail?* false] (emit (:expr node)))
                 call (emit-call *tail?* "jolt-throw" [e] (or line 0))]
             (if (and (trace-frames?) line)
               (str "#|L" line "|# " call)
               call))
     ;; numeric coercion. A :cast-fn (from a user (double x)/(long x)/… cast)
     ;; emits the checked runtime helper — clojure.core's full JVM semantics —
     ;; NOT the hint coercion (jolt->fl / jolt->fx) a proven typed-param cast uses.
     ;; The 2-arg :coerce (inlined ^double/^long param or return) has no :cast-fn
     ;; and keeps the hint coercion.
     :coerce (let [e (emit (:expr node))]
               (cond (:cast-fn node) (str "(" (:cast-fn node) " " e ")")
                     (= :double (:kind node)) (str "(jolt->fl " e ")")
                     (= :long (:kind node)) (str "(jolt->fx " e ")")
                     :else e))
    :try (emit-try node)
    ;; regex literal #"…" -> a jolt-regex value (regex.ss, vendored irregex).
    :regex (str "(jolt-regex " (chez-str-lit (:source node)) ")")
    ;; #inst / #uuid literals -> runtime inst / uuid values.
    :inst (str "(jolt-inst-from-string " (chez-str-lit (:source node)) ")")
    :uuid (str "(jolt-uuid-from-string " (chez-str-lit (:source node)) ")")
    ;; bigdecimal literal (1.5M) -> a runtime jbigdec from its numeric text.
    :bigdec (str "(jolt-bigdec-from-string " (chez-str-lit (:source node)) ")")
    ;; a namespace value spliced into a form (~*ns*) -> reconstruct by name.
    :the-ns (str "(intern-ns! " (chez-str-lit (:name node)) ")")
    ;; (.method target arg*) -> jolt-host-call for an rt-shimmed method, else
    ;; record-method-dispatch (a reify/record protocol method).
    :host-call (let [m (:method node)
                     target (emit (:target node))
                     args (map emit (:args node))]
                 (if (supported-host-methods m)
                   (str "(jolt-host-call " (chez-str-lit m) " " target
                        (if (empty? args) "" (str " " (str/join " " args))) ")")
                   (str "(record-method-dispatch " target " " (chez-str-lit m)
                        " (jolt-vector" (if (empty? args) "" (str " " (str/join " " args))) "))")))
    :let (emit-let node)
    :loop (emit-loop node)
    :recur (emit-recur node)
    :ffi-fn (emit-ffi-fn node)
    :ffi-callable (emit-ffi-callable node)
    :fn (emit-fn node)
    ;; (def name) with no init (declare): reserve the cell. A def with non-empty
    ;; reader metadata lowers to def-var-with-meta! (ported in a later increment).
    ;; Qualify only when this def will actually register a source map — that is the
    ;; whole point of the unique name, and it ties qualification to the runtime-eval
    ;; path. The seed mint and `jolt build` emit with source-reg off, so core keeps
    ;; its short names and prelude.ss stays byte-identical across a re-mint.
    ;; *callsites* is bound HERE, not in register-callsite! — a dynamic var
    ;; must hold the atom before the init emission runs or every site record
    ;; silently no-ops and the callsite table stays empty.
    :def (binding [*qualifying-ns* (when (source-reg?) (:ns node))
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
                     (str "(def-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
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
               (str pre body post))))
    (throw (ex-info (str "emit: op not yet ported / unhandled: " (pr-str (:op node))) {}))))

;; ^:dynamic / ^:redef on a def opts it out of direct-linking: it stays redefinable,
;; so callers must go through the var cell. m is a def's :meta (a jolt map value).
(defn- dl-opt-out? [m] (or (get m :dynamic) (get m :redef)))

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
        ;; A fn def gets a source-registry entry so a native backtrace can map its
        ;; frame to ns/name (file:line). Chez names the frame by whatever emit-fn
        ;; binds the lambda to: a NAMED fn (defn, or (fn foo …)) gets a letrec
        ;; self-binding = munge-name of the fn's own name; an ANONYMOUS fn def has
        ;; no letrec, so the lambda sits directly under (define jv$ns$name …) and
        ;; takes that name. Register under whichever Chez will report.
        pos (:pos node)
        frame-name (when fn? (if-let [fnm (:name (:init node))] (munge-name fnm) b))
        reg (when (and dl? fn? pos)
              (str " (jolt-register-source! " (chez-str-lit frame-name) " "
                   (chez-str-lit ns) " " (chez-str-lit nm) " "
                   (if (get pos :file) (chez-str-lit (get pos :file)) "jolt-nil") " "
                   (or (get pos :line) 0) ")"))
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
        ;; *callsites* is bound around the init emission and the registration
        ;; sibling read while still bound — this is the BUILD path's def emit,
        ;; and without it a built binary had empty callsite tables: the
        ;; reporter's backwalk dead-ended after the throw-time pair and a
        ;; built trace showed the erased fn but never its erased callers.
        ;; The fnsrc context rides the same binding: the init's anon literals
        ;; collect into *fnsrc-regs* and flush before the binding returns.
        init+creg (binding [*variadic-reg-suppressed?* (boolean anon-variadic)
                            *callsites* (when (trace-frames?) (atom {}))
                            *fnsrc-ns* ns
                            *fnsrc-def* nm
                            *fnsrc-counter* (atom 0)
                            *fnsrc-regs* (atom [])
                            *fnsrc-def-init?* fn?]
                    (let [i (emit-with-cells #(emit (:init node)))]
                      [i (trace-callsite-reg) (fnsrc-flush)]))
        init (nth init+creg 0)
        creg (nth init+creg 1)
        freg (nth init+creg 2)]
    ;; fnsrc registrations run BEFORE the define/def-var!: static data, and the
    ;; init (or a form evaluated right after in the same top-level do) may dump a
    ;; closure the init just created.
    (cond
      dl?
      (if (jmeta-nonempty? (:meta node))
        (str "(begin" freg " (define " b " " init ") (def-var-with-meta! "
             (chez-str-lit ns) " " (chez-str-lit nm) " " b " " (emit-def-meta node) ")"
             (or reg "") (or vreg "") creg ")")
        (str "(begin" freg " (define " b " " init ") (def-var! "
             (chez-str-lit ns) " " (chez-str-lit nm) " " b ")" (or reg "") (or vreg "") creg ")"))
      (jmeta-nonempty? (:meta node))
      (if (= (str creg freg) "")
        (str "(def-var-with-meta! " (chez-str-lit ns) " " (chez-str-lit nm) " " init " " (emit-def-meta node) ")")
        ;; a def evaluates to its var — register first, bind, hand the var back
        (let [v (fresh-label "_dv$")]
          (str "(begin" freg " (let ((" v " (def-var-with-meta! " (chez-str-lit ns) " " (chez-str-lit nm) " " init " " (emit-def-meta node) ")))" creg " " v "))")))
      :else
      (if (= (str creg freg) "")
        (str "(def-var! " (chez-str-lit ns) " " (chez-str-lit nm) " " init ")")
        (let [v (fresh-label "_dv$")]
          (str "(begin" freg " (let ((" v " (def-var! " (chez-str-lit ns) " " (chez-str-lit nm) " " init ")))" creg " " v "))"))))))

(defn emit-top-form [node]
  (binding [*fnsrc-ns* (or (:ns node) (:fnsrc-ns node))
            *fnsrc-def* (when (= :def (:op node)) (:name node))
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
      (if (= freg "") scm
          ;; registrations run BEFORE the form: they are static data with no
          ;; dependency on the form's evaluation, and the form itself may dump a
          ;; closure it just created — the registration must already be there.
          ;; begin keeps the form's value as the result.
          (str "(begin" freg " " scm ")")))))
