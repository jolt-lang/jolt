(ns jolt.backend-scheme.session
  "The emit session the Scheme back end reads as it lowers a form: the
  compilation unit's mode flags — prelude, direct-link, seed mint, var cache,
  target, tracing, source registration, debug locals — the record shapes and
  clone sites published on it, the gensym counter, the direct-link registries,
  and the thread-bound dynamic context an emit establishes for its children:
  tail position, recur target, known procedures, trace sites, callsite
  tables, debug-local scopes. The setters the drivers call by name stay in
  jolt.backend-scheme."
  (:require [clojure.string :as str]
            [jolt.host
             :refer [inline-enabled? var-dynamic? var-root-callable? var-linked?]]
            [jolt.passes.types :as types]
            [jolt.backend-scheme.names :refer [chez-str-lit dl-fqn munge-name]]))

;; The current compilation-unit context (jolt.passes.types unit). ALL emit-session
;; state — the mode flags, the direct-link name registries, the record-ctor shape
;; registry, the gensym counter and the per-site cache cells — lives on it, read
;; here because the emitter threads no unit. The pointer lives in jolt.op-registry
;; (the leaf) so the passes reach the same unit — jolt.passes.inline can't require
;; the back end (a cycle). A driver publishes the unit (set-emit-unit!); `cur` lazily
;; installs a fresh one so a bare emit never reads nil. `current-unit` is the public
;; entry run-passes uses to share this one unit as its inference context too.
(defn cur [] (or @jolt.op-registry/current-unit-box
                  (let [u (types/new-unit)] (reset! jolt.op-registry/current-unit-box u) u)))

;; PRELUDE MODE. The default (subset) mode rejects any clojure.core ref
;; that isn't a native-op — a clean "out of subset" signal for user-facing `-e`.
;; When emitting clojure.core ITSELF as a prelude, core fns reference each other
;; constantly; those lower to var-deref (resolved at runtime).
(defn prelude-mode? [] @(:prelude-mode? (cur)))

;; DIRECT-LINK MODE. Off for ordinary runs, the seed mint, and `-e`/repl/load-string
;; (open world — vars are redefinable). `jolt build` (release/optimized) flips it on
;; during app emission: a closed-world program whose set of defs is final, so an
;; app->app call binds to the def's Scheme binding directly, skipping the var-table
;; lookup and the generic jolt-invoke dispatch. Final in SHAPE, not in value — the
;; def is emitted linked, so a root write still reaches the binding (see
;; emit-def-cached); what the closed world freezes is an inlined body.
;; ...or the runtime compile spine is compiling one form under
;; *compiler-options* {:direct-linking true} (compile-eval.ss
;; jolt-direct-link-emit). That is a per-thread host flag, and the host's
;; inline-enabled? answers it: inlining follows linkage, so the two are the same
;; question there, and the unit flag stays what a build or the seed mint set.
(defn direct-link? [] (or @(:direct-link? (cur)) (inline-enabled? nil)))

;; SEED-MINT MODE. bootstrap.ss mints clojure.core and the compiler with
;; direct-link ON — a core->core call applies the callee's jv$ binding, one
;; top-level load in place of var-cell-deref + jolt-invokeN — and this flag says
;; the emission is the SEED, which differs from a `jolt build` in that the
;; seed-callable arm of emit-invoke is off, because the seed vars that arm would
;; hoist are the ones being emitted. (Both bind a top-level def with
;; def-var-linked! (rt.ss), which keeps the var's root and the jv$ binding one
;; value under redefinition.) Nothing is spliced: the inline pass reads the host
;; contract's direct-link flag, which the mint leaves off, so a minted core is
;; direct-called and still redefinable, as JVM Clojure's direct-linked core is
;; not.
(defn seed-mint? [] @(:seed-mint? (cur)))

;; Cache a resolved var cell in a per-site cell so a non-direct-linked var
;; reference skips the name lookup (string-append + hash) after the first use.
;; OFF during the seed mint (the seed must stay a byte-fixpoint, and caching the
;; compiler's own refs shifts the gensym-numbered cell names every pass); the
;; runtime eval path turns it on for user code, where it's the big win.
(defn var-cache? [] @(:var-cache? (cur)))

;; TARGET PRIMITIVES. The back end emits a small set of Chez-only spellings
;; directly — the #3% unsafe variants of runtime ops (vector-ref, fx=, the
;; flonum arithmetic ops) that the PIC inline-cache probe and the proven-
;; :double/:long emit sites rely on. A target port swaps THIS table, never the
;; emitter: each key is a MEANING, and all seven emission sites derive their
;; spelling from :unsafe-prefix + the safe op name (one mechanism, no per-site
;; strings). A target without unsafe variants maps :unsafe-prefix to "" — every
;; derived spelling degrades to the checked op, safe-and-slower, never wrong.
(defn target [] @(:target (cur)))
(def ^:private target-prims
  {:chez {:unsafe-prefix "#3%"}})
(defn unsafe-prefix [] (get-in target-prims [(target) :unsafe-prefix]))

;; contagion clone sites (jolt devirt-gated fl* contagion for :num fields): a set of
;; "tag|proto|method" keys for impls whose body has a :num field beside a proven
;; :double operand — worth a contagion-specialized clone at the devirt site. It lives
;; on the compilation unit (:clone-sites), populated by a whole-program pre-pass
;; (contagion-prepass!) so the devirt-site resolution can consult it regardless of emit
;; order; nil in non-WP builds. The emit-time reader reaches it through the emit-unit
;; pointer (emit threads no unit) — nil off the optimize path, so clone-site? is false
;; and the non-specialized path stays byte-identical.
(defn clone-site-key [tag proto method] (str tag "|" proto "|" method))
(defn clone-site? [tag proto method]
  (when-some [u @jolt.op-registry/current-unit-box]
    (let [cs @(:clone-sites u)] (and cs (contains? cs (clone-site-key tag proto method))))))

;; Record-ctor shapes ("ns/->Name" -> {:fields (:k ..) :type tag}): the direct
;; fixed-arity ctor call emit-invoke recognizes reads the SAME record shapes the
;; inference installed on the unit (set-record-shapes!) — one registry, not a
;; separate backend copy. run-passes shares one unit for inference and emit, so it
;; is populated before each form's emit.
(defn ctor-shapes [] (get @(:config (cur)) :record-shapes))
;; the shape of the record whose class tag is CLASS ("ns.Name"), or nil — what a
;; (Name. …) constructor call names, where (->Name …) names the shape's key
(defn record-shape-for-class [class]
  (when (string? class)
    (some (fn [e] (let [s (val e)] (when (= class (get s :type)) s))) (ctor-shapes))))

;; Tail-frame tracing (JOLT_TRACE): a TAIL CALL SITE stores its static
;; ('ns/fn' . line) pair in the site vreg — one store, no allocation, nothing
;; in a fn prologue (R4, bead jolt-hm1p) — and every call site registers its
;; static callee in the callsite tables at def load, from which the reporter
;; reconstructs TCO-erased chains. OFF during the seed mint (byte-determinism);
;; compile-eval.ss turns it on for runtime-eval'd user code and build.ss for
;; `jolt build` output, both honoring JOLT_TRACE=0.
(defn trace-frames? [] @(:trace-frames? (cur)))

;; The line an :invoke node sits on, or nil. The analyzer stamps :pos on every call
;; form the reader gave a position to; a macro-built form carries none.
(defn node-line [node]
  (let [p (:pos node)] (when (map? p) (:line p))))

;; Under tracing, emit an inline Chez block comment #|L<line>|# right before every
;; call: a frame's source object gives a byte offset into the generated .scm, and
;; scanning back for the nearest marker recovers the ORIGINAL clj line
;; (source-registry.ss jolt-marker-line-at-offset). Must be a block comment — a
;; `;;` comment would comment out the rest of the one-line top-level form. R2
;; (bead jolt-knn8): the per-call (jolt-site! LINE) fixnum store is GONE — non-tail
;; code now compiles with no per-call instrumentation at all, so a continuation
;; names a non-tail frame with its exact line for free (a non-tail call now only
;; puts the site slot back when it returns, emit-call). The ('ns/fn' . line) PAIR
;; the innermost-frame splice needs is stored only by TAIL call emissions
;; (sited-tail-call / the :throw case), there AFTER the operands are temp-bound.
;; A pair now survives its chain's return, so the reporter validates the splice
;; against the compile-time callsite table (*callsites* / jolt-register-callsite!)
;; before accepting it (source-registry.ss jolt-site-splice?).
;; A node the inline pass copied out of another fn carries :inline-chain — the
;; logical frames between it and the physical fn it ended up in, innermost first
;; (jolt.passes.inline stamp-inline). The marker carries the whole chain, so the
;; reporter can name a spliced site after the fn it came from and still show every
;; call site above it: a spliced callee has no procedure at runtime, so without
;; this its frame is named after whatever it was spliced into and located at a
;; line that fn does not contain.
(defn marker-safe-fqn? [q]
  (and (string? q)
       (not (or (str/includes? q "|") (str/includes? q "#") (str/includes? q "@")))))

;; "@<fqn>@<line>" per chain entry, innermost first — or nil when the chain is
;; absent or holds an fqn with one of the marker's own delimiters in it. A var
;; name may contain almost anything, and a `|`, `#` or `@` would either close the
;; block comment early (breaking the emitted Scheme) or split a field, so such a
;; site emits the plain marker: unattributed, exactly as before, never malformed.
(defn- inline-chain-suffix [node]
  (let [ch (get node :inline-chain)]
    (when (and (seq ch) (every? (fn [e] (marker-safe-fqn? (nth e 0))) ch))
      (apply str (map (fn [e] (str "@" (nth e 0) "@" (nth e 1))) ch)))))

(defn with-site [node s]
  (if-let [l (and (trace-frames?) (node-line node))]
    (str "#|L" l (or (inline-chain-suffix node) "") "|# " s)
    s))

;; Source-map registration for a fn def: one hashtable insert at definition time,
;; no per-call cost, so a backtrace can name a frame ns/name (file:line) instead of
;; falling back to its bare Chez procedure name. On for runtime eval
;; (compile-eval.ss) — a `jolt run` trace reads the same as an AOT build's. OFF for
;; the seed mint (emit-image.ss): the record carries the def's absolute path, and
;; baking this machine's paths into prelude.ss would break the byte-fixpoint
;; everywhere else.
(defn source-reg? [] @(:source-reg? (cur)))
;; Debug locals (jolt.profile/instrument! :debug, #1464). Code compiled while it
;; is on keeps every fn's params and let-locals visible to its frame, so
;; jolt.debug/frames can read them off a throwable's captured continuation. Chez
;; 10.4 has no continuation attachments, so the locals travel the way the
;; _ss$/_so$ site slots do: in a variable the frame keeps live across its calls.
;;
;; Each scope binds a jolt-dbg chain record (rt.ss) in a fresh _dl$ variable — an
;; arity's params, each let binding (a catch's local is one), a loop iteration's
;; bindings — and *dbg-scope* names the innermost one while its body emits. Every tail leaf
;; is emitted NON-tail and followed by (keep-live _dl$N): the fn's frame then
;; exists while the leaf runs, so a tail throw keeps the frame it was thrown
;; from, and _dl$N is live across every call in the scope. recur stays a tail
;; jump (emit-recur). So debug-compiled code loses tail calls except recur, as on
;; the JVM. Off, nothing here emits a byte, so the seed mint is unchanged.
(defn debug-locals? [] (boolean (some-> (:debug-locals? (cur)) deref)))
(def ^:dynamic *dbg-scope* nil)
;; The analyzer's own locals (gen-name: _r$<prefix><n>, a raw catch value, a
;; destructured arg) are not the program's, so no scope record names them, and
;; neither does a bare (gensym), G__<n>, which is how destructure names the
;; whole of a destructured arg.
(defn dbg-hidden? [nm]
  (or (str/starts-with? nm "_r$")
      (and (str/starts-with? nm "G__") (> (count nm) 3)
           (every? (fn [c] (<= (int \0) (int c) (int \9))) (subs nm 3)))))
;; A scope record's constructor call: one name and value, or a vector of each.
(defn dbg-record1 [parent nm munged]
  (str "(jolt-dbg-new1 " (or parent "#f") " " (chez-str-lit nm) " " munged ")"))
(defn dbg-record [parent nms munged]
  (let [ps (remove (fn [p] (dbg-hidden? (nth p 0))) (map vector nms munged))]
    (str "(jolt-dbg-new " (or parent "#f") " (quote #(" (str/join " " (map (fn [p] (chez-str-lit (nth p 0))) ps)) "))"
         " (vector " (str/join " " (map (fn [p] (nth p 1)) ps)) "))")))
;; The def whose init is being emitted, as [fqn fn-node]. Its own root is not
;; this fn yet while it compiles (unbound, or the previous definition), so a
;; call to it from its body is checked against the arities it declares.
(def ^:dynamic *dl-self* nil)
(defn direct-linkable? [ns nm]
  (and (direct-link?) (contains? @(:direct-link-defined (cur)) (dl-fqn ns nm))
       ;; A form the runtime spine compiles direct-linked (the unit flag is off)
       ;; sees vars that live on after it: remove-ns / ns-unmap can drop the
       ;; cell the binding was linked to, so the live cell has to be linked now
       ;; -- or be the def being compiled, which links when it runs.
       (or @(:direct-link? (cur))
           (= (dl-fqn ns nm) (first *dl-self*))
           (some? (var-linked? nil ns nm)))
       ;; .setDynamic after the def: a binding has to reach this reference
       (not (var-dynamic? nil ns nm))))
;; A direct-linked var whose value is a fn literal — its binding is a Scheme
;; procedure, so a call site can apply it directly.
(defn direct-link-fn? [ns nm]
  (contains? @(:direct-link-fns (cur)) (dl-fqn ns nm)))
(defn- arities-admit? [f n]
  (some (fn [a] (if (:rest a) (>= n (count (:params a))) (= n (count (:params a)))))
        (:arities f)))
;; May a call with n args apply the var's jv$ binding as a procedure? Being a fn
;; when it was defined is not enough: the root can since have become a
;; multimethod or a map (a later def), and a call of the wrong arity has to go
;; through jolt-invoke, which raises the ArityException naming the fn.
(defn direct-link-callable? [ns nm n]
  (let [self *dl-self*]
    (if (and self (= (nth self 0) (dl-fqn ns nm)))
      (boolean (arities-admit? (nth self 1) n))
      (some? (var-root-callable? nil ns nm n)))))

;; recur-target and the munged local names known to hold a procedure (a named
;; fn's self-recursion name) are lexically scoped — dynamic vars so the
;; recursion auto-restores them (no manual save/restore, no throw-leak).
;; *known-procs* maps each such name to the fn's trace site (the name its frame
;; carries: ns/name for a def's direct init, the registry label for a nested
;; literal), so a call site inside it records the same name the frame reports.
(def ^:dynamic *recur-target* nil)
(def ^:dynamic *known-procs* {})
;; munged local name -> the Scheme name holding its backing flvector, for the
;; ^doubles PARAMS of the arities being emitted. emit-arity-clause binds one per
;; such param at entry ((_av$N (jolt-array-vec-of a))), and a proven aget/aset
;; on that local indexes it directly instead of re-reading the checked record
;; accessor per access — bench/arrays 225 -> 145 ms in a Chez probe of the
;; emitted loop. Every binding form that can SHADOW the param (let, loop, a
;; nested arity's params, a catch binding) drops the name for its scope, so an
;; inner `a` bound to some other array never reads the outer one's vector.
(def ^:dynamic *array-vecs* {})
;; When set (in the :def emit path), the def's direct named init is emitted with
;; a qualified letrec binding (ns/name) so Chez reports a unique per-var frame
;; name — no collisions across namespaces — and that name is what the source
;; registry keys the def's record on. A nested named literal is bound under its
;; registry label instead (emit-fn), never under ns/name: that name is not
;; unique per literal, and it reads as a var the namespace does not have.
(def ^:dynamic *qualifying-ns* nil)
;; Set while emitting the init of a def whose value is an ANONYMOUS fn. Such a fn
;; is emitted bare so the enclosing (define jv$ns/name …) names the procedure and
;; its backtrace frame resolves; wrapping or re-binding it would drop that name.
;; emit-fn therefore skips its own variadic registration and emit-def-cached emits
;; the sibling (jolt-register-variadic! …) against the define's own binding.
(def ^:dynamic *variadic-reg-suppressed?* false)
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
;; The procedure name of the enclosing fn, including anonymous literals. A
;; finally thunk runs that fn's code after a tail call erased its frame, so it
;; keeps the same name for source lookup. Separate from *trace-site*: anonymous
;; literals need a frame name without changing their tail-site instrumentation.
(def ^:dynamic *trace-frame* nil)
;; A named fn's direct calls to ITSELF skip the restore when nothing the fn
;; tail-calls can run a jolt fn: every tail site it has then stores the fn's own
;; pair, and the reporter drops a pair whose fn is a live frame -- the caller is
;; that fn. fib and tak recurse that way and nothing else, and the restore was
;; ~9% and ~20% of them. Whether the fn qualifies is known only once its body is
;; emitted, so a self-call is emitted under a per-fn placeholder head that emit-fn
;; rewrites to the restore or to nothing (*self-wrap*: {:tok base :unsafe atom},
;; site-restore).
(def ^:dynamic *self-wrap* nil)
;; Inside a fn on Chez, a non-tail call's restore puts back the value the fn
;; clause read at entry (site-restore, emit-fn). Bound to an atom per clause
;; while that clause emits: :site once an ordinary site uses the entry value,
;; :self while only self-calls have (their restore is decided per fn, after the
;; body, see *self-wrap*).
(def ^:dynamic *site-call* nil)
;; Plain applications through a var root on a Chez with the call-error hook.
;; Set by the runtime capability probe; seed minting and stock Chez keep it off.
(def native-var-call? (atom false))
(defn set-native-var-call! [on] (reset! native-var-call? (boolean on)))
;; Global var calls (JOLT_GLOBAL_VAR_CALL, compile-eval.ss; only on a Chez with
;; patch 0010, never while minting the seed). The runtime links every var to the
;; top-level symbol jg$<ns>/<name> (rt.ss "global vars"), so a call through a var
;; that is not dynamic is a plain Chez application of that symbol and a value
;; reference is (jolt-gv 'jg$ns/name): no hoisted var lookup, no deref, no invoke
;; helper. Off, nothing changes.
(def global-var-call (atom false))
(defn set-global-var-call! [on] (reset! global-var-call (boolean on)))
;; Does a reference to ns/nm go through its jg$ symbol? Not for a var that is
;; dynamic when this code is compiled -- its reads stay binding-aware at the site
;; -- nor for *ns*, whose value is per thread without being bound. A var that
;; turns dynamic later reaches the symbol's trampoline (rt.ss).
(defn global-var-ref? [ns nm]
  (and @global-var-call
       (not (and (= ns "clojure.core") (= nm "*ns*")))
       (not (var-dynamic? nil ns nm))))
;; Keep-frames experiment (JOLT_KEEP_FRAMES=1, compile-eval.ss jolt-enable-trace!,
;; only on a Chez with $call-keeping-frame, host/chez/chez-patches/0012). Traced
;; code then erases no frame: every node in tail position that can make a call
;; runs inside (keep-frame ...), which Chez compiles as an ordinary call and
;; return, so the caller's frame -- and the line it is at -- stays on the stack,
;; as on the JVM. recur stays a jump. The trace then comes from the continuation
;; alone, so none of the slot machinery below is emitted: no tail-site pair
;; stores, no non-tail restores, no callsite registrations, no catch snapshots.
;; Off (the default, and always on stock Chez and Gambit), nothing changes.
;; Lazy quoted data experiment (JOLT_LAZY_META=1, compile-eval.ss; only where the
;; runtime answers jolt-lazy-meta-supported?, never while minting the seed or
;; building direct-linked). A def whose metadata is all constants hands the var
;; ONE quoted descriptor of it (literals.clj meta-desc) instead of the code that
;; builds the map, and the var builds the map the first time anything reads it
;; (rt.ss def-var-lazy-meta!). Off, nothing changes.
(def lazy-meta (atom false))
(defn set-lazy-meta! [on] (reset! lazy-meta (boolean on)))
(defn lazy-meta? [] (and @lazy-meta (var-cache?) (not (direct-link?)) (not (seed-mint?))))
;; ...and with JOLT_LAZY_META=1 (not =meta), the per-def registration rows too:
;; a form's fn-form rows as one quoted table (fnsrc.clj fnsrc-flush), a fn def's
;; source row as one more argument of its lazy def (backend_scheme.clj
;; emit-def-node), registered when something first looks.
(def lazy-rows (atom false))
(defn set-lazy-rows! [on] (reset! lazy-rows (boolean on)))
(defn lazy-rows? [] (and @lazy-rows (lazy-meta?)))
(def keep-frames (atom false))
(defn set-keep-frames! [on] (reset! keep-frames (boolean on)))
(defn keep-frames? [] (and @keep-frames (trace-frames?)))
(defn keep-frame [s] (str "(#2%$call-keeping-frame (lambda () " s "))"))
;; A tail call that may run a jolt fn disqualifies the enclosing fn.
(defn note-tail-call! [tail?]
  (when (and tail? *self-wrap*) (reset! (:unsafe *self-wrap*) true)))

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
(defn register-callsite! [line callee tail?]
  (when (and (trace-frames?) (not (keep-frames?)) *callsites* *trace-site* line callee)
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
;; reports; :local — a known procedure (self / an enclosing named fn): the site
;; its frame carries (*known-procs*), or the enclosing fn's *trace-site* for a
;; self-call; :keyword / :coll / computed callees — nil.
(defn static-callee [fnode]
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
             (when-let [site (get *known-procs* nm)]
               (if (contains? *trace-self* nm) *trace-site* site)))
    nil))
;; The def-emit sibling: one (jolt-register-callsite! …) per collected (line,
;; callee) entry, or "" when tracing is off / nothing collected — the seed mint
;; and `jolt build` emit with tracing off, so they stay byte-identical. Entries
;; sorted for deterministic output; "last call emitted on a line wins" (a tail
;; call is emitted after its operands, so its callee wins a shared line).
(defn trace-callsite-reg []
  (if (and (trace-frames?) *callsites* (seq @*callsites*))
    (str/join ""
      (for [[site sites] (sort-by key @*callsites*)
            [line callees] (sort-by key sites)
            [callee tail?] (sort-by (fn [e] (nth e 0)) callees)]
        (str " (jolt-register-callsite! " (chez-str-lit site) " " line " "
             (chez-str-lit callee) " " (if tail? "#t" "#f") ")")))
    ""))

(defn fresh-label [prefix] (str prefix (swap! (:gensym-counter (cur)) inc)))
;; The source names a letrec* group is CURRENTLY initialising (letfn, and the
;; state-machine loops core.async's CPS transform builds out of it). A literal in
;; one of those inits may reference a sibling — or itself — and letrec* makes
;; that legal because the reference is not read until the closure RUNS. Passing
;; such a name to a maker as an ARGUMENT reads it immediately, while the binding
;; is still uninitialised, which Chez reports as "variable lp__2 is not bound".
;; So a literal whose free names meet this set gets no maker. See fnsrc-maker-site.
(def ^:dynamic *letrec-binders* #{})
