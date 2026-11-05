(ns jolt.passes.inline
  "Inlining + flatten-lets + scalar-replace (AOT escape analysis). These run only
  when host/inline-enabled? (user code opted into direct-linking); they
  share the alpha-rename invariant (every spliced binder is made globally fresh)
  and the `dirty` fixpoint flag. Portable Clojure (compiler-tier)."
  (:require [jolt.host :refer [inline-ir mark-spliced!]]
            [jolt.ir :refer [map-ir-children reduce-ir-children coerce-node]]
            [jolt.op-registry :as op-registry]
            [jolt.passes.fold :refer [scalar-const? kw-callee? get-callee?]]))

;; ---------------------------------------------------------------------------
;; Shared state on the current compilation unit: the fixpoint `dirty` flag the
;; run-passes loop reads/resets, an alpha-rename counter for inlined bodies, and
;; the record-ctor shapes (the SAME shapes the inference installed on the unit).
;; The unit pointer lives in jolt.op-registry (the leaf) — this namespace can't
;; require the back end (a cycle), and the state must be shared with it.
;; ---------------------------------------------------------------------------
(defn- unit [] @jolt.op-registry/current-unit-box)
(defn- mark! [] (reset! (:dirty (unit)) true))

;; Record-ctor shapes ("ns/->Name" -> {:fields (:k ..) :type tag}) from the unit's
;; installed record-shapes, so scalar-replace recognizes a (->Rec ..) call and maps
;; its positional args to declared fields — the record analogue of the inline keys a
;; map literal already carries in the IR.
(defn- rec-shapes [] (get @(:config (unit)) :record-shapes))

(defn- fresh [base]
  (str base "__il" (swap! (:fresh-counter (unit)) inc)))

;; ---------------------------------------------------------------------------
;; Inlining. The back end stashes {:params [..] :body ir} on the var
;; cell of each single-fixed-arity defn compiled under :inline?; here we splice
;; that body at a call site. To stay capture-safe we ALPHA-RENAME the body —
;; every param and every inner let-bound name becomes a globally fresh name —
;; then bind the fresh params to the call's args in a wrapping let (args eval
;; once, in source order). After full renaming no name in the spliced body can
;; collide with a caller local, so flatten-lets and scalar-replace need no
;; shadowing logic.
;; ---------------------------------------------------------------------------

(defn- safe-op? [op]
  ;; ops an inline-eligible body may contain. The criterion is what the SPLICER
  ;; can handle, not purity: subst walks children through map-ir-children and
  ;; special-cases only :local (substitution) and :let (alpha-renames its
  ;; binders), so any op that binds no names is safe however it is spelled and
  ;; whether or not it can throw (:invoke and :throw are here already).
  ;;
  ;; :try and :def stay out — a winder and a definition, neither of which the
  ;; splicer has a case for, so a body containing one is rejected by body-size
  ;; below and never inlined or alpha-renamed.
  ;;
  ;; :fn, :loop and :recur BIND names, and subst/body-closed? below have explicit
  ;; cases for them (see subst's :fn and :loop arms). :recur additionally needs
  ;; recur-bound? — a recur that targets the inlined fn's OWN arity has no target
  ;; once spliced, so safe-op? alone cannot admit it.
  ;;
  ;; The literal leaves and the interop arg-only forms were missing rather than
  ;; refused. Measured while building a real project (grenadine) under --opt, a
  ;; :regex literal alone blocked 538 splice attempts and :host-new another 56 —
  ;; a fn was disqualified for containing #"..." Everything added here is either
  ;; a leaf (no child nodes at all) or carries only :args/:target, both of which
  ;; map-ir-children and reduce-ir-children already recurse.
  (or (= op :const) (= op :local) (= op :var) (= op :host) (= op :the-var)
      (= op :quote) (= op :if) (= op :do) (= op :let) (= op :invoke)
      (= op :map) (= op :vector) (= op :set) (= op :throw) (= op :coerce)
      ;; literal leaves, like :const
      (= op :regex) (= op :inst) (= op :uuid) (= op :bigdec)
      ;; interop: a leaf, and two arg-only call forms (:host is already above)
      (= op :host-static) (= op :host-new) (= op :host-call)
      ;; binders — see subst/body-closed?/recur-bound? below
      (= op :fn) (= op :loop) (= op :recur)))

(defn- recur-bound?
  "True if every :recur in node is enclosed by a :loop or :fn WITHIN node, so its
  target travels with it when the body is spliced.

  A :recur that is not — one sitting under :if/:do/:let straight off the body —
  targets the arity of the fn being inlined, and that arity does not exist at the
  call site. Splicing it would emit a recur with nothing to recur to. `bound?`
  starts false at the body root and turns true under a binder that provides a
  target.

  Separate from body-size because that is a context-free fold and this is not."
  [node bound?]
  (let [op (get node :op)]
    (cond
      (= op :recur) bound?
      (= op :loop) (and (reduce (fn [ok b] (and ok (recur-bound? (nth b 1) bound?)))
                                true (get node :bindings))
                        (recur-bound? (get node :body) true))
      (= op :fn) (reduce (fn [ok a] (and ok (recur-bound? (get a :body) true)))
                         true (get node :arities))
      :else (reduce-ir-children (fn [ok c] (and ok (recur-bound? c bound?))) true node))))

(def ^:private inline-budget 120)

(defn- body-size
  "Node count of an inline-eligible body. A disallowed op contributes a number
  larger than any budget, so the caller's (<= size budget) test fails and we
  never try to inline (or alpha-rename) such a body. Only reached for safe ops,
  so the shared child fold covers it exactly (leaves fold to 1)."
  [node]
  (if (not (safe-op? (get node :op)))
    100000
    (reduce-ir-children (fn [acc c] (+ acc (body-size c))) 1 node)))

;; Growth budget per top-level form. The per-site check above bounds one splice;
;; nothing bounded how many a form takes, so a mid-size callee at many sites grew
;; the form by its size at every one of them — clojure.test's `is` spliced
;; do-report twice per assertion, a 41-line deftest emitted 69KB of Scheme, and on
;; a test-heavy app inlining was half the build (#1059). run-passes opens a budget
;; for each form; a splice is charged its NET growth (the body minus the call it
;; replaces), so a callee no bigger than its call is free at any number of sites,
;; and once a form has grown by the budget its remaining calls stay calls. The
;; budget is a flat node count, not a multiple of the form: a big form is the one
;; least likely to be hot and the one where a multiple buys the most bytes (the
;; deftest above is ~1500 nodes before inlining and ~20 bytes of Scheme per node).
;; JOLT_INLINE_GROWTH=<nodes> at build time overrides it.
;;
;; An atom rather than a binding: inlining only runs in a closed-world build,
;; which compiles one form at a time on one thread. nil = no budget open (a
;; caller outside run-passes), which splices as before.
(defn- digits->long [s]
  (when (and (string? s) (pos? (count s)))
    (loop [i 0 n 0]
      (if (< i (count s))
        (let [d (- (int (nth s i)) 48)]
          (when (and (>= d 0) (<= d 9)) (recur (inc i) (+ (* n 10) d))))
        n))))
(def ^:private growth-default 400)
(def ^:private growth-left (atom nil))

(defn begin-growth-budget!
  "Open the code-growth budget for one top-level form."
  [_node]
  (reset! growth-left (or (digits->long (jolt.host/getenv "JOLT_INLINE_GROWTH"))
                          growth-default)))

(defn end-growth-budget! [] (reset! growth-left nil))

(defn- growth-of [body args]
  ;; a call is the invoke node, its fn node and one node per arg
  (max 0 (- (body-size body) (+ 2 (count args)))))

(defn- growth-fits? [g]
  (let [left @growth-left] (or (nil? left) (<= g left))))

(defn- charge-growth! [g]
  (when @growth-left (swap! growth-left - g)))

(defn- spliced-captures
  "What a fn literal's captures become in a SPLICED copy, one entry per name in
  its registered :free-names and in that order — a vector the back end emits
  alongside the (unchanged) source form.

  A capture is either still a live variable, under whatever name the splice
  renamed it to, or gone: a constant argument copy-propagated into the body
  leaves the closure with nothing to capture, and the value has to travel as
  data instead. So an entry is a NAME STRING (recover this one from the live
  closure through the inspector) or a one-element vector holding the VALUE.

  nil when some capture is neither — the caller then drops the registration, the
  copy is unregistered, and the image refuses it rather than rebuilding from
  names that no longer mean anything. env only ever maps to a :local or a
  :const, so that is a guard against a future substitution shape, not a case
  reached today."
  [frees env]
  (reduce (fn [acc nm]
            (if (nil? acc)
              nil
              (let [r (get env nm)]
                (cond
                  ;; not substituted at all: same name, same capture
                  (nil? r) (conj acc nm)
                  (= :local (get r :op)) (conj acc (get r :name))
                  (= :const (get r :op)) (conj acc [(get r :val)])
                  :else nil))))
          []
          frees))

(defn- subst
  "Substitute locals in node per env (a map name -> replacement IR node), and
  alpha-rename every inner :let binder to a globally fresh name (so the spliced
  body shares no name with the caller). env seeds the params: a trivial arg
  (local/const) maps a param straight to the arg node (copy propagation — this
  is what lets scalar-replace see a map-literal arg through the call boundary);
  a non-trivial arg maps the param to a fresh :local that a wrapping let binds.

  cns is the namespace the body was WRITTEN in — the callee's, not the call
  site's. Threaded rather than read off the node because only a fn literal needs
  it, and it needs it for the image registration (see the :fn arm)."
  [node env cns]
  (let [op (get node :op)]
    (cond
      (= op :local) (let [r (get env (get node :name))]
                      ;; carry the param's ^:struct hint onto a let-bound fresh
                      ;; local, so lookups inside the inlined body keep the bare
                      ;; (no-guard) path. The param hint asserts the
                      ;; arg is a struct; inlining doesn't change that contract.
                      (if r
                        (let [r (if (and (= :local (get r :op)) (get node :hint) (not (get r :hint)))
                                  (assoc r :hint (get node :hint))
                                  r)]
                          ;; a second splice keeps what the first one marked
                          (if (and (get node :boxed-param) (or (= :local (get r :op)) (= :const (get r :op))))
                            (assoc r :boxed-param true)
                            r))
                        node))
      ;; :let alpha-renames each binder to a fresh name, threading the extended
      ;; env left-to-right — sequential scope the uniform combinator can't model,
      ;; so it stays explicit.
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [e (nth acc 0)
                                binds (nth acc 1)
                                nm (nth b 0)
                                init (subst (nth b 1) e cns)
                                f (fresh nm)]
                            [(assoc e nm {:op :local :name f}) (conj binds [f init])]))
                        [env []]
                        (get node :bindings))]
        (assoc node :bindings (nth res 1) :body (subst (get node :body) (nth res 0) cns)))
      ;; :loop binds exactly like :let and its :recur travels with it, so it gets
      ;; the same sequential alpha-rename.
      (= op :loop)
      (let [res (reduce (fn [acc b]
                          (let [e (nth acc 0)
                                binds (nth acc 1)
                                nm (nth b 0)
                                init (subst (nth b 1) e cns)
                                f (fresh nm)]
                            [(assoc e nm {:op :local :name f}) (conj binds [f init])]))
                        [env []]
                        (get node :bindings))]
        (assoc node :bindings (nth res 1) :body (subst (get node :body) (nth res 0) cns)))
      ;; :fn — each arity's params (and :rest) bind within that arity, and a
      ;; named fn binds its own name across all of them. All are alpha-renamed,
      ;; for the reason the :let arm is: a param that merely SHADOWED the name in
      ;; env would still let the substituted argument be captured. (defn callee
      ;; [a] (map (fn [y] (+ a y)) xs)) inlined at (let [y 10] (callee y)) puts
      ;; the caller's `y` inside a fn that binds `y`, and shadowing alone turns
      ;; (+ a y) into (+ y y).
      ;;
      ;; A named inner fn is renamed too, and its emitted procedure — hence its
      ;; backtrace frame — is then `foo__ilN` rather than `foo`. That is the
      ;; cosmetic price of the same hygiene; an anonymous fn (the common case)
      ;; has no name to change.
      (= op :fn)
      (let [;; the env OUTSIDE this literal — what its free names resolve
            ;; through. Captured before the self-name shadows anything.
            outer env
            self (get node :name)
            env (if self (assoc env self {:op :local :name (fresh self)}) env)
            ren (fn [e nm] (get (get e nm) :name))
            arities
            (mapv (fn [a]
                    (let [names (if (get a :rest)
                                  (conj (vec (get a :params)) (get a :rest))
                                  (vec (get a :params)))
                          e (reduce (fn [e nm] (assoc e nm {:op :local :name (fresh nm)}))
                                    env names)
                          ;; :phints/:nhints/:ahints are vectors of [param value]
                          ;; keyed BY PARAM NAME; rekey them or the numeric and
                          ;; record-type passes look up names that no longer exist.
                          rekey (fn [k]
                                  (when-let [hs (get a k)]
                                    (mapv (fn [pr] [(ren e (nth pr 0)) (nth pr 1)]) hs)))
                          a (assoc a
                                   :params (mapv (fn [nm] (ren e nm)) (get a :params))
                                   :body (subst (get a :body) e cns))
                          a (if (get a :rest) (assoc a :rest (ren e (get a :rest))) a)
                          a (reduce (fn [a k] (if-let [v (rekey k)] (assoc a k v) a))
                                    a [:phints :nhints :ahints])]
                      a))
                  (get node :arities))
            node (assoc node :arities arities)
            node (if self (assoc node :name (ren env self)) node)
            ;; :src-form and :free-names are what the image rebuilds a travelling
            ;; closure from (analyzer analyze-fn, backend emit-fn/fnsrc-flush,
            ;; state-image image-eval-fnsrc): compile (fn* [free-names…] src-form)
            ;; in the defining ns and apply it to the values recovered from the
            ;; live closure by those same names.
            ;;
            ;; The SOURCE survives a splice untouched — a copy computes what the
            ;; callee's text says, that is the whole point. What does not survive
            ;; is the assumption that a free name is also the live variable's
            ;; name: the splice renamed the callee's binders and may have
            ;; substituted a caller local or a constant. So the registration
            ;; carries the source names AND, separately, what each one became
            ;; (spliced-captures) — the wrapper is still built from the source
            ;; names, and only the value lookup follows the copy.
            ;;
            ;; :src-ns for the same reason: the form is the callee's text and
            ;; resolves in the callee's namespace, which is not the one this copy
            ;; is being emitted into.
            ;;
            ;; Composes: a copy already spliced once carries :live-names, and
            ;; those — not the source names — are what this substitution renames
            ;; again. A constant entry has no name to look up and passes through.
            caps (when (get node :src-form)
                   (spliced-captures (or (get node :live-names) (get node :free-names))
                                     outer))]
        (cond
          (nil? (get node :src-form)) node
          ;; a capture the splice turned into something neither recoverable nor
          ;; constant: drop the registration rather than rebuild from a name that
          ;; no longer means anything. A name the inspector cannot report binds
          ;; jolt-nil, so that would surface as a wrong value long after the
          ;; restore; unregistered refuses at dump instead.
          (nil? caps) (dissoc node :src-form :free-names :live-names :src-ns)
          :else (assoc node :live-names caps :src-ns (or (get node :src-ns) cns))))
      ;; every other op substitutes env uniformly into its children.
      :else (map-ir-children (fn [c] (subst c env cns)) node))))

(defn- stamp-inline
  "Record, on every node of a callee body, the logical frames a backtrace should
  report for it — the inline equivalent of a DWARF inline record.

  :inline-chain is a vector of [callee-fqn call-line] pairs, INNERMOST FIRST.
  Each entry says \"the code here belongs to callee-fqn, which was called at
  call-line of the next frame out\"; the last entry's call-line is a line in the
  physical fn the whole thing ended up inside. Reading it back: the node's own
  line locates the first entry's fn, entry i's call-line locates entry i+1's, and
  the final call-line locates the physical fn (source-registry jolt-site-frame*).

  APPENDS rather than overwrites, which is what makes nesting work. inline-node
  runs bottom-up, so when C is spliced into P the body already carries whatever
  chain an earlier splice into C left; appending [C at] puts P's view outside it.
  deep-boom spliced into mid-boom spliced into -main ends up
  [[deep-boom 70] [mid-boom 31]], and reports three frames.

  `outer` is the chain the CALL SITE already carried, and it goes on the end of
  every stamped node. Without it a splice into an already-spliced region loses
  everything above the region: mid-boom is spliced into -main, the fixpoint then
  splices deep-boom into that copy, and deep-boom's nodes would claim
  [[deep-boom 70]] alone — naming -main at a line in another file.

  Applied to the callee body BEFORE subst, never after: subst replaces params
  with CALLER expressions, and those did not come from the callee — stamping the
  result would attribute the caller's own code to it."
  [node fqn at outer]
  (let [n (if at
            (assoc node :inline-chain
                   (into (conj (or (get node :inline-chain) []) [fqn at]) outer))
            node)]
    ;; STOP at a nested :fn. The chain describes the frames between a node and the
    ;; physical fn it ends up inside, and a nested fn is emitted as its own lambda
    ;; — it HAS a physical frame, so nothing separates its body from it. Stamping
    ;; through the boundary made the reporter expand that frame as though it were
    ;; spliced code and print the callee twice:
    ;;
    ;;   app.core/helper (core.clj:6)   <- spurious, from the inner fn's own frame
    ;;   named-step__il1
    ;;   app.core/helper (core.clj:7)
    ;;   app.core/-main  (core.clj:10)
    ;;
    ;; The :fn NODE itself still takes the stamp — building the closure is the
    ;; callee's code, and that expression really does sit in the caller's frame.
    ;; Only its arity bodies are excluded, which is exactly what map-ir-children
    ;; recurses into for a :fn (jolt-pzos).
    (if (= :fn (get node :op))
      n
      (map-ir-children (fn [c] (stamp-inline c fqn at outer)) n))))

(defn- trivial-arg? [n]
  ;; safe to substitute directly (immutable, free to duplicate): a local read or
  ;; a constant. Everything else is let-bound so it evaluates exactly once.
  (let [op (get n :op)] (or (= op :local) (= op :const))))

(defn- body-closed?
  "True if every :local in node is bound — by a param (in the initial scope set)
  or by an enclosing :let within the body. A self-recursive fn fails this: the
  analyzer binds the fn's own name as a local, so its body has a FREE local (the
  self-reference) that would dangle once the body is spliced elsewhere."
  [node scope]
  (let [op (get node :op)]
    (cond
      (= op :local) (contains? scope (get node :name))
      ;; :let threads scope sequentially (each binding extends it), so it can't go
      ;; through the uniform fold.
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [sc (nth acc 0) ok (nth acc 1)]
                            (if (not ok)
                              acc
                              [(conj sc (nth b 0)) (body-closed? (nth b 1) sc)])))
                        [scope true]
                        (get node :bindings))]
        (and (nth res 1) (body-closed? (get node :body) (nth res 0))))
      ;; :loop threads scope sequentially, exactly like :let.
      (= op :loop)
      (let [res (reduce (fn [acc b]
                          (let [sc (nth acc 0) ok (nth acc 1)]
                            (if (not ok)
                              acc
                              [(conj sc (nth b 0)) (body-closed? (nth b 1) sc)])))
                        [scope true]
                        (get node :bindings))]
        (and (nth res 1) (body-closed? (get node :body) (nth res 0))))
      ;; :fn — each arity's params and :rest bind only within that arity; a named
      ;; fn's own name binds across all of them (the analyzer makes the
      ;; self-reference a :local). Anything else its body reads has to be in the
      ;; enclosing scope, or the copy would dangle at the call site.
      (= op :fn)
      (let [sc (if (get node :name) (conj scope (get node :name)) scope)]
        (reduce (fn [ok a]
                  (and ok
                       (body-closed? (get a :body)
                                     (let [s (reduce conj sc (get a :params))]
                                       (if (get a :rest) (conj s (get a :rest)) s)))))
                true
                (get node :arities)))
      ;; leaves (:const/:var/:host/:the-var/:quote) fold to true; the rest AND
      ;; their children. Unsafe ops never reach here (body-size rejects them).
      (safe-op? op) (reduce-ir-children (fn [ok c] (and ok (body-closed? c scope))) true node)
      :else false)))

(defn direct-call-edges
  "The [ns name] of every direct callee in body — each :invoke whose :fn is a
  :var. These are the only edges the splicer can follow (a var passed as a VALUE
  is referenced, never spliced), so they are exactly the graph
  splice-cycle-member? walks. Computed once at stash time (jolt.passes/stash-of)
  and stored on the stash as :calls."
  [node]
  (let [f (get node :fn)
        acc (if (and (= :invoke (get node :op)) (= :var (get f :op)))
              [[(get f :ns) (get f :name)]]
              [])]
    (reduce-ir-children (fn [a c] (into a (direct-call-edges c))) acc node)))

(defn- stash-calls [ctx nsn nm]
  (let [s (inline-ir ctx nsn nm)]
    (if s (get s :calls) [])))

(defn- splice-cycle-member?
  "True when the callee can reach itself through the stash graph — a self- or
  mutually-recursive cluster every edge of which is spliceable. Inlining such a
  fn cannot converge: the spliced body re-exposes a call into the cycle, so each
  inline-fixpoint round pastes one more layer and the body grows by the cycle's
  branching factor per round until the round cap. Ruuter's 4-way matcher cycle
  unrolled to 4^5 residual calls — a 3.4MB match-trie and +73MiB idle RSS
  (jolt-682). A cycle member is never an inline candidate; its calls stay real,
  exactly as they compile without --opt. Only MEMBERSHIP disqualifies: a fn that
  merely calls INTO a cycle splices one bounded copy, because the cycle members
  inside that copy are refused here.

  Recomputed per splice attempt, no memo: stashes stream in as forms compile —
  from parallel namespace loads too — and a cached 'acyclic' goes stale the
  moment a later stash closes the loop."
  [ctx nsn nm]
  (let [start [nsn nm]]
    (loop [work (seq (stash-calls ctx nsn nm)) seen #{}]
      (if work
        (let [e (first work)]
          (cond
            (= e start) true
            (contains? seen e) (recur (next work) seen)
            :else (recur (seq (concat (stash-calls ctx (nth e 0) (nth e 1))
                                      (next work)))
                         (conj seen e))))
        false))))

(defn- try-inline
  "node is an :invoke whose children are already inlined. If its :fn is a var
  with a stashed, in-budget, arity-matching inline body, return the spliced
  let; else node."
  [node ctx]
  (let [f (get node :fn)]
    (if (= :var (get f :op))
      (let [stash (inline-ir ctx (get f :ns) (get f :name))]
        (if stash
          (let [params (get stash :params)
                body (get stash :body)
                ;; param name -> declared ^double/^long or array kind; both
                ;; bind under a :coerce of that kind
                nh (reduce (fn [m pr] (assoc m (nth pr 0) (nth pr 1)))
                           {} (concat (get stash :nhints) (get stash :ahints)))
                ;; declared ^Record param hints, param name -> ctor-key
                ph (reduce (fn [m pr] (assoc m (nth pr 0) (nth pr 1))) {} (get stash :phints))
                ret (get stash :ret)
                args (get node :args)]
            (if (and (= (count params) (count args))
                     (<= (body-size body) inline-budget)
                     (growth-fits? (growth-of body args))
                     (body-closed? body (reduce conj #{} params))
                     ;; a recur targeting the callee's own arity has no target
                     ;; once the body leaves it
                     (recur-bound? body false)
                     ;; cheapest checks first; the graph walk only runs on a
                     ;; stash that would otherwise splice.
                     (not (splice-cycle-member? ctx (get f :ns) (get f :name))))
              (let [n (count params)
                    ;; trivial args (local/const) substitute straight in (copy
                    ;; propagation); the rest get a fresh local bound once in a
                    ;; wrapping let, so they evaluate exactly once in source order.
                    ;; A ^double/^long param always binds (no copy-prop) so its
                    ;; entry coercion runs — preserving the called fn's semantics.
                    ;; An array-hinted one binds the same way: its coerce emits
                    ;; nothing, but it is what types the local for aget/aset.
                    ;; A ^Record param is a DECLARATION, not an inferred fact: it
                    ;; types the param whether or not the caller's argument type
                    ;; could be inferred (types.clj seeds an arity from :phints for
                    ;; exactly the open-world case). A spliced body has no arity to
                    ;; carry it, so the declared type rides on the substituted
                    ;; :local instead — every reference to the param is one of these
                    ;; nodes, so it reaches nested fns and let bodies alike, and it
                    ;; survives flatten-lets and scalar-replace rearranging the
                    ;; bindings around it. Dropping it made a spliced (:x p) fall
                    ;; back to jolt-get where the callee's own body bare-indexed.
                    rec-hint (fn [nd p]
                               (let [ck (get ph p)]
                                 ;; only a :local carries it — a constant argument
                                 ;; is not a record whatever the callee declared
                                 (if (and ck (= :local (get nd :op)))
                                   (assoc nd :rec-hint ck)
                                   nd)))
                    ;; An untyped param is boxed at the JVM's call, whatever the
                    ;; caller proves about its argument; the mark rides on the
                    ;; substituted node, as rec-hint's does, for a cast on it
                    ;; (jolt.passes.numeric boxed-operand?). A param the
                    ;; whole-program seeds typed is untyped in the source.
                    seeded (or (get stash :wp-seeded) #{})
                    boxed (fn [nd] (assoc nd :boxed-param true))
                    res (loop [i 0 env {} binds []]
                          (if (< i n)
                            (let [p (nth params i) a (nth args i) k (get nh p)]
                              (cond
                                k (let [f (fresh p) l {:op :local :name f}]
                                    (recur (inc i) (assoc env p (if (contains? seeded p) (boxed l) l))
                                           (conj binds [f (coerce-node k a)])))
                                (trivial-arg? a) (recur (inc i) (assoc env p (boxed (rec-hint a p))) binds)
                                :else (let [f (fresh p)]
                                        (recur (inc i)
                                               (assoc env p (boxed (rec-hint {:op :local :name f} p)))
                                               (conj binds [f a])))))
                            [env binds]))
                    env (nth res 0)
                    binds (nth res 1)
                    ;; stamp before substituting — see stamp-inline
                    rbody0 (subst (stamp-inline body
                                                (str (get f :ns) "/" (get f :name))
                                                (let [p (get node :pos)]
                                                  (when (map? p) (get p :line)))
                                                (get node :inline-chain))
                                  env
                                  (get f :ns))
                    ;; preserve the fn's ^double/^long return coercion -- as a
                    ;; RETURN coercion, which for ^long truncates a bignum to its
                    ;; low 64 bits and saturates a flonum where a param's raises
                    ;; (emit-arity-clause, jolt->fx-ret).
                    rbody (if ret (assoc (coerce-node ret rbody0) :return? true) rbody0)]
                (mark!)
                (charge-growth! (growth-of body args))
                ;; Tell the host this callee's body was actually copied somewhere.
                ;; The tree-shake graph roots the set: with every call site spliced
                ;; there is no reference left to the callee's def, so the shake
                ;; would drop it — along with the (jolt-register-source! …) its
                ;; record carries, which is what names an inlined frame in a
                ;; backtrace (jolt-o13s). Recorded at the SPLICE, so a stashed fn
                ;; nobody spliced still shakes away.
                (mark-spliced! ctx (get f :ns) (get f :name))
                (if (= 0 (count binds))
                  rbody
                  {:op :let :bindings binds :body rbody}))
              node))
          node))
      node)))

(defn inline-node
  "Bottom-up: inline children first, then attempt to inline this node."
  [node ctx]
  (if (= :invoke (get node :op))
    ;; inline children first, then attempt to splice this call
    (try-inline (map-ir-children (fn [c] (inline-node c ctx)) node) ctx)
    (map-ir-children (fn [c] (inline-node c ctx)) node)))

;; ---------------------------------------------------------------------------
;; flatten-lets: (let [a (let [b X] Y) ..] body) -> (let [b X a Y ..] body).
;; Safe because inlined bodies are alpha-renamed (every binder unique), so the
;; hoisted bindings can't collide. Exposes a map-returning init directly to
;; scalar-replace when it was wrapped in an inlined arg's let.
;; ---------------------------------------------------------------------------
(defn- flatten-let-bindings [binds]
  ;; returns a flattened binding vector; sets dirty when it hoists.
  (reduce (fn [out b]
            (let [nm (nth b 0) init (nth b 1)]
              (if (= :let (get init :op))
                (do (mark!)
                    (conj (reduce conj out (get init :bindings))
                          [nm (get init :body)]))
                (conj out b))))
          []
          binds))

(defn flatten-lets [node]
  (if (= :let (get node :op))
    ;; flatten children first, then hoist any let-valued binding inits
    (let [n (map-ir-children flatten-lets node)]
      (assoc n :bindings (flatten-let-bindings (get n :bindings))))
    (map-ir-children flatten-lets node)))

;; ---------------------------------------------------------------------------
;; scalar-replace (AOT escape analysis). A map allocation whose ONLY use is
;; constant-keyword lookup is dead weight: replace each (:k m) with the literal
;; value at :k and drop the allocation. Two forms:
;;   (a) direct:    (:k {:k a ..})            -> a
;;   (b) let-bound: (let [m {:k a ..}] .. (:k m) ..) -> .. a ..   (m non-escaping)
;; Both require the dropped sibling values to be pure (we duplicate/discard them).
;; ---------------------------------------------------------------------------

;; Pure = no side effects AND total (never throws), so a fold may duplicate or
;; discard the call. / quot rem mod throw on a zero divisor; even?/odd? throw on
;; a non-integer — admitting them let scalar-replace drop (:b (/ 1 0)) and swallow
;; the ArithmeticException. Add nothing here that can throw on a legal input.
(def ^:private pure-fns op-registry/pure-ops)

;; A call at an arity its callee lacks throws ArityException, so it is neither
;; pure nor total however pure the callee: dropping (:k m x y) would swallow the
;; throw (#1162). A keyword invokes at one or two args; a registry op at what its
;; :arity admits (stricter than Clojure for some, e.g. (= x) — which only costs a
;; fold). A callee with no recorded arity keeps the old answer.
(defn- invoke-arity-ok? [node]
  (let [f (get node :fn)
        n (count (get node :args))]
    (cond
      (kw-callee? f) (<= 1 n 2)
      (contains? #{:var :host} (get f :op))
      (let [ok (op-registry/op-arity (get f :name))]
        (or (nil? ok) (boolean (ok n))))
      :else true)))

(defn- pure-fn? [f]
  (let [op (get f :op)]
    (cond
      (kw-callee? f) true
      (= op :var) (and (= "clojure.core" (get f :ns)) (contains? pure-fns (get f :name)))
      (= op :host) (contains? pure-fns (get f :name))
      :else false)))

;; forward ref: a record ctor (allocating an immutable struct from its args) is
;; side-effect-free, so pure? treats (->Rec pure-args..) as pure — which lets a
;; nested record (a Ray holding a Vec3) fold bottom-up.
(declare ctor-shape)

(defn- pure?
  "Conservative: true only for expressions with no side effects that are safe to
  duplicate or discard. A var/host ref is a pure read; an invoke is pure for a
  known-pure fn (arithmetic, comparison, keyword lookup, get) or a record
  constructor (an immutable struct alloc) whose args are themselves pure."
  [node]
  (let [op (get node :op)]
    (cond
      ;; :invoke is pure only for a known-pure fn / record ctor, and only its ARGS
      ;; are folded (not the :fn position) — so it can't go through the uniform fold.
      (= op :invoke) (and (or (pure-fn? (get node :fn)) (ctor-shape node))
                          (invoke-arity-ok? node)
                          (every? pure? (get node :args)))
      ;; leaves (:const/:local/:var/:host/:the-var/:quote) fold to true; :if/:do/
      ;; :let/:vector/:set/:map AND their children's purity. :throw is safe-op? (an
      ;; inline body may contain one — splicing preserves it) but is NOT pure: it
      ;; must not be duplicated, relocated across effects, or reach total?.
      (= op :throw) false
      (safe-op? op) (reduce-ir-children (fn [ok c] (and ok (pure? c))) true node)
      :else false)))

;; A pure fn is safe to DUPLICATE / RELOCATE (it throws at the new site, same as the
;; old), but DISCARDING one that throws would swallow the exception. total-fns is the
;; subset of pure-fns that never throws on any input, so it is also safe to discard.
;; The numeric ops in pure-fns throw on non-numeric args; scalar-replace runs before
;; type inference, so their operand types aren't known here — they stay pure (for
;; relocation) but are not total (for a drop).
(def ^:private total-fns
  #{"=" "not=" "nil?" "some?" "not" "get"})

(defn- total-fn? [f]
  (let [op (get f :op)]
    (cond
      (kw-callee? f) true
      (= op :var) (and (= "clojure.core" (get f :ns)) (contains? total-fns (get f :name)))
      (= op :host) (contains? total-fns (get f :name))
      :else false)))

(defn- total?
  "Stronger than pure?: no side effects AND never throws, so the expression is safe
  to DISCARD entirely (a merely-pure expression that throws would swallow the
  exception if dropped). A record ctor is total when its args are — the alloc
  itself doesn't throw."
  [node]
  (let [op (get node :op)]
    (cond
      ;; :throw always throws — discarding it swallows the exception.
      (= op :throw) false
      (= op :invoke) (and (or (total-fn? (get node :fn)) (ctor-shape node))
                          (invoke-arity-ok? node)
                          (every? total? (get node :args)))
      (safe-op? op) (reduce-ir-children (fn [ok c] (and ok (total? c))) true node)
      :else false)))

(defn- const-key-map? [node]
  (let [prs (get node :pairs)]
    (and (> (count prs) 0)
         (every? (fn [pr] (scalar-const? (nth pr 0))) prs))))

;; total variant — for a map whose unread values would be DISCARDED when the map
;; binding is dropped, so they must not throw.
(defn- all-vals-total? [node]
  (every? (fn [pr] (total? (nth pr 1))) (get node :pairs)))

(defn- map-val
  "The value IR at scalar key k in a const-key map node, or a nil constant when k
  is absent (struct-eligible literal: a missing key reads nil, like the back end)."
  [mapnode k]
  (let [prs (get mapnode :pairs) n (count prs)]
    (loop [i 0]
      (if (< i n)
        (let [pr (nth prs i)]
          (if (= (get (nth pr 0) :val) k) (nth pr 1) (recur (inc i))))
        {:op :const :val nil}))))

(defn- lookup-key
  "If node is a constant-keyword lookup of (:local nm) — either (:k nm) or
  (get nm :k) — return the keyword k; else nil."
  [node nm]
  (if (= :invoke (get node :op))
    (let [f (get node :fn) args (get node :args)]
      (cond
        (and (kw-callee? f)
             (= 1 (count args))
             (= :local (get (nth args 0) :op)) (= nm (get (nth args 0) :name)))
        (get f :val)

        (and (get-callee? f)
             (= 2 (count args))
             (= :local (get (nth args 0) :op)) (= nm (get (nth args 0) :name))
             (scalar-const? (nth args 1)))
        (get (nth args 1) :val)

        :else nil))
    nil))

(defn- any-binding-named? [binds nm]
  (loop [i 0]
    (if (< i (count binds))
      (if (= nm (nth (nth binds i) 0)) true (recur (inc i)))
      false)))

(defn- any-name? [names nm]
  (loop [i 0]
    (if (< i (count names))
      (if (= nm (nth names i)) true (recur (inc i)))
      false)))

(defn- local-escapes?
  "Does local nm escape in node — i.e. is it used anywhere other than as the
  subject of a constant-keyword lookup? Precise over straight-line expression
  ops; conservatively true for loop/fn/try/recur/def (and any rebinding of nm),
  so scalar replacement only fires where the whole use region is simple.

  Stays an explicit per-op walk (not the shared reduce-ir-children fold): its
  default is conservatively TRUE, and the lookup-subject and rebinding cases
  inspect node shape beyond child purity — folding an unhandled op over its
  children would under-report escape and is unsound for scalar replacement."
  [node nm]
  (let [op (get node :op)
        k (lookup-key node nm)]
    (cond
      ;; an ok lookup of nm: nm itself is consumed; still scan any extra args
      ;; (a get default could reference nm), never the subject local at arg 0.
      k (let [args (get node :args)]
          (if (> (count args) 1)
            (loop [i 1]
              (if (< i (count args))
                (if (local-escapes? (nth args i) nm) true (recur (inc i)))
                false))
            false))
      (= op :local) (= nm (get node :name))
      (= op :const) false
      (= op :var) false
      (= op :host) false
      (= op :the-var) false
      (= op :quote) false
      (= op :if) (or (local-escapes? (get node :test) nm)
                     (local-escapes? (get node :then) nm)
                     (local-escapes? (get node :else) nm))
      (= op :do) (or (loop [i 0 ss (get node :statements)]
                       (if (< i (count ss))
                         (if (local-escapes? (nth ss i) nm) true (recur (inc i) ss))
                         false))
                     (local-escapes? (get node :ret) nm))
      (= op :throw) (local-escapes? (get node :expr) nm)
      (= op :invoke) (or (local-escapes? (get node :fn) nm)
                         (loop [i 0 as (get node :args)]
                           (if (< i (count as))
                             (if (local-escapes? (nth as i) nm) true (recur (inc i) as))
                             false)))
      (= op :vector) (loop [i 0 xs (get node :items)]
                       (if (< i (count xs))
                         (if (local-escapes? (nth xs i) nm) true (recur (inc i) xs))
                         false))
      (= op :set) (loop [i 0 xs (get node :items)]
                    (if (< i (count xs))
                      (if (local-escapes? (nth xs i) nm) true (recur (inc i) xs))
                      false))
      (= op :map) (loop [i 0 ps (get node :pairs)]
                    (if (< i (count ps))
                      (if (or (local-escapes? (nth (nth ps i) 0) nm)
                              (local-escapes? (nth (nth ps i) 1) nm))
                        true (recur (inc i) ps))
                      false))
      (= op :let) (let [binds (get node :bindings)]
                    (if (any-binding-named? binds nm)
                      true ;; nm rebound here — bail (safe; inlined names are unique)
                      (or (loop [i 0]
                            (if (< i (count binds))
                              (if (local-escapes? (nth (nth binds i) 1) nm) true (recur (inc i)))
                              false))
                          (local-escapes? (get node :body) nm))))
      ;; recur binds nothing — its args are ordinary expressions (this is the
      ;; common loop-body tail; treating it as a blanket escape would block
      ;; scalar replacement in every loop).
      (= op :recur) (loop [i 0 as (get node :args)]
                      (if (< i (count as))
                        (if (local-escapes? (nth as i) nm) true (recur (inc i) as))
                        false))
      (= op :loop) (let [binds (get node :bindings)]
                     (if (any-binding-named? binds nm)
                       true
                       (or (loop [i 0]
                             (if (< i (count binds))
                               (if (local-escapes? (nth (nth binds i) 1) nm) true (recur (inc i)))
                               false))
                           (local-escapes? (get node :body) nm))))
      (= op :fn) (loop [i 0 ars (get node :arities)]
                   (if (< i (count ars))
                     (let [ar (nth ars i)
                           ps (get ar :params)]
                       ;; a param (or rest) shadowing nm hides ours in that arity
                       (if (or (any-name? ps nm) (= nm (get ar :rest)))
                         true
                         (if (local-escapes? (get ar :body) nm) true (recur (inc i) ars))))
                     false))
      (= op :try) (or (local-escapes? (get node :body) nm)
                      (let [cb (get node :catch-body)]
                        (and cb (not (= nm (get node :catch-sym))) (local-escapes? cb nm)))
                      (let [f (get node :finally)] (and f (local-escapes? f nm))))
      (= op :def) (local-escapes? (get node :init) nm)
      :else true)))

;; --- record constructors as foldable struct sources -------------------------
;; A record ctor (->Rec a b ..) is a positional struct: the registry maps its
;; ctor key ("ns/->Name", exactly how the IR names the call head) to the DECLARED
;; field order. A field read on a non-escaping ctor folds to the matching arg,
;; just as (:k {:k a ..}) folds to a. Two soundness differences from maps:
;;   - the ctor's args are duplicated/discarded, so they must be pure (like map
;;     vals), and the arg count must equal the field count (a positional call);
;;   - a record answers the virtual :jolt/deftype key with its type tag and any
;;     other non-field key with nil — neither is a positional arg, so we only
;;     fold DECLARED-field reads and keep the allocation otherwise.

(defn- ctor-shape
  "If node is a record-constructor :invoke (its :fn a :var whose ns/name is a
  registered ctor key, with arg count matching the declared field count), return
  that record's shape entry; else nil."
  [node]
  (if (= :invoke (get node :op))
    (let [f (get node :fn)]
      (if (= :var (get f :op))
        (let [rs (get (rec-shapes) (str (get f :ns) "/" (get f :name)))]
          (if (and rs (= (count (get rs :fields)) (count (get node :args))))
            rs
            nil))
        nil))
    nil))

(defn- ctor-all-args-pure? [node] (every? pure? (get node :args)))
;; total variant — for the (:k (->Rec …)) fold, where every arg EXCEPT the one at k
;; is discarded, so a discarded sibling must not throw.
(defn- ctor-all-args-total? [node] (every? total? (get node :args)))

(defn- field-index
  "Index of scalar key k in the declared field tuple fields, or nil."
  [fields k]
  (let [n (count fields)]
    (loop [i 0]
      (if (< i n)
        (if (= (nth fields i) k) i (recur (inc i)))
        nil))))

(defn- ctor-val
  "The positional arg IR at declared field k of record ctor node (shape rs). Only
  called for a key known to be a field, so the index is always present."
  [ctor rs k]
  (nth (get ctor :args) (field-index (get rs :fields) k)))

(defn- collect-keys!
  "Accumulate (into atom acc) every constant-keyword lookup key applied to local
  nm in node. The caller has proven (via local-escapes?) that nm appears only as
  a lookup subject and is never rebound, so a uniform recursion suffices: at a
  lookup of nm we record the key and stop (its subject is nm itself); elsewhere
  we recurse into children."
  [node nm acc]
  (let [k (lookup-key node nm)]
    (if k
      (swap! acc conj k)
      (map-ir-children (fn [c] (collect-keys! c nm acc) c) node))))

(defn- lookups-all-fields?
  "True if every lookup of nm across nodes uses a declared field in fields — the
  record-only guard that keeps a :jolt/deftype/unknown-key read (not a positional
  arg) from being folded to the wrong value."
  [nodes nm fields]
  (every? (fn [node]
            (let [acc (atom #{})]
              (collect-keys! node nm acc)
              (every? (fn [k] (field-index fields k)) @acc)))
          nodes))

(defn- src-val
  "Field value at k from a foldable struct source — a const-key map (absent key
  -> nil, struct-map semantics) or a record ctor (k is always a declared field
  here, guaranteed by lookups-all-fields?)."
  [src k]
  (if (= :map (get src :op))
    (map-val src k)
    (ctor-val src (ctor-shape src) k)))

(defn- subst-lookup
  "Replace every (:k nm)/(get nm :k) in node with the source value at k, or with
  the local hoisted maps k to. The caller guarantees (via local-escapes?) that nm
  is never rebound here and appears only as a lookup subject, so no shadowing
  logic is needed."
  [node nm src hoisted]
  (let [k (lookup-key node nm)]
    (if k
      (let [h (get hoisted k)] (if h h (src-val src k)))
      ;; the caller's escape check guarantees nm is never rebound below, so we
      ;; recurse uniformly into every child — leaving any lookup of nm
      ;; un-substituted would dangle.
      (map-ir-children (fn [c] (subst-lookup c nm src hoisted)) node))))

;; --- field values that make an object ----------------------------------------
;; Moving a field value to its reads evaluates it once per read. For a number
;; that is the same value; for a value that ALLOCATES it is a new object each
;; time, where the struct held one: (let [m {:x [a]}] (identical? (:x m) (:x m)))
;; was false, and a read in a loop body made one per iteration. Such a value is
;; bound once to a fresh local where the struct was, and its reads read that.
;; The local is a struct source in its own right when the value is a map or a
;; record, so the fixpoint still scalar-replaces a nested Vec3 inside a Ray.

(defn- allocates?
  "Can evaluating node make an object that a second evaluation would not share?
  Conservative: a collection literal, a fn literal, a host constructor or a record
  ctor anywhere in node. A constant collection is hoisted by the back end, so a
  :const is one object however often it is read."
  [node]
  (let [op (get node :op)]
    (cond
      (or (= op :vector) (= op :map) (= op :set) (= op :fn) (= op :host-new)) true
      (ctor-shape node) true
      :else (reduce-ir-children (fn [acc c] (or acc (allocates? c))) false node))))

(defn- src-entries
  "The [k value] fields of a struct source in evaluation order."
  [src]
  (if (= :map (get src :op))
    (mapv (fn [pr] [(get (nth pr 0) :val) (nth pr 1)]) (get src :pairs))
    (let [fields (get (ctor-shape src) :fields) args (get src :args)]
      (mapv (fn [i] [(nth fields i) (nth args i)]) (range (count args))))))

(defn- hoist-allocating
  "[bindings hoisted]: a fresh binding for each field of src that is read (in
  nodes) and whose value allocates, in source order, and the map from its key to
  the :local that reads it."
  [src nm nodes]
  (let [acc (atom #{})]
    (reduce (fn [_ n] (collect-keys! n nm acc) nil) nil nodes)
    (reduce (fn [r e]
              (let [k (nth e 0) v (nth e 1)]
                (if (and (contains? @acc k) (not (get (nth r 1) k)) (allocates? v))
                  (let [f (fresh (str nm "_" (name k)))]
                    [(conj (nth r 0) [f v]) (assoc (nth r 1) k {:op :local :name f})])
                  r)))
            [[] {}]
            (src-entries src))))

;; --- where a struct's field args may move to ---------------------------------
;; elim-let-structs MOVES each field arg from the let binding to every read of
;; that field. local-escapes? proves the reads are the only uses; it does not
;; prove the arg still means the same thing at the read. Two places it does not:
;;   - under a binder that rebinds a local the arg reads. (let [p (->P a 2)]
;;     (let [a 10] (:x p))) is 1, and moved it reads the inner a: 10. Inlined
;;     bodies are alpha-renamed, user code is not.
;;   - inside a fn literal. The closure captured p; moved, it captures whatever
;;     the args read instead, while the literal's :free-names (what the back end
;;     passes to the site's image maker, and what an image rebuilds the closure
;;     from) still names p, which no longer exists: "variable p is not bound" at
;;     the site. The args would also re-run on every call instead of once.
;; Both are refused here, so the allocation stays.

(defn- mentions-local? [node nm]
  (if (and (= :local (get node :op)) (= nm (get node :name)))
    true
    (reduce-ir-children (fn [acc c] (or acc (mentions-local? c nm))) false node)))

(defn- local-names
  "Every local name node reads, free or not — an over-approximation of its free
  locals, which only makes relocation-blocked? refuse more."
  [node]
  (let [acc (atom #{})]
    (letfn [(walk [n]
              (when (= :local (get n :op)) (swap! acc conj (get n :name)))
              (reduce-ir-children (fn [_ c] (walk c) nil) nil n))]
      (walk node))
    @acc))

(defn- relocation-blocked?
  "Would moving an expression reading the locals in fv to the reads of nm in node
  change what it computes? True when a read of nm sits inside a fn literal, or
  under a let/loop/catch binder whose name is in fv."
  [node nm fv]
  (let [op (get node :op)]
    (cond
      (= op :fn) (mentions-local? node nm)
      (and (or (= op :let) (= op :loop))
           (some (fn [b] (contains? fv (nth b 0))) (get node :bindings)))
      (mentions-local? node nm)
      (and (= op :try) (get node :catch-body) (contains? fv (get node :catch-sym))
           (mentions-local? (get node :catch-body) nm))
      true
      :else (reduce-ir-children (fn [acc c] (or acc (relocation-blocked? c nm fv))) false node))))

(defn- fold-kw-literal
  "(a) (:k <source>) -> the value at k. <source> is a const-key pure map
  ((:k {:k a ..}) -> a) or a pure record ctor ((:k (->Rec a ..)) -> the arg for
  field k). Siblings are duplicated/discarded, so all must be pure; a record
  lookup folds only for a declared field."
  [node]
  (let [f (get node :fn) args (get node :args)]
    (if (and (kw-callee? f) (= 1 (count args)))
      (let [m (nth args 0) k (get f :val)]
        (if (and (= :map (get m :op)) (const-key-map? m) (all-vals-total? m))
          (do (mark!) (map-val m k))
          (let [rs (ctor-shape m)]
            (if (and rs (ctor-all-args-total? m) (field-index (get rs :fields) k))
              (do (mark!) (ctor-val m rs k))
              node))))
      node)))

(defn- elim-let-structs
  "(b) Drop the first non-escaping let binding whose init is a foldable struct
  source — a pure const-key map literal or a pure record ctor — substituting its
  field reads into the remaining bindings and body. Fixpoint re-runs us for the
  rest, so one elimination per call keeps it simple. For a record every lookup
  of the binding must hit a declared field, else we keep the allocation."
  [node]
  (let [binds (get node :bindings) n (count binds) body (get node :body)]
    (loop [i 0]
      (if (< i n)
        (let [b (nth binds i) nm (nth b 0) init (nth b 1)
              ;; a map's unread values are DISCARDED when the binding is dropped (must
              ;; be total); a record requires every field be read (lookups-all-fields?
              ;; below), so its args are all relocated, not discarded — pure is enough.
              ismap (and (= :map (get init :op)) (const-key-map? init) (all-vals-total? init))
              rs (when (not ismap) (ctor-shape init))
              isrec (and rs (ctor-all-args-pure? init))]
          (if (and (or ismap isrec)
                   (not (any-binding-named? (subvec binds (inc i) n) nm))
                   (not (loop [j (inc i)]
                          (if (< j n)
                            (if (local-escapes? (nth (nth binds j) 1) nm) true (recur (inc j)))
                            false)))
                   (not (local-escapes? body nm))
                   (let [fv (local-names init)
                         tail (subvec binds (inc i) n)]
                     (and (not (some (fn [bb] (contains? fv (nth bb 0))) tail))
                          (not (some (fn [bb] (relocation-blocked? (nth bb 1) nm fv)) tail))
                          (not (relocation-blocked? body nm fv))))
                   (or ismap
                       (lookups-all-fields?
                         (conj (mapv (fn [bb] (nth bb 1)) (subvec binds (inc i) n)) body)
                         nm (get rs :fields))))
            (let [tail0 (subvec binds (inc i) n)
                  hz (hoist-allocating init nm (conj (mapv (fn [bb] (nth bb 1)) tail0) body))
                  hoisted (nth hz 1)
                  head (reduce conj (subvec binds 0 i) (nth hz 0))
                  tail (mapv (fn [bb] [(nth bb 0) (subst-lookup (nth bb 1) nm init hoisted)])
                             tail0)
                  newbinds (reduce conj head tail)
                  newbody (subst-lookup body nm init hoisted)]
              (mark!)
              (if (= 0 (count newbinds))
                newbody
                (assoc node :bindings newbinds :body newbody)))
            (recur (inc i))))
        node))))

(defn scalar-replace
  "Bottom-up: scalar-replace children, then apply (a) at invokes / (b) at lets."
  [node]
  (let [op (get node :op)]
    (cond
      ;; (a) fold (:k <map|ctor>) at invokes, after scalar-replacing children
      (= op :invoke) (fold-kw-literal (map-ir-children scalar-replace node))
      ;; (b) drop a non-escaping foldable-struct let binding, after children
      (= op :let) (elim-let-structs (map-ir-children scalar-replace node))
      :else (map-ir-children scalar-replace node))))
