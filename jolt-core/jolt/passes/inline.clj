(ns jolt.passes.inline
  "Inlining + flatten-lets + scalar-replace (AOT escape analysis). These run only
  when host/inline-enabled? (user code opted into direct-linking); they
  share the alpha-rename invariant (every spliced binder is made globally fresh)
  and the `dirty` fixpoint flag. Portable Clojure (compiler-tier)."
  (:require [jolt.host :refer [inline-ir]]
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
  ;; ops an inline-eligible body may contain. recur/loop/fn/try/def are excluded
  ;; (binding/control forms the splicer doesn't handle), so a body containing one
  ;; is rejected by body-size below and never inlined or alpha-renamed.
  (or (= op :const) (= op :local) (= op :var) (= op :host) (= op :the-var)
      (= op :quote) (= op :if) (= op :do) (= op :let) (= op :invoke)
      (= op :map) (= op :vector) (= op :set) (= op :throw) (= op :coerce)))

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

(defn- subst
  "Substitute locals in node per env (a map name -> replacement IR node), and
  alpha-rename every inner :let binder to a globally fresh name (so the spliced
  body shares no name with the caller). env seeds the params: a trivial arg
  (local/const) maps a param straight to the arg node (copy propagation — this
  is what lets scalar-replace see a map-literal arg through the call boundary);
  a non-trivial arg maps the param to a fresh :local that a wrapping let binds."
  [node env]
  (let [op (get node :op)]
    (cond
      (= op :local) (let [r (get env (get node :name))]
                      ;; carry the param's ^:struct hint onto a let-bound fresh
                      ;; local, so lookups inside the inlined body keep the bare
                      ;; (no-guard) path. The param hint asserts the
                      ;; arg is a struct; inlining doesn't change that contract.
                      (if r
                        (if (and (= :local (get r :op)) (get node :hint) (not (get r :hint)))
                          (assoc r :hint (get node :hint))
                          r)
                        node))
      ;; :let alpha-renames each binder to a fresh name, threading the extended
      ;; env left-to-right — sequential scope the uniform combinator can't model,
      ;; so it stays explicit.
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [e (nth acc 0)
                                binds (nth acc 1)
                                nm (nth b 0)
                                init (subst (nth b 1) e)
                                f (fresh nm)]
                            [(assoc e nm {:op :local :name f}) (conj binds [f init])]))
                        [env []]
                        (get node :bindings))]
        (assoc node :bindings (nth res 1) :body (subst (get node :body) (nth res 0))))
      ;; every other op substitutes env uniformly into its children. Inline
      ;; bodies only contain safe ops (see safe-op?), so loop/recur/fn/def/try
      ;; never reach here; the combinator handles them harmlessly regardless.
      :else (map-ir-children (fn [c] (subst c env)) node))))

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
      ;; leaves (:const/:var/:host/:the-var/:quote) fold to true; the rest AND
      ;; their children. Unsafe ops never reach here (body-size rejects them).
      (safe-op? op) (reduce-ir-children (fn [ok c] (and ok (body-closed? c scope))) true node)
      :else false)))

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
                nh (reduce (fn [m pr] (assoc m (nth pr 0) (nth pr 1))) {} (get stash :nhints))
                ret (get stash :ret)
                args (get node :args)]
            (if (and (= (count params) (count args))
                     (<= (body-size body) inline-budget)
                     (body-closed? body (reduce conj #{} params)))
              (let [n (count params)
                    ;; trivial args (local/const) substitute straight in (copy
                    ;; propagation); the rest get a fresh local bound once in a
                    ;; wrapping let, so they evaluate exactly once in source order.
                    ;; A ^double/^long param always binds (no copy-prop) so its
                    ;; entry coercion runs — preserving the called fn's semantics.
                    res (loop [i 0 env {} binds []]
                          (if (< i n)
                            (let [p (nth params i) a (nth args i) k (get nh p)]
                              (cond
                                k (let [f (fresh p)]
                                    (recur (inc i) (assoc env p {:op :local :name f})
                                           (conj binds [f (coerce-node k a)])))
                                (trivial-arg? a) (recur (inc i) (assoc env p a) binds)
                                :else (let [f (fresh p)]
                                        (recur (inc i) (assoc env p {:op :local :name f})
                                               (conj binds [f a])))))
                            [env binds]))
                    env (nth res 0)
                    binds (nth res 1)
                    rbody0 (subst body env)
                    ;; preserve the fn's ^double/^long return coercion.
                    rbody (if ret (coerce-node ret rbody0) rbody0)]
                (mark!)
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
                          (every? pure? (get node :args)))
      ;; leaves (:const/:local/:var/:host/:the-var/:quote) fold to true; :if/:do/
      ;; :let/:vector/:set/:map AND their children's purity.
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
      (= op :invoke) (and (or (total-fn? (get node :fn)) (ctor-shape node))
                          (every? total? (get node :args)))
      (safe-op? op) (reduce-ir-children (fn [ok c] (and ok (total? c))) true node)
      :else false)))

(defn- const-key-map? [node]
  (let [prs (get node :pairs)]
    (and (> (count prs) 0)
         (every? (fn [pr] (scalar-const? (nth pr 0))) prs))))

(defn- all-vals-pure? [node]
  (every? (fn [pr] (pure? (nth pr 1))) (get node :pairs)))
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
  "Replace every (:k nm)/(get nm :k) in node with the source value at k. The
  caller guarantees (via local-escapes?) that nm is never rebound here and
  appears only as a lookup subject, so no shadowing logic is needed."
  [node nm src]
  (let [k (lookup-key node nm)]
    (if k
      (src-val src k)
      ;; the caller's escape check guarantees nm is never rebound below, so we
      ;; recurse uniformly into every child — leaving any lookup of nm
      ;; un-substituted would dangle.
      (map-ir-children (fn [c] (subst-lookup c nm src)) node))))

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
                   (or ismap
                       (lookups-all-fields?
                         (conj (mapv (fn [bb] (nth bb 1)) (subvec binds (inc i) n)) body)
                         nm (get rs :fields))))
            (let [head (subvec binds 0 i)
                  tail (mapv (fn [bb] [(nth bb 0) (subst-lookup (nth bb 1) nm init)])
                             (subvec binds (inc i) n))
                  newbinds (reduce conj head tail)
                  newbody (subst-lookup body nm init)]
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
