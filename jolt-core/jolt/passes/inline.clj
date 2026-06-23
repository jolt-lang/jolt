(ns jolt.passes.inline
  "Inlining + flatten-lets + scalar-replace (AOT escape analysis). These run only
  when host/inline-enabled? (user code opted into direct-linking); they
  share the alpha-rename invariant (every spliced binder is made globally fresh)
  and the `dirty` fixpoint flag. Portable Clojure (compiler-tier)."
  (:require [jolt.host :refer [inline-ir]]
            [jolt.ir :refer [map-ir-children]]
            [jolt.passes.fold :refer [scalar-const?]]))

;; ---------------------------------------------------------------------------
;; Shared state: a dirty flag the fixpoint loop reads, and a fresh-name counter
;; for alpha-renaming inlined bodies (same atom pattern as analyzer/gen-name).
;; ---------------------------------------------------------------------------
(def dirty (atom false))   ;; read/reset by the run-passes fixpoint (jolt.passes)
(defn- mark! [] (reset! dirty true))

;; Record-ctor shape registry ("ns/->Name" -> {:fields (:k ..) :type tag}), fed
;; per unit by run-passes (set-rec-shapes!) before the fixpoint so scalar-replace
;; can recognize a (->Rec ..) call and map its positional args to declared fields
;; — the record analogue of the inline keys a map literal already carries in the
;; IR.
(def ^:private rec-shapes (atom {}))
(defn set-rec-shapes!
  "Install the record-ctor shape registry the record fold consults."
  [m] (reset! rec-shapes (or m {})))

(def ^:private fresh-counter (atom 0))
(defn- fresh [base]
  (let [n @fresh-counter]
    (swap! fresh-counter inc)
    (str base "__il" n)))

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
      (= op :map) (= op :vector) (= op :set) (= op :throw)))

(def ^:private inline-budget 120)

(defn- body-size
  "Node count of an inline-eligible body. A disallowed op contributes a number
  larger than any budget, so the caller's (<= size budget) test fails and we
  never try to inline (or alpha-rename) such a body."
  [node]
  (let [op (get node :op)]
    (cond
      (not (safe-op? op)) 100000
      (= op :if) (+ 1 (body-size (get node :test))
                      (body-size (get node :then))
                      (body-size (get node :else)))
      (= op :do) (+ 1 (reduce + 0 (mapv body-size (get node :statements)))
                      (body-size (get node :ret)))
      (= op :throw) (+ 1 (body-size (get node :expr)))
      (= op :invoke) (+ 1 (body-size (get node :fn))
                          (reduce + 0 (mapv body-size (get node :args))))
      (= op :let) (+ 1 (reduce + 0 (mapv (fn [b] (body-size (nth b 1))) (get node :bindings)))
                       (body-size (get node :body)))
      (= op :vector) (+ 1 (reduce + 0 (mapv body-size (get node :items))))
      (= op :set) (+ 1 (reduce + 0 (mapv body-size (get node :items))))
      (= op :map) (+ 1 (reduce + 0 (mapv (fn [pr] (+ (body-size (nth pr 0))
                                                     (body-size (nth pr 1))))
                                         (get node :pairs))))
      :else 1)))

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
      (= op :const) true
      (= op :var) true
      (= op :host) true
      (= op :the-var) true
      (= op :quote) true
      (= op :if) (and (body-closed? (get node :test) scope)
                      (body-closed? (get node :then) scope)
                      (body-closed? (get node :else) scope))
      (= op :do) (and (every? (fn [s] (body-closed? s scope)) (get node :statements))
                      (body-closed? (get node :ret) scope))
      (= op :throw) (body-closed? (get node :expr) scope)
      (= op :invoke) (and (body-closed? (get node :fn) scope)
                          (every? (fn [a] (body-closed? a scope)) (get node :args)))
      (= op :vector) (every? (fn [x] (body-closed? x scope)) (get node :items))
      (= op :set) (every? (fn [x] (body-closed? x scope)) (get node :items))
      (= op :map) (every? (fn [pr] (and (body-closed? (nth pr 0) scope)
                                        (body-closed? (nth pr 1) scope)))
                          (get node :pairs))
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [sc (nth acc 0) ok (nth acc 1)]
                            (if (not ok)
                              acc
                              [(conj sc (nth b 0)) (body-closed? (nth b 1) sc)])))
                        [scope true]
                        (get node :bindings))]
        (and (nth res 1) (body-closed? (get node :body) (nth res 0))))
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
                args (get node :args)]
            (if (and (= (count params) (count args))
                     (<= (body-size body) inline-budget)
                     (body-closed? body (reduce conj #{} params)))
              (let [n (count params)
                    ;; trivial args (local/const) substitute straight in (copy
                    ;; propagation); the rest get a fresh local bound once in a
                    ;; wrapping let, so they evaluate exactly once in source order.
                    res (loop [i 0 env {} binds []]
                          (if (< i n)
                            (let [p (nth params i) a (nth args i)]
                              (if (trivial-arg? a)
                                (recur (inc i) (assoc env p a) binds)
                                (let [f (fresh p)]
                                  (recur (inc i)
                                         (assoc env p {:op :local :name f})
                                         (conj binds [f a])))))
                            [env binds]))
                    env (nth res 0)
                    binds (nth res 1)
                    rbody (subst body env)]
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

(def ^:private pure-fns
  #{"+" "-" "*" "/" "<" ">" "<=" ">=" "=" "not=" "inc" "dec"
    "mod" "rem" "quot" "min" "max" "abs"
    "nil?" "some?" "not" "get" "zero?" "pos?" "neg?" "even?" "odd?"
    "bit-and" "bit-or" "bit-xor"})

(defn- pure-fn? [f]
  (let [op (get f :op)]
    (cond
      (and (= op :const) (keyword? (get f :val))) true
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
      (= op :const) true
      (= op :local) true
      (= op :var) true
      (= op :host) true
      (= op :the-var) true
      (= op :quote) true
      (= op :if) (and (pure? (get node :test)) (pure? (get node :then)) (pure? (get node :else)))
      (= op :do) (and (every? pure? (get node :statements)) (pure? (get node :ret)))
      (= op :let) (and (every? (fn [b] (pure? (nth b 1))) (get node :bindings)) (pure? (get node :body)))
      (= op :vector) (every? pure? (get node :items))
      (= op :set) (every? pure? (get node :items))
      (= op :map) (every? (fn [pr] (and (pure? (nth pr 0)) (pure? (nth pr 1)))) (get node :pairs))
      (= op :invoke) (and (or (pure-fn? (get node :fn)) (ctor-shape node))
                          (every? pure? (get node :args)))
      :else false)))

(defn- const-key-map? [node]
  (let [prs (get node :pairs)]
    (and (> (count prs) 0)
         (every? (fn [pr] (scalar-const? (nth pr 0))) prs))))

(defn- all-vals-pure? [node]
  (every? (fn [pr] (pure? (nth pr 1))) (get node :pairs)))

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
        (and (= :const (get f :op)) (keyword? (get f :val))
             (= 1 (count args))
             (= :local (get (nth args 0) :op)) (= nm (get (nth args 0) :name)))
        (get f :val)

        (and (or (and (= :var (get f :op)) (= "clojure.core" (get f :ns)) (= "get" (get f :name)))
                 (and (= :host (get f :op)) (= "get" (get f :name))))
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
  so scalar replacement only fires where the whole use region is simple."
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
        (let [rs (get @rec-shapes (str (get f :ns) "/" (get f :name)))]
          (if (and rs (= (count (get rs :fields)) (count (get node :args))))
            rs
            nil))
        nil))
    nil))

(defn- ctor-all-args-pure? [node] (every? pure? (get node :args)))

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
    (if (and (= :const (get f :op)) (keyword? (get f :val)) (= 1 (count args)))
      (let [m (nth args 0) k (get f :val)]
        (if (and (= :map (get m :op)) (const-key-map? m) (all-vals-pure? m))
          (do (mark!) (map-val m k))
          (let [rs (ctor-shape m)]
            (if (and rs (ctor-all-args-pure? m) (field-index (get rs :fields) k))
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
              ismap (and (= :map (get init :op)) (const-key-map? init) (all-vals-pure? init))
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
