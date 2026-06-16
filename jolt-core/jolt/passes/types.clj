(ns jolt.passes.types
  "Collection-type inference (jolt-99x) and success-type checking (RFC 0006).
  A forward, soft-typing pass (simplified HM: monovariant, never-fails, lattice
  top = :any) that types expressions and reuses the SAME walk as a loose success
  checker. Also the inter-procedural driver API (jolt-767) the back end calls to
  propagate param types across a unit / the whole program. Weakly coupled to the
  IR-rewriting passes — shares only the const-shape predicate (jolt.passes.fold)."
  (:require [jolt.passes.fold :refer [scalar-const?]]))

;; ---------------------------------------------------------------------------
;; Collection-type inference (jolt-99x), Phase 0: intra-procedural. A forward,
;; soft-typing-style pass (simplified HM: monovariant, never-fails, lattice top
;; = :any) that types expressions from literals/arithmetic and flows the type
;; through let bindings and if-joins. Where a keyword-lookup subject is PROVEN a
;; plain struct map it sets :hint :struct (the same channel a manual hint uses,
;; so the back end drops the guard); where the type is :any it leaves the
;; dynamic guard in place. Sound by construction: a concrete type is assigned
;; only when proven, so a wrong bare get is impossible.
;;
;; Recursive STRUCTURAL types (RFC 0005). A type mirrors the data tree:
;;   compound: {:struct {field -> T}}  (raw-get-safe map, field types)
;;             {:vec T}                (vector of T)
;;             {:set T}                (set of T)
;;   scalar:   :num :str :kw :truthy   (all provably non-nil/non-false)
;;             :phm                    (persistent hash map; NOT raw-get-safe)
;;   :any (top), nil (bottom, identity for join).
;; Compound types are small jolt maps, so they compare by value on both the
;; Clojure and the Janet (orchestrator) side. struct/vec/set use distinct keys so
;; a type is recognised by which key it carries.
;; (get t :KEY) is nil for a keyword type and the child for a compound, so a
;; compound is detected by some? — no map?/contains? needed.
(defn- velem [t] (get t :vec))
(defn- selem [t] (get t :set))
(defn- sfields [t] (get t :struct))
(defn- vec-type? [t] (some? (velem t)))
(defn- set-type? [t] (some? (selem t)))
(defn- struct-type? [t] (some? (sfields t)))
(defn- mk-vec [t] {:vec (if t t :any)})
(defn- mk-set [t] {:set (if t t :any)})
(defn- mk-struct [fs] {:struct fs})

;; Bounded union types (RFC 0006 / jolt-pz5). A union {:union #{T...}} records
;; that a value is provably one of a small, fixed set of SCALAR types — what
;; differing if-branches used to collapse to :any. It exists so the success
;; checker can reject a use where EVERY member is in the op's error domain
;; ((inc (if c "a" :k))) while still accepting one where any member is valid
;; ((inc (if c 1 "x"))). Scalars only, capped cardinality: the member space is
;; the five scalar tags, so the lattice stays finite and the inter-procedural
;; fixpoint terminates. A union is opaque to every STRUCTURAL predicate
;; (struct-type?/vec-type?/set-type? key on :struct/:vec/:set, which a union
;; lacks), so specialization treats it exactly like :any — codegen is
;; unchanged; only the checker reads inside it.
(def ^:private union-cap 4)
(defn- scalar-t? [t] (or (= t :num) (= t :str) (= t :kw) (= t :truthy) (= t :phm)))
(defn- union-type? [t] (some? (get t :union)))
(defn- umembers [t] (get t :union))
(defn- union-of
  "Normalize a seq of member types into a lattice value: flatten nested unions,
  keep only scalars (any non-scalar member collapses the whole thing to :any,
  the conservative top), then return the lone member if one, {:union #{...}}
  for 2..cap distinct scalars, or :any past the cap."
  [ts]
  (let [flat (reduce (fn [acc t]
                       (if (union-type? t)
                         (reduce conj acc (umembers t))
                         (conj acc t)))
                     #{} ts)]
    (cond
      (not (every? scalar-t? flat)) :any
      (= 0 (count flat)) :any
      (= 1 (count flat)) (first flat)
      (> (count flat) union-cap) :any
      :else {:union flat})))

(declare join-t)
(defn- merge-fields
  "Per-field join of two field maps (a key in only one side joins with :any)."
  [fa fb]
  (let [m1 (reduce (fn [m k] (assoc m k (join-t (get fa k :any) (get fb k :any)))) {} (keys fa))]
    (reduce (fn [m k] (if (get m k) m (assoc m k (join-t (get fa k :any) (get fb k :any))))) m1 (keys fb))))
(defn- join-t [a b]
  (cond
    (= a b) a
    (nil? a) b
    (nil? b) a
    (and (struct-type? a) (struct-type? b))
      (let [merged (mk-struct (merge-fields (sfields a) (sfields b)))]
        ;; joining two values of the SAME complete shape preserves it — the
        ;; merged struct has the same key set (jolt-t34 R2). Different shapes
        ;; (or an incomplete side) drop it, as the layout is no longer proven.
        (if (and (get a :shape) (= (get a :shape) (get b :shape)))
          (assoc merged :shape (get a :shape))
          merged))
    (and (vec-type? a) (vec-type? b)) (mk-vec (join-t (velem a) (velem b)))
    (and (set-type? a) (set-type? b)) (mk-set (join-t (selem a) (selem b)))
    ;; differing kinds: form a scalar union when both sides reduce to scalars
    ;; (or scalar unions); anything compound on either side stays :any (jolt-pz5)
    :else (let [ma (cond (union-type? a) (umembers a) (scalar-t? a) #{a} :else nil)
                mb (cond (union-type? b) (umembers b) (scalar-t? b) #{b} :else nil)]
            (if (and ma mb) (union-of (reduce conj ma mb)) :any))))
(defn- join [a b] (join-t a b))
;; depth cap (RFC 0005): truncate a type below depth d to :any, so recursive data
;; can't make an infinite type and the inter-procedural fixpoint stays finite.
(def ^:private type-depth 4)
(defn- cap [t d]
  (cond
    (<= d 0) (if (or (struct-type? t) (vec-type? t) (set-type? t)) :any t)
    (struct-type? t)
      ;; capping truncates VALUES below depth d, but the KEY SET is unchanged, so
      ;; a complete :shape survives — keep it so nested/container field reads can
      ;; still bare-index (jolt-t34 R2). cap recurses into fields, so a nested
      ;; shaped value (a vec3 inside a hit-info) keeps its own :shape too.
      (let [capped (mk-struct (reduce (fn [m k] (assoc m k (cap (get (sfields t) k) (dec d))))
                                      {} (keys (sfields t))))
            ;; the record :type tag (and :shape) are independent of field-value
            ;; depth, so they survive truncation — a record read from a deep
            ;; container keeps its identity, so devirtualization (jolt-41m),
            ;; record? folding, and the record fast path still fire on it.
            capped (if (get t :shape) (assoc capped :shape (get t :shape)) capped)
            capped (if (get t :type) (assoc capped :type (get t :type)) capped)]
        capped)
    (vec-type? t) (mk-vec (cap (velem t) (dec d)))
    (set-type? t) (mk-set (cap (selem t) (dec d)))
    :else t))
;; raw-get-safe (a Janet struct / record): a struct type. The field type of key
;; k, if known, else :any.
(defn- struct-safe? [t] (struct-type? t))
(defn- field-type [t k] (if (struct-type? t) (get (sfields t) k :any) :any))
;; Shape (hidden class, jolt-t34). A struct type built from a map LITERAL carries
;; its complete layout — :shape, the canonical (str-sorted) key vector. The back
;; end represents such a map as a shape tuple and reads a field by bare index.
;; A struct type from a JOIN or from field-access inference has no :shape
;; (incomplete: the full key set isn't proven), so it keeps the dynamic path —
;; never a bare index. No shape is hardcoded; any constant key set is one.
(defn- shape-order
  "Canonical key order for a shape: keys sorted by their string form, so two
  literals with the same keys in any order intern to the same shape."
  [ks] (vec (sort (fn [a b] (compare (str a) (str b))) ks)))
(defn- type-shape [t] (get t :shape))
;; tag a node (any expression, not just a :local) so the back end can specialize
;; a lookup whose SUBJECT is that node — this is what makes nested access work:
;; (:direction ray) is tagged struct, so (:r (:direction ray)) drops its guard.
;; tag a lookup subject as a struct, carrying the complete shape when known
;; (so the back end bare-indexes) — jolt-t34
(defn- mark-struct [node t]
  (let [n (assoc node :hint :struct)]
    (if (get t :shape) (assoc n :shape (get t :shape)) n)))
;; a value provably neither nil nor false — the back end only builds a struct
;; (vs a phm) when every value is non-nil/non-false, so a map literal is a struct
;; only when all its values have such a type. Collections are non-nil.
(defn- truthy-type? [t]
  (or (= t :num) (= t :str) (= t :kw) (= t :truthy) (= t :phm)
      (struct-type? t) (vec-type? t) (set-type? t)))

;; core fns whose result is a number (so it is non-nil/non-false and, for the
;; success-type checker, provably numeric).
(def ^:private num-ret-fns
  #{"+" "-" "*" "/" "inc" "dec" "mod" "rem" "quot" "min" "max" "abs"
    "bit-and" "bit-or" "bit-xor" "count"})
(def ^:private vector-ret-fns #{"vec" "vector" "mapv" "filterv" "subvec"})

;; Inter-procedural state (jolt-767, Phase 1). The Janet orchestrator (backend
;; infer-unit!) drives a whole-unit fixpoint: before typing a fn body it installs
;; the current return-type estimates of all unit fns here, and after typing it
;; reads back the call sites this body made (callee + inferred arg types) to
;; propagate into callee param types. Both are plain module state, like `dirty`.
(def ^:private rtenv-box (atom {}))   ;; "ns/name" -> inferred return type
(def ^:private calls-box (atom []))   ;; collected [ "ns/name" [arg-types...] ]
(def ^:private escapes-box (atom #{})) ;; var-keys used as a VALUE (not a call head)
(def ^:private diag-box (atom []))    ;; success-type-check diagnostics (RFC 0006)
;; jolt-d6u: a var reference's VALUE type — a fn var is :truthy (non-nil), a def
;; var carries its inferred init type (e.g. a color table -> {:vec :struct-map}).
;; The orchestrator populates this from sealed (opt-mode) cell roots + def inits.
(def ^:private vtype-box (atom {}))   ;; "ns/name" -> value type

;; User-function error domains (jolt-zo1), opt-in. As the checker walks defs it
;; registers each non-redefinable single-fixed-arity user fn's {:params :body}
;; here, keyed "ns/name". At a later call site (strict mode only) the body is
;; re-checked with ONE parameter bound to its concrete argument type — if that
;; alone produces a diagnostic the all-:any body did not, that argument is
;; provably wrong and the CALL is reported. Module state, like rtenv-box: a def
;; must precede its call (the same closed-world ordering RFC 0005 assumes).
(def ^:private user-sig-box (atom {}))      ;; "ns/name" -> {:params [..] :body ir}
;; jolt-t34: a record constructor's return shape. "ns/->Name" -> [field-kw ...]
;; in DECLARED order (the runtime lays records out in declared field order, so
;; the back end bare-indexes by that order). A call (->Point a b) types as a
;; struct of this shape, so field reads on the result bare-index — declared
;; shapes are clean fuel: a lookup, not fragile inference.
(def ^:private record-shapes-box (atom {}))
;; jolt-41m: protocol-method registry "ns/method" -> [proto method], for
;; devirtualizing a protocol call whose receiver is a known record type.
(def ^:private protocol-methods-box (atom {}))

;; jolt-3ko: build a record's struct TYPE from its registry entry, resolving each
;; field's declared type hint. A field tagged with a record type (its ctor-key)
;; recurses, so a Vec3 stored in a Ray field reads back as Vec3 — not :any —
;; which is what lets nested-record code prove its reads. Depth-bounded so a
;; self/cyclic-referencing record type can't loop.
(declare record-type-from-entry)
(defn- field-type-from-tag [tag depth]
  (cond
    (or (nil? tag) (<= depth 0)) :any
    (= tag "num") :num
    :else (let [e (get @record-shapes-box tag)]
            (if e (record-type-from-entry e depth) :any))))
(defn- record-type-from-entry [rs depth]
  (let [fields (get rs :fields)
        tags (get rs :tags)
        fmap (reduce (fn [m i]
                       (assoc m (nth fields i)
                              (field-type-from-tag (when tags (nth tags i)) (dec depth))))
                     {} (range (count fields)))]
    (assoc (mk-struct fmap) :shape (vec fields) :type (get rs :type))))
;; jolt-t34: whether to shape generic const-key MAP literals (opt-in, JOLT_SHAPE).
;; Records are shaped regardless; maps only when this is on.
(def ^:private map-shapes-box (atom false))
(def ^:private checking-box (atom #{}))     ;; keys mid-recheck — cycle guard
(def ^:private strict-box (atom false))     ;; report against user-fn domains?
;; When true, `infer` emits success-type diagnostics as it types (jolt audit).
;; The checker IS the inference walk now — one O(n) pass that both types and
;; checks, instead of a separate check-walk that re-inferred every subtree
;; (quadratic in nesting). Off during the optimization fixpoint so it doesn't
;; emit intermediate diagnostics; on only inside check-form.
(def ^:private checking? (atom false))

;; fns that RETURN an element of their (first) collection arg, so a lookup on the
;; result of (rand-nth coll-of-structs) etc. types as the element.
(def ^:private elem-fns #{"rand-nth" "first" "peek" "last" "nth" "fnext" "second"})

;; the checker's emission points, defined after infer but referenced from it
(declare check-invoke check-user-call register-user-fn! not-callable? type-name)

(defn- var-key [fnode] (str (get fnode :ns) "/" (get fnode :name)))

(defn- call-ret-type [fnode]
  (let [op (get fnode :op)]
    (cond
      ;; a user fn whose return type the fixpoint has estimated
      (= op :var) (let [rs (get @record-shapes-box (var-key fnode))]
                    (if rs
                      ;; record ctor -> struct of declared shape (jolt-t34); :shape
                      ;; is the DECLARED field order the back end indexes by, :type
                      ;; the record tag (devirt), and field types come from the
                      ;; declared hints so nested records stay typed (jolt-3ko)
                      (record-type-from-entry rs type-depth)
                      (let [r (get @rtenv-box (var-key fnode))]
                        (if r r (let [nm (and (= "clojure.core" (get fnode :ns)) (get fnode :name))]
                                  (cond (nil? nm) :any
                                        (contains? num-ret-fns nm) :num
                                        (contains? vector-ret-fns nm) (mk-vec :any)
                                        :else :any))))))
      (= op :host) (let [nm (get fnode :name)]
                     (cond (contains? num-ret-fns nm) :num
                           (contains? vector-ret-fns nm) (mk-vec :any)
                           :else :any))
      :else :any)))

;; Predicate folding (jolt-wcw): a type predicate whose argument's type is
;; PROVEN folds to a compile-time boolean. Only the precise tags are folded —
;; :num/:str/:kw mean exactly that scalar, and a record carries its defrecord
;; :type tag. NOT folded: vector?/set?/map?, because the :vec tag conflates a
;; real vector with a range/seq (so vector? could be wrong) — left for when the
;; lattice distinguishes them. :any and :truthy carry no structural info (a
;; value known only non-nil could be any concrete type), so every predicate is
;; unknown on them. nil?/some? fold because every concrete type here is
;; provably non-nil.
(def ^:private fold-preds
  #{"number?" "string?" "keyword?" "record?" "nil?" "some?"})
(defn- record-t? [t] (and (struct-type? t) (some? (get t :type))))
(defn- pred-on [pname t]
  (cond
    (or (= t :any) (= t :truthy)) nil
    ;; a bounded scalar union folds only when every member agrees
    (union-type? t)
    (let [vs (map (fn [m] (pred-on pname m)) (umembers t))]
      (if (and (seq vs) (not (nil? (first vs))) (apply = vs)) (first vs) nil))
    :else
    (case pname
      "number?"  (= t :num)
      "string?"  (= t :str)
      "keyword?" (= t :kw)
      "record?"  (record-t? t)
      "nil?"     false
      "some?"    true
      nil)))
;; Side-effect-free node whose evaluation can be dropped when its predicate
;; folds away (a wider purity analysis can broaden this later).
(defn- pure-node? [n] (let [op (get n :op)] (or (= op :const) (= op :local))))

(declare infer)

;; HOFs that apply their fn arg to the ELEMENTS of a collection (jolt-d6u,
;; Phase 3). :epos is which param of the fn receives an element. reduce is
;; handled separately (its arity changes the coll position, and its closure
;; also takes an accumulator).
(def ^:private hof-table
  {"map" {:epos 0} "mapv" {:epos 0} "filter" {:epos 0} "filterv" {:epos 0}
   "keep" {:epos 0} "remove" {:epos 0} "run!" {:epos 0} "mapcat" {:epos 0}})

(defn- infer-fn-seeded
  "Infer a fn-literal passed to a HOF, seeding the given params to element/accum
  types (seeds: param-index -> type), other params :any, captured locals from
  tenv. Returns [ret-type node'] — ret is the lub of arity tail types, used to
  type the HOF result (e.g. reduce's accumulator, mapv's element)."
  [node seeds tenv]
  (let [res (mapv (fn [a]
                    (let [params (get a :params)
                          pe (reduce (fn [e i]
                                       (assoc e (nth params i)
                                              (let [s (get seeds i)] (if s s :any))))
                                     tenv (range (count params)))
                          pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)
                          br (infer (get a :body) pe)]
                      [(nth br 0) (assoc a :body (nth br 1))]))
                  (get node :arities))
        rets (mapv (fn [r] (nth r 0)) res)
        ret (if (empty? rets) :any (reduce join (first rets) (rest rets)))]
    [ret (assoc node :arities (mapv (fn [r] (nth r 1)) res))]))

(defn- infer
  "Returns [type node'] — the inferred type of node and node with struct-safe
  :local references annotated :hint :struct. tenv maps in-scope local names to
  inferred types."
  [node tenv]
  (let [op (get node :op)]
    (cond
      (= op :const)
      [(let [v (get node :val)]
         (cond (number? v) :num
               (string? v) :str
               (keyword? v) :kw
               (or (nil? v) (= false v)) :any   ; nil/false are not struct-eligible
               :else :truthy))                  ; true, char, ... -> non-nil
       node]
      (= op :local)
      (let [t (get tenv (get node :name))]
        [(if t t :any)
         (cond
           (struct-safe? t) (let [n (assoc node :hint :struct)]
                              (if (type-shape t) (assoc n :shape (type-shape t)) n))
           (vec-type? t) (assoc node :hint :vector)
           :else node)])
      (= op :map)
      (let [pairs (get node :pairs)
            res (mapv (fn [pr]
                        (let [kr (infer (nth pr 0) tenv)
                              vr (infer (nth pr 1) tenv)]
                          [(nth kr 1) (nth vr 1) (nth vr 0) (get (nth pr 0) :val)]))
                      pairs)
            struct? (and (> (count res) 0)
                         (every? (fn [pr] (scalar-const? (nth pr 0))) pairs)
                         (every? (fn [r] (truthy-type? (nth r 2))) res))
            base (when struct?
                   (cap (mk-struct (reduce (fn [m r] (assoc m (nth r 3) (nth r 2))) {} res)) type-depth))
            ;; a literal is a COMPLETE shape: carry its sorted key vector so the
            ;; back end can lay it out and bare-index lookups (jolt-t34)
            shp (when (and @map-shapes-box base (struct-type? base)) (shape-order (keys (sfields base))))
            t (if base (if shp (assoc base :shape shp) base) :any)
            node' (assoc node :pairs (mapv (fn [r] [(nth r 0) (nth r 1)]) res))]
        [t (if shp (assoc node' :shape shp) node')])
      (= op :vector)
      (let [irs (mapv (fn [x] (infer x tenv)) (get node :items))
            ets (mapv (fn [r] (nth r 0)) irs)
            el (if (empty? ets) :any (reduce join (first ets) (rest ets)))]
        [(cap (mk-vec el) type-depth) (assoc node :items (mapv (fn [r] (nth r 1)) irs))])
      (= op :set)
      (let [irs (mapv (fn [x] (infer x tenv)) (get node :items))
            ets (mapv (fn [r] (nth r 0)) irs)
            el (if (empty? ets) :any (reduce join (first ets) (rest ets)))]
        [(cap (mk-set el) type-depth) (assoc node :items (mapv (fn [r] (nth r 1)) irs))])
      (= op :if)
      (let [tr (infer (get node :test) tenv)
            thn (infer (get node :then) tenv)
            els (infer (get node :else) tenv)]
        [(join (nth thn 0) (nth els 0))
         (assoc node :test (nth tr 1) :then (nth thn 1) :else (nth els 1))])
      (= op :do)
      (let [stmts (mapv (fn [s] (nth (infer s tenv) 1)) (get node :statements))
            r (infer (get node :ret) tenv)]
        [(nth r 0) (assoc node :statements stmts :ret (nth r 1))])
      (= op :throw)
      [:any (assoc node :expr (nth (infer (get node :expr) tenv) 1))]
      ;; a :var reached HERE is in value position (an arg, a let init, ...), not
      ;; a call head — so the fn it names escapes and its params can't be inferred.
      ;; Its VALUE type comes from vtype-box (a fn is :truthy, a def carries its
      ;; inferred type); unknown -> :any.
      (= op :var) (do (swap! escapes-box conj (var-key node))
                      [(let [vt (get @vtype-box (var-key node))] (if vt vt :any)) node])
      (= op :invoke)
      (let [fnode (get node :fn)
            iscall-var (= :var (get fnode :op))
            cn (when (and iscall-var (= "clojure.core" (get fnode :ns))) (get fnode :name))
            args (get node :args)
            n (count args)]
        (cond
          ;; predicate folding (jolt-wcw): a type predicate over a single,
          ;; side-effect-free argument whose type PROVES the answer becomes a
          ;; boolean constant — eliminating the call, and (once const-fold runs
          ;; after inference) collapsing any `if` it gates. Falls through to the
          ;; normal call path when the answer isn't provable or the arg is impure.
          (and iscall-var (contains? fold-preds cn) (= n 1))
          (let [ar (infer (nth args 0) tenv)
                v (pred-on cn (nth ar 0))]
            (if (and (not (nil? v)) (pure-node? (nth ar 1)))
              [:any {:op :const :val v}]
              [(call-ret-type fnode) (assoc node :args [(nth ar 1)])]))
          ;; (:k m) / (:k m default): the result is m's field type, and if m is a
          ;; struct the subject is tagged so the back end drops the guard — this
          ;; types nested access end to end (RFC 0005).
          (and (= :const (get fnode :op)) (keyword? (get fnode :val)) (>= n 1) (<= n 2))
          (let [mr (infer (nth args 0) tenv)
                mt (nth mr 0)
                msub (if (struct-safe? mt) (mark-struct (nth mr 1) mt) (nth mr 1))
                ft (field-type mt (get fnode :val))
                dr (when (= n 2) (infer (nth args 1) tenv))]
            [(if dr (join ft (nth dr 0)) ft)
             (assoc node :args (if dr [msub (nth dr 1)] [msub]))])
          ;; (get m :k [default]): same, when the key is a constant keyword.
          (and (or (and (= :var (get fnode :op)) (= "clojure.core" (get fnode :ns)) (= "get" (get fnode :name)))
                   (and (= :host (get fnode :op)) (= "get" (get fnode :name))))
               (>= n 2) (= :const (get (nth args 1) :op)) (keyword? (get (nth args 1) :val)))
          (let [mr (infer (nth args 0) tenv)
                mt (nth mr 0)
                msub (if (struct-safe? mt) (mark-struct (nth mr 1) mt) (nth mr 1))
                kr (infer (nth args 1) tenv)
                ft (field-type mt (get (nth args 1) :val))
                dr (when (= n 3) (infer (nth args 2) tenv))]
            [(if dr (join ft (nth dr 0)) ft)
             (assoc node :args (if dr [msub (nth kr 1) (nth dr 1)] [msub (nth kr 1)]))])
          ;; reduce over a typed vector with a fn-literal (jolt-d6u): seed the
          ;; closure's accumulator (param 0) to the init type and its element
          ;; (param 1) to the vector's element type, so its body — and any calls
          ;; it makes — see those types.
          (and (= cn "reduce") (>= n 2) (= :fn (get (nth args 0) :op)))
          (let [three (>= n 3)
                coll-r (infer (nth args (if three 2 1)) tenv)
                init-r (when three (infer (nth args 1) tenv))
                et (let [ct (nth coll-r 0)] (if (vec-type? ct) (velem ct) :any))
                init-t (if init-r (nth init-r 0) :any)
                fn-r (infer-fn-seeded (nth args 0) {0 init-t 1 et} tenv)]
            [(join init-t (nth fn-r 0))
             (assoc node :args (if three
                                 [(nth fn-r 1) (nth init-r 1) (nth coll-r 1)]
                                 [(nth fn-r 1) (nth coll-r 1)]))])
          ;; map/mapv/filter/... over a typed vector with a fn-literal: seed the
          ;; fn's element param; mapv/filterv produce a typed vector.
          (and cn (get hof-table cn) (>= n 2) (= :fn (get (nth args 0) :op)))
          (let [coll-r (infer (nth args 1) tenv)
                et (let [ct (nth coll-r 0)] (if (vec-type? ct) (velem ct) :any))
                fn-r (infer-fn-seeded (nth args 0) {(get (get hof-table cn) :epos) et} tenv)
                rt (cond (= cn "mapv") (mk-vec (nth fn-r 0))
                         (= cn "filterv") (mk-vec et)
                         :else :any)]
            [rt (assoc node :args [(nth fn-r 1) (nth coll-r 1)])])
          ;; conj/into: track the element type of a vector being grown.
          (and (or (= cn "conj") (= cn "into")) (>= n 1))
          (let [ares (mapv (fn [a] (infer a tenv)) args)
                base (nth (nth ares 0) 0)
                rest-ts (mapv (fn [r] (nth r 0)) (rest ares))
                rt (cond
                     (and (= cn "conj") (vec-type? base))
                     (mk-vec (reduce join (velem base) rest-ts))
                     (and (= cn "into") (vec-type? base) (= 2 n) (vec-type? (nth rest-ts 0)))
                     (mk-vec (join (velem base) (velem (nth rest-ts 0))))
                     :else (call-ret-type fnode))]
            [rt (assoc node :args (mapv (fn [r] (nth r 1)) ares))])
          ;; everything else: type args, collect the call (var callee), use the
          ;; declared/estimated return type. range produces a numeric vector.
          :else
          (let [fr (when (not iscall-var) (infer fnode tenv))
                fnode' (if iscall-var fnode (nth fr 1))
                ;; the callee's value type: a var's from vtype-box (a fn is
                ;; :truthy, a def carries its inferred type), else the inferred
                ;; type of the callee expression (jolt-wwy)
                callee-t (if iscall-var (get @vtype-box (var-key fnode)) (nth fr 0))
                ares (mapv (fn [a] (infer a tenv)) args)]
            (when iscall-var
              (swap! calls-box conj [(var-key fnode) (mapv (fn [r] (nth r 0)) ares)]))
            ;; success-type check at this call, reusing the arg types just
            ;; computed (jolt audit): core error domains always, user-fn domains
            ;; in strict mode. The arg subtrees are inferred exactly once.
            (when @checking?
              (let [ats (mapv (fn [r] (nth r 0)) ares) pos (get node :pos)]
                (when cn (check-invoke cn args ats pos))
                ;; calling a provably non-function (jolt-wwy)
                (when (not-callable? callee-t)
                  (swap! diag-box conj
                         {:op :call :type (type-name callee-t) :pos pos
                          :msg (str "cannot call " (type-name callee-t) " as a function")}))
                (when (and @strict-box iscall-var)
                  (let [k (var-key fnode) usig (get @user-sig-box k)]
                    (when usig (check-user-call k usig ats pos))))))
            ;; devirtualization (jolt-41m): a protocol-method call whose receiver
            ;; (arg 0) is a known record type resolves to a direct method call.
            ;; Annotate the node with [type-tag proto method]; the back end looks
            ;; up the impl at emit time and calls it directly, skipping the
            ;; registry dispatch (~19x cheaper than protocol-dispatch).
            (let [pm (and iscall-var (get @protocol-methods-box (var-key fnode)))
                  rtype (when (and pm (pos? n)) (get (nth (nth ares 0) 0) :type))
                  base (assoc node :fn fnode' :args (mapv (fn [r] (nth r 1)) ares))]
              [(cond
                 (= cn "range") (mk-vec :num)
                 ;; element-returning fn over a typed vector -> the element type
                 (and cn (contains? elem-fns cn) (> n 0))
                 (let [a0 (nth (nth ares 0) 0)] (if (vec-type? a0) (velem a0) :any))
                 :else (call-ret-type fnode))
               (if rtype
                 (assoc base :devirt-type rtype :devirt-proto (nth pm 0) :devirt-method (nth pm 1))
                 base)]))))
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [te (nth acc 0) binds (nth acc 1)
                                ir (infer (nth b 1) te)]
                            [(assoc te (nth b 0) (nth ir 0)) (conj binds [(nth b 0) (nth ir 1)])]))
                        [tenv []] (get node :bindings))
            br (infer (get node :body) (nth res 0))]
        [(nth br 0) (assoc node :bindings (nth res 1) :body (nth br 1))])
      (= op :loop)
      ;; conservative + sound: loop bindings join across recur, which we don't
      ;; track in Phase 0, so they stay :any. Still descend to annotate any
      ;; known-type lookups inside the body.
      [:any (assoc node
                   :bindings (mapv (fn [b] [(nth b 0) (nth (infer (nth b 1) tenv) 1)]) (get node :bindings))
                   :body (nth (infer (get node :body) tenv) 1))]
      (= op :recur)
      [:any (assoc node :args (mapv (fn [a] (nth (infer a tenv) 1)) (get node :args)))]
      (= op :fn)
      ;; a closure inherits the enclosing tenv so CAPTURED locals keep their
      ;; types (e.g. a reduce closure that calls (f captured-struct ...)); its own
      ;; params/rest shadow to :any (unknown until Phase 1 types them via callers).
      [:any (assoc node :arities
                   (mapv (fn [a]
                           (let [pe (reduce (fn [e p] (assoc e p :any)) tenv (get a :params))
                                 pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)]
                             (assoc a :body (nth (infer (get a :body) pe) 1))))
                         (get node :arities)))]
      (= op :def)
      (do (when @checking? (register-user-fn! node))
          [:any (assoc node :init (nth (infer (get node :init) tenv) 1))])
      (= op :try)
      [:any (assoc node
                   :body (nth (infer (get node :body) tenv) 1)
                   :catch-body (when (get node :catch-body) (nth (infer (get node :catch-body) tenv) 1))
                   :finally (when (get node :finally) (nth (infer (get node :finally) tenv) 1)))]
      :else [:any node])))

(defn- infer-top [node] (nth (infer node {}) 1))

;; ---------------------------------------------------------------------------
;; Success-type checking (RFC 0006). Reuse the inference above as a loose type
;; checker: flag a core-fn call ONLY when an argument's inferred type is
;; concrete AND lies in that op's error domain (the op provably throws on it).
;; Everything ambiguous — :any, :truthy (true/char/...), :nil — is accepted, so
;; there are no false positives. The table is curated to genuinely-throwing
;; cases; lenient ops ((get 5 :k) -> nil, (:k 5) -> nil) are NOT listed.

;; concrete non-numbers: arithmetic provably throws on these. A union is in the
;; error domain only when EVERY member is (jolt-pz5) — if any member is an
;; accepted type the call is accepted (no false positive).
(defn- not-number? [t]
  (if (union-type? t)
    (every? not-number? (umembers t))
    (or (= t :str) (= t :kw) (= t :phm)
        (struct-type? t) (vec-type? t) (set-type? t))))

;; concrete non-seqable scalars: seq/count/first/nth provably throw on these.
;; (Strings and collections ARE seqable/countable; :truthy is ambiguous; :nil
;; and :any are accepted.) A union throws only when every member does.
(defn- not-seqable? [t]
  (if (union-type? t)
    (every? not-seqable? (umembers t))
    (or (= t :num) (= t :kw))))

;; concrete non-callable values (jolt-wwy): calling them throws "Cannot call X
;; as a function". Only :num and :str — keywords/maps/vectors/sets are IFn,
;; :truthy/:any/:nil are ambiguous (accepted). A union is non-callable only when
;; every member is.
(defn- not-callable? [t]
  (if (union-type? t)
    (every? not-callable? (umembers t))
    (or (= t :num) (= t :str))))

;; arithmetic / numeric ops: EVERY argument must be a number.
(def ^:private num-ops
  #{"+" "-" "*" "/" "inc" "dec" "mod" "rem" "quot" "min" "max" "abs"
    "bit-and" "bit-or" "bit-xor" "bit-not" "bit-shift-left" "bit-shift-right"})
;; seq/count/index ops: argument 0 must be seqable/countable.
(def ^:private seq-ops #{"count" "first" "rest" "next" "seq" "nth"})

(defn- type-name
  "Render an inferred type for an error message."
  [t]
  (cond (union-type? t)
          (reduce (fn [s m] (if (= s "") (type-name m) (str s " or " (type-name m))))
                  "" (umembers t))
        (struct-type? t) "a map"
        (vec-type? t) "a vector"
        (set-type? t) "a set"
        (= t :str) "a string"
        (= t :kw) "a keyword"
        (= t :num) "a number"
        (= t :phm) "a map"
        :else (str t)))

(defn- check-invoke
  "If node is a core-op call whose argument type is provably in the error domain,
  conj a diagnostic. arg-types is the vector of inferred argument types; pos is
  the call form's source offset (jolt-fqy), carried into each diagnostic."
  [cn args arg-types pos]
  (cond
    (contains? num-ops cn)
    (reduce (fn [_ i]
              (let [t (nth arg-types i)]
                (when (not-number? t)
                  (swap! diag-box conj
                         {:op cn :argpos i :type (type-name t) :pos pos
                          :msg (str "`" cn "` requires a number, but argument "
                                    (inc i) " is " (type-name t))})))
              nil)
            nil (range (count args)))
    (and (contains? seq-ops cn) (> (count args) 0))
    (let [t (nth arg-types 0)]
      (when (not-seqable? t)
        (swap! diag-box conj
               {:op cn :argpos 0 :type (type-name t) :pos pos
                :msg (str "`" cn "` requires "
                          (if (= cn "count") "a countable collection" "a seqable")
                          ", but argument 1 is " (type-name t))})))
    :else nil))

;; --- user-function error domains (jolt-zo1), opt-in --------------------------
(defn- all-any-env
  "tenv binding every param name to :any (the all-ambiguous baseline)."
  [params]
  (reduce (fn [e p] (assoc e p :any)) {} params))

(defn- isolated-diag-count
  "Count of diagnostics typing body under tenv produces, with the shared
  diag-box saved and restored so this probe never leaks into the real report.
  Runs the same checking inference as check-form (checking? is already on)."
  [body tenv]
  (let [saved @diag-box]
    (reset! diag-box [])
    (infer body tenv)
    (let [n (count @diag-box)]
      (reset! diag-box saved)
      n)))

(defn- register-user-fn!
  "Record a (def name (fn [params] body)) — single fixed arity, not redefinable —
  for later user-fn call checking. Redefinable/dynamic and multi/variadic fns are
  skipped (their body is not a stable requirement)."
  [node]
  (let [init (get node :init)
        m (get node :meta)
        redefable (and m (or (get m :redef) (get m :dynamic)))]
    (when (and (not redefable) (= :fn (get init :op)))
      (let [arities (get init :arities)]
        (when (= 1 (count arities))
          (let [ar (first arities)]
            (when (not (get ar :rest))
              (swap! user-sig-box assoc
                     (str (get node :ns) "/" (get node :name))
                     {:name (get node :name)
                      :params (get ar :params) :body (get ar :body)}))))))))

(defn- check-user-call
  "Strict mode: report a call to a registered user fn that provably throws —
  either a WRONG ARITY (the registered fn has one fixed arity, so a different
  arg count always throws, jolt-wwy) or an argument whose concrete type the body
  rejects. For the latter, re-check the body with ONLY that parameter bound to
  its arg type (others :any); a diagnostic the all-:any body did not already
  have means the argument alone is provably wrong. Monotonic — binding a
  concrete type can only ADD error-domain hits — so no false positive.
  Cycle-guarded so mutually recursive fns terminate."
  [key sig arg-types pos]
  (when (not (contains? @checking-box key))
    (let [prev @checking-box]
      (reset! checking-box (conj prev key))
      (let [params (:params sig)
            body (:body sig)
            npar (count params)
            nargs (count arg-types)]
        (if (not= npar nargs)
          ;; arity is provably wrong regardless of types — report and stop (the
          ;; per-arg type re-check would bind params positionally, meaningless
          ;; under a mismatch)
          (swap! diag-box conj
                 {:op :user-call :type :arity :pos pos
                  :msg (str "wrong number of args (" nargs ") passed to `"
                            (:name sig) "` (expected " npar ")")})
          (let [base (isolated-diag-count body (all-any-env params))]
            (reduce
              (fn [_ i]
                (let [at (nth arg-types i)]
                  (when (and (not= at :any) (not= at :truthy))
                    (let [pe (assoc (all-any-env params) (nth params i) at)]
                      (when (> (isolated-diag-count body pe) base)
                        (swap! diag-box conj
                               {:op :user-call :argpos i :type (type-name at) :pos pos
                                :msg (str "argument " (inc i) " to `" (:name sig)
                                          "` is " (type-name at)
                                          ", which its body provably rejects")})))))
                nil)
              nil (range npar)))))
      (reset! checking-box prev))))

;; --- Inter-procedural driver API (jolt-767) consumed by the back end --------
(defn set-rtenv!
  "Install the current return-type estimates (a map \"ns/name\" -> type) used to
  type call results during the fixpoint."
  [m] (reset! rtenv-box m))

;; jolt-t34: install record-ctor shapes ("ns/->Name" -> [field-kw ...]) and the
;; map-shaping flag (opt-in JOLT_SHAPE), both read by infer.
(defn set-record-shapes! [m] (reset! record-shapes-box (or m {})))
(defn set-protocol-methods! [m] (reset! protocol-methods-box (or m {})))
(defn set-map-shapes! [b] (reset! map-shapes-box (boolean b)))

(defn set-vtypes!
  "Install var VALUE types (a map \"ns/name\" -> type): fn vars are :truthy
  (non-nil), def vars carry their inferred init type (jolt-d6u)."
  [m] (reset! vtype-box m))

(defn join-types
  "Public structural join (lub), used by the orchestrator's fixpoint so param/
  return types join field-wise/element-wise instead of collapsing to :any."
  [a b] (join-t a b))

(defn reset-escapes! [] (reset! escapes-box #{}))
(defn collected-escapes [] (vec @escapes-box))

(defn check-form
  "Success-type check a single analyzed form (RFC 0006). Returns a vector of
  diagnostics [{:op :argpos :type :msg} ...] for provably-wrong calls; empty
  when nothing is provably wrong. Runs independently of specialization so it is
  usable in normal builds (the decoupled checking path).

  With strict? true, also reports calls to registered user functions whose
  concrete argument types provably make the body throw (jolt-zo1, opt-in,
  closed-world). user-sig-box accumulates registered defs across forms, so a
  def must precede its call — the same ordering RFC 0005 already assumes."
  ([node] (check-form node false))
  ([node strict?]
   (reset! strict-box (if strict? true false))
   (reset! checking-box #{})
   (reset! diag-box [])
   ;; the check IS the inference: one walk that types and emits diagnostics
   ;; (jolt audit). checking? gates emission so the optimization fixpoint, which
   ;; also calls infer, stays silent.
   (reset! checking? true)
   (infer node {})
   (reset! checking? false)
   (reset! strict-box false)
   (vec @diag-box)))

(defn infer-body
  "Type `body` under tenv (local-name -> type). Returns [ret-type node' calls],
  where calls is the [[\"ns/name\" [arg-types...]] ...] this body invokes (for
  propagating into callee param types). Also accumulates escapes (read with
  collected-escapes after a full sweep)."
  [body tenv]
  (reset! calls-box [])
  (let [r (infer body tenv)]
    [(nth r 0) (nth r 1) @calls-box]))

(defn reinfer-def
  "Re-run inference on a stashed :def's fn arity bodies with param types seeded
  (ptmap: param-name -> type), returning the def with annotated bodies. The back
  end emits the result directly (no further passes), so the param-typed lookups
  keep their specialization. Used by the inter-procedural recompile."
  [def-node ptmap]
  (let [fnode (get def-node :init)]
    (if (= :fn (get fnode :op))
      (assoc def-node :init
             (assoc fnode :arities
                    (mapv (fn [a]
                            ;; seed declared record param hints (:phints, name ->
                            ;; ctor-key) so a record param is typed even with no
                            ;; inferred caller type — the open-world / cross-ns
                            ;; case. An inferred type in ptmap wins (it's at least
                            ;; as precise), so this only fills the gaps.
                            (let [pt (reduce (fn [m pr]
                                               (let [nm (nth pr 0)
                                                     e (get @record-shapes-box (nth pr 1))]
                                                 (if (and e (not (contains? m nm)))
                                                   (assoc m nm (record-type-from-entry e type-depth))
                                                   m)))
                                             ptmap (get a :phints))]
                              (assoc a :body (nth (infer (get a :body) pt) 1))))
                          (get fnode :arities))))
      def-node)))

(defn phint-seed
  "Positional declared-hint type seeds for a fn arity. Given the param-name
  vector and the arity's :phints (a seq of [name ctor-key] pairs), return a
  vector parallel to params whose slot i is the resolved record TYPE of that
  param's ^Record hint (via the record-shapes registry), or nil. The
  whole-program fixpoint seeds these as a param-type FLOOR so a declared hint
  propagates to a fn's callees DURING inference — not only at the final re-emit
  (reinfer-def). Without it a hinted param with no callers stays :any through the
  fixpoint, so a field read off it (e.g. (:origin ^Ray r)) never tells a shared
  callee its arg is a Vec3 (jolt-3ko)."
  [params phints]
  (let [m (reduce (fn [acc pr] (assoc acc (nth pr 0) (nth pr 1))) {} phints)]
    (mapv (fn [nm]
            (let [ck (get m nm)
                  e (and ck (get @record-shapes-box ck))]
              (when e (record-type-from-entry e type-depth))))
          params)))

;; Piggyback checking (jolt audit). In direct-link mode infer-top already runs
;; one inference pass for specialization; turning checking? on during it makes
;; the success checker nearly free there (no extra traversal — just the
;; per-call error-domain predicates). The back end sets the mode before
;; run-passes and reads take-diags! after. It checks the POST-optimization IR,
;; which matches what the optimized program actually evaluates (scalar-replace
;; only drops provably-pure code, an accepted opt-mode divergence).
(def ^:private check-mode-box (atom {:on false :strict false}))
(defn set-check-mode!
  "Enable/disable checking during the next run-passes inference (direct-link)."
  [on strict?] (reset! check-mode-box {:on (if on true false) :strict (if strict? true false)}))
(defn take-diags!
  "Diagnostics accumulated by the last checking run-passes; clears the buffer."
  [] (let [d (vec @diag-box)] (reset! diag-box []) d))

(defn run-inference
  "Type-infer the optimized node (the inference walk specializes struct-safe
  lookups). When check mode is on (set-check-mode!), the same walk also emits
  success-type diagnostics into the buffer take-diags! drains afterward. Pulled
  out of run-passes so the checking state stays private to this namespace."
  [opt]
  (if (get @check-mode-box :on)
    (do (reset! diag-box [])
        (reset! checking-box #{})
        (reset! strict-box (get @check-mode-box :strict))
        (reset! checking? true)
        (let [r (infer-top opt)]
          (reset! checking? false)
          (reset! strict-box false)
          r))
    (infer-top opt)))
