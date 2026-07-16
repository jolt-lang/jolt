(ns jolt.passes.types
  "Collection-type inference and success-type checking (RFC 0006).
  A forward, soft-typing pass (simplified HM: monovariant, never-fails, lattice
  top = :any) that types expressions and reuses the same walk as a loose success
  checker. Also the inter-procedural driver API the back end calls to
  propagate param types across a unit / the whole program. Weakly coupled to the
  IR-rewriting passes — shares the const-shape predicates (jolt.passes.fold)."
  (:require [jolt.ir :refer [reduce-ir-children map-ir-children coerce-node]]
            [jolt.passes.fold :refer [scalar-const? kw-callee? get-callee?]]
            [jolt.passes.types.check :refer
             [not-callable? type-name check-invoke register-user-fn!]]
            [jolt.passes.types.lattice :refer
             [velem selem sfields vec-type? set-type? struct-type? mk-vec mk-set
              mk-struct union-cap scalar-t? union-type? umembers union-of merge-fields
              join-t join type-depth cap struct-safe? field-type shape-order type-shape
              mark-struct truthy-type? num-ret-fns vector-ret-fns nilable? strip-nilable]]))

;; --- engine state ------------------------------------------------------------
;; The walk threads an immutable `env` (mk-env) instead of reading scattered
;; module atoms: it carries the read-only config (rtenv/vtypes/record-shapes/
;; protocol-methods/map-shapes?) plus the per-run flags (checking?/strict?) and
;; per-run accumulator/guard CELLS (diags/calls/checking-set/diag-memo). A fresh
;; env per run makes the pass re-entrant — a nested probe (isolated-diag-count)
;; runs under a sub-env with its own diags cell, no save/restore.
;;
;; Only state whose lifecycle spans separate API calls stays module-level: the
;; config the orchestrator installs (set-*! before a sweep), the escapes and
;; user-sig registries (collected/registered across the forms of a sweep), and a
;; bridge holding the last checking run's diagnostics for take-diags!.
(def ^:private config-box
  (atom {:rtenv {}              ;; "ns/name" -> inferred return type
         :vtypes {}             ;; "ns/name" -> var VALUE type (fn=:truthy, def=init type)
         :record-shapes {}      ;; "ns/->Name" -> {:fields :tags :type}
         :protocol-methods {}   ;; "ns/method" -> [proto method]
         :map-shapes? false}))  ;; shape generic const-key maps (opt-in, JOLT_SHAPE)
;; var-keys used as a VALUE (not a call head) — accumulated across a whole sweep,
;; reset by reset-escapes! and read by collected-escapes.
(def ^:private escapes-box (atom #{}))
;; User-function error domains, opt-in. As the checker walks defs it registers
;; each non-redefinable single-fixed-arity user fn's {:params :body} here, keyed
;; "ns/name"; a later call site (strict mode) re-checks the body with one param
;; bound to its concrete argument type. Accumulates ACROSS forms — a def must
;; precede its call (the closed-world ordering RFC 0005 assumes).
(def ^:private user-sig-box (atom {}))    ;; "ns/name" -> {:params [..] :body ir}
;; Diagnostics from the last checking run-inference, for take-diags! to drain.
(def ^:private last-diags-box (atom []))
;; Whether run-inference also checks, and strictly. Set by set-check-mode!.
(def ^:private check-mode-box (atom {:on false :strict false}))
;; "Proto/method" -> the join of its impls' return types, so a protocol-method call
;; types as that record when every impl returns the same one (monomorphic return —
;; e.g. all Scatter impls return a ScatterResult). Set by collect-pm-rets! before
;; the fixpoint, read by call-ret-type. A disagreeing impl widens it to :any.
(def ^:private pm-rets-box (atom {}))

;; build a per-run env: a snapshot of the installed config plus this run's flags
;; and fresh accumulator/guard cells. escapes/user-sigs reference the sweep-level
;; module cells (their lifecycle spans calls); diags/calls/checking-set/diag-memo
;; are this run's own.
(defn- mk-env [checking? strict?]
  (let [c @config-box]
    {:rtenv (get c :rtenv) :vtypes (get c :vtypes)
     :record-shapes (get c :record-shapes) :protocol-methods (get c :protocol-methods)
     :map-shapes? (get c :map-shapes?)
     :checking? checking? :strict? strict?
     :diags (atom []) :calls (atom []) :checking-set (atom #{}) :diag-memo (atom {})
     :escapes escapes-box :user-sigs user-sig-box}))

;; inferred record field types: ctor-key "ns/->Name" -> {field-kw -> type}. Each
;; field's type is the CLOSED join of its ctor-argument types across every reachable
;; (->Ctor ...) site, derived by wp-infer! A field every ctor site fills with a
;; flonum -> :double (reads unbox); a record-or-nil field -> nilable record (guarded
;; reads narrow to the direct accessor); a conflicting/escaping/mutable field is
;; absent here (reads :any). Consulted by record-type-from-entry for UNTAGGED fields.
(def ^:private field-types-box (atom {}))
;; generation counter bumped on every reset! of field-types-box, so
;; record-type-from-entry can cache by [gen ctor-key depth] and flush
;; stale entries when field types change across fixpoint rounds.
(def ^:private field-gen (atom 0))
(def ^:private record-type-cache (atom {}))
;; rich variant: same ctor-arg joins but :num is kept (an integer/mixed join), so a
;; specialized clone built off it sees :num field reads as :double-contagion operands.
;; Populated alongside field-types-box in wp-infer!, but consulted ONLY through
;; *field-type-box* (bound by contagion-specialize-arity) — the shared fixpoint,
;; pm-rets, and the ordinary impl-body path read the lean field-types-box, so they
;; never see :num. That isolation is what keeps Option A's mega-regression fix.
(def ^:private rich-field-types-box (atom {}))
(def ^:dynamic *field-type-box* nil)

;; clone-resolving devirt sites whose contagion clone returns :double. Populated by
;; the whole-program pre-pass (backend contagion-prepass!) AFTER wp-infer! has set
;; rich-field-types-box; empty outside a whole-program build. infer-call consults it
;; at a devirt site to type the call's return :double per-site — so a caller's
;; accumulator add over that site fires dbl-arith? and lowers to fl+ — WITHOUT
;; touching global pm-rets (the leak that sank Option A's :double-everywhere). A
;; PIC/megamorphic site has no :devirt-type, so it never sees this. Sound because the
;; clone's body lowered under the :double-sibling invariant, so its return is a
;; flonum; double-ret? is the clone's inferred return type, not assumed.
(def ^:private clone-double-ret-box (atom #{}))
(defn reset-clone-double-ret! [] (reset! clone-double-ret-box #{}))
(defn add-clone-double-ret! [type-tag proto method]
  (swap! clone-double-ret-box conj (str type-tag "|" proto "|" method)))
(defn- clone-double-ret? [type-tag proto method]
  (contains? @clone-double-ret-box (str type-tag "|" proto "|" method)))

;; per-wp-pass ctor-site collection. collecting-fields? gates the infer-call hook so
;; only the wp fixpoint's final pass collects (not check-form / pm-rets / reinfer).
;; wp-field-joins: ctor-key -> [field-type-or-nil ...] positional arg joins. A field
;; appears when at least one ctor site was seen; absent fields (no site) read :any.
(def ^:private collecting-fields?-box (atom false))
(def ^:private wp-field-joins-box (atom {}))
;; ctor-keys whose join can't be closed (a ctor value escapes — apply/value — or a
;; map->Name site, or an assoc the WP can't attribute to a proven record) -> all
;; fields :any (drop the inferred types for that record).
(def ^:private wp-field-demote-box (atom #{}))

;; simple record name from a ctor-key "ns/->Name".
(defn- ctor-simple-name [ctor-key]
  (let [i (.indexOf ^String ctor-key "/->")]
    (if (>= i 0) (.substring ^String ctor-key (+ i 3)) ctor-key)))
;; defrecord emits a map->Name fn whose body is the (->Name (get m :f) ...) site —
;; the untyped-map ctor path. Its positional ->Name call is excluded from the direct
;; join (a map->Name call demotes the record instead, via wp-field-demote-box), so a
;; never-called map->Name can't poison every field to :any.
(defn- map->-self-for-record? [self-name ctor-key]
  (and self-name (.startsWith ^String self-name "map->")
       (= (.substring ^String self-name 5) (ctor-simple-name ctor-key))))
;; join one ctor site's positional arg types into the per-pass accumulator.
(defn- join-ctor-args [joins ctor-key fields argtypes]
  (let [cur (or (get joins ctor-key) (vec (repeat (count fields) nil)))]
    (assoc joins ctor-key
           (vec (for [i (range (count cur))]
                  (let [c (nth cur i) a (nth argtypes i nil)]
                    (cond (nil? c) a (nil? a) c :else (join c a))))))))

;; build a record's struct TYPE from its registry entry, resolving each field's
;; declared type hint against `shapes` ("ns/->Name" -> entry). A field tagged with
;; a record type (its ctor-key) recurses, so a Vec3 stored in a Ray field reads
;; back as Vec3 — not :any — which is what lets nested-record code prove its reads.
;; Depth-bounded so a self/cyclic-referencing record type can't loop.
(declare record-type-from-entry)
;; the type a field reads back as. A declared coercible ^double hint always wins
;; (the ctor coerces the arg to a flonum). A declared ^Record hint recurses. An
;; UNTAGGED field takes the inferred ctor-arg join from field-types-box (or :any).
(defn- field-type-from-tag [tag depth shapes ctor-key field-kw]
  (cond
    (<= depth 0) :any
    (= tag "double") :double
    (= tag "num") :num
    (some? tag) (let [e (get shapes tag)]
                  (if e (record-type-from-entry e tag depth shapes) :any))
    :else (let [box (or *field-type-box* field-types-box)
                inf (get-in @box [ctor-key field-kw])]
            (if inf inf :any))))
(defn- record-type-from-entry-raw [rs ctor-key depth shapes]
  (let [fields (get rs :fields)
        tags (get rs :tags)
        fmap (reduce (fn [m i]
                       (let [fw (nth fields i)]
                         (assoc m fw (field-type-from-tag (when tags (nth tags i))
                                                           (dec depth) shapes ctor-key fw))))
                     {} (range (count fields)))]
    (assoc (mk-struct fmap) :shape (vec fields) :type (get rs :type))))
(defn- record-type-from-entry [rs ctor-key depth shapes]
  (let [gen @field-gen
        k [gen ctor-key depth]
        cached (get @record-type-cache k)]
    (if (some? cached)
      cached
      (let [v (record-type-from-entry-raw rs ctor-key depth shapes)]
        (swap! record-type-cache assoc k v)
        v))))

;; fns that RETURN an element of their (first) collection arg, so a lookup on the
;; result of (rand-nth coll-of-structs) etc. types as the element.
(def ^:private elem-fns #{"rand-nth" "first" "peek" "last" "nth" "fnext" "second"})

;; defined after infer but referenced from it (the rest of the checker lives in
;; jolt.passes.types.check, required above)
(declare check-user-call)
(declare inline-impl-receiver-type)

(defn- var-key [fnode] (str (get fnode :ns) "/" (get fnode :name)))

(defn- call-ret-type [fnode env]
  (let [op (get fnode :op)
        shapes (get env :record-shapes)]
    (cond
      ;; a user fn whose return type the fixpoint has estimated
      (= op :var) (let [rs (get shapes (var-key fnode))]
                    (if rs
                      ;; record ctor -> struct of declared shape; :shape
                      ;; is the DECLARED field order the back end indexes by, :type
                      ;; the record tag (devirt), and field types come from the
                       ;; declared hints so nested records stay typed
                       (record-type-from-entry rs (var-key fnode) type-depth shapes)
                      (let [r (get (get env :rtenv) (var-key fnode))]
                        (if r r
                          ;; a protocol-method call types as its impls' joined return
                          ;; (monomorphic): so (:ray (scatter m ..)) reads off a Ray.
                          (let [pm (get (get env :protocol-methods) (var-key fnode))
                                pmr (when pm (get @pm-rets-box (str (nth pm 0) "/" (nth pm 1))))]
                            (if (and pmr (not= pmr :any))
                              pmr
                              (let [nm (and (= "clojure.core" (get fnode :ns)) (get fnode :name))]
                                (cond (nil? nm) :any
                                      (contains? num-ret-fns nm) :num
                                      (contains? vector-ret-fns nm) (mk-vec :any)
                                      :else :any))))))))
      (= op :host) (let [nm (get fnode :name)]
                     (cond (contains? num-ret-fns nm) :num
                           (contains? vector-ret-fns nm) (mk-vec :any)
                           :else :any))
      :else :any)))

;; Predicate folding: a type predicate whose argument's type is
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
    ;; a nilable struct might be nil — nil?/some?/record? can't be proven, so the
    ;; runtime guard must stay (this is what makes the narrowing sound).
    (nilable? t) nil
    ;; a bounded scalar union folds only when every member agrees
    (union-type? t)
    (let [vs (map (fn [m] (pred-on pname m)) (umembers t))]
      (if (and (seq vs) (not (nil? (first vs))) (apply = vs)) (first vs) nil))
    :else
    (case pname
      "number?"  (or (= t :num) (= t :double))
      "string?"  (= t :str)
      "keyword?" (= t :kw)
      "record?"  (record-t? t)
      "nil?"     false
      "some?"    true
      nil)))
;; Side-effect-free node whose evaluation can be dropped when its predicate
;; folds away (a wider purity analysis can broaden this later).
(defn- pure-node? [n] (let [op (get n :op)] (or (= op :const) (= op :local))))

;; Flow-sensitive nil narrowing: in (if (some? x) ..) / (if x ..) / (if (nil? x) ..)
;; a nilable-struct local x is proven non-nil in one branch, so its field reads
;; bare-index and unbox there. Only a nilable local narrows — nothing else changes.
(defn- test-local [test pred-name]
  (when (= :invoke (get test :op))
    (let [f (get test :fn) args (get test :args)]
      (when (and (= :var (get f :op)) (= "clojure.core" (get f :ns))
                 (= pred-name (get f :name))
                 (= 1 (count args)) (= :local (get (nth args 0) :op)))
        (get (nth args 0) :name)))))
(defn- narrow-nonnil [tenv nm]
  (let [t (get tenv nm)] (if (nilable? t) (assoc tenv nm (strip-nilable t)) tenv)))
;; [then-tenv else-tenv] for an `if` whose test narrows a nilable local.
(defn- if-narrow [test tenv]
  (let [somev (test-local test "some?")
        nilv (test-local test "nil?")]
    (cond
      (= :local (get test :op)) [(narrow-nonnil tenv (get test :name)) tenv]
      somev [(narrow-nonnil tenv somev) tenv]
      nilv [tenv (narrow-nonnil tenv nilv)]
      :else [tenv tenv])))

(declare infer)

;; infer (and infer-fn-seeded) return a [type node'] tuple — the result type plus
;; the rewritten subtree. A bare (nth r 0)/(nth r 1) transposes silently and still
;; type-checks, so name the projections; the call-pattern code below is dense in them.
(defn- ty [r] (nth r 0))
(defn- nd [r] (nth r 1))

;; Arg types for a self-recursive call. A same-position pass-through of the
;; enclosing param (arg i is the bare param i) contributes nil — the join identity —
;; instead of its type: it can't add information (param i ⊇ param i is trivial), but
;; its type is :any until external callers determine it, and :any is absorbing, so
;; collecting it would pin the param at :any forever (a recursive fn that threads a
;; param straight through, e.g. ray-cast passing `hittables` unchanged). A computed
;; arg, or a DIFFERENT param at this position, is a real constraint and is collected.
(defn- self-rec-argtys [args ares self-params]
  (mapv (fn [i]
          (let [a (nth args i)]
            (if (and self-params (< i (count self-params))
                     (= :local (get a :op)) (= (get a :name) (nth self-params i)))
              nil
              (ty (nth ares i)))))
        (range (count ares))))

;; arithmetic core ops that yield a flonum when their operands are flonums — a
;; mirror of jolt.passes.numeric/dbl-spec's arithmetic set, used to flow :double
;; across fn boundaries so a hintless fn whose callers all pass doubles is unboxed.
;; Comparisons are excluded: they yield a boolean, not a number.
(def ^:private dbl-arith-ops #{"+" "-" "*" "/" "min" "max" "inc" "dec"})
(defn- int-lit-node? [n]
  (and (= :const (get n :op)) (let [v (get n :val)] (and (number? v) (integer? v)))))
;; an arithmetic result is :double when every operand is a proven flonum or a
;; proven numeric type (:num, :long, or an integer literal — all coercible to
;; double) and at least one operand is a proven flonum — so (* x 2) with x:double
;; is :double, and (* x y) with x:double y:num is :double (y coerced at emission).
;; A bare (* a b) with both :num stays :num (no flonum proof, no fl-op).
(defn- dbl-arith? [ares argnodes]
  (and (pos? (count ares))
       (every? (fn [i]
                 (let [t (ty (nth ares i))]
                   (or (= :double t) (= :num t) (= :long t)
                       (int-lit-node? (nth argnodes i)))))
               (range (count ares)))
       (some (fn [r] (= :double (ty r))) ares)))

;; HOFs that apply their fn arg to the ELEMENTS of a collection. :epos is which
;; param of the fn receives an element. reduce is
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
  [node seeds tenv env]
  (let [res (mapv (fn [a]
                    (let [params (get a :params)
                          pe (reduce (fn [e i]
                                       (assoc e (nth params i)
                                              (let [s (get seeds i)] (if s s :any))))
                                     tenv (range (count params)))
                          pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)
                          ;; a seeded :double param (a reduce accumulator over 0.0, or
                          ;; a :double vector element) becomes a ^double nhint so the
                          ;; numeric pass unboxes arithmetic on it — without this a
                          ;; reduce closure's acc stays generic even when the callee's
                          ;; pm-ret is :double.
                          nh (reduce (fn [h i]
                                       (let [s (get seeds i)]
                                         (if (= s :double) (assoc h (nth params i) :double) h)))
                                     {} (range (count params)))
                          br (infer (get a :body) pe env)
                          ret-ty (ty br)]
                      [(ty br) (assoc a :body (nd br) :nhints
                                 (let [existing (into {} (get a :nhints))]
                                   (if (= ret-ty :double) (merge nh existing) existing)))]))
                  (get node :arities))
        rets (mapv (fn [r] (ty r)) res)
        ret (if (empty? rets) :any (reduce join (first rets) (rest rets)))]
    [ret (assoc node :arities (mapv (fn [r] (nd r)) res))]))

;; --- :invoke call patterns ---------------------------------------------------
;; infer's :invoke arm splits the callee/args once, then dispatches by callee
;; shape to one of these. Each returns [type node']; all recurse through `infer`.

(defn- infer-pred-fold
  "A type predicate over a single side-effect-free arg whose type PROVES the answer
  folds to a boolean constant — eliminating the call, and (once const-fold runs
  after inference) collapsing any `if` it gates. Falls back to the normal call path
  when the answer isn't provable or the arg is impure."
  [node fnode cn args tenv env]
  (let [ar (infer (nth args 0) tenv env)
        v (pred-on cn (ty ar))]
    (if (and (not (nil? v)) (pure-node? (nd ar)))
      [:any {:op :const :val v}]
      [(call-ret-type fnode env) (assoc node :args [(nd ar)])])))

(defn- infer-kw-lookup
  "(:k m) / (:k m default): the result is m's field type, and if m is a struct the
  subject is tagged so the back end drops the guard — this types nested access end
  to end (RFC 0005)."
  [node fnode args n tenv env]
  (let [mr (infer (nth args 0) tenv env)
        mt (ty mr)
        msub (if (struct-safe? mt) (mark-struct (nd mr) mt) (nd mr))
        ft (field-type mt (get fnode :val))
        dr (when (= n 2) (infer (nth args 1) tenv env))
        rt (if dr (join ft (ty dr)) ft)
        node' (assoc node :args (if dr [msub (nd dr)] [msub]))]
    ;; a flonum field read is a :double operand for the numeric pass (fl-ops); the
    ;; lookup itself still emits as a keyword/jrec-field-at read, this only feeds
    ;; its kind up so (* (:x v) (:x v)) over a ^double-fielded record unboxes.
    [rt (if (= rt :double) (assoc node' :num-read :double) node')]))

(defn- infer-get-lookup
  "(get m :k [default]): the keyword-lookup result type, when the key is a constant
  keyword."
  [node args n tenv env]
  (let [mr (infer (nth args 0) tenv env)
        mt (ty mr)
        msub (if (struct-safe? mt) (mark-struct (nd mr) mt) (nd mr))
        kr (infer (nth args 1) tenv env)
        ft (field-type mt (get (nth args 1) :val))
        dr (when (= n 3) (infer (nth args 2) tenv env))
        rt (if dr (join ft (ty dr)) ft)
        node' (assoc node :args (if dr [msub (nd kr) (nd dr)] [msub (nd kr)]))]
    [rt (if (= rt :double) (assoc node' :num-read :double) node')]))

(defn- infer-reduce-hof
  "reduce over a typed vector with a fn-literal: seed the closure's accumulator
  (param 0) to the init type and its element (param 1) to the vector's element
  type, so its body — and any calls it makes — see those types."
  [node args n tenv env]
  (let [three (>= n 3)
        coll-r (infer (nth args (if three 2 1)) tenv env)
        init-r (when three (infer (nth args 1) tenv env))
        et (let [ct (ty coll-r)] (if (vec-type? ct) (velem ct) :any))
        init-t (if init-r (ty init-r) :any)
        fn-r (infer-fn-seeded (nth args 0) {0 init-t 1 et} tenv env)]
    [(join init-t (ty fn-r))
     (assoc node :args (if three
                         [(nd fn-r) (nd init-r) (nd coll-r)]
                         [(nd fn-r) (nd coll-r)]))]))

(defn- infer-seq-hof
  "map/mapv/filter/... over a typed vector with a fn-literal: seed the fn's element
  param; mapv/filterv produce a typed vector."
  [node cn args tenv env]
  (let [coll-r (infer (nth args 1) tenv env)
        et (let [ct (ty coll-r)] (if (vec-type? ct) (velem ct) :any))
        fn-r (infer-fn-seeded (nth args 0) {(get (get hof-table cn) :epos) et} tenv env)
        rt (cond (= cn "mapv") (mk-vec (ty fn-r))
                 (= cn "filterv") (mk-vec et)
                 :else :any)]
    [rt (assoc node :args [(nd fn-r) (nd coll-r)])]))

(defn- infer-conj-into
  "conj/into: track the element type of a vector being grown."
  [node fnode cn args n tenv env]
  (let [ares (mapv (fn [a] (infer a tenv env)) args)
        base (ty (nth ares 0))
        rest-ts (mapv (fn [r] (ty r)) (rest ares))
        rt (cond
             (and (= cn "conj") (vec-type? base))
             (mk-vec (reduce join (velem base) rest-ts))
             (and (= cn "into") (vec-type? base) (= 2 n) (vec-type? (nth rest-ts 0)))
             (mk-vec (join (velem base) (velem (nth rest-ts 0))))
             :else (call-ret-type fnode env))]
    [rt (assoc node :args (mapv (fn [r] (nd r)) ares))]))

;; record a ctor site's contribution to field-type inference. Called from infer-call
;; only while collecting-fields? is set (the wp fixpoint's final pass). A direct
;; (->Name ...) with matching arity joins each arg's type into its field slot; the
;; map->Name body's ->Name call is excluded (a map->Name CALL demotes instead). A
;; (map->Name m) site demotes (its arg is an untyped map). A (assoc c :k v) the WP
;; can't attribute to a proven record demotes every record declaring :k (sound: an
;; untyped c could be any of them). Escapes/apply are caught via the escapes box
;; (a ->Name in value position -> demote), handled in wp-infer!.
(defn- record-ctor-site! [fnode args ares env]
  (let [vk (var-key fnode)
        shapes (get env :record-shapes)
        entry (get shapes vk)
        n (count args)
        ats (mapv ty ares)]
    (cond
      ;; direct positional ctor ->Name with matching arity. Skip the auto-generated
      ;; map->Name template body (:map->-ctor-key / a specializable map-> self-name):
      ;; that site reads an untyped map, so a CALLED map->Name demotes instead.
      (and entry (= n (count (get entry :fields)))
           (not (= (get env :map->-ctor-key) vk))
           (not (map->-self-for-record? (get env :self-name) vk)))
      (swap! wp-field-joins-box
             #(join-ctor-args % vk (get entry :fields) ats))
      (and (nil? entry) (.startsWith ^String (get fnode :name) "map->"))
      (let [ck (str (get fnode :ns) "/->" (.substring ^String (get fnode :name) 5))]
        (when (contains? shapes ck) (swap! wp-field-demote-box conj ck)))
      (and (= "clojure.core" (get fnode :ns)) (= (get fnode :name) "assoc")
           (= n 3) (= :const (get (nth args 1) :op)) (keyword? (get (nth args 1) :val)))
      (let [coll-t (ty (nth ares 0)) kw (get (nth args 1) :val)]
        (when-not (record-t? coll-t)
          (doseq [[ck e] shapes]
            (when (contains? (into #{} (get e :fields)) kw)
              (swap! wp-field-demote-box conj ck)))))
      :else nil)))

(defn- infer-call
  "Everything else: type the args, collect the call (var callee) for whole-program
  inference, run the success-type check, and use the declared/estimated return type.
  range produces a numeric vector; an element-returning fn over a typed vector
  yields the element type. A protocol-method call whose receiver (arg 0) is a known
  record type is annotated [type-tag proto method] for devirtualization — the back
  end looks up the impl at emit time and calls it directly, skipping the registry
  dispatch (~19x cheaper)."
  [node fnode iscall-var cn args n tenv env]
  (let [fr (when (not iscall-var) (infer fnode tenv env))
        fnode' (if iscall-var fnode (nd fr))
        ;; the callee's value type: a var's from vtypes (a fn is :truthy, a def
        ;; carries its inferred type), else the inferred type of the callee expr
        callee-t (if iscall-var (get (get env :vtypes) (var-key fnode)) (ty fr))
        ares (mapv (fn [a] (infer a tenv env)) args)]
    (when iscall-var
      ;; a `defn` recurses through its own VAR, so a self-recursion is a var-call
      ;; here (not the :local case below). When the callee is the enclosing def,
      ;; drop same-position pass-through args so threading a param straight through
      ;; the recursion doesn't poison it to :any.
      (swap! (get env :calls) conj
             [(var-key fnode)
              (if (= (var-key fnode) (get env :self-key))
                (self-rec-argtys args ares (get env :self-params))
                (mapv (fn [r] (ty r)) ares))]))
    ;; a named fn calling itself binds its name as a :local, so the recursion is
    ;; invisible to the var-call collection above — yet it constrains the fn's own
    ;; params. Collect it under the fn's var-key so the whole-program fixpoint joins
    ;; the recursive arg types (else a self-recursive param is typed from external
    ;; callers alone and may be specialized to a type the recursion violates).
    (when (and (= :local (get fnode :op)) (get env :self-key)
               (= (get fnode :name) (get env :self-name)))
      (swap! (get env :calls) conj
             [(get env :self-key) (self-rec-argtys args ares (get env :self-params))]))
    ;; collect ctor-site arg types for field-type inference (wp fixpoint's final pass)
    (when (and iscall-var @collecting-fields?-box)
      (record-ctor-site! fnode args ares env))
    ;; success-type check at this call, reusing the arg types just computed (jolt
    ;; audit): core error domains always, user-fn domains in strict mode.
    (when (get env :checking?)
      (let [ats (mapv (fn [r] (ty r)) ares) pos (get node :pos)]
        (when cn (check-invoke cn args ats pos env))
        (when (not-callable? callee-t)
          (swap! (get env :diags) conj
                 {:op :call :type (type-name callee-t) :pos pos
                  :msg (str "cannot call " (type-name callee-t) " as a function")}))
        (when (and (get env :strict?) iscall-var)
          (let [k (var-key fnode) usig (get @(get env :user-sigs) k)]
            (when usig (check-user-call k usig ats pos env))))))
       (let [pm (and iscall-var (get (get env :protocol-methods) (var-key fnode)))
            rtype (when (and pm (pos? n)) (get (ty (nth ares 0)) :type))
            ;; Annotate EVERY recognized protocol call with :proto/:method so the back
            ;; end can build a per-site inline cache even at a megamorphic site (where
            ;; the receiver joins to :any and devirt below doesn't fire). A monomorphic
            ;; site additionally carries :devirt-type and takes the faster devirt path.
            base (if pm
                   (assoc node :proto (nth pm 0) :method (nth pm 1)
                                :fn fnode' :args (mapv (fn [r] (nd r)) ares))
                   (assoc node :fn fnode' :args (mapv (fn [r] (nd r)) ares)))]
          (let [maybe-dbl (and cn (contains? dbl-arith-ops cn) (dbl-arith? ares args))
                rt (cond
                   (= cn "range") (mk-vec :num)
                   (and cn (contains? elem-fns cn) (> n 0))
                   (let [a0 (ty (nth ares 0))] (if (vec-type? a0) (velem a0) :any))
                   ;; flonum arithmetic yields a flonum — flows :double into a callee
                   ;; param (and into the fixpoint's return type) so hintless double
                   ;; code unboxes.
                   maybe-dbl :double
                   :else (call-ret-type fnode env))
              ;; When dbl-arith? proves :double, wrap non-double operands in coerce
              ;; :double nodes so the numeric pass sees :double and emits fl-ops.
              base* (if maybe-dbl
                      (let [coerced (mapv (fn [r a]
                                           (let [t (ty r)]
                                             (cond (int-lit-node? a)
                                                   (assoc a :val (double (get a :val)))
                                                   (= t :num) (coerce-node :double (nd r))
                                                   (= t :long) (coerce-node :double (nd r))
                                                   :else (nd r))))
                                         ares args)]
                        (assoc base :args coerced))
                      base)]
           (let [;; a devirt site whose contagion clone returns :double types its
                 ;; return :double per-site — so a caller accumulator add over it
                 ;; fires dbl-arith? and lowers to fl+. global pm-rets is untouched;
                 ;; PIC/megamorphic sites (no rtype) never reach here.
                 devirt-double? (and rtype pm
                                     (clone-double-ret? rtype (nth pm 0) (nth pm 1)))
                 rt* (if devirt-double? :double rt)
                 rt1 (if (and (= rt* :double) (not maybe-dbl))
                       (assoc base* :num-read :double) base*)
                 rt2 (if rtype
                       (assoc rt1 :devirt-type rtype :devirt-proto (nth pm 0) :devirt-method (nth pm 1))
                       rt1)]
             [rt* rt2])))))

(defn- infer-invoke
  "Split the callee/args once and dispatch by callee shape to a pattern helper."
  [node tenv env]
  (let [fnode (get node :fn)
        iscall-var (= :var (get fnode :op))
        cn (when (and iscall-var (= "clojure.core" (get fnode :ns))) (get fnode :name))
        args (get node :args)
        n (count args)]
    (cond
      (and iscall-var (contains? fold-preds cn) (= n 1))
      (infer-pred-fold node fnode cn args tenv env)

      (and (kw-callee? fnode) (>= n 1) (<= n 2))
      (infer-kw-lookup node fnode args n tenv env)

      (and (get-callee? fnode)
           (>= n 2) (= :const (get (nth args 1) :op)) (keyword? (get (nth args 1) :val)))
      (infer-get-lookup node args n tenv env)

      (and (= cn "reduce") (>= n 2) (= :fn (get (nth args 0) :op)))
      (infer-reduce-hof node args n tenv env)

      (and cn (get hof-table cn) (>= n 2) (= :fn (get (nth args 0) :op)))
      (infer-seq-hof node cn args tenv env)

      (and (or (= cn "conj") (= cn "into")) (>= n 1))
      (infer-conj-into node fnode cn args n tenv env)

      :else
      (infer-call node fnode iscall-var cn args n tenv env))))

(defn- infer
  "Returns [type node'] — the inferred type of node and node with struct-safe
  :local references annotated :hint :struct. tenv maps in-scope local names to
  inferred types; env carries the inference config and this run's accumulators."
  [node tenv env]
  (let [op (get node :op)]
    (cond
      (= op :const)
      [(let [v (get node :val)]
         (cond (and (number? v) (float? v)) :double   ; a flonum literal is :double
               (number? v) :num
               (string? v) :str
               (keyword? v) :kw
               (nil? v) :nil        ; a record|nil branch types as a nilable record
               (= false v) :any     ; false is not struct-eligible
               :else :truthy))                  ; true, char, ... -> non-nil
       node]
      (= op :local)
      (let [t (get tenv (get node :name))]
        [(if t t :any)
         (cond
           (struct-safe? t) (let [n (assoc node :hint :struct)]
                              (if (type-shape t) (assoc n :shape (type-shape t)) n))
           :else node)])
      (= op :map)
      (let [pairs (get node :pairs)
            res (mapv (fn [pr]
                        (let [kr (infer (nth pr 0) tenv env)
                              vr (infer (nth pr 1) tenv env)]
                          [(nth kr 1) (nth vr 1) (nth vr 0) (get (nth pr 0) :val)]))
                      pairs)
            struct? (and (> (count res) 0)
                         (every? (fn [pr] (scalar-const? (nth pr 0))) pairs)
                         (every? (fn [r] (truthy-type? (nth r 2))) res))
            base (when struct?
                   (cap (mk-struct (reduce (fn [m r] (assoc m (nth r 3) (nth r 2))) {} res)) type-depth))
            ;; a literal is a COMPLETE shape: carry its sorted key vector so the
            ;; back end can lay it out and bare-index lookups
            shp (when (and (get env :map-shapes?) base (struct-type? base)) (shape-order (keys (sfields base))))
            t (if base (if shp (assoc base :shape shp) base) :any)
            node' (assoc node :pairs (mapv (fn [r] [(nth r 0) (nth r 1)]) res))]
        [t (if shp (assoc node' :shape shp) node')])
      (= op :vector)
      (let [irs (mapv (fn [x] (infer x tenv env)) (get node :items))
            ets (mapv (fn [r] (nth r 0)) irs)
            el (if (empty? ets) :any (reduce join (first ets) (rest ets)))]
        [(cap (mk-vec el) type-depth) (assoc node :items (mapv (fn [r] (nth r 1)) irs))])
      (= op :set)
      (let [irs (mapv (fn [x] (infer x tenv env)) (get node :items))
            ets (mapv (fn [r] (nth r 0)) irs)
            el (if (empty? ets) :any (reduce join (first ets) (rest ets)))]
        [(cap (mk-set el) type-depth) (assoc node :items (mapv (fn [r] (nth r 1)) irs))])
      (= op :if)
      (let [test (get node :test)
            tr (infer test tenv env)
            nr (if-narrow test tenv)   ; narrow a nilable local in the proven branch
            thn (infer (get node :then) (nth nr 0) env)
            els (infer (get node :else) (nth nr 1) env)]
        [(join (nth thn 0) (nth els 0))
         (assoc node :test (nth tr 1) :then (nth thn 1) :else (nth els 1))])
      (= op :do)
      (let [stmts (mapv (fn [s] (nth (infer s tenv env) 1)) (get node :statements))
            r (infer (get node :ret) tenv env)]
        [(nth r 0) (assoc node :statements stmts :ret (nth r 1))])
      (= op :throw)
      [:any (assoc node :expr (nth (infer (get node :expr) tenv env) 1))]
      ;; a :var reached HERE is in value position (an arg, a let init, ...), not
      ;; a call head — so the fn it names escapes and its params can't be inferred.
      ;; Its VALUE type comes from vtypes (a fn is :truthy, a def carries its
      ;; inferred type); unknown -> :any.
      (= op :var) (do (swap! (get env :escapes) conj (var-key node))
                      [(let [vt (get (get env :vtypes) (var-key node))] (if vt vt :any)) node])
      (= op :invoke) (infer-invoke node tenv env)
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [te (nth acc 0) binds (nth acc 1)
                                ir (infer (nth b 1) te env)]
                            [(assoc te (nth b 0) (nth ir 0)) (conj binds [(nth b 0) (nth ir 1)])]))
                        [tenv []] (get node :bindings))
            br (infer (get node :body) (nth res 0) env)]
        [(nth br 0) (assoc node :bindings (nth res 1) :body (nth br 1))])
      (= op :loop)
      ;; conservative + sound: loop bindings join across recur, which we don't
      ;; track here, so they stay :any. Still descend to annotate any
      ;; known-type lookups inside the body. A recur inside this body targets the
      ;; loop, not the enclosing fn, so mark :in-loop? to suppress self-collection.
      (let [lenv (assoc env :in-loop? true)]
        [:any (assoc node
                     :bindings (mapv (fn [b] [(nth b 0) (nth (infer (nth b 1) tenv env) 1)]) (get node :bindings))
                     :body (nth (infer (get node :body) tenv lenv) 1))])
      (= op :recur)
      (let [ares (mapv (fn [a] (infer a tenv env)) (get node :args))]
        ;; a fn-level recur (not inside a loop) rebinds the enclosing fn's params,
        ;; so its args constrain them like a self-call — collect under the fn key.
        (when (and (not (get env :in-loop?)) (get env :self-key))
          (swap! (get env :calls) conj
                 [(get env :self-key) (self-rec-argtys (get node :args) ares (get env :self-params))]))
        [:any (assoc node :args (mapv (fn [r] (nd r)) ares))])
      (= op :fn)
      ;; a closure inherits the enclosing tenv so CAPTURED locals keep their
      ;; types (e.g. a reduce closure that calls (f captured-struct ...)). Its own
      ;; params shadow to :any UNLESS a param carries a ^Record declared hint
      ;; (:phints, name -> ctor-key) — then seed it to that record type so field
      ;; reads off it bare-index per-form, not only under whole-program. This is
      ;; what makes a protocol method's `this` (hinted by defrecord/extend-type)
      ;; read its fields without the runtime tag guard.
      ;; a nested closure resets the self/loop context: its own recur/self-call
      ;; targets IT, not the enclosing whole-program def, so it must not collect
      ;; into that def's param key.
      (let [fenv (assoc env :self-name nil :self-key nil :self-params nil :in-loop? false)]
        [:any (assoc node :arities
                     (mapv (fn [a]
                             (let [shapes (get env :record-shapes)
                                   phm (reduce (fn [m pr] (assoc m (nth pr 0) (nth pr 1)))
                                               {} (get a :phints))
                                   pe (reduce (fn [e p]
                                                (assoc e p
                                                       (let [ent (get shapes (get phm p))]
                                                          (if ent (record-type-from-entry ent (get phm p) type-depth shapes) :any))))
                                              tenv (get a :params))
                                   pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)]
                               (assoc a :body (nth (infer (get a :body) pe fenv) 1))))
                           (get node :arities)))])
       (= op :def)
       (do (when (get env :checking?) (register-user-fn! node env))
           (let [nm (get node :name)
                 ;; a defrecord emits (def map->Name (fn [m] (->Name (get m :f) ...)))
                 ;; whose body is the untyped-map ctor path. Mark it so record-ctor-site!
                 ;; skips that template site (a CALLED/escaped map->Name demotes the
                 ;; record separately — see record-ctor-site!/derive-field-types), else a
                 ;; never-called map-> would poison every field to :any.
                 env' (if (and nm (.startsWith ^String nm "map->"))
                        (let [ck (str (get node :ns) "/->" (.substring ^String nm 5))
                              shapes (get env :record-shapes)]
                          (if (contains? shapes ck) (assoc env :map->-ctor-key ck) env))
                        env)]
             [:any (assoc node :init (nth (infer (get node :init) tenv env') 1))]))
      (= op :try)
      (let [n (assoc node :body (nth (infer (get node :body) tenv env) 1))
            n (if (get node :catch-body) (assoc n :catch-body (nth (infer (get node :catch-body) tenv env) 1)) n)
            n (if (get node :finally) (assoc n :finally (nth (infer (get node :finally) tenv env) 1)) n)]
        [:any n])
      :else [:any node])))

(defn- infer-top [node env] (nth (infer node {} env) 1))

;; ---------------------------------------------------------------------------
;; Success-type checking (RFC 0006). Reuse the inference above as a loose type
;; checker: flag a core-fn call ONLY when an argument's inferred type is
;; concrete AND lies in that op's error domain (the op provably throws on it).
;; Everything ambiguous — :any, :truthy (true/char/...), :nil — is accepted, so
;; there are no false positives. The table is curated to genuinely-throwing
;; cases; lenient ops ((get 5 :k) -> nil, (:k 5) -> nil) are NOT listed.

;; --- user-function error domains, opt-in -------------------------------------
(defn- all-any-env
  "tenv binding every param name to :any (the all-ambiguous baseline)."
  [params]
  (reduce (fn [e p] (assoc e p :any)) {} params))

(defn- isolated-diag-count
  "Count of diagnostics typing body under tenv produces. Runs under a SUB-ENV
  with its own diags cell, so this probe never leaks into the real report (the
  shared calls/escapes/guard cells are intentionally still threaded — they are
  not read here). Runs the same checking inference as check-form."
  [body tenv env]
  (let [sub (assoc env :diags (atom []))]
    (infer body tenv sub)
    (count @(get sub :diags))))

(defn- check-user-call
  "Strict mode: report a call to a registered user fn that provably throws —
  either a WRONG ARITY (the registered fn has one fixed arity, so a different
  arg count always throws) or an argument whose concrete type the body
  rejects. For the latter, re-check the body with ONLY that parameter bound to
  its arg type (others :any); a diagnostic the all-:any body did not already
  have means the argument alone is provably wrong. Monotonic — binding a
  concrete type can only ADD error-domain hits — so no false positive.
  Cycle-guarded (env's checking-set) so mutually recursive fns terminate."
  [key sig arg-types pos env]
  (let [cset (get env :checking-set)]
    (when (not (contains? @cset key))
      (let [prev @cset]
        (reset! cset (conj prev key))
        (let [params (:params sig)
              body (:body sig)
              npar (count params)
              nargs (count arg-types)
              memo (get env :diag-memo)]
          (if (not= npar nargs)
            ;; arity is provably wrong regardless of types — report and stop (the
            ;; per-arg type re-check would bind params positionally, meaningless
            ;; under a mismatch)
            (swap! (get env :diags) conj
                   {:op :user-call :type :arity :pos pos
                    :msg (str "wrong number of args (" nargs ") passed to `"
                              (:name sig) "` (expected " npar ")")})
            ;; all-any-env is built once (was rebuilt per param), and each probe is
            ;; memoized by [key i argtype] so the same fn re-checked across call
            ;; sites in this form re-infers its body at most once per (param, type).
            (let [base-env (all-any-env params)
                  base (let [bk [:base key]]
                         (if (contains? @memo bk)
                           (get @memo bk)
                           (let [b (isolated-diag-count body base-env env)]
                             (swap! memo assoc bk b) b)))]
              (reduce
                (fn [_ i]
                  (let [at (nth arg-types i)]
                    (when (and (not= at :any) (not= at :truthy))
                      (let [mk [:arg key i at]
                            rejects (if (contains? @memo mk)
                                      (get @memo mk)
                                      (let [r (> (isolated-diag-count body (assoc base-env (nth params i) at) env) base)]
                                        (swap! memo assoc mk r) r))]
                        (when rejects
                          (swap! (get env :diags) conj
                                 {:op :user-call :argpos i :type (type-name at) :pos pos
                                  :msg (str "argument " (inc i) " to `" (:name sig)
                                            "` is " (type-name at)
                                            ", which its body provably rejects")})))))
                  nil)
                nil (range npar)))))
        (reset! cset prev)))))

;; --- Inter-procedural driver API consumed by the back end -------------------
(defn set-rtenv!
  "Install the current return-type estimates (a map \"ns/name\" -> type) used to
  type call results during the fixpoint."
  [m] (swap! config-box assoc :rtenv (or m {})))

;; install record-ctor shapes ("ns/->Name" -> [field-kw ...]) and the
;; map-shaping flag (opt-in JOLT_SHAPE), both read by infer.
(defn set-record-shapes! [m] (swap! config-box assoc :record-shapes (or m {})))
(defn set-protocol-methods! [m] (swap! config-box assoc :protocol-methods (or m {})))
(defn set-map-shapes! [b] (swap! config-box assoc :map-shapes? (boolean b)))

(defn set-vtypes!
  "Install var VALUE types (a map \"ns/name\" -> type): fn vars are :truthy
  (non-nil), def vars carry their inferred init type."
  [m] (swap! config-box assoc :vtypes (or m {})))

(defn reset-escapes! [] (reset! escapes-box #{}))
(defn collected-escapes [] (vec @escapes-box))

(defn check-form
  "Success-type check a single analyzed form (RFC 0006). Returns a vector of
  diagnostics [{:op :argpos :type :msg} ...] for provably-wrong calls; empty
  when nothing is provably wrong. Runs independently of specialization so it is
  usable in normal builds (the decoupled checking path).

  With strict? true, also reports calls to registered user functions whose
  concrete argument types provably make the body throw (opt-in,
  closed-world). user-sig-box accumulates registered defs across forms, so a
  def must precede its call — the same ordering RFC 0005 already assumes."
  ([node] (check-form node false))
  ([node strict?]
   ;; the check IS the inference: one walk that types and emits diagnostics into
   ;; this run's env. The optimization fixpoint runs with checking? false so it
   ;; stays silent.
   (let [env (mk-env true strict?)]
     (infer node {} env)
     (vec @(get env :diags)))))

(defn infer-body
  "Type `body` under tenv (local-name -> type). Returns [ret-type node' calls],
  where calls is the [[\"ns/name\" [arg-types...]] ...] this body invokes (for
  propagating into callee param types). Also accumulates escapes (read with
  collected-escapes after a full sweep). With self-name/self-key, a recursive
  self-call or fn-level recur in `body` is collected under self-key too, so a
  self-recursive fn's params are constrained by its recursion, not just callers."
  ([body tenv] (infer-body body tenv nil nil nil))
  ([body tenv self-name self-key] (infer-body body tenv self-name self-key nil))
  ([body tenv self-name self-key self-params]
   (let [env (assoc (mk-env false false)
                    :self-name self-name :self-key self-key :self-params self-params)
         r (infer body tenv env)]
     [(nth r 0) (nth r 1) @(get env :calls)])))

;; --- protocol-method return types -------------------------------------------
;; An impl is emitted as (register-(inline-)method TAG "Proto" "method" (fn ...)).
;; Its fn body's return type is one impl's contribution to the method's return; the
;; join over every impl is the method's return type (monomorphic when all agree).

(defn register-impl-invoke?
  "Recognize a register-inline-method / register-method invoke node whose first
  three args are const strings and whose fn arg is a fn literal. Returns
  [type-name proto method fn-node] or nil. The one shared predicate — the
  backend and both inference paths use it so the checks can't drift."
  [node]
  (let [f (:fn node) a (:args node)]
    (when (and (= :var (:op f)) (= "clojure.core" (:ns f))
               (#{"register-inline-method" "register-method"} (:name f))
               (= 4 (count a)))
      (let [[tc pc mc fc] a]
        (when (and (= :const (:op tc)) (string? (:val tc))
                   (= :const (:op pc)) (string? (:val pc))
                   (= :const (:op mc)) (string? (:val mc))
                   (= :fn (:op fc)))
          [(:val tc) (:val pc) (:val mc) fc])))))

(defn- impl-reg-ret [node]
  (when-some [[type-name proto method fnn] (register-impl-invoke? node)]
    (when (seq (get fnn :arities))
      (let [arity (first (get fnn :arities))
            rtype (inline-impl-receiver-type type-name (get arity :params))
            tenv (if rtype {(first (get arity :params)) rtype} {})]
        [(str proto "/" method)
         (nth (infer-body (get arity :body) tenv) 0)]))))

(defn- walk-pm-rets [node acc]
  (let [kr (impl-reg-ret node)
        acc (if kr (update acc (nth kr 0) (fn [t] (if t (join t (nth kr 1)) (nth kr 1)))) acc)]
    (reduce-ir-children (fn [a c] (walk-pm-rets c a)) acc node)))

(defn collect-pm-rets!
  "Scan the unit's nodes for protocol-method impl registrations and stash each
  method's joined impl-return type (record-shapes must already be installed)."
  [nodes]
  (reset! pm-rets-box (reduce (fn [acc n] (walk-pm-rets n acc)) {} nodes)))

;; --- inline method body receiver typing ------------------------------------
;; A defrecord/deftype inline method body reads its fields via (get this :field).
;; With the receiver param seeded as the record type, those reads resolve to
;; jrec-field-at (bare index) instead of jolt-get. The receiver-typed node is
;; spliced into the register-inline-method's fn arg so the backend emits the
;; fast-path body. See also collect-pm-rets! (return-type inference only).

(defn- inline-impl-receiver-type
  "Given the type-name string from args[0] of register-inline-method (e.g.
  \"Circle\") and the fn's param names, return the record type for seeding
  param 0, or nil if type-name is not a known record (e.g. a host type name
  from extend-type)."
  [type-name params]
  (when (and type-name (seq params))
    (let [shapes (get @config-box :record-shapes)
          target (str "->" type-name)
          matches (filterv (fn [[k v]] (.endsWith k target)) shapes)]
      ;; only use suffix match when exactly one shape matches; zero or >1
      ;; is ambiguous (two same-named records in different namespaces) and
      ;; no seeding is safer than wrong seeding
      (when (= 1 (count matches))
        (let [ctor-key (key (first matches))]
          (record-type-from-entry (get shapes ctor-key) ctor-key type-depth shapes))))))

(defn reinfer-inline-method-bodies
  "Walk node and re-infer the fn bodies of register-inline-method and
  register-method invocations with param 0 (the receiver) seeded as the
  record type. This lets field reads in inline method bodies emit
  jrec-field-at (bare index) instead of jolt-get + keyword.
  Must be called after record-shapes are installed (set-record-shapes!).
  Skips host type names (no record shape). Returns the node with annotated
  fn bodies spliced into the invoke args."
  [node]
  (let [walk (fn walk [n]
               (if-some [[type-name _ _ fnn] (register-impl-invoke? n)]
                 (if (seq (get fnn :arities))
                   (let [rtype (inline-impl-receiver-type type-name
                                  (get (first (get fnn :arities)) :params))
                         env (mk-env false false)
                         args (:args n)]
                     (if rtype
                       ;; re-infer every arity with param 0 seeded as record type
                       (let [annotated (mapv (fn [arity]
                                               (let [params (get arity :params)
                                                     tenv (if (seq params)
                                                            {(first params) rtype} {})]
                                                 (assoc arity :body
                                                        (nth (infer (get arity :body) tenv env) 1))))
                                             (get fnn :arities))]
                         (assoc n :args (assoc args 3 (assoc fnn :arities annotated))))
                       (map-ir-children walk n)))
                   (map-ir-children walk n))
                 (map-ir-children walk n)))]
    (walk node)))

;; count :coerce :double nodes anywhere in a subtree (reduce-ir-children is one
;; level, so walk it). Used to tell whether contagion added a coercion the shared
;; (lean) path leaves out — the clone-worth-emitting signal.
(defn- count-coerce-double [node]
  (letfn [(walk [n]
            (let [self (if (and (= :coerce (get n :op)) (= :double (get n :kind))) 1 0)]
              (+ self (reduce-ir-children (fn [acc c] (+ acc (walk c))) 0 n))))]
    (walk node)))

(defn contagion-specialize-arity
  "Build a contagion-specialized clone of a protocol-method impl's fn `arity` for the
  receiver record `type-name`. Returns [specialized-arity eligible?].

  The receiver (param 0) is seeded as the record type with its :num fields surfaced
  (rich-field-types-box, via *field-type-box*), so dbl-arith? contagions a :num field
  read that sits beside a proven :double operand — wrapping it in coerce :double,
  emitted as exact->inexact. That is the same machinery a genuine ^double field
  reaches; contagion is sound because Clojure double contagion makes the result a
  double regardless, so eagerly converting the :num operand is value-identical
  (the same rule dbl-arith? already encodes). A pure-:num expression (no proven :double operand)
  contagions nothing — dbl-arith? requires a :double sibling.

  eligible? is true only when contagion fired a site the shared (lean) path leaves
  generic — more coerce :double nodes than the lean receiver type yields — so a clone
  is emitted exactly when it recovers fl* the shared body lacks. Isolated from the
  shared fixpoint: it reads rich-field-types-box, never field-types-box, so pm-rets
  and the ordinary impl body stay Option-A lean. Returns [arity false] when the
  receiver isn't a known record (a host type, or no record shape)."
  [arity type-name]
  (let [params (get arity :params)
        body (get arity :body)
        rich-rtype (binding [*field-type-box* rich-field-types-box]
                     (inline-impl-receiver-type type-name params))]
    (if (not rich-rtype)
      [arity false false]
      (let [env (mk-env false false)
            rich-res (infer body (if (seq params) {(first params) rich-rtype} {}) env)
            rich-body (nth rich-res 1)
            rich-ret (nth rich-res 0)
            lean-rtype (inline-impl-receiver-type type-name params)
            lean-body (if lean-rtype
                        (nth (infer body (if (seq params) {(first params) lean-rtype} {}) env) 1)
                        body)]
        (if (> (count-coerce-double rich-body) (count-coerce-double lean-body))
          [(assoc arity :body rich-body) true (= :double rich-ret)]
          [arity false false])))))

(defn reinfer-def
  "Re-run inference on a stashed :def's fn arity bodies with param types seeded
  (ptmap: param-name -> type), returning the def with annotated bodies. The back
  end emits the result directly (no further passes), so the param-typed lookups
  keep their specialization. Used by the inter-procedural recompile."
  [def-node ptmap]
  (let [fnode (get def-node :init)
        env (if (get @check-mode-box :on)
              (mk-env true (get @check-mode-box :strict))
              (mk-env false false))
        shapes (get env :record-shapes)]
    (if (= :fn (get fnode :op))
      (let [result (assoc def-node :init
                    (assoc fnode :arities
                           (mapv (fn [a]
                                   ;; seed declared record param hints (:phints, name ->
                                   ;; ctor-key) so a record param is typed even with no
                                   ;; inferred caller type — the open-world / cross-ns
                                   ;; case. An inferred type in ptmap wins (it's at least
                                   ;; as precise), so this only fills the gaps.
                                   (let [pt (reduce (fn [m pr]
                                                      (let [nm (nth pr 0)
                                                            e (get shapes (nth pr 1))]
                                                        (if (and e (not (contains? m nm)))
                                                          (assoc m nm (record-type-from-entry e (nth pr 1) type-depth shapes))
                                                          m)))
                                                    ptmap (get a :phints))]
                                     (assoc a :body (nth (infer (get a :body) pt env) 1))))
                                 (get fnode :arities))))]
        (when (get @check-mode-box :on)
          (reset! last-diags-box @(get env :diags)))
        result)
      def-node)))

;; --- whole-program param-type fixpoint --------------------------------------
;; Re-derive each app fn's param types from its call sites under closed world
;; (--opt), so a record type flows across fn boundaries: a ctor's return type
;; reaches a callee param ((check-tree (make-tree d)) -> node is a Node), and a
;; typed vector's element reaches a HOF closure's param (sum-area's reduce sees a
;; Circle). The back end then bare-indexes a field read and devirtualizes a
;; protocol call at those sites. Only single-fixed-arity fns are specialized;
;; anything called in value position (collected-escapes) keeps :any params —
;; its callers aren't all visible, so a concrete seed would be unsound.
(def ^:private wp-seeds-box (atom {}))
(defn param-seeds-for
  "The param-name -> type seed map a top-level def should be reinferred with, or
  nil. Set by wp-infer!, read by run-passes during the final per-def emit."
  [k] (get @wp-seeds-box k))

;; numeric refinement of the same fixpoint: params the closed-world join proved
;; are always flonums. Kept SEPARATE from the structural box — these don't reinfer
;; (field-read/devirt), they become synthetic ^double nhints (jolt.passes/inject-
;; wp-nhints) so the hint-directed pass unboxes the arithmetic.
(def ^:private wp-num-seeds-box (atom {}))
(defn param-num-seeds-for
  "The param-name -> :double seed map for a def's hintless flonum params, or nil."
  [k] (get @wp-num-seeds-box k))

;; var-key -> {:params [names] :body ir} for each single-fixed-arity fn def.
(defn- wp-specializable [nodes]
  (reduce (fn [m d]
            (let [f (get d :init)]
              (if (and (= :def (get d :op)) (= :fn (get f :op))
                       (= 1 (count (get f :arities)))
                       (not (get (first (get f :arities)) :rest)))
                (let [a (first (get f :arities))]
                  (assoc m (str (get d :ns) "/" (get d :name))
                         {:name (get d :name) :params (get a :params) :body (get a :body)}))
                m)))
          {} nodes))

(defn- wp-empty-ptypes [spec ks]
  (reduce (fn [m k] (assoc m k (vec (repeat (count (:params (get spec k))) nil)))) {} ks))

;; join one call's arg types into its (specializable) callee's param slots.
(defn- wp-accum [pt spec calls]
  (reduce (fn [pt2 c]
            (let [callee (nth c 0) args (nth c 1)]
              (if (contains? spec callee)
                (let [cur (get pt2 callee)]
                  (assoc pt2 callee
                         (vec (map-indexed
                                (fn [i t] (if (< i (count args)) (join t (nth args i)) t)) cur))))
                pt2)))
          pt calls))

;; one fixpoint pass over every top-level node: a specializable def is typed
;; under the current param seeds (so a seeded record flows into the calls it
;; makes) and contributes its return type; any other form is typed only to
;; harvest its call sites and escapes. Returns {:rets :ptypes}, with ptypes
;; recomputed fresh each pass — :any is absorbing, so accumulating across passes
;; would pin a param at :any before its callers' return types are known.
(defn- wp-pass [nodes spec ks ptypes]
  (reduce
    (fn [acc node]
      (let [k (when (= :def (get node :op)) (str (get node :ns) "/" (get node :name)))
            s (and k (get spec k))]
        (if s
          (let [r (infer-body (:body s) (zipmap (:params s) (get ptypes k)) (:name s) k (:params s))]
            (-> acc (assoc-in [:rets k] (nth r 0))
                    (update :ptypes wp-accum spec (nth r 2))))
          (update acc :ptypes wp-accum spec (nth (infer-body node {}) 2)))))
    {:rets {} :ptypes (wp-empty-ptypes spec ks)} nodes))

;; fold a pass's positional ctor-arg joins into a {ctor-key {field-kw type}} map,
;; dropping demoted/escaped records (their fields read :any) and :any/conflicting
;; fields (the sound default — a conflicting join unboxes nothing). Field types are
;; projected to a SHALLOW form (type + shape + nilable, no nested :struct) — a deep
;; nilable-Node would embed Node (which re-embeds nilable-Node), deepening each
;; fixpoint round and never value-equaling, so the field-type loop wouldn't converge.
;; The shallow form is stable and all the read/mark machinery needs (it keys off
;; :type/:shape/:nilable, not nested fields — a nilable struct's subfields read :any).
(defn- shallow-field-type [t]
  (cond
    ;; Option B: only a genuine all-flonum ctor-arg join unboxes. A :num (integer/
    ;; mixed) join reads :any, so flonum arithmetic over it stays generic — dbl
    ;; contagion is reserved for :double fields (the all-flonum->:double spec).
    (= t :double) t
    (struct-type? t) (let [base {:type (get t :type) :struct {} :shape (get t :shape)}]
                       (if (nilable? t) (assoc base :nilable true) base))
    :else :any))
;; the rich variant keeps :num, so a specialized clone's :num field reads type :num
;; and dbl-arith? contagions them beside a proven :double operand (the invariant).
;; Feeds rich-field-types-box only; never the shared path.
(defn- shallow-field-type-rich [t]
  (cond
    (or (= t :double) (= t :num)) t
    (struct-type? t) (let [base {:type (get t :type) :struct {} :shape (get t :shape)}]
                       (if (nilable? t) (assoc base :nilable true) base))
    :else :any))
(defn- derive-field-types [joins demoted escaped shallow]
  (reduce-kv
    (fn [m ctor-key argts]
      (if (or (contains? demoted ctor-key) (contains? escaped ctor-key))
        m
        (let [fields (get-in @config-box [:record-shapes ctor-key :fields])]
          (reduce (fn [m2 i]
                    (let [fw (nth fields i nil) t (nth argts i nil)]
                      (if (and fw t (not= t :any))
                        (assoc-in m2 [ctor-key fw] (shallow t)) m2)))
                  m (range (count argts))))))
    {} joins))

;; inner param-type fixpoint, run with field-types-box held FIXED by the caller.
;; returns [converged? ptypes]. The outer field-type loop in wp-infer! re-runs this
;; until field types stabilize, so the param fixpoint converges on its own each round
;; (folding field types into the SAME loop produced a ptypes<->rets 2-cycle).
(defn- wp-param-fixpoint [nodes spec ks]
  (loop [iter 0 ptypes (wp-empty-ptypes spec ks) rets {}]
    (set-rtenv! (reduce (fn [m k] (let [v (get rets k)] (if (some? v) (assoc m k v) m))) {} ks))
    (reset-escapes!)
    (reset! wp-field-joins-box {})
    (reset! wp-field-demote-box #{})
    (let [pass (wp-pass nodes spec ks ptypes)
          escaped (set (collected-escapes))
          new-ptypes (reduce (fn [m k]
                               (if (contains? escaped k)
                                 (assoc m k (vec (repeat (count (get m k)) :any))) m))
                             (:ptypes pass) ks)
          new-rets (:rets pass)
          converged? (and (= new-ptypes ptypes) (= new-rets rets))]
      (if (or converged? (>= iter 16))
        [converged? new-ptypes]
        (recur (inc iter) new-ptypes new-rets)))))

(defn wp-infer!
  "Run the closed-world param-type fixpoint over the unit's analyzed top-level
  nodes and stash the resulting per-def seed maps (read via param-seeds-for).
  record-shapes / protocol-methods must already be installed. Idempotent — resets
  the seed box; called once per build before per-form emit."
  [nodes]
  (collect-pm-rets! nodes)
  (let [spec (wp-specializable nodes)
        ks (keys spec)]
    (try
      (reset! collecting-fields?-box true)
      ;; OUTER loop over field types: run the (converging) param fixpoint with the
      ;; current field types installed, derive field types from the converged pass's
      ;; ctor-site joins, repeat until field types stabilize. Separating the loops
      ;; keeps the param fixpoint monotone (record-type-from-entry sees a fixed
      ;; field-types-box each round); field types converge in ~2 rounds because a
      ;; record's ctor-arg join depends on the record TAG (intrinsic) not on field
      ;; types. Only when BOTH the param fixpoint AND field types have converged do we
      ;; trust the result — otherwise we widen every param to :any and drop field
      ;; types (a pre-fixpoint is more specific than the truth, so unsound to seed).
      (loop [ft-iter 0 ftypes {}]
        (reset! field-types-box ftypes)
        (swap! field-gen inc)
        (let [[param-converged? new-ptypes] (wp-param-fixpoint nodes spec ks)
              escaped (set (collected-escapes))
               new-ftypes (derive-field-types @wp-field-joins-box @wp-field-demote-box escaped shallow-field-type)
               new-rich-ftypes (derive-field-types @wp-field-joins-box @wp-field-demote-box escaped shallow-field-type-rich)
               ft-stable? (= new-ftypes ftypes)
              sound? (and param-converged? ft-stable?)]
          (if (or sound? (>= ft-iter 8))
            (let [seed-ptypes (if sound?
                                new-ptypes
                                (reduce (fn [m k] (assoc m k (vec (repeat (count (get m k)) :any))))
                                        new-ptypes ks))
                  _ (reset! field-types-box (if sound? new-ftypes {}))
                  _ (swap! field-gen inc)
                  _ (reset! rich-field-types-box (if sound? new-rich-ftypes {}))
                  ;; re-derive the protocol-method return types now that field types
                  ;; are known: an impl body that reads a field unboxes once the field
                  ;; proves :double, so its joined return (the callers' pm-ret)
                  ;; tightens from :any/:num to :double — that's what unboxes the caller.
                  _ (when sound? (collect-pm-rets! nodes))
                  ;; build both seed maps from the same converged ptypes: the
                  ;; structural one (struct/vec, drives reinfer-def's field-read/
                  ;; devirt) excludes :double and nilable (a nilable param's reads are
                  ;; generic anyway, and a fn recursing on a nilable field must not be
                  ;; specialized — its param can't be soundly typed non-nil); the
                  ;; numeric one keeps only :double.
                  pick (fn [keep?]
                         (reduce (fn [m k]
                                   (let [s (get spec k)
                                         pm (reduce (fn [pm pr]
                                                      (let [nm (nth pr 0) t (nth pr 1)]
                                                        (if (and t (keep? t)) (assoc pm nm t) pm)))
                                                    {} (map vector (:params s) (get seed-ptypes k)))]
                                     (if (seq pm) (assoc m k pm) m)))
                                 {} ks))]
              (reset! wp-seeds-box (pick (fn [t] (and (not= t :any) (not= t :double) (not (nilable? t))))))
              (reset! wp-num-seeds-box (pick (fn [t] (= t :double))))
              sound?)
            (recur (inc ft-iter) new-ftypes))))
      (finally (reset! collecting-fields?-box false)))))

;; Piggyback checking (jolt audit). In direct-link mode infer-top already runs
;; one inference pass for specialization; turning checking? on during it makes
;; the success checker nearly free there (no extra traversal — just the
;; per-call error-domain predicates). The back end sets the mode before
;; run-passes and reads take-diags! after. It checks the POST-optimization IR,
;; which matches what the optimized program actually evaluates (scalar-replace
;; only drops provably-pure code, an accepted opt-mode divergence).
(defn set-check-mode!
  "Enable/disable checking during the next run-passes inference (direct-link)."
  [on strict?] (reset! check-mode-box {:on (if on true false) :strict (if strict? true false)}))
(defn take-diags!
  "Diagnostics accumulated by the last checking run-passes; clears the buffer."
  [] (let [d @last-diags-box] (reset! last-diags-box []) d))

(defn run-inference
  "Type-infer the optimized node (the inference walk specializes struct-safe
  lookups). When check mode is on (set-check-mode!), the same walk also emits
  success-type diagnostics, stashed for take-diags! to drain afterward. Pulled
  out of run-passes so the checking state stays private to this namespace."
  [opt]
  (if (get @check-mode-box :on)
    (let [env (mk-env true (get @check-mode-box :strict))
          r (infer-top opt env)]
      (reset! last-diags-box @(get env :diags))
      r)
    (infer-top opt (mk-env false false))))
