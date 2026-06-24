(ns jolt.passes.types
  "Collection-type inference and success-type checking (RFC 0006).
  A forward, soft-typing pass (simplified HM: monovariant, never-fails, lattice
  top = :any) that types expressions and reuses the same walk as a loose success
  checker. Also the inter-procedural driver API the back end calls to
  propagate param types across a unit / the whole program. Weakly coupled to the
  IR-rewriting passes — shares the const-shape predicates (jolt.passes.fold)."
  (:require [jolt.passes.fold :refer [scalar-const? kw-callee? get-callee?]]
            [jolt.passes.types.lattice :refer
             [velem selem sfields vec-type? set-type? struct-type? mk-vec mk-set
              mk-struct union-cap scalar-t? union-type? umembers union-of merge-fields
              join-t join type-depth cap struct-safe? field-type shape-order type-shape
              mark-struct truthy-type? num-ret-fns vector-ret-fns]]))

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

;; build a record's struct TYPE from its registry entry, resolving each field's
;; declared type hint against `shapes` ("ns/->Name" -> entry). A field tagged with
;; a record type (its ctor-key) recurses, so a Vec3 stored in a Ray field reads
;; back as Vec3 — not :any — which is what lets nested-record code prove its reads.
;; Depth-bounded so a self/cyclic-referencing record type can't loop.
(declare record-type-from-entry)
(defn- field-type-from-tag [tag depth shapes]
  (cond
    (or (nil? tag) (<= depth 0)) :any
    (= tag "num") :num
    :else (let [e (get shapes tag)]
            (if e (record-type-from-entry e depth shapes) :any))))
(defn- record-type-from-entry [rs depth shapes]
  (let [fields (get rs :fields)
        tags (get rs :tags)
        fmap (reduce (fn [m i]
                       (assoc m (nth fields i)
                              (field-type-from-tag (when tags (nth tags i)) (dec depth) shapes)))
                     {} (range (count fields)))]
    (assoc (mk-struct fmap) :shape (vec fields) :type (get rs :type))))

;; fns that RETURN an element of their (first) collection arg, so a lookup on the
;; result of (rand-nth coll-of-structs) etc. types as the element.
(def ^:private elem-fns #{"rand-nth" "first" "peek" "last" "nth" "fnext" "second"})

;; the checker's emission points, defined after infer but referenced from it
(declare check-invoke check-user-call register-user-fn! not-callable? type-name)

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
                      (record-type-from-entry rs type-depth shapes)
                      (let [r (get (get env :rtenv) (var-key fnode))]
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

;; infer (and infer-fn-seeded) return a [type node'] tuple — the result type plus
;; the rewritten subtree. A bare (nth r 0)/(nth r 1) transposes silently and still
;; type-checks, so name the projections; the call-pattern code below is dense in them.
(defn- ty [r] (nth r 0))
(defn- nd [r] (nth r 1))

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
                          br (infer (get a :body) pe env)]
                      [(ty br) (assoc a :body (nd br))]))
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
        dr (when (= n 2) (infer (nth args 1) tenv env))]
    [(if dr (join ft (ty dr)) ft)
     (assoc node :args (if dr [msub (nd dr)] [msub]))]))

(defn- infer-get-lookup
  "(get m :k [default]): the keyword-lookup result type, when the key is a constant
  keyword."
  [node args n tenv env]
  (let [mr (infer (nth args 0) tenv env)
        mt (ty mr)
        msub (if (struct-safe? mt) (mark-struct (nd mr) mt) (nd mr))
        kr (infer (nth args 1) tenv env)
        ft (field-type mt (get (nth args 1) :val))
        dr (when (= n 3) (infer (nth args 2) tenv env))]
    [(if dr (join ft (ty dr)) ft)
     (assoc node :args (if dr [msub (nd kr) (nd dr)] [msub (nd kr)]))]))

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
      (swap! (get env :calls) conj [(var-key fnode) (mapv (fn [r] (ty r)) ares)]))
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
          base (assoc node :fn fnode' :args (mapv (fn [r] (nd r)) ares))]
      [(cond
         (= cn "range") (mk-vec :num)
         (and cn (contains? elem-fns cn) (> n 0))
         (let [a0 (ty (nth ares 0))] (if (vec-type? a0) (velem a0) :any))
         :else (call-ret-type fnode env))
       (if rtype
         (assoc base :devirt-type rtype :devirt-proto (nth pm 0) :devirt-method (nth pm 1))
         base)])))

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
      (let [tr (infer (get node :test) tenv env)
            thn (infer (get node :then) tenv env)
            els (infer (get node :else) tenv env)]
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
      ;; known-type lookups inside the body.
      [:any (assoc node
                   :bindings (mapv (fn [b] [(nth b 0) (nth (infer (nth b 1) tenv env) 1)]) (get node :bindings))
                   :body (nth (infer (get node :body) tenv env) 1))]
      (= op :recur)
      [:any (assoc node :args (mapv (fn [a] (nth (infer a tenv env) 1)) (get node :args)))]
      (= op :fn)
      ;; a closure inherits the enclosing tenv so CAPTURED locals keep their
      ;; types (e.g. a reduce closure that calls (f captured-struct ...)). Its own
      ;; params shadow to :any UNLESS a param carries a ^Record declared hint
      ;; (:phints, name -> ctor-key) — then seed it to that record type so field
      ;; reads off it bare-index per-form, not only under whole-program. This is
      ;; what makes a protocol method's `this` (hinted by defrecord/extend-type)
      ;; read its fields without the runtime tag guard.
      [:any (assoc node :arities
                   (mapv (fn [a]
                           (let [shapes (get env :record-shapes)
                                 phm (reduce (fn [m pr] (assoc m (nth pr 0) (nth pr 1)))
                                             {} (get a :phints))
                                 pe (reduce (fn [e p]
                                              (assoc e p
                                                     (let [ent (get shapes (get phm p))]
                                                       (if ent (record-type-from-entry ent type-depth shapes) :any))))
                                            tenv (get a :params))
                                 pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)]
                             (assoc a :body (nth (infer (get a :body) pe env) 1))))
                         (get node :arities)))]
      (= op :def)
      (do (when (get env :checking?) (register-user-fn! node env))
          [:any (assoc node :init (nth (infer (get node :init) tenv env) 1))])
      (= op :try)
      [:any (assoc node
                   :body (nth (infer (get node :body) tenv env) 1)
                   :catch-body (when (get node :catch-body) (nth (infer (get node :catch-body) tenv env) 1))
                   :finally (when (get node :finally) (nth (infer (get node :finally) tenv env) 1)))]
      :else [:any node])))

(defn- infer-top [node env] (nth (infer node {} env) 1))

;; ---------------------------------------------------------------------------
;; Success-type checking (RFC 0006). Reuse the inference above as a loose type
;; checker: flag a core-fn call ONLY when an argument's inferred type is
;; concrete AND lies in that op's error domain (the op provably throws on it).
;; Everything ambiguous — :any, :truthy (true/char/...), :nil — is accepted, so
;; there are no false positives. The table is curated to genuinely-throwing
;; cases; lenient ops ((get 5 :k) -> nil, (:k 5) -> nil) are NOT listed.

;; concrete non-numbers: arithmetic provably throws on these. A union is in the
;; error domain only when EVERY member is — if any member is an
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

;; concrete non-callable values: calling them throws "Cannot call X
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
  conj a diagnostic into env's diags cell. arg-types is the vector of inferred
  argument types; pos is the call form's source offset, carried into each
  diagnostic."
  [cn args arg-types pos env]
  (cond
    (contains? num-ops cn)
    (reduce (fn [_ i]
              (let [t (nth arg-types i)]
                (when (not-number? t)
                  (swap! (get env :diags) conj
                         {:op cn :argpos i :type (type-name t) :pos pos
                          :msg (str "`" cn "` requires a number, but argument "
                                    (inc i) " is " (type-name t))})))
              nil)
            nil (range (count args)))
    (and (contains? seq-ops cn) (> (count args) 0))
    (let [t (nth arg-types 0)]
      (when (not-seqable? t)
        (swap! (get env :diags) conj
               {:op cn :argpos 0 :type (type-name t) :pos pos
                :msg (str "`" cn "` requires "
                          (if (= cn "count") "a countable collection" "a seqable")
                          ", but argument 1 is " (type-name t))})))
    :else nil))

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

(defn- register-user-fn!
  "Record a (def name (fn [params] body)) — single fixed arity, not redefinable —
  for later user-fn call checking. Redefinable/dynamic and multi/variadic fns are
  skipped (their body is not a stable requirement)."
  [node env]
  (let [init (get node :init)
        m (get node :meta)
        redefable (and m (or (get m :redef) (get m :dynamic)))]
    (when (and (not redefable) (= :fn (get init :op)))
      (let [arities (get init :arities)]
        (when (= 1 (count arities))
          (let [ar (first arities)]
            (when (not (get ar :rest))
              (swap! (get env :user-sigs) assoc
                     (str (get node :ns) "/" (get node :name))
                     {:name (get node :name)
                      :params (get ar :params) :body (get ar :body)}))))))))

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
  collected-escapes after a full sweep)."
  [body tenv]
  (let [env (mk-env false false)
        r (infer body tenv env)]
    [(nth r 0) (nth r 1) @(get env :calls)]))

(defn reinfer-def
  "Re-run inference on a stashed :def's fn arity bodies with param types seeded
  (ptmap: param-name -> type), returning the def with annotated bodies. The back
  end emits the result directly (no further passes), so the param-typed lookups
  keep their specialization. Used by the inter-procedural recompile."
  [def-node ptmap]
  (let [fnode (get def-node :init)
        env (mk-env false false)
        shapes (get env :record-shapes)]
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
                                                     e (get shapes (nth pr 1))]
                                                 (if (and e (not (contains? m nm)))
                                                   (assoc m nm (record-type-from-entry e type-depth shapes))
                                                   m)))
                                             ptmap (get a :phints))]
                              (assoc a :body (nth (infer (get a :body) pt env) 1))))
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
  callee its arg is a Vec3."
  [params phints]
  (let [shapes (get @config-box :record-shapes)
        m (reduce (fn [acc pr] (assoc acc (nth pr 0) (nth pr 1))) {} phints)]
    (mapv (fn [nm]
            (let [ck (get m nm)
                  e (and ck (get shapes ck))]
              (when e (record-type-from-entry e type-depth shapes))))
          params)))

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
