(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Pure jolt-core — depends only on the host contract (jolt.host) and IR
  constructors (jolt.ir), never on Janet. The contract fns are referred unqualified
  (host form predicates are `form-*` to avoid colliding with clojure.core), so the
  bootstrap can compile this namespace via its plain :var path. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Coverage grows toward compiler.janet; unsupported forms throw :jolt/uncompilable
  so the caller falls back to the interpreter (the hybrid contract).

  `env` carries lexical state: {:locals #{names} :recur recur-target-name|nil}.
  Definitions are ordered so only `analyze` (mutually recursive) is forward
  declared — the bootstrap compiles forward refs through var cells, but keeping
  them to one keeps the compiled namespace simple."
  (:require [jolt.ir :refer [const local var-ref the-var host-ref if-node do-node invoke
                             def-node let-node fn-node vector-node map-node set-node
                             quote-node throw-node host-static host-new]]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-list?
                               form-vec? form-map? form-set? form-char?
                               form-literal? form-elements form-vec-items
                               form-map-pairs form-set-items form-special? compile-ns
                               form-regex? form-regex-source
                               form-macro? form-expand-1 resolve-global
                               form-sym-meta host-intern! form-syntax-quote-lower
                               record-type? record-ctor-key form-position late-bind?]]))

(declare analyze)

(def ^:private handled
  #{"quote" "if" "do" "def" "fn*" "let*" "loop*" "recur" "throw" "try"
    "syntax-quote" "var" "letfn"})

(defn- uncompilable [why]
  (throw (str "jolt/uncompilable: " why)))

(def ^:private gensym-counter (atom 0))
(defn- gen-name [prefix]
  (let [n @gensym-counter]
    (swap! gensym-counter inc)
    (str "_r$" prefix n)))

(defn- empty-env [] {:locals #{} :hints {}})
(defn- local? [env nm] (contains? (:locals env) nm))
(defn- add-locals [env names] (update env :locals #(reduce conj % names)))
(defn- with-recur [env name] (assoc env :recur name))

;; Type hints (jolt-94n). The reader keeps ^hint metadata on the binding symbol.
;; Two hints resolve to the :struct fast path (a constant-keyword lookup skips
;; the :jolt/type guard and emits a bare get): ^:struct (a plain struct/record
;; map) and ^TypeName where TypeName is a defrecord/deftype (its instances are
;; tagged :jolt/deftype, not :jolt/type, so a raw get is correct). Every other
;; hint (^String, ^long, ...) parses and is ignored, as before.
(defn- hint-of [ctx sym]
  (let [m (form-sym-meta sym)]
    (cond
      (nil? m) nil
      (get m :struct) :struct
      :else (let [t (get m :tag)]
              (when (and t (record-type? ctx t)) :struct)))))
(defn- add-hint [env nm h]
  (if h (assoc env :hints (assoc (:hints env) nm h)) env))

;; The resolved record ctor-key ("ns/->Name") for a ^Type param hint, or nil.
;; Unlike hint-of (which collapses any record hint to the coarse :struct guard-
;; skip marker), this carries the SPECIFIC record type — cross-namespace aware —
;; so the inference can seed the param's type and read its fields shaped/typed,
;; not just :any (the lever for a typed multi-namespace program without whole-
;; program inference).
(defn- phint-of [ctx sym]
  (let [m (form-sym-meta sym)]
    (when m (let [t (get m :tag)] (when t (record-ctor-key ctx t))))))

(defn- analyze-seq [ctx forms env]
  (let [v (mapv #(analyze ctx % env) forms)
        n (count v)]
    (cond
      (zero? n) (const nil)
      (= 1 n) (first v)
      :else (do-node (subvec v 0 (dec n)) (peek v)))))

(defn- analyze-bindings [ctx bvec env]
  (loop [i 0 env env pairs []]
    (if (< i (count bvec))
      (let [bsym (nth bvec i)]
        (when-not (form-sym? bsym) (uncompilable "destructuring binding"))
        (let [nm (form-sym-name bsym)
              init (analyze ctx (nth bvec (inc i)) env)]
          (recur (+ i 2) (add-hint (add-locals env [nm]) nm (hint-of ctx bsym))
                 (conj pairs [nm init]))))
      [pairs env])))

(defn- parse-params [ctx pvec]
  ;; :hints is a vector of [name hint] pairs (vector, not a map, so the caller
  ;; folds it with a plain reduce — no reduce-over-map in the kernel subset).
  ;; :phints is the parallel vector of [name ctor-key] for record param hints,
  ;; carrying the specific type for the inference to seed.
  (loop [i 0 fixed [] rest-name nil hints [] phints []]
    (if (< i (count pvec))
      (let [p (nth pvec i)]
        (when-not (form-sym? p) (uncompilable "destructuring fn param"))
        (if (= "&" (form-sym-name p))
          (let [r (nth pvec (inc i))]
            (when-not (form-sym? r) (uncompilable "destructuring fn rest"))
            (recur (+ i 2) fixed (form-sym-name r) hints phints))
          (let [nm (form-sym-name p) h (hint-of ctx p) ph (phint-of ctx p)]
            (recur (inc i) (conj fixed nm) rest-name
                   (if h (conj hints [nm h]) hints)
                   (if ph (conj phints [nm ph]) phints)))))
      {:fixed fixed :rest rest-name :hints hints :phints phints})))

(defn- analyze-arity [ctx pvec body env fn-name]
  (let [pp (parse-params ctx (vec (form-vec-items pvec)))
        fixed (:fixed pp)
        rst (:rest pp)
        ;; Always a recur target, variadic included: the back end gives the rest
        ;; param an ordinary positional slot (holding the collected seq), so recur
        ;; is a self-call carrying the rest seq directly — Clojure semantics.
        ;; The recur target doubles as the COMPILED FN'S NAME, which is what a
        ;; janet stack trace shows — so carry the Clojure ns/fn-name (jolt-2o7.1):
        ;; an error inside app.deep/level3 traces as _r$app.deep/level3--N
        ;; (report-error demangles the _r$/--N wrapper). gen-name's counter
        ;; keeps recur targets unique per compilation unit.
        rname (gen-name (str (compile-ns ctx) "/" (or fn-name "fn") "--"))
        names (cond-> (vec fixed) rst (conj rst) fn-name (conj fn-name))
        env0 (-> (add-locals env names) (with-recur rname))
        env* (reduce (fn [e pr] (add-hint e (nth pr 0) (nth pr 1))) env0 (:hints pp))
        arity {:params fixed :recur-name rname
               :body (analyze-seq ctx body env*)}
        ;; carry record param hints (name -> ctor-key) for the inference to seed
        ;; the param type; only when present so a hintless arity stays a struct.
        arity (if (seq (:phints pp)) (assoc arity :phints (:phints pp)) arity)]
    ;; :rest only when variadic — an absent :rest reads back nil, same as before,
    ;; but keeps a fixed arity a nil-free struct rather than a phm.
    (if rst (assoc arity :rest rst) arity)))

(defn- analyze-fn [ctx items env]
  (let [named (form-sym? (nth items 1))
        fn-name (when named (form-sym-name (nth items 1)))
        rest-items (if named (drop 2 items) (drop 1 items))
        first* (first rest-items)]
    (cond
      (form-vec? first*)
        (fn-node fn-name [(analyze-arity ctx first* (rest rest-items) env fn-name)])
      (form-list? first*)
        (fn-node fn-name
                 (mapv (fn [clause]
                         (let [cl (vec (form-elements clause))]
                           (analyze-arity ctx (first cl) (rest cl) env fn-name)))
                       rest-items))
      :else (uncompilable "fn: bad params"))))

(defn- analyze-try [ctx items env]
  (let [clauses (rest items)
        body (atom [])
        catch-sym (atom nil)
        catch-body (atom nil)
        finally-body (atom nil)]
    (doseq [c clauses]
      (let [head (when (form-list? c) (first (vec (form-elements c))))
            hname (when (and head (form-sym? head)) (form-sym-name head))]
        (cond
          (= hname "catch")
            (let [cl (vec (form-elements c))]
              ;; (catch class binding body*) — binding (3rd elem) MUST be a symbol.
              ;; Validate eagerly (plain throw, NOT uncompilable, so it's a real
              ;; error rather than a compile->interpret punt) instead of letting
              ;; form-sym-name crash on a non-symbol.
              (when (or (< (count cl) 3) (not (form-sym? (nth cl 2))))
                (throw "Unable to parse catch clause; expected (catch class binding body*)"))
              (reset! catch-sym (form-sym-name (nth cl 2)))
              (reset! catch-body (drop 3 cl)))
          (= hname "finally")
            (reset! finally-body (rest (vec (form-elements c))))
          :else (swap! body conj c))))
    ;; Add :catch-sym/:catch-body/:finally ONLY when present (same discipline as
    ;; the arity :rest key above). Assoc'ing them nil-when-absent would give the
    ;; node a nil-valued key, which makes it a phm in jolt's map representation
    ;; and forces the back end to densify it (norm-node) before reading :op — the
    ;; map-nil-representation trap Phase 2 cleaned up for def/fn/arity nodes. The
    ;; back end reads each key with a nil-safe (node :k) and gates on it, so an
    ;; absent key is indistinguishable from a present-nil one.
    (let [n {:op :try :body (analyze-seq ctx @body env)}
          n (if @catch-body
              (assoc n :catch-sym @catch-sym
                       :catch-body (analyze-seq ctx @catch-body (add-locals env [@catch-sym])))
              n)
          n (if @finally-body
              (assoc n :finally (analyze-seq ctx @finally-body env))
              n)]
      n)))

;; letfn: (letfn [(name [params] body*)...] body*). The named local fns are
;; MUTUALLY recursive, so bind every name into the env BEFORE analyzing any spec
;; — each spec then resolves its siblings (and itself) as locals. Emitted as a
;; :let flagged :letrec so the back ends know the bindings forward-reference each
;; other: Chez lowers it to `letrec*`; the Janet back end punts to the
;; interpreter (its shared mutable env already gives the letrec semantics that a
;; compiled sequential let* lacks — the reason letfn was uncompilable before).
(defn- analyze-letfn [ctx items env]
  (let [specs (vec (form-vec-items (nth items 1)))
        names (mapv #(form-sym-name (first (vec (form-elements %)))) specs)
        env* (add-locals env names)
        binds (mapv (fn [spec]
                      (let [cl (vec (form-elements spec))]
                        ;; analyze as a named fn (items[1] = the name): self- and
                        ;; sibling-calls resolve, the fn carries its own name.
                        [(form-sym-name (first cl))
                         (analyze-fn ctx (vec (cons (first cl) cl)) env*)]))
                    specs)]
    {:op :let :letrec true :bindings binds
     :body (analyze-seq ctx (drop 2 items) env*)}))

(defn- analyze-special [ctx op items env]
  (case op
    "quote" (quote-node (second items))
    "if" (do
           ;; 2 or 3 argument forms only (spec 03-special-forms X1)
           (when (or (< (count items) 3) (> (count items) 4))
             (throw (str "Wrong number of args (" (dec (count items)) ") passed to: if")))
           (if-node (analyze ctx (nth items 1) env)
                    (analyze ctx (nth items 2) env)
                    (if (> (count items) 3)
                      (analyze ctx (nth items 3) env)
                      (const nil))))
    "do" (analyze-seq ctx (rest items) env)
    "throw" (throw-node (analyze ctx (nth items 1) env))
    "def" (let [name-sym (nth items 1)]
            ;; ^{:map} metadata reads as (def (with-meta name m) v) — the
            ;; metadata is a runtime expression, so the interpreter evaluates
            ;; the whole def (it unwraps the name and merges the meta).
            (when-not (form-sym? name-sym)
              (uncompilable "def name with map metadata"))
            (if (< (count items) 3)
              ;; (def name) with no init (declare): intern + reserve the cell so a
              ;; forward reference resolves. The back ends key on :no-init — Chez
              ;; def-var!s an unbound placeholder; the Janet back end punts to the
              ;; interpreter, which interns a genuinely-unbound var.
              (let [nm (form-sym-name name-sym) cur (compile-ns ctx)]
                (host-intern! ctx cur nm)
                {:op :def :ns cur :name nm :no-init true})
              (let [nm (form-sym-name name-sym)
                    cur (compile-ns ctx)
                    ;; (def name docstring value): docstring is form 2, value form 3.
                    ;; Matches the interpreter; without this the docstring was taken
                    ;; as the value and the real init dropped (jolt-6ym).
                    has-doc (and (> (count items) 3) (string? (nth items 2)))
                    val-form (nth items (if has-doc 3 2))
                    base-meta (or (form-sym-meta name-sym) {})
                    node-meta (if has-doc (assoc base-meta :doc (nth items 2)) base-meta)]
                (host-intern! ctx cur nm)
                (def-node cur nm (analyze ctx val-form env) node-meta))))
    "let*" (let [bvec (vec (form-vec-items (nth items 1)))
                 r (analyze-bindings ctx bvec env)]
             (let-node (first r) (analyze-seq ctx (drop 2 items) (second r))))
    "loop*" (let [bvec (vec (form-vec-items (nth items 1)))
                  rname (gen-name "loop")
                  r (analyze-bindings ctx bvec env)
                  env** (with-recur (second r) rname)]
              {:op :loop :recur-name rname :bindings (first r)
               :body (analyze-seq ctx (drop 2 items) env**)})
    "recur" (let [rt (:recur env)]
              (when-not rt (uncompilable "recur outside loop/fn"))
              {:op :recur :recur-name rt
               :args (mapv #(analyze ctx % env) (rest items))})
    "try" (analyze-try ctx items env)
    "letfn" (analyze-letfn ctx items env)
    "fn*" (analyze-fn ctx items env)
    ;; Lower the backtick to construction code (zero runtime cost), then analyze
    ;; it — the macroexpand/compile-time step, per read -> macroexpand -> compile.
    "syntax-quote" (analyze ctx (form-syntax-quote-lower ctx (second items)) env)
    "var" (let [sym (second items)
                r (resolve-global ctx sym)]
            (if (= :var (:kind r))
              (the-var (:ns r) (:name r))
              (uncompilable (str "var of non-var " (form-sym-name sym)))))
    (uncompilable (str "special form " op))))

;; Host interop method call (jolt-0kf5). `(.method target arg*)` — a head that
;; starts with "." but not ".-" (field access stays punted). Analyzes to a
;; :host-call node; the Janet back end punts it at emit (no interop model -> the
;; interpreter runs it), the Chez back end lowers it to a jolt-host-call dispatch.
(defn- method-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm 0 1))
       (not (= "-" (subs nm 1 2)))))

(defn- analyze-host-call [ctx hname items env]
  (when (< (count items) 2)
    (throw (str "Malformed member expression, expecting (.method target ...): " hname)))
  {:op :host-call
   :method (subs hname 1)
   :target (analyze ctx (nth items 1) env)
   :args (mapv #(analyze ctx % env) (drop 2 items))})

;; A constructor head: `Class.` — a symbol ending in "." (but not the member
;; access `.method` / `..` forms). `(Class. args*)` builds an instance.
(defn- ctor-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm (dec (count nm)) (count nm)))
       (not (= "." (subs nm 0 1)))))

;; `(Class. args*)` and `(new Class args*)` -> a :host-new node carrying the class
;; token and the analyzed args. The Janet back end punts it (the interpreter runs
;; the constructor from its class-ctors registry); the Chez back end lowers it to
;; a runtime constructor dispatch (jolt-avt6).
(defn- analyze-ctor [ctx class args env]
  (host-new class (mapv #(analyze ctx % env) args)))

(defn- analyze-symbol [ctx form env]
  (let [nm (form-sym-name form) ns (form-sym-ns form)]
    (cond
      (and (nil? ns) (local? env nm))
        (let [h (get (:hints env) nm)] (if h (assoc (local nm) :hint h) (local nm)))
      ns (let [r (resolve-global ctx form)]
           (if (= :var (:kind r))
             (var-ref (:ns r) (:name r))
             ;; A non-var qualified ref `Class/member` is a host class static
             ;; (Math/sqrt, Long/MAX_VALUE, System/getenv). The Janet back end
             ;; punts the :host-static node (the interpreter resolves it from its
             ;; class-statics registry, exactly as it did when this was an
             ;; uncompilable); the Chez back end lowers it to a runtime static
             ;; dispatch (jolt-avt6).
             (host-static ns nm)))
      :else (let [r (resolve-global ctx form)]
              (case (:kind r)
                :var (var-ref (:ns r) (:name r))
                :host (host-ref (:name r))
                ;; :unresolved — previously emitted a var-ref that auto-interned
                ;; an UNBOUND var, so a typo'd symbol died later as 'Cannot call
                ;; nil as a function' with no hint which symbol (jolt-2o7.3).
                ;; Punt to the interpreter: its resolver raises Clojure's
                ;; 'Unable to resolve symbol' when the form actually runs (at
                ;; eval for top-level forms, at call for fn bodies). A punt
                ;; rather than a hard throw because runtime-interning forms
                ;; (defmulti's setup call) legitimately reference the var they
                ;; are about to create when nested in a non-top-level do. Real
                ;; forward references want (declare ...), as in Clojure.
                ;; Under late-bind? (the Chez back end, which has no interpreter
                ;; to punt to) an unresolved symbol instead lowers to a var-ref
                ;; against the compile ns — resolved at runtime, the open-world
                ;; semantics of -e — so defmulti/defmethod forward references work.
                (if (late-bind? ctx)
                  (var-ref (compile-ns ctx) nm)
                  (uncompilable (str "Unable to resolve symbol: " nm " in this context"))))))))

(defn- analyze-list [ctx form env]
  (let [items (vec (form-elements form))]
    (if (zero? (count items))
      (quote-node form)
      (let [head (first items)
            hname (when (and (form-sym? head) (nil? (form-sym-ns head))) (form-sym-name head))
            shadowed (and hname (local? env hname))]
        (cond
          (and hname (not shadowed) (contains? handled hname))
            (analyze-special ctx hname items env)
          (and hname (not shadowed) (method-head? hname))
            (analyze-host-call ctx hname items env)
          ;; (Class. args*) — trailing-dot constructor sugar.
          (and hname (not shadowed) (ctor-head? hname))
            (analyze-ctor ctx (subs hname 0 (dec (count hname))) (rest items) env)
          ;; (new Class args*) — explicit constructor.
          (and (= hname "new") (not shadowed) (>= (count items) 2)
               (form-sym? (nth items 1)))
            (analyze-ctor ctx (form-sym-name (nth items 1)) (drop 2 items) env)
          (and hname (not shadowed) (form-special? hname))
            (uncompilable (str "special form " hname))
          (and (form-sym? head) (not shadowed) (form-macro? ctx head))
            (analyze ctx (form-expand-1 ctx form) env)
          :else
            ;; stamp the list form's source offset onto the :invoke (jolt-fqy)
            ;; so the success checker can report file:line:col. nil when the
            ;; reader did not record it (synthetic/macro-built forms).
            (let [n (invoke (analyze ctx head env)
                            (mapv #(analyze ctx % env) (rest items)))
                  p (form-position form)]
              (if p (assoc n :pos p) n)))))))

(defn analyze
  ([ctx form] (analyze ctx form (empty-env)))
  ([ctx form env]
   (cond
     (form-literal? form) (const form)
     (form-sym? form) (analyze-symbol ctx form env)
     (form-vec? form) (vector-node (mapv #(analyze ctx % env) (form-vec-items form)))
     (form-map? form) (map-node (mapv (fn [p] [(analyze ctx (first p) env)
                                              (analyze ctx (second p) env)])
                                     (form-map-pairs form)))
     (form-set? form) (set-node (mapv #(analyze ctx % env) (form-set-items form)))
     (form-list? form) (analyze-list ctx form env)
     ;; regex literal #"…" -> a :regex IR node (leaf). The Janet back end punts it
     ;; (interpreter compiles via the seed PEG engine); the Chez back end emits a
     ;; jolt-regex value over the vendored irregex.
     (form-regex? form) {:op :regex :source (form-regex-source form)}
     :else (uncompilable "unsupported form"))))
