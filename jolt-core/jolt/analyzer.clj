(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Depends only on the host contract (jolt.host) and IR
  constructors (jolt.ir). The contract fns are referred unqualified
  (host form predicates are `form-*` to avoid colliding with clojure.core), so the
  bootstrap can compile this namespace via its plain :var path. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Unsupported forms throw :jolt/uncompilable
  so the caller falls back to the interpreter (the hybrid contract).

  `env` carries lexical state: {:locals #{names} :recur recur-target-name|nil}.
  Definitions are ordered so only `analyze` (mutually recursive) is forward
  declared — the bootstrap compiles forward refs through var cells, but keeping
  them to one keeps the compiled namespace simple."
  (:require [jolt.ir :refer [const local var-ref the-var host-ref if-node do-node invoke
                             def-node let-node fn-node vector-node map-node set-node
                             quote-node throw-node host-static host-new]]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-list?
                               form-vec? form-map? form-set?
                               form-literal? form-keyword? form-elements form-vec-items
                               form-map-pairs form-set-items form-special? compile-ns
                               form-regex? form-regex-source
                               form-inst? form-inst-source form-uuid? form-uuid-source
                               form-bigdec? form-bigdec-source
                               form-ns-value? form-ns-value-name
                               form-macro? form-expand-1 resolve-global
                               form-sym-meta form-coll-meta host-intern! form-syntax-quote-lower
                               record-type? record-ctor-key form-position late-bind?
                               resolve-class-hint]]))

(declare analyze)

;; Special forms analyze-special has a dispatch arm for — the subset of the host
;; contract's reserved words (jolt.host/form-special?) the analyzer lowers itself.
;; The two differ deliberately (e.g. interop heads like `new`/`.` are reserved but
;; analyzed in analyze-list), so keep them in sync by intent, not by equality.
(def ^:private handled
  #{"quote" "if" "do" "def" "fn*" "let*" "loop*" "recur" "throw" "try"
    "syntax-quote" "var" "letfn" "set!" "defmacro"})

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

;; Type hints. The reader keeps ^hint metadata on the binding symbol.
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

;; A ^long / ^double tag -> :long / :double, else nil. The tag is a string on the
;; data reader; tolerate a symbol from macroexpansion too.
(defn- tag->nkind [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (cond (= s "double") :double (= s "long") :long :else nil)))

;; A primitive numeric hint (^long / ^double) on a binding symbol. Drives the
;; fl*/fx* fast path (jolt.passes.numeric).
(defn- nhint-of [ctx sym]
  (let [m (form-sym-meta sym)] (when m (tag->nkind (get m :tag)))))

;; Push a numeric return hint (from ^double/^long on a defn's name) onto each arity
;; of its fn, so the back end coerces the body's value to that kind on return —
;; making the hint a contract a caller's arithmetic can trust.
(defn- with-ret-nhint [node kind]
  (if (and kind (= :fn (:op node)))
    (assoc node :arities (mapv (fn [a] (if (:ret-nhint a) a (assoc a :ret-nhint kind)))
                               (:arities node)))
    node))

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
  (loop [i 0 fixed [] rest-name nil hints [] phints [] nhints []]
    (if (< i (count pvec))
      (let [p (nth pvec i)]
        (when-not (form-sym? p) (uncompilable "destructuring fn param"))
        (if (= "&" (form-sym-name p))
          (let [r (nth pvec (inc i))]
            (when-not (form-sym? r) (uncompilable "destructuring fn rest"))
            (recur (+ i 2) fixed (form-sym-name r) hints phints nhints))
          (let [nm (form-sym-name p) h (hint-of ctx p) ph (phint-of ctx p) nh (nhint-of ctx p)]
            (recur (inc i) (conj fixed nm) rest-name
                   (if h (conj hints [nm h]) hints)
                   (if ph (conj phints [nm ph]) phints)
                   (if nh (conj nhints [nm nh]) nhints)))))
      {:fixed fixed :rest rest-name :hints hints :phints phints :nhints nhints})))

;; Clojure lets a later param shadow an earlier same-named one (a macro expander
;; uses _ for both its &form and &env slots, so its param list is (_ _ …)); the
;; body binds the LAST occurrence. Chez rejects duplicate formals, so rename every
;; earlier duplicate to a fresh name — it is shadowed and unreferenceable.
(defn- uniquify-params [names]
  (let [n (count names)]
    (loop [i 0 out []]
      (if (< i n)
        (let [nm (nth names i)
              dup? (loop [j (inc i)]
                     (cond (>= j n) false
                           (= nm (nth names j)) true
                           :else (recur (inc j))))]
          (recur (inc i) (conj out (if dup? (gen-name (str nm "_")) nm))))
        out))))

(defn- analyze-arity [ctx pvec body env fn-name]
  (let [pp (parse-params ctx (vec (form-vec-items pvec)))
        fixed (uniquify-params (:fixed pp))
        rst (:rest pp)
        ;; Always a recur target, variadic included: the back end gives the rest
        ;; param an ordinary positional slot (holding the collected seq), so recur
        ;; is a self-call carrying the rest seq directly — Clojure semantics.
        ;; The recur target doubles as the COMPILED FN'S NAME, which is what a
        ;; host stack trace shows — so carry the Clojure ns/fn-name:
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
        arity (if (seq (:phints pp)) (assoc arity :phints (:phints pp)) arity)
        ;; numeric param hints (name -> :long/:double) for jolt.passes.numeric.
        arity (if (seq (:nhints pp)) (assoc arity :nhints (:nhints pp)) arity)]
    ;; :rest only when variadic — an absent :rest reads back nil, same as before,
    ;; but keeps a fixed arity a nil-free struct rather than a phm.
    (if rst (assoc arity :rest rst) arity)))

;; A reader that lowers ^meta on a collection to a runtime (with-meta <coll> <meta>)
;; form (the Chez data reader) wraps an arglist vector carrying a return-type hint
;; (^bytes [b] / ^String [x y]). Unwrap to the underlying vector so fn parsing sees
;; the params — the hint is ignored at runtime. Only the (with-meta <vec> _) shape
;; matches, so a real arity clause (head is a vector) and a
;; meta-on-vector arglist pass through unchanged.
(defn- strip-arglist-meta [form]
  (if (form-list? form)
    (let [es (vec (form-elements form))]
      (if (and (= 3 (count es))
               (form-sym? (first es))
               (= "with-meta" (form-sym-name (first es)))
               (form-vec? (nth es 1)))
        (nth es 1)
        form))
    form))

(defn- analyze-fn [ctx items env]
  (let [named (form-sym? (nth items 1))
        fn-name (when named (form-sym-name (nth items 1)))
        rest-items (if named (drop 2 items) (drop 1 items))
        first* (strip-arglist-meta (first rest-items))]
    (cond
      (form-vec? first*)
        (fn-node fn-name [(analyze-arity ctx first* (rest rest-items) env fn-name)])
      (form-list? first*)
        (fn-node fn-name
                 (mapv (fn [clause]
                         (let [cl (vec (form-elements clause))]
                           (analyze-arity ctx (strip-arglist-meta (first cl)) (rest cl) env fn-name)))
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
    ;; map-nil-representation trap, also avoided for def/fn/arity nodes. The
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
;; :let flagged :letrec so the back end knows the bindings forward-reference each
;; other: Chez lowers it to `letrec*`. The interpreter's shared mutable env already
;; gives the letrec semantics that a
;; compiled sequential let* lacks — the reason letfn was uncompilable before.
(defn- analyze-letfn [ctx items env]
  (let [specs (vec (form-vec-items (nth items 1)))
        names (mapv #(form-sym-name (first (vec (form-elements %)))) specs)
        env* (add-locals env names)
        binds (mapv (fn [spec]
                      (let [cl (vec (form-elements spec))]
                        ;; Build (fn name [params] body*) and analyze through the fn
                        ;; MACRO so destructuring params desugar (the fn* primitive
                        ;; would not — same trick defmacro uses). The named fn means
                        ;; self- and sibling-calls resolve and it carries its own name.
                        [(form-sym-name (first cl))
                         (analyze ctx (cons (symbol "fn") cl) env*)]))
                    specs)]
    {:op :let :letrec true :bindings binds
     :body (analyze-seq ctx (drop 2 items) env*)}))

;; A `.-field` head: `(.-field target)` is field access (the dash signals field
;; access to the host-call dispatcher). Defined above analyze-special so its set!
;; arm and analyze-list both reach it without a forward reference.
(defn- field-head? [nm]
  (and (> (count nm) 2) (= ".-" (subs nm 0 2))))

(defn- analyze-def [ctx items env]
  (let [name-sym (nth items 1)]
    ;; ^{:map} metadata reads as (def (with-meta name m) v): the metadata is a
    ;; runtime expression, so the interpreter evaluates the whole def.
    (when-not (form-sym? name-sym)
      (uncompilable "def name with map metadata"))
    (if (< (count items) 3)
      ;; (def name) with no init (declare): intern + reserve the cell so a forward
      ;; reference resolves; the back end keys on :no-init.
      (let [nm (form-sym-name name-sym) cur (compile-ns ctx)]
        (host-intern! ctx cur nm)
        {:op :def :ns cur :name nm :no-init true})
      ;; (def name docstring value): docstring is form 2, value form 3 — matching
      ;; the interpreter, else the docstring is taken as the value.
      (let [nm (form-sym-name name-sym)
            cur (compile-ns ctx)
            has-doc (and (> (count items) 3) (string? (nth items 2)))
            val-form (nth items (if has-doc 3 2))
            base0 (or (form-sym-meta name-sym) {})
            ;; resolve a ^Type hint to its canonical class name at def time, as the
            ;; JVM compiler does (^String -> java.lang.String); unknown hints pass.
            tag (get base0 :tag)
            tag-name (cond (form-sym? tag) (form-sym-name tag)
                           (string? tag) tag
                           :else nil)
            base-meta (if tag-name
                        (let [c (resolve-class-hint tag-name)]
                          (if c (assoc base0 :tag c) base0))
                        base0)
            node-meta (if has-doc (assoc base-meta :doc (nth items 2)) base-meta)]
        (host-intern! ctx cur nm)
        ;; a ^double/^long return hint on the name applies to all arities of the fn.
        (def-node cur nm (with-ret-nhint (analyze ctx val-form env) (tag->nkind tag)) node-meta)))))

;; (set! (.-field obj) v) mutates a deftype instance field in place; (set! *var* v)
;; sets the var's innermost thread binding, else its root. A local target (jolt
;; binds fields immutably) or any other shape is uncompilable.
(defn- analyze-set! [ctx items env]
  (let [target (nth items 1)
        val-node (analyze ctx (nth items 2) env)
        ti (when (form-list? target) (vec (form-elements target)))
        thead (when (and ti (pos? (count ti)) (form-sym? (first ti)))
                (form-sym-name (first ti)))]
    (cond
      (and thead (field-head? thead))
      {:op :set-field :obj (analyze ctx (nth ti 1) env)
       :field (subs thead 2) :val val-node}
      (form-sym? target)
      (do (when (local? env (form-sym-name target)) (uncompilable "set! of a local"))
          (let [r (resolve-global ctx target)]
            (when-not (= :var (:kind r)) (uncompilable "set! of a non-var"))
            {:op :set-var :the-var (the-var (:ns r) (:name r)) :val val-node}))
      :else (uncompilable "set! of an unsupported target"))))

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
    "def" (analyze-def ctx items env)
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
    ;; (set! *var* val): set the var's innermost thread binding, else its root
    ;; (jolt-var-set). A local target is a deftype mutable field — not yet
    ;; supported (jolt binds fields immutably); an interop (.-field) target too.
    ;; A defmacro that is not top-level (the spine intercepts those) — e.g. one
    ;; produced by a macro like (when … (defmacro …)). Lower it the way the spine
    ;; does: def the expander fn, then mark the var a macro at runtime so later
    ;; forms expand it. Strip a leading docstring / attr-map, as defmacro allows.
    "defmacro" (let [name-sym (nth items 1)
                     nm (form-sym-name name-sym)
                     cur (compile-ns ctx)
                     after (drop 2 items)
                     after (if (string? (first after)) (rest after) after)
                     after (if (form-map? (first after)) (rest after) after)
                     ;; build (fn params body…) and analyze it through the fn MACRO
                     ;; so a destructuring macro arglist desugars (the fn* primitive
                     ;; would not), then def it and mark the var a macro.
                     fn-form (cons (symbol "fn") after)]
                 (host-intern! ctx cur nm)
                 {:op :defmacro :ns cur :name nm
                  :fn (analyze ctx fn-form env)})
    "set!" (analyze-set! ctx items env)
    (uncompilable (str "special form " op))))

;; Host interop method call. `(.method target arg*)` — a head that
;; starts with "." but not ".-" (field access stays punted). Analyzes to a
;; :host-call node; the Chez back end lowers it to a jolt-host-call dispatch.
(defn- method-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm 0 1))
       (not (= "-" (subs nm 1 2)))     ; .-field is field access
       (not (= "." (subs nm 1 2)))))   ; .. is the threading macro, not .method

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
;; token and the analyzed args. The Chez back end lowers it to a runtime
;; constructor dispatch.
(defn- analyze-ctor [ctx class args env]
  (host-new class (mapv #(analyze ctx % env) args)))

;; jolt.ffi/__cfn: the low-level foreign-function form a jolt library
;; uses (via the jolt.ffi/foreign-fn macro) to bind native code. Shape:
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype)            ; non-blocking
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype :blocking)  ; may block
;; The C symbol is a string literal and the types are literal keywords, read here
;; at compile time; the Chez back end lowers it to a real `foreign-procedure`
;; (typed marshaling, no runtime eval). A :blocking call is emitted __collect_safe
;; so it deactivates the thread for the call — a blocking call (accept/recv/...)
;; must not pin the stop-the-world collector. A leaf IR node.
(defn- analyze-ffi-fn [ctx items env]
  (when-not (<= 4 (count items) 5)
    (throw (str "jolt.ffi/foreign-fn expects (foreign-fn \"sym\" [argtypes] rettype [:blocking])")))
  {:op :ffi-fn
   :csym (nth items 1)
   :argtypes (mapv name (form-vec-items (nth items 2)))
   :rettype (name (nth items 3))
   :blocking (and (= 5 (count items)) (= "blocking" (name (nth items 4))))})

;; jolt.ffi/__ccallable: the foreign-CALLBACK form (via the jolt.ffi/foreign-callable
;; macro) — the inverse of __cfn. It wraps a jolt fn as a C-callable function
;; pointer so C can call back INTO jolt (GTK signal handlers, qsort comparators).
;; Shape:
;;   (jolt.ffi/__ccallable f [:argtype ...] :rettype)                ; thread stays active
;;   (jolt.ffi/__ccallable f [:argtype ...] :rettype :collect-safe)  ; may be invoked
;;                                                                   ; while the thread is
;;                                                                   ; parked in a :blocking call
;; Unlike __cfn, the fn is a CHILD expression (analyzed + walked by the passes);
;; the types are literal keywords read at compile time. The Chez back end lowers
;; it to a locked `foreign-callable` and returns its entry-point address (a jolt
;; pointer). :collect-safe is required when C invokes the callback from a thread
;; that is deactivated inside a :blocking foreign call (e.g. a GTK main loop).
(defn- analyze-ffi-callable [ctx items env]
  (when-not (<= 4 (count items) 5)
    (throw (str "jolt.ffi/foreign-callable expects (foreign-callable f [argtypes] rettype [:collect-safe])")))
  {:op :ffi-callable
   :fn (analyze ctx (nth items 1) env)
   :argtypes (mapv name (form-vec-items (nth items 2)))
   :rettype (name (nth items 3))
   :collect-safe (and (= 5 (count items)) (= "collect-safe" (name (nth items 4))))})

;; The `.` special form: `(. target member arg*)` — member access / method call.
;; A symbol member whose name starts with "-" is a field read; otherwise it is a
;; method (call with the trailing args). Both lower to a :host-call carrying the
;; member name verbatim (the leading "-" survives so the runtime dispatcher reads
;; it as a field). The Chez back end dispatches it through record-method-dispatch.
(defn- analyze-dot [ctx items env]
  (when (< (count items) 3)
    (throw (str "Malformed (. target member ...) form")))
  (let [member (nth items 2)]
    (cond
      (form-sym? member)
        {:op :host-call
         :method (form-sym-name member)
         :target (analyze ctx (nth items 1) env)
         :args (mapv #(analyze ctx % env) (drop 3 items))}
      ;; (. obj :kw) is a keyword lookup — invoke the keyword on the target.
      (form-keyword? member)
        (invoke (analyze ctx member env) [(analyze ctx (nth items 1) env)])
      :else (uncompilable "special form . (non-symbol member)"))))

(defn- analyze-field [ctx hname items env]
  (when (< (count items) 2)
    (throw (str "Malformed (.-field target) form")))
  {:op :host-call
   :method (subs hname 1)        ; ".-field" -> "-field"
   :target (analyze ctx (nth items 1) env)
   :args []})

(defn- analyze-symbol [ctx form env]
  (let [nm (form-sym-name form) ns (form-sym-ns form)]
    (cond
      (and (nil? ns) (local? env nm))
        (let [h (get (:hints env) nm)] (if h (assoc (local nm) :hint h) (local nm)))
      ns (let [r (resolve-global ctx form)]
           (if (= :var (:kind r))
             (cond-> (var-ref (:ns r) (:name r)) (:num-ret r) (assoc :num-ret (:num-ret r)))
             ;; A non-var qualified ref `Class/member` is a host class static
             ;; (Math/sqrt, Long/MAX_VALUE, System/getenv). The Chez back end
             ;; lowers it to a runtime static dispatch.
             (host-static ns nm)))
      :else (let [r (resolve-global ctx form)]
              (case (:kind r)
                ;; :num-ret (a ^double/^long declared return) rides on the var node so
                ;; jolt.passes.numeric types a call to it (an accumulator over the result).
                :var (cond-> (var-ref (:ns r) (:name r)) (:num-ret r) (assoc :num-ret (:num-ret r)))
                :host (host-ref (:name r))
                ;; :unresolved — emitting a var-ref here would auto-intern an
                ;; UNBOUND var, so a typo'd symbol would die later as 'Cannot call
                ;; nil as a function' with no hint which symbol.
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
          ;; Canonical order (Clojure/CLJS analyze-seq): macroexpand FIRST, then
          ;; dispatch special forms / interop / invoke. Expanding before the
          ;; special-form check means a head that is a macro always expands — even
          ;; one whose name is also in the special-form set — matching reference
          ;; read -> macroexpand -> analyze. A local shadows both.
          (and (form-sym? head) (not shadowed) (form-macro? ctx head))
            (analyze ctx (form-expand-1 ctx form) env)
          ;; jolt.ffi/__cfn — the foreign-function special form (always emitted
          ;; fully-qualified by the jolt.ffi/foreign-fn macro, so aliases resolve).
          (and (form-sym? head) (= "jolt.ffi" (form-sym-ns head))
               (= "__cfn" (form-sym-name head)))
            (analyze-ffi-fn ctx items env)
          ;; jolt.ffi/__ccallable — the foreign-callback special form (the fn is a
          ;; child expression, analyzed here).
          (and (form-sym? head) (= "jolt.ffi" (form-sym-ns head))
               (= "__ccallable" (form-sym-name head)))
            (analyze-ffi-callable ctx items env)
          ;; special-form heads are NOT shadowable (unlike macros): a local named
          ;; `if` does not change the meaning of (if …) in operator position, per
          ;; spec §3 and the reference. No (not shadowed) guard here.
          (and hname (contains? handled hname))
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
          ;; (. target member arg*) — the `.` special form.
          (and (= hname ".") (not shadowed))
            (analyze-dot ctx items env)
          ;; (.-field target) — field-access head.
          (and hname (not shadowed) (field-head? hname))
            (analyze-field ctx hname items env)
          (and hname (not shadowed) (form-special? hname))
            (uncompilable (str "special form " hname))
          :else
            ;; stamp the list form's source offset onto the :invoke
            ;; so the success checker can report file:line:col. nil when the
            ;; reader did not record it (synthetic/macro-built forms).
            (let [n (invoke (analyze ctx head env)
                            (mapv #(analyze ctx % env) (rest items)))
                  p (form-position form)]
              (if p (assoc n :pos p) n)))))))

;; A vector/map/set literal carrying reader metadata (^:foo {…}, ^{:tag :int} [1])
;; keeps it as a runtime value: wrap the collection node in (with-meta coll meta).
;; The metadata is itself a form (its values may be expressions, ^{:a (f)}), so
;; analyze it. nil meta passes the node through. Arglist vectors never reach here —
;; analyze-arity reads their items directly — so a ^Type [args] hint is not wrapped.
(defn- with-coll-meta [ctx form env node]
  (let [m (form-coll-meta form)]
    (if (nil? m)
      node
      (invoke (var-ref "clojure.core" "with-meta") [node (analyze ctx m env)]))))

(defn analyze
  ([ctx form] (analyze ctx form (empty-env)))
  ([ctx form env]
   (cond
     (form-literal? form) (const form)
     (form-sym? form) (analyze-symbol ctx form env)
     (form-vec? form) (with-coll-meta ctx form env
                        (vector-node (mapv #(analyze ctx % env) (form-vec-items form))))
     (form-map? form) (with-coll-meta ctx form env
                        (map-node (mapv (fn [p] [(analyze ctx (first p) env)
                                                 (analyze ctx (second p) env)])
                                        (form-map-pairs form))))
     (form-set? form) (with-coll-meta ctx form env
                        (set-node (mapv #(analyze ctx % env) (form-set-items form))))
     (form-list? form) (analyze-list ctx form env)
     ;; regex literal #"…" -> a :regex IR node (leaf). The Chez back end emits a
     ;; jolt-regex value over the vendored irregex.
     (form-regex? form) {:op :regex :source (form-regex-source form)}
     ;; #inst / #uuid literals -> :inst / :uuid IR leaves. The Chez back
     ;; end emits a runtime inst/uuid value (host/chez/inst-time.ss).
     (form-inst? form) {:op :inst :source (form-inst-source form)}
     (form-uuid? form) {:op :uuid :source (form-uuid-source form)}
     ;; bigdecimal literal (1.5M) -> a :bigdec leaf; the back end emits a runtime
     ;; jbigdec built from the numeric text.
     (form-bigdec? form) {:op :bigdec :source (form-bigdec-source form)}
     ;; a live namespace value spliced into a form (~*ns* in a macro) -> a
     ;; :the-ns leaf the back end reconstructs by name at the call site.
     (form-ns-value? form) {:op :the-ns :name (form-ns-value-name form)}
     :else (uncompilable "unsupported form"))))
