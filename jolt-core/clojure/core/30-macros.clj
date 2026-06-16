;; clojure.core — macro tier. Macros expressed in Clojure (defmacro + syntax-quote)
;; rather than as hand-built Janet form-transformers. Loaded after the fn tiers,
;; so a macro here may use any already-frozen core fn/macro.
;;
;; IMPORTANT — only macros NOT used by the self-hosted compiler (jolt-core/jolt/*)
;; or by the earlier overlay tiers belong here; those (and/or/when/when-not/
;; when-let/cond/case/doseq/declare/cond->/->) must stay available before this
;; tier loads, so they remain in Janet for now. Everything here is user-facing.
;;
;; Migration: remove the Janet core-X macro fn AND its core-macro-names entry when
;; moving a macro here (defmacro installs the :macro flag itself).

(defmacro comment [& body] nil)

;; with-out-str: capture everything the body prints to *out* and return it as a
;; string. __with-out-str (clojure.core) runs the thunk with the output captured.
(defmacro with-out-str [& body]
  `(__with-out-str (fn* [] ~@body)))

;; defmulti/defmethod are sugar over defmulti-setup/defmethod-setup (ctx-capturing
;; clojure.core fns) so they compile as plain invokes. name/mm are passed quoted;
;; the dispatch fn, options, and dispatch value evaluate normally, and the method
;; body becomes a compiled (fn …).
;; Clojure allows (defmulti name docstring? attr-map? dispatch-fn & options);
;; drop a leading docstring and/or attr-map so the dispatch fn isn't mistaken for
;; one (migratus's multimethods carry docstrings).
(defmacro defmulti [name & args]
  (let [args (if (string? (first args)) (rest args) args)
        args (if (and (map? (first args)) (not (symbol? (first args)))) (rest args) args)
        dispatch (first args)
        opts (rest args)]
    `(defmulti-setup (quote ~name) ~dispatch ~@opts)))

(defmacro defmethod [mm dispatch-val & fn-tail]
  `(defmethod-setup (quote ~mm) ~dispatch-val (fn ~@fn-tail)))

;; Multimethod table ops (tier 6c): a multimethod's method table lives on its
;; VAR (the value is just the dispatch closure), so these pass the name quoted
;; to ctx-capturing setups — the same shape as defmulti/defmethod above.
(defmacro prefer-method [mm dval-a dval-b]
  `(prefer-method-setup (quote ~mm) ~dval-a ~dval-b))

(defmacro remove-method [mm dval]
  `(remove-method-setup (quote ~mm) ~dval))

(defmacro remove-all-methods [mm]
  `(remove-all-methods-setup (quote ~mm)))

;; methods/get-method take the multimethod VALUE (Clojure semantics); the setup
;; maps it back to its var via the registry, so a bare multifn ref works from a
;; compiled fn in any namespace (jolt multimethod table-visibility fix).
(defmacro get-method [mm dval]
  `(get-method-setup ~mm ~dval))

(defmacro methods [mm]
  `(methods-setup ~mm))

;; prefers reads the store off the VAR (the multifn value can't carry it) —
;; same symbol-passing shape as the other multimethod table ops.
(defmacro prefers [mm]
  `(prefers-setup (quote ~mm)))

;; instance?: class names don't evaluate to values on jolt, so the type arg is
;; passed quoted to the ctx-capturing checker; the value evaluates normally.
;; A LIST in type position is a class-valued expression (e.g. Selmer's
;; (Class/forName "[C")) — evaluate it instead.
(defmacro instance? [t x]
  (if (seq? t)
    `(instance-check ~t ~x)
    `(instance-check (quote ~t) ~x)))

;; Single-threaded host: evaluate the monitor expr (for its effects, matching
;; Clojure's evaluation order) and the body — no lock to take.
(defmacro locking [x & body]
  `(do ~x ~@body))

;; defonce: define name only if it isn't already bound to a non-nil root;
;; returns the existing var untouched otherwise (matching the prior arm).
;; time: evaluate expr, print the elapsed wall-clock, return the value.
;; current-time-ms is the host's monotonic clock.
(defmacro time [expr]
  `(let [start# (current-time-ms)
         ret# ~expr]
     (println (str "Elapsed time: " (- (current-time-ms) start#) " msecs"))
     ret#))

;; with-redefs: temporary root rebinding, restored on exit (incl. throw).
;; Builds (hash-map (var n1) v1 ...) — a call form, since map-literal forms
;; can't carry call forms as keys.
(defmacro with-redefs [bindings & body]
  (let [pairs (reduce (fn [acc p] (conj (conj acc `(var ~(first p))) (second p)))
                      [] (partition 2 bindings))]
    `(with-redefs-fn (hash-map ~@pairs) (fn [] ~@body))))

;; Fresh free-standing var cells bound as locals; read/write with
;; var-get/var-set. The cells come from the host seam __local-var.
(defmacro with-local-vars [bindings & body]
  (let [binds (reduce (fn [acc p] (conj (conj acc (first p)) `(__local-var ~(second p))))
                      [] (partition 2 bindings))]
    `(let [~@binds] ~@body)))

;; Canonical recursive expansion; closing goes through the host seam __close
;; (a map-like value's :close fn or a host file — no .close interop here).
(defmacro with-open [bindings & body]
  (if (zero? (count bindings))
    `(do ~@body)
    `(let [~(first bindings) ~(second bindings)]
       (try
         (with-open ~(vec (drop 2 bindings)) ~@body)
         (finally (__close ~(first bindings)))))))

;; jolt numbers are doubles — there is no BigDecimal math context, so the
;; precision (and optional :rounding mode) is accepted and ignored.
(defmacro with-precision [precision & exprs]
  (let [body (if (= :rounding (first exprs)) (drop 2 exprs) exprs)]
    `(do ~@body)))

(defmacro with-bindings [binding-map & body]
  `(with-bindings* ~binding-map (fn [] ~@body)))

(defmacro bound-fn [& fntail]
  `(bound-fn* (fn ~@fntail)))

(defmacro defonce [name expr]
  `(let [v# (resolve (quote ~name))]
     (if (and v# (some? (var-get v#)))
       v#
       (def ~name ~expr))))

;; Single arglist (Jolt defmacro is single-arity); the optional else defaults nil
;; via rest-destructuring.
(defmacro if-not [test then & [else]]
  `(if (not ~test) ~then ~else))

;; Conditional binding macros: the name is bound ONLY in the taken branch (the
;; auto-gensym temp# tests the value; the else/empty branch sees the surrounding
;; scope). temp# is a single template-local gensym — referenced twice, same symbol.
(defmacro if-let [bindings then & [else]]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if temp# (let [~form temp#] ~then) ~else))))

;; when-let lives in 00-syntax (not here): 20-coll uses it, which loads before this tier.

(defmacro if-some [bindings then & [else]]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (some? temp#) (let [~form temp#] ~then) ~else))))

(defmacro when-some [bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (some? temp#) (let [~form temp#] ~@body) nil))))

(defmacro while [test & body]
  `(loop [] (when ~test ~@body (recur))))

(defmacro dotimes [bindings & body]
  (let [i (bindings 0) n (bindings 1)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#) ~@body (recur (inc ~i)))))))

;; A fresh jolt symbol inside a macro body: (gensym) here resolves to Janet's
;; builtin (a Janet symbol the destructurer rejects), so round-trip through str.
(defn- fresh-sym [] (symbol (str (gensym))))

;; Lazy-safe: take only the head via first (Clojure uses (seq coll), but Jolt's
;; eager seq would realize an infinite coll like (repeat nil) and hang). Matches
;; the prior Janet behavior; the nil/false-head distinction waits on Phase 5
;; laziness.
(defmacro when-first [bindings & body]
  (let [x (bindings 0) coll (bindings 1)]
    `(when-let [~x (first ~coll)] ~@body)))

;; doto threads a single fresh-bound value as the first arg of each form (side
;; effects), returning the value. A shared explicit gensym is needed because the
;; forms are built outside the let's template.
(defmacro doto [x & forms]
  (let [g (fresh-sym)
        steps (map (fn [f] (if (seq? f) (apply list (first f) g (rest f)) (list f g))) forms)]
    `(let [~g ~x] ~@steps ~g)))

;; Threading-with-rebinding macros. The binding pairs are spliced into a TEMPLATE
;; vector (so core-let sees a tuple form, not a runtime pvec value).
(defn- thread-binds [g steps]
  (reduce (fn [acc s] (conj (conj acc g) s)) [] (butlast steps)))

(defmacro as-> [expr name & forms]
  (let [pairs (reduce (fn [acc f] (conj (conj acc name) f)) [] (butlast forms))]
    `(let [~name ~expr ~@pairs] ~(if (empty? forms) name (last forms)))))

(defmacro some-> [expr & forms]
  (let [g (fresh-sym)
        steps (map (fn [f] `(if (nil? ~g) nil (-> ~g ~f))) forms)]
    `(let [~g ~expr ~@(thread-binds g steps)] ~(if (empty? steps) g (last steps)))))

(defmacro some->> [expr & forms]
  (let [g (fresh-sym)
        steps (map (fn [f] `(if (nil? ~g) nil (->> ~g ~f))) forms)]
    `(let [~g ~expr ~@(thread-binds g steps)] ~(if (empty? steps) g (last steps)))))

(defmacro cond->> [expr & clauses]
  (let [g (fresh-sym)
        steps (map (fn [pair] `(if ~(first pair) (->> ~g ~(second pair)) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr ~@(thread-binds g steps)] ~(if (empty? steps) g (last steps)))))

(defmacro assert [x & [message]]
  (let [msg (if message message (str "Assert failed: " (pr-str x)))]
    `(when-not ~x (throw (ex-info ~msg {})))))

;; (pvalues e1 e2 ...) — each expression evaluated in parallel (pcalls).
(defmacro pvalues [& exprs]
  `(pcalls ~@(map (fn [e] `(fn [] ~e)) exprs)))

(defmacro delay [& body]
  `(make-delay (fn [] ~@body)))

(defmacro future [& body]
  `(future-call (fn [] ~@body)))

;; Build the fn* form via a template (a reader-list array): cons/list in a macro
;; body produce a plist the evaluator can't call as a form.
(defmacro letfn [fnspecs & body]
  (let [binds (reduce (fn [acc spec] (conj (conj acc (first spec)) `(fn* ~@(rest spec))))
                      [] fnspecs)]
    `(let* [~@binds] ~@body)))

;; Dynamic binding: install a thread-binding frame of var->value (array-map keeps
;; var-get happy, unlike a phm), restore on exit.
(defmacro binding [bindings & body]
  (let [pairs (reduce (fn [acc p] (conj (conj acc `(var ~(first p))) (second p)))
                      [] (partition 2 bindings))]
    `(let* [frame# (array-map ~@pairs)]
       (push-thread-bindings frame#)
       (try (do ~@body) (finally (pop-thread-bindings))))))

;; condp: clauses are test-expr result-expr, or test-expr :>> result-fn (calls
;; result-fn on the truthy (pred test-expr value)); a lone trailing expr is the
;; default. The recursive emit builds a nested if chain.
(defmacro condp [pred expr & clauses]
  (let [gp (fresh-sym) ge (fresh-sym)
        emit (fn emit [args]
               (let [n (if (= :>> (second args)) 3 2)
                     clause (take n args)
                     more (drop n args)
                     cn (count clause)]
                 (cond
                   (= 0 cn) `(throw (ex-info (str "No matching clause: " ~ge) {}))
                   (= 1 cn) (first clause)
                   (= 2 cn) `(if (~gp ~(first clause) ~ge) ~(second clause) ~(emit more))
                   :else `(if-let [p# (~gp ~(first clause) ~ge)]
                            (~(nth clause 2) p#)
                            ~(emit more)))))]
    `(let [~gp ~pred ~ge ~expr] ~(emit clauses))))

;; --- protocols, records, types ---------------------------------------------
;; These emit Jolt's protocol/type special forms (protocol-dispatch,
;; register-method, make-reified, deftype).

;; Group a flat seq that starts with a head symbol followed by its list specs
;; into [[head spec spec ...] ...] runs. Used by extend-protocol and defrecord.
(defn- group-by-head [items]
  ;; nil is a valid extension head (extend-protocol P ... nil (m [x] ...)).
  (reduce (fn [acc x]
            (if (or (symbol? x) (nil? x))
              (conj acc [x])
              (conj (pop acc) (conj (peek acc) x))))
          [] items))

;; deftype is sugar over make-deftype-ctor (a ctx-capturing clojure.core fn that
;; bakes the ns-qualified type tag at def time) plus extend-type for any inline
;; protocol methods — so it compiles as a plain (do …). Each method body sees the
;; type's fields, bound from the instance (the method's first param), matching
;; Clojure's deftype scope. defrecord (below) expands to a bodyless (deftype …) and
;; handles its own methods, so this also serves the no-body case.
(defmacro deftype [tname fields & body]
  ;; strip ^meta off the type name and fields (the reader yields a (with-meta sym m)
  ;; form for e.g. (deftype ^{:doc …} Foo …)), so (name …) sees a bare symbol.
  (let [unwrap (fn [x] (if (and (seq? x) (symbol? (first x)) (= "with-meta" (name (first x))))
                         (second x) x))
        tname (unwrap tname)
        fields (map unwrap fields)
        arrow (symbol (str "->" (name tname)))
        ;; a seq of field keywords; spliced into a vector LITERAL below ([~@…]) so
        ;; the analyzer sees a vector form, not a runtime pvec value.
        field-kws (map (fn [f] (keyword (name f))) fields)
        ;; per-field TYPE HINT (jolt-3ko): ^Vec3 origin -> "Vec3" (a record type
        ;; name), ^:num x -> "num", else nil. Lets the inference know a field's
        ;; exact type up front, so reading it back carries that type (not :any) —
        ;; the key to fast nested-record code. Spliced as a vector literal too.
        field-tags (map (fn [f] (let [mt (meta f)]
                                  (cond (and mt (:tag mt)) (:tag mt)
                                        (and mt (:num mt)) "num"
                                        :else nil)))
                        fields)
        ;; per-field MUTABILITY (jolt-c3q): ^:unsynchronized-mutable / ^:volatile-
        ;; mutable marks a field set!-able. A type with any mutable field opts out
        ;; of the immutable shape-rec layout and uses the mutable table form, so
        ;; set! can mutate it (the ctor reads this vector). Spliced as a literal.
        field-muts (map (fn [f] (let [mt (meta f)]
                                  (if (and mt (or (:unsynchronized-mutable mt)
                                                  (:volatile-mutable mt)))
                                    true false)))
                        fields)
        impl (fn [proto specs]
               `(extend-type ~tname ~proto
                  ~@(map (fn [spec]
                           (let [argv (nth spec 1)
                                 inst (first argv)
                                 binds (vec (mapcat (fn [f] [f `(get ~inst ~(keyword (name f)))]) fields))]
                             `(~(first spec) ~argv (let [~@binds] ~@(drop 2 spec)))))
                         specs)))]
    `(do
       (def ~tname (make-deftype-ctor (quote ~tname) [~@field-kws] [~@field-tags] [~@field-muts]))
       (def ~arrow ~tname)
       ~@(map (fn [g] (impl (first g) (rest g))) (group-by-head body))
       ~tname)))

;; The protocol value is built by make-protocol (a fn call) rather than an embedded
;; tagged map literal: the interpreter would otherwise self-evaluate such a struct
;; instead of evaluating its fields. methods is a {kw {:name str}} map (only :name
;; is consulted). Each method is a thin dispatch fn over protocol-dispatch.
(defmacro defprotocol [pname & sigs]
  ;; Clojure's defprotocol takes an optional docstring and leading keyword
  ;; options (:extend-via-metadata true, honeysql uses it) before the method
  ;; signatures — drop them (metadata extension is a JVM dispatch detail).
  (let [sigs (loop [s sigs]
               (cond
                 (string? (first s))  (recur (rest s))
                 (keyword? (first s)) (recur (rest (rest s)))
                 :else s))
        methods (reduce (fn [m sig]
                          (assoc m (keyword (name (first sig))) {:name (name (first sig))}))
                        {} sigs)]
    `(do
       (def ~pname (make-protocol ~(name pname) ~methods))
       ;; register method var-keys for devirtualization (jolt-41m); the inference
       ;; reads this (via infer-unit!) to resolve a protocol call on a known record
       (register-protocol-methods! ~(name pname) [~@(map (fn [s] (name (first s))) sigs)])
       ~@(map (fn [sig]
                `(def ~(first sig)
                   ;; protocol-dispatch is a fn (clojure.core); pass the protocol /
                   ;; method NAMES as strings (not the symbols) so it compiles as a
                   ;; plain invoke rather than evaluating the symbols as vars.
                   (fn* [this# & rest#]
                     (protocol-dispatch ~(name pname) ~(name (first sig)) this# rest#))))
              sigs))))

;; Member threading: (.. x f g) => (. (. x f) g); a parenthesized member
;; carries args. Canonical Clojure shape, single-arity defmacro.
(defmacro .. [x form & more]
  (let [step (if (seq? form)
               `(. ~x ~(first form) ~@(rest form))
               `(. ~x ~form))]
    (if (seq more)
      `(.. ~step ~@more)
      step)))

;; True when atype's methods were registered for this protocol (via extend /
;; extend-type). Tags are canonical host names or ns-qualified record names,
;; so a bare record name also matches its "ns.Name" tag.
(defn extends? [protocol atype]
  (let [want (name atype)
        dotted (str "." want)
        dlen (count dotted)]
    (boolean (some (fn [t]
                     (let [tn (name t)]
                       (or (= tn want)
                           (and (> (count tn) dlen)
                                (= (subs tn (- (count tn) dlen)) dotted)))))
                   (extenders protocol)))))

;; extend, the FUNCTION (extend-type's runtime sibling): protocol + method-map
;; pairs, methods registered under the type's (canonicalized) name — so
;; (extend 'String P {:m (fn [x] ...)}) dispatches exactly like extend-type.
(defn extend [atype & proto+mmaps]
  (loop [s (seq proto+mmaps)]
    (when s
      (let [proto (first s)
            mmap (second s)
            pname (name (get proto :name))]
        (doseq [[k f] mmap]
          (register-method (name atype) pname (name k) f)))
      (recur (nnext s)))))

(defmacro extend-type [tsym & body]
  ;; register-method is a fn (clojure.core); pass type/protocol/method NAMES as
  ;; strings (not the symbols) so the call compiles as a plain invoke. A nil
  ;; type extends on nil values (the host tag is the string "nil").
  ;; `body` is one or more protocols, each followed by its method specs:
  ;; (extend-type T P1 (m1 [_] ..) P2 (m2 [_] ..)) — a bare symbol switches the
  ;; current protocol (like reify), so multiple protocols extend in one form.
  (let [tname (if (nil? tsym) "nil" (name tsym))]
    (loop [items (seq body) proto nil forms []]
      (if (empty? items)
        `(do ~@forms)
        (let [x (first items)]
          (if (symbol? x)
            (recur (rest items) (name x) forms)
            (recur (rest items) proto
                   (conj forms
                         `(register-method ~tname ~proto ~(name (first x))
                                           (fn ~(nth x 1) ~@(drop 2 x)))))))))))

(defmacro extend-protocol [psym & type-impls]
  `(do ~@(map (fn [g] `(extend-type ~(first g) ~psym ~@(rest g)))
              (group-by-head type-impls))))

;; extend (the fn form) is not supported — stub to nil, as before.
;; extend is a real FUNCTION now — defined above extend-type.
;; JVM proxies are unsupported.
(defmacro proxy [& args] nil)
;; definterface is JVM-only; bind the name to an empty marker.
(defmacro definterface [name-sym & body] `(def ~name-sym {}))

;; make-reified is a fn (clojure.core); the method map {kw (fn* ...)} is an
;; ordinary map literal that evaluates to {keyword fn}, and the protocol NAME is
;; passed as a string (not the symbol) so the call compiles as a plain invoke.
(defmacro reify [& forms]
  ;; a reify can implement SEVERAL protocols; collect them all (each bare symbol
  ;; switches the current protocol, like extend-type) and pass every protocol name
  ;; to make-reified so (instance? Proto r)/satisfies? recognise all of them.
  (loop [items (seq forms) protos [] methods {}]
    (if (empty? items)
      `(make-reified ~methods ~@(vec (map name protos)))
      (let [x (first items)]
        (if (symbol? x)
          (recur (rest items) (conj protos x) methods)
          (recur (rest items) protos
                 (assoc methods (keyword (name (first x)))
                        `(fn ~(nth x 1) ~@(drop 2 x)))))))))

(defmacro defrecord [name-sym fields & body]
  (let [tn (name name-sym)
        arrow (symbol (str "->" tn))
        mapf (symbol (str "map->" tn))
        m (fresh-sym)
        ;; each method body sees the record fields, bound from the instance (the
        ;; method's first param), matching Clojure's defrecord method scope. vec the
        ;; spliced binding seq so ~@ splices its elements, not the lazy-seq itself.
        impl (fn [proto specs]
               `(extend-type ~name-sym ~proto
                  ~@(map (fn [spec]
                           (let [argv (nth spec 1)
                                 inst (first argv)
                                 ;; hint `this` with the record type so the inference
                                 ;; types it (jolt-3ko) and its field reads bare-index
                                 ;; instead of going through the runtime tag guard.
                                 hinted (assoc argv 0 (vary-meta inst assoc :tag (name name-sym)))
                                 binds (vec (mapcat (fn [f] [f `(get ~inst ~(keyword (name f)))]) fields))]
                             `(~(first spec) ~hinted (let [~@binds] ~@(drop 2 spec)))))
                         specs)))]
    `(do
       ;; deftype already defines ->name (= the ctor); no (name. …) interop needed,
       ;; so defrecord compiles too. map->name builds via that ctor.
       (deftype ~name-sym ~fields)
       (def ~mapf (fn* [~m] (~arrow ~@(map (fn [f] `(get ~m ~(keyword (name f)))) fields))))
       ~@(map (fn [g] (impl (first g) (rest g))) (group-by-head body)))))

;; --- laziness --------------------------------------------------------------
;; lazy-seq / lazy-cat moved to the 00-syntax tier: the seq/coll tiers (10-seq,
;; 20-coll) use lazy-seq, and in compile mode a tier's forms are compiled as it
;; loads — so the macro must be registered BEFORE those tiers, else (lazy-seq …)
;; compiles as a call to the macro-as-function and leaks its expansion at runtime
;; (jolt-r81). They only need seed fns (make-lazy-seq/coll->cells/concat).

;; memfn: a fn wrapping a method call, (memfn toUpperCase) => #(.toUpperCase %).
;; The method symbol is rewritten to jolt's .method call sugar; extra arg names
;; become fn params, as in Clojure.
(defmacro memfn [method-name & args]
  `(fn [target# ~@args]
     (~(symbol (str "." (name method-name))) target# ~@args)))
