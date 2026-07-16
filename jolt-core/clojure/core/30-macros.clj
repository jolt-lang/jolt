;; clojure.core — macro tier. Macros expressed in Clojure (defmacro + syntax-quote).
;; Loaded after the fn tiers, so a macro here may use any already-frozen core
;; fn/macro.
;;
;; IMPORTANT — only macros NOT used by the self-hosted compiler (jolt-core/jolt/*)
;; or by the earlier overlay tiers belong here; those (and/or/when/when-not/
;; when-let/cond/case/doseq/declare/cond->/->) must stay available before this
;; tier loads, so they remain host primitives for now. Everything here is user-facing.
;;
;; Migration: remove the host core-X macro fn AND its core-macro-names entry when
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
        opts (rest args)
        ;; qualify with the EXPANSION ns: a defmulti deferred inside a fn (a
        ;; deftest body) must still define in the ns it was written in.
        qname (symbol (str (clojure.core/ns-name clojure.core/*ns*))
                      (clojure.core/name name))]
    `(defmulti-setup (quote ~qname) ~dispatch ~@opts)))

(defmacro defmethod [mm dispatch-val & fn-tail]
  ;; the expansion ns rides along so a deferred defmethod resolves its multifn
  ;; against the ns it was written in (aliases and refers included).
  `(defmethod-setup (quote ~mm) ~dispatch-val (fn ~@fn-tail)
                    ~(str (clojure.core/ns-name clojure.core/*ns*))))

;; Multimethod table ops: a multimethod's method table lives on its
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
;; compiled fn in any namespace.
(defmacro get-method [mm dval]
  `(get-method-setup ~mm ~dval))

(defmacro methods [mm]
  `(methods-setup ~mm))

;; prefers reads the store off the VAR (the multifn value can't carry it) —
;; same symbol-passing shape as the other multimethod table ops.
(defmacro prefers [mm]
  `(prefers-setup (quote ~mm)))

;; instance?: class names don't evaluate to values on jolt, so bare class-name
;; symbols are passed quoted to the ctx-capturing checker. A LIST in type
;; position is a class-valued expression (e.g. Selmer's (Class/forName "[C"))
;; — evaluate it. A LOCAL in &env may hold a class value (string or jhost)
;; from a (let [c java.util.Map] (instance? c x)) binding — evaluate it too.
;; A symbol resolving to a var that HOLDS A CLASS VALUE (a name string — jolt's
;; class model) also evaluates: on the JVM ns mappings win over class
;; resolution, so (def mc java.util.Map) (instance? mc x) reads the var. The
;; string check keeps (instance? RecordName x) quoting — a defrecord interns a
;; var of that name holding its ctor fn, and the record NAME is the class.
;; resolve/var-get run at expansion time only and never appear in emitted code.
(defmacro instance? [t x]
  (if (or (seq? t) (contains? &env t)
          (and (symbol? t)
               (when-let [v (clojure.core/resolve t)]
                 (and (clojure.core/bound? v)
                      (let [cv (clojure.core/var-get v)]
                        ;; a Class value ((class y) captured in a var, e.g.
                        ;; (def c (class (transient [])))). Class tokens now
                        ;; evaluate to interned Class objects.
                        (jolt.host/class-object? cv))))))
    `(instance-check ~t ~x)
    `(instance-check (quote ~t) ~x)))

;; Take x's monitor for the duration of body (futures/agents/threads share one
;; heap, so this is a real per-object lock), releasing on any exit.
(defmacro locking [x & body]
  `(jolt.host/with-monitor ~x (fn* [] ~@body)))

;; STM macros over the host transaction seams (refs.ss). sync keeps the
;; reference's (sync flags & body) shape — flags are ignored, like the JVM.
(defmacro sync [flags & body]
  `(clojure.core/__sync-call (fn* [] ~@body)))

;; dosync: run body in a serialized transaction (single global mutex).
(defmacro dosync [& body]
  `(clojure.core/__sync-call (fn* [] ~@body)))

;; io!: inside a transaction, throws WITHOUT evaluating body; an optional
;; leading literal string is the exception message.
(defmacro io! [& body]
  (let [message (when (string? (first body)) (first body))
        body (if message (rest body) body)]
    `(if (clojure.core/__txn-running?)
       (throw (new IllegalStateException ~(or message "I/O in transaction")))
       (do ~@body))))

;; defonce: define name only if it isn't already bound to a non-nil root;
;; returns the existing var untouched otherwise.
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

;; Binds *math-context*; BigDecimal arithmetic in the dynamic scope rounds its
;; results to the precision with the rounding mode (default HALF_UP, like
;; java.math.MathContext).
(defmacro with-precision [precision & exprs]
  (let [[rounding body] (if (= :rounding (first exprs))
                          [(second exprs) (drop 2 exprs)]
                          ['HALF_UP exprs])]
    `(binding [clojure.core/*math-context* {:precision ~precision :rounding '~rounding}]
       ~@body)))

(defmacro with-bindings [binding-map & body]
  `(with-bindings* ~binding-map (fn [] ~@body)))

(defmacro bound-fn [& fntail]
  `(bound-fn* (fn ~@fntail)))

(defmacro defonce [name expr]
  ;; Must NOT reference clojure.core/resolve (a tree-shake bail ref).
  ;; Use jolt.host/find-var — a bare var-cell lookup with no alias resolution.
  ;; The ns/name strings are computed at expansion time.
  (let [ns-str (str (clojure.core/ns-name clojure.core/*ns*))
        n-str (clojure.core/name name)]
    `(if-let [v# (jolt.host/find-var ~ns-str ~n-str)]
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

;; fresh-sym (a macro-body gensym round-tripped through str) is defined in
;; 00-syntax, which loads before this tier — reuse it.

;; Lazy-safe: take only the head via first (Clojure uses (seq coll), but Jolt's
;; eager seq would realize an infinite coll like (repeat nil) and hang).
(defmacro when-first [bindings & body]
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
         ~@body))))

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
  ;; the message EXPRESSION evaluates at failure time (JVM: (str "Assert failed: "
  ;; message "\n" form)), not at expansion — it may embed runtime state
  (if message
    `(when-not ~x
       (throw (new AssertionError (str "Assert failed: " ~message "\n" ~(pr-str x)))))
    `(when-not ~x (throw (new AssertionError ~(str "Assert failed: " (pr-str x)))))))

;; (pvalues e1 e2 ...) — each expression evaluated in parallel (pcalls).
(defmacro pvalues [& exprs]
  `(pcalls ~@(map (fn [e] `(fn [] ~e)) exprs)))

(defmacro delay [& body]
  `(make-delay (fn [] ~@body)))

(defmacro future [& body]
  `(future-call (fn [] ~@body)))

;; Build the fn* form via a template (a reader-list array): cons/list in a macro
;; body produce a plist the evaluator can't call as a form.
;; letfn is a primitive special form (analyze-letfn -> letrec*), not a macro: its
;; fns are mutually recursive, which a (let* …) expansion cannot express. Defining
;; it as a macro would shadow the special once macroexpansion runs first (the
;; canonical order), so it is intentionally NOT a macro here.

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
;; Group deftype/defrecord/reify body forms: a symbol/nil head starts a new
;; group, every other form appends to the current one. (extend-protocol uses
;; parse-extend-impls instead — it must treat a COMPUTED class type like
;; (Class/forName "[B"), a seq, as a head, which this would misread as a method.)
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
;; Legacy structmap definer: binds a var to the struct basis (see create-struct).
(defmacro defstruct [name & keys]
  `(def ~name (create-struct ~@keys)))

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
        ;; per-field TYPE HINT: ^Vec3 origin -> "Vec3" (a record type
        ;; name), ^:num x -> "num", else nil. Lets the inference know a field's
        ;; exact type up front, so reading it back carries that type (not :any) —
        ;; the key to fast nested-record code. Spliced as a vector literal too.
        field-tags (map (fn [f] (let [mt (meta f)]
                                  (cond (and mt (:tag mt)) (name (:tag mt))   ; symbol or string -> string
                                        (and mt (:num mt)) "num"
                                        :else nil)))
                        fields)
        ;; per-field MUTABILITY: ^:unsynchronized-mutable / ^:volatile-
        ;; mutable marks a field set!-able. A type with any mutable field opts out
        ;; of the immutable shape-rec layout and uses the mutable table form, so
        ;; set! can mutate it (the ctor reads this vector). Spliced as a literal.
        field-muts (map (fn [f] (let [mt (meta f)]
                                  (if (and mt (or (:unsynchronized-mutable mt)
                                                  (:volatile-mutable mt)))
                                    true false)))
                        fields)
        ;; mutable field symbols (^:unsynchronized-mutable / ^:volatile-mutable):
        ;; (set! field v) in a method body lowers to (set! (.-field inst) v), the
        ;; in-place field write the analyzer compiles to jolt-set-field!.
        mutable-syms (map first (filter second (map vector fields field-muts)))
        mutable? (fn [s] (boolean (some (fn [m] (= m s)) mutable-syms)))
        ;; rewrite a method body: (set! mut-field v) -> an in-place (.-field inst)
        ;; write, and a READ of a mutable field -> (.-field inst) so it observes the
        ;; live value after a set! (the double-checked-locking idiom re-reads a field
        ;; after taking a lock). Immutable fields stay let-bound (captured once is
        ;; correct and cheaper). Tracks lexical shadowing through let/loop/fn/letfn so
        ;; a same-named local wins over a field.
        rewrite-body
        (fn rw [inst shadowed form]
          (cond
            (and (seq? form) (seq form) (symbol? (first form))
                 (= "set!" (name (first form)))
                 (symbol? (second form)) (mutable? (second form))
                 (not (contains? shadowed (second form))))
            (list 'set! (list (symbol (str ".-" (name (second form)))) inst)
                  (rw inst shadowed (nth form 2)))
            ;; let/loop-style vector-binding forms: rewrite inits, then shadow the
            ;; bound names in the body.
            (and (seq? form) (seq form) (symbol? (first form))
                 (contains? #{"let" "let*" "loop" "binding" "when-let" "if-let"
                              "when-some" "if-some"} (name (first form)))
                 (vector? (second form)))
            (let [bv (second form) n (count bv)
                  bv' (loop [i 0 acc []]
                        (if (< i n)
                          (recur (+ i 2)
                                 (let [a (conj acc (nth bv i))]
                                   (if (< (inc i) n) (conj a (rw inst shadowed (nth bv (inc i)))) a)))
                          acc))
                  sh (loop [i 0 acc shadowed]
                       (if (< i n)
                         (recur (+ i 2) (if (symbol? (nth bv i)) (conj acc (nth bv i)) acc))
                         acc))]
              (cons (first form) (cons bv' (map (fn [x] (rw inst sh x)) (drop 2 form)))))
            ;; fn/fn*: shadow each arity's params in its body.
            (and (seq? form) (seq form) (symbol? (first form))
                 (contains? #{"fn" "fn*"} (name (first form))))
            (let [head (first form) tail (rest form)
                  named? (and (seq tail) (symbol? (first tail)))
                  fname (when named? (first tail))
                  arts (if named? (rest tail) tail)
                  psyms (fn [pv] (loop [p (seq pv) acc shadowed]
                                   (if p
                                     (recur (next p)
                                            (if (and (symbol? (first p)) (not= (name (first p)) "&"))
                                              (conj acc (first p)) acc))
                                     acc)))
                  do-art (fn [ar] (cons (first ar) (map (fn [x] (rw inst (psyms (first ar)) x)) (rest ar))))
                  arts' (if (vector? (first arts)) (do-art arts) (map do-art arts))]
              (concat (list head) (when named? (list fname)) arts'))
            ;; a bare read of a mutable field -> live field access
            (and (symbol? form) (mutable? form) (not (contains? shadowed form)))
            (list (symbol (str ".-" (name form))) inst)
            (seq? form) (map (fn [x] (rw inst shadowed x)) form)
            (vector? form) (mapv (fn [x] (rw inst shadowed x)) form)
            :else form))
        ;; inline impls register for dispatch but are NOT extenders of the
        ;; protocol (the JVM compiles them into the class) — register-inline-method,
        ;; not extend-type.
        ;; build one method clause (argv + field-bound body) from a method spec.
        ;; The clause is DATA, not a syntax-quote: a body that is itself a syntax-
        ;; quote would have its ~unquotes consumed a level early if re-spliced.
        mk-clause (fn [spec]
                    ;; fresh-name each _ param so two _ params don't collide on the
                    ;; field binds / live-read instance (see defrecord's mk-clause).
                    (let [argv (mapv (fn [p] (if (= p (quote _)) (gensym "_p") p)) (nth spec 1))
                          inst (first argv)
                          ;; A method param shadows a same-named field (Clojure
                          ;; semantics): don't let-bind a field the param already
                          ;; provides, and treat those params as shadowing so a
                          ;; mutable field's live-read rewrite doesn't override them.
                          pnames (set (map name argv))
                          ;; let-bind only immutable fields; mutable ones are read live
                          ;; via rewrite-body so a set! within the method is observed.
                          binds (vec (mapcat (fn [f] [f `(get ~inst ~(keyword (name f)))])
                                             (filter (fn [f] (and (not (mutable? f))
                                                                  (not (contains? pnames (name f)))))
                                                     fields)))
                          mbody (map (fn [bf] (rewrite-body inst (set argv) bf)) (drop 2 spec))]
                      (list argv (list* 'let binds mbody))))
        groups (group-by-head body)
        ;; merge clauses by method NAME across ALL protocols into one multi-arity
        ;; fn, so a name appearing in two interfaces with different arities
        ;; (data.priority-map's seq is in Seqable [this] AND Sorted [this asc])
        ;; dispatches by arg count instead of one registration shadowing the other.
        ;; (Within one protocol, distinct arities like Indexed's nth merge the same
        ;; way.) Each (protocol, name) registers the merged fn, so dispatch by name
        ;; and satisfies? by protocol both hold.
        by-name (reduce (fn [m spec]
                          (let [nm (name (first spec))]
                            (assoc m nm (conj (get m nm []) (mk-clause spec)))))
                        {} (mapcat rest groups))]
    `(do
       (def ~tname (make-deftype-ctor (quote ~tname) [~@field-kws] [~@field-tags] [~@field-muts]))
       (def ~arrow ~tname)
       ~@(mapcat (fn [g]
                   (let [proto (first g)
                         names (distinct (map (fn [spec] (name (first spec))) (rest g)))]
                     (cons `(register-inline-protocol! ~(name tname) ~(name proto))
                           (map (fn [nm]
                                  `(register-inline-method ~(name tname) ~(name proto) ~nm
                                                           (fn ~@(get by-name nm))))
                                names))))
                 groups)
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
       ;; register method var-keys for devirtualization; the inference
       ;; reads this (via infer-unit!) to resolve a protocol call on a known record
       (register-protocol-methods! ~(name pname) [~@(map (fn [s] (name (first s))) sigs)])
       ;; one fn clause per declared arity. The protocol/method NAMES pass as
       ;; strings so the body compiles as a plain invoke (not symbol-as-var). The
       ;; common 1/2/3-param arities call positional protocol-dispatchN, which
       ;; applies the impl directly — no rest-list cons; 4+ params fall back to the
       ;; variadic protocol-dispatch with a vector of the extra args.
       ~@(map (fn [sig]
                (let [pn (name pname)
                      mn (name (first sig))
                      arglists (filter vector? (rest sig))
                      clause (fn [argv]
                               (let [ps (mapv (fn [_] (fresh-sym)) argv)
                                     n (count ps)
                                     obj (first ps)]
                                 (cond
                                   (= n 1) (list ps (list 'protocol-dispatch1 pn mn obj))
                                   (= n 2) (list ps (list 'protocol-dispatch2 pn mn obj (nth ps 1)))
                                   (= n 3) (list ps (list 'protocol-dispatch3 pn mn obj (nth ps 1) (nth ps 2)))
                                   :else   (list ps (list 'protocol-dispatch pn mn obj (vec (rest ps)))))))]
                  (if (seq arglists)
                    `(def ~(first sig) (fn* ~@(map clause arglists)))
                    `(def ~(first sig)
                       (fn* [this# & rest#] (protocol-dispatch ~pn ~mn this# rest#))))))
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
;; extend-type). Tags are canonical host names or ns-qualified record names, so a
;; name matches its tag when either is a dotted suffix of the other — a bare
;; record name matches its "ns.Name" tag, and a query for a qualified host class
;; (java.util.Map) matches the canonical short tag (Map) extend registered it as.
(defn extends? [protocol atype]
  (let [want (if (nil? atype) "nil" (if (jolt.host/class-object? atype) (.getName atype) (name atype)))
        suffix? (fn [long short]
                  (let [d (str "." short)]
                    (and (> (count long) (count d))
                         (= (subs long (- (count long) (count d))) d))))
        pn-str (some-> protocol :name name)]
    (boolean (or (some (fn [t]
                         (let [tn (name t)]
                           (or (= tn want) (suffix? tn want) (suffix? want tn))))
                       (extenders protocol))
                 (and pn-str (jolt.host/type-satisfies? want pn-str))))))

;; The canonical name for a protocol-extension type: a symbol/keyword via name, a
;; string as-is, nil as "nil" (extends on nil values), and a Class VALUE — e.g.
;; (Class/forName "[B") for the byte-array class — via .getName. Lets a library
;; extend a protocol to a class it computes rather than names with a symbol.
(defn type->name [t]
  (cond (nil? t) "nil"
        (string? t) t
        (symbol? t) (name t)
        (keyword? t) (name t)
        :else (.getName t)))

;; extend, the FUNCTION (extend-type's runtime sibling): protocol + method-map
;; pairs, methods registered under the type's (canonicalized) name — so
;; (extend 'String P {:m (fn [x] ...)}) dispatches exactly like extend-type.
(defn extend [atype & proto+mmaps]
  ;; nil extends on nil values; its host tag is the string "nil" (as extend-type).
  (let [tname (type->name atype)]
    (loop [s (seq proto+mmaps)]
      (when s
        (let [proto (first s)
              mmap (second s)
              pname (name (get proto :name))]
          (doseq [[k f] mmap]
            (register-method tname pname (name k) f)))
        (recur (nnext s))))))

(defmacro extend-type [tsym & body]
  ;; register-method is a fn (clojure.core); pass type/protocol/method NAMES as
  ;; strings (not the symbols) so the call compiles as a plain invoke. A nil
  ;; type extends on nil values (the host tag is the string "nil").
  ;; `body` is one or more protocols, each followed by its method specs:
  ;; (extend-type T P1 (m1 [_] ..) P2 (m2 [_] ..)) — a bare symbol switches the
  ;; current protocol (like reify), so multiple protocols extend in one form.
  ;; tsym may be a symbol/nil (name resolved at compile time) or a computed class
  ;; expression like (Class/forName "[B") — bind its runtime name once.
  (let [literal? (or (nil? tsym) (symbol? tsym))
        tn (gensym "tname")
        tref (if literal? (if (nil? tsym) "nil" (name tsym)) tn)
        emit (fn []
               (loop [items (seq body) proto nil forms []]
                 (if (empty? items)
                   forms
                   (let [x (first items)]
                     (if (symbol? x)
                       (recur (rest items) (name x) forms)
                       (recur (rest items) proto
                              (conj forms
                                    `(register-method ~tref ~proto ~(name (first x))
                                                      (fn ~(nth x 1) ~@(drop 2 x))))))))))]
    (if literal?
      `(do ~@(emit))
      `(let [~tn (type->name ~tsym)] ~@(emit) nil))))

;; Group an extend-protocol body into [type method-spec*] groups: the type is the
;; first item and its method specs are the seqs that follow it (up to the next
;; type — a symbol/nil — or end). Handles a computed class type (a seq like
;; (Class/forName "[B")) positionally, matching Clojure's parse-impls.
(defn- parse-extend-impls [items]
  (loop [s (seq items) groups []]
    (if (empty? s)
      groups
      (let [after (rest s)]
        (recur (drop-while seq? after)
               (conj groups (vec (cons (first s) (take-while seq? after)))))))))

(defmacro extend-protocol [psym & type-impls]
  `(do ~@(map (fn [g] `(extend-type ~(first g) ~psym ~@(rest g)))
              (parse-extend-impls type-impls))))

;; extend is a real FUNCTION — defined above extend-type.
;; JVM proxies are unsupported in general, EXCEPT (proxy [ThreadLocal] [] (initialValue
;; [] body)) — a per-thread store with a lazy initial value (test.check's no-seed
;; PRNG uses one). Other proxies stay nil.
(defmacro proxy [supers ctor-args & methods]
  (if (and (vector? supers) (= 1 (count supers))
           (let [s (name (first supers))] (or (= s "ThreadLocal") (= s "InheritableThreadLocal"))))
    (let [init (some (fn [m] (when (= "initialValue" (name (first m))) m)) methods)]
      `(jolt.host/make-thread-local (fn [] ~@(when init (nnext init)))))
    ;; jolt only implements (proxy [ThreadLocal] …). Emit a runtime throw (not a
    ;; compile-time one) so a file with an unused proxy still loads, but actually
    ;; evaluating an unsupported proxy fails loudly instead of yielding nil.
    `(throw (ex-info
             (str "proxy is unsupported for supers " '~supers
                  " — jolt implements only (proxy [ThreadLocal] …); use reify/deftype")
             {:supers '~supers}))))
;; definterface is JVM-only; bind the name to a marker and return the name (not a
;; var), matching the JVM where definterface yields the interface Class.
(defmacro definterface [name-sym & body]
  `(do (def ~name-sym {}) (quote ~name-sym)))

;; make-reified is a fn (clojure.core); the method map {kw (fn* ...)} is an
;; ordinary map literal that evaluates to {keyword fn}, and the protocol NAME is
;; passed as a string (not the symbol) so the call compiles as a plain invoke.
(defmacro reify [& forms]
  ;; a reify can implement SEVERAL protocols; collect them all (each bare symbol
  ;; switches the current protocol, like extend-type) and pass every protocol name
  ;; to make-reified so (instance? Proto r)/satisfies? recognise all of them.
  ;; Several bodies for the same method name are distinct arities (clojure.spec
  ;; reifies (specize* [s]) and (specize* [s _])): group them into one multi-arity
  ;; fn so dispatch picks the clause by arg count.
  (loop [items (seq forms) protos [] methods {} order []]
    (if (empty? items)
      `(make-reified
         ~(reduce (fn [m k] (assoc m k `(fn ~@(get methods k)))) {} order)
         ~@(vec (map name protos)))
      (let [x (first items)]
        (if (symbol? x)
          (recur (rest items) (conj protos x) methods order)
          (let [k (keyword (name (first x)))
                clause `(~(nth x 1) ~@(drop 2 x))]
            (recur (rest items) protos
                   (assoc methods k (conj (get methods k []) clause))
                   (if (contains? methods k) order (conj order k)))))))))

(defmacro defrecord [name-sym fields & body]
  (let [tn (name name-sym)
        arrow (symbol (str "->" tn))
        mapf (symbol (str "map->" tn))
        m (fresh-sym)
        ;; each method body sees the record fields, bound from the instance (the
        ;; method's first param), matching Clojure's defrecord method scope. vec the
        ;; spliced binding seq so ~@ splices its elements, not the lazy-seq itself.
        ;; inline impls register for dispatch but are NOT extenders of the
        ;; protocol (the JVM compiles them into the class) — register-inline-method,
        ;; not extend-type.
        ;; one clause from a spec; `this` is hinted with the record type so the
        ;; inference reads its fields bare-index. Clause as DATA (see deftype).
        mk-clause (fn [spec]
                    ;; rename each _ parameter to a fresh symbol so two _ params
                    ;; (the common (m [_ _] …) on a 1-arg protocol method) don't
                    ;; collide — the field binds read (get this :field) off the
                    ;; FIRST param, which an ignored second _ would otherwise shadow.
                    (let [argv (mapv (fn [p] (if (= p (quote _)) (gensym "_p") p)) (nth spec 1))
                          inst (first argv)
                          hinted (assoc argv 0 (vary-meta inst assoc :tag (name name-sym)))
                          ;; a method param shadows a same-named field (Clojure
                          ;; semantics), so don't rebind a field the param provides.
                          pnames (set (map name argv))
                          binds (vec (mapcat (fn [f] [f `(get ~inst ~(keyword (name f)))])
                                             (remove (fn [f] (contains? pnames (name f))) fields)))]
                      (list hinted (list* 'let binds (drop 2 spec)))))
        groups (group-by-head body)
        ;; merge clauses by name across protocols into one multi-arity fn (see
        ;; deftype's by-name).
        by-name (reduce (fn [m spec]
                          (let [nm (name (first spec))]
                            (assoc m nm (conj (get m nm []) (mk-clause spec)))))
                        {} (mapcat rest groups))]
    `(do
       ;; deftype already defines ->name (= the ctor); no (name. …) interop needed,
       ;; so defrecord compiles too. map->name builds via that ctor.
       (deftype ~name-sym ~fields)
       ;; mark the type a record (map?/record?/field-seq); a bare deftype is not.
       (register-record-type! (quote ~name-sym))
       ;; build via the positional ctor for declared fields, then carry any
       ;; remaining keys as extension fields (JVM keeps them on the record).
       (def ~mapf (fn* [~m]
                    (reduce-kv assoc
                               (~arrow ~@(map (fn [f] `(get ~m ~(keyword (name f)))) fields))
                               (dissoc ~m ~@(map (fn [f] (keyword (name f))) fields)))))
       ~@(mapcat (fn [g]
                   (let [proto (first g)
                         names (distinct (map (fn [spec] (name (first spec))) (rest g)))]
                     (cons `(register-inline-protocol! ~(name name-sym) ~(name proto))
                           (map (fn [nm]
                                  `(register-inline-method ~(name name-sym) ~(name proto) ~nm
                                                           (fn ~@(get by-name nm))))
                                names))))
                 groups))))

;; --- laziness --------------------------------------------------------------
;; lazy-seq / lazy-cat moved to the 00-syntax tier: the seq/coll tiers (10-seq,
;; 20-coll) use lazy-seq, and in compile mode a tier's forms are compiled as it
;; loads — so the macro must be registered BEFORE those tiers, else (lazy-seq …)
;; compiles as a call to the macro-as-function and leaks its expansion at runtime.
;; They only need seed fns (make-lazy-seq/coll->cells/concat).

;; memfn: a fn wrapping a method call, (memfn toUpperCase) => #(.toUpperCase %).
;; The method symbol is rewritten to jolt's .method call sugar; extra arg names
;; become fn params, as in Clojure.
(defmacro memfn [method-name & args]
  `(fn [target# ~@args]
     (~(symbol (str "." (name method-name))) target# ~@args)))

;; definline — experimental: defines a named fn whose body is the expansion
;; template applied to the arg symbols, like clojure.core (the :inline meta is
;; not stored, so calls are never expanded inline — behavior-compatible).
(defmacro definline
  [name & decl]
  (let [[pre-args [args expr]] (split-with (comp not vector?) decl)]
    `(defn ~name ~@pre-args ~args ~(apply (eval (list `fn args expr)) args))))
