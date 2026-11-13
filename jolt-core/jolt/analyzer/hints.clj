(ns jolt.analyzer.hints
  "Type hints as the analyzer reads them: the JVM primitive-boolean typing a recur
  is checked against, the ^tag readers for bindings and params (struct, string,
  keyword, StringBuilder, numeric and array kinds), functional-interface
  adaptation of a hinted let binding, fn parameter-vector parsing, and the
  arglist return hint. Reads forms and analyzed nodes; never analyzes."
  (:require [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-sym-meta
                               form-coll-meta form-list? form-vec? form-elements
                               record-type? record-ctor-key resolve-global
                               resolve-class-hint]]
            [jolt.analyzer.diagnostics :refer [analysis-error]]))

;; --- the JVM's primitive boolean loop locals --------------------------------
;; The reference compiler gives a loop local the JVM type of its init, and a local
;; whose init is a primitive boolean -- (nil? x), (= a b), (< a b), (zero? n),
;; (instance? C x), (boolean x) all inline to one -- rejects a recur argument that
;; is not also a primitive boolean: "recur arg for primitive local: x is not
;; matching primitive, had: java.lang.Boolean, needed: boolean" for a literal
;; false. (A long or double local is re-analyzed boxed instead; only these fail.)
;; jolt has no primitive locals and ran such a loop. It now refuses exactly where
;; the JVM certainly does: the local's init is one of those inlines, and the
;; argument is certainly not a primitive boolean -- a literal, or a call to a var
;; that does not inline to one, which the JVM always types as an Object. Anything
;; it cannot type the same way (a host call, a branch) is let through, so this
;; never refuses a loop the JVM compiles.
(def ^:private jvm-boolean-inlines
  {"nil?" 1 "zero?" 1 "pos?" 1 "neg?" 1 "boolean" 1
   "identical?" 2 "=" 2 "==" 2 "<" 2 "<=" 2 ">" 2 ">=" 2})
(defn jvm-boolean-prim? [node env]
  (case (:op node)
    :local (= :boolean (get (:jprims env) (:name node)))
    :invoke (let [f (:fn node)]
              (and (= :var (:op f)) (= "clojure.core" (str (:ns f)))
                   (let [nm (str (:name f)) n (count (:args node))]
                     (or (= n (get jvm-boolean-inlines nm))
                         ;; (instance? C x) with a class as written: an InstanceOfExpr
                         (and (= "instance-check" nm) (= 2 n)
                              (= :var (:op (first (:args node)))))))))
    false))
;; The JVM class of a literal, as a ConstantExpr types it; :null for nil.
(defn- const-jvm-class [v]
  (cond (nil? v) :null
        (boolean? v) "java.lang.Boolean"
        (int? v) "long"
        (double? v) "double"
        (string? v) "java.lang.String"
        (keyword? v) "clojure.lang.Keyword"
        (char? v) "java.lang.Character"
        :else nil))

;; a call to a var that does not inline to a primitive: an Object, or the
;; var's :tag
(defn- invoke-jvm-class [f]
  (when (= :var (:op f))
    (let [tag (some-> (find-var (symbol (str (:ns f)) (str (:name f)))) meta :tag)]
      (cond (nil? tag) "Object"
            ;; the reference evaluates a tag written in a metadata
            ;; map, so core's are classes: (defn not {:tag Boolean})
            (class? tag) (.getName tag)
            (and (symbol? tag) (nil? (namespace tag))
                 (contains? #{"Boolean" "String" "Long" "Object" "Number" "Integer"} (name tag)))
            (str "java.lang." (name tag))
            :else (str tag)))))

;; IfExpr: typed when both branches are and agree, or one is nil and the
;; other not primitive; reported as the then branch's class, else the else's
(defn- if-jvm-class [t e]
  (when (and t e)
    (cond (= t e) t
          (and (= t :null) (not (contains? #{"boolean" "long" "double"} e))) e
          (and (= e :null) (not (contains? #{"boolean" "long" "double"} t))) t
          ;; branches that disagree leave the if untyped: an Object
          :else "Object")))

;; The JVM class of an expression as the reference compiler types it -- "boolean"
;; for a primitive boolean, :null for a nil literal (typed, but with no class) --
;; or nil when jolt cannot type it the same way (a host call, a local it has no
;; JVM type for). Only what decides a recur into a primitive boolean local.
(defn- jvm-class [node env]
  (if (jvm-boolean-prim? node env)
    "boolean"
    (case (:op node)
      :const (const-jvm-class (:val node))
      :invoke (invoke-jvm-class (:fn node))
      :if (if-jvm-class (jvm-class (:then node) env) (jvm-class (:else node) env))
      nil)))
;; The class the reference reports for an argument that is certainly NOT a
;; primitive boolean, or nil when it is one or cannot be typed as the JVM would.
;; A nil literal is left alone: the reference's own message has no class for it.
(defn jvm-non-boolean-class [node env]
  (let [c (jvm-class node env)]
    (when (and (string? c) (not= c "boolean")) c)))

;; Type hints. The reader keeps ^hint metadata on the binding symbol.
;; Two hints resolve to the :struct fast path (a constant-keyword lookup skips
;; the :jolt/type guard and emits a bare get): ^:struct (a plain struct/record
;; map) and ^TypeName where TypeName is a defrecord/deftype (its instances are
;; tagged :jolt/deftype, not :jolt/type, so a raw get is correct). ^String
;; resolves to :str (the :host-call direct string-native path, str-target-type
;; below), and ^clojure.lang.Keyword to :kw (the keyword arm of the same path).
;; Every other hint (^long, ...) parses and is ignored, as before.
;;
;; Both new values are inert to the struct machinery: every consumer that ACTS on
;; a hint equality-checks :struct (backend_scheme.clj's field-access and 1-arg
;; lookup sites). The one consumer that does not is passes/inline.clj, which
;; tests truthiness — but only to copy a hint from an inlined call onto the local
;; it was bound to, which is propagation, not interpretation, and is what a :str
;; or :kw hint wants anyway.
(defn- str-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "String") (= s "java.lang.String"))))
(defn- kw-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "Keyword") (= s "clojure.lang.Keyword"))))
;; :sb is StringBuilder and :sbuf StringBuffer. Both take the direct-emit path,
;; but a StringBuffer is synchronized, as on the JDK, so its direct forms are the
;; locking primitives (sb-append*! / sb-str* ...) where a StringBuilder's are the
;; unsynchronized ones.
(defn- sb-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "StringBuilder") (= s "java.lang.StringBuilder"))))
(defn- sbuf-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "StringBuffer") (= s "java.lang.StringBuffer"))))
(defn hint-of [ctx sym]
  (let [m (form-sym-meta sym)]
    (cond
      (nil? m) nil
      (get m :struct) :struct
      :else (let [t (get m :tag)]
              (cond (and t (record-type? ctx t)) :struct
                    (str-tag? t) :str
                    (kw-tag? t) :kw
                    (sb-tag? t) :sb
                    (sbuf-tag? t) :sbuf
                    :else nil)))))

;; A hint the INIT proves rather than the programmer writes. (StringBuilder.) can
;; only ever produce a StringBuilder, so a local bound directly to one is proven
;; :sb with nothing in the source — which matters because nobody writes
;; ^StringBuilder on `(let [sb (StringBuilder.)] …)`, and that let is how every
;; string-building loop in Clojure is shaped (honeysql's join, data.json's reader,
;; clojure.pprint). Deliberately just the direct case: a constructor call in the
;; binding position, no flow analysis, no reassignment to worry about since a let
;; local is immutable.
(defn init-proves-hint [init]
  (when (= :host-new (get init :op))
    (cond (sb-tag? (get init :class)) :sb
          (sbuf-tag? (get init :class)) :sbuf)))
(defn add-hint [env nm h]
  (if h (assoc env :hints (assoc (:hints env) nm h)) env))

;; The resolved record ctor-key ("ns/->Name") for a ^Type param hint, or nil.
;; Unlike hint-of (which collapses any record hint to the coarse :struct guard-
;; skip marker), this carries the SPECIFIC record type — cross-namespace aware —
;; so the inference can seed the param's type and read its fields shaped/typed,
;; not just :any (the lever for a typed multi-namespace program without whole-
;; program inference).
;; A ^Record tag on a parameter -> the record's ctor key, else nil.
(defn- phint-of [ctx sym]
  (let [m (form-sym-meta sym)]
    (when m (let [t (get m :tag)] (when t (record-ctor-key ctx t))))))
;; ...and whether that hint is a PROOF: only the `this` defrecord puts on an
;; inline method is (dispatch reaches the method with nothing else). A hint the
;; program wrote is a declaration -- Clojure's (:x p) answers a map's :x whatever
;; p is hinted -- so it must not type the param on its own.
(defn- phint-proven? [sym]
  (let [m (form-sym-meta sym)] (and m (true? (get m :jolt/proven)))))

;; A ^long / ^double tag -> :long / :double, else nil. The tag is a string on the
;; data reader; tolerate a symbol from macroexpansion too.
(defn tag->nkind [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (cond (= s "double") :double (= s "long") :long :else nil)))

;; A primitive numeric hint on a BINDING symbol. Drives the fl*/fx* fast path
;; (jolt.passes.numeric).
;;
;; ^int joins ^long here, and only here. jolt has no 32-bit integer — an int and a
;; long are the same value — so a ^int binding is a fixnum promise exactly as a
;; ^long one is, and honouring it lights up ported JVM code (60-gvec.clj is written
;; entirely in ^int indices) that until now paid generic arithmetic for a hint the
;; compiler dropped. The RETURN path (tag->nkind, used by with-ret-nhint and
;; arglist-ret-nkind) deliberately does NOT take it: a ^int return would coerce
;; through jolt->fx-ret and WRAP to 64 bits, and a wrap is a value change, where a
;; parameter coercion only refuses a value that was never an int to begin with.
;;
;; Divergence: reference Clojure supports only long and double primitive
;; parameters, so ^int on a param is an inert type tag there and coerces nothing.
;; Here it range-checks (and refuses a char, per RT.longCast(Object)). Recorded in
;; test/conformance/known-divergences.edn.
(defn- tag->pnkind [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (if (= s "int") :long (tag->nkind t))))
(defn- nhint-of [ctx sym]
  (let [m (form-sym-meta sym)] (when m (tag->pnkind (get m :tag)))))

;; A primitive ARRAY hint on a param. Drives the aget/aset fast paths
;; (jolt.passes.numeric):
;;
;;   :doubles  the UNBOXED flvector-ref/-set! path — a double array's backing
;;             is a Chez flvector, so an element reads back a proven :double.
;;   :floats   the same flvector path, but an element is a Float: it reads back
;;             :float (the double an fl op takes, a jfloat anywhere else) and a
;;             store rounds to single precision.
;;   :longs :ints :bytes :objects
;;             the kind's own backing (fxvector/bytevector/vector) read direct, but
;;             no result type (it can widen past a fixnum) and no jolt-nth walk.
(defn- tag->akind [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (cond (= s "doubles") :doubles (= s "floats") :floats
          (= s "longs") :longs (= s "ints") :ints
          (= s "bytes") :bytes (= s "objects") :objects
          :else nil)))
(defn ahint-of [ctx sym]
  (let [m (form-sym-meta sym)] (when m (tag->akind (get m :tag)))))

;; A PRIMITIVE tag jolt parses and then acts on nowhere. The hint surface is
;; invisible from the source — ^double is a load-bearing contract and ^float is
;; decoration, and the two look identical — so the JOLT_CHECK lint reports these,
;; and jolt.passes.types turns each into a located warning.
;;
;; Only primitives and primitive arrays. A tag like ^Object, ^java.util.List or
;; ^clojure.lang.IFn is inert on the JVM too — it is documentation, the programmer
;; knows it, and reporting it would bury the real finding in noise. A ^Foo naming a
;; record that is not registered is excluded for a different reason: the record can
;; be defined later or in another namespace, so "unknown here" is not "wrong".
(def ^:private inert-prim-tags
  #{"float" "boolean" "char" "byte" "short"
    "chars" "booleans" "shorts"})
(defn- dead-hint-of [ctx sym]
  (let [m (form-sym-meta sym)
        t (when m (get m :tag))
        s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (when (and s (contains? inert-prim-tags s)) s)))

;; Push a numeric return hint (from ^double/^long on a defn's name) onto each arity
;; of its fn, so the back end coerces the body's value to that kind on return —
;; making the hint a contract a caller's arithmetic can trust.
(defn with-ret-nhint [node kind]
  (if (and kind (= :fn (:op node)))
    (assoc node :arities (mapv (fn [a] (if (:ret-nhint a) a (assoc a :ret-nhint kind)))
                               (:arities node)))
    node))

;; ---- functional-interface coercion of a hinted let binding (Clojure 1.12) ---
;; (let [^java.util.function.Predicate p even?] (.test p 2)): a binding hinted
;; with a functional interface adapts a fn init to that interface, as the JVM's
;; FISupport.maybeEmitFIAdapter does — only when the value is an IFn and not
;; already an instance of the interface. The adapter's method returns the fn's
;; result converted to the method's return type the way the JVM's FnInvokers do:
;; a boolean return is a Boolean CAST, a numeric one the RT cast (2.7 -> 2).
;; Runnable, Callable and Comparator are absent: a fn already is one of each.
;; Each entry: interface -> [method arity return].
(def ^:private fi-interfaces
  {"java.util.function.Function" ["apply" 1 :obj]
   "java.util.function.BiFunction" ["apply" 2 :obj]
   "java.util.function.UnaryOperator" ["apply" 1 :obj]
   "java.util.function.BinaryOperator" ["apply" 2 :obj]
   "java.util.function.IntFunction" ["apply" 1 :obj]
   "java.util.function.LongFunction" ["apply" 1 :obj]
   "java.util.function.DoubleFunction" ["apply" 1 :obj]
   "java.util.function.Predicate" ["test" 1 :bool]
   "java.util.function.BiPredicate" ["test" 2 :bool]
   "java.util.function.IntPredicate" ["test" 1 :bool]
   "java.util.function.LongPredicate" ["test" 1 :bool]
   "java.util.function.DoublePredicate" ["test" 1 :bool]
   "java.util.function.Consumer" ["accept" 1 :void]
   "java.util.function.BiConsumer" ["accept" 2 :void]
   "java.util.function.IntConsumer" ["accept" 1 :void]
   "java.util.function.LongConsumer" ["accept" 1 :void]
   "java.util.function.DoubleConsumer" ["accept" 1 :void]
   "java.util.function.ObjIntConsumer" ["accept" 2 :void]
   "java.util.function.ObjLongConsumer" ["accept" 2 :void]
   "java.util.function.ObjDoubleConsumer" ["accept" 2 :void]
   "java.util.function.Supplier" ["get" 0 :obj]
   "java.util.function.BooleanSupplier" ["getAsBoolean" 0 :bool]
   "java.util.function.IntSupplier" ["getAsInt" 0 :int]
   "java.util.function.LongSupplier" ["getAsLong" 0 :long]
   "java.util.function.DoubleSupplier" ["getAsDouble" 0 :double]
   "java.util.function.ToIntFunction" ["applyAsInt" 1 :int]
   "java.util.function.ToLongFunction" ["applyAsLong" 1 :long]
   "java.util.function.ToDoubleFunction" ["applyAsDouble" 1 :double]
   "java.util.function.ToIntBiFunction" ["applyAsInt" 2 :int]
   "java.util.function.ToLongBiFunction" ["applyAsLong" 2 :long]
   "java.util.function.ToDoubleBiFunction" ["applyAsDouble" 2 :double]
   "java.util.function.IntUnaryOperator" ["applyAsInt" 1 :int]
   "java.util.function.LongUnaryOperator" ["applyAsLong" 1 :long]
   "java.util.function.DoubleUnaryOperator" ["applyAsDouble" 1 :double]
   "java.util.function.IntBinaryOperator" ["applyAsInt" 2 :int]
   "java.util.function.LongBinaryOperator" ["applyAsLong" 2 :long]
   "java.util.function.DoubleBinaryOperator" ["applyAsDouble" 2 :double]
   "java.util.function.IntToLongFunction" ["applyAsLong" 1 :long]
   "java.util.function.IntToDoubleFunction" ["applyAsDouble" 1 :double]
   "java.util.function.LongToIntFunction" ["applyAsInt" 1 :int]
   "java.util.function.LongToDoubleFunction" ["applyAsDouble" 1 :double]
   "java.util.function.DoubleToIntFunction" ["applyAsInt" 1 :int]
   "java.util.function.DoubleToLongFunction" ["applyAsLong" 1 :long]
   "java.io.FileFilter" ["accept" 1 :bool]
   "java.io.FilenameFilter" ["accept" 2 :bool]})

;; The functional interface a binding's ^tag names, as its FQN, or nil. The tag is
;; written the way any class is: fully qualified, or a name an :import maps.
(defn fi-tag-class [ctx bsym]
  (let [tag (get (form-sym-meta bsym) :tag)
        nm (cond (form-sym? tag) (if (form-sym-ns tag) nil (form-sym-name tag))
                 (string? tag) tag
                 :else nil)]
    (when nm
      (if (contains? fi-interfaces nm)
        nm
        (let [r (resolve-global ctx (symbol nm))
              fqn (if (= :class (:kind r)) (:name r) (resolve-class-hint nm))]
          (when (contains? fi-interfaces fqn) fqn))))))

(defn fi-adapt-form [fqn init]
  (let [[mname arity ret] (get fi-interfaces fqn)
        v (symbol "fi__value")
        ps (mapv #(symbol (str "a" % "__fi")) (range arity))
        call (apply list v ps)
        r (symbol "fi__ret")
        ;; a primitive return unboxes the fn's answer, so nil is the JVM's
        ;; NullPointerException before any conversion
        unbox (fn [conv]
                (list 'let* [r call]
                      (list 'if (list (symbol "clojure.core" "nil?") r)
                            (list 'throw (list 'new (symbol "java.lang.NullPointerException")))
                            (conv r))))
        body (case ret
               ;; the JVM CASTS to Boolean (no truthiness): a non-boolean raises
               :bool (unbox #(list (symbol "clojure.core" "cast") (symbol "java.lang.Boolean") %))
               :int (unbox #(list (symbol "clojure.core" "int") %))
               :long (unbox #(list (symbol "clojure.core" "long") %))
               :double (unbox #(list (symbol "clojure.core" "double") %))
               :void (list 'do call nil)
               call)
        iface (symbol fqn)]
    ;; An IFn that is not already an instance gets the adapter; anything else is
    ;; checked by the cast the JVM's checkcast is — nil and an instance pass, any
    ;; other value is a ClassCastException.
    (list 'let* [v init]
          (list 'if (list 'if (list (symbol "clojure.core" "ifn?") v)
                          (list 'if (list (symbol "clojure.core" "instance?") iface v) false true)
                          false)
                (list (symbol "clojure.core" "reify") iface
                      (list (symbol mname) (into [(symbol "fi__this")] ps) body))
                (list (symbol "clojure.core" "cast") iface v)))))

(defn- conj-hint [acc k nm h]
  (if h (update acc k conj [nm h]) acc))

;; One fixed parameter: its name, and each hint it carries filed under its kind.
(defn- add-param [ctx acc p]
  (let [nm (form-sym-name p) h (hint-of ctx p) ph (phint-of ctx p)
        nh (nhint-of ctx p) ah (ahint-of ctx p) dh (dead-hint-of ctx p)
        proven? (and ph (phint-proven? p))]
    (-> (update acc :fixed conj nm)
        (conj-hint :hints nm h)
        (conj-hint :phints nm (when proven? ph))
        (conj-hint :uphints nm (when (and ph (not proven?)) ph))
        (conj-hint :nhints nm nh)
        (conj-hint :ahints nm ah)
        (conj-hint :dhints nm dh))))

(defn- rest-param-name [r]
  (when-not (form-sym? r)
    (analysis-error :analyze/invalid-fn-parameters
                    "Bad rest parameter, expected symbol"))
  (form-sym-name r))

(defn parse-params [ctx pvec]
  ;; :hints is a vector of [name hint] pairs (vector, not a map, so the caller
  ;; folds it with a plain reduce — no reduce-over-map in the kernel subset).
  ;; :phints is the parallel vector of [name ctor-key] for PROVEN record param
  ;; hints, carrying the specific type for the inference to seed; :uphints is the
  ;; same for the hints a program wrote, which the optimizing passes turn into a
  ;; type test on the param (jolt.passes.types guard-declared-records).
  (loop [i 0 acc {:fixed [] :rest nil :hints [] :phints [] :uphints [] :nhints []
                  :ahints [] :dhints []}]
    (if (< i (count pvec))
      (let [p (nth pvec i)]
        (when-not (form-sym? p)
          (analysis-error :analyze/invalid-fn-parameters
                          "Bad parameter, expected symbol"))
        (if (= "&" (form-sym-name p))
          (recur (+ i 2) (assoc acc :rest (rest-param-name (nth pvec (inc i)))))
          (recur (inc i) (add-param ctx acc p))))
      acc)))

;; A reader that lowers ^meta on a collection to a runtime (with-meta <coll> <meta>)
;; form (the Chez data reader) wraps an arglist vector carrying a return-type hint
;; (^bytes [b] / ^String [x y]). Unwrap to the underlying vector so fn parsing sees
;; the params — the hint is ignored at runtime. Only the (with-meta <vec> _) shape
;; matches, so a real arity clause (head is a vector) and a
;; meta-on-vector arglist pass through unchanged.
(defn strip-arglist-meta [form]
  (if (form-list? form)
    (let [es (vec (form-elements form))]
      (if (and (= 3 (count es))
               (form-sym? (first es))
               (= "with-meta" (form-sym-name (first es)))
               (form-vec? (nth es 1)))
        (nth es 1)
        form))
    form))

;; A ^long/^double on the ARGLIST VECTOR declares that arity's primitive return
;; type — (fn ^long [x] x) and (defn f ^long [x] x) both mean it, and the
;; reference honors both. jolt read the tag only off a defn's NAME, so the
;; arglist spelling was dropped and ((fn ^long [x] x) 18446744073709551616N)
;; answered 2^64 where the reference answers 0. strip-arglist-meta above throws
;; the hint away for parsing, which is right — the params are what fn parsing
;; needs — but the kind has to be read off the ORIGINAL form first.
;;
;; Both reader shapes are covered: the vector carries its metadata directly, and
;; a reader that lowers ^meta on a collection to (with-meta <vec> <meta>) puts it
;; in the third element. A non-numeric tag (^String [x]) still yields nil, so
;; class hints on an arglist stay ignored exactly as before.
(defn arglist-ret-nkind [form]
  (let [m (cond
            (form-vec? form) (form-coll-meta form)
            (form-list? form)
            (let [es (vec (form-elements form))]
              (when (and (= 3 (count es))
                         (form-sym? (first es))
                         (= "with-meta" (form-sym-name (first es)))
                         (form-vec? (nth es 1)))
                (nth es 2)))
            :else nil)]
    (when (map? m) (tag->nkind (get m :tag)))))

;; The compile-time proof that a :host-call's target is a string, driving the
;; backend's direct string-native emission (no record-method-dispatch walk, no
;; jolt-vector rest-args). Three sources: the target's own ^String tag, a ^String
;; let/param binding the target reads through (:hint :str from hint-of), and a
;; string literal. NOT inferred types — this is the hint seam; widening it to the
;; :str lattice value is the follow-up.
(defn str-target-type [raw target]
  (when (or (and (form-sym? raw) (str-tag? (get (form-sym-meta raw) :tag)))
            (= :str (:hint target))
            (and (= :const (:op target)) (string? (:val target))))
    :str))

;; Same seam for keywords. Sources mirror str-target-type: the ^Keyword tag, a
;; :kw-hinted binding, or a keyword literal. honeysql's
;; (.sym ^clojure.lang.Keyword k) is the shape this was written for and jolt
;; reaches it: honeysql orders its conditional #?(:bb … :clj (.sym …)) with :bb
;; first at all three sites, and jolt stopped matching :bb in #893. See
;; keyword-direct-emit.
(defn kw-target-type [raw target]
  (when (or (and (form-sym? raw) (kw-tag? (get (form-sym-meta raw) :tag)))
            (= :kw (:hint target))
            (and (= :const (:op target)) (keyword? (:val target))))
    :kw))

;; And for StringBuilder, the one that pays for itself the most: a jhost method
;; call is the most expensive interop shape jolt has — the tag's method table is
;; found by hashing the tag STRING, the method by hashing its name, the rest args
;; arrive as a jolt-vector and are converted back to a list for `apply`, and the
;; shim lambda takes them as a further rest list. Measured 270 ns for a 0-arg
;; StringBuilder method and 500 ns for (.append sb "a"), against 20 ns on the JVM.
;; Sources are the ^StringBuilder tag and a :sb-hinted binding — which
;; init-proves-hint supplies for (let [sb (StringBuilder.)] …) with nothing written
;; in the source. Both give an IDENTIFIER as the emitted target, which sb-direct-emit
;; requires: its append arm splices the target twice, once to append and once to
;; return (the JVM's fluent contract).
;;
;; A constructor directly in target position, (.append (StringBuilder.) x), is
;; deliberately NOT a source even though it proves the type just as well. It is an
;; expression, so the second splice would run it again and append to a fresh
;; builder — the gate caught exactly that, answering "" for
;; (.toString (.append (StringBuilder.) "lit")). Binding the target to a temporary
;; here would fix it, but nothing writes that shape and the generic path already
;; handles it correctly.
(defn sb-target-type [raw target]
  (let [tag (when (form-sym? raw) (get (form-sym-meta raw) :tag))]
    (cond (or (sb-tag? tag) (= :sb (:hint target))) :sb
          (or (sbuf-tag? tag) (= :sbuf (:hint target))) :sbuf)))
