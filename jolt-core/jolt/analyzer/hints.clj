(ns jolt.analyzer.hints
  "Type hints as the analyzer reads them: the JVM primitive-boolean typing a recur
  is checked against, the ^tag readers for bindings and params (struct, string,
  keyword, StringBuilder, numeric and array kinds), functional-interface
  adaptation of a hinted let binding, fn parameter-vector parsing, and the
  arglist return hint. Reads forms and analyzed nodes; never analyzes."
  (:require [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-sym-meta
                               form-coll-meta form-list? form-vec? form-elements
                               record-type? record-ctor-key resolve-global
                               resolve-class-hint method-return-class
                               method-param-classes method-overload-entry
                               method-sig-registered?]]
            [jolt.analyzer.diagnostics :refer [analysis-error]]
            [jolt.ir :refer [invoke]]))

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

;; --- the receiver class a host call is resolved against ----------------------
;; The JVM compiler resolves (.m x args) to one method when it knows x's class,
;; and then casts each argument to the parameter's class with a checkcast; when
;; it does not, the Reflector finds the method at run time and Class.cast does
;; it. The two raise ClassCastException in different words, so jolt carries the
;; class the compiler would know onto the :host-call (:target-class), and the
;; back end emits the cast where the JVM would have one. The sources are the
;; compiler's: a tag on the target or on the local it reads, the class of a let
;; or loop init (a constructor, a literal, another such local, a call to a var
;; tagged with a class), a catch binding's class. A tag naming no class jolt
;; knows, or a primitive, gives nil.
(defn tag-class [ctx tag]
  (let [nm (cond (form-sym? tag) (if (form-sym-ns tag) nil (form-sym-name tag))
                 (string? tag) tag
                 :else nil)]
    (when nm
      (let [r (resolve-global ctx (symbol nm))]
        (if (= :class (:kind r)) (:name r) (resolve-class-hint nm))))))
(defn sym-tag-class [ctx sym]
  (let [m (form-sym-meta sym)] (when m (tag-class ctx (get m :tag)))))
(declare node-jclass jvm-type reflect-arg-classes)
;; A call's types, memoized by the node's identity for one top-level analysis
;; (analyzer.clj binds it): a call is typed from its arguments' types, asked
;; once per enclosing call each way (node-jclass, jvm-type, the reflect check),
;; so nested calls unmemoized cost exponential time in their depth. A node is
;; only ever typed in the env it was analyzed in.
(def ^:dynamic *jmemo* nil)
(defn- memo [k node f]
  (if-let [m *jmemo*]
    (let [h (jolt.host/identity-hash node)
          hit (some #(when (identical? (nth % 0) node) %) (get-in @m [k h]))]
      (if hit
        (nth hit 1)
        (let [v (f)] (vswap! m update-in [k h] (fnil conj []) [node v]) v)))
    (f)))
(defn jmemo [] (volatile! {}))
(def ^:private jvm-prims #{"long" "double" "boolean" "int" "float" "char" "short" "byte"})
;; An argument's class as the JVM compiler matches it against a method's
;; parameters: node-jclass's, else the type jvm-type gives it ("long" for 1,
;; "double" for (* x 2.0), "null" for nil). nil is an untyped argument, which
;; the compiler takes as an Object.
(defn- arg-jclass [ctx node env]
  (or (node-jclass ctx node env)
      (let [t (jvm-type ctx node env)] (when (string? t) t))))
;; The class a host method's result has: the runtime's signature table
;; (jolt.host/method-return-class) decides from the receiver class and the
;; arguments' classes, as the JVM compiler picks the method it compiles against.
;; A primitive result is left out: node-jclass types no primitive.
(defn- call-return-class [ctx cls method static? args env]
  (when cls
    (let [r (method-return-class cls method static? (mapv #(arg-jclass ctx % env) args))]
      (when (and (string? r) (not (jvm-prims r))) r))))
;; The parameter classes a resolved call (receiver class cls) casts its
;; arguments to, or nil when the call is not resolved to one method — the
;; runtime's signature table picks the method as the JVM compiler does, and
;; refuses as it does when the arguments fit two overloads equally well (a nil
;; literal where one takes a byte[] and another a ByteBuffer).
;; A static call the same way: its arguments may fit two overloads equally
;; well, which the JVM compiler refuses (String/join "," nil). One it resolves
;; to a single overload casts and unboxes its arguments as a resolved instance
;; call does (:arg-casts), and calls the overload's own member when the runtime
;; registers one for it (String.valueOf(char[]) refusing a nil the shim named
;; valueOf would print).
(defn static-invoke [ctx f args env]
  (let [cls (when (= :host-static (:op f)) (tag-class ctx (:class f)))
        acs (when cls (mapv #(arg-jclass ctx % env) args))
        ps (when cls (method-param-classes cls (:member f) acs true))]
    (when (true? ps)
      (analysis-error :analyze/ambiguous-method
                      (str "More than one matching method found: " (:member f))))
    (if (some? ps)
      (let [entry (method-overload-entry cls (:member f) acs)]
        (assoc (invoke (if (string? entry) (assoc f :entry entry) f) args) :arg-casts (vec ps)))
      ;; no overload fits: the Reflector's member when the runtime has one
      ;; (records-interop.ss register-reflect-entry!)
      (let [rcs (when cls (reflect-arg-classes ctx args env))
            entry (when rcs (method-overload-entry cls (:member f) rcs))]
        (invoke (if (string? entry) (assoc f :entry entry) f) args)))))
(defn host-call-arg-casts [ctx cls method args env]
  (when cls
    (let [ps (method-param-classes cls method (mapv #(arg-jclass ctx % env) args))]
      (cond (true? ps) (analysis-error :analyze/ambiguous-method
                                       (str "More than one matching method found: " method))
            (some? ps) (vec ps)))))
;; The class of a constant: a constant collection is the JVM's ConstantExpr,
;; typed by its concrete class.
(defn- const-jclass [v]
  (cond (string? v) "java.lang.String" (keyword? v) "clojure.lang.Keyword"
        (vector? v) "clojure.lang.PersistentVector"
        (set? v) "clojure.lang.PersistentHashSet" :else nil))
;; (jolt.host/static-member "C" "m") with constant arguments
(defn- static-member-call? [f args]
  (and (= :var (:op f)) (= "jolt.host" (str (:ns f))) (= "static-member" (str (:name f)))
       (every? #(= :const (:op %)) args)))
(defn- invoke-jclass [ctx node env]
  (let [f (:fn node)]
    (cond
      (= :host-static (:op f))
      (call-return-class ctx (tag-class ctx (:class f)) (:member f) true (:args node) env)
      ;; (Class/member) with no arguments: a static field or a no-arg static
      ;; method, decided at run time (jolt.host/static-member)
      (static-member-call? f (:args node))
      (let [[c m] (map :val (:args node))]
        (when (and (string? c) (string? m)) (call-return-class ctx (tag-class ctx c) m true [] env)))
      :else
      (let [c (invoke-jvm-class f)]
        (when (and (string? c) (not= c "Object")) (tag-class ctx c))))))
;; IfExpr: both branches typed, and the same class, or one null and the other
;; not a primitive; the then branch's class unless it is null. A primitive
;; result is left untyped here, as node-jclass types no primitive.
(defn- if-jclass [t e]
  (when (and t e)
    (let [c (cond (= t e) t
                  (and (= t "null") (not (jvm-prims e))) e
                  (and (= e "null") (not (jvm-prims t))) t)]
      (when-not (jvm-prims c) c))))
;; The class the JVM compiler gives an expression, "null" for one it types as
;; null (a nil literal, or a let, do or if that ends in one), or nil.
(defn node-jclass [ctx node env]
  (case (:op node)
    :local (get (:jclasses env) (:name node))
    :host-new (tag-class ctx (:class node))
    :const (if (nil? (:val node)) "null" (const-jclass (:val node)))
    ;; LetExpr is its body's class; the let's own locals are left untyped
    ;; rather than read from the outer env, where the same names may mean others
    :let (node-jclass ctx (:body node)
                      (update env :jclasses #(apply dissoc % (map first (:bindings node)))))
    :if (if-jclass (arg-jclass ctx (:then node) env) (arg-jclass ctx (:else node) env))
    ;; one with an element computed at run time is a VectorExpr/SetExpr/MapExpr,
    ;; typed by its interface; all-constant elements make it a constant
    :vector (if (every? #(= :const (:op %)) (:items node))
              "clojure.lang.PersistentVector" "clojure.lang.IPersistentVector")
    :set (if (every? #(= :const (:op %)) (:items node))
           "clojure.lang.PersistentHashSet" "clojure.lang.IPersistentSet")
    :map "clojure.lang.IPersistentMap"
    :quote (when (form-list? (:form node)) "clojure.lang.PersistentList")
    :bigdec "java.math.BigDecimal"
    :do (node-jclass ctx (:ret node) env)
    :host-call (memo :nj node #(call-return-class ctx (:target-class node) (:method node) false
                                                (:args node) env))
    :invoke (memo :nj node #(invoke-jclass ctx node env))
    nil))
;; The class a (.method target ...) call's target has: its ^Tag, else what the
;; analyzed target node is known to be.
(defn host-call-target-class [ctx raw target env]
  (let [m (cond (form-sym? raw) (form-sym-meta raw)
                (form-list? raw) (form-coll-meta raw)
                :else nil)
        tag (when (map? m) (get m :tag))]
    (if tag
      (tag-class ctx tag)
      (let [c (node-jclass ctx target env)] (when-not (= c "null") c)))))
;; A let or loop local's class from its init: a nil gives the local the null
;; type, which picks an overload as the literal would (String/valueOf of such a
;; local is the char[] one).
(defn init-jclass [ctx init env] (node-jclass ctx init env))
(defn add-jclass [env nm c]
  (if c (assoc env :jclasses (assoc (:jclasses env) nm c)) env))

;; --- the JVM type of an argument ---------------------------------------------
;; What the JVM compiler types an expression as when it picks a method for it:
;; a class, a primitive ("double" for (* x 2.0)), "null", nil for an Object, or
;; :unknown when jolt cannot say what the JVM would. A static call no overload
;; fits reaches the Reflector on the JVM, which picks by the arguments' runtime
;; classes; it takes the Reflector's member here (reflect-arg-classes) unless
;; an argument is :unknown, since the JVM may have typed that one a primitive
;; and resolved the call statically ((Float/valueOf d) for a double d).
;; Where the JVM compiler leaves an expression an Object, so does this.
(def ^:private cast-jtypes
  {"jolt-double" "double" "jolt-long-cast" "long" "jolt-int-cast" "int"
   "jolt-float" "float" "jolt-byte-cast" "byte" "jolt-short-cast" "short"
   "jolt-char" "char" "jolt-unchecked-long" "long" "jolt-unchecked-int" "int"
   "jolt-unchecked-float" "float"})
;; clojure.core fns the JVM compiler inlines to a clojure.lang.Numbers static:
;; at two arguments, folded left past two, and at one
(def ^:private numbers-nary
  {"+" "add" "+'" "addP" "-" "minus" "-'" "minusP" "*" "multiply" "*'" "multiplyP"
   "/" "divide" "max" "max" "min" "min"})
(def ^:private numbers-binary
  {"quot" "quotient" "rem" "remainder" "unchecked-add" "unchecked_add"
   "unchecked-subtract" "unchecked_minus" "unchecked-multiply" "unchecked_multiply"})
(def ^:private numbers-unary
  {"-" "minus" "-'" "minusP" "inc" "inc" "inc'" "incP" "dec" "dec" "dec'" "decP"
   "abs" "abs" "unchecked-inc" "unchecked_inc" "unchecked-dec" "unchecked_dec"
   "unchecked-negate" "unchecked_minus"})
;; the other inlines whose result the JVM types the same whatever the arguments:
;; name -> {arity type}
(def ^:private core-inline-jtypes
  (merge
   (zipmap ["count" "alength"] (repeat {1 "int"}))
   (zipmap ["compare"] (repeat {2 "int"}))
   (zipmap ["unchecked-add-int" "unchecked-subtract-int" "unchecked-multiply-int"
            "unchecked-divide-int" "unchecked-remainder-int"] (repeat {2 "int"}))
   (zipmap ["unchecked-inc-int" "unchecked-dec-int" "unchecked-negate-int" "int"
            "unchecked-int"] (repeat {1 "int"}))
   (zipmap ["double" "unchecked-double"] (repeat {1 "double"}))
   (zipmap ["float" "unchecked-float"] (repeat {1 "float"}))
   (zipmap ["long" "unchecked-long" "bit-not"] (repeat {1 "long"}))
   (zipmap ["bit-shift-left" "bit-shift-right" "unsigned-bit-shift-right"] (repeat {2 "long"}))
   (zipmap ["char" "unchecked-char"] (repeat {1 "char"}))
   (zipmap ["byte" "unchecked-byte"] (repeat {1 "byte"}))
   (zipmap ["short" "unchecked-short"] (repeat {1 "short"}))
   (zipmap ["boolean" "NaN?" "infinite?" "reduced?" "zero?" "pos?" "neg?" "nil?"] (repeat {1 "boolean"}))
   (zipmap ["=" "==" "<" "<=" ">" ">=" "identical?"] (repeat {2 "boolean"}))
   {"num" {1 "java.lang.Number"}}))
(def ^:private bit-nary #{"bit-and" "bit-or" "bit-xor" "bit-and-not"})
(def ^:private array-components
  {"[D" "double" "[F" "float" "[J" "long" "[I" "int" "[S" "short" "[B" "byte"
   "[C" "char" "[Z" "boolean"})

;; any keyword: :unknown or a recur
(defn- some-unknown? [ts] (some keyword? ts))
;; A ^tag: a primitive's name, a class jolt resolves, or :unknown.
(defn- tag-jtype [ctx tag]
  (let [s (cond (form-sym? tag) (form-sym-name tag) (symbol? tag) (name tag) (string? tag) tag)]
    (cond (jvm-prims s) s
          :else (or (tag-class ctx tag) :unknown))))
;; Numbers.<m> at these argument types, as getMatchingParams picks it; no
;; overload fitting is a reflective call, an Object.
(defn- numbers-jtype [m ts]
  (if (some-unknown? ts)
    :unknown
    (let [r (method-return-class "clojure.lang.Numbers" m true ts)] (when (string? r) r))))
;; A method call's result: the one overload the arguments pick and the class it
;; returns; a reflective call (registered overloads, none fitting) is an Object;
;; a method the signature table does not have, or whose primitive result it
;; does not record, is :unknown.
(defn- method-jtype [cls method static? ts]
  (if (some-unknown? ts)
    :unknown
    (let [ps (method-param-classes cls method ts static?)]
      (cond (true? ps) :unknown
            (some? ps) (let [r (method-return-class cls method static? ts)]
                         (if (string? r) r :unknown))
            (method-sig-registered? cls method static? (count ts)) nil
            :else :unknown))))
(defn- args-jtypes [ctx args env] (mapv #(jvm-type ctx % env) args))
(defn- core-invoke-jtype [ctx node nm args env]
  (let [n (count args)
        ts #(args-jtypes ctx args env)]
    (cond
      (and (>= n 2) (numbers-nary nm))
      (let [[a b & more] (ts) m (numbers-nary nm)]
        (reduce (fn [t x] (numbers-jtype m [t x])) (numbers-jtype m [a b]) more))
      (and (= n 2) (numbers-binary nm)) (numbers-jtype (numbers-binary nm) (ts))
      (and (= n 1) (numbers-unary nm)) (numbers-jtype (numbers-unary nm) (ts))
      (and (>= n 2) (bit-nary nm)) "long"
      (get-in core-inline-jtypes [nm n]) (get-in core-inline-jtypes [nm n])
      (jvm-boolean-prim? node env) "boolean"
      ;; RT.aget: the component of an array the compiler knows, else reflective
      (and (= nm "aget") (= n 2))
      (let [a (jvm-type ctx (first args) env)]
        (if (keyword? a) :unknown (get array-components a)))
      ;; a helper jolt's own macros expand to has no JVM expansion to compare
      (or (= "__" (subs nm 0 (min 2 (count nm))))
          (:private (meta (find-var (symbol "clojure.core" nm)))))
      :unknown
      :else nil)))
;; InvokeExpr of a var: the arglist's :tag at this arity (a ^double arglist is
;; a primitive invoke), else the var's :tag (node-jclass has the class ones).
(defn- var-invoke-jtype [ctx f n]
  (let [ns (str (:ns f))]
    (if (= "jolt." (subs ns 0 (min 5 (count ns))))
      :unknown
      (let [m (meta (find-var (symbol ns (str (:name f)))))
            al (some #(when (and (vector? %) (= n (count %)) (not (some #{'&} %))) %)
                     (:arglists m))
            st (some-> al meta :tag)
            vt (invoke-jvm-class f)]
        (cond st (tag-jtype ctx st)
              (or (nil? vt) (= vt "Object")) nil
              :else :unknown)))))
;; (Class/member) with no arguments: the field's or no-arg method's type, a
;; primitive included ((System/currentTimeMillis) is a long)
(defn- static-member-jtype [ctx args]
  (let [[c m] (map :val args) cls (when (and (string? c) (string? m)) (tag-class ctx c))]
    (if cls (method-jtype cls m true []) :unknown)))
(defn- invoke-jtype [ctx node env]
  (let [f (:fn node) args (:args node)]
    (case (:op f)
      :var (cond (static-member-call? f args) (static-member-jtype ctx args)
                 (= "clojure.core" (str (:ns f))) (core-invoke-jtype ctx node (str (:name f)) args env)
                 :else (var-invoke-jtype ctx f (count args)))
      :host-static (let [cls (tag-class ctx (:class f))]
                     (if cls (method-jtype cls (:member f) true (args-jtypes ctx args env)) :unknown))
      ;; a local, a keyword, a fn: an InvokeExpr with no tag
      nil)))
(defn- host-call-jtype [ctx node env]
  (let [t (or (:target-class node) (jvm-type ctx (:target node) env))]
    (cond (keyword? t) :unknown
          ;; untyped, null or primitive: reflective, an Object
          (or (nil? t) (= t "null") (jvm-prims t)) nil
          :else (method-jtype t (:method node) false (args-jtypes ctx (:args node) env)))))
;; IfExpr: both branches typed, and the same, or one null and the other not a
;; primitive; a recur branch takes the other's type. Anything else is an Object.
(defn- if-jtype [t e]
  (cond (= t :recur) e
        (= e :recur) t
        (or (keyword? t) (keyword? e)) :unknown
        (and (string? t) (string? e))
        (cond (= t e) t
              (and (= t "null") (not (jvm-prims e))) e
              (and (= e "null") (not (jvm-prims t))) t)
        :else nil))
;; What a local's binding records in :jlocals (analyzer analyze-bindings): its
;; tag's type, or its init to type when asked, memoized.
(defn binding-jlocal [ctx bsym init env]
  (let [m (form-sym-meta bsym) tag (when m (get m :tag))]
    (if tag (tag-jtype ctx tag) {:init init :env env :memo (volatile! nil)})))
(defn add-jlocal [env nm j] (assoc env :jlocals (assoc (:jlocals env) nm j)))
(defn- local-jtype [ctx nm env]
  (let [j (get (:jlocals env) nm)
        t (cond (string? j) j
                (keyword? j) j
                (:tag j) (do (when-let [s (:seen j)] (vreset! s true)) (:tag j))
                (map? j) (let [v (:memo j)]
                           (or @v (vreset! v (let [t (jvm-type ctx (:init j) (:env j))]
                                              (if (nil? t) :object t))))))]
    (if (= t :object) nil t)))
(defn jvm-type [ctx node env]
  (if (:jtag node)
    :unknown
    (case (:op node)
      :local (or (get (:jclasses env) (:name node)) (local-jtype ctx (:name node) env))
      :const (let [v (:val node) c (const-jvm-class v)]
               (cond (= :null c) "null" (string? c) c :else (const-jclass v)))
      :if (if-jtype (jvm-type ctx (:then node) env) (jvm-type ctx (:else node) env))
      :do (jvm-type ctx (:ret node) env)
      :let (jvm-type ctx (:body node)
                     (reduce (fn [e [nm init]]
                               (add-jlocal (update e :jclasses dissoc nm) nm
                                           {:init init :env e :memo (volatile! nil)}))
                             env (:bindings node)))
      :coerce (if-let [c (:cast-fn node)] (get cast-jtypes c :unknown) (jvm-type ctx (:expr node) env))
      :invoke (memo :jt node #(or (node-jclass ctx node env) (invoke-jtype ctx node env)))
      :host-call (memo :jt node #(host-call-jtype ctx node env))
      :recur :recur
      ;; a loop or try as an argument compiles as a call to a fn: an Object
      (:loop :try :fn :throw) nil
      (:vector :set :map :quote :bigdec :host-new) (node-jclass ctx node env)
      :unknown)))
;; The classes a static call no overload fits reflects on, a primitive as its
;; box and an Object or a null as an Object, or nil when an argument's type is
;; :unknown.
(def ^:private jvm-boxes
  {"long" "java.lang.Long" "double" "java.lang.Double" "int" "java.lang.Integer"
   "float" "java.lang.Float" "char" "java.lang.Character" "boolean" "java.lang.Boolean"
   "short" "java.lang.Short" "byte" "java.lang.Byte"})
(defn- reflect-arg-classes [ctx args env]
  (let [ts (mapv #(or (node-jclass ctx % env) (jvm-type ctx % env)) args)]
    (when-not (some #(#{:unknown :recur} %) ts)
      (mapv #(cond (or (nil? %) (= % "null")) "java.lang.Object" (jvm-prims %) (get jvm-boxes %)
                   :else %) ts))))

;; --- loop locals the JVM types a primitive ------------------------------------
;; LetExpr compiles a loop whose local starts as a primitive long or double (an
;; int or float init widened) with that local primitive, and, when a recur
;; passes it something that is not (an Object, a double for a long), compiles
;; the loop again with it boxed, a Number, until no recur mismatches. jolt does
;; the same for the typing above: ANALYZE (the body under an env) runs with the
;; locals primitive, and again with the mismatched ones Numbers, only when the
;; body asked a local's type. note-recur! records each recur's argument types.
(def ^:private loop-prims {"long" "long" "int" "long" "double" "double" "float" "double"})
(def ^:private recur-fits
  {"long" #{"long" "int" "short" "char" "byte"} "double" #{"double" "float"}})
(defn note-recur! [ctx env args]
  (let [[target cell] (:jloop env)]
    (when (and cell (= target (:recur env)))
      (vswap! cell conj (fn [] (mapv #(jvm-type ctx % env) args))))))
;; The candidates some recur passes a mismatched type for, added to BOXED: one
;; it cannot type stays :unknown, any other mismatch boxes it as a Number.
(defn- recur-mismatches [boxed cands recur-types]
  (reduce (fn [m ts]
            (reduce (fn [m [i nm p]]
                      (let [t (nth ts i)]
                        (cond (contains? m nm) m
                              (keyword? t) (assoc m nm :unknown)
                              (contains? (recur-fits p) t) m
                              :else (assoc m nm "java.lang.Number"))))
                    m cands))
          boxed recur-types))
(defn loop-body [ctx names env analyze]
  (let [cands (keep-indexed
               (fn [i nm]
                 (when (map? (get (:jlocals env) nm))
                   (when-let [p (loop-prims (local-jtype ctx nm env))] [i nm p])))
               names)]
    (if (empty? cands)
      (analyze env)
      (loop [boxed {}]
        (let [seen (volatile! false) recurs (volatile! [])
              e (reduce (fn [e [_ nm p]] (add-jlocal e nm (or (get boxed nm) {:tag p :seen seen})))
                        (assoc env :jloop [(:recur env) recurs]) cands)
              node (analyze e)
              m (if @seen (recur-mismatches boxed cands (map #(%) @recurs)) boxed)]
          (if (= m boxed) node (recur m)))))))

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

;; The names of a let/loop binding vector's ^double-hinted locals. On the JVM
;; such a hint does not convert the value the local holds, but each primitive
;; arithmetic use unboxes it (RT.doubleCast); jolt.passes.numeric casts those
;; uses (dcast-args).
(defn double-hinted-names [ctx bvec]
  (loop [i 0 acc []]
    (if (< i (count bvec))
      (let [b (nth bvec i)]
        (recur (+ i 2) (if (and (form-sym? b) (= :double (nhint-of ctx b))) (conj acc (form-sym-name b)) acc)))
      acc)))

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
    ;; the JVM's own class-name spellings too: ^"[Ljava.lang.Object;" is how
    ;; tools.reader hints its pushback buffer, ^"[D" a double array
    (cond (or (= s "doubles") (= s "[D")) :doubles (or (= s "floats") (= s "[F")) :floats
          (or (= s "longs") (= s "[J")) :longs (or (= s "ints") (= s "[I")) :ints
          (or (= s "bytes") (= s "[B")) :bytes (or (= s "objects") (= s "[Ljava.lang.Object;")) :objects
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
        (conj-hint :dhints nm dh)
        (conj-hint :jclasses nm (sym-tag-class ctx p))
        (conj-hint :jlocals nm (let [m (form-sym-meta p) t (when m (get m :tag))]
                                 (when t (tag-jtype ctx t)))))))

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
                  :ahints [] :dhints [] :jclasses [] :jlocals []}]
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
;; An arglist's own metadata, written directly or as the (with-meta [...] m)
;; form a macro leaves when it attaches it.
(defn arglist-meta [form]
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
    (when (map? m) m)))

(defn arglist-ret-nkind [form]
  (when-let [m (arglist-meta form)] (tag->nkind (get m :tag))))

;; A deftype/defrecord/reify method's arglist, marked by those macros. The
;; reference compiles one as a method of the class (NewInstanceMethod), which has
;; no MAX_POSITIONAL_ARITY limit, unlike a fn.
(defn arglist-type-method? [form]
  (boolean (get (arglist-meta form) :jolt/type-method)))

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
