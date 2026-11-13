(ns jolt.analyzer.interop
  "Host-interop names and rewrites the analyzer recognizes in a call head or a
  symbol: member/ctor/field heads, Clojure 1.12 array class symbols and method
  values, the class-or-namespace decision for a qualified non-var name, and the
  core arithmetic/cast rewrites. Answers names and forms; never analyzes."
  (:require [jolt.host :refer [resolve-global resolve-class-hint host-class-name?
                               namespace-for ns-shaped-name? late-bound-namespace?
                               allow-unresolved-vars? compile-ns invoke-rewriter]]
            [jolt.analyzer.diagnostics :refer [analysis-error]]))

;; A `.-field` head: `(.-field target)` is field access (the dash signals field
;; access to the host-call dispatcher). analyze-set! and analyze-list both read
;; it.
(defn field-head? [nm]
  (and (> (count nm) 2) (= ".-" (subs nm 0 2))))

;; Host interop method call. `(.method target arg*)` — a head that
;; starts with "." but not ".-" (field access stays punted). Analyzes to a
;; :host-call node; the Chez back end lowers it to a jolt-host-call dispatch.
(defn method-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm 0 1))
       (not (= "-" (subs nm 1 2)))     ; .-field is field access
       (not (= "." (subs nm 1 2)))))   ; .. is the threading macro, not .method

;; A constructor head: `Class.` — a symbol ending in "." (but not the member
;; access `.method` / `..` forms). `(Class. args*)` builds an instance.
(defn ctor-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm (dec (count nm)) (count nm)))
       (not (= "." (subs nm 0 1)))))

;; ---- Clojure 1.12 qualified methods and array class symbols ----------------
;; The JVM descriptor letter of each primitive an array class symbol may name
;; (long/2 is long[][], "[[J").
(def ^:private array-prim-letters
  {"boolean" "Z" "byte" "B" "char" "C" "short" "S" "int" "I" "long" "J"
   "float" "F" "double" "D"})

;; `Component/N` names an N-dimensional array class (Compiler.maybeArrayClass):
;; a qualified symbol whose name is ONE digit 1-9.
(defn array-class-dims [nm]
  (when (and (= 1 (count nm)) (contains? #{"1" "2" "3" "4" "5" "6" "7" "8" "9"} nm))
    (parse-long nm)))

;; The JVM name of the array class `cname/dims` names — "[[J" for long/2,
;; "[Ljava.lang.String;" for String/1 — or nil when cname is no class. The
;; component resolves the way a class symbol does: a primitive, an auto-imported
;; or :import-ed class, a deftype, or a fully-qualified name.
(defn array-class-name [ctx cname dims]
  (let [prefix (apply str (repeat dims "["))
        letter (get array-prim-letters cname)]
    (if letter
      (str prefix letter)
      (let [r (resolve-global ctx (symbol cname))
            fqn (cond (= :class (:kind r)) (:name r)
                      (resolve-class-hint cname) (resolve-class-hint cname)
                      (host-class-name? cname) cname
                      :else nil)]
        (when fqn (str prefix "L" fqn ";"))))))

;; Fresh parameter names for a synthesized method-value fn. The `__qm` suffix
;; keeps them out of the way of any name the method's class symbol could use.
(defn- qm-params [n] (mapv #(symbol (str "p" % "__qm")) (range n)))

;; `Class/new` and `Class/.method` in VALUE position (Clojure 1.12 method values)
;; are fns. With :param-tags (^[long String] Foo/.bar) the arity is known, so the
;; fn calls the member directly; without them the fn is variadic and reaches the
;; member reflectively, which is what the JVM compiles an unresolved method value
;; to. The forms are analyzed like any written fn.
(defn method-value-form [cname mname ptags]
  (let [csym (symbol cname)
        ctor? (= mname "new")
        n (when (vector? ptags) (count ptags))]
    (if n
      (let [ps (qm-params n)]
        (if ctor?
          (list 'fn* ps (apply list 'new csym ps))
          (let [t (symbol "target__qm")]
            (list 'fn* (into [t] ps) (apply list '. t (symbol (subs mname 1)) ps)))))
      (let [a (symbol "args__qm")]
        (if ctor?
          (list 'fn* ['& a]
                (list (symbol "clojure.lang.Reflector" "invokeConstructor")
                      csym (list (symbol "clojure.core" "to-array") a)))
          (let [t (symbol "target__qm")]
            (list 'fn* [t '& a]
                  (list (symbol "clojure.lang.Reflector" "invokeInstanceMethod")
                        t (subs mname 1) (list (symbol "clojure.core" "to-array") a)))))))))

;; The namespace half of a qualified, non-var `ns/nm` that the host-static and
;; ctor arms may take as a class: `ns` itself, or, where a miss stays late-bound,
;; the namespace an alias names (an alias is lexical, so it is resolved here, and
;; the miss at the call names the namespace whatever *ns* is then). Raises the
;; JVM's compile-time errors (Compiler.resolveIn) instead when `ns` cannot be a
;; class: "No such namespace" for a namespace-shaped name that names nothing
;; loaded, "No such var" for an alias or loaded namespace that lacks the var. The
;; symbol arm and the zero-arg (ns/nm) and (ns/new …) call arms share it, so
;; `(s/nope)` is the same compile error as `(s/nope 1)` and `s/nope`.
;; A registered class wins over the shape: (defrecord point [x]) makes
;; `point/create` a static call however lowercase the name, as the JVM's
;; macroexpand1 finds the class before resolveIn ever runs.
(defn static-class-ns [ctx ns nm]
  (let [target (namespace-for ctx ns)]
    (cond
      (host-class-name? ns) ns
      (and (ns-shaped-name? ns) (not target))
        (analysis-error :analyze/unknown-namespace
         (str "No such namespace: " ns)
         {:jolt.error/symbol (str ns "/" nm)
          :jolt.error/namespace ns
          :jolt.error/ns (compile-ns ctx)})
      (and target (not (late-bound-namespace? ctx target)) (not (allow-unresolved-vars?)))
        (analysis-error :analyze/no-such-var
         (str "No such var: " ns "/" nm)
         {:jolt.error/symbol (str ns "/" nm)
          :jolt.error/namespace target
          :jolt.error/ns (compile-ns ctx)})
      :else (or target ns))))

;; The wrapping unchecked-* name a core arithmetic op rewrites to under
;; *unchecked-math*, or nil. n is the full item count (head + args); unary - is a
;; negate.
(defn unchecked-arith [hname n]
  (cond
    (= hname "+") "unchecked-add"
    (= hname "*") "unchecked-multiply"
    (= hname "-") (if (= n 2) "unchecked-negate" "unchecked-subtract")
    (= hname "inc") "unchecked-inc"
    (= hname "dec") "unchecked-dec"
    :else nil))

;; A clojure.core numeric cast (double/long/int/float/byte/short of one arg)
;; becomes a :coerce node carrying the checked runtime helper, so it feeds the
;; numeric lattice like a ^double/^long hint: (* (double x) 2.0) emits fl*. The
;; helper preserves clojure.core's full JVM semantics (checked, not bare
;; jolt->fl). float -> its own :float kind (a jfloat, which an arithmetic op
;; takes as the double it widens to: jolt.passes.numeric); int ->
;; long-kind (Jolt's integer-box model), routing to jolt-int-cast so the JVM int
;; range is still enforced. Returns {:kind .. :cast-fn ..} or nil. n is the full
;; item count (head + args).
(defn num-cast [hname n]
  (when (= n 2)
    (cond (= hname "double") {:kind :double :cast-fn "jolt-double"}
          (= hname "long")   {:kind :long   :cast-fn "jolt-long-cast"}
          (= hname "int")    {:kind :long   :cast-fn "jolt-int-cast"}
          (= hname "float")  {:kind :float  :cast-fn "jolt-float"}
          ;; byte/short narrow to a RANGE, so their answer is a fixnum on every
          ;; tower jolt has (jolt-checked-cast returns a value inside [lo,hi] or
          ;; throws) — :long, the same kind `int` takes, and for the same reason.
          ;; Being in this table is what keeps (byte v) from costing more than the
          ;; store it feeds: outside it, a var-deref + jolt-invoke1 wrap
          ;; jolt-byte-cast's two fixnum compares for 17.9ns an element, in exactly
          ;; the shape every byte-filling loop is written in.
          (= hname "byte")   {:kind :long   :cast-fn "jolt-byte-cast"}
          (= hname "short")  {:kind :long   :cast-fn "jolt-short-cast"}
          ;; char is a cast too, but its answer is not a numeric operand: kind
          ;; nil, which numeric/an and the back end's cond read as "no kind";
          ;; types.clj's :coerce arm answers :truthy (a char's type) for a cast
          ;; with no kind, never the operand's type. Being a :coerce node is what
          ;; lets a proven-double operand take the inline window (:fl-arg).
          (= hname "char")   {:kind nil     :cast-fn "jolt-char"}
          ;; the unchecked casts are casts too: a primitive long/int on the JVM,
          ;; so the result feeds the :long lattice the same way. Their wrap
          ;; result can be a bignum past the 61-bit fixnum, which is the case
          ;; every :long consumer already guards (the jolt-l* fixnum? tests) —
          ;; the same reason a ^long param is admitted. Before this they were
          ;; ordinary var calls and left every loop counter they fed untyped.
          (= hname "unchecked-long") {:kind :long :cast-fn "jolt-unchecked-long"}
          (= hname "unchecked-int")  {:kind :long :cast-fn "jolt-unchecked-int"}
          (= hname "unchecked-float") {:kind :float :cast-fn "jolt-unchecked-float"})))

;; The compile-time hook over calls through a var (jolt.host/invoke-rewriter,
;; nil unless something bound jolt.host/*invoke-rewrite*): (f var-ns var-name
;; form) answers the form to analyze instead, or nil. Resolved only when a hook
;; is bound, so an ordinary call pays one deref, never a resolution.
(defn invoke-rewrite [ctx head form]
  (when-let [f (invoke-rewriter)]
    (let [r (resolve-global ctx head)]
      (when (= :var (:kind r))
        (f (:ns r) (:name r) form)))))
