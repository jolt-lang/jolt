;;   Portions of this file are from Clojure's src/clj/clojure/core.clj,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; clojure.core — macro tier. Macros expressed in Clojure (defmacro + syntax-quote).
;; Loaded after the fn tiers, so a macro here may use any already-frozen core
;; fn/macro.
;;
;; IMPORTANT — only macros NOT used by the self-hosted compiler (jolt-core/jolt/*)
;; or by the earlier overlay tiers belong here. Those needed earlier
;; (and/or/when/when-not/when-let/cond/case/doseq/declare/cond->/->) are defmacros
;; in 00-syntax.clj, which loads first. Everything here is user-facing.
;;
;; Migration: remove the host core-X macro fn AND its core-macro-names entry when
;; moving a macro here (defmacro installs the :macro flag itself).

(defmacro comment [& body] nil)

;; with-out-str: capture everything the body prints to *out* and return it as a
;; string. __with-out-str (clojure.core) runs the thunk with the output captured.
;; The bare try is the same guard locking carries: the reference's with-out-str
;; expands through `binding`, which is a try/finally, so a recur may not cross it.
;; Without it jolt's capture thunk became the recur target and
;; (loop [] (with-out-str (recur))) spun forever.
(defmacro with-out-str [& body]
  `(__with-out-str (fn* [] (try ~@body))))

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
    ;; (def ~name) declares at ANALYSIS time (analyze-def interns the cell), so
    ;; a reference later in the same top-level form resolves under strict
    ;; analysis — the JVM's defmulti interns the same way by expanding through
    ;; def. defmulti-setup then fills the root; declare never resets one.
    ;; defmulti returns the var (JVM), not the multifn value defmulti-setup returns.
    `(do (def ~name) (defmulti-setup (quote ~qname) ~dispatch ~@opts) (var ~name))))

(defmacro defmethod [mm dispatch-val & fn-tail]
  ;; the expansion ns rides along so a deferred defmethod resolves its multifn
  ;; against the ns it was written in (aliases and refers included).
  `(defmethod-setup (quote ~mm) ~dispatch-val (fn ~@fn-tail)
                    ~(str (clojure.core/ns-name clojure.core/*ns*))))

;; Multimethod table ops are FUNCTIONS (JVM parity): the multifn VALUE carries
;; its method/prefer tables, so they take the value directly — the same shape
;; methods/get-method always used — and work under apply/map/partial. The *-setup
;; host fns take the value; the old name-quoting was incidental.
(defn prefer-method [mm dval-a dval-b]
  (prefer-method-setup mm dval-a dval-b))

(defn remove-method [mm dval]
  (remove-method-setup mm dval))

(defn remove-all-methods [mm]
  (remove-all-methods-setup mm))

;; methods/get-method take the multimethod VALUE (Clojure semantics); the setup
;; uses it directly, so a bare multifn ref works from a compiled fn in any
;; namespace.
(defn get-method [mm dval]
  (get-method-setup mm dval))

(defn methods [mm]
  (methods-setup mm))

;; prefers reads the store off the multifn VALUE.
(defn prefers [mm]
  (prefers-setup mm))

;; A protocol's identity is its defining namespace plus its name: two protocols
;; named alike in different namespaces are distinct interfaces on the JVM, so
;; their impls must not share a dispatch table. protocol-key-of resolves the
;; symbol through :refer and :as and answers the key the protocol was defined
;; with; a symbol naming no protocol — a host class or interface (Object,
;; java.util.Map, an :import-ed clojure.lang.ILookup) — keeps its bare name,
;; which is how the tags a value reports are spelled.
(defn- protocol-key [sym]
  (or (jolt.host/protocol-key-of sym) (name sym)))

;; instance? stays a MACRO: jolt class names (and host-shim class names registered
;; via __register-class-ctor!) don't uniformly evaluate to values, so a bare
;; class-name symbol is passed QUOTED to the ctx-capturing checker. A LIST in type
;; position is a class-valued expression (Class/forName "[C") — evaluate it. A LOCAL
;; in &env may hold a class value from (let [c java.util.Map] (instance? c x)). A
;; symbol resolving to a var that HOLDS A CLASS VALUE also evaluates. instance? is
;; a macro so it can quote a bare class name, but the analyzer resolves a
;; value-position reference — (partial instance? SomeClass) — to clojure.core/
;; instance-check (the fn this expands to), so instance? works as a value for a
;; class that evaluates to a Class (java.*/clojure.lang.*); only a bare deftype/
;; shim name passed as a value can't (no evaluable Class).
;; A symbol naming a PROTOCOL passes its ns-qualified key instead of the bare
;; name, so (instance? Shape x) asks about the Shape of THIS namespace — the JVM
;; resolves the same symbol to one specific generated interface.
(defmacro instance? [t x]
  (let [local? (or (seq? t) (contains? &env t))
        pkey (when (and (symbol? t) (not local?)) (jolt.host/protocol-key-of t))]
    (cond
      local? `(instance-check ~t ~x)
      pkey `(instance-check ~pkey ~x)
      ;; an array class symbol (String/1, long/2) evaluates to its Class
      (and (symbol? t) (namespace t) (contains? #{"1" "2" "3" "4" "5" "6" "7" "8" "9"} (name t)))
      `(instance-check ~t ~x)
      (and (symbol? t)
           (when-let [v (clojure.core/resolve t)]
             ;; resolve hands back the class itself for a class mapping; a var
             ;; may still hold one ((def C SomeClass) — resolve keeps the var).
             (or (jolt.host/class-object? v)
                 (and (clojure.core/bound? v)
                      (jolt.host/class-object? (clojure.core/var-get v))))))
      `(instance-check ~t ~x)
      :else `(instance-check (quote ~t) ~x))))

;; Take x's monitor for the duration of body (futures/agents/threads share one
;; heap, so this is a real per-object lock), releasing on any exit.
;;
;; The bare try costs nothing — with no catch and no finally, emit-try emits the
;; body and nothing else — and it is what keeps `recur` honest. The reference's
;; locking IS a try/finally, so a recur may not cross it; jolt's thunk is a
;; fiber-scheduling requirement (an OS mutex has thread granularity and a fiber
;; is not a thread — java/concurrency.ss), and without the try that thunk quietly
;; became the recur TARGET: (loop [] (locking o (recur))) spun forever instead of
;; being refused.
(defmacro locking [x & body]
  `(jolt.host/with-monitor ~x (fn* [] (try ~@body))))

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
;; time: evaluate expr, print how long it took, return the value. Matches the
;; reference macro on both counts it is observed by.
;;
;; The clock is the MONOTONIC nanosecond one, divided to fractional milliseconds,
;; as (System/nanoTime) is there. It used to be current-time-ms — the epoch wall
;; clock — which reported whole milliseconds, so anything faster than 1 ms timed as
;; "0 msecs", and could run backwards across a clock adjustment and print a negative
;; elapsed. (The comment here claimed current-time-ms was monotonic; it is not.)
;;
;; And it prints with prn, not println, so the line is the QUOTED string the
;; reference emits — "Elapsed time: 0.004583 msecs" — which is what a caller
;; comparing jolt's output against Clojure's sees.
(defmacro time [expr]
  `(let [start# (current-time-ns)
         ret# ~expr]
     (prn (str "Elapsed time: " (/ (double (- (current-time-ns) start#)) 1000000.0) " msecs"))
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
;;
;; The init goes into a THREAD BINDING, not the cell's root — Clojure's expansion
;; is Var/create + setDynamic followed by pushThreadBindings, and var-set (Var.set)
;; only ever writes a thread binding. Rooting them instead left the locals
;; thread-bound? false and only worked because var-set used to fall back to a root
;; write. The frame is scoped like `binding`'s, so leaving the form — by a throw as
;; much as by returning — unbinds them again.
(defmacro with-local-vars [bindings & body]
  (let [ps (partition 2 bindings)
        binds (reduce (fn [acc p] (conj (conj acc (first p)) `(__local-var))) [] ps)
        pairs (reduce (fn [acc p] (conj (conj acc (first p)) (second p))) [] ps)]
    `(let* [~@binds
            frame# (array-map ~@pairs)]
       (push-thread-bindings frame#)
       (try (do ~@body) (finally (pop-thread-bindings))))))

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

(defmacro defonce [name & body]
  ;; Must NOT reference clojure.core/resolve (a tree-shake bail ref).
  ;; Use jolt.host/find-var — a bare var-cell lookup with no alias resolution.
  ;; The ns/name strings are computed at expansion time.
  ;;
  ;; A leading docstring is accepted, so defonce takes the same
  ;; (sym doc-string? init) shape `def` does. clojure.core's defonce is [name expr]
  ;; only, which makes (defonce x "doc" 42) an arity error there rather than a
  ;; different meaning — so accepting it is a superset, not a divergence. ^meta on
  ;; the name rides along either way, since the expansion hands the symbol to `def`
  ;; untouched.
  (let [ns-str (str (clojure.core/ns-name clojure.core/*ns*))
        n-str (clojure.core/name name)
        n (clojure.core/count body)
        ;; Two forms with a string first is the docstring shape; one form is the
        ;; init — and a lone string IS the init, exactly as (def x "s") means.
        doc (clojure.core/when (clojure.core/and (clojure.core/= 2 n)
                                                 (clojure.core/string? (clojure.core/first body)))
              (clojure.core/first body))
        expr (clojure.core/if doc (clojure.core/second body) (clojure.core/first body))]
    (clojure.core/when-not (clojure.core/or (clojure.core/= 1 n) doc)
      (throw (new IllegalArgumentException
                  (str "defonce takes a name, an optional docstring and an init: "
                       "(defonce " n-str " doc-string? init)"))))
    `(if-let [v# (jolt.host/find-var ~ns-str ~n-str)]
       v#
       ~(clojure.core/if doc
          `(def ~name ~doc ~expr)
          `(def ~name ~expr)))))

;; Single arglist (Jolt defmacro is single-arity); the optional else defaults nil
;; via rest-destructuring.
(defmacro if-not [test then & [else]]
  `(if (not ~test) ~then ~else))

;; Macro-argument validation, the reference implementation verbatim: each
;; pred/message pair expands to a check that throws IllegalArgumentException
;; naming the calling macro (via &form) and the requirement. Private on the JVM,
;; but reachable there as @#'clojure.core/assert-args — the shape typedclojure
;; and other macro-heavy libraries use, and what this exists to serve.
(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

;; Conditional binding macros: the name is bound ONLY in the taken branch (the
;; auto-gensym temp# tests the value; the else/empty branch sees the surrounding
;; scope). temp# is a single template-local gensym — referenced twice, same symbol.
(defmacro if-let [bindings then & [else]]
  (when-not (= 2 (count bindings))
    (throw (new IllegalArgumentException "if-let requires exactly 2 forms in binding vector")))
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if temp# (let [~form temp#] ~then) ~else))))

;; when-let lives in 00-syntax (not here): 20-coll uses it, which loads before this tier.

(defmacro if-some [bindings then & [else]]
  (when-not (= 2 (count bindings))
    (throw (new IllegalArgumentException "if-some requires exactly 2 forms in binding vector")))
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (some? temp#) (let [~form temp#] ~then) ~else))))

(defmacro when-some [bindings & body]
  (when-not (= 2 (count bindings))
    (throw (new IllegalArgumentException "when-some requires exactly 2 forms in binding vector")))
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
  ;; *assert* is read at COMPILE time (JVM elides the check entirely when it is
  ;; false), so a false binding around the compile drops the assertion. The
  ;; message EXPRESSION evaluates at failure time, not at expansion — it may
  ;; embed runtime state.
  (when *assert*
    (if message
      `(when-not ~x
         (throw (new AssertionError (str "Assert failed: " ~message "\n" ~(pr-str x)))))
      `(when-not ~x (throw (new AssertionError ~(str "Assert failed: " (pr-str x))))))))

;; (pvalues e1 e2 ...) — each expression evaluated in parallel (pcalls).
(defmacro pvalues [& exprs]
  `(pcalls ~@(map (fn [e] `(fn [] ~e)) exprs)))

(defmacro delay [& body]
  `(make-delay (fn [] ~@body)))

(defmacro future [& body]
  `(future-call (fn [] ~@body)))

;; Build the fn* form via a template (a reader-list array): cons/list in a macro
;; body produce a plist the evaluator can't call as a form.
;; letfn itself is a macro in 00-syntax.clj expanding to the letfn* special form
;; (mutual recursion, which a plain (let* …) can't express); it lives there
;; because earlier tiers use it.

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
                   (= 0 cn) `(throw (new IllegalArgumentException (str "No matching clause: " ~ge)))
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
;; deftype/defrecord options: leading keyword/value pairs before the specs
;; (:load-ns, and any other the reference's parse-opts+specs consumes without
;; reading, such as gvec's :no-print). Skipped, as the reference skips them.
(defn- drop-type-opts [body]
  (loop [b (seq body)]
    (if (and b (keyword? (first b))) (recur (nnext b)) b)))

(defn- group-by-head [items]
  ;; nil is a valid extension head (extend-protocol P ... nil (m [x] ...)).
  (reduce (fn [acc x]
            (if (or (symbol? x) (nil? x))
              (conj acc [x])
              (conj (pop acc) (conj (peek acc) x))))
          [] items))

;; clojure.core/maybe-destructured, for deftype/defrecord method params: a param
;; that is not a symbol becomes a gensym, destructured in a let the caller wraps
;; the body in. Returns [param-vector let-bindings]. core.logic's Substitutions
;; has (cons [this [k v]] …), and the JVM accepts it — deftype's own params are
;; not the fn macro's, so each definer desugars them itself. (reify and
;; extend-type reach fn directly and get this for free.)
(defn- destructured-params [argv]
  (loop [ps (seq argv) nps [] lets []]
    (if ps
      (if (symbol? (first ps))
        (recur (next ps) (conj nps (first ps)) lets)
        (let [g (fresh-sym)]
          (recur (next ps) (conj nps g) (conj (conj lets (first ps)) g))))
      [nps lets])))

;; deftype/defrecord rename each _ param to a fresh symbol so two of them don't
;; collide on the field binds, but _ is still a binding the body can read: on the
;; JVM it names the last _ param, as (fn [_ _] _) does. PARAMS is the method's
;; params as written, RENAMED the same vector after renaming.
(declare keep)
(defn- underscore-rebind [params renamed body]
  (let [g (last (keep (fn [[p r]] (when (= p (quote _)) r)) (map vector params renamed)))]
    (if g (list (list* 'clojure.core/let [(quote _) g] body)) body)))

;; Every symbol a destructuring pattern binds (a superset: an :or default's own
;; symbols come along, which only makes the mutable-field live-read rewrite skip
;; a name it would have rewritten). Used to shadow those names against the fields.
(defn- pattern-syms [x]
  (cond
    (symbol? x) (if (= (name x) "&") [] [x])
    (vector? x) (vec (mapcat pattern-syms x))
    (map? x) (vec (mapcat pattern-syms (vals x)))
    :else []))

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
            ;; (. obj member args*) / (. obj (method args*)): the member position
            ;; names a field or method, it is not a value — rewrite the object and
            ;; argument positions only. Without this a member named like a mutable
            ;; field ((. this v)) had its member symbol rewritten into a field
            ;; read, producing (. this (.-v this)).
            (and (seq? form) (seq form) (symbol? (first form)) (= "." (name (first form)))
                 (>= (count form) 3) (symbol? (nth form 2)))
            (concat (list (first form) (rw inst shadowed (second form)) (nth form 2))
                    (map (fn [x] (rw inst shadowed x)) (drop 3 form)))
            (and (seq? form) (seq form) (symbol? (first form)) (= "." (name (first form)))
                 (= 3 (count form)) (seq? (nth form 2)) (symbol? (first (nth form 2))))
            (list (first form) (rw inst shadowed (second form))
                  (cons (first (nth form 2))
                        (map (fn [x] (rw inst shadowed x)) (rest (nth form 2)))))
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
                    (let [raw (mapv (fn [p] (if (= p (quote _)) (gensym "_p") p)) (nth spec 1))
                          dp (destructured-params raw)
                          argv (nth dp 0)
                          dlets (nth dp 1)
                          inst (first argv)
                          ;; A method param shadows a same-named field (Clojure
                          ;; semantics): don't let-bind a field the param already
                          ;; provides, and treat those params as shadowing so a
                          ;; mutable field's live-read rewrite doesn't override them.
                          ;; A destructured param's own bindings shadow too — hence
                          ;; the destructuring let sits INSIDE the field binds.
                          dsyms (vec (mapcat pattern-syms dlets))
                          shadowed (set (concat argv dsyms))
                          pnames (set (map name shadowed))
                          ;; let-bind only immutable fields; mutable ones are read live
                          ;; via rewrite-body so a set! within the method is observed.
                          ;; The read is the DECLARED-SLOT one, not get: a type
                          ;; declaring clojure.lang.ILookup answers get through its
                          ;; own valAt, so binding fields with get would re-enter
                          ;; that valAt on every method entry — including valAt's
                          ;; own — and never come back. It is also the cheaper read
                          ;; (straight to the slot, no type cascade).
                          binds (vec (mapcat (fn [f] [f (list (symbol "clojure.core" "__deftype-field")
                                                              inst (keyword (name f)))])
                                             (filter (fn [f] (and (not (mutable? f))
                                                                  (not (contains? pnames (name f)))))
                                                     fields)))
                          mbody (map (fn [bf] (rewrite-body inst shadowed bf)) (drop 2 spec))
                          mbody (if (seq dlets) (list (list* 'clojure.core/let dlets mbody)) mbody)
                          mbody (underscore-rebind (nth spec 1) raw mbody)]
                      (list argv (list* 'clojure.core/let binds mbody))))
        groups (group-by-head (drop-type-opts body))
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
                   (let [proto (protocol-key (first g))
                         names (distinct (map (fn [spec] (name (first spec))) (rest g)))]
                     (cons `(register-inline-protocol! ~(name tname) ~proto)
                           (map (fn [nm]
                                  `(register-inline-method ~(name tname) ~proto ~nm
                                                           (fn ~@(get by-name nm))))
                                names))))
                 groups)
       ~tname)))

;; The protocol value is built by make-protocol (a fn call) rather than an embedded
;; tagged map literal: a map carrying :jolt/type is not a map FORM (host-contract.ss
;; hc-map?), so the analyzer would read such a literal as a live value it cannot
;; spell instead of evaluating its fields. methods is a {kw {:name str}} map (only
;; :name is consulted). The rest of the value is Clojure's protocol map, which tooling
;; reads to describe a protocol it was handed:
;;
;;   :sigs       {kw {:name sym :arglists (..) :doc str-or-nil :tag hint-or-nil}}
;;               — the public description of the methods. (The JVM resolves :tag
;;               to a Class; here it is the hint symbol as written.)
;;   :doc        the protocol's own docstring, and ONLY WHEN IT HAS ONE — the JVM
;;               leaves the key absent otherwise, so (contains? P :doc) is false
;;               for an undocumented protocol. Also goes on the var, so (doc P)
;;               prints it.
;;   :method-map {kw kw} over the method names, identity on both halves (the JVM
;;               does not munge either side: :foo-bar maps to :foo-bar).
;;   :var        the protocol's own var, assoc'd AFTER the def below — it does not
;;               exist while make-protocol runs. SCI reads this one (sci.core,
;;               sci.impl.deftype).
;;
;; :on / :on-interface / :method-builders are the JVM's generated-interface keys
;; and are deliberately absent here: jolt generates no interface, dispatching on
;; the receiver's type tag instead. :extend-via-metadata is absent for a
;; different reason — jolt PARSES the option (below) but its dispatch does not
;; honor metadata extension, so recording the key would advertise a rule that
;; does not hold. See make-protocol's comment, unit.edn's defprotocol-value
;; suite, and jolt-2j3.
;;
;; Each method is a thin dispatch fn over protocol-dispatch, def'd with the var
;; metadata the reference gives it — :arglists, :doc, :protocol (the protocol's
;; own var) and the name's ^hint as :tag — so (doc a-method) prints a signature
;; and tooling can tell a protocol method from a plain fn. See the method
;; emission below.
(defmacro defprotocol [pname & sigs]
  ;; Clojure's defprotocol takes an optional docstring and leading keyword
  ;; options (:extend-via-metadata true, honeysql uses it) before the method
  ;; signatures. The docstring is kept (it is :doc on the value and on the var);
  ;; the options are read past. A string anywhere in the leading run is the
  ;; docstring and the last one wins, which is what the JVM's assoc-in-a-loop
  ;; parse does.
  (let [parsed (loop [d nil s sigs]
                 (cond
                   (string? (first s))  (recur (first s) (rest s))
                   (keyword? (first s)) (recur d (rest (rest s)))
                   :else [d s]))
        pdoc (first parsed)
        sigs (second parsed)
        methods (reduce (fn [m sig]
                          (assoc m (keyword (name (first sig))) {:name (name (first sig))}))
                        {} sigs)
        method-map (reduce (fn [m sig]
                             (let [k (keyword (name (first sig)))]
                               (assoc m k k)))
                           {} sigs)
        sigs-map (reduce (fn [m sig]
                           (let [mname (first sig)
                                 args (rest sig)
                                 doc (when (string? (last args)) (last args))]
                             (assoc m (keyword (name mname))
                                    {:tag (:tag (meta mname))
                                     :name (with-meta mname nil)
                                     :arglists (apply list (filter vector? args))
                                     :doc doc})))
                         {} sigs)
        ;; the protocol's identity: this namespace plus the name (see protocol-key).
        ;; Baked here so the value, the dispatch shims and every later impl
        ;; registration all key on one string.
        pkey (str *ns* "/" (name pname))]
    `(do
       (def ~(if pdoc (with-meta pname (assoc (meta pname) :doc pdoc)) pname)
         ;; :sigs is nil, not {}, for a protocol with no methods — the JVM's
         ;; (when sigs ..) leaves the key present with a nil value.
         (make-protocol ~pkey ~methods (quote ~(when (seq sigs) sigs-map)) ~pdoc ~method-map))
       ;; :var is the protocol's own var, so it can only be attached once the def
       ;; above has made one. alter-var-root, not a second def: a re-def would
       ;; drop the metadata just written.
       (alter-var-root (var ~pname) assoc :var (var ~pname))
       ;; register method var-keys for devirtualization; the inference
       ;; reads this (via infer-unit!) to resolve a protocol call on a known record
       (register-protocol-methods! ~pkey [~@(map (fn [s] (name (first s))) sigs)])
       ;; one fn clause per declared arity. The protocol/method NAMES pass as
       ;; strings so the body compiles as a plain invoke (not symbol-as-var). The
       ;; common 1/2/3-param arities call positional protocol-dispatchN, which
       ;; applies the impl directly — no rest-list cons; 4+ params fall back to the
       ;; variadic protocol-dispatch with a vector of the extra args.
       ~@(map (fn [sig]
                (let [pn pkey
                      mnm (first sig)
                      mn (name mnm)
                      arglists (filter vector? (rest sig))
                      mdoc (let [args (rest sig)] (when (string? (last args)) (last args)))
                      ;; the method var's metadata, as the reference sets it:
                      ;; :arglists and :doc (present with a nil value when the
                      ;; method has no docstring, as there), :tag, and :protocol
                      ;; — the protocol's own var, which is what tells tooling a
                      ;; method belongs to a protocol at all. Without these
                      ;; (doc a-method) printed a bare name and no arglists.
                      ;; The values are QUOTED forms: def evaluates the symbol's
                      ;; metadata map, and the arglists' `this` is a symbol, not a
                      ;; reference. :tag is NOT set here — it is the method name's
                      ;; own ^hint, which def already reads and resolves to a Class,
                      ;; the value both hosts put on (defn ^String f ..). The
                      ;; reference's defprotocol keeps a resolved SYMBOL instead;
                      ;; that one difference is a documented divergence.
                      mmeta (assoc (meta mnm)
                                   :doc mdoc
                                   :arglists (list 'quote (when (seq arglists)
                                                            (apply list arglists)))
                                   :protocol (list 'var pname))
                      clause (fn [argv]
                               (let [ps (mapv (fn [_] (fresh-sym)) argv)
                                     n (count ps)
                                     obj (first ps)]
                                 (cond
                                   (= n 1) (list ps (list 'clojure.core/protocol-dispatch1 pn mn obj))
                                   (= n 2) (list ps (list 'clojure.core/protocol-dispatch2 pn mn obj (nth ps 1)))
                                   (= n 3) (list ps (list 'clojure.core/protocol-dispatch3 pn mn obj (nth ps 1) (nth ps 2)))
                                   :else   (list ps (list 'clojure.core/protocol-dispatch pn mn obj (vec (rest ps)))))))]
                  (if (seq arglists)
                    `(def ~(with-meta mnm mmeta) (fn* ~@(map clause arglists)))
                    `(def ~(with-meta mnm mmeta)
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
                       (recur (rest items) (protocol-key x) forms)
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
;; (proxy [ThreadLocal] [] (initialValue [] body)) is a per-thread store with a
;; lazy initial value (test.check's no-seed PRNG uses one). Every other proxy goes
;; to make-proxy, which EXTENDS BY DELEGATION when the first super names a class
;; it can construct: it builds a real base instance from ctor-args, answers the
;; methods declared here, and forwards the rest to that instance, so an inherited
;; concrete method and proxy-super both work (java/proxy.ss). A super naming an
;; interface has nothing to construct, and the proxy is then exactly a reify of
;; that interface's methods, which is what it has always been. A proxy method body
;; takes the declared args with `this` implicit; reify takes `this` first, so
;; prepend it.
;; One reify spec per arity of a proxy method spec. proxy accepts BOTH shapes:
;;   (name [params*] body*)                          — one arity
;;   (name ([params*] body*) ([params*] body*) ...)  — several
;; Only the first was handled, so the multi-arity form fed reify a spec whose
;; "arglist" was the whole ([params*] body*) list and destructuring died on the
;; body expression ("unsupported destructuring pattern: (.read src buf off ...)").
;; reify wants one (name [params*] body*) per arity and groups same-named specs
;; into a multi-arity fn, so both shapes flatten to the same list here. `this` is
;; implicit in a proxy body and explicit in a reify one, hence the cons.
;; A proxy's supers are classes, named as the namespace names them: a simple
;; name it imported is that import's class, which is what the class graph knows
;; it by (java.util.AbstractList, not AbstractList), so what the class inherits
;; is found. A protocol keeps its key; a name nothing resolves stays as written.
(defn- proxy-super-key [sym]
  (or (jolt.host/protocol-key-of sym)
      (let [c (when (and (symbol? sym) (nil? (namespace sym))) (resolve sym))]
        (if (class? c) (.getName c) (name sym)))))
(defn- proxy-arity-specs [m]
  (let [nm (first m)
        arities (if (vector? (second m)) [(rest m)] (rest m))]
    (map (fn [a] (list* nm (vec (cons 'this (first a))) (rest a))) arities)))

(defmacro proxy [supers ctor-args & methods]
  (if (and (vector? supers) (= 1 (count supers))
           (let [s (name (first supers))] (or (= s "ThreadLocal") (= s "InheritableThreadLocal"))))
    ;; WHICH of the two is load-bearing: they differ only in what a forked thread
    ;; sees, and that is the entire difference between the classes. Lowering both
    ;; to one object made whichever storage was chosen wrong for the other.
    ;;
    ;; initialValue is the only override this lowering can honour — the object it
    ;; builds is a host storage shim, not a subclass, so a get/set/remove/
    ;; toString/childValue body has nowhere to go. It used to be dropped in
    ;; silence, which is a method that looks defined and never runs; say so at
    ;; the call site instead.
    (let [extra (remove (fn [m] (= "initialValue" (name (first m)))) methods)
          init  (some (fn [m] (when (= "initialValue" (name (first m))) m)) methods)]
      (when (seq extra)
        (throw (ex-info (str "proxy over " (name (first supers))
                             " can only override initialValue, not "
                             (apply str (interpose ", " (map (fn [m] (name (first m))) extra))))
                        {:class (name (first supers))
                         :unsupported (mapv (fn [m] (name (first m))) extra)})))
      `(jolt.host/make-thread-local (fn [] ~@(when init (nnext init)))
                                    ~(= (name (first supers)) "InheritableThreadLocal")))
    ;; group the flattened specs by method name, so several arities of one method
    ;; become one multi-arity fn — the same shape reify builds.
    (loop [specs (seq (apply concat (map proxy-arity-specs methods)))
           clauses {} order []]
      (if (empty? specs)
        ;; the defining ns names the generated class (proxy-name), as on the JVM
        `(jolt.host/make-proxy-in
           ~(str *ns*)
           ~(reduce (fn [m k] (assoc m k `(fn ~@(get clauses k)))) {} order)
           [~@ctor-args]
           ~(vec (map proxy-super-key supers)))
        (let [spec (first specs)
              k (keyword (name (first spec)))]
          (recur (rest specs)
                 (assoc clauses k (conj (get clauses k []) `(~(nth spec 1) ~@(drop 2 spec))))
                 (if (contains? clauses k) order (conj order k))))))))
;; definterface is JVM-only; bind the name to a marker and return the name (not a
;; var), matching the JVM where definterface yields the interface Class.
(defmacro definterface [name-sym & body]
  `(do (def ~name-sym {}) (quote ~name-sym)))

;; make-reified-at is a fn (clojure.core). The site's SHAPE — its method names and
;; protocol keys, as a literal [[name …] [proto …]] of strings — is a constant, so
;; the compiled site binds it once and every instance hands the runtime the same
;; object, which is what lets it build the method layout once per site instead of
;; a table per instance. The method fns follow, in the order the names list.
(defmacro reify [& forms]
  ;; a reify can implement SEVERAL protocols; collect them all (each bare symbol
  ;; switches the current protocol, like extend-type) and pass every protocol name
  ;; to make-reified so (instance? Proto r)/satisfies? recognise all of them.
  ;; Several bodies for the same method name are distinct arities (clojure.spec
  ;; reifies (specize* [s]) and (specize* [s _])): group them into one multi-arity
  ;; fn so dispatch picks the clause by arg count.
  (loop [items (seq forms) protos [] methods {} order []]
    (if (empty? items)
      ;; the third element names the class: ns$reify__N, one per reify form, as
      ;; the JVM generates one class per form (its name also carries the
      ;; enclosing fn and an eval counter, which jolt cannot reproduce). N comes
      ;; from where the form sits (file, line, column), so a form compiled into
      ;; the AOT cache and one expanded fresh in a later process can never share
      ;; a name; a form with no position takes a gensym, below the position range
      `(make-reified-at
         [[~@(map name order)] [~@(map protocol-key protos)]
          ~(str (apply str (map (fn [c] (if (= c \-) \_ c)) (str *ns*))) "$"
                (let [{:keys [line column]} (meta &form)]
                  (if line
                    (str "reify__" (+ 1000000000 (mod (hash [*file* line column]) 1000000000)))
                    (gensym "reify__"))))]
         ~@(map (fn [k] `(fn ~@(get methods k))) order))
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
        ;; inference reads its fields bare-index. Unlike a hint a user writes, this
        ;; one is a proof -- dispatch only reaches the method with an instance of
        ;; the record -- and :jolt/proven says so (analyzer parse-params).
        ;; Clause as DATA (see deftype).
        mk-clause (fn [spec]
                    ;; rename each _ parameter to a fresh symbol so two _ params
                    ;; (the common (m [_ _] …) on a 1-arg protocol method) don't
                    ;; collide — the field binds read (get this :field) off the
                    ;; FIRST param, which an ignored second _ would otherwise shadow.
                    (let [raw (mapv (fn [p] (if (= p (quote _)) (gensym "_p") p)) (nth spec 1))
                          dp (destructured-params raw)
                          argv (nth dp 0)
                          dlets (nth dp 1)
                          inst (first argv)
                          hinted (assoc argv 0 (vary-meta inst assoc :tag (name name-sym) :jolt/proven true))
                          ;; a method param shadows a same-named field (Clojure
                          ;; semantics), so don't rebind a field the param provides —
                          ;; including a name a destructured param binds, whose let
                          ;; sits inside the field binds.
                          pnames (set (map name (concat argv (mapcat pattern-syms dlets))))
                          binds (vec (mapcat (fn [f] [f `(get ~inst ~(keyword (name f)))])
                                             (remove (fn [f] (contains? pnames (name f))) fields)))
                          mbody (drop 2 spec)
                          mbody (if (seq dlets) (list (list* 'clojure.core/let dlets mbody)) mbody)
                          mbody (underscore-rebind (nth spec 1) raw mbody)]
                      (list hinted (list* 'clojure.core/let binds mbody))))
        groups (group-by-head (drop-type-opts body))
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
                   (let [proto (protocol-key (first g))
                         names (distinct (map (fn [spec] (name (first spec))) (rest g)))]
                     (cons `(register-inline-protocol! ~(name name-sym) ~proto)
                           (map (fn [nm]
                                  `(register-inline-method ~(name name-sym) ~proto ~nm
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

;; --- eduction ---------------------------------------------------------------
;; Here rather than with transduce (22-coll) because Eduction is a deftype.
;; Eduction — a REDUCIBLE and seqable view applying xform to coll, matching
;; clojure.core.Eduction. Reducing one drives the transducer straight into the
;; accumulator (the IReduceInit path, which jolt-reduce honors via iface-method)
;; with no seq cells — which is why (reduce f init (eduction …)) beats the
;; equivalent transduce; seq realizes lazily through `sequence`, so an infinite
;; or expensive source is not forced.
;;
;; It was previously just (sequence xform coll) — a plain lazy seq — so reduce
;; allocated a cell per element and walked it: 152ms where transduce over the
;; same xform and source took 58ms, against the JVM's 9.2ms.
;;
;; Interfaces follow the JVM: Sequential (so sequential? is true) but NOT
;; IPersistentCollection (coll? false) and NOT ISeq (seq? false), and count
;; throws — the value is reducible and seqable, not counted.
(deftype Eduction [xform coll]
  clojure.lang.IReduceInit
  (reduce [_ f init] (transduce xform (completing f) init coll))
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [_] (seq (sequence xform coll)))
  (count [_] (throw (UnsupportedOperationException.
                     "count not supported on this type: Eduction"))))

(defn eduction [& args]
  (->Eduction (apply comp (butlast args)) (last args)))
