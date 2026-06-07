# Jolt Compiler
# Source-to-source: Clojure forms → Janet source
# Two-phase: analyze-form (classify) → emit-ast (generate)
#
# When ctx is passed to analyze-form, macros are expanded at analyze time.

(use ./types)
(use ./core)
(use ./phm)

# The compiler emits Janet that references the core fns (core-+, core-<, …),
# which are bare-bound in this module's environment via (use ./core). Capture it
# so each Jolt context can get a child env where those resolve and where compiled
# `def`/`defn` bindings persist across forms (isolated per context).
(def jolt-runtime-env (curenv))

(defn ctx-janet-env
  "Lazily create/cache a per-context Janet environment for compiled code: a child
  of the runtime env (so core fns resolve) that holds this context's user defs.
  For a nil context (one-off compile/eval) returns a fresh child env."
  [ctx]
  (if (and ctx (table? (get ctx :env)))
    (or (get (ctx :env) :janet-rt)
        (let [e (make-env jolt-runtime-env)]
          (put (ctx :env) :janet-rt e)
          e))
    (make-env jolt-runtime-env)))

(def- core-renames
  # Compile mode emits NATIVE Janet ops for the hot numeric primitives (+,-,*
  # and the comparisons), which match Jolt's semantics for numbers and are
  # ~10-20x faster than the variadic core fns. Trade-off: the strict non-number
  # checks (e.g. (< nil 1) throwing) are relaxed under compilation — a
  # documented perf-mode divergence. = / not= / quot / rem / mod / division stay
  # as core fns (their semantics differ from Janet's).
  @{"+" "+"
    "-" "-"
    "*" "*"
    "/" "core-/"
    "inc" "core-inc"
    "dec" "core-dec"
    "=" "core-="
    "not=" "core-not="
    "<" "<"
    ">" ">"
    "<=" "<="
    ">=" ">="
    "nil?" "core-nil?"
    "not" "core-not"
    "some?" "core-some?"
    "string?" "core-string?"
    "number?" "core-number?"
    "fn?" "core-fn?"
    "keyword?" "core-keyword?"
    "symbol?" "core-symbol?"
    "vector?" "core-vector?"
    "map?" "core-map?"
    "seq?" "core-seq?"
    "coll?" "core-coll?"
    "first" "core-first"
    "rest" "core-rest"
    "next" "core-next"
    "cons" "core-cons"
    "conj" "core-conj"
    "assoc" "core-assoc"
    "dissoc" "core-dissoc"
    "get" "core-get"
    "get-in" "core-get-in"
    "assoc-in" "core-assoc-in"
    "update-in" "core-update-in"
    "fnil" "core-fnil"
    "contains?" "core-contains?"
    "count" "core-count"
    "empty?" "core-empty?"
    "every?" "core-every?"
    "seq" "core-seq"
    "vec" "core-vec"
    "map" "core-map"
    "filter" "core-filter"
    "remove" "core-remove"
    "reduce" "core-reduce"
    "apply" "core-apply"
    "str" "core-str"
    "prn" "core-prn"
    "pr-str" "core-pr-str"
    "println" "core-println"
    "print" "core-print"
    "identity" "core-identity"
    "comp" "core-comp"
    "partial" "core-partial"
    "complement" "core-complement"
    "constantly" "core-constantly"
    "memoize" "core-memoize"
    "range" "core-range"
    "take" "core-take"
    "drop" "core-drop"
    "take-while" "core-take-while"
    "drop-while" "core-drop-while"
    "nth" "core-nth"
    "list" "core-list"
    "name" "core-name"
    "subs" "core-subs"
    "reverse" "core-reverse"
    "into" "core-into"
    "merge" "core-merge"
    "merge-with" "core-merge-with"
    "keys" "core-keys"
    "vals" "core-vals"
    "zipmap" "core-zipmap"
    "select-keys" "core-select-keys"
    "max" "core-max"
    "min" "core-min"
    "odd?" "core-odd?"
    "even?" "core-even?"
    "zero?" "core-zero?"
    "pos?" "core-pos?"
    "neg?" "core-neg?"
    "true?" "core-true?"
    "false?" "core-false?"
    "identical?" "core-identical?"
    "quot" "core-quot"
    "rem" "core-rem"
    "mod" "core-mod"})

(defn- literal?
  [form]
  (or (nil? form) (= true form) (= false form)
      (number? form) (string? form) (keyword? form) (bytes? form) (buffer? form)))

(defn- special-form?
  [name]
  (or (= name "quote") (= name "syntax-quote") (= name "do")
      (= name "if") (= name "def") (= name "defmacro") (= name "fn*")
      (= name "let*") (= name "loop*") (= name "recur") (= name "throw")
      (= name "try") (= name "set!") (= name "var") (= name ".")
      (= name "eval")
      (= name "new") (= name "deftype") (= name "instance?")
      (= name "defmulti") (= name "defmethod") (= name "locking")
      (= name "prefer-method") (= name "remove-method") (= name "remove-all-methods")))

# Forms the compiler can't compile correctly: definitional/stateful special
# forms and macros that mutate the context or build runtime values the emitter
# doesn't model (types, protocols, multimethods, dynamic binding, host interop).
# analyze-form throws uncompilable on these so the enclosing top-level form falls
# back to the interpreter — which handles them — instead of silently miscompiling.
# (Top-level occurrences are usually routed straight to the interpreter by
# loader/stateful-head?; this also covers them nested inside compiled forms.)
(def- uncompilable-heads
  (let [t @{}]
    # Interpreter special forms the compiler does NOT itself implement (it
    # handles quote/do/if/def/fn*/let*/loop*/recur/throw/try). Kept in sync with
    # eval-form's special-form match in evaluator.janet.
    (each n ["syntax-quote" "unquote" "unquote-splicing" "eval" "read-string"
             "macroexpand-1" "defonce" "defmacro" "deftype" "defmulti"
             "defmethod" "prefer-method" "remove-method" "remove-all-methods"
             "get-method" "methods" "register-method" "protocol-dispatch"
             "make-reified" "satisfies?" "instance?" "set!" "var" "var-get"
             "var-set" "var?" "in-ns" "ns" "require" "create-ns" "remove-ns"
             "find-ns" "all-ns" "the-ns" "find-var" "intern" "resolve"
             "ns-resolve" "ns-aliases" "ns-imports" "ns-interns"
             "alter-var-root" "alter-meta!" "reset-meta!" "locking" "new"
             "disj" "set?"
             # Definitional/host macros that mutate context or build runtime
             # values the emitter doesn't model.
             "defrecord" "defprotocol" "definterface" "reify" "proxy"
             "extend-type" "extend-protocol" "extend" "gen-class" "import"
             "use" "refer" "monitor-enter" "monitor-exit" "binding" "."
             # letfn needs all its fns in scope simultaneously (mutual
             # recursion); the sequential let* the compiler would build can't
             # express that, so interpret it.
             "letfn"]
      (put t n true))
    t))

(defn- uncompilable-head? [name] (get uncompilable-heads name))

# ============================================================
# Macro resolution
# ============================================================

(defn- resolve-macro
  [ctx sym-s]
  (when ctx
    (let [name (sym-s :name)
          ns-sym (sym-s :ns)]
      (if ns-sym
        # Resolve :as aliases (e.g. (t/is …) where t aliases clojure.test) so
        # aliased macros are recognized as macros — matching the interpreter's
        # resolve-var — rather than miscompiled as a value ref to the macro var.
        (let [cur (ctx-find-ns ctx (ctx-current-ns ctx))
              aliased (ns-import-lookup cur ns-sym)
              target-ns (ctx-find-ns ctx (or aliased ns-sym))
              v (ns-find target-ns name)]
          (if (and v (var-macro? v)) v))
        (let [current-ns-name (ctx-current-ns ctx)
              current-ns (ctx-find-ns ctx current-ns-name)
              v (ns-find current-ns name)]
          (if v
            (if (var-macro? v) v)
            (let [core-ns (ctx-find-ns ctx "clojure.core")
                  cv (ns-find core-ns name)]
              (if (and cv (var-macro? cv)) cv))))))))

# Loop counter for generating unique loop function names
(var loop-counter 0)

(defn- make-loop-name
  []
  (let [name (string "_loop_" loop-counter)]
    (++ loop-counter)
    name))

(defn- make-gensym
  "A fresh, collision-proof Janet symbol name for compiler-introduced bindings
  (recur targets, arity-dispatch arg vectors). The leading `_jolt$` can't appear
  in a Clojure source symbol, so these never shadow user names."
  [prefix]
  (let [name (string "_jolt$" prefix "_" loop-counter)]
    (++ loop-counter)
    name))

# ============================================================
# Syntax-quote expansion
# ============================================================

(defn- sq-list?
  "Check if form is a (unquote ...) or (unquote-splicing ...) call."
  [form]
  (and (array? form) (> (length form) 0)
       (struct? (first form)) (= :symbol ((first form) :jolt/type))
       (or (= "unquote" ((first form) :name))
           (= "unquote-splicing" ((first form) :name)))))

(defn- sq-has-unquote?
  "Check if any item in a collection is an unquote/unquote-splicing form."
  [items]
  (var found false) (var i 0)
  (while (and (< i (length items)) (not found))
    (if (sq-list? (in items i)) (set found true)) (++ i))
  found)

(defn- sq-resolve-sym
  "Qualify an unqualified symbol with the current namespace."
  [sym-s ctx]
  (if (and ctx (nil? (sym-s :ns)))
    {:jolt/type :symbol :ns (ctx-current-ns ctx) :name (sym-s :name)}
    sym-s))

(defn- syntax-quote-expand
  "Expand a syntax-quoted form into a plain Clojure form.
  Simple forms are wrapped in (quote ...). Forms with unquote produce
  (concat (list ...) ...) calls."
  [form ctx]
  (cond
    # unquote → just the inner expression
    (and (array? form) (> (length form) 0)
         (struct? (first form)) (= :symbol ((first form) :jolt/type))
         (= "unquote" ((first form) :name)))
    (in form 1)

    # unquote-splicing → just the inner expression
    (and (array? form) (> (length form) 0)
         (struct? (first form)) (= :symbol ((first form) :jolt/type))
         (= "unquote-splicing" ((first form) :name)))
    (in form 1)

    # Literals → (quote literal)
    (or (nil? form) (= true form) (= false form)
        (number? form) (string? form) (keyword? form))
    [{:jolt/type :symbol :ns nil :name "quote"} form]

    # Symbols → (quote resolved-symbol)
    (and (struct? form) (= :symbol (form :jolt/type)))
    [{:jolt/type :symbol :ns nil :name "quote"} (sq-resolve-sym form ctx)]

    # Lists/arrays with unquote → (concat (list ...) (list ...) ...)
    (array? form)
    (if (sq-has-unquote? form)
      (let [items (map |(syntax-quote-expand $ ctx) form)
            concat-args @[]]
        (each item items
          (array/push concat-args
            [{:jolt/type :symbol :ns nil :name "list"} item]))
        (if (> (length concat-args) 1)
          (tuple ;(array/insert concat-args 0
            {:jolt/type :symbol :ns nil :name "concat"}))
          (in concat-args 0)))
      [{:jolt/type :symbol :ns nil :name "quote"} form])

    # Vectors → (vec (concat (list ...) ...))
    (tuple? form)
    (if (sq-has-unquote? form)
      (let [items (map |(syntax-quote-expand $ ctx) form)
            concat-args @[]]
        (each item items
          (array/push concat-args
            [{:jolt/type :symbol :ns nil :name "list"} item]))
        [{:jolt/type :symbol :ns nil :name "vec"}
         (tuple ;(array/insert concat-args 0
           {:jolt/type :symbol :ns nil :name "concat"}))])
      [{:jolt/type :symbol :ns nil :name "quote"} form])

    # Default → (quote form)
    [{:jolt/type :symbol :ns nil :name "quote"} form]))

# ============================================================
# Analyzer
# ============================================================

(defn- plain-symbol?
  "A bare Clojure symbol (not a destructuring pattern). `&` counts — it's the
  varargs marker, which the emitter passes straight through to Janet."
  [x]
  (and (struct? x) (= :symbol (x :jolt/type))))

(defn- uncompilable
  "Signal that the compiler can't (yet) handle this form. eval-one catches this
  and falls back to the interpreter, which handles every form correctly. Throwing
  here — rather than miscompiling — is what makes the hybrid path sound."
  [reason]
  (error (string "jolt/uncompilable: " reason)))

# fn* analysis is large enough (optional self-name, multi-arity, varargs, recur
# targets) to live in its own helper. Forward-declared so the fn* case in
# analyze-form can call it; defined after analyze-form (which it recurses into).
(var analyze-fn nil)

(defn analyze-form
  "Analyze a Clojure form and return an AST node with :op key.
  Takes bindings (table) and optional ctx (for macro expansion)."
  [form bindings &opt ctx]
  (default ctx nil)
  (cond
    (literal? form)
    {:op :const :val form}

    (and (struct? form) (= :symbol (form :jolt/type)))
    (let [name (form :name)
          ns (form :ns)]
      (if ns
        {:op :qualified-symbol :ns ns :name name}
        (if (get bindings name)
          {:op :local :name name}
          (if (and (not (special-form? name)) (get core-renames name))
            {:op :core-symbol :name name :janet-name (get core-renames name)}
            # A global reference. Resolution mirrors the interpreter's resolve-sym
            # so compiled and interpreted code agree:
            #   1. a jolt var in the current ns (which also holds refers) or
            #      clojure.core -> deref through the cell, so redefinition is
            #      visible to compiled callers (Janet early-binds plain symbols);
            #   2. otherwise a binding in the runtime/Janet env (resolve-sym's own
            #      fallback — this is how int?, type, etc. resolve) -> emit it
            #      directly;
            #   3. otherwise a forward reference -> intern a pending cell whose
            #      getter derefs at call time, once a later def fills it in.
            # No ctx -> plain symbol.
            (if ctx
              (let [cur-ns (ctx-find-ns ctx (ctx-current-ns ctx))
                    cell (or (ns-find cur-ns name)
                             (ns-find (ctx-find-ns ctx "clojure.core") name))]
                (cond
                  cell {:op :var :name name :var cell}
                  (get jolt-runtime-env (symbol name))
                    {:op :core-symbol :name name :janet-name name}
                  {:op :var :name name :var (ns-intern cur-ns name)}))
              {:op :symbol :name name})))))

    (array? form)
    (let [first-form (first form)
          head-name (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
                     (first-form :name)
                     nil)]
      (when (and head-name (uncompilable-head? head-name))
        (uncompilable head-name))
      # Macro expansion
      (if (and ctx head-name
               (not (special-form? head-name))
               (resolve-macro ctx first-form))
        (let [macro-var (resolve-macro ctx first-form)
              macro-fn (var-get macro-var)
              expanded (apply macro-fn (tuple/slice form 1))]
          (analyze-form expanded bindings ctx))
        (if head-name
          (match head-name
            "quote" {:op :quote :expr (in form 1)}
            "throw" {:op :throw :val (analyze-form (in form 1) bindings ctx)}
            "try" (let [body-form (in form 1)
                        clauses (tuple/slice form 2)
                        n (length clauses)]
                    (var catch-sym nil)
                    (var catch-body nil)
                    (var finally-body nil)
                    (var i 0)
                    (while (< i n)
                      (let [clause (in clauses i)
                            head (first clause)]
                        (if (and (struct? head) (= :symbol (head :jolt/type)))
                          (match (head :name)
                            "catch" (do
                              (set catch-sym (in clause 2))
                              (set catch-body (tuple/slice clause 3)))
                            "finally" (set finally-body (tuple/slice clause 1)))))
                      (++ i))
                    (let [catch-bindings (if catch-sym
                                          (do
                                            (var cb @{})
                                            (loop [[k v] :pairs bindings] (put cb k v))
                                            (put cb (catch-sym :name) :jolt/local)
                                            cb)
                                          nil)]
                      {:op :try
                       :body (analyze-form body-form bindings ctx)
                       :catch-sym (if catch-sym (catch-sym :name))
                       :catch-body (if catch-body
                                    (map |(analyze-form $ catch-bindings ctx) catch-body))
                       :finally-body (if finally-body
                                      (map |(analyze-form $ bindings ctx) finally-body))}))
            "recur" (let [args (map |(analyze-form $ bindings ctx) (tuple/slice form 1))
                          loop-name (get bindings :jolt/current-loop)]
                      {:op :recur :args args :loop-name loop-name})
            "do" (let [all-statements (array/slice form 1)
                       n (length all-statements)
                       analyzed (map |(analyze-form $ bindings ctx) all-statements)]
                   (if (= n 0)
                     {:op :const :val nil}   # (do) -> nil
                     {:op :do
                      :statements (array/slice analyzed 0 (- n 1))
                      :ret (in analyzed (- n 1))}))
            "if" {:op :if
                  :test (analyze-form (in form 1) bindings ctx)
                  :then (analyze-form (in form 2) bindings ctx)
                  :else (if (> (length form) 3)
                         (analyze-form (in form 3) bindings ctx)
                         {:op :const :val nil})}
            "def" (let [name-sym (in form 1)
                        nm (if (struct? name-sym) (name-sym :name) (string name-sym))
                        # Create/find the var cell first so a recursive init body
                        # self-references the same cell.
                        cell (when ctx (ns-intern (ctx-find-ns ctx (ctx-current-ns ctx)) nm))
                        # (def x) with no init (declare) -> nil.
                        init-form (if (> (length form) 2) (in form 2) nil)]
                    {:op :def :name name-sym :var cell
                     :init (analyze-form init-form bindings ctx)})
            "fn*" (analyze-fn form bindings ctx)
            "let*" (let [bind-vec (in form 1)
                         body-exprs (tuple/slice form 2)
                         # Accumulate scope as we go so a later binding's init can
                         # reference an earlier binding (sequential let scoping).
                         acc (do (var bb @{}) (loop [[k v] :pairs bindings] (put bb k v)) bb)
                         binding-pairs (do
                                         (var pairs @[])
                                         (var i 0)
                                         (let [n (length bind-vec)]
                                           (while (< i n)
                                             (let [sym-s (in bind-vec i)
                                                   _ (unless (plain-symbol? sym-s)
                                                       (uncompilable "destructuring let binding"))
                                                   name (sym-s :name)
                                                   val-form (if (< (+ i 1) n) (in bind-vec (+ i 1)) nil)
                                                   val-ast (if val-form (analyze-form val-form acc ctx) {:op :const :val nil})]
                                               (array/push pairs {:name name :init val-ast})
                                               (put acc name :jolt/local)
                                               (+= i 2))))
                                         pairs)
                         body-bindings acc
                         analyzed-body (map |(analyze-form $ body-bindings ctx) body-exprs)
                         n-body (length analyzed-body)]
                     {:op :let
                      :binding-pairs binding-pairs
                      :body (if (> n-body 1)
                              {:op :do
                               :statements (array/slice analyzed-body 0 (- n-body 1))
                               :ret (last analyzed-body)}
                              (first analyzed-body))})
            "loop*" (let [bind-vec (in form 1)
                          loop-name (make-loop-name)
                          acc (do (var bb @{}) (loop [[k v] :pairs bindings] (put bb k v)) bb)
                          binding-pairs (do
                                          (var pairs @[])
                                          (var i 0)
                                          (let [n (length bind-vec)]
                                            (while (< i n)
                                              (let [sym-s (in bind-vec i)
                                                    _ (unless (plain-symbol? sym-s)
                                                        (uncompilable "destructuring loop binding"))
                                                    name (sym-s :name)
                                                    val-form (if (< (+ i 1) n) (in bind-vec (+ i 1)) nil)
                                                    val-ast (if val-form (analyze-form val-form acc ctx) {:op :const :val nil})]
                                                (array/push pairs {:name name :init val-ast})
                                                (put acc name :jolt/local)
                                                (+= i 2))))
                                          pairs)
                          param-names (map |($ :name) binding-pairs)
                          body-bindings (do
                                          (var bb @{})
                                          (loop [[k v] :pairs bindings] (put bb k v))
                                          (each bp binding-pairs
                                            (put bb (bp :name) :jolt/local))
                                          (put bb :jolt/current-loop loop-name)
                                          bb)
                          body-exprs (tuple/slice form 2)
                          analyzed-body (map |(analyze-form $ body-bindings ctx) body-exprs)
                          n-body (length analyzed-body)]
                      {:op :loop
                       :loop-name loop-name
                       :param-names param-names
                       :init-vals (map |($ :init) binding-pairs)
                       :body (if (> n-body 1)
                               {:op :do
                                :statements (array/slice analyzed-body 0 (- n-body 1))
                                :ret (last analyzed-body)}
                               (first analyzed-body))})
            (let [f-ast (analyze-form first-form bindings ctx)
                  args (map |(analyze-form $ bindings ctx) (tuple/slice form 1))]
              {:op :invoke :fn f-ast :args args}))
          (let [f-ast (analyze-form first-form bindings ctx)
                args (map |(analyze-form $ bindings ctx) (tuple/slice form 1))]
            {:op :invoke :fn f-ast :args args}))))

    (tuple? form)
    (let [items (map |(analyze-form $ bindings ctx) form)]
      {:op :vector :items items})

    (struct? form)
    (cond
      (= :jolt/set (form :jolt/type))
        {:op :set :items (map |(analyze-form $ bindings ctx) (form :value))}
      (= :jolt/char (form :jolt/type))
        {:op :const :val form}
      # Tagged literals (#"regex", data readers) need runtime construction the
      # compiler doesn't model — interpret them.
      (form :jolt/type)
        (uncompilable (string "tagged literal " (form :jolt/type)))
      # Plain map literal: keys and values are expressions to evaluate.
      {:op :map
       :pairs (map (fn [k] [(analyze-form k bindings ctx)
                            (analyze-form (get form k) bindings ctx)])
                   (keys form))})

    {:op :const :val form}))

(defn- parse-fn-params
  "Split a param vector into fixed param names and an optional rest name. Only
  plain symbols are handled here; destructuring params signal uncompilable so the
  whole fn falls back to the interpreter."
  [params]
  (unless (tuple? params) (uncompilable "fn params not a vector"))
  (def fixed @[])
  (var rest-name nil)
  (var i 0)
  (def n (length params))
  (while (< i n)
    (def p (in params i))
    (unless (plain-symbol? p) (uncompilable "destructuring fn params"))
    (if (= "&" (p :name))
      (do
        (++ i)
        (when (< i n)
          (def r (in params i))
          (unless (plain-symbol? r) (uncompilable "destructuring fn rest param"))
          (set rest-name (r :name)))
        (++ i))
      (do (array/push fixed (p :name)) (++ i))))
  {:fixed (tuple/slice fixed) :rest rest-name})

(set analyze-fn
  (fn analyze-fn [form bindings ctx]
    # (fn* name? params-or-clauses...) where a clause is (params body...).
    (def named? (plain-symbol? (in form 1)))
    (def fn-name (when named? ((in form 1) :name)))
    (def idx (if named? 2 1))
    (def first-clause (in form idx))
    # Single arity: a param vector at idx. Multi arity: each remaining element is
    # an (params body...) list.
    (def raw-clauses
      (cond
        (tuple? first-clause) [[first-clause (tuple/slice form (+ idx 1))]]
        (array? first-clause) (map |[(in $ 0) (tuple/slice $ 1)] (tuple/slice form idx))
        (uncompilable "fn: unexpected param shape")))
    (def multi (> (length raw-clauses) 1))
    # Public name: the symbol the fn binds to itself. Single-arity fns recur
    # straight into this name; multi-arity fns recur into a per-arity inner fn so
    # recur stays in its own arity rather than re-dispatching.
    (def outer-name (or fn-name (make-gensym "fn")))
    (def arities
      (map
        (fn [clause]
          (def pinfo (parse-fn-params (in clause 0)))
          (def fixed (pinfo :fixed))
          (def rest-name (pinfo :rest))
          (def recur-name
            (if (and (not multi) (not rest-name)) outer-name (make-gensym "arity")))
          (def body-bindings
            (do
              (var bb @{})
              (loop [[k v] :pairs bindings] (put bb k v))
              (when fn-name (put bb fn-name :jolt/local))
              (each pn fixed (put bb pn :jolt/local))
              (when rest-name (put bb rest-name :jolt/local))
              (put bb :jolt/current-loop recur-name)
              bb))
          (def body-exprs (in clause 1))
          (def analyzed (map |(analyze-form $ body-bindings ctx) body-exprs))
          (def n-body (length analyzed))
          {:param-names fixed
           :rest-name rest-name
           :n-fixed (length fixed)
           :recur-name recur-name
           :body (cond
                   (= 0 n-body) {:op :const :val nil}
                   (= 1 n-body) (first analyzed)
                   {:op :do
                    :statements (array/slice analyzed 0 (- n-body 1))
                    :ret (last analyzed)})})
        raw-clauses))
    {:op :fn :name outer-name :fn-name fn-name :multi multi :arities arities}))

# ============================================================
# Emitter — AST → Janet source string
# ============================================================

(var emit-ast nil)

(defn- emit-const-str [val buf]
  (cond
    (nil? val) (buffer/push buf "nil")
    (= true val) (buffer/push buf "true")
    (= false val) (buffer/push buf "false")
    (string? val) (do (buffer/push buf "\"") (buffer/push buf val) (buffer/push buf "\""))
    (keyword? val) (do (buffer/push buf ":") (buffer/push buf (string val)))
    (buffer/push buf (string val))))

(defn- emit-do-str [statements ret buf]
  (buffer/push buf "(do ")
  (var i 0)
  (let [n (length statements)]
    (while (< i n)
      (emit-ast (in statements i) buf)
      (buffer/push buf " ")
      (++ i)))
  (when ret (emit-ast ret buf))
  (buffer/push buf ")"))

(defn- emit-if-str [test then else buf]
  (buffer/push buf "(if ")
  (emit-ast test buf) (buffer/push buf " ")
  (emit-ast then buf)
  (when else (buffer/push buf " ") (emit-ast else buf))
  (buffer/push buf ")"))

(defn- emit-def-str [name-sym init buf]
  (buffer/push buf "(def ") (buffer/push buf (name-sym :name))
  (buffer/push buf " ") (emit-ast init buf) (buffer/push buf ")"))

(defn- emit-arity-str [ar buf]
  (buffer/push buf "[")
  (var i 0)
  (let [n (length (ar :param-names))]
    (while (< i n)
      (buffer/push buf (in (ar :param-names) i))
      (when (or (< (+ i 1) n) (ar :rest-name)) (buffer/push buf " "))
      (++ i)))
  (when (ar :rest-name)
    (buffer/push buf "& ") (buffer/push buf (ar :rest-name)))
  (buffer/push buf "] ")
  (emit-ast (ar :body) buf))

# Debug/source rendering. Single arity matches the original `(fn [params] body)`
# shape; multi-arity renders each arity as a clause. This path is for inspection
# (compile-string); the data emitter is the one that actually runs.
(defn- emit-fn-str [ast buf]
  (def arities (ast :arities))
  (if (ast :multi)
    (do
      (buffer/push buf "(fn")
      (each ar arities
        (buffer/push buf " (") (emit-arity-str ar buf) (buffer/push buf ")"))
      (buffer/push buf ")"))
    (do
      (buffer/push buf "(fn ") (emit-arity-str (first arities) buf) (buffer/push buf ")"))))

(defn- emit-let-str [binding-pairs body buf]
  (buffer/push buf "(let [")
  (var i 0)
  (let [n (length binding-pairs)]
    (while (< i n)
      (let [bp (in binding-pairs i)]
        (buffer/push buf (bp :name)) (buffer/push buf " ")
        (emit-ast (bp :init) buf)
        (when (< (+ i 1) n) (buffer/push buf " ")))
      (++ i)))
  (buffer/push buf "] ") (emit-ast body buf) (buffer/push buf ")"))

(defn- emit-throw-str [val buf]
  (buffer/push buf "(error ") (emit-ast val buf) (buffer/push buf ")"))

(defn- emit-try-str [body catch-sym catch-body finally-body buf]
  (buffer/push buf "(try ")
  (emit-ast body buf)
  (when catch-sym
    (buffer/push buf " ([")
    (buffer/push buf catch-sym)
    (buffer/push buf "] ")
    (if (= 1 (length catch-body))
      (emit-ast (first catch-body) buf)
      (do
        (buffer/push buf "(do ")
        (var i 0)
        (let [n (length catch-body)]
          (while (< i n)
            (emit-ast (in catch-body i) buf)
            (when (< (+ i 1) n) (buffer/push buf " "))
            (++ i)))
        (buffer/push buf ")")))
    (buffer/push buf ")"))
  (buffer/push buf ")"))

(defn- emit-loop-str [loop-name param-names init-vals body buf]
  (buffer/push buf "(do (var ") (buffer/push buf loop-name) (buffer/push buf " nil) ")
  (buffer/push buf "(set ") (buffer/push buf loop-name) (buffer/push buf " (fn [")
  (var i 0)
  (let [n (length param-names)]
    (while (< i n)
      (buffer/push buf (in param-names i))
      (when (< (+ i 1) n) (buffer/push buf " "))
      (++ i)))
  (buffer/push buf "] ")
  (emit-ast body buf)
  (buffer/push buf ")) (")
  (buffer/push buf loop-name)
  (each iv init-vals
    (buffer/push buf " ")
    (emit-ast iv buf))
  (buffer/push buf "))"))

(defn- emit-recur-str [args loop-name buf]
  (buffer/push buf "(") (buffer/push buf loop-name)
  (each arg args
    (buffer/push buf " ")
    (emit-ast arg buf))
  (buffer/push buf ")"))

(defn- emit-invoke-str [f-ast args buf]
  (buffer/push buf "(") (emit-ast f-ast buf)
  (each arg args (buffer/push buf " ") (emit-ast arg buf))
  (buffer/push buf ")"))

(defn- emit-symbol-str [name buf] (buffer/push buf name))
(defn- emit-local-str [name buf] (buffer/push buf name))
(defn- emit-core-symbol-str [janet-name buf] (buffer/push buf janet-name))

(defn- emit-qualified-symbol-str [ns name buf]
  (buffer/push buf "(ns-get \"") (buffer/push buf ns)
  (buffer/push buf "\" \"") (buffer/push buf name) (buffer/push buf "\")"))

(defn- emit-vector-str [items buf]
  (buffer/push buf "[")
  (var i 0)
  (let [n (length items)]
    (while (< i n)
      (emit-ast (in items i) buf)
      (when (< (+ i 1) n) (buffer/push buf " "))
      (++ i)))
  (buffer/push buf "]"))

(defn- emit-map-str [pairs buf]
  (buffer/push buf "(build-map-literal")
  (each [k v] pairs
    (buffer/push buf " ") (emit-ast k buf)
    (buffer/push buf " ") (emit-ast v buf))
  (buffer/push buf ")"))

(defn- emit-set-str [items buf]
  (buffer/push buf "(make-phs")
  (each it items (buffer/push buf " ") (emit-ast it buf))
  (buffer/push buf ")"))

(defn- raw-form->janet
  "Convert a Jolt reader form to a Janet data structure for quoting."
  [form]
  (cond
    (and (struct? form) (= :symbol (form :jolt/type)))
    (if (form :ns)
      (symbol (string (form :ns) "/" (form :name)))
      (symbol (form :name)))
    (array? form)
    (tuple/slice (tuple ;(map raw-form->janet form)))
    (tuple? form)
    (tuple/slice (tuple ;(map raw-form->janet form)))
    form))

(defn- emit-quote-str [expr buf]
  (buffer/push buf "'")
  (def janet-val (raw-form->janet expr))
  (cond
    (symbol? janet-val) (buffer/push buf (string janet-val))
    (number? janet-val) (buffer/push buf (string janet-val))
    (string? janet-val) (do (buffer/push buf "\"") (buffer/push buf janet-val) (buffer/push buf "\""))
    (keyword? janet-val) (do (buffer/push buf ":") (buffer/push buf (string janet-val)))
    (nil? janet-val) (buffer/push buf "nil")
    (= true janet-val) (buffer/push buf "true")
    (= false janet-val) (buffer/push buf "false")
    (buffer/push buf (string janet-val))))

(set emit-ast
  (fn [ast buf]
    (match (ast :op)
      :const (emit-const-str (ast :val) buf)
      :symbol (emit-symbol-str (ast :name) buf)
      :var (emit-symbol-str (ast :name) buf)
      :local (emit-local-str (ast :name) buf)
      :core-symbol (emit-core-symbol-str (ast :janet-name) buf)
      :qualified-symbol (emit-qualified-symbol-str (ast :ns) (ast :name) buf)
      :do (emit-do-str (ast :statements) (ast :ret) buf)
      :if (emit-if-str (ast :test) (ast :then) (ast :else) buf)
      :def (emit-def-str (ast :name) (ast :init) buf)
      :fn (emit-fn-str ast buf)
      :let (emit-let-str (ast :binding-pairs) (ast :body) buf)
      :throw (emit-throw-str (ast :val) buf)
      :try (emit-try-str (ast :body) (ast :catch-sym) (ast :catch-body) (ast :finally-body) buf)
      :loop (emit-loop-str (ast :loop-name) (ast :param-names) (ast :init-vals) (ast :body) buf)
      :recur (emit-recur-str (ast :args) (ast :loop-name) buf)
      :invoke (emit-invoke-str (ast :fn) (ast :args) buf)
      :vector (emit-vector-str (ast :items) buf)
      :map (emit-map-str (ast :pairs) buf)
      :set (emit-set-str (ast :items) buf)
      :quote (emit-quote-str (ast :expr) buf)
      (buffer/push buf (string "/* unhandled op: " (ast :op) " */")))))

# ============================================================
# Emitter — AST → Janet data structure (for direct eval)
# ============================================================

(var emit-expr nil)

(defn- emit-const-expr [val] val)
(defn- emit-symbol-expr [name] (symbol name))
(defn- emit-local-expr [name] (symbol name))

# Native Janet numeric ops: emit them as SYMBOLS (not inlined fn values) so
# Janet's compiler recognizes the primitive and uses its fast arithmetic/compare
# opcode rather than a function call.
(def- native-ops @{"+" true "-" true "*" true "<" true ">" true "<=" true ">=" true})

(defn- emit-core-symbol-expr [janet-name]
  (if (get native-ops janet-name)
    (symbol janet-name)
    # Resolve the core-* function value from the compiler's runtime env (where
    # `(use ./core)` bound them all) rather than a hand-maintained table that can
    # drift out of sync. A name with no binding falls back to the interpreter.
    (let [b (get jolt-runtime-env (symbol janet-name))]
      (if b (b :value)
        (uncompilable (string "core fn not found: " janet-name))))))

(defn- emit-qualified-symbol-expr [ns name]
  (error (string "Cannot eval qualified symbol at compile time: " ns "/" name)))

(defn- emit-do-expr [statements ret]
  (def exprs @['do])
  (each s statements (array/push exprs (emit-expr s)))
  (when ret (array/push exprs (emit-expr ret)))
  (tuple/slice (tuple ;exprs)))

(defn- emit-if-expr [test then else]
  (def exprs @['if])
  (array/push exprs (emit-expr test))
  (array/push exprs (emit-expr then))
  (when else (array/push exprs (emit-expr else)))
  (tuple/slice (tuple ;exprs)))

(defn- emit-def-expr [name-sym init]
  ['def (symbol (name-sym :name)) (emit-expr init)])

# Var-indirection: a global reference derefs its cell at call time, and a def
# sets the same cell's root and returns it (Clojure's #'var). Janet COPIES table
# constants when compiling but references functions, so we embed memoized
# getter/setter CLOSURES over the cell (by reference) rather than the cell itself.
(defn- var-getter [cell]
  (or (get cell :jolt/getter)
      (let [g (fn [] (var-get cell))] (put cell :jolt/getter g) g)))
(defn- var-setter [cell]
  (or (get cell :jolt/setter)
      (let [s (fn [v] (bind-root cell v) cell)] (put cell :jolt/setter s) s)))
(defn- emit-var-expr [cell] (tuple (var-getter cell)))
(defn- emit-def-var-expr [cell init] (tuple (var-setter cell) (emit-expr init)))

# An arity compiles to a named Janet fn whose name is its recur target — a
# recur is just a self-call (Janet tail-calls it). The rest param is an ordinary
# param holding a seq (not Janet `&`), so `(recur fixed... rest-seq)` works the
# way Clojure recur into a variadic arity does.
(defn- emit-arity-fn [ar]
  (def ps @[])
  (each pn (ar :param-names) (array/push ps (symbol pn)))
  (when (ar :rest-name) (array/push ps (symbol (ar :rest-name))))
  ['fn (symbol (ar :recur-name)) (tuple/slice ps) (emit-expr (ar :body))])

# Invoke an arity's fn with the actual args pulled out of the dispatch vector:
# fixed params by index, rest as a tuple slice.
(defn- emit-arity-invoke [ar jargs]
  (def call @[(emit-arity-fn ar)])
  (for i 0 (ar :n-fixed) (array/push call ['in jargs i]))
  (when (ar :rest-name) (array/push call ['tuple/slice jargs (ar :n-fixed)]))
  (tuple/slice call))

(defn- emit-fn-expr [ast]
  (def arities (ast :arities))
  (cond
    # Single fixed arity — the common, hot case. Emit the arity fn directly
    # (its name is the public name and the recur target); no dispatch overhead.
    (and (not (ast :multi)) (not ((first arities) :rest-name)))
      (emit-arity-fn (first arities))
    # Single variadic arity: a thin wrapper collects the call's args so the rest
    # seq can be built, then hands off to the arity fn.
    (not (ast :multi))
      (let [jargs (symbol (make-gensym "args"))]
        ['fn (symbol (ast :name)) ['& jargs] (emit-arity-invoke (first arities) jargs)])
    # Multi-arity: dispatch on arg count. Fixed arities match exactly; the (one)
    # variadic arity matches >= its fixed count and goes last.
    (let [jargs (symbol (make-gensym "args"))
          n-sym (symbol (make-gensym "n"))
          cond-form @['cond]]
      (each ar arities
        (if (ar :rest-name)
          (array/push cond-form ['>= n-sym (ar :n-fixed)])
          (array/push cond-form ['= n-sym (ar :n-fixed)]))
        (array/push cond-form (emit-arity-invoke ar jargs)))
      (array/push cond-form ['error "Wrong number of args passed to fn"])
      ['fn (symbol (ast :name)) ['& jargs]
       ['let [n-sym ['length jargs]] (tuple/slice cond-form)]])))

(defn- emit-let-expr [binding-pairs body]
  (def bind-tuple @[])
  (each bp binding-pairs
    (array/push bind-tuple (symbol (bp :name)))
    (array/push bind-tuple (emit-expr (bp :init))))
  ['let (tuple/slice (tuple ;bind-tuple)) (emit-expr body)])

(defn- emit-throw-expr [val]
  ['error (emit-expr val)])

(defn- emit-try-expr [body catch-sym catch-body finally-body]
  # Janet try: (try body ([err] handler-body))
  (def forms @['try (emit-expr body)])
  (when catch-sym
    (def err-binding [(symbol catch-sym)])
    (def handler
      (if (= 1 (length catch-body))
        (emit-expr (first catch-body))
        (do
          (def do-body @['do])
          (each cb catch-body (array/push do-body (emit-expr cb)))
          (tuple/slice (tuple ;do-body)))))
    (array/push forms [(tuple ;err-binding) handler]))
  (when finally-body
    (def finally-do @['do])
    (each fb finally-body (array/push finally-do (emit-expr fb)))
    (array/push forms (tuple/slice (tuple ;finally-do))))
  (tuple/slice (tuple ;forms)))

(defn- emit-loop-expr [loop-name param-names init-vals body]
  # Emit: (do (var loop-name nil) (set loop-name (fn [params] body)) (loop-name init-vals...))
  (def param-syms (map symbol param-names))
  (def loop-sym (symbol loop-name))
  (def body-emitted (emit-expr body))
  # For recur calls, rewrite (recur arg1 arg2) → (loop-name arg1 arg2)
  # This is done by the :recur op handler below which uses the loop-name from ast
  ['do
   ['var loop-sym nil]
   ['set loop-sym ['fn (tuple/slice (tuple ;param-syms)) body-emitted]]
   (tuple ;(array/insert (map emit-expr init-vals) 0 loop-sym))])

(defn- emit-recur-expr [args loop-name]
  # Emit: (loop-name arg1 arg2...)
  (def exprs @[(symbol loop-name)])
  (each arg args (array/push exprs (emit-expr arg)))
  (tuple/slice (tuple ;exprs)))

(defn- emit-invoke-expr [f-ast args]
  # Emit a DIRECT Janet call (f arg…) when the callee is a function reference —
  # a core op/fn, a local/global symbol, or an fn literal — so native ops keep
  # their fast opcodes and recursion is a direct call. Fall back to jolt-call
  # only when the head is a keyword/collection literal in call position (an IFn
  # that needs runtime lookup), e.g. (:k m) or ({:a 1} :a).
  (def direct (case (f-ast :op)
                :core-symbol true :symbol true :var true :local true
                :qualified-symbol true :fn true
                false))
  (def f (emit-expr f-ast))
  (def exprs (if direct @[f] @[jolt-call f]))
  (each arg args (array/push exprs (emit-expr arg)))
  (tuple/slice (tuple ;exprs)))

# A vector literal builds a mode-appropriate jolt vector (pvec when immutable,
# array when mutable) via make-vec — the same constructor the interpreter uses —
# so compiled and interpreted vectors share one representation. (Emitting a bare
# Janet tuple diverged: type-strict ops like rseq reject tuples.)
(defn- emit-vector-expr [items]
  (def t @['tuple])
  (each item items (array/push t (emit-expr item)))
  [make-vec (tuple/slice t)])

# Build a jolt map literal from evaluated alternating k/v args, mirroring the
# interpreter (eval-form's map-literal case): a Janet struct unless a key is a
# collection, in which case a phm so the key compares by value. Embedded as a
# function constant in emitted code (functions marshal by reference).
(defn build-map-literal [& kvs]
  # phm (not a Janet struct) when a key is a collection (value-based hashing) or a
  # key/value is nil (structs drop nil; phm preserves it, matching Clojure).
  (var need-phm false)
  (var ki 0)
  (while (< ki (length kvs))
    (let [kk (in kvs ki) vv (in kvs (+ ki 1))]
      (when (or (table? kk) (array? kk) (nil? kk) (nil? vv)) (set need-phm true)))
    (+= ki 2))
  (if need-phm
    (do (var m (make-phm)) (var j 0)
        (while (< j (length kvs)) (set m (phm-assoc m (in kvs j) (in kvs (+ j 1)))) (+= j 2))
        m)
    (struct ;kvs)))

(defn- emit-map-expr [pairs]
  (def call @[build-map-literal])
  (each [k v] pairs
    (array/push call (emit-expr k))
    (array/push call (emit-expr v)))
  (tuple/slice call))

(defn- emit-set-expr [items]
  (tuple/slice (tuple make-phs ;(map emit-expr items))))

(defn- emit-quote-expr [expr]
  ['quote (raw-form->janet expr)])

(set emit-expr
  (fn [ast]
    (match (ast :op)
      :const (emit-const-expr (ast :val))
      :symbol (emit-symbol-expr (ast :name))
      :var (emit-var-expr (ast :var))
      :local (emit-local-expr (ast :name))
      :core-symbol (emit-core-symbol-expr (ast :janet-name))
      :qualified-symbol (emit-qualified-symbol-expr (ast :ns) (ast :name))
      :do (emit-do-expr (ast :statements) (ast :ret))
      :if (emit-if-expr (ast :test) (ast :then) (ast :else))
      :def (if (ast :var) (emit-def-var-expr (ast :var) (ast :init))
             (emit-def-expr (ast :name) (ast :init)))
      :fn (emit-fn-expr ast)
      :let (emit-let-expr (ast :binding-pairs) (ast :body))
      :throw (emit-throw-expr (ast :val))
      :try (emit-try-expr (ast :body) (ast :catch-sym) (ast :catch-body) (ast :finally-body))
      :loop (emit-loop-expr (ast :loop-name) (ast :param-names) (ast :init-vals) (ast :body))
      :recur (emit-recur-expr (ast :args) (ast :loop-name))
      :invoke (emit-invoke-expr (ast :fn) (ast :args))
      :vector (emit-vector-expr (ast :items))
      :map (emit-map-expr (ast :pairs))
      :set (emit-set-expr (ast :items))
      :quote (emit-quote-expr (ast :expr))
      (error (string "Unhandled op: " (ast :op))))))

# ============================================================
# Public API
# ============================================================

(defn compile-form
  "Compile a Clojure form to a Janet source string."
  [form &opt ctx]
  (default ctx nil)
  (let [ast (analyze-form form @{} ctx)
        buf @""]
    (emit-ast ast buf)
    (string buf)))

(defn compile-ast
  "Compile a Clojure form to an eval-able Janet data structure."
  [form &opt ctx]
  (default ctx nil)
  (emit-expr (analyze-form form @{} ctx)))

(defn compile-and-eval
  "Compile a Clojure form and evaluate it as Janet. Globals resolve through Jolt
  var cells (see analyze-form/:var), so compiled def/defn results are visible to
  the interpreter (the cell is the namespace var), recursion self-references the
  cell, and redefinition is seen by compiled callers — no separate interning or
  named-fn rewrite needed."
  [form ctx]
  (eval (compile-ast form ctx) (ctx-janet-env ctx)))

(defn eval-compiled
  "Evaluate an already-compiled Janet form (the result of compile-ast) in the
  context's compiled env. Split out from compile-and-eval so callers can guard
  the compile step alone — see eval-one's hybrid fallback."
  [compiled ctx]
  (eval compiled (ctx-janet-env ctx)))
