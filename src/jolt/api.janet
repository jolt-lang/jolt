# Jolt Public API
# High-level interface for the Clojure-on-Janet interpreter.

(use ./types)
(use ./reader)
(use ./evaluator)
(use ./core)
(use ./compiler)
(use ./loader)

(defn- load-persistent-structures
  "Load immutable persistent data structures and swap clojure.core bindings."
  [ctx]
  (def saved-ns (ctx-current-ns ctx))
  (def source (slurp "src/jolt/clojure/lang/persistent_vector.clj"))
  (var cur source)
  (while (> (length (string/trim cur)) 0)
    (def [form rest] (parse-next cur))
    (set cur rest)
    (when (not (nil? form))
      (eval-form ctx @{} form)))
  # Vectors are represented as Janet tuples throughout core; bind vec/vector/
  # vector? to the tuple-based implementations so literals (`[...]`) and the
  # constructors share one representation. The PersistentVector namespace stays
  # loaded for code that wants it explicitly via jolt.lang.persistent-vector.
  (let [core-ns (ctx-find-ns ctx "clojure.core")]
    (ns-intern core-ns "vec" core-vec)
    (ns-intern core-ns "vector" core-vector)
    (ns-intern core-ns "vector?" core-vector?))
  # Restore the namespace: loading the PV file above left current-ns set to
  # jolt.lang.persistent-vector, which would shadow clojure.core bindings.
  (ctx-set-current-ns ctx saved-ns))

(defn init
  "Create a new Jolt evaluation context.
  opts may contain:
    :namespaces — map of {ns-name → {sym → value, ...}, ...}
    :mutable?   — use Janet mutable data structures instead of persistent
    :compile?   — enable compilation of Clojure forms to Janet"
  [&opt opts]
  (default opts {})
  (let [ctx (make-ctx opts)
        mutable? (get opts :mutable?)]
    (init-core! ctx)
    (if mutable?
      nil
      (load-persistent-structures ctx))
    ctx))

(defn eval-string
  "Evaluate a Clojure source string in a Jolt context.
  When :compile? is enabled, compiles to Janet and evaluates.
  Macros are expanded at compile time.
  Context-modifying forms (ns, defmacro, deftype, require, in-ns, defmulti, defmethod)
  always use the interpreter."
  [ctx s]
  (let [compile? (get (ctx :env) :compile?)
        form (parse-string s)]
    (if compile?
      (if (array? form)
        # Lists: check for stateful forms
        (let [first-form (first form)
              head-name (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
                         (first-form :name)
                         nil)
              stateful? (or (= head-name "defmacro") (= head-name "ns")
                            (= head-name "deftype") (= head-name "defmulti") (= head-name "defmethod")
                            (= head-name "require") (= head-name "in-ns")
                            (= head-name "syntax-quote") (= head-name "set!")
                            (= head-name "var") (= head-name ".") (= head-name "new")
                            (= head-name "eval"))]
          (if stateful?
            (eval-form ctx @{} form)
            (compile-and-eval form ctx)))
        # Bare symbols and other non-literal forms: also compile
        (if (or (and (struct? form) (= :symbol (form :jolt/type)))
                (tuple? form))
          (compile-and-eval form ctx)
          (eval-form ctx @{} form)))
      # No compile flag: always interpret
      (eval-form ctx @{} form))))

(defn eval-string*
  "Evaluate a Clojure source string with explicit bindings."
  [ctx s bindings]
  (let [form (parse-string s)]
    (eval-form ctx bindings form)))

(defn load-string
  "Evaluate all forms from a Clojure source string.
  Uses parse-next to load every top-level form in sequence.
  Returns the result of the last form evaluated."
  [ctx s]
  (var cur s)
  (var result nil)
  (while (> (length (string/trim cur)) 0)
    (def [form rest] (parse-next cur))
    (set cur rest)
    (when (not (nil? form))
      (set result (eval-form ctx @{} form))))
  result)

(defn compile-string
  "Compile a Clojure source string to Janet source.
  Returns the Janet source string."
  [s]
  (let [form (parse-string s)]
    (compile-form form)))

(defn compile-file
  "Compile a .clj file to Janet source and optionally eval it.
  When ctx has :compile? enabled, also evaluates the compiled forms.
  Returns the namespace name."
  [ctx filepath]
  (load-ns ctx filepath))
