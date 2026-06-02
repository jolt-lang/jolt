# Jolt Public API
# High-level interface for the Clojure-on-Janet interpreter.

(use ./types)
(use ./reader)
(use ./evaluator)
(use ./core)

(defn- load-persistent-structures
  "Load immutable persistent data structures and swap clojure.core bindings.
  Replaces vec, vector, hash-map, hash-set, set with Jolt's persistent versions."
  [ctx]
  (def source (slurp "src/jolt/clojure/lang/persistent_vector.clj"))
  (var cur source)
  (while (> (length (string/trim cur)) 0)
    (def [form rest] (parse-next cur))
    (set cur rest)
    (when (not (nil? form))
      (eval-form ctx @{} form)))
  (let [core-ns (ctx-find-ns ctx "clojure.core")
        pv-ns (ctx-find-ns ctx "jolt.lang.persistent-vector")]
    (ns-intern core-ns "vec" (var-get (ns-find pv-ns "vector")))
    (ns-intern core-ns "vector" (var-get (ns-find pv-ns "vector")))
    (ns-intern core-ns "vector?" (var-get (ns-find pv-ns "vector?")))))

(defn init
  "Create a new Jolt evaluation context, optionally with opts.
  (init)          — empty context with clojure.core loaded
  (init opts)     — context with opts and clojure.core loaded
  
  Persistent immutable data structures are loaded by default.
  
  opts may contain:
    :namespaces — map of {ns-name → {sym → value, ...}, ...}
    :mutable?   — if true, use Janet mutable data structures instead"
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
  (eval-string ctx s) → value
  
  Returns the result of evaluating the first form in s."
  [ctx s]
  (let [form (parse-string s)]
    (eval-form ctx @{} form)))

(defn eval-string*
  "Evaluate a Clojure source string in a Jolt context.
  Like eval-string but with explicit bindings.
  (eval-string* ctx s bindings) → value"
  [ctx s bindings]
  (let [form (parse-string s)]
    (eval-form ctx bindings form)))
