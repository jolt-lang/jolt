# Jolt Loader
# Namespace loading with optional compilation.
# Supports in-memory bytecode caching when :compile? is enabled.

(use ./reader)
(use ./compiler)
(use ./evaluator)

# Stateful / context-modifying forms always interpret: they mutate the context
# (namespaces, macros, types, multimethods, dynamic vars, …) in ways the compiler
# doesn't model. Kept here so the compile/interpret routing lives in one place,
# used by both load-ns and the public eval-one.
(defn- stateful-head? [head-name]
  (or (= head-name "defmacro") (= head-name "ns")
      (= head-name "deftype") (= head-name "defmulti") (= head-name "defmethod")
      (= head-name "require") (= head-name "in-ns")
      (= head-name "syntax-quote") (= head-name "set!")
      (= head-name "var") (= head-name ".") (= head-name "new")
      (= head-name "eval")))

(defn- form-head-name [form]
  (when (array? form)
    (let [ff (first form)]
      (when (and (struct? ff) (= :symbol (ff :jolt/type))) (ff :name)))))

(defn eval-toplevel
  "Evaluate one top-level form for ctx, honoring :compile?. Stateful forms always
  interpret; otherwise the form is compiled and run, falling back to the
  interpreter when the compiler can't handle it. Only the compile step is guarded
  — runtime errors in compiled code propagate (no double-eval, no hidden errors)."
  [ctx form]
  (defn try-compile []
    (let [compiled (protect (compile-ast form ctx))]
      (if (compiled 0)
        (eval-compiled (compiled 1) ctx)
        (eval-form ctx @{} form))))
  (if (get (ctx :env) :compile?)
    (if (array? form)
      # A call/list: compile it unless its head is a stateful special form.
      (let [hn (form-head-name form)]
        (if (and hn (stateful-head? hn))
          (eval-form ctx @{} form)
          (try-compile)))
      # A bare symbol or vector literal compiles; anything else interprets.
      (if (or (and (struct? form) (= :symbol (form :jolt/type))) (tuple? form))
        (try-compile)
        (eval-form ctx @{} form)))
    (eval-form ctx @{} form)))

(defn load-ns
  "Load a Clojure namespace from a .clj file.
  When ctx has :compile? enabled, forms are compiled to Janet source,
  evaluated via Janet's evaluator, and cached.
  
  (load-ns ctx filepath) → namespace symbol string"
  [ctx filepath]
  (let [env (ctx :env)
        compile? (get env :compile?)
        cache (get env :compiled-cache)]
    
    (def source (slurp filepath))
    (var ns-name nil)
    (var remaining source)
    (var forms @[])
    
    # Parse all forms
    (while (> (length (string/trim remaining)) 0)
      (def [form rest] (parse-next remaining))
      (set remaining rest)
      (when (not (nil? form))
        (array/push forms form)
        # Extract ns name from the first ns form
        (when (and (nil? ns-name)
                   (array? form)
                   (> (length form) 0)
                   (and (struct? (first form))
                        (= :symbol ((first form) :jolt/type))
                        (= "ns" ((first form) :name))))
          (let [name-form (in form 1)]
            (set ns-name (if (struct? name-form) (name-form :name) (string name-form)))))))
    
    (when (nil? ns-name)
      (error (string "No ns form found in " filepath)))
    
    # Per-form routing (compile-or-interpret, stateful forms interpret) is shared
    # with eval-one via eval-toplevel. When compiling, also record the forms so a
    # namespace can be inspected / re-emitted.
    (when compile?
      (var cached (get cache ns-name))
      (when (nil? cached)
        (set cached @[])
        (put cache ns-name cached))
      (each form forms (array/push cached form)))
    (each form forms (eval-toplevel ctx form))
    ns-name))

(defn compiled?
  "Check if a namespace has been compiled and cached."
  [ctx ns-name]
  (let [cache (get (ctx :env) :compiled-cache)]
    (not (nil? (get cache ns-name)))))

(defn get-compiled-forms
  "Get the compiled Janet source forms for a namespace."
  [ctx ns-name]
  (get (ctx :env) :compiled-cache ns-name))

(defn clear-compiled-cache
  "Clear the compiled form cache for a namespace or all namespaces."
  [ctx &opt ns-name]
  (let [cache (get (ctx :env) :compiled-cache)]
    (if ns-name
      (put cache ns-name nil)
      (loop [[k] :pairs cache] (put cache k nil)))))
