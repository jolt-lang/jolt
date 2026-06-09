# Jolt Loader
# Namespace loading with optional compilation.
# Supports in-memory bytecode caching when :compile? is enabled.

(use ./reader)
(use ./evaluator)
(import ./backend :as backend)

# Stateful / context-modifying forms always interpret: they mutate the context
# (namespaces, macros, types, multimethods, dynamic vars, …) in ways the compiler
# doesn't model. Kept here so the compile/interpret routing lives in one place,
# used by both load-ns and the public eval-one. Shrinking toward the frozen
# host-coupled set (Stage 2 jolt-eaa): forms move off this list as they gain a
# compile path; syntax-quote already compiles via the analyzer's `handled` set.
(defn- stateful-head? [head-name]
  (or (= head-name "defmacro") (= head-name "ns")
      (= head-name "deftype") (= head-name "defmulti") (= head-name "defmethod")
      (= head-name "set!")
      (= head-name ".") (= head-name "new")
      (= head-name "eval")))

(defn- form-head-name [form]
  (when (array? form)
    (let [ff (first form)]
      (when (and (struct? ff) (= :symbol (ff :jolt/type))) (ff :name)))))

(defn eval-toplevel
  "Evaluate one top-level form for ctx, honoring :compile?. Stateful forms always
  interpret; otherwise the form runs through the self-hosted compile pipeline
  (portable Clojure analyzer -> IR -> Janet back end), which falls back to the
  interpreter for forms it can't compile. Only the compile step is guarded —
  runtime errors in compiled code propagate (no double-eval, no hidden errors)."
  [ctx form]
  (defn try-compile [] (backend/compile-and-eval ctx form))
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
  "Load a Clojure namespace from a .clj file. Per-form routing (compile-or-
  interpret, stateful forms interpret) is shared with eval-one via eval-toplevel.

  (load-ns ctx filepath) → namespace symbol string"
  [ctx filepath]
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

  (each form forms (eval-toplevel ctx form))
  ns-name)
