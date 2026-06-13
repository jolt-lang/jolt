# Jolt Loader
# Namespace loading with optional compilation.
# Supports in-memory bytecode caching when :compile? is enabled.

(use ./reader)
(use ./types)
(use ./evaluator)
(import ./backend :as backend)

# Stateful / context-modifying forms always interpret: they mutate the context
# (namespaces, macros, types, multimethods, dynamic vars, …) in ways the compiler
# doesn't model. Kept here so the compile/interpret routing lives in one place,
# used by both load-ns and the public eval-one. Shrinking toward the frozen
# host-coupled set (Stage 2 jolt-eaa): forms move off this list as they gain a
# compile path; syntax-quote already compiles via the analyzer's `handled` set.
(defn- stateful-head? [head-name]
  (or (= head-name "defmacro")
      (= head-name "set!")
      (= head-name ".") (= head-name "new")
      (= head-name "eval")))

(defn- form-head-name [form]
  (when (array? form)
    (let [ff (first form)]
      (when (and (struct? ff) (= :symbol (ff :jolt/type))) (ff :name)))))

(defn- eval-toplevel-1
  [ctx form]
  # Repair point for the interpreted-fn ns swap: a body runs with current-ns
  # rebound to its defining ns and restores it on normal return; an UNWINDING
  # throw skips those restores (they're plain trailing calls — defer/try per
  # call would cost a fiber per frame and blow the C stack on deep recursion).
  # So save here, and on error put the entry ns back before re-raising — the
  # ctx never leaks a callee's ns across top-level forms.
  (def entry-ns (ctx-current-ns ctx))
  (defn- run []
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
  (try
    (run)
    ([err fib]
      (ctx-set-current-ns ctx entry-ns)
      # Stash the full trace TEXT at this innermost boundary: janet's
      # debug/stacktrace walks the propagation chain (fiber->child, no public
      # accessor), so this is the only place the user's compiled frames
      # (in _r$ns/fn--N ...) are reachable. Innermost capture wins; the CLI's
      # report-error filters + demangles it. (jolt-2o7.1/2)
      (when (nil? (get (ctx :env) :error-trace))
        (def buf @"")
        (with-dyns [:err buf] (debug/stacktrace fib err ""))
        (put (ctx :env) :error-trace (string buf)))
      # propagate (not error): re-raising with `error` discards the failing
      # fiber's stack
      (propagate err fib))))

(defn eval-toplevel
  "Evaluate one top-level form for ctx, honoring :compile?. Stateful forms always
  interpret; otherwise the form runs through the self-hosted compile pipeline
  (portable Clojure analyzer -> IR -> Janet back end), which falls back to the
  interpreter for forms it can't compile. Only the compile step is guarded —
  runtime errors in compiled code propagate (no double-eval, no hidden errors)."
  [ctx form]
  # Clojure's top-level `do` rule: children are compiled AND evaluated one at
  # a time, so a child's runtime effects (defmulti's var intern, requires, …)
  # are visible while the NEXT child compiles. Without the split, (do
  # (defmulti area …) (area …)) can't analyze — `area` only exists once the
  # defmulti has RUN, and unresolved symbols are analysis errors now
  # (jolt-2o7.3).
  (if (and (array? form) (= "do" (form-head-name form)))
    (do
      (var res nil)
      (each child (array/slice form 1) (set res (eval-toplevel ctx child)))
      res)
    (eval-toplevel-1 ctx form)))

(defn eval-forms-positioned
  "Evaluate parsed [form line] pairs, recording WHERE an error happened: the
  innermost failing form's {:file :line} goes to (env :error-pos) and each
  file unwound through joins the (env :error-loading) chain — the CLI's
  report-error prints 'at file:line' and 'while loading …' from these.
  (jolt-2o7.4)"
  [ctx pairs file]
  (var res nil)
  (each [form line] pairs
    (try
      (set res (eval-toplevel ctx form))
      ([err fib]
        (def env (ctx :env))
        (when (nil? (get env :error-pos))
          (put env :error-pos {:file file :line line}))
        (when (nil? (get env :error-loading)) (put env :error-loading @[]))
        (def chain (get env :error-loading))
        (when (not= (last chain) file) (array/push chain file))
        (propagate err fib))))
  res)

(defn load-ns
  "Load a Clojure namespace from a .clj file. Per-form routing (compile-or-
  interpret, stateful forms interpret) is shared with eval-one via eval-toplevel.

  (load-ns ctx filepath) → namespace symbol string"
  [ctx filepath]
  (def source (slurp filepath))
  (when (or (checker-enabled?) (get (ctx :env) :inline?))
    (track-positions! true)
    (put (ctx :env) :tc-source source)
    (put (ctx :env) :tc-file filepath))
  (def pairs (parse-all-positioned source filepath))
  (var ns-name nil)
  (each [form _] pairs
    # Extract ns name from the first ns form
    (when (and (nil? ns-name)
               (array? form)
               (> (length form) 0)
               (and (struct? (first form))
                    (= :symbol ((first form) :jolt/type))
                    (= "ns" ((first form) :name))))
      (let [name-form (in form 1)]
        (set ns-name (if (struct? name-form) (name-form :name) (string name-form))))))

  (when (nil? ns-name)
    (error (string "No ns form found in " filepath)))

  (eval-forms-positioned ctx pairs filepath)
  ns-name)
