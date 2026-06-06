# Janet implementation of the Jolt host contract (ns `jolt.host`).
#
# This is the seam between the portable jolt-core (analyzer/IR/core, pure Clojure
# under jolt-core/) and the Janet runtime. jolt-core calls ONLY these functions —
# never Janet directly. Re-hosting Jolt to another runtime means reimplementing
# this contract (+ the back end and RT) for that runtime.
#
# Lives in src/jolt/ (with the rest of the Janet host) rather than a separate
# host/janet/ dir: Janet resolves relative imports per-file, so a host/janet
# module importing ../../src/jolt/* loads SECOND instances of compiler/types/core
# (inconsistent state). The portability boundary is the `jolt.host` namespace
# contract + jolt-core/, not the directory.
#
# Two groups:
#   1. Form introspection — reader forms are host-specific (the reader is the
#      host's), so shape predicates/accessors live here. Returns jolt values the
#      analyzer walks with ordinary Clojure.
#   2. Compile-time environment — resolve symbols to vars/macros, expand macros,
#      the current namespace. These take ctx (an opaque host handle).

(use ./types)
(use ./evaluator)
(use ./core)

# ---------------------------------------------------------------------------
# Form introspection
# ---------------------------------------------------------------------------

(defn h-sym? [form] (and (struct? form) (= :symbol (form :jolt/type))))
(defn h-sym-name [form] (form :name))
(defn h-sym-ns [form] (form :ns))

(defn h-list? [form] (array? form))          # a call / list (reader: array)
(defn h-vector? [form] (tuple? form))        # a vector literal (reader: tuple)
(defn h-map? [form] (and (struct? form) (nil? (form :jolt/type))))
(defn h-set? [form] (and (struct? form) (= :jolt/set (form :jolt/type))))
(defn h-char? [form] (and (struct? form) (= :jolt/char (form :jolt/type))))

(defn h-literal? [form]
  (or (nil? form) (boolean? form) (number? form) (string? form)
      (keyword? form) (h-char? form)))

# Items of a list/vector as a jolt vector, so the analyzer walks them with Clojure.
(defn h-elements [form] (make-vec form))
(defn h-vector-items [form] (make-vec form))
(defn h-map-pairs [form]
  (make-vec (map (fn [k] (make-vec [k (get form k)])) (keys form))))
(defn h-set-items [form] (make-vec (form :value)))

# ---------------------------------------------------------------------------
# Compile-time environment
# ---------------------------------------------------------------------------

# Names the analyzer must NOT treat as a function call: interpreter special forms
# plus definitional/host macros the compiler doesn't lower. The analyzer handles
# a subset (quote/if/do/def/fn*/let*/loop*/recur/throw/try) and falls back to the
# interpreter for the rest. Kept in sync with evaluator/special-symbol? and
# compiler/uncompilable-heads.
(def- special-names
  (let [t @{}]
    (each n ["quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
             "defmacro" "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "var"
             "locking" "eval" "instance?" "defmulti" "defmethod" "deftype" "new"
             "." "var-get" "var-set" "var?" "alter-var-root" "find-var" "intern"
             "alter-meta!" "reset-meta!" "disj" "set?" "satisfies?"
             "protocol-dispatch" "register-method" "make-reified" "prefer-method"
             "remove-method" "remove-all-methods" "get-method" "methods"
             "read-string" "macroexpand-1" "defonce" "ns" "in-ns" "require"
             "import" "use" "refer" "defrecord" "defprotocol" "definterface"
             "reify" "proxy" "extend-type" "extend-protocol" "extend" "gen-class"
             "monitor-enter" "monitor-exit" "binding" "letfn"]
      (put t n true))
    t))

(defn h-special? [name] (if (get special-names name) true false))

(defn h-current-ns [ctx] (ctx-current-ns ctx))

(defn h-macro? [ctx sym]
  (let [v (resolve-var ctx @{} sym)]
    (if (and v (var-macro? v)) true false)))

(defn h-expand-1 [ctx form]
  (let [head (in form 0)
        v (resolve-var ctx @{} head)
        macro-fn (var-get v)]
    (apply macro-fn (tuple/slice form 1))))

# Classify a global (non-local) symbol reference:
#   {:kind :var  :ns NS :name NAME}  — a Jolt var (current ns / clojure.core)
#   {:kind :host :name NAME}         — resolves only via the host env (+, int?, …),
#                                      same fallback the interpreter's resolve-sym uses
#   {:kind :unresolved :name NAME}   — not yet defined (forward reference)
(defn h-resolve-global [ctx sym]
  (let [v (resolve-var ctx @{} sym)]
    (if v
      {:kind :var :ns (var-ns v) :name (var-name v)}
      (let [nm (sym :name)
            entry (in (fiber/getenv (fiber/current)) (symbol nm))]
        (if (not (nil? entry))
          {:kind :host :name nm}
          {:kind :unresolved :name nm})))))

(defn h-intern! [ctx ns-name nm]
  (ns-intern (ctx-find-ns ctx ns-name) nm)
  nil)

# ---------------------------------------------------------------------------
# Installation: bind these fns as vars in the `jolt.host` namespace so jolt-core
# can call them. Idempotent per context.
# ---------------------------------------------------------------------------

(def- exports
  {"sym?" h-sym? "sym-name" h-sym-name "sym-ns" h-sym-ns
   "list?" h-list? "vector?" h-vector? "map?" h-map? "set?" h-set? "char?" h-char?
   "literal?" h-literal? "elements" h-elements "vector-items" h-vector-items
   "map-pairs" h-map-pairs "set-items" h-set-items
   "special?" h-special? "current-ns" h-current-ns "macro?" h-macro?
   "expand-1" h-expand-1 "resolve-global" h-resolve-global "intern!" h-intern!})

(defn install! [ctx]
  (def ns (ctx-find-ns ctx "jolt.host"))
  (eachp [nm f] exports (ns-intern ns nm f))
  ns)
