# Janet back end: host-neutral IR (from jolt.analyzer) -> Janet form -> bytecode.
#
# Host-specific by definition (it targets Janet). It resolves name-based :var
# nodes to Janet var cells and reuses runtime helpers (jolt-call, make-vec,
# build-map-literal). The portable front end (jolt.analyzer) never sees any of
# this; a different runtime provides its own back end against the same IR.
#
# In src/jolt/ (not host/janet/) for the same module-resolution reason as
# host_iface — see that file's header.

(use ./types)
(use ./core)
(import ./compiler :as comp)
(use ./evaluator)
(import ./reader :as r)

# Var late-binding: deref/set through the cell via a memoized closure so compiled
# code sees redefinition (Janet early-binds plain symbols). Same scheme as the
# bootstrap compiler.
(defn- var-getter [cell]
  (or (get cell :jolt/getter)
      (let [g (fn [] (var-get cell))] (put cell :jolt/getter g) g)))
(defn- var-setter [cell]
  (or (get cell :jolt/setter)
      (let [s (fn [v] (bind-root cell v) cell)] (put cell :jolt/setter s) s)))

(defn- cell-for [ctx ns-name nm]
  (ns-intern (ctx-find-ns ctx ns-name) nm))

# Fresh Janet symbol for back-end-introduced bindings (arity dispatch). NOT
# Janet's `gensym` — `(use ./core)` shadows it with Jolt's, which returns a jolt
# symbol struct (invalid in a Janet param position).
(var- gsym-counter 0)
(defn- gsym [] (def s (symbol "_be$" gsym-counter)) (++ gsym-counter) s)

(var emit nil)

(defn- emit-seq [ctx node]
  (def out @['do])
  (each s (vview (node :statements)) (array/push out (emit ctx s)))
  (array/push out (emit ctx (node :ret)))
  (tuple/slice out))

(defn- emit-let [ctx node]
  (def binds @[])
  (each pair (vview (node :bindings))
    (def p (vview pair))
    (array/push binds (symbol (in p 0)))
    (array/push binds (emit ctx (in p 1))))
  ['let (tuple/slice binds) (emit ctx (node :body))])

# A named Janet fn whose name is the arity's recur target, so recur is a
# self-call (Janet tail-calls it).
(defn- emit-arity-fn [ctx ar]
  (def ps @[])
  (each pn (vview (ar :params)) (array/push ps (symbol pn)))
  (when (ar :rest) (array/push ps '&) (array/push ps (symbol (ar :rest))))
  (if (ar :recur-name)
    ['fn (symbol (ar :recur-name)) (tuple/slice ps) (emit ctx (ar :body))]
    ['fn (tuple/slice ps) (emit ctx (ar :body))]))

(defn- emit-loop [ctx node]
  (def L (symbol (node :recur-name)))
  (def params @[])
  (def inits @[])
  (each pair (vview (node :bindings))
    (def p (vview pair))
    (array/push params (symbol (in p 0)))
    (array/push inits (emit ctx (in p 1))))
  ['do
   ['var L nil]
   ['set L ['fn (tuple/slice params) (emit ctx (node :body))]]
   (tuple/slice (array/concat @[L] inits))])

(defn- emit-recur [ctx node]
  (tuple/slice (array/concat @[(symbol (node :recur-name))]
                            (map |(emit ctx $) (vview (node :args))))))

(defn- emit-try [ctx node]
  (def core
    (if (node :catch-sym)
      ['try (emit ctx (node :body))
       [[(symbol (node :catch-sym))] (emit ctx (node :catch-body))]]
      (emit ctx (node :body))))
  (if (node :finally)
    ['defer (emit ctx (node :finally)) core]
    core))

(defn- emit-fn [ctx node]
  (def arities (vview (node :arities)))
  (if (= 1 (length arities))
    (emit-arity-fn ctx (first arities))
    # Multi-arity: dispatch on arg count; fixed arities match exactly, the
    # variadic one matches >= its fixed count. apply spreads the captured args
    # into the chosen arity fn (whose own & collects any rest).
    (let [jargs (gsym)
          nsym (gsym)
          cf @['cond]]
      (each ar arities
        (def nfixed (length (vview (ar :params))))
        (array/push cf (if (ar :rest) [>= nsym nfixed] [= nsym nfixed]))
        (array/push cf [apply (emit-arity-fn ctx ar) jargs]))
      (array/push cf ['error "wrong number of args passed to fn"])
      ['fn ['& jargs]
       ['do ['def nsym ['length jargs]] (tuple/slice cf)]])))

(defn- direct-call? [fnode]
  (case (fnode :op) :var true :local true :fn true :host true false))

# Hot primitives emitted as native Janet ops (host-specific optimization): a
# call to clojure.core/+ etc. becomes (+ …) rather than a var deref + variadic
# core fn. Matches numeric semantics; relaxes the non-number checks (a documented
# perf-mode divergence, same as the bootstrap's core-renames).
(def- native-ops
  {"+" '+ "-" '- "*" '* "<" '< ">" '> "<=" '<= ">=" '>= "inc" '++ "dec" '--})

(defn- native-op
  "If fnode is a clojure.core ref (or host ref) to a native-op primitive, return
  the Janet op symbol, else nil. inc/dec are unary so only at arity 1."
  [fnode nargs]
  (def nm (case (fnode :op)
            :var (when (= "clojure.core" (fnode :ns)) (fnode :name))
            :host (fnode :name)
            nil))
  (def op (and nm (get native-ops nm)))
  (cond
    (nil? op) nil
    (and (or (= op '++) (= op '--)) (not= nargs 1)) nil
    op))

(defn- emit-invoke [ctx node]
  (def args (map |(emit ctx $) (vview (node :args))))
  (def nop (native-op (node :fn) (length args)))
  (cond
    nop (case nop
          '++ ['+ (in args 0) 1]
          '-- ['- (in args 0) 1]
          (tuple nop ;args))
    (direct-call? (node :fn)) (tuple (emit ctx (node :fn)) ;args)
    (tuple jolt-call (emit ctx (node :fn)) ;args)))

(defn- emit-vector [ctx node]
  (def items (map |(emit ctx $) (vview (node :items))))
  (tuple make-vec (tuple/slice (array/concat @['tuple] items))))

(defn- emit-map [ctx node]
  (def args @[comp/build-map-literal])
  (each pair (vview (node :pairs))
    (def p (vview pair))
    (array/push args (emit ctx (in p 0)))
    (array/push args (emit ctx (in p 1))))
  (tuple/slice args))

(set emit
  (fn emit [ctx node]
    (case (node :op)
      :const (node :val)
      :local (symbol (node :name))
      :host (symbol (node :name))
      :var (tuple (var-getter (cell-for ctx (node :ns) (node :name))))
      :if ['if (emit ctx (node :test)) (emit ctx (node :then)) (emit ctx (node :else))]
      :do (emit-seq ctx node)
      :loop (emit-loop ctx node)
      :recur (emit-recur ctx node)
      :try (emit-try ctx node)
      :throw ['error (emit ctx (node :expr))]
      :def (tuple (var-setter (cell-for ctx (node :ns) (node :name))) (emit ctx (node :init)))
      :let (emit-let ctx node)
      :fn (emit-fn ctx node)
      :invoke (emit-invoke ctx node)
      :vector (emit-vector ctx node)
      :map (emit-map ctx node)
      :quote ['quote (node :form)]
      (error (string "backend: unhandled op " (node :op))))))

(defn emit-ir
  "IR node -> Janet form (public entry for the back end)."
  [ctx node]
  (emit ctx node))

# --- pipeline wiring (the self-hosted compile path) ---

# Compile-load a jolt-core namespace via the bootstrap so it runs as native
# bytecode. The analyzer uses unqualified referred names (jolt.host form-* + the
# IR ctors), so the bootstrap's plain :var path compiles it. Stateful forms (the
# ns/require) fall back to the interpreter. Source from the embedded stdlib map.
(defn- compile-load [ctx ns-name]
  (def src (get (get (ctx :env) :embedded-sources @{}) ns-name))
  (when src
    (def saved (ctx-current-ns ctx))
    (ctx-set-current-ns ctx ns-name)
    (var s src)
    (while (> (length (string/trim s)) 0)
      (def parsed (r/parse-next s))
      (set s (in parsed 1))
      (def f (in parsed 0))
      (when (not (nil? f))
        # Guard BOTH compile and the Janet-compile-of-emitted step: a form whose
        # emitted Janet is invalid (e.g. a bad splice) falls back to interpreted
        # definition rather than killing the whole load.
        (def r (protect (comp/eval-compiled (comp/compile-ast f ctx) ctx)))
        (unless (r 0) (eval-form ctx @{} f))))
    (ctx-set-current-ns ctx saved)))

(defn- ensure-analyzer [ctx]
  (when (= 0 (length ((ctx-find-ns ctx "jolt.analyzer") :mappings)))
    (compile-load ctx "jolt.ir")
    (compile-load ctx "jolt.analyzer")))

(defn analyze-form
  "Run the portable Clojure analyzer (jolt.analyzer/analyze) on a reader form,
  returning host-neutral IR."
  [ctx form]
  (ensure-analyzer ctx)
  # Capture the real compile ns: the analyzer runs interpreted (defined in
  # jolt.analyzer), and the interpreter rebinds current-ns to a fn's defining ns
  # while it runs — so h/current-ns must read this instead of ctx-current-ns.
  (put (ctx :env) :compile-ns (ctx-current-ns ctx))
  (def av (ns-find (ctx-find-ns ctx "jolt.analyzer") "analyze"))
  (def r ((var-get av) ctx form))
  (put (ctx :env) :compile-ns nil)
  r)

(defn compile-and-eval
  "Self-hosted compile path: analyze (portable Clojure) -> IR -> Janet -> eval.
  Hybrid: only the compile step (analyze+emit) is guarded — a form the analyzer
  can't handle throws and falls back to the interpreter; runtime errors in
  compiled code propagate (no double-eval, no hidden errors)."
  [ctx form]
  (def compiled (protect (emit-ir ctx (analyze-form ctx form))))
  (if (compiled 0)
    (eval (compiled 1) (comp/ctx-janet-env ctx))
    (eval-form ctx @{} form)))
