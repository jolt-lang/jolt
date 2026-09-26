(ns hooks.jolt.ffi
  "clj-kondo hooks for jolt.ffi.

  jolt.ffi/defcfn expands to a __cfn special form that only the compiler
  understands, so clj-kondo never sees the var it binds: every use of the
  bound name reads as an unresolved symbol, and clj-kondo cannot check its
  arity either. This hook mirrors stdlib/jolt/ffi.clj's own `def-cfn-form`
  well enough for static analysis: it rewrites each defcfn call into a
  `def`/`defn`-shaped form clj-kondo can see, with the same arity as the
  real binding.

  jolt.ffi/with-arena binds a single lexical symbol with no accompanying
  value ([arena] & body, not [sym expr] like `let`), which `:lint-as
  clojure.core/let` cannot model, because `let` requires an even number of binding
  forms. The other scoped-allocation macros (with-alloc, with-out,
  with-layout, with-c-string, with-c-string-array) all bind exactly one
  [sym expr] pair and are handled by :lint-as in config.edn instead; only
  with-arena needs a hook.

  See ../../../../../CHANGELOG.md and ../../../../../README.md's
  \"REPL and editor integration\" section for how this is verified and
  imported."
  (:require [clj-kondo.hooks-api :as api]))

;; -- defcfn --------------------------------------------------------------

(def ^:private varargs-markers
  "Both spellings def-cfn-form's argtype vector accepts for the variadic
  boundary; see ffi.clj's comment above `cfn-form`."
  #{:varargs :&})

(defn- csym-index
  "def-cfn-form finds the C symbol by shape, not position: the first string
  immediately followed by the argtype vector. Everything before it is an
  optional docstring then an optional attribute map, in that order; anything
  after it is [argtypes rettype [option] [wrapper...]]. `args` is the vector
  of nodes following `defcfn`'s name argument."
  [args]
  (first (keep-indexed (fn [i n]
                         (when (and (api/string-node? n)
                                    (< (inc i) (count args))
                                    (api/vector-node? (nth args (inc i))))
                           i))
                       args)))

(defn- arg-params
  "A parameter vector with the same arity as calling the raw C binding.
  A :varargs/:& marker inside argtypes splits it: fixed params before the
  marker, plus either more fixed params (a declared tail: the types after
  the marker are concrete arguments the binding always passes, e.g. fcntl's
  [:int :int :varargs :int] calls as (fd cmd 0), arity 3) or, when nothing
  follows the marker, a bare variadic tail whose shape each call infers
  (snprintf's [:pointer :size_t :string :&], arity 3+)."
  [argtypes-node]
  (let [types (:children argtypes-node)
        marker-i (first (keep-indexed (fn [i n] (when (contains? varargs-markers (api/sexpr n)) i))
                                      types))]
    (if marker-i
      (let [after (- (count types) marker-i 1)
            fixed (+ marker-i after)]
        (if (zero? after)
          (api/vector-node
           (conj (mapv (fn [i] (api/token-node (symbol (str "_arg" i)))) (range fixed))
                 (api/token-node '&) (api/token-node '_rest)))
          (api/vector-node (mapv (fn [i] (api/token-node (symbol (str "_arg" i)))) (range fixed)))))
      (api/vector-node (mapv (fn [i] (api/token-node (symbol (str "_arg" i)))) (range (count types)))))))

(defn- opaque-value
  "The C return value has no static type clj-kondo should assume. A wrong
  guess (say, false for :bool) can trip an unrelated truthiness lint at a
  call site that was never wrong. deref of a fresh atom is `any?` to
  clj-kondo, the same device jolt.ffi's own model, babashka.ffi's exported
  hook, uses for its defcfn."
  []
  (api/list-node [(api/token-node 'clojure.core/deref)
                  (api/list-node [(api/token-node 'clojure.core/atom) (api/token-node nil)])]))

(defn- raw-fn-node [argtypes-node]
  (api/list-node [(api/token-node 'clojure.core/fn) (arg-params argtypes-node) (opaque-value)]))

(defn- wrapper-arities
  "def-cfn-form splices the wrapper's own forms straight after the name:
  `[params] body...` for one arity, or `([p1] b1...) ([p2] b2...)` for
  several. Normalize either shape into a seq of {:params :body} maps."
  [fn-tail]
  (if (api/vector-node? (first fn-tail))
    [{:params (first fn-tail) :body (vec (rest fn-tail))}]
    (map (fn [arity-node]
           (let [children (:children arity-node)]
             {:params (first children) :body (vec (rest children))}))
         fn-tail)))

(defn- wrapper-arity-node
  "One `defn` arity whose body is the wrapper's own body, with the raw
  binding in lexical scope exactly as def-cfn-form's `let` puts it there.
  A `defn`-shaped result (rather than def-cfn-form's own `(def name (let
  [raw ...] (fn name ...)))`) is deliberate: clj-kondo infers a var's arity
  from a `defn` or from `(def name (fn ...))` directly, but not through a
  `let` between `def` and the `fn`. Verified empirically, since neither
  ffi.clj nor clj-kondo's docs say so outright."
  [raw-sym argtypes-node {:keys [params body]}]
  (api/list-node
   [params
    (api/list-node (list* (api/token-node 'clojure.core/let)
                          (api/vector-node [raw-sym (raw-fn-node argtypes-node)])
                          body))]))

(defn defcfn
  [{:keys [node]}]
  (let [[name-node & args] (rest (:children node))
        args (vec args)
        idx (csym-index args)]
    (if (nil? idx)
      ;; Shape def-cfn-form itself would reject (no C symbol found). Leave
      ;; the node alone rather than guess wrong.
      {:node node}
      (let [argtypes-node (nth args (inc idx) nil)
            rettype-node (nth args (+ idx 2) nil)]
        (if (or (nil? argtypes-node) (nil? rettype-node))
          {:node node}
          (let [after (vec (drop (+ idx 3) args))
                option? (and (seq after)
                             (let [f (first after)]
                               (or (api/keyword-node? f) (api/map-node? f))))
                tail (if option? (vec (rest after)) after)]
            {:node
             (if (empty? tail)
               (api/list-node [(api/token-node 'clojure.core/def) name-node (raw-fn-node argtypes-node)])
               (let [raw-sym (first tail)
                     fn-tail (vec (rest tail))]
                 (api/list-node
                  (list* (api/token-node 'clojure.core/defn) name-node
                         (map (partial wrapper-arity-node raw-sym argtypes-node)
                              (wrapper-arities fn-tail))))))}))))))

;; -- with-arena ------------------------------------------------------------

(defn with-arena
  "(with-arena [arena] body...) binds one lexical symbol with no paired
  value, unlike `let`'s [sym expr ...]. Rewrite it to `(let [arena nil]
  body...)`, which clj-kondo's own let handling then resolves correctly."
  [{:keys [node]}]
  (let [[binding-node & body] (rest (:children node))]
    (if (and (api/vector-node? binding-node) (= 1 (count (:children binding-node))))
      (let [sym (first (:children binding-node))]
        {:node (api/list-node
                (list* (api/token-node 'clojure.core/let)
                       (api/vector-node [sym (api/token-node nil)])
                       body))})
      {:node node})))
