# Jolt Compiler
# Source-to-source: Clojure forms → Janet source
# Two-phase: analyze-form (classify) → emit-ast (generate)
#
# When ctx is passed to analyze-form, macros are expanded at analyze time.

(use ./types)
(use ./core)

(def- core-renames
  @{"+" "core-+"
    "-" "core-sub"
    "*" "core-*"
    "/" "core-/"
    "inc" "core-inc"
    "dec" "core-dec"
    "=" "core-="
    "not=" "core-not="
    "<" "core-<"
    ">" "core->"
    "<=" "core-<="
    ">=" "core->="
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
    "some" "core-some"
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
      (= name "new") (= name "deftype") (= name "instance?")
      (= name "defmulti") (= name "defmethod") (= name "locking")))

# ============================================================
# Macro resolution
# ============================================================

(defn- resolve-macro
  [ctx sym-s]
  (when ctx
    (let [name (sym-s :name)
          ns-sym (sym-s :ns)]
      (if ns-sym
        (let [target-ns (ctx-find-ns ctx ns-sym)
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

# ============================================================
# Core function value lookup
# ============================================================

(def- core-fn-values
  (let [t @{}]
    (put t "core-+" core-+)
    (put t "core-sub" core-sub)
    (put t "core-*" core-*)
    (put t "core-/" core-/)
    (put t "core-inc" core-inc)
    (put t "core-dec" core-dec)
    (put t "core-=" core-=)
    (put t "core-not=" core-not=)
    (put t "core-<" core-<)
    (put t "core->" core->)
    (put t "core-<=" core-<=)
    (put t "core->=" core->=)
    (put t "core-nil?" core-nil?)
    (put t "core-not" core-not)
    (put t "core-some?" core-some?)
    (put t "core-string?" core-string?)
    (put t "core-number?" core-number?)
    (put t "core-fn?" core-fn?)
    (put t "core-keyword?" core-keyword?)
    (put t "core-symbol?" core-symbol?)
    (put t "core-vector?" core-vector?)
    (put t "core-map?" core-map?)
    (put t "core-seq?" core-seq?)
    (put t "core-coll?" core-coll?)
    (put t "core-true?" core-true?)
    (put t "core-false?" core-false?)
    (put t "core-identical?" core-identical?)
    (put t "core-zero?" core-zero?)
    (put t "core-pos?" core-pos?)
    (put t "core-neg?" core-neg?)
    (put t "core-even?" core-even?)
    (put t "core-odd?" core-odd?)
    (put t "core-empty?" core-empty?)
    (put t "core-every?" core-every?)
    (put t "core-first" core-first)
    (put t "core-rest" core-rest)
    (put t "core-next" core-next)
    (put t "core-cons" core-cons)
    (put t "core-conj" core-conj)
    (put t "core-assoc" core-assoc)
    (put t "core-dissoc" core-dissoc)
    (put t "core-get" core-get)
    (put t "core-get-in" core-get-in)
    (put t "core-contains?" core-contains?)
    (put t "core-count" core-count)
    (put t "core-seq" core-seq)
    (put t "core-vec" core-vec)
    (put t "core-map" core-map)
    (put t "core-filter" core-filter)
    (put t "core-remove" core-remove)
    (put t "core-reduce" core-reduce)
    (put t "core-str" core-str)
    (put t "core-prn" core-prn)
    (put t "core-println" core-println)
    (put t "core-print" core-print)
    (put t "core-identity" core-identity)
    (put t "core-comp" core-comp)
    (put t "core-partial" core-partial)
    (put t "core-complement" core-complement)
    (put t "core-constantly" core-constantly)
    (put t "core-memoize" core-memoize)
    (put t "core-range" core-range)
    (put t "core-take" core-take)
    (put t "core-drop" core-drop)
    (put t "core-take-while" core-take-while)
    (put t "core-drop-while" core-drop-while)
    (put t "core-reverse" core-reverse)
    (put t "core-into" core-into)
    (put t "core-merge" core-merge)
    (put t "core-merge-with" core-merge-with)
    (put t "core-keys" core-keys)
    (put t "core-vals" core-vals)
    (put t "core-zipmap" core-zipmap)
    (put t "core-select-keys" core-select-keys)
    (put t "core-max" core-max)
    (put t "core-min" core-min)
    (put t "core-quot" core-quot)
    (put t "core-rem" core-rem)
    (put t "core-mod" core-mod)
    (put t "core-apply" apply)
    (put t "core-some" core-some?)
    (put t "core-pr-str" core-pr-str)
    (put t "core-nth" core-nth)
    (put t "core-list" core-list)
    (put t "core-name" core-name)
    (put t "core-subs" core-subs)
    t))

# Loop counter for generating unique loop function names
(var loop-counter 0)

(defn- make-loop-name
  []
  (let [name (string "_loop_" loop-counter)]
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
            {:op :symbol :name name}))))

    (array? form)
    (let [first-form (first form)
          head-name (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
                     (first-form :name)
                     nil)]
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
                   {:op :do
                    :statements (array/slice analyzed 0 (- n 1))
                    :ret (in analyzed (- n 1))})
            "if" {:op :if
                  :test (analyze-form (in form 1) bindings ctx)
                  :then (analyze-form (in form 2) bindings ctx)
                  :else (if (> (length form) 3)
                         (analyze-form (in form 3) bindings ctx)
                         {:op :const :val nil})}
            "def" {:op :def
                   :name (in form 1)
                   :init (analyze-form (in form 2) bindings ctx)}
            "fn*" (let [params (in form 1)
                        body-bindings (do
                                        (var bb @{})
                                        (loop [[k v] :pairs bindings] (put bb k v))
                                        (each p params
                                          (put bb (if (struct? p) (p :name) p) :jolt/local))
                                        bb)
                        body-exprs (tuple/slice form 2)
                        analyzed-body (map |(analyze-form $ body-bindings ctx) body-exprs)
                        n-body (length analyzed-body)]
                    {:op :fn :params params
                     :body (if (> n-body 1)
                             {:op :do
                              :statements (array/slice analyzed-body 0 (- n-body 1))
                              :ret (last analyzed-body)}
                             (first analyzed-body))})
            "let*" (let [bind-vec (in form 1)
                         body-exprs (tuple/slice form 2)
                         binding-pairs (do
                                         (var pairs @[])
                                         (var i 0)
                                         (let [n (length bind-vec)]
                                           (while (< i n)
                                             (let [sym-s (in bind-vec i)
                                                   name (if (struct? sym-s) (sym-s :name) sym-s)
                                                   val-form (if (< (+ i 1) n) (in bind-vec (+ i 1)) nil)
                                                   val-ast (if val-form (analyze-form val-form bindings ctx) {:op :const :val nil})]
                                               (array/push pairs {:name name :init val-ast})
                                               (+= i 2))))
                                         pairs)
                         body-bindings (do
                                         (var bb @{})
                                         (loop [[k v] :pairs bindings] (put bb k v))
                                         (each bp binding-pairs
                                           (put bb (bp :name) :jolt/local))
                                         bb)
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
                          binding-pairs (do
                                          (var pairs @[])
                                          (var i 0)
                                          (let [n (length bind-vec)]
                                            (while (< i n)
                                              (let [sym-s (in bind-vec i)
                                                    name (if (struct? sym-s) (sym-s :name) sym-s)
                                                    val-form (if (< (+ i 1) n) (in bind-vec (+ i 1)) nil)
                                                    val-ast (if val-form (analyze-form val-form bindings ctx) {:op :const :val nil})]
                                                (array/push pairs {:name name :init val-ast})
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
    {:op :map :form form}

    {:op :const :val form}))

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

(defn- emit-fn-str [params body buf]
  (buffer/push buf "(fn [")
  (var i 0)
  (let [n (length params)]
    (while (< i n)
      (let [p (in params i)]
        (buffer/push buf (if (struct? p) (p :name) (string p))))
      (when (< (+ i 1) n) (buffer/push buf " "))
      (++ i)))
  (buffer/push buf "] ") (emit-ast body buf) (buffer/push buf ")"))

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

(defn- emit-map-str [form buf] (buffer/push buf (string form)))

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
      :local (emit-local-str (ast :name) buf)
      :core-symbol (emit-core-symbol-str (ast :janet-name) buf)
      :qualified-symbol (emit-qualified-symbol-str (ast :ns) (ast :name) buf)
      :do (emit-do-str (ast :statements) (ast :ret) buf)
      :if (emit-if-str (ast :test) (ast :then) (ast :else) buf)
      :def (emit-def-str (ast :name) (ast :init) buf)
      :fn (emit-fn-str (ast :params) (ast :body) buf)
      :let (emit-let-str (ast :binding-pairs) (ast :body) buf)
      :throw (emit-throw-str (ast :val) buf)
      :try (emit-try-str (ast :body) (ast :catch-sym) (ast :catch-body) (ast :finally-body) buf)
      :loop (emit-loop-str (ast :loop-name) (ast :param-names) (ast :init-vals) (ast :body) buf)
      :recur (emit-recur-str (ast :args) (ast :loop-name) buf)
      :invoke (emit-invoke-str (ast :fn) (ast :args) buf)
      :vector (emit-vector-str (ast :items) buf)
      :map (emit-map-str (ast :form) buf)
      :quote (emit-quote-str (ast :expr) buf)
      (buffer/push buf (string "/* unhandled op: " (ast :op) " */")))))

# ============================================================
# Emitter — AST → Janet data structure (for direct eval)
# ============================================================

(var emit-expr nil)

(defn- emit-const-expr [val] val)
(defn- emit-symbol-expr [name] (symbol name))
(defn- emit-local-expr [name] (symbol name))

(defn- emit-core-symbol-expr [janet-name]
  (or (get core-fn-values janet-name)
      (error (string "Core fn not found: " janet-name))))

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

(defn- emit-fn-expr [params body]
  (def param-syms @[])
  (each p params
    (array/push param-syms (symbol (if (struct? p) (p :name) p))))
  ['fn (tuple/slice (tuple ;param-syms)) (emit-expr body)])

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
  (def exprs @[(emit-expr f-ast)])
  (each arg args (array/push exprs (emit-expr arg)))
  (tuple/slice (tuple ;exprs)))

(defn- emit-vector-expr [items]
  (def exprs @['tuple])
  (each item items (array/push exprs (emit-expr item)))
  (tuple/slice (tuple ;exprs)))

(defn- emit-map-expr [form] form)

(defn- emit-quote-expr [expr]
  ['quote (raw-form->janet expr)])

(set emit-expr
  (fn [ast]
    (match (ast :op)
      :const (emit-const-expr (ast :val))
      :symbol (emit-symbol-expr (ast :name))
      :local (emit-local-expr (ast :name))
      :core-symbol (emit-core-symbol-expr (ast :janet-name))
      :qualified-symbol (emit-qualified-symbol-expr (ast :ns) (ast :name))
      :do (emit-do-expr (ast :statements) (ast :ret))
      :if (emit-if-expr (ast :test) (ast :then) (ast :else))
      :def (emit-def-expr (ast :name) (ast :init))
      :fn (emit-fn-expr (ast :params) (ast :body))
      :let (emit-let-expr (ast :binding-pairs) (ast :body))
      :throw (emit-throw-expr (ast :val))
      :try (emit-try-expr (ast :body) (ast :catch-sym) (ast :catch-body) (ast :finally-body))
      :loop (emit-loop-expr (ast :loop-name) (ast :param-names) (ast :init-vals) (ast :body))
      :recur (emit-recur-expr (ast :args) (ast :loop-name))
      :invoke (emit-invoke-expr (ast :fn) (ast :args))
      :vector (emit-vector-expr (ast :items))
      :map (emit-map-expr (ast :form))
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
  "Compile a Clojure form and evaluate it as Janet.
  For def/defn forms, interns the result in the Jolt namespace
  so the interpreter can resolve it later."
  [form ctx]
  (def result (eval (compile-ast form ctx)))
  # Intern def/defn results in Jolt namespace for interpreter resolution
  (when (and ctx (array? form) (> (length form) 0)
             (struct? (first form)) (= :symbol ((first form) :jolt/type)))
    (def head-name ((first form) :name))
    (when (or (= head-name "def") (= head-name "defn") (= head-name "defn-"))
      (def name-sym (in form 1))
      (def name (if (struct? name-sym) (name-sym :name) name-sym))
      (def ns (ctx-find-ns ctx (ctx-current-ns ctx)))
      (ns-intern ns name result)))
  result)
