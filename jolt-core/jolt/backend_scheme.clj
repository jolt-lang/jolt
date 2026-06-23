(ns jolt.backend-scheme
  "Lowers the host-neutral IR (jolt.ir) to Chez Scheme source text.

  The analyzer produces IR; this emitter turns each IR op into a string of Scheme
  source, which the host compiles with (eval (read ...)). It depends only on
  clojure.core and clojure.string, so once cross-compiled it runs on Chez and can
  emit its own code — the bootstrap spine. Quoted forms are walked through the
  portable jolt.host form-* contract, the same seam the analyzer uses, so the
  emitter never touches a concrete host representation directly."
  (:require [clojure.string :as str]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-sym-meta
                               form-list? form-vec? form-map? form-set? form-char?
                               form-literal? form-elements form-vec-items
                               form-map-pairs form-set-items form-char-code]]))

;; Hot clojure.core primitives lowered to native Scheme.
;; `=` is the exactness-aware jolt= from values.ss; inc/dec/
;; not are rt shims; mod/rem/quot map to Scheme's (Scheme has all three).
(def ^:private native-ops
  {"+" "+" "-" "-" "*" "*" "/" "/"
   "<" "<" ">" ">" "<=" "<=" ">=" ">="
   "=" "jolt=" "inc" "jolt-inc" "dec" "jolt-dec" "not" "jolt-not"
   "min" "min" "max" "max"
   "mod" "modulo" "rem" "remainder" "quot" "quotient"
   "vector" "jolt-vector" "hash-map" "jolt-hash-map" "hash-set" "jolt-hash-set"
   "conj" "jolt-conj" "get" "jolt-get" "nth" "jolt-nth" "count" "jolt-count"
   "assoc" "jolt-assoc" "dissoc" "jolt-dissoc" "contains?" "jolt-contains?"
   "empty?" "jolt-empty?" "peek" "jolt-peek" "pop" "jolt-pop"
   "first" "jolt-first" "rest" "jolt-rest" "next" "jolt-next" "seq" "jolt-seq"
   "cons" "jolt-cons" "list" "jolt-list" "reverse" "jolt-reverse" "last" "jolt-last"
   "map" "jolt-map" "filter" "jolt-filter" "remove" "jolt-remove"
   "reduce" "jolt-reduce" "into" "jolt-into" "concat" "jolt-concat" "apply" "jolt-apply"
   "range" "jolt-range" "take" "jolt-take" "drop" "jolt-drop"
   "keys" "jolt-keys" "vals" "jolt-vals"
   "even?" "jolt-even?" "odd?" "jolt-odd?" "pos?" "jolt-pos?" "neg?" "jolt-neg?"
   "zero?" "jolt-zero?" "identity" "jolt-identity"
   "ex-info" "jolt-ex-info"})

;; Value-position resolution for a clojure.core ref passed AS A VALUE (to map /
;; filter / reduce / apply). Arithmetic is the exception — Scheme's +/-/*// return
;; EXACT results for exact/zero-arg inputs, breaking the all-double model in
;; higher-order use, so value-position arithmetic routes to the flonum wrappers.
(def ^:private core-value-procs
  (merge native-ops {"+" "jolt-add" "-" "jolt-sub" "*" "jolt-mul" "/" "jolt-div"}))

;; Per-op arity gate: only lower when the Scheme prim and the jolt fn agree at
;; this arity. Ops absent from the table are variadic (legal at any arity).
(def ^:private op-arity
  {"inc" #(= % 1) "dec" #(= % 1) "not" #(= % 1)
   "count" #(= % 1) "empty?" #(= % 1) "peek" #(= % 1) "pop" #(= % 1)
   "mod" #(= % 2) "rem" #(= % 2) "quot" #(= % 2) "contains?" #(= % 2)
   "get" #(or (= % 2) (= % 3)) "nth" #(or (= % 2) (= % 3))
   "assoc" #(and (>= % 3) (odd? %)) "dissoc" #(>= % 1) "conj" #(>= % 1)
   "first" #(= % 1) "rest" #(= % 1) "next" #(= % 1) "seq" #(= % 1)
   "reverse" #(= % 1) "last" #(= % 1) "keys" #(= % 1) "vals" #(= % 1)
   "even?" #(= % 1) "odd?" #(= % 1) "pos?" #(= % 1) "neg?" #(= % 1)
   "zero?" #(= % 1) "identity" #(= % 1)
   "cons" #(= % 2) "filter" #(= % 2) "remove" #(= % 2) "into" #(= % 2)
   "take" #(= % 2) "drop" #(= % 2) "map" #(>= % 2) "apply" #(>= % 2)
   "reduce" #(or (= % 2) (= % 3)) "range" #(and (>= % 0) (<= % 3))
   "ex-info" #(or (= % 2) (= % 3))})

;; jolt's comparison ops are vacuously true at arity 1 and DON'T inspect the arg,
;; but Scheme's < demands a number even there — special-case.
(def ^:private cmp1-ops #{"<" ">" "<=" ">="})

;; Host interop methods with a Chez RT shim (rt.ss jolt-host-call). A `.method`
;; call on any other method routes to record-method-dispatch (a reify/record
;; protocol method).
(def ^:private supported-host-methods #{"isDirectory" "listFiles"})

;; Native-op Scheme procedures that return a genuine Scheme boolean (#t/#f), so an
;; :if test built from them needs no jolt-truthy? wrapper.
(def ^:private bool-returning-ops
  #{"<" "<=" ">" ">=" "jolt=" "jolt-not"
    "jolt-even?" "jolt-odd?" "jolt-pos?" "jolt-neg?"
    "jolt-zero?" "jolt-empty?" "jolt-contains?"})

;; PRELUDE MODE. The default (subset) mode rejects any clojure.core ref
;; that isn't a native-op — a clean "out of subset" signal for user-facing `-e`.
;; When emitting clojure.core ITSELF as a prelude, core fns reference each other
;; constantly; those lower to var-deref (resolved at runtime).
(def prelude-mode? (atom false))
(defn set-prelude-mode! [on] (reset! prelude-mode? on))

;; recur-target and the set of munged local names known to hold a procedure (a
;; named fn's self-recursion name) are lexically scoped — dynamic vars so the
;; recursion auto-restores them (no manual save/restore, no throw-leak).
(def ^:dynamic *recur-target* nil)
(def ^:dynamic *known-procs* #{})

(def ^:private gensym-counter (atom 0))
(defn- fresh-label [prefix] (str prefix (swap! gensym-counter inc)))

;; Scheme syntactic keywords. A jolt local with one of these names would, when
;; emitted verbatim, shadow the Scheme form in operator position (a local named
;; `if` would turn the special form (if …) the back end emits into a call), so
;; such locals are prefixed. Matches the spec: special-form heads are not
;; shadowable, but a value local may legally be named `if`.
(def ^:private scheme-reserved
  #{"if" "begin" "lambda" "let" "let*" "letrec" "letrec*" "quote" "quasiquote"
    "unquote" "set!" "define" "define-syntax" "cond" "case" "when" "unless"
    "and" "or" "do" "else" "guard" "parameterize" "delay" "values"})

;; Most jolt names are already valid Scheme identifiers. The one that isn't is
;; `#`, which jolt auto-gensyms use as a suffix (p1__0000X4# from #(...)) — `#`
;; starts a datum in Scheme, so replace it with `_`. A name that collides with a
;; Scheme keyword is prefixed with `_` so it can never shadow the emitted form.
(defn- munge-name [s]
  ;; A Clojure symbol may contain chars that break a Scheme identifier: ' is the
  ;; quote reader macro (a bare f' would read as f then 'rest), # already maps to
  ;; _. Munge both to safe tokens; the same mapping applies at the binding and at
  ;; every reference, so resolution stays consistent.
  (let [s (-> s
              (str/replace "#" "_")
              (str/replace "'" "_PRIME_"))]
    (if (contains? scheme-reserved s) (str "_" s) s)))

(declare emit)

;; A Chez string literal. Every char outside printable ASCII becomes a
;; codepoint hex escape \x<cp>; ; the named escapes (\n \t \r \" \\) match what
;; Chez's reader accepts. For pure printable ASCII this is byte-identical to %j.
(defn- char-escape [cp]
  (cond
    (= cp 34) "\\\""
    (= cp 92) "\\\\"
    (= cp 10) "\\n"
    (= cp 9) "\\t"
    (= cp 13) "\\r"
    (and (>= cp 32) (< cp 127)) (str (char cp))
    :else (str "\\x" (format "%x" cp) ";")))

(defn- chez-str-lit [s]
  (str "\"" (apply str (map (fn [c] (char-escape (int c))) s)) "\""))

(defn- emit-const [v]
  (cond
    (nil? v) "jolt-nil"
    (boolean? v) (if v "#t" "#f")
    ;; Numeric tower: emit a literal Chez re-reads as the SAME number.
    ;; Exact integers -> "42", exact ratios -> "1/2" (str renders both faithfully);
    ;; a flonum must carry a decimal point/exponent or Chez reads it back as exact,
    ;; so a whole flonum (str drops its .0) gets ".0" appended. ##Inf/##-Inf/##NaN
    ;; -> Chez's flonum literals.
    (number? v) (cond
                  (= v ##Inf) "+inf.0"
                  (= v ##-Inf) "-inf.0"
                  (not= v v) "+nan.0"
                  (float? v) (let [s (str v)]
                               (if (or (str/includes? s ".") (str/includes? s "e")) s (str s ".0")))
                  :else (str v))
    (string? v) (chez-str-lit v)
    ;; keyword literal -> (keyword ns name)
    (keyword? v) (if-let [kns (namespace v)]
                   (str "(keyword " (chez-str-lit kns) " " (chez-str-lit (name v)) ")")
                   (str "(keyword #f " (chez-str-lit (name v)) ")"))
    ;; char literal -> (integer->char <codepoint>). Get the codepoint via the host
    ;; contract (form-char-code), NOT (get v :ch): on Chez (the self-hosted spine)
    ;; a char is a native char, so a struct-field read returns nil and would emit
    ;; (integer->char) with no arg.
    (form-char? v) (str "(integer->char " (form-char-code v) ")")
    :else (throw (ex-info (str "emit-const: unsupported literal " (pr-str v)) {}))))

;; Emit a call `(ctor a0 a1 ...)` with the args evaluated LEFT-TO-RIGHT. Chez's
;; procedure-argument evaluation order is unspecified (in practice right-to-left),
;; but Clojure evaluates collection-literal elements left to right, so a literal
;; like [(read r) (read r)] over side-effecting reads must bind in source order.
;; Bind each arg to a fresh temp in a let* then construct. Only wraps at >= 2 args.
(defn- emit-ordered [ctor arg-strs]
  (if (< (count arg-strs) 2)
    (str "(" ctor (if (empty? arg-strs) "" (str " " (str/join " " arg-strs))) ")")
    (let [tmps (map (fn [_] (fresh-label "_o$")) arg-strs)
          binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps arg-strs))]
      (str "(let* (" binds ") (" ctor " " (str/join " " tmps) "))"))))

;; An operand whose evaluation has no observable effect and whose result doesn't
;; depend on when it runs: constants, locals, var/the-var reads, quoted literals.
;; Re-ordering such operands relative to others is invisible.
(defn- side-effect-free? [n]
  (contains? #{:const :local :var :the-var :quote} (:op n)))

;; Clojure evaluates a call's operands (and recur's args) left to right; Chez's
;; application order is unspecified (right-to-left in practice). Force source
;; order by binding operands to fresh temps in a let* — but only when two or more
;; could have observable effects, so hot calls over locals/consts stay un-wrapped.
(defn- needs-order? [nodes]
  (> (count (remove side-effect-free? nodes)) 1))

;; Build a call from operand strings, forcing left-to-right evaluation when
;; needed. `nodes`/`strs` are the operands (parallel); `build` receives the
;; operand strings to splice (temps when wrapped, raw otherwise) and returns the
;; call. Operands that don't need ordering are passed through inline.
(defn- ordered-call [nodes strs build]
  (if (needs-order? nodes)
    (let [tmps (mapv (fn [_] (fresh-label "_a$")) strs)
          binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps strs))]
      (str "(let* (" binds ") " (build tmps) ")"))
    (build strs)))

;; Quoted literals. A :quote node's :form is the RAW reader form;
;; reconstruct each as the matching Chez RT constructor — the runtime value of a
;; quote is just that literal data. The form is walked via the jolt.host form-*
;; contract (the portable seam the analyzer uses), NOT host-native predicates, so
;; this stays host-neutral — the contract walks the host's reader forms.
(declare emit-quoted)
(defn- emit-quoted-map [pairs]
  ;; pairs: a jolt vector of [k-form v-form] pairs (form-map-pairs)
  (str "(jolt-hash-map "
       (str/join " " (mapcat (fn [p] [(emit-quoted (nth p 0)) (emit-quoted (nth p 1))]) pairs))
       ")"))
(defn- emit-quoted-map-value [m]
  ;; a jolt map VALUE (def/symbol metadata is a value, not a reader form)
  (str "(jolt-hash-map "
       (str/join " " (mapcat (fn [k] [(emit-quoted k) (emit-quoted (get m k))]) (keys m)))
       ")"))
;; emit-quoted reconstructs both raw reader forms (from :quote) AND plain jolt
;; values (def/symbol :meta). Reader forms are walked via the jolt.host form-*
;; contract; the native-predicate branches below catch genuine jolt collection
;; VALUES. The form-* branches come first so a reader form (a host-native struct/
;; array that a native predicate might also match) is always handled as a form.
(defn- emit-quoted [form]
  (cond
    (form-char? form) (emit-const form)
    (form-literal? form) (emit-const form)
    (form-sym? form)
    (let [m (form-sym-meta form) sns (form-sym-ns form) nm (form-sym-name form)]
      (if (and m (pos? (count m)))
        ;; carry reader metadata (^:foo bar) onto the quoted symbol so (meta 'x) sees it
        (str "(jolt-symbol/meta " (if sns (chez-str-lit sns) "#f") " " (chez-str-lit nm) " "
             (emit-quoted m) ")")
        (str "(jolt-symbol " (if sns (chez-str-lit sns) "#f") " " (chez-str-lit nm) ")")))
    (form-set? form) (str "(jolt-hash-set " (str/join " " (map emit-quoted (form-set-items form))) ")")
    (form-list? form) (str "(jolt-list " (str/join " " (map emit-quoted (form-elements form))) ")")
    (form-vec? form) (str "(jolt-vector " (str/join " " (map emit-quoted (form-vec-items form))) ")")
    (form-map? form) (emit-quoted-map (form-map-pairs form))
    ;; plain jolt VALUES (metadata maps and anything nested in them)
    (map? form) (emit-quoted-map-value form)
    (vector? form) (str "(jolt-vector " (str/join " " (map emit-quoted form)) ")")
    (set? form) (str "(jolt-hash-set " (str/join " " (map emit-quoted form)) ")")
    (seq? form) (str "(jolt-list " (str/join " " (map emit-quoted form)) ")")
    :else (throw (ex-info (str "emit-quoted: unsupported quoted form " (pr-str form)) {}))))

;; A def's :meta is a jolt map value. Non-empty? (a plain def carries {}).
(defn- jmeta-nonempty? [m] (and (map? m) (pos? (count m))))

(defn- emit-binding [b]
  (str "(" (munge-name (nth b 0)) " " (emit (nth b 1)) ")"))

;; letfn lowers to a :let flagged :letrec (mutually-recursive named local fns):
;; Scheme `letrec*` binds them so each sees its siblings. A plain let uses let*.
(defn- emit-let [node]
  (let [kw (if (:letrec node) "letrec*" "let*")]
    (str "(" kw " (" (str/join " " (map emit-binding (:bindings node))) ") "
         (emit (:body node)) ")")))

(defn- emit-loop [node]
  (let [label (fresh-label "loop")
        pairs (:bindings node)
        names (map #(munge-name (nth % 0)) pairs)
        ;; inits evaluate in the OUTER scope (recur-target unchanged) and, like
        ;; Clojure loop/let, SEQUENTIALLY — wrap a let* around the named let.
        inits (map #(emit (nth % 1)) pairs)
        seq-bs (str/join " " (map (fn [n i] (str "(" n " " i ")")) names inits))
        rebinds (str/join " " (map (fn [n] (str "(" n " " n ")")) names))
        body (binding [*recur-target* label] (emit (:body node)))]
    (str "(let* (" seq-bs ") (let " label " (" rebinds ") " body "))")))

;; jolt.ffi/__cfn -> a Chez foreign-procedure (jolt-ffi). The C symbol + types are
;; compile-time literals from the analyzer, so this emits a real typed binding;
;; the resulting Scheme procedure is callable like any jolt fn. The library must
;; have loaded the shared object (jolt.ffi/load-library) before this def runs.
(def ^:private ffi-types
  {"int" "int" "uint" "unsigned-int" "long" "long" "ulong" "unsigned-long"
   "int64" "integer-64" "uint64" "unsigned-64" "size_t" "size_t" "ssize_t" "ssize_t"
   "iptr" "iptr" "uptr" "uptr" "double" "double" "float" "float"
   "pointer" "void*" "void*" "void*" "string" "string" "void" "void"
   "uint8" "unsigned-8" "u8" "unsigned-8" "byte" "unsigned-8" "char" "char"})
(defn- ffi-type->chez [t]
  (or (ffi-types t) (throw (ex-info (str "jolt.ffi: unknown foreign type :" t) {}))))
(defn- emit-ffi-fn [node]
  (str "(foreign-procedure " (when (:blocking node) "__collect_safe ") (chez-str-lit (:csym node))
       " (" (str/join " " (map ffi-type->chez (:argtypes node))) ") "
       (ffi-type->chez (:rettype node)) ")"))

(defn- emit-recur [node]
  (when-not *recur-target* (throw (ex-info "emit: recur outside a loop/fn target" {})))
  (let [arg-nodes (:args node)]
    (ordered-call arg-nodes (mapv emit arg-nodes)
                  (fn [as] (str "(" *recur-target* " " (str/join " " as) ")")))))

;; One arity -> a Scheme lambda param-list + a named-let-wrapped body. The named
;; let lets fn-level `recur` rebind this arity's params. A variadic arity takes a
;; Scheme rest arg coerced to a jolt seq (nil when empty); recur carries the rest
;; seq directly, and the named let's init only runs on first entry.
(defn- emit-arity-clause [a]
  (let [params (map munge-name (:params a))
        restp (when-let [r (:rest a)] (munge-name r))
        label (fresh-label "fnrec")
        body (binding [*recur-target* label] (emit (:body a)))
        paramlist (cond
                    (and restp (empty? params)) restp
                    restp (str "(" (str/join " " params) " . " restp ")")
                    :else (str "(" (str/join " " params) ")"))
        binds (if restp
                (concat (map (fn [p] (str "(" p " " p ")")) params)
                        [(str "(" restp " (list->cseq " restp "))")])
                (map (fn [p] (str "(" p " " p ")")) params))]
    [paramlist (str "(let " label " (" (str/join " " binds) ") " body ")")]))

(defn- emit-fn [node]
  (let [arities (:arities node)
        ;; a named fn binds its own name as a known-procedure local across ALL
        ;; arities, so self-calls emit directly rather than via jolt-invoke.
        self (when-let [nm (:name node)] (munge-name nm))
        clauses (binding [*known-procs* (if self (conj *known-procs* self) *known-procs*)]
                  (mapv emit-arity-clause arities))
        lambda (if (= 1 (count clauses))
                 (let [c (first clauses)] (str "(lambda " (nth c 0) " " (nth c 1) ")"))
                 (str "(case-lambda "
                      (str/join " " (map (fn [c] (str "(" (nth c 0) " " (nth c 1) ")")) clauses))
                      ")"))]
    ;; A named fn references itself by name — the analyzer binds that name as a
    ;; :local in the body. letrec makes the name visible to the lambda.
    (if-let [nm (:name node)]
      (let [m (munge-name nm)] (str "(letrec ((" m " " lambda ")) " m ")"))
      lambda)))

;; If fnode is a clojure.core (or host) ref to a native-op primitive, return the
;; Scheme op string — only at an arity where the Scheme op and the jolt fn agree.
(defn- native-op [fnode nargs]
  (let [nm (case (:op fnode)
             :var (when (= "clojure.core" (:ns fnode)) (:name fnode))
             :host (:name fnode)
             nil)
        op (when nm (native-ops nm))
        arity-ok (when nm (op-arity nm))]
    (cond
      (nil? op) nil
      (and arity-ok (not (arity-ok nargs))) nil
      :else op)))

;; IFn dispatch for a LITERAL callee (Clojure's "value as fn"): a keyword looks
;; itself up in its arg; a map/set/vector literal looks up its arg.
(defn- ifn-kind [fnode]
  (case (:op fnode)
    :const (when (keyword? (:val fnode)) :keyword)
    (:map :set :vector) :coll
    nil))

;; A reference into the Clojure stdlib (clojure.*) with no impl on Chez yet.
(defn- stdlib-var? [n]
  (and (= :var (:op n)) (str/starts-with? (or (:ns n) "") "clojure.")))

(defn- emit-invoke [node]
  (let [fnode (:fn node)
        arg-nodes (:args node)
        args (mapv emit arg-nodes)
        nop (native-op fnode (count args))
        kind (ifn-kind fnode)
        ;; order args left-to-right (build receives the spliced operand strings)
        order-args (fn [build] (ordered-call arg-nodes args build))
        defstr (fn [as] (if (> (count as) 1) (str " " (nth as 1)) ""))
        ;; jolt-invoke dispatch: Clojure evaluates the fn expr before the args, so
        ;; order [callee & args] together when ordering is observable.
        invoke (fn []
                 (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                               (fn [[f & as]]
                                 (str "(jolt-invoke " f (if (seq as) (str " " (str/join " " as)) "") ")"))))]
    (cond
      ;; zero-arg + / * : exact integer identity (= JVM long: (+) -> 0, (*) -> 1).
      (and nop (empty? args) (= nop "+")) "0"
      (and nop (empty? args) (= nop "*")) "1"
      (and nop (= 1 (count args)) (cmp1-ops nop)) (str "(begin " (first args) " #t)")
      nop (order-args (fn [as] (str "(" nop " " (str/join " " as) ")")))
      ;; (:k coll [default]) -> (jolt-get coll :k [default]) — the key (fnode) is a
      ;; const, so only the coll/default args carry order.
      (= kind :keyword)
      (order-args (fn [as] (str "(jolt-get " (first as) " " (emit fnode) (defstr as) ")")))
      ;; (coll k [default]) -> (jolt-get coll k [default]) — coll (fnode) is the
      ;; callee, evaluated before the key/default args.
      (= kind :coll)
      (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                    (fn [[c & as]] (str "(jolt-get " c " " (str/join " " as) ")")))
      (and (stdlib-var? fnode) (not (deref prelude-mode?)))
      (throw (ex-info (str "emit: unsupported stdlib fn `" (:ns fnode) "/" (:name fnode)
                           "` (no core on Chez yet)") {}))
      ;; static method call (Class/method arg*) -> (host-static-call ...).
      (= :host-static (:op fnode))
      (order-args (fn [as]
                    (str "(host-static-call " (chez-str-lit (:class fnode)) " " (chez-str-lit (:member fnode))
                         (if (empty? as) "" (str " " (str/join " " as))) ")")))
      (= :host (:op fnode))
      (throw (ex-info (str "emit: unsupported host call `" (:name fnode) "`") {}))
      ;; a :local callee that isn't a known procedure -> dynamic IFn dispatch.
      (and (= :local (:op fnode)) (not (*known-procs* (munge-name (:name fnode)))))
      (invoke)
      ;; a late-bound :var call head can hold a procedure OR a non-applicable
      ;; value the RT dispatches (multimethod, keyword/coll IFn) — route via
      ;; jolt-invoke (transparent for a procedure).
      (= :var (:op fnode))
      (invoke)
      ;; a computed callee can yield ANY IFn — route through jolt-invoke.
      :else
      (invoke))))

;; try/catch/finally. throw raises the jolt value RAW (jolt-throw =
;; Scheme `raise`); catch lowers to `guard` with an `else` clause (the IR drops
;; the class), finally to `dynamic-wind`'s after-thunk (runs on success, catch and
;; escape — Clojure finally semantics). Both keys optional on the node.
(defn- emit-try [node]
  (let [core (if-let [cs (:catch-sym node)]
               (str "(guard (" (munge-name cs) " (else " (emit (:catch-body node)) ")) "
                    (emit (:body node)) ")")
               (emit (:body node)))]
    (if-let [fin (:finally node)]
      (str "(dynamic-wind (lambda () #f) (lambda () " core ") (lambda () " (emit fin) "))")
      core)))

;; Does this IR node emit to an expression that yields a Scheme boolean? Used to
;; drop the redundant jolt-truthy? on an :if test.
(defn- returns-scheme-bool? [node]
  (cond
    (and (= :const (:op node)) (boolean? (:val node))) true
    (= :invoke (:op node))
    (let [nop (native-op (:fn node) (count (:args node)))]
      (if (and nop (bool-returning-ops nop)) true false))
    :else false))

(defn emit [node]
  (case (:op node)
    :const (emit-const (:val node))
    :local (munge-name (:name node))
    ;; late-bound var: read the cell's current root at use time. A value-position
    ;; ref to a clojure.core fn the RT provides lowers to the RT procedure.
    :var (let [core-proc (and (= "clojure.core" (:ns node)) (core-value-procs (:name node)))]
           (cond
             core-proc core-proc
             (and (stdlib-var? node) (not (deref prelude-mode?)))
             (throw (ex-info (str "emit: unsupported stdlib ref `" (:ns node) "/" (:name node)
                                  "` (no core on Chez yet)") {}))
             :else (str "(var-deref " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")")))
    :the-var (str "(jolt-var " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")")
    ;; (set! *var* val) -> set the var's innermost binding (else root); returns val.
    :set-var (str "(jolt-var-set " (emit (:the-var node)) " " (emit (:val node)) ")")
    ;; (set! (.-field obj) val) -> mutate the deftype instance field in place.
    :set-field (str "(jolt-set-field! " (emit (:obj node)) " (keyword #f "
                    (chez-str-lit (:field node)) ") " (emit (:val node)) ")")
    ;; a non-top-level defmacro -> def the expander fn + mark the var a macro at
    ;; runtime (the spine does the same for top-level forms).
    :defmacro (str "(begin (def-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                   (emit (:fn node)) ") (mark-macro! " (chez-str-lit (:ns node)) " "
                   (chez-str-lit (:name node)) ") jolt-nil)")
    :host (throw (ex-info (str "emit: unsupported host ref `" (:name node) "`") {}))
    :host-static (str "(host-static-ref " (chez-str-lit (:class node)) " "
                      (chez-str-lit (:member node)) ")")
    :host-new (str "(host-new " (chez-str-lit (:class node))
                   (let [args (map emit (:args node))]
                     (if (empty? args) "" (str " " (str/join " " args)))) ")")
    :if (let [test (:test node)
              t (if (returns-scheme-bool? test) (emit test)
                    (str "(jolt-truthy? " (emit test) ")"))]
          (str "(if " t " " (emit (:then node)) " " (emit (:else node)) ")"))
    :do (str "(begin " (str/join " " (map emit (:statements node)))
             (if (empty? (:statements node)) "" " ") (emit (:ret node)) ")")
    :invoke (emit-invoke node)
    ;; collection literals -> rt constructors (collections.ss). Elements are
    ;; already-analyzed IR nodes; evaluate LEFT-TO-RIGHT (emit-ordered).
    :vector (emit-ordered "jolt-vector" (map emit (:items node)))
    :set (emit-ordered "jolt-hash-set" (map emit (:items node)))
    :map (emit-ordered "jolt-hash-map"
                       (mapcat (fn [p] [(emit (nth p 0)) (emit (nth p 1))]) (:pairs node)))
    :quote (emit-quoted (:form node))
    :throw (str "(jolt-throw " (emit (:expr node)) ")")
    :try (emit-try node)
    ;; regex literal #"…" -> a jolt-regex value (regex.ss, vendored irregex).
    :regex (str "(jolt-regex " (chez-str-lit (:source node)) ")")
    ;; #inst / #uuid literals -> runtime inst / uuid values.
    :inst (str "(jolt-inst-from-string " (chez-str-lit (:source node)) ")")
    :uuid (str "(jolt-uuid-from-string " (chez-str-lit (:source node)) ")")
    ;; bigdecimal literal (1.5M) -> a runtime jbigdec from its numeric text.
    :bigdec (str "(jolt-bigdec-from-string " (chez-str-lit (:source node)) ")")
    ;; a namespace value spliced into a form (~*ns*) -> reconstruct by name.
    :the-ns (str "(intern-ns! " (chez-str-lit (:name node)) ")")
    ;; (.method target arg*) -> jolt-host-call for an rt-shimmed method, else
    ;; record-method-dispatch (a reify/record protocol method).
    :host-call (let [m (:method node)
                     target (emit (:target node))
                     args (map emit (:args node))]
                 (if (supported-host-methods m)
                   (str "(jolt-host-call " (chez-str-lit m) " " target
                        (if (empty? args) "" (str " " (str/join " " args))) ")")
                   (str "(record-method-dispatch " target " " (chez-str-lit m)
                        " (jolt-vector" (if (empty? args) "" (str " " (str/join " " args))) "))")))
    :let (emit-let node)
    :loop (emit-loop node)
    :recur (emit-recur node)
    :ffi-fn (emit-ffi-fn node)
    :fn (emit-fn node)
    ;; (def name) with no init (declare): reserve the cell. A def with non-empty
    ;; reader metadata lowers to def-var-with-meta! (ported in a later increment).
    :def (cond
           (:no-init node)
           (str "(declare-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")")
           (jmeta-nonempty? (:meta node))
           (str "(def-var-with-meta! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                (emit (:init node)) " " (emit-quoted (:meta node)) ")")
           :else
           (str "(def-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                (emit (:init node)) ")"))
    (throw (ex-info (str "emit: op not yet ported / unhandled: " (pr-str (:op node))) {}))))
