# Phase 1 — jolt IR -> Chez Scheme emitter (jolt-cf1q.2).
#
# The new back end: consumes the SAME host-neutral IR (jolt.ir, see
# jolt-core/jolt/ir.clj) the live analyzer produces and the Janet backend
# consumes, but emits Scheme source text instead of Janet. `host/compile` (Chez
# `eval`) turns that into a procedure. Covers the pure-functional + numeric
# subset (const/local/var/host/if/do/let/fn/invoke/def/loop/recur) — enough to
# run fib/mandelbrot-shaped code through the REAL analyzer.
#
# IR access mirrors the Janet backend: live IR fields are jolt VALUES — vectors
# are persistent (pv), and a nil-valued node densifies to a phm. `nn`/`vv` below
# normalize both into Janet structs/arrays, so the same code drives hand-built
# IR (the unit tests) and live analyzer output (the driver).

(import ../../src/jolt/pv :as pv)
(import ../../src/jolt/phm :as phm)

# Normalize a node (phm -> struct) and a vector field (pvec -> array view); both
# pass plain Janet values through untouched, so hand-built IR still works.
(defn- nn [n] (if (phm/phm? n) (phm/phm-to-struct n) n))
(defn- vv [x] (if (pv/pvec? x) (pv/pv->array x) x))

# Hot clojure.core primitives lowered to native Scheme, mirroring the Janet
# backend's native-ops (documented numbers-only relaxation). `=` is the
# exactness-aware jolt= from values.ss; inc/dec/not are rt shims; mod/rem/quot
# map to Scheme's (correct: Scheme has all three, unlike Janet which lacked quot).
(def- native-ops
  {"+" "+" "-" "-" "*" "*" "/" "/"
   "<" "<" ">" ">" "<=" "<=" ">=" ">="
   "=" "jolt=" "inc" "jolt-inc" "dec" "jolt-dec" "not" "jolt-not"
   "min" "min" "max" "max"
   "mod" "modulo" "rem" "remainder" "quot" "quotient"
   # persistent-collection leaf ops (jolt-wgbz) -> rt prims in collections.ss
   "vector" "jolt-vector" "hash-map" "jolt-hash-map" "hash-set" "jolt-hash-set"
   "conj" "jolt-conj" "get" "jolt-get" "nth" "jolt-nth" "count" "jolt-count"
   "assoc" "jolt-assoc" "dissoc" "jolt-dissoc" "contains?" "jolt-contains?"
   "empty?" "jolt-empty?" "peek" "jolt-peek" "pop" "jolt-pop"
   # seq tier (jolt-5pso) -> rt prims in seq.ss
   "first" "jolt-first" "rest" "jolt-rest" "next" "jolt-next" "seq" "jolt-seq"
   "cons" "jolt-cons" "list" "jolt-list" "reverse" "jolt-reverse" "last" "jolt-last"
   "map" "jolt-map" "filter" "jolt-filter" "remove" "jolt-remove"
   "reduce" "jolt-reduce" "into" "jolt-into" "concat" "jolt-concat" "apply" "jolt-apply"
   "range" "jolt-range" "take" "jolt-take" "drop" "jolt-drop"
   "keys" "jolt-keys" "vals" "jolt-vals"
   "even?" "jolt-even?" "odd?" "jolt-odd?" "pos?" "jolt-pos?" "neg?" "jolt-neg?"
   "zero?" "jolt-zero?" "identity" "jolt-identity"
   # exceptions (jolt-vcsl): ex-info builds the tagged map; ex-data/ex-message/
   # ex-cause are pure-over-get Clojure tier fns (no native-op needed).
   "ex-info" "jolt-ex-info"})

# Value-position resolution for a clojure.core ref passed AS A VALUE (to map /
# filter / reduce / apply). Each native-op already names a usable Scheme
# procedure; arithmetic is the exception — Scheme's +/-/*// return EXACT results
# for exact/zero-arg inputs, breaking the all-double model in higher-order use,
# so value-position arithmetic routes to the flonum-coercing rt wrappers.
(def- core-value-procs
  (merge native-ops {"+" "jolt-add" "-" "jolt-sub" "*" "jolt-mul" "/" "jolt-div"}))

# Per-op arity gate: only lower when the Scheme prim and the jolt fn agree at
# this arity. Ops absent from the table are variadic (arith/compare/=, the
# collection constructors, conj/assoc/dissoc) and legal at any arity.
(def- op-arity
  {"inc" |(= $ 1) "dec" |(= $ 1) "not" |(= $ 1)
   "count" |(= $ 1) "empty?" |(= $ 1) "peek" |(= $ 1) "pop" |(= $ 1)
   "mod" |(= $ 2) "rem" |(= $ 2) "quot" |(= $ 2) "contains?" |(= $ 2)
   "get" |(or (= $ 2) (= $ 3)) "nth" |(or (= $ 2) (= $ 3))
   "assoc" |(and (>= $ 3) (odd? $)) "dissoc" |(>= $ 1) "conj" |(>= $ 1)
   # seq tier arities the shims support
   "first" |(= $ 1) "rest" |(= $ 1) "next" |(= $ 1) "seq" |(= $ 1)
   "reverse" |(= $ 1) "last" |(= $ 1) "keys" |(= $ 1) "vals" |(= $ 1)
   "even?" |(= $ 1) "odd?" |(= $ 1) "pos?" |(= $ 1) "neg?" |(= $ 1)
   "zero?" |(= $ 1) "identity" |(= $ 1)
   "cons" |(= $ 2) "filter" |(= $ 2) "remove" |(= $ 2) "into" |(= $ 2)
   "take" |(= $ 2) "drop" |(= $ 2) "map" |(>= $ 2) "apply" |(>= $ 2)
   "reduce" |(or (= $ 2) (= $ 3)) "range" |(and (>= $ 0) (<= $ 3))
   "ex-info" |(or (= $ 2) (= $ 3))})

# If fnode is a clojure.core (or host) ref to a native-op primitive, return the
# Scheme op string — only at an arity where the Scheme op and the jolt fn agree.
(defn- native-op [fnode nargs]
  (def nm (case (get fnode :op)
            :var (when (= "clojure.core" (get fnode :ns)) (get fnode :name))
            :host (get fnode :name)
            nil))
  (def op (and nm (get native-ops nm)))
  (def arity-ok (get op-arity nm))
  (cond
    (nil? op) nil
    (and arity-ok (not (arity-ok nargs))) nil
    op))

# PRELUDE MODE (inc 3d). The default (subset) mode rejects any clojure.core ref
# that isn't a native-op — a clean "out of subset" signal for user-facing `-e`.
# When emitting clojure.core ITSELF as a Scheme prelude, core fns reference each
# other constantly; those refs must lower to `var-deref` (resolved at runtime
# from the prelude's own def-var! forms) instead of being rejected. Host interop
# (:host) and unhandled IR ops still error in both modes — those are the real
# gaps that need a hand-written RT shim.
(var- prelude-mode? false)
(defn set-prelude-mode! [on] (set prelude-mode? on))

(var- recur-target nil)
# Munged local names known to hold a procedure (a named fn's self-recursion name).
# Calls to these stay DIRECT; any other :local callee routes through jolt-invoke
# (dynamic IFn dispatch) — keeps the fib self-call off the invoke fallback.
(def- known-procs @{})
(var- gensym-n 0)
(defn- fresh-label [prefix] (string prefix (++ gensym-n)))

# Emit a call `(ctor a0 a1 ...)` with the args evaluated LEFT-TO-RIGHT. Chez's
# procedure-argument evaluation order is unspecified (and in practice right-to-
# left), but Clojure/JVM evaluates collection-literal elements left-to-right, so a
# literal like [(read r) (read r)] over side-effecting reads must bind in source
# order. Bind each arg to a fresh temp in a let* (sequential) then construct. Only
# wraps when there are >= 2 args (0/1 have no ordering to preserve), keeping the
# common small-literal output compact (jolt-avt6).
(defn- emit-ordered [ctor arg-strs]
  (if (< (length arg-strs) 2)
    (string "(" ctor (if (empty? arg-strs) "" (string " " (string/join arg-strs " "))) ")")
    (let [tmps (map (fn [_] (fresh-label "_o$")) arg-strs)
          binds (string/join (map (fn [t a] (string "(" t " " a ")")) tmps arg-strs) " ")]
      (string "(let* (" binds ") (" ctor " " (string/join tmps " ") "))"))))

# Most jolt names are already valid Scheme identifiers (inc, even?, +, ->str all
# are — Scheme allows ! $ % & * + - . / : < = > ? @ ^ _ ~). The one that isn't is
# `#`, which jolt auto-gensyms use as a suffix (e.g. p1__0000X4# from #(...)
# shorthand) — `#` starts a datum in Scheme, so replace it with `_`.
(defn- munge [name] (string/replace-all "#" "_" name))

(var emit nil)   # forward declaration (mutual recursion with the helpers below)

# A Chez string literal (jolt-x0os). Janet's %j renders a non-ASCII char as raw
# UTF-8 bytes (\xC3\xA9) and a control char / DEL as \xHH with NO terminating
# semicolon — both forms Chez's reader rejects ("invalid character \ in string
# hex escape"). Emit a Chez string where every byte outside printable ASCII
# becomes a codepoint hex escape \x<cp>; (UTF-8 decoded for multibyte) and the
# named escapes (\n \t \r \" \\) match what both readers accept. For pure
# printable ASCII this is byte-identical to %j.
(defn- utf8-cp [s i]
  # decode the UTF-8 sequence starting at byte i -> [codepoint byte-length]
  (def b (in s i))
  (cond
    (< b 0x80) [b 1]
    (= (band b 0xE0) 0xC0) [(bor (blshift (band b 0x1F) 6)
                                 (band (in s (+ i 1)) 0x3F)) 2]
    (= (band b 0xF0) 0xE0) [(bor (blshift (band b 0x0F) 12)
                                 (blshift (band (in s (+ i 1)) 0x3F) 6)
                                 (band (in s (+ i 2)) 0x3F)) 3]
    (= (band b 0xF8) 0xF0) [(bor (blshift (band b 0x07) 18)
                                 (blshift (band (in s (+ i 1)) 0x3F) 12)
                                 (blshift (band (in s (+ i 2)) 0x3F) 6)
                                 (band (in s (+ i 3)) 0x3F)) 4]
    [b 1]))   # malformed lead byte: pass through one byte
(defn- chez-str-lit [s]
  (def out @"\"")
  (def n (length s))
  (var i 0)
  (while (< i n)
    (def b (in s i))
    (cond
      (= b 0x22) (do (buffer/push-string out "\\\"") (++ i))
      (= b 0x5C) (do (buffer/push-string out "\\\\") (++ i))
      (= b 0x0A) (do (buffer/push-string out "\\n") (++ i))
      (= b 0x09) (do (buffer/push-string out "\\t") (++ i))
      (= b 0x0D) (do (buffer/push-string out "\\r") (++ i))
      (and (>= b 0x20) (< b 0x7F)) (do (buffer/push-byte out b) (++ i))
      (< b 0x80) (do (buffer/push-string out (string/format "\\x%x;" b)) (++ i))
      (let [[cp len] (utf8-cp s i)]
        (buffer/push-string out (string/format "\\x%x;" cp))
        (+= i len))))
  (buffer/push-string out "\"")
  (string out))

(defn- emit-const [v]
  (cond
    (nil? v) "jolt-nil"
    (boolean? v) (if v "#t" "#f")
    # jolt models every number as a double (no ratios/bignums; see reader.janet).
    # Emit flonums so arithmetic matches the Janet host and Chez doesn't fall into
    # exploding exact rationals (mandelbrot). Integer-valued -> append ".0".
    # ##Inf/##-Inf/##NaN: Janet stringifies these as inf/-inf/nan, which are
    # unbound symbols in Chez — emit Chez's flonum literals instead.
    (number? v) (cond
                  (= v math/inf) "+inf.0"
                  (= v (- math/inf)) "-inf.0"
                  (not= v v) "+nan.0"
                  (let [s (string v)]
                    (if (or (string/find "." s) (string/find "e" s)) s (string s ".0"))))
    (string? v) (chez-str-lit v)   # quoted+escaped string literal
    # keyword literal -> (keyword ns name); ns is everything before the first "/"
    (keyword? v) (let [s (string v) idx (string/find "/" s)]
                   (if (and idx (> idx 0))
                     (string "(keyword " (chez-str-lit (string/slice s 0 idx)) " "
                             (chez-str-lit (string/slice s (inc idx))) ")")
                     (string "(keyword #f " (chez-str-lit s) ")")))
    # jolt char value {:ch <codepoint> :jolt/type :jolt/char}
    (and (struct? v) (= :jolt/char (get v :jolt/type)))
    (string "(integer->char " (get v :ch) ")")
    (errorf "emit-const: unsupported literal %p" v)))

# Quoted literals (jolt-u8j7). A :quote node's :form is the RAW reader form (a
# Janet value): scalars are Janet natives, a symbol is {:jolt/type :symbol …}, a
# list is an array, a vector a tuple, a map a struct/phm, a set a tagged struct.
# Reconstruct each as the matching Chez RT constructor — the runtime value of a
# quote is just that literal data (the interpreter returns the reader form
# verbatim; the Janet backend Janet-quotes it; here we rebuild it on the RT).
(var emit-quoted nil)
(defn- emit-quoted-map [m]
  (def flat @[])
  (eachp [k v] m (array/push flat (emit-quoted k)) (array/push flat (emit-quoted v)))
  (string "(jolt-hash-map " (string/join flat " ") ")"))
(set emit-quoted (fn emit-quoted [form]
  (cond
    # scalars emit-const already lowers (nil/bool/number/string/keyword/char)
    (or (nil? form) (boolean? form) (number? form) (string? form) (keyword? form))
    (emit-const form)
    (and (struct? form) (= :symbol (get form :jolt/type)))
    (let [ns (get form :ns)
          m (get form :meta)]
      (if (and m (not (nil? m)) (> (length m) 0))
        # carry reader metadata (^:foo bar) onto the quoted symbol so (meta 'x) sees it
        (string "(jolt-symbol/meta " (if ns (chez-str-lit ns) "#f") " "
                (chez-str-lit (get form :name)) " " (emit-quoted m) ")")
        (string "(jolt-symbol " (if ns (chez-str-lit ns) "#f") " "
                (chez-str-lit (get form :name)) ")")))
    (and (struct? form) (= :jolt/char (get form :jolt/type))) (emit-const form)
    (and (struct? form) (= :jolt/set (get form :jolt/type)))
    (string "(jolt-hash-set " (string/join (map emit-quoted (get form :value)) " ") ")")
    (array? form) (string "(jolt-list " (string/join (map emit-quoted form) " ") ")")
    (tuple? form) (string "(jolt-vector " (string/join (map emit-quoted form) " ") ")")
    (phm/phm? form) (emit-quoted-map (phm/phm-to-struct form))
    (or (struct? form) (table? form)) (emit-quoted-map form)
    (errorf "emit-quoted: unsupported quoted form %p" form))))

# A def's :meta is a jolt map value (Janet struct/table or phm). Non-empty?
# (a plain def carries {} — keep it on the lean def-var! path).
(defn- jmeta-nonempty? [m]
  (cond
    (nil? m) false
    (phm/phm? m) (> (length (phm/phm-to-struct m)) 0)
    (or (struct? m) (table? m)) (> (length m) 0)
    false))

(defn- emit-binding [b]
  (def b (vv b))
  (string "(" (munge (get b 0)) " " (emit (get b 1)) ")"))

# letfn lowers to a :let flagged :letrec (mutually-recursive named local fns):
# Scheme `letrec*` binds them so each sees its siblings (and itself), which a
# sequential let* can't. A plain let uses let* (Clojure let binds sequentially).
(defn- emit-let [node]
  (def kw (if (get node :letrec) "letrec*" "let*"))
  (string "(" kw " (" (string/join (map emit-binding (vv (get node :bindings))) " ") ") "
          (emit (get node :body)) ")"))

(defn- emit-loop [node]
  (def label (fresh-label "loop"))
  (def pairs (map vv (vv (get node :bindings))))
  (def names (map |(munge (get $ 0)) pairs))
  # inits are evaluated in the OUTER scope (recur-target unchanged) and, like
  # Clojure loop/let, SEQUENTIALLY — a later init sees earlier bindings. Scheme's
  # named `let` binds in parallel, so wrap a sequential let* around the loop.
  (def inits (map |(emit (get $ 1)) pairs))
  (def seq-bs (string/join (map (fn [n i] (string "(" n " " i ")")) names inits) " "))
  (def rebinds (string/join (map (fn [n] (string "(" n " " n ")")) names) " "))
  (def prev recur-target)
  (set recur-target label)
  (def body (emit (get node :body)))
  (set recur-target prev)
  (string "(let* (" seq-bs ") (let " label " (" rebinds ") " body "))"))

(defn- emit-recur [node]
  (unless recur-target (error "emit: recur outside a loop/fn target"))
  (string "(" recur-target " " (string/join (map emit (vv (get node :args))) " ") ")"))

# One arity -> a Scheme lambda param-list + a named-let-wrapped body. The named
# let lets fn-level `recur` rebind this arity's params. A variadic arity takes a
# Scheme rest arg (proper list) and the let binding coerces it to a jolt seq
# (nil when empty — Clojure's rest semantics; list->cseq already does this); recur
# carries the rest seq directly, and the named let's init only runs on first
# entry, so the coercion isn't re-applied on a recur.
# try/catch/finally (jolt-vcsl). throw raises the jolt value RAW (jolt-throw =
# Scheme `raise`), mirroring the Janet COMPILED backend (which does `(error v)`,
# no :jolt/exception envelope) — so catch binds the value directly, no unwrap.
# catch lowers to `guard` with an `else` clause (catch-all: the IR drops the
# class), finally to `dynamic-wind`'s after-thunk (runs on success, catch, and
# escape — Clojure finally semantics). Both keys are optional on the node.
(defn- emit-try [node]
  (def core
    (if-let [cs (get node :catch-sym)]
      (string "(guard (" (munge cs) " (else " (emit (get node :catch-body)) ")) "
              (emit (get node :body)) ")")
      (emit (get node :body))))
  (if-let [fin (get node :finally)]
    (string "(dynamic-wind (lambda () #f) (lambda () " core ") (lambda () " (emit fin) "))")
    core))

(defn- emit-arity-clause [a]
  (def params (map munge (vv (get a :params))))
  (def restp (when-let [r (get a :rest)] (munge r)))
  (def label (fresh-label "fnrec"))
  (def prev recur-target)
  (set recur-target label)
  (def body (emit (get a :body)))
  (set recur-target prev)
  (def paramlist
    (cond
      # only a rest param: Scheme formals are the bare symbol, not `( . xs)`
      (and restp (empty? params)) restp
      restp (string "(" (string/join params " ") " . " restp ")")
      (string "(" (string/join params " ") ")")))
  (def binds
    (if restp
      [;(map (fn [p] (string "(" p " " p ")")) params)
       (string "(" restp " (list->cseq " restp "))")]
      (map (fn [p] (string "(" p " " p ")")) params)))
  [paramlist (string "(let " label " (" (string/join binds " ") ") " body ")")])

(defn- emit-fn [node]
  (def arities (map nn (vv (get node :arities))))
  # a named fn binds its own name as a known-procedure local across ALL arities,
  # so self-calls (to any arity) emit directly rather than via jolt-invoke; the
  # case-lambda value dispatches on argument count.
  (def self (when-let [nm (get node :name)] (munge nm)))
  (def had-self (and self (get known-procs self)))
  (when self (put known-procs self true))
  # Restore known-procs even when a body is uncompilable: a throw mid-emit must
  # not leak this fn's name into the module global, or a LATER case binding the
  # same name to a keyword/coll would emit a direct call to a non-procedure
  # (runtime crash). The corpus probe shares one emit state across all cases, so
  # this leak is order-dependent and otherwise invisible in single-case tests.
  (def clauses
    (try (map emit-arity-clause arities)
      ([err fib]
        (unless had-self (when self (put known-procs self nil)))
        (propagate err fib))))
  (unless had-self (when self (put known-procs self nil)))
  (def lambda
    (if (= 1 (length clauses))
      (let [[pl body] (first clauses)] (string "(lambda " pl " " body ")"))
      (string "(case-lambda "
              (string/join (map (fn [c] (string "(" (get c 0) " " (get c 1) ")")) clauses) " ")
              ")")))
  # A named fn (defn / (fn self [..])) references itself by name — the analyzer
  # binds that name as a :local in the body. letrec makes the name visible to the
  # lambda so self-calls resolve (recur stays a separate self-call to the arity).
  (if-let [nm (get node :name)]
    (let [m (munge nm)] (string "(letrec ((" m " " lambda ")) " m ")"))
    lambda))

# The Clojure stdlib (clojure.core, clojure.math, clojure.string, …) and host
# interop (Math/sqrt etc.) have no implementation on Chez yet (Phase 2+). A
# reference to one — except a clojure.core call lowered to a native op — is
# genuinely uncompilable here. Reject it at emit time (a clean "out of subset"
# signal) rather than emitting a var-deref that resolves to nil and fails
# confusingly at runtime.
(defn- stdlib-var? [n]
  (and (= :var (get n :op)) (string/has-prefix? "clojure." (or (get n :ns) ""))))

# Host interop methods with a Chez RT shim (rt.ss jolt-host-call). A `.method`
# call on any other method is out of subset until shimmed — keep this in sync.
# `.write` is NOT here: StringWriter (a jhost, host-static.ss) handles .write via
# record-method-dispatch; the old jolt-host-call "write" fast-path (display to a
# port) would mis-route a writer to `(display x jhost)`. Keep the File-op methods.
(def- supported-host-methods {"isDirectory" true "listFiles" true})

# jolt's comparison ops are vacuously true at arity 1 and DON'T inspect the arg
# (so (< :kw) is true), but Scheme's < demands a number even there — special-case.
(def- cmp1-ops {"<" true ">" true "<=" true ">=" true})

# IFn dispatch for a LITERAL callee (Clojure's "value as fn"): a keyword looks
# itself up in its arg ((:k m) = (get m :k)); a map/set/vector literal looks up
# its arg ((m :k) = (get m :k)). This static lowering avoids the jolt-invoke
# dispatch overhead; the dynamic case (a local holding a keyword/coll/fn) routes
# through jolt-invoke in the emit-invoke fallback below.
(defn- ifn-kind [fnode]
  (case (get fnode :op)
    :const (when (keyword? (get fnode :val)) :keyword)
    :map :coll :set :coll :vector :coll
    nil))

(defn- emit-invoke [node]
  (def fnode (nn (get node :fn)))
  (def args (map emit (vv (get node :args))))
  (def nop (native-op fnode (length args)))
  (def kind (ifn-kind fnode))
  (def default (if (> (length args) 1) (string " " (in args 1)) ""))
  (cond
    # zero-arg + / * : Scheme's identity is the EXACT 0 / 1, but jolt models every
    # number as a double, so emit the flonum identity to keep (= 0 (+)) true.
    (and nop (empty? args) (= nop "+")) "0.0"
    (and nop (empty? args) (= nop "*")) "1.0"
    (and nop (= 1 (length args)) (get cmp1-ops nop)) (string "(begin " (first args) " #t)")
    nop (string "(" nop " " (string/join args " ") ")")
    # (:k coll [default]) -> (jolt-get coll :k [default])
    (= kind :keyword) (string "(jolt-get " (first args) " " (emit fnode) default ")")
    # (coll k [default]) -> (jolt-get coll k [default])
    (= kind :coll) (string "(jolt-get " (emit fnode) " " (first args) default ")")
    (and (stdlib-var? fnode) (not prelude-mode?))
    (errorf "emit: unsupported stdlib fn `%s/%s` (no core on Chez yet)" (get fnode :ns) (get fnode :name))
    # static method call (Class/method arg*) -> (host-static-call "Class"
    # "method" arg*). host-static.ss resolves the method from the class-statics
    # registry and applies it (jolt-avt6).
    (= :host-static (get fnode :op))
    (string "(host-static-call " (chez-str-lit (get fnode :class)) " "
            (chez-str-lit (get fnode :member))
            (if (empty? args) "" (string " " (string/join args " "))) ")")
    (= :host (get fnode :op))
    (errorf "emit: unsupported host call `%s` (no host interop on Chez yet)" (get fnode :name))
    # a :local callee that isn't a known procedure (a let/param binding holding a
    # keyword/coll/fn) -> dynamic IFn dispatch. Excludes the named-fn self-call.
    (and (= :local (get fnode :op)) (not (get known-procs (munge (get fnode :name)))))
    (string "(jolt-invoke " (emit fnode) " " (string/join args " ") ")")
    # a late-bound :var call head can hold a plain procedure OR a non-applicable
    # value the RT dispatches (a multimethod record, a keyword/coll IFn) — route it
    # through jolt-invoke so all of those work. Transparent for a procedure
    # (jolt-invoke just applies it); the hot self-recursive call is a :local
    # known-proc above, so it stays a direct call.
    (= :var (get fnode :op))
    (string "(jolt-invoke " (emit fnode) " " (string/join args " ") ")")
    # a computed callee (an :invoke / :if / :do expression) can yield ANY IFn —
    # a procedure, but also a coll/keyword/multimethod (e.g. ((sorted-map …) k)).
    # Route through jolt-invoke: transparent for a procedure, correct for the rest.
    (string "(jolt-invoke " (emit fnode) " " (string/join args " ") ")")))

# Native-op Scheme procedures that return a genuine Scheme boolean (#t/#f), so an
# :if test built from them needs no jolt-truthy? wrapper (jolt-nkcb). Comparisons
# and `not` are #t/#f; the numeric/collection predicates bottom out in fx=?/>/etc.
(def- bool-returning-ops
  {"<" true "<=" true ">" true ">=" true "jolt=" true "jolt-not" true
   "jolt-even?" true "jolt-odd?" true "jolt-pos?" true "jolt-neg?" true
   "jolt-zero?" true "jolt-empty?" true "jolt-contains?" true})

# Does this IR node emit to an expression that yields a Scheme boolean? Used to
# drop the redundant jolt-truthy? on an :if test — sound because jolt-truthy? of
# #t/#f is the identity. Conservative: only a boolean const or an :invoke that
# lowers to a bool-returning native op (any other shape keeps the wrapper).
(defn- returns-scheme-bool? [node]
  (def node (nn node))
  (cond
    (and (= :const (get node :op)) (boolean? (get node :val))) true
    (= :invoke (get node :op))
    (let [nop (native-op (nn (get node :fn)) (length (vv (get node :args))))]
      (truthy? (and nop (get bool-returning-ops nop))))
    false))

(set emit (fn emit [node]
  (def node (nn node))
  (case (get node :op)
    :const (emit-const (get node :val))
    :local (munge (get node :name))
    # late-bound var: read the cell's current root at use time. A value-position
    # ref to a clojure.core fn the RT provides (e.g. passing `inc`/`even?`/`:k` to
    # (map inc xs)) lowers to the RT procedure — native-ops names a real Scheme
    # procedure for each. Any OTHER stdlib var (clojure.string, an unimplemented
    # core fn) has no impl on Chez yet, so it's out of subset.
    :var   (let [core-proc (and (= "clojure.core" (get node :ns)) (get core-value-procs (get node :name)))]
             (cond
               core-proc core-proc
               (and (stdlib-var? node) (not prelude-mode?))
               (errorf "emit: unsupported stdlib ref `%s/%s` (no core on Chez yet)" (get node :ns) (get node :name))
               (string "(var-deref " (chez-str-lit (get node :ns)) " "
                       (chez-str-lit (get node :name)) ")")))
    :host  (errorf "emit: unsupported host ref `%s` (no host interop on Chez yet)" (get node :name))
    # value-position static ref (Class/member, e.g. Long/MAX_VALUE, System/exit
    # passed as a value) -> the registered static value/procedure (host-static.ss).
    :host-static (string "(host-static-ref " (chez-str-lit (get node :class)) " "
                         (chez-str-lit (get node :member)) ")")
    # constructor (Class. args*) / (new Class args*) -> (host-new "Class" args*).
    :host-new (string "(host-new " (chez-str-lit (get node :class))
                      (let [args (map emit (vv (get node :args)))]
                        (if (empty? args) "" (string " " (string/join args " ")))) ")")
    # (var x) / #'x -> the var cell itself (the rt.ss var-cell, a first-class var
    # object). var?/var-get/deref/invoke/= operate on it (vars.ss).
    :the-var (string "(jolt-var " (chez-str-lit (get node :ns)) " " (chez-str-lit (get node :name)) ")")
    :if    (let [test (get node :test)
                 t (if (returns-scheme-bool? test) (emit test)
                       (string "(jolt-truthy? " (emit test) ")"))]
             (string "(if " t " " (emit (get node :then)) " " (emit (get node :else)) ")"))
    :do    (string "(begin "
                   (string/join (map emit (vv (get node :statements))) " ")
                   (if (empty? (vv (get node :statements))) "" " ")
                   (emit (get node :ret)) ")")
    :invoke (emit-invoke node)
    # collection literals -> rt constructors (collections.ss). Elements evaluate
    # LEFT-TO-RIGHT (emit-ordered) to match Clojure for side-effecting elements.
    :vector (emit-ordered "jolt-vector" (map emit (vv (get node :items))))
    :set    (emit-ordered "jolt-hash-set" (map emit (vv (get node :items))))
    :map   (let [flat @[]]
             (each p (vv (get node :pairs))
               (def p (vv p))
               (array/push flat (emit (get p 0)))
               (array/push flat (emit (get p 1))))
             (emit-ordered "jolt-hash-map" flat))
    :let   (emit-let node)
    :loop  (emit-loop node)
    :recur (emit-recur node)
    :throw (string "(jolt-throw " (emit (get node :expr)) ")")
    :try   (emit-try node)
    :quote (emit-quoted (get node :form))
    # regex literal #"…" -> a jolt-regex value (regex.ss compiles the source via
    # the vendored irregex). chez-str-lit quotes+escapes the source; a backslash
    # in the pattern becomes \\ in the Scheme string literal -> the 1-char
    # backslash irregex expects (same escaping emit-const uses for strings).
    :regex (string "(jolt-regex " (chez-str-lit (get node :source)) ")")
    # host interop (jolt-0kf5): (.method target arg*) -> (jolt-host-call "method"
    # target arg*). Only the methods the RT dispatcher (rt.ss) actually shims are
    # IN the subset; any other method is out of subset (a clean emit-time reject,
    # like an unimplemented stdlib fn), so it doesn't masquerade as a compiled-but-
    # broken divergence. The Janet back end punts ALL :host-call to the interpreter.
    :host-call (let [m (get node :method)
                     target (emit (get node :target))
                     args (map emit (vv (get node :args)))]
                 (if (get supported-host-methods m)
                   (string "(jolt-host-call " (chez-str-lit m) " "
                           target (if (empty? args) "" (string " " (string/join args " "))) ")")
                   # a non-shimmed method: dispatch at runtime by the target's type
                   # — a record/reify protocol method (jolt-jgoc). On a non-record
                   # host value this errors (was an emit-fail before, so no new
                   # divergence), but it lets (.protoMethod record …) compile.
                   (string "(record-method-dispatch " target " " (chez-str-lit m)
                           " (jolt-vector" (if (empty? args) "" (string " " (string/join args " "))) "))")))
    :fn    (emit-fn node)
    # (def name) with no init (declare): reserve the var cell (declare-var!
    # doesn't clobber an existing root) so a forward reference resolves.
    # A def with non-empty reader metadata (^:private / ^Type tag / docstring ->
    # {:doc}) lowers to def-var-with-meta! so (meta (var x)) sees it (jolt-zikh).
    :def   (cond
             (get node :no-init)
             (string "(declare-var! " (chez-str-lit (get node :ns)) " "
                     (chez-str-lit (get node :name)) ")")
             (jmeta-nonempty? (get node :meta))
             (string "(def-var-with-meta! " (chez-str-lit (get node :ns)) " "
                     (chez-str-lit (get node :name)) " " (emit (get node :init)) " "
                     (emit-quoted (get node :meta)) ")")
             (string "(def-var! " (chez-str-lit (get node :ns)) " "
                     (chez-str-lit (get node :name)) " " (emit (get node :init)) ")"))
    (errorf "emit: unhandled op %p" (get node :op)))))

# Wrap emitted top-level forms into a runnable Chez program: load the RT, then
# the def forms, then print `final` (an emitted Scheme expr string) via jolt's
# number/value printing.
(defn program [forms-scheme final]
  (string
    "(import (chezscheme))\n"
    "(load \"host/chez/rt.ss\")\n"
    (string/join forms-scheme "\n") "\n"
    "(printf \"~a\\n\" (jolt-final-str " final "))\n"))
