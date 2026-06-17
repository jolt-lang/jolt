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
   "empty?" "jolt-empty?" "peek" "jolt-peek" "pop" "jolt-pop"})

# Per-op arity gate: only lower when the Scheme prim and the jolt fn agree at
# this arity. Ops absent from the table are variadic (arith/compare/=, the
# collection constructors, conj/assoc/dissoc) and legal at any arity.
(def- op-arity
  {"inc" |(= $ 1) "dec" |(= $ 1) "not" |(= $ 1)
   "count" |(= $ 1) "empty?" |(= $ 1) "peek" |(= $ 1) "pop" |(= $ 1)
   "mod" |(= $ 2) "rem" |(= $ 2) "quot" |(= $ 2) "contains?" |(= $ 2)
   "get" |(or (= $ 2) (= $ 3)) "nth" |(or (= $ 2) (= $ 3))
   "assoc" |(and (>= $ 3) (odd? $)) "dissoc" |(>= $ 1) "conj" |(>= $ 1)})

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

(var- recur-target nil)
(var- gensym-n 0)
(defn- fresh-label [prefix] (string prefix (++ gensym-n)))

# Most jolt names are already valid Scheme identifiers (inc, even?, +, ->str all
# are — Scheme allows ! $ % & * + - . / : < = > ? @ ^ _ ~). The one that isn't is
# `#`, which jolt auto-gensyms use as a suffix (e.g. p1__0000X4# from #(...)
# shorthand) — `#` starts a datum in Scheme, so replace it with `_`.
(defn- munge [name] (string/replace-all "#" "_" name))

(var emit nil)   # forward declaration (mutual recursion with the helpers below)

(defn- emit-const [v]
  (cond
    (nil? v) "jolt-nil"
    (boolean? v) (if v "#t" "#f")
    # jolt models every number as a double (no ratios/bignums; see reader.janet).
    # Emit flonums so arithmetic matches the Janet host and Chez doesn't fall into
    # exploding exact rationals (mandelbrot). Integer-valued -> append ".0".
    (number? v) (let [s (string v)]
                  (if (or (string/find "." s) (string/find "e" s) (string/find "n" s))
                    s
                    (string s ".0")))
    (string? v) (string/format "%j" v)   # quoted+escaped string literal
    # keyword literal -> (keyword ns name); ns is everything before the first "/"
    (keyword? v) (let [s (string v) idx (string/find "/" s)]
                   (if (and idx (> idx 0))
                     (string "(keyword " (string/format "%j" (string/slice s 0 idx)) " "
                             (string/format "%j" (string/slice s (inc idx))) ")")
                     (string "(keyword #f " (string/format "%j" s) ")")))
    # jolt char value {:ch <codepoint> :jolt/type :jolt/char}
    (and (struct? v) (= :jolt/char (get v :jolt/type)))
    (string "(integer->char " (get v :ch) ")")
    (errorf "emit-const: unsupported literal %p" v)))

(defn- emit-binding [b]
  (def b (vv b))
  (string "(" (munge (get b 0)) " " (emit (get b 1)) ")"))

(defn- emit-let [node]
  (string "(let* (" (string/join (map emit-binding (vv (get node :bindings))) " ") ") "
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

(defn- emit-fn [node]
  (def arities (map nn (vv (get node :arities))))
  (when (not= 1 (length arities)) (error "emit: multi-arity fn not in this increment"))
  (def a (first arities))
  (when (get a :rest) (error "emit: variadic fn not in this increment"))
  (def params (map munge (vv (get a :params))))
  # wrap the body in a named let so fn-level `recur` rebinds the params
  (def label (fresh-label "fnrec"))
  (def prev recur-target)
  (set recur-target label)
  (def body (emit (get a :body)))
  (set recur-target prev)
  (def lambda
    (string "(lambda (" (string/join params " ") ") "
            "(let " label " (" (string/join (map (fn [p] (string "(" p " " p ")")) params) " ") ") "
            body "))"))
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

# jolt's comparison ops are vacuously true at arity 1 and DON'T inspect the arg
# (so (< :kw) is true), but Scheme's < demands a number even there — special-case.
(def- cmp1-ops {"<" true ">" true "<=" true ">=" true})

# IFn dispatch for a LITERAL callee (Clojure's "value as fn"): a keyword looks
# itself up in its arg ((:k m) = (get m :k)); a map/set/vector literal looks up
# its arg ((m :k) = (get m :k)). The general dynamic case — a local/var holding a
# keyword — is runtime IFn dispatch, a later increment, and stays out of subset.
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
    (stdlib-var? fnode)
    (errorf "emit: unsupported stdlib fn `%s/%s` (no core on Chez yet)" (get fnode :ns) (get fnode :name))
    (= :host (get fnode :op))
    (errorf "emit: unsupported host call `%s` (no host interop on Chez yet)" (get fnode :name))
    (string "(" (emit fnode) " " (string/join args " ") ")")))

(set emit (fn emit [node]
  (def node (nn node))
  (case (get node :op)
    :const (emit-const (get node :val))
    :local (munge (get node :name))
    # late-bound var: read the cell's current root at use time. A value-position
    # ref to a stdlib var (e.g. passing `inc` to (map inc xs)) needs a real fn,
    # which native-op lowering doesn't provide — so it's out of subset regardless.
    :var   (if (stdlib-var? node)
             (errorf "emit: unsupported stdlib ref `%s/%s` (no core on Chez yet)" (get node :ns) (get node :name))
             (string "(var-deref " (string/format "%j" (get node :ns)) " "
                     (string/format "%j" (get node :name)) ")"))
    :host  (errorf "emit: unsupported host ref `%s` (no host interop on Chez yet)" (get node :name))
    :if    (string "(if (jolt-truthy? " (emit (get node :test)) ") "
                   (emit (get node :then)) " " (emit (get node :else)) ")")
    :do    (string "(begin "
                   (string/join (map emit (vv (get node :statements))) " ")
                   (if (empty? (vv (get node :statements))) "" " ")
                   (emit (get node :ret)) ")")
    :invoke (emit-invoke node)
    # collection literals -> rt constructors (collections.ss)
    :vector (string "(jolt-vector " (string/join (map emit (vv (get node :items))) " ") ")")
    :set    (string "(jolt-hash-set " (string/join (map emit (vv (get node :items))) " ") ")")
    :map   (let [flat @[]]
             (each p (vv (get node :pairs))
               (def p (vv p))
               (array/push flat (emit (get p 0)))
               (array/push flat (emit (get p 1))))
             (string "(jolt-hash-map " (string/join flat " ") ")"))
    :let   (emit-let node)
    :loop  (emit-loop node)
    :recur (emit-recur node)
    :fn    (emit-fn node)
    :def   (string "(def-var! " (string/format "%j" (get node :ns)) " "
                   (string/format "%j" (get node :name)) " " (emit (get node :init)) ")")
    (errorf "emit: unhandled op %p" (get node :op)))))

# Wrap emitted top-level forms into a runnable Chez program: load the RT, then
# the def forms, then print `final` (an emitted Scheme expr string) via jolt's
# number/value printing.
(defn program [forms-scheme final]
  (string
    "(import (chezscheme))\n"
    "(load \"host/chez/rt.ss\")\n"
    (string/join forms-scheme "\n") "\n"
    "(printf \"~a\\n\" (jolt-pr-str " final "))\n"))
