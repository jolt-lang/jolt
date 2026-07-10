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
                               form-map-pairs form-set-items form-char-code
                               form-regex? form-regex-source
                               form-inst? form-inst-source form-uuid? form-uuid-source]]))

;; Hot clojure.core primitives lowered to native Scheme. Every per-op fact —
;; the call-position Scheme name, the value-position proc, the arity gate, any
;; fixed-arity fast-path helper, the flonum/fixnum/bigdec specializations, and
;; whether the result is a Scheme boolean — lives in ONE entry in op-registry,
;; so the derived accessor tables can never drift against each other (two
;; shipped bugs were table-drift artifacts: the =/fl=? exactness bug, and
;; jolt=2 missing from bool-returning-ops so truthy-elision never fired for
;; 2-arg =). Add a fact to an op's entry and every consumer sees it.
;;
;; `=` is the exactness-aware jolt= from values.ss; inc/dec/not are rt shims.
;; Arithmetic and comparisons lower to the jolt-n* checked macros
;; (host/chez/seq.ss): the both-Chez-numbers fast path is open-coded and
;; anything else (BigDecimal, a non-number) takes the Numbers.ops-style category
;; dispatch, with JVM contagion (a double operand wins; an exact zero divisor is
;; ArithmeticException; a double zero divisor is ##Inf/##NaN).
;;
;; Entry keys:
;;   :call   Scheme name emitted in call position (a jolt-n* macro or prim).
;;   :value  Scheme proc emitted when the op is passed AS A VALUE (to map /
;;           reduce / apply); defaults to :call. The jolt-n* call forms are
;;           macros, so value position substitutes the variadic procedures over
;;           the same binary dispatch.
;;   :arity  predicate gating which arities lower; absent means variadic.
;;   :fixed  {arity helper} fast-path overrides — emit the direct fixed-arity
;;           helper at that exact arity (no rest list, no fold-left overhead),
;;           checked before the :call lookup.
;;   :dbl/:lng/:bd  Chez flonum/fixnum/bigdec op jolt.passes.numeric lowers to
;;           when every operand is that num-kind. ^long is 64-bit but a Chez
;;           fixnum is only 61-bit, so +/-/* keep the raw fx ops while
;;           comparisons / min/max / quot/rem/mod use the jolt-l* fast-path-
;;           with-fallback macros so a full 64-bit operand falls back instead of
;;           raising; unchecked-* WRAP to signed 64 bits so they use jolt-unc*
;;           helpers, not the raising fx ops. inc/dec (and unchecked-inc/dec)
;;           are special-cased in emit-numeric, not listed. / has no :lng
;;           specialization (integer division stays on quot/rem).
;;   :bool?  the emitted proc(s) return a genuine Scheme boolean, so an :if test
;;           built from one needs no jolt-truthy? wrapper.
(def ^:private op-registry
  ;; arithmetic
  {"+"   {:call "jolt-n+"    :value "jolt-add"   :dbl "fl+"  :lng "fx+"        :bd "jbd-add"}
   "-"   {:call "jolt-n-"    :value "jolt-sub"   :dbl "fl-"  :lng "fx-"        :bd "jbd-sub"}
   "*"   {:call "jolt-n*"    :value "jolt-mul"   :dbl "fl*"  :lng "fx*"        :bd "jbd-mul"}
   "/"   {:call "jolt-n-div" :value "jolt-div"   :dbl "fl/"                   :bd "jbd-div"}
   ;; comparisons: vacuously true at arity 1 and don't inspect the arg, but
   ;; Scheme's < demands a number even there — cmp1-ops special-cases that.
   "<"   {:call "jolt-n<"   :value "jolt-lt" :bool? true
          :dbl "fl<?"  :lng "jolt-l<"  :bd "jbd-lt?"}
   ">"   {:call "jolt-n>"   :value "jolt-gt" :bool? true
          :dbl "fl>?"  :lng "jolt-l>"  :bd "jbd-gt?"}
   "<="  {:call "jolt-n<="  :value "jolt-le" :bool? true
          :dbl "fl<=?" :lng "jolt-l<=" :bd "jbd-le?"}
   ">="  {:call "jolt-n>="  :value "jolt-ge" :bool? true
          :dbl "fl>=?" :lng "jolt-l>=" :bd "jbd-ge?"}
   "="   {:call "jolt=" :bool? true :arity #(>= % 2)
          :fixed {2 "jolt=2"} :dbl "fl=?" :lng "jolt-l="}
   "=="  {:dbl "fl=?" :lng "jolt-l="}  ; numeric-only, not a native op
   "inc" {:call "jolt-inc" :arity #(= % 1)}
   "dec" {:call "jolt-dec" :arity #(= % 1)}
   "not" {:call "jolt-not" :arity #(= % 1) :bool? true}
   "min" {:call "jolt-n-min" :value "jolt-min"
          :dbl "flmin" :lng "jolt-l-min" :bd "jbd-min"}
   "max" {:call "jolt-n-max" :value "jolt-max"
          :dbl "flmax" :lng "jolt-l-max" :bd "jbd-max"}
   "mod"   {:call "jolt-mod"  :arity #(= % 2)
            :lng "jolt-l-mod"  :bd "jbd-mod"}
   "rem"   {:call "jolt-rem"  :arity #(= % 2)
            :lng "jolt-l-rem"  :bd "jbd-rem"}
   "quot"  {:call "jolt-quot" :arity #(= % 2)
            :lng "jolt-l-quot" :bd "jbd-quot"}
   "unchecked-add"      {:lng "jolt-uncadd2"}
   "unchecked-subtract" {:lng "jolt-uncsub2"}
   "unchecked-multiply" {:lng "jolt-uncmul2"}
   ;; collections
   "vector"      {:call "jolt-vector"}
   "hash-map"    {:call "jolt-hash-map-fn"}
   "hash-set"    {:call "jolt-hash-set"}
   "conj"        {:call "jolt-conj"    :arity #(>= % 1) :fixed {2 "jolt-conj2"}}
   "get"         {:call "jolt-get"     :arity #(or (= % 2) (= % 3))}
   "nth"         {:call "jolt-nth"     :arity #(or (= % 2) (= % 3))}
   "count"       {:call "jolt-count"   :arity #(= % 1)}
   "assoc"       {:call "jolt-assoc"   :arity #(and (>= % 3) (odd? %))
                  :fixed {3 "jolt-assoc3"}}
   "dissoc"      {:call "jolt-dissoc"  :arity #(>= % 1) :fixed {2 "jolt-dissoc2"}}
   "contains?"   {:call "jolt-contains?" :arity #(= % 2) :bool? true}
   "empty?"      {:call "jolt-empty?"   :arity #(= % 1) :bool? true}
   "peek"        {:call "jolt-peek"    :arity #(= % 1)}
   "pop"         {:call "jolt-pop"     :arity #(= % 1)}
   ;; seq
   "first"   {:call "jolt-first"   :arity #(= % 1)}
   "rest"    {:call "jolt-rest"    :arity #(= % 1)}
   "next"    {:call "jolt-next"    :arity #(= % 1)}
   "seq"     {:call "jolt-seq"     :arity #(= % 1)}
   "cons"    {:call "jolt-cons"    :arity #(= % 2)}
   "list"    {:call "jolt-list"}
   "reverse" {:call "jolt-reverse" :arity #(= % 1)}
   "last"    {:call "jolt-last"    :arity #(= % 1)}
   "map"     {:call "jolt-map"     :arity #(>= % 2)}
   "filter"  {:call "jolt-filter"  :arity #(= % 2)}
   "remove"  {:call "jolt-remove"  :arity #(= % 2)}
   "reduce"  {:call "jolt-reduce"  :arity #(or (= % 2) (= % 3))}
   "into"    {:call "jolt-into"    :arity #(= % 2)}
   "concat"  {:call "jolt-concat"}
   "apply"   {:call "jolt-apply"   :arity #(>= % 2)}
   "range"   {:call "jolt-range"   :arity #(and (>= % 0) (<= % 3))}
   "take"    {:call "jolt-take"    :arity #(= % 2)}
   "drop"    {:call "jolt-drop"    :arity #(= % 2)}
   "keys"    {:call "jolt-keys"    :arity #(= % 1)}
   "vals"    {:call "jolt-vals"    :arity #(= % 1)}
   ;; predicates
   "even?"    {:call "jolt-even?"  :arity #(= % 1) :bool? true}
   "odd?"     {:call "jolt-odd?"   :arity #(= % 1) :bool? true}
   "pos?"     {:call "jolt-pos?"   :arity #(= % 1) :bool? true :bd "jbd-pos?"}
   "neg?"     {:call "jolt-neg?"   :arity #(= % 1) :bool? true :bd "jbd-neg?"}
   "zero?"    {:call "jolt-zero?"  :arity #(= % 1) :bool? true :bd "jbd-zero?"}
   "identity" {:call "jolt-identity" :arity #(= % 1)}
   "nil?"     {:call "jolt-nil?"   :arity #(= % 1) :bool? true}
   "some?"    {:call "jolt-some?"  :arity #(= % 1) :bool? true}
   "ex-info"  {:call "jolt-ex-info" :arity #(or (= % 2) (= % 3))}
   ;; bit ops: and/or/xor/not are Chez bitwise primitives (inlined to native
   ;; code, no helper call); operands must be integers (a non-integer errors,
   ;; like the JVM). The shifts keep their helpers (Java >>> masking /
   ;; arithmetic shift) but emit a direct call instead of var-deref + the
   ;; variadic overlay. and/or/xor/not are variadic Chez prims (safe as a value
   ;; at any arity); bit-and-not is left to its overlay: its only Scheme impl is
   ;; 2-arg, so a value-position arity-3 use (via the variadic overlay) would
   ;; mis-emit.
   "bit-and"                 {:call "bitwise-and"  :arity #(= % 2)}
   "bit-or"                  {:call "bitwise-ior"  :arity #(= % 2)}
   "bit-xor"                 {:call "bitwise-xor"  :arity #(= % 2)}
   "bit-not"                 {:call "bitwise-not"  :arity #(= % 1)}
   "bit-shift-left"          {:call "jolt-bit-shift-left"          :arity #(= % 2)}
   "bit-shift-right"         {:call "jolt-bit-shift-right"         :arity #(= % 2)}
   "unsigned-bit-shift-right" {:call "jolt-unsigned-bit-shift-right" :arity #(= % 2)}
   ;; positional protocol-method dispatch (defprotocol-emitted shims) — bind
   ;; directly to the records.ss entry points so a protocol call doesn't
   ;; var-deref. Emitted bare (call name == op name).
   "protocol-dispatch1" {:call "protocol-dispatch1" :arity #(= % 3)}
   "protocol-dispatch2" {:call "protocol-dispatch2" :arity #(= % 4)}
   "protocol-dispatch3" {:call "protocol-dispatch3" :arity #(= % 5)}})

;; Derived accessor tables. Every consumer below looks these up by op name (or,
;; for bool-returning-ops, by emitted Scheme name); content is fixed entirely by
;; op-registry, so edit per-op facts there, not here.
(def ^:private native-ops
  (into {} (keep (fn [[op spec]] (when (:call spec) [op (:call spec)]))) op-registry))
(def ^:private core-value-procs
  (into {} (keep (fn [[op spec]]
                   (when (:call spec) [op (or (:value spec) (:call spec))]))
                 op-registry)))
;; Native-op Scheme procedures that return a genuine Scheme boolean (#t/#f), so
;; an :if test built from them needs no jolt-truthy? wrapper. Covers EVERY
;; procedure name a :bool? op can emit — its :call name plus each :fixed helper
;; — so jolt=2 (the 2-arg = helper) is covered, not just jolt=.
(def ^:private bool-returning-ops
  (into #{}
        (for [[_ spec] op-registry
              :when (:bool? spec)
              proc (cons (:call spec) (vals (:fixed spec)))
              :when proc]
          proc)))
(def ^:private op-arity
  (into {} (keep (fn [[op spec]] (when (:arity spec) [op (:arity spec)]))) op-registry))
(def ^:private fixed-arity-ops
  (into {} (keep (fn [[op spec]] (when (:fixed spec) [op (:fixed spec)]))) op-registry))
(def ^:private dbl-ops
  (into {} (keep (fn [[op spec]] (when (:dbl spec) [op (:dbl spec)]))) op-registry))
(def ^:private lng-ops
  (into {} (keep (fn [[op spec]] (when (:lng spec) [op (:lng spec)]))) op-registry))
(def ^:private bd-ops
  (into {} (keep (fn [[op spec]] (when (:bd spec) [op (:bd spec)]))) op-registry))

;; jolt's comparison ops are vacuously true at arity 1 and DON'T inspect the arg,
;; but Scheme's < demands a number even there — special-case.
(def ^:private cmp1-ops #{"jolt-n<" "jolt-n>" "jolt-n<=" "jolt-n>="})

;; Host interop methods with a Chez RT shim (rt.ss jolt-host-call). A `.method`
;; call on any other method routes to record-method-dispatch (a reify/record
;; protocol method).
(def ^:private supported-host-methods #{"isDirectory" "listFiles"})

;; PRELUDE MODE. The default (subset) mode rejects any clojure.core ref
;; that isn't a native-op — a clean "out of subset" signal for user-facing `-e`.
;; When emitting clojure.core ITSELF as a prelude, core fns reference each other
;; constantly; those lower to var-deref (resolved at runtime).
(def prelude-mode? (atom false))
(defn set-prelude-mode! [on] (reset! prelude-mode? on))

;; DIRECT-LINK MODE. Off for ordinary runs, the seed mint, and `-e`/repl/load-string
;; (open world — vars are redefinable). `jolt build` (release/optimized) flips it on
;; during app emission: a closed-world program where every app def is final, so an
;; app->app call binds to the def's Scheme binding directly, skipping the var-table
;; lookup and the generic jolt-invoke dispatch.
(def direct-link? (atom false))
(defn set-direct-link! [on] (reset! direct-link? on))

;; Fully-qualified app var names ("ns/name") already emitted with a direct-link
;; binding in the current unit. A call/value-ref direct-links only to a name in this
;; set — one defined earlier in emission order (or itself), so the Scheme binding
;; exists by the time the reference runs. Reset per build.
(def direct-link-defined (atom #{}))
;; Of those, the ones whose init is a fn literal — safe to call as a raw Scheme
;; application. A def of a non-fn value (a map, set, keyword, …) is invokable in
;; Clojure but is not a Scheme procedure, so its calls must still route through
;; jolt-invoke even with a direct binding.
(def direct-link-fns (atom #{}))
(defn direct-link-reset! [] (reset! direct-link-defined #{}) (reset! direct-link-fns #{}))

;; Cache a resolved var cell in a per-site cell so a non-direct-linked var
;; reference skips the name lookup (string-append + hash) after the first use.
;; OFF during the seed mint (the seed must stay a byte-fixpoint, and caching the
;; compiler's own refs shifts the gensym-numbered cell names every pass); the
;; runtime eval path turns it on for user code, where it's the big win.
(def var-cache? (atom false))
(defn set-var-cache! [on] (reset! var-cache? on))

;; Record-ctor shape registry ("ns/->Name" -> {:fields (:k ..) :type tag}), set by
;; run-passes before each form's emit so emit-invoke can recognize ctor calls with
;; matching arity and emit a direct fixed-arity call instead of jolt-invoke dispatch.
(def ctor-shapes (atom {}))
(defn set-ctor-shapes! [m] (reset! ctor-shapes (or m {})))

;; Opt-in tail-frame history (JOLT_TRACE): emit a (jolt-trace-push! "name") at the
;; head of every named fn body, so an entry records the frame into the runtime ring
;; buffer (rt.ss) and a TCO-elided frame still shows in an error's backtrace. OFF
;; during the seed mint and `jolt build` (byte-determinism + no runtime cost);
;; compile-eval.ss turns it on for runtime-eval'd user code when JOLT_TRACE is set.
(def trace-frames? (atom false))
(defn set-trace-frames! [on] (reset! trace-frames? on))

;; A direct-link Scheme binding name for a var. The fqn maps to a unique identifier
;; jv$<ns>$<name>; chars that break a Scheme identifier or the `$` separator are
;; escaped so distinct vars never collide.
(defn- dl-munge [s]
  (-> s (str/replace "$" "_D_") (str/replace "#" "_H_") (str/replace "'" "_Q_")))
(defn- dl-name [ns nm] (str "jv$" (dl-munge ns) "$" (dl-munge nm)))
(defn- dl-fqn [ns nm] (str ns "/" nm))
(defn- direct-linkable? [ns nm]
  (and @direct-link? (contains? @direct-link-defined (dl-fqn ns nm))))
;; A direct-linked var whose value is a fn literal — its binding is a Scheme
;; procedure, so a call site can apply it directly.
(defn- direct-link-fn? [ns nm]
  (contains? @direct-link-fns (dl-fqn ns nm)))

;; recur-target and the set of munged local names known to hold a procedure (a
;; named fn's self-recursion name) are lexically scoped — dynamic vars so the
;; recursion auto-restores them (no manual save/restore, no throw-leak).
(def ^:dynamic *recur-target* nil)
(def ^:dynamic *known-procs* #{})
;; True while emitting a node in TAIL position. Only used, in trace mode, to mark a
;; tail call so the runtime routes its callee into the current history rib instead
;; of a new one (rt.ss). It never affects semantics — a wrong value only mislabels
;; a debug trace line — so partial propagation is safe. `emit` (the wrapper below)
;; clears it by default; the tail-transparent forms (fn body, if/do/let/loop) pass
;; it to their tail child. Default false so a top-level form is treated non-tail.
(def ^:dynamic *tail?* false)

(def ^:private gensym-counter (atom 0))
(defn- fresh-label [prefix] (str prefix (swap! gensym-counter inc)))

;; Per-site cache cells collected while emitting one top-level def. A site that
;; resolves a STABLE value — a devirtualized impl (constant tag/proto/method) or a
;; var cell (interned, so the cell never changes even when the var is redefined) —
;; resolves it once, not per call, the inline cache the JVM gets for free. When a
;; def init is being emitted this holds an atom; each site appends a fresh cell name
;; (bound to #f in a let wrapping the def, so it persists across calls and is shared
;; by every invocation) and resolves into it on first use. nil outside a def (a site
;; there falls back to a per-call resolve).
(def ^:private cache-cells (atom nil))

;; Emit a def's init (via the supplied thunk) under a fresh cache-cell collector,
;; then wrap the result in a let binding any cells its body registered so they
;; persist in the def's closure. Saves/restores the outer collector for nested
;; defs. Used by both the runtime def emit and the direct-link top-level emit.
(defn- emit-with-cells [emit-thunk]
  (let [cells (atom [])
        prev @cache-cells
        _ (reset! cache-cells cells)
        raw (emit-thunk)
        _ (reset! cache-cells prev)]
    (if (seq @cells)
      (str "(let (" (str/join " " (map (fn [c] (str "(" c " #f)")) @cells)) ") " raw ")")
      raw)))

;; Scheme syntactic keywords. A jolt local with one of these names would, when
;; emitted verbatim, shadow the Scheme form in operator position (a local named
;; `if` would turn the special form (if …) the back end emits into a call), so
;; such locals are prefixed. Matches the spec: special-form heads are not
;; shadowable, but a value local may legally be named `if`.
(def ^:private scheme-reserved
  #{"if" "begin" "lambda" "let" "let*" "letrec" "letrec*" "quote" "quasiquote"
    "unquote" "set!" "define" "define-syntax" "cond" "case" "when" "unless"
    "and" "or" "do" "else" "guard" "parameterize" "delay" "values"})

;; clojure.core ops emitted as a BARE Scheme name (where native-ops maps the op
;; to itself: + - * / < > min max …). A local binding with one of these names
;; would otherwise shadow the emitted prim — e.g. (fn [max] (clojure.core/max …))
;; emits (max …) calling the param — so such locals are prefixed, like reserved
;; words. Derived from native-ops so the two never drift.
(def ^:private bare-native-names
  (set (keep (fn [[k v]] (when (= k v) k)) native-ops)))

;; Most jolt names are already valid Scheme identifiers. The one that isn't is
;; `#`, which jolt auto-gensyms use as a suffix (p1__0000X4# from #(...)) — `#`
;; starts a datum in Scheme, so replace it with `_`. A name that collides with a
;; Scheme keyword OR a bare-emitted native op is prefixed with `_` so it can never
;; shadow the emitted form.
(defn- munge-name [s]
  ;; A Clojure symbol may contain chars that break a Scheme identifier: ' is the
  ;; quote reader macro (a bare f' would read as f then 'rest), # already maps to
  ;; _. Munge both to safe tokens; the same mapping applies at the binding and at
  ;; every reference, so resolution stays consistent.
  (let [s (-> s
              (str/replace "#" "_")
              (str/replace "'" "_PRIME_"))]
    (if (or (contains? scheme-reserved s) (contains? bare-native-names s)) (str "_" s) s)))

(declare emit)
(declare emit*)
;; Ops that pass tail position through to a child (the child can itself be a tail
;; call): if/do carry it to their tail branch/last form, let/loop to their body,
;; and invoke reads it to decide whether the call is tail. Every other op's
;; children are non-tail, so `emit` clears *tail?* before dispatching them — that
;; way a stray true can't leak into, say, a call sitting in a vector literal.
(def ^:private tail-transparent-ops #{:if :do :let :loop :invoke})
(defn emit [node]
  (if (and *tail?* (not (tail-transparent-ops (:op node))))
    (binding [*tail?* false] (emit* node))
    (emit* node)))

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
    ;; Numeric tower: emit a literal Chez re-reads as the SAME number, via the
    ;; host's own writer (jolt.host/chez-number-literal) — NOT jolt's str, whose
    ;; rendering follows the reference printer (bigint N suffix, E exponents)
    ;; and is not Chez-readable. ##Inf/##-Inf/##NaN -> Chez's flonum literals.
    (number? v) (cond
                  (= v ##Inf) "+inf.0"
                  (= v ##-Inf) "-inf.0"
                  (not= v v) "+nan.0"
                  :else (jolt.host/chez-number-literal v))
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
;; Delegates to ordered-call (needs-order?): wrap in a let* of fresh temps only
;; when two or more operands could have observable effects, so a literal over
;; locals/consts stays un-wrapped. `nodes` are the item IR nodes (for the
;; side-effect check); each is emitted once here.
(defn- emit-ordered [ctor nodes]
  (ordered-call nodes (mapv emit nodes)
    (fn [strs] (str "(" ctor (if (empty? strs) "" (str " " (str/join " " strs))) ")"))))

;; An operand whose evaluation has no observable effect: constants, locals,
;; var/the-var reads, quoted literals.
(defn- side-effect-free? [n]
  (contains? #{:const :local :var :the-var :quote} (:op n)))

;; A var VALUE read is effect-free but order-SENSITIVE: a mutating sibling
;; (def/alter-var-root/set!) changes what it yields, so it must not move across
;; an effectful operand in either direction. A :the-var read (the var object)
;; is a stable reference — insensitive.
(defn- order-sensitive-read? [n]
  (= :var (:op n)))

;; Clojure evaluates a call's operands (and recur's args, and collection-literal
;; elements) left to right; Chez's application order is unspecified
;; (right-to-left in practice). Force source order by binding operands to fresh
;; temps in a let* — but only when an effectful operand coexists with another
;; order-sensitive one (effectful or a var read), so hot calls over
;; locals/consts stay un-wrapped.
(defn- needs-order? [nodes]
  (let [e (count (remove side-effect-free? nodes))]
    (and (>= e 1)
         (>= (+ e (count (filter order-sensitive-read? nodes))) 2))))

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
  ;; A jolt map VALUE (def/symbol metadata is a value, not a reader form). (keys m)
  ;; iterates in host-hash order, which is not stable across Chez versions, so emit
  ;; the pairs sorted by their emitted Scheme text — keeps the seed byte-fixed
  ;; regardless of the host hash (jolt-8479).
  (let [pairs (sort (map (fn [k] (str (emit-quoted k) " " (emit-quoted (get m k)))) (keys m)))]
    (str "(jolt-hash-map " (str/join " " pairs) ")")))
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
    ;; sort items by emitted text: a set has no source order, and host-hash order
    ;; is not stable across Chez versions (jolt-8479).
    (form-set? form) (str "(jolt-hash-set " (str/join " " (sort (map emit-quoted (form-set-items form)))) ")")
    (form-list? form) (str "(jolt-list " (str/join " " (map emit-quoted (form-elements form))) ")")
    (form-vec? form) (str "(jolt-vector " (str/join " " (map emit-quoted (form-vec-items form))) ")")
    (form-map? form) (emit-quoted-map (form-map-pairs form))
    ;; a quoted #"…" regex value -> reconstruct it (same as the :regex IR leaf).
    (form-regex? form) (str "(jolt-regex " (chez-str-lit (form-regex-source form)) ")")
    ;; quoted #inst / #uuid literals construct their value, like the JVM reader
    ;; (which builds the Date/UUID at read time, so a quoted/macro form carries the
    ;; value, not the raw tagged form). Same emit as the :inst / :uuid IR leaves.
    (form-inst? form) (str "(jolt-inst-from-string " (chez-str-lit (form-inst-source form)) ")")
    (form-uuid? form) (str "(jolt-uuid-from-string " (chez-str-lit (form-uuid-source form)) ")")
    ;; a quoted custom #tag with no registered reader -> a tagged-literal value
    ;; (Clojure's reader builds a TaggedLiteral), not the raw reader map. The tag is
    ;; stored as a :#name keyword; strip the leading # to the bare symbol.
    (and (map? form) (= :jolt/tagged (get form :jolt/type)))
    (let [nm (name (get form :tag))
          tsym (if (= \# (first nm)) (subs nm 1) nm)]
      (str "(jolt-tagged-literal (jolt-symbol #f " (chez-str-lit tsym) ") "
           (emit-quoted (get form :form)) ")"))
    ;; plain jolt VALUES (metadata maps and anything nested in them)
    (map? form) (emit-quoted-map-value form)
    (vector? form) (str "(jolt-vector " (str/join " " (map emit-quoted form)) ")")
    (set? form) (str "(jolt-hash-set " (str/join " " (sort (map emit-quoted form))) ")")
    (seq? form) (str "(jolt-list " (str/join " " (map emit-quoted form)) ")")
    :else (throw (ex-info (str "emit-quoted: unsupported quoted form " (pr-str form)) {}))))

;; A def's :meta is a jolt map value. Non-empty? (a plain def carries {}).
(defn- jmeta-nonempty? [m] (and (map? m) (pos? (count m))))

;; The meta argument to def-var-with-meta!. When the analyzer attached a
;; :meta-expr (metadata with values to evaluate, e.g. ^{:a some-fn}), emit it as a
;; runtime expression; otherwise the static :meta map as quoted data.
(defn- emit-def-meta [node]
  (if (:meta-expr node)
    (emit (:meta-expr node))
    (emit-quoted (:meta node))))

(defn- emit-binding [b]
  (str "(" (munge-name (nth b 0)) " " (emit (nth b 1)) ")"))

;; letfn lowers to a :let flagged :letrec (mutually-recursive named local fns):
;; Scheme `letrec*` binds them so each sees its siblings. A plain let uses let*.
(defn- emit-let [node]
  (let [kw (if (:letrec node) "letrec*" "let*")
        ;; bindings are non-tail; the body inherits the let's tail position
        binds (binding [*tail?* false] (str/join " " (mapv emit-binding (:bindings node))))]
    (str "(" kw " (" binds ") " (emit (:body node)) ")")))

(defn- emit-loop [node]
  (let [label (fresh-label "loop")
        pairs (:bindings node)
        names (map #(munge-name (nth % 0)) pairs)
        ;; inits evaluate in the OUTER scope (recur-target unchanged) and, like
        ;; Clojure loop/let, SEQUENTIALLY — wrap a let* around the named let.
        inits (binding [*tail?* false] (mapv #(emit (nth % 1)) pairs))
        seq-bs (str/join " " (map (fn [n i] (str "(" n " " i ")")) names inits))
        rebinds (str/join " " (map (fn [n] (str "(" n " " n ")")) names))
        ;; the loop body inherits the loop's tail position
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
  (let [n (count (:argtypes node))
        params (mapv (fn [i] (str "a" i)) (range n))
        fp (str "(foreign-procedure " (when (:blocking node) "__collect_safe ")
                (chez-str-lit (:csym node))
                " (" (str/join " " (map ffi-type->chez (:argtypes node))) ") "
                (ffi-type->chez (:rettype node)) ")")]
    ;; Lazy resolution: the foreign-procedure form is deferred inside a closure.
    ;; On first call, the cell `p` is set to the FP and then invoked; subsequent
    ;; calls skip the set!. This lets a defcfn's defining form (top-level def)
    ;; evaluate to a callable closure before the shared library is loaded —
    ;; critical for :optional :jolt/native libs whose load-object runs in the
    ;; scheme-start launcher, after the heap is already built.
    (str "(let ((p #f)) (lambda (" (str/join " " params) ") "
         "((or p (begin (set! p " fp ") p)) " (str/join " " params) ")))")))

;; jolt.ffi/__ccallable -> a Chez foreign-callable wrapping the emitted jolt fn,
;; locked + registered (jolt-ffi-register-callable!, host/chez/java/ffi.ss) so the
;; collector neither moves nor reclaims it while C may still call through it. The
;; expression evaluates to the entry-point address — a jolt pointer the caller
;; hands to C. :collect-safe emits the convention that reactivates the thread on
;; entry, for callbacks invoked while it is parked in a :blocking foreign call.
(defn- emit-ffi-callable [node]
  (str "(jolt-ffi-register-callable! (foreign-callable "
       (when (:collect-safe node) "__collect_safe ")
       (emit (:fn node))
       " (" (str/join " " (map ffi-type->chez (:argtypes node))) ") "
       (ffi-type->chez (:rettype node)) "))"))

(defn- emit-recur [node]
  (when-not *recur-target* (throw (ex-info "emit: recur outside a loop/fn target" {})))
  (let [arg-nodes (:args node)]
    (ordered-call arg-nodes (mapv emit arg-nodes)
                  (fn [as] (str "(" *recur-target* " " (str/join " " as) ")")))))

;; One arity -> a Scheme lambda param-list + a named-let-wrapped body. The named
;; let lets fn-level `recur` rebind this arity's params. A variadic arity takes a
;; Scheme rest arg coerced to a jolt seq (nil when empty); recur carries the rest
;; seq directly, and the named let's init only runs on first entry.
;; Coerce a numeric-hinted param at fn entry, the way the JVM coerces a primitive
;; parameter: ^double -> exact->inexact, ^long -> jolt->fx. Only the named-let init
;; (first entry) coerces — recur carries already-typed values, like a JVM goto. This
;; is what makes the hint a contract the body's fl*/fx* ops can rely on. `orig` is
;; the param's source name (the :nhints key); `munged` the emitted identifier.
(defn- nhint-init [nh orig munged]
  (let [k (get nh orig)]
    (cond (= k :double) (str "(exact->inexact " munged ")")
          (= k :long)   (str "(jolt->fx " munged ")")
          :else munged)))

(defn- emit-arity-clause [a]
  (let [orig (:params a)
        nh (into {} (:nhints a))
        params (map munge-name orig)
        restp (when-let [r (:rest a)] (munge-name r))
        label (fresh-label "fnrec")
        ret (:ret-nhint a)
        ;; the body is the fn's tail position — UNLESS a ^double/^long return hint
        ;; wraps it in a coercion below, which puts the body back in non-tail.
        body-tail? (not (or (= ret :double) (= ret :long)))
        body (binding [*recur-target* label *tail?* body-tail?] (emit (:body a)))
        paramlist (cond
                    (and restp (empty? params)) restp
                    restp (str "(" (str/join " " params) " . " restp ")")
                    :else (str "(" (str/join " " params) ")"))
        pbind (map (fn [o p] (str "(" p " " (nhint-init nh o p) ")")) orig params)
        binds (if restp
                (concat pbind [(str "(" restp " (list->cseq " restp "))")])
                pbind)
        lett (str "(let " label " (" (str/join " " binds) ") " body ")")
        ;; a ^double/^long return hint coerces the arity's value on the way out
        ;; (exact->inexact / jolt->fx), like a JVM primitive return — so a caller's
        ;; arithmetic over the result is sound.
        ret (:ret-nhint a)]
    [paramlist (cond (= ret :double) (str "(exact->inexact " lett ")")
                     (= ret :long)   (str "(jolt->fx " lett ")")
                     :else lett)]))

(defn- emit-fn [node]
  (let [arities (:arities node)
        ;; a named fn binds its own name as a known-procedure local across ALL
        ;; arities, so self-calls emit directly rather than via jolt-invoke.
        self (when-let [nm (:name node)] (munge-name nm))
        clauses (binding [*known-procs* (if self (conj *known-procs* self) *known-procs*)]
                  (mapv emit-arity-clause arities))
        ;; trace mode: record this frame on entry (before the body), so a frame
        ;; the body then tail-calls away is still in the ring at throw time. A
        ;; `recur` re-enters via the named-let, not the lambda, so a tight loop
        ;; records once, not per iteration.
        clauses (if (and @trace-frames? self)
                  (mapv (fn [c] [(nth c 0)
                                 (str "(begin (jolt-trace-push! " (chez-str-lit self) ") "
                                      (nth c 1) ")")])
                        clauses)
                  clauses)
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
        fixed (when nm (get (fixed-arity-ops nm) nargs))
        op (or fixed (when nm (native-ops nm)))
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

;; Polymorphic inline-cache width. MUST match jolt-pic-n in host/chez/records.ss:
;; the emitted scan reads slots [0..2N) and the epoch slot at 2N+1 of the cache
;; vector jolt-pic-make allocates, so the two must agree.
(def ^:private pic-n 4)
(def ^:private pic-epoch-idx (+ (* 2 pic-n) 1))
;; the eq? scan over a PIC cache's N (desc . impl) pairs: each clause returns the
;; impl when its cached desc is eq? to d. Strung together with `or` so a hit short-
;; circuits; a full miss falls through to the install helper (the caller appends it).
(defn- pic-scan-clauses [v d]
  (str/join " "
            (for [i (range pic-n)]
              (str "(and (eq? (vector-ref " v " " (* 2 i) ") " d ")"
                   " (vector-ref " v " " (+ (* 2 i) 1) "))"))))

;; A reference into the Clojure stdlib (clojure.*) with no impl on Chez yet.
(defn- stdlib-var? [n]
  (and (= :var (:op n)) (str/starts-with? (or (:ns n) "") "clojure.")))

;; Emit a :num-kind-tagged arithmetic call as a Chez flonum/fixnum op. inc/dec are
;; unary (fl +/- 1.0, fx1+/fx1-); the rest map through dbl-ops/lng-ops. Integer
;; literal operands of a :double op were coerced to flonums by jolt.passes.numeric.
(defn- emit-numeric [kind nm args order-args]
  (cond
    (and (= kind :double) (= nm "inc")) (str "(fl+ " (first args) " 1.0)")
    (and (= kind :double) (= nm "dec")) (str "(fl- " (first args) " 1.0)")
    ;; inc/dec tolerate a 64-bit operand (jolt-l-inc/dec fall back past fixnum range);
    ;; unchecked-inc/dec wrap (Java long). Neither can use the raising fx1+/fx1-.
    (and (= kind :long) (= nm "inc")) (str "(jolt-l-inc " (first args) ")")
    (and (= kind :long) (= nm "dec")) (str "(jolt-l-dec " (first args) ")")
    (and (= kind :long) (= nm "unchecked-inc")) (str "(jolt-uncinc " (first args) ")")
    (and (= kind :long) (= nm "unchecked-dec")) (str "(jolt-uncdec " (first args) ")")
    :else
    (let [op (case kind :double (dbl-ops nm) :long (lng-ops nm) :bigdec (bd-ops nm))]
      (order-args (fn [as] (str "(" op " " (str/join " " as) ")"))))))

;; slot of a declared field key in a record's field-order shape, or nil.
(defn- struct-field-index [shape kw]
  (when shape
    (loop [i 0]
      (cond (>= i (count shape)) nil
            (= (nth shape i) kw) i
            :else (recur (inc i))))))

;; A plain Scheme application: (callee op ...).
(defn- plain-call [callee operand-strs]
  (str "(" callee (if (seq operand-strs) (str " " (str/join " " operand-strs)) "") ")"))
;; A tail call in trace mode. Force-bind the operands to temps FIRST (so any
;; operand whose own evaluation records a trace entry runs before our mark), THEN
;; set the tail mark, THEN apply — the callee's entry prologue consumes the mark
;; with nothing in between, so it can't be clobbered. Still a tail call: the let*'s
;; last form is the application, so TCO is preserved.
(defn- tail-marked-call [callee operand-strs]
  (let [tmps (mapv (fn [_] (fresh-label "_tt$")) operand-strs)
        binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps operand-strs))]
    (str "(let* (" binds ") (jolt-trace-mark! #t) " (plain-call callee tmps) ")")))
;; Emit a call, tail-marked when we're in tail position and tracing is on; a plain
;; application otherwise. The mark is consumed by the callee's entry prologue —
;; direct calls (:local known-proc, direct-link) always have one; a jolt-invoke
;; call usually reaches one but not always (see the best-effort note in rt.ss).
(defn- emit-call [tail? callee operand-strs]
  (if (and @trace-frames? tail?)
    (tail-marked-call callee operand-strs)
    (plain-call callee operand-strs)))

(defn- emit-invoke [node]
  (let [tail? *tail?*]           ; capture: children below emit non-tail
   (binding [*tail?* false]
    (let [fnode (:fn node)
        arg-nodes (:args node)
        args (mapv emit arg-nodes)
        nop (native-op fnode (count args))
        kind (ifn-kind fnode)
        ;; order args left-to-right (build receives the spliced operand strings)
        order-args (fn [build] (ordered-call arg-nodes args build))
        defstr (fn [as] (if (> (count as) 1) (str " " (nth as 1)) ""))
        ;; jolt-invoke dispatch: Clojure evaluates the fn expr before the args, so
        ;; order [callee & args] together when ordering is observable. Pick a
        ;; fixed-arity entry point (jolt-invoke0..4) by arg count so the common
        ;; raw-procedure fast path allocates no rest-list; keep variadic jolt-invoke
        ;; for larger arities / apply tails.
        invoke (fn []
                 (let [callee (if (<= (count args) 4)
                                (str "jolt-invoke" (count args))
                                "jolt-invoke")]
                   (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                                 (fn [operands] (emit-call tail? callee operands)))))]
    (cond
      ;; devirtualized protocol call: the inference proved the receiver (arg 0) is
      ;; one record type, so resolve the impl by that static tag instead of routing
      ;; through the protocol var -> jolt-invoke -> protocol-resolve (which recomputes
      ;; the tag and walks the type table). devirt-resolve does the same table lookup
      ;; the dispatch would, but with no var-deref and no receiver-type computation;
      ;; it falls back to ordinary dispatch when the static tag has no direct impl (a
      ;; record satisfying the protocol via an Object/host-tag default). Fires only on
      ;; a monomorphic site (a megamorphic receiver joins to :any, no :devirt-type).
      ;; The receiver is bound once — it feeds both the resolve and the application.
      (:devirt-type node)
      (order-args (fn [as]
                    (let [r (fresh-label "_r$")
                          dv (str "(devirt-resolve " (chez-str-lit (:devirt-type node)) " "
                                  (chez-str-lit (:devirt-proto node)) " " (chez-str-lit (:devirt-method node))
                                  " " r ")")
                          cells @cache-cells
                          ;; cache the resolved impl in a per-site cell when inside a
                          ;; def; else resolve per call. The cell carries (epoch . fn):
                          ;; each call compares its epoch against jolt-proto-epoch and
                          ;; re-resolves on mismatch, so an extend-type after warmup
                          ;; (a register-protocol-method epoch bump) invalidates this
                          ;; site like every other dispatch path, mirroring the PIC.
                          resolver (if cells
                                      (let [c (fresh-label "_dvc$")]
                                        (swap! cells conj c)
                                        (str "(if (and (pair? " c ") (fx= (car " c ") jolt-proto-epoch))"
                                             " (cdr " c ")"
                                             " (let ((_f " dv ")) (set! " c " (cons jolt-proto-epoch _f)) _f))"))
                                      dv)]
                      (str "(let* ((" r " " (first as) ")) ("
                           resolver " " (str/join " " (cons r (rest as))) "))"))))
      ;; polymorphic inline cache: a protocol call the inference recognized (:proto)
      ;; but could NOT prove monomorphic (no :devirt-type — a megamorphic / unknown
      ;; receiver). Emit a per-site cache keyed on the receiver's descriptor identity:
      ;; after warmup each call is one field read + an eq? scan over <= jolt-pic-n
      ;; cached descs + a direct apply, with no string hashing or table walk. The
      ;; cache lives in a def-closure cell (emit-with-cells); a register-protocol-
      ;; method after population bumps jolt-proto-epoch and the cache re-resolves,
      ;; so an extend-type at runtime can't strand a stale impl. Falls back to a
      ;; direct protocol-resolve (still the per-descriptor fast path) when not inside
      ;; a def. The receiver is bound once and feeds both the resolve and the apply.
      (:proto node)
      (order-args (fn [as]
                    (let [r (fresh-label "_r$")
                          d (fresh-label "_d$")
                          v (fresh-label "_v$")
                          cells @cache-cells
                          proto (chez-str-lit (:proto node))
                          method (chez-str-lit (:method node))
                          apply-args (str/join " " (cons r (rest as)))]
                      (if cells
                        (let [c (fresh-label "_picv$")
                              scan (pic-scan-clauses v d)]
                          (swap! cells conj c)
                          ;; hot path inlined: bind the receiver, the cache vector
                          ;; (lazily allocated on first call), and its desc; then, if
                          ;; the epoch still matches, eq?-scan the cached descs and
                          ;; apply the hit impl directly — no helper call after warmup.
                          ;; A miss (no cached desc / stale epoch) resolves + (re)fills
                          ;; via the jolt-pic-install/-rebuild helpers.
                          (str "(let* ((" r " " (first as) ")"
                               " (" v " (or " c " (let ((_nv (jolt-pic-make))) (set! " c " _nv) _nv)))"
                               " (" d " (jrec-pic-desc " r ")))"
                               " ((if (and " d " (fx= (vector-ref " v " " pic-epoch-idx ") jolt-proto-epoch))"
                                " (or " scan " (jolt-pic-install " v " " d " " proto " " method " " r "))"
                                " (jolt-pic-rebuild " v " " d " " proto " " method " " r "))"
                                " " apply-args "))"))
                        (str "(let* ((" r " " (first as) "))"
                             " ((protocol-resolve " proto " " method " " r ") " apply-args "))")))))
      ;; hint-directed fast arithmetic: jolt.passes.numeric proved every operand a
      ;; flonum (^double) or fixnum (^long), so emit the Chez fl*/fx* op.
      (:num-kind node) (emit-numeric (:num-kind node) (:name fnode) args order-args)
      ;; zero-arg + / * : exact integer identity (= JVM long: (+) -> 0, (*) -> 1).
      (and nop (empty? args) (= nop "+")) "0"
      (and nop (empty? args) (= nop "*")) "1"
      (and nop (= 1 (count args)) (cmp1-ops nop)) (str "(begin " (first args) " #t)")
      ;; (get coll k [default]) with a struct-typed coll — the inference marked the
      ;; receiver with :hint :struct and a :shape matching the declared field layout.
      ;; Read the field by its static slot (jrec-field-at) instead of the generic
      ;; jolt-get dispatch. Only the 2-arg (no-default) form takes this path since a
      ;; declared field is always present. The key must be a compile-time keyword
      ;; literal so (struct-field-index) can resolve it.
      (and (= nop "jolt-get") (<= 2 (count arg-nodes) 3))
      (let [recv (first arg-nodes) key-node (second arg-nodes)
            idx (when (and (= 2 (count arg-nodes))
                           (= :struct (:hint recv))
                           (= :const (:op key-node))
                           (keyword? (:val key-node)))
                  (struct-field-index (:shape recv) (:val key-node)))]
        (if idx
          (order-args (fn [as] (str "(jrec-field-at " (first as) " " idx " " (emit key-node) ")")))
          (order-args (fn [as] (str "(jolt-get " (str/join " " as) ")")))))
      nop (order-args (fn [as] (str "(" nop " " (str/join " " as) ")")))
      ;; (:k coll [default]) -> (jolt-get coll :k [default]) — the key (fnode) is a
      ;; const, so only the coll/default args carry order. When the inference typed
      ;; the receiver as a record whose declared fields include :k (it carries the
      ;; field-order :shape), read the field by its static slot — no field-key
      ;; lookup, no jolt-get dispatch. Only the no-default form (a declared field is
      ;; always present, so a default is never taken).
      (= kind :keyword)
      (let [recv (first arg-nodes)
            idx (when (and (= :struct (:hint recv)) (= 1 (count arg-nodes)))
                  (struct-field-index (:shape recv) (:val fnode)))]
        (if idx
          (order-args (fn [as] (str "(jrec-field-at " (first as) " " idx " " (emit fnode) ")")))
          (order-args (fn [as] (str "(jolt-get " (first as) " " (emit fnode) (defstr as) ")")))))
      ;; (coll k [default]) -> lookup — coll (fnode) is the callee, evaluated
      ;; before the key/default args. A VECTOR literal invokes as nth (a bad
      ;; index throws, IPersistentVector.invoke); maps/sets invoke as get.
      (= kind :coll)
      (ordered-call (cons fnode arg-nodes) (cons (emit fnode) args)
                    (fn [[c & as]]
                      (str (if (and (= :vector (:op fnode)) (= 1 (count as)))
                             "(jolt-nth "
                             "(jolt-get ")
                           c " " (str/join " " as) ")")))
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
      ;; a :local callee: a known procedure (the letrec-bound self-name of a named
      ;; fn — i.e. self-recursion) is a real Scheme proc, so call it directly with
      ;; no jolt-invoke / arg consing; case-lambda handles arity. Any other local
      ;; holds an arbitrary IFn -> dynamic dispatch.
      (= :local (:op fnode))
      (if (*known-procs* (munge-name (:name fnode)))
        (order-args (fn [as] (emit-call tail? (munge-name (:name fnode)) as)))
        (invoke))
      ;; closed-world direct call: the callee var is an app fn def already emitted
      ;; with a Scheme binding — apply it directly, no var lookup, no jolt-invoke.
      ;; Only fn-valued defs qualify; a non-fn invokable value (a map/set/keyword
      ;; held in a var) isn't a Scheme procedure, so it falls through to jolt-invoke
      ;; below (which still uses the direct binding as the invoke target).
      (and (= :var (:op fnode)) (direct-linkable? (:ns fnode) (:name fnode))
           (direct-link-fn? (:ns fnode) (:name fnode)))
      (order-args (fn [as] (emit-call tail? (dl-name (:ns fnode) (:name fnode)) as)))
       ;; record ctor with matching arity: inline the native per-arity ctor
       ;; (make-jrecN) directly — desc + ext + one inline slot per field —
       ;; eliminating jolt-invoke / var-deref / rest-list / ctor call / hashtable
       ;; lookup AND the field vector. After per-site desc-cell warmup the hot
       ;; path is: cell read -> make-jrecN — one allocation, no lookups, no
       ;; dispatch. The shape is set by run-passes (set-ctor-shapes!) before emit.
       (let [key (str (:ns fnode) "/" (:name fnode))
             shape (get @ctor-shapes key)]
         (and (= :var (:op fnode)) shape
              (= (count (get shape :fields)) (count args))
              (<= (count args) 6)
              ;; skip if any ^double field — the inlined path doesn't coerce
              (not-any? #{"double"} (get shape :tags))))
       (let [s (get @ctor-shapes (str (:ns fnode) "/" (:name fnode)))
             tag (:type s)
             cells @cache-cells
             desc-lookup (str "(hashtable-ref chez-tag-desc " (chez-str-lit tag) " #f)")
             cached-desc (if cells
                           (let [c (fresh-label "_cdesc$")]
                             (swap! cells conj c)
                             (str "(or " c " (let ((_d " desc-lookup ")) (set! " c " _d) _d))"))
                           desc-lookup)]
         (order-args (fn [as]
                       (let [n (count as)]
                         (if (<= n 8)
                           (str "(make-jrec" n " " cached-desc " jolt-nil"
                                (when (pos? n) (str " " (str/join " " as))) ")")
                           (str "(let ((v (vector " (str/join " " as) "))) (make-jrec " cached-desc " v jolt-nil))"))))))
      ;; a late-bound :var call head can hold a procedure OR a non-applicable
      ;; value the RT dispatches (multimethod, keyword/coll IFn) — route via
      ;; jolt-invoke (transparent for a procedure).
      (= :var (:op fnode))
      (invoke)
      ;; a computed callee can yield ANY IFn — route through jolt-invoke.
      :else
      (invoke))))))

;; try/catch/finally. throw raises a Chez condition wrapping the jolt value
;; (jolt-throw = Scheme `raise` of a &jolt-throw condition); catch lowers to
;; `guard`, whose raw binding is unwrapped via jolt-unwrap-throw so the catch var
;; receives the jolt value (preserving ex-data/ex-message and the backtrace
;; identity tag). finally lowers to `dynamic-wind`'s after-thunk (runs on
;; success, catch and escape — Clojure finally semantics). Both keys optional.
(defn- emit-try [node]
  (let [core (if-let [cs (:catch-sym node)]
               (let [raw (munge-name (:catch-raw-sym node))]
                 (str "(guard (" raw " (else (let ((" (munge-name cs) " (jolt-unwrap-throw " raw "))) "
                      (emit (:catch-body node)) "))) "
                      (emit (:body node)) ")"))
               (emit (:body node)))]
    (if-let [fin (:finally node)]
      (str "(dynamic-wind (lambda () #f) (lambda () " core ") (lambda () " (emit fin) "))")
      core)))

;; Does this IR node emit to an expression that yields a Scheme boolean? Used to
;; drop the redundant jolt-truthy? on an :if test. Sees through the let*/if an
;; (or ...)/(and ...) of bool-returning ops desugars to: `or` is
;; (let* [g E1] (if (truthy? g) g E2)), `and` is (let* [g E1] (if (truthy? g) E2 g))
;; — both return a Scheme boolean when E1/E2 are bool ops, since the value yielded
;; is always one of the (boolean) operand results. `bools` tracks let-bound locals
;; proven to hold a Scheme boolean.
(defn- returns-scheme-bool?
  ([node] (returns-scheme-bool? node #{}))
  ([node bools]
   (cond
     (and (= :const (:op node)) (boolean? (:val node))) true
     (= :invoke (:op node))
     (let [nop (native-op (:fn node) (count (:args node)))]
       (boolean (and nop (bool-returning-ops nop))))
     (= :local (:op node)) (contains? bools (:name node))
     (= :if (:op node))
     (and (returns-scheme-bool? (:then node) bools)
          (returns-scheme-bool? (:else node) bools))
     (= :let (:op node))
     (let [bools' (reduce (fn [s b]
                            (if (returns-scheme-bool? (nth b 1) s)
                              (conj s (nth b 0))
                              (disj s (nth b 0))))
                          bools (:bindings node))]
       (returns-scheme-bool? (:body node) bools'))
     :else false)))

;; In trace mode, a fn def also registers its source so the tail-frame history maps
;; the recorded frame-name to "ns/name (file:line)" instead of a bare name. Keyed by
;; the SAME munged name the entry push records (emit-fn's letrec self-binding = the
;; fn's own name). Returns "" when off / not a positioned fn def, so trace-off output
;; (seed mint, `jolt build`) is byte-identical. Direct-link builds already register
;; via emit-def-cached; this covers the open-world eval path.
(defn- trace-source-reg [node]
  (let [init (:init node) pos (:pos node)]
    (if (and @trace-frames? (= :fn (:op init)) (:name init) pos)
      (str " (jolt-register-source! " (chez-str-lit (munge-name (:name init))) " "
           (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
           (if (:file pos) (chez-str-lit (:file pos)) "jolt-nil") " "
           (or (:line pos) 0) ")")
      "")))

(defn emit* [node]
  (case (:op node)
    :const (emit-const (:val node))
    :local (munge-name (:name node))
    ;; late-bound var: read the cell's current root at use time. A value-position
    ;; ref to a clojure.core fn the RT provides lowers to the RT procedure.
    :var (let [core-proc (and (= "clojure.core" (:ns node)) (core-value-procs (:name node)))]
           (cond
             core-proc core-proc
             ;; direct-linked app var used as a value -> reference its binding (same
             ;; root as the var cell for a final var; helps DCE keep it live).
             (direct-linkable? (:ns node) (:name node)) (dl-name (:ns node) (:name node))
             (and (stdlib-var? node) (not (deref prelude-mode?)))
             (throw (ex-info (str "emit: unsupported stdlib ref `" (:ns node) "/" (:name node)
                                  "` (no core on Chez yet)") {}))
             ;; inside a def, cache the interned var cell in a per-site cell so the
             ;; name lookup (string-append + hash) runs once, not per access; the
             ;; cell is stable and def-var! mutates its root in place, so this stays
             ;; correct under redefinition. Read through var-cell-deref — the
             ;; cell-based var-deref: binding-aware (a thread-bound dynamic var
             ;; resolves to its binding) AND lenient on an unbound root (the strict
             ;; jolt-var-get throws on a forward-declared var). Outside a def,
             ;; resolve per access.
             :else
             (let [cells @cache-cells
                   nslit (chez-str-lit (:ns node)) nmlit (chez-str-lit (:name node))]
               (if (and @var-cache? cells)
                 (let [c (fresh-label "_vc$")]
                   (swap! cells conj c)
                   (str "(var-cell-deref (or " c " (let ((_v (jolt-var " nslit " " nmlit "))) (set! " c " _v) _v)))"))
                 (str "(var-deref " nslit " " nmlit ")")))))
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
    ;; the test is non-tail; then/else inherit the if's tail position
    :if (let [test (:test node)
              t (binding [*tail?* false]
                  (if (returns-scheme-bool? test) (emit test)
                      (str "(jolt-truthy? " (emit test) ")")))]
          (str "(if " t " " (emit (:then node)) " " (emit (:else node)) ")"))
    ;; non-last statements are non-tail; the ret inherits the do's tail position
    :do (str "(begin " (binding [*tail?* false] (str/join " " (mapv emit (:statements node))))
             (if (empty? (:statements node)) "" " ") (emit (:ret node)) ")")
    :invoke (emit-invoke node)
     ;; collection literals -> rt constructors (collections.ss). Elements are
     ;; already-analyzed IR nodes; evaluate LEFT-TO-RIGHT (emit-ordered, which
     ;; wraps only when two or more operands could have observable effects).
     :vector (emit-ordered "jolt-vector" (:items node))
     :set (emit-ordered "jolt-hash-set" (:items node))
     :map (emit-ordered "jolt-hash-map"
                       (mapcat (fn [p] [(nth p 0) (nth p 1)]) (:pairs node)))
    :quote (emit-quoted (:form node))
    :throw (str "(jolt-throw " (emit (:expr node)) ")")
    ;; numeric coercion (from an inlined ^double/^long param or return).
    :coerce (let [e (emit (:expr node))]
              (cond (= :double (:kind node)) (str "(exact->inexact " e ")")
                    (= :long (:kind node)) (str "(jolt->fx " e ")")
                    :else e))
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
    :ffi-callable (emit-ffi-callable node)
    :fn (emit-fn node)
    ;; (def name) with no init (declare): reserve the cell. A def with non-empty
    ;; reader metadata lowers to def-var-with-meta! (ported in a later increment).
    :def (let [reg (trace-source-reg node)
               d (cond
                   (:no-init node)
                   (str "(declare-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")")
                   (jmeta-nonempty? (:meta node))
                   (str "(def-var-with-meta! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                        (emit-with-cells #(emit (:init node))) " " (emit-def-meta node) ")")
                   :else
                   (str "(def-var! " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) " "
                        (emit-with-cells #(emit (:init node))) ")"))]
           (if (= reg "") d (str "(begin " d reg ")")))
    (throw (ex-info (str "emit: op not yet ported / unhandled: " (pr-str (:op node))) {}))))

;; ^:dynamic / ^:redef on a def opts it out of direct-linking: it stays redefinable,
;; so callers must go through the var cell. m is a def's :meta (a jolt map value).
(defn- dl-opt-out? [m] (or (get m :dynamic) (get m :redef)))

;; Per-form entry used by the image/build emitter. In direct-link mode a TOP-LEVEL
;; def (form root, or spliced from a top-level do) without an opt-out also binds
;; jv$<fqn> and aliases the var cell to it, so app->app calls/refs bind directly.
;; Off direct-link mode this is exactly `emit`, so the seed mint and runtime eval are
;; byte-unchanged. Nested defs (a defonce's inner def) never reach a top-level branch
;; here, so they stay indirect — a `define` would be illegal in their position.
;; Emit a def, wrapping its init in a let that binds each per-site cache cell
;; (var-ref + devirt) so a hot loop's lookups resolve once into the def's closure.
;; Runs in BOTH modes; in direct-link mode a non-opt-out def also binds jv$<fqn>
;; and registers it for app->app direct linking + a source-map frame.
(defn- emit-def-cached [node]
  (let [ns (:ns node) nm (:name node)
        dl? (and @direct-link? (not (dl-opt-out? (:meta node))))
        b (dl-name ns nm)
        fn? (= :fn (:op (:init node)))
        ;; A fn def gets a source-registry entry so a native backtrace can map its
        ;; frame to ns/name (file:line). Chez names the frame by whatever emit-fn
        ;; binds the lambda to: a NAMED fn (defn, or (fn foo …)) gets a letrec
        ;; self-binding = munge-name of the fn's own name; an ANONYMOUS fn def has
        ;; no letrec, so the lambda sits directly under (define jv$ns$name …) and
        ;; takes that name. Register under whichever Chez will report.
        pos (:pos node)
        frame-name (when fn? (if-let [fnm (:name (:init node))] (munge-name fnm) b))
        reg (when (and dl? fn? pos)
              (str " (jolt-register-source! " (chez-str-lit frame-name) " "
                   (chez-str-lit ns) " " (chez-str-lit nm) " "
                   (if (get pos :file) (chez-str-lit (get pos :file)) "jolt-nil") " "
                   (or (get pos :line) 0) ")"))
        ;; register before emitting the init so a self-referential body direct-links.
        _ (when dl? (swap! direct-link-defined conj (dl-fqn ns nm))
                    (when fn? (swap! direct-link-fns conj (dl-fqn ns nm))))
        init (emit-with-cells #(emit (:init node)))]
    (cond
      dl?
      (if (jmeta-nonempty? (:meta node))
        (str "(begin (define " b " " init ") (def-var-with-meta! "
             (chez-str-lit ns) " " (chez-str-lit nm) " " b " " (emit-def-meta node) ")" (or reg "") ")")
        (str "(begin (define " b " " init ") (def-var! "
             (chez-str-lit ns) " " (chez-str-lit nm) " " b ")" (or reg "") ")"))
      (jmeta-nonempty? (:meta node))
      (str "(def-var-with-meta! " (chez-str-lit ns) " " (chez-str-lit nm) " " init " " (emit-def-meta node) ")")
      :else
      (str "(def-var! " (chez-str-lit ns) " " (chez-str-lit nm) " " init ")"))))

(defn emit-top-form [node]
  (cond
    ;; off direct-link (the seed mint + runtime-via-image) this is exactly `emit`,
    ;; whose :def case already wraps cache cells, so the seed stays byte-unchanged.
    (not @direct-link?) (emit node)
    ;; top-level do splices: each statement/ret is itself a top-level form.
    (= :do (:op node))
    (str "(begin " (str/join " " (map emit-top-form (:statements node)))
         (if (empty? (:statements node)) "" " ") (emit-top-form (:ret node)) ")")
    (and (= :def (:op node)) (not (:no-init node)) (not (dl-opt-out? (:meta node))))
    (emit-def-cached node)
    :else (emit node)))
