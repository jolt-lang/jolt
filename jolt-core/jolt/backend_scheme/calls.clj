(ns jolt.backend-scheme.calls
  "Call sites for the Scheme back end: left-to-right operand ordering, the
  trace-site instrumentation a call carries — sited tail calls, site-slot
  restores — the native-op and IFn-literal call heads, proven-numeric
  arithmetic, and the specialized call shapes emit-invoke chooses among:
  record ctors, devirtualized and inline-cached protocol calls, flonum array
  access, fixnum bit ops, static host calls. Each takes its operands already
  emitted, through an order-args fn of the caller's."
  (:require [clojure.string :as str]
            [jolt.op-registry :as op-registry]
            [jolt.backend-scheme.names :refer [chez-str-lit munge-name]]
            [jolt.backend-scheme.session
             :refer [*array-vecs* *self-wrap* *site-call* *trace-self* *trace-site*
                     clone-site? ctor-shapes fresh-label marker-safe-fqn?
                     note-tail-call! trace-frames? unsafe-prefix var-cache?]]
            [jolt.backend-scheme.hoist
             :refer [*cache-cells* *const-pool* cached-tag-desc const-coll-node?
                     hoist-const-per-site]]))

;; The op-registry tables the call heads read, bound to local names as
;; jolt.backend-scheme binds them (qualified refs, not :refer — see the note
;; there).
(def ^:private native-ops op-registry/native-ops)
(def ^:private op-arity op-registry/op-arity)
(def ^:private fixed-arity-ops op-registry/fixed-arity-ops)
(def ^:private dbl-ops op-registry/dbl-ops)
(def ^:private lng-ops op-registry/lng-ops)
(def ^:private bd-ops op-registry/bd-ops)

;; The Scheme procs of the native ops that cannot run a jolt fn: the numeric ops
;; (every operand a number -- :num-args?, or a :dbl/:lng fast path, except `=`,
;; which can reach a type's own equiv) and the predicates that only look at the
;; value they are handed. Each op's :call and :fixed helpers. emit-call leaves
;; the site-slot restore off a non-tail call to one of these (returning-call?),
;; and a tail call to one leaves only the fn's own site pair behind
;; (*self-wrap*).
(def ^:private no-jolt-call-procs
  (let [preds #{"nil?" "some?" "identical?" "not" "zero?" "pos?" "neg?" "even?" "odd?"
                "chunked-seq?" "identity"}]
    ;; the array access helpers: each coerces a number or throws, and reads or
    ;; writes the backing -- nothing a jolt fn can run under
    (into #{"jolt-throw" "jolt-flaget" "jolt-flaset" "jolt-flaset-float"
            "jolt-vaget" "jolt-vaset" "jolt-baset"}
          (for [[op spec] op-registry/op-registry
                :when (and (not= op "=")
                           (or (:num-args? spec) (:dbl spec) (:lng spec) (contains? preds op)))
                proc (cons (:call spec) (vals (:fixed spec)))
                :when proc]
            proc))))
;; The Chez fixnum op that agrees with each jolt-bit-* helper when EVERY operand
;; is a fixnum, and the arity it applies at. Used by the bit-op inline in
;; emit-invoke; see the comment there for why the guard is a runtime test.
;; and/or/xor/not only: on a 61-bit tower their result is always representable as
;; a fixnum, since the bit pattern is bounded by the operands'. That is NOT true
;; of the shifts — bit-shift-left overflows fixnum range and jolt-bit-shift-left
;; wraps to 64 bits — so the shifts are absent here and keep their plain call.
;; Kept in this ns rather than derived from an op-registry fact on purpose: a NEW
;; var in op-registry referenced from here cannot be resolved by the running seed
;; during the first mint pass, so it analyzes as a host-static read, throws at ns
;; load, and takes every def below it with it.
(def bit-fx-ops
  {"jolt-bit-and" ["fxand" 2]
   "jolt-bit-or"  ["fxior" 2]
   "jolt-bit-xor" ["fxxor" 2]
   "jolt-bit-not" ["fxnot" 1]})
;; An operand whose evaluation has no observable effect: constants, locals,
;; var/the-var reads, quoted literals.
(defn- side-effect-free? [n]
  (or (contains? #{:const :local :var :the-var :quote} (:op n))
      ;; ...and a CONSTANT collection literal, whose value is fully determined at
      ;; emit time and which is hoisted rather than built here. Without this every
      ;; nested constant map/vector/set read as effectful, so needs-order? wrapped
      ;; its own construction in a let* of temporaries — and it is nested constants
      ;; that a macro-heavy expansion is made of. Measured on 1000 deftest forms:
      ;; 98000 ordering temporaries, one hoisted constant in every form wrapped in
      ;; an ordering let* it cannot need. The reference emits no ordering
      ;; temporaries for constants at all; they are constant-pool loads.
      (const-coll-node? n)))

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
(defn ordered-call [nodes strs build]
  (if (needs-order? nodes)
    (let [tmps (mapv (fn [_] (fresh-label "_a$")) strs)
          binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps strs))]
      (str "(let* (" binds ") " (build tmps) ")"))
    (build strs)))

;; If fnode is a clojure.core (or host) ref to a native-op primitive, return the
;; Scheme op string — only at an arity where the Scheme op and the jolt fn agree.
(defn native-op [fnode nargs]
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
;; itself up in its arg; a map/set/vector literal looks up its arg. Only at an
;; arity the callee's invoke has — Keyword and APersistentMap take one or two
;; args, APersistentSet and APersistentVector one. Any other count is nil, so
;; the call goes through jolt-invoke and throws the callee's ArityException
;; rather than a lookup quietly dropping the extra args (#1162).
(defn ifn-kind [fnode nargs]
  (case (:op fnode)
    :const (when (and (keyword? (:val fnode)) (<= 1 nargs 2)) :keyword)
    :map (when (<= 1 nargs 2) :coll)
    (:set :vector) (when (= 1 nargs) :coll)
    nil))

;; Polymorphic inline-cache width. MUST match jolt-pic-n in host/chez/protocols.ss:
;; the emitted scan reads slots [0..2N) and the epoch slot at 2N of the cache
;; vectors jolt-pic-empty and jolt-pic-add make, so the two must agree.
(def ^:private pic-n 4)
(def ^:private pic-epoch-idx (* 2 pic-n))
;; the eq? scan over a PIC cache's N (desc . impl) pairs: each clause returns the
;; impl when its cached desc is eq? to d. Strung together with `or` so a hit short-
;; circuits; a full miss falls through to the add helper (the caller appends it).
;; v is always a length-(2N+1) cache vector (the cell starts #f, read as
;; jolt-pic-empty, and is only ever set to one), and the indices are constants
;; below its length, so the vector type/bounds checks are redundant -- emit the
;; per-site unsafe variant.
(defn- pic-scan-clauses [v d]
  (str/join " "
            (for [i (range pic-n)]
              (str "(and (eq? (" (unsafe-prefix) "vector-ref " v " " (* 2 i) ") " d ")"
                   " (" (unsafe-prefix) "vector-ref " v " " (+ (* 2 i) 1) "))"))))

;; A reference into the Clojure stdlib (clojure.*) with no impl on Chez yet.
(defn stdlib-var? [n]
  (and (= :var (:op n)) (str/starts-with? (or (:ns n) "") "clojure.")))

;; Emit a :num-kind-tagged arithmetic call as a Chez flonum/fixnum op. inc/dec are
;; unary (fl +/- 1.0, fx1+/fx1-); the rest map through dbl-ops/lng-ops. Integer
;; literal operands and a :long var operand of a :double op were coerced to flonums
;; by jolt.passes.numeric (the literal as a compile-time flonum const, the :long var
;; via a :fl-coerce tag the emitter wraps in (fixnum->flonum ...)). A :double op is
;; only emitted once the numeric pass has PROVEN every operand is a flonum (that
;; proof gates the fl* specialization itself), so the flonum type check the safe fl
;; ops run is redundant there — emit the per-site unsafe #3% variant. There is no
;; matching unsafe variant for ^long +/-/*: a ^long is 64-bit and a Chez fixnum is
;; 61, so those map to the widening jolt-l+/-/* rather than any fx op.
;; The one-operand ops with a form of their own, by [kind name]: each a fn of
;; the emitted operand.
;; inc/dec tolerate a 64-bit operand (jolt-l-inc/dec fall back past fixnum range);
;; unchecked-inc/dec wrap (Java long). Neither can use the raising fx1+/fx1-.
;; On a proven flonum the unchecked forms are the plain flonum ops: they
;; wrap only longs (jolt-uncinc on 1.5 is 2.5, exactly what fl+ says).
(def ^:private numeric-unary-forms
  {[:double "inc"] (fn [a] (str "(" (unsafe-prefix) "fl+ " a " 1.0)"))
   [:double "dec"] (fn [a] (str "(" (unsafe-prefix) "fl- " a " 1.0)"))
   [:long "inc"] (fn [a] (str "(jolt-l-inc " a ")"))
   [:long "dec"] (fn [a] (str "(jolt-l-dec " a ")"))
   [:long "unchecked-inc"] (fn [a] (str "(jolt-uncinc " a ")"))
   [:long "unchecked-dec"] (fn [a] (str "(jolt-uncdec " a ")"))
   [:long "unchecked-negate"] (fn [a] (str "(jolt-uncneg " a ")"))
   [:double "unchecked-inc"] (fn [a] (str "(" (unsafe-prefix) "fl+ " a " 1.0)"))
   [:double "unchecked-dec"] (fn [a] (str "(" (unsafe-prefix) "fl- " a " 1.0)"))
   [:double "unchecked-negate"] (fn [a] (str "(" (unsafe-prefix) "fl- " a ")"))})

;; Every other op, through dbl-ops / lng-ops / bd-ops at any operand count.
(defn- emit-numeric-op [kind nm args order-args]
  (let [op (case kind :double (dbl-ops nm) :long (lng-ops nm) :bigdec (bd-ops nm))
        op (if (= kind :double) (str (unsafe-prefix) op) op)]
    (cond
      (and (contains? #{"<" "<=" ">" ">=" "==" "="} nm) (> (count args) 2))
      ;; a chained comparison (<= a b c) means (and (<= a b) (<= b c)); the fast
      ;; binary op is 2-ary, so expand rather than pass 3+ args to it. order-args
      ;; binds each operand to a temp once, so reusing a temp across pairs is safe.
      (order-args (fn [as]
        (str "(and " (str/join " " (map (fn [pair] (str "(" op " " (first pair) " " (second pair) ")"))
                                        (partition 2 1 as))) ")")))
      ;; Every fast-path op is BINARY — jolt-l+ and friends are 2-arg macros (3
      ;; operands is a syntax error at expansion, not a runtime one) and the
      ;; unchecked ops are 2-arg procs — while +/-/*/min/max are variadic. Lower N
      ;; operands as a left fold of the binary op, which is also the reference
      ;; semantics: (+ a b c) is (+ (+ a b) c), so each step overflow-checks
      ;; separately rather than one check over the whole sum. Each operand still
      ;; appears exactly once and in source order, so this is safe whether or not
      ;; order-args bound them to temps.
      (> (count args) 2)
      (order-args (fn [as]
        (reduce (fn [acc a] (str "(" op " " acc " " a ")")) (first as) (rest as))))
      ;; The other end: ONE operand. (+ x)/(* x)/(min x)/(max x) ARE x, and a
      ;; binary op has no 1-operand form to splice it into. A specialized operand
      ;; is already coerced (^long -> fixnum, ^double -> flonum), so unlike the
      ;; generic jolt-add there is nothing left to type-check either. `-` and `/`
      ;; are NOT identities here — they negate and reciprocate — and every
      ;; fast-path op spells those out, so they keep their call.
      ;; unchecked-add/-multiply are identities the same way: jolt's overlay folds
      ;; them over any arity ((apply unchecked-add [x]) => x), so the inline path
      ;; has to agree with it — a 2-arg proc handed one operand is an arity error.
      (and (= 1 (count args))
           (contains? #{"+" "*" "min" "max" "unchecked-add" "unchecked-multiply"} nm))
      (first args)
      ;; unchecked-subtract at one operand negates, matching both `-` and jolt's
      ;; own overlay ((apply unchecked-subtract [x]) => -x). jolt-uncsub2 has no
      ;; unary form, so subtract from zero — which wraps identically. On a
      ;; proven flonum it is fl-'s own unary negation (a fixnum 0 is not a
      ;; flonum, and negating keeps -0.0 where 0.0 - x would not).
      (and (= 1 (count args)) (= "unchecked-subtract" nm))
      (order-args (fn [as] (if (= kind :double)
                             (str "(" op " " (first as) ")")
                             (str "(" op " 0 " (first as) ")"))))
      :else
      (order-args (fn [as] (str "(" op " " (str/join " " as) ")"))))))

(defn emit-numeric [kind nm args order-args]
  (if-let [unary (get numeric-unary-forms [kind nm])]
    (unary (first args))
    (emit-numeric-op kind nm args order-args)))

;; slot of a declared field key in a record's field-order shape, or nil.
(defn struct-field-index [shape kw]
  (when shape
    (loop [i 0]
      (cond (>= i (count shape)) nil
            (= (nth shape i) kw) i
            :else (recur (inc i))))))
;; the direct per-arity slot accessor (jrecN-fI) for a field at idx in `shape`, or
;; nil when there is no per-arity accessor (>8 fields spill into jrec*, which stores
;; fields in a vector, not inline slots). Sound only for a proven-non-nil receiver.
(defn direct-field-accessor [shape idx]
  (let [n (count shape)]
    (when (<= n 8)
      (str "jrec" n "-f" idx))))

;; A plain Scheme application: (callee op ...).
(defn plain-call [callee operand-strs]
  (str "(" callee (if (seq operand-strs) (str " " (str/join " " operand-strs)) "") ")"))
;; A tail call in trace mode (R4, bead jolt-hm1p): ONE virtual-register store
;; of the static ('ns/fn' . line) site pair, after the operands are temp-bound
;; — an operand's own tail site would otherwise stomp the slot before the call
;; runs. The call stays the last form, so TCO is preserved. This is the WHOLE
;; runtime instrumentation; erased chains are reconstructed at report time
;; from the callsite tables. (The R1-R3 continuation-marks design recorded
;; chains at runtime and ballooned the heap on delegation-heavy code — every
;; mark op allocated; see rt.ss.) Self-tail calls never reach here (emit-call
;; elides them), so a tight self-loop stores once, not per iteration.
;; The static site pair literal: the cdr is the line, or #(line callee-fqn
;; call-site-line) when this site is code the inline pass copied out of another
;; fn — the same shape a trace marker carries, read back by the same accessors
;; (source-registry jolt-marker-entry-*). Without it a tail site inside a
;; spliced body reports the fn it was spliced INTO, at a line that fn does not
;; contain: this is the path the continuation walk cannot cover, because the
;; tail call erased the frame.
(defn site-literal [site-qname line inl-chain]
  (str "'(" (chez-str-lit site-qname) " . "
       (if (seq inl-chain)
         (str "#(" line " ("
              (str/join " " (map (fn [e] (str "(" (chez-str-lit (nth e 0))
                                              " . " (nth e 1) ")")) inl-chain))
              "))")
         (str line))
       ")"))
;; The inline chain a node carries, when every fn in it can be named by a marker.
(defn node-inline-chain [node]
  (let [c (get node :inline-chain)]
    (when (and (seq c) (every? (fn [e] (marker-safe-fqn? (nth e 0))) c)) c)))
(defn sited-tail-call
  ([site-qname line callee operand-strs] (sited-tail-call site-qname line callee operand-strs nil))
  ([site-qname line callee operand-strs inl-chain]
  (let [tts (mapv (fn [_] (fresh-label "_tt$")) operand-strs)
        binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tts operand-strs))
        site (site-literal site-qname line inl-chain)]
    (if (seq binds)
      (str "(let* (" binds ") (jolt-site! " site ") " (plain-call callee tts) ")")
      (str "(begin (jolt-site! " site ") " (plain-call callee operand-strs) ")")))))
;; A NON-tail call with tracing on is wrapped in site-restore, which puts the
;; site slot (vreg 2, rt.ss jolt-vreg-site) back as it was when the call returns. The tail sites
;; that ran inside the call stored pairs, and once it has returned none of them is
;; a live frame: left in the slot, the next throw from host code -- whose
;; innermost frame is a host one, with nothing to check the pair against --
;; reported the returned fn as its innermost frame. Put back rather than cleared,
;; because the slot can hold a genuine pair from below the call: a fn that was
;; itself tail-called keeps its erased caller across the calls it makes. A call
;; that cannot run a jolt fn (no-jolt-call-procs) is left bare.
(defn- returning-call? [callee]
  (not (contains? no-jolt-call-procs callee)))
;; Spelled out in Chez primitives rather than a runtime macro: a cached fasl is
;; then compiled from text that needs nothing of the runtime to expand (the AOT
;; cache gate recompiles one with a bare Chez), and the literal slot compiles to a
;; load and a store. rd / wr are the read form and the write head; a self-call
;; passes placeholders emit-fn fills in later (*self-wrap*).
;; The one-argument form also hands the slot's pair to the outer slot (vreg 12,
;; rt.ss jolt-vreg-outer-site) and clears the slot for the call's duration:
;; nothing the call runs is the tail call that pair describes, so a throw inside
;; it must not splice the pair innermost -- a pair a callback left behind when it
;; returned to untraced code (a core HOF) was reported as the innermost frame of
;; the next callback's throw. The pair is still the nearest erased caller from
;; further out, which the reporter reads from the outer slot as a gap bridge.
(defn site-restore
  ([call]
   (if *site-call*
     (do (reset! *site-call* true)
         (str "(jolt-site-call (lambda () " call "))"))
     (let [sv (fresh-label "_ss$") so (fresh-label "_so$") rv (fresh-label "_sr$")]
       (str "(let ((" sv " (virtual-register 2)) (" so " (virtual-register 12)))"
            " (if (pair? " sv ") (set-virtual-register! 12 " sv "))"
            " (set-virtual-register! 2 0)"
            " (let ((" rv " " call ")) (set-virtual-register! 2 " sv ")"
            " (set-virtual-register! 12 " so ") " rv "))"))))
  ([call rd wr]
   (let [sv (fresh-label "_ss$") rv (fresh-label "_sr$")]
     (str "(let ((" sv " " rd ")) (let ((" rv " " call ")) (" wr " " sv ") " rv "))"))))
;; A lookup the back end open-codes (jolt-get, a keyword or a collection in call
;; position) can still run jolt code: a deftype's valAt, a sorted map's
;; comparator, a key's equals. Not in tail position with tracing on, it restores
;; the site slot the way emit-call's non-tail calls do, or the callee that ran
;; inside it and returned was reported as the innermost frame of a later throw.
(defn lookup-restore [tail? call]
  (if (and (trace-frames?) (not tail?)) (site-restore call) call))
;; Emit a call. In tail position with tracing on, a call to a DIFFERENT fn than
;; the enclosing one stores its site pair (sited-tail-call), and a non-tail call
;; restores the slot when it returns (returning-call?). Everything else —
;; JOLT_TRACE=0, direct self-tail-calls, a tail call outside a named fn — is a
;; plain application, byte-identical to untraced code.
(defn emit-call
  ([tail? callee operand-strs line] (emit-call tail? callee operand-strs line nil))
  ([tail? callee operand-strs line inl-chain]
   (let [self? (contains? *trace-self* callee)]
     (when-not (or self? (contains? no-jolt-call-procs callee)) (note-tail-call! tail?))
     (cond
       (and (trace-frames?) tail? *trace-site* (not self?))
       (sited-tail-call *trace-site* line callee operand-strs inl-chain)
       (and (trace-frames?) (not tail?) self? *self-wrap*)
       (site-restore (plain-call callee operand-strs)
                     (str "(" (:tok *self-wrap*) "r)") (str (:tok *self-wrap*) "w"))
       (and (trace-frames?) (not tail?) (returning-call? callee))
       (site-restore (plain-call callee operand-strs))
       :else (plain-call callee operand-strs)))))

;; A record's positional constructor, inlined: the native per-arity ctor
;; (make-jrecN) applied to the descriptor (a per-site cell after warmup), no
;; extension map, and one inline slot per field — no var deref, no rest list, no
;; field vector, no class lookup. S is the record's shape from the unit's
;; record-shapes ({:fields :tags :type}); ARG-NODES / ARGS the field arguments,
;; nodes and emitted strings. Shared by (->R …) and (R. …), which construct the
;; same record.
(defn emit-record-ctor [s arg-nodes args]
  (let [tag (:type s)
        ;; A ^double field is widened on the way in, exactly as the dispatched
        ;; ctor does it (make-deftype-ctor's build calls the same jolt-rec-dbl).
        ;; This used to disqualify the whole inline path — which meant a
        ;; coordinate record, the shape the ^double machinery exists for, always
        ;; paid the slow ctor: jolt-invoke, var-deref, rest-list, ctor call,
        ;; hashtable lookup and a field vector.
        tags (vec (get s :tags))
        ;; ...but only over an argument that is not ALREADY a flonum.
        ;; jolt-rec-dbl is a runtime guard — (number? a) and (not (flonum? a))
        ;; before exact->inexact — so wrapping it around a proven double makes
        ;; the ^double DECLARATION cost two type tests per field per
        ;; construction that the same record without the tag does not pay.
        ;; Measured: (->Vec3 i (+ i 1) 2.5) in a loop ran 2.3x SLOWER declared
        ;; ^double than undeclared, all of it here. Extra static type
        ;; information must never make the emitted code slower than its
        ;; absence; where it cannot help it has to cost nothing.
        ;;
        ;; Proven means: a literal flonum (double? is exact here — a bigdec
        ;; and a ratio both answer false, and both still need the coercion),
        ;; or a node the numeric pass typed :double, whose emission is an fl
        ;; op and so yields a flonum by construction. Anything else keeps the
        ;; guard: a :long is 64-bit and may be a bignum at runtime, which is
        ;; exactly what jolt-rec-dbl's exact->inexact handles.
        proven-double? (fn [nd]
                         (and (map? nd)
                              (or (and (= :const (:op nd)) (double? (:val nd)))
                                  (= :double (:num-kind nd)))))
        ;; One tag per ARGUMENT (a record may declare fewer tags than the
        ;; ctor takes), so the three vectors below are the same length and
        ;; map together. Mapping rather than indexing three vectors apart is
        ;; the point: it is what makes pairing a field's tag with another
        ;; field's argument impossible to write.
        arg-tags (mapv (fn [i] (nth tags i nil)) (range (count arg-nodes)))
        ;; field-tag: what the FIELD declares. nd: the IR node being passed
        ;; into it. a: that node already emitted. Only nd can answer a
        ;; question about the value. Named field-tag, not tag, because `tag`
        ;; in this scope is the record's TYPE tag, bound above.
        coerce-arg (fn [field-tag nd a]
                     (if (and (= "double" field-tag) (not (proven-double? nd)))
                       (str "(jolt-rec-dbl " a ")") a))
        cached-desc (cached-tag-desc tag)]
    (ordered-call arg-nodes args (fn [as]
                  (let [n (count as)
                        as (vec (map coerce-arg arg-tags arg-nodes as))]
                    (if (<= n 8)
                      (str "(make-jrec" n " " cached-desc " jolt-nil 0"
                           (when (pos? n) (str " " (str/join " " as))) ")")
                      (str "(let ((v (vector " (str/join " " as) "))) (make-jrec " cached-desc " v jolt-nil))")))))))

;; A protocol call devirtualized to one record type.
(defn emit-devirt-call [node tail? order-args]
  (do
   (note-tail-call! tail?)
   (order-args (fn [as]
                 (let [r (fresh-label "_r$")
                      ;; a site whose impl has a contagion clone resolves the clone
                      ;; (fl* + exact->inexact on the :num operand) instead of the
                      ;; shared impl; otherwise the ordinary devirt-resolve. The
                      ;; non-specialized path is byte-identical (clone-sites empty
                      ;; outside a whole-program build).
                      resolver-name (if (clone-site? (:devirt-type node) (:devirt-proto node) (:devirt-method node))
                                       "devirt-resolve-fl" "devirt-resolve")
                      dv (str "(" resolver-name " " (chez-str-lit (:devirt-type node)) " "
                              (chez-str-lit (:devirt-proto node)) " " (chez-str-lit (:devirt-method node))
                              " " r ")")
                      cells *cache-cells*
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
                       resolver " " (str/join " " (cons r (rest as))) "))"))))))

;; A protocol call through a per-site polymorphic inline cache.
(defn emit-pic-call [node tail? order-args]
  (do
   (note-tail-call! tail?)
   (order-args (fn [as]
                (let [r (fresh-label "_r$")
                      d (fresh-label "_d$")
                      v (fresh-label "_v$")
                      cells *cache-cells*
                      proto (chez-str-lit (:proto node))
                      method (chez-str-lit (:method node))
                      apply-args (str/join " " (cons r (rest as)))]
                  (if cells
                    (let [c (fresh-label "_picv$")
                          scan (pic-scan-clauses v d)]
                      (swap! cells conj c)
                      ;; hot path inlined: bind the receiver, the site's cache
                      ;; vector and the receiver's desc; then, if the epoch still
                      ;; matches, eq?-scan the cached descs and apply the hit impl
                      ;; directly — no helper call after warmup. A miss (no cached
                      ;; desc / stale epoch) has jolt-pic-add build the next cache
                      ;; and the cell is set! to it: a cache vector is shared by
                      ;; every thread calling here, so none is written in place.
                      ;; A full cache gets the impl back instead (megamorphic).
                      ;; A receiver with no desc resolves without caching.
                      (let [miss (str "(let ((_nv (jolt-pic-add " v " " d " " proto " " method " " r ")))"
                                      " (if (procedure? _nv) _nv"
                                      " (begin (set! " c " _nv) (" (unsafe-prefix) "vector-ref _nv 1))))")]
                        (str "(let* ((" r " " (first as) ")"
                             " (" v " (or " c " jolt-pic-empty))"
                             " (" d " (jrec-pic-desc " r ")))"
                             " ((if (and " d " (" (unsafe-prefix) "fx= (" (unsafe-prefix) "vector-ref " v " " pic-epoch-idx ") jolt-proto-epoch))"
                             " (or " scan " " miss ")"
                             " (if " d " " miss " (protocol-resolve " proto " " method " " r ")))"
                             " " apply-args "))")))
                    (str "(let* ((" r " " (first as) "))"
                         " ((protocol-resolve " proto " " method " " r ") " apply-args "))")))))))

;; (aget ^doubles a i), read inline when the index is a proven long.
(defn emit-fl-aget [node tail? arg-nodes tl ich order-args]
  (let [an (first arg-nodes)
        hv (when (= :local (:op an)) (get *array-vecs* (munge-name (:name an))))]
    (order-args
     (fn [as]
       (let [wrap? (and (:fl-float node) (not (:fl-unbox node)))
             r (if (:fl-idx-long node)
                 (str "(flvector-ref " (or hv (str "(jolt-array-vec " (first as) ")")) " " (second as) ")")
                 ;; a call, sited like any other: in tail position its frame
                 ;; is gone, and a bad index faults inside the helper
                 (emit-call (and tail? (not wrap?)) "jolt-flaget" as tl ich))]
         (if wrap? (str "(make-jfloat " r ")") r))))))

;; (aset ^doubles a i v), written inline when the index is a proven long.
(defn emit-fl-aset [node tail? arg-nodes tl ich order-args]
  (let [an (first arg-nodes)
        hv (when (= :local (:op an)) (get *array-vecs* (munge-name (:name an))))
        fl? (:fl-float node)]
    (order-args
     (fn [as]
       (let [wrap? (and fl? (not (:fl-unbox node)))
             r (if (and (:fl-idx-long node) (:fl-val-double node))
                 (let [v (fresh-label "_v$")]
                   (str "(let ((" v " " (if fl? (str "(flsingle " (nth as 2) ")") (nth as 2)) ")) (flvector-set! "
                        (or hv (str "(jolt-array-vec " (first as) ")"))
                        " " (second as) " " v ") " v ")"))
                 (emit-call (and tail? (not wrap?)) (if fl? "jolt-flaset-float" "jolt-flaset") as tl ich))]
         (if wrap? (str "(make-jfloat " r ")") r))))))

;; A bit op on two fixnums, with the generic op as the fallback.
(defn emit-bit-fx-call [tail? args tl ich nop]
  (let [[fxop _] (bit-fx-ops nop)
        tmps (mapv (fn [_] (fresh-label "_bx$")) args)
        binds (str/join " " (map (fn [t a] (str "(" t " " a ")")) tmps args))
        tests (str/join " " (map (fn [t] (str "(fixnum? " t ")")) tmps))
        test (if (= 1 (count tmps)) tests (str "(and " tests ")"))]
    (str "(let* (" binds ") (if " test
         " (" fxop " " (str/join " " tmps) ")"
         " " (emit-call tail? nop tmps tl ich) "))")))

;; A host static method call, through a per-site cache inside a pool.
(defn emit-host-static-call [tail? fnode order-args]
  (if (do (note-tail-call! tail?) (and *const-pool* (var-cache?)))
    (let [site (hoist-const-per-site "(host-static-site-make)")]
      (order-args (fn [as]
                    (str "((host-static-proc-site " site " " (chez-str-lit (:class fnode)) " "
                         (chez-str-lit (:member fnode)) " " (count as) ")"
                         (if (empty? as) "" (str " " (str/join " " as))) ")"))))
    (order-args (fn [as]
                  (str "(host-static-call " (chez-str-lit (:class fnode)) " " (chez-str-lit (:member fnode))
                       (if (empty? as) "" (str " " (str/join " " as))) ")")))))

;; jolt.host/rec-type?, answered from the record type the inference proved.
(defn emit-rec-type-check [node arg-nodes order-args]
  (order-args (fn [as]
                (str "(and"
                     (str/join "" (map (fn [i]
                                         (let [shape (get (ctor-shapes) (nth (:rec-guard node) i))
                                               nf (count (:fields shape))
                                               ;; the per-arity layout's own predicate: the
                                               ;; proven branch reads with that layout's
                                               ;; accessors, and cp0 folds their type tests
                                               ;; into this one
                                               pred (if (<= 1 nf 8) (str "jrec" nf "?") "jrec?")]
                                           ;; fresh binders: a fixed name would shadow
                                           ;; a user local the value expression reads
                                           (let [d (fresh-label "_rgd$") x (fresh-label "_rgx$")]
                                             (str " (let ((" d " " (cached-tag-desc (:val (nth arg-nodes (inc (* 2 i))))) ")"
                                                  " (" x " " (nth as (* 2 i)) "))"
                                                  " (and " d " (" pred " " x ") (eq? (jrec-desc " x ") " d ")))"))))
                                       (range (quot (count as) 2))))
                     ")"))))

