;; clojure.core — syntax tier. The control macros the compiler and every later
;; tier depend on (when/cond/and/or/...), expressed as defmacro. Loaded FIRST
;; (before 00-kernel), interpreted, so the macros exist before any code that uses
;; them is compiled — including the kernel tier, the self-hosted analyzer, and the
;; seq/coll tiers.
;;
;; CONSTRAINT: code here may use ONLY special forms (if/do/let*/fn*/not) and
;; SEED primitives (first/next/rest/nth/count/seq/...), plus earlier defs in
;; THIS file. It must NOT use kernel-tier fns (second/peek/subvec/...) or
;; anything defined later — those don't exist yet when this tier loads. Raw
;; fn*/let* (no destructuring) and no when/cond/and/or above their defmacros.
;;
;; This tier's defns load interpreted and are recompiled by the staged pass
;; (backend/recompile-defns!) once the analyzer is alive — same lifecycle as
;; the defmacro expanders.

;; zero?/pos?/every? live HERE (not 20-coll): empty? below calls zero?, and
;; the self-hosted analyzer — compiled right after the kernel tier — uses all
;; three. Raw def+fn* per the file constraint. zero? checks number? itself
;; (= doesn't throw); pos? inherits throwing from >.
(def zero?
  (fn* zero? [x]
    (if (number? x)
      (= x 0)
      (throw (str "zero? requires a number, got: " x)))))

;; pos? checks number? explicitly: this tier is recompiled by the staged pass,
;; where a bare (> x 0) emits the native op that happily orders strings
;; (the documented native-ops relaxation) — the guard keeps Clojure's throw.
(def pos?
  (fn* pos? [x]
    (if (number? x)
      (> x 0)
      (throw (str "pos? requires a number, got: " x)))))

;; Canonical every?: short-circuits on the first falsey result, so infinite
;; seqs with an early counterexample terminate.
(def every?
  (fn* every? [pred coll]
    (if (nil? (seq coll))
      true
      (if (pred (first coll))
        (recur pred (next coll))
        false))))

;; empty?/keys/vals live HERE (not 20-coll) because the expanders below call
;; them at expansion time, which first happens during the kernel-tier compile.
;; empty? keeps O(1) dispatch for counted things; only the lazy/list fallback
;; goes through seq's cell check.
(def empty?
  (fn* empty? [coll]
    (if (nil? coll)
      true
      (if (vector? coll)
        (zero? (count coll))
        (if (map? coll)
          (zero? (count coll))
          (if (set? coll)
            (zero? (count coll))
            (if (string? coll)
              (zero? (count coll))
              (nil? (seq coll)))))))))

;; Canonical: the seq of entries/elements, projected. (keys {}) is nil; sorted
;; maps iterate in comparator order ((seq sm) is the value's own :seq op).
(def keys
  (fn* keys [m]
    (let* [s (seq m)]
      (if s (map (fn* [e] (nth e 0)) s) nil))))

(def vals
  (fn* vals [m]
    (let* [s (seq m)]
      (if s (map (fn* [e] (nth e 1)) s) nil))))

(defmacro when [test & body]
  `(if ~test (do ~@body)))

(defmacro when-not [test & body]
  `(if (not ~test) (do ~@body)))

(defmacro and [& exprs]
  (if (empty? exprs)
    true
    (if (empty? (rest exprs))
      (first exprs)
      `(let* [and# ~(first exprs)] (if and# (and ~@(rest exprs)) and#)))))

(defmacro or [& exprs]
  (if (empty? exprs)
    nil
    (if (empty? (rest exprs))
      (first exprs)
      `(let* [or# ~(first exprs)] (if or# or# (or ~@(rest exprs)))))))

;; :else (any truthy value) is just a test, so no special case — (if :else e ...)
;; takes e.
(defmacro cond [& clauses]
  (if (empty? clauses)
    nil
    `(if ~(first clauses) ~(nth clauses 1) (cond ~@(drop 2 clauses)))))

;; ns is sugar over the namespace-op fns (in-ns/require/use/import/refer-clojure,
;; all ctx-capturing clojure.core fns) — matching Clojure, where require is a fn and
;; the ns macro expands its clauses into require calls. Each spec is quoted
;; individually and passed as data; non-list clauses (docstring, attr-map,
;; :gen-class, …) are ignored. So ns compiles to a plain (do …) of invokes.
;; MUST live in this first tier: the self-hosted analyzer build (triggered while
;; 10-seq loads) processes jolt.analyzer's own (ns …) form, so ns has to exist by
;; then. Its body resolves fn/map/reduce/cond at EXPANSION time, by which point all
;; of 00-syntax has loaded, so using them here is fine.
(defmacro ns [nm & clauses]
  ;; ^{:map} metadata on the ns name reads as a (with-meta sym {...}) form, not an
  ;; annotated symbol. Real libraries put :author/:doc there
  ;; (clojure.tools.logging), so unwrap to the bare symbol; jolt does not track
  ;; namespace metadata, so the map is dropped.
  (let [nm (if (and (seq? nm) (= 'with-meta (first nm))) (second nm) nm)
        calls (reduce
                (fn [acc clause]
                  ;; a reference clause may be a list (:require …) or a vector
                  ;; [:require …]; Clojure accepts both, dispatching on (first clause).
                  (if (or (seq? clause) (vector? clause))
                    (let [head (first clause) args (rest clause)]
                      (cond
                        (= head :require) (conj acc `(require ~@(map (fn [s] `(quote ~s)) args)))
                        (= head :use)     (conj acc `(use ~@(map (fn [s] `(quote ~s)) args)))
                        (= head :import)  (conj acc `(import ~@(map (fn [s] `(quote ~s)) args)))
                        (= head :refer-clojure)
                          (conj acc `(refer-clojure ~@(map (fn [s] `(quote ~s)) args)))
                        :else acc))
                    acc))
                [] clauses)]
    `(do (in-ns (quote ~nm)) ~@calls)))

;; Threading: a list form threads x in as the first (->) or last (->>) arg; a bare
;; symbol becomes (form x). Recursive; the expand-once cache makes that free.
(defmacro -> [x & forms]
  (if (empty? forms)
    x
    (let [form (first forms)
          threaded (if (seq? form)
                     `(~(first form) ~x ~@(rest form))
                     `(~form ~x))]
      `(-> ~threaded ~@(rest forms)))))

(defmacro ->> [x & forms]
  (if (empty? forms)
    x
    (let [form (first forms)
          threaded (if (seq? form)
                     `(~(first form) ~@(rest form) ~x)
                     `(~form ~x))]
      `(->> ~threaded ~@(rest forms)))))

;; Forward declaration interns unbound vars (Clojure semantics). The interpreter
;; resolves forward refs lazily either way, but the COMPILER classifies globals at
;; compile time: without the var, a declared name that collides with a host root
;; binding (parse, hash, …) would compile to the host fn instead of the var.
(defmacro declare [& syms]
  `(do ~@(map (fn* [s] `(def ~s)) syms)))

;; letfn is a macro over the letfn* special form, matching Clojure: each
;; (name [params] body*) spec becomes a name + a (fn name [params] body*) binding.
;; So (macroexpand-1 '(letfn …)) yields the letfn* form macroexpansion tooling
;; (tools.macro / tools.analyzer) expects, instead of an opaque special form.
(defmacro letfn [fnspecs & body]
  (cons 'letfn*
        (cons (reduce (fn [acc s] (conj (conj acc (first s)) (cons 'fn s))) [] fnspecs)
              body)))

;; destructure — Clojure's binding-vector expander.
;; Turns a binding vector that may contain destructuring
;; patterns into a plain binding vector (alternating symbol / init-form) built from
;; nth/nthnext/get, so the COMPILER only ever sees plain symbols (analyze-bindings
;; rejects patterns). `let` consumes it directly; `loop`/`fn` reuse it transitively
;; through `let`. Written with let*/fn* and seed primitives only — it never uses
;; let/loop/fn, so expanding its own body can't recurse back into destructure.
;; Note map? is true for symbol structs too, so the symbol? clause must come first.
;; def+fn* (not defn) because the defn macro is not defined until later in the tier.
(def destructure
 (fn* destructure [bindings]
  (let* [find-or
           (fn* [or-map nm]
             (reduce (fn* [acc k]
                       (if (and (symbol? k) (= nm (name k)))
                         [true (get or-map k)]
                         acc))
                     [false nil]
                     (if or-map (keys or-map) [])))
         amp? (fn* [x] (and (symbol? x) (= "&" (name x))))
         ;; split a :keys/:syms/:strs name list at & into [sym bind?] pairs. Names
         ;; before & bind normally (bind? true); names after & are declared-only
         ;; (bind? false) — accepted keys (:keys) or required keys (:keys!), per
         ;; CLJ-2961.
         classify
           (fn* [names]
             (nth (reduce (fn* [st x]
                            (if (amp? x)
                              [(nth st 0) false]
                              [(conj (nth st 0) [x (nth st 1)]) (nth st 1)]))
                          [[] true] names)
                  0))
         proc
           (fn* proc [pat init acc]
             (cond
               ;; CLJ-2954: & is reserved for destructuring rest, never a binding.
               (amp? pat) (throw (new IllegalArgumentException "Can't use & as a local binding"))
               (symbol? pat) (conj (conj acc pat) init)
               (vector? pat)
                 (let* [g (symbol (str (gensym)))
                        n (count pat)
                        vloop
                          (fn* vloop [i idx a]
                            (if (< i n)
                              (let* [elem (nth pat i)]
                                (cond
                                  (amp? elem)
                                    (vloop (+ i 2) idx (proc (nth pat (inc i)) `(nthnext ~g ~idx) a))
                                  (= elem :as)
                                    (vloop (+ i 2) idx (proc (nth pat (inc i)) g a))
                                  :else
                                    (vloop (inc i) (inc idx) (proc elem `(nth ~g ~idx nil) a))))
                              a))]
                   (vloop 0 0 (conj (conj acc g) init)))
               (map? pat)
                 (let* [g (symbol (str (gensym)))
                        gm (symbol (str (gensym)))
                        ;; kwargs: a map pattern may bind against the sequential rest
                        ;; of a fn — (& {:keys [...]}) — a seq of alternating k/v args,
                        ;; optionally with a trailing map (Clojure 1.11: (f :a 1 {:b 2})
                        ;; merges the map over the pairs; (f {:a 1}) is just the map).
                        ;; An odd count means the last arg is that trailing map. A real
                        ;; map value is used as-is, so ordinary map destructuring is
                        ;; unaffected. g holds init once; gm is the coerced map every
                        ;; lookup (and :as) reads from.
                        coerce `(if (sequential? ~g)
                                  (if (odd? (count ~g))
                                    (merge (apply hash-map (butlast ~g)) (last ~g))
                                    (apply hash-map ~g))
                                  ~g)
                        or-map (get pat :or)
                        as-sym (get pat :as)
                        bound (conj (conj (conj (conj acc g) init) gm) coerce)
                        base (if as-sym (conj (conj bound as-sym) gm) bound)
                        ;; group binds a :keys/:strs/:syms list. dnsp is the destructuring
                        ;; namespace from a qualified key like :ns/keys — it both prefixes
                        ;; the lookup key and overrides a bare symbol's namespace.
                        ;; group binds a :keys/:strs/:syms list. checked? marks the
                        ;; :keys!/:strs!/:syms! variants (CLJ-2961): lookups use req!
                        ;; (throw on missing) instead of get. A pair is [sym bind?];
                        ;; bind? false (names after &) is declared-only — for checked
                        ;; groups it still runs req! (bound to a throwaway gensym) to
                        ;; enforce the key, for unchecked groups it's a no-op.
                        group
                          (fn* group [a names kind dnsp checked?]
                            (if names
                                (reduce
                                  ;; s is a symbol (a b) or a keyword (:a :b); name/
                                  ;; namespace handle both, so :keys [:major] binds
                                  ;; `major` looking up :major (str would keep the colon).
                                  (fn* [aa pair]
                                    (let* [s (nth pair 0)
                                           bind? (nth pair 1)
                                           local (name s)
                                           nsp (or (namespace s) dnsp)
                                           keyform (cond
                                                     (= kind :kw) (keyword (if nsp (str nsp "/" local) local))
                                                     (= kind :str) local
                                                     :else `(quote ~(symbol nsp local)))
                                           fo (find-or or-map local)
                                           lookup (cond
                                                    checked? `(req! ~gm ~keyform)
                                                    (nth fo 0) `(get ~gm ~keyform ~(nth fo 1))
                                                    :else `(get ~gm ~keyform))]
                                      (cond
                                        bind?    (conj (conj aa (symbol local)) lookup)
                                        checked? (conj (conj aa (symbol (str (gensym)))) lookup)
                                        :else    aa)))
                                  a (classify names))
                                a))
                        g1 (group base (get pat :keys)  :kw  nil false)
                        g2 (group g1   (get pat :strs)  :str nil false)
                        g3 (group g2   (get pat :syms)  :sym nil false)
                        g4 (group g3   (get pat :keys!) :kw  nil true)
                        g5 (group g4   (get pat :strs!) :str nil true)
                        g6 (group g5   (get pat :syms!) :sym nil true)]
                   ;; remaining keys: a qualified :ns/keys|:ns/strs|:ns/syms groups under
                   ;; its namespace; any other keyword is skipped; a non-keyword is a
                   ;; nested binding pattern.
                   (reduce (fn* [a k]
                             (if (keyword? k)
                               (let* [kn (name k) kns (namespace k)]
                                 (cond
                                   (and kns (= kn "keys"))  (group a (get pat k) :kw  kns false)
                                   (and kns (= kn "strs"))  (group a (get pat k) :str kns false)
                                   (and kns (= kn "syms"))  (group a (get pat k) :sym kns false)
                                   (and kns (= kn "keys!")) (group a (get pat k) :kw  kns true)
                                   (and kns (= kn "strs!")) (group a (get pat k) :str kns true)
                                   (and kns (= kn "syms!")) (group a (get pat k) :sym kns true)
                                   :else a))
                               ;; a direct binding {x :x}: apply its :or default
                               ;; (keyed by the local symbol) when the key is absent.
                               (let* [fo (if (symbol? k) (find-or or-map (name k)) [false nil])]
                                 (proc k (if (nth fo 0)
                                           `(get ~gm ~(get pat k) ~(nth fo 1))
                                           `(get ~gm ~(get pat k)))
                                       a))))
                           g6 (keys pat)))
               :else (throw (str "unsupported destructuring pattern: " (pr-str pat)))))
         ploop
           (fn* ploop [i acc]
             (if (< i (count bindings))
               (ploop (+ i 2) (proc (nth bindings i) (nth bindings (inc i)) acc))
               acc))]
    (ploop 0 []))))

;; let desugars destructuring patterns to plain bindings (via destructure) so the
;; COMPILER sees only plain symbols — analyze-bindings rejects patterns as
;; uncompilable, relying on this macro to have expanded them. (The interpreter
;; could destructure let* directly, but the compiler can't.) let* is sequential, so
;; a later init can reference an earlier destructured name. Splice via [~@..] so the
;; binding vector is a tuple form (destructure returns a pvec), not a pvec literal.
(defmacro let [bindings & body]
  `(let* [~@(destructure bindings)] ~@body))

;; loop binds destructuring forms like let, but recur must target the loop* vars,
;; whose count can't change. So (matching Clojure): gensym one loop var per binding,
;; loop* over those, and destructure them via an inner let each iteration; an outer
;; let establishes the destructured names so later inits can see them. Plain loops
;; (no patterns) pass straight through to loop*.
(defmacro loop [bindings & body]
  (let [d (destructure bindings)]
    (if (= d bindings)
      `(loop* ~bindings ~@body)
      (let [bs (take-nth 2 bindings)
            vs (take-nth 2 (drop 1 bindings))
            gs (map (fn [b] (if (symbol? b) b (symbol (str (gensym))))) bs)
            outer (reduce (fn [acc t]
                            (let [b (nth t 0) v (nth t 1) g (nth t 2)]
                              (if (symbol? b) (conj (conj acc g) v)
                                  (conj (conj (conj (conj acc g) v) b) g))))
                          [] (map vector bs vs gs))
            inner (reduce (fn [acc t] (conj (conj acc (nth t 0)) (nth t 1)))
                          [] (map vector bs gs))
            loopv (reduce (fn [acc g] (conj (conj acc g) g)) [] gs)]
        ;; splice via [~@..] so the binding vectors are tuple forms, not pvecs.
        `(let [~@outer] (loop* [~@loopv] (let [~@inner] ~@body)))))))

;; fn: desugar destructuring params to plain symbols + a body let (matching
;; Clojure's maybe-destructured), so fn* only ever sees plain params (the compiler's
;; analyze-fn requires that). Plain params pass through untouched. Handles an
;; optional name and single- or multi-arity. md/mk are fn* (not fn) to avoid a cycle.
;; md walks a param seq, replacing non-symbol patterns with gensyms and recording
;; [pattern gensym] let-bindings; mk turns one arity (params . body) into a rewritten
;; arity. Output: single arity splices the arity's elements straight into fn*; multi
;; arity splices the rewritten clauses.
(defmacro fn [& raw]
  (let [nm (if (symbol? (first raw)) (first raw) nil)
        aftn (if nm (next raw) raw)
        ;; a return-type hint (defn f ^bytes [x] ...) reaches us as a
        ;; (with-meta [x] {:tag ...}) FORM in params position — unwrap it
        ;; (the hint means nothing on jolt; ring-codec carries several).
        unhint (fn* [x]
                 (if (if (seq? x) (= 'with-meta (first x)) false)
                   (nth x 1)
                   x))
        ;; a :pre/:post conditions map (a leading map when the body has more forms
        ;; after it) becomes assertions: pre before the body, then bind % to the
        ;; result, post after, return %. (map? is a native, so this is tier-safe;
        ;; the assert/map calls only run when a conditions map is actually present.)
        wrap-conds
          (fn* [body]
            (if (if (map? (first body)) (next body) false)
              (let [conds (first body)
                    real (next body)
                    mka (fn* [cs] (map (fn* [c] `(assert ~c)) cs))]
                `(~@(mka (get conds :pre))
                  (let [~'% (do ~@real)]
                    ~@(mka (get conds :post))
                    ~'%)))
              body))
        md (fn* go [ps nps lets]
             (if (seq ps)
               (if (symbol? (first ps))
                 (go (next ps) (conj nps (first ps)) lets)
                 ;; a bare (gensym) returns a host symbol the destructurer rejects;
                 ;; round-trip through str for a jolt symbol.
                 (let [g (symbol (str (gensym)))]
                   (go (next ps) (conj nps g) (conj (conj lets (first ps)) g))))
               [nps lets]))
        mk (fn* [sig]
             (let [ps (unhint (first sig))
                   hinted (not (= ps (first sig)))
                   r (md (seq ps) [] [])
                   raw-body (rest sig)
                   body (wrap-conds raw-body)
                   conds? (not (= body raw-body))]
               (if (if (empty? (nth r 1)) (if (not hinted) (not conds?) false) false)
                 sig
                 ;; build the params/let vectors via [~@..] so they are tuple forms
                 ;; (the accumulators are plain seqs, the wrong representation).
                 ;; A hinted-but-undestructured arity also rebuilds, to shed the
                 ;; with-meta wrapper without changing the clause representation.
                 (let [pv `[~@(nth r 0)]
                       lv `[~@(nth r 1)]]
                   (if (empty? (nth r 1))
                     `(~pv ~@body)
                     `(~pv (let ~lv ~@body)))))))]
    (if (vector? (unhint (first aftn)))
      (let [a (mk aftn)]
        (if nm `(fn* ~nm ~@a) `(fn* ~@a)))
      (let [as (vec (map mk aftn))]
        (if nm `(fn* ~nm ~@as) `(fn* ~@as))))))

;; defn: drop an optional leading docstring and attr-map, then (def name (fn ...)).
;; Emits the fn MACRO (not the fn* primitive) so destructuring params desugar — fn*
;; requires plain symbols (like Clojure). Unnamed (as before): self-recursion
;; resolves through the def'd var, so this only adds the desugaring step.
;; Both single- and multi-arity reduce to (fn ~@body) — fn takes either a params
;; vector + body or a sequence of ([params] body) clauses, so no arity branching is
;; needed. (map? is true for symbol forms too, so guard the attr-map with symbol?.)
;; Defined before fresh-sym below, which is a defn-.
;; defn lives in the earliest tier, so its macro body may only use primitives
;; available before the seq/coll tiers — conj (which merges a map onto a map),
;; assoc, meta, with-meta — not merge/last/butlast.
(defmacro defn [fn-name & body]
  (let [docstring (when (and (seq body) (string? (first body))) (first body))
        body (if docstring (rest body) body)
        ;; the attr-map after an optional docstring (or after the name) — its keys
        ;; merge into the var metadata, like Clojure. A map in the first arity
        ;; position is the attr-map only when more body follows (else it is a lone
        ;; map body) and is never a symbol (a name carries its meta as a form).
        attr-map (when (and (seq body) (next body) (map? (first body)) (not (symbol? (first body))))
                   (first body))
        body (if attr-map (rest body) body)
        ;; the bare name + any ^{:map} metadata the reader attached to it.
        fn-only-name (if (symbol? fn-name) fn-name (first (rest fn-name)))
        name-meta (meta fn-only-name)
        m1 (if attr-map (if name-meta (conj name-meta attr-map) attr-map) name-meta)
        meta-map (if docstring (assoc (if m1 m1 {}) :doc docstring) m1)]
    ;; pass the name through to fn: the compiled fn's host name carries it, so
    ;; stack traces read app.deep/level3 instead of a gensym. All metadata
    ;; (docstring + attr-map + the name's own) is attached to the def name symbol,
    ;; which analyze-def reads and evaluates — so (meta #'f) reflects every source.
    (if meta-map
      `(def ~(with-meta fn-only-name meta-map) (fn ~(with-meta fn-only-name nil) ~@body))
      `(def ~fn-only-name (fn ~fn-only-name ~@body)))))

;; defn- marks the var :private (like Clojure). Jolt doesn't restrict access, but
;; ns-publics filters private vars out — a lib that introspects ns-publics (e.g.
;; honeysql's "all helpers have docstrings") sees only the public ones.
(defmacro defn- [fn-name & body]
  `(defn ~(with-meta fn-name (assoc (if (meta fn-name) (meta fn-name) {}) :private true)) ~@body))

;; A fresh jolt symbol inside a macro body (a bare (gensym) returns a host symbol
;; the destructurer rejects). This defn compiles fine: by the time a tier triggers
;; the analyzer build the kernel is in place (the build is gated until then).
(defn- fresh-sym [] (symbol (str (gensym))))

;; cond->: thread expr through each (test form) pair, only when the test is truthy.
;; Linear nested let*, a distinct fresh symbol per step.
(defmacro cond-> [expr & clauses]
  (let [step (fn step [prev cls]
               (if (empty? cls)
                 prev
                 (let [t (first cls)
                       f (nth cls 1)
                       gn (fresh-sym)
                       call (if (seq? f) `(~(first f) ~prev ~@(rest f)) `(~f ~prev))]
                   `(let* [~gn (if ~t ~call ~prev)] ~(step gn (drop 2 cls))))))
        g0 (fresh-sym)]
    `(let* [~g0 ~expr] ~(step g0 clauses))))

;; case: nested =/or tests (no jump table). Test constants are NOT evaluated —
;; symbols and list constants are quoted; a list in test position is a set (or).
(defmacro case [expr & clauses]
  (let [g (fresh-sym)
        mk-const (fn [c] (if (or (symbol? c) (seq? c)) `(quote ~c) c))
        mk-test (fn [c]
                  (if (seq? c)
                    `(or ~@(map (fn [v] `(= ~g ~(mk-const v))) c))
                    `(= ~g ~(mk-const c))))
        ;; Collect test constants pairwise (so a trailing unpaired default is
        ;; excluded), flattening list/or-group tests into individual constants.
        ;; seed-only fns (reduce/conj/first/rest/drop/empty?/seq?) — analyzer.clj
        ;; uses case during its own build, before some/distinct load.
        collect (fn* collect [cls acc]
                  (if (or (empty? cls) (empty? (rest cls)))
                    acc
                    (let [t (first cls)
                          acc (if (seq? t) (reduce conj acc t) (conj acc t))]
                      (collect (drop 2 cls) acc))))
        ;; first duplicate constant, wrapped in [x] (so a duplicate nil is detected);
        ;; nil = none. Clojure rejects duplicate case constants at compile time.
        first-dup (fn* fd [items seen]
                    (if (empty? items)
                      nil
                      (let [x (first items)]
                        (if (reduce (fn [f s] (or f (= s x))) false seen)
                          [x]
                          (fd (rest items) (conj seen x))))))
        dup (first-dup (collect clauses []) [])
        build (fn build [cls]
                (if (empty? cls)
                  ;; no clause matched and no default — Clojure throws here.
                  `(throw (ex-info (str "No matching clause: " ~g) {}))
                  (if (empty? (rest cls))
                    (first cls)
                    `(if ~(mk-test (first cls)) ~(nth cls 1) ~(build (drop 2 cls))))))]
    (if dup
      (throw (str "Duplicate case test constant: " (first dup)))
      `(let* [~g ~expr] ~(build clauses)))))

;; for: list comprehension, desugared to nested map/mapcat over the binding colls.
;; Per binding group: :when wraps the inner form in (if test (list inner) []) so
;; mapcat drops it when false; :let wraps it in a let*; :while wraps the coll in
;; take-while. The last group with no modifiers is a plain map (no flatten needed).
;; Single body expr. The body uses only kernel/seed fns so it runs at
;; analyzer-build time. `fn` (not fn*) carries the binding so destructuring forms
;; work.
(defmacro for [bindings body]
  (let [scan (fn scan [bvec i bind coll mods]
               (if (and (< i (count bvec)) (keyword? (nth bvec i)))
                 (let [k (nth bvec i)
                       v (nth bvec (inc i))]
                   (cond
                     (= k :when)  (scan bvec (+ i 2) bind coll (conj mods [:when v]))
                     (= k :let)   (scan bvec (+ i 2) bind coll (conj mods [:let v]))
                     (= k :while) (scan bvec (+ i 2) bind `(take-while (fn [~bind] ~v) ~coll) mods)
                     :else        (scan bvec (inc i) bind coll mods)))
                 [i bind coll mods]))
        parse-groups (fn parse-groups [bvec i groups]
                       (if (>= i (count bvec))
                         groups
                         (let [r (scan bvec (+ i 2) (nth bvec i) (nth bvec (inc i)) [])]
                           (parse-groups bvec (nth r 0)
                                         (conj groups [(nth r 1) (nth r 2) (nth r 3)])))))
        ;; Apply the group's modifiers around a contribution that is ALREADY a seq
        ;; (a (list body) for the last group, an inner comprehension otherwise), so
        ;; :when just returns it or [] — no extra (list ...) that mapcat couldn't
        ;; flatten. :let binds around it; mods apply outer-to-inner (left to right).
        wrap-mods (fn wrap-mods [mods inner]
                    (if (empty? mods)
                      inner
                      (let [m (first mods)
                            sub (wrap-mods (rest mods) inner)]
                        (if (= (first m) :when)
                          `(if ~(nth m 1) ~sub [])
                          ;; `let` (not let*) so a :let binding may itself
                          ;; destructure — (for [x xs :let [{:keys [y]} x]] …).
                          `(let ~(nth m 1) ~sub)))))
        build (fn build [idx groups]
                (let [g (nth groups idx)
                      my-bind (nth g 0)
                      my-coll (nth g 1)
                      my-mods (nth g 2)
                      is-last (= idx (dec (count groups)))]
                  (if (and is-last (empty? my-mods))
                    ;; fast path: last group, no modifiers -> a plain map of body
                    `(map (fn [~my-bind] ~body) ~my-coll)
                    ;; general: mapcat over a seq contribution (wrap a last-group
                    ;; body in a one-element list so mapcat yields the bodies).
                    (let [base (if is-last `(list ~body) (build (inc idx) groups))]
                      `(mapcat (fn [~my-bind] ~(wrap-mods my-mods base)) ~my-coll)))))]
    (if (>= (count bindings) 2)
      (build 0 (parse-groups bindings 0 []))
      body)))

;; doseq runs body for side effects across the bindings, returning nil. Realizes
;; a `for` comprehension with count (for handles :when/:let/:while and multiple
;; bindings).
(defmacro doseq [bindings & body]
  `(do (count (for ~bindings (do ~@body nil))) nil))

;; when-let must live in this (early) tier, not 30-macros with its if-let/if-some/
;; when-some siblings: 20-coll uses it (not-empty), and 20-coll loads before 30. The
;; name binds only in the taken branch (temp# tests the value); via `let` so the
;; binding form may itself destructure, matching Clojure.
(defmacro when-let [bindings & body]
  (when (not= 2 (count bindings))
    (throw (new IllegalArgumentException "when-let requires exactly 2 forms in binding vector")))
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if temp# (let [~form temp#] ~@body) nil))))

;; lazy-seq / lazy-cat live here (not 30-macros) because the seq/coll tiers use
;; them and compile-as-they-load: the macro must be registered before those tiers
;; or (lazy-seq …) compiles to a call of the macro-as-function and leaks its
;; expansion at runtime. They use only seed fns (make-lazy-seq/
;; coll->cells/concat) + map, all available from the start.
;; lazy-seq defers its body: make-lazy-seq holds a thunk that realizes the body
;; to cells when forced. lazy-cat wraps each coll in a lazy-seq and concats.
(defmacro lazy-seq [& body]
  `(make-lazy-seq (fn* [] (coll->cells (do ~@body)))))

(defmacro lazy-cat [& colls]
  `(concat ~@(map (fn [c] `(lazy-seq ~c)) colls)))

;; not= here (not 20-coll): the kernel tier uses it, and the kernel
;; bootstrap-compiles right after this file loads. Canonical Clojure arities.
(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more] (not (apply = x y more))))

;; unreduced here: the seq tier's reduce machinery unwraps with it.
(defn unreduced [x] (if (reduced? x) (deref x) x))
