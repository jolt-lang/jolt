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

;; import is a MACRO like the JVM's: its specs are never evaluated, so
;; (import [java.nio.file Paths]) works bare — under strict resolution the
;; old function-import analyzed java.nio.file as a value and threw. Specs
;; arriving already quoted (the ns macro above quotes its clause args) are
;; passed through, mirroring clojure.core/import's quote-unwrap. The runtime
;; work happens in __import (host/chez/java/natives-str.ss).
(defmacro import [& specs]
  `(clojure.core/__import
     ~@(map (fn [s] (if (and (seq? s) (= 'quote (first s))) s (list 'quote s)))
            specs)))

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
  (let* [amp? (fn* [x] (and (symbol? x) (= "&" (name x))))
         proc
           (fn* proc [pat init acc]
             (cond
               ;; CLJ-2954: & is reserved for destructuring rest, never a binding.
               (amp? pat) (throw (new IllegalArgumentException "Can't use & as a local binding"))
               (symbol? pat) (conj (conj acc pat) init)
               (vector? pat)
                 (let* [g (symbol (str (gensym)))
                        n (count pat)
                        has-amp (reduce (fn* [st x] (if st st (amp? x))) false pat)
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
                              a))
                        ;; a pattern with & walks (seq v) via first/next like the
                        ;; JVM, so a map (or any non-indexed seqable) destructures
                        ;; positionally through its seq — nth on it would not.
                        gs (symbol (str (gensym)))
                        sloop
                          (fn* sloop [i a]
                            (if (< i n)
                              (let* [elem (nth pat i)]
                                (cond
                                  (amp? elem)
                                    (sloop (+ i 2) (proc (nth pat (inc i)) gs a))
                                  (= elem :as)
                                    (sloop (+ i 2) (proc (nth pat (inc i)) g a))
                                  :else
                                    (let* [gf (symbol (str (gensym)))
                                           a2 (conj (conj a gf) `(first ~gs))
                                           a3 (conj (conj a2 gs) `(next ~gs))]
                                      (sloop (inc i) (proc elem gf a3)))))
                              a))]
                   (if has-amp
                     (sloop 0 (conj (conj (conj (conj acc g) init) gs) `(seq ~g)))
                     (vloop 0 0 (conj (conj acc g) init))))
               (map? pat)
                 (let* [g       (symbol (str (gensym)))
                        gm      (symbol (str (gensym)))
                        gignore (symbol (str (gensym)))
                        defaults    (get pat :or)
                        defaults-as (get pat :defaults)
                        ;; :defaults binds a map of the resolved :or defaults, so it
                        ;; requires :or (CLJ-2966).
                        _ (if (and defaults-as (not defaults))
                            (throw (new IllegalArgumentException "Can't specify :defaults without :or")))
                        select (get pat :select)
                        as-sym (get pat :as)
                        or-keys (if defaults (keys defaults) [])
                        ;; kwargs: a map pattern may bind against the sequential rest of
                        ;; a fn — (& {:keys [...]}) — a seq of alternating k/v args,
                        ;; optionally with a trailing map (Clojure 1.11: (f :a 1 {:b 2})
                        ;; merges the map over the pairs; (f {:a 1}) is just the map). An
                        ;; odd count means the last arg is that trailing map. A real map
                        ;; is used as-is. g holds init once; gm is the coerced map every
                        ;; lookup (and :as) reads from.
                        coerce `(if (sequential? ~g)
                                  (if (odd? (count ~g))
                                    (merge (apply hash-map (butlast ~g)) (last ~g))
                                    (apply hash-map ~g))
                                  ~g)
                        acc-m (conj (conj (conj (conj acc g) init) gm) coerce)
                        base  (if as-sym (conj (conj acc-m as-sym) gm) acc-m)
                        has-def? (fn* [k] (and defaults (contains? defaults k)))
                        ;; init form reading key bk for binding form bb; a checked (!)
                        ;; directive uses req! (throw on missing). An :or default applies
                        ;; when the local name (binding-style :or) or the key itself
                        ;; (key-style :or) has one; a default for a required key errors.
                        getter
                          (fn* [bb bk req?]
                            (let* [ident (or (symbol? bb) (keyword? bb))
                                   local (if ident (symbol (name bb)) bb)
                                   ld? (and ident (has-def? local))
                                   kd? (has-def? bk)]
                              (cond
                                (and ld? kd?)
                                  (throw (new Exception (str "Multiple :or defaults for same key: " (pr-str bk))))
                                (or ld? kd?)
                                  (if req?
                                    (throw (new Exception (str "Can't supply default value for required key: " (pr-str bk))))
                                    `(get ~gm ~bk ~(get defaults (if ld? local bk))))
                                req? `(req! ~gm ~bk)
                                :else `(get ~gm ~bk))))
                        ;; bind bb (a plain ident, a throwaway gignore, or a nested
                        ;; pattern) to key bk. :or default expressions are inlined
                        ;; as get's not-found arg (eager, JVM-exact).
                        push1
                          (fn* [a bb bk req?]
                            (let* [bv (getter bb bk req?)]
                              (if (or (symbol? bb) (keyword? bb))
                                ;; carry a type hint / metadata onto the local, like
                                ;; the reference's localize (drops only the namespace).
                                (conj (conj a (with-meta (symbol (name bb)) (meta bb))) bv)
                                (proc bb bv a))))
                        ;; a directive name -> its lookup key. dnsp (a qualified :ns/keys)
                        ;; takes precedence over a bare name's own namespace.
                        mk-key
                          (fn* [kind dnsp s]
                            (let* [nsp (or dnsp (namespace s)) nm (name s)]
                              (cond
                                (= kind :kw)  (keyword (if nsp (str nsp "/" nm) nm))
                                (= kind :sym) `(quote ~(symbol nsp nm))
                                :else nm)))
                        ;; walk one directive's name list. Names before & bind and are
                        ;; transformed to keys; names after & are the keys themselves
                        ;; (a bare symbol there is an error — quote it for :syms), used
                        ;; to declare :select membership and, for a checked directive, to
                        ;; enforce the key. st is [acc sel b->k].
                        do-dir
                          (fn* do-dir [st names kind dnsp req? preamp?]
                            (if (seq names)
                              (let* [bb (first names)]
                                (if (amp? bb)
                                  (if preamp?
                                    (do-dir st (next names) kind dnsp req? false)
                                    (throw (new IllegalArgumentException "& can only appear once in a directive")))
                                  (let* [_ (if (and (not preamp?) (symbol? bb))
                                             (throw (new IllegalArgumentException
                                                      (str "'" bb "' - binding symbols can only appear before '&', use keys after"))))
                                         a    (nth st 0)
                                         sel  (nth st 1)
                                         b->k (nth st 2)
                                         bk   (if preamp? (mk-key kind dnsp bb) bb)
                                         a2   (if (or preamp? req?)
                                                (push1 a (if preamp? bb gignore) bk req?)
                                                a)
                                         b->k2 (if preamp? (assoc b->k (symbol (name bb)) bk) b->k)]
                                    (do-dir [a2 (conj sel bk) b->k2] (next names) kind dnsp req? preamp?))))
                              st))
                        ;; a keyword key is a directive: split :keys/:syms/:strs from its
                        ;; ! (checked) suffix. Returns [kind req?].
                        dir-kind
                          (fn* [dir]
                            (let* [nm (name dir)
                                   n  (count nm)
                                   req? (and (> n 0) (= "!" (subs nm (dec n) n)))
                                   b  (if req? (subs nm 0 (dec n)) nm)]
                              (cond
                                (= b "keys") [:kw req?]
                                (= b "syms") [:sym req?]
                                (= b "strs") [:str req?]
                                :else (throw (new Exception (str "Unsupported map directive: " dir))))))
                        ;; process one entry key. st is [acc sel b->k subm], where subm
                        ;; maps a nested-map key to its :select var (for deep :select).
                        do-entry
                          (fn* [st k]
                            (let* [a    (nth st 0)
                                   sel  (nth st 1)
                                   b->k (nth st 2)
                                   subm (nth st 3)]
                              (if (keyword? k)
                                (let* [dk   (dir-kind k)
                                       kind (nth dk 0)
                                       req? (nth dk 1)
                                       r    (do-dir [a sel b->k] (get pat k) kind (namespace k) req? true)]
                                  [(nth r 0) (nth r 1) (nth r 2) subm])
                                ;; a direct binding {bb key}. Under an active :select, a
                                ;; nested map subselects: give it a :select if it lacks
                                ;; one, and thread its selected submap up through subm.
                                (let* [bk      (get pat k)
                                       subsel? (and select (map? k))
                                       k2      (if (or (not subsel?) (get k :select))
                                                 k
                                                 (assoc k :select (symbol (str (gensym)))))
                                       subm2   (if subsel? (assoc subm bk (get k2 :select)) subm)
                                       b->k2   (if (symbol? k2) (assoc b->k k2 bk) b->k)]
                                  [(push1 a k2 bk false) (conj sel bk) b->k2 subm2]))))
                        entry-keys (reduce (fn* [ks k]
                                             (if (or (= k :or) (= k :as) (= k :select) (= k :defaults))
                                               ks (conj ks k)))
                                           [] (keys pat))
                        st1  (reduce do-entry [base [] {} {}] entry-keys)
                        acc-e (nth st1 0)
                        sel   (nth st1 1)
                        b->k  (nth st1 2)
                        subm  (nth st1 3)
                        ;; resolve each :or key to the actual map key: a binding symbol
                        ;; maps through b->k; a key-style :or entry is the key itself.
                        dm-pairs (reduce (fn* [ps k]
                                            (let* [rk (if (symbol? k) (get b->k k) k)
                                                   ls (if (symbol? k) k (symbol (name k)))]
                                              (if (nil? rk) ps (conj (conj ps rk) ls))))
                                          [] or-keys)
                        subm-pairs (reduce (fn* [ps k] (conj (conj ps k) (get subm k))) [] (keys subm))
                        mmg (symbol (str (gensym)))
                        ;; :select binds a map of the selected keys — the source map with
                        ;; missing keys filled from the defaults and nested submaps
                        ;; replaced by their own selections (CLJ-2964).
                        acc-sel (if select
                                  (conj (conj acc-e select)
                                        `(let* [~mmg (merge (some-vals (select-keys (hash-map ~@dm-pairs)
                                                                                    (hash-set ~@sel)))
                                                            ~gm
                                                            (some-vals (hash-map ~@subm-pairs)))]
                                           (if ~mmg (select-keys ~mmg (hash-set ~@sel)) nil)))
                                  acc-e)]
                   (if defaults-as
                     (conj (conj acc-sel defaults-as) `(hash-map ~@dm-pairs))
                     acc-sel))
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
;; symbols, lists, and composite literals (vectors/maps/sets) are quoted so their
;; elements aren't resolved as code; a LIST in test position is an or-group of
;; constants, while a vector/map/set is a single literal constant.
(defmacro case [expr & clauses]
  (let [g (fresh-sym)
        mk-const (fn [c] (if (or (symbol? c) (seq? c) (vector? c) (map? c) (set? c)) `(quote ~c) c))
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
                  ;; no clause matched and no default — Clojure throws
                  ;; IllegalArgumentException here (catchable by class).
                  `(throw (new IllegalArgumentException (str "No matching clause: " ~g)))
                  (if (empty? (rest cls))
                    (first cls)
                    `(if ~(mk-test (first cls)) ~(nth cls 1) ~(build (drop 2 cls))))))]
    (if dup
      (throw (str "Duplicate case test constant: " (first dup)))
      `(let* [~g ~expr] ~(build clauses)))))

;; for/doseq share these. for-parse-groups turns a binding vector into groups
;; [bind coll mods], mods a vector of [kw form] in SOURCE ORDER.
(defn- for-scan [bvec i bind coll mods]
  (if (and (< i (count bvec)) (keyword? (nth bvec i)))
    (let [k (nth bvec i) v (nth bvec (inc i))]
      (cond
        (= k :when)  (for-scan bvec (+ i 2) bind coll (conj mods [:when v]))
        (= k :let)   (for-scan bvec (+ i 2) bind coll (conj mods [:let v]))
        (= k :while) (for-scan bvec (+ i 2) bind coll (conj mods [:while v]))
        :else        (for-scan bvec (inc i) bind coll mods)))
    [i bind coll mods]))
(defn- for-parse-groups [bvec i groups]
  (if (>= i (count bvec))
    groups
    (let [r (for-scan bvec (+ i 2) (nth bvec i) (nth bvec (inc i)) [])]
      (for-parse-groups bvec (nth r 0)
                        (conj groups [(nth r 1) (nth r 2) (nth r 3)])))))
;; thread the modifier chain for ONE element in SOURCE ORDER, matching the JVM:
;; :let binds for the rest of the chain, :when skips this element and continues,
;; :while stops the whole coll. proceed/skip/stop are the forms for those outcomes.
(defn- comprehension-chain [mods proceed skip stop]
  (if (empty? mods)
    proceed
    (let [m (first mods) k (first m) v (nth m 1) r (rest mods)]
      (cond
        (= k :let)   `(let ~v ~(comprehension-chain r proceed skip stop))
        (= k :when)  `(if ~v ~(comprehension-chain r proceed skip stop) ~skip)
        (= k :while) `(if ~v ~(comprehension-chain r proceed skip stop) ~stop)
        :else        (comprehension-chain r proceed skip stop)))))

;; for: lazy list comprehension. A group with no modifiers is a plain map (last)
;; or mapcat (nested); a group with :let/:when/:while uses a lazy walk that applies
;; them in source order — a :when skips one element (looping, so long skip runs
;; don't grow the stack), a :while ends the seq.
(defmacro for [bindings body]
  (let [build (fn build [idx groups]
                (let [g (nth groups idx)
                      my-bind (nth g 0)
                      my-coll (nth g 1)
                      my-mods (nth g 2)
                      is-last (= idx (dec (count groups)))
                      k-form (if is-last `(list ~body) (build (inc idx) groups))]
                  (if (empty? my-mods)
                    (if is-last
                      `(map (fn [~my-bind] ~body) ~my-coll)
                      `(mapcat (fn [~my-bind] ~k-form) ~my-coll))
                    (let [stepf (fresh-sym) colls (fresh-sym) sv (fresh-sym)]
                      `((fn ~stepf [~colls]
                          (lazy-seq
                            (loop [~sv (seq ~colls)]
                              (when ~sv
                                (let [~my-bind (first ~sv)]
                                  ~(comprehension-chain my-mods
                                          `(concat ~k-form (~stepf (rest ~sv)))
                                          `(recur (next ~sv))
                                          nil))))))
                        ~my-coll)))))]
    (if (>= (count bindings) 2)
      (build 0 (for-parse-groups bindings 0 []))
      body)))

;; doseq runs body for side effects across the bindings in constant space,
;; returning nil. A direct nested loop/recur per group (not (count (for …)),
;; which allocated a lazy cell per iteration and held the seq head): :let binds,
;; :when skips one element, :while stops the coll — same source-order semantics
;; as for.
(defmacro doseq [bindings & body]
  (let [build (fn build [idx groups]
                (let [g (nth groups idx)
                      my-bind (nth g 0)
                      my-coll (nth g 1)
                      my-mods (nth g 2)
                      is-last (= idx (dec (count groups)))
                      k-form (if is-last `(do ~@body nil) (build (inc idx) groups))
                      sv (fresh-sym)]
                  `(loop [~sv (seq ~my-coll)]
                     (when ~sv
                       (let [~my-bind (first ~sv)]
                         ~(comprehension-chain my-mods
                                 `(do ~k-form (recur (next ~sv)))
                                 `(recur (next ~sv))
                                 nil))))))]
    (if (>= (count bindings) 2)
      `(do ~(build 0 (for-parse-groups bindings 0 [])) nil)
      `(do ~@body nil))))

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

;; unquote/unquote-splicing: the reader lowers ~x to (clojure.core/unquote x)
;; and ~@x to (clojure.core/unquote-splicing x). Declare them so (resolve
;; 'unquote) is non-nil, as on the JVM (they are otherwise unbound).
(declare unquote unquote-splicing)
