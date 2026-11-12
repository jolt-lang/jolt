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
      (throw (ClassCastException. (str "zero? requires a number, got: " x))))))

;; pos? checks number? explicitly: this tier is recompiled by the staged pass,
;; where a bare (> x 0) emits the native op that happily orders strings
;; (the documented native-ops relaxation) — the guard keeps Clojure's throw.
(def pos?
  (fn* pos? [x]
    (if (number? x)
      (> x 0)
      (throw (ClassCastException. (str "pos? requires a number, got: " x))))))

;; Canonical every?: short-circuits on the first falsey result, so infinite
;; seqs with an early counterexample terminate. Expressed over reduce+reduced so a
;; chunked source (range, vector) drives reduce's tight index loop instead of a
;; per-element seq/first/next walk; reduce steps a lazy seq one cell at a time and
;; `reduced` stops it, preserving laziness and short-circuit. (fn* only here — this
;; is kernel tier, above the fn/when/cond defmacros.)
(def every?
  (fn* every? [pred coll]
    (reduce (fn* [_ x] (if (pred x) true (reduced false))) true coll)))

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

;; ns's helpers: fn*/let* like the rest of this file above the let/fn macros.
;; pr-str is not declared until above destructure (the declare macro is defined
;; there); like ns's own body, these resolve it when they run, after 20-coll.
(def ^:private ns-bad-clause
  (fn* ns-bad-clause [nm clause why]
    (throw (ex-info
             (str "ns " nm ": " why ", got: " (pr-str clause)
                  ". After the name come an optional docstring, an optional"
                  " attr-map and reference clauses such as (:require ...);"
                  " an unescaped \" in a docstring splits it this way")
             {:jolt.error/kind :analyze/invalid-ns-form
              :jolt.error/type :analysis-error}))))

;; one reference clause appended to acc as its namespace-op call, each spec
;; quoted. A reference clause may be a list (:require …) or a vector
;; [:require …]; Clojure accepts both, dispatching on (first clause).
(def ^:private ns-clause-calls
  (fn* ns-clause-calls [nm acc clause]
    (if (or (seq? clause) (vector? clause))
      (let* [head (first clause)
             args (map (fn* [s] `(quote ~s)) (rest clause))]
        (cond
          (= head :require) (conj acc `(require ~@args))
          (= head :use)     (conj acc `(use ~@args))
          (= head :import)  (conj acc `(import ~@args))
          (= head :refer-clojure) (conj acc `(refer-clojure ~@args))
          (= head :refer)   (conj acc `(refer ~@args))
          (= head :load)    (conj acc `(load ~@args))
          (= head :gen-class) acc
          :else (ns-bad-clause nm clause "unknown reference clause")))
      (ns-bad-clause nm clause "not a reference clause"))))

;; the namespace metadata: the name's own, then the docstring, then the attr-map.
(def ^:private ns-metadata
  (fn* ns-metadata [name-meta doc attrs]
    (let* [m (if doc (assoc (if name-meta name-meta {}) :doc doc) name-meta)]
      (if attrs (conj (if m m {}) attrs) m))))


;; ns is sugar over the namespace-op fns (in-ns/require/use/import/refer-clojure,
;; all ctx-capturing clojure.core fns) — matching Clojure, where require is a fn and
;; the ns macro expands its clauses into require calls. Each spec is quoted
;; individually and passed as data. So ns compiles to a plain (do …) of invokes.
;; MUST live in this first tier: the self-hosted analyzer build (triggered while
;; 10-seq loads) processes jolt.analyzer's own (ns …) form, so ns has to exist by
;; then. Its body resolves fn/map/reduce/cond at EXPANSION time, by which point all
;; of 00-syntax has loaded, so using them here is fine.
;;
;; After the name comes an optional docstring, an optional attr-map, and reference
;; clauses, and nothing else. An unescaped quote in a docstring splits it into
;; strings and bare symbols; those are refused here, as the JVM refuses them.
;;
;; A build never runs an ns form. ei-ns-calls (emit-image.ss) lifts the
;; reset-meta! and load calls out of this expansion, so these are the one place
;; they are made, whichever way the namespace loads.
(defmacro ns [nm & clauses]
  ;; ^{:map} metadata on the ns name reads as a (with-meta sym {...}) form, not an
  ;; annotated symbol. Real libraries put :author/:doc there
  ;; (clojure.tools.logging). It is evaluated, in the new namespace and before
  ;; clojure.core is referred into it, as the JVM's .resetMeta call is.
  (let [meta-form? (and (seq? nm) (= 'with-meta (first nm)))
        name-meta (if meta-form? (nth nm 2) (meta nm))
        nm (if meta-form? (second nm) nm)
        _ (if (symbol? nm)
            nil
            (throw (ex-info (str "ns: the namespace name must be a symbol, got: " (pr-str nm))
                            {:jolt.error/kind :analyze/invalid-ns-form
                             :jolt.error/type :analysis-error})))
        doc (if (string? (first clauses)) (first clauses) nil)
        clauses (if doc (rest clauses) clauses)
        attrs (if (map? (first clauses)) (first clauses) nil)
        clauses (if attrs (rest clauses) clauses)
        ns-meta (ns-metadata name-meta doc attrs)
        calls (reduce (fn [acc clause] (ns-clause-calls nm acc clause)) [] clauses)
        ;; no :refer-clojure clause means the default (refer-clojure), as on the
        ;; JVM — it is what makes a later standalone (refer-clojure :exclude …)
        ;; withhold nothing, the names being mapped already (ns.ss).
        ;; (reduce/or, not some/into: this expands before 10-seq has loaded)
        explicit? (reduce (fn [a c] (or a (= :refer-clojure (first c))))
                          false clauses)
        default (if (or explicit? (= nm 'clojure.core)) () (list `(refer-clojure)))
        set-meta (if ns-meta
                   (list `(reset-meta! (create-ns (quote ~nm)) ~ns-meta))
                   ())]
    `(do (in-ns (quote ~nm)) ~@set-meta ~@default ~@calls)))

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
;; symbol becomes (form x). Iterative, so ONE expansion step yields the whole
;; chain: a consumer that reads a single macroexpand-1 (tools.analyzer, a linter)
;; gets the threaded shape, where a recursive definition would hand back another
;; (-> ...) and only agree after a full macroexpand. A threaded step keeps its own
;; form's metadata, so a fault inside the chain reports that step's line and not
;; the whole chain's; a bare symbol has no form of its own and inherits the call's.
;; loop*/recur, not loop: the loop macro is defined further down this file.
(defmacro -> [x & forms]
  (loop* [x x
          forms (seq forms)]
    (if forms
      (let* [form (first forms)
             threaded (if (seq? form)
                        (with-meta `(~(first form) ~x ~@(rest form)) (meta form))
                        `(~form ~x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->> [x & forms]
  (loop* [x x
          forms (seq forms)]
    (if forms
      (let* [form (first forms)
             threaded (if (seq? form)
                        (with-meta `(~(first form) ~@(rest form) ~x) (meta form))
                        `(~form ~x))]
        (recur threaded (next forms)))
      x)))

;; Forward declaration interns unbound vars (Clojure semantics). The interpreter
;; resolves forward refs lazily either way, but the COMPILER classifies globals at
;; compile time: without the var, a declared name that collides with a host root
;; binding (parse, hash, …) would compile to the host fn instead of the var.
(defmacro declare [& syms]
  `(do ~@(map (fn* [s] `(def ~(with-meta s (assoc (meta s) :declared true)))) syms)))

;; letfn is a macro over the letfn* special form, matching Clojure: each
;; (name [params] body*) spec becomes a name + a (fn name [params] body*) binding.
;; So (macroexpand-1 '(letfn …)) yields the letfn* form macroexpansion tooling
;; (tools.macro / tools.analyzer) expects, instead of an opaque special form.
(defmacro letfn [fnspecs & body]
  (cons 'letfn*
        (cons (reduce (fn [acc s] (conj (conj acc (first s)) (cons 'clojure.core/fn s))) [] fnspecs)
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
;; pr-str (20-coll) builds its error messages.
;; The destructure-* helpers below are its pieces, one per pattern shape; each
;; appends bindings to the vector acc and returns it.
(declare pr-str)
(declare ^:private destructure-binding)

(def ^:private destructure-amp?
  (fn* destructure-amp? [x] (and (symbol? x) (= "&" (name x)))))

;; a vector pattern without &: element idx binds (nth g idx nil); :as binds g.
(def ^:private destructure-indexed
  (fn* destructure-indexed [pat g i idx a]
    (if (< i (count pat))
      (let* [elem (nth pat i)]
        (if (= elem :as)
          (destructure-indexed pat g (+ i 2) idx (destructure-binding (nth pat (inc i)) g a))
          (destructure-indexed pat g (inc i) (inc idx)
                               (destructure-binding elem `(nth ~g ~idx nil) a))))
      a)))

;; a pattern with & walks (seq v) via first/next like the
;; JVM, so a map (or any non-indexed seqable) destructures
;; positionally through its seq — nth on it would not.
;; gs holds the seq still to walk; & binds it whole.
(def ^:private destructure-seq-walk
  (fn* destructure-seq-walk [pat g gs i a]
    (if (< i (count pat))
      (let* [elem (nth pat i)]
        (cond
          (destructure-amp? elem)
            (destructure-seq-walk pat g gs (+ i 2) (destructure-binding (nth pat (inc i)) gs a))
          (= elem :as)
            (destructure-seq-walk pat g gs (+ i 2) (destructure-binding (nth pat (inc i)) g a))
          :else
            (let* [gf (symbol (str (gensym)))
                   a2 (conj (conj a gf) `(first ~gs))
                   a3 (conj (conj a2 gs) `(next ~gs))]
              (destructure-seq-walk pat g gs (inc i) (destructure-binding elem gf a3)))))
      a)))

(def ^:private destructure-vector
  (fn* destructure-vector [pat init acc]
    (let* [g (symbol (str (gensym)))
           has-amp (reduce (fn* [st x] (if st st (destructure-amp? x))) false pat)
           gs (symbol (str (gensym)))]
      (if has-amp
        (destructure-seq-walk pat g gs 0 (conj (conj (conj (conj acc g) init) gs) `(seq ~g)))
        (destructure-indexed pat g 0 0 (conj (conj acc g) init))))))

;; kwargs: a map pattern may bind against the seq rest of a fn
;; — (& {:keys [...]}) — a seq of alternating k/v args, optionally
;; with a trailing map (Clojure 1.11: (f :a 1 {:b 2}) merges the
;; map over the pairs). An odd count means the last arg is that
;; trailing map — except for a LONE element, which is the whole
;; value: (f {:a 1}) passes the map itself, and the JVM takes that
;; element as the map whatever it is, so a stray (f 1 :a) looks
;; keys up in a keyword and answers nil instead of throwing
;; (#909). Only a seq is coerced, like destmap*'s (seq? gmap): a
;; vector init destructures as itself, never as pairs. g holds
;; init once; gm is the coerced map every lookup (and :as) reads
;; from.
(def ^:private destructure-coerce-map
  (fn* destructure-coerce-map [g]
    `(if (seq? ~g)
       (if (next ~g)
         (if (odd? (count ~g))
           (merge (apply hash-map (butlast ~g)) (last ~g))
           (apply hash-map ~g))
         (if (seq ~g) (first ~g) (hash-map)))
       ~g)))

;; A map pattern's helpers share m, its context: {:pat :gm :gignore :defaults
;; :select}. gm is the coerced map, gignore the throwaway local for a checked
;; key that binds nothing, defaults the :or map.
(def ^:private destructure-has-default?
  (fn* destructure-has-default? [defaults k] (and defaults (contains? defaults k))))

;; init form reading key bk for binding form bb; a checked (!)
;; directive uses req! (throw on missing). An :or default applies
;; when the local name (binding-style :or) or the key itself
;; (key-style :or) has one; a default for a required key errors.
(def ^:private destructure-map-lookup
  (fn* destructure-map-lookup [m bb bk req?]
    (let* [gm (get m :gm)
           defaults (get m :defaults)
           ident (or (symbol? bb) (keyword? bb))
           local (if ident (symbol (name bb)) bb)
           ld? (and ident (destructure-has-default? defaults local))
           kd? (destructure-has-default? defaults bk)]
      (cond
        (and ld? kd?)
          (throw (new Exception (str "Multiple :or defaults for same key: " (pr-str bk))))
        (or ld? kd?)
          (if req?
            (throw (new Exception (str "Can't supply default value for required key: " (pr-str bk))))
            `(get ~gm ~bk ~(get defaults (if ld? local bk))))
        req? `(req! ~gm ~bk)
        :else `(get ~gm ~bk)))))

;; bind bb (a plain ident, a throwaway gignore, or a nested
;; pattern) to key bk. :or default expressions are inlined
;; as get's not-found arg (eager, JVM-exact).
(def ^:private destructure-map-bind
  (fn* destructure-map-bind [m a bb bk req?]
    (let* [bv (destructure-map-lookup m bb bk req?)]
      (if (or (symbol? bb) (keyword? bb))
        ;; carry a type hint / metadata onto the local, like
        ;; the reference's localize (drops only the namespace).
        (conj (conj a (with-meta (symbol (name bb)) (meta bb))) bv)
        (destructure-binding bb bv a)))))

;; a directive name -> its lookup key. dnsp (a qualified :ns/keys)
;; takes precedence over a bare name's own namespace.
(def ^:private destructure-directive-key
  (fn* destructure-directive-key [kind dnsp s]
    (let* [nsp (or dnsp (namespace s)) nm (name s)]
      (cond
        (= kind :kw)  (keyword (if nsp (str nsp "/" nm) nm))
        (= kind :sym) `(quote ~(symbol nsp nm))
        :else nm))))

;; one name bb of a directive, before or after its &. st is [acc sel b->k].
(def ^:private destructure-directive-name
  (fn* destructure-directive-name [m st bb kind dnsp req? preamp?]
    (let* [_ (if (and (not preamp?) (symbol? bb))
               (throw (new IllegalArgumentException
                        (str "'" bb "' - binding symbols can only appear before '&', use keys after"))))
           a    (nth st 0)
           sel  (nth st 1)
           b->k (nth st 2)
           bk   (if preamp? (destructure-directive-key kind dnsp bb) bb)
           a2   (if (or preamp? req?)
                  (destructure-map-bind m a (if preamp? bb (get m :gignore)) bk req?)
                  a)
           b->k2 (if preamp? (assoc b->k (symbol (name bb)) bk) b->k)]
      [a2 (conj sel bk) b->k2])))

;; walk one directive's name list. Names before & bind and are
;; transformed to keys; names after & are the keys themselves
;; (a bare symbol there is an error — quote it for :syms), used
;; to declare :select membership and, for a checked directive, to
;; enforce the key. st is [acc sel b->k].
(def ^:private destructure-directive
  (fn* destructure-directive [m st names kind dnsp req? preamp?]
    (if (seq names)
      (let* [bb (first names)]
        (if (destructure-amp? bb)
          (if preamp?
            (destructure-directive m st (next names) kind dnsp req? false)
            (throw (new IllegalArgumentException "& can only appear once in a directive")))
          (destructure-directive m (destructure-directive-name m st bb kind dnsp req? preamp?)
                                 (next names) kind dnsp req? preamp?)))
      st)))

;; a keyword key is a directive: split :keys/:syms/:strs from its
;; ! (checked) suffix. Returns [kind req?].
(def ^:private destructure-directive-kind
  (fn* destructure-directive-kind [dir]
    (let* [nm (name dir)
           n  (count nm)
           req? (and (> n 0) (= "!" (subs nm (dec n) n)))
           b  (if req? (subs nm 0 (dec n)) nm)]
      (cond
        (= b "keys") [:kw req?]
        (= b "syms") [:sym req?]
        (= b "strs") [:str req?]
        :else (throw (new Exception (str "Unsupported map directive: " dir)))))))

;; a direct binding {bb key}. Under an active :select, a
;; nested map subselects: give it a :select if it lacks
;; one, and thread its selected submap up through subm.
(def ^:private destructure-direct-entry
  (fn* destructure-direct-entry [m st k]
    (let* [subm    (nth st 3)
           bk      (get (get m :pat) k)
           subsel? (and (get m :select) (map? k))
           k2      (if (or (not subsel?) (get k :select))
                     k
                     (assoc k :select (symbol (str (gensym)))))
           subm2   (if subsel? (assoc subm bk (get k2 :select)) subm)
           b->k2   (if (symbol? k2) (assoc (nth st 2) k2 bk) (nth st 2))]
      [(destructure-map-bind m (nth st 0) k2 bk false) (conj (nth st 1) bk) b->k2 subm2])))

;; process one entry key. st is [acc sel b->k subm], where subm
;; maps a nested-map key to its :select var (for deep :select).
(def ^:private destructure-map-entry
  (fn* destructure-map-entry [m st k]
    (if (keyword? k)
      (let* [dk (destructure-directive-kind k)
             r  (destructure-directive m [(nth st 0) (nth st 1) (nth st 2)] (get (get m :pat) k)
                                       (nth dk 0) (namespace k) (nth dk 1) true)]
        [(nth r 0) (nth r 1) (nth r 2) (nth st 3)])
      (destructure-direct-entry m st k))))

;; the pattern's binding entries: every key but the :or/:as/:select/:defaults options
(def ^:private destructure-entry-keys
  (fn* destructure-entry-keys [pat]
    (reduce (fn* [ks k]
              (if (or (= k :or) (= k :as) (= k :select) (= k :defaults))
                ks (conj ks k)))
            [] (keys pat))))

;; resolve each :or key to the actual map key: a binding symbol
;; maps through b->k; a key-style :or entry is the key itself.
(def ^:private destructure-default-pairs
  (fn* destructure-default-pairs [or-keys b->k]
    (reduce (fn* [ps k]
              (let* [rk (if (symbol? k) (get b->k k) k)
                     ls (if (symbol? k) k (symbol (name k)))]
                (if (nil? rk) ps (conj (conj ps rk) ls))))
            [] or-keys)))

;; :select binds a map of the selected keys — the source map with
;; missing keys filled from the defaults and nested submaps
;; replaced by their own selections (CLJ-2964).
(def ^:private destructure-selection
  (fn* destructure-selection [gm mmg dm-pairs sel subm]
    (let* [subm-pairs (reduce (fn* [ps k] (conj (conj ps k) (get subm k))) [] (keys subm))]
      `(let* [~mmg (merge (some-vals (select-keys (hash-map ~@dm-pairs)
                                                  (hash-set ~@sel)))
                          ~gm
                          (some-vals (hash-map ~@subm-pairs)))]
         (if ~mmg (select-keys ~mmg (hash-set ~@sel)) nil)))))

(def ^:private destructure-map
  (fn* destructure-map [pat init acc]
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
           acc-m (conj (conj (conj (conj acc g) init) gm) (destructure-coerce-map g))
           base  (if as-sym (conj (conj acc-m as-sym) gm) acc-m)
           m {:pat pat :gm gm :gignore gignore :defaults defaults :select select}
           st1 (reduce (fn* [st k] (destructure-map-entry m st k))
                       [base [] {} {}] (destructure-entry-keys pat))
           acc-e (nth st1 0)
           dm-pairs (destructure-default-pairs (if defaults (keys defaults) []) (nth st1 2))
           mmg (symbol (str (gensym)))
           acc-sel (if select
                     (conj (conj acc-e select)
                           (destructure-selection gm mmg dm-pairs (nth st1 1) (nth st1 3)))
                     acc-e)]
      (if defaults-as
        (conj (conj acc-sel defaults-as) `(hash-map ~@dm-pairs))
        acc-sel))))

(def ^:private destructure-binding
  (fn* destructure-binding [pat init acc]
    (cond
      ;; CLJ-2954: & is reserved for destructuring rest, never a binding.
      (destructure-amp? pat) (throw (new IllegalArgumentException "Can't use & as a local binding"))
      (symbol? pat) (conj (conj acc pat) init)
      (vector? pat) (destructure-vector pat init acc)
      (map? pat) (destructure-map pat init acc)
      :else (throw (IllegalArgumentException. (str "unsupported destructuring pattern: " (pr-str pat)))))))

(def destructure
 (fn* destructure [bindings]
  (let* [ploop
           (fn* ploop [i acc]
             (if (< i (count bindings))
               (ploop (+ i 2) (destructure-binding (nth bindings i) (nth bindings (inc i)) acc))
               acc))]
    ;; Checked before the walk, which steps in twos and reads (nth bindings
    ;; (inc i)): an odd vector ran off the end and the user got the raw fault
    ;; back — "java.lang.IndexOutOfBoundsException: index out of bounds" as the
    ;; compile error for (let [a 1 b] a). analyze-bindings makes the same check
    ;; for let*, but destructuring runs FIRST, so every `let`, `loop`, `for`,
    ;; `doseq` and `if-let` reached the fault before the analyzer ever saw the
    ;; form. Same wording as the special form, which is the reference's.
    (if (odd? (count bindings))
      (throw (ex-info "Bad binding form, expected matched symbol expression pairs"
                      {:jolt.error/kind :analyze/invalid-binding
                       :jolt.error/type :analysis-error}))
      (ploop 0 [])))))

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
;; optional name and single- or multi-arity. Its helpers are fn* (not fn) to avoid
;; a cycle. fn-params walks a param seq, replacing non-symbol patterns with gensyms
;; and recording [pattern gensym] let-bindings; fn-arity turns one arity
;; (params . body) into a rewritten arity. Output: single arity splices the arity's
;; elements straight into fn*; multi arity splices the rewritten clauses.

;; a return-type hint (defn f ^bytes [x] ...) reaches us as a
;; (with-meta [x] {:tag ...}) FORM in params position — unwrap it
;; (the hint means nothing on jolt; ring-codec carries several).
(def ^:private fn-unhint
  (fn* fn-unhint [x]
    (if (if (seq? x) (= 'with-meta (first x)) false)
      (nth x 1)
      x)))

;; a :pre/:post conditions map (a leading map when the body has more forms
;; after it) becomes assertions: pre before the body, then — ONLY when
;; there are :post conditions — bind % to the result, assert them, return
;; %. (map? is a native, so this is tier-safe; the assert/map calls only
;; run when a conditions map is actually present.)
;;
;; The % binding is what :post needs and what a :pre-only fn must not pay:
;; wrapping the body in (let [% (do body)] %) puts the body in a binding
;; INIT, which is not tail position, so a tail `recur` inside a :pre-only
;; fn was rejected — malli's validate-times is one, and eight of its test
;; namespaces stopped loading. The reference emits the wrapper only for
;; :post, and this now matches it.
;;
;; Tail CALLS were never affected, measured rather than assumed: 3e6 deep
;; self-calls through the var run at baseline RSS with the wrapper and
;; without it, because the back end collapses (let [% X] %) to X. Only
;; `recur`, which the analyzer checks on the pre-collapse tree, saw it.
(def ^:private fn-wrap-conditions
  (fn* fn-wrap-conditions [body]
    (if (if (map? (first body)) (next body) false)
      (let [conds (first body)
            real (next body)
            mka (fn* [cs] (map (fn* [c] `(assert ~c)) cs))
            posts (get conds :post)]
        (if (seq posts)
          `(~@(mka (get conds :pre))
            (let [~'% (do ~@real)]
              ~@(mka posts)
              ~'%))
          `(~@(mka (get conds :pre)) ~@real)))
      body)))

(def ^:private fn-params
  (fn* fn-params [ps nps lets]
    (if (seq ps)
      (if (symbol? (first ps))
        (fn-params (next ps) (conj nps (first ps)) lets)
        ;; a bare (gensym) returns a host symbol the destructurer rejects;
        ;; round-trip through str for a jolt symbol.
        (let [g (symbol (str (gensym)))]
          (fn-params (next ps) (conj nps g) (conj (conj lets (first ps)) g))))
      [nps lets])))

(def ^:private fn-arity
  (fn* fn-arity [sig]
    (let [ps (fn-unhint (first sig))
          hinted (not (= ps (first sig)))
          r (fn-params (seq ps) [] [])
          raw-body (rest sig)
          body (fn-wrap-conditions raw-body)
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
            `(~pv (let ~lv ~@body))))))))

(defmacro fn [& raw]
  (let [nm (if (symbol? (first raw)) (first raw) nil)
        aftn (if nm (next raw) raw)]
    (if (vector? (fn-unhint (first aftn)))
      (let [a (fn-arity aftn)]
        (if nm `(fn* ~nm ~@a) `(fn* ~@a)))
      (let [as (vec (map fn-arity aftn))]
        (if nm `(fn* ~nm ~@as) `(fn* ~@as))))))

;; defn's helpers. defn lives in the earliest tier, so they (like its macro body)
;; may only use primitives available before the seq/coll tiers — conj (which
;; merges a map onto a map), assoc, meta, with-meta — not merge/last/butlast.

;; A TRAILING attr-map after the arity clauses — (defn f ([x] …) ([x y] …)
;; {:k v}) — also merges into the var metadata. Only a clause-list body
;; can carry one: the JVM wraps a single-arity `[params] body` into one
;; clause before it looks at the last form, which is why
;; (defn f [x] {:a 1}) is a map-returning body and not an attr-map.
;; malli's instrument suite declares its :malli/schema this way.
(def ^:private defn-trailing-attr-map
  (fn defn-trailing-attr-map [body]
    (let [trail (when (not (vector? (first body)))
                  (loop [cs body prev nil]
                    (if (seq cs) (recur (rest cs) (first cs)) prev)))]
      (when (and trail (map? trail) (not (symbol? trail))) trail))))

(def ^:private defn-drop-last
  (fn defn-drop-last [body]
    (loop [cs body acc []]
      (if (next cs) (recur (rest cs) (conj acc (first cs))) (seq acc)))))

;; A macro's two implicit params are elided, as the JVM's `sigs` elides
;; them: a defmacro expands to a defn whose params are
;; [&form &env & declared], and the var still reports the declared ones —
;; what `doc` and every arglists-driven tool show. The trigger is &form in
;; FIRST position, which is what the reference keys on. Guarded on length:
;; a lone [&form] is an IndexOutOfBoundsException there (subvec 2..1), a
;; crash rather than a shape worth reproducing.
(def ^:private defn-declared-params
  (fn defn-declared-params [pv]
    (if (and (vector? pv) (> (count pv) 1) (= (first pv) '&form))
      (subvec pv 2)
      pv)))

;; Past the single-arity `[params] body*` shape, every remaining form must
;; be a `([params] body*)` clause. Checked here because the arglists walk
;; below reaches straight into each one with (first (first cs)): given
;; (defn f 5 6) that is (first 5), and the user got the raw cast back —
;; "Don't know how to create ISeq from: java.lang.Long" as the compile
;; error for their defn. The reference reports this through
;; clojure.core.specs.alpha; jolt has no core specs, so it says so itself.
;; Only `assert`-tier primitives are available this early in the prelude.
(def ^:private defn-check-clauses
  (fn defn-check-clauses [body]
    (when (not (vector? (first body)))
      (loop [cs body]
        (if (seq cs)
          (let [c (first cs)]
            (if (if (seq? c) (vector? (first c)) false)
              (recur (rest cs))
              (throw (ex-info (str "Parameter declaration " (pr-str c)
                                   " should be a vector")
                              {:jolt.error/kind :analyze/invalid-fn-parameters
                               :jolt.error/type :analysis-error}))))
          nil)))))

;; :arglists — the parameter vectors as written (single arity: the one
;; vector; multi-arity: each clause's), attached to the var like Clojure so
;; doc/spec/expound tools can read it. tier-0 primitives only (loop, not
;; map/reduce, which load later).
(def ^:private defn-arglists
  (fn defn-arglists [body]
    (if (vector? (first body))
      (list (defn-declared-params (first body)))
      (loop [cs body acc []]
        (if (seq cs)
          (recur (rest cs) (conj acc (defn-declared-params (first (first cs)))))
          (seq acc))))))

;; precedence, matching the JVM's conj order: name metadata < the derived
;; :arglists < docstring < leading attr-map < trailing attr-map. So an
;; explicit {:arglists '([x])} in an attr-map wins, while ^{:arglists …} on
;; the NAME does not — the JVM ignores it there — and a :doc key in either
;; attr-map overrides the docstring (the JVM conj'es the attr-maps onto
;; {:doc docstring}, so they are what wins).
;; :arglists is QUOTED in the expansion like the JVM's
;; (def ^{:arglists (quote ([x]))} f …): def-meta-expr strips one quote
;; layer either way, but a consumer that EVALUATES the def-name meta
;; (tools.analyzer, typedclojure) read the bare ([x]) as a call of [x].
(def ^:private defn-var-meta
  (fn defn-var-meta [name-meta arglists docstring attr-map trail-attr]
    (let [m2 (if arglists (assoc (if name-meta name-meta {}) :arglists (list 'quote arglists)) name-meta)
          m3 (if docstring (assoc (if m2 m2 {}) :doc docstring) m2)
          m4 (if attr-map (conj (if m3 m3 {}) attr-map) m3)]
      (if trail-attr (conj (if m4 m4 {}) trail-attr) m4))))

;; defn: drop an optional leading docstring and attr-map, then (def name (fn ...)).
;; Emits the fn MACRO (not the fn* primitive) so destructuring params desugar — fn*
;; requires plain symbols (like Clojure). Unnamed (as before): self-recursion
;; resolves through the def'd var, so this only adds the desugaring step.
;; Both single- and multi-arity reduce to (fn ~@body) — fn takes either a params
;; vector + body or a sequence of ([params] body) clauses, so no arity branching is
;; needed. (map? is true for symbol forms too, so guard the attr-map with symbol?.)
;; Defined before fresh-sym below, which is a defn-.
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
        trail-attr (defn-trailing-attr-map body)
        body (if trail-attr (defn-drop-last body) body)
        ;; the bare name + any ^{:map} metadata the reader attached to it.
        fn-only-name (if (symbol? fn-name) fn-name (first (rest fn-name)))
        _ (defn-check-clauses body)
        meta-map (defn-var-meta (meta fn-only-name) (defn-arglists body)
                                docstring attr-map trail-attr)]
    ;; pass the name through to fn: the compiled fn's host name carries it, so
    ;; stack traces read app.deep/level3 instead of a gensym. All metadata
    ;; (docstring + attr-map + the name's own + :arglists) is attached to the def
    ;; name symbol, which analyze-def reads — so (meta #'f) reflects every source.
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
;; ONE let with g rebound per step, each step delegating to -> rather than
;; building the call itself — the reference's shape, and cond->>'s (30-macros).
;; It used to be a nest of let* forms with a fresh symbol per step, threading by
;; hand, so a consumer reading a single macroexpand-1 saw neither shape nor the
;; per-step position -> now carries. It stays in this tier, not beside cond->>,
;; because jolt.analyzer uses it and compiles while 10-seq loads.
;; loop/conj/nth only: partition, butlast and interleave are later tiers, and
;; this body runs at expansion time (like `ns` above), so `let` and `loop` from
;; earlier in this file are what it has.
(defmacro cond-> [expr & clauses]
  (let [g (fresh-sym)
        steps (loop [cs (seq clauses) acc []]
                (if cs
                  (recur (next (next cs))
                         (conj acc `(if ~(first cs) (-> ~g ~(nth cs 1)) ~g)))
                  acc))
        n (count steps)]
    (if (zero? n)
      `(let [~g ~expr] ~g)
      ;; g is rebound by every step but the last; the last IS the body.
      (let [binds (loop [i 0 acc [g expr]]
                    (if (< i (dec n))
                      (recur (inc i) (conj (conj acc g) (nth steps i)))
                      acc))]
        `(let ~binds ~(nth steps (dec n)))))))

;; case: nested =/or tests, or a constant-map lookup for a large case (below).
;; Test constants are NOT evaluated — symbols, lists, and composite literals
;; (vectors/maps/sets) are quoted so their elements aren't resolved as code; a
;; LIST in test position is an or-group of constants, while a vector/map/set is
;; a single literal constant. The case-* helpers below build it; g is the local
;; holding the dispatch value.

;; Quote symbols, lists, and composite literals (vector/map/set) so their
;; elements aren't resolved as code. A reader tagged literal (#bigdec/#inst/
;; #uuid/#regex, e.g. 4.0M) is represented as a pmap, so exclude it from the
;; map? arm — it must flow to the back end as its constant, not be quoted as
;; map data.
(defn- case-tagged? [c] (and (map? c) (= (:jolt/type c) :jolt/tagged)))
(defn- case-const [c]
  (if (and (or (symbol? c) (seq? c) (vector? c) (map? c) (set? c))
           (not (case-tagged? c)))
    `(quote ~c) c))

;; a keyword, char, nil, true or false constant is compared by identity: a
;; keyword is interned (and re-interned on image restore), and identical?
;; is an inline native op where = is a call. That is what the reference
;; does for these too (its case hashes then compares identity). Symbols,
;; strings and numbers keep = — a symbol is not interned here.
;; A CHAR is an immediate on the host, so identity IS equality for every
;; code point, and a `case` over characters — the way Clojure spells a
;; scanner state machine — was paying a generic `=` call per arm per
;; input character.
(defn- case-ident-const? [c] (or (keyword? c) (char? c) (nil? c) (true? c) (false? c)))
(defn- case-const-test [g c]
  (if (case-ident-const? c)
    `(identical? ~g ~c)
    `(= ~g ~(case-const c))))
(defn- case-test [g c]
  (if (seq? c)
    `(or ~@(map (fn [c] (case-const-test g c)) c))
    (case-const-test g c)))

;; Collect test constants pairwise (so a trailing unpaired default is
;; excluded), flattening list/or-group tests into individual constants.
;; seed-only fns (reduce/conj/first/rest/drop/empty?/seq?) — analyzer.clj
;; uses case during its own build, before some/distinct load.
(defn- case-constants [cls acc]
  (if (or (empty? cls) (empty? (rest cls)))
    acc
    (let [t (first cls)
          acc (if (seq? t) (reduce conj acc t) (conj acc t))]
      (case-constants (drop 2 cls) acc))))

;; first duplicate constant, wrapped in [x] (so a duplicate nil is detected);
;; nil = none. Clojure rejects duplicate case constants at compile time.
(defn- case-first-dup [items seen]
  (if (empty? items)
    nil
    (let [x (first items)]
      (if (reduce (fn [f s] (or f (= s x))) false seen)
        [x]
        (case-first-dup (rest items) (conj seen x))))))

;; Table dispatch, from 9 or 16 constants (below): ONE lookup in a quoted
;; constant map from test constant to clause index, then a binary search
;; over the index — O(log n) fixnum compares where the chain below is n
;; generic = calls. The reference gets O(1) from a tableswitch over the
;; hashes; this is the same contract, hash then equality, because a map
;; lookup IS hasheq then =.
;;
;; Only for constants whose map lookup provably agrees with the chain's =
;; (and the JVM): strings, symbols, keywords, chars, nil, booleans and
;; integers. Not floats — -0.0 and 0.0 are = with different hashes, and
;; ##NaN — and not ratios, decimals, collections or tagged/regex literals;
;; a case holding any of those keeps the chain. The quoted map is a
;; constant, hoisted and built once per def (backend hoist-const-for).
;; An all-keyword case stays below 65 constants: up to there the quoted
;; map is an array map, whose scan is the same identity compares the chain
;; already makes inline.
(defn- case-table-const? [c]
  (or (keyword? c) (string? c) (symbol? c) (char? c) (nil? c)
      (true? c) (false? c) (integer? c)))
(defn- case-table? [consts]
  (let [nconst (count consts)]
    (and ;; where the lookup starts paying (measured, 10k dispatches,
         ;; first vs last arm): a string or symbol arm is a generic =
         ;; call of ~12 ns, so a table of ~50 ns wins from 9 constants
         ;; (at 8 the quoted map is an array map, a scan like the
         ;; chain); an integer arm is cheaper and takes 16.
         (>= nconst (if (reduce (fn [a c] (or a (string? c) (symbol? c))) false consts) 9 16))
         (reduce (fn [ok c] (and ok (case-table-const? c))) true consts)
         (or (> nconst 64)
             (not (reduce (fn [ok c] (and ok (case-ident-const? c))) true consts))))))

;; no clause matched and no default — Clojure throws
;; IllegalArgumentException here (catchable by class).
(defn- case-no-match [g]
  `(throw (new IllegalArgumentException (str "No matching clause: " ~g))))

;; [constant->index map, bodies]: clause i's constants map to i; the
;; default (or the no-match throw) is the last body, the lookup's
;; not-found index.
(defn- case-table [g cls i m bodies]
  (if (or (empty? cls) (empty? (rest cls)))
    [m (conj bodies (if (empty? cls) (case-no-match g) (first cls)))]
    (let [t (first cls)
          m (if (seq? t) (reduce (fn [m c] (assoc m c i)) m t) (assoc m t i))]
      (case-table g (drop 2 cls) (inc i) m (conj bodies (nth cls 1))))))

;; bodies lo..hi, i known to be in that range: no equality test at a leaf.
(defn- case-bsearch [ix bodies lo hi]
  (if (= lo hi)
    (nth bodies lo)
    (let [mid (quot (+ lo hi) 2)]
      `(if (< ~ix ~(inc mid))
         ~(case-bsearch ix bodies lo mid)
         ~(case-bsearch ix bodies (inc mid) hi)))))

(defn- case-dispatch-table [g expr clauses]
  (let [tb (case-table g clauses 0 {} [])
        bodies (nth tb 1)
        n (dec (count bodies))
        ix (fresh-sym)]
    `(let* [~g ~expr
            ~ix (get (quote ~(nth tb 0)) ~g ~n)]
       ~(case-bsearch ix bodies 0 n))))

(defn- case-chain [g cls]
  (cond
    (empty? cls) (case-no-match g)
    (empty? (rest cls)) (first cls)
    :else `(if ~(case-test g (first cls)) ~(nth cls 1) ~(case-chain g (drop 2 cls)))))

(defmacro case [expr & clauses]
  (let [g (fresh-sym)
        consts (case-constants clauses [])
        dup (case-first-dup consts [])]
    (cond
      dup (throw (IllegalArgumentException. (str "Duplicate case test constant: " (first dup))))
      (case-table? consts) (case-dispatch-table g expr clauses)
      :else `(let* [~g ~expr] ~(case-chain g clauses)))))

;; for/doseq share these. for-parse-groups turns a binding vector into groups
;; [bind coll mods], mods a vector of [kw form] in SOURCE ORDER.
(defn- for-scan [bvec i bind coll mods]
  (if (and (< i (count bvec)) (keyword? (nth bvec i)))
    (let [k (nth bvec i) v (nth bvec (inc i))]
      (cond
        (= k :when)  (for-scan bvec (+ i 2) bind coll (conj mods [:when v]))
        (= k :let)   (for-scan bvec (+ i 2) bind coll (conj mods [:let v]))
        (= k :while) (for-scan bvec (+ i 2) bind coll (conj mods [:while v]))
        ;; kept, so for and doseq refuse it by name (skipping one form misread
        ;; the rest of the vector: "index out of bounds")
        :else        (for-scan bvec (+ i 2) bind coll (conj mods [k v]))))
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
        ;; the reference fails here too, from inside its own expansion (a
        ;; NullPointerException); this names the keyword
        :else        (throw (IllegalArgumentException. (str "Invalid 'doseq' keyword " k)))))))

;; for: lazy list comprehension, the reference's own expansion. Each binding
;; group is a step fn over its seq; modifiers apply in source order (:let binds,
;; :when skips the element with a recur, :while ends the group). The innermost
;; group conses each value onto the step over the rest -- and over a chunked seq
;; fills a chunk buffer a chunk at a time and chunk-conses it, as the reference
;; does -- while an outer group concats the inner group's seq for each element.
;; jolt's own expansion used (concat (list x) (step (rest s))) for every value:
;; a list, a concat and its descriptor per element, 4x the JVM's cost. An
;; innermost group with no modifiers is exactly (map (fn [x] body) coll) -- the
;; same values, realized the same way (a chunk at a time over a chunked seq, one
;; at a time otherwise) -- so it is emitted as that, and runs on the native map.
(defn- for-bad-keyword [k]
  (throw (IllegalArgumentException. (str "Invalid 'for' keyword " k))))

;; one element's modifier chain inside a for step fn, in source order: :let
;; binds, :while ends the group (a nil from the step), :when skips the element
;; with the form skip. finish is a thunk for the form that yields the element,
;; called only once the whole chain has been walked.
(defn- for-mod-chain [ms skip finish]
  (if (seq ms)
    (let [m (first ms) k (nth m 0) v (nth m 1) etc (rest ms)]
      (cond
        (= k :let) `(let ~v ~(for-mod-chain etc skip finish))
        (= k :while) `(when ~v ~(for-mod-chain etc skip finish))
        (= k :when) `(if ~v ~(for-mod-chain etc skip finish) ~skip)
        :else (for-bad-keyword k)))
    (finish)))

(declare ^:private for-step-fn)

;; an outer group's element: concat the inner group's seq over this element
;; onto the step over the rest, or skip the element when that seq is empty.
(defn- for-nested-yield [groups body gi giter gxs]
  `(let [iterys# ~(for-step-fn groups body (inc gi))
         fs# (seq (iterys# ~(nth (nth groups (inc gi)) 1)))]
     (if fs#
       (concat fs# (~giter (rest ~gxs)))
       (recur (rest ~gxs)))))

;; one chunk of the innermost group: fill a chunk buffer gb from the chunk
;; (cmods is an element's modifier chain, which appends it) and chunk-cons it
;; onto the step over the chunk rest -- or onto nil once a :while has ended it.
(defn- for-chunk [bind giter gxs gi2 gb cmods]
  `(let [c# (chunk-first ~gxs)
         size# (count c#)
         ~gb (chunk-buffer size#)]
     (if (loop [~gi2 0]
           (if (< ~gi2 size#)
             (let [~bind (nth c# ~gi2)]
               ~cmods)
             true))
       (chunk-cons (chunk ~gb) (~giter (chunk-rest ~gxs)))
       (chunk-cons (chunk ~gb) nil))))

;; the innermost group's step over a seq that may be chunked: a chunk is
;; filled into a chunk buffer and chunk-consed onto the step over the chunk
;; rest; any other seq conses one element at a time (do-mod is its chain).
(defn- for-chunked-step-fn [bind body mods giter gxs do-mod]
  (let [gi2 (gensym "i__")
        gb (gensym "b__")
        cmods (for-mod-chain mods `(recur (inc ~gi2))
                             (fn [] `(do (chunk-append ~gb ~body)
                                         (recur (inc ~gi2)))))]
    `(fn ~giter [~gxs]
       (lazy-seq
         (loop [~gxs ~gxs]
           (when-let [~gxs (seq ~gxs)]
             (if (chunked-seq? ~gxs)
               ~(for-chunk bind giter gxs gi2 gb cmods)
               (let [~bind (first ~gxs)]
                 ~(do-mod)))))))))

;; the step fn for binding group gi: from a seq to the lazy seq of body values
;; over it and the groups after it.
(defn- for-step-fn [groups body gi]
  (let [g (nth groups gi)
        bind (nth g 0)
        mods (nth g 2)
        next? (< (inc gi) (count groups))
        giter (gensym "iter__")
        gxs (gensym "s__")
        do-mod (fn []
                 (for-mod-chain mods `(recur (rest ~gxs))
                                (fn []
                                  (if next?
                                    (for-nested-yield groups body gi giter gxs)
                                    `(cons ~body (~giter (rest ~gxs)))))))]
    (cond
      next?
      `(fn ~giter [~gxs]
         (lazy-seq
           (loop [~gxs ~gxs]
             (when-first [~bind ~gxs]
               ~(do-mod)))))
      (empty? mods)
      `(fn ~giter [~gxs] (map (fn [~bind] ~body) ~gxs))
      :else
      (for-chunked-step-fn bind body mods giter gxs do-mod))))

(defmacro for [bindings body]
  (if (< (count bindings) 2)
    body
    (let [groups (for-parse-groups bindings 0 [])]
      `(let [iter# ~(for-step-fn groups body 0)]
         (iter# ~(nth (nth groups 0) 1))))))

;; doseq runs body for side effects across the bindings in constant space,
;; returning nil. A direct nested loop/recur per group (not (count (for …)),
;; which allocated a lazy cell per iteration and held the seq head): :let binds,
;; :when skips one element, :while stops the coll — same source-order semantics
;; as for.
;;
;; Each group walks a chunked seq (a vector's, a range's, a chunked map's) as
;; the reference's doseq does: a chunk at a time, stepping the seq once per
;; chunk, where first/next made a new cell for every element (3.5x a list's walk
;; over a vector; make doseqperf). The block is read where the seq holds it
;; (__chunk-vec and friends, natives-transduce.ss, lowered by the compiler), not
;; copied out. The outer loop takes one seq cell per pass: a chunked one runs its
;; block [start, end) in an inner index loop that answers true when the block is
;; done (so the outer steps past it) and nil when a :while stopped the walk; any
;; other seq — from the start, or a rest that turns out unchunked, a concat's —
;; drops into the plain first/next loop, which is what the old macro was, so a
;; list pays one __chunk-vec test per doseq over it and nothing per element. A
;; first pass through the block loop with an empty block (iv = ev = 0) cost a
;; short list 10%. The body is written out twice per group, once per loop, as
;; the reference's is: a doseq nested in a doseq's body doubles per level (the
;; gate in corpus "doseq / chunked" holds the growth to the reference's).

;; the block loop over chunk cv of seq cell sv: true when the block is done,
;; nil when a :while stopped the walk.
(defn- doseq-chunk-loop [sv cv iv ev bind mods k-form]
  (let [in-chunk `(recur (unchecked-inc ~iv))]
    `(let [~ev (__chunk-end ~sv)]
       (loop [~iv (__chunk-start ~sv)]
         (if (< ~iv ~ev)
           (let [~bind (__chunk-nth ~cv ~iv)]
             ~(comprehension-chain mods `(do ~k-form ~in-chunk) in-chunk nil))
           true)))))

;; the plain first/next loop from seq cell sv. It is entered with a cell the
;; outer loop already found non-nil, so it tests each next, not ps.
(defn- doseq-plain-loop [sv ps nx bind mods k-form]
  (let [plain-next `(let [~nx (next ~ps)] (when ~nx (recur ~nx)))]
    `(loop [~ps ~sv]
       (let [~bind (first ~ps)]
         ~(comprehension-chain mods `(do ~k-form ~plain-next) plain-next nil)))))

;; the walk for binding group idx, with the groups after it nested in its body
(defn- doseq-group [groups idx body]
  (let [g (nth groups idx)
        bind (nth g 0)
        mods (nth g 2)
        k-form (if (= idx (dec (count groups)))
                 `(do ~@body nil)
                 (doseq-group groups (inc idx) body))
        sv (fresh-sym) cv (fresh-sym) iv (fresh-sym) ev (fresh-sym) ps (fresh-sym)
        nx (fresh-sym)]
    `(loop [~sv (seq ~(nth g 1))]
       (when ~sv
         (let [~cv (__chunk-vec ~sv)]
           (if ~cv
             (when ~(doseq-chunk-loop sv cv iv ev bind mods k-form)
               (recur (seq (__chunk-after ~sv))))
             ~(doseq-plain-loop sv ps nx bind mods k-form)))))))

(defmacro doseq [bindings & body]
  (if (>= (count bindings) 2)
    `(do ~(doseq-group (for-parse-groups bindings 0 []) 0 body) nil)
    `(do ~@body nil)))

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
  ;; ^:once, as the reference writes it: the thunk runs once, and lets go of what
  ;; it closed over as it runs (backend emit-fn) -- a thunk walking a source must
  ;; not pin the source's head
  `(make-lazy-seq (~(with-meta 'fn* {:once true}) [] (coll->cells (do ~@body)))))

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
