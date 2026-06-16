# Jolt Core — additional clojure.core fns, transients, hashing
# Extracted from core.janet (jolt-nma8, phase 2b split).

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./regex)
(use ./config)
(use ./pv)
(use ./plist)
(use ./core_types)
(use ./core_coll)
(use ./core_print)
(use ./core_io)
(use ./core_refs)
# Threading macros (as regular functions? No, as macros in Clojure)
# These need to be defined as macros in the Jolt namespace system.
# For now, skip — they need proper macro definition via the evaluator.
# ============================================================

# ============================================================
# Initialization — intern everything into a context's namespace
# ============================================================

(def gensym_counter @{:val 0})

(defn gensym
  "Returns a new symbol with a unique name."
  [&opt prefix-string]
  (default prefix-string "G__")
  (def n (get gensym_counter :val))
  (put gensym_counter :val (+ n 1))
  {:jolt/type :symbol :ns nil :name (string prefix-string n)})


# if-let/when-let/if-some/when-some now live in the Clojure overlay
# (core/30-macros.clj) as defmacros.

(defn core-push-thread-bindings [b] (push-thread-bindings b))
(defn core-pop-thread-bindings [] (pop-thread-bindings))

(defn core-var-get [v] (var-get v))
(defn core-var-set [v val] (var-set v val))
(defn core-var? [x] (var? x))
(defn core-alter-var-root [v f & args] (apply alter-var-root v f args))
(defn core-alter-meta! [v f & args] (apply alter-meta! v f args))
(defn core-reset-meta! [v meta] (reset-meta! v meta))

# intern is a ctx-capturing clojure.core fn now (install-stateful-fns!).

# get-method/methods/remove-method/remove-all-methods/prefer-method are
# overlay macros (core/30-macros.clj) over the evaluator's *-setup fns.

(defn core-with-meta [obj meta]
  # Functions and scalars can't carry metadata in Jolt's model — return as-is
  # rather than crashing (Clojure attaches meta only to IObj values).
  (cond
    (or (function? obj) (cfunction? obj) (number? obj) (boolean? obj)
        (nil? obj) (string? obj) (keyword? obj) (buffer? obj))
    obj
    # Symbols carry metadata IN-PLACE in their struct's :meta field (this is how
    # the reader attaches ^hint and keeps symbol? true — see reader/read-meta).
    # The table-proto path below would make (symbol? (with-meta sym ..)) false and
    # break destructuring/hint reading, so keep a symbol a symbol.
    (and (struct? obj) (= :symbol (obj :jolt/type)))
    (struct ;(kvs obj) :meta meta)
    true
    (do
      (var new-obj @{})
      (each k (keys obj)
        (put new-obj k (get obj k)))
      # table/setproto requires a table, convert struct meta to table. meta may
      # be nil (Clojure allows (with-meta obj nil) to clear metadata).
      (var meta-tab @{})
      (when meta (each k (keys meta) (put meta-tab k (get meta k))))
      (table/setproto new-obj meta-tab)
      (put new-obj :jolt/meta meta)
      new-obj)))

(defn core-var-dynamic? [v]
  (var-dynamic? v))

# Java interop stubs
(def core-Object (fn [] (struct ;[:jolt/type :jolt/java-object])))

# Volatiles — typed box so deref/volatile? can recognize them.
(defn core-volatile! [v] @{:jolt/type :jolt/volatile :val v})
# volatile? / vreset! / vswap! now live in the Clojure collection tier — vreset!
# over jolt.host/ref-put!, vswap! over vreset! + get. The constructor stays native.

# Delays — created lazily by the `delay` macro; forced once via force/deref.
(defn core-make-delay [thunk] @{:jolt/type :jolt/delay :fn thunk :realized false :val nil})
(defn core-delay? [x] (and (table? x) (= :jolt/delay (x :jolt/type))))
# Proxy stub — returns nil form (macro, args not evaluated)
# Thread stubs
(def core-Thread (fn [& args] (struct ;[:jolt/type :jolt/thread])))
(def core-ThreadLocal (fn [& args] (struct ;[:jolt/type :jolt/thread-local])))
(def core-IllegalStateException (fn [& args] (struct ;[:jolt/type :jolt/exception])))



# letfn — mutually-recursive local fns. Expands to let* of fn* bindings; jolt
# closures capture the (shared, mutable) bindings table, so forward references
# between the fns resolve at call time.

# doseq — like `for` but eager and returns nil. Reuse `for`, force realization
# with `count`, discard the result.
# assert — (assert x) / (assert x message). Throws when x is falsy.

# ns-name now lives in the Clojure collection tier (pure over get + symbol).

# update lives in the Clojure kernel tier — core/00-kernel.clj. update-in stays
# (it's recursive and has internal callers).
(defn- ks-rest [ks]
  (if (tuple? ks) (tuple/slice ks 1) (array/slice ks 1)))

# assoc-in / update-in now live in the Clojure collection tier (canonical
# recursive ports).



# fnil now lives in the Clojure collection tier (core/20-coll.clj), with
# Clojure's canonical 2/3/4-arity (patch the first 1-3 args only).

# copy-var stubs for sci.impl.copy-vars (used by sci.impl.namespaces)
(defn core-copy-core-var [sym] nil)
(defn core-copy-var [sym & args] nil)
(defn core-macrofy [sym fn & more] fn)
(defn core-new-var [sym & args] nil)
# A free-standing var cell (not interned anywhere): with-local-vars binds
# these as locals; var-get/var-set work on any cell.
(defn core-local-var [&opt val]
  @{:jolt/type :jolt/var :name "local" :ns nil :root val :gen 0})
# with-open's close seam: a map-like value closes via its :close fn, a host
# file via file/close. No .close interop on the Janet host.
(defn core-close-resource [x]
  (cond
    (and (or (table? x) (struct? x)) (function? (get x :close))) ((get x :close))
    (= :core/file (type x)) (file/close x)
    (error (string "with-open: don't know how to close " (type x)))))
# sci stub: pass the registry map through (it was @{} — a raw host table that
# strict map-conj rightly rejects; identity also keeps sci's registry intact).
(defn core-avoid-method-too-large [& args] (if (> (length args) 0) (in args 0) {}))

# declare macro — accepts symbols, does nothing (forward declaration)

# Build a protocol value (a self-evaluating tagged table). Exposed so the overlay
# `defprotocol` can construct one via a fn call rather than embedding a tagged
# struct literal (which the interpreter would try to re-evaluate). `methods` is a
# {kw {:name str}} map; only :name is consulted (by satisfies?).
(defn core-make-protocol [name-str methods]
  @{:jolt/type :jolt/protocol
    :name {:jolt/type :symbol :ns nil :name name-str}
    :methods methods})

# extends? is a real overlay fn now (30-macros, over extenders).
(def core-implements? (fn [& args] false))

# ============================================================
# Additional clojure.core functions (conformance batch)
# ============================================================

(defn core-keyword
  "(keyword name) or (keyword ns name). Namespaced keywords are `:ns/name`.
  (keyword nil) is nil; the 2-arg form requires string args (nil ns allowed)."
  [& args]
  (case (length args)
    1 (let [a (in args 0)]
        (cond
          (nil? a) nil
          (keyword? a) a
          (or (string? a) (core-symbol? a)) (keyword (core-name a))
          (error (string "keyword requires a string, symbol or keyword, got " (type a)))))
    2 (let [ns (in args 0) nm (in args 1)]
        (when (not (and (or (nil? ns) (string? ns)) (string? nm)))
          (error "keyword ns and name must be strings"))
        (keyword (if ns (string ns "/" nm) nm)))
    (keyword ;args)))

(defn core-symbol
  "(symbol name) or (symbol ns name) -> a jolt symbol struct. name/ns must be
  strings (a single symbol arg is returned as-is)."
  [& args]
  (case (length args)
    1 (let [a (in args 0)]
        (cond
          (core-symbol? a) a
          (or (string? a) (keyword? a)) {:jolt/type :symbol :ns nil :name (core-name a)}
          (error (string "symbol requires a string or symbol, got " (type a)))))
    2 (let [ns (in args 0) nm (in args 1)]
        (when (not (and (or (nil? ns) (string? ns)) (string? nm)))
          (error "symbol ns and name must be strings"))
        {:jolt/type :symbol :ns ns :name nm})
    (error "symbol expects 1 or 2 args")))

# (take-nth's transducer arity lives in the overlay now.)


# filterv now lives in the Clojure collection tier (core/20-coll.clj).

# mapv lives in the Clojure kernel tier — core/00-kernel.clj.

# (interpose's transducer arity lives in the overlay now.)
# interpose / take-nth now live in the Clojure lazy tier (core/40-lazy.clj),
# with the canonical transducer arities.

# keep now lives in the Clojure lazy tier (core/40-lazy.clj).

# empty now lives in the Clojure collection tier (core/20-coll.clj); a lazy
# seq empties to () there (this fn returned a host table for it).

# not-empty now lives in the Clojure collection tier (core/20-coll.clj).

# rseq is defined only on vectors and sorted collections (Reversible).
(defn core-rseq [coll]
  (cond
    (pvec? coll) (tuple/slice (tuple ;(reverse (pv->array coll))))
    (core-sorted? coll) ((sorted-op coll :rseq) coll)
    (error (string "rseq requires a vector or sorted collection, got " (type coll)))))



# some-fn now lives in the Clojure collection tier (core/20-coll.clj).

# Associative = maps and (real) vectors only. pvec is a literal/built vector;
# tuples and lists are seq results, not associative.
# ifn? now lives in the Clojure collection tier — canonical IFn set (fns,
# keywords, symbols, maps, sets, vectors, vars); lists are NOT IFn.
# With a single item, Clojure returns it WITHOUT calling f. On ties, the last
# extremal item wins (>=/<= update), matching Clojure.
# Clojure's min-key/max-key: the 2-arg base compares with strict < / > (so the
# second wins on ties/NaN), and each further item switches on <= / >=. This
# asymmetry reproduces the JVM's NaN-ordering behavior. Janet's < / > are used
# directly (NaN comparisons are false, never throwing).
# keys must be numbers (NaN allowed) — like Clojure, which compares them with </>.
# min-key / max-key now live in the Clojure collection tier (core/20-coll.clj).

# vary-meta / namespace-munge now live in the Clojure collection tier
# (core/20-coll.clj) — pure compositions of meta/with-meta and str/map.

# Exceptions (ex-info / ex-data / ex-message)
(defn core-ex-info [msg data & more]
  @{:jolt/type :jolt/ex-info :message msg :data data
    :cause (if (> (length more) 0) (in more 0) nil)})
# ex-data / ex-message / ex-cause now live in the Clojure collection tier
# (core/20-coll.clj) — pure over get on the tagged value the constructor builds.

# String split/replace that accept either a literal string or a regex value.
(defn core-str-split [pat s]
  (if (regex? pat)
    (re-split pat s)
    (string/split pat s)))
(defn core-str-replace-all [pat repl s]
  (if (regex? pat)
    (re-replace-all pat s repl)
    (string/replace-all pat repl s)))
(defn core-str-replace-first [pat repl s]
  (if (regex? pat)
    (re-replace-first pat s repl)
    (string/replace pat repl s)))

# Iterator/enumeration seqs — Jolt has no Java iterators, so adapt to plain seq.
# enumeration-seq / iterator-seq live in the Clojure collection tier.
# xml-seq now lives in the Clojure collection tier (core/20-coll.clj).
# line-seq now lives in the Clojure IO tier (core/50-io.clj), over the reader
# protocol of the *in* family.
(defn core-re-matcher [re s] @{:jolt/type :jolt/matcher :re re :s s :pos 0})

# bean / print-method / print-dup / the proxy surface live in the Clojure
# collection tier (JVM-shape stubs; print hooks inert until jolt-g1r).
# == lives in the Clojure collection tier (core/20-coll.clj); memfn is an
# overlay macro (core/30-macros.clj) over the .method call sugar.
# eduction / ->Eduction live in the Clojure collection tier (core/20-coll.clj).

(def- char-escapes
  {10 "\\n" 9 "\\t" 13 "\\r" 12 "\\f" 8 "\\b" 34 "\\\"" 92 "\\\\"})
(def- char-names
  {10 "newline" 9 "tab" 13 "return" 12 "formfeed" 8 "backspace" 32 "space"})
# char-escape-string / char-name-string now live in the Clojure collection
# tier as char-keyed maps. The CODE-keyed tables below stay: pr-render uses them.


# subseq / rsubseq over sorted collections
# subseq / rsubseq now live in the Clojure sorted tier (core/25-sorted.clj),
# along with the constructors and all sorted-coll semantics.

# ============================================================
# Additional clojure.core functions
# ============================================================

# Integer-valued: a finite number equal to its floor. Infinity floors to itself
# but is NOT integer-valued (so float?/double? are true for ##Inf, and int?/
# pos-int?/… are false), and NaN is excluded by the equality check.
(defn- intval? [x] (and (number? x) (< (math/abs x) math/inf) (= x (math/floor x))))

# Forcing lazy seqs
# Map entries (represented as 2-element vectors)
# key/val require a map entry (a 2-element vector/tuple in Jolt); Clojure throws
# otherwise. (Jolt can't distinguish a 2-vector from a real MapEntry.)
# A map entry is a 2-element tuple — Jolt produces tuples only from map
# iteration (first/seq/map over a map), while vector literals are pvecs and
# lists are arrays. So key/val/map-entry? accept a 2-tuple and reject a plain
# vector, matching Clojure (where a MapEntry is distinct from a vector).
(defn- entry-like? [x] (and (tuple? x) (= 2 (length x))))
# key / val now live in the Clojure collection tier (core/20-coll.clj),
# along with find (previously missing from jolt entirely).
(defn core-map-entry? [x] (entry-like? x))

# Reversible (supports rseq) = vectors and sorted collections.
# Numeric predicates (Jolt has no ratios/bigdec). nat-int?/pos-int?/neg-int?/
# ratio?/decimal?/rational? live in the Clojure collection tier (core/20-coll.clj).
# Jolt has no ratio type, so numerator/denominator have no valid input (Clojure
# requires a Ratio and throws otherwise).
# numerator / denominator now live in the Clojure collection tier (Jolt has
# no ratios; they throw, as on a non-ratio in Clojure).

# special-symbol? lives in the Clojure collection tier (a quoted symbol set).

# record? now lives in the Clojure collection tier (tagged-value predicate).

# Promise: single-threaded box backed by an atom (deref returns nil until set).
# promise / deliver live in the Clojure collection tier (an atom; deref of an
# undelivered promise is nil — single-threaded host, no blocking).

(defn core-tagged-literal [tag form] @{:jolt/type :jolt/tagged-literal :tag tag :form form})
# ensure-reduced / halt-when live in the Clojure collection tier
# (core/20-coll.clj) — halt-when is the canonical ::halt-map version there.
(defn core-re-groups [m] (error "re-groups: stateful matchers are not supported in Jolt"))

# Transients — real mutable scratch collections backed by Janet's native arrays
# and tables (host interop): O(1) conj!/assoc!/dissoc!/disj!/pop!, frozen back to
# a persistent value by persistent!. A transient is a tagged table holding either
# a Janet array (vectors) or a Janet table keyed by canonical key (maps/sets, so
# collection keys still compare by value). The mutating ops return the transient.
(defn core-transient [coll]
  (cond
    (pvec? coll)
      @{:jolt/type :jolt/transient :kind :vector :arr (pv->array coll)}
    (set? coll)
      (let [t @{}] (each e (phs-seq coll) (put t (canon-key e) e))
        @{:jolt/type :jolt/transient :kind :set :tbl t})
    (or (phm? coll) (and (struct? coll) (nil? (get coll :jolt/type))))
      (let [t @{}]
        (each pair (realize-for-iteration coll)
          (put t (canon-key (in pair 0)) @[(in pair 0) (in pair 1)]))
        @{:jolt/type :jolt/transient :kind :map :tbl t})
    # mutable-build arrays (vectors/lists) — copy into a transient vector
    (array? coll) @{:jolt/type :jolt/transient :kind :vector :arr (array/slice coll)}
    # tuples (reader vectors / map entries) are vectors too
    (tuple? coll) @{:jolt/type :jolt/transient :kind :vector :arr (array ;coll)}
    (error (string "Don't know how to create a transient from " (type coll)))))

# A transient is invalidated by persistent!; using it afterwards is a bug.
(defn- tr-check-active! [t]
  (when (get t :jolt/persistent)
    (error "Transient used after persistent! call")))

(defn- tr-conj! [t x]
  (tr-check-active! t)
  (case (t :kind)
    :vector (array/push (t :arr) x)
    :set    (put (t :tbl) (canon-key x) x)
    :map    (cond
              # a [k v] pair (map-entry / 2-vector)
              (and (or (pvec? x) (tuple? x) (array? x))
                   (= 2 (if (pvec? x) (pv-count x) (length x))))
                (put (t :tbl) (canon-key (vnth x 0)) @[(vnth x 0) (vnth x 1)])
              # a map: merge all its entries
              (or (phm? x) (and (struct? x) (nil? (get x :jolt/type))))
                (each e (map-entries-of x)
                  (put (t :tbl) (canon-key (in e 0)) @[(in e 0) (in e 1)]))
              (error "conj! on a transient map requires a [key value] pair or a map")))
  t)

(defn- tr-assoc! [t k v]
  (tr-check-active! t)
  (case (t :kind)
    :vector (let [a (t :arr)]
              (when (not (and (number? k) (= k (math/floor k)) (>= k 0) (<= k (length a))))
                (error (string "Index " k " out of bounds for assoc! on a transient vector of length " (length a))))
              (if (= k (length a)) (array/push a v) (put a k v)))
    :map    (put (t :tbl) (canon-key k) @[k v])
    (error "assoc! expects a transient vector or map"))
  t)

# The bang ops require a transient (Clojure throws otherwise); no lenient
# fallback to the persistent op.
(defn core-conj! [& args]
  (cond
    (= 0 (length args)) (core-transient (make-vec @[]))   # (conj!) -> (transient [])
    (= 1 (length args)) (first args)                      # (conj! coll) -> coll, as-is
    (let [t (first args) xs (tuple/slice args 1)]
      (if (core-transient? t)
        (do (each x xs (tr-conj! t x)) t)
        (error "conj! requires a transient")))))

(defn core-assoc! [t & kvs]
  # Unlike assoc, assoc! accepts an ODD number of args — a missing final value
  # is taken as nil (so (get kvs (+ i 1)) rather than (in ...), which would
  # error on the dangling key).
  (if (core-transient? t)
    (do (var i 0) (while (< i (length kvs)) (tr-assoc! t (in kvs i) (get kvs (+ i 1))) (+= i 2)) t)
    (error "assoc! requires a transient")))

(defn core-dissoc! [t & ks]
  (if (and (core-transient? t) (= :map (t :kind)))
    (do (tr-check-active! t) (each k ks (put (t :tbl) (canon-key k) nil)) t)
    (error "dissoc! requires a transient map")))

(defn core-disj! [t & xs]
  (if (and (core-transient? t) (= :set (t :kind)))
    (do (tr-check-active! t) (each x xs (put (t :tbl) (canon-key x) nil)) t)
    (error "disj! requires a transient set")))

(defn core-pop! [t]
  (if (and (core-transient? t) (= :vector (t :kind)))
    (do (tr-check-active! t)
        (when (= 0 (length (t :arr))) (error "Can't pop empty vector"))
        (array/pop (t :arr)) t)
    (error "pop! requires a transient vector")))

(defn core-persistent! [t]
  (if (core-transient? t)
    (do
      (tr-check-active! t)
      (def result
        (case (t :kind)
          :vector (make-vec (t :arr))
          # The transient already deduped into a native table; bulk-build the
          # persistent value ONCE (bottom-up HAMT) instead of folding a phm-assoc
          # per entry. This is the lever behind every transient-based builder
          # (frequencies/group-by/set/into) — jolt-5vsp collections.
          :set (phs-from-seq (values (t :tbl)))
          :map (phm-from-pairs (values (t :tbl)))))
      # Invalidate: any further bang op (or a second persistent!) now throws.
      (put t :jolt/persistent true)
      result)
    (error "persistent! requires a transient")))

# Unchecked arithmetic — Jolt numbers don't overflow, so these are plain ops.
# unchecked-* arithmetic lives in the Clojure collection tier
# (core/20-coll.clj); only the masking byte/short/char coercions remain above.

# Hashing helpers
# Hashes are masked to 24 bits at each step so intermediate products stay within
# Janet's integer range (a float here would make band error).
(defn- h24 [x] (band (hash x) 0xffffff))
(defn core-hash-combine [a b] (band (bxor (h24 a) (+ (h24 b) 0x9e3779)) 0xffffff))
(defn core-hash-ordered-coll [coll]
  (var h 1) (each x (realize-for-iteration coll) (set h (band (+ (* 31 h) (h24 x)) 0xffffff))) h)
(defn core-hash-unordered-coll [coll]
  (var h 0) (each x (realize-for-iteration coll) (set h (band (+ h (h24 x)) 0xffffff))) h)

# prefers is a macro over prefers-setup now (the store lives on the VAR).



# parse-uuid lives in the Clojure collection tier (core/20-coll.clj) over
# re-matches + the __make-uuid host constructor (types.janet).

