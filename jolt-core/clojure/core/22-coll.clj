;; clojure.core — collection tier, part 3 (canonical Clojure ports: key/val/find,
;; merge-with, memoize, group-by, frequencies, transduce/into/eduction, and the
;; JVM-shape stubs). Continues 21-coll.clj; same constraints.

;; --- canonical Clojure ports -------------------------------------------------
;; key/val/find first — merge-with and memoize below use them.

;; Strict, as in Clojure: an entry is what (seq m) yields (a host tuple), NOT
;; a plain vector — (key [1 2]) throws.
;; key/val moved above the hierarchies section (underive uses them).

;; find was previously missing from jolt entirely. Presence (contains?), not
;; value, decides — so (find {:a nil} :a) is [:a nil]. Works on vectors by
;; index. The result must be a REAL entry (key/val are strict), so it is
;; minted as the first entry of a one-entry map — nil values survive (the
;; map builder switches to a phm when nil is involved).
(defn find [m k]
  (when (contains? m k) (first {k (get m k)})))

;; some? lives in the top leaf block now (forward refs are errors).
(defn true? [x] (= true x))
(defn false? [x] (= false x))

;; Presence-preserving and order-preserving: a key with a nil value is kept, and
;; the result follows keyseq order (an empty-map base keeps nil values and
;; canonicalizes collection keys).
(defn select-keys [map keyseq]
  (reduce (fn [m k] (if (contains? map k) (assoc m k (get map k)) m))
          (with-meta {} (meta map)) keyseq))

(defn some-vals
  "Returns a map with only the non-nil values of map m. Returns nil if m has no
  non-nil vals."
  [m]
  (reduce-kv (fn [m k v] (if (some? v) (assoc m k v) m)) nil m))

(defn zipmap [keys vals]
  (loop [m {} ks (seq keys) vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs)) (next ks) (next vs))
      m)))

;; Structmaps (legacy). A struct basis is the ordered vector of slot keys; a
;; struct map is a plain map carrying every basis key (nil when unset), in basis
;; order, so it looks up and compares like any other map.
(defn create-struct [& keys] (vec keys))

(defn struct-map [basis & inits]
  (let [base (loop [m {} ks (seq basis)]
               (if ks (recur (assoc m (first ks) nil) (next ks)) m))]
    (loop [m base kvs (seq inits)]
      (if kvs
        (recur (assoc m (first kvs) (first (next kvs))) (next (next kvs)))
        m))))

(defn struct [basis & vals]
  (loop [m (struct-map basis) ks (seq basis) vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs)) (next ks) (next vs))
      m)))

(defn accessor [basis key]
  (fn [m] (get m key)))

;; conj semantics per entry arg (a map merges, a [k v] pair adds); nil args are
;; no-ops; all-nil (or no args) is nil.
(defn merge [& maps]
  (when (some identity maps)
    (reduce (fn [acc m] (if (nil? m) acc (conj (or acc {}) m)))
            maps)))

(defn merge-with [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          ;; presence — not nil-of-value — decides combination
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn get-in
  ([m ks] (reduce get m ks))
  ([m ks not-found]
   ;; a fresh table is its own identity — a present-but-nil step is
   ;; distinguished from a missing one
   (let [sentinel (hash-map)]
     (loop [m m ks (seq ks)]
       (if ks
         (let [nxt (get m (first ks) sentinel)]
           (if (identical? sentinel nxt)
             not-found
             (recur nxt (next ks))))
         m)))))

(defn req!
  "Returns the value mapped to key k in map m, like `get`, but throws
  IllegalArgumentException when k is not present. Unlike `get`, does not nil-pun:
  a key present with a nil value returns nil, an absent key throws. The primitive
  behind checked-keys destructuring (:keys! / :syms! / :strs!)."
  {:added "1.13"}
  [m k]
  ;; a fresh map is its own identity, so a present-but-nil value is distinguished
  ;; from an absent key (same trick as get-in's sentinel).
  (let [sentinel (hash-map)
        v (get m k sentinel)]
    (if (identical? sentinel v)
      (throw (new IllegalArgumentException (str "Expected key: " k)))
      v)))

;; find-based, so nil RESULTS are cached too; args canonicalize as a collection key.
(defn memoize [f]
  (let [mem (atom (hash-map))]
    (fn [& args]
      ;; plain let/if, not if-let: this tier loads before 30-macros defines it
      (let [e (find (deref mem) args)]
        (if e
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc args ret)
            ret))))))

(defn partial
  ([f] f)
  ([f a] (fn [& args] (apply f a args)))
  ([f a b] (fn [& args] (apply f a b args)))
  ([f a b c] (fn [& args] (apply f a b c args)))
  ([f a b c & more] (fn [& args] (apply f a b c (concat more args)))))

(defn trampoline
  ([f] (let [ret (f)] (if (fn? ret) (trampoline ret) ret)))
  ([f & args] (trampoline (fn [] (apply f args)))))

;; Canonical pairwise max/min: > / < throw on non-numbers, and the NaN
;; behavior is Clojure's by construction.
(defn max
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more] (reduce max (max x y) more)))

(defn min
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more] (reduce min (min x y) more)))

(defn reverse [coll] (reduce conj (list) coll))

;; An empty coll of the same category, carrying the receiver's metadata (Clojure's
;; .empty() does EMPTY.withMeta(meta())). Sorted colls keep their comparator (the
;; value's own :empty op). Strings and scalars are nil, as in Clojure; a lazy
;; seq empties to ().
(defn empty [coll]
  (cond
    (nil? coll) nil
    ;; a deftype/record with its own empty (IPersistentCollection) — e.g.
    ;; data.priority-map — uses it, before the generic map/set/vector arms.
    (jolt.host/jrec-method? coll "empty") (.empty coll)
    ;; a defrecord without its own empty can't have one (RT: UnsupportedOperation)
    (record? coll) (throw (new UnsupportedOperationException
                               (str "Can't create empty: " (.getName (class coll)))))
    (sorted? coll) ((get (jolt.host/ref-get coll :ops) :empty) coll)
    (map? coll) (with-meta {} (meta coll))
    (set? coll) (with-meta #{} (meta coll))
    (vector? coll) (with-meta [] (meta coll))
    (coll? coll) (with-meta () (meta coll))
    :else nil))

(defn assoc-in [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn update-in [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [[k & ks] ks]
               (if ks
                 (assoc m k (up (get m k) ks f args))
                 (assoc m k (apply f (get m k) args)))))]
    (up m ks f args)))

;; jolt keywords have no intern table (any keyword "exists"), so find-keyword
;; always finds — babashka makes the same call.
(defn find-keyword
  ([nm] (keyword nm))
  ([ns nm] (keyword ns nm)))

;; The raw Inst protocol method; jolt insts have one representation, so it is
;; inst-ms itself.
(defn inst-ms* [i] (inst-ms i))

;; Canonical comp — here rather than a host primitive so each stage is invoked with
;; jolt call semantics: (comp seq :content) works because the keyword stage
;; goes through IFn dispatch.
(defn comp
  ([] identity)
  ([f] f)
  ([f g]
   ;; fixed arities first (Clojure's own shape): the 1-arg path — every
   ;; map/filter stage — is two direct calls, no rest-seq, no apply.
   (fn
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs] (reduce comp (comp f g) fs)))

;; Canonical IFn set: fns, keywords, symbols, maps (sorted incl.), sets,
;; vectors, vars — NOT lists ((ifn? '(1 2)) is false in Clojure) — plus the
;; host callables (multimethods, promises) and a deftype/record implementing
;; clojure.lang.IFn's invoke.
(defn ifn? [x]
  (if (or (fn? x) (keyword? x) (symbol? x) (map? x) (set? x) (vector? x) (var? x)
          (jolt.host/callable-host? x)
          (jolt.host/jrec-method? x "invoke"))
    true
    false))

;; Auto-promoting (') and unchecked arithmetic. Jolt numbers don't overflow,
;; so all of these are the checked ops; fixed arities mirror Clojure's
;; signatures. unchecked-divide-int goes through quot, so dividing by zero
;; throws as on the JVM.
(def +' +)
(def -' -)
(def *' *)
(def inc' inc)
(def dec' dec)
;; unchecked-add / -subtract / -multiply / -negate / -inc / -dec (+ the -int
;; variants), -divide-int / -remainder-int, and the unchecked-long/-int casts are
;; host-defined (host/chez/seq.ss, converters.ss): they WRAP like the JVM
;; primitive conversions, which a plain overlay over checked casts can't do.

;; int? is integer? on jolt: one number type, so fixed-precision and
;; arbitrary-precision integers coincide.
(defn int? [x] (integer? x))

;; num: Clojure coerces to java.lang.Number; jolt just checks.
(defn num [x]
  (if (number? x) x (throw (str "num requires a number, got: " x))))

;; == numeric equality: 1-arity is trivially true without inspecting the value
;; (Clojure's shape); 2+ args must be numbers, as Numbers.equiv throws.
(defn ==
  ([x] true)
  ([x y]
   (if (and (number? x) (number? y))
     (= x y)
     (throw (str "Cannot cast to number: " (if (number? x) y x)))))
  ([x y & more]
   (if (== x y)
     (apply == y more)
     false)))

;; ensure-reduced / halt-when: canonical Clojure. halt-when smuggles the halt
;; value through reduce in a ::halt-keyed map and unwraps it in the completion
;; arity, so the halt REPLACES the whole reduction result.
(defn ensure-reduced [x] (if (reduced? x) x (reduced x)))

(defn halt-when
  ([pred] (halt-when pred nil))
  ([pred retf]
   (fn [rf]
     (fn
       ([] (rf))
       ([result]
        (if (and (map? result) (contains? result ::halt))
          (get result ::halt)
          (rf result)))
       ([result input]
        (if (pred input)
          (reduced (hash-map ::halt (if retf (retf (rf result) input) input)))
          (rf result input)))))))

;; parse-boolean: exact "true"/"false" only; nil on anything else, throw on a
;; non-string (Clojure 1.11).
(defn parse-boolean [s]
  (if (string? s)
    (cond (= s "true") true (= s "false") false :else nil)
    (throw (str "parse-boolean requires a string, got: " s))))

(defn newline [] (print "\n") nil)

;; seque: jolt is single-threaded eager here — the queue is a no-op and the
;; coll passes through.
(defn seque
  ([s] s)
  ([n-or-q s] s))

(defn array-seq [arr & _] (seq arr))

(defn to-array-2d [coll] (to-array (map to-array coll)))

;; Wrapping (unchecked) coercions: truncate to the width and sign-fold like the
;; JVM primitive conversions ((unchecked-byte 200) is -56); unchecked-char wraps
;; into char range. unchecked-long/int are host natives (converters.ss).
(defn unchecked-byte [x]
  (let [b (bit-and (unchecked-long x) 0xff)] (if (< b 128) b (- b 256))))
(defn unchecked-short [x]
  (let [s (bit-and (unchecked-long x) 0xffff)] (if (< s 32768) s (- s 65536))))
(defn unchecked-char [x] (char (bit-and (unchecked-long x) 0xffff)))
(defn unchecked-float [x] (double x))
(defn unchecked-double [x] (double x))

;; --- transduce / into / eduction ---------------------------------------------
;; Canonical transduce: build the stacked rf once, reduce (which honors
;; `reduced` and steps lazy seqs incrementally), then run the completion arity.
(defn transduce
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
   (let [xf (xform f)]
     (xf (reduce xf init coll)))))

;; into stays a host primitive: it's perf-wall hot (the into-vec bench pays ~11%
;; through the overlay call layers — same lesson as even?/odd?).

;; eduction is EAGER on jolt (documented divergence): the composed
;; xforms applied to coll, realized into a vector.
;; A lazy application of the composed xforms to coll (sequence is lazy now), so an
;; infinite or expensive source isn't realized up front. Not a re-iterable Eduction
;; object, but reduce / into / seq / first over it all work.
(defn eduction [& args]
  (let [coll (last args)
        xforms (butlast args)]
    (if xforms
      (sequence (apply comp xforms) coll)
      (sequence coll))))

(defn ->Eduction [xform coll] (sequence xform coll))

;; --- JVM-shape stubs and trivial shells --------------------------------------
;; Pure compositions or documented jolt stubs; the host keeps nothing.
;; enumeration-seq drives a java.util.Enumeration (StringTokenizer, etc.) through
;; hasMoreElements/nextElement, like the JVM; an already-seqable arg (a jolt seq —
;; some host code passes a list) just seqs.
(defn enumeration-seq [e]
  (if (or (nil? e) (seq? e) (sequential? e))
    (seq e)
    (lazy-seq (when (.hasMoreElements e)
                (cons (.nextElement e) (enumeration-seq e))))))
(defn iterator-seq [i] (seq i))

;; jolt is single-threaded: a promise is an atom, deref never blocks
;; ((deref undelivered) is nil rather than a hang).
(defn promise [] (atom nil))
(defn deliver [p v] (reset! p v) p)

(defn bean [x] (if (map? x) x {}))

(defn uri? [x] false)

;; An EVALUATED set of quoted symbols — a quoted set literal ('#{if ...})
;; stays an unevaluated reader form on jolt and contains? can't see into it.
(def ^:private special-syms
  #{'if 'do 'let* 'fn* 'quote 'var 'def 'loop* 'recur 'throw 'try 'catch
    'finally 'new 'set! '. 'monitor-enter 'monitor-exit
    '& 'case* 'deftype* 'letfn* 'reify*})

(defn special-symbol? [s] (contains? special-syms s))

;; print-method / print-dup are real multimethods in the io tier (50-io.clj).

;; JVM proxies don't exist on this host: the read-only surface is inert,
;; the constructive surface throws.
(defn proxy-mappings [p] {})
(defn proxy-call-with-super [f p meth] (f))
(defn init-proxy [p mappings] p)
(defn update-proxy [p mappings] p)
(defn proxy-super [& args] (throw "proxy-super: JVM proxies are not supported in Jolt"))
(defn construct-proxy [c & args] (throw "construct-proxy: not supported in Jolt"))
(defn get-proxy-class [& interfaces] (throw "get-proxy-class: not supported in Jolt"))

;; resolve, requiring the symbol's namespace first when it isn't loaded yet —
;; the dynamic-require pattern (tooling, plugin registries). The require and
;; resolve are the runtime fns, so this works identically under joltc run and
;; in an AOT binary (which compiles the namespace from the source roots).
(defn requiring-resolve [sym]
  (if (qualified-symbol? sym)
    (or (resolve sym)
        (do (require (symbol (namespace sym)))
            (resolve sym)))
    (throw (new IllegalArgumentException (str "Not a qualified symbol: " sym)))))
