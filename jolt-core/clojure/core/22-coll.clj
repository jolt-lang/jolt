;; clojure.core — collection tier, part 3 (canonical Clojure ports: find,
;; merge-with, memoize, get-in/assoc-in/update-in, comp, transduce/into/eduction,
;; and the JVM-shape stubs). Continues 21-coll.clj; same constraints.
;; (key/val/group-by/frequencies live in 20-coll.clj.)

;; --- canonical Clojure ports -------------------------------------------------
;; find first — merge-with and memoize below use it.

;; Presence (contains?), not value, decides — so (find {:a nil} :a) is [:a nil].
;; Works on vectors by index. The result is a REAL entry (key/val are strict).
;;
;; The entry's key is the key the COLLECTION holds, not the equal one the caller
;; probed with: (meta (key (find m k))) reads the stored key's metadata on the
;; JVM. A native map answers from its own storage, and a sorted map through its
;; :entry-at op; a vector (keyed by index), a record and a deftype have no
;; separate stored key, so those mint the entry from k.
;;
;; RT.find takes nil, Associative and java.util.Map (plus the key-addressable
;; transients) and throws on anything else — a set, a string, a list.
;;
;; Everything but the native map lives here, apart from find, because find's
;; 2-arity call sites lower to the native jolt-find2 (op-registry): that answers
;; for a native map directly and calls this one by name for the rest, so the type
;; taxonomy stays in Clojure and the hot path pays no var deref. NOT private —
;; jolt-find2 resolves it by name.
(defn find-other [m k]
  (cond
    (nil? m) nil
    ;; associative? is false for a sorted SET, so one falls through to the throw
    (associative? m)
    (if (sorted? m)
      ((get (jolt.host/ref-get m :ops) :entry-at) m k)
      (when (contains? m k) (jolt.host/map-entry k (get m k))))
    ;; a java.util.Map has no Clojure key object to preserve, and neither does a
    ;; transient (the JVM's TransientHashMap.entryAt mints from the probe key too)
    (or (instance? java.util.Map m) (jolt.host/transient-associative? m))
    (when (contains? m k) (jolt.host/map-entry k (get m k)))
    :else (throw (jolt.host/throwable
                  "java.lang.IllegalArgumentException"
                  (str "find not supported on type: " (.getName (class m)))))))

(defn find [m k]
  (let [e (jolt.host/entry-at m k ::not-native)]
    (if (identical? e ::not-native) (find-other m k) e)))

;; Back to the plain JVM shape now that find is a native call: conj the ENTRY, so
;; the result carries the source map's own key objects.
(defn select-keys [map keyseq]
  (reduce (fn [m k] (let [e (find map k)] (if (nil? e) m (conj m e))))
          (with-meta {} (meta map)) keyseq))

;; some? lives in the top leaf block now (forward refs are errors).
;; identity, not = — the reference bodies are Util/identical, and a mixed-type
;; (= true x) here walked the whole extension-arm registry (a registered arm
;; predicate like jolt.time's runs per call): true? on a keyword measured 367ns
;; against the JVM's 16. identical? lowers to eq?.
(defn true? [x] (identical? true x))
(defn false? [x] (identical? false x))

(defn some-vals
  "Returns a map with only the non-nil values of map m. Returns nil if m has no
  non-nil vals."
  [m]
  (reduce-kv (fn [m k v] (if (some? v) (assoc m k v) m)) nil m))

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
  (if (number? x) x (throw (ClassCastException. (str "num requires a number, got: " x)))))

;; == numeric equality: 1-arity is trivially true without inspecting the value
;; (Clojure's shape); 2+ args must be numbers, as Numbers.equiv throws.
(defn ==
  ([x] true)
  ([x y]
   (if (and (number? x) (number? y))
     (= x y)
     (throw (ClassCastException. (str "Cannot cast to number: " (if (number? x) y x))))))
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
    (throw (IllegalArgumentException. (str "parse-boolean requires a string, got: " s)))))

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

;; Eduction — a REDUCIBLE and seqable view applying xform to coll, matching
;; clojure.core.Eduction. Reducing one drives the transducer straight into the
;; accumulator (the IReduceInit path, which jolt-reduce honors via iface-method)
;; with no seq cells — which is why (reduce f init (eduction …)) beats the
;; equivalent transduce; seq realizes lazily through `sequence`, so an infinite
;; or expensive source is not forced.
;;
;; It was previously just (sequence xform coll) — a plain lazy seq — so reduce
;; allocated a cell per element and walked it: 152ms where transduce over the
;; same xform and source took 58ms, against the JVM's 9.2ms.
;;
;; Interfaces follow the JVM: Sequential (so sequential? is true) but NOT
;; IPersistentCollection (coll? false) and NOT ISeq (seq? false), and count
;; throws — the value is reducible and seqable, not counted.
(deftype Eduction [xform coll]
  clojure.lang.IReduceInit
  (reduce [_ f init] (transduce xform (completing f) init coll))
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [_] (seq (sequence xform coll)))
  (count [_] (throw (UnsupportedOperationException.
                     "count not supported on this type: Eduction"))))

(defn eduction [& args]
  (->Eduction (apply comp (butlast args)) (last args)))

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
;; proxy-super is a MACRO on the JVM, and has to be one here too: its first
;; argument is a method NAME, not an expression, so analyzing it as one rejects
;; every proxy body whose super call names something no var matches —
;; (proxy-super reset) in clojure.tools.logging's log-stream. `this` is in scope
;; inside a proxy body, and the proxy carries its base instance, so this reaches
;; the base's own implementation rather than the override now running.
(defmacro proxy-super [meth & args]
  `(jolt.host/proxy-super-call ~'this ~(name meth) ~@args))
(defn construct-proxy [c & args] (throw (UnsupportedOperationException. "construct-proxy: not supported in Jolt")))
(defn get-proxy-class [& interfaces] (throw (UnsupportedOperationException. "get-proxy-class: not supported in Jolt")))

;; Clojure's serialized-require: require while holding
;; clojure.lang.RT/REQUIRE_LOCK. Private there and here, and here it exists ONLY
;; for code that reaches for the private var — jolt's own requiring-resolve does
;; not go through it, for the reason given below. jolt.host/with-monitor rather
;; than `locking`: that macro is defined in 30-macros.clj, which loads after this
;; file.
(defn ^:private serialized-require [& args]
  (jolt.host/with-monitor clojure.lang.RT/REQUIRE_LOCK
    (fn* [] (apply require args))))

;; resolve, requiring the symbol's namespace first when it isn't loaded yet —
;; the dynamic-require pattern (tooling, plugin registries). The require and
;; resolve are the runtime fns, so this works identically under jolt run and
;; in an AOT binary (which compiles the namespace from the source roots).
;;
;; A plain require, NOT serialized-require. Clojure holds RT/REQUIRE_LOCK across
;; the require because its loader has no other guard against two threads loading
;; one namespace. jolt's loader does (loader.ss, JLS 12.4.2 per namespace): a
;; second thread's require blocks until the first thread's load of that namespace
;; has finished, so the process-wide lock would add nothing it does not already
;; give — and it adds a lock edge the loader's wait-for graph cannot see. Thread A
;; holds the lock and waits on a namespace B is loading; B's top level calls
;; requiring-resolve and waits on the lock. Neither wait is a loader wait, so the
;; cycle walk never fires and both hang for good. test/chez/concurrent-require.clj
;; pins this (property J).
(defn requiring-resolve [sym]
  (if (qualified-symbol? sym)
    (or (resolve sym)
        (do (require (symbol (namespace sym)))
            (resolve sym)))
    (throw (new IllegalArgumentException (str "Not a qualified symbol: " sym)))))
