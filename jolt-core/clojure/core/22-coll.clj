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
          {} keyseq))

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

;; Canonical IFn set: fns, keywords, symbols, maps (sorted incl.),
;; sets, vectors, and vars — NOT lists ((ifn? '(1 2)) is false in Clojure).
(defn ifn? [x]
  (or (fn? x) (keyword? x) (symbol? x) (map? x) (set? x) (vector? x) (var? x)))

;; Auto-promoting (') and unchecked arithmetic. Jolt numbers don't overflow,
;; so all of these are the checked ops; fixed arities mirror Clojure's
;; signatures. unchecked-divide-int goes through quot, so dividing by zero
;; throws as on the JVM.
(def +' +)
(def -' -)
(def *' *)
(def inc' inc)
(def dec' dec)
(defn unchecked-add [x y] (+ x y))
(defn unchecked-subtract [x y] (- x y))
(defn unchecked-multiply [x y] (* x y))
(defn unchecked-negate [x] (- x))
(defn unchecked-inc [x] (+ x 1))
(defn unchecked-dec [x] (- x 1))
(def unchecked-add-int unchecked-add)
(def unchecked-subtract-int unchecked-subtract)
(def unchecked-multiply-int unchecked-multiply)
(def unchecked-negate-int unchecked-negate)
(def unchecked-inc-int unchecked-inc)
(def unchecked-dec-int unchecked-dec)
(defn unchecked-divide-int [x y] (quot x y))
(defn unchecked-remainder-int [x y] (rem x y))
(defn unchecked-int [x] (int x))
(def unchecked-long unchecked-int)

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

;; Masking integer coercions (not aliases): byte/short wrap to their width.
;; unchecked-byte/short truncate to a number; unchecked-char returns a char (as on
;; the JVM). int handles chars, so (unchecked-byte \a) works.
(defn unchecked-byte [x] (bit-and (int x) 0xff))
(defn unchecked-short [x] (bit-and (int x) 0xffff))
(defn unchecked-char [x] (char (bit-and (int x) 0xffff)))
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
(defn enumeration-seq [e] (seq e))
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
    'finally 'new 'set! '. 'monitor-enter 'monitor-exit})

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
