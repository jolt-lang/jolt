;; clojure.core — collection tier, part 2 (rand/shuffle/sort host seams, uuid,
;; seq builders tree-seq/file-seq/xml-seq, fn combinators comparator/reductions).
;; Continues 20-coll.clj; same constraints (pure, eager, no macros), loaded in the
;; 20 slot before 25-sorted. (clojure.test lives in stdlib/clojure/test.clj.)

;; --- leaves over the rand / sort host seams ----------------------------------

;; Canonical truncation toward zero via int (the kernel fn floored, which is
;; wrong for a negative n).
(defn rand-int [n] (int (rand n)))

;; Pure-functional Fisher-Yates over vector assoc; returns a vector, as in
;; Clojure. Collections only — a string is seqable but not shuffleable, as on
;; the JVM (Collections/shuffle wants a Collection).
(defn shuffle [coll]
  ;; Collections/shuffle wants a java.util.Collection — a map is not one
  (when (or (not (coll? coll)) (map? coll))
    (throw (ex-info (str "shuffle requires a collection, got: " coll) {})))
  (loop [v (vec coll) i (dec (count v))]
    (if (pos? i)
      (let [j (rand-int (inc i))
            t (nth v i)]
        (recur (assoc (assoc v i (nth v j)) j t) (dec i)))
      v)))

;; Canonical sort-by: the default comparator is compare (so nil sorts first,
;; like Clojure — the kernel fn used host ordering, which put nil last); the
;; comparator compares KEYS and may be 3-way or a boolean predicate (the host
;; sort seam normalizes).
(defn sort-by
  ([keyfn coll] (sort-by keyfn compare coll))
  ([keyfn comp coll]
   ;; a collection is never a Comparator (the JVM cast would fail); catching it
   ;; here beats silently "sorting" through coll-as-fn lookups
   (when (coll? comp)
     (throw (new ClassCastException (str (class comp) " cannot be cast to java.util.Comparator"))))
   ;; comp may be a reify/deftype Comparator, which is not IFn — route it through
   ;; the shared comparator seam rather than invoking it directly.
   (let [cf (__comparator-fn comp)]
     (sort (fn [x y] (cf (keyfn x) (keyfn y))) coll))))

;; parse-uuid / random-uuid live in the host (natives-misc.ss) — a uuid is a Chez
;; record, not a tagged map, so post-prelude.ss re-asserts the native versions
;; over anything the overlay defines here. An overlay copy would be dead code
;; (this one was, and it called a __make-uuid constructor the host never had).

;; The char escape/name tables, as char-keyed maps (Clojure's shape).
(def ^:private char-escape-strings
  {\newline "\\n" \tab "\\t" \return "\\r" \formfeed "\\f"
   \backspace "\\b" \" "\\\"" \\ "\\\\"})
(defn char-escape-string [c] (get char-escape-strings c))

(def ^:private char-name-strings
  {\newline "newline" \tab "tab" \return "return" \formfeed "formfeed"
   \backspace "backspace" \space "space"})
(defn char-name-string [c] (get char-name-strings c))

;; Random selection over the host rand primitives — the reference shape:
;; nth directly (nil returns nil via RT.nth; a set throws like the JVM).
(defn rand-nth [coll]
  (nth coll (rand-int (count coll))))

(defn random-sample
  ([prob] (filter (fn [_] (< (rand) prob))))
  ([prob coll] (filter (fn [_] (< (rand) prob)) coll)))

(defn comparator [pred]
  (fn [a b] (cond (pred a b) -1 (pred b a) 1 :else 0)))

;; Lazy: the running accumulators, one at a time (matches Clojure).
(defn reductions
  ([f coll]
   (lazy-seq
     (let [s (seq coll)]
       (if s
         (reductions f (first s) (rest s))
         (list (f))))))
  ([f init coll]
   (cons init
         (lazy-seq
           (when-let [s (seq coll)]
             (reductions f (f init (first s)) (rest s)))))))

;; Lazy pre-order DFS (matches Clojure): node, then its children's walks spliced
;; via the (now lazy) mapcat.
(defn tree-seq [branch? children root]
  (let [walk (fn walk [node]
               (lazy-seq
                 (cons node
                       (when (branch? node)
                         (mapcat walk (children node))))))]
    (walk root)))

;; file-seq: the tree of paths under root (root included), directories walked
;; via the host dir primitives. Paths (strings), not File objects. (Lives below
;; tree-seq: forward references are analysis errors.)
(defn file-seq [root]
  (if (__file? root)
    ;; java.io.File tree: walk via the File method surface so leaves are File
    ;; values callers can invoke .isFile/.getName/slurp on.
    (tree-seq (fn [f] (.isDirectory f)) (fn [f] (seq (.listFiles f))) root)
    (tree-seq __dir? __list-dir root)))

;; Canonical flatten via tree-seq: the leaves (non-sequential nodes) in order.
;; Flattens lists too (sequential?), matching Clojure/CLJS.
(defn flatten [coll]
  (filter (complement sequential?) (rest (tree-seq sequential? seq coll))))

;; xml-seq: tree-seq over XML element trees. Elements are maps with :content.
(defn xml-seq [root]
  (tree-seq (complement string?) (comp seq :content) root))

;; Lazy interleave: round-robin one element from each coll until any exhausts.
(defn interleave
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (when (and s1 s2)
         (cons (first s1)
               (cons (first s2)
                     (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & cs]
   (lazy-seq
     (let [ss (map seq (list* c1 c2 cs))]
       (when (every? identity ss)
         (concat (map first ss)
                 (apply interleave (map rest ss))))))))

;; rationalize is host-native (java/bigdec.ss): a double routes through its
;; shortest decimal print like BigDecimal.valueOf, so (rationalize 1.1) is 11/10.

;; 0-arg: a stateful transducer (tracks [seen? prev] in a volatile, so no sentinel
;; value is needed). 1-arg: eager dedupe of consecutive equal elements.
(defn dedupe
  ([]
   (fn [rf]
     (let [pv (volatile! [false nil])]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [[seen prior] @pv]
            (vreset! pv [true input])
            (if (and seen (= prior input)) result (rf result input))))))))
  ([coll]
   (let [step (fn step [s prev]
                (make-lazy-seq
                  (fn* []
                    (let [s (seq s)]
                      (if s
                        (let [x (first s)]
                          (if (= x prev)
                            (coll->cells (step (rest s) prev))
                            (coll->cells (cons x (step (rest s) x)))))
                        nil)))))]
     ;; defer (seq coll) into the lazy-seq so a side-effecting source is not
     ;; realized at construction (dedupe is lazy, like Clojure's).
     (make-lazy-seq
       (fn* []
         (let [s (seq coll)]
           (if s
             (coll->cells (cons (first s) (step (rest s) (first s))))
             nil)))))))

;; Internal helper for {:keys [...]} destructuring over a seq of k/v pairs —
;; canonical Clojure 1.11 shape (core.clj seq-to-map-for-destructuring):
;; even pairs build a map (later keys win, as createAsIfByAssoc), a SINGLE
;; element is returned as-is (the trailing-map calling convention), and an
;; unpaired key past pairs throws.
(defn seq-to-map-for-destructuring [s]
  (if (next s)
    (loop [m {} xs (seq s)]
      (if xs
        (if (next xs)
          (recur (assoc m (first xs) (second xs)) (nnext xs))
          (throw (str "No value supplied for key: " (first xs))))
        m))
    (if (seq s) (first s) {})))

;; Host-coupled fns that are pure logic over existing core primitives, so they
;; need no new jolt.host surface.

;; vary-meta: f applied to obj's metadata (+ extra args), reattached. meta and
;; with-meta are the irreducible host primitives; vary-meta is just their compose.
(defn vary-meta [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

;; namespace-munge: Clojure namespace name -> legal Java package name (- -> _).
(defn namespace-munge [s]
  (apply str (map (fn [c] (if (= c \-) \_ c)) (seq (str s)))))

;; ex-info accessors. The constructor (ex-info) stays native — it builds the tagged
;; value and wires into throw — but the value exposes :jolt/type/:message/:data/
;; :cause via get, so the accessors are pure over get. A thrown non-ex-info arrives
;; wrapped as {:jolt/type :jolt/exception :value v}; unwrap that first.
(defn- ex-info-val? [x] (= (get x :jolt/type) :jolt/ex-info))
(defn- ex-unwrap [e]
  (if (= (get e :jolt/type) :jolt/exception) (get e :value) e))
(defn ex-data [e]
  (let [e (ex-unwrap e)] (if (ex-info-val? e) (get e :data) nil)))
(defn ex-message [e]
  (let [e (ex-unwrap e)]
    (cond (ex-info-val? e) (get e :message)
          :else            nil)))
(defn ex-cause [e]
  (let [e (ex-unwrap e)] (if (ex-info-val? e) (get e :cause) nil)))

;; Throwable->map: the reference data rendering of a throwable. :via chains
;; through ex-cause the way the reference walks getCause; :cause/:data come
;; from the root cause. Throwables carry no stack-trace elements here, so
;; :trace is empty and :via entries have no :at.
(defn Throwable->map [o]
  (let [msg-of (fn [t] (or (ex-message t) (jolt.host/condition-message t)))
        entry (fn [t]
                (let [c (class t)
                      m {:type (symbol (if (string? c) c (.getName c)))
                         :message (msg-of t)}]
                  (if-let [d (ex-data t)] (assoc m :data d) m)))
        via (loop [acc [] t o]
              (if (some? t) (recur (conj acc t) (ex-cause t)) acc))
        root (peek via)
        m {:via (mapv entry via) :trace []}
        m (if-let [c (msg-of root)] (assoc m :cause c) m)]
    (if-let [d (ex-data root)] (assoc m :data d) m)))

;; inst-ms: epoch milliseconds of an instant; throws on a non-inst (Clojure
;; protocol behavior).
(defn inst-ms [x]
  (if (inst? x) (get x :ms) (throw (str "inst-ms requires an inst, got: " x))))

;; Clojure 1.11 map transformers. An empty-map base keeps insertion order;
;; transformed keys canonicalize via assoc (collisions: last entry in seq order
;; wins, matching the reference).
(defn update-keys [m f]
  (reduce-kv (fn [acc k v] (assoc acc (f k) v)) {} m))

(defn update-vals [m f]
  (reduce-kv (fn [acc k v] (assoc acc k (f v))) {} m))

;; Vector-returning partition variants (1.11): lazy seqs OF vectors.
(defn partitionv
  ([n coll] (map vec (partition n coll)))
  ([n step coll] (map vec (partition n step coll)))
  ([n step pad coll] (map vec (partition n step pad coll))))

;; partition-all is a lazy-tier fn (40-lazy) — declared so partitionv-all
;; compiles; bound by the time anything calls it.
(declare partition-all)

(defn partitionv-all
  ([n coll] (map vec (partition-all n coll)))
  ([n step coll] (map vec (partition-all n step coll))))

;; First part a vector, rest a seq — matching the reference implementation.
(defn splitv-at [n coll]
  [(vec (take n coll)) (drop n coll)])

;; with-redefs-fn: temporarily set each var's ROOT to the mapped value, run
;; the thunk, restore the saved roots even on throw. The with-redefs macro
;; (30-macros) builds the {var val} map from names.
;;
;; Root, and read back with .getRawRoot rather than var-get, because that is
;; what .bindRoot / .getRawRoot mean in the reference implementation and the
;; difference shows: under (binding [*v* 5] (with-redefs [*v* 2] …)) var-get
;; answers 5, so a save/restore through it would write the thread value over the
;; var's real root on the way out. It also has to be a root write for the redef
;; to reach a thread that did not inherit this one's bindings — a redefed var is
;; usually not dynamic at all, and var-set refuses those outright.
(defn with-redefs-fn [binding-map func]
  (let [vars (vec (keys binding-map))
        saved (mapv (fn [v] (.getRawRoot v)) vars)]
    (doseq [v vars] (alter-var-root v (constantly (get binding-map v))))
    (try
      (func)
      (finally
        ;; loop/recur, not dotimes: dotimes is a 30-macros macro and this tier
        ;; compiles before it exists (a forward ref would resolve to the macro
        ;; fn at runtime and mis-apply it).
        (loop [i 0]
          (when (< i (count vars))
            (alter-var-root (nth vars i) (constantly (nth saved i)))
            (recur (inc i))))))))
;; A vector's seq IS a real chunked-seq (chunk-first hands out a 32-element block).
;; This is only a placeholder so references compile during overlay load; the host
;; rebinds chunked-seq? to na-chunked-seq? in post-prelude.ss, which returns true
;; for a vector seq and false otherwise.
(defn chunked-seq? [x] false)

;; Atom peripheral operations. atom/swap!/reset!/deref stay native — the compiler
;; depends on them and they're hot. swap-vals!/reset-vals!/compare-and-set! ALSO
;; stay native: atoms.ss implements them atomically (mutex/CAS) and binds them as
;; clojure.core vars, so the overlay must NOT redefine them — a check-then-act
;; composition here races under jolt's real OS threads (futures/agents/fork-thread),
;; losing updates. get-validator reads a slot; add-watch/remove-watch/
;; set-validator! mutate the atom (or its watches sub-table) through the one host
;; primitive jolt.host/ref-put! — the minimal mutation kernel the overlay can't
;; express over core fns (a nil value removes the key).
(defn get-validator [a] (get a :validator))
(defn add-watch [a key f]
  (jolt.host/ref-put! (get a :watches) key f) a)
(defn remove-watch [a key]
  (jolt.host/ref-put! (get a :watches) key nil) a)
(defn set-validator! [a f]
  (jolt.host/ref-put! a :validator f) nil)

;; vreset!/vswap! live in the seq tier (10-seq.clj): its transducers use them.

;; Future status predicates — pure reads of the future's :cached/:cancelled slots.
;; future? stays native (deref/future-cancel/realized? call it); future-call and
;; future-cancel stay native too (OS threads).
(defn future-done? [x]
  (if (future? x) (boolean (get x :cached)) (throw "future-done? requires a future")))
(defn future-cancelled? [x]
  (and (future? x) (boolean (get x :cancelled))))

;; ns-name: a namespace object's :name as a symbol. Pure over get + symbol.
(defn ns-name [ns]
  (let [nm (get ns :name)] (if nm (symbol (str nm)) nil)))

;; Java-array element access. Jolt arrays are mutable backing arrays; aget/alength
;; read them (nth/count) and aset writes a slot through ref-put!. Both handle the
;; multi-dimensional form (aget a i j ... / aset a i j ... v) by walking. The array
;; constructors (object-array/make-array/to-array/...) stay native — they build the
;; mutable backing.
(defn aget [arr & idxs]
  (reduce (fn [v i] (nth v i)) arr idxs))
(defn alength [arr] (if (nil? arr) (throw (NullPointerException. "")) (count arr)))
(defn aset [arr & idxs+val]
  (let [n (count idxs+val)
        val (nth idxs+val (dec n))
        target (reduce (fn [t k] (nth t k)) arr (take (- n 2) idxs+val))]
    (jolt.host/ref-put! target (nth idxs+val (- n 2)) val)
    val))

;; areduce / amap: index-loop macros over an array, verbatim in shape from
;; clojure.core. Both bind the array once (the expression may have side effects)
;; and walk it by index, so they need nothing beyond alength/aget/aset/aclone.
;; areduce threads an accumulator; amap writes into an aclone of the source and
;; returns it, leaving the original untouched.
(defmacro areduce [a idx ret init expr]
  `(let [a# ~a
         l# (alength a#)]
     (loop [~idx 0 ~ret ~init]
       (if (< ~idx l#)
         (recur (unchecked-inc-int ~idx) ~expr)
         ~ret))))

(defmacro amap [a idx ret expr]
  `(let [a# ~a
         ~ret (aclone a#)
         l# (alength a#)]
     (loop [~idx 0]
       (if (< ~idx l#)
         (do (aset ~ret ~idx ~expr)
             (recur (unchecked-inc-int ~idx)))
         ~ret))))

;; --- fn combinators + host-free stubs ----------------------------------------

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f, has the
  same effects, if any, and returns the opposite truth value."
  [f]
  (fn [& args] (not (apply f args))))

;; Canonical Clojure fnil: patches only the FIRST 1-3 arguments.
(defn fnil
  ([f x]
   (fn [a & args] (apply f (if (nil? a) x a) args)))
  ([f x y]
   (fn [a b & args] (apply f (if (nil? a) x a) (if (nil? b) y b) args)))
  ([f x y z]
   (fn [a b c & args]
     (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) args))))

(defn clojure-version [] "1.11.0-jolt")

;; bigdec is a host fn (host/chez/java/bigdec.ss) — a real BigDecimal value type.
;; numerator/denominator are host natives (converters.ss) over Chez's exact
;; rationals; a non-ratio is the Ratio cast failure.

;; jolt has no reflection, but a few common JVM interfaces carry a modeled
;; ancestry (jolt.host/class-supers) so reflective checks like
;; (ancestors (class f)) answer like the JVM.
(defn supers [x]
  (let [s (jolt.host/class-supers x)]
    (if s (set s) #{})))

;; munge: the name as the compiler would spell it in a class or method name,
;; preserving the argument's type — a symbol munges to a symbol, anything else to
;; a string. Clojure's own definition, character for character: the whole
;; Compiler CHAR_MAP, not just the dash. Rewriting dashes alone left every other
;; special character in place, so the result was not a legal identifier at all —
;; (munge 'a?) answered a? — and a caller asking "do these two compile to one
;; class name?" (typedclojure's datatype collision check does) was told no for
;; a? and a_QMARK_, which do.
(defn munge [s]
  (let [m (clojure.lang.Compiler/munge (str s))]
    (if (symbol? s) (symbol m) m)))

(defn test
  "Calls the :test fn from v's metadata; :ok if it runs, :no-test if absent."
  [v]
  (let [t (:test (meta v))]
    (if t (do (t) :ok) :no-test)))

