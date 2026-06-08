;; clojure.core — collection tier. Pure, eager fns expressed as compositions of
;; already-frozen core primitives (reduce/assoc/get/conj/filter/vec/count/>=).
;; No host internals, no laziness, no macros — so they compile cleanly and stay
;; redefinable. Loaded after the seq tier; self-hosted in compile mode.
;;
;; Same migration rule as the seq tier (see 10-seq.clj): not in core-renames, no
;; internal Janet callers, not used by the self-hosted compiler.

;; Base is (hash-map), not the {} literal: a literal map is a struct that doesn't
;; canonicalize collection keys across representations (a {:a 1} literal vs
;; (hash-map :a 1) key), whereas a PHM does — so counting/grouping by collection
;; value needs the PHM base (the prior Janet impl used make-phm for this reason).
(defn frequencies [coll]
  (reduce (fn [counts x] (assoc counts x (inc (get counts x 0)))) (hash-map) coll))

(defn group-by [f coll]
  (reduce (fn [ret x] (let [k (f x)] (assoc ret k (conj (get ret k []) x)))) (hash-map) coll))

(defn not-empty [coll]
  (if (or (nil? coll) (zero? (count coll))) nil coll))

(defn filterv [pred coll]
  (vec (filter pred coll)))

;; Greatest/least x by (k x). Canonical Clojure multi-arity: the first pair uses
;; strict < / > and the fold uses <= / >= — this exact ordering reproduces the
;; JVM IEEE-754 NaN behavior (e.g. (min-key identity 1 ##NaN) => ##NaN). > / <
;; throw on non-numbers, as Clojure does.
(defn max-key
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (let [kx (k x) ky (k y)
         v (if (> kx ky) x y)
         kv (if (> kx ky) kx ky)]
     (loop [v v kv kv more more]
       (if (seq more)
         (let [w (first more) kw (k w)]
           (if (>= kw kv) (recur w kw (next more)) (recur v kv (next more))))
         v)))))

(defn min-key
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (let [kx (k x) ky (k y)
         v (if (< kx ky) x y)
         kv (if (< kx ky) kx ky)]
     (loop [v v kv kv more more]
       (if (seq more)
         (let [w (first more) kw (k w)]
           (if (<= kw kv) (recur w kw (next more)) (recur v kv (next more))))
         v)))))

;; Function combinators (pure HOFs).
(defn juxt [& fs]
  (fn [& args] (mapv (fn [f] (apply f args)) fs)))

(defn every-pred [& preds]
  (fn [& xs] (every? (fn [p] (every? p xs)) preds)))

(defn some [pred coll]
  (when-let [s (seq coll)]
    (or (pred (first s)) (recur pred (next s)))))

(defn some-fn [& preds]
  (fn [& xs] (some (fn [p] (some p xs)) preds)))

(defn not-any? [pred coll] (not (some pred coll)))

(defn not-every? [pred coll] (not (every? pred coll)))

(defn split-at [n coll] [(take n coll) (drop n coll)])

(defn split-with [pred coll] [(take-while pred coll) (drop-while pred coll)])

(defn ident? [x] (or (keyword? x) (symbol? x)))

(defn qualified-ident? [x] (or (qualified-symbol? x) (qualified-keyword? x)))

(defn simple-ident? [x] (or (simple-symbol? x) (simple-keyword? x)))

;; Jolt has no ratio or bigdecimal types, so these are constants / reduce to int?.
(defn ratio? [x] false)
(defn decimal? [x] false)
(defn rational? [x] (int? x))
(defn nat-int? [x] (and (int? x) (>= x 0)))
(defn neg-int? [x] (and (int? x) (neg? x)))
(defn pos-int? [x] (and (int? x) (pos? x)))

(defn replicate [n x] (map (fn [_] x) (range n)))

(defn take-last [n coll]
  (let [c (vec coll) len (count c)]
    (when (pos? len) (subvec c (max 0 (- len n))))))

(defn drop-last
  ([coll] (drop-last 1 coll))
  ([n coll] (let [c (vec coll)] (subvec c 0 (max 0 (- (count c) n))))))

(defn distinct?
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
   (if (not (= x y))
     (loop [s #{x y} xs more]
       (if xs
         (let [x (first xs)]
           (if (contains? s x) false (recur (conj s x) (next xs))))
         true))
     false)))

(defn replace [smap coll] (mapv (fn [x] (get smap x x)) coll))

(defn nthnext [coll n]
  (loop [n n xs (seq coll)]
    (if (and xs (pos? n))
      (recur (dec n) (next xs))
      xs)))

(defn bounded-count [n coll] (min n (count coll)))

(defn run! [proc coll] (reduce (fn [_ x] (proc x) nil) nil coll) nil)

(defn completing
  ([f] (completing f identity))
  ([f cf] (fn ([] (f)) ([x] (cf x)) ([x y] (f x y)))))

;; Matches Clojure exactly: n<=0 returns coll unchanged; for n>0 the walk yields
;; (seq xs), and an exhausted/nil walk falls back to () via (or ... ()) — so
;; (nthrest nil 100) is () (not nil), while (nthrest nil 0) is nil.
(defn nthrest [coll n]
  (if (pos? n)
    (or (loop [n n xs coll]
          (let [s (and (pos? n) (seq xs))]
            (if s (recur (dec n) (rest s)) (seq xs))))
        (list))
    coll))

(defn abs [x] (if (neg? x) (- 0 x) x))

(defn NaN? [x]
  (if (number? x) (not (= x x)) (throw (str "NaN? requires a number"))))

;; No distinct host object / undefined types on Jolt.
(defn object? [x] false)
(defn undefined? [x] false)

(defn keyword-identical? [a b] (= a b))

(defn comparator [pred]
  (fn [a b] (cond (pred a b) -1 (pred b a) 1 :else 0)))

;; Eager (Jolt has no laziness yet): a vector of the running accumulators.
(defn reductions
  ([f coll]
   (let [s (seq coll)]
     (if s
       (reductions f (first s) (rest s))
       (list (f)))))
  ([f init coll]
   (loop [acc init xs (seq coll) out [init]]
     (if xs
       (let [a (f acc (first xs))] (recur a (next xs) (conj out a)))
       out))))

;; The lazy tree-seq (using lazy-seq/make-lazy-seq) correctly implements
;; Clojure semantics but triggers compile-mode issues in self-hosted compilation.
;; When compile mode is fixed, replace the eager version below with:
;;   (defn tree-seq [branch? children root]
;;     (let [walk (fn walk [node]
;;                  (lazy-seq
;;                    (cons node
;;                      (when (branch? node)
;;                        (mapcat walk (children node))))))]
;;       (walk root)))
(defn tree-seq [branch? children root]
  (let [walk (fn walk [acc node]
                (let [acc (conj acc node)]
                  (if (branch? node)
                    (reduce walk acc (children node))
                    acc)))]
    (walk [] root)))

;; Canonical flatten via tree-seq: the leaves (non-sequential nodes) in order.
;; Flattens lists too (sequential?), matching Clojure/CLJS.
(defn flatten [coll]
  (filter (complement sequential?) (rest (tree-seq sequential? seq coll))))

;; xml-seq: tree-seq over XML element trees. Elements are maps with :content.
(defn xml-seq [root]
  (tree-seq (complement string?) (comp seq :content) root))

;; Eager interleave: round-robin one element from each coll until any exhausts.
;; A lazy version (canonical Clojure cons-recursion) hits the same compile-mode
;; overlay bug as reductions/tree-seq — a self-recursive lazy-seq leaks its macro
;; expansion under :compile? (see jolt-r81). Eager until that's fixed.
(defn interleave [& colls]
  (if (empty? colls)
    (list)
    (let [cs (mapv vec colls)
          n (apply min (map count cs))]
      (loop [i 0 out []]
        (if (< i n)
          (recur (inc i) (reduce (fn [o c] (conj o (nth c i))) out cs))
          out)))))

;; No ratio type on Jolt, so rationalize is identity.
(defn rationalize [x] x)

;; trampoline: repeatedly calls f with args until a non-function result.

;; rand-int: random integer in [0, n). Uses Janet math/random.

;; Eager dedupe of consecutive equal elements (Jolt has no transducer arity yet).
(defn dedupe [coll]
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
    (let [s (seq coll)]
      (if s
        (make-lazy-seq
          (fn* [] (coll->cells (cons (first s) (step (rest s) (first s))))))
        ()))))

;; Internal helper for {:keys [...]} destructuring over a seq of k/v pairs:
;; builds a map from consecutive pairs, dropping a trailing unpaired element.
(defn seq-to-map-for-destructuring [s]
  (if (sequential? s)
    (loop [m {} xs (seq s)]
      (if (and xs (next xs))
        (recur (assoc m (first xs) (second xs)) (next (next xs)))
        m))
    s))

;; Phase 4 (jolt-1j0): host-coupled fns that are pure logic over existing core
;; primitives, so they need no new jolt.host surface.

;; vary-meta: f applied to obj's metadata (+ extra args), reattached. meta and
;; with-meta are the irreducible host primitives; vary-meta is just their compose.
(defn vary-meta [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

;; namespace-munge: Clojure namespace name -> legal Java package name (- -> _).
(defn namespace-munge [s]
  (apply str (map (fn [c] (if (= c \-) \_ c)) (seq (str s)))))

;; reduce-kv over a map (k v) or vector (index v). Both branches go through reduce,
;; so reduced short-circuits — and the vector path indexes correctly. (The prior
;; Janet version saw a pvec as a table and folded over its internal keys; it also
;; ignored reduced.) nil folds to init, matching Clojure.
(defn reduce-kv [f init coll]
  (cond
    (vector? coll) (reduce (fn [acc i] (f acc i (nth coll i))) init (range (count coll)))
    (map? coll)    (reduce (fn [acc k] (f acc k (get coll k))) init (keys coll))
    (nil? coll)    init
    :else (throw (str "reduce-kv not supported on: " coll))))

;; ex-info accessors. The Janet constructor (ex-info) stays — it builds the tagged
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
          (string? e)      e
          :else            nil)))
(defn ex-cause [e]
  (let [e (ex-unwrap e)] (if (ex-info-val? e) (get e :cause) nil)))

;; Tagged-value predicates. The constructors (atom/volatile!/...) stay in Janet,
;; but every tagged value carries its kind under :jolt/type (records under
;; :jolt/deftype), reachable via get — which is nil on non-tables — so the
;; predicates are pure over get and move out of the seed.
(defn atom? [x]               (= (get x :jolt/type) :jolt/atom))
(defn volatile? [x]           (= (get x :jolt/type) :jolt/volatile))
(defn reader-conditional? [x] (= (get x :jolt/type) :jolt/reader-conditional))
(defn tagged-literal? [x]     (= (get x :jolt/type) :jolt/tagged-literal))
(defn record? [x]             (some? (get x :jolt/deftype)))
;; Jolt has no chunked seqs (Phase 5 territory), so this is always false.
(defn chunked-seq? [x] false)

;; Atom peripheral operations. atom/swap!/reset!/deref stay native — the compiler
;; depends on them and they're hot. swap-vals!/reset-vals!/compare-and-set! compose
;; the native ops (which already validate and notify watches); get-validator reads a
;; slot; add-watch/remove-watch/set-validator! mutate the atom (or its watches
;; sub-table) through the one host primitive jolt.host/ref-put! — the minimal
;; mutation kernel the overlay can't express over core fns (a nil value removes the
;; key). compare-and-set! compares by value, matching the prior Janet behavior.
(defn swap-vals! [a f & args]
  (let [old (deref a)] [old (apply swap! a f args)]))
(defn reset-vals! [a newval]
  (let [old (deref a)] (reset! a newval) [old newval]))
(defn compare-and-set! [a oldval newval]
  (if (= oldval (deref a)) (do (reset! a newval) true) false))
(defn get-validator [a] (get a :validator))
(defn add-watch [a key f]
  (jolt.host/ref-put! (get a :watches) key f) a)
(defn remove-watch [a key]
  (jolt.host/ref-put! (get a :watches) key nil) a)
(defn set-validator! [a f]
  (jolt.host/ref-put! a :validator f) nil)

;; Volatiles. The constructor (volatile!) stays native — it builds the mutable box —
;; but vreset! sets the box's slot through ref-put! and vswap! is pure over it + get.
(defn vreset! [vol newval]
  (jolt.host/ref-put! vol :val newval) newval)
(defn vswap! [vol f & args]
  (vreset! vol (apply f (get vol :val) args)))

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
(defn alength [arr] (count arr))
(defn aset [arr & idxs+val]
  (let [n (count idxs+val)
        val (nth idxs+val (dec n))
        target (reduce (fn [t k] (nth t k)) arr (take (- n 2) idxs+val))]
    (jolt.host/ref-put! target (nth idxs+val (- n 2)) val)
    val))
