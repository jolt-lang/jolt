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
;; No first-class Class objects either: class names are symbols the evaluator
;; handles in instance?/new positions, never values — so nothing is a class.
(defn class? [x] false)
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

;; Clojure 1.9: true for ANY argument incl. nil (used as a spec predicate).
(defn any? [x] true)

;; printf: print (no newline) the formatted string to *out*.
(defn printf [fmt & args] (print (apply format fmt args)))

;; bound?: every var has a root value. (jolt vars store the root in :root;
;; a nil-valued root reads as unbound — documented divergence.)
(defn bound? [& vars]
  (every? (fn [v] (some? (get v :root))) vars))

;; Run f with a frame of dynamic bindings installed; restore on exit.
(defn with-bindings* [binding-map f & args]
  (push-thread-bindings binding-map)
  (try
    (apply f args)
    (finally (pop-thread-bindings))))

;; Capture the CURRENT thread bindings; the returned fn re-installs them
;; around every call (binding conveyance — Clojure's bound-fn*).
(defn bound-fn* [f]
  (let [bs (get-thread-bindings)]
    (fn [& args] (apply with-bindings* bs f args))))

(defn thread-bound? [& vars]
  (every? (fn [v] (__thread-bound? v)) vars))

;; file-seq: the tree of paths under root (root included), directories walked
;; via the host dir primitives. Paths (strings), not File objects.
(defn file-seq [root]
  (tree-seq __dir? __list-dir root))

;; --- Ad-hoc hierarchies (stage 3) — Clojure's canonical pure-map port. -----
;; A hierarchy is {:parents {tag #{parents}} :ancestors {tag #{all}} 
;; :descendants {tag #{all}}}. The 3-arity forms are PURE; the 1/2-arity forms
;; operate on the private global hierarchy atom. Multimethod dispatch
;; (evaluator defmulti-setup) calls isa? through the interned var.

(defn make-hierarchy []
  {:parents {} :descendants {} :ancestors {}})

(def ^:private global-hierarchy (atom (make-hierarchy)))

(defn isa?
  ([child parent] (isa? (deref global-hierarchy) child parent))
  ([h child parent]
   (or (= child parent)
       (contains? (get (get h :ancestors) child #{}) parent)
       (and (vector? parent) (vector? child)
            (= (count parent) (count child))
            (loop [ret true i 0]
              (if (or (not ret) (= i (count parent)))
                ret
                (recur (isa? h (nth child i) (nth parent i)) (inc i))))))))

(defn parents
  ([tag] (parents (deref global-hierarchy) tag))
  ([h tag] (not-empty (get (get h :parents) tag))))

(defn ancestors
  ([tag] (ancestors (deref global-hierarchy) tag))
  ([h tag] (not-empty (get (get h :ancestors) tag))))

(defn descendants
  ([tag] (descendants (deref global-hierarchy) tag))
  ([h tag] (not-empty (get (get h :descendants) tag))))

(defn derive
  ([tag parent] (swap! global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (let [tp (get h :parents)
         td (get h :descendants)
         ta (get h :ancestors)
         tf (fn [m source sources target targets]
              (reduce (fn [ret k]
                        (assoc ret k
                               (reduce conj (get targets k #{})
                                       (cons target (get targets target)))))
                      m (cons source (get sources source))))]
     (or
      (when-not (contains? (get tp tag #{}) parent)
        (when (contains? (get ta tag #{}) parent)
          (throw (str tag " already has " parent " as ancestor")))
        (when (contains? (get ta parent #{}) tag)
          (throw (str "Cyclic derivation: " parent " has " tag " as ancestor")))
        {:parents (assoc tp tag (conj (get tp tag #{}) parent))
         :ancestors (tf ta tag td parent ta)
         :descendants (tf td parent ta tag td)})
      h))))

(defn underive
  ([tag parent] (swap! global-hierarchy underive tag parent) nil)
  ([h tag parent]
   (let [parent-map (get h :parents)
         childs-parents (if (get parent-map tag)
                          (disj (get parent-map tag) parent)
                          #{})
         new-parents (if (not-empty childs-parents)
                       (assoc parent-map tag childs-parents)
                       (dissoc parent-map tag))
         deriv-seq (mapcat (fn [e] (cons (key e) (interpose (key e) (val e))))
                           (seq new-parents))]
     (if (contains? (get parent-map tag #{}) parent)
       (reduce (fn [p [t pr]] (derive p t pr))
               (make-hierarchy) (partition 2 deriv-seq))
       h))))

;; --- Stage 3 tier shrink: pure-over-core leaves moved off the Janet seed ----

;; Representation predicates over the overlay's own predicates (no Janet reps).
(defn sequential? [x] (or (vector? x) (seq? x)))
(defn associative? [x] (or (map? x) (vector? x)))
(defn counted? [x]
  (or (vector? x) (map? x) (set? x) (list? x) (string? x)))
(defn indexed? [x] (vector? x))
(defn reversible? [x] (or (vector? x) (sorted? x)))
(defn seqable? [x]
  (or (nil? x) (coll? x) (string? x)))

(defn boolean? [x] (or (true? x) (false? x)))
(defn double? [x] (and (number? x) (not (integer? x))))
(defn float? [x] (double? x))
(defn infinite? [x] (and (number? x) (or (= x ##Inf) (= x ##-Inf))))

(defn qualified-keyword? [x] (and (keyword? x) (some? (namespace x))))
(defn simple-keyword? [x] (and (keyword? x) (nil? (namespace x))))
(defn qualified-symbol? [x] (and (symbol? x) (some? (namespace x))))
(defn simple-symbol? [x] (and (symbol? x) (nil? (namespace x))))

;; find: the map entry [k v] when k is present (nil values included), nil
;; otherwise. contains? gives vectors-by-index for free, matching Clojure.
(defn find [m k]
  (when (contains? m k) [k (get m k)]))

;; realized?: defined on the pending types only (delay/lazy-seq/future read
;; their realization slot; promises/atoms always-realized), error otherwise.
(defn realized? [x]
  (cond
    (delay? x) (boolean (get x :realized))
    (future? x) (boolean (get x :cached))
    (= :jolt/lazy-seq (get x :jolt/type)) (boolean (get x :realized))
    (atom? x) true
    :else (throw (str "realized? not supported on: " x))))

(defn force [x] (if (delay? x) (deref x) x))

;; pop: vectors drop the last element, lists/seqs the first; empty pops throw.
(defn pop [coll]
  (cond
    (nil? coll) nil
    (vector? coll)
      (if (zero? (count coll)) (throw "Can't pop empty vector")
        (subvec coll 0 (dec (count coll))))
    (seq? coll)
      (if (nil? (seq coll)) (throw "Can't pop empty list")
        (rest coll))
    :else (throw (str "pop not supported on: " coll))))

;; doall/dorun: realization boundaries. dorun walks (optionally at most n
;; steps — the Janet seed version ignored n); doall walks then returns coll.
(defn dorun
  ([coll]
   (loop [s (seq coll)]
     (when s (recur (next s)))))
  ([n coll]
   (loop [n n s (seq coll)]
     (when (and s (pos? n)) (recur (dec n) (next s))))))

(defn doall
  ([coll] (dorun coll) coll)
  ([n coll] (dorun n coll) coll))

;; list*: cons the leading args onto the final seq argument.
(defn list*
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
   (cons a (cons b (cons c (cons d (spread more)))))))

;; spread: (spread [1 2 [3 4]]) => (1 2 3 4) — list*'s variadic helper
;; (private in Clojure; defined after use is fine, vars resolve at call time).
(defn- spread [arglist]
  (cond
    (nil? arglist) nil
    (nil? (next arglist)) (seq (first arglist))
    :else (cons (first arglist) (spread (next arglist)))))

;; print-str family: print/println/prn into a captured *out*.
(defn print-str [& xs] (__with-out-str (fn* [] (apply print xs))))
(defn println-str [& xs] (__with-out-str (fn* [] (apply println xs))))
(defn prn-str [& xs] (__with-out-str (fn* [] (apply prn xs))))

;; --- Phase 2 leaf batch 4 (jolt-ded): over the rand / sort host seams --------

;; Canonical truncation toward zero via int (the kernel fn floored, which is
;; wrong for a negative n).
(defn rand-int [n] (int (rand n)))

;; Pure-functional Fisher-Yates over vector assoc; returns a vector, as in
;; Clojure. Collections only — a string is seqable but not shuffleable, as on
;; the JVM (Collections/shuffle wants a Collection).
(defn shuffle [coll]
  (when-not (coll? coll)
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
   (sort (fn [x y] (comp (keyfn x) (keyfn y))) coll)))

;; Version-4 UUID (RFC 4122): zero-padded hex groups 8-4-4-4-12, version
;; nibble 4, variant 8-b — built over rand-int and validated by parse-uuid.
(defn random-uuid []
  (let [hx4 (fn [] (format "%04x" (rand-int 0x10000)))
        hx3 (fn [] (format "%03x" (rand-int 0x1000)))]
    (parse-uuid (str (hx4) (hx4) "-" (hx4) "-4" (hx3)
                     "-" (format "%x" (+ 8 (rand-int 4))) (hx3)
                     "-" (hx4) (hx4) (hx4)))))

;; The char escape/name tables, as char-keyed maps (Clojure's shape).
(def ^:private char-escape-strings
  {\newline "\\n" \tab "\\t" \return "\\r" \formfeed "\\f"
   \backspace "\\b" \" "\\\"" \\ "\\\\"})
(defn char-escape-string [c] (get char-escape-strings c))

(def ^:private char-name-strings
  {\newline "newline" \tab "tab" \return "return" \formfeed "formfeed"
   \backspace "backspace" \space "space"})
(defn char-name-string [c] (get char-name-strings c))

;; Random selection over the host rand primitives.
(defn rand-nth [coll]
  (let [v (vec coll)] (nth v (rand-int (count v)))))

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
(defn uuid? [x]               (= (get x :jolt/type) :jolt/uuid))
(defn inst? [x]               (= (get x :jolt/type) :jolt/inst))

;; inst-ms: epoch milliseconds of an instant; throws on a non-inst (Clojure
;; protocol behavior).
(defn inst-ms [x]
  (if (inst? x) (get x :ms) (throw (str "inst-ms requires an inst, got: " x))))

;; Clojure 1.11 map transformers. PHM base so transformed keys canonicalize
;; (collisions: last entry in seq order wins, matching the reference).
(defn update-keys [m f]
  (reduce-kv (fn [acc k v] (assoc acc (f k) v)) (hash-map) m))

(defn update-vals [m f]
  (reduce-kv (fn [acc k v] (assoc acc k (f v))) (hash-map) m))

;; Vector-returning partition variants (1.11): lazy seqs OF vectors.
(defn partitionv
  ([n coll] (map vec (partition n coll)))
  ([n step coll] (map vec (partition n step coll)))
  ([n step pad coll] (map vec (partition n step pad coll))))

(defn partitionv-all
  ([n coll] (map vec (partition-all n coll)))
  ([n step coll] (map vec (partition-all n step coll))))

;; First part a vector, rest a seq — matching the reference implementation.
(defn splitv-at [n coll]
  [(vec (take n coll)) (drop n coll)])

;; with-redefs-fn: temporarily set each var's root to the mapped value, run
;; the thunk, restore the saved roots even on throw. The with-redefs macro
;; (30-macros) builds the {var val} map from names.
(defn with-redefs-fn [binding-map func]
  (let [vars (vec (keys binding-map))
        saved (mapv var-get vars)]
    (doseq [v vars] (var-set v (get binding-map v)))
    (try
      (func)
      (finally
        ;; loop/recur, not dotimes: dotimes is a 30-macros macro and this tier
        ;; compiles before it exists (a forward ref would resolve to the macro
        ;; fn at runtime and mis-apply it).
        (loop [i 0]
          (when (< i (count vars))
            (var-set (nth vars i) (nth saved i))
            (recur (inc i))))))))
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

;; --- Phase 2 leaf batch (jolt-ded): fn combinators + host-free stubs ---------

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f, has the
  same effects, if any, and returns the opposite truth value."
  [f]
  (fn [& args] (not (apply f args))))

;; Canonical Clojure fnil: patches only the FIRST 1-3 arguments (the old Janet
;; kernel patched every position it had a default for, which Clojure does not).
(defn fnil
  ([f x]
   (fn [a & args] (apply f (if (nil? a) x a) args)))
  ([f x y]
   (fn [a b & args] (apply f (if (nil? a) x a) (if (nil? b) y b) args)))
  ([f x y z]
   (fn [a b c & args]
     (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) args))))

(defn clojure-version [] "1.11.0-jolt")

;; Jolt numbers are doubles; no BigDecimal, no ratios.
(defn bigdec [x] (* 1.0 x))
(defn numerator [x] (throw (ex-info "numerator requires a ratio (Jolt has no ratios)" {})))
(defn denominator [x] (throw (ex-info "denominator requires a ratio (Jolt has no ratios)" {})))

;; No class hierarchy on the Janet host.
(defn supers [x] #{})

;; The kernel's munge only rewrote dashes; kept as-is for parity.
(defn munge [s] (str-replace-all "-" "_" (str s)))

(defn test
  "Calls the :test fn from v's metadata; :ok if it runs, :no-test if absent."
  [v]
  (let [t (:test (meta v))]
    (if t (do (t) :ok) :no-test)))

;; --- Phase 2 leaf batch 2 (jolt-ded): canonical Clojure ports ----------------
;; key/val/find first — merge-with and memoize below use them.

;; Strict, as in Clojure: an entry is what (seq m) yields (a host tuple), NOT
;; a plain vector — (key [1 2]) throws.
(defn key [e] (if (map-entry? e) (nth e 0) (throw (ex-info "key requires a map entry" {}))))
(defn val [e] (if (map-entry? e) (nth e 1) (throw (ex-info "val requires a map entry" {}))))

;; find was previously missing from jolt entirely. Presence (contains?), not
;; value, decides — so (find {:a nil} :a) is [:a nil]. Works on vectors by
;; index. The result must be a REAL entry (key/val are strict), so it is
;; minted as the first entry of a one-entry map — nil values survive (the
;; map builder switches to a phm when nil is involved).
(defn find [m k]
  (when (contains? m k) (first {k (get m k)})))

(defn some? [x] (not (nil? x)))
(defn true? [x] (= true x))
(defn false? [x] (= false x))

;; Presence-preserving: a key with a nil value is kept ((hash-map) base keeps
;; nil values and canonicalizes collection keys).
(defn select-keys [map keyseq]
  (reduce (fn [m k] (if (contains? map k) (assoc m k (get map k)) m))
          (hash-map) keyseq))

(defn zipmap [keys vals]
  (loop [m (hash-map) ks (seq keys) vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs)) (next ks) (next vs))
      m)))

;; conj semantics per entry arg (a map merges, a [k v] pair adds); nil args are
;; no-ops; all-nil (or no args) is nil.
(defn merge [& maps]
  (when (some identity maps)
    (reduce (fn [acc m] (if (nil? m) acc (conj (or acc (hash-map)) m)))
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
                   (reduce merge-entry (or m1 (hash-map)) (seq m2)))]
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

;; find-based, so nil RESULTS are cached too (the old kernel fn re-computed
;; them); args canonicalize as a collection key.
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

;; --- Phase 2 leaf batch 3 (jolt-ded) -----------------------------------------

;; An empty coll of the same category; sorted colls keep their comparator (the
;; value's own :empty op). Strings and scalars are nil, as in Clojure; a lazy
;; seq empties to () (the old kernel fn returned a host table for it).
(defn empty [coll]
  (cond
    (nil? coll) nil
    (sorted? coll) ((get (jolt.host/ref-get coll :ops) :empty) coll)
    (map? coll) {}
    (set? coll) #{}
    (vector? coll) []
    (coll? coll) ()
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

;; --- jolt-brh: the last missing-portable vars --------------------------------

;; jolt keywords have no intern table (any keyword "exists"), so find-keyword
;; always finds — babashka makes the same call.
(defn find-keyword
  ([nm] (keyword nm))
  ([ns nm] (keyword ns nm)))

;; The raw Inst protocol method; jolt insts have one representation, so it is
;; inst-ms itself.
(defn inst-ms* [i] (inst-ms i))

;; Canonical comp — here rather than the seed so each stage is invoked with
;; jolt call semantics: (comp seq :content) works because the keyword stage
;; goes through IFn dispatch (raw Janet keyword application does not).
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

;; Canonical IFn set (jolt-1vx): fns, keywords, symbols, maps (sorted incl.),
;; sets, vectors, and vars — NOT lists ((ifn? '(1 2)) is false in Clojure).
;; Mutable-mode caveat: vectors and lists share the array representation
;; there, so vector? can't separate them and lists read as ifn?.
(defn ifn? [x]
  (or (fn? x) (keyword? x) (symbol? x) (map? x) (set? x) (vector? x) (var? x)))

;; Auto-promoting (') and unchecked arithmetic. Jolt numbers don't overflow,
;; so all of these are the checked ops; fixed arities mirror Clojure's
;; signatures. unchecked-divide-int goes through quot, so dividing by zero
;; throws as on the JVM (the old seed fn silently truncated infinity).
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
