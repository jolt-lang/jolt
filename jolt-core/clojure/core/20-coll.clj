;; clojure.core — collection tier. Pure, eager fns expressed as compositions of
;; already-frozen core primitives (reduce/assoc/get/conj/filter/vec/count/>=).
;; No host internals, no laziness, no macros — so they compile cleanly and stay
;; redefinable. Loaded after the seq tier; self-hosted in compile mode.
;;
;; Same migration rule as the seq tier (see 10-seq.clj): not in core-renames, no
;; internal callers, not used by the self-hosted compiler.

;; Tiny leaves first — fns below in this tier (and 25-sorted) use them.
(defn some? [x] (not (nil? x)))

(defn identity [x] x)

(defn constantly [x] (fn [& args] x))

;; neg? throws on non-numbers via <, as Clojure's Numbers.isNeg does.
(defn neg? [x] (< x 0))

;; even?/odd? stay host primitives: (filter even? ...) is idiomatic-hot and the
;; overlay versions cost an extra call layer per element (seq-pipe bench 4x).

;; Variadic bit ops — canonical Clojure arities folding the binary host op
;; (__bit-* seams). 2-arg call sites still compile to the native op via
;; the backend's native-ops table, so the binary fast path is unchanged.
(defn bit-and
  ([x y] (__bit-and x y))
  ([x y & more] (reduce __bit-and (__bit-and x y) more)))

(defn bit-or
  ([x y] (__bit-or x y))
  ([x y & more] (reduce __bit-or (__bit-or x y) more)))

(defn bit-xor
  ([x y] (__bit-xor x y))
  ([x y & more] (reduce __bit-xor (__bit-xor x y) more)))

(defn bit-and-not
  ([x y] (__bit-and-not x y))
  ([x y & more] (reduce __bit-and-not (__bit-and-not x y) more)))

;; The printing family, over two host seams: __write (push a string to *out*)
;; and __pr-str1 (render ONE value readably). The renderer itself stays host —
;; it's representation-coupled (pvec/phm/phs/sorted internals) and shared with
;; the hot str. print uses str semantics (unreadable), pr/pr-str readable;
;; println/prn append the newline. Defined this early because printf and the
;; print-str family below call them. (print-method as a real multimethod is a
;; separate project.)
(defn pr-str [& xs]
  (loop [out "" s (seq xs) first? true]
    (if s
      (recur (str out (if first? "" " ") (__pr-str1 (first s))) (next s) false)
      out)))

(defn pr [& xs] (__write (apply pr-str xs)) nil)

(defn prn [& xs] (apply pr xs) (__write "\n") nil)

;; print renders each arg non-readably (strings/chars unquoted) like str — except
;; nil, which prints as "nil" (str yields ""). Only the top-level arg needs the
;; guard; nil nested in a collection already renders as "nil" via the collection
;; printer.
(defn print [& xs]
  (__write (loop [out "" s (seq xs) first? true]
             (if s
               (let [x (first s)
                     r (if (nil? x) "nil" (str x))]
                 (recur (str out (if first? "" " ") r) (next s) false))
               out)))
  nil)

(defn println [& xs] (apply print xs) (__write "\n") nil)

;; Transient accumulation (canonical JVM form): assoc! into a native-backed
;; scratch table per element, then persistent! bulk-builds the HAMT once —
;; instead of a fresh persistent assoc (full trie-path rebuild) per element.
;; A transient map canonicalizes collection keys (it is canon-keyed, like a
;; PHM), so counting/grouping by collection value still works across map reps.
(defn frequencies [coll]
  (persistent!
    (reduce (fn [counts x] (assoc! counts x (inc (get counts x 0)))) (transient {}) coll)))

;; Buckets are transient vectors, not persistent ones: the JVM form rebuilds the
;; bucket's persistent vector per element (conj (get ret k []) x), an O(log n)
;; trie path-rebuild + alloc per element — so a coarse grouping (few large
;; buckets) is bound on that conj, not the map build. Push onto a per-bucket
;; native array (O(1)) instead, then bulk-build the persistent map ONCE.
;; Distinct keys are recorded in a side vector so the buckets can be frozen in
;; place (no second map rebuild). A bucket's FIRST element is stored as a cheap
;; persistent [x]; only the second element promotes it to a transient — so an
;; all-singletons grouping pays no transient alloc, while any bucket that
;; actually grows rides the O(1) push.
(defn group-by [f coll]
  (let [tm (transient {})
        ks (reduce (fn [ks x]
                     (let [k (f x)
                           b (get tm k)]
                       (if (nil? b)
                         (do (assoc! tm k [x]) (conj! ks k))
                         (if (vector? b)
                           (do (assoc! tm k (conj! (transient b) x)) ks)
                           (do (conj! b x) ks)))))
                   (transient []) coll)]
    (reduce (fn [_ k]
              (let [b (get tm k)]
                (if (vector? b) nil (assoc! tm k (persistent! b)))))
            nil (persistent! ks))
    (persistent! tm)))

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

(defn qualified-keyword? [x] (and (keyword? x) (some? (namespace x))))
(defn simple-keyword? [x] (and (keyword? x) (nil? (namespace x))))
(defn qualified-symbol? [x] (and (symbol? x) (some? (namespace x))))
(defn simple-symbol? [x] (and (symbol? x) (nil? (namespace x))))

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

;; Returns a seq (JVM does), nil when n<=0 or coll is empty.
(defn take-last [n coll]
  (let [c (vec coll) len (count c)]
    (when (pos? len) (seq (subvec c (max 0 (- len n)))))))

;; The JVM definition: a lazy seq (() when empty), not a vector.
(defn drop-last
  ([coll] (drop-last 1 coll))
  ([n coll] (map (fn [x _] x) coll (drop n coll))))

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

(defn bounded-count [n coll]
  (if (counted? coll)
    (count coll)
    (loop [i 0 s (seq coll)]
      (if (and s (< i n)) (recur (inc i) (next s)) i))))

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

(defn key [e] (if (map-entry? e) (nth e 0) (throw (ex-info "key requires a map entry" {}))))
(defn val [e] (if (map-entry? e) (nth e 1) (throw (ex-info "val requires a map entry" {}))))

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
  ([h tag]
   ;; the user hierarchy plus any modeled JVM ancestry (jolt.host/class-ancestors)
   ;; so (ancestors (class x)) answers like the JVM for the common interfaces.
   (let [hier (get (get h :ancestors) tag)
         host (jolt.host/class-ancestors tag)]
     (not-empty (if host (into (or hier #{}) host) hier)))))

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

;; --- pure-over-core leaves expressed off the host primitives -----------------

;; Representation predicates over the overlay's own predicates.
(defn sequential? [x] (or (vector? x) (seq? x)))
(defn associative? [x] (or (map? x) (vector? x)))
(defn counted? [x]
  (or (vector? x) (map? x) (set? x) (list? x) (string? x)))
(defn indexed? [x] (vector? x))
;; sorted? is defined by the next tier (25-sorted) — declared here so this
;; tier compiles (forward references are analysis errors).
(declare sorted?)

(defn reversible? [x] (or (vector? x) (sorted? x)))
(defn seqable? [x]
  (or (nil? x) (coll? x) (string? x)))

(defn boolean? [x] (or (true? x) (false? x)))
(defn double? [x] (and (number? x) (not (integer? x))))
(defn float? [x] (double? x))
(defn infinite? [x] (and (number? x) (or (= x ##Inf) (= x ##-Inf))))

;; qualified-/simple- keyword?/symbol? moved above qualified-ident? (forward
;; references are analysis errors).


;; realized?: defined on the pending types only (delay/lazy-seq/future read
;; Tagged-value predicates. The constructors (atom/volatile!/...) are host
;; primitives, but every tagged value carries its kind under :jolt/type (records
;; under :jolt/deftype), reachable via get — which is nil on non-tables — so the
;; predicates are pure over get.
(defn atom? [x]               (= (get x :jolt/type) :jolt/atom))
(defn volatile? [x]           (= (get x :jolt/type) :jolt/volatile))
(defn reader-conditional? [x] (= (get x :jolt/type) :jolt/reader-conditional))
(defn tagged-literal? [x]     (= (get x :jolt/type) :jolt/tagged-literal))
(defn record? [x]             (some? (get x :jolt/deftype)))
(defn uuid? [x]               (= (get x :jolt/type) :jolt/uuid))
(defn inst? [x]               (= (get x :jolt/type) :jolt/inst))
(defn char? [x]               (= (get x :jolt/type) :jolt/char))

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
;; steps); doall walks then returns coll.
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

;; spread: (spread [1 2 [3 4]]) => (1 2 3 4) — list*'s variadic helper
;; (private in Clojure).
(defn- spread [arglist]
  (cond
    (nil? arglist) nil
    (nil? (next arglist)) (seq (first arglist))
    :else (cons (first arglist) (spread (next arglist)))))

;; list*: cons the leading args onto the final seq argument.
(defn list*
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
   (cons a (cons b (cons c (cons d (spread more)))))))

;; print-str family: print/println/prn into a captured *out*.
(defn print-str [& xs] (__with-out-str (fn* [] (apply print xs))))
(defn println-str [& xs] (__with-out-str (fn* [] (apply println xs))))
(defn prn-str [& xs] (__with-out-str (fn* [] (apply prn xs))))

