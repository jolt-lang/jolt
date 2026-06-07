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
