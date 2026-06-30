;; clojure.core — kernel tier (stage just above the host primitives).
;;
;; These are the structural fns the self-hosted compiler itself uses
;; (jolt.analyzer): second/peek/subvec/mapv/update. Because the compiler must be
;; able to compile the *rest* of clojure.core, anything it calls has to exist
;; before it is built. So this tier is loaded FIRST and, in compile mode, is
;; bootstrap-compiled directly into clojure.core (not routed through the
;; self-hosted pipeline, which would need these to already exist — the
;; circularity that previously forced `second` to stay a host primitive). With this tier
;; in place the analyzer is built against the Clojure definitions.
;;
;; Constraint: depend only on core-renames primitives (first/next/nth/count/conj/
;; vec/map/apply/assoc/get/…, all hardwired to host primitives) and on each other.

(defn second [coll] (first (next coll)))

(defn peek [coll]
  (cond
    (nil? coll) nil
    ;; vectors (incl. jolt's eager seq results): last element; lists/seqs: first.
    (vector? coll) (if (zero? (count coll)) nil (nth coll (dec (count coll))))
    (seq? coll) (first coll)
    :else (throw (str "peek not supported on: " coll))))

(defn subvec
  ([v start] (subvec v start (count v)))
  ([v start end]
   (when (not (vector? v)) (throw (str "subvec requires a vector")))
   ;; Clojure coerces indices with (int ...): NaN -> 0, floats/ratios truncate
   ;; toward zero; non-numbers throw. Only then range-check.
   (let [coerce (fn [x]
                  (cond
                    (not (number? x)) (throw (str "subvec index must be a number"))
                    (not= x x) 0
                    :else (long x)))
         s (coerce start)
         e (coerce end)]
     (when (or (< s 0) (< e s) (< (count v) e))
       (throw (str "subvec index out of range: " s " " e)))
     (loop [i s acc []]
       (if (< i e) (recur (inc i) (conj acc (nth v i))) acc)))))

(defn mapv [f & colls] (vec (apply map f colls)))

(defn update [m k f & args] (assoc m k (apply f (get m k) args)))

;; set: realize a seqable and dedup through the set constructor; nil -> #{}. The
;; compiler uses it off the emit path (backend bare-native-names, type inference),
;; so unlike boolean it can live here — compiling this tier never calls set, and by
;; the time those callers run the tier is bound. Pure composition of hash-set/seq/
;; apply, so it lowers to the same code the native shim did.
(defn set [coll] (if (nil? coll) #{} (apply hash-set (seq coll))))
