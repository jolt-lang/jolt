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
    :else (throw (jolt.host/throwable "java.lang.ClassCastException"
                                       (str "peek not supported on: " coll)))))

(defn subvec
  ([v start] (subvec v start (count v)))
  ([v start end]
   (when (not (vector? v))
     (throw (jolt.host/throwable "java.lang.ClassCastException"
                                  (str "subvec requires a vector: " v))))
   ;; Clojure coerces indices with (int ...): NaN -> 0, floats/ratios truncate
   ;; toward zero; non-numbers throw. Only then range-check.
   (let [coerce (fn [x]
                  (cond
                    (not (number? x))
                      (throw (jolt.host/throwable "java.lang.IllegalArgumentException"
                                                   (str "subvec index must be a number: " x)))
                    (not= x x) 0
                    :else (long x)))
         s (coerce start)
         e (coerce end)]
     (when (or (< s 0) (< e s) (< (count v) e))
       (throw (jolt.host/throwable "java.lang.IndexOutOfBoundsException"
                                    (str "subvec index out of range: " s " " e))))
     ;; O(log n) structural slice, stamped as the JVM's SubVector class for a
     ;; non-empty range (as-subvec — a fresh nil-meta view, so the stamp also
     ;; sheds any metadata a full-range identity return would carry; an empty
     ;; range is RT.subvec's PersistentVector.EMPTY and stays plain).
     (let [r (jolt.host/as-subvec (jolt.host/slice v s e))]
       (if (and (identical? r v) (meta v)) (with-meta r nil) r)))))

;; Deliberately NOT Clojure's transient fold — (persistent! (reduce (fn [v o]
;; (conj! v (f o))) (transient []) coll)). That is faster on the JVM and slower
;; here: measured 5053ns against 2298ns for this body over 32 elements, in one
;; binary. The same fold written INLINE (with inc in place of f) runs 1499ns, so
;; the cost is not the fold — it appears only when the element fn arrives as a
;; parameter, and a bare call through a parameter measures just 20.7ns against
;; 14.6ns direct, which does not account for the gap. Unexplained; the numbers
;; are reproducible and that is why this stays. Worth re-deriving before anyone
;; "fixes" this to match Clojure.
(defn mapv [f & colls] (vec (apply map f colls)))

(defn update [m k f & args] (assoc m k (apply f (get m k) args)))

;; set: build through a transient, like clojure.core. The compiler uses it off
;; the emit path (backend bare-native-names, type inference), so unlike boolean it
;; can live here — compiling this tier never calls set, and by the time those
;; callers run the tier is bound.
;;
;; An existing set is handed back rather than rebuilt: PersistentHashSet.withMeta
;; returns `this` when the metadata is unchanged, so on the JVM (set s) on a
;; meta-less set is the set itself. jolt's with-meta always makes a fresh value
;; (a separate divergence), hence the explicit nil-meta arm rather than a plain
;; (with-meta coll nil) — without it this was a full O(n) rebuild, 136ms against
;; the reference's 0ms over 200k.
(defn set [coll]
  (cond
    (nil? coll) #{}
    (set? coll) (if (nil? (meta coll)) coll (with-meta coll nil))
    :else (persistent! (reduce conj! (transient #{}) coll))))
