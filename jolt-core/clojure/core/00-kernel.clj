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
     ;; RT.subvec takes ints: Math.toIntExact refuses a long past that range
     (when (or (< s -2147483648) (> s 2147483647) (< e -2147483648) (> e 2147483647))
       (throw (jolt.host/throwable "java.lang.ArithmeticException" "integer overflow")))
     (when (or (< s 0) (< e s) (< (count v) e))
       (throw (jolt.host/throwable "java.lang.IndexOutOfBoundsException"
                                    (str "subvec index out of range: " s " " e))))
     ;; O(log n) structural slice, stamped as the JVM's SubVector class for a
     ;; non-empty range (as-subvec — a fresh nil-meta view, so the stamp also
     ;; sheds any metadata a full-range identity return would carry; an empty
     ;; range is RT.subvec's PersistentVector.EMPTY and stays plain).
     (let [r (jolt.host/as-subvec (jolt.host/slice v s e))]
       (if (and (identical? r v) (meta v)) (with-meta r nil) r)))))

;; Clojure's own definition: the single-collection arity is a transient fold.
;; This was (vec (apply map f colls)) for a long time because the fold measured
;; SLOWER here (5053 ns against 2298 over 32 elements) for no reason anyone
;; could name. Re-derived 2026-09-17: the reason was in conj! and make-pvec, not
;; the fold — conj! was variadic and consed a rest list per element, and
;; make-pvec built anything past 32 elements by conj, copying the tail each
;; time. With both fixed (transients.ss, collections.ss) the fold is 0.7 µs /
;; 0.7 KB against 1.6 µs / 6.2 KB over 32 elements, and 535 µs / 0.9 MB against
;; 1317 µs / 4.6 MB over 26k — the lazy map's cells and the seq->list copy are
;; what the old body paid for. The transient gate pins the per-element bytes.
(defn mapv
  ([f coll] (persistent! (reduce (fn [v o] (conj! v (f o))) (transient []) coll)))
  ([f c1 c2] (into [] (map f c1 c2)))
  ([f c1 c2 c3] (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls] (into [] (apply map f c1 c2 c3 colls))))

(defn update [m k f & args] (assoc m k (apply f (get m k) args)))

;; set: build through a transient, like clojure.core. The compiler uses it off
;; the emit path (backend bare-native-names, type inference), so unlike boolean it
;; can live here — compiling this tier never calls set, and by the time those
;; callers run the tier is bound.
;;
;; An existing set is handed back rather than rebuilt: with-meta returns the
;; value itself when the metadata is unchanged, so (set s) on a meta-less set is
;; s. The corpus row "an existing set is returned, not rebuilt" gates that — if
;; with-meta ever starts allocating unconditionally again, this silently becomes
;; a full O(n) rebuild (136ms against 0ms over 200k) and that row is what says so.
(defn set [coll]
  (cond
    (nil? coll) #{}
    (set? coll) (with-meta coll nil)
    :else (persistent! (reduce conj! (transient #{}) coll))))
