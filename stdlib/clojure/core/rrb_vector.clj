;; clojure.core.rrb-vector — a thin mapping onto jolt's native RRB pvec.
;;
;; The host (host-table.ss) binds jolt.host/catvec and jolt.host/slice to the
;; O(log n) pvec-catvec / pvec-slice in collections.ss; this namespace exposes
;; them with the core.rrb-vector API (catvec, subvec, vector, vector-of, vec).
;; jolt's pvec carries relaxedness in its nodes rather than as a distinct type,
;; so every result here is an ordinary jolt vector and interops with the rest of
;; clojure.core without conversion.
;;
;; DIVERGENCE from the JVM core.rrb-vector: vector-of. The JVM library packs
;; primitives into unboxed typed arrays (a genuine gvec). jolt has no per-type
;; unboxed vector, so vector-of coerces each element where a cast exists
;; (long/double) and stores a plain vector of the coerced values. The element
;; type is NOT retained — the result answers :object behavior throughout — so a
;; :long vector-of holds boxed longs, not packed longs. catvec/subvec over one
;; stay correct (they are ordinary vectors); only storage density differs.
;;
;; subvec here is the RRB slice (jolt.host/slice), NOT clojure.core/subvec
;; (which copies). Both agree on values for valid ranges.
;; catvec/subvec delegate to the O(log n) host pvec ops. They are called
;; fully-qualified (jolt.host/catvec, not :as host) because jolt's compiler
;; resolves a Prefix/member qualified symbol only against actual namespaces —
;; an :as alias is treated as a class and fails with "Unknown class". This
;; matches how the rest of jolt-core references jolt.host.
(ns clojure.core.rrb-vector
  (:refer-clojure :exclude [vector vector-of vec subvec]))

(defn catvec
  "Concatenates the given vectors in logarithmic time."
  ([] [])
  ([v1] v1)
  ([v1 v2] (jolt.host/catvec v1 v2))
  ([v1 v2 v3] (catvec (jolt.host/catvec v1 v2) v3))
  ([v1 v2 v3 v4] (catvec (jolt.host/catvec v1 v2) (jolt.host/catvec v3 v4)))
  ([v1 v2 v3 v4 & vn]
   (apply catvec (jolt.host/catvec (jolt.host/catvec v1 v2) (jolt.host/catvec v3 v4)) vn)))

(defn subvec
  "Returns a new vector containing the elements of v from start (inclusive)
  to end (exclusive) in logarithmic time. end defaults to the end of v. The
  result shares structure with v and does not retain elements outside the
  range."
  ([v start] (jolt.host/slice v start (count v)))
  ([v start end] (jolt.host/slice v start end)))

(defn vector
  "Creates a new vector containing the args."
  ([] [])
  ([x1] [x1])
  ([x1 x2] [x1 x2])
  ([x1 x2 x3] [x1 x2 x3])
  ([x1 x2 x3 x4] [x1 x2 x3 x4])
  ([x1 x2 x3 x4 & xn] (reduce conj [x1 x2 x3 x4] xn)))

(def ^:private coerce-for
  {;; jolt casts where it can; the rest keep :object storage.
   :long long :double double
   ;; :int/:short/:byte narrow to long on jolt (no fixed-width int vector).
   :int long :short long :byte long
   ;; :float widens to double (jolt has no 32-bit float vector).
   :float double
   ;; :char/:boolean/:object store as-is.
   :char identity :boolean identity :object identity})

(defn vector-of
  "Creates a vector of homogeneous primitive type t, one of :object :int :long
  :float :double :byte :short :char :boolean. Optionally takes elements.

  Divergence: jolt has no unboxed typed vector, so elements are coerced where a
  cast exists and stored in a plain (object) vector. See the namespace docstring."
  ([t] [])
  ([t x1] (clojure.core/vec (map (coerce-for t) [x1])))
  ([t x1 x2] (clojure.core/vec (map (coerce-for t) [x1 x2])))
  ([t x1 x2 x3] (clojure.core/vec (map (coerce-for t) [x1 x2 x3])))
  ([t x1 x2 x3 x4] (clojure.core/vec (map (coerce-for t) [x1 x2 x3 x4])))
  ([t x1 x2 x3 x4 & xn]
   (clojure.core/vec (map (coerce-for t) (list* x1 x2 x3 x4 xn)))))

(defn vec
  "Returns a vector containing the contents of coll.
  If coll is a vector, returns it (its tree already carries any relaxedness).
  Otherwise realizes coll into a vector."
  [coll]
  (if (vector? coll) coll (clojure.core/vec coll)))
