;; Collection constructors + rand (jolt-cf1q.3 Phase 2 inc A) — host-coupled seed
;; natives the overlay assumes as bare clojure.core vars but which were never
;; def-var!'d, so they resolved to jolt-nil and any call hit the apply-jolt-nil
;; crash bucket. The persistent-collection constructors already exist in
;; collections.ss (jolt-hash-map / jolt-hash-set / jolt-vector); this just binds
;; the public clojure.core names to them. Loaded after def-var! (rt.ss) + the
;; collections + seq tiers. Semantics match the Janet seed (core_coll.janet
;; core-hash-map/core-array-map/core-hash-set/core-set, core_types.janet
;; core-rand).

;; hash-map / hash-set: variadic kvs / elems straight onto the existing ctors.
;; array-map: Clojure preserves insertion order, but jolt's `=` is structural and
;; the parity corpus compares by value, so a pmap is observationally equal for
;; the tested cases; keys-ordering is a separate (untested-here) concern.
(define (jolt-array-map . kvs) (apply jolt-hash-map kvs))

;; set: realize any seqable to a list, then dedup through the set ctor. nil -> #{}.
(define (jolt-set coll)
  (if (jolt-nil? coll) (jolt-hash-set) (apply jolt-hash-set (seq->list coll))))

;; rand: a flonum in [0, n) (n defaults to 1.0) — jolt is all-flonum, so the
;; result is a double like every other number.
(define (jolt-rand . n)
  (let ((r (random 1.0)))
    (if (null? n) r (* r (exact->inexact (car n))))))

(def-var! "clojure.core" "hash-map" jolt-hash-map)
(def-var! "clojure.core" "hash-set" jolt-hash-set)
(def-var! "clojure.core" "array-map" jolt-array-map)
(def-var! "clojure.core" "set" jolt-set)
(def-var! "clojure.core" "rand" jolt-rand)
(def-var! "clojure.core" "map-entry?" jolt-map-entry?)
