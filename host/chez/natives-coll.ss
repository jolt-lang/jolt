;; Collection constructors + rand — host-coupled natives the overlay assumes as
;; bare clojure.core vars. The persistent-collection constructors already exist
;; in collections.ss (jolt-hash-map / jolt-hash-set / jolt-vector); this just
;; binds the public clojure.core names to them. Loaded after def-var! (rt.ss) +
;; the collections + seq tiers. hash-map/array-map/hash-set/set/rand semantics.

;; array-map: insertion-ordered, any size (Clojure's PersistentArrayMap, via
;; createAsIfByAssoc). hash-map: hash order (PersistentHashMap). The map LITERAL
;; ctor (jolt-hash-map, emitted for {...}) is array-ordered up to 8 entries and
;; hash beyond, matching RT.map.
(define (jolt-array-map . kvs) (jolt-array-map-build kvs))
(define (jolt-hash-map-fn . kvs) (jolt-hash-map-build kvs))

;; set: realize any seqable to a list, then dedup through the set ctor. nil -> #{}.
(define (jolt-set coll)
  (if (jolt-nil? coll) (jolt-hash-set) (apply jolt-hash-set (seq->list coll))))

;; rand: a flonum in [0, n) (n defaults to 1.0) — jolt is all-flonum, so the
;; result is a double like every other number.
(define (jolt-rand . n)
  (let ((r (random 1.0)))
    (if (null? n) r (* r (exact->inexact (car n))))))

(def-var! "clojure.core" "hash-map" jolt-hash-map-fn)
(def-var! "clojure.core" "hash-set" jolt-hash-set)
(def-var! "clojure.core" "array-map" jolt-array-map)
(def-var! "clojure.core" "set" jolt-set)
(def-var! "clojure.core" "rand" jolt-rand)
(def-var! "clojure.core" "map-entry?" jolt-map-entry?)
