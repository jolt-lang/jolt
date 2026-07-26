;; The reduce/datafy extension points core exposes as protocols. jolt implements
;; reduce natively over its own collections, so these exist for the libraries that
;; EXTEND them to their own types (and for the ones that merely :refer a name, as
;; instaparse does with IKVReduce) — not as the path core's own reduce takes.
;;
;; reduce and reduce-kv consult these before their native paths when the value's
;; type extends one, so a type that participates gets its own implementation.
(ns clojure.core.protocols)

(defprotocol CollReduce
  "Protocol for collection types that can implement reduce faster than the
  sequence walk. Called by clojure.core/reduce."
  (coll-reduce [coll f] [coll f val]))

(defprotocol InternalReduce
  "Protocol for concrete seq types that can reduce themselves faster than the
  first/next walk. Called by clojure.core/reduce."
  (internal-reduce [seq f start]))

(defprotocol IKVReduce
  "Protocol for key/value collections that can implement reduce-kv themselves.
  Called by clojure.core/reduce-kv."
  (kv-reduce [amap f init]))

(defprotocol Datafiable
  (datafy [o] "Return a representation of o as data (default identity)."))

(defprotocol Navigable
  (nav [coll k v] "Return v as it exists in coll, navigating if needed."))
