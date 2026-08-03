;; Deliberately the SAME protocol and method name as pa.core, in a different
;; namespace — the shape malli hits when malli.generator-ast evals a renamed copy
;; of malli.generator, so both define a protocol named Generator extended to
;; Object.
(ns pb.core)

(defprotocol Greet (greet [this]))

(extend-protocol Greet
  Object
  (greet [_] :from-B))
