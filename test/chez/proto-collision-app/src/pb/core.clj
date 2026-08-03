;; Deliberately the SAME protocol name as pa.core in a different namespace — the
;; shape malli hits when malli.generator-ast evals a renamed copy of
;; malli.generator, so both define a protocol named Generator extended to Object.
(ns pb.core)

(defprotocol Greet (greet [this]))

(extend-protocol Greet
  Object
  (greet [_] :from-B))

;; The same collision seen through instance?: a RECORD here named like pa.core's
;; PROTOCOL. spec-tools has (defrecord Spec …) while clojure.spec.alpha has
;; (defprotocol Spec …), and its own spec? — (instance? Spec x) — answered true
;; for every spec.alpha reify.
(defrecord Greeter [n])
