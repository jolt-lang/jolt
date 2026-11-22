(ns jolt.backend-scheme.fused
  "Fused call lowering on Chez: explicit filter/map into empty vectors.
  Each emitter checks linkage and emits a runtime fallback when a linked root
  or runtime domain guard fails."
  (:require [clojure.string :as str]
            [jolt.host :refer [seed-callable?]]
            [jolt.passes.transducers :as transducers]
            [jolt.backend-scheme.session :as session
             :refer [direct-link? fresh-label node-line register-callsite!
                     seed-mint? static-callee target with-site]]
            [jolt.backend-scheme.hoist :refer [*const-pool* hoist-seed-root]]
            [jolt.backend-scheme.calls :refer [emit-call node-inline-chain plain-call]]))

(defn- emit-fused-site-call [node tail? proc args]
  (let [line (or (node-line node) 0)]
    (register-callsite! line (static-callee (:fn node)) tail?)
    (with-site node (emit-call tail? proc args line (node-inline-chain node)))))

(defn- emit-filter-map-walk [emit node operands compose tail?]
  (let [[to p filter-ok filter-xf m map-ok map-xf c fuse xf from]
        (repeatedly 11 #(fresh-label "_xf$"))
        [dest pred mapper source] (mapv emit operands)
        core "clojure.core"
        into-proc (hoist-seed-root core "into")
        filter-proc (hoist-seed-root core "filter")
        map-proc (hoist-seed-root core "map")
        [comp-node] (rest (:args node))
        [filter-node map-node] (:args comp-node)
        call emit-fused-site-call
        td-filter (plain-call "td-filter" [p])
        td-map (plain-call "td-map" [m])
        ;; The linked comp root is read after evaluating the stages. Build
        ;; nonstandard stages at their original position and call comp before
        ;; the source. Only pure standard constructors can be deferred.
        binds [[to dest]
               [p pred]
               [filter-ok (plain-call "eq?" [filter-proc "jolt-filter"])]
               [filter-xf (str "(and (not " filter-ok ") " (call filter-node false filter-proc [p]) ")")]
               [m mapper]
               [map-ok (plain-call "eq?" [map-proc "jolt-map"])]
               [map-xf (str "(and (not " map-ok ") " (call map-node false map-proc [m]) ")")]
               [c compose]
               [fuse (str "(and " filter-ok " " map-ok " (eq? " c " jolt-vector-filter-map-comp)"
                          " (eq? " into-proc " jolt-into))")]
               [xf (str "(and (not " fuse ") "
                        (call comp-node false "jolt-invoke2"
                              [c (str "(if " filter-ok " " td-filter " " filter-xf ")")
                               (str "(if " map-ok " " td-map " " map-xf ")")]) ")")]
               [from source]]
        ordinary-xf (str "(if " fuse " " (with-site comp-node (plain-call c [td-filter td-map])) " " xf ")")]
    (str "(let* (" (str/join " " (map (fn [[name value]] (str "(" name " " value ")")) binds)) ")"
         " (if (and " fuse " (pvec? " from ") (not (eq? (pvec-ent " from ") #t)))"
         " " (call node tail? "jolt-vector-filter-map" [to p m from])
         " " (call node tail? into-proc [to ordinary-xf from]) "))")))

(defn- chez-linked-fast-path? []
  (and *const-pool* (= :chez (target)) (direct-link?)
       (not (seed-mint?)) (not (session/debug-locals?))))

(defn- emit-vector-filter-map [emit node tail?]
  (when (and (:vector-filter-map node) (chez-linked-fast-path?))
    ;; Three native seed calls hoist their procedure at the enclosing def.
    ;; comp uses its linked Scheme binding and can change while evaluating
    ;; the stage arguments. Leave all other linkage to emit-var-invoke.
    (let [core "clojure.core"
          compose (seed-callable? nil core "comp" 2)]
      (when (and (string? compose)
                 (true? (seed-callable? nil core "into" 3))
                 (true? (seed-callable? nil core "filter" 1))
                 (true? (seed-callable? nil core "map" 1)))
        (when-let [operands (transducers/filter-map-operands node)]
          (emit-filter-map-walk emit node operands compose tail?))))))

(defn emit-fused-invoke [emit node tail?]
  (emit-vector-filter-map emit node tail?))
