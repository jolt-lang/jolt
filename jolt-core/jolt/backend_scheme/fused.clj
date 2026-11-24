(ns jolt.backend-scheme.fused
  "Fused call lowering on Chez: explicit filter/map into empty vectors,
  guarded sibling assoc-in chains, and confined reduce-kv map builders.
  Each emitter checks linkage and emits a runtime fallback when a linked root
  or runtime domain guard fails."
  (:require [clojure.string :as str]
            [jolt.host :refer [seed-callable?]]
            [jolt.ir :as ir]
            [jolt.passes.facts :as facts]
            [jolt.passes.transducers :as transducers]
            [jolt.backend-scheme.session :as session
             :refer [cur direct-link? fresh-label
                     node-line register-callsite! seed-mint? static-callee target
                     with-site]]
            [jolt.backend-scheme.hoist :refer [*const-pool* hoist-seed-root]]
            [jolt.backend-scheme.calls :refer [emit-call node-inline-chain plain-call]]
            [jolt.backend-scheme.lambda :refer [*step-variant*]]))

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

(defn- assoc-in-call? [node]
  (let [f (:fn node)]
    (and (= :invoke (:op node)) (= :var (:op f))
         (= "clojure.core" (:ns f)) (= "assoc-in" (:name f))
         (= 3 (count (:args node))) (not (:clear-caps node)))))

(defn- stable-edit-operand? [node]
  (and (not (:clear-caps node)) (#{:local :const} (:op node))))

(defn- same-edit-key? [a b]
  (case (:op a)
    ;; Only walk directly nested calls and their literal vectors. There is no
    ;; binder between these occurrences, so a name identifies the same local.
    :local (and (= :local (:op b)) (= (:name a) (:name b)))
    :const (and (= :const (:op b)) (keyword? (:val a)) (= (:val a) (:val b)))
    false))

(defn- assoc-in-chain [node]
  (loop [call node edits []]
    (let [[m path value] (:args call)
          keys (:items path)]
      (if (and (assoc-in-call? call) (= :vector (:op path)) (not (:clear-caps path))
               (<= 2 (count keys) 5) (every? stable-edit-operand? keys)
               (stable-edit-operand? value))
        (when (< (count edits) 4)
          (recur m (conj edits [call keys value])))
        (when (and (<= 2 (count edits)) (stable-edit-operand? call))
          (let [paths (mapv second edits)
                path (first paths)
                shared (when (every? #(= (count path) (count %)) paths)
                         (count (take-while
                                  (fn [i] (every? #(same-edit-key? (nth path i) (nth % i)) paths))
                                  (range (dec (count path))))))]
            (when (and shared (pos? shared) (<= (- (count path) shared) 2))
              {:base call :prefix (subvec path 0 shared)
               :edits (mapv (fn [[call keys v]] [call (subvec keys shared) v])
                            (reverse edits))})))))))

(defn- emit-assoc-in-map-edit [emit m key child body]
  (plain-call "jolt-assoc-in-map-edit" [m (emit key) (str "(lambda (" child ") " body ")")]))

(defn- emit-assoc-in-suffix [emit m keys value]
  (let [leaf (fn [m key] (plain-call "jolt-assoc-in-leaf-edit" [m (emit key) (emit value)]))]
    (if (= 1 (count keys))
      (leaf m (first keys))
      (let [child (fresh-label "_ae$")]
        (emit-assoc-in-map-edit emit m (first keys) child (leaf child (second keys)))))))

(defn- emit-assoc-in-edits [emit initial edits linked stop]
  (letfn [(step [m i]
            (if-let [[_ keys value] (get edits i)]
              (let [first? (zero? i)
                    proc (when-not first? (fresh-label "_ae$"))
                    next-map (fresh-label "_ae$")
                    edit (emit-assoc-in-suffix emit m keys value)
                    ;; The first root is checked before descent. On a later
                    ;; miss, return the last good subtree for the ascent to
                    ;; rebuild. Save the exact callable already read for i.
                    attempt (if first? edit (str "(and (eq? " proc " jolt-assoc-in-original) " edit ")"))
                    miss (if first? "#f" (str "(begin (set! " stop " (cons " i " " proc ")) " m ")"))]
                (str "(let* (" (when proc (str "(" proc " " linked ") "))
                     "(" next-map " " attempt "))"
                     " (if " next-map " " (step next-map (inc i)) " " miss "))"))
              m))]
    (step initial 0)))

(defn- emit-unfused-assoc-in [emit base edits first-proc linked stop tail?]
  (let [index (fresh-label "_ae$")
        last-i (dec (count edits))
        names (into [base] (map (fn [_] (fresh-label "_ae$"))) (rest edits))
        calls (mapv (fn [i [call _ value]]
                      (let [path (fresh-label "_ae$")
                            v (fresh-label "_ae$")
                            m (nth names i)
                            tail-edit? (and tail? (= i last-i))
                            proc (if (zero? i) first-proc (str "(cdr " stop ")"))
                            captured (emit-fused-site-call call tail-edit? "jolt-assoc-in-apply" [proc m path v])
                            dispatch (if (zero? i) captured
                                         (str "(if (fx=? " index " " i ") " captured " "
                                              (emit-fused-site-call call tail-edit? linked [m path v]) ")"))
                            apply-edit (str "(let* ((" path " " (emit (nth (:args call) 1)) ")"
                                            " (" v " " (emit value) ")) " dispatch ")")]
                        (if (= i last-i) apply-edit
                            (str "(if (fx>=? " i " " index ") " apply-edit " " m ")"))))
                    (range (count edits)) edits)
        bindings (map (fn [nm expr] (str "(" nm " " expr ")")) (rest names) (pop calls))]
    (str "(let* ((" index " (if " stop " (car " stop ") 0)) " (str/join " " bindings) ") " (peek calls) ")")))

(defn- emit-assoc-in-chain [emit node tail?]
  (when (and (assoc-in-call? node) (chez-linked-fast-path?))
    (let [linked (seed-callable? nil "clojure.core" "assoc-in" 3)]
      (when (string? linked)
        ;; A literal path leaves the ordinary call's root read and its nested
        ;; receiver in one Scheme let, so that order is the backend's choice
        ;; and reading each root after the inner edit is one ordinary outcome.
        (when-let [{:keys [base prefix edits]} (assoc-in-chain node)]
          (let [base-name (fresh-label "_ae$")
                first-proc (fresh-label "_ae$")
                stop (fresh-label "_ae$")
                result (fresh-label "_ae$")
                frames (into [base-name] (map (fn [_] (fresh-label "_ae$"))) prefix)
                original-base (emit base)
                edited (reduce (fn [body [key m child]] (emit-assoc-in-map-edit emit m key child body))
                               (emit-assoc-in-edits emit (peek frames) edits linked stop)
                               (reverse (map vector prefix frames (rest frames))))
                fallback (emit-unfused-assoc-in emit base-name edits first-proc linked stop tail?)]
            (str "(let* ((" base-name " " original-base ")"
                 " (" first-proc " " linked ")"
                 " (" stop " #f)"
                 " (" result " (and (eq? " first-proc " jolt-assoc-in-original) " edited ")))"
                 " (if (and " result " (not " stop ")) " result
                 " (let ((" base-name " (or " result " " base-name "))) " fallback ")))")))))))

(defn- collecting-step
  "The reducer's arity with its final assoc re-targeted to the runtime
  collector. The confinement proof makes that assoc the accumulator's only
  use, so the step computes the same key and value and conses them on."
  [arity]
  (update arity :body
          (fn [edit]
            (assoc (ir/invoke (ir/host-ref "reduce-kv-collect") (:args edit))
                   :runtime-call "jolt-reduce-kv-collect"))))

(defn- emit-reduce-kv-builder [emit node tail?]
  (when (and (= "reduce-kv" (:name (:fn node))) (chez-linked-fast-path?)
             (facts/reducer-confinement-at (cur) node [] :opaque-accumulator-frames))
    (let [variant (atom {:rewrite collecting-step})
          ;; The callback is emitted once more, as the collecting step.
          args (into [(binding [*step-variant* variant] (emit (first (:args node))))]
                     (map emit) (rest (:args node)))
          temps (mapv (fn [_] (fresh-label "_kv$")) args)
          binds (str/join " " (map (fn [nm arg] (str "(" nm " " arg ")")) temps args))
          call (fn [proc as] (emit-fused-site-call node tail? proc as))
          ordinary (call "jolt-reduce-kv" temps)]
      (if-let [collect (:step @variant)]
        (str "(let* (" binds ") (if " (plain-call "jolt-reduce-kv-builder-source?" [(peek temps)])
             " " (call "jolt-reduce-kv-build" (conj (subvec temps 1) collect)) " " ordinary "))")
        (str "(let* (" binds ") " ordinary ")")))))

(defn emit-fused-invoke [emit node tail?]
  (or (emit-vector-filter-map emit node tail?)
      (emit-assoc-in-chain emit node tail?)
      (emit-reduce-kv-builder emit node tail?)))
