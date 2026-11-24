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
            [jolt.backend-scheme.names :refer [dl-name munge-name]]
            [jolt.backend-scheme.session :as session
             :refer [*trace-site* cur direct-link? direct-linkable? fresh-label
                     node-line register-callsite! seed-mint? static-callee target
                     trace-frames? with-site]]
            [jolt.backend-scheme.hoist :refer [*const-pool* hoist-const-per-site hoist-seed-root]]
            [jolt.backend-scheme.calls :refer [emit-call native-op node-inline-chain plain-call]]
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

(defn- ordered-assoc-in-chain? [edits linked tail?]
  ;; Ordinary ordered-call binds a nested receiver before a dynamic path and
  ;; the linked callee. A traced tail call also binds its operands first.
  ;; Otherwise a constant path leaves the receiver and root read in the same
  ;; Scheme let, whose evaluation order Chez may choose at each site.
  (let [traced-tail? (and tail? (trace-frames?) *trace-site* (not (contains? session/*trace-self* linked)))
        end (if traced-tail? (dec (count edits)) (count edits))]
    (every? (fn [[call]] (not (ir/const-coll-node? (nth (:args call) 1) true)))
            (subvec edits 1 end))))

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
        (when-let [{:keys [base prefix edits]} (assoc-in-chain node)]
          (when (ordered-assoc-in-chain? edits linked tail?)
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
                   " (let ((" base-name " (or " result " " base-name "))) " fallback ")))"))))))))

(def ^:private builder-symbol-calls
  [["symbol?" [1] "jolt-symbol?"]
   ["namespace" [1] "jolt-namespace"]
   ["name" [1] "jolt-name"]
   ["keyword" [1 2] "jolt-keyword"]])

(defn- emit-builder-symbol-guard []
  (let [core "clojure.core"]
    (when (every? (fn [[nm arities _]]
                    (and (not (direct-linkable? core nm))
                         (every? #(and (nil? (native-op {:op :var :ns core :name nm} %))
                                       (true? (seed-callable? nil core nm %)))
                                 arities)))
                  builder-symbol-calls)
      ;; These calls use roots hoisted at load time. The ordinary callback
      ;; and the guard must consult the same hoists.
      (plain-call "and"
        (map (fn [[nm _ proc]] (plain-call "eq?" [(hoist-seed-root core nm) proc]))
             builder-symbol-calls)))))

(defn- builder-replace-step
  "The reducer's arity with a fourth parameter, the linked replace callable
  the fast step read, which its replace call applies in place of a root read."
  [arity]
  (let [taken (set (map munge-name (:params arity)))
        proc (first (remove #(contains? taken (munge-name %)) (repeatedly #(fresh-label "_kv$"))))]
    (-> arity
        (update :params #(conj (vec %) proc))
        (assoc-in [:body :args 2 :fn :root-binding] (munge-name proc)))))

(defn- reduce-kv-replace-root [kind]
  (when (= kind :string-replace)
    (let [root (seed-callable? nil "clojure.string" "replace" 3)]
      (when (and (string? root)
                 (not (direct-linkable? "clojure.string" "replace"))
                 (= root (dl-name "clojure.string" "replace")))
        root))))

(defn- reduce-kv-fast-call [kind call temps replace-root match replacement step]
  (case kind
    :copy (call "jolt-reduce-kv-build-copy" temps)
    :symbol-keys (call "jolt-reduce-kv-build-symbol-keys" temps)
    :string-replace
    (when step
      (call "jolt-reduce-kv-build-string-replace"
            (into temps [(str "(lambda () " replace-root ")") match replacement step])))))

(defn- emit-reduce-kv-plan [emit node tail? kind literals replace-root symbol-guard]
  (let [[match replacement] (when replace-root (mapv #(hoist-const-per-site (emit %)) literals))
        ;; The first ordinary step after a root change and the rest
        ;; of the callback must receive the very same literal objects.
        arg-nodes (cond-> (:args node)
                    replace-root (assoc-in [0 :arities 0 :body :args 2 :args 1 :literal-binding] match)
                    replace-root (assoc-in [0 :arities 0 :body :args 2 :args 2 :literal-binding] replacement))
        ;; On a root change the runtime applies the reducer's own body,
        ;; emitted once more by emit-fn with the callable it read, to
        ;; the persistent prefix. Its trace is the literal's.
        variant (atom (when replace-root {:rewrite builder-replace-step}))
        args (into [(binding [*step-variant* variant] (emit (first arg-nodes)))]
                   (map emit) (rest arg-nodes))
        temps (mapv (fn [_] (fresh-label "_kv$")) args)
        binds (str/join " " (map (fn [nm arg] (str "(" nm " " arg ")")) temps args))
        source-guard (plain-call "jolt-reduce-kv-builder-source?" [(peek temps)])
        guard (if symbol-guard (plain-call "and" [source-guard symbol-guard]) source-guard)
        call (fn [proc as] (emit-fused-site-call node tail? proc as))
        fast (reduce-kv-fast-call kind call temps replace-root match replacement (:step @variant))
        ordinary (call "jolt-reduce-kv" temps)]
    (if fast
      (str "(let* (" binds ") (if " guard " " fast " " ordinary "))")
      (str "(let* (" binds ") " ordinary ")"))))

(defn- emit-reduce-kv-builder [emit node tail?]
  (when (and (= "reduce-kv" (:name (:fn node))) (chez-linked-fast-path?))
    (when-let [{:keys [kind literals]} (facts/closed-reducer-plan (cur) node)]
      (let [replace-root (reduce-kv-replace-root kind)
            symbol-guard (when (= kind :symbol-keys) (emit-builder-symbol-guard))]
        (when (or (= kind :copy) symbol-guard replace-root)
          (emit-reduce-kv-plan emit node tail? kind literals replace-root symbol-guard))))))

(defn emit-fused-invoke [emit node tail?]
  (or (emit-vector-filter-map emit node tail?)
      (emit-assoc-in-chain emit node tail?)
      (emit-reduce-kv-builder emit node tail?)))
