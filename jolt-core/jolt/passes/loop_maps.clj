(ns jolt.passes.loop-maps
  "Fixed-field map loop lowering. Rewrites a loop carrying a fixed keyword-keyed
  array-map accumulator into scalar field bindings with lazy materialization
  and an ordinary fallback lane for runtime update redefinitions."
  (:require [jolt.host :refer [seed-callable?]]
            [jolt.ir :as ir :refer [map-ir-children reduce-ir-children distinct-const-keys?]]
            [jolt.passes.fold :refer [kw-callee?]]
            [jolt.passes.inline :refer [fresh lookup-key any-binding-named?]]))

;; Fixed-field loop state. The version starts as the seed and becomes a lazy
;; map memo on each effective write. A resumed continuation can exit twice
;; with the same version; the memo makes both exits return the same object.
;; A linked update can change at runtime. The first nonstandard invocation
;; materializes its receiver and puts its result in the ordinary lane. A
;; one-element vector marks this lane even if the replacement returned nil.

(defn- unit [] @jolt.op-registry/current-unit-box)

(defn- core-invoke [nm args]
  (ir/invoke (ir/var-ref "clojure.core" nm) args))

(defn- loop-map-runtime [name args]
  (assoc (ir/invoke (ir/host-ref name) args) :loop-map-runtime (str "jolt-" name)))

(defn- core-head? [f nm]
  (and (= :var (:op f)) (= "clojure.core" (:ns f)) (= nm (:name f))))

(defn- named-local? [node names]
  (and (= :local (:op node)) (contains? names (:name node))))

(defn- ordinary-lane [state ordinary scalar]
  (if-let [spill (:spill state)] (ir/if-node spill ordinary scalar) scalar))

(defn- spilled-map [state]
  (core-invoke "nth" [(:spill state) (ir/const 0)]))

(defn- loop-map [state]
  (let [name ((:fresh state) "#loop")
        saved (ir/local name)
        version (:version state)
        fields (ir/map-node (mapv (fn [k] [(ir/const k) (get (:fields state) k)]) (:keys state)))]
    (ordinary-lane state (spilled-map state)
      (ir/let-node [[name (loop-map-runtime "loop-map-value" [version])]]
        (ir/if-node saved saved (loop-map-runtime "loop-map-save!" [version fields]))))))

(defn- loop-project [state lookup k]
  (assoc (ordinary-lane state (assoc-in lookup [:args 0] (spilled-map state))
                        (get (:fields state) k))
         :boxed-map-read true))

(defn- loop-exit-values [args]
  (assoc (ir/invoke (ir/host-ref "values") args) :loop-map-runtime "values"))

(defn- destructure-loop-exit [state value]
  (let [keys (:keys (:projection state))
        ordinary (fn [v]
                   (loop-exit-values (into [(ir/const false) v]
                                           (repeat (dec (count keys)) (ir/const nil)))))]
    (if (named-local? value (:aliases state))
      (let [seq? (assoc (ir/var-ref "clojure.core" "seq?") :loop-map-procedure :seed-root)
            standard? (loop-map-runtime "loop-map-standard-seq?" [seq?])
            scalar? (ordinary-lane state (ir/const false) standard?)]
        (ir/if-node scalar? (loop-exit-values (into [(ir/const true)] (map (:fields state) keys)))
                    (ordinary (loop-map state))))
      (ordinary value))))

(defn- reject-loop-map! [state node]
  (vreset! (:valid? state) false)
  node)

(declare rewrite-loop-value)

(defn- references-loop-name? [node names]
  (or (named-local? node names)
      (some names (:free-names node))
      (some names (:live-names node))
      (reduce-ir-children (fn [hit c] (or hit (references-loop-name? c names))) false node)))

(defn- rewrite-loop-let
  "Walk sequential bindings. A read-only alias of the slot has the same
  fields; any other binding with that name shadows it from here on."
  [node state finish]
  (if (:letrec node)
    (if (references-loop-name? node (into (:aliases state) (:retired state)))
      (reject-loop-map! state node)
      node)
    (loop [remaining (seq (:bindings node)) out [] state state]
      (if-let [[name init] (first remaining)]
        (if (named-local? init (:aliases state))
          (recur (next remaining) out
                 (-> state (update :aliases conj name) (update :retired disj name)))
          (recur (next remaining) (conj out [name (rewrite-loop-value init state)])
                 (-> state (update :aliases disj name) (update :retired disj name))))
        (let [body (finish (:body node) state)]
          (if (empty? out) body (assoc node :bindings out :body body)))))))

(defn- rewrite-loop-value [node state]
  (let [aliases (:aliases state)
        k (lookup-key node (:name (first (:args node))))]
    (cond
      (and (contains? aliases (:name (first (:args node))))
           (contains? (:fields state) k)
           (not (or (:proto node) (:devirt-type node))))
      (loop-project state node k)

      (or (named-local? node aliases) (named-local? node (:retired state)))
      (reject-loop-map! state node)

      (= :let (:op node)) (rewrite-loop-let node state rewrite-loop-value)
      ;; These regions can capture the old binding or have their own recur.
      (contains? #{:fn :loop :try :load-time-value} (:op node))
      (if (references-loop-name? node (into aliases (:retired state)))
        (reject-loop-map! state node)
        node)

      :else (map-ir-children #(rewrite-loop-value % state) node))))

(defn- fixed-map-edit [node state]
  (let [[_ key-node updater] (:args node)
        k (:val key-node)
        f (:fn node)
        n (count (:args node))]
    (when (and (= :invoke (:op node))
               (kw-callee? key-node) (contains? (:fields state) k)
               (not (or (:proto node) (:devirt-type node) (:prim-op node))))
      (cond
        (and (= n 3) (core-head? f "assoc")) [:assoc k]
        (and (or (and (= n 3) (core-head? updater "inc"))
                 (and (= n 4) (or (core-head? updater "conj") (core-head? updater "into"))))
             (core-head? f "update"))
        (let [link (seed-callable? nil "clojure.core" "update" n)]
          (when (string? link) [:update k link]))))))

(defn- bind-loop-values [state inits]
  (mapv (fn [v] [((:fresh state) "#loop") v]) inits))

(defn- binding-reads [binds]
  (mapv (fn [[name _]] (ir/local name)) binds))

(defn- write-loop-field
  "Build one edit as sequential bindings. Keeping the old and new field in
  locals also records whether array-map assoc would have returned its receiver."
  [node state read-state [edit k link] finish]
  (let [args (subvec (:args node) 2)
        fresh-name (:fresh state)
        operands (bind-loop-values state (mapv #(rewrite-loop-value % read-state) args))
        reads (binding-reads operands)
        old (get (:fields state) k)
        callee (fresh-name "#loop")
        fast? (fresh-name "#loop")
        update? (= edit :update)
        linked (when update? (assoc (:fn node) :loop-map-procedure link))
        fast-test (when (:spill state)
                    (ir/if-node (:spill state) (ir/const false)
                      (if update?
                        (loop-map-runtime "loop-map-update?" [(ir/local callee)])
                        (ir/const true))))
        next-value (if update?
                     ;; Only admitted native value procedures, at the arity
                     ;; update's apply would invoke. Keep their normal math.
                     (assoc node :fn (first args) :args (into [old] (subvec reads 1)))
                     (first reads))
        next-value (if fast-test (ir/if-node (ir/local fast?) next-value old) next-value)
        value-name (fresh-name "#loop")
        value (ir/local value-name)
        version-name (fresh-name "#loop")
        version (ir/if-node (core-invoke "identical?" [old value]) (:version state)
                            (loop-map-runtime "loop-map-memo" []))
        spill-name (when fast-test (fresh-name "#loop"))
        spill (when spill-name
                (ir/if-node (ir/local fast?) (:spill state)
                  (ir/vector-node
                    [(cond-> (assoc node :args (into [(loop-map state) (ir/const k)] reads))
                       update? (assoc :fn (ir/local callee) :loop-map-update true))])))
        binds (cond-> operands
                linked (conj [callee linked])
                fast-test (conj [fast? fast-test])
                true (conj [value-name next-value] [version-name version])
                spill (conj [spill-name spill]))]
    (vreset! (:edited? state) true)
    (ir/let-node binds (finish (cond-> (-> state
                                         (assoc-in [:fields k] value)
                                         (assoc :version (ir/local version-name)))
                                 spill (assoc :spill (ir/local spill-name)))))))

(defn- rewrite-edit-chain [node state finish]
  (letfn [(walk [n finish]
            (if (named-local? n (:aliases state))
              (finish state)
              (if-let [edit (fixed-map-edit n state)]
                (walk (first (:args n)) #(write-loop-field n % state edit finish))
                (reject-loop-map! state n))))]
    (walk node finish)))

(defn- rewrite-loop-transfer
  "Each arm feeds the same recur. Let/if regions may move around that recur
  only for its last operand, so source binders cannot capture a later operand."
  [node state last-arg? finish]
  (if-not last-arg?
    (rewrite-edit-chain node state finish)
    (case (:op node)
      :if (assoc node :test (rewrite-loop-value (:test node) state)
                      :then (rewrite-loop-transfer (:then node) state true finish)
                      :else (rewrite-loop-transfer (:else node) state true finish))
      :do (assoc node :statements (mapv #(rewrite-loop-value % state) (:statements node))
                      :ret (rewrite-loop-transfer (:ret node) state true finish))
      :let
      (let [[[name init] & more] (:bindings node)]
        (if (and (not (:letrec node)) (fixed-map-edit init state))
          (rewrite-edit-chain init state
            (fn [next-state]
              (rewrite-loop-transfer (assoc node :bindings (vec more))
                (assoc next-state :aliases #{name}
                                  :retired (disj (into (:retired state) (:aliases state)) name))
                true finish)))
          (if (and (not (:letrec node)) name)
            ;; Visit one binding at a time so a later initializer may be the
            ;; first edit, with all ordinary inits still evaluated in order.
            (rewrite-loop-let (assoc node :bindings [[name init]]
                                         :body (assoc node :bindings (vec more)))
              state #(rewrite-loop-transfer %1 %2 true finish))
            (if (:letrec node)
              (reject-loop-map! state node)
              (rewrite-loop-transfer (:body node) state true finish)))))
      (rewrite-edit-chain node state finish))))

(defn- rewrite-loop-recur [node state]
  (let [args (:args node)
        i (:index state)
        before (bind-loop-values state (mapv #(rewrite-loop-value % state) (subvec args 0 i)))
        after (subvec args (inc i))
        tail (rewrite-loop-transfer (nth args i) state (empty? after)
               (fn [new-state]
                 ;; Sibling operands read the OLD loop fields even when the
                 ;; edited map occupies an earlier argument.
                 (let [later (bind-loop-values state (mapv #(rewrite-loop-value % state) after))
                       group (into [(:version new-state)] (mapv (:fields new-state) (:keys state)))
                       group (cond-> group (:spill new-state) (conj (:spill new-state)))
                       recur-node (assoc node :args
                                    (into (into (binding-reads before) group)
                                          (binding-reads later)))]
                   (if (seq later) (ir/let-node later recur-node) recur-node))))]
    (if (seq before) (ir/let-node before tail) tail)))

(defn- rewrite-loop-tail [node state]
  (let [{:keys [lookup keys names]} (:projection state)]
    (case (:op node)
      :recur (rewrite-loop-recur node state)
      :if (assoc node :test (rewrite-loop-value (:test node) state)
                      :then (rewrite-loop-tail (:then node) state)
                      :else (rewrite-loop-tail (:else node) state))
      :do (assoc node :statements (mapv #(rewrite-loop-value % state) (:statements node))
                      :ret (rewrite-loop-tail (:ret node) state))
      :let (if (:letrec node) (reject-loop-map! state node)
             (rewrite-loop-let node state rewrite-loop-tail))
      :let-values
      (let [names (:names node)
            body-state (-> state
                           (update :aliases #(apply disj % names))
                           (update :retired #(apply disj % names)))]
        (assoc node :init (rewrite-loop-value (:init node) state)
                    :body (rewrite-loop-tail (:body node) body-state)))
      (cond
        names
        (destructure-loop-exit state (if (named-local? node (:aliases state))
                                       node (rewrite-loop-value node state)))
        (named-local? node (:aliases state))
        (if lookup (loop-project state lookup (first keys)) (loop-map state))
        :else
        (let [value (rewrite-loop-value node state)]
          (if lookup (assoc-in lookup [:args 0] value) value))))))

(defn- fixed-map-keys [node layouts]
  (case (:op node)
    :local (get layouts (:name node))
    :map (let [ks (mapv first (:pairs node))]
           ;; These sizes always use the array-map literal representation.
           (when (and (<= 1 (count ks) 8) (every? kw-callee? ks) (distinct-const-keys? ks))
             (mapv :val ks)))
    nil))

(defn- loop-has-update? [node]
  (or (core-head? (:fn node) "update")
      (reduce-ir-children (fn [found c] (or found (loop-has-update? c))) false node)))

(defn- loop-local-names [names node]
  (let [names (case (:op node)
                :local (conj names (:name node))
                (:let :loop) (into names (map first (:bindings node)))
                :let-values (into names (:names node))
                names)]
    (reduce-ir-children loop-local-names names node)))

(defn- fresh-loop-local [used-names prefix]
  (loop [candidate (fresh prefix)]
    (if (contains? used-names candidate)
      (recur (fresh prefix))
      candidate)))

(defn- scalar-loop-binding-eligible? [binds i name ks projection]
  (let [later (subvec binds (inc i))]
    (and ks (every? (set ks) (:keys projection))
         (not (any-binding-named? later name))
         ;; Even a lookup in a later initializer refers to the
         ;; original binder, outside the rewrite of the loop body.
         (not-any? #(references-loop-name? (second %) #{name}) later))))

(defn- try-scalar-loop-binding [node binds i name init ks projection]
  (when (scalar-loop-binding-eligible? binds i name ks projection)
    (let [used-names (loop-local-names #{} node)
          fresh-name #(fresh-loop-local used-names %)
          seed (fresh-name "#loop-seed")
          fields (mapv (fn [_] (fresh-name "#loop-field")) ks)
          spill (when (loop-has-update? (:body node))
                  (fresh-name "#loop-spill"))
          state {:index i :keys ks :aliases #{name} :retired #{}
                 :projection projection :version (ir/local seed)
                 :spill (when spill (ir/local spill))
                 :fresh fresh-name
                 :fields (zipmap ks (mapv ir/local fields))
                 :valid? (volatile! true) :edited? (volatile! false)}
          body (rewrite-loop-tail (:body node) state)]
      (when (and @(:valid? state) @(:edited? state))
        (let [field-inits (mapv (fn [nm k] [nm (core-invoke "get" [(ir/local seed) (ir/const k)])])
                                fields ks)
              group (cond-> (into [[seed init]] field-inits)
                      spill (conj [spill (ir/const false)]))]
          (assoc node :bindings (into (into (subvec binds 0 i) group) (subvec binds (inc i)))
                      :body body))))))

(defn- scalar-loop-map [node layouts projection]
  (let [binds (:bindings node)]
    (loop [i 0 layouts layouts]
      (when-let [[name init] (get binds i)]
        (let [ks (fixed-map-keys init layouts)]
          (or (try-scalar-loop-binding node binds i name init ks projection)
              (recur (inc i) (if ks (assoc layouts name ks) (dissoc layouts name)))))))))

(defn- loop-destructuring
  "Match the map destructurer's sequence coercion and consecutive fixed reads.
  Its two intermediate bindings must be private to this prefix."
  [bindings body]
  (let [[[raw _] [coerced normalize]] bindings
        test (:test normalize)]
    (when (and coerced (not= raw coerced) (= :if (:op normalize))
               (named-local? (:else normalize) #{raw})
               (= :invoke (:op test)) (core-head? (:fn test) "seq?")
               (= 1 (count (:args test))) (named-local? (first (:args test)) #{raw})
               ;; A seed-root call uses the same hoisted procedure at the
               ;; fast-path check and at the original fallback test.
               (true? (seed-callable? nil "clojure.core" "seq?" 1)))
      (let [private-names #{raw coerced}
            reads (take-while (fn [[_ v]] (keyword? (lookup-key v coerced))) (subvec bindings 2))
            n (+ 2 (count reads))
            names (mapv first reads)
            remaining (subvec bindings n)]
        (when (and (seq reads) (apply distinct? names)
                   (not-any? private-names names)
                   (not-any? #(references-loop-name? (second %) private-names) remaining)
                   (not (references-loop-name? body private-names)))
          {:bindings (subvec bindings 0 n) :names names
           :keys (mapv #(lookup-key (second %) coerced) reads)})))))

(declare lower-map-loop-tree lower-map-bindings)

(defn- bind-loop-exit [node producer projection body]
  (let [[[raw _] [coerced normalize] & reads] (:bindings projection)
        used (loop-local-names #{} node)
        flag (fresh-loop-local used "#loop-exit")
        slots (mapv (fn [_] (fresh-loop-local used "#loop-result")) reads)
        scalar? (ir/local flag)
        ;; The first slot holds the raw result on fallback. Do the original
        ;; coercion and reads here, outside the loop and in their lexical scope.
        coerce [coerced (ir/if-node scalar? (ir/const nil)
                                   (ir/let-node [[raw (ir/local (first slots))]] normalize))]
        fields (mapv (fn [[name read] slot] [name (ir/if-node scalar? (ir/local slot) read)])
                     reads slots)]
    {:op :let-values :names (into [flag] slots) :init producer
     :body (ir/let-node (into [coerce] fields) body)}))

(defn- lower-loop-destructuring [node bindings i scope init]
  (when (= :loop (:op init))
    (when-let [projection (loop-destructuring (subvec bindings i) (:body node))]
      (let [subject (lower-map-bindings init scope)]
        {:projection projection
         :subject subject
         :producer (scalar-loop-map subject scope projection)}))))

(defn- lower-ordinary-binding [init subject scope name sequential-let?]
  (let [v (if subject
            (or (scalar-loop-map subject scope nil) subject)
            (lower-map-loop-tree init scope))
        ks (when sequential-let? (fixed-map-keys v scope))
        next-scope (if ks (assoc scope name ks) (dissoc scope name))]
    [v next-scope]))

(defn- finish-map-bindings [node out scope sequential-let?]
  (let [body (lower-map-loop-tree (:body node) scope)]
    (if (and sequential-let? (empty? out))
      body
      (assoc node :bindings out :body body))))

(defn- lower-map-bindings [node layouts]
  (let [sequential-let? (not (or (= :loop (:op node)) (:letrec node)))
        bindings (:bindings node)]
    (loop [i 0 out [] scope (if (:letrec node) {} layouts)]
      (if-let [[name init :as b] (get bindings i)]
        (let [{:keys [projection subject producer]}
              (when sequential-let? (lower-loop-destructuring node bindings i scope init))]
          (if producer
            (let [tail (subvec bindings (+ i (count (:bindings projection))))
                  body (lower-map-bindings (assoc node :bindings tail)
                                           (apply dissoc scope (:names projection)))
                  joined (bind-loop-exit node producer projection body)]
              (if (seq out) (assoc node :bindings out :body joined) joined))
            (let [[v next-scope] (lower-ordinary-binding init subject scope name sequential-let?)
                  binding (if (identical? v init) b [name v])]
              (recur (inc i) (conj out binding) next-scope))))
        (finish-map-bindings node out scope sequential-let?)))))

(defn- lower-map-loop-tree [node layouts]
  (case (:op node)
    :let (lower-map-bindings node layouts)
    :loop (let [n (lower-map-bindings node layouts)] (or (scalar-loop-map n layouts nil) n))
    :invoke
    (let [subject (first (:args node))
          placeholder "#loop-projection"
          key (when (and (= :loop (:op subject)) (not (or (:proto node) (:devirt-type node))))
                (lookup-key (assoc-in node [:args 0] (ir/local placeholder)) placeholder))]
      (or (when key (scalar-loop-map subject layouts {:lookup node :keys [key]}))
          (map-ir-children #(lower-map-loop-tree % layouts) node)))
    (:fn :try :load-time-value) (map-ir-children #(lower-map-loop-tree % {}) node)
    (map-ir-children #(lower-map-loop-tree % layouts) node)))

(defn lower-fixed-map-loops [node]
  (let [u (unit)]
    (if (and (= :chez @(:target u)) (not @(:seed-mint? u)) (not @(:debug-locals? u)))
      (lower-map-loop-tree node {})
      node)))
