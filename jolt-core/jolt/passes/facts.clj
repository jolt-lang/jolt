(ns jolt.passes.facts
  "Lexical bindings, value origins, and reachability for one IR body revision."
  (:require [jolt.analyzer.tree :refer [arity-params]]
            [jolt.host :as host]
            [jolt.ir :as ir]
            [jolt.op-registry :as ops]))

(def ^:private max-nodes 10000)
(def ^:private max-path-length 192)
(def ^:private max-origins 256)
(def ^:private unknown #{{:kind :unknown}})

(defn- site [kind path] #{{:kind kind :site path}})

(defn- incomplete! []
  (throw (ex-info "IR facts analysis incomplete" {::incomplete true})))

(defn- spend! [remaining]
  (when (neg? (swap! remaining dec)) (incomplete!)))

(defn- join-origins [left right]
  (let [joined (into left right)]
    (when (> (count joined) max-origins) (incomplete!))
    joined))

(defn- effect! [state kind origins]
  (swap! (:effects state) update kind (fnil into #{}) origins))

(defn- opaque! [state origins]
  (effect! state :opaque origins))

(defn- merge-effects! [state effects]
  (swap! (:effects state) #(merge-with into % effects)))

;; Scope maps printed names to [binder path, value origins, enclosing fn]. Neither the
;; bindings nor their uses are keyed by IR maps: two children can share the
;; very same node object after substitution and still be different occurrences.
(defn- bind-local! [state env name path kind origins]
  (swap! (:bindings state) assoc path {:name name :kind kind})
  (assoc env name [path origins (:fn-path state)]))

(declare walk)

(defn- note-capture! [state name binding]
  (when (and (:captures state) (not= (:fn-path state) (nth binding 2 nil)))
    (swap! (:captures state) assoc name
           {:binding (first binding) :origins (or (second binding) unknown)})))

(defn- walk-local [state node path env]
  (let [name (:name node)
        [binder origins :as binding] (get env name)]
    (note-capture! state name binding)
    (when binder (swap! (:uses state) assoc path binder))
    (or origins unknown)))

(defn- letrec-scope [state bindings path env]
  (reduce-kv
    (fn [env i [name init]]
      (let [p (conj path :bindings i)]
        ;; Letfn's procedure literals are known before walking any of their
        ;; bodies. Other letrec* initializer values may depend on initialization
        ;; order or a cycle, so leave their origins unknown.
        (bind-local! state env name (conj p 0) :letfn
                     (when (= :fn (:op init)) (site :allocation (conj p 1))))))
    env bindings))

(defn- walk-loop-body [state node path env]
  (let [binders (mapv #(conj path :bindings % 0) (range (count (:bindings node))))
        initializers (mapv #(conj path :bindings % 1) (range (count binders)))]
    (swap! (:loops state) assoc path {:binders binders :initializers initializers})
    (let [env (reduce-kv (fn [env i [name _]]
                          (let [binder (nth binders i)]
                            (assoc env name [binder (site :loop-slot binder) (:fn-path state)])))
                        env (:bindings node))]
      (walk state (:body node) (conj path :body) env {:loop path :binders binders}))))

(defn- walk-let [state node path env recur-params]
  (let [bindings (:bindings node)
        loop? (= :loop (:op node))
        recursive? (:letrec node)
        env (if recursive? (letrec-scope state bindings path env) env)
        body-env
        (reduce-kv
          (fn [env i [name init]]
            (let [p (conj path :bindings i)
                  origins (walk state init (conj p 1) env recur-params)]
              (when (and recursive? (not= :fn (:op init)))
                ;; Non-fn letrec slots lose provenance.
                (opaque! state origins))
              (if recursive?
                env
                (bind-local! state env name (conj p 0) (if loop? :loop :let) origins))))
          env bindings)]
    (if loop?
      (walk-loop-body state node path body-env)
      (walk state (:body node) (conj path :body) body-env recur-params))))

(defn- walk-arity [state arity path env operands]
  (let [fixed-count (count (:params arity))
        numeric-hints (into {} (:nhints arity))
        [arity-env params]
        (reduce-kv
          (fn [[env params] j name]
            (let [rest? (= j fixed-count)
                  binder (if rest? (conj path :rest) (conj path :params j))]
              [(bind-local! state env name binder (if rest? :rest :param)
                            ;; Entry coercion can replace the caller's value.
                            (if operands
                              (nth operands j)
                              (when-not (and (not rest?)
                                             (contains? #{:long :double} (get numeric-hints name)))
                                #{{:kind :argument :binding binder}})))
               (conj params binder)]))
          [env []] (arity-params arity))
        origins (walk state (:body arity) (conj path :body) arity-env {:binders params})]
    ;; Coercion may return its input, convert it, or name its class in a
    ;; cast exception. Preserve all three possibilities for downstream queries.
    (if (contains? #{:long :double} (:ret-nhint arity))
      (do (effect! state :representation origins)
          (join-origins origins unknown))
      origins)))

(defn- once-body-env [node env]
  ;; Match the emitter's capture box, including the spliced-closure exception.
  ;; On a retry a slot may have been cleared to nil. Keep both the binder and
  ;; the original possibility, which can still flow out on the first call.
  (if (and (:once node) (nil? (:live-names node)))
    (reduce (fn [env name]
              (if-let [[binder origins owner] (get env name)]
                (assoc env name [binder (if origins (join-origins origins unknown) unknown) owner])
                env))
            env (:free-names node))
    env))

(defn- walk-fn [state node path env]
  (let [origins (site :allocation path)
        inner (-> state
                  (assoc :fn-path path :captures (atom {}) :effects (atom {}))
                  (update :active-fns conj node))
        body-env (once-body-env node env)
        self-env (if-let [name (:name node)]
                   (bind-local! inner body-env name (conj path :name) :self origins)
                   body-env)
        returns (reduce-kv
                  (fn [returns i arity]
                    (join-origins returns
                                  (walk-arity inner arity (conj path :arities i) self-env nil)))
                  #{} (:arities node))]
    (swap! (:fn-targets state) assoc path {:fn node :env self-env})
    ;; The image's live names describe the spliced closure. A vector entry
    ;; carries literal data, with no lexical binding. The body walk also sees
    ;; captures in generated or rewritten IR without image registration.
    (doseq [name (or (:live-names node) (:free-names node)) :when (string? name)]
      (note-capture! inner name (get env name)))
    (let [captures @(:captures inner)]
      (doseq [name (keys captures)] (note-capture! state name (get env name)))
      (swap! (:closures state) assoc path
             {:captures captures :returns returns :effects @(:effects inner)
              :zero-arity? (and (ir/single-fixed-arity-fn? node)
                                (empty? (:params (first (:arities node)))))}))
    origins))

(defn- walk-try [state node path env recur-params]
  (let [normal (walk state (:body node) (conj path :body) env recur-params)
        caught (when-let [catch-body (:catch-body node)]
                 (let [env (if-let [name (:catch-sym node)]
                             (bind-local! state env name (conj path :catch-sym) :catch nil)
                             env)]
                   (walk state catch-body (conj path :catch-body) env recur-params)))]
    (when-let [finally (:finally node)]
      (walk state finally (conj path :finally) env recur-params))
    (join-origins normal caught)))

(defn- scalar-key?
  "Keys whose comparison cannot invoke user code when both keys qualify."
  [v]
  (or (keyword? v) (nil? v) (boolean? v) (string? v) (int? v)))

(defn- literal-key? [node]
  (and (= :const (:op node)) (scalar-key? (:val node))))

(defn- child-origins [state path]
  (get @(:origins state) path unknown))

(defn- container [state origin]
  (get @(:containers state) (:site origin)))

(defn- walk-collection [state node path]
  (let [kind (:op node)
        map? (= :map kind)
        items (if map? (:pairs node) (:items node))
        indexed? (= :vector kind)
        entries (reduce-kv
                  (fn [entries i item]
                    (let [p (conj path (if map? :pairs :items) i)
                          k (if map? (first item) item)
                          value (child-origins state (if map? (conj p 1) p))]
                      (if indexed?
                        (assoc entries i value)
                        (if (and entries (literal-key? k)
                                 (not (contains? entries (:val k))))
                          (assoc entries (:val k) value)
                          (reduced nil)))))
                  {} items)]
    (when (and (not indexed?) (nil? entries))
      ;; Hashing and equality can call out through keys or members. The
      ;; constructor itself never passes the value slots of a map to them.
      (dotimes [i (count items)]
        (opaque! state (child-origins state (if map? (conj path :pairs i 0)
                                               (conj path :items i))))))
    (let [children (ir/reduce-ir-children-with-path
                     (fn [acc _ p] (join-origins acc (child-origins state p)))
                     #{} node path)]
      (swap! (:containers state) assoc path {:kind kind :slots entries :children children}))
    (site (if (ir/const-coll-node? node) :literal :allocation) path)))

(defn- record-constructor [state node]
  (let [shape (get (:record-classes state) (:class node))]
    ;; Match the emitter's positional :host-new arm. Named Vars may instead
    ;; dispatch to a different function. Bare deftypes can override lookup.
    (when (and (string? (:class node)) (:record shape)
               (= (count (:fields shape)) (count (:args node)))
               (<= (count (:args node)) 6))
      shape)))

(defn- walk-record [state path shape args]
  (let [stored (mapv (fn [i origins]
                       (if (= "double" (nth (:tags shape) i nil))
                         (join-origins origins unknown)
                         origins))
                     (range (count args)) args)
]
    (swap! (:containers state) assoc path
           {:kind :record :children (reduce join-origins #{} stored)})
    (site :allocation path)))

(defn- projection-slots [mode k kind slots]
  (case kind
    :vector (when (and (int? k) (<= 0 k 2147483647)) slots)
    (:map :set) (when (= mode :get) slots)
    nil))

(defn- projection [state mode receiver k fallback]
  (reduce
    (fn [result origin]
      (let [{:keys [kind slots]} (container state origin)
            table (projection-slots mode k kind slots)
            entry (find table k)]
        (if (and table (or entry fallback))
          (let [present? (some? entry)
                projected (if entry (val entry) fallback)]
            {:present? (when (or (nil? result) (= present? (:present? result))) present?)
             :origins (join-origins (:origins result #{}) projected)})
          (reduced nil))))
    nil receiver))

(defn- projection-call [state path mode receiver-path key-node fallback]
  (when (literal-key? key-node)
    (let [k (:val key-node)]
      (if-let [answer (projection state mode (child-origins state receiver-path) k fallback)]
        (do (effect! state :read (child-origins state receiver-path))
            (when (some? (:present? answer)) (swap! (:projections state) assoc path answer))
            (:origins answer))
        nil))))

(defn- reducer-edit [state path [receiver key value]]
  ;; Only the scoped reducer query can install this native map receiver.
  ;; Assoc can call out while comparing or hashing keys. Its result stores
  ;; the supplied operands, and can reuse the receiver on a no-op. Track the
  ;; new contents separately from the old version.
  (effect! state :read receiver)
  (opaque! state key)
  (swap! (:containers state) assoc path {:children (join-origins key value)})
  (join-origins receiver (site :reducer-edit path)))

(defn- fixed-clause [node n]
  (some (fn [a] (when (and (not (:rest a)) (= n (count (:params a)))) a))
        (:arities node)))

(defn- uncoerced? [arity]
  (and (not (:ret-nhint arity))
       (empty? (:nhints arity)) (empty? (:ahints arity)) (empty? (:phints arity))))

(defn- invoke-known [state path target operands]
  (let [callee (:fn target)
        arity (fixed-clause callee (count operands))]
    (when (and arity (uncoerced? arity) (not (:once callee))
               (not-any? #(identical? callee %) (:active-fns state)))
      (let [p (conj path :call-body)
            inner (-> state
                      (assoc :fn-path p :captures (atom {}))
                      (update :active-fns conj callee))]
        (walk-arity inner arity p (:env target) operands)))))

(defn- invoke-target-origins [state path callee n operands]
  (when (and (= 1 (count callee)) (= :allocation (:kind (first callee))))
    (let [target-path (:site (first callee))]
      (when-let [target (get @(:fn-targets state) target-path)]
        (or (invoke-known state path target operands)
            ;; Existing capture flow for zero-arg once/coercing closures.
            (when-let [closure (when (zero? n) (get @(:closures state) target-path))]
              (when (and (:zero-arity? closure)
                         (not-any? #(identical? (:fn target) %) (:active-fns state)))
                (merge-effects! state (:effects closure))
                (:returns closure))))))))

(defn- invoke-core-origins [state path core args operands absent]
  (let [n (count args)
        arg (fn [i] (nth operands i))]
    (case core
      "identical?"
      (when (= n 2)
        (effect! state :identity (into (arg 0) (arg 1)))
        absent)
      "identity"
      (when (= n 1) (arg 0))
      ("not" "nil?" "some?")
      (when (= n 1)
        (effect! state :read (arg 0))
        absent)
      ("get" "nth")
      (when (<= 2 n 3)
        (let [get? (= core "get")
              fallback (if (> n 2) (arg 2) (when get? absent))]
          (projection-call state path (if get? :get :nth)
                           (conj path :args 0) (second args) fallback)))
      ("class" "type")
      (when (= n 1)
        (effect! state :representation (arg 0))
        nil)
      nil)))

(defn- invoke-literal-callee-origins [state path f args operands absent]
  (let [n (count args)
        op (:op f)]
    (case op
      :const
      (when (and (keyword? (:val f)) (<= 1 n 2))
        (projection-call state path :get (conj path :args 0) f
                         (if (> n 1) (nth operands 1) absent)))
      :map
      (when (<= 1 n 2)
        (projection-call state path :get (conj path :fn) (first args)
                         (if (> n 1) (nth operands 1) absent)))
      (:vector :set)
      (when (= n 1)
        (projection-call state path (if (= :vector op) :nth :get)
                         (conj path :fn) (first args) (when (= :set op) absent)))
      nil)))

(defn- invoke-origins [state node path operands]
  (when-not (or (:proto node) (:devirt-type node))
    (cond
      (= path (:reducer-edit-path state)) (reducer-edit state path operands)
      :else
      (let [f (:fn node)
            args (:args node)
            callee (child-origins state (conj path :fn))
            absent (site :literal path)
            core (when (and (= :var (:op f)) (= "clojure.core" (:ns f)))
                   (:name f))]
        (or (invoke-target-origins state path callee (count args) operands)
            ;; These exact call heads and arities lower directly to native ops.
            (when core (invoke-core-origins state path core args operands absent))
            (invoke-literal-callee-origins state path f args operands absent))))))

(defn- walk-invoke [state node path]
  (let [operands (mapv #(child-origins state (conj path :args %)) (range (count (:args node))))
        effects (atom {})
        inner (assoc state :effects effects)
        known (invoke-origins inner node path operands)
        returns (or known
                    (do (opaque! inner (reduce join-origins
                                               (child-origins state (conj path :fn))
                                               operands))
                        unknown))]
    (merge-effects! state @effects)
    (swap! (:calls state) assoc path {:args operands :effects @effects :returns returns
                                     :resolved? (some? known)})
    returns))

(defn- walk-recur [state path target n]
  (let [{:keys [loop binders]} target
        args (mapv #(conj path :args %) (range n))]
    (if loop
      (do (when-not (= n (count binders)) (incomplete!))
          ;; Every operand has already been walked in the old environment.
          (swap! (:recurs state) assoc path {:loop loop :args args})
          #{})
      (do (swap! (:recurrent state) into binders)
          (doseq [arg args] (opaque! state (child-origins state arg)))
          unknown))))

(defn- walk [state node path env recur-params]
  (when (> (count path) max-path-length) (incomplete!))
  (spend! (:remaining state))
  (let [op (:op node)
        origins
        (case op
          :local (walk-local state node path env)
          (:let :loop) (walk-let state node path env recur-params)
          :fn (walk-fn state node path env)
          :try (walk-try state node path env recur-params)
          (let [child (fn [step] (child-origins state (conj path step)))
                opaque-children
                (fn []
                  (ir/reduce-ir-children-with-path
                    (fn [_ _ p] (opaque! state (child-origins state p))) nil node path)
                  unknown)]
            (when-not (contains? ir/node-ops op) (incomplete!))
            (ir/reduce-ir-children-with-path
              (fn [_ child p] (walk state child p env recur-params)) nil node path)
            (case op
              (:const :quote :regex :load-time-value) (site :literal path)
              (:vector :map :set) (walk-collection state node path)
              :if (do (effect! state :read (child :test))
                      (join-origins (child :then) (child :else)))
              :do (child :ret)
              :invoke (walk-invoke state node path)
              :host-new (if-let [shape (record-constructor state node)]
                          (walk-record state path shape
                                       (mapv #(child-origins state (conj path :args %))
                                             (range (count (:args node)))))
                          (opaque-children))
              :recur (walk-recur state path recur-params (count (:args node)))
              ;; Stores, throws (including locally caught ones), host/FFI
              ;; calls, and any other unmodeled operation can leak their operands.
              (opaque-children))))]
    (swap! (:origins state) assoc path origins)
    origins))

(defn- captures-origins [closure]
  (reduce join-origins #{} (map :origins (vals (:captures closure)))))

(defn- reachable [roots edges remaining]
  (loop [seen roots pending (vec roots)]
    (if-let [origin (peek pending)]
      (let [_ (when remaining (spend! remaining))
            children (remove seen (get edges (:site origin)))]
        (recur (into seen children) (into (pop pending) children)))
      seen)))

(defn- transfer-origins [state]
  (reduce-kv
    (fn [transfers _ {:keys [loop args]}]
      (reduce-kv (fn [out i binder]
                   (update out binder (fnil join-origins #{})
                           (child-origins state (nth args i))))
                 transfers (get-in @(:loops state) [loop :binders])))
    {} @(:recurs state)))

(defn- header-origins [state transfers]
  (reduce-kv
    (fn [headers _ {:keys [binders initializers]}]
      (reduce-kv (fn [out i binder]
                   (assoc out binder (join-origins (child-origins state (nth initializers i))
                                                  (get transfers binder))))
                 headers binders))
    {} @(:loops state)))

(defn- intersect? [a b]
  (boolean (some #(contains? b %) a)))

(defn- walk-body [body record-classes remaining options]
  (let [state {:bindings (atom {}) :uses (atom {}) :origins (atom {})
               :recurrent (atom #{}) :remaining remaining
               :loops (atom {}) :recurs (atom {})
               :containers (atom {}) :closures (atom {}) :projections (atom {})
               :effects (atom {}) :calls (atom {}) :fn-targets (atom {}) :active-fns []
               :record-classes record-classes}
        state (merge state options)]
    (assoc state :result (walk state body [] {} nil))))

(defn- analyze-body [body record-classes options]
  (let [state (walk-body body record-classes (atom max-nodes) options)
        ;; Value flow into each loop slot, from its initializer and every
        ;; recur. These are distinct from aggregates' stored fields.
        aliases (header-origins state (transfer-origins state))
        closures @(:closures state)
        contained (into {}
                        (map (fn [[p c]] [p (captures-origins c)])) closures)
        contained (into contained (map (fn [[p c]] [p (:children c)])) @(:containers state))
        ;; Constructing a procedure captures values but doesn't run its body.
        ;; An exposed procedure could be invoked, leak its result, or publish
        ;; other values. Keep latent effects off the containment graph.
        flow-edges (merge-with join-origins contained aliases)
        edges (reduce-kv (fn [edges p c]
                           (update edges p join-origins
                                   (join-origins (:returns c) (get-in c [:effects :opaque]))))
                         flow-edges closures)
        immediate @(:effects state)
        published (reachable (into (:result state) (get immediate :opaque)) edges nil)
        effects (reduce (fn [effects {:keys [site kind]}]
                          (if (= :allocation kind)
                            (merge-with into effects (get-in closures [site :effects]))
                            effects))
                        immediate published)
        opaque (reachable (get effects :opaque #{}) edges nil)
        identity-observed (reachable (get effects :identity #{}) aliases nil)
        representation-observed (reachable (get effects :representation #{}) aliases nil)]
    {:bindings @(:bindings state) :uses @(:uses state) :origins @(:origins state)
     :recurrent @(:recurrent state)
     :calls @(:calls state) :aliases aliases :flow-edges flow-edges :escape-edges edges
     :contained contained :closures closures :projections @(:projections state)
     :published published :opaque opaque :identity identity-observed
     :representation representation-observed}))

(defn- analyze-facts [body shapes options]
  (let [classes (reduce (fn [classes shape]
                          (let [class (:type shape)]
                            ;; Dotted record names can share a runtime class
                            ;; tag. The matching constructor is ambiguous.
                            (assoc classes class (when-not (contains? classes class) shape))))
                        {} (vals shapes))]
    (try
      (analyze-body body classes options)
      (catch clojure.lang.ExceptionInfo e
        ;; Unsupported layouts and exhausted budgets can hide uses of even
        ;; the sites we already walked.
        (if (::incomplete (ex-data e)) {} (throw e))))))

(defn body-facts
  "Return facts for body in the given compilation unit. Request again after
  rewriting IR. The unit caches only its latest root and record registry
  snapshot. Debug-local frames can publish any live
  local through a throwable, so instrumented bodies get conservative answers.
  All binding and site paths are relative to this snapshot. Include the
  enclosing fn/let/etc. to resolve its bindings."
  [unit body]
  (let [cache (:body-facts unit)
        shapes (:record-shapes @(:config unit))
        previous @cache]
    (cond
      @(:debug-locals? unit) {}
      (and previous (identical? body (nth previous 0)) (identical? shapes (nth previous 1)))
      (nth previous 2)
      :else
      (let [facts (analyze-facts body shapes {})]
        (reset! cache [body shapes facts])
        facts))))

(defn binding-at
  "The binder path for a local use, or nil. The :bindings table describes those
  binder paths. For an alias, query its initializer to find the source binder."
  [facts path]
  (get (:uses facts) path))

(defn- unresolved-origin? [facts origin]
  (or (= :loop-slot (:kind origin))
      (contains? (:recurrent facts) (:binding origin))))

(defn- resolved-origins [facts origins]
  (if (some #(unresolved-origin? facts %) origins)
    (into #{} (map #(if (unresolved-origin? facts %) (first unknown) %)) origins)
    origins))

(defn call-summary-at
  "Summarize this invoke's fixed arity and operand origins, including effects
  of the selected body before normal return. :arguments is indexed by operand.
  Its sets describe possible :read, :identity, :representation, :publish
  (exposure during the call), :return-alias and :return-contains. Returning a
  closure or aggregate does not itself publish its contents. :unknown means
  the argument can have further effects or result flow. :resolved? says the
  immediate dispatch was modeled. Nested calls can still be opaque. Origins
  inside a called body use paths under :call-body at the invoke site.

  These facts concern value flow. A consumer must still preserve execution,
  including effects unrelated to these operands, evaluation order and throws.
  Returns nil for a missing site or incomplete analysis."
  [facts path]
  (when-let [{:keys [args effects returns resolved?]} (get (:calls facts) path)]
    (let [aliases (:aliases facts)
          observed (fn [kind] (reachable (get effects kind #{}) aliases nil))
          read (observed :read)
          identity (observed :identity)
          representation (observed :representation)
          published (reachable (get effects :opaque #{}) (:escape-edges facts) nil)
          returned (reachable returns aliases nil)
          retained (reachable (reduce (fn [out origin]
                                        (into out (get (:contained facts) (:site origin))))
                                      #{} returned)
                              (:flow-edges facts) nil)]
      {:arity (count args)
       :resolved? resolved?
       :returns (resolved-origins facts returns)
       :arguments (mapv (fn [origins]
                          (let [opaque? (intersect? origins published)]
                            (cond-> #{}
                              (intersect? origins read) (conj :read)
                              (intersect? origins identity) (conj :identity)
                              (intersect? origins representation) (conj :representation)
                              opaque? (conj :publish :unknown)
                              (contains? (resolved-origins facts origins) (first unknown)) (conj :unknown)
                              (intersect? origins returned) (conj :return-alias)
                              (intersect? origins retained) (conj :return-contains))))
                        args)})))

(defn origins-at
  "Possible origins at an expression path. :argument identifies an incoming
  formal, including the rest slot. :literal identifies a potentially shared
  literal or load-time value. :allocation identifies a constructor occurrence,
  which may produce many live objects across executions. Different origins
  can alias and no origin guarantees ownership. A singleton :unknown covers
  unresolved locals, unsupported values, coerced or recur-rebound formals, and
  analysis beyond budget."
  [facts path]
  (resolved-origins facts (get (:origins facts) path unknown)))

(defn projection-at
  "A proven lookup at the expression path, or nil. :present? describes the
  key or slot and :origins describes the normal result, including a default
  on a proven miss. The proof says nothing about effects of evaluating operands."
  [facts path]
  (when-let [answer (get (:projections facts) path)]
    (update answer :origins #(resolved-origins facts %))))

(defn contained-at
  "Direct possible contents of the construction at path, or nil for an
  unmodeled site. Includes keys and members as well as values. This describes
  the constructed value, with no guarantee about subsequent mutation."
  [facts path]
  (when-let [children (get (:contained facts) path)]
    (resolved-origins facts children)))

(defn captures-at
  "A closure site's captures by live name, with the binder path and origins
  of each capture. A name from outside the analyzed root has an unknown
  origin and no binder path. Image-only constant data has no live binding."
  [facts path]
  (when-let [closure (get (:closures facts) path)]
    (reduce-kv (fn [out name capture]
                 (assoc out name (update capture :origins #(resolved-origins facts %))))
               {} (:captures closure))))

(defn may-publish?
  "Whether a value at path may become reachable outside the analyzed body,
  or its origin is unresolved. This says nothing about who already owns it."
  [facts path]
  (boolean (some (fn [origin]
                   (or (= :unknown (:kind origin)) (contains? (:published facts) origin)))
                 (origins-at facts path))))

(defn confined?
  "True when every possible origin at path is a known allocation and none
  can become reachable outside this analyzed body. A nested closure's result
  can flow to its caller, or out of the body via an exposed closure."
  [facts path]
  (and (every? (fn [origin] (= :allocation (:kind origin))) (origins-at facts path))
       (not (may-publish? facts path))))

(defn observations-at
  "Possible local observations of the expression's origins. :identity and
  :representation describe recognized uses; :unknown covers opaque uses or
  unresolved provenance. Confinement alone does not rule out observation."
  [facts path]
  (reduce
    (fn [out origin]
      (cond-> out
        (or (= :unknown (:kind origin)) (contains? (:opaque facts) origin)) (conj :unknown)
        (contains? (:identity facts) origin) (conj :identity)
        (contains? (:representation facts) origin) (conj :representation)))
    #{} (origins-at facts path)))

(defn- resolved-call? [node ns name n]
  (let [f (:fn node)]
    (and (= :invoke (:op node)) (= n (count (:args node)))
         (= :var (:op f)) (= ns (:ns f)) (= name (:name f))
         (not (or (:proto node) (:devirt-type node) (:prim-op node)))
         (not (host/protocol-call-key nil ns name n)))))

(defn- core-native-call? [node name n proc]
  (let [{:keys [arity call fixed]} (get ops/op-registry name)]
    (and (resolved-call? node "clojure.core" name n)
         (= proc (get fixed n call)) (or (nil? arity) (arity n)))))

(defn- references-binding? [facts node path binding]
  (or (= binding (binding-at facts path))
      ;; Include image captures even when no corresponding :local remains.
      (some #(= binding (:binding %)) (vals (get-in facts [:closures path :captures])))
      (ir/reduce-ir-children-with-path
        (fn [seen child p] (or seen (references-binding? facts child p binding)))
        false node path)))

(defn- private-reducer-step? [callback edit edit-path]
  (let [facts (analyze-facts callback {} {:reducer-edit-path edit-path})
        binding [:arities 0 :params 0]
        receiver (conj edit-path :args 0)]
    (and (= binding (binding-at facts receiver))
         ;; Assoc itself reads the receiver and a no-op can return that same
         ;; version. A reference retained *inside* the result is observable
         ;; in a later step, so :return-contains is disqualifying.
         (= #{:read :return-alias} (get-in (call-summary-at facts edit-path) [:arguments 0]))
         (empty? (observations-at facts receiver))
         ;; No additional reference, even in an effect-free read or in a
         ;; closure passed to an opaque transform.
         (not (references-binding? facts (get-in edit [:args 1]) (conj edit-path :args 1) binding))
         (not (references-binding? facts (get-in edit [:args 2]) (conj edit-path :args 2) binding)))))

(defn- native-map-source? [unit body path]
  (or (= :map (:op (get-in body path)))
      (let [origins (origins-at (body-facts unit body) path)]
        (and (seq origins)
             (every? (fn [{:keys [kind site]}]
                       (and (#{:literal :allocation} kind)
                            (= :map (:op (get-in body site)))))
                     origins)))))

(defn reducer-confinement-at
  "Prove private intermediate accumulator versions at a reduce-kv call in
  this IR revision. Returns nil for unsupported calls (including reduce).
  The three-argument query and unknown frame modes return nil. Pass
  :opaque-accumulator-frames only if the consumer can establish, for the
  final executable, that neither old accumulators nor their replacement can
  be obtained through live or captured frames. This covers the driver and
  assoc as well as the callback, including indirect calls during hashing or
  equality and exceptions that retain a continuation. The builder must also
  be safe across any continuation re-entry. Debug-local emission is refused
  even with this assertion.

  For example, a closed compilation can establish the condition by keeping
  the relevant frames free of visible locals and excluding access through
  raw Scheme, native interop or any other continuation inspector on the
  paths that can run with the accumulator live. Chez --opt drops naming
  information but the native inspector can still read unnamed slots.
  Gambit's empty jolt.debug frames likewise say nothing about native slot
  access. Neither a host tag, the IR optimization mode, nor the settings of
  the compiler running this analysis certify later code generation. Carry
  the obligation through deferred/cross compilation and re-establish it
  before using the proof for different code or a different target.

  A proof identifies the :step assoc occurrence. :guard is nil for a source
  proven to be a native map, or specifies the host predicate pmap? on the
  evaluated third operand. This is the exact branch in jolt-reduce-kv-walk,
  ahead of IKVReduce lookup. Map interfaces and type hints cannot prove it.
  The two call heads must reach the core intrinsics, which do not read a
  Var root. Calls through locals or other values have no such proof.

  Evaluate operands once in source order and fall back with those values
  on a failed guard. Treat the possibly pooled seed as immutable and
  return it on empty input. Preserve capture evaluation, pmap-kv-reduce
  visitation order, key/value evaluation, assoc behavior and exceptions
  without retrying a step. This callback can only return a map on normal
  completion. Callbacks returning Reduced are outside the proof. A builder
  must separately match persistent assoc's representation (including
  promotion, iteration order and metadata)."
  ([unit body path]
   (reducer-confinement-at unit body path nil))
  ([unit body path frame-access]
   (when (and (= :opaque-accumulator-frames frame-access) (not @(:debug-locals? unit)))
     (let [call (get-in body path)]
       (when (core-native-call? call "reduce-kv" 3 "jolt-reduce-kv")
         (let [[callback seed] (:args call)
               arity (first (:arities callback))
               edit-path [:arities 0 :body]
               edit (:body arity)]
           (when (and (= :map (:op seed)) (empty? (:pairs seed))
                      (ir/single-fixed-arity-fn? callback)
                      (= 3 (count (:params arity))) (uncoerced? arity)
                      (not (or (:name callback) (:once callback)))
                      (core-native-call? edit "assoc" 3 "jolt-assoc3")
                      (= :local (:op (get-in edit [:args 0])))
                      (private-reducer-step? callback edit edit-path))
             {:driver :native-map-reduce-kv
              :step (into (conj path :args 0) edit-path)
              :guard (when-not (native-map-source? unit body (conj path :args 2))
                       {:predicate "pmap?" :argument 2})})))))))
