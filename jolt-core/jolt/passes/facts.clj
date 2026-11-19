(ns jolt.passes.facts
  "Lexical bindings, value origins, and reachability for one IR body revision."
  (:require [jolt.analyzer.tree :refer [arity-params]]
            [jolt.ir :as ir]))

(def ^:private max-nodes 10000)
(def ^:private max-path-length 192)
(def ^:private max-origins 256)
(def ^:private unknown #{{:kind :unknown}})

(defn- site [kind path] #{{:kind kind :site path}})

(defn- incomplete! []
  (throw (ex-info "IR facts analysis incomplete" {::incomplete true})))

(defn- join-origins [left right]
  (let [joined (into left right)]
    (when (> (count joined) max-origins) (incomplete!))
    joined))

(defn- opaque! [state origins]
  (swap! (:opaque state) into origins))

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
              (when (or loop? (and recursive? (not= :fn (:op init))))
                ;; Rebound and non-fn letrec slots lose provenance.
                (opaque! state origins))
              (if recursive?
                env
                (bind-local! state env name (conj p 0) (if loop? :loop :let) origins))))
          env bindings)
        ;; Loop inits are sequential, just like let. At the loop body, the
        ;; same binders may hold values from recur instead of their inits.
        body-env (if loop?
                   (reduce-kv (fn [env i [name _]]
                                (assoc env name [(conj path :bindings i 0) nil (:fn-path state)]))
                              body-env bindings)
                   body-env)]
    (walk state (:body node) (conj path :body) body-env (when-not loop? recur-params))))

(defn- walk-arity [state arity path env]
  (let [fixed-count (count (:params arity))
        numeric-hints (into {} (:nhints arity))
        [arity-env params]
        (reduce-kv
          (fn [[env params] j name]
            (let [rest? (= j fixed-count)
                  binder (if rest? (conj path :rest) (conj path :params j))]
              [(bind-local! state env name binder (if rest? :rest :param)
                            ;; Entry coercion can replace the caller's value.
                            (when-not (and (not rest?)
                                           (contains? #{:long :double} (get numeric-hints name)))
                              #{{:kind :argument :binding binder}}))
               (conj params binder)]))
          [env []] (arity-params arity))
        origins (walk state (:body arity) (conj path :body) arity-env params)]
    ;; Coercion may return its input, convert it, or name its class in a
    ;; cast exception. Preserve all three possibilities for downstream queries.
    (if (contains? #{:long :double} (:ret-nhint arity))
      (do (swap! (:representation state) into origins)
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
        inner (assoc state :fn-path path :captures (atom {}))
        body-env (once-body-env node env)
        self-env (if-let [name (:name node)]
                   (bind-local! inner body-env name (conj path :name) :self origins)
                   body-env)
        returns (reduce-kv
                  (fn [returns i arity]
                    (join-origins returns
                                  (walk-arity inner arity (conj path :arities i) self-env)))
                  #{} (:arities node))]
    ;; The image's live names describe the spliced closure. A vector entry
    ;; carries literal data, with no lexical binding. The body walk also sees
    ;; captures in generated or rewritten IR without image registration.
    (doseq [name (or (:live-names node) (:free-names node)) :when (string? name)]
      (note-capture! inner name (get env name)))
    (let [captures @(:captures inner)]
      (doseq [name (keys captures)] (note-capture! state name (get env name)))
      (swap! (:closures state) assoc path
             {:captures captures :returns returns
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
        slots (reduce-kv (fn [slots i k]
                           (assoc slots k {:index i :origins (nth stored i)}))
                         {} (:fields shape))]
    (swap! (:containers state) assoc path
           {:kind :record :slots slots :arity (count args)
            :children (reduce join-origins #{} stored)})
    (site :allocation path)))

(defn- projection-slots [mode k kind slots]
  (case kind
    :vector (when (and (int? k) (<= 0 k 2147483647)) slots)
    (:map :set) (when (= mode :get) slots)
    nil))

(defn- projection [state mode receiver k fallback]
  (reduce
    (fn [result origin]
      (let [{:keys [kind slots]} (get @(:containers state) (:site origin))
            table (projection-slots mode k kind slots)
            entry (find table k)
            present? (some? entry)]
        (if (and table (or entry fallback) (or (nil? result) (= present? (:present? result))))
          {:present? present?
           :origins (join-origins (:origins result #{}) (if entry (val entry) fallback))}
          (reduced nil))))
    nil receiver))

(defn- note-guarded-record! [state path receiver-path k]
  ;; A direct positional construction is fresh even when argument/default
  ;; evaluation runs user code. Through an alias a field could have changed.
  (let [{:keys [kind slots arity]} (get @(:containers state) receiver-path)]
    (when (and (= kind :record) (not= k :jolt/deftype))
      (when-let [{:keys [index origins]} (get slots k)]
        (swap! (:guarded-records state) assoc path
               {:guard {:key k :index index :arity arity} :origins origins})))))

(defn- projection-call [state path mode receiver-path key-node fallback]
  (when (literal-key? key-node)
    (let [k (:val key-node)]
      (if-let [answer (projection state mode (child-origins state receiver-path) k fallback)]
        (do (swap! (:projections state) assoc path answer)
            (:origins answer))
        (when (= mode :get)
          (note-guarded-record! state path receiver-path k)
          nil)))))

(defn- invoke-origins [state node path]
  (let [f (:fn node)
        args (:args node)
        n (count args)
        arg (fn [i] (child-origins state (conj path :args i)))
        callee (child-origins state (conj path :fn))
        core (when (and (= :var (:op f)) (= "clojure.core" (:ns f))) (:name f))
        default (fn [i implicit] (if (> n i) (arg i) implicit))
        absent (site :literal path)
        closure (when (and (= 0 n) (= 1 (count callee)))
                  (get @(:closures state) (:site (first callee))))]
    (cond
      (:zero-arity? closure) (:returns closure)
      ;; These exact call heads and arities lower directly to native ops.
      (and (= core "identical?") (= n 2))
      (do (swap! (:identity state) into (into (arg 0) (arg 1))) absent)
      (and (= core "identity") (= n 1)) (arg 0)
      (and (#{"get" "nth"} core) (<= 2 n 3))
      (projection-call state path (if (= core "get") :get :nth)
                       (conj path :args 0) (second args) (default 2 (when (= core "get") absent)))
      (and (= :const (:op f)) (keyword? (:val f)) (<= 1 n 2))
      (projection-call state path :get (conj path :args 0) f (default 1 absent))
      (and (= :map (:op f)) (<= 1 n 2))
      (projection-call state path :get (conj path :fn) (first args) (default 1 absent))
      (and (#{:vector :set} (:op f)) (= n 1))
      (projection-call state path (if (= :vector (:op f)) :nth :get)
                       (conj path :fn) (first args) (when (= :set (:op f)) absent))
      (and (#{"class" "type"} core) (= n 1))
      (do (swap! (:representation state) into (arg 0)) nil)
      :else nil)))

(defn- walk [state node path env recur-params]
  (when (or (> (count path) max-path-length)
            (neg? (swap! (:remaining state) dec)))
    (incomplete!))
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
              :if (join-origins (child :then) (child :else))
              :do (child :ret)
              :invoke (or (invoke-origins state node path) (opaque-children))
              :host-new (if-let [shape (record-constructor state node)]
                          (walk-record state path shape
                                       (mapv #(child-origins state (conj path :args %))
                                             (range (count (:args node)))))
                          (opaque-children))
              :recur (do (when (seq recur-params)
                           (swap! (:recurrent state) into recur-params))
                         (opaque-children))
              ;; Stores, throws (including locally caught ones), host/FFI
              ;; calls, and any other unmodeled operation can leak their operands.
              (opaque-children))))]
    (swap! (:origins state) assoc path origins)
    origins))

(defn- captures-origins [closure]
  (reduce join-origins #{} (map :origins (vals (:captures closure)))))

(defn- reachable [roots edges]
  (loop [seen roots pending (vec roots)]
    (if-let [origin (peek pending)]
      (let [children (remove seen (get edges (:site origin)))]
        (recur (into seen children) (into (pop pending) children)))
      seen)))

(defn- analyze-body [body record-classes]
  (let [state {:bindings (atom {}) :uses (atom {}) :origins (atom {})
               :recurrent (atom #{}) :remaining (atom max-nodes)
               :containers (atom {}) :closures (atom {}) :projections (atom {})
               :opaque (atom #{}) :identity (atom #{}) :representation (atom #{})
               :guarded-records (atom {}) :record-classes record-classes}
        result (walk state body [] {} nil)
        closures @(:closures state)
        contained (into {}
                        (map (fn [[p c]] [p (captures-origins c)])) closures)
        contained (into contained (map (fn [[p c]] [p (:children c)])) @(:containers state))
        ;; An exposed procedure could be invoked and leak its result. Keep
        ;; those results off the containment graph used by clients.
        edges (reduce-kv (fn [edges p c] (update edges p join-origins (:returns c)))
                         contained closures)
        opaque (reachable @(:opaque state) edges)
        published (reachable (into opaque result) edges)]
    {:bindings @(:bindings state) :uses @(:uses state) :origins @(:origins state)
     :recurrent @(:recurrent state)
     :contained contained :closures closures :projections @(:projections state)
     :guarded-records @(:guarded-records state)
     :published published :opaque opaque :identity @(:identity state)
     :representation @(:representation state)}))

(defn body-facts
  "Return facts for body in the given compilation unit. Request again after
  rewriting IR. The unit caches only its latest root and record registry
  snapshot, compared by identity. Debug-local frames can publish any live
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
      (let [classes (reduce (fn [classes shape]
                              (let [class (:type shape)]
                                ;; Dotted record names can share a runtime class
                                ;; tag. The matching constructor is ambiguous.
                                (assoc classes class (when-not (contains? classes class) shape))))
                            {} (vals shapes))
            facts (try
                    (analyze-body body classes)
                    (catch clojure.lang.ExceptionInfo e
                      ;; Unsupported layouts and exhausted budgets can hide uses
                      ;; of even the sites we already walked.
                      (if (::incomplete (ex-data e)) {} (throw e))))]
        (reset! cache [body shapes facts])
        facts))))

(defn binding-at
  "The binder path for a local use, or nil. The :bindings table describes those
  binder paths. For an alias, query its initializer to find the source binder."
  [facts path]
  (get (:uses facts) path))

(defn- resolved-origins [facts origins]
  (if (some (fn [origin] (contains? (:recurrent facts) (:binding origin))) origins)
    (into #{} (map (fn [origin]
                     (if (contains? (:recurrent facts) (:binding origin))
                       (first unknown)
                       origin))) origins)
    origins))

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

(defn guarded-record-projection-at
  "A conditional field hit for a directly constructed positional record.
  Use only with the matching unit's positional constructor emission. After
  evaluating operands once, check the actual receiver's record and descriptor
  arities against :arity and its effective lookup index for :key (including
  lookup masking) against :index. The slot then has :origins. Otherwise run
  ordinary lookup on the evaluated operands. A class name or registry snapshot
  alone cannot validate the layout. Unconditional facts include the fallback."
  [facts path]
  (when-let [answer (get (:guarded-records facts) path)]
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
