(ns jolt.passes.facts
  "Lexical bindings and value origins for one revision of an IR body."
  (:require [jolt.analyzer.tree :refer [arity-params]]
            [jolt.ir :as ir]))

(def ^:private max-nodes 10000)
(def ^:private max-path-length 192)
(def ^:private unknown #{{:kind :unknown}})

(defn- site [kind path] #{{:kind kind :site path}})

;; Scope maps printed names to [binder path, value origins]. Neither the
;; bindings nor their uses are keyed by IR maps: two children can share the
;; very same node object after substitution and still be different occurrences.
(defn- bind-local! [state env name path kind origins]
  (swap! (:bindings state) assoc path {:name name :kind kind})
  (assoc env name [path origins]))

(declare walk)

(defn- walk-local [state node path env]
  (when-let [[binder origins] (get env (:name node))]
    (swap! (:uses state) assoc path binder)
    origins))

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
              (if recursive?
                env
                (bind-local! state env name (conj p 0) (if loop? :loop :let) origins))))
          env bindings)
        ;; Loop inits are sequential, just like let. At the loop body, the
        ;; same binders may hold values from recur instead of their inits.
        body-env (if loop?
                   (reduce-kv (fn [env i [name _]]
                                (assoc env name [(conj path :bindings i 0) nil]))
                              body-env bindings)
                   body-env)]
    (walk state (:body node) (conj path :body) body-env (when-not loop? recur-params))))

(defn- walk-fn [state node path env]
  (let [origins (site :allocation path)
        env (if-let [name (:name node)]
              (bind-local! state env name (conj path :name) :self origins)
              env)]
    (reduce-kv
      (fn [_ i arity]
        (let [p (conj path :arities i)
              fixed-count (count (:params arity))
              [arity-env params]
              (reduce-kv
                (fn [[env params] j name]
                  (let [rest? (= j fixed-count)
                        binder (if rest? (conj p :rest) (conj p :params j))]
                    [(bind-local! state env name binder (if rest? :rest :param)
                                  #{{:kind :argument :binding binder}})
                     (conj params binder)]))
                [env []] (arity-params arity))]
          (walk state (:body arity) (conj p :body) arity-env params)))
      nil (:arities node))
    origins))

(defn- walk-try [state node path env recur-params]
  (walk state (:body node) (conj path :body) env recur-params)
  (when-let [catch-body (:catch-body node)]
    (let [env (if-let [name (:catch-sym node)]
                (bind-local! state env name (conj path :catch-sym) :catch nil)
                env)]
      (walk state catch-body (conj path :catch-body) env recur-params)))
  (when-let [finally (:finally node)]
    (walk state finally (conj path :finally) env recur-params))
  nil)

(defn- incomplete! []
  (throw (ex-info "IR facts analysis incomplete" {::incomplete true})))

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
          (do
            (when-not (contains? ir/node-ops op) (incomplete!))
            (ir/reduce-ir-children-with-path
              (fn [_ child p] (walk state child p env recur-params)) nil node path)
            (case op
              (:const :quote :regex :load-time-value) (site :literal path)
              (:vector :map :set) (site (if (ir/const-coll-node? node) :literal :allocation) path)
              :recur (do (when (seq recur-params)
                           (swap! (:recurrent state) into recur-params))
                         nil)
              nil)))]
    (when origins (swap! (:origins state) assoc path origins))
    origins))

(defn- analyze-body [body]
  (let [state {:bindings (atom {}) :uses (atom {}) :origins (atom {})
               :recurrent (atom #{}) :remaining (atom max-nodes)}]
    (try
      (walk state body [] {} nil)
      {:bindings @(:bindings state) :uses @(:uses state) :origins @(:origins state)
       :recurrent @(:recurrent state)}
      (catch clojure.lang.ExceptionInfo e
        ;; An unknown op has no known child layout. Like an exhausted budget,
        ;; that can hide recur and make even a previously walked param unsound.
        (if (::incomplete (ex-data e)) {} (throw e))))))

(defn body-facts
  "Return facts for body in the given compilation unit. Request again after
  rewriting IR. The unit caches only its latest root, compared by identity.
  All binding and site paths are relative to this snapshot. Include the
  enclosing fn/let/etc. to resolve its bindings."
  [unit body]
  (let [cache (:body-facts unit)
        previous @cache]
    (if (and previous (identical? body (nth previous 0)))
      (nth previous 1)
      (let [facts (analyze-body body)]
        (reset! cache [body facts])
        facts))))

(defn binding-at
  "The binder path for a local use, or nil. The :bindings table describes those
  binder paths. For an alias, query its initializer to find the source binder."
  [facts path]
  (get (:uses facts) path))

(defn origins-at
  "Possible origins at an expression path. :argument identifies an incoming
  formal, including the rest slot. :literal identifies a potentially shared
  literal or load-time value. :allocation identifies a constructor occurrence,
  which may produce many live objects across executions. Different origins
  can alias and no origin guarantees ownership. A singleton :unknown covers
  unresolved locals, unsupported values, formals rebound by recur, and
  analysis beyond budget."
  [facts path]
  (let [origins (get (:origins facts) path unknown)]
    (if (some (fn [origin]
                (and (= :argument (:kind origin))
                     (contains? (:recurrent facts) (:binding origin))))
              origins)
      unknown
      origins)))
