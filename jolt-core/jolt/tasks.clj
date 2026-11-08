;;   Portions of this file are adapted from babashka's
;;   src/babashka/impl/tasks.clj (https://github.com/babashka/babashka),
;;   Copyright (c) Michiel Borkent. The task semantics follow babashka's.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

(ns jolt.tasks
  "The task runner behind `jolt <task>` / `jolt run <task>` / `jolt tasks`.

  Tasks come from a project's bb.edn or deps.edn `:tasks` map (jolt.deps merges
  both, bb.edn last) and follow babashka's semantics:

    {:tasks {:init     (def version \"1.2.3\")     ; once, before any task
             :requires ([babashka.fs :as fs])      ; for every task
             :enter    (println \"->\" (:name (current-task)))
             :leave    (println \"<-\" (:name (current-task)))

             clean {:doc \"remove build output\" :task (fs/delete-tree \"target\")}
             build {:doc \"build\" :depends [clean] :task (shell \"make\")}
             ship  {:requires ([app.release :as r]) :task (r/ship! version)}}}

  A task's value is either a map — :doc, :task (the body), :depends, :requires,
  :private, :extra-paths, :extra-deps — or the body on its own. Bodies run in
  the `user` namespace with clojure.core and babashka.tasks referred, so
  `shell`, `jolt`, `clojure`, `run` and `current-task` are available
  unqualified, and the arguments after the task name are *command-line-args*.

  Two forms are jolt's own rather than babashka's, and work in either file:
  a STRING body is a shell command line (babashka would evaluate it as an
  expression, which does nothing), and a map with :main-opts runs them like an
  alias's — `{:doc \"test\" :main-opts [\"-m\" \"app.test\"]}`.

  jolt.main injects what it owns (how to resolve the project, how to run
  :main-opts) rather than being required back — this namespace is loaded only
  when a task actually runs, and stays out of the CLI's startup closure.

  babashka.tasks is required lazily for the same reason, one level down: a
  string or :main-opts task with no hooks needs neither it nor the
  babashka.process it pulls in, and requiring it up front put half a second on
  every `jolt <task>` that only wanted to run a shell line.")

;; Task bodies are evaluated in `user`, like babashka's.
(def ^:private task-ns 'user)

;; The run in progress, for babashka.tasks/run to re-enter. One per process:
;; the CLI runs one task tree, and under :parallel every thread shares it (the
;; only mutable part is the :ran registry, an atom).
(def ^:private current-ctx (atom nil))

;; The tasks whose bodies are on the stack in THIS thread — a task that depends
;; on itself, directly or through a cycle, would otherwise wait on a delay it is
;; itself forcing.
(def ^:private ^:dynamic *in-progress* #{})

(defn tasks-of
  "The runnable entries of a :tasks map — every symbol key. The keyword keys
  (:init / :requires / :enter / :leave) configure the run, they are not tasks."
  [tasks]
  (into {} (remove (comp keyword? key)) tasks))

(defn- entry
  "The task named `name` as a map, with its :name — nil when there is none. A
  non-map value is the body: `{build (shell \"make\")}`."
  [tasks name]
  (let [sym (symbol name)]
    (when-let [v (get (tasks-of tasks) sym)]
      (if (map? v) (assoc v :name sym) {:name sym :task v}))))

;; --- the task namespace ------------------------------------------------------

(defn- eval-body [form]
  (binding [*ns* (the-ns task-ns)] (eval form)))

;; :requires land their :as aliases in the CURRENT namespace, so this has to run
;; with *ns* bound to the task namespace — a per-task :requires left to whatever
;; namespace the CLI happened to be in registered the alias somewhere else, and
;; the body then failed to resolve it ("Unknown class c").
(defn- require-all! [specs]
  (when (seq specs)
    (binding [*ns* (the-ns task-ns)]
      (doseq [spec specs] (require spec)))))

(defn- prepare-ns!
  "Create the task namespace, refer core + the babashka.tasks API into it, run
  the :tasks-level :requires, then :init. Held in the run context as a delay:
  once per invocation, on the first task that needs it, and safe to force from
  two :parallel threads at once."
  [tasks]
  (create-ns task-ns)
  (require 'babashka.tasks)
  (binding [*ns* (the-ns task-ns)]
    (refer 'clojure.core)
    (refer 'babashka.tasks :only '[shell jolt clojure run current-task]))
  (require-all! (:requires tasks))
  (when-let [init (:init tasks)] (eval-body init))
  true)

;; --- CLI tasks (:exec-fn / :cmd) ---------------------------------------------
;;
;; A task that names a handler (:exec-fn) or a command tree (:cmd) parses its
;; arguments with babashka.cli before anything runs, which is what gives it
;; --help, coercion, validation and subcommands. babashka assembles a program
;; string and hands it to SCI; jolt evaluates the task map directly, so the same
;; semantics are wired here as ordinary function calls. The reference is
;; babashka.impl.tasks at 1.13.223 — cli-node, -task-node, -cli-dispatch,
;; -run-cli-dep — and the shapes below keep its names so the two can be read
;; side by side.
;;
;; Errors here are plain ex-info, not babashka's {:babashka/exit 1}: in jolt that
;; key means "exit with this status, the failure has already reported itself",
;; which is right for a failed subprocess and wrong for a mistake in a bb.edn —
;; it would make an unresolvable :exec-fn exit 1 in silence.
;;
;; One babashka behaviour is deliberately not here: it falls back to a handler's
;; DOCSTRING for a task with no :doc, in `bb tasks` and in completion. Both of
;; those read the project's config files alone on jolt (see jolt.completions on
;; why a completing shell must not be what discovers your deps don't resolve),
;; and deriving that doc means loading the handler's namespace. A docstring still
;; reaches --help, where the fn is being resolved anyway.

;; babashka.cli on first use, like babashka.tasks above and for the same reason:
;; a :tasks map that names no :exec-fn and no :cmd parses nothing, and loading
;; the parser up front would put it on every `jolt <task>` that only wanted to
;; run a shell line.
(def ^:private cli-dispatch* (delay (requiring-resolve 'babashka.cli/dispatch)))
(def ^:private cli-merge-opts* (delay (requiring-resolve 'babashka.cli/merge-opts)))
(def ^:private cli-apply-defaults* (delay (requiring-resolve 'babashka.cli/apply-defaults)))

(defn cli-node
  "The babashka.cli dispatch node for a task, or nil when the task is a plain
  one. Naming a handler (:exec-fn) or a command tree (:cmd) is what opts a task
  in. :exec-args may sit on the task as well as under :cli, which is what an
  exec call already accepts, and the one on the task wins. Everything else
  babashka.cli takes lives under :cli, so reading a task map tells you which
  keys are jolt's and which are the parser's. Inside :cmd there is no such
  split: those are babashka.cli nodes already."
  [task-map]
  (when (and (map? task-map)
             (or (:exec-fn task-map) (:cmd task-map)))
    (select-keys task-map [:exec-fn :cmd :doc :cli :exec-args])))

(defn- task-resolve
  "requiring-resolve with *ns* on the task namespace, so a handler named through
  a :requires alias — {:requires ([app.api :as api]) :exec-fn api/serve} —
  resolves the way the task body would see it."
  [sym]
  (create-ns task-ns)
  (binding [*ns* (the-ns task-ns)]
    (requiring-resolve sym)))

(defn- resolve-or-throw
  "Resolves `sym`, reporting `what` when it names a var that is not there.
  Covers both failures: a missing var resolves to nil, a missing namespace
  throws. The original failure is kept as the cause and its message appended,
  since a bb.edn typo usually shows up as the namespace not being on the
  classpath rather than as a missing var."
  [sym what]
  (let [v (try (task-resolve sym)
               (catch :default e
                 (throw (ex-info (str what ": " (ex-message e)) {:task-cli (str sym)} e))))]
    (or v (throw (ex-info what {:task-cli (str sym)})))))

(defn- fold-fn-meta
  "Merges a handler var's :org.babashka/cli metadata into its `node`, like
  babashka's `bb -x`: namespace metadata first, then the var's own, then its
  docstring, with explicit node keys winning over all of it. A :cmd on the fn is
  dropped — command trees belong in the task map. `var-meta` is nil when the
  node holds a fn value rather than a symbol, which leaves the node as it is.

  The namespace half is inert on jolt today, where a namespace object carries no
  metadata and (meta (:ns var-meta)) is therefore nil. It is read anyway: the
  alternative is a silent difference from babashka that nothing here records."
  [var-meta node]
  (if var-meta
    (@cli-merge-opts* (:org.babashka/cli (meta (:ns var-meta)))
                      (dissoc (:org.babashka/cli var-meta) :cmd)
                      (when-let [d (:doc var-meta)] {:doc d})
                      node)
    node))

(defn- resolve-cli-opts
  "A :cli entry resolved to a map: a map as it is, or a symbol naming a def of
  one. The symbol form is what holds options a bb.edn cannot express, such as an
  :error-fn. nil when there is no entry. `what` names the entry in errors. The
  same rule applies to the runner-level :tasks {:cli ...} and to a task's own."
  [v what]
  (cond
    (nil? v)    nil
    (map? v)    v
    (symbol? v) (let [m @(resolve-or-throw v (str what " " v " cannot be resolved"))]
                  (when-not (map? m)
                    (throw (ex-info (str what " " v " is not a map") {:task-cli (str v)})))
                  m)
    :else (throw (ex-info (str what " must be a map or a symbol naming a def, got: " (pr-str v))
                          {:task-cli (pr-str v)}))))

(defn- resolve-cmd
  "A symbol :cmd resolved to the command tree it names, so a large tree can live
  in code as a def. A literal tree is kept as it is."
  [task-name cmd]
  (if (symbol? cmd)
    (let [tree @(resolve-or-throw cmd (str "Task " task-name ": :cmd " cmd " cannot be resolved"))]
      (when-not (or (map? tree) (sequential? tree))
        (throw (ex-info (str "Task " task-name ": :cmd " cmd " is not a command tree")
                        {:task (str task-name)})))
      tree)
    cmd))

(defn- task-node
  "The dispatch node for a task: the keys jolt reads (:exec-fn, :cmd, :doc) over
  the parser options its :cli resolves to. Both the invocation and the
  completion path go through here, so a task is described the same way whichever
  asks."
  [task-name node]
  (merge (resolve-cli-opts (:cli node) (str "Task " task-name ": :cli"))
         (cond-> (dissoc node :cli)
           (:cmd node) (update :cmd #(resolve-cmd task-name %)))))

(defn- map-cmd
  "Applies `f` to every command in a :cmd, keeping its shape. A vector of
  [name node] pairs is how babashka.cli takes an ordered command list, so
  rebuilding it as a map would throw that order away."
  [cmd f]
  (into (empty cmd) (map (fn [[name node]] [name (f node)])) cmd))

(defn- resolve-cli-specs
  "Walks a node tree, folding each handler's metadata into its node, for both
  :fn and :exec-fn. Used where the tree is inspected but the handlers are not
  called (--help and shell completion), so a node's spec and doc show up even
  though they live on the fn. Unlike the dispatch path this does not insist that
  a symbol resolves: a stale name should not stop the rest from being described."
  [node]
  (let [merge-fn-meta (fn [node k]
                        (let [fv (k node)]
                          (fold-fn-meta (when (symbol? fv)
                                          (try (meta (task-resolve fv)) (catch :default _ nil)))
                                        node)))
        node (-> node (merge-fn-meta :fn) (merge-fn-meta :exec-fn))]
    (cond-> node
      (:cmd node) (update :cmd map-cmd resolve-cli-specs))))

(defn- dep-node
  "A :depends task's node, ready to read a :spec off: its own :cli folded in,
  then its handler's metadata, the same two steps a target goes through.

  A handler whose namespace will not load contributes no spec rather than
  failing here: describing a task must not depend on every one of its
  dependencies being loadable. Running one still reports it."
  [task-name node]
  (let [node (task-node task-name node)]
    (try (resolve-cli-specs node)
         (catch :default _ node))))

(defn- spec-map
  "A :spec as a map. babashka.cli takes a vector of pairs too, which is how a
  spec fixes the order its options print in, and those do not merge."
  [spec]
  (cond (nil? spec) {}
        (map? spec) spec
        :else       (into {} spec)))

(defn- dep-spec
  "The merged spec of a task's CLI :depends, from their [name node] pairs. It
  goes in as the dispatch-level spec, so the same options parse, print in help
  and are offered by completion."
  [dep-nodes]
  (reduce (fn [acc [nm node]]
            (merge acc (spec-map (:spec (dep-node nm node)))))
          {} dep-nodes))

(defn- target-order
  "The transitive :depends of `name`, dependency-first and each once, with
  `name` itself last. The static graph, which is where a CLI target's
  dependencies are read from — running them is still the registry's job."
  [tasks name]
  (let [order (volatile! [])
        seen  (volatile! #{})]
    (letfn [(walk [sym path]
              (when-not (contains? @seen sym)
                (when (contains? path sym)
                  (throw (ex-info (str "circular task dependency at " sym) {:task (str sym)})))
                (doseq [d (:depends (entry tasks sym))]
                  (walk (symbol d) (conj path sym)))
                (when-not (contains? @seen sym)
                  (vswap! seen conj sym)
                  (vswap! order conj sym))))]
      (walk (symbol name) #{}))
    @order))

(defn- cli-dep-nodes
  "[name node] for every CLI task in `name`'s transitive :depends, in dependency
  order. Both the invocation and the completion path read it, so the options a
  task accepts because of its dependencies are the same set everywhere. A cyclic
  graph is reported where the task runs, not here."
  [tasks name]
  (try
    (vec (keep #(when-let [node (cli-node (get (tasks-of tasks) %))] [(str %) node])
               (butlast (target-order tasks name))))
    (catch :default _ nil)))

(defn- restrict-keys
  "The keys a dep with :restrict receives: its restrict coll, or for `true` what
  the runner-level :tasks {:cli ...} and the dep's own spec and :coerce declare,
  as a target's parse would. :exec-args keys are always kept — babashka.cli does
  not restrict those either."
  [defaults node]
  (let [restrict (:restrict node)]
    (set (concat (if (true? restrict)
                   (mapcat #(concat (keys (spec-map (:spec %))) (keys (:coerce %)))
                           [defaults node])
                   restrict)
                 (keys (:exec-args defaults))
                 (keys (:exec-args node))))))

(defn- run-cli-dep!
  "Calls the handler of a CLI task named in :depends with the map `jolt <dep>`
  would build from the same command line: the options `opts` carries from the
  target's parse, over its own and the runner-level defaults. With :restrict,
  its own or else the runner-level one, it gets only the keys it declares.
  `defaults` is the runner-level :tasks {:cli ...} entry. Called in the dep's own
  place in the walk, so it keeps its position in the graph and its own :depends
  have run. A plain target parses nothing, so its CLI dependencies are handed {}
  and get their own defaults alone."
  [node task-name defaults opts]
  (let [node (dep-node task-name node)]
    (when-let [f (:exec-fn node)]
      ((if (symbol? f)
         @(resolve-or-throw f (str "Task " task-name ": cannot resolve :exec-fn " f))
         f)
       (let [defaults (resolve-cli-opts defaults ":tasks :cli")
             own-defaults #(-> (select-keys % [:spec :exec-args]) (update :spec spec-map))
             m (@cli-apply-defaults*
                ;; only what the command line actually supplied: a target's
                ;; defaults are its own, and handing them on would make every
                ;; dep inherit options it never declared
                (select-keys opts (:supplied (:org.babashka/cli (meta opts))))
                (@cli-merge-opts* (own-defaults defaults) (own-defaults node)))
             restrict (if (contains? node :restrict) (:restrict node) (:restrict defaults))]
         (if restrict
           (select-keys m (restrict-keys defaults (assoc node :restrict restrict)))
           m))))))

(defn- cli-dispatch!
  "Runs babashka.cli/dispatch over a task's node. A :fn / :exec-fn symbol is
  resolved in the task namespace and the var's metadata folded into its node, so
  specs and help live with the fn.

  `fns` holds what must not run until the parser picks a command: :body-fn (the
  task's own :task body), :deps-fn (its :depends walk) and :hook-fn (the
  :enter / :leave pair around the call), each nil when absent. That is what
  makes --help free of side effects: it prints from the tree and returns, and
  the dependencies of a task nobody ran do not run.

  `defaults` is the runner-level :tasks {:cli ...} entry. `dep-nodes` are the
  [name node] pairs of the CLI :depends tasks: their specs merge under this
  task's own, so one parse covers everything the invocation can consume. The
  handlers themselves are called from the :depends walk, in their own place in
  the graph. A dep never parses; its :restrict only narrows what it receives."
  [cli-opts task-name {:keys [body-fn deps-fn hook-fn]} defaults dep-nodes args]
  (let [;; the task's own :cli also provides dispatch opts, for options that
        ;; only exist there, such as an :error-fn
        task-cli (resolve-cli-opts (:cli cli-opts) (str "Task " task-name ": :cli"))
        cli-opts (task-node task-name cli-opts)
        dspec    (dep-spec dep-nodes)
        own-spec (fn [m]
                   (cond-> (select-keys m [:spec :exec-args])
                     (and (seq dspec) (:spec m)) (update :spec spec-map)))
        ;; babashka.cli hands an :exec-fn the parsed options and a :fn the whole
        ;; dispatch result. A :depends handler always wants the options, so
        ;; unwrap where the handler being wrapped is a :fn
        with-deps (fn [f dispatch-shape?]
                    (fn [m]
                      (when deps-fn (deps-fn (if dispatch-shape? (:opts m) m)))
                      (f m)))
        with-hooks (fn [f] (if hook-fn (fn [m] (hook-fn (fn [] (f m)))) f))
        ;; resolve a :fn / :exec-fn symbol, fold in the fn's metadata and gate
        ;; :depends and :enter/:leave on the fn being called
        wrap-key (fn [node k]
                   (if-let [fv (k node)]
                     (let [handler (if (symbol? fv)
                                     @(resolve-or-throw
                                       fv (str "Task " task-name ": cannot resolve " k " " fv))
                                     fv)]
                       (-> (fold-fn-meta (when (symbol? fv)
                                           (meta (task-resolve fv)))
                                         node)
                           (assoc k (with-deps (with-hooks handler) (= :fn k)))))
                     node))
        wrap (fn wrap [node]
               (let [node (-> node (wrap-key :fn) (wrap-key :exec-fn))]
                 (cond-> node
                   (:cmd node) (update :cmd map-cmd wrap))))
        tree (wrap cli-opts)
        ;; the task's own body becomes the tree's root :fn, so a task may have
        ;; both a :cmd tree and a body to run when no command is named
        tree (if body-fn (assoc tree :fn (with-deps body-fn true)) tree)
        ;; a :cli entry in the :tasks map (like :requires / :init) provides
        ;; dispatch defaults for every CLI task; node keys win. :prog stays
        ;; jolt's, so help always names the task it belongs to.
        defaults (resolve-cli-opts defaults ":tasks :cli")]
    (@cli-dispatch* tree args (merge {:help true}
                                     defaults
                                     task-cli
                                     (@cli-merge-opts*
                                      (when (seq dspec) {:spec dspec})
                                      (own-spec defaults)
                                      (own-spec task-cli))
                                     {:prog (str "jolt " task-name)}))))

;; Does running this task involve evaluating Clojure in the task namespace? A
;; code body or a per-task :requires does; so does any :tasks-level hook, which
;; runs around every task including a shell one. A string or :main-opts task in
;; a :tasks map with no hooks does not, and takes neither the namespace setup
;; nor the babashka.tasks load.
(defn- needs-task-ns? [tasks t]
  (boolean (or (:requires t)
               (and (contains? t :task) (not (string? (:task t))))
               ;; a CLI task resolves its handler in the task namespace, through
               ;; that namespace's aliases, before it can parse anything
               (cli-node t)
               (:init tasks) (:requires tasks) (:enter tasks) (:leave tasks))))

;; --- running -----------------------------------------------------------------

(defn- shell-line!
  "A string body: one shell command line, run by the shell (so pipes, globs and
  && work), failing the task with its exit status."
  [cmd]
  (let [status (jolt.host/sh cmd)]
    (when-not (and (integer? status) (zero? status))
      (throw (ex-info (str "task exited with " status)
                      {:babashka/exit (if (integer? status) status 1) :cmd cmd})))))

(defn- run-body! [ctx t]
  (cond
    (contains? t :main-opts) ((:run-main-opts ctx) (:main-opts t) (:args ctx))
    (string? (:task t))      (shell-line! (:task t))
    (contains? t :task)      (eval-body (:task t))
    :else (throw (ex-info (str "task " (:name t) " has no :task body") {:task (:name t)}))))

(declare ensure-run!)

(defn- run-deps!
  "The :depends walk — each dependency at most once per invocation. :parallel
  runs the independent ones concurrently; the registry in ensure-run! is what
  keeps a shared dependency from running twice when it does."
  [ctx deps]
  (when (seq deps)
    (if (:parallel ctx)
      (doseq [f (doall (map (fn [d] (future (ensure-run! ctx d false))) deps))] @f)
      (doseq [d deps] (ensure-run! ctx d false)))))

(defn- run-cli-target!
  "A target task that parses. Its arguments go through babashka.cli first, and
  what runs afterwards is whatever the parser picked: the :depends walk, then
  :enter, the handler (or the body, for a task that has one), then :leave.

  Handing those over as thunks is what makes --help free of side effects — it
  prints from the tree and returns, so the dependencies of a task nobody ran do
  not run, which is the whole reason the walk moved in here."
  [ctx t node deps]
  (let [tasks     (:tasks ctx)
        task-name (str (:name t))
        dep-nodes (cli-dep-nodes tasks (:name t))]
    ;; a :cmd task is a tree of commands, not one thing to run, so naming one in
    ;; :depends cannot mean anything — refused rather than picking a command for
    ;; the project (babashka 1.13.221)
    (when-let [bad (some (fn [nm]
                           (let [d (get (tasks-of tasks) (symbol nm))]
                             (when (and (:cmd d) (not (:task d))) nm)))
                         (map first dep-nodes))]
      (throw (ex-info (str "Task " task-name ": :depends cannot name " bad
                           ", a :cmd task has no single handler to run")
                      {:task task-name})))
    (cli-dispatch!
     node task-name
     {:body-fn (when (or (contains? t :task) (contains? t :main-opts))
                 (fn [_] (run-body! ctx t)))
      :deps-fn (when (seq deps)
                 ;; the parsed options travel to the CLI dependencies in the ctx
                 ;; rather than in a dynamic binding: under :parallel the walk
                 ;; runs in futures, and the ctx is the one thing every branch
                 ;; of it is already handed
                 (fn [opts] (run-deps! (assoc ctx :dep-opts opts) deps)))
      :hook-fn (when (or (:enter tasks) (:leave tasks))
                 (fn [thunk]
                   (when-let [h (:enter tasks)] (eval-body h))
                   (let [v (thunk)]
                     (when-let [h (:leave tasks)] (eval-body h))
                     v)))}
     (:cli tasks) dep-nodes (vec (:app-args ctx)))))

(defn- run-one! [ctx sym target?]
  (let [tasks (:tasks ctx)
        t (or (entry tasks sym)
              (throw (ex-info (str "unknown command or task: " sym " (see 'jolt tasks')")
                              {:name (str sym)})))
        deps (:depends t)
        node (cli-node t)]
    (if-not (needs-task-ns? tasks t)
      (do (run-deps! ctx deps)
          (run-body! ctx t))
      (do
        @(:prepare ctx)
        (with-bindings {(requiring-resolve 'babashka.tasks/*task*) t}
          (require-all! (:requires t))
          (if (and node target?)
            ;; the parser owns the order from here: see run-cli-target!
            (run-cli-target! ctx t node deps)
            (do
              (run-deps! ctx deps)
              (when-let [h (:enter tasks)] (eval-body h))
              ;; a CLI task may be nothing but an :exec-fn, so a missing body is
              ;; only an error for a plain one
              (if node
                (when (or (contains? t :task) (contains? t :main-opts)) (run-body! ctx t))
                (run-body! ctx t))
              ;; reached through :depends, a CLI task does not parse: its handler
              ;; is called with what the target's parse produced
              (when node (run-cli-dep! node (str sym) (:cli tasks) (:dep-opts ctx {})))
              (when-let [h (:leave tasks)] (eval-body h)))))))))

(defn- ensure-run!
  "Run `name` unless this invocation already has. The registry holds a delay per
  task, so two :depends edges onto the same task run it once even when they are
  being walked by different threads. `target?` is what the invocation asked for
  by name, as opposed to what its :depends reached: only a target parses."
  [ctx name target?]
  (let [sym (symbol name)]
    (when (contains? *in-progress* sym)
      (throw (ex-info (str "circular task dependency at " sym) {:task (str sym)})))
    (let [d (delay (binding [*in-progress* (conj *in-progress* sym)] (run-one! ctx sym target?)))]
      @(get (swap! (:ran ctx) #(if (contains? % sym) % (assoc % sym d))) sym))))

(defn run-nested!
  "babashka.tasks/run: run a task from inside another one. The named task always
  runs (that is what the call asked for); its :depends still honour the
  invocation's already-ran registry."
  [name opts]
  (let [sym (symbol name)
        ctx (or @current-ctx
                (throw (ex-info "run called outside a task" {:task (str sym)})))]
    (when (contains? *in-progress* sym)
      (throw (ex-info (str "circular task dependency at " sym) {:task (str sym)})))
    (binding [*in-progress* (conj *in-progress* sym)]
      ;; a task run by name from inside another is a target of its own: a CLI
      ;; task reached this way parses, as `jolt <task>` would
      (run-one! (merge ctx (select-keys opts [:parallel])) sym true))))

(defn- exit-code
  "The exit status a thrown task failure should become, or nil for an error that
  is not one — a failed subprocess (babashka.process/check's ex-data carries
  :cmd and :exit) or an explicit :babashka/exit. Anything else is a program
  error and belongs in the uncaught handler's report, with a stack trace.

  The cause chain, not just the throw: a :depends that failed under :parallel
  arrives wrapped by the future's deref, and reading only the top ex-data lost
  the child's status there (the task reported 1 with a stack trace over it)."
  [e]
  (loop [e e]
    (when e
      (let [d (ex-data e)
            code (or (:babashka/exit d)
                     (when (and (:cmd d) (integer? (:exit d)) (not (zero? (:exit d))))
                       (:exit d)))]
        (if code code (recur (ex-cause e)))))))

(defn- check-depends-acyclic!
  "Walk the static :depends graph reachable from `name` and raise on a cycle
  BEFORE anything runs. *in-progress* catches a cycle the sequential walk enters,
  but it is per-thread and conveyed down each branch: under :parallel two sibling
  branches force each other's delay, neither one's *in-progress* holds the other's
  task, and the run DEADLOCKED with no output where the sequential one reported.
  :depends is static data, so the graph is where that question is actually
  answerable. An unknown name has no edges here — run-one! still reports it."
  [tasks name]
  (letfn [(walk [sym path seen]
            (cond
              (contains? path sym)
              (throw (ex-info (str "circular task dependency at " sym) {:task (str sym)}))
              (contains? seen sym) seen
              :else
              (conj (reduce (fn [s d] (walk (symbol d) (conj path sym) s))
                            seen
                            (:depends (entry tasks sym)))
                    sym)))]
    (walk (symbol name) #{} #{})
    nil))

(defn run-task!
  "Run one task tree. `ctx` is
  {:tasks :name :args :app-args :run-main-opts :parallel} — :args is the argv
  after the task name verbatim (what a :main-opts task forwards), :app-args the
  same with a leading \"--\" consumed (what a code body sees as
  *command-line-args*).

  A failed subprocess exits with ITS status and prints nothing further — the
  child already reported, and a jolt stack trace over it would only bury that.
  Every other exception propagates to the CLI's uncaught handler."
  [{:keys [tasks name app-args] :as ctx}]
  (let [ctx (assoc ctx :ran (atom {}) :prepare (delay (prepare-ns! tasks)))]
    (reset! current-ctx ctx)
    (binding [*command-line-args* (seq app-args)]
      (try
        (check-depends-acyclic! tasks name)
        (ensure-run! ctx (symbol name) true)
        (catch :default e
          (if-let [code (exit-code e)]
            (System/exit code)
            (throw e)))))))

;; --- the listing -------------------------------------------------------------

(defn- first-line
  "Everything before the first newline. A listing is one task per line, so a
  docstring that spans lines has to be cut to fit it — see list-tasks!."
  [s]
  (apply str (take-while #(not= \newline %) s)))

(defn listable
  "The entries a listing offers, sorted by name. Two kinds of task stay out,
  both of them helpers for other tasks rather than things to pick off a list:
  a :private one, and one whose name starts with `-`, which is babashka's
  spelling of the same intent and reaches us through the bb.edn files we read.
  Neither is unrunnable — `jolt -dash` works — they are only unlisted.

  `jolt tasks` and shell completion both go through here, so that what a person
  is shown and what TAB offers cannot drift apart."
  [tasks]
  (->> (tasks-of tasks)
       (remove (fn [[k v]]
                 (or (and (map? v) (:private v))
                     (= \- (first (str k))))))
       (sort-by (comp str key))))

(defn doc-line
  "The one line of :doc a listing shows for a task's value, or nil."
  [v]
  (when-let [d (and (map? v) (:doc v))]
    (first-line d)))

(defn list-tasks!
  "`jolt tasks` — the project's task names and their :doc, babashka's listing.
  What is listed and what is hidden is `listable`. Only the first line of a
  :doc is printed, because the listing's whole shape is one task per line: a
  docstring's second line would otherwise sit in the name column and read as a
  task of its own."
  [tasks]
  (let [entries (listable tasks)]
    (if (empty? entries)
      (println "No tasks found. Add a :tasks map to bb.edn or deps.edn.")
      (let [w (apply max (map (comp count str key) entries))]
        (println "The following tasks are available:")
        (println)
        (doseq [[k v] entries]
          (let [doc (doc-line v)]
            (println (if doc
                       (str (apply str k (repeat (- w (count (str k))) \space))
                            "  " doc)
                       (str k)))))))))

;; --- shell completion for a CLI task -----------------------------------------

(defn- with-dep-spec
  "Adds the merged spec of a task's CLI :depends to dispatch `opts` as the
  dispatch-level :spec, under whatever the task already declares there."
  [opts dep-nodes]
  (let [spec (dep-spec dep-nodes)]
    (cond-> opts
      (seq spec) (update :spec #(merge spec (spec-map %))))))

(defn- prepare-cli-ns!
  "The task namespace as a describing caller needs it: created, referring
  clojure.core, with the runner-level and the task's own :requires run, so a
  handler named through an alias resolves. :init is NOT run — describing a task
  must not evaluate a project's setup code."
  [tasks t]
  (create-ns task-ns)
  (binding [*ns* (the-ns task-ns)] (refer 'clojure.core))
  (require-all! (:requires tasks))
  (require-all! (:requires t)))

(defn complete!
  "The `org.babashka.cli/completions complete` half of babashka's hidden
  completion command: print what the task at the head of `toks` accepts, given
  the tokens of the command line before the cursor.

  Only a CLI task has options to offer. Anything else prints babashka.cli's
  file-completion marker, which is what tells a stub to fall back to the shell's
  own file completion — jolt's snippets complete the task NAME themselves, from
  the cached `jolt completions tasks` lines, and only call back for a task that
  those lines marked as one that parses.

  Nothing here may throw or print anything else: a completing shell discards
  stderr, so a failure would show up as 'no candidates' while also suppressing
  the file-completion fallback. The tree is built inside the guard too, since a
  stale task map is exactly the case where resolving a handler fails."
  [tasks toks shell]
  (let [[run & task-args] toks
        t    (and run (get (tasks-of tasks) (symbol run)))
        node (cli-node t)]
    (or (when node
          (try
            (let [built (volatile! nil)]
              ;; loading a namespace may print, and a banner on stdout would be
              ;; offered to the shell as a candidate
              (with-out-str
                (prepare-cli-ns! tasks t)
                (let [dep-nodes (cli-dep-nodes tasks run)]
                  (vreset! built
                           [(resolve-cli-specs (task-node run node))
                            (with-dep-spec (merge {:help true}
                                                  (resolve-cli-opts (:cli tasks) ":tasks :cli")
                                                  {:prog (str "jolt " run)})
                                           dep-nodes)])))
              (let [[tree opts] @built]
                (@cli-dispatch* tree
                                (into ["org.babashka.cli/completions" "complete"
                                       "--shell" (str shell) "--"]
                                      task-args)
                                opts))
              true)
            (catch :default _ nil)))
        (println "org.babashka.cli/file-completion"))))
