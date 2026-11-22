(ns babashka.tasks
  "The API a bb.edn (or deps.edn) `:tasks` body is written against — babashka's
  task namespace, on jolt.

  Every task body runs with these referred, so a task calls them unqualified:

    {:tasks {build {:task (shell \"cc -o app app.c\")}
             all   {:depends [build] :task (run 'test)}}}

  `shell` is babashka.process/shell — it inherits stdio and throws on a
  non-zero exit, and the runner turns that throw into jolt's own exit status.

  `jolt` re-invokes this CLI. `clojure` is the same function under babashka's
  name for it, so a bb.edn that calls `(clojure \"-M:test\")` runs here — on
  this host jolt IS the Clojure, and the point of the exercise is not to need a
  JVM. Write `jolt` in new task maps; it says what it does. Shell out
  explicitly — `(shell \"clojure\" \"-M:test\")` — to reach the real
  Clojure CLI.

  jolt.tasks is the runner; this namespace is only the surface it binds.

  babashka.process is resolved on first use rather than required here: a task
  namespace refers these whether or not the task shells out, and loading the
  process layer for a task body that only prints cost half a second on every
  run.")

;; babashka.process, on first call (see the namespace docstring).
(def ^:private p-shell (delay (requiring-resolve 'babashka.process/shell)))
(def ^:private p-tokenize (delay (requiring-resolve 'babashka.process/tokenize)))

;; The task map currently running — {:name sym :doc str :task form …}, exactly
;; the entry from the :tasks map plus its :name. Bound by jolt.tasks around
;; every body, :enter and :leave included.
(def ^:dynamic *task* nil)

(defn current-task
  "The task map currently running, or nil outside a task."
  []
  *task*)

(defn shell
  "Run a command, inheriting stdin/stdout/stderr, and throw when it exits
  non-zero (`{:continue true}` in a leading options map suppresses the throw).
  The first argument is tokenized, so both `(shell \"ls -la\")` and
  `(shell \"ls\" \"-la\")` work. babashka.process/shell verbatim — see
  https://github.com/babashka/process."
  {:arglists '([opts? & args])}
  [& args]
  (apply @p-shell args))

(defn- jolt-exe
  "The jolt executable to re-invoke: $JOLT_EXE (bin/jolt exports it), else
  this binary's own path, else `jolt` on PATH."
  []
  (or (System/getenv "JOLT_EXE") (jolt.host/self-exe) "jolt"))

(defn jolt
  "Run the jolt CLI as a subprocess with these arguments. Same call shape as
  `shell` (leading options map, first argument tokenized), so
  `(jolt \"-M:test -m app.test-runner\")` works as written.

  This is where babashka's `clojure` lands — see the namespace docstring."
  {:arglists '([opts? & args])}
  [& args]
  (let [[opts args] (if (map? (first args)) [(first args) (rest args)] [nil args])
        ;; the executable first, then the tokenized first argument (`shell`
        ;; only tokenizes what it thinks is the command) and the rest verbatim.
        ;; Spread, not passed as one vector: shell takes a vector only when it
        ;; is the sole argument, and would stringify it after an options map.
        cmd (into [(jolt-exe)]
                  (concat (when (seq args) (@p-tokenize (str (first args))))
                          (rest args)))]
    (if opts (apply @p-shell opts cmd) (apply @p-shell cmd))))

(defn clojure
  "babashka.tasks/clojure under its babashka name, so a bb.edn written for `bb`
  runs unchanged. It runs the jolt CLI, not the JVM Clojure CLI: `jolt` is the
  same function and the spelling to prefer in new task maps."
  {:arglists '([opts? & args])}
  [& args]
  (apply jolt args))

(defn run
  "Run another task by name, in this process. Its :depends run first (each at
  most once per invocation), then its :enter, body and :leave.

    (run 'build)

  `opts` is accepted for babashka call-shape compatibility; :parallel is the
  only key jolt reads, and it applies to the task's :depends."
  ([task] (run task nil))
  ([task opts] ((requiring-resolve 'jolt.tasks/run-nested!) task opts)))
