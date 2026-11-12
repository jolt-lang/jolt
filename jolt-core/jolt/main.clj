(ns jolt.main
  "The jolt CLI dispatch: resolve a project's deps.edn, set the source roots, and
  run a namespace's -main, a file, a deps.edn task, or a REPL. Driven by cli.ss,
  which hands it the raw argv; the project directory is JOLT_PWD (the user's cwd
  before the launcher cd'd to the jolt repo)."
  (:require [jolt.deps :as deps]
            [jolt.main.natives :as natives]
            [clojure.string :as str]))

(defn- project-dir [] (or (jolt.host/getenv "JOLT_PWD") "."))

(defn- version [] (jolt.host/jolt-version))

;; Install the :jolt/provides declarations a resolved project collected (RFC 0014).
;; An entry is [install-ns lib class ...]; lib is nil for the project's own.
;; jolt.deps records them, so a later add-deps of the same library doesn't claim
;; the classes a second time.
(defn- register-class-providers! [provides]
  (deps/install-class-providers! provides))

;; Install the project's :jolt/replaces declarations: namespaces jolt provides as
;; a host built-in that this project supplies itself. Like the providers above,
;; it has to land before anything of the project compiles — the first require of
;; babashka.fs is what reads it.
(defn- register-ns-replacements! [replaces]
  (doseq [n replaces] (jolt.host/replace-builtin-ns! n)))

;; ...and the project's :jolt/features: reader-conditional keys to read on top of
;; jolt's own. Earlier still than the two above — the first form READ consults
;; the feature set, and reading precedes every class reference and every require.
(defn- register-reader-features! [features]
  (when (seq features) (jolt.host/add-reader-features! features)))

;; Apply a resolved project's roots on top of the current (jolt-core) roots so app
;; namespaces resolve while jolt.* stays loadable, then register its declared host
;; class providers and load its native deps.
(defn- apply-project!
  ([resolved] (apply-project! resolved true))
  ([{:keys [roots immutable-roots natives provides replaces features project-dir]} strict?]
   (jolt.host/add-immutable-roots! immutable-roots)
   (jolt.host/set-source-roots! (vec (distinct (concat roots (jolt.host/source-roots)))))
   ;; Before the providers: those matter at the first class REFERENCE, this one
   ;; at the first form read, and reading comes first.
   (register-reader-features! features)
   ;; Providers go in BEFORE anything of the project compiles (RFC 0014): the
   ;; table has to be complete before the first class reference can miss, which
   ;; is the whole reason the mapping is declared rather than registered by the
   ;; provider as it loads.
   (register-class-providers! provides)
   (register-ns-replacements! replaces)
   ;; project-dir is absent from a cpcache entry written by an older jolt; JOLT_PWD
   ;; is where the user invoked us, which is the same directory in the common case.
   (natives/load-project-natives! natives strict? (or project-dir (jolt.host/getenv "JOLT_PWD")))))

;; Aliases selected by a leading -A:… — bound around the re-dispatch so every
;; command that resolves the project (run/path/build/repl/nrepl/task, and a
;; following -M's own aliases) resolves WITH them, not just the initial
;; apply-project!. `jolt -A:jolt path` lists the alias roots; `-A:x -M:y`
;; combines both alias sets like tools.deps.
(def ^:private ^:dynamic *cli-aliases* [])

;; An extra deps.edn map from a leading -Sdeps '<edn>' — merged last into the
;; user+project chain by resolve-project, like tools.deps.
(def ^:private ^:dynamic *cli-extra-edn* nil)

;; -T tool mode: the project's own :paths/:deps are replaced (the tool alias
;; supplies its own), like the clj CLI's tool basis.
(def ^:private ^:dynamic *cli-tool?* false)

;; The report options — -Spath (print the roots), -Stree (print the dependency
;; tree), -Sdescribe (print the environment), -P (fetch the deps and stop). Each
;; answers something about the project instead of running it, and each is bound
;; around the re-dispatch of the rest of the argv so the forms that only add
;; resolution context still count: `jolt -Spath -M:test` and `jolt -A:test
;; -Spath` both report on the resolution that run would use, like the clj CLI,
;; which accepts these on either side of the alias options.
(def ^:private ^:dynamic *cli-report* nil)

;; -Srepro: resolve from the project deps.edn alone, ignoring the user one.
(def ^:private ^:dynamic *cli-repro?* false)

;; -Scp: source roots supplied on the command line, in place of the ones the
;; dependencies would resolve to — the expansion is skipped entirely.
(def ^:private ^:dynamic *cli-cp* nil)

(defn- resolve-current
  ([] (resolve-current [] nil))
  ([aliases] (resolve-current aliases nil))
  ([aliases opts]
   (deps/resolve-project (project-dir) (into *cli-aliases* aliases)
                         *cli-extra-edn*
                         (merge {:tool? *cli-tool?*
                                 :repro? *cli-repro?*
                                 :cp *cli-cp*
                                 :trace? (contains? #{:tree :trace-file} *cli-report*)
                                 :graph? (contains? #{:graph :outdated} *cli-report*)}
                                opts))))

;; The resolution a task runs against: :tasks? lets a project's bb.edn
;; contribute its own :paths/:deps (see jolt.deps), and a task's :extra-paths /
;; :extra-deps ride in as a synthetic alias so they combine by the ordinary
;; tools.deps rules rather than by a second set of them.
(defn- resolve-for-task
  ([] (resolve-for-task nil))
  ([task]
   (if-let [extra (and (map? task) (not-empty (select-keys task [:extra-paths :extra-deps])))]
     (binding [*cli-aliases* (conj (vec *cli-aliases*) :jolt/task)
               *cli-extra-edn* (assoc-in (or *cli-extra-edn* {}) [:aliases :jolt/task] extra)]
       (resolve-current [] {:tasks? true}))
     (resolve-current [] {:tasks? true}))))

(defn- print-roots [{:keys [roots]}]
  (println (str/join ":" roots)))

;; -Sdescribe's answer: the environment as one readable EDN map. Rendered a key
;; per line, in a fixed order, so it stays diffable — a jolt map of this size is
;; a hash map, and printing it directly would order the keys arbitrarily.
(defn- print-describe [aliases]
  (let [env (deps/env-info (project-dir) *cli-repro?*)
        entries [[:version (version)]
                 [:project-dir (:project-dir env)]
                 [:config-files (:config-files env)]
                 [:config-user (:config-user env)]
                 [:config-project (:config-project env)]
                 [:gitlibs-dir (:gitlibs-dir env)]
                 [:mvn-local-repo (:mvn-local-repo env)]
                 [:repro (:repro env)]
                 [:aliases (vec aliases)]]]
    (println (str "{" (str/join "\n " (map (fn [[k v]] (str k " " (pr-str v))) entries))
                  "}"))))

;; -Sverbose's preamble: where this resolution reads from and fetches into.
;; The clj CLI prints its version of this on stdout; jolt keeps it on stderr,
;; with the rest of its resolution chatter, so `-Sverbose -Spath` still pipes.
(defn- print-verbose-env []
  (let [env (deps/env-info (project-dir) *cli-repro?*)]
    (binding [*out* *err*]
      (println (str "version        = " (version)))
      (println (str "project_dir    = " (:project-dir env)))
      (println (str "user_deps      = " (or (:config-user env) "(skipped)")))
      (println (str "project_deps   = " (:config-project env)))
      (println (str "gitlibs_dir    = " (:gitlibs-dir env)))
      (println (str "mvn_local_repo = " (:mvn-local-repo env))))))

;; Answer the bound report option. `aliases` are the command's own (a -M/-X/-T
;; selection) on top of the ones -A bound; `resolved` is its resolution — nil
;; for -Sdescribe, which reads no dependencies at all.
(defn- report! [aliases resolved]
  (case *cli-report*
    :path     (print-roots resolved)
    :tree     (run! println (deps/dep-tree-lines (:trace resolved)))
    ;; -Sgraph and -Soutdated render the same tree; only -Soutdated pays for the
    ;; remote lookups that fill in the `-> VERSION` arrows.
    :graph    (run! println (deps/dep-graph-lines resolved))
    :outdated (run! println (deps/dep-graph-lines resolved (deps/dep-updates resolved)))
    :describe (print-describe (into *cli-aliases* aliases))
    ;; -Strace: the same trace -Stree renders, written beside the deps.edn it
    ;; describes (`clojure -Strace` drops it in the current directory; jolt's is
    ;; the project dir, which is where the argv's deps.edn came from).
    :trace-file
    (let [f (str (project-dir) "/trace.edn")]
      (spit f (deps/trace-edn-string (:trace resolved)))
      (binding [*out* *err*] (println (str "Wrote " f))))
    ;; -P: resolving IS the work — every dep is fetched into the caches by the
    ;; time we get here, which is the point of a prepare step.
    :prepare  nil))

;; Every command that resolves the project with aliases of its own funnels
;; through here: under a report option the resolution IS the answer, so report
;; and run nothing — no natives loaded, no :main-opts, no :exec-fn.
(defn- with-project [aliases resolved f]
  (if *cli-report*
    (report! aliases resolved)
    (do (apply-project! resolved) (f))))

;; …and the terminal form of the same thing, for an argv with no command left to
;; take its aliases from: resolve (unless the report needs nothing) and answer.
(defn- cmd-report []
  (report! [] (when-not (= :describe *cli-report*) (resolve-current))))

;; Consume the first standalone "--" (POSIX end-of-options marker); everything
;; else — including any later "--" — is left as literal program data.
(defn- drop-end-of-options [args]
  (loop [in (seq args) acc []]
    (cond
      (nil? in)              (seq acc)
      (= "--" (first in))    (concat (seq acc) (rest in))
      :else                  (recur (next in) (conj acc (first in))))))

(defn- with-entry-bindings
  "Run F under clojure.main's entry bindings for the vars a source commonly
  set!s — *warn-on-reflection*, *assert*, *unchecked-math*. clojure.main wraps
  every entry — repl, -e, -m, -X/-T — in with-bindings, so user code running
  AFTER the load (a -main, an exec fn, a task body, and anything it evaluates
  at runtime, a jolt.loader source dep included) has a thread-local slot for a
  top-level (set! *warn-on-reflection* true). jolt bound them for -e and for a
  file load, but not around its own entries. Scoped, so a throw pops the frame
  instead of leaving it standing for the rest of the thread. The -e evaluator
  keeps the same frame in host/chez/dyn-binding.ss (jolt-with-ns-load-vars) and
  the built binary's launcher takes it there too; all four agree on the set."
  [f]
  (binding [*warn-on-reflection* *warn-on-reflection*
            *assert* *assert*
            *unchecked-math* *unchecked-math*]
    (f)))

(defn- run-ns
  "Require ns-name and invoke its -main with the string app args. A leading
  standalone \"--\" in app-args is consumed as POSIX end-of-options, so this is
  the single end-of-options point for every ns-based entry form — `run -m`,
  `-m`, `-M`/`-A` aliases, and a :main-opts task all route through here."
  [ns-name app-args]
  (let [app-args (drop-end-of-options app-args)]
    ;; The require brackets the file it loads (loader.ss ldr-with-file-vars),
    ;; but that frame pops with the load; the -main that follows is code
    ;; running AFTER it, and needs its own entry frame (with-entry-bindings).
    (binding [*command-line-args* (seq app-args)]
      (with-entry-bindings
        (fn []
          ;; The entry namespace not being on the roots usually means there is no
          ;; project here at all -- jolt was started in a subdirectory, or somewhere
          ;; else entirely -- and "could not locate app/core" hides that. Asked
          ;; BEFORE the require, not caught around it: a catch re-raises a load error
          ;; that propagates through the require from here, and the report then names
          ;; run-ns instead of the form that failed.
          (let [dir (project-dir)]
            (when (and (not (.exists (java.io.File. (str dir "/deps.edn"))))
                       (not (.exists (java.io.File. (str dir "/bb.edn"))))
                       (nil? (jolt.host/ns-source ns-name)))
              (let [here (.getCanonicalPath (java.io.File. dir))]
                (throw (ex-info (str "No project found in " here " (no deps.edn or bb.edn), so "
                                     ns-name " was looked for on the built-in source roots only. "
                                     "Run from the project directory, or pass -Sdeps.")
                                {:ns ns-name :dir here})))))
          (require (symbol ns-name))
          (if-let [mainv (ns-resolve (symbol ns-name) (symbol "-main"))]
            (apply (deref mainv) app-args)
            (throw (ex-info (str "namespace " ns-name " has no -main") {:ns ns-name}))))))))

;; Evaluate a string of forms through the launcher's evaluator (cli-core.ss), the
;; same primitive a bare `jolt -e` runs — so the fast path and the project-aware
;; path share their read/compile/eval loop, their require auto-quoting, and their
;; final-value printing. EXPR of "-" reads the expression from stdin. Assumes the
;; project has already been resolved by the caller.
(defn- eval-expr-string [expr app-args print?]
  (when (nil? expr)
    (throw (ex-info "-e needs an expression (or - to read it from stdin)" {})))
  (jolt.host/run-expr-string (if (= "-" expr) (jolt.host/read-all-stdin) expr)
                             (vec (drop-end-of-options app-args))
                             print?))

;; A FILE argument of "-" means stdin, like bb/ys and most CLIs; map it to
;; /dev/stdin, which load-file reads. Relative paths belong to the project
;; directory. This differs from the process directory for the development
;; launcher: bin/jolt cd's to its checkout and carries the caller's directory in
;; JOLT_PWD so its host Scheme files remain findable.
;; The platform is a parameter so the Windows rows are reachable from a POSIX
;; host (test/chez/file-arg-test.clj); file-arg below binds it to this one.
(defn- file-arg-for [kind windows? dir x]
  (cond
    (= "-" x) "/dev/stdin"
    ;; Only a RELATIVE path belongs to the project directory. POSIX spells an
    ;; absolute path one way; Windows has drive-absolute (C:/x, C:\x) and UNC
    ;; (//server/share) as well, plus two rooted-but-not-absolute forms — /x,
    ;; which the OS resolves against the current drive, and C:x, against that
    ;; drive's own current directory — that this process cannot resolve any
    ;; better than the OS can, so they are passed through as given.
    ;; Matching a leading "/" alone sent `jolt C:/Users/x/hello.clj` down the
    ;; :else branch, and open-input-file failed on "./C:/Users/x/hello.clj"
    ;; however correct the path the user typed was (jolt-lang/jolt#992).
    (not= :relative kind) x
    ;; a ./-prefixed argument is already project-relative: joining it as is
    ;; reported the file as ././x.clj. .\ is the same argument on Windows.
    (or (str/starts-with? x "./")
        (and windows? (str/starts-with? x ".\\"))) (file-arg-for kind windows? dir (subs x 2))
    ;; with no project directory the path is already the one load-file resolves;
    ;; a "./" put in front would stay in its name, as getAbsolutePath keeps one
    (= dir ".") x
    :else (str dir "/" x)))

(defn- file-arg [x]
  (file-arg-for (deps/path-kind x) (= :windows (natives/current-platform)) (project-dir) x))

;; main-opts is a vector like ["-m" "app.core"] or ["-e" "(prn :hi)"] (optionally
;; with trailing args). The user-supplied extra args are appended, so an alias's
;; :main-opts and the command line combine exactly like the clj CLI, which
;; prepends the alias's :main-opts to the argv.
(declare repl-session run-script!)

(defn- apply-main-opts [main-opts extra-args]
  (let [opts (concat main-opts extra-args)]
    (cond
      (= "-m" (first opts)) (run-ns (second opts) (drop 2 opts))
      (= "-e" (first opts)) (eval-expr-string (second opts) (drop 2 opts) true)
      ;; clojure.main -r / --repl: a REPL, the rest as *command-line-args*.
      (#{"-r" "--repl"} (first opts))
      (do (push-thread-bindings
            {#'clojure.core/*command-line-args* (seq (drop-end-of-options (rest opts)))})
          (repl-session))
      ;; clojure.main: a bare non-option first arg is a script path and the
      ;; rest is *command-line-args* — `jolt -M file.clj a b`, or an alias
      ;; whose :main-opts name a script. Same load shape as `run FILE`.
      (and (string? (first opts))
           (seq (first opts))
           (not (str/starts-with? (first opts) "-")))
      (run-script! (first opts) (rest opts))
      :else (throw (ex-info (str "unsupported :main-opts " (pr-str (vec opts))
                                 " (accepted: -m NS, -e EXPR, -r, or a script FILE)")
                            {:main-opts (vec opts)})))))

(defn- parse-aliases [s]            ; "-M:a:b" / ":a:b" -> [:a :b]
  (let [s (if (str/starts-with? s "-") (subs s 2) s)]
    (->> (str/split s #":") (remove str/blank?) (map keyword) vec)))

;; Load a FILE as a script: the arguments after it are *command-line-args* (nil
;; when there are none), and the first standalone -- is consumed as POSIX
;; end-of-options. Every entry that runs a file goes through here — `jolt FILE`,
;; `jolt run FILE`, `jolt -f FILE`, and an alias's :main-opts naming a script — so
;; the four cannot drift apart. The file may open with a `#!/usr/bin/env jolt`
;; line: the reader takes `#!` as a comment to end of line, the way Clojure's does,
;; so an executable script needs nothing else to be readable as a program.
(defn- run-script! [path args]
  (push-thread-bindings
    {#'clojure.core/*command-line-args* (seq (drop-end-of-options args))})
  (load-file (file-arg path))
  nil)

;; Does a bare argv token name a file to run (rather than a deps.edn task)? A "-"
;; (stdin), an existing file, or a *.jolt/*.clj/*.cljc/*.cljs path. .jolt is the
;; same language as .clj and only marks a file as using jolt-specific interop
;; rather than portable Clojure.
;; Asked of the path load-file will actually read (file-arg's, i.e. relative to
;; the PROJECT directory), never of the raw token: a launcher that cd's away
;; carries the user's directory in JOLT_PWD, and testing the token against the
;; process directory made a script with no extension invisible there — `jolt
;; script.clj` worked, because the extension arm never touches the filesystem,
;; while `jolt script` fell through to the task lookup and reported an unknown
;; task. The two arms have to agree about which file they mean.
;; A directory is never a file to run, however much it looks like one: `test` is a
;; :tasks entry AND a directory in every jolt project, and file-exists? answers #t
;; for a directory, so `jolt test` used to be dispatched here and die in
;; load-file's decoder ("failed on #<binary input port test>: is a directory")
;; rather than running the task.
(defn- run-file-arg? [x]
  (or (= "-" x)
      (let [p (file-arg x)]
        (and (not (jolt.host/directory? p))
             (or (some #(str/ends-with? x %) [".jolt" ".clj" ".cljc" ".cljs"])
                 (jolt.host/file-exists? p))))))

(declare run-task)

;; run [-m NS args… | FILE | TASK]  — FILE may be "-" (stdin). A token that
;; isn't a file names a task, so `jolt run build` works like `bb run build`
;; (and like the bare `jolt build`); --parallel is bb's flag for running a
;; task's :depends concurrently.
(defn- cmd-run [more]
  (let [parallel? (= "--parallel" (first more))
        more (if parallel? (rest more) more)]
    (cond
      (= "-m" (first more))
      (do (apply-project! (resolve-current)) (run-ns (second more) (drop 2 more)))

      ;; -f/--file FILE — bb's spelling for "this argument is a file", and the only
      ;; way to run a script whose name a command or a task also answers to: a
      ;; `build` script in the project root is otherwise the compiler, and a
      ;; `greet` one that a :tasks entry also names is ambiguous by eye.
      (#{"-f" "--file"} (first more))
      (let [path (second more)]
        (when (nil? path)
          (throw (ex-info (str (first more) " needs a FILE argument") {})))
        (apply-project! (resolve-current))
        (run-script! path (drop 2 more)))

      (and (seq more) (not (run-file-arg? (first more))))
      (run-task (first more) (rest more) parallel?)

      (seq more)
      (do (apply-project! (resolve-current))
          (run-script! (first more) (rest more)))

      :else (throw (ex-info "run needs -m NS, a FILE, or a task name" {})))))

;; -M[:alias…] [main-opts…] — resolve with the aliases (plus any bound by a
;; leading -A), then run their :main-opts followed by the command-line ones. With
;; no :main-opts anywhere in the selected aliases the command line supplies them
;; on its own, so a bare `jolt -M -e EXPR` (or `-M -m NS`) works like clj. With
;; nothing to run at all this is clojure.main with no arguments: a REPL over the
;; resolved project — `clj -M:dev` is how a REPL with an alias's deps is started.
;; (It used to be an error, "alias(es) [...] have no :main-opts".)
(defn- cmd-M [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [main-opts] :as resolved} (resolve-current aliases)]
    (with-project aliases resolved
      (fn []
        (if (or (seq main-opts) (seq more))
          (apply-main-opts main-opts more)
          (repl-session))))))

;; -A:alias… — select the aliases, then run the remaining argv as a command with
;; *cli-aliases* bound, so the command's own project resolution (run/path/build/
;; repl/task, or a following -M's) includes them. Binding them is all it does:
;; every command it re-dispatches to resolves the project for itself, and `path`
;; wants the roots printed rather than applied — resolving here as well walked
;; the whole dependency tree a second time and printed its warnings twice.
;; -A and -Sdeps re-dispatch the remaining argv through -main, the command
;; dispatcher at the bottom of this file — declared here for the forward ref.
(declare -main)

(defn- cmd-A [arg more]
  (binding [*cli-aliases* (into *cli-aliases* (parse-aliases arg))]
    (apply -main more)))

(defn- cmd-path []
  (print-roots (resolve-current)))

;; -X / -T exec: invoke a function with a map argument, tools.deps style.
;; qualify-fn is taken directly from clojure.tools.deps.
(defn- qualify-fn
  "Compute function symbol based on fn, ns-aliases, and ns-default"
  [fsym {:keys [ns-aliases ns-default] :as _x}]
  (when (and fsym (not (symbol? fsym)))
    (throw (ex-info (str "Expected function symbol: " fsym) {})))
  (when fsym
    (if (qualified-ident? fsym)
      (let [nsym (get ns-aliases (symbol (namespace fsym)))]
        (if nsym
          (symbol (str nsym) (name fsym))
          fsym))
      (if ns-default
        (symbol (str ns-default) (str fsym))
        (throw (ex-info (str "Unqualified function can't be resolved: " fsym) {}))))))

(defn- parse-exec-args
  "Trailing -X args: k v pairs (edn-read; a vector k is an assoc-in path) and
  an optional final map that merges over everything, like the clj CLI."
  [base args]
  (loop [m (or base {}) [k v & more :as a] (seq args)]
    (cond
      (nil? a) m
      (nil? v) (let [x (clojure.core/read-string k)]
                 (if (map? x)
                   (merge m x)
                   (throw (ex-info (str "Key is missing value: " k) {}))))
      :else (let [kx (clojure.core/read-string k)
                  vx (clojure.core/read-string v)]
              (recur (assoc-in m (if (vector? kx) kx [kx]) vx) more)))))

(defn- exec-fn-call
  "Resolve and invoke an exec fn per the combined argmap: an explicit ns/fn
  from the command line wins, else the aliases' :exec-fn; :exec-args merges
  under the command-line k v overrides."
  [argmap fsym-arg args]
  ;; -X/-T is an entry like -m: the exec fn is user code after the load.
  (with-entry-bindings
    (fn []
      (let [fsym (qualify-fn (or fsym-arg (:exec-fn argmap)) argmap)
            _ (when-not fsym (throw (ex-info "No function to execute: supply ns/fn or an alias with :exec-fn" {})))
            exec-args (parse-exec-args (:exec-args argmap) args)]
        (require (symbol (namespace fsym)))
        (if-let [v (resolve fsym)]
          ((deref v) exec-args)
          (throw (ex-info (str "Function not found: " fsym) {:fn fsym})))))))

;; -X:alias… [ns/fn] [k v …] — resolve with the aliases, invoke :exec-fn (or
;; the given ns/fn) with :exec-args + overrides.
(defn- cmd-X [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [argmap] :as resolved} (resolve-current aliases)]
    (with-project aliases resolved
      (fn []
        (let [[fsym-arg args] (if (and (seq more) (str/includes? (first more) "/"))
                                [(symbol (first more)) (rest more)]
                                [nil more])]
          (exec-fn-call argmap fsym-arg args))))))

;; -T:alias… [ns/fn] [k v …] — like -X but a TOOL execution: the project's own
;; paths and deps are replaced (the tool's alias supplies its deps), matching
;; the clj CLI's -T basis (:replace-paths ["."] :replace-deps {} defaults).
(defn- cmd-T [arg more]
  (binding [*cli-tool?* true]
    (cmd-X arg more)))

(defn- repl-literal-step
  "One character inside a string or regex literal (STATE says which): a
  backslash skips the character it escapes, a quote returns to code."
  [i depth c state]
  (cond
    (= c \\) [(+ i 2) depth state]            ; escaped char
    (= c \") [(inc i) depth :code]
    :else    [(inc i) depth state]))

(defn- repl-code-step
  "One character of code in `s` at `i`: answers the next [i depth state]."
  [s i depth c]
  (cond
    (= c \;)        [(inc i) depth :comment]
    (= c \\)        [(+ i 2) depth :code]     ; char literal: \(
    (= c \")        [(inc i) depth :string]
    (= c \#)        (if (= (get s (inc i)) \")
                      [(+ i 2) depth :regex]   ; consume the #" together
                      [(inc i) depth :code])
    (#{\( \[ \{} c) [(inc i) (inc depth) :code]
    (#{\) \] \}} c) [(inc i) (dec depth) :code]
    :else           [(inc i) depth :code]))

(defn- repl-form-complete?
  "True when `s` has balanced ()/[]/{}, no open string/char/regex, and at most
  a trailing comment past the last form. Drives the REPL's read-until-complete
  decision so a form split across lines is accumulated, not evaluated half-read."
  [s]
  (let [n (count s)]
    (loop [i 0 depth 0 state :code]               ; state: :code :string :regex :comment
      (if (>= i n)
        (and (<= depth 0) (#{:code :comment} state))
        (let [c (get s i)
              [i depth state]
              (case state
                :code    (repl-code-step s i depth c)
                (:string :regex) (repl-literal-step i depth c state)
                :comment [(inc i) depth (if (#{\newline \return} c) :code :comment)])]
          (recur i depth state))))))

(defn- repl-read-form []
  ;; Read lines — printing a secondary prompt for continuations — until the
  ;; accumulated buffer is a complete form. Returns the (possibly multi-line)
  ;; buffer, or nil on EOF at the primary prompt.
  (loop [buf nil]
    (print (if buf "... " (str (ns-name *ns*) "=> "))) (flush)
    (let [line (read-line)]
      (cond
        (nil? line) buf                                 ; EOF: nil at primary, partial mid-form
        (nil? buf)  (cond
                      (str/blank? line)        (recur nil)      ; skip a blank first line
                      (repl-form-complete? line) line
                      :else                    (recur line))
        :else       (let [nb (str buf "\n" line)]
                      (if (repl-form-complete? nb) nb (recur nb)))))))

(defn- repl-entry-reader
  "How the session reads an entry. A terminal gets jolt.line-editor (multi-line
  editing with parinfer keeping the parens balanced, docs, history); anything
  else, a pipe or a script or TERM=dumb, keeps repl-read-form, so input that is
  not typed is read exactly as written. The editor ns loads only when used."
  []
  (or (when (jolt.host/term-open?)
        (when-let [ed ((requiring-resolve 'jolt.line-editor/editor))]
          (let [read-entry (requiring-resolve 'jolt.line-editor/read-entry)]
            (fn [] (read-entry ed (str (ns-name *ns*) "=> "))))))
      repl-read-form))

(defn- repl-eval
  "Evaluate an entry so that ^C interrupts it: while it runs, SIGINT trips token
  instead of ending the process, stopping a computation and throwing a sleep or
  a deref out with InterruptedException. Between entries ^C is the exit it
  always was."
  [form token]
  (jolt.host/interrupt-on-sigint! token)
  ;; eval-string, not load-string: load-string binds *ns* for its load, as
  ;; Compiler.load does, and an entry's (ns foo) must switch the session. The
  ;; session binds *ns* itself (repl-session), as clojure.main's REPL does.
  (try (jolt.host/run-interruptible token (fn [] (jolt.host/eval-string form)))
       (finally (jolt.host/interrupt-on-sigint! nil))))

;; `jolt repl`: resolve the project so deps (git libs) are on the roots and
;; native libs are loaded — same context a run gets, so (require '[some.lib])
;; works in the REPL — then the session. -M reaches the session directly, with
;; its aliases already applied.
(defn- repl []
  (try (apply-project! (resolve-current))
       (catch :default _ nil))
  (repl-session))

(defn- repl-load-error
  "The report for a load's CompilerException, or nil for any other throw. A
  form that throws while a load runs it (a require, load-file, load-string)
  leaves the load wrapped, as Compiler.load does: \"Syntax error macroexpanding
  at (file:line:col).\", :clojure.error/phase :execution, the throw as its
  cause. That message names neither the cause nor what went wrong, so the REPL
  reports it as clojure.main's does: an execution error naming the cause's
  class and the form's position, then the cause's message on its own line.
  Like clojure.main/ex-str, a plain Exception or RuntimeException goes unnamed.
  The position is the wrapper's own, path and column included, as the -e
  report gives it, since no frame of jolt's names an eval fn the way the
  JVM's does."
  [e]
  (let [d (ex-data e)
        c (ex-cause e)]
    (when (and c (= :execution (:clojure.error/phase d)))
      (str "Execution error"
           (let [n (.getSimpleName (class c))]
             (when-not (#{"Exception" "RuntimeException"} n) (str " (" n ")")))
           " at ("
           (when-let [s (:clojure.error/source d)] (str s ":"))
           (:clojure.error/line d) ":" (:clojure.error/column d) ").\n"
           (or (ex-message c) (pr-str c))))))

(defn- repl-quit?
  "Is the entry :repl/quit or :exit? Either ends the session — a reliable
  gesture that works in any terminal, unlike ^D (some terminals/editors don't
  deliver it as EOF)."
  [form]
  (#{:repl/quit :exit} (try (read-string form) (catch :default _ nil))))

(defn- repl-print-value!
  "Shift the result history (*1 *2 *3) and print V."
  [v token]
  (var-set #'clojure.core/*3 *2)
  (var-set #'clojure.core/*2 *1)
  (var-set #'clojure.core/*1 v)
  ;; the body caught the interrupt and returned
  (when (jolt.host/interrupted? token) (println))
  (println (pr-str v)))

(defn- repl-print-error!
  "Set *e and report E, with the backtrace unless ^C interrupted the entry."
  [e token]
  (var-set #'clojure.core/*e e)
  (let [interrupted? (jolt.host/interrupted? token)]
    ;; off the line the terminal echoed ^C on
    (when interrupted? (println))
    (println "error:" (or (when interrupted? "Evaluation interrupted")
                          (repl-load-error e)
                          (ex-message e)
                          (try ((resolve 'jolt.host/condition-message) e) (catch :default _ nil))
                          (pr-str e)))
    (when-let [bt (and (not interrupted?) (jolt.host/backtrace-string))]
      (print bt))))

(defn- repl-eval-entry! [form]
  (let [token (jolt.host/make-interrupt)]
    (try (repl-print-value! (repl-eval form token) token)
         (catch :default e
           (repl-print-error! e token)))))

(defn- repl-session []
  ;; REPL-driven development: trace by default so an uncaught error in evaluated
  ;; code shows a tail-frame backtrace, no JOLT_TRACE needed (JOLT_TRACE=0 opts out).
  (jolt.host/enable-trace!)
  (println (str ";; jolt " (version) " repl — :repl/quit or ^D to exit"))
  ;; *repl* reads true inside the session, and the result/exception history
  ;; vars update per evaluation, like clojure.main's REPL.
  ;; *ns* too, as clojure.main/with-bindings binds it: an entry's in-ns or ns
  ;; form switches this binding, and the prompt and the next entry read it.
  (push-thread-bindings {#'clojure.core/*repl* true
                         #'clojure.core/*ns* *ns*
                         #'clojure.core/*1 nil #'clojure.core/*2 nil
                         #'clojure.core/*3 nil #'clojure.core/*e nil})
  (loop [read-entry (repl-entry-reader)]
    (let [form (read-entry)]
      (when form
        (cond
          (repl-quit? form)
          nil
          ;; an entry the editor submitted blank, or abandoned with ^C
          (str/blank? form)
          (recur read-entry)
          :else
          (do (repl-eval-entry! form)
              (recur read-entry)))))))

;; A bb.edn / deps.edn :tasks entry. The semantics live in jolt.tasks — required
;; here only when a task actually runs, so the CLI's startup closure (and the
;; AOT'd jolt.main in a built binary) stays free of it and of babashka.process.
;; The task's own :extra-paths / :extra-deps change the resolution, so the map is
;; read from a first resolution and the run gets a second one only when it must.
(defn- run-task [name more parallel?]
  (let [base (resolve-for-task)
        task (get (:tasks base) (symbol name))
        resolved (if (and (map? task)
                          (or (:extra-paths task) (:extra-deps task)))
                   (resolve-for-task task)
                   base)]
    (when (nil? task)
      (throw (ex-info (str "unknown command or task: " name " (see 'jolt tasks')")
                      {:name name})))
    ;; tolerant: see natives/load-natives! — the task may be the build step for a
    ;; :jolt/native library that does not exist yet
    (apply-project! resolved false)
    ;; babashka's exit rule, not clojure.main's: bb's future and agent threads are
    ;; daemons, so a task ends without waiting on them (a Thread it starts, or a
    ;; pool it never shuts down, still holds the process up, as there)
    (jolt.host/agent-threads-daemon!)
    ;; a task is an entry too (clojure.main -T): its body is user code running
    ;; after the load, and :run-main-opts lands in run-ns' own entry frame.
    (with-entry-bindings
      (fn []
        ((requiring-resolve 'jolt.tasks/run-task!)
         {:tasks (:tasks resolved)
          :name name
          ;; the argv after the task name, verbatim. A code body sees it as
          ;; *command-line-args* with every "--" kept, as babashka hands a task
          ;; its argv (jolt#1603): jolt parses no options after the task name, so
          ;; a "--" there is the task's to read. A :main-opts task hands the list
          ;; to run-ns, the end-of-options point for every ns entry form, which
          ;; consumes a leading "--" the way `bb -m` does.
          :args (vec more)
          :parallel parallel?
          :run-main-opts apply-main-opts})))))

;; `jolt tasks` — the listing. Reads the :tasks maps alone: naming the tasks
;; needs no dependency expansion, and a project whose deps don't resolve should
;; still be able to say what it can run.
(defn- cmd-tasks []
  ((requiring-resolve 'jolt.tasks/list-tasks!) (deps/project-tasks (project-dir))))

;; `jolt completions <shell>` prints the function to source; `completions tasks`
;; prints the name/doc lines that function asks for when a project's config
;; changes. Both read the config files only, like cmd-tasks and for the same
;; reason: a completing shell must not be the thing that discovers your deps
;; don't resolve.
(defn- cmd-completions [more]
  (let [what (first more)]
    (case what
      nil (throw (ex-info "completions needs a shell: zsh, bash, fish, or tasks" {}))
      "tasks" (run! println
                    ((requiring-resolve 'jolt.completions/task-lines)
                     (deps/project-tasks (project-dir))))
      (print ((requiring-resolve 'jolt.completions/snippet) what "jolt")))))

;; `jolt org.babashka.cli/completions complete --shell zsh -- <task> <token>…`
;; — babashka.cli's hidden completion callback, answered here for a task that
;; parses (one with :exec-fn or :cmd). The generated snippets complete a task
;; NAME from their own cache and call back only for a task those cached lines
;; marked as one that parses, so a plain task still costs a completing shell no
;; jolt start at all.
;;
;; Nothing on this path may report a failure: the stub discards stderr and reads
;; stdout as the candidate list, so a project whose deps do not resolve has to
;; come back as babashka.cli's file-completion marker rather than as an error —
;; anything else both offers nothing and suppresses the shell's own fallback.
(defn- cmd-cli-completions [more]
  (let [[sub & opts] more
        shell (or (second (drop-while #(not= "--shell" %) opts)) "zsh")
        toks  (vec (rest (drop-while #(not= "--" %) opts)))]
    (case sub
      ;; the stub to install is jolt's own — it is the one that knows to call
      ;; back here, and for which tasks
      "snippet" (print ((requiring-resolve 'jolt.completions/snippet) shell "jolt"))
      "complete"
      (let [tasks (try (deps/project-tasks (project-dir)) (catch :default _ nil))
            t     (and (seq toks) (get tasks (symbol (first toks))))]
        ;; a handler lives on the project's paths, or on the task's own, so the
        ;; project is applied before the tree is described — tolerantly (a task
        ;; may be the build step for a :jolt/native library that is missing) and
        ;; muted, since a resolution's own output is not a candidate
        (when (map? t)
          (try (with-out-str (apply-project! (resolve-for-task t) false))
               (catch :default _ nil)))
        ((requiring-resolve 'jolt.tasks/complete!) tasks toks shell))
      (println "org.babashka.cli/file-completion"))))

;; A task that shares a built-in's name but does not claim it loses the name, and
;; the built-in then reports in its own terms: in a project whose deps.edn has a
;; `build` task, `jolt build` fails with "build needs an entry: -m NS", which
;; reads like jolt lost the task rather than like a collision. Name the collision
;; before the command runs, and say both ways out of it (jolt-935).
(defn- warn-shadowed-task! [cmd]
  (binding [*out* *err*]
    (println (str "warning: the task `" cmd "` is shadowed by jolt's built-in `"
                  cmd "` command, which is what runs here."))
    (println (str "         run the task with `jolt run " cmd
                  "`, or give it the name by adding :override-builtin true to it."))))

;; babashka's :override-builtin — a task only displaces the jolt command of the
;; same name when it says so. Checked from the :tasks maps directly, so it costs
;; a small file read rather than loading the task runner on every command.
;; A task that does not claim the name is warned about on the way past.
(defn- builtin-overridden? [cmd]
  (let [t (get (deps/project-tasks (project-dir)) (symbol cmd))]
    (cond
      (nil? t)                             false
      (and (map? t) (:override-builtin t)) true
      :else                                (do (warn-shadowed-task! cmd) false))))

 ;; build [-m NS | FILE] [-o OUT] [--opt | --dev] [--no-direct-link] — AOT-compile
 ;; the app into a standalone executable. Resolves deps + roots like `run`, then hands
 ;; the entry namespace to the host build driver (jolt.host/build-binary, defined by
 ;; build.ss). Default mode is release; --dev unoptimized. --opt now selects only
 ;; the Chez compile parameters (inspector + procedure-source information off, so
 ;; a smaller and faster binary that cannot render a Clojure backtrace) — it no
 ;; longer changes what the compiler emits, because inline/scalar-replace follow
 ;; direct-linking rather than a separate opt-in.
 ;; Release and optimized default to closed-world direct-linking + whole-program
 ;; inference (the throughput lever the perf audit identified). The tradeoff is runtime
 ;; redefinition: a plain def is frozen in the built binary (its eval/load-string and
 ;; redef no-op), so a def that must stay redefinable carries ^:redef — a ^:dynamic var
 ;; stays var-routed automatically. --no-direct-link (or deps.edn :jolt/build
 ;; {:direct-link false}) opts back out to dynamic var routing; --dev stays dynamically
 ;; linked. --direct-link is kept as a now-redundant alias.

(defn- profile-use-files
  "The files after each --profile-use in ARGS, up to the next flag."
  [args]
  (loop [a args, acc []]
    (cond
      (empty? a) acc
      (= "--profile-use" (first a))
      (let [[fs rest-a] (split-with #(not (str/starts-with? % "-")) (rest a))]
        (recur rest-a (into acc fs)))
      :else (recur (rest a) acc))))

;; Flags of `jolt build` that consume the next argument, and the key its value
;; lands under in parse-build-args' result. --boot's value is read from the flag
;; args by build-boot-mode instead (it needs the alias rules there); it is
;; consumed here so a bare `build --boot small` does not read `small` as the
;; entry namespace.
(def ^:private build-value-flags
  {"-m" :entry
   "-o" :out
   ;; cross-compilation: --target <machine> [--target-pack <dir>]
   "--target" :target
   "--target-pack" :target-pack
   "--boot" nil})

(defn- parse-build-args
  "The entry namespace, -o, --target, --target-pack and --include values from a
  `jolt build` command line. Flags are recognized only before `--`; the first
  other argument is the entry unless -m names one."
  [args]
  (loop [a args, opts {:includes []}, end-opts? false]
    (let [cur (first a)]
      (cond
        (empty? a) opts
        end-opts? (recur (rest a) (update opts :entry #(or % cur)) true)
        (= "--" cur) (recur (rest a) opts true)
        (contains? build-value-flags cur)
        (recur (drop 2 a)
               (if-let [k (get build-value-flags cur)] (assoc opts k (second a)) opts)
               false)
        ;; --profile-use takes every file after it, so a shell glob over several
        ;; runs' dumps works (profile-use-files)
        (= "--profile-use" cur)
        (recur (drop-while #(not (str/starts-with? % "-")) (rest a)) opts false)
        ;; --include takes a value too, and repeats; deps.edn
        ;; :jolt/build {:include [ns …]} is read alongside it (build-include-names).
        (= "--include" cur)
        (let [v (second a)]
          (when (or (nil? v) (str/starts-with? v "-"))
            (throw (ex-info "--include needs a namespace name" {:include v})))
          (recur (drop 2 a) (update opts :includes conj v) false))
        (str/starts-with? cur "-") (recur (rest a) opts false)
        :else (recur (rest a) (update opts :entry #(or % cur)) false)))))

(defn- as-vector
  "A deps.edn value that may be given as one item or a collection of them."
  [v]
  (cond (nil? v) [] (sequential? v) (vec v) :else [v]))

(defn- build-mode [flag-args build]
  (cond (some #{"--opt"} flag-args) "optimized"
        (some #{"--dev"} flag-args) "dev"
        (:opt build)                "optimized"
        :else                       "release"))

;; Output paths resolve against the project dir (JOLT_PWD), not the CLI's
;; cwd — bin/jolt cd's to the jolt repo, so a bare relative path would land
;; there. Default output is cargo-style under target/: --dev -> target/debug,
;; release/--opt -> target/release, the binary named after the project dir
;; (falling back to the entry's first segment). The <name>.build scratch dir
;; the driver creates sits next to it, so it lands under the same target dir.
;; An explicit -o is honored: absolute as-is, relative against the project.
(defn- build-output-path [pdir entry mode out]
  (let [;; the project dir's own name; "." (JOLT_PWD unset, the built binary
        ;; started in the project) resolves to the directory it stands for
        seg (.getName (java.io.File. pdir))
        seg (if (or (str/blank? seg) (= "." seg))
              (.getName (.getCanonicalFile (java.io.File. pdir)))
              seg)
        proj (if (str/blank? seg) (first (str/split entry #"\.")) seg)]
    (cond
      (nil? out) (str pdir "/target/" (if (= mode "dev") "debug" "release") "/" proj)
      (natives/path-rooted? out) out
      :else (str pdir "/" out))))

;; How the boot image is encoded (jolt-lang/jolt#886), ordered from
;; fastest-to-start to smallest-on-disk:
;;   fast   vfasl + LZ4   the default
;;   small  vfasl + gzip  ~a third smaller than a plain boot, and
;;                        still faster to start than one
;;   plain  no vfasl      the boot 0.8.4 produced
;; --no-vfasl is the spelling #886 asked for and stays as an alias for
;; `--boot plain`. Precedence, resolved here and nowhere else so there
;; is one rule: CLI > deps.edn > environment > default, and WITHIN
;; each of those the explicit --boot spelling beats the alias. The
;; other order looks harmless and is not: a script that adds
;; `--boot small` without dropping the `--no-vfasl` it already had is
;; exactly the migration #886 is on, and it would silently keep the
;; larger, slower `plain` boot it was trying to leave.
;;
;; A BLANK environment variable reads as unset, the way bin/jolt
;; already treats JOLT_NO_DEVCACHE. CI exports an empty value for a
;; matrix leg that did not fill one in, and that must neither fail the
;; build (JOLT_BOOT= hit the validation below with an empty string)
;; nor quietly change it (JOLT_NO_VFASL= forced plain).
(defn- build-boot-mode [flag-args build]
  (let [env (fn [k] (let [v (System/getenv k)]
                      (when-not (str/blank? v) v)))
        tail (drop-while #(not= "--boot" %) flag-args)
        cli (when (seq tail)
              (let [v (second tail)]
                (when (or (nil? v) (str/starts-with? v "-"))
                  (throw (ex-info "--boot needs a value: fast, small or plain" {})))
                v))
        v (or cli
              (when (some #{"--no-vfasl"} flag-args) "plain")
              (some-> (:boot build) name)
              (when (:no-vfasl build) "plain")
              (env "JOLT_BOOT")
              (when (env "JOLT_NO_VFASL") "plain")
              "fast")]
    (when-not (#{"fast" "small" "plain"} v)
      (throw (ex-info (str "--boot must be fast, small or plain (got " v ")")
                      {:boot v})))
    v))

;; Namespaces to bake besides the require closure — deps.edn
;; :jolt/build {:include […]}, unioned with --include NS: names the
;; static scan cannot see, reached only by a runtime
;; requiring-resolve (a plugin loader). Symbols or strings; the
;; driver fails the build on a name with no source file, rather
;; than silently baking nothing.
(defn- build-include-names [cli-includes build]
  (into [] (comp (map str) (remove str/blank?) (distinct))
        (concat cli-includes (as-vector (:include build)))))

;; Dependency-root files NOT to bake into the binary — deps.edn
;; :jolt/build {:exclude-resources [glob …]}, matched against the
;; name io/resource asks for: a file no jolt program reads (a
;; ClojureScript externs `*.ext.js`) that a dependency ships anyway.
;; The project's only, like :embed.
(defn- build-exclude-resources [build]
  (let [globs (as-vector (:exclude-resources build))]
    (doseq [p globs]
      (when-not (and (string? p) (not (str/blank? p)))
        (throw (ex-info (str ":exclude-resources takes glob strings (got " (pr-str p) ")")
                        {:exclude-resources p}))))
    (vec (distinct globs))))

;; --profile-gen builds a binary that writes Chez block counts as it
;; exits; --profile-use FILE... builds with them (RFC 0015 part 2).
;; Passed to the driver as "gen" or the files, resolved like -o.
(defn- build-profile [flag-args pdir]
  (let [gen? (boolean (some #{"--profile-gen"} flag-args))
        use? (boolean (some #{"--profile-use"} flag-args))
        files (mapv #(if (natives/path-rooted? %) % (str pdir "/" %))
                    (profile-use-files flag-args))]
    (cond
      (and gen? (or (seq files) use?))
      (throw (ex-info "--profile-gen and --profile-use are two builds: generate first, then use" {}))
      gen? "gen"
      use? (do (when (empty? files)
                 (throw (ex-info "--profile-use needs the profile files a --profile-gen binary wrote" {})))
               (doseq [f files]
                 (when-not (.exists (java.io.File. f))
                   (throw (ex-info (str "--profile-use: no profile file " f) {:file f}))))
               files)
      :else nil)))

(defn- build-options
  "Everything `jolt build` decides from its command line and deps.edn's
  :jolt/build, validated before any native library is built."
  [more {:keys [build] :as resolved}]
  (let [opts (parse-build-args more)
        entry (:entry opts)
        ;; flags are only recognized before the end-of-options marker
        flag-args (take-while #(not= "--" %) more)
        mode (build-mode flag-args build)
        pdir (project-dir)
        _ (when (nil? entry)
            (throw (ex-info "build needs an entry: -m NS" {})))
        ;; cross-compilation (--target <machine>): the output binary is for a
        ;; different Chez machine. Needs a prepared target pack (--target-pack
        ;; DIR or $JOLT_TARGET_PACK) — see tools/cross-compile/README.md.
        target (:target opts)
        target-pack (or (:target-pack opts) (System/getenv "JOLT_TARGET_PACK"))]
    {:entry entry
     :out (build-output-path pdir entry mode (:out opts))
     :mode mode
     :pdir pdir
     :target target
     :target-pack target-pack
     ;; :jolt/native libs with a :static archive are cc-linked into the
     ;; binary by default; --dynamic (or deps.edn :jolt/build {:dynamic-natives
     ;; true}) keeps the old behavior — load a shared object at runtime.
     :dynamic-natives? (boolean (or (some #{"--dynamic"} flag-args) (:dynamic-natives build)))
     ;; closed-world direct-linking is the release default: ON for release and
     ;; optimized (the throughput lever), OFF for --dev. --no-direct-link (or
     ;; deps.edn :jolt/build {:direct-link false}) opts back out; --direct-link
     ;; is a now-redundant alias. ^:redef/^:dynamic defs always stay var-routed.
     :direct-link? (and (not= mode "dev")
                        (not (or (some #{"--no-direct-link"} flag-args)
                                 (false? (:direct-link build)))))
     ;; closed world (drop every def not reachable from -main, core included):
     ;; --closed-world, or --tree-shake as it was first named, or deps.edn
     ;; :jolt/build {:closed-world true} / {:tree-shake true}.
     :tree-shake? (boolean (or (some #{"--closed-world" "--tree-shake"} flag-args)
                               (:closed-world build) (:tree-shake build)))
     :boot-mode (build-boot-mode flag-args build)
     ;; defs the project and its deps vouch never resolve vars at runtime
     ;; in the built binary (deps.edn :jolt/tree-shake {:allow-dynamic […]},
     ;; unioned by resolve-project): the shake skips them in its bail scan
     ;; instead of keeping everything. Every build reads it: the compiler
     ;; verdict (dce-needs-compiler?) runs the same bail scan, so a vouched
     ;; resolve no longer keeps the compiler resident. A vouched eval still
     ;; bails: the compiler image is direct-linked against the whole core
     ;; and cannot run over a shaken one (dce.ss dce-bail-scan).
     :allow-dynamic (vec (:allow-dynamic resolved))
     :include-names (build-include-names (:includes opts) build)
     :exclude-resources (build-exclude-resources build)
     ;; a shared library (callable from C/C++/Rust via jolt_library_init +
     ;; jolt_lookup) instead of an executable: --library.
     :library? (boolean (some #{"--library"} flag-args))
     ;; --signable: an ordinary, same-machine executable build still
     ;; produces a structurally complete (and therefore strictly
     ;; signable) binary by cc-linking the boot in as a C array (against
     ;; this jolt's own embedded kernel when it carries one), rather than
     ;; appending the boot image past its embedded launcher stub. A
     ;; self-contained jolt's default output has most of its bytes
     ;; outside its own Mach-O/PE/ELF image, which a strict signature
     ;; check (codesign --verify --strict on macOS) correctly refuses.
     ;; No effect on --library (build-shared is always signable) or on
     ;; a --target build (already forced onto this path).
     :signable? (boolean (some #{"--signable"} flag-args))
     :profile (build-profile flag-args pdir)}))

(defn- cmd-build [more]
  (let [{:keys [project-paths embed-dirs] :as resolved} (resolve-current)]
    (apply-project! resolved)
    (let [{:keys [entry out mode target target-pack boot-mode allow-dynamic
                  include-names exclude-resources profile direct-link? tree-shake?
                  library? signable?]
           :as opts} (build-options more resolved)]
      (when (and target (nil? target-pack))
        (throw (ex-info "--target needs a target pack: --target-pack DIR (or $JOLT_TARGET_PACK)"
                        {:target target})))
      ;; embed-dirs (absolute) are walked + baked into the binary by the driver;
      ;; project-paths (relative) become runtime io/resource roots (ship-alongside).
      (let [natives (natives/build-natives resolved opts)]
        (if library?
          (jolt.host/build-library entry out mode natives embed-dirs project-paths direct-link? tree-shake? target target-pack boot-mode allow-dynamic nil include-names exclude-resources profile)
          (jolt.host/build-binary entry out mode natives embed-dirs project-paths direct-link? tree-shake? target target-pack boot-mode allow-dynamic signable? include-names exclude-resources profile))))))

(defn- nrepl [more]
  ;; resolve the project (deps on the roots, native libs loaded), then start the
  ;; nREPL server so an editor can connect and (require '[some.lib]) live. A
  ;; library's middleware (deps.edn :nrepl/middleware) is composed over the
  ;; built-in handler — sessions / interruptible-eval / completion etc.
  (let [resolved (resolve-current)]
    (apply-project! resolved)
    ;; --no-port-file / --port-file PATH become start's :port-file (nil writes
    ;; none), for a tool that picks the port itself and has no use for a
    ;; .nrepl-port that another client would take as the project's server. The
    ;; PATH is consumed here so it isn't read as the port.
    (let [[raw-port opts] (loop [as more raw nil opts {}]
                            (if-let [a (first as)]
                              (cond
                                (= a "--no-port-file") (recur (rest as) raw (assoc opts :port-file nil))
                                (= a "--port-file")
                                (if-let [p (second as)]
                                  (recur (nnext as) raw (assoc opts :port-file p))
                                  (throw (ex-info "--port-file needs a path" {})))
                                (str/starts-with? a "-") (recur (rest as) raw opts)
                                :else (recur (rest as) (or raw a) opts))
                              [raw opts]))
          parsed (some-> raw-port parse-long)
          default (parse-long (or (jolt.host/getenv "JOLT_NREPL_PORT") "7888"))
          port (or parsed default)]
      (when (and raw-port (nil? parsed))
        (println (str "warning: ignoring invalid nREPL port '" raw-port "', using " default)))
      (require 'jolt.nrepl)
      ;; start binds the socket synchronously on this (primordial) thread, so a
      ;; failure like the port already being in use surfaces here and exits rather
      ;; than being swallowed by a background thread. It then runs the accept loop
      ;; on a worker thread and returns a stop fn, leaving this thread free to own
      ;; the process main loop: main-thread-affine work an eval starts (e.g. a UI
      ;; toolkit's event loop) marshals here via jolt.host/call-on-main-thread —
      ;; on macOS a native UI event loop must run on the main thread or the
      ;; process aborts (e.g. AppKit rejects setting the main menu off-main).
      ;; Block SIGINT in this (primordial) thread before starting the server so the
      ;; accept-loop future — and the conn-handler futures it spawns — inherit a
      ;; blocked SIGINT mask. Without this, ^C lands on the accept loop blocked in
      ;; c-accept (a foreign call), where Chez can't fire the keyboard-interrupt
      ;; handler, and the server hangs. Registering the stop hook below then arms
      ;; the shutdown watcher, which takes SIGINT along with SIGTERM/SIGHUP, so ^C
      ;; is picked up by sigwait and the hooks run cleanly wherever this thread is.
      ;; (park-until-interrupt keeps SIGINT blocked while that watcher is running,
      ;; and only unblocks it for its own ^C handler when nothing armed one.)
      (jolt.host/block-sigint)
      (let [stop ((resolve 'jolt.nrepl/start) port (:nrepl-middleware resolved) opts)]
        ;; register stop so ^C (handled by park-until-interrupt) closes the socket
        ;; and drops the port file it wrote on the way out.
        (jolt.host/add-shutdown-hook stop)
        ;; park here until ^C (handled by park-until-interrupt's keyboard-interrupt-
        ;; handler, which runs the shutdown hooks and exits). The accept loop
        ;; inherited SIGINT-blocked above, so ^C is delivered to this thread.
        (jolt.host/park-until-interrupt)
        (when stop (stop))))))

(def ^:private command-docs
  "The commands section of `jolt help`, in order: [command lines]. A row whose
  command is nil is not a command of its own (a bare FILE, a task). `jolt CMD
  --help` prints CMD's rows."
  [["repl"
    ["  repl                   start a REPL"]]
   ["nrepl-server"
    ["  nrepl-server [port]    start an nREPL server (default 7888) for editors"
     "    [--no-port-file | --port-file PATH]"
     "                         write no .nrepl-port, or write the port to PATH"]]
   ["run"
    ["  run -m NS [args]       resolve deps.edn, load NS, call its -main"
     "  run FILE [args]        load a Clojure file"]]
   [nil
    ["  FILE [args]            the same, with `run` left out — so a file whose"
     "                         first line is `#!/usr/bin/env jolt` runs as an"
     "                         executable script, with or without an extension"]]
   ["build"
    ["  build -m NS [-o OUT] [--opt|--dev] [--direct-link] [--closed-world] [--dynamic]"
     "              [--boot fast|small|plain] [--library] [--signable]"
     "              [--include NS] [--target MACHINE --target-pack DIR]"
     "                         compile a standalone binary, or with --library a"
     "                         shared object an embedder dlopens and calls through"
     "                         jolt_library_init + jolt_lookup; --target"
     "                         cross-compiles either one for another Chez machine"
     "                         (see tools/cross-compile). A self-contained jolt's"
     "                         default executable output has most of its bytes"
     "                         outside its own Mach-O/PE/ELF image, which a strict"
     "                         signature check (codesign --verify --strict on"
     "                         macOS) refuses; --signable produces a structurally"
     "                         complete binary instead, at the cost of needing a"
     "                         C compiler to link it"
     "                         (no effect on --library, always structurally"
     "                         complete, or on a --target build, already forced"
     "                         onto this same path)"
     "                         --include NS bakes a namespace the require scan"
     "                         cannot see (one reached only by a runtime"
     "                         requiring-resolve); repeatable, and"
     "                         :jolt/build {:include [ns …]} does the same"
     "              [--profile-gen | --profile-use FILE…]"
     "                         --profile-gen builds a binary that writes its"
     "                         block counts to NAME-PID.jpd (or"
     "                         $JOLT_PROFILE_DATA) as it exits; --profile-use"
     "                         builds the same sources with those counts, and"
     "                         fails naming any unit whose code has changed"]]
   ["path"
    ["  path                   print the resolved source roots"]]
   ["tasks"
    ["  tasks                  list the project's bb.edn/deps.edn :tasks"]]
   ["completions"
    ["  completions SHELL      print a completion function to source, for"
     "                         zsh, bash or fish; `completions tasks` prints"
     "                         the name/doc lines that function asks for"]]
   [nil
    ["  <task> [args]          run a task (`run <task>` and `run --parallel"
     "                         <task>` do the same)"]]])

(def ^:private builtin-commands
  "The commands jolt dispatches itself, which a task may take over only with
  :override-builtin, and which answer -h/--help with their own usage."
  #{"run" "repl" "nrepl-server" "path" "build" "tasks" "completions"})

(defn- command-usage [cmd]
  (println "usage:")
  (doseq [[k ls] command-docs :when (= k cmd), l ls] (println l)))

(defn- usage []
  (println (str "jolt " (version)))
  (println "usage: jolt [command] [args]")
  (println)
  (println "With no command, starts a REPL.")
  (println)
  (println "commands:")
  (doseq [[_ ls] command-docs, l ls] (println l))
  (println "  help, --help, -h       print this message")
  (println "  version, --version, -V print the jolt version")
  (println)
  (println "options:")
  (println "  -e EXPR [args]         evaluate EXPR and print the result")
  (println "  -e - [args]            evaluate an EXPR read from stdin")
  (println "  - [args]               run a program read from stdin (as a script)")
  (println "  -m NS [args]           shorthand for run -m")
  (println "  -f FILE [args]         load FILE, whose name may be a command or a task")
  (println "  -M[:alias] [main-opts] run the alias's :main-opts, then the ones given")
  (println "                         here (-m NS [args] or -e EXPR [args]); with no")
  (println "                         :main-opts the command line supplies them")
  (println "  -A:alias [args]        add the alias's paths/deps, run the rest")
  (println "  -X:alias [ns/fn] [k v …]  invoke :exec-fn (or ns/fn) with :exec-args")
  (println "  -T:alias [ns/fn] [k v …]  like -X, with the project paths/deps replaced")
  (println "  -Sdeps '<edn>' …       merge an extra deps.edn map, run the rest")
  (println)
  (println "The report options answer something about the project and run nothing.")
  (println "Each takes the aliases around it, so -A:test -Spath and -Spath -M:test")
  (println "both report on the resolution that run would use:")
  (println "  -Spath                 print the resolved source roots")
  (println "  -Stree                 print the dependency tree, tools.deps format")
  (println "  -Sgraph                print the dependency tree as an indented graph")
  (println "  -Soutdated             the same graph, marking available updates")
  (println "  -Strace                write the dep expansion to trace.edn")
  (println "  -Sdescribe             print the environment as an edn map")
  (println "  -P                     fetch every dependency, then stop")
  (println)
  (println "  -Scp CP …              run against these roots, expanding no deps")
  (println "  -Srepro                ignore the user deps.edn (~/.clojure/deps.edn)")
  (println "  -Sverbose              print where deps are read from and fetched into")
  (println "  -Sforce, -Sthreads N, -Jopt   accepted and ignored: jolt resolves on")
  (println "                         every run, fetches serially, and has no JVM")
  (println)
  (println "The first standalone -- ends option parsing; everything after it is")
  (println "passed to the program as *command-line-args*.")
  (println)
  (println "-e and - resolve deps.edn when the project has one, so the expression")
  (println "can require the project's namespaces and its dependencies. Combine them")
  (println "with -Sdeps/-A to add paths or deps for a one-off evaluation."))

;; Argv forms that only add resolution context — which files to read, which
;; aliases to select, what to merge in — rather than doing something. A report
;; option (-Spath / -Stree / -Sgraph / -Soutdated / -Sdescribe / -P) still lets
;; these dispatch, since
;; they change the answer, and skips everything else: a report runs no program.
(defn- context-arg? [cmd]
  (boolean (and cmd (or (#{"-Sdeps" "-Scp" "-Srepro" "-Sforce" "-Sthreads" "-Sverbose"
                           "-Spath" "-Stree" "-Sgraph" "-Soutdated" "-Strace"
                           "-Sdescribe"} cmd)
                        (some #(str/starts-with? cmd %) ["-A" "-M" "-X" "-T" "-J"])))))

(def ^:private report-opts
  {"-Spath" :path, "-Stree" :tree, "-Sgraph" :graph, "-Soutdated" :outdated,
   "-Strace" :trace-file, "-Sdescribe" :describe, "-P" :prepare})

;; The -S/-J options that change how the REST of the command line runs: each
;; binds its setting and re-dispatches what follows it.
(def ^:private context-options #{"-Srepro" "-Scp" "-Sverbose" "-Sforce" "-Sthreads"})

(defn- run-with-context-option [cmd more]
  (case cmd
    ;; -Srepro — resolve from the project deps.edn alone, ignoring the user
    ;; one, so a build is reproducible on a machine with a different
    ;; ~/.clojure/deps.edn. (JOLT_NO_USER_DEPS does the same by environment.)
    "-Srepro"
    (binding [*cli-repro?* true] (apply -main more))

    ;; -Scp CP — run against these source roots and expand no dependencies.
    ;; The deps.edn chain is still read (aliases, :main-opts, :tasks), so a
    ;; recorded classpath can drive a run offline: -Scp "$(jolt -Spath)".
    "-Scp"
    (let [[cp & rest-args] more]
      (when (nil? cp) (throw (ex-info "-Scp needs a classpath argument" {})))
      (binding [*cli-cp* (vec (remove str/blank? (str/split cp #":")))]
        (apply -main rest-args)))

    ;; -Sverbose — say where the resolution reads from and fetches into, then
    ;; run it with the progress lines on (the JOLT_DEBUG stream, for one run).
    "-Sverbose"
    (binding [deps/*verbose* true]
      (print-verbose-env)
      (apply -main more))

    ;; Accepted and ignored, so tooling that always passes them still works:
    ;; jolt computes its roots on every run (there is no classpath cache to
    ;; force or bypass), fetches serially, and has no JVM to pass options to.
    "-Sforce"   (apply -main more)
    "-Sthreads" (apply -main (rest more))))

(defn- run-command
  "Runs CMD: a built-in command, a main option, a file, or else a task."
  [cmd more]
  (case cmd
    "run"          (cmd-run more)
    "repl"         (repl)
    "nrepl-server" (nrepl more)
    "path"         (cmd-path)
    "tasks"        (cmd-tasks)
    "completions"  (cmd-completions more)
    "build"        (cmd-build more)
    ;; -Sdeps '<edn>' — an extra deps.edn map merged last into the chain,
    ;; bound around the re-dispatch of the remaining argv.
    "-Sdeps"
    (let [[edn-str & rest-args] more]
      (when (nil? edn-str) (throw (ex-info "-Sdeps needs an edn map argument" {})))
      (binding [*cli-extra-edn* (clojure.core/read-string edn-str)]
        (apply -main rest-args)))
    ;; -e EXPR [args] / - [args] — evaluate with the project resolved. The
    ;; launcher (cli-core.ss) short-circuits these when the project dir has no
    ;; deps.edn (nothing to resolve, and it skips loading jolt.main); a project,
    ;; or a leading -Sdeps/-A, lands here instead so the expression sees the
    ;; project's paths, deps, and native libs. Both paths evaluate through the
    ;; same jolt.host/run-expr-string.
    "-e"
    (do (apply-project! (resolve-current))
        (eval-expr-string (first more) (rest more) true))
    "-"
    (do (apply-project! (resolve-current))
        (eval-expr-string "-" more false))
    "-m"           (cmd-run (cons "-m" more))
    ("-f" "--file") (cmd-run (cons cmd more))
    (cond
      (str/starts-with? cmd "-M") (cmd-M cmd more)
      (str/starts-with? cmd "-A") (cmd-A cmd more)
      (str/starts-with? cmd "-X") (cmd-X cmd more)
      (str/starts-with? cmd "-T") (cmd-T cmd more)
      ;; An -S option jolt doesn't have. Falling through would report it as an
      ;; unknown task, which reads like a typo in the deps.edn rather than an
      ;; option this CLI doesn't carry.
      (str/starts-with? cmd "-S")
      (throw (ex-info (str "unsupported option: " cmd " (jolt has -Sdeps, -Scp, -Spath,"
                           " -Stree, -Strace, -Sdescribe, -Srepro, -Sverbose,"
                           " -Sforce, -Sthreads)")
                      {:option cmd}))
      ;; a bare FILE (or "-" for stdin) runs it, `run` optional — like bb; a
      ;; non-file token falls through to a bb.edn/deps.edn :tasks lookup.
      (run-file-arg? cmd) (cmd-run (cons cmd more))
      :else (run-task cmd more false))))

(defn -main [& args]
  (let [[cmd & more] args]
    (cond
      ;; The report options — answer something about the project and run
      ;; nothing. They may come before or after the alias options (`jolt
      ;; -A:test:dev -Spath` is what editors ask with), so each binds the mode
      ;; and re-dispatches rather than answering on the spot.
      (get report-opts cmd)
      (binding [*cli-report* (get report-opts cmd)] (apply -main more))

      ;; …and once one is in effect, only the context-adding forms still
      ;; dispatch: a task, a file, main opts, a REPL — none of it runs.
      (and *cli-report* (not (context-arg? cmd)))
      (cmd-report)

      ;; bare `jolt` starts a REPL, like bb/clj
      (nil? cmd)                         (repl)

      (context-options cmd)              (run-with-context-option cmd more)
      (str/starts-with? cmd "-J")        (apply -main more)

      (#{"help" "--help" "-h"} cmd)      (usage)
      (#{"version" "--version" "-V"} cmd) (println (str "jolt " (version)))

      ;; the hidden completion callback, before the :override-builtin check: it
      ;; is not a name a project can take, and a shell must not be able to lose
      ;; it to a task
      (= cmd "org.babashka.cli/completions") (cmd-cli-completions more)

      ;; A task may take a built-in command's name, but only by asking to
      ;; (babashka's :override-builtin). Checked here, after the two commands
      ;; that read no project at all, so it costs nothing a command doesn't
      ;; already pay: everything below resolves the project anyway.
      (and (builtin-commands cmd) (builtin-overridden? cmd))
      (run-task cmd more false)

      ;; CMD -h / --help: that command's usage, before it resolves a project or
      ;; reads its arguments -- each command parses its own, and none of them
      ;; knew the flag (nrepl-server dropped every "-" argument and started a
      ;; server, repl started a REPL, build asked for an entry; #1152)
      (and (builtin-commands cmd) (#{"-h" "--help"} (first more)))
      (command-usage cmd)

      :else                              (run-command cmd more))))
