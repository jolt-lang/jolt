(ns jolt.main
  "The jolt CLI dispatch: resolve a project's deps.edn, set the source roots, and
  run a namespace's -main, a file, a deps.edn task, or a REPL. Driven by cli.ss,
  which hands it the raw argv; the project directory is JOLT_PWD (the user's cwd
  before the launcher cd'd to the jolt repo)."
  (:require [jolt.deps :as deps]
            [clojure.string :as str]))

(defn- project-dir [] (or (jolt.host/getenv "JOLT_PWD") "."))

(defn- version [] (jolt.host/jolt-version))

(defn- current-platform []
  (let [os (str/lower-case (or (System/getProperty "os.name") ""))]
    (cond (str/includes? os "mac") :darwin
          (str/includes? os "win") :windows
          :else :linux)))

;; Load a library's declared native shared objects (deps.edn :jolt/native) before
;; its Clojure is required, so its foreign-fn bindings resolve. Each entry is a
;; map: {:name "sqlite3" :darwin ["libsqlite3.0.dylib" ...] :linux ["libsqlite3.so.0" ...]}
;; with optional :optional (missing is fine — a feature-gated dep) and :process
;; (use the running process's symbols, e.g. libc sockets — no external file).
;; A :jolt/native candidate that names a PATH — it has a separator — is relative
;; to the project, not to the current directory. dlopen draws the same line:
;; a name with no separator is searched for, a name with one is used as given.
;; Without this, a project that keeps its shared library beside its sources
;; ("native/libfoo.so", which is what a build task produces) loaded only when
;; jolt happened to be run from the project root: `bin/jolt` exports JOLT_PWD and
;; then cd's to its own tree, so the path resolved against the wrong directory
;; and the library read as missing.
(defn- native-candidate [base c]
  (if (and base
           (or (str/includes? c "/") (str/includes? c "\\"))
           (not (str/starts-with? c "/"))
           (not (re-find #"^[A-Za-z]:" c)))
    (str base "/" c)
    c))

;; strict? false lets a MISSING required library through with a warning instead of
;; an error. A task is the one command that may be what PRODUCES the library it
;; is declared alongside: a project whose native/ holds C sources and whose
;; :jolt/native names the .so/.dylib built from them could not run its own build
;; task, because applying the project loaded the natives first and the artifact
;; was not there yet. Any task that actually needs the library still fails on
;; use, and the warning names what was missing either way.
(defn- load-natives!
  ([natives] (load-natives! natives true))
  ([natives strict?] (load-natives! natives strict? nil))
  ([natives strict? base]
  (when (seq natives)
    (let [plat (current-platform)]
      (doseq [spec natives]
        (if (:process spec)
          (jolt.ffi/load-library)
          (let [c (get spec plat)
                cands (mapv #(native-candidate base %)
                            (if (string? c) [c] (vec c)))
                ;; Load the native RTLD_LOCAL and register its handle, so the
                ;; spec's defcfns resolve from the handle (isolated from the
                ;; process-global namespace) rather than depending on global
                ;; foreign-entry search order. load-native returns #t on success
                ;; (a handle, or Windows-global), #f when no candidate opens.
                hit (some #(when (jolt.ffi/load-native %) %) cands)]
            ;; A :static spec has no runtime shared object (it's linked into a
            ;; built binary), so an interpreted `run`/`repl` has nothing to load —
            ;; skip it rather than fail. Its foreign calls only resolve in a static
            ;; build; document a dynamic candidate too to use it under `run`.
            (when (and (nil? hit) (not (:optional spec)) (not (:static spec)))
              (let [msg (str "required native library "
                             (or (:name spec) (first cands) "?")
                             " not found — tried " (pr-str cands) " for " (name plat))]
                (if strict?
                  (throw (ex-info msg {:native spec}))
                  (binding [*out* *err*]
                    (println (str "warning: " msg " (a task may build it)")))))))))))))

;; Install the :jolt/provides declarations a resolved project collected (RFC 0014).
;; An entry is [install-ns lib class ...]; lib is nil for the project's own.
(defn- register-class-providers! [provides]
  (doseq [[install-ns lib & classes] provides]
    (jolt.host/register-class-provider! install-ns lib (vec classes))))

;; Apply a resolved project's roots on top of the current (jolt-core) roots so app
;; namespaces resolve while jolt.* stays loadable, then register its declared host
;; class providers and load its native deps.
(defn- apply-project!
  ([resolved] (apply-project! resolved true))
  ([{:keys [roots natives provides project-dir]} strict?]
   (jolt.host/set-source-roots! (vec (distinct (concat roots (jolt.host/source-roots)))))
   ;; Providers go in BEFORE anything of the project compiles (RFC 0014): the
   ;; table has to be complete before the first class reference can miss, which
   ;; is the whole reason the mapping is declared rather than registered by the
   ;; provider as it loads.
   (register-class-providers! provides)
   ;; project-dir is absent from a cpcache entry written by an older jolt; JOLT_PWD
   ;; is where the user invoked us, which is the same directory in the common case.
   (load-natives! natives strict? (or project-dir (jolt.host/getenv "JOLT_PWD")))))

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
                                 :trace? (contains? #{:tree :trace-file} *cli-report*)}
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

(defn- run-ns
  "Require ns-name and invoke its -main with the string app args. A leading
  standalone \"--\" in app-args is consumed as POSIX end-of-options, so this is
  the single end-of-options point for every ns-based entry form — `run -m`,
  `-m`, `-M`/`-A` aliases, and a :main-opts task all route through here."
  [ns-name app-args]
  (let [app-args (drop-end-of-options app-args)]
    (push-thread-bindings {#'clojure.core/*command-line-args* (seq app-args)})
    (require (symbol ns-name))
    (if-let [mainv (ns-resolve (symbol ns-name) (symbol "-main"))]
      (apply (deref mainv) app-args)
      (throw (ex-info (str "namespace " ns-name " has no -main") {:ns ns-name})))))

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
(defn- file-arg [x]
  (cond
    (= "-" x) "/dev/stdin"
    (str/starts-with? x "/") x
    :else (str (project-dir) "/" x)))

;; main-opts is a vector like ["-m" "app.core"] or ["-e" "(prn :hi)"] (optionally
;; with trailing args). The user-supplied extra args are appended, so an alias's
;; :main-opts and the command line combine exactly like the clj CLI, which
;; prepends the alias's :main-opts to the argv.
(defn- apply-main-opts [main-opts extra-args]
  (let [opts (concat main-opts extra-args)]
    (cond
      (= "-m" (first opts)) (run-ns (second opts) (drop 2 opts))
      (= "-e" (first opts)) (eval-expr-string (second opts) (drop 2 opts) true)
      ;; clojure.main: a bare non-option first arg is a script path and the
      ;; rest is *command-line-args* — `jolt -M file.clj a b`, or an alias
      ;; whose :main-opts name a script. Same load shape as `run FILE`.
      (and (string? (first opts))
           (seq (first opts))
           (not (str/starts-with? (first opts) "-")))
      (do (push-thread-bindings
            {#'clojure.core/*command-line-args* (seq (drop-end-of-options (rest opts)))})
          (load-file (file-arg (first opts)))
          nil)
      :else (throw (ex-info (str "unsupported :main-opts " (pr-str (vec opts))) {})))))

(defn- parse-aliases [s]            ; "-M:a:b" / ":a:b" -> [:a :b]
  (let [s (if (str/starts-with? s "-") (subs s 2) s)]
    (->> (str/split s #":") (remove str/blank?) (map keyword) vec)))

;; Does a bare argv token name a file to run (rather than a deps.edn task)? A "-"
;; (stdin), an existing file, or a *.jolt/*.clj/*.cljc/*.cljs path. .jolt is the
;; same language as .clj and only marks a file as using jolt-specific interop
;; rather than portable Clojure.
;; A directory is never a file to run, however much it looks like one: `test` is a
;; :tasks entry AND a directory in every jolt project, and file-exists? answers #t
;; for a directory, so `jolt test` used to be dispatched here and die in
;; load-file's decoder ("failed on #<binary input port test>: is a directory")
;; rather than running the task.
(defn- run-file-arg? [x]
  (and (not (jolt.host/directory? x))
       (or (= "-" x)
           (some #(str/ends-with? x %) [".jolt" ".clj" ".cljc" ".cljs"])
           (jolt.host/file-exists? x))))

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

      (and (seq more) (not (run-file-arg? (first more))))
      (run-task (first more) (rest more) parallel?)

      (seq more)
      (do (apply-project! (resolve-current))
          (push-thread-bindings
            {#'clojure.core/*command-line-args* (seq (drop-end-of-options (rest more)))})
          (load-file (file-arg (first more))) nil)

      :else (throw (ex-info "run needs -m NS, a FILE, or a task name" {})))))

;; -M[:alias…] [main-opts…] — resolve with the aliases (plus any bound by a
;; leading -A), then run their :main-opts followed by the command-line ones. With
;; no :main-opts anywhere in the selected aliases the command line supplies them
;; on its own, so a bare `jolt -M -e EXPR` (or `-M -m NS`) works like clj — only
;; an empty command line with no :main-opts is an error.
(defn- cmd-M [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [main-opts] :as resolved} (resolve-current aliases)]
    (with-project aliases resolved
      (fn []
        (if (or (seq main-opts) (seq more))
          (apply-main-opts main-opts more)
          (throw (ex-info (str "alias(es) " (pr-str aliases) " have no :main-opts") {})))))))

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
  (let [fsym (qualify-fn (or fsym-arg (:exec-fn argmap)) argmap)
        _ (when-not fsym (throw (ex-info "No function to execute: supply ns/fn or an alias with :exec-fn" {})))
        exec-args (parse-exec-args (:exec-args argmap) args)]
    (require (symbol (namespace fsym)))
    (if-let [v (resolve fsym)]
      ((deref v) exec-args)
      (throw (ex-info (str "Function not found: " fsym) {:fn fsym})))))

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

(defn- repl-form-complete?
  "True when `s` has balanced ()/[]/{}, no open string/char/regex, and at most
  a trailing comment past the last form. Drives the REPL's read-until-complete
  decision so a form split across lines is accumulated, not evaluated half-read."
  [s]
  (let [n (count s)]
    (loop [i 0 depth 0 state :code]               ; state: :code :string :regex :comment
      (if (>= i n)
        (and (<= depth 0) (#{:code :comment} state))
        (let [c (get s i)]
          (case state
            :code    (cond
                       (= c \;)        (recur (inc i) depth :comment)
                       (= c \\)        (recur (+ i 2) depth :code)     ; char literal: \(
                       (= c \")        (recur (inc i) depth :string)
                       (= c \#)        (if (= (get s (inc i)) \")
                                         (recur (+ i 2) depth :regex)   ; consume the #" together
                                         (recur (inc i) depth :code))
                       (#{\( \[ \{} c) (recur (inc i) (inc depth) :code)
                       (#{\) \] \}} c) (recur (inc i) (dec depth) :code)
                       :else           (recur (inc i) depth :code))
            :string  (cond
                       (= c \\) (recur (+ i 2) depth :string)          ; escaped char
                       (= c \") (recur (inc i) depth :code)
                       :else    (recur (inc i) depth :string))
            :regex   (cond
                       (= c \\) (recur (+ i 2) depth :regex)
                       (= c \") (recur (inc i) depth :code)
                       :else    (recur (inc i) depth :regex))
            :comment (recur (inc i) depth
                            (if (#{\newline \return} c) :code :comment))))))))

(defn- repl-read-form []
  ;; Read lines — printing a secondary prompt for continuations — until the
  ;; accumulated buffer is a complete form. Returns the (possibly multi-line)
  ;; buffer, or nil on EOF at the primary prompt.
  (loop [buf nil]
    (print (if buf "... " "user=> ")) (flush)
    (let [line (read-line)]
      (cond
        (nil? line) buf                                 ; EOF: nil at primary, partial mid-form
        (nil? buf)  (cond
                      (str/blank? line)        (recur nil)      ; skip a blank first line
                      (repl-form-complete? line) line
                      :else                    (recur line))
        :else       (let [nb (str buf "\n" line)]
                      (if (repl-form-complete? nb) nb (recur nb)))))))

(defn- repl []
  ;; resolve the project so deps (git libs) are on the roots and native libs are
  ;; loaded — same context a run gets, so (require '[some.lib]) works in the REPL.
  (try (apply-project! (resolve-current))
       (catch :default _ nil))
  ;; REPL-driven development: trace by default so an uncaught error in evaluated
  ;; code shows a tail-frame backtrace, no JOLT_TRACE needed (JOLT_TRACE=0 opts out).
  (jolt.host/enable-trace!)
  (println (str ";; jolt " (version) " repl — :repl/quit or ^D to exit"))
  ;; *repl* reads true inside the session, and the result/exception history
  ;; vars update per evaluation, like clojure.main's REPL.
  (push-thread-bindings {#'clojure.core/*repl* true
                         #'clojure.core/*1 nil #'clojure.core/*2 nil
                         #'clojure.core/*3 nil #'clojure.core/*e nil})
  (loop []
    (let [form (repl-read-form)]
      (when form
        ;; :repl/quit / :exit exit the loop — a reliable gesture that works in any
        ;; terminal, unlike ^D (some terminals/editors don't deliver it as EOF).
        (if (#{:repl/quit :exit} (try (read-string form) (catch :default _ nil)))
          nil
          (do
            (try (let [v (load-string form)]
                   (var-set #'clojure.core/*3 *2)
                   (var-set #'clojure.core/*2 *1)
                   (var-set #'clojure.core/*1 v)
                   (println (pr-str v)))
                 (catch :default e
                   (var-set #'clojure.core/*e e)
                   (println "error:" (or (ex-message e)
                                         (try ((resolve 'jolt.host/condition-message) e) (catch :default _ nil))
                                         (pr-str e)))
                   (when-let [bt (jolt.host/backtrace-string)]
                     (print bt))))
            (recur)))))))

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
    ;; tolerant: see load-natives! — the task may be the build step for a
    ;; :jolt/native library that does not exist yet
    (apply-project! resolved false)
    ((requiring-resolve 'jolt.tasks/run-task!)
     {:tasks (:tasks resolved)
      :name name
      ;; :args verbatim — a :main-opts task hands them to run-ns, which is the
      ;; one end-of-options point for every ns entry form and consumes the
      ;; leading "--" itself. :app-args is the same list with it consumed, for
      ;; the *command-line-args* a code body sees. Dropping it in both places
      ;; would eat a second standalone "--" that is the program's own data.
      :args (vec more)
      :app-args (vec (drop-end-of-options more))
      :parallel parallel?
      :run-main-opts apply-main-opts})))

;; `jolt tasks` — the listing. Reads the :tasks maps alone: naming the tasks
;; needs no dependency expansion, and a project whose deps don't resolve should
;; still be able to say what it can run.
(defn- cmd-tasks []
  ((requiring-resolve 'jolt.tasks/list-tasks!) (deps/project-tasks (project-dir))))

;; babashka's :override-builtin — a task only displaces the jolt command of the
;; same name when it says so. Checked from the :tasks maps directly, so it costs
;; a small file read rather than loading the task runner on every command.
(defn- builtin-overridden? [cmd]
  (let [t (get (deps/project-tasks (project-dir)) (symbol cmd))]
    (boolean (and (map? t) (:override-builtin t)))))

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
;; The static-link description of a :jolt/native spec for this platform, or nil.
;; :static may be flat ({:archive "…"} / {:lib "z" :libdir "…"}) or per-platform
;; ({:darwin {…} :linux {…}}). Returns a vector build.ss reads and wraps in the
;; platform's force-load flags: ["archive" abspath] or ["lib" name libdir].
(defn- static-link-spec [spec plat]
  (when-let [s (:static spec)]
    (let [p (get s plat)
          s (if (map? p) p s)]
      (cond
        (:archive s) ["archive" (:archive s)]
        (:lib s)     ["lib" (:lib s) (or (:libdir s) "")]
        :else        nil))))

;; Encode a deps.edn :jolt/native spec for the build launcher, resolving the
;; current platform's candidate list now (the binary runs on this OS). Each entry
;; becomes a vector the launcher (build.ss) reads:
;;   ["process"]            — the running binary's own symbols (libc)
;;   ["static" form …]      — the lib's archive, cc-linked into the binary; its
;;                            symbols load from the process (default when :static
;;                            is present and --dynamic wasn't passed)
;;   ["req"|"opt" cand…]    — load a shared object at runtime, trying each in turn
;; dynamic? forces the runtime path for every lib (the --dynamic build flag).
(defn- encode-natives [natives dynamic?]
  (let [plat (current-platform)]
    (vec (for [spec natives]
           (let [static (and (not dynamic?) (static-link-spec spec plat))]
             (cond
               (:process spec) ["process"]
               static          (into ["static"] static)
               :else           (let [c (get spec plat)
                                     cands (if (string? c) [c] (vec c))]
                                 (into [(if (:optional spec) "opt" "req")] cands))))))))

(defn- cmd-build [more]
  (let [{:keys [project-paths embed-dirs build] :as resolved}
        (resolve-current)]
    (apply-project! resolved)
    (let [opts (loop [a more, entry nil, out nil, target nil, tpack nil, end-opts? false]
                 (let [cur (first a)]
                   (cond
                     (empty? a)                              {:entry entry :out out :target target :target-pack tpack}
                     (and (not end-opts?) (= "--" cur))      (recur (rest a) entry out target tpack true)
                     (and (not end-opts?) (= "-m" cur))      (recur (drop 2 a) (second a) out target tpack false)
                     (and (not end-opts?) (= "-o" cur))      (recur (drop 2 a) entry (second a) target tpack false)
                     ;; cross-compilation: --target <machine> [--target-pack <dir>]
                     (and (not end-opts?) (= "--target" cur))      (recur (drop 2 a) entry out (second a) tpack false)
                     (and (not end-opts?) (= "--target-pack" cur)) (recur (drop 2 a) entry out target (second a) false)
                     (and (not end-opts?) (str/starts-with? cur "-")) (recur (rest a) entry out target tpack false)
                     :else                                   (recur (rest a) (or entry cur) out target tpack end-opts?))))
          entry (:entry opts)
          ;; flags are only recognized before the end-of-options marker
          flag-args (take-while #(not= "--" %) more)
          mode  (cond (some #{"--opt"} flag-args) "optimized"
                      (some #{"--dev"} flag-args) "dev"
                      (:opt build)                "optimized"
                      :else                       "release")]
      (when (nil? entry)
        (throw (ex-info "build needs an entry: -m NS" {})))
      ;; Output paths resolve against the project dir (JOLT_PWD), not the CLI's
      ;; cwd — bin/jolt cd's to the jolt repo, so a bare relative path would land
      ;; there. Default output is cargo-style under target/: --dev -> target/debug,
      ;; release/--opt -> target/release, the binary named after the project dir
      ;; (falling back to the entry's first segment). The <name>.build scratch dir
      ;; the driver creates sits next to it, so it lands under the same target dir.
      ;; An explicit -o is honored: absolute as-is, relative against the project.
      (let [pdir (project-dir)
            proj (let [seg (last (str/split pdir #"/"))]
                   (if (or (str/blank? seg) (= "." seg)) (first (str/split entry #"\.")) seg))
            out (let [o (:out opts)]
                  (cond
                    (nil? o) (str pdir "/target/" (if (= mode "dev") "debug" "release") "/" proj)
                    (str/starts-with? o "/") o
                    :else (str pdir "/" o)))
            ;; :jolt/native libs with a :static archive are cc-linked into the
            ;; binary by default; --dynamic (or deps.edn :jolt/build {:dynamic-natives
            ;; true}) keeps the old behavior — load a shared object at runtime.
            dynamic-natives? (boolean (or (some #{"--dynamic"} flag-args) (:dynamic-natives build)))
            natives (encode-natives (:natives resolved) dynamic-natives?)
            ;; closed-world direct-linking is the release default: ON for release and
            ;; optimized (the throughput lever), OFF for --dev. --no-direct-link (or
            ;; deps.edn :jolt/build {:direct-link false}) opts back out; --direct-link
            ;; is a now-redundant alias. ^:redef/^:dynamic defs always stay var-routed.
            no-dl?       (or (some #{"--no-direct-link"} flag-args) (false? (:direct-link build)))
            direct-link? (and (not (= mode "dev")) (not no-dl?))
            ;; tree-shaking (drop library code not reachable from -main): --tree-shake
            ;; or deps.edn :jolt/build {:tree-shake true}.
            tree-shake? (boolean (or (some #{"--tree-shake"} flag-args) (:tree-shake build)))
            ;; a shared library (callable from C/C++/Rust via jolt_library_init +
            ;; jolt_lookup) instead of an executable: --library.
            library? (some #{"--library"} flag-args)
            ;; cross-compilation (--target <machine>): the output binary is for a
            ;; different Chez machine. Needs a prepared target pack (--target-pack
            ;; DIR or $JOLT_TARGET_PACK) — see tools/cross-compile/README.md.
            target (:target opts)
            target-pack (or (:target-pack opts) (System/getenv "JOLT_TARGET_PACK"))]
        (when (and target (nil? target-pack))
          (throw (ex-info "--target needs a target pack: --target-pack DIR (or $JOLT_TARGET_PACK)" {:target target})))
        ;; embed-dirs (absolute) are walked + baked into the binary by the driver;
        ;; project-paths (relative) become runtime io/resource roots (ship-alongside).
        (if library?
          (jolt.host/build-library entry out mode natives embed-dirs project-paths direct-link? tree-shake? target target-pack)
          (jolt.host/build-binary entry out mode natives embed-dirs project-paths direct-link? tree-shake? target target-pack))))))

(defn- nrepl [more]
  ;; resolve the project (deps on the roots, native libs loaded), then start the
  ;; nREPL server so an editor can connect and (require '[some.lib]) live. A
  ;; library's middleware (deps.edn :nrepl/middleware) is composed over the
  ;; built-in handler — sessions / interruptible-eval / completion etc.
  (let [resolved (resolve-current)]
    (apply-project! resolved)
    (let [raw-port (first (filter #(not (str/starts-with? % "-")) more))
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
      ;; handler, and the server hangs. park-until-interrupt unblocks SIGINT here
      ;; once its own ^C handler is installed, so ^C reaches this thread and the
      ;; shutdown hooks run cleanly.
      (jolt.host/block-sigint)
      (let [stop ((resolve 'jolt.nrepl/start) port (:nrepl-middleware resolved))]
        ;; register stop so ^C (handled by park-until-interrupt) closes the socket
        ;; and drops .nrepl-port on the way out.
        (jolt.host/add-shutdown-hook stop)
        ;; park here until ^C (handled by park-until-interrupt's keyboard-interrupt-
        ;; handler, which runs the shutdown hooks and exits). The accept loop
        ;; inherited SIGINT-blocked above, so ^C is delivered to this thread.
        (jolt.host/park-until-interrupt)
        (when stop (stop))))))

(defn- usage []
  (println (str "jolt " (version)))
  (println "usage: jolt [command] [args]")
  (println)
  (println "With no command, starts a REPL.")
  (println)
  (println "commands:")
  (println "  repl                   start a REPL")
  (println "  nrepl-server [port]    start an nREPL server (default 7888) for editors")
  (println "  run -m NS [args]       resolve deps.edn, load NS, call its -main")
  (println "  run FILE [args]        load a Clojure file")
  (println "  build -m NS [-o OUT] [--opt|--dev] [--direct-link] [--tree-shake] [--dynamic]")
  (println "              [--library] [--target MACHINE --target-pack DIR]")
  (println "                         compile a standalone binary, or with --library a")
  (println "                         shared object an embedder dlopens and calls through")
  (println "                         jolt_library_init + jolt_lookup; --target")
  (println "                         cross-compiles either one for another Chez machine")
  (println "                         (see tools/cross-compile)")
  (println "  path                   print the resolved source roots")
  (println "  tasks                  list the project's bb.edn/deps.edn :tasks")
  (println "  <task> [args]          run a task (`run <task>` and `run --parallel")
  (println "                         <task>` do the same)")
  (println "  help, --help, -h       print this message")
  (println "  version, --version, -V print the jolt version")
  (println)
  (println "options:")
  (println "  -e EXPR [args]         evaluate EXPR and print the result")
  (println "  -e - [args]            evaluate an EXPR read from stdin")
  (println "  - [args]               run a program read from stdin (as a script)")
  (println "  -m NS [args]           shorthand for run -m")
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
  (println "  -Stree                 print the dependency tree")
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
;; option (-Spath / -Stree / -Sdescribe / -P) still lets these dispatch, since
;; they change the answer, and skips everything else: a report runs no program.
(defn- context-arg? [cmd]
  (boolean (and cmd (or (#{"-Sdeps" "-Scp" "-Srepro" "-Sforce" "-Sthreads" "-Sverbose"
                           "-Spath" "-Stree" "-Strace" "-Sdescribe"} cmd)
                        (some #(str/starts-with? cmd %) ["-A" "-M" "-X" "-T" "-J"])))))

(def ^:private report-opts
  {"-Spath" :path, "-Stree" :tree, "-Strace" :trace-file,
   "-Sdescribe" :describe, "-P" :prepare})

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

      ;; -Srepro — resolve from the project deps.edn alone, ignoring the user
      ;; one, so a build is reproducible on a machine with a different
      ;; ~/.clojure/deps.edn. (JOLT_NO_USER_DEPS does the same by environment.)
      (= cmd "-Srepro")
      (binding [*cli-repro?* true] (apply -main more))

      ;; -Scp CP — run against these source roots and expand no dependencies.
      ;; The deps.edn chain is still read (aliases, :main-opts, :tasks), so a
      ;; recorded classpath can drive a run offline: -Scp "$(jolt -Spath)".
      (= cmd "-Scp")
      (let [[cp & rest-args] more]
        (when (nil? cp) (throw (ex-info "-Scp needs a classpath argument" {})))
        (binding [*cli-cp* (vec (remove str/blank? (str/split cp #":")))]
          (apply -main rest-args)))

      ;; -Sverbose — say where the resolution reads from and fetches into, then
      ;; run it with the progress lines on (the JOLT_DEBUG stream, for one run).
      (= cmd "-Sverbose")
      (binding [deps/*verbose* true]
        (print-verbose-env)
        (apply -main more))

      ;; Accepted and ignored, so tooling that always passes them still works:
      ;; jolt computes its roots on every run (there is no classpath cache to
      ;; force or bypass), fetches serially, and has no JVM to pass options to.
      (= cmd "-Sforce")                  (apply -main more)
      (= cmd "-Sthreads")                (apply -main (rest more))
      (str/starts-with? cmd "-J")        (apply -main more)

      (#{"help" "--help" "-h"} cmd)      (usage)
      (#{"version" "--version" "-V"} cmd) (println (str "jolt " (version)))

      ;; A task may take a built-in command's name, but only by asking to
      ;; (babashka's :override-builtin). Checked here, after the two commands
      ;; that read no project at all, so it costs nothing a command doesn't
      ;; already pay: everything below resolves the project anyway.
      (and (#{"run" "repl" "nrepl-server" "path" "build" "tasks"} cmd)
           (builtin-overridden? cmd))
      (run-task cmd more false)

      (= cmd "run")                      (cmd-run more)
      (= cmd "repl")                     (repl)
      (= cmd "nrepl-server")             (nrepl more)
      (= cmd "path")                     (cmd-path)
      (= cmd "tasks")                    (cmd-tasks)
      ;; -Sdeps '<edn>' — an extra deps.edn map merged last into the chain,
      ;; bound around the re-dispatch of the remaining argv.
      (= cmd "-Sdeps")
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
      (= cmd "-e")
      (do (apply-project! (resolve-current))
          (eval-expr-string (first more) (rest more) true))
      (= cmd "-")
      (do (apply-project! (resolve-current))
          (eval-expr-string "-" more false))
      (str/starts-with? cmd "-M")        (cmd-M cmd more)
      (str/starts-with? cmd "-A")        (cmd-A cmd more)
      (str/starts-with? cmd "-X")        (cmd-X cmd more)
      (str/starts-with? cmd "-T")        (cmd-T cmd more)
      (= cmd "-m")                       (cmd-run (cons "-m" more))
      (= cmd "build")                    (cmd-build more)
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
      (run-file-arg? cmd)                (cmd-run (cons cmd more))
      :else                              (run-task cmd more false))))
