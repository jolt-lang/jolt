(ns jolt.main
  "The jolt CLI dispatch: resolve a project's deps.edn, set the source roots, and
  run a namespace's -main, a file, a deps.edn task, or a REPL. Driven by cli.ss,
  which hands it the raw argv; the project directory is JOLT_PWD (the user's cwd
  before the launcher cd'd to the jolt repo)."
  (:require [jolt.deps :as deps]
            [clojure.string :as str]))

(defn- project-dir [] (or (jolt.host/getenv "JOLT_PWD") "."))

;; Load a library's declared native shared objects (deps.edn :jolt/native) before
;; its Clojure is required, so its foreign-fn bindings resolve. Each entry is a
;; map: {:name "sqlite3" :darwin ["libsqlite3.0.dylib" ...] :linux ["libsqlite3.so.0" ...]}
;; with optional :optional (missing is fine — a feature-gated dep) and :process
;; (use the running process's symbols, e.g. libc sockets — no external file).
(defn- load-natives! [natives]
  (when (seq natives)
    (let [os (str/lower-case (or (System/getProperty "os.name") ""))
          plat (cond (str/includes? os "mac") :darwin
                     (str/includes? os "win") :windows
                     :else :linux)]
      (doseq [spec natives]
        (if (:process spec)
          (jolt.ffi/load-library)
          (let [c (get spec plat)
                cands (if (string? c) [c] (vec c))
                hit (some #(when (jolt.ffi/loaded? %) %) cands)]
            (when (and (nil? hit) (not (:optional spec)))
              (throw (ex-info (str "required native library "
                                   (or (:name spec) (first cands) "?")
                                   " not found — tried " (pr-str cands) " for " (name plat))
                              {:native spec})))))))))

;; Apply a resolved project's roots on top of the current (jolt-core) roots so app
;; namespaces resolve while jolt.* stays loadable, then load its native deps.
(defn- apply-project! [{:keys [roots natives]}]
  (jolt.host/set-source-roots! (vec (distinct (concat roots (jolt.host/source-roots)))))
  (load-natives! natives))

(defn- run-ns
  "Require ns-name and invoke its -main with the string app args."
  [ns-name app-args]
  (require (symbol ns-name))
  (if-let [mainv (ns-resolve (symbol ns-name) (symbol "-main"))]
    (apply (deref mainv) app-args)
    (throw (ex-info (str "namespace " ns-name " has no -main") {:ns ns-name}))))

;; main-opts is a vector like ["-m" "app.core"] (optionally trailing args). Apply
;; it with the user-supplied extra args appended.
(defn- apply-main-opts [main-opts extra-args]
  (cond
    (and (seq main-opts) (= "-m" (first main-opts)))
    (run-ns (second main-opts) (concat (drop 2 main-opts) extra-args))
    :else
    (throw (ex-info (str "unsupported :main-opts " (pr-str main-opts)) {}))))

(defn- parse-aliases [s]            ; "-M:a:b" / ":a:b" -> [:a :b]
  (let [s (if (str/starts-with? s "-") (subs s 2) s)]
    (->> (str/split s #":") (remove str/blank?) (map keyword) vec)))

;; run [-m NS args… | FILE]
(defn- cmd-run [more]
  (apply-project! (deps/resolve-project (project-dir)))
  (cond
    (= "-m" (first more)) (run-ns (second more) (drop 2 more))
    (seq more)            (do (load-file (first more)) nil)
    :else (throw (ex-info "run needs -m NS or a FILE" {}))))

;; -M:alias…  — resolve with the aliases, run their :main-opts
(defn- cmd-M [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [main-opts] :as resolved} (deps/resolve-project (project-dir) aliases)]
    (apply-project! resolved)
    (if main-opts
      (apply-main-opts main-opts more)
      (throw (ex-info (str "alias(es) " (pr-str aliases) " have no :main-opts") {})))))

;; -A:alias… — add the aliases' paths/deps, then run the remaining argv as a command
(defn- cmd-A [arg more]
  (let [aliases (parse-aliases arg)]
    (apply-project! (deps/resolve-project (project-dir) aliases))
    (when (seq more) (run-ns (second more) (drop 2 more)))))

(defn- cmd-path []
  (let [{:keys [roots]} (deps/resolve-project (project-dir))]
    (println (str/join ":" roots))))

(defn- repl []
  ;; resolve the project so deps (git libs) are on the roots and native libs are
  ;; loaded — same context a run gets, so (require '[some.lib]) works in the REPL.
  (try (apply-project! (deps/resolve-project (project-dir)))
       (catch :default _ nil))
  (println ";; jolt repl — ^D to exit")
  (loop []
    (print "user=> ") (flush)
    (let [line (read-line)]
      (when line
        (try (println (pr-str (load-string line)))
             (catch :default e
               (println "error:" (or (ex-message e)
                                     (try ((resolve 'jolt.host/condition-message) e) (catch :default _ nil))
                                     (pr-str e)))))
        (recur)))))

;; A deps.edn :tasks entry: a string is a shell command; a map is {:main-opts …}.
(defn- run-task [name more]
  (let [{:keys [tasks] :as resolved} (deps/resolve-project (project-dir))
        task (get tasks (symbol name))]
    (cond
      (nil? task) (throw (ex-info (str "unknown command or task: " name) {:name name}))
      (string? task) (jolt.host/sh task)
      (map? task) (do (apply-project! resolved) (apply-main-opts (:main-opts task) more))
      :else (throw (ex-info (str "bad task " name) {})))))

;; build [-m NS | FILE] [-o OUT] [--opt | --dev] — AOT-compile the app into a
;; standalone executable. Resolves deps + roots like `run`, then hands the entry
;; namespace to the host build driver (jolt.host/build-binary, defined by
;; build.ss). Default mode is release; --opt selects optimized, --dev unoptimized.
(defn- cmd-build [more]
  (apply-project! (deps/resolve-project (project-dir)))
  (let [opts (loop [a more, entry nil, out nil]
               (cond
                 (empty? a)          {:entry entry :out out}
                 (= "-m" (first a))  (recur (drop 2 a) (second a) out)
                 (= "-o" (first a))  (recur (drop 2 a) entry (second a))
                 (str/starts-with? (first a) "-") (recur (rest a) entry out)
                 :else               (recur (rest a) (or entry (first a)) out)))
        entry (:entry opts)
        mode  (cond (some #{"--opt"} more) "optimized"
                    (some #{"--dev"} more) "dev"
                    :else                  "release")]
    (when (nil? entry)
      (throw (ex-info "build needs an entry: -m NS" {})))
    (let [out (or (:out opts) (first (str/split entry #"\.")))]
      (jolt.host/build-binary entry out mode))))

(defn- nrepl [more]
  ;; resolve the project (deps on the roots, native libs loaded), then start the
  ;; nREPL server so an editor can connect and (require '[some.lib]) live. A
  ;; library's middleware (deps.edn :nrepl/middleware) is composed over the
  ;; built-in handler — sessions / interruptible-eval / completion etc.
  (let [resolved (deps/resolve-project (project-dir))]
    (apply-project! resolved)
    (let [port (or (some-> (first (filter #(not (str/starts-with? % "-")) more)) parse-long)
                   (parse-long (or (jolt.host/getenv "JOLT_NREPL_PORT") "7888")))]
      (require 'jolt.nrepl)
      ((resolve 'jolt.nrepl/start) port (:nrepl-middleware resolved)))))

(defn- usage []
  (println "usage: jolt <command> [args]")
  (println "  run -m NS [args]   resolve deps.edn, load NS, call its -main")
  (println "  run FILE           load a Clojure file")
  (println "  build -m NS [-o OUT] [--opt|--dev]  compile a standalone binary")
  (println "  -M:alias [args]    run the alias's :main-opts")
  (println "  -A:alias [args]    add the alias's paths/deps")
  (println "  repl               start a line REPL")
  (println "  nrepl [port]       start an nREPL server (default 7888) for editors")
  (println "  path               print the resolved source roots")
  (println "  <task>             run a deps.edn :tasks entry"))

(defn -main [& args]
  (let [[cmd & more] args]
    (cond
      (nil? cmd)                  (usage)
      (= cmd "run")               (cmd-run more)
      (= cmd "repl")              (repl)
      (= cmd "nrepl")             (nrepl more)
      (= cmd "path")              (cmd-path)
      (str/starts-with? cmd "-M") (cmd-M cmd more)
      (str/starts-with? cmd "-A") (cmd-A cmd more)
      (= cmd "-m")                (cmd-run (cons "-m" more))
      (= cmd "build")             (cmd-build more)
      :else                       (run-task cmd more))))
