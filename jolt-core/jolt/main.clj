(ns jolt.main
  "The jolt CLI dispatch: resolve a project's deps.edn, set the source roots, and
  run a namespace's -main, a file, a deps.edn task, or a REPL. Driven by cli.ss,
  which hands it the raw argv; the project directory is JOLT_PWD (the user's cwd
  before the launcher cd'd to the jolt repo)."
  (:require [jolt.deps :as deps]
            [clojure.string :as str]))

(defn- project-dir [] (or (jolt.host/getenv "JOLT_PWD") "."))

;; Apply a resolved project's roots on top of the current (jolt-core) roots so app
;; namespaces resolve while jolt.* stays loadable.
(defn- apply-roots! [roots]
  (jolt.host/set-source-roots! (vec (distinct (concat roots (jolt.host/source-roots))))))

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
  (let [{:keys [roots]} (deps/resolve-project (project-dir))]
    (apply-roots! roots)
    (cond
      (= "-m" (first more)) (run-ns (second more) (drop 2 more))
      (seq more)            (do (load-file (first more)) nil)
      :else (throw (ex-info "run needs -m NS or a FILE" {})))))

;; -M:alias…  — resolve with the aliases, run their :main-opts
(defn- cmd-M [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [roots main-opts]} (deps/resolve-project (project-dir) aliases)]
    (apply-roots! roots)
    (if main-opts
      (apply-main-opts main-opts more)
      (throw (ex-info (str "alias(es) " (pr-str aliases) " have no :main-opts") {})))))

;; -A:alias… — add the aliases' paths/deps, then run the remaining argv as a command
(defn- cmd-A [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [roots]} (deps/resolve-project (project-dir) aliases)]
    (apply-roots! roots)
    (when (seq more) (run-ns (second more) (drop 2 more)))))

(defn- cmd-path []
  (let [{:keys [roots]} (deps/resolve-project (project-dir))]
    (println (str/join ":" roots))))

(defn- repl []
  (println ";; jolt repl — ^D to exit")
  (loop []
    (print "user=> ") (flush)
    (let [line (read-line)]
      (when line
        (try (println (pr-str (load-string line)))
             (catch :default e (println "error:" (ex-message e))))
        (recur)))))

;; A deps.edn :tasks entry: a string is a shell command; a map is {:main-opts …}.
(defn- run-task [name more]
  (let [{:keys [roots tasks]} (deps/resolve-project (project-dir))
        task (get tasks (symbol name))]
    (cond
      (nil? task) (throw (ex-info (str "unknown command or task: " name) {:name name}))
      (string? task) (jolt.host/sh task)
      (map? task) (do (apply-roots! roots) (apply-main-opts (:main-opts task) more))
      :else (throw (ex-info (str "bad task " name) {})))))

(defn- usage []
  (println "usage: jolt <command> [args]")
  (println "  run -m NS [args]   resolve deps.edn, load NS, call its -main")
  (println "  run FILE           load a Clojure file")
  (println "  -M:alias [args]    run the alias's :main-opts")
  (println "  -A:alias [args]    add the alias's paths/deps")
  (println "  repl               start a line REPL")
  (println "  path               print the resolved source roots")
  (println "  <task>             run a deps.edn :tasks entry"))

(defn -main [& args]
  (let [[cmd & more] args]
    (cond
      (nil? cmd)                  (usage)
      (= cmd "run")               (cmd-run more)
      (= cmd "repl")              (repl)
      (= cmd "path")              (cmd-path)
      (str/starts-with? cmd "-M") (cmd-M cmd more)
      (str/starts-with? cmd "-A") (cmd-A cmd more)
      (= cmd "-m")                (cmd-run (cons "-m" more))
      :else                       (run-task cmd more))))
