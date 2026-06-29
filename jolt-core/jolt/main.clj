(ns jolt.main
  "The jolt CLI dispatch: resolve a project's deps.edn, set the source roots, and
  run a namespace's -main, a file, a deps.edn task, or a REPL. Driven by cli.ss,
  which hands it the raw argv; the project directory is JOLT_PWD (the user's cwd
  before the launcher cd'd to the jolt repo)."
  (:require [jolt.deps :as deps]
            [clojure.string :as str]))

(defn- project-dir [] (or (jolt.host/getenv "JOLT_PWD") "."))

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
(defn- load-natives! [natives]
  (when (seq natives)
    (let [plat (current-platform)]
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

;; build [-m NS | FILE] [-o OUT] [--opt | --dev] [--direct-link] — AOT-compile the
;; app into a standalone executable. Resolves deps + roots like `run`, then hands the
;; entry namespace to the host build driver (jolt.host/build-binary, defined by
;; build.ss). Default mode is release; --opt selects optimized, --dev unoptimized.
;; --direct-link (or deps.edn :jolt/build {:direct-link true}) opts into closed-world
;; direct-linking: app->app calls bind directly, giving up runtime redefinition of
;; those vars and eval/load-string. Off by default — release stays dynamically linked.
;; Encode a deps.edn :jolt/native spec for the build launcher, resolving the
;; current platform's candidate list now (the binary runs on this OS). Each entry
;; becomes a vector the launcher (build.ss) reads: ["process"] for the running
;; binary's own symbols, else ["req"|"opt" cand…] to try in turn.
(defn- encode-natives [natives]
  (let [plat (current-platform)]
    (vec (for [spec natives]
           (if (:process spec)
             ["process"]
             (let [c (get spec plat)
                   cands (if (string? c) [c] (vec c))]
               (into [(if (:optional spec) "opt" "req")] cands)))))))

(defn- cmd-build [more]
  (let [{:keys [project-paths embed-dirs build] :as resolved}
        (deps/resolve-project (project-dir))]
    (apply-project! resolved)
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
      ;; Output paths resolve against the project dir (JOLT_PWD), not the CLI's
      ;; cwd — bin/joltc cd's to the jolt repo, so a bare relative path would land
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
            natives (encode-natives (:natives resolved))
            ;; closed-world direct-linking is opt-in: the --direct-link flag or a
            ;; deps.edn :jolt/build {:direct-link true}. Off otherwise.
            direct-link? (boolean (or (some #{"--direct-link"} more) (:direct-link build)))
            ;; tree-shaking (drop library code not reachable from -main): --tree-shake
            ;; or deps.edn :jolt/build {:tree-shake true}.
            tree-shake? (boolean (or (some #{"--tree-shake"} more) (:tree-shake build)))]
        ;; embed-dirs (absolute) are walked + baked into the binary by the driver;
        ;; project-paths (relative) become runtime io/resource roots (ship-alongside).
        (jolt.host/build-binary entry out mode natives embed-dirs project-paths direct-link? tree-shake?)))))

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
      ;; Run the accept loop on a worker thread so the primordial (main) thread
      ;; stays free to own the GUI main loop. glimmer's run marshals its startup
      ;; onto this thread via jolt.host/call-on-main-thread — on macOS GTK quartz,
      ;; g_application_run must run on the main thread or AppKit aborts when it
      ;; sets the main menu.
      (future ((resolve 'jolt.nrepl/start) port (:nrepl-middleware resolved)))
      (jolt.host/run-main-pump))))

(defn- usage []
  (println "usage: jolt <command> [args]")
  (println "  run -m NS [args]       resolve deps.edn, load NS, call its -main")
  (println "  run FILE               load a Clojure file")
  (println "  build -m NS [-o OUT] [--opt|--dev] [--direct-link] [--tree-shake]  compile a standalone binary")
  (println "  -M:alias [args]        run the alias's :main-opts")
  (println "  -A:alias [args]        add the alias's paths/deps")
  (println "  repl                   start a line REPL")
  (println "  --nrepl-server [port]  start an nREPL server (default 7888) for editors")
  (println "  path                   print the resolved source roots")
  (println "  <task>                 run a deps.edn :tasks entry")
  (println "  --help                 print this message"))

(defn -main [& args]
  (let [[cmd & more] args]
    (cond
      (nil? cmd)                  (usage)
      (= cmd "--help")            (usage)
      (= cmd "-h")                (usage)
      (= cmd "run")               (cmd-run more)
      (= cmd "repl")              (repl)
      (= cmd "--nrepl-server")    (nrepl more)
      (= cmd "path")              (cmd-path)
      (str/starts-with? cmd "-M") (cmd-M cmd more)
      (str/starts-with? cmd "-A") (cmd-A cmd more)
      (= cmd "-m")                (cmd-run (cons "-m" more))
      (= cmd "build")             (cmd-build more)
      :else                       (run-task cmd more))))
