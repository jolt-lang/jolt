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
(defn- load-natives! [natives]
  (when (seq natives)
    (let [plat (current-platform)]
      (doseq [spec natives]
        (if (:process spec)
          (jolt.ffi/load-library)
          (let [c (get spec plat)
                cands (if (string? c) [c] (vec c))
                hit (some #(when (jolt.ffi/loaded? %) %) cands)]
            ;; A :static spec has no runtime shared object (it's linked into a
            ;; built binary), so an interpreted `run`/`repl` has nothing to load —
            ;; skip it rather than fail. Its foreign calls only resolve in a static
            ;; build; document a dynamic candidate too to use it under `run`.
            (when (and (nil? hit) (not (:optional spec)) (not (:static spec)))
              (throw (ex-info (str "required native library "
                                   (or (:name spec) (first cands) "?")
                                   " not found — tried " (pr-str cands) " for " (name plat))
                              {:native spec})))))))))

;; Apply a resolved project's roots on top of the current (jolt-core) roots so app
;; namespaces resolve while jolt.* stays loadable, then load its native deps.
(defn- apply-project! [{:keys [roots natives]}]
  (jolt.host/set-source-roots! (vec (distinct (concat roots (jolt.host/source-roots)))))
  (load-natives! natives))

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

;; A FILE argument of "-" means stdin, like bb/ys and most CLIs; map it to
;; /dev/stdin, which load-file reads.
(defn- file-arg [x] (if (= "-" x) "/dev/stdin" x))

;; Does a bare argv token name a file to run (rather than a deps.edn task)? A "-"
;; (stdin), an existing file, or a *.clj/*.cljc/*.cljs path.
(defn- run-file-arg? [x]
  (or (= "-" x)
      (some #(str/ends-with? x %) [".clj" ".cljc" ".cljs"])
      (jolt.host/file-exists? x)))

;; run [-m NS args… | FILE]  — FILE may be "-" (stdin)
(defn- cmd-run [more]
  (apply-project! (deps/resolve-project (project-dir)))
  (cond
    (= "-m" (first more)) (run-ns (second more) (drop 2 more))
    (seq more)            (do (push-thread-bindings
                                {#'clojure.core/*command-line-args* (seq (drop-end-of-options (rest more)))})
                              (load-file (file-arg (first more))) nil)
    :else (throw (ex-info "run needs -m NS or a FILE" {}))))

;; -M:alias…  — resolve with the aliases, run their :main-opts
(defn- cmd-M [arg more]
  (let [aliases (parse-aliases arg)
        {:keys [main-opts] :as resolved} (deps/resolve-project (project-dir) aliases)]
    (apply-project! resolved)
    (if main-opts
      (apply-main-opts main-opts more)
      (throw (ex-info (str "alias(es) " (pr-str aliases) " have no :main-opts") {})))))

;; -A:alias… — add the aliases' paths/deps, then run the remaining argv as a command.
;; apply-project! concats with current source-roots, so the alias-added paths survive
;; the cmd-run re-resolution — re-dispatching through -main is safe and avoids
;; duplicating the dispatch table.
(defn- cmd-A [arg more]
  (let [aliases (parse-aliases arg)]
    (apply-project! (deps/resolve-project (project-dir) aliases))
    (apply -main more)))

(defn- cmd-path []
  (let [{:keys [roots]} (deps/resolve-project (project-dir))]
    (println (str/join ":" roots))))

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
  (try (apply-project! (deps/resolve-project (project-dir)))
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

;; A deps.edn :tasks entry: a string is a shell command; a map is {:main-opts …}.
(defn- run-task [name more]
  (let [{:keys [tasks] :as resolved} (deps/resolve-project (project-dir))
        task (get tasks (symbol name))]
    (cond
      (nil? task) (throw (ex-info (str "unknown command or task: " name " (see 'joltc help')") {:name name}))
      (string? task) (jolt.host/sh task)
      (map? task) (do (apply-project! resolved) (apply-main-opts (:main-opts task) more))
      :else (throw (ex-info (str "bad task " name) {})))))

 ;; build [-m NS | FILE] [-o OUT] [--opt | --dev] [--no-direct-link] — AOT-compile
 ;; the app into a standalone executable. Resolves deps + roots like `run`, then hands
 ;; the entry namespace to the host build driver (jolt.host/build-binary, defined by
 ;; build.ss). Default mode is release; --opt selects optimized (release + the riskier
 ;; inline/scalar-replace passes), --dev unoptimized.
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
        (deps/resolve-project (project-dir))]
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
        (when (and target library?)
          (throw (ex-info "cross build (--target) does not support --library yet" {})))
        (when (and target (nil? target-pack))
          (throw (ex-info "--target needs a target pack: --target-pack DIR (or $JOLT_TARGET_PACK)" {:target target})))
        ;; embed-dirs (absolute) are walked + baked into the binary by the driver;
        ;; project-paths (relative) become runtime io/resource roots (ship-alongside).
        (if library?
          (jolt.host/build-library entry out mode natives embed-dirs project-paths direct-link? tree-shake?)
          (jolt.host/build-binary entry out mode natives embed-dirs project-paths direct-link? tree-shake? target target-pack))))))

(defn- nrepl [more]
  ;; resolve the project (deps on the roots, native libs loaded), then start the
  ;; nREPL server so an editor can connect and (require '[some.lib]) live. A
  ;; library's middleware (deps.edn :nrepl/middleware) is composed over the
  ;; built-in handler — sessions / interruptible-eval / completion etc.
  (let [resolved (deps/resolve-project (project-dir))]
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
  (println "usage: joltc [command] [args]")
  (println)
  (println "With no command, starts a REPL.")
  (println)
  (println "commands:")
  (println "  repl                   start a REPL")
  (println "  nrepl-server [port]    start an nREPL server (default 7888) for editors")
  (println "  run -m NS [args]       resolve deps.edn, load NS, call its -main")
  (println "  run FILE [args]        load a Clojure file")
  (println "  build -m NS [-o OUT] [--opt|--dev] [--direct-link] [--tree-shake] [--dynamic]")
  (println "              [--target MACHINE --target-pack DIR]")
  (println "                         compile a standalone binary (--target cross-compiles")
  (println "                         for another Chez machine; see tools/cross-compile)")
  (println "  path                   print the resolved source roots")
  (println "  <task> [args]          run a deps.edn :tasks entry")
  (println "  help, --help, -h       print this message")
  (println "  version, --version, -V print the jolt version")
  (println)
  (println "options:")
  (println "  -e EXPR [args]         evaluate EXPR and print the result")
  (println "  -e - [args]            evaluate an EXPR read from stdin")
  (println "  - [args]               run a program read from stdin (as a script)")
  (println "  -m NS [args]           shorthand for run -m")
  (println "  -M:alias [args]        run the alias's :main-opts")
  (println "  -A:alias [args]        add the alias's paths/deps, run the rest")
  (println)
  (println "The first standalone -- ends option parsing; everything after it is")
  (println "passed to the program as *command-line-args*."))

(defn -main [& args]
  (let [[cmd & more] args]
    (cond
      ;; bare `joltc` starts a REPL, like bb/clj
      (nil? cmd)                         (repl)
      (#{"help" "--help" "-h"} cmd)      (usage)
      (#{"version" "--version" "-V"} cmd) (println (str "jolt " (version)))
      (= cmd "run")                      (cmd-run more)
      (= cmd "repl")                     (repl)
      (= cmd "nrepl-server")             (nrepl more)
      (= cmd "path")                     (cmd-path)
      (str/starts-with? cmd "-M")        (cmd-M cmd more)
      (str/starts-with? cmd "-A")        (cmd-A cmd more)
      (= cmd "-m")                       (cmd-run (cons "-m" more))
      (= cmd "build")                    (cmd-build more)
      ;; a bare FILE (or "-" for stdin) runs it, `run` optional — like bb; a
      ;; non-file token falls through to a deps.edn :tasks lookup.
      (run-file-arg? cmd)                (cmd-run (cons cmd more))
      :else                              (run-task cmd more))))
