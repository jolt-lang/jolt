(ns jolt.main.natives
  "A project's :jolt/native libraries: loading them for a run (a dependency's on
  its first require), building a missing one with the task its spec names, and
  encoding them for `jolt build`, which links :static archives into the binary
  and reports the libraries the binary still loads at runtime."
  (:require [clojure.string :as str]))

;; The key a :jolt/native spec's candidates are read under. These three
;; spellings are the whole set — jolt.deps.declarations/native-platform-keys,
;; which builds a spec's dedup identity, must list the same ones.
(defn current-platform []
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
;; A leading backslash is rooted for the same reason "/" is: "\\server\share\x.dll"
;; (UNC) and "\Windows\System32\x.dll" (rooted on the current drive) are paths the
;; OS resolves, and joining either to a project directory produces one nothing
;; can open. That matters now that the Windows fallback COMPOSES candidates from
;; the loader's own search directories — a UNC entry on PATH would otherwise
;; come back as "C:/proj/\\srv\share\bin/libcrypto-3-x64.dll".
(defn path-rooted? [p]
  (or (str/starts-with? p "/")
      (str/starts-with? p "\\")
      (boolean (re-find #"^[A-Za-z]:" p))))

(defn- native-candidate [base c]
  (if (and base
           (or (str/includes? c "/") (str/includes? c "\\"))
           (not (path-rooted? c)))
    (str base "/" c)
    c))

;; The base a spec's relative paths resolve against: the deps.edn that DECLARED
;; it (jolt.deps attaches the root), falling back to the project for a spec that
;; carries none — a stale cpcache entry, or a caller that built the map itself.
;; Without this a dependency's "native/libfoo.so" resolved against the APP's
;; directory, where it is not. A RECONCILED spec (jolt.deps folds two
;; declarations of the same :name into one) carries a per-platform-key root for
;; every key it took from the other declaration, so those candidates still
;; resolve against the deps.edn that wrote them.
(defn- native-root [spec plat base]
  (or (get (:jolt.deps/roots spec) plat) (:jolt.deps/root spec) base))

;; A BUILD-TIME path (a :static archive, a -L directory) relative to that root.
;; Unlike native-candidate this prefixes a bare filename too: those paths are
;; handed to the linker, which reads "libfoo.a" as a file in the current
;; directory rather than a name to search for — dlopen's rule, which
;; native-candidate implements, is the opposite and must not change.
(defn- native-build-path [base p]
  (if (and base
           (not (str/blank? p))
           (not (path-rooted? p)))
    (str base "/" p)
    p))

;; Does NAME read as a library name — the thing a platform has a conventional
;; spelling of? A :jolt/native :name is free text ("libc (POSIX sockets)"), and
;; deriving "libc (POSIX sockets).dll" from one would put noise in the
;; diagnostic and dlopen nothing.
(defn- library-name? [n]
  (and (string? n) (seq n) (re-matches #"[A-Za-z0-9_.+-]+" n)))

;; The candidates SPEC itself declares for PLAT — one string or a list of them.
(defn- native-declared [spec plat]
  (let [c (get spec plat)] (if (string? c) [c] (vec c))))

;; The candidates to try for SPEC on PLAT, in order. A spec that declares the
;; platform's key is taken at its word. One that declares none — every
;; :darwin/:linux library on Windows, which is all of jolt-lang/crypto and
;; jolt-lang/http-client — used to yield the empty list, so nothing was dlopen'd
;; and the failure still read "not found" about a search that never happened
;; (jolt-lang/jolt#989). Fall back to the platform's conventional spellings of
;; the :name, which is what load-system-library would have tried; on Windows
;; that includes the version-suffixed files (libcrypto-3-x64.dll) the OS loader
;; cannot reach from a bare name.
(defn- native-candidates [spec plat base]
  (let [declared (native-declared spec plat)
        raw (if (seq declared)
              declared
              (when (library-name? (:name spec))
                (vec (jolt.ffi/system-library-candidates (:name spec)))))]
    (mapv #(native-candidate (native-root spec plat base) %) raw)))

;; What to say when nothing loaded. "tried []" was the tell for a spec with no
;; key for this platform, and it read as a missing library — it named no search
;; because none happened, and "(a task may build it)" pointed at a build for a
;; file that is already on disk. Say which keys the spec DOES declare instead.
;; A candidate that IS on disk but failed to load (a DLL whose own dependency is
;; missing, #1127) is named with the loader's reason rather than reported as
;; not found.
(defn- native-missing-msg [spec plat cands]
  (let [nm (or (:name spec) (first cands) "?")
        declared-keys (filterv #(contains? spec %) [:darwin :linux :windows])
        note (when (seq cands) (jolt.ffi/load-failure-note cands))]
    (cond
      note
      (str "required native library " nm " did not load — " note)

      (seq (native-declared spec plat))
      (str "required native library " nm " not found — tried " (pr-str cands)
           " for " (name plat))

      :else
      (str "required native library " nm " has no " plat " candidates"
           (when (seq declared-keys)
             (str " (declares " (str/join ", " declared-keys) ")"))
           (if (seq cands)
             (str " — tried this platform's conventional names " (pr-str cands))
             "")))))

;; --- building a missing :jolt/native library --------------------------------
;; A spec may name the task that produces it: {:name "ents" :darwin [...] :build
;; "native"}, where "native" is a task in the DECLARING library's own deps.edn.
;; A library with a C shim reached as a plain git dep then works without the
;; consumer cloning it and running the task by hand: the checkout under
;; ~/.jolt/gitlibs holds the sources and nothing else ever built them.
;;
;; The build is a child `jolt run <task>` in the library's root. Each [root task]
;; runs at most once per process (several specs may share one task); a lock
;; directory in the root keeps two processes from building into the same
;; checkout at once, the loser waiting and then retrying the load. Roots being
;; built by an ancestor process travel in JOLT_NATIVE_BUILDING, so the build
;; task's own process (which applies the same project) never starts another
;; build of it — and never waits on the lock its parent holds.
(def ^:private native-builds (atom {}))   ; [root task] -> {:status n} | {:error s}

(def ^:private native-building-env "JOLT_NATIVE_BUILDING")

(defn- native-build-off? [] (= "0" (jolt.host/getenv "JOLT_NATIVE_BUILD")))

(defn- canonical-dir [d] (.getCanonicalPath (java.io.File. ^String d)))

(defn- native-build-dir [spec base]
  (when-let [d (or (:jolt.deps/root spec) base)] (canonical-dir d)))

(defn- ancestor-building? [dir]
  (let [v (jolt.host/getenv native-building-env)]
    (boolean (and v (some #{dir} (str/split v #"\n"))))))

;; The jolt to run the task with: bin/jolt exports JOLT_EXE, a built jolt binary
;; knows its own path, and an installed jolt is on PATH.
(defn- jolt-exe []
  (or (jolt.host/getenv "JOLT_EXE") (jolt.host/self-exe) "jolt"))

;; Run the task, its stdout copied to our stderr: the build is not the program's
;; output, and `jolt -M:app > out` must not capture a compiler's chatter.
(defn- spawn-native-build! [nm dir task]
  (binding [*out* *err*]
    (println (str "jolt: building native library " nm
                  " (task " task " in " dir ")"))
    (flush))
  (try
    (let [pb (java.lang.ProcessBuilder. ^java.util.List [(jolt-exe) "run" task])
          env (.environment pb)
          prev (jolt.host/getenv native-building-env)]
      (.directory pb (java.io.File. ^String dir))
      (.put env "JOLT_PWD" dir)
      (.put env native-building-env (if prev (str prev "\n" dir) dir))
      (.redirectError pb java.lang.ProcessBuilder$Redirect/INHERIT)
      (let [p (.start pb)
            in (.getInputStream p)
            buf (byte-array 8192)]
        ;; nothing is ever written to the task: close its stdin, so one that
        ;; reads it (a prompt, a tool that defaults to stdin) sees EOF instead
        ;; of waiting forever on a pipe no one writes to
        (.close (.getOutputStream p))
        (loop []
          (let [n (.read in buf)]
            (when (pos? n) (.write System/err buf 0 n) (recur))))
        (.flush System/err)
        {:status (.waitFor p)}))
    (catch Exception e
      {:error (str "could not run " (jolt-exe) ": " (ex-message e))})))

;; The build lock is a directory (mkdir is the atomic create-if-absent) holding
;; an `owner` file: "<pid> <host> <nonce>". Its holder touches the directory
;; every few seconds while the build runs, so a lock is stale once its holder is
;; dead on this host, or once its mtime stops moving — a build killed by Ctrl-C,
;; kill -9 or a crash (which never reaches the `finally` that releases it) costs
;; the next run a moment, not a wait.
(def ^:private native-lock-heartbeat-ms 5000)
(def ^:private native-lock-stale-ms 60000)

(def ^:private native-lock-host
  (delay (try (str (.getHostName (java.net.InetAddress/getLocalHost)))
              (catch Exception _ "unknown"))))

(defn- native-lock-owner-file ^java.io.File [^java.io.File lock]
  (java.io.File. lock "owner"))

;; [pid host token] from LOCK's owner file, nil when it has none (not written
;; yet, a lock an older jolt took, or one gone since).
(defn- native-lock-owner [^java.io.File lock]
  (try (let [s (str/trim (slurp (native-lock-owner-file lock)))
             [pid host] (str/split s #" ")]
         (when-let [pid (and pid (parse-long pid))] [pid host s]))
       (catch Exception _ nil)))

(defn- pid-alive? [pid]
  (try (.isPresent (java.lang.ProcessHandle/of pid))
       (catch Exception _ true)))

;; Why LOCK is stale, or nil while its holder may still be building.
(defn- native-lock-stale [^java.io.File lock owner]
  (let [[pid host] owner]
    (cond
      (and pid (= host @native-lock-host) (not (pid-alive? pid)))
      (str "its holder, pid " pid ", is gone")

      (> (- (System/currentTimeMillis) (.lastModified lock)) native-lock-stale-ms)
      "it has not been refreshed for a minute")))

(defn- delete-native-lock! [^java.io.File lock]
  (.delete (native-lock-owner-file lock))
  (.delete lock))

;; Break a stale LOCK. It is renamed aside first, so of two waiters that both
;; judged it stale only one takes it; and the one that did checks it took the
;; lock it judged (same owner line), putting back a fresh lock some other process
;; took in between rather than deleting it.
(defn- break-native-lock! [^java.io.File lock owner]
  (let [aside (java.io.File. (str (.getPath lock) ".stale-"
                                  (.pid (java.lang.ProcessHandle/current)) "-"
                                  (System/nanoTime)))]
    (when (.renameTo lock aside)
      (if (= (nth owner 2 nil) (nth (native-lock-owner aside) 2 nil))
        (delete-native-lock! aside)
        (when-not (.renameTo aside lock) (delete-native-lock! aside))))))

;; Take DIR's build lock, waiting while another process holds it. Answers true
;; when it had to wait — the other process has probably built the library, so
;; the caller tries the load again before building anything itself.
(defn- acquire-native-lock! [^java.io.File lock nm]
  (loop [waited? false, said? false, vanished 0]
    (if (.mkdir lock)
      waited?
      (let [owner (native-lock-owner lock)]
        (cond
          ;; Gone between the mkdir and this check: its holder just released
          ;; it, so try again, as a waiter (its build may have just produced
          ;; the library). Only a lock that never appears (an unwritable or
          ;; missing dir) is an error.
          (not (.exists lock))
          (if (< vanished 50)
            (recur true said? (inc vanished))
            (throw (ex-info (str "cannot create " (.getPath lock)) {})))

          :else
          (if-let [why (native-lock-stale lock owner)]
            (do (binding [*out* *err*]
                  (println (str "jolt: removing a stale native build lock for " nm
                                " (" why "): " (.getPath lock))))
                (break-native-lock! lock owner)
                (recur waited? said? vanished))
            (do (when-not said?
                  (binding [*out* *err*]
                    (println (str "jolt: waiting for another jolt building " nm
                                  (when-let [pid (first owner)] (str " (pid " pid ")"))
                                  "; if no build is running, delete "
                                  (.getPath lock)))))
                (Thread/sleep 200)
                (recur true true vanished))))))))

;; Record this process as LOCK's holder and keep its mtime fresh until the
;; answered fn is called, which stops the heartbeat and releases the lock — only
;; if it is still ours (a waiter may have judged it stale and broken it).
(defn- hold-native-lock! [^java.io.File lock]
  (let [line (str (.pid (java.lang.ProcessHandle/current)) " " @native-lock-host
                  " " (System/nanoTime))
        _ (try (spit (native-lock-owner-file lock) line) (catch Exception _ nil))
        stop (atom false)
        beat (doto (Thread.
                    ^Runnable
                    (fn []
                      (loop []
                        (when-not @stop
                          (try (Thread/sleep native-lock-heartbeat-ms)
                               (catch InterruptedException _ nil))
                          (when-not @stop
                            (.setLastModified lock (System/currentTimeMillis)))
                          (recur)))))
               (.setDaemon true)
               (.start))]
    (fn []
      (reset! stop true)
      (.interrupt beat)
      (when (= line (nth (native-lock-owner lock) 2 nil))
        (delete-native-lock! lock)))))

;; Build SPEC's library with its :build task, then try LOAD! again. Answers
;; [hit result], result being the build's {:status n} / {:error s}.
(defn- build-native! [spec dir load!]
  (let [task (str (:build spec))
        nm (or (:name spec) task)
        k [dir task]]
    (if-let [r (get @native-builds k)]
      [(load!) r]
      (let [lock (java.io.File. (str dir "/.jolt-native-build.lock"))
            waited? (try (acquire-native-lock! lock nm)
                         (catch Exception e {:error (ex-message e)}))]
        (if (map? waited?)
          [nil waited?]
          (let [release! (hold-native-lock! lock)]
            (try
              (or (when waited? (when-let [h (load!)] [h {:status 0}]))
                  (let [r (spawn-native-build! nm dir task)]
                    (swap! native-builds assoc k r)
                    [(load!) r]))
              (finally (release!)))))))))

;; Build a missing library whose spec names its task — except the project's own
;; library under a task (strict? false): the task being run may BE that build,
;; or one that must not trigger it (a clean), and a task that needs the library
;; can :depends on its build. DIR is the spec's build dir, nil when it has no
;; :build or nothing is missing.
(defn- auto-build-native? [dir strict? base]
  (boolean (and dir
                (not (native-build-off?))
                (not (ancestor-building? dir))
                (or strict? (not= dir (some-> base canonical-dir))))))

;; What the missing-library message adds about the :build task, if anything.
(defn- native-build-note [spec dir result cands]
  (when-let [task (:build spec)]
    (let [where (str "task " task " in " dir)]
      (cond
        (:error result)
        (str "; building it failed: " (:error result) " (" where ")")

        (and result (not (zero? (:status result))))
        (str "; building it failed: " where " exited with status " (:status result))

        (and result (seq cands) (jolt.ffi/load-failure-note cands))
        (str " (built by " where ")")

        result
        (str "; " where " ran but did not produce it")

        (native-build-off?)
        (str "; " where " builds it (auto-build is off: JOLT_NATIVE_BUILD=0)")

        :else
        (str "; " where " builds it")))))

(defn- native-load-candidates [spec plat base]
  ;; a :static spec's conventional names are not derived here: its
  ;; symbols come from its archive (preloaded by `jolt build`,
  ;; linked into the binary), and a same-named shared object the
  ;; loader happens to find is a different library — it answered
  ;; the build's calls in the archive's place, and for "crypto"
  ;; on macOS it is Apple's libcrypto.dylib, which aborts the
  ;; process. Candidates the spec DECLARES are still loaded.
  (if (:static spec)
    (mapv #(native-candidate (native-root spec plat base) %)
          (native-declared spec plat))
    (native-candidates spec plat base)))

;; A required library none of whose candidates loaded: an error when strict?,
;; otherwise a warning (or nothing, under the auto-build that is producing it).
(defn- report-missing-native! [spec plat cands dir result buildable? strict?]
  (let [msg (str (native-missing-msg spec plat cands)
                 (when dir (native-build-note spec dir result cands)))]
    (cond
      strict?
      (throw (ex-info msg {:native spec}))

      ;; the task an auto-build started: its parent already said
      ;; what is being built, and the library is missing by design
      (and dir (ancestor-building? dir))
      nil

      :else
      (binding [*out* *err*]
        ;; a task may still BUILD a library it declared candidates
        ;; for; it cannot build one this platform was never told
        ;; the name of.
        (println (str "warning: " msg
                      (when (and buildable? (not dir))
                        " (a task may build it)")))))))

(defn- load-native-spec! [spec plat strict? base]
  (let [cands (native-load-candidates spec plat base)
        ;; Load the native RTLD_LOCAL and register its handle, so the
        ;; spec's defcfns resolve from the handle (isolated from the
        ;; process-global namespace) rather than depending on global
        ;; foreign-entry search order. load-native returns #t on success
        ;; (a handle, or Windows-global), #f when no candidate opens.
        load! #(some (fn [c] (when (jolt.ffi/load-native c) c)) cands)
        hit (load!)
        ;; a task may build a library it declared candidates for, but
        ;; not one already on disk that failed to load. A :static
        ;; spec's declared shared object is built the same way; only a
        ;; missing one stays silent below.
        buildable? (and (nil? hit)
                        (not (:optional spec))
                        (seq (native-declared spec plat))
                        (nil? (jolt.ffi/load-failure-note cands)))
        dir (when (and buildable? (:build spec)) (native-build-dir spec base))
        [hit result] (if (auto-build-native? dir strict? base)
                       (build-native! spec dir load!)
                       [hit nil])]
    ;; A :static spec has no runtime shared object (it's linked into a
    ;; built binary), so an interpreted `run`/`repl` has nothing to load —
    ;; skip it rather than fail. Its foreign calls only resolve in a static
    ;; build; document a dynamic candidate too to use it under `run`.
    (when (and (nil? hit) (not (:optional spec)) (not (:static spec)))
      (report-missing-native! spec plat cands dir result buildable? strict?))))

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
           (load-native-spec! spec plat strict? base)))))))

;; --- a dependency's natives load on its first require ------------------------
;; A dependency's :jolt/native libraries are not opened at startup. Each spec
;; carries the source roots of the dependency that declared it (deps.clj
;; resolve-deps), and the loader opens it the first time a namespace is read
;; off one of them, before that namespace's code runs (loader.ss
;; ldr-natives-for-file!). A program that never requires the dependency never
;; opens its library: ring-chez-adapter's hello-world no longer loads OpenSSL
;; because jolt.crypto is on the roots.
;;
;; These still load at startup: a :process spec (the process's own symbols),
;; the project's own (no :jolt.deps/paths), one a dependency marks :eager
;; because its C must be there before any of its namespaces loads, and any spec
;; a cpcache entry from an older jolt recorded without roots.
(defn- deferred-native? [spec]
  (boolean (and (seq (:jolt.deps/paths spec))
                (not (:process spec))
                (not (:eager spec)))))

;; A dependency's natives failing at first require name the dependency and the
;; namespace that triggered the load, since the failure no longer happens at
;; startup where "the project's natives" was all the context there was.
(defn- load-dependency-natives! [specs strict? base ns]
  (try
    (load-natives! specs strict? base)
    (catch clojure.lang.ExceptionInfo e
      (if-let [spec (:native (ex-data e))]
        (throw (ex-info (str (ex-message e)
                             " (declared by dependency " (or (:jolt.deps/lib spec) "?")
                             (when ns (str ", loaded on first require of " ns))
                             ")")
                        (assoc (ex-data e) :ns (some-> ns str))
                        e))
        (throw e)))))

;; Load what has to be there now, and hand each dependency's natives to the
;; loader under the source roots that load them. Specs sharing a set of roots
;; (one dependency's) go in one group, in declaration order.
(defn load-project-natives! [natives strict? base]
  (load-natives! (vec (remove deferred-native? natives)) strict? base)
  (let [deferred (filter deferred-native? natives)]
    (doseq [roots (distinct (map :jolt.deps/paths deferred))]
      (let [specs (vec (filter #(= roots (:jolt.deps/paths %)) deferred))]
        (jolt.host/defer-natives! roots
                                  (fn [ns] (load-dependency-natives! specs strict? base ns)))))))

;; --- natives in a built binary ---------------------------------------------
;; The static-link description of a :jolt/native spec for this platform, or nil.
;; :static may be flat ({:archive "…"} / {:lib "z" :libdir "…"}) or per-platform
;; ({:darwin {…} :linux {…}}). Returns a vector build.ss reads and wraps in the
;; platform's force-load flags: ["archive" abspath] or ["lib" name libdir].
(defn- static-link-spec [spec plat base]
  (when-let [s (:static spec)]
    (let [p (get s plat)
          s (if (map? p) p s)
          ;; relative to the deps.edn that declared it, not to the build's cwd:
          ;; a dependency ships its archive beside its own sources, and even the
          ;; project's own "native/libfoo.a" only worked when the build happened
          ;; to run from the project dir — which bin/jolt, which cd's to the jolt
          ;; tree, never does (jolt-9a8).
          root (native-root spec plat base)]
      (cond
        (:archive s) ["archive" (native-build-path root (:archive s))]
        (:lib s)     ["lib" (:lib s) (if-let [d (:libdir s)]
                                       (native-build-path root d)
                                       "")]
        :else        nil))))

;; `jolt build` links a :static archive into the binary, and the run-time load
;; in load-natives! never looks at it, so a missing archive whose spec names a
;; :build task is built here. One the task does not produce fails with what was
;; run rather than as a linker error about a missing file.
(defn- build-static-archives! [natives base]
  (let [plat (current-platform)]
    (doseq [spec natives
            :when (and (:build spec) (not (:optional spec)) (not (:process spec)))]
      (let [[kind archive] (static-link-spec spec plat base)
            present #(when (jolt.host/file-exists? archive) archive)]
        (when (and (= "archive" kind) (not (present)))
          (let [dir (native-build-dir spec base)
                [hit result] (if (auto-build-native? dir true base)
                               (build-native! spec dir present)
                               [nil nil])]
            (when-not hit
              (throw (ex-info (str "static archive " archive " for :jolt/native "
                                   (or (:name spec) "?") " not found"
                                   (native-build-note spec dir result []))
                              {:native spec})))))))))

;; `jolt build` builds a dependency's missing shared library too. A run builds
;; it at the first require of one of the dependency's namespaces, but the build
;; process only requires what the binary carries, and a binary that binds the
;; library's symbols without carrying any of the dependency's namespaces still
;; loads it, at the first such call (build.ss bld-natives-for-closure). That
;; binary would ship pointing at a file nothing ever built. Which symbols the app
;; binds is not known here, so every dependency library the binary may load at
;; runtime (an encoded "dep" entry) is built if its declared file is missing.
;; Only a candidate naming a path is looked for on disk: a bare soname is the
;; system's to find, and a task cannot be said to have produced it.
(defn- build-dependency-libraries! [encoded natives base]
  (let [plat (current-platform)]
    (doseq [[e spec] (map vector encoded natives)
            :when (and (= "dep" (first e)) (:build spec) (not (:optional spec)))]
      (let [cands (filterv #(or (str/includes? % "/") (str/includes? % "\\"))
                           (mapv #(native-candidate (native-root spec plat base) %)
                                 (native-declared spec plat)))
            present #(some (fn [c] (when (jolt.host/file-exists? c) c)) cands)]
        (when (and (seq cands) (not (present)))
          (let [dir (native-build-dir spec base)
                [hit result] (if (auto-build-native? dir true base)
                               (build-native! spec dir present)
                               [nil nil])]
            (when-not hit
              (throw (ex-info (str (native-missing-msg spec plat cands)
                                   (native-build-note spec dir result cands)
                                   " (declared by dependency "
                                   (or (:jolt.deps/lib spec) "?") ")")
                              {:native spec})))))))))

;; Encode a deps.edn :jolt/native spec for the build launcher, resolving the
;; current platform's candidate list now (the binary runs on this OS). Each entry
;; becomes a vector the launcher (build.ss) reads:
;;   ["process"]            — the running binary's own symbols (libc)
;;   ["static" form …]      — the lib's archive, cc-linked into the binary; its
;;                            symbols load from the process (default when :static
;;                            is present and --dynamic wasn't passed)
;;   ["req"|"opt" cand…]    — load a shared object at runtime, trying each in turn
;; dynamic? forces the runtime path for every lib (the --dynamic build flag).
;;
;; An :optional spec whose :static archive is not on disk is loaded at runtime
;; like any other optional native, rather than handed to the linker, which fails
;; the whole build on the missing file: optional means the app runs without it,
;; and a tree that never built the optional archive is the common case.
;; The same holds for a :static {:lib "name"}: the linker resolves -lname, and
;; a host without that library fails the link just as surely. The build probes
;; it (jolt.host/build-lib-links?, a try-link cached per lib) for optional specs
;; only, so a build with no optional {:lib} native never runs the probe.
(defn- unlinkable-optional-static? [spec static target target-pack]
  (and (:optional spec)
       (case (first static)
         "archive" (not (jolt.host/file-exists? (second static)))
         "lib"     (not (jolt.host/build-lib-links? (nth static 1) (nth static 2)
                                                    target target-pack))
         false)))

(defn- encode-natives [natives dynamic? base target target-pack]
  (let [plat (current-platform)]
    (vec (for [spec natives]
           (let [static (and (not dynamic?) (static-link-spec spec plat base))
                 static (when-not (unlinkable-optional-static? spec static target target-pack)
                          static)]
             (cond
               (:process spec) ["process"]
               static          (into ["static"] static)
               :else
               (let [load (into [(if (:optional spec) "opt" "req")]
                                (native-candidates spec plat base))]
                 ;; a binary loads a dependency's library at startup only when
                 ;; it carries one of that dependency's namespaces, and
                 ;; otherwise only if a foreign symbol nothing loaded defines
                 ;; asks for it (build.ss bld-natives-for-closure), as a run does
                 (if (deferred-native? spec)
                   (-> ["dep" (str (count (:jolt.deps/paths spec)))]
                       (into (:jolt.deps/paths spec))
                       (into load))
                   load))))))))

;; Say which :jolt/native libraries the built binary will still dlopen. A
;; `jolt build` is otherwise self-contained — the Clojure, the runtime and every
;; :static archive are IN the file — so a lib that stayed dynamic is the one
;; reason the binary is not the single dependency-free artifact a static build
;; is taken to be, and nothing said so (jolt-9a8). Silence here read as "there is
;; nothing left to ship", which was wrong exactly when it mattered.
;;
;; Not a warning: a system lib the OS resolves by soname (libc, libcrypto) is the
;; normal case and there is nothing to fix. It names what the binary needs so the
;; person shipping it knows, and points at the key that would link it in.
(defn- report-runtime-natives! [encoded natives dynamic?]
  (let [kind (fn [e] (if (= "dep" (first e))
                       (nth e (+ 2 (parse-long (second e))))
                       (first e)))
        dyn (->> (map vector encoded natives)
                 (filter (fn [[e _]] (#{"req" "opt"} (kind e))))
                 vec)]
    (when (seq dyn)
      (binding [*out* *err*]
        (println (str "jolt build: " (count dyn) " :jolt/native "
                      (if (= 1 (count dyn)) "library is" "libraries are")
                      " loaded at runtime — the binary needs "
                      (if (= 1 (count dyn)) "it" "them")
                      " on the host"
                      (if dynamic?
                        " (--dynamic was passed):"
                        "; declare :static {:archive \"…\"} to link one in:")))
        (doseq [[e spec] dyn]
          (let [load (if (= "dep" (first e)) (drop (+ 2 (parse-long (second e))) e) e)]
            (println (str "  " (or (:name spec) "?")
                          (when (= "opt" (first load)) " (optional)")
                          (when (= "dep" (first e))
                            (str " (declared by " (or (:jolt.deps/lib spec) "a dependency")
                                 "; loaded at startup if the binary carries one of its"
                                 " namespaces, else at the first call that needs it)"))
                          " — " (str/join ", " (rest load))))))))))

;; The project's :jolt/native libraries, built where the binary needs them and
;; encoded for the driver.
(defn build-natives [resolved {:keys [pdir target target-pack dynamic-natives?]}]
  (let [natives-dir (or (:project-dir resolved) pdir)]
    ;; a :static archive a dependency's :build task produces is built
    ;; now, before the linker is handed a path to nothing (not when
    ;; cross-compiling: the task builds for this host)
    (when-not (or dynamic-natives? target)
      (build-static-archives! (:natives resolved) natives-dir))
    (let [natives (encode-natives (:natives resolved) dynamic-natives? natives-dir
                                  target target-pack)]
      ;; ...and so is a dependency's shared library (not when
      ;; cross-compiling, for the same reason)
      (when-not target
        (build-dependency-libraries! natives (:natives resolved) natives-dir))
      (report-runtime-natives! natives (:natives resolved) dynamic-natives?)
      natives)))
