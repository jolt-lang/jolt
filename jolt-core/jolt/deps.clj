(ns jolt.deps
  "Resolve a deps.edn into an ordered list of source roots. A reduced
  tools.deps: :paths, :deps (`:git/url`+`:git/sha` / `:local/root` /
  `:mvn/version`), :aliases (:extra-paths / :extra-deps / :main-opts), :tasks.

  The deps walk is breadth-first so a top-level coordinate registers before any
  transitive one (a top-level pin wins). Git deps reuse an existing
  tools.gitlibs checkout ($GITLIBS / ~/.gitlibs) when the JVM toolchain already
  fetched them, else clone into a sha-immutable cache ($JOLT_GITLIBS, else
  ~/.jolt/gitlibs, or a jolt/ subdir of $GITLIBS) shared across projects.
  Maven jars live in the standard local repository (~/.m2/repository;
  :mvn/local-repo in deps.edn relocates it like tools.deps, JOLT_LOCAL_REPO
  overrides from the environment) shared with the JVM toolchain in both
  directions. Resolution shells out to `git`/`curl`/`unzip` through
  jolt.host/sh; nothing here touches the JVM."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; --- small host seams -------------------------------------------------------
(defn- getenv [n] (jolt.host/getenv n))
(defn- file-exists? [p] (jolt.host/file-exists? p))
(defn- sh [cmd] (jolt.host/sh cmd))           ; exit code, inherits stdout/stderr
(defn- warn [& xs] (binding [*out* *err*] (println (str "[jolt.deps] " (apply str xs)))))

(defn- read-edn [path]
  (when (file-exists? path)
    (try (edn/read-string (slurp path))
         (catch :default e
           (throw (ex-info (str path ": " (ex-message e)) {:path path :error e}))))))

(defn- abspath [dir p]
  (if (str/starts-with? p "/") p (str dir "/" p)))

;; --- git cache --------------------------------------------------------------
;; jolt's own clone cache. $GITLIBS (the tools.gitlibs location knob) is
;; respected for WHERE the cache lives — under a jolt/ subdir so tools.gitlibs'
;; own _repos/ and libs/ namespaces are never written to. JOLT_GITLIBS pins an
;; exact directory.
(defn- gitlibs-dir []
  (or (getenv "JOLT_GITLIBS")
      (when-let [g (getenv "GITLIBS")] (str g "/jolt"))
      (str (or (getenv "HOME") ".") "/.jolt/gitlibs")))

(defn- alnum? [c]
  (let [n (int c)]
    (or (and (>= n 48) (<= n 57))     ; 0-9
        (and (>= n 65) (<= n 90))     ; A-Z
        (and (>= n 97) (<= n 122))))) ; a-z
(defn- sanitize [s]
  (str/join (map (fn [c] (if (or (alnum? c) (= c \.) (= c \-)) c \_)) (seq s))))

(defn- gitlibs-shared-checkout
  "An existing tools.gitlibs checkout for lib@sha ($GITLIBS or ~/.gitlibs,
  layout libs/<group>/<name>/<sha>) — reused read-only when the JVM toolchain
  already fetched this dep. jolt never writes there: tools.gitlibs keeps its
  own bookkeeping (_repos bare clones + worktrees) that a foreign writer could
  corrupt, so jolt's own fetches go to its cache below."
  [lib sha]
  (when (and lib (namespace lib))
    (let [base (or (getenv "GITLIBS") (str (or (getenv "HOME") ".") "/.gitlibs"))
          dir (str base "/libs/" (namespace lib) "/" (name lib) "/" sha)]
      (when (file-exists? dir) dir))))

(defn- ensure-git
  "Return a checkout dir for url@sha: an existing tools.gitlibs checkout for
  `lib` when present, else clone into jolt's cache (once)."
  [lib url sha]
  (let [dir (str (gitlibs-dir) "/" (sanitize url) "/" sha)]
    (if-let [shared (and (not (file-exists? dir)) (gitlibs-shared-checkout lib sha))]
      shared
      (if (file-exists? dir)
        dir
        (do
          (warn "fetching " url " @ " (subs sha 0 (min 12 (count sha))))
          (sh (str "mkdir -p " (pr-str dir)))
          (when-not (zero? (sh (str "git clone --quiet " (pr-str url) " " (pr-str dir))))
            (throw (ex-info (str "git clone failed: " url) {:url url})))
          (when-not (zero? (sh (str "git -C " (pr-str dir) " checkout --quiet " (pr-str sha))))
            (throw (ex-info (str "git checkout failed: " sha " in " url) {:url url :sha sha})))
          ;; submodules are pinned in the checkout; pull them if the dep uses any.
          (when-not (zero? (sh (str "git -C " (pr-str dir) " submodule update --init --recursive --quiet")))
            (throw (ex-info (str "git submodule update failed for " url) {:url url})))
          dir)))))

;; --- maven cache ------------------------------------------------------------
;; jolt has no JVM, but a Clojure library's Maven JAR carries its .clj/.cljc/.cljs
;; SOURCE (Clojure ships source, not just bytecode). So a :mvn/version coordinate
;; resolves by fetching the JAR (Clojars, then Central), extracting it, and using
;; the extraction as a source root — its pom.xml supplies the transitive deps.
;; A JAR of pure Java classes has no source to run and simply contributes nothing.
;;
;; JARs live at their standard path in the local Maven repository
;; (~/.m2/repository), so they are shared with JVM Clojure/tools.deps in both
;; directions: an artifact clj already fetched is reused without a download, and
;; one jolt fetches is there for clj. The jolt-only source extraction sits in a
;; "<artifact>-<version>.jar.jolt/" directory beside the jar. The repository
;; location is configured the way tools.deps configures it — the :mvn/local-repo
;; top key of deps.edn (also accepted in an add-deps map); anyone already using
;; it gets the same behavior for free. JOLT_LOCAL_REPO overrides it from the
;; environment as a jolt-specific convenience. Setting JOLT_MVNLIBS opts out of
;; sharing entirely: the legacy self-contained layout under it, jar not kept.

(def ^:private ^:dynamic *mvn-local-repo*
  "The :mvn/local-repo of the resolution in progress (bound by resolve-project /
  add-deps from their deps.edn / deps map), nil for the default." nil)

(defn- m2-repo-dir
  "The local Maven repository dir, resolved like tools.deps: JOLT_LOCAL_REPO
  (env, jolt-specific convenience) wins, then :mvn/local-repo, then
  ~/.m2/repository."
  ([] (m2-repo-dir (getenv "JOLT_LOCAL_REPO") *mvn-local-repo* (getenv "HOME")))
  ([env-override cfg home]
   (or env-override cfg (str (or home ".") "/.m2/repository"))))

(def ^:private mvn-repos
  ["https://repo.clojars.org" "https://repo1.maven.org/maven2"])

(defn- mvn-group [coord] (or (namespace coord) (name coord)))

(defn- ensure-maven
  "Ensure coord@version's JAR is in the local Maven repository (reusing one the
  JVM toolchain already fetched; downloading from Clojars then Central when
  absent) and extract its source beside it (once). Returns the extraction dir,
  or nil if no repo has the artifact (a non-fatal skip)."
  [coord version]
  (let [group (mvn-group coord) artifact (name coord)
        vdir-rel (str (str/replace group "." "/") "/" artifact "/" version)
        jar-name (str artifact "-" version ".jar")
        legacy (getenv "JOLT_MVNLIBS")
        dir (if legacy
              (str legacy "/" (sanitize (str coord)) "/" (sanitize version))
              (str (m2-repo-dir) "/" vdir-rel "/" jar-name ".jolt"))
        jar (if legacy
              (str dir "/dep.jar")
              (str (m2-repo-dir) "/" vdir-rel "/" jar-name))]
    (if (file-exists? (str dir "/.jolt-ok"))
      dir
      (do
        (sh (str "mkdir -p " (pr-str dir)))
        (if (and (not legacy) (file-exists? jar))
          (do (warn "using " jar-name " from the local Maven repository")
              (sh (str "unzip -o -q " (pr-str jar) " -d " (pr-str dir)))
              (sh (str "touch " (pr-str (str dir "/.jolt-ok"))))
              dir)
          (loop [repos mvn-repos]
            (if (empty? repos)
              (do (warn "maven dep " coord " " version " not found (Clojars/Central)") nil)
              (if (zero? (sh (str "curl -fsSL " (pr-str (str (first repos) "/" vdir-rel "/" jar-name))
                                  " -o " (pr-str jar))))
                (do (warn "fetching " coord " " version)
                    (sh (str "unzip -o -q " (pr-str jar) " -d " (pr-str dir)))
                    ;; legacy layout never keeps the jar; the m2 layout does —
                    ;; that IS the sharing.
                    (when legacy (sh (str "rm -f " (pr-str jar))))
                    (sh (str "touch " (pr-str (str dir "/.jolt-ok"))))
                    dir)
                (recur (rest repos))))))))))

(defn- pom-deps
  "Transitive deps of an extracted Maven dep, from its pom.xml — as a deps map so
  the BFS walks them like any other. Skips test/provided/system scope, org.clojure/
  clojure (intrinsic), and non-literal versions (ranges / ${properties})."
  [root coord]
  (let [pom (str root "/META-INF/maven/" (mvn-group coord) "/" (name coord) "/pom.xml")]
    (when (file-exists? pom)
      (let [xml (slurp pom)
            grab (fn [tag block] (second (re-find (re-pattern (str "<" tag ">(.*?)</" tag ">")) block)))]
        (into {}
          (for [[_ block] (re-seq #"(?s)<dependency>(.*?)</dependency>" xml)
                :let [g (grab "groupId" block) a (grab "artifactId" block)
                      v (grab "version" block) scope (grab "scope" block)
                      optional (grab "optional" block)]
                ;; Maven does not inherit optional deps or test/provided/system
                ;; scope transitively — so a cljc lib's optional ClojureScript
                ;; toolchain (clojurescript, closure-compiler) stays out.
                :when (and g a v
                           (not (#{"test" "provided" "system"} scope))
                           (not= "true" optional)
                           (not (and (= g "org.clojure") (= a "clojure")))
                           (re-matches #"[0-9A-Za-z.\-]+" v))]
            [(symbol g a) {:mvn/version v}]))))))

;; --- coordinate -> root dir -------------------------------------------------
(defn- coord-root
  "The on-disk root directory for one dependency coordinate, or nil to skip."
  [coord spec base-dir]
  (cond
    (:local/root spec) (abspath base-dir (:local/root spec))
    (and (:git/url spec) (:git/sha spec))
    (let [checkout (ensure-git coord (:git/url spec) (:git/sha spec))]
      (if-let [root (:deps/root spec)] (str checkout "/" root) checkout))
    (:git/url spec)
    (throw (ex-info (str "git dep " coord " needs :git/sha") {:coord coord :spec spec}))
    (:jolt/module spec)
    (do (warn "skipping janet dependency " coord " (:jolt/module is obsolete on Chez)") nil)
    ;; jolt IS Clojure — a dependency on org.clojure/clojure is satisfied
    ;; intrinsically, so skip it silently rather than warning about the (unusable)
    ;; :mvn/version coordinate.
    (= coord 'org.clojure/clojure) nil
    ;; jolt has no ClojureScript compiler, so clojurescript (and the closure /
    ;; rhino toolchain it drags in) is unusable dead weight — a cljc library
    ;; declares it for its :cljs branch, which jolt never takes. Skip its subtree.
    (= coord 'org.clojure/clojurescript) nil
    (:mvn/version spec) (ensure-maven coord (:mvn/version spec))
    :else
    (do (warn "skipping unsupported coordinate " coord " " (pr-str spec)) nil)))

(defn- has-clj-source?
  "Does the tree hold any jolt-loadable source (.clj/.cljc)? A Maven JAR that is
  pure-Java (closure-compiler) or ClojureScript-only (cljs.java-time) has none —
  it contributes nothing to run and its transitive deps are the cljs/JVM toolchain,
  so the walk skips it rather than dragging in that whole subtree."
  [root]
  (zero? (sh (str "find " (pr-str root)
                  " \\( -name '*.clj' -o -name '*.cljc' \\) -print -quit 2>/dev/null | grep -q ."))))

(defn- dep-source-roots
  "Source roots a resolved dep contributes. A Maven extraction's classpath root IS
  its source root; a git/local dep uses its deps.edn :paths (default [\"src\"])."
  [root maven?]
  (if maven?
    [root]
    (let [edn (try (read-edn (str root "/deps.edn"))
                   (catch :default e (warn (ex-message e)) nil))
          paths (or (:paths edn) ["src"])]
      (map #(abspath root %) paths))))

;; --- reconciliation ---------------------------------------------------------
;; Dependencies are resolved as a TREE (resolve-deps' BFS, which visits each
;; coordinate once) and then reconciled into a definitive, de-duplicated set —
;; one place, not ad-hoc per call site. dedup-by keeps the first item per key,
;; order preserved; it dedups both source roots (by path) and native libraries
;; (by identity), so an app pulling two libs that declare the same shared object
;; (e.g. libcrypto via both http-client and the ring adapter) includes and loads
;; it ONCE.
(defn- dedup-by [key xs]
  (second (reduce (fn [[seen acc] x]
                    (let [k (key x)]
                      (if (contains? seen k) [seen acc] [(conj seen k) (conj acc x)])))
                  [#{} []] xs)))

(defn- native-key
  "Identity of a :jolt/native spec. A :process lib (the running process's own
  symbols, e.g. libc) keys on that flag; a file lib on its :name, else on its
  platform candidate paths — two deps naming the same lib reconcile to one load."
  [spec]
  (letfn [(cands [k] (let [v (get spec k)] (cond (string? v) [v] (sequential? v) (vec v) :else [])))]
    (if (:process spec)
      [:process (:name spec)]
      [:native (or (:name spec) (vec (sort (concat (cands :darwin) (cands :linux) (cands :win)))))])))

(defn- resolve-deps
  "Breadth-first walk of a deps map; returns {:roots [...] :natives [...]} — the
  source-root directories and the collected :jolt/native declarations from every
  dep's deps.edn (raw, in walk order; reconcile-project dedups them). `base-dir`
  resolves :local/root and is replaced by a dep's own root as the walk descends."
  [deps base-dir]
  ;; queue grows by appending children at the tail; an index cursor walks it so
  ;; each dequeue is O(1) (was (subvec (vec queue) 1) per pop -> O(n^2)).
  (loop [queue (mapv (fn [[c s]] [c s base-dir]) (seq deps))
         i 0
         seen #{}
         roots []
         natives []]
    (if (>= i (count queue))
      {:roots roots :natives natives}
      (let [[coord spec bd] (nth queue i)
            i (inc i)]
        (if (contains? seen coord)
          (recur queue i seen roots natives)
          (let [root (coord-root coord spec bd)]
            (if (nil? root)
              (recur queue i (conj seen coord) roots natives)
              ;; a DEP repo's malformed deps.edn warns and contributes nothing;
              ;; only the project's own deps.edn is a hard error (resolve-project).
              ;; A Maven dep has no deps.edn — its children come from its pom.xml.
              (let [maven? (boolean (:mvn/version spec))
                    ;; a Maven dep with no jolt-loadable source contributes nothing
                    ;; and its transitive deps are cljs/JVM tooling — don't walk them.
                    usable? (or (not maven?) (has-clj-source? root))
                    edn (when (and usable? (not maven?))
                          (try (read-edn (str root "/deps.edn"))
                               (catch :default e (warn (ex-message e)) nil)))
                    deps (when usable? (if maven? (pom-deps root coord) (:deps edn)))
                    _ (when (and edn deps (not (map? deps)))
                        (throw (ex-info (str "malformed :deps in " root "/deps.edn: expected a map")
                                        {:path root :given (class deps)})))
                    child (mapv (fn [[c s]] [c s root]) (seq deps))]
                (recur (into queue child)
                       i
                       (conj seen coord)
                       (into roots (if usable? (dep-source-roots root maven?) []))
                       (into natives (:jolt/native edn)))))))))))

;; --- public -----------------------------------------------------------------
(defn resolve-project
  "Resolve `project-dir`'s deps.edn with the selected alias keywords. Returns
  {:roots [...] :main-opts [...] :tasks {...} :natives [...]}; :main-opts is the
  last selected alias's, else nil; :natives are the project's + deps' :jolt/native
  shared-library declarations."
  ([project-dir] (resolve-project project-dir []))
  ([project-dir alias-kws]
   (let [edn (read-edn (str project-dir "/deps.edn"))
         aliases (:aliases edn)
         selected (keep #(get aliases %) alias-kws)
         extra-paths (mapcat :extra-paths selected)
         extra-deps (apply merge (map :extra-deps selected))
         main-opts (some :main-opts (reverse selected))
         project-paths (concat (or (:paths edn) ["src"]) extra-paths)
         project-roots (map #(abspath project-dir %) project-paths)
         all-deps (merge (:deps edn) extra-deps)
         {dep-roots :roots dep-natives :natives}
         (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo edn)]
                                      (abspath project-dir r))]
           (resolve-deps all-deps project-dir))]
     ;; reconcile: the project's own roots/natives + every dep's, deduped once.
     {:roots (dedup-by identity (concat project-roots dep-roots))
      :main-opts main-opts
      ;; the project's own paths (relative to project-dir) and absolute resource
      ;; roots, plus its :jolt/build options — `jolt build` uses these to bundle
      ;; resources into / alongside a standalone binary.
      :project-dir project-dir
      :project-paths (vec project-paths)
      :project-roots (vec project-roots)
      :build (:jolt/build edn)
      :embed-dirs (mapv #(abspath project-dir %) (:embed (:jolt/build edn)))
      :tasks (:tasks edn)
      :natives (dedup-by native-key (concat (:jolt/native edn) dep-natives))
      ;; nREPL middleware a library contributes (jolt.nrepl composes them over its
      ;; built-in handler) — symbols resolving to a middleware fn or a vector of them.
      :nrepl-middleware (:nrepl/middleware edn)})))

(defn add-deps
  "Resolve an inline deps map and add the resulting source roots to the loader,
  so a following `require` can load them — the programmatic twin of a deps.edn
  :deps entry, mirroring babashka.deps/add-deps:

    (add-deps '{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"}}})
    (require '[clojure.data.json :as json])

  Coordinates: :git/url + :git/sha, :local/root (resolved against JOLT_PWD),
  and :mvn/version (JAR source fetched from Clojars, then Central). A top-level
  :mvn/local-repo in the map relocates the Maven repository for this call,
  like the deps.edn key. New roots
  are appended AFTER the current roots, so an added dep can never shadow a
  namespace the runtime already resolves. Returns the vector of roots added
  (empty when everything was already on the roots).

  :jolt/native declarations carried by added deps are NOT auto-loaded (that is
  a project-launch concern — see jolt.main); a warning names them so the
  caller can load via jolt.ffi. The second arity accepts an options map for
  babashka call-shape compatibility; no options are currently honored."
  ([deps-map] (add-deps deps-map nil))
  ([{:keys [deps] :as m} _opts]
   (let [base (or (jolt.host/getenv "JOLT_PWD") ".")
         {:keys [roots natives]}
         (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo m)]
                                      (abspath base r))]
           (resolve-deps deps base))
         current (vec (jolt.host/source-roots))
         added (vec (remove (set current) (dedup-by identity roots)))]
     (when (seq added)
       (jolt.host/set-source-roots! (into current added)))
     (when (seq natives)
       (warn "added deps declare :jolt/native libraries (not auto-loaded): "
             (pr-str (dedup-by native-key natives))))
     added)))
