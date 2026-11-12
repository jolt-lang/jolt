(ns jolt.deps
  "Resolve a deps.edn into an ordered list of source roots. A reduced
  tools.deps: :paths, :deps (`:git/url`+`:git/sha` / `:local/root` /
  `:mvn/version`), :aliases, :tasks. jolt's own keys are :jolt/native (shared
  libraries to load), :jolt/build, :nrepl/middleware, and :jolt/min-version —
  the oldest jolt the project or library works on, refused rather than run. Alias maps combine with the reference
  tools.deps semantics (jolt.deps.edn, lifted from clojure.tools.deps.edn):
  :extra-deps / :override-deps / :default-deps / :replace-deps (legacy :deps),
  :extra-paths / :replace-paths (legacy :paths), :main-opts.

  The deps walk is breadth-first so a top-level coordinate registers before any
  transitive one (a top-level pin wins). Git deps reuse an existing
  tools.gitlibs checkout ($GRENADINE_GITLIBS_DIR / $GITLIBS / ~/.gitlibs)
  when the JVM toolchain already fetched them, else clone into a sha-immutable
  cache ($JOLT_GITLIBS_DIR, else a jolt/ subdir of the shared cache) across
  projects.
  Maven POMs and jars live in the standard local repository
  (~/.m2/repository; :mvn/local-repo in deps.edn relocates it like tools.deps,
  JOLT_MAVEN_REPOSITORY or GRENADINE_MAVEN_REPOSITORY supplies an environment
  default) shared with the JVM toolchain
  in both directions. Grenadine expands the dependency tree, builds effective
  POMs, and compares Maven versions; files are fetched by jolt itself over
  HTTPS (jolt.mvn-http);
  git still shells out through jolt.host/sh; a jar is a source root read in
  place through its central directory, never extracted (nothing here touches
  the JVM)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [grenadine.expander :as expander]
            [grenadine.pom :as grenadine.pom]
            [grenadine.require-deps :as required]
            [grenadine.version :as grenadine.version]
            [jolt.deps.edn :as dedn]
            [jolt.deps.declarations :as decl]
            [jolt.deps.ext :as ext]
            [jolt.deps.git :as git]
            [jolt.deps.io :refer [getenv file-exists? mkdirs! rm-f mv! tmp-dir warn
                                  sanitize]]
            [jolt.mvn-http :as http]))

(defonce ^:private required-state
  (atom {:coordinates {} :namespaces {}}))

;; The directory a path names, as File.getCanonicalPath answers it: "." and ".."
;; folded and symlinks resolved. A :local/root is identified by this, not by its
;; spelling — tools.deps canonicalizes the root the same way — so a multi-module
;; project that reaches one module as "../libs" and again as "../tui/../libs"
;; holds one coordinate, not two that cannot be compared.
(defn- canonical-path [p] (.getCanonicalPath (java.io.File. p)))

(defn- find-file
  "Walk `dir` for the first entry whose NAME satisfies `pred`, returning its
  full path, or nil. Every name at a level is checked before any subdirectory
  below it is descended into, and a symlinked directory is not followed — as
  the `find … -print -quit` this replaces did not."
  [dir pred]
  (let [full #(str dir "/" %)]
    (when-let [entries (seq (jolt.host/list-dir dir))]
      (or (some #(when (pred %) (full %)) entries)
          (some #(let [p (full %)]
                   (when (and (jolt.host/directory? p) (not (jolt.host/symlink? p)))
                     (find-file p pred)))
                entries)))))

;; Progress / informational lines (fetching, using-cache, skipping, added-natives)
;; print only when JOLT_DEBUG is set — otherwise a routine run (e.g. a ys-generated
;; program pulling a native-declaring lib) barfs them on every invocation. Genuine
;; warnings (an unresolvable dep, a malformed deps.edn) always print via `warn`.
(def ^:dynamic *verbose*
  "True while the CLI's -Sverbose is in effect: the progress lines print for this
  resolution without JOLT_DEBUG being set for the whole process."
  false)
(defn- info [& xs] (when (or *verbose* (getenv "JOLT_DEBUG")) (apply warn xs)))

;; --- deps that could not be resolved ------------------------------------------
;; A Maven dep whose download failed used to leave procurement with no root, and
;; the expansion treats no-root as {:manifest :none} — contributing nothing,
;; exactly like the DELIBERATELY tolerant case of a jar with no jolt-loadable
;; source. So a declared dependency silently left the classpath and the program
;; failed later somewhere that never mentioned it: a transient fetch failure for
;; org.clojure/spec.alpha surfaced as 55 "connect refused" errors in an nREPL
;; suite, because orchard needed it and never loaded.
;;
;; UNRESOLVABLE IS NOW FATAL, which is what the reference implementation does.
;; tools.deps aborts with "Error building classpath. The following artifacts
;; could not be resolved: …" and exits 1, for a missing artifact and for an
;; unreachable repository alike — verified against Clojure CLI 1.12.5.1654.
;; jolt already did this for GIT deps (a tag that does not exist throws); Maven
;; was the outlier.
;;
;; Note what is NOT affected: a jar that downloaded fine and simply carries no
;; jolt-loadable source is a quiet leaf — on the roots for the resources it may
;; package, its own deps unwalked. That is a different condition — the artifact
;; resolved — and plenty of JVM-only jars are declared transitively. Only a
;; failure to OBTAIN the artifact is fatal.
;;
;; Collected rather than thrown at the first failure, so the message names every
;; unresolvable dep at once the way tools.deps' does, instead of making you fix
;; them one build at a time.
(def ^:private ^:dynamic *unresolvable* nil)
(defn- note-unresolvable! [coord version detail]
  (when *unresolvable*
    (swap! *unresolvable* conj {:coord coord :version version :detail detail})))
(defn- fail-unresolvable! []
  (when-let [missed (some-> *unresolvable* deref seq)]
    (throw (ex-info
            (str "Error building classpath. The following artifacts could not be resolved:\n"
                 (str/join "\n" (map #(str "  " (:coord %) " " (:version %) " — " (:detail %))
                                     missed)))
            {:unresolved (mapv #(select-keys % [:coord :version :detail]) missed)}))))

;; A project file is edn that also carries CODE: a :tasks body is a form jolt
;; evaluates, and the ones people write are full of reader macros —
;; (run 'clean), (require '[app.core]), @(future …), a `~ template. So the
;; reader here is the source one with *read-eval* off, which is what babashka
;; reads a bb.edn with and the only reader that takes those.
;;
;; clojure.edn cannot: EDN has no quote and no deref, and refuses ' @ ` ~
;; outright. It only appeared to work while jolt's edn mode still honored the
;; source reader macros, and the moment that seam was made strict (#905) a task
;; body holding an @ stopped reading at all. *read-eval* false is the piece
;; worth keeping from the edn reading — a project file must not evaluate
;; anything at READ time.
(defn- read-edn [path]
  (when (file-exists? path)
    (try (binding [*read-eval* false] (read-string (slurp path)))
         (catch :default e
           (throw (ex-info (str path ": " (ex-message e)) {:path path :error e}))))))

(defn- ascii-alpha? [c]
  (let [n (int c)]
    (or (<= (int \A) n (int \Z))
        (<= (int \a) n (int \z)))))

(defn- path-separator? [c] (or (= c \/) (= c \\)))

(defn- native-path-kind-for
  "Classify P without touching the filesystem. Windows has four useful forms:
  drive-absolute (C:/x), UNC/device (//server/share, //?/C:/x), root-relative
  (/x), and drive-relative (C:x). The last one depends on Windows' hidden
  per-drive current directory, which the source launcher cannot preserve after
  it changes directory, so the resolver rejects it rather than inventing a
  different path."
  [windows? p]
  (let [n (count p)
        drive? (and (>= n 2) (ascii-alpha? (nth p 0)) (= \: (nth p 1)))
        sep0? (and (pos? n) (path-separator? (nth p 0)))]
    (cond
      (not windows?) (if (str/starts-with? p "/") :absolute :relative)
      drive? (if (and (>= n 3) (path-separator? (nth p 2)))
               :absolute
               :drive-relative)
      sep0? (if (and (>= n 2) (path-separator? (nth p 1)))
              :absolute
              :root-relative)
      :else :relative)))

(defn- windows? []
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "win"))

;; native-path-kind-for against the RUNNING host, public because the same line
;; has to be drawn outside the resolver: jolt.main classifies the FILE argument
;; with it. Recognizing only the POSIX root there read `jolt C:/x/hello.clj` as
;; project-relative and tried to open "./C:/x/hello.clj" (jolt-lang/jolt#992).
(defn path-kind [p] (native-path-kind-for (windows?) p))

(defn- windows-drive-prefix? [p]
  (and (>= (count p) 2)
       (ascii-alpha? (nth p 0))
       (= \: (nth p 1))))

(defn- root-relative-for [dir p]
  (cond
    (windows-drive-prefix? dir) (str (subs dir 0 2) p)
    (= :absolute (native-path-kind-for true dir))
    (str (if (and (> (count dir) 2)
                  (path-separator? (nth dir (dec (count dir)))))
           (subs dir 0 (dec (count dir)))
           dir)
         p)
    :else p))

(defn- abspath-for [windows? dir p]
  (case (native-path-kind-for windows? p)
    :absolute p
    :root-relative (root-relative-for dir p)
    :drive-relative
    (throw (ex-info (str "drive-relative path " (pr-str p)
                         " is ambiguous; use a drive-absolute path such as C:/path")
                    {:path p :kind :drive-relative}))
    (str dir "/" p)))

(defn- abspath [dir p]
  (abspath-for (windows?) dir p))

;; --- git checkouts ------------------------------------------------------------
;; The cache, tag resolution and the clone itself live in jolt.deps.git.
(defn- ensure-git
  "Return a checkout dir for url@sha: jolt's cached checkout when complete, else
  an existing tools.gitlibs checkout for `lib`, else a fresh clone into jolt's
  cache."
  [lib url sha]
  (let [repo-dir (str (git/gitlibs-dir) "/" (sanitize url))
        dir (str repo-dir "/" sha)]
    (if (git/checkout-complete? dir)
      dir
      (or (git/gitlibs-shared-checkout lib sha)
          (do (info "fetching " url " @ " (subs sha 0 (min 12 (count sha))))
              (git/fetch-git! url sha repo-dir))))))

;; --- maven cache ------------------------------------------------------------
;; jolt has no JVM, but a Clojure library's Maven JAR carries its .clj/.cljc/.cljs
;; SOURCE (Clojure ships source, not just bytecode). So a :mvn/version coordinate
;; resolves by fetching the JAR (Clojars, then Central) and putting the jar
;; itself on the source roots: the loader reads a namespace or a resource out
;; of it through its central directory, as the JVM reads a jar on the
;; classpath, and nothing is extracted (jolt issue #1005). Its pom.xml, read
;; the same way, supplies the transitive deps. A JAR of pure Java classes has no
;; source to run, but the resources it packages are still readable through it,
;; so it stays on the roots as a leaf.
;;
;; JARs live at their standard path in the local Maven repository
;; (~/.m2/repository), so they are shared with JVM Clojure/tools.deps in both
;; directions: an artifact clj already fetched is reused without a download, and
;; one jolt fetches is there for clj. The repository location is configured the
;; way tools.deps configures it — the :mvn/local-repo top key of deps.edn (also
;; accepted in an add-deps map); anyone already using it gets the same behavior
;; for free. JOLT_MAVEN_REPOSITORY supplies the dialect override and
;; GRENADINE_MAVEN_REPOSITORY the shared default. Setting JOLT_MVNLIBS opts out
;; of sharing entirely: jars are kept under it, in a layout of jolt's own.

(def ^:private ^:dynamic *mvn-local-repo*
  "The :mvn/local-repo of the resolution in progress (bound by resolve-project /
  add-deps from their deps.edn / deps map), nil for the default." nil)

(defn- m2-repo-dir
  "The local Maven repository dir. An explicit :mvn/local-repo wins, followed
  by Jolt's environment setting, Grenadine's shared setting, then the standard
  ~/.m2/repository. Relative environment overrides resolve against JOLT_PWD,
  matching :mvn/local-repo and every other project-relative path."
  ([] (m2-repo-dir *mvn-local-repo*
                   (getenv "JOLT_MAVEN_REPOSITORY")
                   (getenv "GRENADINE_MAVEN_REPOSITORY")
                   (getenv "HOME")
                   (or (getenv "JOLT_PWD") ".")))
  ([cfg jolt-override grenadine-override home base]
   (or cfg
       (when jolt-override (abspath base jolt-override))
       (when grenadine-override (abspath base grenadine-override))
       (str (or home ".") "/.m2/repository"))))

(def ^:private default-mvn-repos
  ["https://repo.clojars.org" "https://repo1.maven.org/maven2"])

(def ^:private ^:dynamic *mvn-repos*
  "Repository base URLs consulted in order, bound by resolve-project from the
  merged deps.edn's :mvn/repos ({\"name\" {:url \"…\"}}, the tools.deps key) —
  defaults first, then custom repos sorted by name for a deterministic order."
  default-mvn-repos)

(defn- mvn-repo-urls [edn]
  (into default-mvn-repos
        (keep (fn [[_name m]] (let [u (:url m)]
                                (when (and u (not-any? #(= % u) default-mvn-repos))
                                  (str/replace u #"/+$" ""))))
              (sort-by key (:mvn/repos edn)))))

(defn- mvn-group [coord] (or (namespace coord) (name coord)))

(defn- maven-path
  [{:keys [group artifact version]} extension]
  (str (str/replace group "." "/") "/" artifact "/" version "/"
       artifact "-" version extension))

(defn- fetch-maven-file
  "Fetch `relative` into `target` from the configured repositories. Jolt's
  downloader writes through a temporary file and atomically renames."
  [relative target]
  (if (file-exists? target)
    target
    (do
      (mkdirs! (subs target 0 (str/last-index-of target "/")))
      (loop [repos *mvn-repos*]
        (when-let [repo (first repos)]
          (if (http/fetch (str repo "/" relative) target)
            target
            (recur (rest repos))))))))

(defn- mvn-http-host
  "The host map grenadine.repo asks for: an in-memory GET. jolt's downloader
  writes an artifact to a path — that is the right shape for a jar and the
  wrong one for maven-metadata.xml, which is a few KB nobody wants on disk, so
  this fetches to a temp file and reads it back.

  :bytes->utf8 is `identity` because the read already decoded: slurp returns the
  text, and the contract only requires that (:bytes->utf8 host) turn whatever
  (:http-get host) put in :body into a string."
  []
  {:http-get
   (fn [url]
     (let [target (str (tmp-dir) "/jolt-mvn-meta-"
                       (System/currentTimeMillis) "-" (rand-int 100000) ".xml")
           result (http/fetch* url target)]
       (try
         (if (= :ok (:outcome result))
           {:status 200 :body (slurp target)}
           {:status (or (:status result) 404)})
         (finally (rm-f target)))))
   :bytes->utf8 identity})

(defn- pom-text
  "Return a POM as text, sharing the standard Maven repository with tools.deps."
  [coords]
  (let [relative (maven-path coords ".pom")
        target (str (m2-repo-dir) "/" relative)]
    (if-let [path (fetch-maven-file relative target)]
      (slurp path)
      (throw
       (ex-info
        (str "POM not found: " (:group coords) "/" (:artifact coords)
             " " (:version coords))
        {:type :jolt.deps/pom-not-found
         :coords coords})))))

(defn- jar-path
  "The path of ENTRY inside the jar at JAR, as the loader spells it: the JVM's
  jar: URL, which slurp, io/reader and jolt.host/jar-entry-string all read."
  [jar entry]
  (str "jar:file:" jar "!/" entry))

(defn- jar-path? [p] (str/starts-with? (str p) "jar:file:"))

(defn- archive-or-note!
  "The jar's path when `jar` is a whole zip archive — an END record and every
  central-directory header that read — else nil after recording the artifact
  as unresolvable. The jar is on disk but cannot be read (a cut download, a
  file that is not a jar, a full disk) — a failure to OBTAIN the artifact, not
  a skip. Left silent, the resolution completes without the dep's root and
  the cpcache persists the degraded roots until the project's .jolt is
  deleted by hand; refusing here fails it before anything is cached."
  [jar coord version]
  (if (jolt.host/zip-archive? jar)
    jar
    (do (note-unresolvable! coord version
                            (str jar " is not a whole zip archive (a cut download, or not a jar); delete it to fetch again"))
        nil)))

(defn- repo-failure
  "Why REPO's fetch result R failed, for the unresolvable message — nil when the
  repo simply answered that it has no such artifact."
  [repo r]
  (when-not (= :not-found (:outcome r))
    (str repo " — "
         (or (:error r)
             (when (:status r) (str "HTTP " (:status r)))
             (name (:outcome r))))))

(defn- download-maven-jar
  "Fetch the jar at URL-PATH (relative to a repository) into JAR under DIR,
  trying each of *mvn-repos* in turn, and answer its path — or nil after
  recording coord@version as unresolvable."
  [coord version dir url-path jar]
  ;; ERRORS carries the repos that failed for a reason OTHER than "no such
  ;; artifact". Every repo answering 404 is a real "not found" and says so;
  ;; a reset or a 503 is not, and reporting it as absent sent people
  ;; looking for a dependency that was published years ago. Either way the
  ;; dep is unresolvable and resolution will abort — the distinction is
  ;; what the message says, and whether a retry was even attempted.
  (loop [repos *mvn-repos* errors []]
    (if (empty? repos)
      (do (note-unresolvable!
           coord version
           (if (seq errors)
             ;; a reset, a 503, a TLS failure: the repos never told us
             ;; whether they have it, so do not claim they said no
             (str "could not be fetched: " (str/join "; " errors))
             (str "not found in any repository (tried "
                  (str/join ", " *mvn-repos*) ")")))
          nil)
      (let [repo (first repos)
            _ (mkdirs! dir)
            r (http/fetch* (str repo "/" url-path) jar)]
        (if (= :ok (:outcome r))
          (do (info "fetching " coord " " version)
              (archive-or-note! jar coord version))
          (recur (rest repos)
                 (if-let [failure (repo-failure repo r)]
                   (conj errors failure)
                   errors)))))))

(defn- ensure-maven
  "Ensure coord@version's JAR is in the local Maven repository (reusing one the
  JVM toolchain already fetched; downloading from Clojars then Central when
  absent) and answer the jar's path, which is the dependency's source root:
  the loader reads it in place. Returns nil after recording the artifact as
  unresolvable (absent from every repo, fetch failures, or a jar that is not a
  whole archive) — the expansion reports every one and aborts at its end."
  [coord version]
  (let [group (mvn-group coord) artifact (name coord)
        vdir-rel (str (str/replace group "." "/") "/" artifact "/" version)
        jar-name (str artifact "-" version ".jar")
        legacy (getenv "JOLT_MVNLIBS")
        dir (if legacy
              (str legacy "/" (sanitize (str coord)) "/" (sanitize version))
              (str (m2-repo-dir) "/" vdir-rel))
        jar (str dir "/" jar-name)]
    (if (file-exists? jar)
      (do (when-not legacy (info "using " jar-name " from the local Maven repository"))
          (archive-or-note! jar coord version))
      (download-maven-jar coord version dir (str vdir-rel "/" jar-name) jar))))

(defn- raw-pom-deps-from
  "Transitive deps read from a pom.xml — as a deps map so the expansion walks
  them like any other. Repository Maven deps use Grenadine effective POMs; this
  reads whatever the jar itself packages, for a local jar whose Maven
  coordinates are unknown and for a dep whose effective POM wouldn't build. It
  skips a dependency it can't read a literal version for rather than failing."
  [pom]
  (do
    (when-let [xml (if (jar-path? pom)
                     (jolt.host/jar-entry-string pom)
                     (when (file-exists? pom) (slurp pom)))]
      (let [grab (fn [tag block] (second (re-find (re-pattern (str "<" tag ">(.*?)</" tag ">")) block)))]
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

(defn- jar-root?
  "Is `root` a jar rather than a directory? The loader reads a .jar/.zip root
  in place; every other root is a directory."
  [root]
  (let [r (str/lower-case (str root))]
    (and (or (str/ends-with? r ".jar") (str/ends-with? r ".zip"))
         (not (jolt.host/directory? root)))))

(defn- has-clj-source?
  "Does the tree hold any jolt-loadable source (.clj/.cljc)? A Maven JAR that is
  pure-Java (closure-compiler) or ClojureScript-only (cljs.java-time) has none,
  so nothing in it can be required. It still belongs on the roots — a jar can
  carry RESOURCES and nothing else, which is exactly what Cognitect's aws
  endpoints data is, and io/resource has to find them. What it does not get is
  a WALK: with no source of ours to load, the deps it declares are its
  publisher's own JVM/cljs toolchain, and jolt has no JVM to run them on."
  [root]
  (let [source? #(or (str/ends-with? % ".clj") (str/ends-with? % ".cljc"))]
    (boolean (if (jar-root? root)
               (some source? (jolt.host/zip-entry-names root))
               (find-file root source?)))))

;; --- coordinate skips + normalization ----------------------------------------
;; jolt IS Clojure, so org.clojure/clojure is intrinsic; jolt has no
;; ClojureScript compiler, so clojurescript (and the closure/rhino toolchain it
;; drags in) is dead weight a cljc library declares only for its :cljs branch.
(defn- intrinsic-dep? [lib]
  (or (= lib 'org.clojure/clojure) (= lib 'org.clojure/clojurescript)))

(defn- absolutize-local [spec base-dir]
  (if (and (map? spec) (:local/root spec))
    (update spec :local/root #(abspath base-dir %))
    spec))

(defn- filter-deps
  "Normalize a raw child/top deps map into expansion entries: drop intrinsics
  and obsolete :jolt/module coords, absolutize :local/root against the
  declaring project's dir. A nil coordinate survives (an alias's :default-deps
  may fill it during expansion)."
  [deps base-dir]
  (into []
        (mapcat (fn [[lib spec]]
                  (cond
                    (intrinsic-dep? lib) nil
                    (:jolt/module spec)
                    (do (info "skipping janet dependency " lib " (:jolt/module is obsolete on Chez)") nil)
                    :else [[lib (absolutize-local spec base-dir)]])))
        deps))

(defn- known-coord? [coord]
  (try (some? (ext/coord-type coord)) (catch :default _ false)))

(defn- warn-unsupported [lib coord]
  (warn "skipping unsupported coordinate " lib " " (pr-str coord)
        "\n  a dependency needs one of:"
        "\n    {:mvn/version \"1.2.3\"}                              a Maven artifact"
        "\n    {:git/url \"https://…\" :git/sha \"<full-sha>\"}       an explicit git repo"
        "\n    {:git/sha \"<full-sha>\"} on io.github.OWNER/REPO     a git repo by host-prefixed name"
        "\n    {:git/tag \"v1.2\" :git/sha \"<short-sha>\"}           a git repo pinned by tag + sha prefix"
        "\n    {:local/root \"../path\"}                             a directory (or jar) on disk"))

;; --- coordinate extensions ----------------------------------------------------
;; The :mvn / :git / :local coordinate types implement the jolt.deps.ext SPI
;; over jolt's procurement (ensure-maven / ensure-git / local dirs). Procurement
;; is memoized per resolution (*procure-memo*).

(def ^:private ^:dynamic *procure-memo* nil)
(defn- memoized [k f]
  (if *procure-memo*
    (if-let [e (find @*procure-memo* k)]
      (val e)
      (let [v (f)] (swap! *procure-memo* assoc k v) v))
    (f)))

;; True when the caller runs JVM bytecode itself and asked for source-less Maven
;; jars to be walked too. resolve-deps binds it for the length of one resolution.
(def ^:private ^:dynamic *jvm-deps* nil)

(defn- effective-pom-deps
  "Build a Maven dependency map from Grenadine's effective POM model. Parent
  inheritance, properties, dependency management, BOM imports, and exclusions
  have already been resolved by Grenadine.

  nil when the effective POM can't be built — the .pom isn't in the repository
  and can't be fetched (a hand-installed jar, or an offline machine), or the POM
  names something Grenadine won't guess at, such as a version that stays
  `${unresolved}` because it is set in a profile. Grenadine asserts every
  declared coordinate before jolt filters by scope, so a test-scoped dependency
  jolt drops anyway can be what makes a POM unmodellable. A nil sends the caller
  to whatever pom.xml the jar itself carries; the alternative, failing the whole
  resolution over a dependency that may not even be reachable, is worse.

  quiet? suppresses the fallback warning, for a caller that has its own answer
  for a missing POM and so is not degrading — the Clojure spec substitution
  below reads this way and would otherwise warn on every offline resolution
  about a jar jolt never loads."
  ([lib coord] (effective-pom-deps lib coord false))
  ([lib coord quiet?]
  (memoized
   [:effective-pom lib (ext/dep-id lib coord)]
   (fn []
     (let [coords {:group (mvn-group lib)
                   :artifact (name lib)
                   :version (:mvn/version coord)}
           effective (try (grenadine.pom/effective-pom coords pom-text)
                          (catch :default e
                            (when-not quiet?
                              (warn "no effective POM for " lib " "
                                    (:mvn/version coord) ": " (ex-message e)
                                    "\n  falling back to the pom.xml in its jar;"
                                    " transitive deps may be incomplete"))
                            nil))]
       (when effective
         (into
          {}
          (keep
           (fn [{:keys [group artifact version scope optional exclusions]}]
             (when (and group artifact version
                        (not (#{"test" "provided" "system"} scope))
                        (not optional))
               [(symbol group artifact)
                (cond-> {:mvn/version version}
                  (seq exclusions)
                  (assoc :exclusions
                         (set
                          (map
                           (fn [{:keys [group artifact]}]
                             (symbol group artifact))
                           exclusions))))])))
          (:deps effective))))))))

(defn- manifest-info
  "Manifest detection for a git/local directory root: its deps.edn, else a bare
  source tree. A directory with no deps.edn contributes its default `src` path
  and no transitive deps — a pom.xml is only consulted for an EXTRACTED artifact
  (a Maven dep or a jar local root), where it is the only source of children."
  [root]
  (if (file-exists? (str root "/deps.edn"))
    (let [edn (try (some-> (read-edn (str root "/deps.edn")) dedn/canonicalize)
                   (catch :default e (warn (ex-message e)) nil))]
      (when (and edn (:deps edn) (not (map? (:deps edn))))
        (throw (ex-info (str "malformed :deps in " root "/deps.edn: expected a map")
                        {:path root :given (class (:deps edn))})))
      {:root root :manifest :deps-edn :edn edn})
    {:root root :manifest :deps-edn :edn nil}))

(defmethod ext/coord-type-keys :mvn [_] #{:mvn/version})
(defmethod ext/coord-type-keys :git [_] #{:git/url :git/sha :git/tag})
(defmethod ext/coord-type-keys :local [_] #{:local/root})

(defmethod ext/dep-id :mvn [_ coord] (select-keys coord [:mvn/version]))
(defmethod ext/dep-id :git [_ coord] (select-keys (git/git-coord coord) [:git/url :git/sha :git/tag]))
(defmethod ext/dep-id :local [_ coord] {:local/root (canonical-path (:local/root coord))})

;; What names a version in the dependency tree: the Maven version, the git tag
;; if the coordinate is pinned by one (else the sha, shortened the way git
;; prints it), the directory for a local root.
(defmethod ext/coord-summary :mvn [lib coord] (str lib " " (:mvn/version coord)))
(defmethod ext/coord-summary :git [lib coord]
  (let [coord (git/git-coord coord)
        sha (:git/sha coord)]
    (str lib " " (or (:git/tag coord)
                     (when sha (subs sha 0 (min 7 (count sha))))))))
(defmethod ext/coord-summary :local [lib coord] (str lib " " (:local/root coord)))

(defn- mvn-info [lib coord]
  (memoized [:info lib (ext/dep-id lib coord) (boolean *jvm-deps*)]
    (fn []
      (let [root (ensure-maven lib (:mvn/version coord))]
        (cond
          (nil? root) {:root nil :manifest :none}
          ;; a Maven dep with no jolt-loadable source is a LEAF: the jar
          ;; stays on the roots so io/resource can read whatever it packages,
          ;; but with no source of ours in it, the deps it declares are cljs/JVM
          ;; tooling — no :deps and no :pom here is what stops the walk. A caller
          ;; with its own JVM (:jvm-deps?) gets the walk instead, through :else.
          (and (not *jvm-deps*) (not (has-clj-source? root))) {:root root :manifest :mvn}
          ;; :pom is the fallback children-of reaches for when the effective POM
          ;; can't be built — not every jar carries one, and the ones that do
          ;; predate their own dependencyManagement, so it is second choice.
          :else {:root root :manifest :mvn
                 :deps (effective-pom-deps lib coord)
                 :pom (jar-path root (str "META-INF/maven/" (mvn-group lib) "/"
                                          (name lib) "/pom.xml"))})))))

(defn- git-info [lib coord]
  (memoized [:info lib (ext/dep-id lib coord)]
    (fn []
      (let [coord (git/git-coord coord)
            url (or (:git/url coord) (git/infer-git-url lib)
                    (throw (ex-info
                             (str "git dep " lib " has no :git/url and none could be inferred "
                                  "from its lib name. Add :git/url, or name the coordinate after "
                                  "its host, e.g. io.github.OWNER/REPO for a GitHub repo.")
                             {:lib lib :coord coord})))
            sha (git/resolve-git-sha lib url coord)
            checkout (ensure-git lib url sha)
            root (if-let [r (:deps/root coord)] (str checkout "/" r) checkout)]
        (assoc (manifest-info root) :sha sha :url url :checkout checkout)))))

(defn- local-info [lib coord]
  (memoized [:info lib (ext/dep-id lib coord)]
    (fn []
      (let [path (:local/root coord)]
        (if (str/ends-with? path ".jar")
          ;; a jar local root is read in place, like a Maven jar; the pom it
          ;; packages, if any, is the first META-INF/**/pom.xml entry
          (if (archive-or-note! path lib path)
            (let [pom (some #(when (and (str/starts-with? % "META-INF/")
                                        (str/ends-with? % "/pom.xml"))
                               (jar-path path %))
                            (jolt.host/zip-entry-names path))]
              {:root path :manifest :mvn :pom pom})
            {:root nil :manifest :none})
          (manifest-info path))))))

(defmethod ext/coord-info :mvn [lib coord] (mvn-info lib coord))
(defmethod ext/coord-info :git [lib coord] (git-info lib coord))
(defmethod ext/coord-info :local [lib coord] (local-info lib coord))

(defn- children-of [{:keys [root manifest edn pom deps]}]
  (cond
    (nil? root) []
    (= manifest :deps-edn) (filter-deps (:deps edn) root)
    (and (= manifest :mvn) deps) (filter-deps deps root)
    ;; the pom.xml the jar packages, when no effective POM could be built
    ;; from a .pom beside it: a jar: path, which raw-pom-deps-from reads out
    ;; of the archive and answers nil for when the jar holds none
    (and (= manifest :mvn) pom) (filter-deps (raw-pom-deps-from pom) root)
    :else []))

(defmethod ext/coord-deps :mvn [lib coord] (children-of (mvn-info lib coord)))
(defmethod ext/coord-deps :git [lib coord] (children-of (git-info lib coord)))
(defmethod ext/coord-deps :local [lib coord] (children-of (local-info lib coord)))

;; version comparison: Maven versions order by ComparableVersion semantics;
;; git shas by commit ancestry when determinable from an existing clone. Any
;; other pairing has no order — the expansion warns and keeps the already-
;; selected coordinate (tools.deps throws instead; jolt prefers resolving with
;; the first-seen version over failing the whole resolution).
(defmethod ext/compare-versions [:mvn :mvn] [_ x y]
  (grenadine.version/compare-versions
   (:mvn/version x)
   (:mvn/version y)))

(defmethod ext/compare-versions [:git :git] [lib x y]
  (let [xi (git-info lib x) yi (git-info lib y)
        xs (:sha xi) ys (:sha yi)]
    (cond
      (= xs ys) 0
      :else
      (let [in (fn [dir] (cond (git/git-ancestor? dir xs ys) -1
                               (git/git-ancestor? dir ys xs) 1))]
        (or (in (:checkout xi)) (in (:checkout yi))
            (throw (ex-info (str "No known ancestor relationship between git versions for " lib)
                            {:lib lib :x x :y y})))))))

;; --- dependency expansion --------------------------------------------------

(defn- base-lib
  [lib]
  (let [lib-name (first (str/split (name lib) #"\$"))]
    (symbol (namespace lib) lib-name)))

(defn- expansion-warning
  [{:keys [warning lib coordinate selected candidate message]}]
  (case warning
    :unsupported-coordinate
    (warn-unsupported lib coordinate)

    :versions-not-comparable
    (warn "version conflict for " lib ": keeping " (pr-str selected)
          " over " (pr-str candidate) " (" message ")")

    (warn (pr-str warning))))

;; --- reconciliation ---------------------------------------------------------
;; Dependencies are resolved as a TREE (resolve-deps' BFS, which visits each
;; coordinate once) and then reconciled into a definitive, de-duplicated set —
;; one place, not ad-hoc per call site. dedup-by keeps the first item per key,
;; order preserved; it dedups source roots (by path), native libraries (by
;; identity) and the :allow-dynamic list (by name), so an app pulling two libs
;; that declare the same shared object (e.g. libcrypto via both http-client and
;; the ring adapter) includes and loads it ONCE.
(defn- dedup-by [key xs]
  (second (reduce (fn [[seen acc] x]
                    (let [k (key x)]
                      (if (contains? seen k) [seen acc] [(conj seen k) (conj acc x)])))
                  [#{} []] xs)))

(defn- absolutize-coords
  "An :override-deps / :default-deps map with each :local/root made absolute
  against BASE-DIR; nil stays nil."
  [coords base-dir]
  (some->> coords (map (fn [[l c]] [l (absolutize-local c base-dir)])) (into {})))

(defn- expand
  "Run the tools.deps expansion engine over the TOP entries with jolt's
  coordinate extensions."
  [top override-deps default-deps trace?]
  (expander/expand-deps
   top
   {:coord-id ext/dep-id
    :coord-deps ext/coord-deps
    :compare-versions ext/compare-versions
    :known-coordinate? known-coord?
    :base-lib base-lib
    :override-deps override-deps
    :default-deps default-deps
    :trace? trace?
    :on-warning expansion-warning}))

(defn- selected-infos
  "The coord-info of every selected library that procured a root, in ORDER,
  each tagged with its :lib."
  [libmap order]
  (keep (fn [lib]
          (when-let [coord (get libmap lib)]
            (let [info (ext/coord-info lib coord)]
              (when (:root info) (assoc info :lib lib)))))
        order))

(defn- deps-edn-roots
  "The absolute source roots a deps.edn at ROOT declares (:paths, else src)."
  [root edn]
  (map #(abspath root %) (or (:paths edn) ["src"])))

(defn- info-roots [{:keys [root manifest edn]}]
  (if (= manifest :deps-edn)
    (deps-edn-roots root edn)
    [root]))

(defn- info-natives
  "A library's :jolt/native specs, each tagged with the root that declared it,
  the library, and — for a deps.edn library — its source roots."
  [{:keys [edn root manifest lib]}]
  (let [paths (when (= manifest :deps-edn)
                (vec (deps-edn-roots root edn)))]
    (map #(cond-> (assoc % :jolt.deps/root root :jolt.deps/lib (str lib))
            (seq paths) (assoc :jolt.deps/paths paths))
         (:jolt/native edn))))

(defn- release-root
  "LIB's root when its selected coordinate is a Maven release (not a SNAPSHOT)."
  [libmap {:keys [lib root]}]
  (let [v (:mvn/version (get libmap lib))]
    (when (and (string? v) (not (str/ends-with? v "-SNAPSHOT")))
      root)))

(defn- resolution
  "resolve-deps' answer, collected from the selected libraries' INFOS."
  [infos libmap expansion graph]
  {:roots (vec (mapcat info-roots infos))
   ;; Each spec carries the ROOT of the deps.edn that declared it. A
   ;; :jolt/native path is written relative to its own project ("native/
   ;; libfoo.so" is what a build task produces beside the sources), and
   ;; without the root there is nothing to resolve it against: a dependency's
   ;; relative path was being resolved against the APP's directory, and a
   ;; :static archive not at all — it went to `cc` verbatim and resolved
   ;; against whatever the build's cwd happened to be (jolt-9a8).
   ;;
   ;; It also carries the dependency's SOURCE roots and its coordinate: a
   ;; dependency's natives load when the first namespace from one of those
   ;; roots is required, not at startup (jolt.main apply-project!), and a
   ;; library that is missing then names the dependency that declared it.
   :natives (vec (mapcat info-natives infos))
   ;; The defs each dep vouches never resolve vars at runtime in a built
   ;; binary (:jolt/tree-shake {:allow-dynamic […]}). A LIBRARY is the
   ;; natural declarer — spec.alpha knows its `res` only qualifies a symbol
   ;; for a description and its `dynaload` sits behind a delay — and it
   ;; ships the list once for every app that uses it, the way :jolt/native
   ;; ships a shared library.
   :allow-dynamic (vec (mapcat (fn [{:keys [edn]}] (decl/allow-dynamic-entries edn)) infos))
   ;; Each dep's declared jolt floor, as [lib version] — checked by
   ;; resolve-project against the running runtime. A LIBRARY is the common
   ;; declarer: it knows which jolt its FFI bindings or host shims need, and
   ;; the app that pulls it in does not.
   :min-versions (vec (keep (fn [{:keys [lib edn]}]
                              (when-let [v (:jolt/min-version edn)] [lib v]))
                            infos))
   ;; Host classes each dep declares it provides (RFC 0014), as
   ;; [install-ns lib class-name …]. A library knows which java.* classes
   ;; its shim installs; the runtime must not, or core ends up naming
   ;; specific libraries to decide what to autoload on a class miss.
   :provides (vec (mapcat (fn [{:keys [lib edn]}] (decl/provides-entries edn lib)) infos))
   ;; libs whose deps.edn declares :deps/prep-lib — jolt runs no prep
   ;; steps, so their compiled/generated assets will be missing; the
   ;; caller warns with the lib names.
   :prep (vec (keep (fn [{:keys [lib edn]}] (when (:deps/prep-lib edn) lib)) infos))
   ;; The roots that are Maven RELEASE artifacts: a released version is
   ;; never republished under the same path, so the loader may key its
   ;; entries by the jar's stat instead of reading them on every start. A
   ;; SNAPSHOT is republished in place and is left out.
   :immutable-roots (vec (keep #(release-root libmap %) infos))
   :libs libmap
   :trace (:trace expansion)
   ;; The edges -Sgraph renders. Their labels come from :libs above, which
   ;; is the mediated map — a library reached through two parents is
   ;; labelled with the version that won.
   :graph graph})

(defn resolve-deps
  "Expand a deps map through the tools.deps expansion engine, then collect the
  selected libraries' source roots, :jolt/native and :jolt/provides declarations
  in stable first-inclusion order. Returns {:roots [...] :natives [...]
  :allow-dynamic [...] :min-versions [...] :provides [...] :prep [...]
  :libs {lib coord}} — :libs is the tools.deps lib map (selected coordinate per
  library).

  `opts` carries the alias-combined coordinate maps applied at every node like
  tools.deps: :override-deps replaces a lib's coordinate wherever it appears
  (top-level or transitive); :default-deps supplies one where a dependent left
  the coordinate nil. With :trace? the result also carries the expansion
  :trace — {:log [node …] :vmap version-map}, the tools.deps trace shape, which
  dep-tree-lines renders and trace-edn-string writes out. With :jvm-deps?, a
  Maven jar with no Clojure source has its POM deps walked like any other, for a
  caller that runs JVM bytecode itself. A nested call that doesn't pass the key
  inherits the outer value."
  ([deps base-dir] (resolve-deps deps base-dir nil))
  ([deps base-dir {:keys [override-deps default-deps trace? graph? jvm-deps?] :as opts}]
   ;; A nested resolve-deps inherits the outer atom, so the summary is printed
   ;; ONCE by whichever frame created it rather than once per level.
   (let [outermost? (nil? *unresolvable*)]
   (binding [*procure-memo* (or *procure-memo* (atom {}))
             *unresolvable* (or *unresolvable* (atom []))
             *jvm-deps* (if (contains? opts :jvm-deps?) (boolean jvm-deps?) *jvm-deps*)]
     (let [top (filter-deps deps base-dir)
           override-deps (absolutize-coords override-deps base-dir)
           default-deps (absolutize-coords default-deps base-dir)
           expansion (expand top override-deps default-deps trace?)
           libmap (:libs expansion)
           ;; The SELECTED-edge graph -Sgraph renders: parent -> child over the
           ;; coordinates that won, which is a different question from :trace,
           ;; whose log carries every decision including the losing ones.
           ;; coord-deps is the same procurement the expansion just did and
           ;; shares its *procure-memo*, so walking it again re-fetches nothing.
           graph (when graph?
                   ((requiring-resolve 'grenadine.tree/dependency-graph)
                    top libmap ext/coord-deps))
           infos (selected-infos libmap (:order expansion))]
       (when outermost? (fail-unresolvable!))
       (resolution infos libmap expansion graph))))))

;; --- resolved-roots cache (.jolt/cpcache) -----------------------------------
;; tools.deps calls this .cpcache: the resolved classpath keyed on a content
;; hash of the inputs, so a warm run skips dep-graph expansion (POM parsing,
;; version selection, gitlib probing) entirely. The key folds in everything the
;; expansion reads, so changing any of it misses automatically:
;;   - the project deps.edn bytes
;;   - the user deps.edn bytes, or an explicit "absent/skipped" marker
;;     (JOLT_NO_USER_DEPS / :repro?)
;;   - the active alias set (-M:alias must not share a no-alias entry)
;;   - the runtime version (the same source the AOT cache keys on)
;;   - every :local/root dep's deps.edn bytes (editing a local dep invalidates)
;; Gated the same way the AOT namespace cache is: JOLT_AOT_CACHE. The dev
;; bin/jolt exports JOLT_AOT_CACHE=0, so source-mode dev never caches — a
;; volatile compiler re-expands every run, same posture as a fasl cache would
;; take. No gate (e.g. a non-binary running with the cache explicitly off) means
;; we cannot safely tell one runtime from another, so the cache stays off.
;;
;; The cache sits AFTER fetch/resolution: a miss runs the full resolve-deps
;; (which fetches missing deps over the network) and then writes; a hit never
;; touches the network. On a hit every cached path is checked to still exist on
;; disk — a pruned gitlib checkout or a deleted jar is a miss, not an
;; error. A corrupt or unreadable cache file is a miss too, never an error.

;; --- :jolt/min-version ------------------------------------------------------
;; A project or a library declares the oldest jolt it works on, and a runtime
;; below that refuses to load it rather than run it. Not every breaking change is
;; visible at the call site — ffi/write's argument order moved in 0.8.0, and the
;; old and new spellings are both integers, so an older runtime writes to the
;; wrong place and reports nothing — and a declared floor turns the NEXT break of
;; that shape into a message.
;;
;; Honoured by the jolt that READS the key, so it protects from 0.8.0 onward and
;; not before: an older jolt ignores it, as it ignores every key it does not
;; know. Note what that excludes — this key landed in the commit AFTER the one
;; that moved ffi/write, so no floor catches that particular break: every runtime
;; with the old argument order skips the key. Pinning the toolchain is the answer
;; there. The floor is for the breaks that come after a reader exists on both
;; sides, which is the reason to add it now rather than at the next one.
(defn- version-parts
  "The leading numeric components of a version string, as a vector of longs.
  Tolerates a `v` prefix and reads each component's numeric PREFIX, stopping at
  the first component that has none — so \"v0.7.29-24-gabc\" is [0 7 29], the git
  describe suffix riding along on the last component, and \"0.8\" is [0 8]. A
  string with no numeric components at all (a source build's \"dev\") is []."
  [v]
  (let [v (str v)
        v (if (and (pos? (count v)) (= "v" (subs v 0 1))) (subs v 1) v)]
    (loop [parts (str/split v #"\.") acc []]
      (if (empty? parts)
        acc
        (let [digits (re-find #"^[0-9]+" (first parts))]
          (if digits
            (recur (rest parts) (conj acc (parse-long digits)))
            acc))))))

(defn- version<
  "True when version a orders before version b on their numeric components, a
  missing component reading as 0 ([0 8] and [0 8 0] are the same version)."
  [a b]
  (let [x (version-parts a) y (version-parts b)
        n (max (count x) (count y))
        at (fn [v i] (or (get v i) 0))]
    (loop [i 0]
      (cond
        (= i n) false
        (< (at x i) (at y i)) true
        (> (at x i) (at y i)) false
        :else (recur (inc i))))))

;; The decision, pure and separate from the throw: a source build fails no floor,
;; so this is the only way the refusal is testable at all.
(defn- min-version-failure
  "nil when every declared floor is met, else [message ex-data] for the HIGHEST
  unmet one — one message naming the newest thing needed, not one per dependency.
  `declared` is a seq of [who version], `who` being a lib symbol or nil for the
  project itself."
  [running declared]
  ;; A runtime naming no version — a source build answers "dev" — is never
  ;; refused: it reads as 0.0.0, the oldest possible, while in practice it is the
  ;; newest. The floor protects a released runtime, and those always name one.
  (when (seq (version-parts running))
    (let [unmet (filter (fn [[_ v]] (version< running v)) declared)]
      (when (seq unmet)
        ;; max-key would compare version VECTORS as numbers and throw the moment
        ;; two floors were unmet — and never compare at all with one, so the
        ;; crash hid behind every single-dependency project.
        (let [[who wanted] (reduce (fn [a b] (if (version< (second a) (second b)) b a))
                                   unmet)]
          [(str "this project needs jolt " wanted " or newer, and this is "
                running
                (when who (str " (required by " who ")"))
                ". Upgrade jolt, or set JOLT_SKIP_MIN_VERSION=1 to run anyway.")
           {:jolt/running running
            :jolt/required wanted
            :jolt/required-by who
            :jolt/unmet (vec unmet)}])))))

(defn- check-min-versions!
  "Refuse to run when the project, or any dependency, declares a jolt floor the
  running runtime is below. JOLT_SKIP_MIN_VERSION runs anyway, for a runtime
  whose version string understates what it actually carries."
  [declared]
  (when-not (getenv "JOLT_SKIP_MIN_VERSION")
    (when-let [[msg data] (min-version-failure (jolt.host/jolt-version) declared)]
      (throw (ex-info msg data)))))

(defn- cpcache-enabled?
  "Same posture as the AOT namespace cache: ON by default, OFF under
  JOLT_AOT_CACHE=0/false/no/off (which the dev bin/jolt sets). Reachable from
  Clojure because the host already reads it, so no new host seam is needed."
  []
  (let [e (getenv "JOLT_AOT_CACHE")]
    (if (and (string? e) (pos? (count e)))
      (not (#{"0" "false" "no" "off"} (str/lower-case e)))
      true)))

(defn- slurp-quiet
  "slurp that returns nil for a missing file instead of throwing, for the cache
  key (a deleted local deps.edn folds in as empty, which still moves the key)."
  [p]
  (when (file-exists? p) (slurp p)))

(defn- cpcache-file-key
  "Filename-safe key for the cache entry: jolt's own hash of the material. The
  hash only NAMES the file — a collision cannot serve a wrong entry, because
  cpcache-hit? requires the stored material to equal the current one exactly.
  That equality is also what makes the key content-exact (a same-length edit
  moves it), the trap a sampled or truncated hash walks into. No subprocess, no
  temp file: an earlier draft shelled to shasum, which minimal Linux images may
  not carry and which costs a spawn on the path this cache exists to shave."
  [material]
  (str "k" (bit-and (hash material) 0x7fffffff)))

(defn- local-deps-edn-paths
  "The deps.edn files of every :local/root coordinate reachable from `deps`
  (absolutized against base-dir), transitively. Each local dep's own :local/root
  deps are followed so a whole local subgraph folds into the cache key; a jar or
  missing deps.edn contributes nothing. The walk is bounded by the set of dirs
  visited, so a local cycle terminates."
  [deps base-dir]
  (letfn [(locals-in [m dir]
            (when-let [ds (:deps m)]
              (keep (fn [[_ spec]]
                      (when-let [r (:local/root spec)]
                        (abspath dir r)))
                    ds)))
          (walk [queue seen acc]
            (lazy-seq
              (if-let [dir (first queue)]
                (if (contains? seen (canonical-path dir))
                  (walk (rest queue) seen acc)
                  (let [seen (conj seen (canonical-path dir))
                        dep-edn (str dir "/deps.edn")
                        ;; a local dep without a deps.edn (bare source dir, a jar)
                        ;; contributes nothing to the key — its roots are its own.
                        m (read-edn dep-edn)
                        acc (if m (conj acc dep-edn) acc)
                        next (or (locals-in m dir) [])]
                    (walk (concat (rest queue) next) seen acc)))
                acc)))]
    (vec (walk (locals-in {:deps deps} base-dir) #{} []))))

(defn- cpcache-material
  "Everything the expansion reads, as one string — the cache entry stores this
  verbatim and a hit requires exact equality against it. project-edn-bytes is
  the project deps.edn as a string (already read by the caller); user-edn-bytes
  is the user deps.edn as a string, or a fixed marker when it is skipped
  (JOLT_NO_USER_DEPS / :repro?) or absent. alias-kws is the active alias set.
  Each :local/root dep's deps.edn CONTENT is folded in — its length alone once
  was, and a same-length edit (one version digit for another) then kept serving
  the stale entry.

  `deps` is the EFFECTIVE dep map the expansion is about to run on, with the
  coordinate adjustments (:override-deps / :default-deps) that go with it. The
  files above do not determine it on their own: -Sdeps merges a map that is in
  no file, and a bb.edn contributes :deps for a task run and not otherwise. Key
  on the input to the expansion, not only on the files it was read from."
  [project-edn-bytes user-edn-bytes alias-kws local-dep-edns runtime-version deps opts]
  (str (count project-edn-bytes) ":" project-edn-bytes
       "|" (count user-edn-bytes) ":" user-edn-bytes
       "|" (pr-str (vec (sort alias-kws)))
       "|" (pr-str (vec (sort-by (comp str key) deps)))
       "|" (pr-str (vec (sort-by (comp str key) (:override-deps opts))))
       "|" (pr-str (vec (sort-by (comp str key) (:default-deps opts))))
       ;; :jvm-deps? walks source-less jars, so it changes the result. It is
       ;; resolved the way resolve-deps binds it, and only written when on, so
       ;; every entry cached without it keeps the key it already has.
       (when (if (contains? opts :jvm-deps?) (:jvm-deps? opts) *jvm-deps*)
         "|jvm-deps")
       "|" runtime-version
       ;; the environment-dependent artifact roots resolution materializes into:
       ;; a run pointed at a different gitlibs (JOLT_GITLIBS_DIR — the
       ;; deps-alias smoke's retry scenarios do exactly this), a different
       ;; HOME (the ~/.m2 and ~/.jolt defaults), or a moved Maven repo
       ;; (JOLT_MVNLIBS / JOLT_MAVEN_REPOSITORY /
       ;; GRENADINE_MAVEN_REPOSITORY) must
       ;; not share an entry whose cached roots point into the old location —
       ;; the old paths usually still exist, so validation alone won't miss.
       "|" (git/gitlibs-dir)
       "|" (or (getenv "JOLT_MVNLIBS") "")
       "|" (or (getenv "JOLT_MAVEN_REPOSITORY") "")
       "|" (or (getenv "GRENADINE_MAVEN_REPOSITORY") "")
       "|" (or (getenv "HOME") "")
       "|" (str/join "|" (for [p (sort local-dep-edns)]
                           (let [b (or (slurp-quiet p) "")]
                             (str p "=" (count b) ":" b))))))
(defn- cpcache-dir
  "The project-local cache dir, under the project's .jolt/ state dir."
  [project-dir]
  (str project-dir "/.jolt/cpcache"))

(defn- cached-paths
  "Every filesystem path a cached resolution depends on at run time — the roots
  that were resolved, plus the source-tree roots a :local/root dep (or the
  project itself) declared. A missing one means the cache is stale: a pruned
  gitlib checkout, a deleted jar, or a removed local dep dir."
  [resolved]
  ;; roots only: :natives entries are the :jolt/native DESCRIPTOR maps from
  ;; dep deps.edn files, not paths — validating them as paths threw off the
  ;; hit path and took Selmer's whole suite down (the fleet gate caught it).
  ;; String-filtered so a future shape change in the result can never turn
  ;; validation into a crash again.
  (let [roots (:roots resolved)
        project-roots (:project-roots resolved)]
    (vec (filter string? (distinct (concat roots project-roots))))))

(defn- cpcache-hit?
  "Read and validate the cache file named by `k`: returns the cached value when
  the file parses, its stored :material EQUALS `material` (the hash in the
  filename only names the entry — equality is what rules a collision out), and
  every cached path still exists. A corrupt/unreadable file is a miss, never an
  error."
  [project-dir k material]
  (let [f (str (cpcache-dir project-dir) "/" k ".edn")]
    ;; ONE guard around parse AND validation: the corrupt-is-a-miss contract
    ;; has to hold for anything a cache file can make this code do, not just
    ;; for read-string — an unexpected shape in a cached value threw from the
    ;; path check and turned every run in the project into the error.
    (try
      (when (file-exists? f)
        (when-let [cached (edn/read-string (slurp f))]
          (when (= (:material cached) material)
            (if (every? file-exists? (cached-paths (:value cached)))
              (:value cached)
              (do (info "cpcache miss (a cached path no longer exists)")
                  (rm-f f)                         ; stale — don't re-strike
                  nil)))))
      (catch :default _ nil))))

(defn- cpcache-write!
  "Write `resolved` to the cache atomically (temp + rename), so a reader never
  sees a half-written file. A write failure (read-only dir, full disk) is a
  quiet miss: the cache is best-effort, the resolution already succeeded."
  [project-dir k material resolved]
  (let [dir (cpcache-dir project-dir)
        f (str dir "/" k ".edn")
        stage (str f ".part-" (System/currentTimeMillis) "-" (rand-int 100000))]
    (mkdirs! dir)
    (try
      (spit stage (pr-str {:material material :value resolved}))
      ;; rename within one directory is atomic and never copies.
      (when-not (mv! stage f)
        (rm-f stage))
      (catch :default _
         (rm-f stage)))))

(defn- resolution-empty?
  "True when a resolution found nothing at all — no roots, no libs, no natives,
  nothing to prep. Worth its own predicate because that is the common case for
  `jolt -e` outside a project, and it is the one case the cache must NOT write.

  Two reasons. The entry would buy nothing: what it stores is the empty map, and
  recomputing that is the 2ms `bench/startup-phases.sh` attributes to dispatch,
  with no graph to expand and no network to touch. And writing it has a cost that
  is not jolt's to impose — cpcache-write! mkdirs its directory, so a one-off
  `jolt -e` left a .jolt/ behind in whatever directory it was run from, project
  or not. A real project resolves something and still caches."
  [r]
  (and (empty? (:roots r))
       (empty? (:libs r))
       (empty? (:natives r))
       (empty? (:provides r))
       (empty? (:prep r))
       (empty? (:min-versions r))))

(declare user-deps-path)   ; defined below with the deps.edn readers

(defn- resolve-deps-cached
  "resolve-deps wrapped in the .jolt/cpcache cache. On a hit the cached result
  (the resolve-deps return map) is returned without expanding the graph or
  touching the network; on a miss the full expansion runs (which fetches missing
  deps) and the result is written. The cache is OFF when JOLT_AOT_CACHE is off
  (the dev bin/jolt posture) or when trace? is set — a trace is a one-off query
  (-Stree/-Strace), not the resolution a run reuses, so it never caches and a
  cached run never serves one. graph? is off for the same reason: a cached
  basis carries no :graph, and -Sgraph would silently print nothing. Emits the JOLT_DEBUG-gated hit/miss lines."
  [project-dir deps alias-kws repro? opts]
  (if (and (cpcache-enabled?) (not (:trace? opts)) (not (:graph? opts)))
    (let [proj-bytes (or (slurp-quiet (str project-dir "/deps.edn")) "")
          skip-user? (or repro? (getenv "JOLT_NO_USER_DEPS"))
          user-path (user-deps-path)
          user-bytes (if skip-user?
                        "::no-user-deps::"
                        (or (slurp-quiet user-path) "::absent::"))
          runtime-version (jolt.host/jolt-version)
          local-edns (local-deps-edn-paths deps project-dir)
          material (cpcache-material proj-bytes user-bytes alias-kws local-edns runtime-version
                                     deps opts)
          k (cpcache-file-key material)]
      (if-let [cached (cpcache-hit? project-dir k material)]
        (do (info "cpcache hit") cached)
        (let [r (resolve-deps deps project-dir opts)]
          (info "cpcache miss")
          (if (resolution-empty? r)
            (info "cpcache not written (nothing resolved)")
            (cpcache-write! project-dir k material r))
          r)))
    (resolve-deps deps project-dir opts)))

;; --- dependency tree --------------------------------------------------------
;; The trace is a flat log of expansion decisions in traversal order; the tree is
;; that log re-hung on the paths the nodes were reached by. Rendering follows
;; clojure.tools.deps.tree so `jolt -Stree` reads like `clojure -Stree`.

(defn- supersede
  "A :newer-version decision retroactively unseats the nodes for that lib that
  were included earlier in the walk — they were selected, then lost when the
  newer coordinate turned up. tools.deps marks them :superseded as it goes."
  [log]
  (reduce (fn [acc {:keys [lib reason] :as node}]
            (conj (if (= :newer-version reason)
                    (mapv (fn [n] (if (and (= lib (:lib n)) (:include n))
                                    (assoc n :include false :reason :superseded)
                                    n))
                          acc)
                    acc)
                  node))
          [] log))

(defn- tree-line [{:keys [lib coord include reason]} indent]
  (let [pre (apply str (repeat indent " "))
        summary (ext/coord-summary lib coord)]
    (case reason
      :new-top-dep (str pre summary)
      (:new-dep :same-version) (str pre ". " summary)
      :newer-version (str pre ". " summary " " reason)
      (:use-top :older-version :excluded :parent-omitted :superseded)
      (str pre "X " summary " " reason)
      (str pre "? " summary " " include " " reason))))

(defn dep-tree-lines
  "Render an expansion trace (resolve-project with {:trace? true}) as the lines
  of a dependency tree, in the tools.deps format: a top-level dep unprefixed, a
  child indented under the dep that pulled it in and marked `.` when it made the
  resolution or `X <reason>` when it lost.

    local/app ../app
      . local/lib 1.2.3
        X local/other 1.0.0 :older-version"
  [trace]
  (let [log (supersede (:log trace))
        by-parent (reduce (fn [m {:keys [path] :as node}]
                            (update m (vec path) (fnil conj []) node))
                          {} log)]
    (letfn [(walk [path depth]
              (mapcat (fn [{:keys [lib] :as node}]
                        (cons (tree-line node depth)
                              (walk (conj path lib) (+ depth 2))))
                      (get by-parent path)))]
      (vec (walk [] 0)))))

(defn- graph-label
  "How -Sgraph names a node: the same one-line coordinate summary -Stree
  prints, so the two renderings never disagree about what a version is."
  [libs lib]
  (if-let [coord (get libs lib)]
    (ext/coord-summary lib coord)
    (str lib)))

(defn dep-graph-lines
  "Render the SELECTED dependency graph as an indented tree, the shape
  grenadine's --expand prints:

    ├── babashka/process 0.6.25
    │   └── babashka/fs 0.5.34 (already shown)
    └── rewrite-clj/rewrite-clj 1.1.49

  A library is expanded once — a later parent shows it `(already shown)` rather
  than repeating the subtree — and a coordinate that reaches itself is marked
  `(cycle)` instead of recurring forever. This answers \"what does the program
  depend on\", where -Stree answers \"how did the resolution get here\", so the
  losing candidates -Stree marks `X` are simply absent.

  `updates` is an optional lib -> newer-version map; each entry appends
  ` -> VERSION` to its line (-Soutdated supplies it, -Sgraph does not)."
  ([resolved] (dep-graph-lines resolved nil))
  ([{:keys [graph libs]} updates]
   (if (nil? graph)
     []
     ((requiring-resolve 'grenadine.tree/lines)
      graph
      (fn [lib]
        (str (graph-label libs lib)
             (when-let [v (get updates lib)] (str " -> " v))))))))

;; The remote half of -Soutdated. Only a :mvn coordinate has a version to
;; compare — a git dep is pinned to a sha and a :local/root to a directory, so
;; neither has a \"newer\" to report and both are simply printed as they are.
;;
;; A lookup that fails leaves the library unannotated rather than failing the
;; command: -Soutdated over a dependency set that includes one unreachable
;; repository should still report the rest, the way `grenadine --current` does.
;; The warning goes to stderr so the tree itself still pipes.
(defn dep-updates
  "For each selected Maven library, the newest release available from the
  configured repositories when it is newer than the selected version. Returns a
  lib -> version string map; libraries with no update are absent."
  [{:keys [graph libs mvn-repos]}]
  (let [visible? (requiring-resolve 'grenadine.tree/visible?)
        latest-version (requiring-resolve 'grenadine.repo/latest-version)
        visible (when graph
                  (filter visible?
                          (distinct (concat (:roots graph)
                                            (mapcat val (:children graph))))))]
    (reduce
     (fn [acc lib]
       (let [coord (get libs lib)
             current (:mvn/version coord)]
         (if-not current
           acc
           ;; latest-version answers the version STRING, and THROWS when no
           ;; release is found — it is not a nil-returning lookup.
           (let [latest (try (latest-version {:group (mvn-group lib)
                                              :artifact (name lib)}
                                             {:host (mvn-http-host)
                                              :repos mvn-repos})
                             (catch :default e
                               (warn "cannot determine the latest version of " lib
                                     ": " (ex-message e))
                               nil))]
             (if (and latest (grenadine.version/newer? latest current))
               (assoc acc lib latest)
               acc)))))
     {} visible)))

(defn trace-edn-string
  "The trace as the edn text of `jolt -Strace`'s trace.edn: {:log [node …]
  :vmap {…}}, one expansion decision per line so the file can be read by eye as
  well as by `read-string`."
  [{:keys [log vmap]}]
  ;; long-hand keys, not the #:git{…} shorthand — tools.deps writes its trace
  ;; the same way, and every edn reader takes the long form.
  (binding [*print-namespace-maps* false]
    (str "{:log\n [" (str/join "\n  " (map pr-str log)) "]\n"
         " :vmap " (pr-str vmap) "}\n")))

;; --- public -----------------------------------------------------------------
(defn- user-deps-path
  "The user deps.edn location, per the same rules as clj: $CLJ_CONFIG, else
  $XDG_CONFIG_HOME/clojure, else ~/.clojure."
  []
  (let [cfg (getenv "CLJ_CONFIG")
        xdg (getenv "XDG_CONFIG_HOME")
        home (getenv "HOME")]
    (cond cfg (str cfg "/deps.edn")
          xdg (str xdg "/clojure/deps.edn")
          :else (str (or home ".") "/.clojure/deps.edn"))))

(defn- read-deps-file
  "Read + canonicalize a deps.edn file (simple lib symbols qualify with a
  deprecation warning, like tools.deps); nil when absent."
  [path]
  (some-> (read-edn path) dedn/canonicalize))

;; bb.edn — babashka's project file, read for the same :paths / :deps / :tasks
;; keys deps.edn carries. Two shapes, and which one a project has decides how
;; far bb.edn reaches:
;;
;;   bb.edn alone   — it IS the project config, standing in for deps.edn
;;                    everywhere (a babashka project has no other file to
;;                    resolve from).
;;   both files     — deps.edn is the project: it alone answers `path`, `run`,
;;                    `build`, the REPL. bb.edn contributes its :tasks, and its
;;                    :paths/:deps join the resolution only for a task run
;;                    (:tasks? below), where babashka's environment is what the
;;                    task was written against. Merging them everywhere would
;;                    let a bb.edn :paths ["script"] displace the app's own
;;                    source roots on every command.
;;
;; :min-bb-version is ignored (jolt is not babashka, and a version comparison
;; against it would answer nothing useful); :pods are not supported and say so,
;; since a task that shells to a pod otherwise fails somewhere far from here.
(defn- read-bb-file [path]
  (let [m (read-deps-file path)]
    (when (seq (:pods m))
      (warn path " declares :pods, which jolt does not run: "
            (pr-str (vec (keys (:pods m))))))
    m))

(defn- read-project-edns
  "The project files resolve-project reads, merged: {:edn the merged deps.edn
  chain, :bb-edn the project's bb.edn, :bb-extra the bb.edn whose paths/deps
  join this resolution (a task run beside a deps.edn), else nil}."
  [project-dir extra-edn repro? tasks?]
  (let [deps-edn (read-deps-file (str project-dir "/deps.edn"))
        bb-edn (read-bb-file (str project-dir "/bb.edn"))
        ;; with no deps.edn, bb.edn stands in as the project config
        project-edn (or deps-edn bb-edn)
        ;; …and when there is one, bb.edn's own paths/deps are additional, and
        ;; only for a task run. Its paths go LAST (after the project's own), so
        ;; a script root can never shadow the app's namespaces.
        bb-extra (when (and tasks? deps-edn bb-edn) bb-edn)
        user-edn (when-not (or repro? (getenv "JOLT_NO_USER_DEPS"))
                   (try (read-deps-file (user-deps-path))
                        (catch :default e (warn (ex-message e)) nil)))]
    {:edn (dedn/merge-edns [user-edn project-edn (some-> extra-edn dedn/canonicalize)])
     :bb-edn bb-edn
     :bb-extra bb-extra}))

;; An alias the merged chain doesn't declare is skipped with a warning,
;; the tools.deps behavior verbatim ("WARNING: Specified aliases are
;; undeclared and are not being used"): loud enough that a typo'd alias
;; doesn't run the program without its deps and fail somewhere far
;; away, but not fatal — an editor sends a fixed alias set (Calva asks
;; for `-A:test:dev -Spath`) and a project that declares only some of
;; them still has a classpath, and a program, to give it.
(defn- warn-undeclared-aliases! [edn alias-kws]
  (let [missing (distinct (remove #(get-in edn [:aliases %]) alias-kws))]
    (when (seq missing)
      (warn "Specified aliases are undeclared and are not being used: "
            (pr-str (vec missing))))))

;; Path order matches the clj CLI: the selected aliases' :extra-paths
;; come FIRST (in alias-selection order), then the project's :paths —
;; or an alias's :replace-paths, which :extra-paths still precedes.
;; `clojure -A:t -Spath` prints `test src`, not `src test`, and the
;; order is load-bearing: the loader takes the first root that has the
;; namespace, so a -A:dev extra path shadows the project's own copy
;; rather than being shadowed by it.
;; tool mode (-T): the project's own paths and deps are replaced —
;; the tool's alias supplies its own (clj CLI tool-basis defaults
;; :replace-paths ["."] :replace-deps {}).
(defn- project-paths-for [edn argmap bb-extra tool?]
  (concat (:extra-paths argmap)
          (or (:replace-paths argmap) (:paths argmap)
              (if tool? ["."] (or (:paths edn) ["src"])))
          ;; last: a task run's bb.edn paths (see read-bb-file)
          (:paths bb-extra)))

(defn- project-deps-for [edn argmap bb-extra tool?]
  (merge (or (:replace-deps argmap) (:deps argmap)
             (if tool? {} (:deps edn)))
         (:deps bb-extra)
         (:extra-deps argmap)))

(defn- check-resolved-project!
  "The checks that need the resolution in hand: every :jolt/min-version floor,
  and a warning for deps whose :deps/prep-lib steps jolt does not run."
  [edn dep-min-versions prep-libs]
  ;; The floor is checked AFTER resolution, so a dep's own declaration is
  ;; in hand — a library is the natural declarer, since it knows which
  ;; jolt its bindings need and the app pulling it in does not. The
  ;; project's own declaration is checked here too, and is what an app
  ;; uses to pin the runtime it was written against.
  (check-min-versions!
   (concat (when-let [v (:jolt/min-version edn)] [[nil v]])
           dep-min-versions))
  (when (seq prep-libs)
    (warn "deps declare :deps/prep-lib steps jolt does not run "
          "(their compiled/generated assets will be missing): "
          (str/join ", " prep-libs))))

(defn- project-resolution
  "resolve-project's answer, from the project's own config (PROJECT) and its
  dependency resolution (DEPS, resolve-deps' answer; nil under :cp)."
  [project-dir cp
   {:keys [edn bb-edn argmap project-paths project-roots]}
   {dep-roots :roots dep-natives :natives dep-provides :provides
    dep-immutable-roots :immutable-roots dep-trace :trace dep-graph :graph
    dep-libs :libs dep-allow-dynamic :allow-dynamic}]
  ;; reconcile: the project's own roots/natives + every dep's, deduped once.
  ;; With :cp the given roots ARE the answer — they replace the project's own
  ;; paths too, like the clj CLI, where -Scp is the whole classpath.
  {:roots (dedup-by identity (or cp (concat project-roots dep-roots)))
   ;; with :cp nothing was resolved, so nothing is known to be a release
   :immutable-roots (if cp [] (vec dep-immutable-roots))
   :main-opts (:main-opts argmap)
   ;; the combined alias args map — the CLI's -X/-T read :exec-fn /
   ;; :exec-args / :ns-default / :ns-aliases from it.
   :argmap argmap
   ;; the project's own paths (relative to project-dir) and absolute resource
   ;; roots, plus its :jolt/build options — `jolt build` uses these to bundle
   ;; resources into / alongside a standalone binary.
   :project-dir project-dir
   :project-paths (vec project-paths)
   :project-roots (vec project-roots)
   :build (:jolt/build edn)
   :embed-dirs (mapv #(abspath project-dir %) (:embed (:jolt/build edn)))
   ;; :tasks from both files, bb.edn last — a name in both is babashka's.
   ;; (When bb.edn IS the project config the second merge is a no-op.)
   :tasks (not-empty (merge (:tasks edn) (:tasks bb-edn)))
   ;; the project's own specs are rooted at the project, the deps' at their own
   ;; deps.edn's directory (resolve-deps attached those). Reconciled by
   ;; native-key, which does not read the root — two deps naming the same lib
   ;; still reconcile to one load, keeping the first one's root, and each
   ;; platform key the first is silent about is filled in from the others
   ;; along with the root it was declared under.
   :natives (decl/reconcile-natives
              (concat (map #(assoc % :jolt.deps/root project-dir)
                           (:jolt/native edn))
                      dep-natives))
   ;; the project's own vouched-for defs first, then every dep's, deduped —
   ;; `jolt build --tree-shake` hands the union to the bail scan. The
   ;; project's list is where an app allows a site in a library that has
   ;; not shipped its own list yet.
   :allow-dynamic (dedup-by identity
                            (concat (decl/allow-dynamic-entries edn) dep-allow-dynamic))
   ;; declared host-class providers (RFC 0014), the project's own first: a
   ;; project may supply a class itself rather than take a library's.
   :provides (decl/host-class-providers (concat (decl/provides-entries edn nil) dep-provides))
   ;; :jolt/replaces — namespaces jolt provides as a host built-in
   ;; (babashka.fs, babashka.process) that THIS project supplies itself, so
   ;; its copy resolves ahead of jolt's.
   ;; The PROJECT's only: a library that took a built-in over would decide
   ;; what the namespace means for every other library in the program, which
   ;; is what host-class-providers already refuses for classes.
   :replaces (mapv str (:jolt/replaces edn))
   ;; :jolt/features — extra reader-conditional keys this project wants read,
   ;; on top of jolt's own {:jolt :clj :default}. Additive: it cannot remove
   ;; one. The PROJECT's only, for the same reason :jolt/replaces is — the
   ;; feature set decides which branch EVERY library in the program is read
   ;; through, so a dependency must not get to change it under the project.
   :features (mapv #(if (keyword? %) (name %) (str %)) (:jolt/features edn))
   ;; the expansion trace, when it was asked for (-Stree renders it)
   :trace dep-trace
   ;; the selected-edge graph and its coordinates (-Sgraph renders these),
   ;; with the repositories the resolution used — -Soutdated asks those for
   ;; a newer version, and *mvn-repos* is bound only around the expansion.
   :graph dep-graph
   :libs dep-libs
   :mvn-repos (mvn-repo-urls edn)
   ;; nREPL middleware a library contributes (jolt.nrepl composes them over its
   ;; built-in handler) — symbols resolving to a middleware fn or a vector of them.
   :nrepl-middleware (:nrepl/middleware edn)})

(defn resolve-project
  "Resolve `project-dir`'s deps.edn with the selected alias keywords. Returns
  {:roots [...] :main-opts [...] :tasks {...} :natives [...] :allow-dynamic [...]};
  :natives are the project's + deps' :jolt/native shared-library declarations,
  :allow-dynamic the project's + deps' :jolt/tree-shake {:allow-dynamic […]}
  entries as \"ns/name\" strings.

  :jolt/min-version, declared by the project or by any dependency, is the oldest
  jolt that entry works on; a runtime below it raises here rather than running
  code written for a newer API. A library is the natural declarer — it knows
  which jolt its FFI bindings or host shims need. JOLT_SKIP_MIN_VERSION=1
  overrides, for a dev build off main (which reads as its last tag).

  A bb.edn beside (or instead of) the deps.edn contributes its :tasks always,
  and its :paths/:deps when the :tasks? option is set — see the comment above.

  The deps.edn chain merges like tools.deps (jolt.deps.edn/merge-edns): the
  user deps.edn ($CLJ_CONFIG / $XDG_CONFIG_HOME/clojure / ~/.clojure — skipped
  under JOLT_NO_USER_DEPS or the :repro? option, the CLI's -Srepro), then the
  project deps.edn, then an optional extra
  map (the CLI's -Sdeps). Aliases combine from the merged map with the
  tools.deps rules (combine-aliases) and apply like tools.deps: :replace-deps
  (legacy :deps) replaces the project deps map, :extra-deps merges into it,
  :override-deps / :default-deps adjust coordinates at every node of the walk;
  :replace-paths (legacy :paths) replaces the project paths, :extra-paths
  precede them; :main-opts is last-wins across the selected aliases. Custom
  Maven repositories come from the merged :mvn/repos.

  :roots is ordered like the clj CLI's classpath — the aliases' :extra-paths,
  then the project's :paths, then the dependency roots — and the loader takes
  the first root that has a namespace, so the order decides which copy wins."
  ([project-dir] (resolve-project project-dir []))
  ([project-dir alias-kws] (resolve-project project-dir alias-kws nil))
  ([project-dir alias-kws extra-edn] (resolve-project project-dir alias-kws extra-edn nil))
  ([project-dir alias-kws extra-edn {:keys [tool? repro? trace? graph? cp tasks?]}]
   (let [{:keys [edn bb-extra] :as edns} (read-project-edns project-dir extra-edn repro? tasks?)
         argmap (dedn/combine-aliases edn alias-kws)
         _ (warn-undeclared-aliases! edn alias-kws)
         project-paths (project-paths-for edn argmap bb-extra tool?)
         project-roots (map #(abspath project-dir %) project-paths)
         all-deps (project-deps-for edn argmap bb-extra tool?)
         ;; :cp (the CLI's -Scp) supplies the roots outright, so the dependency
         ;; graph is never expanded and nothing is fetched. The deps.edn chain is
         ;; still read and the aliases still combine — that is only file reads,
         ;; and an alias's :main-opts / :exec-fn and the project's :tasks come
         ;; from there. tools.deps' --skip-cp draws the line in the same place:
         ;; the merged edn and argmap, without calc-basis.
         deps (when-not cp
                (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo edn)]
                                             (abspath project-dir r))
                          *mvn-repos* (mvn-repo-urls edn)]
                  (resolve-deps-cached project-dir all-deps alias-kws repro?
                                       {:override-deps (:override-deps argmap)
                                        :default-deps (:default-deps argmap)
                                        :trace? trace?
                                        :graph? graph?})))]
     (check-resolved-project! edn (:min-versions deps) (:prep deps))
     (project-resolution project-dir cp
                         (assoc edns :argmap argmap :project-paths project-paths
                                     :project-roots project-roots)
                         deps))))

(defn project-tasks
  "The project's :tasks map — deps.edn's and bb.edn's merged, bb.edn last —
  read from the config files alone. Naming what a project can run needs no
  dependency expansion, and a project whose deps don't resolve (offline, a
  typo'd coordinate) should still be able to list its tasks."
  [project-dir]
  (let [user-edn (when-not (getenv "JOLT_NO_USER_DEPS")
                   (try (read-deps-file (user-deps-path)) (catch :default _ nil)))]
    (not-empty (merge (:tasks user-edn)
                      (:tasks (read-deps-file (str project-dir "/deps.edn")))
                      (:tasks (read-edn (str project-dir "/bb.edn")))))))

(defn env-info
  "The files a resolution reads and the caches it fetches into, as data. One
  source of truth for the CLI's two renderings of it: -Sdescribe prints the map,
  -Sverbose the same values as lines before resolving. `repro?` reports whether
  the user deps.edn is being skipped (-Srepro / JOLT_NO_USER_DEPS)."
  ([project-dir] (env-info project-dir false))
  ([project-dir repro?]
   (let [repro? (boolean (or repro? (getenv "JOLT_NO_USER_DEPS")))
         user (user-deps-path)
         project (str project-dir "/deps.edn")
         bb (str project-dir "/bb.edn")]
     {:project-dir project-dir
      :config-user (when-not repro? user)
      :config-project project
      :config-files (vec (concat (when (and (not repro?) (file-exists? user)) [user])
                                 (when (file-exists? project) [project])
                                 (when (file-exists? bb) [bb])))
      :gitlibs-dir (git/gitlibs-dir)
      :mvn-local-repo (m2-repo-dir)
      :repro repro?})))

;; The :jolt/provides rows this process has handed to the runtime. A project's
;; resolved deps install theirs at launch and every add-deps call adds its own,
;; so a class is claimed once however many times its library is added: a second
;; claim on a class the first one's install namespace already registered would be
;; dropped as one "this jolt provides", with a warning about a stale library.
;; Tracked per [install-ns class], not per row, because the runtime judges each
;; class: a re-add under another coordinate, or a newer version declaring one
;; more class, is a different row that must still claim only what is new.
(defonce ^:private installed-provides (atom []))

(defn- fresh-class-providers
  "The :jolt/provides rows in ENTRIES this process hasn't installed yet, checked
  together with the installed ones. Throws, changing nothing, when one would give
  a class a second provider."
  [entries]
  (let [seen @installed-provides
        claimed (set (for [[install-ns _ & classes] seen, c classes] [install-ns c]))
        fresh (vec (keep (fn [[install-ns lib & classes]]
                           (when-let [cs (seq (remove #(claimed [install-ns %]) classes))]
                             (into [install-ns lib] cs)))
                         (distinct entries)))]
    (when (seq fresh)
      (decl/host-class-providers (concat seen fresh)))
    fresh))

(defn- register-class-providers! [fresh]
  (doseq [[install-ns lib & classes] fresh]
    (jolt.host/register-class-provider! install-ns lib (vec classes)))
  (when (seq fresh)
    (swap! installed-provides into fresh))
  nil)

(defn install-class-providers!
  "Hand :jolt/provides rows (RFC 0014), as host-class-providers returns them, to
  the runtime so a reference to a provided class autoloads its install
  namespace. Classes an install namespace already claimed in this process are
  skipped, and the new rows are checked together with the installed ones, so a
  library added later can't claim a class that another one already provides."
  [entries]
  (register-class-providers! (fresh-class-providers entries)))

(defn add-deps
  "Resolve an inline deps map and add the resulting source roots to the loader,
  so a following `require` can load them — the programmatic twin of a deps.edn
  :deps entry, mirroring babashka.deps/add-deps:

    (add-deps '{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"}}})
    (require '[clojure.data.json :as json])

  Coordinates: :git/url + :git/sha (:git/url may be omitted when the lib name
  names a host, e.g. io.github.OWNER/REPO), :local/root (resolved against
  JOLT_PWD), and :mvn/version (JAR source fetched from Clojars, then Central). A top-level
  :mvn/local-repo in the map relocates the Maven repository for this call,
  like the deps.edn key. New roots
  are appended AFTER the current roots, so an added dep can never shadow a
  namespace the runtime already resolves. Returns the vector of roots added
  (empty when everything was already on the roots).

  The added libraries' :jolt/provides declarations are installed, as a
  project's are at launch, so a class one of them provides autoloads its
  install namespace on first reference.

  :jolt/native declarations carried by added deps are NOT auto-loaded (that is
  a project-launch concern — see jolt.main); a warning names them so the
  caller can load via jolt.ffi. The second arity accepts an options map for
  babashka call-shape compatibility; no options are currently honored."
  ([deps-map] (add-deps deps-map nil))
  ([{:keys [deps] :as m} _opts]
   (let [base (or (getenv "JOLT_PWD") ".")
         {:keys [roots natives immutable-roots provides]}
         (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo m)]
                                      (abspath base r))]
           (resolve-deps deps base))
         current (vec (jolt.host/source-roots))
         added (vec (remove (set current) (dedup-by identity roots)))
         ;; checked before anything changes, so a refused claim leaves the
         ;; loader as it was rather than the library's roots added and its
         ;; classes unregistered
         fresh (fresh-class-providers (decl/host-class-providers provides))]
     (jolt.host/add-immutable-roots! immutable-roots)
     (when (seq added)
       (jolt.host/set-source-roots! (into current added)))
     ;; after the roots, so the install namespace a class reference loads is
     ;; already findable
     (register-class-providers! fresh)
     (when (seq natives)
       (info "added deps declare :jolt/native libraries (not auto-loaded): "
             (pr-str (mapv #(dissoc % :jolt.deps/root :jolt.deps/roots
                                    :jolt.deps/lib :jolt.deps/paths)
                           (decl/reconcile-natives natives)))))
     added)))

(defn- required-host
  []
  {:home-dir #(getenv "HOME")
   :gitlibs-dir git/gitlibs-dir
   :file-exists? file-exists?
   :mkdirs! #(do (mkdirs! %) nil)
   :delete! #(do (rm-f %) nil)
   :read-text slurp
   :download! http/fetch
   :atomic-move!
   (fn [from to]
     (when-not (or (mv! from to)
                   (and (rm-f to) (mv! from to)))
       (throw
        (ex-info (str "Unable to move dependency cache file to " to)
                 {:from from :to to})))
     nil)})

(defn- read-first-form
  [source]
  (binding [*read-eval* false]
    (read-string source)))

(defn- loaded-namespace
  [identity]
  (get-in @required-state [:coordinates identity]))

(defn- load-required!
  [coordinate namespace-symbol load!]
  (let [identity (:identity coordinate)
        loaded-coordinate (get-in @required-state
                                  [:namespaces namespace-symbol])]
    (cond
      (= identity (:identity loaded-coordinate)) namespace-symbol
      loaded-coordinate
      (required/namespace-conflict! namespace-symbol loaded-coordinate coordinate)
      :else
      (do
        (load!)
        (swap! required-state
               (fn [state]
                 (-> state
                     (assoc-in [:coordinates identity] namespace-symbol)
                     (assoc-in [:namespaces namespace-symbol] coordinate))))
        namespace-symbol))))

(defn prepare-required!
  "Internal hook used by clojurestar.deps/require-deps."
  [coordinate options]
  (or
   (loaded-namespace (:identity coordinate))
   (case (:provider coordinate)
     :mvn
     (load-required!
      coordinate (:namespace coordinate)
      #(do
         (add-deps
          (cond-> {:deps {(:lib coordinate)
                          {:mvn/version (:version coordinate)}}}
            (:mvn/local-repo options)
            (assoc :mvn/local-repo (:mvn/local-repo options))))
         (require (:namespace coordinate))))

     :gist
     (let [{:keys [path source]}
           (required/acquire-gist! (required-host) options coordinate)
           namespace-symbol
           (required/gist-namespace coordinate (read-first-form source))]
       (load-required! coordinate namespace-symbol
                       #(let [caller (ns-name *ns*)]
                          (try
                            (load-file path)
                            (finally (in-ns caller))))))

     :github
     (let [{:keys [path source]}
           (required/acquire-github! (required-host) options coordinate)
           namespace-symbol
           (required/github-namespace coordinate (read-first-form source))]
       (load-required! coordinate namespace-symbol
                       #(let [caller (ns-name *ns*)]
                          (try
                            (load-file path)
                            (finally (in-ns caller)))))))))
