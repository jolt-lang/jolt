(ns jolt.deps
  "Resolve a deps.edn into an ordered list of source roots. A reduced
  tools.deps: :paths, :deps (`:git/url`+`:git/sha` / `:local/root` /
  `:mvn/version`), :aliases, :tasks. Alias maps combine with the reference
  tools.deps semantics (jolt.deps.edn, lifted from clojure.tools.deps.edn):
  :extra-deps / :override-deps / :default-deps / :replace-deps (legacy :deps),
  :extra-paths / :replace-paths (legacy :paths), :main-opts.

  The deps walk is breadth-first so a top-level coordinate registers before any
  transitive one (a top-level pin wins). Git deps reuse an existing
  tools.gitlibs checkout ($GITLIBS / ~/.gitlibs) when the JVM toolchain already
  fetched them, else clone into a sha-immutable cache ($JOLT_GITLIBS, else
  ~/.jolt/gitlibs, or a jolt/ subdir of $GITLIBS) shared across projects.
  Maven POMs and jars live in the standard local repository
  (~/.m2/repository; :mvn/local-repo in deps.edn relocates it like tools.deps,
  GRENADINE_LOCAL_REPOSITORY supplies an environment default, and
  JOLT_LOCAL_REPO remains as a legacy fallback) shared with the JVM toolchain
  in both directions. Grenadine expands the dependency tree, builds effective
  POMs, and compares Maven versions; files are fetched by jolt itself over
  HTTPS (jolt.mvn-http);
  git and unzip still shell out through jolt.host/sh (nothing here touches the JVM)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [grenadine.expander :as expander]
            [grenadine.pom :as grenadine.pom]
            [grenadine.version :as grenadine.version]
            [jolt.deps.edn :as dedn]
            [jolt.deps.ext :as ext]
            [jolt.mvn-http :as http]))

;; --- small host seams -------------------------------------------------------
;; An env var set to the empty string reads as UNSET — `FOO= cmd` is the usual
;; way to clear a variable for one command, and treating "" as a value would
;; turn that into "set to nothing".
(defn- getenv [n] (let [v (jolt.host/getenv n)] (when-not (str/blank? v) v)))
(defn- file-exists? [p] (jolt.host/file-exists? p))
(defn- sh [cmd] (jolt.host/sh cmd))           ; exit code, inherits stdout/stderr
(defn- sh-out
  "Run a shell command, returning its trimmed stdout, or nil on a non-zero
  exit. Captures through a temp file (jolt.host/sh inherits stdio)."
  [cmd]
  (let [tmp (str "/tmp/jolt-deps-" (System/currentTimeMillis) "-" (rand-int 100000) ".out")
        code (sh (str cmd " > " (pr-str tmp) " 2>/dev/null"))
        out (when (file-exists? tmp) (str/trim (slurp tmp)))]
    (sh (str "rm -f " (pr-str tmp)))
    (when (zero? code) out)))
(defn- warn [& xs] (binding [*out* *err*] (println (str "[jolt.deps] " (apply str xs)))))
;; Progress / informational lines (fetching, using-cache, skipping, added-natives)
;; print only when JOLT_DEBUG is set — otherwise a routine run (e.g. a ys-generated
;; program pulling a native-declaring lib) barfs them on every invocation. Genuine
;; warnings (an unresolvable dep, a malformed deps.edn) always print via `warn`.
(def ^:dynamic *verbose*
  "True while the CLI's -Sverbose is in effect: the progress lines print for this
  resolution without JOLT_DEBUG being set for the whole process."
  false)
(defn- info [& xs] (when (or *verbose* (getenv "JOLT_DEBUG")) (apply warn xs)))

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

(defn- full-sha? [s] (and (string? s) (= 40 (count s)) (re-matches #"[0-9a-f]+" s)))

(defn- git-coord
  "A git coordinate with tools.deps' legacy spellings folded into the namespaced
  keys. deps.edn files in the wild still write :sha and :tag — malli pins
  spec-alpha2 as {:git/url … :sha …} — and tools.deps accepts both, so a
  coordinate that resolves there has to resolve here. The namespaced key wins if
  both are somehow present."
  [coord]
  (cond-> coord
    (and (:sha coord) (not (:git/sha coord))) (assoc :git/sha (:sha coord))
    (and (:tag coord) (not (:git/tag coord))) (assoc :git/tag (:tag coord))))

(defn- resolve-git-tag
  "Resolve a tag via git ls-remote to [commit-sha tag-object-sha]. An annotated
  tag carries both: the peeled ^{} ref is the commit it points at, the unpeeled
  ref is the tag object itself. A deps.edn may pin EITHER — `git ls-remote`
  prints the tag object for refs/tags/X, so that is what a coordinate written
  from its output holds (cognitect-labs/test-runner v0.5.0 is one), and
  tools.deps accepts both. tag-object-sha is nil for a lightweight tag, where
  the two coincide. Cached under the gitlibs dir as one line of two tokens —
  the commit, then the tag object or \"-\" when there is none. A one-token file
  is what an older jolt wrote, which recorded only the commit; it is re-resolved
  rather than trusted, so a coordinate pinning the tag object is not rejected
  off a cache that predates knowing about it."
  [url tag]
  (let [cache (str (gitlibs-dir) "/tags/" (sanitize url) "/" (sanitize tag))]
    (if-let [cached (when (file-exists? cache)
                      (let [[commit obj] (str/split (str/trim (slurp cache)) #"\s+")]
                        (when obj [commit (when (not= obj "-") obj)])))]
      cached
      (when-let [out (sh-out (str "git ls-remote " (pr-str url) " "
                                  (pr-str (str "refs/tags/" tag)) " "
                                  (pr-str (str "refs/tags/" tag "^{}"))))]
        (let [lines (str/split-lines out)
              parse (fn [suffix]
                      (some (fn [l] (let [[sha ref] (str/split l #"\s+")]
                                      (when (and ref (str/ends-with? ref suffix)) sha)))
                            lines))
              peeled (parse "^{}")
              obj (parse (str "refs/tags/" tag))
              commit (or peeled obj)
              obj (when (and peeled obj (not= peeled obj)) obj)]
          (when commit
            (sh (str "mkdir -p " (pr-str (str (gitlibs-dir) "/tags/" (sanitize url)))))
            (spit cache (str commit " " (or obj "-")))
            [commit obj]))))))

(defn- resolve-git-sha
  "The full commit sha a git coordinate pins: a full :git/sha as-is; with a
  :git/tag, a short (prefix) sha is completed from the tag's commit and a full
  sha is verified against it — like tools.deps, a tag alone doesn't pin."
  [coord url {:git/keys [sha tag] :as spec}]
  (cond
    (and sha (full-sha? sha) (not tag)) sha
    (and tag sha)
    (let [[tag-sha obj-sha]
          (or (resolve-git-tag url tag)
              (throw (ex-info (str "git dep " coord ": tag " tag " not found in " url)
                              {:coord coord :spec spec})))]
      ;; Either sha an annotated tag carries pins it, as in tools.deps. What gets
      ;; checked out is always the commit.
      (if (or (str/starts-with? tag-sha sha)
              (and obj-sha (str/starts-with? obj-sha sha)))
        tag-sha
        (throw (ex-info (str "git dep " coord ": :git/sha " sha " does not match tag "
                             tag " (" tag-sha
                             (when obj-sha (str ", tag object " obj-sha)) ")")
                        {:coord coord :spec spec :tag-sha tag-sha
                         :tag-object-sha obj-sha}))))
    (full-sha? sha) sha
    :else
    (throw (ex-info
             (str "git dep " coord " needs :git/sha — the full commit sha, or a "
                  "prefix of it alongside :git/tag"
                  (when (and tag (not sha)) " (a :git/tag alone doesn't pin a commit)") ".")
             {:coord coord :spec spec}))))

(defn- checkout-complete?
  "Does `dir` hold a finished checkout? jolt publishes a checkout by moving a
  fully populated staging dir into place and marks it with `.jolt-git-ok`, so
  anything jolt wrote there is complete. A checkout an older jolt cloned in
  place carries no marker, and its `.git` stands in — which still rejects the
  empty directory an interrupted in-place clone left behind. That leftover is
  why this check exists: git only cleans up a clone directory it created itself,
  the in-place clone pre-created it with mkdir -p, and a plain existence test
  then read the empty remains as a valid checkout. The dep resolved to nothing
  on every later run, and the failure surfaced much later as `Could not locate`
  on one of its namespaces."
  [dir]
  (or (file-exists? (str dir "/.jolt-git-ok"))
      (file-exists? (str dir "/.git"))))

(defn- fetch-git!
  "Clone url@sha under `repo-dir` and return the checkout dir. The work happens
  in a staging dir beside it (same filesystem, so publishing is a rename) and
  any failure removes it: the cache only ever holds finished checkouts. The
  staging name carries a nonce so two jolt processes fetching the same dep at
  once don't clone over each other."
  [url sha repo-dir]
  (let [dir (str repo-dir "/" sha)
        stage (str repo-dir "/.part-" sha "-" (System/currentTimeMillis) "-" (rand-int 100000))
        scrub #(sh (str "rm -rf " (pr-str stage)))
        fail (fn [msg data] (scrub) (throw (ex-info msg (merge {:url url :sha sha} data))))]
    (info "fetching " url " @ " (subs sha 0 (min 12 (count sha))))
    (sh (str "mkdir -p " (pr-str repo-dir)))
    (when-not (zero? (sh (str "git clone --quiet " (pr-str url) " " (pr-str stage))))
      (fail (str "git clone failed: " url) nil))
    (when-not (zero? (sh (str "git -C " (pr-str stage) " checkout --quiet " (pr-str sha))))
      (fail (str "git checkout failed: " sha " in " url) nil))
    ;; submodules are pinned in the checkout; pull them if the dep uses any.
    (when-not (zero? (sh (str "git -C " (pr-str stage) " submodule update --init --recursive --quiet")))
      (fail (str "git submodule update failed for " url) nil))
    (sh (str "touch " (pr-str (str stage "/.jolt-git-ok"))))
    (if (checkout-complete? dir)
      (do (scrub) dir)                    ; another process published it first
      (do
        ;; clears the incomplete remains of an interrupted in-place clone
        (sh (str "rm -rf " (pr-str dir)))
        (when-not (zero? (sh (str "mv " (pr-str stage) " " (pr-str dir))))
          (fail (str "git checkout could not be moved into the cache: " dir) {:dir dir}))
        dir))))

(defn- ensure-git
  "Return a checkout dir for url@sha: jolt's cached checkout when complete, else
  an existing tools.gitlibs checkout for `lib`, else a fresh clone into jolt's
  cache."
  [lib url sha]
  (let [repo-dir (str (gitlibs-dir) "/" (sanitize url))
        dir (str repo-dir "/" sha)]
    (if (checkout-complete? dir)
      dir
      (or (gitlibs-shared-checkout lib sha)
          (fetch-git! url sha repo-dir)))))

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
;; it gets the same behavior for free. GRENADINE_LOCAL_REPOSITORY supplies the
;; shared environment default, with JOLT_LOCAL_REPO retained as a legacy
;; fallback. Setting JOLT_MVNLIBS opts out of sharing entirely: the legacy
;; self-contained layout under it, jar not kept.

(def ^:private ^:dynamic *mvn-local-repo*
  "The :mvn/local-repo of the resolution in progress (bound by resolve-project /
  add-deps from their deps.edn / deps map), nil for the default." nil)

(defn- m2-repo-dir
  "The local Maven repository dir. An explicit :mvn/local-repo wins, followed
  by Grenadine's environment setting, Jolt's legacy override, then the standard
  ~/.m2/repository."
  ([] (m2-repo-dir *mvn-local-repo*
                   (getenv "GRENADINE_LOCAL_REPOSITORY")
                   (getenv "JOLT_LOCAL_REPO")
                   (getenv "HOME")))
  ([cfg grenadine-override jolt-override home]
   (or cfg grenadine-override jolt-override
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
      (sh (str "mkdir -p "
               (pr-str (subs target 0 (str/last-index-of target "/")))))
      (loop [repos *mvn-repos*]
        (when-let [repo (first repos)]
          (if (http/fetch (str repo "/" relative) target)
            target
            (recur (rest repos))))))))

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

(defn- cache-fresh?
  "Is the extraction at `dir` still valid for `jar`? The `.jolt-ok` marker is
  written after a successful unzip; it is stale once the jar is rebuilt/refetched
  (a SNAPSHOT, or the same coord re-installed into ~/.m2). A POSIX `test -nt`
  re-extracts when the jar is newer than the marker. The legacy JOLT_MVNLIBS
  layout keeps no jar, so its extraction is the only copy — trust it. A jar that
  has since vanished (m2 pruned) also leaves the extraction as the last good copy."
  [dir jar legacy]
  (let [ok (str dir "/.jolt-ok")]
    (and (file-exists? ok)
         (or legacy
             (not (file-exists? jar))
             (not (zero? (sh (str "test " (pr-str jar) " -nt " (pr-str ok)))))))))

(defn- extract-jar!
  "Unzip `jar` into `dir` (overwriting), marking `.jolt-ok` only on success so a
  failed/partial unzip is never trusted as a complete extraction. A stale
  `.jolt-ok` from a prior extraction is cleared first, so a failed re-extract
  isn't left looking valid. Returns dir on success, nil on failure (a non-fatal
  skip). The jar may live inside `dir` (the legacy JOLT_MVNLIBS layout), so `dir`
  is not wiped."
  [jar dir]
  (sh (str "mkdir -p " (pr-str dir)))
  (sh (str "rm -f " (pr-str (str dir "/.jolt-ok"))))
  (if (zero? (sh (str "unzip -o -q " (pr-str jar) " -d " (pr-str dir))))
    (do (sh (str "touch " (pr-str (str dir "/.jolt-ok")))) dir)
    (do (warn "failed to extract " jar) nil)))

(defn- ensure-maven
  "Ensure coord@version's JAR is in the local Maven repository (reusing one the
  JVM toolchain already fetched; downloading from Clojars then Central when
  absent) and extract its source beside it. Re-extracts when the jar is newer
  than the last extraction. Returns the extraction dir, or nil if no repo has the
  artifact / extraction failed (a non-fatal skip)."
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
    (if (cache-fresh? dir jar legacy)
      dir
      (if (and (not legacy) (file-exists? jar))
        (do (info "using " jar-name " from the local Maven repository")
            (extract-jar! jar dir))
        (loop [repos *mvn-repos*]
          (if (empty? repos)
            (do (warn "maven dep " coord " " version " not found (tried " (str/join ", " *mvn-repos*) ")") nil)
            (if (do (sh (str "mkdir -p " (pr-str (if legacy dir (str (m2-repo-dir) "/" vdir-rel)))))
                    (http/fetch (str (first repos) "/" vdir-rel "/" jar-name) jar))
              (do (info "fetching " coord " " version)
                  (let [d (extract-jar! jar dir)]
                    ;; legacy layout never keeps the jar; the m2 layout does —
                    ;; that IS the sharing.
                    (when legacy (sh (str "rm -f " (pr-str jar))))
                    d))
              (recur (rest repos)))))))))

(defn- raw-pom-deps-from
  "Transitive deps read from a pom.xml — as a deps map so the expansion walks
  them like any other. Repository Maven deps use Grenadine effective POMs; this
  reads whatever the jar itself packages, for a local jar whose Maven
  coordinates are unknown and for a dep whose effective POM wouldn't build. It
  skips a dependency it can't read a literal version for rather than failing."
  [pom]
  (do
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

;; --- git URL inference ------------------------------------------------------
;; tools.deps lets a git coordinate omit :git/url when the lib name encodes a
;; known host: `io.github.OWNER/REPO` resolves to https://github.com/OWNER/REPO.git,
;; and similarly for GitLab, Bitbucket, and Sourcehut. jolt honors the same
;; convention so a deps.edn copied from a tools.deps project resolves unchanged.
;; See https://clojure.org/reference/deps_edn#deps_git.
(def ^:private git-url-hosts
  ;; [namespace-prefix  url-prefix  url-suffix] — the URL is
  ;; url-prefix + OWNER + "/" + REPO + url-suffix, where OWNER is the coordinate
  ;; namespace with its host prefix stripped and REPO is the coordinate name.
  [["io.github."    "https://github.com/"    ".git"]
   ["com.github."   "https://github.com/"    ".git"]
   ["io.gitlab."    "https://gitlab.com/"    ".git"]
   ["com.gitlab."   "https://gitlab.com/"    ".git"]
   ["io.bitbucket."  "https://bitbucket.org/" ".git"]
   ["org.bitbucket." "https://bitbucket.org/" ".git"]
   ;; Sourcehut lib names carry the leading ~ in the owner, e.g. ht.sr.~owner/repo,
   ;; so stripping "ht.sr." leaves "~owner" and no .git suffix is used.
   ["ht.sr."        "https://git.sr.ht/"     ""]])

(defn- infer-git-url
  "The git clone URL a coordinate's lib name implies via the tools.deps host
  convention (io.github.OWNER/REPO -> https://github.com/OWNER/REPO.git), or nil
  when the namespace names no known host."
  [coord]
  (when-let [ns (namespace coord)]
    (some (fn [[prefix url-prefix url-suffix]]
            (when (str/starts-with? ns prefix)
              (str url-prefix (subs ns (count prefix)) "/" (name coord) url-suffix)))
          git-url-hosts)))

(defn- has-clj-source?
  "Does the tree hold any jolt-loadable source (.clj/.cljc)? A Maven JAR that is
  pure-Java (closure-compiler) or ClojureScript-only (cljs.java-time) has none —
  it contributes nothing to run and its transitive deps are the cljs/JVM toolchain,
  so the walk skips it rather than dragging in that whole subtree."
  [root]
  (zero? (sh (str "find " (pr-str root)
                  " \\( -name '*.clj' -o -name '*.cljc' \\) -print -quit 2>/dev/null | grep -q ."))))

;; --- coordinate skips + normalization ----------------------------------------
;; jolt IS Clojure, so org.clojure/clojure is intrinsic; jolt has no
;; ClojureScript compiler, so clojurescript (and the closure/rhino toolchain it
;; drags in) is dead weight a cljc library declares only for its :cljs branch.
(defn- intrinsic-dep? [lib]
  (or (= lib 'org.clojure/clojure) (= lib 'org.clojure/clojurescript)))

(declare effective-pom-deps)

;; ...but org.clojure/clojure being intrinsic does NOT make its DEPENDENCIES
;; intrinsic. The artifact depends on spec.alpha and core.specs.alpha, neither of
;; which is part of core on either host, so on the JVM a project that declares
;; only Clojure still gets clojure.spec.alpha — and libraries lean on that,
;; requiring spec without ever naming it (kaocha, and spec.alpha's own suite).
;; Dropping the coordinate whole took its children with it and those libraries
;; could not load. Substitute the children instead.
(def ^:private clojure-spec-libs
  '[org.clojure/spec.alpha org.clojure/core.specs.alpha])

;; What Clojure 1.12 ships, for when the POM cannot be read (offline, or a
;; hand-installed jar). Being a version behind is far better than the namespace
;; being absent, which is a load failure rather than a resolution difference.
(def ^:private clojure-spec-fallback
  {'org.clojure/spec.alpha {:mvn/version "0.5.238"}
   'org.clojure/core.specs.alpha {:mvn/version "0.4.74"}})

(defn- clojure-spec-deps
  "The spec coordinates the declared Clojure would bring in on the JVM, read off
  that Clojure's own POM so the versions match what tools.deps would resolve."
  [coord]
  (or (when (:mvn/version coord)
        (not-empty (select-keys (or (effective-pom-deps 'org.clojure/clojure coord true) {})
                                clojure-spec-libs)))
      clojure-spec-fallback))

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
                    ;; jolt IS Clojure, so the coordinate itself contributes
                    ;; nothing — but its spec children are not part of core here
                    ;; either, and on the JVM they come in with it.
                    (= lib 'org.clojure/clojure) (seq (clojure-spec-deps spec))
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
(defmethod ext/dep-id :git [_ coord] (select-keys (git-coord coord) [:git/url :git/sha :git/tag]))
(defmethod ext/dep-id :local [_ coord] (select-keys coord [:local/root]))

;; What names a version in the dependency tree: the Maven version, the git tag
;; if the coordinate is pinned by one (else the sha, shortened the way git
;; prints it), the directory for a local root.
(defmethod ext/coord-summary :mvn [lib coord] (str lib " " (:mvn/version coord)))
(defmethod ext/coord-summary :git [lib coord]
  (let [coord (git-coord coord)
        sha (:git/sha coord)]
    (str lib " " (or (:git/tag coord)
                     (when sha (subs sha 0 (min 7 (count sha))))))))
(defmethod ext/coord-summary :local [lib coord] (str lib " " (:local/root coord)))

(defn- mvn-info [lib coord]
  (memoized [:info lib (ext/dep-id lib coord)]
    (fn []
      (let [root (ensure-maven lib (:mvn/version coord))]
        (cond
          (nil? root) {:root nil :manifest :none}
          ;; a Maven dep with no jolt-loadable source contributes nothing and
          ;; its transitive deps are cljs/JVM tooling — don't walk them.
          (not (has-clj-source? root)) {:root nil :manifest :none}
          ;; :pom is the fallback children-of reaches for when the effective POM
          ;; can't be built — not every jar carries one, and the ones that do
          ;; predate their own dependencyManagement, so it is second choice.
          :else {:root root :manifest :mvn
                 :deps (effective-pom-deps lib coord)
                 :pom (str root "/META-INF/maven/" (mvn-group lib) "/"
                           (name lib) "/pom.xml")})))))

(defn- git-info [lib coord]
  (memoized [:info lib (ext/dep-id lib coord)]
    (fn []
      (let [coord (git-coord coord)
            url (or (:git/url coord) (infer-git-url lib)
                    (throw (ex-info
                             (str "git dep " lib " has no :git/url and none could be inferred "
                                  "from its lib name. Add :git/url, or name the coordinate after "
                                  "its host, e.g. io.github.OWNER/REPO for a GitHub repo.")
                             {:lib lib :coord coord})))
            sha (resolve-git-sha lib url coord)
            checkout (ensure-git lib url sha)
            root (if-let [r (:deps/root coord)] (str checkout "/" r) checkout)]
        (assoc (manifest-info root) :sha sha :url url :checkout checkout)))))

(defn- jar-extraction-dir [jar]
  (str (or (getenv "JOLT_JARLIBS")
           (str (or (getenv "HOME") ".") "/.jolt/jarlibs"))
       "/" (sanitize jar)))

(defn- local-info [lib coord]
  (memoized [:info lib (ext/dep-id lib coord)]
    (fn []
      (let [path (:local/root coord)]
        (if (str/ends-with? path ".jar")
          ;; a jar local root extracts into a cache keyed by the jar path (the
          ;; jar's directory may not be writable) and is re-extracted when the
          ;; jar is newer than the extraction, like a Maven jar.
          (let [dir (jar-extraction-dir path)]
            (if (or (cache-fresh? dir path false)
                    (extract-jar! path dir))
              (let [pom (sh-out (str "find " (pr-str dir) "/META-INF -name pom.xml -print -quit 2>/dev/null"))]
                {:root dir :manifest :mvn :pom (when (and pom (not (str/blank? pom))) pom)})
              {:root nil :manifest :none}))
          (manifest-info path))))))

(defmethod ext/coord-info :mvn [lib coord] (mvn-info lib coord))
(defmethod ext/coord-info :git [lib coord] (git-info lib coord))
(defmethod ext/coord-info :local [lib coord] (local-info lib coord))

(defn- children-of [{:keys [root manifest edn pom deps]}]
  (cond
    (nil? root) []
    (= manifest :deps-edn) (filter-deps (:deps edn) root)
    (and (= manifest :mvn) deps) (filter-deps deps root)
    (and (= manifest :mvn) pom (file-exists? pom))
    (filter-deps (raw-pom-deps-from pom) root)
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

(defn- git-ancestor? [dir a b]
  (zero? (sh (str "git -C " (pr-str dir) " merge-base --is-ancestor " a " " b " 2>/dev/null"))))

(defmethod ext/compare-versions [:git :git] [lib x y]
  (let [xi (git-info lib x) yi (git-info lib y)
        xs (:sha xi) ys (:sha yi)]
    (cond
      (= xs ys) 0
      :else
      (let [in (fn [dir] (cond (git-ancestor? dir xs ys) -1
                               (git-ancestor? dir ys xs) 1))]
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

(defn resolve-deps
  "Expand a deps map through the tools.deps expansion engine, then collect the
  selected libraries' source roots and :jolt/native declarations in stable
  first-inclusion order. Returns {:roots [...] :natives [...] :prep [...]
  :libs {lib coord}} — :libs is the tools.deps lib map (selected coordinate
  per library).

  `opts` carries the alias-combined coordinate maps applied at every node like
  tools.deps: :override-deps replaces a lib's coordinate wherever it appears
  (top-level or transitive); :default-deps supplies one where a dependent left
  the coordinate nil. With :trace? the result also carries the expansion
  :trace — {:log [node …] :vmap version-map}, the tools.deps trace shape, which
  dep-tree-lines renders and trace-edn-string writes out."
  ([deps base-dir] (resolve-deps deps base-dir nil))
  ([deps base-dir {:keys [override-deps default-deps trace?]}]
   (binding [*procure-memo* (or *procure-memo* (atom {}))]
     (let [abs #(absolutize-local % base-dir)
           top (filter-deps deps base-dir)
           override-deps (some->> override-deps (map (fn [[l c]] [l (abs c)])) (into {}))
           default-deps (some->> default-deps (map (fn [[l c]] [l (abs c)])) (into {}))
           expansion
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
             :on-warning expansion-warning})
           libmap (:libs expansion)
           infos (keep (fn [lib]
                         (when-let [coord (get libmap lib)]
                           (let [info (ext/coord-info lib coord)]
                             (when (:root info) (assoc info :lib lib)))))
                       (:order expansion))]
       {:roots (vec (mapcat (fn [{:keys [root manifest edn]}]
                              (if (= manifest :deps-edn)
                                (map #(abspath root %) (or (:paths edn) ["src"]))
                                [root]))
                            infos))
        :natives (vec (mapcat (fn [{:keys [edn]}] (:jolt/native edn)) infos))
        ;; libs whose deps.edn declares :deps/prep-lib — jolt runs no prep
        ;; steps, so their compiled/generated assets will be missing; the
        ;; caller warns with the lib names.
        :prep (vec (keep (fn [{:keys [lib edn]}] (when (:deps/prep-lib edn) lib)) infos))
        :libs libmap
        :trace (:trace expansion)}))))

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

(defn resolve-project
  "Resolve `project-dir`'s deps.edn with the selected alias keywords. Returns
  {:roots [...] :main-opts [...] :tasks {...} :natives [...]}; :natives are the
  project's + deps' :jolt/native shared-library declarations.

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
  ([project-dir alias-kws extra-edn {:keys [tool? repro? trace? cp]}]
   (let [project-edn (read-deps-file (str project-dir "/deps.edn"))
         user-edn (when-not (or repro? (getenv "JOLT_NO_USER_DEPS"))
                    (try (read-deps-file (user-deps-path))
                         (catch :default e (warn (ex-message e)) nil)))
         edn (dedn/merge-edns [user-edn project-edn (some-> extra-edn dedn/canonicalize)])
         argmap (dedn/combine-aliases edn alias-kws)
         ;; An alias the merged chain doesn't declare is skipped with a warning,
         ;; the tools.deps behavior verbatim ("WARNING: Specified aliases are
         ;; undeclared and are not being used"): loud enough that a typo'd alias
         ;; doesn't run the program without its deps and fail somewhere far
         ;; away, but not fatal — an editor sends a fixed alias set (Calva asks
         ;; for `-A:test:dev -Spath`) and a project that declares only some of
         ;; them still has a classpath, and a program, to give it.
         _ (let [missing (distinct (remove #(get-in edn [:aliases %]) alias-kws))]
             (when (seq missing)
               (warn "Specified aliases are undeclared and are not being used: "
                     (pr-str (vec missing)))))
         main-opts (:main-opts argmap)
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
         project-paths (concat (:extra-paths argmap)
                               (or (:replace-paths argmap) (:paths argmap)
                                   (if tool? ["."] (or (:paths edn) ["src"]))))
         project-roots (map #(abspath project-dir %) project-paths)
         all-deps (merge (or (:replace-deps argmap) (:deps argmap)
                             (if tool? {} (:deps edn)))
                         (:extra-deps argmap))
         ;; :cp (the CLI's -Scp) supplies the roots outright, so the dependency
         ;; graph is never expanded and nothing is fetched. The deps.edn chain is
         ;; still read and the aliases still combine — that is only file reads,
         ;; and an alias's :main-opts / :exec-fn and the project's :tasks come
         ;; from there. tools.deps' --skip-cp draws the line in the same place:
         ;; the merged edn and argmap, without calc-basis.
         {dep-roots :roots dep-natives :natives prep-libs :prep dep-trace :trace}
         (when-not cp
           (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo edn)]
                                        (abspath project-dir r))
                     *mvn-repos* (mvn-repo-urls edn)]
             (resolve-deps all-deps project-dir
                           {:override-deps (:override-deps argmap)
                            :default-deps (:default-deps argmap)
                            :trace? trace?})))
         _ (when (seq prep-libs)
             (warn "deps declare :deps/prep-lib steps jolt does not run "
                   "(their compiled/generated assets will be missing): "
                   (str/join ", " prep-libs)))]
     ;; reconcile: the project's own roots/natives + every dep's, deduped once.
     ;; With :cp the given roots ARE the answer — they replace the project's own
     ;; paths too, like the clj CLI, where -Scp is the whole classpath.
     {:roots (dedup-by identity (or cp (concat project-roots dep-roots)))
      :main-opts main-opts
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
      :tasks (:tasks edn)
      :natives (dedup-by native-key (concat (:jolt/native edn) dep-natives))
      ;; the expansion trace, when it was asked for (-Stree renders it)
      :trace dep-trace
      ;; nREPL middleware a library contributes (jolt.nrepl composes them over its
      ;; built-in handler) — symbols resolving to a middleware fn or a vector of them.
      :nrepl-middleware (:nrepl/middleware edn)})))

(defn env-info
  "The files a resolution reads and the caches it fetches into, as data. One
  source of truth for the CLI's two renderings of it: -Sdescribe prints the map,
  -Sverbose the same values as lines before resolving. `repro?` reports whether
  the user deps.edn is being skipped (-Srepro / JOLT_NO_USER_DEPS)."
  ([project-dir] (env-info project-dir false))
  ([project-dir repro?]
   (let [repro? (boolean (or repro? (getenv "JOLT_NO_USER_DEPS")))
         user (user-deps-path)
         project (str project-dir "/deps.edn")]
     {:project-dir project-dir
      :config-user (when-not repro? user)
      :config-project project
      :config-files (vec (concat (when (and (not repro?) (file-exists? user)) [user])
                                 (when (file-exists? project) [project])))
      :gitlibs-dir (gitlibs-dir)
      :mvn-local-repo (m2-repo-dir)
      :repro repro?})))

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

  :jolt/native declarations carried by added deps are NOT auto-loaded (that is
  a project-launch concern — see jolt.main); a warning names them so the
  caller can load via jolt.ffi. The second arity accepts an options map for
  babashka call-shape compatibility; no options are currently honored."
  ([deps-map] (add-deps deps-map nil))
  ([{:keys [deps] :as m} _opts]
   (let [base (or (getenv "JOLT_PWD") ".")
         {:keys [roots natives]}
         (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo m)]
                                      (abspath base r))]
           (resolve-deps deps base))
         current (vec (jolt.host/source-roots))
         added (vec (remove (set current) (dedup-by identity roots)))]
     (when (seq added)
       (jolt.host/set-source-roots! (into current added)))
     (when (seq natives)
       (info "added deps declare :jolt/native libraries (not auto-loaded): "
             (pr-str (dedup-by native-key natives))))
     added)))
