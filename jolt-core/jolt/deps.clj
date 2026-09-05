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
  git and unzip still shell out through jolt.host/sh (nothing here touches the JVM)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [grenadine.expander :as expander]
            [grenadine.pom :as grenadine.pom]
            [grenadine.require-deps :as required]
            [grenadine.version :as grenadine.version]
            [jolt.deps.edn :as dedn]
            [jolt.deps.ext :as ext]
            [jolt.mvn-http :as http]))

(defonce ^:private required-state
  (atom {:coordinates {} :namespaces {}}))

;; --- small host seams -------------------------------------------------------
;; An env var set to the empty string reads as UNSET — `FOO= cmd` is the usual
;; way to clear a variable for one command, and treating "" as a value would
;; turn that into "set to nothing".
(defn- getenv [n] (let [v (jolt.host/getenv n)] (when-not (str/blank? v) v)))
(defn- file-exists? [p] (jolt.host/file-exists? p))
(defn- sh [cmd] (jolt.host/sh cmd))           ; exit code, inherits stdout/stderr

;; The filesystem seams. These used to be shell commands (`mkdir -p`, `mv`,
;; `rm -f`, `rm -rf`, `test -nt`, `find`), which is a POSIX assumption jolt.host/sh
;; does not carry: on Windows it runs the string through cmd.exe, where `mkdir -p
;; a/b` creates a directory named `-p` and the rest are not commands at all. Every
;; one of these is now a filesystem call, so the only subprocesses left in this
;; namespace are the two real external programs, git and unzip.
(defn- mkdirs! [p] (jolt.host/mkdirs! p))     ; mkdir -p; true if p ends up a dir
(defn- rm-f [p] (jolt.host/delete-file! p))   ; absent is success
(defn- rm-rf [p] (jolt.host/delete-tree! p))
(defn- mv! [from to] (jolt.host/rename-file! from to))
(defn- file-mtime [p] (jolt.host/file-mtime p))   ; epoch ms, 0 when absent
(defn- touch! [p] (spit p ""))                ; the markers are all empty files

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

(defn- tmp-dir
  "A directory for scratch files. /tmp is not universal — Windows has no such
  path, and %TEMP% is where a temporary file goes there."
  []
  (let [d (or (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP") "/tmp")]
    (str/replace d #"[/\\]+$" "")))

(defn- sh-out*
  "Run a shell command, capturing stdout and stderr separately:
  {:exit n :out s :err s}. Captures through temp files (jolt.host/sh inherits
  stdio), which is also how a command's output gets discarded portably — there
  is no /dev/null to redirect to everywhere jolt runs."
  [cmd]
  (let [stamp (str (System/currentTimeMillis) "-" (rand-int 100000))
        out-f (str (tmp-dir) "/jolt-deps-" stamp ".out")
        err-f (str (tmp-dir) "/jolt-deps-" stamp ".err")
        code (sh (str cmd " > " (pr-str out-f) " 2> " (pr-str err-f)))
        read-f (fn [p] (when (file-exists? p) (str/trim (slurp p))))
        r {:exit code :out (read-f out-f) :err (read-f err-f)}]
    (rm-f out-f)
    (rm-f err-f)
    r))

(defn- warn [& xs] (binding [*out* *err*] (println (str "[jolt.deps] " (apply str xs)))))

;; --- retrying the network steps ---------------------------------------------
;; A git command that talks to a remote fails transiently more often than not — a
;; reset, a rate limit, a DNS blip — and the cost of finding out is one more
;; round trip. The HTTPS fetch already works this way (jolt/mvn_http.clj); these
;; are the git-side equivalents, and they matter for the same reason: a flake
;; here fails a whole build, and a release with it.
(def ^:private net-attempts 3)
;; Deliberately short, same reasoning as the fetch's: this sits in front of a
;; developer waiting on a build, and the failures it covers clear in well under a
;; second. A long backoff trades a rare recovery for a guaranteed stall on the
;; genuinely-unreachable case.
(def ^:private net-backoff-ms [200 600])

(defn- with-net-retries
  "Call ATTEMPT until `ok?` accepts its result, at most net-attempts times, with
  a short backoff between. Returns the LAST result either way — still a failing
  one when the cap is reached, so the caller reports the failure it actually got
  rather than treating the cap as a different kind of answer."
  [ok? attempt]
  (loop [n 1]
    (let [r (attempt)]
      (if (or (ok? r) (>= n net-attempts))
        r
        (do (when-let [ms (nth net-backoff-ms (dec n) nil)] (Thread/sleep ms))
            (recur (inc n)))))))
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

(defn- read-edn [path]
  (when (file-exists? path)
    (try (edn/read-string (slurp path))
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

;; --- git cache --------------------------------------------------------------
;; jolt's own clone cache. $GITLIBS (the tools.gitlibs location knob) is
;; respected for WHERE the cache lives — under a jolt/ subdir so tools.gitlibs'
;; own _repos/ and libs/ namespaces are never written to. JOLT_GITLIBS_DIR
;; pins an exact directory.
(defn- gitlibs-dir
  ([] (gitlibs-dir (getenv "JOLT_GITLIBS_DIR")
                   (getenv "GRENADINE_GITLIBS_DIR")
                   (getenv "GITLIBS")
                   (getenv "HOME")))
  ([jolt-cache grenadine-cache gitlibs home]
   (or jolt-cache
       (when grenadine-cache (str grenadine-cache "/jolt"))
       (when gitlibs (str gitlibs "/jolt"))
       (str (or home ".") "/.jolt/gitlibs"))))

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
    (let [base (or (getenv "GRENADINE_GITLIBS_DIR")
                   (getenv "GITLIBS")
                   (str (or (getenv "HOME") ".") "/.gitlibs"))
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
      ;; "the tag is not there" and "we could not go look" are different answers.
      ;; Collapsing them reported a transient ls-remote failure — a reset, a rate
      ;; limit, an unreachable host — as a tag that does not exist, which sends
      ;; the reader to the repository and the pin instead of to the fetch. (The
      ;; v0.7.7 release failed this way against a tag two weeks old.) Same
      ;; distinction the Maven path makes below, and for the same reason.
      (let [{:keys [exit out err]}
            (with-net-retries #(zero? (:exit %))
              #(sh-out* (str "git ls-remote " (pr-str url) " "
                             (pr-str (str "refs/tags/" tag)) " "
                             (pr-str (str "refs/tags/" tag "^{}")))))]
        (when-not (zero? exit)
          (throw (ex-info (str "git dep: could not list " url " (git ls-remote exited "
                               exit " after " net-attempts " attempts"
                               (when-not (str/blank? err)
                                 (str ": " (first (str/split-lines err))))
                               ")")
                          {:type :jolt.deps/git-ls-remote-failed
                           :url url :tag tag :exit exit :err err :attempts net-attempts})))
        (let [lines (str/split-lines (or out ""))
              parse (fn [suffix]
                      (some (fn [l] (let [[sha ref] (str/split l #"\s+")]
                                      (when (and ref (str/ends-with? ref suffix)) sha)))
                            lines))
              peeled (parse "^{}")
              obj (parse (str "refs/tags/" tag))
              commit (or peeled obj)
              obj (when (and peeled obj (not= peeled obj)) obj)]
          (when commit
            (mkdirs! (str (gitlibs-dir) "/tags/" (sanitize url)))
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
        scrub #(rm-rf stage)
        fail (fn [msg data] (scrub) (throw (ex-info msg (merge {:url url :sha sha} data))))]
    (info "fetching " url " @ " (subs sha 0 (min 12 (count sha))))
    (mkdirs! repo-dir)
    ;; the clone talks to the remote, so it gets the same bounded retry the tag
    ;; listing does — scrubbing first, since a failed clone leaves a partial
    ;; staging dir that the next attempt would refuse to clone into.
    (when-not (zero? (with-net-retries zero? (fn [] (scrub) (sh (str "git clone --quiet " (pr-str url) " " (pr-str stage))))))
      (fail (str "git clone failed after " net-attempts " attempts: " url) nil))
    ;; local, so no retry: the objects are already in the staging clone.
    (when-not (zero? (sh (str "git -C " (pr-str stage) " checkout --quiet " (pr-str sha))))
      (fail (str "git checkout failed: " sha " in " url) nil))
    ;; submodules are pinned in the checkout; pull them if the dep uses any. This
    ;; one fetches too, and is idempotent, so it retries in place.
    (when-not (zero? (with-net-retries zero? #(sh (str "git -C " (pr-str stage) " submodule update --init --recursive --quiet"))))
      (fail (str "git submodule update failed after " net-attempts " attempts for " url) nil))
    (touch! (str stage "/.jolt-git-ok"))
    (if (checkout-complete? dir)
      (do (scrub) dir)                    ; another process published it first
      (do
        ;; clears the incomplete remains of an interrupted in-place clone
        (rm-rf dir)
        (when-not (mv! stage dir)
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
;; A JAR of pure Java classes has no source to run, but the resources it packages
;; are still readable through it, so it stays on the roots as a leaf.
;;
;; JARs live at their standard path in the local Maven repository
;; (~/.m2/repository), so they are shared with JVM Clojure/tools.deps in both
;; directions: an artifact clj already fetched is reused without a download, and
;; one jolt fetches is there for clj. The jolt-only source extraction sits in a
;; "<artifact>-<version>.jar.jolt/" directory beside the jar. The repository
;; location is configured the way tools.deps configures it — the :mvn/local-repo
;; top key of deps.edn (also accepted in an add-deps map); anyone already using
;; it gets the same behavior for free. JOLT_MAVEN_REPOSITORY supplies the
;; dialect override and GRENADINE_MAVEN_REPOSITORY the shared default. Setting
;; JOLT_MVNLIBS opts out of sharing entirely: the legacy
;; self-contained layout under it, jar not kept.

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
  (a SNAPSHOT, or the same coord re-installed into ~/.m2), so a jar whose mtime
  is later than the marker's re-extracts. The legacy JOLT_MVNLIBS layout keeps
  no jar, so its extraction is the only copy — trust it. A jar that has since
  vanished (m2 pruned) also leaves the extraction as the last good copy."
  [dir jar legacy]
  (let [ok (str dir "/.jolt-ok")]
    (and (file-exists? ok)
         (or legacy
             (not (file-exists? jar))
             (<= (file-mtime jar) (file-mtime ok))))))

(defn- extract-jar!
  "Unzip `jar` into `dir` (overwriting), marking `.jolt-ok` only on success so a
  failed/partial unzip is never trusted as a complete extraction. A stale
  `.jolt-ok` from a prior extraction is cleared first, so a failed re-extract
  isn't left looking valid. Returns dir on success, nil on failure (a non-fatal
  skip). The jar may live inside `dir` (the legacy JOLT_MVNLIBS layout), so `dir`
  is not wiped."
  [jar dir]
  (mkdirs! dir)
  (rm-f (str dir "/.jolt-ok"))
  (if (zero? (sh (str "unzip -o -q " (pr-str jar) " -d " (pr-str dir))))
    (do (touch! (str dir "/.jolt-ok")) dir)
    (do (warn "failed to extract " jar) nil)))

(defn- extract-or-note!
  "extract-jar! that records a failed extraction as an unresolvable artifact.
  The jar is on disk but its source cannot be materialized (unzip missing, disk
  full) — a failure to OBTAIN the artifact, not a skip. Left silent, the
  resolution completes without the dep's root and the cpcache persists the
  degraded roots until the project's .jolt is deleted by hand."
  [jar dir coord version]
  (or (extract-jar! jar dir)
      (do (note-unresolvable! coord version
                              (str jar " could not be extracted (is unzip installed?)"))
          nil)))

(defn- ensure-maven
  "Ensure coord@version's JAR is in the local Maven repository (reusing one the
  JVM toolchain already fetched; downloading from Clojars then Central when
  absent) and extract its source beside it. Re-extracts when the jar is newer
  than the last extraction. Returns the extraction dir, or nil after recording
  the artifact as unresolvable (absent from every repo, fetch failures, or a
  failed extraction) — the expansion reports every one and aborts at its end."
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
            (extract-or-note! jar dir coord version))
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
                  _ (mkdirs! (if legacy dir (str (m2-repo-dir) "/" vdir-rel)))
                  r (http/fetch* (str repo "/" vdir-rel "/" jar-name) jar)]
              (if (= :ok (:outcome r))
                (do (info "fetching " coord " " version)
                    (let [d (extract-or-note! jar dir coord version)]
                      ;; legacy layout never keeps the jar; the m2 layout does —
                      ;; that IS the sharing.
                      (when legacy (rm-f jar))
                      d))
                (recur (rest repos)
                       (if (= :not-found (:outcome r))
                         errors
                         (conj errors (str repo " — "
                                           (or (:error r)
                                               (when (:status r) (str "HTTP " (:status r)))
                                               (name (:outcome r)))))))))))))))

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
  pure-Java (closure-compiler) or ClojureScript-only (cljs.java-time) has none,
  so nothing in it can be required. It still belongs on the roots — a jar can
  carry RESOURCES and nothing else, which is exactly what Cognitect's aws
  endpoints data is, and io/resource has to find them. What it does not get is
  a WALK: with no source of ours to load, the deps it declares are its
  publisher's own JVM/cljs toolchain, and jolt has no JVM to run them on."
  [root]
  (boolean (find-file root #(or (str/ends-with? % ".clj") (str/ends-with? % ".cljc")))))

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
          ;; a Maven dep with no jolt-loadable source is a LEAF: the extraction
          ;; stays on the roots so io/resource can read whatever it packages,
          ;; but with no source of ours in it, the deps it declares are cljs/JVM
          ;; tooling — no :deps and no :pom here is what stops the walk.
          (not (has-clj-source? root)) {:root root :manifest :mvn}
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
                    (extract-or-note! path dir lib path))
              (let [pom (find-file (str dir "/META-INF") #(= % "pom.xml"))]
                {:root dir :manifest :mvn :pom pom})
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

;; sh-out* rather than sh: this asks a question whose "no" arrives as both a
;; non-zero exit and a message on stderr, and the 2>/dev/null that used to
;; silence it is a POSIX path, not a portable way to discard output.
(defn- git-ancestor? [dir a b]
  (zero? (:exit (sh-out* (str "git -C " (pr-str dir) " merge-base --is-ancestor " a " " b)))))

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

(defn- provides-entries
  "A deps.edn :jolt/provides map as provider-table rows: [install-ns lib class ...].
  lib is the declaring dependency's coordinate, or nil for the project's own."
  [edn lib]
  ;; Both names are stringified here: the coordinate is a symbol in deps.edn and
  ;; the runtime string-appends it into the "declared but failed to load"
  ;; message. nil stays nil — it means jolt's own, which has no coordinate.
  (map (fn [[install-ns classes]]
         (into [(str install-ns) (when lib (str lib))] (map str classes)))
       (:jolt/provides edn)))

(defn resolve-deps
  "Expand a deps map through the tools.deps expansion engine, then collect the
  selected libraries' source roots, :jolt/native and :jolt/provides declarations
  in stable first-inclusion order. Returns {:roots [...] :natives [...]
  :min-versions [...] :provides [...] :prep [...] :libs {lib coord}} — :libs is
  the tools.deps lib map (selected coordinate per library).

  `opts` carries the alias-combined coordinate maps applied at every node like
  tools.deps: :override-deps replaces a lib's coordinate wherever it appears
  (top-level or transitive); :default-deps supplies one where a dependent left
  the coordinate nil. With :trace? the result also carries the expansion
  :trace — {:log [node …] :vmap version-map}, the tools.deps trace shape, which
  dep-tree-lines renders and trace-edn-string writes out."
  ([deps base-dir] (resolve-deps deps base-dir nil))
  ([deps base-dir {:keys [override-deps default-deps trace?]}]
   ;; A nested resolve-deps inherits the outer atom, so the summary is printed
   ;; ONCE by whichever frame created it rather than once per level.
   (let [outermost? (nil? *unresolvable*)]
   (binding [*procure-memo* (or *procure-memo* (atom {}))
             *unresolvable* (or *unresolvable* (atom []))]
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
       (when outermost? (fail-unresolvable!))
       {:roots (vec (mapcat (fn [{:keys [root manifest edn]}]
                              (if (= manifest :deps-edn)
                                (map #(abspath root %) (or (:paths edn) ["src"]))
                                [root]))
                            infos))
        :natives (vec (mapcat (fn [{:keys [edn]}] (:jolt/native edn)) infos))
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
        :provides (vec (mapcat (fn [{:keys [lib edn]}] (provides-entries edn lib)) infos))
        ;; libs whose deps.edn declares :deps/prep-lib — jolt runs no prep
        ;; steps, so their compiled/generated assets will be missing; the
        ;; caller warns with the lib names.
        :prep (vec (keep (fn [{:keys [lib edn]}] (when (:deps/prep-lib edn) lib)) infos))
        :libs libmap
        :trace (:trace expansion)})))))

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
;; disk — a pruned gitlib checkout or a deleted jar/extraction is a miss, not an
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

(defn- host-class-providers
  "Reconcile the :jolt/provides declarations (RFC 0014) into the provider table
  the runtime autoloads from, as [install-ns lib class-name ...].

  Two dependencies claiming the same host class is refused rather than resolved.
  Whichever won would decide what `java.sql.Connection` MEANS for the whole
  program, and the loser's shim would be half-installed — registered for the
  classes nobody else claimed and silently absent for the rest. That is the
  shape of bug this table exists to stop being possible, so it is an error at
  resolve time, where both claimants can be named.

  Entries are [install-ns lib class ...]; lib is nil for the project's own."
  [entries]
  (let [claims (reduce (fn [acc [install-ns lib & classes]]
                         (reduce (fn [a c] (update a c (fnil conj #{}) [install-ns lib]))
                                 acc classes))
                       {} entries)
        conflicts (filter (fn [[_ owners]] (< 1 (count (set (map first owners)))))
                          claims)]
    (when (seq conflicts)
      (let [[c owners] (first (sort-by key conflicts))
            who (fn [[install-ns lib]] (str (or lib "this project") " (" install-ns ")"))]
        (throw (ex-info
                (str "two dependencies both claim to provide the host class " c
                     ": " (str/join " and " (sort (map who owners)))
                     ". A host class can have one provider; drop one of them.")
                {:jolt/class c
                 :jolt/claimed-by (vec (sort (map who owners)))}))))
    (vec (distinct entries))))

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
                (if (contains? seen dir)
                  (walk (rest queue) seen acc)
                  (let [seen (conj seen dir)
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
       "|" runtime-version
       ;; the environment-dependent artifact roots resolution materializes into:
       ;; a run pointed at a different gitlibs/jarlibs (JOLT_GITLIBS_DIR — the
       ;; deps-alias smoke's retry scenarios do exactly this), a different
       ;; HOME (the ~/.m2 and ~/.jolt defaults), or a moved Maven repo
       ;; (JOLT_MVNLIBS / JOLT_MAVEN_REPOSITORY /
       ;; GRENADINE_MAVEN_REPOSITORY) must
       ;; not share an entry whose cached roots point into the old location —
       ;; the old paths usually still exist, so validation alone won't miss.
       "|" (gitlibs-dir)
       "|" (or (getenv "JOLT_JARLIBS") "")
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

(declare user-deps-path)   ; defined below with the deps.edn readers

(defn- resolve-deps-cached
  "resolve-deps wrapped in the .jolt/cpcache cache. On a hit the cached result
  (the resolve-deps return map) is returned without expanding the graph or
  touching the network; on a miss the full expansion runs (which fetches missing
  deps) and the result is written. The cache is OFF when JOLT_AOT_CACHE is off
  (the dev bin/jolt posture) or when trace? is set — a trace is a one-off query
  (-Stree/-Strace), not the resolution a run reuses, so it never caches and a
  cached run never serves one. Emits the JOLT_DEBUG-gated hit/miss lines."
  [project-dir deps alias-kws repro? opts]
  (if (and (cpcache-enabled?) (not (:trace? opts)))
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
          (cpcache-write! project-dir k material r)
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

(defn resolve-project
  "Resolve `project-dir`'s deps.edn with the selected alias keywords. Returns
  {:roots [...] :main-opts [...] :tasks {...} :natives [...]}; :natives are the
  project's + deps' :jolt/native shared-library declarations.

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
  ([project-dir alias-kws extra-edn {:keys [tool? repro? trace? cp tasks?]}]
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
                                   (if tool? ["."] (or (:paths edn) ["src"])))
                               ;; last: a task run's bb.edn paths (see read-bb-file)
                               (:paths bb-extra))
         project-roots (map #(abspath project-dir %) project-paths)
         all-deps (merge (or (:replace-deps argmap) (:deps argmap)
                             (if tool? {} (:deps edn)))
                         (:deps bb-extra)
                         (:extra-deps argmap))
          ;; :cp (the CLI's -Scp) supplies the roots outright, so the dependency
          ;; graph is never expanded and nothing is fetched. The deps.edn chain is
          ;; still read and the aliases still combine — that is only file reads,
          ;; and an alias's :main-opts / :exec-fn and the project's :tasks come
          ;; from there. tools.deps' --skip-cp draws the line in the same place:
          ;; the merged edn and argmap, without calc-basis.
          {dep-roots :roots dep-natives :natives dep-provides :provides
           prep-libs :prep dep-trace :trace
           dep-min-versions :min-versions}
          (when-not cp
            (binding [*mvn-local-repo* (when-let [r (:mvn/local-repo edn)]
                                         (abspath project-dir r))
                      *mvn-repos* (mvn-repo-urls edn)]
              (resolve-deps-cached project-dir all-deps alias-kws repro?
                                   {:override-deps (:override-deps argmap)
                                    :default-deps (:default-deps argmap)
                                    :trace? trace?})))
         ;; The floor is checked AFTER resolution, so a dep's own declaration is
         ;; in hand — a library is the natural declarer, since it knows which
         ;; jolt its bindings need and the app pulling it in does not. The
         ;; project's own declaration is checked here too, and is what an app
         ;; uses to pin the runtime it was written against.
         _ (check-min-versions!
            (concat (when-let [v (:jolt/min-version edn)] [[nil v]])
                    dep-min-versions))
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
      ;; :tasks from both files, bb.edn last — a name in both is babashka's.
      ;; (When bb.edn IS the project config the second merge is a no-op.)
      :tasks (not-empty (merge (:tasks edn) (:tasks bb-edn)))
      :natives (dedup-by native-key (concat (:jolt/native edn) dep-natives))
      ;; declared host-class providers (RFC 0014), the project's own first: a
      ;; project may supply a class itself rather than take a library's.
      :provides (host-class-providers (concat (provides-entries edn nil) dep-provides))
      ;; :jolt/replaces — namespaces jolt provides as a host built-in
      ;; (babashka.fs, babashka.process) that THIS project supplies itself, so
      ;; its copy resolves ahead of jolt's and no supplement loads over it.
      ;; The PROJECT's only: a library that took a built-in over would decide
      ;; what the namespace means for every other library in the program, which
      ;; is what host-class-providers already refuses for classes.
      :replaces (mapv str (:jolt/replaces edn))
      ;; the expansion trace, when it was asked for (-Stree renders it)
      :trace dep-trace
      ;; nREPL middleware a library contributes (jolt.nrepl composes them over its
      ;; built-in handler) — symbols resolving to a middleware fn or a vector of them.
      :nrepl-middleware (:nrepl/middleware edn)})))

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

(defn- required-host
  []
  {:home-dir #(getenv "HOME")
   :gitlibs-dir gitlibs-dir
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
