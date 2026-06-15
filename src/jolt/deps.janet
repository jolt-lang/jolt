# deps.edn resolution for Jolt.
#
# Resolve git and :local/root dependencies from a deps.edn into a list of source
# directories, which the loader then searches (see evaluator/find-ns-file). We
# reuse jpm's git fetch + cache (jpm/pm) rather than shipping a package manager.
# Maven (:mvn/version) deps are ignored — git only, pure clj/cljc only.
#
# jpm is loaded lazily (require, not import) so it's needed only at resolve time
# (dev/build), never embedded in the shipped binary.

(import ./reader :as reader)

# Read deps.edn with Jolt's reader (not Janet's parse) so EDN `;` line comments
# are handled. It returns plain Janet data — structs with keyword keys, tuples —
# which we walk directly. (#{} sets and tagged literals aren't expected in the
# :deps/:paths we read.)
(defn read-edn [path]
  (when (os/stat path)
    (try (reader/parse-string (slurp path)) ([_] nil))))

(defn- jpm-fn [mod sym]
  (get-in (require mod) [sym :value]))

(defn- ensure-jpm-config [tree]
  ((jpm-fn "jpm/config" 'load-default))
  (setdyn :modpath tree)
  (setdyn :gitpath (dyn :gitpath "git")))

(defn- clone-git [spec]
  # spec is a deps.edn dep value: {:git/url ... :git/sha/:git/tag ...}
  (def resolve-bundle (jpm-fn "jpm/pm" 'resolve-bundle))
  (def download-bundle (jpm-fn "jpm/pm" 'download-bundle))
  # Run git silenced (jpm's shell honors :silent): its checkout chatter
  # ("HEAD is now at …") otherwise lands on STDOUT and corrupts the
  # documented `JOLT_PATH=$(jolt-deps path)` capture. Progress goes to stderr.
  (eprintf "jolt-deps: fetching %s @ %s"
           (get spec :git/url) (or (get spec :git/sha) (get spec :git/tag) "HEAD"))
  (with-dyns [:silent true]
    (def b (resolve-bundle {:url (get spec :git/url)
                            :sha (get spec :git/sha)
                            :tag (get spec :git/tag)
                            :shallow false}))
    (download-bundle (b :url) (b :type) (b :tag) (b :shallow))))

(defn- src-roots
  "Source dirs of a project/dep at `dir`: its deps.edn :paths joined to dir
  (default [\"src\"])."
  [dir edn]
  (map |(string dir "/" $) (or (and edn (get edn :paths)) ["src"])))

# --- user config + aliases (tools.deps-shaped, scoped to git/:local) -----------

(defn config-dir
  "User-level config dir: $JOLT_CONFIG, else $XDG_CONFIG_HOME/jolt, else
  ~/.jolt — the same fallback chain the Clojure CLI uses for ~/.clojure."
  []
  (or (os/getenv "JOLT_CONFIG")
      (when-let [x (os/getenv "XDG_CONFIG_HOME")]
        (when (> (length x) 0) (string x "/jolt")))
      (string (os/getenv "HOME") "/.jolt")))

(defn- merge-per-key [a b]  # dictionary union, b's entries win
  (def out @{})
  (each m [a b] (when (dictionary? m) (eachp [k v] m (put out k v))))
  out)

# Reader symbols carry position metadata, so any map keyed by SYMBOLS (deps
# libs, task names) must be re-keyed by name before merging or deduping.
(defn- sym-name [x]
  (if (and (dictionary? x) (get x :name))
    (if-let [ns (get x :ns)]
      (string ns "/" (get x :name))
      (get x :name))
    (string x)))


(defn- ensure-jpm-dep
  "A :jpm/module dep declares a janet module installed through jpm (e.g.
  spork/http). jolt-deps doesn't manage janet packages — jpm does — so this
  just verifies the module is importable, optionally running `jpm install
  <:jpm/install>` once when it isn't, and fails with the install hint
  otherwise. Contributes no source roots; the janet.* bridge autoloads the
  module at first use."
  [lib spec]
  (def mod (get spec :jpm/module))
  (defn importable? [] ((protect (require mod)) 0))
  (unless (importable?)
    (when-let [pkg (get spec :jpm/install)]
      (eprintf "jolt-deps: %s: jpm module %s missing — running `jpm install %s`"
               (sym-name lib) mod pkg)
      (os/execute ["jpm" "install" pkg] :p))
    (unless (importable?)
      (errorf "%s: janet module %s is not importable. Install it with `jpm install %s` (jolt-deps leaves janet packages to jpm)."
              (sym-name lib) mod (or (get spec :jpm/install) mod)))))

(defn- merge-by-name [a b]  # union of symbol-keyed dictionaries, b wins
  (def out @{})
  (each m [a b] (when (dictionary? m) (eachp [k v] m (put out (sym-name k) v))))
  out)

(defn- mkdirs [p]
  (def abs (string/has-prefix? "/" p))
  (var acc nil)
  (each seg (filter |(not= "" $) (string/split "/" p))
    (set acc (cond (nil? acc) (if abs (string "/" seg) seg) (string acc "/" seg)))
    (unless (os/stat acc) (os/mkdir acc))))

(defn- default-tree
  "Where git clones land when no tree is given: $JOLT_GITLIBS, else
  (config-dir)/gitlibs — a global, sha-immutable cache shared across projects
  (the tools.gitlibs ~/.gitlibs model), not a per-project ./jpm_tree."
  []
  (def g (os/getenv "JOLT_GITLIBS"))
  (if (and g (> (length g) 0)) g (string (config-dir) "/gitlibs")))

(defn load-config
  "The project deps.edn merged over the user-level one (config-dir)/deps.edn.
  tools.deps merge semantics: scalar keys and :paths replace (project wins),
  :deps and :aliases merge per key with the project winning. Relative
  :local/root in the USER file is resolved against the cwd — prefer absolute
  paths there."
  [deps-edn-path]
  (def proj (read-edn deps-edn-path))
  (def user (read-edn (string (config-dir) "/deps.edn")))
  (if (and (nil? user) (nil? proj))
    nil
    # normalize even when only one file exists: :deps/:tasks come back keyed
    # by NAME (reader symbols carry position metadata and never compare equal)
    (let [u (when (dictionary? user) user)
          p (when (dictionary? proj) proj)
          out (merge-per-key u p)]
      (put out :deps (merge-by-name (and u (get u :deps)) (and p (get p :deps))))
      (put out :aliases (merge-per-key (and u (get u :aliases)) (and p (get p :aliases))))
      (put out :tasks (merge-by-name (and u (get u :tasks)) (and p (get p :tasks))))
      out)))

(defn combine-aliases
  "Combine the selected alias keywords against `edn`'s :aliases:
  :extra-paths and :extra-deps accumulate in order, :main-opts is last-wins
  (the tools.deps CLI rules). Unknown alias -> error."
  [edn aliases]
  (def als (or (and (dictionary? edn) (get edn :aliases)) {}))
  (def extra-paths @[])
  (def extra-deps @{})
  (var main-opts nil)
  (each a (or aliases [])
    (def spec (get als a))
    (when (nil? spec) (error (string "unknown alias: " a)))
    (each p (or (get spec :extra-paths) []) (array/push extra-paths p))
    (when (dictionary? (get spec :extra-deps))
      (eachp [lib coord] (get spec :extra-deps) (put extra-deps lib coord)))
    (when-let [mo (get spec :main-opts)]
      (set main-opts (tuple ;(map string mo)))))
  {:extra-paths extra-paths :extra-deps extra-deps :main-opts main-opts})

(defn alias-main-opts
  "The :main-opts the selected aliases produce (last alias with the key wins),
  or nil. Reads the merged user+project config."
  [deps-edn-path aliases]
  (get (combine-aliases (load-config deps-edn-path) aliases) :main-opts))

(defn resolve-deps
  "Resolve the git/:local deps of `deps-edn-path` into an ordered, de-duplicated
  array of source dirs (the project's own :paths first, then each dependency's,
  transitively). `tree` is where jpm's clone cache lives (default ./jpm_tree).
  `aliases` (keywords) pull :extra-paths/:extra-deps from the merged config's
  :aliases. The user-level deps.edn (see load-config) merges under the project."
  [deps-edn-path &opt tree aliases]
  (default tree (default-tree))
  (mkdirs tree)
  (ensure-jpm-config tree)
  (def edn (load-config deps-edn-path))
  (def extra (combine-aliases edn aliases))
  (def roots @[])
  (def seen @{})   # lib name -> chosen coordinate (for conflict reporting)
  (defn add-root [r] (unless (index-of r roots) (array/push roots r)))
  (defn coord-str [spec]
    (cond
      (and (dictionary? spec) (get spec :local/root))
        (string ":local/root " (get spec :local/root))
      (and (dictionary? spec) (get spec :git/url))
        (string (get spec :git/url) " @ " (or (get spec :git/sha) (get spec :git/tag)))
      (string/format "%j" spec)))
  (defn coord= [a b]
    (and (deep= (get a :local/root) (get b :local/root))
         (deep= (get a :git/url) (get b :git/url))
         (deep= (get a :git/sha) (get b :git/sha))
         (deep= (get a :git/tag) (get b :git/tag))
         (deep= (get a :jpm/module) (get b :jpm/module))))
  (def queue @[])
  (defn discover [lib spec base-dir]
    (def k (sym-name lib))
    (if-let [prev (get seen k)]
      (unless (coord= prev spec)
        (eprintf "WARNING: %s: conflicting coordinates — using %s, ignoring %s"
                 k (coord-str prev) (coord-str spec)))
      (do
        (put seen k spec)
        (when (and (dictionary? spec) (get spec :jpm/module))
          (ensure-jpm-dep lib spec))
        (def dir
          (cond
            (and (dictionary? spec) (get spec :git/url))
              # :deps/root (tools.deps): the project lives in a subdirectory
              # of the repo — monorepos like ring-clojure/ring.
              (let [cloned (clone-git spec)
                    root (get spec :deps/root)]
                (if root (string cloned "/" root) cloned))
            (and (dictionary? spec) (get spec :local/root))
              (let [lr (get spec :local/root)]
                (if (string/has-prefix? "/" lr) lr (string base-dir "/" lr)))
            nil))  # :mvn/* and anything else: skip
        (when dir
          (def dep-edn (read-edn (string dir "/deps.edn")))
          (each r (src-roots dir dep-edn) (add-root r))
          (array/push queue [dep-edn dir])))))
  # the project's own paths (+ alias extra paths) lead the roots
  (each r (src-roots (os/cwd) edn) (add-root r))
  (each pp (extra :extra-paths) (add-root (string (os/cwd) "/" pp)))
  # breadth-first: every top-level dep registers before any transitive dep —
  # so a top-level coordinate always wins, matching tools.deps. Alias
  # :extra-deps go first: a selected alias's pin beats the project's.
  (eachp [lib spec] (extra :extra-deps)
    (discover lib spec (os/cwd)))
  (eachp [lib spec] (or (and (dictionary? edn) (get edn :deps)) {})
    (discover lib spec (os/cwd)))
  (while (> (length queue) 0)
    (def [dep-edn dir] (get queue 0))
    (array/remove queue 0)
    (when (dictionary? dep-edn)
      (eachp [lib spec] (or (get dep-edn :deps) {})
        (discover lib spec dir))))
  roots)

(defn resolve-deps-cached
  "Like resolve-deps, but caches the resolved roots in the tree keyed on a hash
  of the project deps.edn + the user deps.edn + the selected aliases, so an
  unchanged config resolves without re-fetching."
  [deps-edn-path &opt tree aliases]
  (default tree (default-tree))
  (when (os/stat deps-edn-path)
    # the roots depend on the PROJECT (config + aliases), so their cache is
    # project-local like tools.deps' .cpcache; the clone tree stays global
    (os/mkdir ".cpcache")
    (def cache-file ".cpcache/jolt-deps.jdn")
    (def user-path (string (config-dir) "/deps.edn"))
    # The raw key material, not (hash …): janet's hash is seeded per process,
    # so a hashed key never matches across invocations and the cache never hit.
    (def key [(slurp deps-edn-path)
              (or (when (os/stat user-path) (slurp user-path)) "")
              (string/format "%j" (map string (or aliases [])))])
    (def cached (when (os/stat cache-file) (try (parse (slurp cache-file)) ([_] nil))))
    (if (and cached (deep= key (get cached :key)))
      (get cached :roots)
      (let [roots (resolve-deps deps-edn-path tree aliases)]
        (spit cache-file (string/format "%j" {:key key :roots roots}))
        roots))))

(defn project-source-roots
  "The project's OWN source roots — its deps.edn :paths plus any alias
  :extra-paths, joined to cwd — as opposed to dependency roots. These are the
  'app' namespaces: the runtime scopes the whole-program inference fixpoint to
  them (JOLT_APP_PATHS) so a dep-heavy app's startup doesn't re-infer every
  transitive dependency namespace (jolt-87e)."
  [deps-edn-path &opt aliases]
  (def out @[])
  (when (os/stat deps-edn-path)
    (def edn (load-config deps-edn-path))
    (def extra (combine-aliases edn aliases))
    (each r (src-roots (os/cwd) edn) (array/push out r))
    (each pp (extra :extra-paths) (array/push out (string (os/cwd) "/" pp))))
  out)

# --- :tasks (the honest subset of babashka's) ----------------------------------
# A STRING task is a shell command. A MAP task carries :main-opts (jolt args —
# `-e "(...)"` covers expression tasks) and an optional :doc. Babashka-style
# bare-expression tasks aren't supported: the reader hands us parsed data, and
# round-tripping it back to source isn't worth the fragility.

(defn tasks
  "Sorted [name doc] pairs from the merged user+project :tasks."
  [deps-edn-path]
  (def m (get (load-config deps-edn-path) :tasks))
  (def names (sort (keys (or m @{}))))
  (map (fn [n]
         (def v (get m n))
         [n (when (dictionary? v) (get v :doc))])
       names))

(defn task-spec
  "What running task `name` means: {:type :shell :cmd s} or
  {:type :jolt :argv [...]}; nil when undefined."
  [deps-edn-path name]
  (def v (get (or (get (load-config deps-edn-path) :tasks) @{}) name))
  (cond
    (nil? v) nil
    (or (string? v) (buffer? v)) {:type :shell :cmd (string v)}
    (and (dictionary? v) (get v :main-opts))
      {:type :jolt :argv (tuple ;(map string (get v :main-opts)))}
    (error (string "task " name ": use a shell string or {:main-opts [...]}"))))
