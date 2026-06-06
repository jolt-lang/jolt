# deps.edn resolution for Jolt.
#
# Resolve git and :local/root dependencies from a deps.edn into a list of source
# directories, which the loader then searches (see evaluator/find-ns-file). We
# reuse jpm's git fetch + cache (jpm/pm) rather than shipping a package manager.
# Maven (:mvn/version) deps are ignored — git only, pure clj/cljc only.
#
# jpm is loaded lazily (require, not import) so it's needed only at resolve time
# (dev/build), never embedded in the shipped binary.

# A typical deps.edn is also valid Janet data, so we read it with Janet's parser.
# (EDN-only forms — #{} sets, tagged literals, namespaced maps — aren't handled;
# deps.edn rarely uses them in :deps/:paths.)
(defn read-edn [path]
  (when (os/stat path)
    (try (parse (slurp path)) ([_] nil))))

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
  (def b (resolve-bundle {:url (get spec :git/url)
                          :sha (get spec :git/sha)
                          :tag (get spec :git/tag)
                          :shallow false}))
  (download-bundle (b :url) (b :type) (b :tag) (b :shallow)))

(defn- src-roots
  "Source dirs of a project/dep at `dir`: its deps.edn :paths joined to dir
  (default [\"src\"])."
  [dir edn]
  (map |(string dir "/" $) (or (and edn (get edn :paths)) ["src"])))

(defn resolve-deps
  "Resolve the git/:local deps of `deps-edn-path` into an ordered, de-duplicated
  array of source dirs (the project's own :paths first, then each dependency's,
  transitively). `tree` is where jpm's clone cache lives (default ./jpm_tree)."
  [deps-edn-path &opt tree]
  (default tree (string (os/cwd) "/jpm_tree"))
  (os/mkdir tree)
  (ensure-jpm-config tree)
  (def roots @[])
  (def seen @{})
  (defn add-root [r] (unless (index-of r roots) (array/push roots r)))
  (defn process [edn base-dir own-paths?]
    (when (dictionary? edn)
      (when own-paths? (each r (src-roots base-dir edn) (add-root r)))
      (eachp [lib spec] (or (get edn :deps) {})
        (def k (string lib))
        (unless (get seen k)
          (put seen k true)
          (def dir
            (cond
              (and (dictionary? spec) (get spec :git/url)) (clone-git spec)
              (and (dictionary? spec) (get spec :local/root))
                (let [lr (get spec :local/root)]
                  (if (string/has-prefix? "/" lr) lr (string base-dir "/" lr)))
              nil))  # :mvn/* and anything else: skip
          (when dir
            (def dep-edn (read-edn (string dir "/deps.edn")))
            (each r (src-roots dir dep-edn) (add-root r))
            (process dep-edn dir false))))))
  (process (read-edn deps-edn-path) (os/cwd) true)
  roots)

(defn resolve-deps-cached
  "Like resolve-deps, but caches the resolved roots in the tree keyed on a hash
  of the deps.edn, so an unchanged deps.edn resolves without re-fetching."
  [deps-edn-path &opt tree]
  (default tree (string (os/cwd) "/jpm_tree"))
  (when (os/stat deps-edn-path)
    (os/mkdir tree)
    (def cache-file (string tree "/.jolt-deps-roots.jdn"))
    (def h (hash (slurp deps-edn-path)))
    (def cached (when (os/stat cache-file) (try (parse (slurp cache-file)) ([_] nil))))
    (if (and cached (= h (get cached :hash)))
      (get cached :roots)
      (let [roots (resolve-deps deps-edn-path tree)]
        (spit cache-file (string/format "%j" {:hash h :roots roots}))
        roots))))
