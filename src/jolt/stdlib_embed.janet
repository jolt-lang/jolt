# Bakes the Clojure stdlib (.clj/.cljc under src/jolt/clojure and src/jolt/jolt)
# into the image at build time, so the runtime can load clojure.string, jolt.http,
# jolt.nrepl, etc. from any directory — not just when run from the repo.
#
# `sources` is built at module-load time. During `jpm build` that's the build
# (cwd = repo), so the map is captured into the image and frozen; in the shipped
# binary the files are never read from disk. Running from source rebuilds it.

(defn- relpath->ns [rel]
  # string/replace-all takes the string LAST, so thread with ->>
  (->> rel (string/replace-all "/" ".") (string/replace-all "_" "-")))

(defn- strip-ext [name]
  (cond
    (string/has-suffix? ".cljc" name) (string/slice name 0 (- (length name) 5))
    (string/has-suffix? ".clj" name)  (string/slice name 0 (- (length name) 4))
    name))

(defn- collect [root prefix acc]
  (when (os/stat root)
    (each e (os/dir root)
      (def p (string root "/" e))
      (cond
        (= :directory (os/stat p :mode)) (collect p (string prefix e "/") acc)
        (or (string/has-suffix? ".clj" e) (string/has-suffix? ".cljc" e))
          (put acc (relpath->ns (string prefix (strip-ext e))) (slurp p)))))
  acc)

(def sources
  (let [acc @{}]
    (collect "src/jolt/clojure" "clojure/" acc)
    (collect "src/jolt/jolt" "jolt/" acc)
    acc))
