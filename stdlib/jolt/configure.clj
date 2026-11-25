(ns jolt.configure
  "Find a project's native libraries on this machine and record where they are
  in its local override file (deps.local.edn) — what ./configure does for a C
  project, for the :jolt/native libraries a jolt project and its dependencies
  declare.

  A spec in deps.edn names a library portably (`{:name \"sqlite3\" :linux
  [\"libsqlite3.so.0\"]}`); where that file actually is differs per machine: a
  Homebrew prefix, a Nix store path, /opt, a distro's multiarch directory, a
  library built into ~/.local. jolt.deps merges deps.local.edn over deps.edn and
  its :jolt/native specs override the keys they state for a library of the same
  :name — declared by the project or by any dependency — so a found path written
  there is what the next run loads, and the file stays out of version control.

  Run it from a task:

    :tasks {configure {:exec-fn jolt.configure/configure!}}

  then `jolt run configure` (flags: --all --dry-run --no-verify --file PATH,
  and library names to limit it to). Or call `find-library` / `discover`
  directly.

  Where it looks, in order, for each library — all of it read by the runtime
  itself, without running pkg-config, ldconfig or brew:
    1. the libdir (and -L directories) of the pkg-config .pc file for the spec's
       :pkg-config (a string or a list of package names; the :name, then
       lib<name>, by default), found on $PKG_CONFIG_PATH and the usual prefixes
    2. Homebrew's opt/<formula>/lib for the spec's :brew (macOS; the :name)
    3. the spec's :search directories (relative to the deps.edn declaring it)
    4. $JOLT_LIBRARY_PATH, $LD_LIBRARY_PATH / $DYLD_LIBRARY_PATH /
       $DYLD_FALLBACK_LIBRARY_PATH, $LIBRARY_PATH (and $PATH on Windows), the
       dynamic linker's configured directories (/etc/ld.so.conf and its
       includes), then the usual system directories.
  A file is taken only if it opens (dlopen), so a library for another
  architecture, or one whose own dependencies are missing, is passed over."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [jolt.deps :as deps]))

;; --- platform ------------------------------------------------------------------

(defn platform
  "The :jolt/native key the running platform reads: :darwin, :linux or :windows."
  []
  (let [os (str/lower-case (or (System/getProperty "os.name") ""))]
    (cond (str/includes? os "mac") :darwin
          (str/includes? os "win") :windows
          :else :linux)))

(defn- getenv [k] (let [v (System/getenv k)] (when-not (str/blank? v) v)))

(defn- path-list [v]
  (when v (remove str/blank? (str/split v (if (= :windows (platform)) #";" #":")))))

(defn- dir? [d] (and d (.isDirectory (java.io.File. ^String d))))

(defn- file? [f] (and f (.isFile (java.io.File. ^String f))))

(defn- list-dir [d]
  (when (dir? d) (seq (.list (java.io.File. ^String d)))))

;; --- naming --------------------------------------------------------------------

(defn- library-name? [n]
  (and (string? n) (seq n) (re-matches #"[A-Za-z0-9_.+-]+" n)))

(defn- quote-re [s] (str/replace s #"[.+\-]" #(str "\\" %)))

(defn- file-pattern
  "The file names a shared library NAME goes by on PLAT."
  [plat nm]
  (let [n (quote-re (str/replace nm #"^lib" ""))]
    (re-pattern
     (case plat
       :darwin  (str "lib" n "(\\.[0-9]+)*\\.dylib")
       :windows (str "(?i)(lib)?" n "([-_.][0-9A-Za-z_.-]*)?\\.dll")
       (str "lib" n "\\.so(\\.[0-9]+)*")))))

(defn- version-key [f]
  (mapv parse-long (re-seq #"[0-9]+" f)))

(defn- version-depth [f]
  (count (re-seq #"[0-9]+" (str/replace f #"^(lib)?[^.]*" ""))))

(defn- rank
  "FILES best first: one the spec names among its candidates, then the plain
  unversioned name, then the soname (libfoo.so.1 over the libfoo.so.1.2.3 it
  links to, so an upgrade of the library keeps the recorded path), then the
  newest version."
  [files preferred]
  (let [preferred (set preferred)]
    (sort-by (fn [f] [(if (preferred f) 0 1) (version-depth f)])
             (sort-by version-key #(compare %2 %1) (distinct files)))))

(defn- basename [p] (last (str/split p #"[/\\]")))

(defn- files-in [dir pat preferred]
  (map #(str dir "/" %) (rank (filter #(re-matches pat %) (list-dir dir)) preferred)))

;; --- sources -------------------------------------------------------------------
;; Everything here is read from the filesystem by the runtime itself: pkg-config's
;; .pc files, the dynamic linker's configuration and Homebrew's opt/ links are
;; plain text and symlinks, so no pkg-config, ldconfig or brew has to be installed
;; (or spawned) for discovery to work, and the answer is the same in a container
;; with none of them.

(defn- multiarch-dirs [root]
  (when (= :linux (platform))
    (keep #(when (str/includes? % "-linux-") (str root "/" %)) (list-dir root))))

(defn pkg-config-dirs
  "The directories pkg-config reads .pc files from: $PKG_CONFIG_PATH, then
  $PKG_CONFIG_LIBDIR or the usual prefixes' lib/pkgconfig and share/pkgconfig."
  []
  (let [home (getenv "HOME")
        prefixes (concat ["/usr" "/usr/local" "/opt/homebrew" "/opt/local"]
                         (when home [(str home "/.local") (str home "/.nix-profile")])
                         ["/run/current-system/sw"])]
    (distinct
     (filter dir?
             (concat (path-list (getenv "PKG_CONFIG_PATH"))
                     (or (seq (path-list (getenv "PKG_CONFIG_LIBDIR")))
                         (for [pre prefixes
                               d (concat [(str pre "/lib/pkgconfig") (str pre "/lib64/pkgconfig")]
                                         (map #(str % "/pkgconfig") (multiarch-dirs (str pre "/lib")))
                                         [(str pre "/share/pkgconfig")])]
                           d)))))))

(defn- expand-vars [s vars]
  (loop [s s n 0]
    (let [s' (str/replace s #"\$\{([^}]+)\}" (fn [[_ k]] (get vars k "")))]
      (if (or (= s s') (> n 16)) s' (recur s' (inc n))))))

(defn parse-pc
  "A pkg-config .pc file's text as {:vars {name value} :fields {\"Libs\" …}},
  with ${var} references expanded."
  [text]
  (reduce (fn [{:keys [vars] :as acc} line]
            (let [line (str/trim (str/replace line #"#.*" ""))]
              (if-let [[_ k v] (re-matches #"([A-Za-z0-9_.]+)\s*=\s*(.*)" line)]
                (assoc-in acc [:vars k] (expand-vars v vars))
                (if-let [[_ k v] (re-matches #"([A-Za-z0-9_.]+)\s*:\s*(.*)" line)]
                  (assoc-in acc [:fields k] (expand-vars v vars))
                  acc))))
          {:vars {} :fields {}}
          (str/split-lines text)))

(defn- words [s] (vec (remove str/blank? (str/split (or s "") #"\s+"))))

(defn pkg-config
  "What pkg-config would answer for PKG — {:file :version :libdir :includedir
  :cflags :libs} — read from its .pc file, or nil when there is none."
  [pkg]
  (when-let [file (some #(let [f (str % "/" pkg ".pc")] (when (file? f) f))
                        (pkg-config-dirs))]
    (let [{:keys [vars fields]} (parse-pc (slurp file))]
      {:file file
       :version (get fields "Version")
       :libdir (get vars "libdir")
       :includedir (get vars "includedir")
       :cflags (words (get fields "Cflags"))
       :libs (words (get fields "Libs"))})))

(defn- names [v dflt]
  (cond (string? v) [v] (sequential? v) (vec v) :else [dflt]))

(defn- pc-lib-dirs [{:keys [name] :as spec}]
  (for [pkg (distinct (concat (names (:pkg-config spec) name)
                              (when-not (:pkg-config spec) [(str "lib" name)])))
        :when (and pkg (not= "lib" pkg))
        :let [{:keys [libdir libs]} (pkg-config pkg)]
        d (concat [libdir] (keep #(second (re-matches #"-L(.+)" %)) libs))
        :when (dir? d)]
    d))

(defn- brew-dirs [{:keys [name] :as spec}]
  (when (= :darwin (platform))
    (for [formula (names (:brew spec) name)
          prefix ["/opt/homebrew" "/usr/local"]
          :let [d (str prefix "/opt/" formula "/lib")]
          :when (dir? d)]
      d)))

(defn- brew-all-dirs
  "Every Homebrew formula's lib directory (macOS): a library whose formula is not
  named after it — libcrypto.3.dylib is openssl@3's — is still found."
  []
  (when (= :darwin (platform))
    (for [prefix ["/opt/homebrew" "/usr/local"]
          formula (sort (list-dir (str prefix "/opt")))
          :let [d (str prefix "/opt/" formula "/lib")]
          :when (dir? d)]
      d)))

(defn- glob-files
  "The files an ld.so.conf `include` pattern names (a * in the file part only)."
  [pattern]
  (let [i (str/last-index-of pattern "/")
        dir (if (and i (pos? i)) (subs pattern 0 i) "/")
        pat (re-pattern (str/replace (quote-re (subs pattern (inc (or i -1)))) "*" ".*"))]
    (sort (map #(str dir "/" %) (filter #(re-matches pat %) (list-dir dir))))))

(defn ld-conf-dirs
  "The directories the dynamic linker is configured to search (Linux
  /etc/ld.so.conf and the files it includes) — what `ldconfig -p` caches."
  ([] (ld-conf-dirs "/etc/ld.so.conf" #{}))
  ([file seen]
   (when (and (= :linux (platform)) (file? file) (not (seen file)))
     (mapcat (fn [line]
               (let [line (str/trim (str/replace line #"#.*" ""))]
                 (cond
                   (str/blank? line) nil
                   (str/starts-with? line "include")
                   (mapcat #(ld-conf-dirs % (conj seen file))
                           (glob-files (str/trim (subs line 7))))
                   :else [line])))
             (str/split-lines (slurp file))))))

(defn system-dirs
  "The directories searched after the spec's own hints: the library-path
  variables, the linker's configured directories and the usual system ones."
  []
  (let [home (getenv "HOME")]
    (distinct
     (filter dir?
             (concat (path-list (getenv "JOLT_LIBRARY_PATH"))
                     (path-list (getenv "LD_LIBRARY_PATH"))
                     (path-list (getenv "DYLD_LIBRARY_PATH"))
                     (path-list (getenv "DYLD_FALLBACK_LIBRARY_PATH"))
                     (path-list (getenv "LIBRARY_PATH"))
                     (when (= :windows (platform)) (path-list (getenv "PATH")))
                     (when home [(str home "/.local/lib") (str home "/.nix-profile/lib")])
                     ["/opt/homebrew/lib" "/usr/local/lib" "/opt/local/lib"
                      "/run/current-system/sw/lib"]
                     (ld-conf-dirs)
                     ["/usr/lib64" "/lib64"]
                     (multiarch-dirs "/usr/lib")
                     (multiarch-dirs "/lib")
                     ["/usr/lib" "/lib"])))))

;; --- finding -------------------------------------------------------------------

(defn- loads? [path]
  (boolean (try (jolt.ffi/load-native path) (catch Exception _ nil))))

(defn- spec-root [spec plat]
  (or (get (:jolt.deps/roots spec) plat) (:jolt.deps/root spec)))

(defn- under [root p]
  (if (or (nil? root) (str/starts-with? p "/") (re-find #"^[A-Za-z]:" p)) p (str root "/" p)))

(defn- declared [spec plat]
  (let [c (get spec plat)] (if (string? c) [c] (vec c))))

(defn find-library
  "Where the shared library SPEC names is on this machine: {:path :via} for the
  first file found that opens, else nil. SPEC is a :jolt/native map — or just a
  library name — and may carry :pkg-config, :brew and :search hints. Options:
  :verify? (default true) — false takes the first file found without opening it."
  ([spec] (find-library spec nil))
  ([spec {:keys [verify?] :or {verify? true}}]
   (let [spec (if (string? spec) {:name spec} spec)
         plat (platform)
         nm (:name spec)
         pat (file-pattern plat nm)
         preferred (map basename (declared spec plat))
         ok? (if verify? loads? file?)
         root (spec-root spec plat)
         sources (concat
                  (for [d (pc-lib-dirs spec)] [:pkg-config (files-in d pat preferred)])
                  (for [d (brew-dirs spec)] [:brew (files-in d pat preferred)])
                  (for [d (map #(under root %) (when (:search spec) (names (:search spec) nil)))]
                    [:search (files-in d pat preferred)])
                  (for [d (system-dirs)] [:system (files-in d pat preferred)])
                  (for [d (brew-all-dirs)] [:brew (files-in d pat preferred)]))]
     (some (fn [[via files]]
             (some (fn [f] (when (ok? f) {:path f :via via})) files))
           sources))))

(defn locate
  "The path of the exact library file FILENAME (\"libcrypto.3.dylib\") that
  opens, or nil — what jolt.main.natives falls back to when a :jolt/native
  spec's bare filename does not load as the platform's loader finds it. Looks
  in ROOT (the deps.edn that declared the spec) first, then where find-library
  looks: the spec's :search directories, its pkg-config libdirs, Homebrew, the
  library-path variables, the linker's configured directories and the system
  ones, and finally every Homebrew formula's lib. SPEC is the :jolt/native map
  (or a library name) whose hints steer the search."
  ([spec filename] (locate spec filename nil))
  ([spec filename root]
   (let [spec (if (string? spec) {:name spec} spec)
         dirs (distinct
               (concat (when root [root])
                       (map #(under root %) (when (:search spec) (names (:search spec) nil)))
                       (when (library-name? (:name spec)) (pc-lib-dirs spec))
                       (when (library-name? (:name spec)) (brew-dirs spec))
                       (system-dirs)
                       (brew-all-dirs)))]
     (some (fn [d] (let [f (str d "/" filename)] (when (and (file? f) (loads? f)) f)))
           dirs))))

(defn- declared-loads?
  "Does one of the candidates SPEC already declares for PLAT open?"
  [spec plat]
  (let [cands (declared spec plat)
        cands (if (seq cands)
                (map #(if (re-find #"[/\\]" %) (under (spec-root spec plat) %) %) cands)
                (when (library-name? (:name spec))
                  (jolt.ffi/system-library-candidates (:name spec))))]
    (some loads? cands)))

(defn- discoverable? [spec plat]
  (and (library-name? (:name spec))
       (not (:process spec))
       ;; a :static library is linked into a built binary, not opened, unless the
       ;; spec also names a shared object for this platform
       (or (not (:static spec)) (seq (declared spec plat)))))

(defn discover
  "Look for each of SPECS' libraries on this machine. One result per spec:
  {:name :status :path :via :optional} with :status :found (a path to record),
  :present (what the spec declares already opens — skipped unless :all?) or
  :missing. Options: :all? — look for every library, :verify?, :only — a set
  of names to limit it to."
  [specs {:keys [all? only] :as opts}]
  (let [plat (platform)]
    (for [spec specs
          :when (discoverable? spec plat)
          :when (or (empty? only) (contains? (set only) (:name spec)))]
      (let [base {:name (:name spec) :optional (boolean (:optional spec))}]
        (if (and (not all?) (declared-loads? spec plat))
          (assoc base :status :present)
          (if-let [{:keys [path via]} (find-library spec opts)]
            (assoc base :status :found :path path :via via)
            (assoc base :status :missing)))))))

(defn project-dir
  "The directory jolt was run from (bin/jolt exports it as JOLT_PWD)."
  []
  (or (getenv "JOLT_PWD") (System/getProperty "user.dir") "."))

;; --- recording -----------------------------------------------------------------

(defn- read-local [file]
  (when (file? file)
    (edn/read-string (slurp file))))

(defn merge-found
  "LOCAL (a deps.local.edn map) with each :found result's path recorded as its
  library's candidate for PLAT, keeping every other key and platform."
  [local results plat]
  (let [found (filter #(= :found (:status %)) results)
        specs (vec (:jolt/native local))
        specs (reduce (fn [specs {:keys [name path]}]
                        (if-let [i (first (keep-indexed #(when (= name (:name %2)) %1) specs))]
                          (assoc-in specs [i plat] [path])
                          (conj specs {:name name plat [path]})))
                      specs found)]
    (cond-> (or local {})
      (seq specs) (assoc :jolt/native specs))))

(defn write-local!
  "Record RESULTS' found paths in FILE (the project's deps.local.edn by default),
  merged into what it already holds. Returns the map written."
  ([results] (write-local! results (deps/local-deps-path (project-dir))))
  ([results file]
   (let [m (merge-found (read-local file) results (platform))]
     (spit file (str ";; Local overrides for this machine, merged over deps.edn by jolt.\n"
                     ";; Written by jolt.configure; keep it out of version control.\n"
                     (binding [*print-namespace-maps* false] (with-out-str (pprint/pprint m)))))
     m)))

;; --- entry points --------------------------------------------------------------

(defn project-natives
  "The :jolt/native specs the project at DIR resolves to — its own, its
  dependencies' and its current local overrides, reconciled."
  [dir]
  (:natives (deps/resolve-project dir)))

(defn- report [results file dry-run?]
  (doseq [{:keys [name status path via optional]} results]
    (println (format "  %-20s %s"
                     name
                     (case status
                       :found (str "found    " path " (" (clojure.core/name via) ")")
                       :present "present  (a declared candidate opens)"
                       :missing (str (if optional "missing  (optional)" "MISSING"))))))
  (cond
    (not-any? #(= :found (:status %)) results) nil
    dry-run? (println (str "dry run: " file " not written"))
    :else (println (str "wrote " file))))

(defn configure!
  "Find the project's native libraries and record the ones found in its
  deps.local.edn. The :exec-fn of a configure task; OPTS are its parsed flags:
  :all (look for libraries whose declared candidates already open, too),
  :dry-run, :no-verify, :file (where to write), :dir (the project; the current
  one by default) and :libs (names to limit it to). Exits 1 when a required
  library is missing, so a task chain stops there."
  [{:keys [all dry-run no-verify file dir libs] :as _opts}]
  (let [dir (or dir (project-dir))
        file (or file (deps/local-deps-path dir))
        libs (cond (string? libs) (str/split libs #"[ ,]+") :else libs)
        results (doall (discover (project-natives dir)
                                 {:all? all :verify? (not no-verify) :only (set libs)}))]
    (println (str "jolt.configure: " (count results) " native "
                  (if (= 1 (count results)) "library" "libraries")
                  " on " (name (platform))))
    (when (and (not dry-run) (some #(= :found (:status %)) results))
      (write-local! results file))
    (report results file dry-run)
    (when (some #(and (= :missing (:status %)) (not (:optional %))) results)
      (System/exit 1))
    results))

(defn -main
  "jolt -m jolt.configure [--all] [--dry-run] [--no-verify] [--file PATH] [LIB ...]"
  [& args]
  (loop [args args opts {:libs []}]
    (if-let [[a & more] (seq args)]
      (case a
        "--all" (recur more (assoc opts :all true))
        ("--dry-run" "-n") (recur more (assoc opts :dry-run true))
        "--no-verify" (recur more (assoc opts :no-verify true))
        "--file" (recur (rest more) (assoc opts :file (first more)))
        "--dir" (recur (rest more) (assoc opts :dir (first more)))
        (recur more (update opts :libs conj a)))
      (do (configure! opts) nil))))
