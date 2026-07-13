(ns babashka.fs
  ;; {:squint/compile-time true} lets squint load only this ns's compile-time
  ;; part (the with-temp-dir defmacro below), instead of evaluating this whole
  ;; namespace in SCI.
  {:squint/compile-time true}
  (:refer-clojure :exclude [exists? slurp spit])
  ;; cljs/shadow self-require this ns's macros so a plain (:require [babashka.fs])
  ;; exposes them; squint reads :squint -> nothing here (it uses the flag above).
  #?@(:squint []
      :cljs [(:require-macros [babashka.fs])])
  (:require #?@(:shadow [[clojure.string :as str]
                         ["node:fs" :as node-fs]
                         ["node:path" :as node-path]
                         ["node:os" :as node-os]
                         ["node:zlib" :as node-zlib]
                         ["node:crypto" :as node-crypto]]
                :squint [[clojure.string :as str]
                         ["node:fs" :as node-fs]
                         ["node:path" :as node-path]
                         ["node:os" :as node-os]
                         ["node:zlib" :as node-zlib]
                         ["node:crypto" :as node-crypto]]
                :cljs [[clojure.string :as str]
                       ["fs" :as node-fs]
                       ["path" :as node-path]
                       ["os" :as node-os]
                       ["zlib" :as node-zlib]
                       ["crypto" :as node-crypto]]
                :default [[clojure.java.io :as io]
                          [clojure.string :as str]
                          [clojure.walk :as walk]]))
  #?@(:cljs []
      :default [(:import [java.io File InputStream BufferedInputStream FileInputStream FileOutputStream]
                         [java.net URI]
                         [java.nio.file StandardOpenOption CopyOption
                          #?@(:bb [] :clj [DirectoryStream DirectoryStream$Filter])
                          Files
                          FileSystems
                          FileVisitOption
                          FileVisitResult
                          StandardCopyOption
                          LinkOption Path
                          FileVisitor]
                         [java.nio.file.attribute BasicFileAttributes FileAttribute FileTime PosixFilePermissions PosixFilePermission]
                         [java.nio.charset Charset]
                         [java.util.zip GZIPInputStream GZIPOutputStream ZipInputStream ZipOutputStream ZipEntry])]))

#?(:clj (set! *warn-on-reflection* true))

;;;; Private helpers

(defn- as-path
  ^Path [path]
  #?(:clj (if (instance? Path path) path
              (if (instance? URI path)
                (java.nio.file.Paths/get ^URI path)
                (.toPath (io/file path))))
     :cljs (str path)))

(defn- as-file
  ^java.io.File [path]
  #?(:clj (if (instance? Path path) (.toFile ^Path path)
              (io/file path))
     :cljs (as-path path)))

(defn- get-env [k]
  #?(:clj (System/getenv k)
     :cljs (unchecked-get (.-env js/process) k)))

#?(:clj
   (def ^:private fvr-lookup
     {:continue FileVisitResult/CONTINUE
      :skip-subtree FileVisitResult/SKIP_SUBTREE
      :skip-siblings FileVisitResult/SKIP_SIBLINGS
      :terminate FileVisitResult/TERMINATE})
   :cljs
   (def ^:private fvr-lookup #{:continue :skip-subtree :skip-siblings :terminate}))

(defn- file-visit-result
  [x]
  #?(:clj (if (instance? FileVisitResult x) x
              (or (fvr-lookup x)
                  (throw (Exception. "Expected: one of :continue, :skip-subtree, :skip-siblings, :terminate."))))
     :cljs (if (fvr-lookup x)
             x
             (throw (ex-info "Expected: one of :continue, :skip-subtree, :skip-siblings, :terminate." {})))))

(defn path
  "Coerces `path`(s) into a `Path`, combining multiple paths into one.
  Multiple-arg versions treat the first argument as parent and subsequent
  args as children relative to the parent."
  (^Path [path] (as-path path))
  #?(:clj (^Path [parent child]
           (if parent
             (if (string? child)
               (.resolve ^Path (as-path parent) ^String child)
               (.resolve ^Path (as-path parent) (as-path child)))
             (as-path child)))
     :cljs ([parent child]
            (if parent
              (let [c (str child)]
                (if (.isAbsolute node-path c)
                  c
                  (.join node-path (str parent) c)))
              (str child))))
  ([parent child & more]
   (reduce path (path parent child) more)))

(def ^:private path* path)

(defn file
  "Coerces `path`(s) into a `File`, combining multiple paths into one.
  Multiple-arg versions treat the first argument as parent and subsequent args
  as children relative to the parent."
  (^File [path] (as-file path))
  ([path & paths]
   #?(:clj (apply io/file (map as-file (cons path paths)))
      :cljs (reduce path* (path* path) paths))))

#?(:clj
   (defn- ->link-opts ^"[Ljava.nio.file.LinkOption;" [nofollow-links]
     (into-array LinkOption
                 (cond-> []
                   nofollow-links
                   (conj LinkOption/NOFOLLOW_LINKS)))))

#?(:cljs
   (defn- stat-path
     ;; JVM Path "" is the cwd; Node statSync("") throws ENOENT, so map "" to ".".
     ;; nil stays "" so statSync throws (JVM throws NPE for nil), not treated as cwd
     [path]
     (let [p (str path)]
       (if (and (= "" p) (some? path)) "." p))))

#?(:cljs
   (defn- stat [path nofollow-links]
     (let [p (stat-path path)]
       (if nofollow-links
         (.lstatSync node-fs p)
         (.statSync node-fs p)))))

#?(:cljs (def ^:private stat-bigint-opts #js {:bigint true}))

#?(:cljs
   (defn- stat-ns [path nofollow-links]
     (let [p (stat-path path)]
       (if nofollow-links
         (.lstatSync node-fs p stat-bigint-opts)
         (.statSync node-fs p stat-bigint-opts)))))

#?(:cljs
   (defn- bigint? [x]
     (and x (identical? js/BigInt (.-constructor x)))))

#?(:cljs (def ^:private fs-constants (.-constants node-fs)))

#?(:cljs
   (defn- access? [path mode]
     (try (.accessSync node-fs (str path) mode) true
          (catch :default _ false))))

#?(:cljs
   (defn- chmod [path mode]
     (.chmodSync node-fs (str path) mode)))

#?(:cljs
   (defn- copy-file [src dest replace-existing]
     (.copyFileSync node-fs (str src) (str dest)
                    (if replace-existing 0 (.-COPYFILE_EXCL fs-constants)))))

#?(:cljs
   (defn- mkdtemp [base prefix]
     (.mkdtempSync node-fs (path base prefix))))

#?(:cljs (def ^:private regex-escape-re (js/RegExp. "[.*+?^${}()|[\\]\\\\]" "g")))

#?(:cljs
   (defn- regex-escape [s]
     (.replace s regex-escape-re "\\$&")))

#?(:cljs (def ^:private ns-per-ms (js/BigInt 1000000)))

(declare absolutize normalize parent file-name win? exists?)

(defn real-path
  "Converts `path` into real path via [Path#toRealPath](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#toRealPath(java.nio.file.LinkOption...)).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path] (real-path path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (.toRealPath (as-path path) (->link-opts nofollow-links))
      :cljs (if nofollow-links
              (if (exists? path {:nofollow-links true})
                (normalize (absolutize path))
                (throw (ex-info (str "File does not exist: " path) {})))
              (.realpathSync node-fs (str path))))))

(defn owner
  "Returns the owner of `path` via [Files/getOwner](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#getOwner(java.nio.file.Path,java.nio.file.LinkOption...)):
  a `UserPrincipal` on the JVM (call `str` on it to get the owner name), the
  numeric `uid` on Node.js, which cannot resolve it to a name.

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path] (owner path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (Files/getOwner (as-path path) (->link-opts nofollow-links))
      :cljs (.-uid (stat path nofollow-links)))))

;;;; Predicates

(defn regular-file?
  "Returns `true` if `path` is a regular file via [Files/isRegularFile](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isRegularFile(java.nio.file.Path,java.nio.file.LinkOption...)).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path] (regular-file? path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (Files/isRegularFile (as-path path) (->link-opts nofollow-links))
      :cljs (try
              (.isFile (stat path nofollow-links))
              (catch :default _ false)))))

(defn directory?
  "Returns `true` if `path` is a directory via [Files/isDirectory](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isDirectory(java.nio.file.Path,java.nio.file.LinkOption...)).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path] (directory? path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (Files/isDirectory (as-path path) (->link-opts nofollow-links))
      :cljs (try
              (.isDirectory (stat path nofollow-links))
              (catch :default _ false)))))

(defn hidden?
  "Returns `true` if `path` is hidden via [Files/isHidden](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isHidden(java.nio.file.Path)).

  TIP: some older JDKs can throw on empty-string path `(hidden \"\")`.
  Consider instead checking cwd via `(hidden \".\")`."
  [path]
  #?(:clj (Files/isHidden (as-path path))
     :cljs (str/starts-with? (file-name path) ".")))

(defn absolute?
  "Returns `true` if `path` is absolute via [Path#isAbsolute](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#isAbsolute())."
  [path]
  #?(:clj (.isAbsolute (as-path path))
     :cljs (.isAbsolute node-path (str path))))

(defn executable?
  "Returns `true` if `path` is executable via [Files/isExecutable](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isExecutable(java.nio.file.Path))."
  [path]
  #?(:clj (Files/isExecutable (as-path path))
     :cljs (access? path (.-X_OK fs-constants))))

(defn readable?
  "Returns `true` if `path` is readable via [Files/isReadable](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isReadable(java.nio.file.Path))"
  [path]
  #?(:clj (Files/isReadable (as-path path))
     :cljs (access? path (.-R_OK fs-constants))))

(defn writable?
  "Returns `true` if `path` is writable via [Files/isWritable](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isWritable(java.nio.file.Path))"
  [path]
  #?(:clj (Files/isWritable (as-path path))
     :cljs (access? path (.-W_OK fs-constants))))

(defn relative?
  "Returns `true` if `path` is relative (in other words, is not [[absolute?]])."
  [path] (not (absolute? path)))

(defn exists?
  "Returns `true` if `path` exists via [Files/exists](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path,java.nio.file.LinkOption...)).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path] (exists? path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (try
             (Files/exists (as-path path) (->link-opts nofollow-links))
             (catch Exception _e false))
      :cljs (try (stat path nofollow-links) true
                 (catch :default _ false)))))

;;;; End predicates

#?(:cljs
   (defn- native-sep [path]
     (cond-> (str path) win? (str/replace "/" (.-sep node-path)))))

(defn components
  "Returns a seq of paths for all components of `path`.
  i.e.: split on the [[file-separator]]."
  [path]
  #?(:clj (seq (as-path path))
     :cljs (if (= "" (str path))
             '("") ; JVM Path "" has a single empty name component
             (seq (remove str/blank? (.split (native-sep path) (.-sep node-path)))))))

(defn absolutize
  "Converts `path` into an absolute path via [Path#toAbsolutePath](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#toAbsolutePath())."
  [path]
  #?(:clj (.toAbsolutePath (as-path path))
     :cljs (let [sep (.-sep node-path)
                 p (native-sep path)]
             (cond
               (= "" p) (.cwd js/process)
               (.isAbsolute node-path p) p
               :else (str (.cwd js/process) sep p)))))

(defn relativize
  "Returns `other-path` relative to `base-path` via [Path#relativize](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#relativize(java.nio.file.Path)).

  Examples:
  - `(fs/relativize \"a/b\" \"a/b/c/d\")` => `c/d`
  - `(fs/relativize \"a/b/c/d\" \"a/b\")` => `../..`"
  [base-path other-path]
  #?(:clj (.relativize (as-path base-path) (as-path other-path))
     :cljs (.relative node-path (str base-path) (str other-path))))

(defn normalize
  "Returns normalized path for `path` via [Path#normalize](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#normalize())."
  [path]
  #?(:clj (.normalize (as-path path))
     :cljs (.normalize node-path (str path))))

(defn canonicalize
  "Returns the canonical path for `path` via [File#getCanonicalPath](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#getCanonicalPath()).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links) - when set, falls back on [[absolutize]] + [[normalize]].

  This function can be used as an alternative to [[real-path]] which requires files to exist."
  ([path] (canonicalize path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (if nofollow-links
             (-> path absolutize normalize)
             (as-path (.getCanonicalPath (as-file path))))
      :cljs (if nofollow-links
              (-> path absolutize normalize)
              (try (.realpathSync node-fs (str path))
                   (catch :default _
                     (str (path* (canonicalize (parent (absolutize path)))
                                 (file-name path)))))))))

(defn root
  "Returns root path for `path`, or `nil`, via [Path#getRoot](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#getRoot()).

  The return value depends upon the runtime platform.

  On Windows, returns Windows specific roots, ex:
  (replace forward slash with backslash):
  * `C:/` for `C:/foo/bar`
  * `C:`  for `C:foo/bar`
  * `//server/share` for `//server/share/foo/bar`

  On Linux and macOS, returns the leading `/` for anything that looks like an absolute path."
  [path]
  #?(:clj (.getRoot (as-path path))
     :cljs (let [r (.-root (.parse node-path (str path)))]
             (when (seq r) r))))

(defn file-name
  "Returns the name of the file or directory for `path` via [File#getName](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#getName()).
  E.g. `(file-name \"foo/bar/baz\")` returns `\"baz\"`."
  [path]
  #?(:clj (.getName (as-file path))
     :cljs (.basename node-path (str path))))

(def ^:private continue (constantly :continue))

(declare list-dir)

(defn walk-file-tree
  "Walks `path` via [Files/walkFileTree](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#walkFileTree(java.nio.file.Path,java.util.Set,int,java.nio.file.FileVisitor)).

  Returns `path`.

  Options:
  * [`:follow-links`](/README.md#follow-links)
  * `:max-depth` - maximum directory depth to walk, defaults is unlimited
  * Override default visitor functions via:
    * `:pre-visit-dir` - args `[dir attrs]`
    * `:post-visit-dir` - args `[dir ex]`
    * `:visit-file` - args `[file attrs]`
    * `:visit-file-failed` - args `[file ex]`

  All visitor functions must return one of `:continue`, `:skip-subtree`, `:skip-siblings` or `:terminate`.
  A different return value will throw. When not supplied, visitor functions default
  to `(constantly :continue)`."
  [path
   {:keys [:pre-visit-dir :post-visit-dir
           :visit-file :visit-file-failed
           :follow-links :max-depth]}]
  #?(:clj
     (let [pre-visit-dir (or pre-visit-dir continue)
           post-visit-dir (or post-visit-dir continue)
           visit-file (or visit-file continue)
           max-depth (or max-depth Integer/MAX_VALUE)
           visit-opts (set (cond-> []
                             follow-links (conj FileVisitOption/FOLLOW_LINKS)))
           visit-file-failed (or visit-file-failed
                                 (fn [_path _attrs]
                                   :continue))]
       (Files/walkFileTree (as-path path)
                           visit-opts
                           max-depth
                           (reify FileVisitor
                             (preVisitDirectory [_ dir attrs]
                               (-> (pre-visit-dir dir attrs)
                                   file-visit-result))
                             (postVisitDirectory [_ dir ex]
                               (-> (post-visit-dir dir ex)
                                   file-visit-result))
                             (visitFile [_ path attrs]
                               (-> (visit-file path attrs)
                                   file-visit-result))
                             (visitFileFailed [_ path ex]
                               (-> (visit-file-failed path ex)
                                   file-visit-result)))))
     :cljs
     (let [pre-visit-dir (or pre-visit-dir continue)
           post-visit-dir (or post-visit-dir continue)
           visit-file (or visit-file continue)
           visit-file-failed (or visit-file-failed continue)
           max-depth (or max-depth js/Infinity)
           nofollow (not follow-links)
           root (str path)
           do-walk (fn do-walk [dir depth seen]
                     (let [rp (when follow-links
                                (try (.realpathSync node-fs (str dir)) (catch :default _ nil)))]
                       (if (and rp (contains? seen rp))
                         (file-visit-result (visit-file-failed dir nil))
                         (let [seen (if rp (conj seen rp) seen)
                               pre (file-visit-result (pre-visit-dir dir nil))]
                           (cond
                             (= :terminate pre) :terminate
                             (= :skip-subtree pre) :continue
                             (= :skip-siblings pre) :skip-siblings
                             :else
                             ;; withFileTypes gives the entry kind without a stat per child
                             (let [[entries err] (try [(.readdirSync node-fs (str dir) #js {:withFileTypes true}) nil]
                                                      (catch :default e [nil e]))]
                               (if (nil? entries)
                                 (file-visit-result (post-visit-dir dir err))
                                 (loop [i 0]
                                   (if (>= i (.-length entries))
                                     (file-visit-result (post-visit-dir dir nil))
                                     (let [^js d (aget entries i)
                                           child (.join node-path (str dir) (.-name d))
                                           cd (inc depth)
                                           sym? (.isSymbolicLink d)
                                           ;; Dirent is lstat-based: a symlink under follow-links
                                           ;; needs a real stat to know if its target is a directory
                                           dir? (if sym?
                                                  (and follow-links (directory? child))
                                                  (.isDirectory d))
                                           cr (cond
                                                (and dir? (< cd max-depth)) (do-walk child cd seen)
                                                (and sym? follow-links (not (exists? child)))
                                                (file-visit-result (visit-file-failed child nil))
                                                :else (file-visit-result (visit-file child nil)))]
                                       (cond
                                         (= :terminate cr) :terminate
                                         (= :skip-siblings cr) (file-visit-result (post-visit-dir dir nil))
                                         :else (recur (inc i)))))))))))))]
       (cond
         (and (directory? root {:nofollow-links nofollow}) (< 0 max-depth))
         (do-walk root 0 #{})
         (exists? root {:nofollow-links nofollow}) (file-visit-result (visit-file root nil))
         :else (file-visit-result (visit-file-failed root nil)))
       root)))

#?(:bb nil :clj
   (defn- directory-stream
     "Returns a stream of all files in `dir`. The caller of this function is
  responsible for closing the stream, e.g. using `with-open`. The stream
  can be consumed as a seq by calling seq on it. Accepts optional [[glob]] string or
  accept function of one argument."
     (^DirectoryStream [dir]
      (Files/newDirectoryStream (as-path dir)))
     (^DirectoryStream [dir glob-or-accept]
      (if (string? glob-or-accept)
        (Files/newDirectoryStream (as-path dir) (str glob-or-accept))
        (let [accept* glob-or-accept]
          (Files/newDirectoryStream (as-path dir)
                                    (reify DirectoryStream$Filter
                                      (accept [_ entry]
                                        (boolean (accept* entry))))))))))

(def ^:private win?
  #?(:clj (-> (System/getProperty "os.name")
              (str/lower-case)
              (str/includes? "win"))
     :cljs (= "win32" (.-platform js/process))))

#?(:cljs
   (defn ^:no-doc glob->regex
     "Compiles glob `pattern` to a RegExp matching JVM `getPathMatcher` semantics.
  Handles `**` (any chars including separator), `*` (any chars except separator),
  `?` (single char except separator), `[abc]`/`[!abc]` char classes, `{a,b}` braces
  and `\\`-escaped metachars. Throws on invalid glob syntax."
     [pattern]
     (let [sep-class (if win? "[^/\\\\]" "[^/]")
           esc regex-escape
           convert-segment
           (fn convert-segment [seg]
             (let [n (.-length seg)]
               (loop [i 0 out ""]
                 (if (>= i n)
                   out
                   (let [c (.charAt seg i)]
                     (cond
                       (= "\\" c) (if (>= (inc i) n)
                                    (throw (ex-info (str "No character to escape in glob pattern: " pattern) {}))
                                    (recur (+ i 2) (str out (esc (.charAt seg (inc i))))))
                       (= "*" c) (recur (inc i) (str out sep-class "*"))
                       (= "?" c) (recur (inc i) (str out sep-class))
                       (= "[" c) (let [end (.indexOf seg "]" (inc i))]
                                   (when (neg? end)
                                     (throw (ex-info (str "Missing ']' in glob pattern: " pattern) {})))
                                   (let [body (subs seg (inc i) end)
                                         body (cond
                                                (str/starts-with? body "!") (str "^" (subs body 1))
                                                (str/starts-with? body "^") (str "\\^" (subs body 1))
                                                :else body)]
                                     (recur (inc end) (str out "[" body "]"))))
                       (= "{" c) (let [end (.indexOf seg "}" (inc i))]
                                   (when (neg? end)
                                     (throw (ex-info (str "Missing '}' in glob pattern: " pattern) {})))
                                   (let [body (subs seg (inc i) end)]
                                     (when (str/includes? body "{")
                                       (throw (ex-info (str "Cannot nest groups in glob pattern: " pattern) {})))
                                     (recur (inc end) (str out "(" (str/join "|" (map convert-segment (.split body ","))) ")"))))
                       :else (recur (inc i) (str out (esc c)))))))))
           ;; split on ** to handle separately, then rejoin with .*
           parts (.split pattern "**")
           regex-str (str/join ".*" (map convert-segment parts))]
       (js/RegExp. (str "^" regex-str "$")))))

;; Not defined in babashka so reload keeps its built-in list-dir: the source impl needs java.nio DirectoryStream, which babashka does not expose to interpreted code.
#?(:bb nil
   :default
   (defn list-dir
     "Returns a vector of all paths in `dir`. For descending into subdirectories use [[glob]].

     - `glob-or-accept` - a [[glob]] string such as `\"*.edn\"` or a `(fn accept [^java.nio.file.Path p]) -> truthy`"
     ([dir]
      #?(:clj (with-open [stream (directory-stream dir)]
                (vec stream))
         :cljs (let [d (str dir)]
                 (mapv #(.join node-path d %) (.readdirSync node-fs d)))))
     ([dir glob-or-accept]
      #?(:clj (with-open [stream (directory-stream dir glob-or-accept)]
                (vec stream))
         :cljs (let [entries (list-dir dir)]
                 (if (string? glob-or-accept)
                   (let [re (glob->regex glob-or-accept)]
                     (filterv #(.test re (file-name %)) entries))
                   (filterv glob-or-accept entries)))))))

#?(:clj
   (defn- path-seq
     [path]
     (tree-seq
      directory?
      list-dir
      (as-path path))))

#?(:cljs
   (defn- regular-files-children
     ;; regular files under dir; entry kinds come from readdir, so only a symlink costs a stat
     [dir]
     (mapcat (fn [^js d]
               (let [child (.join node-path (str dir) (.-name d))]
                 (cond
                   (.isSymbolicLink d) (if (directory? child)
                                         (lazy-seq (regular-files-children child))
                                         (when (regular-file? child) [child]))
                   (.isDirectory d) (lazy-seq (regular-files-children child))
                   (.isFile d) [child]
                   :else nil)))
             (.readdirSync node-fs (str dir) #js {:withFileTypes true}))))

(defn- regular-files
  [path]
  #?(:clj (filter regular-file? (path-seq path))
     :cljs (cond
             (directory? path) (regular-files-children path)
             (regular-file? path) [path]
             :else nil)))

(def file-separator
  "The system-dependent default path component separator character (as string) via [File/separator](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#separator)."
  #?(:clj File/separator
     :cljs (.-sep node-path)))

(def path-separator
  "The system-dependent path-separator character (as string) via [File/pathSeparator](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#pathSeparator)."
  #?(:clj File/pathSeparator
     :cljs (.-delimiter node-path)))

(defn- escape-glob-chars
  "Escapes special glob characters in the input string."
  [s]
  (let [special-chars #{\\ \* \? \[ \] \{ \}}
        escape-char (fn [c]
                      (if (contains? special-chars c)
                        (str "\\" c)
                        (str c)))]
    (apply str (map escape-char s))))

(defn- escape-regex-chars
  "Escapes a string so it can be used literally in a regular expression."
  [s]
  #?(:clj (java.util.regex.Pattern/quote s)
     :cljs (regex-escape s)))

(defn match
  "Returns a vector of paths matching `pattern` (on path and filename) relative to `root-dir`.
  Pattern interpretation is done using the rules described in
  [FileSystem#getPathMatcher](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/FileSystem.html#getPathMatcher(java.lang.String))

  Options:
  * `:hidden` - match hidden paths - note: on Windows paths starting with
  a dot are not hidden, unless their hidden attribute is set. Defaults to
  `false`, i.e. skip hidden files and folders.
  * [`:follow-links`](/README.md#follow-links) - follow symlinks. Defaults to false.
  * `:recursive`
    * `true` - `pattern` is matched against all descendant files and directories under `root-dir`
    * `false` (default) - `pattern` is matched only against immediate children under `root-dir`
  * `:max-depth` - max depth to descend into directory structure, when
  matching recursively. Defaults to `Integer/MAX_VALUE`.

  Examples:
  - `(fs/match \".\" \"regex:.*\\\\.clj\" {:recursive true})`

  See also: [[glob]]"
  ([root-dir pattern] (match root-dir pattern nil))
  ([root-dir pattern {:keys [hidden follow-links max-depth recursive]}]
   (let [[prefix pattern] (str/split pattern #":")
         base-path (-> root-dir absolutize normalize str)
         escaped-base-path (case prefix
                             "glob" (escape-glob-chars base-path)
                             "regex" (escape-regex-chars base-path)
                             base-path)
         skip-hidden? (not hidden)
         results (atom (transient []))
         past-root? (volatile! nil)
         pattern (let [separator (when-not (str/ends-with? base-path file-separator)
                                   (str (when win? "\\")
                                        file-separator))]
                   (str escaped-base-path
                        separator
                        (if win?
                          (str/replace pattern "/" "\\\\")
                          pattern)))
         #?@(:clj [matcher (.getPathMatcher
                            (FileSystems/getDefault)
                            (str prefix ":" pattern))]
             :cljs [matcher (case prefix
                              "glob"
                              (let [re (glob->regex pattern)]
                                (fn [p]
                                  (.test re (if win?
                                              (str/replace p "/" "\\")
                                              p))))
                              "regex"
                              (let [re (js/RegExp. (str "^(?:" pattern ")$"))]
                                (fn [p] (.test re p)))
                              (throw (ex-info (str "Syntax '" prefix "' not recognized") {})))])
         match (fn [path]
                 (when (#?(:clj .matches :cljs matcher) #?(:clj matcher) path)
                   (swap! results conj! #?(:clj path :cljs (str path))))
                 nil)]
     (walk-file-tree
      base-path
      {:max-depth max-depth
       :follow-links follow-links
       :pre-visit-dir (fn [dir _attrs]
                        (if (and @past-root?
                                 (or (not recursive)
                                     (and skip-hidden?
                                          (hidden? dir))))
                          :skip-subtree
                          (do
                            (if @past-root? (match dir)
                                (vreset! past-root? true))
                            :continue)))
       :visit-file (fn [path _attrs]
                     (when-not (and skip-hidden?
                                    (hidden? path))
                       (match path))
                     :continue)})
     (let [results (persistent! @results)
           absolute-cwd (absolutize "")]
       (if (relative? root-dir)
         (mapv #(relativize absolute-cwd %)
               results)
         results)))))

(defn glob
  "Returns a vector of paths matching glob `pattern` (on path and filename) relative to `root-dir`.
  Patterns containing `**` or `/` will cause a recursive walk under
  `root-dir`, unless overriden with `:recursive false`. Similarly, `:hidden` will be automatically enabled
  when `pattern` starts with a dot.
  Glob interpretation is done using the rules described in
  [FileSystem#getPathMatcher](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/FileSystem.html#getPathMatcher(java.lang.String))

  Options:
  * `:hidden` - match hidden paths. Implied `true` when `pattern` starts with a dot;
  otherwise, defaults to `false`. Note: on Windows files starting with a dot are
  not hidden, unless their hidden attribute is set.
  * [`:follow-links`](/README.md#follow-links) - follow symlinks. Defaults to `false`.
  * `:recursive` - implied `true` when `pattern` contains `**` or `/`; otherwise, defaults to `false`.
    * `true` - `pattern` is matched against all descendant files and directories under `root-dir`
    * `false` - `pattern` is matched only against immediate children under `root-dir`
  * `:max-depth` - max depth to descend into directory structure, when
  recursing. Defaults to `Integer/MAX_VALUE`.

  Examples:
  - `(fs/glob \".\" \"**.clj\")` - finds `.clj` files and dirs under `.` dir and its subdirs
  - `(fs/glob \".\" \"**.clj\" {:recursive false})` - finds `.clj` files and dirs immediately under `.` dir only
  - `(fs/glob \".\" \"*.clj\" {:recursive true})` - finds `.clj` files and dirs immediately under `.` only (`pattern` lacks directory wildcards)

  If on macOS, see [note on glob](/README.md#glob)

  See also: [[match]]"
  ([root-dir pattern] (glob root-dir pattern nil))
  ([root-dir pattern opts]
   (let [recursive (:recursive opts
                               (or (str/includes? pattern "**")
                                   (str/includes? pattern file-separator)
                                   (when win?
                                     (str/includes? pattern "/"))))
         hidden (:hidden opts (str/starts-with? pattern "."))]
     (match root-dir (str "glob:" pattern) (assoc opts :recursive recursive :hidden hidden)))))

#?(:clj
   (defn- ->copy-opts ^"[Ljava.nio.file.CopyOption;"
     [replace-existing copy-attributes atomic-move nofollow-links]
     (into-array CopyOption
                 (cond-> []
                   replace-existing (conj StandardCopyOption/REPLACE_EXISTING)
                   copy-attributes (conj StandardCopyOption/COPY_ATTRIBUTES)
                   atomic-move (conj StandardCopyOption/ATOMIC_MOVE)
                   nofollow-links (conj LinkOption/NOFOLLOW_LINKS)))))

(declare sym-link? read-link create-sym-link delete-if-exists same-file?)

#?(:cljs
   (defn- copy-one [src dest {:keys [nofollow-links replace-existing copy-attributes]}]
     (if (and nofollow-links (sym-link? src))
       (do (when replace-existing (delete-if-exists dest))
           (create-sym-link dest (read-link src))
           (when copy-attributes
             (let [st (stat src nofollow-links)]
               (.lutimesSync node-fs dest (.-atime st) (.-mtime st)))))
       (do
         ;; copyFileSync follows a symlink dest; JVM replaces the link itself
         (when (and replace-existing (sym-link? dest))
           (delete-if-exists dest))
         (copy-file src dest replace-existing)
         (when copy-attributes
           (let [st (stat src nofollow-links)]
             (chmod dest (bit-and (.-mode st) 8r7777))
             (.utimesSync node-fs dest (.-atime st) (.-mtime st))))))))

(defn copy
  "Copies `source` file or input-stream to `target-path` dir or file via [Files/copy](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#copy(java.nio.file.Path,java.nio.file.Path,java.nio.file.CopyOption...)).

  Returns copied target file.

  Options:
  * `:replace-existing`
  * `:copy-attributes`
  * [`:nofollow-links`](/README.md#nofollow-links) - used to determine to copy symbolic link itself or not."
  ([source target-path] (copy source target-path nil))
  ([source target-path opts]
   #?(:clj
      (let [{:keys [replace-existing copy-attributes nofollow-links]} opts
            copy-options (->copy-opts replace-existing copy-attributes false nofollow-links)
            dest (as-path target-path)
            dest (if (directory? dest)
                   (path dest (file-name source))
                   dest)
            input-stream? (instance? java.io.InputStream source)]
        (if input-stream?
          (do (Files/copy ^java.io.InputStream source dest copy-options)
              dest)
          (Files/copy (as-path source) dest copy-options)))
      :cljs
      (let [dest (str target-path)
            dest (if (directory? dest)
                   (path dest (file-name source))
                   dest)]
        ;; JVM Files/copy is a no-op for the same file; copyFileSync would throw.
        ;; A symlink dest is not the same file (checked NOFOLLOW), let copy-one handle it
        (when-not (and (exists? dest) (not (sym-link? dest)) (same-file? source dest))
          (copy-one source dest opts))
        dest))))

#?(:cljs (def ^:private octal->rwx ["---" "--x" "-w-" "-wx" "r--" "r-x" "rw-" "rwx"]))

(defn posix->str
  "Converts a set of `PosixFilePermission` `p` to a string, like `\"rwx------\"` via [PosixFilePermissions/toString](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/PosixFilePermissions.html#toString(java.util.Set)).

  See also: [[str->posix]]"
  [p]
  #?(:clj (PosixFilePermissions/toString p)
     :cljs (apply str (for [shift [6 3 0]]
                        (octal->rwx (bit-and (unsigned-bit-shift-right p shift) 7))))))

(defn str->posix
  "Converts string `s` to a set of `PosixFilePermission` via [PosixFilePermissions/fromString](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/PosixFilePermissions.html#fromString(java.lang.String)).

  `s` is a string like `\"rwx------\"`.

  See also: [[posix->str]]"
  [s]
  #?(:clj (PosixFilePermissions/fromString s)
     :cljs (let [parse-triple (fn [t]
                                (+ (if (= \r (nth t 0)) 4 0)
                                   (if (= \w (nth t 1)) 2 0)
                                   (if (= \x (nth t 2)) 1 0)))]
             (+ (* 64 (parse-triple (subs s 0 3)))
                (* 8 (parse-triple (subs s 3 6)))
                (parse-triple (subs s 6 9))))))

(defn- ->posix-file-permissions [s]
  (cond (string? s)
        (str->posix s)
        :else
        s))

#?(:cljs
   (defn- chmod-umasked
     ;; JVM passes posix perms as a create-time FileAttribute, so the OS masks
     ;; them with umask; chmod sets exact bits, mask here to match
     [path posix-file-permissions]
     (chmod path (bit-and (->posix-file-permissions posix-file-permissions)
                          (bit-not (.umask js/process))))))

#?(:clj
   (defn- posix->file-attribute [x]
     (PosixFilePermissions/asFileAttribute x)))

#?(:clj
   (defn- posix->attrs
     ^"[Ljava.nio.file.attribute.FileAttribute;" [posix-file-permissions]
     (let [attrs (if posix-file-permissions
                   (-> posix-file-permissions
                       (->posix-file-permissions)
                       (posix->file-attribute)
                       vector)
                   [])]
       (into-array FileAttribute attrs))))

(defn create-dir
  "Creates `dir` via [Files/createDirectory](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createDirectory(java.nio.file.Path,java.nio.file.attribute.FileAttribute...)).
  Does not create parents.

  Returns `dir`.

  Options:
  * `:posix-file-permissions` - string format for unix-like system permissions for `dir`, as described in [[str->posix]].
  Affected by [umask](/README.md#umask)."
  ([dir]
   (create-dir dir nil))
  ([dir {:keys [:posix-file-permissions]}]
   #?(:clj (let [attrs (posix->attrs posix-file-permissions)]
             (Files/createDirectory (as-path dir) attrs))
      :cljs (do
              (.mkdirSync node-fs (str dir))
              (when posix-file-permissions
                (chmod-umasked dir posix-file-permissions))
              (str dir)))))

#?(:cljs
   (defn- create-dirs*
     ;; create each missing ancestor and apply the perms to it, like JVM
     ;; createDirectories; existing dirs are left untouched
     [dir posix-file-permissions]
     (let [dir (str dir)]
       (when-not (or (= "" dir) (exists? dir))
         (when-let [p (parent dir)]
           (create-dirs* p posix-file-permissions))
         (.mkdirSync node-fs dir)
         (when posix-file-permissions
           (chmod-umasked dir posix-file-permissions))))))

(defn create-dirs
  "Creates `dir` via [Files/createDirectories](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createDirectories(java.nio.file.Path,java.nio.file.attribute.FileAttribute...)).
  Also creates parents if needed.
  Does not throw an exception if the dirs exist already. Similar to `mkdir -p` shell command.

  Returns `dir`.

  Options:
  * `:posix-file-permissions` - string format for unix-like system permissions for `dir`, as described in [[str->posix]].
  Affected by [umask](/README.md#umask)."
  ([dir] (create-dirs dir nil))
  ([dir {:keys [:posix-file-permissions]}]
   #?(:clj (let [p (as-path dir)]
             (if (directory? p)
               p
               (Files/createDirectories (as-path dir) (posix->attrs posix-file-permissions))))
      :cljs (do
              (create-dirs* dir posix-file-permissions)
              (str dir)))))

(defn set-posix-file-permissions
  "Sets `posix-file-permissions` on `path` via [Files/setPosixFilePermissions](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#setPosixFilePermissions(java.nio.file.Path,java.util.Set)).
  Accepts a string like `\"rwx------\"` or a set of `PosixFilePermission`.

  Returns `path`.

  See also: [[posix-file-permissions]]"
  [path posix-file-permissions]
  #?(:clj (Files/setPosixFilePermissions (as-path path) (->posix-file-permissions posix-file-permissions))
     :cljs (do
             (chmod path (->posix-file-permissions posix-file-permissions))
             (str path))))

(defn posix-file-permissions
  "Returns POSIX permissions for `path`: a set of `PosixFilePermission` on the
  JVM, the permission bits as an integer (e.g. `0755`) on Node.js.
  Use [[posix->str]] to convert to a string.

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)

  See also: [[set-posix-file-permissions]]"
  ([path] (posix-file-permissions path nil))
  ([path {:keys [:nofollow-links]}]
   #?(:clj (Files/getPosixFilePermissions (as-path path) (->link-opts nofollow-links))
      :cljs (bit-and (.-mode (stat path nofollow-links)) 8r777))))

(defn- u+wx
  [f]
  #?(:clj (if win?
            (.setWritable (file f) true)
            (let [^java.util.Set perms (posix-file-permissions f)
                  p1 (.add perms PosixFilePermission/OWNER_WRITE)
                  p2 (.add perms PosixFilePermission/OWNER_EXECUTE)]
              (when (or p1 p2)
                (set-posix-file-permissions f perms))))
     :cljs (chmod f (bit-or (posix-file-permissions f) 8r300))))

(defn starts-with?
  "Returns `true` if `this-path` starts with `other-path` via [Path#startsWith](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#startsWith(java.nio.file.Path)).

  See also: [[ends-with?]]"
  [this-path other-path]
  #?(:clj (.startsWith (as-path this-path) (as-path other-path))
     :cljs (let [a (str this-path)
                 b (str other-path)]
             (if (= "" b)
               (= "" a)
               (let [ac (vec (components a))
                     bc (vec (components b))]
                 (and (= (.isAbsolute node-path a) (.isAbsolute node-path b))
                      (<= (count bc) (count ac))
                      (= bc (vec (take (count bc) ac)))))))))

(defn ends-with?
  "Returns `true` if `this-path` ends with `other-path` via [Path#endsWith](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#endsWith(java.nio.file.Path)).

  See also: [[starts-with?]]"
  [this-path other-path]
  #?(:clj (.endsWith (as-path this-path) (as-path other-path))
     :cljs (let [a (str this-path)
                 b (str other-path)]
             (if (= "" b)
               (= "" a)
               (let [ac (vec (components a))
                     bc (vec (components b))]
                 (if (.isAbsolute node-path b)
                   (and (.isAbsolute node-path a) (= ac bc))
                   (and (<= (count bc) (count ac))
                        (= bc (vec (drop (- (count ac) (count bc)) ac))))))))))

(defn copy-tree
  "Copies entire file tree from `source-dir` to `target-dir`. Creates `target-dir` if needed.

  Returns `target-dir`.

  Options:
  * same as [[copy]]
  * `:posix-file-permissions` - string format unix-like system permissions passed to [[create-dirs]] when creating `target-dir`."
  ([source-dir target-dir] (copy-tree source-dir target-dir nil))
  ([source-dir target-dir {:keys [:replace-existing
                                  :copy-attributes
                                  :nofollow-links]
                           :as opts}]
   #?(:clj
      (let [target-dir (as-path target-dir)]
        (when-not (directory? source-dir opts)
          (throw (IllegalArgumentException. (str "Not a directory: " source-dir))))
        (when (and (exists? target-dir opts)
                   (not (directory? target-dir opts)))
          (throw (IllegalArgumentException. (str "Not a directory: " target-dir))))
        (let [csrc (canonicalize source-dir)
              cdest (canonicalize target-dir)]
          (when (not= csrc cdest)
            (when (starts-with? cdest csrc)
              (throw (Exception. (format "Cannot copy src directory: %s, under itself to dest: %s"
                                         (str source-dir) (str target-dir)))))
            (create-dirs target-dir opts)
            (let [copy-options (->copy-opts replace-existing copy-attributes false nofollow-links)
                  link-options (->link-opts nofollow-links)
                  from (real-path source-dir {:nofollow-links nofollow-links})
                  to (canonicalize target-dir {:nofollow-links nofollow-links})]
              (walk-file-tree from {:pre-visit-dir (fn [dir _attrs]
                                                     (let [rel (relativize from dir)
                                                           to-dir (path to rel)]
                                                       (when-not (Files/exists to-dir link-options)
                                                         (Files/copy ^Path dir to-dir
                                                                     ^"[Ljava.nio.file.CopyOption;"
                                                                     copy-options)
                                                         (when-not win?
                                                           (u+wx to-dir))))
                                                     :continue)
                                    :visit-file (fn [from-path _attrs]
                                                  (let [rel (relativize from from-path)
                                                        to-file (path to rel)]
                                                    (Files/copy ^Path from-path to-file
                                                                ^"[Ljava.nio.file.CopyOption;"
                                                                copy-options)
                                                    :continue)
                                                  :continue)
                                    :post-visit-dir (fn [dir _ex]
                                                      (let [rel (relativize from dir)
                                                            to-dir (path to rel)]
                                                        (when-not win?
                                                          (let [perms (posix-file-permissions (file dir))]
                                                            (Files/setPosixFilePermissions to-dir perms)))
                                                        :continue))}))))
        target-dir)
      :cljs
      (let [src (str source-dir)
            dst (str target-dir)]
        (when-not (directory? src opts)
          (throw (ex-info (str "Not a directory: " src) {})))
        (when (and (exists? dst opts) (not (directory? dst opts)))
          (throw (ex-info (str "Not a directory: " dst) {})))
        (let [csrc (canonicalize src {:nofollow-links true})
              cdest (canonicalize dst {:nofollow-links true})]
          (when (not= csrc cdest)
            (when (starts-with? cdest csrc)
              (throw (ex-info (str "Cannot copy src directory: " src
                                   ", under itself to dest: " dst) {})))
            (create-dirs dst opts)
            (let [from (real-path src {:nofollow-links nofollow-links})]
              (walk-file-tree from
                              {:pre-visit-dir (fn [dir _]
                                                (let [rel (relativize from dir)
                                                      to-dir (path dst rel)]
                                                  (when-not (exists? to-dir)
                                                    (create-dir to-dir)
                                                    (when-not win?
                                                      (u+wx to-dir)))
                                                  :continue))
                               :visit-file (fn [f _]
                                             (copy-one f (path dst (relativize from f)) opts)
                                             :continue)
                               :post-visit-dir (fn [dir _]
                                                 (when-not win?
                                                   (let [mode (posix-file-permissions dir)]
                                                     (chmod (path dst (relativize from dir)) mode)))
                                                 :continue)}))))
        dst))))

(defn temp-dir
  "Returns `java.io.tmpdir` property as path."
  []
  #?(:clj (as-path (System/getProperty "java.io.tmpdir"))
     :cljs (.tmpdir node-os)))

(defn create-temp-dir
  "Returns path to directory created via [Files/createTempDirectory](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createTempDirectory(java.nio.file.Path,java.lang.String,java.nio.file.attribute.FileAttribute...)).

  This function does not set up any automatic deletion of the directories it
  creates. See [[with-temp-dir]] for that functionality.

  Options:
  * `:dir` - directory in which to create the new directory. Defaults to default
  system temp dir (e.g. `/tmp`); see [[temp-dir]]. Must already exist.
  * `:prefix` - provided as a hint to the process that generates the name of the
  new directory. In most cases, this will be the beginning of the new directory
  name. Defaults to a random (v4) UUID.
  * `:posix-file-permissions` - string format unix-like system permissions as described in [[str->posix]] for new directory.
  If not specified, uses the file system default permissions for new directories.
  Affected by [umask](/README.md#umask).
  * :warning: `:path` - **[DEPRECATED]** previous name for `:dir`, kept
  for backwards compatibility. If both `:path` and `:dir` are given (don't do
  that!), `:dir` is used.

  Examples:
  * `(create-temp-dir)`
  * `(create-temp-dir {:posix-file-permissions \"rwx------\"})`
  * `(create-temp-dir {:dir (path (cwd) \"_workdir\") :prefix \"process-1-\"})`
  "
  ([] (create-temp-dir {}))
  ([{:keys [:dir :prefix :posix-file-permissions] :as opts}]
   #?(:clj (let [attrs (posix->attrs posix-file-permissions)
                 prefix (or prefix (str (java.util.UUID/randomUUID)))
                 dir (or dir (:path opts))]
             (if dir
               (Files/createTempDirectory (as-path dir) prefix attrs)
               (Files/createTempDirectory prefix attrs)))
      :cljs (let [base (str (or dir (:path opts) (temp-dir)))
                  pre (or prefix (.randomUUID node-crypto))
                  result (mkdtemp base pre)]
              (when posix-file-permissions
                (chmod-umasked result posix-file-permissions))
              result))))

(defn create-temp-file
  "Returns path to empty file created via [Files/createTempFile](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createTempFile(java.nio.file.Path,java.lang.String,java.lang.String,java.nio.file.attribute.FileAttribute...)).

  This function does not set up any automatic deletion of the files it
  creates. Create the file in a [[with-temp-dir]] for that functionality.

  Options:
  * `:dir` - directory in which to create the new file. Defaults to default
  system temp dir (e.g. `/tmp`); see [[temp-dir]]. Must already exist.
  * `:prefix` - provided as a hint to the process that generates the name of the
  new file. In most cases, this will be the beginning of the new file name.
  Defaults to a random (v4) UUID.
  * `:suffix` - provided as a hint to the process that generates the name of the
  new file. In most cases, this will be the end of the new file name.
  Defaults to a random (v4) UUID.
  * `:posix-file-permissions` - string format unix-like system permissions for new file, as described in [[str->posix]].
  If not specified, uses the file system default permissions for new files.
  Affected by [umask](/README.md#umask).
  * :warning: `:path` - **[DEPRECATED]** Previous name for `:dir`, kept
  for backwards compatibility. If both `:path` and `:dir` are given (don't do
  that!), `:dir` is used.

  Examples:
  * `(create-temp-file)`
  * `(create-temp-file {:posix-file-permissions \"rw-------\"})`
  * `(create-temp-file {:dir (path (cwd) \"_workdir\") :prefix \"process-1-\" :suffix \"-queue\"})`
  "
  ([] (create-temp-file {}))
  ([{:keys [:dir :prefix :suffix :posix-file-permissions] :as opts}]
   #?(:clj (let [attrs (posix->attrs posix-file-permissions)
                 prefix (or prefix (str (java.util.UUID/randomUUID)))
                 suffix (or suffix (str (java.util.UUID/randomUUID)))
                 dir (or dir (:path opts))]
             (if dir
               (Files/createTempFile (as-path dir) prefix suffix attrs)
               (Files/createTempFile prefix suffix attrs)))
      :cljs (let [base (str (or dir (:path opts) (temp-dir)))
                  pre (or prefix (.randomUUID node-crypto))
                  suf (or suffix (.randomUUID node-crypto))]
              (loop [tries 0]
                (let [rand (.toString (.randomBytes node-crypto 8) "hex")
                      result (.join node-path base (str pre rand suf))
                      ok (try (.writeFileSync node-fs result "" #js {:flag "wx"})
                              true
                              (catch :default _ false))]
                  (cond
                    ;; writeFileSync leaves 0644; JVM createTempFile defaults to
                    ;; an exact 0600, unaffected by umask
                    ok (do (if posix-file-permissions
                             (chmod-umasked result posix-file-permissions)
                             (chmod result 8r600))
                           result)
                    (< tries 100) (recur (inc tries))
                    :else (throw (ex-info (str "Could not create temp file in: " base) {})))))))))

(defn create-sym-link
  "Creates a symbolic `link` to `target-path` via [Files/createSymbolicLink](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createSymbolicLink(java.nio.file.Path,java.nio.file.Path,java.nio.file.attribute.FileAttribute...)).

  Returns `link`.

  As of this writing, JDKs do not recognize empty-string `target-path` `\"\"` as the cwd.
  Consider instead using a `target-path` of `\".\"` to link to the cwd."
  [link target-path]
  #?(:clj (Files/createSymbolicLink
           (as-path link)
           (as-path target-path)
           (make-array FileAttribute 0))
     :cljs (do
             (.symlinkSync node-fs (str target-path) (str link))
             (str link))))

(defn create-link
  "Creates a new hard `link` (directory entry) for an `existing-file` via [Files/createLink](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createLink(java.nio.file.Path,java.nio.file.Path)).

  Returns `link`."
  [link existing-file]
  #?(:clj (Files/createLink (as-path link) (as-path existing-file))
     :cljs (do
             (.linkSync node-fs (str existing-file) (str link))
             (str link))))

(defn read-link
  "Returns the immediate target of `sym-link-path` via [Files/readSymbolicLink](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#readSymbolicLink(java.nio.file.Path)).
  The target need not exist."
  [sym-link-path]
  #?(:clj (java.nio.file.Files/readSymbolicLink (as-path sym-link-path))
     :cljs (.readlinkSync node-fs (str sym-link-path))))

(defn delete
  "Deletes `path` via [Files/delete](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#delete(java.nio.file.Path)).
  Returns `nil` if the delete was successful,
  throws otherwise. Does not follow symlinks."
  [path]
  #?(:clj (Files/delete (as-path path))
     :cljs (let [p (str path)]
             (if (directory? path {:nofollow-links true})
               (.rmdirSync node-fs p)
               (.unlinkSync node-fs p)))))

(defn delete-if-exists
  "Deletes `path` if it exists via [Files/deleteIfExists](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#deleteIfExists(java.nio.file.Path)).
  Returns `true` if the delete was successful,
  `false` if `path` didn't exist. Does not follow symlinks."
  [path]
  #?(:clj (Files/deleteIfExists (as-path path))
     :cljs (try
             (delete path)
             true
             (catch :default e
               (if (= "ENOENT" (.-code e))
                 false
                 (throw e))))))

(defn sym-link?
  "Returns `true` if `path` is a symbolic link via [Files/isSymbolicLink](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isSymbolicLink(java.nio.file.Path))."
  [path]
  #?(:clj (Files/isSymbolicLink (as-path path))
     :cljs (try
             (.isSymbolicLink (stat path true))
             (catch :default _ false))))

(defn delete-tree
  "Deletes the file tree at `root-path` using [[walk-file-tree]]. Similar to `rm -rf` shell command. Does not follow symlinks.

  Returns `root-path`, or `nil` if `root-path` not found.

  Options:
  * `:force` - if `true` forces deletion of read-only files/directories. Similar to `chmod -R +wx` + `rm -rf` shell commands."
  ([root-path] (delete-tree root-path nil))
  ([root-path {:keys [force]}]
   (when (exists? root-path {:nofollow-links true})
     #?(:clj (walk-file-tree root-path
                             {:visit-file (fn [path _]
                                            (when (and win? force)
                                              (.setWritable (file path) true))
                                            (delete path)
                                            :continue)
                              :pre-visit-dir (fn [path _]
                                               (when force
                                                 (u+wx path))
                                               :continue)
                              :post-visit-dir (fn [path _]
                                                (delete path)
                                                :continue)})
        :cljs (do (if (sym-link? root-path)
                    (delete root-path)
                    (do
                      (when (and force (not win?))
                        ;; rmSync force only ignores missing paths, it does not chmod;
                        ;; make each dir writable so a read-only tree can be removed
                        (walk-file-tree root-path
                                        {:pre-visit-dir (fn [path _]
                                                          (u+wx path)
                                                          :continue)}))
                      (.rmSync node-fs (str root-path) #js {:recursive true :force (boolean force)})))
                  root-path)))))

(defn create-file
  "Creates empty `file` via [Files/createFile](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createFile(java.nio.file.Path,java.nio.file.attribute.FileAttribute...)).

  Returns `file`.

  Options:
  * `:posix-file-permissions` - string format for unix-like system permissions for `file`, as described in [[str->posix]].
  Affected by [umask](/README.md#umask)."
  ([file]
   (create-file file nil))
  ([file {:keys [:posix-file-permissions]}]
   #?(:clj (let [attrs (posix->attrs posix-file-permissions)]
             (Files/createFile (as-path file) attrs))
      :cljs (do
              (.writeFileSync node-fs (str file) "" #js {:flag "wx"})
              (when posix-file-permissions
                (chmod-umasked file posix-file-permissions))
              (str file)))))

(defn move
  "Moves or renames dir or file at `source-path` to `target-path` dir or file via [Files/move](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#move(java.nio.file.Path,java.nio.file.Path,java.nio.file.CopyOption...)).
  If `target-path` is a directory, moves `source-path` under `target-path`.
  Never follows symbolic links.

  Returns `target-path`.

  Options:
  * `replace-existing` - overwrite existing `target-path`, default `false`
  * `atomic-move` - watchers will only see complete `target-path` file, default `false`. Ignored on Node.js, which always uses `renameSync`. A cross-filesystem move throws on Node.js."
  ([source-path target-path] (move source-path target-path nil))
  ([source-path target-path {:keys [:replace-existing
                                    :atomic-move]}]
   #?(:clj (let [target (as-path target-path)
                 nofollow-links true
                 link-opts (->link-opts nofollow-links)]
             (if (Files/isDirectory target link-opts)
               (Files/move (as-path source-path)
                           (path target (file-name source-path))
                           (->copy-opts replace-existing false atomic-move nofollow-links))
               (Files/move (as-path source-path)
                           target
                           (->copy-opts replace-existing false atomic-move nofollow-links))))
      :cljs (let [dest (str target-path)
                  dest (if (directory? dest {:nofollow-links true})
                         (path dest (file-name source-path))
                         dest)]
              (cond
                ;; JVM Files/move is a no-op for the same file; a symlink dest is
                ;; not the same file (checked NOFOLLOW)
                (and (exists? dest) (not (sym-link? dest)) (same-file? source-path dest)) dest
                (and (not replace-existing) (exists? dest {:nofollow-links true}))
                (throw (ex-info (str "Target already exists: " dest) {}))
                :else (do (.renameSync node-fs (str source-path) dest)
                          dest))))))

(defn parent
  "Returns parent path of `path` via [Path#getParent](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html#getParent()).
  Akin to `dirname` in bash."
  [path]
  #?(:clj (.getParent (as-path path))
     :cljs (let [p (str path)
                 d (.dirname node-path p)]
             (when (and (not= "." d) (not= d p))
               d))))

(defn size
  "Returns the size of `path` in bytes."
  [path]
  #?(:clj (Files/size (as-path path))
     :cljs (.-size (stat path false))))

#?(:cljs (def ^:private delete-on-exit-paths (atom [])))

(defn delete-on-exit
  "Requests delete of `path` on exit via [File#deleteOnExit](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#deleteOnExit()).
  Returns `path`."
  [path]
  #?(:clj (do (.deleteOnExit (as-file path)) path)
     :cljs (do
             (when (empty? @delete-on-exit-paths)
               (.on js/process "exit"
                    (fn [] (doseq [p (reverse @delete-on-exit-paths)]
                             (try
                               (if (directory? p)
                                 (.rmdirSync node-fs (str p))
                                 (.rmSync node-fs (str p) #js {:force true}))
                               (catch :default _))))))
             (swap! delete-on-exit-paths conj (str path))
             (str path))))

(defn same-file?
  "Returns `true` if `this-path` is the same file as `other-path` via [Files/isSamefile](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#isSameFile(java.nio.file.Path,java.nio.file.Path))."
  [this-path other-path]
  #?(:clj (Files/isSameFile (as-path this-path) (as-path other-path))
     :cljs (or (= (str this-path) (str other-path))
               (let [s1 (stat-ns this-path false)
                     s2 (stat-ns other-path false)]
                 (and (= (str (.-dev s1)) (str (.-dev s2)))
                      (= (str (.-ino s1)) (str (.-ino s2))))))))

(defn read-all-bytes
  "Returns contents of `file` as byte array via [Files/readAllBytes](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#readAllBytes(java.nio.file.Path))."
  [file]
  #?(:clj (Files/readAllBytes (as-path file))
     :cljs (.readFileSync node-fs (str file))))

(defn- ->charset
  ^Charset [charset]
  #?(:clj (if (string? charset) (Charset/forName charset) charset)
     :cljs charset))

(defn read-all-lines
  "Returns contents of `file` as a vector of lines via [Files/readAllLines](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#readAllLines(java.nio.file.Path,java.nio.charset.Charset)).

  Options:
  * `:charset` - defaults to `\"utf-8\"`"
  ([file]
   #?(:clj (vec (Files/readAllLines (as-path file)))
      :cljs (read-all-lines file nil)))
  ([file {:keys [charset]
          :or {charset "utf-8"}}]
   #?(:clj (vec (Files/readAllLines (as-path file) (->charset charset)))
      :cljs (let [content (.readFileSync node-fs (str file) #js {:encoding (->charset charset)})]
              ;; match Files/readAllLines: strip at most one final terminator,
              ;; split on \n / \r / \r\n keeping empties; empty file is []
              (if (= "" content)
                []
                (let [content (cond
                                (str/ends-with? content "\r\n") (subs content 0 (- (count content) 2))
                                (or (str/ends-with? content "\n")
                                    (str/ends-with? content "\r")) (subs content 0 (dec (count content)))
                                :else content)]
                  (vec (.split content #"\r\n|\r|\n"))))))))

;;;; Attributes

#?(:cljs
   (defn- attr-name
     "Strips a `view:` prefix from an attribute spec, e.g. \"basic:size\" -> \"size\"."
     [s]
     (let [i (.indexOf s ":")]
       (if (neg? i) s (subs s (inc i))))))

(declare read-attributes*)
(declare file-time->millis)

(defn get-attribute
  "Returns value of `attribute` for `path` via [Files/getAttribute](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#getAttribute(java.nio.file.Path,java.lang.String,java.nio.file.LinkOption...)).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path attribute]
   (get-attribute path attribute nil))
  ([path attribute {:keys [:nofollow-links]}]
   #?(:clj (Files/getAttribute (as-path path) attribute (->link-opts nofollow-links))
      :cljs (get (read-attributes* path attribute {:nofollow-links nofollow-links})
                 (attr-name attribute)))))

(defn- keyize
  [key-fn m]
  (let [f (fn [[k v]] (if (string? k) [(key-fn k) v] [k v]))]
    #?(:clj (walk/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)
       :cljs (into {} (map f m)))))

(defn read-attributes*
  "Returns requested `attributes` for `path` via [Files/readAttributes](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#readAttributes(java.nio.file.Path,java.lang.Class,java.nio.file.LinkOption...)).

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path attributes]
   (read-attributes* path attributes nil))
  ([path attributes {:keys [:nofollow-links]}]
   #?(:clj (let [p (as-path path)
                 link-opts (->link-opts nofollow-links)
                 attrs
                 (if (instance? String attributes)
                   (Files/readAttributes p ^String attributes link-opts)
                   (Files/readAttributes p ^Class attributes link-opts))]
             attrs)
      :cljs (let [st (stat-ns path nofollow-links)
                  all {"lastModifiedTime" (.-mtimeNs st)
                       "lastAccessTime" (.-atimeNs st)
                       "creationTime" (.-birthtimeNs st)
                       "size" (js/Number (.-size st))
                       "isRegularFile" (.isFile st)
                       "isDirectory" (.isDirectory st)
                       "isSymbolicLink" (.isSymbolicLink st)
                       "isOther" (not (or (.isFile st) (.isDirectory st)
                                          (.isSymbolicLink st)))
                       "fileKey" nil}
                  spec (attr-name attributes)]
              (if (= "*" spec)
                all
                (select-keys all (vec (.split spec ","))))))))

(defn read-attributes
  "Same as [[read-attributes*]] but returns requested `attributes` for `path` as a map with keywordized attribute keys.

  Options:
  * `:key-fn` - optionally override keywordizing function with your own.
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path attributes]
   (read-attributes path attributes nil))
  ([path attributes {:keys [:nofollow-links :key-fn] :as opts}]
   (->> (read-attributes* path attributes opts)
        (into {})
        (keyize (or key-fn keyword)))))

(defn set-attribute
  "Sets `attribute` for `path` to `value` via [Files/setAttribute](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#setAttribute(java.nio.file.Path,java.lang.String,java.lang.Object,java.nio.file.LinkOption...)).

  Returns `path`."
  ([path attribute value]
   (set-attribute path attribute value nil))
  ([path attribute value {:keys [:nofollow-links]}]
   #?(:clj (Files/setAttribute (as-path path) attribute value (->link-opts nofollow-links))
      :cljs (let [p (str path)
                  k (attr-name attribute)
                  st (stat path nofollow-links)
                  v (js/Date. (file-time->millis value))
                  [atime mtime] (case k
                                  "lastModifiedTime" [(.-atime st) v]
                                  "lastAccessTime" [v (.-mtime st)]
                                  (throw (ex-info (str "set-attribute not supported on Node.js for: " attribute) {})))]
              (if nofollow-links
                (.lutimesSync node-fs p atime mtime)
                (.utimesSync node-fs p atime mtime))
              path))))

(defn file-time->instant
  "Converts file time `ft` (`FileTime` on the JVM, BigInt nanoseconds on Node.js)
  to an [Instant](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Instant.html).
  Node.js has no instant type, so there `ft` is returned as-is."
  [ft]
  #?(:clj (.toInstant ^FileTime ft)
     :cljs ft))

(defn instant->file-time
  "Converts `instant` to a file time (`FileTime` on the JVM, BigInt nanoseconds on
  Node.js). Node.js has no instant type, so there `instant` is returned as-is."
  [instant]
  #?(:clj (FileTime/from instant)
     :cljs instant))

(defn file-time->millis
  "Converts file time `ft` (`FileTime` on the JVM, BigInt nanoseconds on Node.js)
  to epoch milliseconds."
  [ft]
  #?(:clj (.toMillis ^FileTime ft)
     :cljs (js/Number (/ ft ns-per-ms))))

(defn millis->file-time
  "Converts epoch milliseconds to a file time (`FileTime` on the JVM, BigInt
  nanoseconds on Node.js)."
  [millis]
  #?(:clj (FileTime/fromMillis millis)
     :cljs (* (js/BigInt millis) ns-per-ms)))

(defn- ->file-time [x]
  #?(:clj (cond (int? x) (millis->file-time x)
                (instance? java.time.Instant x) (instant->file-time x)
                (instance? FileTime x) x
                :else (throw (ex-info "Unrecognized time type" {})))
     :cljs (cond (number? x) (millis->file-time x)
                 (bigint? x) x
                 (instance? js/Date x) (* (js/BigInt (.getTime x)) ns-per-ms)
                 :else (throw (ex-info "Unrecognized time type" {})))))

(defn last-modified-time
  "Returns last modified time of `path` as [FileTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/FileTime.html) (JVM) or BigInt nanoseconds (Node.js).

  See also: [[set-last-modified-time]], [[creation-time]], [[file-time->instant]], [[file-time->millis]]"
  ([path]
   (last-modified-time path nil))
  ([path opts]
   (get-attribute path "basic:lastModifiedTime" opts)))

(defn set-last-modified-time
  "Sets last modified `time` of `path`.
  `time` can be `epoch milliseconds` (long),
  [FileTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/FileTime.html),
  or [Instant](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Instant.html)
  (on JVM), or BigInt nanoseconds / epoch ms (on Node.js).

  Returns `path`.

  See also: [[last-modified-time]]"
  ([path time]
   (set-last-modified-time path time nil))
  ([path time opts]
   (set-attribute path "basic:lastModifiedTime" (->file-time time) opts)))

(defn creation-time
  "Returns creation time of `path` as [FileTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/FileTime.html) (JVM) or BigInt nanoseconds (Node.js).

  See [README notes](/README.md#creation-time) for some details on behaviour.

  See also: [[set-creation-time]], [[last-modified-time]], [[file-time->instant]], [[file-time->millis]]"
  ([path]
   (creation-time path nil))
  ([path opts]
   (get-attribute path "basic:creationTime" opts)))

(defn set-creation-time
  "Sets creation `time` of `path`.
  `time` can be `epoch milliseconds` (long),
  [FileTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/FileTime.html),
  or [Instant](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Instant.html).

  Not supported on Node.js.

  Returns `path`.

  Options:
  * [`:nofollow-links`](/README.md#nofollow-links)

  See [README notes](/README.md#set-creation-time) for some details on behaviour.

  See also: [[creation-time]]"
  ([path time]
   (set-creation-time path time nil))
  ([path time {:keys [nofollow-links] :as opts}]
   #?(:clj (set-attribute path "basic:creationTime" (->file-time time) opts)
      :cljs (throw (ex-info "set-creation-time not supported on Node.js" {})))))

(defn touch
  "Updates last modified time of `path` to `:time`, creating `path` as a file if it does not exist.

  If `path` is deleted by some other process/thread before `:time` is set,
  a `NoSuchFileException` will be thrown. Callers can, if their use case requires it,
  implement their own retry loop.

  Returns `path`.

  Options:
  * `:time` - last modified time (epoch milliseconds (long), [Instant](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Instant.html),
  or [FileTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/attribute/FileTime.html)), defaults to current time
  * [`:nofollow-links`](/README.md#nofollow-links)"
  ([path]
   (touch path nil))
  ([path {:keys [time nofollow-links] :as opts}]
   #?(:clj (let [time (when time (->file-time time))
                 path (as-path path)
                 time (or time (java.time.Instant/now))]
             (try
               (set-last-modified-time path time opts)
               (catch java.nio.file.NoSuchFileException _
                 (with-open [_chan (-> (java.nio.channels.FileChannel/open
                                        path
                                        (into-array [java.nio.file.StandardOpenOption/CREATE
                                                     java.nio.file.StandardOpenOption/WRITE])))])
                 (set-last-modified-time path time opts))))
      :cljs (let [p (str path)
                  t (if time (js/Date. (file-time->millis (->file-time time))) (js/Date.))]
              (when-not (exists? p {:nofollow-links nofollow-links})
                (.writeFileSync node-fs p "" #js {:flag "a"}))
              (if nofollow-links
                (.lutimesSync node-fs p t t)
                (.utimesSync node-fs p t t))
              p))))

(defn list-dirs
  "Similar to [[list-dir]] but accepts multiple roots in `dirs` and returns the concatenated results.
  - `glob-or-accept` - a [[glob]] string such as `\"*.edn\"` or a `(fn accept [^java.nio.file.Path p]) -> truthy`"
  [dirs glob-or-accept]
  (mapcat #(list-dir % glob-or-accept) dirs))

(defn split-ext
  "Splits `path` on extension. Returns `[name ext]`.
  Leading directories in `path` are not processed.

  Options:
  * `:ext` - split on specified extension (do not include a leading dot)

  Examples:
  - `(fs/split-ext \"foo.bar.baz\")` => `[\"foo.bar\" \"baz\"]`
  - `(fs/split-ext \"foo.bar.baz\" {:ext \"bar.baz\"})`  => `[\"foo\" \"bar.baz\"]`
  - `(fs/split-ext \"foo.bar.baz\" {:ext \"png\"})`  => `[\"foo.bar.baz\" nil]`"
  ([path] (split-ext path nil))
  ([path {:keys [ext]}]
   (let [path-str (str path)
         file-name (file-name path)]
     (let [ext (if ext
                 (str "." ext)
                 (when-let [last-dot (str/last-index-of file-name ".")]
                   (subs file-name last-dot)))]
       (if (and ext
                (str/ends-with? path-str ext)
                (not= file-name ext))
         (let [loc (str/last-index-of path-str ext)]
           [(subs path-str 0 loc)
            (subs path-str (inc loc))])
         [path-str nil])))))

(defn strip-ext
  "Strips extension from `path` via [[split-ext]]."
  ([path]
   (strip-ext path nil))
  ([path {:keys [ext] :as opts}]
   (first (split-ext path opts))))

(defn extension
  "Returns the extension of `path` via [[split-ext]]."
  [path]
  (-> path split-ext last))

(defn split-paths
  "Splits `joined-paths` string into a vector of paths by OS-specific [[path-separator]]."
  [^String joined-paths]
  (mapv #?(:clj path :cljs str) (.split joined-paths path-separator)))

(defn exec-paths
  "Returns a vector of command search paths (from the `PATH` environment variable). Same
  as `(split-paths (System/getenv \"PATH\"))`."
  []
  (let [path (get-env "PATH")]
    (if (str/blank? path)
      []
      (split-paths path))))

(defn- filename-only?
  "Returns `true` if `path` is exactly a file name (i.e. with no absolute or
  relative path information)."
  [path]
  #?(:clj (let [f-as-path (as-path path)]
            (= f-as-path (.getFileName f-as-path)))
     :cljs (= (str path) (file-name (str path)))))

(defn which
  "Returns path to first executable `program` found in `:paths`, similar to the `which` Unix command.

  When `program` is a relative or absolute path, `:paths` option is not consulted.
  On Windows, the `:win-exts` variants are still searched.
  On other OSes, the path for `program` will be returned if executable, else `nil`.

  Options:
  * `:paths` - paths to search, default is return of ([[exec-paths]])
  * `:win-exts` - active on Windows only. Searches for `program` with filename extensions specified in `:win-exts` option.
  If `program` already includes an extension from `:win-exts`, it will be searched as-is first.
  Default is `[\"com\" \"exe\" \"bat\" \"cmd\"]`."
  ([program] (which program nil))
  ([program opts]
   (let [exts (if win?
                (let [exts (or (:win-exts opts)
                               ["com" "exe" "bat" "cmd"])
                      ext (extension program)]
                  (if (and ext (contains? (set exts) ext))
                    (into [nil] exts)
                    exts))
                [nil])
         paths (or (:paths opts) (exec-paths))
         candidate-paths (if (filename-only? program)
                           paths
                           [nil])]
     (loop [paths candidate-paths
            results []]
       (if (seq paths)
         (let [p (first paths)
               fs (loop [exts exts
                         candidates []]
                    (if (seq exts)
                      (let [ext (first exts)
                            program (str program (when ext (str "." ext)))
                            f (if (relative? program)
                                (path p program)
                                (path program))]
                        (if (and (executable? f) (not (directory? f)))
                          (recur (rest exts)
                                 (conj candidates f))
                          (recur (rest exts)
                                 candidates)))
                      candidates))]
           (if (seq fs)
             (if (:all opts)
               (recur (rest paths) (into results fs))
               (first fs))
             (recur (rest paths) results)))
         (if (:all opts) results (first results)))))))

(defn which-all
  "Returns a vector of every path to `program` found in ([[exec-paths]]). See [[which]]."
  ([program] (which-all program nil))
  ([program opts]
   (which program (assoc opts :all true))))

;;;; Modified since

#?(:cljs
   (defn- mtime-ns
     "Returns mtime of `file` as nanosecond BigInt, preserving sub-millisecond filesystem precision for newness comparison."
     [file]
     (.-mtimeNs (stat-ns file false))))

(def ^:private epoch-file-time #?(:clj (FileTime/fromMillis 0) :cljs (js/BigInt 0)))

(defn- file-time>
  [a b]
  #?(:clj (pos? (.compareTo ^FileTime a ^FileTime b))
     :cljs (> a b)))

(defn- last-modified-1
  [file]
  #?(:clj (try (last-modified-time file)
               (catch java.io.IOException _ epoch-file-time))
     :cljs (try (mtime-ns file)
                (catch :default _ epoch-file-time))))

(defn- max-filetime [filetimes]
  (reduce #(if (file-time> %1 %2) %1 %2) epoch-file-time filetimes))

(defn- last-modified
  [path]
  (if (exists? path)
    (if (regular-file? path)
      (last-modified-1 path)
      (max-filetime
       (map last-modified-1
            (regular-files path))))
    epoch-file-time))

(defn- expand-file-set
  [file-set]
  (if (coll? file-set)
    (mapcat expand-file-set file-set)
    (regular-files file-set)))

(defn modified-since
  "Returns seq of regular files (non-directories, non-symlinks) from `path-set` that were modified since the `anchor-path`.
  The `anchor-path` can be a regular file or directory, in which case
  the recursive max last modified time stamp is used as the timestamp
  to compare with.  The `path-set` may be a regular file, directory or
  collection of paths (e.g. as returned by [[glob]]). Directories are
  searched recursively."
  [anchor-path path-set]
  (let [lm (last-modified anchor-path)]
    (map path
         (filter (fn [f]
                   (file-time> (last-modified-1 f) lm))
                 (expand-file-set path-set)))))

;;;; Zip

(defn unzip
  "Unzips `zip-file` to `target-dir` (default `\".\"`).

  Returns `target-dir`.

  Options:
   * `:replace-existing` - `true` / `false`: overwrite existing files
   * `:extract-fn` - function that decides if the current [ZipEntry](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/zip/ZipEntry.html)
     should be extracted. Extraction only occurs if a truthy value is returned (i.e. not nil/false).
     The function is only called for files (not directories) with a single map arg:
     * `:entry` - the current `ZipEntry`
     * `:name` - the name of the `ZipEntry` (result of calling `getName`)

  See also: [[zip]]."
  ([zip-file] (unzip zip-file "."))
  ([zip-file target-dir] (unzip zip-file target-dir nil))
  ([zip-file target-dir {:keys [replace-existing extract-fn]}]
   #?(:clj
      (let [output-path (as-path target-dir)
            _ (create-dirs target-dir)
            cp-opts (->copy-opts replace-existing nil nil nil)]
        (with-open
         [^InputStream fis
          (if (instance? InputStream zip-file) zip-file
              (Files/newInputStream (as-path zip-file) (into-array java.nio.file.OpenOption [])))
          zis (ZipInputStream. fis)]
          (loop []
            (let [entry (.getNextEntry zis)]
              (when entry
                (let [entry-name (.getName entry)
                      new-path (.resolve output-path entry-name)]
                  (if (.isDirectory entry)
                    (create-dirs new-path)
                    (when (or (nil? extract-fn)
                              (and (fn? extract-fn)
                                   (extract-fn {:entry entry :name entry-name})))
                      (when-let [p (parent new-path)]
                        (create-dirs p))
                      (Files/copy ^java.io.InputStream zis
                                  new-path
                                  cp-opts))))
                (recur)))))
        output-path)
      :cljs
      (throw (ex-info "unzip not supported on Node.js without an npm dependency" {})))))

#?(:clj
   (defn- add-zip-entry [^ZipOutputStream output-stream ^Path path fpath]
     (let [dir (directory? path)
           attrs (Files/readAttributes path BasicFileAttributes
                                       (->link-opts []))
           entry (doto (ZipEntry. (str fpath))
                   (.setLastModifiedTime (.lastModifiedTime attrs)))]
       (.putNextEntry output-stream entry)
       (when-not dir
         (with-open [fis (BufferedInputStream. (FileInputStream. (file path)))]
           (io/copy fis output-stream)))
       (.closeEntry output-stream))))

#?(:clj
   (defn- copy-to-zip [^ZipOutputStream jos path path-fn]
     (let [files (path-seq path)]
       (run! (fn [^Path f]
               (let [dir (directory? f)
                     fpath (str f)
                     fpath (if (and dir (not (.endsWith fpath "/"))) (str fpath "/") fpath)
                     fpath (str/replace fpath \\ \/)
                     fpath (path-fn fpath)]
                 (when-not (str/blank? fpath)
                   (assert (relative? fpath)
                           (str "Zip entry must be relative: " fpath))
                   (add-zip-entry jos f fpath))))
             files))))

(defn zip
  "Zips `path-or-paths` into `zip-file`. A path may be a file or
  directory. Directories are included recursively and their names are
  preserved in the zip file.

  Zip entries must be relative. Absolute source paths are allowed as
  long as `:root` or `:path-fn` maps them to relative entries, e.g.:
  `(fs/zip \"/tmp/out.zip\" \"/tmp/src\" {:root \"/tmp/src\"})`.

  Returns created `zip-file`.

  Options:
  * `:root` - optional directory to be elided in `zip-file` entries. E.g.: `(fs/zip [\"src\"] {:root \"src\"})`
  * `:path-fn` - an optional custom path conversion function.
  A single-arg function called for each file system path returning the path to be used for the corresponding zip entry.

  See also: [[unzip]]."
  ([zip-file path-or-paths]
   (zip zip-file path-or-paths nil))
  ([zip-file path-or-paths opts]
   #?(:clj (let [zip-file (as-path zip-file)
                 entries (if (or (string? path-or-paths)
                                 (instance? File path-or-paths)
                                 (instance? Path path-or-paths))
                           [path-or-paths]
                           path-or-paths)
                 path-fn (or (:path-fn opts)
                             (when-let [root (:root opts)]
                               #(str/replace % (re-pattern (str "^" (java.util.regex.Pattern/quote root) "/")) ""))
                             identity)]
             (with-open [zos (ZipOutputStream.
                              (FileOutputStream. (file zip-file)))]
               (doseq [zpath entries]
                 (copy-to-zip zos zpath #(when-not (same-file? % zip-file)
                                           (path-fn %)))))
             zip-file)
      :cljs
      (throw (ex-info "zip not supported on Node.js without an npm dependency" {})))))

;;;; GZip

(defn gunzip
  "Extracts `gz-file` to `target-dir`.

   If `target-dir` not specified (or `nil`) defaults to `gz-file` dir.

   File is extracted to `target-dir` with `gz-file` [[file-name]] without `.gz` extension.

   Creates `target-dir` dir(s) if necessary.
   The `gz-file` is not deleted.

   Returns the extracted file.

   Options:
   * `:replace-existing` - when `true` overwrites existing file

   See also: [[gzip]]"
  ([gz-file] (gunzip gz-file nil))
  ([gz-file target-dir] (gunzip gz-file target-dir {}))
  ([gz-file target-dir {:keys [replace-existing]}]
   (let [dest-dir (or target-dir (parent gz-file) "")
         dest-filename (str/replace-first (file-name gz-file) #"\.gz$" "")
         output-file (path dest-dir dest-filename)]
     (when-let [p (parent output-file)]
       (create-dirs p))
     #?(:clj (with-open [fis (Files/newInputStream (as-path gz-file) (into-array java.nio.file.OpenOption []))
                         gzis (GZIPInputStream. fis)]
               (Files/copy ^java.io.InputStream gzis output-file
                           (->copy-opts replace-existing nil nil nil)))
        :cljs (do (when (and (not replace-existing) (exists? output-file))
                    (throw (ex-info (str "File already exists: " output-file) {})))
                  (.writeFileSync node-fs output-file
                                  (.gunzipSync node-zlib (.readFileSync node-fs (str gz-file))))))
     output-file)))

(defn gzip
  "Gzips `source-file` to `:dir`/`:out-file`.

  Does not store the `source-file` name in the `.gz` file.
  The `source-file` is not deleted.

  Returns the created gzip file.

  Options:
  * `:dir`(s) - created if necessary. If not specified, defaults to `source-file` dir.
  * `:out-file` - if not specified, defaults to `source-file` [[file-name]] with `.gz` extension.

  See also: [[gunzip]]"
  ([source-file]
   (gzip source-file {}))
  ([source-file {:keys [dir out-file]}]
   (assert source-file "source-file must be specified")
   (assert (exists? source-file) "source-file does not exist")
   (let [dest-dir (or dir (parent source-file) "")
         dest-filename (if out-file (str out-file) (str (file-name source-file) ".gz"))
         output-file (path dest-dir dest-filename)]
     (when-let [p (parent output-file)]
       (create-dirs p))
     #?(:clj (with-open [source-input-stream (io/input-stream (file source-file))
                         gzos (GZIPOutputStream. (FileOutputStream. (file output-file)))]
               (io/copy source-input-stream gzos))
        :cljs (.writeFileSync node-fs output-file
                              (.gzipSync node-zlib (.readFileSync node-fs (str source-file)))))
     (str output-file))))

;;;; End gzip

;; One definition for every target. JVM/bb/cljs/shadow read this defmacro (its var
;; interned in babashka.fs so `fs/with-temp-dir` resolves). squint skips emitting
;; it to JS and, via the ns's {:squint/compile-time true} flag, loads this defmacro
;; alone as babashka.fs, so the bare create-temp-dir/delete-tree refs resolve,
;; without evaluating this whole namespace.
(defmacro with-temp-dir
  "Evaluates body with `temp-dir` bound to the result of `(create-temp-dir opts)`.

  By default, the `temp-dir` will be removed with [[delete-tree]] on exit from the scope.

  Options:
  * see [[create-temp-dir]] for options that control directory creation
  * `:keep` - if `true` does not delete the directory on exit from macro scope.

  Example:
  ```
  (with-temp-dir [d]
    (let [t (path d \"extract\")
      (create-dir t)
      (gunzip path-to-zip t)
      (copy (path t \"the-one-file-I-wanted.txt\") (path permanent-dir \"file-I-extracted.txt\"))))
  ;; d no longer exists here
  ```
  "
  {:arglists '[[[temp-dir] & body]
               [[temp-dir opts] & body]]}
  [[temp-dir opts & more] & body]
  {:pre [(empty? more) (symbol? temp-dir)]}
  `(let [opts# ~opts
         ~temp-dir (create-temp-dir opts#)]
     (try
       ~@body
       (finally
         (when-not (:keep opts#)
           (delete-tree ~temp-dir {:force true}))))))

(def ^:private cached-home-dir
  #?(:clj (delay (path (System/getProperty "user.home")))
     :cljs (delay (.homedir node-os))))

(def ^:private cached-users-dir
  (delay (parent @cached-home-dir)))

(defn home
  "Returns home dir path.

  With no arguments, returns the current value of the `user.home`
  system property. If a `user` is passed, returns that user's home
  directory as found in the parent of home with no args."
  ([] @cached-home-dir)
  ([user] (if (empty? user) @cached-home-dir
              (path @cached-users-dir user))))

(defn expand-home
  "Returns `path` replacing `~` (tilde) with home dir.

  If `path`:
  - does not start with `~`, returns `path`.
  - starts with `~` then [[file-separator]], `~` is replaced with `(home)`.
  e.g., `~/foo` -> `/home/myuser/foo`
  - starts with `~` then some other chars, those other chars are
  assumed to be a username, then naively expanded to `(home username)`.
  e.g., `~someuser/foo` -> `/home/someuser/foo`

  See also: [[home]]"
  [path]
  ;; the JVM Path normalizes / to \ on Windows; Node paths are strings, so
  ;; normalize explicitly or the file-separator lookup below misses
  (let [path-str #?(:clj (str (as-path path))
                    :cljs (native-sep path))]
    (if (.startsWith path-str "~")
      (let [sep (.indexOf path-str ^String file-separator)]
        (if (neg? sep)
          (home (subs path-str 1))
          (path* (home (subs path-str 1 sep)) (subs path-str (inc sep)))))
      (as-path path))))

(defn windows?
  "Returns `true` if OS is Windows."
  []
  win?)

(defn cwd
  "Returns current working directory path."
  []
  #?(:clj (as-path (System/getProperty "user.dir"))
     :cljs (.cwd js/process)))

#?(:clj
   (defn- ->open-option [k]
     (case k
       :append StandardOpenOption/APPEND
       :create StandardOpenOption/CREATE
       :truncate-existing StandardOpenOption/TRUNCATE_EXISTING
       :write StandardOpenOption/WRITE
       k)))

#?(:clj
   (defn- ->open-options ^"[Ljava.nio.file.OpenOption;" [opts]
     (into-array java.nio.file.OpenOption
                 (reduce-kv (fn [acc k v]
                              (if v
                                (conj acc (->open-option k))
                                acc)) [] opts))))

(defn write-bytes
  "Writes `bytes` to `file` via [Files/write](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#write(java.nio.file.Path,byte%5B%5D,java.nio.file.OpenOption...)).

  Returns `file`.

  Options:
  * `:create` - (default `true`)
  * `:truncate-existing` - (default `true`)
  * `:write` - (default `true`)
  * `:append` - (default `false`)
  * or any `java.nio.file.StandardOption`.

  Examples:

  ``` clojure
  (fs/write-bytes f (.getBytes (String. \"foo\"))) ;; overwrites + truncates or creates new file
  (fs/write-bytes f (.getBytes (String. \"foo\")) {:append true})
  ```"
  ([file bytes] (write-bytes file bytes nil))
  ([file bytes {:keys [append] :as opts}]
   #?(:clj (let [path (as-path file)
                 opts (->open-options opts)]
             (java.nio.file.Files/write path ^bytes bytes opts))
      :cljs (do
              (.writeFileSync node-fs (str file) bytes #js {:flag (if append "a" "w")})
              (str file)))))

(defn write-lines
  "Writes `lines`, a seqable of strings, to `file` via [Files/write](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#write(java.nio.file.Path,java.lang.Iterable,java.nio.charset.Charset,java.nio.file.OpenOption...)).

  Returns `file`.

  Options:
  * `:charset` - (default `\"utf-8\"`)

  Open options:
  * `:create` - (default `true`)
  * `:truncate-existing` - (default `true`)
  * `:write` - (default `true`)
  * `:append` - (default `false`)
  * or any `java.nio.file.StandardOption`."
  ([file lines] (write-lines file lines nil))
  ([file lines {:keys [charset append]
                :or {charset "utf-8"}
                :as opts}]
   #?(:clj (java.nio.file.Files/write (as-path file)
                                      lines
                                      (->charset charset)
                                      (->open-options (dissoc opts :charset)))
      :cljs (let [eol (.-EOL node-os)
                  content (if (seq lines) (str (str/join eol lines) eol) "")]
              (.writeFileSync node-fs (str file) content
                              #js {:encoding (->charset charset) :flag (if append "a" "w")})
              (str file)))))

(defn slurp
  "Reads file as text.
  Options:
  * `:charset` - (default `\"utf-8\"`)"
  ([file] (slurp file nil))
  ([file {:keys [charset] :or {charset "utf-8"}}]
   #?(:clj (clojure.core/slurp (as-file file) :encoding charset)
      :cljs (.readFileSync node-fs (str file) #js {:encoding charset}))))

(defn spit
  "Writes `content` to `file` as text.
  Options:
  * `:charset` - (default `\"utf-8\"`)
  * `:append` - append to `file` instead of overwriting (default `false`)"
  ([file content] (spit file content nil))
  ([file content {:keys [charset append] :or {charset "utf-8"}}]
   #?(:clj (clojure.core/spit (as-file file) content :encoding charset :append append)
      :cljs (.writeFileSync node-fs (str file) content
                            #js {:encoding charset :flag (if append "a" "w")}))
   file))

(defn update-file
  "Updates the contents of text `file` with result of applying function `f` with old contents and args `xs`.
  Returns the new contents.

  Options:
  * `:charset` - charset of file, default to \"utf-8\""
  {:arglists '([file f & xs] [file opts f & xs])}
  ([file f & xs]
   (let [[opts f xs] (if (map? f)
                       [f (first xs) (rest xs)]
                       [nil f xs])
         {:keys [charset]
          :or {charset "utf-8"}} opts
         old-val (slurp file {:charset charset})
         new-val (apply f old-val xs)]
     (spit file new-val {:charset charset})
     new-val)))

(defn unixify
  "Returns `path` as string with Unix-style file separators (`/`)."
  [path]
  (let [s (str path)]
    (if win?
      (str/replace s "\\" "/")
      s)))

(defn- xdg-path-from-env-var
  [k]
  (some-> (get-env k)
          (#(when (absolute? %) %))
          #?(:clj (path) :cljs (str))))

(def ^:private xdg-type->env-var&default-path
  (delay {:config ["XDG_CONFIG_HOME" (path (home) ".config")]
          :cache ["XDG_CACHE_HOME" (path (home) ".cache")]
          :data ["XDG_DATA_HOME" (path (home) ".local" "share")]
          :state ["XDG_STATE_HOME" (path (home) ".local" "state")]}))

(defn- xdg-home-for [k]
  (let [[env-var default-path] (get @xdg-type->env-var&default-path k)]
    (or (xdg-path-from-env-var env-var)
        default-path)))

(defn xdg-config-home
  "Returns path to user-specific configuration files as described in the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

  Uses env-var `XDG_CONFIG_HOME` (if set and representing an absolute path), else `(fs/path (fs/home) \".config\")`.
  When provided, appends `app` to the returned path."
  ([] (xdg-config-home nil))
  ([app]
   (cond-> (xdg-home-for :config)
     (seq app) (path app))))

(defn xdg-cache-home
  "Returns path to user-specific non-essential data as described in the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

  Uses env-var `XDG_CACHE_HOME` (if set and representing an absolute path), else `(fs/path (fs/home) \".cache\")`.
  When provided, appends `app` to the returned path."
  ([] (xdg-cache-home nil))
  ([app]
   (cond-> (xdg-home-for :cache)
     (seq app) (path app))))

(defn xdg-data-home
  "Returns path to user-specific data files as described in the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

  Uses env-var `XDG_DATA_HOME` (if set and representing an absolute path), else `(fs/path (fs/home) \".local\" \"share\")`.
  When provided, appends `app` to the returned path."
  ([] (xdg-data-home nil))
  ([app]
   (cond-> (xdg-home-for :data)
     (seq app) (path app))))

(defn xdg-state-home
  "Returns path to user-specific state files as described in the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

  Uses env-var `XDG_STATE_HOME` (if set and representing an absolute path), else `(fs/path (fs/home) \".local\" \"state\")`.
  When provided, appends `app` to the returned path."
  ([] (xdg-state-home nil))
  ([app]
   (cond-> (xdg-home-for :state)
     (seq app) (path app))))
