(ns jolt.fs
  "File-system utilities over java.io.File, API-compatible with babashka.fs
  where the two overlap. Functions accept a String or a File; path-valued
  results are Files (Jolt's path value — there is no java.nio.file.Path).
  Symbolic-link operations (sym-link?, read-link) and creation-time are not
  available on the host and throw."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(defn file
  "Coerce to a java.io.File. With several args, nests: (file a b c) = a/b/c."
  ([f] (if (instance? File f) f (File. (str f))))
  ([f & fs] (reduce (fn [^File p c] (File. (str p) (str c))) (file f) fs)))

;; --- predicates ---------------------------------------------------------------

(defn exists? [f] (.exists (file f)))
(defn directory? [f] (.isDirectory (file f)))
(defn regular-file? [f] (.isFile (file f)))
(defn absolute? [f] (.isAbsolute (file f)))
(defn relative? [f] (not (.isAbsolute (file f))))
(defn readable? [f] (.canRead (file f)))
(defn writable? [f] (.canWrite (file f)))
(defn executable? [f] (.canExecute (file f)))
(defn hidden? [f] (.isHidden (file f)))
(defn sym-link? [f]
  (throw (ex-info "sym-link? is not supported on this host" {:path (str f)})))

;; --- path pieces ---------------------------------------------------------------

(defn file-name
  "Final path segment as a string."
  [f] (.getName (file f)))

(defn parent
  "Parent as a File, nil at a root or a bare name."
  [f] (.getParentFile (file f)))

(defn extension
  "Extension without the dot, nil when there is none."
  [f]
  (let [n (file-name f)
        i (.lastIndexOf n ".")]
    (when (pos? i) (subs n (inc i)))))

(defn strip-ext
  "File name without its extension."
  [f]
  (let [n (file-name f)
        i (.lastIndexOf n ".")]
    (if (pos? i) (subs n 0 i) n)))

(defn absolutize [f] (.getAbsoluteFile (file f)))
(defn canonicalize [f] (.getCanonicalFile (file f)))

(defn cwd
  "Current working directory as a File."
  [] (File. (System/getProperty "user.dir")))

(defn relativize
  "Path of `other` relative to `root`, as a File."
  [root other]
  (let [r (str (absolutize root))
        o (str (absolutize other))
        sep File/separator
        r (if (str/ends-with? r sep) r (str r sep))]
    (if (str/starts-with? o r)
      (File. (subs o (count r)))
      (throw (ex-info "cannot relativize: not nested under root"
                      {:root r :other o})))))

;; --- reading the tree ------------------------------------------------------------

(defn list-dir
  "Immediate children as Files."
  [dir] (seq (.listFiles (file dir))))

(defn walk
  "Depth-first seq of every file and directory under root, root first."
  [root]
  (tree-seq #(.isDirectory ^File %) #(.listFiles ^File %) (file root)))

(defn- glob->re
  "Translate a glob pattern to a regex over the /-separated relative path.
  ** crosses directory separators, * and ? stay within a segment,
  {a,b} alternates."
  [pattern]
  (loop [cs (seq pattern) out "^"]
    (if-not cs
      (re-pattern (str out "$"))
      (let [c (first cs)]
        (cond
          (and (= c \*) (= (second cs) \*))
          (recur (nnext cs) (str out ".*"))
          (= c \*) (recur (next cs) (str out "[^/]*"))
          (= c \?) (recur (next cs) (str out "[^/]"))
          (= c \{) (recur (next cs) (str out "("))
          (= c \}) (recur (next cs) (str out ")"))
          (= c \,) (recur (next cs) (str out "|"))
          (contains? #{\. \( \) \[ \] \^ \$ \+ \|} c)
          (recur (next cs) (str out "\\" c))
          :else (recur (next cs) (str out c)))))))

(defn glob
  "Files under root whose path relative to root matches the glob pattern
  (** crosses directories, * within a segment, ?, {a,b}). Returns Files."
  [root pattern]
  (let [root-f (file root)
        re (glob->re pattern)
        prefix (str root-f File/separator)]
    (->> (walk root-f)
         (remove #(= % root-f))
         (filter (fn [^File f]
                   (let [rel (subs (str f) (count prefix))]
                     (re-matches re rel)))))))

;; --- creation / deletion ---------------------------------------------------------

(defn create-dir
  "Create one directory level; the parent must exist. Returns the File."
  [dir]
  (let [f (file dir)]
    (when-not (.mkdir f)
      (throw (ex-info "could not create directory" {:path (str f)})))
    f))

(defn create-dirs
  "Create a directory and any missing parents. Returns the File."
  [dir]
  (let [f (file dir)]
    (when-not (or (.isDirectory f) (.mkdirs f))
      (throw (ex-info "could not create directories" {:path (str f)})))
    f))

(defn create-file
  "Create an empty file. Returns the File."
  [f]
  (let [f (file f)] (.createNewFile f) f))

(defn create-temp-dir
  "Fresh directory under the system temp dir. Returns the File."
  [& [{:keys [prefix] :or {prefix "jolt-"}}]]
  (let [f (File/createTempFile prefix "")]
    (.delete f)
    (create-dir (str f))))

(defn create-temp-file
  "Fresh file under the system temp dir. Returns the File."
  [& [{:keys [prefix suffix] :or {prefix "jolt-" suffix ".tmp"}}]]
  (File/createTempFile prefix suffix))

(defn delete
  "Delete a file or empty directory; throws when it does not exist."
  [f]
  (let [f (file f)]
    (when-not (.exists f)
      (throw (ex-info "no such file" {:path (str f)})))
    (when-not (.delete f)
      (throw (ex-info "could not delete" {:path (str f)})))
    nil))

(defn delete-if-exists
  "Delete when present. True when something was deleted."
  [f]
  (let [f (file f)] (and (.exists f) (.delete f))))

(defn delete-tree
  "Delete a file or directory recursively. Missing paths are a no-op."
  [root]
  (let [f (file root)]
    (when (.exists f)
      (doseq [^File c (reverse (walk f))]
        (.delete c)))
    nil))

;; --- copy / move ------------------------------------------------------------------

(defn size [f] (.length (file f)))

(defn copy
  "Copy a regular file. Returns the destination File."
  [src dst & [{:keys [replace-existing]}]]
  (let [s (file src) d (file dst)]
    (when (.isDirectory s)
      (throw (ex-info "copy takes a regular file; use copy-tree" {:path (str s)})))
    (when (and (.exists d) (not replace-existing))
      (throw (ex-info "destination exists" {:path (str d)})))
    (io/copy s d)
    d))

(defn copy-tree
  "Copy a directory tree recursively. Returns the destination File."
  [src dst & [{:keys [replace-existing] :as opts}]]
  (let [s (file src) d (file dst)]
    (if (.isDirectory s)
      (do (create-dirs d)
          (doseq [^File c (.listFiles s)]
            (copy-tree c (File. (str d) (.getName c)) opts)))
      (copy s d opts))
    d))

(defn move
  "Move (rename) src to dst. Falls back to copy+delete for a regular file
  when rename fails (cross-device). Returns the destination File."
  [src dst & [{:keys [replace-existing]}]]
  (let [s (file src) d (file dst)]
    (when (and (.exists d) replace-existing)
      (delete-tree d))
    (cond
      (.renameTo s d) d
      (.isFile s) (do (copy s d {:replace-existing replace-existing})
                      (delete s)
                      d)
      :else (throw (ex-info "could not move" {:src (str s) :dst (str d)})))))

;; --- times -------------------------------------------------------------------------

(defn last-modified-time
  "Last-modified time as a java.time.Instant."
  [f] (java.time.Instant/ofEpochMilli (.lastModified (file f))))

(defn set-last-modified-time
  "Set last-modified from a java.time.Instant."
  [f inst] (.setLastModified (file f) (.toEpochMilli inst)))

(defn creation-time [f]
  (throw (ex-info "creation-time is not supported on this host" {:path (str f)})))

(defn read-link [f]
  (throw (ex-info "read-link is not supported on this host" {:path (str f)})))

;; --- lookup ------------------------------------------------------------------------

(defn which
  "First executable named `nm` on PATH, as a File, else nil."
  [nm]
  (let [dirs (str/split (or (System/getenv "PATH") "")
                        (re-pattern File/pathSeparator))]
    (some (fn [d]
            (let [f (File. d (str nm))]
              (when (and (.isFile f) (.canExecute f))
                (absolutize f))))
          dirs)))
