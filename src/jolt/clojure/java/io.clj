; Jolt Standard Library: clojure.java.io
;
; A Janet-backed shim: file I/O via Janet's file/ and os/ through the janet.*
; interop bridge. It deals in plain path strings and Janet file handles, not
; java.io objects — so JVM-specific interop on the results (.toURL, .lastModified,
; …) won't work, but file/reader/writer/resource/copy/slurp do.

(defn file
  "A file path. With a parent and child, joins them with '/'."
  ([path] (str path))
  ([parent child] (str parent "/" child)))

(defn as-file [x] (str x))

(defn reader [x]
  (cond
    ;; already a reader (java.io shim or janet file handle) — pass through,
    ;; like the JVM's io/reader on a Reader
    (= :jolt/jio-string-reader (get x :jolt/type)) x
    (= :jolt/url (get x :jolt/type)) (java.io.StringReader. (janet/slurp (.getPath x)))
    (= :core/file (janet/type x)) x
    (sequential? x) (java.io.StringReader. (apply str x))   ; char[] → in-memory reader
    ;; a path: an in-memory reader over the file's content — gives the
    ;; java.io.Reader surface (.read/.mark/.reset) plus line-seq's
    ;; :read-line-fn, which a raw janet file handle has neither of
    :else (java.io.StringReader. (janet/slurp (str x)))))
(defn writer [x] (janet.file/open (str x) :w))
(defn input-stream [x] (reader x))
(defn output-stream [x] (writer x))

(defn resource
  "Returns a slurp-able path for `path` if it exists, else nil. (Clojure
  returns a classpath URL; here the loader's source roots play the classpath
  role — the bare path is tried first, then path under each root.)"
  [path]
  (let [p (str path)]
    (if (janet.os/stat p)
      p
      (loop [roots (seq (__source-roots))]
        (when roots
          (let [cand (str (first roots) "/" p)]
            (if (janet.os/stat cand)
              cand
              (recur (next roots)))))))))

(defn delete-file
  ([f] (delete-file f false))
  ([f silently]
   (try (do (janet.os/rm (str f)) true)
        (catch Throwable e (if silently false (throw e))))))

(defn make-parents
  "Create the parent directories of `f`."
  [f]
  (let [path (str f)
        i (clojure.string/last-index-of path "/")]
    (when (and i (pos? i))
      (let [parent (subs path 0 i)]
        (make-parents parent)
        (when-not (janet.os/stat parent) (janet.os/mkdir parent))))))

(defn copy
  "Copy from a path/handle `in` to a path/handle `out`."
  [in out]
  (let [content (if (string? in) (slurp in) (janet.file/read in :all))]
    (if (string? out) (spit out content) (janet.file/write out content))))
