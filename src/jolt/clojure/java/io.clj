; Jolt Standard Library: clojure.java.io
;
; A Janet-backed shim: file I/O via Janet's file/ and os/ through the janet.*
; interop bridge. It deals in plain path strings and Janet file handles, not
; java.io objects — so JVM-specific interop on the results (.toURL, .lastModified,
; …) won't work, but file/reader/writer/resource/copy/slurp do.

(defn file
  "A java.io.File. With a parent and child, joins them with '/'. Returns a
  :jolt/file value (instance? File true) with the File method surface; str/slurp
  and io/reader coerce it back to its path."
  ([path] (__make-file path))
  ([parent child] (__make-file parent child)))

(defn as-file [x] (if (__file? x) x (__make-file x)))

(defn as-url
  "Coerce `x` to a java.net.URL value (a :jolt/url). Strings are parsed."
  [x]
  (if (= :jolt/url (get x :jolt/type)) x (java.net.URL. (str x))))

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
(defn writer [x]
  (cond
    ;; already a Writer/StringWriter shim — pass through, like the JVM's
    ;; io/writer on a Writer (markdown-clj's md-to-html writes into a StringWriter)
    (= :jolt/writer (get x :jolt/type)) x
    (= :core/file (janet/type x)) x          ; already a janet file handle
    :else (janet.file/open (str x) :w)))     ; a path
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
  "Copy from `in` to `out`. A value carrying a truthy `:jolt/output-stream`
  marker is treated as a java.io OutputStream (write via its .write); a
  `:jolt/input-stream` source is pumped through its .read. This lets host-shim
  libraries (e.g. jolt-lang/http-client's byte streams) participate without core
  knowing their concrete types. Falls back to the original path/handle copy."
  [in out & opts]
  (cond
    (get out :jolt/output-stream)
    (if (get in :jolt/input-stream)
      (let [buf (byte-array 8192)]
        (loop []
          (let [n (.read in buf 0 8192)]
            (when (not= -1 n)
              (.write out buf 0 n)
              (recur)))))
      ;; in is a byte-array / string
      (.write out in))
    :else
    (let [content (if (string? in) (slurp in) (janet.file/read in :all))]
      (if (string? out) (spit out content) (janet.file/write out content)))))
