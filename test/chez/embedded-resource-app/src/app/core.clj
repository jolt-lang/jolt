(ns app.core
  (:require [clojure.string :as str]))

(defn check [label got want]
  (when-not (= got want)
    (throw (ex-info (str label ": want " (pr-str want) ", got " (pr-str got))
                    {:label label :got got :want want}))))

(defn jar-name [url]
  (some-> url
          .getFile
          (str/replace "+" "%2B")
          (java.net.URLDecoder/decode "UTF-8")
          (.split "!/")
          first
          (.replaceFirst "file:" "")))

(defn jar-test []
  (let [loader (clojure.lang.RT/baseLoader)
        url (.getResource loader "migrations/")
        other-url (.getResource loader "migrations/")
        urls (enumeration-seq (.getResources loader "migrations/"))
        jar (java.util.jar.JarFile. (jar-name url))
        entries (enumeration-seq (.entries jar))
        entry (.getEntry jar "migrations/001-create-users.up.sql")]
    (check "directory protocol" (.getProtocol url) "jar")
    (check "URL equals method" (.equals url other-url) true)
    (check "URL Jolt equality" (= url other-url) true)
    (check "URL hash equality" (contains? #{url} other-url) true)
    (check "directory enumeration" (mapv str urls) [(str url)])
    (check "directory URL equality" (= urls [url]) true)
    (check "archive entries"
           (mapv (fn [entry] (.getName entry)) (enumeration-seq (.entries jar)))
           ["greeting.txt"
            "migrations-old/"
            "migrations-old/ignore.sql"
            "migrations/"
            "migrations/001-create-users.down.sql"
            "migrations/001-create-users.up.sql"
            "migrations/init.sql"
            "migrations/nested/"
            "migrations/nested/002-add-email.up.sql"])
    (check "synthetic directory" (.isDirectory (.getEntry jar "migrations/")) true)
    (check "entry stream" (slurp (.getInputStream jar entry))
           "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);\n")
    (check "directory without trailing slash" (some? (.getResource loader "migrations")) true)
    (check "sibling directory" (some? (.getResource loader "migrations-old/")) true)
    (check "missing directory" (.getResource loader "missing/") nil)
    ;; JarFile's constructors: no charset, and a mode only after verify
    (doseq [archive [(java.util.jar.JarFile. (java.io.File. "jolt-embedded-resources.jar"))
                     (java.util.jar.JarFile. "jolt-embedded-resources.jar" true)
                     (java.util.jar.JarFile. (java.io.File. "jolt-embedded-resources.jar")
                                             true java.util.zip.ZipFile/OPEN_READ)
                     (java.util.jar.JarFile. (java.io.File. "jolt-embedded-resources.jar")
                                             true java.util.zip.ZipFile/OPEN_READ (Runtime/version))]]
      (check "constructor archive name" (.getName archive) "jolt-embedded-resources.jar")
      (check "constructor entries" (count (enumeration-seq (.entries archive))) 9)
      (.close archive))
    (.close jar)
    (.close jar)
    (check "closed archive" (try (.entries jar) :not-closed
                                 (catch java.lang.IllegalStateException _ :closed))
           :closed)
    (check "closed enumeration snapshot"
           (try (count entries) :not-closed
                (catch java.lang.IllegalStateException _ :closed))
           :closed)
    (prn :jar-ok)))

(defn nio-test []
  (let [uri (java.net.URI. "jar:file:jolt-embedded-resources.jar")
        fs (java.nio.file.FileSystems/newFileSystem uri {})]
    (check "filesystem lookup" (identical? fs (java.nio.file.FileSystems/getFileSystem uri)) true)
    (check "duplicate open"
           (try (java.nio.file.FileSystems/newFileSystem uri {}) :not-refused
                (catch java.nio.file.FileSystemAlreadyExistsException _ :already-open))
           :already-open)
    (check "read only" (.isReadOnly fs) true)
    (check "open" (.isOpen fs) true)
    (check "separator" (.getSeparator fs) "/")
    (check "roots" (mapv str (.getRootDirectories fs)) ["/"])
    (let [path (.getPath fs "/migrations/nested/../001-create-users.up.sql" (make-array String 0))
          normalized (.normalize path)
          parent (.getParent normalized)
          filename (.getFileName normalized)
          resolved (.resolve parent "other.sql")
          sibling (.resolveSibling normalized "other.sql")
          relative (.relativize parent resolved)
          subpath (.subpath normalized 0 1)
          segments (iterator-seq (.iterator normalized))]
      (check "path owner" (identical? fs (.getFileSystem path)) true)
      (check "normalize owner" (identical? fs (.getFileSystem normalized)) true)
      (check "normalized path" (str normalized) "/migrations/001-create-users.up.sql")
      (check "path root" (str (.getRoot normalized)) "/")
      (check "path name" (str (.getName normalized 1)) "001-create-users.up.sql")
      (check "path hash" (= (.hashCode normalized) (.hashCode (.normalize path))) true)
      (check "path resolve varargs"
             [(str (.resolve parent "other.sql" (make-array String 0)))
              (str (.resolve parent "other" (into-array String ["nested.sql"])))]
             ["/migrations/other.sql" "/migrations/other/nested.sql"])
      (check "parent owner" (identical? fs (.getFileSystem parent)) true)
      (check "filename owner" (identical? fs (.getFileSystem filename)) true)
      (check "ZIP backslash is not a separator"
             (str (.getFileName (.getPath fs "/migrations/backup\\001.sql" (make-array String 0))))
             "backup\\001.sql")
      (check "resolve owner" (identical? fs (.getFileSystem resolved)) true)
      (check "resolve sibling owner" (identical? fs (.getFileSystem sibling)) true)
      (check "relativize owner" (identical? fs (.getFileSystem relative)) true)
      (check "subpath owner" (identical? fs (.getFileSystem subpath)) true)
      (check "iterator owners" (mapv #(identical? fs (.getFileSystem %)) segments) [true true])
      (check "matcher" (.matches (.getPathMatcher fs "glob:*.sql") filename) true)
      (check "default filesystem is distinct"
             (= normalized (.getPath (java.nio.file.FileSystems/getDefault)
                                     "/migrations/001-create-users.up.sql"
                                     (make-array String 0)))
             false)
      (check "path URI" (str (.toUri normalized))
             "jar:file:jolt-embedded-resources.jar!/migrations/001-create-users.up.sql")
      (check "compareTo rejects non-path"
             (try (.compareTo normalized "not-a-path") :not-refused
                  (catch java.lang.ClassCastException _ :class-cast))
             :class-cast)
      (check "toFile unsupported"
             (try (.toFile normalized) :not-refused
                  (catch java.lang.UnsupportedOperationException _ :unsupported))
             :unsupported))
    (.close fs)
    (check "closed" (.isOpen fs) false)
    (check "closed lookup"
           (try (java.nio.file.FileSystems/getFileSystem uri) :still-open
                (catch java.nio.file.FileSystemNotFoundException _ :not-found))
           :not-found)
    (let [reopened (java.nio.file.FileSystems/newFileSystem uri {})]
      (check "reopen" (.isOpen reopened) true)
      (.close reopened))
    (prn :nio-ok)))

(defn expect-throw [label cls thunk]
  (check label
         (try (thunk) :not-thrown
              (catch Throwable t (if (instance? cls t) :thrown (throw t))))
         :thrown))

(defn expect-read-only [label thunk]
  (expect-throw label java.nio.file.ReadOnlyFileSystemException thunk))

(defn nio-read-test []
  (let [uri (java.net.URI. "jar:file:jolt-embedded-resources.jar")
        fs (java.nio.file.FileSystems/newFileSystem uri {})
        root (.getPath fs "/" (make-array String 0))
        dir (.getPath fs "/migrations" (make-array String 0))
        path (.getPath fs "/migrations/001-create-users.up.sql" (make-array String 0))
        relative (.getPath fs "migrations/001-create-users.up.sql" (make-array String 0))
        missing (.getPath fs "/missing.sql" (make-array String 0))
        default-path (.getPath (java.nio.file.FileSystems/getDefault)
                               "/migrations/001-create-users.up.sql"
                               (make-array String 0))
        content "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);\n"]
    (check "root exists" (java.nio.file.Files/exists root) true)
    (check "directory exists" (java.nio.file.Files/exists dir) true)
    (check "file exists" (java.nio.file.Files/exists path) true)
    (check "missing exists" (java.nio.file.Files/exists missing) false)
    (check "missing notExists" (java.nio.file.Files/notExists missing) true)
    (check "directory type" [(java.nio.file.Files/isDirectory dir)
                             (java.nio.file.Files/isRegularFile dir)] [true false])
    (check "file type" [(java.nio.file.Files/isDirectory path)
                        (java.nio.file.Files/isRegularFile path)] [false true])
    (check "access" [(java.nio.file.Files/isReadable path)
                     (java.nio.file.Files/isWritable path)
                     (java.nio.file.Files/isExecutable path)] [true false false])
    (check "not hidden" (java.nio.file.Files/isHidden path) false)
    (check "not symlink" (java.nio.file.Files/isSymbolicLink path) false)
    (check "size" (java.nio.file.Files/size path) (count content))
    (check "bytes" (String. (java.nio.file.Files/readAllBytes path) "UTF-8") content)
    (check "relative bytes never hit host"
           (String. (java.nio.file.Files/readAllBytes relative) "UTF-8") content)
    (check "relative absolute path" (str (.toAbsolutePath relative))
           "/migrations/001-create-users.up.sql")
    (check "relative absolute owner" (identical? fs (.getFileSystem (.toAbsolutePath relative))) true)
    (check "relative real path" (str (.toRealPath relative))
           "/migrations/001-create-users.up.sql")
    (check "lines" (vec (java.nio.file.Files/readAllLines path))
           ["CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);"])
    (with-open [in (java.nio.file.Files/newInputStream path)]
      (check "stream" (slurp in) content))
    (with-open [children (java.nio.file.Files/newDirectoryStream dir)]
      (check "sorted children" (mapv str children)
             ["/migrations/001-create-users.down.sql"
              "/migrations/001-create-users.up.sql"
              "/migrations/init.sql"
              "/migrations/nested"]))
    (with-open [children (java.nio.file.Files/newDirectoryStream dir "*.sql")]
      (check "glob children" (mapv str children)
             ["/migrations/001-create-users.down.sql"
              "/migrations/001-create-users.up.sql"
              "/migrations/init.sql"]))
    (with-open [children (java.nio.file.Files/newDirectoryStream
                          dir
                          (reify java.nio.file.DirectoryStream$Filter
                            (accept [_ entry] (str/ends-with? (str entry) "nested"))))]
      (check "filter children" (mapv str children) ["/migrations/nested"]))
    (let [visited (atom [])
          visitor (reify java.nio.file.FileVisitor
                    (preVisitDirectory [_ path _]
                      (swap! visited conj [:pre (str path)])
                      java.nio.file.FileVisitResult/CONTINUE)
                    (visitFile [_ path _]
                      (swap! visited conj [:file (str path)])
                      java.nio.file.FileVisitResult/CONTINUE)
                    (postVisitDirectory [_ path _]
                      (swap! visited conj [:post (str path)])
                      java.nio.file.FileVisitResult/CONTINUE)
                    (visitFileFailed [_ path error]
                      (throw error)))]
      (java.nio.file.Files/walkFileTree dir #{} 10 visitor)
      (check "tree walk" @visited
             [[:pre "/migrations"]
              [:file "/migrations/001-create-users.down.sql"]
              [:file "/migrations/001-create-users.up.sql"]
              [:file "/migrations/init.sql"]
              [:pre "/migrations/nested"]
              [:file "/migrations/nested/002-add-email.up.sql"]
              [:post "/migrations/nested"]
              [:post "/migrations"]]))
    (let [attrs (java.nio.file.Files/readAttributes path "basic:*")]
      (check "attribute size" (get attrs "size") (count content))
      (check "attribute fileKey" (get attrs "fileKey") nil)
      (check "attribute type" (get attrs "isRegularFile") true))
    (check "attribute time" (.toMillis (java.nio.file.Files/getLastModifiedTime path)) 0)
    (check "single attribute" (java.nio.file.Files/getAttribute path "basic:size") (count content))
    (check "same file" (java.nio.file.Files/isSameFile path (.normalize path)) true)
    (check "different file" (java.nio.file.Files/isSameFile path dir) false)
    (check "different filesystems" (java.nio.file.Files/isSameFile path default-path) false)
    (check "different filesystems reversed" (java.nio.file.Files/isSameFile default-path path) false)
    (check "equal missing paths" (java.nio.file.Files/isSameFile missing (.normalize missing)) true)
    (expect-throw "existing and missing comparison" java.nio.file.NoSuchFileException
                  #(java.nio.file.Files/isSameFile path missing))
    (expect-throw "distinct missing comparison" java.nio.file.NoSuchFileException
                  #(java.nio.file.Files/isSameFile
                    missing
                    (.getPath fs "/also-missing.sql" (make-array String 0))))
    (check "real path owner" (identical? fs (.getFileSystem (.toRealPath path))) true)
    (let [out (java.io.ByteArrayOutputStream.)]
      (java.nio.file.Files/copy path out)
      (check "copy to stream" (.toString out "UTF-8") content))
    (expect-throw "missing read" java.nio.file.NoSuchFileException
                  #(java.nio.file.Files/readAllBytes missing))
    (expect-throw "missing real path" java.nio.file.NoSuchFileException
                  #(.toRealPath missing))
    (with-open [channel (java.nio.channels.FileChannel/open path)]
      (let [buffer (java.nio.ByteBuffer/allocate 4)]
        (check "embedded FileChannel" (.read channel buffer) 4)
        (.flip buffer)
        (check "embedded FileChannel bytes" (String. (.array buffer) "UTF-8") "CREA")))
    (expect-throw "file as directory" java.nio.file.NotDirectoryException
                  #(java.nio.file.Files/newDirectoryStream path))
    (doseq [[label thunk] [["delete" #(java.nio.file.Files/delete path)]
                           ["deleteIfExists" #(java.nio.file.Files/deleteIfExists path)]
                           ["write" #(java.nio.file.Files/write path ["x"])]
                           ["newOutputStream" #(java.nio.file.Files/newOutputStream path)]
                           ["createDirectory" #(java.nio.file.Files/createDirectory dir)]
                           ["createDirectories" #(java.nio.file.Files/createDirectories dir)]
                           ["createFile" #(java.nio.file.Files/createFile path)]
                           ["createTempFile" #(java.nio.file.Files/createTempFile dir "prefix" ".sql")]
                           ["createTempDirectory" #(java.nio.file.Files/createTempDirectory dir "prefix")]
                           ["createSymbolicLink" #(java.nio.file.Files/createSymbolicLink path path)]
                           ["createLink" #(java.nio.file.Files/createLink path path)]
                           ["move" #(java.nio.file.Files/move path missing)]
                           ["setLastModifiedTime" #(java.nio.file.Files/setLastModifiedTime
                                                   path (java.nio.file.attribute.FileTime/fromMillis 0))]
                           ["setAttribute" #(java.nio.file.Files/setAttribute
                                             path "basic:lastModifiedTime"
                                             (java.nio.file.attribute.FileTime/fromMillis 0))]
                           ["setPosixFilePermissions" #(java.nio.file.Files/setPosixFilePermissions path #{})]
                           ["copy destination" #(java.nio.file.Files/copy path missing)]]]
      (expect-read-only label thunk))
    (expect-throw "unsupported owner" java.lang.UnsupportedOperationException
                  #(java.nio.file.Files/getOwner path))
    (.close fs)
    (expect-throw "closed read" java.nio.file.ClosedFileSystemException
                  #(java.nio.file.Files/readAllBytes path))
    (prn :nio-read-ok)))

(defn embedded-file-system [jar]
  (let [uri (java.net.URI. (str "jar:file:" (.getName jar)))
        open #(try (java.nio.file.FileSystems/getFileSystem uri)
                   (catch java.nio.file.FileSystemNotFoundException _
                     (java.nio.file.FileSystems/newFileSystem uri {})))]
    (or (try (open) (catch java.nio.file.FileSystemAlreadyExistsException _ nil))
        (open))))

(defn script-excluded? [script jar exclusions]
  (when (seq exclusions)
    (let [fs (embedded-file-system jar)
          path (.getPath fs script (make-array String 0))]
      (some (fn [pattern]
              (.matches (.getPathMatcher fs (str "glob:" pattern)) path))
            exclusions))))

(defn find-migration-resources [dir jar exclusions]
  (vec
   (keep (fn [entry]
           (let [entry-name (.getName entry)
                 relative (.replaceAll entry-name dir "")
                 slash (str/last-index-of relative "/")
                 filename (subs relative (if slash (inc slash) 0))]
             (when (and (str/starts-with? entry-name dir)
                        (not (.isDirectory entry))
                        (not (script-excluded? filename jar exclusions)))
               [filename (slurp (.getInputStream jar entry))])))
         (enumeration-seq (.entries jar)))))

(defn migratus-test []
  (let [loader (java.lang.ClassLoader/getSystemClassLoader)
        url (.getResource loader "migrations/")
        jar (java.util.jar.JarFile. (jar-name url))
        found (find-migration-resources "migrations/" jar #{"init.sql"})]
    (check "jar protocol" (.getProtocol url) "jar")
    (check "migration resources" found
           [["001-create-users.down.sql" "DROP TABLE users;\n"]
            ["001-create-users.up.sql" "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);\n"]
            ["002-add-email.up.sql" "ALTER TABLE users ADD COLUMN email TEXT;\n"]])
    (check "migrations-old is not included"
           (some (fn [[name _]] (= name "ignore.sql")) found) nil)
    (.close (embedded-file-system jar))
    (.close jar)
    (prn :migratus-ok)))

(defn -main [& args]
  (case (first args)
    "--names" (prn (jolt.host/embedded-resource-names))
    "--jar" (jar-test)
    "--nio" (nio-test)
    "--nio-read" (nio-read-test)
    "--migratus" (migratus-test)
    nil))
