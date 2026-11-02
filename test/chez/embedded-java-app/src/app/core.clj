(ns app.core
  (:require [clojure.java.io :as io]))

(defn check [label got want]
  (when-not (= got want)
    (throw (ex-info (str label ": want " (pr-str want) ", got " (pr-str got))
                    {:label label :got got :want want}))))

(defn expect-throw [label class thunk]
  (check label (try (thunk) :not-thrown
                    (catch Throwable error
                      (if (instance? class error) :thrown (throw error))))
         :thrown))

(defn url-test []
  (let [resource (io/resource "plain.txt")
        other (io/resource "plain.txt")
        connection (.openConnection resource)
        proxy-connection (.openConnection resource java.net.Proxy/NO_PROXY)
        content "embedded resource ok\n"]
    (check "connection class" (instance? java.net.JarURLConnection connection) true)
    (check "proxy connection class" (instance? java.net.JarURLConnection proxy-connection) true)
    (expect-throw "nil proxy rejected" java.lang.IllegalArgumentException #(.openConnection resource nil))
    (expect-throw "wrong proxy rejected" java.lang.ClassCastException #(.openConnection resource "not-a-proxy"))
    (expect-throw "negative connect timeout" java.lang.IllegalArgumentException #(.setConnectTimeout connection -1))
    (expect-throw "negative read timeout" java.lang.IllegalArgumentException #(.setReadTimeout connection -1))
    (check "entry name" (.getEntryName connection) "plain.txt")
    (check "jar file URL" (str (.getJarFileURL connection)) "file:jolt-embedded-resources.jar")
    (check "jar file" (instance? java.util.jar.JarFile (.getJarFile connection)) true)
    (check "jar entry" (.getName (.getJarEntry connection)) "plain.txt")
    (check "manifest present" (some? (.getManifest connection)) true)
    (check "entry attributes" (.getValue (.getAttributes connection) "X-Entry") "yes")
    (check "main attributes" (.getValue (.getMainAttributes connection) "Multi-Release") "true")
    (check "certificates" (count (.getCertificates connection)) 0)
    (check "content length" (.getContentLength connection) (count content))
    (check "content length long" (.getContentLengthLong connection) (count content))
    (check "content type" (.getContentType connection) "text/plain")
    (check "content encoding" (.getContentEncoding connection) nil)
    (check "expiration" (.getExpiration connection) 0)
    (check "date" (.getDate connection) 0)
    (check "last modified" (.getLastModified connection) 0)
    (check "headers" (.getHeaderFields connection) {})
    (check "header" (.getHeaderField connection "X-Test") nil)
    (check "header int" (.getHeaderFieldInt connection "X-Test" -1) -1)
    (check "header long" (.getHeaderFieldLong connection "X-Test" -2) -2)
    (check "header date" (.getHeaderFieldDate connection "X-Test" 42) 42)
    (check "header key" (.getHeaderFieldKey connection 0) nil)
    (check "indexed header" (.getHeaderField connection 0) nil)
    (check "content" (slurp (.getContent connection)) content)
    (check "typed content" (slurp (.getContent connection (into-array Class [java.io.InputStream]))) content)
    (check "permission" (some? (.getPermission connection)) true)
    (check "URL" (= resource (.getURL connection)) true)
    (.setConnectTimeout connection 123)
    (.setReadTimeout connection 456)
    (check "connect timeout" (.getConnectTimeout connection) 123)
    (check "read timeout" (.getReadTimeout connection) 456)
    (check "doInput default" (.getDoInput connection) true)
    (.setDoInput connection false)
    (check "doInput set" (.getDoInput connection) false)
    (check "doOutput default" (.getDoOutput connection) false)
    (.setDoOutput connection true)
    (check "doOutput set" (.getDoOutput connection) true)
    (check "output rejected"
           (try (.getOutputStream connection) :not-rejected
                (catch java.io.IOException _ :rejected))
           :rejected)
    (.setDoOutput connection false)
    (.setAllowUserInteraction connection true)
    (check "user interaction" (.getAllowUserInteraction connection) true)
    (.setUseCaches connection false)
    (check "use caches" (.getUseCaches connection) false)
    (.setDefaultUseCaches connection false)
    (check "default use caches" (.getDefaultUseCaches connection) false)
    (.setIfModifiedSince connection 42)
    (check "if modified since" (.getIfModifiedSince connection) 42)
    (.setRequestProperty connection "X-Test" "one")
    (.addRequestProperty connection "X-Test" "two")
    (check "request property" (.getRequestProperty connection "X-Test") "one")
    (check "request properties" (get (.getRequestProperties connection) "X-Test") ["one" "two"])
    (.connect connection)
    (check "connected request property rejected"
           (try (.setRequestProperty connection "X-Other" "x") :not-rejected
                (catch java.lang.IllegalStateException _ :rejected))
           :rejected)
    (check "connection string" (string? (str connection)) true)
    (check "URL protocol" (.getProtocol resource) "jar")
    (check "URL string" (str resource) "jar:file:jolt-embedded-resources.jar!/plain.txt")
    (check "URL path" (.getPath resource) "file:jolt-embedded-resources.jar!/plain.txt")
    (check "URL file" (.getFile resource) "file:jolt-embedded-resources.jar!/plain.txt")
    (check "URL host" (.getHost resource) "")
    (check "URL authority" (.getAuthority resource) nil)
    (check "URL port" (.getPort resource) -1)
    (check "URL default port" (.getDefaultPort resource) -1)
    (check "URL query" (.getQuery resource) nil)
    (check "URL user info" (.getUserInfo resource) nil)
    (check "URL ref" (.getRef resource) nil)
    (check "URL external form" (.toExternalForm resource) "jar:file:jolt-embedded-resources.jar!/plain.txt")
    (check "URL URI" (str (.toURI resource)) "jar:file:jolt-embedded-resources.jar!/plain.txt")
    (check "URL same file" (.sameFile resource other) true)
    (check "URL stream" (slurp (.openStream resource)) content)
    (check "URL content" (slurp (.getContent resource)) content)
    (check "URL typed content" (slurp (.getContent resource (into-array Class [java.io.InputStream]))) content)
    (prn :url-ok)))

(defn resource-url-test []
  ;; A class loader opens a multi-release jar at the runtime version, so the
  ;; resource it answers is the entry that version selects.
  (let [greeting (io/resource "greeting.txt")]
    (check "multi-release resource URL" (str greeting)
           "jar:file:jolt-embedded-resources.jar!/META-INF/versions/17/greeting.txt")
    (check "multi-release resource content" (slurp greeting) "version 17\n")
    (check "multi-release entry name" (.getEntryName (.openConnection greeting))
           "META-INF/versions/17/greeting.txt"))
  (check "below base version never selected" (io/resource "below7.txt") nil)
  ;; (URL. context spec) resolves against a resource's entry, as the jar
  ;; handler does: a sibling, ".." and "." segments dropped, "/" from the root
  (let [migration (io/resource "migrations/001-create-users.up.sql")]
    (check "relative resource URL" (str (java.net.URL. (io/resource "plain.txt") "data.bin"))
           "jar:file:jolt-embedded-resources.jar!/data.bin")
    (check "relative resource URL with .." (str (java.net.URL. migration "../plain.txt"))
           "jar:file:jolt-embedded-resources.jar!/plain.txt")
    (check "relative resource URL from the root" (str (java.net.URL. migration "/plain.txt"))
           "jar:file:jolt-embedded-resources.jar!/plain.txt")
    (check "relative resource URL content" (slurp (java.net.URL. migration "../plain.txt"))
           "embedded resource ok\n")
    (check "relative resource URL is a URL" (instance? java.net.URL (java.net.URL. migration "x")) true))
  ;; nothing in the archive answers the empty name (a source root on disk may)
  (check "empty resource name" (= "jar" (some-> (io/resource "") .getProtocol)) false)
  (check "runtime version is the modeled Java version"
         (str (java.util.jar.JarFile/runtimeVersion)) (str (.feature (Runtime/version))))
  ;; ParseUtil.encodePath: lowercase escapes, ";" and "=" escaped too
  (let [name "encoded/001 my café;v=1.sql"
        url (io/resource name)]
    (check "encoded URL" (str url)
           "jar:file:jolt-embedded-resources.jar!/encoded/001%20my%20caf%c3%a9%3bv%3d1.sql")
    (check "encoded URL content" (slurp url) "encoded\n")
    (check "URL rebuilt from its text" (slurp (java.net.URL. (str url))) "encoded\n")
    (check "URL text as a source" (slurp (str url)) "encoded\n")
    (check "encoded URL entry" (.getEntryName (.openConnection url)) name)
    (check "encoded URL URI" (str (.toURI url))
           "jar:file:jolt-embedded-resources.jar!/encoded/001%20my%20caf%c3%a9%3bv%3d1.sql")
    (check "encoded directory URL" (str (io/resource "encoded"))
           "jar:file:jolt-embedded-resources.jar!/encoded"))
  ;; a zipfs Path.toUri quotes only what a URI cannot hold, uppercase, and keeps
  ;; other non-ASCII; Path.of(URI) reads it back
  (let [fs (java.nio.file.FileSystems/newFileSystem (java.net.URI. "jar:file:jolt-embedded-resources.jar") {})
        path (.getPath fs "/encoded/001 my café;v=1.sql" (make-array String 0))
        uri (.toUri path)]
    (check "encoded path URI" (str uri)
           "jar:file:jolt-embedded-resources.jar!/encoded/001%20my%20café;v=1.sql")
    (check "encoded path URI round trip" (= path (java.nio.file.Path/of uri)) true)
    (check "encoded path read" (java.nio.file.Files/readString (java.nio.file.Path/of uri)) "encoded\n")
    (check "provider path from URI" (str (.getPath (.provider fs) uri)) "/encoded/001 my café;v=1.sql")
    (.close fs))
  (prn :resource-url-ok))

(defn jar-full-test []
  (let [jar (java.util.jar.JarFile. "jolt-embedded-resources.jar")
        entry (.getJarEntry jar "greeting.txt")
        manifest (.getManifest jar)
        main (.getMainAttributes manifest)]
    (check "archive comment" (.getComment jar) nil)
    (check "archive size" (.size jar) 21)
    ;; ZipFile has no toString of its own in JDK 21: Object's
    (check "archive string" (.startsWith (.toString jar) "java.util.jar.JarFile@") true)
    (check "entry constructors"
           (mapv #(.getName %)
                 [(java.util.zip.ZipEntry. "x")
                  (java.util.zip.ZipEntry. (java.util.zip.ZipEntry. "x"))
                  (java.util.jar.JarEntry. "x")
                  (java.util.jar.JarEntry. (java.util.zip.ZipEntry. "x"))
                  (java.util.jar.JarEntry. (java.util.jar.JarEntry. "x"))])
           ["x" "x" "x" "x" "x"])
    (check "multi release" (.isMultiRelease jar) true)
    (check "runtime version" (str (.getVersion jar)) "8")
    (check "base version" (str (java.util.jar.JarFile/baseVersion)) "8")
    (check "current version" (str (java.util.jar.JarFile/runtimeVersion)) "21")
    (check "manifest cached" (identical? manifest (.getManifest jar)) true)
    (check "default selected content" (slurp (.getInputStream jar entry)) "embedded resource ok\n")
    (check "entry real name" (.getRealName entry) "greeting.txt")
    (check "entry attributes" (.getValue (.getAttributes entry) "X-Entry") "yes")
    (check "entry certificates" (count (.getCertificates entry)) 0)
    (check "entry signers" (count (.getCodeSigners entry)) 0)
    (check "raw stream versions"
           (mapv #(.getName %)
                 (filter #(.startsWith (.getName %) "META-INF/versions/")
                         (iterator-seq (.iterator (.stream jar)))))
           ["META-INF/versions/" "META-INF/versions/17/" "META-INF/versions/17/greeting.txt"
            "META-INF/versions/7/" "META-INF/versions/7/below7.txt"
            "META-INF/versions/8/" "META-INF/versions/8/below.txt"
            "META-INF/versions/9/" "META-INF/versions/9/greeting.txt"
            "META-INF/versions/9/notgreeting.txt" "META-INF/versions/9/only.txt"])
    (check "versioned stream"
           (mapv #(.getName %) (iterator-seq (.iterator (.versionedStream jar))))
           ["META-INF/" "META-INF/MANIFEST.MF" "data.bin" "encoded/" "encoded/001 my café;v=1.sql"
            "greeting.txt" "latin1-lines.bin" "migrations/" "migrations/001-create-users.up.sql" "plain.txt"])
    (doseq [[version expected] [["8" "embedded resource ok\n"]
                                ["9" "version 9\n"]
                                ["17" "version 17\n"]
                                ["22" "version 17\n"]]]
      (let [versioned (java.util.jar.JarFile. (java.io.File. "jolt-embedded-resources.jar")
                                              true 1 (Runtime$Version/parse version))
            selected (.getJarEntry versioned "greeting.txt")]
        (check (str "version " version)
               (slurp (.getInputStream versioned selected)) expected)
        (check (str "version real name " version) (.getRealName selected)
               (if (= version "8") "greeting.txt"
                   (str "META-INF/versions/" (if (= version "22") "17" version) "/greeting.txt")))
        (check (str "version only " version)
               (some-> (.getJarEntry versioned "only.txt") (#(slurp (.getInputStream versioned %))))
               (when-not (= version "8") "version only\n"))
        (check (str "base version selection " version)
               (some-> (.getJarEntry versioned "below.txt") (#(slurp (.getInputStream versioned %))))
               (when-not (= version "8") "below nine\n"))
        (check (str "below base version ignored " version) (.getJarEntry versioned "below7.txt") nil)
        (check (str "version value " version) (str (.getVersion versioned)) version)
        (.close versioned)))
    (doseq [archive [(java.util.jar.JarFile. "jolt-embedded-resources.jar" true)
                     (java.util.jar.JarFile. (java.io.File. "jolt-embedded-resources.jar") true)
                     (java.util.jar.JarFile. (java.io.File. "jolt-embedded-resources.jar") true 1)]]
      (check "constructor variants" (.getName archive) "jolt-embedded-resources.jar")
      (.close archive))
    (check "manifest version" (.getValue main "Manifest-Version") "1.0")
    (check "manifest multi release" (.getValue main "Multi-Release") "true")
    (check "manifest main name" (.getValue main "Main-Name") "Example")
    (check "manifest continuation" (.getValue main "Long-Header") "first second")
    (check "manifest unicode" (.getValue main "X-Unicode") "café")
    (check "manifest entry" (.getValue (.getAttributes manifest "greeting.txt") "X-Entry") "yes")
    (check "manifest entry case sensitive" (.getAttributes manifest "GREETING.TXT") nil)
    (check "manifest entries" (contains? (.getEntries manifest) "greeting.txt") true)
    (check "attribute name equality"
           (= (java.util.jar.Attributes$Name. "Multi-Release")
              (java.util.jar.Attributes$Name. "multi-release")) true)
    (check "attribute name hash"
           (= (.hashCode (java.util.jar.Attributes$Name. "Multi-Release"))
              (.hashCode (java.util.jar.Attributes$Name. "multi-release"))) true)
    (check "attribute name string" (str (java.util.jar.Attributes$Name. "Multi-Release")) "Multi-Release")
    (check "attribute name lookup" (.getValue main (java.util.jar.Attributes$Name. "multi-release")) "true")
    (let [copy (java.util.jar.Manifest. manifest)
          out (java.io.ByteArrayOutputStream.)]
      (check "manifest copy equality" (= copy manifest) true)
      (check "manifest hash equality" (= (.hashCode copy) (.hashCode manifest)) true)
      (check "manifest copy not identical" (identical? copy manifest) false)
      (.putValue (.getMainAttributes copy) "X-Wrap" (apply str (repeat 100 "é")))
      (.write copy out)
      (let [text (.toString out "UTF-8")
            reread (java.util.jar.Manifest. (java.io.ByteArrayInputStream. (.toByteArray out)))]
        (check "manifest write version" (.contains text "Manifest-Version: 1.0") true)
        (check "manifest write multi release" (.contains text "Multi-Release: true") true)
        (check "manifest utf8 line wrapping"
               (every? #(<= (alength (.getBytes % "UTF-8")) 72) (.split text "\r\n")) true)
        (check "manifest wrapped unicode value" (.getValue (.getMainAttributes reread) "X-Wrap")
               (apply str (repeat 100 "é")))
        (check "manifest reread" (= reread copy) true))
      (.clear copy)
      (check "manifest clear main" (.isEmpty (.getMainAttributes copy)) true)
      (check "manifest clear entries" (.isEmpty (.getEntries copy)) true))
    (let [attrs (java.util.jar.Attributes.)]
      (check "empty attributes" (.isEmpty attrs) true)
      (.putValue attrs "A" "one")
      (.put attrs (java.util.jar.Attributes$Name. "B") "two")
      (expect-throw "attribute put needs a Name" java.lang.ClassCastException #(.put attrs "B" "two"))
      (check "attribute get by string" (.get attrs "A") nil)
      (check "attribute get" (.get attrs (java.util.jar.Attributes$Name. "a")) "one")
      (check "attribute contains key" (.containsKey attrs (java.util.jar.Attributes$Name. "A")) true)
      (check "attribute contains value" (.containsValue attrs "one") true)
      (check "attribute size" (.size attrs) 2)
      (check "attribute keys" (count (.keySet attrs)) 2)
      (check "attribute values" (count (.values attrs)) 2)
      (check "attribute entries" (count (.entrySet attrs)) 2)
      (check "attribute hash" (number? (.hashCode attrs)) true)
      (let [keys (.keySet attrs)
            a-name (java.util.jar.Attributes$Name. "A")]
        (check "attribute key type" (every? #(instance? java.util.jar.Attributes$Name %) (seq keys)) true)
        (check "attribute backed key view" (.remove keys a-name) true)
        (check "attribute view removed from map" (.getValue attrs "A") nil)
        (check "attribute backed key size" (.size keys) 1))
      (let [copy (.clone attrs)]
        (.putAll copy (doto (java.util.jar.Attributes.) (.putValue "C" "three")))
        (check "attribute putAll" (.getValue copy "C") "three")
        (check "attribute clone equality" (= copy attrs) false)
        (.remove copy (java.util.jar.Attributes$Name. "B"))
        (check "attribute remove" (.getValue copy "B") nil)
        (.clear copy)
        (check "attribute clear" (.isEmpty copy) true))
      (.clear attrs))
    (let [entry (.getJarEntry jar "greeting.txt")
          copy (.clone entry)]
      (check "embedded entry zero time" (.getTime entry) 0)
      (check "embedded entry zero modified" (.toMillis (.getLastModifiedTime entry)) 0)
      ;; the epoch in the local zone: 1969 west of UTC
      (check "embedded entry local epoch" (contains? #{1969 1970} (.getYear (.getTimeLocal entry))) true)
      (.setTime entry 123456)
      (check "entry time" (.getTime entry) 123456)
      (check "entry local time accessors" [(number? (.getYear (.getTimeLocal entry)))
                                           (number? (.getMonthValue (.getTimeLocal entry)))] [true true])
      (.setTimeLocal entry (java.time.LocalDateTime/of 2020 1 2 3 4 5))
      ;; the DOS fields hold the local time; getTime reads them in the local zone
      (check "entry local time set"
             (str (.getTimeLocal (doto (java.util.zip.ZipEntry. "y") (.setTime (.getTime entry)))))
             "2020-01-02T03:04:05")
      (check "entry local time value" (some? (.getTimeLocal entry)) true)
      (.setLastModifiedTime entry (java.nio.file.attribute.FileTime/fromMillis 1000))
      (.setLastAccessTime entry (java.nio.file.attribute.FileTime/fromMillis 2000))
      (.setCreationTime entry (java.nio.file.attribute.FileTime/fromMillis 3000))
      (check "entry modified" (.toMillis (.getLastModifiedTime entry)) 1000)
      (check "entry access" (.toMillis (.getLastAccessTime entry)) 2000)
      (check "entry creation" (.toMillis (.getCreationTime entry)) 3000)
      (let [metadata-copy (.clone entry)]
        (check "clone modified time" (.toMillis (.getLastModifiedTime metadata-copy)) 1000)
        (check "clone access time" (.toMillis (.getLastAccessTime metadata-copy)) 2000)
        (check "clone creation time" (.toMillis (.getCreationTime metadata-copy)) 3000)
        (check "clone local time" (str (.getTimeLocal metadata-copy)) (str (.getTimeLocal entry))))
      (.setSize entry 42)
      (.setCompressedSize entry 24)
      (.setCrc entry 99)
      (.setMethod entry java.util.zip.ZipEntry/DEFLATED)
      (.setExtra entry (byte-array [1 2]))
      (.setComment entry "note")
      (check "entry size" (.getSize entry) 42)
      (check "entry compressed size" (.getCompressedSize entry) 24)
      (check "entry crc" (.getCrc entry) 99)
      (check "entry method" (.getMethod entry) 8)
      (check "entry extra" (vec (.getExtra entry)) [1 2])
      (check "entry comment" (.getComment entry) "note")
      (check "entry directory" (.isDirectory entry) false)
      (check "entry string" (str entry) "greeting.txt")
      (check "entry hash" (number? (.hashCode entry)) true)
      (check "entry clone" (and (not (identical? copy entry))
                                (= (.getName copy) (.getName entry))) true))
    (.close jar)
    (prn :jar-full-ok)))

(defn fs-full-test []
  (let [uri (java.net.URI. "jar:file:jolt-embedded-resources.jar")
        fs (java.nio.file.FileSystems/newFileSystem uri {})
        path (.getPath fs "greeting.txt" (make-array String 0))
        dir (.getPath fs "migrations" (make-array String 0))
        missing (.getPath fs "missing" (make-array String 0))
        provider (.provider fs)
        watch (.newWatchService fs)
        store (first (.getFileStores fs))]
    (check "provider scheme" (.getScheme provider) "jar")
    (check "provider instance" (identical? provider (.provider fs)) true)
    (check "provider filesystem" (identical? fs (.getFileSystem provider uri)) true)
    (check "provider path" (str (.getPath provider uri)) "/")
    (check "path absolute" (.isAbsolute path) false)
    (check "path name count" (.getNameCount path) 1)
    (check "path from embedded path" (identical? fs (.newFileSystem provider path {})) true)
    (check "filesystem roots" (mapv str (.getRootDirectories fs)) ["/"])
    (check "filesystem views" (.supportedFileAttributeViews fs) #{"basic"})
    (check "file stores" (count (.getFileStores fs)) 1)
    (check "store name" (.name store) "jolt-embedded-resources")
    (check "store type" (.type store) "embedded")
    (check "store read only" (.isReadOnly store) true)
    (check "store total" (.getTotalSpace store) 0)
    (check "store usable" (.getUsableSpace store) 0)
    (check "store unallocated" (.getUnallocatedSpace store) 0)
    (check "store block size" (.getBlockSize store) 0)
    (check "store blocks" (.getBlockCount store) 0)
    (check "store free blocks" (.getFreeBlocks store) 0)
    (check "store available blocks" (.getAvailableBlocks store) 0)
    (check "store basic view" (.supportsFileAttributeView store "basic") true)
    (check "store basic class view" (.supportsFileAttributeView store java.nio.file.attribute.BasicFileAttributeView) true)
    (check "store unsupported view" (.supportsFileAttributeView store "posix") false)
    (check "store attribute" (.getAttribute store "basic:size") nil)
    (check "store attribute view" (.getFileStoreAttributeView store java.nio.file.attribute.BasicFileAttributeView) nil)
    (check "provider file store" (identical? store (.getFileStore provider path)) true)
    (with-open [input (.newInputStream provider path)]
      (check "provider input" (slurp input) "embedded resource ok\n"))
    (with-open [channel (.newFileChannel provider path)]
      (check "provider file channel size" (.size channel) 21))
    (with-open [channel (.newByteChannel provider path)]
      (check "provider byte channel size" (.size channel) 21))
    (check "provider async channel unsupported"
           (try (.newAsynchronousFileChannel provider path nil nil) :not-rejected
                (catch java.lang.UnsupportedOperationException _ :unsupported))
           :unsupported)
    (check "provider output rejected"
           (try (.newOutputStream provider path) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (with-open [children (.newDirectoryStream provider dir)]
      (check "provider directory" (mapv str children) ["migrations/001-create-users.up.sql"]))
    (check "provider exists" (.exists provider path) true)
    (check "provider missing" (.exists provider missing) false)
    (check "provider hidden" (.isHidden provider path) false)
    (check "provider same file" (.isSameFile provider path (.normalize path)) true)
    (check "provider read attributes" (.isRegularFile (.readAttributes provider path java.nio.file.attribute.BasicFileAttributes)) true)
    (check "provider attributes if exists" (.isRegularFile (.readAttributesIfExists provider path java.nio.file.attribute.BasicFileAttributes)) true)
    (check "provider missing attributes" (.readAttributesIfExists provider missing java.nio.file.attribute.BasicFileAttributes) nil)
    (check "provider string attributes" (get (.readAttributes provider path "basic:size") "size") 21)
    (let [attrs (.readAttributes provider path java.nio.file.attribute.BasicFileAttributes)]
      (check "basic modified time" (.toMillis (.lastModifiedTime attrs)) 0)
      (check "basic access time" (.toMillis (.lastAccessTime attrs)) 0)
      (check "basic creation time" (.toMillis (.creationTime attrs)) 0)
      (check "basic directory" (.isDirectory attrs) false)
      (check "basic other" (.isOther attrs) false)
      (check "basic size" (.size attrs) 21))
    (.checkAccess provider path)
    (check "provider write access"
           (try (.checkAccess provider path (into-array java.nio.file.AccessMode [java.nio.file.AccessMode/WRITE])) :allowed
                (catch java.nio.file.AccessDeniedException _ :denied))
           :denied)
    (let [view (.getFileAttributeView provider path java.nio.file.attribute.BasicFileAttributeView)]
      (check "attribute view name" (.name view) "basic")
      (check "attribute view values" (.isRegularFile (.readAttributes view)) true)
      (check "attribute view set times"
             (try (.setTimes view nil nil nil) :not-rejected
                  (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
             :read-only))
    (check "provider delete"
           (try (.delete provider path) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (check "provider delete missing"
           (try (.deleteIfExists provider missing) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (check "provider move"
           (try (.move provider path missing) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (check "provider create directory"
           (try (.createDirectory provider missing) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (check "provider create link"
           (try (.createLink provider missing path) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (check "provider create symlink"
           (try (.createSymbolicLink provider missing path) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (check "provider read symlink"
           (try (.readSymbolicLink provider path) :not-rejected
                (catch java.lang.UnsupportedOperationException _ :unsupported))
           :unsupported)
    (check "provider set attribute"
           (try (.setAttribute provider path "basic:lastModifiedTime" (java.nio.file.attribute.FileTime/fromMillis 0)) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (let [principals (.getUserPrincipalLookupService fs)]
      (check "principal lookup"
             (try (.lookupPrincipalByName principals "nobody") :not-rejected
                  (catch java.lang.UnsupportedOperationException _ :unsupported))
             :unsupported))
    (let [key (.register path watch (make-array java.nio.file.WatchEvent$Kind 0))]
      (check "watch key path" (= path (.watchable key)) true)
      (check "watch events" (count (.pollEvents key)) 0)
      (check "watch reset" (.reset key) true)
      (check "watch poll" (.poll watch) nil)
      (check "watch timed poll" (.poll watch 0 java.util.concurrent.TimeUnit/MILLISECONDS) nil)
      (check "watch take" (.take watch) nil)
      (.cancel key)
      (check "watch cancelled" (.isValid key) false))
    (.close watch)
    (expect-throw "closed watch poll" java.nio.file.ClosedWatchServiceException #(.poll watch))
    (expect-throw "closed watch take" java.nio.file.ClosedWatchServiceException #(.take watch))
    (.close fs)
    (expect-throw "closed filesystem watch" java.nio.file.ClosedFileSystemException #(.newWatchService fs))
    (prn :fs-full-ok)))

(defn files-full-test []
  (let [uri (java.net.URI. "jar:file:jolt-embedded-resources.jar")
        fs (java.nio.file.FileSystems/newFileSystem uri {})
        path (.getPath fs "greeting.txt" (make-array String 0))
        migration (.getPath fs "migrations/001-create-users.up.sql" (make-array String 0))
        dir (.getPath fs "migrations" (make-array String 0))
        binary (.getPath fs "data.bin" (make-array String 0))
        latin (.getPath fs "latin1-lines.bin" (make-array String 0))
        missing (.getPath fs "missing" (make-array String 0))
        host-destination (.getPath (java.nio.file.FileSystems/getDefault) "copied.txt" (make-array String 0))
        content "embedded resource ok\n"]
    (check "read string" (java.nio.file.Files/readString path) content)
    (check "read string charset" (java.nio.file.Files/readString path java.nio.charset.StandardCharsets/UTF_8) content)
    (check "read all lines" (vec (java.nio.file.Files/readAllLines path)) ["embedded resource ok"])
    (check "read lines charset" (vec (java.nio.file.Files/readAllLines path java.nio.charset.StandardCharsets/UTF_8)) ["embedded resource ok"])
    (check "line stream" (vec (iterator-seq (.iterator (java.nio.file.Files/lines path)))) ["embedded resource ok"])
    (check "line stream charset" (vec (iterator-seq (.iterator (java.nio.file.Files/lines path java.nio.charset.StandardCharsets/UTF_8)))) ["embedded resource ok"])
    (check "binary bytes" (vec (java.nio.file.Files/readAllBytes binary)) [0 -1 -128 65 10])
    (check "latin1" (java.nio.file.Files/readString binary java.nio.charset.StandardCharsets/ISO_8859_1)
           (str (char 0) (char 255) (char 128) "A\n"))
    (check "latin1 CR lines" (vec (java.nio.file.Files/readAllLines latin java.nio.charset.StandardCharsets/ISO_8859_1))
           ["café" "second"])
    (check "latin1 CR line stream"
           (vec (iterator-seq (.iterator (java.nio.file.Files/lines latin java.nio.charset.StandardCharsets/ISO_8859_1))))
           ["café" "second"])
    (expect-throw "invalid UTF8 lines" java.nio.charset.MalformedInputException
                  #(java.nio.file.Files/readAllLines binary))
    (check "content type" (java.nio.file.Files/probeContentType path) "text/plain")
    (check "missing content type" (java.nio.file.Files/probeContentType missing) nil)
    (with-open [reader (java.nio.file.Files/newBufferedReader path)]
      (check "buffered reader" (.readLine reader) "embedded resource ok"))
    (with-open [reader (java.nio.file.Files/newBufferedReader path java.nio.charset.StandardCharsets/UTF_8)]
      (check "buffered reader charset" (.readLine reader) "embedded resource ok"))
    (with-open [channel (java.nio.file.Files/newByteChannel path)]
      (let [buffer (java.nio.ByteBuffer/allocate 4)]
        (check "channel size" (.size channel) 21)
        (check "channel first read" (.read channel buffer) 4)
        (check "channel position" (.position channel) 4)
        (.flip buffer)
        (check "channel bytes" (String. (.array buffer) "UTF-8") "embe")
        (.position channel 4)
        (check "channel position set" (.position channel) 4)
        (.clear buffer)
        (check "channel second read" (.read channel buffer) 4)
        (.flip buffer)
        (check "channel second bytes" (String. (.array buffer) "UTF-8") "dded")
        (.clear buffer)
        (check "channel third read" (.read channel buffer) 4)
        (.clear buffer)
        (check "channel fourth read" (.read channel buffer) 4)
        (.clear buffer)
        (check "channel fifth read" (.read channel buffer) 4)
        (.clear buffer)
        (check "channel final read" (.read channel buffer) 1)
        (check "channel eof" (.read channel buffer) -1)
        (check "channel zero read at eof" (.read channel (java.nio.ByteBuffer/allocate 0)) 0)
        (expect-throw "channel negative position" java.lang.IllegalArgumentException #(.position channel -1))
        (check "channel write rejected"
               (try (.write channel buffer) :not-rejected
                    (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
               :read-only)
        (check "channel truncate rejected"
               (try (.truncate channel 1) :not-rejected
                    (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
               :read-only)
        (.close channel)
        (expect-throw "closed channel position" java.nio.channels.ClosedChannelException #(.position channel))
        (expect-throw "closed channel read" java.nio.channels.ClosedChannelException #(.read channel buffer))
        (expect-throw "closed channel write" java.nio.channels.ClosedChannelException #(.write channel buffer))
        (expect-throw "closed channel truncate" java.nio.channels.ClosedChannelException #(.truncate channel 1))))
    (let [channel (java.nio.channels.FileChannel/open path)
          buffer (java.nio.ByteBuffer/allocate 4)]
      (check "file channel type" (instance? java.nio.channels.FileChannel channel) true)
      (check "file channel read" (.read channel buffer) 4)
      (.close channel))
    (with-open [stream (java.nio.file.Files/list dir)]
      (check "relative list" (mapv str (iterator-seq (.iterator stream))) ["migrations/001-create-users.up.sql"]))
    (with-open [stream (java.nio.file.Files/walk dir)]
      (check "relative walk" (mapv str (iterator-seq (.iterator stream)))
             ["migrations" "migrations/001-create-users.up.sql"]))
    (with-open [stream (java.nio.file.Files/walk dir 1)]
      (check "walk depth" (mapv str (iterator-seq (.iterator stream)))
             ["migrations" "migrations/001-create-users.up.sql"]))
    (with-open [stream (java.nio.file.Files/find dir 10 (fn [path _] (.endsWith (str path) ".sql")))]
      (check "find" (mapv str (iterator-seq (.iterator stream))) ["migrations/001-create-users.up.sql"]))
    (let [visited (atom 0)
          visitor (reify java.nio.file.FileVisitor
                    (preVisitDirectory [_ _ _] java.nio.file.FileVisitResult/CONTINUE)
                    (visitFile [_ _ _] (swap! visited inc) java.nio.file.FileVisitResult/CONTINUE)
                    (visitFileFailed [_ _ error] (throw error))
                    (postVisitDirectory [_ _ _] java.nio.file.FileVisitResult/CONTINUE))]
      (java.nio.file.Files/walkFileTree dir visitor)
      (check "two argument walkFileTree" @visited 1))
    (expect-throw "missing walk root" java.nio.file.NoSuchFileException
                  #(iterator-seq (.iterator (java.nio.file.Files/walk missing))))
    (expect-throw "missing find root" java.nio.file.NoSuchFileException
                  #(java.nio.file.Files/find missing 1 (fn [_ _] true)))
    (with-open [stream (java.nio.file.Files/list dir)]
      (.iterator stream)
      (check "stream second use"
             (try (.count stream) :not-rejected
                  (catch java.lang.IllegalStateException _ :closed))
             :closed))
    (check "mismatch same" (java.nio.file.Files/mismatch path path) -1)
    (check "mismatch different" (java.nio.file.Files/mismatch path migration) 0)
    (java.nio.file.Files/copy path host-destination)
    (check "copy to host" (slurp (.toFile host-destination)) content)
    (check "mismatch host" (java.nio.file.Files/mismatch path host-destination) -1)
    (check "copy embedded destination"
           (try (java.nio.file.Files/copy path missing) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (expect-throw "host to embedded destination" java.nio.file.ReadOnlyFileSystemException
                  #(java.nio.file.Files/copy host-destination missing))
    ;; a copy across providers checks the target first (CopyMoveHelper)
    (expect-throw "host to existing embedded destination" java.nio.file.FileAlreadyExistsException
                  #(java.nio.file.Files/copy host-destination path))
    (expect-throw "stream to embedded destination" java.nio.file.ReadOnlyFileSystemException
                  #(java.nio.file.Files/copy (java.io.ByteArrayInputStream. (.getBytes "x" "UTF-8")) missing))
    (expect-throw "provider host to embedded destination" java.nio.file.ReadOnlyFileSystemException
                  #(.copy (.provider fs) host-destination missing (make-array java.nio.file.CopyOption 0)))
    (check "move embedded"
           (try (java.nio.file.Files/move path missing) :not-rejected
                (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
           :read-only)
    (let [out (java.io.ByteArrayOutputStream.)]
      (java.nio.file.Files/copy path out)
      (check "copy output stream" (.toString out "UTF-8") content))
    (check "file store" (.name (java.nio.file.Files/getFileStore path)) "jolt-embedded-resources")
    (expect-throw "missing file store" java.nio.file.NoSuchFileException #(java.nio.file.Files/getFileStore missing))
    (check "attribute view" (.name (java.nio.file.Files/getFileAttributeView path java.nio.file.attribute.BasicFileAttributeView)) "basic")
    (check "unknown attribute view" (java.nio.file.Files/getFileAttributeView path java.nio.file.attribute.PosixFileAttributeView) nil)
    (expect-throw "unsupported attributes class" java.lang.UnsupportedOperationException
                  #(java.nio.file.Files/readAttributes path java.nio.file.attribute.PosixFileAttributes))
    (expect-throw "unsupported attributes view" java.lang.UnsupportedOperationException
                  #(java.nio.file.Files/readAttributes path "posix:*"))
    (expect-throw "unknown basic attribute" java.lang.IllegalArgumentException
                  #(java.nio.file.Files/readAttributes path "basic:notAnAttribute"))
    (check "posix permissions unsupported"
           (try (java.nio.file.Files/getPosixFilePermissions path) :not-rejected
                (catch java.lang.UnsupportedOperationException _ :unsupported))
           :unsupported)
    (check "owner unsupported"
           (try (java.nio.file.Files/getOwner path) :not-rejected
                (catch java.lang.UnsupportedOperationException _ :unsupported))
           :unsupported)
    (doseq [[label thunk] [["newBufferedWriter" #(java.nio.file.Files/newBufferedWriter path)]
                           ["writeString" #(java.nio.file.Files/writeString path "x")]
                           ["setOwner" #(java.nio.file.Files/setOwner path nil)]]]
      (check label (try (thunk) :not-rejected
                        (catch java.nio.file.ReadOnlyFileSystemException _ :read-only))
             :read-only))
    (check "missing mismatch"
           (try (java.nio.file.Files/mismatch path missing) :not-rejected
                (catch java.nio.file.NoSuchFileException _ :missing))
           :missing)
    (check "cross provider startsWith" (.startsWith path host-destination) false)
    (check "cross provider endsWith" (.endsWith path host-destination) false)
    (expect-throw "cross provider resolve" java.nio.file.ProviderMismatchException #(.resolve path host-destination))
    (expect-throw "cross provider resolveSibling" java.nio.file.ProviderMismatchException #(.resolveSibling path host-destination))
    (expect-throw "cross provider relativize" java.nio.file.ProviderMismatchException #(.relativize path host-destination))
    (expect-throw "cross provider compareTo" java.nio.file.ProviderMismatchException #(.compareTo path host-destination))
    (.close fs)
    (prn :files-full-ok)))

(defn archive-lifecycle-test []
  (let [jar (java.util.jar.JarFile. "jolt-embedded-resources.jar")
        entries (.entries jar)
        stream (.stream jar)
        versioned (.versionedStream jar)]
    (.close jar)
    (doseq [[label thunk] [["closed manifest" #(.getManifest jar)]
                           ["closed comment" #(.getComment jar)]
                           ["closed version" #(.getVersion jar)]
                           ["closed multi release" #(.isMultiRelease jar)]
                           ["closed enumeration" #(.nextElement entries)]
                           ["closed stream" #(iterator-seq (.iterator stream))]
                           ["closed versioned stream" #(iterator-seq (.iterator versioned))]]]
      (expect-throw label java.lang.IllegalStateException thunk))
    (prn :archive-lifecycle-ok)))

(defn oracle-test []
  (let [jar-path "jolt-embedded-resources.jar"]
    (with-open [jar (java.util.jar.JarFile. jar-path)]
      (let [manifest (.getManifest jar)
            versions (mapv (fn [version]
                             (with-open [versioned (java.util.jar.JarFile. (java.io.File. jar-path) true 1
                                                                         (Runtime$Version/parse version))]
                               [version
                                (slurp (.getInputStream versioned (.getJarEntry versioned "greeting.txt")))
                                (.getRealName (.getJarEntry versioned "greeting.txt"))
                                (some-> (.getJarEntry versioned "only.txt")
                                        (#(slurp (.getInputStream versioned %))))
                                (some? (.getJarEntry versioned "below.txt"))
                                (some? (.getJarEntry versioned "below7.txt"))
                                (mapv #(.getName %) (iterator-seq (.iterator (.versionedStream versioned))))]))
                           ["8" "9" "17" "22"])]
        (prn {:default-content (slurp (.getInputStream jar (.getJarEntry jar "greeting.txt")))
              :manifest-cached (identical? manifest (.getManifest jar))
              :resources (mapv #(when-let [url (io/resource %)]
                                  (let [s (str url)] [(subs s (+ 2 (.indexOf s "!/"))) (slurp url)]))
                               ["greeting.txt" "only.txt" "below.txt" "below7.txt" "plain.txt"
                                "encoded/001 my café;v=1.sql" "encoded/"])
              :base-version (str (java.util.jar.JarFile/baseVersion))
              :multi-release (.isMultiRelease jar)
              :versioned-names (mapv #(.getName %) (iterator-seq (.iterator (.versionedStream jar))))
              :versions versions
              :manifest [(.getValue (.getMainAttributes manifest) "Multi-Release")
                         (.getValue (.getMainAttributes manifest) "Long-Header")
                         (.getValue (.getMainAttributes manifest) "X-Unicode")
                         (.getValue (.getAttributes manifest "greeting.txt") "X-Entry")]})))))

(defn -main [& args]
  (case (first args)
    "--url" (url-test)
    "--resource-url" (resource-url-test)
    "--jar-full" (jar-full-test)
    "--fs-full" (fs-full-test)
    "--files-full" (files-full-test)
    "--archive-lifecycle" (archive-lifecycle-test)
    "--oracle" (oracle-test)
    nil))
