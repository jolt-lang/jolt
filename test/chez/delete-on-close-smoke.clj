;; Files/newInputStream, newOutputStream and newByteChannel with
;; StandardOpenOption/DELETE_ON_CLOSE or LinkOption/NOFOLLOW_LINKS, asserted the
;; way the JDK answers. On
;; Windows they open a HANDLE with FILE_FLAG_DELETE_ON_CLOSE and read and write
;; it with ReadFile/WriteFile (nio-file.ss fc-win32-port), so the file is there
;; while the stream is open and gone once it closes; on POSIX it is unlinked as
;; it opens. Either way the stream reads and writes the file until it closes.
;;
;;   make deleteonclose                                           # this host
;;   target/release/jolt.exe test/chez/delete-on-close-smoke.clj  # windows-static
;;   tools/wine/run.sh jolt-nt test/chez/delete-on-close-smoke.clj
;;
;; Exits 1 on any failure.
(import '[java.nio.file Files OpenOption StandardOpenOption LinkOption]
        '[java.nio.file.attribute FileAttribute]
        '[java.nio ByteBuffer]
        '[java.nio.channels FileChannel])

(def windows? (.startsWith (System/getProperty "os.name") "Windows"))
(def fails (atom 0))
(defmacro check [label expr want]
  `(let [got# (try ~expr (catch Throwable e# [:threw (.getName (class e#)) (.getMessage e#)]))]
     (if (= got# ~want)
       (println "ok  " ~label)
       (do (swap! fails inc)
           (println "FAIL" ~label "\n     got: " (pr-str got#) "\n     want:" (pr-str ~want))))))

(def R StandardOpenOption/READ)
(def W StandardOpenOption/WRITE)
(def A StandardOpenOption/APPEND)
(def C StandardOpenOption/CREATE)
(def CN StandardOpenOption/CREATE_NEW)
(def TE StandardOpenOption/TRUNCATE_EXISTING)
(def SY StandardOpenOption/SYNC)
(def DS StandardOpenOption/DSYNC)
(def DOC StandardOpenOption/DELETE_ON_CLOSE)
(defn opts [& xs] (into-array OpenOption xs))
(def dir (Files/createTempDirectory "jolt-doc" (make-array FileAttribute 0)))
(defn path [s] (.resolve dir s))
(defn there? [p] (Files/exists p (make-array LinkOption 0)))
(defn fresh [s content]
  (let [p (path s)] (Files/write p (.getBytes content) (opts)) p))
(defn bytes-of [s] (ByteBuffer/wrap (.getBytes s)))
;; Open on POSIX means unlinked; on Windows the entry stays until the last
;; HANDLE closes.
(def while-open (boolean windows?))

;; A FileChannel opened first sees what a stream writes, by its size: it shares
;; read, write and delete, so the DELETE_ON_CLOSE open is allowed beside it, and
;; the file goes when the channel closes too.
(defn size-through-channel [s content f]
  (let [p (fresh s content)
        ch (FileChannel/open p (opts R))
        r (try (f p ch) (finally (.close ch)))]
    [r (there? p)]))

;; --- newInputStream ------------------------------------------------------------
(check "newInputStream reads the file, which is gone once the stream closes"
       (let [p (fresh "in" "hello")
             in (Files/newInputStream p (opts DOC))
             open? (there? p)
             s (String. (.readAllBytes in))]
         (.close in)
         [s open? (there? p)])
       ["hello" while-open false])
(check "newInputStream available and skip follow the channel's position and size"
       (let [p (fresh "in-av" "hello")]
         (with-open [in (Files/newInputStream p (opts DOC))]
           [(.available in) (.read in) (.available in) (.skip in 100) (.available in)
            (.skip in -2) (.read in) (.skip in -100) (.available in)]))
       [5 104 4 4 0 -2 108 -4 5])
(check "newInputStream READ + DELETE_ON_CLOSE, a missing file"
       (let [e (try (Files/newInputStream (path "nothere") (opts R DOC)) (catch Exception e e))]
         (.getName (class e)))
       "java.nio.file.NoSuchFileException")
(check "200 DELETE_ON_CLOSE input streams each give their file back"
       (every? (fn [i]
                 (let [p (fresh (str "many" i) "x")]
                   (with-open [in (Files/newInputStream p (opts DOC))] (.read in))
                   (not (there? p))))
               (range 200))
       true)

;; --- newOutputStream -----------------------------------------------------------
(check "newOutputStream CREATE_NEW writes the file, which is gone once the stream closes"
       (let [p (path "out")
             out (Files/newOutputStream p (opts CN W DOC))
             _ (.write out (.getBytes "hello"))
             open? (there? p)]
         (.close out)
         [open? (there? p)])
       [while-open false])
(check "newOutputStream writes reach the file before it closes"
       (size-through-channel "out-w" "hello"
         (fn [p ch]
           (with-open [out (Files/newOutputStream p (opts W DOC))]
             (.write out (.getBytes "abc")) (.flush out)
             [(.size ch)])))
       [[5] false])
(check "newOutputStream APPEND + DELETE_ON_CLOSE writes at the end"
       (size-through-channel "out-a" "hello"
         (fn [p ch]
           (with-open [out (Files/newOutputStream p (opts A DOC))]
             (.write out (.getBytes "XY")) (.flush out)
             (.write out (.getBytes "Z")) (.flush out)
             (.size ch))))
       [8 false])
(check "newOutputStream TRUNCATE_EXISTING + DELETE_ON_CLOSE empties the file first"
       (size-through-channel "out-t" "hello"
         (fn [p ch]
           (with-open [out (Files/newOutputStream p (opts W TE DOC))]
             (let [at-open (.size ch)]
               (.write out (.getBytes "Z")) (.flush out)
               [at-open (.size ch)]))))
       [[0 1] false])
(check "newOutputStream SYNC + DELETE_ON_CLOSE writes through each call, no flush"
       (size-through-channel "out-s" ""
         (fn [p ch]
           (with-open [out (Files/newOutputStream p (opts W SY DOC))]
             (.write out (.getBytes "abc")) (.size ch))))
       [3 false])
(check "newOutputStream DSYNC + DELETE_ON_CLOSE writes through each call, no flush"
       (size-through-channel "out-d" ""
         (fn [p ch]
           (with-open [out (Files/newOutputStream p (opts W DS DOC))]
             (.write out (.getBytes "ab")) (.size ch))))
       [2 false])
(check "newOutputStream CREATE + DELETE_ON_CLOSE creates a missing file, then deletes it"
       (let [p (path "out-c")]
         (with-open [out (Files/newOutputStream p (opts W C DOC))] (.write out 65))
         (there? p))
       false)
(check "newOutputStream CREATE_NEW + DELETE_ON_CLOSE on an existing file"
       (let [p (fresh "out-cn" "x")
             e (try (Files/newOutputStream p (opts CN DOC)) (catch Exception e e))
             r [(.getName (class e)) (there? p)]]
         (Files/delete p)
         r)
       ["java.nio.file.FileAlreadyExistsException" true])
(check "Files/write with DELETE_ON_CLOSE leaves nothing behind"
       (let [p (fresh "write" "")]
         (Files/write p (.getBytes "data") (opts W DOC))
         (there? p))
       false)

;; --- newByteChannel ------------------------------------------------------------
(check "newByteChannel READ + WRITE + CREATE_NEW + DELETE_ON_CLOSE reads back what it wrote"
       (let [p (path "ch")
             ch (Files/newByteChannel p (opts R W CN DOC))
             n (.write ch (bytes-of "abcdef"))
             open? (there? p)
             _ (.position ch 2)
             b (ByteBuffer/allocate 2)
             _ (.read ch b)
             _ (.truncate ch 4)
             r [n open? (String. (.array b)) (.size ch) (.position ch)]]
         (.close ch)
         (conj r (there? p)))
       [6 while-open "cd" 4 4 false])
(check "newByteChannel READ + DELETE_ON_CLOSE reads the file and deletes it"
       (let [p (fresh "ch-r" "hello")
             b (ByteBuffer/allocate 16)]
         (with-open [ch (Files/newByteChannel p (opts R DOC))] (.read ch b))
         [(String. (.array b) 0 (.position b)) (there? p)])
       ["hello" false])
(check "newByteChannel APPEND + DELETE_ON_CLOSE writes at the end; it does not read"
       (let [p (fresh "ch-a" "hello")]
         (with-open [ch (Files/newByteChannel p (opts A DOC))]
           (.write ch (bytes-of "XY"))
           [(.size ch) (.position ch)
            (try (.read ch (ByteBuffer/allocate 1)) (catch Exception e (.getName (class e))))
            (there? p)]))
       [7 7 "java.nio.channels.NonReadableChannelException" while-open])
(check "newByteChannel WRITE + DELETE_ON_CLOSE overwrites from the start"
       (let [p (fresh "ch-w" "hello")]
         (with-open [ch (Files/newByteChannel p (opts W DOC))]
           (.write ch (bytes-of "J"))
           [(.size ch) (.position ch)]))
       [5 1])

;; --- NOFOLLOW_LINKS --------------------------------------------------------------
;; The JDK refuses a link with an IOException whose message is the host's: ELOOP
;; with " (NOFOLLOW_LINKS specified)" on Unix, "File is symbolic link" on Windows.
(def NF LinkOption/NOFOLLOW_LINKS)
(defn link-refused [f]
  (try (.close (f)) :opened
       (catch java.io.IOException e
         (let [m (str (.getMessage e))]
           (if (or (.endsWith m " (NOFOLLOW_LINKS specified)") (= m "File is symbolic link"))
             :refused
             [(.getName (class e)) m])))))
;; Wine's CreateSymbolicLinkW is a stub that answers success and makes nothing,
;; so where no link appears the row is skipped, unless JOLT_REQUIRE_SYMLINKS is
;; set (windows-static sets it: the runner can make links).
(def link-ok?
  (let [t (fresh "probe-target" "x") l (path "probe-ln")
        ok (try (Files/createSymbolicLink l t (make-array FileAttribute 0))
                (Files/isSymbolicLink l)
                (catch Exception e (println "     createSymbolicLink:" (str e)) false))]
    (Files/deleteIfExists l) (Files/delete t)
    ok))
(if (or link-ok? (System/getenv "JOLT_REQUIRE_SYMLINKS"))
(check "NOFOLLOW_LINKS refuses a symbolic link on all three opens, with and without DELETE_ON_CLOSE"
       (let [target (fresh "nf-target" "hello")
             ln (Files/createSymbolicLink (path "nf-ln") target (make-array FileAttribute 0))
             r [(link-refused #(Files/newInputStream ln (opts NF)))
                (link-refused #(Files/newInputStream ln (opts NF DOC)))
                (link-refused #(Files/newOutputStream ln (opts W NF)))
                (link-refused #(Files/newOutputStream ln (opts W NF DOC)))
                (link-refused #(Files/newByteChannel ln (opts NF)))
                (link-refused #(Files/newByteChannel ln (opts R W NF DOC)))
                (there? ln) (slurp (str target))]]
         (Files/delete ln) (Files/delete target)
         r)
       [:refused :refused :refused :refused :refused :refused true "hello"])
(println "skip  NOFOLLOW_LINKS on a symbolic link: this host makes no links"))
(check "NOFOLLOW_LINKS on a plain file reads and writes it"
       (let [p (fresh "nf-plain" "hello")
             r [(with-open [in (Files/newInputStream p (opts NF))] (String. (.readAllBytes in)))
                (do (with-open [out (Files/newOutputStream p (opts A NF))] (.write out (.getBytes "!")))
                    (slurp (str p)))
                (with-open [ch (Files/newByteChannel p (opts R W NF))] (.size ch))]]
         (Files/delete p)
         r)
       ["hello" "hello!" 6])

(Files/delete dir)
(if (pos? @fails)
  (do (println @fails "failure(s)") (System/exit 1))
  (println "delete-on-close smoke: all passed"))
