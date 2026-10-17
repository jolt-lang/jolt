;; End-to-end checks for the Windows parity reports #1108, #1109, #1117, #1118
;; and #1119: each is the issue's own repro, asserted the way the JVM answers.
;; Runs on any host (the child program and the drive spelling follow os.name),
;; but it exists for the ones a POSIX runner cannot see:
;;
;;   tools/wine/run.sh jolt-nt test/chez/win-parity-smoke.clj  # under Wine
;;   make winparity                                             # this host
;;
;; Exits 1 on any failure.
(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[clojure.java.io :as io]
         '[clojure.java.shell :as sh]
         '[clojure.string :as str])

(def windows? (str/starts-with? (System/getProperty "os.name") "Windows"))

;; ONE carrier for the fiber rows below: a fiber that blocked its carrier on a
;; socket instead of parking would then stop every other fiber, which is what
;; they assert does not happen. Set before anything spawns.
(require 'jolt.fibers)
(jolt.fibers/set-carrier-count! 1)
(def fails (atom 0))
(def total (atom 0))

(defmacro check [label expr want]
  `(let [got# (try ~expr (catch Throwable e# [:threw (str e#)]))]
     (swap! total inc)
     (if (= got# ~want)
       (println "ok  " ~label)
       (do (swap! fails inc)
           (println "FAIL" ~label "\n     got: " (pr-str got#) "\n     want:" (pr-str ~want))))))

(def root (str (fs/create-temp-dir {:prefix "jolt-parity"})))
(defn under [& parts] (str/join "/" (cons root parts)))
(defn gone? [f] (not (fs/exists? f)))

;; --- #1108: every subprocess spawn --------------------------------------------
(def echo-argv (if windows? ["cmd" "/c" "echo" "hi"] ["sh" "-c" "echo hi"]))

;; FIRST, before anything else here spawns: the kernel32 entry points behind a
;; spawn are resolved on first use, and that lookup marked itself done before it
;; had an answer, so threads starting their first child together read #f and
;; failed with "no Windows spawn path" (#1331). Only a process's first spawns can
;; show it, which is why this row comes ahead of the others.
(check "#1331 concurrent first spawns all start"
       (let [n 8
             go (promise)
             ts (mapv (fn [_]
                        (future
                          @go
                          (try
                            (let [pr (.start (ProcessBuilder. echo-argv))]
                              (slurp (.getInputStream pr))
                              (.waitFor pr))
                            (catch Throwable e (str e)))))
                      (range n))]
         (deliver go true)
         (mapv deref ts))
       (vec (repeat 8 0)))

(check "#1108 ProcessBuilder reads the child's stdout"
       (let [pr (.start (ProcessBuilder. echo-argv))
             out (str/trim (slurp (.getInputStream pr)))]
         [out (.waitFor pr)])
       ["hi" 0])
(check "#1108 babashka.process/shell"
       (-> (apply p/shell {:out :string :err :string} echo-argv) :out str/trim)
       "hi")
(check "#1108 clojure.java.shell/sh, nonzero exit"
       (let [r (apply sh/sh (if windows? ["cmd" "/c" "exit 3"] ["sh" "-c" "exit 3"]))] (:exit r))
       3)
(check "#1108 an argument with a space arrives whole"
       (let [r (apply sh/sh (if windows?
                              ["cmd" "/c" "echo" "a b"]
                              ["sh" "-c" "printf '%s' \"$1\"" "sh" "a b"]))]
         (str/trim (:out r)))
       (if windows? "\"a b\"" "a b"))

;; --- #1109: PushbackReader.close closes the wrapped reader ---------------------
(let [f (under "close-only.clj")]
  (spit f "(ns dep)")
  (with-open [r (java.io.PushbackReader. (io/reader f))])
  (fs/delete f)
  (check "#1109 with-open PushbackReader releases the file" (gone? f) true))

;; --- #1117: ...and still after clojure.core/read -------------------------------
(let [f (under "read-then-close.clj")]
  (spit f "(ns dep)")
  (check "#1117 read over a PushbackReader(FileReader)"
         (with-open [r (java.io.PushbackReader. (java.io.FileReader. f))] (read r false nil))
         '(ns dep))
  (fs/delete f)
  (check "#1117 ...and the file is released on close" (gone? f) true))
(let [f (under "read-io-reader.clj")]
  (spit f "(ns dep)")
  (with-open [r (java.io.PushbackReader. (io/reader f))] (read r false nil))
  (fs/delete f)
  (check "#1117 read over a PushbackReader(io/reader) releases the file" (gone? f) true))

;; --- #1118: file: URLs -----------------------------------------------------------
(let [d (under "has space")
      f (str d "/x.txt")
      abs (str (fs/absolutize f))
      slashed (str/replace abs "\\" "/")
      url-path (if windows? (str "/" slashed) slashed)]
  (fs/create-dirs d)
  (spit f "payload")
  (check "#1118 File.toURI is the JDK's spelling"
         (str (.toURI (io/file abs)))
         (str "file:" (str/replace url-path " " "%20")))
  (check "#1118 toURI -> toURL round trip opens"
         (with-open [is (io/input-stream (.toURL (.toURI (io/file abs))))] (slurp is))
         "payload")
  (check "#1118 io/as-url opens" (slurp (io/as-url (io/file abs))) "payload")
  (check "#1118 file:/… opens" (slurp (java.net.URL. (str "file:" url-path))) "payload")
  (check "#1118 file:///… opens" (slurp (java.net.URL. (str "file://" url-path))) "payload")
  (check "#1118 a file: string opens" (slurp (str "file:" url-path)) "payload")
  ;; the same FILE: its spelling may differ from abs's, whose separators are
  ;; whatever TEMP held (the #1110 rendering question), so ask canonically
  (check "#1118 io/as-file of the URL is the file"
         (.getCanonicalPath (io/as-file (.toURL (.toURI (io/file abs)))))
         (.getCanonicalPath (io/file abs))))
(let [jar (under "has space" "r.jar")
      jar-path (str/replace (str (fs/absolutize jar)) "\\" "/")]
  (with-open [zo (java.util.zip.ZipOutputStream. (io/output-stream jar))]
    (.putNextEntry zo (java.util.zip.ZipEntry. "data.txt"))
    (.write zo (.getBytes "in-jar"))
    (.closeEntry zo))
  (check "#1118 jar:file: over toURI opens"
         (slurp (java.net.URL. (str "jar:" (.toURI (io/file jar-path)) "!/data.txt")))
         "in-jar"))

;; --- #1203: jolt.loader opens a file: resource hit -------------------------------
;; The loader stripped "file:" with (subs url 5), so the drive form a resource
;; URL has on Windows, file:/C:/…, became /C:/… and could not be opened; on every
;; platform a %hh escape or a localhost authority was left in the path. A hit's
;; URL is the JDK's spelling: absolute, the drive behind a "/", escaped.
(require '[jolt.loader :as jl])
(let [d (under "res dir")
      f (str d "/r.txt")
      abs (str (fs/absolutize f))
      uri (str (.toURI (io/file abs)))
      slashed (str/replace abs "\\" "/")
      url-path (if windows? (str "/" slashed) slashed)
      located (fn [url] (jl/->loader (fn [req] (when (= :resource (:kind req))
                                                 {:kind :resource :url url}))))]
  (fs/create-dirs d)
  (spit f "resource")
  (doseq [[label url] [["an escaped file: URI" uri]
                       ["a localhost authority" (str "file://localhost" url-path)]
                       ["an empty authority" (str "file://" url-path)]
                       ["the unescaped spelling" (str "file:" url-path)]]]
    (let [l (located url)
          hit (first (jl/find l {:kind :resource :name "r.txt"}))]
      (check (str "#1203 open-hit: " label) (slurp (jl/open-hit l hit)) "resource")
      (check (str "#1203 getResource: " label)
             (slurp (.getResource (jl/as-classloader l) "r.txt")) "resource")))
  (let [l (jl/classpath [d])
        hit (first (jl/find l {:kind :resource :name "r.txt"}))]
    (check "#1203 a classpath root's hit is the JDK's URL" (:url hit) uri)
    (check "#1203 ...and opens" (slurp (jl/open-hit l hit)) "resource")
    (check "#1203 ...and getResource answers the same URL"
           (str (.getResource (jl/as-classloader l) "r.txt")) uri)))

;; --- #1208: Socket half-close -----------------------------------------------------
;; Winsock's shutdown takes SD_SEND / SD_RECEIVE, and a proxy that closed without
;; half-closing first got a reset on Windows where Linux closed gracefully. The
;; exchange a proxy pumps: the client sends and half-closes, the server reads to
;; EOF and answers, the client reads the answer, and each side's state is named.
(defn- read-all [in]
  (loop [acc []]
    (let [b (.read in)]
      (if (neg? b) (String. (byte-array acc) "UTF-8") (recur (conj acc b))))))
(let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      c (java.net.Socket. "127.0.0.1" (.getLocalPort ss))
      s (.accept ss)
      cin (.getInputStream c)
      cout (.getOutputStream c)
      msg (fn [f] (try (f) :ok (catch java.net.SocketException e (.getMessage e))))]
  (try
    (.write cout (.getBytes "req" "UTF-8"))
    (.shutdownOutput c)
    (let [req (read-all (.getInputStream s))]
      (.write (.getOutputStream s) (.getBytes (str "echo:" req) "UTF-8"))
      (.shutdownOutput s)
      (check "#1208 shutdownOutput: the peer reads to EOF and answers"
             [req (read-all cin) (.isOutputShutdown c) (.isInputShutdown c) (.isClosed c)]
             ["req" "echo:req" true false false]))
    (check "#1208 a write after shutdownOutput throws" (msg #(.write cout 1)) "Broken pipe")
    (check "#1208 shutdownOutput twice" (msg #(.shutdownOutput c)) "Socket output is already shutdown")
    (.shutdownInput c)
    (check "#1208 shutdownInput reads EOF"
           [(.isInputShutdown c) (.read cin) (msg #(.getInputStream c))]
           [true -1 "Socket input is shutdown"])
    (finally (.close s) (.close c) (.close ss))))

;; --- sockets wait on jolt.io-poller everywhere --------------------------------------
;; Windows sockets were blocking, for want of a readiness poller there: a fiber
;; reading one held its carrier, and SO_TIMEOUT and the connect timeout were
;; stored but never enforced. The WSAPoll backend makes them what they are on
;; POSIX.
(defn- pair []
  (let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
        c (java.net.Socket. "127.0.0.1" (.getLocalPort ss))
        s (.accept ss)]
    [ss c s]))

(let [[ss c s] (pair)]
  (try
    (.setSoTimeout s 300)
    ;; On nanoTime, the clock the poller's deadlines are on, so the timeout is
    ;; never early (exact lower bound). The cap only catches one that fires far
    ;; too late; a reading past it is taken again before it counts, so one late
    ;; wakeup on a loaded machine (or under Wine) does not fail the row.
    (let [timed-read (fn []
                       (let [t0 (System/nanoTime)
                             r (try (.read (.getInputStream s)) :no-timeout
                                    (catch java.net.SocketTimeoutException e (.getMessage e)))]
                         [r (/ (- (System/nanoTime) t0) 1000000)]))
          [r dt] (timed-read)
          on-time? (fn [[r dt]] (and (= r "Read timed out") (<= 300 dt 5000)))]
      (check "SO_TIMEOUT: a read with no data times out"
             [r (>= dt 300) (or (<= dt 5000) (on-time? (timed-read)))]
             ["Read timed out" true true]))
    (.write (.getOutputStream c) 65)
    (check "SO_TIMEOUT: the socket still reads after a timeout"
           (.read (.getInputStream s)) 65)
    (finally (.close s) (.close c) (.close ss))))

(let [ss (java.net.ServerSocket. 0)]
  (try
    (.setSoTimeout ss 300)
    (check "SO_TIMEOUT: accept with no client times out"
           (try (.accept ss) :accepted
                (catch java.net.SocketTimeoutException e (.getMessage e)))
           "Accept timed out")
    (finally (.close ss))))

(check "connect to a closed port is refused"
       (let [ss (java.net.ServerSocket. 0) port (.getLocalPort ss)]
         (.close ss)
         (try (java.net.Socket. "127.0.0.1" (int port)) :connected
              (catch java.net.ConnectException e (.getMessage e))))
       "Connection refused")

(let [[ss c s] (pair)
      r (promise)
      t (Thread. (fn [] (deliver r (try (.read (.getInputStream s)) :read
                                         (catch java.net.SocketException e (.getMessage e))))))]
  (.start t)
  (Thread/sleep 200)
  (.close s)
  (check "close wakes a read blocked on another thread"
         (deref r 3000 :still-blocked) "Socket closed")
  (.close c) (.close ss))

;; eight fibers park reading silent sockets on the ONE carrier, and a ninth
;; still runs; then every reader gets its byte
(let [pairs (vec (repeatedly 8 pair))
      readers (mapv (fn [[_ _ s]] (jolt.fibers/spawn (fn [] (.read (.getInputStream s)))))
                    pairs)
      other (jolt.fibers/spawn (fn [] :ran))]
  (try
    (check "fibers park on socket reads: another fiber still runs"
           (jolt.fibers/join other 3000 :starved) :ran)
    (doseq [[_ c _] pairs] (.write (.getOutputStream c) 7))
    (check "fibers park on socket reads: every reader wakes with its byte"
           (mapv #(jolt.fibers/join % 3000 :stuck) readers) (vec (repeat 8 7)))
    (finally
      (doseq [[ss c s] pairs] (.close s) (.close c) (.close ss)))))

;; --- #1119: last-modified time ---------------------------------------------------
(let [d (under "lock")
      now (System/currentTimeMillis)]
  (fs/create-dirs d)
  (fs/set-last-modified-time d (- now 60000))
  (check "#1119 a directory back-dated with a number"
         (- now (fs/file-time->millis (fs/last-modified-time d)))
         60000)
  (fs/set-last-modified-time d (java.time.Instant/ofEpochMilli (- now 120000)))
  (check "#1119 ...and with an Instant"
         (- now (fs/file-time->millis (fs/last-modified-time d)))
         120000)
  (check "#1119 last-modified-time is a FileTime"
         (instance? java.nio.file.attribute.FileTime (fs/last-modified-time d))
         true))
(let [f (under "file.txt")]
  (spit f "x")
  (fs/set-last-modified-time f 1500000000456)
  (check "#1119 a file, to the millisecond"
         (fs/file-time->millis (fs/last-modified-time f))
         1500000000456))

;; --- POSIX permissions follow the filesystem provider ----------------------------
;; The JDK's Windows provider has no POSIX view: get and set both raise. The shim
;; answered rwxr-xr-x and ignored the set, so a read-only check always passed.
(let [f (under "perms.txt")]
  (spit f "x")
  (check "POSIX permissions: set"
         (try (fs/set-posix-file-permissions f "r--r--r--") :set
              (catch UnsupportedOperationException _ :unsupported))
         (if windows? :unsupported :set))
  (check "POSIX permissions: get"
         (try (fs/posix->str (fs/posix-file-permissions f))
              (catch UnsupportedOperationException _ :unsupported))
         (if windows? :unsupported "r--r--r--"))
  (when-not windows? (fs/set-posix-file-permissions f "rw-r--r--")))

;; --- the tree the loader and the checks above used deletes cleanly -------------
(let [src (under "src")]
  (fs/create-dirs src)
  (spit (str src "/parity_dep.clj") "(ns parity-dep) (def x 1)")
  (require '[jolt.loader :as jl])
  ((resolve 'jl/load) ((resolve 'jl/classpath) [src]) {:kind :ns :name "parity-dep"})
  (check "#1117 a loaded source tree deletes" (do (fs/delete-tree src) (gone? src)) true))
(check "the whole scratch tree deletes" (do (fs/delete-tree root) (gone? root)) true)

(println (format "parity smoke: %d/%d passed" (- @total @fails) @total))
(System/exit (if (pos? @fails) 1 0))
