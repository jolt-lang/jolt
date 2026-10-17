;; jolt.socket gate — the java.net.Socket/ServerSocket surface over real
;; loopback TCP. Run: bin/jolt run test/chez/socket-test.clj (smoke.sh greps
;; for "SOCKET-TEST OK"). Every server binds port 0 (kernel-assigned), so
;; parallel gates never collide on a port.
(ns socket-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(require 'jolt.socket)

(def failures (atom []))

;; announce BEFORE evaluating, and flush: a check that blocks (accept/recv
;; with no peer) must name itself in the log rather than hang silently.
(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; connect lands in the listen backlog, so single-threaded connect-then-accept
;; is safe; every helper closes what it opens.
;;
;; A listener the test dials at 127.0.0.1 binds 127.0.0.1, not the wildcard. A
;; wildcard listener on port P does not own 127.0.0.1:P: another process
;; listening there (a long-running nREPL server, say) takes the connection, and
;; the accept below waits forever. The wildcard sites left are the ones testing
;; the wildcard itself.
(defn with-pair [f]
  (let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
        client (java.net.Socket. "127.0.0.1" (.getLocalPort server))
        conn   (.accept server)]
    (try (f server client conn)
         (finally (.close conn) (.close client) (.close server)))))

;; roundtrip both directions
(with-pair
  (fn [server client conn]
    (let [msg (.getBytes "hello over tcp" "UTF-8")]
      (.write (.getOutputStream client) msg 0 (alength msg)))
    (let [buf (byte-array 64)
          n   (.read (.getInputStream conn) buf 0 64)]
      (check-eq "roundtrip client->server" (String. buf 0 n "UTF-8") "hello over tcp"))
    (let [msg (.getBytes "pong" "UTF-8")]
      (.write (.getOutputStream conn) msg 0 (alength msg)))
    (let [buf (byte-array 16)
          n   (.read (.getInputStream client) buf 0 16)]
      (check-eq "roundtrip server->client" (String. buf 0 n "UTF-8") "pong"))))

;; hostname resolution (gethostbyname path)
(let [server (java.net.ServerSocket. 0)
      client (java.net.Socket. "localhost" (.getLocalPort server))]
  (check-eq "hostname connect" (.isConnected client) true)
  (.close client) (.close server))

;; binary-safe bytes above 127
(with-pair
  (fn [server client conn]
    (let [data (byte-array [(unchecked-byte 0) (unchecked-byte 127)
                            (unchecked-byte 128) (unchecked-byte 200)
                            (unchecked-byte 255)])]
      (.write (.getOutputStream client) data 0 5)
      (let [buf (byte-array 8)
            n   (.read (.getInputStream conn) buf 0 8)]
        (check-eq "binary bytes" [n (mapv #(bit-and % 0xff) (take n buf))]
                  [5 [0 127 128 200 255]])))))

;; single-byte write/read arities; zero-length read answers 0 like Java
(with-pair
  (fn [server client conn]
    (.write (.getOutputStream client) 65)
    (check-eq "single byte" (.read (.getInputStream conn)) 65)
    (check-eq "zero-length read" (.read (.getInputStream client) (byte-array 0)) 0)))

;; OutputStream.write(byte[]) — the abstract class's convenience overload, and
;; the form library code reaches for. It shares an arity with write(int), so the
;; argument is what tells them apart; without that a byte array landed on the
;; single-byte arm and reported "class [B cannot be cast to class
;; java.lang.Number", naming neither the socket nor the overload (jolt#954).
(with-pair
  (fn [server client conn]
    (.write (.getOutputStream client) (.getBytes "hi" "UTF-8"))
    (let [buf (byte-array 8)
          n   (.read (.getInputStream conn) buf 0 8)]
      (check-eq "write(byte[]) writes the whole array" (String. buf 0 n "UTF-8") "hi"))))

;; …and it is binary-safe and empty-safe, like the 3-arg form beside it
(with-pair
  (fn [server client conn]
    (let [out (.getOutputStream client)]
      (.write out (byte-array 0))                    ; a no-op, not a hang
      (.write out (byte-array [(unchecked-byte 0) (unchecked-byte 255)])))
    (let [buf (byte-array 4)
          n   (.read (.getInputStream conn) buf 0 4)]
      (check-eq "write(byte[]) is binary-safe"
                [n (mapv #(bit-and % 0xff) (take n buf))] [2 [0 255]]))))

;; a write larger than the socket buffers, from an offset into the array: every
;; byte arrives, in order, and only the [off, off+len) range. The peer reads on
;; another thread, since the send blocks until it does.
(with-pair
  (fn [server client conn]
    (let [n    (* 4 1024 1024)
          data (byte-array n)
          _    (dotimes [i n] (aset data i (unchecked-byte (mod i 251))))
          off  3
          len  (- n 10)
          got  (future
                 (let [in  (.getInputStream conn)
                       out (java.io.ByteArrayOutputStream.)
                       buf (byte-array 65536)]
                   (loop []
                     (let [r (.read in buf)]
                       (when (pos? r)
                         (.write out buf 0 r)
                         (recur))))
                   (.toByteArray out)))]
      (.write (.getOutputStream client) data off len)
      (.shutdownOutput client)
      (let [b @got]
        (check-eq "large write from an offset: length" (alength b) len)
        (check-eq "large write from an offset: bytes"
                  (java.util.Arrays/equals b (java.util.Arrays/copyOfRange data off (+ off len)))
                  true)))))

;; read into an offset
(with-pair
  (fn [server client conn]
    (let [msg (.getBytes "ab" "UTF-8")]
      (.write (.getOutputStream client) msg 0 2))
    (let [buf (byte-array [(byte 45) (byte 45) (byte 45) (byte 45)])
          n   (.read (.getInputStream conn) buf 1 2)]
      (check-eq "offset read" [n (String. buf 0 4 "UTF-8")] [2 "-ab-"]))))

;; a region outside the array is IndexOutOfBoundsException, checked before the write
(with-pair
  (fn [server client conn]
    (check-eq "out-of-range write region"
              (try (.write (.getOutputStream client) (byte-array 3) 1 10) :wrote
                   (catch IndexOutOfBoundsException e (.getMessage e)))
              "Range [1, 1 + 10) out of bounds for length 3")))

;; EOF after peer close
(with-pair
  (fn [server client conn]
    (.close client)
    (check-eq "eof read" (.read (.getInputStream conn)) -1)))

;; refused connect throws (bind an ephemeral port, close it, dial it)
(let [server (java.net.ServerSocket. 0)
      port   (.getLocalPort server)]
  (.close server)
  (check-eq "refused connect throws"
            (try (java.net.Socket. "127.0.0.1" port) "no-throw"
                 (catch java.io.IOException e "threw"))
            "threw"))

;; bind conflict throws
(let [server (java.net.ServerSocket. 0)]
  (check-eq "bind conflict throws"
            (try (java.net.ServerSocket. (.getLocalPort server)) "no-throw"
                 (catch java.io.IOException e "threw"))
            "threw")
  (.close server))

;; -- the unbound ServerSocket and .bind --------------------------------------
;; (ServerSocket.) makes an UNBOUND socket, as Java's does. It used to bind an
;; ephemeral wildcard port in the constructor, so it answered isBound true and a
;; real getLocalPort where the JVM answers false and -1, and .bind did not exist
;; to bind it afterwards. Every expectation below was read off JVM Clojure 1.12,
;; error classes and messages included. Ports stay kernel-assigned (0).
(let [s (java.net.ServerSocket.)]
  (check-eq "a fresh no-arg ServerSocket is not bound" (.isBound s) false)
  (check-eq "an unbound socket has no local port" (.getLocalPort s) -1)
  (check-eq "an unbound socket says so" (str s) "ServerSocket[unbound]")
  (check-eq "accept on an unbound socket throws rather than blocking"
            (try (.accept s) "no-throw"
                 (catch java.net.SocketException e (.getMessage e)))
            "Socket is not bound yet")
  (.close s))

(let [s (java.net.ServerSocket.)]
  (.bind s (java.net.InetSocketAddress. "127.0.0.1" 0))
  (check-eq "bind makes it bound" (.isBound s) true)
  (check-eq "bind assigns a real port" (pos? (.getLocalPort s)) true)
  (check-eq "re-binding a bound socket throws"
            (try (.bind s (java.net.InetSocketAddress. "127.0.0.1" 0)) "no-throw"
                 (catch java.net.SocketException e (.getMessage e)))
            "Already bound")
  (.close s)
  ;; Java's isBound asks "was it ever bound", so close does not clear it, and
  ;; the port it was bound to is still readable.
  (check-eq "isBound survives close" (.isBound s) true)
  (check-eq "the port survives close" (pos? (.getLocalPort s)) true))

;; the two-argument overload takes the backlog Java's does
(let [s (java.net.ServerSocket.)]
  (.bind s (java.net.InetSocketAddress. "127.0.0.1" 0) 10)
  (check-eq "the 2-arg bind binds too" (pos? (.getLocalPort s)) true)
  (.close s))

(let [s (java.net.ServerSocket.)]
  (.close s)
  (check-eq "bind after close throws"
            (try (.bind s (java.net.InetSocketAddress. "127.0.0.1" 0)) "no-throw"
                 (catch java.net.SocketException e (.getMessage e)))
            "Socket is closed"))

;; a bound-by-.bind socket is a working server, not just a bound fd
(let [srv (java.net.ServerSocket.)]
  (.bind srv (java.net.InetSocketAddress. "127.0.0.1" 0))
  (let [client (java.net.Socket. "127.0.0.1" (.getLocalPort srv))
        conn   (.accept srv)]
    (check-eq "a socket bound by .bind accepts connections"
              (some? conn) true)
    (.close conn) (.close client))
  (.close srv))

;; what #1093 was actually doing: probing whether a port is free. The held port
;; must refuse and a free one must take it — the failing form reported every
;; port unavailable.
(let [held (java.net.ServerSocket.)]
  (.bind held (java.net.InetSocketAddress. "127.0.0.1" 0))
  (let [taken (.getLocalPort held)]
    (check-eq "binding a held port is refused"
              (let [s (java.net.ServerSocket.)]
                (try (do (.bind s (java.net.InetSocketAddress. "127.0.0.1" taken)) :bound)
                     (catch java.io.IOException _ :refused)
                     (finally (.close s))))
              :refused))
  (.close held))
(check-eq "binding a free port succeeds"
          (let [s (java.net.ServerSocket.)]
            (try (do (.bind s (java.net.InetSocketAddress. "127.0.0.1" 0)) :bound)
                 (catch java.io.IOException _ :refused)
                 (finally (.close s))))
          :bound)

;; write to a peer-closed socket throws instead of silently dropping — and the
;; process must survive it (SIGPIPE guarded via MSG_NOSIGNAL / SO_NOSIGPIPE).
(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      client (java.net.Socket. "127.0.0.1" (.getLocalPort server))
      conn   (.accept server)
      out    (.getOutputStream client)
      msg    (.getBytes "x" "UTF-8")]
  (.close conn)
  (Thread/sleep 100)
  ;; the first write may itself draw the RST (timing differs by platform), so
  ;; both live inside the try: what's asserted is that SOME write throws.
  (check-eq "broken pipe throws"
            (try (.write out msg 0 1)
                 (Thread/sleep 100)
                 (.write out msg 0 1)
                 "no-throw"
                 (catch java.io.IOException e "threw"))
            "threw")
  (.close client) (.close server))

;; port 0 reports the kernel-assigned port; connected sockets know both ends
(with-pair
  (fn [server client conn]
    (check-eq "server ephemeral port" (pos? (.getLocalPort server)) true)
    (check-eq "client local port" (pos? (.getLocalPort client)) true)
    (check-eq "client remote port" (.getPort client) (.getLocalPort server))
    (check-eq "accepted peer port" (.getPort conn) (.getLocalPort client))
    (check-eq "accepted peer addr" (.getHostAddress (.getInetAddress conn)) "127.0.0.1")))

;; class model: class / instance? / str-through-toString
(with-pair
  (fn [server client conn]
    (check-eq "class Socket" (.getName (class client)) "java.net.Socket")
    (check-eq "class ServerSocket" (.getName (class server)) "java.net.ServerSocket")
    (check-eq "instance? Socket" (instance? java.net.Socket client) true)
    (check-eq "instance? cross-class" (instance? java.net.ServerSocket client) false)
    (check-eq "instance? InputStream" (instance? java.io.InputStream (.getInputStream client)) true)
    (check-eq "str routes toString" (str/starts-with? (str client) "Socket[addr=") true)
    (check-eq "unconnected toString" (str (java.net.Socket.)) "Socket[unconnected]")))

;; InetAddress / InetSocketAddress
(check-eq "getByName localhost" (.getHostAddress (java.net.InetAddress/getByName "localhost")) "127.0.0.1")
(check-eq "getByName class" (.getName (class (java.net.InetAddress/getByName "localhost"))) "java.net.Inet4Address")
(let [isa (java.net.InetSocketAddress. "127.0.0.1" 8080)]
  (check-eq "isa port" (.getPort isa) 8080)
  ;; getHostString, not getHostName: getHostName reverse-resolves a literal,
  ;; so its answer depends on the machine's nameservice
  (check-eq "isa host" (.getHostString isa) "127.0.0.1")
  (check-eq "isa getAddress" (.getHostAddress (.getAddress isa)) "127.0.0.1")
  ;; resolved when built: a literal has no name, a host name keeps its own
  (check-eq "isa literal toString" (str isa) "/127.0.0.1:8080")
  (check-eq "isa name toString" (str (java.net.InetSocketAddress. "localhost" 80)) "localhost/127.0.0.1:80")
  (check-eq "isa wildcard toString" (str (java.net.InetSocketAddress. 80)) "0.0.0.0/0.0.0.0:80"))
(let [u (java.net.InetSocketAddress/createUnresolved "localhost" 80)
      bad (java.net.InetSocketAddress. "no-such-host.invalid" 80)]
  (check-eq "unresolved toString" [(str u) (str bad)]
            ["localhost/<unresolved>:80" "no-such-host.invalid/<unresolved>:80"])
  (check-eq "unresolved getAddress" [(.isUnresolved u) (.getAddress u) (.isUnresolved bad)] [true nil true])
  (check-eq "unresolved is never = a resolved one"
            [(= u (java.net.InetSocketAddress. "localhost" 80))
             (= u (java.net.InetSocketAddress/createUnresolved "LocalHost" 80))]
            [false true])
  (check-eq "connect to an unresolved address"
            (try (.connect (java.net.Socket.) u) :connected
                 (catch java.net.UnknownHostException e (ex-message e)))
            "localhost")
  (check-eq "bind to an unresolved address"
            (try (.bind (java.net.ServerSocket.) u) :bound
                 (catch java.net.SocketException e (ex-message e)))
            "Unresolved address"))
(check-eq "getByName of a literal carries no name"
          (str (java.net.InetAddress/getByName "127.0.0.1")) "/127.0.0.1")

;; no-arg Socket + .connect(endpoint)
(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      client (java.net.Socket.)]
  (.connect client (java.net.InetSocketAddress. "127.0.0.1" (.getLocalPort server)))
  (check-eq "connect endpoint" (.isConnected client) true)
  (.close client) (.close server))

;; ServerSocket(port, backlog, bindAddr) restricts the bind
(let [lb (java.net.ServerSocket. 0 5 (java.net.InetAddress/getByName "127.0.0.1"))]
  ;; the JDK prints the bind address as given: a literal has no name
  (check-eq "bindAddr honored" (str/includes? (str lb) "addr=/127.0.0.1,") true)
  (.close lb))

;; closing a stream closes the socket, like Java
(with-pair
  (fn [server client conn]
    (.close (.getInputStream conn))
    (check-eq "stream close closes socket" (.isClosed conn) true)))

;; a closed socket's streams raise instead of touching the fd: the number is
;; free once closed, and the next socket to open gets it. Without the guard A's
;; read takes B's first byte and A's write reaches B's peer (jolt#1183). The JVM
;; prints SocketException "Socket closed" for every call below.
(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      port   (.getLocalPort server)
      a      (java.net.Socket. "127.0.0.1" port)
      a-peer (.accept server)
      a-in   (.getInputStream a)
      a-out  (.getOutputStream a)
      _      (.close a)
      b      (java.net.Socket. "127.0.0.1" port)
      b-peer (.accept server)
      raised (fn [f] (try (f) :no-throw
                          (catch java.net.SocketException e [:socket-exception (ex-message e)])))]
  (try
    (let [msg (.getBytes "hello-B" "UTF-8")]
      (.write (.getOutputStream b-peer) msg 0 (alength msg)))
    (check-eq "read() on a closed socket" (raised #(.read a-in)) [:socket-exception "Socket closed"])
    (check-eq "read(b) on a closed socket" (raised #(.read a-in (byte-array 4)))
              [:socket-exception "Socket closed"])
    (check-eq "read(b off len) on a closed socket" (raised #(.read a-in (byte-array 4) 0 4))
              [:socket-exception "Socket closed"])
    (check-eq "write(int) on a closed socket" (raised #(.write a-out 65))
              [:socket-exception "Socket closed"])
    (check-eq "write(b) on a closed socket" (raised #(.write a-out (.getBytes "from-A")))
              [:socket-exception "Socket closed"])
    (check-eq "write(b off len) on a closed socket" (raised #(.write a-out (.getBytes "from-A") 0 6))
              [:socket-exception "Socket closed"])
    ;; zero-length calls never reach the fd, and the JVM answers them closed
    (check-eq "zero-length calls on a closed socket"
              [(.read a-in (byte-array 0)) (.read a-in (byte-array 4) 0 0)
               (.write a-out (byte-array 0)) (.write a-out (byte-array 4) 0 0)]
              [0 0 nil nil])
    (let [buf (byte-array 64) n (.read (.getInputStream b) buf 0 64)]
      (check-eq "the next socket keeps its own bytes" (String. buf 0 n "UTF-8") "hello-B"))
    (check-eq "the next socket's peer got nothing from the closed one"
              (.available (.getInputStream b-peer)) 0)
    (finally (.close b-peer) (.close b) (.close a-peer) (.close server))))

;; the rest of a closed socket's surface answers from the object, never the fd
;; (which may be another socket's by now), and raises what the JVM raises.
(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      port   (.getLocalPort server)
      a      (java.net.Socket. "127.0.0.1" port)
      a-peer (.accept server)
      u      (java.net.Socket.)
      raised (fn [f] (try (f) :no-throw
                          (catch java.net.SocketException e [:socket-exception (ex-message e)])))]
  (check-eq "an unbound socket's local port is -1" (.getLocalPort u) -1)
  (.close a) (.close u)
  (check-eq "getInputStream on a closed socket" (raised #(.getInputStream a))
            [:socket-exception "Socket is closed"])
  (check-eq "getOutputStream on a closed socket" (raised #(.getOutputStream a))
            [:socket-exception "Socket is closed"])
  (check-eq "a closed unbound socket's local port is -1" (.getLocalPort u) -1)
  (check-eq "connect on a closed socket"
            (raised #(.connect u (java.net.InetSocketAddress. "127.0.0.1" port)))
            [:socket-exception "Socket is closed"])
  (.close a-peer) (.close server)
  (check-eq "accept on a closed server socket" (raised #(.accept server))
            [:socket-exception "Socket is closed"]))

;; Closing a socket is how another thread stops a blocked read or accept, and the
;; blocked call raises. It used to wait on in a private kqueue/epoll that close
;; never signalled, so it hung for good. The JVM prints
;; [SocketException "Socket closed"] for each of these; a write stuck on a full
;; send buffer answers "Broken pipe" there instead.
(defn blocked-then-closed [call target]
  (let [p (promise)
        _ (future (deliver p (try (call) :no-throw
                                  (catch java.net.SocketException e
                                    [:socket-exception (ex-message e)]))))]
    (Thread/sleep 200)
    (.close target)
    (deref p 5000 :hung)))

(with-pair
  (fn [server client conn]
    (let [in (.getInputStream conn)]
      (check-eq "close wakes a thread blocked in read"
                (blocked-then-closed #(.read in) conn)
                [:socket-exception "Socket closed"]))))

(let [server (java.net.ServerSocket. 0)]
  (check-eq "close wakes a thread blocked in accept"
            (blocked-then-closed #(.accept server) server)
            [:socket-exception "Socket closed"]))

(with-pair
  (fn [server client conn]
    ;; The peer never reads, so the send buffer fills and a write blocks. A slow
    ;; machine can still be copying a chunk when close lands, and the next write
    ;; then raises "Socket closed", which is also what the JVM does there; what
    ;; matters is that the writer wakes and raises.
    (let [out (.getOutputStream client)
          chunk (byte-array (* 64 1024))
          r (blocked-then-closed #(loop [] (.write out chunk) (recur)) client)]
      (check-eq "close wakes a thread blocked in write"
                (if (contains? #{[:socket-exception "Broken pipe"]
                                 [:socket-exception "Socket closed"]} r)
                  :raised
                  r)
                :raised))))

;; The same on a fiber, where the woken read used to retry recv on the fd number
;; close had freed: B, opened right after, is handed that number, and A's read
;; took B's bytes. A's fd stays reserved until its read has left.
(require '[jolt.fibers :as fib])
(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      port   (.getLocalPort server)
      a      (java.net.Socket. "127.0.0.1" port)
      a-peer (.accept server)
      a-in   (.getInputStream a)
      p      (promise)
      _      (fib/spawn (fn [] (deliver p (try (.read a-in) :no-throw
                                               (catch java.net.SocketException e
                                                 [:socket-exception (ex-message e)])))))
      _      (Thread/sleep 200)
      _      (.close a)
      b      (java.net.Socket. "127.0.0.1" port)
      b-peer (.accept server)]
  (try
    (.write (.getOutputStream b-peer) (.getBytes "B" "UTF-8") 0 1)
    (check-eq "close wakes a fiber blocked in read" (deref p 5000 :hung)
              [:socket-exception "Socket closed"])
    (check-eq "the woken fiber left the next socket's bytes alone"
              (.read (.getInputStream b)) (int \B))
    (finally (.close b-peer) (.close b) (.close a-peer) (.close server))))

;; Half-close (jolt-lang/jolt#1208): shutdownOutput sends FIN and keeps the read
;; direction, shutdownInput reads EOF and keeps the write direction, and the two
;; is*Shutdown predicates report it. Every expected value below is what JDK 21
;; answers for the same forms over loopback TCP (certify.clj over these forms as
;; corpus rows: 6/6 certified). They cannot live in the corpus itself, whose
;; runner has no jolt.socket to install.
(defn- sock-msg [f]
  (try (f) :ok (catch java.net.SocketException e (.getMessage e))))

(with-pair
  (fn [server c s]
    (.write (.getOutputStream c) 120)
    (let [before (.isOutputShutdown c)
          _ (.shutdownOutput c)
          si (.getInputStream s)]
      (.write (.getOutputStream s) 122)
      (check-eq "shutdownOutput sends the peer EOF and leaves this side reading"
                [before (.isOutputShutdown c) (.isInputShutdown c)
                 (.read si) (.read si) (.read si)
                 (.read (.getInputStream c)) (.isClosed c) (.isConnected c)]
                [false true false 120 -1 -1 122 false true]))))

(with-pair
  (fn [server c s]
    (let [o (.getOutputStream c)]
      (.shutdownOutput c)
      (check-eq "after shutdownOutput a write throws and the state is named"
                [(sock-msg #(.write o 1)) (sock-msg #(.write o (byte-array [1 2])))
                 (sock-msg #(.write o (byte-array 0))) (sock-msg #(.flush o))
                 (sock-msg #(.shutdownOutput c)) (sock-msg #(.getOutputStream c))
                 (sock-msg #(.getInputStream c))]
                ["Broken pipe" "Broken pipe" :ok :ok "Socket output is already shutdown"
                 "Socket output is shutdown" :ok]))))

(with-pair
  (fn [server c s]
    (.write (.getOutputStream s) 113)
    (Thread/sleep 100)
    (let [i (.getInputStream c)]
      (.shutdownInput c)
      (check-eq "shutdownInput reads EOF over pending data and leaves this side writing"
                [(.isInputShutdown c) (.isOutputShutdown c)
                 (.read i) (.read i (byte-array 4)) (.read i (byte-array 4) 0 4)
                 (.read i (byte-array 0)) (.available i)
                 (.readLine (java.io.BufferedReader. (java.io.InputStreamReader. i)))
                 (sock-msg #(.getInputStream c)) (sock-msg #(.shutdownInput c))
                 (do (.write (.getOutputStream c) 119) (.read (.getInputStream s)))]
                [true false -1 -1 -1 0 0 nil "Socket input is shutdown"
                 "Socket input is already shutdown" 119]))))

(let [u (java.net.Socket.)]
  (check-eq "a half-close needs a connected, open socket"
            [(sock-msg #(.shutdownOutput u)) (sock-msg #(.shutdownInput u))
             (.isOutputShutdown u) (.isInputShutdown u)
             (sock-msg #(.getInputStream u)) (sock-msg #(.getOutputStream u))
             (do (.close u) (sock-msg #(.shutdownOutput u))) (sock-msg #(.getInputStream u))]
            ["Socket is not connected" "Socket is not connected" false false
             "Socket is not connected" "Socket is not connected"
             "Socket is closed" "Socket is closed"]))

(with-pair
  (fn [server c s]
    (.shutdownOutput c)
    (.close c)
    (check-eq "the half-closed state outlives close, which then refuses a shutdown"
              [(.isOutputShutdown c) (.isInputShutdown c) (sock-msg #(.shutdownOutput c))
               (sock-msg #(.shutdownInput c)) (sock-msg #(.getInputStream c))]
              [true false "Socket is closed" "Socket is closed" "Socket is closed"])))

(with-pair
  (fn [server c s]
    (let [f (future (.read (.getInputStream c)))]
      (Thread/sleep 200)
      (.shutdownInput c)
      (check-eq "shutdownInput wakes a thread blocked in read with EOF"
                (deref f 5000 :blocked) -1))))

;; On a fiber the read is parked on the poller rather than blocked in recv, and
;; the shutdown has to reach it the same way.
(with-pair
  (fn [server c s]
    (let [p (promise)
          in (.getInputStream c)]
      (fib/spawn (fn [] (deliver p (try (.read in) (catch Throwable e [:threw (str e)])))))
      (Thread/sleep 200)
      (.shutdownInput c)
      (check-eq "shutdownInput wakes a fiber blocked in read with EOF"
                (deref p 5000 :blocked) -1))))

;; The peer of a half-closed socket sees a whole conversation: request, FIN,
;; then the response and the close — what a proxy pumping one direction does.
(with-pair
  (fn [server c s]
    (let [co (.getOutputStream c)]
      (.write co (.getBytes "request" "UTF-8"))
      (.shutdownOutput c)
      ;; not slurp: it closes the stream, and closing a socket's stream closes
      ;; the socket, on the JVM as here
      (let [in (.getInputStream s)
            req (loop [acc []]
                  (let [b (.read in)]
                    (if (neg? b) (String. (byte-array acc) "UTF-8") (recur (conj acc b)))))]
        (.write (.getOutputStream s) (.getBytes (str "echo:" req) "UTF-8"))
        (.close s)
        (check-eq "a request half-closed by the client reads to EOF, and the reply arrives"
                  (slurp (.getInputStream c)) "echo:request")))))

;; A socket's streams are java.io streams to clojure.java.io: io/reader,
;; io/writer, io/input-stream, io/output-stream and io/copy all raised "Cannot
;; open" over them, so the ordinary (io/reader (.getInputStream sock)) did not
;; work. Measured against JDK 21 (the flush is the JVM's: io/output-stream is a
;; BufferedOutputStream there).
(with-pair
  (fn [server c s]
    (io/copy "abc\n" (.getOutputStream c))
    (let [w (io/writer (.getOutputStream c))] (.write w "de\n") (.flush w))
    (let [o (io/output-stream (.getOutputStream c))]
      (io/copy (.getBytes "x\ny\n" "UTF-8") o)
      (.flush o))
    (.shutdownOutput c)
    (check-eq "clojure.java.io reads and writes a socket's streams"
              [(vec (line-seq (io/reader (io/input-stream (.getInputStream s))))) (.isClosed c)]
              [["abc" "de" "x" "y"] false])))

;; available() is a real byte count, from the same ioctl(FIONREAD) the JVM asks.
;; It answered 0 always, which java.io permits ("an estimate") but which leaves
;; (pos? (.available in)) false forever. ioctl is variadic, and binding it
;; fixed-arity is what made it look unreachable: on Apple arm64 the call returns
;; SUCCESS with the out-parameter untouched. jolt.ffi's :varargs marker puts the
;; argument where the callee reads it. The JVM prints [0 14 9 0] for this.
(with-pair
  (fn [server client conn]
    (let [in (.getInputStream conn)
          msg (.getBytes "hello over tcp" "UTF-8")]
      (check-eq "available before anything is sent" (.available in) 0)
      (.write (.getOutputStream client) msg 0 (alength msg))
      ;; loopback delivery is not instant; wait for it rather than assume it
      (loop [tries 0]
        (when (and (zero? (.available in)) (< tries 100))
          (Thread/sleep 10)
          (recur (inc tries))))
      (check-eq "available counts what arrived" (.available in) 14)
      (.read in (byte-array 5) 0 5)
      (check-eq "available drops by what was read" (.available in) 9)
      (.read in (byte-array 64) 0 64)
      (check-eq "available is 0 once drained" (.available in) 0))))

;; and it is not bounded by any buffer of jolt's — the kernel's whole count,
;; which is what the JVM answers here too
(with-pair
  (fn [server client conn]
    (let [in (.getInputStream conn)]
      (.write (.getOutputStream client) (byte-array 20000) 0 20000)
      (loop [tries 0]
        (when (and (< (.available in) 20000) (< tries 100))
          (Thread/sleep 10)
          (recur (inc tries))))
      (check-eq "available counts past any buffer" (.available in) 20000))))

;; a peer that closed leaves its bytes readable, and the count with them
(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      client (java.net.Socket. "127.0.0.1" (.getLocalPort server))
      conn   (.accept server)
      in     (.getInputStream conn)]
  (.write (.getOutputStream client) (.getBytes "tail" "UTF-8") 0 4)
  (.close client)
  (loop [tries 0]
    (when (and (zero? (.available in)) (< tries 100))
      (Thread/sleep 10)
      (recur (inc tries))))
  (check-eq "available after the peer closed" (.available in) 4)
  (.read in (byte-array 8) 0 8)
  (check-eq "available at end of stream" (.available in) 0)
  (.close conn) (.close server))

;; and a CLOSED socket raises SocketException, as Java's does. Asking the kernel
;; about a closed fd would be worse than wrong: the number is free to have been
;; reused by the next socket, so the count would be somebody else's.
(with-pair
  (fn [server client conn]
    (let [in (.getInputStream conn)]
      (.close conn)
      (check-eq "available on a closed socket"
                (try (.available in)
                     (catch java.io.IOException e [(class e) (.getMessage e)]))
                [java.net.SocketException "Socket closed"]))))


;; -- SO_TIMEOUT and the timed connect (jolt-lang/jolt#1191, #1192) -------------
;; Every expectation here was read off JDK 20, messages included. A timed-out
;; read or accept raises SocketTimeoutException and leaves the socket open and
;; usable; a timed-out or refused connect closes it.
(defn thrown [f]
  (try (f) :no-throw
       (catch Exception e [(.getSimpleName (class e)) (ex-message e)])))

(defn elapsed-ms [f]
  (let [t0 (System/nanoTime)]
    [(f) (/ (- (System/nanoTime) t0) 1000000)]))

;; Whether a wait due after `due` ms that took `ms` was on time. Timed on
;; System/nanoTime, the clock the poller's deadlines are on, so the lower bound
;; is exact: a timeout never fires before it is due, here as on the JDK (the
;; old 50 ms of slack below the timeout was for currentTimeMillis). The upper
;; bound only has to catch a timeout that fires far too late, and one late
;; wakeup on a loaded machine can cross any bound, so a reading past it is
;; taken again with `remeasure` (-> ms) and only a second late reading fails.
(defn on-time? [due ms remeasure]
  (and (>= ms due)
       (or (<= ms 2000)
           (let [ms2 (remeasure)] (<= due ms2 2000)))))

(with-pair
  (fn [server client conn]
    (check-eq "getSoTimeout defaults to 0" (.getSoTimeout client) 0)
    (.setSoTimeout client 300)
    (check-eq "getSoTimeout round-trips" (.getSoTimeout client) 300)
    (check-eq "a negative SO_TIMEOUT is refused"
              (thrown #(.setSoTimeout client -1))
              ["IllegalArgumentException" "timeout can't be negative"])
    (check-eq "an accepted socket does not inherit the listener's timeout"
              (.getSoTimeout conn) 0)
    (let [in (.getInputStream client)
          [r ms] (elapsed-ms #(thrown (fn [] (.read in))))]
      (check-eq "a read with nothing to read times out" r
                ["SocketTimeoutException" "Read timed out"])
      (check-eq "…not before the timeout, nor far past it"
                (on-time? 300 ms #(second (elapsed-ms (fn [] (thrown (fn [] (.read in)))))))
                true)
      (check-eq "…and is an InterruptedIOException"
                (try (.read in) (catch java.io.InterruptedIOException _ :caught))
                :caught)
      (check-eq "…and leaves the socket open" (.isClosed client) false)
      (.write (.getOutputStream conn) (.getBytes "ok" "UTF-8") 0 2)
      (let [buf (byte-array 8)
            n (.read in buf 0 8)]
        (check-eq "the next read after a timeout gets the data" (String. buf 0 n "UTF-8") "ok"))
      ;; bytes already there are returned, whatever the timeout
      (.write (.getOutputStream conn) 65)
      (check-eq "a read with data waiting returns it" (.read in) 65)
      (check-eq "the timed-out read is not EOF"
                (thrown #(.read in (byte-array 4)))
                ["SocketTimeoutException" "Read timed out"])
      ;; 0 is infinite again
      (.setSoTimeout client 0)
      (future (Thread/sleep 400) (.write (.getOutputStream conn) 66))
      (check-eq "SO_TIMEOUT 0 blocks until data" (.read in) 66))
    (.close client)
    (check-eq "setSoTimeout on a closed socket"
              (thrown #(.setSoTimeout client 5)) ["SocketException" "Socket is closed"])
    (check-eq "getSoTimeout on a closed socket"
              (thrown #(.getSoTimeout client)) ["SocketException" "Socket is closed"])))

;; A 1ms SO_TIMEOUT ends at its deadline. The poller's deadline was a whole
;; millisecond rounded up from now, so with the wake latency on top the read took
;; ~2.4ms where the JDK's takes ~1.2. Timed interleaved with a (Thread/sleep 1)
;; from this run, so load stretches both; a round that noise puts behind is
;; retried, and the rounding shows in every round.
(defn- median-ns [xs] (nth (sort xs) (quot (count xs) 2)))
(defn- time-ns [f] (let [t (System/nanoTime)] (f) (- (System/nanoTime) t)))
(with-pair
  (fn [server client conn]
    (.setSoTimeout client 1)
    (let [in (.getInputStream client)
          rd #(try (.read in) (catch java.net.SocketTimeoutException _ nil))
          sl #(Thread/sleep 1)]
      (rd) (sl)
      (check-eq "a 1ms read timeout never fires early"
                (every? #(>= % 1000000) (repeatedly 5 #(time-ns rd))) true)
      (check-eq "a 1ms read timeout costs a sleep(1), not two ticks"
                (boolean
                  (some (fn [_]
                          (let [pairs (doall (repeatedly 21 #(vector (time-ns rd) (time-ns sl))))
                                r (median-ns (map first pairs)) s (median-ns (map second pairs))]
                            (<= r (+ (* 5/4 s) 100000))))
                        (range 4)))
                true))))

(let [server (java.net.ServerSocket. 0)]
  (check-eq "ServerSocket getSoTimeout defaults to 0" (.getSoTimeout server) 0)
  (.setSoTimeout server 200)
  (check-eq "ServerSocket getSoTimeout round-trips" (.getSoTimeout server) 200)
  (check-eq "ServerSocket refuses a negative timeout"
            (thrown #(.setSoTimeout server -1)) ["IllegalArgumentException" "timeout < 0"])
  (let [[r ms] (elapsed-ms #(thrown (fn [] (.accept server))))]
    (check-eq "accept with nobody dialing times out" r
              ["SocketTimeoutException" "Accept timed out"])
    (check-eq "…not before the timeout, nor far past it"
              (on-time? 200 ms #(second (elapsed-ms (fn [] (thrown (fn [] (.accept server)))))))
              true))
  (let [c (java.net.Socket. "127.0.0.1" (.getLocalPort server))
        a (.accept server)]
    (check-eq "the listener still accepts after a timeout" (.isConnected a) true)
    (.close a) (.close c))
  (.close server)
  (check-eq "ServerSocket getSoTimeout on a closed socket"
            (thrown #(.getSoTimeout server)) ["SocketException" "Socket is closed"]))

;; The same on a fiber: the timeout wakes the parked fiber with the exception, not
;; EOF and not a hang, and close still wins over a timeout that has not expired.
(with-pair
  (fn [server client conn]
    (.setSoTimeout client 200)
    (let [p (promise)]
      (fib/spawn (fn [] (deliver p (thrown #(.read (.getInputStream client))))))
      (check-eq "a fiber's read times out" (deref p 5000 :hung)
                ["SocketTimeoutException" "Read timed out"]))
    (let [p (promise)]
      (fib/spawn (fn [] (deliver p (thrown #(.read (.getInputStream client))))))
      (Thread/sleep 50)
      (.write (.getOutputStream conn) 67)
      (check-eq "a fiber's timed read still gets data that arrives in time"
                (deref p 5000 :hung) :no-throw))
    (.setSoTimeout client 5000)
    (let [p (promise)]
      (fib/spawn (fn [] (deliver p (thrown #(.read (.getInputStream client))))))
      (Thread/sleep 200)
      (.close client)
      (check-eq "close wakes a fiber in a timed read" (deref p 3000 :hung)
                ["SocketException" "Socket closed"]))))

(let [server (java.net.ServerSocket. 0)
      p (promise)]
  (.setSoTimeout server 200)
  (fib/spawn (fn [] (deliver p (thrown #(.accept server)))))
  (check-eq "a fiber's accept times out" (deref p 5000 :hung)
            ["SocketTimeoutException" "Accept timed out"])
  (.close server))

;; connect(endpoint, timeout). A listener that never accepts, with a backlog of
;; one, stops completing handshakes once the queue is full, so a connect to it
;; hangs until its timeout — on the JVM as here. macOS stalls the second dial,
;; Linux the third; the loop takes whichever.
(defn dial-until-stalled [port ms]
  (loop [held [] i 0]
    (let [s (java.net.Socket.)
          [r el] (elapsed-ms #(thrown (fn [] (.connect s (java.net.InetSocketAddress. "127.0.0.1" port) ms))))]
      (if (and (= r :no-throw) (< i 16))
        (recur (conj held s) (inc i))
        {:held held :result r :ms el :closed? (.isClosed s)}))))

(let [server (java.net.ServerSocket. 0 1)
      {:keys [held result ms closed?]} (dial-until-stalled (.getLocalPort server) 300)]
  (check-eq "a connect that cannot complete times out" result
            ["SocketTimeoutException" "Connect timed out"])
  (check-eq "…not before the timeout, nor far past it"
            (on-time? 300 ms #(let [again (dial-until-stalled (.getLocalPort server) 300)]
                                (doseq [s (:held again)] (.close s))
                                (:ms again)))
            true)
  (check-eq "…and closes the socket" closed? true)
  (let [p (promise) port (.getLocalPort server)]
    (fib/spawn (fn [] (deliver p (thrown #(.connect (java.net.Socket.)
                                                    (java.net.InetSocketAddress. "127.0.0.1" port) 200)))))
    (check-eq "a fiber's connect times out" (deref p 5000 :hung)
              ["SocketTimeoutException" "Connect timed out"]))
  (let [s (java.net.Socket.) p (promise) port (.getLocalPort server)]
    (future (deliver p (thrown #(.connect s (java.net.InetSocketAddress. "127.0.0.1" port) 5000))))
    (Thread/sleep 200)
    (.close s)
    (check-eq "close wakes a timed connect" (deref p 3000 :hung)
              ["SocketException" "Socket closed"]))
  (doseq [s held] (.close s))
  (.close server))

(let [s (java.net.Socket.)]
  (check-eq "a negative connect timeout is refused"
            (thrown #(.connect s (java.net.InetSocketAddress. "127.0.0.1" 1) -1))
            ["IllegalArgumentException" "connect: timeout can't be negative"])
  (check-eq "…and leaves the socket open" (.isClosed s) false)
  (.close s))

(let [server (java.net.ServerSocket. 0)
      port (.getLocalPort server)
      s (java.net.Socket.)]
  (.close server)
  (check-eq "a refused connect is a ConnectException"
            (thrown #(.connect s (java.net.InetSocketAddress. "127.0.0.1" port) 1000))
            ["ConnectException" "Connection refused"])
  (check-eq "…and closes the socket" (.isClosed s) true))

(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      s (java.net.Socket.)]
  (.connect s (java.net.InetSocketAddress. "127.0.0.1" (.getLocalPort server)) 1000)
  (check-eq "a timed connect that completes is connected" (.isConnected s) true)
  (.close s) (.close server))

;; -- host identity: InetAddress statics + NetworkInterface --------------------
;; What a program asks about the machine it is on. The loopback interface is
;; found by the address it carries, not by name — it is lo0 on macOS and lo on
;; Linux, and a gate that hardcodes either is a gate that only runs on one.

(defn- dotted-quad? [s]
  (boolean (and (string? s) (re-matches #"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}" s))))

(def local-host (java.net.InetAddress/getLocalHost))

(check-eq "getLocalHost is an InetAddress" (instance? java.net.InetAddress local-host) true)
(check-eq "getLocalHost has a dotted-quad address" (dotted-quad? (.getHostAddress local-host)) true)
(check-eq "getLocalHost names the host" (pos? (count (.getHostName local-host))) true)
;; toString is "host/address", the JVM's shape.
(check-eq "getLocalHost toString joins host and address"
          (str local-host) (str (.getHostName local-host) "/" (.getHostAddress local-host)))
(check-eq "getCanonicalHostName answers a name"
          (pos? (count (.getCanonicalHostName local-host))) true)

;; getAllByName returns an ARRAY on the JVM, so alength and aget both hold.
(def all-loopback (java.net.InetAddress/getAllByName "localhost"))
(check-eq "getAllByName returns a non-empty array" (pos? (alength all-loopback)) true)
(check-eq "getAllByName resolves localhost to the loopback address"
          (boolean (some (fn [a] (= "127.0.0.1" (.getHostAddress a))) (seq all-loopback))) true)

(def interfaces (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces)))
(check-eq "getNetworkInterfaces enumerates at least one interface" (pos? (count interfaces)) true)
(check-eq "every interface has a name"
          (every? (fn [ni] (pos? (count (.getName ni)))) interfaces) true)
(check-eq "an interface is a NetworkInterface"
          (instance? java.net.NetworkInterface (first interfaces)) true)

(def loopback-ni
  (first (filter (fn [ni]
                   (some (fn [a] (= "127.0.0.1" (.getHostAddress a)))
                         (enumeration-seq (.getInetAddresses ni))))
                 interfaces)))

(check-eq "the loopback interface is enumerated" (some? loopback-ni) true)
;; An address read off an interface carries no hostname until asked — the JVM
;; prints it as "/127.0.0.1" — and .getHostName resolves once and caches.
(check-eq "an interface address is unresolved until asked"
          (str (first (filter (fn [a] (= "127.0.0.1" (.getHostAddress a)))
                              (enumeration-seq (.getInetAddresses loopback-ni)))))
          "/127.0.0.1")
(check-eq "getHostName resolves it and caches the name"
          (let [a (first (filter (fn [x] (= "127.0.0.1" (.getHostAddress x)))
                                 (enumeration-seq (.getInetAddresses loopback-ni))))]
            (.getHostName a)
            (str a))
          "localhost/127.0.0.1")
(check-eq "getByName finds the same interface"
          (.getName (java.net.NetworkInterface/getByName (.getName loopback-ni)))
          (.getName loopback-ni))
(check-eq "getByInetAddress finds the loopback interface"
          (.getName (java.net.NetworkInterface/getByInetAddress
                      (java.net.InetAddress/getByName "127.0.0.1")))
          (.getName loopback-ni))
;; No interface carries a routable public address of someone else's.
(check-eq "getByInetAddress is nil for an address no interface holds"
          (java.net.NetworkInterface/getByInetAddress
            (java.net.InetAddress/getByName "8.8.8.8"))
          nil)
(check-eq "getByName is nil for an interface that does not exist"
          (java.net.NetworkInterface/getByName "jolt-no-such-iface0") nil)
;; The loopback has no hardware address on the JVM; a physical one is 6 bytes.
(check-eq "loopback has no hardware address" (.getHardwareAddress loopback-ni) nil)
(check-eq "a hardware address, where present, is six bytes"
          (every? (fn [ni] (let [h (.getHardwareAddress ni)]
                             (or (nil? h) (= 6 (alength h)))))
                  interfaces)
          true)
(check-eq "toString names the interface"
          (str loopback-ni)
          (str "name:" (.getName loopback-ni) " (" (.getDisplayName loopback-ni) ")"))

;; -- System/getProperties is a java.util.Properties ---------------------------
;; It answers getProperty, not just map lookup: a JVM library reads system
;; properties through the Properties API (clj-uuid's node id digests six of them).
(def sys-props (System/getProperties))
(check-eq "getProperties answers getProperty"
          (.getProperty sys-props "os.name") (System/getProperty "os.name"))
(check-eq "getProperty falls back to its default"
          (.getProperty sys-props "jolt.no.such.property" "fallback") "fallback")
(check-eq "getProperties is still map-readable"
          (get sys-props "os.name") (System/getProperty "os.name"))
(check-eq "setProperty writes through"
          (do (.setProperty sys-props "jolt.socket.test.prop" "set")
              (System/getProperty "jolt.socket.test.prop"))
          "set")
(check-eq "stringPropertyNames includes a known key"
          (contains? (set (.stringPropertyNames sys-props)) "os.name") true)
(check-eq "getProperties is a Properties"
          (instance? java.util.Properties sys-props) true)
;; count, seq and get answer over the same key set — a value visible to one of
;; them and not the others is the half-map state this shape invites.
(check-eq "count and seq agree" (count sys-props) (count (seq sys-props)))
(check-eq "every key seq reports is readable"
          (every? (fn [k] (= (get sys-props k) (.getProperty sys-props k))) (keys sys-props))
          true)
(check-eq "into {} round-trips the whole view"
          (count (into {} sys-props)) (count sys-props))
(check-eq "setProperty through the object is what System/getProperty reports"
          (do (.setProperty sys-props "jolt.socket.test.wt" "through")
              (System/getProperty "jolt.socket.test.wt"))
          "through")
;; the computed values are recomputed per call, not frozen at the first one.
(check-eq "user.dir is current, not a frozen entry"
          (.getProperty (System/getProperties) "user.dir") (System/getProperty "user.dir"))

;; A Properties built by hand carries its own defaults. The JVM is precise about
;; which operations span that chain — getProperty and the two name enumerations
;; do, the inherited Hashtable surface does not — and these pin that split.
(def defaulted (java.util.Properties. {"only-default" "from-defaults"}))
(check-eq "getProperty spans the defaults"
          (.getProperty defaulted "only-default") "from-defaults")
(check-eq "propertyNames spans the defaults"
          (vec (enumeration-seq (.propertyNames defaulted))) ["only-default"])
(check-eq "stringPropertyNames spans the defaults"
          (vec (.stringPropertyNames defaulted)) ["only-default"])
(check-eq "containsKey does NOT span the defaults"
          (.containsKey defaulted "only-default") false)
(check-eq "keySet does NOT span the defaults" (vec (.keySet defaulted)) [])
(check-eq "isEmpty does NOT span the defaults" (.isEmpty defaulted) true)
(check-eq "an own value wins over the defaults"
          (let [p (java.util.Properties. {"k" "default"})]
            (.setProperty p "k" "own")
            (.getProperty p "k"))
          "own")
;; setProperty delegates to put on the JVM, so it reports THIS object's previous
;; value — nil when the key was only ever in the defaults, not the value it is
;; now shadowing.
(check-eq "setProperty reports the own previous value, not the shadowed default"
          (.setProperty (java.util.Properties. {"k" "default"}) "k" "own") nil)
;; the chain is walked recursively: a Properties whose defaults is a Properties.
(check-eq "nested defaults are searched through"
          (.getProperty (java.util.Properties. (java.util.Properties. {"deep" "found"})) "deep")
          "found")
(check-eq "propertyNames enumerates the whole chain"
          (vec (enumeration-seq
                 (.propertyNames (java.util.Properties. (java.util.Properties. {"deep" "found"})))))
          ["deep"])
(check-eq "removing an own value uncovers the default"
          (let [p (java.util.Properties. {"k" "default"})]
            (.setProperty p "k" "own")
            (.remove p "k")
            (.getProperty p "k"))
          "default")

;; -- jolt.socket.native: the addrinfo layout probe (#979) ------------------------
;; ai_canonname and ai_addr trade places between libcs (glibc: ai_addr at 24; the
;; BSDs, Win64 and bionic: 32), and bionic calls itself Linux, so the resolver
;; finds ai_addr by checking which slot holds a sockaddr of ai_family. A node is
;; built by hand in each layout, so every host checks both — including the one
;; that broke Android when the offset came from os.name.
(require '[jolt.socket.native :as native] '[jolt.ffi :as ffi])
(let [ai-addr @(ns-resolve 'jolt.socket.native 'ai-addr)
      [sa _] (native/make-sockaddr native/af-inet "10.1.2.3" 80)
      node (fn [off]
             (let [ai (ffi/alloc 48)]
               (ffi/write ai :int native/af-inet 4)
               (ffi/write ai :pointer sa off)
               ai))
      glibc (node 24)
      bsd (node 32)
      canon (ffi/string->ptr "example.org")
      both (doto (node 32) (ffi/write :pointer canon 24))
      none (ffi/alloc 48)]
  (try
    (check-eq "ai_addr probe: glibc order" (native/sockaddr-ip (ai-addr glibc native/af-inet)) "10.1.2.3")
    (check-eq "ai_addr probe: BSD/bionic order" (native/sockaddr-ip (ai-addr bsd native/af-inet)) "10.1.2.3")
    (check-eq "ai_addr probe: a canonname in the other slot is not taken for it"
              (native/sockaddr-ip (ai-addr both native/af-inet)) "10.1.2.3")
    (check-eq "ai_addr probe: no address in either slot" (ai-addr none native/af-inet) nil)
    (finally (doseq [p [sa glibc bsd canon both none]] (ffi/free p)))))

;; clojure.java.io's openers take a Socket itself, as io.clj's IOFactory does:
;; its own streams, opened by getInputStream / getOutputStream
(with-pair
  (fn [server client conn]
    (with-open [w (clojure.java.io/writer client)] (.write w "hi\n"))
    (check-eq "io/writer and io/reader on a Socket"
              (with-open [r (clojure.java.io/reader conn)] (.readLine r)) "hi")))
(with-pair
  (fn [server client conn]
    (with-open [o (clojure.java.io/output-stream client)] (.write o (byte-array [1 2 3])))
    (check-eq "io/output-stream and io/input-stream on a Socket"
              (with-open [i (clojure.java.io/input-stream conn)] (vec (.readAllBytes i))) [1 2 3])))

;; InetAddress and InetSocketAddress are values: = and hash go by the address
;; (and port), as their equals/hashCode do, and hash is hashCode.
(let [a (java.net.InetAddress/getByName "127.0.0.1")
      b (java.net.InetAddress/getByName "127.0.0.1")]
  (check-eq "InetAddress = by address" [(= a b) (count (hash-set a b)) (= (hash a) (.hashCode a))]
            [true 1 true])
  (check-eq "InetAddress hashCode is the address as an int" (.hashCode a) 2130706433))
(check-eq "InetSocketAddress = by host and port"
          [(= (java.net.InetSocketAddress. "localhost" 80) (java.net.InetSocketAddress. "localhost" 80))
           (= (java.net.InetSocketAddress. "localhost" 80) (java.net.InetSocketAddress. "localhost" 81))
           (= (hash (java.net.InetSocketAddress. "localhost" 80))
              (hash (java.net.InetSocketAddress. "localhost" 80)))]
          [true false true])
;; values the JVM answers: a resolved one compares and hashes by its address,
;; an unresolved one by its host name ignoring case
(check-eq "InetSocketAddress = and hash by resolved address"
          [(= (java.net.InetSocketAddress. "127.0.0.1" 80) (java.net.InetSocketAddress. "127.0.0.1" 80))
           (hash (java.net.InetSocketAddress. "127.0.0.1" 80))
           (.hashCode (java.net.InetSocketAddress. "127.0.0.1" 80))
           (.isUnresolved (java.net.InetSocketAddress. "no-such-host.invalid" 80))
           (hash (java.net.InetSocketAddress. "No-Such-Host.invalid" 80))
           (= (java.net.InetSocketAddress. "no-such-host.invalid" 80)
              (java.net.InetSocketAddress. "NO-SUCH-HOST.invalid" 80))]
          [true 2130706513 2130706513 true -1080077125 true])

;; -- IPv6 ------------------------------------------------------------------------
;; Every expected value below is what JDK 21 answers for the same expression.

(defn- by-name [h] (java.net.InetAddress/getByName h))
(defn- thrown-msg [f]
  (try (f) :no-throw
       (catch java.net.UnknownHostException e (ex-message e))))

;; literals: brackets or not, printed as eight uncompressed groups
(check-eq "v6 literal prints uncompressed"
          (mapv (fn [h] (str (by-name h)))
                ["::1" "[::1]" "::" "2001:DB8:0:0:0:0:0:1" "1:2:3:4:5:6:1.2.3.4"
                 "01234::1" "::1.2.3.4"])
          ["/0:0:0:0:0:0:0:1" "/0:0:0:0:0:0:0:1" "/0:0:0:0:0:0:0:0" "/2001:db8:0:0:0:0:0:1"
           "/1:2:3:4:5:6:102:304" "/1234:0:0:0:0:0:0:1" "/0:0:0:0:0:0:102:304"])
(check-eq "v6 getHostAddress"
          (.getHostAddress (by-name "2001:db8::ff00:42:8329")) "2001:db8:0:0:0:ff00:42:8329")
(check-eq "v6 class and instance?"
          (let [a (by-name "::1")]
            [(.getName (class a)) (instance? java.net.Inet6Address a)
             (instance? java.net.InetAddress a) (instance? java.net.Inet4Address a)
             (instance? java.net.Inet6Address (by-name "127.0.0.1"))])
          ["java.net.Inet6Address" true true false false])
(check-eq "a mapped address is an Inet4Address"
          (let [a (by-name "::ffff:1.2.3.4")] [(str a) (.getName (class a))])
          ["/1.2.3.4" "java.net.Inet4Address"])
(check-eq "v6 getAddress is 16 bytes"
          [(vec (.getAddress (by-name "::1")))
           (vec (.getAddress (by-name "2001:db8::ff00:42:8329")))]
          [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
           [32 1 13 -72 0 0 0 0 0 0 -1 0 0 66 -125 41]])
;; Inet6Address.hashCode sums the address as four ints of signed bytes
(check-eq "v6 hashCode"
          (mapv (fn [h] (.hashCode (by-name h)))
                ["::1" "2001:db8::ff00:42:8329" "fe80::1" "ffff:ffff::80ff"])
          [1 541232865 -41943039 -16875778])
(check-eq "v6 equality by address, scope aside"
          [(= (by-name "::1") (by-name "0:0::1"))
           (= (by-name "::1") (by-name "127.0.0.1"))
           (= (by-name "fe80::1%1") (by-name "fe80::1"))
           (hash (by-name "::1"))
           (count (hash-set (by-name "::1") (by-name "0::1")))]
          [true false true 1 1])
(check-eq "address predicates"
          (mapv (fn [[m h]] (m (by-name h)))
                [[#(.isLoopbackAddress %) "::1"] [#(.isLoopbackAddress %) "::2"]
                 [#(.isLoopbackAddress %) "127.0.0.2"] [#(.isAnyLocalAddress %) "::"]
                 [#(.isLinkLocalAddress %) "fe80::1"] [#(.isSiteLocalAddress %) "fec0::1"]
                 [#(.isMulticastAddress %) "ff02::1"] [#(.isSiteLocalAddress %) "192.168.1.1"]])
          [true false true true true true true true])
(check-eq "numeric scope zone"
          (let [a (by-name "fe80::1%1")] [(str a) (.getScopeId a) (str (by-name "[fe80::1%1]"))])
          ["/fe80:0:0:0:0:0:0:1%1" 1 "/fe80:0:0:0:0:0:0:1%1"])
(check-eq "malformed v6 literals"
          (mapv (fn [h] (thrown-msg #(by-name h)))
                ["1::2::3" "[::1" "[127.0.0.1]" "1:2:3:4:5:6:7:8:9" "::12345" "::1%" "[]"
                 "::1%jolt-no-such-if0"])
          ["1::2::3: invalid IPv6 address literal" "[::1: invalid IPv6 address literal"
           "[127.0.0.1]: invalid IPv6 address literal"
           "1:2:3:4:5:6:7:8:9: invalid IPv6 address literal" "::12345: invalid IPv6 address literal"
           "::1%: invalid IPv6 address literal" "[]: invalid IPv6 address literal"
           "no such interface jolt-no-such-if0"])
;; textToNumericFormatV4's forms, and the hex one it refuses as ambiguous
(check-eq "v4 literal forms"
          [(str (by-name "127.1")) (str (by-name "1.2.3")) (str (by-name "2130706433"))
           (thrown-msg #(by-name "0x7f.0.0.1"))]
          ["/127.0.0.1" "/1.2.0.3" "/127.0.0.1" "0x7f.0.0.1"])
(check-eq "getByAddress"
          [(str (java.net.InetAddress/getByAddress (byte-array (concat (repeat 15 0) [1]))))
           (str (java.net.InetAddress/getByAddress "foo" (byte-array [127 0 0 1])))
           (thrown-msg #(java.net.InetAddress/getByAddress (byte-array 5)))]
          ["/0:0:0:0:0:0:0:1" "foo/127.0.0.1" "addr is of illegal length"])
(check-eq "getAllByName of a v6 literal"
          (mapv str (java.net.InetAddress/getAllByName "[::1]")) ["/0:0:0:0:0:0:0:1"])
;; IPv4 first for a name with both, java.net.preferIPv6Addresses being false
(check-eq "getAllByName puts IPv4 first"
          (let [as (map (fn [a] (instance? java.net.Inet6Address a))
                        (java.net.InetAddress/getAllByName "localhost"))]
            [(false? (first as)) (= as (sort as))])
          [true true])

;; InetSocketAddress brackets a v6 address in toString, not in getHostString
(check-eq "v6 InetSocketAddress"
          (let [isa (java.net.InetSocketAddress. "::1" 80)]
            [(str isa) (str (java.net.InetSocketAddress. "[::1]" 80)) (.getHostString isa)
             (.hashCode isa) (= isa (java.net.InetSocketAddress. "0::1" 80))
             (str (java.net.InetSocketAddress. "fe80::1%1" 80))
             (str (java.net.InetSocketAddress/createUnresolved "::1" 80))
             (str (java.net.InetSocketAddress. "[::1" 80))])
          ["/[0:0:0:0:0:0:0:1]:80" "/[0:0:0:0:0:0:0:1]:80" "0:0:0:0:0:0:0:1" 81 true
           "/[fe80:0:0:0:0:0:0:1%1]:80" "::1/<unresolved>:80" "[::1/<unresolved>:80"])

;; Over the wire. A host with no IPv6 loopback (a container with IPv6 off) cannot
;; bind ::1; that is said, not counted as a failure, and the rest still runs.
(def v6-loopback?
  (try (.close (java.net.ServerSocket. 0 50 (by-name "::1"))) true
       (catch java.io.IOException _
         (println "  (no IPv6 loopback here: the [::1] socket checks are skipped)")
         false)))

(defn- port-free [s port] (str/replace s (str port) "P"))

(when v6-loopback?
  (let [ss   (java.net.ServerSocket. 0 50 (by-name "::1"))
        port (.getLocalPort ss)
        c    (java.net.Socket. "::1" port)
        a    (.accept ss)]
    (try
      (check-eq "ServerSocket on ::1"
                [(port-free (str ss) port) (str (.getInetAddress ss))
                 (port-free (str (.getLocalSocketAddress ss)) port)]
                ["ServerSocket[addr=/0:0:0:0:0:0:0:1,localport=P]" "/0:0:0:0:0:0:0:1"
                 "/[0:0:0:0:0:0:0:1]:P"])
      (check-eq "v6 client addresses"
                [(str/replace (port-free (str c) port) #"localport=\d+" "localport=L")
                 (str (.getInetAddress c)) (port-free (str (.getRemoteSocketAddress c)) port)
                 (str (.getLocalAddress c))
                 (str/replace (str (.getLocalSocketAddress c)) #":\d+$" ":L")]
                ["Socket[addr=/0:0:0:0:0:0:0:1,port=P,localport=L]" "/0:0:0:0:0:0:0:1"
                 "/[0:0:0:0:0:0:0:1]:P" "/0:0:0:0:0:0:0:1" "/[0:0:0:0:0:0:0:1]:L"])
      (check-eq "v6 accepted addresses"
                [(str/replace (port-free (str a) port) #"port=\d+," "port=R,")
                 (str (.getInetAddress a)) (str (.getLocalAddress a))
                 (str/replace (str (.getRemoteSocketAddress a)) #":\d+$" ":R")]
                ["Socket[addr=/0:0:0:0:0:0:0:1,port=R,localport=P]" "/0:0:0:0:0:0:0:1"
                 "/0:0:0:0:0:0:0:1" "/[0:0:0:0:0:0:0:1]:R"])
      (let [msg (.getBytes "hello over v6" "UTF-8")]
        (.write (.getOutputStream c) msg 0 (alength msg)))
      (let [buf (byte-array 64) n (.read (.getInputStream a) buf 0 64)]
        (check-eq "v6 roundtrip client->server" (String. buf 0 n "UTF-8") "hello over v6"))
      (.write (.getOutputStream a) 33)
      (check-eq "v6 roundtrip server->client" (.read (.getInputStream c)) 33)
      ;; a ::1 listener takes no IPv4 peer, as on the JDK
      (check-eq "a ::1 listener refuses 127.0.0.1"
                (try (.close (java.net.Socket. "127.0.0.1" port)) :connected
                     (catch java.net.ConnectException e (ex-message e)))
                "Connection refused")
      (finally (.close a) (.close c) (.close ss))))

  ;; ServerSocket(port) is dual-stack: a [::1] client and a 127.0.0.1 one both
  ;; reach it, and the IPv4 peer reads back as an Inet4Address
  (let [ss   (java.net.ServerSocket. 0)
        port (.getLocalPort ss)
        c6   (java.net.Socket. "::1" port)
        a6   (.accept ss)
        c4   (java.net.Socket. "127.0.0.1" port)
        a4   (.accept ss)]
    (try
      (check-eq "wildcard listener takes both families"
                [(port-free (str ss) port)
                 (str (.getInetAddress a6)) (str (.getLocalAddress a6))
                 (str (.getInetAddress a4)) (str (.getLocalAddress a4))
                 (.getName (class (.getInetAddress a4)))]
                ["ServerSocket[addr=0.0.0.0/0.0.0.0,localport=P]"
                 "/0:0:0:0:0:0:0:1" "/0:0:0:0:0:0:0:1" "/127.0.0.1" "/127.0.0.1"
                 "java.net.Inet4Address"])
      (finally (.close a6) (.close c6) (.close a4) (.close c4) (.close ss))))

  ;; no-arg sockets take their family from the address they are given
  (let [ss (java.net.ServerSocket.)]
    (.bind ss (java.net.InetSocketAddress. "::" 0))
    (let [port (.getLocalPort ss)
          c (java.net.Socket.)]
      (check-eq "unconnected getLocalAddress is the wildcard"
                [(str (.getLocalAddress c)) (.getLocalSocketAddress c)] ["0.0.0.0/0.0.0.0" nil])
      (.connect c (java.net.InetSocketAddress. "::1" port))
      (let [a (.accept ss)]
        (try
          (check-eq "bind :: then connect [::1]"
                    [(str/replace (str ss) #"localport=\d+" "localport=P")
                     (str/replace (str c) #"port=\d+" "port=P")
                     (str (.getInetAddress a))]
                    ["ServerSocket[addr=/0:0:0:0:0:0:0:0,localport=P]"
                     "Socket[addr=/0:0:0:0:0:0:0:1,port=P,localport=P]" "/0:0:0:0:0:0:0:1"])
          (.write (.getOutputStream c) 7)
          (check-eq "no-arg v6 socket carries bytes" (.read (.getInputStream a)) 7)
          (finally (.close a) (.close c) (.close ss)))))))

;; ServerSocket toString shows the bind address as given, name and all
(let [ss (java.net.ServerSocket. 0 50 (by-name "localhost"))]
  (check-eq "ServerSocket toString keeps the bind name"
            (str/replace (str ss) #"localport=\d+" "localport=P")
            "ServerSocket[addr=localhost/127.0.0.1,localport=P]")
  (.close ss))

;; NetworkInterface: v6 addresses are listed, scoped to their interface by name
(let [lo6 (filter (fn [a] (instance? java.net.Inet6Address a))
                  (enumeration-seq (.getInetAddresses loopback-ni)))]
  (check-eq "loopback v6 addresses are scoped to the interface"
            (every? (fn [a] (str/ends-with? (.getHostAddress a) (str "%" (.getName loopback-ni)))) lo6)
            true)
  (when v6-loopback?
    (check-eq "the loopback interface lists ::1"
              (boolean (some (fn [a] (= a (by-name "::1"))) lo6)) true)
    (check-eq "getByInetAddress finds ::1 on the loopback"
              (.getName (java.net.NetworkInterface/getByInetAddress (by-name "::1")))
              (.getName loopback-ni))))
(check-eq "getInterfaceAddresses carries the prefix"
          (some (fn [ia] (when (= "127.0.0.1" (.getHostAddress (.getAddress ia)))
                           ;; the same InetAddress getInetAddresses handed out,
                           ;; as on the JDK, so a name resolved above shows here
                           [(str/replace (str ia) #"^[^/]*" "") (.getNetworkPrefixLength ia)]))
                (.getInterfaceAddresses loopback-ni))
          ["/127.0.0.1/8 [null]" 8])
(check-eq "every listed interface has an address"
          (every? (fn [ni] (.hasMoreElements (.getInetAddresses ni))) interfaces) true)

(if (empty? @failures)
  (println "SOCKET-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "SOCKET-TEST FAILED:" (count @failures))))

;; Done with the agent system's pools (futures, agents): end them, as a JVM
;; program does, or their idle workers hold the process up for their keep-alive.
(shutdown-agents)
