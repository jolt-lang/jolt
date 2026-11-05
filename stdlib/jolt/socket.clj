;; java.net.Socket / ServerSocket / InetSocketAddress for Jolt via jolt.ffi.
;; C sockets — POSIX, and ws2_32 on Windows — plus jolt.host/tagged-table and
;; ref-put!/ref-get for state.
;;
;; Usage: (require 'jolt.socket)  ;; registers classes globally

(ns jolt.socket
  "Socket support over the host's C sockets — POSIX, and Winsock on Windows.
  Registers Socket, ServerSocket, InetSocketAddress, InetAddress and the socket
  stream classes with the host class registry.

  Deliberate divergences from the JVM (test/conformance/known-divergences.edn):
  a recv error reads as EOF (-1) rather than throwing.

  IPv4 and IPv6, as the JDK has them: Inet4Address and Inet6Address, v6
  literals with or without brackets and with a scope zone, IPv4 answered
  first for a name with both. A socket's family follows the address it
  connects or binds to, and a listener on the wildcard address is dual-stack,
  taking IPv4 peers as mapped addresses that read back as Inet4Address.

  Sockets are non-blocking on every platform and wait on jolt.io-poller — kqueue,
  epoll, or WSAPoll on Windows — so a fiber parks rather than holding its
  carrier, and SO_TIMEOUT and the connect timeout are deadlines on that wait.
  On Windows NetworkInterface enumerates nothing, for want of a
  GetAdaptersAddresses walk; that is a recorded entry (jolt-lang/jolt#1107)."
  (:require [jolt.ffi :as ffi]
            [jolt.io-poller :as poller]
            [jolt.socket.native :as native]
            [clojure.string :as str]))

;; -- platform ----------------------------------------------------------------
;; Every C call, constant and struct layout lives in jolt.socket.native, which
;; knows macOS, Linux and Windows apart (jolt-lang/jolt#1107 is what answering
;; Linux's numbers on Windows cost). What is left here is the java.net object
;; model over it.

;; -- tagged-table constructors ------------------------------------------------
;; a "class" entry makes (class x) report the mirrored class name; instance?
;; and str rendering are registered at the bottom of the file.

(defn- tt [tag class]
  (doto (jolt.host/tagged-table tag)
    (jolt.host/ref-put! :class class)))

;; -- addresses ------------------------------------------------------------------
;; An InetAddress is its bytes — 4 for an Inet4Address, 16 for an Inet6Address —
;; with the host name it was looked up by ("" until one is known) and, for v6,
;; its scope: the zone number (:scope-id, nil when none was given) and the
;; interface it was named by (:scope-ifname). :address is getHostAddress, the
;; JDK's text: a dotted quad, or eight uncompressed lowercase hex groups
;; ("0:0:0:0:0:0:0:1", never "::1") followed by %interface or %number.

(defn- v4-text [bs] (str/join "." bs))

(defn- v6-text [bs]
  (str/join ":" (map (fn [i] (Integer/toHexString (+ (* 256 (nth bs i)) (nth bs (inc i)))))
                     (range 0 16 2))))

(defn- make-inet
  ([host bs] (make-inet host bs nil nil))
  ([host bs scope-id ifname]
   (let [bs (vec bs) v6? (= 16 (count bs))]
     (doto (tt :inet-address (if v6? "java.net.Inet6Address" "java.net.Inet4Address"))
       (jolt.host/ref-put! :host host)
       (jolt.host/ref-put! :bytes bs)
       (jolt.host/ref-put! :scope-id scope-id)
       (jolt.host/ref-put! :scope-ifname ifname)
       (jolt.host/ref-put! :address
                           (if v6?
                             (str (v6-text bs) (cond ifname (str "%" ifname)
                                                     scope-id (str "%" scope-id)
                                                     :else ""))
                             (v4-text bs)))))))

(defn- inet? [x] (= :inet-address (jolt.host/ref-get x :jolt/type)))
(defn- inet-bytes [a] (jolt.host/ref-get a :bytes))
(defn- inet6? [a] (= 16 (count (inet-bytes a))))

;; InetAddress.anyLocalAddress — what an unbound socket reports and a wildcard
;; bind means. With java.net.preferIPv6Addresses false, the JDK's default, it is
;; the Inet4Address 0.0.0.0, named "0.0.0.0".
(defn- any-local [] (make-inet "0.0.0.0" [0 0 0 0]))
(defn- loopback [] (make-inet "localhost" [127 0 0 1]))

;; -- literals (the JDK's IPAddressUtil) --
(defn- parse-v4
  "The four bytes of an IPv4 literal, or nil: decimal, one to four parts, the
  last filling the bytes the others leave (127.1 is 127.0.0.1, 2130706433 is
  too), at most 15 characters — textToNumericFormatV4."
  [s]
  (when (<= 1 (count s) 15)
    (let [parts (str/split s #"\." -1)
          n (count parts)]
      (when (and (<= n 4) (every? (fn [p] (re-matches #"\d+" p)) parts))
        (let [vs (mapv (fn [p] (Long/parseLong p)) parts)
              lead (pop vs)
              lst (peek vs)
              room (- 5 n)]
          (when (and (every? (fn [v] (<= v 255)) lead)
                     (< lst (bit-shift-left 1 (* 8 room))))
            (into lead (map (fn [i] (bit-and (bit-shift-right lst (* 8 i)) 0xff))
                            (range (dec room) -1 -1)))))))))

(defn- bsd-v4?
  "Whether s is an IPv4 literal only as inet_aton reads one — a hex or octal
  part — which the JDK refuses as ambiguous (jdk.net.allowAmbiguousIPAddressLiterals
  is false by default) rather than resolve."
  [s]
  (let [parts (str/split s #"\." -1)
        n (count parts)
        field (fn [p]
                (cond
                  (re-matches #"0[xX][0-9a-fA-F]*" p)
                  (if (= 2 (count p)) 0 (reduce (fn [a c] (+ (* a 16) (Character/digit c 16))) 0 (subs p 2)))
                  (re-matches #"0[0-7]*" p) (reduce (fn [a c] (+ (* a 8) (Character/digit c 8))) 0 p)
                  (re-matches #"[1-9]\d*" p) (reduce (fn [a c] (+ (* a 10) (Character/digit c 10))) 0 p)))]
    (and (<= 1 n 4) (<= (count s) 64)
         (let [vs (map field parts)]
           (and (every? some? vs)
                (every? (fn [v] (<= v 255)) (butlast vs))
                (< (last vs) (bit-shift-left 1 (* 8 (- 5 n)))))))))

(defn- hex-group
  "One IPv6 group's value. Any number of hex digits, as the JDK takes them, so
  long as the value fits in 16 bits."
  [g]
  (when (seq g)
    (reduce (fn [acc c]
              (let [d (Character/digit c 16)]
                (if (neg? d)
                  (reduced nil)
                  (let [v (+ (* acc 16) d)]
                    (if (> v 0xffff) (reduced nil) v)))))
            0 g)))

(defn- v6-part
  "The bytes of one side of a literal's ::, or nil. TAIL? is the side that ends
  the literal, whose last group may be an embedded dotted quad."
  [part tail?]
  (if (= "" part)
    []
    (let [gs (str/split part #":" -1) n (count gs)]
      (loop [i 0 out []]
        (if (= i n)
          out
          (let [g (nth gs i)]
            (if (and tail? (= i (dec n)) (str/includes? g "."))
              (when-let [q (when (= 3 (count (filter #{\.} g))) (parse-v4 g))] (into out q))
              (when-let [v (hex-group g)]
                (recur (inc i) (conj out (bit-shift-right v 8) (bit-and v 0xff)))))))))))

(defn- parse-v6
  "The 16 bytes of an IPv6 literal without brackets or zone, or nil."
  [s]
  (let [d (str/index-of s "::")]
    (if (nil? d)
      (let [bs (v6-part s true)] (when (= 16 (count bs)) bs))
      (when-not (str/index-of s "::" (inc d))
        (let [hd (v6-part (subs s 0 d) false)
              tl (v6-part (subs s (+ d 2)) true)]
          (when (and hd tl (< (+ (count hd) (count tl)) 16))
            (vec (concat hd (repeat (- 16 (count hd) (count tl)) 0) tl))))))))

(defn- unmap
  "An IPv4-mapped address (::ffff:a.b.c.d) as the IPv4 address it is — the JDK
  hands those back as Inet4Address wherever they come from."
  [bs]
  (if (and (= 16 (count bs)) (every? zero? (subvec bs 0 10))
           (= 0xff (nth bs 10)) (= 0xff (nth bs 11)))
    (subvec bs 12)
    bs))

(defn- invalid-v6! [host]
  (throw (java.net.UnknownHostException. (str host ": invalid IPv6 address literal"))))

(defn- literal-inet
  "host as an address literal, nameless, or nil when it is a name to resolve.
  As InetAddress.getAllByName reads it: brackets or a colon make it an IPv6
  literal, which must then parse; a zone after % is a number or an interface
  name, which must exist."
  [host]
  (let [bracketed? (str/starts-with? host "[")
        h (if bracketed?
            (if (and (> (count host) 2) (str/ends-with? host "]"))
              (subs host 1 (dec (count host)))
              (invalid-v6! host))
            host)]
    (if (or bracketed? (str/includes? h ":"))
      (let [pct  (str/index-of h "%")
            zone (when pct (subs h (inc pct)))
            bs   (when-not (= "" zone) (parse-v6 (if pct (subs h 0 pct) h)))]
        (when-not bs (invalid-v6! host))
        (let [bs (unmap bs)]
          (cond
            (or (nil? zone) (= 4 (count bs))) (make-inet "" bs)
            (re-matches #"\d{1,9}" zone) (make-inet "" bs (Integer/parseInt zone) nil)
            :else (if-let [i (native/interface-index zone)]
                    (make-inet "" bs i zone)
                    (throw (java.net.UnknownHostException. (str "no such interface " zone)))))))
      (if-let [bs (parse-v4 h)]
        (make-inet "" bs)
        (when (bsd-v4? h) (throw (java.net.UnknownHostException. h)))))))

;; -- resolution --
(defn- entry->inet
  "The InetAddress for a native address map {:ip :scope-id}, named host."
  [host {:keys [ip scope-id]}]
  (let [bs (unmap (if (str/includes? ip ":") (parse-v6 ip) (parse-v4 ip)))]
    (make-inet host bs (when (and (= 16 (count bs)) scope-id (pos? scope-id)) scope-id) nil)))

(defn- all-inets
  "Every InetAddress host names, IPv4 before IPv6 as the JDK orders them while
  java.net.preferIPv6Addresses is false, its default. A literal is itself, with
  no name; nil or \"\" is the loopback. Throws UnknownHostException."
  [host]
  (let [h (when (some? host) (str host))]
    (if (or (nil? h) (= "" h))
      [(loopback)]
      (if-let [lit (literal-inet h)]
        [lit]
        (let [{:keys [addrs message]} (native/resolve-addrs h 0)
              _ (native/free-addrs! addrs)
              inets (mapv (fn [e] (entry->inet h e)) addrs)]
          (when (empty? inets)
            (throw (java.net.UnknownHostException. (if message (str h ": " message) h))))
          (into (filterv (fn [a] (not (inet6? a))) inets) (filter inet6? inets)))))))

(defn- inet-by-name [host] (first (all-inets host)))

(defn- host-arg->inet
  "An InetAddress argument as it is, anything else resolved as a host name."
  [h]
  (if (inet? h) h (inet-by-name h)))

;; -- sockaddr --
(defn- inet-family [a] (if (inet6? a) native/af-inet6 native/af-inet))

(defn- inet-sockaddr
  "[sa len] for a:port, scope included. The caller frees sa."
  [a port]
  (let [bs (inet-bytes a)]
    (native/make-sockaddr (inet-family a) (if (inet6? a) (v6-text bs) (v4-text bs))
                          port (or (jolt.host/ref-get a :scope-id) 0))))

(defn- sockaddr->inet [sa]
  (entry->inet "" {:ip (native/sockaddr-ip sa) :scope-id (native/sockaddr-scope-id sa)}))

(defn- local-inet
  "The address fd is bound to, nameless, or nil when getsockname fails."
  [fd]
  (when-let [m (native/local-address fd)] (entry->inet "" m)))

(defn- local-port [fd]
  (max 0 (native/local-port fd)))

(defn- guard-fd! [fd]
  ;; accepted fds don't reliably inherit socket options — close-on-exec and
  ;; SO_NOSIGPIPE go on every fd we hand out (native/guard-accepted!), and
  ;; non-blocking mode so the R8 readiness interception can park a fiber instead
  ;; of pinning its carrier. That is O_NONBLOCK on POSIX and FIONBIO on Windows,
  ;; where the WSAPoll backend does the waiting.
  (native/guard-accepted! fd)
  (poller/nonblock! fd))

(defn- new-fd! [family]
  (let [fd (native/new-socket family)]
    ;; SO_REUSEADDR on POSIX, as the JDK sets it; nothing on Windows, where the
    ;; option means sharing a live listener and the JDK binds exclusively, so a
    ;; busy port is a BindException there (native/set-listener-reuse!)
    (native/set-listener-reuse! fd)
    ;; every v6 socket is dual-stack, as the JDK's are
    (when (= family native/af-inet6) (native/set-dual-stack! fd))
    (poller/nonblock! fd)
    fd))

;; -- fd lifetime ---------------------------------------------------------------
;; A socket's fd is live while any operation is using it. Closing marks the
;; socket closed, which stops new operations, and wakes the ones blocked on it
;; (poller/cancel!); the fd itself is closed by whichever of the closer or the
;; last operation out comes second. That is the JDK's shape, and the reason is
;; the same: a closed fd number is handed to the next socket or pipe the process
;; opens, so an operation that retried on it would read that socket's bytes or
;; write to its peer (jolt#1183, jolt-hmnr). process.ss counts its pipe fds the
;; same way. Windows included: its sockets wait on the WSAPoll backend, which
;; cancel! wakes like the others.
(defn- fd-release! [fd]
  (poller/forget! fd)
  (native/c-close fd))

;; Under the owner's lock: claims the release, answering the fd to close, when the
;; socket is closed and nothing is using it.
(defn- claim-release! [owner]
  (when (and (jolt.host/ref-get owner :closed?)
             (zero? (or (jolt.host/ref-get owner :ops) 0))
             (not (jolt.host/ref-get owner :released?)))
    (jolt.host/ref-put! owner :released? true)
    (jolt.host/ref-get owner :fd)))

(defn- op-enter! [owner]
  (locking owner
    (when (jolt.host/ref-get owner :closed?)
      (throw (java.net.SocketException. "Socket closed")))
    (jolt.host/ref-put! owner :ops (inc (or (jolt.host/ref-get owner :ops) 0)))))

(defn- op-leave! [owner]
  (when-let [fd (locking owner
                  (jolt.host/ref-put! owner :ops (dec (jolt.host/ref-get owner :ops)))
                  (claim-release! owner))]
    (fd-release! fd)))

(defmacro ^:private with-op [owner & body]
  `(let [o# ~owner]
     (op-enter! o#)
     (try ~@body (finally (op-leave! o#)))))

(declare drop-recv-buf!)

(defn- close-owner! [owner]
  (let [[first? fd] (locking owner
                      (if (jolt.host/ref-get owner :closed?)
                        [false nil]
                        (do (jolt.host/ref-put! owner :closed? true)
                            [true (claim-release! owner)])))]
    (when first? (drop-recv-buf! owner))
    (cond
      fd (fd-release! fd)
      first? (poller/cancel! (jolt.host/ref-get owner :fd))))
  nil)

;; An operation woken by close raises what the JVM's does: a read, accept or
;; connect "Socket closed", a write cut off mid-send "Broken pipe".
(defn- raise-if-closed! [owner msg]
  (when (jolt.host/ref-get owner :closed?)
    (throw (java.net.SocketException. msg))))

(defn- connect-fd! [owner fd inet port deadline]
  ;; connect fd to the InetAddress inet; frees the sockaddr either way.
  ;; The fd is O_NONBLOCK (fibers R8), so connect answers EINPROGRESS; wait for
  ;; writability (parking on a fiber, blocking kevent on a thread — the same
  ;; dispatch every other IO path uses), then read SO_ERROR for the verdict.
  ;; DEADLINE (poller/deadline-ms, or nil for none) bounds that wait: connect(endpoint,
  ;; timeout). Windows answers WSAEWOULDBLOCK where POSIX says EINPROGRESS, and
  ;; native/connect-pending? knows both.
  (let [[sa len] (inet-sockaddr inet port)
        ;; 0 when connected, else the failure's errno (-1 when there is none)
        e  (try
             (loop []
               (let [[r e] (native/c-connect fd sa len)]
                 (cond
                   (zero? r) 0
                   (native/connect-pending? e)
                   (let [t (poller/wait-ready fd :write deadline)]
                     (raise-if-closed! owner "Socket closed")
                     (when (= t :timeout)
                       (throw (java.net.SocketTimeoutException. "Connect timed out")))
                     (let [e (native/pending-error fd)]
                       (cond (zero? e) 0
                             (native/connect-pending? e) (recur)
                             :else e)))
                   :else (if (pos? e) e -1))))
             (finally (ffi/free sa)))]
    (cond
      (zero? e) nil
      (= e native/econnrefused) (throw (java.net.ConnectException. "Connection refused"))
      :else (throw (java.io.IOException.
                    (str "connect failed: "
                         (let [h (jolt.host/ref-get inet :host)]
                           (if (str/blank? h) (jolt.host/ref-get inet :address) h))
                         ":" port))))))

;; -- Socket ------------------------------------------------------------------
(declare make-isa inet-address->str)

;; The peer's InetAddress: the one connected to, named as it was looked up —
;; none for a literal or an accepted peer
(defn- peer-address [self] (jolt.host/ref-get self :remote-inet))

(defn- mark-connected! [self fd inet port]
  (jolt.host/ref-put! self :connected? true)
  (jolt.host/ref-put! self :remote-inet inet)
  (jolt.host/ref-put! self :port port)
  (jolt.host/ref-put! self :local-port (local-port fd))
  ;; read now: once the socket closes, its fd number may be another socket's
  (jolt.host/ref-put! self :local-inet (local-inet fd)))

;; The JDK picks a socket's family when it connects or binds, from the address;
;; an unconnected Socket or unbound ServerSocket starts with an IPv4 fd here and
;; trades it for one of the address's family when that is different. Nothing
;; has been set on the fd that the swap would lose: socket options this class
;; has are kept on the object and applied per call.
(defn- ensure-family! [self family]
  (when-not (= family (jolt.host/ref-get self :family))
    (let [nfd (new-fd! family)
          old (locking self
                (when-not (jolt.host/ref-get self :closed?)
                  (let [o (jolt.host/ref-get self :fd)]
                    (jolt.host/ref-put! self :fd nfd)
                    (jolt.host/ref-put! self :family family)
                    o)))]
      (if old
        (fd-release! old)
        (do (native/c-close nfd)
            (throw (java.net.SocketException. "Socket is closed")))))))

(defn- socket-ctor [& args]
  (let [inst (tt :socket "java.net.Socket")]
    (jolt.host/ref-put! inst :closed? false)
    (jolt.host/ref-put! inst :connected? false)
    (if (= 2 (count args))
      (let [inet (host-arg->inet (first args))
            p    (int (second args))
            fam  (inet-family inet)
            fd   (new-fd! fam)]
        (jolt.host/ref-put! inst :fd fd)
        (jolt.host/ref-put! inst :family fam)
        (try (connect-fd! inst fd inet p nil)
             (catch java.io.IOException e (fd-release! fd) (throw e)))
        (mark-connected! inst fd inet p))
      (do (jolt.host/ref-put! inst :fd (new-fd! native/af-inet))
          (jolt.host/ref-put! inst :family native/af-inet)))
    inst))

(defn- socket-close! [self] (close-owner! self))

(defn- ensure-socket-open! [self]
  (when (jolt.host/ref-get self :closed?)
    (throw (java.net.SocketException. "Socket is closed"))))

;; getInputStream, getOutputStream and the two shutdowns refuse a socket that is
;; not connected, in the JDK's order: closed first, then unconnected.
(defn- ensure-socket-connected! [self]
  (ensure-socket-open! self)
  (when-not (jolt.host/ref-get self :connected?)
    (throw (java.net.SocketException. "Socket is not connected"))))

;; shutdownInput / shutdownOutput (jolt-lang/jolt#1208). The flag is the JDK's
;; isInputShutdown / isOutputShutdown: set by a call that succeeds, kept after
;; close, and asked by the streams, since what the JDK answers after a half-close
;; is decided by the flag rather than by the kernel — a read after shutdownInput
;; is EOF even over data that had already arrived, which Linux would still hand
;; back. A write after shutdownOutput reaches send and fails there with EPIPE, as
;; the JDK's does. The shutdown also wakes a read parked on the poller, which
;; then sees the flag.
(defn- socket-shutdown! [self how flag already]
  (ensure-socket-connected! self)
  (when (jolt.host/ref-get self flag)
    (throw (java.net.SocketException. already)))
  ;; the flag goes up before the call: the shutdown wakes a parked read, which
  ;; must find it set rather than recv data SHUT_RD left queued
  (jolt.host/ref-put! self flag true)
  (with-op self
    (let [[r e] (native/c-shutdown (jolt.host/ref-get self :fd) how)]
      ;; ENOTCONN is not an error here, as it is not in the JDK's Net.shutdown:
      ;; macOS answers it once both directions have seen a FIN, and the socket
      ;; is then as shut down as the caller asked
      (when (and (neg? r) (not= e native/enotconn))
        (jolt.host/ref-put! self flag false)
        (throw (java.net.SocketException.
                 (native/error-message e))))))
  nil)

(defn- socket-connect! [self endpoint timeout]
  ;; The JDK's order: the timeout is validated before the socket's state, and
  ;; timeout 0 is the untimed connect the 1-arity is.
  (when (neg? timeout)
    (throw (IllegalArgumentException. "connect: timeout can't be negative")))
  (ensure-socket-open! self)
  (when (jolt.host/ref-get self :connected?)
    (throw (java.net.SocketException. "Already connected")))
  ;; A connect that fails closes the socket, as the JDK's does — a refused or
  ;; timed-out socket is not left half-connected for the caller to reuse.
  (try
    (with-op self
      (when-not (= :inet-socket-address (jolt.host/ref-get endpoint :jolt/type))
        (throw (IllegalArgumentException. "Unsupported address type")))
      (when (nil? (jolt.host/ref-get endpoint :addr))
        (throw (java.net.UnknownHostException. (str (jolt.host/ref-get endpoint :host)))))
      (let [inet (jolt.host/ref-get endpoint :addr)
            p    (jolt.host/ref-get endpoint :port)
            _    (ensure-family! self (inet-family inet))
            fd   (jolt.host/ref-get self :fd)]
        (connect-fd! self fd inet p (when (pos? timeout)
                                      (poller/deadline-ms timeout)))
        (mark-connected! self fd inet p)))
    (catch java.io.IOException e
      (close-owner! self)
      (throw e)))
  nil)

;; SO_TIMEOUT: milliseconds a read (Socket) or an accept (ServerSocket) may wait
;; before raising SocketTimeoutException; 0, the default, waits forever. It bounds
;; each call, not the connection, and a timeout leaves the socket usable. The
;; JDK validates it after the closed check; the two classes word the negative
;; case differently. Accepted sockets start at 0 — they do not inherit it.
(defn- so-timeout [self] (or (jolt.host/ref-get self :so-timeout) 0))

(defn- set-so-timeout! [self ms negative-msg]
  (when (jolt.host/ref-get self :closed?)
    (throw (java.net.SocketException. "Socket is closed")))
  (when (neg? ms) (throw (IllegalArgumentException. negative-msg)))
  (jolt.host/ref-put! self :so-timeout (int ms))
  nil)

(defn- get-so-timeout [self]
  (when (jolt.host/ref-get self :closed?)
    (throw (java.net.SocketException. "Socket is closed")))
  (so-timeout self))

(defn- socket->str [self]
  (if (jolt.host/ref-get self :connected?)
    (str "Socket[addr=" (inet-address->str (peer-address self))
         ",port=" (or (jolt.host/ref-get self :port) 0)
         ",localport=" (or (jolt.host/ref-get self :local-port) 0) "]")
    "Socket[unconnected]"))

;; Socket.getLocalAddress: the wildcard until connected, and again once closed,
;; when the JDK can no longer ask the socket
(defn- socket-local-address [self]
  (or (when (and (jolt.host/ref-get self :connected?)
                 (not (jolt.host/ref-get self :closed?)))
        (jolt.host/ref-get self :local-inet))
      (any-local)))

(def ^:private socket-methods
  {"connect"
   (fn
     ([self endpoint] (socket-connect! self endpoint 0))
     ([self endpoint timeout] (socket-connect! self endpoint (int timeout))))

   "setSoTimeout" (fn [self ms] (set-so-timeout! self ms "timeout can't be negative"))
   "getSoTimeout" get-so-timeout

   "getInputStream"
   (fn [self]
     (ensure-socket-connected! self)
     (when (jolt.host/ref-get self :in-shutdown?)
       (throw (java.net.SocketException. "Socket input is shutdown")))
     ;; :jolt/in-stream: a java.io.InputStream to clojure.java.io's coercions
     ;; (io-streams.ss user-in-stream?), so io/reader, slurp and io/copy take it
     (doto (tt :socket-input-stream "java.net.SocketInputStream")
       (jolt.host/ref-put! :jolt/in-stream true)
       (jolt.host/ref-put! :fd (jolt.host/ref-get self :fd))
       (jolt.host/ref-put! :socket self)))

   "getOutputStream"
   (fn [self]
     (ensure-socket-connected! self)
     (when (jolt.host/ref-get self :out-shutdown?)
       (throw (java.net.SocketException. "Socket output is shutdown")))
     (doto (tt :socket-output-stream "java.net.SocketOutputStream")
       (jolt.host/ref-put! :jolt/out-stream true)
       (jolt.host/ref-put! :fd (jolt.host/ref-get self :fd))
       (jolt.host/ref-put! :socket self)))

   "close"        socket-close!
   "shutdownInput"
   (fn [self] (socket-shutdown! self native/shut-rd :in-shutdown? "Socket input is already shutdown"))
   "shutdownOutput"
   (fn [self] (socket-shutdown! self native/shut-wr :out-shutdown? "Socket output is already shutdown"))
   "isInputShutdown"  (fn [self] (boolean (jolt.host/ref-get self :in-shutdown?)))
   "isOutputShutdown" (fn [self] (boolean (jolt.host/ref-get self :out-shutdown?)))
   "isConnected"  (fn [self] (boolean (jolt.host/ref-get self :connected?)))
   "isClosed"     (fn [self] (boolean (jolt.host/ref-get self :closed?)))
   "isBound"      (fn [self] (boolean (jolt.host/ref-get self :connected?)))
   ;; -1 until connected, as Java answers for an unbound socket. Never asked of
   ;; the fd: once closed its number may be another socket's.
   "getLocalPort" (fn [self] (or (jolt.host/ref-get self :local-port) -1))
   "getPort"      (fn [self] (or (jolt.host/ref-get self :port) 0))
   "toString"     socket->str

   "getInetAddress"
   (fn [self]
     (when (jolt.host/ref-get self :connected?) (peer-address self)))

   "getRemoteSocketAddress"
   (fn [self]
     (when (jolt.host/ref-get self :connected?)
       (make-isa nil (peer-address self) (jolt.host/ref-get self :port))))

   "getLocalAddress" socket-local-address

   "getLocalSocketAddress"
   (fn [self]
     (when (jolt.host/ref-get self :connected?)
       (make-isa nil (socket-local-address self)
                 (or (jolt.host/ref-get self :local-port) 0))))})

;; -- SocketInputStream -------------------------------------------------------
(defn- io-call
  ([owner op fd wait-kind] (io-call owner op fd wait-kind 0 nil))
  ([owner op fd wait-kind timeout-ms timeout-msg]
  ;; Run one blocking-capable syscall with the fd in O_NONBLOCK mode (fibers
  ;; R8). EAGAIN waits for readiness — parking the fiber on the poller when
  ;; there is a current fiber, blocking on a private kevent/epoll_wait when
  ;; there is not — and retries; EINTR retries immediately; anything else is
  ;; the syscall's real answer, returned as-is. OP hands back the captured
  ;; [result errno] pair from one foreign return path (jolt.ffi
  ;; :capture-native-error) — every binding io-call drives is declared that
  ;; way. The errno is spent on that classification and not returned, so a
  ;; caller sees a terminal failure only as a negative result.
  ;;
  ;; OWNER is the socket: the call counts as one of its operations, so a close
  ;; meanwhile wakes the wait and leaves the fd open until this call has left.
  ;; A socket found closed after a wait, or behind a failure, raises.
  ;;
  ;; A positive TIMEOUT-MS bounds the whole call (SO_TIMEOUT): the waits share
  ;; one deadline, and running out raises SocketTimeoutException with
  ;; TIMEOUT-MSG — never a -1 that a read would take for EOF. A close that lands
  ;; first still raises as a close.
  (with-op owner
    (let [closed-msg (if (= wait-kind :write) "Broken pipe" "Socket closed")
          deadline (when (pos? timeout-ms) (poller/deadline-ms timeout-ms))]
      (loop []
        (let [[r e] (op)]
          (cond
            (and (neg? r) (native/eintr? e)) (recur)
            (and (neg? r) (native/eagain? e))
            (let [t (poller/wait-ready fd wait-kind deadline)]
              (raise-if-closed! owner closed-msg)
              (when (= t :timeout)
                (throw (java.net.SocketTimeoutException. timeout-msg)))
              (recur))
            :else
            (do
              (when (neg? r) (raise-if-closed! owner closed-msg))
              ;; A negative return that is neither retryable nor a wait is where a
              ;; socket read turns into EOF (do-recv below), and the caller then
              ;; sees a closed connection with no reason attached. It is the one
              ;; place a syscall failure goes quiet, so say what it was when asked.
              (when (and (neg? r) (jolt.host/getenv "JOLT_DEBUG"))
                (binding [*out* *err*]
                  (println "jolt.socket: fd" fd wait-kind "syscall failed, errno" e
                           "- answered as EOF")))
              r))))))))

;; A read of a socket whose input is shut down is EOF, whatever the kernel still
;; holds (see socket-shutdown!).
(defn- input-shutdown? [stream]
  (jolt.host/ref-get (jolt.host/ref-get stream :socket) :in-shutdown?))

;; The receive buffer. recv writes into native memory, and allocating `len`
;; bytes of it per call made a read cost what was ASKED for: a 64 KB array read
;; for a 120-byte request allocated (zeroed) 64 KB every time. The socket keeps
;; one buffer instead, grown to the largest read up to RECV-MAX and reused.
;; A read takes it out of the socket and puts it back after, so two fibers
;; reading one stream at once never share one (the second allocates its own,
;; which is freed rather than kept). Close frees the kept buffer; a buffer put
;; back after close is freed instead. A read asking for more than RECV-MAX
;; receives at most RECV-MAX, a short read InputStream allows.
(def ^:private recv-max 65536)

(defn- take-recv-buf! [owner want]
  ;; answers [ptr size], size >= want
  (let [[p size] (locking owner
                   (let [p (jolt.host/ref-get owner :recv-buf)]
                     (jolt.host/ref-put! owner :recv-buf nil)
                     [p (jolt.host/ref-get owner :recv-size)]))]
    (if (and p (>= size want))
      [p size]
      (do (when p (ffi/free p))
          [(ffi/alloc want) want]))))

(defn- return-recv-buf! [owner p size]
  (when-not (locking owner
              (when-not (or (jolt.host/ref-get owner :closed?)
                            (jolt.host/ref-get owner :recv-buf))
                (jolt.host/ref-put! owner :recv-buf p)
                (jolt.host/ref-put! owner :recv-size size)
                true))
    (ffi/free p)))

(defn- drop-recv-buf! [owner]
  (when-let [p (locking owner
                 (let [p (jolt.host/ref-get owner :recv-buf)]
                   (jolt.host/ref-put! owner :recv-buf nil)
                   p))]
    (ffi/free p)))

(defn- do-recv [owner fd buf len]
  ;; n <= 0 answers EOF: recv 0 is orderly shutdown; a negative return (error)
  ;; also reads as EOF. Java throws SocketException there — documented
  ;; divergence. What is left of the error is a CHOICE, not a limitation:
  ;; io-call classifies a captured errno and returns only the syscall result,
  ;; so a reset arrives here as -1 with nothing attached. Retryable errnos are
  ;; already gone by this point — io-call loops on EINTR and waits out EAGAIN
  ;; — so every negative n here is terminal. Narrowing that to SocketException
  ;; on ECONNRESET means widening io-call's contract to hand the errno back.
  (let [n (if (jolt.host/ref-get owner :in-shutdown?)
            -1
            (io-call owner #(native/c-recv fd buf len 0) fd :read
                     (so-timeout owner) "Read timed out"))
        ;; a read parked when shutdownInput landed is woken by it, and may find
        ;; data that arrived meanwhile; the JDK answers EOF there too
        n (if (jolt.host/ref-get owner :in-shutdown?) -1 n)]
    (if (pos? n) n -1)))

;; A stream of a closed socket must not reach its fd: close frees the number,
;; and the next socket to open is handed it, so a read would take that socket's
;; bytes and a write would send to its peer (jolt#1183). The socket carries the
;; flag, so this is a KNOWN error and raises what Java raises — SocketException,
;; a subclass of IOException, so a catch of either sees it. A zero-length read
;; or write never reaches the fd and answers 0 / nil on a closed socket, as the
;; JVM's does, so callers guard after that shortcut.
(defn- ensure-open! [stream]
  (when (jolt.host/ref-get (jolt.host/ref-get stream :socket) :closed?)
    (throw (java.net.SocketException. "Socket closed"))))

;; InputStream.available — the same question the JVM asks, through the same
;; syscall: ioctl(fd, FIONREAD, &n) reports what has arrived without reading it
;; or waiting for more (native/available).
(defn- socket-available [self]
  ;; Closed raises rather than answering 0, where a recv error can only read as
  ;; EOF; asking the kernel would count some other socket's bytes. A shut-down
  ;; input has nothing to read, whatever arrived.
  (ensure-open! self)
  (if (input-shutdown? self)
    0
    (with-op (jolt.host/ref-get self :socket)
      ;; a failed ioctl reads as "nothing there", the way a failed recv reads
      ;; as EOF
      (max 0 (native/available (jolt.host/ref-get self :fd))))))

;; Objects.checkFromIndexSize, before anything else, as the JDK's socket streams
;; do: a negative len must not reach recv, where it is a huge size_t.
(defn- check-from-index-size! [b off len]
  (let [n (alength b)]
    (when (or (neg? off) (neg? len) (> off (- n len)))
      (throw (IndexOutOfBoundsException.
              (str "Range [" off ", " off " + " len ") out of bounds for length " n))))))

;; One read of up to len bytes into b at off: the count, or -1 at EOF. The
;; bytes are block-copied out of the receive buffer (ffi/read-into!).
(defn- recv-into! [self b off len]
  (ensure-open! self)
  (let [owner (jolt.host/ref-get self :socket)
        want  (min len recv-max)
        [buf size] (take-recv-buf! owner want)]
    (try
      (let [n (do-recv owner (jolt.host/ref-get self :fd) buf want)]
        (when (pos? n) (ffi/read-into! buf b off n))
        n)
      (finally (return-recv-buf! owner buf size)))))

(def ^:private socket-input-stream-methods
  {"read"
   (fn
     ([self]
      (ensure-open! self)
      (let [owner (jolt.host/ref-get self :socket)
            [buf size] (take-recv-buf! owner 1)]
        (try
          (let [n (do-recv owner (jolt.host/ref-get self :fd) buf 1)]
            (if (pos? n) (bit-and (ffi/read buf :uint8 0) 0xff) -1))
          (finally (return-recv-buf! owner buf size)))))
     ([self b]
      (let [len (alength b)]
        (if (zero? len) 0 (recv-into! self b 0 len))))
     ([self b off len]
      (check-from-index-size! b off len)
      (if (zero? len) 0 (recv-into! self b off len))))
   "available" (fn [self] (socket-available self))
   "close"     (fn [self] (socket-close! (jolt.host/ref-get self :socket)))})

;; -- SocketOutputStream ------------------------------------------------------
(defn- send-fully! [owner fd buf len]
  ;; loop over short sends; a non-positive return is a dead peer (EPIPE /
  ;; ECONNRESET) — throw like Java rather than silently dropping the rest.
  (loop [off 0]
    (when (< off len)
      (let [s (io-call owner #(native/c-send fd (+ buf off) (- len off) native/msg-nosignal) fd :write)]
        (when-not (pos? s)
          (throw (java.net.SocketException. "Broken pipe")))
        (recur (+ off s))))))

(defn- write-bytes! [self bytes off len]
  (check-from-index-size! bytes off len)
  (when (pos? len)
    (ensure-open! self)
    (let [fd (jolt.host/ref-get self :fd) buf (ffi/alloc len)]
      (try
        (ffi/write-array buf bytes off len)
        (send-fully! (jolt.host/ref-get self :socket) fd buf len)
        (finally (ffi/free buf))))))

(def ^:private socket-output-stream-methods
  {"write"
   (fn
     ;; OutputStream.write has TWO one-argument overloads on the JVM: write(int)
     ;; writes a byte, and write(byte[]) — the convenience method the abstract
     ;; class defines as write(b, 0, b.length) — writes the array. Dispatching on
     ;; the argument is the only way to tell them apart here, and without it a
     ;; byte array reached (int b) and reported "class [B cannot be cast to class
     ;; java.lang.Number", which named neither the socket nor the overload
     ;; (jolt#954). This is the same argument-shaped dispatch io-streams.ss's
     ;; out-stream/write does.
     ([self b]
      (if (bytes? b)
        (write-bytes! self b 0 (alength b))
        (let [_ (ensure-open! self)
              fd (jolt.host/ref-get self :fd)
              buf (ffi/alloc 1)]
          (try
            (ffi/write buf :uint8 (bit-and (int b) 0xff))
            (send-fully! (jolt.host/ref-get self :socket) fd buf 1)
            (finally (ffi/free buf))))))
     ([self bytes off len] (write-bytes! self bytes off len)))
   "flush" (fn [self] nil)
   "close" (fn [self] (socket-close! (jolt.host/ref-get self :socket)))})

;; -- ServerSocket ------------------------------------------------------------
;; A new fd of family bound to ip:port and listening, or throw having closed it.
(defn- open-listener! [family sa len port backlog]
  (let [fd (new-fd! family)]
    (try
      (when (neg? (first (native/c-bind fd sa len)))
        (throw (java.io.IOException. (str "bind failed on port " port))))
      (when (neg? (first (native/c-listen fd backlog)))
        (throw (java.io.IOException. "listen() failed")))
      fd
      (catch :default e
        (native/c-close fd)
        (throw e)))))

(defn- listen-on! [family ip port backlog]
  (let [[sa len] (native/make-sockaddr family ip port)]
    (try (open-listener! family sa len port backlog)
         (finally (ffi/free sa)))))

(defn- any-local? [inet] (every? zero? (inet-bytes inet)))

;; A listening fd for inet:port: one of inet's family, or for the IPv4 wildcard
;; — what ServerSocket(port) and a null or port-only bind address mean — a
;; dual-stack IPv6 socket on ::, as the JDK opens, so IPv6 clients reach it too.
;; A host without IPv6 gets the IPv4 wildcard instead. Answers [fd family].
(defn- bind-listen! [inet port backlog]
  (if (and (not (inet6? inet)) (any-local? inet))
    (if-let [fd (try (listen-on! native/af-inet6 "::" port backlog)
                     (catch java.io.IOException _ nil))]
      [fd native/af-inet6]
      [(listen-on! native/af-inet "0.0.0.0" port backlog) native/af-inet])
    (let [[sa len] (inet-sockaddr inet port)
          fam (inet-family inet)]
      (try [(open-listener! fam sa len port backlog) fam]
           (finally (ffi/free sa))))))

(defn- mark-bound! [self fd family inet port]
  (jolt.host/ref-put! self :fd fd)
  (jolt.host/ref-put! self :family family)
  (jolt.host/ref-put! self :bound? true)
  ;; the address as given, name and all: getInetAddress and toString answer it
  (jolt.host/ref-put! self :bind-inet inet)
  (jolt.host/ref-put! self :port (if (zero? port) (local-port fd) port)))

(defn- server-ctor [& args]
  ;; [] [port] [port backlog] [port backlog bindAddr]. The arg'd forms bind the
  ;; wildcard address unless bindAddr says otherwise, like Java, and port 0 asks
  ;; the kernel for an ephemeral port that getsockname recovers.
  ;;
  ;; The NO-ARG form makes an UNBOUND socket, which is what Java's does: nothing
  ;; is bound and nothing listens until bind is called. It used to bind an
  ;; ephemeral wildcard port right here, so (ServerSocket.) answered isBound true
  ;; and a real getLocalPort where the JVM answers false and -1, and it held a
  ;; port the caller never asked for.
  (if (zero? (count args))
    (doto (tt :server-socket "java.net.ServerSocket")
      (jolt.host/ref-put! :fd (new-fd! native/af-inet))
      (jolt.host/ref-put! :family native/af-inet)
      (jolt.host/ref-put! :closed? false)
      (jolt.host/ref-put! :bound? false))
    (let [port    (int (first args))
          backlog (if (>= (count args) 2) (int (second args)) 50)
          inet    (if (and (>= (count args) 3) (some? (nth args 2)))
                    (host-arg->inet (nth args 2))
                    (any-local))
          [fd fam] (bind-listen! inet port backlog)]
      (doto (tt :server-socket "java.net.ServerSocket")
        (jolt.host/ref-put! :closed? false)
        (mark-bound! fd fam inet port)))))

;; (.bind ss endpoint) / (.bind ss endpoint backlog), the two overloads
;; ServerSocket declares. Java's default backlog is 50, the same one the
;; [port backlog] ctor form defaults to.
;;
;; A failed bind leaves the socket open and unbound, as Java's does: the
;; listener is a fresh fd, and only a successful one replaces the socket's.
(defn- server-bind! [self endpoint backlog]
  (when (jolt.host/ref-get self :closed?)
    (throw (java.net.SocketException. "Socket is closed")))
  (when (jolt.host/ref-get self :bound?)
    (throw (java.net.SocketException. "Already bound")))
  (when (and endpoint (not= :inet-socket-address (jolt.host/ref-get endpoint :jolt/type)))
    (throw (IllegalArgumentException. "Unsupported address type")))
  (when (and endpoint (nil? (jolt.host/ref-get endpoint :addr)))
    (throw (java.net.SocketException. "Unresolved address")))
  (let [inet (if endpoint (jolt.host/ref-get endpoint :addr) (any-local))
        p    (int (if endpoint (jolt.host/ref-get endpoint :port) 0))
        [fd fam] (bind-listen! inet p backlog)
        old  (locking self
               (when-not (jolt.host/ref-get self :closed?)
                 (let [o (jolt.host/ref-get self :fd)]
                   (mark-bound! self fd fam inet p)
                   o)))]
    (if old
      (fd-release! old)
      (do (native/c-close fd)
          (throw (java.net.SocketException. "Socket is closed")))))
  nil)

(defn- server->str [self]
  (if (jolt.host/ref-get self :bound?)
    (str "ServerSocket[addr=" (inet-address->str (jolt.host/ref-get self :bind-inet))
         ",localport=" (or (jolt.host/ref-get self :port) 0) "]")
    "ServerSocket[unbound]"))

(def ^:private server-socket-methods
  {"accept"
   (fn [self]
     (when (jolt.host/ref-get self :closed?)
       (throw (java.net.SocketException. "Socket is closed")))
     ;; A no-arg socket has an fd but nothing is listening on it, so accept would
     ;; block or fail obscurely. Java names the case.
     (when-not (jolt.host/ref-get self :bound?)
       (throw (java.net.SocketException. "Socket is not bound yet")))
     (let [[sa lenp] (native/alloc-sockaddr)]
       (try
         (let [cfd (io-call self #(native/c-accept (jolt.host/ref-get self :fd) sa lenp)
                            (jolt.host/ref-get self :fd) :read
                            (so-timeout self) "Accept timed out")]
           (when (neg? cfd) (throw (java.io.IOException. "accept() failed")))
           (guard-fd! cfd)
           ;; a dual-stack listener's IPv4 peer arrives as ::ffff:a.b.c.d,
           ;; which sockaddr->inet reads back as the Inet4Address it is
           (doto (tt :socket "java.net.Socket")
             (jolt.host/ref-put! :fd cfd)
             (jolt.host/ref-put! :family (jolt.host/ref-get self :family))
             (jolt.host/ref-put! :closed? false)
             (mark-connected! cfd (sockaddr->inet sa) (native/sockaddr-port sa))))
         (finally (ffi/free sa) (ffi/free lenp)))))

   "close"
   (fn [self]
     (close-owner! self))

   "setSoTimeout" (fn [self ms] (set-so-timeout! self ms "timeout < 0"))
   "getSoTimeout" get-so-timeout

   "bind"
   (fn
     ([self endpoint] (server-bind! self endpoint 50))
     ([self endpoint backlog] (server-bind! self endpoint (int backlog))))

   "isClosed"     (fn [self] (boolean (jolt.host/ref-get self :closed?)))
   "getInetAddress"
   (fn [self] (when (jolt.host/ref-get self :bound?) (jolt.host/ref-get self :bind-inet)))
   "getLocalSocketAddress"
   (fn [self]
     (when (jolt.host/ref-get self :bound?)
       (make-isa nil (jolt.host/ref-get self :bind-inet) (jolt.host/ref-get self :port))))
   ;; Java's isBound asks "was this ever bound", not "is it usable now": it stays
   ;; true after close, and it is false on a fresh no-arg socket. Answering
   ;; (not closed?) had it backwards at both ends.
   "isBound"      (fn [self] (boolean (jolt.host/ref-get self :bound?)))
   ;; -1 until bound, as Java answers, and the port survives close.
   "getLocalPort" (fn [self]
                    (if (jolt.host/ref-get self :bound?)
                      (or (jolt.host/ref-get self :port) 0)
                      -1))
   "toString"     server->str})

;; -- host identity: local host + network interfaces ---------------------------

(defn- ifaddr-entries
  "One map per getifaddrs entry that carries an IPv4 or IPv6 address, with the
  InetAddress built (:inet). A v6 address is scoped to its interface, by name
  and index, as the JDK reports it (/0:0:0:0:0:0:0:1%lo0).

  Empty on Windows, which has no getifaddrs (native/interface-addresses).
  getLocalHost answers from gethostname plus the resolver, which is the primary
  path on every platform and the one the JDK's own Windows getLocalHost uses;
  NetworkInterface enumerates nothing there, recorded in
  test/conformance/known-divergences.edn (jolt-lang/jolt#1107)."
  [entries]
  (keep (fn [{:keys [name ip] :as e}]
          (when ip
            (let [bs (inet-bytes (entry->inet "" e))]
              (assoc e :inet
                     (if (= 16 (count bs))
                       (make-inet "" bs (or (native/interface-index name) (:scope-id e)) name)
                       (make-inet "" bs))))))
        entries))

;; -- InetAddress --------------------------------------------------------------
(defn- inet-address-ctor [& _] (loopback))

(defn- inet-address->str [self]
  (str (or (jolt.host/ref-get self :host) "")
       "/" (or (jolt.host/ref-get self :address) "")))

;; equals and hashCode go by the address alone: the host name it was looked up
;; by plays no part, and neither does a v6 scope. Inet4Address's hash is the four
;; octets as one int; Inet6AddressHolder's is the sum of the address read as four
;; big-endian ints of SIGNED bytes.
(defn- inet-address= [a b]
  (boolean (and (inet? b) (= (inet-bytes a) (inet-bytes b)))))

(defn- signed-byte [b] (if (> b 127) (- b 256) b))

(defn- inet-address-hash [self]
  (let [bs (inet-bytes self)]
    (if (= 4 (count bs))
      (unchecked-int (reduce (fn [acc o] (bit-or (bit-shift-left acc 8) o)) 0 bs))
      (reduce (fn [h k]
                (unchecked-int
                 (+ h (reduce (fn [c b]
                                (unchecked-int (+ (unchecked-int (bit-shift-left c 8))
                                                  (signed-byte b))))
                              0 (subvec bs k (+ k 4))))))
              0 [0 4 8 12]))))

(defn- inet-plain-text
  "The address with no zone: what the resolver's reverse lookup takes."
  [self]
  (let [bs (inet-bytes self)] (if (= 16 (count bs)) (v6-text bs) (v4-text bs))))

(defn- loopback-address? [self]
  (let [bs (inet-bytes self)]
    (if (= 4 (count bs))
      (= 127 (first bs))
      (= bs (conj (vec (repeat 15 0)) 1)))))

;; The address-class predicates, by the same prefixes the JDK tests.
(defn- inet-pred [v4 v6]
  (fn [self]
    (let [bs (inet-bytes self)]
      (boolean (if (= 4 (count bs)) (v4 bs) (v6 bs))))))

(def ^:private inet-address-methods
  {"getHostAddress" (fn [self] (jolt.host/ref-get self :address))
   ;; Resolves on first call and caches, as the JVM does — an address built
   ;; from a literal or read off an interface carries no name until asked.
   "getHostName"
   (fn [self]
     (let [h (jolt.host/ref-get self :host)]
       (if (str/blank? h)
         (let [nm (or (native/reverse-lookup (inet-plain-text self))
                      (jolt.host/ref-get self :address))]
           (jolt.host/ref-put! self :host nm)
           nm)
         h)))
   ;; The JVM reverse-resolves and caches; so does this, on the address it holds,
   ;; falling back to the name it was built with and then to the address itself.
   "getCanonicalHostName"
   (fn [self]
     (or (jolt.host/ref-get self :canonical)
         (let [nm (or (native/reverse-lookup (inet-plain-text self))
                      (let [h (jolt.host/ref-get self :host)] (when-not (str/blank? h) h))
                      (jolt.host/ref-get self :address))]
           (jolt.host/ref-put! self :canonical nm)
           nm)))
   ;; the address bytes, network order — the JVM's byte[] of 4 or 16
   "getAddress" (fn [self] (byte-array (map unchecked-byte (inet-bytes self))))
   "equals"   (fn [self other] (inet-address= self other))
   "hashCode" inet-address-hash
   "isLoopbackAddress" loopback-address?
   "isAnyLocalAddress" (inet-pred (fn [bs] (every? zero? bs)) (fn [bs] (every? zero? bs)))
   "isLinkLocalAddress"
   (inet-pred (fn [[a b]] (and (= a 169) (= b 254)))
              (fn [[a b]] (and (= a 0xfe) (= 0x80 (bit-and b 0xc0)))))
   "isSiteLocalAddress"
   (inet-pred (fn [[a b]] (or (= a 10) (and (= a 172) (= 16 (bit-and b 0xf0)))
                              (and (= a 192) (= b 168))))
              (fn [[a b]] (and (= a 0xfe) (= 0xc0 (bit-and b 0xc0)))))
   "isMulticastAddress"
   (inet-pred (fn [[a]] (= 0xe0 (bit-and a 0xf0))) (fn [[a]] (= a 0xff)))
   ;; Inet6Address only, as on the JVM, where Inet4Address has no such method
   "getScopeId" (fn [self] (or (jolt.host/ref-get self :scope-id) 0))
   "isIPv4CompatibleAddress"
   (fn [self] (boolean (and (inet6? self) (every? zero? (subvec (inet-bytes self) 0 12)))))
   "toString"       inet-address->str})

(defn- inet-by-address
  "InetAddress.getByAddress: 4 bytes or 16, a mapped v6 address being v4."
  [host addr]
  (let [bs (unmap (mapv (fn [b] (bit-and b 0xff)) addr))]
    (when-not (#{4 16} (count bs))
      (throw (java.net.UnknownHostException. "addr is of illegal length")))
    (make-inet (or host "") bs)))

(def ^:private inet-address-statics
  {"getByName" inet-by-name
   ;; an array, as on the JVM, so alength and aget hold on the result.
   "getAllByName" (fn [h] (object-array (all-inets h)))
   "getByAddress"
   (fn ([addr] (inet-by-address nil addr))
       ([host addr] (inet-by-address host addr)))
   "getLoopbackAddress" loopback
   ;; gethostname(2) plus whatever the resolver says that name is. A machine
   ;; whose own name does not resolve — a laptop off any DNS that knows it — is
   ;; answered from its own interfaces rather than by throwing
   ;; UnknownHostException, which is what the JVM does there.
   "getLocalHost"
   (fn []
     (let [nm (native/host-name)]
       (or (try (let [a (inet-by-name nm)] (jolt.host/ref-put! a :host nm) a)
                (catch java.io.IOException _ nil))
           (make-inet nm (or (some (fn [{:keys [inet]}]
                                     (when (and (not (inet6? inet)) (not (loopback-address? inet)))
                                       (inet-bytes inet)))
                                   (ifaddr-entries (native/interface-addresses)))
                             [127 0 0 1])))))})

;; -- InetSocketAddress -------------------------------------------------------
;; As the JDK's: (InetSocketAddress. host port) resolves host when it is built and
;; keeps the InetAddress (:addr), or none when the lookup failed; createUnresolved
;; never looks. :host is the host string as given, :port the port. getHostString
;; is the address's name when it has one, else its text, else the host as given.
;; A literal host carries no name until getHostName asks for one, so it prints
;; /127.0.0.1:80 — or /[0:0:0:0:0:0:0:1]:80, a v6 address bracketed — and an
;; unresolved one prints host/<unresolved>:80.
(defn- make-isa [host addr port]
  (doto (tt :inet-socket-address "java.net.InetSocketAddress")
    (jolt.host/ref-put! :host host)
    (jolt.host/ref-put! :addr addr)
    (jolt.host/ref-put! :port (int port))))

(defn- check-port! [p]
  (when-not (<= 0 p 65535)
    (throw (IllegalArgumentException. (str "port out of range:" p))))
  p)

(defn- inet-address-of
  "The InetAddress host names, or nil when it does not resolve."
  [host]
  (try (inet-by-name host)
       (catch java.io.IOException _ nil)))

(defn- isa-ctor [& args]
  (case (count args)
    ;; (InetSocketAddress. port): the wildcard address, like Java
    1 (make-isa "0.0.0.0" (any-local) (check-port! (int (first args))))
    (let [[h p] args
          p (check-port! (int p))]
      (cond
        (inet? h) (make-isa nil h p)
        (nil? h) (make-isa "0.0.0.0" (any-local) p)
        :else (make-isa (str h) (inet-address-of (str h)) p)))))

(defn- isa-unresolved [host port]
  (when (nil? host) (throw (IllegalArgumentException. "hostname can't be null")))
  (make-isa (str host) nil (check-port! (int port))))

(defn- isa-addr [self] (jolt.host/ref-get self :addr))

(defn- isa-host-string [self]
  (if-let [a (isa-addr self)]
    (let [h (jolt.host/ref-get a :host)]
      (if (str/blank? h) (jolt.host/ref-get a :address) h))
    (jolt.host/ref-get self :host)))

(defn- isa->str [self]
  (let [a (isa-addr self)]
    (str (cond
           (nil? a) (str (jolt.host/ref-get self :host) "/<unresolved>")
           (inet6? a) (str (jolt.host/ref-get a :host) "/[" (jolt.host/ref-get a :address) "]")
           :else (inet-address->str a))
         ":" (or (jolt.host/ref-get self :port) 0))))

;; InetSocketAddressHolder.equals/hashCode: by address and port when resolved,
;; by host name ignoring case when not, and a resolved one never equals an
;; unresolved one.
(defn- isa= [a b]
  (boolean
   (and (= :inet-socket-address (jolt.host/ref-get b :jolt/type))
        (= (jolt.host/ref-get a :port) (jolt.host/ref-get b :port))
        (let [ia (isa-addr a) ib (isa-addr b)]
          (if ia
            (and ib (inet-address= ia ib))
            (and (nil? ib)
                 (= (str/lower-case (str (jolt.host/ref-get a :host)))
                    (str/lower-case (str (jolt.host/ref-get b :host))))))))))

(defn- isa-hash [self]
  (unchecked-int
   (+ (if-let [a (isa-addr self)]
        (inet-address-hash a)
        (.hashCode (str/lower-case (str (jolt.host/ref-get self :host)))))
      (or (jolt.host/ref-get self :port) 0))))

(def ^:private inet-socket-address-methods
  {"getHostName"   (fn [self] (if-let [a (isa-addr self)]
                                ((get inet-address-methods "getHostName") a)
                                (jolt.host/ref-get self :host)))
   "getHostString" isa-host-string
   "getPort"       (fn [self] (or (jolt.host/ref-get self :port) 0))
   "isUnresolved"  (fn [self] (nil? (isa-addr self)))
   "equals"        (fn [self other] (isa= self other))
   "hashCode"      (fn [self] (isa-hash self))
   "getAddress"    (fn [self] (isa-addr self))
   "toString"      isa->str})

(def ^:private inet-socket-address-statics
  {"createUnresolved" isa-unresolved})

;; -- java.util.Enumeration ----------------------------------------------------
;; NetworkInterface hands back Enumerations, which enumeration-seq drives
;; through hasMoreElements/nextElement.

(defn- make-enumeration [coll]
  (doto (tt :enumeration "java.util.Enumeration")
    (jolt.host/ref-put! :rest (seq coll))))

(def ^:private enumeration-methods
  {"hasMoreElements" (fn [self] (boolean (jolt.host/ref-get self :rest)))
   "nextElement"     (fn [self]
                       (let [r (jolt.host/ref-get self :rest)]
                         (when-not r
                           (throw (ex-info "no more elements" {})))
                         (jolt.host/ref-put! self :rest (next r))
                         (first r)))
   "toString"        (fn [_] "java.util.Enumeration")})

;; -- java.net.NetworkInterface ------------------------------------------------
;; A snapshot, as on the JVM: the addresses and hardware address are read when
;; the interface is looked up, not on each call.

(defn- make-interface-address [inet prefix broadcast]
  (doto (tt :interface-address "java.net.InterfaceAddress")
    (jolt.host/ref-put! :inet inet)
    (jolt.host/ref-put! :prefix prefix)
    (jolt.host/ref-put! :broadcast broadcast)))

(defn- ia->str [self]
  (str (inet-address->str (jolt.host/ref-get self :inet))
       "/" (jolt.host/ref-get self :prefix)
       " [" (if-let [b (jolt.host/ref-get self :broadcast)] (inet-address->str b) "null") "]"))

(def ^:private interface-address-methods
  {"getAddress"             (fn [self] (jolt.host/ref-get self :inet))
   "getBroadcast"           (fn [self] (jolt.host/ref-get self :broadcast))
   "getNetworkPrefixLength" (fn [self] (short (jolt.host/ref-get self :prefix)))
   "toString"               ia->str})

(defn- make-network-interface [nm entries mac]
  (doto (tt :network-interface "java.net.NetworkInterface")
    (jolt.host/ref-put! :name nm)
    ;; No hostname: an address read off an interface is unresolved on the JVM
    ;; too (its toString is "/1.2.3.4"), and resolving every one of them
    ;; eagerly would put a DNS round trip per address in the way of enumerating
    ;; interfaces. .getHostName resolves on demand.
    (jolt.host/ref-put! :addresses (mapv :inet entries))
    (jolt.host/ref-put! :interface-addresses
                        (mapv (fn [{:keys [inet prefix broadcast]}]
                                (make-interface-address
                                 inet (or prefix 0)
                                 (when broadcast (make-inet "" (parse-v4 broadcast)))))
                              entries))
    (jolt.host/ref-put! :mac mac)))

(defn- network-interfaces []
  ;; One per name that carries an address, as the JDK lists them — an interface
  ;; with only a link-layer entry is not one it knows — and in its order: it
  ;; walks the IPv4 addresses and then the IPv6 ones, putting each new interface
  ;; at the front of the list and each address at the front of its interface's.
  (let [raw     (native/interface-addresses)
        entries (ifaddr-entries raw)
        [names by-name]
        (reduce (fn [[names by-name] e]
                  (let [nm (:name e)]
                    [(if (contains? by-name nm) names (cons nm names))
                     (update by-name nm (fn [es] (cons e es)))]))
                [() {}]
                (concat (remove (fn [e] (inet6? (:inet e))) entries)
                        (filter (fn [e] (inet6? (:inet e))) entries)))]
    (mapv (fn [nm]
            (make-network-interface
              nm
              (vec (by-name nm))
              (some (fn [e] (when (= nm (:name e)) (:mac e))) raw)))
          names)))

(defn- ni->str [self]
  (str "name:" (jolt.host/ref-get self :name)
       " (" (jolt.host/ref-get self :name) ")"))

(def ^:private network-interface-methods
  {"getName"        (fn [self] (jolt.host/ref-get self :name))
   ;; jolt has no separate friendly name for an interface, as Linux does not
   ;; either — the JVM reports the name for both there.
   "getDisplayName" (fn [self] (jolt.host/ref-get self :name))
   "getIndex"       (fn [self] (or (native/interface-index (jolt.host/ref-get self :name)) -1))
   "getInetAddresses" (fn [self] (make-enumeration (jolt.host/ref-get self :addresses)))
   "getInterfaceAddresses" (fn [self] (jolt.host/ref-get self :interface-addresses))
   "getHardwareAddress" (fn [self] (jolt.host/ref-get self :mac))
   "isLoopback"     (fn [self] (boolean (some loopback-address? (jolt.host/ref-get self :addresses))))
   "toString"       ni->str})

(def ^:private network-interface-statics
  {"getNetworkInterfaces" (fn [] (make-enumeration (network-interfaces)))
   "getByName" (fn [nm] (or (first (filter (fn [ni] (= (str nm) (jolt.host/ref-get ni :name)))
                                           (network-interfaces)))
                            nil))
   "getByInetAddress"
   (fn [addr]
     (let [want (host-arg->inet addr)]
       (or (first (filter (fn [ni]
                            (some (fn [a] (inet-address= want a))
                                  (jolt.host/ref-get ni :addresses)))
                          (network-interfaces)))
           nil)))})

;; -- value-semantics + registration -------------------------------------------

(def ^:private tag->classes
  {:socket               #{"Socket" "java.net.Socket"}
   :server-socket        #{"ServerSocket" "java.net.ServerSocket"}
   :socket-input-stream  #{"InputStream" "java.io.InputStream"}
   :socket-output-stream #{"OutputStream" "java.io.OutputStream"}
   :inet-socket-address  #{"InetSocketAddress" "java.net.InetSocketAddress"
                           "SocketAddress" "java.net.SocketAddress"}
   ;; and Inet4Address or Inet6Address by the object's own :class
   :inet-address         #{"InetAddress" "java.net.InetAddress"}
   :interface-address    #{"InterfaceAddress" "java.net.InterfaceAddress"}
   :network-interface    #{"NetworkInterface" "java.net.NetworkInterface"}
   :enumeration          #{"Enumeration" "java.util.Enumeration"}})

(def ^:private tag->render
  {:socket              socket->str
   :server-socket       server->str
   :inet-socket-address isa->str
   :inet-address        inet-address->str
   :interface-address   ia->str
   :network-interface   ni->str})

(def ^:private registered? (atom false))

(defn register-all! []
  (when (compare-and-set! registered? false true)
    (clojure.core/__register-class-methods! :socket socket-methods)
    (clojure.core/__register-class-methods! :socket-input-stream socket-input-stream-methods)
    (clojure.core/__register-class-methods! :socket-output-stream socket-output-stream-methods)
    (clojure.core/__register-class-methods! :server-socket server-socket-methods)
    (clojure.core/__register-class-methods! :inet-socket-address inet-socket-address-methods)
    (clojure.core/__register-class-methods! :inet-address inet-address-methods)
    (clojure.core/__register-class-methods! :network-interface network-interface-methods)
    (clojure.core/__register-class-methods! :interface-address interface-address-methods)
    (clojure.core/__register-class-methods! :enumeration enumeration-methods)

    (clojure.core/__register-class-ctor! "InetSocketAddress" isa-ctor)
    (clojure.core/__register-class-ctor! "java.net.InetSocketAddress" isa-ctor)
    (clojure.core/__register-class-statics! "InetSocketAddress" inet-socket-address-statics)
    (clojure.core/__register-class-statics! "java.net.InetSocketAddress" inet-socket-address-statics)

    (clojure.core/__register-class-ctor! "InetAddress" inet-address-ctor)
    (clojure.core/__register-class-ctor! "java.net.InetAddress" inet-address-ctor)
    (clojure.core/__register-class-statics! "InetAddress" inet-address-statics)
    (clojure.core/__register-class-statics! "java.net.InetAddress" inet-address-statics)

    (clojure.core/__register-class-statics! "NetworkInterface" network-interface-statics)
    (clojure.core/__register-class-statics! "java.net.NetworkInterface" network-interface-statics)

    (clojure.core/__register-class-ctor! "Socket" socket-ctor)
    (clojure.core/__register-class-ctor! "java.net.Socket" socket-ctor)

    (clojure.core/__register-class-ctor! "ServerSocket" server-ctor)
    (clojure.core/__register-class-ctor! "java.net.ServerSocket" server-ctor)

    ;; (instance? java.net.Socket s) etc.; only ever asserts true — anything
    ;; else defers to the next check and the built-ins.
    (clojure.core/__register-instance-check!
      (fn [cn val]
        (let [tag (jolt.host/ref-get val :jolt/type)
              cs  (tag->classes tag)]
          (when (and cs (or (contains? cs cn)
                            (and (= tag :inet-address)
                                 (let [c (jolt.host/ref-get val :class)]
                                   (or (= cn c) (= cn (subs c (count "java.net."))))))))
            true))))

    ;; = and hash on an InetAddress or InetSocketAddress are by value, as
    ;; their equals/hashCode are.
    (clojure.core/__register-eq!
      (fn [a b] (or (contains? #{:inet-address :inet-socket-address} (jolt.host/ref-get a :jolt/type))
                    (contains? #{:inet-address :inet-socket-address} (jolt.host/ref-get b :jolt/type))))
      (fn [a b]
        (let [ta (jolt.host/ref-get a :jolt/type)]
          (if (= ta :inet-address) (inet-address= a b) (isa= a b)))))
    (clojure.core/__register-hash!
      (fn [x] (contains? #{:inet-address :inet-socket-address} (jolt.host/ref-get x :jolt/type)))
      (fn [x]
        (if (= :inet-address (jolt.host/ref-get x :jolt/type))
          (inet-address-hash x)
          (isa-hash x))))

    ;; (str sock) renders through toString like Java; pred is two cheap lookups.
    (clojure.core/__register-str!
      (fn [x] (contains? tag->render (jolt.host/ref-get x :jolt/type)))
      (fn [x] ((tag->render (jolt.host/ref-get x :jolt/type)) x)))
    true))

(register-all!)
