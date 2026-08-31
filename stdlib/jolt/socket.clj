;; java.net.Socket / ServerSocket / InetSocketAddress for Jolt via jolt.ffi.
;; POSIX sockets + jolt.host/tagged-table + ref-put!/ref-get for state.
;;
;; Usage: (require 'jolt.socket)  ;; registers classes globally

(ns jolt.socket
  "POSIX socket support. Registers Socket, ServerSocket, InetSocketAddress,
  InetAddress and the socket stream classes with the host class registry.

  Deliberate divergences from the JVM (test/conformance/known-divergences.edn):
  a recv error reads as EOF (-1) rather than throwing, .connect ignores its
  timeout argument (always blocking), and toString formats are approximate.
  IPv4 only."
  (:require [jolt.ffi :as ffi]
            [jolt.io-poller :as poller]
            [clojure.string :as str]))

;; -- FFI --------------------------------------------------------------------
(ffi/load-library)
(def ^:private AF-INET 2)
(def ^:private SOCK-STREAM 1)

(ffi/defcfn c-socket      "socket"      [:int :int :int] :int)
(ffi/defcfn c-connect     "connect"     [:int :pointer :int] :int :blocking)
(ffi/defcfn c-bind        "bind"        [:int :pointer :int] :int)
(ffi/defcfn c-listen      "listen"      [:int :int] :int)
(ffi/defcfn c-accept      "accept"      [:int :pointer :pointer] :int :blocking)
(ffi/defcfn c-setsockopt  "setsockopt"  [:int :int :int :pointer :int] :int)
(ffi/defcfn c-getsockname "getsockname" [:int :pointer :pointer] :int)
(ffi/defcfn c-recv        "recv"        [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-send        "send"        [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-close       "close"       [:int] :int)
;; ioctl is (int fd, unsigned long request, ...) — the :varargs marker puts the
;; third argument where the callee's va_list reads it. Binding it fixed-arity
;; instead is what makes Apple arm64 return SUCCESS with the out-parameter
;; untouched, since variadic arguments travel on the stack there.
(ffi/defcfn c-ioctl       "ioctl"       [:int :ulong :varargs :pointer] :int)
(ffi/defcfn c-inet-addr   "inet_addr"   [:pointer] :uint)
(ffi/defcfn c-gethostbyname "gethostbyname" [:pointer] :pointer :blocking)
(ffi/defcfn c-gethostname  "gethostname"  [:pointer :size_t] :int)
(ffi/defcfn c-getifaddrs   "getifaddrs"   [:pointer] :int)
(ffi/defcfn c-freeifaddrs  "freeifaddrs"  [:pointer] :void)
(ffi/defcfn c-getnameinfo  "getnameinfo"
  [:pointer :uint :pointer :uint :pointer :uint :int] :int :blocking)

;; macOS carries BSD constants and a sin_len-led sockaddr; Linux has a 16-bit
;; sin_family and needs MSG_NOSIGNAL on send — without it a write to a
;; peer-closed socket raises SIGPIPE and kills the process (macOS suppresses
;; the signal per-fd via the SO_NOSIGPIPE socket option instead).
(def ^:private macos?
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))
(def ^:private sol-socket   (if macos? 0xffff 1))
(def ^:private so-reuse     (if macos? 4 2))
(def ^:private so-nosigpipe 0x1022)
(def ^:private msg-nosignal (if macos? 0 0x4000))
(def ^:private fionread (if macos? 0x4004667F 0x541B))

;; The link-layer address family getifaddrs reports a MAC under, and where the
;; MAC sits inside that entry's sockaddr. BSD's sockaddr_dl carries a
;; variable-length name before the address (data starts at 8, the name occupies
;; sdl_nlen of it); Linux's sockaddr_ll has a fixed 12-byte header.
(def ^:private af-link (if macos? 18 17))
;; A sockaddr's family byte: BSD leads with a one-byte sa_len, Linux with a
;; 16-bit sa_family.
(defn- sa-family [sa] (if macos? (ffi/read sa :uint8 1) (ffi/read sa :uint16 0)))

;; -- sockaddr helpers ---------------------------------------------------------

(defn- resolve-host [host]
  ;; sin_addr (network byte order) for a numeric IP or hostname (IPv4 only).
  ;; string->ptr NUL-terminates — a bare alloc+write-array leaves the
  ;; terminator to whatever malloc hands back.
  (let [hp (ffi/string->ptr (str host))]
    (try
      (let [addr (c-inet-addr hp)]
        (if (= addr 4294967295) ;; INADDR_NONE: not a numeric IP, try DNS
          (let [he (c-gethostbyname hp)]
            (when (ffi/null? he)
              (throw (java.io.IOException. (str "unknown host: " host))))
            (let [h-addr-list (ffi/read he :uptr 24)
                  h-addr (ffi/read h-addr-list :uptr 0)]
              (ffi/read h-addr :uint 0)))
          addr))
      (finally (ffi/free hp)))))

(defn- ip->str [ip]
  ;; ip is sin_addr in network byte order read back as a native (little-endian)
  ;; uint, so the low byte is the first octet.
  (str (bit-and ip 0xff) "."
       (bit-and (bit-shift-right ip 8) 0xff) "."
       (bit-and (bit-shift-right ip 16) 0xff) "."
       (bit-and (bit-shift-right ip 24) 0xff)))

(defn- make-sockaddr [ip port]
  ;; sockaddr_in (16 bytes): header(2) + sin_port(2, network order) +
  ;; sin_addr(4) + padding(8). BSD's header is sin_len + one-byte sin_family;
  ;; Linux's is a 16-bit little-endian sin_family. ffi/alloc zeroes the block,
  ;; so every byte this does not set is already 0 — which is what the padding
  ;; and the unset half of the header have to be.
  (let [sa (ffi/alloc 16)]
    (if macos?
      (do (ffi/write sa :uint8 16)          ;; sin_len
          (ffi/write sa :uint8 AF-INET 1))
      (ffi/write sa :uint8 AF-INET))
    (ffi/write sa :uint8 (bit-and (bit-shift-right port 8) 0xff) 2)
    (ffi/write sa :uint8 (bit-and port 0xff) 3)
    (ffi/write sa :uint ip 4)
    sa))

(defn- make-sockaddr-in [host port]
  (make-sockaddr (resolve-host host) port))

(defn- sa-port [sa]
  (bit-or (bit-shift-left (ffi/read sa :uint8 2) 8) (ffi/read sa :uint8 3)))
(defn- sa-addr [sa]
  (ip->str (ffi/read sa :uint 4)))

(defn- local-port [fd]
  (let [sa (ffi/alloc 16) lenp (ffi/alloc 4)]
    (try
      (ffi/write lenp :int 16)
      (if (neg? (c-getsockname fd sa lenp)) 0 (sa-port sa))
      (finally (ffi/free sa) (ffi/free lenp)))))

(defn- set-opt-1! [fd opt]
  (let [p (ffi/alloc 4)]
    (try
      (ffi/write p :int 1)
      (c-setsockopt fd sol-socket opt p 4)
      (finally (ffi/free p)))))

(defn- guard-fd! [fd]
  ;; accepted fds don't reliably inherit socket options — set SO_NOSIGPIPE
  ;; explicitly on every fd we hand out, and O_NONBLOCK so the R8 readiness
  ;; interception can park a fiber instead of pinning its carrier.
  (when macos? (set-opt-1! fd so-nosigpipe))
  (poller/nonblock! fd))

(defn- new-fd! []
  (let [fd (c-socket AF-INET SOCK-STREAM 0)]
    (when (neg? fd) (throw (java.io.IOException. "socket() failed")))
    (set-opt-1! fd so-reuse)
    (guard-fd! fd)
    fd))

;; -- tagged-table constructors ------------------------------------------------
;; a "class" entry makes (class x) report the mirrored class name; instance?
;; and str rendering are registered at the bottom of the file.

(defn- tt [tag class]
  (doto (jolt.host/tagged-table tag)
    (jolt.host/ref-put! :class class)))

(defn- make-inet-address [host addr]
  (doto (tt :inet-address "java.net.Inet4Address")
    (jolt.host/ref-put! :host host)
    (jolt.host/ref-put! :address addr)))

(defn- host-arg->str [h]
  ;; Socket(InetAddress, port) / ServerSocket(..., bindAddr) pass the
  ;; InetAddress table; take its literal before falling back to str.
  (if (= :inet-address (jolt.host/ref-get h :jolt/type))
    (str (or (jolt.host/ref-get h :address) (jolt.host/ref-get h :host)))
    (str h)))

(defn- connect-fd! [fd host port]
  ;; resolve + connect; frees the sockaddr either way. Returns the resolved ip.
  ;; The fd is O_NONBLOCK (fibers R8), so connect answers EINPROGRESS; wait for
  ;; writability (parking on a fiber, blocking kevent on a thread — the same
  ;; dispatch every other IO path uses), then read SO_ERROR for the verdict.
  (let [ip (resolve-host host)
        sa (make-sockaddr ip port)
        r  (loop []
             (let [r (c-connect fd sa 16)
                   ;; captured before anything else runs, for the reason io-call
                   ;; spells out above
                   e (if (zero? r) 0 (poller/errno))]
               (cond
                 (zero? r) 0
                 (poller/connect-pending? e)
                 (do (poller/wait-ready fd :write)
                     (let [e (poller/so-error fd)]
                       (if (zero? e)
                         0
                         (if (poller/connect-pending? e) (recur) -1))))
                 :else r)))]
    (ffi/free sa)
    (when (neg? r)
      (throw (java.io.IOException. (str "connect failed: " host ":" port))))
    ip))

;; -- Socket ------------------------------------------------------------------

(defn- socket-ctor [& args]
  (let [fd   (new-fd!)
        inst (tt :socket "java.net.Socket")]
    (jolt.host/ref-put! inst :fd fd)
    (jolt.host/ref-put! inst :closed? false)
    (jolt.host/ref-put! inst :connected? false)
    (when (= 2 (count args))
      (let [h  (host-arg->str (first args))
            p  (int (second args))
            ip (try (connect-fd! fd h p)
                    (catch java.io.IOException e (c-close fd) (throw e)))]
        (jolt.host/ref-put! inst :connected? true)
        (jolt.host/ref-put! inst :host h)
        (jolt.host/ref-put! inst :remote-addr (ip->str ip))
        (jolt.host/ref-put! inst :port p)
        (jolt.host/ref-put! inst :local-port (local-port fd))))
    inst))

(defn- socket-close! [self]
  (when-not (jolt.host/ref-get self :closed?)
    (jolt.host/ref-put! self :closed? true)
    (let [fd (jolt.host/ref-get self :fd)]
      (c-close fd)
      ;; close first, then forget: forget! wakes any reader still parked on the
      ;; fd (no event is coming — close removed it from the kernel set), and a
      ;; woken read must see EBADF, not EAGAIN-and-repark on a dying socket.
      (poller/forget! fd)))
  nil)

(defn- socket-connect! [self endpoint]
  (when (jolt.host/ref-get self :closed?)
    (throw (java.io.IOException. "Socket closed")))
  (when (jolt.host/ref-get self :connected?)
    (throw (java.io.IOException. "Already connected")))
  (let [h  (str (jolt.host/ref-get endpoint :host))
        p  (jolt.host/ref-get endpoint :port)
        fd (jolt.host/ref-get self :fd)
        ip (connect-fd! fd h p)]
    (jolt.host/ref-put! self :connected? true)
    (jolt.host/ref-put! self :host h)
    (jolt.host/ref-put! self :remote-addr (ip->str ip))
    (jolt.host/ref-put! self :port p)
    (jolt.host/ref-put! self :local-port (local-port fd)))
  nil)

(defn- socket->str [self]
  (if (jolt.host/ref-get self :connected?)
    (str "Socket[addr=" (or (jolt.host/ref-get self :host) "")
         "/" (or (jolt.host/ref-get self :remote-addr) "")
         ",port=" (or (jolt.host/ref-get self :port) 0)
         ",localport=" (or (jolt.host/ref-get self :local-port) 0) "]")
    "Socket[unconnected]"))

(def ^:private socket-methods
  {"connect"
   (fn
     ([self endpoint] (socket-connect! self endpoint))
     ;; Java's timeout is milliseconds-until-abort; this connect is always
     ;; blocking (equivalent to timeout 0). Divergence, documented in the ns.
     ([self endpoint _timeout] (socket-connect! self endpoint)))

   "getInputStream"
   (fn [self]
     (doto (tt :socket-input-stream "java.net.SocketInputStream")
       (jolt.host/ref-put! :fd (jolt.host/ref-get self :fd))
       (jolt.host/ref-put! :socket self)))

   "getOutputStream"
   (fn [self]
     (doto (tt :socket-output-stream "java.net.SocketOutputStream")
       (jolt.host/ref-put! :fd (jolt.host/ref-get self :fd))
       (jolt.host/ref-put! :socket self)))

   "close"        socket-close!
   "isConnected"  (fn [self] (boolean (jolt.host/ref-get self :connected?)))
   "isClosed"     (fn [self] (boolean (jolt.host/ref-get self :closed?)))
   "isBound"      (fn [self] (boolean (jolt.host/ref-get self :connected?)))
   "getLocalPort" (fn [self] (or (jolt.host/ref-get self :local-port)
                                 (local-port (jolt.host/ref-get self :fd))))
   "getPort"      (fn [self] (or (jolt.host/ref-get self :port) 0))
   "toString"     socket->str

   "getInetAddress"
   (fn [self]
     (make-inet-address (jolt.host/ref-get self :host)
                        (jolt.host/ref-get self :remote-addr)))

   "getRemoteSocketAddress"
   (fn [self]
     (doto (tt :inet-socket-address "java.net.InetSocketAddress")
       (jolt.host/ref-put! :host (or (jolt.host/ref-get self :remote-addr)
                                     (jolt.host/ref-get self :host)))
       (jolt.host/ref-put! :port (jolt.host/ref-get self :port))))})

;; -- SocketInputStream -------------------------------------------------------
(defn- io-call [op fd wait-kind]
  ;; Run one blocking-capable syscall with the fd in O_NONBLOCK mode (fibers
  ;; R8). EAGAIN waits for readiness — parking the fiber on the poller when
  ;; there is a current fiber, blocking on a private kevent/epoll_wait when
  ;; there is not — and retries; EINTR retries immediately; anything else is
  ;; the syscall's real answer, returned as-is (callers read errno semantics).
  (loop []
    (let [r (op)
          ;; ERRNO IS READ HERE AND NOWHERE ELSE. It survives only until the next
          ;; thing that can set it, and that includes reading it: the accessor is
          ;; a foreign call, and an allocation on the way can trip a collection
          ;; whose mmap leaves ENOMEM behind. Asking twice -- once for EINTR, once
          ;; for EAGAIN -- read recv's EAGAIN as ENOMEM often enough to matter: the
          ;; retry was missed, the -1 fell through, and a socket read answered EOF
          ;; on a live connection. The (neg? r) guard is a fixnum compare, which
          ;; allocates nothing and so cannot collect.
          e (if (neg? r) (poller/errno) 0)]
      (cond
        (and (neg? r) (poller/eintr? e)) (recur)
        (and (neg? r) (poller/eagain? e)) (do (poller/wait-ready fd wait-kind) (recur))
        :else
        (do
          ;; A negative return that is neither retryable nor a wait is where a
          ;; socket read turns into EOF (do-recv below), and the caller then sees
          ;; a closed connection with no reason attached. It is the one place a
          ;; syscall failure goes quiet, so say what it was when asked.
          (when (and (neg? r) (jolt.host/getenv "JOLT_DEBUG"))
            (binding [*out* *err*]
              (println "jolt.socket: fd" fd wait-kind "syscall failed, errno" e
                       "- answered as EOF")))
          r)))))

(defn- do-recv [fd buf len]
  ;; n <= 0 answers EOF: recv 0 is orderly shutdown; a negative return (error)
  ;; also reads as EOF because errno isn't reachable to tell ECONNRESET from
  ;; EINTR. Java throws SocketException there — documented divergence.
  (let [n (io-call #(c-recv fd buf len 0) fd :read)]
    (if (pos? n)
      {:n n :bytes (ffi/read-array buf n)}
      {:n -1 :bytes nil})))

;; InputStream.available — the same question the JVM asks, through the same
;; syscall: ioctl(fd, FIONREAD, &n) reports what has arrived without reading it
;; or waiting for more. The binding is what has to be right; see c-ioctl above.
(defn- socket-available [self]
  ;; Closed is an error on both, and here it is a KNOWN one — the socket carries
  ;; the flag — so it raises rather than answering, where a recv error can only
  ;; read as EOF. SocketException is the class Java raises and a subclass of
  ;; IOException, so a catch of either sees it. Asking the kernel is also not an
  ;; option once the fd is closed: the number is free to be reused by the next
  ;; socket, and the count would be somebody else's.
  (when (jolt.host/ref-get (jolt.host/ref-get self :socket) :closed?)
    (throw (java.net.SocketException. "Socket closed")))
  (let [fd (jolt.host/ref-get self :fd)
        out (ffi/alloc 4)]
    (try
      (ffi/write out :int 0)
      ;; a failed ioctl reads as "nothing there", the way a failed recv reads as
      ;; EOF — errno is not reachable to say more
      (if (neg? (c-ioctl fd fionread out)) 0 (max 0 (ffi/read out :int 0)))
      (finally (ffi/free out)))))

(def ^:private socket-input-stream-methods
  {"read"
   (fn
     ([self]
      (let [fd (jolt.host/ref-get self :fd) buf (ffi/alloc 1)]
        (try
          (let [{:keys [n]} (do-recv fd buf 1)]
            (if (pos? n) (bit-and (ffi/read buf :uint8 0) 0xff) -1))
          (finally (ffi/free buf)))))
     ([self b]
      (let [fd (jolt.host/ref-get self :fd) len (alength b)]
        (if (zero? len) 0
            (let [buf (ffi/alloc len)]
              (try
                (let [{:keys [n bytes]} (do-recv fd buf len)]
                  (if (pos? n) (do (dotimes [i n] (aset b i (nth bytes i))) n) -1))
                (finally (ffi/free buf)))))))
     ([self b off len]
      (let [fd (jolt.host/ref-get self :fd)]
        (if (zero? len) 0
            (let [buf (ffi/alloc len)]
              (try
                (let [{:keys [n bytes]} (do-recv fd buf len)]
                  (if (pos? n) (do (dotimes [i n] (aset b (+ off i) (nth bytes i))) n) -1))
                (finally (ffi/free buf))))))))
   "available" (fn [self] (socket-available self))
   "close"     (fn [self] (socket-close! (jolt.host/ref-get self :socket)))})

;; -- SocketOutputStream ------------------------------------------------------
(defn- send-fully! [fd buf len]
  ;; loop over short sends; a non-positive return is a dead peer (EPIPE /
  ;; ECONNRESET) — throw like Java rather than silently dropping the rest.
  (loop [off 0]
    (when (< off len)
      (let [s (io-call #(c-send fd (+ buf off) (- len off) msg-nosignal) fd :write)]
        (when-not (pos? s)
          (throw (java.io.IOException. "Broken pipe")))
        (recur (+ off s))))))

(def ^:private socket-output-stream-methods
  {"write"
   (fn
     ([self b]
      (let [fd (jolt.host/ref-get self :fd) buf (ffi/alloc 1)]
        (try
          (ffi/write buf :uint8 (bit-and (int b) 0xff))
          (send-fully! fd buf 1)
          (finally (ffi/free buf)))))
     ([self bytes off len]
      (when (pos? len)
        (let [fd (jolt.host/ref-get self :fd) buf (ffi/alloc len)]
          (try
            (dotimes [i len]
              (ffi/write buf :uint8 (bit-and (aget bytes (+ off i)) 0xff) i))
            (send-fully! fd buf len)
            (finally (ffi/free buf)))))))
   "flush" (fn [self] nil)
   "close" (fn [self] (socket-close! (jolt.host/ref-get self :socket)))})

;; -- ServerSocket ------------------------------------------------------------
(defn- server-ctor [& args]
  ;; [] [port] [port backlog] [port backlog bindAddr] — binds the wildcard
  ;; address unless bindAddr says otherwise, like Java. Port 0 asks the kernel
  ;; for an ephemeral port; getsockname recovers the real one.
  (let [port      (if (pos? (count args)) (int (first args)) 0)
        backlog   (if (>= (count args) 2) (int (second args)) 50)
        bind-host (if (>= (count args) 3) (host-arg->str (nth args 2)) "0.0.0.0")
        fd        (new-fd!)
        sa        (make-sockaddr-in bind-host port)]
    (when (neg? (c-bind fd sa 16))
      (c-close fd) (ffi/free sa)
      (throw (java.io.IOException. (str "bind failed on port " port))))
    (ffi/free sa)
    (when (neg? (c-listen fd backlog))
      (c-close fd)
      (throw (java.io.IOException. "listen() failed")))
    (doto (tt :server-socket "java.net.ServerSocket")
      (jolt.host/ref-put! :fd fd)
      (jolt.host/ref-put! :closed? false)
      (jolt.host/ref-put! :bind-addr bind-host)
      (jolt.host/ref-put! :port (if (zero? port) (local-port fd) port)))))

(defn- server->str [self]
  (let [ba (or (jolt.host/ref-get self :bind-addr) "0.0.0.0")]
    (str "ServerSocket[addr=" ba "/" ba
         ",localport=" (or (jolt.host/ref-get self :port) 0) "]")))

(def ^:private server-socket-methods
  {"accept"
   (fn [self]
     (when (jolt.host/ref-get self :closed?)
       (throw (java.io.IOException. "ServerSocket closed")))
     (let [sa (ffi/alloc 16) lenp (ffi/alloc 4)]
       (try
         (ffi/write lenp :int 16)
         (let [cfd (io-call #(c-accept (jolt.host/ref-get self :fd) sa lenp)
                            (jolt.host/ref-get self :fd) :read)]
           (when (neg? cfd) (throw (java.io.IOException. "accept() failed")))
           (guard-fd! cfd)
           (doto (tt :socket "java.net.Socket")
             (jolt.host/ref-put! :fd cfd)
             (jolt.host/ref-put! :closed? false)
             (jolt.host/ref-put! :connected? true)
             (jolt.host/ref-put! :host (sa-addr sa))
             (jolt.host/ref-put! :remote-addr (sa-addr sa))
             (jolt.host/ref-put! :port (sa-port sa))
             (jolt.host/ref-put! :local-port (local-port cfd))))
         (finally (ffi/free sa) (ffi/free lenp)))))

   "close"
   (fn [self]
     (when-not (jolt.host/ref-get self :closed?)
       (jolt.host/ref-put! self :closed? true)
       (let [fd (jolt.host/ref-get self :fd)]
         (c-close fd)
         (poller/forget! fd)))   ; see socket-close!
     nil)

   "isClosed"     (fn [self] (boolean (jolt.host/ref-get self :closed?)))
   "isBound"      (fn [self] (not (jolt.host/ref-get self :closed?)))
   "getLocalPort" (fn [self] (or (jolt.host/ref-get self :port) 0))
   "toString"     server->str})

;; -- InetSocketAddress -------------------------------------------------------
(defn- isa-ctor [& args]
  ;; (InetSocketAddress. port) is the wildcard address, like Java.
  (let [h (if (= 1 (count args)) "0.0.0.0" (host-arg->str (first args)))
        p (int (if (= 1 (count args)) (first args) (second args)))]
    (doto (tt :inet-socket-address "java.net.InetSocketAddress")
      (jolt.host/ref-put! :host h)
      (jolt.host/ref-put! :port p))))

(defn- isa->str [self]
  (str (or (jolt.host/ref-get self :host) "0.0.0.0")
       ":" (or (jolt.host/ref-get self :port) 0)))

(def ^:private inet-socket-address-methods
  {"getHostName"   (fn [self] (or (jolt.host/ref-get self :host) "0.0.0.0"))
   "getHostString" (fn [self] (or (jolt.host/ref-get self :host) "0.0.0.0"))
   "getPort"       (fn [self] (or (jolt.host/ref-get self :port) 0))
   "isUnresolved"  (fn [self] false)
   "getAddress"
   (fn [self]
     (let [h (or (jolt.host/ref-get self :host) "0.0.0.0")]
       (make-inet-address h (try (ip->str (resolve-host h))
                                 (catch java.io.IOException _ nil)))))
   "toString"      isa->str})

;; -- host identity: local host + network interfaces ---------------------------
;; getifaddrs(3) reports one entry PER ADDRESS, so an interface with an IPv4
;; address and a MAC appears twice; the entries are grouped by name below into
;; one NetworkInterface each, which is the shape java.net presents.
;;
;; struct ifaddrs is laid out the same on both platforms for the fields read
;; here: ifa_next 0, ifa_name 8, ifa_flags 16, ifa_addr 24.

(defn- mac-bytes
  "The hardware address inside a link-layer sockaddr, or nil when the entry
  carries none (a loopback or tunnel interface reports a zero-length address)."
  [sa]
  (let [[off len] (if macos?
                    ;; sockaddr_dl: sdl_nlen 5, sdl_alen 6, sdl_data 8 — the
                    ;; interface name occupies sdl_nlen bytes of data first.
                    [(+ 8 (ffi/read sa :uint8 5)) (ffi/read sa :uint8 6)]
                    ;; sockaddr_ll: sll_halen 11, sll_addr 12.
                    [12 (ffi/read sa :uint8 11)])]
    (when (pos? len)
      (let [bs (mapv (fn [i] (ffi/read sa :uint8 (+ off i))) (range len))]
        ;; an all-zero address is what an interface with no hardware reports.
        (when (some pos? bs) (byte-array bs))))))

(defn- ifaddr-entries
  "One map per getifaddrs entry: {:name :ip :mac}. The list is freed before
  returning, so everything needed is read out here."
  []
  (let [pp (ffi/alloc (ffi/sizeof :pointer))]
    (try
      (ffi/write pp :pointer ffi/null)
      (when (neg? (c-getifaddrs pp))
        (throw (java.io.IOException. "getifaddrs() failed")))
      (let [head (ffi/read pp :pointer)]
        (try
          (loop [cur head acc []]
            (if (ffi/null? cur)
              acc
              (let [nm (ffi/ptr->string (ffi/read cur :pointer 8))
                    sa (ffi/read cur :pointer 24)
                    fam (when-not (ffi/null? sa) (sa-family sa))]
                (recur (ffi/read cur :pointer 0)
                       (conj acc (cond-> {:name nm}
                                   (= fam AF-INET) (assoc :ip (sa-addr sa))
                                   (= fam af-link) (assoc :mac (mac-bytes sa))))))))
          (finally (c-freeifaddrs head))))
      (finally (ffi/free pp)))))

(defn- local-hostname []
  (let [n 256 buf (ffi/alloc n)]
    (try
      (ffi/write buf :uint8 0)
      (if (neg? (c-gethostname buf n)) "localhost" (ffi/ptr->string buf))
      (finally (ffi/free buf)))))

(defn- reverse-name
  "The name DNS gives back for an address, or nil. getnameinfo with no flags
  asks for the canonical name and falls back to the numeric form itself, so a
  result equal to the address means the lookup found nothing."
  [address]
  (let [sa (make-sockaddr (resolve-host address) 0)
        n 1025
        buf (ffi/alloc n)]
    (try
      (ffi/write buf :uint8 0)
      (when (zero? (c-getnameinfo sa 16 buf n ffi/null 0 0))
        (let [nm (ffi/ptr->string buf)]
          (when-not (or (str/blank? nm) (= nm address)) nm)))
      (catch java.io.IOException _ nil)
      (finally (ffi/free buf) (ffi/free sa)))))

;; -- InetAddress --------------------------------------------------------------
(defn- inet-address-ctor [& _]
  (make-inet-address "localhost" "127.0.0.1"))

(defn- inet-address->str [self]
  (str (or (jolt.host/ref-get self :host) "")
       "/" (or (jolt.host/ref-get self :address) "")))

(def ^:private inet-address-methods
  {"getHostAddress" (fn [self] (or (jolt.host/ref-get self :address) "127.0.0.1"))
   ;; Resolves on first call and caches, as the JVM does — an address built
   ;; from a literal or read off an interface carries no name until asked.
   "getHostName"
   (fn [self]
     (let [h (jolt.host/ref-get self :host)]
       (if (str/blank? h)
         (let [addr (jolt.host/ref-get self :address)
               nm (or (and addr (reverse-name addr)) addr)]
           (jolt.host/ref-put! self :host nm)
           nm)
         h)))
   ;; The JVM reverse-resolves and caches; so does this, on the address it holds,
   ;; falling back to the name it was built with and then to the address itself.
   "getCanonicalHostName"
   (fn [self]
     (or (jolt.host/ref-get self :canonical)
         (let [addr (jolt.host/ref-get self :address)
               nm (or (and addr (reverse-name addr))
                      (jolt.host/ref-get self :host)
                      addr)]
           (jolt.host/ref-put! self :canonical nm)
           nm)))
   ;; the four address octets, network order — the JVM's byte[].
   "getAddress"
   (fn [self]
     (byte-array (mapv (fn [o] (Integer/parseInt o))
                       (str/split (or (jolt.host/ref-get self :address) "127.0.0.1") #"\."))))
   "equals"   (fn [self other]
                (boolean (and (= :inet-address (jolt.host/ref-get other :jolt/type))
                              (= (jolt.host/ref-get self :address)
                                 (jolt.host/ref-get other :address)))))
   "hashCode" (fn [self] (hash (jolt.host/ref-get self :address)))
   "isLoopbackAddress" (fn [self] (= "127.0.0.1" (jolt.host/ref-get self :address)))
   "toString"       inet-address->str})

(defn- all-addresses-of
  "Every address the resolver has for `host`. gethostbyname's h_addr_list (a
  NULL-terminated array of pointers at offset 24 of struct hostent) carries them
  all; a numeric literal resolves to itself without a lookup."
  [host]
  (let [hp (ffi/string->ptr (str host))]
    (try
      (let [numeric (c-inet-addr hp)]
        (if (not= numeric 4294967295)
          [(ip->str numeric)]
          (let [he (c-gethostbyname hp)]
            (when (ffi/null? he)
              (throw (java.io.IOException. (str "unknown host: " host))))
            (let [list-ptr (ffi/read he :uptr 24)]
              (loop [i 0 acc []]
                (let [entry (ffi/read list-ptr :uptr (* i (ffi/sizeof :pointer)))]
                  (if (zero? entry)
                    acc
                    (recur (inc i) (conj acc (ip->str (ffi/read entry :uint 0)))))))))))
      (finally (ffi/free hp)))))

(def ^:private inet-address-statics
  {"getByName"
   (fn [h] (make-inet-address (str h) (ip->str (resolve-host h))))
   "getAllByName"
   ;; an array, as on the JVM, so alength and aget hold on the result.
   (fn [h] (object-array (mapv (fn [ip] (make-inet-address (str h) ip))
                               (all-addresses-of h))))
   "getLoopbackAddress"
   (fn [] (make-inet-address "localhost" "127.0.0.1"))
   ;; gethostname(2) plus whatever the resolver says that name is. A machine
   ;; whose own name does not resolve — a laptop off any DNS that knows it — is
   ;; answered from its own interfaces rather than by throwing
   ;; UnknownHostException, which is what the JVM does there.
   "getLocalHost"
   (fn []
     (let [nm (local-hostname)]
       (make-inet-address
         nm
         (or (try (ip->str (resolve-host nm)) (catch java.io.IOException _ nil))
             (first (remove (fn [ip] (= "127.0.0.1" ip))
                            (keep :ip (ifaddr-entries))))
             "127.0.0.1"))))})

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

(defn- make-network-interface [nm addresses mac]
  (doto (tt :network-interface "java.net.NetworkInterface")
    (jolt.host/ref-put! :name nm)
    (jolt.host/ref-put! :addresses addresses)
    (jolt.host/ref-put! :mac mac)))

(defn- network-interfaces []
  ;; Preserve the order getifaddrs reports, one interface per distinct name.
  (let [entries (ifaddr-entries)
        names (distinct (map :name entries))]
    (mapv (fn [nm]
            (let [mine (filter (fn [e] (= nm (:name e))) entries)]
              (make-network-interface
                nm
                ;; No hostname: an address read off an interface is unresolved
                ;; on the JVM too (its toString is "/1.2.3.4"), and resolving
                ;; every one of them eagerly would put a DNS round trip per
                ;; address in the way of enumerating interfaces. .getHostName
                ;; resolves on demand.
                (mapv (fn [e] (make-inet-address "" (:ip e))) (filter :ip mine))
                (first (keep :mac mine)))))
          names)))

(defn- ni->str [self]
  (str "name:" (jolt.host/ref-get self :name)
       " (" (jolt.host/ref-get self :name) ")"))

(def ^:private network-interface-methods
  {"getName"        (fn [self] (jolt.host/ref-get self :name))
   ;; jolt has no separate friendly name for an interface, as Linux does not
   ;; either — the JVM reports the name for both there.
   "getDisplayName" (fn [self] (jolt.host/ref-get self :name))
   "getInetAddresses" (fn [self] (make-enumeration (jolt.host/ref-get self :addresses)))
   "getHardwareAddress" (fn [self] (jolt.host/ref-get self :mac))
   "isLoopback"     (fn [self] (boolean (some (fn [a] (= "127.0.0.1" (jolt.host/ref-get a :address)))
                                              (jolt.host/ref-get self :addresses))))
   "toString"       ni->str})

(def ^:private network-interface-statics
  {"getNetworkInterfaces" (fn [] (make-enumeration (network-interfaces)))
   "getByName" (fn [nm] (or (first (filter (fn [ni] (= (str nm) (jolt.host/ref-get ni :name)))
                                           (network-interfaces)))
                            nil))
   "getByInetAddress"
   (fn [addr]
     (let [want (host-arg->str addr)]
       (or (first (filter (fn [ni]
                            (some (fn [a] (= want (jolt.host/ref-get a :address)))
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
   :inet-address         #{"InetAddress" "java.net.InetAddress"
                           "Inet4Address" "java.net.Inet4Address"}
   :network-interface    #{"NetworkInterface" "java.net.NetworkInterface"}
   :enumeration          #{"Enumeration" "java.util.Enumeration"}})

(def ^:private tag->render
  {:socket              socket->str
   :server-socket       server->str
   :inet-socket-address isa->str
   :inet-address        inet-address->str
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
    (clojure.core/__register-class-methods! :enumeration enumeration-methods)

    (clojure.core/__register-class-ctor! "InetSocketAddress" isa-ctor)
    (clojure.core/__register-class-ctor! "java.net.InetSocketAddress" isa-ctor)

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
        (let [cs (tag->classes (jolt.host/ref-get val :jolt/type))]
          (when (and cs (contains? cs cn)) true))))

    ;; (str sock) renders through toString like Java; pred is two cheap lookups.
    (clojure.core/__register-str!
      (fn [x] (contains? tag->render (jolt.host/ref-get x :jolt/type)))
      (fn [x] ((tag->render (jolt.host/ref-get x :jolt/type)) x)))
    true))

(register-all!)
