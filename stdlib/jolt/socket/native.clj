;; jolt.socket.native — the fd-level socket layer under jolt.socket.
;;
;; The C socket calls, the numbers that differ between macOS, Linux and Windows,
;; and the struct layouts they pass, in one place. jolt.socket's java.net shim is
;; built on it, and so can anything else that wants sockets without the java.net
;; object model: an HTTP server that owns its accept loop, say, which used to
;; have to carry its own copy of every binding and constant here and port it to
;; each platform separately.
;;
;; Every syscall binding is declared :capture-native-error and answers
;; [result error], the error read on the foreign call's own return path —
;; errno on POSIX, GetLastError (which is what WSAGetLastError reads) on Windows.
;; Reading errno in a later call instead is racy, and on Windows it is the wrong
;; slot altogether: Winsock never sets errno. Classify the error with eagain?,
;; eintr? and connect-pending?, which know each platform's numbers.

(ns jolt.socket.native
  "fd-level sockets for macOS, Linux and Windows.

  Raw calls: c-socket c-bind c-listen c-accept c-connect c-recv c-send
  c-shutdown c-close c-setsockopt c-getsockopt c-getsockname c-getpeername
  c-poll. Each answers [result error]; the error is only meaningful when result
  is negative. On Windows c-close is closesocket and c-poll is WSAPoll.

  Helpers: new-socket, set-listener-reuse!, set-dual-stack!, set-int-option!,
  set-timeout!, pending-error, close-on-exec!, set-blocking!, available,
  make-sockaddr, alloc-sockaddr, sockaddr-family, sockaddr-ip, sockaddr-port,
  sockaddr-scope-id, local-address, peer-address, resolve-addrs, pollfd
  helpers and poll-one, error classification and messages, host-name,
  numeric-address?, reverse-lookup, interface-index, interface-addresses.

  Constants are plain defs (af-inet, sol-socket, pollin, eaddrinuse, ...) for
  this host; consts-for answers the table for any of :macos :linux :windows."
  (:require [jolt.ffi :as ffi]
            [jolt.winsock :as winsock]
            [clojure.string :as str]))

;; -- platform ------------------------------------------------------------------

(def os
  "This host's socket platform: :macos, :windows or :linux. Every other POSIX
  host jolt runs on takes Linux's numbers."
  (let [n (str/lower-case (or (System/getProperty "os.name") ""))]
    (cond (str/includes? n "mac") :macos
          (str/includes? n "win") :windows
          :else :linux)))

(def ^:private macos?   (= os :macos))
(def ^:private windows? (= os :windows))

(defn consts-for
  "Every socket number that is not the same on all three platforms, plus the
  ones that are, as a map. A function of the platform rather than reads of this
  host's, so the rows no CI runner here can observe are pinned from one that
  can (test/chez/unit.edn).

  Winsock grew out of the BSD API, so it sides with macOS on the option
  numbers (SOL_SOCKET 0xffff, SO_REUSEADDR 4) and with Linux on the sockaddr
  (no sin_len). Its own: error codes are WSA* (10000 + the BSD number),
  AF_INET6 is 23, there is no SO_REUSEPORT and no MSG_NOSIGNAL (no SIGPIPE to
  suppress, and an unknown flag fails the send), and WSAPOLLFD has its own
  layout and POLL* bits."
  [os]
  (let [mac? (= os :macos) win? (= os :windows) bsd? (or mac? win?)]
    {:af-inet        2
     :af-inet6       (case os :macos 30 :windows 23 10)
     :sock-stream    1
     ;; or'd into socket()'s type (and accept4's flags) for an fd that is
     ;; close-on-exec from birth; the BSDs and Winsock have no such flag
     :sock-cloexec   (when-not bsd? 0x80000)
     :sol-socket     (if bsd? 0xffff 1)
     :so-reuseaddr   (if bsd? 4 2)
     ;; Linux load-balances new connections over every socket bound to the
     ;; port; the BSDs allow the bind but do not balance; Windows has none.
     :so-reuseport   (case os :macos 0x200 :windows nil 15)
     :so-error       (if bsd? 0x1007 4)
     :so-sndtimeo    (if bsd? 0x1005 21)
     :so-rcvtimeo    (if bsd? 0x1006 20)
     :so-nosigpipe   (when mac? 0x1022)
     :ipproto-ipv6   41
     :ipproto-tcp    6
     :tcp-nodelay    1
     :ipv6-v6only    (if bsd? 27 26)
     ;; macOS suppresses SIGPIPE per socket with SO_NOSIGPIPE instead
     :msg-nosignal   (if bsd? 0 0x4000)
     :msg-peek       2
     :fionread       (if bsd? 0x4004667F 0x541B)
     :shut-rd 0 :shut-wr 1 :shut-rdwr 2
     ;; struct pollfd is {int fd; short events; short revents} on POSIX;
     ;; WSAPOLLFD is {SOCKET fd; short events; short revents}, SOCKET being
     ;; pointer-sized. WSAPoll rejects an events mask with bits it does not
     ;; take (POLLIN there is POLLRDNORM|POLLRDBAND).
     :pollfd-size    (if win? 16 8)
     :pollfd-events  (if win? 8 4)
     :pollin         (if win? 0x300 0x1)
     :pollout        (if win? 0x10 0x4)
     :pollerr        (if win? 0x1 0x8)
     :pollhup        (if win? 0x2 0x10)
     :pollnval       (if win? 0x4 0x20)
     ;; errors. Windows answers a non-blocking connect in flight with
     ;; WSAEWOULDBLOCK, and has no EPIPE (a send on a reset connection is
     ;; WSAECONNRESET or WSAECONNABORTED).
     :eagain         (case os :macos 35 :windows 10035 11)
     :eintr          (if win? 10004 4)
     :einprogress    (case os :macos 36 :windows 10036 115)
     :ealready       (case os :macos 37 :windows 10037 114)
     :epipe          (when-not win? 32)
     :econnreset     (case os :macos 54 :windows 10054 104)
     :econnrefused   (case os :macos 61 :windows 10061 111)
     :enotconn       (case os :macos 57 :windows 10057 107)
     :eaddrinuse     (case os :macos 48 :windows 10048 98)
     :eaddrnotavail  (case os :macos 49 :windows 10049 99)
     ;; whether accept() hands the new socket the listener's non-blocking mode:
     ;; the BSDs and Winsock do, Linux does not
     :accept-inherits-nonblocking? bsd?
     ;; struct addrinfo: four ints, then ai_addrlen at 16 (socklen_t, or size_t
     ;; on Windows — the low half is the same bytes), then three pointers. The
     ;; BSDs and Windows put ai_canonname at 24 and ai_addr at 32; glibc swaps
     ;; them; bionic, which is Linux to os.name, has the BSD order. So this is
     ;; only where to look first (see ai-addr). ai_next is at 40 everywhere.
     :ai-addr-offset (if bsd? 32 24)}))

(def consts (consts-for os))

(def af-inet        (:af-inet consts))
(def af-inet6       (:af-inet6 consts))
(def sock-stream    (:sock-stream consts))
(def sock-cloexec   (:sock-cloexec consts))
(def sol-socket     (:sol-socket consts))
(def so-reuseaddr   (:so-reuseaddr consts))
(def so-reuseport   (:so-reuseport consts))
(def so-error       (:so-error consts))
(def so-sndtimeo    (:so-sndtimeo consts))
(def so-rcvtimeo    (:so-rcvtimeo consts))
(def so-nosigpipe   (:so-nosigpipe consts))
(def ipproto-ipv6   (:ipproto-ipv6 consts))
(def ipproto-tcp    (:ipproto-tcp consts))
(def tcp-nodelay    (:tcp-nodelay consts))
(def ipv6-v6only    (:ipv6-v6only consts))
(def msg-nosignal   (:msg-nosignal consts))
(def msg-peek       (:msg-peek consts))
(def fionread       (:fionread consts))
(def shut-rd        (:shut-rd consts))
(def shut-wr        (:shut-wr consts))
(def shut-rdwr      (:shut-rdwr consts))
(def pollfd-size    (:pollfd-size consts))
(def pollin         (:pollin consts))
(def pollout        (:pollout consts))
(def pollerr        (:pollerr consts))
(def pollhup        (:pollhup consts))
(def pollnval       (:pollnval consts))
(def eagain         (:eagain consts))
(def eintr          (:eintr consts))
(def einprogress    (:einprogress consts))
(def ealready       (:ealready consts))
(def epipe          (:epipe consts))
(def econnreset     (:econnreset consts))
(def econnrefused   (:econnrefused consts))
(def enotconn       (:enotconn consts))
(def eaddrinuse     (:eaddrinuse consts))
(def eaddrnotavail  (:eaddrnotavail consts))

(def accept-inherits-nonblocking?
  "Whether accept() on a non-blocking listener answers a non-blocking socket:
  true on macOS and Windows, false on Linux. A server that wants blocking
  connections from a non-blocking listener calls set-blocking! only when this
  is true."
  (:accept-inherits-nonblocking? consts))

(def poll-readable
  "The revents under which a recv answers at once: data, a hangup (0) or a bad
  fd (-1). Anything else — writability above all — means a recv would block."
  (bit-or pollin pollerr pollhup pollnval))

;; -- bindings -----------------------------------------------------------------
;; POSIX: the running process's own libc. Windows: ws2_32, which has to be asked
;; for by name — its symbols are not in jolt.exe's export table even though it is
;; linked in (jolt.winsock says more) — and kernel32 for handle inheritance.
(ffi/load-library)
(when windows?
  (ffi/load-library ["ws2_32.dll" "ws2_32"])
  (ffi/load-library ["kernel32.dll" "kernel32"])
  ;; if_nametoindex only — a scope zone named by interface (fe80::1%eth0) is
  ;; the one thing that asks, so a host without it answers no index rather
  ;; than failing the namespace
  (try (ffi/load-library ["iphlpapi.dll" "iphlpapi"]) (catch Throwable _ nil)))

;; accept/connect/recv/send/poll may block — :blocking emits them collect-safe,
;; so a thread parked in one never holds up the collector.
(ffi/defcfn c-socket      "socket"      [:int :int :int] :int {:capture-native-error true})
(ffi/defcfn c-bind        "bind"        [:int :pointer :int] :int {:capture-native-error true})
(ffi/defcfn c-listen      "listen"      [:int :int] :int {:capture-native-error true})
(ffi/defcfn c-connect     "connect"     [:int :pointer :int] :int
  {:blocking true :capture-native-error true})
(ffi/defcfn c-shutdown    "shutdown"    [:int :int] :int {:capture-native-error true})
(ffi/defcfn c-setsockopt  "setsockopt"  [:int :int :int :pointer :int] :int
  {:capture-native-error true})
(ffi/defcfn c-getsockopt  "getsockopt"  [:int :int :int :pointer :pointer] :int
  {:capture-native-error true})
(ffi/defcfn c-getsockname "getsockname" [:int :pointer :pointer] :int
  {:capture-native-error true})
(ffi/defcfn c-getpeername "getpeername" [:int :pointer :pointer] :int
  {:capture-native-error true})
(ffi/defcfn c-getaddrinfo  "getaddrinfo"  [:pointer :pointer :pointer :pointer] :int :blocking)
(ffi/defcfn c-freeaddrinfo "freeaddrinfo" [:pointer] :void)
(ffi/defcfn c-inet-ntop    "inet_ntop"    [:int :pointer :pointer :uint] :pointer)
(ffi/defcfn c-inet-pton    "inet_pton"    [:int :pointer :pointer] :int)
(ffi/defcfn c-gethostname  "gethostname"  [:pointer :size_t] :int)
(ffi/defcfn c-getnameinfo  "getnameinfo"
  [:pointer :uint :pointer :uint :pointer :uint :int] :int :blocking)
;; libc on POSIX; iphlpapi on Windows (loaded below). 0 for an unknown name.
(ffi/defcfn c-if-nametoindex "if_nametoindex" [:pointer] :uint)

;; The rest differ by platform in signature or name, not only in value, so they
;; live in the taken branch — a symbol one OS lacks (closesocket, getifaddrs) can
;; be bound nowhere else. jolt interns the vars from both branches at analysis
;; time, so references resolve either way.
(if windows?
  (do
    ;; Winsock's recv/send take and return int, not size_t/ssize_t; a socket is
    ;; closed with closesocket; the ioctl is ioctlsocket, fixed-arity.
    (ffi/defcfn c-recv  "recv" [:int :pointer :int :int] :int
      {:blocking true :capture-native-error true})
    (ffi/defcfn c-send  "send" [:int :pointer :int :int] :int
      {:blocking true :capture-native-error true})
    (ffi/defcfn c-close "closesocket" [:int] :int {:capture-native-error true})
    (ffi/defcfn c-accept "accept" [:int :pointer :pointer] :int
      {:blocking true :capture-native-error true})
    (ffi/defcfn c-poll  "WSAPoll" [:pointer :uint :int] :int
      {:blocking true :capture-native-error true})
    (ffi/defcfn c-ioctl "ioctlsocket" [:int :int :pointer] :int {:capture-native-error true})
    (ffi/defcfn c-set-handle-information "SetHandleInformation" [:uptr :uint :uint] :int)
    (ffi/defcfn c-get-handle-information "GetHandleInformation" [:uptr :pointer] :int))
  (do
    (ffi/defcfn c-recv  "recv" [:int :pointer :size_t :int] :ssize_t
      {:blocking true :capture-native-error true})
    (ffi/defcfn c-send  "send" [:int :pointer :size_t :int] :ssize_t
      {:blocking true :capture-native-error true})
    (ffi/defcfn c-close "close" [:int] :int {:capture-native-error true})
    ;; Linux accepts close-on-exec in one call, so a process spawned on another
    ;; thread never sees the connection; macOS has no accept4 and guard-accepted!
    ;; sets it after
    (if sock-cloexec
      (do (ffi/defcfn c-accept4 "accept4" [:int :pointer :pointer :int] :int
            {:blocking true :capture-native-error true})
          (defn c-accept [fd addr addrlen] (c-accept4 fd addr addrlen sock-cloexec)))
      (ffi/defcfn c-accept "accept" [:int :pointer :pointer] :int
        {:blocking true :capture-native-error true}))
    (ffi/defcfn c-poll  "poll" [:pointer :int :int] :int
      {:blocking true :capture-native-error true})
    ;; ioctl and fcntl are variadic; the :varargs marker puts the third argument
    ;; where the callee's va_list reads it. Bound fixed-arity, Apple arm64
    ;; answers success with the out-parameter untouched, since variadic
    ;; arguments travel on the stack there.
    (ffi/defcfn c-ioctl "ioctl" [:int :ulong :varargs :pointer] :int {:capture-native-error true})
    (ffi/defcfn c-fcntl "fcntl" [:int :int :varargs :int] :int {:capture-native-error true})
    (ffi/defcfn c-gai-strerror "gai_strerror" [:int] :pointer)
    (ffi/defcfn c-getifaddrs  "getifaddrs"  [:pointer] :int)
    (ffi/defcfn c-freeifaddrs "freeifaddrs" [:pointer] :void)))

;; -- errors ---------------------------------------------------------------------

(defn eagain?
  "Whether error e means the call would block: EAGAIN/EWOULDBLOCK, or
  WSAEWOULDBLOCK."
  [e] (= e eagain))

(defn eintr?
  "Whether error e is an interrupted call, to be retried as is."
  [e] (= e eintr))

(defn connect-pending?
  "Whether a non-blocking connect's error e means it is still in progress.
  Windows answers WSAEWOULDBLOCK for what POSIX calls EINPROGRESS."
  [e] (or (= e einprogress) (= e ealready) (and windows? (= e eagain))))

(def ^:private wsa-names
  {10004 "Interrupted function call" 10009 "Bad file descriptor"
   10013 "Permission denied" 10014 "Bad address" 10022 "Invalid argument"
   10024 "Too many open sockets" 10035 "Resource temporarily unavailable"
   10036 "Operation now in progress" 10037 "Operation already in progress"
   10038 "Socket operation on nonsocket" 10040 "Message too long"
   10048 "Address already in use" 10049 "Cannot assign requested address"
   10050 "Network is down" 10051 "Network is unreachable"
   10053 "Software caused connection abort" 10054 "Connection reset by peer"
   10055 "No buffer space available" 10056 "Socket is already connected"
   10057 "Socket is not connected" 10058 "Cannot send after socket shutdown"
   10060 "Connection timed out" 10061 "Connection refused"
   10065 "No route to host" 10093 "WSAStartup not yet performed"
   11001 "No such host is known" 11002 "Nonauthoritative host not found"
   11003 "This is a nonrecoverable error"
   11004 "Valid name, no data record of requested type"})

(defn error-message
  "A description of socket error e: strerror on POSIX. Windows's C library does
  not know the WSA codes, so they are named from a table, falling back to the
  number."
  [e]
  (if windows?
    (str (or (wsa-names e) "Winsock error") " (WSA " e ")")
    (or (try (ffi/errno-message e) (catch Throwable _ nil))
        (str "errno " e))))

(defn- gai-message [code]
  (if windows?
    (error-message code)
    (or (try (ffi/ptr->string (c-gai-strerror code)) (catch Throwable _ nil))
        (str "getaddrinfo error " code))))

;; -- sockets and options ----------------------------------------------------

(defn- int-cell [v]
  (let [p (ffi/alloc 4)] (ffi/write p :int v 0) p))

(defn set-int-option!
  "setsockopt with an int value. Answers [result error]."
  [fd level opt v]
  (let [p (int-cell v)]
    (try (c-setsockopt fd level opt p 4)
         (finally (ffi/free p)))))

(defn get-int-option
  "getsockopt of an int option, or nil when the call fails."
  [fd level opt]
  (let [p (int-cell 0) lenp (int-cell 4)]
    (try
      (let [[r _] (c-getsockopt fd level opt p lenp)]
        (when-not (neg? r) (ffi/read p :int 0)))
      (finally (ffi/free p) (ffi/free lenp)))))

(defn pending-error
  "SO_ERROR: the pending error on fd, which is how a non-blocking connect
  reports its outcome. 0 for none; -1 when the option cannot be read."
  [fd]
  (or (get-int-option fd sol-socket so-error) -1))

(defn set-timeout!
  "Set SO_RCVTIMEO or SO_SNDTIMEO (opt) to ms milliseconds; 0 waits forever.
  POSIX takes a struct timeval, whose tv_usec is MICROseconds — the sub-second
  part of ms times 1000, and tv_usec is 4 bytes on macOS and 8 on Linux.
  Winsock takes a DWORD of milliseconds. Answers [result error]."
  [fd opt ms]
  (if windows?
    (set-int-option! fd sol-socket opt ms)
    (let [tv (ffi/alloc 16)]
      (try
        (ffi/write tv :uint64 (quot ms 1000) 0)
        (let [usec (* 1000 (rem ms 1000))]
          (if macos?
            (ffi/write tv :uint usec 8)
            (ffi/write tv :uint64 usec 8)))
        (c-setsockopt fd sol-socket opt tv 16)
        (finally (ffi/free tv))))))

;; F_GETFD / F_SETFD / FD_CLOEXEC and F_GETFL / F_SETFL are the same numbers on
;; macOS and Linux; O_NONBLOCK is not.
(def ^:private f-getfd 1)
(def ^:private f-setfd 2)
(def ^:private fd-cloexec 1)
(def ^:private f-getfl 3)
(def ^:private f-setfl 4)
(def ^:private o-nonblock (if macos? 0x4 0x800))
(def ^:private handle-flag-inherit 1)
(def ^:private fionbio 0x8004667E)

(defn close-on-exec?
  "Whether fd is kept from processes this one starts: FD_CLOEXEC on POSIX, a
  socket handle that is not inheritable on Windows."
  [fd]
  (if windows?
    (let [p (int-cell 0)]
      (try
        (and (not (zero? (c-get-handle-information fd p)))
             (zero? (bit-and (ffi/read p :int 0) handle-flag-inherit)))
        (finally (ffi/free p))))
    (let [[flags _] (c-fcntl fd f-getfd 0)]
      (and (not (neg? flags)) (pos? (bit-and flags fd-cloexec))))))

(defn close-on-exec!
  "Keep fd from every process this one starts. Without it each child — a build,
  a REPL, a test runner — holds a duplicate of the socket: a listener's port
  stays bound while any of them lives, and an accepted connection stays open
  past its close. Answers whether the flag is set, read back."
  [fd]
  (if windows?
    (c-set-handle-information fd handle-flag-inherit 0)
    (c-fcntl fd f-setfd fd-cloexec))
  (close-on-exec? fd))

(defn set-blocking!
  "Put fd in blocking (true) or non-blocking (false) mode: O_NONBLOCK on POSIX,
  FIONBIO on Windows. Answers [result error]."
  [fd blocking?]
  (if windows?
    (let [p (int-cell (if blocking? 0 1))]
      (try (c-ioctl fd fionbio p)
           (finally (ffi/free p))))
    (let [[flags e :as r] (c-fcntl fd f-getfl 0)]
      (if (neg? flags)
        r
        (c-fcntl fd f-setfl (if blocking?
                              (bit-and-not flags o-nonblock)
                              (bit-or flags o-nonblock)))))))

(defn available
  "How many bytes can be read from fd without waiting (FIONREAD), or -1 when
  the call fails."
  [fd]
  (let [p (int-cell 0)]
    (try
      (let [[r _] (c-ioctl fd fionread p)]
        (if (neg? r) -1 (max 0 (ffi/read p :int 0))))
      (finally (ffi/free p)))))

(defn set-listener-reuse!
  "What a listener sets so a restarted server can rebind a port its
  predecessor left in TIME_WAIT, without letting a second listener share a
  live one: SO_REUSEADDR on POSIX. Nothing on Windows, where SO_REUSEADDR
  means the second thing — bind a port another socket is listening on — and
  the default already allows the first. Answers [result error]."
  [fd]
  (if windows?
    [0 0]
    (set-int-option! fd sol-socket so-reuseaddr 1)))

(defn set-reuse-address!
  "SO_REUSEADDR on or off for a listener's fd, as ServerSocket.setReuseAddress
  asks before bind; nothing on Windows (set-listener-reuse!). Answers [result
  error]."
  [fd on?]
  (if windows?
    [0 0]
    (set-int-option! fd sol-socket so-reuseaddr (if on? 1 0))))

(def listener-reuses?
  "What a new listener's getReuseAddress answers: set-listener-reuse! turns
  SO_REUSEADDR on except on Windows."
  (not windows?))

(defn new-socket
  "A new stream socket of family (af-inet or af-inet6), or throws IOException.
  Initializes Winsock first. The fd is close-on-exec, and on macOS it carries
  SO_NOSIGPIPE, so a send to a peer that has gone raises an error rather than
  the signal that would end the process. Linux suppresses that per send
  (msg-nosignal); Windows has no SIGPIPE."
  [family]
  (winsock/ensure!)
  (let [[fd e] (c-socket family (bit-or sock-stream (or sock-cloexec 0)) 0)]
    (when (neg? fd)
      (throw (java.io.IOException. (str "socket() failed: " (error-message e)))))
    (close-on-exec! fd)
    (when so-nosigpipe (set-int-option! fd sol-socket so-nosigpipe 1))
    fd))

(defn set-dual-stack!
  "Clear IPV6_V6ONLY on a v6 socket, so it also carries IPv4 traffic as mapped
  addresses (::ffff:a.b.c.d) — the JDK's sockets are dual-stack. Linux's
  default is a sysctl and Windows's is v6-only, so it is set either way.
  Answers [result error]."
  [fd]
  (set-int-option! fd ipproto-ipv6 ipv6-v6only 0))

(defn guard-accepted!
  "What an accepted fd needs before use: close-on-exec, and SO_NOSIGPIPE on
  macOS — an accepted socket does not reliably inherit either."
  [fd]
  (close-on-exec! fd)
  (when so-nosigpipe (set-int-option! fd sol-socket so-nosigpipe 1))
  fd)

;; -- sockaddr ---------------------------------------------------------------------
;; sockaddr_in (16 bytes): header(2) + port(2, network order) + addr(4) + pad(8).
;; sockaddr_in6 (28): header(2) + port(2) + flowinfo(4) + addr(16) + scope_id(4).
;; The header is a one-byte length then a one-byte family on macOS, and a 16-bit
;; family on Linux and Windows. The port sits at bytes 2-3 in both families.

(def sockaddr-storage-size
  "sockaddr_storage: room for either family, which is what accept and
  getsockname should be handed."
  128)

(defn sockaddr-family [sa]
  (if macos? (ffi/read sa :uint8 1) (ffi/read sa :uint16 0)))

(defn sockaddr-port [sa]
  (bit-or (bit-shift-left (ffi/read sa :uint8 2) 8) (ffi/read sa :uint8 3)))

(defn set-sockaddr-port! [sa port]
  (ffi/write sa :uint8 (bit-and (bit-shift-right port 8) 0xff) 2)
  (ffi/write sa :uint8 (bit-and port 0xff) 3)
  sa)

(defn- write-header! [sa family len]
  (if macos?
    (do (ffi/write sa :uint8 len 0) (ffi/write sa :uint8 family 1))
    (ffi/write sa :uint16 family 0)))

(defn sockaddr-ip
  "The presentation form of sa's address: a dotted quad for v4, inet_ntop's
  RFC 5952 form for v6 (no scope zone). nil for any other family."
  [sa]
  (let [fam (sockaddr-family sa)]
    (cond
      (= fam af-inet)
      (str/join "." (map #(ffi/read sa :uint8 (+ 4 %)) (range 4)))

      (= fam af-inet6)
      (let [out (ffi/alloc 46)]
        (try
          (let [p (c-inet-ntop af-inet6 (+ sa 8) out 46)]
            (when-not (or (nil? p) (ffi/null? p)) (ffi/ptr->string out)))
          (finally (ffi/free out)))))))

(defn sockaddr-scope-id
  "sin6_scope_id of a v6 sockaddr (the interface a link-local address is on),
  0 for none or for a v4 sockaddr."
  [sa]
  (if (= (sockaddr-family sa) af-inet6) (ffi/read sa :uint 24) 0))

(defn sockaddr-length
  "The length bind/connect want for a sockaddr of family."
  [family]
  (if (= family af-inet6) 28 16))

(defn make-sockaddr
  "A freshly allocated sockaddr for the numeric address ip (a v4 or v6 literal,
  no scope zone) and port, as [sa len], or nil when ip is not a literal of
  family. scope-id, for a v6 address, is its sin6_scope_id. The caller frees
  sa."
  ([family ip port] (make-sockaddr family ip port 0))
  ([family ip port scope-id]
   (let [len (sockaddr-length family)
         sa  (ffi/alloc len)
         src (ffi/string->ptr (str ip))]
     (try
       (write-header! sa family len)
       (set-sockaddr-port! sa port)
       (if (= 1 (c-inet-pton family src (+ sa (if (= family af-inet6) 8 4))))
         (do (when (and (= family af-inet6) scope-id)
               (ffi/write sa :uint scope-id 24))
             [sa len])
         (do (ffi/free sa) nil))
       (finally (ffi/free src))))))

(defn alloc-sockaddr
  "A zeroed sockaddr_storage and its in/out length cell, as [sa lenp], ready for
  accept, getsockname or getpeername. Reset the cell to sockaddr-storage-size
  before reusing it. The caller frees both."
  []
  (let [sa (ffi/alloc sockaddr-storage-size)]
    [sa (int-cell sockaddr-storage-size)]))

(defn- named-address [f fd]
  (let [[sa lenp] (alloc-sockaddr)]
    (try
      (let [[r _] (f fd sa lenp)]
        (when-not (neg? r)
          {:family (sockaddr-family sa) :ip (sockaddr-ip sa) :port (sockaddr-port sa)
           :scope-id (sockaddr-scope-id sa)}))
      (finally (ffi/free sa) (ffi/free lenp)))))

(defn local-address
  "{:family :ip :port :scope-id} fd is bound to, or nil when getsockname fails."
  [fd] (named-address c-getsockname fd))

(defn peer-address
  "{:family :ip :port :scope-id} of fd's peer, or nil when getpeername fails."
  [fd] (named-address c-getpeername fd))

(defn local-port
  "The port fd is bound to — the kernel's pick after binding port 0 — or -1."
  [fd]
  (or (:port (local-address fd)) -1))

;; -- name resolution --------------------------------------------------------------

(def ^:private addrinfo-size 48)
(def ^:private ai-passive 1)

(defn- literal-addr
  "The resolve-addrs entry for host when it is a numeric literal of an allowed
  family, else nil."
  [host port family]
  (some (fn [fam]
          (when (or (nil? family) (= family fam))
            (when-let [[sa len] (make-sockaddr fam host port)]
              {:family fam :addr sa :addrlen len :ip (sockaddr-ip sa) :scope-id 0})))
        [af-inet af-inet6]))

(declare getaddrinfo-addrs free-addrs!)

(defn- ai-addr
  "An addrinfo entry's ai_addr. ai_canonname and ai_addr trade places between
  libcs, and bionic reports itself as Linux while keeping the BSD order, so the
  pointer is checked rather than trusted: without AI_CANONNAME only one of the
  two slots is set, and it is the sockaddr whose family is ai_family. The
  platform's usual offset is tried first."
  [ai fam]
  (let [first-off (:ai-addr-offset consts)]
    (some (fn [off]
            (let [p (ffi/read ai :pointer off)]
              (when (and p (not (ffi/null? p)) (= fam (sockaddr-family p)))
                p)))
          [first-off (if (= 24 first-off) 32 24)])))

(defn resolve-addrs
  "The addresses host names, as data:
  {:addrs [{:family :addr :addrlen :ip :scope-id} ...]}
  or {:error code :message text}. Numeric literals and names, v4 and v6.

  A literal is parsed with inet_pton and never reaches the resolver, as the
  java.net shim did before this layer existed (inet_addr first): no lookup
  for what is already an address. A v6 literal with a scope zone
  (fe80::1%en0) is not inet_pton's, and takes getaddrinfo.

  Each :addr is a sockaddr of OUR allocation with port already written in — the
  getaddrinfo chain is freed before returning — and is the caller's to free
  (free-addrs!). Duplicates (a name listed twice in /etc/hosts) come back once.

  opts: :family (af-inet or af-inet6; default either), :passive? (AI_PASSIVE,
  for an address to bind). A nil host with :passive? is the wildcard address,
  nil without it the loopback, as getaddrinfo answers a NULL node."
  ([host port] (resolve-addrs host port nil))
  ([host port {:keys [family] :as opts}]
   (winsock/ensure!)
   (if-let [lit (when host (literal-addr (str host) port family))]
     {:addrs [lit]}
     (getaddrinfo-addrs host port opts))))

(defn- getaddrinfo-addrs
  [host port {:keys [family passive?]}]
   ;; a NULL node, not "": AI_PASSIVE answers the wildcard only for NULL. Node
   ;; and service cannot both be NULL, so a NULL node names the port.
   (let [node  (if host (ffi/string->ptr (str host)) ffi/null)
         svc   (if host ffi/null (ffi/string->ptr (str port)))
         hints (ffi/alloc addrinfo-size)
         resp  (ffi/alloc 8)]
     (try
       (ffi/write hints :int (if passive? ai-passive 0) 0)
       (ffi/write hints :int (or family 0) 4)
       (ffi/write hints :int sock-stream 8)
       (let [rc (c-getaddrinfo node svc hints resp)]
         (if-not (zero? rc)
           {:error rc :message (gai-message rc)}
           (let [head (ffi/read resp :pointer)
                 ;; what has been copied so far, freed if the walk throws
                 copied (volatile! [])]
             (try
               (loop [ai head out [] seen #{}]
                 (if (or (nil? ai) (ffi/null? ai))
                   (do (vreset! copied nil)
                       {:addrs out})
                   (let [fam     (ffi/read ai :int 4)
                         addrlen (ffi/read ai :int 16)
                         src     (ai-addr ai fam)
                         k (when (and (or (= fam af-inet) (= fam af-inet6))
                                      (pos? addrlen)
                                      src)
                             [fam (mapv #(ffi/read src :uint8 %) (range addrlen))])
                         entry (when (and k (not (contains? seen k)))
                                 (let [sa (ffi/alloc addrlen)]
                                   (dotimes [i addrlen]
                                     (ffi/write sa :uint8 (ffi/read src :uint8 i) i))
                                   (vswap! copied conj {:addr sa})
                                   (set-sockaddr-port! sa port)
                                   {:family fam :addr sa :addrlen addrlen
                                    :ip (sockaddr-ip sa)
                                    :scope-id (sockaddr-scope-id sa)}))]
                     (recur (ffi/read ai :pointer 40)
                            (if entry (conj out entry) out)
                            (if k (conj seen k) seen)))))
             (finally
               (when-not (or (nil? head) (ffi/null? head)) (c-freeaddrinfo head))
               (when-let [c @copied] (free-addrs! c)))))))
       (finally (if host (ffi/free node) (ffi/free svc)) (ffi/free hints) (ffi/free resp)))))

(defn free-addrs!
  "Free the :addr of every entry resolve-addrs answered."
  [addrs]
  (doseq [{a :addr} addrs] (ffi/free a)))

(defn host-name
  "gethostname, or \"localhost\" when it fails."
  []
  (winsock/ensure!)
  (let [n 256 buf (ffi/alloc n)]
    (try
      (if (neg? (c-gethostname buf n)) "localhost" (ffi/ptr->string buf))
      (finally (ffi/free buf)))))

(defn interface-index
  "The index of the network interface named nm (if_nametoindex), or nil when
  there is no such interface."
  [nm]
  (winsock/ensure!)
  (let [p (ffi/string->ptr (str nm))]
    (try
      (let [i (try (c-if-nametoindex p) (catch Throwable _ 0))]
        (when (pos? i) i))
      (finally (ffi/free p)))))

(defn numeric-address?
  "Whether s is a v4 or v6 address literal, as inet_pton reads them."
  [s]
  (boolean (some (fn [fam] (when-let [[sa _] (make-sockaddr fam s 0)] (ffi/free sa) true))
                 [af-inet af-inet6])))

(defn reverse-lookup
  "The name DNS gives back for the numeric address ip, or nil. getnameinfo with
  no flags falls back to the numeric form itself, so an answer equal to ip means
  the lookup found nothing."
  [ip]
  (winsock/ensure!)
  (let [fam (if (str/includes? (str ip) ":") af-inet6 af-inet)]
    (when-let [[sa len] (make-sockaddr fam ip 0)]
      (let [n 1025 buf (ffi/alloc n)]
        (try
          (when (zero? (c-getnameinfo sa len buf n ffi/null 0 0))
            (let [nm (ffi/ptr->string buf)]
              ;; the numeric fallback comes back in inet_ntop's form, which
              ;; need not be ip's (0:0:0:0:0:0:0:2 is answered as ::2)
              (when-not (or (str/blank? nm) (= nm ip) (numeric-address? nm)) nm)))
          (finally (ffi/free buf) (ffi/free sa)))))))

;; -- loopback pair ----------------------------------------------------------------

(defn- loopback-pair-once
  "One try at loopback-pair: [a b], or nil when what accept() answered is not a's
  peer — another local process connected to the listener first."
  []
  (let [l (new-socket af-inet)
        opened (atom [])                  ; l is closed by the finally
        fail (fn [what e]
               (doseq [fd @opened] (c-close fd))
               (throw (java.io.IOException. (str "loopback-pair: " what ": " (error-message e)))))]
    (try
      (let [[sa len] (make-sockaddr af-inet "127.0.0.1" 0)]
        (try
          (let [[r e] (c-bind l sa len)] (when (neg? r) (fail "bind" e)))
          (let [[r e] (c-listen l 1)] (when (neg? r) (fail "listen" e)))
          (set-sockaddr-port! sa (local-port l))
          (let [a (new-socket af-inet)]
            (swap! opened conj a)
            (let [[r e] (c-connect a sa len)] (when (neg? r) (fail "connect" e)))
            (let [[psa plen] (alloc-sockaddr)]
              (try
                (let [[b e] (c-accept l psa plen)]
                  (when (neg? b) (fail "accept" e))
                  (guard-accepted! b)
                  (if (= (local-address a) (peer-address b))
                    [a b]
                    (do (c-close b) (c-close a) nil)))
                (finally (ffi/free psa) (ffi/free plen)))))
          (finally (ffi/free sa))))
      (finally (c-close l)))))

(defn loopback-pair
  "Two connected TCP sockets over 127.0.0.1, as [a b], both close-on-exec — a
  socketpair that works on Windows, where the only thing WSAPoll can wait on is
  a socket, so a wake channel for a poll loop has to be one. Throws
  IOException when any step fails, closing what it opened.

  The accepted end is checked to be a's peer: the listener is on an open
  loopback port, and a stranger connecting first would otherwise be handed back
  as b. Both ends are TCP_NODELAY, so a one-byte write is not held by Nagle
  behind an unacknowledged earlier one (up to the delayed-ACK timer, 200ms on
  Windows)."
  []
  (loop [tries 3]
    (if-let [[a b :as pair] (loopback-pair-once)]
      (do (set-int-option! a ipproto-tcp tcp-nodelay 1)
          (set-int-option! b ipproto-tcp tcp-nodelay 1)
          pair)
      (if (pos? (dec tries))
        (recur (dec tries))
        (throw (java.io.IOException. "loopback-pair: another connection took the listener"))))))

;; -- poll -----------------------------------------------------------------------------
;; One array of pollfds: entry i at i * pollfd-size. fd at 0, events and revents
;; as shorts after it.

(defn alloc-pollfds
  "A zeroed array of n pollfds. Zeroed matters: a stray high byte in events is
  a POLLOUT-family bit on POSIX, and turns every poll on a writable socket
  into an instant wake. The caller frees it."
  [n]
  (ffi/alloc (* n pollfd-size)))

(defn init-pollfd!
  "Arm pollfd i of pfds for events on fd, clearing revents."
  ([pfds fd events] (init-pollfd! pfds 0 fd events))
  ([pfds i fd events]
   (let [base (* i pollfd-size) ev (:pollfd-events consts)]
     (if windows?
       (ffi/write pfds :uint64 fd base)
       (ffi/write pfds :int fd base))
     (ffi/write pfds :uint16 (bit-and events 0xffff) (+ base ev))
     (ffi/write pfds :uint16 0 (+ base ev 2)))
   pfds))

(defn pollfd-revents
  "What poll reported for pollfd i."
  ([pfds] (pollfd-revents pfds 0))
  ([pfds i]
   (ffi/read pfds :uint16 (+ (* i pollfd-size) (:pollfd-events consts) 2))))

(defn poll-one
  "Wait up to timeout-ms (-1 forever, 0 not at all) for events on fd. Answers
  revents — 0 on a timeout — or -1 when poll fails other than by EINTR, which
  is retried."
  [fd events timeout-ms]
  (let [pfds (alloc-pollfds 1)]
    (try
      (init-pollfd! pfds fd events)
      (loop []
        (let [[rc e] (c-poll pfds 1 timeout-ms)]
          (cond
            (pos? rc) (pollfd-revents pfds)
            (zero? rc) 0
            (eintr? e) (recur)
            :else -1)))
      (finally (ffi/free pfds)))))

;; -- interfaces ---------------------------------------------------------------------
;; getifaddrs(3) reports one entry per address, so an interface with an IPv4
;; address and a MAC appears twice. struct ifaddrs is laid out the same on macOS
;; and Linux for the fields read here: ifa_next 0, ifa_name 8, ifa_flags 16,
;; ifa_addr 24, ifa_netmask 32, ifa_broadaddr 40 (valid under IFF_BROADCAST,
;; which is 0x2 on both).

;; The link-layer family a MAC is reported under, and where it sits in that
;; entry's sockaddr: BSD's sockaddr_dl carries the interface name (sdl_nlen at 5)
;; before the address (sdl_alen at 6, data at 8); Linux's sockaddr_ll has a
;; fixed 12-byte header (sll_halen at 11).
(def ^:private af-link (if macos? 18 17))

(defn- mac-bytes [sa]
  (let [[off len] (if macos?
                    [(+ 8 (ffi/read sa :uint8 5)) (ffi/read sa :uint8 6)]
                    [12 (ffi/read sa :uint8 11)])]
    (when (pos? len)
      (let [bs (mapv (fn [i] (ffi/read sa :uint8 (+ off i))) (range len))]
        ;; all zero is what an interface without hardware reports
        (when (some pos? bs) (byte-array bs))))))

(def ^:private iff-broadcast 0x2)

(defn- prefix-length
  "The bits set in a netmask sockaddr, read where family's address sits. A
  macOS netmask can be cut short — its sa_len stops at the last nonzero byte —
  so nothing past that length is read there."
  [sa family]
  (if (or (nil? sa) (ffi/null? sa))
    0
    (let [[off n] (if (= family af-inet6) [8 16] [4 4])
          n (if macos? (max 0 (min n (- (ffi/read sa :uint8 0) off))) n)]
      (reduce + (map (fn [i] (Long/bitCount (ffi/read sa :uint8 (+ off i)))) (range n))))))

(defn interface-addresses
  "One map per getifaddrs entry, {:name} plus, for an IPv4 or IPv6 address,
  :family :ip :scope-id :prefix (the netmask's length) and, for a v4 address on
  a broadcast interface, :broadcast; or :mac for a link-layer one. Empty on Windows, which has no
  getifaddrs: enumerating adapters there is a GetAdaptersAddresses walk, not
  written here. Throws IOException when getifaddrs fails."
  []
  (if windows?
    []
    (let [pp (ffi/alloc (ffi/sizeof :pointer))]
      (try
        (when (neg? (c-getifaddrs pp))
          (throw (java.io.IOException. "getifaddrs() failed")))
        (let [head (ffi/read pp :pointer)]
          (try
            (loop [cur head acc []]
              (if (or (nil? cur) (ffi/null? cur))
                acc
                (let [nm  (ffi/ptr->string (ffi/read cur :pointer 8))
                      sa  (ffi/read cur :pointer 24)
                      fam (when-not (ffi/null? sa) (sockaddr-family sa))
                      flags (ffi/read cur :uint 16)
                      bsa (ffi/read cur :pointer 40)]
                  (recur (ffi/read cur :pointer 0)
                         (conj acc (cond-> {:name nm}
                                     (or (= fam af-inet) (= fam af-inet6))
                                     (assoc :family fam :ip (sockaddr-ip sa)
                                            :scope-id (sockaddr-scope-id sa)
                                            :prefix (prefix-length (ffi/read cur :pointer 32) fam))
                                     (and (= fam af-inet)
                                          (pos? (bit-and flags iff-broadcast))
                                          bsa (not (ffi/null? bsa)))
                                     (assoc :broadcast (sockaddr-ip bsa))
                                     (= fam af-link) (assoc :mac (mac-bytes sa))))))))
            (finally (c-freeifaddrs head))))
        (finally (ffi/free pp))))))
