(ns jolt.mvn-http
  "Minimal cert-verifying HTTPS GET-to-file for dependency download. A plain
  TCP socket (getaddrinfo/socket/connect) carries ciphertext; TLS runs against
  in-memory BIOs over the system OpenSSL via jolt.ffi, so no raw fd reaches
  OpenSSL. libcrypto then libssl lazy-load on first use, from an ordered
  candidate list: a JOLT_OPENSSL_LIBDIR directory first when set, then on
  macOS the Homebrew paths, on Windows the OpenSSL 3 that Git for Windows
  ships (Git/mingw64/bin, tried BEFORE the bare DLL names so a PATH that
  happens to hold some other libcrypto does not decide), then the bare
  sonames the OS loader searches. fetch returns true on a 2xx, false on any
  failure, so jolt.deps falls through to the next repo. HTTPS only; the server
  certificate is verified (default verify paths + VERIFY_PEER + hostname check).
  On Windows the sockets are ws2_32 + WSAStartup + closesocket."
  (:require [jolt.ffi :as ffi]
            [jolt.winsock :as winsock]
            [jolt.socket.native :as native]
            [clojure.string :as str]))

(def ^:private os-name
  (str/lower-case (or (System/getProperty "os.name") "")))
(def ^:private macos? (str/includes? os-name "mac"))
(def ^:private windows? (str/includes? os-name "windows"))

;; libcrypto loads first (libssl links it). On macOS the system /usr/lib
;; libcrypto is a protected Apple image and *aborts the process* if dlopen'd
;; (an uncatchable SIGABRT, not a Scheme error) — so only explicit real-OpenSSL
;; paths are tried; the bare "libcrypto.dylib" name is deliberately NOT a
;; candidate. If none of these exist, fetch fails gracefully.
(def ^:private crypto-names
  (cond
    macos?   ["libcrypto.3.dylib" "libcrypto.dylib"]
    windows? ["libcrypto-3-x64.dll" "libcrypto-3.dll" "libcrypto-1_1-x64.dll"]
    :else    ["libcrypto.so.3" "libcrypto.so.1.1" "libcrypto.so"]))

(def ^:private ssl-names
  (cond
    macos?   ["libssl.3.dylib" "libssl.dylib"]
    windows? ["libssl-3-x64.dll" "libssl-3.dll" "libssl-1_1-x64.dll"]
    :else    ["libssl.so.3" "libssl.so.1.1" "libssl.so"]))

;; Everywhere but macOS the bare names double as the system candidates (the
;; dynamic loader owns the search); macOS gets the explicit real-OpenSSL paths.
(def ^:private crypto-candidates
  (if macos?
    ["/opt/homebrew/opt/openssl@3/lib/libcrypto.dylib"
     "/opt/homebrew/lib/libcrypto.dylib"
     "/usr/local/opt/openssl@3/lib/libcrypto.dylib"]
    crypto-names))

(def ^:private ssl-candidates
  (if macos?
    ["/opt/homebrew/opt/openssl@3/lib/libssl.dylib"
     "/opt/homebrew/lib/libssl.dylib"
     "/usr/local/opt/openssl@3/lib/libssl.dylib"]
    ssl-names))

;; Windows has no OpenSSL of its own, but Git for Windows ships OpenSSL 3: under
;; ucrt64/bin from 2.56 (its MINGW64 -> UCRT64 move left no mingw64 directory),
;; under mingw64/bin before that. Both are tried, the current layout first, in
;; the machine-wide install under ProgramFiles (ProgramW6432 is the same
;; directory as a 32-bit process sees it) and the per-user one under
;; LOCALAPPDATA/Programs. Pure over its three roots so the gate can pin the
;; list. One entry per directory: Windows compares paths without case, and a
;; root may arrive with a trailing separator.
(defn- windows-openssl-libdirs-for [program-files program-w6432 local-app-data]
  (let [trimmed (fn [root] (str/replace root #"[\\/]+$" ""))
        dirs (for [[root git] [[program-files "Git"]
                               [program-w6432 "Git"]
                               [local-app-data "Programs\\Git"]]
                   :when (not (str/blank? root))
                   env ["ucrt64" "mingw64"]]
               (str (trimmed root) "\\" git "\\" env "\\bin"))]
    (loop [dirs dirs seen #{} acc []]
      (if-let [[d & more] (seq dirs)]
        (let [k (str/lower-case d)]
          (recur more (conj seen k) (if (seen k) acc (conj acc d))))
        acc))))

;; The directories whose libcrypto/libssl are tried before the bare names, in
;; order: JOLT_OPENSSL_LIBDIR when set — the seam for an OpenSSL living outside
;; the built-in paths (Nix, MacPorts, Guix, a nonstandard Homebrew prefix); the
;; macOS hazard above does not apply to it, its entries are absolute paths into
;; the named directory, never the bare Apple-shadowed sonames — then, on
;; Windows, the Git for Windows directories. Pure over what ensure-native!
;; reads from the environment, so the gate pins the order.
(defn- openssl-libdirs-for [explicit windows? program-files program-w6432 local-app-data]
  (cond-> (if (str/blank? explicit) [] [explicit])
    windows? (into (windows-openssl-libdirs-for program-files program-w6432 local-app-data))))

(defn- runtime-openssl-libdirs []
  (openssl-libdirs-for (System/getenv "JOLT_OPENSSL_LIBDIR") windows?
                       (System/getenv "ProgramFiles")
                       (System/getenv "ProgramW6432")
                       (System/getenv "LOCALAPPDATA")))

;; NAME under DIR, joined with the separator DIR itself uses, so a Windows
;; directory stays one spelling end to end (the loader takes either).
(defn- dir-join [dir name]
  (str dir (if (and (str/includes? dir "\\") (not (str/includes? dir "/"))) "\\" "/") name))

;; NAMES under each of LIBDIRS, directory-major (so libcrypto and libssl come
;; from the same install), then the FALLBACKS. LIBDIRS is a list: a string
;; would walk character by character and the explicit directory would be
;; silently lost.
(defn- lib-candidates [libdirs names fallbacks]
  (when (string? libdirs)
    (throw (ex-info "lib-candidates: libdirs is a list of directories" {:libdirs libdirs})))
  (into (vec (mapcat (fn [libdir] (map #(dir-join libdir %) names)) libdirs))
        fallbacks))

(def ^:private native-ready? (volatile! false))
;; {:crypto path :ssl path} once loaded — which of the candidates this machine
;; actually has — and, after a failed attempt, why it failed.
(def ^:private native-libs (volatile! nil))
(def ^:private native-failure (volatile! nil))

;; Windows sockets live in ws2_32.dll and need WSAStartup(2.2) once before any
;; socket call; POSIX sockets are process symbols, so jolt.winsock/ensure! is a
;; no-op there. This used to be a third private copy of that, alongside
;; jolt.nrepl's and the one jolt.socket did not have (jolt-lang/jolt#1107).

;; ffi/load-library takes the ordered candidates, answers the one that loaded
;; and raises naming every one it tried when none does.
(defn- load-native! []
  (winsock/ensure!)
  (let [libdirs (runtime-openssl-libdirs)
        crypto  (:path (ffi/load-library (lib-candidates libdirs crypto-names crypto-candidates)))
        ssl     (:path (ffi/load-library (lib-candidates libdirs ssl-names ssl-candidates)))]
    {:crypto crypto :ssl ssl}))

;; What the resolver's warning says when the transport did not load: the
;; loader's message names the candidates tried, and the remedy is the
;; directory variable — without it a Windows machine with no Git for Windows
;; reports only that the transport could not be loaded.
(defn- transport-load-error [cause]
  (str "the native TLS transport could not be loaded: " cause
       " (JOLT_OPENSSL_LIBDIR names the directory holding libcrypto and libssl)"))

(defn- ensure-native!
  "Lazy-load the native transport on first use: (Windows) ws2_32 + WSAStartup,
  then libcrypto then libssl. JOLT_OPENSSL_LIBDIR is read here, at fetch time,
  so baked app binaries and restored images honor the runtime environment.
  Returns true once ready; a later fetch retries."
  []
  (or @native-ready?
      (try
        (vreset! native-libs (load-native!))
        (vreset! native-failure nil)
        (vreset! native-ready? true)
        true
        (catch :default e
          (vreset! native-failure (or (ex-message e) (str e)))
          false))))

(defn loaded-native-libraries
  "The OpenSSL libraries the transport loaded, {:crypto path :ssl path} — which
  of the candidates this machine has — or nil before the first fetch or when
  none loaded. The HTTPS smoke asserts on it: a fetch that succeeded says
  nothing about which directory answered."
  []
  @native-libs)

;; --- sockets: jolt.socket.native, which carries the per-platform bindings,
;; numbers and struct layouts (including bionic's BSD-ordered addrinfo, #979).
;; ws2_32 + WSAStartup on Windows come from ensure-native! before the first call.

;; SO_RCVTIMEO/SO_SNDTIMEO bound every blocking socket call so a stalled or
;; malicious repo can't wedge dependency resolution forever (a timed-out recv
;; returns -1 -> recv-bytes throws -> fetch returns false).
(def ^:private socket-timeout-ms 30000)

(defn- set-timeouts! [fd ms]
  (native/set-timeout! fd native/so-rcvtimeo ms)
  (native/set-timeout! fd native/so-sndtimeo ms))

;; Every candidate failing used to be reported as "connection refused", which
;; is a guess: the exhaustion says only that the LAST connect failed, and for
;; the bionic bug above the real error was EFAULT. Report the code the kernel
;; gave, so a wrong sockaddr does not read as a network outage. strerror only
;; speaks errno, so the Windows path (GetLastError codes) shows the number.
(defn- connect-error-message [host port err]
  (str "could not connect to " host ":" port
       (cond
         (nil? err) ""
         (= err :timeout) (str " (no answer in " socket-timeout-ms "ms)")
         windows?   (str " (error " err ")")
         :else      (str " (errno " err ": " (ffi/errno-message err) ")"))))

(defn- connect-one
  "Connect fd to one resolved address within socket-timeout-ms: nil when
  connected, else the error (:timeout for none in time). The connect runs
  non-blocking and is waited for with poll — SO_SNDTIMEO bounds a connect only
  on Linux, and a black-holed address would otherwise hold each attempt for the
  kernel's SYN timeout (minutes) before the next address is tried."
  [fd a]
  (native/set-blocking! fd false)
  (let [[rc code] (native/c-connect fd (:addr a) (:addrlen a))
        err (cond
              (zero? rc) nil
              (not (native/connect-pending? code)) code
              :else
              (let [ev (native/poll-one fd native/pollout socket-timeout-ms)]
                (cond
                  (zero? ev) :timeout
                  (neg? ev) -1
                  :else (let [e (native/pending-error fd)] (when-not (zero? e) e)))))]
    (when-not err (native/set-blocking! fd true))
    err))

(defn- connect
  "Resolve host:port and open a connected TCP socket; return its fd. Every
  address the name resolves to is tried in order; the error reported when all
  fail is the last connect's."
  [host port]
  (let [{:keys [addrs error]} (native/resolve-addrs host port)]
    (when error
      (throw (ex-info (str "lookup failed: " host) {:host host})))
    (try
      (loop [[a & more] addrs err nil]
        (if (nil? a)
          (throw (ex-info (connect-error-message host port err)
                          {:host host :port port :error err}))
          (let [[fd e] (native/c-socket (:family a) (bit-or native/sock-stream (or native/sock-cloexec 0)) 0)]
            (if (neg? fd)
              (recur more (or err e))
              ;; close-on-exec before connect: a subprocess spawned while the
              ;; connect waits must not inherit the socket
              (let [_ (native/close-on-exec! fd)
                    code (connect-one fd a)]
                (if (nil? code)
                  (do (set-timeouts! fd socket-timeout-ms)
                      fd)
                  (do (native/c-close fd)
                      (recur more code))))))))
      (finally (native/free-addrs! addrs)))))

(def ^:private recv-bufsize 65536)

(defn- recv-bytes
  "Read up to one bufferful from fd: a byte-array, nil at EOF."
  [fd]
  (let [buf (ffi/alloc recv-bufsize)]
    (try
      (let [[got _] (native/c-recv fd buf recv-bufsize 0)]
        (cond (pos? got) (ffi/read-array buf got)
              (zero? got) nil
              :else (throw (ex-info "recv failed" {}))))
      (finally (ffi/free buf)))))

(defn- send-bytes [fd data]
  (let [n (alength data) buf (ffi/alloc (max 1 n))]
    (try
      (ffi/write-array buf data)
      (loop [off 0]
        (when (< off n)
          (let [[sent _] (native/c-send fd (+ buf off) (- n off) native/msg-nosignal)]
            (if (pos? sent) (recur (+ off sent))
                (throw (ex-info "send failed" {}))))))
      (finally (ffi/free buf)))))

(defn- close-sock [fd] (native/c-close fd) nil)

;; --- OpenSSL TLS client (memory-BIO) ---
(def ^:private WANT-READ 2)
(def ^:private WANT-WRITE 3)
(def ^:private VERIFY-PEER 1)
(def ^:private BIO-PENDING 10)
(def ^:private SET-TLSEXT-HOSTNAME 55)
(def ^:private NAMETYPE-host-name 0)
(def ^:private SET-MIN-PROTO-VERSION 123)
(def ^:private TLS1_2-VERSION 0x0303)
(def ^:private ssl-chunk 16384)

(ffi/defcfn c-TLS-client-method "TLS_client_method" [] :pointer)
(ffi/defcfn c-SSL-CTX-new        "SSL_CTX_new"        [:pointer] :pointer)
(ffi/defcfn c-SSL-CTX-free       "SSL_CTX_free"       [:pointer] :void)
(ffi/defcfn c-SSL-CTX-set-verify "SSL_CTX_set_verify" [:pointer :int :pointer] :void)
(ffi/defcfn c-SSL-CTX-default-verify "SSL_CTX_set_default_verify_paths" [:pointer] :int)
(ffi/defcfn c-SSL-new            "SSL_new"            [:pointer] :pointer)
(ffi/defcfn c-SSL-free           "SSL_free"           [:pointer] :void)
(ffi/defcfn c-SSL-set-bio        "SSL_set_bio"        [:pointer :pointer :pointer] :void)
(ffi/defcfn c-SSL-set-connect    "SSL_set_connect_state" [:pointer] :void)
(ffi/defcfn c-SSL-connect        "SSL_connect"        [:pointer] :int)
(ffi/defcfn c-SSL-read           "SSL_read"           [:pointer :pointer :int] :int)
(ffi/defcfn c-SSL-write          "SSL_write"          [:pointer :pointer :int] :int)
(ffi/defcfn c-SSL-get-error      "SSL_get_error"      [:pointer :int] :int)
(ffi/defcfn c-SSL-ctrl           "SSL_ctrl"           [:pointer :int :int64 :pointer] :int64)
(ffi/defcfn c-SSL-CTX-ctrl       "SSL_CTX_ctrl"       [:pointer :int :int64 :pointer] :int64)
(ffi/defcfn c-SSL-shutdown       "SSL_shutdown"       [:pointer] :int)
(ffi/defcfn c-SSL-set1-host      "SSL_set1_host"      [:pointer :pointer] :int)
(ffi/defcfn c-BIO-new            "BIO_new"            [:pointer] :pointer)
(ffi/defcfn c-BIO-s-mem          "BIO_s_mem"          [] :pointer)
(ffi/defcfn c-BIO-read           "BIO_read"           [:pointer :pointer :int] :int)
(ffi/defcfn c-BIO-write          "BIO_write"          [:pointer :pointer :int] :int)
(ffi/defcfn c-BIO-ctrl           "BIO_ctrl"           [:pointer :int :int64 :pointer] :int64)

(defn- cstr [s] (ffi/string->ptr (str s)))

(defn- bio-pending [bio] (c-BIO-ctrl bio BIO-PENDING 0 ffi/null))

;; A TLS stream is a plain map {:sock fd :ssl :ctx :rbio :wbio}.
;; Drain ciphertext OpenSSL produced into wbio out to the socket.
(defn- flush-out [st]
  (let [wbio (:wbio st) sock (:sock st)]
    (loop []
      (let [p (bio-pending wbio)]
        (when (pos? p)
          (let [buf (ffi/alloc p) n (c-BIO-read wbio buf p)]
            (when (pos? n) (send-bytes sock (ffi/read-array buf n)))
            (ffi/free buf) (recur)))))))

;; Pull one ciphertext chunk off the socket into rbio; false at EOF.
(defn- feed-in [st]
  (let [data (recv-bytes (:sock st))]
    (if (and data (pos? (alength data)))
      (let [n (alength data) buf (ffi/alloc n)]
        (ffi/write-array buf data)
        (c-BIO-write (:rbio st) buf n)
        (ffi/free buf) true)
      false)))

(defn- handshake! [st]
  (loop []
    (let [ret (c-SSL-connect (:ssl st))]
      (flush-out st)
      (when (not= ret 1)
        (let [err (c-SSL-get-error (:ssl st) ret)]
          (case err
            2 (if (feed-in st) (recur)
                  (throw (ex-info "connection closed during TLS handshake" {})))
            3 (recur)
            (throw (ex-info (str "TLS handshake failed (SSL_get_error=" err ")") {}))))))))

(defn- tls-write [st data]
  (let [n (alength data)
        buf (ffi/alloc (max 1 n))]
    (ffi/write-array buf data)
    (try
      (loop [off 0]
        (when (< off n)
          (let [wrote (c-SSL-write (:ssl st) (+ buf off) (- n off))]
            (flush-out st)
            (if (pos? wrote)
              (recur (+ off wrote))
              (let [err (c-SSL-get-error (:ssl st) wrote)]
                (if (or (= err 2) (= err 3))
                  (do (feed-in st) (recur off))
                  (throw (ex-info "TLS write failed" {}))))))))
      (finally (ffi/free buf)))))

;; One decrypted byte-array chunk, or nil at EOF.
(defn- tls-read [st]
  (let [tmp (ffi/alloc ssl-chunk)]
    (try
      (loop []
        (let [got (c-SSL-read (:ssl st) tmp ssl-chunk)]
          (if (pos? got) (ffi/read-array tmp got)
              (let [err (c-SSL-get-error (:ssl st) got)]
                (cond
                  (= err 2) (if (feed-in st) (recur) nil)
                  (= err 3) (do (flush-out st) (recur))
                  :else nil)))))
      (finally (ffi/free tmp)))))

(defn- tls-close [st]
  (try (c-SSL-shutdown (:ssl st)) (catch :default _ nil))
  (try (close-sock (:sock st)) (catch :default _ nil))
  (try (c-SSL-free (:ssl st)) (catch :default _ nil))
  (try (c-SSL-CTX-free (:ctx st)) (catch :default _ nil))
  nil)

(defn- tls-connect
  "Open a verified TLS client connection to host:port."
  [host port]
  ;; Open the socket FIRST: connect throws on DNS/refused (the common failure),
  ;; and doing it before any OpenSSL allocation means that path leaks nothing.
  (let [sock (connect host port)
        ctx  (c-SSL-CTX-new (c-TLS-client-method))]
    (when (ffi/null? ctx)
      (close-sock sock)
      (throw (ex-info "SSL_CTX_new failed" {})))
    (let [ssl  (c-SSL-new ctx)]
      (when (ffi/null? ssl)
        (c-SSL-CTX-free ctx) (close-sock sock)
        (throw (ex-info "SSL_new failed" {})))
      (let [rbio (c-BIO-new (c-BIO-s-mem))
            wbio (c-BIO-new (c-BIO-s-mem))
            st   {:sock sock :ssl ssl :ctx ctx :rbio rbio :wbio wbio}]
        ;; Hand the BIOs to the SSL up front — SSL_free then owns and frees them,
        ;; so tls-close in the catch below reclaims everything.
        (c-SSL-set-bio ssl rbio wbio)
        (try
          (when (zero? (c-SSL-CTX-default-verify ctx))
            (throw (ex-info "no CA trust store (SSL_CTX_set_default_verify_paths)" {})))
          (c-SSL-CTX-set-verify ctx VERIFY-PEER ffi/null)
          (c-SSL-CTX-ctrl ctx SET-MIN-PROTO-VERSION TLS1_2-VERSION ffi/null) ; TLS >= 1.2
          (c-SSL-set-connect ssl)
          (let [host-buf (cstr host)]
            (try
              (c-SSL-ctrl ssl SET-TLSEXT-HOSTNAME NAMETYPE-host-name host-buf) ; SNI
              (when-not (= 1 (c-SSL-set1-host ssl host-buf))                   ; hostname check
                (throw (ex-info "SSL_set1_host failed" {})))
              (finally (ffi/free host-buf))))
          (handshake! st)
          st
          (catch :default e (tls-close st) (throw e)))))))

;; --- HTTP/1.1 request/response ---
(defn- jolt-version []
  (or (System/getProperty "jolt.version") "jolt"))

;; A CR, LF, or NUL in a URL would let a crafted :mvn/version or a server
;; redirect Location inject a second request line / header (request smuggling).
(defn- ctl-free? [s]
  (not-any? (fn [c] (or (= c \return) (= c \newline) (= c (char 0)))) s))

(defn- parse-url
  "https://host[:port]/path[?query] -> {:host :port :path}. HTTPS only."
  [spec]
  (let [s (str spec)]
    (when-not (str/starts-with? s "https://")
      (throw (ex-info (str "jolt.mvn-http: HTTPS only, got " s) {:url s})))
    (let [after (subs s 8)
          slash (str/index-of after "/")
          authority (if slash (subs after 0 slash) after)
          path (if slash (subs after slash) "/")
          colon (str/index-of authority ":")
          host (if colon (subs authority 0 colon) authority)]
      (when-not (and (ctl-free? host) (ctl-free? path))
        (throw (ex-info "jolt.mvn-http: control character in URL" {:url s})))
      (if colon
        {:host host :port (or (parse-long (subs authority (inc colon))) 443) :path path}
        {:host host :port 443 :path path}))))

(defn- build-request [host port path]
  (let [host-hdr (if (= port 443) host (str host ":" port))]
    (str "GET " path " HTTP/1.1\r\n"
         "Host: " host-hdr "\r\n"
         "User-Agent: jolt/" (jolt-version) "\r\n"
         "Accept: */*\r\n"
         "Connection: close\r\n\r\n")))

(defn- recv-all [st]
  (loop [chunks []]
    (if-let [b (tls-read st)]
      (recur (conj chunks b))
      (byte-array (mapcat seq chunks)))))

;; Index of the first \r\n\r\n in the byte-array (the body start), else nil.
(defn- header-end [ba]
  (let [n (alength ba)]
    (loop [i 0]
      (if (> i (- n 4))
        nil
        (if (and (= (aget ba i) 13) (= (aget ba (inc i)) 10)
                 (= (aget ba (+ i 2)) 13) (= (aget ba (+ i 3)) 10))
          (+ i 4)
          (recur (inc i)))))))

(defn- subbytes [ba start end]
  (let [out (byte-array (- end start))]
    (dotimes [i (- end start)] (aset out i (aget ba (+ start i))))
    out))

(defn- header-ci [pairs name]
  (let [low (str/lower-case name)]
    (reduce (fn [v pair] (if (= low (str/lower-case (first pair))) (second pair) v)) nil pairs)))

(defn- parse-chunk-size [s]
  (let [semi (str/index-of s ";")
        s (if semi (subs s 0 semi) s)]
    (try (Long/parseLong (str/trim s) 16) (catch :default _ nil))))

;; Dechunk a Transfer-Encoding: chunked body. ISO-8859-1 is a byte-identity
;; map (each byte 0..255 <-> one char), so a binary chunk body survives exactly;
;; the chunk framing (hex size + CRLF) is ASCII.
(defn- dechunk-step [ba raw i]
  (let [crlf (str/index-of raw "\r\n" i)]
    (if (nil? crlf)
      [nil i]
      (let [sz (parse-chunk-size (subs raw i crlf))]
        (if (or (nil? sz) (<= sz 0))
          [nil i]
          (let [start (+ crlf 2)]
            [(subbytes ba start (+ start sz)) (+ start sz 2)]))))))

(defn- dechunk [ba]
  (let [raw (String. ba "ISO-8859-1")
        out (java.io.ByteArrayOutputStream.)]
    (loop [i 0]
      (let [[chunk next-i] (dechunk-step ba raw i)]
        (if (nil? chunk)
          (.toByteArray out)
          (do (.write out chunk) (recur next-i)))))))

(defn- parse-retry-after
  "Retry-After as whole seconds, or nil. Only the delta-seconds form is used; an
  HTTP-date would mean trusting this machine's clock against the server's."
  [v]
  (when v
    (when-let [n (parse-long (str/trim v))]
      (when-not (neg? n) n))))

(defn- parse-response
  "raw: the full response byte-array. Returns {:status :header-pairs :body}."
  [raw]
  (let [he (header-end raw)]
    (when (nil? he)
      (throw (ex-info "malformed response: no header terminator" {})))
    (let [header-ba (subbytes raw 0 (- he 4))
          body-ba (subbytes raw he (alength raw))
          s (String. header-ba "UTF-8")
          lines (str/split s #"\r\n")
          status-line (first lines)
          parts (str/split status-line #" ")
          status (or (parse-long (nth parts 1 ""))
                     (throw (ex-info (str "bad status line: " status-line) {})))
          pairs (vec (keep (fn [line]
                             (when-let [c (str/index-of line ":")]
                               [(str/trim (subs line 0 c)) (str/trim (subs line (inc c)))]))
                           (rest lines)))
          te (header-ci pairs "transfer-encoding")
          chunked? (and te (str/includes? (str/lower-case te) "chunked"))
          body (if chunked? (dechunk body-ba) body-ba)
          clen (when-let [c (header-ci pairs "content-length")] (parse-long (str/trim c)))]
      {:status status :header-pairs pairs :body body
       :retry-after (parse-retry-after (header-ci pairs "retry-after"))
       ;; a Content-Length only frames a non-chunked body; used to reject a
       ;; truncated download (a mid-body TLS drop reads as a short clean EOF).
       :content-length (when-not chunked? clen)})))

;; Resolve a redirect Location against the current base, HTTPS only. A redirect
;; to plain http:// is a downgrade and is refused outright (returns nil) rather
;; than silently followed — defense in depth, independent of parse-url.
(defn- resolve-location [base loc]
  (let [host-port (str (:host base)
                       (when-not (= (:port base) 443) (str ":" (:port base))))]
    (cond
      (str/starts-with? loc "http://")  nil
      (str/starts-with? loc "//")       (str "https:" loc)
      (str/starts-with? loc "https://") loc
      (str/starts-with? loc "/")        (str "https://" host-port loc)
      :else                             (str "https://" host-port "/" loc))))

;; Write via a temp file + atomic rename so a crash/partial write never leaves a
;; corrupt jar at the final path (which deps.clj would then trust and never
;; re-fetch).
(defn- write-bytes-to-file [path ba]
  (let [tmp (str path ".part")]
    (doto (java.io.FileOutputStream. tmp) (.write ba) (.close))
    (let [t (java.io.File. tmp) f (java.io.File. path)]
      (.delete f)
      (when-not (.renameTo t f)
        (.delete t)
        (throw (ex-info "rename of downloaded file failed" {:path path}))))))

(def ^:private max-redirects 5)
(def ^:private redirect-codes #{301 302 303 307 308})

;; --- outcome classification ---------------------------------------------------
;; A fetch used to answer one bit, and the caller (jolt.deps) printed "not found"
;; for it. Only ONE of the ways a fetch fails means that. The rest — a reset, a
;; TLS failure, a timeout, a 503 — say nothing about whether the artifact exists,
;; and reporting them as absent sent people looking for a missing dependency that
;; was published years ago. So an outcome names its reason:
;;
;;   :ok         2xx, body written
;;   :not-found  the repo answered, definitively, that it does not have this
;;   :retryable  a transient condition; the same request may well succeed
;;   :failed     a real failure that retrying cannot fix (cert, parse, 403)
;;
;; The split matters twice: it decides what to retry, and it decides what the
;; user is told.
(defn- classify-status [status]
  (cond
    (<= 200 status 299)               :ok
    (or (= status 404) (= status 410)) :not-found
    ;; 408 request timeout, 429 rate limited, 5xx server-side — the statuses a
    ;; retry exists for. Central and Clojars serve these under load.
    (or (= status 408) (= status 429) (<= 500 status 599)) :retryable
    ;; 401/403 and the rest: the repo answered and it was not a "no such
    ;; artifact". Retrying cannot help and neither can looking for a typo.
    :else                             :failed))

(def ^:private max-attempts 3)
;; Backoff between attempts. Deliberately short: this sits in front of a
;; developer waiting on a build, and the failures it covers (a reset, a brief
;; 503) clear in well under a second. A long backoff would trade a rare recovery
;; for a guaranteed stall on the genuinely-down case.
(def ^:private retry-backoff-ms [200 600])
;; A 429 is the exception: a rate limit lasts seconds, and Central hands them to
;; shared CI runners, so 800 ms of retries failed whole release runs. The
;; server's Retry-After wins when it sends one, capped so a bad header cannot
;; stall a build for an hour.
(def ^:private rate-limit-backoff-ms [2000 5000])
(def ^:private max-retry-after-ms 30000)

(defn- retry-delay-ms
  "How long to wait after attempt N failed with R."
  [r n]
  (if-let [secs (:retry-after r)]
    (min max-retry-after-ms (* 1000 secs))
    (let [backoff (if (= 429 (:status r)) rate-limit-backoff-ms retry-backoff-ms)]
      (nth backoff (dec n) (peek backoff)))))

(defn- with-retries
  "Call ATTEMPT until it returns a non-:retryable outcome, at most max-attempts
  times. Returns the last outcome — still :retryable when the cap is reached, so
  the caller reports 'could not fetch' rather than 'not found'."
  [attempt]
  (loop [n 1]
    (let [r (attempt)]
      (if (or (not= :retryable (:outcome r)) (>= n max-attempts))
        r
        (do (Thread/sleep (retry-delay-ms r n))
            (recur (inc n)))))))

(defn- fetch-once [url out-path]
  (let [{:keys [host port path]} (parse-url url)
        st (tls-connect host port)]
    (try
      (tls-write st (.getBytes (build-request host port path) "UTF-8"))
      (let [{:keys [status header-pairs body content-length retry-after]} (parse-response (recv-all st))
            loc (header-ci header-pairs "location")]
        (cond
          (and (redirect-codes status) loc)
          {:redirect (resolve-location {:host host :port port} loc)}
          ;; a Content-Length that disagrees with the body we read means the
          ;; transfer was cut short — don't write a truncated jar, and don't call
          ;; it absent: a mid-body drop is the clearest retry candidate there is.
          (and (<= 200 status 299) content-length (not= content-length (alength body)))
          {:outcome :retryable :status status :error "truncated body"}
          (<= 200 status 299) (do (write-bytes-to-file out-path body) {:outcome :ok :status status})
          :else (cond-> {:outcome (classify-status status) :status status}
                  retry-after (assoc :retry-after retry-after))))
      (finally (tls-close st)))))

(defn fetch*
  "GET `url` (HTTPS only) over a cert-verified TLS connection and write the
  response body as raw bytes to `out-path`. Follows up to 5 redirects, and
  retries a transient failure up to `max-attempts` times.

  Returns {:outcome :ok|:not-found|:retryable|:failed, :status n, :error s}.
  Only :ok wrote a file; a failed fetch never leaves a partial one."
  [url out-path]
  (if-not (ensure-native!)
    {:outcome :failed :error (transport-load-error @native-failure)}
    (with-retries
      (fn []
        ;; The catch stays — a fetch must not escape into dependency resolution
        ;; — but the condition is CARRIED now instead of discarded, so the
        ;; warning can name what went wrong. A connect/TLS/parse throw is
        ;; transient far more often than not, so it retries.
        (try
          (loop [u url redirects 0]
            (let [r (fetch-once u out-path)]
              (cond
                (:redirect r) (if (>= redirects max-redirects)
                                {:outcome :failed :error "too many redirects"}
                                (recur (:redirect r) (inc redirects)))
                :else r)))
          (catch :default e
            {:outcome :retryable :error (or (ex-message e) (str e))}))))))

(defn fetch
  "GET `url` to `out-path`; true when the body was written. The boolean face of
  `fetch*`, kept for callers that only need to know whether it worked."
  [url out-path]
  (= :ok (:outcome (fetch* url out-path))))
