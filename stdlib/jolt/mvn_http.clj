(ns jolt.mvn-http
  "Minimal cert-verifying HTTPS GET-to-file for dependency download. A plain
  TCP socket (getaddrinfo/socket/connect) carries ciphertext; TLS runs against
  in-memory BIOs over the system OpenSSL via jolt.ffi, so no raw fd reaches
  OpenSSL. libcrypto then libssl lazy-load on first use (candidate lists,
  Homebrew first on macOS). fetch returns true on a 2xx, false on any failure,
  so jolt.deps falls through to the next repo. HTTPS only; the server
  certificate is verified (default verify paths + VERIFY_PEER + hostname check).
  macOS and Linux are validated; the Windows path (ws2_32 + WSAStartup +
  closesocket) is implemented but not yet tested on a Windows host."
  (:require [jolt.ffi :as ffi]
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
(def ^:private crypto-candidates
  (cond
    macos?   ["/opt/homebrew/opt/openssl@3/lib/libcrypto.dylib"
              "/opt/homebrew/lib/libcrypto.dylib"
              "/usr/local/opt/openssl@3/lib/libcrypto.dylib"]
    windows? ["libcrypto-3-x64.dll" "libcrypto-3.dll" "libcrypto-1_1-x64.dll"]
    :else    ["libcrypto.so.3" "libcrypto.so.1.1" "libcrypto.so"]))

(def ^:private ssl-candidates
  (cond
    macos?   ["/opt/homebrew/opt/openssl@3/lib/libssl.dylib"
              "/opt/homebrew/lib/libssl.dylib"
              "/usr/local/opt/openssl@3/lib/libssl.dylib"]
    windows? ["libssl-3-x64.dll" "libssl-3.dll" "libssl-1_1-x64.dll"]
    :else    ["libssl.so.3" "libssl.so.1.1" "libssl.so"]))

(def ^:private native-ready? (volatile! false))

(defn- load-one [path]
  (try (ffi/load-library path) true (catch :default _ false)))

(defn- try-candidates [cs]
  (loop [cs cs]
    (cond (empty? cs) false
          (load-one (first cs)) true
          :else (recur (rest cs)))))

;; Windows sockets live in ws2_32.dll and need WSAStartup(2.2) once before any
;; socket call; POSIX sockets are process symbols, so this is a no-op there.
;; UNTESTED on Windows — no Windows machine was available to validate against.
(defn- init-sockets! []
  (if-not windows?
    true
    (when (or (load-one "ws2_32.dll") (load-one "ws2_32"))
      (let [wsadata (ffi/alloc 512)]
        (try (zero? (c-WSAStartup 0x0202 wsadata))
             (finally (ffi/free wsadata)))))))

(defn- ensure-native!
  "Lazy-load the native transport on first use: (Windows) ws2_32 + WSAStartup,
  then libcrypto then libssl. Returns true once ready; a later fetch retries."
  []
  (or @native-ready?
      (when (and (init-sockets!)
                 (try-candidates crypto-candidates)
                 (try-candidates ssl-candidates))
        (vreset! native-ready? true)
        true)))

;; --- BSD socket layer. On POSIX these are the process's own symbols (libc);
;; on Windows they live in ws2_32.dll (loaded, with WSAStartup, by ensure-native!
;; before the first call), which exports the same getaddrinfo/socket/connect/
;; recv/send names plus closesocket. ---
(ffi/defcfn c-socket      "socket"      [:int :int :int] :int)
(ffi/defcfn c-connect     "connect"     [:int :pointer :int] :int :blocking)
(ffi/defcfn c-close       "close"       [:int] :int)
(ffi/defcfn c-closesocket "closesocket" [:int] :int)              ; Windows sockets
(ffi/defcfn c-recv        "recv"        [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-send        "send"        [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-getaddrinfo "getaddrinfo" [:pointer :pointer :pointer :pointer] :int :blocking)
(ffi/defcfn c-freeaddrinfo "freeaddrinfo" [:pointer] :void)
(ffi/defcfn c-WSAStartup  "WSAStartup"  [:int :pointer] :int)     ; Windows Winsock init

;; struct addrinfo field offsets. Both macOS and Win64 place ai_addr at 32
;; (ai_canonname before it); Linux packs ai_addr at 24.
(def ^:private O-ai-family 4)
(def ^:private O-ai-socktype 8)
(def ^:private O-ai-protocol 12)
(def ^:private O-ai-addrlen 16)
(def ^:private O-ai-addr (if (or macos? windows?) 32 24))
(def ^:private O-ai-next 40)

(defn- connect
  "Resolve host:port and open a connected TCP socket; return its fd."
  [host port]
  (let [node (ffi/string->ptr (str host))
        service (ffi/string->ptr (str port))
        respp (ffi/alloc (ffi/sizeof :pointer))
        hints (ffi/alloc 48)]
    (dotimes [i 48] (ffi/write hints :uint8 i 0))
    ;; SOCK_STREAM in ai_socktype, else getaddrinfo also returns UDP entries
    ;; and connect() on a datagram socket spuriously "succeeds".
    (ffi/write hints :int O-ai-socktype 1)
    (try
      (let [rc (c-getaddrinfo node service hints respp)]
        (when-not (zero? rc)
          (throw (ex-info (str "lookup failed: " host) {:host host})))
        (let [res (ffi/read respp :pointer)]
          (try
            (loop [ai res]
              (if (ffi/null? ai)
                (throw (ex-info (str "connection refused: " host ":" port)
                                {:host host :port port}))
                (let [fam (ffi/read ai :int O-ai-family)
                      sockt (ffi/read ai :int O-ai-socktype)
                      proto (ffi/read ai :int O-ai-protocol)
                      addrlen (ffi/read ai :int O-ai-addrlen)
                      addr (ffi/read ai :pointer O-ai-addr)
                      fd (c-socket fam sockt proto)]
                  (cond
                    (neg? fd) (recur (ffi/read ai :pointer O-ai-next))
                    (zero? (c-connect fd addr addrlen)) fd
                    :else (do (c-close fd) (recur (ffi/read ai :pointer O-ai-next)))))))
            (finally (c-freeaddrinfo res)))))
      (finally (ffi/free node) (ffi/free service) (ffi/free respp) (ffi/free hints)))))

(def ^:private recv-bufsize 65536)

(defn- recv-bytes
  "Read up to one bufferful from fd: a byte-array, nil at EOF."
  [fd]
  (let [buf (ffi/alloc recv-bufsize)]
    (try
      (let [got (c-recv fd buf recv-bufsize 0)]
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
          (let [sent (c-send fd (+ buf off) (- n off) 0)]
            (if (pos? sent) (recur (+ off sent))
                (throw (ex-info "send failed" {}))))))
      (finally (ffi/free buf)))))

(defn- close-sock [fd] (if windows? (c-closesocket fd) (c-close fd)) nil)

;; --- OpenSSL TLS client (memory-BIO) ---
(def ^:private WANT-READ 2)
(def ^:private WANT-WRITE 3)
(def ^:private VERIFY-PEER 1)
(def ^:private BIO-PENDING 10)
(def ^:private SET-TLSEXT-HOSTNAME 55)
(def ^:private NAMETYPE-host-name 0)
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
  (let [ctx (c-SSL-CTX-new (c-TLS-client-method))]
    (when (ffi/null? ctx) (throw (ex-info "SSL_CTX_new failed" {})))
    (c-SSL-CTX-default-verify ctx)
    (c-SSL-CTX-set-verify ctx VERIFY-PEER ffi/null)
    (let [ssl (c-SSL-new ctx)
          rbio (c-BIO-new (c-BIO-s-mem))
          wbio (c-BIO-new (c-BIO-s-mem))
          host-buf (cstr host)]
      (c-SSL-set-bio ssl rbio wbio)
      (c-SSL-set-connect ssl)
      (c-SSL-ctrl ssl SET-TLSEXT-HOSTNAME NAMETYPE-host-name host-buf) ; SNI
      (c-SSL-set1-host ssl host-buf)                                   ; hostname check
      (ffi/free host-buf)
      (let [sock (connect host port)
            st {:sock sock :ssl ssl :ctx ctx :rbio rbio :wbio wbio}]
        (try (handshake! st)
             (catch :default e (tls-close st) (throw e)))
        st))))

;; --- HTTP/1.1 request/response ---
(defn- jolt-version []
  (or (System/getProperty "jolt.version") "jolt"))

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
          colon (str/index-of authority ":")]
      (if colon
        {:host (subs authority 0 colon)
         :port (or (parse-long (subs authority (inc colon))) 443)
         :path path}
        {:host authority :port 443 :path path}))))

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
          body (if (and te (str/includes? (str/lower-case te) "chunked"))
                 (dechunk body-ba) body-ba)]
      {:status status :header-pairs pairs :body body})))

(defn- resolve-location [base loc]
  (let [host-port (str (:host base)
                       (when-not (= (:port base) 443) (str ":" (:port base))))]
    (cond
      (str/starts-with? loc "//")       (str "https:" loc)
      (str/starts-with? loc "https://") loc
      (str/starts-with? loc "http://")  loc
      (str/starts-with? loc "/")        (str "https://" host-port loc)
      :else                             (str "https://" host-port "/" loc))))

(defn- write-bytes-to-file [path ba]
  (doto (java.io.FileOutputStream. path) (.write ba) (.close)))

(def ^:private max-redirects 5)

(defn- fetch-once [url out-path]
  (let [{:keys [host port path]} (parse-url url)
        st (tls-connect host port)]
    (try
      (tls-write st (.getBytes (build-request host port path) "UTF-8"))
      (let [{:keys [status header-pairs body]} (parse-response (recv-all st))]
        (cond
          (and (#{301 302 307 308} status) (header-ci header-pairs "location"))
          {:redirect (resolve-location {:host host :port port}
                                        (header-ci header-pairs "location"))}
          (<= 200 status 299) (do (write-bytes-to-file out-path body) {:result true})
          :else {:result false}))
      (finally (tls-close st)))))

(defn fetch
  "GET `url` (HTTPS only) over a cert-verified TLS connection and write the
  response body as raw bytes to `out-path`. Follows up to 5 redirects. Returns
  true on a 2xx final status, false/nil on any failure (DNS, connect, TLS,
  cert, parse, non-2xx). A failed fetch never leaves a partial file."
  [url out-path]
  (try
    (if-not (ensure-native!) false
            (loop [u url redirects 0]
              (let [r (fetch-once u out-path)]
                (cond
                  (:redirect r) (if (>= redirects max-redirects)
                                  false
                                  (recur (:redirect r) (inc redirects)))
                  (contains? r :result) (:result r)
                  :else false))))
    (catch :default _ false)))
