(ns jolt.mvn-http
  "Cert-verifying HTTPS GET-to-file for dependency download. A minimal HTTP/1.1
  client over BSD sockets (jolt.ffi) and the system OpenSSL via memory-BIO TLS:
  VERIFY_PEER + the default verify paths + SNI + SSL_set1_host (hostname check).
  Follows redirects (max 5), handles Content-Length / chunked / read-to-EOF
  bodies, and writes the body as raw bytes.

  libssl + libcrypto load on first use; if either is absent, or any step errors
  (DNS / connect / TLS / cert / parse), fetch returns false — a failed download
  never crashes dependency resolution. Plain HTTP is rejected: a jar is
  executable, so it is not fetched over unauthenticated transport."
  (:require [jolt.ffi :as ffi]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; --- platform + lazy library load -------------------------------------------
;; ffi/loaded? loads the shared object (a guarded load-shared-object) and reports
;; success, so trying each candidate both probes and loads. libssl depends on
;; libcrypto symbols, so crypto loads first.
(def ^:private os-name
  (str/lower-case (or (System/getProperty "os.name") "")))
(def ^:private macos? (str/includes? os-name "mac"))

(def ^:private ssl-candidates
  (if macos?
    ["libssl.dylib" "libssl.35.dylib" "libssl.3.dylib" "libssl.46.dylib"]
    ["libssl.so.3" "libssl.so.1.1" "libssl.so"]))
(def ^:private crypto-candidates
  (if macos?
    ["libcrypto.dylib" "libcrypto.35.dylib" "libcrypto.3.dylib" "libcrypto.46.dylib"]
    ["libcrypto.so.3" "libcrypto.so.1.1" "libcrypto.so"]))

(def ^:private ssl-ready? (atom false))

(defn- load-candidates [cands]
  (boolean (some #(ffi/loaded? %) cands)))

(defn- ensure-ssl-loaded! []
  (or @ssl-ready?
      (when (and (load-candidates crypto-candidates)
                 (load-candidates ssl-candidates))
        (reset! ssl-ready? true)
        true)))

;; Any internal failure throws; fetch catches everything and returns false, so
;; the message is for diagnostics only.
(defn- fail [msg] (throw (ex-info (str "jolt.mvn-http: " msg) {})))

;; --- BSD sockets (libc; symbols resolve from the running process) -----------
;; accept/recv/send/connect/getaddrinfo are :blocking so a parked socket call
;; never pins jolt's stop-the-world collector.
(ffi/defcfn c-socket       "socket"       [:int :int :int] :int)
(ffi/defcfn c-connect      "connect"      [:int :pointer :int] :int :blocking)
(ffi/defcfn c-close        "close"        [:int] :int)
(ffi/defcfn c-recv         "recv"         [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-send         "send"         [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-getaddrinfo  "getaddrinfo"  [:pointer :pointer :pointer :pointer] :int :blocking)
(ffi/defcfn c-freeaddrinfo "freeaddrinfo" [:pointer] :void)

;; struct addrinfo field offsets (LP64). macOS swaps ai_canonname/ai_addr versus
;; Linux, so ai_addr sits at 32 on macOS, 24 on Linux. ai_addrlen=16, ai_next=40.
(def ^:private O-ai-family 4)
(def ^:private O-ai-socktype 8)
(def ^:private O-ai-protocol 12)
(def ^:private O-ai-addrlen 16)
(def ^:private O-ai-addr (if macos? 32 24))
(def ^:private O-ai-next 40)

(defn- connect
  "Resolve host:port and open a connected TCP socket; return its fd."
  [host port]
  (let [node    (ffi/string->ptr (str host))
        service (ffi/string->ptr (str port))
        respp   (ffi/alloc (ffi/sizeof :pointer))
        ;; hints: ai_socktype = SOCK_STREAM, else getaddrinfo also returns UDP
        ;; entries and connect() on a datagram socket spuriously "succeeds".
        hints   (ffi/alloc 48)]
    (dotimes [i 48] (ffi/write hints :uint8 i 0))
    (ffi/write hints :int O-ai-socktype 1)
    (try
      (let [rc (c-getaddrinfo node service hints respp)]
        (when-not (zero? rc) (fail (str "unknown host: " host)))
        (let [res (ffi/read respp :pointer)]
          (try
            (loop [ai res]
              (if (ffi/null? ai)
                (fail (str "connection refused: " host ":" port))
                (let [fam     (ffi/read ai :int O-ai-family)
                      sockt   (ffi/read ai :int O-ai-socktype)
                      proto   (ffi/read ai :int O-ai-protocol)
                      addrlen (ffi/read ai :int O-ai-addrlen)
                      addr    (ffi/read ai :pointer O-ai-addr)
                      fd      (c-socket fam sockt proto)]
                  (cond
                    (neg? fd) (recur (ffi/read ai :pointer O-ai-next))
                    (zero? (c-connect fd addr addrlen)) fd
                    :else (do (c-close fd) (recur (ffi/read ai :pointer O-ai-next)))))))
            (finally (c-freeaddrinfo res)))))
      (finally (ffi/free node) (ffi/free service) (ffi/free respp) (ffi/free hints)))))

(def ^:private bufsize 65536)

(defn- recv-bytes
  "Read up to one bufferful from fd: a byte-array, or nil at EOF (recv 0)."
  [fd]
  (let [buf (ffi/alloc bufsize)]
    (try
      (let [got (c-recv fd buf bufsize 0)]
        (cond
          (pos? got) (ffi/read-array buf got)
          (zero? got) nil
          :else (fail "read timed out")))
      (finally (ffi/free buf)))))

(defn- send-bytes
  "Send all of byte-array data over fd."
  [fd data]
  (let [n (alength data) buf (ffi/alloc (max 1 n))]
    (try
      (ffi/write-array buf data)
      (loop [off 0]
        (when (< off n)
          (let [sent (c-send fd (+ buf off) (- n off) 0)]
            (if (pos? sent)
              (recur (+ off sent))
              (fail "send failed")))))
      (finally (ffi/free buf)))))

(defn- close [fd] (c-close fd) nil)

;; --- OpenSSL TLS (client, memory-BIO over the socket above) -----------------
;; SSL runs against in-memory BIOs while ciphertext is shuttled over the plain
;; socket, so no raw-fd access into OpenSSL is needed.
(def ^:private WANT-READ 2)
(def ^:private WANT-WRITE 3)
(def ^:private VERIFY-PEER 1)
(def ^:private BIO-PENDING 10)
(def ^:private SET-TLSEXT-HOSTNAME 55)
(def ^:private NAMETYPE-host-name 0)
(def ^:private chunk 16384)

(ffi/defcfn c-TLS-client-method  "TLS_client_method"              [] :pointer)
(ffi/defcfn c-SSL-CTX-new        "SSL_CTX_new"                    [:pointer] :pointer)
(ffi/defcfn c-SSL-CTX-free       "SSL_CTX_free"                   [:pointer] :void)
(ffi/defcfn c-SSL-CTX-set-verify "SSL_CTX_set_verify"             [:pointer :int :pointer] :void)
(ffi/defcfn c-SSL-CTX-default-verify "SSL_CTX_set_default_verify_paths" [:pointer] :int)
(ffi/defcfn c-SSL-new            "SSL_new"                        [:pointer] :pointer)
(ffi/defcfn c-SSL-free           "SSL_free"                       [:pointer] :void)
(ffi/defcfn c-SSL-set-bio        "SSL_set_bio"                    [:pointer :pointer :pointer] :void)
(ffi/defcfn c-SSL-set-connect    "SSL_set_connect_state"          [:pointer] :void)
(ffi/defcfn c-SSL-connect        "SSL_connect"                    [:pointer] :int)
(ffi/defcfn c-SSL-read           "SSL_read"                       [:pointer :pointer :int] :int)
(ffi/defcfn c-SSL-write          "SSL_write"                      [:pointer :pointer :int] :int)
(ffi/defcfn c-SSL-get-error      "SSL_get_error"                  [:pointer :int] :int)
(ffi/defcfn c-SSL-ctrl           "SSL_ctrl"                       [:pointer :int :int64 :pointer] :int64)
(ffi/defcfn c-SSL-shutdown       "SSL_shutdown"                   [:pointer] :int)
(ffi/defcfn c-SSL-set1-host      "SSL_set1_host"                  [:pointer :pointer] :int)
(ffi/defcfn c-BIO-new            "BIO_new"                        [:pointer] :pointer)
(ffi/defcfn c-BIO-s-mem          "BIO_s_mem"                      [] :pointer)
(ffi/defcfn c-BIO-read           "BIO_read"                       [:pointer :pointer :int] :int)
(ffi/defcfn c-BIO-write          "BIO_write"                      [:pointer :pointer :int] :int)
(ffi/defcfn c-BIO-ctrl           "BIO_ctrl"                       [:pointer :int :int64 :pointer] :int64)

;; A NUL-terminated C-string pointer; the caller frees it.
(defn- cstr [s] (ffi/string->ptr (str s)))
(defn- bio-pending [bio] (c-BIO-ctrl bio BIO-PENDING 0 ffi/null))

;; Pull one ciphertext chunk off the socket into rbio; false at EOF.
(defn- feed-in [st]
  (let [data (recv-bytes (jolt.host/ref-get st :sock))]
    (if (and data (pos? (alength data)))
      (let [n (alength data) buf (ffi/alloc n)]
        (ffi/write-array buf data)
        (c-BIO-write (jolt.host/ref-get st :rbio) buf n)
        (ffi/free buf)
        true)
      false)))

(defn- handshake! [st]
  (loop []
    (let [ret (c-SSL-connect (jolt.host/ref-get st :ssl))]
      ((jolt.host/ref-get st :flush) st)
      (when-not (= ret 1)
        (let [err (c-SSL-get-error (jolt.host/ref-get st :ssl) ret)]
          (cond
            (= err WANT-READ) (do (when-not (feed-in st)
                                    (fail "connection closed during TLS handshake"))
                                  (recur))
            (= err WANT-WRITE) (recur)
            :else (fail (str "TLS handshake failed (SSL_get_error=" err ")"))))))))

;; A TLS stream is a host tagged-table carrying :write / :read / :close closures
;; and the shared ssl/ctx/bio/eof state they mutate.
(defn- make-stream [sock ssl ctx rbio wbio]
  (let [st (jolt.host/tagged-table :jolt/tls-stream)]
    (jolt.host/ref-put! st :sock sock) (jolt.host/ref-put! st :ssl ssl)
    (jolt.host/ref-put! st :ctx ctx) (jolt.host/ref-put! st :rbio rbio)
    (jolt.host/ref-put! st :wbio wbio) (jolt.host/ref-put! st :eof false)
    ;; Drain ciphertext OpenSSL produced into wbio out to the socket.
    (jolt.host/ref-put! st :flush
      (fn [self]
        (let [wbio (jolt.host/ref-get self :wbio)]
          (loop []
            (let [p (bio-pending wbio)]
              (when (pos? p)
                (let [buf (ffi/alloc p) n (c-BIO-read wbio buf p)]
                  (when (pos? n) (send-bytes (jolt.host/ref-get self :sock) (ffi/read-array buf n)))
                  (ffi/free buf)
                  (recur))))))))
    (jolt.host/ref-put! st :write
      (fn [self data]
        (let [n (alength data) buf (ffi/alloc (max 1 n))]
          (ffi/write-array buf data)
          (try
            (loop [off 0]
              (when (< off n)
                (let [wrote (c-SSL-write (jolt.host/ref-get self :ssl) (+ buf off) (- n off))]
                  ((jolt.host/ref-get self :flush) self)
                  (if (pos? wrote)
                    (recur (+ off wrote))
                    (let [err (c-SSL-get-error (jolt.host/ref-get self :ssl) wrote)]
                      (cond
                        (= err WANT-READ) (do (feed-in self) (recur off))
                        (= err WANT-WRITE) (recur off)
                        :else (fail "TLS write failed")))))))
            (finally (ffi/free buf)))
          self)))
    (jolt.host/ref-put! st :read
      ;; return a decrypted byte-array chunk, or nil at EOF.
      (fn [self _timeout]
        (when-not (jolt.host/ref-get self :eof)
          (let [tmp (ffi/alloc chunk)]
            (try
              (loop []
                (let [got (c-SSL-read (jolt.host/ref-get self :ssl) tmp chunk)]
                  (if (pos? got)
                    (ffi/read-array tmp got)
                    (let [err (c-SSL-get-error (jolt.host/ref-get self :ssl) got)]
                      (cond
                        (= err WANT-READ) (if (feed-in self) (recur)
                                              (do (jolt.host/ref-put! self :eof true) nil))
                        (= err WANT-WRITE) (do ((jolt.host/ref-get self :flush) self) (recur))
                        :else (do (jolt.host/ref-put! self :eof true) nil))))))
              (finally (ffi/free tmp)))))))
    (jolt.host/ref-put! st :close
      (fn [& _]
        (try (c-SSL-shutdown ssl) (catch Throwable _ nil))
        (try (close sock) (catch Throwable _ nil))
        (try (c-SSL-free ssl) (catch Throwable _ nil))
        (try (c-SSL-CTX-free ctx) (catch Throwable _ nil))
        nil))
    st))

(defn- tls-connect
  "Open a TLS client connection to host:port with peer verification (default
  verify paths + VERIFY_PEER) and SNI. The server certificate and hostname are
  checked; a verification failure fails the handshake."
  [host port]
  (let [ctx (c-SSL-CTX-new (c-TLS-client-method))]
    (when (ffi/null? ctx) (fail "SSL_CTX_new failed"))
    (c-SSL-CTX-default-verify ctx)
    (c-SSL-CTX-set-verify ctx VERIFY-PEER ffi/null)
    (let [ssl     (c-SSL-new ctx)
          memmeth (c-BIO-s-mem)
          rbio    (c-BIO-new memmeth)
          wbio    (c-BIO-new memmeth)
          host-buf (cstr host)]
      (c-SSL-set-bio ssl rbio wbio)
      (c-SSL-set-connect ssl)
      (c-SSL-ctrl ssl SET-TLSEXT-HOSTNAME NAMETYPE-host-name host-buf)  ; SNI
      (c-SSL-set1-host ssl host-buf)                                    ; hostname check
      (let [sock (connect host port)
            st   (make-stream sock ssl ctx rbio wbio)]
        (try (handshake! st)
             (catch Throwable e
               ((jolt.host/ref-get st :close)) (ffi/free host-buf) (throw e)))
        (ffi/free host-buf)
        st))))

;; --- HTTP/1.1 client --------------------------------------------------------
(defn- min-idx [s chars]
  (reduce (fn [best ch] (if-let [i (str/index-of s (str ch))] (min best i) best))
          (count s) chars))

(defn- parse-url [spec]
  (let [s (str spec) colon (str/index-of s ":")]
    (when (or (nil? colon) (= colon 0) (str/index-of (subs s 0 colon) "/"))
      (fail (str "no protocol: " s)))
    (let [scheme (subs s 0 colon) after-colon (subs s (inc colon))]
      (when-not (str/starts-with? after-colon "//")
        (fail (str "not an absolute URL: " s)))
      (let [rest     (subs after-colon 2)
            auth-end (min-idx rest [\/ \? \#])
            authority (subs rest 0 auth-end)
            after    (subs rest auth-end)
            pc       (str/index-of authority ":")
            host     (if pc (subs authority 0 pc) authority)
            port     (if pc (or (parse-long (subs authority (inc pc))) -1) -1)
            q        (str/index-of after "?")]
        {:scheme scheme :host host :port port
         :path (if q (subs after 0 q) after)
         :query (when q (subs after (inc q)))}))))

(defn- default-port? [url] (or (= (:port url) -1) (= (:port url) 443)))
(defn- effective-port [url]
  (let [p (:port url)] (if (and (number? p) (>= p 0)) p 443)))
(defn- host-port [url]
  (let [p (:port url)] (if (and (number? p) (>= p 0)) (str (:host url) ":" p) (:host url))))

(defn- build-request [url]
  (let [path (let [p (:path url) q (:query url)]
               (str (if (or (nil? p) (= "" p)) "/" p) (if q (str "?" q) "")))
        ua   (str "jolt/" (or (System/getProperty "jolt.version") "dev"))
        host-hdr (if (default-port? url) (:host url) (str (:host url) ":" (effective-port url)))]
    (byte-array (.getBytes (str "GET " path " HTTP/1.1\r\n"
                                "Host: " host-hdr "\r\n"
                                "User-Agent: " ua "\r\n"
                                "Accept: */*\r\n"
                                "Connection: close\r\n\r\n")
                           "UTF-8"))))

;; bytes flow as jolt byte-arrays; a latin1 round-trip is byte-exact (one char
;; per byte 0-255), so the header/chunk parsing below is binary-safe.
(defn- ba->latin1 [ba] (String. ba "ISO-8859-1"))
(defn- latin1->ba [s] (byte-array (map int s)))

(defn- header-ci [pairs name]
  (let [low (str/lower-case name)]
    (reduce (fn [v pair] (if (= low (str/lower-case (first pair))) (second pair) v)) nil pairs)))

(defn- dechunk
  "raw: latin1 string of a chunked body. Returns the dechunked latin1 string."
  [raw]
  (loop [i 0 out (StringBuilder.)]
    (if (>= i (count raw))
      (.toString out)
      (let [crlf (str/index-of raw "\r\n" i)]
        (if (nil? crlf)
          (.toString out)
          (let [line (subs raw i crlf)
                semi (str/index-of line ";")
                line (if semi (subs line 0 semi) line)
                sz (try (Long/parseLong (str/trim line) 16) (catch Throwable _ nil))]
            (if (or (nil? sz) (<= sz 0))
              (.toString out)
              (let [start (+ crlf 2)
                    end   (min (count raw) (+ start sz))]
                (.append out (subs raw start end))
                (recur (+ start sz 2) out)))))))))

(defn- parse-response
  "raw: the full response byte-array. Returns {:status :header-pairs :body}."
  [raw]
  (let [s   (ba->latin1 raw)
        end (str/index-of s "\r\n\r\n")]
    (when (nil? end) (fail "malformed response: no header terminator"))
    (let [head        (subs s 0 end)
          body-raw    (subs s (+ end 4))
          lines       (str/split head #"\r\n")
          status-line (first lines)
          parts       (str/split status-line #" ")
          status      (or (parse-long (nth parts 1 ""))
                          (fail (str "bad status line: " status-line)))
          pairs       (vec (keep (fn [line]
                                   (when-let [c (str/index-of line ":")]
                                     [(str/trim (subs line 0 c)) (str/trim (subs line (inc c)))]))
                                 (rest lines)))
          te          (header-ci pairs "transfer-encoding")
          body        (if (and te (str/includes? (str/lower-case te) "chunked"))
                        (dechunk body-raw) body-raw)]
      {:status status :header-pairs pairs :body (latin1->ba body)})))

(defn- recv-all [stream]
  (loop [chunks []]
    (if-let [b ((jolt.host/ref-get stream :read) stream nil)]
      (recur (conj chunks b))
      (byte-array (mapcat seq chunks)))))

(defn- stream-write [stream data] ((jolt.host/ref-get stream :write) stream data))
(defn- stream-close [stream] ((jolt.host/ref-get stream :close)))

(def ^:private redirect-statuses #{301 302 303 307 308})

(defn- resolve-location [base loc]
  (cond
    (or (str/starts-with? loc "http://") (str/starts-with? loc "https://"))
    (parse-url loc)
    (str/starts-with? loc "//")
    (parse-url (str (:scheme base) ":" loc))
    (str/starts-with? loc "/")
    (parse-url (str (:scheme base) "://" (host-port base) loc))
    :else
    (parse-url (str (:scheme base) "://" (host-port base) "/" loc))))

(defn- do-fetch
  "GET start-url over TLS, following redirects (max 5). Returns the final
  parsed response map."
  [start-url]
  (loop [url start-url redirects 0]
    ;; https only — a redirect to plain http is rejected, not followed.
    (when-not (= (:scheme url) "https") (fail "redirect to non-https"))
    (let [stream (tls-connect (:host url) (effective-port url))
          resp   (try
                   (stream-write stream (build-request url))
                   (parse-response (recv-all stream))
                   (finally (try (stream-close stream) (catch Throwable _ nil))))
          loc    (header-ci (:header-pairs resp) "location")]
      (if (and (redirect-statuses (:status resp)) loc (< redirects 5))
        (recur (resolve-location url loc) (inc redirects))
        resp))))

;; --- public entry point -----------------------------------------------------
(defn fetch
  "HTTPS GET `url` and write the response body as raw bytes to `out-path`.
  Verifies the server certificate. Returns true on a 2xx final status, false on
  any failure — a non-https URL, missing libssl/libcrypto, a DNS/connect/TLS/
  cert/parse error, or a non-2xx status. No partial file is written on failure."
  [url out-path]
  (try
    (let [u (parse-url url)]
      (when (= (:scheme u) "https")
        (when (ensure-ssl-loaded!)
          (let [resp (do-fetch u)]
            (when (and (number? (:status resp)) (<= 200 (:status resp) 299))
              (io/copy (:body resp) out-path)
              true)))))
    (catch Throwable _ false)))
