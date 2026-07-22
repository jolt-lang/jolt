(ns jolt.nrepl
  "A minimal, extensible nREPL server for jolt, so an editor (CIDER / Calva /
  Cursive) can connect and develop a project live. Speaks bencode over a loopback
  TCP socket bound through jolt.ffi. Built in: clone, describe, eval, load-file,
  close — enough to connect and eval, with the project's deps on the roots and
  native libs loaded (jolt.main applies the project first), so (require '[lib])
  works.

  EXTENSIBLE: a library can add the heavier nREPL features (sessions,
  interruptible-eval, completion, lookup) as MIDDLEWARE without bloating core. A
  middleware is `(fn [handler] (fn [request] ...))`; list them in deps.edn under
  :nrepl/middleware (symbols resolving to a middleware fn, or to a vector of them)
  and jolt.nrepl composes them over the built-in handler. The request is the
  decoded bencode map (string keys: \"op\" \"code\" \"ns\" \"id\" \"session\" …)
  plus :reply — a thread-safe (fn [response-map]) that adds id/session and sends.
  Public seam for middleware: respond, evaluate, register-ops!, new-session.

  Writes .nrepl-port in the project dir so editors auto-detect the port."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [jolt.ffi :as ffi]
            [jolt.analyzer :as ana]))

;; --- sockets (loopback server) ---------------------------------------------
(def ^:private os-name
  (str/lower-case (or (System/getProperty "os.name") "")))
(def ^:private macos?   (str/includes? os-name "mac"))
(def ^:private windows? (str/includes? os-name "win"))

;; Load the library that provides the socket symbols BEFORE the foreign-fn
;; bindings below — defcfn resolves the C entry point when the def is evaluated
;; (at ns load), so the symbols must already be available. POSIX: the running
;; process's own libc symbols. Windows: the Winsock DLL (ws2_32), whose symbols
;; are NOT in joltc.exe's export table even though it's linked in — without this
;; explicit load, (ffi/defcfn c-socket "socket" ...) fails at load with
;; "no entry for socket".
(if windows?
  (ffi/load-library "ws2_32.dll")
  (ffi/load-library))

;; A socket is an int fd on POSIX; on Win64 it's a SOCKET (uintptr_t) handle, but
;; those are small kernel handle values that round-trip through :int, and the
;; INVALID_SOCKET error sentinel (~0) reads back as -1 — so the fd checks below
;; work unchanged on both.
(ffi/defcfn c-socket     "socket"     [:int :int :int] :int)
(ffi/defcfn c-bind       "bind"       [:int :pointer :int] :int)
(ffi/defcfn c-listen     "listen"     [:int :int] :int)
(ffi/defcfn c-setsockopt "setsockopt" [:int :int :int :pointer :int] :int)
(ffi/defcfn c-accept     "accept"     [:int :pointer :pointer] :int :blocking)

;; recv/send and the socket-close call differ by platform. Winsock's recv/send
;; take an int length and return int (not ssize_t), and a socket is closed with
;; closesocket, not close. A symbol that exists on only one OS (closesocket on
;; Windows, close on POSIX) can only be bound there, so these live in the taken
;; platform branch — jolt interns the vars from both branches at analysis time,
;; so later references resolve either way.
(if windows?
  (do
    (ffi/defcfn c-recv  "recv"        [:int :pointer :int :int] :int :blocking)
    (ffi/defcfn c-send  "send"        [:int :pointer :int :int] :int :blocking)
    (ffi/defcfn c-close "closesocket" [:int] :int)
    ;; Winsock must be initialized once per process before any socket call.
    (ffi/defcfn c-wsastartup "WSAStartup" [:int :pointer] :int))
  (do
    (ffi/defcfn c-recv  "recv"  [:int :pointer :size_t :int] :ssize_t :blocking)
    (ffi/defcfn c-send  "send"  [:int :pointer :size_t :int] :ssize_t :blocking)
    (ffi/defcfn c-close "close" [:int] :int)))

(def ^:private AF-INET 2)
(def ^:private SOCK-STREAM 1)
;; SOL_SOCKET / SO_REUSEADDR: 0xffff / 4 on macOS and Windows, 1 / 2 on Linux.
(def ^:private sol-socket (if (or macos? windows?) 0xffff 1))
(def ^:private so-reuse   (if (or macos? windows?) 4 2))

;; Initialize Winsock (a no-op off Windows). WSAStartup is refcounted and must
;; precede any socket call; WSADATA is ~408 bytes on x64, so 512 is ample.
(defn- ensure-winsock! []
  (when windows?
    (let [wsadata (ffi/alloc 512)]
      (try
        (let [r (c-wsastartup 0x0202 wsadata)]
          (when-not (zero? r)
            (throw (ex-info (str "WSAStartup failed: " r) {}))))
        (finally (ffi/free wsadata))))))

(defn- make-sockaddr [port]
  (let [sa (ffi/alloc 16)]
    (dotimes [i 16] (ffi/write sa :uint8 i 0))
    (if macos?
      (do (ffi/write sa :uint8 0 16) (ffi/write sa :uint8 1 AF-INET))
      (ffi/write sa :uint8 0 AF-INET))
    (ffi/write sa :uint8 2 (bit-and (bit-shift-right port 8) 0xff))
    (ffi/write sa :uint8 3 (bit-and port 0xff))
    (ffi/write sa :uint8 4 127) (ffi/write sa :uint8 7 1)   ; 127.0.0.1
    sa))

(defn- listen-socket [port]
  (ensure-winsock!)                                          ; no-op off Windows
  (let [fd (c-socket AF-INET SOCK-STREAM 0)]
    (when (neg? fd) (throw (ex-info "socket() failed" {})))
    (let [opt (ffi/alloc 4)] (ffi/write opt :int 0 1) (c-setsockopt fd sol-socket so-reuse opt 4) (ffi/free opt))
    (let [sa (make-sockaddr port)]
      (when (neg? (c-bind fd sa 16)) (c-close fd) (ffi/free sa) (throw (ex-info (str "bind() failed on port " port) {})))
      (ffi/free sa))
    (when (neg? (c-listen fd 16)) (c-close fd) (throw (ex-info "listen() failed" {})))
    fd))

;; bytes flow as latin1 strings on the wire (1 char = 1 byte). Text fields that
;; may carry unicode (code / value / out) convert at the boundary.
(defn- ->wire [s] (String. (.getBytes (str s) "UTF-8") "ISO-8859-1"))
(defn- wire-> [s] (String. (byte-array (map int s)) "UTF-8"))

(def ^:private bufsize 65536)
(defn- recv-str [fd]
  (let [buf (ffi/alloc bufsize)]
    (try (let [n (c-recv fd buf bufsize 0)]
           (when (pos? n) (String. (ffi/read-array buf n) "ISO-8859-1")))
         (finally (ffi/free buf)))))

(defn- send-str [fd s]
  (let [data (byte-array (map int s)) n (alength data) buf (ffi/alloc (max 1 n))]
    (try (ffi/write-array buf data)
         (loop [off 0] (when (< off n) (let [sent (c-send fd (+ buf off) (- n off) 0)]
                                         (when (pos? sent) (recur (+ off sent))))))
         (finally (ffi/free buf)))))

;; --- bencode ---------------------------------------------------------------
(defn- bencode [v]
  (cond
    (integer? v) (str "i" v "e")
    (string? v)  (let [w (->wire v)] (str (count w) ":" w))
    (keyword? v) (let [w (->wire (name v))] (str (count w) ":" w))
    (map? v)     (str "d" (apply str (mapcat (fn [[k val]] [(bencode (name k)) (bencode val)])
                                             (sort-by #(name (first %)) v))) "e")
    (or (seq? v) (vector? v)) (str "l" (apply str (map bencode v)) "e")
    (nil? v)     "0:"
    :else        (let [w (->wire (str v))] (str (count w) ":" w))))

;; decode one value from `s` at index `i` -> [value next-index], or nil if the
;; buffer doesn't yet hold a complete value.
(defn- bdecode [s i]
  (when (< i (count s))
    (let [c (nth s i)]
      (cond
        (= c \i) (let [e (str/index-of s "e" i)]
                   (when e [(parse-long (subs s (inc i) e)) (inc e)]))
        (= c \l) (loop [j (inc i) acc []]
                   (cond (>= j (count s)) nil
                         (= (nth s j) \e) [acc (inc j)]
                         :else (let [r (bdecode s j)] (when r (recur (second r) (conj acc (first r)))))))
        (= c \d) (loop [j (inc i) acc {}]
                   (cond (>= j (count s)) nil
                         (= (nth s j) \e) [acc (inc j)]
                         :else (let [k (bdecode s j)]
                                 (when k (let [val (bdecode s (second k))]
                                           (when val (recur (second val) (assoc acc (wire-> (first k)) (first val)))))))))
        (and (char? c) (>= (int c) 48) (<= (int c) 57))   ; string: <len>:<bytes>
        (let [colon (str/index-of s ":" i)]
          (when colon
            (let [n (parse-long (subs s i colon)) start (inc colon) end (+ start n)]
              (when (<= end (count s)) [(subs s start end) end]))))
        :else nil))))

;; --- public seam for middleware --------------------------------------------
(def ^:private session-counter (atom 0))
(defn new-session
  "A fresh session id (middleware that implements sessions uses this)."
  [] (str "jolt-" (swap! session-counter inc)))

(defn respond
  "Send a response map for `request` (id/session added, then bencoded)."
  [request m] ((:reply request) m))

(defn err-msg
  "Best-effort message for any thrown value (ex-info, or a raw Chez condition —
  ex-message is nil for those, so fall back to the host condition text)."
  [e]
  (or (ex-message e)
      (try ((resolve 'jolt.host/condition-message) e) (catch :default _ nil))
      (pr-str e)))

(defn evaluate
  "Evaluate `code` (optionally in loaded ns `ns-str`), capturing *out*. Returns
  {:value .. :out .. :ns .. :err ..}. in-ns — not (binding [*ns* ..]) — sets the
  ns load-string resolves against on jolt. Reusable by eval middleware."
  [code ns-str]
  (let [result (atom nil) err (atom nil)
        out (with-out-str
              (try (when (and ns-str (not (str/blank? ns-str)) (find-ns (symbol ns-str)))
                     (in-ns (symbol ns-str)))
                   (reset! result (binding [ana/*allow-unresolved-vars* true]
                                    (load-string code)))
                   (catch :default e
                     (reset! err (str (err-msg e)
                                      (when-let [bt (jolt.host/backtrace-string)]
                                        (str "\n" bt)))))))]
    {:value (when (nil? @err) (pr-str @result))
     :out out
     :ns (str (ns-name *ns*))
     :err @err}))

;; ops middleware advertise via describe (built-ins + any a library registers).
(def ^:private extra-ops (atom #{}))
(defn register-ops!
  "Register op name(s) so `describe` advertises them. Call at middleware load."
  [& ops] (swap! extra-ops into (map name ops)))

(defn completions
  "Return nREPL completion entries for `prefix` in request namespace `ns-str`.
  This intentionally stays small: vars from ns-map/ns-publics plus alias and
  namespace-name candidates."
  [prefix ns-str]
  (let [prefix (or prefix "")
        ns-str (if (str/blank? ns-str) (str (ns-name *ns*)) ns-str)
        ns (or (find-ns (symbol ns-str)) (the-ns 'user))
        entry (fn ([candidate] {"candidate" candidate})
                ([candidate type] {"candidate" candidate "type" type}))
        var-entry (fn [candidate v]
                    (if-let [n (:ns (meta v))]
                      {"candidate" candidate "ns" (str n)}
                      {"candidate" candidate}))
        keep (fn [s p] (str/starts-with? (str s) p))]
    (if-let [slash (str/index-of prefix "/")]
      (let [ns-prefix (subs prefix 0 slash)
            sym-prefix (subs prefix (inc slash))
            target (or (get (ns-aliases ns) (symbol ns-prefix))
                       (find-ns (symbol ns-prefix)))]
        (if target
          (vec (for [[sym v] (ns-publics target)
                     :let [sym-name (str sym)]
                     :when (keep sym-name sym-prefix)]
                 (var-entry (str ns-prefix "/" sym-name) v)))
          []))
      (vec (concat
             (for [[sym v] (ns-map ns)
                   :let [candidate (str sym)]
                   :when (keep candidate prefix)]
               (var-entry candidate v))
             (for [[alias _] (ns-aliases ns)
                   :let [candidate (str alias "/")]
                   :when (keep candidate prefix)]
               (entry candidate "namespace"))
             (for [n (all-ns)
                   :let [candidate (str (ns-name n) "/")]
                   :when (keep candidate prefix)]
               (entry candidate "namespace")))))))

;; --- built-in handler ------------------------------------------------------
(defn- built-in-handler [request]
  (let [op (get request "op")]
    (cond
      (= op "clone")    (respond request {"new-session" (new-session) "status" ["done"]})
      (= op "close")    (respond request {"status" ["session-closed" "done"]})
      (= op "describe") (respond request {"status" ["done"]
                                          "versions" {"jolt-nrepl" {"major" 0 "minor" 1}}
                                          "ops" (zipmap (into #{"clone" "close" "describe" "eval" "load-file"
                                                                "completions" "complete"}
                                                              @extra-ops)
                                                        (repeat {}))})
      (or (= op "completions") (= op "complete"))
      (respond request {"completions" (completions (or (get request "prefix")
                                                       (get request "symbol"))
                                                   (get request "ns"))
                        "status" ["done"]})
      (or (= op "eval") (= op "load-file"))
      (let [code (wire-> (if (= op "load-file") (get request "file") (get request "code")))
            {:keys [value out ns err]} (evaluate code (get request "ns"))]
        (when (seq out) (respond request {"out" out}))
        (if err
          (do (respond request {"err" (str err "\n")})
              (respond request {"ex" (str err) "status" ["eval-error" "done"]}))
          (respond request {"value" value "ns" ns "status" ["done"]})))
      :else (respond request {"status" ["done" "unknown-op"]}))))

;; --- middleware composition ------------------------------------------------
;; resolve deps.edn :nrepl/middleware symbols to middleware fns. An entry may
;; resolve to a single (fn [handler] handler') or to a vector of them (so a
;; library can export one `default-middleware` var).
(defn- resolve-middleware [syms]
  (vec (mapcat
         (fn [sym]
           (require (symbol (namespace sym)))
           (let [v (deref (resolve sym))]
             (if (sequential? v) (map #(if (var? %) (deref %) %) v) [v])))
         syms)))

(defn- build-handler [middleware]
  ;; first listed middleware is outermost.
  (reduce (fn [h mw] (mw h)) built-in-handler (reverse middleware)))

(defn- handle-conn [fd handler]
  ;; one send lock per connection: eval/session middleware reply from other
  ;; threads, so sends must not interleave.
  (let [lock (Object.)
        reply-for (fn [msg]
                    (let [id (get msg "id") session (or (get msg "session") "none")]
                      (fn [m]
                        (locking lock
                          (send-str fd (bencode (cond-> m id (assoc "id" id)
                                                        session (assoc "session" session))))))))]
    (loop [buf ""]
      (let [chunk (recv-str fd)]
        (if (nil? chunk)
          (c-close fd)
          (let [rest-buf (loop [b (str buf chunk)]
                           (let [r (bdecode b 0)]
                             (if (nil? r) b
                                 (do (when (map? (first r))
                                       (let [msg (first r)]
                                         (try (handler (assoc msg :reply (reply-for msg)))
                                              (catch :default e (println "nrepl handler error:" (err-msg e))))))
                                     (recur (subs b (second r)))))))]
            (recur rest-buf)))))))

(defn start
  "Start the nREPL server on `port` (a concrete port; loopback only). `middleware`
  is a vector of deps.edn :nrepl/middleware symbols to compose over the built-in
  handler.

  Binds the socket synchronously, so a startup failure (e.g. the port is already
  in use) is thrown to the caller rather than swallowed by the accept thread, then
  accepts connections on a background thread and returns immediately. Writes
  .nrepl-port. Does NOT block — the caller keeps the process alive (jolt.main
  parks the main thread in jolt.host/park-until-interrupt, which also runs the
  main-thread pump, so main-thread-affine work an eval starts, such as a UI
  toolkit's event loop, marshals onto the main thread via call-on-main-thread).

  Returns a zero-arg stop fn: it stops the accept loop, closes the listen socket
  (freeing the port), and removes .nrepl-port. Calling it more than once is a
  no-op."
  ([port] (start port nil))
  ([port middleware]
   ;; An nREPL session is REPL-driven development: trace by default so an uncaught
   ;; error in code evaluated over the connection shows a tail-frame backtrace, with
   ;; no JOLT_TRACE needed. Covers both `nrepl-server` and an app that starts its
   ;; own server under `-M:run` (reload a namespace to trace already-loaded code).
   (jolt.host/enable-trace!)
   (let [handler (build-handler (resolve-middleware (or middleware [])))
         fd (listen-socket port)                  ; throws on bind/listen failure
         stopped (atom false)]
     (try (spit ".nrepl-port" (str port)) (catch :default _ nil))
     (println (str "jolt " (jolt.host/jolt-version) " nREPL server started on port "
                   port " (127.0.0.1) — .nrepl-port written"))
     (when (seq middleware) (println (str ";; middleware: " (str/join " " middleware))))
     (println ";; connect your editor; ^C to stop")
      (future
        ;; A stop closes fd, which makes the blocking accept() return an error; the
        ;; @stopped check then breaks the loop instead of spinning on the dead fd.
        (loop []
         (let [conn (c-accept fd ffi/null ffi/null)]
           (when-not @stopped
             (when (>= conn 0)
               (future (try (handle-conn conn handler)
                            (catch :default e (println "nrepl conn error:" (err-msg e)) (c-close conn)))))
             (recur)))))
      (fn stop []
        (when (compare-and-set! stopped false true)
          (c-close fd)
          (jolt.host/delete-file ".nrepl-port"))
        nil))))
