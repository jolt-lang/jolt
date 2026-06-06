; Jolt Standard Library: jolt.nrepl
;
; An nREPL (https://nrepl.org) server and client written in Clojure, on top of
; Jolt's Janet interop bridge (the `janet.*` namespace segment). The bencode
; codec follows nrepl.bencode and the op/response shapes follow babashka.nrepl
; (the SCI-targeted nREPL server). Because the whole thing is ordinary Clojure
; over `janet.net/*`, the networking it uses is reusable for anything else.
;
; Notes:
;   - One Jolt runtime backs the server; sessions are tracked ids and share the
;     runtime (defs persist across a connection, like a dev REPL).
;   - eval uses Jolt's own `eval`/`read-string`; printed output is captured by
;     rebinding Janet's :out dynamic.
;   - No true interrupt: an in-flight synchronous eval can't be stopped.

;; ───────────────────────── bencode ─────────────────────────

(defn benc
  "Encode `x` (integer, string, keyword, sequential, or map) to a bencode string."
  [x]
  (cond
    (integer? x) (str "i" x "e")
    (string? x)  (str (count x) ":" x)
    (keyword? x) (benc (name x))
    (symbol? x)  (benc (name x))
    (map? x) (let [ks (sort (fn [a b] (compare (name a) (name b))) (keys x))]
               (str "d" (apply str (mapcat (fn [k] [(benc (name k)) (benc (get x k))]) ks)) "e"))
    (sequential? x) (str "l" (apply str (map benc x)) "e")
    (nil? x) "le"
    :else (throw (ex-info "bencode: cannot encode" {:value x}))))

(defn encode [x] (benc x))

; A reader buffers bytes from a janet.net connection (or a preloaded string for
; tests) and refills via janet.net/read.
(defn reader [conn buf] (atom {:conn conn :buf (or buf "") :pos 0}))

(defn- rd-ensure [r n]
  (loop []
    (let [{:keys [conn buf pos]} @r]
      (when (< (count buf) (+ pos n))
        (let [chunk (janet.net/read conn 4096)]
          (when (nil? chunk) (throw (ex-info "eof" {})))
          (swap! r assoc :buf (str buf (str chunk)))
          (recur))))))

(defn- take-n [r n]
  (rd-ensure r n)
  (let [{:keys [buf pos]} @r]
    (swap! r assoc :pos (+ pos n))
    (subs buf pos (+ pos n))))

(defn- take-ch [r] (take-n r 1))

(def ^:private digits #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"})

(defn decode
  "Read one bencode value from reader `r`. Throws on EOF. Dict keys come back as
  strings; the top-level nREPL message is a dict (map)."
  [r]
  (let [c (take-ch r)]
    (cond
      (= c "i") (loop [acc ""] (let [d (take-ch r)] (if (= d "e") (janet/scan-number acc) (recur (str acc d)))))
      (= c "l") (loop [out []] (let [v (decode r)] (if (= v ::end) out (recur (conj out v)))))
      (= c "d") (loop [out {}] (let [k (decode r)] (if (= k ::end) out (recur (assoc out k (decode r))))))
      (= c "e") ::end
      (contains? digits c)
        (loop [acc c] (let [d (take-ch r)] (if (= d ":") (take-n r (janet/scan-number acc)) (recur (str acc d)))))
      :else (throw (ex-info "bad bencode byte" {:byte c})))))

;; ───────────────────────── server ─────────────────────────

(def version "0.1.0")

(def ^:private session-counter (atom 0))
(defn- new-session []
  (str "jolt-" (swap! session-counter inc) "-" (janet.math/floor (* 1000000 (janet.math/random)))))

(defn- resp-for
  "Build a response by echoing the request's id/session (an nREPL requirement)."
  [msg extra]
  (assoc extra "session" (get msg "session" "none") "id" (get msg "id" "unknown")))

; Jolt resolves a function body's unqualified symbols against the *dynamic*
; current-ns, not the function's home ns. So evaluating user code (which switches
; ns) would break jolt.nrepl's own later symbol lookups. eval-in-ns confines the
; switch: it evaluates one form in `ns-str` and ALWAYS restores current-ns to
; jolt.nrepl before returning, reporting the form's value/error and resulting ns.
; It uses only special forms (in-ns/eval/the-ns/try) + keywords, so it resolves
; regardless of the ambient ns.
(defn- eval-in-ns [ns-str form]
  (in-ns (symbol ns-str))
  ; Bind the value before reading the ns: jolt evaluates map-literal values
  ; right-to-left, so the result ns must be captured *after* eval runs any in-ns.
  (let [result (try (let [v (eval form)] {:val v :ns (:name (the-ns))})
                    (catch Throwable e {:err e :ns (:name (the-ns))}))]
    (in-ns 'jolt.nrepl)
    result))

(defn- eval-handler [server msg send!]
  ; current-ns is global ctx state shared by all fibers, so set the eval ns
  ; explicitly each time: requested :ns, else the session's last ns, else user.
  ; `respond` / `flush-out` are locals (lexical, ns-independent) on purpose.
  (let [code (get msg "code" "")
        out-buf (janet/buffer "")
        old-out (janet/dyn :out)
        respond (fn [extra] (send! (assoc extra "session" (get msg "session" "none")
                                          "id" (get msg "id" "unknown"))))
        flush-out (fn [] (when (pos? (count out-buf))
                           (respond {"out" (str out-buf)})
                           (janet.buffer/clear out-buf)))]
    (try
      (do
        (janet/setdyn :out out-buf)
        (loop [forms (seq (read-string (str "[" code "]")))
               cur-ns (or (get msg "ns") (:eval-ns @server) "user")]
          (when forms
            (let [{:keys [val ns err]} (eval-in-ns cur-ns (first forms))]
              (flush-out)
              (swap! server assoc :eval-ns ns)
              (when err (throw err))
              (respond {"ns" ns "value" (pr-str val)})
              (recur (next forms) ns))))
        (janet/setdyn :out old-out)
        (respond {"status" ["done"]}))
      (catch Throwable e
        (janet/setdyn :out old-out)
        (flush-out)
        (respond {"err" (str e "\n")})
        (respond {"ex" "class jolt/Exception"
                  "root-ex" "class jolt/Exception"
                  "status" ["eval-error"]})
        (respond {"status" ["done"]})))))

(def ^:private describe-ops
  {"clone" {} "close" {} "describe" {} "eval" {} "load-file" {}
   "ls-sessions" {} "interrupt" {} "eldoc" {}})

(defn- dispatch [server msg send!]
  (case (get msg "op")
    "clone" (let [id (new-session)]
              (swap! server update :sessions conj id)
              (send! (resp-for msg {"new-session" id "status" ["done"]})))
    "describe" (send! (resp-for msg {"ops" describe-ops
                                     "versions" {"jolt" {"version-string" version}
                                                 "nrepl" {"version-string" version}}
                                     "status" ["done"]}))
    "eval" (eval-handler server msg send!)
    "load-file" (eval-handler server (assoc msg "code" (get msg "file" "")) send!)
    "close" (do (swap! server update :sessions disj (get msg "session"))
                (send! (resp-for msg {"status" ["done" "session-closed"]})))
    "ls-sessions" (send! (resp-for msg {"sessions" (vec (:sessions @server)) "status" ["done"]}))
    "interrupt" (send! (resp-for msg {"status" ["done"]}))
    "eldoc" (send! (resp-for msg {"status" ["done" "no-eldoc"]}))
    (send! (resp-for msg {"status" ["error" "unknown-op" "done"]}))))

(defn- handle-conn [server conn]
  (let [r (reader conn nil)
        send! (fn [resp] (janet.net/write conn (encode resp)))]
    (try
      (loop []
        (let [msg (decode r)]
          (when (map? msg)
            (try (dispatch server msg send!)
                 (catch Throwable e
                   (send! (resp-for msg {"err" (str e "\n") "status" ["done"]}))))
            (recur))))
      (catch Throwable _ nil))
    (try (janet.net/close conn) (catch Throwable _ nil))))

; We run the accept loop ourselves with janet.net/accept rather than passing a
; handler to janet.net/server: Janet's built-in accept loop arity-checks the
; handler, which a Jolt closure doesn't satisfy. janet.ev/call schedules each
; connection (and the loop itself) on a fiber.
(defn- accept-loop [server]
  (loop []
    (let [conn (try (janet.net/accept (:sock @server)) (catch Throwable _ nil))]
      (when conn
        (janet.ev/call handle-conn server conn)
        (recur)))))

(defn start-server!
  "Start an nREPL server. opts: :host (default \"127.0.0.1\"), :port (default
  7888). Returns a server handle (an atom). Non-blocking — connections are served
  on the event loop."
  [opts]
  (let [host (get opts :host "127.0.0.1")
        port (get opts :port 7888)
        sock (janet.net/server host (str port))
        server (atom {:sessions #{} :host host :port port :sock sock :eval-ns "user"})]
    (janet.ev/call accept-loop server)
    server))

(defn stop-server!
  "Stop accepting new connections."
  [server]
  (when-let [sock (:sock @server)] (janet.net/close sock))
  server)

;; ───────────────────────── client ─────────────────────────

(defn connect
  "Connect to an nREPL server. opts: :host (default \"127.0.0.1\"), :port
  (default 7888). Returns a client handle."
  [opts]
  (let [conn (janet.net/connect (get opts :host "127.0.0.1") (str (get opts :port 7888)))]
    {:conn conn :reader (reader conn nil)}))

(defn send-msg [client msg] (janet.net/write (:conn client) (encode msg)))
(defn read-msg [client] (decode (:reader client)))

(defn- status-done? [resp]
  (when-let [st (get resp "status")]
    (and (sequential? st) (some (fn [s] (= "done" (str s))) st))))

(defn request
  "Send `msg` (a map with at least an \"op\") and collect responses until one
  carries the \"done\" status. Returns the vector of responses."
  [client msg]
  (send-msg client msg)
  (loop [out []]
    (let [resp (read-msg client)
          out (conj out resp)]
      (if (status-done? resp) out (recur out)))))

(defn client-clone
  "Send a clone op; return the new session id."
  [client]
  (some (fn [r] (get r "new-session")) (request client {"op" "clone"})))

(defn client-eval
  "Eval `code`; returns the responses. Pass `session` to eval in a cloned session."
  ([client code] (request client {"op" "eval" "code" code}))
  ([client code session] (request client {"op" "eval" "code" code "session" session})))

(defn client-close [client] (janet.net/close (:conn client)))
