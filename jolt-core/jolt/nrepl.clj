(ns jolt.nrepl
  "A minimal, extensible nREPL server for jolt, so an editor (CIDER / Calva /
  Cursive) can connect and develop a project live. Speaks bencode over a loopback
  TCP socket (jolt.socket.native). Built in: clone, describe, eval, load-file,
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
  Public seam for middleware: respond, evaluate, register-ops!, new-session,
  err->msg.

  Writes .nrepl-port in the working directory (or wherever start's :port-file
  says) so editors auto-detect the port."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [jolt.ffi :as ffi]
            [jolt.socket.native :as native]
            [jolt.analyzer :as ana]
            [clojure.main :as main]))

;; --- sockets (loopback server) ---------------------------------------------
;; The calls, numbers and per-platform shapes are jolt.socket.native's.

(defn- listen-socket
  "A listening socket on 127.0.0.1:port, close-on-exec — without that every
  subprocess an eval spawns holds a duplicate, and the port stays bound for as
  long as any of them lives (seen with a server's Prolog sessions holding
  7888) — and non-blocking, so the accept loop waits in poll slices and a stop
  never has to close the fd under an accept() (see start). Throws on failure,
  closing the fd."
  [port]
  (let [fd (native/new-socket native/af-inet)
        fail (fn [what e]
               (native/c-close fd)
               (throw (ex-info (str what " failed on port " port ": " (native/error-message e))
                               {:port port :errno e})))]
    (native/set-listener-reuse! fd)
    (let [[sa len] (native/make-sockaddr native/af-inet "127.0.0.1" port)]
      (try
        (let [[r e] (native/c-bind fd sa len)] (when (neg? r) (fail "bind()" e)))
        (finally (ffi/free sa))))
    (let [[r e] (native/c-listen fd 16)] (when (neg? r) (fail "listen()" e)))
    (let [[r e] (native/set-blocking! fd false)] (when (neg? r) (fail "set non-blocking" e)))
    fd))

(def ^:private accept-poll-ms
  "The longest the accept loop waits before it looks at the stop flag again."
  100)

;; bytes flow as latin1 strings on the wire (1 char = 1 byte). Text fields that
;; may carry unicode (code / value / out) convert at the boundary.
(defn- ->wire [s] (String. (.getBytes (str s) "UTF-8") "ISO-8859-1"))
(defn- wire-> [s] (String. (byte-array (map int s)) "UTF-8"))

(def ^:private bufsize 65536)
(defn- recv-str [fd]
  (let [buf (ffi/alloc bufsize)]
    (try (let [[n _] (native/c-recv fd buf bufsize 0)]
           (when (pos? n) (String. (ffi/read-array buf n) "ISO-8859-1")))
         (finally (ffi/free buf)))))

(defn- send-str [fd s]
  (let [data (byte-array (map int s)) n (alength data) buf (ffi/alloc (max 1 n))]
    (try (ffi/write-array buf data)
         (loop [off 0] (when (< off n) (let [[sent _] (native/c-send fd (+ buf off) (- n off) native/msg-nosignal)]
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

(def ^:dynamic *capturing-thread*
  "The thread of an `evaluate` in flight, for as long as it is capturing *out* to
  return with the result — nil otherwise. Middleware that also forwards the
  server's output (an out-subscribe implementation, which tees the *out* ROOT)
  compares this against the writing thread, so an eval's own output isn't sent
  twice: once in the eval reply and again as a stray `out` message.

  It holds the THREAD rather than a flag because a future started by an eval
  inherits the eval's bindings: a flag would read as true there and silence the
  background output an out-subscribe exists to deliver. The inherited value is
  the parent's thread, which is not the one writing."
  nil)

(def ^:dynamic *eval-wrapper*
  "When bound to a function, `evaluate` calls it with a no-argument thunk that
  evaluates the code (eval-string, or load-string for a load) and uses what it
  returns as the value. It runs inside evaluate's own try and *ns* binding, so
  a throw from the code reaches the wrapper before evaluate catches it: a tool
  can run the evaluation under jolt.debug/call-with-break-handler, a timer or a
  binding of its own. Printing the value happens after it returns. nil, the
  default, evaluates directly.

  A middleware that binds it around a session's clone gives every eval in that
  session the wrapper, since the session's worker starts with that clone's
  bindings."
  nil)

(def ^:private last-backtrace (atom nil))
(defn last-error-backtrace
  "The backtrace of the exception now in *e (the \"  trace:\" block evaluate
  appended to :err), or nil.

  It is the eval error's own trace, cut where the eval began, so it holds the
  eval'd code's frames and none of the server's (see eval-frames). A read or
  compile error in the eval'd text has none: its frames would be jolt's reader
  or analyzer, as the CLI's report has it. `evaluate` records it here
  for tooling that presents the error afterwards (an editor's error view, a
  stacktrace op), which otherwise has nothing but the message to show."
  [] @last-backtrace)

;; --- describing an eval error ------------------------------------------------

(def ^:private syntax-phases
  #{:read-source :macro-syntax-check :macroexpansion :compile-syntax-check :compilation})

(defn- syntax-error?
  "A read or analysis diagnostic: raised while reading or compiling the eval'd
  text, so its frames are jolt's reader or analyzer, never the user's code."
  [e]
  (let [d (ex-data e)]
    (boolean (or (= :analysis-error (:jolt.error/type d))
                 (syntax-phases (:clojure.error/phase d))))))

(defn- eval-boundary?
  "A frame of `evaluate` itself — everything from it outward is the server."
  [cls]
  (str/starts-with? (str cls) "jolt.nrepl$evaluate"))

;; evaluate's own pr-str of the value; outward of it is the server
(defn- printer-frame? [line]
  (#{"pr-str" "clojure.core/pr-str"} (first (str/split (str/trim line) #"\s"))))

(defn- outer-frames
  "The frames on the stack where `evaluate` was called, as StackTraceElement
  vectors: the caller's, never the eval'd code's. evaluate's own frames don't
  always make it into an error's trace, so cutting the trace at them can miss
  and leave the caller's frames on its outer end; dropping these from there is
  what keeps an nREPL server's session worker from being named as the error's
  location."
  []
  (into #{} (map StackTraceElement->vec) (.getStackTrace (Thread/currentThread))))

(defn- outer-frame-names [outer]
  (into #{} (map (fn [[cls]] (main/demunge (str cls)))) outer))

(defn- drop-last-while [pred xs]
  (vec (reverse (drop-while pred (reverse xs)))))

(defn- evaluate-frame?
  "A frame line of `evaluate` itself. Its own capture can name it
  jolt.nrepl/evaluate or render it bare, as `evaluate`."
  [line]
  (let [t (str/trim line)]
    (or (str/starts-with? t "jolt.nrepl/evaluate")
        (= "evaluate" (first (str/split t #"\s"))))))

(defn- cut-at-eval
  "Frame lines (\"    ns/fn (file:line)\") down to the first of `evaluate`'s own.
  For an error while printing the value, down to evaluate's pr-str, the
  outermost one."
  ([lines] (cut-at-eval lines false nil))
  ([lines printing?] (cut-at-eval lines printing? nil))
  ([lines printing? outer?]
   (let [[fs more] (split-with #(not (evaluate-frame? %)) (remove str/blank? lines))
         ;; no frame of evaluate's: the caller's frames are the outer end.
         ;; Only that end — the eval'd code can call a fn the caller's stack
         ;; also holds, and that frame is the code's
         fs (if (and outer? (empty? more)) (drop-last-while outer? fs) fs)]
     (if-let [i (when printing? (last (keep-indexed #(when (printer-frame? %2) %1) fs)))]
       (take i fs)
       fs))))

(declare frame-location)

(defn- eval-frames
  "The frame lines of an eval error, innermost first, cut where the eval began;
  empty for a read or compile error in the eval'd text, whose frames would be
  jolt's reader or analyzer (the CLI's report leaves them out too).

  The exception's OWN capture first, as .printStackTrace renders it. It keeps
  the live frames, but a tail call erases a frame, and for a fn eval'd over the
  connection (no source file to check the recovery against) the erased chain is
  not rebuilt from it — (defn f [x] (/ x 0)) then (f 1) captures nothing of f.
  Then the thread's tail-call history (jolt.host/backtrace-string), which
  does rebuild it. That history is the thread's, not the error's: read for a
  reader error, which runs no jolt code before it raises, it was whatever ran
  last — the chunk writer's flush, an unrelated middleware fn. It is only
  consulted for an error raised by the eval'd code's own execution, where
  every top-level form starts the history afresh (jolt-enter-form!)."
  ([e printing?] (eval-frames e printing? #{}))
  ([e printing? outer]
  (if (syntax-error? e)
    []
    (let [names (outer-frame-names outer)
          outer? #(contains? names (some-> (frame-location %) :symbol str))
          own (try
                (when (seq (.getStackTrace e))
                  (let [sw (java.io.StringWriter.)
                        _ (.printStackTrace e (java.io.PrintWriter. sw))
                        s (str sw)
                        head (str (.toString e) "\n")]
                    (when (str/starts-with? s head)
                      (cut-at-eval (str/split-lines (subs s (count head))) printing? outer?))))
                (catch :default _ nil))]
      (vec (or (seq own)
               (when-let [bt (try (jolt.host/backtrace-string) (catch :default _ nil))]
                 (cut-at-eval (remove #(= (str/trim %) "trace:") (str/split-lines bt))
                              printing? outer?))))))))

(defn- frame-location
  "{:symbol :source :line} named by a frame line, nil for one that names no
  Clojure fn (a host frame like map-seq)."
  [line]
  (when-let [[_ nm loc] (re-matches #"\s*([^\s/()]+/[^\s()]+)(?:\s+\((.*)\))?\s*" line)]
    (let [[_ file ln] (when loc (re-matches #"(.*):(\d+)" loc))]
      (cond-> {:symbol (symbol nm)}
        file (assoc :source (last (str/split file #"/")))
        ln (assoc :line (parse-long ln))))))

(defn- user-frame?
  "A frame line naming a fn outside clojure.* and jolt.* — the one ex-triage
  would name, as it skips the core's frames."
  [line]
  (when-let [{:keys [symbol]} (frame-location line)]
    (not (re-find #"^(clojure|jolt)\." (namespace symbol)))))

(defn- simple-class-name [c]
  (when c (last (str/split (name c) #"\."))))

(defn- render-triage
  "clojure.main/ex-str, except that an execution or printing error with no
  frame and no line to name leaves the location out rather than inventing one."
  [{:clojure.error/keys [phase symbol line source class cause spec] :as t}]
  (if (and (#{:execution :print-eval-result} phase) (not spec))
    (let [simple (simple-class-name class)
          cause-type (if (or (nil? simple) (#{"Exception" "RuntimeException"} simple))
                       ""
                       (str " (" simple ")"))
          where (if (or symbol line)
                  (str " at " (when symbol (str symbol " "))
                       "(" (or source "REPL") ":" (or line 1) ")")
                  "")]
      (str (if (= phase :execution) "Execution error" "Error printing return value")
           cause-type where ".\n" (if (nil? cause) "null" cause) "\n"))
    (main/ex-str t)))

(defn err->msg
  "The text JVM nREPL sends as `err` for an eval error: clojure.main/err->msg's
  phase and location line, then the root cause's message, ending in a newline.
  The frames are the exception's own, cut where `evaluate` began, so a location
  names the user's code or nothing.

  `opts` adds what the throwable cannot say about itself, all optional:
    :phase  overrides the triage phase (evaluate passes :print-eval-result for an
            error raised while printing the value)
    :frame  the innermost user frame line of the error's trace, as evaluate's
            :err shows it (\"    user/f (path/x.clj:3)\"), naming the fn when the
            throwable's own frames do not
    :line   the line of the eval'd form that failed, used when no frame names
            one (evaluate passes it; it reads it where it catches)
    :outer  the frames on the stack where evaluate was called (outer-frames),
            which are the caller's and are dropped from the trace's outer end
            when it holds no frame of evaluate's"
  ([e] (err->msg e nil))
  ([e {:keys [phase frame line outer]}]
   (try
     (let [m (Throwable->map e)
           m (cond-> (update m :trace
                             (fn [tr]
                               (let [[fs more] (split-with #(not (eval-boundary? (first %))) tr)]
                                 (if (and outer (empty? more))
                                   (drop-last-while outer fs)
                                   (vec fs)))))
               phase (assoc :phase phase))
           t (main/ex-triage m)
           ;; a jolt diagnostic is the throwable itself, where the JVM's
           ;; CompilerException wraps a RuntimeException, which ex-str leaves
           ;; unnamed. Removing the class instead renders "Syntax error ()".
           t (if (and (syntax-phases (:clojure.error/phase t))
                      (:jolt.error/kind (:data (last (:via m)))))
               (assoc t :clojure.error/class 'java.lang.RuntimeException)
               t)
           ;; the cause as the JVM words it: jolt's "(did you mean …?)" is a
           ;; suffix on the same message, kept on ex-message and :err
           d (:data (last (:via m)))
           t (if (and (= :analyze/unresolved-symbol (:jolt.error/kind d)) (:jolt.error/symbol d))
               (assoc t :clojure.error/cause
                      (str "Unable to resolve symbol: " (:jolt.error/symbol d) " in this context"))
               t)
           run-phase? (#{:execution :print-eval-result} (:clojure.error/phase t))
           floc (when (and frame run-phase? (nil? (:clojure.error/symbol t)))
                  (frame-location frame))
           ;; a frame with no line: the line its fn was defined on, which is
           ;; where the JVM's frame points for a one-line fn. Not the running
           ;; top-level form's, which is the call site
           fline (or (:line floc)
                     (when floc
                       (try (some-> (resolve (:symbol floc)) meta :line)
                            (catch :default _ nil))))
           t (cond-> t
               floc (assoc :clojure.error/symbol (:symbol floc))
               (:source floc) (assoc :clojure.error/source (:source floc))
               fline (assoc :clojure.error/line fline))
           ;; a load's CompilerException (load-string, load-file, require) names
           ;; the form that failed in the loaded text: with no frame of the
           ;; user's to say more, that is the place, as the JVM's eval frame
           ;; there names it (its file's base name)
           wd (when (= 'clojure.lang.Compiler$CompilerException (:type (first (:via m))))
                (:data (first (:via m))))
           t (if (and run-phase? (nil? floc) (nil? (:clojure.error/symbol t)) (:clojure.error/line wd))
               (cond-> (assoc t :clojure.error/line (:clojure.error/line wd))
                 (:clojure.error/source wd)
                 (assoc :clojure.error/source (last (str/split (:clojure.error/source wd) #"/"))))
               t)
           t (if (and line run-phase? (nil? floc) (nil? (:clojure.error/line t)))
               (assoc t :clojure.error/line line)
               t)]
       (render-triage t))
     (catch :default _ (str (err-msg e) "\n")))))

(defn- ex-class-strings
  "nREPL's ex / root-ex: the class of the exception and of its root cause, as
  (str (class e)) prints them, with the JVM's wrappers where it has them. JVM
  nREPL wraps a read error and an error while printing in an ExceptionInfo
  (phase :read-source / :print-eval-result), so ex names that for those. A
  compile-time error there is a Compiler$CompilerException, which root-cause
  stops at, so it is both ex and root-ex; jolt's diagnostic is the throwable
  itself, with no such wrapper to name."
  [e phase]
  (if (and (syntax-phases phase) (not= :read-source phase))
    (let [c "class clojure.lang.Compiler$CompilerException"] [c c])
    [(if (#{:read-source :print-eval-result} phase)
       (str (class (ex-info "" {})))
       (str (class e)))
     (str (class (main/root-cause e)))]))

(defn- chunk-writer
  "A java.io.Writer that hands what was written to `send` in chunks: on flush (a
  println flushes, as *flush-on-newline* has it), and on close. Nothing is sent
  for a flush with nothing new. A send that fails (the client went away) drops
  the chunk rather than failing the println that flushed it: the eval runs on."
  [send]
  (let [sb (StringBuilder.)
        lock (Object.)
        emit! (fn [] (let [s (locking lock (let [s (str sb)] (.setLength sb 0) s))]
                       (when (pos? (count s))
                         (try (send s) (catch :default _ nil)))))]
    (proxy [java.io.Writer] []
      (write
        ([x] (locking lock
               (.append sb (cond (string? x) x
                                 (integer? x) (str (char x))
                                 :else (String. ^chars x)))))
        ([x off len] (locking lock
                       (.append sb (if (string? x)
                                     (subs x off (+ off len))
                                     (String. ^chars x (int off) (int len)))))))
      (flush [] (emit!))
      (close [] (emit!)))))

(defn evaluate
  "Evaluate `code` (optionally in loaded ns `ns-str`). Returns
  {:value .. :out .. :ns .. :err ..}. As JVM nREPL's eval does, it binds *ns*
  to that namespace (else the current one) around the eval, and :ns is the
  binding's value when the eval is done, so an (ns foo) or in-ns in the code is
  what the session's next eval starts in. Reusable by eval middleware.

  With two arguments, *out* is captured and returned whole as :out when the eval
  finishes. With a third, a map {:out f :err f}, output streams instead, as
  upstream nREPL's session writers do: *out* (and *err*, when :err is given) is a
  writer that calls f with each chunk as it is flushed — println flushes — while
  the eval is still running, so an editor's console shows a long job's progress.
  Whatever is left unflushed is sent before this returns, and :out is then empty.
  A future the eval starts inherits the writers, so its output streams too.
  :load true in the map evaluates the code as a load (load-string), as the
  load-file op does, so a namespace switch in it does not reach :ns. :file
  names the code's source path, as nREPL's load-file sends it: its forms carry
  it as :file (the position a def records, the trace) and *file* is that path,
  as for a file load. :file-name, when the map has the key, is *source-path*
  (Compiler.load's sourceName, which load-file sends as file-name); without it
  *source-path* is the path's last segment.

  The value is printed inside the eval, under its *out* and its try: what a lazy
  result prints as it is realized is the eval's output, and an error while
  printing is the eval's error, as on JVM nREPL.

  On an error :value is nil and :err is the message (err-msg), then a newline and
  the trace block when the error has frames of its own (see
  last-error-backtrace). Alongside it, for a middleware that sends what JVM
  nREPL sends:
    :ex             the exception (also in *e)
    :err-msg        the `err` text, clojure.main/err->msg style (err->msg)
    :ex-class       the `ex` string, \"class java.lang.ArithmeticException\"
    :root-ex-class  the `root-ex` string, the class of the root cause"
  ([code ns-str] (evaluate code ns-str nil))
  ([code ns-str {on-out :out on-err :err load? :load file :file :as opts}]
   (let [result (atom nil) evaluated (atom false) printed (atom nil)
         err (atom nil) exc (atom nil) failure (atom nil)
         start-ns (or (when (and ns-str (not (str/blank? ns-str)))
                        (find-ns (symbol ns-str)))
                      *ns*)
         ns-after (atom (str (ns-name start-ns)))
         run (fn []
               (binding [*capturing-thread* (Thread/currentThread)
                         *ns* start-ns]
                 (let [phase (volatile! nil)]
                   (try
                        ;; strict, as clojure.main's REPL and `jolt repl`: an
                        ;; undefined name is "Unable to resolve symbol", and a
                        ;; forward reference across evals takes a declare.
                        ;; eval-string, not load-string: load-string binds *ns*
                        ;; for its own load, and this eval's switch must reach :ns
                        (let [src (when file
                                    (if (contains? opts :file-name) [file (:file-name opts)] [file]))
                              ev (fn []
                                   (if load?
                                     ;; a load-file op is a load, as nREPL's is
                                     ;; (Compiler.load): the file's ns form does
                                     ;; not switch the session
                                     (if src (apply jolt.host/load-string code src) (load-string code))
                                     (if src (apply jolt.host/eval-string code src) (jolt.host/eval-string code))))
                              v (if-let [w *eval-wrapper*] (w ev) (ev))]
                          (reset! result v)
                          (reset! evaluated true)
                          (vreset! phase :print-eval-result)
                          (reset! printed (pr-str v)))
                        (catch :default e
                          ;; read here, while the thread still says which form
                          ;; of the eval'd text was running
                          ;; and the stack below this handler is evaluate's own
                          ;; and its caller's: taken here, only when an eval fails,
                          ;; since a stack walk per eval was most of its cost
                          (let [outer (outer-frames)
                                frames (eval-frames e (= @phase :print-eval-result) outer)
                                bt (when (seq frames)
                                     (str "  trace:\n" (str/join "\n" frames) "\n"))
                                pos (jolt.host/current-source-position)]
                            (reset! exc e)
                            (reset! last-backtrace bt)
                            (reset! failure {:phase @phase :frame (first (filter user-frame? frames))
                                             :line (:line pos) :outer outer})
                            (reset! err (str (err-msg e) (when bt (str "\n" bt))))))
                        (finally (reset! ns-after (str (ns-name *ns*))))))))
         out (if on-out
               (let [ow (chunk-writer on-out)
                     ew (when on-err (chunk-writer on-err))]
                 (try (binding [*out* ow]
                        (if ew (binding [*err* ew] (run)) (run)))
                      (finally (.flush ow) (when ew (.flush ew))))
                 "")
               (with-out-str (run)))]
     ;; the REPL history vars, like clojure.main's REPL and every other nREPL
     ;; server: an editor session can reach for *1 or (ex-data *e) after an error,
     ;; and tooling (a stacktrace op) reads *e to find the last exception. These
     ;; are process-wide roots — an nREPL server has no outer REPL loop whose
     ;; thread bindings would shadow them.
     ;; *1 moves once the code evaluated, before its value is printed, as on JVM
     ;; nREPL: a value whose printing failed is still *1.
     ;; *e (and the backtrace beside it) survives a later successful eval, like
     ;; every REPL's does
     (when @evaluated
       (alter-var-root #'*3 (constantly *2))
       (alter-var-root #'*2 (constantly *1))
       (alter-var-root #'*1 (constantly @result)))
     (when @exc
       (alter-var-root #'*e (constantly @exc)))
     (cond-> {:value (when (nil? @err) @printed)
              :out out
              :ns @ns-after
              :err @err}
       @exc (merge (let [e @exc
                         {:keys [phase] :as f} @failure
                         [ex root] (ex-class-strings e (or phase (:clojure.error/phase (ex-data e))))]
                     {:ex e
                      :err-msg (err->msg e f)
                      :ex-class ex
                      :root-ex-class root}))))))

;; ops middleware advertise via describe (built-ins + any a library registers).
(def ^:private extra-ops (atom #{}))
(defn register-ops!
  "Register op name(s) so `describe` advertises them. Call at middleware load."
  [& ops] (swap! extra-ops into (map name ops)))

;; versions middleware advertise via describe, beside jolt's own. An editor reads
;; this to decide what the server can do: CIDER looks for a "cider-nrepl" entry
;; and refuses to use the ops it implies without one.
(def ^:private extra-versions (atom {}))
(defn register-version!
  "Register a `describe` version entry: (register-version! \"cider-nrepl\"
  {\"major\" 0 \"minor\" 57 \"incremental\" 0 \"version-string\" \"0.57.0\"}).
  Call at middleware load, like register-ops!."
  [name version] (swap! extra-versions assoc (clojure.core/name name) version))

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
                                          "versions" (merge {"jolt-nrepl" {"major" 0 "minor" 1}
                                                             "jolt" {"version-string" (jolt.host/jolt-version)}}
                                                            @extra-versions)
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
            ;; output goes out as it is printed, each flush its own `out` (or
            ;; `err`) message, ahead of the value
            {:keys [value ns err err-msg ex-class root-ex-class]}
            (evaluate code (get request "ns")
                      {:out #(respond request {"out" %})
                       :err #(respond request {"err" %})
                       :load (= op "load-file")
                       ;; the file's path, which load-file sends, as JVM nREPL
                       ;; compiles it under
                       :file (when (= op "load-file") (some-> (get request "file-path") wire->))
                       ;; and *source-path* is its file-name, nil when it sends none
                       :file-name (some-> (get request "file-name") wire->)})]
        ;; an error goes out as JVM nREPL sends it: the err->msg text, then the
        ;; classes with eval-error, then done on its own
        (if err
          (do (respond request {"err" err-msg})
              (respond request {"ex" ex-class "root-ex" root-ex-class "status" ["eval-error"]})
              (respond request {"status" ["done"]}))
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
          (native/c-close fd)
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
  "Start the nREPL server on `port` (loopback only; 0 asks the OS for any free
  port). `middleware` is a vector of deps.edn :nrepl/middleware symbols to compose
  over the built-in handler. `opts`:

    :port-file  where to write the bound port, so an editor can find the server.
                Defaults to .nrepl-port in the working directory; nil writes no
                file. A program that starts its own server from somewhere other
                than the project (a host application loading a --library build,
                say) points this at the project's .nrepl-port.

  Binds the socket synchronously, so a startup failure (e.g. the port is already
  in use) is thrown to the caller rather than swallowed by the accept thread, then
  accepts connections on a background thread and returns immediately. Does NOT
  block — the caller keeps the process alive (jolt.main parks the main thread in
  jolt.host/park-until-interrupt, which also runs the main-thread pump, so
  main-thread-affine work an eval starts, such as a UI toolkit's event loop,
  marshals onto the main thread via call-on-main-thread).

  Returns a zero-arg stop fn: it stops the accept loop, closes the listen socket
  (freeing the port), and removes the port file it wrote. Calling it more than
  once is a no-op. Its metadata carries :port, the port actually bound (the OS's
  pick when `port` is 0), and :port-file, the file written or nil."
  ([port] (start port nil {}))
  ([port middleware] (start port middleware {}))
  ([port middleware opts]
   ;; An nREPL session is REPL-driven development: trace by default so an uncaught
   ;; error in code evaluated over the connection shows a tail-frame backtrace, with
   ;; no JOLT_TRACE needed. Covers both `nrepl-server` and an app that starts its
   ;; own server under `-M:run` (reload a namespace to trace already-loaded code).
   (jolt.host/enable-trace!)
   (let [handler (build-handler (resolve-middleware (or middleware [])))
         fd (listen-socket port)                  ; throws on bind/listen failure
         ;; port 0 asks the kernel for any free port; getsockname says which
         port (let [bound (native/local-port fd)] (if (pos? bound) bound port))
         requested (if (contains? opts :port-file) (:port-file opts) ".nrepl-port")
         ;; absolute, so the delete hits the file spit wrote: spit resolves a
         ;; relative path against the started-from directory, delete-file!
         ;; against the process's (bin/jolt cd's to the repo root). nil when the
         ;; write fails, so stop never deletes a file it did not write; the
         ;; server still starts, but says the editor won't find it that way.
         port-file (when requested
                     (let [port-file (.getAbsolutePath (io/file requested))]
                       (try (spit port-file (str port)) port-file
                            (catch :default e
                              (binding [*out* *err*]
                                (println (str ";; could not write " requested ": " (ex-message e))))
                              nil))))
         stopped (atom false)]
     (println (str "jolt " (jolt.host/jolt-version) " nREPL server started on port "
                   port " (127.0.0.1)"
                   (when port-file (str " — " requested " written"))))
     (when (seq middleware) (println (str ";; middleware: " (str/join " " middleware))))
     (println ";; connect your editor; ^C to stop")
      ;; Plain threads, not futures: the server is not on the agent pool, so an
      ;; eval'd (shutdown-agents) — the usual last form of a -main — must not
      ;; leave the next accept unable to start its connection.
      ;;
      ;; The listen fd is non-blocking and the loop waits in poll slices, looking
      ;; at `stopped` between them. A stop used to close the fd under a blocked
      ;; accept(): Linux does not wake an accept for that, so the thread stayed
      ;; in it holding a freed fd NUMBER, and the next socket the process opened
      ;; could be accepted on. The loop leaves within a slice instead, and stop
      ;; closes the fd once it has.
      (let [acceptor
            (Thread.
             (bound-fn []
               ;; the loop owns the fd: it closes it on the way out, so the
               ;; number is never freed while an accept() may still use it,
               ;; and a loop that dies (Thread. failing, say) frees the port
               (try
                 (loop []
                   (when-not @stopped
                     (let [[conn e] (native/c-accept fd ffi/null ffi/null)]
                       (cond
                         (>= conn 0)
                         (do
                           ;; macOS and Windows hand the listener's non-blocking
                           ;; mode down; the connection is read blocking
                           (native/set-blocking! conn true)
                           (native/guard-accepted! conn)
                           (.start
                            (Thread.
                             (bound-fn []
                               (try (handle-conn conn handler)
                                    (catch :default e
                                      (println "nrepl conn error:" (err-msg e))
                                      (native/c-close conn)))))))
                         (native/eagain? e)
                         ;; a failing poll answers at once; wait the slice anyway
                         (when (neg? (native/poll-one fd native/pollin accept-poll-ms))
                           (Thread/sleep accept-poll-ms))
                         (native/eintr? e) nil
                         ;; anything else (EMFILE, say) persists until something is
                         ;; released; do not spin on it
                         :else (Thread/sleep accept-poll-ms))
                       (recur))))
               (finally (native/c-close fd)))))]
        (.start acceptor)
      (with-meta
       (fn stop []
         (when (compare-and-set! stopped false true)
           ;; the loop closes the fd as it leaves, within a slice; the join is
           ;; what makes the port free when stop returns
           (try (.join acceptor (* 20 accept-poll-ms))
                ;; delete-file!, not the raw Chez delete-file this used to call:
                ;; that one RAISES when the file is already gone, so a stop after
                ;; someone cleaned the port file up threw out of the shutdown path.
                (finally (when port-file (jolt.host/delete-file! port-file)))))
         nil)
       {:port port :port-file port-file})))))
