# clojure.core.async on Janet fibers.
#
# Janet fibers are stackful coroutines, so a `go` block is just "run the body in
# a fiber" — the body parks on a channel op by yielding to the event loop, and
# the whole interpreter call stack rides along on the fiber's stack. No CPS/state
# machine transform (unlike Clojure's `go` macro), so <! / >! work anywhere
# (inside try, nested fns, loops, …).
#
# A channel is a pair of Janet ev/chans wrapped in a tagged table: a `:ch` that
# carries values and a `:done` that is closed to signal channel close. A take is
# `(ev/select :ch :done)` — ev/select checks in order, so buffered values drain
# before the close signal is seen, giving Clojure's drain-then-nil semantics. We
# use a separate `:done` channel because Janet's ev/chan-close *discards* a
# channel's buffered values. close! just closes :done (idempotent, no fiber), so
# nothing leaks.
#
# Single OS thread: go blocks run cooperatively on the event loop, so <! (park)
# and <!! (block) are the same here.

(use ./types)
(use ./pv)
(use ./config)

(defn jolt-chan? [x] (and (table? x) (= :jolt/chan (get x :jolt/type))))

(defn- wrap [vc dc] @{:jolt/type :jolt/chan :ch vc :done dc :closed @[false]})

(defn- vchan [x]
  (if (jolt-chan? x) (x :ch) (error (string "expected a channel, got " (type x)))))

# (chan) unbuffered, (chan n) fixed buffer of n. A transducer 3rd arg is
# accepted but ignored until Phase 3.
(defn async-chan [&opt n xform]
  (wrap (if (and (number? n) (> n 0)) (ev/chan n) (ev/chan)) (ev/chan)))

(defn async-close! [ch]
  (when (not (in (ch :closed) 0))
    (put (ch :closed) 0 true)
    (protect (ev/chan-close (ch :done))))
  nil)

# <! / <!! — take, parking the fiber. Drains buffered values, then returns nil
# once the channel is closed and empty.
(defn async-take [ch]
  (def r (ev/select (ch :ch) (ch :done)))
  (if (= :take (in r 0)) (in r 2) nil))

# >! / >!! — put, parking the fiber. Returns true if delivered, false if the
# channel is closed. nil may not be put on a channel (it is the closed value).
(defn async-give [ch v]
  (when (nil? v) (error "Can't put nil on a channel"))
  (if (in (ch :closed) 0) false
    (if (in (protect (ev/give (ch :ch) v)) 0) true false)))

# Run thunk (a jolt 0-arg closure, directly callable) in a fiber; return a
# buffered(1) channel that conveys its value once, then closes. A nil result
# just closes. Buffered(1) so a fire-and-forget go leaves no parked fiber.
#
# The dynamic-var bindings in effect at spawn time are conveyed into the fiber
# (Clojure binding conveyance): we snapshot them here (on the spawning fiber)
# and install a private copy inside the new fiber before running the body.
(defn async-go-spawn [thunk]
  (def snap (snapshot-bindings))
  (def w (async-chan 1))
  (ev/go (fn []
           (install-bindings snap)
           (def res (protect (thunk)))
           (when (and (in res 0) (not (nil? (in res 1))))
             (async-give w (in res 1)))
           (async-close! w)))
  w)

# (alts! [ch ...]) — take from whichever channel is ready first; returns
# [value channel] (value is nil if that channel closed). Take-only for v1.
(defn async-alts [chans]
  (def cs (cond (pvec? chans) (pv->array chans)
                (tuple? chans) chans
                (array? chans) chans
                (error "alts! expects a vector of channels")))
  (def raws @[])
  (def lookup @{})            # raw ev/chan -> [jolt-chan done?]
  (each c cs
    (array/push raws (c :ch))   (put lookup (c :ch)   [c false])
    (array/push raws (c :done)) (put lookup (c :done) [c true]))
  (def r (ev/select ;raws))
  (def info (get lookup (in r 1)))
  (def jc (in info 0))
  (def val (if (or (in info 1) (not= :take (in r 0))) nil (in r 2)))
  (pv-from-indexed @[val jc]))

# (timeout ms) — a channel that closes after ms milliseconds.
(defn async-timeout [ms]
  (def w (async-chan))
  (ev/go (fn [] (ev/sleep (/ ms 1000)) (async-close! w)))
  w)

# (put! ch v [cb]) — async put; (take! ch cb) — async take. Fire a fiber and
# call the optional callback with the result.
(defn async-put! [ch v &opt cb]
  (ev/go (fn []
           (def ok (async-give ch v))
           (when (and cb (not (nil? cb))) (cb ok))))
  nil)
(defn async-take! [ch cb]
  (ev/go (fn []
           (def val (async-take ch))
           (when (and cb (not (nil? cb))) (cb val))))
  nil)

# --- macros (Janet macro-fns that return forms) ---

(defn- sym [name &opt ns] {:jolt/type :symbol :ns ns :name name})

# (go body...) -> (go-spawn (fn* [] body...))
(defn async-go [& body]
  @[(sym "go-spawn" "clojure.core.async") (array (sym "fn*") [] ;body)])

# (go-loop bindings body...) -> (go (loop bindings body...))
(defn async-go-loop [bindings & body]
  @[(sym "go" "clojure.core.async") (array (sym "loop") bindings ;body)])

# (thread body...) — runs cooperatively in a fiber here (no OS thread); same
# shape as go (returns a result channel).
(defn async-thread [& body]
  @[(sym "go-spawn" "clojure.core.async") (array (sym "fn*") [] ;body)])

(def- async-bindings
  @{"chan" async-chan
    "chan?" jolt-chan?
    "close!" async-close!
    "<!" async-take   "<!!" async-take
    ">!" async-give   ">!!" async-give
    "alts!" async-alts "alts!!" async-alts
    "timeout" async-timeout
    "put!" async-put!
    "take!" async-take!
    "go-spawn" async-go-spawn
    "go" async-go
    "go-loop" async-go-loop
    "thread" async-thread})

(def- async-macros @{"go" true "go-loop" true "thread" true})

(defn install-async!
  "Create/populate the clojure.core.async namespace in ctx."
  [ctx]
  (let [ns (ctx-find-ns ctx "clojure.core.async")]
    (loop [[name f] :pairs async-bindings]
      (def v (ns-intern ns name f))
      (when (get async-macros name) (put v :macro true)))
    ns))
