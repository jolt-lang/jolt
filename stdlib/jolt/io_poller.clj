;; jolt.io-poller — one readiness poller per process (kqueue on macOS, epoll on
;; Linux, WSAPoll on Windows) behind one internal interface, plus the fd-level syscall helpers the
;; socket layer needs (fibers R8, epic jolt-nvpr.8 — sockets + poller half).
;;
;; Why this exists: a blocking call on a fiber PINTS its carrier, and since
;; continuations cannot migrate (R0(d)), the fibers queued behind it are
;; stranded. So the socket layer sets O_NONBLOCK, and on EAGAIN asks this module
;; to wait for readiness — parking the fiber (via the jolt.host seams installed
;; in host/chez/java/fibers-async.ss) when there is a current fiber, and doing a
;; plain blocking kevent/epoll_wait on this thread when there is not. Same
;; user-facing code either way.
;;
;; The poller thread blocks in kevent/epoll_wait with the :blocking marker
;; (__collect_safe). That is measured, not theoretical: a thread inside a
;; foreign call that is not collect-safe stays ACTIVE for Chez's stop-the-world
;; collector, and a collection from another thread then fails outright with
;; "cannot collect when multiple threads are active" — a poller that is not
;; collect-safe stops the whole process from collecting for as long as it waits,
;; which is nearly always. The R8 gate asserts a full collect succeeds while the
;; poller is blocked.
;;
;; A fiber registers its wait itself, with a one-shot kevent or epoll_ctl, while the
;; poller may be blocked in its wait: the kernel takes changes from any thread, and
;; a readiness that already holds fires as soon as the registration lands. So a park
;; does not wake the poller, and a registration has nothing to retire: it is gone
;; once it fires. Never a timed poll, never a sleep in the wait path. (A timed wait
;; — SO_TIMEOUT, a timed connect — bounds the WAITER, not the poller: see
;; wait-ready's deadline arity.) WSAPoll has no kernel-held set, so that backend
;; still wakes its round through a control pipe; see wsapoll-round.
;;
;; Locking: the table (fds, pending, pipe) is mutated under ONE monitor (pm).
;; The fiber's commit-to-park (jolt.host/fiber-park-commit!) runs under pm, and
;; the poller's wake collects woken fibers under pm and resumes them AFTER
;; releasing it — the R3 deliver-vs-park race, closed the same way alt-deliver!
;; closes it (both the commit and the wake's state read are serialized by the
;; caller's lock; pm is a new leaf in the lock chain: nothing the park path does
;; takes the run-queue mutex, and the wake resumes outside pm).
;;
;; What this file CANNOT do, and does not have to. The channel ops bracket their
;; commit and their switch in disable-interrupts, because between the two the fiber
;; is marked but has not left, and a preemption there takes the commit apart. This
;; file is Clojure and has no such bracket: wait-fiber commits under pm and switches
;; after releasing it, so the gap is open here by construction. The scheduler closes
;; it instead — jolt-fiber-preempt-handler refuses to preempt a fiber that is not
;; 'running, which is exactly the set of fibers that have committed to a transition
;; they have not finished (jolt-9d3m, and fibers-preempt-test.ss section 12). So the
;; order below matters and the interrupt state does not: commit under pm, release,
;; then switch.

(ns jolt.io-poller
  (:require [jolt.ffi :as ffi]
            [jolt.socket.native :as native]
            [clojure.string :as str]))

(ffi/load-library)

;; -- platform constants --------------------------------------------------------
;; EAGAIN/EWOULDBLOCK share one value on both platforms; O_NONBLOCK, the socket
;; error option and the connect-in-progress errno do not.
(def ^:private os-name
  (str/lower-case (or (System/getProperty "os.name") "")))
(def ^:private macos?   (str/includes? os-name "mac"))
;; Windows has neither kqueue nor epoll; its backend is WSAPoll, which is not a
;; translation of the other two. WSAPoll is a stateless poll rather than a
;; kernel-held registration set, so each round hands it the whole set — every
;; (fd, filter) with a parked waiter — rebuilt from :fds, and there is no
;; changelist and nothing to delete: an fd whose waiters were woken is simply
;; not in the next round's set. Its wake channel is a loopback socket pair
;; (native/loopback-pair), since WSAPoll accepts only sockets. See wsapoll-round.
(def ^:private windows? (str/includes? os-name "win"))

;; Which backend this process uses. JOLT_IO_POLLER=poll selects the WSAPoll one
;; on POSIX too, where native/c-poll is poll(2) with the same contract: that is
;; how the Windows backend runs under the POSIX gates (make fiberspoll),
;; rather than only on a Windows runner.
(def ^:private poll-backend?
  (or windows? (= "poll" (jolt.host/getenv "JOLT_IO_POLLER"))))

(def ^:private F-GETFL 3)
(def ^:private F-SETFL 4)
(def ^:private O-NONBLOCK (if macos? 0x4 0x800))
(def ^:private EAGAIN (if macos? 35 11))
(def ^:private EINTR 4)
(def ^:private EINPROGRESS (if macos? 36 115))
(def ^:private EALREADY (if macos? 37 114))
(def ^:private SOL-SOCKET (if macos? 0xffff 1))
(def ^:private SO-ERROR (if macos? 0x1007 4))

;; -- syscalls -----------------------------------------------------------------
(ffi/defcfn c-fcntl "fcntl" [:int :int :varargs :int] :int)
(ffi/defcfn c-close "close" [:int] :int)
(ffi/defcfn c-write "write" [:int :pointer :size_t] :ssize_t)
(ffi/defcfn c-getsockopt "getsockopt" [:int :int :int :pointer :pointer] :int)
;; macOS: kqueue/kevent. kevent's timeout is a NULL-able pointer; NULL (via
;; ffi/null means wait forever. :blocking => __collect_safe.
(ffi/defcfn c-kqueue "kqueue" [] :int)
(ffi/defcfn c-kevent "kevent" [:int :pointer :int :pointer :int :pointer] :int :blocking)
;; The same call for a registration: no event list, so it returns at once and has
;; no need to be collect-safe. Answers [result errno].
(ffi/defcfn c-kevent-change "kevent" [:int :pointer :int :pointer :int :pointer] :int
  {:capture-native-error true})
;; Linux: epoll. epoll_wait's timeout is milliseconds; -1 means forever.
(ffi/defcfn c-epoll-create1 "epoll_create1" [:int] :int)
(ffi/defcfn c-epoll-ctl "epoll_ctl" [:int :int :int :pointer] :int)
(ffi/defcfn c-epoll-wait "epoll_wait" [:int :pointer :int :int] :int :blocking)
(ffi/defcfn c-eventfd "eventfd" [:uint :int] :int)

;; -- fd helpers (the socket layer's syscall surface) ---------------------------
(defn errno [] (ffi/errno))
;; errno is only meaningful until the next thing that can set it, and reading it
;; is itself a foreign call -- so a caller that asks two questions about one
;; syscall can get two different answers. Capture the value ONCE at the call site
;; and pass it in; the no-arg forms remain for a caller that reads it directly.
(defn eagain? ([] (= EAGAIN (errno))) ([e] (= EAGAIN e)))
(defn eintr? ([] (= EINTR (errno))) ([e] (= EINTR e)))
(defn connect-pending? [e] (or (= EINPROGRESS e) (= EALREADY e)))
(defn available?
  "Whether this platform has a readiness backend: kqueue on macOS, epoll on
  Linux, WSAPoll on Windows. A caller that needs to park on readiness — a
  server running a fiber per connection — asks before it relies on one."
  []
  true)
(defn nonblock! [fd]
  ;; On Windows this must be a SOCKET: FIONBIO is the only non-blocking switch
  ;; there, and WSAPoll, which waits for it, takes nothing else.
  (if poll-backend?
    (native/set-blocking! fd false)
    (let [f (c-fcntl fd F-GETFL 0)]
      (c-fcntl fd F-SETFL (bit-or f O-NONBLOCK)))))
(defn so-error [fd]
  (let [v (ffi/alloc 4) lenp (ffi/alloc 4)]
    (try
      (ffi/write lenp :int 4)
      (if (neg? (c-getsockopt fd SOL-SOCKET SO-ERROR v lenp)) -1 (ffi/read v :int 0))
      (finally (ffi/free v) (ffi/free lenp)))))

;; -- kqueue / epoll behind one interface ---------------------------------------
(def ^:private EVFILT-READ -1)
(def ^:private EVFILT-WRITE -2)
(def ^:private EV-ADD 1)
(def ^:private EV-ONESHOT 0x10)
(def ^:private EV-CLEAR 0x20)
(def ^:private EVFILT-USER -10)
(def ^:private NOTE-TRIGGER 0x01000000)
(def ^:private EFD-NONBLOCK 0x800)
(def ^:private EFD-CLOEXEC 0x80000)
(def ^:private KEVENT-SIZE 32)      ; struct kevent: uptr ident @0, i16 filter @8, u16 flags @10, u32 fflags @12, iptr data @16, ptr udata @24
(def ^:private EPOLLIN 0x1)
(def ^:private EPOLLOUT 0x4)
(def ^:private EPOLL-ADD 1)
(def ^:private EPOLL-MOD 3)
(def ^:private EPOLLONESHOT 0x40000000)
;; struct epoll_event's layout is ARCHITECTURE-DEPENDENT. The kernel UAPI marks it
;; EPOLL_PACKED only on x86_64 — there it is 12 bytes, u32 events @0 and the u64
;; data @4. Everywhere else (aarch64, and every other Linux port) the u64 is
;; naturally aligned: 16 bytes with events @0, four bytes of padding, and data @8.
;;
;; Hardcoding the x86_64 numbers made an aarch64 Linux poller read the fd out of the
;; padding, so every event named an fd nobody was waiting on and was dropped —
;; and epoll is level-triggered, so the same readiness was re-reported immediately
;; and the loop span at 100% of a core reporting nothing. All fiber socket I/O on
;; ARM Linux hung. Verified against the C struct on both arches rather than assumed.
;; The 12-byte layout is the x86 family's: x86_64 because the kernel packs it
;; there, and 32-bit x86 because a u64 aligns to 4 so no padding is needed anyway.
;; Everything else pads. os.arch is the JVM's spelling — "amd64", "aarch64", "x86".
(def ^:private epoll-packed?
  (contains? #{"amd64" "x86_64" "x86" "i386" "i686"}
             (str/lower-case (or (System/getProperty "os.arch") ""))))
(def ^:private EPOLL-EVENT-SIZE (if epoll-packed? 12 16))
(def ^:private EPOLL-DATA-OFFSET (if epoll-packed? 4 8))

(defn- ev-fd [buf i]
  (if macos?
    (ffi/read buf :uptr (* i KEVENT-SIZE))
    (ffi/read buf :uint (+ (* i EPOLL-EVENT-SIZE) EPOLL-DATA-OFFSET))))

;; WHICH filter fired. Read it back off the event: the u32 at offset 8 is filter in
;; the low half, flags in the high half (see kevent-put!). epoll reports a mask
;; instead, at offset 0.
(defn- ev-filts [buf i]
  (if macos?
    (if (= (bit-and (ffi/read buf :uint (+ (* i KEVENT-SIZE) 8)) 0xffff)
           (bit-and EVFILT-WRITE 0xffff))
      [:write] [:read])
    ;; epoll reports a MASK, and one event carries both directions whenever both are
    ;; ready — a socket with data to read and room to write reports EPOLLIN|EPOLLOUT
    ;; in a single event. Answering with one filter dropped the other direction's
    ;; readiness on the floor: its waiters were not resumed.
    ;;
    ;; EPOLLERR/EPOLLHUP arrives with neither direction bit set. Both directions have
    ;; to hear that, or whichever one is parked sleeps through the fd's death; the
    ;; woken fiber retries and surfaces the error through its own read/write.
    (let [m (ffi/read buf :uint (* i EPOLL-EVENT-SIZE))
          rd (pos? (bit-and m EPOLLIN))
          wr (pos? (bit-and m EPOLLOUT))]
      (cond (and rd wr) [:read :write]
            wr [:write]
            rd [:read]
            :else [:read :write]))))

(defn- kevent-put!
  ([buf i fd filt flags] (kevent-put! buf i fd filt flags 0))
  ([buf i fd filt flags fflags]
   (let [o (* i KEVENT-SIZE)]
     (ffi/write buf :uptr fd o)
     (ffi/write buf :int (bit-or (bit-and filt 0xffff) (bit-shift-left flags 16)) (+ o 8))
     (ffi/write buf :uint fflags (+ o 12))
     (ffi/write buf :int64 0 (+ o 16))
     (ffi/write buf :uptr 0 (+ o 24)))))

(defn- ep-ctl! [ep op fd filt]
  (let [ev (ffi/alloc EPOLL-EVENT-SIZE)]
    (try
      (ffi/write ev :uint (if (= filt :read) EPOLLIN EPOLLOUT))
      (ffi/write ev :uint fd EPOLL-DATA-OFFSET)        ; epoll_data_t.fd — u64 low half
      (ffi/write ev :uint 0 (+ EPOLL-DATA-OFFSET 4))
      (c-epoll-ctl ep op fd ev)
      (finally (ffi/free ev)))))

;; One-shot registration, stating the whole fd: an epoll registration is per fd and
;; carries both directions in one events word, so two waiters on the two directions
;; of a socket are one registration with EPOLLIN|EPOLLOUT. MOD re-arms what an earlier
;; wait left in the set; ENOENT means the fd has none, so ADD. Readiness that already
;; holds is reported as soon as either lands. Negative when the kernel refused it.
(defn- ep-arm! [ep fd filts]
  (let [ev (ffi/alloc EPOLL-EVENT-SIZE)]
    (try
      (ffi/write ev :uint (reduce (fn [m f] (bit-or m (if (= f :read) EPOLLIN EPOLLOUT)))
                                  EPOLLONESHOT filts) 0)
      (ffi/write ev :uint fd EPOLL-DATA-OFFSET)
      (ffi/write ev :uint 0 (+ EPOLL-DATA-OFFSET 4))
      (let [r (c-epoll-ctl ep EPOLL-MOD fd ev)]
        (if (neg? r) (c-epoll-ctl ep EPOLL-ADD fd ev) r))
      (finally (ffi/free ev)))))

;; -- the poller table ----------------------------------------------------------
;; One monitor serializes everything a fiber and the poller thread share:
;;   :fds      {fd {filt {:waiters [fiber ...]}}}   filt = :read|:write
;;   :pending  {fd #{filt ...}}  ; WSAPoll only: a waiter its round has not seen yet
;;   :pipe     [r w]       ; WSAPoll only: wakes the round
;;   :kq       n           ; the poller's kqueue / epoll fd
;;   :cancelled #{fd ...}  ; closing: every wait on the fd returns at once
;;   :threads  {fd #{handle ...}}  ; thread waiters' wake handles (wait-thread)
;;
;; Both are keyed by (fd, FILTER), not by fd, because that is what a registration
;; actually is: kqueue keys a registration by (ident, filter), and the two
;; directions of one socket are independent readiness facts. Keying by fd alone
;; lost registrations and delivered wakeups to the wrong waiter:
;;
;;   One filter per fd, with the wake skipped when the fd was already present, meant
;;   a fiber waiting to WRITE an fd that another fiber had just queued a READ for was
;;   never registered with the kernel at all: no event for it could ever arrive.
;;   Reachable state (fd registered for read only, nothing pending, a write-waiter
;;   parked) confirmed by model-checking the protocol.
;;
;;   :waiters was one flat list per fd, so process-events! resumed EVERY waiter on
;;   an fd whichever filter fired. A read event woke the write-waiters too; they
;;   retried, got EAGAIN and re-registered. That is what masked the bug above
;;   whenever read traffic happened to arrive, and it is why the hang needs a
;;   full-duplex fd whose peer is silent to show itself.
;;
(def ^:private pm (Object.))
(def ^:private state (atom {:fds {} :pending {} :pipe nil :kq nil :started? false
                            :cancelled #{} :threads {}}))

;; how many times the poller entered its blocking wait — the R8 gate-3 handle
(def waits (atom 0))

(defn- pipe-read! [] (first (:pipe @state)))
;; The "pipe" is a loopback socket pair, as WSAPoll accepts only sockets, read and
;; written with recv/send; both ends are non-blocking, so a full wake channel drops
;; the byte (one byte already queued wakes the poller just as well) and draining
;; stops at WSAEWOULDBLOCK.
(defn- pipe-write! []
  (let [w (second (:pipe @state))]
    (when w
      (let [b (ffi/alloc 1)]
        (try (ffi/write b :uint8 1)
             (native/c-send w b 1 0)
             (finally (ffi/free b)))))))

(defn- open-wake-pair! []
  (let [[r w] (native/loopback-pair)]
    (native/set-blocking! r false)
    (native/set-blocking! w false)
    (swap! state assoc :pipe [r w])))

;; Under pm, as every pipe-write! is, so no writer holds the old pair's fds
;; while they are closed.
(defn- drain-pipe! []
  (let [r (pipe-read!) b (ffi/alloc 64)]
    (try
      (loop []
        (let [[n e] (native/c-recv r b 64 0)]
          (cond
            (pos? n) (recur)
            (and (neg? n) (or (native/eagain? e) (native/eintr? e))) nil
            ;; end of stream or a hard error: the pair is dead (reset by
            ;; something outside), and its read end would report ready on
            ;; every round from now on — a spinning poller. Replace it.
            :else (let [[_ w] (:pipe @state)]
                    (open-wake-pair!)
                    (native/c-close r)
                    (native/c-close w)))))
      (finally (ffi/free b)))))

;; The directions of fd with a fiber parked on them. Under pm.
(defn- parked-filts [fd]
  (into #{} (keep (fn [[filt e]] (when (seq (:waiters e)) filt)))
        (get-in @state [:fds fd])))

(defn- process-events! [evs]
  ;; under pm: for each (fd, filter) that fired, collect + clear THAT filter's
  ;; waiters; the control pipe just gets drained. Returns the woken fibers.
  ;;
  ;; Only the fired filter's waiters are resumed. Resuming an fd's whole waiter list
  ;; woke the other direction's fibers on every event — they retried, got EAGAIN and
  ;; re-registered, so it cost a wake and a round each time and, worse, hid a lost
  ;; write registration.
  ;;
  ;; An event with nobody waiting is the registration of a wait that has since ended
  ;; (a timeout, a cancel!, a wake from an event already on its way). It was one-shot,
  ;; so it is spent, and there is nothing to do.
  ;;
  ;; epoll's one-shot disables the whole fd, not the direction that fired, so a
  ;; direction still parked goes back in the set here. Whatever the kernel holds for
  ;; the fd is replaced, so an armed direction nobody waits on any more goes with it.
  ;; An EPOLLERR/EPOLLHUP reaches both directions (see ev-filts) and leaves neither.
  (locking pm
    (let [woken (loop [evs evs woken []]
                  (if (empty? evs)
                    woken
                    (let [[fd filt] (first evs)]
                      (if (= fd (pipe-read!))
                        (do (drain-pipe!) (recur (rest evs) woken))
                        (let [ws (get-in @state [:fds fd filt :waiters])]
                          (when (seq ws)
                            (swap! state assoc-in [:fds fd filt :waiters] []))
                          (recur (rest evs) (into woken ws)))))))]
      (when-not (or macos? poll-backend?)
        (doseq [fd (distinct (map first evs))]
          (let [filts (parked-filts fd)]
            (when (seq filts) (ep-arm! (:kq @state) fd filts)))))
      woken)))

(defn- wsapoll-round
  "One round of the Windows poller: WSAPoll over the wake socket and every
  (fd, filter) with a parked waiter, built from :fds under pm, blocking until
  one is ready. :pending is cleared but not otherwise read — it exists to wake
  the round, and the set is derived from :fds, which already holds every
  registration (a waiter is published there and in :pending in one critical
  section). Answers the [fd filt] pairs that fired, or nil when WSAPoll failed.

  Readiness is reported for the directions that were asked for. An error,
  hangup or invalid handle wakes both, as epoll's EPOLLERR does: the woken
  operation retries and meets the error itself."
  []
  (let [entries (locking pm
                  (swap! state assoc :pending {})
                  (into [[(pipe-read!) #{:read}]]
                        (keep (fn [[fd per-filt]]
                                (let [fs (into #{} (keep (fn [[filt e]]
                                                           (when (seq (:waiters e)) filt))
                                                         per-filt))]
                                  (when (seq fs) [fd fs]))))
                        (:fds @state)))
        n (count entries)
        pfds (native/alloc-pollfds n)
        bad (bit-or native/pollerr native/pollhup native/pollnval)]
    (try
      (dotimes [i n]
        (let [[fd fs] (nth entries i)]
          (native/init-pollfd! pfds i fd
                               (bit-or (if (:read fs) native/pollin 0)
                                       (if (:write fs) native/pollout 0)))))
      (swap! waits inc)
      (let [[rc e] (native/c-poll pfds n -1)]
        ;; a signal is not a failure: an empty round, and the next one rebuilds
        (if (neg? rc)
          (when (native/eintr? e) [])
          (loop [i 0 acc []]
            (if (< i n)
              (let [rev (native/pollfd-revents pfds i)
                    [fd fs] (nth entries i)]
                (recur (inc i)
                       (cond-> acc
                         (and (:read fs)
                              (pos? (bit-and rev (bit-or native/pollin bad))))
                         (conj [fd :read])
                         (and (:write fs)
                              (pos? (bit-and rev (bit-or native/pollout bad))))
                         (conj [fd :write]))))
              acc))))
      (finally (ffi/free pfds)))))

(declare process-events!)

(defn- wsapoll-loop []
  (loop []
    (if-let [evs (wsapoll-round)]
      (doseq [f (process-events! evs)] (jolt.host/fiber-resume f))
      ;; WSAPoll itself failed. Nothing was lost — the set is rebuilt from :fds
      ;; every round — but a failure that persists would spin, so take a breath.
      (Thread/sleep 10))
    (recur)))

(defn- event-list [evbuf n]
  (loop [i 0 acc []]
    (if (< i n)
      (let [fd (ev-fd evbuf i)]
        (recur (inc i) (into acc (map (fn [f] [fd f]) (ev-filts evbuf i)))))
      acc)))

;; One thread blocks in the ONE collect-safe wait, and wakes whoever fired. 256 is
;; the most events one round reports, so it bounds itself. A failed wait (a signal,
;; most likely) reports nothing and goes round again; registrations are the kernel's
;; by then, so nothing is lost with it.
(defn- poller-loop [kq]
  (let [evbuf (ffi/alloc (* 256 (if macos? KEVENT-SIZE EPOLL-EVENT-SIZE)))]
    (loop []
      (swap! waits inc)
      (let [n (if macos?
                (c-kevent kq ffi/null 0 evbuf 256 ffi/null)
                (c-epoll-wait kq evbuf 256 -1))]
        (when (pos? n)
          (doseq [f (process-events! (event-list evbuf n))]
            (jolt.host/fiber-resume f))))
      (recur))))

;; Wake one thread waiter (wait-thread). Under pm: a waiter unregisters under pm
;; before it closes its kqueue/eventfd, so the number named here is still its.
(defn- thread-wake! [h]
  (cond
    poll-backend? (reset! (:cancelled h) true)
    macos?
    (let [ch (ffi/alloc KEVENT-SIZE)]
      (try (kevent-put! ch 0 0 EVFILT-USER 0 NOTE-TRIGGER)
           (c-kevent (:kq h) ch 1 ffi/null 0 ffi/null)
           (finally (ffi/free ch))))
    :else
    (let [b (ffi/alloc 8)]
      (try (ffi/write b :int64 1)
           (c-write (:efd h) b 8)
           (finally (ffi/free b))))))

;; The fd's owner is closing it. Every wait on the fd returns now, and every
;; later one returns at once, until forget!: parked fibers are resumed, and
;; threads blocked in wait-thread are woken through their own wake handle, since
;; nothing about the fd itself will ever signal them. Each woken caller sees its
;; owner closed and raises instead of retrying.
;;
;; The fd must still be open, and must stay open until every operation on it has
;; left — that is the owner's use count (jolt.socket, process.ss). Closing it
;; first frees the number for the next socket or pipe, and a woken operation
;; would then retry on someone else's descriptor.
(defn cancel! [fd]
  (let [woken (locking pm
                (let [e (get-in @state [:fds fd])]
                  (swap! state (fn [s] (-> s
                                           (update :cancelled conj fd)
                                           (update :pending dissoc fd)
                                           (update :fds dissoc fd))))
                  (doseq [h (get-in @state [:threads fd])] (thread-wake! h))
                  (when (and poll-backend? e) (pipe-write!))
                  (into (vec (:waiters (:read e))) (:waiters (:write e)))))]
    (doseq [f woken] (jolt.host/fiber-resume f))))

;; The fd's story ends with its owner: it is about to be closed and nothing is
;; using it. Drop every trace of it, so the next owner of the number starts
;; fresh — a previous owner's ready=true tombstone, or its cancelled mark, would
;; otherwise answer the new owner's first wait. Anything still waiting is woken
;; as cancel! would, which only matters to an owner that skipped cancel!.
(defn forget! [fd]
  (let [woken (locking pm
                (let [e (get-in @state [:fds fd])]
                  (doseq [h (get-in @state [:threads fd])] (thread-wake! h))
                  ;; WSAPoll is holding the fd in its current set; wake it so the
                  ;; next round's set leaves the fd out. The owner may close the
                  ;; fd before that round, and a new socket given the number can
                  ;; then see a spurious wake from the old set — harmless, every
                  ;; woken operation retries its call
                  (when (and poll-backend? e) (pipe-write!))
                  (swap! state (fn [s] (-> s
                                           (update :cancelled disj fd)
                                           (update :pending dissoc fd)
                                           (update :fds dissoc fd))))
                  (into (vec (:waiters (:read e))) (:waiters (:write e)))))]
    (doseq [f woken] (jolt.host/fiber-resume f))))

;; A point-in-time classification of the poller's table, for a stress gate to
;; print WHEN it loses a wakeup — which of the stages lost it is otherwise
;; unrecoverable after the sockets close. Cheap and lock-free on purpose: one
;; atom read; the caller is already in a failure path.
;;   :pending entries -> WSAPoll only: registered, its round has not run
;;   waiters>0        -> in the kernel set (or a round's), event never fired
;;   waiters=0        -> event fired and waiters were collected; if a fiber is
;;                       still parked, the resume was lost after that
(defn debug-state []
  (let [s @state]
    {:pending (:pending s)
     :waits @waits
     ;; per fd, per filter — a loss is now attributable to a DIRECTION as well as
     ;; a stage, which is the whole point of printing this from a stress gate
     :fds (into {} (map (fn [[fd per-filt]]
                          [fd (into {} (map (fn [[filt e]]
                                              [filt {:waiters (count (:waiters e))}])
                                            per-filt))])
                        (:fds s)))}))

(defn- ensure-started-wsapoll! []
  (open-wake-pair!)
  (swap! state assoc :started? true)
  (doto (Thread. wsapoll-loop "jolt-io-poller")
    (.setDaemon true)
    (.start)))

(defn- ensure-started! []
  ;; under pm. One poller thread per process, started on the first fiber wait.
  (cond
    (:started? @state) nil
    poll-backend? (ensure-started-wsapoll!)
    :else
    (let [kq (if macos? (c-kqueue) (c-epoll-create1 0))]
      (swap! state assoc :kq kq :started? true)
      ;; a daemon thread, not a future: the poller runs for the life of
      ;; the process, and a non-daemon thread would keep it from ending
      (doto (Thread. (fn [] (poller-loop kq)) "jolt-io-poller")
        (.setDaemon true)
        (.start)))))

;; -- the wait API --------------------------------------------------------------
;; (wait-ready fd :read|:write) -> void. Fiber-aware: parks the current fiber on
;; readiness and returns when the poller wakes it; on a plain thread, blocks in
;; a private kevent/epoll_wait (level-triggered, so a readiness that raced the
;; registration fires immediately — no missed wakeup). Either way it also returns
;; when the fd is cancelled (cancel!), so the caller has to check whether its
;; owner is closing before it retries.
;;
;; The clock wait-ready's deadlines are on, and jolt.host/timer-at!'s: monotonic
;; milliseconds (System/nanoTime's clock), so a wall-clock step neither fires a
;; socket timeout early nor holds it late. A caller builds a deadline with
;; deadline-ms, which is exact: a ratio of milliseconds off the clock read to the
;; nanosecond, so a timeout ends when its time has passed, as the JDK's (on
;; System.nanoTime) do. It used to round now up to a whole millisecond, and with
;; the wait's own latency a 1ms SO_TIMEOUT took 2.4ms where the JDK takes 1.2.
(defn clock-ms [] (/ (System/nanoTime) 1000000))
(defn deadline-ms [timeout-ms] (+ (clock-ms) timeout-ms))
;; A positive time left, rounded up to a whole unit, for a syscall that takes one:
;; rounded down, the last wait ends short of the deadline and goes round again.
(defn- ceil-long [x] (let [q (long (quot x 1))] (if (> x q) (inc q) q)))

;; (wait-ready fd filt deadline-ms) is the same wait bounded by a deadline on
;; clock-ms, answering :timeout when the deadline passed first and nil otherwise
;; (SO_TIMEOUT and the timed connect, jolt-lang/jolt#1191/#1192). The poller's own
;; wait stays untimed: a thread bounds its private kevent/epoll_wait, and a fiber
;; is woken at the deadline by the runtime's one timer thread (jolt.host/timer-at!).

;; Hand the kernel a one-shot registration for (fd, filt), whose waiter is already in
;; :fds. Under pm, so an event it fires at once cannot reach process-events! before
;; the fiber has committed. False when the kernel refused it: no event will ever come.
(defn- register! [fd filt]
  (if macos?
    (let [ch (ffi/alloc KEVENT-SIZE)]
      (try
        (kevent-put! ch 0 fd (if (= filt :read) EVFILT-READ EVFILT-WRITE)
                     (bit-or EV-ADD EV-ONESHOT))
        ;; with no event list a refusal is the call's own -1, not an EV_ERROR event
        (loop []
          (let [[r e] (c-kevent-change (:kq @state) ch 1 ffi/null 0 ffi/null)]
            (cond (not (neg? r)) true
                  (eintr? e) (recur)
                  :else false)))
        (finally (ffi/free ch))))
    (not (neg? (ep-arm! (:kq @state) fd (parked-filts fd))))))

;; Under pm: register the current fiber as a waiter on (fd, filt) and commit it to
;; park. True when committed — the caller must then switch — false when the wait
;; is already over: the fd is cancelled, or the kernel will not watch it, and the
;; caller's retry of its syscall reports what is wrong with the fd.
(defn- commit-wait! [fd filt]
  (ensure-started!)
  (when-not (contains? (:cancelled @state) fd)
    (let [ws (or (get-in @state [:fds fd filt :waiters]) [])]
      (swap! state assoc-in [:fds fd filt :waiters] (conj ws (jolt.host/current-fiber)))
      (if poll-backend?
        (do
          ;; The guard is per (fd, FILTER). Keyed by fd alone it skipped the wake
          ;; whenever the OTHER direction of the same fd was already queued.
          (when-not (contains? (get (:pending @state) fd #{}) filt)
            (swap! state update-in [:pending fd] (fnil conj #{}) filt)
            (pipe-write!))
          (jolt.host/fiber-park-commit!)
          true)
        (if (register! fd filt)
          (do (jolt.host/fiber-park-commit!) true)
          (do (swap! state assoc-in [:fds fd filt :waiters] ws) false))))))

(defn wait-fiber [fd filt]
  (when (locking pm (commit-wait! fd filt))
    (jolt.host/fiber-to-scheduler!)))

;; Under pm: take fiber F off (fd, filt)'s waiters. True when it was there — then
;; nothing else will resume it, and whoever took it must.
(defn- unpark-waiter! [fd filt f]
  (let [ws (get-in @state [:fds fd filt :waiters])]
    (when (some #(identical? % f) ws)
      (swap! state assoc-in [:fds fd filt :waiters] (filterv #(not (identical? % f)) ws))
      true)))

;; The timed fiber wait. The deadline thunk and the wait decide under pm who wakes
;; the fiber: the thunk resumes it only if it is still parked on THIS (fd, filt) —
;; if the poller took it first, the readiness wins and the caller retries its
;; syscall. W is this wait's own record, so a thunk that fires after the wait is
;; over — the cancel below lost the race to the timer thread — finds :done? and
;; does nothing. The timer is armed before the commit; a deadline that passes
;; before the fiber commits leaves :expired? for the commit to see, so it never
;; parks past it. A wait that ends first cancels its deadline, so the timer does
;; not hold the fiber until a long SO_TIMEOUT runs out.
;; A timeout leaves its one-shot registration behind: it fires once, finds no
;; waiter, and is spent.
(defn- wait-fiber-until [fd filt deadline]
  (let [f (jolt.host/current-fiber)
        w (atom {:expired? false :timed-out? false :done? false})
        timer (jolt.host/timer-at!
                deadline
                (fn []
                  (when (locking pm
                          (when-not (:done? @w)
                            (swap! w assoc :expired? true)
                            (when (unpark-waiter! fd filt f)
                              (swap! w assoc :timed-out? true)
                              true)))
                    (jolt.host/fiber-resume f))))
        park? (locking pm (if (:expired? @w) :expired (commit-wait! fd filt)))]
    (if (= park? :expired)
      :timeout
      (do (try (when park? (jolt.host/fiber-to-scheduler!))
               (finally (locking pm (swap! w assoc :done? true))
                        (jolt.host/timer-cancel! timer)))
          (when (:timed-out? @w) :timeout)))))

;; A thread waiter registers its wake handle — its private kqueue, which carries
;; an EVFILT_USER event, or an eventfd in its private epoll set — so cancel! can
;; reach it. Both the fd and the wake event are in the set BEFORE the handle is
;; published: a cancel! that finds the handle can always trigger it. And the
;; cancelled check is made under the same lock as the publish, so a cancel! that
;; ran first is never missed.
(defn- thread-enter! [fd h]
  (locking pm
    (when-not (contains? (:cancelled @state) fd)
      (swap! state update-in [:threads fd] (fnil conj #{}) h)
      true)))

(defn- thread-leave! [fd h]
  (locking pm
    (let [hs (disj (get-in @state [:threads fd]) h)]
      (if (seq hs)
        (swap! state assoc-in [:threads fd] hs)
        (swap! state update :threads dissoc fd)))))

;; The blocking wait itself, bounded by DEADLINE (clock-ms) when there is one.
;; Answers :timeout once the deadline has passed, nil when the wait returned for
;; any other reason. EINTR, and a timed wait that returns early, go round again;
;; with no deadline this is the plain untimed wait it always was.
(defn- remaining-ms [deadline] (- deadline (clock-ms)))

(defn- kevent-wait! [kq ev deadline]
  (if (nil? deadline)
    (loop []
      (when (neg? (c-kevent kq ffi/null 0 ev 1 ffi/null)) (recur)))
    (let [ts (ffi/alloc 16)]
      (try
        (loop []
          (let [ms (remaining-ms deadline)]
            (if (<= ms 0)
              :timeout
              (let [ns (ceil-long (* ms 1000000))]
                (ffi/write ts :int64 (quot ns 1000000000) 0)
                (ffi/write ts :int64 (rem ns 1000000000) 8)
                  (when-not (pos? (c-kevent kq ffi/null 0 ev 1 ts)) (recur))))))
        (finally (ffi/free ts))))))

(defn- epoll-wait! [ep ev deadline]
  (if (nil? deadline)
    (loop []
      (when (neg? (c-epoll-wait ep ev 1 -1)) (recur)))
    (loop []
      (let [ms (remaining-ms deadline)]
        (if (<= ms 0)
          :timeout
          (when-not (pos? (c-epoll-wait ep ev 1 (min (ceil-long ms) 2147483647))) (recur)))))))

;; Windows: WSAPoll on the one fd, in slices, so a cancel! — which can only set
;; the handle's flag, there being no per-wait wake channel short of a socket pair
;; per wait — is seen within one slice. Readiness ends the wait at once; only a
;; cancel waits out the slice.
(def ^:private wsapoll-thread-slice-ms 50)

(defn- wsapoll-thread-wait [fd filt deadline]
  (let [h {:cancelled (atom false)}
        events (if (= filt :read) native/pollin native/pollout)]
    (when (thread-enter! fd h)
      (try
        (loop []
          (cond
            @(:cancelled h) nil
            (and deadline (<= (remaining-ms deadline) 0)) :timeout
            :else
            (let [slice (if deadline
                          (min wsapoll-thread-slice-ms (ceil-long (remaining-ms deadline)))
                          wsapoll-thread-slice-ms)
                  rev (native/poll-one fd events (max 0 slice))]
              (if (zero? rev) (recur) nil))))
        (finally (thread-leave! fd h))))))

;; A failed registration returns without waiting: the caller retries its syscall,
;; which reports what is wrong with the fd.
(defn wait-thread
  ([fd filt] (wait-thread fd filt nil))
  ([fd filt deadline]
   (cond
     poll-backend? (wsapoll-thread-wait fd filt deadline)
     macos?
     (let [kq (c-kqueue) ch (ffi/alloc (* 2 KEVENT-SIZE)) ev (ffi/alloc KEVENT-SIZE) h {:kq kq}]
       (try
         (kevent-put! ch 0 fd (if (= filt :read) EVFILT-READ EVFILT-WRITE) EV-ADD)
         (kevent-put! ch 1 0 EVFILT-USER (bit-or EV-ADD EV-CLEAR))
         (when (and (not (neg? (c-kevent kq ch 2 ffi/null 0 ffi/null)))
                    (thread-enter! fd h))
           (try
             (kevent-wait! kq ev deadline)
             (finally (thread-leave! fd h))))
         (finally (ffi/free ch) (ffi/free ev) (c-close kq))))
     :else
     (let [ep (c-epoll-create1 0)
           efd (c-eventfd 0 (bit-or EFD-NONBLOCK EFD-CLOEXEC))
           ev (ffi/alloc EPOLL-EVENT-SIZE)
           h {:efd efd}]
       (try
         (when (and (not (neg? (ep-ctl! ep EPOLL-ADD fd filt)))
                    (not (neg? (ep-ctl! ep EPOLL-ADD efd :read)))
                    (thread-enter! fd h))
           (try
             (epoll-wait! ep ev deadline)
             (finally (thread-leave! fd h))))
         (finally (ffi/free ev) (c-close efd) (c-close ep)))))))

(defn wait-ready
  ([fd filt]
   (if (jolt.host/fiber?)
     (wait-fiber fd filt)
     (wait-thread fd filt)))
  ([fd filt deadline]
   (cond
     (nil? deadline) (wait-ready fd filt)
     (<= (remaining-ms deadline) 0) :timeout
     (jolt.host/fiber?) (wait-fiber-until fd filt deadline)
     :else (wait-thread fd filt deadline))))
