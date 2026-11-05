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
;; Registration races (a fiber registering while the poller is already inside
;; kevent/epoll_wait) are closed with a control pipe, the textbook shape: the
;; pipe read end is always in the poller's set, a registration writes a byte to
;; the write end, and the poller drains pending registrations into its kq/epoll
;; on every wake. Never a timed poll, never a sleep in the wait path. (A timed
;; wait — SO_TIMEOUT, a timed connect — bounds the WAITER, not the poller: see
;; wait-ready's deadline arity.)
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
(ffi/defcfn c-pipe "pipe" [:pointer] :int)
(ffi/defcfn c-write "write" [:int :pointer :size_t] :ssize_t)
(ffi/defcfn c-read "read" [:int :pointer :size_t] :ssize_t)
(ffi/defcfn c-getsockopt "getsockopt" [:int :int :int :pointer :pointer] :int)
;; macOS: kqueue/kevent. kevent's timeout is a NULL-able pointer; NULL (via
;; ffi/null means wait forever. :blocking => __collect_safe.
(ffi/defcfn c-kqueue "kqueue" [] :int)
(ffi/defcfn c-kevent "kevent" [:int :pointer :int :pointer :int :pointer] :int :blocking)
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
(def ^:private EV-DELETE 2)
(def ^:private EV-CLEAR 0x20)
(def ^:private EVFILT-USER -10)
(def ^:private NOTE-TRIGGER 0x01000000)
(def ^:private EFD-NONBLOCK 0x800)
(def ^:private EFD-CLOEXEC 0x80000)
(def ^:private KEVENT-SIZE 32)      ; struct kevent: uptr ident @0, i16 filter @8, u16 flags @10, u32 fflags @12, iptr data @16, ptr udata @24
(def ^:private EPOLLIN 0x1)
(def ^:private EPOLLOUT 0x4)
(def ^:private EPOLL-ADD 1)
(def ^:private EPOLL-DEL 2)
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

;; Diagnosis counters (debug-state): kevent reports a changelist entry it could
;; not process — an EV_DELETE for an already-closed fd is the common one — as an
;; EV_ERROR (#x4000) EVENT in the eventlist rather than failing the call, and
;; ev-fd's caller would otherwise treat that error entry as readiness for
;; whatever socket currently owns the reused fd number.
(def ^:private ev-errors (atom 0))
;; flags is a u16 at offset 10 and the FFI reads no 16-bit type: read the u32 at
;; offset 8 (little-endian: filter in the low half, flags in the high half) and
;; test EV_ERROR (#x4000) against the high half.
(defn- ev-error? [buf i]
  (and macos?
       (pos? (bit-and (unsigned-bit-shift-right
                        (ffi/read buf :uint (+ (* i KEVENT-SIZE) 8)) 16)
                      0x4000))))

;; WHICH filter fired. A kqueue registration is keyed by (ident, filter), so the
;; EV_DELETE that retires one has to name the same filter the ADD used — it used to
;; be hardcoded to EVFILT_READ, which silently left every write registration in the
;; set forever (and, since a socket is almost always writable and kqueue is
;; level-triggered, made it fire on every round). Read it back off the event: the
;; u32 at offset 8 is filter in the low half, flags in the high half (see
;; kevent-put!). epoll reports a mask instead, at offset 0.
(defn- ev-filts [buf i]
  (if macos?
    (if (= (bit-and (ffi/read buf :uint (+ (* i KEVENT-SIZE) 8)) 0xffff)
           (bit-and EVFILT-WRITE 0xffff))
      [:write] [:read])
    ;; epoll reports a MASK, and one event carries both directions whenever both are
    ;; ready — a socket with data to read and room to write reports EPOLLIN|EPOLLOUT
    ;; in a single event. Answering with one filter dropped the other direction's
    ;; readiness on the floor: its waiters were not resumed, and the delete scheduled
    ;; for the direction that WAS reported then retired the whole fd (epoll has no
    ;; per-direction delete), so nothing was left to report it later either.
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

;; …with a SET of filters as one mask. An epoll registration is per fd and carries
;; both directions in one events word, so two waiters on the two directions of a
;; socket are one registration with EPOLLIN|EPOLLOUT — not two, which is what a
;; second EPOLL_CTL_ADD would ask for (and be refused with EEXIST).
(defn- ep-ctl-mask! [ep op fd filts]
  (let [ev (ffi/alloc EPOLL-EVENT-SIZE)]
    (try
      (ffi/write ev :uint (reduce (fn [m f] (bit-or m (if (= f :read) EPOLLIN EPOLLOUT)))
                                    0 filts) 0)
      (ffi/write ev :uint fd EPOLL-DATA-OFFSET)
      (ffi/write ev :uint 0 (+ EPOLL-DATA-OFFSET 4))
      (c-epoll-ctl ep op fd ev)
      (finally (ffi/free ev)))))

;; -- the poller table ----------------------------------------------------------
;; One monitor serializes everything a fiber and the poller thread share:
;;   :fds      {fd {filt {:waiters [fiber ...]}}}   filt = :read|:write
;;   :pending  {fd #{filt ...}}  ; registered by a fiber, not yet in the poller's set
;;   :pipe     [r w]       ; control pipe — registration wake
;;   :kq       n           ; the poller's kqueue / epoll fd
;;   :cancelled #{fd ...}  ; closing: every wait on the fd returns at once
;;   :threads  {fd #{handle ...}}  ; thread waiters' wake handles (wait-thread)
;;
;; Both are keyed by (fd, FILTER), not by fd, because that is what a registration
;; actually is: kqueue keys a registration by (ident, filter), and the two
;; directions of one socket are independent readiness facts. Keying by fd alone
;; lost registrations and delivered wakeups to the wrong waiter:
;;
;;   :pending held ONE filter per fd, and wait-fiber skipped the wake when the fd
;;   was already present. So a fiber waiting to WRITE an fd that another fiber had
;;   just queued a READ registration for was never registered with the kernel at
;;   all: its filter was dropped on the floor, and no event for it could ever
;;   arrive. Reachable state (fd registered for read only, nothing pending, a
;;   write-waiter parked) confirmed by model-checking the protocol.
;;
;;   :waiters was one flat list per fd, so process-events! resumed EVERY waiter on
;;   an fd whichever filter fired. A read event woke the write-waiters too; they
;;   retried, got EAGAIN and re-registered. That is what masked the bug above
;;   whenever read traffic happened to arrive, and it is why the hang needs a
;;   full-duplex fd whose peer is silent to show itself.
;;
;; The pending EV_DELETE/EPOLL_CTL_DEL set is NOT here: process-events! hands it
;; straight to poller-loop, which carries it in a loop variable to the next
;; poller-round. It is a set of [fd filt] pairs, since a kqueue delete has to name
;; the filter its add used.
;; (It used to be mirrored into this atom as :to-delete, written on every round and
;; read by nobody, which made the round's critical section look like it was
;; protecting something it was not.)
(def ^:private pm (Object.))
(def ^:private state (atom {:fds {} :pending {} :pipe nil :kq nil :started? false
                            :cancelled #{} :threads {}}))

;; how many times the poller entered its blocking wait — the R8 gate-3 handle
(def waits (atom 0))

(defn- pipe-read! [] (first (:pipe @state)))
;; On Windows the "pipe" is a loopback socket pair, read and written with
;; recv/send; both ends are non-blocking, so a full wake channel drops the byte
;; (one byte already queued wakes the poller just as well) and draining stops at
;; WSAEWOULDBLOCK.
(defn- pipe-write! []
  (let [w (second (:pipe @state))]
    (when w
      (let [b (ffi/alloc 1)]
        (try (ffi/write b :uint8 1)
             (if poll-backend? (native/c-send w b 1 0) (c-write w b 1))
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
      (if poll-backend?
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
        (loop [] (when-not (neg? (c-read r b 64)) (recur))))
      (finally (ffi/free b)))))

;; Put a drained-but-unapplied add set back into :pending, unioning per fd with
;; whatever landed while the round was in flight. Under pm, like every other write
;; to the table. No pipe byte is needed: the caller is the poller itself, and the
;; next thing a round does is drain :pending, so the retry is already scheduled.
(defn- requeue-adds! [adds]
  (when (seq adds)
    (locking pm
      ;; Only what is still LIVE. forget! drops an fd's :fds and :pending entries
      ;; together when its socket closes, and it can run while the round is in
      ;; flight — putting the drained set back verbatim would resurrect a
      ;; registration for a closed fd, and if that number has been reused, hand it
      ;; to whatever socket owns it now. wait-fiber writes :fds and :pending in one
      ;; critical section, so a registration that is still wanted always has its
      ;; :fds entry.
      (let [live (into {} (keep (fn [[fd filts]]
                                  (let [ks (filter #(get-in @state [:fds fd %]) filts)]
                                    (when (seq ks) [fd (set ks)])))
                                adds))]
        (when (seq live)
          (swap! state update :pending
                 (fn [p] (reduce (fn [m [fd filts]] (update m fd (fnil into #{}) filts))
                                 (or p {}) live))))))))

;; One loop iteration of the poller thread. Under pm, drains the pending
;; registrations; builds a kevent changelist (or epoll_ctl calls) carrying those
;; ADDs and last round's DELETEs, then blocks in the ONE collect-safe wait with
;; that changelist applied atomically.
;;
;; Returns the [fd filt] pairs whose events fired — or NIL if the wait itself
;; failed, which is a different thing from an empty list and poller-loop treats it
;; as one: empty means the kernel processed the changelist and reported nothing
;; usable, nil means it may never have seen it.
(defn- poller-round [kq to-delete]
  ;; Read AND clear :pending in ONE critical section. As two — read, then clear —
  ;; a registration landing in between was erased without ever being applied to
  ;; the kqueue/epoll set, so that fd's readiness was never reported and the fiber
  ;; waiting on it never resumed. The window is microseconds, which is why it
  ;; showed up as one fiber of eight failing to finish, once, and never again in
  ;; isolation; a stress that keeps registrations landing loses ~11% of them.
  (let [[adds wanted]
        (locking pm
          (let [a (:pending @state)]
            (swap! state assoc :pending {})
            ;; epoll only: for every fd this round touches, the set of directions it
            ;; still WANTS registered — those with a fiber parked on them. Read in
            ;; the same critical section as the drain so it cannot disagree with what
            ;; was drained. See the epoll changelist below for why a per-fd delete
            ;; cannot be issued without also re-stating the directions that survive.
            [a (when-not macos?
                 (into {} (for [fd (distinct (concat (keys a) (map first to-delete)))]
                            [fd (into #{} (keep (fn [[filt e]] (when (seq (:waiters e)) filt))
                                                (get-in @state [:fds fd])))])))]))]
    ;; The changelist is sized to nch, not to a fixed 256. :pending holds one
    ;; entry per fd and nothing caps it, so a round that drains more than 256
    ;; registrations wrote past the end of the buffer and then told the kernel to
    ;; read that many entries — heap corruption, not a dropped registration. The
    ;; event buffer below is a different thing: 256 is the count passed to
    ;; kevent/epoll_wait as the most events to report, so it bounds itself.
    ;; adds is {fd #{filt ...}}: one kqueue registration per (fd, filter).
    (let [add-pairs (for [[fd filts] adds filt filts] [fd filt])
          nch (+ (count add-pairs) (count to-delete))
          chbuf (when (and macos? (pos? nch)) (ffi/alloc (* nch KEVENT-SIZE)))]
      (try
        (when chbuf
          (let [i (atom 0)]
            ;; Each delete names the FILTER its add used. Hardcoding EVFILT_READ
            ;; here retired the read registration (or errored with ENOENT when only
            ;; a write one existed) and left write registrations in the set for good.
            (doseq [[fd filt] to-delete]
              (kevent-put! chbuf @i fd (if (= filt :read) EVFILT-READ EVFILT-WRITE) EV-DELETE)
              (swap! i inc))
            (doseq [[fd filt] add-pairs]
              (kevent-put! chbuf @i fd (if (= filt :read) EVFILT-READ EVFILT-WRITE) EV-ADD)
              (swap! i inc))))
        ;; epoll keys a registration by fd alone and carries the wanted directions as
        ;; ONE mask, so a second filter is a change to the existing registration, not
        ;; a second registration. DEL-then-ADD with the union mask says that without
        ;; having to track what the kernel currently holds: the DEL of an
        ;; unregistered fd fails harmlessly, and epoll is level-triggered, so a
        ;; readiness that existed across the gap is reported as soon as the ADD lands.
        ;; epoll keys a registration by fd and carries both directions in ONE mask, so
        ;; there is no such thing as retiring one of them: EPOLL_CTL_DEL takes no mask
        ;; and ignores one if given (it answers ENOENT), and it removes the fd
        ;; outright. Issuing a bare DEL to retire a fired :read therefore also
        ;; dropped a :write registration the same fd still had a fiber parked on —
        ;; and that direction was no longer in :pending, so nothing re-added it and
        ;; the writer never woke. The same hole ran the other way: an ADD carrying
        ;; only the newly pending direction dropped the direction already registered.
        ;;
        ;; So state the whole fd rather than one direction of it. DEL-then-ADD with
        ;; the set of directions that still have a parked waiter says what the kernel
        ;; should hold without tracking what it does hold; a DEL of an unregistered
        ;; fd fails harmlessly, and epoll is level-triggered, so a readiness spanning
        ;; the gap is reported as soon as the ADD lands. An fd with nothing left
        ;; parked on it gets the DEL alone, which is the retirement.
        ;;
        ;; (kqueue needs none of this: it keys by (ident, filter), so its changelist
        ;; above retires exactly the one registration it names.)
        (when (and (not macos?) (seq wanted))
          (doseq [[fd filts] wanted]
            (c-epoll-ctl kq EPOLL-DEL fd ffi/null)
            (when (seq filts) (ep-ctl-mask! kq EPOLL-ADD fd filts))))
        (swap! waits inc)
        (let [evbuf (ffi/alloc (if macos? (* 256 KEVENT-SIZE) (* 256 EPOLL-EVENT-SIZE)))]
          (try
            (let [n (if macos? (c-kevent kq (or chbuf ffi/null) nch evbuf 256 ffi/null)
                                (c-epoll-wait kq evbuf 256 -1))]
              (if (neg? n)
                  ;; The wait FAILED, and :pending was drained and cleared under pm
                  ;; before the call — so without this the registrations that were in
                  ;; this round's changelist are gone. Nothing else remembers them:
                  ;; the waiter is parked in :fds, its fd is no longer pending, and
                  ;; there is no retry. Put them back so the next round applies them.
                  ;;
                  ;; Safe whether or not the kernel got them. On kqueue the changelist
                  ;; rides with the failing call, so it may or may not have been
                  ;; applied; a repeat EV_ADD for the same (ident, filter) just
                  ;; updates the existing registration. On epoll the ctls already ran
                  ;; as their own syscalls before the wait, so the re-add is redundant
                  ;; and harmless — DEL-then-ADD is what that path does anyway, and
                  ;; epoll is level-triggered, so a readiness spanning the gap is
                  ;; reported as soon as the ADD lands.
                  ;;
                  ;; NIL, not an empty list, and the difference is the whole point:
                  ;; empty means the kernel processed the changelist and reported
                  ;; nothing usable, nil means it may never have seen it. Only the
                  ;; second is a reason to carry the round's deletes forward, and
                  ;; poller-loop tells them apart on exactly this.
                  (do (requeue-adds! adds) nil)
                  ;; An EV_ERROR entry is a changelist entry the kernel REFUSED (an
                  ;; EV_DELETE for an fd that is closed or was never registered with
                  ;; that filter), not a readiness report. It used to be counted and
                  ;; then handed on as if it were one, so it marked whatever socket
                  ;; currently owns that fd number ready and cleared its waiters —
                  ;; ev-error?'s own comment says this is what must not happen. Count
                  ;; it and drop it.
                  (loop [i 0 acc []]
                    (if (< i n)
                      (if (ev-error? evbuf i)
                        (do (swap! ev-errors inc) (recur (inc i) acc))
                        (let [fd (ev-fd evbuf i)]
                          (recur (inc i)
                                 (into acc (map (fn [f] [fd f]) (ev-filts evbuf i))))))
                      acc))))
            (finally (ffi/free evbuf))))
        (finally (when chbuf (ffi/free chbuf)))))))

(defn- process-events! [evs]
  ;; under pm: for each (fd, filter) that fired, collect +
  ;; clear THAT filter's waiters, and schedule its delete; the control pipe just
  ;; gets drained. Returns [woken new-deletes], deletes as [fd filt] pairs.
  ;;
  ;; Only the fired filter's waiters are resumed. Resuming an fd's whole waiter list
  ;; woke the other direction's fibers on every event — they retried, got EAGAIN and
  ;; re-registered, so it cost a wake and a round each time and, worse, hid the lost
  ;; write registration this shape exists to prevent.
  (locking pm
    (loop [evs evs dels #{} woken []]
      (if (empty? evs)
        [woken dels]
        (let [[fd filt] (first evs)]
          (if (= fd (pipe-read!))
            (do (drain-pipe!) (recur (rest evs) dels woken))
            (let [e (get-in @state [:fds fd filt])]
              ;; Nobody is waiting on this direction any more: cancel! dropped the
              ;; entry while the fd stays open until its last operation leaves. The
              ;; registration is still in the kernel set and level-triggered, so
              ;; retire it, or it fires on every round until the fd closes.
              (if (nil? e)
                (recur (rest evs) (conj dels [fd filt]) woken)
                (do (swap! state assoc-in [:fds fd filt :waiters] [])
                    (recur (rest evs) (conj dels [fd filt]) (into woken (:waiters e))))))))))))

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
      (let [[woken _] (process-events! evs)]
        (doseq [f woken] (jolt.host/fiber-resume f)))
      ;; WSAPoll itself failed. Nothing was lost — the set is rebuilt from :fds
      ;; every round — but a failure that persists would spin, so take a breath.
      (Thread/sleep 10))
    (recur)))

(defn- poller-loop [kq]
  (loop [to-delete #{}]
    (let [evs (poller-round kq to-delete)]
      (cond
        ;; nil — the WAIT ITSELF failed, so the changelist may never have reached the
        ;; kernel. Carry the deletes: dropping them would leave retired registrations
        ;; in the set, and level-triggered they fire on every later round, each
        ;; phantom marking its fd ready and clearing whatever waiters it has by then.
        ;; (The adds are already back in :pending — poller-round requeued them.)
        (nil? evs) (recur to-delete)
        ;; Empty — the kernel DID process the changelist and every entry it reported
        ;; was an EV_ERROR that got filtered out. An EV_ERROR is the kernel refusing a
        ;; changelist entry, so these deletes are done: the registration they name is
        ;; already gone (a closed fd is dropped from the kqueue set, and the delete
        ;; the poller had scheduled for it comes back ENOENT). Carrying them forward
        ;; re-issues a delete that will be refused again, and kevent returns as soon
        ;; as a changelist entry errors and reports ONLY the error — so the round
        ;; reports nothing, carries the same delete, and the poller spins without ever
        ;; reporting readiness again. One closed socket was enough to stop all fiber
        ;; I/O in the process. Drop them, which is what a refusal means.
        (empty? evs) (recur #{})
        :else (let [[woken new-del] (process-events! evs)]
                (doseq [f woken] (jolt.host/fiber-resume f))
                (recur new-del))))))

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
;;   :pending entries  -> registered, never drained into the kernel set
;;   waiters>0 -> in the kernel set (or add failed silently), event never fired
;;   waiters=0 -> event fired and waiters were collected; if a fiber is still
;;                parked, the resume was lost after that
(defn debug-state []
  (let [s @state]
    {:pending (:pending s)
     :waits @waits
     :ev-errors @ev-errors
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
    (let [pfds (ffi/alloc 8)]
      (try
        (when (neg? (c-pipe pfds))
          (throw (Exception. "jolt.io-poller: pipe() failed")))
        (let [r (ffi/read pfds :int 0) w (ffi/read pfds :int 4)]
          (nonblock! r) (nonblock! w)
          (let [kq (if macos? (c-kqueue) (c-epoll-create1 0))]
            (swap! state assoc :pipe [r w] :kq kq :started? true)
            (if macos?
              (let [ch (ffi/alloc KEVENT-SIZE)]
                (try
                  (kevent-put! ch 0 r EVFILT-READ EV-ADD)
                  (c-kevent kq ch 1 ffi/null 0 ffi/null)
                  (finally (ffi/free ch))))
              (ep-ctl! kq EPOLL-ADD r :read))
            ;; a daemon thread, not a future: the poller runs for the life of
            ;; the process, and a non-daemon thread would keep it from ending
            (doto (Thread. (fn [] (poller-loop kq)) "jolt-io-poller")
              (.setDaemon true)
              (.start))))
        (finally (ffi/free pfds))))))

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

;; Under pm: register the current fiber as a waiter on (fd, filt) and commit it to
;; park. True when committed — the caller must then switch — false when the wait
;; is already over (the fd is cancelled, or a ready flag answers it).
(defn- commit-wait! [fd filt]
  (ensure-started!)
  (let [e (get-in @state [:fds fd filt])]
    (cond
      (contains? (:cancelled @state) fd) false
      :else
      (do (swap! state assoc-in [:fds fd filt :waiters]
                 (conj (or (:waiters e) []) (jolt.host/current-fiber)))
          ;; The guard is per (fd, FILTER). Keyed by fd alone it
          ;; skipped the registration whenever the OTHER direction of
          ;; the same fd was already queued, and that filter then never
          ;; reached the kernel at all.
          (when-not (contains? (get (:pending @state) fd #{}) filt)
            (swap! state update-in [:pending fd] (fnil conj #{}) filt)
            (pipe-write!))
          (jolt.host/fiber-park-commit!)
          true))))

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
;; The entry the poller keeps for the fd after a timeout is the same stale
;; registration a woken waiter leaves: it fires once, finds no waiter, is retired.
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
