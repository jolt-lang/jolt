;; jolt.io-poller — one readiness poller per process (kqueue on macOS, epoll on
;; Linux) behind one internal interface, plus the fd-level syscall helpers the
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
;; on every wake. Never a timed poll, never a sleep in the wait path.
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
            [clojure.string :as str]))

(ffi/load-library)

;; -- platform constants --------------------------------------------------------
;; EAGAIN/EWOULDBLOCK share one value on both platforms; O_NONBLOCK, the socket
;; error option and the connect-in-progress errno do not.
(def ^:private macos?
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))

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

;; -- fd helpers (the socket layer's syscall surface) ---------------------------
(defn errno [] (ffi/errno))
;; errno is only meaningful until the next thing that can set it, and reading it
;; is itself a foreign call -- so a caller that asks two questions about one
;; syscall can get two different answers. Capture the value ONCE at the call site
;; and pass it in; the no-arg forms remain for a caller that reads it directly.
(defn eagain? ([] (= EAGAIN (errno))) ([e] (= EAGAIN e)))
(defn eintr? ([] (= EINTR (errno))) ([e] (= EINTR e)))
(defn connect-pending? [e] (or (= EINPROGRESS e) (= EALREADY e)))
(defn nonblock! [fd]
  (let [f (c-fcntl fd F-GETFL 0)]
    (c-fcntl fd F-SETFL (bit-or f O-NONBLOCK))))
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
;; whatever socket currently owns the reused fd number. stale-consumes counts
;; wait-fiber fast-path hits on a ready flag left by a PREVIOUS owner of the fd
;; (:fds entries are never removed, so a one-read socket leaves ready=true
;; behind forever).
(def ^:private ev-errors (atom 0))
(def ^:private stale-consumes (atom 0))
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

(defn- kevent-put! [buf i fd filt flags]
  (let [o (* i KEVENT-SIZE)]
    (ffi/write buf :uptr fd o)
    (ffi/write buf :int (bit-or (bit-and filt 0xffff) (bit-shift-left flags 16)) (+ o 8))
    (ffi/write buf :uint 0 (+ o 12))
    (ffi/write buf :int64 0 (+ o 16))
    (ffi/write buf :uptr 0 (+ o 24))))

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
;;   :fds      {fd {filt {:waiters [fiber ...] :ready bool}}}   filt = :read|:write
;;   :pending  {fd #{filt ...}}  ; registered by a fiber, not yet in the poller's set
;;   :pipe     [r w]       ; control pipe — registration wake
;;   :kq       n           ; the poller's kqueue / epoll fd
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
;; :ready is per filter for the same reason — a read tombstone must not satisfy a
;; write wait.
;;
;; The pending EV_DELETE/EPOLL_CTL_DEL set is NOT here: process-events! hands it
;; straight to poller-loop, which carries it in a loop variable to the next
;; poller-round. It is a set of [fd filt] pairs, since a kqueue delete has to name
;; the filter its add used.
;; (It used to be mirrored into this atom as :to-delete, written on every round and
;; read by nobody, which made the round's critical section look like it was
;; protecting something it was not.)
(def ^:private pm (Object.))
(def ^:private state (atom {:fds {} :pending {} :pipe nil :kq nil :started? false}))

;; how many times the poller entered its blocking wait — the R8 gate-3 handle
(def waits (atom 0))

(defn- pipe-read! [] (first (:pipe @state)))
(defn- pipe-write! []
  (let [w (second (:pipe @state))]
    (when w
      (let [b (ffi/alloc 1)]
        (try (ffi/write b :uint8 1)
             (c-write w b 1)
             (finally (ffi/free b)))))))

(defn- drain-pipe! []
  (let [r (pipe-read!) b (ffi/alloc 64)]
    (try
      (loop [] (when-not (neg? (c-read r b 64)) (recur)))
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
  ;; under pm: for each (fd, filter) that fired, mark THAT filter ready, collect +
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
              (if (nil? e)
                (recur (rest evs) dels woken)
                (do (swap! state assoc-in [:fds fd filt :ready] true)
                    (swap! state assoc-in [:fds fd filt :waiters] [])
                    (recur (rest evs) (conj dels [fd filt]) (into woken (:waiters e))))))))))))

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

;; The fd's story ends with its socket. Drop the table entry and any pending
;; registration, and wake anything still parked on it: the kernel auto-removes
;; a closed fd from the kqueue/epoll set, so no event is coming for a parked
;; reader — a close racing a parked read otherwise sleeps forever. The woken
;; read sees EBADF/EOF and surfaces through the normal error path. This also
;; means a REUSED fd number always starts with a fresh entry: a previous
;; owner's ready=true tombstone (set by its final event, consumable only by a
;; next wait that never came) made every new socket's first wait skip its park
;; and re-register — one extra wake/park race per socket per round, which is
;; the amplification that made the park-commit race observable at all.
(defn forget! [fd]
  (let [woken (locking pm
                (let [e (get-in @state [:fds fd])]
                  (swap! state update :pending dissoc fd)
                  (swap! state update :fds dissoc fd)
                  ;; every direction's waiters — the fd is going away for both
                  (into (vec (:waiters (:read e))) (:waiters (:write e)))))]
    (doseq [f woken] (jolt.host/fiber-resume f))))

;; A point-in-time classification of the poller's table, for a stress gate to
;; print WHEN it loses a wakeup — which of the stages lost it is otherwise
;; unrecoverable after the sockets close. Cheap and lock-free on purpose: one
;; atom read; the caller is already in a failure path.
;;   :pending entries  -> registered, never drained into the kernel set
;;   ready=false + waiters>0 -> in the kernel set (or add failed silently),
;;                              event never fired
;;   ready=true + waiters=0  -> event fired and waiters were collected; the
;;                              fiber resume was lost after that
(defn debug-state []
  (let [s @state]
    {:pending (:pending s)
     :waits @waits
     :ev-errors @ev-errors
     :stale-consumes @stale-consumes
     ;; per fd, per filter — a loss is now attributable to a DIRECTION as well as
     ;; a stage, which is the whole point of printing this from a stress gate
     :fds (into {} (map (fn [[fd per-filt]]
                          [fd (into {} (map (fn [[filt e]]
                                              [filt {:ready (:ready e)
                                                     :waiters (count (:waiters e))}])
                                            per-filt))])
                        (:fds s)))}))

(defn- ensure-started! []
  ;; under pm. One poller thread per process, started on the first fiber wait.
  (when-not (:started? @state)
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
            (future (poller-loop kq))))
        (finally (ffi/free pfds))))))

;; -- the wait API --------------------------------------------------------------
;; (wait-ready fd :read|:write) -> void. Fiber-aware: parks the current fiber on
;; readiness and returns when the poller wakes it; on a plain thread, blocks in
;; a private kevent/epoll_wait (level-triggered, so a readiness that raced the
;; registration fires immediately — no missed wakeup).
(defn wait-fiber [fd filt]
  (let [park? (locking pm
                (ensure-started!)
                (let [e (get-in @state [:fds fd filt])]
                  (if (and e (:ready e))
                    (do (swap! stale-consumes inc)
                        (swap! state assoc-in [:fds fd filt :ready] false) false)
                    (do (swap! state assoc-in [:fds fd filt]
                               {:waiters (conj (or (:waiters e) []) (jolt.host/current-fiber))
                                :ready false})
                        ;; The guard is per (fd, FILTER). Keyed by fd alone it
                        ;; skipped the registration whenever the OTHER direction of
                        ;; the same fd was already queued, and that filter then never
                        ;; reached the kernel at all.
                        (when-not (contains? (get (:pending @state) fd #{}) filt)
                          (swap! state update-in [:pending fd] (fnil conj #{}) filt)
                          (pipe-write!))
                        (jolt.host/fiber-park-commit!)
                        true))))]
    (when park?
      (jolt.host/fiber-to-scheduler!))))

(defn wait-thread [fd filt]
  (if macos?
    (let [kq (c-kqueue) ch (ffi/alloc KEVENT-SIZE) ev (ffi/alloc KEVENT-SIZE)]
      (try
        (kevent-put! ch 0 fd (if (= filt :read) EVFILT-READ EVFILT-WRITE) EV-ADD)
        (loop []
          (when (neg? (c-kevent kq ch 1 ev 1 ffi/null)) (recur)))
        (finally (ffi/free ch) (ffi/free ev) (c-close kq))))
    (let [ep (c-epoll-create1 0) ev (ffi/alloc EPOLL-EVENT-SIZE)]
      (try
        (ffi/write ev :uint (if (= filt :read) EPOLLIN EPOLLOUT))
        (ffi/write ev :uint fd EPOLL-DATA-OFFSET)
        (ffi/write ev :uint 0 (+ EPOLL-DATA-OFFSET 4))
        (c-epoll-ctl ep EPOLL-ADD fd ev)
        (loop []
          (when (neg? (c-epoll-wait ep ev 1 -1)) (recur)))
        (finally (ffi/free ev) (c-close ep))))))

(defn wait-ready [fd filt]
  (if (jolt.host/fiber?)
    (wait-fiber fd filt)
    (wait-thread fd filt)))
