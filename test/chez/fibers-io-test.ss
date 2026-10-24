;; test/chez/fibers-io-test.ss — R8 gate: transparent IO parking (sockets +
;; poller half, epic jolt-nvpr.8). Run: chez --script test/chez/fibers-io-test.ss
;; (wired into `make fibers` after the R5 pool gate).
;;
;; R8 (fibers-r8-io.md) delivers the JDK's trick: every blocking socket call with
;; a non-blocking OS equivalent uses the non-blocking one when called from a
;; fiber, and blocks the OS thread otherwise. Same user-facing code either way.
;; stdlib/jolt/socket.clj sets O_NONBLOCK on every fd it hands out; on EAGAIN it
;; asks jolt.io-poller to wait for readiness — parking the fiber (jolt.host
;; seams from fibers-async.ss) when there is a current fiber, and blocking in a
;; private kevent/epoll_wait on this thread when there is not. One poller thread
;; per process (kqueue on macOS, epoll on Linux), its blocking wait declared
;; :blocking (__collect_safe).
;;
;; Why this matters HERE more than on the JVM: a blocking call PINTS its carrier,
;; and since continuations cannot migrate (R0(d)), the fibers queued behind it
;; are stranded and adding carriers cannot rescue them. Gate 1 is the whole
;; round in one check; gate 3 is the collect-safety check — a poller whose wait
;; is not collect-safe stops the WHOLE PROCESS from collecting for as long as it
;; waits, which is nearly always.
;;
;; Gate checks (spec, in order; item 6 — file offload — is the other half of the
;; round and is NOT in scope here):
;;   1. a fiber reading a socket with no data available PARKS, and a sibling
;;      fiber on the SAME carrier makes progress while it waits — the whole
;;      round in one check (red without the R8 socket layer).
;;   2. the same code path on a plain OS thread still blocks and still works.
;;   3. a FULL COLLECT succeeds while the poller is blocked in kevent/
;;      epoll_wait (the __collect_safe check — asserted, not assumed).
;;   4. 8 fibers each doing a socket round trip concurrently on ONE carrier:
;;      all 8 park at once, then all 8 complete with their own payloads — real
;;      parking, not serialized blocking.
;;   5. accept parks too: a fiber blocked in accept resumes when a connection
;;      arrives.
;;   6. existing jolt.socket behaviour off a fiber is untouched: a thread-mode
;;      round trip + close semantics, unchanged (the full socket-test.clj suite
;;      keeps running under `make test` / smoke).
;;
;; The gate loads the REAL loader (gate-boot + loader.ss + ffi.ss, in cli.ss's
;; order) so `require 'jolt.socket` resolves the real stdlib stack with its
;; transitive requires — the same combination the production CLI has. Timing is
;; only ever a floor relative to an in-run delay (gate 2), never an absolute
;; ceiling — see the flake history note at the top of fibers-test.ss.
;;
;; Unlike the R1-R5 gates (which pin the pool to 1 for determinism and drive
;; everything through sa-fiber-run-all), this gate runs with the R4/R5 carrier
;; THREAD live (jolt-fiber-ensure-carrier!): the poller's wake resumes a parked
;; fiber by enqueueing it on its carrier, and only the live carrier loop will
;; pick it up. It never pumps sa-fiber-run-all once the carrier is live.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
;; The real loader, in cli.ss's order: loader.ss seeds its loaded-ns from the
;; vars that exist AT LOAD TIME, so jolt.ffi's host vars (java/ffi.ss) must come
;; AFTER it, or a (require '[jolt.ffi]) skips stdlib/jolt/ffi.clj and the
;; defcfn macro never exists. gate-boot's image (target/dev/gate.so, when fresh)
;; already bakes both — the delete below makes the require pull the Clojure
;; side in that case too, so the gate behaves identically with and without the
;; image.
(load "host/chez/loader.ss")
(hashtable-delete! loaded-ns "jolt.ffi")
(set-source-roots!* '("jolt-core" "stdlib" "vendor/fs/src" "vendor/process/src" "vendor/cli/src" "vendor/grenadine/src"
                      "vendor/grenadine-generated"))
(load "host/chez/java/ffi.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(define (mono-nanos)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000000000 (time-second t)) (time-nanosecond t))))
(define (now-secs) (/ (exact->inexact (mono-nanos)) 1000000000.0))

;; Bounded wait: a bug must FAIL the gate, never hang it.
(define (wait-until pred secs what)
  (let ((deadline (+ (now-secs) secs)))
    (let loop ()
      (if (pred)
          #t
          (if (> (now-secs) deadline)
              (begin (set! fails (+ fails 1))
                     (printf "  FAIL: ~a (timed out)\n" what) #f)
              (begin (sleep (make-time 'time-duration 1000000 0)) (loop)))))))

;; Compile+eval one jolt form in the "user" ns and return its value.
(define (ev s) (jolt-compile-eval s "user"))
(define (jv-nth v i) (pvec-nth-d v i jolt-nil))
(define (all-done? fs)
  (or (null? fs) (and (eq? (jolt-fiber-state (car fs)) 'done) (all-done? (cdr fs)))))
(define (all-parked? fs)
  (or (null? fs) (and (eq? (jolt-fiber-state (car fs)) 'parked) (all-parked? (cdr fs)))))
(define (spawn-n n mk)
  (let loop ((i 0) (acc '()))
    (if (fx<? i n)
        (loop (fx+ i 1) (cons (mk i) acc))
        (reverse acc))))

(ev "(require 'jolt.socket)")
(ev "(require 'jolt.io-poller)")
(ev "(require '[clojure.core.async :refer [go chan <!! >!! *go-backend*]])")

;; One carrier, started as a real carrier thread — the R8 wake path (poller ->
;; sa-fiber-resume -> carrier run queue) needs it live. Never reset mid-file.
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-pool-reset!)
(jolt-fiber-ensure-carrier!)

(printf "== fibers-io: transparent socket parking on a fiber ==\n")

;; Every listener here binds the loopback address its clients dial. A wildcard
;; listener on port P does not own 127.0.0.1:P, and another process listening
;; there (a long-running nREPL server) took the connections: the accept waited
;; forever, about one run in 600 on a machine with one.
;; --- 1. THE whole round: a read with no data parks; a sibling progresses ------
(printf "\n== 1. a fiber read with no data parks; a sibling on the same carrier runs ==\n")
(ev "(def r8-ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress)))")
(ev "(def r8-port (.getLocalPort r8-ss))")
(ev "(def r8-client (java.net.Socket. \"127.0.0.1\" r8-port))")
(ev "(def r8-srv (.accept r8-ss))")
(ev "(def r8-srv-out (.getOutputStream r8-srv))")
;; the read thunk: one recv on a socket with NOTHING in it -> EAGAIN -> park
(define read-thunk
  (ev "(fn [] (let [c r8-client b (byte-array 64) n (.read (.getInputStream c) b 0 64)] (String. b 0 n \"UTF-8\")))"))
(define fa (sa-fiber-spawn read-thunk))
(define fb (sa-fiber-spawn (lambda () 4242)))
(ok "1. the two fibers share one carrier" (eq? (jolt-fiber-carrier fa) (jolt-fiber-carrier fb)))
(define fa-parked
  (wait-until (lambda () (eq? (jolt-fiber-state fa) 'parked)) 15.0 "1. the read fiber parked"))
(ok "1. the socket read parked the fiber (no data available)" fa-parked)
(define fb-done
  (wait-until (lambda () (eq? (jolt-fiber-state fb) 'done)) 15.0 "1. the sibling completed"))
(ok "1. the sibling fiber on the SAME carrier made progress while the read was parked"
    (and fb-done (eq? (jolt-fiber-state fa) 'parked)))
(ok "1. the sibling ran to completion" (eqv? (jolt-fiber-result fb) 4242))
(ev "(.write r8-srv-out (.getBytes \"pong\" \"UTF-8\") 0 4)")
(define fa-done
  (wait-until (lambda () (eq? (jolt-fiber-state fa) 'done)) 15.0 "1. the read resumed"))
(ok "1. the parked read resumed with the data"
    (and fa-done (string=? (jolt-fiber-result fa) "pong")))

;; --- 2. the same code path on a plain OS thread blocks and works --------------
(printf "\n== 2. the thread path still blocks and still works ==\n")
(ev "(def r8-ss2 (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress)))")
(ev "(def r8-port2 (.getLocalPort r8-ss2))")
(ev "(def r8-client2 (java.net.Socket. \"127.0.0.1\" r8-port2))")
(ev "(def r8-srv2 (.accept r8-ss2))")
;; a real OS thread delivers the data 300 ms late. The clock starts BEFORE the
;; writer exists, so the data cannot be there until 300 ms after t2 and a read
;; that waited for it takes at least that long, however loaded the machine (load
;; only stretches it). Started after the future, as it was, the floor had to
;; leave room for the main thread being descheduled between the two, and was
;; half the delay.
(define t2 (mono-nanos))
(ev "(def r8-w (future (Thread/sleep 300) (.write (.getOutputStream r8-srv2) (.getBytes \"threaded\" \"UTF-8\") 0 8)))")
(define r2
  (ev "(let [b (byte-array 64) n (.read (.getInputStream r8-client2) b 0 64)] (String. b 0 n \"UTF-8\"))"))
(define el2 (/ (exact->inexact (- (mono-nanos) t2)) 1000000.0))
(ok "2. the thread-mode read returned the delayed data" (string=? r2 "threaded"))
;; a read that returned without waiting for the data (a spin, or a wrong
;; EAGAIN-as-EOF) clocks in under the writer's delay
(ok "2. it actually waited for the writer (>= the 300 ms delay)"
    (>= el2 300.0))
(printf "   thread-path read took ~,1f ms for a 300 ms delayed write\n" el2)

;; --- 3. a full collect succeeds while the poller is blocked -------------------
(printf "\n== 3. a full collect succeeds while the poller is blocked in kevent/epoll_wait ==\n")
(ev "(def r8-ss3 (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress)))")
(ev "(def r8-port3 (.getLocalPort r8-ss3))")
(ev "(def r8-client3 (java.net.Socket. \"127.0.0.1\" r8-port3))")
(ev "(def r8-srv3 (.accept r8-ss3))")
(define read3
  (ev "(fn [] (let [c r8-client3 b (byte-array 64) n (.read (.getInputStream c) b 0 64)] (String. b 0 n \"UTF-8\")))"))
(define fc (sa-fiber-spawn read3))
(define fc-parked
  (wait-until (lambda () (eq? (jolt-fiber-state fc) 'parked)) 15.0 "3. the read fiber parked"))
(ok "3. the read fiber parked" fc-parked)
;; the poller entered its blocking wait (jolt.io-poller/waits increments each
;; round, immediately before kevent/epoll_wait) — so the collect below races a
;; thread that is (about to be / already) inside the blocking foreign call
(define w3
  (wait-until (lambda () (> (ev "@jolt.io-poller/waits") 0)) 15.0 "3. the poller entered its wait"))
(ok "3. the poller entered its blocking wait" w3)
(sleep (make-time 'time-duration 50000000 0))  ; let it settle into the wait
;; An explicit collect also refuses while ANY other thread is briefly active, and
;; one can be: test 2's future leaves an idle pool worker that spins on yields
;; before it parks, which on a loaded runner can outlast the settle above. So the
;; collect is retried for a while. A poller whose wait is not collect-safe stays
;; in that wait for the whole window (nothing is written until after this check),
;; so it fails every attempt; only a bystander that goes quiet lets one through.
(define collect-ok
  (let retry ((n 0))
    (guard (e (#t (if (fx<? n 40)
                      (begin (sleep (make-time 'time-duration 50000000 0)) (retry (fx+ n 1)))
                      (begin (printf "  FAIL detail: collect raised ~a (~a attempts)\n"
                                     (condition-message e) (fx+ n 1))
                             #f))))
      (collect (collect-maximum-generation))
      #t)))
(ok "3. a full collect succeeded while the poller was blocked (wait is collect-safe)"
    collect-ok)
(ok "3. the parked fiber survived the collection" (eq? (jolt-fiber-state fc) 'parked))
(ev "(.write (.getOutputStream r8-srv3) (.getBytes \"z\" \"UTF-8\") 0 1)")
(define fc-done
  (wait-until (lambda () (eq? (jolt-fiber-state fc) 'done)) 15.0 "3. the read resumed after the collect"))
(ok "3. the fiber completed after the collect"
    (and fc-done (string=? (jolt-fiber-result fc) "z")))

;; --- 4. 8 fibers, one carrier: all park at once, all round-trip ---------------
(printf "\n== 4. 8 fibers, one carrier, all parked simultaneously, all round-trip ==\n")
(define r8-n 8)
(ev "(def r8-ss4 (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress)))")
(ev "(def r8-port4 (.getLocalPort r8-ss4))")
(ev "(def r8-clients (atom []))")
(ev "(dotimes [i 8] (swap! r8-clients conj (java.net.Socket. \"127.0.0.1\" r8-port4)))")
(ev "(def r8-srvs (atom []))")
(ev "(dotimes [i 8] (swap! r8-srvs conj (.getOutputStream (.accept r8-ss4))))")
(define r8-worker
  (ev "(fn [i] (let [c (nth @r8-clients i) b (byte-array 64) n (.read (.getInputStream c) b 0 64)] (String. b 0 n \"UTF-8\")))"))
(define f4s (spawn-n r8-n (lambda (i) (sa-fiber-spawn (lambda () (r8-worker i))))))
;; serialized blocking could never get here: with the carrier pinned by one
;; blocking recv, at most ONE fiber is 'parked at a time and the rest sit
;; 'ready behind it. All eight 'parked at one instant proves real parking.
(define all-parked4
  (wait-until (lambda () (all-parked? f4s)) 20.0 "4. all 8 fibers parked on their reads"))
(ok "4. all 8 fibers parked simultaneously on one carrier (real parking, not serialized blocking)"
    all-parked4)
(ev "(dotimes [i 8] (.write (nth @r8-srvs i) (.getBytes (str \"m\" i) \"UTF-8\") 0 2))")
(define all-done4
  (wait-until (lambda () (all-done? f4s)) 20.0 "4. all 8 round trips completed"))
(ok "4. all 8 round trips completed" all-done4)
;; A fiber that never resumed has #f for a result, and string=? on it RAISES —
;; which turned an already-reported timeout into a crash that ended the whole
;; fibers gate and buried the failure. Guard on all-done4 like the other result
;; reads, and compare only strings; anything else is a failed check, not an
;; exception.
(ok "4. every fiber got its own payload"
    (and all-done4
         (let loop ((i 0))
           (or (fx=? i r8-n)
               (let ((r (jolt-fiber-result (list-ref f4s i))))
                 (and (string? r)
                      (string=? r (string-append "m" (number->string i)))
                      (loop (fx+ i 1))))))))

;; --- 5. accept parks too -------------------------------------------------------
(printf "\n== 5. a fiber blocked in accept parks and resumes on a connection ==\n")
(ev "(def r8-ss5 (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress)))")
(ev "(def r8-port5 (.getLocalPort r8-ss5))")
(define accept-thunk
  (ev "(fn [] (let [s (.accept r8-ss5)] (if (.isConnected s) :accepted :no)))"))
(define fd (sa-fiber-spawn accept-thunk))
(define fd-parked
  (wait-until (lambda () (eq? (jolt-fiber-state fd) 'parked)) 15.0 "5. accept parked"))
(ok "5. a fiber blocked in accept parked" fd-parked)
(ev "(java.net.Socket. \"127.0.0.1\" r8-port5)")   ; the arriving connection
(define fd-done
  (wait-until (lambda () (eq? (jolt-fiber-state fd) 'done)) 15.0 "5. accept resumed"))
(ok "5. accept resumed with a connection"
    (and fd-done (eq? (jolt-fiber-result fd) (keyword #f "accepted"))))

;; --- 6. jolt.socket off a fiber is unchanged ----------------------------------
(printf "\n== 6. existing socket behaviour off a fiber is untouched ==\n")
(define r6 (ev "
(let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
      port (.getLocalPort ss)
      c (java.net.Socket. \"127.0.0.1\" port)
      s (.accept ss)
      out (.getOutputStream c)
      b1 (byte-array 16) b2 (byte-array 16)]
  (.write out (.getBytes \"hello over tcp\" \"UTF-8\") 0 14)
  (let [n1 (.read (.getInputStream s) b1 0 16)
        m1 (String. b1 0 n1 \"UTF-8\")]
    (.write (.getOutputStream s) (.getBytes \"pong\" \"UTF-8\") 0 4)
    (let [n2 (.read (.getInputStream c) b2 0 16)
          m2 (String. b2 0 n2 \"UTF-8\")]
      (let [r [m1 m2 (.isConnected c) (.isConnected s)
               (do (.close s) (.close c) (.close ss)
                   [(.isClosed c) (.isClosed s) (.isClosed ss)])]]
        r))))"))
(ok "6. client->server round trip" (string=? (jv-nth r6 0) "hello over tcp"))
(ok "6. server->client round trip" (string=? (jv-nth r6 1) "pong"))
(ok "6. both sides connected" (and (eq? (jv-nth r6 2) #t) (eq? (jv-nth r6 3) #t)))
(ok "6. close marks the sockets closed"
    (jolt=2 (jv-nth r6 4) (jolt-vector #t #t #t)))

;; --- 7. a timed wait that ends early drops its deadline -------------------------
;; A fiber's timed wait arms the shared timer; one that ends before the deadline
;; must take that entry back out. Left behind, it pins the fiber (through the
;; timer's thunk) until the deadline, so a server with a long SO_TIMEOUT held
;; one dead fiber per parked read for the whole timeout (jolt-lang/jolt#1191
;; follow-up). Counted directly on the heap: every wait below parks for real
;; and is answered long before its hour-long deadline.
(printf "\n== 7. timed fiber waits answered early leave no timer behind ==\n")
(define (pending-timers) (jolt-with-mutex timeout-mu (timeout-pending-count)))
(define base-timers (pending-timers))
(ev "(def t7-ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress)))")
(ev "(def t7-client (java.net.Socket. \"127.0.0.1\" (.getLocalPort t7-ss)))")
(ev "(def t7-srv (.accept t7-ss))")
(ev "(.setSoTimeout t7-client 3600000)")
(define t7-read (ev "(fn [] (.read (.getInputStream t7-client)))"))
(define t7-write (ev "(fn [] (.write (.getOutputStream t7-srv) 7))"))
(define t7-reads
  (let loop ((i 0) (acc '()))
    (if (= i 20)
        (reverse acc)
        (let ((f (sa-fiber-spawn t7-read)))
          (wait-until (lambda () (eq? (jolt-fiber-state f) 'parked)) 15.0 "7. timed read parked")
          (t7-write)
          (wait-until (lambda () (eq? (jolt-fiber-state f) 'done)) 15.0 "7. timed read resumed")
          (loop (+ i 1) (cons (jolt-fiber-result f) acc))))))
(ok "7a. every timed read got its byte" (for-all (lambda (r) (eqv? r 7)) t7-reads))
(ok "7b. and no read left its deadline pending" (= base-timers (pending-timers)))
(ev "(do (.close t7-client) (.close t7-srv) (.close t7-ss))")
;; the same seam under a timed deref, which parks through jolt-cv-wait
(ev "(def t7-p (promise))")
(define t7-deref (sa-fiber-spawn (ev "(fn [] (deref t7-p 3600000 :timed-out))")))
(wait-until (lambda () (eq? (jolt-fiber-state t7-deref) 'parked)) 15.0 "7. timed deref parked")
(ev "(deliver t7-p :delivered)")
(wait-until (lambda () (eq? (jolt-fiber-state t7-deref) 'done)) 15.0 "7. timed deref resumed")
(ok "7c. the timed deref got the value"
    (eq? (jolt-fiber-result t7-deref) (keyword #f "delivered")))
(ok "7d. and left no deadline pending" (= base-timers (pending-timers)))

;; --- 8. a raise the wait survives keeps its deadline -----------------------------
;; The timed wait drops its deadline when a raise passes through it. A raise
;; that a handler further out RESUMES does not end the wait, so the deadline has
;; to come back, or the fiber parks with nothing left to wake it at the deadline.
(printf "\n== 8. a resumed raise inside a timed fiber wait keeps its deadline ==\n")
(define t8-mu (make-mutex))
(define t8-cv (jolt-make-cv))
(define t8-raised? #f)
(define t8
  (sa-fiber-spawn
    (lambda ()
      (with-exception-handler
        (lambda (c) 'resumed)
        (lambda ()
          (jolt-cv-wait t8-mu t8-cv (+ (mono-millis) 200)
            (lambda (timed-out?)
              (unless t8-raised?
                (set! t8-raised? #t)
                (raise-continuable 'passing-through))
              (if timed-out? 'timed-out jolt-cv-again))))))))
(wait-until (lambda () (eq? (jolt-fiber-state t8) 'done)) 15.0 "8. resumed wait still timed out")
(ok "8a. the wait answered at its deadline" (eq? (jolt-fiber-result t8) 'timed-out))
(ok "8b. and left no deadline pending" (= base-timers (pending-timers)))

;; --- 9. a condition keeps its own waiters (#1311) -------------------------------
;; A jolt-cv holds the fibers parked on it and a count of the threads blocked on
;; it, so a wake takes no lock but the waiters' mu and skips the broadcast when no
;; thread waits. Neither kind of waiter may be lost to that, a wake must leave
;; another condition's fibers where they are, and both records must drain.
(printf "\n== 9. a condition keeps its own waiters ==\n")
(define t9-mu (make-mutex))
(define t9-cv (jolt-make-cv))
(define t9-other (jolt-make-cv))
(define t9-ready? #f)
(define t9
  (sa-fiber-spawn
    (lambda ()
      (jolt-cv-wait t9-mu t9-cv #f
        (lambda (_) (if t9-ready? 'woken jolt-cv-again))))))
(wait-until (lambda () (eq? (jolt-fiber-state t9) 'parked)) 15.0 "9. fiber parked on the condition")
(ok "9a. the parked fiber is listed on its condition"
    (jolt-with-mutex t9-mu (equal? (jolt-cv-fibers t9-cv) (list t9))))
(jolt-with-mutex t9-mu (jolt-cv-wake! t9-other))
(ok "9b. a wake on another condition leaves it parked"
    (and (eq? (jolt-fiber-state t9) 'parked)
         (jolt-with-mutex t9-mu (equal? (jolt-cv-fibers t9-cv) (list t9)))))
(jolt-with-mutex t9-mu (set! t9-ready? #t) (jolt-cv-wake! t9-cv))
(wait-until (lambda () (eq? (jolt-fiber-state t9) 'done)) 15.0 "9. parked fiber resumed")
(ok "9c. the wake reached the parked fiber" (eq? (jolt-fiber-result t9) 'woken))
(ok "9d. and drained the list" (jolt-with-mutex t9-mu (null? (jolt-cv-fibers t9-cv))))
(define t9-thread-ready? #f)
(define t9-thread-result #f)
(fork-thread
  (lambda ()
    (set! t9-thread-result
      (jolt-cv-wait t9-mu t9-cv #f
        (lambda (_) (if t9-thread-ready? 'thread-woken jolt-cv-again))))))
(wait-until (lambda () (jolt-with-mutex t9-mu (fx=? 1 (jolt-cv-threads t9-cv)))) 15.0
            "9. thread blocked on the condition")
(jolt-with-mutex t9-mu (set! t9-thread-ready? #t) (jolt-cv-wake! t9-cv))
(wait-until (lambda () t9-thread-result) 15.0 "9. blocked thread woken")
(ok "9e. the wake reached the blocked thread" (eq? t9-thread-result 'thread-woken))
(ok "9f. and its count came back to zero" (jolt-with-mutex t9-mu (fx=? 0 (jolt-cv-threads t9-cv))))
;; jolt-pause-ms parks a fiber on a private condition of its own; that one has to
;; be a jolt-cv too, or every fiber pause (Process.waitFor's poll) raised.
(define t9-pause (sa-fiber-spawn (lambda () (jolt-pause-ms 5) 'paused)))
(wait-until (lambda () (eq? (jolt-fiber-state t9-pause) 'done)) 15.0 "9. pausing fiber finished")
(ok "9g. a fiber pause parks and comes back" (eq? (jolt-fiber-result t9-pause) 'paused))

;; An interrupt ends a fiber's park without a wake, so the wake that would have
;; drained the fiber from its condition never comes: the wait has to take itself
;; off, or the condition holds the dead wait's continuation until its next wake.
(define t9-never (jolt-make-cv))
(define t9-int
  (sa-fiber-spawn
    (lambda ()
      (guard (e (#t 'interrupted))
        (jolt-cv-wait t9-mu t9-never #f (lambda (_) jolt-cv-again))))))
(wait-until (lambda () (eq? (jolt-fiber-state t9-int) 'parked)) 15.0 "9. fiber parked to be interrupted")
(jolt-fiber-interrupt! t9-int 'stop)
(wait-until (lambda () (eq? (jolt-fiber-state t9-int) 'done)) 15.0 "9. interrupted fiber finished")
(ok "9h. the interrupt ended the wait" (eq? (jolt-fiber-result t9-int) 'interrupted))
(ok "9i. and took the fiber off its condition"
    (jolt-with-mutex t9-mu (null? (jolt-cv-fibers t9-never))))

(printf "\nfibers-io: ~a checks, ~a failures\n" total fails)
(exit (if (zero? fails) 0 1))
