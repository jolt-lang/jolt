;; test/chez/fibers-go-test.ss — R4 gate: `go` on fibers, and `alts!` as a
;; wait set (epic jolt-nvpr.5). Run: chez --script test/chez/fibers-go-test.ss
;; (wired into `make fibers`).
;;
;; R4 (fibers-r4-go.md) delivers two things:
;;   1. clojure.core.async/go on the fiber backend, opt-in via the dynamic var
;;      *go-backend* (:thread default, :fiber opt-in) read AT SPAWN TIME. The
;;      default stays byte-for-byte today's go (a real OS thread). Parking works
;;      ACROSS FUNCTION BOUNDARIES — a go body calling a function that calls a
;;      function that does <! — the headline capability. The carrier is exactly
;;      ONE OS thread (fibers.ss), started lazily on the first :fiber spawn,
;;      parked on a condition when the run queue is empty (never a spin). No
;;      carrier pool — that is R5.
;;   2. alts! as a WAIT SET: one handler registered across all ports, committing
;;      exactly once via alt-claim!; both waiter kinds wake (a thread waiter is
;;      signalled via the condvar, a fiber waiter is resumed); the immediate
;;      case stays immediate and allocation-free; a losing registration is
;;      PRUNED so a long-lived channel does not accumulate dead handlers.
;;
;; Sections 1-3 run real jolt code through the real compiler (gate-boot.ss =
;; full runtime + compile-eval): (binding [*go-backend* :fiber] (go ...)).
;; The alts! sections drive the HOST seam — jolt-async-do-alts, jolt-async-*,
;; async-chan-alt-takers — the same style as fibers-chan-test.ss. The gate boot
;; deliberately stops at compile-eval.ss (no loader), so the overlay names
;; (alts!, poll!) are brought in explicitly: the gate loads
;; stdlib/clojure/core/async.clj via load-string ONCE, up front — the same
;; host+overlay combination the production loader always has — because the
;; :default shortcut exists only in the overlay's do-alts and has no host seam.
;; The overlay is additive (it wraps host seams, never redefines a host
;; binding), so loading it first changes nothing for the host-seam sections. It
;; is also where `go` and `go-loop` are DEFINED — the host provides go-spawn and
;; nothing above it — so a gate that wants either must load it, as this one does.
;;
;; This file never pumps sa-fiber-run-all once the R4 carrier is live (a manual
;; pump would race the carrier over the shared queue).
;;
;; Gate scenarios (spec, in order):
;;   1. a :fiber go body runs to completion and its value lands on the channel
;;   2. parking through TWO NESTED CALLS (the differentiator vs the JVM's
;;      state-machine go), plus loop/recur, plus inside a dynamic-wind-carrying
;;      form (try/finally — R2: every park fires a handler pair, so the after
;;      thunk must run once at real exit, not at park)
;;   3. :thread unchanged: the same body under both backends, same result
;;   4. alts! wait set: value arrives on the second port; the waiter wakes with
;;      the right [val port] — once as a thread waiter, once as a fiber waiter
;;      (and the fiber waiter parks: a sibling fiber still runs meanwhile)
;;   5. alts! commits exactly once when several ports become ready at once
;;   6. a losing registration is pruned (assert the waiter list length)
;;   7. mixed take/put specs, :priority true, :default
;;
;; No timing or memory numbers in this round: do-alts was already a registered
;; single-handler wait before R4 — the 1ms polling loop the R4 spec draft
;; described never existed in this codebase (review correction), so there is no
;; before/after latency to report; R0/R3 measured the fiber machinery already.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

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

;; the ::none sentinel jolt-async-poll! returns for an empty-but-open channel
(define async-none (keyword "clojure.core.async" "none"))
;; host-level non-blocking poll: #f if nothing there (empty or closed)
(define (host-poll! ch)
  (let ((v (jolt-async-poll! ch)))
    (if (or (eq? v async-none) (eq? v jolt-nil)) #f v)))

;; Load the real async overlay up front, exactly as the production loader
;; does (host async.ss + stdlib/clojure/core/async.clj together). It is
;; ADDITIVE — portable dataflow ops (alts!, alts!!, pipe, thread-call, ...)
;; wrapping the host seams (__do-alts, __poll!, go-spawn, ...) — it never
;; redefines a host binding, so nothing in this file changes meaning. Loading
;; it once at the top makes the gate's clojure.core.async surface match
;; production for every section instead of being a mid-test environment
;; change (and makes alts!/poll! referable here).
(load "host/chez/gate-async-overlay.ss")
(gate-load-async-overlay!)

;; The jolt-level names sections 1-3 use (a refer; unqualified *go-backend*
;; binding needs it referred, exactly as in real Clojure). alts!/poll! resolve
;; from the overlay loaded above.
(ev "(require '[clojure.core.async
                :refer [go go-loop chan <! >! <!! >!! close! poll!
                        thread alts! *go-backend*]])")

(printf "== R4: go on fibers, alts! as a wait set ==\n")

;; --- 1. a :fiber go body runs to completion; value lands on the channel ------
;; The same shape under the default :thread backend first (the no-touch
;; baseline), then the opt-in :fiber backend. Note the FIRST :fiber spawn
;; starts the R4 carrier lazily and it stays live for the rest of the file.
(printf "\n== 1. go body runs, value lands on the channel ==\n")
(define r1a (ev "(<!! (go 42))"))
(ok "1a. default :thread backend value" (= r1a 42))
(define r1b (ev "(binding [*go-backend* :fiber] (<!! (go 42)))"))
(ok "1b. :fiber backend value" (= r1b 42))

;; --- 2. parking through two nested calls, loop/recur, dynamic-wind -----------
(printf "\n== 2. parking across function boundaries ==\n")
(ev "(defn r4-inner [c] (<! c))")
(ev "(defn r4-mid [c] (r4-inner c))")
;; The go body calls r4-mid, which calls r4-inner, which parks. The >!! on the
;; main thread blocks until the fiber's registered taker claims it — proof the
;; park happened several frames deep — and the go result channel carries the
;; value back. The JVM's state-machine go cannot express this at all.
(define r2 (ev
  "(binding [*go-backend* :fiber]
     (let [c (chan)
           g (go (r4-mid c))]
       (>!! c 99)
       (<!! g)))"))
(ok "2a. park through two nested calls resumes with the value" (eqv? r2 99))

;; loop/recur inside a fiber go body
(define r2b (ev "
(binding [*go-backend* :fiber]
  (let [c (chan 5)
        g (go-loop [n 0]
            (when (< n 5)
              (>! c n)
              (recur (inc n))))]
    (<!! g)                ; the loop is done putting only when g closes
    (close! c)
    (loop [acc []]
      (let [v (<!! c)]
        (if (nil? v) acc (recur (conj acc v)))))))"))
(ok "2b. go-loop on a fiber drained 5 values" (eqv? (jv-nth r2b 0) 0))
(ok "2b. go-loop order preserved" (eqv? (jv-nth r2b 4) 4))
(ok "2b. go-loop length" (= (jolt-count r2b) 5))

;; try/finally lowers to dynamic-wind, and a park is a continuation escape, so
;; the after-thunk fires on the park unless something stops it — which would run
;; the cleanup mid-operation (with-open closing a file still in use). The
;; after-thunk is guarded by jolt-park-unwinding? (values.ss), set only around a
;; park escape.
;;
;; The park is FORCED here, and that matters: the first version of this check let
;; the >!! race the fiber, so locally the value was ready before the fiber ever
;; parked, no park happened, and the check passed while the bug was live. Only
;; the slower CI runner parked first and caught it. Assert the log is EMPTY after
;; the park, then exactly [:finally] after the exit — the empty-after-park half is
;; the one with teeth.
(define r2c (ev "
(binding [*go-backend* :fiber]
  (let [c (chan)
        log (atom [])
        g (go (try (let [v (<! c)] v)
                   (finally (swap! log conj :finally))))]
    (Thread/sleep 300)                  ;; the fiber is parked on the <! by now
    (let [at-park (count @log)]
      (>!! c :got)
      (let [r (<!! g)]
        [r (count @log) @log at-park])))))"))
(ok "2c. try/finally body value" (jolt=2 (jv-nth r2c 0) (keyword #f "got")))
(ok "2c. finally did NOT run at the park" (= (jv-nth r2c 3) 0))
(ok "2c. finally ran exactly once, at the real exit" (= (jv-nth r2c 1) 1))
(ok "2c. finally ran with the right content"
    (jolt=2 (jv-nth r2c 2) (jolt-vector (keyword #f "finally"))))

;; A real exit AFTER a park must still run the finally — this is what proves the
;; park flag is cleared on resume rather than left set for the rest of the fiber.
;; Without it, suppressing the park would silently suppress every later exit too.
(define r2d (ev "
(binding [*go-backend* :fiber]
  (let [c (chan)
        log (atom [])
        g (go (try (let [v (<! c)] (throw (ex-info \"after the park\" {:v v})))
                   (catch Exception e :caught)
                   (finally (swap! log conj :finally))))]
    (Thread/sleep 300)
    (>!! c :go)
    (let [r (<!! g)] [r @log])))"))
(ok "2d. throw after a park still caught" (jolt=2 (jv-nth r2d 0) (keyword #f "caught")))
(ok "2d. finally still runs on a real exit after a park"
    (jolt=2 (jv-nth r2d 1) (jolt-vector (keyword #f "finally"))))

;; --- 3. :thread is unchanged: same body under both backends, same result -----
(printf "\n== 3. same body, both backends, same result ==\n")
(ev "(defn r4-same []
      (let [c (chan)
            g (go (let [x (<! c) y (<! c)] (+ x y)))]
        (>!! c 20)
        (>!! c 22)
        (<!! g)))")
(define r3t (ev "(r4-same)"))
(define r3f (ev "(binding [*go-backend* :fiber] (r4-same))"))
(ok "3. :thread result" (= r3t 42))
(ok "3. :fiber result" (= r3f 42))
(ok "3. identical under both backends" (= r3t r3f))

;; --- 4. alts! wait set: value on the second port, both waiter kinds ----------
;; Host seam (see header): jolt-async-do-alts registers ONE handler across all
;; ports; a deliverer claims it, writes the mailbox, and wakes the waiter — a
;; thread waiter via the condvar, a fiber waiter via the R3 resume hook.
(printf "\n== 4. alts! wait set, thread and fiber waiters ==\n")
;; thread waiter: value arrives on the SECOND port; the waiter wakes with
;; [val port]. The registration is asserted on both ports before the give.
(define w4a (jolt-vector (jolt-async-chan) (jolt-async-chan)))
(define r4a 'unset)
(fork-thread (lambda () (set! r4a (jolt-async-do-alts w4a #f))))
(wait-until (lambda () (and (pair? (async-chan-alt-takers (jv-nth w4a 0)))
                            (pair? (async-chan-alt-takers (jv-nth w4a 1)))))
            5.0 "4a. handler registered on both ports")
(sleep (make-time 'time-duration 80000000 0))
(jolt-async-give (jv-nth w4a 1) (keyword #f "from-b"))
(wait-until (lambda () (not (eq? r4a 'unset))) 5.0 "4a. thread waiter woke")
(ok "4a. thread waiter woke with the second port's value"
    (jolt=2 (jv-nth r4a 0) (keyword #f "from-b")))
(ok "4a. woke on the right port" (eq? (jv-nth r4a 1) (jv-nth w4a 1)))

;; fiber waiter: a go fiber parks in alts! (the handler's wake IS the fiber, so
;; alt-deliver! resumes it); a SIBLING fiber runs to completion meanwhile —
;; proof the alts! parked the fiber instead of blocking the carrier (a
;; condition-wait in the fiber would have stalled the sibling). tick is the
;; sibling's progress; the value is given 80ms later, after both fibers have
;; been scheduled, and the go result channel carries the alts! [val port].
(define w4b (jolt-vector (jolt-async-chan) (jolt-async-chan)))
(define tick (vector 0))
(define g4b (jolt-fiber-go-spawn (lambda () (jolt-async-do-alts w4b #f))))
(define g4b-sib
  (jolt-fiber-go-spawn
   (lambda ()
     (let loop ((n 0))
       (when (fx< n 2000)
         (vector-set! tick 0 (+ 1 (vector-ref tick 0)))
         (loop (fx+ n 1))))
     #t)))
(wait-until (lambda () (and (pair? (async-chan-alt-takers (jv-nth w4b 0)))
                            (pair? (async-chan-alt-takers (jv-nth w4b 1)))))
            5.0 "4b. fiber registered on both ports")
(sleep (make-time 'time-duration 80000000 0))
(jolt-async-give (jv-nth w4b 1) (keyword #f "fb"))
(define r4b 'unset)
(fork-thread (lambda () (set! r4b (jolt-async-take g4b))))
(wait-until (lambda () (not (eq? r4b 'unset))) 5.0 "4b. fiber waiter woke")
(ok "4b. sibling fiber ran while the alts! fiber was parked" (= (vector-ref tick 0) 2000))
(ok "4b. fiber waiter woke with the right value"
    (jolt=2 (jv-nth r4b 0) (keyword #f "fb")))
(ok "4b. woke on the right port" (eq? (jv-nth r4b 1) (jv-nth w4b 1)))

;; --- 5. alts! commits exactly once when several ports become ready -----------
(printf "\n== 5. alts! commits exactly once ==\n")
;; The alts! registers (no value anywhere yet); four givers each put to one
;; channel 40ms later, so all four land after registration. Exactly one port
;; wins the claim (alt-claim! is once per handler); the other three puts stay
;; in their buffers. Each giver signals a completion counter when its give
;; returns, and the leftover count is taken only once all four have landed —
;; poll! removes what it finds, so the count must not race the still-sleeping
;; givers. It must see exactly three values: one (and only one) was consumed.
(define chs5 (vector (jolt-async-chan 1) (jolt-async-chan 1)
                     (jolt-async-chan 1) (jolt-async-chan 1)))
(define r5 'unset)
(fork-thread
  (lambda ()
    (set! r5 (jolt-async-do-alts
              (jolt-vector (vector-ref chs5 0) (vector-ref chs5 1)
                           (vector-ref chs5 2) (vector-ref chs5 3)) #f))))
(wait-until (lambda () (and (pair? (async-chan-alt-takers (vector-ref chs5 0)))
                            (pair? (async-chan-alt-takers (vector-ref chs5 1)))
                            (pair? (async-chan-alt-takers (vector-ref chs5 2)))
                            (pair? (async-chan-alt-takers (vector-ref chs5 3)))))
            5.0 "5. registered on all four ports")
(define given (vector 0 0 0 0))
(let loop ((i 0))
  (when (fx< i 4)
    (let ((ch (vector-ref chs5 i)) (v (+ 10 i)))
      (fork-thread
        (lambda ()
          (sleep (make-time 'time-duration 40000000 0))
          (jolt-async-give ch v)
          (vector-set! given i 1))))
    (loop (fx+ i 1))))
(wait-until (lambda () (and (= (vector-ref given 0) 1)
                            (= (vector-ref given 1) 1)
                            (= (vector-ref given 2) 1)
                            (= (vector-ref given 3) 1)
                            (not (eq? r5 'unset))))
            5.0 "5. all four gives landed and alts completed")
(define left5
  (let loop ((i 0) (acc 0))
    (if (fx= i 4) acc
        (loop (fx+ i 1)
              (if (host-poll! (vector-ref chs5 i)) (fx+ acc 1) acc)))))
(when (not (= left5 3))
  (printf "  (left5 = ~a)\n" left5))
(ok "5. exactly one value consumed (3 left of 4)" (= left5 3))
(ok "5. winner's value is one of the fed values"
    (let ((v (jv-nth r5 0))) (and (>= v 10) (<= v 13))))

;; --- 6. a losing registration is pruned --------------------------------------
;; Host-level, so the waiter LIST LENGTH is asserted directly (the spec: assert
;; the list, not a timing). The alts! registers one handler on both channels;
;; a delivery on a commits the alts!, whose unregister! must remove the handler
;; from b as well — a long-lived channel must not accumulate dead handlers.
(printf "\n== 6. losing registration pruned ==\n")
(define ch6a (jolt-async-chan))
(define ch6b (jolt-async-chan))
(define alts6 #f)
(fork-thread (lambda () (set! alts6 (jolt-async-do-alts (jolt-vector ch6a ch6b) #f))))
(wait-until (lambda () (and (pair? (async-chan-alt-takers ch6a))
                            (pair? (async-chan-alt-takers ch6b))))
            5.0 "handler registered on both ports")
(ok "6. registered on both ports" #t)
(jolt-async-give ch6a 1)
(wait-until (lambda () (not (eq? alts6 #f))) 5.0 "alts completed")
(ok "6. committed on a with the right value" (eqv? (jv-nth alts6 0) 1))
(ok "6. losing port b pruned (list empty)" (null? (async-chan-alt-takers ch6b)))
(ok "6. winning port a pruned too" (null? (async-chan-alt-takers ch6a)))

;; --- 7. mixed take/put specs, :priority, :default -----------------------------
(printf "\n== 7. mixed specs, :priority, :default ==\n")
;; 7a. mixed: a put spec on a (unbuffered — not ready, would block), a take
;; from b which holds a value — the fast pass picks b.
(define w7a (jolt-vector (jolt-vector (jolt-async-chan) (keyword #f "av"))
                         (jolt-async-chan 1)))
(jolt-async-give (jv-nth w7a 1) (keyword #f "bv"))
(define r7a (jolt-async-do-alts w7a #f))
(ok "7a. mixed take/put specs pick the ready port"
    (jolt=2 (jv-nth r7a 0) (keyword #f "bv")))

;; 7b. :priority true — both ready, the FIRST in argument order wins (with
;; priority the scan starts at index 0).
(define w7b (jolt-vector (jolt-async-chan 1) (jolt-async-chan 1)))
(jolt-async-give (jv-nth w7b 0) (keyword #f "pa"))
(jolt-async-give (jv-nth w7b 1) (keyword #f "pb"))
(define r7b (jolt-async-do-alts w7b #t))
(ok "7b. :priority true picks the first port"
    (jolt=2 (jv-nth r7b 0) (keyword #f "pa")))

;; 7c. :default — the immediate-return shortcut lives only in the overlay's
;; do-alts (the host seam has no default argument); the overlay is loaded up
;; front (see top), so alts! here is the real production function. It must
;; return [default :default] immediately and register nothing on either
;; channel (checked via the waiter lists).
(define r7c (ev "(let [a (chan) b (chan)
                     r (alts! [a b] :default :none)]
                  [r a b])"))
(ok "7c. :default returns [val :default]"
    (and (jolt=2 (jv-nth (jv-nth r7c 0) 0) (keyword #f "none"))
         (jolt=2 (jv-nth (jv-nth r7c 0) 1) (keyword #f "default"))))
(ok "7c. :default registered nothing on a" (null? (async-chan-alt-takers (jv-nth r7c 1))))
(ok "7c. :default registered nothing on b" (null? (async-chan-alt-takers (jv-nth r7c 2))))

(printf "\nfibers-go-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "fibers-go-test: PASS — go on fibers, alts! wait set\n") (exit 0))
    (exit 1))
