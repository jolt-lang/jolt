;; jolt.fibers — the fiber primitive as a public API, side by side with
;; core.async. core.async runs go bodies ON fibers when asked
;; (*go-backend* :fiber, io-thread); this namespace hands out the fiber
;; itself: spawn a body, join its value, watch its completion. Channels stay
;; in core.async — a fiber taking or putting parks through the same waiter
;; protocol whichever API spawned it, and deref on a promise/future parks
;; rather than pinning the carrier.
;;
;; What the runtime pins (host/chez/fibers.ss) and this API inherits:
;;   - a fiber is a virtual thread: Thread/currentThread inside it is its own
;;     java.lang.Thread (isVirtual true), with its own interrupt flag, so
;;     (.interrupt t) throws that fiber alone out of an interruptible wait.
;;   - a fiber is bound to its carrier (an OS thread of the pool) for life;
;;     a blocking foreign call in a body pins that carrier and strands the
;;     fibers queued behind it. Park-capable waits — channel ops, deref,
;;     Thread/sleep, jolt.socket IO, jolt.process subprocess pipe IO — are the
;;     ones to use inside a body.
;;   - spawn conveys the caller's dynamic bindings; *txn* never conveys (a
;;     child cannot join the parent's STM transaction).
;;   - a body that throws kills its fiber: join rethrows the original error,
;;     monitor! receives it, (state f) answers :dead.
;;   - preemption is always on (a compute-bound body cannot starve its
;;     carrier); the quantum is set-preempt-ticks!, floored, never zero.
;;   - kill! is interrupt! that no catch can swallow: the fiber escapes to the
;;     edge of its outermost unmasked region, or its entry, running only its
;;     finally blocks on the way.
;;   - fibers lists every fiber that hasn't finished, whoever spawned it, so
;;     a tool can show what's running without wrapping spawn (core.async's
;;     io-thread and :fiber go blocks never go through spawn).
;;   - interrupt! makes another fiber raise a throwable wherever it is: at its
;;     next park, yield or preemption, or at once if it is parked. That is an
;;     asynchronous exception, a different thing from Thread.interrupt, which
;;     only sets the fiber's flag and throws InterruptedException out of an
;;     interruptible wait (a park on <!, >! or alts! is not one).
(ns jolt.fibers)

(defn pool?
  "True when x is a pool made by pool."
  [x]
  (jolt.host/fiber-pool? x))

(defn spawn
  "Run (f) on a fiber and return the fiber handle immediately. The body runs
  on the carrier pool; its value or error is read back with join or
  monitor!. The caller's dynamic bindings convey; *txn* does not.

  opts:
    :pool  a pool made by pool to run on instead of the default one (nil is
           the default). Throws if that pool is shut down."
  ([f] (jolt.host/fiber-spawn f))
  ([f {p :pool}]
   (cond
     (nil? p) (jolt.host/fiber-spawn f)
     (not (pool? p)) (throw (ex-info ":pool must be a pool" {:pool p}))
     :else (or (jolt.host/fiber-pool-spawn p f)
               (throw (ex-info "pool is shut down" {:pool (jolt.host/fiber-pool-name p)}))))))

(defn pool
  "Make a pool of carriers apart from the default one, and start them. A
  fiber spawned into it with (spawn f {:pool p}) runs on those carriers only,
  for life, so a fiber that pins its carrier -- a blocking foreign call, a
  long computation -- holds up only the fibers of its own pool. Channels,
  promises, interrupt! and kill! work across pools as within one.

  opts:
    :name  any value, for pool-name (default nil)
    :size  how many carriers (OS threads); defaults to the processor count"
  ([] (pool {}))
  ([{:keys [name size]}]
   (jolt.host/fiber-pool-make name (or size (jolt.host/fiber-carrier-count)))))

(defn pool-name "The :name pool was made with." [p] (jolt.host/fiber-pool-name p))

(defn pool-size "How many carriers pool p has." [p] (jolt.host/fiber-pool-size p))

(defn pool-of
  "The pool fiber fib runs in, or nil for the default pool."
  [fib]
  (jolt.host/fiber-pool-of fib))

(defn shutdown!
  "Stop pool p taking fibers: a spawn into it throws from now on. Its carriers
  run the fibers already in it to the end, parked ones included, and then
  their threads end. Returns at once, without waiting for them."
  [p]
  (jolt.host/fiber-pool-shutdown! p))

(defn fiber?
  "True when x is a fiber handle."
  [x]
  (jolt.host/fiber-instance? x))

(defn current-fiber
  "The running fiber's handle, or nil when called off a fiber."
  []
  (jolt.host/current-fiber))

(defn in-fiber?
  "True when the caller is running on a fiber."
  []
  (jolt.host/fiber?))

(defn yield
  "Reschedule the current fiber behind the others on its carrier and return
  when it runs again. Rarely needed — preemption already time-slices — but a
  cooperative point in a long computation costs less than a preempt. Throws
  when called off a fiber."
  []
  (if (jolt.host/fiber?)
    (jolt.host/fiber-yield)
    (throw (ex-info "yield called off a fiber" {}))))

(defn interrupt!
  "Make fib raise throwable, the way an Erlang process dies of an exit signal
  wherever it is. A parked fiber is woken and raises at once -- a channel wait
  it was parked in is abandoned, so no value is lost to it; a running fiber
  raises at its next park, yield or preemption, so within one quantum. The
  fiber's own try/catch sees the throwable as if the body had thrown it, and a
  fiber that does not catch it dies with it (join rethrows it, monitor! gets it).
  Returns true, or false when fib had already finished."
  [fib throwable]
  (jolt.host/fiber-interrupt! fib throwable))

(defn kill!
  "Make fib die of throwable as an Erlang process dies of kill: delivered as
  interrupt! is, but no try/catch of fib's can catch it. At its safe point fib
  escapes -- its finally blocks run, its catch clauses do not -- to its
  outermost unmasked region, where the throwable is raised in the masked code
  around it (the cleanup of the masked/unmasked shape), or, outside any, to its
  entry, and fib dies with it. A masked region defers it as it defers an
  interrupt. A later interrupt! does not replace a pending kill. Returns true,
  or false when fib had already finished."
  [fib throwable]
  (jolt.host/fiber-kill! fib throwable))

(defn masked
  "Run (f) with interrupts deferred: an interrupt! arriving meanwhile stays
  pending and is raised when the outermost masked region is left. For a
  region that must not be torn, like the cleanup after a body. Off a fiber
  it just runs (f)."
  [f]
  (jolt.host/fiber-masked f))

(defn unmasked
  "Run (f) interruptible again inside a masked region -- the body of the
  (masked (fn [] ... (unmasked body) ... cleanup)) shape, where body may be
  interrupted and cleanup may not. A pending interrupt lands at once."
  [f]
  (jolt.host/fiber-unmasked f))

(defn state
  "The fiber's scheduling state: :ready (queued), :running, :parked (waiting
  to be resumed), :done (completed), or :dead (its body threw)."
  [fib]
  (jolt.host/fiber-state fib))

(defn fibers
  "The fibers that haven't finished yet, as a vector: every one spawned by
  spawn, into a pool, or by core.async (io-thread, a go block on the :fiber
  backend, a flow process), the caller's own included. A snapshot, so a fiber
  in it may finish before you look at it; state, interrupt! and kill! work on
  each the same as on a handle you kept. The list holds the fibers weakly, so
  a parked fiber nothing else can reach (one waiting on a channel nobody holds)
  can drop out of it once the collector runs."
  []
  (vec (jolt.host/fiber-live)))

(defn monitor!
  "Call (cb err) exactly once when fib finishes: err is nil for a clean
  completion, the thrown error for a death. A fiber that already finished
  fires cb inline — registration cannot race the completion. The callback
  runs on the fiber's carrier; keep it short and never block in it."
  [fib cb]
  (jolt.host/fiber-monitor! fib cb))

(defn- completion
  "A promise settled with {:val v} or {:err e} when fib finishes."
  [fib]
  (let [p (promise)]
    (jolt.host/fiber-monitor! fib
      (fn [err]
        (deliver p (if err {:err err} {:val (jolt.host/fiber-result fib)}))))
    p))

(defn join
  "Wait for fib to finish; return its body's value, or rethrow its error.
  On a fiber the wait parks (the carrier keeps running other fibers); on a
  thread it blocks. With a timeout, returns timeout-val if fib is still
  running when timeout-ms elapse. Joining the current fiber throws — it
  could never finish while waiting on itself."
  ([fib]
   (when (identical? fib (jolt.host/current-fiber))
     (throw (ex-info "a fiber cannot join itself" {})))
   (let [r @(completion fib)]
     (if-let [err (:err r)]
       (throw err)
       (:val r))))
  ([fib timeout-ms timeout-val]
   (when (identical? fib (jolt.host/current-fiber))
     (throw (ex-info "a fiber cannot join itself" {})))
   (let [r (deref (completion fib) timeout-ms ::timeout)]
     (cond
       (identical? r ::timeout) timeout-val
       (:err r) (throw (:err r))
       :else (:val r)))))

(defn carrier-count
  "The carrier pool's size: the pinned count if set, else the machine's
  processor count."
  []
  (jolt.host/fiber-carrier-count))

(defn set-carrier-count!
  "Pin the carrier pool to n OS threads (nil restores the machine default).
  Read once, at the pool's first spawn — set it before spawning anything."
  [n]
  (jolt.host/fiber-carrier-count-set! n))

(defn preempt-ticks
  "The preemption quantum, in engine ticks (~0.45 ms per million)."
  []
  (jolt.host/fiber-preempt-ticks))

(defn set-preempt-ticks!
  "Set the preemption quantum in ticks; nil restores the default. Floored —
  there is no way to turn preemption off (a cooperative-only pool is an
  unbounded starvation window); effectively-cooperative wants a very large
  quantum instead."
  [n]
  (jolt.host/fiber-preempt-ticks-set! n))
