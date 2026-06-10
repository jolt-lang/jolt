# Specification: clojure.core futures on Janet OS threads (ev/thread).
#
# A `future` runs its body on a *real* OS thread (ev/thread), so it can use a
# second core for CPU-bound work — unlike the cooperatively-scheduled go blocks.
# Because Janet threads have separate heaps, the body and its captured state are
# MARSHALLED (copied) to the worker thread and the result is marshalled back: a
# future sees a snapshot of captured state and communicates only via its return
# value (mutations to captured atoms do NOT propagate back). `deref`/`@` blocks
# (parks) until the worker finishes; the result is cached for later derefs.
(use ../support/harness)

(defspec "clojure.core / futures — deref"
  ["future + deref"            "3"       "(deref (future (+ 1 2)))"]
  ["@ reader macro derefs"     "42"      "@(future (* 6 7))"]
  ["future returns collection" "[2 3 4]" "(deref (future (mapv inc [1 2 3])))"]
  ["future returns a map"      "{:a 1}"  "(deref (future {:a 1}))"]
  ["deref is cached/idempotent" "[2 2]"  "(let [f (future (+ 1 1))] [(deref f) (deref f)])"]
  ["timed deref of ready future" "42"    "(let [f (future 42)] (deref f) (deref f 1000 :nope))"]
  ["body error re-raised on deref" :throws "(deref (future (throw \"boom\")))"]
  # Thread/sleep parks the WORKER's own event loop (each future thread has one),
  # so a sleeping future doesn't block the parent — and timed deref can fire.
  ["timed deref times out"     ":timed-out" "(deref (future (do (Thread/sleep 300) :late)) 10 :timed-out)"]
  ["Thread/sleep in body"      ":slept"     "(deref (future (do (Thread/sleep 5) :slept)))"]
  ["timed-out future still completes" ":late"
   "(let [f (future (do (Thread/sleep 30) :late))] (deref f 5 :early) (deref f))"])

(defspec "clojure.core / futures — predicates"
  ["future? true"             "true"  "(future? (future 1))"]
  ["future? false"            "false" "(future? 42)"]
  ["future-done? after deref" "true"  "(let [f (future 1)] (deref f) (future-done? f))"]
  ["realized? after deref"    "true"  "(let [f (future 1)] (deref f) (realized? f))"]
  # Cancel marks the future done (the worker can't be interrupted, but the
  # future object reflects the cancellation: deref raises, predicates flip).
  ["cancel an in-flight future returns true" "true"
   "(let [f (future 1)] (future-cancel f))"]
  ["future-cancelled? after cancel" "true"
   "(let [f (future 1)] (future-cancel f) (future-cancelled? f))"]
  ["future-done? after cancel" "true"
   "(let [f (future 1)] (future-cancel f) (future-done? f))"]
  ["cancel an already-completed future returns false" "false"
   "(let [f (future 1)] (deref f) (future-cancel f))"]
  ["future-cancelled? fresh is false" "false"
   "(future-cancelled? (future 1))"])

(defspec "clojure.core / futures — snapshot (copy) semantics"
  # The worker thread swaps its *copy* of the atom; the parent's atom is untouched.
  ["captured atom is snapshotted, not shared"
   "0" "(let [a (atom 0)] (deref (future (swap! a inc))) @a)"]
  # The future's own return value still reflects the swap on its copy.
  ["future sees its own mutation"
   "1" "(let [a (atom 0)] (deref (future (swap! a inc))))"])
