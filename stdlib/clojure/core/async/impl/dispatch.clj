;;   Copyright (c) Rich Hickey and contributors. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true} clojure.core.async.impl.dispatch
  "The workload -> Executor mapping core.async.flow runs its processes on.

  Upstream this namespace is also core.async's own go-block dispatcher; on jolt
  the channel runtime is Scheme (host/chez/java/async.ss) and dispatches itself,
  so what remains here is the executor registry flow asks for by workload tag.

  The three tags mean what they mean for clojure.core.async/thread-call, and get
  the same carriers:

    :io      a fiber. Blocking-shaped I/O that the runtime can see — channel ops,
             jolt.socket reads, subprocess pipes — PARKS and frees its carrier, so
             a flow can hold far more :io processes than there are threads, and
             each costs a stack rather than an OS thread. A flow process's loop is
             mostly channel ops, which is exactly this shape.
    :mixed   a real thread, one per task. A :mixed step may block somewhere the
             runtime cannot see (a raw fd, a blocking FFI call), and
             that pins a fiber's carrier; a thread is the escape. One per task and
             not a pool because a flow process's task is its whole lifetime —
             pooling buys no reuse, while a bounded pool silently strands every
             process past the worker count.
    :compute a cached pool. Unlike the other two this runs one SHORT task per
             message (flow futurizes a :compute step per call and waits on it), so
             thread reuse is the whole point: a stream of :compute calls is served
             by the handful of workers that keep up with it, and a burst grows the
             pool rather than queueing behind a fixed ceiling."
  (:require [clojure.core.async :as async])
  (:import [java.util.concurrent Executors Executor]))

;; Fire-and-forget onto a fiber. clojure.core.async/fiber-execute is the lean
;; spawn behind this: no result channel, because Executor/execute returns void
;; and there would be nobody to hand one to.
(def ^:private io-executor
  (reify Executor
    (execute [_ r] (async/fiber-execute r))))

(def ^:private thread-counter (atom 0))

(def ^:private mixed-executor
  (reify Executor
    (execute [_ r]
      ;; a daemon, as core.async's own threads are: it must not hold the
      ;; process up once the program is done
      (doto (Thread. r (str "async-mixed-" (swap! thread-counter inc)))
        (.setDaemon true)
        (.start)))))

;; Lazy: a flow that never runs a :compute step never builds the pool. (A cached
;; pool forks no worker until a task arrives, so this no longer costs a thread at
;; load either way.)
(def ^:private compute-executor
  (delay (Executors/newCachedThreadPool
          (reify java.util.concurrent.ThreadFactory
            (newThread [_ r] (doto (Thread. ^Runnable r) (.setDaemon true)))))))

(defn executor-for
  "Given a workload tag, returns the Executor for it. The tags are :io, :compute
  and :mixed; :core-async-dispatch is accepted as an alias for :mixed, as
  upstream. Throws on any other tag."
  ^Executor [workload]
  (case workload
    :io io-executor
    (:mixed :core-async-dispatch) mixed-executor
    :compute @compute-executor
    (throw (IllegalArgumentException.
            (str "Invalid workload " (pr-str workload)
                 " — expected :io, :compute or :mixed")))))

(defn exec
  "Runs Runnable r on the executor for the given workload tag."
  [r workload]
  (.execute (executor-for workload) r)
  nil)
