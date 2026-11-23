(ns provfib.install
  "The declared provider of provfib.Tick. Its top level spawns two fibers that
  reach provfib.Tick before this namespace has registered it: one it never
  joins, and one it joins."
  (:require [jolt.fibers :as f]))

;; Not joined: the fiber parks until this load is done, then gets the class.
(def started (java.util.concurrent.CountDownLatch. 1))
(def bg (f/spawn #(do (.countDown started) (provfib.Tick/now))))
(.await started)

;; Joined: this load waits on the fiber and the fiber on this load. The fiber's
;; wait raises instead of the two waiting forever.
(def from-fiber
  (f/join (f/spawn #(try (provfib.Tick/now) (catch Throwable e (ex-message e))))))

(__register-class-statics! "provfib.Tick" {"now" (fn [] "fib-tick")})
