(ns provself.install
  "The declared provider of provself.Clock. Its top level starts two futures
  that reach provself.Clock before this namespace has registered it: one it
  never waits for, and one it derefs.")

;; Not waited for: it waits for this load, as a JVM thread waits for a class
;; another thread is initializing, and gets the class once the load is done.
(def started (java.util.concurrent.CountDownLatch. 1))
(def bg (future (.countDown started) (provself.Clock/now)))
(.await started)

;; Waited for: this load waits on the future and the future on this load, the
;; JVM's class-initialization deadlock. The future's wait raises instead.
(def from-future
  @(future (try (provself.Clock/now) (catch Throwable e (ex-message e)))))

(__register-class-statics! "provself.Clock" {"now" (fn [] "self-clock")})
