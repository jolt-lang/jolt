(ns provself.install
  "The declared provider of provself.Clock. Its top level blocks on a future
  that reaches provself.Clock before this namespace has registered it.")

;; On the JVM a thread reaching a class another thread is initializing waits for
;; it, and this load is waiting for that thread: the two would deadlock. The
;; future gets an error naming the load instead, at once.
(def from-future
  @(future (try (provself.Clock/now) (catch Throwable e (ex-message e)))))

(__register-class-statics! "provself.Clock" {"now" (fn [] "self-clock")})
