(ns provfail.install
  "The declared provider of provfail.Clock, which raises before it registers it.")

;; counts the runs of this namespace, in the probe's user/runs when it has one
(when-let [runs (resolve 'user/runs)] (swap! @runs inc))
;; every thread that reaches provfail.Clock has started before this one fails
(when-let [started (resolve 'user/started)]
  (.await ^java.util.concurrent.CountDownLatch @started 10 java.util.concurrent.TimeUnit/SECONDS))
(Thread/sleep 100)

(throw (ex-info "provfail.install broke" {}))
