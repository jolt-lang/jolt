;; tryLock on a FREE lock must succeed every time (ReentrantLock.tryLock, and
;; the monitor's monitor-try-enter!). vector-cas! is a weak compare-and-swap on
;; arm64, and the try path had no slow path to absorb a spurious failure, so on
;; Apple silicon about 3 in 5*10^7 uncontended tryLocks answered false (it failed
;; fibers-lock-test's 9f on an arm64 Android runner). On x86 the CAS is strong
;; and this passes either way.
;;
;;   jolt run test/chez/trylock-free.clj   (from smoke.sh)
(let [lk (java.util.concurrent.locks.ReentrantLock.)
      fails (loop [i 0 fails 0]
              (if (< i 50000000)
                (if (.tryLock lk)
                  (do (.unlock lk) (recur (inc i) fails))
                  (recur (inc i) (inc fails)))
                fails))]
  (if (zero? fails)
    (println "TRYLOCK-FREE OK")
    (do (println "TRYLOCK-FREE FAIL:" fails "refusals of a free lock") (System/exit 1))))
