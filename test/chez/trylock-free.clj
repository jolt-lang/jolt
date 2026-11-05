;; tryLock on a FREE lock must succeed every time (ReentrantLock.tryLock, and
;; the monitor's monitor-try-enter!). vector-cas! is a weak compare-and-swap on
;; arm64, and the try path had no slow path to absorb a spurious failure, so on
;; Apple silicon about 1 in 10^7 uncontended tryLocks answered false (it failed
;; fibers-lock-test's 9f on an arm64 Android runner). On x86 the CAS is strong
;; and this passes either way.
;;
;;   jolt run test/chez/trylock-free.clj   (from smoke.sh)
(let [lk (java.util.concurrent.locks.ReentrantLock.)
      ;; about a second of tries, checking the clock every 2^20: long enough to
      ;; catch the refusal on 0.8.23 most runs, stopping at the first one
      deadline (+ (System/nanoTime) 1000000000)
      [n refused?] (loop [i 0]
                     (cond (not (.tryLock lk)) [i true]
                           (and (zero? (bit-and i 0xFFFFF)) (> (System/nanoTime) deadline)) [i false]
                           :else (do (.unlock lk) (recur (inc i)))))]
  (if refused?
    (do (println "TRYLOCK-FREE FAIL: a free lock refused after" n "tries") (System/exit 1))
    (println "TRYLOCK-FREE OK")))
