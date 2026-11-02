;; Semaphore, ReentrantReadWriteLock and the atomic arrays under real contention
;; (java/concurrency-sync.ss, java/atomic-arrays.ss), from threads and from
;; fibers. Every assertion is a final count or an occupancy bound, never a time.
;;   chez --script host/chez/cli.ss test/chez/sync-classes-test.clj
(ns sync-classes-test
  (:require [jolt.fibers :as f])
  (:import [java.util.concurrent Semaphore TimeUnit]
           [java.util.concurrent.locks ReentrantReadWriteLock]
           [java.util.concurrent.atomic AtomicLongArray AtomicIntegerArray AtomicLong]))

(def fails (atom 0))
(def total (atom 0))
(defn ok [label pred]
  (swap! total inc)
  (when-not pred
    (swap! fails inc)
    (println "  FAIL:" label)))

(defn run-threads [n body]
  (let [ts (mapv (fn [i] (Thread. #(body i))) (range n))]
    (run! #(.start ^Thread %) ts)
    (run! #(.join ^Thread %) ts)))
(defn run-fibers [n body]
  (let [fs (mapv (fn [i] (f/spawn #(body i))) (range n))]
    (mapv f/join fs)))

;; occupancy: how many are inside at once, and the most ever seen
(defn occupancy [] (let [in (AtomicLong. 0) hi (AtomicLong. 0)]
                     {:enter (fn [] (let [n (.incrementAndGet in)]
                                      (loop [] (let [h (.get hi)]
                                                 (when (and (> n h) (not (.compareAndSet hi h n))) (recur))))))
                      :leave (fn [] (.decrementAndGet in))
                      :max (fn [] (.get hi))}))

;; --- Semaphore ------------------------------------------------------------------
(doseq [[kind run] [[:threads run-threads] [:fibers run-fibers]]]
  (let [s (Semaphore. 3)
        {:keys [enter leave max]} (occupancy)
        done (AtomicLong. 0)]
    (run 8 (fn [_] (dotimes [_ 300]
                     (.acquire s)
                     (enter) (Thread/yield) (leave)
                     (.incrementAndGet done)
                     (.release s))))
    (ok [kind "semaphore: every acquire ran"] (= 2400 (.get done)))
    (ok [kind "semaphore: never more than 3 inside"] (<= (max) 3))
    (ok [kind "semaphore: permits restored"] (= 3 (.availablePermits s)))
    (ok [kind "semaphore: queue empty"] (zero? (.getQueueLength s)))))

;; multi-permit acquires and fair mode
(doseq [fair [false true]]
  (let [s (Semaphore. 4 fair)
        done (AtomicLong. 0)]
    (run-threads 6 (fn [i] (dotimes [_ 200]
                             (let [n (inc (mod i 3))]
                               (.acquire s n) (.incrementAndGet done) (.release s n)))))
    (ok [fair "semaphore(n): all done, permits back"] (and (= 1200 (.get done)) (= 4 (.availablePermits s))))))

;; a fair semaphore serves fibers in arrival order
(let [s (Semaphore. 0 true)
      order (atom [])
      ;; each one spawned only once the one before is queued, so arrival order is i
      fs (mapv (fn [i]
                 (let [fib (f/spawn #(do (.acquire s) (swap! order conj i)))]
                   (loop [k 0] (when (and (< k 2000) (<= (.getQueueLength s) i)) (Thread/sleep 1) (recur (inc k))))
                   fib))
               (range 5))]
  (ok "fair semaphore: five fibers queued" (= 5 (.getQueueLength s)))
  (dotimes [_ 5] (.release s) (Thread/sleep 5))
  (run! f/join fs)
  (ok "fair semaphore: FIFO order" (= [0 1 2 3 4] @order)))

;; an interrupted waiter leaves the queue
(let [s (Semaphore. 0)
      r (promise)
      t (Thread. #(deliver r (try (.acquire s) :got (catch InterruptedException _ :ie))))]
  (.start t)
  (loop [k 0] (when (and (< k 2000) (not (.hasQueuedThreads s))) (Thread/sleep 1) (recur (inc k))))
  (.interrupt t)
  (.join t)
  (ok "semaphore: interrupt ends the wait" (= :ie @r))
  (ok "semaphore: and the waiter left the queue" (zero? (.getQueueLength s)))
  (.release s)
  (ok "semaphore: the permit is still there" (.tryAcquire s)))

;; a timed tryAcquire that times out leaves the queue too, and one in time succeeds
(let [s (Semaphore. 0)]
  (ok "semaphore: timed out" (false? (.tryAcquire s 20 TimeUnit/MILLISECONDS)))
  (ok "semaphore: no ticket left behind" (zero? (.getQueueLength s)))
  (let [fib (f/spawn #(.tryAcquire s 10 TimeUnit/SECONDS))]
    (Thread/sleep 20)
    (.release s)
    (ok "semaphore: a fiber's timed acquire gets a later release" (true? (f/join fib)))))

;; --- ReentrantReadWriteLock -------------------------------------------------------
(doseq [[kind run] [[:threads run-threads] [:fibers run-fibers]]
        fair [false true]]
  (let [l (ReentrantReadWriteLock. fair)
        r (.readLock l) w (.writeLock l)
        counter (long-array 1)
        writers (occupancy)
        bad (AtomicLong. 0)
        reads (AtomicLong. 0)]
    (run 8 (fn [i]
             (dotimes [n 200]
               (if (zero? (mod (+ i n) 3))
                 (do (.lock w)
                     (try ((:enter writers))
                          (.lock w)            ; reentrant
                          (aset counter 0 (inc (aget counter 0)))
                          (.unlock w)
                          ((:leave writers))
                          (finally (.unlock w))))
                 (do (.lock r)
                     (try (when (.isWriteLocked l) (.incrementAndGet bad))
                          (.incrementAndGet reads)
                          (finally (.unlock r))))))))
    (let [expected (count (for [i (range 8) n (range 200) :when (zero? (mod (+ i n) 3))] 1))]
      (ok [kind fair "rwlock: every write counted"] (= expected (aget counter 0)))
      (ok [kind fair "rwlock: every read ran"] (= (- 1600 expected) (.get reads))))
    (ok [kind fair "rwlock: one writer at a time"] (= 1 ((:max writers))))
    (ok [kind fair "rwlock: no reader saw a writer"] (zero? (.get bad)))
    (ok [kind fair "rwlock: free afterwards"]
        (and (not (.isWriteLocked l)) (zero? (.getReadLockCount l)) (zero? (.getQueueLength l))))))

;; readers share, a writer waits for them, and a new reader waits behind a queued writer
(let [l (ReentrantReadWriteLock.)
      r (.readLock l) w (.writeLock l)
      release (promise)
      in (AtomicLong. 0)
      readers (mapv (fn [_] (f/spawn #(do (.lock r) (.incrementAndGet in) @release (.unlock r))))
                    (range 2))]
  (loop [k 0] (when (and (< k 2000) (< (.get in) 2)) (Thread/sleep 1) (recur (inc k))))
  (ok "rwlock: two readers inside together" (= 2 (.getReadLockCount l)))
  (let [wrote (promise)
        writer (f/spawn #(do (.lock w) (deliver wrote true) (.unlock w)))]
    (loop [k 0] (when (and (< k 2000) (zero? (.getQueueLength l))) (Thread/sleep 1) (recur (inc k))))
    (ok "rwlock: the writer queues behind the readers" (and (= 1 (.getQueueLength l)) (not (realized? wrote))))
    (ok "rwlock: a new reader does not barge past a queued writer" (false? (.tryLock r 10 TimeUnit/MILLISECONDS)))
    (deliver release true)
    (run! f/join readers)
    (f/join writer)
    (ok "rwlock: the writer got in once the readers left" (true? @wrote))))

;; a sibling cannot release a write lock it does not own
(let [l (ReentrantReadWriteLock.)
      w (.writeLock l)
      hold (promise)
      holder (f/spawn #(do (.lock w) @hold (.unlock w) :done))
      _ (loop [k 0] (when (and (< k 2000) (not (.isWriteLocked l))) (Thread/sleep 1) (recur (inc k))))
      other (f/spawn #(try (.unlock w) :released (catch IllegalMonitorStateException _ :imse)))]
  (ok "rwlock: another fiber's unlock is refused" (= :imse (f/join other)))
  (ok "rwlock: still held" (.isWriteLocked l))
  (deliver hold true)
  (ok "rwlock: the owner releases it" (and (= :done (f/join holder)) (not (.isWriteLocked l)))))

;; a fiber whose body throws through its finally still releases
(let [l (ReentrantReadWriteLock.)
      fib (f/spawn #(do (.lock (.writeLock l))
                        (try (throw (ex-info "boom" {}))
                             (finally (.unlock (.writeLock l))))))]
  (ok "rwlock: the throw reaches join" (= "boom" (try (f/join fib) nil (catch Exception e (ex-message e)))))
  (ok "rwlock: and the lock was released" (.tryLock (.writeLock l))))

;; --- atomic arrays -----------------------------------------------------------------
(doseq [[kind run] [[:threads run-threads] [:fibers run-fibers]]]
  (let [a (AtomicLongArray. 4)
        b (AtomicIntegerArray. 4)]
    (run 8 (fn [i] (dotimes [n 500]
                     (.getAndIncrement a (mod n 4))
                     (.updateAndGet b (mod (+ i n) 4) (fn [x] (+ x 2))))))
    (ok [kind "AtomicLongArray: no lost increment"] (= [1000 1000 1000 1000] (mapv #(.get a %) (range 4))))
    (ok [kind "AtomicIntegerArray: no lost update"] (= 8000 (reduce + (map #(.get b %) (range 4)))))))

(println (str "sync-classes-test: " (- @total @fails) "/" @total " passed"))
(if (zero? @fails)
  (println "SYNC-CLASSES-TEST OK")
  (System/exit 1))
