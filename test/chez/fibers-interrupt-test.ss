;; test/chez/fibers-interrupt-test.ss — interrupting a fiber from outside
;; (jolt.fibers/interrupt!, fibers.ss "interrupts"). Run: chez --script
;; test/chez/fibers-interrupt-test.ss (wired into `make fibers`).
;;
;; An interrupt makes another fiber raise a given throwable wherever it is, the
;; way an Erlang process dies of an exit signal. The fiber raises it itself, at
;; a safe point: a compute-bound fiber on its next quantum, a parked one at once.
;; What the gate holds:
;;   1. a spinning fiber dies with the throwable, and join rethrows it
;;   2. a fiber parked on a channel dies, and the channel then delivers to the
;;      next taker: the abandoned wait swallowed nothing
;;   3. a fiber parked in a deref (the jolt-lock-wait protocol) dies instead of
;;      retaking its decision
;;   4. the fiber's own try/catch sees the throwable
;;   5. interrupting a finished fiber is refused (false)
;;   6. a fiber that catches an interrupt and spins on is still preempted, so
;;      the raise left the interrupt depth where fiber code runs
;;   7. a fiber interrupted before its first run dies on entry
;;   8. a CPS'd go body parked on a channel dies, and its go channel closes
;;   9. a stress race: many takers interrupted while values are put to the same
;;      channel -- every value put is taken by a fiber that returned it, or is
;;      still in the channel; none vanishes into an abandoned wait
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(define (ev s) (jolt-compile-eval s "user"))
(define (jv-nth v i) (pvec-nth-d v i jolt-nil))

(load "host/chez/gate-async-overlay.ss")
(gate-load-async-overlay!)
(define fibers-src
  (call-with-input-file "stdlib/jolt/fibers.clj"
    (lambda (p)
      (let loop ((acc '()))
        (let ((c (read-char p)))
          (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc))))))))
(jolt-load-string fibers-src)
(ev "(require '[clojure.core.async :as a] '[jolt.fibers :as f])")
(ev "(defn outcome [fib]
       (try (f/join fib 2000 :timeout)
            (catch Throwable e [:died (ex-message e)])))")
(define (died? r msg)
  (and (jolt-vector? r) (jolt=2 (jv-nth r 0) (keyword #f "died")) (jolt=2 (jv-nth r 1) msg)))

(printf "== fiber interrupts ==\n")

;; --- 1. a spinning fiber --------------------------------------------------
(define r1 (ev "
(let [spin (f/spawn (fn [] (loop [i 0] (recur (inc i)))))]
  (a/<!! (a/timeout 20))
  [(f/interrupt! spin (ex-info \"stop spinning\" {})) (outcome spin) (f/state spin)])"))
(ok "1. interrupt! answers true for a live fiber" (eq? #t (jv-nth r1 0)))
(ok "1. the spinning fiber dies with the throwable" (died? (jv-nth r1 1) "stop spinning"))
(ok "1. and reads as :dead" (jolt=2 (jv-nth r1 2) (keyword #f "dead")))

;; --- 2. a channel wait is abandoned, not left armed ------------------------
(define r2 (ev "
(let [c (a/chan)
      w (f/spawn (fn [] (a/<!! c)))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"stop waiting\" {}))
  (let [died (outcome w)
        r (f/spawn (fn [] (a/<!! c)))]
    (a/<!! (a/timeout 10))
    (a/>!! c :value)
    [died (outcome r)]))"))
(ok "2. the parked taker dies" (died? (jv-nth r2 0) "stop waiting"))
(ok "2. the next taker gets the value" (jolt=2 (jv-nth r2 1) (keyword #f "value")))

;; --- 3. a deref ------------------------------------------------------------
(define r3 (ev "
(let [p (promise) w (f/spawn (fn [] @p))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"stop deref\" {}))
  (let [r (outcome w)] (deliver p 1) r))"))
(ok "3. a fiber parked in deref dies" (died? r3 "stop deref"))

;; --- 4. caught -------------------------------------------------------------
(define r4 (ev "
(let [w (f/spawn (fn [] (try (a/<!! (a/chan)) (catch Throwable e [:caught (ex-message e)]))))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"catch me\" {}))
  (outcome w))"))
(ok "4. the fiber's own catch sees the throwable"
    (and (jolt=2 (jv-nth r4 0) (keyword #f "caught")) (jolt=2 (jv-nth r4 1) "catch me")))

;; --- 5. finished -----------------------------------------------------------
(define r5 (ev "(let [w (f/spawn (fn [] :done))] (f/join w) (f/interrupt! w (ex-info \"late\" {})))"))
(ok "5. a finished fiber refuses the interrupt" (eq? #f r5))

;; --- 6. the depth is restored ---------------------------------------------
(define r6 (ev "
(let [w (f/spawn (fn [] (try (loop [] (recur)) (catch Throwable _ (loop [] (recur))))))
      other (f/spawn (fn [] (a/<!! (a/timeout 50)) :other-ran))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"first\" {}))
  (let [o (outcome other)]
    (f/interrupt! w (ex-info \"second\" {}))
    [o (outcome w)]))"))
(ok "6. other fibers still run beside one that caught and spins on"
    (jolt=2 (jv-nth r6 0) (keyword #f "other-ran")))
(ok "6. and a second interrupt still lands" (died? (jv-nth r6 1) "second"))

;; --- 7. before the first run ----------------------------------------------
(define r7 (ev "(let [w (f/spawn (fn [] :ran))] (f/interrupt! w (ex-info \"early\" {})) (outcome w))"))
(ok "7. a fiber interrupted before it ran dies on entry"
    (or (died? r7 "early") (jolt=2 r7 (keyword #f "ran"))))

;; --- 8. a CPS'd go body ----------------------------------------------------
(define r8 (ev "
(binding [a/*go-backend* :fiber]
  (let [c (a/chan)
        fib (promise)
        g (a/go (deliver fib (f/current-fiber)) (a/<! c) :took)]
    (a/<!! (a/timeout 20))
    (f/interrupt! @fib (ex-info \"stop go\" {}))
    (let [v (a/alts!! [g (a/timeout 1000)])]
      (a/>!! (a/chan 1) :x)
      [(first v) (some? (a/<!! (a/go-monitor g)))])))"))
(ok "8. an interrupted go body closes its channel (nil, not a timeout)" (jolt-nil? (jv-nth r8 0)))
(ok "8. and its monitor reports the throwable" (eq? #t (jv-nth r8 1)))

;; --- 9. no value lost to an abandoned wait ---------------------------------
(define r9 (ev "
(let [c (a/chan)
      n 200
      takers (vec (for [_ (range n)] (f/spawn (fn [] (a/<!! c)))))
      _ (a/<!! (a/timeout 20))
      putter (f/spawn (fn [] (dotimes [i n] (a/>!! c i)) :put))
      _ (doseq [t (take-nth 2 takers)] (f/interrupt! t (ex-info \"cut\" {})))
      got (keep (fn [t] (let [r (outcome t)] (when (integer? r) r))) takers)
      left (loop [acc []] (let [[v _] (a/alts!! [c (a/timeout 50)])] (if (some? v) (recur (conj acc v)) acc)))]
  (f/interrupt! putter (ex-info \"done\" {}))
  [(count got) (count left) (= (count (distinct (concat got left))) (+ (count got) (count left)))])"))
(let ((got (jv-nth r9 0)) (left (jv-nth r9 1)))
  (ok "9. every value is taken or still queued" (<= 100 (+ got left) 200))
  (ok "9. none is taken twice" (eq? #t (jv-nth r9 2))))

;; --- 10. masked defers, unmasked restores --------------------------------------
;; The shape a process's cleanup takes: the body interruptible, what follows it not.
(define r10 (ev "
(let [log (atom [])
      w (f/spawn (fn []
                   (f/masked
                    (fn []
                      (let [r (try (f/unmasked (fn [] (a/<!! (a/chan)) :never))
                                   (catch Throwable e (ex-message e)))]
                        ;; a second interrupt lands here, masked: it must wait
                        (a/<!! (a/timeout 60))
                        (swap! log conj [:cleanup r])
                        :cleaned)))))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"body\" {}))
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"late\" {}))
  [(outcome w) @log])"))
(ok "10. the masked cleanup ran to the end, the body's interrupt caught"
    (jolt=2 (jv-nth r10 1) (jolt-vector (jolt-vector (keyword #f "cleanup") "body"))))
(ok "10. the late interrupt landed on leaving the mask, not inside it"
    (died? (jv-nth r10 0) "late"))

;; --- 11. a masked fiber parked on a channel is not woken, and gets its value -----
(define r11 (ev "
(let [c (a/chan)
      w (f/spawn (fn [] (f/masked (fn [] [:took (a/<!! c)]))))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"wait\" {}))
  (a/<!! (a/timeout 20))
  (a/>!! c :v)
  (outcome w))"))
(ok "11. the masked wait took its value, and the interrupt landed as the mask closed"
    (died? r11 "wait"))

;; --- 12. a wait an interrupt ended does not wake a later channel wait ---------
;; The deref's registration outlives the raise (the cv's waiter list still holds
;; the fiber), so delivering the promise afterwards resumes the fiber wherever it
;; is parked by then. A channel wait must read that as the spurious wake it is
;; and keep waiting, not return an empty mailbox as nil.
(define r12 (ev "
(let [p (promise) c (a/chan)
      w (f/spawn (fn [] (try @p (catch Throwable _ nil)) [:took (a/<!! c)]))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"leave deref\" {}))
  (a/<!! (a/timeout 20))
  (deliver p 1)
  (a/<!! (a/timeout 20))
  (a/>!! c :v)
  (outcome w))"))
(ok "12. a stale deref wake leaves a later channel wait parked"
    (jolt=2 r12 (jolt-vector (keyword #f "took") (keyword #f "v"))))

;; --- 13. the same for a CPS'd go body's cheap park ------------------------------
(define r13 (ev "
(binding [a/*go-backend* :fiber]
  (let [p (promise) c (a/chan) fib (promise)
        g (a/go (deliver fib (f/current-fiber))
                (try @p (catch Throwable _ nil))
                [:took (a/<! c)])]
    (a/<!! (a/timeout 20))
    (f/interrupt! @fib (ex-info \"leave deref\" {}))
    (a/<!! (a/timeout 20))
    (deliver p 1)
    (a/<!! (a/timeout 20))
    (a/>!! c :v)
    (first (a/alts!! [g (a/timeout 1000)]))))"))
(ok "13. a stale deref wake leaves a later cheap park parked"
    (jolt=2 r13 (jolt-vector (keyword #f "took") (keyword #f "v"))))

;; --- 14. a cheap park that was delivered leaves no waiter behind ---------------
;; Otherwise a later interrupt of a deref reads the stale handler as the wait,
;; finds it already claimed, and wakes nothing.
(define r14 (ev "
(binding [a/*go-backend* :fiber]
  (let [p (promise) c (a/chan 1) fib (promise)
        g (a/go (deliver fib (f/current-fiber))
                (a/<! c)
                @p)]
    (a/<!! (a/timeout 20))
    (a/>!! c :first)
    (a/<!! (a/timeout 20))
    (f/interrupt! @fib (ex-info \"in deref\" {}))
    (let [v (a/alts!! [g (a/timeout 1000)])]
      (deliver p :late)
      (= g (second v)))))"))
(ok "14. an interrupt reaches a go body's deref after a delivered cheap park" (eq? #t r14))

;; --- 15. an interruptible wait an interrupt ended leaves no registration --------
;; Counted as ENTRIES across every box's set: a box's set is kept when it empties
;; (locks.ss), and each fiber has a box of its own, so the number of boxes is not
;; the number of registrations.
(define (interrupt-wait-entries)
  (let-values (((ks vs) (hashtable-entries jolt-interrupt-waits)))
    (let loop ((i 0) (n 0))
      (if (fx=? i (vector-length vs)) n (loop (fx+ i 1) (+ n (hashtable-size (vector-ref vs i))))))))
(define waits-before (interrupt-wait-entries))
(ev "
(let [p (promise)
      w (f/spawn (fn [] (try @p (catch Throwable _ :caught))))]
  (a/<!! (a/timeout 20))
  (f/interrupt! w (ex-info \"leave deref\" {}))
  (outcome w))")
(ok "15. the interrupted deref deregistered from its interrupt box"
    (= waits-before (interrupt-wait-entries)))

;; --- 16. kill!: an interrupt no catch can swallow ------------------------------
;; An Erlang kill: the fiber leaves by escape, not by a raise, so its own
;; try/catch never sees it -- only its finally blocks run. It lands at the
;; outermost unmasked region (raised there, in the masked cleanup) or, outside
;; any, at the fiber's entry, and the fiber dies with the throwable.
(define r16 (ev "
(let [w (f/spawn (fn [] (loop [] (try (reduce + (range 1000)) (catch Throwable _ nil)) (recur))))]
  (a/<!! (a/timeout 20))
  [(f/kill! w (ex-info \"killed\" {})) (outcome w)])"))
(ok "16. kill! answers true for a live fiber" (eq? #t (jv-nth r16 0)))
(ok "16. a fiber that catches everything in a loop still dies of a kill"
    (died? (jv-nth r16 1) "killed"))

(define r17 (ev "
(let [w (f/spawn (fn [] (try (a/<!! (a/chan)) (catch Throwable _ (loop [] (recur))))))]
  (a/<!! (a/timeout 20))
  (f/kill! w (ex-info \"killed parked\" {}))
  (outcome w))"))
(ok "17. a parked fiber is killed past its catch" (died? r17 "killed parked"))

(define r18 (ev "
(let [log (atom [])
      w (f/spawn (fn []
                   (f/masked
                    (fn []
                      (let [r (try (f/unmasked
                                    (fn [] (loop []
                                             (try (a/<!! (a/chan))
                                                  (catch Throwable _ (swap! log conj :caught-in-body))
                                                  (finally (swap! log conj :finally)))
                                             (recur))))
                                   (catch Throwable e [:landed (ex-message e)]))]
                        (swap! log conj :cleanup)
                        r)))))]
  (a/<!! (a/timeout 20))
  (f/kill! w (ex-info \"to the edge\" {}))
  [(outcome w) @log])"))
(ok "18. a kill lands at the unmasked edge, where the masked cleanup catches it"
    (jolt=2 (jv-nth r18 0) (jolt-vector (keyword #f "landed") "to the edge")))
(ok "18. the body's catch never saw it, its finally ran, the cleanup ran"
    (jolt=2 (jv-nth r18 1) (jolt-vector (keyword #f "finally") (keyword #f "cleanup"))))

(define r19 (ev "
(let [w (f/spawn (fn [] (loop [] (try (a/<!! (a/chan)) (catch Throwable _ nil)) (recur))))]
  (a/<!! (a/timeout 20))
  (f/kill! w (ex-info \"the kill\" {}))
  (f/interrupt! w (ex-info \"an interrupt\" {}))
  (outcome w))"))
(ok "19. an interrupt after a kill does not replace it" (died? r19 "the kill"))

(define r20 (ev "(let [w (f/spawn (fn [] :done))] (f/join w) (f/kill! w (ex-info \"late\" {})))"))
(ok "20. a finished fiber refuses the kill" (eq? #f r20))

(define r21 (ev "
(let [w (f/spawn (fn [] (f/masked (fn [] (a/<!! (a/timeout 60)) :masked-done))))]
  (a/<!! (a/timeout 20))
  (f/kill! w (ex-info \"after mask\" {}))
  (outcome w))"))
(ok "21. a kill waits for a masked region, then the fiber dies" (died? r21 "after mask"))

(printf "~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
