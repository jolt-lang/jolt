;; test/chez/async-io-thread-test.ss — the io-thread gate (jolt-579).
;; Run: chez --script test/chez/async-io-thread-test.ss (wired into `make fibers`,
;; because io-thread's carrier IS a fiber and this asserts that).
;;
;; core.async names three carriers and jolt now has all three, with no option to
;; set: `thread` is a real OS thread, `go` is the CPS pass on whatever
;; *go-backend* says, and `io-thread` — (thread-call f :io) — is a FIBER. What the
;; JVM buys with a virtual thread (a blocking-shaped body that is cheap to have
;; thousands of, and releases its carrier when it blocks on a channel) is what a
;; fiber is, so :io maps onto fibers and :compute / :mixed stay OS threads.
;;
;; Gate checks (in order):
;;   1. the result channel behaves like thread's: the value lands, a nil body
;;      just closes
;;   2. the carrier each name picks — io-thread body IS on a fiber, thread body is
;;      NOT — and neither honors *go-backend*: an io-thread inside
;;      (binding [*go-backend* :thread] …) is still a fiber, a thread inside
;;      :fiber is still a thread. A dispatcher here would make io-thread mean
;;      "a fiber unless someone above me said otherwise", which is not something
;;      a caller can use.
;;   3. the workload table: :io -> fiber, :mixed / :compute / no-arg -> thread,
;;      anything else throws instead of silently picking one
;;   4. a park SEVERAL FRAMES DEEP in the body, with no binding and no CPS pass:
;;      a fiber parks by capturing its continuation, so a blocking-shaped body
;;      parks from inside a called function — which is the whole point of having
;;      io-thread as well as go
;;   5. the check with teeth: N io-thread bodies parked AT THE SAME TIME on ONE
;;      carrier, all of them resuming. A body that held its carrier instead of
;;      parking would let exactly one of the N run, so this is what separates a
;;      fiber from the OS thread io-thread used to be an alias for. The park
;;      counter (jolt-fiber-chan-parks) is asserted to move by at least N, so
;;      "they all finished" cannot be satisfied by N bodies that never parked.
;;   6. a throwing body closes its channel and publishes to go-monitor, exactly
;;      as a thread-backed one does
;;   7. the spawner's dynamic bindings reach the body, and its transaction does
;;      not (the rule async-go-spawn-thread and jolt-fiber-go-spawn both enforce)
;;
;; ONE CARRIER for the whole file, pinned before the first spawn: it makes check 5
;; mean what it says, and it also means any body that pins its carrier hangs
;; everything after it rather than passing quietly on a machine with cores to
;; spare. Every wait in here is bounded in the JOLT code (alts!! against a
;; timeout, via iot-await) so that failure prints as a FAIL and not as a hung CI
;; job. Nothing in a body blocks the carrier by other means — no Thread/sleep, no
;; raw fd — because on one carrier that would strand the rest of the file, which
;; is the documented reason `thread` exists.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

;; Compile+eval one jolt form in the "user" ns and return its value.
(define (ev s) (jolt-compile-eval s "user"))
(define (jv-nth v i) (pvec-nth-d v i jolt-nil))
(define (kw s) (keyword #f s))

;; Load the real async overlay up front, exactly as the production loader does
;; (host async.ss + stdlib/clojure/core/async.clj together) — the same setup the
;; R4/R5 gates use. io-thread and thread-call are overlay definitions over the
;; host's fiber-spawn / thread-spawn, so a gate for them has to have it loaded.
(define overlay-src
  (call-with-input-file "stdlib/clojure/core/async.clj"
    (lambda (p)
      (let loop ((acc '()))
        (let ((c (read-char p)))
          (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc))))))))
(jolt-load-string overlay-src)
(ev "(require '[clojure.core.async
                :refer [chan <! >! <!! >!! close! timeout alts!! go
                        thread thread-call io-thread go-monitor *go-backend*]])")

;; Every wait a check makes is bounded: :timeout instead of a hung gate.
(ev "(defn iot-await [ch ms]
       (let [[v p] (alts!! [ch (timeout ms)])]
         (if (= p ch) v :timeout)))")

;; One carrier, pinned before the first fiber exists — see the header.
(jolt-fiber-carrier-count-set! 1)
(jolt-fiber-pool-reset!)

(printf "== io-thread: core.async's third carrier is a fiber ==\n")

;; --- 1. the result channel ---------------------------------------------------
(printf "\n== 1. value on the channel; a nil body just closes ==\n")
(ok "1a. the body's value lands on the channel" (eqv? (ev "(<!! (io-thread (+ 1 2)))") 3))
(ok "1b. a nil body closes the channel (nil is not a channel value)"
    (jolt-nil? (ev "(iot-await (io-thread nil) 5000)")))

;; --- 2. which carrier each name picks ----------------------------------------
;; jolt.host/fiber? is the runtime's own answer to "am I on a fiber?" — the same
;; read <! / >! dispatch on — so this is the carrier, not a proxy for it.
(printf "\n== 2. io-thread is a fiber, thread is a thread, *go-backend* moves neither ==\n")
(ok "2a. an io-thread body runs on a fiber" (eq? #t (ev "(<!! (io-thread (jolt.host/fiber?)))")))
(ok "2b. a thread body does not" (eq? #f (ev "(<!! (thread (jolt.host/fiber?)))")))
(ok "2c. io-thread under (binding [*go-backend* :thread] …) is still a fiber"
    (eq? #t (ev "(binding [*go-backend* :thread] (<!! (io-thread (jolt.host/fiber?))))")))
(ok "2d. thread under (binding [*go-backend* :fiber] …) is still a thread"
    (eq? #f (ev "(binding [*go-backend* :fiber] (<!! (thread (jolt.host/fiber?))))")))

;; --- 3. the workload table ---------------------------------------------------
(printf "\n== 3. thread-call's workload selects the carrier ==\n")
(define r3 (ev "[(<!! (thread-call (fn [] (jolt.host/fiber?)) :io))
                 (<!! (thread-call (fn [] (jolt.host/fiber?)) :mixed))
                 (<!! (thread-call (fn [] (jolt.host/fiber?)) :compute))
                 (<!! (thread-call (fn [] (jolt.host/fiber?))))]"))
(ok "3a. :io is a fiber" (eq? #t (jv-nth r3 0)))
(ok "3b. :mixed is a thread" (eq? #f (jv-nth r3 1)))
(ok "3c. :compute is a thread" (eq? #f (jv-nth r3 2)))
(ok "3d. no workload defaults to :mixed, a thread" (eq? #f (jv-nth r3 3)))
;; An unknown workload must not quietly pick one — a caller who misspells :io
;; would otherwise get a thread and never learn that the fiber they asked for
;; is not what ran.
(ok "3e. an unknown workload throws"
    (jolt=2 (kw "threw")
            (ev "(try (thread-call (fn [] 1) :bogus) :no-throw
                      (catch IllegalArgumentException e :threw))")))

;; --- 4. a park several frames deep --------------------------------------------
;; No *go-backend* binding, no CPS pass: the body calls a function that calls a
;; function that takes. The >!! on this thread cannot complete until the fiber has
;; registered as a taker, so the value coming back IS the resume.
(printf "\n== 4. a park inside a called function ==\n")
(ev "(defn iot-inner [c] (<!! c))")
(ev "(defn iot-mid [c] (iot-inner c))")
(define r4 (ev "(let [c (chan)
                      g (io-thread (iot-mid c))]
                  (>!! c 99)
                  (iot-await g 5000))"))
(ok "4a. the park resumed with the value, two frames down" (eqv? r4 99))

;; --- 5. N bodies parked at once on ONE carrier --------------------------------
;; The parks are SIMULTANEOUS by construction: nothing is fed until all N have
;; announced themselves on `ready`, and each body's next act after announcing is a
;; take on an empty channel. On one carrier, body i+1 only gets to announce
;; because body i released the carrier when it parked — an OS-thread-per-body
;; would pass this too, but the io-thread this replaced could not have, and
;; neither could a fiber that blocked its carrier: exactly one body would run.
(printf "\n== 5. 8 bodies parked at the same time, one carrier ==\n")
(define parks-before (jolt-fiber-chan-parks))
(define r5 (ev "(let [n 8
                      ready (chan n)
                      cs (vec (repeatedly n #(chan)))
                      gs (mapv (fn [i] (io-thread (>!! ready i) (* 10 (<!! (nth cs i))))) (range n))
                      arrived (loop [k 0 acc []]
                                (if (= k n)
                                  acc
                                  (let [v (iot-await ready 10000)]
                                    (if (= v :timeout) acc (recur (inc k) (conj acc v))))))]
                  (dotimes [i n] (>!! (nth cs i) i))
                  [(count arrived) (mapv #(iot-await % 10000) gs)])"))
(ok "5a. all 8 bodies ran and reached their park" (= 8 (jv-nth r5 0)))
(ok "5b. all 8 resumed with their own value"
    (jolt=2 (jv-nth r5 1) (jolt-vector 0 10 20 30 40 50 60 70)))
;; Bodies 0..6 must have parked on their take: on one carrier, body i+1 only
;; announced because body i released it. The last body need not park. Once it
;; announces, this thread may start feeding before it reaches its take, and a
;; put already waiting on cs[7] is met without a park. The 8 puts onto a
;; buffered `ready` do not park, and the helper's alts!! runs on this thread.
(ok "5c. the park counter moved by at least 7 (they parked, they did not spin)"
    (>= (- (jolt-fiber-chan-parks) parks-before) 7))

;; --- 6. a throwing body -------------------------------------------------------
;; Same contract as a thread-backed body: the channel closes (so a reader gets
;; nil rather than hanging) and the throwable is published to go-monitor, which is
;; the only thing that separates "threw" from "returned nil". The report on stderr
;; is expected output of this section, not a failure.
(printf "\n== 6. a throwing body closes the channel and reports to go-monitor ==\n")
(printf "   (the 'Exception in go/fiber body' report below is expected)\n")
;; The monitor channel conveys the throwable ONCE and closes, so take it once and
;; ask the value two questions — a second take reads the close, not the failure.
(define r6 (ev "(let [g (io-thread (throw (ex-info \"iot-boom\" {:k :v})))
                      m (go-monitor g)
                      closed (iot-await g 5000)
                      e (iot-await m 5000)]
                  [closed (ex-message e) (:k (ex-data e))])"))
(ok "6a. the channel closed" (jolt-nil? (jv-nth r6 0)))
(ok "6b. go-monitor carries the throwable" (equal? "iot-boom" (jv-nth r6 1)))
(ok "6c. with its ex-data" (jolt=2 (kw "v") (jv-nth r6 2)))

;; --- 7. what the body inherits ------------------------------------------------
(printf "\n== 7. bindings are conveyed, the transaction is not ==\n")
(ev "(def ^:dynamic *iot-v* :root)")
(ok "7a. the spawner's dynamic bindings reach the body"
    (jolt=2 (kw "bound") (ev "(binding [*iot-v* :bound] (<!! (io-thread *iot-v*)))")))
(ok "7b. the body does not join the spawner's transaction"
    (eq? #f (ev "(dosync (<!! (io-thread (clojure.lang.LockingTransaction/isRunning))))")))

(printf "\n~a checks, ~a failed\n" total fails)
(when (> fails 0) (exit 1))
(printf "io-thread gate: PASS\n")
