;; Fibers: monitoring a go block that dies (jolt-atc.5, the observable half of
;; swish's monitors, erlang.ss:434).
;;
;; Before this, a go body that threw and a go body that returned nil were
;; indistinguishable: both reported to async-report-uncaught! at best, closed
;; the result channel, and gave the reader nil. (go-monitor g) yields the
;; throwable if the body died and closes (nil) if it did not.
;;
;; Loads the host runtime plus the async overlay, the same combination
;; fibers-go-test uses, because the surface is a jolt-level var.
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
(ev "(require '[clojure.core.async :refer [chan go <! >!! <!! timeout *go-backend* go-monitor]])")

(printf "== fiber monitors ==\n")

;; --- 1. a throwing body is distinguishable from a nil one -------------------
;; The bug this closes. Both give nil on the go channel; only the monitor
;; separates them.
(define r1 (ev "
(binding [*go-backend* :fiber]
  (let [boom (go (throw (ex-info \"boom\" {:k 1})))
        nily (go nil)
        bm   (<!! (go-monitor boom))
        nm   (<!! (go-monitor nily))]
    [(<!! boom) (<!! nily) (ex-message bm) (ex-data bm) (nil? nm)]))"))
(ok "1. both bodies give nil on the go channel"
    (and (jolt-nil? (jv-nth r1 0)) (jolt-nil? (jv-nth r1 1))))
(ok "1. the monitor reports the throwing body's message"
    (jolt=2 (jv-nth r1 2) "boom"))
(ok "1. ex-data survives, so it is the original throwable"
    (jolt=2 (jv-nth r1 3) (jolt-hash-map (keyword #f "k") 1)))
(ok "1. a body that returned nil monitors as nil" (jolt-truthy? (jv-nth r1 4)))

;; --- 2. a body that succeeds monitors as nil --------------------------------
(define r2 (ev "
(binding [*go-backend* :fiber]
  (let [g (go 42)]
    [(<!! g) (nil? (<!! (go-monitor g)))]))"))
(ok "2. the value still lands on the go channel" (jolt=2 (jv-nth r2 0) 42))
(ok "2. a successful body monitors as nil" (jolt-truthy? (jv-nth r2 1)))

;; --- 3. registering AFTER the fiber already died still resolves -------------
;; The race nobody can win from outside: a caller cannot check the state and
;; register atomically, so a fiber that finished in between must deliver inline
;; rather than leave the caller waiting forever. This is the job swish's
;; demonitor&flush does at the other end.
(define r3 (ev "
(binding [*go-backend* :fiber]
  (let [g (go (throw (ex-info \"late\" {})))]
    (Thread/sleep 300)
    (ex-message (<!! (go-monitor g)))))"))
(ok "3. a monitor registered after death still resolves" (jolt=2 r3 "late"))

;; --- 4. two monitors on one fiber both fire ---------------------------------
(define r4 (ev "
(binding [*go-backend* :fiber]
  (let [g  (go (throw (ex-info \"twice\" {})))
        m1 (go-monitor g)
        m2 (go-monitor g)]
    [(ex-message (<!! m1)) (ex-message (<!! m2))]))"))
(ok "4. every registered monitor fires"
    (and (jolt=2 (jv-nth r4 0) "twice") (jolt=2 (jv-nth r4 1) "twice")))

;; --- 5. a body that parks and THEN throws ------------------------------------
;; Exercises the resume path: the throw happens after a park, so the failure has
;; to survive the continuation round trip.
(define r5 (ev "
(binding [*go-backend* :fiber]
  (let [c (chan)
        g (go (let [v (<! c)] (throw (ex-info \"after park\" {:v v}))))]
    (Thread/sleep 200)
    (>!! c :go)
    (ex-message (<!! (go-monitor g)))))"))
(ok "5. a throw after a park is reported" (jolt=2 r5 "after park"))

;; --- 6. the :thread backend reports the same way ----------------------------
;; WHICH BACKEND RAN THE BODY IS NOT THE CALLER'S BUSINESS. *go-backend* is read
;; at spawn time off a dynamic binding, so a library's go runs on whichever
;; backend its caller established, and a monitor that answered "clean" for a
;; thread-backed body that threw is the exact failure this whole file exists to
;; close — reported as a clean completion, invisible from the jolt side. Same
;; argument sm.ss makes for registering both fiber spawn paths, one level up.
(define r6 (ev "
(binding [*go-backend* :thread]
  (let [g    (go 7)
        boom (go (throw (ex-info \"thread boom\" {:t 1})))
        bm   (<!! (go-monitor boom))]
    [(<!! g) (nil? (<!! (go-monitor g))) (<!! boom) (ex-message bm) (ex-data bm)]))"))
(ok "6. a thread-backend go still returns its value" (jolt=2 (jv-nth r6 0) 7))
(ok "6. a thread-backend body that succeeded monitors as nil"
    (jolt-truthy? (jv-nth r6 1)))
(ok "6. a thread-backend body that threw still gives nil on the go channel"
    (jolt-nil? (jv-nth r6 2)))
(ok "6. and its monitor reports the throwable"
    (jolt=2 (jv-nth r6 3) "thread boom"))
(ok "6. with ex-data intact, so it is the original"
    (jolt=2 (jv-nth r6 4) (jolt-hash-map (keyword #f "t") 1)))

;; Registering after a thread-backed body died resolves inline, as it does on a
;; fiber (section 3) — the caller cannot check and register in one step.
(ok "6. a late monitor on a dead thread-backed body still resolves"
    (jolt=2 (ev "
(binding [*go-backend* :thread]
  (let [g (go (throw (ex-info \"late thread\" {})))]
    (Thread/sleep 300)
    (ex-message (<!! (go-monitor g)))))") "late thread"))

;; A channel that is not a go channel has no completion to report, which is a
;; different thing from a body that completed cleanly, but nil is the only
;; honest answer for it and it must not raise.
(ok "6. monitoring a plain channel degrades to nil"
    (jolt-truthy? (ev "(nil? (<!! (go-monitor (chan))))")))

;; --- 6b. the same body, both backends, same verdict --------------------------
;; The invariant stated directly: run one body on each backend and require the
;; monitors to agree. A regression that reintroduces a backend-specific answer
;; fails here even if it slipped past the cases above.
(define r6b (ev "
(let [run (fn [backend]
            (binding [*go-backend* backend]
              (let [g (go (throw (ex-info \"same\" {})))]
                (ex-message (<!! (go-monitor g))))))]
  [(run :fiber) (run :thread)])"))
(ok "6b. both backends report a dying body identically"
    (and (jolt=2 (jv-nth r6b 0) "same") (jolt=2 (jv-nth r6b 1) "same")))

;; --- 7. a CPS'd body that dies is reported too ------------------------------
;; Section 5 covers a throw after a real park, which reaches jolt-fiber-dead!
;; through the captured continuation. This is the other representation: the pass
;; rewrites the body because it can SEE the take, so the fiber is spawned through
;; __sm-spawn and the throw lands in jolt-sm-drive's handler. Both end at
;; jolt-fiber-dead!, and both have to report. The channel is pre-filled so the
;; take completes inline — which representation the body got is decided at
;; compile time and does not depend on that.
;;
;; Run repeatedly with no sleep before the registration, so a good share of the
;; rounds take jolt-fiber-monitor!'s ALREADY-FINISHED path rather than the
;; registration path. Not a reproducer for the publication race — that window is
;; two instructions wide and this does not hit it; test/chez/fibers-test.ss
;; section 7c is what pins that, by holding the lock rather than racing it.
(define r7 (ev "
(binding [*go-backend* :fiber]
  (loop [i 0 clean 0]
    (if (= i 400)
      clean
      (let [c (chan 1)
            _ (>!! c :v)
            g (go (let [v (<! c)] (throw (ex-info \"race\" {:v v}))))
            m (<!! (go-monitor g))]
        (recur (inc i) (if (nil? m) (inc clean) clean))))))"))
(ok "7. a rewritten body's death is never reported as a clean completion"
    (jolt=2 r7 0))

;; --- 8. a raising monitor is contained -------------------------------------
;; Not reachable from the jolt surface — the only proc go-monitor registers is
;; the one that fills its own channel — so this drives the registry directly.
;; What it protects is the CLOSE: an escape out of the notify loop would skip
;; go-chan-finish!'s jolt-async-close!, and every reader of that go block would
;; wait forever on a channel nothing will ever close again.
(define m8-ch (ac-make 1 'fixed #f))
(define m8-seen (box 'unset))
(go-chan-register! m8-ch)
(go-chan-monitor! m8-ch (lambda (err) (error 'monitor "monitor blew up")))
(go-chan-monitor! m8-ch (lambda (err) (set-box! m8-seen err)))
(define m8-finished
  (guard (e (#t #f)) (go-chan-finish! m8-ch 'the-error) #t))
(ok "8. a raising monitor does not escape the finish" m8-finished)
(ok "8. the other monitor still fired" (eq? 'the-error (unbox m8-seen)))
(ok "8. and the result channel is still closed" (async-chan-closed? m8-ch))

(printf "\nfibers-monitor-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "fibers-monitor-test: PASS — a dying fiber is observable\n") (exit 0))
    (exit 1))
