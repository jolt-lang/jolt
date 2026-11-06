;; test/chez/fibers-process-io-test.ss — R8 gate, extended to jolt.process
;; subprocess pipe I/O. Run:
;; chez --script test/chez/fibers-process-io-test.ss (wired into `make fibers`
;; immediately after fibers-io-test.ss).
;;
;; fibers-io-test.ss covers R8's socket half; this file covers the other half of
;; the same trick applied to a subprocess's stdin/stdout pipes:
;; proc-fd-input-port / proc-fd-output-port (host/chez/java/process.ss) set
;; O_NONBLOCK unconditionally on the pipe fd the parent retains, and on EAGAIN
;; ask jolt.io-poller to wait for readiness — parking the fiber when there is a
;; current fiber, blocking this thread when there is not. Same user-facing
;; jolt.process code either way. The retry loop also has a THIRD branch neither
;; socket.clj nor fibers-io-test.ss needs: off a fiber, when jolt.io-poller was
;; never required at all (jolt.process is usable standalone, and jolt.socket is
;; the only ns in the tree that requires the poller), EAGAIN clears O_NONBLOCK
;; on the fd and falls through to a real blocking read/write — behaviorally
;; identical to the pre-R8-extension code, once triggered. ON a fiber that same
;; state autoloads the poller instead, since falling back there is precisely the
;; carrier-pinning this file exists to rule out.
;;
;; Gate checks, in order:
;;   1. a fiber reading a subprocess pipe with nothing written yet PARKS, and a
;;      sibling fiber on the SAME carrier makes progress while it waits — the
;;      whole round in one check, same shape as fibers-io-test.ss's check 1.
;;   2. the same code path on a plain OS thread (no current fiber) still blocks
;;      and still works — a lower-bound elapsed-time check.
;;   3. jolt.host/gc-full! succeeds while the poller is blocked servicing a pipe
;;      registration (the __collect_safe property fibers-io-test.ss's check 3
;;      asserts for sockets — this is the same poller, so this is really a
;;      belt-and-suspenders repeat of it via a different registration path,
;;      worth asserting explicitly since jolt.process is a distinct caller of
;;      proc-poller-wait-ready from jolt.socket).
;;   4. N fibers (N picked below, see the note at its definition), each reading
;;      its OWN subprocess's pipe, all on ONE carrier: all N park at once, then
;;      all N complete with their own payload — real parking, not serialized
;;      blocking.
;;   5. jolt.process's existing higher-level API (process / sh / shell, :in /
;;      :out piping, @proc deref) is unaffected OFF a fiber — confirms the R8
;;      extension does not leak into or change the non-fiber path every
;;      existing jolt.process caller (process-test.clj, babashka.process
;;      itself) depends on.
;;   6. a FRESH Chez process that requires jolt.process but never
;;      jolt.io-poller (so proc-poller-wait-ready's var-deref finds it genuinely
;;      unbound, not just un-invoked) gets BOTH halves of the choice right: off a
;;      fiber it blocks correctly and leaves the poller unloaded — not a spurious
;;      short-read/EOF on the first EAGAIN, not a busy spin, and not a private
;;      kqueue per wait either; on a fiber it autoloads the poller and genuinely
;;      parks, with a sibling on the same carrier proving it. That fiber half is
;;      the one that reproduces jolt-641 in full if the autoload is removed, and
;;      it is the shape of every subprocess-driving fiber program that never
;;      opens a socket. No analog in fibers-io-test.ss: sockets always require
;;      jolt.io-poller by construction, so this state is only reachable through
;;      jolt.process. Runs the probe as a genuinely separate `chez --script`
;;      subprocess (see check 6 below) — jolt-var auto-vivifies an unbound
;;      placeholder for any never-required ns, so within ONE process there is
;;      no way back to "genuinely never required" once checks 1-5 have required
;;      jolt.io-poller for real parking.
;;   7. closing a pipe-read port while a fiber is parked on it must not strand
;;      the fiber — the CRITICAL close-races-a-parked-read regression: the fd
;;      close proc now tells jolt.io-poller to forget the fd (mirroring
;;      jolt.socket's socket-close!), which wakes any parked waiter instead of
;;      leaving it waiting on an event that will never come (the kernel
;;      auto-drops a closed fd from the kqueue/epoll set).
;;   8. a fiber write that fills a subprocess pipe parks, and a sibling fiber
;;      on the SAME carrier makes real progress while the write is blocked —
;;      the write-side twin of check 1, closing the permanent-gate coverage
;;      gap the write path (EAGAIN branch + fcntl fallback, symmetric with the
;;      read side) had until now.
;;   9. closing a pipe-WRITE port while a fiber is parked on it makes the woken
;;      fiber take the port's closed? short-circuit, rather than re-running
;;      proc-c-write against a fd the close proc has already closed (and that
;;      the kernel may already have reassigned to an unrelated
;;      pipe/socket/open) with a buf it has already free()d. Check 7's twin one
;;      level deeper: check 7 asserts the woken fiber is not STRANDED, this one
;;      asserts WHICH path it takes once woken.
;;
;; Watchdog: the whole workload (all 9 checks) runs on a forked thread; this
;; script's main thread does nothing but watch a hard-coded deadline. A wedge
;; anywhere in the workload — including a raw blocking spawn/read call that
;; isn't wrapped in wait-until's own per-check bound — fails the gate loudly
;; with the phase it was in and jolt.io-poller's debug-state, instead of
;; hanging `make fibers` silently. Same shape as test/chez/poller-registration.clj's
;; watchdog, adapted to this file's plain Chez-thread (not Jolt Thread/System)
;; primitives, since this is a raw gate-boot .ss script, not a .clj file run
;; through jolt.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
;; The real loader, in cli.ss's order: loader.ss seeds its loaded-ns from the
;; vars that exist AT LOAD TIME, so jolt.ffi's host vars (java/ffi.ss) must come
;; AFTER it, or a (require '[jolt.ffi]) skips stdlib/jolt/ffi.clj and the
;; defcfn macro never exists. gate-boot's image (target/dev/gate.so, when fresh)
;; already bakes both — the delete below makes the require pull the Clojure
;; side in that case too, so the gate behaves identically with and without the
;; image.
(load "host/chez/loader.ss")
(hashtable-delete! loaded-ns "jolt.ffi")
(set-source-roots!* '("jolt-core" "stdlib" "vendor/fs/src" "vendor/process/src" "vendor/cli/src" "vendor/grenadine/src"
                      "vendor/grenadine-generated"))
(load "host/chez/java/ffi.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(define (mono-nanos)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000000000 (time-second t)) (time-nanosecond t))))
(define (now-secs) (/ (exact->inexact (mono-nanos)) 1000000000.0))

;; Bounded wait: a bug must FAIL the gate, never hang it.
(define (wait-until pred secs what)
  (let ((deadline (+ (now-secs) secs)))
    (let loop ()
      (if (pred)
          #t
          (if (> (now-secs) deadline)
              (begin (set! fails (+ fails 1))
                     (printf "  FAIL: ~a (timed out)\n" what) #f)
              (begin (sleep (make-time 'time-duration 1000000 0)) (loop)))))))

;; Compile+eval one jolt form in the "user" ns and return its value.
(define (ev s) (jolt-compile-eval s "user"))
(define (all-done? fs)
  (or (null? fs) (and (eq? (jolt-fiber-state (car fs)) 'done) (all-done? (cdr fs)))))
(define (all-parked? fs)
  (or (null? fs) (and (eq? (jolt-fiber-state (car fs)) 'parked) (all-parked? (cdr fs)))))
(define (spawn-n n mk)
  (let loop ((i 0) (acc '()))
    (if (fx<? i n)
        (loop (fx+ i 1) (cons (mk i) acc))
        (reverse acc))))

;; Render a raised condition (or any thrown value) as a string for a diagnostic
;; print — mirrors test/chez/op-arity-test.ss's render-condition.
(define (render-condition e)
  (if (condition? e)
      (let ((p (open-output-string))) (display-condition e p) (get-output-string p))
      (format "~s" e)))

;; A plain, dependency-free substring search — used on check 6's captured
;; subprocess stdout and, since check 9, on captured condition messages too.
(define (string-contains-substr? s sub)
  (let ((ls (string-length s)) (lsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i lsub) ls) #f)
            ((string=? (substring s i (+ i lsub)) sub) #t)
            (else (loop (+ i 1)))))))

;; --- watchdog scaffolding -------------------------------------------------------
;; progress: the phase the workload thread is currently in, for the watchdog's
;; diagnostic print on a trip. outcome: 'running until the workload sets it to
;; 'ok / 'failed, or (threw . <string>) if the workload itself raised.
(define progress (box "starting"))
(define (at! phase) (set-box! progress phase))
(define outcome (box 'running))

;; Measured real runtime (5 repeated runs, this machine): ~8.5-12.5s total —
;; roughly 3.2s for this file's own gate-boot+loader+ffi load plus requiring
;; jolt.process/jolt.io-poller, ~1.9s for checks 1-5 combined (real subprocess
;; spawns, N=6 of them in check 4), and ~5.5s for check 6's nested Chez
;; subprocess (which repeats the same gate-boot+loader+ffi+require cost, plus
;; the 0.3s delayed read of its thread half and the 2s one of its fiber half —
;; long enough that the fiber is provably still parked when the sibling's own
;; progress is read). hang-secs is ~10x the slowest observed run —
;; generous for a slower CI machine, still bounded: anything that reaches it
;; is wedged, not slow.
(define hang-secs 120.0)

;; Every check-local value below is pre-declared here and filled with set!,
;; never nested define: R6RS bodies require every internal define to precede
;; every expression, and these checks interleave "compute a value" with
;; "assert on it" (an ok call between two dependent state reads) by design —
;; see check 1's "sibling made progress while parked" assertion, which must
;; read fa's state at that exact point in the sequence, not after fa-done is
;; also known. A single flat let keeps that interleaving exactly as written.
(define (workload)
 (let ((read-thunk-1 #f) (fa #f) (fb #f) (fa-parked #f) (fb-done #f) (fa-done #f)
       (t2 #f) (r2 #f) (el2 #f)
       (read-thunk-3 #f) (fc #f) (fc-parked #f) (w3 #f) (gc-ok #f) (fc-done #f)
       (r4-n #f) (r4-worker #f) (f4s #f) (all-parked4 #f) (all-done4 #f)
       (r5-sh-out #f) (r5-in #f) (r5-pipe #f) (r5-deref-exit #f)
       (repo-dir #f) (no-poller-script #f) (no-poller-path #f) (chez-bin #f)
       (np-out #f) (np-err #f) (np-exit #f)
       (read-thunk-7 #f) (f7 #f) (f7-parked #f) (f7-unparked #f) (f7-result #f)
       (r8-payload-len #f) (write-thunk-8 #f) (f8w #f) (f8sib #f)
       (f8w-parked #f) (f8sib-done #f) (r8-total #f) (f8w-done #f)
       (r9-payload-len #f) (write-thunk-9 #f) (f9 #f) (f9-parked #f)
       (f9-done #f) (f9-result #f))
  (at! "requiring jolt.process + jolt.io-poller")
  (ev "(require 'jolt.process)")
  ;; Checks 1-5 need REAL parking, which needs jolt.io-poller genuinely
  ;; required (poller-wait-ready's var-deref returns unbound, and the
  ;; no-poller fallback fires, until this happens) — mirrors
  ;; fibers-io-test.ss's own explicit require.
  (ev "(require 'jolt.io-poller)")

  ;; One carrier, started as a real carrier thread — the R8 wake path (poller ->
  ;; sa-fiber-resume -> carrier run queue) needs it live. Never reset mid-file.
  (jolt-fiber-carrier-count-set! 1)
  (jolt-fiber-pool-reset!)
  (jolt-fiber-ensure-carrier!)

  (printf "== fibers-process-io: transparent jolt.process pipe parking on a fiber ==\n")

  ;; --- 1. a fiber read on a subprocess pipe parks; a sibling progresses ------
  (at! "1. park+sibling-progress")
  (printf "\n== 1. a fiber read on a subprocess pipe parks; a sibling on the same carrier runs ==\n")
  (ev "(def p1 (jolt.process/process [\"sh\" \"-c\" \"sleep 0.2; printf x\"]))")
  (ev "(def p1-out (:out p1))")
  (set! read-thunk-1
    (ev "(fn [] (let [b (byte-array 64) n (.read p1-out b 0 64)] (String. b 0 n \"UTF-8\")))"))
  (set! fa (sa-fiber-spawn read-thunk-1))
  (set! fb (sa-fiber-spawn (lambda () 4242)))
  (ok "1. the two fibers share one carrier" (eq? (jolt-fiber-carrier fa) (jolt-fiber-carrier fb)))
  (set! fa-parked
    (wait-until (lambda () (eq? (jolt-fiber-state fa) 'parked)) 15.0 "1. the pipe-read fiber parked"))
  (ok "1. the pipe read parked the fiber (no data written yet)" fa-parked)
  (set! fb-done
    (wait-until (lambda () (eq? (jolt-fiber-state fb) 'done)) 15.0 "1. the sibling completed"))
  (ok "1. the sibling fiber on the SAME carrier made progress while the read was parked"
      (and fb-done (eq? (jolt-fiber-state fa) 'parked)))
  (ok "1. the sibling ran to completion" (eqv? (jolt-fiber-result fb) 4242))
  (set! fa-done
    (wait-until (lambda () (eq? (jolt-fiber-state fa) 'done)) 15.0 "1. the read resumed"))
  (ok "1. the parked read resumed with the child's data"
      (and fa-done (string=? (jolt-fiber-result fa) "x")))
  (ev "(deref p1 5000 :reap-timeout)")

  ;; --- 2. the same code path on a plain OS thread blocks and works ----------
  (at! "2. thread-mode")
  (printf "\n== 2. the thread path still blocks and still works ==\n")
  ;; the clock starts before the child exists, so its output cannot be there
  ;; until 300 ms after t2 (see fibers-io-test.ss section 2)
  (set! t2 (mono-nanos))
  (ev "(def p2 (jolt.process/process [\"sh\" \"-c\" \"sleep 0.3; printf threaded\"]))")
  (set! r2
    (ev "(let [b (byte-array 64) n (.read (:out p2) b 0 64)] (String. b 0 n \"UTF-8\"))"))
  (set! el2 (/ (exact->inexact (- (mono-nanos) t2)) 1000000.0))
  (ok "2. the thread-mode read returned the delayed data" (string=? r2 "threaded"))
  ;; a read that returned without waiting for the data (a spin, or a wrong
  ;; EAGAIN-as-EOF) clocks in under the child's delay
  (ok "2. it actually waited for the child (>= the 300 ms delay)"
      (>= el2 300.0))
  (printf "   thread-path read took ~,1f ms for a 300 ms delayed write\n" el2)
  (ev "(deref p2 5000 :reap-timeout)")

  ;; --- 3. jolt.host/gc-full! succeeds while the poller is blocked -----------
  (at! "3. gc-full! mid poller-blocked wait")
  (printf "\n== 3. jolt.host/gc-full! succeeds while the poller is blocked servicing a pipe registration ==\n")
  ;; The child writes when we tell it to, after the collection, not after a
  ;; fixed delay: on a slow runner the waits and the collection below outlasted
  ;; a 300 ms sleep, the data arrived first, and the fiber had already resumed
  ;; when the row asked whether it was still parked.
  (ev "(def p3 (jolt.process/process [\"sh\" \"-c\" \"read go; printf z\"]))")
  (ev "(def p3-out (:out p3))")
  (set! read-thunk-3
    (ev "(fn [] (let [b (byte-array 64) n (.read p3-out b 0 64)] (String. b 0 n \"UTF-8\")))"))
  (set! fc (sa-fiber-spawn read-thunk-3))
  (set! fc-parked
    (wait-until (lambda () (eq? (jolt-fiber-state fc) 'parked)) 15.0 "3. the pipe-read fiber parked"))
  (ok "3. the pipe-read fiber parked" fc-parked)
  ;; The registration does not wake the poller, so its round counter says nothing
  ;; about THIS park. What the check needs is that the poller is settled in its
  ;; blocking wait when gc-full! runs: it started long before (check 1), nothing has
  ;; fired for this pipe yet, and so the counter holds still across a settle.
  (set! w3
    (let ((before (ev "@jolt.io-poller/waits")))
      (sleep (make-time 'time-duration 50000000 0))
      (and (> before 0) (= before (ev "@jolt.io-poller/waits")))))
  (ok "3. the poller sits in its blocking wait with this check's registration in the kernel" w3)
  (sleep (make-time 'time-duration 50000000 0))  ; let it settle into the wait
  (set! gc-ok
    (guard (e (#t (printf "  FAIL detail: gc-full! raised ~a\n" (render-condition e)) #f))
      (ev "(jolt.host/gc-full!)")
      #t))
  (ok "3. jolt.host/gc-full! succeeded while the poller was blocked (a pipe registration mid-flight)"
      gc-ok)
  (ok "3. the parked fiber survived the collection" (eq? (jolt-fiber-state fc) 'parked))
  (ev "(doto (:in p3) (.write (.getBytes \"go\\n\")) (.flush) (.close))")
  (set! fc-done
    (wait-until (lambda () (eq? (jolt-fiber-state fc) 'done)) 15.0 "3. the read resumed after gc-full!"))
  (ok "3. the fiber completed after gc-full! with the correct payload"
      (and fc-done (string=? (jolt-fiber-result fc) "z")))
  (ev "(deref p3 5000 :reap-timeout)")

  ;; --- 4. N fibers, one carrier: all park at once, all round-trip -----------
  (at! "4. N processes one carrier")
  (printf "\n== 4. N fibers, one carrier, each reading its own subprocess pipe, all round-trip ==\n")
  ;; N in the 4-8 range: 6 was tuned against this gate's own measured runtime
  ;; (profiled per-check: N=6 here added well under a second on top of the
  ;; fixed gate-boot/require/nested-chez costs that dominate this file's total
  ;; runtime) — real subprocess spawn (posix_spawn + two pipes each) is
  ;; materially heavier than fibers-io-test.ss's check 4 (8 in-process
  ;; socketpairs), so this check does not default to 8.
  (set! r4-n 6)
  (ev (string-append
       "(def p4-procs (mapv (fn [i] (jolt.process/process [\"sh\" \"-c\" (str \"sleep 0.3; printf m\" i)])) (range "
       (number->string r4-n) ")))"))
  (ev "(def p4-outs (mapv :out p4-procs))")
  (set! r4-worker
    (ev "(fn [i] (let [s (nth p4-outs i) b (byte-array 64) n (.read s b 0 64)] (String. b 0 n \"UTF-8\")))"))
  (set! f4s (spawn-n r4-n (lambda (i) (sa-fiber-spawn (lambda () (r4-worker i))))))
  ;; serialized blocking could never get here: with the carrier pinned by one
  ;; blocking read, at most ONE fiber is 'parked at a time and the rest sit
  ;; 'ready behind it. All N 'parked at one instant proves real parking.
  (set! all-parked4
    (wait-until (lambda () (all-parked? f4s)) 20.0 "4. all N fibers parked on their pipe reads"))
  (ok "4. all N fibers parked simultaneously on one carrier (real parking, not serialized blocking)"
      all-parked4)
  (set! all-done4
    (wait-until (lambda () (all-done? f4s)) 20.0 "4. all N round trips completed"))
  (ok "4. all N round trips completed" all-done4)
  ;; A fiber that never resumed has #f for a result, and string=? on it RAISES —
  ;; guard on all-done4 like the other result reads, matching
  ;; fibers-io-test.ss's own check 4 hardening.
  (ok "4. every fiber got its own payload"
      (and all-done4
           (let loop ((i 0))
             (or (fx=? i r4-n)
                 (let ((r (jolt-fiber-result (list-ref f4s i))))
                   (and (string? r)
                        (string=? r (string-append "m" (number->string i)))
                        (loop (fx+ i 1))))))))
  (ev "(doseq [p p4-procs] (deref p 5000 :reap-timeout))")

  ;; --- 5. existing jolt.process API is unaffected off-fiber ------------------
  (at! "5. existing API off-fiber")
  (printf "\n== 5. existing jolt.process API (process/sh/shell, :in/:out piping, deref) is unaffected off-fiber ==\n")
  ;; Every call in this section runs directly on this (workload) thread — a
  ;; plain OS thread with no current fiber, never inside sa-fiber-spawn — the
  ;; same off-fiber path process-test.clj exercises.
  (set! r5-sh-out (ev "(:out (jolt.process/sh [\"echo\" \"hi-check5\"]))"))
  (ok "5. sh captures stdout, off-fiber" (string=? r5-sh-out "hi-check5\n"))
  (set! r5-in (ev "(:out (jolt.process/sh [\"cat\"] {:in \"check5-in\"}))"))
  (ok "5. :in piping still works, off-fiber" (string=? r5-in "check5-in"))
  (set! r5-pipe (ev "(-> (jolt.process/process [\"echo\" \"check5-pipe\"]) (jolt.process/process [\"cat\"]) :out slurp)"))
  (ok "5. :out piping between two processes still works, off-fiber" (string=? r5-pipe "check5-pipe\n"))
  (set! r5-deref-exit (ev "(:exit @(jolt.process/process [\"true\"]))"))
  (ok "5. @proc deref still returns the exit code, off-fiber" (eqv? r5-deref-exit 0))
  (ok "5. shell (stdio-inherit, throw-on-failure) still runs without raising, off-fiber"
      (guard (e (#t (printf "  FAIL detail: shell raised ~a\n" (render-condition e)) #f))
        (ev "(jolt.process/shell \"true\")")
        #t))

  ;; --- 6. jolt.io-poller reachability, with nobody having required it --------
  (at! "6. poller autoload + off-fiber fallback (nested chez subprocess)")
  (printf "\n== 6. jolt.process standalone: a fiber autoloads jolt.io-poller, a thread keeps the blocking fallback ==\n")
  ;; This process already required jolt.io-poller for checks 1-5 (jolt-var
  ;; auto-vivifies an unbound placeholder for a never-required ns, so once a
  ;; require has happened for real there is no way back within this process).
  ;; So this check runs the probe in a genuinely fresh `chez --script`
  ;; subprocess that requires jolt.process and nothing else, and reports its
  ;; own verdict on stdout.
  ;;
  ;; The probe covers both halves of proc-poller-wait-ready's choice, in the
  ;; one order that can observe them: OFF a fiber first, where the poller must
  ;; stay unloaded and the read must fall back to a real blocking read; then ON
  ;; a fiber, where the poller must autoload and the read must genuinely park.
  ;; Reversed, the fiber half would leave the poller loaded and the thread half
  ;; could no longer tell "did not autoload" from "someone else had".
  ;;
  ;; jolt.socket is the only ns in the tree that requires jolt.io-poller, so
  ;; this is the shape every subprocess-driving fiber program that never opens
  ;; a socket is in — a build-tool wrapper, a shell pipeline. Without the
  ;; autoload the fiber half here pins its carrier and jolt-641 reproduces in
  ;; full on top of everything else in this file.
  (set! repo-dir (current-directory))
  (set! no-poller-script (string-append
    "(import (chezscheme))\n"
    "(load \"host/chez/gate-boot.ss\")\n"
    "(load \"host/chez/loader.ss\")\n"
    "(hashtable-delete! loaded-ns \"jolt.ffi\")\n"
    "(set-source-roots!* '(\"jolt-core\" \"stdlib\" \"vendor/fs/src\" \"vendor/process/src\" \"vendor/cli/src\" \"vendor/grenadine/src\"\n"
    "                      \"vendor/grenadine-generated\"))\n"
    "(load \"host/chez/java/ffi.ss\")\n"
    "(define (ev s) (jolt-compile-eval s \"user\"))\n"
    "(define (mono-nanos) (let ((t (current-time 'time-monotonic))) (+ (* 1000000000 (time-second t)) (time-nanosecond t))))\n"
    "(define (now-secs) (/ (exact->inexact (mono-nanos)) 1000000000.0))\n"
    "(define (wait-until pred secs)\n"
    "  (let ((deadline (+ (now-secs) secs)))\n"
    "    (let loop ()\n"
    "      (cond ((pred) #t)\n"
    "            ((> (now-secs) deadline) #f)\n"
    "            (else (sleep (make-time 'time-duration 1000000 0)) (loop))))))\n"
    "(define (poller-loaded?) (not (jolt-var-unbound? (var-deref \"jolt.io-poller\" \"wait-ready\"))))\n"
    ;; deliberately never requires jolt.io-poller
    "(ev \"(require 'jolt.process)\")\n"
    "(define pre (poller-loaded?))\n"
    ;; -- off a fiber: the blocking fallback, and no autoload --
    "(define t1 (mono-nanos))\n"
    "(ev \"(def np1 (jolt.process/process [\\\"sh\\\" \\\"-c\\\" \\\"sleep 0.3; printf npo\\\"]))\")\n"
    "(define r1 (ev \"(let [b (byte-array 64) n (.read (:out np1) b 0 64)] (String. b 0 n \\\"UTF-8\\\"))\"))\n"
    "(define el1 (/ (exact->inexact (- (mono-nanos) t1)) 1000000.0))\n"
    "(define mid (poller-loaded?))\n"
    ;; -- on a fiber: the autoload, and real parking with a sibling progressing --
    ;; TWO carriers and TWO readers, not one of each, because the concurrent
    ;; autoload is the case with a wrong answer available: both readers reach
    ;; their first EAGAIN at once, and a "have we tried yet" latch in
    ;; proc-poller-autoload! would let the second one past it while the first is
    ;; still loading, so the second finds the var unbound, takes the blocking
    ;; fallback, and clears O_NONBLOCK on its own fd for good. That shows up
    ;; here as ONE of the two readers stuck 'running instead of 'parked, which
    ;; is exactly what parked2 catches. Two carriers is also the smallest pool
    ;; that can host both readers at once, which is what makes them race at all.
    "(jolt-fiber-carrier-count-set! 2)\n"
    "(jolt-fiber-pool-reset!)\n"
    "(jolt-fiber-ensure-carrier!)\n"
    "(ev \"(def np2s (mapv (fn [i] (jolt.process/process [\\\"sh\\\" \\\"-c\\\" \\\"cat >/dev/null; printf npf\\\"])) (range 2)))\")\n"
    "(ev \"(def np2-outs (mapv :out np2s))\")\n"
    "(define read-thunk (ev \"(fn [i] (let [s (nth np2-outs i) b (byte-array 64) n (.read s b 0 64)] (String. b 0 n \\\"UTF-8\\\")))\"))\n"
    "(define fs (list (sa-fiber-spawn (lambda () (read-thunk 0)))\n"
    "                 (sa-fiber-spawn (lambda () (read-thunk 1)))))\n"
    "(define (all-state? st) (lambda () (andmap (lambda (f) (eq? (jolt-fiber-state f) st)) fs)))\n"
    "(define parked2 (wait-until (all-state? 'parked) 8.0))\n"
    ;; A third body on a pool both of whose carriers are hosting a pipe read. It
    ;; samples the readers from ITS OWN carrier, at the instant it runs, and carries
    ;; the verdict out in its result. Sampling them from THIS thread after waiting
    ;; for sib raced: the readers used to be gated on `sleep 2`, and everything
    ;; before the park — require, spawn, first read, and the jolt.io-poller autoload
    ;; this check exists to force — came out of that same 2s. Measured at 1041-1153ms
    ;; on an idle 10-core dev machine, so over half the budget was already spent
    ;; before the window opened; a CI runner lands near 2000ms and the readers wake
    ;; within a millisecond of sib running. Observed as one failed and one passed run
    ;; on the same commit. Now nothing is on a clock: the readers park until we send
    ;; EOF below, so if parking did NOT free a carrier sib can never run at all and
    ;; the wait times out, which is the real property this check is for.
    ;; sib records the readers' states rather than requiring both 'parked at
    ;; the instant it runs: parked2 has already seen both parked, neither can
    ;; finish before the EOF below, and a reader passing through 'ready or
    ;; 'running on the other carrier as it re-parks is not a failure. The old
    ;; check failed once on a bionic runner with both readers parked before and
    ;; done after, its report silent on sib itself (it prints sib now). A reader that never parked
    ;; (the blocking fallback) fails parked2, and one stuck on a carrier leaves
    ;; the other free, so what sib proves is that it ran before any reader was
    ;; done.
    "(define sib (sa-fiber-spawn (lambda () (map jolt-fiber-state fs))))\n"
    "(define sib-ran (and (wait-until (lambda () (eq? (jolt-fiber-state sib) 'done)) 15.0)\n"
    "                     (let ((r (jolt-fiber-result sib))) (and (list? r) (not (memq 'done r))))))\n"
    "(define post (poller-loaded?))\n"
    ;; only now let the readers finish: closing our end of each stdin is the EOF
    ;; their `cat` is waiting for.
    "(ev \"(doseq [p np2s] (.close (:in p)))\")\n"
    "(define done2 (wait-until (all-state? 'done) 15.0))\n"
    "(define r2 (map jolt-fiber-result fs))\n"
    "(if (and (not pre) (string? r1) (string=? r1 \"npo\") (>= el1 300.0) (not mid)\n"
    "         parked2 sib-ran post done2 (equal? r2 (list \"npf\" \"npf\")))\n"
    "    (begin (printf \"NO-POLLER-PROBE OK thread-read=~,1fms\\n\" el1) (exit 0))\n"
    "    (begin (printf (string-append \"NO-POLLER-PROBE FAIL pre=~a r1=~a el1=~,1fms mid=~a\"\n"
    "                                  \" parked2=~a sib-ran=~a sib=~a/~s post=~a done2=~a r2=~s\"\n"
    "                                  \" states=~a\\n\")\n"
    "                   pre r1 el1 mid parked2 sib-ran (jolt-fiber-state sib) (jolt-fiber-result sib)\n"
    "                   post done2 r2 (map jolt-fiber-state fs))\n"
    "           (exit 1)))\n"))
  (set! no-poller-path
    (string-append (host-temp-dir) "/jolt-fibers-process-io-no-poller-probe-" (number->string (get-process-id)) ".ss"))
  (let ((p (open-output-file no-poller-path 'replace)))
    (display no-poller-script p)
    (close-output-port p))
  (set! chez-bin (or (getenv "JOLT_CHEZ") "chez"))
  (ev (string-append
       "(def np-result (jolt.process/sh [\"" chez-bin "\" \"--script\" \"" no-poller-path "\"]"
       " {:out :string :err :string :dir \"" repo-dir "\"}))"))
  (set! np-out (ev "(:out np-result)"))
  (set! np-err (ev "(:err np-result)"))
  (set! np-exit (ev "(:exit np-result)"))
  (ok "6. the standalone-jolt.process subprocess exited 0"
      (eqv? np-exit 0))
  (ok "6. off a fiber it fell back to a real blocking read and left jolt.io-poller unloaded; on a fiber it autoloaded the poller and genuinely parked"
      (and (string? np-out) (string-contains-substr? np-out "NO-POLLER-PROBE OK")))
  (unless (and (eqv? np-exit 0) (string? np-out) (string-contains-substr? np-out "NO-POLLER-PROBE OK"))
    (printf "  standalone subprocess stdout: ~a\n" np-out)
    (printf "  standalone subprocess stderr: ~a\n" np-err))
  (guard (e (#t #f)) (delete-file no-poller-path))

  ;; --- 7. closing a port while parked on its read must not strand the fiber -
  (at! "7. close-while-parked (no strand)")
  (printf "\n== 7. closing a pipe-read port while a fiber is parked on it must not strand it ==\n")
  ;; sleep long enough that nothing is ever written before we close — the
  ;; fiber must park on a genuinely empty pipe, not race a real payload
  (ev "(def p7 (jolt.process/process [\"sh\" \"-c\" \"sleep 5\"]))")
  (ev "(def p7-out (:out p7))")
  (set! read-thunk-7
    (ev "(fn [] (let [b (byte-array 64) n (.read p7-out b 0 64)] (String. b 0 (max 0 n) \"UTF-8\")))"))
  (set! f7 (sa-fiber-spawn read-thunk-7))
  (set! f7-parked
    (wait-until (lambda () (eq? (jolt-fiber-state f7) 'parked)) 15.0 "7. the pipe-read fiber parked"))
  (ok "7. the read parked (nothing written yet)" f7-parked)
  ;; close from the workload thread while the fiber is still parked — this is
  ;; the exact race Fix 1 closes: the close proc now forgets the fd with
  ;; jolt.io-poller, waking the parked waiter instead of leaving it waiting on
  ;; an event the kernel will never deliver for an already-closed fd.
  (ev "(.close p7-out)")
  (set! f7-unparked
    (wait-until (lambda () (not (eq? (jolt-fiber-state f7) 'parked))) 5.0
                "7. the parked read left 'parked after the port closed (did not strand)"))
  (ok "7. closing the port unparked the fiber within the bound" f7-unparked)
  (set! f7-result
    (wait-until (lambda () (eq? (jolt-fiber-state f7) 'done)) 5.0 "7. the unparked read finished"))
  (ok "7. the fiber finished cleanly (closed-fd read reads as EOF, not a hang or a crash)"
      (and f7-result (string? (jolt-fiber-result f7))))
  ;; Pin the read side's closed-case convention: EOF, i.e. an empty payload.
  ;; Note this cannot tell the two ways of GETTING there apart, and no
  ;; value-based assertion can: the read side's closed? short-circuit and its
  ;; "any other errno -> 0" branch both answer 0/EOF by construction, so they
  ;; are indistinguishable to a caller. Check 9 asserts the shared mechanism on
  ;; the write side, where the two paths raise distinguishable conditions.
  (ok "7. the closed-port read reported EOF (empty payload), not partial or garbage data"
      (and f7-result (equal? "" (jolt-fiber-result f7))))
  (ev "(jolt.process/destroy p7)")
  (ev "(deref p7 5000 :reap-timeout)")

  ;; --- 8. a fiber write that fills a subprocess pipe parks; a sibling runs --
  (at! "8. write-park+sibling-progress")
  (printf "\n== 8. a fiber write that fills a subprocess pipe parks; a sibling on the same carrier runs ==\n")
  ;; 2MB -- comfortably over any platform's actual pipe-buffer size (commonly
  ;; 16-64KB), not a hardcoded assumption of what that size is
  (set! r8-payload-len (* 2 1024 1024))
  (ev "(def p8 (jolt.process/process [\"cat\"]))")
  (ev "(def p8-in (:in p8))")
  (ev "(def p8-out (:out p8))")
  (ev (string-append "(def p8-payload (byte-array " (number->string r8-payload-len) "))"))
  (set! write-thunk-8
    (ev "(fn [] (.write p8-in p8-payload 0 (alength p8-payload)) (.flush p8-in) (.close p8-in) (alength p8-payload))"))
  (set! f8w (sa-fiber-spawn write-thunk-8))
  (set! f8sib (sa-fiber-spawn (lambda () 8181)))
  (ok "8. the two fibers share one carrier" (eq? (jolt-fiber-carrier f8w) (jolt-fiber-carrier f8sib)))
  (set! f8w-parked
    (wait-until (lambda () (eq? (jolt-fiber-state f8w) 'parked)) 15.0
                "8. the pipe-write fiber parked (the pipe filled)"))
  (ok "8. the oversized write parked the fiber once the pipe filled" f8w-parked)
  (set! f8sib-done
    (wait-until (lambda () (eq? (jolt-fiber-state f8sib) 'done)) 15.0 "8. the sibling completed"))
  ;; NOT check 1's parked-at-this-instant read: a pipe WRITER oscillates
  ;; park/unpark while cat drains its stdin+stdout kernel buffers (~128-192KB),
  ;; and the trivial sibling finishes inside that window — sampling 'parked here
  ;; raced under full-suite load (one observed CI failure). A read on an empty
  ;; pipe parks once and stays parked, which is why check 1's read is stable.
  ;; The load-independent property is that the sibling finished while the 2MB
  ;; write could not yet have: nothing drains p8-out until the loop below, so
  ;; the writer is done only after ~10x more bytes than the kernel can buffer.
  (ok "8. the sibling fiber on the SAME carrier made real progress while the write was parked"
      (and f8sib-done (not (eq? (jolt-fiber-state f8w) 'done))))
  (ok "8. the sibling ran to completion" (eqv? (jolt-fiber-result f8sib) 8181))
  ;; drain cat's echoed stdout on this (plain OS thread) workload thread to
  ;; release the backpressure chain: cat is blocked writing its own stdout
  ;; before it reads more stdin, so nothing unparks the write until this reads
  (set! r8-total
    (ev (string-append
         "(let [b (byte-array 65536)]"
         " (loop [total 0]"
         "   (let [n (.read p8-out b 0 65536)]"
         "     (if (< n 0) total (recur (+ total n))))))")))
  (set! f8w-done
    (wait-until (lambda () (eq? (jolt-fiber-state f8w) 'done)) 15.0 "8. the parked write resumed"))
  (ok "8. the parked write resumed and reported the full payload length written"
      (and f8w-done (eqv? (jolt-fiber-result f8w) r8-payload-len)))
  (ok "8. the drain side received the full payload byte-for-byte" (eqv? r8-total r8-payload-len))
  (ev "(deref p8 5000 :reap-timeout)")

  ;; --- 9. a close under a PARKED write short-circuits on the closed? flag ---
  (at! "9. close-while-parked-write (closed short-circuit)")
  (printf "\n== 9. closing a pipe-write port while a fiber is parked on it takes the closed short-circuit ==\n")
  ;; Check 8's fill-the-pipe setup, minus the drain: cat's own stdout is never
  ;; read here, so cat stops consuming stdin and the write stays parked until
  ;; the close below wakes it. What this adds over check 7 is WHICH path the
  ;; woken fiber then takes, and the two candidates are told apart by the
  ;; condition each raises:
  ;;   "write to closed pipe"  -- the closed? test at the top of the retry loop
  ;;       fired and proc-c-write was never called. The fix's own path.
  ;;   "write to child failed" -- the retry re-ran proc-c-write against a fd the
  ;;       close proc had already closed (and that the kernel is free to hand
  ;;       straight to an unrelated pipe/socket/open, since POSIX reuses the
  ;;       lowest free number) passing a buf the close proc had already
  ;;       free()d. That is the use-after-free the flag exists to prevent, and
  ;;       it is exactly what this check observes when the flag is removed.
  ;; So this is a direct assertion on the mechanism, not on "nothing crashed" —
  ;; and it needs no timing race to be deterministic, because the fiber cannot
  ;; run at all until the close proc has already set the flag and woken it.
  (set! r9-payload-len (* 2 1024 1024))
  (ev "(def p9 (jolt.process/process [\"cat\"]))")
  (ev "(def p9-in (:in p9))")
  (ev (string-append "(def p9-payload (byte-array " (number->string r9-payload-len) "))"))
  (set! write-thunk-9
    (ev "(fn [] (.write p9-in p9-payload 0 (alength p9-payload)) (.flush p9-in) :never-raised)"))
  ;; guard inside the fiber thunk so the raise becomes this fiber's RESULT: an
  ;; unguarded raise ends the fiber through jolt-fiber-dead! instead, and the
  ;; message is the whole point of the check.
  (set! f9 (sa-fiber-spawn (lambda () (guard (e (#t (render-condition e))) (write-thunk-9)))))
  (set! f9-parked
    (wait-until (lambda () (eq? (jolt-fiber-state f9) 'parked)) 15.0
                "9. the pipe-write fiber parked (the pipe filled)"))
  (ok "9. the oversized write parked the fiber once the pipe filled" f9-parked)
  (ev "(.close p9-in)")
  (set! f9-done
    (wait-until (lambda () (eq? (jolt-fiber-state f9) 'done)) 10.0
                "9. the parked write left 'parked after the port closed"))
  (ok "9. closing the port unparked the parked write within the bound" f9-done)
  (set! f9-result (and f9-done (jolt-fiber-result f9)))
  (ok "9. the woken retry took the CLOSED short-circuit (proc-c-write never re-ran on the closed fd)"
      (and (string? f9-result) (string-contains-substr? f9-result "write to closed pipe")))
  (ok "9. and did NOT reach the real-write error path (no syscall on the freed buf / closed fd)"
      (and (string? f9-result) (not (string-contains-substr? f9-result "write to child failed"))))
  (unless (and (string? f9-result) (string-contains-substr? f9-result "write to closed pipe"))
    (printf "  check 9 fiber result: ~s\n" f9-result))
  (ev "(jolt.process/destroy p9)")
  (ev "(deref p9 5000 :reap-timeout)")

  (printf "\nfibers-process-io: ~a checks, ~a failures\n" total fails)
  (set-box! outcome (if (zero? fails) 'ok 'failed))))

(fork-thread
  (lambda ()
    (guard (e (#t (set-box! outcome (cons 'threw (render-condition e)))))
      (workload))))

;; The main thread from here on is only the deadline. Every exit goes through it,
;; matching test/chez/poller-registration.clj's watchdog shape: the workload runs
;; entirely on a spawned thread, and the one thread that cannot itself be what
;; wedges is the one doing nothing but watching the clock.
(let ((deadline (+ (now-secs) hang-secs)))
  (let loop ()
    (sleep (make-time 'time-duration 25000000 0))
    (let ((o (unbox outcome)))
      (cond
        ((eq? o 'ok) (exit 0))
        ((eq? o 'failed) (exit 1))
        ((and (pair? o) (eq? (car o) 'threw))
         (printf "FIBERS-PROCESS-IO THREW: ~a\n" (cdr o))
         (exit 1))
        ((> (now-secs) deadline)
         (printf "FIBERS-PROCESS-IO HUNG after ~a s, last phase: ~a\n" hang-secs (unbox progress))
         (guard (e (#t (printf "  (debug-state unavailable: ~a)\n" (render-condition e))))
           (printf "  jolt.io-poller debug-state: ~a\n" (ev "(pr-str (jolt.io-poller/debug-state))")))
         (exit 1))
        (else (loop))))))
