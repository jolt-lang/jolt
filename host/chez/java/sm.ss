;; host/chez/java/sm.ss — the cheap park. Loaded by rt.ss AFTER fibers-async.ss.
;;
;; A fiber parks by capturing a continuation, and Chez represents that as a stack
;; segment which stays live for as long as the process is parked (~3.5 KB, whatever
;; the depth). A park the CPS pass in clojure.core.async could rewrite does not
;; need one: the rest of the body is already a closure, so the op stores the
;; closure and switches to the scheduler with no capture at all.
;;
;; The two mechanisms coexist inside one fiber and are chosen per park site, by
;; whether a continuation was threaded to the op. So a body that parks lexically
;; AND through a helper gets the cheap park for the first and today's capture for
;; the second, with no static analysis deciding anything.
;;
;; The channel protocol is unchanged: the same R3 waiter handler, the same
;; commit-under-wmu, the same "release the channel mutex before parking". Only the
;; park itself differs, and it is the last thing either path does.
;;
;; Not covered: alts! (threading a continuation through the waiter registration in
;; __do-alts is its own round) and a park inside a try (the rewrite would have to
;; carry the exception frame explicitly). Both fall back to the capture.

;; The cheap-park counter is (jolt-sm-parks), the mirror of
;; (jolt-fiber-chan-parks) (continuation captures in channel ops). Both are summed
;; over the carriers in fibers.ss and bumped through jolt-fiber-bump-sm-parks! /
;; jolt-fiber-bump-chan-parks!, which touch only the parking fiber's own carrier.
;; The gates read both: a body whose parks were all rewritten moves this one and
;; leaves that one alone.

;; --- the driver invariant ----------------------------------------------------
;; The sm field holds 'running while a driver is on the stack, the pending step
;; while parked, #f otherwise. A cheap park is only correct UNDER a driver: the
;; resume is re-entered through the fiber's thunk, and if that thunk is not
;; jolt-sm-drive then nothing consumes the stored step and the thunk re-runs the
;; body FROM THE TOP — a silent re-take. Calling __sm-take by hand on an ordinary
;; fiber is the way to get there, so it is checked rather than described.
;;
;; Checked at the OP, before the channel is touched. Checking it at the park
;; instead let the op run to the point of no return first: the handler is already
;; on the channel's waiter list and the fiber is already committed to 'parked
;; under the handler's wmu, so the throw left a registered waiter behind and a
;; value delivered to it afterwards went nowhere. Here the fiber dies with the
;; channel exactly as it was.
(define (jolt-sm-check-driver! who f)
  (unless (eq? (jolt-fiber-sm f) 'running)
    (error who
           "cheap park outside a CPS'd go body (needs jolt-sm-drive as the fiber thunk)"
           (jolt-fiber-sm f))))

;; --- the park ---------------------------------------------------------------
;; THE INVARIANT THE CHEAP PARK RESTS ON, which the pass states as its
;; consequence ("not covered: a park inside a try") rather than as itself:
;;
;;   NO dynamic-wind MAY SIT BETWEEN jolt-sm-drive AND A CHEAP PARK SITE.
;;
;; A continuation park re-enters through k, so Chez rewinds the chain and every
;; winder is put back. A cheap park has no k. It escapes to the scheduler — which
;; runs every after-thunk above the carrier's base — and the resume comes back in
;; through the THUNK, at base, with nothing rebuilt. So a wind in that extent is
;; not suspended across the park, it is destroyed by it: the after-thunk fires
;; while the computation is still live and the before-thunk never runs again. A
;; parameterize would revert, a lock would be released and not retaken, a
;; binding frame would be popped out from under the code that pushed it.
;;
;; What keeps it true is the pass, not this file, and the two heads it turns on
;; are opaque for DIFFERENT reasons. `try` is the only head the back end emits a
;; dynamic-wind for, and only for its finally clause (backend_scheme emit-try).
;; `fn*` emits a plain lambda and no wind at all; it is opaque because the pass
;; cannot see what the thunk is handed to, and a host form that takes one winds
;; around the call. That distinction is load-bearing and not pedantic: the pass
;; emits its OWN fn* continuations, in sm-kont and sm-cps-loop, with cheap park
;; sites inside them, so a bare fn* has to be wind-free or the pass would break
;; this invariant by construction. Between them the two cover binding, dosync and
;; locking, since each puts its body behind one or the other. A park inside any of
;; them falls back to the capture, where the wind is fine.
;;
;; The invariant IS checked, by run-gosm.ss section 1c, which scans the emitted
;; Scheme for a rewritten park site inside a dynamic-wind's extent. Nesting and
;; not presence: a body may legally carry both, and does whenever a park inside a
;; try falls back. What the drift check beside it cannot see is exactly this
;; direction — that one catches a special form ADDED to the analyzer, not the back
;; end growing a wind for a head the pass already rewrites, and not a new
;; rewriting arm for a head that winds. Either change means adding the head to
;; sm-opaque in the same commit; 1c is what says so instead of hoping.
;;
;; Store the rest of the computation and hand the carrier back. The differences
;; from jolt-fiber-to-scheduler! are the whole point: no call/1cc, and k is left
;; CLEAR so the scheduler re-enters through the thunk (jolt-sm-drive below), which
;; is what re-establishes the body's exception handler on every resume. Everything
;; else — clearing the current-fiber vreg, saving the slice, flagging the escape as
;; a park so try/finally after-thunks skip — is identical, and has to be.
;;
;; The two field writes below are deliberately UNORDERED with respect to a
;; concurrent observer: sm first leaves (k = a consumed continuation, sm = step)
;; visible, k first would leave (k = none, sm = 'running), and jolt-fiber-resume*
;; dispatches wrongly on either. Neither order is safe on its own. What makes it
;; safe is R0(d): the fiber is pinned to its carrier and ONLY that carrier's
;; thread dequeues it, so the carrier executing this sequence cannot also be
;; dispatching it. A cross-thread sa-fiber-resume landing mid-sequence only flips
;; 'parked to 'ready and enqueues. Anything that gave a second thread the right to
;; run this fiber — a manual sa-fiber-run-all against a live pool is the one way
;; in — breaks the park, not just the counters.
(define (jolt-sm-park! f resume)
  ;; unreachable, both ops check before they touch the channel; kept because this
  ;; is the point the invariant is actually load-bearing
  (jolt-sm-check-driver! 'jolt-sm-park! f)
  ;; The other switch point, and the same invariant (locks.ss). A cheap park is
  ;; the worse of the two to break it from: it does not rewind, so a lock the
  ;; escape released is never retaken, and the resumed step runs on believing it
  ;; still holds one.
  (jolt-locks-assert-none! 'jolt-sm-park!)
  (jolt-fiber-bump-sm-parks! f)
  (jolt-fiber-sm-set! f resume)
  (jolt-fiber-k-set! f #f)
  (set-virtual-register! jolt-vreg-current-fiber 0)
  (jolt-fiber-slice-save! f)
  ;; Same escape discipline as jolt-fiber-to-scheduler!: drop the finally
  ;; winders so their after-thunks do not run on a park, leave every other
  ;; winder to unwind normally.
  (jolt-park-drop-finallys!)
  (jolt-park-unwinding-set! #t)
  ;; NO interrupt depth is recorded, which is the one place this differs from
  ;; jolt-fiber-to-scheduler! rather than merely skipping its capture. That one
  ;; saves the depth in jolt-fiber-sic because its resume re-enters THROUGH k, back
  ;; inside whatever disabled region the fiber parked in, so the region has to be
  ;; rebuilt around it. A cheap park has no k and does not rewind: the region
  ;; jolt-sm-commit! opened is destroyed by this escape exactly as a dynamic-wind
  ;; would be, and the resume comes in through the thunk at the carrier's baseline
  ;; with nothing to restore. jolt-fiber-run reads sic only when k is set, so a
  ;; value written here could never be read anyway — it was, and it read as though
  ;; the depth travelled (jolt-kkt3). fibers-sm-test.ss scenario 13 pins the depth
  ;; a resumed step actually runs at, in both directions.
  ((jolt-carrier-sched-k (jolt-fiber-carrier f))))

;; Commit to a cheap park on an already-registered handler whose channel mutex is
;; RELEASED. The commit is atomic with alt-deliver!'s mailbox write under h's wmu,
;; exactly as jolt-fiber-waiter-wait! does it: a deliver that beat the commit is
;; seen here and the resume runs inline instead of parking.
;; The commit and the park that follows it are ONE non-preemptible region. A
;; timer landing between them finds the fiber already marked 'parked, sets it
;; back to 'ready and enqueues it, and then the park runs anyway — so the fiber
;; is on the run queue AND parked, gets dispatched, and returns from its park
;; with an empty mailbox. jolt-sm-park! clears the region as it escapes; the
;; no-park path exits it here.
(define (jolt-sm-commit! f h resume)
  (jolt-fiber-may-park! 'jolt-sm-commit!)
  (disable-interrupts)
  (let* ((park? (jolt-with-mutex (alt-handler-wmu h)
                  (if (vector-ref (alt-handler-mailbox h) 0)
                      #f
                      (begin (jolt-fiber-state-set! f 'parked) #t)))))
    (if park?
        (jolt-sm-park! f resume)
        (begin (enable-interrupts) (resume)))))

;; --- the driver -------------------------------------------------------------
;; The fiber thunk of a CPS'd body. It runs on the first entry AND on every
;; resume, because jolt-fiber-resume* enters through the thunk whenever k is
;; clear and a cheap park clears it. That re-entrance is what makes the body's
;; error handling survive a park: a cheap park leaves no frames, so nothing else
;; could put the handler back.
;;
;; The handler's job is jolt-fiber-go-spawn's contract for a throwing body —
;; report it and CLOSE the result channel, so a reader sees nil instead of
;; waiting forever. resume* deliberately does not guard an sm resume (its handler
;; marks the fiber dead WITHOUT closing that channel, which is the wrong one),
;; so this is the only thing between a throwing body and the carrier.
;;
;; with-exception-handler and not `guard`: guard unwinds to its own body, which
;; costs a call/cc per entry, and this sits on the resume path the cheap park
;; exists to make cheap (measured: 1.25x -> 1.05x). Nothing here needs the
;; unwind — jolt-fiber-dead! escapes to the scheduler itself, so the handler
;; never returns to the raise point.
(define (jolt-sm-drive w body-fn)
  (let ((f (jolt-current-fiber)))
    (with-exception-handler
      (lambda (e)
        ;; a failure while reporting or closing must still mark the fiber dead
        (guard (_ (#t (jolt-fiber-dead! f e)))
          (async-report-uncaught! "go/fiber body (channel closed)" e)
          (go-chan-finish! w e))            ; publishes the failure, then closes w
        (jolt-fiber-dead! f e))
      (lambda ()
        (let ((step (jolt-fiber-sm f)))
          ;; 'running marks "a driver is on the stack" — see jolt-sm-park!
          (jolt-fiber-sm-set! f 'running)
          (if (procedure? step)
              (step)
              (jolt-invoke body-fn (lambda (v) (jolt-sm-finish! w f v)))))))))

;; The terminal continuation on a fiber. The value cannot simply be returned: after
;; a cheap park nothing is left on the stack to return through, so the delivery and
;; the completion both happen here. A nil result just closes the channel (nil is
;; not a channel value).
(define (jolt-sm-finish! w f v)
  (when (not (jolt-nil? v)) (jolt-async-give w v))
  (go-chan-finish! w #f)                    ; completed: no throwable, then closes w
  (jolt-fiber-done! f v))

;; The terminal continuation on a thread: every CPS call is a tail call, so
;; returning the value hands it to the thunk's caller and today's wrapper does the
;; delivery unchanged.
(define (jolt-sm-thread-finish v) v)

;; --- the spawn --------------------------------------------------------------
;; (__sm-spawn body-fn) -> channel, where body-fn is (fn [k] ...). Honors
;; *go-backend* at spawn time, like async-go-spawn.
(define (jolt-sm-spawn body-fn)
  (if (eq? (go-backend-current) jolt-go-backend-fiber)
      (jolt-sm-fiber-spawn body-fn)
      (async-go-spawn-thread (lambda () (jolt-invoke body-fn jolt-sm-thread-finish)))))

;; The thunk is RE-ENTRANT — jolt-fiber-resume* runs it again on every cheap park
;; — so it must hold nothing that is meant to happen once per fiber.
;; jolt-fiber-go-spawn's thunk opens with (*txn* #f), and copying that here would
;; have re-cleared *txn* on every resume, after jolt-fiber-slice-restore! had just
;; put the fiber's own value back. Nothing reaches it today (dosync expands
;; through an fn*, which is opaque to the pass, so a park inside a transaction
;; takes the capture), but the line is not needed either way: sa-fiber-spawn
;; builds the child's slice with txn #f by construction, and jolt-fiber-run
;; restores that slice before the first entry.
;; The monitor registration has to happen HERE TOO, not only in
;; jolt-fiber-go-spawn: a go body the CPS pass could transform is spawned by
;; this function instead, and which of the two runs is a property of the body
;; (whether every park site was rewritable), not something the caller chose. A
;; monitor that worked on one and silently answered nil on the other would be
;; worse than no monitor at all, because the difference is invisible from the
;; jolt side. go-chan-register! lives in async.ss, which rt.ss loads first.
(define (jolt-sm-fiber-spawn body-fn)
  (let ((w (ac-make 1 'fixed #f)))
    (go-chan-register! w)
    (sa-fiber-spawn
     (lambda ()
       (jolt-sm-drive w body-fn)))
    (jolt-fiber-ensure-carrier!)
    w))

;; --- the ops ----------------------------------------------------------------
;; Each op is (op args... k). Off a fiber it is today's blocking op with k applied
;; to the result in tail position — so a CPS'd body costs a thread backend nothing
;; and grows no stack per park. On a fiber a ready channel completes inline and an
;; empty/full one parks cheaply.

;; The fiber branch checks the driver invariant BEFORE the channel mutex, so a
;; misuse throws with the channel and the waiter lists exactly as it found them.
(define (jolt-sm-take ch k)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-sm-check-driver! 'clojure.core.async/__sm-take f)
               (jolt-sm-fiber-take f ch k))
        (jolt-invoke k (jolt-async-take ch)))))

(define (jolt-sm-fiber-take f ch k)
  (jolt-fiber-may-park! 'clojure.core.async/<!)   ; before registering as a taker
  (jolt-chan-lock! ch)
  (let ((r (ac-poll!/locked ch)))
    (if (eq? r ac-poll-empty)
        (let ((h (jolt-fiber-waiter f)))
          (async-chan-alt-takers-set! ch (append (async-chan-alt-takers ch) (list h)))
          (ac-notify! ch)
          (if (vector-ref (alt-handler-mailbox h) 0)
              (let ((v (vector-ref (alt-handler-mailbox h) 1)))
                (jolt-chan-unlock! ch)
                (jolt-invoke k v))
              (begin
                (jolt-chan-unlock! ch)
                (jolt-sm-commit!
                 f h (lambda () (jolt-invoke k (vector-ref (alt-handler-mailbox h) 1)))))))
        (begin
          (jolt-chan-unlock! ch)
          (jolt-invoke k r)))))

(define (jolt-sm-put ch v k)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-sm-check-driver! 'clojure.core.async/__sm-put f)
               (jolt-sm-fiber-put f ch v k))
        (jolt-invoke k (jolt-async-give ch v)))))

(define (jolt-sm-fiber-put f ch v k)
  (jolt-fiber-may-park! 'clojure.core.async/>!)   ; before registering as a putter
  ;; the nil check BEFORE the mutex: it throws, and this path releases by hand
  (async-check-put! v)
  (jolt-chan-lock! ch)
  (let ((r (jolt-chan-locked-give! ch v)))
    (cond
      ((eq? r 'ok) (jolt-chan-unlock! ch) (jolt-invoke k #t))
      ((eq? r 'closed) (jolt-chan-unlock! ch) (jolt-invoke k #f))
      (else
       (let ((h (jolt-fiber-waiter f)))
         (async-chan-alt-putters-set! ch
           (append (async-chan-alt-putters ch) (list (cons h v))))
         (ac-notify! ch)
         (if (vector-ref (alt-handler-mailbox h) 0)
             (let ((ok (vector-ref (alt-handler-mailbox h) 1)))
               (jolt-chan-unlock! ch)
               (jolt-invoke k ok))
             (begin
               (jolt-chan-unlock! ch)
               (jolt-sm-commit!
                f h (lambda () (jolt-invoke k (vector-ref (alt-handler-mailbox h) 1)))))))))))

(cca-def! "__sm-spawn" jolt-sm-spawn)
(cca-def! "__sm-take" jolt-sm-take)
(cca-def! "__sm-put" jolt-sm-put)
