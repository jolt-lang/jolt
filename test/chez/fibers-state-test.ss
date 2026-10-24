;; test/chez/fibers-state-test.ss — R2 gate for per-fiber dynamic state
;; (epic jolt-nvpr.3). Run: chez --script test/chez/fibers-state-test.ss
;; (wired in as part of `make fibers`).
;;
;; R0 settled the scope: `guard` and `parameterize` need nothing (a resumed
;; fiber's own handler catches; parameterize unwinds on park and reinstates on
;; resume). The one real bug: dyn-binding.ss pushes a binding frame by calling
;; the dyn-binding-stack thread parameter as a SETTER, and a setter write is
;; not undone by a continuation escape — a fiber that parks inside a binding
;; leaves its frames on the carrier, visible to the scheduler and to every
;; other fiber, and a second fiber popping its own frame can pop the parked
;; fiber's. set-chez-ns! leaks the same way. R2 fixes it by saving and
;; restoring the fiber's dynamic slice (dyn-binding-stack, current ns, *txn*)
;; in the switch.
;;
;; This file loads the FULL host runtime (rt.ss), because the slice tests need
;; the real dyn-binding-stack / chez-current-ns-param / *txn* parameters plus
;; the push/bind and STM seams. It drives them at host level
;; (jolt-push-thread-bindings / jolt-sync / jolt-ref-* / set-chez-ns!) rather
;; than through the overlay macros, so it stays a chez --script gate like
;; fibers-test.ss. The assert conventions match fibers-test.ss.
;;
;; Gate scenarios (spec, in order):
;;   1. `binding` in one fiber is invisible to another fiber on the same carrier.
;;   2. A parked fiber's binding frame is invisible to the scheduler — the exact
;;      R0 leak, as a regression test.
;;   3. A fiber's bindings are intact on resume after other fibers have pushed
;;      and popped their own.
;;   4. Two fibers on one carrier cannot join each other's transaction: a fiber
;;      inside a dosync parks, another fiber runs and sees no transaction — and
;;      its commit inside the first one's extent makes the first one retry.
;;   5. Spawn conveys the parent's bindings (and does NOT convey its *txn*).
;;   6. The current namespace follows the fiber, not the carrier.

(import (chezscheme))
(load "host/chez/rt.ss")
;; R5 (jolt-nvpr.6): the pool defaults to the processor count; this gate
;; drives fibers synchronously (sa-fiber-run-all on one carrier), so pin it —
;; the documented "pin to 1 for determinism" use of the count knob.
(jolt-fiber-carrier-count-set! 1)

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(printf "== per-fiber dynamic state ==\n")

;; --- 1. binding in one fiber invisible to another on the same carrier -------
(define *x* (def-dynvar! "app" "*x*" 0))
(define vis-log '())
(define vis-f1
  (sa-fiber-spawn
    (lambda ()
      (jolt-push-thread-bindings (jolt-hash-map *x* 42))
      (set! vis-log (cons (jolt-var-get *x*) vis-log))     ; 42, its own binding
      (sa-fiber-yield)
      (set! vis-log (cons (jolt-var-get *x*) vis-log))     ; still 42 after f2 ran
      (jolt-pop-thread-bindings))))
(define vis-f2
  (sa-fiber-spawn
    (lambda ()
      (set! vis-log (cons (jolt-var-get *x*) vis-log)))))  ; must be the root 0
(sa-fiber-run-all)
(ok "1. binding invisible to a sibling fiber" (equal? vis-log '(42 0 42)))
(ok "1. no frame left on the carrier after both done" (eq? (dyn-binding-stack) '()))

;; --- 2. a parked fiber's frame is invisible to the scheduler (R0 leak) ------
;; Without the slice swap, (dyn-binding-stack) after the drain would be
;; ((FIBER-FRAME)) — the fiber parks INSIDE the binding, the setter write
;; survives the continuation escape, and the leak is visible to the caller.
(define *y* (def-dynvar! "app" "*y*" 0))
(define leak-f
  (sa-fiber-spawn
    (lambda ()
      (jolt-push-thread-bindings (jolt-hash-map *y* 7))
      (jolt-fiber-park!)            ; park inside the binding
      (jolt-pop-thread-bindings))))
(sa-fiber-run-all)
(ok "2. parked fiber's frame invisible to the scheduler" (eq? (dyn-binding-stack) '()))
(ok "2. parked fiber's value not visible to the caller" (= (jolt-var-get *y*) 0))
(sa-fiber-resume leak-f)
(sa-fiber-run-all)
(ok "2. fiber that parked inside a binding completes"
    (eq? (jolt-fiber-state leak-f) 'done))

;; --- 3. bindings intact on resume after other fibers pushed and popped ------
(define *z* (def-dynvar! "app" "*z*" 0))
(define intact-log '())
(define z-f1
  (sa-fiber-spawn
    (lambda ()
      (jolt-push-thread-bindings (jolt-hash-map *z* 100))
      (set! intact-log (cons (jolt-var-get *z*) intact-log))   ; 100
      (sa-fiber-yield)
      ;; z-f2 has pushed and popped its own 200-frame meanwhile
      (set! intact-log (cons (jolt-var-get *z*) intact-log))   ; still 100
      (jolt-pop-thread-bindings))))
(define z-f2
  (sa-fiber-spawn
    (lambda ()
      (jolt-push-thread-bindings (jolt-hash-map *z* 200))
      (set! intact-log (cons (jolt-var-get *z*) intact-log))   ; 200
      (jolt-pop-thread-bindings))))
(sa-fiber-run-all)
(ok "3. bindings intact on resume after sibling push/pop"
    (equal? intact-log '(100 200 100)))
(ok "3. top-level stack clean after the round" (eq? (dyn-binding-stack) '()))

;; --- 4. two fibers on one carrier cannot join each other's transaction ------
;; t1 parks INSIDE a dosync (ref-set r 1, park, ref-set r 2). On park the
;; slice swap keeps the carrier's *txn* #f — t2 runs and sees NO transaction; if
;; it had inherited t1's *txn*, t2's jolt-sync would have joined it (ref-set into
;; t1's log) instead of starting its own. That is the slice property this section
;; is for.
;;
;; A parked transaction holds no lock, so t2 runs its whole transaction and
;; commits r = 9 inside t1's extent. t1's write then conflicts with that commit:
;; its commit retries, the body runs again over r = 9 and parks again, and the
;; outcome is serializable (t2 then t1).
(define r (jolt-ref-new 0))
(define t1-in-txn #f)        ; what t1 reads through its OWN log after resuming
(define t1-committed-r #f)   ; and what is COMMITTED at that moment
(define t1-runs 0)
(define t1
  (sa-fiber-spawn
    (lambda ()
      (jolt-sync
        (lambda ()
          (set! t1-runs (+ t1-runs 1))
          (jolt-ref-set r 1)
          (jolt-fiber-park!)
          ;; resumed: the log is t1's own
          (set! t1-in-txn (jolt-ref-deref r))
          (set! t1-committed-r (jolt-ref-val r))
          (jolt-ref-set r 2)))
      't1-committed)))
(sa-fiber-run-all)
(ok "4. t1 parked inside its txn" (eq? (jolt-fiber-state t1) 'parked))
(ok "4. no txn on the carrier while t1 is parked" (eq? (*txn*) #f))
(ok "4. t1's write not committed while parked" (= (jolt-ref-val r) 0))
(define txn-log '())
(define t2
  (sa-fiber-spawn
    (lambda ()
      (set! txn-log (cons (jolt-txn-running?) txn-log))        ; #f — cannot join
      (jolt-sync (lambda () (jolt-ref-set r 9)))               ; its OWN txn
      (set! txn-log (cons (jolt-txn-running?) txn-log)))))     ; #f — over, not joined
(sa-fiber-run-all)
(ok "4. t2 ran its own txn to the end, unjoined"
    (and (eq? (jolt-fiber-state t2) 'done) (equal? txn-log '(#f #f))))
(ok "4. and committed while t1 is parked" (= (jolt-ref-val r) 9))
;; t1 resumes inside ITS txn, conflicts with t2's commit, and runs again.
(sa-fiber-resume t1)
(sa-fiber-run-all)
(ok "4. t1 resumed inside ITS txn" (= t1-in-txn 1))
(ok "4. over t2's commit" (= t1-committed-r 9))
(ok "4. its commit conflicted and the body ran again" (and (= t1-runs 2) (eq? (jolt-fiber-state t1) 'parked)))
(sa-fiber-resume t1)
(sa-fiber-run-all)
(ok "4. t1 completed" (eq? (jolt-fiber-state t1) 'done))
(ok "4. and the outcome is serializable (t2 then t1)" (= (jolt-ref-val r) 2))

;; --- 5. spawn conveys the parent's bindings (and NOT its *txn*) ------------
(define *w* (def-dynvar! "app" "*w*" 0))
(define conv-log '())
(define conv-f
  (sa-fiber-spawn
    (lambda ()
      (jolt-push-thread-bindings (jolt-hash-map *w* 5))
      (let ((child
             (sa-fiber-spawn
               (lambda () (set! conv-log (cons (jolt-var-get *w*) conv-log))))))
        (sa-fiber-yield)                                    ; let the child run
        (set! conv-log (cons 'parent-ok conv-log)))
      (jolt-pop-thread-bindings))))
(sa-fiber-run-all)
(ok "5. spawn conveys the parent's bindings" (equal? conv-log '(parent-ok 5)))
(ok "5. parent's frame popped at the end" (= (jolt-var-get *w*) 0))
(define conv-txn #t)
(define txn-parent
  (sa-fiber-spawn
    (lambda ()
      (jolt-sync
        (lambda ()
          (let ((child
                 (sa-fiber-spawn
                   (lambda () (set! conv-txn (jolt-txn-running?))))))
            (sa-fiber-yield)))))))
(sa-fiber-run-all)
(ok "5. spawn does NOT convey the parent's txn" (eq? conv-txn #f))

;; --- 6. the current namespace follows the fiber, not the carrier ------------
(define ns-log '())
(define ns-f1
  (sa-fiber-spawn
    (lambda ()
      (set-chez-ns! "app.a")
      (set! ns-log (cons (chez-current-ns) ns-log))
      (sa-fiber-yield)
      (set! ns-log (cons (chez-current-ns) ns-log)))))
(define ns-f2
  (sa-fiber-spawn
    (lambda ()
      (set! ns-log (cons (chez-current-ns) ns-log))      ; must be the carrier's "user"
      (set-chez-ns! "app.b"))))
(sa-fiber-run-all)
(ok "6. namespace follows the fiber, not the carrier"
    (equal? ns-log '("app.a" "user" "app.a")))
(ok "6. carrier ns restored to the scheduler's after the round"
    (string=? (chez-current-ns) "user"))

;; --- 7. the park drops jolt finallys and NOTHING else -----------------------
;; A park escape unwinds the fiber's dynamic-wind chain. jolt-park-drop-finallys!
;; (fibers.ss) removes exactly the winders the back end emitted for a `finally`,
;; recognised by their before-thunk being the shared jolt-finally-in marker, and
;; leaves every other winder to unwind normally.
;;
;; Both halves have teeth, and the SECOND is the one with real consequences:
;;
;;   - drop too little and a finally runs mid-park, closing a file the fiber is
;;     still using. That is what the flag used to prevent.
;;   - drop too much and a with-mutex does not release on the park, so the fiber
;;     parks HOLDING the lock. loader.ss's ldr-wait-for-load! deliberately parks
;;     inside ldr-load-mu and relies on with-mutex's dynamic-wind releasing it
;;     and re-acquiring on resume; dropping that winder wedges every later
;;     require in the process. Chez tags with-mutex as the same `winder` record
;;     type as a finally, so nothing but the marker distinguishes them — which is
;;     exactly why this check exists.
;;
;; A parameterize is checked alongside it: it has to unwind so the next fiber on
;; the carrier does not inherit the parked fiber's value, and has to be back in
;; force when the fiber resumes.
(define w-log '())
(define (w-say x) (set! w-log (cons x w-log)))
(define w-mu (make-mutex))
(define w-param (make-parameter 'scheduler))
;; Probed from ANOTHER THREAD, and that is not incidental: Chez mutexes are
;; recursive, so the thread running the drain can re-acquire a mutex it is
;; already holding and would report "free" even when the parked fiber still owns
;; it — the exact deadlock this check exists to catch would pass silently.
(define (w-mutex-free?)
  (let ((free? #f))
    (thread-join
     (fork-thread
      (lambda ()
        (set! free? (if (mutex-acquire w-mu #f)
                        (begin (mutex-release w-mu) #t)
                        #f)))))
    free?))

(define w-fiber
  (sa-fiber-spawn
   (lambda ()
     (parameterize ((w-param 'fiber))
       (with-mutex w-mu
         (dynamic-wind
          jolt-finally-in                       ; the marker the back end emits
          (lambda ()
            (w-say (list 'before-park (w-param)))
            ;; jolt-fiber-park! and NOT sa-fiber-yield: yield re-enqueues, so a
            ;; one-fiber drain would resume it in the same pass and run to
            ;; completion before the park could be observed at all.
            (jolt-fiber-park!)
            (w-say (list 'after-park (w-param)))
            'ok)
          (lambda () (w-say 'FINALLY))))))))

;; run until the fiber parks in the yield
(jolt-fiber-drain! (jolt-fiber-carrier w-fiber))
(define w-at-park (reverse w-log))
(ok "7. finally did NOT run on the park"
    (not (memq 'FINALLY w-at-park)))
(ok "7. with-mutex DID release on the park (loader.ss depends on this)"
    (w-mutex-free?))
(ok "7. parameterize DID unwind on the park"
    (eq? (w-param) 'scheduler))

;; resume: a parked fiber is only runnable again through sa-fiber-resume
(sa-fiber-resume w-fiber)
(jolt-fiber-drain! (jolt-fiber-carrier w-fiber))
(define w-final (reverse w-log))
(ok "7. fiber resumed inside its own parameterize"
    (equal? (cadr w-final) '(after-park fiber)))
(ok "7. finally ran exactly once, at the real exit"
    (= 1 (length (filter (lambda (x) (eq? x 'FINALLY)) w-final))))
(ok "7. mutex released again after the real exit" (w-mutex-free?))
(ok "7. carrier parameter back to the scheduler's after the round"
    (eq? (w-param) 'scheduler))

;; The marker is matched with eq?, so it must be ONE shared procedure. A fresh
;; (lambda () #f) per site would compare unequal and the finally would run
;; mid-park again — silently, with every test above still passing except this.
(ok "7. a non-marker dynamic-wind is not mistaken for a finally"
    (not (jolt-finally-winder?
          (dynamic-wind (lambda () #f)
                        (lambda () (car (sa-current-winders)))
                        (lambda () #f)))))
(ok "7. a marker dynamic-wind IS recognised"
    (jolt-finally-winder?
     (dynamic-wind jolt-finally-in
                   (lambda () (car (sa-current-winders)))
                   (lambda () #f))))

;; --- 8. a winder must not re-establish state the dslice already carries -----
;; A hand-rolled binding frame over a dynamic extent,
;;     (dynamic-wind (lambda () (dyn-push-frame! pairs)) thunk (lambda () (pop)))
;; double-applies across a park. jolt-fiber-to-scheduler! saves the slice — which
;; already holds the pushed frame — BEFORE the escape unwinds; the unwind pops
;; it; on resume the slice restore puts it back and THEN the continuation rewinds
;; and the before-thunk pushes a second one. Three sites did this (*agent*,
;; *compile-files*, and the loader's file/spath frame) and all three were wrong
;; after any park, with no preemption involved.
;;
;; dyn-with-frame is the one correct shape: push ONCE outside the wind, nothing
;; in the before-thunk, and an ABSOLUTE restore in the after-thunk. Both halves
;; are checked below, and the broken shape is checked alongside it so this cannot
;; pass by accident if the helper stops being used.
(define (dyn-depths make-body)
  (let ((log '()))
    (define (note! tag) (set! log (cons (cons tag (length (dyn-binding-stack))) log)))
    (let* ((c (var-cell-lookup "clojure.core" "*compile-files*"))
           (f (sa-fiber-spawn
               (lambda ()
                 ((make-body c note!))
                 (note! 'after)))))
      (jolt-fiber-drain! (jolt-fiber-carrier f))
      (note! 'parked)
      (sa-fiber-resume f)
      (jolt-fiber-drain! (jolt-fiber-carrier f))
      (reverse log))))

(define (dyn-body-inner note!)
  (lambda () (note! 'in) (jolt-fiber-park!) (note! 'resumed)))

(define good
  (dyn-depths (lambda (c note!)
                (lambda () (dyn-with-frame (list (cons c #t)) (dyn-body-inner note!))))))
(define bad
  (dyn-depths (lambda (c note!)
                (lambda ()
                  (dynamic-wind
                   (lambda () (dyn-push-frame! (list (cons c #t))))
                   (dyn-body-inner note!)
                   (lambda () (dyn-binding-stack (cdr (dyn-binding-stack)))))))))

(ok "8. dyn-with-frame: depth is the same after a park as before"
    (= (cdr (assq 'in good)) (cdr (assq 'resumed good))))
(ok "8. dyn-with-frame: the frame is gone once the extent ends"
    (= 0 (cdr (assq 'after good))))
(ok "8. dyn-with-frame: the carrier is clean while the fiber is parked"
    (= 0 (cdr (assq 'parked good))))
;; The teeth: the shape dyn-with-frame replaced still double-applies and leaks.
;; If this ever passes, the hazard is gone by some other route and the helper's
;; comment needs revisiting — it does not mean the check is obsolete.
(ok "8. the hand-rolled push/pop shape still double-applies (teeth)"
    (and (= (+ 1 (cdr (assq 'in bad))) (cdr (assq 'resumed bad)))
         (= 1 (cdr (assq 'after bad)))))

(printf "\nfibers-state-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "fibers-state-test: PASS — per-fiber dynamic state\n") (exit 0))
    (exit 1))
