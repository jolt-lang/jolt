;; host/chez/sampler.ss — the sampling profiler behind jolt.profile/sample!.
;;
;; It is the SAMPLE client of the tick scheduler (ticks.ss), so it reaches every
;; thread that runs jolt code: the timer is per thread and it is the only
;; interrupt that is (a signal is delivered to the main thread only). A thread
;; joins when sampling starts on it, when it is forked while sampling is on,
;; and otherwise at its next timer event of any kind — a fiber's quantum, a
;; run-interruptible poll, the slow watch every thread keeps armed (ticks.ss). A
;; thread blocked in a wait joins when it wakes and runs.
;;
;; TIME, NOT TICKS. Ticks count calls and back-edges, and what a tick costs
;; varies by orders of magnitude, so a sample every N ticks would weigh a loop of
;; cheap calls far above a loop of expensive ones. The client reads the thread's
;; CPU clock each time it is due and takes a sample once the interval has passed,
;; weighted by the CPU time since the last sample. Its demand adapts to the
;; thread's measured rate so that a check lands about when the interval ends,
;; and most fires are a clock read.
;;
;; A SAMPLE is the stack under the interrupted call, each frame identified by its
;; code object and return offset. The first time a thread sees a frame it reads
;; the frame's name and source position (sa-frame-source-pair, the inspector),
;; and caches that as a SITE; after that a frame is two hash lookups. Stacks are
;; counted in a per-thread table keyed by their sites, so memory grows with the
;; number of distinct stacks, not with the run's length. Nothing holds the
;; continuation.
;;
;; Sites resolve to Clojure fns and lines only when the profile is read, through
;; the same path error traces use (source-registry.ss srcreg-logical-frames).
;;
;; What it cannot show: frames erased by tail calls, threads blocked in a wait
;; (this is a CPU profile), and the difference between a leaf and its caller when
;; the leaf makes no call the timer could be polled at.

(define jolt-sample-gen 0)            ; bumped by each start; a thread on an older one rejoins
(define jolt-sample-interval 1000000) ; nanoseconds of thread CPU time between samples
(define jolt-sample-depth 64)
(define jolt-sample-start-stats #f)
(define jolt-sample-stop-stats #f)
(define jolt-sample-tables '())       ; every joined thread's record, for the reader
(define jolt-sample-mu (make-mutex))

;; The demand a thread starts with, and the bounds on the adapted one. The lower
;; bound keeps a thread in a slow foreign-heavy stretch from paying a clock read
;; every few calls; the upper keeps a thread whose rate was measured across a
;; long wait from going quiet.
(define jolt-sample-first-ticks 10000)
(define jolt-sample-min-ticks 1000)
(define jolt-sample-max-ticks 20000000)

;; per-thread record
(define (sample-rec-make gen thread)
  ;; #(gen thread mutex stacks sites last-cpu check-cpu demand next-site-id)
  (vector gen thread (make-mutex)
          (make-hashtable sample-stack-hash sample-stack=?)
          (make-weak-eq-hashtable)
          (sa-thread-cpu-nanos) (sa-thread-cpu-nanos) jolt-sample-first-ticks 0))
(define (sample-rec-gen r) (vector-ref r 0))
(define (sample-rec-thread r) (vector-ref r 1))
(define (sample-rec-mu r) (vector-ref r 2))
(define (sample-rec-stacks r) (vector-ref r 3))
(define (sample-rec-sites r) (vector-ref r 4))

;; A stack is a vector of sites, innermost first. A site is #(id name
;; source-pair), one per thread for each place a frame was seen stopped, so two
;; stacks are the same when their sites are the same objects, and the hash folds
;; the small fixnum ids.
(define (sample-stack-hash v)
  (let ((n (vector-length v)))
    (let loop ((i 0) (h n))
      (if (fx=? i n)
          h
          (loop (fx+ i 1)
                (fxand (fx+ (fx* (fxand h #x3ffffff) 31) (vector-ref (vector-ref v i) 0))
                       #x3fffffff))))))

(define (sample-stack=? a b)
  (let ((n (vector-length a)))
    (and (fx=? n (vector-length b))
         (let loop ((i 0))
           (or (fx=? i n)
               (and (eq? (vector-ref a i) (vector-ref b i)) (loop (fx+ i 1))))))))

(define (sample-site! r code offset frame)
  (let* ((sites (sample-rec-sites r))
         (by-off (hashtable-ref sites code '()))
         (hit (assv offset by-off)))
    (if hit
        (cdr hit)
        (let ((site (vector (vector-ref r 8) (sa-code-name code) (sa-frame-source-pair frame))))
          (vector-set! r 8 (fx+ 1 (vector-ref r 8)))
          (hashtable-set! sites code (cons (cons offset site) by-off))
          site))))

;; The scheduler's own frames sit above the interrupted call: this client, and
;; the dispatcher that called it.
(define (sample-plumbing-site? site)
  (let ((nm (vector-ref site 1)))
    (and nm (or (string=? nm "jolt-tick-fire-multi")
                (string=? nm "jolt-tick-fire")
                (string=? nm "jolt-sample-tick")
                (string=? nm "jolt-sample-take!")))))

(define (jolt-sample-take! r weight)
  (call/cc
    (lambda (k)
      (let* ((acc '())
             (cap (fx+ jolt-sample-depth 4))
             (n (sa-stack-walk k cap
                  (lambda (code offset frame)
                    (set! acc (cons (sample-site! r code offset frame) acc)))))
             (frames (let drop ((fs (reverse acc)))
                       (if (and (pair? fs) (sample-plumbing-site? (car fs))) (drop (cdr fs)) fs)))
             ;; cut when the walk hit its cap, or when the frames left after the
             ;; plumbing still run past the depth
             (truncated? (or (fx>=? n cap) (fx>? (length frames) jolt-sample-depth)))
             (frames (if (fx>? (length frames) jolt-sample-depth)
                         (list-head frames jolt-sample-depth)
                         frames))
             (key (list->vector (if truncated? (append frames (list 'truncated)) frames)))
             (key (vector-map (lambda (s) (if (eq? s 'truncated) sample-truncated-site s)) key)))
        (jolt-with-mutex (sample-rec-mu r)
          (let ((cell (hashtable-ref (sample-rec-stacks r) key #f)))
            (if cell
                (begin (vector-set! cell 0 (fx+ 1 (vector-ref cell 0)))
                       (vector-set! cell 1 (+ weight (vector-ref cell 1))))
                (hashtable-set! (sample-rec-stacks r) key (vector 1 weight)))))))))
(define sample-truncated-site (vector -1 "[truncated]" #f))

;; The next tick demand, from the demand PREV just spent, the CPU nanoseconds
;; SPENT doing it and the nanoseconds REMAINING to the next sample: ticks per
;; nanosecond as measured, and at most twice PREV. One interval read short (a
;; clock that did not move, a collection charged to the ticks) must not send the
;; demand to the ceiling: the tail of a run became one long interval, lost when
;; sampling stopped, and a 40 ms fn read as 25.
(define (jolt-sample-next-demand prev remaining spent)
  (let ((next (if (> spent 0) (quotient (* prev remaining) spent) (* 2 prev))))
    (max jolt-sample-min-ticks (min jolt-sample-max-ticks (* 2 prev) next))))

;; The client. Runs on the sampled thread from the tick scheduler's dispatcher.
(define (jolt-sample-tick)
  (let* ((s (jolt-tick-state))
         (r (and (vector? s) (vector-ref s jolt-tick-sampler))))
    (cond
      ((not (and jolt-tick-sampling? r (eqv? (sample-rec-gen r) jolt-sample-gen)))
       ;; Stopped, or restarted since this thread joined: leave, and rejoin the
       ;; new run if there is one.
       (when (vector? s) (vector-set! s jolt-tick-sampler #f))
       (when jolt-tick-sampling? (jolt-sample-join!)))
      (else
       (let* ((now (sa-thread-cpu-nanos))
              (spent (- now (vector-ref r 6)))
              (since (- now (vector-ref r 5)))
              (remaining (if (>= since jolt-sample-interval)
                             (begin (jolt-sample-take! r since)
                                    (vector-set! r 5 now)
                                    jolt-sample-interval)
                             (- jolt-sample-interval since)))
              (next (jolt-sample-next-demand (vector-ref r 7) remaining spent)))
         (vector-set! r 6 now)
         (vector-set! r 7 next)
         (jolt-tick-set! jolt-tick-slot-sample next))))))

;; Join the current run on this thread.
(define (jolt-sample-join!)
  (let ((r (sample-rec-make jolt-sample-gen (get-thread-id))))
    (jolt-with-mutex jolt-sample-mu
      (set! jolt-sample-tables (cons r jolt-sample-tables)))
    (jolt-tick-set! jolt-tick-slot-sample jolt-sample-first-ticks)
    ;; registering made the thread multi, so the state vector is there now
    (vector-set! (jolt-tick-state) jolt-tick-sampler r)))

(jolt-tick-client-set! jolt-tick-slot-sample jolt-sample-tick)

;; (jolt-sample-start! interval-ns depth) -> #t | #f. Drops the previous run's
;; samples. #f when the target has no stacks to walk.
(define (jolt-sample-start! interval depth)
  (and (sa-stack-walk (call/cc (lambda (k) k)) 1 (lambda (c o f) (void)))
       (begin
         (jolt-with-mutex jolt-sample-mu
           (set! jolt-sample-gen (fx+ jolt-sample-gen 1))
           (set! jolt-sample-tables '())
           (set! jolt-sample-interval (max 1000 interval))
           (set! jolt-sample-depth (max 1 depth))
           (set! jolt-sample-start-stats (sa-stats))
           (set! jolt-sample-stop-stats #f))
         (jolt-tick-sampling-set! #t jolt-sample-join!)
         (jolt-sample-join!)
         #t)))

(define (jolt-sample-stop!)
  (when jolt-tick-sampling?
    (jolt-tick-sampling-set! #f jolt-sample-join!)
    (set! jolt-sample-stop-stats (sa-stats))))

;; --- reading ------------------------------------------------------------------
;; A site's frames as #(name file line def-line), innermost first: one site can
;; stand for several fns when code was inlined into it. #f where there is none.
(define (sample-site-frames site cache)
  (or (hashtable-ref cache site #f)
      (let* ((nm (vector-ref site 1))
             (fs (if (eq? site sample-truncated-site)
                     (list (vector "[truncated]" #f #f #f))
                     (map sample-frame-row
                          (srcreg-logical-frames nm (lambda () (vector-ref site 2)))))))
        (hashtable-set! cache site fs)
        fs)))
(define (sample-frame-row f)
  (let ((r (srcreg-frame-rec f)) (line (srcreg-frame-line f)))
    (if (vector? r)
        (vector (string-append (vector-ref r 0) "/" (vector-ref r 1))
                (and (string? (vector-ref r 2)) (vector-ref r 2))
                (or line (vector-ref r 3))
                (vector-ref r 3))
        (vector (srcreg-display-name (srcreg-frame-nm f)) #f #f #f))))

;; Every recorded stack as (thread count nanos frames), frames a list of
;; #(name file line def-line) ROOT FIRST.
(define (jolt-sample-rows)
  (let ((cache (make-eq-hashtable))
        (tables (jolt-with-mutex jolt-sample-mu jolt-sample-tables)))
    (apply append
      (map (lambda (r)
             ;; Copied with interrupts off: this thread may be one of the sampled
             ;; ones, and its own sample must not land in the table while it is
             ;; read (the mutex is recursive, so it would not wait).
             (let ((entries
                    (begin
                      (disable-interrupts)
                      (let ((es (jolt-with-mutex (sample-rec-mu r)
                                  (let-values (((ks cs) (hashtable-entries (sample-rec-stacks r))))
                                    (map (lambda (k c) (list k (vector-ref c 0) (vector-ref c 1)))
                                         (vector->list ks) (vector->list cs))))))
                        (enable-interrupts)
                        es))))
               (map (lambda (e)
                      ;; innermost first within a site and across them, then reversed
                      (let ((inner (apply append (map (lambda (site) (sample-site-frames site cache))
                                                      (vector->list (car e))))))
                        (list (sample-rec-thread r) (cadr e) (caddr e) (reverse inner))))
                    entries)))
           tables))))

;; The collector's CPU time over the run, for the whole process.
(define (jolt-sample-gc-nanos)
  (let ((start jolt-sample-start-stats)
        (end (or jolt-sample-stop-stats (sa-stats))))
    (if start (- (vector-ref end 3) (vector-ref start 3)) 0)))

;; (jolt-sample-read) -> [gc-nanos [[thread count nanos frames] ...]], each frame
;; [name file line def-line] with nil for what it does not have.
(define (jolt-sample-read)
  (let ((nil-or (lambda (x) (or x jolt-nil))))
    (jolt-vector
      (jolt-sample-gc-nanos)
      (apply jolt-vector
        (map (lambda (row)
               (jolt-vector (car row) (cadr row) (caddr row)
                            (apply jolt-vector
                                   (map (lambda (f) (apply jolt-vector (map nil-or (vector->list f))))
                                        (cadddr row)))))
             (jolt-sample-rows))))))

;; --- JOLT_PROFILE=sample ----------------------------------------------------------
;; Sample the whole run and report on stderr at exit. The CLI calls this, and so
;; does a built binary's launcher, which is why the report does not depend on
;; jolt.profile: a binary has it only when the app requires it. When it is there
;; its report is used; otherwise the fns tables are printed from here.
(define (jolt-sample-init-from-env!)
  (let ((e (getenv "JOLT_PROFILE")))
    (when (and (string? e) (string=? e "sample")
               (jolt-sample-start! jolt-sample-interval jolt-sample-depth))
      (jolt-add-runtime-shutdown-hook jolt-sample-report-at-exit))))

;; --- jolt build --profile-gen ------------------------------------------------------
;; A binary built with --profile-gen carries Chez's block counters in every unit,
;; and they reach a file as it exits, where `jolt build --profile-use` reads them
;; (build.ss). Every binary's launcher calls this, since the launcher's text has
;; to be the same in both builds; one with no counters writes nothing. The file
;; is $JOLT_PROFILE_DATA, or NAME-PID.jpd in the current directory, NAME the
;; binary's: one per run, so runs over several workloads each leave their own
;; and the build averages them. TAG is the build mode, which --profile-use has
;; to match.
(define (jolt-block-profile-at-exit! tag)
  (jolt-add-runtime-shutdown-hook
    (lambda ()
      (let ((path (let ((e (getenv "JOLT_PROFILE_DATA"))
                        ;; asked of the OS (loader.ss): a binary's argv[0] is empty
                        (exe (guard (e (#t #f)) (aot-self-exe))))
                    (if (and (string? e) (not (string=? e "")))
                        e
                        (string-append (if (string? exe) (path-last exe) "jolt")
                                       "-" (number->string (get-process-id)) ".jpd")))))
        (guard (e (#t (fprintf (current-error-port) "jolt: could not write the block profile ~a: " path)
                      (display-condition e (current-error-port))
                      (newline (current-error-port))))
          (when (fx>? (sa-block-profile-dump! path tag) 0)
            (fprintf (current-error-port) "jolt: wrote the block profile ~a\n" path)))))))

(define (jolt-sample-report-at-exit)
  (let ((cell (var-cell-lookup "jolt.profile" "sample-report-at-exit")))
    (if (and cell (var-cell-defined? cell))
        (jolt-invoke (var-cell-root cell))
        (begin (jolt-sample-stop!)
               (jolt-sample-print-report (current-error-port) 20)))))

(define (jolt-sample-print-report port limit)
  (let* ((rows (jolt-sample-rows))
         (fns (make-hashtable string-hash string=?))
         (row-for (lambda (f)
                    (or (hashtable-ref fns (vector-ref f 0) #f)
                        (let ((r (vector (vector-ref f 0) (vector-ref f 1) (vector-ref f 3) 0 0)))
                          (hashtable-set! fns (vector-ref f 0) r)
                          r))))
         (ms (lambda (ns) (exact->inexact (/ (round (/ ns 100000)) 10))))
         (n 0) (total 0))
    (for-each
      (lambda (row)
        (let ((nanos (caddr row)) (frames (cadddr row)))
          (set! n (+ n (cadr row)))
          (set! total (+ total nanos))
          (let ((seen (make-hashtable string-hash string=?)))
            (for-each (lambda (f)
                        (unless (hashtable-ref seen (vector-ref f 0) #f)
                          (hashtable-set! seen (vector-ref f 0) #t)
                          (let ((r (row-for f))) (vector-set! r 4 (+ nanos (vector-ref r 4))))))
                      frames))
          (let ((leaf (if (pair? frames) (list-ref frames (- (length frames) 1)) (vector "[runtime]" #f #f #f))))
            (let ((r (row-for leaf))) (vector-set! r 3 (+ nanos (vector-ref r 3)))))))
      rows)
    (let* ((all (vector->list (hashtable-values fns)))
           (show (lambda (title col)
                   (fprintf port "~a\n" title)
                   (let loop ((rs (list-sort (lambda (a b) (> (vector-ref a col) (vector-ref b col))) all))
                              (i 0))
                     (when (and (pair? rs) (< i limit))
                       (let ((r (car rs)))
                         (fprintf port "~12a  ~a~a\n" (ms (vector-ref r col)) (vector-ref r 0)
                                  (if (vector-ref r 1)
                                      (format "  ~a:~a" (vector-ref r 1) (or (vector-ref r 2) "?"))
                                      "")))
                       (loop (cdr rs) (+ i 1))))
                   (newline port))))
      (if (= n 0)
          (fprintf port "jolt.profile: no samples\n")
          (begin
            (fprintf port "jolt.profile: ~a samples, ~a ms of CPU time, ~a ms in the collector\n\n"
                     n (ms total) (ms (jolt-sample-gc-nanos)))
            (show "fns by self time (ms)" 3)
            (show "fns by total time (ms)" 4))))))

(def-var! "jolt.host" "sample-start!" jolt-sample-start!)
(def-var! "jolt.host" "sample-stop!" (lambda () (jolt-sample-stop!) jolt-nil))
(def-var! "jolt.host" "sampling?" (lambda () jolt-tick-sampling?))
(def-var! "jolt.host" "sample-read" jolt-sample-read)
