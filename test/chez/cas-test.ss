;; test/chez/cas-test.ss — compare-and-swap is STRONG: it answers #f only when
;; the value is not the expected one. Run: chez --script test/chez/cas-test.ss
;; (wired into `make cas`, part of `make ci`).
;;
;; WHY. Chez's $record-cas! is one ldxr/stxr attempt on AArch64 (s/arm64.ss
;; asm-cas): stxr fails whenever the core's exclusive monitor was cleared
;; between the two — a context switch, or another core storing into the same
;; reservation granule — and the primitive then answers #f with the field still
;; holding the expected value. That is a weak CAS, and every caller that read
;; the #f as "somebody else got there first" was wrong once in a few hundred
;; thousand under load: compare-and-set! told ring-chez-adapter's worker its
;; connection was claimed by another owner, the worker walked away, and the
;; connection was served by nobody (one in ~1400 accepts under ab on an M-series
;; Mac; never on x86, whose cmpxchg cannot fail spuriously). sa-record-cas! now
;; retries while the field still holds `old`, so the answer means what every
;; caller took it to mean.
;;
;; Scenario 2 and 3 are REPRODUCERS, not smokes: on the weak primitive they
;; count a handful of spurious failures per million attempts on Apple silicon
;; (5-8 per 2M measured), and zero on x86 whatever the primitive does. A pass
;; here on ARM is the property; a pass on x86 is only the semantics.

(import (chezscheme))
(load "host/chez/rt.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "  FAIL: ~a\n" name)))

(printf "== compare-and-swap is strong ==\n")

;; --- 1. semantics ------------------------------------------------------------
(printf "\n== 1. semantics ==\n")
(define-record-type cell (fields (mutable v) (mutable w)) (nongenerative cas-test-cell))
(define c (make-cell 'a 'b))
(ok "1. sa-record-cas! swaps when the field is eq? to old" (eq? #t (sa-record-cas! c 0 'a 'x)))
(ok "1. ...and the field moved" (eq? (cell-v c) 'x))
(ok "1. sa-record-cas! refuses when the field is not eq? to old" (eq? #f (sa-record-cas! c 0 'a 'y)))
(ok "1. ...and the field stayed" (eq? (cell-v c) 'x))
(ok "1. ...and the neighbouring field is untouched" (eq? (cell-w c) 'b))
(define a (jolt-atom-new #f))
(ok "1. compare-and-set! swaps when the value is eq? to old" (eq? #t (jolt-compare-and-set! a #f #t)))
(ok "1. ...and the value moved" (eq? #t (jolt-deref a)))
(ok "1. compare-and-set! refuses when the value is not eq? to old" (eq? #f (jolt-compare-and-set! a #f 'x)))
(ok "1. ...and the value stayed" (eq? #t (jolt-deref a)))

;; --- the harness: one CASer, many neighbour writers ---------------------------
;; The CASer allocates every record itself, so targets sit next to the fields the
;; writers store into — the same cache line, which is what clears an exclusive
;; monitor. Nobody but the CASer ever touches the field it swaps, so every #f it
;; is answered while the field still holds the expected value is spurious.
(define writers 8)
(define (mono-secs)
  (let ((t (current-time 'time-monotonic)))
    (+ (time-second t) (/ (exact->inexact (time-nanosecond t)) 1e9))))
(define (with-writers hammer! body)
  (let ((stop? (box #f)) (started 0) (running 0) (mu (make-mutex)) (cv (make-condition)))
    (let spawn ((w 0))
      (when (fx<? w writers)
        (with-mutex mu (set! running (fx+ running 1)))
        (fork-thread (lambda ()
                       (hammer! w)
                       (with-mutex mu
                         (set! started (fx+ started 1))
                         (when (fx=? started writers) (condition-broadcast cv)))
                       (let loop ((i (fx+ w 7)))
                         (unless (unbox stop?)
                           (hammer! i)
                           (loop (fx+ i 7))))
                       (with-mutex mu (set! running (fx- running 1)))))
        (spawn (fx+ w 1))))
    (with-mutex mu
      (let wait-start ()
        (unless (fx=? started writers)
          (condition-wait cv mu)
          (wait-start))))
    (let ((result (body)))
      (set-box! stop? #t)
      (let ((deadline (+ (mono-secs) 30)))
        (let wait ()
          (when (and (fx>? running 0) (< (mono-secs) deadline))
            (sleep (make-time 'time-duration 10000000 0))
            (wait))))
      result)))

;; --- 2. the primitive under neighbour stores ---------------------------------
(printf "\n== 2. sa-record-cas! with 8 threads storing into the next field ==\n")
(define n2 500000)
(define rounds 6)
(define cells (let ((v (make-vector n2)))
                (let fill ((i 0)) (when (fx<? i n2) (vector-set! v i (make-cell 0 0)) (fill (fx+ i 1))))
                v))
(define spurious2
  (with-writers
    (lambda (i) (cell-w-set! (vector-ref cells (fxmod i n2)) i))
    (lambda ()
      (let round ((r 0) (bad 0))
        (if (fx<? r rounds)
            (round (fx+ r 1)
                   (let loop ((i 0) (bad bad))
                     (if (fx<? i n2)
                         (loop (fx+ i 1)
                               (if (sa-record-cas! (vector-ref cells i) 0 r (fx+ r 1)) bad (fx+ bad 1)))
                         bad)))
            bad)))))
(printf "  ~a attempts, ~a spurious failure(s)\n" (* n2 rounds) spurious2)
(ok "2. sa-record-cas! never refused a field that held the expected value" (= spurious2 0))
(ok "2. every field reached the final round" 
    (let check ((i 0)) (or (fx=? i n2) (and (fx=? (cell-v (vector-ref cells i)) rounds) (check (fx+ i 1))))))

;; --- 3. compare-and-set! with neighbouring atoms under reset! ------------------
;; The adapter's shape: one thread's atoms allocated together, a claim CASed on
;; one while another thread reset!s the one beside it.
(printf "\n== 3. compare-and-set! with 8 threads reset!-ing the neighbouring atom ==\n")
(define n3 500000)
(define pairs (let ((v (make-vector n3)))
                (let fill ((i 0))
                  (when (fx<? i n3)
                    (vector-set! v i (cons (jolt-atom-new 0) (jolt-atom-new 0)))
                    (fill (fx+ i 1))))
                v))
(define spurious3
  (with-writers
    (lambda (i) (jolt-reset! (cdr (vector-ref pairs (fxmod i n3))) i))
    (lambda ()
      (let round ((r 0) (bad 0))
        (if (fx<? r rounds)
            (round (fx+ r 1)
                   (let loop ((i 0) (bad bad))
                     (if (fx<? i n3)
                         (loop (fx+ i 1)
                               (if (jolt-compare-and-set! (car (vector-ref pairs i)) r (fx+ r 1)) bad (fx+ bad 1)))
                         bad)))
            bad)))))
(printf "  ~a attempts, ~a spurious failure(s)\n" (* n3 rounds) spurious3)
(ok "3. compare-and-set! never refused a value that was the expected one" (= spurious3 0))
(ok "3. every atom reached the final round"
    (let check ((i 0)) (or (fx=? i n3) (and (eqv? (jolt-deref (car (vector-ref pairs i))) rounds) (check (fx+ i 1))))))

;; --- 4. sa-box-cas! with neighbouring boxes under set-box! ------------------
;; box-cas! is the same one-shot ldxr/stxr. CompletableFuture's either-stages
;; claim the winning side with it, so a spurious #f there dropped a completion.
(printf "\n== 4. sa-box-cas! with 8 threads set-box!-ing the neighbouring box ==\n")
(define b0 (box 'a))
(ok "4. sa-box-cas! swaps when the box is eq? to old" (eq? #t (sa-box-cas! b0 'a 'x)))
(ok "4. ...and refuses when it is not" (eq? #f (sa-box-cas! b0 'a 'y)))
(ok "4. ...and the box stayed" (eq? (unbox b0) 'x))
(define n4 500000)
(define bpairs (let ((v (make-vector n4)))
                 (let fill ((i 0))
                   (when (fx<? i n4)
                     (vector-set! v i (cons (box 0) (box 0)))
                     (fill (fx+ i 1))))
                 v))
(define spurious4
  (with-writers
    (lambda (i) (set-box! (cdr (vector-ref bpairs (fxmod i n4))) i))
    (lambda ()
      (let round ((r 0) (bad 0))
        (if (fx<? r rounds)
            (round (fx+ r 1)
                   (let loop ((i 0) (bad bad))
                     (if (fx<? i n4)
                         (loop (fx+ i 1)
                               (if (sa-box-cas! (car (vector-ref bpairs i)) r (fx+ r 1)) bad (fx+ bad 1)))
                         bad)))
            bad)))))
(printf "  ~a attempts, ~a spurious failure(s)\n" (* n4 rounds) spurious4)
(ok "4. sa-box-cas! never refused a box that held the expected value" (= spurious4 0))

;; --- 5. static-deferred-force under neighbour box contention -----------------
;; PersistentTreeMap/EMPTY and PersistentTreeSet/EMPTY publish lazily through
;; static-deferred-force. With raw box-cas!, a spurious arm64 failure left the
;; cell unset and returned #f instead of the collection.
(printf "\n== 5. static-deferred-force with 8 threads set-box!-ing the neighbouring box ==\n")
(let* ((runs 0)
       (obj (list 'singleton))
       (d (make-static-deferred (lambda () (set! runs (fx+ runs 1)) obj))))
  (ok "5. static-deferred-force returns the thunk's value" (eq? obj (static-deferred-force d)))
  (ok "5. ...caches the same object without re-running the thunk"
      (and (eq? obj (static-deferred-force d)) (fx=? runs 1))))
(let* ((runs 0)
       (d (make-static-deferred (lambda () (set! runs (fx+ runs 1)) #f))))
  (ok "5. static-deferred-force caches a falsy value (#f)"
      (and (eq? #f (static-deferred-force d))
           (eq? #f (static-deferred-force d))
           (fx=? runs 1))))
;; Synchronized two-thread race: both threads enter the same thunk, thread 1
;; publishes winner-val first, then thread 2 returns loser-val; both must get
;; winner-val (including when winner-val is #f).
(define (test-deferred-race winner-val loser-val)
  (let* ((mu (make-mutex))
         (cv (make-condition))
         (entered 0)
         (published? #f)
         (loser-res 'unset)
         (loser-done? #f)
         (d (make-static-deferred
              (lambda ()
                (let ((id (with-mutex mu
                            (set! entered (fx+ entered 1))
                            (condition-broadcast cv)
                            entered)))
                  (if (fx=? id 1)
                      (begin
                        (with-mutex mu
                          (let wait-both ()
                            (unless (fx>=? entered 2)
                              (condition-wait cv mu)
                              (wait-both))))
                        winner-val)
                      (begin
                        (with-mutex mu
                          (let wait-pub ()
                            (unless published?
                              (condition-wait cv mu)
                              (wait-pub))))
                        loser-val)))))))
    (fork-thread
      (lambda ()
        (let ((r (static-deferred-force d)))
          (with-mutex mu
            (set! published? #t)
            (set! loser-res r)
            (set! loser-done? #t)
            (condition-broadcast cv)))))
    (let ((w-res (static-deferred-force d)))
      (with-mutex mu
        (set! published? #t)
        (condition-broadcast cv)
        (let wait-done ()
          (unless loser-done?
            (condition-wait cv mu)
            (wait-done))))
      (and (eq? w-res winner-val)
           (eq? loser-res winner-val)
           (eq? (static-deferred-force d) winner-val)))))
(ok "5. racing threads both observe the winning object"
    (test-deferred-race (list 'winner) (list 'loser)))
(ok "5. racing threads both observe a falsy (#f) winner"
    (test-deferred-race #f (list 'loser)))
;; Allocate each deferred's cell box right beside its hammered neighbour box so
;; they share a cache line, and cycle through a compact pool across rounds.
(define n5 4096)
(define rounds5 150)
(define dpairs (let ((v (make-vector n5)))
                 (let fill ((i 0))
                   (when (fx<? i n5)
                     (let* ((target (box static-deferred-unset))
                            (neighbor (box 0))
                            (d (make-static-deferred* (lambda () i) target)))
                       (vector-set! v i (cons d neighbor)))
                     (fill (fx+ i 1))))
                 v))
(define bad5
  (with-writers
    (lambda (i) (set-box! (cdr (vector-ref dpairs (fxmod i n5))) i))
    (lambda ()
      (let round ((r 0) (bad 0))
        (if (fx<? r rounds5)
            (round (fx+ r 1)
                   (let loop ((i 0) (bad bad))
                     (if (fx<? i n5)
                         (let* ((d (car (vector-ref dpairs i)))
                                (_ (set-box! (static-deferred-cell d) static-deferred-unset))
                                (v (static-deferred-force d)))
                           (loop (fx+ i 1) (if (eqv? v i) bad (fx+ bad 1))))
                         bad)))
            bad)))))
(printf "  ~a forces, ~a bad result(s)\n" (* n5 rounds5) bad5)
(ok "5. static-deferred-force never returned unset/#f under neighbour writes" (= bad5 0))

(printf "\ncas-test: ~a checks, ~a failure(s)\n" total fails)
(if (= fails 0)
    (begin (printf "cas-test: PASS — compare-and-swap is strong\n") (exit 0))
    (exit 1))
