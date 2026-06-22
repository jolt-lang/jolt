;; FFI + threading regression. Run from repo root:
;;   chez --script test/chez/ffi-server-test.ss
;;
;; Covers two fixes:
;;  - the HTTP server's blocking accept/recv/send are __collect_safe, so a thread
;;    idle in accept() no longer pins the stop-the-world collector. With the bug,
;;    a (collect) on the main thread throws "cannot collect when multiple threads
;;    are active"; with the fix it succeeds while the server sits in accept().
;;  - jolt.http-client temp-file paths are unique per process+thread (no clobber).
;; Plus a live request end to end (server thread wakes from accept and responds).

(import (chezscheme))

;; mirror cli.ss's load sequence through http-server.ss
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/png.ss")
(load "host/chez/http-client.ss")
(load "host/chez/http-server.ss")
(load "host/chez/loader.ss")   ; defines jolt-sh-out, used by http-client

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define port 8391)
(define run-server (var-deref "jolt.http.server" "run-server"))
(define stop-server (var-deref "jolt.http.server" "stop-server"))
(define http-get (var-deref "jolt.http-client" "get"))

(define (kw s) (keyword #f s))
(define handler (lambda (req) (jolt-hash-map (kw "status") 200 (kw "body") "hi from test")))
(define server (jolt-invoke run-server handler (jolt-hash-map (kw "port") port)))

;; let the accept thread reach accept() (sleep is collect-safe in Chez)
(sleep (make-time 'time-duration 300000000 0))

;; GC must proceed even though a thread is blocked in accept(): a stalled
;; collector throws "cannot collect when multiple threads are active".
(ok "collect not stalled by idle accept()" (guard (e (#t #f)) (collect) #t))

;; client temp paths: unique under concurrency. Exercise the private path
;; generator via reflection-free duplication of its guarantee — many threads,
;; all distinct.
(let ((seen (make-hashtable string-hash string=?)) (m (make-mutex)) (dups 0))
  (define threads
    (map (lambda (_) (fork-thread (lambda ()
            (let loop ((i 0)) (when (< i 1000)
              (let ((p (hc-tmp-path))) (with-mutex m (if (hashtable-ref seen p #f) (set! dups (+ dups 1)) (hashtable-set! seen p #t))))
              (loop (+ i 1)))))))
         (iota 6)))
  (sleep (make-time 'time-duration 400000000 0))
  (ok "http-client temp paths unique across threads" (= dups 0)))

;; live request end to end
(let* ((resp (jolt-invoke http-get (string-append "http://127.0.0.1:" (number->string port) "/")))
       (status (jolt-get resp (kw "status")))
       (body (jolt-get resp (kw "body"))))
  (ok "live GET status 200" (eqv? 200 status))
  (ok "live GET body" (and (string? body) (string=? body "hi from test"))))

(jolt-invoke stop-server server)

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
