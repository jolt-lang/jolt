;; trace-compile-cost-test.ss — what tracing adds to the time Chez spends compiling
;; jolt code, as a ratio measured inside ONE process (#1645).
;;
;; Tracing (on by default for runtime-eval'd code and for `jolt build`) emits a
;; little code at every call site: a tail site stores its site, and a non-tail
;; site puts the site slot back when it returns (backend_scheme calls.clj
;; site-restore). There are a LOT of non-tail sites -- ~20k in honeysql and its
;; tests -- so the size of that code is multiplied into every compile. 0.8.23's
;; restore saved and restored two slots at each one, and Chez compiled traced code
;; ~40% slower than untraced: the HoneySQL suite went from 10-12s to 15-17s on a
;; cold cache, every answer still right, every gate still green. Nothing timed a
;; compile, so nothing caught it.
;;
;; This loads real code (stdlib pprint and test, and babashka.cli) twice, traced
;; and untraced, capturing the Scheme each form compiles to -- the text the AOT
;; cache stores (compile-eval.ss jolt-aot-capture) -- and then compiles both
;; captures with Chez, alternating, best of several rounds per side. It asserts
;; the RATIO, so it judges the shape of the emitted code and not the machine: no
;; millisecond figure appears anywhere. Measured on this corpus (x86_64 Linux,
;; best of three, stable to +-0.02 across runs): 1.24 at the fix, 1.57 with the
;; two-slot restore, 1.34 with 0.8.22's one-slot save and restore at each site.
;; The ceiling sits between the fix and the regression, with room for another
;; machine's code generator to land the ratio somewhere else.
;;
;;   JOLT_AOT_CACHE=0 chez --script test/chez/trace-compile-cost-test.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/loader.ss")

(define set-trace-frames! (var-deref "jolt.backend-scheme" "set-trace-frames!"))
;; babashka.cli requires its own internal namespace off the source roots, and
;; clojure.test requires clojure.stacktrace, a stdlib sibling the files below
;; are loaded beside by path rather than through a root
(set-source-roots! (append (get-source-roots) '("stdlib" "vendor/cli/src")))

(define corpus
  '("stdlib/clojure/pprint.clj"
    "stdlib/clojure/test.clj"
    "vendor/cli/src/babashka/cli.cljc"))

(define ceiling 1.40)
(define rounds 3)

;; The Scheme text loading PATH compiles, with tracing on or off.
(define (capture path trace?)
  (set-trace-frames! trace?)
  (let-values (((port get) (open-string-output-port)))
    (parameterize ((jolt-aot-capture port) (jolt-aot-capture-file path))
      (load-jolt-file/restoring-ns path))
    (get)))

(define (read-all text)
  (let ((p (open-input-string text)))
    (let loop ((acc '()))
      (let ((x (read p)))
        (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))

(define (captured trace?)
  (apply append (map (lambda (p) (read-all (capture p trace?))) corpus)))

(define untraced (captured #f))
(define traced (captured #t))
(set-trace-frames! #t)

(gate-check "the corpus compiled to something" (> (length untraced) 100) #t)
(gate-check "...the same forms both ways" (= (length untraced) (length traced)) #t)

;; Compile only: each form is wrapped in a lambda, so nothing it does runs. The
;; optimize level and the (absent) inspector information are a runtime eval's.
(define (compile-ms forms)
  (collect (collect-maximum-generation))
  (let ((t0 (cpu-time)))
    (parameterize ((optimize-level 2)
                   (generate-inspector-information #f)
                   (generate-procedure-source-information #f))
      (for-each (lambda (f) (compile `(lambda () ,f))) forms))
    (- (cpu-time) t0)))

;; a discarded warm-up, then alternate so any drift lands on both sides; noise
;; only ever adds time, so the best run of each is the measurement
(compile-ms untraced)
(compile-ms traced)
(define-values (best-untraced best-traced)
  (let loop ((i 0) (u #f) (t #f))
    (if (= i rounds)
        (values u t)
        (let* ((u1 (compile-ms untraced)) (t1 (compile-ms traced)))
          (loop (+ i 1) (if u (min u u1) u1) (if t (min t t1) t1))))))

(define ratio (/ (inexact best-traced) (max 1 best-untraced)))
(printf "trace compile cost: ~a forms, untraced ~ams, traced ~ams, ratio ~,2f (ceiling ~,2f)\n"
        (length traced) best-untraced best-traced ratio ceiling)
(gate-check (format "traced code compiles within ~,2fx of untraced" ceiling)
            (<= ratio ceiling) #t)

(gate-summary "trace-compile-cost")
