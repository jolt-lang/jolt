;; string-builder-perf.ss — StringBuilder.append must not be quadratic.
;; Run: chez --script test/chez/string-builder-perf.ss (or `make sbperf`).
;;
;; `.append` was (string-append (sb-str self) piece): it copied the entire
;; accumulated buffer on every call, so building an n-char string cost O(n^2).
;; That is not a micro-optimisation. clojure.data.json reads a quoted string one
;; character at a time into a StringBuilder, so in veriframe a single 88KB JSON
;; string value took 623ms to parse while the same 88KB spread over many short
;; values took 30ms — a 20x gap that is pure append copying. Every accumulator
;; built this way paid it.
;;
;; The assertion is a SCALING RATIO, not a wall-clock floor. Quadrupling the
;; append count costs ~4x when append is amortised O(1) and ~16x when it is not,
;; and that ratio survives a loaded machine where an absolute threshold would be
;; flaky. The bar is 8x: well above linear-with-noise, well below quadratic.
;; Timing stays out of the default gate for the reason printperf documents.
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

;; This thread's CPU time, not the wall clock: a preempted sample reads as a regression.
(define (now-ms)
  (let ((t (current-time 'time-thread)))
    (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1000000.0))))

(define runs 3)
(define (best-ms expr)
  (let loop ((i 0) (best +inf.0))
    (if (fx>= i runs)
        best
        (let ((t0 (now-ms)))
          (jolt-compile-eval expr "user")
          (loop (fx+ i 1) (min best (- (now-ms) t0)))))))

(define (build-expr n)
  (string-append "(let [sb (StringBuilder.)]"
                 "  (dotimes [_ " (number->string n) "] (.append sb \"abcdefgh\"))"
                 "  (.length (.toString sb)))"))

(define failed? #f)
(define (fail! msg) (set! failed? #t) (printf "  FAIL: ~a\n" msg))

;; Correctness first: a buffer that appends nothing would time beautifully.
;; Each check evaluates to a string, so the comparison is plain Scheme.
(define (check! label expr want)
  (let ((got (jolt-compile-eval expr "user")))
    (unless (equal? got want)
      (fail! (format "~a: expected ~s, got ~s" label want got)))))

(check! "append returns the buffer and renders its content"
        "(.toString (-> (StringBuilder.) (.append \"a\") (.append \\b) (.append 1)))"
        "ab1")
(check! "5000 appends build the whole string"
        "(str (.length (.toString (let [sb (StringBuilder.)] (dotimes [_ 5000] (.append sb \"xy\")) sb))))"
        "10000")
;; A read part-way through must see the pending appends, not only what was
;; already materialised: the failure mode a lazy buffer introduces is .length
;; or .charAt answering from a stale cache.
(check! "a read mid-build sees the pending appends"
        "(pr-str (let [sb (StringBuilder.)] (.append sb \"abc\") [(.length sb) (.charAt sb 1) (do (.append sb \"de\") [(.length sb) (str sb)])]))"
        "[3 \\b [5 \"abcde\"]]")
;; Mutators have to flush pending appends before they index into the buffer.
(check! "setLength/deleteCharAt/insert see pending appends"
        "(pr-str [(str (doto (StringBuilder.) (.append \"abcd\") (.setLength 2))) (str (doto (StringBuilder.) (.append \"abcdef\") (.deleteCharAt 2))) (str (doto (StringBuilder.) (.append \"abcdef\") (.insert 3 \"XY\"))) (str (doto (StringBuilder.) (.append \"abcdef\") (.reverse)))])"
        "[\"ab\" \"abdef\" \"abcXYdef\" \"fedcba\"]")

;; StringWriter shares the accumulator, and a PrintWriter over either one writes
;; through it — which is the path printStackTrace and print-to-a-writer take.
(check! "StringWriter accumulates"
        "(let [w (StringWriter.)] (.write w \"a\") (.append w \\b) (.toString w))"
        "ab")
(check! "PrintWriter writes through to both accumulators"
        "(pr-str [(let [w (StringWriter.)] (.print (PrintWriter. w) \"xy\") (str w)) (let [b (StringBuilder.)] (.print (PrintWriter. b) \"zw\") (str b))])"
        "[\"xy\" \"zw\"]")

;; A read between appends must not re-join the whole buffer: a formatter that
;; appends a token and then looks at the line so far (charAt, length) made that
;; O(line) per token while the buffer was a chunk list joined on read.
(define (interleave-expr n)
  (string-append "(let [sb (StringBuilder.)]"
                 "  (dotimes [i " (number->string n) "] (.append sb \"abcdefgh\") (.charAt sb i))"
                 "  (.length sb))"))
(jolt-compile-eval (interleave-expr 2000) "user")
(let* ((small (best-ms (interleave-expr 8000)))
       (large (best-ms (interleave-expr 32000)))
       (ratio (/ large (max small 0.001))))
  (printf "interleaved charAt: 4x the appends cost ~ax the time\n" (/ (round (* 10 ratio)) 10.0))
  (when (> ratio 8.0) (fail! "a read between appends is superlinear")))

;; Warm up so the first measurement is not paying one-time costs.
(jolt-compile-eval (build-expr 2000) "user")

(let* ((small (best-ms (build-expr 8000)))
       (large (best-ms (build-expr 32000)))
       (ratio (/ large (max small 0.001))))
  (printf " 8000 appends: ~a ms\n" (/ (round (* 100 small)) 100.0))
  (printf "32000 appends: ~a ms\n" (/ (round (* 100 large)) 100.0))
  (printf "4x the appends cost ~ax the time (linear ~~4x, quadratic ~~16x)\n"
          (/ (round (* 10 ratio)) 10.0))
  (when (> ratio 8.0) (fail! "append is superlinear")))

;; Building costs a copy of each char, not three. Chez strings are four bytes a
;; char, and a buffer that copied each piece in, grew by copying and copied out at
;; toString took 2.4x a join of the same pieces (100 40-char pieces, built 2000
;; times: 140ms against 58ms). The builder joins its pending pieces once at
;; toString. Measured against clojure.string/join over the same pieces (whose seq
;; walk and per-element render are most of its cost), a build reads ~0.49x and the
;; copying buffer read ~0.92x, so the bar sits between them. A ratio in one
;; process, so a loaded machine moves both sides.
(define medium-piece "\"0123456789012345678901234567890123456789\"")
(define (medium-build-expr n)
  (string-append "(dotimes [_ " (number->string n) "]"
                 "  (let [sb (StringBuilder.)]"
                 "    (dotimes [_ 100] (.append sb " medium-piece "))"
                 "    (.toString sb)))"))
(define (medium-join-expr n)
  (string-append "(let [ps (vec (repeat 100 " medium-piece "))]"
                 "  (dotimes [_ " (number->string n) "] (clojure.string/join ps)))"))
(jolt-compile-eval "(require 'clojure.string)" "user")
(jolt-compile-eval (medium-build-expr 200) "user")
(jolt-compile-eval (medium-join-expr 200) "user")
(let* ((build (best-ms (medium-build-expr 2000)))
       (join (best-ms (medium-join-expr 2000)))
       (ratio (/ build (max join 0.001))))
  (printf "medium pieces: a build costs ~ax a join of the same pieces\n"
          (/ (round (* 100 ratio)) 100.0))
  (when (> ratio 0.7) (fail! "building copies each char more than once")))

(if failed? (exit 1) (begin (printf "PASS\n") (exit 0)))
