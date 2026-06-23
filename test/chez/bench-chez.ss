;; bench-chez.ss — perf probe for the Chez compute path. Loads the runtime ONCE,
;; then times compile+run of each program (min of N) over the
;; analyze->Scheme->Chez eval pipeline. Run:
;;   chez --script test/chez/bench-chez.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define runs 5)
(define benches
  (list
   (cons "fib"       "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 28)")
   (cons "seq-pipe"  "(loop [i 0 a 0] (if (< i 300) (recur (inc i) (+ a (reduce + 0 (map inc (filter even? (range 200)))))) a))")
   (cons "reduce"    "(loop [i 0 a 0] (if (< i 2000) (recur (inc i) (+ a (reduce + 0 (range 500)))) a))")
   (cons "into-vec"  "(loop [i 0 a 0] (if (< i 1000) (recur (inc i) (+ a (count (into [] (map inc (range 100)))))) a))")
   (cons "map-build" "(loop [i 0 a 0] (if (< i 2000) (recur (inc i) (+ a (count (reduce (fn [m k] (assoc m k k)) {} (range 50))))) a))")
   (cons "map-read"  "(let [m (zipmap (range 100) (range 100))] (loop [i 0 a 0] (if (< i 5000) (recur (inc i) (+ a (get m (mod i 100) 0))) a)))")
   (cons "str-join"  "(loop [i 0 a 0] (if (< i 1000) (recur (inc i) (+ a (count (apply str (map str (range 100)))))) a))")
   (cons "hof"       "(loop [i 0 a 0] (if (< i 2000) (recur (inc i) (+ a (reduce + 0 (map (comp inc inc) (range 200))))) a))")))

(define (now-ms)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1000000.0))))

(for-each
 (lambda (b)
   (let ((name (car b)) (src (string-append "(do " (cdr b) ")")))
     (let loop ((i 0) (best +inf.0))
       (if (>= i runs)
           (printf "~a\t~a ms\n" name (/ (round (* 100 best)) 100.0))
           (let ((t0 (now-ms)))
             (jolt-compile-eval src "user")
             (loop (+ i 1) (min best (- (now-ms) t0))))))))
 benches)
