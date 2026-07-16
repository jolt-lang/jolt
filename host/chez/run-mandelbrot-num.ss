;; Gate: verify that the hint-free mandelbrot count-point hot loop emits
;; fl+/fl-/fl*/fl>? with no jolt-n* in double arithmetic, proving the
;; WP-E numeric inference loop-var fixpoint + dbl-arith? :num/:long
;; contagion are working end-to-end.

(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze          (var-deref "jolt.analyzer" "analyze"))
(define run-passes       (var-deref "jolt.passes" "run-passes"))
(define wp-infer!        (var-deref "jolt.passes.types" "wp-infer!"))
(define param-num-seeds  (var-deref "jolt.passes.types" "param-num-seeds-for"))
(define emit             (var-deref "jolt.backend-scheme" "emit"))
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))

(set-optimize! #t)

(define count-point (anode
  "(defn count-point [cr ci cap]
     (loop [i 0 zr 0.0 zi 0.0]
       (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
         i
         (recur (inc i)
                (+ (- (* zr zr) (* zi zi)) cr)
                (+ (* 2.0 (* zr zi)) ci)))))"))

(define run-def (anode
  "(defn run [n]
     (let [cap 200 nd (* 1.0 n)]
       (loop [y 0 acc 0]
         (if (< y n)
           (let [ci (- (/ (* 2.0 y) nd) 1.0)
                 row (loop [x 0 a 0]
                       (if (< x n)
                         (let [cr (- (/ (* 2.0 x) nd) 1.5)]
                           (recur (inc x) (+ a (count-point cr ci cap))))
                         a))]
             (recur (inc y) (+ acc row)))
           acc))))"))

;; Run WP whole-program fixpoint on both defs together
(wp-infer! (jolt-vector count-point run-def))

;; count-point's params must be :double from caller analysis
(let ((seeds (param-num-seeds "user/count-point")))
  (if (not (jolt-truthy? seeds))
    (begin (printf "FAIL: param-num-seeds not found for count-point~%") (exit 1)))
  (if (not (eq? (jolt-get seeds "cr") (keyword #f "double")))
    (begin (printf "FAIL: cr should be :double, got ~s~%" (jolt-get seeds "cr")) (exit 1)))
  (if (not (eq? (jolt-get seeds "ci") (keyword #f "double")))
    (begin (printf "FAIL: ci should be :double, got ~s~%" (jolt-get seeds "ci")) (exit 1)))
  (printf "OK: param seeds exist, cr and ci are :double~%"))

;; Process through the full pass pipeline and emit
(define ctx (make-analyze-ctx "user"))
(define processed (run-passes count-point ctx))
(define emitted (emit processed))

;; Count occurrences in the emitted string
(define (count-substr s pat)
  (let ((n (string-length s)) (m (string-length pat)))
    (letrec ((go (lambda (i cnt)
                   (cond
                     ((> (+ i m) n) cnt)
                     ((string=? (substring s i (+ i m)) pat) (go (+ i 1) (+ cnt 1)))
                     (else (go (+ i 1) cnt))))))
      (go 0 0))))

(define fl+ (count-substr emitted "fl+"))
(define fl- (count-substr emitted "fl-"))
(define fl* (count-substr emitted "fl*"))
(define fl> (count-substr emitted "fl>"))
(define fl-total (+ fl+ fl- fl* fl>))

;; jolt-n* in the double arithmetic paths must be 0. The counter comparison
;; (>= i cap) uses jolt-n>= which is expected and excluded.
(define jn-got (+ (count-substr emitted "jolt-n*")
                  (count-substr emitted "jolt-n+")
                  (count-substr emitted "jolt-n-")
                  (count-substr emitted "jolt-n<")))

(if (not (> fl-total 0))
  (begin (printf "FAIL: must have fl-ops, got ~s~%" fl-total) (exit 1)))
(if (not (= jn-got 0))
  (begin (printf "FAIL: ~s jolt-n* in double arith~%" jn-got) (exit 1)))

(printf "  fl-ops in count-point: ~s (fl+: ~s, fl-: ~s, fl*: ~s, fl>: ~s)~%"
        fl-total fl+ fl- fl* fl>)
(printf "  jolt-n* in double arith: ~s~%" jn-got)
