;; run-infer.ss — inference / success-type-checking gate (jolt.passes.types).
;;
;; The corpus and unit gates compile through run-passes' const-fold-only branch,
;; so the inference walk (jolt.passes.types) runs only under `jolt build --opt` —
;; buildsmoke exercises it on one trivial app and asserts stdout only. This gate
;; drives the pass DIRECTLY: analyze a source string to IR, then call the public
;; checker/driver entry points (check-form, infer-body, the set-*! registries) and
;; assert their observable output (diagnostic counts, collected calls/escapes). It
;; pins the behavior the inference walk's internal state produces, so a refactor of
;; that state is gate-validatable.
;;
;;   chez --script host/chez/run-infer.ss
(import (chezscheme))

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze            (var-deref "jolt.analyzer" "analyze"))
(define check-form         (var-deref "jolt.passes.types" "check-form"))
(define infer-body         (var-deref "jolt.passes.types" "infer-body"))
(define reset-escapes!     (var-deref "jolt.passes.types" "reset-escapes!"))
(define collected-escapes  (var-deref "jolt.passes.types" "collected-escapes"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-vtypes!        (var-deref "jolt.passes.types" "set-vtypes!"))

;; analyze a source string to its IR node (fresh ctx, ns "user", no passes).
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
;; number of success-type diagnostics check-form produces for src.
(define (diags src strict?) (jolt-count (check-form (anode src) strict?)))

(define fails 0)
(define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

;; --- core error-domain checking (strict not required) -----------------------
(check "num-op on keyword"        (diags "(+ 1 :k)" #f) 1)
(check "num-op all numbers"       (diags "(+ 1 2)" #f) 0)
(check "count on number"          (diags "(count 5)" #f) 1)
(check "count on vector"          (diags "(count [1 2])" #f) 0)
(check "lenient (:k 5)"           (diags "(:k 5)" #f) 0)
(check "call a number"            (diags "(5 1)" #f) 1)
(check "nested count return type" (diags "(+ 1 (count :k))" #f) 1)

;; --- walk arms thread the type env ------------------------------------------
(check "let binds kw"             (diags "(let [x :k] (+ x 1))" #f) 1)
(check "let binds ok"             (diags "(let [x 1] (+ x 1))" #f) 0)
(check "if then branch error"     (diags "(if true (+ 1 :k) 2)" #f) 1)
(check "do statement error"       (diags "(do (+ 1 :k) 2)" #f) 1)
(check "mapv seeds element type"  (diags "(mapv (fn [x] (+ x 1)) [:a :b])" #f) 1)
(check "mapv ok element type"     (diags "(mapv (fn [x] (+ x 1)) [1 2])" #f) 0)
(check "reduce seeds element"     (diags "(reduce (fn [acc x] (+ acc x)) 0 [:a])" #f) 1)

;; --- strict user-function domains (checking-box / diag-memo / user-sig) ------
(check "user wrong arg type"      (diags "(do (defn f [x] (+ x 1)) (f :k))" #t) 1)
(check "user wrong arity"         (diags "(do (defn g [x] x) (g 1 2))" #t) 1)
(check "user call ok"             (diags "(do (defn h [x] (+ x 1)) (h 3))" #t) 0)
(check "user domains off w/o strict" (diags "(do (defn f [x] (+ x 1)) (f :k))" #f) 0)
;; recursive user fn terminates (cycle guard) and still flags the bad arg
(check "user recursive terminates"
       (diags "(do (defn rf [x] (+ x (rf x))) (rf :k))" #t) 1)

;; --- infer-body collects calls + escapes ------------------------------------
(reset-escapes!)
(let ((r (infer-body (anode "(do (foo 1) (bar 2) (map inc [1]))") (jolt-hash-map))))
  (check "infer-body calls" (jolt-count (jolt-nth r 2)) 3)        ; foo, bar, map
  (check "infer-body escapes" (jolt-count (collected-escapes)) 1)) ; inc (value position)

;; --- the record-shapes registry feeds call-result types --------------------
;; without shapes a (->P …) call result is :any (accepted); with the registry it
;; types as a struct, so an arithmetic op over it is provably not-a-number.
(check "ctor result :any w/o shapes" (diags "(+ (->P 1) 1)" #f) 0)
(set-record-shapes!
  (jolt-hash-map "user/->P"
                 (jolt-hash-map (keyword #f "fields") (jolt-vector (keyword #f "x"))
                                (keyword #f "tags")   (jolt-vector jolt-nil)
                                (keyword #f "type")   "user.P")))
(check "ctor result struct w/ shapes" (diags "(+ (->P 1) 1)" #f) 1)
(set-record-shapes! (jolt-hash-map))

(if (= fails 0)
    (begin (printf "infer gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "infer gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
