;; A direct-linked call racing a redefinition of its callee (backend calls.clj
;; linked-call). A linked var's jv$ binding is a Chez top-level variable that a
;; root write assigns (rt.ss var-root-set! -> the def's setter). Chez compiles a
;; call of a top-level variable as "load its value into cp, jump through the code
;; pointer cached in the symbol", and an assignment writes the value and then
;; clears the cache, so a thread calling while another assigns could run one
;; closure's code over the other's free variables. The first call after an
;; assignment also refills the cache from a value it read before the write, which
;; could leave every later call on the old code. Before the fix this gate saw tens
;; of thousands of mismatched calls per run, and sometimes a binding stuck on the
;; wrong code after the writer stopped.
;;
;; Callers apply a direct-linked fn while the main thread swaps its root between
;; two closures with different code and different free variables. Each closure
;; answers (its-code-tag . its-free-variable), so a mismatched pair is a call
;; that ran one closure's code with the other's environment.
;;   chez --script test/chez/linked-call-race-test.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze          (var-deref "jolt.analyzer" "analyze"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit-top-form    (var-deref "jolt.backend-scheme" "emit-top-form"))
(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
((var-deref "jolt.backend-scheme" "set-emit-unit!") ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)

(define (emit-dl src)
  (set-direct-link! #t)
  (let ((e (emit-top-form (numeric-annotate (analyze (make-analyze-ctx "user") (jolt-ce-read src))))))
    (set-direct-link! #f)
    e))
(define (run-dl src)
  (let ((e (emit-dl src)))
    (eval (read (open-input-string e)) (interaction-environment))
    e))

(run-dl "(defn racef [x] [:one x])")
(define caller-src (run-dl "(defn race-call [i] (racef i))"))
(gate-check "the caller applies racef's binding through a local"
            (gate-sub? caller-src " jv$user/racef)) (if (#3%procedure? _lp$") #t)
(gate-check "the caller never applies the top-level variable"
            (gate-sub? caller-src "(jv$user/racef ") #f)

;; Built from arguments, so each closure really carries its free variable: a
;; constant tag would be folded into the code, and a mismatch would not show.
(define (mk-one tag) (lambda (x) (cons 'one tag)))
(define (mk-two tag) (lambda (x) (cons 'two tag)))
(define cell (jolt-var "user" "racef"))
(define race-call (var-deref "user" "race-call"))

(var-root-set! cell (mk-two 'two))
(gate-check "a root write reaches the direct site" (race-call 0) '(two . two))
(var-root-set! cell (mk-one 'one))

(define mismatched 0)
(define stop? #f)
(define mu (make-mutex))
(define done (make-condition))
(define running 0)
(define (caller)
  (let loop ()
    (unless stop?
      (let ((r (race-call 0)))
        (unless (eq? (car r) (cdr r))
          (with-mutex mu (set! mismatched (+ mismatched 1)))))
      (loop)))
  (with-mutex mu
    (set! running (- running 1))
    (condition-signal done)))
(define callers 3)
(set! running callers)
(do ((i 0 (+ i 1))) ((= i callers)) (fork-thread caller))
(do ((i 0 (+ i 1))) ((= i 300000))
  (var-root-set! cell (if (even? i) (mk-two 'two) (mk-one 'one))))
(set! stop? #t)
(with-mutex mu
  (let wait () (unless (= running 0) (condition-wait done mu) (wait))))

(gate-check "no call ran one closure's code over another's free variables" mismatched 0)
(gate-check "after the writer stops, the site calls the current root"
            (race-call 0) '(one . one))
(var-root-set! cell (mk-two 'two))
(gate-check "...and follows the next redefinition" (race-call 0) '(two . two))

(gate-summary "linked-call-race")
