;; run-devirt.ss — protocol-call devirtualization gate (backend_scheme emit).
;;
;; The inference annotates a monomorphic protocol call with :devirt-type/-proto/
;; -method (jolt.passes.types); the back end then resolves the impl by that static
;; tag. This gate pins both halves: the emitted form uses find-protocol-method, and
;; evaluating it returns the same value the ordinary dispatch would — for a record's
;; inline impl, an extend-type impl, and across distinct receiver types.
;;
;;   chez --script host/chez/run-devirt.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define emit    (var-deref "jolt.backend-scheme" "emit"))
(define kw      (lambda (n) (keyword #f n)))

(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
;; define two record types implementing one protocol — Circle via an inline impl,
;; Square via extend-type — plus instances to dispatch on.
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [r] Shape (area [s] (:r s)))")
(evals "(defrecord Square [w])")
(evals "(extend-type Square Shape (area [s] (* (:w s) (:w s))))")
(evals "(def c (->Circle 7))")
(evals "(def sq (->Square 5))")

;; analyze (area RECV), annotate it as a devirt call on `type`, and emit. RECV is a
;; var name (c/sq) the emitted code resolves at eval time.
(define (devirt-emit type recv)
  (let* ((ir (analyze (make-analyze-ctx "user") (jolt-ce-read (string-append "(area " recv ")"))))
         (dv (jolt-assoc ir (kw "devirt-type") type (kw "devirt-proto") "Shape"
                         (kw "devirt-method") "area")))
    (emit dv)))

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))
(define (has-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0)) (cond ((> (+ i m) n) #f)
                            ((string=? (substring s i (+ i m)) sub) #t)
                            (else (loop (+ i 1)))))))
;; eval an emitted Scheme string in the loaded runtime (var-deref resolves c/sq).
(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))

(let ((e (devirt-emit "user.Circle" "c")))
  (check "emit uses devirt-resolve" (has-sub? e "devirt-resolve") #t)
  (check "devirt inline impl == dispatch" (run-emit e) (evals "(area c)")))   ; 7

(let ((e (devirt-emit "user.Square" "sq")))
  (check "devirt extend-type impl == dispatch" (run-emit e) (evals "(area sq)")))  ; 25

;; a normal (no devirt) call still goes through dispatch and agrees — the path the
;; megamorphic / unknown-receiver site keeps.
(let ((e (emit (analyze (make-analyze-ctx "user") (jolt-ce-read "(area c)")))))
  (check "non-devirt path no devirt-resolve" (has-sub? e "devirt-resolve") #f)
  (check "non-devirt still dispatches" (run-emit e) 7))

;; a record that relies on the protocol's Object default (no direct impl): the
;; inference still types it as a concrete record and annotates devirt, so the
;; emitted call must resolve the same value dispatch would. find-protocol-method
;; on the record's own tag misses here, so the devirt path has to fall back to
;; ordinary dispatch (else it applies #f and crashes).
(evals "(extend-protocol Shape Object (area [s] :obj-default))")
(evals "(defrecord Plain [n])")
(evals "(def pl (->Plain 9))")
(let ((e (devirt-emit "user.Plain" "pl")))
  (check "devirt Object-default == dispatch" (run-emit e) (evals "(area pl)")))  ; :obj-default

(if (= fails 0)
    (begin (printf "devirt gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "devirt gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
