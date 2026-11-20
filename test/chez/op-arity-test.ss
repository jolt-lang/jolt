;; Every numeric fast-path op, at every arity it admits — DERIVED from op-registry
;; rather than hand-listed, so a specialization added later is covered the moment
;; it lands. Run:
;;   chez --script test/chez/op-arity-test.ss
;;
;; What this pins. op-registry names one Scheme proc per numeric kind (:dbl/:lng/
;; :bd) and emit-numeric splices the call's operands into it. That is sound only if
;; the named proc accepts the operand count the emitter will hand it, and the
;; registry has no field that says whether it does — :arity gates which SOURCE
;; arities get lowered, not what the target proc takes. The assumption held for
;; years by luck of what was picked (fx+/fl+ are variadic Chez primitives, jbd-add
;; is variadic by construction), so when 0.5.2 moved ^long +/-/* onto the binary
;; jolt-l+ every 3-operand long form stopped compiling and nothing noticed: every
;; probe that "covered" long arithmetic was written at arity 2.
;;
;; So each case asserts two things, and the second is the one that matters:
;;   1. the specialized form compiles and agrees with the generic value-position
;;      path (apply, which goes through the variadic overlay);
;;   2. the emitted code actually CONTAINS the specialization. Without (2) a case
;;      passes vacuously when the form silently fails to specialize — the precise
;;      way the gap stayed invisible.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (has? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

(define (emitf ns str)
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx ns)))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))

;; Both the expander (a binary macro handed 3 operands is a SYNTAX error, raised
;; while compiling) and the runtime (a binary proc handed 3 is an arity error) are
;; caught here, since the two halves of this bug class surface at different times.
(define (render-condition e)
  (if (condition? e)
      (let ((p (open-output-string))) (display-condition e p) (get-output-string p))
      (format "~s" e)))
(define (attempt thunk)
  (call/cc (lambda (k)
    (with-exception-handler
      (lambda (e) (k (cons 'error (render-condition e))))
      thunk))))
(define (try-ev s) (attempt (lambda () (jolt-compile-eval s "u"))))
(define (try-emit s) (attempt (lambda () (emitf "u" s))))
(define (err? r) (and (pair? r) (eq? (car r) 'error)))
(define (err-msg r) (cdr r))

;; --- operand values -------------------------------------------------------
;; Powers of two, strictly decreasing, none zero: * cannot overflow the long path
;; into a throw the generic path wouldn't take, bigdec / terminates (8M/4M is exact
;; where 8M/3M is not), quot/rem/mod have no zero divisor, and the strict order
;; gives the comparisons a definite answer at every arity.
(define (kind-vals kind)
  (cond ((string=? kind "lng") '("8" "4" "2" "1"))
        ((string=? kind "dbl") '("8.0" "4.0" "2.0" "1.0"))
        (else                  '("8M" "4M" "2M" "1M"))))

;; NB: this script loads the runtime into the interaction environment, so a
;; top-level define here SILENTLY OVERRIDES a runtime procedure of the same name.
;; first-n was named take-n until it shadowed natives-seq.ss's (take-n n s) — same
;; name, reversed args, so seq-ing a param vector during analysis raised "is not a
;; number" from a comparison in the wrong callee. Check names against host/chez/.
(define (first-n xs n) (if (= n 0) '() (cons (car xs) (first-n (cdr xs) (- n 1)))))
(define (sep-join sep xs)
  (if (null? xs) "" (fold-left (lambda (a x) (string-append a sep x)) (car xs) (cdr xs))))

;; How a case names its op. A private native (__unchecked-add-int, the
;; *unchecked-math* twin the analyzer rewrites to) has no public var, so a bare
;; reference from this namespace is refused; it is written qualified, as the
;; rewrite and a syntax-quoted expansion reach it.
(define (op-ref op)
  (if (and (> (string-length op) 2) (string=? (substring op 0 2) "__"))
      (string-append "clojure.core/" op)
      op))

;; The specialized form. :lng/:dbl are seeded by a param hint; :bigdec is seeded by
;; the LITERAL (there is no ^bigdec hint), so its operands sit in the body — which
;; is also why it takes no arguments.
(define (specialized-src op kind n)
  (let ((vs (first-n (kind-vals kind) n)))
    (if (string=? kind "bd")
        (string-append "((fn* ([] (" (op-ref op) " " (sep-join " " vs) "))))")
        (let* ((hint (if (string=? kind "lng") "^long " "^double "))
               (ps   (map (lambda (i) (string-append "p" (number->string i))) (iota n)))
               (params (sep-join " " (map (lambda (p) (string-append hint p)) ps))))
          (string-append "((fn* ([" params "] (" (op-ref op) " " (sep-join " " ps) "))) " (sep-join " " vs) ")")))))

;; The reference: the same operands through apply, which reaches the variadic
;; value-position proc instead of any inline specialization.
(define (generic-src op kind n)
  (string-append "(apply " (op-ref op) " [" (sep-join " " (first-n (kind-vals kind) n)) "])"))

;; --- the matrix, straight out of the registry -----------------------------
(define cases
  (jolt-compile-eval
   "(vec (for [[op spec] jolt.op-registry/op-registry
               kind [:dbl :lng :bd]
               :when (get spec kind)
               n [1 2 3 4]
               :when ((get spec :arity (fn [_] true)) n)]
           [op (name kind) n (get spec kind)]))"
   "u"))

;; Arity 1 is exempt from the emission check: several ops have no work to do with a
;; single operand — (+ x)/(* x)/(min x)/(max x) ARE x, and a lone comparison is
;; vacuously true — so the emitter legitimately drops the call. Arity >= 2 always
;; has a real op to emit.
(define (emission-checked? n) (>= n 2))

(let loop ((i 0))
  (when (< i (pvec-count cases))
    (let* ((row  (pvec-nth-d cases i jolt-nil))
           (op   (pvec-nth-d row 0 jolt-nil))
           (kind (pvec-nth-d row 1 jolt-nil))
           (n    (pvec-nth-d row 2 jolt-nil))
           (proc (pvec-nth-d row 3 jolt-nil))
           (label (string-append op "/" kind "/arity-" (number->string n)))
           (src  (specialized-src op kind n))
           (got  (try-ev src)))
      ;; 1. it compiles and runs at all — the half that broke at 3+ operands
      (if (err? got)
          (ok (string-append label " compiles and runs")
              (begin (printf "  ~a\n    ~a\n" src (err-msg got)) #f))
          (let ((want (try-ev (generic-src op kind n))))
            ;; 2. and agrees with the generic value-position path
            (if (err? want)
                (ok (string-append label " reference evaluates")
                    (begin (printf "  ~a => ~a\n" (generic-src op kind n) (err-msg want)) #f))
                (ok (string-append label " agrees with (apply " op " ...): got "
                                   (jolt-pr-str got) ", want " (jolt-pr-str want))
                    (jolt= got want)))))
      ;; 3. and it really did specialize — without this the case can pass vacuously
      (when (emission-checked? n)
        (let ((e (try-emit src)))
          (ok (string-append label " emits " proc)
              (and (not (err? e)) (has? e proc))))))
    (loop (+ i 1))))

(printf "~a/~a passed  (~a ops x kinds x arities from op-registry)\n"
        (- total fails) total (pvec-count cases))
(when (> fails 0) (exit 1))
