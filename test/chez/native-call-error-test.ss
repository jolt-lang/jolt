;; Exercise both the checked closure path and the top-level pvalue detour,
;; including arguments spilled past the architecture's argument registers.
(import (chezscheme))
(optimize-level 3)
(define hook
  (guard (c (else #f)) (eval '($primitive $call-error-handler))))
(define checks 0)
(define (check label got expected)
  (set! checks (+ checks 1))
  (unless (equal? got expected)
    (error 'native-call-error label got expected)))
(define (failure thunk)
  (guard (c (else (condition-message c))) (thunk) #f))
(define (args n) (let loop ((i 0)) (if (= i n) '() (cons i (loop (+ i 1))))))
(define (caller n)
  (let ((xs (map (lambda (i) (string->symbol (format "a~a" i))) (args n))))
    (eval `(lambda (f ,@xs) (#2%$app f ,@xs)))))
(define wrong-arity (lambda (a b c d e f g h i j k l m n o p q r s t u) #f))
(define other-arities
  (list (cons (lambda () 'actual-call) 0)
        (cons (lambda (a) 'actual-call) 1)
        (cons (lambda (a b c) 'actual-call) 3)))
(define native-target #f)
(check "ordinary procedure call" ((caller 2) + 1 2) 3)
(check "stock nonprocedure failure"
       (failure (lambda () ((caller 1) 'not-callable 1)))
       "attempt to apply non-procedure ~s")
(check "stock arity failure"
       (failure (lambda () ((caller 0) wrong-arity)))
       "incorrect number of arguments ~s to ~s")
(when hook
  (hook
    (lambda (callee . arguments) (cons callee arguments)))
  (do ((n 0 (+ n 1))) ((= n 21))
    (let ((xs (args n)) (call (caller n)))
      (check "checked nonprocedure retains arguments"
             (apply call 'not-callable xs) (cons 'not-callable xs))
      (check "wrong arity retains arguments"
             (apply call wrong-arity xs) (cons wrong-arity xs))
      (for-each
        (lambda (entry)
          (unless (= n (cdr entry))
            (check "wrong arity retains unused argument registers"
                   (apply call (car entry) xs) (cons (car entry) xs))))
        other-arities)
      (let* ((names (map (lambda (i) (string->symbol (format "a~a" i))) xs))
             (topcall (eval `(lambda ,names (native-target ,@names)))))
        (set! native-target 'top-level-value)
        (check "pvalue nonprocedure retains arguments"
               (apply topcall xs) (cons 'top-level-value xs))
        (set! native-target wrong-arity)
        (check "pvalue arity failure retains arguments"
               (apply topcall xs) (cons wrong-arity xs)))))
  (hook
    (lambda (callee . arguments) (apply values arguments)))
  (check "handler returns multiple values to the original continuation"
         (call-with-values (lambda () ((caller 2) 'not-callable 17 23)) list)
         '(17 23))
  (hook #f)
  (check "unregistered hook keeps stock failure"
         (failure (lambda () ((caller 1) 'not-callable 1)))
         "attempt to apply non-procedure ~s"))
(printf "native-call-error: ~a passed (~a)\n" checks
        (if hook 'patched 'stock))
