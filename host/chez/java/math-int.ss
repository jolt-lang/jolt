;; java.lang.Math's statics past the transcendentals (math.ss): the division,
;; exact and high-product integer methods JDK 9 to 21 added, fma, clamp, and
;; the int overloads whose answer is not the long one's. StrictMath has the
;; same set, so both classes register them. Portable, as math.ss is.

(define (jm-int-exact r)
  (if (and (>= r -2147483648) (<= r 2147483647)) r (throw-jvm 'ArithmeticException "integer overflow")))
(define (jm-quot a b round)                            ; "/ by zero" first, as the JDK
  (if (eqv? b 0) (jolt-l-div0-throw) (exact (round (/ a b)))))
(define (jm-abs-exact a min-value cls)
  (if (= a min-value)
      (throw-jvm 'ArithmeticException (string-append "Overflow to represent absolute value of " cls ".MIN_VALUE"))
      (abs a)))
(define u64 18446744073709551616)

;; fma: the exact a*b+c rounded once. A zero product's sign, and the
;; infinities, are the floating expression's, as the JDK computes them.
(define (jm-fma a b c)
  (cond
    ((or (nan? a) (nan? b) (nan? c)) +nan.0)
    ((or (infinite? a) (infinite? b) (infinite? c))
     (let ((p (fl* a b)))
       (cond ((or (and (infinite? a) (fl= b 0.0)) (and (infinite? b) (fl= a 0.0))) +nan.0)
             ((and (infinite? p) (not (infinite? a)) (not (infinite? b))) c)
             (else (fl+ p c)))))
    ((or (fl= a 0.0) (fl= b 0.0)) (fl+ (fl* a b) c))
    (else (exact->inexact (+ (* (exact a) (exact b)) (exact c))))))
(define (jm-fma-float a b c)
  (if (and (finite? a) (finite? b) (finite? c) (not (fl= a 0.0)) (not (fl= b 0.0)))
      (let ((q (+ (* (exact a) (exact b)) (exact c))))
        (cond ((= q 0) 0.0)
              ((> q 0) (bits->flt (flt-mag->bits q)))
              (else (fl- (bits->flt (flt-mag->bits (- q)))))))
      (jolt-unchecked-float->flonum (jm-fma a b c))))
(define (jolt-math-fma a b c)
  (if (and (jfloat? a) (jfloat? b) (jfloat? c))
      (make-jfloat (jm-fma-float (jfloat-fl a) (jfloat-fl b) (jfloat-fl c)))
      (jm-fma (->dbl a) (->dbl b) (->dbl c))))

;; clamp: the long overloads, and the double and float ones, whose min and max
;; refuse NaN and min > max (Double.compare's, so 0.0 > -0.0) as the JDK words it
(define (jm-clamp-check lo hi show)
  (unless (fl< lo hi)
    (cond ((nan? lo) (throw-jvm 'IllegalArgumentException "min is NaN"))
          ((nan? hi) (throw-jvm 'IllegalArgumentException "max is NaN"))
          ((or (fl> lo hi) (and (fl= lo 0.0) (not (dbl-negative? lo)) (dbl-negative? hi)))
           (throw-jvm 'IllegalArgumentException
                      (string-append (jolt-str-one (show lo)) " > " (jolt-str-one (show hi))))))))
(define (jolt-math-clamp v lo hi)
  (cond
    ((and (math-long-arg? v) (math-long-arg? lo) (math-long-arg? hi))
     (if (> lo hi)
         (throw-jvm 'IllegalArgumentException
                    (string-append (number->string lo) " > " (number->string hi)))
         (min hi (max v lo))))
    ((and (jfloat? v) (jfloat? lo) (jfloat? hi))
     (let ((lo (jfloat-fl lo)) (hi (jfloat-fl hi)))
       (jm-clamp-check lo hi make-jfloat)
       (make-jfloat (jolt-math-min hi (jolt-math-max (jfloat-fl v) lo)))))
    (else
     (let ((v (->dbl v)) (lo (->dbl lo)) (hi (->dbl hi)))
       (jm-clamp-check lo hi (lambda (d) d))
       (jolt-math-min hi (jolt-math-max v lo))))))

;; floorDiv/ceilDiv: a long quotient wraps (Long.MIN_VALUE / -1 is itself), and
;; so does an int one at 32 bits
(define (jm-floor-div a b) (jvm-wrap (jm-quot a b floor) 64))
(define (jm-ceil-div a b) (jvm-wrap (jm-quot a b ceiling) 64))
(define (jm-ceil-mod a b) (- a (* b (jm-quot a b ceiling))))

;; an integer method refuses what is not a long, as the long methods in
;; math.ss do
(define (jm-long-only entry)
  (let ((name (car entry)) (f (cdr entry)))
    (cons name
          (host-arity-like f
            (lambda args
              (if (let all ((as args)) (or (null? as) (and (math-long-arg? (car as)) (all (cdr as)))))
                  (apply f args)
                  (math-no-long-overload name (length args))))))))
(define math-int-statics
  (list
   (cons "floorDiv" jm-floor-div)
   (cons "ceilDiv" jm-ceil-div)
   (cons "ceilMod" jm-ceil-mod)
   (cons "divideExact" (lambda (a b) (jolt-math-long-exact (jm-quot a b truncate))))
   (cons "floorDivExact" (lambda (a b) (jolt-math-long-exact (jm-quot a b floor))))
   (cons "ceilDivExact" (lambda (a b) (jolt-math-long-exact (jm-quot a b ceiling))))
   (cons "absExact" (lambda (a) (jm-abs-exact a -9223372036854775808 "Long")))
   (cons "toIntExact" (lambda (a) (jm-int-exact a)))
   (cons "multiplyFull" (lambda (a b) (* a b)))
   (cons "multiplyHigh" (lambda (a b) (bitwise-arithmetic-shift-right (* a b) 64)))
   (cons "unsignedMultiplyHigh"
         (lambda (a b) (jvm-wrap (bitwise-arithmetic-shift-right (* (modulo a u64) (modulo b u64)) 64) 64)))
   ;; the int overloads (records-interop.ss names them), their arguments ints
   (cons "abs(int)" (lambda (a) (jvm-wrap (abs a) 32)))
   (cons "absExact(int)" (lambda (a) (jm-abs-exact a -2147483648 "Integer")))
   (cons "addExact(int)" (lambda (a b) (jm-int-exact (+ a b))))
   (cons "subtractExact(int)" (lambda (a b) (jm-int-exact (- a b))))
   (cons "multiplyExact(int)" (lambda (a b) (jm-int-exact (* a b))))
   (cons "incrementExact(int)" (lambda (a) (jm-int-exact (+ a 1))))
   (cons "decrementExact(int)" (lambda (a) (jm-int-exact (- a 1))))
   (cons "negateExact(int)" (lambda (a) (jm-int-exact (- a))))
   (cons "divideExact(int)" (lambda (a b) (jm-int-exact (jm-quot a b truncate))))
   (cons "floorDivExact(int)" (lambda (a b) (jm-int-exact (jm-quot a b floor))))
   (cons "ceilDivExact(int)" (lambda (a b) (jm-int-exact (jm-quot a b ceiling))))
   (cons "floorDiv(int)" (lambda (a b) (jvm-wrap (jm-quot a b floor) 32)))
   (cons "ceilDiv(int)" (lambda (a b) (jvm-wrap (jm-quot a b ceiling) 32)))))
(for-each
 (lambda (cls)
   (register-class-statics! cls (map jm-long-only math-int-statics))
   (register-class-statics! cls (list (cons "fma" jolt-math-fma) (cons "clamp" jolt-math-clamp)
                                      (cons "TAU" 6.283185307179586))))
 '("Math" "StrictMath"))
;; The methods with overloads a runtime class chooses among: a call no overload
;; fits at compile time (an untyped argument) takes the Reflector's member
;; (records-interop.ss), on both classes
(for-each
 (lambda (cls)
   (for-each (lambda (m) (register-reflect-member! (string-append "java.lang." cls) cls (car m) (cadr m)))
             '(("abs" 1) ("max" 2) ("min" 2) ("round" 1) ("signum" 1) ("ulp" 1) ("nextUp" 1)
               ("nextDown" 1) ("nextAfter" 2) ("scalb" 2) ("getExponent" 1) ("copySign" 2)
               ("absExact" 1) ("addExact" 2) ("subtractExact" 2) ("multiplyExact" 2)
               ("incrementExact" 1) ("decrementExact" 1) ("negateExact" 1) ("divideExact" 2)
               ("floorDiv" 2) ("floorMod" 2) ("ceilDiv" 2) ("ceilMod" 2) ("floorDivExact" 2)
               ("ceilDivExact" 2) ("clamp" 3) ("fma" 3))))
 '("Math" "StrictMath"))
