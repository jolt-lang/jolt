;; ffi-stack-args-test.ss: foreign-procedure arguments past the argument
;; registers land where the C ABI puts them on arm64 and x86_64
;; (host/chez/chez-patches/0006-arm64-stack-arguments.patch and
;; 0007-x86_64-stack-struct-arguments.patch, both from cisco/ChezScheme#1091).
;;
;; On x86_64, Chez 10.4.1 copied a struct on the stack in 8/4/2/1-byte loads but
;; stored each piece as a full word, so a struct whose last slot takes more than
;; one piece (3, 5, 6, 7, 11, 13, 14 or 15 bytes) overwrote the start of the
;; next argument's slot: the 3-char row's final char arrived as 0.
;;
;; Once the registers are used up, Chez 10.4.1 on arm64 Mac OS packed a struct
;; of 16 bytes or less that isn't a homogeneous floating-point aggregate like a
;; scalar, where Apple's ABI gives it 8-byte-aligned slots, and padded before a
;; stack argument by its alignment minus the previous end, which goes negative
;; (three floats, a char, three floats: the second struct at 13, not 16). On
;; every arm64, a struct over 16 bytes passed by pointer on the stack failed to
;; compile ("unexpected Triv"). The callee read the wrong bytes, silently.
;;
;; Each row calls a function of test/chez/ffi-stack-args-helper.c that folds
;; its arguments in order, so one argument read from the wrong place changes
;; the result.
;;
;; A stock kernel reports and passes, unless JOLT_REQUIRE_LOCKFREE_CHEZ=1 says the
;; kernel must carry jolt's patches (CI): a stock arm64 can't compile the 24-byte
;; row, and a stock x86_64 delivers the 3-char row's final char as 0.
;;
;;   sh test/chez/ffi-stack-args-test.sh <chez>
(import (chezscheme))

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))

(define helper (getenv "JOLT_FFI_STACK_ARGS_HELPER"))
(unless helper (error #f "JOLT_FFI_STACK_ARGS_HELPER is required"))
(load-shared-object helper)

(define (fold . vs) (fold-left (lambda (r v) (+ (* r 7) v)) 0 vs))
(define-ftype two_ints (struct (x int) (y int)))
(define-ftype one_short (struct (a short)))
(define-ftype three_chars (struct (a integer-8) (b integer-8) (c integer-8)))
(define-ftype three_floats (struct (a float) (b float) (c float)))
(define-ftype one_float (struct (a float)))
(define-ftype three_longs (struct (a integer-64) (b integer-64) (c integer-64)))

(define (alloc size set) (let ([p (foreign-alloc size)]) (set p) p))
(define (floats a b c)
  (make-ftype-pointer three_floats
    (alloc (ftype-sizeof three_floats)
           (lambda (p) (foreign-set! 'float p 0 a) (foreign-set! 'float p 4 b) (foreign-set! 'float p 8 c)))))
(define (one-float v)
  (make-ftype-pointer one_float (alloc (ftype-sizeof one_float) (lambda (p) (foreign-set! 'float p 0 v)))))

;; compiled at run time, so a signature Chez refuses fails here, not at load
(define (proc-or-condition name args result)
  (guard (e [(condition? e) e])
    (eval `(foreign-procedure ,name ,args ,result))))

(define three-longs-proc
  (proc-or-condition "stack_three_longs" '(int int int int int int int int (& three_longs) int) 'integer-64))
(define (three-chars-row-right?)
  (= ((foreign-procedure "stack_char_three_chars" (int int int int int int int int integer-8 (& three_chars) integer-8) integer-64)
      1 2 3 4 5 6 7 8 9
      (make-ftype-pointer three_chars
        (alloc (ftype-sizeof three_chars)
               (lambda (p) (foreign-set! 'integer-8 p 0 41) (foreign-set! 'integer-8 p 1 42) (foreign-set! 'integer-8 p 2 43))))
      44)
     (fold 1 2 3 4 5 6 7 8 9 41 42 43 44)))
(define required? (equal? (getenv "JOLT_REQUIRE_LOCKFREE_CHEZ") "1"))
(define refuses-big-struct? (condition? three-longs-proc))

(cond
  [(and refuses-big-struct? (not required?))
   (printf "ffi-stack-args: stock Chez kernel (a struct over 16 bytes on the stack: ~a), stack arguments not checked\n"
           (call-with-string-output-port (lambda (p) (display-condition three-longs-proc p))))]
  [(and (not required?) (not (three-chars-row-right?)))
   (printf "ffi-stack-args: stock Chez kernel (a char after a struct of three chars arrives wrong), stack arguments not checked\n")]
  [else
   (ok "a struct over 16 bytes passed by pointer on the stack compiles" (not refuses-big-struct?))
   (unless refuses-big-struct?
     (ok "a struct over 16 bytes passed by pointer on the stack, then an int"
         (= (three-longs-proc 1 2 3 4 5 6 7 8
              (make-ftype-pointer three_longs
                (alloc (ftype-sizeof three_longs)
                       (lambda (p) (foreign-set! 'integer-64 p 0 41) (foreign-set! 'integer-64 p 8 42) (foreign-set! 'integer-64 p 16 43))))
              44)
            (fold 1 2 3 4 5 6 7 8 41 42 43 44))))
   (ok "int, then struct of two ints: the struct in an 8-byte slot"
       (= ((foreign-procedure "stack_int_two_ints" (int int int int int int int int int (& two_ints) int) integer-64)
           1 2 3 4 5 6 7 8 9
           (make-ftype-pointer two_ints (alloc (ftype-sizeof two_ints) (lambda (p) (foreign-set! 'int p 0 41) (foreign-set! 'int p 4 42))))
           43)
          (fold 1 2 3 4 5 6 7 8 9 41 42 43)))
   (ok "short, then struct of one short: the struct in an 8-byte slot"
       (= ((foreign-procedure "stack_short_one_short" (int int int int int int int int short (& one_short) short) integer-64)
           1 2 3 4 5 6 7 8 9
           (make-ftype-pointer one_short (alloc (ftype-sizeof one_short) (lambda (p) (foreign-set! 'short p 0 41))))
           43)
          (fold 1 2 3 4 5 6 7 8 9 41 43)))
   (ok "char, then struct of three chars: the struct in an 8-byte slot" (three-chars-row-right?))
   (ok "three chars, then a short: padding never negative"
       (= ((foreign-procedure "stack_three_chars_short" (int int int int int int int int integer-8 integer-8 integer-8 short int) integer-64)
           1 2 3 4 5 6 7 8 9 10 11 12 13)
          (fold 1 2 3 4 5 6 7 8 9 10 11 12 13)))
   (ok "three floats, a char, three floats: the second struct 4-aligned after the char"
       (= ((foreign-procedure "stack_three_floats_char_three_floats"
             (int int int int int int int int double double double double double double double double
              (& three_floats) integer-8 (& three_floats))
             double)
           1 2 3 4 5 6 7 8 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 (floats 10.0 11.0 12.0) 9 (floats 34.0 35.0 36.0))
          (+ 36 36.0 10.0 (* 2 11.0) (* 3 12.0) (* 4 9) (* 5 34.0) (* 6 35.0) (* 7 36.0))))
   (ok "one float, a char, one float"
       (= ((foreign-procedure "stack_one_float_char_one_float"
             (int int int int int int int int double double double double double double double double
              (& one_float) integer-8 (& one_float))
             double)
           1 2 3 4 5 6 7 8 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 (one-float 10.0) 9 (one-float 11.0))
          (+ 36 36.0 10.0 (* 2 9) (* 3 11.0))))
   (printf "ffi-stack-args: ~a/~a passed\n" (- total fails) total)
   (exit (if (zero? fails) 0 1))])
