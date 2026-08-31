;; Exact scalar-width FFI contract. Covers runtime memory access plus compiled
;; procedures and callables through real C witnesses. The signature half needs
;; compiler seeds re-minted from the edited backend source.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/java/ffi.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "FAIL: ~a\n" name)))
(define (raises? thunk) (guard (e (#t #t)) (thunk) #f))
(define (ev source) (jolt-compile-eval source "user"))

(define helper-so (getenv "JOLT_FFI_WIDTHS_HELPER"))
(unless helper-so
  (error #f "JOLT_FFI_WIDTHS_HELPER must name the compiled helper library"))
(sa-load-shared-object helper-so)

;; Width and alias identity in the runtime memory path.
(for-each
 (lambda (entry)
   (ok (string-append "sizeof :" (car entry))
       (= (cdr entry)
          (jnum->exact
           (ev (string-append "(jolt.ffi/sizeof :" (car entry) ")"))))))
 '(("int8" . 1) ("i8" . 1) ("uint8" . 1) ("bool" . 1)
   ("int16" . 2) ("short" . 2) ("uint16" . 2) ("ushort" . 2)
   ("int32" . 4) ("uint32" . 4)))

(ev "(def width-rw (fn [ty v] (let [p (jolt.ffi/alloc (jolt.ffi/sizeof ty))] (try (jolt.ffi/write p ty v 0) (jolt.ffi/read p ty) (finally (jolt.ffi/free p))))))")
(for-each
 (lambda (entry)
   (ok (car entry) (= (cadr entry) (jnum->exact (ev (caddr entry))))))
 '(("int8 minimum" -128 "(width-rw :int8 -128)")
   ("int8 maximum" 127 "(width-rw :int8 127)")
   ("uint8 maximum" 255 "(width-rw :uint8 255)")
   ("int16 minimum" -32768 "(width-rw :int16 -32768)")
   ("int16 maximum" 32767 "(width-rw :int16 32767)")
   ("uint16 middle-high" 40000 "(width-rw :uint16 40000)")
   ("uint16 maximum" 65535 "(width-rw :uint16 65535)")
   ("int32 minimum" -2147483648 "(width-rw :int32 -2147483648)")
   ("int32 maximum" 2147483647 "(width-rw :int32 2147483647)")
   ("uint32 middle-high" 3000000000 "(width-rw :uint32 3000000000)")
   ("uint32 maximum" 4294967295 "(width-rw :uint32 4294967295)")))

;; Signed and unsigned views at one width expose identical stored bits.
(for-each
 (lambda (entry)
   (ok (car entry) (= (cadr entry) (jnum->exact (ev (caddr entry))))))
 '(("int8 -1 as uint8" 255
    "(let [p (jolt.ffi/alloc 1)] (try (jolt.ffi/write p :int8 -1 0) (jolt.ffi/read p :uint8) (finally (jolt.ffi/free p))))")
   ("i8 -1 as byte" 255
    "(let [p (jolt.ffi/alloc 1)] (try (jolt.ffi/write p :i8 -1 0) (jolt.ffi/read p :byte) (finally (jolt.ffi/free p))))")
   ("int16 -1 as uint16" 65535
    "(let [p (jolt.ffi/alloc 2)] (try (jolt.ffi/write p :int16 -1 0) (jolt.ffi/read p :uint16) (finally (jolt.ffi/free p))))")
   ("short -1 as ushort" 65535
    "(let [p (jolt.ffi/alloc 2)] (try (jolt.ffi/write p :short -1 0) (jolt.ffi/read p :ushort) (finally (jolt.ffi/free p))))")
   ("uint16 max as int16" -1
    "(let [p (jolt.ffi/alloc 2)] (try (jolt.ffi/write p :uint16 65535 0) (jolt.ffi/read p :int16) (finally (jolt.ffi/free p))))")
   ("int32 -1 as uint32" 4294967295
    "(let [p (jolt.ffi/alloc 4)] (try (jolt.ffi/write p :int32 -1 0) (jolt.ffi/read p :uint32) (finally (jolt.ffi/free p))))")))

;; :bool is the one type whose VALUE, not just width, converts at the boundary:
;; one byte on the wire, true/false in jolt, and jolt truthiness on the way out —
;; so nil and false store 0 and everything else stores 1. Without the conversion
;; a C predicate would answer the truthy number 0.
(for-each
 (lambda (entry)
   (ok (car entry) (equal? (cadr entry) (ev (caddr entry)))))
 '(("bool true round-trips" #t "(width-rw :bool true)")
   ("bool false round-trips" #f "(width-rw :bool false)")
   ("bool nil stores false" #f "(width-rw :bool nil)")
   ("bool truthy value stores true" #t "(width-rw :bool 0)")))
(for-each
 (lambda (entry)
   (ok (car entry) (= (cadr entry) (jnum->exact (ev (caddr entry))))))
 '(("bool true is the byte 1" 1
    "(let [p (jolt.ffi/alloc 1)] (try (jolt.ffi/write p :bool true 0) (jolt.ffi/read p :uint8) (finally (jolt.ffi/free p))))")
   ("bool false is the byte 0" 0
    "(let [p (jolt.ffi/alloc 1)] (try (jolt.ffi/write p :bool false 0) (jolt.ffi/read p :uint8) (finally (jolt.ffi/free p))))")))
(ok "a nonzero byte reads as bool true"
    (eq? #t (ev "(let [p (jolt.ffi/alloc 1)] (try (jolt.ffi/write p :uint8 200 0) (jolt.ffi/read p :bool) (finally (jolt.ffi/free p))))")))

(ok "unknown runtime type rejects"
    (raises? (lambda () (ev "(jolt.ffi/sizeof :not-a-type)"))))

;; Native arguments/results prove the emitted ABI, while map inspection pins
;; exact declarations so a widened signature cannot pass through C narrowing.
(ev "(jolt.ffi/load-library)")
(for-each
 ev
 '("(def w-i8 (jolt.ffi/__cfn \"jolt_w_widen_i8\" [:i8] :int64))"
   "(def w-int8 (jolt.ffi/__cfn \"jolt_w_widen_i8\" [:int8] :int64))"
   "(def w-i16 (jolt.ffi/__cfn \"jolt_w_widen_i16\" [:short] :int64))"
   "(def w-int16 (jolt.ffi/__cfn \"jolt_w_widen_i16\" [:int16] :int64))"
   "(def w-u16 (jolt.ffi/__cfn \"jolt_w_widen_u16\" [:ushort] :uint64))"
   "(def w-uint16 (jolt.ffi/__cfn \"jolt_w_widen_u16\" [:uint16] :uint64))"
   "(def w-i32 (jolt.ffi/__cfn \"jolt_w_widen_i32\" [:int32] :int64))"
   "(def w-u32 (jolt.ffi/__cfn \"jolt_w_widen_u32\" [:uint32] :uint64))"
   "(def r-i8 (jolt.ffi/__cfn \"jolt_w_return_i8\" [] :int8))"
   "(def r-i16 (jolt.ffi/__cfn \"jolt_w_return_i16\" [] :int16))"
   "(def r-u16 (jolt.ffi/__cfn \"jolt_w_return_u16\" [] :uint16))"
   "(def r-i32 (jolt.ffi/__cfn \"jolt_w_return_i32\" [] :int32))"
   "(def r-u32 (jolt.ffi/__cfn \"jolt_w_return_u32\" [] :uint32))"
   "(def w-bool (jolt.ffi/__cfn \"jolt_w_widen_bool\" [:bool] :int64))"
   "(def sizeof-bool (jolt.ffi/__cfn \"jolt_w_sizeof_bool\" [] :size_t))"
   "(def r-bool-true (jolt.ffi/__cfn \"jolt_w_return_bool_true\" [] :bool))"
   "(def r-bool-false (jolt.ffi/__cfn \"jolt_w_return_bool_false\" [] :bool))"))

;; C agrees that :bool is one byte wide, and the value converts in both
;; directions across a real _Bool signature.
(ok "C sizeof(bool) matches :bool"
    (= (jnum->exact (ev "(sizeof-bool)")) (jnum->exact (ev "(jolt.ffi/sizeof :bool)"))))
(ok "bool arguments reach C as the boolean they name"
    (equal? '(42 -42 -42 42)
            (map (lambda (source) (jnum->exact (ev source)))
                 '("(w-bool true)" "(w-bool false)" "(w-bool nil)" "(w-bool :yes)"))))
(ok "bool results come back as true and false"
    (equal? '(#t #f) (list (ev "(r-bool-true)") (ev "(r-bool-false)"))))

(ok "exact native arguments preserve boundaries"
    (equal? '(-128 -32768 65535 -2147483648 4294967295)
            (map (lambda (source) (jnum->exact (ev source)))
                 '("(w-i8 -128)" "(w-i16 -32768)" "(w-u16 65535)"
                   "(w-i32 -2147483648)" "(w-u32 4294967295)"))))
(ok "exact native results preserve boundaries"
    (equal? '(-128 -32768 65535 -2147483648 4294967295)
            (map (lambda (source) (jnum->exact (ev source)))
                 '("(r-i8)" "(r-i16)" "(r-u16)" "(r-i32)" "(r-u32)"))))

(define compiler-type->chez (var-deref "jolt.backend-scheme" "ffi-type->chez"))
(ok "compiler exact-width map is complete"
    (for-all
     (lambda (entry)
       (equal? (cdr entry) (jolt-invoke1 compiler-type->chez (car entry))))
     '(("int8" . "integer-8") ("i8" . "integer-8") ("bool" . "unsigned-8")
       ("int16" . "integer-16") ("short" . "integer-16")
       ("uint16" . "unsigned-16") ("ushort" . "unsigned-16")
       ("int32" . "integer-32") ("uint32" . "unsigned-32"))))

;; Chez reinterprets opposite-signed values which fit the bit-pattern width,
;; and rejects values beyond that envelope. Pin both parts of that contract.
(for-each
 (lambda (entry)
   (ok (car entry) (= (cadr entry) (jnum->exact (ev (caddr entry))))))
 '(("i8 narrows 128" -128 "(w-i8 128)")
   ("int8 narrows 128" -128 "(w-int8 128)")
   ("short narrows 32768" -32768 "(w-i16 32768)")
   ("int16 narrows 32768" -32768 "(w-int16 32768)")
   ("ushort narrows -1" 65535 "(w-u16 -1)")
   ("uint16 narrows -1" 65535 "(w-uint16 -1)")
   ("int32 narrows high bit" -2147483648 "(w-i32 2147483648)")
   ("uint32 narrows -1" 4294967295 "(w-u32 -1)")))
(for-each
 (lambda (entry) (ok (car entry) (raises? (lambda () (ev (cdr entry))))))
 '(("i8 rejects below envelope" . "(w-i8 -129)")
   ("int8 rejects below envelope" . "(w-int8 -129)")
   ("short rejects below envelope" . "(w-i16 -32769)")
   ("int16 rejects below envelope" . "(w-int16 -32769)")
   ("ushort rejects above envelope" . "(w-u16 65536)")
   ("uint16 rejects above envelope" . "(w-uint16 65536)")
   ("int32 rejects below envelope" . "(w-i32 -2147483649)")
   ("uint32 rejects above envelope" . "(w-u32 4294967296)")))

;; C invokes callbacks at each exact width, covering the inverse ABI path.
(for-each
 ev
 '("(def cb-i8 (jolt.ffi/__ccallable (fn [x] x) [:int8] :int8))"
   "(def cb-i16 (jolt.ffi/__ccallable (fn [x] x) [:int16] :int16))"
   "(def cb-u16 (jolt.ffi/__ccallable (fn [x] x) [:uint16] :uint16))"
   "(def cb-i32 (jolt.ffi/__ccallable (fn [x] x) [:int32] :int32))"
   "(def cb-u32 (jolt.ffi/__ccallable (fn [x] x) [:uint32] :uint32))"
   "(def call-i8 (jolt.ffi/__cfn \"jolt_w_call_i8\" [:pointer] :int64))"
   "(def call-i16 (jolt.ffi/__cfn \"jolt_w_call_i16\" [:pointer] :int64))"
   "(def call-u16 (jolt.ffi/__cfn \"jolt_w_call_u16\" [:pointer] :uint64))"
   "(def call-i32 (jolt.ffi/__cfn \"jolt_w_call_i32\" [:pointer] :int64))"
   "(def call-u32 (jolt.ffi/__cfn \"jolt_w_call_u32\" [:pointer] :uint64))"
   ;; The callback is the inverse direction: C passes the byte in and reads the
   ;; byte back out, so `not` here answers 0 for C's true and 1 for its false.
   "(def cb-bool (jolt.ffi/__ccallable (fn [x] (not x)) [:bool] :bool))"
   "(def call-bool (jolt.ffi/__cfn \"jolt_w_call_bool\" [:pointer] :int64))"))
(ok "C-invoked bool callbacks convert both directions"
    (= 1 (jnum->exact (ev "(call-bool cb-bool)"))))
(ev "(jolt.ffi/free-callable cb-bool)")
(ok "C-invoked exact-width callbacks preserve boundaries"
    (equal? '(-128 -32768 65535 -2147483648 4294967295)
            (map (lambda (source) (jnum->exact (ev source)))
                 '("(call-i8 cb-i8)" "(call-i16 cb-i16)" "(call-u16 cb-u16)"
                   "(call-i32 cb-i32)" "(call-u32 cb-u32)"))))
(for-each
 (lambda (name) (ev (string-append "(jolt.ffi/free-callable " name ")")))
 '("cb-i8" "cb-i16" "cb-u16" "cb-i32" "cb-u32"))

(for-each
 (lambda (entry) (ok (car entry) (raises? (lambda () (ev (cdr entry))))))
 '(("unknown __cfn argument rejects" .
    "(jolt.ffi/__cfn \"jolt_w_widen_i16\" [:bogus] :int)")
   ("unknown __cfn result rejects" .
    "(jolt.ffi/__cfn \"jolt_w_widen_i16\" [:int16] :bogus)")
   ("unknown __ccallable argument rejects" .
    "(jolt.ffi/__ccallable (fn [x] x) [:bogus] :int)")
   ("unknown __ccallable result rejects" .
    "(jolt.ffi/__ccallable (fn [x] x) [:int] :bogus)")))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
