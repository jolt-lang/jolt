;; Queue an event after callback lock entry, while the Chez boundary mask is
;; still raised. Enabling events must happen only after the cleanup is installed.
(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/java/ffi.ss")
(load-shared-object #f)
(define checks 0)
(define (check label pred)
  (set! checks (+ checks 1))
  (unless pred (error 'ffi-callback-boundary label)))

;; A void adapter ignores all values, while a nonvoid adapter rejects the wrong
;; value count. Both preserve interrupt depth, including collect-safe adapters.
(for-each
  (lambda (co)
    (lock-object co)
    (let ((call (foreign-procedure (foreign-callable-entry-point co) () void))
          (depth (sa-disable-count)))
      (check "void adapter discards zero/multiple values" (guard (e (#t #f)) (call) #t))
      (check "void adapter preserves interrupt depth" (= depth (sa-disable-count))))
    (unlock-object co))
  (list (sa-foreign-callable values () void)
        (sa-foreign-callable (lambda () (values 1 2)) () void)
        (sa-foreign-callable-collect-safe values () void)
        (sa-foreign-callable-collect-safe (lambda () (values 1 2)) () void)))
(for-each
  (lambda (co)
    (lock-object co)
    (let ((call (foreign-procedure (foreign-callable-entry-point co) () int))
          (depth (sa-disable-count)))
      (check "nonvoid adapter rejects zero/multiple values" (guard (e (#t #t)) (call) #f))
      (check "nonvoid adapter preserves interrupt depth" (= depth (sa-disable-count))))
    (unlock-object co))
  (list (sa-foreign-callable values () int)
        (sa-foreign-callable (lambda () (values 1 2)) () int)
        (sa-foreign-callable-collect-safe values () int)
        (sa-foreign-callable-collect-safe (lambda () (values 1 2)) () int)))

;; Instrument the real entry macro without changing its counter operation. Each
;; callback below expands the real adapter and body macros against this binding.
(define inject? #f)
(define-syntax jolt-locks-enter!
  (syntax-rules ()
    ((_)
     (begin
       (set-virtual-register! 7 (fx+ 1 (virtual-register 7)))
       (when inject?
         (set! inject? #f)
         (#3%$tc-field 'keyboard-interrupt-pending (#3%$tc) #t)
         (#3%$tc-field 'something-pending (#3%$tc) #t))))))

(define entered? #f)
(define fired? #f)
(define ordinary (sa-foreign-callable (lambda () (set! entered? #t) 1) () int))
(define collect-safe (sa-foreign-callable-collect-safe (lambda () (set! entered? #t) 1) () int))
(define saved-handler (keyboard-interrupt-handler))
(dynamic-wind
  void
  (lambda ()
    (for-each
      (lambda (co)
        (lock-object co)
        (let* ((call (foreign-procedure (foreign-callable-entry-point co) () int))
               (depth (sa-disable-count)) (locks (jolt-locks-held))
               (result
                 (call/cc
                   (lambda (escape)
                     (keyboard-interrupt-handler
                       (lambda () (set! fired? #t) (escape 'event)))
                     (set! entered? #f) (set! fired? #f) (set! inject? #t)
                     (call)
                     'returned))))
          (check "pending event escapes callback" (eq? result 'event))
          (check "queued event actually fired" fired?)
          (check "pending event precedes user callback body" (not entered?))
          (check "event escape restores lock count" (= locks (jolt-locks-held)))
          (check "event escape restores interrupt depth" (= depth (sa-disable-count))))
        (unlock-object co))
      (list ordinary collect-safe)))
  (lambda () (keyboard-interrupt-handler saved-handler)))
(printf "ffi-callback-boundary: ~a passed\n" checks)
