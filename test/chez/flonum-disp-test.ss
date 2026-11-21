;; test/chez/flonum-disp-test.ss — finding a flonum's payload offset never
;; reads memory in front of the flonum. Run: chez --script test/chez/flonum-disp-test.ss
;; (wired into `make flonumdisp`, part of `make ci` and the windows-deps job).
;;
;; WHY (#1337). scheme-adapter-runtime.ss finds the payload's displacement from
;; a flonum's tagged pointer once at load, by probing 2.5 for its own bytes. The
;; probe used to count up from displacement 0. A flonum's storage starts AT its
;; payload (6 bytes past the tagged pointer on 64-bit Chez), so every probe below
;; the payload read bytes in front of the object. When the literal 2.5 was the
;; first object in a fresh segment and the memory before that segment was not
;; mapped, the first probe faulted. A MinGW-built scheme.exe has no handler for
;; the fault (POSIX Chez raises "invalid memory reference"), so the process died
;; at load with exit 139. Where 2.5 lands depends on everything allocated before
;; it, which is why the crash came and went with the shape of the console ports
;; (`> f 2>&1` merges stdout and stderr into one port, one port fewer) and with
;; unrelated bytes of cli.ss.
;;
;; The scenario is a REPRODUCER, not a smoke: it builds a flonum whose payload
;; is the first byte of a readable page right after an inaccessible one, and
;; runs the probe on it. The upward probe faults there on every host.

(import (chezscheme))
(load "host/chez/scheme-adapter-runtime.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (display "FAIL: ") (display name) (newline)))

(ok "the displacement reads 2.5 back"
    (eqv? (#%$object-ref 'double 2.5 sa-flonum-disp) 2.5))

;; Two 64 KiB halves, the first inaccessible and the second readable; 64 KiB is
;; a multiple of every page size jolt runs on. Answers the readable half's base.
(define span 65536)
(define (guarded-page)
  (if (eq? (sa-os-family) 'windows)
      (begin
        (load-shared-object "kernel32.dll")
        (let* ((valloc (foreign-procedure "VirtualAlloc" (uptr size_t unsigned-32 unsigned-32) uptr))
               ;; reserve both halves (MEM_RESERVE, PAGE_NOACCESS), commit the second
               (base (valloc 0 (* 2 span) #x2000 #x01)))
          (and (not (eqv? base 0))
               (not (eqv? 0 (valloc (+ base span) span #x1000 #x04)))
               (+ base span))))
      (let* ((_ (let try ((libs '(#f "libc.so.6" "libc.so" "libc.dylib")))
                  (unless (or (null? libs) (foreign-entry? "mmap"))
                    (guard (e (#t #f)) (load-shared-object (car libs)))
                    (try (cdr libs)))))
             (mmap (foreign-procedure "mmap" (uptr size_t int int int long) uptr))
             (mprotect (foreign-procedure "mprotect" (uptr size_t int) int))
             (map-anon (if (eq? (sa-os-family) 'macos) #x1000 #x20))
             ;; PROT_NONE, MAP_PRIVATE|MAP_ANON; then PROT_READ|PROT_WRITE on the second half
             (base (mmap 0 (* 2 span) 0 (fxior 2 map-anon) -1 0)))
        (and (not (eqv? base (- (expt 2 64) 1)))
             (eqv? 0 (mprotect (+ base span) span 3))
             (+ base span)))))

(define page (guarded-page))
(ok "mapped a readable page behind an inaccessible one" page)
(when page
  (foreign-set! 'double page 0 2.5)
  ;; no collection while a pointer into foreign memory is live
  (let ((answer
          (parameterize ((collect-request-handler void))
            (guard (e (#t (if (condition? e)
                              (call-with-string-output-port
                                (lambda (p) (display-condition e p)))
                              e)))
              (sa-flonum-payload-disp (#%$address->object page sa-flonum-disp))))))
    (ok (format "the probe stays inside the flonum (answered ~a)" answer)
        (eqv? answer sa-flonum-disp))))

;; The high word against the buffered bit cast.
(for-each
  (lambda (x)
    (ok (format "sa-flonum-hi32 ~s" x)
        (= (sa-flonum-hi32 x) (quotient (sa-flonum->bits x) 4294967296)))
    (ok (format "sa-flonum-lo32 ~s" x)
        (= (sa-flonum-lo32 x) (remainder (sa-flonum->bits x) 4294967296))))
  (list 0.0 -0.0 1.0 2.5 -3.75 1e300 4.9e-324 +inf.0 -inf.0 0.1 (/ 1.0 3.0)))

(display (if (= fails 0) "flonum-disp: all " "flonum-disp: FAILED "))
(display (- total fails)) (display "/") (display total) (newline)
(exit (if (= fails 0) 0 1))
