;; host-faults.ss — a fault the host itself raised becomes a typed throwable.
;;
;; Every operation site is meant to throw a typed jolt throwable before Chez can
;; raise, but a primitive handed the wrong value (string-append on nil, string-ref
;; past the end) raises a raw Chez condition, and a catch used to bind that
;; condition as it was: (class e) answered :object, ex-message nil, (instance?
;; Exception e) false, and only a broad clause matched it. jolt-unwrap-throw
;; (rt.ss) hands every raw condition through jolt-fault->throwable, so what a
;; catch binds always has the class, the message and the Throwable surface of a
;; host throwable, and catch clauses dispatch on that class like on any other.
;;
;; The class comes from the condition's message template and `who` — how Chez
;; names each kind of fault — mapped to the class the same misuse raises on the
;; JVM: a nil argument is a NullPointerException, a wrong-typed one a
;; ClassCastException, a bad index an IndexOutOfBoundsException (String's own
;; subclass from a string primitive), a wrong argument count an ArityException,
;; division by zero or overflow an ArithmeticException, an i/o failure an
;; IOException (FileNotFoundException for a missing file), heap exhaustion an
;; OutOfMemoryError. Anything unrecognised is a RuntimeException: a
;; (catch Exception e) still admits it, a (catch ArithmeticException e) no
;; longer does.
;;
;; One throwable per condition. The same fault observed twice (a future's deref
;; and its .getCause, a rethrow) is one identity, and the throwable finds its
;; condition again for the stack trace: Chez attaches the raise-time continuation
;; to the condition, and jolt-error-continuation (source-registry.ss) reads it
;; through jolt-fault-condition-of. Both maps are ephemeron tables — the
;; throwable and its condition reference each other only through the tables, so
;; neither keeps the other alive once jolt code drops it. A fault can be raised
;; on any thread; the tables are mutex-guarded. Error path only, never per call.

(define fault-throwables (make-ephemeron-eq-hashtable))   ; condition -> throwable
(define fault-conditions (make-ephemeron-eq-hashtable))   ; throwable -> condition
(define fault-mutex (make-mutex))

;; Substring test on a message template. Local so classification depends on
;; nothing loaded after this file: a fault raised while the runtime is still
;; loading is classified too.
(define (fault-template-has? m s)
  (let ((n (string-length m)) (k (string-length s)))
    (let loop ((i 0))
      (cond ((fx>? (fx+ i k) n) #f)
            ((string=? (substring m i (fx+ i k)) s) #t)
            (else (loop (fx+ i 1)))))))

(define string-index-whos
  '(string-ref substring string-copy string-set! string-fill! substring-fill! string-copy!))
;; The proven ^doubles path (jolt-flaget / jolt-flaset, natives-array.ss) relies
;; on flvector-ref's own range check — a pre-check there costs ~1ns per access —
;; so its escaping condition IS the array bounds error. Nothing else in the
;; runtime reaches these two primitives: the generic array path pre-checks and
;; throws typed.
(define array-index-whos '(flvector-ref flvector-set!))

(define (fault-class c)
  (let* ((m (if (message-condition? c) (condition-message c) ""))
         (m (if (string? m) m ""))
         (irr (if (irritants-condition? c) (condition-irritants c) '()))
         (irr (if (list? irr) irr '()))
         (who (and (who-condition? c) (condition-who c)))
         (has? (lambda (s) (fault-template-has? m s)))
         ;; a nil among the irritants is the value the primitive choked on
         (nil-or-cast (lambda ()
                        (if (exists (lambda (x) (jolt-nil? x)) irr)
                            "java.lang.NullPointerException"
                            "java.lang.ClassCastException"))))
    (cond
      ((i/o-file-does-not-exist-error? c) "java.io.FileNotFoundException")
      ((i/o-error? c) "java.io.IOException")
      ((has? "out of memory") "java.lang.OutOfMemoryError")
      ((or (has? "is not a valid index") (has? "are not valid start/end indices") (has? "is out of range"))
       (cond ((memq who string-index-whos) "java.lang.StringIndexOutOfBoundsException")
             ((memq who array-index-whos) "java.lang.ArrayIndexOutOfBoundsException")
             (else "java.lang.IndexOutOfBoundsException")))
      ((or (has? "incorrect argument count") (has? "incorrect number of arguments"))
       "clojure.lang.ArityException")
      ((has? "attempt to apply non-procedure") (nil-or-cast))
      ((or (has? "undefined for") (has? "divide by zero") (has? "overflow")) "java.lang.ArithmeticException")
      ;; an unbound Scheme variable is jolt's own error, not a cast
      ((has? "is not bound") "java.lang.RuntimeException")
      ((has? "is not ") (nil-or-cast))
      (else "java.lang.RuntimeException"))))

(define (fault-message c)
  ;; a message that fails to render must not raise inside the handler that is
  ;; unwrapping the fault
  (guard (e (#t "host fault"))
    (condition->message-string c)))

;; First sight of a condition is also where its throw site is stashed, unless
;; the uncaught-path handler (jolt-capture-fault!, which runs before the stack
;; unwinds) already did for this very condition. A catch's guard handler is
;; nearer than that one, so a caught fault arrives here with the stash empty
;; and the site vreg still holding the faulting call — nothing but the unwind
;; has run since the raise. Read it before rendering the message: printing an
;; irritant can run jolt code whose own tail sites overwrite the slot.
(define (jolt-fault-throwable c)
  (jolt-with-mutex fault-mutex
    (or (hashtable-ref fault-throwables c #f)
        (begin
          (unless (eq? (jolt-fault-captured) c)
            (jolt-throw-sitep (jolt-live-site)))
          (let ((t (jolt-host-throwable (fault-class c) (fault-message c))))
            (hashtable-set! fault-throwables c t)
            (hashtable-set! fault-conditions t c)
            t)))))

(set! jolt-fault->throwable jolt-fault-throwable)
(set! jolt-fault-condition-of
  (lambda (v)
    (and (jolt-ex-info-record? v)
         (jolt-with-mutex fault-mutex (hashtable-ref fault-conditions v #f)))))
