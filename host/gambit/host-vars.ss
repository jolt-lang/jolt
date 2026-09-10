;; host-vars.ss — the clojure.core names host/chez/java/* binds on Chez.
;;
;; On Chez the java/ tree owns the JVM-shaped half of clojure.core: clocks, file
;; IO, the interop entry points, agents and futures, taps, the library shim
;; hooks. This target boots none of those files, so without this one every such
;; name is an UNBOUND GLOBAL: calling it raises a bare Gambit exception with no
;; jolt context and no position. `(time 1)` was exactly that — the macro expands
;; to a current-time-ms call, which crashed with an unreadable error rather than
;; reporting a missing capability.
;;
;; Every name here is either implemented on Gambit or bound to a raise that says
;; which capability is absent. Nothing stays unbound, and nothing pretends to
;; work — the same rule the sa-* capability tiers follow.
;;
;; Loaded after the prelude and post-prelude so these bindings are the last word.

;; ---- degradation -----------------------------------------------------------
;; A catchable UnsupportedOperationException naming the operation and the reason,
;; so the failure reads the same whether it reaches a REPL, a catch clause, or a
;; log line.
(define (gambit-unsupported-fn name reason)
  (lambda args
    (jolt-throw
      (jolt-host-throwable
        "java.lang.UnsupportedOperationException"
        (string-append "clojure.core/" name " is unsupported on the gambit target: "
                       reason)))))

(define (degrade-core-var! name reason)
  (def-var! "clojure.core" name (gambit-unsupported-fn name reason)))

(define (degrade-core-vars! names reason)
  (for-each (lambda (n) (degrade-core-var! n reason)) names))

;; ---- system tier: real -----------------------------------------------------

;; Epoch milliseconds, like System/currentTimeMillis — this backs `time`, and
;; user code reads it as a wall clock, so it must be epoch-based rather than the
;; process-relative (real-time) the adapter's sa-real-time-ms reports.
(define (gambit-epoch-ms)
  (inexact->exact (floor (* 1000 (time->seconds (current-time))))))
(def-var! "clojure.core" "current-time-ms" gambit-epoch-ms)

(def-var! "clojure.core" "flush"
  (lambda args (force-output) jolt-nil))

(def-var! "clojure.core" "__stdin-read-line"
  (lambda args
    (let ((l (read-line)))
      (if (eof-object? l) jolt-nil l))))

;; ---- file IO: real on gsi ---------------------------------------------------
;; These work wherever the host has a filesystem. Under the js target the open
;; fails and Gambit's own error surfaces with its message intact (the condition
;; shims in prelude-shims.ss make it readable), which is the honest outcome for
;; a target that has no files.

(define (gambit-read-all port)
  (call-with-output-string
    (lambda (out)
      (let loop ()
        (let ((c (read-char port)))
          (unless (eof-object? c)
            (write-char c out)
            (loop)))))))

(def-var! "clojure.core" "slurp"
  (lambda (src . opts)
    (if (string? src)
        (call-with-input-file src gambit-read-all)
        (jolt-throw
          (jolt-host-throwable "java.lang.IllegalArgumentException"
            "slurp on the gambit target takes a path string")))))

(def-var! "clojure.core" "spit"
  (lambda (dest content . opts)
    (if (string? dest)
        (begin
          (call-with-output-file dest
            (lambda (p) (display (jolt-str-render-one content) p)))
          jolt-nil)
        (jolt-throw
          (jolt-host-throwable "java.lang.IllegalArgumentException"
            "spit on the gambit target takes a path string")))))

;; ---- library shim hooks: real ----------------------------------------------
;; The underlying arm registries are all present in the portable runtime, so a
;; library that models its own host values works here unchanged.

(def-var! "clojure.core" "__register-eq!"
  (lambda (pred handler)
    (register-eq-arm! (lambda (a b) (jolt-truthy? (jolt-invoke pred a b)))
                      (lambda (a b) (jolt-truthy? (jolt-invoke handler a b))))
    jolt-nil))

(def-var! "clojure.core" "__register-hash!"
  (lambda (pred handler)
    (register-hash-arm! (lambda (x) (jolt-truthy? (jolt-invoke pred x)))
                        (lambda (x) (jolt-invoke handler x)))
    jolt-nil))

(def-var! "clojure.core" "__register-str!"
  (lambda (pred render)
    (register-str-render! (lambda (x) (jolt-truthy? (jolt-invoke pred x)))
                          (lambda (x) (jolt-invoke render x)))
    jolt-nil))

(def-var! "clojure.core" "__register-pr!"
  (lambda (pred render)
    (register-pr-arm! (lambda (x) (jolt-truthy? (jolt-invoke pred x)))
                      (lambda (x) (jolt-invoke render x)))
    jolt-nil))

(def-var! "clojure.core" "__register-instance-check!"
  (lambda (f)
    (register-instance-check-arm!
      (lambda (cls val) (jolt-truthy? (jolt-invoke f cls val))))
    jolt-nil))

;; The class-methods half of the same seam. The table is write-only on this boot
;; (host-static-classes.ss is excluded, so no host interop reads it back), but the
;; var has to EXIST: jolt.socket and jolt/time/*.clj call it at the top level, and
;; an unbound clojure.core/__register-class-methods! fails their load outright.
(def-var! "clojure.core" "__register-class-methods!"
  (lambda (tag members)
    (register-class-methods! tag members)
    jolt-nil))

;; ---- queries answer, they do not raise -------------------------------------
;; A predicate whose type cannot exist on this target is false, not an error —
;; a caller asking "is this a delay?" deserves an answer.

(def-var! "clojure.core" "delay?" (lambda (x) #f))
(def-var! "clojure.core" "queue?" (lambda (x) #f))

;; No tap registry exists, so nothing is listening and tap> reports that.
;; Registering one, on the other hand, is a request this target cannot honor.
(def-var! "clojure.core" "tap>" (lambda (x) #f))

;; The reader consults this map; empty is the truth here, not a missing name.
;; empty-pmap is a VALUE, not a constructor — calling it applies the map
(def-var! "clojure.core" "default-data-readers" empty-pmap)

;; ---- host errors carry a class and a message --------------------------------
;; Gambit has no arity introspection (see procedure-arity-mask in
;; prelude-shims.ss), so seq.ss's structural arity pre-check always passes and
;; Gambit's own runtime raises instead. Those exception objects have no JVM class,
;; so (class e), (str e), and a catch clause all fell through to the class-model
;; default and printed "#object[:object]" — which is what `(time)` reported
;; instead of an arity error. Map Gambit's wording onto the class the equivalent
;; Chez failure carries. The wording is Gambit's own (there is no arg count or
;; callee name to recover), but the class and the message are real.

(define (gambit-msg-has? m sub)
  (let ((ml (string-length m)) (sl (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i sl) ml) #f)
            ((string=? (substring m i (+ i sl)) sub) #t)
            (else (loop (+ i 1)))))))

(define (gambit-host-error? x)
  (and (condition? x) (not (jolt-ex-info-record? x))))

(define (gambit-error-class x)
  (let ((m (or (condition-message x) "")))
    (cond
      ((gambit-msg-has? m "Wrong number of arguments") "clojure.lang.ArityException")
      ((gambit-msg-has? m "Unbound variable")          "java.lang.IllegalStateException")
      ((gambit-msg-has? m "Division by zero")          "java.lang.ArithmeticException")
      ((gambit-msg-has? m "expected")                  "java.lang.ClassCastException")
      ((gambit-msg-has? m "Operator is not a PROCEDURE") "java.lang.ClassCastException")
      (else "java.lang.RuntimeException"))))

(define (gambit-collapse-lines s)
  ;; Gambit's description runs onto further lines (the procedure and the args it
  ;; got); a message is one line everywhere else in jolt, so fold them together.
  (let ((n (string-length s)))
    (call-with-output-string
      (lambda (o)
        (let loop ((i 0) (gap #f))
          (if (>= i n)
              #t
              (let ((c (string-ref s i)))
                (cond ((memv c '(#\newline #\return #\tab)) (loop (+ i 1) #t))
                      (else (when (and gap (> i 0)) (write-char #\space o))
                            (write-char c o)
                            (loop (+ i 1) #f))))))))))

(define (gambit-error-tostring x)
  (string-append (gambit-error-class x) ": "
                 (gambit-collapse-lines (or (condition-message x) "host error"))))

(register-class-arm! gambit-host-error? gambit-error-class)
(register-str-render! gambit-host-error? gambit-error-tostring)
(register-pr-arm! gambit-host-error? gambit-error-tostring)

;; ---- host objects cannot exist here ----------------------------------------
;; The jhost record (java/host-static.ss) and the reader/stream shims built on
;; it live in the java/ tree this target does not boot, so no value here can BE
;; one. Answering #f makes every branch guarded by these predicates dead code —
;; which is why the accessors behind them need no stand-in, since Scheme resolves
;; a global only when it is actually reached. printing.ss consults jhost? on the
;; way to a writer, which is what made `(time 1)` die on an unbound name.

;; The reader/stream shims genuinely do not exist here, so nothing is one.
(define (reader-jhost? x) #f)
(define (jhost-seqable-shim? x) #f)

;; jhost itself, on the other hand, is the generic carrier for a host value, and
;; the class model needs it: (class x) returns a Class OBJECT, which on Chez is a
;; jhost tagged "class" (java/host-static-classes.ss). Without it (class e) died
;; on an unbound make-class-obj. Same record shape as Chez's, so the booted
;; class/printing code reads it unchanged.
(define-record-type jhost
  (fields tag (mutable state))
  (nongenerative gambit-jhost-v1))

(define (make-class-obj name) (make-jhost "class" (vector name)))
(define (jclass? x) (and (jhost? x) (string=? (jhost-tag x) "class")))
(define (jclass-name x) (vector-ref (jhost-state x) 0))

;; Class tokens intern per name so identity, = and defmethod keys are stable.
(define jolt-class-for-tbl (make-hashtable string-hash string=?))
(define (jolt-class-for name)
  (or (hashtable-ref jolt-class-for-tbl name #f)
      (let ((obj (make-class-obj name)))
        (hashtable-set! jolt-class-for-tbl name obj)
        obj)))
(register-str-render! jclass? (lambda (x) (string-append "class " (jclass-name x))))
(register-pr-arm! jclass? jclass-name)
(register-eq-arm! (lambda (a b) (and (jclass? a) (jclass? b)))
                  (lambda (a b) (string=? (jclass-name a) (jclass-name b))))

;; ---- absent capabilities ---------------------------------------------------

(degrade-core-vars! '("host-new" "host-static-call" "host-static-ref" "make-proxy")
                    "there are no JVM class shims on this target")

(degrade-core-vars! '("future-call" "future-cancel" "send-via" "agent-errors"
                      "clear-agent-errors" "await-for" "error-handler" "error-mode"
                      "set-error-handler!" "set-error-mode!" "release-pending-sends"
                      "set-agent-send-executor!" "set-agent-send-off-executor!"
                      "shutdown-agents")
                    "futures and agents are not wired up on this target")

(degrade-core-vars! '("make-delay") "delays are not wired up on this target")

(degrade-core-vars! '("clojure.lang.PersistentQueue")
                    "persistent queues are not wired up on this target")

(degrade-core-vars! '("require" "use")
                    "this target loads no source at runtime")

(degrade-core-vars! '("bigdec" "rationalize")
                    "there is no BigDecimal on this target")

(degrade-core-vars! '("add-tap" "remove-tap")
                    "there is no tap registry on this target")
