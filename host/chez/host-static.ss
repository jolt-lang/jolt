;; host-static.ss — host class statics + constructors on Chez.
;;
;; The analyzer lowers `Class/member` to a :host-static node and `(Class. ...)` /
;; `(new Class ...)` to a :host-new node (jolt-core/jolt/analyzer.clj); the Chez
;; emit lowers a value ref to (host-static-ref "Class" "member"), a
;; call head to (host-static-call "Class" "member" args...), and a constructor to
;; (host-new "Class" args...). This file is the runtime registry those three
;; resolve against — the class-statics / class-ctors /
;; tagged-methods registries,
;; restricted to the java.lang/util/net/io surface portable cljc code calls.
;; (java.time formatting is a separate increment.)
;;
;; Constructed host objects are `jhost` records (a tag + mutable state); their
;; (.method ...) calls reach record-method-dispatch (records.ss), extended below
;; with a jhost arm that dispatches through host-tagged-methods.
;;
;; Loaded from rt.ss LAST (after natives-str.ss / records.ss): it extends
;; record-method-dispatch and reuses jolt-str-render-one / jolt-re-pattern.

;; ---- registries -------------------------------------------------------------
(define class-statics-tbl (make-hashtable string-hash string=?))   ; "Class" -> (member-ht)
(define class-ctors-tbl   (make-hashtable string-hash string=?))   ; "Class" -> ctor proc
(define host-methods-tbl  (make-hashtable string-hash string=?))   ; tag -> (method-ht)

;; A class token may arrive fully qualified (java.io.StringReader) or short
;; (StringReader). Register both; resolve by exact then by last dotted segment.
(define (short-class-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

(define (register-class-statics! name members)  ; members: list of (str . val/proc)
  (let ((h (or (hashtable-ref class-statics-tbl name #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! class-statics-tbl name h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

(define (register-class-ctor! name proc) (hashtable-set! class-ctors-tbl name proc))

(define (register-host-methods! tag members)
  (let ((h (or (hashtable-ref host-methods-tbl tag #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! host-methods-tbl tag h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

(define (lookup-class h-tbl name)
  (or (hashtable-ref h-tbl name #f)
      (hashtable-ref h-tbl (short-class-name name) #f)))

;; ---- host object ------------------------------------------------------------
(define-record-type jhost (fields tag (mutable state)) (nongenerative chez-jhost-v1))

;; record-method-dispatch (records.ss) gets a jhost arm: dispatch (.method obj a*)
;; through the tag's method table.
(define %hs-record-method-dispatch record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (cond
      ;; (.getClass x) is universal — the class token for any value (incl. numbers
      ;; / jhost) — before the per-type arms that would otherwise reject it.
      ((string=? method-name "getClass") (jolt-class obj))
      ((jhost? obj)
       (let ((mh (hashtable-ref host-methods-tbl (jhost-tag obj) #f)))
         (let ((f (and mh (hashtable-ref mh method-name #f))))
           (if f
               (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (error #f (string-append "No method " method-name " on host " (jhost-tag obj)))))))
      ((number? obj) (number-method method-name obj))
      (else (%hs-record-method-dispatch obj method-name rest-args)))))

;; java.lang.Number method surface (the boxed-number methods cljc code calls). The
;; integer projections wrap modulo their width (ring-codec relies on byteValue
;; overflow: (.byteValue 255) => -1); the float projections are identity flonums.
(define (number-method method n)
  (cond
    ((string=? method "byteValue") (let ((b (modulo (jnum->exact n) 256))) (->num (if (>= b 128) (- b 256) b))))
    ((string=? method "shortValue") (let ((b (modulo (jnum->exact n) 65536))) (->num (if (>= b 32768) (- b 65536) b))))
    ((string=? method "intValue") (->num (jnum->exact n)))
    ((string=? method "longValue") (->num (jnum->exact n)))
    ((string=? method "doubleValue") (->num n))
    ((string=? method "floatValue") (->num n))
    ((string=? method "toString") (jolt-num->string n))
    ((string=? method "hashCode") (->num (jnum->exact n)))
    (else (error #f (string-append "No method " method " for number")))))

;; ---- emit entry points ------------------------------------------------------
(define (host-static-ref class member)
  (let ((h (lookup-class class-statics-tbl class)))
    (if h
        (let ((v (hashtable-ref h member #f)))
          (if v v (error #f (string-append "No static " class "/" member))))
        (error #f (string-append "Unknown class " class)))))

(define (host-static-call class member . args)
  (apply (host-static-ref class member) args))

(define (host-new class . args)
  (let ((ctor (lookup-class class-ctors-tbl class)))
    (cond
      (ctor (apply ctor args))
      ;; deftype/defrecord: the type name is bound as a VAR (the
      ;; make-deftype-ctor closure) in its defining ns, not a registered host class.
      ;; Resolve it in the current ns / clojure.core and invoke it — so (P. args)
      ;; works the same as the ->P factory.
      (else
       (let ((cell (or (var-cell-lookup (chez-current-ns) class)
                       (var-cell-lookup "clojure.core" class))))
         (if (and cell (var-cell-defined? cell) (procedure? (var-cell-root cell)))
             (apply (var-cell-root cell) args)
             (error #f (string-append "No constructor for class " class))))))))

;; ---- coercion helpers -------------------------------------------------------
;; numeric tower: currentTimeMillis/nanoTime are exact longs (JVM).
(define (->num x) x)
(define (jnum->exact n) (exact (truncate n)))
;; parse an integer string in radix; #f on failure
(define (parse-int-str s radix)
  (let ((n (string->number (str-trim (if (string? s) s (jolt-str-render-one s))) radix)))
    (and n (integer? n) (->num n))))
(define (parse-int-or-throw s radix what)
  (or (parse-int-str s radix)
      (error #f (string-append "NumberFormatException: For input string: \""
                               (if (string? s) s (jolt-str-render-one s)) "\""))))
(define (char-code c) (if (char? c) (char->integer c) (jnum->exact c)))

;; parse a double string (Double/parseDouble, (Double. s)); JVM accepts NaN /
;; Infinity / decimal / scientific. #f on failure.
(define (parse-double-str s)
  (let ((t (str-trim (if (string? s) s (jolt-str-render-one s)))))
    (cond
      ((or (string=? t "NaN") (string=? t "+NaN") (string=? t "-NaN")) +nan.0)
      ((or (string=? t "Infinity") (string=? t "+Infinity")) +inf.0)
      ((string=? t "-Infinity") -inf.0)
      (else (let ((n (string->number t))) (and n (real? n) (exact->inexact n)))))))
(define (parse-double-or-throw s)
  (or (parse-double-str s)
      (error #f (string-append "NumberFormatException: For input string: \""
                               (if (string? s) s (jolt-str-render-one s)) "\""))))
(define (->double x) (if (number? x) (exact->inexact x) (parse-double-or-throw x)))

;; ---- java.lang statics ------------------------------------------------------
;; java.lang.Math: sqrt/pow/floor/ceil/trig/log/exp always return a DOUBLE on the
;; JVM (Chez's sqrt/expt return EXACT for exact args, e.g. (sqrt 9) -> 3), so coerce
;; to flonum. round -> long (exact); abs/max/min preserve the argument's type.
(define (->dbl x) (exact->inexact x))
(register-class-statics! "Math"
  (list (cons "sqrt" (lambda (x) (->dbl (sqrt x))))
        (cons "pow" (lambda (a b) (->dbl (expt a b))))
        (cons "floor" (lambda (x) (->dbl (floor x))))
        (cons "ceil" (lambda (x) (->dbl (ceiling x))))
        (cons "round" (lambda (x) (exact (floor (+ x 1/2)))))   ; JVM round-half-up -> long
        (cons "abs" (lambda (x) (abs x)))
        (cons "sin" (lambda (x) (->dbl (sin x)))) (cons "cos" (lambda (x) (->dbl (cos x))))
        (cons "tan" (lambda (x) (->dbl (tan x)))) (cons "asin" (lambda (x) (->dbl (asin x))))
        (cons "acos" (lambda (x) (->dbl (acos x)))) (cons "atan" (lambda (x) (->dbl (atan x))))
        (cons "log" (lambda (x) (->dbl (log x)))) (cons "log10" (lambda (x) (->dbl (/ (log x) (log 10)))))
        (cons "exp" (lambda (x) (->dbl (exp x))))
        (cons "max" (lambda (a b) (if (> a b) a b))) (cons "min" (lambda (a b) (if (< a b) a b)))
        (cons "signum" (lambda (x) (cond ((< x 0) -1.0) ((> x 0) 1.0) (else 0.0))))
        (cons "PI" (->dbl (* 4 (atan 1)))) (cons "E" (->dbl (exp 1)))
        (cons "random" (lambda args (random 1.0)))))

;; Thread: real OS threads back futures/promises.
;;  - sleep parks the calling thread for `ms` ms (a worker sleeping doesn't block
;;    the parent).
;;  - yield hands the CPU to another runnable thread (libc sched_yield).
;;  - each thread carries an interrupt flag; interrupted (static) reads AND clears
;;    the current thread's flag, matching the JVM. currentThread / .interrupt /
;;    .isInterrupted are wired in io.ss, where the thread handle is built.

;; Per-thread interrupt flag, lazily allocated so each OS thread gets its own box.
;; A thread handle (from currentThread) captures this box, so .interrupt from
;; another thread sets the target thread's flag.
(define thread-interrupt-box (make-thread-parameter #f))
(define (current-interrupt-box)
  (or (thread-interrupt-box)
      (let ((b (box #f))) (thread-interrupt-box b) b)))
(define (clear-thread-interrupt!) (set-box! (current-interrupt-box) #f))

;; libc sched_yield, resolved once; fall back to a zero-length park if the symbol
;; isn't available.
(define thread-yield!
  (let ((fp #f) (tried? #f))
    (lambda ()
      (unless tried?
        (set! tried? #t)
        (set! fp (guard (e (#t #f))
                   (load-shared-object #f)
                   (foreign-procedure "sched_yield" () int))))
      (if fp (fp) (sleep (make-time 'time-duration 0 0)))
      jolt-nil)))

(define thread-statics
  (list (cons "sleep" (lambda (ms . _)
                        (let* ((ms* (exact (floor ms)))
                               (secs (quotient ms* 1000))
                               (nanos (* (remainder ms* 1000) 1000000)))
                          (sleep (make-time 'time-duration nanos secs)))
                        jolt-nil))
        (cons "yield" (lambda _ (thread-yield!)))
        (cons "interrupted" (lambda _ (let* ((b (current-interrupt-box)) (v (unbox b)))
                                        (set-box! b #f) (and v #t))))))
(register-class-statics! "Thread" thread-statics)
(register-class-statics! "java.lang.Thread" thread-statics)

;; clojure.lang.LockingTransaction: jolt has no STM (no refs/dosync), so a
;; transaction is never running. isRunning -> false.
(register-class-statics! "LockingTransaction" (list (cons "isRunning" (lambda () #f))))
(register-class-statics! "clojure.lang.LockingTransaction" (list (cons "isRunning" (lambda () #f))))

;; clojure.lang.LazilyPersistentVector/createOwning: build a vector from an array
;; (malli's -vmap fills an object-array then hands it over). jolt has no array
;; ownership transfer, so copy the array's elements into a persistent vector.
(define (lpv-create-owning arr) (apply jolt-vector (seq->list (jolt-seq arr))))
(register-class-statics! "LazilyPersistentVector" (list (cons "createOwning" lpv-create-owning)))
(register-class-statics! "clojure.lang.LazilyPersistentVector" (list (cons "createOwning" lpv-create-owning)))

;; clojure.lang.PersistentArrayMap/createWithCheck: build a map from a [k v k v…]
;; array, throwing on a duplicate key. malli's eager entry parser relies on the
;; throw to report ::duplicate-keys, so a missing class would mis-fire on every
;; map. Build the map and signal if a key collapsed (count*2 < array length).
(define (pam-create-with-check arr)
  (let ((items (seq->list (jolt-seq arr))))
    (let loop ((xs items) (m (jolt-hash-map)))
      (if (null? xs) m
          (if (null? (cdr xs)) (error #f "PersistentArrayMap: odd key/value count")
              (let ((k (car xs)))
                (if (jolt-contains? m k) (error #f "Duplicate key")
                    (loop (cddr xs) (jolt-assoc m k (cadr xs))))))))))
(register-class-statics! "PersistentArrayMap" (list (cons "createWithCheck" pam-create-with-check)))
(register-class-statics! "clojure.lang.PersistentArrayMap" (list (cons "createWithCheck" pam-create-with-check)))

(define (now-millis)
  (let ((t (current-time 'time-utc)))
    (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))

;; clojure.core/current-time-ms — epoch milliseconds; backs the `time` macro.
(def-var! "clojure.core" "current-time-ms" (lambda () (->num (now-millis))))
(register-class-statics! "System"
  (list (cons "currentTimeMillis" (lambda () (->num (now-millis))))
        (cons "nanoTime" (lambda () (->num (* 1000000 (now-millis)))))
        (cons "exit" (lambda args (exit (if (null? args) 0 (jnum->exact (car args))))))
        ;; wrapped in lambdas: the helpers are defined below, resolved at call time.
        (cons "getProperty" (lambda (k . d) (apply sys-get-property k d)))
        (cons "setProperty" (lambda (k v) (sys-set-property k v)))
        (cons "clearProperty" (lambda (k) (sys-clear-property k)))
        (cons "getProperties" (lambda () (sys-properties-map)))
        (cons "getenv" (lambda k (apply sys-getenv k)))))

(register-class-statics! "Long"
  (list (cons "MAX_VALUE" (->num 9223372036854775807))
        (cons "MIN_VALUE" (->num -9223372036854775808))
        (cons "parseLong" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "parseLong")))
        (cons "valueOf" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "valueOf")))))

(register-class-statics! "Integer"
  (list (cons "MAX_VALUE" (->num 2147483647)) (cons "MIN_VALUE" (->num -2147483648))
        (cons "valueOf" (lambda (x . r)
                          (if (number? x) (->num x)
                              (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "valueOf"))))
        (cons "parseInt" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "parseInt")))))

(register-class-statics! "Boolean"
  (list (cons "parseBoolean" (lambda (s) (string=? "true" (ascii-string-down (if (string? s) s (jolt-str-render-one s))))))
        (cons "TRUE" #t) (cons "FALSE" #f)))

(register-class-ctor! "Double" ->double)
(register-class-ctor! "Float" ->double)
(register-class-statics! "Double"
  (list (cons "parseDouble" parse-double-or-throw)
        (cons "valueOf" ->double)
        (cons "toString" (lambda (x) (jolt-str-render-one (->double x))))
        (cons "isNaN" (lambda (x) (and (flonum? x) (nan? x))))
        (cons "isInfinite" (lambda (x) (and (flonum? x) (infinite? x))))
        (cons "MAX_VALUE" 1.7976931348623157e308) (cons "MIN_VALUE" 4.9e-324)
        (cons "POSITIVE_INFINITY" +inf.0) (cons "NEGATIVE_INFINITY" -inf.0) (cons "NaN" +nan.0)))
(register-class-statics! "Float"
  (list (cons "parseFloat" parse-double-or-throw) (cons "valueOf" ->double)))

;; Character: ASCII predicates (the engine is byte/ASCII oriented).
(register-class-statics! "Character"
  (list (cons "isUpperCase" (lambda (c) (let ((n (char-code c))) (and (>= n 65) (<= n 90)))))
        (cons "isLowerCase" (lambda (c) (let ((n (char-code c))) (and (>= n 97) (<= n 122)))))
        (cons "isDigit" (lambda (c) (let ((n (char-code c))) (and (>= n 48) (<= n 57)))))
        (cons "isWhitespace" (lambda (c) (char<=? (integer->char (char-code c)) #\space)))))

;; String/valueOf(Object): "null" for nil, else jolt's str semantics.
;; String/format(fmt args…) / (locale fmt args…) -> the clojure.core format engine.
(register-class-statics! "String"
  (list (cons "valueOf" (lambda (x . _) (if (jolt-nil? x) "null" (jolt-str-render-one x))))
        (cons "format" (lambda (a . rest)
                         (if (and (jhost? a) (string=? (jhost-tag a) "locale"))
                             (apply jolt-format (car rest) (cdr rest))
                             (apply jolt-format a rest))))))

;; ---- java.text.NumberFormat -------------------------------------------------
;; A grouping decimal formatter (selmer number-format / cuerdas). state:
;; #(grouping? min-frac max-frac). .format groups the integer part with commas.
(define (nf-make grouping? minf maxf) (make-jhost "numberformat" (vector grouping? minf maxf)))
(define (group-int-str s)               ; "1234567" -> "1,234,567"
  (let* ((neg (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
         (digs (if neg (substring s 1 (string-length s)) s))
         (n (string-length digs)) (out '()))
    (let loop ((i 0))
      (when (< i n)
        (when (and (> i 0) (= 0 (modulo (- n i) 3))) (set! out (cons #\, out)))
        (set! out (cons (string-ref digs i) out)) (loop (+ i 1))))
    (string-append (if neg "-" "") (list->string (reverse out)))))
(define (nf-format self x)
  (let* ((grouping? (vector-ref (jhost-state self) 0))
         (minf (vector-ref (jhost-state self) 1)) (maxf (vector-ref (jhost-state self) 2))
         (neg (< x 0)) (ax (abs (exact->inexact x)))
         (scale (expt 10 maxf))
         (scaled (exact (round (* ax scale))))
         (ipart (quotient scaled scale)) (fpart (remainder scaled scale))
         (istr (number->string ipart))
         (fstr0 (if (> maxf 0) (let ((s (number->string fpart)))
                                 (string-append (make-string (max 0 (- maxf (string-length s))) #\0) s)) ""))
         ;; trim trailing zeros down to minf
         (fstr (let loop ((s fstr0)) (if (and (> (string-length s) minf)
                                              (char=? (string-ref s (- (string-length s) 1)) #\0))
                                         (loop (substring s 0 (- (string-length s) 1))) s))))
    (string-append (if neg "-" "") (if grouping? (group-int-str istr) istr)
                   (if (> (string-length fstr) 0) (string-append "." fstr) ""))))
(register-host-methods! "numberformat"
  (list (cons "format" (lambda (self n) (nf-format self n)))
        (cons "setMaximumFractionDigits" (lambda (self d) (vector-set! (jhost-state self) 2 (jnum->exact d)) jolt-nil))
        (cons "setMinimumFractionDigits" (lambda (self d) (vector-set! (jhost-state self) 1 (jnum->exact d)) jolt-nil))
        (cons "setGroupingUsed" (lambda (self b) (vector-set! (jhost-state self) 0 (jolt-truthy? b)) jolt-nil))))
(register-class-statics! "NumberFormat"
  (list (cons "getInstance" (lambda _ (nf-make #t 0 3)))
        (cons "getNumberInstance" (lambda _ (nf-make #t 0 3)))
        (cons "getIntegerInstance" (lambda _ (nf-make #t 0 0)))))
(register-class-statics! "java.text.NumberFormat"
  (list (cons "getInstance" (lambda _ (nf-make #t 0 3)))
        (cons "getNumberInstance" (lambda _ (nf-make #t 0 3)))
        (cons "getIntegerInstance" (lambda _ (nf-make #t 0 0)))))

(register-class-statics! "Class"
  ;; an array descriptor ("[C", "[I", …) is its own class token (so instance? and
  ;; class compare equal); other names become a class jhost.
  (list (cons "forName" (lambda (nm)
                          (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\[))
                              nm
                              (make-jhost "class" (list (cons 'name nm))))))))

;; ---- System helpers (defined before use above via top-level order) ----------
;; os.name reflects the actual platform (Chez's machine-type names it): a *osx
;; machine is macOS, otherwise Linux. Code that branches on the OS (socket struct
;; layout, path handling) needs the truth, not a fixed value.
(define (substring-index needle hay)
  (let ((nl (string-length needle)) (hl (string-length hay)))
    (let loop ((i 0)) (cond ((> (+ i nl) hl) #f)
                            ((string=? (substring hay i (+ i nl)) needle) i)
                            (else (loop (+ i 1)))))))
(define sys-os-name
  (let ((m (symbol->string (machine-type))))
    (cond ((or (substring-index "osx" m) (substring-index "macos" m)) "Mac OS X")
          ((or (substring-index "nt" m) (substring-index "windows" m)) "Windows")
          (else "Linux"))))
;; runtime-settable system properties (System/setProperty). A set value wins over
;; the built-in defaults below; clearProperty removes it.
(define sys-prop-table (make-hashtable string-hash string=?))
(define (sys-set-property k v)
  (let ((prev (hashtable-ref sys-prop-table k jolt-nil)))
    (hashtable-set! sys-prop-table k (if (string? v) v (jolt-str-render-one v)))
    prev))
(define (sys-clear-property k)
  (let ((prev (hashtable-ref sys-prop-table k jolt-nil)))
    (hashtable-delete! sys-prop-table k) prev))
(define (sys-get-property k . dflt)
  (let ((set-val (hashtable-ref sys-prop-table k #f)))
    (cond (set-val set-val)
          ((string=? k "os.name") sys-os-name)
          ((string=? k "line.separator") "\n")
          ((string=? k "file.separator") "/")
          ((string=? k "path.separator") ":")
          ((string=? k "user.dir") (or (getenv "PWD") "."))
          ((string=? k "user.home") (or (getenv "HOME") ""))
          ((string=? k "java.io.tmpdir") (or (getenv "TMPDIR") "/tmp"))
          ((pair? dflt) (car dflt))
          (else jolt-nil))))
(define (sys-properties-map)
  (jolt-hash-map "os.name" sys-os-name "line.separator" "\n" "file.separator" "/"
                 "user.dir" (or (getenv "PWD") ".") "user.home" (or (getenv "HOME") "")
                 "java.io.tmpdir" (or (getenv "TMPDIR") "/tmp")))

;; full environment as an alist of (name . value), via spawning `env`.
(define (all-env-pairs)
  (call-with-values
    (lambda () (open-process-ports "env" (buffer-mode block) (native-transcoder)))
    (lambda (stdin stdout stderr pid)
      (let loop ((acc '()))
        (let ((l (get-line stdout)))
          (if (eof-object? l) (reverse acc)
              (let ((eq (let scan ((i 0)) (cond ((= i (string-length l)) #f)
                                                ((char=? (string-ref l i) #\=) i)
                                                (else (scan (+ i 1)))))))
                (loop (if eq (cons (cons (substring l 0 eq) (substring l (+ eq 1) (string-length l))) acc) acc)))))))))
;; JOLT_BAKE_ENV_ALLOWLIST: when set, only the listed comma-separated
;; names are served; unset (the normal case) reads are live and unfiltered.
(define (env-allowlist)
  (let ((a (getenv "JOLT_BAKE_ENV_ALLOWLIST")))
    (and a (map str-trim (str-literal-split a ",")))))
(define (sys-getenv . k)
  (let ((allow (env-allowlist)))
    (if (null? k)
        (apply jolt-hash-map
          (let loop ((ps (all-env-pairs)) (acc '()))
            (cond ((null? ps) (reverse acc))
                  ((and allow (not (member (caar ps) allow))) (loop (cdr ps) acc))
                  (else (loop (cdr ps) (cons (cdar ps) (cons (caar ps) acc)))))))
        (let ((name (car k)))
          (if (and allow (not (member name allow))) jolt-nil
              (let ((v (getenv name))) (if v v jolt-nil)))))))

;; ---- StringBuilder ----------------------------------------------------------
;; state: a box (1-vector) holding the accumulated string.
(define (sb-str self) (vector-ref (jhost-state self) 0))
(define (sb-set! self s) (vector-set! (jhost-state self) 0 s))
(define (render-piece x)
  (cond ((jolt-nil? x) "null") ((char? x) (string x)) ((string? x) x)
        (else (jolt-str-render-one x))))
;; (Object.) — a fresh value with distinct identity (libraries use it as a lock
;; or a unique sentinel). Each call returns a new jhost so identical?/= separate.
(register-class-ctor! "Object" (lambda _ (make-jhost "object" (vector))))

;; ---- java.util.ArrayList ----------------------------------------------------
;; A mutable list backed by a Scheme list in a box. medley's stateful transducers
;; (window / partition-between) build one with .add / .size / .toArray / .clear /
;; .remove. (ArrayList.) | (ArrayList. n) | (ArrayList. coll).
(define (al-list self) (vector-ref (jhost-state self) 0))
(define (al-set! self xs) (vector-set! (jhost-state self) 0 xs))
(define (make-arraylist xs) (make-jhost "arraylist" (vector xs)))
(register-class-ctor! "ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))   ; initial capacity, ignored
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(register-class-ctor! "java.util.ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(define (al-remove-at xs i)
  (let loop ((xs xs) (i i) (acc '()))
    (cond ((null? xs) (reverse acc))
          ((= i 0) (append (reverse acc) (cdr xs)))
          (else (loop (cdr xs) (- i 1) (cons (car xs) acc))))))
(register-host-methods! "arraylist"
  (list
    (cons "add" (lambda (self . a)
                  ;; (.add x) -> append+true; (.add i x) -> insert at i, returns nil.
                  (if (= 1 (length a))
                      (begin (al-set! self (append (al-list self) (list (car a)))) #t)
                      (let ((i (jnum->exact (car a))) (x (cadr a)) (xs (al-list self)))
                        (al-set! self (append (list-head xs i) (list x) (list-tail xs i))) jolt-nil))))
    (cons "add!" (lambda (self x) (al-set! self (append (al-list self) (list x))) #t))
    (cons "get" (lambda (self i) (list-ref (al-list self) (jnum->exact i))))
    (cons "set" (lambda (self i x)
                  (let* ((xs (al-list self)) (idx (jnum->exact i)) (old (list-ref xs idx)))
                    (al-set! self (append (list-head xs idx) (list x) (list-tail xs (+ idx 1)))) old)))
    (cons "size" (lambda (self) (->num (length (al-list self)))))
    (cons "isEmpty" (lambda (self) (null? (al-list self))))
    (cons "remove" (lambda (self i)
                     (let* ((xs (al-list self)) (idx (jnum->exact i)) (old (list-ref xs idx)))
                       (al-set! self (al-remove-at xs idx)) old)))
    (cons "clear" (lambda (self) (al-set! self '()) jolt-nil))
    (cons "contains" (lambda (self x) (and (memp (lambda (e) (jolt=2 e x)) (al-list self)) #t)))
    (cons "toArray" (lambda (self . _) (apply jolt-vector (al-list self))))
    (cons "iterator" (lambda (self) (make-jiterator (list->cseq (al-list self)))))
    (cons "toString" (lambda (self) (jolt-pr-str (list->cseq (al-list self)))))))

(register-class-ctor! "StringBuilder"
  (lambda args (make-jhost "string-builder"
    ;; a numeric first arg is a CAPACITY hint, not content.
    (vector (if (and (pair? args) (not (number? (car args)))) (render-piece (car args)) "")))))
(register-host-methods! "string-builder"
  (list (cons "append" (lambda (self x) (sb-set! self (string-append (sb-str self) (render-piece x))) self))
        (cons "toString" (lambda (self) (sb-str self)))
        (cons "length" (lambda (self) (->num (string-length (sb-str self)))))
        (cons "charAt" (lambda (self i) (string-ref (sb-str self) (jnum->exact i))))
        (cons "setLength" (lambda (self n)
                            (let ((cur (sb-str self)) (n (jnum->exact n)))
                              (sb-set! self (if (< n (string-length cur))
                                                (substring cur 0 n)
                                                (string-append cur (make-string (- n (string-length cur)) #\nul)))))
                            jolt-nil))))

;; ---- StringWriter -----------------------------------------------------------
;; Writer.write(int) writes the CHAR for that code; append(char) appends the char.
(define (writer-piece x) (if (number? x) (string (integer->char (jnum->exact x))) (render-piece x)))
(register-class-ctor! "StringWriter" (lambda args (make-jhost "writer" (vector ""))))
(register-host-methods! "writer"
  (list (cons "write" (lambda (self x) (sb-set! self (string-append (sb-str self) (writer-piece x))) jolt-nil))
        (cons "append" (lambda (self x) (sb-set! self (string-append (sb-str self) (render-piece x))) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) (sb-str self)))))

;; a file-backed writer (clojure.java.io/writer of a File/path): accumulates like
;; StringWriter, then persists to the path on flush/close, so
;; (with-open [w (io/writer "f")] (.write w …)) writes the file. State #(path buf).
(define (fw-path self) (vector-ref (jhost-state self) 0))
(define (fw-buf self) (vector-ref (jhost-state self) 1))
(define (fw-append! self s) (vector-set! (jhost-state self) 1 (string-append (fw-buf self) s)))
(define (fw-flush! self) (jolt-spit (fw-path self) (fw-buf self)))  ; jolt-spit: io.ss
(register-host-methods! "file-writer"
  (list (cons "write" (lambda (self x) (fw-append! self (writer-piece x)) jolt-nil))
        (cons "append" (lambda (self x) (fw-append! self (render-piece x)) self))
        (cons "flush" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "close" (lambda (self) (fw-flush! self) jolt-nil))
        (cons "toString" (lambda (self) (fw-buf self)))))

;; a writer over a real Chez port — the values *out* / *err* hold. write/append
;; push to the port (so (.write *out* s) and (binding [*out* *err*] …) work);
;; it isn't a buffer, so toString is empty. Lets libraries that touch *out*/*err*
;; (tools.logging, selmer) compile and run.
(register-host-methods! "port-writer"
  (list (cons "write" (lambda (self x) (display (writer-piece x) (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "append" (lambda (self x) (display (render-piece x) (vector-ref (jhost-state self) 0)) self))
        (cons "flush" (lambda (self) (flush-output-port (vector-ref (jhost-state self) 0)) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "toString" (lambda (self) ""))))
(def-var! "clojure.core" "*out*" (make-jhost "port-writer" (vector (current-output-port))))
(def-var! "clojure.core" "*err*" (make-jhost "port-writer" (vector (current-error-port))))

;; ---- java.util.HashMap ------------------------------------------------------
;; A mutable map keyed by jolt values (jolt-hash / jolt=2). State #(chez-hashtable).
;; Constructors: () | (capacity) | (capacity load-factor) [sizing args ignored] |
;; (Map m) [copy]. Enough of the Map surface for libraries that build a fast lookup
;; (malli's fast-registry: (doto (HashMap. 1024 0.25) (.putAll m)) then .get).
(define (hm-hash k) (let ((h (jolt-hash k)))
                      (bitwise-and (if (and (integer? h) (exact? h)) (abs h) 0) #x3FFFFFFF)))
(define (hm-tbl self) (vector-ref (jhost-state self) 0))
(define (hm-hashmap? x) (and (jhost? x) (string=? (jhost-tag x) "hashmap")))
(define (hm-copy-into! ht src)            ; src: a jolt map or another hashmap
  (if (hm-hashmap? src)
      (vector-for-each (lambda (k) (hashtable-set! ht k (hashtable-ref (hm-tbl src) k jolt-nil)))
                       (hashtable-keys (hm-tbl src)))
      (for-each (lambda (e) (hashtable-set! ht (jolt-nth e 0) (jolt-nth e 1)))
                (seq->list (jolt-seq src)))))
(register-class-ctor! "HashMap"
  (lambda args
    (let ((ht (make-hashtable hm-hash jolt=2)))
      (when (and (pair? args) (or (pmap? (car args)) (hm-hashmap? (car args))))
        (hm-copy-into! ht (car args)))
      (make-jhost "hashmap" (vector ht)))))
(define (hm->pmap self)
  (let ((m (jolt-hash-map)))
    (vector-for-each (lambda (k) (set! m (jolt-assoc m k (hashtable-ref (hm-tbl self) k jolt-nil))))
                     (hashtable-keys (hm-tbl self)))
    m))
(register-host-methods! "hashmap"
  (list (cons "put" (lambda (self k v) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                          (hashtable-set! (hm-tbl self) k v) old)))
        (cons "get" (lambda (self k) (hashtable-ref (hm-tbl self) k jolt-nil)))
        (cons "getOrDefault" (lambda (self k d) (hashtable-ref (hm-tbl self) k d)))
        (cons "containsKey" (lambda (self k) (if (hashtable-contains? (hm-tbl self) k) #t #f)))
        (cons "containsValue" (lambda (self v)
          (let ((found #f))
            (vector-for-each (lambda (k) (when (jolt=2 v (hashtable-ref (hm-tbl self) k jolt-nil)) (set! found #t)))
                             (hashtable-keys (hm-tbl self))) found)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "remove" (lambda (self k) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                           (hashtable-delete! (hm-tbl self) k) old)))
        (cons "clear" (lambda (self) (hashtable-clear! (hm-tbl self)) jolt-nil))
        (cons "putAll" (lambda (self m) (hm-copy-into! (hm-tbl self) m) jolt-nil))
        (cons "keySet" (lambda (self) (apply jolt-hash-set (vector->list (hashtable-keys (hm-tbl self))))))
        (cons "values" (lambda (self) (apply jolt-vector
                          (map (lambda (k) (hashtable-ref (hm-tbl self) k jolt-nil))
                               (vector->list (hashtable-keys (hm-tbl self)))))))
        (cons "entrySet" (lambda (self) (jolt-seq (hm->pmap self))))
        (cons "toString" (lambda (self) (jolt-pr-str (hm->pmap self))))))

;; ---- StringReader -----------------------------------------------------------
;; state: a vector #(string pos marked).
(register-class-ctor! "StringReader"
  ;; src is a String or a char[] ((StringReader. (char-array s)) — selmer's parser
  ;; reads templates this way); a char-array becomes the string of its chars.
  (lambda (src . _)
    (make-jhost "string-reader"
      (vector (cond ((string? src) src)
                    ((jolt-array? src) (apply string-append (map jolt-str-render-one (seq->list (jolt-seq src)))))
                    (else (jolt-str-render-one src)))
              0 0))))
(define (sr-s self) (vector-ref (jhost-state self) 0))
(define (sr-pos self) (vector-ref (jhost-state self) 1))
(define (sr-pos! self p) (vector-set! (jhost-state self) 1 p))
(register-host-methods! "string-reader"
  (list (cons "read" (lambda (self)
                       (let ((s (sr-s self)) (p (sr-pos self)))
                         (if (>= p (string-length s)) -1   ; EOF -> exact int -1 (= JVM)
                             (begin (sr-pos! self (+ p 1)) (->num (char->integer (string-ref s p))))))))
        (cons "mark" (lambda (self . _) (vector-set! (jhost-state self) 2 (sr-pos self)) jolt-nil))
        (cons "reset" (lambda (self) (sr-pos! self (vector-ref (jhost-state self) 2)) jolt-nil))
        (cons "skip" (lambda (self n) (let ((n (jnum->exact n)))
                                        (sr-pos! self (min (string-length (sr-s self)) (+ (sr-pos self) n))) (->num n))))
        ;; readLine: the next line without its terminator (\n or \r\n), nil at EOF —
        ;; what line-seq drives over a BufferedReader.
        (cons "readLine"
          (lambda (self)
            (let ((s (sr-s self)) (p (sr-pos self)) (len (string-length (sr-s self))))
              (if (>= p len) jolt-nil
                  (let scan ((i p))
                    (cond
                      ((>= i len) (sr-pos! self len) (substring s p len))
                      ((char=? (string-ref s i) #\newline)
                       (sr-pos! self (+ i 1))
                       (substring s p (if (and (> i p) (char=? (string-ref s (- i 1)) #\return)) (- i 1) i)))
                      (else (scan (+ i 1)))))))))
        (cons "close" (lambda (self) jolt-nil))))

;; ---- PushbackReader ---------------------------------------------------------
;; state: a vector #(wrapped-reader pushed-list)
(register-class-ctor! "PushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (vector rdr '()))))
(define (read-unit r)        ; read one code unit (flonum) from any reader, -1 at EOF
  (record-method-dispatch r "read" jolt-nil))
(register-host-methods! "pushback-reader"
  (list (cons "read" (lambda (self)
                       (let ((pushed (vector-ref (jhost-state self) 1)))
                         (if (pair? pushed)
                             (begin (vector-set! (jhost-state self) 1 (cdr pushed)) (car pushed))
                             (read-unit (vector-ref (jhost-state self) 0))))))
        (cons "unread" (lambda (self ch)
                         (vector-set! (jhost-state self) 1
                           (cons (if (char? ch) (->num (char->integer ch)) ch) (vector-ref (jhost-state self) 1)))
                         jolt-nil))
        (cons "close" (lambda (self) jolt-nil))))

;; ---- HashMap ----------------------------------------------------------------
;; state: a box holding an alist of (k . v), jolt= keyed.
(define (hm-alist self) (vector-ref (jhost-state self) 0))
(define (hm-set! self al) (vector-set! (jhost-state self) 0 al))
(define (hm-assoc al k v)
  (let loop ((ps al) (acc '()) (hit #f))
    (cond ((null? ps) (reverse (if hit acc (cons (cons k v) acc))))
          ((jolt=2 (caar ps) k) (loop (cdr ps) (cons (cons k v) acc) #t))
          (else (loop (cdr ps) (cons (car ps) acc) hit)))))
(define (hm-get al k) (let loop ((ps al)) (cond ((null? ps) jolt-nil) ((jolt=2 (caar ps) k) (cdar ps)) (else (loop (cdr ps))))))
(define (coll->pairs m)
  (if (jolt-nil? m) '()
      (let loop ((s (jolt-seq m)) (acc '()))
        (if (jolt-nil? s) (reverse acc)
            (let ((e (seq-first s))) (loop (jolt-seq (seq-more s)) (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc)))))))
(register-class-ctor! "HashMap"
  (lambda args
    (let ((init (and (pair? args) (car args))))
      (make-jhost "hashmap" (vector (if (and init (not (number? init))) (coll->pairs init) '()))))))
(register-host-methods! "hashmap"
  (list (cons "get" (lambda (self k) (hm-get (hm-alist self) k)))
        (cons "put" (lambda (self k v) (hm-set! self (hm-assoc (hm-alist self) k v)) v))
        (cons "putAll" (lambda (self m) (for-each (lambda (p) (hm-set! self (hm-assoc (hm-alist self) (car p) (cdr p)))) (coll->pairs m)) jolt-nil))
        (cons "containsKey" (lambda (self k) (not (jolt-nil? (hm-get (hm-alist self) k)))))
        (cons "size" (lambda (self) (->num (length (hm-alist self)))))))

;; ---- StringTokenizer --------------------------------------------------------
;; state: a vector #(tokens-list pos)
(define (tokenize s delims)
  (let ((dset (string->list delims)))
    (let loop ((chars (string->list s)) (cur '()) (toks '()))
      (cond ((null? chars) (reverse (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            ((memv (car chars) dset)
             (loop (cdr chars) '() (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            (else (loop (cdr chars) (cons (car chars) cur) toks))))))
(register-class-ctor! "StringTokenizer"
  (lambda (s . delims) (make-jhost "string-tokenizer"
    (vector (tokenize (if (string? s) s (jolt-str-render-one s))
                      (if (null? delims) " \t\n\r\f" (car delims))) 0))))
(register-host-methods! "string-tokenizer"
  (list (cons "hasMoreTokens" (lambda (self) (< (vector-ref (jhost-state self) 1) (length (vector-ref (jhost-state self) 0)))))
        (cons "countTokens" (lambda (self) (->num (- (length (vector-ref (jhost-state self) 0)) (vector-ref (jhost-state self) 1)))))
        (cons "nextToken" (lambda (self)
                            (let ((toks (vector-ref (jhost-state self) 0)) (p (vector-ref (jhost-state self) 1)))
                              (if (< p (length toks))
                                  (begin (vector-set! (jhost-state self) 1 (+ p 1)) (list-ref toks p))
                                  (error #f "NoSuchElementException")))))))

;; ---- String / BigInteger / MapEntry constructors ----------------------------
;; (String. bytes [charset]) decodes bytes (a bytevector OR a jolt byte-array)
;; with the named charset (UTF-8 default; ISO-8859-1/latin1/ascii = one byte per
;; char); else stringify. clj-http-lite's body coercion is (String. ^[B body cs).
(define (string-charset-name rest)
  (if (pair? rest)
      (let ((c (car rest)))
        (cond ((string? c) c)
              ((and (jhost? c) (string=? (jhost-tag c) "charset"))
               (let ((p (assq 'name (jhost-state c)))) (if p (jolt-str-render-one (cdr p)) "UTF-8")))
              (else "UTF-8")))
      "UTF-8"))
(define (decode-bytevector bv rest)
  (let ((cs (ascii-string-down (string-charset-name rest))))
    (cond
      ((or (string=? cs "utf-8") (string=? cs "utf8")) (utf8->string bv))
      ((or (string=? cs "iso-8859-1") (string=? cs "latin1") (string=? cs "iso8859-1")
           (string=? cs "us-ascii") (string=? cs "ascii"))
       (list->string (map integer->char (bytevector->u8-list bv))))
      ((or (string=? cs "utf-16") (string=? cs "utf16") (string=? cs "utf-16be") (string=? cs "unicode"))
       (utf16->string bv (endianness big)))   ; respects a leading BOM
      ((string=? cs "utf-16le") (utf16->string bv (endianness little)))
      ((or (string=? cs "utf-32") (string=? cs "utf32") (string=? cs "utf-32be"))
       (utf32->string bv (endianness big)))
      ((string=? cs "utf-32le") (utf32->string bv (endianness little)))
      (else (guard (e (#t (list->string (map integer->char (bytevector->u8-list bv))))) (utf8->string bv))))))
(register-class-ctor! "String"
  (lambda (x . rest)
    (cond ((bytevector? x) (decode-bytevector x rest))
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (decode-bytevector (na-bytearray->bv x) rest))
          ((string? x) x)
          (else (jolt-str-render-one x)))))
(register-class-ctor! "BigInteger"
  (lambda (v) (parse-int-or-throw v 10 "BigInteger")))
(register-class-ctor! "MapEntry" (lambda (k v) (make-map-entry k v)))
;; JVM exception ctors -> a typed host throwable carrying the canonical :jolt/class
;; (so class / instance? / getMessage / ex-message reflect the real type) and the
;; message. Supports (E. msg), (E. msg cause), (E. cause), and (E.).
(for-each
  (lambda (nm)
    (let ((canonical (or (resolve-class-hint nm) nm)))
      (register-class-ctor! nm
        (lambda args
          (let* ((a0 (if (pair? args) (car args) jolt-nil))
                 (rest (if (pair? args) (cdr args) '()))
                 (cause (if (pair? rest) (car rest) jolt-nil)))
            (cond
              ((string? a0) (jolt-host-throwable canonical a0 cause))
              ((jolt-nil? a0) (jolt-host-throwable canonical jolt-nil))
              ;; (E. cause): a lone throwable arg is the cause, message nil.
              ((and (null? rest) (ex-info-map? a0)) (jolt-host-throwable canonical jolt-nil a0))
              (else (jolt-host-throwable canonical (jolt-str-render-one a0) cause))))))))
  '("Throwable" "Exception" "RuntimeException" "IllegalArgumentException" "IllegalStateException"
    "InterruptedException" "UnsupportedOperationException" "IOException" "NumberFormatException"
    "ArithmeticException" "NullPointerException" "ClassCastException" "IndexOutOfBoundsException"
    "FileNotFoundException" "UnsupportedEncodingException"))

;; ---- URLEncoder / URLDecoder (www-form-urlencoded) --------------------------
(define (url-unreserved? b)
  (or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90)) (and (>= b 97) (<= b 122))
      (= b 46) (= b 42) (= b 95) (= b 45)))
(define hex-digits "0123456789ABCDEF")
(define (url-encode s . _)
  (let ((bs (string->utf8 (if (string? s) s (jolt-str-render-one s)))) (out '()))
    (let loop ((i 0))
      (if (= i (bytevector-length bs)) (list->string (reverse out))
          (let ((b (bytevector-u8-ref bs i)))
            (cond ((url-unreserved? b) (set! out (cons (integer->char b) out)))
                  ((= b 32) (set! out (cons #\+ out)))
                  (else (set! out (cons (string-ref hex-digits (bitwise-and b 15))
                                   (cons (string-ref hex-digits (bitwise-arithmetic-shift-right b 4))
                                     (cons #\% out))))))
            (loop (+ i 1)))))))
(define (hexv c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
        (else (error #f "URLDecoder: malformed escape"))))
(define (url-decode s . _)
  (let* ((str (if (string? s) s (jolt-str-render-one s))) (n (string-length str)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (utf8->string (u8-list->bytevector (reverse out)))
          (let ((c (string-ref str i)))
            (cond ((char=? c #\+) (set! out (cons 32 out)) (loop (+ i 1)))
                  ((char=? c #\%)
                   (set! out (cons (+ (* 16 (hexv (string-ref str (+ i 1)))) (hexv (string-ref str (+ i 2)))) out))
                   (loop (+ i 3)))
                  (else (set! out (cons (char->integer c) out)) (loop (+ i 1)))))))))
(define (u8-list->bytevector lst)
  (let ((bv (make-bytevector (length lst))))
    (let loop ((l lst) (i 0)) (if (null? l) bv (begin (bytevector-u8-set! bv i (car l)) (loop (cdr l) (+ i 1)))))))
(register-class-statics! "URLEncoder" (list (cons "encode" url-encode)))
(register-class-statics! "URLDecoder" (list (cons "decode" url-decode)))
;; Charset/forName yields the canonical name STRING (not an opaque object) so it
;; threads straight into (.getBytes s cs) / (String. bytes cs), which take a name.
(register-class-statics! "Charset" (list (cons "forName" (lambda (nm) (jolt-str-render-one nm)))))

;; ---- Base64 (RFC 4648) ------------------------------------------------------
(define b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define (->bytevector x)
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        ((string? x) (string->utf8 x))
        (else (string->utf8 (jolt-str-render-one x)))))
(define (b64-encode x)
  (let* ((bs (->bytevector x)) (n (bytevector-length bs)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (list->string (reverse out))
          (let* ((b0 (bytevector-u8-ref bs i))
                 (b1 (if (< (+ i 1) n) (bytevector-u8-ref bs (+ i 1)) #f))
                 (b2 (if (< (+ i 2) n) (bytevector-u8-ref bs (+ i 2)) #f)))
            (set! out (cons (string-ref b64-alphabet (bitwise-arithmetic-shift-right b0 2)) out))
            (set! out (cons (string-ref b64-alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b0 3) 4)
                                                                  (bitwise-arithmetic-shift-right (or b1 0) 4))) out))
            (set! out (cons (if b1 (string-ref b64-alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 15) 2)
                                                                         (bitwise-arithmetic-shift-right (or b2 0) 6))) #\=) out))
            (set! out (cons (if b2 (string-ref b64-alphabet (bitwise-and b2 63)) #\=) out))
            (loop (+ i 3)))))))
(define (b64-char-val c)
  (let loop ((i 0)) (cond ((= i 64) (error #f "Base64: illegal character")) ((char=? (string-ref b64-alphabet i) c) i) (else (loop (+ i 1))))))
(define (b64-decode x)
  (let* ((str (let ((s (if (string? x) x (utf8->string (->bytevector x)))))
                (list->string (filter (lambda (c) (not (char=? c #\=))) (string->list s)))))
         (out '()) (acc 0) (bits 0))
    (for-each (lambda (c)
                (set! acc (bitwise-ior (bitwise-arithmetic-shift-left acc 6) (b64-char-val c)))
                (set! bits (+ bits 6))
                (when (>= bits 8)
                  (set! bits (- bits 8))
                  (set! out (cons (bitwise-and (bitwise-arithmetic-shift-right acc bits) 255) out))))
              (string->list str))
    (u8-list->bytevector (reverse out))))
(register-host-methods! "b64-encoder"
  (list (cons "encode" (lambda (self bs) (string->utf8 (b64-encode bs))))
        (cons "encodeToString" (lambda (self bs) (b64-encode bs)))))
(register-host-methods! "b64-decoder"
  (list (cons "decode" (lambda (self s) (b64-decode s)))))
(register-class-statics! "Base64"
  (list (cons "getEncoder" (lambda () (make-jhost "b64-encoder" '())))
        (cons "getDecoder" (lambda () (make-jhost "b64-decoder" '())))))

;; ---- java.util.regex.Pattern ------------------------------------------------
;; Pattern/compile returns a jolt-regex value (regex-t), so str/replace, re-find,
;; .split etc. accept it transparently.
(define pattern-multiline 8.0)
(define (pattern-quote s)
  (let ((meta "\\.[]{}()*+-?^$|&") (s (if (string? s) s (jolt-str-render-one s))) (out '()))
    (let loop ((i 0))
      (if (= i (string-length s)) (list->string (reverse out))
          (let ((c (string-ref s i)))
            (when (memv c (string->list meta)) (set! out (cons #\\ out)))
            (set! out (cons c out))
            (loop (+ i 1)))))))
(register-class-statics! "Pattern"
  (list (cons "compile" (lambda (s . flags)
                          (if (and (pair? flags) (= (bitwise-and (jnum->exact (car flags)) 8) 8))
                              (jolt-regex (string-append "(?m)" s))
                              (jolt-regex s))))
        (cons "quote" (lambda (s) (pattern-quote s)))
        (cons "MULTILINE" pattern-multiline)))
;; record-method-dispatch already routes string? -> jolt-string-method. Add a
;; regex-t arm (Pattern .split / .matcher-less surface used by corpus) by wrapping
;; once more — a regex-t isn't a jhost.
(define %hs-rmd2 record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (if (regex-t? obj)
        (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
          (cond ((string=? method-name "split")
                 ;; .split returns a String[] — a seq (prints
                 ;; (a b c), not a vector). re-split with no limit; drop trailing
                 ;; empties (JVM default).
                 (let ((parts (re-split (regex-t-irx obj) (car rest) #f)))
                   (list->cseq (str-split-drop-trailing parts))))
                ((string=? method-name "pattern") (regex-t-source obj))
                (else (error #f (string-append "No method " method-name " on Pattern")))))
        (%hs-rmd2 obj method-name rest-args))))

;; ---- def-var! the registry entry points so emit can also reach them ---------
(def-var! "clojure.core" "host-static-ref" host-static-ref)
(def-var! "clojure.core" "host-static-call" (lambda (c m . a) (apply host-static-call c m a)))
(def-var! "clojure.core" "host-new" (lambda (c . a) (apply host-new c a)))

;; Clojure-visible class-registration hooks. A host shim (e.g. reitit.trie-jolt,
;; which mirrors the reitit.Trie Java class) registers a constructor proc or a
;; map of static members against a class token so (Class. args) / (Class/member
;; args) resolve to it. The statics argument is a jolt map {member-name -> val}.
(define (jmap->static-alist m)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s) acc
        (let ((e (jolt-first s)))
          (loop (jolt-seq (jolt-rest s)) (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc))))))
(def-var! "clojure.core" "__register-class-ctor!"
  (lambda (name proc) (register-class-ctor! name proc) jolt-nil))
(def-var! "clojure.core" "__register-class-statics!"
  (lambda (name members) (register-class-statics! name (jmap->static-alist members)) jolt-nil))

;; ---- tagged-table method dispatch + pluggable instance? --------------------
;; A jolt library can build stateful host objects with (jolt.host/tagged-table
;; tag) and dispatch (.method obj ...) to handlers registered here, keyed by the
;; table's "jolt/type" tag — the htable analogue of the jhost method registry
;; above. jolt-lang/http-client uses this to emulate java.net URL /
;; HttpURLConnection / java.io byte streams so clj-http-lite runs unchanged.
(define tagged-methods-tbl (make-hashtable string-hash string=?))   ; tag-key -> (method-ht)
(define (tag->method-key tag)
  (if (keyword-t? tag)
      (let ((ns (keyword-t-ns tag)))
        (if (and ns (not (jolt-nil? ns))) (string-append ns "/" (keyword-t-name tag)) (keyword-t-name tag)))
      (jolt-str-render-one tag)))
(define (register-tagged-methods! tag members)
  (let* ((key (tag->method-key tag))
         (h (or (hashtable-ref tagged-methods-tbl key #f)
                (let ((nh (make-hashtable string-hash string=?)))
                  (hashtable-set! tagged-methods-tbl key nh) nh))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

;; htable arm: dispatch (.method obj a*) through the table's tag method registry;
;; an unregistered method falls through (sorted colls are htables too).
(define %hs-rmd-htable record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (let ((tag (and (htable? obj) (hashtable-ref (htable-h obj) "jolt/type" #f))))
      (let* ((mh (and tag (hashtable-ref tagged-methods-tbl (tag->method-key tag) #f)))
             (f  (and mh (hashtable-ref mh method-name #f))))
        (if f
            (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
            (%hs-rmd-htable obj method-name rest-args))))))

(def-var! "clojure.core" "__register-class-methods!"
  (lambda (tag members) (register-tagged-methods! tag (jmap->static-alist members)) jolt-nil))

;; Pluggable instance? — a library registers (fn [class-name-string val] -> true
;; | false | nil); nil means "not my class, fall through". First non-nil wins.
(define user-instance-checks '())
(define %hs-instance-check instance-check)
(set! instance-check
  (lambda (type-sym val)
    (let ((tname (symbol-t-name type-sym)))
      (let loop ((fs user-instance-checks))
        (if (null? fs)
            (%hs-instance-check type-sym val)
            (let ((r ((car fs) tname val)))
              (if (jolt-nil? r) (loop (cdr fs)) (if (jolt-truthy? r) #t #f))))))))
(def-var! "clojure.core" "instance-check" instance-check)
(def-var! "clojure.core" "__register-instance-check!"
  (lambda (f) (set! user-instance-checks (append user-instance-checks (list f))) jolt-nil))

;; (jolt.host/table? x) — is x a host tagged-table?
(def-var! "jolt.host" "table?" (lambda (x) (if (htable? x) #t #f)))