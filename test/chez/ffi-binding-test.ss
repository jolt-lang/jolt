;; jolt.ffi regression: a compile-time-typed foreign binding lowers to a real
;; Chez foreign-procedure and calls native code. Run:
;;   chez --script test/chez/ffi-binding-test.ss
;; Binds a few libc functions (process symbols, always present) through the
;; jolt.ffi/__cfn special form + the host memory primitives — the same path a
;; library uses to bind its native deps.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/java/ffi.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
;; eval one form (string) in `user`, like the loader does form-by-form, so a def
;; is visible to a later form.
(define (ev s) (jolt-compile-eval s "user"))

;; load libc (process symbols) and bind typed foreign functions
(ev "(jolt.ffi/load-library)")
(ev "(def c-strlen (jolt.ffi/__cfn \"strlen\" [:string] :size_t))")
(ev "(def c-abs (jolt.ffi/__cfn \"abs\" [:int] :int))")

(ok "foreign-procedure built for strlen" (procedure? (var-deref "user" "c-strlen")))

;; load-library contract: a failed named load RAISES. Regression — the scoped
;; native loader returned nil whether or not dlopen succeeded, so candidate
;; probes (try/catch around load-library, as in jolt.mvn-http) accepted the
;; first candidate loaded-or-not and fallback lists never advanced.
(ok "load-library raises on a nonexistent path"
    (jolt-truthy?
      (ev "(try (jolt.ffi/load-library \"/nonexistent-dir/libjolt-no-such.so\") false
                (catch :default e true))")))
(ok "load-library per-OS map raises on a nonexistent path"
    (jolt-truthy?
      (ev "(try (jolt.ffi/load-library {:darwin \"/nonexistent-dir/no.dylib\"
                                        :linux \"/nonexistent-dir/no.so\"
                                        :windows \"Z:/nonexistent-dir/no.dll\"}) false
                (catch :default e true))")))
(ok "typed call: strlen(\"hello\") = 5" (= 5 (jnum->exact (ev "(c-strlen \"hello\")"))))
(ok "typed call: abs(-7) = 7"          (= 7 (jnum->exact (ev "(c-abs -7)"))))

;; memory: alloc / write / read roundtrip through the host primitives
(ok "mem int roundtrip"
    (= 4242 (jnum->exact
              (ev "(let [p (jolt.ffi/alloc (jolt.ffi/sizeof :int))]
                     (jolt.ffi/write p :int 0 4242)
                     (let [v (jolt.ffi/read p :int)] (jolt.ffi/free p) v))"))))
(ok "sizeof :pointer is a word" (let ((n (jnum->exact (ev "(jolt.ffi/sizeof :pointer)")))) (or (= n 8) (= n 4))))

;; byte-array buffer I/O: write a byte-array into foreign memory and read it back
;; byte-exact (high bytes preserved, no UTF-8 mangling). Elements are SIGNED bytes
;; like the JVM's byte[], so a high byte reads negative and 0xff-masks back.
(ok "byte-array roundtrip (binary-faithful, signed elements)"
    (jolt-truthy?
      (ev "(let [src (byte-array [0 65 200 255 10])
                  p (jolt.ffi/alloc 5)]
              (jolt.ffi/write-array p src)
              (let [back (jolt.ffi/read-array p 5)]
                (jolt.ffi/free p)
                (and (= 5 (alength back))
                     (= [0 65 -56 -1 10] (vec back))
                     (= [0 65 200 255 10] (mapv #(bit-and % 0xff) back)))))")))

;; a :blocking foreign call is collect-safe: a thread parked in it must not pin
;; the stop-the-world collector. (collect) here would throw "cannot collect when
;; multiple threads are active" if usleep weren't emitted __collect_safe.
(ev "(def c-usleep (jolt.ffi/__cfn \"usleep\" [:uint] :int :blocking))")
(let ((usleep (var-deref "user" "c-usleep")))
  (fork-thread (lambda () (usleep 2000000)))           ; ~2s in a blocking call
  (let loop ((i 0)) (when (fx<? i 30000000) (loop (fx+ i 1))))  ; spin so the thread enters usleep
  (ok "blocking ffi call is collect-safe" (guard (e (#t #f)) (collect) #t)))

;; callbacks: receive a call FROM C. A foreign-callable wraps a jolt fn as a
;; C-callable function pointer (what GTK signal handlers / qsort comparators need).
;; Build a comparator and sort an int array through libc qsort.
(ev "(def cmp (jolt.ffi/__ccallable
                (fn [pa pb]
                  (let [a (jolt.ffi/read pa :int) b (jolt.ffi/read pb :int)]
                    (cond (< a b) -1 (> a b) 1 :else 0)))
                [:pointer :pointer] :int))")
(ok "foreign-callable returns a pointer"
    (let ((p (jnum->exact (var-deref "user" "cmp")))) (and (integer? p) (> p 0))))
(ev "(def c-qsort (jolt.ffi/__cfn \"qsort\" [:pointer :size_t :size_t :pointer] :void))")
(ok "C calls back into jolt: qsort with a jolt comparator"
    (jolt-truthy?
      (ev "(let [n 5 w (jolt.ffi/sizeof :int) p (jolt.ffi/alloc (* n w))]
             (doseq [[i v] (map vector (range n) [3 1 4 1 5])]
               (jolt.ffi/write p :int (* i w) v))
             (c-qsort p n w cmp)
             (let [out (mapv (fn [i] (jolt.ffi/read p :int (* i w))) (range n))]
               (jolt.ffi/free p)
               (= out [1 1 3 4 5])))")))
;; free-callable unlocks + drops the code object, returning nil.
(ok "free-callable releases the callback"
    (jolt-nil? (ev "(jolt.ffi/free-callable cmp)")))

;; variadic libc: fcntl is (int fd, int cmd, ...) — two fixed args, the flags
;; argument variadic. Apple arm64 passes variadic args on the stack, so only
;; the (__varargs_after 2) calling convention lands it — a fixed-arity binding
;; silently corrupts it (this test FAILED against the marker-last encoding
;; that emitted __varargs_after 3 = nothing variadic). :varargs marks the
;; fixed/variadic boundary. F_SETFL O_NONBLOCK on a real socket, read back
;; with F_GETFL.
(ev "(def c-socket (jolt.ffi/__cfn \"socket\" [:int :int :int] :int))")
(ev "(def c-close  (jolt.ffi/__cfn \"close\" [:int] :int))")
(ev "(def c-fcntl  (jolt.ffi/__cfn \"fcntl\" [:int :int :varargs :int] :int))")
(ev "(def f-getfl 3)")
(ev "(def f-setfl 4)")
;; O_NONBLOCK is 0x0004 on Darwin, 0x800 on Linux — gate on the os family so
;; this passes on both macOS dev machines and Linux CI.
(ev (string-append "(def o-nonblock "
                   (if (eq? (sa-os-family) 'linux) "2048" "4")
                   ")"))
(ok "variadic fcntl F_SETFL sets O_NONBLOCK"
    (jolt-truthy?
      (ev "(let [fd (c-socket 2 1 0)] (c-fcntl fd f-setfl o-nonblock) (let [after (c-fcntl fd f-getfl 0)] (c-close fd) (pos? (bit-and after o-nonblock))))")))
(ok "variadic fcntl F_SETFL can clear O_NONBLOCK again"
    (jolt-truthy?
      (ev "(let [fd (c-socket 2 1 0)] (c-fcntl fd f-setfl o-nonblock) (c-fcntl fd f-setfl 0) (let [after (c-fcntl fd f-getfl 0)] (c-close fd) (zero? (bit-and after o-nonblock))))")))

;; malformed :varargs shapes reject at compile with a message that names the
;; rule — a marker first (C needs a named parameter), a trailing marker
;; (nothing variadic), and :blocking (no convention combining)
(define (rejects? s)
  (call/cc (lambda (k)
    (with-exception-handler (lambda (e) (k #t))
      (lambda () (ev s) #f)))))
(ok ":varargs first rejects"
    (rejects? "(jolt.ffi/__cfn \"fcntl\" [:varargs :int] :int)"))
(ok ":varargs last rejects"
    (rejects? "(jolt.ffi/__cfn \"fcntl\" [:int :int :varargs] :int)"))
(ok ":varargs with :blocking rejects"
    (rejects? "(jolt.ffi/__cfn \"recv\" [:int :varargs :int] :int :blocking)"))

;; The EMITTED code for a :blocking binding carries __collect_safe on BOTH
;; resolution branches — the global-name fallback AND the scoped dlsym-address
;; branch. The scoped branch dropped it in v0.7.10: every :blocking socket call
;; in a process with any registered native handle was built without the
;; convention, and a GC while one blocked froze the whole VM (the first-cold-run
;; fleet wedge, bisected to the scoped-FFI merge and cured by restoring the
;; convention). Asserting the emission pins the regression deterministically —
;; the behavioral shape needs a cold heap and real GC pressure to fire.
(define (emitted-for s)
  (jolt-analyze-emit-form (jolt-read-string s) "user"))
(define (contains-str? hay needle)
  (let ((hl (string-length hay)) (nl (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i nl) hl) #f)
            ((string=? (substring hay i (+ i nl)) needle) #t)
            (else (loop (+ i 1)))))))
(let ((e (emitted-for "(jolt.ffi/__cfn \"recv\" [:int :pointer :size_t :int] :ssize_t :blocking)")))
  (ok ":blocking emits the collect-safe global fallback"
      (contains-str? e "sa-foreign-procedure-blocking"))
  (ok ":blocking emits the collect-safe scoped branch"
      (contains-str? e "(foreign-procedure __collect_safe a")))

;; --- :string <-> NULL on a CALLBACK -----------------------------------------
;; A foreign-fn carries NULL across a :string position in both directions
;; (test/chez/jolt-ffi-string-null-test.clj). A foreign-callable is the same
;; boundary with C as the caller, so the two directions swap: the null char* C
;; passes IN must arrive as nil, and a jolt fn returning nil must leave as a
;; null char*. Untranslated, the first arrived as #f (jolt FALSE, not nil) and
;; the second RAISED "invalid return value" — so a callback could neither model
;; an optional string argument nor decline to answer one, which is ordinary in
;; any C API that hands a callback a nullable path, name or error.
;;
;; No C witness is needed to drive it: a foreign-callable's entry point is an
;; address and Chez takes an address in the entry position, so the test calls
;; the callback exactly the way C does.
(ev "(def cb-seen (atom :unset))")
(ev "(def cb-str (jolt.ffi/__ccallable
                   (fn [s] (reset! cb-seen s) (if (nil? s) nil (str s \"!\")))
                   [:string] :string))")
(let ((call-cb (foreign-procedure (jnum->exact (var-deref "user" "cb-str")) (string) string)))
  ;; C hands in NULL: the jolt fn must see nil (the atom starts at :unset, so a
  ;; callback that never ran would not read as nil either).
  (let ((returned (call-cb #f)))
    (ok "callback :string argument of NULL arrives as nil"
        (jolt-nil? (jolt-deref (var-deref "user" "cb-seen"))))
    ;; and the nil it returns leaves as a null char*, which Chez reads back as #f
    (ok "callback :string result of nil leaves as NULL"
        (eq? returned #f)))
  ;; the ordinary direction is untouched on both halves
  (let ((returned (call-cb "hi")))
    (ok "callback :string argument of a real char* arrives as a string"
        (equal? "hi" (jolt-deref (var-deref "user" "cb-seen"))))
    (ok "callback :string result of a real string still marshals"
        (equal? "hi!" returned))))
(ev "(jolt.ffi/free-callable cb-str)")

;; A callback RETURNING false must be rejected the same way an argument is. This
;; is the direction where the old pass-through was most misleading: the callback
;; hands C a null char*, C acts on "absent", and nothing in jolt ever said so.
;; nil is how a callback declines to answer.
(ev "(def cb-false (jolt.ffi/__ccallable (fn [_] false) [:string] :string))")
(let ((call-cb (foreign-procedure (jnum->exact (var-deref "user" "cb-false")) (string) string)))
  (ok "a callback returning false from :string raises instead of answering NULL"
      (guard (e (#t #t)) (call-cb "x") #f)))
(ev "(jolt.ffi/free-callable cb-false)")

;; The conversions are emitted ONLY where the signature says :string, so every
;; other binding and callable is emitted byte-identically to before they
;; existed — that is what keeps this off the hot path of the FFI's scalar and
;; pointer bindings, and it is the property that regresses silently.
(let ((scalar-fn (emitted-for "(jolt.ffi/__cfn \"abs\" [:int] :int)"))
      (string-fn (emitted-for "(jolt.ffi/__cfn \"getenv\" [:string] :string)"))
      (scalar-cb (emitted-for "(jolt.ffi/__ccallable identity [:int] :int)"))
      (string-cb (emitted-for "(jolt.ffi/__ccallable identity [:string] :string)")))
  (ok "a foreign-fn without :string emits no string conversion"
      (and (not (contains-str? scalar-fn "jolt-ffi-string->c"))
           (not (contains-str? scalar-fn "jolt-ffi-c->string"))))
  (ok "a foreign-fn with :string converts out on the arg and in on the result"
      (and (contains-str? string-fn "jolt-ffi-string->c")
           (contains-str? string-fn "jolt-ffi-c->string")))
  (ok "a callable without :string emits no wrapper lambda"
      (and (not (contains-str? scalar-cb "jolt-ffi-string->c"))
           (not (contains-str? scalar-cb "jolt-ffi-c->string"))))
  ;; the callable's directions are the MIRROR of the foreign-fn's: c->string on
  ;; the argument, string->c on the result
  (ok "a callable with :string converts in on the arg and out on the result"
      (and (contains-str? string-cb "jolt-ffi-c->string")
           (contains-str? string-cb "jolt-ffi-string->c"))))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
