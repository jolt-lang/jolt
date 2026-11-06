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
                     (jolt.ffi/write p :int 4242 0)
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
               (jolt.ffi/write p :int v (* i w)))
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
;; rule — a marker first (C needs a named parameter) and :blocking (no
;; convention combining). A TRAILING marker is no longer malformed: it is the
;; bare form, covered below.
(define (rejects? s)
  (call/cc (lambda (k)
    (with-exception-handler (lambda (e) (k #t))
      (lambda () (ev s) #f)))))
(ok ":varargs first rejects"
    (rejects? "(jolt.ffi/__cfn \"fcntl\" [:varargs :int] :int)"))
(ok ":varargs with :blocking rejects"
    (rejects? "(jolt.ffi/__cfn \"recv\" [:int :varargs :int] :int :blocking)"))

;; :& is babashka.ffi's spelling of the same marker, so a signature written for
;; either FFI declares the same variadic call — and every rule above holds under
;; it, since it IS the same code path.
(ev "(def c-fcntl-amp (jolt.ffi/__cfn \"fcntl\" [:int :int :& :int] :int))")
(ok ":& is the same marker as :varargs"
    (jolt-truthy?
      (ev "(let [fd (c-socket 2 1 0)] (c-fcntl-amp fd f-setfl o-nonblock) (let [after (c-fcntl fd f-getfl 0)] (c-close fd) (pos? (bit-and after o-nonblock))))")))
(ok ":& first rejects"
    (rejects? "(jolt.ffi/__cfn \"fcntl\" [:& :int] :int)"))
(ok ":& with :blocking rejects"
    (rejects? "(jolt.ffi/__cfn \"recv\" [:int :& :int] :int :blocking)"))
;; The BARE marker — no types after it — is babashka.ffi's per-call tail
;; inference: ONE binding serving every tail shape, each call's tail read off the
;; values it is given. A foreign-procedure has its types fixed when it is
;; compiled, so the binding lowers to a dispatcher that compiles (and caches) one
;; foreign-procedure per observed shape. open(2) is the case the marker exists
;; for — open(path, flags) and open(path, flags, mode) off one binding.
(ev "(def c-open (jolt.ffi/__cfn \"open\" [:string :int :&] :int))")
(ev "(def c-unlink (jolt.ffi/__cfn \"unlink\" [:string] :int))")
(ok "bare :& — two-argument open, empty tail"
    (jolt-truthy?
      (ev "(let [fd (c-open \"/dev/null\" 0)] (c-close fd) (>= fd 0))")))
(ok "bare :& — three-argument open, one-int tail, SAME binding"
    (jolt-truthy?
      (ev (string-append
            "(let [p \"" (host-temp-dir) "/jolt-bare-varargs-gate\" fd (c-open p "
            (if (eq? (sa-os-family) 'linux) "65" "513")   ; O_CREAT|O_WRONLY
            " 420)] (c-close fd) (c-unlink p) (>= fd 0))"))))
;; Every carrier, through snprintf: 64-bit integers (integers, pointers,
;; booleans and nil), doubles (floats and ratios) and C strings.
(ev "(def c-snprintf (jolt.ffi/__cfn \"snprintf\" [:pointer :size_t :string :&] :int))")
(define (fmt call)
  (ev (string-append "(let [b (jolt.ffi/alloc 128)]"
                     " (try " call " (jolt.ffi/ptr->string b)"
                     " (finally (jolt.ffi/free b))))")))
(ok "bare :& carrier — no tail at all"    (equal? "none" (fmt "(c-snprintf b 128 \"none\")")))
(ok "bare :& carrier — integer"           (equal? "42" (fmt "(c-snprintf b 128 \"%d\" 42)")))
(ok "bare :& carrier — double"            (equal? "2.50" (fmt "(c-snprintf b 128 \"%.2f\" 2.5)")))
(ok "bare :& carrier — ratio promotes"    (equal? "0.250" (fmt "(c-snprintf b 128 \"%.3f\" 1/4)")))
(ok "bare :& carrier — string"            (equal? "hi" (fmt "(c-snprintf b 128 \"%s\" \"hi\")")))
(ok "bare :& carrier — boolean and nil"   (equal? "1 0 0" (fmt "(c-snprintf b 128 \"%d %d %d\" true false nil)")))
(ok "bare :& — mixed three-value tail"    (equal? "a 1 2.0" (fmt "(c-snprintf b 128 \"%s %d %.1f\" \"a\" 1 2.0)")))
;; Tails of nought to three are arity-specialized case-lambda arms that carry
;; the values as arguments; a LONGER tail falls through to the rest arm, which
;; keys and marshals the list the general way. Both must agree, so the boundary
;; is covered on either side.
(ok "bare :& — four-value tail (the rest arm)"
    (equal? "1 2 3 4" (fmt "(c-snprintf b 128 \"%d %d %d %d\" 1 2 3 4)")))
(ok "bare :& — six-value mixed tail (the rest arm)"
    (equal? "1 a 2.5 2 b 3.5"
            (fmt "(c-snprintf b 128 \"%d %s %.1f %d %s %.1f\" 1 \"a\" 2.5 2 \"b\" 3.5)")))
(ok "bare :& — the rest arm marshals booleans and nil too"
    (equal? "1 0 0 7" (fmt "(c-snprintf b 128 \"%d %d %d %d\" true false nil 7)")))
;; The cache is keyed on the SHAPE, so the same binding serves a second shape
;; and then answers both from the cache.
(ok "bare :& — one binding, two shapes, then reuse"
    (and (equal? "5" (fmt "(c-snprintf b 128 \"%d\" 5)"))
         (equal? "x=6" (fmt "(c-snprintf b 128 \"%s=%d\" \"x\" 6)"))
         (equal? "7" (fmt "(c-snprintf b 128 \"%d\" 7)"))))
;; A value in no carrier names itself rather than reaching C as whatever it is.
(ok "bare :& — an uncarryable tail value rejects at the call"
    (rejects? "(let [b (jolt.ffi/alloc 8)] (c-snprintf b 8 \"%d\" :kw))"))
;; A fixed by-value aggregate cannot combine with the bare form: the tail is
;; compiled at the call, and an aggregate argument needs an ftype the binding
;; declared.
(ok "bare :& with a fixed by-value aggregate rejects"
    (rejects? "(jolt.ffi/__cfn \"f\" [[:by-value [:struct [[:a :int]]]] :&] :int)"))
(ok "bare :& with an aggregate return rejects"
    (rejects? "(jolt.ffi/__cfn \"f\" [:int :&] [:by-value [:struct [[:a :int]]]])"))
(ok "bare :& with :blocking rejects"
    (rejects? "(jolt.ffi/__cfn \"recv\" [:int :&] :int :blocking)"))

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

;; --- Android / Termux: the prefix the OS loader does not search ---------------
;; On Android the process is launched by /system/bin/linker64, whose search path
;; never includes the Termux prefix, so a bare soname either misses (the
;; versioned name lives only in $PREFIX/lib) or binds Android's own library of
;; that name -- /system/lib64/libssl.so is BoringSSL, which has no SSL_ctrl. The
;; runtime gives a bare name a second try under $PREFIX/lib there, AFTER the OS
;; search, so a name that legitimately resolves in /system/lib64 is untouched.
;;
;; Detection is by environment, which is how Termux identifies itself, so this
;; gate can stand in for an Android process on any host. Kept last in the file:
;; Chez has putenv but no unsetenv, so the stand-in environment is cleared by
;; setting PREFIX back to empty rather than removing it.
;; On a REAL Android/Termux host the fallback is already armed from the
;; process's own environment, so the off-Android half of this pair can only be
;; asserted where that environment does not say Android (the same rule ffi.ss
;; uses). The stand-in below sets the variables explicitly and runs everywhere.
(unless (ffi-termux-lib-dir)
  (ok "off Android there is no prefix to fall back to" (not (ffi-termux-lib-dir)))
  (ok "and a bare name is tried exactly once"
      (equal? '("libssl.so") (ffi-native-candidates "libssl.so"))))

(putenv "PREFIX" "/data/data/com.termux/files/usr")
(putenv "TERMUX_VERSION" "0.119.0-beta.3")

(ok "the Termux prefix is detected from the environment"
    (equal? "/data/data/com.termux/files/usr/lib" (ffi-termux-lib-dir)))
;; the ORDER is the fix: the OS search first, the prefix as a fallback.
(ok "a bare soname falls back to $PREFIX/lib, after the OS search"
    (equal? '("libssl.so.3" "/data/data/com.termux/files/usr/lib/libssl.so.3")
            (ffi-native-candidates "libssl.so.3")))
(ok "a path with a directory in it is opened as given"
    (equal? '("/usr/lib/libssl.so.3") (ffi-native-candidates "/usr/lib/libssl.so.3")))
(ok "so is a relative one"
    (equal? '("./libssl.so.3") (ffi-native-candidates "./libssl.so.3")))
;; load-system-library globs the search directories for versioned sonames; the
;; prefix has to be one of them, or libz.so.1 in Termux is invisible to it.
(ok "the prefix joins the directory search too"
    (and (member "/data/data/com.termux/files/usr/lib" (ffi-so-search-dirs)) #t))

(putenv "PREFIX" "")
(ok "clearing PREFIX ends the fallback"
    (and (not (ffi-termux-lib-dir))
         (equal? '("libssl.so") (ffi-native-candidates "libssl.so"))))

;; --- Windows: the soversion is in the FILENAME -------------------------------
;; The Unix half of load-system-library globs for libz.so.* because a distro may
;; ship libz.so.1 and no libz.so. Windows has the same gap spelled differently:
;; OpenSSL's own builds — Git for Windows', which jolt's docs tell a Windows user
;; to install — are libcrypto-3-x64.dll, so neither "crypto.dll" nor
;; "libcrypto.dll" names a file that exists, however completely the DLL is on
;; the loader's search path. That empty search is what left a :jolt/native spec
;; reporting "not found" about a library sitting beside jolt.exe (#989).
;;
;; The matcher is what keeps the glob honest, so it is pinned here rather than
;; inferred from a directory listing this host does not have.
(ok "a version-suffixed DLL is matched" (ffi-dll-variant? "libcrypto-3-x64.dll" "crypto"))
(ok "and the 1.1 spelling too"          (ffi-dll-variant? "libcrypto-1_1-x64.dll" "crypto"))
(ok "the un-prefixed form matches"      (ffi-dll-variant? "crypto-3-x64.dll" "crypto"))
(ok "case is not significant"           (ffi-dll-variant? "LIBCRYPTO-3-X64.DLL" "crypto"))
;; the conventional names are tried first and are not repeated by the glob
(ok "the bare conventional name is not re-listed" (not (ffi-dll-variant? "libcrypto.dll" "crypto")))
;; a longer name that merely starts the same way is a different library
(ok "a longer name is a different library" (not (ffi-dll-variant? "libcryptohelper.dll" "crypto")))
(ok "another library's file does not match" (not (ffi-dll-variant? "libssl-3-x64.dll" "crypto")))
(ok "a non-DLL does not match"          (not (ffi-dll-variant? "libcrypto-3-x64.so" "crypto")))
;; zlib1.dll is NOT a versioned spelling of "z" — no separator after the name —
;; so libz stays a library that has to declare its Windows candidate itself.
(ok "zlib1.dll is not derived from \"z\"" (not (ffi-dll-variant? "zlib1.dll" "z")))

;; The candidate list is what load-natives! falls back to for a spec that
;; declares nothing for the running platform, so its SHAPE is part of the
;; contract: the conventional spellings first, in the loader's own order.
(ok "the conventional names lead on this host"
    (let ((cs (ffi-system-library-candidates "z")))
      (case (sa-os-family)
        ((macos)   (equal? (list "libz.dylib" "z.dylib") cs))
        ((windows) (and (string=? "z.dll" (car cs)) (string=? "libz.dll" (cadr cs))))
        (else      (string=? "libz.so" (car cs))))))
;; and load-system-library is that same list, so the two can never drift
(ok "load-system-library opens one of them"
    (guard (e (#t #f))
      (let ((lib (ffi-load-system-library "c")))
        (and lib #t))))

;; ffi-calloc zeroes the block in place. It used to build a zeroed bytevector the
;; block's size and copy that over, so every large allocation also churned the
;; heap by its own size.
(define (block-bytes p n)
  (let loop ((i (- n 1)) (out '()))
    (if (< i 0) out (loop (- i 1) (cons (foreign-ref 'unsigned-8 p i) out)))))
(let* ((n 32) (p (foreign-alloc n)))
  (do ((i 0 (+ i 1))) ((= i n)) (foreign-set! 'unsigned-8 p i 255))
  (sa-foreign-zero! (+ p 8) 16)
  (ok "sa-foreign-zero! clears exactly the range"
      (equal? (block-bytes p n)
              (append (make-list 8 255) (make-list 16 0) (make-list 8 255))))
  (sa-foreign-zero! p 0)
  (ok "sa-foreign-zero! of nothing touches nothing"
      (= 255 (foreign-ref 'unsigned-8 p 0)))
  (foreign-free p))
(for-each
  (lambda (n)
    ;; dirty a block and free it first, so the allocator's reuse can't pass for
    ;; a zeroed block
    (let ((dirty (foreign-alloc n)))
      (do ((i 0 (+ i 1))) ((= i n)) (foreign-set! 'unsigned-8 dirty i 255))
      (foreign-free dirty))
    (let ((p (ffi-calloc n)))
      (ok (format "ffi-calloc ~a answers zeroed memory" n)
          (for-all zero? (block-bytes p n)))
      (ffi-free p)))
  '(1 15 16 4096 65536))
(let ((quiet (collect-request-handler)))
  (collect-request-handler void)
  (let ((before (bytes-allocated)))
    (do ((i 0 (+ i 1))) ((= i 32)) (ffi-free (ffi-calloc 1048576)))
    (let ((grew (- (bytes-allocated) before)))
      (collect-request-handler quiet)
      (ok (format "ffi-calloc puts no block-sized garbage on the heap (grew ~a)" grew)
          (< grew 1048576)))))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
