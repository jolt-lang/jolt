;; ffi-native-error-test.ss — atomic native-error capture gate for jolt.ffi.
;;
;; Specifies the {:capture-native-error true} contract for foreign-fn/defcfn
;; and the direct jolt.ffi/__cfn special form: a capture-enabled, non-void
;; binding returns [native-result error-code] (result first), where the error
;; code is the value Chez reads from the calling thread's native error slot
;; (POSIX errno / Windows GetLastError) in the foreign-call return path.
;;
;; Required discriminations covered:
;;   * POSIX errno and Windows GetLastError where supported (the captured code
;;     equals what the C helper wrote under the target's convention).
;;   * capture off vs on — same symbol, scalar vs paired return.
;;   * exact [native-result error-code] shape and ordering.
;;   * blocking + capture composition under an actual concurrent collection.
;;   * varargs + capture composition with the compound convention preserved.
;;   * lazy native symbol resolution (the .so may load after the def).
;;   * persistence after a later helper overwrites the same thread's slot.
;;   * scalar preserved for omitted / empty / legacy :blocking / false options.
;;   * analyzer fail-closed: capture on :void, and unknown / namespaced /
;;     non-keyword / non-Boolean / non-map / extra trailing options.
;;
;; Run via the wrapper, which compiles and preloads the C helper:
;;   sh test/chez/ffi-native-error-test.sh "$(CHEZ)"
;; (from the repo root, like every other gate).
;;
;; SEED DEPENDENCY: the capture suite compiles jolt.ffi/__cfn forms with
;; {:capture-native-error true}, so this gate requires a seed re-minted
;; (`make remint`) from an analyzer/backend that validate and lower the option
;; (analyze-ffi-fn in jolt-core/jolt/analyzer.clj, the
;; jolt-ffi-native-error-procedure lowering in backend_scheme.clj). Against an
;; older checked-in seed those forms fail at compile time with an unknown
;; options key — that failure is the seed lagging the source, not a runtime
;; regression.
;;
;; Setup definitions fail fast: if the production lowering is absent or cannot
;; compile, there is no meaningful native-call suite to continue. Individual
;; value assertions are guarded so one bad call reports FAIL without hiding the
;; remaining discriminations. The convention-selection claim is asserted by
;; matching the captured value against the helper-written code, not by branching
;; on the host.

(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/loader.ss")
(set-source-roots! ldr-install-roots)
(load "host/chez/java/ffi.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (raises? thunk) (guard (e (#t #t)) (thunk) #f))
;; eval one form (string) in `user`, like the loader does form-by-form, so a def
;; is visible to a later form.
(define (ev s) (jolt-compile-eval s "user"))
;; guarded eval helpers: a form that should succeed returns its jolt value, or
;; #f if it threw (so an unimplemented lowering reports FAIL rather than
;; aborting the whole gate before later cases run).
(define (evb s) (guard (e (#t #f)) (jolt-truthy? (ev s))))        ; jolt boolean -> Scheme bool
(define (evi s) (guard (e (#t #f)) (jnum->exact (ev s))))         ; jolt number -> exact int, or #f

;; The gate boot intentionally stops before loader.ss, so explicitly load and
;; require the public namespace before exercising foreign-fn/defcfn macros.
(ev "(require '[jolt.ffi :as ffi])")

(define helper-so (getenv "JOLT_FFI_NATIVE_ERROR_HELPER"))
(unless helper-so
  (error #f "JOLT_FFI_NATIVE_ERROR_HELPER must name the compiled helper library"))

;; --- lazy binding: define the capture binding BEFORE the .so is loaded ---------
;; A jolt.ffi/__cfn def lowers to a lazy foreign-procedure (the (let ((p #f))
;; (lambda ...)) wrapper), so the binding may be created before its shared
;; object is present. Loading the helper and THEN calling must resolve. This
;; pins the capture lowering's laziness, not just the scalar one.
(ev "(def c-fail-late (jolt.ffi/__cfn \"jolt_ne_fail\" [:int] :int {:capture-native-error true}))")
(load-shared-object helper-so)

;; --- capture off vs on: same symbol, two bindings -----------------------------
;; The scalar binding of jolt_ne_fail returns the bare int; the capture binding
;; returns the pair. Same C ABI, same symbol — only the opted-in binding changes.
(ev "(def c-fail-scalar (jolt.ffi/__cfn \"jolt_ne_fail\" [:int] :int))")
(ev "(def c-fail-err (jolt.ffi/__cfn \"jolt_ne_fail\" [:int] :int {:capture-native-error true}))")
(ok "scalar binding returns the bare int result (-1)"
    (= -1 (evi "(c-fail-scalar 42)")))
(ok "capture binding returns a vector, not a scalar"
    (evb "(vector? (c-fail-err 42))"))
(ok "capture binding pair has exactly two elements"
    (evb "(= 2 (count (c-fail-err 42)))"))

;; --- exact [native-result error-code] shape and ordering ----------------------
;; Result first, error code second. The result is the failure sentinel (-1) and
;; the error code is the exact value the helper wrote into the slot (42), so a
;; swapped ordering or a stale/zeroed slot fails this case directly.
(ok "capture pair ordering is [native-result error-code] = [-1 42]"
    (evb "(let [[res err] (c-fail-err 42)] (and (= res -1) (= err 42)))"))
(ok "captured error code is the exact int the helper wrote"
    (= 42 (evi "(second (c-fail-err 42))")))
(ok "captured result is the exact int the helper returned"
    (= -1 (evi "(first (c-fail-err 42))")))
(ok "different error codes capture independently (42 then 13)"
    (evb "(let [a (c-fail-err 42) b (c-fail-err 13)] (and (= (second a) 42) (= (second b) 13)))"))

;; --- POSIX errno / Windows GetLastError convention ----------------------------
;; The helper writes 42 to errno (POSIX) or SetLastError (Windows); the binding
;; reads whichever convention the COMPILER TARGET selects. On every supported
;; native target the captured value is 42. A wrong convention (e.g. errno on a
;; Windows target where SetLastError was used) would not match, so this case is
;; the convention discriminator — no host branching in Scheme.
(ok "captured code matches the helper-written slot value (POSIX errno / Windows GetLastError)"
    (= 42 (evi "(second (c-fail-err 42))")))

;; xpatch changes Chez's compiler target but not (machine-type), which continues
;; to name the build host. Expand the actual production selector under synthetic
;; targets so cross-images cannot silently inherit the host convention, and so
;; future/portable targets fail closed instead of guessing errno.
(define (native-error-convention-for-target target)
  (parameterize ((#%$target-machine target))
    (cadr
      (syntax->datum
        (expand
          (list 'jolt-ffi-native-error-convention-case
                (list 'quote '__get_last_error)
                (list 'quote '__errno)))))))
(ok "Windows compiler target selects GetLastError"
    (eq? '__get_last_error (native-error-convention-for-target 'ta6nt)))
(ok "Linux compiler target selects errno"
    (eq? '__errno (native-error-convention-for-target 'ta6le)))
(ok "Linux arm64 compiler target selects errno"
    (eq? '__errno (native-error-convention-for-target 'tarm64le)))
(ok "macOS compiler target selects errno"
    (eq? '__errno (native-error-convention-for-target 'ta6osx)))
(ok "unknown compiler target fails closed"
    (raises? (lambda ()
               (native-error-convention-for-target 'future-a6le))))

;; --- direct __cfn, public foreign-fn, public defcfn all capture ---------------
;; The three entry points the public surface exposes must all lower through the
;; same capture path: foreign-fn/defcfn forward the options map through
;; cfn-form onto the same jolt.ffi/__cfn special form.
(ev "(def c-fail-ff (ffi/foreign-fn \"jolt_ne_fail\" [:int] :int {:capture-native-error true}))")
(ev "(ffi/defcfn c-fail-dc \"jolt_ne_fail\" [:int] :int {:capture-native-error true})")
(ok "direct __cfn captures the pair"
    (evb "(let [[res err] (c-fail-err 42)] (and (= res -1) (= err 42)))"))
(ok "public foreign-fn captures the pair"
    (evb "(let [[res err] (c-fail-ff 42)] (and (= res -1) (= err 42)))"))
(ok "public defcfn captures the pair"
    (evb "(let [[res err] (c-fail-dc 42)] (and (= res -1) (= err 42)))"))
(ok "capture lowering cannot be shadowed by a same-named local"
    (evb "(let [call-with-values (fn [& _] :shadowed)
                f (jolt.ffi/__cfn \"jolt_ne_fail\" [:int] :int
                    {:capture-native-error true})]
            (= [-1 42] (f 42)))"))

;; --- varargs + capture composition -------------------------------------------
;; The native-error convention and (__varargs_after 1) must remain two complete
;; convention data. Flattening the compound marker into `__varargs_after 1`
;; produces malformed Chez foreign-procedure syntax and, on Apple arm64, would
;; send the variadic value through the wrong ABI location.
(ev "(def c-vararg-fail
       (jolt.ffi/__cfn \"jolt_ne_vararg_fail\" [:int :varargs :int] :int
         {:capture-native-error true}))")
(ok "varargs + capture preserves result and native error"
    (evb "(let [[res err] (c-vararg-fail 42 17)]
            (and (= res 17) (= err 42)))"))

;; --- blocking + capture composition -------------------------------------------
;; Keep one worker inside a portable native sleep while the main thread forces a
;; collection. Without __collect_safe on THIS captured binding, Chez rejects the
;; collection while multiple threads are active. Wait for the worker afterward
;; and assert its exact pair so a worker-side binding/capture failure cannot pass.
(ev "(def c-fail-block-err (jolt.ffi/__cfn \"jolt_ne_fail\" [:int] :int {:blocking true :capture-native-error true}))")
(ok "blocking + capture still returns a two-element vector"
    (evb "(= 2 (count (c-fail-block-err 42)))"))
(ok "blocking + capture pair is still [-1 42]"
    (evb "(let [[res err] (c-fail-block-err 42)] (and (= res -1) (= err 42)))"))
(ev "(def c-block-fail-err
       (jolt.ffi/__cfn \"jolt_ne_block_fail\" [:uint32 :int] :int
         {:blocking true :capture-native-error true}))")
(let ((block-fn (var-deref "user" "c-block-fail-err"))
      (mu (make-mutex))
      (cv (make-condition))
      (done? #f)
      (worker-result #f))
  ;; Resolve the lazy foreign symbol before the concurrent witness.
  (block-fn 0 0)
  (fork-thread
    (lambda ()
      ;; A broken capture/collect-safe lowering can throw in this worker. Record
      ;; that as a false result but always publish completion, so the assertion
      ;; below reports FAIL instead of leaving the main thread parked forever.
      (let ((result (guard (e (#t #f)) (block-fn 2000 73))))
        (with-mutex mu
          (set! worker-result result)
          (set! done? #t)
          (condition-signal cv)))))
  ;; Match the established scalar blocking gate: give the worker ample time to
  ;; enter its two-second native sleep before forcing the collector.
  (let loop ((i 0))
    (when (fx<? i 30000000) (loop (fx+ i 1))))
  (ok "blocking + capture permits a concurrent collection"
      (guard (e (#t #f)) (collect) #t))
  (with-mutex mu
    (let wait ()
      (unless done?
        (condition-wait cv mu)
        (wait))))
  (ok "blocking worker returns the exact captured pair [-1 73]"
      (and (jolt-vector? worker-result)
           (= 2 (pvec-count worker-result))
           (= -1 (jnum->exact (pvec-nth-d worker-result 0 jolt-nil)))
           (= 73 (jnum->exact (pvec-nth-d worker-result 1 jolt-nil))))))

;; --- lazy resolution (defined before the .so loaded) --------------------------
(ok "capture binding defined before load resolves on first call"
    (evb "(let [[res err] (c-fail-late 42)] (and (= res -1) (= err 42)))"))

;; --- persistence after a later helper overwrites the slot ----------------------
;; Capture materializes a concrete vector in the foreign-call return path. A
;; later native call that writes a different code into the same thread's slot
;; must not mutate the already-returned pair. jolt_ne_clobber(7) sets the slot
;; to 7; the earlier (c-fail-err 42) pair must still read 42.
(ev "(def c-clobber-scalar (jolt.ffi/__cfn \"jolt_ne_clobber\" [:int] :int))")
(ok "captured pair survives a later slot-overwriting native call"
    (evb "(let [a (c-fail-err 42) _ (c-clobber-scalar 7)] (and (= (first a) -1) (= (second a) 42)))"))

;; --- socket consumer: recv classification uses the captured pair --------------
;; Exercise the production jolt.socket.native binding and jolt.socket's retry loop, not merely the
;; lower-level FFI contract above.  The accepted loopback fd is nonblocking and
;; has no bytes available, so its first recv returns EAGAIN.  Before io-call can
;; classify that result, the operation sends one byte (making the fd readable)
;; and overwrites ambient errno with EBADF.  A delayed errno read therefore
;; mistakes the retryable recv for a terminal failure and returns -1.  Atomic
;; capture preserves EAGAIN, waits, retries, and returns the one available byte.
;;
;; jolt.socket is POSIX-only, while this FFI gate also validates GetLastError on
;; Windows. Keep the socket consumer witness on the platforms that provide it.
(unless (eq? (sa-os-family) 'windows)
  (ev "(require 'jolt.socket)")
  (ok "socket recv keeps EAGAIN across an intervening errno clobber"
      (evb "(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
                   client (java.net.Socket. \"127.0.0.1\" (.getLocalPort server))
                   conn (.accept server)
                   fd (jolt.host/ref-get conn :fd)
                   buf (ffi/alloc 1)
                   out (.getOutputStream client)
                   first? (atom true)
                   raw-recv (deref (ns-resolve 'jolt.socket.native 'c-recv))
                   io-call (deref (ns-resolve 'jolt.socket 'io-call))
                   op (fn []
                        (let [result (raw-recv fd buf 1 0)]
                          (when (compare-and-set! first? true false)
                            (.write out (byte-array [65]) 0 1)
                            (c-clobber-scalar 9))
                          result))]
               (try
                 (= 1 (io-call conn op fd :read))
                 (finally
                   (ffi/free buf)
                   (.close conn)
                   (.close client)
                   (.close server))))"))

  (ok "socket accept keeps EAGAIN across an intervening errno clobber"
      (evb "(let [server (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
                   server-fd (jolt.host/ref-get server :fd)
                   client (atom nil)
                   accepted-fd (atom -1)
                   sa (ffi/alloc 16)
                   lenp (ffi/alloc 4)
                   first? (atom true)
                   raw-accept (deref (ns-resolve 'jolt.socket.native 'c-accept))
                   raw-close (deref (ns-resolve 'jolt.socket.native 'c-close))
                   io-call (deref (ns-resolve 'jolt.socket 'io-call))
                   op (fn []
                        (let [result (raw-accept server-fd sa lenp)]
                          (when (compare-and-set! first? true false)
                            (reset! client
                                    (java.net.Socket. \"127.0.0.1\"
                                                      (.getLocalPort server)))
                            (c-clobber-scalar 9))
                          result))]
               (try
                 (do
                   (ffi/write lenp :int 16)
                   (reset! accepted-fd (io-call server op server-fd :read))
                   (not (neg? @accepted-fd)))
                 (finally
                   (when-not (neg? @accepted-fd) (raw-close @accepted-fd))
                   (when @client (.close @client))
                   (ffi/free sa)
                   (ffi/free lenp)
                   (.close server))))")))

;; --- scalar preserved: omitted / empty / legacy :blocking / false -------------
;; Every non-capturing form must keep the existing scalar result. This is the
;; backwards-compatibility floor: the option is strictly additive.
(ev "(def c-ok-omit  (jolt.ffi/__cfn \"jolt_ne_ok\" [] :int))")
(ev "(def c-ok-empty (jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {}))")
(ev "(def c-ok-false (jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {:capture-native-error false}))")
(ev "(def c-ok-legacy (jolt.ffi/__cfn \"jolt_ne_ok\" [] :int :blocking))")
(ev "(def c-ok-capture (jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {:capture-native-error true}))")
(ok "omitted option keeps scalar result (7)"
    (= 7 (evi "(c-ok-omit)")))
(ok "empty options map keeps scalar result (7)"
    (= 7 (evi "(c-ok-empty)")))
(ok "{:capture-native-error false} keeps scalar result (7)"
    (= 7 (evi "(c-ok-false)")))
(ok "legacy :blocking keyword keeps scalar result (7)"
    (= 7 (evi "(c-ok-legacy)")))
(ok "successful capture is [native-result error-code] = [7 0]"
    (evb "(let [[res err] (c-ok-capture)] (and (= res 7) (= err 0)))"))

;; --- analyzer fail-closed: capture on :void -----------------------------------
;; There is no stable native result to pair with the error code for a :void
;; return, so capture must be rejected at analyze time.
(ok "capture rejected for :void return"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_fail\" [:int] :void {:capture-native-error true})"))))

;; --- analyzer fail-closed: invalid literal options ----------------------------
;; The options map is validated at compile time in jolt.analyzer/analyze-ffi-fn.
;; Each of these must raise rather than silently ignore the bad option.
(ok "unknown options key rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {:bogus true})"))))
(ok "namespaced options key rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {::capture-native-error true})"))))
(ok "non-keyword options key rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {\"blocking\" true})"))))
(ok "non-Boolean :capture-native-error value rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {:capture-native-error \"yes\"})"))))
(ok "non-Boolean :blocking value rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {:blocking 1})"))))
(ok "non-map / non-:blocking trailing option rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int \"blocking\")"))))
(ok "extra trailing argument after options map rejected"
    (raises? (lambda () (ev "(jolt.ffi/__cfn \"jolt_ne_ok\" [] :int {:capture-native-error true} :extra)"))))
(ok "public foreign-fn extra trailing option rejected"
    (raises? (lambda ()
               (ev "(ffi/foreign-fn \"jolt_ne_ok\" [] :int
                       {:capture-native-error true} :extra)"))))
(ok "public defcfn extra trailing option rejected"
    (raises? (lambda ()
               (ev "(ffi/defcfn bad-extra \"jolt_ne_ok\" [] :int
                       {:capture-native-error true} :extra)"))))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
