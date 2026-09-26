;; host/scheme-adapter/guile.ss — the hypothetical Guile target's adapter STUB
;; for the portable Scheme layer (PSL R10 close-out).
;;
;; DESIGN ARTIFACT — NOT LOADABLE CODE. This file does not run: every entry is
;; marked UNIMPLEMENTED. Its value is proving the CONTRACT is target-shaped,
;; not chez-shaped: each contract name below is annotated with the Guile-native
;; candidate where one is obvious and `??` where the mapping needs research. A
;; real Guile port starts by filling this file in (implementing the entries,
;; deleting the annotations), then wiring the gate-time half — a guile.ss
;; assertion pass probing the (guile) environment like chez.ss probes
;; (chezscheme), and a Guile scheme-adapter-runtime.ss providing the sa-* names.
;;
;; Structure mirrors host/scheme-adapter/chez.ss: one section per CONTRACT.txt
;; tier, in contract order, with every name of that tier. Chez semantics PINNED
;; in CONTRACT.txt (thread-parameter fork-inheritance, non-recursive mutexes,
;; numeric thread ids, spurious-wakeup conditions) are noted "must verify" — a
;; candidate is never asserted as matching without checking the target.
;;
;; Guile reference: 3.0.x. Module names follow Guile's (rnrs ...) / (srfi srfi-NN)
;; conventions. Do NOT guess semantics beyond what is written here.

;; ---------------------------------------------------------------------------
;; tier: threads (SRFI-18 / (rnrs) threads)
;; ---------------------------------------------------------------------------
;;   fork-thread            UNIMPLEMENTED  Guile: SRFI-18 fork-thread.
;;                                        must verify: contract pins THREAD-PARAMETER
;;                                        FORK-INHERITANCE (child starts with creator's
;;                                        current values); Guile's SRFI-39 parameters
;;                                        default fresh per thread — inheritance must be
;;                                        reproduced explicitly, like the host relies on.
;;   memory-order-acquire   UNIMPLEMENTED  Guile: no-op when threads share no memory,
;;                                        else a fence; seq.ss publishes a forced
;;                                        tail behind memory-order-release.
;;   memory-order-release   UNIMPLEMENTED  (see memory-order-acquire)
;;   make-mutex             UNIMPLEMENTED  Guile: SRFI-18 make-mutex.
;;   mutex-acquire          UNIMPLEMENTED  Guile: (rnrs) mutex-acquire (SRFI-18 mutex-lock!).
;;                                        must verify: contract pins NON-RECURSIVE mutexes
;;                                        (same-thread re-acquire blocks) — SRFI-18 matches.
;;   mutex-release          UNIMPLEMENTED  Guile: (rnrs) mutex-release.
;;   with-mutex             UNIMPLEMENTED  Guile: (rnrs) with-mutex.
;;   make-condition         UNIMPLEMENTED  Guile: (rnrs) make-condition.
;;   condition-wait         UNIMPLEMENTED  Guile: (rnrs) condition-wait.
;;                                        must verify: contract permits SPURIOUS wakeups
;;                                        (every host wait re-checks its predicate).
;;   condition-signal       UNIMPLEMENTED  Guile: (rnrs) condition-signal.
;;   condition-broadcast    UNIMPLEMENTED  Guile: (rnrs) condition-broadcast.
;;   make-thread-parameter  UNIMPLEMENTED  Guile: make-thread-parameter (SRFI-18 style).
;;                                        must verify: fork-inheritance, same note as
;;                                        fork-thread (host conveys dyn-binding-stack and
;;                                        *txn* through these).
;;   get-thread-id          UNIMPLEMENTED  ?? Guile has no numeric per-thread id;
;;                                        current-thread returns an object.
;;                                        must verify: contract pins a NUMBER distinct per
;;                                        live thread (host keys an eqv?-hashtable by it and
;;                                        multiplies it into the random seed) — needs a
;;                                        numeric mapping.

;; ---------------------------------------------------------------------------
;; tier: system
;; ---------------------------------------------------------------------------
;;   getenv                 UNIMPLEMENTED  Guile: (getenv "NAME") -> string or #f (posix).
;;   sleep                  UNIMPLEMENTED  ?? Guile's sleep is SRFI-18 seconds; contract
;;                                        shape is R6RS (rnrs time) sleep taking a time
;;                                        object. must verify which shape satisfies both.
;;   current-time           UNIMPLEMENTED  Guile: (srfi srfi-19) current-time — zero-arg,
;;                                        time-utc object; matches the pinned zero-arg shape.
;;   system                 UNIMPLEMENTED  Guile: (system "cmd") — POSIX, exit status
;;                                        as exact integer.

;; ---------------------------------------------------------------------------
;; tier: capability-system (sa-* names are OUR contract; Guile adapter supplies them)
;; ---------------------------------------------------------------------------
;;   sa-run-process         UNIMPLEMENTED  ?? Guile subprocess: (system ...) loses stdout;
;;                                        open-pipe/open-process candidates.
;;                                        must verify: contract requires raise 'unsupported
;;                                        on targets without subprocess support.
;;   sa-environment-pairs   UNIMPLEMENTED  Guile: (environ) -> "K=V" strings, split on
;;                                        the first "="; the R7RS alist shape.
;;   sa-os-release          UNIMPLEMENTED  Guile: (utsname:release (uname)) — posix; #f
;;                                        permitted by contract.
;;   sa-gc-collect          UNIMPLEMENTED  Guile: (gc) — may no-op (contract allows).
;;   sa-gc-max-generation   UNIMPLEMENTED  ?? Guile uses Boehm GC — no generations.
;;   sa-bytes-allocated     UNIMPLEMENTED  ?? (gc-stats) candidate; must verify field/shape.
;;   sa-total-memory-bytes  UNIMPLEMENTED  ?? (gc-stats) candidate; must verify.
;;   sa-max-memory-bytes    UNIMPLEMENTED  The current total permitted by contract.
;;   sa-reset-max-memory-bytes! UNIMPLEMENTED  No-op permitted when the above answers "now".
;;   sa-real-time-ms        UNIMPLEMENTED  ?? (get-internal-real-time) — must verify units;
;;                                        may use any monotonic ms clock, never a constant.
;;   sa-file-mtime-ms       UNIMPLEMENTED  Guile: (stat:mtim (stat path)) — posix; must
;;                                        verify unit (time object -> ms conversion).
;;   sa-gc-trip-bytes!      UNIMPLEMENTED  ?? Guile exposes no GC trip threshold; no-op
;;                                        candidate — must verify callers tolerate it.
;;   sa-gc-trip-bytes       UNIMPLEMENTED  0 permitted by contract (no trip threshold).

;; ---------------------------------------------------------------------------
;; tier: capability-introspect
;; ---------------------------------------------------------------------------
;;   sa-host-tag            UNIMPLEMENTED  Guile: (version) -> "3.0.9" — naming/telemetry
;;                                        only; all logic consumes the derived properties.
;;   sa-os-family           UNIMPLEMENTED  ?? (uname) from (ice-9 posix) candidate.
;;   sa-arch                UNIMPLEMENTED  ?? (uname) machine field candidate.
;;   sa-endian              UNIMPLEMENTED  Guile: (native-endianness) — R6RS (rnrs bytevectors).
;;   sa-stats               UNIMPLEMENTED  ?? (gc-stats); contract permits a zero vector.
;;   sa-introspect-enabled? UNIMPLEMENTED  #f — degraded-backtrace gate (contract's
;;                                        documented degradation path).
;;   sa-continuation-frames UNIMPLEMENTED  ?? Guile's call-with-prompt does not expose
;;                                        frames; '() permitted (backtrace renders bare).
;;   sa-procedure-info      UNIMPLEMENTED  #f permitted by contract.
;;   sa-procedure-code-name UNIMPLEMENTED  (procedure-name p) is a symbol in Guile;
;;                                        #f permitted by contract.

;; ---------------------------------------------------------------------------
;; tier: capability-ffi
;; ---------------------------------------------------------------------------
;;   sa-call-with-escape-continuation UNIMPLEMENTED Guile has call/cc; wrap it with a
;;                                        spent flag so a second invocation (or one after
;;                                        the capturing call returned) RAISES rather than
;;                                        re-entering. The escape must unwind dynamic-wind
;;                                        on the way out — jolt's `finally` rides that.
;;   sa-foreign-procedure   UNIMPLEMENTED  Guile: (pointer->procedure ret (dynamic-func name
;;                                        lib) args) — (system foreign). must verify call
;;                                        shape translation; SYNTAX on Chez (lowering).
;;   sa-foreign-procedure-native-error UNIMPLEMENTED Must capture errno atomically
;;                                        at the foreign call boundary; a later
;;                                        read is racy and not equivalent.
;;   sa-foreign-procedure-blocking UNIMPLEMENTED  ?? must verify collect-safety; may
;;                                        collapse to plain sa-foreign-procedure only if the
;;                                        collector never stops other threads.
;;   sa-foreign-alloc       UNIMPLEMENTED  ?? (system foreign) make-c-struct / bytevector
;;                                        candidates.
;;   sa-foreign-free        UNIMPLEMENTED  ?? same.
;;   sa-foreign-ref         UNIMPLEMENTED  ?? (parse-c-struct ...) candidate.
;;   sa-foreign-set!        UNIMPLEMENTED  ?? (make-c-struct ...) candidate.
;;   sa-foreign-sizeof      UNIMPLEMENTED  ?? (sizeof) — must verify (Guile has (sizeof)).
;;   sa-foreign-bytes-ref!  UNIMPLEMENTED  Guile: (pointer->bytevector ptr n) + bytevector-copy!
;;   sa-foreign-bytes-set!  UNIMPLEMENTED  — (system foreign) gives a bytevector VIEW over
;;                                        foreign memory, so both directions are a
;;                                        bytevector-copy!. A target with no block move may
;;                                        loop over sa-foreign-ref/-set! instead; that is
;;                                        correct and gives back the ~30ns/byte these exist
;;                                        to remove. Never collect-safe: the bytevector
;;                                        crosses to C by address.
;;   sa-lock-object         UNIMPLEMENTED  may BOTH no-op on a non-moving collector; Guile's
;;   sa-unlock-object       UNIMPLEMENTED  Boehm GC is non-moving — no-op candidate;
;;                                        must verify (never just one, or it leaks/crashes).
;;   sa-load-shared-object  UNIMPLEMENTED  Guile: (dynamic-link "lib") — (system foreign).
;;   sa-foreign-entry?      UNIMPLEMENTED  ?? (dynamic-func ...) failure shape; must verify.
;;   sa-foreign-entry-address UNIMPLEMENTED Guile: (dynamic-func name lib).
;;   sa-foreign-callable-entry-point UNIMPLEMENTED Guile: (procedure->pointer ret proc args).
;;   sa-foreign-procedure-runtime UNIMPLEMENTED ?? internal plumbing; must verify shape.

;; ---------------------------------------------------------------------------
;; tier: eval
;; ---------------------------------------------------------------------------
;;   interaction-environment UNIMPLEMENTED ?? Guile idiom: (eval expr (current-module));
;;                                        contract pins an env whose eval sees the target's
;;                                        top level (jolt's compile spine evals emitted
;;                                        Scheme there). must verify against (resolve-module ...)
;;                                        / R7RS (environment ...).

;; ---------------------------------------------------------------------------
;; tier: capability-native-compile
;; ---------------------------------------------------------------------------
;;   sa-baked-global        UNIMPLEMENTED  ?? baked-runtime fingerprint plumbing.
;;   sa-compile-file        UNIMPLEMENTED  Guile: compile-file (Guile native, emits .go).
;;                                        must verify profile-alist mapping and AOT-cache
;;                                        contract; may RAISE (cache disables, loads source).
;;   sa-make-boot-file      UNIMPLEMENTED  ?? Guile has no boot files (different build
;;                                        model); raise candidate — must verify.

;; ---------------------------------------------------------------------------
;; tier: capability-image
;; ---------------------------------------------------------------------------
;;   sa-fasl-write          UNIMPLEMENTED  ?? Guile: (ice-9 serialize) / (write ...) sexp
;;                                        candidates; must verify externals hook contract;
;;                                        may RAISE (jolt.image surfaces a clean unsupported
;;                                        error — the raise must carry a message).
;;   sa-fasl-read           UNIMPLEMENTED  ?? same, must verify externals resolution shape.

;; ---------------------------------------------------------------------------
;; tier: capability-unchecked
;; ---------------------------------------------------------------------------
;;   sa-ufx+ sa-ufx- sa-ufx<? sa-ufx>=? sa-ufx=?  UNIMPLEMENTED  Guile: the
;;                                        checked (rnrs arithmetic fixnums) ops are
;;                                        the permitted degradation; a tuned port may
;;                                        expand to the unchecked variants.
;;   sa-uvector-ref sa-uvector-set!       UNIMPLEMENTED  Guile: vector-ref /
;;                                        vector-set! (degradation), or the
;;                                        unchecked accessors when the port is tuned.
;;   sa-vector-copy-range!                UNIMPLEMENTED  Guile: R7RS vector-copy!
;;                                        has the contract's argument order.
;;   sa-vector-copy                       UNIMPLEMENTED  Guile: vector-copy.
;;   sa-subvector                         UNIMPLEMENTED  Guile: R7RS vector-copy with
;;                                        start and end.
;;   sa-vector-append                     UNIMPLEMENTED  Guile: vector-append (R7RS).
;;   sa-string-copy-range!                UNIMPLEMENTED  Guile: string-copy! (SRFI-13 /
;;                                        R7RS) has the contract's argument order.

;; ---------------------------------------------------------------------------
;; tier: misc
;; ---------------------------------------------------------------------------
;;   gensym                 UNIMPLEMENTED  Guile: (gensym) native.
;;   format                 UNIMPLEMENTED  Guile: SRFI-28 (format #f ...) — ~a ~s ~d subset;
;;                                        (ice-9 format) is the full superset.
;;   printf                 UNIMPLEMENTED  Guile: SRFI-28 (format #t ...).
;;   fprintf                UNIMPLEMENTED  Guile: SRFI-28 (format port ...).
;;   pretty-print           UNIMPLEMENTED  Guile: (pretty-print obj [port]) — (ice-9 pretty-print).
;;   void                   UNIMPLEMENTED  ?? Guile: (void) exists in recent versions; must
;;                                        verify; trivial either way (unspecified value).
;;   box                    UNIMPLEMENTED  Guile: (srfi srfi-111) box/unbox/set-box!.
;;   unbox                  UNIMPLEMENTED  Guile: (srfi srfi-111).
;;   set-box!               UNIMPLEMENTED  Guile: (srfi srfi-111).
;;   sort                   UNIMPLEMENTED  Guile: (srfi srfi-132) sort; must verify ARGUMENT
;;                                        ORDER — contract pins Chez shape (sort pred lis),
;;                                        SRFI-132 has the opposite; adapter wraps.
;;   iota                   UNIMPLEMENTED  Guile: (srfi srfi-1) iota.
;;   list-head              UNIMPLEMENTED  Guile: (srfi srfi-1) list-head.
;;   vector-copy            UNIMPLEMENTED  Guile: (srfi srfi-43) vector-copy.
;;   hashtable-values       UNIMPLEMENTED  build from (rnrs) hashtable-keys + ref/for-each.
;;   hashtable-cells        UNIMPLEMENTED  build from (rnrs) hashtable-keys + ref/for-each.
;;   scheme-version         UNIMPLEMENTED  Guile: (version) -> "3.0.9" — zero-arg, naming/
;;                                        telemetry only; nothing may parse it.

;; ---------------------------------------------------------------------------
;; tier: coroutines (fibers R1 — the fiber primitive + single-carrier scheduler)
;; ---------------------------------------------------------------------------
;;   sa-fiber-spawn         UNIMPLEMENTED  ?? Guile: call/cc-based coroutines; Guile Fibers
;;                                        exists upstream as a reference. must verify
;;                                        continuation re-entry semantics (Guile's call/cc
;;                                        is multi-shot — the plan's one-shot discipline
;;                                        plus a scheduler that owns completion is what
;;                                        prevents re-running).
;;   sa-fiber-yield         UNIMPLEMENTED  ?? same.
;;   sa-fiber-resume        UNIMPLEMENTED  ?? same.
;;   sa-fiber-run-all       UNIMPLEMENTED  ?? same; must verify the "drain, then poll"
;;                                        shape against Guile's prompt machinery.
;;                                        must verify: contract permits binding every name
;;                                        to a message-carrying raise when continuations
;;                                        are unusable — `go` then falls back to an OS
;;                                        thread (the plan's documented degradation).

;; ===========================================================================
;; gate-time half (mirrors chez.ss's assertion pass) — UNIMPLEMENTED
;; ===========================================================================
;; A Guile port replaces chez.ss's pass with one probing the (guile) environment:
;;   - read CONTRACT.txt (same "# tier:" format);
;;   - probe each name via (module-bound? ... (resolve-module '(guile))) plus the
;;     sa-* definitions from the Guile scheme-adapter-runtime;
;;   - report ALL missing names at once, exit non-zero.
;; The chez.ss helpers (char-ws?, string->tokens, strip-comment, script-arg,
;; contract-file) port verbatim — they are R6RS and target-neutral.
