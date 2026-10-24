;; host-static-methods.ss — the `Class/member` static surface: System
;; (properties/env), Thread, the Long/Integer/Double/Character/String static
;; methods, java.text.NumberFormat, and the Class registry. Registers into
;; host-static.ss's class-statics table (loaded just before this); instantiable host
;; object classes (ArrayList, StringBuilder, …) live in host-static-classes.ss.
;; java.lang.Math is math.ss's, beside clojure.math, which shares its impls.

;; ---- java.lang statics ------------------------------------------------------

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
;;
;; A VIRTUAL REGISTER and not a thread parameter, and the reason is the bug this
;; used to carry a workaround for: a Chez thread parameter is INHERITED, so a
;; forked thread started out holding its PARENT's box and interrupting the child
;; set the parent's flag. That was patched by storing (thread-id . box) and
;; comparing the id on every read. A vreg has the property directly — a freshly
;; forked thread starts every slot at fixnum 0 — so the id compare, the pair, and
;; the (get-thread-id) call all go away, and what is left is one register read.
;; That read is on the hot path: every interruptible wait consults it before
;; deciding, including the ones that never wait (a deref of a delivered promise),
;; and so do Thread/interrupted, .isInterrupted, monitor enter/exit and
;; ReentrantLock's owner check.
(define jolt-vreg-interrupt-box 9)      ; rt.ss owns the slot map; 0-8 were taken
;; Every thread's box is also recorded under its thread id, so a Thread object
;; made for a thread by SOMEONE ELSE (getAllStackTraces) carries that thread's
;; real flag: the one it will find itself, and the one waits register under.
;; Whichever of the two asks first creates it; the entry goes when the thread ends
;; (io.ss, jolt-thread-exit-hook).
(define thread-boxes-by-id (make-eqv-hashtable))
(define thread-boxes-mu (make-mutex))
(define (thread-box-for-id! id)
  (jolt-with-mutex thread-boxes-mu
    (or (hashtable-ref thread-boxes-by-id id #f)
        (let ((b (box #f))) (hashtable-set! thread-boxes-by-id id b) b))))
(define (thread-box-forget! id)
  (jolt-with-mutex thread-boxes-mu (hashtable-delete! thread-boxes-by-id id)))
;;
;; ON A FIBER the answer is the FIBER's box, not its carrier's. A fiber is a
;; virtual thread: interrupting it must end its own wait and no other, and
;; Thread/interrupted must read its own flag. The carrier is an implementation
;; detail the fiber's code never sees, as a JVM virtual thread never sees its
;; carrier. Slot 0 is the running fiber, or fixnum 0 on a thread and on a
;; carrier between fibers (fibers.ss), so a thread pays one register read and a
;; fixnum test for the check.
(define (current-interrupt-box)
  (let ((f (virtual-register 0)))
    (if (fixnum? f) (current-os-thread-box) (jolt-fiber-ibox! f))))
;; The OS THREAD's box whatever is mounted on it: the identity of the thread
;; itself, for the few places that mean the thread and not the code running on it
;; (a monitor's record of which thread a fiber took it on, a Thread object made
;; for a thread by id).
(define (current-os-thread-box)
  (let ((b (virtual-register jolt-vreg-interrupt-box)))
    (if (box? b)
        b
        (let ((nb (thread-box-for-id! (get-thread-id))))
          (set-virtual-register! jolt-vreg-interrupt-box nb)
          nb))))
;; Set by io.ss: re-point the Thread object already made for this thread, if any,
;; at the box it adopts, so it and the thread keep one flag.
(define jolt-thread-box-adopted-hook (lambda (id b) (void)))
;; A thread jolt itself forked already HAS a flag — the box its Thread object hands
;; .interrupt — so it must not lazily allocate a second one. The child adopts that
;; box as its own before running the body; without this, .interrupt from outside
;; and (.isInterrupted (Thread/currentThread)) inside were two unrelated flags and
;; the ordinary interruption idiom never reached the worker.
(define (adopt-interrupt-box! b)
  (set-virtual-register! jolt-vreg-interrupt-box b)
  (let ((id (get-thread-id)))
    (jolt-with-mutex thread-boxes-mu (hashtable-set! thread-boxes-by-id id b))
    (jolt-thread-box-adopted-hook id b))
  b)
(define (clear-thread-interrupt!) (set-box! (current-interrupt-box) #f))

;; libc sched_yield, resolved once; fall back to a zero-length park if the symbol
;; isn't available.
(define thread-yield!
  ;; One store for the answer and the mark, so a thread arriving mid-lookup
  ;; resolves too instead of reading the #f a separate flag left behind.
  (let ((fp 'unresolved))
    (lambda ()
      (when (eq? fp 'unresolved)
        (set! fp (jolt-foreign-proc-safe "sched_yield" '() 'int)))
      (if fp (fp) (sleep (make-time 'time-duration 0 0)))
      jolt-nil)))

;; Thread/sleep, and TimeUnit.sleep through the same door (java/concurrency.ss).
;;
;; A THREAD sleeps in an interruptible wait on a private mutex and condition, so
;; .interrupt throws it out with InterruptedException the way the JVM does. The
;; pair is fresh and unreachable from anywhere else, so the only wake it can get is
;; an interrupt; the deadline is what ends the sleep otherwise, read from the clock
;; by decide rather than signalled (jolt-cv-wait, host/chez/locks.ss).
;;
;; A FIBER parks the same way, with the deadline registered on the timer, and gives
;; its carrier to other fibers for the length of the nap: a fiber is a virtual
;; thread, and a JVM virtual thread's sleep unmounts it. Interruptible mid-sleep
;; like a thread's, through the fiber's own flag. It used to sleep the CARRIER,
;; which stopped every other fiber placed on it for the duration and could only
;; honour an interrupt that was already set.
;;
;; ms may be fractional -- Thread.sleep(ms, nanos), a TimeUnit finer than a
;; millisecond -- and the deadline is exact (mono-deadline-exact), so the sleep
;; lasts what was asked, not that rounded up to a millisecond boundary. A sleep
;; of nothing is the JDK's: it throws if the thread is already interrupted and
;; otherwise yields. It used to wait for the next tick of the millisecond clock.
(define (jolt-thread-sleep-ms ms)
  (let ((ms (exact ms)))
    (if (<= ms 0)
        (begin
          (jolt-interrupt-poll-check! "sleep interrupted")
          (unless (jolt-fiber-wait-turn! 0) (thread-yield!))
          jolt-nil)
        (let ((mu (make-mutex)) (cv (jolt-make-cv)))
          (jolt-cv-wait-interruptibly "sleep interrupted" mu cv (mono-deadline-exact ms)
            (lambda (timed-out?) (if timed-out? #t jolt-cv-again)))
          jolt-nil))))

;; Thread.sleep(millis, nanos): the nanos are part of the sleep, not ignored.
;; Both are longs to the JVM (truncated here), and its argument checks come first:
;; a negative millis, or nanos outside 0..999999, is IllegalArgumentException.
(define (thread-sleep-millis-arg ms)
  (let ((ms (jnum->exact ms)))
    (when (negative? ms)
      (throw-jvm (quote IllegalArgumentException) "timeout value is negative"))
    ms))
(define (jolt-thread-sleep-ms+ns ms ns)
  (let ((ms (thread-sleep-millis-arg ms)) (ns (jnum->exact ns)))
    (when (or (negative? ns) (> ns 999999))
      (throw-jvm (quote IllegalArgumentException) "nanosecond timeout value out of range"))
    (jolt-thread-sleep-ms (+ ms (/ ns 1000000)))))

(define thread-statics
  (list (cons "sleep" (case-lambda
                        ((ms) (jolt-thread-sleep-ms (thread-sleep-millis-arg ms)))
                        ((ms ns) (jolt-thread-sleep-ms+ns ms ns))))
        ;; a fiber yields to the other fibers on its carrier, as a virtual
        ;; thread yields its carrier; not while it holds a counted lock
        (cons "yield" (lambda _ (unless (jolt-fiber-wait-turn! 0) (thread-yield!)) jolt-nil))
        (cons "interrupted" (lambda _ (let* ((b (current-interrupt-box)) (v (unbox b)))
                                        (set-box! b #f) (and v #t))))))
(register-class-statics! "Thread" thread-statics)
(register-class-statics! "java.lang.Thread" thread-statics)

;; clojure.lang.LockingTransaction: jolt refs and their transactions (refs.ss).
;; isRunning -> true when a transaction is active on this thread.
(register-class-statics! "clojure.lang.LockingTransaction" (list (cons "isRunning" (lambda () (and (*txn*) #t)))))

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
          (if (null? (cdr xs)) (throw-jvm (quote IllegalArgumentException) "PersistentArrayMap: odd key/value count")
              (let ((k (car xs)))
                (if (jolt-contains? m k) (throw-jvm (quote IllegalArgumentException) (string-append "Duplicate key: " (jolt-final-str k)))
                    (loop (cddr xs) (jolt-assoc m k (cadr xs))))))))))
(register-class-statics! "PersistentArrayMap" (list (cons "createWithCheck" pam-create-with-check)))
(register-class-statics! "clojure.lang.PersistentArrayMap" (list (cons "createWithCheck" pam-create-with-check)))
;; clojure.lang.PersistentHashMap/createWithCheck: the same duplicate-checking
;; k/v constructor the JVM reaches for once a map is too big for an array map.
;; sci builds EVERY map literal through one of the two, choosing by size, so
;; without this a map of more than eight pairs holding any non-constant value
;; died with "No dependency provides clojure.lang.PersistentHashMap" — a map
;; that size is ordinary in a sci program.
;;
;; Shares pam-create-with-check: it already seqs its argument, which is what
;; makes the two interchangeable here, since the JVM hands this one an ISeq and
;; the array-map one an Object[]. jolt has a single map representation, so the
;; array-vs-hash split is a size optimization the value semantics do not carry.
(register-class-statics! "PersistentHashMap" (list (cons "createWithCheck" pam-create-with-check)))
(register-class-statics! "clojure.lang.PersistentHashMap" (list (cons "createWithCheck" pam-create-with-check)))

;; clojure.lang.APersistentMap/mapHash + mapHasheq: what a custom map type
;; (flatland's OrderedMap) delegates to for .hashCode / .hasheq. mapHash is the
;; java.util.Map hashCode — sum over entries of key.hashCode ^ val.hashCode, in a
;; 32-bit int. It iterates the map's ENTRIES (via seq) rather than hashing the map
;; as a whole, so a custom map type whose own .hashCode calls back here doesn't
;; recurse. jolt.host/java-hashcode is a forward ref to dot-forms.ss (element
;; hashCodes; bound by call time). mapHasheq is the Murmur3 hasheq (jolt-hash).
(define (ap-map-hash m)
  (let loop ((s (jolt-seq m)) (h 0))
    (if (jolt-nil? s)
        h
        (let ((e (seq-first s)))
          (loop (jolt-seq (seq-more s))
                (i32 (+ h (bitwise-xor (jolt-java-hashcode (jolt-nth e 0))
                                       (jolt-java-hashcode (jolt-nth e 1))))))))))
(define ap-map-statics
  (list (cons "mapHash" ap-map-hash) (cons "mapHasheq" jolt-hash)))
(register-class-statics! "APersistentMap" ap-map-statics)
(register-class-statics! "clojure.lang.APersistentMap" ap-map-statics)

;; clojure.lang.Murmur3 — the hash primitives clojure.core/hash is built on,
;; called directly by libraries that hash a composite value by hand (malli's
;; regex parser combines a function hashCode with a position and a register
;; map). hasheq.ss already holds the port; this only names it for interop.
;; hashOrdered/hashUnordered take any seqable, as the Java Iterable overloads do.
(define murmur3-statics
  (list (cons "hashInt" (lambda (n) (murmur3-hash-int (jolt->fx n))))
        (cons "hashLong" (lambda (n) (murmur3-hash-long (jolt->fx n))))
        (cons "hashUnencodedChars"
              (lambda (s) (murmur3-hash-unencoded-chars (jolt-final-str s))))
        (cons "hashOrdered" (lambda (xs) (hash-ordered (jolt-seq xs))))
        (cons "hashUnordered" (lambda (xs) (hash-unordered (jolt-seq xs))))
        (cons "mixCollHash"
              (lambda (h n) (mix-coll-hash (jolt->fx h) (jolt->fx n))))))
(register-class-statics! "Murmur3" murmur3-statics)
(register-class-statics! "clojure.lang.Murmur3" murmur3-statics)

;; clojure.lang.RT/map: build a map from a [k v k v…] array/seq (RT.map). Small
;; maps keep insertion order (PersistentArrayMap). tools.reader builds map and
;; namespaced-map literals this way.
(define (rt-map arr)
  (let loop ((xs (if (jolt-nil? arr) '() (seq->list (jolt-seq arr)))) (m (jolt-hash-map)))
    (cond ((null? xs) m)
          ((null? (cdr xs)) (throw-jvm (quote IllegalArgumentException) "RT/map: odd key/value count"))
          (else (loop (cddr xs) (jolt-assoc m (car xs) (cadr xs)))))))
(register-class-statics! "RT" (list (cons "map" rt-map)))
(register-class-statics! "clojure.lang.RT" (list (cons "map" rt-map)))

;; clojure.lang.RT/iter: an Iterator over any seqable — the same jiterator
;; (.iterator coll) dispatches to. orchard/inspect.analytics walks collections
;; through it.
;; One procedure for both spellings, like nextID below: the two names share ONE
;; member table (register-class-statics! mirrors the FQN to the short name), so a
;; fresh closure per spelling re-registers the member with a different value and
;; JOLT_DEBUG reports the runtime's own boot as registry drift.
(define (rt-iter coll) (make-jiterator (jolt-seq coll)))
(register-class-statics! "RT" (list (cons "iter" rt-iter)))
(register-class-statics! "clojure.lang.RT" (list (cons "iter" rt-iter)))

;; clojure.lang.RT/REQUIRE_LOCK — the JVM's `static final Object REQUIRE_LOCK`.
;; Nothing inside require takes it; it is the AGREED lock a caller holds around a
;; require so two threads do not load the same namespace at once, and its whole
;; value is that everyone reaches for the same object. clojure.core/serialized-require
;; locks it, and so does io.github.frenchy64.fully-satisfies.requiring-resolve —
;; which typedclojure's runtime requires, and which read the field REFLECTIVELY
;; ((.getField clojure.lang.RT "REQUIRE_LOCK")) and fell back to locking
;; #'clojure.core/require when it was absent. A plain (Object.), the same value
;; the JVM field holds: locking only needs identity, and every jolt value has a
;; monitor (java/concurrency.ss object-monitor).
(define rt-require-lock (make-jhost "object" (vector)))
(register-class-statics! "RT" (list (cons "REQUIRE_LOCK" rt-require-lock)))
(register-class-statics! "clojure.lang.RT" (list (cons "REQUIRE_LOCK" rt-require-lock)))

;; clojure.lang.PersistentList/create: a list (in order) from a seq; empty -> ().
;; Build it the way clojure.core/list does — list->cseq alone yields plain seq
;; cells, and jolt marks the HEAD cell to record that a chain IS a list, so an
;; unmarked one answers `list?` false while still reporting class PersistentList.
;; clojure.tools.reader reads every list through here, and a consumer that asks
;; `list?` (clojure.tools.namespace's ns-decl?, say) would reject the result.
(define (plist-create x)
  (let ((items (seq->list (jolt-seq x))))
    (if (null? items) jolt-empty-list (apply jolt-list items))))
(register-class-statics! "PersistentList" (list (cons "create" plist-create)))
(register-class-statics! "clojure.lang.PersistentList" (list (cons "create" plist-create)))

;; clojure.lang.PersistentHashSet/createWithCheck: a set from a seq, throwing on a
;; duplicate element (tools.reader's #{…} reader reports the dup). The throw is
;; the JVM's IllegalArgumentException, as PersistentHashMap's is above — it was
;; an ex-info, which a caller catching the reference's class did not see.
(define (phs-create-with-check x)
  (let loop ((xs (seq->list (jolt-seq x))) (s (jolt-hash-set)))
    (if (null? xs) s
        (let ((e (car xs)))
          (if (jolt-truthy? (jolt-contains? s e))
              (throw-jvm (quote IllegalArgumentException) (string-append "Duplicate key: " (jolt-str-render-one e)))
              (loop (cdr xs) (jolt-conj1 s e)))))))
(register-class-statics! "PersistentHashSet" (list (cons "createWithCheck" phs-create-with-check)))
(register-class-statics! "clojure.lang.PersistentHashSet" (list (cons "createWithCheck" phs-create-with-check)))

;; java.lang.Character statics. digit(ch, radix) -> the digit value or -1; ch may
;; be a char or an int codepoint (tools.reader passes (int c)), as it may for
;; every predicate below; valueOf boxes a char (identity on jolt).
(define (char->cp x) (if (char? x) (char->integer x) (jnum->exact x)))
(define (char-digit-value cp radix)
  (let ((d (cond ((and (fx>=? cp 48) (fx<=? cp 57)) (fx- cp 48))            ; 0-9
                 ((and (fx>=? cp 97) (fx<=? cp 122)) (fx+ 10 (fx- cp 97)))  ; a-z
                 ((and (fx>=? cp 65) (fx<=? cp 90)) (fx+ 10 (fx- cp 65)))   ; A-Z
                 (else 99))))
    (if (fx<? d radix) d -1)))
;; Character's getType/isDigit/isWhitespace/isUpperCase/isLowerCase/TYPE and the
;; category constants are registered in one block further below; these are the
;; codepoint conversion statics.
(register-class-statics! "java.lang.Character"
  (list (cons "digit" (lambda (ch radix) (->num (char-digit-value (char->cp ch) (jnum->exact radix)))))
        (cons "toChars" (lambda (cp) (na-char-array (jolt-vector (integer->char (char->cp cp))))))
        (cons "valueOf" (lambda (ch) (if (char? ch) ch (integer->char (char->cp ch)))))))

;; java.util.regex.Pattern statics (compile/quote/MULTILINE) are registered once
;; in host-static-classes.ss — the flags-aware compile there subsumes the plain
;; one-arg form this file used to register.

;; clojure.lang.BigInt / clojure.lang.Numbers: jolt has one exact-integer type
;; (Chez bignums auto-reduce), so BigInt.fromBigInteger and Numbers.reduceBigInt
;; are identity. tools.reader's number parser threads integers through these.
(define identity-num-statics (list (cons "fromBigInteger" (lambda (x) x))))
(register-class-statics! "BigInt" identity-num-statics)
(register-class-statics! "clojure.lang.BigInt" identity-num-statics)
(register-class-statics! "clojure.lang.Numbers"
  (list (cons "reduceBigInt" (lambda (x) x)) (cons "toRatio" (lambda (x) x))
        (cons "inc" jolt-inc) (cons "dec" jolt-dec)
        (cons "unchecked_inc" jolt-uncinc) (cons "unchecked_dec" jolt-uncdec)
        (cons "isZero" jolt-zero?) (cons "isPos" jolt-pos?) (cons "isNeg" jolt-neg?)
        (cons "add" jolt-add2) (cons "minus" jolt-sub2) (cons "multiply" jolt-mul2)
        (cons "unchecked_add" jolt-uncadd2) (cons "unchecked_minus" jolt-uncsub2)
        (cons "unchecked_multiply" jolt-uncmul2) (cons "remainder" jolt-rem)
        (cons "lt" jolt-lt2) (cons "gt" jolt-gt2)
        (cons "lte" jolt-le2) (cons "gte" jolt-ge2)
        (cons "equiv" jolt-num-equiv)))

(define (now-millis)
  (let ((t (current-time 'time-utc)))
    (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))
;; The clock every DEADLINE in the runtime is on: milliseconds from an arbitrary
;; origin, never stepping (System/nanoTime's clock, in millis). now-millis is the
;; wall clock, for dating things; a deadline read off it fires early or holds on
;; for hours when ntp or a user steps the clock mid-wait (#1303).
(define (mono-millis)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))
;; (mono-deadline ms) — the deadline ms from now, the one way to build one. A
;; positive timeout's deadline is EXACT: a rational millisecond off the clock read
;; at full precision, so a wait ends when its time has passed, as the JDK's timed
;; waits (all on System.nanoTime) do. It used to be a whole millisecond rounded up
;; from now -- rounded up so a wait never returned early -- and with the wake
;; latency on top, (deref p 1 nil) took 2ms where the JDK takes 1.1. ms may be
;; fractional (a TimeUnit finer than a millisecond, Thread.sleep's nanos).
;; A timeout of zero or less is due already: a whole millisecond off the truncated
;; now, so mono-due? answers it from the cheap clock.
(define (mono-deadline ms)
  (if (<= ms 0)
      (let ((t (current-time 'time-monotonic)))
        (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000)
           (exact (floor ms))))
      (mono-deadline-exact ms)))

;; A deadline is a number of mono-millis: a fixnum (a due one, or one built from a
;; whole-millisecond clock read by a caller) or an exact rational. The two compare
;; on one clock below.
(define (mono-nanos)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000000000 (time-second t)) (time-nanosecond t))))
(define (mono-deadline-exact ms)
  (+ (/ (mono-nanos) 1000000) (exact ms)))
;; (mono-ms-until deadline) — the milliseconds left until a deadline, exact and
;; negative once it has passed, for a poll loop that sizes its naps by it.
(define (mono-ms-until deadline)
  (- deadline (/ (mono-nanos) 1000000)))
;; (mono-due? deadline) — has the deadline passed. For a whole deadline this is
;; exactly (>= (mono-millis) deadline); an exact one is read at full precision.
(define (mono-due? deadline)
  (if (fixnum? deadline)
      (>= (mono-millis) deadline)
      (>= (mono-nanos) (* 1000000 deadline))))
;; (mono-duration-until deadline) — the time-duration from now until a deadline,
;; never negative, for a relative wait. Now is read at full precision: a wait of
;; (- deadline (mono-millis)) ms starts from the truncated millisecond and so
;; overshoots by however far into that millisecond now was.
(define (mono-duration-until deadline)
  (wait-ns->duration (- (exact (ceiling (* 1000000 deadline))) (mono-nanos))))
;; A relative wait of NS nanoseconds (clamped at 0) as a time-duration. Chez on
;; Windows rounds a timed wait to the NEAREST millisecond, so a tail under half
;; a millisecond waited 0 ms and the caller's loop spun until its deadline; there
;; the wait rounds up to whole milliseconds, the platform's own granularity.
(define wait-round-to-ms? (eq? (sa-os-family) 'windows))
(define (wait-ns->duration ns)
  (let* ((ns (max 0 ns))
         (ns (if wait-round-to-ms? (* 1000000 (div (+ ns 999999) 1000000)) ns)))
    (make-time 'time-duration (mod ns 1000000000) (div ns 1000000000))))

;; clojure.core/current-time-ms — epoch milliseconds.
(def-var! "clojure.core" "current-time-ms" (lambda () (->num (now-millis))))
;; clojure.core/current-time-ns — the MONOTONIC nanosecond clock, and what the
;; `time` macro measures with. The JVM's time is (System/nanoTime)-based, and the
;; two properties that buys matter: an interval shorter than a millisecond is
;; visible rather than 0, and no clock adjustment can make an elapsed time come out
;; negative. current-time-ms is the wall clock and has neither. Kept as a core fn
;; rather than having the macro reach for System/nanoTime so the core overlay does
;; not depend on the java class-statics layer.
(def-var! "clojure.core" "current-time-ns" (lambda () (->num (jolt-mono-nanos))))
(register-class-statics! "System"
  (list (cons "currentTimeMillis" (lambda () (->num (now-millis))))
        ;; System/nanoTime is the JVM's MONOTONIC high-resolution timer, not the
        ;; wall clock in finer units — its origin is arbitrary and only differences
        ;; between two readings are meaningful. It was (* 1000000 (now-millis)):
        ;; wall-clock derived, so an ntp step could run it backwards, and
        ;; millisecond-granular, so any interval under a millisecond timed as 0.
        ;; jolt-mono-nanos (rt.ss) is Chez's 'time-monotonic clock, which is both.
        (cons "nanoTime" (lambda () (->num (jolt-mono-nanos))))
        ;; jolt-exit-process (rt.ss), not Chez's exit: this has to end the PROCESS
        ;; from whichever thread called it, the way System.exit does.
        (cons "exit" (lambda args (jolt-exit-process (if (null? args) 0 (jnum->exact (car args))))))
        ;; System/gc -> a full Chez collection (so weak references clear and their
        ;; guardians fire); Runtime.gc() routes here too.
        (cons "gc" (lambda _
                     ;; System.gc is a HINT on the JVM and never throws; Chez's
                     ;; collect refuses when multiple threads are active, so a
                     ;; guarded no-op is the faithful behavior under live threads.
                     (guard (e (#t #f)) (jolt-collect-full!))
                     jolt-nil))
        ;; No finalizers on this host, so running them is genuinely a no-op — which
        ;; is also all the JVM promises (a hint, deprecated for removal since 18).
        ;; criterium calls it alongside System/gc to settle the heap before timing.
        (cons "runFinalization" (lambda _ jolt-nil))
        ;; wrapped in lambdas: the helpers are defined below, resolved at call time.
        (cons "getProperty" (lambda (k . d) (apply sys-get-property k d)))
        (cons "setProperty" (lambda (k v) (sys-set-property k v)))
        (cons "clearProperty" (lambda (k) (sys-clear-property k)))
        (cons "getProperties" (lambda () (sys-properties-map)))
        ;; getenv() is the whole environment, getenv(name) one variable — and
        ;; nothing else: spelled as a rest arg, (System/getenv "HOME" 1) answered
        ;; the variable instead of the JVM's arity miss.
        (cons "getenv" (case-lambda (() (sys-getenv)) ((k) (sys-getenv k))))
        ;; System/console is nil when there is no attached terminal (piped /
        ;; redirected) — the safe default here; libraries (pretty) use it to
        ;; decide whether to emit ANSI, and a nil means "not a tty".
        (cons "console" (lambda _ jolt-nil))
        (cons "lineSeparator" (lambda _ "\n"))
        ;; identityHashCode is the object's own hash, not its contents' —
        ;; equal-hash answers one constant for every procedure and hashes a
        ;; collection structurally, so two distinct fns (and two equal vectors)
        ;; came back with the same "identity". jolt-identity-hasheq is the
        ;; per-object id a weak side table hands out, which is also what
        ;; (hash f) reports, exactly as on the JVM.
        ;; nil is the one input with no object to identify, and the JVM answers 0
        ;; for it rather than hashing the reference.
        (cons "identityHashCode"
              (lambda (x) (if (jolt-nil? x) 0 (->num (jolt-identity-hasheq x)))))
        ;; System.arraycopy(src, srcPos, dest, destPos, length). Specified to
        ;; behave as if the source range were copied to a temporary first, so a
        ;; copy that OVERLAPS within one array still reads pre-copy values —
        ;; walking backwards when the ranges overlap forwards is what gives that
        ;; without the temporary.
        (cons "arraycopy" (lambda (src src-pos dst dst-pos len)
                            (sys-arraycopy src src-pos dst dst-pos len)))))

(define (sys-arraycopy src src-pos dst dst-pos len)
  (when (or (jolt-nil? src) (jolt-nil? dst))
    (throw-jvm 'NullPointerException "arraycopy source or destination is nil"))
  (unless (and (jolt-array? src) (jolt-array? dst))
    (throw-jvm 'ArrayStoreException "arraycopy operands must be arrays"))
  ;; A copy between different element kinds is the JVM's ArrayStoreException;
  ;; only reference arrays may differ (the JVM then checks each element as it
  ;; stores, which jolt does not — its element store is unchecked, see
  ;; known-divergences :typed-array-store), so two reference kinds are compatible.
  (unless (or (eq? (jolt-array-kind src) (jolt-array-kind dst))
              (and (na-ref-kind? (jolt-array-kind src)) (na-ref-kind? (jolt-array-kind dst))))
    (throw-jvm 'ArrayStoreException "arraycopy between arrays of different types"))
  (let ((sp (na-idx src-pos)) (dp (na-idx dst-pos)) (n (na-idx len))
        (slen (ja-len src)) (dlen (ja-len dst)))
    (when (or (negative? sp) (negative? dp) (negative? n)
              (> (+ sp n) slen) (> (+ dp n) dlen))
      (jolt-throw (jolt-host-throwable "java.lang.ArrayIndexOutOfBoundsException"
                                       "arraycopy: last source index out of bounds")))
    ;; ja-copy-range! (natives-array.ss) owns the move: a block copy between two
    ;; bytevector, flvector or fxvector backings, and the descending walk when the regions overlap
    ;; forwards inside one array — which is what gives the JVM's "as if the source
    ;; were copied to a temporary first" without the temporary.
    (ja-copy-range! src sp dst dp n)
    jolt-nil))

;; java.lang.Long.bitCount: the population count of the value's 64-bit two's-
;; complement (mask to 64 bits so a negative long counts like the JVM, e.g.
;; bitCount(-1) = 64). test.check's splittable PRNG uses it.
(define long-mask64 #xFFFFFFFFFFFFFFFF)
(define long-2^63 (expt 2 63))
(define long-2^64 (expt 2 64))
;; interpret a 64-bit value as a signed long (top bit = sign), like the JVM.
(define (as-signed64 v) (if (>= v long-2^63) (- v long-2^64) v))
;; and back: a negative long as its 64-bit unsigned form, for the unsigned string
;; conversions (Long.toHexString(-1) is "ffffffffffffffff").
(define (long->u64 n) (if (< n 0) (+ n long-2^64) n))
(define (long-nlz n) (- 64 (integer-length (bitwise-and (jnum->exact n) long-mask64))))
(define (long-reverse n)
  (let ((v (bitwise-and (jnum->exact n) long-mask64)))
    (let loop ((i 0) (r 0))
      (if (fx=? i 64) (as-signed64 r)
          (loop (fx+ i 1)
                (bitwise-ior (bitwise-arithmetic-shift-left r 1)
                             (bitwise-and (bitwise-arithmetic-shift-right v i) 1)))))))
(register-class-statics! "Long"
  (list (cons "TYPE" "long")
        (cons "MAX_VALUE" (->num 9223372036854775807))
        (cons "MIN_VALUE" (->num -9223372036854775808))
        (cons "bitCount" (lambda (n) (->num (bitwise-bit-count (bitwise-and (jnum->exact n) long-mask64)))))
        (cons "numberOfLeadingZeros" (lambda (n) (->num (long-nlz n))))
        (cons "reverse" (lambda (n) (->num (long-reverse n))))
        (cons "parseLong" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "long")))
        (cons "decode" (lambda (s) (decode-or-throw s "long")))
        (cons "valueOf" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "long")))
        (cons "compare" (lambda (x y) (let ((a (jnum->exact x)) (b (jnum->exact y)))
                                        (->num (cond ((< a b) -1) ((> a b) 1) (else 0))))))
        ;; toHexString/toOctalString/toBinaryString are UNSIGNED (the value's 64-bit
        ;; two's complement, lowercase); toString with a radix is signed magnitude.
        ;; Chez's number->string emits uppercase hex digits, so downcase them all —
        ;; Integer/toString had the same slip ((Integer/toString -255 16) was "-FF").
        (cons "toHexString" (lambda (x) (string-downcase (number->string (long->u64 (jnum->exact x)) 16))))
        (cons "toOctalString" (lambda (x) (number->string (long->u64 (jnum->exact x)) 8)))
        (cons "toBinaryString" (lambda (x) (number->string (long->u64 (jnum->exact x)) 2)))
        ;; radix 10 of a fixnum is the digit loop (values.ss jolt-fixnum->string);
        ;; another radix, or a bignum, is Chez's printer
        (cons "toString" (lambda (x . r)
                           (let ((n (jnum->exact x)))
                             (if (and (null? r) (fixnum? n))
                                 (jolt-fixnum->string n)
                                 (string-downcase (number->string n (if (null? r) 10 (jnum->exact (car r)))))))))))

;; JVM Integer.toHexString/etc. treat the int as 32-bit unsigned.
(define (int->u32 n) (if (< n 0) (+ n 4294967296) n))
(register-class-statics! "Integer"
  (list (cons "MAX_VALUE" (->num 2147483647)) (cons "MIN_VALUE" (->num -2147483648))
        ;; the primitive class token (int.class); jolt models a class as its name
        (cons "TYPE" "int")
        (cons "valueOf" (lambda (x . r)
                          (if (number? x) (->num x)
                              (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "int"))))
        (cons "parseInt" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "int")))
        (cons "decode" (lambda (x) (decode-or-throw x "int")))
        ;; Integer.compare(int, int): -1/0/1 exactly, not an arbitrary sign value.
        (cons "compare" (lambda (x y) (let ((a (jnum->exact x)) (b (jnum->exact y)))
                                        (->num (cond ((< a b) -1) ((> a b) 1) (else 0))))))
        ;; Integer.signum(int): -1/0/1 as an int. Math/signum is the double-valued
        ;; one and stays separate.
        (cons "signum" (lambda (x) (let ((a (jnum->exact x)))
                                     (->num (cond ((< a 0) -1) ((> a 0) 1) (else 0))))))
        ;; lowercase, like the JVM; a negative int is the 32-bit unsigned form.
        (cons "toHexString" (lambda (x) (string-downcase (number->string (int->u32 (jnum->exact x)) 16))))
        (cons "toOctalString" (lambda (x) (number->string (int->u32 (jnum->exact x)) 8)))
        (cons "toBinaryString" (lambda (x) (number->string (int->u32 (jnum->exact x)) 2)))
        ;; radix 10 of a fixnum is the digit loop (values.ss jolt-fixnum->string);
        ;; another radix, or a bignum, is Chez's printer
        (cons "toString" (lambda (x . r)
                           (let ((n (jnum->exact x)))
                             (if (and (null? r) (fixnum? n))
                                 (jolt-fixnum->string n)
                                 (string-downcase (number->string n (if (null? r) 10 (jnum->exact (car r)))))))))
        ;; Integer.max/min (Java 8): plain two-arg integer max/min, not clojure.core's
        ;; variadic ones — orchard calls them.
        (cons "max" (lambda (x y) (->num (max (jnum->exact x) (jnum->exact y)))))
        (cons "min" (lambda (x y) (->num (min (jnum->exact x) (jnum->exact y)))))))

;; Byte / Short bounds (their values are plain integers on jolt; the statics let
;; libraries reference the JVM ranges — clojure.test.check generates over them).
(register-class-statics! "Byte"
  (list (cons "TYPE" "byte")
        (cons "MAX_VALUE" (->num 127)) (cons "MIN_VALUE" (->num -128))
        (cons "valueOf" (lambda (x . r) (->num (if (number? x) x (parse-int-or-throw x 10 "byte")))))
        (cons "parseByte" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "byte")))
        (cons "decode" (lambda (x) (decode-or-throw x "byte")))
        ;; interpret the low 8 bits as unsigned (0..255): a signed byte -1 -> 255.
        (cons "toUnsignedLong" (lambda (x) (->num (bitwise-and (jnum->exact x) #xFF))))
        (cons "toUnsignedInt" (lambda (x) (->num (bitwise-and (jnum->exact x) #xFF))))
        (cons "toString" (lambda (x . r) (number->string (jnum->exact x))))))
(register-class-statics! "Short"
  (list (cons "TYPE" "short")
        (cons "MAX_VALUE" (->num 32767)) (cons "MIN_VALUE" (->num -32768))
        (cons "valueOf" (lambda (x . r) (->num (if (number? x) x (parse-int-or-throw x 10 "short")))))
        (cons "parseShort" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "short")))
        (cons "decode" (lambda (x) (decode-or-throw x "short")))
        (cons "toString" (lambda (x . r) (number->string (jnum->exact x))))))


;; java.lang.Void has no values — it exists so a caller can NAME the void
;; primitive type, which is the only thing TYPE is for here. sci's prim->class
;; maps every primitive name to its class token and stops at the first one
;; missing, so the whole of sci.core failed to load without it.
(register-class-statics! "Void" (list (cons "TYPE" "void")))
(register-class-statics! "java.lang.Void" (list (cons "TYPE" "void")))

(register-class-statics! "Boolean"
  (list (cons "TYPE" "boolean")
        (cons "parseBoolean" (lambda (s) (string=? "true" (ascii-string-down (if (string? s) s (jolt-str-render-one s))))))
        (cons "TRUE" #t) (cons "FALSE" #f)))

(register-class-ctor! "Double" ->double)
(register-class-ctor! "Float" (lambda (x) (jolt-parse-float x)))
(register-class-statics! "Double"
  (list (cons "TYPE" "double")
        (cons "parseDouble" parse-double-or-throw)
        (cons "valueOf" ->double)
        (cons "toString" (lambda (x) (jolt-str-render-one (->double x))))
        ;; a Float argument widens, as the double parameter takes it
        (cons "isNaN" (lambda (x) (let ((d (jfloat-unbox x))) (and (flonum? d) (nan? d)))))
        (cons "isInfinite" (lambda (x) (let ((d (jfloat-unbox x))) (and (flonum? d) (infinite? d)))))
        (cons "MAX_VALUE" 1.7976931348623157e308) (cons "MIN_VALUE" 4.9e-324)
        (cons "POSITIVE_INFINITY" +inf.0) (cons "NEGATIVE_INFINITY" -inf.0) (cons "NaN" +nan.0)))
;; Float's values, parsing and printing live with the float codecs in math.ss.
(register-class-statics! "Float" (list (cons "TYPE" "float")))

;; Character: the JVM's classification is the Unicode general category, and Chez's
;; char-general-category IS that property (the same table the JVM reads), so
;; getType is a rename of its symbol to the JVM's constant, and the predicates are
;; the JVM's own category rules: isLetter is L*, isDigit is Nd (a vulgar fraction
;; is numeric but not a digit), isUpperCase/isLowerCase are the Uppercase/Lowercase
;; properties (Lu/Ll plus Other_Uppercase/Other_Lowercase, which is exactly what
;; char-upper-case?/char-lower-case? answer: a Roman numeral is upper case, the
;; feminine ordinal is lower case, a titlecase digraph is neither). They used to be
;; ASCII ranges, so \é was not a letter and \É was not upper case.
;;
;; Every one takes a char or an int codepoint. An int that is not a Unicode scalar
;; value (negative, a surrogate, above U+10FFFF) is no char on this host, and the
;; JVM classifies it as nothing: every predicate is false and getType is
;; UNASSIGNED, except that a surrogate is SURROGATE. A signed high byte reaches
;; these — cognitect aws-api's request signer classifies each UTF-8 byte of a URI
;; through isLetterOrDigit — and is negative, so it must not reach integer->char.
(define (scalar-char x)            ; the char for x, or #f when x is not a scalar value
  (if (char? x) x
      (let ((cp (jnum->exact x)))
        (and (fixnum? cp) (fx>=? cp 0) (fx<=? cp #x10FFFF)
             (not (and (fx>=? cp #xD800) (fx<=? cp #xDFFF)))
             (integer->char cp)))))
(define (char-category-in? x cats)
  (let ((c (scalar-char x)))
    (and c (memq (char-general-category c) cats) #t)))
;; general-category symbol -> java.lang.Character.getType constant. 17 is unused
;; on the JVM as well; Cs is listed for the record, though no char here carries it.
(define char-category-types
  '((Cn . 0) (Lu . 1) (Ll . 2) (Lt . 3) (Lm . 4) (Lo . 5) (Mn . 6) (Me . 7) (Mc . 8)
    (Nd . 9) (Nl . 10) (No . 11) (Zs . 12) (Zl . 13) (Zp . 14) (Cc . 15) (Cf . 16)
    (Co . 18) (Cs . 19) (Pd . 20) (Ps . 21) (Pe . 22) (Pc . 23) (Po . 24)
    (Sm . 25) (Sc . 26) (Sk . 27) (So . 28) (Pi . 29) (Pf . 30)))
(define (char-type x)
  (let ((c (scalar-char x)))
    (if c
        (cdr (assq (char-general-category c) char-category-types))
        (let ((cp (jnum->exact x)))   ; not a char: scalar-char answers every char
          (if (and (fixnum? cp) (fx>=? cp #xD800) (fx<=? cp #xDFFF)) 19 0)))))
(register-class-statics! "java.lang.Character"
  (list (cons "TYPE" "char")
        (cons "getType" (lambda (c) (->num (char-type c))))
        (cons "isUpperCase" (lambda (c) (let ((ch (scalar-char c))) (and ch (char-upper-case? ch)))))
        (cons "isLowerCase" (lambda (c) (let ((ch (scalar-char c))) (and ch (char-lower-case? ch)))))
        (cons "isDigit" (lambda (c) (char-category-in? c '(Nd))))
        (cons "isLetter" (lambda (c) (char-category-in? c '(Lu Ll Lt Lm Lo))))
        (cons "isLetterOrDigit" (lambda (c) (char-category-in? c '(Lu Ll Lt Lm Lo Nd))))
        ;; The getType constants, in JVM order (17 is skipped there too).
        (cons "UNASSIGNED" (->num 0)) (cons "UPPERCASE_LETTER" (->num 1))
        (cons "LOWERCASE_LETTER" (->num 2)) (cons "TITLECASE_LETTER" (->num 3))
        (cons "MODIFIER_LETTER" (->num 4)) (cons "OTHER_LETTER" (->num 5))
        (cons "NON_SPACING_MARK" (->num 6)) (cons "ENCLOSING_MARK" (->num 7))
        (cons "COMBINING_SPACING_MARK" (->num 8)) (cons "DECIMAL_DIGIT_NUMBER" (->num 9))
        (cons "LETTER_NUMBER" (->num 10)) (cons "OTHER_NUMBER" (->num 11))
        (cons "SPACE_SEPARATOR" (->num 12)) (cons "LINE_SEPARATOR" (->num 13))
        (cons "PARAGRAPH_SEPARATOR" (->num 14)) (cons "CONTROL" (->num 15))
        (cons "FORMAT" (->num 16)) (cons "PRIVATE_USE" (->num 18))
        (cons "SURROGATE" (->num 19)) (cons "DASH_PUNCTUATION" (->num 20))
        (cons "START_PUNCTUATION" (->num 21)) (cons "END_PUNCTUATION" (->num 22))
        (cons "CONNECTOR_PUNCTUATION" (->num 23)) (cons "OTHER_PUNCTUATION" (->num 24))
        (cons "MATH_SYMBOL" (->num 25)) (cons "CURRENCY_SYMBOL" (->num 26))
        (cons "MODIFIER_SYMBOL" (->num 27)) (cons "OTHER_SYMBOL" (->num 28))
        (cons "INITIAL_QUOTE_PUNCTUATION" (->num 29)) (cons "FINAL_QUOTE_PUNCTUATION" (->num 30))
        ;; JVM Character.isWhitespace: Unicode whitespace (so U+2028 line separator
        ;; counts, like the JVM) MINUS the no-break spaces the JVM excludes
        ;; (U+00A0/U+2007/U+202F). char<=?space missed everything above ASCII.
        (cons "isWhitespace" (lambda (c) (let ((cp (char-code c)))
                                           (and (char-whitespace? (integer->char cp))
                                                (not (fx=? cp #xA0)) (not (fx=? cp #x2007)) (not (fx=? cp #x202F))))))
        ;; Codepoint bounds, as plain integers. The UTF-16 surrogate API
        ;; (MIN_HIGH_SURROGATE, highSurrogate/lowSurrogate, toCodePoint, …) is
        ;; deliberately absent: a surrogate is not a Unicode scalar value, so it is
        ;; not representable as a char on this host — jolt strings are indexed by
        ;; codepoint, and there are no surrogate halves to hand back.
        (cons "MIN_VALUE" (integer->char 0))
        (cons "MAX_VALUE" (integer->char #xFFFF))
        (cons "MIN_CODE_POINT" (->num 0))
        (cons "MAX_CODE_POINT" (->num #x10FFFF))
        (cons "MIN_SUPPLEMENTARY_CODE_POINT" (->num #x10000))
        (cons "charCount" (lambda (cp) (->num (if (>= (jnum->exact cp) #x10000) 2 1))))
        ;; codePointAt(seq, index) — the codepoint at `index`. jolt strings are
        ;; indexed by codepoint, so this is the code of the char sitting there,
        ;; which is what the JVM answers for every character in the BMP. Above it
        ;; the JVM's index counts UTF-16 units and an astral character occupies
        ;; two; that is the :string-model divergence already recorded, not a
        ;; difference in this method. The char[] overload is accepted too.
        ;; yaml-parser (yamlstar) classifies each input character through this,
        ;; and its own non-JVM branches spell it (int (nth input i)).
        (cons "codePointAt"
              (lambda (s i)
                (let ((idx (jnum->exact i)))
                  (->num (char->integer
                          (if (jolt-array? s)
                              (ja-ref s idx)
                              (string-ref (jolt-str-render-one s) idx)))))))
        ;; Character.codePointOf(name) is deliberately absent: it is a lookup in the
        ;; Unicode character-name database, which this host does not carry, and a
        ;; partial ASCII-only table would answer wrongly rather than not at all.
        ;; So are getNumericValue (the numeric value of a Roman numeral or a vulgar
        ;; fraction is a table Chez does not expose) and reverseBytes (it can answer
        ;; a surrogate, which is no char here).
        ;;
        ;; The case maps are the simple (one-to-one) Unicode mappings, which is
        ;; what char-upcase and its kin answer and what the JVM's char overloads
        ;; are defined by: \ß upper-cases to itself, not to "SS". The int
        ;; overload answers an int, and a value that is no scalar is unchanged.
        (cons "toUpperCase" (lambda (c) (char-case-map c char-upcase)))
        (cons "toLowerCase" (lambda (c) (char-case-map c char-downcase)))
        (cons "toTitleCase" (lambda (c) (char-case-map c char-titlecase)))
        (cons "isTitleCase" (lambda (c) (char-category-in? c '(Lt))))
        (cons "isDefined" (lambda (c) (let ((ch (scalar-char c))) (and ch (not (eq? (char-general-category ch) 'Cn))))))
        (cons "isAlphabetic" (lambda (c) (let ((ch (scalar-char c))) (and ch (char-alphabetic? ch)))))
        (cons "isSpaceChar" (lambda (c) (char-category-in? c '(Zs Zl Zp))))
        (cons "isISOControl" (lambda (c) (let ((cp (char->cp c)))
                                           (or (and (>= cp 0) (<= cp #x1F)) (and (>= cp #x7F) (<= cp #x9F))))))
        ;; the identifier rules the JVM's javadoc states: a start is a letter, a
        ;; letter number, a currency symbol or a connector; a part adds digits, the
        ;; two combining marks and the ignorable controls and formats
        (cons "isJavaIdentifierStart" (lambda (c) (char-category-in? c '(Lu Ll Lt Lm Lo Nl Sc Pc))))
        (cons "isJavaIdentifierPart"
              (lambda (c) (or (char-category-in? c '(Lu Ll Lt Lm Lo Nl Sc Pc Nd Mn Mc Cf))
                              (let ((cp (char->cp c)))
                                (or (and (>= cp 0) (<= cp 8)) (and (>= cp #xE) (<= cp #x1B))
                                    (and (>= cp #x7F) (<= cp #x9F)))))))
        (cons "isSurrogate" (lambda (c) (jolt-need-char c) #f))   ; no char here is one
        (cons "compare" (lambda (a b) (->num (- (char->integer (jolt-need-char a)) (char->integer (jolt-need-char b))))))
        (cons "hashCode" (lambda (c) (->num (char->integer (jolt-need-char c)))))
        (cons "toString"
              (lambda (c)
                (if (char? c)
                    (string c)
                    (let ((ch (scalar-char c)))
                      (if ch
                          (string ch)
                          (throw-jvm 'IllegalArgumentException
                            (string-append "Not a valid Unicode code point: 0x"
                                           (string-upcase (number->string (jnum->exact c) 16)))))))))
        ))

;; A char argument of an overload that takes only a char.
(define (jolt-need-char x) (if (char? x) x (jolt-cast-throw x "java.lang.Character")))
;; A Character case map over a char or an int codepoint (see toUpperCase above).
(define (char-case-map x f)
  (if (char? x)
      (f x)
      (let ((ch (scalar-char x)))
        (->num (if ch (char->integer (f ch)) (jnum->exact x))))))

;; String/valueOf(Object): "null" for nil, else jolt's str semantics.
;; String/format(fmt args…) / (locale fmt args…) -> the clojure.core format engine.
;; A locale's decimal and grouping separators. Core carries ROOT ("." and ",") and
;; jolt-lang/time registers the rest — the same split as ::date-names, and
;; :fallback :default for the same reason: the JVM falls back to ROOT for an
;; unknown locale rather than failing, so :default reproduces that mechanism and a
;; locale supplied for a pattern with no numeric directive still gets an answer.
(let ((kw (lambda (n) (keyword #f n))))
  (jolt-register-extension-point! (kw "number-symbols")
    (jolt-hash-map
      (kw "key") (kw "string")
      (kw "root") ""
      (kw "fields") (jolt-hash-map (kw "decimal-sep") (kw "string")
                                   (kw "grouping-sep") (kw "string"))
      (kw "default") (jolt-hash-map (kw "decimal-sep") "." (kw "grouping-sep") ",")
      (kw "fallback") (kw "default")
      (kw "hint") "The jolt-lang/time library carries per-locale number symbols.")))
(define (number-symbol id name dflt)
  (let* ((data (jolt-extension-value (keyword #f "number-symbols") id))
         (v (jolt-get-dispatch data (keyword #f name) jolt-nil)))
    (if (jolt-nil? v) dflt v)))

;; String.CASE_INSENSITIVE_ORDER uses the same character-wise comparison as
;; String.compareToIgnoreCase. class-hierarchy.ss maps its private nested tag.
(register-host-methods! "string-ci-comparator"
  (list (cons "compare" (lambda (self a b) (jvm-string-ci-compare a b)))))
(define string-ci-comparator (make-jhost "string-ci-comparator" #f))
;; String.valueOf(char[]) is the chars as a string, not the array's own rendering —
;; it answered "#object[[C]" where (String. ca) already gave "hi". The three-arg
;; overload is valueOf(char[], offset, count) and nothing else: it USED to be the
;; one-arg arm with the offset and count dropped on the floor, so (String/valueOf
;; ca 1 2) answered the whole array. A case-lambda states the two arities, which is
;; what lets the invocation layer reject any other (host-static.ss host-arity-ok?).
;;
;; copyValueOf IS valueOf — the JDK defines the two as the same value at both
;; arities, and it was simply missing.
;; char-array-arg? / char-array-chunk are host-static-classes.ss, loaded after this
;; file and resolved at call time.
(define string-value-of
  (case-lambda
    ((x) (cond ((jolt-nil? x) "null")
               ((char-array-arg? x) (char-array->string x))
               (else (jolt-str-render-one x))))
    ((x offset count)
     (unless (char-array-arg? x)
       (throw-jvm (quote ClassCastException)
                  (string-append "class " (jolt-class-name x) " cannot be cast to class [C")))
     (char-array-chunk x (jnum->exact offset) (jnum->exact count)
                       (quote StringIndexOutOfBoundsException)))))
(register-class-statics! "String"
  (list (cons "CASE_INSENSITIVE_ORDER" string-ci-comparator)
        (cons "valueOf" string-value-of)
        (cons "copyValueOf" string-value-of)
        ;; String.join(delim, elems) — elems as a collection or spread as varargs,
        ;; the two shapes the JVM overloads on.
        (cons "join" (lambda (delim . parts)
                       (let ((items (if (and (fx=? (length parts) 1)
                                             (not (string? (car parts))))
                                        (seq->list (jolt-seq (car parts)))
                                        parts)))
                         (str-join-strs (map jolt-str-render-one items)
                                        (jolt-str-render-one delim)))))
        ;; String.format([Locale] fmt, Object...): jvm-format-string below
        (cons "format" (lambda (a . rest) (jvm-format-string a rest)))))

;; The text of a JVM format(…) call — String.format, and PrintStream's and
;; PrintWriter's printf/format, which all take ([Locale] String Object...). A
;; lone array argument is the varargs array itself, which is what the JVM's
;; varargs compiles to and what Selmer writes, so it is splatted. The leading
;; argument is a Locale when it is not the format string; testing for the core
;; "locale" jhost tag alone missed the Locale jolt-lang/time installs (a tagged
;; table). The locale only picks the decimal separator: the JVM renders %.3f of
;; 123.04455 as "123,045" under de.
(define (jvm-format-string a rest) (jvm-format-pieces a rest #f))
;; The same, handing SINK each piece java.util.Formatter would append separately
;; (natives-format.ss jolt-format*) — PrintWriter/PrintStream .printf.
(define (jvm-format-pieces a rest sink)
  (let* ((locale? (and (pair? rest) (not (string? a))))
         (fmt (if locale? (car rest) a))
         (args (if locale? (cdr rest) rest))
         ;; jolt-array? / ja->list live in natives-array.ss, loaded after this
         ;; file — resolved at call time.
         (args (if (and (pair? args) (null? (cdr args)) (jolt-array? (car args)))
                   (ja->list (car args))
                   args)))
    (if locale?
        (parameterize ((format-decimal-sep
                        (number-symbol (jolt-str-render-one a) "decimal-sep" ".")))
          (jolt-format* sink fmt args))
        (jolt-format* sink fmt args))))

;; ---- java.text.NumberFormat -------------------------------------------------
;; A grouping decimal formatter (selmer number-format / cuerdas). state:
;; #(grouping? min-frac max-frac). .format groups the integer part with commas.
(define (nf-make grouping? minf maxf) (make-jhost "numberformat" (vector grouping? minf maxf #f)))
;; A currency instance carries its locale's data in slot 3; everything else is a
;; plain decimal instance and leaves it #f.
(define (nf-make-currency cur minf maxf) (make-jhost "numberformat" (vector #t minf maxf cur)))
(define (nf-currency-of self) (vector-ref (jhost-state self) 3))
(define (group-int-str* s sep)           ; "1234567" -> "1,234,567" with sep between groups
  (let* ((neg (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
         (digs (if neg (substring s 1 (string-length s)) s))
         (n (string-length digs)) (out '()))
    (let loop ((i 0))
      (when (< i n)
        (when (and (> i 0) (= 0 (modulo (- n i) 3)))
          (set! out (cons sep out)))
        (set! out (cons (string (string-ref digs i)) out)) (loop (+ i 1))))
    (apply string-append (if neg "-" "") (reverse out))))
(define (nf-format self x)
  (let* ((cur (nf-currency-of self))
         (cfield (lambda (name dflt)
                   (if cur (let ((v (jolt-get-dispatch cur (keyword #f name) jolt-nil)))
                             (if (jolt-nil? v) dflt v))
                       dflt)))
         (grouping? (vector-ref (jhost-state self) 0))
         (minf (vector-ref (jhost-state self) 1)) (maxf (vector-ref (jhost-state self) 2))
         (dec-sep (cfield "decimal-sep" "."))
         (grp-sep (cfield "grouping-sep" ","))
         (neg (< x 0)) (ax (abs (exact->inexact x)))
         (scale (expt 10 maxf))
         ;; Chez's round is round-half-to-EVEN, which is what the JVM's currency
         ;; format does (1234.5 -> 1234, 2.5 -> 2 at zero fraction digits).
         (scaled (exact (round (* ax scale))))
         (ipart (quotient scaled scale)) (fpart (remainder scaled scale))
         (istr (number->string ipart))
         (fstr0 (if (> maxf 0) (let ((s (number->string fpart)))
                                 (string-append (make-string (max 0 (- maxf (string-length s))) #\0) s)) ""))
         ;; trim trailing zeros down to minf
         (fstr (let loop ((s fstr0)) (if (and (> (string-length s) minf)
                                              (char=? (string-ref s (- (string-length s) 1)) #\0))
                                         (loop (substring s 0 (- (string-length s) 1))) s)))
         (num (string-append (if grouping? (group-int-str* istr grp-sep) istr)
                             (if (> (string-length fstr) 0) (string-append dec-sep fstr) ""))))
    ;; The JVM puts the minus sign ahead of the whole thing, symbol included:
    ;; -¤ 123.45, -$123.45, -123,45 €.
    (string-append (if neg "-" "")
                   (if cur
                       (let ((sym (cfield "symbol" "")) (sep (cfield "symbol-sep" "")))
                         (if (jolt-truthy? (cfield "symbol-first?" #t))
                             (string-append sym sep num)
                             (string-append num sep sym)))
                       num))))
(register-host-methods! "numberformat"
  (list (cons "format" (lambda (self n) (nf-format self n)))
        (cons "setMaximumFractionDigits" (lambda (self d) (vector-set! (jhost-state self) 2 (jnum->exact d)) jolt-nil))
        (cons "setMinimumFractionDigits" (lambda (self d) (vector-set! (jhost-state self) 1 (jnum->exact d)) jolt-nil))
        (cons "setGroupingUsed" (lambda (self b) (vector-set! (jhost-state self) 0 (jolt-truthy? b)) jolt-nil))))
;; A currency instance's symbol, separators and fraction-digit count are per-locale
;; CLDR data core does not carry. Declared as an extension point instead: core
;; answers for the ROOT locale (the java.util.Locale.ROOT formatting the JVM does,
;; ¤ + NBSP + digits) and is :strict beyond it, so an unregistered locale raises
;; naming itself rather than formatting a German amount with root separators.
;; jolt-lang/time owns java.util.Locale and registers the rest (RFC 0008).
;;
;; The key is a locale ID STRING, not a Locale: core has no Locale class, and
;; rendering the argument through jolt-str-render-one reaches the library's Locale
;; (registered with a :str yielding its id) without core naming the class.
(let ((kw (lambda (n) (keyword #f n))))
  (jolt-register-extension-point! (kw "currency-data")
    (jolt-hash-map
      (kw "key") (kw "string")
      (kw "root") ""
      (kw "fields") (jolt-hash-map (kw "symbol") (kw "string")
                                   (kw "symbol-sep") (kw "string")
                                   (kw "symbol-first?") (kw "boolean")
                                   (kw "decimal-sep") (kw "string")
                                   (kw "grouping-sep") (kw "string")
                                   (kw "frac-digits") (kw "long"))
      ;; Locale.ROOT: currency sign U+00A4, NBSP, then the amount.
      (kw "default") (jolt-hash-map (kw "symbol") "\xa4;"
                                    (kw "symbol-sep") "\xa0;"
                                    (kw "symbol-first?") #t
                                    (kw "decimal-sep") "."
                                    (kw "grouping-sep") ","
                                    (kw "frac-digits") 2)
      (kw "fallback") (kw "strict")
      (kw "hint") "The jolt-lang/time library carries per-locale currency data.")))

;; (getCurrencyInstance) with no argument is the ROOT locale, not the process
;; default the JVM would use — core has no notion of a default locale. Documented
;; in known-divergences.
(define (nf-currency-instance . args)
  (let* ((key (if (null? args) "" (jolt-str-render-one (car args))))
         (data (jolt-extension-value (keyword #f "currency-data") key))
         (frac (let ((f (jolt-get-dispatch data (keyword #f "frac-digits") jolt-nil)))
                 (if (jolt-nil? f) 2 f))))
    (nf-make-currency data frac frac)))

(let ((nf-statics (list (cons "getInstance" (lambda _ (nf-make #t 0 3)))
                        (cons "getNumberInstance" (lambda _ (nf-make #t 0 3)))
                        (cons "getIntegerInstance" (lambda _ (nf-make #t 0 0)))
                        (cons "getCurrencyInstance" nf-currency-instance))))
  (register-class-statics! "NumberFormat" nf-statics)
  (register-class-statics! "java.text.NumberFormat" nf-statics))

;; ---- java.text.Normalizer ----------------------------------------------------
;; Unicode normalization: identifier comparison, path equality on filesystems
;; that store decomposed accents, and search/fuzzy matching all go through it.
;; Chez implements all four Unicode normalization forms natively, so the Form
;; constant only has to pick one of them; java/text-normalize.ss is what stands
;; between that and the call, so text that needs no normalizing does not get
;; rebuilt character by character to prove it.
;;
;; Form is an enum, and jolt models an enum constant the way TimeUnit does: a
;; jhost carrying its name, so (str Normalizer$Form/NFC) is "NFC" as on the JVM
;; and (= f Normalizer$Form/NFC) compares the one interned constant. The
;; constants are registered under the binary name (Normalizer$Form), which is
;; how the analyzer spells a nested class; registering the FQN registers the
;; short name with it.
(define (normalizer-form? x) (and (jhost? x) (string=? (jhost-tag x) "normalizer-form")))
(define (normalizer-form-name f) (vector-ref (jhost-state f) 0))
(define normalizer-form-constants
  (map (lambda (nm) (cons nm (make-jhost "normalizer-form" (vector nm))))
       '("NFC" "NFD" "NFKC" "NFKD")))
(register-str-render! normalizer-form? normalizer-form-name)
(register-host-methods! "normalizer-form"
  (list (cons "name" normalizer-form-name)
        (cons "toString" normalizer-form-name)))
;; The JVM's normalize(CharSequence, Form) rejects a null form with an NPE and
;; has no other failure mode. A form that is not one of the four constants can
;; only come from jolt code that built one by hand, so it names itself in the
;; message rather than reading as a missing method.
(define (normalizer-form-index form)
  (let ((nm (if (normalizer-form? form) (normalizer-form-name form) (jolt-str-render-one form))))
    (cond ((string=? nm "NFC")  0)
          ((string=? nm "NFD")  1)
          ((string=? nm "NFKC") 2)
          ((string=? nm "NFKD") 3)
          (else (throw-jvm (quote IllegalArgumentException)
                           (string-append "Normalizer/normalize: not a Normalizer.Form: " nm))))))
;; Both entry points run over java/text-normalize.ss's per-character
;; classification rather than straight at Chez: text that is already normalized
;; is recognized by a scan and returned as it came in, and isNormalized answers
;; at the first character that disagrees instead of normalizing the whole string
;; to compare it (gh-1066).
(define (normalizer-normalize s form)
  (jtn-normalize (jolt-str-render-one s) (normalizer-form-index form)))
(register-class-statics! "java.text.Normalizer"
  (list (cons "normalize" normalizer-normalize)
        (cons "isNormalized"
              (lambda (s form)
                (jtn-normalized? (jolt-str-render-one s) (normalizer-form-index form))))))
(register-class-statics! "java.text.Normalizer$Form" normalizer-form-constants)

;; Class.forName: an array descriptor ("[C") is its own class token; a class Jolt
;; can back (registered statics/ctor, or a java.*/clojure.* core class) yields a
;; class object; anything else throws a catchable ClassNotFoundException, like the
;; JVM — so the common `(try (Class/forName "optional.Dep") (catch …))` probe a
;; library uses to detect an absent dependency works (e.g. ring's joda-time check).
;; java.* / clojure.* packages jolt models in the class graph are known;
;; optional backends a library feature-probes with (Class/forName …) (e.g.
;; tools.logging's java.util.logging) are absent from the graph and get
;; a definitive ClassNotFoundException.
(define (forname-known? nm)
  ;; exact FQN lookups only — no suffix matching. The JVM throws
  ;; ClassNotFoundException for any unknown FQN even when the last segment
  ;; matches a known class (e.g. "com.acme.String" when "java.lang.String"
  ;; exists). jch-known? does last-segment matching and is NOT used here;
  ;; it lives on for jch-isa?'s suffix matching (round-6 territory).
  ;; And a full name only: the statics table is keyed by the short name as
  ;; well, and jch-known-exact? holds each simple segment (the spelling
  ;; chez-condition-exc-class hands over), where (Class/forName "String") is a
  ;; ClassNotFoundException on the JVM. Every class the graph models answers,
  ;; interfaces included — the set resolve answers for.
  (and (forname-qualified? nm)
       (or (hashtable-ref class-statics-tbl nm #f)
           (hashtable-ref class-ctors-tbl nm #f)
           (jch-known-exact? nm))))
(define (forname-qualified? nm)
  (let loop ((i 0))
    (and (< i (string-length nm))
         (or (char=? (string-ref nm i) #\.) (loop (+ i 1))))))
;; A namespace with a hyphen munges to an underscore in the package name, so a
;; record defined in my-app.core is my_app.core.Foo on the JVM. jolt keeps the
;; namespace as written, so a forName of the munged name goes through the class
;; graph's registered-name lookup (class-hierarchy.ss jch-registered-name, the
;; one canonicalizer every name seam shares) — that is the name a library
;; computes from (munge (str *ns*)). The interned token, so the answer is the
;; very token the type's values report.
(define (class-for-name nm . _)
  (cond
    ;; an array class by its JVM name ("[I", "[Ljava.lang.String;") is a Class
    ;; like any other, when its innermost component is one forName knows
    ((and (> (string-length nm) 0) (char=? (string-ref nm 0) #\[))
     (let ((parts (hsc-array-parts nm)))
       (if (and parts (or (member (car parts) jclass-primitive-names)
                          (string=? (car parts) "java.lang.Object")
                          (forname-known? (car parts))
                          (let ((c (jch-registered-name (car parts)))) (and c (forname-known? c)))))
           (jolt-class-for nm)
           (jolt-throw (jolt-host-throwable "java.lang.ClassNotFoundException" nm)))))
    ((forname-known? nm) (jolt-class-for nm))
    ((let ((c (jch-registered-name nm))) (and c (forname-known? c) c)) => jolt-class-for)
    (else (jolt-throw (jolt-host-throwable "java.lang.ClassNotFoundException" nm)))))
(register-class-statics! "Class" (list (cons "forName" class-for-name)))
;; clojure.lang.RT's own class lookups (the analyzer's resolve path): the same
;; answer as Class/forName. Non-loading is a distinction without a difference
;; here — nothing is initialized on lookup.
;; clojure.lang.Util's comparison statics and the empty list, as clojure.core's
;; gvec (vector-of) calls them; SeqIterator over a seq is the iterator arm.
(define util-extra-statics
  (list (cons "compare" (lambda (a b) (jolt-compare a b)))
        (cons "isInteger" (lambda (x) (if (and (number? x) (exact? x) (integer? x)) #t #f)))
        (cons "equals" (lambda (a b) (if (jolt= a b) #t #f)))))
(register-class-statics! "Util" util-extra-statics)
(register-class-statics! "clojure.lang.Util" util-extra-statics)
(register-class-statics! "PersistentList" (list (cons "EMPTY" jolt-empty-list)))
(register-class-statics! "clojure.lang.PersistentList" (list (cons "EMPTY" jolt-empty-list)))
(register-class-ctor! "SeqIterator" (lambda (s) (make-jiterator (jolt-seq s))))
(register-class-ctor! "clojure.lang.SeqIterator" (lambda (s) (make-jiterator (jolt-seq s))))
(register-class-statics! "RT"
  (list (cons "classForName" class-for-name) (cons "classForNameNonLoading" class-for-name)))
(register-class-statics! "clojure.lang.RT"
  (list (cons "classForName" class-for-name) (cons "classForNameNonLoading" class-for-name)))

;; clojure.lang.RT's value statics: what the reference's inlined core fns compile
;; to (count, nth, get, seq, aset, the primitive casts, ...) and so what a
;; library reaches for by name -- SCI's analyzer rewrites a two-argument (get m k)
;; call to RT/get, its aset on a primitive array goes through RT/aset, and its fn
;; adapters cast a return through RT/longCast; (aset ^longs a i v) inside SCI died
;; here. Each is the native the clojure.core fn it stands for is bound to (ns.ss's
;; core table), so the answer is that fn's; the seed-defined ones (find, subvec)
;; go through their var, resolved per call, since the seed loads after this file.
(define (rt-core-var name)
  (lambda args (apply jolt-invoke (var-deref "clojure.core" name) args)))
(define rt-value-statics
  (list (cons "get" jolt-get) (cons "nth" jolt-nth) (cons "count" jolt-count)
        (cons "seq" jolt-seq) (cons "first" jolt-first) (cons "next" jolt-next)
        (cons "more" jolt-rest) (cons "cons" jolt-cons) (cons "conj" jolt-conj)
        (cons "assoc" jolt-assoc) (cons "dissoc" jolt-dissoc)
        (cons "contains" jolt-contains?) (cons "find" (rt-core-var "find"))
        (cons "keys" jolt-keys) (cons "vals" jolt-vals)
        (cons "peek" jolt-peek) (cons "pop" jolt-pop) (cons "subvec" (rt-core-var "subvec"))
        ;; one array representation, so the typed aset/aget overloads are one
        ;; each; aget/alength are the core fns, which know an array (jolt-nth
        ;; alone does not)
        (cons "aget" (rt-core-var "aget"))
        (cons "aset" (lambda (arr i v) (jolt-ref-put! arr (jnum->exact i) v) v))
        (cons "alength" (rt-core-var "alength"))
        (cons "booleanCast" jolt-boolean) (cons "charCast" jolt-char)
        (cons "byteCast" jolt-byte-cast) (cons "shortCast" jolt-short-cast)
        (cons "intCast" jolt-int-cast) (cons "longCast" jolt-long-cast)
        (cons "floatCast" jolt-float) (cons "doubleCast" jolt-double)
        (cons "isReduced" jolt-reduced-pred)
        ;; list has fixed-arity overloads, so it takes the elements; vector and
        ;; set are Object... only, an ARRAY to a reflective caller (which is what
        ;; a static call is), like RT/map
        (cons "list" jolt-list)
        (cons "vector" (lambda (arr) (apply jolt-vector (seq->list (jolt-seq arr)))))
        ;; RT.set is PersistentHashSet.createWithCheck: a duplicate element throws
        (cons "set" phs-create-with-check)))
(register-class-statics! "RT" rt-value-statics)
(register-class-statics! "clojure.lang.RT" rt-value-statics)

;; ---- System helpers (defined before use above via top-level order) ----------
;; os.name reflects the actual platform (Chez's machine-type names it): a *osx
;; machine is macOS, otherwise Linux. Code that branches on the OS (socket struct
;; layout, path handling) needs the truth, not a fixed value.
;; Optimized: character-by-character scan, no substring allocation per position.
(define (substring-index needle hay)
  (let ((nl (string-length needle)) (hl (string-length hay)))
    (let outer ((i 0))
      (if (> (+ i nl) hl)
          #f
          (let inner ((j 0))
            (cond ((= j nl) i)
                  ((char=? (string-ref hay (+ i j)) (string-ref needle j)) (inner (+ j 1)))
                  (else (outer (+ i 1)))))))))
(define sys-os-name
  (case (sa-os-family)
    ((macos) "Mac OS X")
    ((windows) "Windows")
    (else "Linux")))
;; os.arch in the JVM's spelling — libraries parse these exact strings
;; (aarch64/amd64), not Chez's. An unrecognized arch answers the host tag,
;; which is at least truthful.
(define sys-os-arch
  (case (sa-arch)
    ((arm64) "aarch64")
    ((x86-64) "amd64")
    ((i386) "x86")
    (else (sa-host-tag))))
;; user.name: the login name from the environment, as the JVM reports it.
(define (sys-user-name)
  (or (getenv "USER") (getenv "LOGNAME") (getenv "USERNAME") jolt-nil))
;; os.version, computed once on first read: the macOS PRODUCT version (what
;; the JVM reports there — the kernel release is the wrong number for "which
;; macOS is this"), the kernel release elsewhere, nil where the adapter cannot
;; say (Windows). Read in process (sa-os-release): no program is run for it.
;; A failed probe caches "" so it is not retried.
(define sys-os-version-cache #f)
(define (sys-os-version)
  (cond
    (sys-os-version-cache
     (if (string=? sys-os-version-cache "") jolt-nil sys-os-version-cache))
    (else
     (let ((v (or (sa-os-release) "")))
       (set! sys-os-version-cache v)
       (if (string=? v "") jolt-nil v)))))
;; runtime-settable system properties (System/setProperty). A set value wins over
;; the built-in defaults below; clearProperty removes it.
;; Written at RUN time (System/setProperty) from whatever thread calls it, so the
;; mutations and the whole-table scan take a mutex; the single-key read on the
;; getProperty path stays unlocked (strong general table — see rt.ss's var-table
;; note). read-then-write is one step so the returned previous value is the one
;; this call actually replaced.
(define sys-prop-mu (make-mutex))
(define sys-prop-table (make-hashtable string-hash string=?))
(define (sys-set-property k v)
  (jolt-with-mutex sys-prop-mu
    (let ((prev (hashtable-ref sys-prop-table k jolt-nil)))
      (hashtable-set! sys-prop-table k (if (string? v) v (jolt-str-render-one v)))
      prev)))
(define (sys-clear-property k)
  (jolt-with-mutex sys-prop-mu
    (let ((prev (hashtable-ref sys-prop-table k jolt-nil)))
      (hashtable-delete! sys-prop-table k) prev)))
;; The PATH-LIST separator is not the file separator, and Windows is the reason
;; they must be told apart: ":" is the drive suffix there, so ";" separates list
;; entries. Answering ":" everywhere cut every Windows entry in half —
;; (babashka.fs/split-paths "C:/a;C:/b") came back as ["C" "/a;C" "/b"], so
;; fs/exec-paths was garbage, fs/which never found anything, and
;; babashka.process's Windows resolver threw before a spawn was attempted
;; (jolt-lang/jolt#1074).
;;
;; The FILE separator is "\\" on Windows, and File and Path render with it
;; (java/io.ss path-native), so the property, File/separator and every rendered
;; path agree (jolt-lang/jolt#1110).
;;
;; Parameterized by platform, and defined in this file rather than beside the
;; other path helpers in java/io.ss, because this file loads first (rt.ss) and
;; System/getProperty is the caller closest to the load point.
(define (path-list-separator-for windows?) (if windows? ";" ":"))
(define (path-list-separator) (path-list-separator-for (eq? (sa-os-family) 'windows)))
(define (file-separator-for windows?) (if windows? "\\" "/"))
(define (file-separator) (file-separator-for (eq? (sa-os-family) 'windows)))

;; java.io.tmpdir. TMPDIR is the POSIX spelling and the only one this chain
;; knew, so on Windows — which sets TEMP and TMP and not TMPDIR — it answered
;; "/tmp": a directory on whichever drive the process happened to be on, which
;; is where every File/createTempFile, every jolt.host temp write and every
;; resolver download went (jolt-lang/jolt#1074). TMPDIR still wins everywhere,
;; because a caller that sets it means it and the https-fetch smoke sets it on
;; Windows too.
;;
;; GETENV* is a parameter so the rows a Linux runner cannot reach are still
;; pinned (test/chez/win-platform-test.ss), and one definition serves all three
;; callers — System/getProperty, File/createTempFile (java/io.ss) and
;; Files/createTempFile (java/nio-file.ss) — because two hand-kept copies is how
;; java.io and java.nio.file start disagreeing about a path.
(define (host-temp-dir-for windows? getenv*)
  (define (named k) (let ((v (getenv* k))) (and v (> (string-length v) 0) v)))
  (or (named "TMPDIR")
      (and (not windows?) "/tmp")
      (named "TEMP")
      (named "TMP")
      ;; Nothing named one. SystemRoot is the variable Windows always sets (the
      ;; OS probe in scheme-adapter-runtime.ss keys off it), and <SystemRoot>\Temp
      ;; exists on every install; "C:/Windows/Temp" is the last resort for a
      ;; process started with no environment at all.
      (let ((sr (named "SystemRoot"))) (and sr (string-append sr "/Temp")))
      "C:/Windows/Temp"))
(define (host-temp-dir) (host-temp-dir-for (eq? (sa-os-family) 'windows) getenv))

;; java.class.path — jolt's equivalent of the JVM classpath is the resolved
;; source roots (project :paths + every dependency's roots), which a project
;; command installs with set-source-roots! before it runs anything. Editor
;; tooling ported from the JVM reads this property to find project sources
;; (orchard's namespace/classpath scan, compliment's classpath completion
;; sources, cider-nrepl's classpath op), so answer it from the live roots rather
;; than leaving it unset — an unset value made those return nothing at all.
;; The roots live in the loader, which is layered ABOVE the runtime (the gate
;; harness boots rt.ss without it), so the loader installs the provider and this
;; answers "" until it does.
(define sys-class-path-provider (lambda () '()))
(define (set-class-path-provider! f) (set! sys-class-path-provider f))
(define (sys-class-path)
  (let ((sep (path-list-separator)))
    (let loop ((roots (sys-class-path-provider)) (acc ""))
      (cond ((null? roots) acc)
            ((string=? acc "") (loop (cdr roots) (car roots)))
            (else (loop (cdr roots) (string-append acc sep (car roots))))))))

(define (sys-get-property k . dflt)
  (let* ((k (jolt-need-string k))
         (set-val (hashtable-ref sys-prop-table k #f)))
    (cond (set-val set-val)
          ((string=? k "os.name") sys-os-name)
          ((string=? k "os.arch") sys-os-arch)
          ((string=? k "os.version") (sys-os-version))
          ((string=? k "user.name") (sys-user-name))
          ((string=? k "jolt.version") (jolt-version-string))
          ((string=? k "line.separator") "\n")
          ((string=? k "file.separator") (file-separator))
          ((string=? k "path.separator") (path-list-separator))
          ((string=? k "java.class.path") (sys-class-path))
          ;; user.dir is the user's cwd (JVM: the process cwd). jolt-user-dir (io.ss)
          ;; owns that chain — JOLT_PWD, then the process's own working directory —
          ;; so the property and every relative path resolve against the same place.
          ((string=? k "user.dir") (jolt-user-dir))
          ((string=? k "user.home") (or (getenv "HOME") ""))
          ((string=? k "java.io.tmpdir") (host-temp-dir))
          ((pair? dflt) (car dflt))
          (else jolt-nil))))
;; System/getProperties: a java.util.Properties holding the same values
;; System/getProperty answers one at a time. make-properties-jhost is defined in
;; host-static-classes.ss, which loads after this file — resolved at call time.
(define (sys-properties-map)
  (make-system-properties (sys-properties-pmap) sys-prop-table))
(define (sys-properties-pmap)
  (let ((base (jolt-hash-map "os.name" sys-os-name "os.arch" sys-os-arch
                             "line.separator" "\n" "file.separator" (file-separator)
                             "path.separator" (path-list-separator)
                             "java.class.path" (sys-class-path)
                             "jolt.version" (jolt-version-string)
                             "user.dir" (jolt-user-dir) "user.home" (or (getenv "HOME") "")
                             "java.io.tmpdir" (host-temp-dir))))
    ;; keys whose value can be absent join only when they answer — a JVM
    ;; Properties never holds a nil value
    (let ((v (sys-os-version)))
      (unless (jolt-nil? v) (set! base (jolt-assoc base "os.version" v))))
    (let ((v (sys-user-name)))
      (unless (jolt-nil? v) (set! base (jolt-assoc base "user.name" v))))
    (for-each
      (lambda (kv)
        (set! base (jolt-assoc base (car kv) (cdr kv))))
      (vector->list (jolt-with-mutex sys-prop-mu (hashtable-cells sys-prop-table))))
    base))

;; The full environment as an alist of (name . value), read from the process's
;; own table (sa-environment-pairs) — never by running a program: the `env -0`
;; this used to spawn needed an env program on PATH, and on Windows it is not
;; a command, so (System/getenv) answered an empty map and every ProcessBuilder
;; child started from an empty environment (host/chez/no-external-programs-smoke.sh).
(define (all-env-pairs) (sa-environment-pairs))
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

;; (Object.) — a fresh value with distinct identity (libraries use it as a lock
;; or a unique sentinel). Each call returns a new jhost so identical?/= separate.
(register-class-ctor! "Object" (lambda _ (make-jhost "object" (vector))))
;; …and it reports java.lang.Object. Without this arm it fell off the end of
;; jolt-class-name's chain onto jolt-type's internal keyword, so (class (Object.))
;; answered :object — and that keyword was what count's refusal message named.
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "object")))
                     (lambda (x) "java.lang.Object"))
;; Object's own methods: identity hashCode (what hash answers for it), identity
;; equals, and toString as str renders it — the default Object.toString,
;; java.lang.Object@<hex identity hash> (host-class.ss jolt-identity-hex).
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "object")))
                      (lambda (x) (string-append "java.lang.Object@" (jolt-identity-hex x))))
(register-host-methods! "object"
  (list (cons "hashCode" (lambda (o) (jolt-hasheq o)))
        (cons "equals" (lambda (o x) (eq? o x)))
        (cons "toString" (lambda (o) (jolt-str-render-one o)))))

;; ---- clojure.lang.LispReader$StringReader -----------------------------------
;; The JVM reader's string-literal handler. It is package-private, but a library
;; that wants Clojure's own escape rules without eval reaches for it anyway:
;; instaparse (via test.chuck) instantiates one and applies it to read a grammar's
;; quoted string. On the JVM it is constructed with no args and CALLED as a
;; function — (reader initch) on Clojure <= 1.6, (reader initch opts pending) after
;; — so the ctor here hands back a procedure with that convention rather than an
;; object; nothing ever reads a field or calls a method on it.
;;
;; It consumes the literal's BODY, up to and including the closing quote, and
;; returns the unescaped string. Characters come from the jolt reader the caller
;; passes (with-in-str binds *in* to one). IReader hands out lines rather than
;; chars, so the body is accumulated a line at a time until the terminating quote
;; appears, then parsed by the same helper jolt's own reader uses — so \n, \uXXXX
;; and the octal escapes all agree with the reader proper.
;;
;; Anything after the closing quote is consumed rather than pushed back: IReader
;; has no push-back, and the callers build a reader per literal and drop it. A
;; caller that shared one reader across reads would lose the remainder, so this is
;; deliberately not registered as a general-purpose reader.
(define (lrsr-terminated? s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((fx>=? i n) #f)
            ((char=? (string-ref s i) #\\) (loop (fx+ i 2)))
            ((char=? (string-ref s i) #\") #t)
            (else (loop (fx+ i 1)))))))
(define (lrsr-read-literal rdr)
  (let ((read-line-fn (var-deref "clojure.core" "-read-line")))
    (let loop ((acc ""))
      (if (lrsr-terminated? acc)
          (let-values (((v _) (rdr-read-string-lit acc 0 (string-length acc)))) v)
          (let ((line (jolt-invoke1 read-line-fn rdr)))
            (if (jolt-nil? line)
                (throw-jvm 'RuntimeException "EOF while reading string")
                (loop (string-append acc line "\n"))))))))
(for-each (lambda (nm)
            (register-class-ctor! nm (lambda _ (lambda (rdr . _) (lrsr-read-literal rdr)))))
          '("LispReader$StringReader" "clojure.lang.LispReader$StringReader"))
