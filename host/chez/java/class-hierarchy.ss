;; class-hierarchy.ss — one JVM class/interface graph, the single source of truth
;; for every "what classes does this satisfy" question. value-host-tags (protocol
;; dispatch), instance?, isa?/supers/ancestors, and the exception hierarchy all
;; derive from the ONE table here instead of maintaining parallel hand-kept lists
;; that drift apart.
;;
;; The graph is keyed by canonical (FQN) class name -> its DIRECT super
;; interfaces/classes (also FQN). Transitivity is computed (jch-closure), so a row
;; lists only what a class directly extends/implements, matching the JVM source.
;;
;; It is OPEN: a library registers a class and its supers with
;; jolt.host/register-class-supers! (plus a class-arm in host-class.ss to map its
;; values to that class name), and every derived view picks the class up with no
;; core change. Loaded before records.ss so value-host-tags can derive from it.

;; canonical-name -> list of direct super canonical-names. Mutable + extensible.
(define jvm-class-parents (make-hashtable string-hash string=?))
;; closure cache, invalidated whenever the graph is extended. A Chez hashtable
;; corrupts under concurrent WRITES (the damage surfaces later inside the
;; collector, never as an error naming the table), and these two are memo caches on
;; the protocol-dispatch path, so every thread that dispatches fills them.
;;
;; jch-cache-mutex covers every MUTATION of jvm-class-parents, every WHOLE-TABLE
;; scan of it, both derived caches, and jch-graph-epoch. Single-key reads stay
;; unlocked, on the split rt.ss spells out for var-table — these are strong
;; general hashtables, so an unlocked reader walks consistent structure and the
;; worst it sees is a stale miss. That is not a nicety here: jch-tags is on the
;; protocol-dispatch path and jch-closure under it, so a mutex on the read would
;; sit on every protocol call in the program. Steady state is pure reads, so this
;; costs nothing once warm.
;;
;; jch-graph-epoch is the graph's generation, and it does two jobs.
;;
;; Invalidation is a hashtable-clear!, and a clear can be undone. jch-closure /
;; jch-tags compute outside the lock (the walk is not cheap and calls nothing that
;; needs one), so without a generation a walk that began before a jch-set-supers!
;; could publish its pre-change answer after the clear, and every later dispatch
;; would read it. A stale ancestry is a wrong isa? and a wrong protocol method,
;; and it would never expire. Each publish re-checks the generation it computed
;; against.
;;
;; It is also read by callers that derive a per-type answer from the graph and
;; want to revalidate with one fixnum compare instead of re-walking (jrdesc-ifc-of,
;; records.ss) — a deftype gains interfaces after its descriptor exists, and can
;; gain more later through extend-type. Bumped LAST, after the table is written
;; and the memo caches are cleared: a concurrent reader that saw the new epoch
;; first could derive from the old graph and stamp the answer as current, which
;; revalidation could never catch.
(define jch-cache-mutex (make-mutex))
(define jch-graph-epoch 0)
(define jch-closure-cache (make-hashtable string-hash string=?))
(define jch-tags-cache (make-hashtable string-hash string=?))
;; call with jch-cache-mutex HELD
(define (jch-invalidate!/locked)
  (hashtable-clear! jch-closure-cache)
  (hashtable-clear! jch-tags-cache)
  (set! jch-graph-epoch (fx+ jch-graph-epoch 1)))

;; Merge direct supers for a class (union with any already registered). Public so
;; libraries can graft their own classes onto the modeled hierarchy.
(define (jch-register-supers! name supers)
  ;; the read-modify-write is ONE step: two threads grafting different supers
  ;; onto the same class would otherwise each union against the value they read
  ;; and the second write would drop the first's.
  (jolt-with-mutex jch-cache-mutex
    (let ((cur (hashtable-ref jvm-class-parents name '())))
      (hashtable-set! jvm-class-parents name
                      (let add ((ss supers) (acc cur))
                        (cond ((null? ss) acc)
                              ((member (car ss) acc) (add (cdr ss) acc))
                              (else (add (cdr ss) (append acc (list (car ss)))))))))
    (jch-invalidate!/locked)))

;; A munged fn class name "ns$name" (jolt-class for a def'd fn) isn't in the
;; table; like the JVM (a fn extends clojure.lang.AFunction) its super is
;; AFunction, whose registered supers give AFn / IFn / Fn / Runnable / Callable
;; transitively.
(define (str-has-dollar? s)
  (let loop ((i 0)) (and (< i (string-length s)) (or (char=? (string-ref s i) #\$) (loop (+ i 1))))))

;; A row registered with NO supers (java.util.Map$Entry, a marker interface) is
;; still a row: only a name the table has never seen falls to the fn rule. It
;; used to read an empty row as absent, and a $ in the name then made the
;; interface a fn — (supers java.util.Map$Entry) answered the AFunction chain.
(define (jch-direct-supers name)
  (let ((direct (hashtable-ref jvm-class-parents name #f)))
    (cond (direct direct)
          ((str-has-dollar? name) '("clojure.lang.AFunction"))
          (else '()))))

;; Replace a class's direct supers outright (defrecord re-declares the row its
;; deftype half registered). Same cache invalidation as a register.
(define (jch-set-supers! name supers)
  (jolt-with-mutex jch-cache-mutex
    (hashtable-set! jvm-class-parents name supers)
    (set! jch-known-cache #f)
    (set! jch-simple->fqn-cache #f)
    (jch-invalidate!/locked)))

;; transitive supers of NAME (canonical), excluding NAME and Object; Object is the
;; universal root supplied by callers. Breadth-first, deduped, stable order.
(define (jch-closure name)
  (or (hashtable-ref jch-closure-cache name #f)
      (let* ((epoch jch-graph-epoch)      ; read BEFORE the walk — see jch-graph-epoch
             (result
              (let loop ((pending (jch-direct-supers name)) (seen '()))
                (cond ((null? pending) (reverse seen))
                      ((member (car pending) seen) (loop (cdr pending) seen))
                      (else (loop (append (jch-direct-supers (car pending)) (cdr pending))
                                  (cons (car pending) seen)))))))
        (jolt-with-mutex jch-cache-mutex
          (when (fx= epoch jch-graph-epoch) (hashtable-set! jch-closure-cache name result)))
        result)))

;; ns segment munging for a JVM-spelled class name: dashes become underscores
;; (clojure.core-test.x -> clojure.core_test.x).
(define (jch-munge-segments s)
  (list->string (map (lambda (c) (if (char=? c #\-) #\_ c)) (string->list s))))

(define (jch-last-segment s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          ((char=? (string-ref s i) #\$) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

;; The name a NAMESPACE maps a class under — the part after the last dot, $ and
;; all: the JVM imports java.util.Map$Entry as Map$Entry, not as Entry. Distinct
;; from jch-last-segment above, which goes on past the $ because its job is the
;; alternative SPELLING a protocol extension may use for a tag.
(define (jch-import-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

;; The protocol-dispatch / instance? tag list for a value of class NAME: the class
;; and its whole ancestry, each in BOTH canonical and simple spelling (extend-protocol
;; and instance? accept either "Associative" or "clojure.lang.Associative"), plus
;; "Object". Memoized — this is on the hot protocol-dispatch path.
(define (jch-tags name)
  (or (hashtable-ref jch-tags-cache name #f)
      (let* ((epoch jch-graph-epoch)      ; read BEFORE the walk — see jch-graph-epoch
             (chain (cons name (jch-closure name)))
             (result
              (let build ((cs chain) (acc '()))
                (if (null? cs)
                    (reverse (cons "Object" acc))
                    (let* ((fqn (car cs))
                           (simple (jch-last-segment fqn))
                           (acc1 (if (member fqn acc) acc (cons fqn acc)))
                           (acc2 (if (or (string=? simple fqn) (member simple acc1))
                                     acc1 (cons simple acc1))))
                      (build (cdr cs) acc2))))))
        (jolt-with-mutex jch-cache-mutex
          (when (fx= epoch jch-graph-epoch) (hashtable-set! jch-tags-cache name result)))
        result)))

;; Is WANTED (canonical or simple) the class CHILD (canonical) or one of its
;; ancestors? Object is every class's root. Matched by full name or last segment so
;; "IOException" and "java.io.IOException" both hit.
(define (jch-isa? child wanted)
  (let ((wseg (jch-last-segment wanted)))
    (or (string=? wanted "java.lang.Object") (string=? wanted "Object")
        (let loop ((names (cons child (jch-closure child))))
          (cond ((null? names) #f)
                ((or (string=? wanted (car names))
                     (string=? wseg (jch-last-segment (car names)))) #t)
                (else (loop (cdr names))))))))

;; Does the graph model WANTED at all (as a class or as any class's ancestor)? Used
;; by instance? to decide between a definitive #f and 'pass (defer to other arms).
;; Built lazily, and published only once COMPLETE. It used to (set! … (make-…))
;; first and fill afterwards, which put an EMPTY table in the global for the
;; duration of the scan: a second thread calling instance? in that window read it
;; and got a definitive #f for a class the graph does model. Building into a local
;; and publishing with one set! makes the window unobservable, and the
;; double-check under the mutex means two racers agree on one table rather than
;; each filling their own. The hit path — every instance? after the first — is a
;; single global read and no lock.
(define jch-known-cache #f)
(define (jch-known-table)
  (or jch-known-cache
      (jolt-with-mutex jch-cache-mutex
        (or jch-known-cache
            (let ((t (make-hashtable string-hash string=?)))
              (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
                (vector-for-each
                 (lambda (k supers)
                   (hashtable-set! t k #t)
                   (hashtable-set! t (jch-last-segment k) #t)
                   (for-each (lambda (s)
                               (hashtable-set! t s #t)
                               (hashtable-set! t (jch-last-segment s) #t))
                             supers))
                 keys vals))
              (set! jch-known-cache t)
              t)))))
(define (jch-known? wanted)
  ;; bind once: an invalidation between the two probes would otherwise hand the
  ;; second one #f instead of a table
  (let ((t (jch-known-table)))
    (or (hashtable-ref t wanted #f)
        (hashtable-ref t (jch-last-segment wanted) #f))))

;; Exact membership, no last-segment fallback. The fallback above is there so a
;; SIMPLE name answers (chez-condition-exc-class hands over "ArityException"),
;; but it also makes every dotted name whose last segment happens to be modeled —
;; fake.pkg.String, no.such.Class — read as known. A caller asking "does the host
;; back THIS class" rather than "have I seen this name" wants this one.
(define (jch-known-exact? wanted)
  (and (hashtable-ref (jch-known-table) wanted #f) #t))

;; simple last-segment -> canonical FQN for a modeled class (first registered
;; wins). Lets a simple exception name (from chez-condition-exc-class) resolve to
;; its graph key so the exception hierarchy answers through the one graph.
;; Same publish-when-complete rule as jch-known-table above, and for the same
;; reason: a half-filled table here resolves a simple exception name to itself
;; instead of its FQN.
(define jch-simple->fqn-cache #f)
(define (jch-simple->fqn-table)
  (or jch-simple->fqn-cache
      (jolt-with-mutex jch-cache-mutex
        (or jch-simple->fqn-cache
            (let ((t (make-hashtable string-hash string=?)))
              (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
                (vector-for-each
                 (lambda (k supers)
                   (for-each (lambda (n)
                               (let ((seg (jch-last-segment n)))
                                 (when (not (hashtable-ref t seg #f))
                                   (hashtable-set! t seg n))))
                             (cons k supers)))
                 keys vals))
              (set! jch-simple->fqn-cache t)
              t)))))
(define (jch-fqn-of-simple name)
  (or (hashtable-ref (jch-simple->fqn-table) name #f) name))

;; A register also invalidates the derived caches. The mutation and BOTH resets
;; are one critical section, and the resets come AFTER the mutation: resetting
;; first would leave a window where the graph is still the old one, so a
;; concurrent jch-known-table could rebuild from it and publish, and this
;; register would then never invalidate what that thread just cached. (The inner
;; fn takes the same mutex; Chez's are recursive.)
(define jch-register-supers!-inner jch-register-supers!)
(set! jch-register-supers!
  (lambda (name supers)
    (jolt-with-mutex jch-cache-mutex
      (jch-register-supers!-inner name supers)
      (set! jch-known-cache #f)
      (set! jch-simple->fqn-cache #f))))

;; throw-jvm (rt.ss) resolves an unlisted simple exception name through this graph
;; now that it exists — so (throw-jvm 'RuntimeException …) reports
;; java.lang.RuntimeException, not a bare name. rt.ss loads first, so it defaults
;; the fallback to symbol->string until this point.
(set! jvm-throwable-fqn-fallback
  (lambda (sym) (jch-fqn-of-simple (symbol->string sym))))

;; ---- interface marking ---------------------------------------------------------
;; The JVM distinguishes a concrete class (whose bases/supers chain roots at
;; Object) from an interface (whose don't). The graph marks the modeled
;; interfaces; anything unmarked is treated as a concrete class.
(define jch-interface-set (make-hashtable string-hash string=?))
;; written at deftype / defprotocol time from whatever thread defines, so the
;; write is serialized; the read stays unlocked (strong general table)
;; Object is the one name that can never be an interface, and a deftype's
;; `Object` method block (toString / equals / hashCode) files it here like any
;; declared interface — after which .getSuperclass rendered "interface
;; java.lang.Object" and Object's modifiers grew INTERFACE|ABSTRACT. The guard
;; is here, at the one write, not at each reader.
(define (jch-mark-interface! name)
  (unless (or (string=? name "java.lang.Object") (string=? name "Object"))
    (jolt-with-mutex jch-cache-mutex (hashtable-set! jch-interface-set name #t))))
(define (jch-interface? name) (hashtable-ref jch-interface-set name #f))
(for-each jch-mark-interface!
          '("clojure.lang.Seqable" "clojure.lang.Sequential" "clojure.lang.Sorted"
            "clojure.lang.Reversible" "clojure.lang.Indexed" "clojure.lang.Counted"
            "clojure.lang.Named" "clojure.lang.Fn" "clojure.lang.IFn"
            "clojure.lang.IPersistentCollection" "clojure.lang.ISeq"
            "clojure.lang.IChunkedSeq" "clojure.lang.IChunk"
            "clojure.lang.IPending" "clojure.lang.IRef" "clojure.lang.IAtom"
            "clojure.lang.IAtom2" "clojure.lang.IBlockingDeref" "clojure.lang.IMapEntry"
            "clojure.lang.Associative" "clojure.lang.ILookup"
            "clojure.lang.IPersistentStack" "clojure.lang.IPersistentVector"
            "clojure.lang.IPersistentMap" "clojure.lang.IPersistentSet"
            "clojure.lang.IPersistentList" "clojure.lang.IObj" "clojure.lang.IMeta"
            "clojure.lang.IDeref" "clojure.lang.IRecord" "clojure.lang.IType"
            "clojure.lang.IHashEq" "clojure.lang.IEditableCollection"
            "clojure.lang.IExceptionInfo" "clojure.lang.IReduceInit"
            "java.util.List" "java.util.Set" "java.util.Collection" "java.util.Map"
            "java.util.Iterator" "java.lang.Iterable" "java.lang.CharSequence"
            "java.lang.Appendable" "java.lang.Comparable" "java.lang.Runnable"
            "java.util.concurrent.Callable" "java.io.Serializable"
            "java.lang.AutoCloseable" "java.io.Closeable" "java.io.Flushable"
            "java.lang.Readable"
            ;; interfaces the graph modeled as concrete classes, so .isInterface
            ;; answered false and .getSuperclass named a supertype where the JVM
            ;; returns null — java.util.Map$Entry reported clojure.lang.AFunction.
            ;; Derived by probing the reference JVM for every java.* name this
            ;; graph models, not by eye.
            "java.util.Queue" "java.util.Deque" "java.util.Map$Entry"
            "java.nio.file.Path" "java.nio.file.PathMatcher" "java.nio.file.Watchable"
            ;; the typed.clojure rows (probed the same way)
            "java.util.RandomAccess" "java.util.Comparator" "java.util.SequencedCollection"
            "clojure.lang.ITransientCollection" "clojure.lang.ITransientAssociative"
            "clojure.lang.ITransientAssociative2" "clojure.lang.ITransientMap"
            "clojure.lang.ITransientVector" "clojure.lang.ITransientSet"))

;; ---- class modifiers ---------------------------------------------------------
;; Class.getModifiers is a JVM bitmask; jolt derives it from the graph rather than
;; from bytecode it does not have. Visibility is PUBLIC unless the class is in the
;; visibility table below (package-private contributes nothing, private its own
;; bit); a nested class — a $ in a modeled name — is STATIC, since every nested
;; class the graph models is a static nested class on the JVM and jolt models no
;; inner (instance-bound) one; INTERFACE implies ABSTRACT the way javac emits it;
;; and FINAL / ABSTRACT / ENUM come from the marks below.
;;
;; The lists were derived by probing the reference JVM for every java.* class and
;; every nested class this graph models (Class/getModifiers on each), so they say
;; what the JVM says rather than what looked right. A class jolt does not model
;; reports PUBLIC alone — an honest "I know it is a class and nothing more", not
;; a claim of non-finality.
(define jch-mod-public    1)
(define jch-mod-private   2)
(define jch-mod-static    8)
(define jch-mod-final     16)
(define jch-mod-interface 512)
(define jch-mod-abstract  1024)
(define jch-mod-enum      16384)
(define jch-final-set (make-hashtable string-hash string=?))
(define jch-abstract-set (make-hashtable string-hash string=?))
(define jch-enum-set (make-hashtable string-hash string=?))
;; name -> the visibility bits that REPLACE public for that class
(define jch-visibility-tbl (make-hashtable string-hash string=?))
(define (jch-mark-final! name)
  (jolt-with-mutex jch-cache-mutex (hashtable-set! jch-final-set name #t)))
(define (jch-mark-abstract! name)
  (jolt-with-mutex jch-cache-mutex (hashtable-set! jch-abstract-set name #t)))
(define (jch-mark-enum! name)
  (jolt-with-mutex jch-cache-mutex (hashtable-set! jch-enum-set name #t)))
(define (jch-mark-package-private! name)
  (jolt-with-mutex jch-cache-mutex (hashtable-set! jch-visibility-tbl name 0)))
(define (jch-mark-private! name)
  (jolt-with-mutex jch-cache-mutex (hashtable-set! jch-visibility-tbl name jch-mod-private)))
(define (jch-final? name) (and (hashtable-ref jch-final-set name #f) #t))
(define (jch-abstract? name)
  (or (jch-interface? name) (and (hashtable-ref jch-abstract-set name #f) #t)))
(define (jch-enum? name) (and (hashtable-ref jch-enum-set name #f) #t))
;; The bitmask for `name`, resolving a simple name to its FQN first so
;; (.getModifiers String) and (.getModifiers java.lang.String) agree.
(define (jch-modifiers name)
  (let ((n (if (jch-known? name) (jch-fqn-of-simple name) name)))
    (+ (hashtable-ref jch-visibility-tbl n jch-mod-public)
       (if (and (str-has-dollar? n) (jch-known-exact? n)) jch-mod-static 0)
       (if (jch-final? n) jch-mod-final 0)
       (if (jch-interface? n) jch-mod-interface 0)
       (if (jch-abstract? n) jch-mod-abstract 0)
       (if (jch-enum? n) jch-mod-enum 0))))
(for-each jch-mark-final!
          '(
            "java.lang.Boolean" "java.lang.Byte" "java.lang.Character"
            "java.lang.Class" "java.lang.Double" "java.lang.Float"
            "java.lang.Integer" "java.lang.Long" "java.lang.Math"
            "java.lang.Short" "java.lang.String" "java.lang.StringBuilder"
            "java.lang.System" "java.net.URI" "java.time.DayOfWeek"
            "java.time.Duration" "java.time.format.DateTimeFormatter" "java.time.Instant"
            "java.time.LocalDate" "java.time.LocalDateTime" "java.time.LocalTime"
            "java.time.Month" "java.time.OffsetDateTime" "java.time.OffsetTime"
            "java.time.Period" "java.time.temporal.ChronoField" "java.time.temporal.ChronoUnit"
            "java.time.Year" "java.time.YearMonth" "java.time.zone.ZoneRules"
            "java.time.ZonedDateTime" "java.time.ZoneOffset" "java.util.Base64"
            "java.util.Locale" "java.util.regex.Pattern" "java.util.UUID"
            ;; clojure.lang's final classes, from their declarations
            "clojure.lang.ChunkBuffer" "clojure.lang.Volatile" "clojure.lang.Reduced"
            ;; the nested classes, probed: the four transient classes, the two
            ;; final seq classes, and the Thread.State enum
            "clojure.lang.PersistentHashMap$TransientHashMap"
            "clojure.lang.PersistentArrayMap$TransientArrayMap"
            "clojure.lang.PersistentHashSet$TransientHashSet"
            "clojure.lang.PersistentVector$TransientVector"
            "clojure.lang.PersistentVector$ChunkedSeq" "clojure.lang.PersistentHashMap$NodeSeq"
            "java.lang.Thread$State"
            ))
(for-each jch-mark-abstract!
          '(
            "clojure.lang.AMapEntry" "clojure.lang.ATransientMap" "clojure.lang.ATransientSet"
            "java.io.InputStream" "java.io.OutputStream" "java.io.Reader"
            "java.io.Writer" "java.lang.Number" "java.lang.VirtualMachineError"
            "java.lang.ProcessBuilder$Redirect" "java.lang.ref.Reference"
            "java.nio.ByteBuffer" "java.nio.charset.Charset" "java.nio.file.FileSystem"
            "java.time.Clock" "java.time.ZoneId" "java.util.TimeZone"
            ))
(for-each jch-mark-enum!
          '(
            "java.time.DayOfWeek" "java.time.Month" "java.time.temporal.ChronoField"
            "java.time.temporal.ChronoUnit" "java.lang.Thread$State"
            ))
;; the package-private classes the graph models (all nested; probed): the four
;; transient classes and three of clojure.lang's seq classes
(for-each jch-mark-package-private!
          '(
            "clojure.lang.PersistentHashMap$TransientHashMap"
            "clojure.lang.PersistentArrayMap$TransientArrayMap"
            "clojure.lang.PersistentHashSet$TransientHashSet"
            "clojure.lang.PersistentVector$TransientVector"
            "clojure.lang.PersistentArrayMap$Seq" "clojure.lang.PersistentHashMap$NodeSeq"
            "clojure.lang.PersistentList$EmptyList"
            ))
(for-each jch-mark-private!
          '("java.lang.String$CaseInsensitiveComparator"))

;; ---- seed the built-in graph: direct supers only, faithful to the JVM ---------
;; core clojure.lang interfaces
(jch-register-supers! "clojure.lang.IPersistentCollection" '("clojure.lang.Seqable"))
(jch-register-supers! "clojure.lang.ISeq" '("clojure.lang.IPersistentCollection"))
;; the interface chunk-first / chunk-rest are the contract of — a chunked seq is a
;; Sequential ISeq that can also hand out a whole block at a time
(jch-register-supers! "clojure.lang.IChunkedSeq" '("clojure.lang.ISeq" "clojure.lang.Sequential"))
(jch-register-supers! "clojure.lang.Associative" '("clojure.lang.IPersistentCollection" "clojure.lang.ILookup"))
(jch-register-supers! "clojure.lang.IPersistentStack" '("clojure.lang.IPersistentCollection"))
(jch-register-supers! "clojure.lang.IPersistentVector" '("clojure.lang.Associative" "clojure.lang.Sequential"
                                                         "clojure.lang.IPersistentStack" "clojure.lang.Reversible"
                                                         "clojure.lang.Indexed"))
(jch-register-supers! "clojure.lang.IPersistentMap" '("java.lang.Iterable" "clojure.lang.Associative" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.IPersistentSet" '("clojure.lang.IPersistentCollection" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.IPersistentList" '("clojure.lang.Sequential" "clojure.lang.IPersistentStack"))
(jch-register-supers! "clojure.lang.IObj" '("clojure.lang.IMeta"))
;; IFn extends Runnable + Callable only; Fn is NOT a super of IFn. Symbols,
;; keywords and vars are IFn (callable) but must not satisfy Fn — only real
;; fns do, via AFunction's direct Fn row below.
(jch-register-supers! "clojure.lang.IFn" '("java.lang.Runnable" "java.util.concurrent.Callable"))
;; Fn is a marker interface (no supers).
(jch-register-supers! "clojure.lang.AFn" '("clojure.lang.IFn"))
;; AFunction implements java.util.Comparator on the JVM — every fn sorts — so
;; (instance? java.util.Comparator f) and (.compare f a b) hold. Fn stays ahead
;; of it so an extension on Fn / IFn keeps outranking one on Comparator in the
;; dispatch order. The JVM's other two direct interfaces, IObj and Serializable,
;; are NOT here: IObj puts every fn under IMeta for protocol dispatch, a
;; fleet-wide change that needs the library gate first (jolt-tnt7).
(jch-register-supers! "clojure.lang.AFunction" '("clojure.lang.AFn" "clojure.lang.Fn" "java.util.Comparator"))
;; java.util collection interfaces. JDK 21 put SequencedCollection between
;; Collection and List / Deque, and the reference oracle runs on it, so List's
;; ONE direct super is SequencedCollection and Collection arrives transitively.
;; The shape matters, not just the closure: typed.clojure validates an
;; annotation's :replace keys against a class's DIRECT bases. RandomAccess and
;; Comparator are marker-style interfaces with no supers of their own; the rows
;; below graft them onto the classes the JVM declares them on.
(jch-register-supers! "java.util.SequencedCollection" '("java.util.Collection"))
(jch-register-supers! "java.util.List" '("java.util.SequencedCollection"))
(jch-register-supers! "java.util.Set" '("java.util.Collection"))
(jch-register-supers! "java.util.Collection" '("java.lang.Iterable"))
(jch-register-supers! "java.util.RandomAccess" '())
(jch-register-supers! "java.util.Comparator" '())
;; concrete collection classes
(jch-register-supers! "clojure.lang.APersistentVector" '("clojure.lang.IPersistentVector" "java.util.List" "java.util.RandomAccess"))
(jch-register-supers! "clojure.lang.PersistentVector" '("clojure.lang.APersistentVector" "clojure.lang.IObj"
                                                        "java.util.List" "java.lang.Comparable"))
;; subvec's view class (issue #629): an APersistentVector, so every vector
;; check holds; not a PersistentVector, so concrete-class dispatch doesn't
(jch-register-supers! "clojure.lang.APersistentVector$SubVector"
                      '("clojure.lang.APersistentVector" "clojure.lang.IObj"
                        "java.util.List" "java.lang.Comparable"))
(jch-register-supers! "clojure.lang.APersistentMap" '("clojure.lang.IPersistentMap" "java.util.Map"))
(jch-register-supers! "clojure.lang.PersistentArrayMap" '("clojure.lang.APersistentMap" "clojure.lang.IObj"))
(jch-register-supers! "clojure.lang.PersistentHashMap" '("clojure.lang.APersistentMap" "clojure.lang.IObj"))
(jch-register-supers! "clojure.lang.PersistentTreeMap" '("clojure.lang.APersistentMap" "clojure.lang.IObj" "clojure.lang.Sorted" "clojure.lang.Reversible"))
(jch-register-supers! "clojure.lang.APersistentSet" '("clojure.lang.IPersistentSet" "java.util.Set"))
(jch-register-supers! "clojure.lang.PersistentHashSet" '("clojure.lang.APersistentSet" "clojure.lang.IObj"))
(jch-register-supers! "clojure.lang.PersistentTreeSet" '("clojure.lang.APersistentSet" "clojure.lang.IObj" "clojure.lang.Sorted" "clojure.lang.Reversible"))
(jch-register-supers! "clojure.lang.ASeq" '("clojure.lang.ISeq" "clojure.lang.Sequential" "java.util.List"))
(jch-register-supers! "clojure.lang.PersistentList" '("clojure.lang.ASeq" "clojure.lang.IPersistentList" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.PersistentList$EmptyList" '("clojure.lang.PersistentList"))
(jch-register-supers! "clojure.lang.LazySeq" '("clojure.lang.ISeq" "clojure.lang.Sequential" "java.util.List" "clojure.lang.IObj"))
(jch-register-supers! "clojure.lang.Cons" '("clojure.lang.ASeq"))
;; ---- the concrete seq classes -------------------------------------------------
;; One record backs every seq on this host, so which of these a value IS comes from
;; the cell's flavor tag (seq.ss sk-*, mapped to these names in host-class.ss).
;; Each row lists what the JVM class extends, MINUS any interface jolt does not
;; actually honor for that flavor — the graph answers instance?, counted?,
;; ancestors and protocol dispatch alike, so a row that overclaims turns one wrong
;; answer into four. Each omission is called out where it happens.
;;
;; IChunkedSeq is NOT listed on any row here. Which flavors chunk is stated once, by
;; sk-chunked? in seq.ss (the tier that implements chunk-first/chunk-rest), and
;; host-class.ss grafts the interface onto exactly those flavors' classes — so
;; chunked-seq? and (instance? IChunkedSeq x) cannot answer differently.
;;
;; A vector's own seq: Counted is real here — jolt-count reads (pvec-count - ci)
;; without walking, exactly what Counted promises (collections.ss jolt-count).
(jch-register-supers! "clojure.lang.PersistentVector$ChunkedSeq"
                      '("clojure.lang.ASeq" "clojure.lang.Counted"))
;; A standalone chunk plus an arbitrary, possibly lazy rest. NOT Counted — its
;; length is unknown without forcing, and ChunkedCons is not Counted on the JVM either.
(jch-register-supers! "clojure.lang.ChunkedCons" '("clojure.lang.ASeq"))
;; Array and string seqs are realized cell chains here, not indexed views, so
;; count walks them: IndexedSeq (which extends Counted) is deliberately NOT
;; claimed. The JVM's are Counted; jolt answers counted? false rather than
;; promising an O(1) count it would then have to fake.
(jch-register-supers! "clojure.lang.ArraySeq" '("clojure.lang.ASeq"))
(for-each (lambda (prim) (jch-register-supers! (string-append "clojure.lang.ArraySeq$ArraySeq_" prim)
                                               '("clojure.lang.ArraySeq")))
          '("int" "long" "short" "double" "float" "boolean" "byte" "char"))
(jch-register-supers! "clojure.lang.StringSeq" '("clojure.lang.ASeq"))
;; rseq is a lazy descending walk, so likewise not Counted (the JVM's RSeq is).
(jch-register-supers! "clojure.lang.APersistentVector$RSeq" '("clojure.lang.ASeq"))
;; The map/set seq views. PersistentArrayMap$Seq is Counted on the JVM; jolt
;; materializes it into a cell chain, so it is not claimed here either.
(jch-register-supers! "clojure.lang.PersistentArrayMap$Seq" '("clojure.lang.ASeq"))
(jch-register-supers! "clojure.lang.PersistentHashMap$NodeSeq" '("clojure.lang.ASeq"))
(jch-register-supers! "clojure.lang.PersistentTreeMap$Seq" '("clojure.lang.ASeq"))
(jch-register-supers! "clojure.lang.APersistentMap$KeySeq" '("clojure.lang.ASeq"))
(jch-register-supers! "clojure.lang.APersistentMap$ValSeq" '("clojure.lang.ASeq"))
;; A bounded range chunks by 32 like the JVM's (sk-chunked? says so, and the
;; interface is grafted on from there). NOT Counted, though the JVM's is: jolt's
;; range is one chunk followed by a lazy continuation, so it cannot answer its own
;; length without realizing the whole thing.
(jch-register-supers! "clojure.lang.LongRange" '("clojure.lang.ASeq"))
;; The non-all-longs range — (range 0 1.0 0.1) and friends. Same shape as
;; LongRange, and chunked for the same reason.
(jch-register-supers! "clojure.lang.Range" '("clojure.lang.ASeq"))
(jch-register-supers! "clojure.lang.Iterate" '("clojure.lang.ASeq"))
;; (range start end 0), which the JVM answers with Repeat.create(start). Lazy and
;; unbounded, so not chunked and not Counted.
(jch-register-supers! "clojure.lang.Repeat" '("clojure.lang.ASeq"))
(jch-register-supers! "clojure.lang.PersistentQueue" '("clojure.lang.IPersistentList" "clojure.lang.IPersistentCollection" "java.util.Collection"))
;; scalars / named / callable
(jch-register-supers! "clojure.lang.Keyword" '("clojure.lang.IFn" "clojure.lang.Named" "java.lang.Comparable"))
(jch-register-supers! "clojure.lang.Symbol" '("clojure.lang.IObj" "clojure.lang.IFn" "clojure.lang.Named" "java.lang.Comparable"))
;; The reference types extend ARef, which implements IRef — so each IS an IRef,
;; not just an IDeref (IRef extends IDeref, so that still holds transitively).
;; An atom is also the IAtom2 swap/reset surface; Ref and Var are callable.
(jch-register-supers! "clojure.lang.IRef" '("clojure.lang.IDeref"))
(jch-register-supers! "clojure.lang.IAtom" '())
(jch-register-supers! "clojure.lang.IAtom2" '("clojure.lang.IAtom"))
(jch-register-supers! "clojure.lang.Atom" '("clojure.lang.IRef" "clojure.lang.IAtom2"))
(jch-register-supers! "clojure.lang.Ref" '("clojure.lang.IRef" "clojure.lang.IFn" "java.lang.Comparable"))
(jch-register-supers! "clojure.lang.Var" '("clojure.lang.IRef" "clojure.lang.IFn"))
(jch-register-supers! "clojure.lang.Agent" '("clojure.lang.IRef"))
;; the boxes: Volatile and Reduced are plain derefs, a Delay is a pending one.
;; IBlockingDeref is the timed deref a promise or future answers (their reify
;; classes register in concurrency.ss, beside the values that carry them).
(jch-register-supers! "clojure.lang.IBlockingDeref" '())
(jch-register-supers! "clojure.lang.Volatile" '("clojure.lang.IDeref"))
(jch-register-supers! "clojure.lang.Reduced" '("clojure.lang.IDeref"))
(jch-register-supers! "clojure.lang.Delay" '("clojure.lang.IDeref" "clojure.lang.IPending"))
(jch-register-supers! "clojure.lang.Ratio" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "clojure.lang.BigInt" '("java.lang.Number"))
(jch-register-supers! "java.lang.String" '("java.lang.CharSequence" "java.lang.Comparable"))
(jch-register-supers! "java.lang.Long" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "java.lang.Integer" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "java.lang.Double" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "java.lang.Float" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "java.math.BigDecimal" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "java.math.BigInteger" '("java.lang.Number" "java.lang.Comparable"))
(jch-register-supers! "java.lang.Boolean" '("java.lang.Comparable"))
(jch-register-supers! "java.lang.Character" '("java.lang.Comparable"))
(jch-register-supers! "java.util.UUID" '("java.lang.Comparable"))
;; exception hierarchy (folds in the former exception-parent table)
(jch-register-supers! "java.lang.Exception" '("java.lang.Throwable"))
(jch-register-supers! "java.lang.RuntimeException" '("java.lang.Exception"))
(jch-register-supers! "clojure.lang.ExceptionInfo" '("java.lang.RuntimeException" "clojure.lang.IExceptionInfo"))
(jch-register-supers! "java.lang.IllegalArgumentException" '("java.lang.RuntimeException"))
(jch-register-supers! "clojure.lang.ArityException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.lang.NumberFormatException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.util.regex.PatternSyntaxException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.util.IllegalFormatException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.util.IllegalFormatConversionException" '("java.util.IllegalFormatException"))
(jch-register-supers! "java.util.UnknownFormatConversionException" '("java.util.IllegalFormatException"))
(jch-register-supers! "java.lang.IllegalStateException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.UnsupportedOperationException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.ArithmeticException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.NullPointerException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.ClassCastException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.IndexOutOfBoundsException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.util.ConcurrentModificationException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.util.NoSuchElementException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.io.UncheckedIOException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.util.concurrent.RejectedExecutionException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.util.concurrent.ExecutionException" '("java.lang.Exception"))
(jch-register-supers! "java.time.DateTimeException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.time.format.DateTimeParseException" '("java.time.DateTimeException"))
(jch-register-supers! "java.text.ParseException" '("java.lang.Exception"))
(jch-register-supers! "java.lang.InterruptedException" '("java.lang.Exception"))
(jch-register-supers! "java.io.IOException" '("java.lang.Exception"))
(jch-register-supers! "java.io.InterruptedIOException" '("java.io.IOException"))
(jch-register-supers! "java.io.FileNotFoundException" '("java.io.IOException"))
(jch-register-supers! "java.io.UnsupportedEncodingException" '("java.io.IOException"))
(jch-register-supers! "java.io.EOFException" '("java.io.IOException"))
(jch-register-supers! "java.net.UnknownHostException" '("java.io.IOException"))
(jch-register-supers! "java.net.SocketException" '("java.io.IOException"))
(jch-register-supers! "java.net.ConnectException" '("java.net.SocketException"))
(jch-register-supers! "java.net.SocketTimeoutException" '("java.io.InterruptedIOException"))
(jch-register-supers! "java.net.MalformedURLException" '("java.io.IOException"))
(jch-register-supers! "javax.net.ssl.SSLException" '("java.io.IOException"))
(jch-register-supers! "java.nio.charset.UnsupportedCharsetException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.nio.charset.IllegalCharsetNameException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.lang.Error" '("java.lang.Throwable"))
(jch-register-supers! "java.lang.AssertionError" '("java.lang.Error"))
(jch-register-supers! "java.lang.ArrayIndexOutOfBoundsException" '("java.lang.IndexOutOfBoundsException"))
(jch-register-supers! "java.lang.StringIndexOutOfBoundsException" '("java.lang.IndexOutOfBoundsException"))
(jch-register-supers! "java.lang.ReflectiveOperationException" '("java.lang.Exception"))
(jch-register-supers! "java.lang.ClassNotFoundException" '("java.lang.ReflectiveOperationException"))
(jch-register-supers! "java.lang.NoSuchMethodException" '("java.lang.ReflectiveOperationException"))
(jch-register-supers! "java.lang.NoSuchFieldException" '("java.lang.ReflectiveOperationException"))
(jch-register-supers! "java.lang.IllegalAccessException" '("java.lang.ReflectiveOperationException"))
(jch-register-supers! "java.lang.CloneNotSupportedException" '("java.lang.Exception"))
(jch-register-supers! "java.util.concurrent.CancellationException" '("java.lang.IllegalStateException"))
(jch-register-supers! "java.sql.SQLException" '("java.lang.Exception"))
(jch-register-supers! "java.lang.LinkageError" '("java.lang.Error"))
(jch-register-supers! "java.lang.ClassCircularityError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.IncompatibleClassChangeError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.AbstractMethodError" '("java.lang.IncompatibleClassChangeError"))
(jch-register-supers! "java.lang.IllegalAccessError" '("java.lang.IncompatibleClassChangeError"))
(jch-register-supers! "java.lang.NoClassDefFoundError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.UnsatisfiedLinkError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.VirtualMachineError" '("java.lang.Error"))
(jch-register-supers! "java.lang.InternalError" '("java.lang.VirtualMachineError"))
(jch-register-supers! "java.lang.OutOfMemoryError" '("java.lang.VirtualMachineError"))
(jch-register-supers! "java.lang.StackOverflowError" '("java.lang.VirtualMachineError"))
(jch-register-supers! "java.lang.ThreadDeath" '("java.lang.Error"))
(jch-register-supers! "java.lang.Thread" '("java.lang.Runnable"))
(jch-register-supers! "java.io.IOError" '("java.lang.Error"))
;; leaf/root classes with only Object as super
(jch-register-supers! "java.lang.Object" '())
(jch-register-supers! "java.lang.Class" '())
(jch-register-supers! "java.lang.Throwable" '())
;; java.lang.reflect — the member objects Class.getDeclaredFields / getMethods /
;; getConstructors hand back (java/natives-array.ss, java/host-static-classes.ss).
;; The JVM's own shape: Member is the interface all three implement, Executable
;; the abstract class Method and Constructor share, AccessibleObject the root of
;; all of them. Without these rows the member values reported (class m) => :object
;; and printed as #object[:object], which is all a caller ever saw of them.
(jch-register-supers! "java.lang.reflect.Member" '())
(jch-mark-interface! "java.lang.reflect.Member")
(jch-register-supers! "java.lang.reflect.AccessibleObject" '())
(jch-register-supers! "java.lang.reflect.Executable"
                      '("java.lang.reflect.AccessibleObject" "java.lang.reflect.Member"))
(jch-register-supers! "java.lang.reflect.Method" '("java.lang.reflect.Executable"))
(jch-register-supers! "java.lang.reflect.Constructor" '("java.lang.reflect.Executable"))
(jch-register-supers! "java.lang.reflect.Field"
                      '("java.lang.reflect.AccessibleObject" "java.lang.reflect.Member"))
;; statics-only shims (no value ever carries these tags, so the rows cannot
;; shift protocol dispatch) — present so Class.getSuperclass answers Object
;; as the JVM does, jolt-of08.8
(jch-register-supers! "java.lang.Math" '())
(jch-register-supers! "java.lang.System" '())
(jch-register-supers! "java.lang.Byte" '("java.lang.Number"))
(jch-register-supers! "java.lang.Short" '("java.lang.Number"))
;; java.lang.AutoCloseable / java.io.Closeable / java.io.Flushable — the
;; interfaces the stream taxonomy actually implements. with-open and every
;; (instance? java.io.Closeable x) branch reads them, and a stream that reported
;; no interface at all answered false to a question the JVM answers true.
(jch-register-supers! "java.lang.AutoCloseable" '())
(jch-register-supers! "java.io.Closeable" '("java.lang.AutoCloseable"))
(jch-register-supers! "java.io.Flushable" '())
(jch-register-supers! "java.io.InputStream" '("java.io.Closeable"))
(jch-register-supers! "java.io.OutputStream" '("java.io.Closeable" "java.io.Flushable"))
(jch-register-supers! "java.io.Reader" '("java.io.Closeable" "java.lang.Readable"))
(jch-register-supers! "java.lang.Readable" '())
(jch-register-supers! "java.io.Writer" '("java.io.Closeable" "java.io.Flushable" "java.lang.Appendable"))
(jch-register-supers! "java.io.File" '())
(jch-register-supers! "java.io.StringReader" '("java.io.Reader"))
(jch-register-supers! "java.io.PushbackReader" '("java.io.Reader"))
(jch-register-supers! "clojure.lang.LineNumberingPushbackReader" '("java.io.PushbackReader"))
(jch-register-supers! "java.io.PrintWriter" '("java.io.Writer"))
;; System/out and System/err are PrintStreams — byte streams, not the PrintWriter
;; *out* is. Ported code branches on that (instance? java.io.PrintStream x) and
;; hands them to anything taking an OutputStream.
(jch-register-supers! "java.io.FilterOutputStream" '("java.io.OutputStream"))
(jch-register-supers! "java.io.PrintStream" '("java.io.FilterOutputStream" "java.lang.Appendable"))
(jch-register-supers! "java.io.OutputStreamWriter" '("java.io.Writer"))
(jch-register-supers! "java.io.FileWriter" '("java.io.OutputStreamWriter"))
(jch-register-supers! "java.io.InputStreamReader" '("java.io.Reader"))
(jch-register-supers! "java.io.StringWriter" '("java.io.Writer"))
;; StringBuilder is a CharSequence and an Appendable, which is what lets count/seq/
;; nth and the regex entry points take one the way they take a String.
(jch-register-supers! "java.lang.StringBuilder" '("java.lang.CharSequence" "java.lang.Appendable"))
(jch-register-supers! "java.lang.Appendable" '())
(jch-register-supers! "java.util.StringTokenizer" '())
(jch-register-supers! "java.nio.charset.Charset" '())
(jch-register-supers! "java.util.Base64" '())
;; MapEntry extends AMapEntry: an APersistentVector that is also an IMapEntry, the
;; clojure.lang view of java.util.Map.Entry — so the vector checks and
;; (instance? java.util.Map$Entry e) (orchard.print) both hold through one chain.
(jch-register-supers! "clojure.lang.IMapEntry" '("java.util.Map$Entry"))
(jch-register-supers! "clojure.lang.AMapEntry" '("clojure.lang.APersistentVector" "clojure.lang.IMapEntry"))
(jch-register-supers! "clojure.lang.MapEntry" '("clojure.lang.AMapEntry"))
(jch-register-supers! "java.util.Map$Entry" '())
;; chunk building: a ChunkBuffer is Counted, and IChunk — the block chunk seals
;; one into on the JVM — is Indexed. jolt seals a chunk into a plain vector, so
;; nothing here is an IChunk (known-divergences, :seq-type-model).
(jch-register-supers! "clojure.lang.ChunkBuffer" '("clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.IChunk" '("clojure.lang.Indexed"))
(jch-register-supers! "clojure.lang.Namespace" '())
(jch-register-supers! "java.util.regex.Pattern" '())
(jch-register-supers! "java.net.URI" '())
(jch-register-supers! "java.util.ArrayList" '("java.util.List" "java.util.RandomAccess"))
(jch-register-supers! "java.util.Queue" '("java.util.Collection"))
(jch-register-supers! "java.util.Deque" '("java.util.Queue" "java.util.SequencedCollection"))
(jch-register-supers! "java.util.LinkedList" '("java.util.List" "java.util.Deque"))
(jch-register-supers! "java.util.ArrayDeque" '("java.util.Deque"))
(jch-register-supers! "java.util.HashMap" '("java.util.Map"))
;; Properties is a Hashtable, which is a Map — System/getProperties answers
;; (instance? java.util.Map …) as well as (instance? java.util.Properties …).
(jch-register-supers! "java.util.Hashtable" '("java.util.Map"))
(jch-register-supers! "java.util.Properties" '("java.util.Hashtable"))
(jch-register-supers! "java.util.HashSet" '("java.util.Set"))

;; ---- the rows typed.clojure's annotation corpus names ------------------------
;; typed.ann.clojure.base's override-classes resolves every class it annotates
;; through ns-resolve and throws "Could not resolve class" on a miss; its checker
;; then validates each :replace key against the class's DIRECT bases. Direct
;; supers here are the JVM's (probed), for both reasons.
;; java.lang.ref: the abstract Reference over the two reference classes the shim
;; already backs (host-static-classes.ss), and the queue they enqueue on.
(jch-register-supers! "java.lang.ref.Reference" '())
(jch-register-supers! "java.lang.ref.SoftReference" '("java.lang.ref.Reference"))
(jch-register-supers! "java.lang.ref.WeakReference" '("java.lang.ref.Reference"))
(jch-register-supers! "java.lang.ref.ReferenceQueue" '())
;; String.CASE_INSENSITIVE_ORDER's own class: String's private nested comparator
(jch-register-supers! "java.lang.String$CaseInsensitiveComparator"
                      '("java.util.Comparator" "java.io.Serializable"))
;; a defmulti value: MultiFn extends AFn, so it is an IFn and not a Fn
(jch-register-supers! "clojure.lang.MultiFn" '("clojure.lang.AFn"))
;; the transient lattice: the interfaces, the two abstract bases, and the four
;; concrete (package-private) classes the transient class arm reports. They used
;; to fall to the fn rule below — a $ in an unregistered name — so
;; (bases (class (transient #{}))) answered clojure.lang.AFunction.
(jch-register-supers! "clojure.lang.ITransientCollection" '())
(jch-register-supers! "clojure.lang.ITransientAssociative" '("clojure.lang.ITransientCollection" "clojure.lang.ILookup"))
(jch-register-supers! "clojure.lang.ITransientAssociative2" '("clojure.lang.ITransientAssociative"))
(jch-register-supers! "clojure.lang.ITransientMap" '("clojure.lang.ITransientAssociative" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.ITransientVector" '("clojure.lang.ITransientAssociative" "clojure.lang.Indexed"))
(jch-register-supers! "clojure.lang.ITransientSet" '("clojure.lang.ITransientCollection" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.ATransientMap"
                      '("clojure.lang.AFn" "clojure.lang.ITransientMap" "clojure.lang.ITransientAssociative2"))
(jch-register-supers! "clojure.lang.ATransientSet" '("clojure.lang.AFn" "clojure.lang.ITransientSet"))
(jch-register-supers! "clojure.lang.PersistentHashMap$TransientHashMap" '("clojure.lang.ATransientMap"))
(jch-register-supers! "clojure.lang.PersistentArrayMap$TransientArrayMap" '("clojure.lang.ATransientMap"))
(jch-register-supers! "clojure.lang.PersistentHashSet$TransientHashSet" '("clojure.lang.ATransientSet"))
(jch-register-supers! "clojure.lang.PersistentVector$TransientVector"
                      '("clojure.lang.AFn" "clojure.lang.ITransientVector"
                        "clojure.lang.ITransientAssociative2" "clojure.lang.Counted"))
;; --- the java.lang auto-imports ---------------------------------------------
;; clojure.core maps 96 class names into every namespace, so on the JVM every one
;; of them resolves, always. 38 had no row here, which meant no class token, which
;; meant (resolve 'ExceptionInInitializerError) answered nil where the JVM answers
;; the class (jolt-9my7). A row is what backs the name: it mints the clojure.core
;; token (class-token-alist, host-class.ss) so the bare symbol evaluates to the
;; class, which is the half resolve's answer promises — the instance? macro reads
;; a class from resolve as "this symbol evaluates to that class" and emits it
;; unquoted.
;;
;; A row is the class's NAME and ancestry, not an implementation: (StringBuffer.
;; "a") still has no constructor here, exactly as before. That is safe in a way it
;; would not be for an arbitrary class — resolve is how tooling feature-DETECTS a
;; class, and these 96 are present on every JVM, so no program can be using one as
;; a capability test.
;;
;; Direct supers are the JVM's wherever jolt models the parent. Where it does not
;; — StringBuffer's package-private AbstractStringBuilder, Package's NamedPackage,
;; RuntimePermission's java.security.BasicPermission — the row roots at Object and
;; keeps the interfaces jolt does model, so .getSuperclass is the one thing that
;; differs. Process's java.io.Closeable is left off deliberately: it is JDK-version
;; dependent, and claiming it would let with-open take a Process.
(jch-register-supers! "java.lang.ArrayStoreException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.EnumConstantNotPresentException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.IllegalMonitorStateException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.NegativeArraySizeException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.SecurityException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.TypeNotPresentException" '("java.lang.RuntimeException"))
(jch-register-supers! "java.lang.IllegalThreadStateException" '("java.lang.IllegalArgumentException"))
(jch-register-supers! "java.lang.InstantiationException" '("java.lang.ReflectiveOperationException"))
(jch-register-supers! "java.lang.ClassFormatError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.ExceptionInInitializerError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.VerifyError" '("java.lang.LinkageError"))
(jch-register-supers! "java.lang.UnsupportedClassVersionError" '("java.lang.ClassFormatError"))
(jch-register-supers! "java.lang.InstantiationError" '("java.lang.IncompatibleClassChangeError"))
(jch-register-supers! "java.lang.NoSuchFieldError" '("java.lang.IncompatibleClassChangeError"))
(jch-register-supers! "java.lang.NoSuchMethodError" '("java.lang.IncompatibleClassChangeError"))
(jch-register-supers! "java.lang.UnknownError" '("java.lang.VirtualMachineError"))
(jch-register-supers! "java.lang.ClassLoader" '())
(jch-register-supers! "java.lang.Enum" '("java.lang.Comparable"))
(jch-register-supers! "java.lang.Package" '())
(jch-register-supers! "java.lang.Process" '())
(jch-register-supers! "java.lang.ProcessBuilder" '())
;; the redirect shim's class had a tag row but no graph row, so it was not a
;; known class: no token, no nested-static modifier bit
(jch-register-supers! "java.lang.ProcessBuilder$Redirect" '())
(jch-register-supers! "java.lang.Runtime" '())
(jch-register-supers! "java.lang.RuntimePermission" '())
(jch-register-supers! "java.lang.SecurityManager" '())
(jch-register-supers! "java.lang.StackTraceElement" '())
(jch-register-supers! "java.lang.StrictMath" '())
(jch-register-supers! "java.lang.StringBuffer" '("java.lang.CharSequence" "java.lang.Appendable" "java.lang.Comparable"))
(jch-register-supers! "java.lang.ThreadLocal" '())
(jch-register-supers! "java.lang.InheritableThreadLocal" '("java.lang.ThreadLocal"))
(jch-register-supers! "java.lang.ThreadGroup" '("java.lang.Thread$UncaughtExceptionHandler"))
(jch-register-supers! "java.lang.Thread$State" '("java.lang.Enum"))
(jch-register-supers! "java.lang.Void" '())
(jch-register-supers! "clojure.lang.Compiler" '())
;; interfaces, including the three annotations — an annotation type IS an
;; interface on the JVM, and extends java.lang.annotation.Annotation.
(jch-register-supers! "java.lang.Cloneable" '())
(jch-mark-interface! "java.lang.Cloneable")
(jch-register-supers! "java.lang.Thread$UncaughtExceptionHandler" '())
(jch-mark-interface! "java.lang.Thread$UncaughtExceptionHandler")
(jch-register-supers! "java.lang.annotation.Annotation" '())
(jch-mark-interface! "java.lang.annotation.Annotation")
(jch-register-supers! "java.lang.Deprecated" '("java.lang.annotation.Annotation"))
(jch-mark-interface! "java.lang.Deprecated")
(jch-register-supers! "java.lang.Override" '("java.lang.annotation.Annotation"))
(jch-mark-interface! "java.lang.Override")
(jch-register-supers! "java.lang.SuppressWarnings" '("java.lang.annotation.Annotation"))
(jch-mark-interface! "java.lang.SuppressWarnings")

;; base interfaces used as super targets — need keys for simple-name resolution
(jch-register-supers! "java.lang.Number" '())
(jch-register-supers! "java.lang.Iterable" '())
(jch-register-supers! "java.util.Map" '())
(jch-register-supers! "java.lang.CharSequence" '())
(jch-register-supers! "java.lang.Comparable" '())
(jch-register-supers! "java.lang.Runnable" '())
(jch-register-supers! "java.util.concurrent.Callable" '())
;; java.util.concurrent's executor/future interfaces. The shims for these
;; (concurrency.ss) had no rows at all, so an ExecutorService reported (class x)
;; => :object and answered FALSE to (instance? java.util.concurrent.Executor x) —
;; which is the seam core.async.flow tests a user-supplied :io-exec through, so a
;; real jolt executor was rejected as "not an Executor". The graph is consulted
;; only by instance?/class (host-static-classes.ss), so naming it costs a shim
;; value nothing to construct or call.
(jch-register-supers! "java.util.concurrent.Executor" '())
(jch-mark-interface! "java.util.concurrent.Executor")
(jch-register-supers! "java.util.concurrent.ExecutorService"
                      '("java.util.concurrent.Executor"))
(jch-mark-interface! "java.util.concurrent.ExecutorService")
(jch-register-supers! "java.util.concurrent.AbstractExecutorService"
                      '("java.util.concurrent.ExecutorService" "java.util.concurrent.Executor"))
(jch-register-supers! "java.util.concurrent.ThreadPoolExecutor"
                      '("java.util.concurrent.AbstractExecutorService"
                        "java.util.concurrent.ExecutorService" "java.util.concurrent.Executor"))
(jch-register-supers! "java.util.concurrent.Future" '())
(jch-mark-interface! "java.util.concurrent.Future")
(jch-register-supers! "java.util.concurrent.RunnableFuture"
                      '("java.util.concurrent.Future" "java.lang.Runnable"))
(jch-mark-interface! "java.util.concurrent.RunnableFuture")
;; FutureTask is the class Executors' pools hand back from submit, and the one
;; core.async.flow's futurize constructs directly.
(jch-register-supers! "java.util.concurrent.FutureTask"
                      '("java.util.concurrent.RunnableFuture"
                        "java.util.concurrent.Future" "java.lang.Runnable"))
;; java.time temporal interfaces — base abstractions the concrete time classes implement
(jch-register-supers! "java.time.temporal.TemporalAccessor" '())
(jch-mark-interface! "java.time.temporal.TemporalAccessor")
(jch-register-supers! "java.time.temporal.Temporal" '("java.time.temporal.TemporalAccessor"))
(jch-mark-interface! "java.time.temporal.Temporal")
(jch-register-supers! "java.time.temporal.TemporalAdjuster" '())
(jch-mark-interface! "java.time.temporal.TemporalAdjuster")
(jch-register-supers! "java.time.temporal.TemporalAmount" '())
(jch-mark-interface! "java.time.temporal.TemporalAmount")
;; java.time.chrono super-interfaces the concrete date/time classes implement
(jch-register-supers! "java.time.chrono.ChronoLocalDate" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable"))
(jch-mark-interface! "java.time.chrono.ChronoLocalDate")
(jch-register-supers! "java.time.chrono.ChronoLocalDateTime" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable"))
(jch-mark-interface! "java.time.chrono.ChronoLocalDateTime")
(jch-register-supers! "java.time.chrono.ChronoZonedDateTime" '("java.time.temporal.Temporal" "java.lang.Comparable"))
(jch-mark-interface! "java.time.chrono.ChronoZonedDateTime")
;; java.time concrete classes with their real JVM interfaces (all are Serializable)
(jch-register-supers! "java.time.Instant" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.LocalDate" '("java.time.chrono.ChronoLocalDate" "java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.LocalTime" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.LocalDateTime" '("java.time.chrono.ChronoLocalDateTime" "java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.ZonedDateTime" '("java.time.chrono.ChronoZonedDateTime" "java.time.temporal.Temporal" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.OffsetDateTime" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.OffsetTime" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.Duration" '("java.time.temporal.TemporalAmount" "java.lang.Comparable" "java.io.Serializable"))
(jch-register-supers! "java.time.Period" '("java.time.temporal.TemporalAmount" "java.io.Serializable"))
(jch-register-supers! "java.time.Year" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable"))
(jch-register-supers! "java.time.YearMonth" '("java.time.temporal.Temporal" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable"))
(jch-register-supers! "java.time.ZoneId" '())
(jch-register-supers! "java.time.ZoneOffset" '("java.time.ZoneId" "java.time.temporal.TemporalAccessor" "java.time.temporal.TemporalAdjuster" "java.lang.Comparable"))
(jch-register-supers! "java.time.zone.ZoneRules" '())
(jch-register-supers! "java.time.temporal.ChronoUnit" '())
(jch-register-supers! "java.time.temporal.ChronoField" '())
(jch-register-supers! "java.time.Month" '("java.time.temporal.TemporalAccessor" "java.time.temporal.TemporalAdjuster"))
(jch-register-supers! "java.time.DayOfWeek" '("java.time.temporal.TemporalAccessor" "java.time.temporal.TemporalAdjuster"))
(jch-register-supers! "java.time.Clock" '())
(jch-register-supers! "java.time.format.DateTimeFormatter" '())
;; text / util classes with host shims
(jch-register-supers! "java.text.SimpleDateFormat" '())
(jch-register-supers! "java.util.GregorianCalendar" '())
(jch-register-supers! "java.util.Locale" '())
(jch-register-supers! "java.util.TimeZone" '())
;; #inst / (Date.) classes. java.sql.Date and java.sql.Timestamp are Date
;; subclasses (jolt's one #inst value reports Date only — see inst-time.ss, which
;; answers instance? Timestamp #f — so these rows exist for the sql-date shim and
;; for isa?/supers of the class tokens, not for the #inst value's dispatch tags).
(jch-register-supers! "java.util.Date" '("java.lang.Comparable"))
(jch-register-supers! "java.sql.Date" '("java.util.Date"))
(jch-register-supers! "java.sql.Timestamp" '("java.util.Date"))
(jch-register-supers! "java.nio.ByteBuffer" '("java.lang.Comparable"))
;; java.nio.file (nio-file.ss). The JVM's concrete classes here are private
;; implementation details (sun.nio.fs.UnixPath, sun.nio.fs.MacOSXFileSystem), so
;; the shims report the public interface a caller can actually name, the way the
;; java.io shims report java.io.Reader rather than a subclass.
(jch-register-supers! "java.nio.file.Path" '("java.lang.Comparable" "java.lang.Iterable" "java.nio.file.Watchable"))
(jch-register-supers! "java.nio.file.Watchable" '())
(jch-register-supers! "java.nio.file.FileSystem" '())
(jch-register-supers! "java.nio.file.PathMatcher" '())

;; ---- jhost value tag -> FQN (single value-discriminator registry) --------------
;; A value-layer shim value (make-jhost tag …) reports its JVM class by this map;
;; value-host-tags (records.ss) and (class x)/instance? (host-static-classes.ss)
;; both derive from it, so a shim value's protocol-dispatch tags, class name, and
;; instance? answers stay in agreement and inherit the graph's supers (an Instant
;; is a Temporal, a StringWriter is a Writer). Add a row here, not in three places.
(define jhost-tag->fqn (make-hashtable string-hash string=?))
(for-each (lambda (p) (hashtable-set! jhost-tag->fqn (car p) (cdr p)))
  '(("user-thread" . "java.lang.Thread")
    ("abq" . "java.util.concurrent.ArrayBlockingQueue")
    ("future-task" . "java.util.concurrent.FutureTask")
    ;; Executors' pools are ThreadPoolExecutors on the JVM too, and what their
    ;; submit returns is a FutureTask — the same class the "future-task" tag
    ;; models, which is why two tags share one FQN here (as the writer tags do).
    ("executor-service" . "java.util.concurrent.ThreadPoolExecutor")
    ("j-future" . "java.util.concurrent.FutureTask")
    ("instant" . "java.time.Instant")
    ("local-date" . "java.time.LocalDate")
    ("local-time" . "java.time.LocalTime")
    ("local-date-time" . "java.time.LocalDateTime")
    ("zoned-dt" . "java.time.ZonedDateTime")
    ("zoned-date-time" . "java.time.ZonedDateTime")
    ("offset-date-time" . "java.time.OffsetDateTime")
    ("offset-time" . "java.time.OffsetTime")
    ("duration" . "java.time.Duration")
    ("period" . "java.time.Period")
    ("year" . "java.time.Year")
    ("year-month" . "java.time.YearMonth")
    ("zone-id" . "java.time.ZoneId")
    ("zone-offset" . "java.time.ZoneOffset")
    ("zone-rules" . "java.time.zone.ZoneRules")
    ("chrono-unit" . "java.time.temporal.ChronoUnit")
    ("chrono-field" . "java.time.temporal.ChronoField")
    ("month-enum" . "java.time.Month")
    ("dow-enum" . "java.time.DayOfWeek")
    ("clock" . "java.time.Clock")
    ("dt-formatter" . "java.time.format.DateTimeFormatter")
    ("sdf" . "java.text.SimpleDateFormat")
    ("calendar" . "java.util.GregorianCalendar")
    ("locale" . "java.util.Locale")
    ("timezone" . "java.util.TimeZone")
    ("sql-date" . "java.sql.Date")
    ("uri" . "java.net.URI")
    ;; java.nio.file shims (nio-file.ss)
    ("nio-path" . "java.nio.file.Path")
    ("nio-filesystem" . "java.nio.file.FileSystem")
    ("nio-path-matcher" . "java.nio.file.PathMatcher")
    ("byte-buffer" . "java.nio.ByteBuffer")
    ("arraylist" . "java.util.ArrayList")
    ("linkedlist" . "java.util.LinkedList")
    ("arraydeque" . "java.util.ArrayDeque")
    ("hashmap" . "java.util.HashMap")
    ("properties" . "java.util.Properties")
    ("hashset" . "java.util.HashSet")
    ;; io writer/reader shims: *out* is a PrintWriter like the JVM REPL's
    ("port-writer" . "java.io.PrintWriter")
    ("print-writer" . "java.io.PrintWriter")
    ;; …and System/out is a PrintStream, which is a different class from a
    ;; different branch of the taxonomy. The shim (io-streams.ss) had no row here,
    ;; so every PrintStream reported (class x) => :object and answered false to
    ;; (instance? java.io.OutputStream x).
    ("print-stream" . "java.io.PrintStream")
    ("file-writer" . "java.io.FileWriter")
    ("writer" . "java.io.StringWriter")
    ("string-reader" . "java.io.StringReader")
    ("pushback-reader" . "java.io.PushbackReader")
    ;; the line-numbering subclass carries its own tag so a value reports the
    ;; class it really is — tools.reader does (extend LineNumberingPushbackReader
    ;; IndexingReader …), which only fires when get-line-number's dispatch sees
    ;; that class and not the plain PushbackReader one.
    ("line-numbering-pushback-reader" . "clojure.lang.LineNumberingPushbackReader")
    ("char-writer" . "java.io.OutputStreamWriter")
    ("char-reader" . "java.io.InputStreamReader")
    ("time-unit" . "java.util.concurrent.TimeUnit")
    ;; subprocess shims (process.ss), backing vendored babashka.process
    ("process-builder" . "java.lang.ProcessBuilder")
    ("process" . "java.lang.Process")
    ("process-redirect" . "java.lang.ProcessBuilder$Redirect")
    ("process-handle" . "java.lang.ProcessHandle")
    ;; java.lang.reflect member objects (java/natives-array.ss,
    ;; java/host-static-classes.ss). Two field tags share one FQN: a deftype's
    ;; declared field and a registered host static are both a java.lang.reflect.Field
    ;; to a caller, and differ only in where the value comes from.
    ("reflect-method" . "java.lang.reflect.Method")
    ("reflect-field" . "java.lang.reflect.Field")
    ("static-field" . "java.lang.reflect.Field")
    ("class-ctor" . "java.lang.reflect.Constructor")
    ;; java.lang.ref (host-static-classes.ss): the two reference classes carry
    ;; their own tags — one shared tag reported a WeakReference as a
    ;; SoftReference — and the queue both enqueue on. Before these rows every
    ;; one reported (class x) => :object.
    ("soft-ref" . "java.lang.ref.SoftReference")
    ("weak-ref" . "java.lang.ref.WeakReference")
    ("ref-queue" . "java.lang.ref.ReferenceQueue")
    ;; String.CASE_INSENSITIVE_ORDER (host-static-methods.ss)
    ("string-ci-comparator" . "java.lang.String$CaseInsensitiveComparator")))
;; FQN for a jhost tag, or #f if the tag names no modeled class (e.g. "class",
;; "in-stream", "jolt-comparator") — callers fall through on #f.
(define (jhost-fqn tag) (hashtable-ref jhost-tag->fqn tag #f))

;; The reverse: every tag that models class NAME, or '() for a class no shim
;; backs. More than one tag can map to one FQN (the two writer tags, the two
;; field tags), so this answers a LIST — Class.getDeclaredMethods walks it to
;; report the methods jolt has registered for the class, and dropping the second
;; tag would silently halve the answer for exactly the classes that have two.
(define (jhost-tags-for-fqn name)
  (let-values (((ks vs) (hashtable-entries jhost-tag->fqn)))
    (let loop ((i 0) (acc '()))
      (cond ((fx=? i (vector-length ks)) (reverse acc))
            ((string=? (vector-ref vs i) name) (loop (fx+ i 1) (cons (vector-ref ks i) acc)))
            (else (loop (fx+ i 1) acc))))))

;; Is this tag one of the pushback readers? Two tags model the pair
;; (java.io.PushbackReader and its line-numbering subclass), and everything that
;; asks "is this a pushback reader" must ask HERE rather than compare the tag
;; literally — a second tag that only some sites recognize is how a reader
;; silently stops being closeable, slurpable or re-wrappable on one path while
;; still working on another.
(define (pushback-reader-tag? t)
  (or (string=? t "pushback-reader")
      (string=? t "line-numbering-pushback-reader")))

;; The jhost text sinks: values that take text through their own .write method —
;; a StringWriter, a FileWriter, the process port-writers, a PrintWriter, a
;; PrintStream. spit, io/copy and with-open's close all ask this, and for the same
;; reason as above: the set grew a fifth tag when System/out became a PrintStream,
;; and three hand-copied literal lists is how one of them silently stops accepting
;; a value the other two do ((io/copy in System/out) -> "unsupported output type").
(define (text-sink-tag? t)
  (or (string=? t "writer") (string=? t "file-writer")
      (string=? t "port-writer") (string=? t "print-writer")
      (string=? t "print-stream")))
;; the protocol-dispatch / instance? tag list for a jhost value's tag, or #f.
(define (jhost-value-tags tag)
  (let ((fqn (hashtable-ref jhost-tag->fqn tag #f)))
    (and fqn (jch-tags fqn))))

;; Public seam: libraries extend the modeled hierarchy.
(def-var! "jolt.host" "register-class-supers!"
  (lambda (name supers) (jch-register-supers! name (seq->list supers)) jolt-nil))

;; the ONE superclass edge for Class.getSuperclass: the first direct super that
;; is not an interface, else java.lang.Object for a known concrete class. #f for
;; Object itself, for interfaces (the JVM's null), and for names the graph does
;; not model — the caller decides what unknown means (the reflection surface
;; answers nil there, a recorded divergence for statics-only shims like Math).
(define (jch-superclass name)
  (cond
    ((string=? name "java.lang.Object") #f)
    ((jch-interface? name) #f)
    ((not (jch-known? name)) #f)
    (else
     ;; prefer a concrete super over Object wherever it sits in the row —
     ;; a row may list Object ahead of an abstract base.
     (let loop ((ss (jch-direct-supers name)))
       (cond ((null? ss) "java.lang.Object")
             ((or (jch-interface? (car ss))
                  (string=? (car ss) "java.lang.Object"))
              (loop (cdr ss)))
             (else (car ss)))))))

;; transitive ancestry rooted at Object for a concrete class; an interface's chain
;; has no Object (its getSuperclass is null). '() for Object itself.
(define (jch-ancestors-rooted name)
  (if (or (string=? name "java.lang.Object") (jch-interface? name))
      (jch-closure name)
      (let ((as (jch-closure name)))
        (cond ((member "java.lang.Object" as) as)
              ((null? as) (if (jch-known? name) '("java.lang.Object") '()))
              (else (append as '("java.lang.Object")))))))

;; bases — the direct supers of a class from the jch graph. c may be a class-name
;; string, a jclass object (class token), or a JVM-typed value (number, string, etc.).
;; nil for an unknown class or a nil arg.
(define (jolt-bases c)
  (cond
    ((jolt-nil? c) jolt-nil)
    ((string? c)
     (let ((supers (jch-direct-supers c)))
       (if (null? supers) jolt-nil (list->cseq supers))))
    (else
     ;; For a jclass object (e.g. java.lang.Long after class-token eval), extract
     ;; the represented class name via jclass-name (defined in host-static-classes.ss,
     ;; loaded after us — resolved at call time). For other values (number, string,
     ;; etc.), jolt-class-name gives their JVM class name (java.lang.Long, etc.).
     ;; A deftype/defrecord TYPE TOKEN is its class, not a function: without this
     ;; (bases Rec) walked the ctor PROCEDURE's ancestry and answered
     ;; clojure.lang.AFunction, while (bases (class inst)) gave the record's real
     ;; interfaces. supers/ancestors already routed through the same question via
     ;; class-key; this was the one spelling that did not.
     (let ((name (cond ((and (jhost? c) (string=? (jhost-tag c) "class"))
                        (vector-ref (jhost-state c) 0))
                       ((and (procedure? c) (deftype-ctor-tag c)) => values)
                       (else (jolt-class-name c)))))
       (let ((supers (jch-direct-supers name)))
         (if (null? supers) jolt-nil (list->cseq supers)))))))
;; The Chez host re-defs `bases` over this with Class objects (host-static-classes.ss,
;; which owns the class-token interner and loads after us); this name-string form
;; is what the Gambit boot, which excludes that file, keeps.
(def-var! "clojure.core" "bases" jolt-bases)
