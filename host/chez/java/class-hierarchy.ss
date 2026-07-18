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
;; closure cache, invalidated whenever the graph is extended.
(define jch-cache-mutex (make-mutex))
(define jch-closure-cache (make-hashtable string-hash string=?))
(define jch-tags-cache (make-hashtable string-hash string=?))

;; Merge direct supers for a class (union with any already registered). Public so
;; libraries can graft their own classes onto the modeled hierarchy.
(define (jch-register-supers! name supers)
  (let ((cur (hashtable-ref jvm-class-parents name '())))
    (hashtable-set! jvm-class-parents name
                    (let add ((ss supers) (acc cur))
                      (cond ((null? ss) acc)
                            ((member (car ss) acc) (add (cdr ss) acc))
                            (else (add (cdr ss) (append acc (list (car ss)))))))))
  (with-mutex jch-cache-mutex
    (hashtable-clear! jch-closure-cache)
    (hashtable-clear! jch-tags-cache)))

;; A munged fn class name "ns$name" (jolt-class for a def'd fn) isn't in the
;; table; like the JVM (a fn extends clojure.lang.AFunction) its super is
;; AFunction, whose registered supers give AFn / IFn / Fn / Runnable / Callable
;; transitively.
(define (str-has-dollar? s)
  (let loop ((i 0)) (and (< i (string-length s)) (or (char=? (string-ref s i) #\$) (loop (+ i 1))))))

(define (jch-direct-supers name)
  (let ((direct (hashtable-ref jvm-class-parents name '())))
    (if (pair? direct) direct
        (if (str-has-dollar? name) '("clojure.lang.AFunction")
            '()))))

;; Replace a class's direct supers outright (defrecord re-declares the row its
;; deftype half registered). Same cache invalidation as a register.
(define (jch-set-supers! name supers)
  (hashtable-set! jvm-class-parents name supers)
  (with-mutex jch-cache-mutex
    (hashtable-clear! jch-closure-cache)
    (hashtable-clear! jch-tags-cache))
  (set! jch-known-cache #f)
  (set! jch-simple->fqn-cache #f))

;; transitive supers of NAME (canonical), excluding NAME and Object; Object is the
;; universal root supplied by callers. Breadth-first, deduped, stable order.
(define (jch-closure name)
  (or (hashtable-ref jch-closure-cache name #f)
      (let ((result
             (let loop ((pending (jch-direct-supers name)) (seen '()))
               (cond ((null? pending) (reverse seen))
                     ((member (car pending) seen) (loop (cdr pending) seen))
                     (else (loop (append (jch-direct-supers (car pending)) (cdr pending))
                                 (cons (car pending) seen)))))))
        (hashtable-set! jch-closure-cache name result)
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

;; The protocol-dispatch / instance? tag list for a value of class NAME: the class
;; and its whole ancestry, each in BOTH canonical and simple spelling (extend-protocol
;; and instance? accept either "Associative" or "clojure.lang.Associative"), plus
;; "Object". Memoized — this is on the hot protocol-dispatch path.
(define (jch-tags name)
  (or (hashtable-ref jch-tags-cache name #f)
      (let* ((chain (cons name (jch-closure name)))
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
        (hashtable-set! jch-tags-cache name result)
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
(define jch-known-cache #f)
(define (jch-known? wanted)
  (when (not jch-known-cache)
    (set! jch-known-cache (make-hashtable string-hash string=?))
    (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
      (vector-for-each
       (lambda (k supers)
         (hashtable-set! jch-known-cache k #t)
         (hashtable-set! jch-known-cache (jch-last-segment k) #t)
         (for-each (lambda (s)
                     (hashtable-set! jch-known-cache s #t)
                     (hashtable-set! jch-known-cache (jch-last-segment s) #t))
                   supers))
       keys vals)))
  (or (hashtable-ref jch-known-cache wanted #f)
      (hashtable-ref jch-known-cache (jch-last-segment wanted) #f)))

;; simple last-segment -> canonical FQN for a modeled class (first registered
;; wins). Lets a simple exception name (from chez-condition-exc-class) resolve to
;; its graph key so the exception hierarchy answers through the one graph.
(define jch-simple->fqn-cache #f)
(define (jch-fqn-of-simple name)
  (when (not jch-simple->fqn-cache)
    (set! jch-simple->fqn-cache (make-hashtable string-hash string=?))
    (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
      (vector-for-each
       (lambda (k supers)
         (for-each (lambda (n)
                     (let ((seg (jch-last-segment n)))
                       (when (not (hashtable-ref jch-simple->fqn-cache seg #f))
                         (hashtable-set! jch-simple->fqn-cache seg n))))
                   (cons k supers)))
       keys vals)))
  (or (hashtable-ref jch-simple->fqn-cache name #f) name))

;; A register also invalidates the derived caches.
(define jch-register-supers!-inner jch-register-supers!)
(set! jch-register-supers!
  (lambda (name supers)
    (set! jch-known-cache #f)
    (set! jch-simple->fqn-cache #f)
    (jch-register-supers!-inner name supers)))

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
(define (jch-mark-interface! name) (hashtable-set! jch-interface-set name #t))
(define (jch-interface? name) (hashtable-ref jch-interface-set name #f))
(for-each jch-mark-interface!
          '("clojure.lang.Seqable" "clojure.lang.Sequential" "clojure.lang.Sorted"
            "clojure.lang.Reversible" "clojure.lang.Indexed" "clojure.lang.Counted"
            "clojure.lang.Named" "clojure.lang.Fn" "clojure.lang.IFn"
            "clojure.lang.IPersistentCollection" "clojure.lang.ISeq"
            "clojure.lang.Associative" "clojure.lang.ILookup"
            "clojure.lang.IPersistentStack" "clojure.lang.IPersistentVector"
            "clojure.lang.IPersistentMap" "clojure.lang.IPersistentSet"
            "clojure.lang.IPersistentList" "clojure.lang.IObj" "clojure.lang.IMeta"
            "clojure.lang.IDeref" "clojure.lang.IRecord" "clojure.lang.IType"
            "clojure.lang.IHashEq" "clojure.lang.IEditableCollection"
            "clojure.lang.IExceptionInfo" "clojure.lang.IReduceInit"
            "java.util.List" "java.util.Set" "java.util.Collection" "java.util.Map"
            "java.util.Iterator" "java.lang.Iterable" "java.lang.CharSequence"
            "java.lang.Comparable" "java.lang.Runnable"
            "java.util.concurrent.Callable" "java.io.Serializable"))

;; ---- seed the built-in graph: direct supers only, faithful to the JVM ---------
;; core clojure.lang interfaces
(jch-register-supers! "clojure.lang.IPersistentCollection" '("clojure.lang.Seqable"))
(jch-register-supers! "clojure.lang.ISeq" '("clojure.lang.IPersistentCollection"))
(jch-register-supers! "clojure.lang.Associative" '("clojure.lang.IPersistentCollection" "clojure.lang.ILookup"))
(jch-register-supers! "clojure.lang.IPersistentStack" '("clojure.lang.IPersistentCollection"))
(jch-register-supers! "clojure.lang.IPersistentVector" '("clojure.lang.Associative" "clojure.lang.Sequential"
                                                         "clojure.lang.IPersistentStack" "clojure.lang.Reversible"
                                                         "clojure.lang.Indexed"))
(jch-register-supers! "clojure.lang.IPersistentMap" '("java.lang.Iterable" "clojure.lang.Associative" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.IPersistentSet" '("clojure.lang.IPersistentCollection" "clojure.lang.Counted"))
(jch-register-supers! "clojure.lang.IPersistentList" '("clojure.lang.Sequential" "clojure.lang.IPersistentStack"))
(jch-register-supers! "clojure.lang.IObj" '("clojure.lang.IMeta"))
(jch-register-supers! "clojure.lang.IFn" '("clojure.lang.Fn" "java.lang.Runnable" "java.util.concurrent.Callable"))
;; Fn is a marker interface (no supers).
(jch-register-supers! "clojure.lang.AFn" '("clojure.lang.IFn"))
(jch-register-supers! "clojure.lang.AFunction" '("clojure.lang.AFn" "clojure.lang.Fn"))
;; java.util collection interfaces
(jch-register-supers! "java.util.List" '("java.util.Collection"))
(jch-register-supers! "java.util.Set" '("java.util.Collection"))
(jch-register-supers! "java.util.Collection" '("java.lang.Iterable"))
;; concrete collection classes
(jch-register-supers! "clojure.lang.APersistentVector" '("clojure.lang.IPersistentVector" "java.util.List"))
(jch-register-supers! "clojure.lang.PersistentVector" '("clojure.lang.APersistentVector" "clojure.lang.IObj"
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
(jch-register-supers! "clojure.lang.PersistentQueue" '("clojure.lang.IPersistentList" "clojure.lang.IPersistentCollection" "java.util.Collection"))
;; scalars / named / callable
(jch-register-supers! "clojure.lang.Keyword" '("clojure.lang.IFn" "clojure.lang.Named" "java.lang.Comparable"))
(jch-register-supers! "clojure.lang.Symbol" '("clojure.lang.IObj" "clojure.lang.IFn" "clojure.lang.Named" "java.lang.Comparable"))
(jch-register-supers! "clojure.lang.Var" '("clojure.lang.IDeref" "clojure.lang.IFn"))
(jch-register-supers! "clojure.lang.Atom" '("clojure.lang.IDeref"))
(jch-register-supers! "clojure.lang.Ref" '("clojure.lang.IRef"))
(jch-register-supers! "clojure.lang.IRef" '("clojure.lang.IDeref"))
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
(jch-register-supers! "java.lang.Error" '("java.lang.Throwable"))
(jch-register-supers! "java.lang.AssertionError" '("java.lang.Error"))
(jch-register-supers! "java.lang.ArrayIndexOutOfBoundsException" '("java.lang.IndexOutOfBoundsException"))
(jch-register-supers! "java.lang.StringIndexOutOfBoundsException" '("java.lang.IndexOutOfBoundsException"))
(jch-register-supers! "java.lang.ReflectiveOperationException" '("java.lang.Exception"))
(jch-register-supers! "java.lang.ClassNotFoundException" '("java.lang.ReflectiveOperationException"))
(jch-register-supers! "java.lang.NoSuchMethodException" '("java.lang.ReflectiveOperationException"))
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
(jch-register-supers! "java.io.IOError" '("java.lang.Error"))
;; leaf/root classes with only Object as super
(jch-register-supers! "java.lang.Object" '())
(jch-register-supers! "java.lang.Class" '())
(jch-register-supers! "java.lang.Throwable" '())
(jch-register-supers! "java.lang.Byte" '("java.lang.Number"))
(jch-register-supers! "java.lang.Short" '("java.lang.Number"))
(jch-register-supers! "java.io.InputStream" '())
(jch-register-supers! "java.io.OutputStream" '())
(jch-register-supers! "java.io.Reader" '())
(jch-register-supers! "java.io.Writer" '())
(jch-register-supers! "java.io.File" '())
(jch-register-supers! "java.io.StringReader" '("java.io.Reader"))
(jch-register-supers! "java.io.PushbackReader" '("java.io.Reader"))
(jch-register-supers! "clojure.lang.LineNumberingPushbackReader" '("java.io.PushbackReader"))
(jch-register-supers! "java.io.PrintWriter" '("java.io.Writer"))
(jch-register-supers! "java.io.OutputStreamWriter" '("java.io.Writer"))
(jch-register-supers! "java.io.FileWriter" '("java.io.OutputStreamWriter"))
(jch-register-supers! "java.io.InputStreamReader" '("java.io.Reader"))
(jch-register-supers! "java.io.StringWriter" '("java.io.Writer"))
(jch-register-supers! "java.lang.StringBuilder" '())
(jch-register-supers! "java.util.StringTokenizer" '())
(jch-register-supers! "java.nio.charset.Charset" '())
(jch-register-supers! "java.util.Base64" '())
(jch-register-supers! "clojure.lang.MapEntry" '())
(jch-register-supers! "clojure.lang.Namespace" '())
(jch-register-supers! "java.util.regex.Pattern" '())
(jch-register-supers! "java.net.URI" '())
(jch-register-supers! "java.util.ArrayList" '("java.util.List"))
(jch-register-supers! "java.util.Queue" '("java.util.Collection"))
(jch-register-supers! "java.util.Deque" '("java.util.Queue"))
(jch-register-supers! "java.util.LinkedList" '("java.util.List" "java.util.Deque"))
(jch-register-supers! "java.util.ArrayDeque" '("java.util.Deque"))
(jch-register-supers! "java.util.HashMap" '("java.util.Map"))
(jch-register-supers! "java.util.HashSet" '("java.util.Set"))
;; base interfaces used as super targets — need keys for simple-name resolution
(jch-register-supers! "java.lang.Number" '())
(jch-register-supers! "java.lang.Iterable" '())
(jch-register-supers! "java.util.Map" '())
(jch-register-supers! "java.lang.CharSequence" '())
(jch-register-supers! "java.lang.Comparable" '())
(jch-register-supers! "java.lang.Runnable" '())
(jch-register-supers! "java.util.concurrent.Callable" '())
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

;; Public seam: libraries extend the modeled hierarchy.
(def-var! "jolt.host" "register-class-supers!"
  (lambda (name supers) (jch-register-supers! name (seq->list supers)) jolt-nil))

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
     (let ((name (if (and (jhost? c) (string=? (jhost-tag c) "class"))
                    (vector-ref (jhost-state c) 0)
                    (jolt-class-name c))))
       (let ((supers (jch-direct-supers name)))
         (if (null? supers) jolt-nil (list->cseq supers)))))))
(def-var! "clojure.core" "bases" jolt-bases)
