;; host class tokens — a bare class name (String, Keyword, File...)
;; evaluates to its JVM canonical-name STRING, the same value (class instance)
;; returns, so (= String (class "x")) holds and a (defmethod m String ...) keys
;; against a (class …) dispatch (ring.util.request does this).
;; The analyzer resolves these names to clojure.core vars, so the back end emits
;; (var-deref "clojure.core" "String") — def-var!'ing the canonical strings here is
;; all that's needed at runtime.
;;
;; Loaded after natives-meta.ss (jolt-type) + the printer (jolt-str-render-one).

;; (class x) — Clojure's class of a value. Scalars map to their JVM class name,
;; matching core-class. Collections/seqs have no JVM class on this host;
;; (str (type x)) is the clean host taxonomy and
;; is never compared against a class token in the corpus. Records yield their
;; ns-qualified class name (= (str (type x))). Total — never crashes.
;; A host shim (bigdec, queue, host-table) registers its type's class name via
;; register-class-arm! instead of set!-wrapping jolt-class (cf. register-hash-arm!).
;; The entry is stable, so the var cell bound below stays current as arms register.
(define jolt-class-arms '())
(define (register-class-arm! pred handler)
  (set! jolt-class-arms (cons (cons pred handler) jolt-class-arms)))

;; ---- seq flavor -> JVM class --------------------------------------------------
;; The one place the sk-* tags a cseq cell carries (seq.ss) become clojure.lang.*
;; names. Everything that asks "what class is this seq" — (class …) below, protocol
;; dispatch's value-host-tags, instance?, counted? — reads THIS vector, so the
;; answers cannot drift apart the way two hand-kept conds did: (class (seq [1 2]))
;; said PersistentList while (list? (seq [1 2])) said false.
;;
;; Indexed by kind, sized from sk-count, so adding a flavor in seq.ss is a
;; compile-time-obvious one-line addition here and nothing else.
(define seq-kind-class-v
  (let ((v (make-vector sk-count "clojure.lang.Cons")))
    (vector-set! v sk-cons          "clojure.lang.Cons")
    (vector-set! v sk-list          "clojure.lang.PersistentList")
    (vector-set! v sk-chunked-seq   "clojure.lang.PersistentVector$ChunkedSeq")
    (vector-set! v sk-chunked-cons  "clojure.lang.ChunkedCons")
    (vector-set! v sk-string-seq    "clojure.lang.StringSeq")
    (vector-set! v sk-rseq          "clojure.lang.APersistentVector$RSeq")
    (vector-set! v sk-arraymap-seq  "clojure.lang.PersistentArrayMap$Seq")
    (vector-set! v sk-hashmap-seq   "clojure.lang.PersistentHashMap$NodeSeq")
    (vector-set! v sk-treemap-seq   "clojure.lang.PersistentTreeMap$Seq")
    (vector-set! v sk-key-seq       "clojure.lang.APersistentMap$KeySeq")
    (vector-set! v sk-val-seq       "clojure.lang.APersistentMap$ValSeq")
    (vector-set! v sk-long-range    "clojure.lang.LongRange")
    (vector-set! v sk-range         "clojure.lang.Range")
    (vector-set! v sk-iterate       "clojure.lang.Iterate")
    (vector-set! v sk-repeat        "clojure.lang.Repeat")
    (vector-set! v sk-array-seq     "clojure.lang.ArraySeq")
    (vector-set! v sk-array-int     "clojure.lang.ArraySeq$ArraySeq_int")
    (vector-set! v sk-array-long    "clojure.lang.ArraySeq$ArraySeq_long")
    (vector-set! v sk-array-short   "clojure.lang.ArraySeq$ArraySeq_short")
    (vector-set! v sk-array-double  "clojure.lang.ArraySeq$ArraySeq_double")
    (vector-set! v sk-array-float   "clojure.lang.ArraySeq$ArraySeq_float")
    (vector-set! v sk-array-bool    "clojure.lang.ArraySeq$ArraySeq_boolean")
    (vector-set! v sk-array-byte    "clojure.lang.ArraySeq$ArraySeq_byte")
    (vector-set! v sk-array-char    "clojure.lang.ArraySeq$ArraySeq_char")
    v))
;; One vector-ref. This sits on the protocol-dispatch path for every seq argument,
;; which is why the flavor is a fixnum index and not a name to be looked up.
(define (cseq-class-name s) (vector-ref seq-kind-class-v (cseq-kind s)))

;; Graft clojure.lang.IChunkedSeq onto exactly the flavors that chunk. sk-chunked?
;; (seq.ss) is the single statement of which those are, because that tier is what
;; implements chunk-first/chunk-rest; deriving the interface from it here is what
;; keeps (chunked-seq? x) and (instance? IChunkedSeq x) from ever disagreeing.
(let loop ((k 0))
  (when (fx<? k sk-count)
    (when (sk-chunked? k)
      (jch-register-supers! (vector-ref seq-kind-class-v k) '("clojure.lang.IChunkedSeq")))
    (loop (fx+ k 1))))

;; Which flavors are Counted, derived FROM the graph rather than listed again — the
;; rows in class-hierarchy.ss are the only statement of it, so counted? cannot
;; disagree with instance? or ancestors. Memoized per kind behind the graph's
;; generation counter: the graph is extensible (a library can graft supers onto a
;; class at any time), and a cached answer computed against the old graph must not
;; outlive it. Steady state is two fixnum compares and a vector-ref.
(define seq-kind-counted-v #f)
(define seq-kind-counted-epoch -1)
(define (seq-kind-counted-vec)
  (if (and seq-kind-counted-v (fx=? seq-kind-counted-epoch jch-graph-epoch))
      seq-kind-counted-v
      (let ((v (make-vector sk-count #f))
            (epoch jch-graph-epoch))    ; read BEFORE the walk, like jch-closure
        (let loop ((k 0))
          (when (fx<? k sk-count)
            (vector-set! v k (and (jch-isa? (vector-ref seq-kind-class-v k)
                                            "clojure.lang.Counted")
                                  #t))
            (loop (fx+ k 1))))
        (set! seq-kind-counted-epoch epoch)
        (set! seq-kind-counted-v v)
        v)))
(define (cseq-counted? s) (vector-ref (seq-kind-counted-vec) (cseq-kind s)))
(define (jolt-class-base x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((boolean? x) "java.lang.Boolean")
    ;; per-type number classes, like the JVM: integer -> Long, flonum -> Double,
    ;; exact non-integer -> Ratio.
    ((and (number? x) (flonum? x)) "java.lang.Double")
    ;; split at the long range (jolt-bigint-print?'s boundary, shared with the
    ;; printer's N suffix), not the 61-bit fixnum range: Long/MAX_VALUE is a
    ;; Chez bignum but a JVM Long. Beyond long is the JVM's BigInt — the class
    ;; a value of that magnitude has there (big literals, *' promotion). #627.
    ((and (number? x) (exact? x) (integer? x))
     (if (jolt-bigint-print? x) "clojure.lang.BigInt" "java.lang.Long"))
    ((and (number? x) (exact? x) (rational? x)) "clojure.lang.Ratio")
    ((number? x) "java.lang.Number")
    ((string? x) "java.lang.String")
    ((keyword? x) "clojure.lang.Keyword")
    ((symbol-t? x) "clojure.lang.Symbol")
    ((jolt-atom? x) "clojure.lang.Atom")
    ((jolt-ref? x) "clojure.lang.Ref")
    ((jolt-reduced? x) "clojure.lang.Reduced")
    ((char? x) "java.lang.Character")
    ((regex-t? x) "java.util.regex.Pattern")
    ;; an anonymous / unregistered fn — like the JVM, where (class #(..)) is a
    ;; concrete ns$fn__N subclass. The $fn marker lets clojure.spec.alpha's fn-sym
    ;; recognize it as anonymous and return ::s/unknown. A named fn is registered
    ;; (proc-name-tbl) and handled by a class-arm with its real ns$name.
    ((procedure? x) "clojure.lang.AFunction$fn__0")
    ;; an exception value (ex-info record / host-constructed throwable) reports its JVM
    ;; class, so (= clojure.lang.ExceptionInfo (class e)) and clojure.test's
    ;; (thrown? Class …) match (records-interop.ss ex-info-map?/ex-info-class).
    ((ex-info-map? x) (ex-info-class x))
    ;; persistent collections + namespace report their JVM class names (not jolt's
    ;; internal :vector/:set/… type keyword), so class-based dispatch — e.g. a
    ;; defmulti on [(class a) (class b)] — sees a real clojure.lang.* class.
    ((jns? x) "clojure.lang.Namespace")
    ;; a map entry is a pvec with the entry kind; the JVM class is MapEntry
    ((jolt-map-entry? x) "clojure.lang.MapEntry")
    ;; a subvec view is a pvec with the subvec kind (issue #629)
    ((jolt-subvec-view? x) "clojure.lang.APersistentVector$SubVector")
    ((pvec? x) "clojure.lang.PersistentVector")
    ((pset? x) "clojure.lang.PersistentHashSet")
    ;; array mode (insertion-ordered, small literal maps) is PersistentArrayMap;
    ;; hash mode (hash-map, or grown past the array limit) is PersistentHashMap
    ((pmap? x) (if (pmap-array? x) "clojure.lang.PersistentArrayMap"
                   "clojure.lang.PersistentHashMap"))
    ((jolt-lazyseq? x) "clojure.lang.LazySeq")
    ((empty-list-t? x) "clojure.lang.PersistentList$EmptyList")
    ;; …whichever concrete seq class this cell's flavor says it is
    ((cseq? x) (cseq-class-name x))
    (else (jolt-str-render-one (jolt-type x)))))
;; the class NAME of x (string), or nil for nil. (class x) wraps it in a Class
;; value (make-class-obj, host-static-classes.ss) so it renders like a JVM Class
;; while staying = its name string.
;; No condition? class-arm: a raw Chez condition never reaches the class-name
;; chain, because every one a catch binds has become a typed throwable at the
;; boundary (jolt-unwrap-throw, java/host-faults.ss).
;; A fn def'd into a var reports a JVM-style class name "ns$munged-name" (the
;; forward CHAR_MAP), so clojure.spec.alpha's fn-sym (which splits on $ and
;; demunges) recovers the predicate's symbol. Anonymous / unregistered fns stay
;; clojure.lang.IFn (fn-sym yields :unknown, as on the JVM).
;;
;; The table below IS clojure.lang.Compiler/CHAR_MAP, character by character, and
;; it is the ONE forward munge table. It lives here rather than beside demunge
;; (compile-eval.ss) only because of load order: rt.ss loads this file and
;; compile-eval.ss comes after it, so this is the earlier of the two ends and the
;; later one derives from it — Compiler/CHAR_MAP, Compiler/munge and the demunge
;; token table are all built out of this alist, so no two of them can name
;; different escapes for one character. It used to hold 15 of the JVM's 24
;; entries, which is why (class clojure.core/+') reported "clojure.core$_PLUS_'":
;; a name no JVM emits, demunge cannot reverse, and Java would not accept as an
;; identifier.
(define class-munge-map
  '((#\- . "_") (#\: . "_COLON_") (#\+ . "_PLUS_") (#\> . "_GT_")
    (#\< . "_LT_") (#\= . "_EQ_") (#\~ . "_TILDE_") (#\! . "_BANG_")
    (#\@ . "_CIRCA_") (#\# . "_SHARP_") (#\' . "_SINGLEQUOTE_")
    (#\" . "_DOUBLEQUOTE_") (#\% . "_PERCENT_") (#\^ . "_CARET_")
    (#\& . "_AMPERSAND_") (#\* . "_STAR_") (#\| . "_BAR_") (#\{ . "_LBRACE_")
    (#\} . "_RBRACE_") (#\[ . "_LBRACK_") (#\] . "_RBRACK_") (#\/ . "_SLASH_")
    (#\\ . "_BSLASH_") (#\? . "_QMARK_")))
(define (class-munge-name s)
  (let ((out (open-output-string)))
    (string-for-each
     (lambda (c) (let ((t (assv c class-munge-map))) (if t (display (cdr t) out) (write-char c out))))
     s)
    (get-output-string out)))
(register-class-arm!
  (lambda (x) (and (procedure? x) (proc-name-of x)))
  (lambda (x) (let ((p (proc-name-of x)))
                ;; the ns segment munges too (a-b.core -> a_b.core), like
                ;; Compiler.munge; dots stay.
                (string-append (class-munge-name (car p)) "$" (class-munge-name (cdr p))))))

(define (jolt-class-name x)
  (let loop ((as jolt-class-arms))
    (cond ((null? as) (jolt-class-base x))
          (((caar as) x) ((cdar as) x))
          (else (loop (cdr as))))))
(define (jolt-class x)
  (let ((n (jolt-class-name x)))
    (if (jolt-nil? n) jolt-nil (make-class-obj n))))

(def-var! "clojure.core" "class" jolt-class)

;; The PUBLIC clojure.core/type — Clojure's (or (:type meta) (class x)). This is the
;; java host layer's job: the core taxonomy (natives-meta.ss jolt-type, kept under
;; __type-tag for print-method) is JVM-free, and the JVM class mapping lives HERE,
;; next to (class …). The inst/array/byte-buffer host files extend `class` (a
;; class-arm or jolt-type fallthrough) and re-point `type` at this same fn, so the
;; remap of every value — :jolt/inst -> java.util.Date etc. — happens in one place.
(define ty-meta-key (keyword #f "type"))
(define (jolt-type-pub x)
  (let* ((m (jolt-meta x))
         (override (if (jolt-nil? m) jolt-nil (jolt-get m ty-meta-key jolt-nil))))
    (if (not (jolt-nil? override)) override (jolt-class x))))
(def-var! "clojure.core" "type" jolt-type-pub)

;; bare class-name tokens -> canonical JVM class-name strings, derived from the
;; modeled class graph (jvm-class-parents) so this list stays current with any
;; additions to class-hierarchy.ss.
;;
;; Keyed by the name a namespace would MAP the class under — the part after the
;; last dot, $ and all (java.util.Map$Entry -> Map$Entry, java.lang.Thread$State
;; -> Thread$State), which is what the JVM imports. jch-last-segment goes on past
;; the $ because its job is the alternative spelling a protocol extension may use;
;; taking that as an import name minted a clojure.core/Entry and a
;; clojure.core/Seq the JVM has no mapping for, and left the two nested auto-
;; imports with no token at all.
(define class-token-alist
  (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
    (let ((result '()) (seen (make-hashtable string-hash string=?)))
      (vector-for-each
        (lambda (k _)
          (let ((s (jch-import-name k)))
            (when (not (hashtable-ref seen s #f))
              (hashtable-set! seen s #t)
              (set! result (cons (cons s k) result)))))
        keys vals)
      (reverse result))))

;; resolve a ^Type hint symbol-name to its canonical class name at def time:
;; "String" -> "java.lang.String", matching the JVM compiler. An
;; already-canonical name maps to itself; an unknown name yields #f (left as-is).
(define class-hint-table (make-hashtable string-hash string=?))
(for-each (lambda (p) (hashtable-set! class-hint-table (car p) (cdr p))) class-token-alist)
(for-each (lambda (p) (hashtable-set! class-hint-table (cdr p) (cdr p))) class-token-alist)
(define (resolve-class-hint name) (hashtable-ref class-hint-table name #f))
(def-var! "jolt.host" "resolve-class-hint" resolve-class-hint)

;; fully-qualified canonical class names — value classes only, NOT the collection
;; interfaces (ISeq/IPersistentMap/...), which downstream code (e.g. SCI) references
;; as protocols/interfaces. def-var! into clojure.core happens in
;; host-static-classes.ss (after make-class-obj is loaded) so tokens evaluate to
;; Class objects.
(define class-fqn-list
  (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
    (let ((result '()))
      (vector-for-each
        (lambda (k _)
          (when (or (not (jch-interface? k))
                    (string=? k "clojure.lang.IExceptionInfo"))
            (set! result (cons k result))))
        keys vals)
      (reverse result))))

;; (str f) of a fn renders JVM-style — "ns$name@hexhash" — so code that parses
;; fn identity out of the string (expound's pprint-fn) finds the $-separated
;; class name instead of a raw Chez #<procedure> form.
(register-str-render!
  (lambda (x) (procedure? x))
  (lambda (x) (string-append (jolt-class-name x) "@"
                             (string-downcase (number->string (abs (equal-hash x)) 16)))))
;; pr/print of a fn uses the JVM object form — #object[ns$name 0xHASH
;; "ns$name@HASH"] — which fn-identity parsers (lasertag's resolve-fn-name)
;; read the class name out of.
(register-pr-arm!
  (lambda (x) (procedure? x))
  (lambda (x)
    (let ((cn (jolt-class-name x))
          (h (string-downcase (number->string (abs (equal-hash x)) 16))))
      (string-append "#object[" cn " 0x" h " \"" cn "@" h "\"]"))))
;; print of a fn uses the same #object form as pr (the JVM prints fns through
;; print-method Object on both paths); str keeps the bare cn@hash.
(let ((prev (var-deref "clojure.core" "__print1")))
  (def-var! "clojure.core" "__print1"
    (lambda (x)
      (if (procedure? x)
          (let ((cn (jolt-class-name x))
                (h (string-downcase (number->string (abs (equal-hash x)) 16))))
            (string-append "#object[" cn " 0x" h " \"" cn "@" h "\"]"))
          (jolt-invoke1 prev x)))))
