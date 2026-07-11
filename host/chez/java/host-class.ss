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
(define (jolt-class-base x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((boolean? x) "java.lang.Boolean")
    ;; per-type number classes, like the JVM: integer -> Long, flonum -> Double,
    ;; exact non-integer -> Ratio.
    ((and (number? x) (flonum? x)) "java.lang.Double")
    ((and (number? x) (exact? x) (integer? x)) "java.lang.Long")
    ((and (number? x) (exact? x) (rational? x)) "clojure.lang.Ratio")
    ((number? x) "java.lang.Number")
    ((string? x) "java.lang.String")
    ((keyword? x) "clojure.lang.Keyword")
    ((symbol-t? x) "clojure.lang.Symbol")
    ((jolt-atom? x) "clojure.lang.Atom")
    ((jolt-ref? x) "clojure.lang.Ref")
    ((char? x) "java.lang.Character")
    ((regex-t? x) "java.util.regex.Pattern")
    ;; an anonymous / unregistered fn — like the JVM, where (class #(..)) is a
    ;; concrete ns$fn__N subclass. The $fn marker lets clojure.spec.alpha's fn-sym
    ;; recognize it as anonymous and return ::s/unknown. A named fn is registered
    ;; (proc-name-tbl) and handled by a class-arm with its real ns$name.
    ((procedure? x) "clojure.lang.AFunction$fn__0")
    ;; an exception value (ex-info / host-constructed throwable) reports its JVM
    ;; class, so (= clojure.lang.ExceptionInfo (class e)) and clojure.test's
    ;; (thrown? Class …) match (records.ss ex-info-map?/ex-info-class).
    ((ex-info-map? x) (ex-info-class x))
    ;; persistent collections + namespace report their JVM class names (not jolt's
    ;; internal :vector/:set/… type keyword), so class-based dispatch — e.g. a
    ;; defmulti on [(class a) (class b)] — sees a real clojure.lang.* class.
    ((jns? x) "clojure.lang.Namespace")
    ;; a map entry is a pvec with the entry flag; the JVM class is MapEntry
    ((jolt-map-entry? x) "clojure.lang.MapEntry")
    ((pvec? x) "clojure.lang.PersistentVector")
    ((pset? x) "clojure.lang.PersistentHashSet")
    ;; array mode (insertion-ordered, small literal maps) is PersistentArrayMap;
    ;; hash mode (hash-map, or grown past the array limit) is PersistentHashMap
    ((pmap? x) (if (pmap-order x) "clojure.lang.PersistentArrayMap"
                   "clojure.lang.PersistentHashMap"))
    ((jolt-lazyseq? x) "clojure.lang.LazySeq")
    ((empty-list-t? x) "clojure.lang.PersistentList$EmptyList")
    ((cseq? x) "clojure.lang.PersistentList")
    (else (jolt-str-render-one (jolt-type x)))))
;; the class NAME of x (string), or nil for nil. (class x) wraps it in a Class
;; value (make-class-obj, host-static-classes.ss) so it renders like a JVM Class
;; while staying = its name string.
;; a raw Chez condition Clojure raises a specific class for (records-interop.ss
;; chez-condition-exc-class) reports that JVM class, so (class e) and a
;; (thrown? ArityException …) test match — not the opaque :object fallback.
(register-class-arm!
  (lambda (x) (and (chez-condition-exc-class x) #t))
  (lambda (x) (let ((p (assoc (chez-condition-exc-class x) class-token-alist)))
                (if p (cdr p) "java.lang.IllegalArgumentException"))))
;; A fn def'd into a var reports a JVM-style class name "ns$munged-name" (the
;; forward CHAR_MAP), so clojure.spec.alpha's fn-sym (which splits on $ and
;; demunges) recovers the predicate's symbol. Anonymous / unregistered fns stay
;; clojure.lang.IFn (fn-sym yields :unknown, as on the JVM).
(define class-munge-map
  '((#\? . "_QMARK_") (#\! . "_BANG_") (#\* . "_STAR_") (#\+ . "_PLUS_")
    (#\> . "_GT_") (#\< . "_LT_") (#\= . "_EQ_") (#\/ . "_SLASH_") (#\- . "_")
    (#\& . "_AMPERSAND_") (#\% . "_PERCENT_") (#\~ . "_TILDE_") (#\^ . "_CARET_")
    (#\| . "_BAR_") (#\: . "_COLON_")))
(define (class-munge-name s)
  (let ((out (open-output-string)))
    (string-for-each
     (lambda (c) (let ((t (assv c class-munge-map))) (if t (display (cdr t) out) (write-char c out))))
     s)
    (get-output-string out)))
(register-class-arm!
  (lambda (x) (and (procedure? x) (hashtable-ref proc-name-tbl x #f)))
  (lambda (x) (let ((p (hashtable-ref proc-name-tbl x #f)))
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
(define class-token-alist
  (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
    (let ((result '()) (seen (make-hashtable string-hash string=?)))
      (vector-for-each
        (lambda (k _)
          (let ((s (jch-last-segment k)))
            (when (not (hashtable-ref seen s #f))
              (hashtable-set! seen s #t)
              (set! result (cons (cons s k) result)))))
        keys vals)
      (reverse result))))
(for-each
  (lambda (pair) (def-var! "clojure.core" (car pair) (cdr pair)))
  class-token-alist)

;; resolve a ^Type hint symbol-name to its canonical class name at def time:
;; "String" -> "java.lang.String", matching the JVM compiler. An
;; already-canonical name maps to itself; an unknown name yields #f (left as-is).
(define class-hint-table (make-hashtable string-hash string=?))
(for-each (lambda (p) (hashtable-set! class-hint-table (car p) (cdr p))) class-token-alist)
(for-each (lambda (p) (hashtable-set! class-hint-table (cdr p) (cdr p))) class-token-alist)
(define (resolve-class-hint name) (hashtable-ref class-hint-table name #f))
(def-var! "jolt.host" "resolve-class-hint" resolve-class-hint)

;; fully-qualified canonical class names self-evaluate to their own name string,
;; so (= (class 1) java.lang.Long) and (instance? clojure.lang.Atom x) resolve the
;; class token (= what jolt-class / instance-check key on).
;; Value classes only — NOT the collection interfaces (ISeq/IPersistentMap/...),
;; which downstream code (e.g. SCI) references as protocols/interfaces.
(for-each
  (lambda (nm) (def-var! "clojure.core" nm nm))
  (let-values (((keys vals) (hashtable-entries jvm-class-parents)))
    (let ((result '()))
      (vector-for-each
        (lambda (k _)
          (when (or (not (jch-interface? k))
                    (string=? k "clojure.lang.IExceptionInfo"))
            (set! result (cons k result))))
        keys vals)
      (reverse result))))
