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
    ((char? x) "java.lang.Character")
    ((regex-t? x) "java.util.regex.Pattern")
    ((procedure? x) "clojure.lang.IFn")
    ;; an exception value (ex-info / host-constructed throwable) reports its JVM
    ;; class, so (= clojure.lang.ExceptionInfo (class e)) and clojure.test's
    ;; (thrown? Class …) match (records.ss ex-info-map?/ex-info-class).
    ((ex-info-map? x) (ex-info-class x))
    ;; persistent collections + namespace report their JVM class names (not jolt's
    ;; internal :vector/:set/… type keyword), so class-based dispatch — e.g. a
    ;; defmulti on [(class a) (class b)] — sees a real clojure.lang.* class.
    ((jns? x) "clojure.lang.Namespace")
    ((pvec? x) "clojure.lang.PersistentVector")
    ((pset? x) "clojure.lang.PersistentHashSet")
    ((pmap? x) "clojure.lang.PersistentArrayMap")
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
  (lambda (x) (if (string=? (chez-condition-exc-class x) "ArityException")
                  "clojure.lang.ArityException"
                  "java.lang.IllegalArgumentException")))
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
                (string-append (car p) "$" (class-munge-name (cdr p))))))

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

;; bare class-name tokens -> canonical JVM class-name strings.
(define class-token-alist
  '(("String" . "java.lang.String") ("Number" . "java.lang.Number")
    ("Boolean" . "java.lang.Boolean") ("Long" . "java.lang.Long")
    ("Integer" . "java.lang.Integer") ("Double" . "java.lang.Double")
    ("Float" . "java.lang.Float") ("Byte" . "java.lang.Byte") ("Short" . "java.lang.Short")
    ("Object" . "java.lang.Object") ("Character" . "java.lang.Character")
    ("InputStream" . "java.io.InputStream") ("OutputStream" . "java.io.OutputStream")
    ("File" . "java.io.File") ("Reader" . "java.io.Reader") ("Writer" . "java.io.Writer")
    ("ISeq" . "clojure.lang.ISeq") ("Keyword" . "clojure.lang.Keyword")
    ("Symbol" . "clojure.lang.Symbol") ("MapEntry" . "clojure.lang.MapEntry")
    ("StringReader" . "java.io.StringReader") ("StringWriter" . "java.io.StringWriter")
    ("StringBuilder" . "java.lang.StringBuilder")
    ("StringTokenizer" . "java.util.StringTokenizer")
    ("Charset" . "java.nio.charset.Charset") ("Base64" . "java.util.Base64")
    ("Exception" . "java.lang.Exception")
    ("IllegalArgumentException" . "java.lang.IllegalArgumentException")
    ("ArityException" . "clojure.lang.ArityException")
    ("IllegalStateException" . "java.lang.IllegalStateException")
    ("RuntimeException" . "java.lang.RuntimeException")
    ("UnsupportedOperationException" . "java.lang.UnsupportedOperationException")
    ("InterruptedException" . "java.lang.InterruptedException")
    ("IOException" . "java.io.IOException")
    ("UnknownHostException" . "java.net.UnknownHostException")
    ("ConnectException" . "java.net.ConnectException")
    ("SocketTimeoutException" . "java.net.SocketTimeoutException")
    ("MalformedURLException" . "java.net.MalformedURLException")
    ("SSLException" . "javax.net.ssl.SSLException")
    ("ExceptionInfo" . "clojure.lang.ExceptionInfo")
    ("IExceptionInfo" . "clojure.lang.IExceptionInfo")
    ("Pattern" . "java.util.regex.Pattern")
    ("URI" . "java.net.URI") ("UUID" . "java.util.UUID")
    ("ArrayList" . "java.util.ArrayList") ("PersistentQueue" . "clojure.lang.PersistentQueue")
    ("NumberFormatException" . "java.lang.NumberFormatException")
    ("ArithmeticException" . "java.lang.ArithmeticException")
    ("NullPointerException" . "java.lang.NullPointerException")
    ("ClassCastException" . "java.lang.ClassCastException")
    ("IndexOutOfBoundsException" . "java.lang.IndexOutOfBoundsException")
    ("UnsupportedEncodingException" . "java.io.UnsupportedEncodingException")
    ("FileNotFoundException" . "java.io.FileNotFoundException")
    ("Throwable" . "java.lang.Throwable")
    ;; clojure.lang / java.util types that class-based multimethods dispatch on.
    ("Fn" . "clojure.lang.Fn") ("IFn" . "clojure.lang.IFn")
    ("Namespace" . "clojure.lang.Namespace") ("Named" . "clojure.lang.Named")
    ("Set" . "java.util.Set") ("List" . "java.util.List") ("Map" . "java.util.Map")
    ("Collection" . "java.util.Collection") ("Iterable" . "java.lang.Iterable")
    ("CharSequence" . "java.lang.CharSequence") ("Comparable" . "java.lang.Comparable")
    ("Runnable" . "java.lang.Runnable") ("Callable" . "java.util.concurrent.Callable")
    ("IPersistentSet" . "clojure.lang.IPersistentSet")
    ("IPersistentVector" . "clojure.lang.IPersistentVector")
    ("IPersistentMap" . "clojure.lang.IPersistentMap")
    ("IPersistentCollection" . "clojure.lang.IPersistentCollection")
    ("Sequential" . "clojure.lang.Sequential") ("Seqable" . "clojure.lang.Seqable")
    ("Associative" . "clojure.lang.Associative")))
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
  '("java.lang.Long" "java.lang.Integer" "java.lang.Double" "java.lang.Float"
    "java.lang.Byte" "java.lang.Short"
    "java.lang.Number" "java.lang.String" "java.lang.Boolean" "java.lang.Character"
    "java.lang.Object"
    ;; exception classes compared against (class e): (= java.net.SocketTimeoutException (class e))
    "java.lang.Exception" "java.lang.Throwable" "java.lang.RuntimeException"
    "java.lang.IllegalArgumentException" "java.lang.IllegalStateException"
    "java.lang.UnsupportedOperationException" "java.io.IOException"
    "java.net.UnknownHostException" "java.net.ConnectException"
    "java.net.SocketTimeoutException" "java.net.MalformedURLException"
    "javax.net.ssl.SSLException"
    "java.lang.NumberFormatException" "java.lang.ArithmeticException"
    "java.lang.NullPointerException" "java.lang.ClassCastException"
    "java.lang.IndexOutOfBoundsException" "java.io.FileNotFoundException"
    "java.io.UnsupportedEncodingException"
    ;; clojure.lang.ExceptionInfo / IExceptionInfo compared against (class e)
    "clojure.lang.ExceptionInfo" "clojure.lang.IExceptionInfo" "clojure.lang.ArityException"
    "java.util.regex.Pattern" "java.net.URI" "java.util.UUID"
    "clojure.lang.PersistentQueue"
    "clojure.lang.Keyword" "clojure.lang.Symbol" "clojure.lang.Ratio" "clojure.lang.Atom"))
