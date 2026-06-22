;; host class tokens (jolt-13zk) — a bare class name (String, Keyword, File...)
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
(define (jolt-class x)
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
    ((procedure? x) "clojure.lang.IFn")
    (else (jolt-str-render-one (jolt-type x)))))

(def-var! "clojure.core" "class" jolt-class)

;; bare class-name tokens -> canonical JVM class-name strings.
(define class-token-alist
  '(("String" . "java.lang.String") ("Number" . "java.lang.Number")
    ("Boolean" . "java.lang.Boolean") ("Long" . "java.lang.Long")
    ("Integer" . "java.lang.Integer") ("Double" . "java.lang.Double")
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
    ("Throwable" . "java.lang.Throwable")))
(for-each
  (lambda (pair) (def-var! "clojure.core" (car pair) (cdr pair)))
  class-token-alist)

;; resolve a ^Type hint symbol-name to its canonical class name at def time
;; (jolt-a1ir): "String" -> "java.lang.String", matching the JVM compiler. An
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
    "java.lang.Number" "java.lang.String" "java.lang.Boolean" "java.lang.Character"
    "java.lang.Object"
    ;; exception classes compared against (class e): (= java.net.SocketTimeoutException (class e))
    "java.lang.Exception" "java.lang.Throwable" "java.lang.RuntimeException"
    "java.lang.IllegalArgumentException" "java.lang.IllegalStateException"
    "java.lang.UnsupportedOperationException" "java.io.IOException"
    "java.net.UnknownHostException" "java.net.ConnectException"
    "java.net.SocketTimeoutException" "java.net.MalformedURLException"
    "javax.net.ssl.SSLException"
    "clojure.lang.Keyword" "clojure.lang.Symbol" "clojure.lang.Ratio" "clojure.lang.Atom"))
