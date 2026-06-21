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
(for-each
  (lambda (pair) (def-var! "clojure.core" (car pair) (cdr pair)))
  '(("String" . "java.lang.String") ("Number" . "java.lang.Number")
    ("Boolean" . "java.lang.Boolean") ("Long" . "java.lang.Long")
    ("Integer" . "java.lang.Integer") ("Double" . "java.lang.Double")
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
    ("InterruptedException" . "java.lang.InterruptedException")
    ("Throwable" . "java.lang.Throwable")))

;; fully-qualified canonical class names self-evaluate to their own name string,
;; so (= (class 1) java.lang.Long) and (instance? clojure.lang.Atom x) resolve the
;; class token (= what jolt-class / instance-check key on).
(for-each
  (lambda (nm) (def-var! "clojure.core" nm nm))
  '("java.lang.Long" "java.lang.Integer" "java.lang.Double" "java.lang.Float"
    "java.lang.Number" "java.lang.String" "java.lang.Boolean" "java.lang.Character"
    "java.lang.Object" "java.lang.CharSequence" "java.lang.Comparable"
    "clojure.lang.Keyword" "clojure.lang.Symbol" "clojure.lang.Ratio" "clojure.lang.BigInt"
    "clojure.lang.Atom" "clojure.lang.IFn" "clojure.lang.Fn" "clojure.lang.ISeq"
    "clojure.lang.IPersistentMap" "clojure.lang.IPersistentVector" "clojure.lang.IPersistentCollection"
    "clojure.lang.PersistentVector" "clojure.lang.Var" "clojure.lang.Namespace"
    "clojure.lang.MapEntry"))
