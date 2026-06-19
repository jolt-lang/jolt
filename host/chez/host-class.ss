;; host class tokens (jolt-13zk) — a bare class name (String, Keyword, File...)
;; evaluates to its JVM canonical-name STRING, the same value (class instance)
;; returns, so (= String (class "x")) holds and a (defmethod m String ...) keys
;; against a (class …) dispatch (ring.util.request does this). Mirrors
;; src/jolt/eval_resolve.janet's class-canonical-names + core_refs.janet's
;; core-class. The analyzer already resolves these names to clojure.core vars (the
;; seed ctx interns them via setup-class-ctors), so the back end emits
;; (var-deref "clojure.core" "String") — def-var!'ing the canonical strings here is
;; all that's needed at runtime. No analyzer change, so the Janet back end is
;; untouched.
;;
;; Loaded after natives-meta.ss (jolt-type) + the printer (jolt-str-render-one).

;; (class x) — Clojure's class of a value. Scalars map to their JVM class name,
;; matching core-class. Collections/seqs have no JVM class on this host; the seed
;; leaks the Janet host type ("table"/"struct"/"tuple") there, which we don't
;; reproduce (Janet is going away) — (str (type x)) is the clean host taxonomy and
;; is never compared against a class token in the corpus. Records yield their
;; ns-qualified class name (= (str (type x))). Total — never crashes.
(define (jolt-class x)
  (cond
    ((jolt-nil? x) jolt-nil)
    ((boolean? x) "java.lang.Boolean")
    ((number? x) "java.lang.Number")
    ((string? x) "java.lang.String")
    ((keyword? x) "clojure.lang.Keyword")
    ((symbol-t? x) "clojure.lang.Symbol")
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
