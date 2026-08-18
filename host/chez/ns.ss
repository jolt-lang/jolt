;; namespaces — the namespace value model.
;;
;; The namespace ops (find-ns/resolve/in-ns/…) work over the rt.ss var-table
;; (cells carry ns + name + defined?) and the multimethods.ss chez-current-ns
;; box. A namespace VALUE is a `jns` record carrying its name string — distinct
;; from a map/record so (map? ns) is #f, but the overlay's `ns-name` reads
;; (get ns :name); that's overridden natively in post-prelude.ss (loads after
;; the overlay clobbers it).
;;
;; Loaded LAST from rt.ss. The analyzer bakes a def's target ns at compile time,
;; so a runtime in-ns redirects only *ns* / str-of-ns, not later defs in the
;; same program.

(define-record-type jns (fields name) (nongenerative chez-jns-v1))

;; registry: name-string -> jns. Seeded with the two always-present namespaces;
;; grown by in-ns / create-ns. find-ns ALSO derives existence from the var-table
;; (any cell with that ns), so a namespace that only ever had vars def'd into it
;; is still found.
(define ns-registry (make-hashtable string-hash string=?))
;; ns-registry-mu covers every MUTATION of ns-registry and every WHOLE-TABLE scan
;; of it, on the same split rt.ss spells out for var-table: a strong general
;; hashtable is safe to probe for one key unlocked (a resize publishes a complete
;; new bucket vector and never rewrites the old one, so the worst an unlocked
;; reader sees is a stale miss), but hashtable-keys is not — it reads ht-size,
;; allocates a vector of that length and walks the buckets bounded by both, so a
;; concurrent intern truncates the result and a concurrent remove-ns leaves
;; trailing FILL slots that the caller then hands to string-hash.
;;
;; Interning is double-checked for the same reason jolt-var is: two threads
;; racing (create-ns 'foo) on a name that does not exist yet would otherwise each
;; build their own jns, and find-ns would hand out two objects for one namespace.
(define ns-registry-mu (make-mutex))
(define (intern-ns! name)
  (or (hashtable-ref ns-registry name #f)
      (jolt-with-mutex ns-registry-mu
        (or (hashtable-ref ns-registry name #f)
            (let ((n (make-jns name))) (hashtable-set! ns-registry name n) n)))))
;; The lock covers only the snapshot; callers walk the returned vector outside it.
(define (ns-registry-names)
  (jolt-with-mutex ns-registry-mu (hashtable-keys ns-registry)))
(intern-ns! "user")
(intern-ns! "clojure.core")

;; --- namespace aliases ------------------------------------------------------
;; (require '[ns :as a]) registers a -> ns so the analyzer can resolve a/foo to
;; ns/foo. Keyed by (compile-ns . alias). The requires are pre-registered at
;; analyze time (compile-eval.ss) — analysis precedes eval, so a runtime require
;; no-op is fine. Also drives jolt-ns-aliases below.
;; ns-map-mu covers every MUTATION of the alias / refer / refer-all / exclude
;; tables below and every WHOLE-TABLE scan of them. The single-key READS stay
;; unlocked and that is the whole design: chez-resolve-alias and
;; chez-resolve-refer sit on the analyzer's symbol-resolution path, which runs
;; per symbol per form, and a mutex there would be felt on every compile. It is
;; sound for the same reason var-table's probe is (rt.ss): these are strong
;; general hashtables, so a resize publishes a complete new bucket vector and an
;; unlocked reader's worst case is a stale miss.
;;
;; The writers are require / use / ns-unalias, which run from whatever thread
;; loads a namespace — an nREPL session and the main program at once, say — and
;; unsynchronized inserts into one of these drop mappings, which surfaces as a
;; symbol that will not resolve in a namespace that plainly refers it.
(define ns-map-mu (make-mutex))
(define ns-alias-table (make-hashtable equal-hash equal?))
(define (chez-register-alias! cns alias target)
  (jolt-with-mutex ns-map-mu (hashtable-set! ns-alias-table (cons cns alias) target)))
(define (chez-resolve-alias cns alias)
  (hashtable-ref ns-alias-table (cons cns alias) #f))
;; :refer brings an UNQUALIFIED name into cns, resolving to target-ns/name.
(define ns-refer-table (make-hashtable equal-hash equal?))
(define (chez-register-refer! cns name target)
  (jolt-with-mutex ns-map-mu (hashtable-set! ns-refer-table (cons cns name) target)))
;; refer-all (a bare `use`): cns -> list of fully-referred target ns names. A name
;; not found per-name resolves to the first refer-all target that defines it.
(define ns-refer-all-table (make-hashtable equal-hash equal?))
(define (chez-register-refer-all! cns target)
  ;; read-modify-write of a list, so it is one step: two threads `use`-ing
  ;; different namespaces into one cns would otherwise each cons onto the value
  ;; they read and the second write would drop the first's target.
  (jolt-with-mutex ns-map-mu
    (let ((cur (hashtable-ref ns-refer-all-table cns '())))
      (unless (member target cur)
        (hashtable-set! ns-refer-all-table cns (cons target cur))))))
;; :use with :exclude drops names from the refer-all set: (cns . target) ->
;; (name -> #t). An excluded name is skipped for that target in the refer-all
;; walk, so resolution falls through to an earlier refer-all target (or fails)
;; exactly as load-lib's filtered refer leaves the prior mapping in place.
(define ns-refer-all-exclude-table (make-hashtable equal-hash equal?))
(define (chez-register-refer-all-excludes! cns target names)
  (when (pair? names)
    (jolt-with-mutex ns-map-mu
      (let ((h (or (hashtable-ref ns-refer-all-exclude-table (cons cns target) #f)
                   (let ((h (make-hashtable string-hash string=?)))
                     (hashtable-set! ns-refer-all-exclude-table (cons cns target) h)
                     h))))
        (for-each (lambda (n) (hashtable-set! h n #t)) names)))))
(define (chez-refer-all-excluded? cns target name)
  (let ((h (hashtable-ref ns-refer-all-exclude-table (cons cns target) #f)))
    (and h (hashtable-ref h name #f) #t)))
(define (chez-resolve-refer cns name)
  (let ((r (hashtable-ref ns-refer-table (cons cns name) #f)))
    (cond
     ((eq? r 'unmapped) #f)
     (r r)
     (else
      (let loop ((ts (hashtable-ref ns-refer-all-table cns '())))
        (cond ((null? ts) #f)
              ((and (not (chez-refer-all-excluded? cns (car ts) name))
                    (let ((c (var-cell-lookup (car ts) name))) (and c (var-cell-defined? c))))
               (car ts))
              (else (loop (cdr ts)))))))))
;; --- libspec parsing (shared by the loader + compile-eval) ------------------
;; A libspec is one of: a bare symbol `foo`; a vector `[foo :as f :refer [x]]`;
;; or a LIST with keyword options `(foo :only [x])` (jolt superset — see below).
;; parse-libspec returns (target-name . opts) where opts is an alist of
;; (keyword-name-string . value-form), or #f if `spec` isn't a recognizable
;; libspec. This is the single spec->target+opts parser routed through by
;; loader-require / loader-use (loader.ss), chez-register-spec!, and — via it —
;; ce-scan-requires! (compile-eval.ss).
;;
;; JOLT SUPERSET: a bare LIST libspec — (ns :only [x]) or (ns :as a) — is
;; accepted here. On reference Clojure this shape is FATAL: the list is treated
;; as a prefix list whose second element must be a lib, so a keyword there throws
;; "Don't know how to create ISeq from: clojure.lang.Keyword". Accepting it is a
;; safe superset — no JVM-valid program changes meaning — and keeps the loader's
;; expand-spec consistent with this parser (both treat a list with a keyword
;; second element as one libspec). A list whose second element is NOT a keyword
;; stays a PREFIX list (JVM parity): (clojure [string :as str]).
(define (spec-items spec)
  (cond ((pvec? spec) (seq->list spec))
        ((or (cseq? spec) (empty-list-t? spec)) (seq->list spec))
        ((symbol-t? spec) (list spec))
        (else '())))

(define (parse-libspec spec)
  (let ((items (spec-items spec)))
    (and (pair? items) (symbol-t? (car items))
         (let loop ((xs (cdr items)) (opts '()))
           (cond ((or (null? xs) (null? (cdr xs))) (cons (symbol-t-name (car items)) (reverse opts)))
                 ((keyword? (car xs))
                  (loop (cddr xs) (cons (cons (keyword-t-name (car xs)) (cadr xs)) opts)))
                 (else (cons (symbol-t-name (car items)) (reverse opts))))))))

;; A LIST-shaped spec is a PREFIX list (not a single libspec) iff its second
;; element is NOT a keyword — the JVM (clojure [string :as str]) form. A list
;; whose second element IS a keyword is the list-libspec superset above.
(define (prefix-list-items? items)
  (and (pair? items) (symbol-t? (car items))
       (pair? (cdr items))
       (not (keyword? (cadr items)))))

;; Register a parsed libspec's :as alias + :refer/:only names under `cns`.
(define (chez-register-spec! cns spec)
  (let ((parsed (parse-libspec spec)))
    (when parsed
      (let ((target (car parsed)))
        (for-each
          (lambda (opt)
            (let ((k (car opt)) (v (cdr opt)))
              (cond
                ;; :as-alias aliases exactly like :as (clojure.core load-lib does
                ;; the same `alias` call for both); what differs is that it does not
                ;; load the target — see ldr-load+register.
                ((or (string=? k "as") (string=? k "as-alias"))
                 (when (symbol-t? v) (chez-register-alias! cns (symbol-t-name v) target)))
                ;; :refer (require) and :only (use) both bring unqualified names
                ;; into cns resolving to target/name.
                ((or (string=? k "refer") (string=? k "only"))
                 (cond
                   ;; :refer :all — bring in every public var (require :refer :all)
                   ((and (keyword? v) (string=? (keyword-t-name v) "all"))
                    (chez-register-refer-all! cns target))
                   ;; :refer [a b] or :refer (a b) — both forms list names to bring in.
                   ((or (pvec? v) (cseq? v) (empty-list-t? v))
                    (for-each (lambda (n)
                                (when (symbol-t? n) (chez-register-refer! cns (symbol-t-name n) target)))
                              (seq->list v))))))))
          (cdr parsed))))))

;; a namespace designator -> its name string (a jns or a symbol; the corpus never
;; passes a bare string).
(define (ns-desig->name d)
  (if (jns? d) (jns-name d) (symbol-t-name d)))

(define (ns-has-vars? nm)
  (hashtable-ref ns-has-vars-set nm #f))

;; Does this NAME string denote a live namespace? Same rule find-ns uses (the
;; registry, or any var interned under the name), without the designator
;; conversion — callers that already hold a name string.
(define (chez-ns-exists? nm)
  (and (string? nm) (or (and (hashtable-ref ns-registry nm #f) #t) (ns-has-vars? nm) #f)))

(define (jolt-find-ns desig)
  (let ((nm (ns-desig->name desig)))
    (or (hashtable-ref ns-registry nm #f)
        (and (ns-has-vars? nm) (intern-ns! nm))
        jolt-nil)))

(define (jolt-the-ns desig)
  (if (jns? desig) desig
      (let ((n (jolt-find-ns desig)))
        (if (jns? n) n (throw-jvm (quote Exception) (string-append "No namespace: " (jolt-final-str desig) " found"))))))

(define (jolt-create-ns desig) (intern-ns! (ns-desig->name desig)))

;; in-ns: register + switch the current ns + re-bind *ns* + return the jns. This
;; updates only the RUNTIME current ns — subsequent defs in the same program were
;; already ns-baked by the analyzer, so it does not redirect them. It is enough
;; for *ns* / str-of-ns to track the switch.
(define (jolt-in-ns desig)
  (let* ((nm (ns-desig->name desig)) (n (intern-ns! nm)))
    ;; set the THREAD-LOCAL current ns; *ns* reads derive from it (dyn-binding.ss),
    ;; so this is per-thread — concurrent nREPL sessions don't clobber each other.
    (set-chez-ns! nm)
    n))

;; ns-name: a namespace's name as a (no-ns) symbol. Overrides the overlay (which
;; reads (get ns :name) = nil on a jns record) — wired in via post-prelude.ss.
(define (jolt-ns-name desig)
  (jolt-symbol #f (jns-name (jolt-the-ns desig))))

(define (jolt-all-ns)
  (let ((seen (make-hashtable string-hash string=?)))
    (vector-for-each (lambda (k) (hashtable-set! seen k #t)) (ns-registry-names))
    ;; the ns-cells index's keys ARE the namespaces with interned vars — the
    ;; whole-table cell scan this replaces cost O(total vars) per all-ns call
    (vector-for-each (lambda (k) (hashtable-set! seen k #t)) (ns-index-names))
    (list->cseq (map intern-ns! (vector->list (hashtable-keys seen))))))

;; ns-publics / ns-map / ns-interns: a {sym -> var-cell} jolt map built by scanning
;; the var-table for defined cells in the namespace. ns-interns/ns-map keep every
;; var; ns-publics drops the ones marked ^:private (defn-/def ^:private), like the
;; JVM. ns-aliases is an empty map (map? is true).
(define (var-private? c)
  (let ((m (var-cell-meta c)))
    (and m (jolt-truthy? (jolt-get m (keyword #f "private"))))))
(define (ns-vars-pmap-when nm keep?)
  ;; the namespace's own bucket (rt.ss ns-cells-index): O(vars in nm), where a
  ;; var-table-cells scan cost O(every var in the image) per call
  (let ((m (jolt-hash-map)))
    (for-each
      (lambda (c)
        (when (and (var-cell-defined? c) (keep? c))
          (set! m (jolt-assoc m (jolt-symbol #f (var-cell-name c)) c))))
      (ns-cells-list nm))
    m))
(define (ns-vars-pmap nm) (ns-vars-pmap-when nm (lambda (c) #t)))
(define (jolt-ns-publics desig) (ns-vars-pmap-when (ns-desig->name desig) (lambda (c) (not (var-private? c)))))
(define (jolt-ns-interns desig) (ns-vars-pmap (ns-desig->name desig)))

;; ns-aliases: the {alias-sym -> ns-value} registered under `desig`
;; (default the current ns) via require :as / alias. Reads ns-alias-table.
(define (jolt-ns-aliases . desig)
  (let ((cns (if (pair? desig) (ns-desig->name (car desig)) (chez-current-ns)))
        (m (jolt-hash-map)))
    (vector-for-each
      (lambda (k)
        (when (string=? (car k) cns)
          (set! m (jolt-assoc m (jolt-symbol #f (cdr k))
                              (intern-ns! (hashtable-ref ns-alias-table k #f))))))
      (jolt-with-mutex ns-map-mu (hashtable-keys ns-alias-table)))
    m))

;; ns-refers: the {sym -> var} referred into `desig` via refer/use, plus the
;; implicit refer-clojure every namespace gets — clojure.core's publics are
;; visible everywhere, minus names the ns interns itself (an intern replaces
;; the mapping in the JVM's namespace table).
(define (jolt-ns-refers desig)
  (let* ((cns (ns-desig->name desig))
         (m (if (string=? cns "clojure.core")
                (jolt-hash-map)
                (pmap-fold (ns-vars-pmap-when "clojure.core"
                                              (lambda (c) (not (var-private? c))))
                           (lambda (k v acc)
                             (let ((local (var-cell-lookup cns (symbol-t-name k))))
                               (if (and local (var-cell-defined? local))
                                   acc
                                   (jolt-assoc acc k v))))
                           (jolt-hash-map)))))
     (vector-for-each
       (lambda (k)
         (when (string=? (car k) cns)
           (let* ((target (hashtable-ref ns-refer-table k #f))
                  (c (and target (var-cell-lookup target (cdr k)))))
             (when c (set! m (jolt-assoc m (jolt-symbol #f (cdr k)) c))))))
       (jolt-with-mutex ns-map-mu (hashtable-keys ns-refer-table)))
     ;; refer-all: merge all public vars from :refer :all namespaces
     (let ((all-refs (hashtable-ref ns-refer-all-table cns #f)))
       (when all-refs
         (set! m
               (fold-left
                (lambda (acc target-ns)
                  (let ((publics (ns-vars-pmap-when target-ns
                                                   (lambda (c) (not (var-private? c))))))
                     (pmap-fold
                      publics
                      (lambda (k v acc)
                        (let ((nm (symbol-t-name k)))
                          (if (or (jolt-contains? acc (jolt-symbol #f nm))
                                  (chez-refer-all-excluded? cns target-ns nm))
                              acc
                              (jolt-assoc acc (jolt-symbol #f nm) v))))
                      acc)))
                m
                all-refs))))
     m))

;; ns-imports: clojure.core auto-imports the 96 public java.lang classes into
;; every ns. jolt has no classloader, but returns that map (short symbol ->
;; canonical class-name token) so (count (ns-imports 'user)) = 96 like the JVM.
(define jolt-default-import-names
  '("AbstractMethodError" "Appendable" "ArithmeticException" "ArrayIndexOutOfBoundsException"
    "ArrayStoreException" "AssertionError" "BigDecimal" "BigInteger" "Boolean" "Byte"
    "Callable" "CharSequence" "Character" "Class" "ClassCastException" "ClassCircularityError"
    "ClassFormatError" "ClassLoader" "ClassNotFoundException" "CloneNotSupportedException"
    "Cloneable" "Comparable" "Compiler" "Deprecated" "Double" "Enum"
    "EnumConstantNotPresentException" "Error" "Exception" "ExceptionInInitializerError" "Float"
    "IllegalAccessError" "IllegalAccessException" "IllegalArgumentException"
    "IllegalMonitorStateException" "IllegalStateException" "IllegalThreadStateException"
    "IncompatibleClassChangeError" "IndexOutOfBoundsException" "InheritableThreadLocal"
    "InstantiationError" "InstantiationException" "Integer" "InternalError" "InterruptedException"
    "Iterable" "LinkageError" "Long" "Math" "NegativeArraySizeException" "NoClassDefFoundError"
    "NoSuchFieldError" "NoSuchFieldException" "NoSuchMethodError" "NoSuchMethodException"
    "NullPointerException" "Number" "NumberFormatException" "Object" "OutOfMemoryError" "Override"
    "Package" "Process" "ProcessBuilder" "Readable" "Runnable" "Runtime" "RuntimeException"
    "RuntimePermission" "SecurityException" "SecurityManager" "Short" "StackOverflowError"
    "StackTraceElement" "StrictMath" "String" "StringBuffer" "StringBuilder"
    "StringIndexOutOfBoundsException" "SuppressWarnings" "System" "Thread" "Thread$State"
    "Thread$UncaughtExceptionHandler" "ThreadDeath" "ThreadGroup" "ThreadLocal" "Throwable"
    "TypeNotPresentException" "UnknownError" "UnsatisfiedLinkError" "UnsupportedClassVersionError"
    "UnsupportedOperationException" "VerifyError" "VirtualMachineError" "Void"))
(define jolt-default-imports
  (let loop ((ns jolt-default-import-names) (m (jolt-hash-map)))
    (if (null? ns) m
        (loop (cdr ns)
              (jolt-assoc m (jolt-symbol #f (car ns)) (string-append "java.lang." (car ns)))))))
(define (jolt-ns-imports . _) jolt-default-imports)

;; ns-map: every mapping visible in the ns — the java.lang imports, the refers
;; (including the implicit clojure.core publics), and the ns's own interns,
;; later groups replacing earlier ones like the JVM's single mapping table.
(define (jolt-ns-map desig)
  (let* ((m (jolt-ns-imports desig))
         (m (pmap-fold (jolt-ns-refers desig)
                       (lambda (k v acc) (jolt-assoc acc k v)) m)))
    (pmap-fold (jolt-ns-interns desig)
               (lambda (k v acc) (jolt-assoc acc k v)) m)))

;; resolve: an unqualified symbol resolves in the current ns then clojure.core; a
;; qualified one in its own ns. Returns the var iff genuinely defined, else nil —
;; never interns an empty cell (var-cell-lookup is non-creating).
;; resolve `sym` in the current ns: a qualified ns part is read as an :as alias
;; (then a real ns); an unqualified name resolves in the current ns, its :refers,
;; then clojure.core. (ns-resolve does the same against an explicit ns.)
(define (jolt-resolve-1 sym)
  (let* ((cns (chez-current-ns))
         (sns (symbol-t-ns sym)) (nm (symbol-t-name sym))
         (c (if (string? sns)
                (var-cell-lookup (or (chez-resolve-alias cns sns) sns) nm)
                (or (var-cell-lookup cns nm)
                    (let ((ref (chez-resolve-refer cns nm))) (and ref (var-cell-lookup ref nm)))
                    ;; the implicit clojure.core refer — blocked by an ns-unmap tombstone
                    (and (not (eq? (hashtable-ref ns-refer-table (cons cns nm) #f) 'unmapped))
                         (var-cell-lookup "clojure.core" nm))))))
    (if (and c (var-cell-defined? c)) c jolt-nil)))
;; (resolve sym) resolves globally; (resolve &env sym) additionally answers nil
;; when sym names a local in env (the &env map's keys) — a macro's resolve avoids
;; mistaking a local binding for a global var. schema's macros use the 2-arity.
(define (jolt-resolve a . rest)
  (if (null? rest)
      (jolt-resolve-1 a)
      (let ((env a) (sym (car rest)))
        (if (and (pmap? env) (pmap-contains? env sym)) jolt-nil (jolt-resolve-1 sym)))))

(define (jolt-find-var sym)
  (let ((sns (symbol-t-ns sym)) (nm (symbol-t-name sym)))
    (if (string? sns)
        (let ((c (var-cell-lookup sns nm))) (if (and c (var-cell-defined? c)) c jolt-nil))
        (throw-jvm (quote IllegalArgumentException) "Symbol must be namespace-qualified"))))

;; ns-unmap: clear the mapping — drop defined? and reset the root to unbound, so a
;; later resolve returns nil. Also records an 'unmapped tombstone in the refer table
;; so the name won't resolve through a refer/all mapping.
(define (jolt-ns-unmap ns-desig sym)
  (let* ((cns (jns-name (jolt-the-ns ns-desig)))   ; the-ns throws if the ns is missing (JVM parity)
         (nm  (symbol-t-name sym))
         (c   (var-cell-lookup cns nm)))
    (when c (var-cell-defined?-set! c #f)
            (var-cell-root-set! c (make-jolt-var-unbound (var-cell-ns c) (var-cell-name c))))
    ;; tombstone: block resolution of this name in this ns via refers/all
    (jolt-with-mutex ns-map-mu (hashtable-set! ns-refer-table (cons cns nm) 'unmapped)))
  jolt-nil)

;; --- ns runtime fns ---------------------------------------------------------
;; ns-resolve: resolve `sym` as if reading it in namespace `ns-desig`. Qualified
;; syms consult that ns's :as aliases; unqualified resolve in the ns, its :refers,
;; then clojure.core. Returns the var or nil (never interns).
(define (jolt-ns-resolve ns-desig sym)
  (let* ((cns (jns-name (jolt-the-ns ns-desig)))   ; the-ns throws if the ns is missing (JVM parity)
         (sns (symbol-t-ns sym)) (nm (symbol-t-name sym))
         (c (if (string? sns)
                (var-cell-lookup (or (chez-resolve-alias cns sns) sns) nm)
                (or (var-cell-lookup cns nm)
                    (let ((ref (chez-resolve-refer cns nm))) (and ref (var-cell-lookup ref nm)))
                    (and (not (eq? (hashtable-ref ns-refer-table (cons cns nm) #f) 'unmapped))
                         (var-cell-lookup "clojure.core" nm))))))
    (if (and c (var-cell-defined? c)) c jolt-nil)))

;; remove-ns: drop the namespace from the registry AND its vars, so find-ns
;; (which also derives existence from the var-table) returns nil afterward.
(define (jolt-remove-ns desig)
  (let* ((nm (ns-desig->name desig))
         (n  (jolt-find-ns desig)))            ; the removed Namespace (JVM returns it), or nil
    (jolt-with-mutex ns-registry-mu (hashtable-delete! ns-registry nm))
    ;; the sweep is a var-table mutation, so it runs under rt.ss's var-table-mu
    ;; like every other one — including the hashtable-keys snapshot, which would
    ;; otherwise be taken while another thread interned into the table.
    (jolt-with-mutex var-table-mu
      (hashtable-delete! ns-has-vars-set nm)  ; keep the O(1) index honest, else a
                                              ; later require of nm would no-op
      (hashtable-delete! ns-cells-index nm)   ; and the ns->cells bucket with it
      (vector-for-each
        (lambda (k) (let ((c (hashtable-ref var-table k #f)))
                      (when (and c (string=? (var-cell-ns c) nm)) (hashtable-delete! var-table k))))
        (hashtable-keys var-table)))
    n))

;; intern: create/set a var ns/sym to val (or an unbound cell). Returns the var.
;; The symbol's metadata becomes the var's metadata (Var.setMeta), and a truthy
;; :macro marks the var as a macro so later-compiled forms expand it.
(define (jolt-intern ns-desig sym . vopt)
  (let ((nm (ns-desig->name ns-desig)) (s (symbol-t-name sym)))
    ;; the namespace must exist (Namespace.find), like the JVM's intern
    (unless (hashtable-ref ns-registry nm #f)
      (jolt-throw (jolt-ex-info (string-append "No namespace: " nm " found") empty-pmap)))
    (let ((cell (if (pair? vopt) (def-var! nm s (car vopt)) (declare-var! nm s)))
          (m (jolt-meta sym)))
      (unless (jolt-nil? m)
        (var-cell-meta-set! cell m)
        (var-meta-sync-macro! cell m))
      cell)))

;; alias / ns-unalias: register/drop an :as alias under the current (or given) ns.
;; A runtime alias is registered into the SAME table the analyzer consults, so a
;; later form in the program resolves alias/foo (the spine analyzes form by form).
(define (jolt-alias alias-sym ns-sym)
  (let* ((cns    (chez-current-ns))
         (alias  (symbol-t-name alias-sym))
         ;; the-ns throws "No namespace: X found" if the target doesn't exist —
         ;; JVM aliases the resolved Namespace, so aliasing a missing ns fails now
         ;; instead of leaving a dangling alias that later dies with "Unknown class".
         (target (jns-name (jolt-the-ns ns-sym)))
         (existing (chez-resolve-alias cns alias)))
    ;; re-aliasing an existing alias to a DIFFERENT ns is an error (JVM
    ;; Namespace.addAlias); re-aliasing to the same target is a silent no-op.
    (when (and existing (not (string=? existing target)))
      (throw-jvm (quote IllegalStateException)
                 (string-append "Alias " alias " already exists in namespace " cns
                                ", aliasing " existing)))
    (chez-register-alias! cns alias target)
    jolt-nil))
(define (jolt-ns-unalias ns-desig alias-sym)
  (jolt-with-mutex ns-map-mu
    (hashtable-delete! ns-alias-table (cons (ns-desig->name ns-desig) (symbol-t-name alias-sym))))
  jolt-nil)

;; refer: bring the public vars of `ns-sym` into the current ns as unqualified
;; names. :only [names] restricts to those names; :exclude [names] drops them.
;; (:rename is not yet supported — the refer table keys on the plain name.)
(define (jolt-refer ns-sym . filters)
  (let ((target (ns-desig->name ns-sym)) (cns (chez-current-ns))
        (only #f) (excl '()))
    ;; parse :only / :exclude name lists into string sets
    (let loop ((a filters))
      (when (and (pair? a) (pair? (cdr a)))
        (let ((k (car a)) (v (cadr a)))
          (when (keyword? k)
            (let ((names (lambda () (let ((xs (seq->list v)))
                                      (map symbol-t-name (filter symbol-t? xs))))))
              (cond
                ((string=? (keyword-t-name k) "only")    (set! only (names)))
                ((string=? (keyword-t-name k) "exclude") (set! excl (names)))))))
        (loop (cddr a))))
    ;; the target's own bucket (rt.ss ns-cells-index): a refer walked EVERY
    ;; interned var in the image before, so each (:use ...)/(refer ...) cost
    ;; O(total vars) and a program load O(namespaces x total vars)
    (for-each
      (lambda (c)
        (when (var-cell-defined? c)
          (let ((nm (var-cell-name c)))
            (when (and (or (not only) (member nm only)) (not (member nm excl)))
              (chez-register-refer! cns nm target)))))
      (ns-cells-list target))
    jolt-nil))
;; (:refer-clojure :exclude [names…]) — clojure.core always resolves on Chez, so
;; the only thing to track is the EXCLUDE set: an excluded name is not
;; clojure.core/name, so syntax-quote qualifies it to the current ns instead (a ns
;; that excludes and defines its own, e.g. core.logic.fd's ==).
(define ns-core-exclude-table (make-hashtable equal-hash equal?))  ; cns -> (name -> #t)
(define (chez-register-core-exclude! cns name)
  (jolt-with-mutex ns-map-mu
   (let ((h (or (hashtable-ref ns-core-exclude-table cns #f)
                (let ((h (make-hashtable string-hash string=?)))
                  (hashtable-set! ns-core-exclude-table cns h) h))))
     (hashtable-set! h name #t))))
(define (chez-core-excluded? cns name)
  (let ((h (hashtable-ref ns-core-exclude-table cns #f)))
    (and h (hashtable-ref h name #f) #t)))
;; refer-clojure is a MACRO here (marked below) whose expander is this fn, so
;; args arrive UNEVALUATED: a top-level (:exclude [names]) is raw, while the ns
;; macro emits quoted args ((quote :exclude) (quote [names])) — the JVM shape,
;; where the macro splices them into a (refer ...) call and the quotes evaluate.
;; Unwrapping one quote layer makes both spellings mean the same exclusion.
(define (rc-unquote x)
  (if (cseq? x)
      (let ((items (seq->list x)))
        (if (and (pair? items) (symbol-t? (car items))
                 (string=? (symbol-t-name (car items)) "quote") (pair? (cdr items)))
            (cadr items) x))
      x))
;; The expander must NOT register at expansion time: that would record the
;; exclusion under the ns in effect during ANALYSIS, but an ns form's in-ns only
;; switches chez-current-ns when the expansion EVALUATES. Emit a runtime call
;; instead; it runs after in-ns, so the exclusion lands in the ns the form
;; creates and is in place before the loader reads the next form.
(define (jolt-refer-clojure . args)
  (let loop ((a args) (names '()))
    (cond
     ((or (null? a) (null? (cdr a)))
      (jolt-list (jolt-symbol "clojure.core" "refer-clojure-register!")
                 (jolt-list (jolt-symbol #f "quote") (apply jolt-list (reverse names)))))
     ((let ((k (rc-unquote (car a))) (v (rc-unquote (cadr a))))
        (and (keyword? k) (string=? (keyword-t-name k) "exclude") v))
      => (lambda (v)
           (loop (cddr a) (append (filter symbol-t? (seq->list v)) names))))
     (else (loop (cddr a) names)))))
(define (jolt-refer-clojure-register! names)
  (let ((cns (chez-current-ns)))
    (for-each (lambda (n) (when (symbol-t? n)
                            (chez-register-core-exclude! cns (symbol-t-name n))))
              (seq->list names)))
  jolt-nil)

;; alter-meta! / reset-meta!: a var's metadata lives in the cell's meta field;
;; any other reference (atom/agent/namespace) uses the identity meta side-table
;; jolt-meta reads. A truthy :macro in the new meta marks the var as a macro
;; (JVM parity: Var.isMacro reads meta), so re-export idioms that copy a macro's
;; meta onto a fresh var — (alter-meta! v merge (meta macro-var)) — work. Marking
;; is one-way: meta without :macro does not demote an existing macro, since
;; defmacro vars derive :macro rather than storing it.
(define (var-meta-sync-macro! cell m)
  (when (jolt-truthy? (jolt-get m jolt-kw-var-macro))
    (var-cell-macro?-set! cell #t)))
(define (jolt-alter-meta! ref f . args)
  (if (var-cell? ref)
      (let* ((cur (or (var-cell-meta ref) (jolt-hash-map)))
             (new (apply jolt-invoke f cur args)))
        (var-cell-meta-set! ref new)
        (var-meta-sync-macro! ref new)
        new)
      (let* ((cur (let ((m (jolt-meta ref))) (if (jolt-nil? m) (jolt-hash-map) m)))
             (new (apply jolt-invoke f cur args)))
        (meta-table-set! ref new)
        new)))
(define (jolt-reset-meta! ref m)
  (if (var-cell? ref)
      (begin
        (var-cell-meta-set! ref m)
        (var-meta-sync-macro! ref m))
      (meta-table-set! ref m))
  m)

;; --- RESOLVE FRICTION: native-op cells -------------------------------------
;; Native-op primitives (+ map reduce …) are INLINED at emit, so they have no
;; var-cell and (resolve '+) would be nil — diverging from Clojure where it is a
;; var. def-var! each to its value-position procedure so it has a real, defined
;; cell (calls still inline, so no perf hit; #'+ deref and ((resolve '+) 1 2) also
;; work now). The clojure.core prelude, loaded AFTER rt.ss, overwrites the cells
;; for names it also defines in the overlay (map/filter/…); the purely-inlined
;; scalars (+/-/</inc/…) keep these.
(for-each
  (lambda (p) (def-var! "clojure.core" (car p) (cdr p)))
  (list
    (cons "+" jolt-add) (cons "-" jolt-sub) (cons "*" jolt-mul) (cons "/" jolt-div)
    (cons "<" <) (cons ">" >) (cons "<=" <=) (cons ">=" >=)
    (cons "=" jolt=) (cons "inc" jolt-inc) (cons "dec" jolt-dec) (cons "not" jolt-not)
    (cons "min" min) (cons "max" max)
    (cons "mod" modulo) (cons "rem" remainder) (cons "quot" quotient)
    (cons "vector" jolt-vector) (cons "hash-map" jolt-hash-map) (cons "hash-set" jolt-hash-set)
    (cons "conj" jolt-conj) (cons "get" jolt-get) (cons "nth" jolt-nth) (cons "count" jolt-count)
    (cons "assoc" jolt-assoc) (cons "dissoc" jolt-dissoc) (cons "contains?" jolt-contains?)
    (cons "empty?" jolt-empty?) (cons "peek" jolt-peek) (cons "pop" jolt-pop)
    (cons "first" jolt-first) (cons "rest" jolt-rest) (cons "next" jolt-next) (cons "seq" jolt-seq)
    (cons "cons" jolt-cons) (cons "list" jolt-list) (cons "reverse" jolt-reverse) (cons "last" jolt-last)
    (cons "map" jolt-map) (cons "filter" jolt-filter) (cons "remove" jolt-remove)
    (cons "reduce" jolt-reduce) (cons "into" jolt-into) (cons "concat" jolt-concat) (cons "apply" jolt-apply)
    (cons "range" jolt-range) (cons "take" jolt-take) (cons "drop" jolt-drop)
    (cons "iterate" jolt-iterate)
    (cons "keys" jolt-keys) (cons "vals" jolt-vals)
    (cons "even?" jolt-even?) (cons "odd?" jolt-odd?) (cons "pos?" jolt-pos?) (cons "neg?" jolt-neg?)
    (cons "zero?" jolt-zero?) (cons "identity" jolt-identity)
    (cons "ex-info" jolt-ex-info)))

;; --- bindings + *ns* --------------------------------------------------------
(def-var! "clojure.core" "find-ns" jolt-find-ns)
(def-var! "clojure.core" "the-ns" jolt-the-ns)
(def-var! "clojure.core" "create-ns" jolt-create-ns)
(def-var! "clojure.core" "in-ns" jolt-in-ns)
(def-var! "clojure.core" "all-ns" jolt-all-ns)
(def-var! "clojure.core" "ns-publics" jolt-ns-publics)
(def-var! "clojure.core" "ns-map" jolt-ns-map)
(def-var! "clojure.core" "ns-interns" jolt-ns-interns)
(def-var! "clojure.core" "ns-aliases" jolt-ns-aliases)
(def-var! "clojure.core" "ns-refers" jolt-ns-refers)
(def-var! "clojure.core" "ns-imports" jolt-ns-imports)
(def-var! "clojure.core" "resolve" jolt-resolve)
(def-var! "clojure.core" "ns-resolve" jolt-ns-resolve)
(def-var! "clojure.core" "find-var" jolt-find-var)
(def-var! "clojure.core" "ns-unmap" jolt-ns-unmap)
(def-var! "clojure.core" "remove-ns" jolt-remove-ns)
(def-var! "clojure.core" "intern" jolt-intern)
(def-var! "clojure.core" "alias" jolt-alias)
(def-var! "clojure.core" "ns-unalias" jolt-ns-unalias)
(def-var! "clojure.core" "refer" jolt-refer)
(def-var! "clojure.core" "refer-clojure" jolt-refer-clojure)
(mark-macro! "clojure.core" "refer-clojure")
;; Runtime half of the refer-clojure macro: the expansion calls this AFTER the
;; enclosing ns form's in-ns has switched chez-current-ns (see jolt-refer-clojure).
(def-var! "clojure.core" "refer-clojure-register!" jolt-refer-clojure-register!)
;; defmacro — special form; the var cell exists so (resolve 'defmacro) works.
;; The expander re-emits the form (the special-form path handles analysis).
(def-var! "clojure.core" "defmacro"
  (lambda args (apply jolt-list (cons (jolt-symbol #f "defmacro") (list->cseq args)))))
(mark-macro! "clojure.core" "defmacro")
(def-var! "clojure.core" "alter-meta!" jolt-alter-meta!)
(def-var! "clojure.core" "reset-meta!" jolt-reset-meta!)
;; *ns* starts at the user namespace (the current ns for -e user code). in-ns
;; re-binds it. (ns-name is overridden natively in post-prelude.ss.)
(def-dynvar! "clojure.core" "*ns*" (intern-ns! "user"))

;; Host seam: bare var-cell lookup (no alias resolution) for the defonce macro
;; expansion, which must NOT reference clojure.core/resolve (a tree-shake bail ref).
;; Returns the var cell only when it holds a real root value: a declared cell is
;; "defined" (interned/resolvable) but its root is the unbound sentinel — and in a
;; top-level (do (defonce x 1) …) the analyzer interns x before the form runs, so
;; the bound check is what makes the first defonce actually def.
(def-var! "jolt.host" "find-var"
  (lambda (ns name)
    (let ((c (var-cell-lookup ns name)))
      (if (and c (var-cell-defined? c)
               (not (jolt-var-unbound? (var-cell-root c))))
          c jolt-nil))))

;; --- printer patches: a namespace renders as its name (str / pr-str / -e) ----
(register-pr-arm! jns? jns-name)
(register-str-render! jns? jns-name)
