;; records + protocols — the deftype/defrecord + defprotocol/extend-type
;; subsystem.
;;
;; A record is a `jrec`: a type tag ("ns.Name") + an alist of (kw . val) in
;; declared field order. It is map?/coll?, equal to another jrec of the same tag
;; with equal fields (never equal to a plain map), and prints as #ns.Name{...}.
;; The collection dispatchers (jolt-get/count/keys/vals/seq/assoc/contains?/=/
;; hash/conj + the printers) are set!-extended with a jrec arm that delegates to
;; the original — the transients.ss pattern — so all record logic lives here and
;; the hot collection paths are untouched. (get r :jolt/deftype) returns the tag,
;; so the overlay record? predicate works unchanged.
;;
;; Loaded after collections/seq/values/converters/printing/transients/multimethods
;; (the dispatchers it wraps + chez-current-ns).

(define-record-type jrec (fields tag pairs) (nongenerative chez-jrec-v1))
(define jolt-deftype-kw (keyword "jolt" "deftype"))

(define (jrec-lookup r k d)
  (if (jolt=2 k jolt-deftype-kw)
      (jrec-tag r)
      (let loop ((ps (jrec-pairs r)))
        (cond ((null? ps) d)
              ((jolt=2 (caar ps) k) (cdar ps))
              (else (loop (cdr ps)))))))
(define (jrec-has? r k)
  (let loop ((ps (jrec-pairs r)))
    (cond ((null? ps) #f) ((jolt=2 (caar ps) k) #t) (else (loop (cdr ps))))))
;; mutate a deftype's mutable field in place: the pairs are runtime cons cells,
;; so set-cdr! updates the field. (set! field v) inside a method
;; lowers to this; returns v, as set! does.
(define (jolt-set-field! inst k v)
  (if (jrec? inst)
      (let loop ((ps (jrec-pairs inst)))
        (cond ((null? ps) (error #f "set! of an unknown field" k))
              ((jolt=2 (caar ps) k) (set-cdr! (car ps) v) v)
              (else (loop (cdr ps)))))
      (error #f "set! of a field on a non-record" inst)))
(define (jrec-replace pairs k v)        ; replace existing field (keep order) or append
  (let loop ((ps pairs) (acc '()) (hit #f))
    (cond ((null? ps) (reverse (if hit acc (cons (cons k v) acc))))
          ((jolt=2 (caar ps) k) (loop (cdr ps) (cons (cons k v) acc) #t))
          (else (loop (cdr ps) (cons (car ps) acc) hit)))))
(define (jrec=? a b)
  (and (string=? (jrec-tag a) (jrec-tag b))
       (= (length (jrec-pairs a)) (length (jrec-pairs b)))
       (let loop ((ps (jrec-pairs a)))
         (or (null? ps)
             (and (jrec-has? b (caar ps))
                  (jolt=2 (cdar ps) (jrec-lookup b (caar ps) jolt-nil))
                  (loop (cdr ps)))))))
(define (jrec-hash r)
  (fold-left (lambda (acc p) (+ acc (jolt-hash (car p)) (jolt-hash (cdr p))))
             (string-hash (jrec-tag r)) (jrec-pairs r)))
(define (jrec-pr r)                      ; #ns.Name{:k v, :k v}
  (string-append "#" (jrec-tag r) "{"
    (let loop ((ps (jrec-pairs r)) (first #t) (acc ""))
      (if (null? ps) acc
          (loop (cdr ps) #f
                (string-append acc (if first "" ", ")
                  (jolt-pr-readable (caar ps)) " " (jolt-pr-readable (cdar ps))))))
    "}"))

;; ---- extend the collection dispatchers with a jrec arm ----------------------
(register-eq-arm! (lambda (a b) (or (jrec? a) (jrec? b)))
                  (lambda (a b) (and (jrec? a) (jrec? b) (jrec=? a b))))
(register-hash-arm! jrec? jrec-hash)
;; get on a jrec: a real field reads raw (so a deftype method's own field bindings,
;; compiled to (get inst :field), never recurse); a NON-field key on a deftype that
;; implements clojure.lang.ILookup routes to its valAt (core.match's pattern types
;; compute ::tag in valAt), else the default.
(register-get-arm! jrec?
  (lambda (coll k d)
    (cond ((jrec-has? coll k) (jrec-lookup coll k d))
          ((find-method-any-protocol (jrec-tag coll) "valAt")
           => (lambda (m) (jolt-invoke m coll k d)))
          (else d))))
;; A jrec is a defrecord (map of fields) by default, BUT a deftype that
;; implements a clojure.lang collection interface carries the op as an inline
;; method — prefer that method, else fall back to the field/map behavior. (jrec-cl
;; finds the method; find-method-any-protocol / jolt-invoke resolve at call time.)
(define (jrec-cl coll name) (and (jrec? coll) (find-method-any-protocol (jrec-tag coll) name)))
(define %r-jolt-count jolt-count)
(set! jolt-count (lambda (coll)
  (cond ((jrec-cl coll "count") => (lambda (m) (jolt-invoke m coll)))
        ((jrec? coll) (length (jrec-pairs coll)))
        (else (%r-jolt-count coll)))))
(define %r-jolt-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k) (if (jrec? coll) (jrec-has? coll k) (%r-jolt-contains? coll k))))
(define %r-jolt-assoc1 jolt-assoc1)
(set! jolt-assoc1 (lambda (coll k v)
  (cond ((jrec-cl coll "assoc") => (lambda (m) (jolt-invoke m coll k v)))
        ((jrec? coll) (make-jrec (jrec-tag coll) (jrec-replace (jrec-pairs coll) k v)))
        (else (%r-jolt-assoc1 coll k v)))))
(define %r-jolt-keys jolt-keys)
(set! jolt-keys (lambda (m) (if (jrec? m) (list->cseq (map car (jrec-pairs m))) (%r-jolt-keys m))))
(define %r-jolt-vals jolt-vals)
(set! jolt-vals (lambda (m) (if (jrec? m) (list->cseq (map cdr (jrec-pairs m))) (%r-jolt-vals m))))
(define %r-jolt-seq jolt-seq)
(set! jolt-seq (lambda (x)
  (cond ((jrec-cl x "seq") => (lambda (m) (jolt-seq (jolt-invoke m x))))
        ((jrec? x) (list->cseq (map (lambda (p) (make-map-entry (car p) (cdr p))) (jrec-pairs x))))
        (else (%r-jolt-seq x)))))
(define %r-jolt-conj1 jolt-conj1)
(set! jolt-conj1 (lambda (coll x)
  (cond ((jrec-cl coll "cons") => (lambda (m) (jolt-invoke m coll x)))
        ((jrec? coll) (jolt-assoc1 coll (jolt-nth x 0) (jolt-nth x 1)))
        (else (%r-jolt-conj1 coll x)))))
(register-pr-arm! jrec? jrec-pr)

;; records are map? and coll? (Clojure: a record IS an associative map). The
;; predicates.ss vars hold a snapshot, so re-def-var! after extending. record? is
;; the overlay's (some? (get x :jolt/deftype)) — works for free since the get
;; override returns the tag for that key.
(define %r-jolt-map? jolt-map?)
(set! jolt-map? (lambda (x) (or (jrec? x) (%r-jolt-map? x))))
(def-var! "clojure.core" "map?" jolt-map?)
(def-var! "clojure.core" "coll?" (lambda (x) (or (jrec? x) (jolt-coll-pred? x))))

;; ---- protocol registry ------------------------------------------------------
;; type-tag -> (proto-name -> (method-name -> fn))
(define type-registry (make-hashtable string-hash string=?))
(define (register-protocol-method type-tag proto method fn)
  (let* ((ti (or (hashtable-ref type-registry type-tag #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry type-tag h) h)))
         (pi (or (hashtable-ref ti proto #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! ti proto h) h))))
    (hashtable-set! pi method fn)))
(define (find-protocol-method type-tag proto method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let ((pi (hashtable-ref ti proto #f))) (and pi (hashtable-ref pi method #f))))))
(define (find-method-any-protocol type-tag method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let loop ((protos (vector->list (hashtable-keys ti))))
              (and (pair? protos)
                   (let ((f (hashtable-ref (hashtable-ref ti (car protos) #f) method #f)))
                     (or f (loop (cdr protos)))))))))
(define (type-satisfies? type-tag proto)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (hashtable-ref ti proto #f) #t)))

;; host type-tag candidates for a non-record value (extend-protocol on builtins).
(define (value-host-tags obj)
  ;; numbers dispatch by actual type (a Double is NOT a Long): flonum -> Double,
  ;; exact ratio -> Ratio, exact integer -> Long.
  (cond ((flonum? obj) '("Double" "Float" "Number" "Object"))
        ((and (number? obj) (exact? obj) (not (integer? obj))) '("Ratio" "Number" "Object"))
        ((number? obj) '("Long" "Integer" "BigInteger" "BigInt" "Number" "Object"))
        ((string? obj) '("String" "CharSequence" "Object"))
        ((boolean? obj) '("Boolean" "Object"))
        ((keyword? obj) '("Keyword" "Named" "Object"))
        ((jolt-symbol? obj) '("Symbol" "Named" "Object"))
        ((pvec? obj) '("PersistentVector" "APersistentVector" "IPersistentVector" "IPersistentCollection"
                       "List" "java.util.List" "Sequential" "Collection" "Iterable" "java.lang.Iterable" "Object"))
        ((pmap? obj) '("PersistentArrayMap" "APersistentMap" "IPersistentMap" "Associative"
                       "Map" "java.util.Map" "Iterable" "java.lang.Iterable" "Object"))
        ((pset? obj) '("PersistentHashSet" "APersistentSet" "IPersistentSet" "Set" "java.util.Set" "Collection" "Iterable" "java.lang.Iterable" "Object"))
        ((or (cseq? obj) (empty-list-t? obj)) '("ASeq" "ISeq" "IPersistentCollection" "Sequential" "Collection" "Iterable" "java.lang.Iterable" "Object"))
        ;; java.net.URI jhost — extend-protocol java.net.URI (hiccup ToURI/ToStr).
        ((and (jhost? obj) (string=? (jhost-tag obj) "uri")) '("URI" "java.net.URI" "Object"))
        ;; host value types a library may extend a protocol to by class (data.json
        ;; extends JSONWriter to java.util.UUID / java.util.Date / java.math.BigDecimal).
        ((juuid? obj) '("UUID" "java.util.UUID" "Object"))
        ((jinst? obj) '("Date" "java.util.Date" "Timestamp" "java.sql.Timestamp" "Object"))
        ((jbigdec? obj) '("BigDecimal" "java.math.BigDecimal" "Number" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "instant")) '("Instant" "java.time.Instant" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "local-date")) '("LocalDate" "java.time.LocalDate" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "local-time")) '("LocalTime" "java.time.LocalTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "local-date-time")) '("LocalDateTime" "java.time.LocalDateTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "duration")) '("Duration" "java.time.Duration" "TemporalAmount" "java.time.temporal.TemporalAmount" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "period")) '("Period" "java.time.Period" "TemporalAmount" "java.time.temporal.TemporalAmount" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "month-enum")) '("Month" "java.time.Month" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "dow-enum")) '("DayOfWeek" "java.time.DayOfWeek" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "year")) '("Year" "java.time.Year" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "year-month")) '("YearMonth" "java.time.YearMonth" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "chrono-unit")) '("ChronoUnit" "java.time.temporal.ChronoUnit" "TemporalUnit" "java.time.temporal.TemporalUnit" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "chrono-field")) '("ChronoField" "java.time.temporal.ChronoField" "TemporalField" "java.time.temporal.TemporalField" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "zone-offset")) '("ZoneOffset" "java.time.ZoneOffset" "ZoneId" "java.time.ZoneId" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "zone-id")) '("ZoneId" "java.time.ZoneId" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "zoned-date-time")) '("ZonedDateTime" "java.time.ZonedDateTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "offset-date-time")) '("OffsetDateTime" "java.time.OffsetDateTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "offset-time")) '("OffsetTime" "java.time.OffsetTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "clock")) '("Clock" "java.time.Clock" "Object"))
        ;; java.sql.Date — a distinct class from java.util.Date so a protocol
        ;; extended to both (data.json's JSONWriter) routes a sql.Date to its impl.
        ((and (jhost? obj) (string=? (jhost-tag obj) "sql-date")) '("java.sql.Date" "Date" "java.util.Date" "Object"))
        ;; a bare procedure (fn) — extend-protocol to clojure.lang.{Fn,IFn,AFn}.
        ((procedure? obj) '("Fn" "IFn" "AFn" "Object"))
        ((jolt-nil? obj) '("nil"))
        (else '("Object"))))

(define (record-tag obj) (and (jrec? obj) (jrec-tag obj)))

;; ---- the native that handles the analyzer/overlay call ----------------------
;; make-deftype-ctor: (name-sym field-kws field-tags field-muts) -> ctor closure.
;; The tag is baked at definition time in the type's ns (chez-current-ns).
(define (make-deftype-ctor name-sym field-kws . _ignored)
  (let* ((tag (string-append (chez-current-ns) "." (symbol-t-name name-sym)))
         (kws (seq->list field-kws))
         (ctor (lambda args
                 (make-jrec tag (let loop ((ks kws) (as args) (acc '()))
                                  (if (null? ks) (reverse acc)
                                      (loop (cdr ks) (if (null? as) '() (cdr as))
                                            (cons (cons (car ks) (if (null? as) jolt-nil (car as))) acc))))))))
    ;; Register the ctor globally by simple class name (like StringBuilder) so
    ;; (Name. …) interop resolves ns-agnostically: a deftype used across files works
    ;; even when the runtime current ns is the caller's, not the defining ns
    ;; (host-new checks class-ctors-tbl before the current-ns var fallback).
    (register-class-ctor! (symbol-t-name name-sym) ctor)
    ctor))

;; make-protocol: a protocol value the overlay reads via (get p :name)/(get p :methods).
(define (make-protocol name-str methods)
  (jolt-hash-map (keyword #f "jolt/type") (keyword #f "jolt/protocol")
                 (keyword #f "name") (jolt-symbol jolt-nil name-str)
                 (keyword #f "methods") methods))

;; register-protocol-methods!: intentional no-op. Chez dispatches a protocol method
;; by the receiver's type tag at call time, so there is no method table to register;
;; this binding exists only because defprotocol-emitted code calls it.
(define (register-protocol-methods! proto-name method-names) jolt-nil)

;; register-method: extend-type/extend register an impl. Host type names keep a
;; bare canonical tag; record names qualify to the current ns.
(define host-type-set
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (n) (hashtable-set! h n #t))
              '("Long" "Integer" "Number" "Double" "Ratio" "BigInt" "BigInteger"
                "String" "CharSequence" "Boolean" "Character"
                "Keyword" "Symbol" "Named" "Object" "nil"
                "Fn" "IFn" "AFn" "URI"
                "PersistentVector" "APersistentVector" "IPersistentVector"
                "PersistentArrayMap" "APersistentMap" "IPersistentMap"
                "PersistentHashSet" "APersistentSet" "IPersistentSet"
                "ASeq" "ISeq" "IPersistentCollection" "Associative" "Sequential"
                "Map" "java.util.Map" "List" "java.util.List" "Set" "java.util.Set"
                "Collection" "java.util.Collection" "Iterable" "java.lang.Iterable"
                "UUID" "BigDecimal" "Date" "Timestamp" "Instant" "java.sql.Date"
                ;; java.time value types (extend-protocol Duration / ZonedDateTime / …)
                "Duration" "Period" "LocalDate" "LocalTime" "LocalDateTime"
                "ZonedDateTime" "OffsetDateTime" "OffsetTime" "ZoneId" "ZoneOffset"
                "Clock" "Year" "YearMonth" "Month" "DayOfWeek"
                "ChronoUnit" "ChronoField" "TemporalAmount" "TemporalUnit" "TemporalField"))
    h))
(define (strip-prefix s p)
  (let ((pl (string-length p)))
    (and (> (string-length s) pl) (string=? (substring s 0 pl) p) (substring s pl (string-length s)))))
(define (canonical-host-tag type-name)
  (let ((base (or (strip-prefix type-name "java.lang.")
                  (strip-prefix type-name "java.util.")
                  (strip-prefix type-name "java.net.")
                  (strip-prefix type-name "java.math.")
                  (strip-prefix type-name "java.time.")
                  (strip-prefix type-name "clojure.lang.")
                  type-name)))
    (and (hashtable-ref host-type-set base #f) base)))
;; An extend/extend-type/extend-protocol registration marks the tag as an
;; extender of the protocol (recorded inside type-registry so the per-case prune
;; restores it). deftype/defrecord inline impls go through register-inline-method
;; and skip the mark: the JVM compiles inline protocol methods into the class, so
;; extenders excludes them.
(define extend-mark "__jolt_extend__")
(define (mark-extend! tag proto-name)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (when ti (let ((pi (hashtable-ref ti proto-name #f)))
               (when pi (hashtable-set! pi extend-mark #t))))))
(define (register-method type-name proto-name method-name fn)
  (let* ((host (canonical-host-tag type-name))
         (tag (or host (string-append (chez-current-ns) "." type-name))))
    (register-protocol-method tag proto-name method-name fn)
    (mark-extend! tag proto-name)
    jolt-nil))

;; register-inline-method: a deftype/defrecord inline impl. Registers for dispatch
;; under the ns-qualified record tag but does NOT mark it as an extender.
(define (register-inline-method type-name proto-name method-name fn)
  (register-protocol-method (string-append (chez-current-ns) "." type-name) proto-name method-name fn)
  jolt-nil)
;; record that a deftype/defrecord implements a protocol even when it adds no
;; methods (a MARKER protocol, e.g. core.match's IPseudoPattern) — so
;; instance?/satisfies? on the protocol hold.
(define (register-inline-protocol! type-name proto-name)
  (let* ((tag (string-append (chez-current-ns) "." type-name))
         (ti (or (hashtable-ref type-registry tag #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry tag h) h))))
    (unless (hashtable-ref ti proto-name #f)
      (hashtable-set! ti proto-name (make-hashtable string-hash string=?))))
  jolt-nil)

;; protocol-dispatch: look up the impl by the value's type tag (record) or host
;; candidates, invoke it; reified objects carry instance-local methods.
(define (protocol-dispatch proto-name method-name obj rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ((and (jrec? obj) (find-protocol-method (jrec-tag obj) proto-name method-name))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ((reified-methods obj)
       => (lambda (rm) (let ((f (hashtable-ref rm method-name #f)))
                         (if f (apply jolt-invoke f obj rest)
                             ;; not implemented on the reify — fall back to the
                             ;; protocol's extended impls over the reify's host tags
                             ;; (e.g. an Object/default extension). malli reifies some
                             ;; protocols and relies on a protocol's default for the
                             ;; rest.
                             (let loop ((tags (value-host-tags obj)))
                               (cond ((null? tags) (error #f (string-append "No reified method " method-name)))
                                     ((find-protocol-method (car tags) proto-name method-name)
                                      => (lambda (g) (apply jolt-invoke g obj rest)))
                                     (else (loop (cdr tags)))))))))
      (else
       (let loop ((tags (value-host-tags obj)))
         (cond ((null? tags) (error #f (string-append "No method " method-name " in " proto-name)))
               ((find-protocol-method (car tags) proto-name method-name)
                => (lambda (f) (apply jolt-invoke f obj rest)))
               (else (loop (cdr tags)))))))))

;; dot-dispatch fallback used by emit for (.method record args): find the method
;; in ANY protocol the record's type implements.
;; java.util.Iterator over a jolt seqable: (.iterator coll) returns a jiterator
;; holding a mutable cursor over (seq coll); (.hasNext it)/(.next it) walk it.
;; hiccup/compiler's run! loop iterates collections this way.
(define-record-type jiterator (fields (mutable cur)) (nongenerative jolt-iterator-v1))
;; A Chez condition's message string (for Throwable .getMessage/.toString): the
;; &message text plus any &irritants, or display-condition output as a fallback.
(define (condition->message-string c)
  (if (message-condition? c)
      (let* ((m (condition-message c))
             (irr (if (irritants-condition? c) (condition-irritants c) '()))
             (append-irr (lambda ()
                           (let loop ((xs irr) (acc m))
                             (if (null? xs) acc
                                 (loop (cdr xs) (string-append acc " " (jolt-pr-str (car xs)))))))))
        ;; some Chez conditions (open-input-file etc.) carry a format-template
        ;; message ("failed for ~a: ~(~a~)") whose irritants fill the directives;
        ;; format it in. Fall back to appending the irritants if that fails.
        (if (and (string? m) (let scan ((i 0))
                               (cond ((>= i (string-length m)) #f)
                                     ((char=? (string-ref m i) #\~) #t)
                                     (else (scan (+ i 1))))))
            (guard (e (#t (append-irr))) (apply format m irr))
            (append-irr)))
      (with-output-to-string (lambda () (display-condition c)))))
;; expose a Chez condition's message to Clojure (ex-message returns nil for raw
;; host conditions): the nREPL eval handler surfaces it instead of an opaque
;; "#<compound condition>".
(def-var! "jolt.host" "condition-message"
  (lambda (c) (if (condition? c) (condition->message-string c) jolt-nil)))
(define (record-method-dispatch obj method-name rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ;; (.getClass x): universal Object method — the class token for any value
      ;; (jolt has no Class objects; the token is the canonical name string, on
      ;; which .getName/.getSimpleName work via the String method shim).
      ((and (string=? method-name "getClass") (not (jrec? obj)) (not (jreify? obj)))
       (jolt-class obj))
      ((and (jrec? obj) (find-method-any-protocol (jrec-tag obj) method-name))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ((reified-methods obj)
       => (lambda (rm) (let ((f (hashtable-ref rm method-name #f)))
                         (if f (apply jolt-invoke f obj rest) (error #f (string-append "No method " method-name))))))
      ;; java.lang.String interop: defined in natives-str.ss, loaded
      ;; after this file (free reference, resolved at call time).
      ((string? obj) (jolt-string-method method-name obj rest))
      ((jiterator? obj)
       (cond ((string=? method-name "hasNext") (not (jolt-nil? (jolt-seq (jiterator-cur obj)))))
             ((string=? method-name "next")
              (let ((s (jolt-seq (jiterator-cur obj))))
                (if (jolt-nil? s) (error #f "iterator exhausted")
                    (let ((v (jolt-first s))) (jiterator-cur-set! obj (jolt-rest s)) v))))
             (else (error #f (string-append "No method " method-name " on Iterator")))))
      ((string=? method-name "iterator") (make-jiterator (jolt-seq obj)))
      ;; clojure.lang.Keyword interop: a Keyword carries an interned `sym` field
      ;; (the symbol form, ns + name) plus the Named methods. honeysql/reitit read
      ;; (.sym k) on their :clj branch to recover the symbol without the colon.
      ((keyword-t? obj)
       (cond ((string=? method-name "sym")
              (jolt-symbol (keyword-t-ns obj) (keyword-t-name obj)))
             ((string=? method-name "getName") (keyword-t-name obj))
             ((string=? method-name "getNamespace") (or (keyword-t-ns obj) jolt-nil))
             ((string=? method-name "toString")
              (string-append ":" (if (keyword-t-ns obj) (string-append (keyword-t-ns obj) "/") "")
                             (keyword-t-name obj)))
             ((string=? method-name "hashCode") (keyword-t-khash obj))
             ((string=? method-name "equals") (and (pair? rest) (eq? obj (car rest))))
             (else (error #f (string-append "No method " method-name " on Keyword")))))
      ;; clojure.lang.Symbol interop: the Named methods + getName/getNamespace.
      ((symbol-t? obj)
       (cond ((string=? method-name "getName") (symbol-t-name obj))
             ((string=? method-name "getNamespace") (or (symbol-t-ns obj) jolt-nil))
             ((string=? method-name "toString")
              (string-append (if (symbol-t-ns obj) (string-append (symbol-t-ns obj) "/") "")
                             (symbol-t-name obj)))
             ((string=? method-name "equals") (and (pair? rest) (jolt=2 obj (car rest))))
             (else (error #f (string-append "No method " method-name " on Symbol")))))
      ;; clojure.lang.Namespace: name/getName yield the ns name as a Symbol (JVM:
      ;; Namespace.name is a Symbol). clojure.spec.alpha reads (.name *ns*).
      ((jns? obj)
       (cond ((or (string=? method-name "name") (string=? method-name "getName"))
              (jolt-symbol #f (jns-name obj)))
             ((string=? method-name "toString") (jns-name obj))
             (else (error #f (string-append "No method " method-name " on Namespace")))))
      ;; clojure.lang.Var: ns -> its Namespace, sym -> the simple-name Symbol.
      ;; clojure.spec.alpha's ->sym reads (.name (.ns v)) and (.sym v).
      ((var-cell? obj)
       (cond ((string=? method-name "ns") (intern-ns! (var-cell-ns obj)))
             ((or (string=? method-name "sym") (string=? method-name "name"))
              (jolt-symbol #f (var-cell-name obj)))
             ((string=? method-name "getName")
              (jolt-symbol (var-cell-ns obj) (var-cell-name obj)))
             ((string=? method-name "toString") (string-append "#'" (var-cell-ns obj) "/" (var-cell-name obj)))
             (else (error #f (string-append "No method " method-name " on Var")))))
      ;; java.lang.Throwable interop over a Chez condition. A jolt host error
      ;; (`error`/`assertion-violationf`) raises a Chez condition; Clojure code
      ;; that catches it as a Throwable reads (.getMessage e) / (.toString e).
      ((condition? obj)
       (cond ((or (string=? method-name "getMessage") (string=? method-name "getLocalizedMessage"))
              (condition->message-string obj))
             ((string=? method-name "toString") (condition->message-string obj))
             ((string=? method-name "getCause") jolt-nil)
             ;; java.sql.SQLException chaining — jolt errors don't chain (nil).
             ((string=? method-name "getNextException") jolt-nil)
             ((string=? method-name "getStackTrace") (jolt-vector))
             ((string=? method-name "printStackTrace") jolt-nil)
             (else (error #f (string-append "No method " method-name " on Throwable")))))
      ;; java.lang.Character interop: (.toString \+) -> "+", etc.
      ((char? obj)
       (cond ((string=? method-name "toString") (string obj))
             ((string=? method-name "charValue") obj)
             ((string=? method-name "hashCode") (char->integer obj))
             ((string=? method-name "equals") (and (pair? rest) (char? (car rest)) (char=? obj (car rest))))
             ((string=? method-name "compareTo")
              (let ((o (car rest))) (cond ((char<? obj o) -1) ((char>? obj o) 1) (else 0))))
             (else (error #f (string-append "No method " method-name " on char")))))
      ;; java.util.List .indexOf / .lastIndexOf over any seqable (vector / list /
      ;; seq) — -1 when absent, like the JVM (medley/index-of reads this).
      ((or (string=? method-name "indexOf") (string=? method-name "lastIndexOf"))
       (let ((target (car rest)) (last? (string=? method-name "lastIndexOf")))
         (let loop ((s (jolt-seq obj)) (i 0) (found -1))
           (cond ((jolt-nil? s) found)
                 ((jolt=2 (seq-first s) target)
                  (if last? (loop (jolt-seq (seq-more s)) (fx+ i 1) i) i))
                 (else (loop (jolt-seq (seq-more s)) (fx+ i 1) found))))))
      ;; universal Object methods on any remaining value (boolean, etc.).
      ((string=? method-name "toString") (jolt-str-render-one obj))
      ((string=? method-name "hashCode") (jolt-hash obj))
      ((string=? method-name "equals") (and (pair? rest) (if (jolt= obj (car rest)) #t #f)))
      (else (error #f (string-append "No method " method-name " for value: "
                                     (jolt-pr-str obj)))))))

;; reify: instance-local method table. obj is a jreify carrying a method ht +
;; the protocol short-names it implements (for satisfies?/instance?).
(define-record-type jreify (fields methods protos) (nongenerative chez-jreify-v1))
(define (reified-methods obj) (and (jreify? obj) (jreify-methods obj)))
(define (make-reified methods-map . proto-names)
  (let ((ht (make-hashtable string-hash string=?))
        (protos (if (and (pair? proto-names) (null? (cdr proto-names)) (jolt-coll-pred? (car proto-names)))
                    (seq->list (car proto-names)) proto-names)))
    (for-each (lambda (p) (hashtable-set! ht (if (keyword? p) (keyword-t-name p) p)
                                          (jolt-get methods-map p jolt-nil)))
              (seq->list (jolt-keys methods-map)))
    (make-jreify ht (map (lambda (p) (if (symbol-t? p) (symbol-t-name p) p)) protos))))

;; satisfies?: does obj's type implement the protocol?
(define (jolt-satisfies? proto obj)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn)))
    (cond
      ((jrec? obj) (type-satisfies? (jrec-tag obj) pn-str))
      ((jreify? obj)
       (let ((short (last-dot pn-str)))
         (and (memp (lambda (p) (string=? (last-dot p) short)) (jreify-protos obj)) #t)))
      (else (let loop ((tags (value-host-tags obj)))
              (cond ((null? tags) #f)
                    ((type-satisfies? (car tags) pn-str) #t)
                    (else (loop (cdr tags)))))))))
(define (last-dot s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s) ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s))) (else (loop (- i 1))))))
(define (memp pred lst) (cond ((null? lst) #f) ((pred (car lst)) lst) (else (memp pred (cdr lst)))))

;; extenders: type-tags that extend a protocol via extend/extend-type/extend-
;; protocol, as symbols (extends? reads this). Inline deftype/defrecord impls are
;; excluded — only tags carrying the extend mark count, matching the JVM.
(define (extenders proto)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn))
         (out '()))
    (vector-for-each
      (lambda (tag)
        (let ((ti (hashtable-ref type-registry tag #f)))
          (when ti (let ((pi (hashtable-ref ti pn-str #f)))
                     (when (and pi (hashtable-ref pi extend-mark #f))
                       (set! out (cons (jolt-symbol jolt-nil tag) out)))))))
      (hashtable-keys type-registry))
    (if (null? out) jolt-nil (list->cseq out))))

;; jolt exception values (ex-info + host-constructed throwables) are ex-info-shaped
;; maps tagged :jolt/type :jolt/ex-info; (class …)/instance? read the JVM class off
;; the optional :jolt/class key, defaulting to clojure.lang.ExceptionInfo.
(register-str-render! jrec?
  (lambda (v)
    (let ((f (find-protocol-method (jrec-tag v) "Object" "toString")))
      (if f (jolt-invoke f v)
          (let ((s (jrec-pr v))) (substring s 1 (string-length s)))))))

;; `type` lives in natives-meta.ss: it needs jolt-meta for the :type
;; override and a total value->taxonomy mapping, so it sits with meta — a record
;; yields (jolt-symbol #f (jrec-tag x)), the ns.Name class-name symbol.

(def-var! "clojure.core" "make-deftype-ctor" make-deftype-ctor)
(def-var! "clojure.core" "make-protocol" make-protocol)
(def-var! "clojure.core" "register-protocol-methods!" register-protocol-methods!)
(def-var! "clojure.core" "register-method" register-method)
(def-var! "clojure.core" "register-inline-method" register-inline-method)
(def-var! "clojure.core" "register-inline-protocol!" register-inline-protocol!)
(def-var! "jolt.host" "set-field!" jolt-set-field!)
(def-var! "clojure.core" "protocol-dispatch" (lambda (pn mn obj rest) (protocol-dispatch pn mn obj rest)))
(def-var! "clojure.core" "satisfies?" jolt-satisfies?)
(def-var! "clojure.core" "extenders" extenders)
(def-var! "clojure.core" "make-reified" (lambda (mm . rest) (apply make-reified mm rest)))
(def-var! "clojure.core" "record-method-dispatch" (lambda (obj m rest) (record-method-dispatch obj m rest)))
