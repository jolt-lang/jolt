;; host-contract.ss (jolt-hs9n, Phase 3 inc6) — the jolt.host contract on Chez.
;;
;; The portable seam between jolt-core (analyzer/IR/emitter, cross-compiled to
;; Scheme) and the host. Mirrors src/jolt/host_iface.janet's `exports`: every
;; contract fn is def-var!'d into the "jolt.host" namespace so the cross-compiled
;; jolt.analyzer / jolt.backend-scheme — whose unqualified form-*/resolve-global/
;; ... refs lower to (var-deref "jolt.host" ...) — resolve here at runtime.
;;
;; This is what puts analyze->IR->emit ON CHEZ (the zero-Janet spine). It runs
;; over the Chez data reader's forms (reader.ss): symbols are symbol-t, lists are
;; cseq (list?), () is empty-list-t, vectors/maps are pvec/pmap, sets and #tag/
;; regex/inst/uuid are pmaps tagged :jolt/type, chars are NATIVE Chez chars.
;;
;; Loaded after rt.ss + reader.ss + the core prelude; before the compiler image.

;; --- the analyze ctx --------------------------------------------------------
;; ctx is opaque to the analyzer (only ever threaded to these contract fns); we
;; make it a box carrying the compile namespace. The var/ns registry it consults
;; is the global var-table (rt.ss).
(define-record-type chez-actx (fields (mutable cns)) (nongenerative chez-actx-v1))
(define (make-analyze-ctx ns) (make-chez-actx ns))

;; Interned keywords reused for form tags + resolve-global's result map.
(define hc-kw-jolt-type (keyword "jolt" "type"))
(define hc-kw-jolt-set  (keyword "jolt" "set"))
(define hc-kw-jolt-tagged (keyword "jolt" "tagged"))
(define hc-kw-value (keyword #f "value"))
(define hc-kw-tag   (keyword #f "tag"))
(define hc-kw-form  (keyword #f "form"))
(define hc-kw-kind  (keyword #f "kind"))
(define hc-kw-ns    (keyword #f "ns"))
(define hc-kw-name  (keyword #f "name"))
(define hc-kw-var   (keyword #f "var"))
(define hc-kw-unresolved (keyword #f "unresolved"))
(define hc-kw-regex (keyword #f "regex"))
(define hc-kw-inst  (keyword #f "#inst"))
(define hc-kw-uuid  (keyword #f "#uuid"))

;; --- form predicates --------------------------------------------------------
(define (hc-sym? x) (symbol-t? x))
(define (hc-list? x) (or (empty-list-t? x) (and (cseq? x) (cseq-list? x))))
(define (hc-vec? x) (pvec? x))
(define (hc-map? x) (and (pmap? x) (jolt-nil? (jolt-get x hc-kw-jolt-type))))
(define (hc-set? x) (and (pmap? x) (eq? (jolt-get x hc-kw-jolt-type) hc-kw-jolt-set)))
(define (hc-char? x) (char? x))
(define (hc-literal? x)
  (or (jolt-nil? x) (boolean? x) (number? x) (string? x) (keyword-t? x) (char? x)))

(define (hc-tagged-of x tag)
  (and (pmap? x)
       (eq? (jolt-get x hc-kw-jolt-type) hc-kw-jolt-tagged)
       (eq? (jolt-get x hc-kw-tag) tag)))
(define (hc-regex? x) (hc-tagged-of x hc-kw-regex))
(define (hc-inst? x) (hc-tagged-of x hc-kw-inst))
(define (hc-uuid? x) (hc-tagged-of x hc-kw-uuid))

;; --- form accessors ---------------------------------------------------------
(define (hc-sym-name x) (symbol-t-name x))
;; The reader stores an unqualified symbol's ns inconsistently (#f, '(), or
;; jolt-nil — see converters.ss). The contract is jolt-nil for unqualified (the
;; analyzer tests (nil? ns)), so normalize; a real ns string passes through.
(define (hc-sym-ns x)
  (let ((ns (symbol-t-ns x)))
    (if (and ns (not (jolt-nil? ns)) (not (null? ns))) ns jolt-nil)))
(define (hc-sym-meta x)
  (let ((m (symbol-t-meta x)))
    (if (and m (not (jolt-nil? m)) (not (null? m))) m jolt-nil)))

;; list items -> jolt vector (pvec); the analyzer mapv's over the result.
(define (hc-elements x)
  (cond ((empty-list-t? x) empty-pvec)
        ((cseq? x) (make-pvec (list->vector (seq->list x))))
        (else empty-pvec)))
(define (hc-vec-items x) x)                 ; already a pvec
(define (hc-set-items x) (jolt-get x hc-kw-value))
(define (hc-map-pairs x)
  (let loop ((ks (if (jolt-nil? (jolt-seq (jolt-keys x))) '()
                     (seq->list (jolt-seq (jolt-keys x))))) (acc '()))
    (if (null? ks) (apply jolt-vector (reverse acc))
        (loop (cdr ks) (cons (jolt-vector (car ks) (jolt-get x (car ks))) acc)))))
(define (hc-regex-source x) (jolt-get x hc-kw-form))
(define (hc-inst-source x) (jolt-get x hc-kw-form))
(define (hc-uuid-source x) (jolt-get x hc-kw-form))

;; The Chez reader does not record source offsets yet (jolt-q2kg).
(define (hc-form-position x) jolt-nil)

;; --- special forms ----------------------------------------------------------
;; Mirrors host_iface special-names + interop-head? — forms the analyzer marks
;; uncompilable (the handled specials are dispatched in analyze-list BEFORE this).
(define hc-special-names
  '("quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
    "defmacro" "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "eval" "new"
    "." "gen-class" "monitor-enter" "monitor-exit" "letfn"))
(define (hc-interop-head? name)
  (let ((n (string-length name)))
    (and (> n 1)
         (or (char=? (string-ref name 0) #\.)
             (char=? (string-ref name (- n 1)) #\.)))))
(define (hc-special? name)
  (if (or (member name hc-special-names) (hc-interop-head? name)) #t #f))

;; --- compile-time environment -----------------------------------------------
(define (hc-current-ns ctx) (chez-actx-cns ctx))
(define (hc-late-bind? ctx) #t)            ; Chez has no interpreter to punt to

;; Runtime macros land in inc6b (jolt-r8ku): no macro is emitted as a runtime
;; value yet, so nothing is a macro. form-macro? is only ever asked about a
;; non-handled, non-special head (e.g. +, a user fn) — all non-macros today.
(define (hc-macro? ctx sym) #f)
(define (hc-expand-1 ctx form)
  (jolt-throw (jolt-ex-info "form-expand-1: runtime macros not on Chez yet (jolt-r8ku)"
                            (jolt-hash-map))))

;; Classify a global (non-local) symbol reference against the var registry:
;;   {:kind :var :ns NS :name NAME}   — a defined var (compile ns / clojure.core)
;;   {:kind :unresolved :name NAME}   — not found (late-bind -> var-ref @ compile ns;
;;                                      a qualified one -> host-static in the analyzer)
;; No :host branch: there is no Janet-style native-op env on Chez — the hot
;; clojure.core primitives (+,-,map,...) are declared in clojure.core below so
;; they classify as :var and the emitter's native-op path lowers them.
(define (hc-resolve-global ctx sym)
  (let* ((nm (symbol-t-name sym))
         (sns (symbol-t-ns sym))
         (qualified (and (not (jolt-nil? sns)) sns))
         (cell (if qualified
                   (var-cell-lookup qualified nm)
                   (or (var-cell-lookup (chez-actx-cns ctx) nm)
                       (var-cell-lookup "clojure.core" nm)))))
    (if (and cell (var-cell-defined? cell))
        (jolt-hash-map hc-kw-kind hc-kw-var
                       hc-kw-ns (var-cell-ns cell)
                       hc-kw-name (var-cell-name cell))
        (jolt-hash-map hc-kw-kind hc-kw-unresolved hc-kw-name nm))))

(define (hc-intern! ctx ns-name nm) (declare-var! ns-name nm) jolt-nil)

;; syntax-quote lowering + record hints land in later increments (6b+); stub so
;; the contract is complete (not on the macro-free spine).
(define (hc-syntax-quote-lower ctx inner)
  (jolt-throw (jolt-ex-info "form-syntax-quote-lower: not on Chez yet (jolt-r8ku)"
                            (jolt-hash-map))))
(define (hc-record-type? ctx name) #f)
(define (hc-record-ctor-key ctx name) jolt-nil)
(define (hc-record-shapes ctx) (jolt-hash-map))
(define (hc-inline-enabled? ctx) #f)
(define (hc-inline-ir ctx ns-name nm) jolt-nil)

;; --- declare the hot clojure.core primitives so resolve-global sees them ------
;; (mirrors backend_scheme.clj native-ops keys — the emitter lowers these inline,
;;  so the declared cell's unbound root is never deref'd.)
(for-each (lambda (nm) (declare-var! "clojure.core" nm))
  '("+" "-" "*" "/" "<" ">" "<=" ">=" "=" "inc" "dec" "not" "min" "max"
    "mod" "rem" "quot" "vector" "hash-map" "hash-set" "conj" "get" "nth" "count"
    "assoc" "dissoc" "contains?" "empty?" "peek" "pop" "first" "rest" "next" "seq"
    "cons" "list" "reverse" "last" "map" "filter" "remove" "reduce" "into" "concat"
    "apply" "range" "take" "drop" "keys" "vals" "even?" "odd?" "pos?" "neg?"
    "zero?" "identity" "ex-info"))

;; --- install: bind the contract into the jolt.host namespace -----------------
(define (hc-install!)
  (def-var! "jolt.host" "form-sym?" hc-sym?)
  (def-var! "jolt.host" "form-sym-name" hc-sym-name)
  (def-var! "jolt.host" "form-sym-ns" hc-sym-ns)
  (def-var! "jolt.host" "form-sym-meta" hc-sym-meta)
  (def-var! "jolt.host" "form-list?" hc-list?)
  (def-var! "jolt.host" "form-vec?" hc-vec?)
  (def-var! "jolt.host" "form-map?" hc-map?)
  (def-var! "jolt.host" "form-set?" hc-set?)
  (def-var! "jolt.host" "form-char?" hc-char?)
  (def-var! "jolt.host" "form-literal?" hc-literal?)
  (def-var! "jolt.host" "form-regex?" hc-regex?)
  (def-var! "jolt.host" "form-inst?" hc-inst?)
  (def-var! "jolt.host" "form-uuid?" hc-uuid?)
  (def-var! "jolt.host" "form-elements" hc-elements)
  (def-var! "jolt.host" "form-vec-items" hc-vec-items)
  (def-var! "jolt.host" "form-set-items" hc-set-items)
  (def-var! "jolt.host" "form-map-pairs" hc-map-pairs)
  (def-var! "jolt.host" "form-regex-source" hc-regex-source)
  (def-var! "jolt.host" "form-inst-source" hc-inst-source)
  (def-var! "jolt.host" "form-uuid-source" hc-uuid-source)
  (def-var! "jolt.host" "form-position" hc-form-position)
  (def-var! "jolt.host" "form-special?" hc-special?)
  (def-var! "jolt.host" "compile-ns" hc-current-ns)
  (def-var! "jolt.host" "late-bind?" hc-late-bind?)
  (def-var! "jolt.host" "form-macro?" hc-macro?)
  (def-var! "jolt.host" "form-expand-1" hc-expand-1)
  (def-var! "jolt.host" "resolve-global" hc-resolve-global)
  (def-var! "jolt.host" "host-intern!" hc-intern!)
  (def-var! "jolt.host" "form-syntax-quote-lower" hc-syntax-quote-lower)
  (def-var! "jolt.host" "record-type?" hc-record-type?)
  (def-var! "jolt.host" "record-ctor-key" hc-record-ctor-key)
  (def-var! "jolt.host" "record-shapes" hc-record-shapes)
  (def-var! "jolt.host" "inline-enabled?" hc-inline-enabled?)
  (def-var! "jolt.host" "inline-ir" hc-inline-ir))

(hc-install!)
