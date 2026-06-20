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
;; A set form is the reader's tagged map {:jolt/type :jolt/set :value <pvec>} OR a
;; real pset value — a macro template's #{...} expansion (syntax-quote.ss jolt-sqset)
;; produces a pset, which the analyzer must still read as a set literal (jolt-r9lm).
(define (hc-set? x)
  (or (pset? x)
      (and (pmap? x) (eq? (jolt-get x hc-kw-jolt-type) hc-kw-jolt-set))))
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
(define (hc-char-code x) (char->integer x))  ; native Chez char -> codepoint
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
(define (hc-set-items x)
  (if (pset? x)
      (apply jolt-vector (pset-fold x cons '()))
      (jolt-get x hc-kw-value)))
(define (hc-map-pairs x)
  (let ((kv (hashtable-ref rdr-map-order x #f)))
    (if kv
        ;; reader-built map literal: emit pairs in SOURCE order (kv = k1 v1 k2 v2 …)
        ;; so the analyzer evaluates the values left-to-right (jolt-qjr0).
        (let loop ((kv kv) (acc '()))
          (if (null? kv) (apply jolt-vector (reverse acc))
              (loop (cddr kv) (cons (jolt-vector (car kv) (cadr kv)) acc))))
        ;; a runtime/non-reader map: pmap iteration order
        (let loop ((ks (if (jolt-nil? (jolt-seq (jolt-keys x))) '()
                           (seq->list (jolt-seq (jolt-keys x))))) (acc '()))
          (if (null? ks) (apply jolt-vector (reverse acc))
              (loop (cdr ks) (cons (jolt-vector (car ks) (jolt-get x (car ks))) acc)))))))
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

;; Resolve a global symbol to its var cell against the compile ns then clojure.core
;; (a qualified ns wins). Shared by resolve-global / form-macro? / form-expand-1.
;; Normalizes the reader's unqualified-ns sentinel (#f / '() / jolt-nil) like
;; hc-sym-ns, so an unqualified symbol never looks up a bogus "#f" namespace.
(define (hc-resolve-cell ctx sym)
  (let* ((nm (symbol-t-name sym))
         (sns (symbol-t-ns sym))
         (qualified (and sns (not (jolt-nil? sns)) (not (null? sns)) sns)))
    (if qualified
        ;; a qualified ns may be a require :as alias (s/split -> clojure.string/split)
        (let ((target (or (chez-resolve-alias (chez-actx-cns ctx) qualified) qualified)))
          (var-cell-lookup target nm))
        (or (var-cell-lookup (chez-actx-cns ctx) nm)
            ;; a :refer'd name resolves to its source ns
            (let ((ref (chez-resolve-refer (chez-actx-cns ctx) nm)))
              (and ref (var-cell-lookup ref nm)))
            (var-cell-lookup "clojure.core" nm)))))

;; Runtime macros (jolt-r9lm, inc6b): a defmacro is emitted into the prelude as a
;; def-var! of its cross-compiled expander fn plus (mark-macro! ns name), so the
;; var cell is flagged a macro (rt.ss var-macro-table). form-macro? checks the
;; flag; form-expand-1 applies the expander to the unevaluated arg forms (the rest
;; of the list), and the analyzer re-analyzes the returned form. Mirrors
;; host_iface.janet h-macro?/h-expand-1.
(define (hc-macro? ctx sym)
  (macro-var? (hc-resolve-cell ctx sym)))
(define (hc-expand-1 ctx form)
  (let* ((items (seq->list form))
         (head (car items))
         (args (cdr items))
         (expander (var-cell-root (hc-resolve-cell ctx head))))
    (apply jolt-invoke expander args)))

;; Classify a global (non-local) symbol reference against the var registry:
;;   {:kind :var :ns NS :name NAME}   — a defined var (compile ns / clojure.core)
;;   {:kind :unresolved :name NAME}   — not found (late-bind -> var-ref @ compile ns;
;;                                      a qualified one -> host-static in the analyzer)
;; No :host branch: there is no Janet-style native-op env on Chez — the hot
;; clojure.core primitives (+,-,map,...) are declared in clojure.core below so
;; they classify as :var and the emitter's native-op path lowers them.
(define (hc-resolve-global ctx sym)
  (let* ((nm (symbol-t-name sym))
         (cell (hc-resolve-cell ctx sym)))
    (if (and cell (var-cell-defined? cell))
        (jolt-hash-map hc-kw-kind hc-kw-var
                       hc-kw-ns (var-cell-ns cell)
                       hc-kw-name (var-cell-name cell))
        (jolt-hash-map hc-kw-kind hc-kw-unresolved hc-kw-name nm))))

(define (hc-intern! ctx ns-name nm) (declare-var! ns-name nm) jolt-nil)

;; --- syntax-quote lowering (jolt-qjr0, inc7) ---------------------------------
;; Mirrors src/jolt/eval_base.janet syntax-quote-lower/sq-symbol. Lowers a `form
;; to CONSTRUCTION CODE — Chez reader forms calling __sqcat/__sqvec/__sqmap/
;; __sqset/__sq1 + quote — that the analyzer re-analyzes, so a backtick compiles
;; with zero runtime cost (read -> macroexpand -> compile). Symbols resolve to
;; clojure.core / the compile ns; a foo# auto-gensym is stable within one `.
(define hc-special-symbols
  '("quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
    "defmacro" "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "var" "eval"
    "new" "."))
(define (hc-special-symbol? nm) (and (member nm hc-special-symbols) #t))

(define hc-sq-gensym-counter 0)
(define (hc-sq-gensym base)
  (set! hc-sq-gensym-counter (+ hc-sq-gensym-counter 1))
  (jolt-symbol #f (string-append base "__" (number->string hc-sq-gensym-counter) "__auto")))

(define (hc-sym nm) (jolt-symbol #f nm))
;; is `x` a non-empty list FORM whose head is the unqualified symbol `nm`?
(define (hc-head-is? x nm)
  (and (cseq? x) (cseq-list? x)
       (let ((h (seq-first x)))
         (and (symbol-t? h) (jolt-nil? (hc-sym-ns h)) (string=? (symbol-t-name h) nm)))))
(define (hc-second x) (seq-first (jolt-seq (seq-more x))))

(define (hc-sq-symbol ctx form gsmap)
  (let ((sns (hc-sym-ns form)) (nm (symbol-t-name form)))
    (if (jolt-nil? sns)
        (cond
          ;; foo# -> a stable per-` auto-gensym
          ((and (> (string-length nm) 0)
                (char=? (string-ref nm (- (string-length nm) 1)) #\#))
           (or (hashtable-ref gsmap nm #f)
               (let ((g (hc-sq-gensym (substring nm 0 (- (string-length nm) 1)))))
                 (hashtable-set! gsmap nm g) g)))
          ((hc-special-symbol? nm) form)               ; special form: leave bare
          ((var-cell-lookup "clojure.core" nm) (jolt-symbol "clojure.core" nm))
          (else (jolt-symbol (chez-actx-cns ctx) nm)))  ; else: qualify to compile ns
        ;; qualified (a real ns or an alias): ns aliases aren't modeled on the Chez
        ;; data layer yet, so leave a qualified symbol as written (jolt-qjr0).
        form)))

(define (hc-sq-lower ctx form gsmap)
  (cond
    ((hc-head-is? form "unquote") (hc-second form))
    ((hc-head-is? form "unquote-splicing")
     (jolt-throw (jolt-ex-info "~@ used outside of a list or vector in syntax-quote"
                               (jolt-hash-map))))
    ((hc-literal? form) form)
    ((symbol-t? form) (jolt-list (hc-sym "quote") (hc-sq-symbol ctx form gsmap)))
    ((hc-list? form)
     (apply jolt-list (hc-sym "__sqcat")
            (map (lambda (it) (hc-sq-lower-part ctx it gsmap)) (seq->list form))))
    ((hc-vec? form)
     (apply jolt-list (hc-sym "__sqvec")
            (map (lambda (it) (hc-sq-lower-part ctx it gsmap)) (seq->list form))))
    ((hc-set? form)
     (apply jolt-list (hc-sym "__sqset")
            (map (lambda (it) (hc-sq-lower-part ctx it gsmap)) (seq->list (hc-set-items form)))))
    ((hc-map? form)
     (apply jolt-list (hc-sym "__sqmap")
            (let loop ((pairs (seq->list (hc-map-pairs form))) (acc '()))
              (if (null? pairs) (reverse acc)
                  (let ((p (seq->list (car pairs))))
                    (loop (cdr pairs)
                          (cons (hc-sq-lower ctx (cadr p) gsmap)
                                (cons (hc-sq-lower ctx (car p) gsmap) acc))))))))
    (else (jolt-list (hc-sym "quote") form))))            ; tagged (char/regex/...) etc.

;; a list/vector/set element: a ~@ splice passes through (its seq is spliced by
;; __sqcat), any other item is wrapped (__sq1 <lowered>) so __sqcat flattens it.
(define (hc-sq-lower-part ctx item gsmap)
  (if (hc-head-is? item "unquote-splicing")
      (hc-second item)
      (jolt-list (hc-sym "__sq1") (hc-sq-lower ctx item gsmap))))

(define (hc-syntax-quote-lower ctx inner)
  (hc-sq-lower ctx inner (make-hashtable string-hash string=?)))
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
  (def-var! "jolt.host" "form-char-code" hc-char-code)
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
