;; host-contract.ss — the jolt.host contract on Chez.
;;
;; The portable seam between jolt-core (analyzer/IR/emitter, cross-compiled to
;; Scheme) and the host. Every
;; contract fn is def-var!'d into the "jolt.host" namespace so the cross-compiled
;; jolt.analyzer / jolt.backend-scheme — whose unqualified form-*/resolve-global/
;; ... refs lower to (var-deref "jolt.host" ...) — resolve here at runtime.
;;
;; This is what puts analyze->IR->emit ON CHEZ. It runs
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
(define hc-kw-class (keyword #f "class"))
(define hc-kw-num-ret (keyword #f "num-ret"))
(define hc-kw-double (keyword #f "double"))
(define hc-kw-long (keyword #f "long"))
(define hc-kw-inst  (keyword #f "#inst"))
(define hc-kw-uuid  (keyword #f "#uuid"))
(define hc-kw-bigdec (keyword #f "bigdec"))

;; --- form predicates --------------------------------------------------------
(define (hc-sym? x) (symbol-t? x))
;; ANY non-empty seq is a list form for analysis (a macro/eval form built via
;; concat/map/cons is a lazy cseq with list?=#f, but evaluating it still means
;; calling its head) — not just reader-built lists.
;; a lazy seq is a list form too: a macro that builds its expansion with map/for
;; (now a LazySeq, not an eager cseq) and splices it must still analyze.
(define (hc-list? x) (or (empty-list-t? x) (cseq? x) (jolt-lazyseq? x)))
(define (hc-vec? x) (pvec? x))
(define (hc-map? x) (and (pmap? x) (jolt-nil? (jolt-get x hc-kw-jolt-type))))
;; A set form is the reader's tagged map {:jolt/type :jolt/set :value <pvec>} OR a
;; real pset value — a macro template's #{...} expansion (syntax-quote.ss jolt-sqset)
;; produces a pset, which the analyzer must still read as a set literal.
(define (hc-set? x)
  (or (pset? x)
      (and (pmap? x) (eq? (jolt-get x hc-kw-jolt-type) hc-kw-jolt-set))))
(define (hc-char? x) (char? x))
(define (hc-keyword? x) (keyword? x))
(define (hc-literal? x)
  (or (jolt-nil? x) (boolean? x) (number? x) (string? x) (keyword-t? x) (char? x)))

(define (hc-tagged-of x tag)
  (and (pmap? x)
       (eq? (jolt-get x hc-kw-jolt-type) hc-kw-jolt-tagged)
       (eq? (jolt-get x hc-kw-tag) tag)))
(define (hc-regex? x) (regex-t? x))   ; #"..." reads as a regex VALUE now
(define (hc-inst? x) (hc-tagged-of x hc-kw-inst))
(define (hc-uuid? x) (hc-tagged-of x hc-kw-uuid))
(define (hc-bigdec? x) (hc-tagged-of x hc-kw-bigdec))
(define (hc-bigdec-source x) (jolt-get x hc-kw-form))
;; A live namespace value spliced into a form (e.g. `(str ~*ns*) in a macro):
;; the analyzer can't carry an opaque runtime value, so recognize a jns and
;; reconstruct it by name at the call site.
(define (hc-ns-value? x) (jns? x))
(define (hc-ns-value-name x) (jns-name x))
;; a live Var value spliced into a form (a macro that does `(~v …)` with v a
;; resolved var) — the analyzer turns it into a :the-var reference by ns+name.
(define (hc-var-value? x) (var-cell? x))
(define (hc-var-value-ns x) (var-cell-ns x))
(define (hc-var-value-name x) (var-cell-name x))

;; *unchecked-math* read at compile time: when truthy (a file's (set!
;; *unchecked-math* …)), the analyzer rewrites +/-/*/inc/dec to their wrapping
;; unchecked-* forms for the rest of that file, like the JVM.
(define (hc-unchecked-math?)
  (jolt-truthy? (guard (e (#t #f)) (var-deref "clojure.core" "*unchecked-math*"))))

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
;; Metadata the reader attached to a collection literal (vec/map/set/list), or
;; jolt-nil. The analyzer re-emits a runtime (with-meta ..) for a meta-carrying
;; vector/map/set so the value keeps its metadata.
(define (hc-coll-meta x) (jolt-meta x))

;; list items -> jolt vector (pvec); the analyzer mapv's over the result.
(define (hc-elements x)
  (cond ((empty-list-t? x) empty-pvec)
        ((or (cseq? x) (jolt-lazyseq? x)) (make-pvec (list->vector (seq->list x))))
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
        ;; so the analyzer evaluates the values left-to-right.
        (let loop ((kv kv) (acc '()))
          (if (null? kv) (apply jolt-vector (reverse acc))
              (loop (cddr kv) (cons (jolt-vector (car kv) (cadr kv)) acc))))
        ;; a runtime/non-reader map: pmap iteration order
        (let loop ((ks (if (jolt-nil? (jolt-seq (jolt-keys x))) '()
                           (seq->list (jolt-seq (jolt-keys x))))) (acc '()))
          (if (null? ks) (apply jolt-vector (reverse acc))
              (loop (cdr ks) (cons (jolt-vector (car ks) (jolt-get x (car ks))) acc)))))))
(define (hc-regex-source x) (regex-t-source x))
(define (hc-inst-source x) (jolt-get x hc-kw-form))
(define (hc-uuid-source x) (jolt-get x hc-kw-form))

;; Source position for a list form: the reader stamps :line/:column (+ :file when
;; compiling a file) into the form's metadata. Return a clean {:line :column
;; :file?} map, or nil for a synthetic/macro-built form that carries none.
(define hc-kw-line   (keyword #f "line"))
(define hc-kw-column (keyword #f "column"))
(define hc-kw-file   (keyword #f "file"))
(define (hc-form-position x)
  (let ((m (jolt-meta x)))
    (if (and (pmap? m) (not (jolt-nil? (jolt-get m hc-kw-line))))
        (let ((line (jolt-get m hc-kw-line))
              (col  (jolt-get m hc-kw-column))
              (file (jolt-get m hc-kw-file)))
          (if (jolt-nil? file)
              (jolt-hash-map hc-kw-line line hc-kw-column col)
              (jolt-hash-map hc-kw-line line hc-kw-column col hc-kw-file file)))
        jolt-nil)))

;; --- special forms ----------------------------------------------------------
;; Mirrors host_iface special-names + interop-head? — forms the analyzer marks
;; uncompilable (the handled specials are dispatched in analyze-list BEFORE this).
;; `eval` is NOT here: it is a clojure.core FUNCTION on the spine (compile-eval.ss
;; def-var!s it), so it must resolve as an ordinary var, not punt.
;; `defmacro` stays special — the spine intercepts it before analysis.
(define hc-special-names
  '("quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
    "defmacro" "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "new"
    "." "gen-class" "monitor-enter" "monitor-exit" "letfn"))
(define (hc-interop-head? name)
  (let ((n (string-length name)))
    (and (> n 1)
         (not (string=? name ".."))   ; the .. threading macro, not an interop form
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
        (or (let ((c (var-cell-lookup (chez-actx-cns ctx) nm)))
              ;; an undefined forward-intern must not shadow a real referred
              ;; or clojure.core var — e.g. the compiler ns referencing `set`,
              ;; which late-binds (interns `jolt.backend-scheme/set` undefined)
              ;; and would otherwise hide clojure.core/set on the mint fixpoint.
              (and c (var-cell-defined? c) c))
            ;; a :refer'd name resolves to its source ns
            (let ((ref (chez-resolve-refer (chez-actx-cns ctx) nm)))
              (and ref (var-cell-lookup ref nm)))
            (var-cell-lookup "clojure.core" nm)))))

;; Runtime macros: a defmacro is emitted into the prelude as a
;; def-var! of its cross-compiled expander fn plus (mark-macro! ns name), so the
;; var cell is flagged a macro (rt.ss var-macro-table). form-macro? checks the
;; flag; form-expand-1 applies the expander to the unevaluated arg forms (the rest
;; of the list), and the analyzer re-analyzes the returned form.
(define (hc-macro? ctx sym)
  (macro-var? (hc-resolve-cell ctx sym)))
;; Clojure parity: a macro expansion inherits the call form's source position, so
;; errors/traces in macro-generated code point at the macro call site. Carry it
;; onto the top of a LIST expansion (code) that has none of its own — merged under
;; any meta the macro set, leaving collection literals (runtime data) alone. The
;; recursion through analyze re-expands inner macros, so each level's top form
;; picks up the position the same way (as the reference compiler does).
(define (hc-propagate-pos src dst)
  (if (and (cseq? dst) (cseq-list? dst))
      (let ((sp (hc-form-position src))
            (dm (jolt-meta dst)))
        (if (and (pmap? sp)
                 (or (jolt-nil? dm) (jolt-nil? (jolt-get dm hc-kw-line))))
            (jolt-with-meta dst
              (if (pmap? dm)
                  (pmap-fold-fwd sp (lambda (k v acc) (jolt-assoc1 acc k v)) dm)
                  sp))
            dst))
      dst))

;; A set literal reads as the tagged set-form {:jolt/type :jolt/set :value [...]}
;; for the analyzer, but a macro must see a real set value (Clojure parity, so
;; (set? arg) / seq / conj work — hiccup's compiler does this). Convert a set-form
;; argument to a set; elements stay as read (a deeply-nested set literal inside
;; another form is rarer and left for the analyzer).
(define (hc-macro-arg x)
  (if (rdr-set-form? x)
      (let ((items (jolt-get x rdr-kw-value)))
        (let loop ((i 0) (s empty-pset))
          (if (fx>=? i (pvec-count items)) s
              (loop (fx+ i 1) (pset-conj s (pvec-nth-d items i jolt-nil))))))
      x))
;; &form and &env are bound (as dynamic vars) around the expander call, so a
;; macro body can read the call form / lexical env without changing the calling
;; convention. The analyzer passes amp-env (the in-scope locals); macroexpand-1
;; has none, so it defaults to {}.
(define hc-amp-form-cell (declare-var! "clojure.core" "&form"))
(define hc-amp-env-cell (declare-var! "clojure.core" "&env"))
(define (hc-expand-1 ctx form . maybe-env)
  (let* ((items (seq->list form))
         (head (car items))
         (args (map hc-macro-arg (cdr items)))
         (expander (var-cell-root (hc-resolve-cell ctx head)))
         (amp-env (if (pair? maybe-env) (car maybe-env) (jolt-hash-map))))
    (dynamic-wind
      (lambda () (jolt-push-thread-bindings
                  (jolt-hash-map hc-amp-form-cell form hc-amp-env-cell amp-env)))
      (lambda () (hc-propagate-pos form (apply jolt-invoke expander args)))
      (lambda () (jolt-pop-thread-bindings)))))

;; Classify a global (non-local) symbol reference against the var registry:
;;   {:kind :var :ns NS :name NAME}   — a defined var (compile ns / clojure.core)
;;   {:kind :unresolved :name NAME}   — not found (late-bind -> var-ref @ compile ns;
;;                                      a qualified one -> host-static in the analyzer)
;; No :host branch: there is no separate native-op env — the hot
;; clojure.core primitives (+,-,map,...) are declared in clojure.core below so
;; they classify as :var and the emitter's native-op path lowers them.
;; A var's declared numeric return (^double/^long on its name) -> :double/:long,
;; read from its meta. Lets jolt.passes.numeric type a call to it.
(define (hc-cell-num-ret cell)
  (let ((m (and cell (hashtable-ref var-meta-table cell #f))))
    (and m (let* ((t (jolt-get m hc-kw-tag))   ; ^double/^long is a symbol; ^"double" a string
                  (s (cond ((symbol-t? t) (symbol-t-name t)) ((string? t) t) (else #f))))
             (cond ((equal? s "double") hc-kw-double)
                   ((equal? s "long") hc-kw-long)
                   (else #f))))))

;; A slash-free dotted symbol whose final segment is Capitalized is a class
;; reference (java.util.Map, clojure.lang.Named) — Clojure has no such vars. With
;; no JVM classes, jolt models a class as its name string, so the symbol
;; self-evaluates to that string (the analyzer emits a :const). This lets a lib
;; extend a protocol to / instance?-check a host class jolt has no shim for.
(define (hc-fq-class-name? nm)
  (let ((n (string-length nm)))
    (let loop ((i (fx- n 1)))
      (cond ((fx<? i 0) #f)
            ((char=? (string-ref nm i) #\.)
             (and (fx<? (fx+ i 1) n) (char-upper-case? (string-ref nm (fx+ i 1)))))
            (else (loop (fx- i 1)))))))

(define (hc-resolve-global ctx sym)
  (let* ((nm (symbol-t-name sym))
         (cell (hc-resolve-cell ctx sym)))
    (if (and cell (var-cell-defined? cell))
        (let ((base (jolt-hash-map hc-kw-kind hc-kw-var
                                   hc-kw-ns (var-cell-ns cell)
                                   hc-kw-name (var-cell-name cell)))
              (nr (hc-cell-num-ret cell)))
          (if nr (jolt-assoc base hc-kw-num-ret nr) base))
        (cond
          ;; java.util.Map / clojure.lang.Named — a dotted class name.
          ((hc-fq-class-name? nm) (jolt-hash-map hc-kw-kind hc-kw-class hc-kw-name nm))
          ;; a bare Capitalized name that names a registered host class — an
          ;; imported short name (`(:import [java.time ZonedDateTime])` then
          ;; `(. ZonedDateTime parse s)`). Only when otherwise unresolved, so a
          ;; same-named var still wins.
          ((and (fx>? (string-length nm) 0) (char-upper-case? (string-ref nm 0))
                (hashtable-ref class-statics-tbl nm #f))
           (jolt-hash-map hc-kw-kind hc-kw-class hc-kw-name nm))
          (else (jolt-hash-map hc-kw-kind hc-kw-unresolved hc-kw-name nm))))))

(define (hc-intern! ctx ns-name nm) (declare-var! ns-name nm) jolt-nil)

;; --- syntax-quote lowering ---------------------------------------------------
;; Lowers a `form
;; to CONSTRUCTION CODE — Chez reader forms calling __sqcat/__sqvec/__sqmap/
;; __sqset/__sq1 + quote — that the analyzer re-analyzes, so a backtick compiles
;; with zero runtime cost (read -> macroexpand -> compile). Symbols resolve to
;; clojure.core / the compile ns; a foo# auto-gensym is stable within one `.
(define hc-special-symbols
  '("quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
    "defmacro" "fn*" "let*" "loop*" "recur" "throw" "try" "set!" "var"
    "new" "."))
(define (hc-special-symbol? nm) (and (member nm hc-special-symbols) #t))

(define hc-sq-gensym-counter 0)
(define (hc-sq-gensym base)
  (set! hc-sq-gensym-counter (+ hc-sq-gensym-counter 1))
  (jolt-symbol #f (string-append base "__" (number->string hc-sq-gensym-counter) "__auto")))

(define (hc-sym nm) (jolt-symbol #f nm))
;; is `x` a non-empty list FORM whose head is the unqualified symbol `nm`?
;; Detect a (unquote …) / (unquote-splicing …) form in a syntax-quote template.
;; Any seq counts, not just a proper list: a macro that builds the template with
;; map/for (e.g. deftype's rewrite-set) yields a LAZY seq, and its ~unquotes must
;; still be recognized.
;; head symbol matches name nm, bare or clojure.core-qualified — the reader
;; produces clojure.core/unquote(-splicing) for ~/~@ (JVM parity), and this is
;; only used to spot those heads in syntax-quote templates.
(define (hc-head-is? x nm)
  (and (cseq? x)
       (let ((h (seq-first x)))
         (and (symbol-t? h) (string=? (symbol-t-name h) nm)
              (let ((ns (hc-sym-ns h)))
                (or (jolt-nil? ns) (and (string? ns) (string=? ns "clojure.core"))))))))
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
          ((hc-interop-head? nm) form)                 ; interop (.method / Class. / .-field): bare
          ;; a fully-qualified class name (java.util.Map, clojure.lang.ILookup) is
          ;; a class token, not a var to namespace-qualify — leave it bare, as
          ;; Clojure's syntax-quote resolves it to the class.
          ((hc-fq-class-name? nm) form)
          ;; the compile ns's OWN def shadows clojure.core — a name the ns
          ;; excluded and redefined (e.g. core.logic's `==` after
          ;; (:refer-clojure :exclude [==])), or any ns-local redefinition.
          ;; Referred names live in a separate table, so this only hits a real
          ;; local intern, matching how the analyzer resolves the bare symbol.
          ((var-cell-lookup (chez-actx-cns ctx) nm) (jolt-symbol (chez-actx-cns ctx) nm))
          ;; a name the compile ns excluded from clojure.core (:refer-clojure
          ;; :exclude) is not clojure.core/nm even before the ns defines its own —
          ;; qualify to the compile ns, like Clojure (core.logic.fd's `==`).
          ((chez-core-excluded? (chez-actx-cns ctx) nm) (jolt-symbol (chez-actx-cns ctx) nm))
          ((var-cell-lookup "clojure.core" nm) (jolt-symbol "clojure.core" nm))
          ;; a name referred into the compile ns (:require :refer / :use :only)
          ;; qualifies to its SOURCE ns, not the compile ns — so a macro that
          ;; syntax-quotes a referred var (e.g. clojure.tools.logging/spy using
          ;; clojure.pprint's pprint) expands to the real var.
          ((chez-resolve-refer (chez-actx-cns ctx) nm)
           => (lambda (target) (jolt-symbol target nm)))
          (else (jolt-symbol (chez-actx-cns ctx) nm)))  ; else: qualify to compile ns
        ;; qualified: if the ns part is an :as alias in the compile ns, resolve it
        ;; to the target namespace — Clojure resolves the alias part of a qualified
        ;; symbol in syntax-quote, so a macro's `impl/foo` expands to its real
        ;; (clojure.tools.logging.impl/foo) name and stays unambiguous even when
        ;; another loaded ns shares the alias's short name. Otherwise
        ;; leave it as written (a real ns or an interop class token).
        (let ((target (chez-resolve-alias (chez-actx-cns ctx) sns)))
          (if target (jolt-symbol target nm) form)))))

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
;; a ^Type param hint: name is the tag (a symbol, sometimes a string). Resolve it
;; against the record registry (records.ss) so the inference seeds the param as
;; that record — the open-world / cross-ns path where no caller type is inferred.
(define (hc-record-tag-name name)
  (cond ((symbol-t? name) (symbol-t-name name))
        ((string? name) name)
        (else #f)))
(define (hc-record-type? ctx name)
  (let ((nm (hc-record-tag-name name)))
    (if (and nm (chez-find-ctor-key nm (chez-current-ns))) #t #f)))
(define (hc-record-ctor-key ctx name)
  (let ((nm (hc-record-tag-name name)))
    (or (and nm (chez-find-ctor-key nm (chez-current-ns))) jolt-nil)))
;; The fully-qualified deftype tag ("ns.Name") IFF `class` names a deftype DEFINED
;; in the ctx's compile ns — the analyzer qualifies a bare (Name. …) to it, so a
;; deftype doesn't shadow a same-named built-in host class in an unrelated ns
;; (rewrite-clj imports java.io.PushbackReader; tools.reader defines its own). Strict:
;; only this ns's own def (the preferred shape key) counts, not the global
;; simple-name fallback, so a ns that merely uses the built-in resolves nil.
(define (hc-deftype-ctor-class ctx class)
  (let* ((nm (jolt-str-render-one class))
         (cns (hc-current-ns ctx))
         (key (string-append cns "/->" nm)))
    (if (hashtable-ref chez-record-shapes-tbl key #f)
        (string-append cns "." nm)
        jolt-nil)))
;; record + protocol-method shapes for the inference, from the runtime registries
;; (records.ss) populated as deftype/defprotocol forms load.
(define (hc-record-shapes ctx) (chez-record-shapes-map))
(define (hc-protocol-methods ctx) (chez-protocol-methods-map))
;; Optimization gate. On for --opt / :opt builds; off for release and dev.
;; Inference + inline + scalar-replace passes are gated on this.
(define hc-optimize? #f)
(define (set-optimize! on) (set! hc-optimize? on))
;; Inference gate. On for release builds too (inference without inline/scalar).
(define hc-release? #f)
(define (set-release! on) (set! hc-release? on))
(define (hc-inference-enabled? ctx) (or hc-optimize? hc-release?))
;; Inline additionally requires direct-link (closed-world guarantee).
(define hc-direct-link? #f)
(define (set-direct-link-flag! on) (set! hc-direct-link? on))
(define (hc-inline-enabled? ctx) (and hc-optimize? hc-direct-link?))
;; Inline-body registry: jolt.passes stashes an inline-eligible defn's
;; {:params :body :nhints :ret} here (keyed ns/name) as its form is optimized;
;; jolt.passes.inline fetches it to splice the body at a call site. The stash is an
;; opaque jolt value to the host — IR maps round-tripping through the table.
(define inline-stash-table (make-hashtable string-hash string=?))
(define (hc-stash-inline! ctx ns-name nm m)
  (hashtable-set! inline-stash-table (string-append ns-name "/" nm) m) jolt-nil)
(define (hc-inline-ir ctx ns-name nm)
  (or (hashtable-ref inline-stash-table (string-append ns-name "/" nm) #f) jolt-nil))

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
  (def-var! "jolt.host" "form-coll-meta" hc-coll-meta)
  (def-var! "jolt.host" "form-list?" hc-list?)
  (def-var! "jolt.host" "form-vec?" hc-vec?)
  (def-var! "jolt.host" "form-map?" hc-map?)
  (def-var! "jolt.host" "form-set?" hc-set?)
  (def-var! "jolt.host" "form-char?" hc-char?)
  (def-var! "jolt.host" "form-char-code" hc-char-code)
  (def-var! "jolt.host" "form-literal?" hc-literal?)
  (def-var! "jolt.host" "form-keyword?" hc-keyword?)
  (def-var! "jolt.host" "form-regex?" hc-regex?)
  (def-var! "jolt.host" "form-inst?" hc-inst?)
  (def-var! "jolt.host" "form-uuid?" hc-uuid?)
  (def-var! "jolt.host" "form-ns-value?" hc-ns-value?)
  (def-var! "jolt.host" "form-ns-value-name" hc-ns-value-name)
  (def-var! "jolt.host" "form-var-value?" hc-var-value?)
  (def-var! "jolt.host" "form-var-value-ns" hc-var-value-ns)
  (def-var! "jolt.host" "form-var-value-name" hc-var-value-name)
  (def-var! "jolt.host" "unchecked-math?" hc-unchecked-math?)
  (def-var! "jolt.host" "form-bigdec?" hc-bigdec?)
  (def-var! "jolt.host" "form-bigdec-source" hc-bigdec-source)
  (def-var! "jolt.host" "form-elements" hc-elements)
  (def-var! "jolt.host" "form-vec-items" hc-vec-items)
  (def-var! "jolt.host" "form-set-items" hc-set-items)
  (def-var! "jolt.host" "form-map-pairs" hc-map-pairs)
  (def-var! "jolt.host" "form-regex-source" hc-regex-source)
  (def-var! "jolt.host" "form-inst-source" hc-inst-source)
  (def-var! "jolt.host" "form-uuid-source" hc-uuid-source)
  (def-var! "jolt.host" "form-position" hc-form-position)
  ;; a number literal in CHEZ syntax for the backend's emitted source — jolt's
  ;; own str follows the reference printer (bigint N suffix, E exponents),
  ;; which Chez's reader rejects
  (def-var! "jolt.host" "chez-number-literal" (lambda (n) (number->string n)))
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
  (def-var! "jolt.host" "deftype-ctor-class" hc-deftype-ctor-class)
  (def-var! "jolt.host" "record-shapes" hc-record-shapes)
  (def-var! "jolt.host" "protocol-methods" hc-protocol-methods)
  (def-var! "jolt.host" "inline-enabled?" hc-inline-enabled?)
  (def-var! "jolt.host" "inference-enabled?" hc-inference-enabled?)
  (def-var! "jolt.host" "inline-ir" hc-inline-ir)
  (def-var! "jolt.host" "stash-inline!" hc-stash-inline!))

(hc-install!)
