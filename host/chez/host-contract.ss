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
(define hc-kw-private (keyword #f "private"))
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
;; ANY #tag form the reader built. The analyzer has a leaf for the four tags it
;; knows (#inst/#uuid/#bigdec, and #"regex" as a value); one that reaches the end
;; of the cond is a tag with no reader function, which is what the JVM's reader
;; raises on by name — better than "unsupported form", which names nothing.
(define (hc-tagged? x)
  (and (pmap? x) (eq? (jolt-get x hc-kw-jolt-type) hc-kw-jolt-tagged)))
;; the tag as written. The reader keys a source tag :#name / :#ns/name and its own
;; internal ones (:bigdec, :regex) bare, so strip a leading # if there is one.
(define (hc-tag-name x)
  (let* ((k (jolt-get x hc-kw-tag))
         (nm (if (keyword-t? k) (keyword-t-name k) (jolt-pr-str k))))
    (if (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\#))
        (substring nm 1 (string-length nm))
        nm)))
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
;; a live CLASS value spliced into a form by a macro — (resolve 'Throwable)
;; hands back the class, and `~(resolve sym)` embeds it in the expansion. It
;; re-evaluates through the jolt-class-for interner, the same value the class
;; symbol itself compiles to.
(define (hc-class-value? x) (jclass? x))
(define (hc-class-value-name x) (jclass-name x))

;; *unchecked-math* read at compile time: when truthy (a file's (set!
;; *unchecked-math* …)), the analyzer rewrites +/-/*/inc/dec to their wrapping
;; unchecked-* forms for the rest of that file, like the JVM.
(define (hc-unchecked-math?)
  (jolt-truthy? (guard (e (#t #f)) (var-deref "clojure.core" "*unchecked-math*"))))

;; *allow-unresolved-vars* read at compile time — clojure.core's var, the same one
;; RT.ALLOW_UNRESOLVED_VARS is on the JVM. Default false: an unqualified symbol
;; with no mapping is a compile error. Bound true, the analyzer emits a late-bound
;; var-ref in the compiling namespace instead, so a name that arrives in a later
;; eval still works (what nREPL binds it for). Only unqualified symbols consult it,
;; like Compiler.resolveIn's else branch.
(define (hc-allow-unresolved-vars?)
  (jolt-truthy? (guard (e (#t #f)) (var-deref "clojure.core" "*allow-unresolved-vars*"))))

;; jolt.host/*invoke-rewrite* read at compile time: a hook over calls through a
;; VAR. (f var-ns var-name form) answers a replacement form, or nil to leave the
;; call alone; the analyzer consults it at its invoke arm only for a head that
;; is a symbol, is not a local, and resolves to a var -- a local named `resolve`
;; is a local call whatever the hook thinks of clojure.core/resolve -- and after
;; macro expansion, so a macro-emitted (clojure.core/require ...) is seen like a
;; hand-written one. nil (the default) costs one deref per var call analyzed.
;; jolt.loader binds it while evaluating a context's source, so `require` and
;; its kin carry the loader that owns the code, whoever calls the fn later.
(define (hc-invoke-rewriter)
  (let ((f (guard (e (#t #f)) (var-deref "jolt.host" "*invoke-rewrite*"))))
    (if (or (not f) (jolt-nil? f)) #f f)))

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
  (let ((kv (rdr-map-order-ref x)))
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

;; A live bigdec / inst / uuid VALUE embedded in a form (read via read-string, or
;; produced by evaluating a literal and spliced into a form handed to eval) — as
;; opposed to the reader's tagged pmap the compile path sees. The analyzer emits it
;; as the same :bigdec/:inst/:uuid leaf, reconstructing from the value's canonical
;; string. Long/BigInt/Ratio already round-trip as plain number constants; these are
;; the host-object constants that had no VALUE path (only a tagged-form path).
;; jbigdec?/jinst?/juuid? and the string accessors resolve at call time (runtime).
(define (hc-bigdec-value? x) (jbigdec? x))
(define (hc-bigdec-value-source x) (jbigdec->string x))
(define (hc-inst-value? x) (jinst? x))
(define (hc-inst-value-source x) (inst-rfc3339 x))
(define (hc-uuid-value? x) (juuid? x))
(define (hc-uuid-value-source x) (juuid-s x))

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

;; Just the :line of a form's reader metadata, or nil — the non-allocating half of
;; hc-form-position, which builds a fresh position map every call. analyze-list
;; asks this of EVERY list form (to decide whether the form can name a location
;; while it descends), so it must not allocate; hc-form-position is then called
;; once, on the error path.
(define (hc-form-line x)
  (let ((m (jolt-meta x)))
    (if (pmap? m) (jolt-get m hc-kw-line) jolt-nil)))

;; --- special forms ----------------------------------------------------------
;; Mirrors host_iface special-names + interop-head? — forms the analyzer marks
;; uncompilable (the handled specials are dispatched in analyze-list BEFORE this).
;; `eval` is NOT here: it is a clojure.core FUNCTION on the spine (compile-eval.ss
;; def-var!s it), so it must resolve as an ordinary var, not punt.
;; `defmacro` stays special — the spine intercepts it before analysis.
;; `syntax-quote` is NOT here: jolt's ` marker is clojure.core-qualified, and the
;; bare name reserves nothing (no Clojure special form goes by it), so a program
;; is free to define one — reserving it silently miscompiled every call.
(define hc-special-names
  '("quote" "unquote" "unquote-splicing" "do" "if" "def"
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
              (and c (var-cell-defined? c)
                   ;; def-ordinal visibility (rt.ss var-def-ordinals): during a
                   ;; build's emit walk the process holds the WHOLE program's
                   ;; defs, but this file's forms were analyzed in order — a
                   ;; same-ns var first defined by a LATER top-level form was
                   ;; NOT resolvable when the in-order load compiled the form
                   ;; being re-analyzed now, so it must not be here either
                   ;; (fall through to refer / clojure.core, the in-order
                   ;; answer). Unstamped vars read 0 and pass; the gate is #f
                   ;; (off) outside the build's walks.
                   (let ((cur (jolt-form-ordinal)))
                     (or (not cur)
                         (fx<=? (var-def-ordinal (chez-actx-cns ctx) nm) cur)))
                   c))
            ;; a :refer'd name resolves to its source ns
            (let ((ref (chez-resolve-refer (chez-actx-cns ctx) nm)))
              (and ref (var-cell-lookup (car ref) (cdr ref))))
            ;; clojure.core last — unless (:refer-clojure :exclude [nm]) or
            ;; ns-unmap took the name out of this ns. The JVM has no core
            ;; mapping to fall back on then, so a use ABOVE the ns's own
            ;; (defn nm …) is "Unable to resolve symbol", not clojure.core/nm
            ;; (jolt#1095). The same guard jsq-resolve-symbol applies.
            ;; Public vars only, as the JVM's refer maps them.
            (chez-core-refer-cell (chez-actx-cns ctx) nm)))))

;; Runtime macros: a defmacro is emitted into the prelude as a
;; def-var! of its cross-compiled expander fn plus (mark-macro! ns name), so the
;; var cell is flagged a macro (rt.ss var-cell macro? field). form-macro? checks the
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
  (if (cseq? dst)
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

;; --- rewriting a form the reader built, before anything else sees it ----------
;; Two things have to happen to a form between the reader and the code that reads
;; it: a syntax-quote marker gets lowered (hc-sq-expand-all) and a set-form gets
;; turned into a real set (hc-macro-arg). Both are "the reader's shape is not the
;; language's shape", both walk the same eager reader shapes, so both go through
;; this one traversal. `visit` gets first refusal on each node and answers a
;; replacement, or #f to keep descending.
;;
;; A container is rebuilt only when something below it changed, so a form with
;; nothing to rewrite comes back as it went in, unallocated. A rebuilt one is
;; handed back the three things the reader keys off OBJECT IDENTITY and a plain
;; copy would drop: its metadata, its map source key order (which is its
;; evaluation order) and its record-literal mark.
(define (hc-keep-identity old new)
  (let ((m (jolt-meta old)))
    (if (jolt-nil? m)
        new
        (let ((c (jolt-with-meta new m)))
          (let ((order (and (pmap? new) (rdr-map-order-ref new))))
            (when order (rdr-map-order-set! c order)))
          c))))

(define (hc-walk-items visit items)
  (let loop ((xs items) (acc '()) (changed #f))
    (if (null? xs)
        (values (reverse acc) changed)
        (let ((y (hc-walk-form visit (car xs))))
          (loop (cdr xs) (cons y acc) (or changed (not (eq? y (car xs)))))))))

(define (hc-walk-map visit form)
  (let ((ty (jolt-get form hc-kw-jolt-type)))
    (cond
      ;; the reader's set FORM: {:jolt/type :jolt/set :value <pvec>}
      ((eq? ty hc-kw-jolt-set)
       (let* ((v (jolt-get form hc-kw-value))
              (nv (hc-walk-form visit v)))
         (if (eq? nv v) form (hc-keep-identity form (jolt-assoc form hc-kw-value nv)))))
      ;; a tagged literal: {:jolt/type :jolt/tagged :tag t :form f}
      ((eq? ty hc-kw-jolt-tagged)
       (let* ((f (jolt-get form hc-kw-form))
              (nf (hc-walk-form visit f)))
         (if (eq? nf f) form (hc-keep-identity form (jolt-assoc form hc-kw-form nf)))))
      ((jolt-nil? ty)
       (let ((order (rdr-map-order-ref form)))
         (if order
             ;; rdr-make-map re-registers the source order for the rebuilt map
             (let-values (((kvs changed) (hc-walk-items visit order)))
               (if changed (hc-keep-identity form (rdr-make-map kvs)) form))
             (let-values (((kvs changed)
                           (hc-walk-items
                             visit (pmap-fold form (lambda (k v a) (cons k (cons v a))) '()))))
               (if changed (hc-keep-identity form (apply jolt-hash-map kvs)) form)))))
      (else form))))

;; A symbol's metadata map is walked too: a def evaluates its name's metadata, so
;; (def ^{:test (fn [] '`foo)} x …) — the shape clojure.test's deftest expands to
;; — must not keep a marker the same code in the init would have had lowered.
(define (hc-walk-sym-meta visit form)
  (let ((m (symbol-t-meta form)))
    (if (pmap? m)
        (let ((nm (hc-walk-map visit m)))
          (if (eq? nm m) form (jolt-with-meta form nm)))
        form)))

(define (hc-walk-form visit form)
  (cond
    ((visit form))
    ((symbol-t? form) (hc-walk-sym-meta visit form))
    ((or (hc-literal? form) (empty-list-t? form)) form)
    ((cseq? form)
     (let-values (((items changed) (hc-walk-items visit (seq->list form))))
       (if (not changed)
           form
           (let ((new (hc-keep-identity form (apply jolt-list items))))
             (when (rdr-ctor-call? form) (rdr-mark-ctor-form new))
             new))))
    ((pvec? form)
     (let-values (((items changed) (hc-walk-items visit (vector->list (pvec-v form)))))
       (if changed (hc-keep-identity form (apply jolt-vector items)) form)))
    ((pset? form)
     (let-values (((items changed) (hc-walk-items visit (pset-fold form cons '()))))
       (if changed (hc-keep-identity form (apply jolt-hash-set items)) form)))
    ((pmap? form) (hc-walk-map visit form))
    (else form)))

;; A set literal reads as the tagged set-form {:jolt/type :jolt/set :value [...]}
;; for the analyzer, but Clojure's ` #{...} ` is a reader macro: a real set exists
;; before any macro runs, so a macro must see one too — (set? arg), seq and conj
;; all depend on it, and hiccup's compiler does exactly that. The shape is jolt's,
;; not the language's, so it is normalized out of the WHOLE argument rather than
;; only its top level. A nested one used to arrive as the raw map, where (set? x)
;; was false and (map? x) TRUE, so the obvious cond over vector?/set?/map? routed
;; a set into the map branch and died on the key :jolt/type (#762).
(define (hc-set-form->set x)
  (and (rdr-set-form? x)
       ;; the set-form is what carries the literal's metadata, so the set built
       ;; from it has to inherit that — reitit writes ^:replace #{...} in route
       ;; data and meta-merge unions instead of replacing without it.
       (let ((items (hc-walk-form hc-set-form->set (jolt-get x rdr-kw-value))))
         (hc-keep-identity x
           (let loop ((i 0) (s empty-pset))
             (if (fx>=? i (pvec-count items)) s
                 (loop (fx+ i 1) (pset-conj s (pvec-nth-d items i jolt-nil)))))))))
(define (hc-macro-arg x) (hc-walk-form hc-set-form->set x))
;; &form and &env are the expander fn's first two PARAMETERS, as on the JVM —
;; the lowering that adds them is analyzer.clj's macro-fn-arities (and its build
;; path twin, ce-macro-arities). They used to be dynamic vars bound around the
;; call instead, which read the same from inside a macro body but left the
;; expander fn taking only its declared parameters: (apply macro-fn form env args)
;; — the call every tools.analyzer-style macroexpand-1 makes — raised an
;; ArityException. Params also outlive the expansion, so a closure a macro
;; returns over &form still has it.
;; The analyzer passes amp-env (the in-scope locals); macroexpand-1 has none, so
;; it defaults to {}.
;; &form meta matches the JVM's {:line :column}. hc-kw-file is defined above
;; with hc-kw-line/hc-kw-column. hc-form-sans-file strips :file from &form meta
;; so a macro reading (meta &form) doesn't see it — libraries branch on its presence.
(define (hc-form-sans-file form)
  (let ((m (jolt-meta form)))
    (if (or (jolt-nil? m) (jolt-nil? (jolt-get m hc-kw-file jolt-nil)))
        form
        (jolt-with-meta form (jolt-dissoc2 m hc-kw-file)))))
(define (hc-expand-1 ctx form . maybe-env)
  ;; Normalize the WHOLE call form once, then take both the arguments and &form
  ;; out of the result. Walking each argument separately left &form holding the
  ;; raw shapes — a macro reading (nth &form 1) rather than its parameter still
  ;; saw a set literal as the tagged map, set? false and map? TRUE, which is the
  ;; #762 hazard surviving in the one place the argument walk did not reach. One
  ;; walk is also less work than one per argument, and an unchanged form comes
  ;; back unallocated.
  (let* ((nform (hc-macro-arg form))
         (items (seq->list nform))
         (head (car items))
         (args (cdr items))
         (expander (var-cell-root (hc-resolve-cell ctx head)))
         (amp-env (if (pair? maybe-env) (car maybe-env) (jolt-hash-map))))
    ;; A macro's two implicit parameters are invisible to its caller, so an arity
    ;; error names the DECLARED count: (m 1 2) on a one-param macro is (2), not
    ;; the (4) the expander is actually called with. The JVM corrects that after
    ;; the fact (Compiler.macroexpand1 rethrows the ArityException with
    ;; actual - 2); jolt asks first, the way seq.ss's arity pre-check classifies
    ;; a site structurally rather than by matching a message afterwards — which
    ;; also means the count never has to be parsed back out of one.
    ;; Only for a real procedure: anything else jolt-invoke owns, error included.
    (when (and (procedure? expander)
               (not (proc-accepts? expander (fx+ 2 (length args)))))
      (jolt-arity-error-name (jolt-proc-arity-name expander) (length args)))
    (hc-propagate-pos form
      (apply jolt-invoke expander (hc-form-sans-file nform) amp-env args))))

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
  (let ((m (and cell (var-cell-meta cell))))
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

;; Does `nm` name a host class? A fully-qualified class name (java.time.Instant),
;; or a short name registered with statics or a constructor (Long, Math, File).
;; The value classes (Long/Integer/String) also self-evaluate to a class through
;; a clojure.core var, so resolve-global tags them :var — this predicate lets the
;; analyzer still treat `(. Long parseLong x)` as a static call, mirroring the
;; `(Long/parseLong x)` slash form.
(define (hc-host-class-name? nm)
  (or (hc-fq-class-name? nm)
      (host-class-registered? nm)))

;; The other side of that line, for a DOTTED name: dotted, with a LOWERCASE
;; segment after the last dot — jolt.host, clojure.string. That is what a
;; NAMESPACE looks like, and what no class jolt models looks like: every class the
;; host registers has a capitalized final segment, hc-fq-class-name? above calls
;; a lowercase one not-a-class, and the reader refuses to read one as a class
;; token.
;;
;; The analyzer needs the question asked this way because resolve-global cannot
;; tell the two apart on its own: a qualified sym it cannot resolve comes back
;; :unresolved whether it is `Math/sqrt` or `no.such.ns/foo`, and reading the
;; second as a class static is what made `(no.such.ns/foo 1)` report "Unknown
;; class no.such.ns" from inside the call. Paired with jolt.host/namespace-for
;; below (an alias in the compile ns, else ns.ss chez-ns-exists?, the rule find-ns
;; uses) it fires only on a namespace-shaped ns that names nothing loaded AT ALL
;; — the JVM's "No such namespace".
;; A loaded namespace missing only the var must stay late-bound: that is how
;; jolt-core reaches this very contract, and which jolt.host vars exist depends on
;; which host files the boot loaded, so the guard there is the static one
;; (jolt-host-manifest.txt) and the miss reports at the call.
;;
;; NOT merely (not (hc-fq-class-name? nm)): a name with no dot is read by its
;; FIRST letter, the same convention. Lowercase — `str/join` — is an :as alias
;; that was never registered, and the JVM's answer is the same "No such
;; namespace" (the analyzer still asks host-class-name? first: a (defrecord
;; point …) makes `point/create` a class, on both runtimes). A bare `Foo` is how
;; an :import-ed short name is spelled, and its class may not be registered until
;; its provider autoloads, so a Capitalized no-dot name stays with the host-static
;; arm (documented in known-divergences.edn). Anything but a lowercase LETTER at
;; the deciding position is left there too.
(define (hc-ns-shaped-name? nm)
  (let ((n (string-length nm)))
    (let loop ((i (fx- n 1)))
      (cond ((fx<? i 0) (and (fx>? n 0) (char-lower-case? (string-ref nm 0))))
            ((char=? (string-ref nm i) #\.)
             (and (fx<? (fx+ i 1) n) (char-lower-case? (string-ref nm (fx+ i 1)))))
            (else (loop (fx- i 1)))))))

;; Compiler.namespaceFor: the namespace `nm` names from the compile ns — an :as
;; alias registered there, else a namespace by that name — as its name string,
;; or #f when there is neither. The alias half is what keeps a loaded namespace
;; that merely lacks the var on the late-bound arm above when it is reached
;; THROUGH an alias (the mint compiles `op-registry/leaf-call-procs` against a
;; seed that has the alias but not yet the var); the analyzer's "No such
;; namespace" is this answering #f.
(define (hc-namespace-for ctx nm)
  (let ((target (or (chez-resolve-alias (chez-actx-cns ctx) nm) nm)))
    (and (chez-ns-exists? target) target)))

(define (hc-resolve-global ctx sym)
  (let* ((nm (symbol-t-name sym))
         (cell (hc-resolve-cell ctx sym)))
    (if (and cell (var-cell-defined? cell))
        (let* ((base (jolt-hash-map hc-kw-kind hc-kw-var
                                    hc-kw-ns (var-cell-ns cell)
                                    hc-kw-name (var-cell-name cell)))
               (nr (hc-cell-num-ret cell))
               (base (if nr (jolt-assoc base hc-kw-num-ret nr) base)))
          ;; :private — a ^:private var of ANOTHER namespace, which the analyzer
          ;; refuses to reference ("var: a/hidden is not public"), as the JVM's
          ;; Compiler.resolveIn / isMacro do. (var a/hidden) still takes it.
          (if (and (not (string=? (var-cell-ns cell) (chez-actx-cns ctx)))
                   (var-private? cell))
              (jolt-assoc base hc-kw-private #t)
              base))
        (cond
          ;; java.util.Map / clojure.lang.Named — a dotted class name.
          ((hc-fq-class-name? nm) (jolt-hash-map hc-kw-kind hc-kw-class hc-kw-name nm))
          ;; a bare Capitalized name that names a registered host class — an
          ;; imported short name (`(:import [java.time ZonedDateTime])` then
          ;; `(. ZonedDateTime parse s)`). Only when otherwise unresolved, so a
          ;; same-named var still wins.
          ((and (fx>? (string-length nm) 0) (char-upper-case? (string-ref nm 0))
                (host-class-has-statics? nm))
           (jolt-hash-map hc-kw-kind hc-kw-class hc-kw-name nm))
          ;; a bare Capitalized name that names a deftype/defrecord defined in this
          ;; session resolves to its "ns.Name" class-name STRING (jolt models a class
          ;; as its name), matching (type inst)/(class inst) — so (= SomeType
          ;; (type inst)) holds and (instance? SomeType x) works whether the type was
          ;; :import-ed or referenced locally. As with host classes, only fires when
          ;; otherwise unresolved (a same-named var wins). chez-deftype-simple->tag
          ;; is a forward ref to records.ss (bound by analyze time).
          ((and (fx>? (string-length nm) 0) (char-upper-case? (string-ref nm 0))
                (chez-deftype-simple->tag nm))
           => (lambda (dtag) (jolt-hash-map hc-kw-kind hc-kw-class hc-kw-name dtag)))
          (else (jolt-hash-map hc-kw-kind hc-kw-unresolved hc-kw-name nm))))))

;; Every unqualified var name resolvable from the compile ns — the current ns's
;; own interns plus clojure.core's publics (referred into every namespace). The
;; analyzer matches an unresolved symbol against this pool for a "did you mean"
;; suggestion, so it runs only on the compile-error path (a full var-table scan is
;; fine there). Returns a jolt list of name strings; duplicates are harmless (the
;; analyzer dedupes).
(define (hc-resolvable-names ctx)
  (let ((cns (chez-actx-cns ctx))
        (acc '()))
    (vector-for-each
      (lambda (c)
        (when (var-cell-defined? c)
          (let ((ns (var-cell-ns c)))
            (when (or (string=? ns cns) (string=? ns "clojure.core"))
              (set! acc (cons (var-cell-name c) acc))))))
      (var-table-cells))
    (list->cseq acc)))

(define (hc-intern! ctx ns-name nm) (declare-var! ns-name nm) jolt-nil)

;; --- syntax-quote lowering ---------------------------------------------------
;; Lowers a `form
;; to CONSTRUCTION CODE — Chez reader forms calling __sqcat/__sqvec/__sqmap/
;; __sqset/__sq1 + quote — that the analyzer re-analyzes, so a backtick compiles
;; with zero runtime cost (read -> macroexpand -> compile). Symbols resolve to
;; clojure.core / the compile ns; a foo# auto-gensym is stable within one `.
;; the syntax-quote specials + resolver live in reader.ss (jsq-specials /
;; jsq-resolve-symbol), shared with the read-string data path.

;; The bump and the read are one step. This one runs during COMPILATION, which is
;; now parallel across namespaces, and unlocked two threads draw the same number
;; — see jolt-gensym in converters.ss.
(define hc-sq-gensym-counter 0)
(define hc-sq-gensym-mutex (make-mutex))
(define (hc-sq-gensym base)
  (jolt-symbol #f (string-append base "__"
                                 (number->string
                                  (jolt-with-mutex hc-sq-gensym-mutex
                                    (set! hc-sq-gensym-counter (+ hc-sq-gensym-counter 1))
                                    hc-sq-gensym-counter))
                                 "__auto")))

(define (hc-sym nm) (jolt-symbol #f nm))
;; is `x` a non-empty list FORM whose head is the unqualified symbol `nm`?
;; Detect a (unquote …) / (unquote-splicing …) form in a syntax-quote template.
;; Any seq counts, not just a proper list: a macro that builds the template with
;; map/for (e.g. deftype's rewrite-set) yields a LAZY seq, and its ~unquotes must
;; still be recognized.
;; head symbol matches name nm, bare or clojure.core-qualified — the reader
;; produces clojure.core/unquote(-splicing) for ~/~@ (JVM parity), and this is
;; only used to spot those heads in syntax-quote templates.
;; hc-list?, not cseq?: a macro that REBUILDS a template — deftype's rewrite-body
;; maps over the method body to rewrite mutable-field reads — hands back a lazy
;; seq, and a ~ inside it must still read as an unquote. Testing cseq? only meant
;; those templates lowered their (unquote x) as an ordinary list call, so a
;; deftype method's `(= ~a ~b) came out as (clojure.core/= (clojure.core/unquote
;; a) …) — which is what broke core.match, whose pattern types are deftypes. The
;; JVM's isUnquote is likewise a plain ISeq + head check.
(define (hc-head-is? x nm)
  (and (hc-list? x)
       (let ((s (jolt-seq x)))
         (and (not (jolt-nil? s))
              (let ((h (seq-first s)))
                (and (symbol-t? h) (string=? (symbol-t-name h) nm)
                     (let ((ns (hc-sym-ns h)))
                       (or (jolt-nil? ns)
                           (and (string? ns) (string=? ns "clojure.core"))))))))))
(define (hc-second x) (seq-first (jolt-seq (seq-more (jolt-seq x)))))

;; compile path: resolve against the compile ns, via the shared resolver
;; (reader.ss jsq-resolve-symbol). Same resolution the data path uses, so a
;; compiled ` and a read-string ` agree on every name.
(define (hc-sq-symbol ctx form gsmap)
  (jsq-resolve-symbol (chez-actx-cns ctx) form gsmap hc-sq-gensym))

(define (hc-sq-lower ctx form gsmap)
  ;; Non-nil metadata wraps the lowered form in with-meta, with the meta map
  ;; itself lowered as a template (so a ^Thread tag qualifies to its FQN) —
  ;; matches what the JVM emits for `^Thread [] and `^:foo a alike. Reader
  ;; position keys (:line/:column/:file) are reader artifacts, not user meta:
  ;; they are stripped, or every macro template form would carry its
  ;; definition-site position into the expansion and shadow the reader
  ;; position of the macro's input form.
  (let* ((out (hc-sq-lower-bare ctx form gsmap))
         (m (jolt-meta form))
         (m (if (jolt-nil? m) m (jolt-dissoc m rdr-kw-line rdr-kw-column rdr-kw-file))))
    (if (or (jolt-nil? m) (zero? (jolt-count m)))
        out
        (jolt-list (hc-sym "with-meta") out (hc-sq-lower ctx m gsmap)))))

;; Is this the raw (clojure.core/syntax-quote x) form the reader emits for a `?
;; The qualification is the point, exactly as in rdr-syntax-quote-form?: a BARE
;; (syntax-quote x) is an ordinary call to whatever the program means by that
;; name, not a marker. hc-head-is? accepts either spelling, so it cannot be used
;; here.
(define (hc-syntax-quote-form? x)
  (and (hc-list? x)
       (let ((s (jolt-seq x)))
         (and (not (jolt-nil? s))
              (let ((h (seq-first s)))
                (and (symbol-t? h) (string=? (symbol-t-name h) "syntax-quote")
                     (let ((ns (hc-sym-ns h)))
                       (and (string? ns) (string=? ns "clojure.core")))))))))

(define (hc-sq-lower-bare ctx form gsmap)
  (cond
    ((hc-head-is? form "unquote") (hc-second form))
    ;; A NESTED backquote, lowered INSIDE-OUT — the order the JVM gets for free
    ;; by resolving syntax-quote in the reader, where the inner ` has already
    ;; become construction code before the outer one walks it. Two things follow
    ;; from doing it in that order, and neither works without it:
    ;;
    ;;   * the inner template's ~unquotes belong to the INNER `. Lowering the
    ;;     nested form as an ordinary list let the outer walk claim them, so
    ;;     `(defmacro f [x#] `(g ~x#)) tried to resolve x# as a variable while
    ;;     merely DEFINING the outer macro — the parameter it names does not
    ;;     exist until the outer macro is called.
    ;;   * a bare symbol left in the inner's construction code (what an inner
    ;;     ~x# lowers to) is then walked by the OUTER gsmap, so the x# in the
    ;;     parameter vector and the x# in the body get the same gensym.
    ;;
    ;; Fresh gsmap for the inner: each ` has its own auto-gensym scope.
    ((hc-syntax-quote-form? form)
     (hc-sq-lower ctx (hc-syntax-quote-lower ctx (hc-second form)) gsmap))
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

;; --- lowering every marker in a form, before anything can see it -------------
;; Clojure's ` is a READER macro: by the time a program sees a form the backtick
;; is already gone, replaced by its expansion. jolt reads ` to a marker
;; (clojure.core/syntax-quote FORM) and lowers it in the analyzer, which is right
;; for a marker in evaluated position and leaves it VISIBLE everywhere else — a
;; macro reading its own argument forms, or a quoted form. typedclojure's
;; (f/sub-f sb `call-abstract-many* opts) asserts its argument is
;; (quote qualified-sym) and got (clojure.core/syntax-quote call-abstract-many*),
;; so the checker would not load (jolt-024c).
;;
;; So the analyzer lowers every marker in a top-level form up front, at the
;; reader's moment, through this same hc-syntax-quote-lower. A marker is replaced
;; by its lowering and NOT rewalked: it is already fully lowered, nested backticks
;; included (hc-sq-lower-bare does those inside-out).
;;
;; Only the reader's eager shapes are walked. A macro that builds its expansion
;; lazily can still hand back a marker; analyze's own syntax-quote case — the
;; evaluated-position path — is what lowers that one.
(define (hc-sq-expand-all ctx form)
  (hc-walk-form
    (lambda (f)
      (and (hc-syntax-quote-form? f) (hc-syntax-quote-lower ctx (hc-second f))))
    form))
;; a ^Type param hint: name is the tag (a symbol, sometimes a string). Resolve it
;; against the record registry (records.ss) so the inference seeds the param as
;; that record — the open-world / cross-ns path where no caller type is inferred.
(define (hc-record-tag-name name)
  (cond ((symbol-t? name) (symbol-t-name name))
        ((string? name) name)
        (else #f)))
(define (hc-record-type? ctx name)
  (let ((nm (hc-record-tag-name name)))
    (if (and nm (chez-find-ctor-key nm (hc-current-ns ctx))) #t #f)))
(define (hc-record-ctor-key ctx name)
  (let ((nm (hc-record-tag-name name)))
    (or (and nm (chez-find-ctor-key nm (hc-current-ns ctx))) jolt-nil)))
;; The fully-qualified deftype tag ("ns.Name") IFF `class` names a deftype DEFINED
;; in the ctx's compile ns — the analyzer qualifies a bare (Name. …) to it, so a
;; deftype doesn't shadow a same-named built-in host class in an unrelated ns
;; (rewrite-clj imports java.io.PushbackReader; tools.reader defines its own). Strict:
;; only this ns's own def (the preferred shape key) counts, not the global
;; simple-name fallback, so a ns that merely uses the built-in resolves nil.
(define (hc-deftype-ctor-class ctx class)
  (let* ((nm (jolt-str-render-one class))
         (cns (hc-current-ns ctx))
         ;; a QUALIFIED ctor (ns/Name. or alias/Name., a cross-ns deftype — SCI
         ;; builds sci.impl.types/Reified this way) resolves the ns segment (an
         ;; alias -> its real ns) and looks the factory up there; a bare Name.
         ;; resolves against THIS ns (a deftype named like a host class stays local).
         (slash (let loop ((i 0))
                  (cond ((fx>=? i (string-length nm)) #f)
                        ((char=? (string-ref nm i) #\/) i)
                        (else (loop (fx+ i 1))))))
         (rns (if slash
                  (let ((seg (substring nm 0 slash)))
                    (or (chez-resolve-alias cns seg) seg))
                  cns))
         (base (if slash (substring nm (fx+ slash 1) (string-length nm)) nm))
         (key (string-append rns "/->" base)))
    (if (hashtable-ref chez-record-shapes-tbl key #f)
        (string-append rns "." base)
        jolt-nil)))
;; record + protocol-method shapes for the inference, from the runtime registries
;; (records.ss) populated as deftype/defprotocol forms load.
(define (hc-record-shapes ctx) (chez-record-shapes-map))
(define (hc-protocol-methods ctx) (chez-protocol-methods-map))
;; Do the optimizing passes run at all? On for every build that is not --dev,
;; and for nothing else: the runtime compile spine (REPL, load-string, runtime
;; require) leaves it off, because a form compiled there must stay redefinable.
;;
;; ONE flag. There were two -- hc-optimize? for --opt and hc-release? for
;; release -- and (or …) of them was the only thing either was ever read for, so
;; they were the same question asked twice. That duplication is what let the
;; inline gate below read "--opt" when it meant "closed world".
(define hc-optimize? #f)
(define (set-optimize! on) (set! hc-optimize? on))
(define (hc-inference-enabled? ctx) hc-optimize?)
;; Inline requires direct-link, and that is the WHOLE condition: splicing a defn
;; body at a call site is sound exactly when the callee's var cannot be redefined
;; out from under the copy, which is the closed world direct-linking commits to.
;; A ^:dynamic/^:redef def, --no-direct-link and --dev all stay var-routed, so
;; none of them splice.
;;
;; It used to also require hc-optimize? -- i.e. --opt -- which made the DEFAULT
;; release build emit a real call everywhere the optimized build emitted a spliced
;; body, and Chez cannot make that up: it does not inline across top-level forms
;; in a compiled file. That was a policy dial on a pass whose precondition is a
;; correctness property, so it is gone rather than defaulted differently. There is
;; no configuration in which the un-spliced code is preferable to the spliced code
;; at the same linkage, so there is no reason to keep a second path to test.
(define hc-direct-link? #f)
(define (set-direct-link-flag! on) (set! hc-direct-link? on))
(define (hc-inline-enabled? ctx) hc-direct-link?)
;; Inline-body registry: jolt.passes stashes an inline-eligible defn's
;; {:params :body :nhints :ret} here (keyed ns/name) as its form is optimized;
;; jolt.passes.inline fetches it to splice the body at a call site. The stash is an
;; opaque jolt value to the host — IR maps round-tripping through the table.
;; Shared across compilations on purpose — that is what makes cross-namespace
;; inlining possible — so it is written from every thread compiling once
;; namespaces load in parallel. Concurrent inserts into a strong hashtable lose
;; each other, and a lost stash is an inline that silently does not happen. The
;; fetch is a single-key read and stays unlocked: it sits at every candidate call
;; site, and an unlocked read of a strong table is safe (see var-table in rt.ss).
(define inline-stash-table (make-hashtable string-hash string=?))
(define inline-stash-mu (make-mutex))
;; Has this var been defined more than once, with a value, in the program loaded
;; so far? The inline pass asks before stashing: splicing a var that is redefined
;; later freezes whichever definition was current when the CALLER compiled, so one
;; binary answers two ways depending which side of the second def a call site sat
;; on (jolt-rtjm). `jolt build` loads the whole app from source before it emits any
;; of it, so the answer is already final by stash time.
;;
;; Conservative in the harmless direction: a var redefined for any reason at all
;; (the post-prelude native clobber, a reloaded namespace) loses its stash and
;; keeps a real call, which is exactly what it compiles to without direct-linking.
;; Every callee the inline pass actually spliced somewhere, "ns/name". A callee
;; whose every call site was spliced has no reference left in the emitted code, so
;; the tree-shake graph walk drops its def -- and the def's record is where the
;; (jolt-register-source! …) lives, which is the only thing that maps an inlined
;; frame back to ns/name (file:line). A --tree-shake binary then printed ONE frame
;; where the same build unshaken printed three (jolt-o13s). dce.ss roots this set,
;; so the identity survives the shake.
;;
;; Recorded at the SPLICE, not at the stash: a stashed fn nobody spliced is still
;; genuinely dead and should still shake away.
(define inline-spliced-set (make-hashtable string-hash string=?))
(define (hc-mark-spliced! ctx ns-name nm)
  (jolt-with-mutex inline-stash-mu
    (hashtable-set! inline-spliced-set (string-append ns-name "/" nm) #t))
  jolt-nil)
(define (inline-spliced-fqns)
  (jolt-with-mutex inline-stash-mu (vector->list (hashtable-keys inline-spliced-set))))
(define (hc-var-redefined? ctx ns-name nm)
  (if (var-redefined? ns-name nm) #t jolt-nil))
(define (hc-stash-inline! ctx ns-name nm m)
  (jolt-with-mutex inline-stash-mu
    (hashtable-set! inline-stash-table (string-append ns-name "/" nm) m))
  jolt-nil)
(define (hc-inline-ir ctx ns-name nm)
  (or (hashtable-ref inline-stash-table (string-append ns-name "/" nm) #f) jolt-nil))

;; --- direct-link of a seed var --------------------------------------------------
;; The back end asks this before emitting a call to a var it did not define in
;; this build: may the site bind the var's ROOT procedure once, at load, and
;; call it directly? The answer is the same closed-world rule an app def gets
;; under direct-link, applied to the seed image:
;;   - the var's namespace was already defined when the runtime image booted
;;     (ldr-runtime-image-ns-copy — the set build.ss reads as bld-boot-loaded),
;;     so it is preloaded ahead of every app def and NOT emitted by this build;
;;   - its root is a procedure whose arity mask admits the call's arity, so the
;;     direct call cannot land on a keyword/map/multimethod or a wrong arity
;;     (those keep jolt-invoke, which owns the ArityException);
;;   - it is not ^:dynamic or ^:redef (the closed-world opt-outs), and the app
;;     has not redefined it (var-redefined?).
;; What changes for such a site is what changes under the JVM's direct linking:
;; a later alter-var-root / with-redefs of the var is not seen by the compiled
;; call. `jolt run` never direct-links, so the REPL and with-redefs in tests
;; keep the var-routed call.
(define hc-seed-ns-tbl #f)
;; The boot that owns the runtime supplies the set: loader.ss installs its
;; boot-time copy (ldr-runtime-image-ns-copy — the same set build.ss reads as
;; bld-boot-loaded) for the CLI and `jolt build`, and gate-boot.ss snapshots the
;; var table after the image loads for the pass gates. A boot that installs
;; nothing (the Gambit host) direct-links no seed var, which is the safe answer.
(define hc-seed-ns-source #f)
;; The one seam a driver calls when the runtime image has finished booting —
;; every namespace with vars is image-defined, no user code has run — with the
;; thunk that answers that set. Two things follow from "booted": the seed set
;; the direct-link rule reads, and the end of the boot's own re-assertions of
;; its natives, which are not redefinitions to anything compiled from here on
;; (rt.ss var-redefined-clear!).
(define (hc-runtime-image-booted! source)
  (set! hc-seed-ns-source source)
  (var-redefined-clear!))
(define (hc-seed-ns? ns)
  (and hc-seed-ns-source
       (begin
         (unless hc-seed-ns-tbl (set! hc-seed-ns-tbl (hc-seed-ns-source)))
         (hashtable-ref hc-seed-ns-tbl ns #f))))
(define hc-kw-dynamic (keyword #f "dynamic"))
(define hc-kw-redef (keyword #f "redef"))
(define (hc-seed-callable? ctx ns-name nm nargs)
  (let ((cell (var-cell-lookup ns-name nm)))
    (if (and cell
             (hc-seed-ns? ns-name)
             (var-cell-defined? cell)
             (procedure? (var-cell-root cell))
             (fixnum? nargs)
             (fxlogbit? nargs (procedure-arity-mask (var-cell-root cell)))
             (let ((m (var-cell-meta cell)))
               (or (not m) (jolt-nil? m)
                   (and (not (jolt-truthy? (jolt-get m hc-kw-dynamic jolt-nil)))
                        (not (jolt-truthy? (jolt-get m hc-kw-redef jolt-nil))))))
             (not (var-redefined? ns-name nm)))
        ;; a var the seed minted direct-linked answers its jv$ binding name, so
        ;; the site applies the binding (rt.ss var-linked-symbol); any other
        ;; seed var answers #t and the site hoists its root at load
        (let ((sym (var-linked-symbol cell)))
          (if sym (symbol->string sym) #t))
        jolt-nil)))

;; --- declare the hot clojure.core primitives so resolve-global sees them ------
;; Mirrors backend_scheme.clj native-ops keys (op-registry entries with a :call)
;; minus the internal protocol-dispatch{1,2,3} emit helpers, which are not
;; clojure.core names. The emitter lowers each of these inline, so the declared
;; cell's unbound root is never deref'd. host/chez/manifest-check.sh fails CI if
;; this list drifts from native-ops.
(for-each (lambda (nm) (declare-var! "clojure.core" nm))
  '("+" "-" "*" "/" "<" ">" "<=" ">=" "=" "inc" "dec" "not" "min" "max"
    "mod" "rem" "quot" "vector" "hash-map" "hash-set" "conj" "get" "nth" "count"
    "assoc" "dissoc" "contains?" "find" "empty?" "peek" "pop" "first" "rest" "next" "seq"
    "cons" "list" "reverse" "last" "map" "filter" "remove" "reduce" "reduce-kv" "into" "concat"
    "apply" "range" "take" "drop" "keys" "vals" "even?" "odd?" "pos?" "neg?"
    "zero?" "identity" "nil?" "some?" "identical?" "ex-info"
    ;; not a public name — the deftype macro's field bindings lower through it
    ;; (records.ss jrec-field). Declared here like the rest so the list matches
    ;; native-ops, which manifest-check.sh enforces.
    "__deftype-field"
    ;; the `lazy-seq` macro's two halves (lazy-bridge.ss): internal names too
    "make-lazy-seq" "coll->cells" "chunked-seq?"
    "aget" "aset" "alength"
    "bit-and" "bit-or" "bit-xor" "bit-not"
    "bit-shift-left" "bit-shift-right" "unsigned-bit-shift-right"
    "unchecked-add" "unchecked-subtract" "unchecked-multiply"
    "unchecked-inc" "unchecked-dec" "unchecked-negate"))


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
  (def-var! "jolt.host" "form-tagged?" hc-tagged?)
  (def-var! "jolt.host" "form-tag-name" hc-tag-name)
  (def-var! "jolt.host" "form-inst?" hc-inst?)
  (def-var! "jolt.host" "form-uuid?" hc-uuid?)
  (def-var! "jolt.host" "form-ns-value?" hc-ns-value?)
  (def-var! "jolt.host" "form-ns-value-name" hc-ns-value-name)
  (def-var! "jolt.host" "form-var-value?" hc-var-value?)
  (def-var! "jolt.host" "form-var-value-ns" hc-var-value-ns)
  (def-var! "jolt.host" "form-var-value-name" hc-var-value-name)
  (def-var! "jolt.host" "form-class-value?" hc-class-value?)
  (def-var! "jolt.host" "form-class-value-name" hc-class-value-name)
  (def-var! "jolt.host" "unchecked-math?" hc-unchecked-math?)
  (def-var! "jolt.host" "allow-unresolved-vars?" hc-allow-unresolved-vars?)
  (def-dynvar! "jolt.host" "*invoke-rewrite*" jolt-nil)
  (def-var! "jolt.host" "invoke-rewriter" hc-invoke-rewriter)
  (def-var! "jolt.host" "form-bigdec?" hc-bigdec?)
  (def-var! "jolt.host" "form-bigdec-source" hc-bigdec-source)
  (def-var! "jolt.host" "form-bigdec-value?" hc-bigdec-value?)
  (def-var! "jolt.host" "form-bigdec-value-source" hc-bigdec-value-source)
  (def-var! "jolt.host" "form-inst-value?" hc-inst-value?)
  (def-var! "jolt.host" "form-inst-value-source" hc-inst-value-source)
  (def-var! "jolt.host" "form-uuid-value?" hc-uuid-value?)
  (def-var! "jolt.host" "form-uuid-value-source" hc-uuid-value-source)
  (def-var! "jolt.host" "form-elements" hc-elements)
  (def-var! "jolt.host" "form-vec-items" hc-vec-items)
  (def-var! "jolt.host" "form-set-items" hc-set-items)
  (def-var! "jolt.host" "form-map-pairs" hc-map-pairs)
  (def-var! "jolt.host" "form-regex-source" hc-regex-source)
  (def-var! "jolt.host" "form-inst-source" hc-inst-source)
  (def-var! "jolt.host" "form-uuid-source" hc-uuid-source)
  (def-var! "jolt.host" "form-position" hc-form-position)
  (def-var! "jolt.host" "form-line" hc-form-line)
  ;; a number literal in CHEZ syntax for the backend's emitted source — jolt's
  ;; own str follows the reference printer (bigint N suffix, E exponents),
  ;; which Chez's reader rejects
  (def-var! "jolt.host" "chez-number-literal" (lambda (n) (number->string n)))
  (def-var! "jolt.host" "form-special?" hc-special?)
  (def-var! "jolt.host" "compile-ns" hc-current-ns)
  ;; A ctx for a DIFFERENT namespace than the one being compiled. The analyzer
  ;; needs one to re-analyze a registered fn literal's SOURCE form in the ns it
  ;; was compiled in (jolt-5n2p): its free symbols resolve there and not at the
  ;; site the value was spliced into, which is the same reason the image restore
  ;; path compiles its wrapper in (image-fnsrc-ns x).
  (def-var! "jolt.host" "ctx-for-ns" (lambda (ns) (make-analyze-ctx ns)))
  (def-var! "jolt.host" "late-bind?" hc-late-bind?)
  (def-var! "jolt.host" "form-macro?" hc-macro?)
  (def-var! "jolt.host" "form-expand-1" hc-expand-1)
  (def-var! "jolt.host" "resolve-global" hc-resolve-global)
  (def-var! "jolt.host" "resolvable-names" hc-resolvable-names)
  (def-var! "jolt.host" "host-class-name?" hc-host-class-name?)
  (def-var! "jolt.host" "ns-shaped-name?" hc-ns-shaped-name?)
  (def-var! "jolt.host" "namespace-for" hc-namespace-for)
  (def-var! "jolt.host" "host-intern!" hc-intern!)
  (def-var! "jolt.host" "form-syntax-quote-lower" hc-syntax-quote-lower)
  (def-var! "jolt.host" "form-syntax-quote-expand" hc-sq-expand-all)
  (def-var! "jolt.host" "record-type?" hc-record-type?)
  (def-var! "jolt.host" "record-ctor-key" hc-record-ctor-key)
  (def-var! "jolt.host" "deftype-ctor-class" hc-deftype-ctor-class)
  (def-var! "jolt.host" "record-shapes" hc-record-shapes)
  (def-var! "jolt.host" "protocol-methods" hc-protocol-methods)
  (def-var! "jolt.host" "inline-enabled?" hc-inline-enabled?)
  (def-var! "jolt.host" "inference-enabled?" hc-inference-enabled?)
  (def-var! "jolt.host" "inline-ir" hc-inline-ir)
  (def-var! "jolt.host" "stash-inline!" hc-stash-inline!)
  (def-var! "jolt.host" "var-redefined?" hc-var-redefined?)
  (def-var! "jolt.host" "seed-callable?" hc-seed-callable?)
  (def-var! "jolt.host" "mark-spliced!" hc-mark-spliced!))

(hc-install!)

;; --- jolt.scheme's lookup half -----------------------------------------------
;; scheme-proc resolves a top-level Scheme binding at CALL time, through the
;; adapter's global-reflection seam (sa-baked-global — portcheck pins the raw
;; top-level-value/bound? primitives to scheme-adapter-runtime.ss). Its #f
;; sentinel conflates "unbound" with "bound to #f", which is fine here: this
;; fetches PROCEDURES, and a procedure is never #f. An unbound name answers a
;; catchable ex-info rather than Chez's raw error, because "you typo'd the
;; primitive" is the hatch's everyday failure — and in a tree-shaken binary it
;; is also how a shaken-out primitive reports itself. It lives HERE, in the
;; runtime half, because it needs no compiler: a build that drops the compiler
;; image (build.ss drop-compiler?) leaves compile-eval.ss out, and a program
;; whose only hatch use is jolt.scheme/proc must still answer. Its sibling
;; scheme-eval-string, which does compile, stays in compile-eval.ss.
(def-var! "jolt.host" "scheme-proc"
  (lambda (name)
    (let ((sym (string->symbol (jolt-need-string name))))
      (let ((v (sa-baked-global sym)))
        (or v
            (jolt-throw (jolt-ex-info
                         (string-append "no top-level Scheme binding: "
                                        (symbol->string sym))
                         (jolt-hash-map (jolt-keyword "name")
                                        (symbol->string sym)))))))))
