;; dce.ss — tree-shaking (jolt build --tree-shake): whole-program reachability DCE.
;;
;; Build one call graph over the re-emitted app + libraries AND the clojure.core
;; prelude, keep -main + every side-effecting top-level form + everything reachable
;; from those, drop the rest. Bails (keeps everything) if reachable code resolves a
;; var by name at runtime (eval/resolve/...), which a static graph can't follow —
;; unless a deps.edn vouches for that def (:jolt/tree-shake {:allow-dynamic […]}). Per
;; Stalin's rule, ANY reference — a call OR a value/#'x — keeps its target live, so a
;; fn passed to map or referenced as #'x is never dropped.
;;
;; Loaded by build.ss after the compiler image (needs jolt.ir/reduce-ir-children).
;; The records it consumes come from ei-emit-ns-records (app/libs) + dce-blob-records
;; (the prelude); both build the (dce-rec …) shape below.

;; --- the DCE record ---------------------------------------------------------
;; keep?: #t = a non-def form (side effect / registration) — always emitted, and its
;; refs are reachability roots. #f = a prunable def emitted only if fqn is reached.
;; fqn: "ns/name" of a prunable def, else #f. refs: "ns/name" strings it references.
;; str: the Scheme source to emit.
;; INIT-RUNS?: the def's init executes code at load (anything but a fn literal
;; or a constant) -- see dce-def-init-runs?. Optional, #f when not given: keep
;; records are roots anyway and prelude records are never rooted by it.
(define (dce-rec keep? fqn refs str . init-runs)
  (vector keep? fqn refs str (and (pair? init-runs) (car init-runs) #t)))
(define (dce-rec-init-runs? r) (vector-ref r 4))
(define (dce-rec-keep? r) (vector-ref r 0))
(define (dce-rec-fqn r)   (vector-ref r 1))
(define (dce-rec-refs r)  (vector-ref r 2))
(define (dce-rec-str r)   (vector-ref r 3))

;; --- reference extraction from IR -------------------------------------------
(define dce-kw-op   (keyword #f "op"))
(define dce-kw-var  (keyword #f "var"))
(define dce-kw-the-var (keyword #f "the-var"))
(define dce-kw-def  (keyword #f "def"))
(define dce-kw-ns   (keyword #f "ns"))
(define dce-kw-name (keyword #f "name"))
(define dce-reduce-children (var-deref "jolt.ir" "reduce-ir-children"))

;; "ns/name" of every var reference anywhere in an IR node, prepended to acc. Counts
;; a :var (call head or value) and a :the-var (#'x). Arg order (acc node) matches
;; reduce-ir-children's fold fn so it nests directly.
;; A BARE varargs binding -- [:string :int :&], babashka.ffi's form where each
;; call infers its own tail -- compiles a foreign-procedure for every new tail
;; shape at the CALL (java/ffi.ss ffi-varargs-compile), and petite cannot
;; compile one ("cannot compile foreign-procedure: compiler is not loaded").
;; The back end lowers the binding to direct Scheme calls, so no :var names
;; what it reaches; the ref is the host entry that does the compiling,
;; jolt.host/ffi-varargs-compile, which dce-compile-refs lists.
(define dce-kw-ffi-fn   (keyword #f "ffi-fn"))
(define dce-kw-argtypes (keyword #f "argtypes"))
(define dce-ffi-varargs-ref "jolt.host/ffi-varargs-compile")
(define (dce-ffi-bare-varargs? node)
  (let ((at (jolt-get node dce-kw-argtypes)))
    (and (not (jolt-nil? at))
         (> (jolt-count at) 0)
         (let ((last (jolt-peek at)))
           (and (string? last) (or (string=? last "&") (string=? last "varargs")))))))
;; A require the graph can READ is baked: every (require 'a.b) and every ns
;; form's clauses name their namespaces as constants, bld-require-closure walks
;; them into the binary, and the call no-ops at startup because the namespace
;; is already defined. A require whose argument is COMPUTED -- (require (symbol
;; nm)), a plugin loaded by name, (require ns :reload) over shipped source --
;; names a namespace the build never saw, so at run time it is a load from
;; source: the compiler, and code the static graph cannot follow. Same for
;; `use` and `load-libs`, and for `compile`, which recompiles unconditionally.
;; A built binary that did this worked in 0.8.6 because every binary carried
;; the compiler; with the verdict dropping it by default such a program died
;; at the require on "variable jolt-aot-capture-file is not bound". The ref is
;; the host entry that does the loading, jolt.host/load-namespace, which both
;; lists carry.
(define dce-kw-invoke (keyword #f "invoke"))
(define dce-kw-args   (keyword #f "args"))
(define dce-kw-quote  (keyword #f "quote"))
(define dce-dynamic-load-ref "jolt.host/load-namespace")
(define dce-load-by-name-fns
  '("clojure.core/require" "clojure.core/use" "clojure.core/load-libs"))
(define (dce-static-arg? a)
  (let ((op (jolt-get a dce-kw-op)))
    (or (eq? op dce-kw-const) (eq? op dce-kw-quote))))
(define (dce-invoke-fqn node)
  (let ((f (jolt-get node dce-kw-fn)))
    (and (not (jolt-nil? f))
         (eq? (jolt-get f dce-kw-op) dce-kw-var)
         (string-append (jolt-get f dce-kw-ns) "/" (jolt-get f dce-kw-name)))))
(define (dce-loader-fqn? fqn)
  (or (string=? fqn "clojure.core/compile") (and (member fqn dce-load-by-name-fns) #t)))
;; An INVOKE of one of them is static when every argument is a constant (and
;; compile never is); the var in any other position -- (apply require specs),
;; (run! require nss), a value handed on -- is a load by a name the graph
;; cannot read, so it is dynamic wherever it is used.
(define (dce-dynamic-load? node)
  (let ((fqn (dce-invoke-fqn node)))
    (and fqn
         (dce-loader-fqn? fqn)
         (or (string=? fqn "clojure.core/compile")
             (let ((args (jolt-seq (jolt-get node dce-kw-args))))
               (and (not (jolt-nil? args))
                    (not (for-all dce-static-arg? (seq->list args)))))))))
(define (dce-collect-refs acc node)
  (let ((op (jolt-get node dce-kw-op)))
    (cond ((or (eq? op dce-kw-var) (eq? op dce-kw-the-var))
           (let ((fqn (string-append (jolt-get node dce-kw-ns) "/" (jolt-get node dce-kw-name))))
             (if (dce-loader-fqn? fqn)
                 (cons dce-dynamic-load-ref (cons fqn acc))
                 (cons fqn acc))))
          ((and (eq? op dce-kw-ffi-fn) (dce-ffi-bare-varargs? node))
           (dce-reduce-children dce-collect-refs (cons dce-ffi-varargs-ref acc) node))
          ((and (eq? op dce-kw-invoke) (dce-invoke-fqn node) (dce-loader-fqn? (dce-invoke-fqn node)))
           ;; the callee var is counted as itself, never as a dynamic load; the
           ;; arguments decide that, and are walked for what they reference
           (let ((acc (cons (dce-invoke-fqn node)
                            (if (dce-dynamic-load? node) (cons dce-dynamic-load-ref acc) acc))))
             (let ((args (jolt-seq (jolt-get node dce-kw-args))))
               (if (jolt-nil? args)
                   acc
                   (fold-left dce-collect-refs acc (seq->list args))))))
          (else (dce-reduce-children dce-collect-refs acc node)))))

;; The fqn of a bare top-level def (the only prunable IR form), else #f.
(define (dce-def-fqn node)
  (and (eq? (jolt-get node dce-kw-op) dce-kw-def)
       (string-append (jolt-get node dce-kw-ns) "/" (jolt-get node dce-kw-name))))

;; Does this top-level def's init RUN code at load? A fn literal (every defn)
;; or a constant only binds; any other init -- a call, a let, an atom around a
;; fn -- executes when the namespace loads, so in a build that prunes nothing
;; it is a root of the compiler verdict (dce-needs-compiler?): what it calls,
;; and what it creates and may call later, are scanned. A defn whose BODY
;; evals is not one -- nothing runs until it is called, and the graph answers
;; whether it is.
(define dce-kw-init  (keyword #f "init"))
(define dce-kw-fn    (keyword #f "fn"))
(define dce-kw-const (keyword #f "const"))
(define (dce-def-init-runs? node)
  (and (eq? (jolt-get node dce-kw-op) dce-kw-def)
       (let ((init (jolt-get node dce-kw-init)))
         (and (not (jolt-nil? init))
              (let ((op (jolt-get init dce-kw-op)))
                (not (or (eq? op dce-kw-fn) (eq? op dce-kw-const))))))))

;; --- reference sets that gate the analysis ----------------------------------
;; A reference whose presence in reachable code forces keep-everything (the static
;; graph can't follow runtime name resolution).
(define dce-bail-refs
  '("clojure.core/eval" "clojure.core/resolve" "clojure.core/ns-resolve"
    "clojure.core/requiring-resolve" "clojure.core/find-var" "clojure.core/intern"
    "clojure.core/load-string" "clojure.core/load-file" "clojure.core/load-reader"
    "clojure.core/load"
    ;; Reflective enumeration — a program that walks a namespace's var table at
    ;; runtime via ns-publics/ns-interns/&c finds vars invisible to the static
    ;; IR graph, so any such reference bails the shake.
    "clojure.core/ns-publics" "clojure.core/ns-interns" "clojure.core/ns-map"
    "clojure.core/ns-refers" "clojure.core/all-ns" "clojure.core/ns-aliases"
    ;; Restoring an image COMPILES the fn sources it carries, and a source can
    ;; name any core var — one the compiled program never reached, because the
    ;; inline pass spliced it away at every site. A shaken build restored an
    ;; image whose closure source called `update` and died on an unbound var
    ;; that its own compiled -main had used without a trace. So an image
    ;; restore is a bail, for the same reason eval is: the static graph cannot
    ;; see what the restored code will reference. (Writing an image is not —
    ;; see dce-compile-refs.) Named by the HOST entry points: jolt.image's
    ;; wrappers are one-line defns the inline pass splices into their callers,
    ;; after which only the host call is left in the caller's refs — and an
    ;; unspliced wrapper still reaches the host call through its own record.
    "jolt.host/image-read" "jolt.host/image-restore-world!"
    ;; A require/use/load-libs by a COMPUTED name, or `compile`: a load of source
    ;; the graph never saw (dce-dynamic-load?, above).
    "jolt.host/load-namespace"))

;; A reference that needs the analyzer/back end at runtime (compile-from-source). If
;; reachable code uses none of these, the compiler image is dropped from the binary —
;; an AOT app is fully compiled. (resolve doesn't need it: it is a var-table
;; lookup; a require of a baked ns no-ops, and a require the build could not
;; bake is a dce-dynamic-load ref.)
;;
;; NOTE: every CORE entry in dce-compile-refs (eval, load-string, …) is also a
;; dce-bail-ref -- the static graph cannot track what the compiler will compile,
;; and eval'd code may reference any core def a shaken prelude would be
;; missing -- so reaching one keeps the compiler AND everything else. The
;; converse does not hold: the two lists answer two questions. Bail is "can the
;; shake trust the graph"; compile is "does this program compile at run time".
;; Writing an image needs the fasl writer in scheme.boot and shakes fine; a bare
;; varargs FFI binding compiles a foreign-procedure per tail shape and shakes
;; fine. A def :allow-dynamic vouches for is spared the bail for a RESOLUTION
;; ref and for a load by a COMPUTED name (jolt.host/load-namespace, the one
;; compile ref a vouch covers): a ref that runs the compiler on code bails
;; regardless, because the compiler image is direct-linked against the whole
;; core and cannot run over a shaken one (dce-bail-scan). drop-compiler? is (and (not bail) (not
;; needs-compiler)): a bail keeps the compiler too, since a requiring-resolve
;; may load and compile source at runtime.
(define dce-compile-refs
  '("clojure.core/eval" "clojure.core/load-string" "clojure.core/load-file"
    "clojure.core/load-reader" "clojure.core/load"
    ;; Images. Restoring one compiles the fn SOURCES it carries (state-image.ss
    ;; image-compile-eval-seam), so a program that reads one back needs the
    ;; compiler as surely as one that evals; and WRITING one goes through the
    ;; fasl writer ($write-fasl-bytevectors), which lives in scheme.boot — the
    ;; compiler kernel a dropped build boots without. Named by the HOST entry
    ;; points (see the image note in dce-bail-refs for why not the wrappers).
    ;; Every build decides now, not only a shaken one, so these are what keep
    ;; the compiler resident for an image-using program.
    "jolt.host/image-read" "jolt.host/image-write!"
    "jolt.host/image-dump-world!" "jolt.host/image-restore-world!"
    ;; Scheme text evaluated at run time (jolt.scheme/eval-string); proc is a
    ;; top-level lookup and lives in the runtime half, so it is not one.
    "jolt.host/scheme-eval-string"
    ;; the bare varargs FFI binding, see dce-collect-refs
    "jolt.host/ffi-varargs-compile"
    ;; a require/use/load-libs by a computed name, or `compile` -- a load from
    ;; source at run time, see dce-collect-refs
    "jolt.host/load-namespace"))

;; clojure.core fns the runtime .ss shims reference by name (via var-deref) — they
;; aren't visible in the IR call graph, so seed them as roots. (Found by grepping the
;; runtime shims; run-dce-refs.ss's core-root guard now keeps this list honest — a
;; new shim reference that isn't here fails that gate instead of shipping a prunable
;; var a tree-shaken app dereferences at runtime.) `send` is rooted because the STM
;; commit path (refs.ss) dispatches every queued agent send — including send-off /
;; send-via queued inside a dosync — through clojure.core/send, so an app that uses
;; send-off in a transaction roots send-off but not send in its own IR.
(define dce-runtime-core-roots
  '("clojure.core/identity" "clojure.core/isa?" "clojure.core/line-seq"
    "clojure.core/make-hierarchy" "clojure.core/read" "clojure.core/read-string"
    "clojure.core/read+string" "clojure.core/realized?" "clojure.core/reset!"
    ;; post-prelude's read-line wrapper routes a host reader in *in* to its own
    ;; readLine and closes over the overlay version for the reify reader
    "clojure.core/read-line"
    "clojure.core/send"
    ;; the LispReader$StringReader shim (host-static-methods.ss) pulls the literal
    ;; off the reader it is handed, a line at a time, through the IReader method
    "clojure.core/-read-line"
     ;; post-prelude taxonomy wrappers close over the overlay versions
     "clojure.core/ifn?" "clojure.core/seqable?" "clojure.core/inst-ms"
     ;; post-prelude's native sequential? closes over the overlay version for
     ;; the exotic-value fallback
     "clojure.core/sequential?"
     ;; …and its native counted? does the same for sorted colls, records and
     ;; deftypes that declare clojure.lang.Counted
     "clojure.core/counted?"
    ;; the fn print form wraps the overlay __print1
    "clojure.core/__print1"
    ;; the multimethod dispatch cache resolves the hierarchy value per call
    ;; (mm-current-hierarchy, multimethods.ss); mm-prefers? walks parents
    "clojure.core/global-hierarchy" "clojure.core/parents"
    ;; the readable printer consults print-method for a value with a user method
    ;; (io-streams.ss user-print-method), so it must survive tree-shaking
    "clojure.core/print-method"
    ;; find's call sites lower to jolt-find2 (host-table.ss), which answers for a
    ;; native map itself and resolves find-other by name for every other type — so
    ;; an app that only ever calls find has no IR edge to it
    "clojure.core/find-other"))

;; --- reading a minted blob (prelude.ss) into records ------------------------
;; The prelude is a flat list of (guard CLAUSE (def-var! "ns" "name" V)) forms (+ the
;; occasional side-effecting init). Read each with Chez `read` so it joins the graph
;; instead of being baked wholesale: a def-var! is a prunable node whose core->core
;; edges are the (var-deref/jolt-var "ns" "name") calls in V; any other form is
;; non-prunable (kept, refs are roots).
(define (dce-unwrap form)
  (if (and (pair? form) (eq? (car form) 'guard) (pair? (cddr form))) (caddr form) form))

;; The seed is minted direct-linked: a core def binds jv$<fqn> and a core->core
;; call is that SYMBOL, not a (var-deref "ns" "nm") literal. This maps each
;; such symbol to its "ns/name", filled by dce-blob-records from the
;; (def-var-linked! "ns" "nm" 'jv$… …) forms before any record's refs are
;; read, so a direct call is an edge exactly as a var-routed one is. Process
;; global, like the records it describes; refilled on every read of the blob.
(define dce-linked-syms (make-eq-hashtable))
(define (dce-linked-fqn x)
  (and (symbol? x) (hashtable-ref dce-linked-syms x #f)))
(define (dce-note-linked! form)
  (when (and (pair? form) (eq? (car form) 'def-var-linked!)
             (pair? (cdr form)) (string? (cadr form))
             (pair? (cddr form)) (string? (caddr form))
             (pair? (cdddr form)) (pair? (cadddr form))
             (eq? (car (cadddr form)) 'quote) (pair? (cdr (cadddr form)))
             (symbol? (cadr (cadddr form))))
    (hashtable-set! dce-linked-syms (cadr (cadddr form))
                    (string-append (cadr form) "/" (caddr form)))))

;; "ns/name" of every (var-deref "ns" "nm"), (jolt-var "ns" "nm"), or
;; (var-cell-lookup "ns" "nm") literal in a read form, and of every linked jv$
;; symbol, inserted into ht.
(define (dce-sexp-refs-into! form ht)
  (cond
    ((and (pair? form) (memq (car form) '(var-deref jolt-var var-cell-lookup))
          (pair? (cdr form)) (string? (cadr form)) (pair? (cddr form)) (string? (caddr form)))
     (hashtable-set! ht (string-append (cadr form) "/" (caddr form)) #t))
    ((pair? form)
     (dce-sexp-refs-into! (car form) ht)
     (dce-sexp-refs-into! (cdr form) ht))
    ((dce-linked-fqn form) => (lambda (fqn) (hashtable-set! ht fqn #t)))))

;; List-accumulating variant, still live: the round-trip scan below (and the
;; run-dce-refs gate) consume it. dce-sexp-refs-into! is the hash-set fast path.
(define (dce-sexp-refs form acc)
  (cond
    ((and (pair? form) (memq (car form) '(var-deref jolt-var var-cell-lookup))
          (pair? (cdr form)) (string? (cadr form)) (pair? (cddr form)) (string? (caddr form)))
     (cons (string-append (cadr form) "/" (caddr form)) acc))
    ((pair? form) (dce-sexp-refs (cdr form) (dce-sexp-refs (car form) acc)))
    ((dce-linked-fqn form) => (lambda (fqn) (cons fqn acc)))
    (else acc)))

;; All "ns/name" refs a text scan of an emitted Scheme string carries, deduped via
;; hash set. Read every top-level form and fold dce-sexp-refs-into!.
(define (dce-sexp-refs-str str)
  (let ((p (open-input-string str))
        (ht (make-hashtable string-hash string=?)))
    (let loop ()
      (let ((form (read p)))
        (unless (eof-object? form)
          (dce-sexp-refs-into! form ht)
          (loop))))
    (vector->list (hashtable-keys ht))))

;; Refs an app record roots: the IR walk (every :var/:the-var node) UNIONED with the
;; text scan of its emitted Scheme. The IR walk is structural truth for compiled
;; Clojure; the text scan defends a var-deref the back end emits outside a :var node
;; (a macro splicing raw scheme, or a future emit path) that the IR walk would miss
;; but the prelude's text scan catches.
(define (dce-app-refs ir str)
  (append (dce-collect-refs '() ir) (dce-sexp-refs-str str)))

;; The (def-var! "ns" "name" …) / (def-var-with-meta! …) form a prelude record
;; defines, or #f for a non-def form. A def whose value holds anonymous fn
;; literals is minted as (begin (image-register-fn-form! ...)... (def-var...)) --
;; one source registration per literal first, then the def; a literal with no
;; source rendering keeps its quoted construction, a (let* <quote pool> ...)
;; around the registrations -- so look through exactly those shapes. Read as a non-def form, every such def (138 of the 685
;; prelude defs) was an unprunable root, and two of them, clojure.repl/find-doc
;; and apropos, reference all-ns, ns-interns and ns-publics: every --tree-shake
;; build bailed from the commit that made core's literals register (7d11cfed,
;; 0.7.29), whatever the app did.
;; Any other begin (a defrecord's several defs, a def-var-plain! group) stays a
;; keep form as before: a record carries one fqn, and pruning several defs
;; under one of their names is unsound.
;; A direct-linked seed def (bootstrap.ss) is minted as
;;   (begin [(image-register-fn-form! ...)... | (let* <quote pool> (image-register-fn-form! ...)...)]
;;          (define jv$ns$name <init>)
;;          (def-var-linked! "ns" "name" 'jv$ns$name jv$ns$name (lambda (v) (set! jv$ns$name v)) meta)
;;          [(jolt-register-variadic! n jv$ns$name)])
;; — the same single-fqn def, so it prunes under that name too. The walk below
;; admits exactly these siblings, in this order; any other begin stays a keep form.
(define (dce-def-var-form b)
  (define (def-var-form? x)
    (and (pair? x) (memq (car x) '(def-var! def-var-with-meta! def-var-plain! def-var-linked!))
         (pair? (cdr x)) (string? (cadr x))
         (pair? (cddr x)) (string? (caddr x))))
  (define (headed? x head) (and (pair? x) (eq? (car x) head)))
  (define (linked-define? x)
    (and (headed? x 'define) (pair? (cdr x)) (symbol? (cadr x))
         (let ((s (symbol->string (cadr x))))
           (and (fx>= (string-length s) 3) (string=? (substring s 0 3) "jv$")))))
  (define (trailing-ok? xs)
    (or (null? xs)
        (and (headed? (car xs) 'jolt-register-variadic!) (trailing-ok? (cdr xs)))))
  (cond
    ((def-var-form? b) b)
    ((headed? b 'begin)
     (let* ((xs (cdr b))
            (xs (let skip ((xs xs))
                  (if (and (pair? xs)
                           (or (headed? (car xs) 'image-register-fn-form!)
                               (headed? (car xs) 'let*)))
                      (skip (cdr xs))
                      xs)))
            (xs (if (and (pair? xs) (linked-define? (car xs))) (cdr xs) xs)))
       (and (pair? xs) (def-var-form? (car xs)) (trailing-ok? (cdr xs))
            (car xs))))
    (else #f)))

;; str re-serializes the read form (compiled identically; comments/whitespace are
;; irrelevant).
(define (dce-blob-records path)
  ;; bld-source-string (build.ss) reads the embedded copy when running from a
  ;; self-contained jolt, else the file on disk — so tree-shake works with no
  ;; jolt checkout present. Forward ref: build.ss loads after this file.
  (call-with-port (open-input-string (bld-source-string path))
    (lambda (p)
      ;; two passes: every linked symbol is known before any record's refs are
      ;; read, or a call to a def minted LATER in the blob would be no edge
      (let ((forms (let rd ((acc '()))
                     (let ((form (read p)))
                       (if (eof-object? form) (reverse acc) (rd (cons form acc)))))))
        (hashtable-clear! dce-linked-syms)
        (for-each (lambda (form)
                    (let ((b (dce-unwrap form)))
                      (when (pair? b) (for-each dce-note-linked! (if (eq? (car b) 'begin) (cdr b) (list b))))))
                  forms)
      (let loop ((acc '()) (forms forms))
        (let ((form (if (null? forms) (eof-object) (car forms))))
          (if (eof-object? form)
              (reverse acc)
              (let ((b (dce-unwrap form))
                    (str (with-output-to-string (lambda () (write form))))
                    (refs (dce-sexp-refs form '())))
                ;; the shaken prelude is this re-serialization — a datum that
                ;; does not round-trip (shared structure, unwritable value)
                ;; would silently corrupt clojure.core, so prove it reads back
                ;; identical before using it.
                (unless (equal? form (with-input-from-string str read))
                  (error 'jolt-build
                         "tree-shake: a prelude form does not round-trip through write/read"
                         (if (pair? form) (car form) form)))
                (loop (cons
                         (let ((d (dce-def-var-form b)))
                           (if d
                               (dce-rec #f (string-append (cadr d) "/" (caddr d)) refs str)
                               (dce-rec #t #f refs str)))
                        acc)
                      (cdr forms))))))))))

;; A reader fn reached ONLY via runtime (read-string "#my/tag ..") resolves through
;; *data-readers* var-deref — invisible to the IR graph. The baked *data-readers* map
;; is the source of truth: it carries every reader-fn symbol whether registered via a
;; data_readers.{clj,cljc} file OR programmatically (alter-var-root), so every symbol
;; in the live map is a root. The source scan below additionally roots a reader whose
;; ns failed to load (its symbol still in data_readers.clj, unresolved at bake time).
(define (dce-reader-sym-roots tbl roots)
  (if (not (pmap? tbl)) roots
      (pmap-fold tbl
        (lambda (k v a)
          (if (symbol-t? v)
              (let ((ns-part (symbol-t-ns v)) (nm-part (symbol-t-name v)))
                (if (and ns-part (not (jolt-nil? ns-part)) nm-part)
                    (cons (string-append ns-part "/" nm-part) a)
                    a))
              a))
        roots)))

(define (dce-data-reader-roots)
  (let ((roots (dce-reader-sym-roots (var-deref "clojure.core" "*data-readers*") '())))
    (for-each
      (lambda (root)
        (let ((paths (map (lambda (e) (string-append root "/data_readers" e))
                          ldr-source-exts)))
          (for-each (lambda (path)
            (when (file-exists? path)
              (let ((src (read-file-string path)))
                (guard (e (#t #f))
                  (let-values (((m j) (rdr-read-form src 0 (string-length src))))
                    (when (pmap? m)
                      (set! roots (dce-reader-sym-roots m roots))))))))
            paths)))
      (get-source-roots))
    roots))

;; --- the shake: graph -> reachable -> bail check -> partition ----------------
;; edges: fqn -> refs (prunable defs only). roots: -main + the runtime-core roots +
;; every non-def form's refs.
;; A callee the inline pass spliced is KEPT even when nothing calls it any more.
;; Splicing removes the last reference to a fn whose every call site was inlined,
;; so the graph walk below would prune its def -- and the def's record carries the
;; (jolt-register-source! …) that maps an inlined frame back to ns/name
;; (file:line). Without this a --tree-shake binary printed one frame where the
;; unshaken build printed three (jolt-o13s). The kept def is bounded by the inline
;; budget, so the size this costs is small and the alternative is a trace that
;; silently loses frames the same build shows without --tree-shake.
;;
;; Kept, but not a ROOT of the bail scan. A spliced callee that no remaining
;; reference reaches is code that never runs: its call sites are all copies now.
;; Rooting it treated its references as reachable code, so a helper the inline
;; pass had spliced — core.async's go-macro walkers, which call `resolve` while
;; expanding a go body — bailed the shake of a program that never expands a go
;; form. dce-build-graph therefore returns the spliced set apart from the roots:
;; dce-shake closes over roots alone for the bail scan, and over roots plus the
;; spliced set for what the binary keeps, so a kept callee's load-time var
;; lookups still find every def they name.
;; inline-spliced-fqns is host-contract.ss; loaded well before build.ss loads this.
(define (dce-build-graph records entry-main)
  (let ((edges (make-hashtable string-hash string=?))
        (roots (append (dce-data-reader-roots)
                       (cons entry-main dce-runtime-core-roots))))
    (for-each (lambda (r)
                (if (dce-rec-keep? r)
                    (set! roots (append (dce-rec-refs r) roots))
                    (hashtable-update! edges (dce-rec-fqn r)
                      (lambda (old) (append (dce-rec-refs r) old))
                      '())))
              records)
    (values edges roots (inline-spliced-fqns))))

;; Closure of roots over edges -> a reached set (hashtable fqn -> #t). The append
;; copies only the visited node's OWN edge list and shares (cdr work) — append
;; copies every argument but its last — so the walk is O(V+E) total. New work
;; goes in front, so the order is depth-first; order is irrelevant to the closure.
;; Linearity is pinned by the scaling check in run-dce-refs.ss.
(define (dce-reachable edges roots)
  (let ((reached (make-hashtable string-hash string=?)))
    (let dfs ((work roots))
      (unless (null? work)
        (let ((fq (car work)))
          (if (hashtable-ref reached fq #f)
              (dfs (cdr work))
              (begin (hashtable-set! reached fq #t)
                     (dfs (append (or (hashtable-ref edges fq #f) '()) (cdr work))))))))
    reached))

(define (dce-rec-reached? r reached)
  (or (dce-rec-keep? r) (hashtable-ref reached (dce-rec-fqn r) #f)))

;; Scan the KEPT records: does any resolve a var at runtime (bail), and does any need
;; the compiler? Returns (values bail? bail-why bail-hint needs-compiler?). bail-why
;; is up to 6 DISTINCT (def . bail-ref) pairs for the diagnostic — distinct because
;; dce-app-refs unions an IR walk with a text scan, so one call is usually two refs,
;; and each line printed twice. bail-hint is every distinct def that bailed
;; (uncapped, first-seen order) for the paste-ready key. Uses hash sets for O(1)
;; membership instead of O(n*m) linear scans over the lists.
;;
;; allow: "ns/name" strings from deps.edn :jolt/tree-shake {:allow-dynamic […]} —
;; callers pass the union of the app's and every library's; a list, '() when
;; nothing is declared, never #f. A def in the set is skipped by BOTH scans: the
;; author is asserting its lookup never runs in the built binary (or names only
;; vars the graph keeps anyway), and a site that never runs needs no compiler
;; either. Nothing is kept on an allowed def's behalf — it enters the graph
;; exactly as before, and the scan is the only place the set is consulted. A
;; top-level non-def form has no fqn and cannot be allowed by name. The fqn to
;; allow is the def the ref ended up IN — after the inline pass that is the
;; caller of a spliced helper, not the helper — which is why the hint prints the
;; name rather than leaving it to the reader to derive.
(define (dce-bail-scan records reached allow)
  (let ((bail #f) (why '()) (hint '()) (needs-compiler #f)
        (bail-ht (make-hashtable string-hash string=?))
        (compile-ht (make-hashtable string-hash string=?))
        (allow-ht (make-hashtable string-hash string=?)))
    (for-each (lambda (b) (hashtable-set! bail-ht b #t)) dce-bail-refs)
    (for-each (lambda (c) (hashtable-set! compile-ht c #t)) dce-compile-refs)
    (for-each (lambda (a) (hashtable-set! allow-ht a #t)) allow)
    (for-each
      (lambda (r)
        (let ((fqn (dce-rec-fqn r)))
          (when (dce-rec-reached? r reached)
            ;; :allow-dynamic vouches for a RESOLUTION the graph cannot follow
            ;; (resolve, requiring-resolve, ns-publics ...) and for a load by a
            ;; COMPUTED name (jolt.host/load-namespace): both are the one
            ;; assertion that the site never runs in the binary, or names only
            ;; what the build baked, and spec.gen's dynaload is exactly that
            ;; pair in one def -- a vouch that covered its resolve but not its
            ;; require bailed again on the key its own hint had printed. It
            ;; cannot vouch for a ref that RUNS the compiler on code -- eval,
            ;; load-string, an image restore -- because the compiler image is
            ;; direct-linked against the whole core and cannot run over a
            ;; shaken one (an allowed eval used to shake, and died on a pruned
            ;; core def inside the compiler). So an allowed def bails on those
            ;; still, and a def whose bail includes one gets no hint at all,
            ;; whatever else it references: a key that names it would not
            ;; proceed.
            (let* ((allowed? (and fqn (hashtable-ref allow-ht fqn #f)))
                   (refs (dce-rec-refs r))
                   ;; a bail ref no vouch covers
                   (blocks? (lambda (ref)
                              (and (hashtable-ref bail-ht ref #f)
                                   (hashtable-ref compile-ht ref #f)
                                   (not (string=? ref dce-dynamic-load-ref)))))
                   (hintable? (and fqn (not (ormap blocks? refs)))))
              (for-each (lambda (ref)
                          (when (and (hashtable-ref bail-ht ref #f)
                                     (or (not allowed?) (blocks? ref)))
                            (set! bail #t)
                            (let ((pair (cons (or fqn "<form>") ref)))
                              (when (and (< (length why) 6) (not (member pair why)))
                                (set! why (cons pair why))))
                            (when (and hintable? (not (member fqn hint)))
                              (set! hint (cons fqn hint)))))
                        refs)
              ;; the vouched load is the one compile ref a vouch spares: a site
              ;; that never runs needs no compiler. Every other compile ref
              ;; keeps it, vouched or not.
              (when (ormap (lambda (ref)
                             (and (hashtable-ref compile-ht ref #f)
                                  (not (and allowed? (string=? ref dce-dynamic-load-ref)))))
                           refs)
                (set! needs-compiler #t))))))
      records)
    (values bail (reverse why) (reverse hint) needs-compiler)))

;; Kept records -> (values kept-strings n-defs n-kept-defs).
(define (dce-partition records reached)
  (let loop ((rs records) (acc '()) (n 0) (k 0))
    (if (null? rs)
        (values (reverse acc) n k)
        (let* ((r (car rs)) (isdef (and (dce-rec-fqn r) #t)))
          (if (dce-rec-reached? r reached)
              (loop (cdr rs) (cons (dce-rec-str r) acc) (if isdef (+ n 1) n) (if isdef (+ k 1) k))
              (loop (cdr rs) acc (if isdef (+ n 1) n) k))))))

;; The compiler verdict ALONE, for the build that keeps every def (the
;; default — no --tree-shake): the same graph, the same reach and the same bail
;; scan as dce-shake, and none of its partitioning. A program that never
;; reaches eval, load-string or an image restore ships without the analyzer
;; and back end (1.2MB of fasl, the scheme.boot compiler kernel and their
;; load), and one that does keeps them; a bail keeps them too, for the reason
;; dce-compile-refs gives. Nothing is printed: no shake was asked for, so a
;; bail is not a skipped shake here, it is simply a program that needs its
;; compiler.
(define (dce-app-init-roots records)
  (filter (lambda (x) x)
          (map (lambda (r) (and (dce-rec-init-runs? r) (dce-rec-fqn r))) records)))
(define (dce-needs-compiler? core-records app-records entry-main allow)
  (let ((all (append core-records app-records)))
    (let-values (((edges roots spliced) (dce-build-graph all entry-main)))
      ;; Nothing is pruned in this build, so every app def whose init RUNS at
      ;; load is a root, not only what -main reaches: an unreferenced
      ;; (def x (eval ...)) needs the compiler as surely as -main calling it,
      ;; and rooted at -main alone the binary booted from petite and died in
      ;; that def's init. A defn only binds (dce-def-init-runs?), so a library
      ;; that DEFINES an eval-calling fn nobody reaches drops it still.
      (let ((reached (dce-reachable edges (append (dce-app-init-roots app-records) roots))))
        (let-values (((bail why hint needs-compiler) (dce-bail-scan all reached allow)))
          (or bail needs-compiler))))))

;; Returns (values core-strs app-strs drop-compiler?). core-strs is #f on a bail,
;; signalling "inline prelude.ss unshaken" + keep the compiler. allow: see
;; dce-bail-scan. On a bail the diagnostic ends with the deps.edn key that would
;; allow every def it named, so the path from "skipped" to "kept" is one paste.
(define (dce-shake core-records app-records entry-main allow)
  (let-values (((edges roots spliced)
                (dce-build-graph (append core-records app-records) entry-main)))
    (let* ((reached (dce-reachable edges roots))
           ;; what the binary keeps: the reachable code, plus the spliced
           ;; callees kept for frame identity closed over what they reference
           (kept (if (null? spliced)
                     reached
                     (dce-reachable edges (append spliced roots)))))
      (let-values (((bail why hint needs-compiler)
                    (dce-bail-scan (append core-records app-records) reached allow)))
        (let ((drop-compiler? (and (not bail) (not needs-compiler))))
          (if bail
              (begin
                (display "jolt build: tree-shake skipped (reachable code resolves vars at runtime):\n")
                (for-each (lambda (w) (display (string-append "  " (car w) " -> " (cdr w) "\n"))) why)
                (unless (null? hint)
                  (display "to proceed, if these never run in the built binary, add to deps.edn:\n")
                  (display (string-append "  :jolt/tree-shake {:allow-dynamic [" (jolt-str-join hint) "]}\n")))
                (values #f (map dce-rec-str app-records) drop-compiler?))
              (let-values (((core-strs cn ck) (dce-partition core-records kept))
                           ((app-strs an ak) (dce-partition app-records kept)))
                (display (string-append "jolt build: tree-shake kept " (number->string (+ ck ak))
                                        " of " (number->string (+ cn an)) " defs (core "
                                        (number->string ck) "/" (number->string cn) ")\n"))
                (values core-strs app-strs drop-compiler?))))))))
