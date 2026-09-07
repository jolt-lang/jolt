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
(define (dce-rec keep? fqn refs str) (vector keep? fqn refs str))
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
(define (dce-collect-refs acc node)
  (let ((op (jolt-get node dce-kw-op)))
    (if (or (eq? op dce-kw-var) (eq? op dce-kw-the-var))
        (cons (string-append (jolt-get node dce-kw-ns) "/" (jolt-get node dce-kw-name)) acc)
        (dce-reduce-children dce-collect-refs acc node))))

;; The fqn of a bare top-level def (the only prunable IR form), else #f.
(define (dce-def-fqn node)
  (and (eq? (jolt-get node dce-kw-op) dce-kw-def)
       (string-append (jolt-get node dce-kw-ns) "/" (jolt-get node dce-kw-name))))

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
    "clojure.core/ns-refers" "clojure.core/all-ns" "clojure.core/ns-aliases"))

;; A reference that needs the analyzer/back end at runtime (compile-from-source). If
;; reachable code uses none of these, the compiler image is dropped from the binary —
;; an AOT app is fully compiled. (resolve/require don't need it: resolve is a
;; var-table lookup; a require of a baked ns no-ops.)
;;
;; NOTE: dce-compile-refs is a SUBSET of dce-bail-refs — every form needing the
;; compiler at runtime (eval, load-string/…) also bails the tree-shake because the
;; static graph can't track what the compiler will compile. So a successful shake
;; (no bail) always drops the compiler, and ANY bail keeps it (drop-compiler? is
;; (and (not bail) (not needs-compiler)) in dce-shake) — conservative on purpose:
;; a requiring-resolve bail may load and compile source at runtime. The subset
;; relationship is load-bearing: eval'd code might reference any core def, and a
;; shaken prelude would be missing some — bail keeps everything and the compiler.
(define dce-compile-refs
  '("clojure.core/eval" "clojure.core/load-string" "clojure.core/load-file"
    "clojure.core/load-reader" "clojure.core/load"))

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

;; "ns/name" of every (var-deref "ns" "nm"), (jolt-var "ns" "nm"), or
;; (var-cell-lookup "ns" "nm") literal in a read form, inserted into ht.
(define (dce-sexp-refs-into! form ht)
  (cond
    ((and (pair? form) (memq (car form) '(var-deref jolt-var var-cell-lookup))
          (pair? (cdr form)) (string? (cadr form)) (pair? (cddr form)) (string? (caddr form)))
     (hashtable-set! ht (string-append (cadr form) "/" (caddr form)) #t))
    ((pair? form)
     (dce-sexp-refs-into! (car form) ht)
     (dce-sexp-refs-into! (cdr form) ht))))

;; List-accumulating variant, still live: the round-trip scan below (and the
;; run-dce-refs gate) consume it. dce-sexp-refs-into! is the hash-set fast path.
(define (dce-sexp-refs form acc)
  (cond
    ((and (pair? form) (memq (car form) '(var-deref jolt-var var-cell-lookup))
          (pair? (cdr form)) (string? (cadr form)) (pair? (cddr form)) (string? (caddr form)))
     (cons (string-append (cadr form) "/" (caddr form)) acc))
    ((pair? form) (dce-sexp-refs (cdr form) (dce-sexp-refs (car form) acc)))
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
;; defines, or #f for a non-def form. A def whose value holds an anonymous fn
;; literal is minted as (begin (let* <quote pool> (image-register-fn-form! …)…)
;; (def-var…)) — the source registration first, then the def — so look through
;; exactly that shape. Read as a non-def form, every such def (138 of the 685
;; prelude defs) was an unprunable root, and two of them, clojure.repl/find-doc
;; and apropos, reference all-ns, ns-interns and ns-publics: every --tree-shake
;; build bailed from the commit that made core's literals register (7d11cfed,
;; 0.7.29), whatever the app did.
;; Any other begin (a defrecord's several defs, a def-var-plain! group) stays a
;; keep form as before: a record carries one fqn, and pruning several defs
;; under one of their names is unsound.
(define (dce-def-var-form b)
  (define (def-var-form? x)
    (and (pair? x) (memq (car x) '(def-var! def-var-with-meta!))
         (pair? (cdr x)) (string? (cadr x))
         (pair? (cddr x)) (string? (caddr x))))
  (cond
    ((def-var-form? b) b)
    ((and (pair? b) (eq? (car b) 'begin)
          (pair? (cdr b)) (pair? (cadr b)) (eq? (car (cadr b)) 'let*)
          (pair? (cddr b)) (null? (cdddr b))
          (def-var-form? (caddr b)))
     (caddr b))
    (else #f)))

;; str re-serializes the read form (compiled identically; comments/whitespace are
;; irrelevant).
(define (dce-blob-records path)
  ;; bld-source-string (build.ss) reads the embedded copy when running from a
  ;; self-contained jolt, else the file on disk — so tree-shake works with no
  ;; jolt checkout present. Forward ref: build.ss loads after this file.
  (call-with-port (open-input-string (bld-source-string path))
    (lambda (p)
      (let loop ((acc '()))
        (let ((form (read p)))
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
                        acc)))))))))

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
          (when (and (dce-rec-reached? r reached)
                     (not (and fqn (hashtable-ref allow-ht fqn #f))))
            (for-each (lambda (ref)
                        (when (hashtable-ref bail-ht ref #f)
                          (set! bail #t)
                          (let ((pair (cons (or fqn "<form>") ref)))
                            (when (and (< (length why) 6) (not (member pair why)))
                              (set! why (cons pair why))))
                          (when (and fqn (not (member fqn hint)))
                            (set! hint (cons fqn hint)))))
                      (dce-rec-refs r))
            (when (ormap (lambda (ref) (and (hashtable-ref compile-ht ref #f) #t)) (dce-rec-refs r))
              (set! needs-compiler #t)))))
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
