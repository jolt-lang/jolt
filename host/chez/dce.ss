;; dce.ss — tree-shaking (jolt build --tree-shake): whole-program reachability DCE.
;;
;; Build one call graph over the re-emitted app + libraries AND the clojure.core
;; prelude, keep -main + every side-effecting top-level form + everything reachable
;; from those, drop the rest. Bails (keeps everything) if reachable code resolves a
;; var by name at runtime (eval/resolve/...), which a static graph can't follow. Per
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
    "clojure.core/send"
    ;; post-prelude taxonomy wrappers close over the overlay versions
    "clojure.core/ifn?" "clojure.core/seqable?" "clojure.core/inst-ms"
    ;; the fn print form wraps the overlay __print1
    "clojure.core/__print1"))

;; --- reading a minted blob (prelude.ss) into records ------------------------
;; The prelude is a flat list of (guard CLAUSE (def-var! "ns" "name" V)) forms (+ the
;; occasional side-effecting init). Read each with Chez `read` so it joins the graph
;; instead of being baked wholesale: a def-var! is a prunable node whose core->core
;; edges are the (var-deref/jolt-var "ns" "name") calls in V; any other form is
;; non-prunable (kept, refs are roots).
(define (dce-unwrap form)
  (if (and (pair? form) (eq? (car form) 'guard) (pair? (cddr form))) (caddr form) form))

(define (dce-sexp-refs form acc)
  (cond
    ((and (pair? form) (memq (car form) '(var-deref jolt-var))
          (pair? (cdr form)) (string? (cadr form)) (pair? (cddr form)) (string? (caddr form)))
     (cons (string-append (cadr form) "/" (caddr form)) acc))
    ((pair? form) (dce-sexp-refs (cdr form) (dce-sexp-refs (car form) acc)))
    (else acc)))

;; All "ns/name" refs a text scan of an emitted Scheme string carries — read every
;; top-level form and fold dce-sexp-refs. Mirrors how dce-blob-records scans the
;; minted prelude; run over emitted app strings so a literal (var-deref "ns" "nm")
;; spliced into an emitted form (with no :var IR node) still roots its target.
(define (dce-sexp-refs-str str)
  (let ((p (open-input-string str)))
    (let loop ((acc '()))
      (let ((form (read p)))
        (if (eof-object? form) acc (loop (dce-sexp-refs form acc)))))))

;; Refs an app record roots: the IR walk (every :var/:the-var node) UNIONED with the
;; text scan of its emitted Scheme. The IR walk is structural truth for compiled
;; Clojure; the text scan defends a var-deref the back end emits outside a :var node
;; (a macro splicing raw scheme, or a future emit path) that the IR walk would miss
;; but the prelude's text scan catches.
(define (dce-app-refs ir str)
  (append (dce-collect-refs '() ir) (dce-sexp-refs-str str)))

;; str re-serializes the read form (compiled identically; comments/whitespace are
;; irrelevant).
(define (dce-blob-records path)
  ;; bld-source-string (build.ss) reads the embedded copy when running from a
  ;; self-contained joltc, else the file on disk — so tree-shake works with no
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
                        (if (and (pair? b) (eq? (car b) 'def-var!) (pair? (cdr b)) (string? (cadr b))
                                 (pair? (cddr b)) (string? (caddr b)))
                            (dce-rec #f (string-append (cadr b) "/" (caddr b)) refs str)
                            (dce-rec #t #f refs str))
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
        (let ((paths (list (string-append root "/data_readers.clj")
                           (string-append root "/data_readers.cljc"))))
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
(define (dce-build-graph records entry-main)
  (let ((edges (make-hashtable string-hash string=?))
        (roots (append (dce-data-reader-roots)
                       (cons entry-main dce-runtime-core-roots))))
    (for-each (lambda (r)
                (if (dce-rec-keep? r)
                    (set! roots (append (dce-rec-refs r) roots))
                    (hashtable-set! edges (dce-rec-fqn r) (dce-rec-refs r))))
              records)
    (values edges roots)))

;; Closure of roots over edges -> a reached set (hashtable fqn -> #t).
(define (dce-reachable edges roots)
  (let ((reached (make-hashtable string-hash string=?)))
    (let bfs ((work roots))
      (unless (null? work)
        (let ((fq (car work)))
          (if (hashtable-ref reached fq #f)
              (bfs (cdr work))
              (begin (hashtable-set! reached fq #t)
                     (bfs (append (or (hashtable-ref edges fq #f) '()) (cdr work))))))))
    reached))

(define (dce-rec-reached? r reached)
  (or (dce-rec-keep? r) (hashtable-ref reached (dce-rec-fqn r) #f)))

;; Scan the KEPT records: does any resolve a var at runtime (bail), and does any need
;; the compiler? Returns (values bail? bail-why needs-compiler?). bail-why is up to 6
;; (def . bail-ref) pairs for the diagnostic.
(define (dce-bail-scan records reached)
  (let ((bail #f) (why '()) (needs-compiler #f))
    (for-each
      (lambda (r)
        (when (dce-rec-reached? r reached)
          (for-each (lambda (b)
                      (when (member b (dce-rec-refs r))
                        (set! bail #t)
                        (when (< (length why) 6)
                          (set! why (cons (cons (or (dce-rec-fqn r) "<form>") b) why)))))
                    dce-bail-refs)
          (when (ormap (lambda (c) (and (member c (dce-rec-refs r)) #t)) dce-compile-refs)
            (set! needs-compiler #t))))
      records)
    (values bail (reverse why) needs-compiler)))

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
;; signalling "inline prelude.ss unshaken" + keep the compiler.
(define (dce-shake core-records app-records entry-main)
  (let-values (((edges roots) (dce-build-graph (append core-records app-records) entry-main)))
    (let* ((reached (dce-reachable edges roots)))
      (let-values (((bail why needs-compiler) (dce-bail-scan (append core-records app-records) reached)))
        (let ((drop-compiler? (and (not bail) (not needs-compiler))))
          (if bail
              (begin
                (display "jolt build: tree-shake skipped (reachable code resolves vars at runtime):\n")
                (for-each (lambda (w) (display (string-append "  " (car w) " -> " (cdr w) "\n"))) why)
                (values #f (map dce-rec-str app-records) drop-compiler?))
              (let-values (((core-strs cn ck) (dce-partition core-records reached))
                           ((app-strs an ak) (dce-partition app-records reached)))
                (display (string-append "jolt build: tree-shake kept " (number->string (+ ck ak))
                                        " of " (number->string (+ cn an)) " defs (core "
                                        (number->string ck) "/" (number->string cn) ")\n"))
                (values core-strs app-strs drop-compiler?))))))))
