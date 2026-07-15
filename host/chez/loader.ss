;; loader.ss — file-based namespace loading + a shell primitive.
;;
;; The corpus/CLI spine compiles one program at a time; namespaces declared in
;; that program see each other because a top-level (do …) unrolls. A real project
;; spans many FILES, so `require` must locate a namespace's source on the search
;; roots and load it — transitively, once each.
;;
;; Loaded by cli.ss AFTER compile-eval.ss (it calls jolt-compile-eval-form). The
;; gates load compile-eval.ss but NOT this file, so the corpus/unit/sci runners
;; keep their alias-only `require` and are unaffected.

;; --- search roots -----------------------------------------------------------
;; An ordered list of directory strings. `require` searches them left to right.
;; The CLI seeds this with the project's resolved deps roots (jolt.deps) plus the
;; jolt-core roots so jolt.main/jolt.deps themselves load.
(define source-roots '("."))
(define (set-source-roots! roots)
  (set! source-roots roots)
  (load-data-readers!))   ; scan every entry point (cli startup + jolt.host wrapper)
;; Roots setter that does NOT re-scan data_readers — for an emitted binary's
;; prologue/launcher, where *data-readers* and the reader namespaces are already
;; baked (bld-emit-data-readers + the app ns walk). Re-scanning would eagerly
;; reload each reader namespace via load-jolt-file/jolt-compile-eval-form, which a
;; tree-shaken no-eval binary has dropped, crashing startup. joltc/build entry
;; points keep the scanning set-source-roots!.
(define (set-source-roots!* roots) (set! source-roots roots))
(define (get-source-roots) source-roots)

;; Install roots — the directories that ship with Jolt (compiler + stdlib + vendored
;; deps). Shared by cli.ss, build-joltc.ss, and the bld-require-closure filter so the
;; literal list stays in one place.
(define ldr-install-roots '("jolt-core" "stdlib" "vendor/fs/src"))

;; True when `f` is a file owned by the Jolt runtime (compiler + stdlib) — either
;; an embedded-resource key or a path under one of ldr-install-roots.
(define (ldr-install-file? f)
  (or (string? (hashtable-ref embedded-resources f #f))
      (let loop ((roots ldr-install-roots))
        (and (pair? roots)
             (or (let ((root (car roots)))
                   (and (>= (string-length f) (+ (string-length root) 1))
                        (string=? (substring f 0 (string-length root)) root)
                        (char=? (string-ref f (string-length root)) #\/)))
                 (loop (cdr roots)))))))

;; A Chez source string for the install roots list — "(list \"jolt-core\" \"stdlib\" \"vendor/fs/src\")".
;; Used by build templates so the literal stays in one place.
(define (ldr-install-roots-str)
  (string-append "(list"
    (fold-left (lambda (s r) (string-append s " \"" r "\"")) "" ldr-install-roots)
    ")"))

;; --- data readers (#tag literals) -------------------------------------------
;; A project's data_readers.{clj,cljc} at a source root maps a tag symbol to a
;; qualified reader fn (e.g. {time/date time-literals.data-readers/date}). We
;; merge those into clojure.core/*data-readers* and require each reader's
;; namespace, then while loading source rewrite a registered #tag form into a
;; call (reader-fn 'inner-form) so the value is built at runtime. #inst/#uuid and
;; #"regex" stay built-in (the analyzer lowers them); only tags present in
;; *data-readers* are rewritten. data-readers-active gates the per-form walk so
;; projects without data readers (the common case) pay nothing.
(define data-readers-active #f)
(define (data-readers-table) (var-deref "clojure.core" "*data-readers*"))
;; tag keyword (:#time/date) -> its registered reader symbol, or #f.
(define (data-reader-symbol tag)
  (and (keyword? tag)
       (let ((nm (keyword-t-name tag)))
         (and (> (string-length nm) 0) (char=? (string-ref nm 0) #\#)
              (let* ((bare (substring nm 1 (string-length nm)))
                     (slash (let loop ((i 0))
                              (cond ((>= i (string-length bare)) #f)
                                    ((char=? (string-ref bare i) #\/) i)
                                    (else (loop (+ i 1))))))
                     (sym (if slash
                              (jolt-symbol (substring bare 0 slash) (substring bare (+ slash 1) (string-length bare)))
                              (jolt-symbol #f bare)))
                     (t (data-readers-table))
                     (v (and (pmap? t) (jolt-get t sym))))
                 (and v (not (jolt-nil? v)) v))))))
;; Invoke a resolved data-reader fn at load time, letting a throw surface as an
;; ex-info naming the tag (with the reader's own error as the cause) instead of
;; silently degrading to the runtime-call fallback.
(define (ldr-invoke-reader tag-kw rfn inner)
  (guard (e (else
              (let* ((orig (jolt-unwrap-throw e))
                     (orig-msg (guard (_ (#t #f))
                                 (let ((m ((var-deref "jolt.host" "condition-message") e)))
                                   (and (string? m) m))))
                     (msg (string-append "data reader " (keyword-t-name tag-kw) " threw"
                                         (if orig-msg (string-append ": " orig-msg) ""))))
                (jolt-throw
                  (jolt-ex-info msg (jolt-hash-map (keyword #f "tag") tag-kw) orig)))))
    (jolt-invoke rfn inner)))
;; change-tracking walk: rewrite registered #tag forms, keep everything else
;; (and its identity/metadata) intact. Mirrors reader.ss rdr-form->data but keeps
;; set FORMS for the compiler spine instead of building real sets.
(define (ldr-conv-each xs)
  (let loop ((xs xs) (acc '()) (changed #f))
    (if (null? xs) (values (reverse acc) changed)
        (let ((c (ldr-apply-readers (car xs))))
          (loop (cdr xs) (cons c acc) (or changed (not (eq? c (car xs)))))))))
(define (ldr-apply-readers x)
  (cond
    ((and (pmap? x) (eq? (jolt-get x rdr-kw-jolt-type) rdr-kw-jolt-tagged))
     (let ((rdr (data-reader-symbol (jolt-get x rdr-kw-tag)))
           (inner (ldr-apply-readers (jolt-get x rdr-kw-form))))
       (cond
          (rdr
           ;; Clojure applies a data reader at read time and substitutes its result
           ;; as code. A reader that returns a FORM (a list — e.g. borkdude.html's
           ;; #html expands to (->Html (str …))) must be compiled, so splice it in.
           ;; A reader that returns a VALUE (time-literals #time/date -> a Date) is
           ;; left as a runtime call (reader-fn 'inner): the value rebuilds at
           ;; startup, which also keeps a non-serializable constant out of an AOT
           ;; build. The reader runs at load time only when its var RESOLVES — a
           ;; reader whose ns isn't loaded yet falls back to the runtime call. A
           ;; resolved reader that throws surfaces (the prior catch-all guard
           ;; silently downgraded every reader bug to a runtime call).
           (let ((rfn (and (symbol-t? rdr) (not (jolt-nil? (symbol-t-ns rdr)))
                           (let ((v (var-deref (symbol-t-ns rdr) (symbol-t-name rdr))))
                             (and (procedure? v) v)))))
             (if rfn
                 (let ((result (ldr-invoke-reader (jolt-get x rdr-kw-tag) rfn inner)))
                   (if (cseq? result)
                       result
                       (jolt-list rdr (jolt-list (jolt-symbol #f "quote") inner))))
                 ;; unresolved reader (ns not loaded yet): runtime-call fallback
                 (jolt-list rdr (jolt-list (jolt-symbol #f "quote") inner)))))
         ((eq? inner (jolt-get x rdr-kw-form)) x)
         (else (rdr-make-tagged (jolt-get x rdr-kw-tag) inner)))))
    ((rdr-set-form? x)
     (let-values (((items changed) (ldr-conv-each (seq->list (jolt-get x rdr-kw-value)))))
       (if changed (rdr-carry-meta x (rdr-make-set items)) x)))
    ((pvec? x)
     (let-values (((items changed) (ldr-conv-each (vector->list (pvec-v x)))))
       (if changed (rdr-carry-meta x (apply jolt-vector items)) x)))
    ((pmap? x)
     (let ((order (hashtable-ref rdr-map-order x #f)))
       (if order
           (let-values (((kvs changed) (ldr-conv-each order)))
             (if changed (rdr-carry-meta x (rdr-make-map kvs)) x))
           (let-values (((kvs changed) (ldr-conv-each (pmap-fold x (lambda (k v a) (cons k (cons v a))) '()))))
             (if changed (rdr-carry-meta x (apply jolt-hash-map kvs)) x)))))
    ((cseq? x)
     (let-values (((items changed) (ldr-conv-each (seq->list x))))
       (if changed (rdr-carry-meta x (apply jolt-list items)) x)))
    (else x)))

;; read+merge one data_readers file: a literal {tag-sym reader-sym …} map.
(define (merge-data-readers-file path)
  (let* ((src (read-file-string path)))
    (let-values (((m j) (rdr-read-form src 0 (string-length src))))
    (when (and (not (rdr-eof? m)) (pmap? m))
      (let ((cur (data-readers-table)))
        (def-var! "clojure.core" "*data-readers*"
          (apply jolt-assoc (if (pmap? cur) cur empty-pmap)
                 (pmap-fold m (lambda (k v a) (cons k (cons v a))) '()))))
      (set! data-readers-active #t)
      ;; eagerly load each reader fn's namespace so the rewritten call resolves.
      (pmap-fold m (lambda (k v a)
                     (when (and (symbol-t? v) (symbol-t-ns v) (not (jolt-nil? (symbol-t-ns v))))
                       ;; tolerant — a data_readers entry must not kill the project
                       ;; load — but say WHICH reader ns failed and why, or the
                       ;; miss surfaces later as an unrelated unresolved-var error
                       ;; at first #tag read.
                       (guard (e (#t (display
                                      (string-append "jolt: warning: data-reader namespace "
                                                     (symbol-t-ns v) " failed to load: "
                                                     (guard (_ (#t "(unprintable error)"))
                                                       ((var-deref "jolt.host" "condition-message") e))
                                                     "\n")
                                      (current-error-port))))
                         (load-namespace (symbol-t-ns v))))
                     a)
                 #f)))))
(define (load-data-readers!)
  (for-each
    (lambda (root)
      (let ((clj (string-append root "/data_readers.clj"))
            (cljc (string-append root "/data_readers.cljc")))
        (cond ((file-exists? clj) (merge-data-readers-file clj))
              ((file-exists? cljc) (merge-data-readers-file cljc)))))
    source-roots))

;; --- namespace -> file path -------------------------------------------------
;; "app.commonmark-test" -> "app/commonmark_test": split on '.', munge '-'->'_'
;; per segment, join with '/'. Matches Clojure's ns->file munging.
(define (ns-seg-munge seg) (jch-munge-segments seg)) ; shared with the class graph
(define (ns-name->rel name)
  (let loop ((cs (string->list name)) (seg '()) (segs '()))
    (cond
      ((null? cs)
       (let ((all (reverse (cons (list->string (reverse seg)) segs))))
         (let join ((xs all) (acc ""))
           (cond ((null? xs) acc)
                 ((string=? acc "") (join (cdr xs) (ns-seg-munge (car xs))))
                 (else (join (cdr xs) (string-append acc "/" (ns-seg-munge (car xs)))))))))
      ((char=? (car cs) #\.)
       (loop (cdr cs) '() (cons (list->string (reverse seg)) segs)))
      (else (loop (cdr cs) (cons (car cs) seg) segs)))))

;; First existing <root>/rel.clj or <root>/rel.cljc on the search roots, else #f.
;; A self-contained joltc binary embeds jolt-core + stdlib source keyed by their
;; root-relative path ("clojure/string.clj"); those are checked first, so a
;; `require` resolves with no source on disk. The dev bin/joltc has an empty
;; source store, so the two hashtable probes miss and it falls straight to disk.
(define (resolve-on-roots rel)
  (let ((eclj (string-append rel ".clj")) (ecljc (string-append rel ".cljc")))
    (cond
      ((string? (hashtable-ref embedded-resources eclj #f)) eclj)
      ((string? (hashtable-ref embedded-resources ecljc #f)) ecljc)
      (else
        (let loop ((roots source-roots))
          (if (null? roots) #f
              (let ((clj  (string-append (car roots) "/" rel ".clj"))
                    (cljc (string-append (car roots) "/" rel ".cljc")))
                (cond ((file-exists? clj) clj)
                      ((file-exists? cljc) cljc)
                      (else (loop (cdr roots)))))))))))

;; Read a namespace source. An embedded key (resolve-on-roots above, or the
;; build driver's app-order entries) reads its baked string; everything else is
;; a real path read off disk. Bytevector entries (the bundled boots/stub) are not
;; source, so a string? guard skips them.
(define (ldr-read-source path)
  (let ((emb (hashtable-ref embedded-resources path #f)))
    (if (string? emb) emb (read-file-string path))))

(define (find-ns-file name) (resolve-on-roots (ns-name->rel name)))

;; --- the loaded set ---------------------------------------------------------
;; Seeded with every namespace that already has vars at load time — the baked
;; prelude/image (clojure.core, clojure.string, jolt.analyzer, …). A `require` of
;; one of those then no-ops instead of hunting for a (nonexistent) source file.
(define loaded-ns (make-hashtable string-hash string=?))
(vector-for-each (lambda (c) (hashtable-set! loaded-ns (var-cell-ns c) #t))
                 (hashtable-values var-table))

;; clojure.core.async ships native channel primitives (async.ss) AND a Clojure
;; overlay (stdlib/clojure/core/async.clj) with the higher-level dataflow API
;; (alts!, pipe, mult, mix, pub/sub, map, merge, …). The primitives pre-seed the
;; namespace above, which would make a `require` no-op and skip the overlay. Drop
;; it from the loaded set so a require pulls the overlay from the source roots
;; (like clojure.test); the primitives stay defined either way.
(hashtable-delete! loaded-ns "clojure.core.async")

;; Seed *loaded-libs* ref from the initial loaded-ns set (for tools.namespace
;; and core.typed which conj/disj on it).  Must happen after the async deletion.
(let* ((libs-cell (var-cell-lookup "clojure.core" "*loaded-libs*"))
       (libs-ref (and libs-cell (var-cell-root libs-cell))))
  (when (and libs-ref (jolt-ref? libs-ref))
    (vector-for-each
      (lambda (k) (jolt-ref-val-set! libs-ref
                     (pset-conj (jolt-ref-val libs-ref)
                       (jolt-symbol #f k))))
      (hashtable-keys loaded-ns))))

;; Does `name` already have vars in the var-table? A namespace baked into the
;; image after the snapshot above — an AOT'd app namespace in a `jolt build`
;; binary — exists in memory with no source file; a later `require` of it must
;; no-op rather than hunt the (absent) source.
(define (ns-has-vars? name)
  (let ((found #f))
    (vector-for-each
      (lambda (c) (when (and (not found) (string=? (var-cell-ns c) name)) (set! found #t)))
      (hashtable-values var-table))
    found))

;; Called after a file-backed namespace finishes loading, with (name file). The
;; build driver sets this to record app namespaces in dependency order for AOT
;; emission; a no-op for normal runs.
(define ns-loaded-hook (lambda (name file) #f))
(define (set-ns-loaded-hook! f) (set! ns-loaded-hook f))

;; Read every form from a file and compile+eval it in turn. The first form is
;; normally (ns …), which expands to (in-ns …) and switches the current ns, so
;; later forms compile in that namespace — (chez-current-ns) is re-read each step.
;;
;; Reads by POSITION rather than via __parse-next: a top-level form that reads as
;; nothing — a :cljs-only #? with no matching branch, a #_ discard, a trailing
;; comment — yields rdr-eof but still advances. parse-next collapses that to "no
;; more forms", which would silently drop the entire rest of the file; here we
;; skip the no-op form and continue to true end-of-string.
;; A file load binds *file* to the path and *source-path* to the bare file
;; name around its forms (the reference binds both in Compiler.load), so loaded
;; code can read its own location. Cells resolve lazily — the vars' defaults
;; load after this file.
(define ldr-file-cell #f)
(define ldr-spath-cell #f)
(define (ldr-with-file-vars path thunk)
  (unless ldr-file-cell
    (set! ldr-file-cell (var-cell-lookup "clojure.core" "*file*"))
    (set! ldr-spath-cell (var-cell-lookup "clojure.core" "*source-path*")))
  (if (not (and ldr-file-cell ldr-spath-cell))
      (thunk)
      (let ((name (let loop ((i (- (string-length path) 1)))
                    (cond ((< i 0) path)
                          ((char=? (string-ref path i) #\/)
                           (substring path (+ i 1) (string-length path)))
                          (else (loop (- i 1)))))))
        (dynamic-wind
          (lambda () (dyn-binding-stack
                      (cons (list (cons ldr-file-cell path) (cons ldr-spath-cell name))
                            (dyn-binding-stack))))
          thunk
          (lambda () (dyn-binding-stack (cdr (dyn-binding-stack))))))))

(define (load-jolt-file path)
  (let* ((src (ldr-read-source path)) (end (string-length src)))
    ;; parameterize (not a bare set!) so a require nested in this file's ns form
    ;; restores path when control returns to the rest of this file.
    (parameterize ((rdr-source-file path))   ; list forms read here carry :file = path
      (ldr-with-file-vars path
        (lambda ()
          (let loop ((i 0))
            (when (< i end)
              (let-values (((form j) (rdr-read-form src i end)))
                (when (> j i)
                  (unless (rdr-eof? form)
                    (when (getenv "JOLT_TRACE_LOAD")
                      (display "  [load-form] " (current-error-port))
                      (display (jolt-pr-str form) (current-error-port)) (newline (current-error-port)))
                    (jolt-compile-eval-form (if data-readers-active (ldr-apply-readers form) form)
                                            (chez-current-ns)))
                  (loop j))))))))))

;; Mark a namespace as loaded in both the host hashtable and the *loaded-libs* ref.
(define (ldr-mark-loaded! name)
  (hashtable-set! loaded-ns name #t)
  (let* ((libs-cell (var-cell-lookup "clojure.core" "*loaded-libs*"))
         (libs-ref (and libs-cell (var-cell-root libs-cell))))
    (when (and libs-ref (jolt-ref? libs-ref))
      (jolt-ref-val-set! libs-ref
        (pset-conj (jolt-ref-val libs-ref) (jolt-symbol #f name))))))

;; Undo ldr-mark-loaded! — a failed load rolls its mark back so a retry loads.
(define (ldr-unmark-loaded! name)
  (hashtable-delete! loaded-ns name)
  (let* ((libs-cell (var-cell-lookup "clojure.core" "*loaded-libs*"))
         (libs-ref (and libs-cell (var-cell-root libs-cell))))
    (when (and libs-ref (jolt-ref? libs-ref))
      (jolt-ref-val-set! libs-ref
        (pset-disj (jolt-ref-val libs-ref) (jolt-symbol #f name))))))

;; Has `name` cleared the loaded-ns / *loaded-libs* dedup? A tools.namespace disj
;; from *loaded-libs* forces a reload even though loaded-ns still holds.
(define (ns-dedup-loaded? name)
  (let* ((libs-cell (var-cell-lookup "clojure.core" "*loaded-libs*"))
         (libs-ref (and libs-cell (var-cell-root libs-cell))))
    (and (hashtable-ref loaded-ns name #f)
         (or (not libs-ref)
             (not (jolt-ref? libs-ref))
             (jolt-contains? (jolt-ref-val libs-ref) (jolt-symbol #f name))))))

;; :reload-all forces the dedup off for the whole dynamic extent of a load (the
;; loader learns dependencies only as it loads them), so every namespace pulled in
;; reloads too — mirroring clojure.core. :verbose prints each load to stderr.
(define ldr-reload-all? (make-thread-parameter #f))
(define ldr-verbose? (make-thread-parameter #f))

;; load-namespace: load `name`'s source once. Marked loaded BEFORE eval so a
;; dependency cycle terminates (Clojure's behavior). force? (a :reload of the
;; named lib) bypasses the dedup for THIS load only; ldr-reload-all? bypasses it
;; for this load and every nested one in its extent. On a throw the mark is rolled
;; back IF this call made it (a retry then loads) and the caller's ns is restored —
;; matching Clojure, which marks a lib loaded only after success.
(define (load-namespace name) (load-namespace* name #f))
(define (load-namespace* name force?)
  (let ((was-loaded? (ns-dedup-loaded? name)))
    (unless (and (not force?) (not (ldr-reload-all?)) was-loaded?)
      (let ((file (find-ns-file name)))
        (cond
          (file
           (when (ldr-verbose?)
             (display (string-append "Loading " name " from " file "\n")
                      (current-error-port)))
           (ldr-mark-loaded! name)            ; mark before load so a cycle terminates
           (let ((saved (chez-current-ns)))
             (guard (e (else
                         (set-chez-ns! saved)          ; restore ns, then roll the mark back
                         (unless was-loaded? (ldr-unmark-loaded! name))
                         (raise e)))
               (load-jolt-file file))
             (set-chez-ns! saved)             ; restore the current ns (thread-local)
             (ns-loaded-hook name file)))
          ;; No source file but the namespace exists in memory (AOT'd into a built
          ;; binary): it's already defined — mark loaded and move on.
          ((ns-has-vars? name)
           (ldr-mark-loaded! name))
          ;; Same-file namespace (inlined ns form in a Jolt file): registered via
          ;; intern-ns! in the runtime registry even if no vars bear its ns name yet.
          ((hashtable-ref ns-registry name #f)
           (ldr-mark-loaded! name))
          (else
            (error #f (string-append "Could not locate " (ns-name->rel name)
                                     ".clj (or .cljc) on the source roots") name)))))))

;; load-file: load an explicit path (a `run FILE`), in the current ns.
(define (jolt-load-file path)
  (load-jolt-file path)
  jolt-nil)

;; A libspec under a prefix joins onto it: a bare symbol `string` -> `prefix.string`,
;; a vector `[string :as s]` -> `[prefix.string :as s]` (opts preserved).
(define (prefix-join prefix lib)
  (cond
    ((symbol-t? lib) (jolt-symbol #f (string-append prefix "." (symbol-t-name lib))))
    ((pvec? lib)
     (let ((items (seq->list lib)))
       (if (and (pair? items) (symbol-t? (car items)))
           (apply jolt-vector (jolt-symbol #f (string-append prefix "." (symbol-t-name (car items)))) (cdr items))
           lib)))
    (else lib)))

;; The prefix-list form of a require/use spec: a LIST `(prefix lib …)` expands to
;; one spec per lib (prefix.lib), so (:require (clojure [string :as str])) means
;; clojure.string :as str. A list with a KEYWORD second element is a single
;; libspec (the jolt superset — see parse-libspec in ns.ss), not a prefix list;
;; a vector / symbol spec is already a single lib.
(define (expand-spec s)
  (if (or (cseq? s) (empty-list-t? s))
      (let ((items (seq->list s)))
        (if (prefix-list-items? items)
            (map (lambda (lib) (prefix-join (symbol-t-name (car items)) lib)) (cdr items))
            (list s)))
      (list s)))

;; --- require/use that LOAD ---------------------------------------------------
;; Override the alias-only versions from natives-str.ss. Load each spec's target
;; (no-op if baked/already loaded), THEN register its :as/:refer under the caller
;; ns (chez-register-spec! reads the current ns, restored by load-namespace).
;;
;; keyword flags (clojure.core load-libs) are collected from the arg list before
;; the libspecs: :reload forces the named libs past the dedup, :reload-all forces
;; it off for the whole load (so transitively-required libs reload too), :verbose
;; prints each load.
(define (ldr-flag-names specs)
  (let loop ((xs specs) (acc '()))
    (cond ((null? xs) (reverse acc))
          ((keyword? (car xs)) (loop (cdr xs) (cons (keyword-t-name (car xs)) acc)))
          (else (loop (cdr xs) acc)))))

;; Load each expanded libspec's target (no-op if baked/already loaded), register
;; its :as/:refer under the caller ns, and — for `use` (use? #t) — refer every
;; public var when the spec has no :only/:refer filter. Target + opts both come
;; from the shared parse-libspec (ns.ss): the single spec->target+opts parser
;; routed through by loader-require / loader-use / chez-register-spec! /
;; ce-scan-requires!.
(define (ldr-load+register specs force-named? use?)
  (for-each
    (lambda (s0)
      (for-each
        (lambda (s)
          (let* ((parsed (parse-libspec s))
                 (target (and parsed (car parsed)))
                 (opt-names (if parsed (map car (cdr parsed)) '())))
            (when target (load-namespace* target force-named?))
            (chez-register-spec! (chez-current-ns) s)
            (when (and use? target
                       (not (or (member "only" opt-names) (member "refer" opt-names))))
              (chez-register-refer-all! (chez-current-ns) target))))
        (expand-spec s0)))
    specs))

(define (loader-require . specs)
  (let* ((flags (ldr-flag-names specs))
         (real (filter (lambda (s) (not (keyword? s))) specs))
         (reload-all? (member "reload-all" flags))
         (reload? (and (not reload-all?) (member "reload" flags)))
         (verbose? (member "verbose" flags)))
    (if reload-all?
        (parameterize ((ldr-reload-all? #t) (ldr-verbose? verbose?))
          (ldr-load+register real #f #f))
        (parameterize ((ldr-verbose? verbose?))
          (ldr-load+register real (and reload? #t) #f))))
  jolt-nil)
(def-var! "clojure.core" "require" loader-require)

(define (loader-use . specs0)
  (let* ((flags (ldr-flag-names specs0))
         (real (filter (lambda (s) (not (keyword? s))) specs0))
         (reload-all? (member "reload-all" flags))
         (reload? (and (not reload-all?) (member "reload" flags)))
         (verbose? (member "verbose" flags)))
    (if reload-all?
        (parameterize ((ldr-reload-all? #t) (ldr-verbose? verbose?))
          (ldr-load+register real #f #t))
        (parameterize ((ldr-verbose? verbose?))
          (ldr-load+register real (and reload? #t) #t))))
  jolt-nil)
(def-var! "clojure.core" "use" loader-use)

(def-var! "clojure.core" "load-file" jolt-load-file)

;; The directory of a namespace's resource path: "clojure.tools.reader-test" ->
;; "clojure/tools" (drop the last segment of ns-name->rel). "" for a top-level ns.
(define (ns-rel-dir name)
  (let* ((r (ns-name->rel name)))
    (let loop ((k (fx- (string-length r) 1)))
      (cond ((fx<? k 0) "")
            ((char=? (string-ref r k) #\/) (substring r 0 k))
            (else (loop (fx- k 1)))))))

;; load: an arg starting with "/" is a root-relative resource path ("/app/extra");
;; otherwise it is resolved against the CURRENT namespace's directory, matching
;; Clojure — (load "common_tests") from clojure.tools.reader-test loads
;; clojure/tools/common_tests.clj. Strip the leading slash / try .clj/.cljc.
(define (jolt-load . paths)
  (for-each
    (lambda (p)
      (let* ((rel (cond
                    ((and (> (string-length p) 0) (char=? (string-ref p 0) #\/))
                     (substring p 1 (string-length p)))
                    (else (let ((dir (ns-rel-dir (chez-current-ns))))
                            (if (string=? dir "") p (string-append dir "/" p))))))
             (f (resolve-on-roots rel)))
        (if f (load-jolt-file f)
            (error #f "Could not locate resource on source roots" p))))
    paths)
  jolt-nil)
(def-var! "clojure.core" "load" jolt-load)

;; --- shell primitive (jolt.host/sh, sh-out) ---------------------------------
;; `sh` runs `sh -c CMD`, inheriting stdout/stderr (so git progress shows), and
;; returns the exit code. `sh-out` captures stdout to a string (exit ignored) for
;; commands whose output we parse (git rev-parse). Used by jolt.deps for git.
(define (jolt-sh cmd) (system cmd))
(def-var! "jolt.host" "sh" jolt-sh)

(define (jolt-sh-out cmd)
  (call-with-values
    (lambda () (open-process-ports (string-append "exec sh -c " (sh-quote cmd))
                                   (buffer-mode block) (native-transcoder)))
    (lambda (stdin stdout stderr pid)
      (close-port stdin)
      (let ((out (get-string-all stdout)))
        (close-port stdout) (close-port stderr)
        (if (eof-object? out) "" out)))))
(define (sh-quote s)   ; single-quote for the outer sh -c
  (string-append "'"
    (apply string-append
      (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s)))
    "'"))
(def-var! "jolt.host" "sh-out" jolt-sh-out)

;; Expose source-root control + ns loading to Clojure (jolt.main / jolt.deps).
(def-var! "jolt.host" "set-source-roots!"
  (lambda (roots) (set-source-roots! (seq->list roots)) jolt-nil))
(def-var! "jolt.host" "source-roots" (lambda () (list->cseq source-roots)))
(def-var! "jolt.host" "load-namespace" (lambda (n) (load-namespace n) jolt-nil))
(def-var! "jolt.host" "file-exists?" (lambda (p) (if (file-exists? p) #t #f)))
(def-var! "jolt.host" "getenv" (lambda (n) (let ((v (getenv n))) (if v v jolt-nil))))

;; jolt version string. A self-contained binary build bakes the real tag into the
;; saved heap by emitting (set! jolt-baked-version "…") in flat.ss; a dev run off
;; the seed leaves it #f and falls back to $JOLT_VERSION (bin/joltc sets it from
;; `git describe`), then "dev".
(define jolt-baked-version #f)
(def-var! "jolt.host" "jolt-version"
  (lambda ()
    (or jolt-baked-version
        (let ((v (getenv "JOLT_VERSION"))) (and v (> (string-length v) 0) v))
        "dev")))
