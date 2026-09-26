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
;; tree-shaken no-eval binary has dropped, crashing startup. jolt/build entry
;; points keep the scanning set-source-roots!.
(define (set-source-roots!* roots) (set! source-roots roots))
(define (get-source-roots) source-roots)

;; jolt's answer to (System/getProperty "java.class.path") is these roots — the
;; loader owns them, the runtime's system-property table renders them. Tooling
;; ported from the JVM (orchard's / compliment's classpath scans, cider-nrepl's
;; classpath op) discovers project sources through that property.
(set-class-path-provider! get-source-roots)

;; Install roots — the directories that ship with Jolt (compiler + stdlib + vendored
;; deps). Shared by cli.ss, build-jolt.ss, and the bld-require-closure filter so the
;; literal list stays in one place. ORDER IS PRECEDENCE: the first root holding a
;; namespace wins, on disk (resolve-on-roots) and in a built binary (build-jolt.ss
;; bakes first-wins to match). Grenadine ships host adapters alongside its portable
;; core, one of them named jolt.deps, so jolt-core has to precede it.
(define ldr-install-roots
  '("jolt-core" "stdlib" "vendor/fs/src" "vendor/process/src" "vendor/cli/src"
    "vendor/grenadine/src"
    ;; the four namespaces grenadine generates instead of committing — see
    ;; vendor/grenadine-generated/README.md
    "vendor/grenadine-generated"))

;; True when `f` is a file owned by the Jolt runtime (compiler + stdlib) — either
;; an embedded-resource key — eagerly registered, or carried by the source blob's
;; index — or a path under one of ldr-install-roots.
(define (ldr-install-file? f)
  (or (embedded-resource-has? f)
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

;; --- namespaces Jolt vendors and owns ---------------------------------------
;; A copy of one of these on a project's roots must not shadow jolt's. A built
;; binary already resolves jolt's copy first (install sources are embedded, and
;; resolve-on-roots probes those before any root), so resolving these from the
;; install roots is what keeps source mode answering the same file as the
;; binary — a project that pulls babashka.fs in as a transitive dependency gets
;; one babashka.fs, not two. babashka does not let a classpath copy shadow a
;; built-in either.
;;
;; There is no per-namespace "supplement" seam any more. There used to be one
;; (babashka.fs -> jolt.bb.fs), because jolt's reader matched :bb and babashka.fs
;; writes list-dir as #?(:bb nil :default (defn list-dir …)) — so on jolt the var
;; stayed declared and unbound and jolt had to fill it. Dropping :bb (issue #893,
;; reader.ss rdr-features) means babashka.fs defines its own list-dir off the
;; :clj branch, which is the one jolt's java.nio shims target.
;;
;; The escape hatch stays, because "jolt always wins" is not a thing a project
;; can be stuck with. A project declares (deps.edn) which of these it supplies
;; itself:
;;
;;   :jolt/replaces [babashka.fs]
;;
;; and its own copy resolves ahead of jolt's. jolt.deps collects the key and
;; jolt.main hands it here through jolt.host/replace-builtin-ns! before any of
;; the project compiles, which is the same ordering :jolt/provides needs.
;;
;; Only the PROJECT may declare one. A library that took a built-in over for the
;; whole program would decide what babashka.fs MEANS for every other library in
;; it, which is the shape the provider table already refuses for classes.
(define ldr-ns-replacements '())
(define (ldr-ns-replaced? name) (and (member name ldr-ns-replacements) #t))
(define (replace-builtin-ns! name)
  (unless (ldr-ns-replaced? name)
    (set! ldr-ns-replacements (cons name ldr-ns-replacements))))
;; the same question at the path level, for resolve-on-roots. A replacement
;; covers the namespace's children too (babashka/fs/whatever), like the
;; built-in list it overrides.
(define (ldr-ns-replaced-rel? rel)
  (let loop ((ns ldr-ns-replacements))
    (and (pair? ns)
         (or (ldr-rel-prefix? rel (ns-name->rel (car ns))) (loop (cdr ns))))))
;; Matched as a namespace PREFIX so a child (babashka/process/pprint) travels
;; with its parent.
(define ldr-builtin-ns-rels '("babashka/fs" "babashka/process"))
(define (ldr-builtin-ns-rel? rel)
  (let loop ((bs ldr-builtin-ns-rels))
    (and (pair? bs)
         (or (ldr-rel-prefix? rel (car bs)) (loop (cdr bs))))))
;; REL is B, or a child of it: matched as a namespace prefix so a child
;; (babashka/process/pprint) travels with its parent.
(define (ldr-rel-prefix? rel b)
  (let ((bn (string-length b)))
    (and (>= (string-length rel) bn)
         (string=? (substring rel 0 bn) b)
         (or (fx=? (string-length rel) bn)
             (char=? (string-ref rel bn) #\/)))))

;; --- data readers (#tag literals) -------------------------------------------
;; A project's data_readers.{jolt,clj,cljc} at a source root maps a tag symbol to a
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

;; The reader FUNCTION for a *data-readers* value: a fn as-is, a var's root, or a
;; qualified symbol's var — the same three shapes the read-string path accepts
;; (reader.ss rdr-data-reader-fn). The load path took symbols only, so an entry
;; added with (alter-var-root #'*data-readers* assoc 'my/tag (fn …)) reached the
;; analyzer as (#<procedure> 'form) and died there as "unsupported form".
(define (ldr-reader-fn rdr)
  (cond
    ((procedure? rdr) rdr)
    ((var-cell? rdr) (let ((f (var-cell-root rdr))) (and (procedure? f) f)))
    ((and (symbol-t? rdr) (not (jolt-nil? (symbol-t-ns rdr))))
     (guard (e (#t #f))
       (let ((v (var-deref (symbol-t-ns rdr) (symbol-t-name rdr))))
         (and (procedure? v) v))))
    (else #f)))

;; The deferred shape, (reader-fn 'inner) evaluated at runtime. Only a SYMBOL
;; reader can be written as a call; anything else that reaches here is a table
;; entry that is not a reader at all, and saying so beats emitting a form the
;; analyzer can only report as "unsupported form".
(define (ldr-reader-call tag-kw rdr inner)
  (if (symbol-t? rdr)
      (jolt-list rdr (jolt-list (jolt-symbol #f "quote") inner))
      (jolt-throw (jolt-ex-info
                    (string-append "data reader " (keyword-t-name tag-kw) " is not a function")
                    (jolt-hash-map (keyword #f "tag") tag-kw)))))

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
           ;; A reader NAMED BY A SYMBOL that returns a VALUE (time-literals
           ;; #time/date -> a Date) is left as a runtime call (reader-fn 'inner):
           ;; the value rebuilds at startup, which also keeps a non-serializable
           ;; constant out of an AOT build. A fn/var reader has no name to call,
           ;; so its value is spliced in. The reader runs at load time only when
           ;; it RESOLVES — a reader whose ns isn't loaded yet falls back to the
           ;; runtime call. A resolved reader that throws surfaces (the prior
           ;; catch-all guard silently downgraded every reader bug to a call).
           (let ((rfn (ldr-reader-fn rdr)))
             (if rfn
                 (let ((result (ldr-invoke-reader (jolt-get x rdr-kw-tag) rfn inner)))
                   (cond
                     ((cseq? result) result)
                     ;; a SYMBOL reader has a name to call at runtime
                     ((symbol-t? rdr) (ldr-reader-call (jolt-get x rdr-kw-tag) rdr inner))
                     ;; a fn/var reader has no name to defer to, so splice the value
                     ;; it just produced — what the read-string path does anyway
                     (else result)))
                 ;; unresolved reader (ns not loaded yet): runtime-call fallback
                 (ldr-reader-call (jolt-get x rdr-kw-tag) rdr inner))))
         ((eq? inner (jolt-get x rdr-kw-form)) x)
         (else (rdr-make-tagged (jolt-get x rdr-kw-tag) inner)))))
    ((rdr-set-form? x)
     (let-values (((items changed) (ldr-conv-each (seq->list (jolt-get x rdr-kw-value)))))
       (if changed (rdr-carry-meta x (rdr-make-set items)) x)))
    ((pvec? x)
     (let-values (((items changed) (ldr-conv-each (vector->list (pvec-v x)))))
       (if changed (rdr-carry-meta x (apply jolt-vector items)) x)))
    ((pmap? x)
     (let ((order (rdr-map-order-ref x)))
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
        (def-dynvar! "clojure.core" "*data-readers*"
          (apply jolt-assoc (if (pmap? cur) cur empty-pmap)
                 (pmap-fold m (lambda (k v a) (cons k (cons v a))) '()))))
      (set! data-readers-active #t)
      ;; eagerly load each reader fn's namespace so the rewritten call resolves.
      ;; Tolerant — a data_readers entry must not kill the project load — but a
      ;; failure is reported in full (ldr-warn-reader-ns-failed!), or the miss
      ;; surfaces later as an unrelated unresolved-var error at first #tag read.
      (pmap-fold m (lambda (k v a)
                     (when (and (symbol-t? v) (symbol-t-ns v) (not (jolt-nil? (symbol-t-ns v))))
                       (guard (e (#t (ldr-warn-reader-ns-failed! (symbol-t-ns v) e m)))
                         (load-namespace (symbol-t-ns v))))
                     a)
                 #f)))))

;; The warning for a data_readers namespace that failed to load — everything the
;; reader needs to act on it: which namespace, why, WHERE it failed (the throw's
;; own position, in the shape the uncaught report uses), and which tags of the
;; file now have no reader. The load's position does not outlive the warning:
;; load-jolt-file* unwinds it, so the next error in the process is not reported
;; "at" this namespace's failing form.
(define (ldr-warn-reader-ns-failed! ns-name e readers)
  (let ((port (current-error-port))
        (msg (guard (_ (#t "(unprintable error)"))
               ((var-deref "jolt.host" "condition-message") e)))
        (where (jolt-throwable-source-string e))
        (tags (sort string<?
                    (pmap-fold readers
                               (lambda (k v a)
                                 (if (and (symbol-t? v) (equal? (symbol-t-ns v) ns-name))
                                     (cons (string-append "#" (jolt-pr-str k)) a)
                                     a))
                               '()))))
    (display (string-append "jolt: warning: data-reader namespace " ns-name
                            " failed to load: " msg "\n")
             port)
    (when where (display (string-append "  at " where "\n") port))
    (unless (null? tags)
      (display (string-append "  tags " (jolt-str-join tags) " will not read\n") port))))
(define (load-data-readers!)
  (for-each
    (lambda (root)
      ;; data_readers.{jolt,clj,cljc}, in the same precedence as a namespace's
      ;; source (ldr-source-exts below) — first one found on this root wins,
      ;; inside a jar root as on disk.
      (let ((f (ldr-root-source root "data_readers")))
        (when f (merge-data-readers-file f))))
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

;; The extensions a namespace's source can carry, in resolution order. .jolt is
;; the same language as .clj — the reader, analyzer, and emitter never look at the
;; extension — and only marks intent: the file uses jolt-specific interop and is
;; not portable Clojure. It resolves FIRST so a library can ship a portable
;; foo.cljc/foo.clj alongside a foo.jolt that wins here, the way .clj wins over
;; .cljc on the JVM.
(define ldr-source-exts '(".jolt" ".clj" ".cljc"))

(define (ldr-source-path? p)
  (let loop ((es ldr-source-exts))
    (and (pair? es)
         (let* ((suf (car es)) (n (string-length p)) (m (string-length suf)))
           (or (and (>= n m) (string=? (substring p (- n m) n) suf))
               (loop (cdr es)))))))

;; --- embedded roots ---------------------------------------------------------
;; A root spelled "embed:<prefix>" is not a directory on disk but a prefix
;; into the runtime's embedded-resource table — the store `jolt build`'s
;; deps.edn :jolt/build {:embed [dirs]} registers (io.ss
;; register-embedded-resource!), and the one a self-contained jolt binary
;; carries jolt-core + stdlib under. A loader context over such a root serves
;; namespaces and resources with no files anywhere, which is how a shipped
;; binary runs a library it baked in.
;;
;; The spelling cannot collide with a real path: ":" is illegal in a Windows
;; path, and a POSIX directory literally named "embed:" is not a thing. Keys
;; are "<prefix>/<name>", so the prefix never carries a trailing slash.
(define (ldr-embedded-root? root)
  (and (string? root)
       (>= (string-length root) 6)
       (string=? (substring root 0 6) "embed:")))
(define (ldr-embedded-root-prefix root)
  (substring root 6 (string-length root)))

;; --- jar roots -------------------------------------------------------------
;; A root that names a jar (a .jar or .zip file, either case) is read through
;; its central directory (java/zip-file.ss root-jar-index): a namespace or a
;; resource in it resolves to a jar path, "jar:file:<jar>!/<entry>" (io.ss),
;; which read-file-string reads out of the archive. Nothing is extracted
;; (jolt issue #1005), and the jar's own path is what the roots hold, as a jar
;; on the JVM's classpath is.
;;
;; The location NAME resolves to on ROOT — a disk path under a directory root,
;; a jar path into a jar root, an embedded KEY under an embedded root — or #f
;; when it is not there.
(define (ldr-root-file root name)
  (if (ldr-embedded-root? root)
      ;; The answer is the key itself, not a path: ldr-read-source reads such a
      ;; key (embedded-resource-ref), and io.ss's reader/opener accept it the
      ;; way they accept any other source location.
      (let ((k (string-append (ldr-embedded-root-prefix root) "/" name)))
        (and (embedded-resource-has? k) k))
      (let ((d (root-jar-index root)))
        (if d
            (and (zipdir-has? d name) (make-jar-path (root-path-abs root) name))
            (let ((f (string-append root "/" name)))
              (and (file-exists? f) f))))))
;; The source of REL on ROOT: the first extension present, in ldr-source-exts
;; order, or #f.
(define (ldr-root-source root rel)
  (let ext ((es ldr-source-exts))
    (and (pair? es)
         (or (ldr-root-file root (string-append rel (car es)))
             (ext (cdr es))))))

;; First existing <root>/rel.<ext> on the search roots, else #f.
;; A self-contained jolt binary embeds jolt-core + stdlib source keyed by their
;; root-relative path ("clojure/string.clj"); those are checked first, so a
;; `require` resolves with no source on disk. The dev bin/jolt has an empty
;; source store, so the hashtable probes miss and it falls straight to disk.
(define (resolve-on-roots rel)
  (define (embedded-key? k) (embedded-resource-has? k))
  (define (on-roots roots)
    (let loop ((roots roots))
      (and (pair? roots)
           (or (ldr-root-source (car roots) rel)
               (loop (cdr roots))))))
  ;; A namespace the project DECLARED it supplies is the project's, ahead of
  ;; everything — including the embedded copy a built binary carries, which is
  ;; what probes first for every other namespace. Anything less and the hatch
  ;; would work in source mode and not in a build, which is the divergence the
  ;; built-in rule exists to prevent.
  (if (ldr-ns-replaced-rel? rel)
      (on-roots source-roots)
      (or (let loop ((es ldr-source-exts))
            (and (pair? es)
                 (let ((k (string-append rel (car es))))
                   (if (embedded-key? k) k (loop (cdr es))))))
          ;; a namespace jolt provides as a host built-in resolves to jolt's copy
          ;; before a project root's — see ldr-builtin-ns-rels
          (and (ldr-builtin-ns-rel? rel) (on-roots ldr-install-roots))
          (on-roots source-roots))))

;; Read a namespace source. An embedded key (resolve-on-roots above, or the
;; build driver's app-order entries) reads its baked string; everything else is
;; a real path read off disk. Bytevector entries (the bundled boots/stub, and
;; source embeds stored as bytevectors to save heap) decode via utf8->string.
(define (ldr-read-source path)
  (let ((emb (embedded-resource-ref path)))
    (cond ((string? emb) emb)
          ((bytevector? emb) (utf8->string emb))
          (else (read-file-string path)))))

(define (find-ns-file name) (resolve-on-roots (ns-name->rel name)))

;; --- the loaded set ---------------------------------------------------------
;; Seeded with every namespace that already has vars at load time — the baked
;; prelude/image (clojure.core, clojure.string, jolt.analyzer, …). A `require` of
;; one of those then no-ops instead of hunting for a (nonexistent) source file.
;; ldr-tbl-mu covers every MUTATION of loaded-ns and of the AOT memos below, and
;; every WHOLE-TABLE scan of them. Single-key reads stay unlocked (strong general
;; hashtables — see rt.ss's var-table note for why that is sound).
(define ldr-tbl-mu (make-mutex))
(define loaded-ns (make-hashtable string-hash string=?))
(vector-for-each (lambda (c) (hashtable-set! loaded-ns (var-cell-ns c) #t))
                 (var-table-cells))

;; clojure.core.async ships native channel primitives (async.ss) AND a Clojure
;; overlay (stdlib/clojure/core/async.clj) with the higher-level dataflow API
;; (alts!, pipe, mult, mix, pub/sub, map, merge, …). The primitives pre-seed the
;; namespace above, which would make a `require` no-op and skip the overlay. Drop
;; it from the loaded set so a require pulls the overlay from the source roots
;; (like clojure.test); the primitives stay defined either way.
(hashtable-delete! loaded-ns "clojure.core.async")

;; Immutable baseline for app-image construction.  The build command itself
;; loads jolt.main and lazy stdlib namespaces before build-binary runs; those
;; process-local additions must not be mistaken for namespaces baked into the
;; runtime image that the new app will inherit.
(define ldr-runtime-image-ns (hashtable-copy loaded-ns #f))
(define (ldr-runtime-image-ns-copy) (hashtable-copy ldr-runtime-image-ns #f))
;; host-contract's seed-var direct-link check reads the same boot set, and the
;; boot's re-assertions of its natives stop counting as redefinitions here.
(hc-runtime-image-booted! ldr-runtime-image-ns-copy)

;; *loaded-libs* is the other half of the loaded set: a clojure.lang.Ref that
;; tools.namespace and core.typed conj/disj on, and that ns-dedup-loaded? below
;; reads alongside loaded-ns. A bare read-modify-write of the ref field lost marks
;; outright once two loads could overlap: both threads read the old set and the
;; second store dropped the first's symbol, so a
;; namespace that HAD loaded read as unloaded and ran its top level again on the
;; next require — the very bug the load protocol below exists to stop, reached
;; through the other table.
;;
;; A LOAD MUST NEVER ACQUIRE stm-lock. jolt-sync holds stm-lock across the whole
;; body of a transaction (refs.ss), so (dosync (require 'x)) is inside require while
;; holding it — and if that require parks in step 2, condition-wait releases
;; ldr-load-mu and NOTHING else, so it sits there holding stm-lock until the load it
;; is waiting for completes. A loading thread that then needed stm-lock could not
;; complete, and the two would wait on each other forever:
;;
;;   stm-lock -> ns_load   the parked dosync cannot release stm-lock until the load
;;                         it is waiting for finishes
;;   ns_load  -> stm-lock  the loader cannot finish without stm-lock
;;
;; That is not hypothetical. It reproduces on the rollback path, where a failed load
;; re-takes this lock to unmark before releasing its claim, and in the window between
;; claiming a namespace and marking it. So the mark goes through a leaf mutex of our
;; own that no transaction ever waits on, and the edge ns_load -> stm-lock does not
;; exist.
;;
;; NOT through the ambient transaction's log when there is one, which is what this
;; did first. It looks like the better answer — transactional, and it takes no lock
;; at all — and it is wrong twice, because the mark is not the transaction's to own.
;; A load either happened or it did not, and the file's defs are in the image either
;; way; an abort that rolls the mark back leaves loaded-ns marked and *loaded-libs*
;; not, which reads as unloaded and re-runs the whole namespace. And a buffered write
;; is invisible to ns-dedup-loaded? until commit, so
;; (dosync (require 'x) (require 'x)) ran x's top level twice.
;;
;; What the direct write leaves open is a mark landing while a user transaction that
;; also writes *loaded-libs* is committing, which costs that namespace one extra
;; reload. Narrow, one-directional, and the alternatives are a hang or the two bugs
;; above.
;;
;; Call with NO loader mutex held either. A (dosync (require ...)) reaches ldr-tbl-mu
;; while holding stm-lock, so the loader taking these in the other order would be the
;; same cycle by another route. Sequential, never nested, is what keeps them apart.
(define ldr-libs-mu (make-mutex))
(define (ldr-libs-update! f)
  (let* ((libs-cell (var-cell-lookup "clojure.core" "*loaded-libs*"))
         (libs-ref (and libs-cell (var-cell-root libs-cell))))
    (when (and libs-ref (jolt-ref? libs-ref))
      (jolt-with-mutex ldr-libs-mu
        (jolt-ref-val-set! libs-ref (f (jolt-ref-val libs-ref)))))))

;; Seed *loaded-libs* ref from the initial loaded-ns set (for tools.namespace
;; and core.typed which conj/disj on it).  Must happen after the async deletion.
(let ((ks (jolt-with-mutex ldr-tbl-mu (hashtable-keys loaded-ns))))
  (ldr-libs-update!
    (lambda (s)
      (let loop ((i 0) (s s))
        (if (fx=? i (vector-length ks))
            s
            (loop (fx+ i 1) (pset-conj s (jolt-symbol #f (vector-ref ks i)))))))))

;; ns-has-vars? (ns.ss) answers whether a namespace baked into the image after
;; the snapshot above — an AOT'd app namespace in a `jolt build` binary — exists
;; in memory with no source file; a later `require` of it must no-op rather than
;; hunt the (absent) source.

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
;; code can read its own location. It also rebinds the compiler-flag vars
;; *warn-on-reflection*, *assert* and *unchecked-math* to the values they
;; currently hold, so a file's top-level (set! *unchecked-math* …) is legal and
;; its effect ends with the file rather than leaking into the root.
;;
;; CURRENT, not root: Compiler.load pushes WARN_ON_REFLECTION.deref(), so a file
;; loaded from inside another file INHERITS the outer one's flags and only stops
;; inheriting where the outer frame ends. Binding the root reset every nested
;; load to the defaults instead — measured against the reference, an outer
;; (set! *unchecked-math* true) was invisible to load-string, load-file and
;; require alike. Cells resolve lazily — the vars' defaults load after this file.
(define ldr-file-cell #f)
(define ldr-spath-cell #f)
(define ldr-warn-cell #f)
(define ldr-assert-cell #f)
(define ldr-unchecked-cell #f)
(define ldr-allow-cells (make-hashtable string-hash string=?))
(define (ldr-with-file-vars path thunk)
  (unless ldr-file-cell
    (set! ldr-file-cell (var-cell-lookup "clojure.core" "*file*"))
    (set! ldr-spath-cell (var-cell-lookup "clojure.core" "*source-path*"))
    (set! ldr-warn-cell (var-cell-lookup "clojure.core" "*warn-on-reflection*"))
    (set! ldr-assert-cell (var-cell-lookup "clojure.core" "*assert*"))
    (set! ldr-unchecked-cell (var-cell-lookup "clojure.core" "*unchecked-math*")))
  (if (not (and ldr-file-cell ldr-spath-cell ldr-warn-cell ldr-assert-cell
                ldr-unchecked-cell))
      (thunk)
      (let ((name (let loop ((i (- (string-length path) 1)))
                    (cond ((< i 0) path)
                          ((char=? (string-ref path i) #\/)
                           (substring path (+ i 1) (string-length path)))
                          (else (loop (- i 1)))))))
        (dyn-with-frame
          (list (cons ldr-file-cell path)
                (cons ldr-spath-cell name)
                (cons ldr-warn-cell (var-cell-deref ldr-warn-cell))
                (cons ldr-assert-cell (var-cell-deref ldr-assert-cell))
                (cons ldr-unchecked-cell (var-cell-deref ldr-unchecked-cell)))
          thunk))))

;; The loader's two compile-from-source entrances -- load-jolt-file* below and
;; the AOT capture around it (aot-capture-load) -- each refuse BY NAME in a
;; binary built without the compiler, before either touches a compiler binding.
;; build.ss drop-compiler? leaves compile-eval.ss out of such a binary, so
;; jolt-compile-eval-form and the capture parameters both entrances parameterize
;; are unbound there, and the first one touched died as "variable
;; jolt-aot-capture-file is not bound": a raw Chez error naming a loader
;; internal, with nothing pointing at the cause. The one way a compiler-dropped
;; binary reaches source is a :jolt/tree-shake {:allow-dynamic […]} vouch that
;; was wrong -- a require, requiring-resolve or compile of a computed name the
;; vouch said never runs in the binary, or names only what the build baked,
;; running and naming a namespace whose source is on the roots (dce.ss
;; dce-bail-scan); the verdict runs on every build, so the default build is as
;; exposed as a shaken one. Image restore refuses the same way (state-image.ss
;; image-compile-eval-seam). Probed through sa-baked-global, the seam
;; aot-runtime-fingerprint already reads a baked global through.
(define (ldr-need-compiler! path)
  (unless (procedure? (sa-baked-global 'jolt-compile-eval-form))
    (jolt-throw (jolt-ex-info
                  (string-append
                    "this build has no compiler; cannot load " path " from source."
                    " The build dropped the compiler because nothing reachable"
                    " compiles at run time, and a deps.edn :jolt/tree-shake"
                    " {:allow-dynamic […]} entry vouched for the site that just did"
                    " -- a require, requiring-resolve or compile of a computed name"
                    " that never runs in the binary, or names only a namespace the"
                    " build baked. Drop the entry that covers this site, or require"
                    " the namespace statically so the build bakes it.")
                  (jolt-hash-map (keyword #f "file") path)))))

(define (load-jolt-file path)
  (load-jolt-file* path (ldr-read-source path)))

;; load-jolt-file* — the read/compile/eval loop over a PRE-READ source string.
;; Split out so the AOT cache (below) reads source once for both keying and the
;; capture load, instead of re-reading inside the loop.
(define (load-jolt-file* path src)
  (ldr-need-compiler! path)
  (let ((end (string-length src)))
    ;; parameterize (not a bare set!) so a require nested in this file's ns form
    ;; restores path when control returns to the rest of this file.
    (parameterize ((rdr-source-file path)    ; list forms read here carry :file = path
                   ;; The current-source position too: loading a file advances it
                   ;; per form, and the requiring file's next error (a second,
                   ;; missing require in the same ns form) must not be blamed on
                   ;; the dependency's last form. Bound around the WHOLE load, so a
                   ;; throw restores it as well — the failing form's position
                   ;; travels with the throw instead (the handler below), which is
                   ;; what the uncaught report prints. Restoring on a normal
                   ;; return only, as this used to, left a throw that was CAUGHT
                   ;; (a data_readers namespace the loader tolerates, a require
                   ;; in a try) pinning the position on the file that failed, and
                   ;; every later, unrelated error was reported "at" it.
                   (jolt-current-source (jolt-current-source))
                   ;; Tee into the AOT capture only while loading the file that
                   ;; capture was opened for. A nested load must not append its
                   ;; forms to the requiring namespace's artifact: that artifact
                   ;; already re-runs the require which loads the nested namespace,
                   ;; so the copy is a SECOND definition of it, replayed after the
                   ;; require — and a top-level registration in it then lands on
                   ;; top of whatever the requiring namespace layered over it.
                   ;; jolt.time (a 14-line ns form) baked in eight install-owned
                   ;; jolt/time/*.clj namespaces this way; on a cache hit
                   ;; jolt.time.local's ISO-only java.time.LocalDate/parse replayed
                   ;; after jolt.time.fmt's pattern-aware override and undid it, so
                   ;; a warm cache silently ignored a DateTimeFormatter. Only
                   ;; install-owned namespaces leaked: a cacheable one opens its own
                   ;; capture (aot-capture-load) and so already redirected.
                   (jolt-aot-capture (and (equal? path (jolt-aot-capture-file))
                                          (jolt-aot-capture))))
      (ldr-with-file-vars path
        (lambda ()
          ;; The failing form's position, recorded before the stack unwinds (an
          ;; exception handler runs at the raise; a guard runs after) and keyed
          ;; by the raised object, so the report can ask for it back. The
          ;; innermost load records first and outer ones keep its answer.
          ;; raise-continuable, so a continuable raise (a compiler warning)
          ;; resumes exactly as it would without this handler.
          (with-exception-handler
            (lambda (e) (jolt-note-throw-source! e) (raise-continuable e))
            (lambda ()
              ;; rdr-read-top, not rdr-read-form: a stray close delimiter is a
              ;; READ ERROR at a file's top level, and only the top-level entry
              ;; says so. rdr-read-form leaves the position where it found the
              ;; `)`, and the (> j i) guard below reads no progress as end of
              ;; input — so one extra paren silently DROPPED the rest of the file
              ;; and the run exited 0. A test file that lost its whole body that
              ;; way still looked like a pass. The JVM raises "Unmatched
              ;; delimiter: )" here (jolt-3amm).
              (let loop ((i 0) (ord 0))
                (when (< i end)
                  (let-values (((form j) (rdr-read-top src i end)))
                    (when (> j i)
                      ;; ord counts every top-level form read (the ns form
                      ;; included): it is the def-ordinal clock for the build's
                      ;; visibility replay — rt.ss var-def-ordinals, stamped via
                      ;; jolt-load-frames (NOT the gate: fibers share a
                      ;; parameter cell, and the gate must stay off outside a
                      ;; build's walks). PUSHED, not set: a file loaded from
                      ;; inside this form — (load "impl") in a multi-file
                      ;; namespace — defines vars that become visible to the
                      ;; rest of THIS file at THIS ordinal, and its own frame
                      ;; alone cannot say that. The frame carries the namespace
                      ;; current as the form starts, so a nested REQUIRE's defs
                      ;; (another namespace) claim no ordinal here. Bound for the
                      ;; form's whole compile+eval (a macro expanding to defs
                      ;; stamps at its call form's ordinal), then bumped. One
                      ;; tail call: an eof placeholder read consumes no ordinal.
                      (if (rdr-eof? form)
                          (loop j ord)
                          (begin
                            (when (getenv "JOLT_TRACE_LOAD")
                              (display "  [load-form] " (current-error-port))
                              (display (jolt-pr-str form) (current-error-port)) (newline (current-error-port)))
                            (parameterize ((jolt-load-frames
                                             (cons (make-load-frame path ord (chez-current-ns))
                                                   (jolt-load-frames))))
                              (jolt-compile-eval-form (if data-readers-active (ldr-apply-readers form) form)
                                                      (chez-current-ns)))
                            (loop j (fx+ ord 1)))))))))))))))

;; --- AOT / compile cache for required namespaces ----------------------------
;; A disk-backed namespace is recompiled from source on EVERY run (load-jolt-file
;; → analyze+emit+eval per form). This cache fasls the emitted Scheme on the first
;; load of a namespace and `load`s the .so on subsequent loads, recovering most of
;; that per-run compile cost. The cache FILENAME embeds a content hash of the
;; source, so any edit (same path, different bytes) misses automatically — no
;; mtime tracking needed. Gated by JOLT_AOT_CACHE; OFF for install-owned source
;; (embedded in the binary) and bypassed on :reload / :reload-all (live editing).
(define (aot-cache-dir)
  (or (getenv "JOLT_CACHE_DIR")
      (string-append (or (getenv "HOME") ".") "/.jolt/aot-cache")))
;; Default ON — a built jolt benefits every run (ys-style startup pulling many
;; library namespaces). JOLT_AOT_CACHE=0/false/no/off opts out. The dev bin/jolt
;; script exports JOLT_AOT_CACHE=0, so source-mode dev (a volatile compiler whose
;; "dev" version tag would NOT invalidate the cache across edits, and whose
;; startup is already covered by the devboot cache) stays OFF by default.
(define (aot-cache-enabled?)
  (let ((e (getenv "JOLT_AOT_CACHE")))
    (if (and (string? e) (fx>? (string-length e) 0))
        (not (or (string=? e "0") (string-ci=? e "false")
                 (string-ci=? e "no") (string-ci=? e "off")))
        #t)))   ; unset/empty → default ON
;; A cached fasl is only valid for the runtime that emitted it, and the version
;; string alone does not pin one: `git describe` reports the same "…-dirty" for
;; every edit in a working tree, so successive builds out of one checkout all
;; share a key and each happily loads the previous runtime's output. Mix in a
;; fingerprint of the runtime itself. A binary bakes one over its whole emitted
;; image (build-jolt.ss); running from a checkout there is none, so hash the
;; runtime sources on disk instead. #f means we could not identify the runtime —
;; the cache stays off rather than key on something that doesn't distinguish it.
(define aot-runtime-source-dirs '("host/chez" "host/chez/java" "host/chez/seed"))
(define (aot-ss-file? f)
  (let ((n (string-length f)))
    (and (> n 3) (string=? (substring f (- n 3) n) ".ss"))))
;; 32-bit multiply-accumulate over each file's length and content hash, folded in
;; sorted path order so the result is reproducible across runs and machines.
(define (aot-hash-mix a b) (bitwise-and (+ (* a 1000003) b) #xFFFFFFFF))
;; FNV-1a 32-bit — every byte contributes. equal-hash on a string is a
;; bounded-sample hash (~26 bytes regardless of length), so a same-length edit
;; away from the sampled offsets produces a cache collision.  This replaces it.
(define (aot-content-hash s)
  (let ((n (string-length s)))
    (let loop ((i 0) (h 2166136261))
      (if (fx=? i n)
          h
          (loop (fx+ i 1)
                (fxlogand (fx* (fxlogxor h (char->integer (string-ref s i)))
                               16777619)
                          #xFFFFFFFF))))))
;; The same hash over BYTES. A file a compile read is whatever the program said it
;; was — a .sql migration, an .edn config, a PNG — so it cannot be decoded as text
;; first; the third would raise.
(define (aot-bytes-hash bv)
  (let ((n (bytevector-length bv)))
    (let loop ((i 0) (h 2166136261))
      (if (fx=? i n)
          h
          (loop (fx+ i 1)
                (fxlogand (fx* (fxlogxor h (bytevector-u8-ref bv i))
                               16777619)
                          #xFFFFFFFF))))))
(define (aot-source-fingerprint)
  (let loop ((dirs aot-runtime-source-dirs) (h 17) (n 0))
    (if (null? dirs)
        (and (fx>? n 0) (string-append (number->string n 16) "-" (number->string h 16)))
        (let ((files (sort string<?
                           (filter aot-ss-file?
                                   (if (file-directory? (car dirs))
                                       (directory-list (car dirs))
                                       '())))))
          (let inner ((fs files) (h h) (n n))
            (if (null? fs)
                (loop (cdr dirs) h n)
                (let* ((p (string-append (car dirs) "/" (car fs)))
                       ;; an unreadable file just doesn't contribute; a real
                       ;; runtime change still moves some other file's hash.
                       (s (guard (e (else #f)) (read-file-string p))))
                  (if s
                      (inner (cdr fs)
                             (aot-hash-mix (aot-hash-mix h (string-length s)) (aot-content-hash s))
                             (fx+ n 1))
                      (inner (cdr fs) h n)))))))))
;; Computed at most once per process, and only when the cache is consulted, so
;; the source-tree walk never costs a run with the cache off (the default in the
;; dev bin/jolt) or a binary, which reads its baked value.
(define aot-fingerprint-memo 'unset)
(define (aot-runtime-fingerprint)
  (when (eq? aot-fingerprint-memo 'unset)
    (set! aot-fingerprint-memo
      (or (sa-baked-global 'jolt-baked-runtime-fingerprint)
          (aot-source-fingerprint))))
  aot-fingerprint-memo)
;; …-tr when the tail-frame history is on. Whether tracing is enabled changes the
;; CODE the emitter produces — an entry prologue per fn, plus a ring save/restore
;; around every non-tail call that can reach one — so a fasl is only valid for the
;; mode it was compiled under, and the generation has to say which. It did not, so
;; the two modes shared a generation and each was served the other's artifacts. Both
;; directions were wrong, and the frame-losing one is the worse: a traced run after
;; an untraced one reported NO history frames, the feature silently turned off by a
;; cache hit. (The reverse merely paid tracing's cost with tracing nominally off,
;; which is how it surfaced.) Read from the runtime flag rather than from JOLT_TRACE
;; directly, so a REPL that calls jolt.host/enable-trace! with the env var unset is
;; keyed correctly too. trace-smoke.sh gates both directions.
(define (aot-trace-tag) (if jolt-trace-on? "-tr" ""))
;; <dir>/<jolt-version>-<runtime fingerprint>[-tr]/v1 — the version names the release
;; a fasl came from, the fingerprint pins the exact runtime that emitted it. One
;; such GENERATION per runtime; rebuilding jolt starts a new one and strands the
;; last, so the first consult in a process also collects the superseded ones.
(define (aot-generation-dir)
  (string-append (aot-cache-dir) "/" (jolt-version-string)
                 "-" (aot-runtime-fingerprint) (aot-trace-tag)))
;; Generations are kept by LAST USE, not by age: a marker file refreshed once per
;; process is the only evidence a generation is still someone's, since a run that
;; hits on everything never writes to it.
(define aot-generations-kept 3)
(define aot-prune-grace-seconds 60)
(define (aot-used-marker gen) (string-append gen "/.used"))
(define (aot-touch-used! gen)
  (guard (e (else #f))
    (let ((out (open-output-file (aot-used-marker gen) 'replace)))
      (put-string out "jolt aot cache generation\n")
      (close-port out))))
(define (aot-file-seconds path)
  (guard (e (else 0))
    (div (sa-file-mtime-ms path) 1000)))
(define (aot-delete-tree path)
  (guard (e (else #f))
    (if (and (file-directory? path) (not (file-symbolic-link? path)))
        (begin
          (for-each (lambda (f) (aot-delete-tree (string-append path "/" f)))
                    (directory-list path))
          (delete-directory path))
        (delete-file path #f))))
;; Drop every generation that is neither the current one nor among the few most
;; recently used. The grace period keeps a generation another live process may be
;; midway through: worst case that process misses and recompiles (mkdir -p and the
;; corrupt-fasl guard both recover), so this is a throughput risk, never a crash.
(define (aot-prune-generations! current)
  (guard (e (else #f))
    (let* ((root (aot-cache-dir))
           (now (aot-file-seconds (aot-used-marker current)))
           (gens (filter (lambda (p)
                           (and (not (string=? (cdr p) current))
                                (file-directory? (cdr p))
                                (fx>? (- now (car p)) aot-prune-grace-seconds)))
                         (map (lambda (d)
                                (let ((p (string-append root "/" d)))
                                  (cons (aot-file-seconds (aot-used-marker p)) p)))
                              (if (file-directory? root) (directory-list root) '()))))
           ;; newest first; the current generation holds one of the kept slots
           (ordered (sort (lambda (a b) (> (car a) (car b))) gens))
           (doomed (if (fx>? (length ordered) (fx- aot-generations-kept 1))
                       (list-tail ordered (fx- aot-generations-kept 1))
                       '())))
      (for-each (lambda (p)
                  (aot-info (string-append "pruning superseded generation " (cdr p)))
                  (aot-delete-tree (cdr p)))
                doomed))))
;; The generation is stamped and swept once per process, on the first consult.
(define aot-generation-memo #f)
(define (aot-cache-subdir)
  (unless aot-generation-memo
    (let ((gen (aot-generation-dir)))
      (aot-mkdir-p gen)
      (aot-touch-used! gen)
      (aot-prune-generations! gen)
      (set! aot-generation-memo (string-append gen "/v1"))))
  aot-generation-memo)
(define (aot-cache-sanitize s)
  (list->string
    (map (lambda (c)
           (let ((n (char->integer c)))
             (if (or (and (>= n 48) (<= n 57))    ; 0-9
                     (and (>= n 65) (<= n 90))    ; A-Z
                     (and (>= n 97) (<= n 122))   ; a-z
                     (char=? c #\-) (char=? c #\.) (char=? c #\_))
                 c #\_)))
         (string->list s))))
;; length (hex) + full-content FNV-1a 32-bit hash (hex). FNV-1a is process-STABLE
;; (no randomized seed), so the key is reproducible across runs and machines —
;; required for the cache to hit at all. The length prefix stays as a cheap second
;; factor: a false share needs a genuine 32-bit collision BETWEEN SOURCES OF EQUAL
;; LENGTH, which is remote enough to sit below other failure modes here.
;;
;; This was equal-hash, which is NOT a content hash: Chez samples a bounded ~26
;; characters (first 6, ~15 strided, last 5) no matter how long the string is, so
;; for any real source ~99% of the bytes were invisible to the key. Length was
;; doing all the invalidation work, and any length-preserving edit — 42→99, <→>,
;; inc→dec, a rename to an equal-length name — kept the key and silently served
;; the previous fasl. Do not "optimize" this back to a sampling hash: the whole
;; cost is one linear pass over source jolt is about to compile anyway.
(define (aot-cache-key src)
  (string-append (number->string (string-length src) 16) "-"
                 (number->string (aot-content-hash src) 16)))
(define (aot-info msg)
  (when (getenv "JOLT_DEBUG")
    (display (string-append "[jolt.aot] " msg "\n") (current-error-port))))

;; --- dependency closure ------------------------------------------------------
;; A namespace's fasl bakes in what its dependencies contributed at compile time —
;; macro expansions, inlined defs, record shapes — so its own source hash does not
;; describe it. Editing a macro namespace left every consumer serving expansions of
;; a definition that no longer existed. The key therefore folds in the key of each
;; namespace this one required, which makes it transitive by construction: a change
;; three namespaces down moves each key on the path.
;;
;; The requires are learned by RECORDING them during the compile that produced the
;; fasl, not by re-parsing ns forms: the loader funnels every require/use through
;; ns-load+register (ns.ss), so the record covers a top-level (require …) and a
;; :require clause alike. They are written beside the fasl, in a sidecar named by
;; the namespace's own hash alone — the one key derivable before its deps are known.
(define aot-dep-sink (make-thread-parameter #f))
(define (aot-new-dep-sink) (vector '()))
;; Called from the require/use load step for every target, whether or not the
;; target was already loaded — a dedup'd require is still a dependency.
(define (aot-record-dep! name)
  (let ((sink (aot-dep-sink)))
    (when (and (vector? sink) (not (member name (vector-ref sink 0))))
      (vector-set! sink 0 (cons name (vector-ref sink 0))))))

(define (aot-dep-sidecar base) (string-append base ".deps"))
(define (aot-write-dep-list! path names)
  (guard (e (else #f))
    (let ((out (open-output-file path 'replace)))
      (for-each (lambda (n) (put-string out n) (put-string out "\n")) names)
      (close-port out))))
(define (aot-read-dep-list path)
  (if (file-exists? path)
      (guard (e (else '()))
        (filter (lambda (s) (fx>? (string-length s) 0))
                (bld-string-lines-like (read-file-string path))))
      '()))
;; --- files read at compile time ----------------------------------------------
;; The same argument as the dependency closure, for the other thing a compile can
;; bake in. A macro that slurps a SQL migration embeds that file's CONTENTS in the
;; artifact; keying on the .clj alone left every consumer serving the previous
;; build's statements out of the cache, with nothing to notice — the .clj is
;; untouched, so its hash still matches (jolt#576).
;;
;; The reads are RECORDED during the compile, the way the requires are: io.ss's
;; io-file-read-sink is bound around the capture load and every user-facing read
;; (slurp, io/reader, io/input-stream, io/resource, Files/read*) announces its
;; path. That covers a path spelled any of the ways a program can spell one,
;; without the loader having to know which.
;;
;; The sidecar is written only when the namespace actually read something, so the
;; ordinary namespace carries no extra file — and it is DELETED when a recompile
;; reads nothing, since it is named by the own hash and a stale one would
;; otherwise still be found under an unchanged source.
(define (aot-res-sidecar base) (string-append base ".res"))
(define (aot-write-res-list! path paths)
  (if (null? paths)
      (guard (e (else #f)) (delete-file path #f))
      (aot-write-dep-list! path paths)))
;; What one recorded file contributes: its length and content, or 0 when it isn't
;; there. ABSENT has to be a value of its own rather than "no contribution":
;; (io/resource "003.sql") answering nil is a compile-time answer, and adding the
;; file has to move the key. An empty file hashes to something else (the fold of a
;; zero length with FNV's basis), so the two never collide.
(define (aot-file-digest path)
  (guard (e (else 0))
    (if (if (jar-path? path) (jar-path-exists? path) (file-exists? path))
        (let ((bv (read-file-bytes path)))
          (aot-hash-mix (bytevector-length bv) (aot-bytes-hash bv)))
        0)))
;; The PATH is folded alongside its content, so gaining or losing an entry moves
;; the digest even when the contents happen to coincide. Sorted, like the deps, so
;; the result doesn't depend on the order the reads happened to be recorded in.
(define (aot-res-fold paths)
  (fold-left (lambda (h p)
               (aot-hash-mix (aot-hash-mix h (aot-content-hash p)) (aot-file-digest p)))
             17 (sort string<? paths)))
;; A dependency's recorded reads, for the transitive fold. Memoized per process
;; like the dep digest — a namespace required from several places is hashed once.
(define aot-res-digest-memo (make-hashtable string-hash string=?))
(define (aot-res-digest name own)
  (or (hashtable-ref aot-res-digest-memo name #f)
      (let ((d (aot-res-fold (aot-read-dep-list
                               (aot-res-sidecar (aot-base-for-own name own))))))
        (jolt-with-mutex ldr-tbl-mu (hashtable-set! aot-res-digest-memo name d))
        d)))

;; local line splitter — build.ss's is not loaded on the run path.
(define (bld-string-lines-like s)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((fx>=? i n) (reverse (if (fx>? i start) (cons (substring s start i) acc) acc)))
            ((char=? (string-ref s i) #\newline)
             (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) acc)))
            (else (loop (fx+ i 1) start acc))))))

;; A namespace contributes to a key only if it is cacheable: install-owned source
;; (stdlib, jolt-core) is part of the runtime and already covered by the runtime
;; fingerprint, so it is skipped — which keeps the walk to project and library
;; namespaces instead of the whole prelude.
(define (aot-cacheable-file name)
  (let ((f (find-ns-file name)))
    (and f (not (ldr-install-file? f)) f)))
(define aot-own-key-memo (make-hashtable string-hash string=?))
(define (aot-own-key name)
  (or (hashtable-ref aot-own-key-memo name #f)
      (let ((f (aot-cacheable-file name)))
        (and f (let ((k (aot-cache-key (ldr-read-source f))))
                 (jolt-with-mutex ldr-tbl-mu (hashtable-set! aot-own-key-memo name k))
                 k)))))
(define (aot-base-for-own name own) (string-append (aot-cache-subdir) "/"
                                                   (aot-cache-sanitize name) "-" own))
;; Fold the dep keys into one integer. Sorted, so the digest doesn't depend on the
;; order requires happened to be recorded in.
(define aot-dep-digest-memo (make-hashtable string-hash string=?))
;; The in-flight set is one walk's recursion stack — it exists only to stop
;; aot-dep-digest recursing forever on a require cycle — so it is keyed by THREAD
;; as well as namespace. Shared across threads it got the question wrong in both
;; directions once unrelated namespaces could load in parallel: another thread's
;; mark read here as a cycle and truncated this namespace's digest to its own
;; hash, which is a different and wrong cache key (an edit two deps down no longer
;; moves it, so a stale .so hits), and another thread's delete cleared the guard
;; out from under a walk that really was cyclic, which then recursed until the
;; stack gave out. Nothing is shared between the walks, so there is nothing here
;; two threads need to agree on.
(define aot-dep-inflight (make-hashtable string-hash string=?))
(define (aot-inflight-key name)
  (string-append (number->string (get-thread-id)) "\x0;" name))
(define (aot-ns-digest name)
  ;; a namespace's whole contribution: its own hash, the files its compile read,
  ;; and its deps'. All three so a change anywhere below reaches every consumer.
  (let ((own (aot-own-key name)))
    (if (not own)
        0
        (aot-hash-mix (aot-hash-mix (equal-hash own) (aot-res-digest name own))
                      (aot-dep-digest name own)))))
(define (aot-dep-digest name own)
  (or (hashtable-ref aot-dep-digest-memo name #f)
      (let ((ik (aot-inflight-key name)))
        (if (hashtable-ref aot-dep-inflight ik #f)
            ;; a require cycle: stop at the namespace's own hash. Every namespace on
            ;; the cycle still contributes it, so an edit anywhere still moves the key.
            (equal-hash own)
            (begin
              (jolt-with-mutex ldr-tbl-mu (hashtable-set! aot-dep-inflight ik #t))
              (let* ((deps (sort string<? (aot-read-dep-list
                                            (aot-dep-sidecar (aot-base-for-own name own)))))
                     (d (fold-left (lambda (h dep) (aot-hash-mix h (aot-ns-digest dep))) 17 deps)))
                (jolt-with-mutex ldr-tbl-mu
                  (hashtable-delete! aot-dep-inflight ik)
                  (hashtable-set! aot-dep-digest-memo name d))
                d))))))
;; The full cache base: own hash, then the dep digest folded with the digest of
;; the files this compile read. A namespace with no recorded deps or reads (or
;; none cacheable) folds to the digest of the empty list, so the suffix is
;; constant for the whole leaf case rather than absent.
(define (aot-base-full name own deps res)
  (string-append (aot-base-for-own name own) "-"
                 (number->string
                   (aot-hash-mix
                     (fold-left (lambda (h dep) (aot-hash-mix h (aot-ns-digest dep))) 17
                                (sort string<? deps))
                     (aot-res-fold res))
                   16)))
;; Tee the per-form emitted Scheme (compile-eval.ss jolt-aot-capture) while running
;; the normal load loop, so a cache miss reproduces the EXACT interleaved analyze
;; →eval semantics (forward macro refs and same-file requires expand correctly).
;; The captured Scheme, loaded in order, reproduces every top-level effect — the
;; same property `jolt build` / the seed mint rely on.
;; parameterize (not a bare set!) so a require fired by this ns's forms — which
;; itself triggers a nested aot-capture-load — restores OUR capture port on
;; return. A bare thread-parameter set would be clobbered by the nested capture
;; and reset to #f, dropping this ns's forms AFTER the require (the require's
;; target would cache, but the requiring ns's own defs would vanish from its .so).
(define (aot-capture-load file src)
  (ldr-need-compiler! file)
  (let ((cap (open-output-string)))
    (parameterize ((jolt-aot-capture cap) (jolt-aot-capture-file file))
      (load-jolt-file* file src)
      (get-output-string cap))))
;; On a miss: run the capture load (which also evals the ns into the running
;; image), then fasl the captured Scheme. A compile-file failure is non-fatal —
;; the ns is already loaded; we just skip caching this run and miss again next.
;; mkdir -p without a subprocess. The built Windows jolt runs jolt-sh via
;; cmd.exe, where `mkdir -p <forward/slash/path>` is invalid syntax, so the
;; cache dir was never created and open-output-file failed. Native mkdir +
;; path-parent recursion is portable (mirrors build.ss bld-mkdir-p).
(define (aot-mkdir-p dir)
  (unless (or (string=? dir "") (string=? dir "/") (string=? dir ".") (file-exists? dir))
    (aot-mkdir-p (path-parent dir))
    ;; tolerate the benign race (created concurrently); re-raise a real failure.
    (guard (e (#t (unless (file-exists? dir) (raise e))))
      (mkdir dir))))
;; Publish the cached .scm/.so atomically. Parallel jolt processes (e.g. the cts
;; gate's workers) share one cache dir and can compile the SAME namespace at once;
;; writing straight to base.so let a reader's (file-exists? so)+load see a fasl
;; another process was mid-write, loading a truncated image that defined nothing
;; ("No namespace: X found"). So each process compiles to a pid-unique temp and
;; rename(2)s it into place — atomic within a filesystem, so the .so appears
;; complete-or-not-at-all. The .so (the cache-hit signal) is published LAST.
;; The .scm is also published via temp + rename, landing at its FINAL name BEFORE
;; the compile: compile-file bakes the name of the file it read into the frame
;; source objects, so compiling the temp would leave every cached frame pointing
;; at a path deleted by the rename. Temp+rename keeps a concurrent compiler from
;; ever seeing a half-written .scm (two processes race on base.scm; each sees a
;; complete file, and the content is a pure function of the key, so both write
;; the same bytes anyway).
;; The final base is only known AFTER the capture load: it folds in the deps, and
;; the deps are what that load records. Writing under the pre-load base instead
;; would strand the fasl — the next run computes the key WITH deps and misses it,
;; paying a second compile for every namespace that requires anything.
;; Completion markers. compile-file writes a fasl as a SEQUENCE of compiled
;; top-level forms, and `load` of a file cut exactly at a form boundary succeeds
;; silently, running only a prefix of the namespace — the corrupt-fasl guard
;; below never fires, and the partial artifact is served on every later run
;; while a plain source load works. So the published .scm ends with a marker
;; call naming the namespace, and a hit-load that finishes without its marker is
;; treated exactly like a corrupt fasl: drop the artifact, recompile from
;; source. Per-name (not one flag), because a namespace's fasl re-runs its own
;; requires, and a nested cache hit's marker would satisfy a shared flag even
;; when the OUTER fasl is the truncated one. Artifacts published before markers
;; existed read as incomplete and recompile once — self-migrating.
(define aot-complete-tbl (make-hashtable string-hash string=?))
(define (aot-mark-complete! name)
  (jolt-with-mutex ldr-tbl-mu (hashtable-set! aot-complete-tbl name #t)))
(define (aot-complete-reset! name)
  (jolt-with-mutex ldr-tbl-mu (hashtable-delete! aot-complete-tbl name)))
(define (aot-complete? name)
  (jolt-with-mutex ldr-tbl-mu (hashtable-ref aot-complete-tbl name #f)))

(define (aot-compile-and-cache name file src own)
  (let ((sink (aot-new-dep-sink))
        ;; the files this compile reads, collected the same way and for the same
        ;; reason (io.ss io-file-read-sink). A nested require binds its own, so a
        ;; dependency's reads are recorded against the dependency.
        (res-sink (vector '())))
    (let* ((stamps (vector file '()))
           (captured (parameterize ((aot-dep-sink sink) (io-file-read-sink res-sink)
                                    (jolt-def-ordinal-sink stamps))
                       (aot-capture-load file src))))
      (unless (and (string? captured) (fx>? (string-length captured) 0))
        (aot-info (string-append "nothing captured for " name ", not caching")))
      (when (and (string? captured) (fx>? (string-length captured) 0))
        (let* ((deps (filter aot-cacheable-file (vector-ref sink 0)))
               (res (vector-ref res-sink 0))
               (base (aot-base-full name own deps res))
               (scm (string-append base ".scm"))
               (so  (string-append base ".so"))
               (pid (number->string (get-process-id)))
               (tmp-scm (string-append base ".tmp" pid ".scm"))
               (tmp-so  (string-append base ".tmp" pid ".so")))
          (aot-mkdir-p (path-parent base))
          ;; the sidecars are named by the own hash alone, so the next run can read
          ;; them back before it is able to compute the full key.
          (aot-write-dep-list! (aot-dep-sidecar (aot-base-for-own name own)) deps)
          (aot-write-res-list! (aot-res-sidecar (aot-base-for-own name own)) res)
          ;; this run already computed digests for `name` from the OLD sidecars;
          ;; drop them so a later require in the same process sees the new ones.
          (jolt-with-mutex ldr-tbl-mu
            (hashtable-delete! aot-dep-digest-memo name)
            (hashtable-delete! aot-res-digest-memo name))
          (guard (e (else (aot-info (string-append "compile failed for " name))
                          (delete-file tmp-scm #f) (delete-file tmp-so #f) #f))
            (let ((out (open-output-file tmp-scm 'replace)))
              (put-string out captured)
              ;; the first-def stamps this load made, replayed against whatever
              ;; file the artifact is loaded for (aot-replay-def-ordinals!)
              (put-string out (format "\n(aot-replay-def-ordinals! '~s)" (reverse (vector-ref stamps 1))))
              (put-string out (format "\n(aot-mark-complete! ~s)\n" name))
              (close-output-port out))
            (rename-replace! tmp-scm scm)
            ;; compile-file prints "compiling X with output to Y" per file to
            ;; current-output-port by default — swallow it so a cache miss can't
            ;; corrupt the running program's stdout.
            (parameterize ((current-output-port (open-output-string)))
              (sa-compile-file scm tmp-so #f))
            (rename-replace! tmp-so so))
          (unless (file-exists? so)
            (aot-info (string-append "no .so produced for " name))))))))
;; Evaluate a namespace's top-level forms from COMPILED code — an embedded fasl,
;; an AOT-cached .so, a classpath artifact. RT.load brackets a compiled class's
;; init with the compiler-flag vars exactly as Compiler.load brackets a source
;; load, and so does `jolt build` for the namespaces it AOTs into a binary
;; (jolt-ns-load-vars-push! is the same frame ldr-with-file-vars establishes for
;; source). The loader's own compiled paths were the ones left out.
;;
;; Without the frame, a namespace whose top level does (set! *warn-on-reflection*
;; true) — the standard idiom in ported Clojure libraries, and what both vendored
;; babashka namespaces do — writes the ROOT binding and raises. Every caller here
;; reads that raise as a broken artifact, so the failure is invisible and
;; permanent: the embedded fasl silently recompiled babashka.fs and
;; babashka.process from source on every process start, and a cached .so deleted
;; and rebuilt itself on every run without ever once being served. It only ever
;; worked when some enclosing file load happened to have the frame up already,
;; which is why loading such a namespace from a script looked fine and loading it
;; from -e, a REPL, or an nREPL eval did not.
(define (ldr-with-compiled-ns-vars thunk)
  (jolt-with-ns-load-vars thunk))

;; " (msg)" for a diagnostic line, or "" when the message can't be read — the
;; jolt.host seam is absent in a bootstrap image, and a diagnostic may not throw.
(define (ldr-condition-suffix e)
  (let ((m (guard (_ (#t #f))
             (let ((m ((var-deref "jolt.host" "condition-message") e)))
               (and (string? m) m)))))
    (if m (string-append " (" m ")") "")))

;; A garbled .so makes `load` throw; one cut at a form boundary loads fine and
;; just stops early, which the completion marker catches instead. Either way:
;; delete the bad files and recompile from source. Recompiling after a partial
;; load is safe — the prefix's defs are idempotent (def-var! replaces roots, a
;; require dedups) and the fresh full load re-runs them; install-owned
;; namespaces, whose defs DO get set!-overridden after load, never take this
;; path at all. Non-fatal — a repeated failure just misses every run.
;; The file a cached artifact is being loaded for, while it loads.
(define aot-loading-file (make-parameter #f))
;; Replay the first-def stamps a source load of the artifact's file made (rt.ss
;; var-def-ordinals): a cached load runs compiled defs outside the reader walk
;; that stamps them, and a build's pass 1 loading from the cache would otherwise
;; hand the emit walk an unstamped program — the #451 forward-reference
;; divergence. First writer wins, as for a source load.
(define (aot-replay-def-ordinals! stamps)
  (let ((file (aot-loading-file)))
    (when file
      (for-each (lambda (s) (var-def-ordinal-stamp1! file (car s) (cadr s) (caddr s)))
                stamps))))

(define (aot-safe-load-or-recompile name file src own base)
  (let ((so (string-append base ".so")))
    (define (recover! why)
      (aot-info (string-append why " cache for " name ", recompiling"))
      (delete-file so #f)           ; best-effort; ignore if already gone
      (delete-file (string-append base ".scm") #f)
      (aot-compile-and-cache name file src own))
    (let ((state (guard (e (else 'corrupt))
                   (aot-complete-reset! name)
                   (parameterize ((aot-loading-file file))
                     (ldr-with-compiled-ns-vars (lambda () (load so))))
                   (if (aot-complete? name) 'ok 'incomplete))))
      (case state
        ((ok) (aot-complete-reset! name))     ; done with the entry
        ((incomplete) (recover! "incomplete"))
        (else (recover! "corrupt"))))))
;; Load an embedded compiled fasl for `name` if one was baked into this binary.
;; The release jolt embeds one fasl per install-owned stdlib namespace so a
;; require never recompiles from source on process start. The embedded bytes
;; are produced by the same runtime that consumes them (build-jolt compiles them
;; in a fresh Chez over this same runtime image), so there is no fingerprint
;; check here — unlike the on-disk AOT cache, an embedded fasl cannot be stale
;; relative to the binary it ships in. A guard falls back to the caller's source
;; path: the bytes are baked (never truncated by a killed write), but a guard
;; beats a dead binary. Returns #t when the embedded fasl was loaded, else #f.
 (define (jolt-load-embedded-fasl! name)
   (and (not (ldr-source-only?))
        (let ((bv (jolt-embedded-fasl name)))
          (and bv
               (begin
                 ;; Make success explicit: load-compiled-from-port returns the
                 ;; fasl's LAST expression value, which can be #f for a ns whose
                 ;; final form evaluates to nil/false. A #f read as "failed" so
                 ;; the caller reloaded the namespace from source ON TOP of the
                 ;; already-loaded fasl — the override-replay bug class at
                 ;; loader.ss:~432. #t is the real success signal.
                 ;;
                 ;; The aot-info line reports the OUTCOME, not the attempt. It
                 ;; used to print before the load, so it said "embedded" just as
                 ;; loudly for a fasl that raised on its first form and sent the
                 ;; whole namespace to the source compiler — which is exactly
                 ;; what both babashka namespaces did, unnoticed, for as long as
                 ;; they have been embedded.
                 (guard (e (else (aot-info (string-append "embedded " name
                                                          " FAILED to load"
                                                          (ldr-condition-suffix e)
                                                          ", falling back to source"))
                                 #f))
                   (ldr-with-compiled-ns-vars
                     (lambda () (load-compiled-from-port (open-bytevector-input-port bv))))
                   (aot-info (string-append "embedded " name))
                   #t))))))

;; Dispatch for load-namespace*: embedded fasl (install-owned ns in a built
;; binary) / cache hit (load .so) / miss (compile+cache) / bypass (plain load).
;; `file` is the resolved on-disk path. force? (:reload) and ldr-reload-all?
;; bypass the cache and the embedded branch alike — live editing must win over
;; either a stale cache or a stale embedded fasl. ldr-source-only? is honored so
;; the build driver (jb-emit-cli-ns) keeps loading source to emit it.
(define (aot-load-or-compile name file force?)
  (cond
    ;; install-owned + an embedded fasl baked in: load the compiled code.
    ;; Sinks are #f like a cache hit: the fasl re-runs its own requires and
    ;; reads, which the bytes already describe, so there is nothing to capture.
    ((and (not force?) (not (ldr-reload-all?))
          (ldr-install-file? file)
          (jolt-embedded-fasl name))
     (parameterize ((aot-dep-sink #f) (io-file-read-sink #f))
       (unless (jolt-load-embedded-fasl! name)
         ;; embedded fasl registered but failed to load: fall back to source.
         (load-jolt-file file))))
    ((and (aot-cache-enabled?) (not force?) (not (ldr-reload-all?))
          (or (not (ldr-source-only?)) (ldr-build-aot-cache?))
          (not (ldr-install-file? file))
          ;; no fingerprint = we can't tell this runtime from another one, so
          ;; there is no key that would be safe to reuse.
          (aot-runtime-fingerprint))
     ;; the deps and reads this ns's own load records belong to IT, not to
     ;; whoever is requiring it — bind fresh sinks so a nested load can't append
     ;; to the enclosing one. A hit binds #f on both: the fasl it loads re-runs
     ;; the requires and re-does the reads, and those are already described by
     ;; this namespace's own sidecars.
     (let* ((src (ldr-read-source file))
            (own (aot-cache-key src))
            (obase (aot-base-for-own name own))
            (base (aot-base-full name own
                                 (aot-read-dep-list (aot-dep-sidecar obase))
                                 (aot-read-dep-list (aot-res-sidecar obase))))
            (so (string-append base ".so")))
       (if (file-exists? so)
           (begin (aot-info (string-append "hit " name))
                  (parameterize ((aot-dep-sink #f) (io-file-read-sink #f))
                    (aot-safe-load-or-recompile name file src own base)))
           (begin (aot-info (string-append "miss " name))
                  (aot-compile-and-cache name file src own)))))
    (else (parameterize ((aot-dep-sink #f) (io-file-read-sink #f)) (load-jolt-file file)))))

;; Mark a namespace as loaded in both the host hashtable and the *loaded-libs* ref.
;; Namespaces defined by the CLI's OWN AOT closure (bld-emit-cli-aot bakes
;; jolt.main, jolt.deps and their on-demand requires into the CLI boot image and
;; marks each loaded). They really are preloaded in the jolt process — but an app
;; image written by `jolt build` is a DIFFERENT image and carries none of them,
;; so the app build must not skip them as "already in the image". Without this,
;; a ns that is in the CLI closure and neither in the runtime image nor the
;; stdlib-fasl manifest — jolt.ffi, jolt.mvn-http — has every var it defines
;; interned but UNBOUND in a built binary, while `jolt run` masks it by
;; compiling the source at require time.
(define ldr-cli-aot-ns (make-hashtable string-hash string=?))
(define (ldr-mark-cli-aot! name) (hashtable-set! ldr-cli-aot-ns name #t))
(define (ldr-cli-aot? name) (hashtable-ref ldr-cli-aot-ns name #f))

(define (ldr-mark-loaded! name)
  ;; the overlay has loaded, so a partly-seeded namespace is now complete and
  ;; joins find-ns / all-ns (ns.ss ns-deferred)
  (ns-undefer! name)
  (jolt-with-mutex ldr-tbl-mu (hashtable-set! loaded-ns name #t))
  (ldr-libs-update! (lambda (s) (pset-conj s (jolt-symbol #f name)))))

;; Undo ldr-mark-loaded! — a failed load rolls its mark back so a retry loads.
(define (ldr-unmark-loaded! name)
  (jolt-with-mutex ldr-tbl-mu (hashtable-delete! loaded-ns name))
  (ldr-libs-update! (lambda (s) (pset-disj s (jolt-symbol #f name)))))

;; Has `name` cleared the loaded-ns / *loaded-libs* dedup? A tools.namespace disj
;; from *loaded-libs* forces a reload even though loaded-ns still holds.
;;
;; txn-read and not jolt-ref-val, because the disj this is here to honour is a USER
;; write and a user writes a ref through the STM. tools.namespace disj'ing inside a
;; (dosync …) should force the reload from the next form onward, not from the
;; commit. Outside a transaction txn-read IS jolt-ref-val, so the ordinary path is
;; unchanged, and it acquires nothing, so this stays safe to call under ldr-load-mu
;; (see ldr-libs-update!'s "a load must never take stm-lock"). The loader's own
;; marks are not in any log — ldr-libs-update! writes the ref directly, for the
;; reasons set out there — so this sees them whether a transaction is open or not.
(define (ns-dedup-loaded? name)
  (let* ((libs-cell (var-cell-lookup "clojure.core" "*loaded-libs*"))
         (libs-ref (and libs-cell (var-cell-root libs-cell))))
    (and (hashtable-ref loaded-ns name #f)
         (or (not libs-ref)
             (not (jolt-ref? libs-ref))
             (jolt-contains? (txn-read libs-ref) (jolt-symbol #f name))))))

;; --- clojure.core/compile + *compile-path* -----------------------------------
;; Clojure's `compile` writes a namespace's compiled form under *compile-path* so a
;; later load takes that instead of re-reading the source, and that directory has to
;; be on the classpath for the load side to find it (core.clj's docstring says so
;; outright). jolt keeps the shape and swaps two substrate details: the artifact is
;; a Chez fasl of the emitted Scheme — what the AOT cache above already produces —
;; rather than .class files, and the load side searches the source roots, which are
;; jolt's classpath.
;;
;;   <dir>/<ns-rel>.so     the fasl; published last, so its presence is the signal
;;   <dir>/<ns-rel>.scm    the emitted Scheme it was compiled from
;;   <dir>/<ns-rel>.meta   what it was compiled against (below)
;;
;; RT.load picks a .class over a .clj on mtime alone. A fasl is much less forgiving
;; than a class file — one emitted by a different jolt calls runtime helpers that
;; may not exist any more — so .meta pins the jolt version and the runtime
;; fingerprint, and a mismatch means the artifact is ignored rather than loaded in
;; hope. Staleness against the source is a content hash, not an mtime: same intent
;; as the JVM's comparison, immune to a bare touch, and the same rule the AOT cache
;; decides by. Like the JVM this does NOT walk the whole dependency graph — .meta
;; records the direct requires and their source hashes, so editing a namespace this
;; one requires invalidates it, but a change further down does not. Recompile
;; dependents, the same discipline an edited macro namespace needs under JVM AOT.
(define (cpath-so-file base) (string-append base ".so"))
(define (cpath-scm-file base) (string-append base ".scm"))
(define (cpath-meta-file base) (string-append base ".meta"))

;; The source content key an artifact for `name` can go stale against, or "" when
;; there is none to track: a namespace with no source file (only the artifact was
;; shipped), and install-owned source, which is part of the runtime and already
;; covered by the runtime fingerprint.
(define (cpath-source-key name)
  (let ((f (aot-cacheable-file name)))
    (if f (aot-cache-key (ldr-read-source f)) "")))

;; Does `name` still hash to what the artifact recorded for it? An empty key now
;; means there is no source to be stale against — the artifact-only deploy, where
;; RT.load takes the .class because there is no .clj beside it — so nothing to
;; check. A key that appeared where none was recorded (source shadowing an
;; install-owned namespace) does not match, and the artifact loses.
(define (cpath-key-current? recorded name)
  (let ((now (cpath-source-key name)))
    (or (string=? now "") (string=? now recorded))))

;; jolt version / runtime fingerprint / own source key, then one "name key" line
;; per direct require that has a key worth tracking.
(define (cpath-meta-lines name deps)
  (cons* (jolt-version-string)
         (or (aot-runtime-fingerprint) "")
         (cpath-source-key name)
         (map (lambda (d) (string-append d " " (cpath-source-key d)))
              (sort string<? deps))))

(define (cpath-write-meta! path lines)
  (let ((out (open-output-file path 'replace)))
    (for-each (lambda (l) (put-string out l) (put-string out "\n")) lines)
    (close-port out)))

;; "name key" -> ("name" . "key"); a dep with no key trails a bare space.
(define (cpath-split-dep-line s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((fx>=? i n) #f)
            ((char=? (string-ref s i) #\space)
             (cons (substring s 0 i) (substring s (fx+ i 1) n)))
            (else (loop (fx+ i 1)))))))

(define (cpath-meta-matches? name base)
  (let ((mf (cpath-meta-file base))
        (fp (aot-runtime-fingerprint)))
    (and fp                        ; no fingerprint = can't tell this runtime apart
         (file-exists? mf)
         (let ((lines (guard (e (else '())) (bld-string-lines-like (read-file-string mf)))))
           (and (fx>=? (length lines) 3)
                (string=? (list-ref lines 0) (jolt-version-string))
                (string=? (list-ref lines 1) fp)
                (cpath-key-current? (list-ref lines 2) name)
                (let loop ((ds (list-tail lines 3)))
                  (or (null? ds)
                      (let ((dep (cpath-split-dep-line (car ds))))
                        (and dep
                             (cpath-key-current? (cdr dep) (car dep))
                             (loop (cdr ds)))))))))))

(define (cpath-artifact-valid? name base)
  (and (file-exists? (cpath-so-file base))
       (cpath-meta-matches? name base)))

;; Set while a build driver walks an app's namespaces. `jolt build` emits each
;; namespace from its source, and it learns the load order (and the path to emit
;; from) through ns-loaded-hook — so a compiled artifact standing in for the source
;; would hand it a path with no source behind it. Compiled output is a load
;; shortcut; a build wants the real thing.
(define ldr-source-only? (make-thread-parameter #f))
;; ...except the AOT cache, when the build's pass 1 asks for it: a cached
;; namespace loads the same program its source would (the cache's contract for
;; `jolt run`), replays its def-ordinal stamps (aot-replay-def-ordinals!), and
;; the emit walk reads the source file the ns-loaded hook names either way. The
;; build otherwise recompiled every namespace from source on every build just
;; to load it — 16s of a 46s rebuild of a 231-namespace app (#1059).
(define ldr-build-aot-cache? (make-thread-parameter #f))

;; The base path of the first usable artifact for `name` on the source roots, or
;; #f. This is the load side: the JVM finds compiled output through the classpath,
;; never through *compile-path* itself, so a compile-path takes effect on load only
;; once it is also a root.
(define (cpath-find-artifact name)
  (and (not (ldr-source-only?))
       (let ((rel (ns-name->rel name)))
         (let loop ((roots source-roots))
           (and (pair? roots)
                ;; a jar root holds sources and resources, never a compiled .so
                (if (root-jar-index (car roots))
                    (loop (cdr roots))
                    (let ((base (string-append (car roots) "/" rel)))
                      (if (cpath-artifact-valid? name base) base (loop (cdr roots))))))))))

;; The first artifact on the roots whatever its state, for the error path: an
;; artifact-only deployment that a jolt upgrade invalidated otherwise reports only
;; that the SOURCE is missing, which says nothing about the .so sitting right there.
(define (cpath-any-artifact name)
  (let ((rel (ns-name->rel name)))
    (let loop ((roots source-roots))
      (and (pair? roots)
           (if (root-jar-index (car roots))
               (loop (cdr roots))
               (let ((base (string-append (car roots) "/" rel)))
                 (if (file-exists? (cpath-so-file base)) base (loop (cdr roots)))))))))

;; *compile-path* as a directory string, or #f when it is nil — the case
;; Compiler.compile reports as "*compile-path* not set".
(define (cpath-dir)
  (let ((v (guard (e (#t jolt-nil)) (var-deref "clojure.core" "*compile-path*"))))
    (and (string? v) v)))

;; *compile-files* is true for the extent of a compile, as core.clj's compile binds
;; it — both so library code can read it and because load-namespace* branches on it
;; (cpath-compiling-dir) to carry the compile through the load closure.
(define (cpath-with-compile-files thunk)
  (let ((cell (var-cell-lookup "clojure.core" "*compile-files*")))
    (if (not cell)
        (thunk)
        (dyn-with-frame (list (cons cell #t)) thunk))))

;; Publish under `dir`, borrowing the AOT cache's discipline: compile to a
;; pid-unique temp and rename(2) each file into place, .so last, so a concurrent
;; reader sees a complete artifact or none. The stale .so goes first for the same
;; reason — until the new one lands there must be no signal pointing at old .meta.
;; The .scm lands at its FINAL name before the compile: compile-file bakes the
;; name of the file it read into the frame source objects, so compiling the temp
;; would leave every frame loaded from this artifact pointing at a path deleted
;; by the rename (same bug as the AOT cache's, fixed alongside it).
(define (cpath-publish! name dir captured deps)
  (let* ((base (string-append dir "/" (ns-name->rel name)))
         (pid (number->string (get-process-id)))
         (tmp-scm (string-append base ".tmp" pid ".scm"))
         (tmp-so  (string-append base ".tmp" pid ".so")))
    (aot-mkdir-p (path-parent base))
    (guard (e (else (delete-file tmp-scm #f) (delete-file tmp-so #f) (raise e)))
      (let ((out (open-output-file tmp-scm 'replace)))
        (put-string out captured) (close-output-port out))
      (rename-replace! tmp-scm (cpath-scm-file base))
      ;; compile-file narrates to current-output-port by default — swallow it so a
      ;; compile can't corrupt the running program's stdout.
      (parameterize ((current-output-port (open-output-string)))
        (sa-compile-file (cpath-scm-file base) tmp-so #f))
      (delete-file (cpath-so-file base) #f)
      (cpath-write-meta! (cpath-meta-file base) (cpath-meta-lines name deps))
      (rename-replace! tmp-so (cpath-so-file base)))
    base))

;; The compiling load: evaluate the namespace while capturing its emitted Scheme,
;; then publish that under `dir`. Returns the artifact base, or #f when the file
;; emitted nothing to compile.
;;
;; load-namespace* takes this branch instead of the plain load whenever
;; *compile-files* is set, which is how the JVM's `(compile 'lib)` ends up emitting
;; classes for lib's whole load closure rather than lib alone (RT.load: when
;; COMPILE_FILES is true and the class wasn't already loaded, compile the .clj).
;; Without it a compiled namespace is undeployable — its artifact still re-runs its
;; requires, and those would find nothing.
(define (cpath-compile-load name file dir)
  (let* ((src (ldr-read-source file))
         (sink (aot-new-dep-sink))
         (captured (parameterize ((aot-dep-sink sink)) (aot-capture-load file src))))
    (and (string? captured) (fx>? (string-length captured) 0)
         (cpath-publish! name dir captured (filter aot-cacheable-file (vector-ref sink 0))))))

;; Is a compile in progress and pointed somewhere? Install-owned source is exempt:
;; it is part of the runtime (already covered by the runtime fingerprint), and
;; emitting a copy of the stdlib into the user's output directory would shadow the
;; runtime's own on every later load.
(define (cpath-compiling-dir file)
  (and (jolt-truthy? (guard (e (#t #f)) (var-deref "clojure.core" "*compile-files*")))
       (not (ldr-install-file? file))
       (cpath-dir)))

;; clojure.core/compile — core.clj's (binding [*compile-files* true] (load-one lib
;; true true)) wrapped in Compiler.compile's *compile-path* check. Like load-one it
;; loads the namespace into the running image (ignoring the already-loaded dedup,
;; so a compile always recompiles), verifies the file actually produced the
;; namespace, marks it loaded, and returns the lib.
(define (jolt-compile lib)
  (let* ((name (cond ((symbol-t? lib) (symbol-t-name lib))
                     ((string? lib) lib)
                     (else (throw-jvm 'java.lang.IllegalArgumentException
                                      (string-append "compile expects a namespace symbol, got "
                                                     (jolt-pr-str lib))))))
         (dir (or (cpath-dir)
                  (throw-jvm 'java.lang.RuntimeException "*compile-path* not set")))
         (file (or (find-ns-file name)
                   (throw-jvm 'java.io.FileNotFoundException
                              (string-append "Could not locate " (ns-name->rel name)
                                             ".jolt (or .clj/.cljc) on the source roots"))))
         (saved (chez-current-ns))
         (base (guard (e (else (set-chez-ns! saved) (raise e)))
                 (cpath-with-compile-files
                   (lambda () (cpath-compile-load name file dir))))))
    (set-chez-ns! saved)
    (unless (or (ns-has-vars? name) (hashtable-ref ns-registry name #f))
      (throw-jvm 'java.lang.Exception
                 (string-append "namespace '" name "' not found after loading '" file "'")))
    (unless base
      (throw-jvm 'java.lang.RuntimeException
                 (string-append "compile produced no code for " name)))
    (ldr-mark-loaded! name)
    (ns-loaded-hook name file)
    lib))
(def-var! "clojure.core" "compile" jolt-compile)

;; :reload-all forces the dedup off for the whole dynamic extent of a load (the
;; loader learns dependencies only as it loads them), so every namespace pulled in
;; reloads too — mirroring clojure.core. :verbose prints each load to stderr.
(define ldr-reload-all? (make-thread-parameter #f))
(define ldr-verbose? (make-thread-parameter #f))

;; --- the load protocol (concurrent require) ---------------------------------
;; Two threads requiring one namespace at once both used to pass the loaded-ns
;; check and both run its top-level forms, double-running every def and side
;; effect in the file. The mark-before-load below terminates a require CYCLE, but
;; only re-entry on ONE thread — to another thread the mark just says "loaded"
;; while the namespace is still half-built, which is worse than no dedup at all.
;;
;; This is JLS 12.4.2, the JVM's class-initialization procedure, over namespaces
;; instead of classes. It is the design that fits: initialization here is
;; dynamic, re-entrant, and allowed to be cyclic, which is exactly the shape the
;; JLS procedure exists for. Its step 3 IS jolt's existing mark-before-load
;; semantics, so nothing that works today changes; step 2 is the part that was
;; missing. The steps, and where each one lives:
;;
;;   1. acquire the lock                          -> ldr-load-mu
;;   2. in progress by ANOTHER thread: release,   -> condition-wait, then re-loop
;;      block until notified, repeat
;;   3. in progress by THIS thread: release and   -> 'recursive (a require cycle;
;;      complete normally (recursive request)        the caller sees a partially
;;                                                   loaded ns, as on the JVM)
;;   4. already initialized: release, return      -> 'loaded
;;   6. record in-progress-by-this-thread,        -> 'claimed, and the load then
;;      release, and initialize WITHOUT the lock     runs unlocked
;;  11. on success acquire, mark done, NOTIFY ALL -> ldr-end-load!
;;  12. on failure jolt rolls the mark back so a  -> the guard in load-namespace*
;;      retry can load, where the JVM marks the      (Clojure's behavior, and the
;;      class permanently erroneous                  file's existing behavior)
;;
;; Per namespace and not one global lock, so unrelated namespaces load in parallel
;; and a load that blocks only blocks threads wanting THAT namespace. Clojure went
;; the other way — serialized-require is (locking RT/REQUIRE_LOCK (apply require
;; args)) — but it is private and its own docstring calls it an "Interim function",
;; and it serializes every load in the process. Go avoids the question entirely
;; (package init is one goroutine, sequential, one package at a time, and import
;; cycles are a compile error); none of that is available here.
;;
;; Loading in parallel means COMPILING in parallel, which the compiler had to be
;; made safe for first: its emit-session scratch (the per-def cache cells and the
;; hoisted constant pool) lived on one process-global unit and was swapped in and
;; out around each def, so two threads emitting at once traded collectors and a
;; namespace that compiled cleanly alone died with "variable _kc$81 is not bound".
;; Those are thread-bound vars in backend_scheme now, the analyzer's position box is
;; per compilation, and its gensym counter reads and bumps in one swap!. See
;; test/chez/concurrent-require.clj, which loads distinct namespaces in parallel for
;; exactly this reason.
;;
;; The JVM's own hazard is that two threads entering a genuine require cycle from
;; opposite ends deadlock — spec-conformant, and OpenJDK closed JDK-8037567 and
;; two earlier reports as won't fix. We do not have to inherit the hang: the
;; owner of each in-flight load is already recorded, so ldr-wait-cycle walks the
;; wait-for graph and we raise instead. That changes nothing for a program that
;; works today; it only turns an undiagnosable hang into an error that names both
;; namespaces and both threads.
;;
;; Ownership is by EXECUTION CONTEXT — the fiber when a fiber is loading, the
;; thread id otherwise (ldr-load-ctx) — which is what makes step 3 a cycle break
;; rather than a self-deadlock, and it survives a park: a fiber is pinned to its
;; carrier for life (R0(d)), so a load that parks resumes as the same context and
;; load-namespace* deliberately keeps the claim across the park rather than
;; treating the escape as an exit.
;;
;; A load CAN park. (require 'x) from a go block on the :fiber backend, where x's
;; top level does a blocking channel op, is the shape, and it is the only way one
;; context ever observes another's claim on one carrier — same-carrier fibers do
;; not run concurrently. Everyone else wanting x then waits in step 2 until the
;; fiber comes back and finishes, which is the right answer and better than the
;; pre-protocol behaviour (they read the mark-before-load and returned with x
;; half-built).
;;
;; Thread id alone was not enough for that, because two fibers on one carrier share
;; one: a sibling fiber read the parked load's claim as its own, took step 3, and
;; returned with x half-loaded and no error. Nor is it enough to make a sibling
;; wait, because the wait itself has to be a PARK — a condition-wait would block the
;; carrier the parked load needs in order to finish. ldr-wait-for-load! picks the
;; mechanism from the waiter, and the cycle walk keys on the context, so two fibers
;; deadlocking over each other's loads is reported like any other cycle instead of
;; hanging.
;;
;; One behaviour improves as a side effect: :reload-all over a require cycle used
;; to recurse forever, because it turns the dedup off for the whole extent and
;; the mark was then no longer a stopping condition. Step 3 stops it.
;;
;; WHAT IS ACTUALLY PROVED, and what each proof rests on. Every transition below
;; happens under ldr-load-mu, which is what makes single-step reasoning sound: no
;; other thread can change ldr-loading or ldr-waiting between a check and the write
;; that follows it.
;;
;;   Mutual exclusion   pc = in-body implies you own the claim. Inductive over every
;;                      transition (claim, wait, recursive, end, escape, re-enter).
;;                      This is why ldr-assert-claim! refuses re-entry instead of
;;                      re-taking the claim: re-taking it is a single step that
;;                      breaks the invariant, putting two threads in one load body.
;;   No lost wakeup     a blocked waiter always has an owner left to wake it. Rests
;;                      on the wait committing before the lock drops — condition-wait
;;                      releases ldr-load-mu atomically with blocking, and a fiber
;;                      registers itself and sets its own state to 'parked while the
;;                      lock is still held (its SWITCH is outside the lock, which is
;;                      a separate matter and jolt-lock-wait's; what this property
;;                      needs is the commit, not the switch) — so no ldr-end-load!
;;                      fits between the owner check and the wait, and on
;;                      ldr-end-load! broadcasting
;;                      AND resuming from dynamic-wind's after thunk, which runs on
;;                      every exit. The LOADER's own park is not an exit and is
;;                      skipped there, which keeps the property rather than weakening
;;                      it: the owner is still recorded, still the same context, and
;;                      still on its way back to finish the load and wake everyone.
;;   Cycle detection    ldr-wait-cycle fires on exactly the waits that would
;;                      deadlock: it never misses a cycle and never raises on a
;;                      graph that would still have made progress. Rests on the
;;                      wait-for graph being FUNCTIONAL (one namespace per blocked
;;                      context, one owner per namespace) and on the pre-state being
;;                      acyclic, which holds because every earlier wait was checked.
;;   No deadlock        the lock graph is acyclic, and the edge that is hardest to
;;                      see is the one a park used to add: a fiber parked inside this
;;                      critical section left a blocking re-acquire of ldr-load-mu on
;;                      its resume, which runs in the scheduler and puts every fiber
;;                      on that carrier behind it. That edge is gone rather than
;;                      argued about — the switch happens with the lock released, and
;;                      locks.ss checks it at both switch points (jolt-04ee). The one
;;                      edge that is not the loader's to remove is stm-lock -> the load a parked
;;                      transaction is waiting for, because jolt-sync holds
;;                      stm-lock across a whole dosync body. So no load may acquire
;;                      stm-lock — see ldr-libs-update!, where that cycle was real
;;                      and hung.
(define ldr-load-mu (make-mutex))
(define ldr-load-cv (make-condition))
(define ldr-loading (make-hashtable string-hash string=?))  ; ns -> the context loading it
(define ldr-waiting (make-eqv-hashtable))                   ; context -> ns it waits on
(define ldr-fiber-waiters (make-hashtable string-hash string=?))  ; ns -> parked fibers

;; --- who a load belongs to ---------------------------------------------------
;; The FIBER when a fiber is loading, and the thread id otherwise. Thread id alone
;; was wrong in both directions once a load could park, because two fibers on one
;; carrier share one. Fiber A's load parks; fiber B on the same carrier requires the
;; same namespace, reads A's claim as its own, takes step 3 and returns with the
;; namespace half-loaded, silently. A fiber is the finer identity and it is the right
;; one: step 3 asks "am I already inside this load", and the answer is per execution,
;; not per OS thread. It is only ever compared and used as a table key, never
;; dereferenced, so nothing here depends on the fiber record's shape.
;;
;; Same-carrier fibers never run concurrently, so B can only ever observe A's claim
;; while A is PARKED — this is the parked-load case and nothing else.
(define (ldr-load-ctx) (or (jolt-current-fiber) (get-thread-id)))
(define (ldr-ctx-str ctx)
  (if (jolt-fiber? ctx) "a fiber" (string-append "thread " (number->string ctx))))

;; --- the load watchdog -------------------------------------------------------
;; How long a THREAD may wait for another context's load before the wait is
;; declared a deadlock and raised with the whole claim state. The require-cycle
;; walk above only sees load-edges; a load that parks on something that is NOT a
;; load — a channel take, a promise — while holding its claim closes a cycle the
;; walk cannot see, and the process used to wedge silently forever (the v0.7.10
;; first-cold-run wedge). 120s is 6x the slowest whole-chain cold compile
;; measured on CI runners; JOLT_LOAD_WAIT_LIMIT_SECS overrides, 0 disables.
;; Fiber waiters park instead of blocking and are not covered here; a parked
;; fiber's carrier keeps running, so a fiber-side wedge starves one fiber, not
;; the process.
(define ldr-wait-limit-ms
  (let ((s (getenv "JOLT_LOAD_WAIT_LIMIT_SECS")))
    (* 1000 (if s (or (string->number s) 120) 120))))
(define ldr-wait-slice (make-time 'time-duration 0 15))

;; under ldr-load-mu: every claim and every waiter, one line each
(define (ldr-claims-str)
  (let ((out ""))
    (vector-for-each
      (lambda (ns)
        (set! out (string-append out "  load " ns " held by "
                                 (ldr-ctx-str (hashtable-ref ldr-loading ns #f)) "
")))
      (hashtable-keys ldr-loading))
    (vector-for-each
      (lambda (ctx)
        (set! out (string-append out "  " (ldr-ctx-str ctx) " waits on "
                                 (hashtable-ref ldr-waiting ctx "?") "
")))
      (hashtable-keys ldr-waiting))
    (vector-for-each
      (lambda (ns)
        (set! out (string-append out "  " (number->string
                                            (length (hashtable-ref ldr-fiber-waiters ns '())))
                                 " fiber(s) parked on " ns "
")))
      (hashtable-keys ldr-fiber-waiters))
    (if (string=? out "") "  (no claims recorded)
" out)))

(define (ldr-watchdog-raise! me name)
  (let ((owner (hashtable-ref ldr-loading name #f)))
    (hashtable-delete! ldr-waiting me)
    (throw-jvm (quote IllegalStateException)
      (string-append
        "Load watchdog: " (ldr-ctx-str me) " waited more than "
        (number->string (quotient ldr-wait-limit-ms 1000)) "s for the load of "
        name " (held by " (if owner (ldr-ctx-str owner) "nobody — claim vanished")
        "). A load is stuck holding its claim while waiting on something that is"
        " not a load, which the require-cycle detector cannot see. Claim state:
"
        (ldr-claims-str)))))

;; Wait for the load of `name` to end. Runs inside jolt-lock-wait's decision, so
;; ldr-load-mu is HELD. Answers #f when the wait is over and the caller should
;; re-check under the lock, or jolt-lock-parked when this fiber has committed to a
;; park that jolt-lock-wait will perform once it has released the lock. A wakeup is
;; "something changed", never "the namespace is yours", either way.
;;
;; A fiber must not use the condition variable. condition-wait blocks the CARRIER
;; thread, and the load it is waiting for is very likely parked on that same carrier
;; (see above), so blocking there is a deadlock, and even when the owner is on
;; another thread it stalls every unrelated fiber the carrier is running. It parks
;; instead, and ldr-end-load! resumes it.
;;
;; NO WAKEUP IS LOST, and the reason is the registration and the 'parked commit both
;; happening under ldr-load-mu, which ldr-end-load! must take: a release either lands
;; before them and is seen by the re-check, or after them and finds a fiber to resume.
;; The SWITCH is the one part that happens outside the lock (jolt-04ee). It used to
;; happen right here, inside the critical section, leaning on jolt-with-mutex being a
;; dynamic-wind to release the lock on the way out and re-acquire it on the resume.
;; That is not safe, for the reason host/chez/locks.ss now states as a rule: the
;; re-acquire runs from Chez's rewind, on the carrier thread, at the interrupt depth
;; the fiber parked at, and the carrier can do nothing else until it succeeds — so the
;; park attaches a blocking acquire to a point in the scheduler, and every fiber on
;; that carrier is behind it. Every fiber and thread requiring any namespace passes
;; through this same region, so the precondition that would have licensed it does not
;; hold here any more than it held for the object monitor (jolt-3a87, jolt-dfuo).
;;
;; Committing here and switching there is also what keeps load-namespace*'s after
;; thunk honest: the escape is still a park and still not an exit, so that one still
;; asks jolt-park-unwinding? and keeps the claim across it.
(define (ldr-wait-for-load! name deadline)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin
          (hashtable-set! ldr-fiber-waiters name
                          (cons f (hashtable-ref ldr-fiber-waiters name '())))
          (jolt-fiber-state-set! f 'parked)
          jolt-lock-parked)
        (begin
          ;; sliced so the deadline is checked even when no wakeup ever comes;
          ;; each slice returns #f and the caller re-checks the owner first.
          (when (and deadline (>= (sa-real-time-ms) deadline))
            (ldr-watchdog-raise! (ldr-load-ctx) name))
          (jolt-condition-wait ldr-load-cv ldr-load-mu ldr-wait-slice)
          #f))))

;; Call with ldr-load-mu HELD. Would `me` waiting on `name` (owned by `owner`) close
;; a cycle? Follow owner -> what it waits on -> who owns that -> …; if the chain
;; reaches `me`, everyone in it is waiting on everyone else. Returns the chain as a
;; list of (context . ns-it-waits-on) in wait order, or #f if there is no cycle — the
;; error names every context in it, because a cycle of three reported as two sends
;; you looking for a two-namespace cycle that isn't there. Bounded so a table
;; mutated under us can never spin.
;;
;; Contexts and not thread ids, which is what extends this to fibers: two fibers on
;; one carrier that park mid-load waiting on each other are a genuine cycle, and
;; keyed by thread id they collided in ldr-waiting and the walk could not see it.
(define (ldr-wait-cycle owner me)
  (let loop ((t owner) (n 0) (acc '()))
    (and (fx< n 256)
         (let ((w (hashtable-ref ldr-waiting t #f)))
           (and w
                (let ((o (hashtable-ref ldr-loading w #f))
                      (acc (cons (cons t w) acc)))
                  (and o (if (eqv? o me) (reverse acc) (loop o (fx+ n 1) acc)))))))))

;; "thread 3 waits on a/b (held by thread 5), thread 5 waits on …" — the wait-for
;; chain the caller is about to close, rendered in the order it was walked. Fibers
;; render as "a fiber" rather than being numbered; the namespaces are what names the
;; cycle, and a fiber has no id to print.
(define (ldr-wait-chain-str chain)
  (let loop ((c chain) (out ""))
    (if (null? c)
        out
        (loop (cdr c)
              (string-append out (if (string=? out "") "" ", ")
                             (ldr-ctx-str (caar c))
                             " waits on " (cdar c))))))

;; Every way out of the decision below leaves the wait-for graph as it found it.
;; Deleting on the way out rather than immediately after each wait is what lets the
;; entry STAND while this context is parked, which is the point of recording it: a
;; cycle is only visible to another context's walk if the waits in it are all on the
;; table at once. A delete with nothing to delete is a no-op, so the first-iteration
;; exits (steps 3 and 4) cost one lookup and say what they mean.
(define (ldr-decided! me v) (hashtable-delete! ldr-waiting me) v)

;; -> 'loaded | 'recursive | 'claimed. Steps 1-6.
;;
;; The whole decision is jolt-lock-wait's `decide`: it runs under ldr-load-mu, and a
;; fiber that has to wait answers jolt-lock-parked, is switched out with the lock
;; RELEASED, and then runs this again from the top. Retaking it rather than resuming
;; into the middle of it is the same thing step 2 always did on a thread's wakeup —
;; the loop below — and it is now the same code path for both contenders.
(define (ldr-begin-load! name force?)
  (let ((me (ldr-load-ctx))
        (deadline (and (> ldr-wait-limit-ms 0) (+ (sa-real-time-ms) ldr-wait-limit-ms))))
    (jolt-lock-wait ldr-load-mu
      (lambda ()
        (let loop ()
          (let ((owner (hashtable-ref ldr-loading name #f)))
            (cond
              ((and owner (eqv? owner me))                    ; step 3
               (ldr-decided! me 'recursive))
              (owner                                          ; step 2
               (let ((chain (ldr-wait-cycle owner me)))
                 (when chain
                   (throw-jvm (quote IllegalStateException)
                     (string-append
                       "Deadlocked require: " (ldr-ctx-str me)
                       " is about to wait on " name " (held by "
                       (ldr-ctx-str owner) "), and " (ldr-wait-chain-str chain)
                       ", which closes back on " (ldr-ctx-str me) ". "
                       (number->string (fx+ 1 (length chain)))
                       " loads entered a require cycle from different ends; break"
                       " the cycle or require these namespaces from one thread."))))
               (hashtable-set! ldr-waiting me name)
               ;; a thread's wait ends under this lock, so it loops HERE; a fiber
               ;; answers jolt-lock-parked and comes back in at the top.
               (or (ldr-wait-for-load! name deadline) (loop)))
              ((and (not force?) (not (ldr-reload-all?)) (ns-dedup-loaded? name))
               (ldr-decided! me 'loaded))                     ; step 4
              (else (hashtable-set! ldr-loading name me)      ; step 6
                    (ldr-decided! me 'claimed)))))))))

;; Step 11: drop the claim and wake everyone waiting on this namespace. Broadcast
;; and not signal — the waiters re-check a condition that may not be true for all
;; of them (a failed load leaves the namespace unloaded, and exactly one of them
;; should go on to claim it). Parked fibers are woken the same way and for the same
;; reason: resume them all and let them re-check.
;;
;; Both under the lock, so a waiter that has registered but not yet parked cannot be
;; missed — it registered with the lock held and stays committed to the park until
;; the escape drops it. The list is cleared as it is drained, so a fiber that goes
;; on to wait again registers itself afresh and no resume is ever delivered twice.
;; sa-fiber-resume takes only its carrier's run-queue mutex, which is the last lock
;; in the order, so calling it from here closes no cycle.
(define (ldr-end-load! name)
  (jolt-with-mutex ldr-load-mu
    (hashtable-delete! ldr-loading name)
    (let ((fs (hashtable-ref ldr-fiber-waiters name '())))
      (unless (null? fs)
        (hashtable-delete! ldr-fiber-waiters name)
        (for-each sa-fiber-resume fs)))
    (condition-broadcast ldr-load-cv)))

;; The dynamic-wind before thunk below: the claim must be ours. On first entry it is,
;; because ldr-begin-load! has just set it, so this is a check and nothing more.
;;
;; It re-runs if a continuation captured inside a load escapes and is then re-entered.
;; A PARK is that, and it is not an exit — the after thunk below keeps the claim
;; across one, so the resumed load still owns it and this passes.
;;
;; Any OTHER re-entry is refused. Re-taking the claim unconditionally looks like the
;; natural mirror of dropping it on the way out, and it is wrong: the load has ended
;; by then and another thread can own the namespace, so the re-entering thread would
;; take the claim out from under a thread that is running the body, and both would be
;; in it at once. There is no version of that re-entry that keeps "in the body implies
;; you hold the claim" true, so it is refused rather than papered over.
(define (ldr-assert-claim! name)
  (let ((me (ldr-load-ctx)))
    (jolt-with-mutex ldr-load-mu
      (let ((owner (hashtable-ref ldr-loading name #f)))
        (unless (eqv? owner me)
          (throw-jvm (quote IllegalStateException)
            (string-append
              "Re-entered the load of " name " on " (ldr-ctx-str me)
              ", which no longer holds its claim ("
              (if owner (string-append (ldr-ctx-str owner) " does")
                  "the load has already ended")
              "). A continuation captured inside a load cannot be resumed; the"
              " namespace would be loaded twice at once.")))))))

;; load-namespace: load `name`'s source once. Marked loaded BEFORE eval so a
;; dependency cycle terminates (Clojure's behavior). force? (a :reload of the
;; named lib) bypasses the dedup for THIS load only; ldr-reload-all? bypasses it
;; for this load and every nested one in its extent. On a throw the mark is rolled
;; back IF this call made it (a retry then loads) and the caller's ns is restored —
;; matching Clojure, which marks a lib loaded only after success.
(define (load-namespace name) (load-namespace* name #f))
(define (load-namespace* name force?)
  ;; steps 1-6 above: 'loaded and 'recursive are both "complete normally, do
  ;; nothing"; only 'claimed means this thread owns the load and must run it.
  (when (eq? 'claimed (ldr-begin-load! name force?))
    ;; read AFTER claiming, not before: while this thread was waiting in step 2
    ;; another may have loaded the namespace and then had its own load fail, and
    ;; a stale #t here would make the guard below skip the rollback.
    (let ((was-loaded? (ns-dedup-loaded? name))
          (finished? #f))
      ;; step 11/12: drop the claim and wake the waiters on EVERY exit. dynamic-wind
      ;; and not a guard: a guard only sees a raise, and any other way out of the
      ;; body — a continuation escaping the load — would leave the claim standing,
      ;; which is a namespace no other thread can ever load and no error to say why.
      ;; The inner guard in ldr-load-body still rolls the mark back and re-raises, so
      ;; the throw path is unchanged.
      ;;
      ;; A PARK is the one escape that is not an exit, and it has to be told apart
      ;; from the others. A fiber whose load parks — a top-level (<!! ch) in a
      ;; namespace required from a go block on the :fiber backend — comes back and
      ;; carries the load on from the park point, on the SAME thread, because R0(d)
      ;; pins a fiber to its carrier for life. Treating that as an exit dropped the
      ;; claim, woke the waiters onto a half-built namespace, and then made the
      ;; resume die in ldr-assert-claim! with the mark still standing and the
      ;; rollback stranded in the abandoned guard — so the namespace was wedged
      ;; loaded-but-empty for the life of the process and every later require of it
      ;; no-op'd. jolt-park-unwinding? answers that question: cleanup belonging to
      ;; the real exit does not run on a park.
      ;;
      ;; jolt's own try/finally used to consult the same flag and no longer does.
      ;; The back end now emits a shared marker (values.ss jolt-finally-in) as
      ;; every finally's before-thunk and the scheduler drops those winders off
      ;; the chain before it escapes, so a finally never runs on a park in the
      ;; first place. That trick is not available here, because THIS wind needs a
      ;; before-thunk of its own: ldr-assert-claim! has to re-run when the fiber
      ;; resumes. A winder that must rewind cannot be dropped, so it stays on the
      ;; chain and asks instead. It is the only such site left.
      ;;
      ;; And when it IS the real exit, a body that neither finished nor raised
      ;; (a continuation escaping the load for good) never reached ldr-load-body's
      ;; guard, so the mark is rolled back here instead. Idempotent against the
      ;; guard's own rollback on the throw path.
      ;;
      ;; RFC 0014: an install namespace's registrations belong to the provider
      ;; that DECLARES it, whichever way the load was reached — the class-miss
      ;; autoload, or a plain require from another provider's install namespace
      ;; (host-static.ss lib-with-install-ns-mark, jolt#926).
      (dynamic-wind
        (lambda () (ldr-assert-claim! name))
        (lambda ()
          (lib-with-install-ns-mark name (lambda () (ldr-load-body name force? was-loaded?)))
          (set! finished? #t))
        (lambda ()
          (unless (jolt-park-unwinding?)
            (unless (or finished? was-loaded?) (ldr-unmark-loaded! name))
            (ldr-end-load! name)))))))

;; The load itself, run by the one thread that claimed it (never concurrently for
;; a given name, and never re-entered for it on this thread).
(define (ldr-load-body name force? was-loaded?)
  ;; A compiled artifact on the roots wins over the source, like RT.load
  ;; preferring a .class to its .clj. :reload does NOT bypass it, also like the
  ;; JVM: the artifact is only offered here when it still matches the source it
  ;; was built from, so there is nothing stale for a reload to get past.
  (let* ((file (find-ns-file name))
         (art (cpath-find-artifact name)))
    (cond
      ((or art file)
       (when (ldr-verbose?)
         (display (string-append "Loading " name " from " (or art file) "\n")
                  (current-error-port)))
       (ldr-mark-loaded! name)            ; mark before load so a cycle terminates
       (let ((saved (chez-current-ns)))
         ;; the ns restore is on the way out, not written twice on the two paths
         ;; that used to reach it — a load leaving *ns* pointing at the file it was
         ;; part way through is the kind of damage that surfaces three forms later
         ;; somewhere else, so it happens however the body exits.
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (guard (e (else
                         (unless was-loaded? (ldr-unmark-loaded! name)) ; roll the mark back
                         (raise e)))
               (cond
                 (art (ldr-with-compiled-ns-vars (lambda () (load (cpath-so-file art)))))
                 ;; inside a compile, loading from source also emits the artifact
                 ;; — RT.load's COMPILE_FILES branch, which is what carries a
                 ;; compile through to the whole load closure.
                 ((cpath-compiling-dir file)
                  => (lambda (dir) (cpath-compile-load name file dir)))
                 (else (aot-load-or-compile name file force?)))))
           (lambda () (set-chez-ns! saved)))   ; the current ns is thread-local
         ;; the hook feeds `jolt build`, which needs the SOURCE path; an
         ;; artifact-only namespace has none to give.
         (ns-loaded-hook name (or file art))))
      ;; No source file but the namespace exists in memory (AOT'd into a built
      ;; binary): it's already defined — mark loaded and move on.
      ((ns-has-vars? name)
       (ldr-mark-loaded! name))
      ;; Same-file namespace (inlined ns form in a Jolt file): registered via
      ;; intern-ns! in the runtime registry even if no vars bear its ns name yet.
      ((hashtable-ref ns-registry name #f)
       (ldr-mark-loaded! name))
      (else
        (let ((art (cpath-any-artifact name)))
          (throw-jvm (quote java.io.FileNotFoundException)
            (string-append "Could not locate " (ns-name->rel name)
                           ".jolt (or .clj/.cljc) on the source roots"
                           (cond
                             ((not art) "")
                             ;; a build asked for source and there is only an
                             ;; artifact — say that, rather than blame the artifact
                             ((ldr-source-only?)
                              (string-append "; only the compiled " (cpath-so-file art)
                                             " is there, and a build emits from source"))
                             (else
                               (string-append "; " (cpath-so-file art)
                                              " was compiled by a different jolt build, or"
                                              " a namespace it requires has changed — recompile it"))))))))))

;; load-file / load run a file's forms starting in the current ns, and put
;; *ns* back afterwards however the load exits: Compiler.load binds *ns* for the
;; duration, so an `ns` form in the loaded file never leaks into the caller.
(define (load-jolt-file/restoring-ns path)
  (let ((saved (chez-current-ns)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (load-jolt-file path))
      (lambda () (set-chez-ns! saved)))))

;; load-file: load an explicit path, in the current ns.
(define (jolt-load-file path)
  (load-jolt-file/restoring-ns path)
  jolt-nil)

;; expand-spec: the shared prefix-list expansion (expand-libspec, ns.ss) —
;; kept under its old name for the build driver's callers.
(define (expand-spec s) (expand-libspec s))

;; --- the load step of require/use --------------------------------------------
;; ns.ss owns `require`/`use`; what it cannot own is reading a namespace off the
;; source roots, because it is loaded by runtimes that have no loader. So the
;; body is there and the two steps only a loader can take are installed here.
;;
;; ns-load-target! reads and initializes one already-expanded target. The dep is
;; recorded BEFORE the load: a target already loaded is still a dependency of
;; whoever is being compiled, and load-namespace* would dedup it away.
(set-ns-load-target!
  (lambda (target force-named?)
    (aot-record-dep! target)
    (load-namespace* target force-named?)))

;; ns-with-load-opts interprets the keyword flags clojure.core's load-libs
;; collects, once for the whole call: :reload forces the NAMED libs past the
;; dedup, :reload-all forces it off for the whole load (so transitively-required
;; libs reload too), :verbose prints each load.
(set-ns-with-load-opts!
  (lambda (flags k)
    (let ((reload-all? (and (member "reload-all" flags) #t))
          (verbose? (and (member "verbose" flags) #t)))
      (if reload-all?
          (parameterize ((ldr-reload-all? #t) (ldr-verbose? verbose?)) (k #f))
          (parameterize ((ldr-verbose? verbose?))
            (k (and (member "reload" flags) #t)))))))

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
;; clojure/tools/common_tests.clj. Strip the leading slash / try each of
;; ldr-source-exts.
(define (jolt-load . paths)
  (for-each
    (lambda (p)
      (let* ((rel (cond
                    ((and (> (string-length p) 0) (char=? (string-ref p 0) #\/))
                     (substring p 1 (string-length p)))
                    (else (let ((dir (ns-rel-dir (chez-current-ns))))
                            (if (string=? dir "") p (string-append dir "/" p))))))
             (f (resolve-on-roots rel)))
        (if f (load-jolt-file/restoring-ns f)
            (throw-jvm (quote java.io.FileNotFoundException) (string-append "Could not locate resource on source roots: " p)))))
    paths)
  jolt-nil)
(def-var! "clojure.core" "load" jolt-load)

;; --- shell primitive (jolt.host/sh, sh-out) ---------------------------------
;; `sh` runs `sh -c CMD`, inheriting stdout/stderr (so git progress shows), and
;; returns the exit code. `sh-out` captures stdout to a string (exit ignored) for
;; commands whose output we parse (git rev-parse). Used by jolt.deps for git.
;;
;; Both spawn with an EMPTY signal mask, as ProcessBuilder does (process.ss): the
;; CLI blocks SIGTERM/SIGHUP/SIGINT on the primordial thread at entry for the
;; shutdown watcher (java/concurrency.ss), a blocked mask survives fork and exec,
;; and bash — /bin/sh on macOS and several Linuxes — does not clear it. The
;; child and everything it starts would then ignore ^C and a plain `kill`.
(define (jolt-sh cmd) (jolt-with-empty-sigmask (lambda () (system cmd))))
(def-var! "jolt.host" "sh" jolt-sh)

(define (jolt-sh-out cmd)
  (call-with-values
    (lambda () (jolt-with-empty-sigmask
                 (lambda () (sa-run-process (string-append "exec sh -c " (sh-quote cmd))
                                            (native-transcoder)))))
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
;; The file a namespace would load from, or nil: the same search a require
;; does, without loading. jolt.main asks before requiring an entry namespace
;; so it can say "no project here" instead of "could not locate" -- a catch
;; around the require would re-raise a propagating load error from the wrong
;; place and lose its location.
(def-var! "jolt.host" "ns-source"
  (lambda (nm)
    (let ((f (find-ns-file (if (string? nm) nm (jolt-str-render-one nm)))))
      (if f f jolt-nil))))
(def-var! "jolt.host" "load-namespace" (lambda (n) (load-namespace n) jolt-nil))
;; The Clojure-facing seam for :jolt/replaces (see ldr-ns-replacements above).
;; jolt.deps collects the key and jolt.main calls this once per namespace after
;; it resolves the project, before any of the project compiles.
(def-var! "jolt.host" "replace-builtin-ns!"
  (lambda (n) (replace-builtin-ns! (jolt-str-render-one n)) jolt-nil))
(def-var! "jolt.host" "file-exists?" (lambda (p) (if (file-exists? p) #t #f)))
;; …and whether it is a DIRECTORY, which file-exists? also answers #t for. A bare
;; argv token is dispatched as a file to run before a :tasks lookup (main.clj's
;; run-file-arg?), so `jolt test` in any project with a test/ dir — which is every
;; jolt library — took the file path and died decoding a directory.
(def-var! "jolt.host" "directory?" (lambda (p) (if (file-directory? p) #t #f)))
;; jolt.host/getenv is defined in rt.ss, not here — the compiler image reads it
;; as it loads, which is before this file (see the comment there).

;; --- filesystem primitives (jolt.host) --------------------------------------
;; jolt.deps did its filesystem work by shelling out: `mkdir -p`, `mv`, `rm -f`,
;; `rm -rf`, `test -nt`, `find`. jolt-sh is Chez's `system`, which on Windows
;; runs the string through cmd.exe, where none of those mean what they mean on
;; POSIX — cmd's `mkdir` has no `-p` and takes a list of paths, so `mkdir -p
;; a/b` silently creates a directory literally NAMED `-p` alongside a/b, and
;; mv/rm/test/find are not commands at all. That left every Windows run
;; littering the project with a `-p` dir and `.part-` files the failed `mv`
;; never published, and the classpath cache never hit. Same failure the AOT
;; cache hit (aot-mkdir-p above); these are the native equivalents, so the
;; resolver never spawns a shell for something the filesystem API does. Only
;; git stays a subprocess — it is a real external program. Jars are read in
;; place (the jar seams below).
;;
;; A Windows path can arrive with backslashes (a %TEMP%- or %HOME%-derived one
;; does), and the separator-splitting walks below know only "/" — which Windows
;; accepts everywhere anyway. POSIX keeps the path verbatim: there a backslash
;; is an ordinary character in a filename, not a separator.
(define (host-fs-path p)
  (if (eq? (sa-os-family) 'windows)
      (list->string (map (lambda (c) (if (char=? c #\\) #\/ c)) (string->list p)))
      p))
(def-var! "jolt.host" "mkdirs!"
  (lambda (p) (if (mkdirs! (host-fs-path p)) #t #f)))
;; `rm -f`: an absent path is success, not failure.
(def-var! "jolt.host" "delete-file!"
  (lambda (p)
    (let ((p (host-fs-path p)))
      (if (file-exists? p) (if (delete-path! p) #t #f) #t))))
;; `rm -rf`, reusing the AOT cache's pruner (which does not follow symlinks).
(def-var! "jolt.host" "delete-tree!"
  (lambda (p)
    (let ((p (host-fs-path p)))
      (aot-delete-tree p)
      (if (file-exists? p) #f #t))))
;; `mv` within one filesystem: rename(2), which is the atomicity the publish
;; steps (a staged cache entry, a staged git checkout) depend on — INCLUDING
;; rename(2)'s replacement of an existing destination, which Windows refuses
;; (java/io.ss rename-replace!, jolt-lang/jolt#1074).
(def-var! "jolt.host" "rename-file!"
  (lambda (from to)
    (guard (e (#t #f)) (rename-replace! (host-fs-path from) (host-fs-path to)) #t)))
;; last-modified in epoch milliseconds, 0 when absent — what `test -nt` compared.
(def-var! "jolt.host" "file-mtime"
  (lambda (p)
    (let ((p (host-fs-path p)))
      (if (file-exists? p) (sa-file-mtime-ms p) 0))))
;; directory entries (names only), nil when p is not a directory — the pieces a
;; `find` walk is built from on the Clojure side.
(def-var! "jolt.host" "list-dir"
  (lambda (p)
    (let ((p (host-fs-path p)))
      (if (file-directory? p)
          (guard (e (#t jolt-nil)) (list->cseq (directory-list p)))
          jolt-nil))))
;; …and whether it is a symlink, so that walk can decline to follow one, as
;; `find` does by default.
(def-var! "jolt.host" "symlink?"
  (lambda (p) (if (file-symbolic-link? (host-fs-path p)) #t #f)))

;; --- jars on the roots (jolt.host) -------------------------------------------
;; What jolt.deps and jolt.loader ask about a jar: whether a file is a whole
;; archive, the names it holds, the text of one entry, and where a name resolves
;; on a root. Each reads the central directory (java/zip-file.ss); none extracts.

;; Is the file at PATH a whole zip archive — one whose END record and every
;; central-directory header read? The resolver asks this of every jar it puts
;; on the roots, so a cut download or a file that is not a jar fails resolution
;; loudly, before anything is cached, rather than failing the first require.
(def-var! "jolt.host" "zip-archive?"
  (lambda (path) (if (zipdir-for (host-fs-path path)) #t #f)))

;; The entry names of the archive at PATH, in directory order; nil when the file
;; is not a whole archive.
(def-var! "jolt.host" "zip-entry-names"
  (lambda (path)
    (let ((d (zipdir-for (host-fs-path path))))
      (if d (list->cseq (zipdir-names d)) jolt-nil))))

;; The text of the entry a jar path ("jar:file:<jar>!/<entry>") names, or nil
;; when there is no such entry.
(def-var! "jolt.host" "jar-entry-string"
  (lambda (p)
    (let ((bv (jar-path-bytes p)))
      (if bv (utf8->string bv) jolt-nil))))

;; The location NAME resolves to on ROOT — a file under a directory root, a
;; jar path into a jar root, an embedded key under an "embed:<prefix>" root —
;; or nil. jolt.loader's roots backend locates through this so a context's
;; roots may hold jars (and embedded roots) as the global roots may.
(def-var! "jolt.host" "root-file"
  (lambda (root name)
    (or (ldr-root-file (host-fs-path root) name) jolt-nil)))

;; True when NAME is a key in the runtime's embedded-resource table — what
;; deps.edn :jolt/build {:embed [dirs]} bakes into a binary, and what a
;; jolt.loader embedded root ("embed:<prefix>", ldr-embedded-root? above)
;; resolves against. The Clojure side reads such a source through the host
;; resolver instead of opening the key as a filesystem path.
(def-var! "jolt.host" "embedded-resource?"
  (lambda (name) (if (embedded-resource-has? name) #t #f)))

;; jolt version string — one source (jolt-version-string, rt.ss): the baked
;; release tag in a binary, $JOLT_VERSION under bin/jolt, else "dev".
(def-var! "jolt.host" "jolt-version" (lambda () (jolt-version-string)))
