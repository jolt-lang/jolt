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
(define (set-source-roots! roots) (set! source-roots roots))
(define (get-source-roots) source-roots)

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
       (cond (rdr (jolt-list rdr (jolt-list (jolt-symbol #f "quote") inner)))
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
                       (guard (e (#t #f)) (load-namespace (symbol-t-ns v))))
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
(define (ns-seg-munge seg)
  (list->string (map (lambda (c) (if (char=? c #\-) #\_ c)) (string->list seg))))
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
(define (resolve-on-roots rel)
  (let loop ((roots source-roots))
    (if (null? roots) #f
        (let ((clj  (string-append (car roots) "/" rel ".clj"))
              (cljc (string-append (car roots) "/" rel ".cljc")))
          (cond ((file-exists? clj) clj)
                ((file-exists? cljc) cljc)
                (else (loop (cdr roots))))))))

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
(define (load-jolt-file path)
  (let* ((src (read-file-string path)) (end (string-length src)))
    ;; parameterize (not a bare set!) so a require nested in this file's ns form
    ;; restores path when control returns to the rest of this file.
    (parameterize ((rdr-source-file path))   ; list forms read here carry :file = path
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
              (loop j))))))))

;; load-namespace: load `name`'s source once. Marked loaded BEFORE eval so a
;; dependency cycle terminates (Clojure's behavior). The caller's current ns is
;; restored afterward, since loading the file switched it.
(define (load-namespace name)
  (unless (hashtable-ref loaded-ns name #f)
    (let ((file (find-ns-file name)))
      (cond
        (file
         (hashtable-set! loaded-ns name #t)   ; mark before load so a cycle terminates
         (let ((saved (chez-current-ns)))
           (load-jolt-file file)
           ;; restore the current ns (thread-local); *ns* reads derive from it.
           (set-chez-ns! saved))
         (ns-loaded-hook name file))
        ;; No source file but the namespace exists in memory (AOT'd into a built
        ;; binary): it's already defined — mark loaded and move on.
        ((ns-has-vars? name)
         (hashtable-set! loaded-ns name #t))
        (else
         (error #f (string-append "Could not locate " (ns-name->rel name)
                                  ".clj (or .cljc) on the source roots") name))))))

;; load-file: load an explicit path (a `run FILE`), in the current ns.
(define (jolt-load-file path)
  (load-jolt-file path)
  jolt-nil)

;; The target ns name of a require/use spec ([ns …] / (ns …) / bare ns).
(define (spec-target-name spec)
  (let ((items (cond ((pvec? spec) (seq->list spec))
                     ((or (cseq? spec) (empty-list-t? spec)) (seq->list spec))
                     ((symbol-t? spec) (list spec))
                     (else '()))))
    (and (pair? items) (symbol-t? (car items)) (symbol-t-name (car items)))))

;; --- require/use that LOAD ---------------------------------------------------
;; Override the alias-only versions from natives-str.ss. Load each spec's target
;; (no-op if baked/already loaded), THEN register its :as/:refer under the caller
;; ns (chez-register-spec! reads the current ns, restored by load-namespace).
(define (loader-require . specs)
  (for-each
    (lambda (s)
      (let ((target (spec-target-name s)))
        (when target (load-namespace target)))
      (chez-register-spec! (chez-current-ns) s))
    specs)
  jolt-nil)
(def-var! "clojure.core" "require" loader-require)

(define (loader-use . specs)
  (for-each
    (lambda (spec)
      (let ((target (spec-target-name spec)))
        (when target (load-namespace target)))
      (chez-register-spec! (chez-current-ns) spec)
      (let* ((items (cond ((pvec? spec) (seq->list spec))
                          ((or (cseq? spec) (empty-list-t? spec)) (seq->list spec))
                          ((symbol-t? spec) (list spec))
                          (else '())))
             (target (and (pair? items) (symbol-t? (car items)) (symbol-t-name (car items))))
             (filtered (let scan ((xs (if (pair? items) (cdr items) '())))
                         (cond ((null? xs) #f)
                               ((and (keyword? (car xs))
                                     (member (keyword-t-name (car xs)) '("only" "refer"))) #t)
                               (else (scan (cdr xs)))))))
        (when (and target (not filtered))
          (chez-register-refer-all! (chez-current-ns) target))))
    specs)
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
  (lambda (roots) (set-source-roots! (seq->list roots)) (load-data-readers!) jolt-nil))
(def-var! "jolt.host" "source-roots" (lambda () (list->cseq source-roots)))
(def-var! "jolt.host" "load-namespace" (lambda (n) (load-namespace n) jolt-nil))
(def-var! "jolt.host" "file-exists?" (lambda (p) (if (file-exists? p) #t #f)))
(def-var! "jolt.host" "getenv" (lambda (n) (let ((v (getenv n))) (if v v jolt-nil))))
