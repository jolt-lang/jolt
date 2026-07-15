;; build.ss — `jolt build`: AOT-compile an app into a standalone executable.
;;
;; Loaded on demand by cli.ss when the command is `build`. Defines the host
;; primitive jolt.host/build-binary, which jolt.main's build command calls after
;; resolving the project's deps + source roots.
;;
;; The pipeline (Phase 4 stage 2):
;;   1. load the entry namespace — registers its macros/vars and follows requires,
;;      recording the app namespaces in dependency order (loader's ns-loaded-hook).
;;   2. re-emit each app namespace to Scheme (the emit-image cross-compile path),
;;      now that its macros are registered.
;;   3. textually inline the cli.ss runtime load sequence into one flat source,
;;      append the app emission + a launcher that calls the entry's -main.
;;   4. compile-file -> make-boot-file -> embed the boot as C bytes -> cc-link
;;      against libkernel.a into a single self-contained binary.
;;
;; emit-image.ss supplies the cross-compiler (ei-* helpers); it's loaded here so a
;; normal run never pays for it.

(load "host/chez/emit-image.ss")
(load "host/chez/dce.ss")

;; --- shell helpers ----------------------------------------------------------
;; Run a command, return its stdout as one trimmed string ("" on no output).
(define (bld-sh-capture cmd)
  (let* ((p (process (bld-sh-wrap cmd))) (in (car p)))
    (let loop ((acc '()))
      (let ((l (get-line in)))
        (if (eof-object? l)
            (begin (close-port in)
                   ;; rejoin with newlines (get-line stripped them). Callers use
                   ;; single-line output; this just avoids silently concatenating
                   ;; two lines into one corrupt token if a command emits more.
                   (let ((ls (reverse acc)))
                     (if (null? ls) ""
                         (fold-left (lambda (s x) (string-append s "\n" x)) (car ls) (cdr ls)))))
            (loop (cons l acc)))))))

(define (bld-system cmd)
  (let ((rc (system (bld-sh-wrap cmd))))
    (unless (zero? rc)
      (error 'jolt-build (string-append "command failed (" (number->string rc) "): " cmd)))))

;; mkdir -p without a subprocess (the self-contained build shells out to nothing).
(define (bld-mkdir-p dir)
  (unless (or (string=? dir "") (string=? dir "/") (string=? dir ".") (file-exists? dir))
    (bld-mkdir-p (path-parent dir))
    ;; tolerate only the benign race (someone else created it) — a real mkdir
    ;; failure (permissions) used to surface later as a less specific
    ;; open-output-file error.
    (guard (e (#t (unless (file-exists? dir) (raise e))))
      (mkdir dir))))

(define (bld-contains? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

;; Shell-quote a path: wrap in single quotes. Paths in this project are assumed
;; to not contain single quotes (which would break the quoting).
(define (bld-sh-quote s)
  (string-append "'" s "'"))

;; --- toolchain discovery ----------------------------------------------------
(define bld-machine (symbol->string (machine-type)))
(define bld-osx? (bld-contains? bld-machine "osx"))
(define bld-nt? (bld-contains? bld-machine "nt"))

;; Platform-appropriate flag to export executable symbols so a statically-linked
;; native lib's symbols resolve via (load-shared-object #f). macOS keeps unstripped
;; dlsym visibility; Windows needs an explicit export table; ELF (Linux) needs -rdynamic.
(define (bld-export-symbols-flag)
  (cond (bld-osx? "")
        (bld-nt? "-Wl,--export-all-symbols ")
        (else "-rdynamic ")))

;; Chez's system/process run through cmd.exe on Windows; every build command
;; here is written for sh (MSYS2 provides it). On nt, spill the command to a
;; script and run `sh <file>` — workspace paths carry no spaces, and the
;; script file sidesteps cmd's quoting entirely. Identity elsewhere.
(define bld-shell-counter 0)
;; On nt, spill the command to a script and run `sh <file>`. Chez has no getpid,
;; so per-process uniqueness comes from first-use millis + a counter (the same
;; scheme spit's temp files use) — concurrent builds sharing TEMP don't collide.
;; The stamp resolves lazily: joltc bakes this file into a saved heap, and a
;; load-time stamp would freeze identical across every process run from it.
;; Delete the script on success; leave it on failure for debugging.
(define bld-shell-stamp #f)
(define (bld-sh-wrap cmd)
  (if bld-nt?
      (let* ((stamp (or bld-shell-stamp
                        (let ((s (number->string (real-time))))
                          (set! bld-shell-stamp s) s)))
             (tmp (or (getenv "TEMP") (getenv "TMP") "."))
             (f (begin (set! bld-shell-counter (+ bld-shell-counter 1))
                       (string-append tmp "\\jolt-sh-" stamp "-"
                                      (number->string bld-shell-counter) ".sh"))))
        (let ((p (open-output-file f 'replace)))
          (put-string p cmd)
          (close-port p))
        (string-append "sh " f " && rm -f " f))
      cmd))

;; The Chez executable, for the isolated compile pass (see build-binary step 4).
(define bld-chez
  (let ((p (bld-sh-capture "command -v chez || command -v scheme || command -v petite")))
    (if (> (string-length p) 0) p "chez")))

;; Chez version off (scheme-version) "Chez Scheme Version X.Y.Z" — last token.
(define bld-version
  (let* ((s (scheme-version)) (n (string-length s)))
    (let loop ((i n))
      (if (or (= i 0) (char=? (string-ref s (- i 1)) #\space))
          (substring s i n)
          (loop (- i 1))))))

;; The csv<ver>/<machine> dir holding scheme.h, libkernel.a, *.boot. Derived from
;; the chez executable's location; JOLT_CHEZ_CSV overrides.
(define bld-csv-dir
  (let ((env (getenv "JOLT_CHEZ_CSV")))
    (or (and env (> (string-length env) 0) env)
        (let* ((bindir (bld-sh-capture "dirname \"$(command -v chez || command -v scheme || command -v petite)\""))
               (cand (string-append bindir "/../lib/csv" bld-version "/" bld-machine)))
          cand))))

(define (bld-have-cc?)
  (> (string-length (bld-sh-capture "command -v cc")) 0))

(define (bld-check-toolchain)
  (for-each
    (lambda (f)
      (let ((p (string-append bld-csv-dir "/" f)))
        (unless (file-exists? p)
          (error 'jolt-build (string-append "Chez build file missing: " p
                                             "\nSet JOLT_CHEZ_CSV to the csv<ver>/<machine> dir.")))))
    '("scheme.h" "libkernel.a" "petite.boot" "scheme.boot")))

;; Link flags. macOS Homebrew layout for the kernel's lz4/zlib/ncurses deps.
(define (bld-link-libs)
  (cond
    (bld-osx?
     (let ((lz4 (bld-sh-capture "brew --prefix lz4 2>/dev/null")))
       (if (> (string-length lz4) 0)
           (string-append "-L" lz4 "/lib -llz4 -lz -lncurses -framework Foundation -liconv -lm")
           (let ((pc (bld-sh-capture "pkg-config --libs-only-L liblz4 2>/dev/null")))
             (if (> (string-length pc) 0)
                 (string-append pc " -llz4 -lz -lncurses -framework Foundation -liconv -lm")
                 (begin
                   (display "jolt build: warning: lz4 library path not found via brew or pkg-config")
                   (display " — linker may not find -llz4\n")
                   "-llz4 -lz -lncurses -framework Foundation -liconv -lm"))))))
    ;; Windows (ta6nt, MinGW-w64 under MSYS2): the Chez kernel pulls in
    ;; compression, winsock, COM/UUID, and the registry.
    (bld-nt?
          ;; -static: a single-file exe (no libwinpthread/libgcc/lz4 DLL deps) —
     ;; required for a distributable binary and for TLS init consistency.
     "-static -llz4 -lz -lws2_32 -lrpcrt4 -lole32 -luuid -ladvapi32 -luser32 -lshell32 -lm")
    ;; Linux: the Chez kernel pulls in compression (lz4/z), the expression
    ;; editor (ncurses + terminfo), threads, dlopen, libuuid, and clock_gettime.
    (else "-llz4 -lz -lncurses -ltinfo -ldl -lm -lpthread -luuid -lrt")))

;; --- runtime manifest (mirrors host/chez/cli.ss's load order) ---------------
;; A line is either literal Scheme text to inline, or a tag whose emission the build
;; controls: 'prelude (the clojure.core blob, replaced by the shaken core under
;; tree-shake), 'image + 'compile-eval (the compiler, dropped for a no-eval app).
;; Tagging keeps the splice/drop decisions off fragile substring matching.
(define bld-runtime-manifest
  (list
    "(load \"host/chez/rt.ss\")"
    "(set-chez-ns! \"clojure.core\")"
    'prelude
    "(load \"host/chez/post-prelude.ss\")"
    "(set-chez-ns! \"user\")"
    "(load \"host/chez/host-contract.ss\")"
    'image
    'compile-eval
    "(load \"host/chez/cli-core.ss\")"
    "(load \"host/chez/png.ss\")"
    "(load \"host/chez/loader.ss\")"
    "(load \"host/chez/java/ffi.ss\")"
    (string-append "(set-source-roots! " (ldr-install-roots-str) ")")))

(define bld-tagged-loads
  '((prelude . "(load \"host/chez/seed/prelude.ss\")")
    (image . "(load \"host/chez/seed/image.ss\")")
    (compile-eval . "(load \"host/chez/compile-eval.ss\")")))

;; A single-line top-level `(load "PATH")` -> PATH, else #f.
(define (bld-load-path line)
  (let ((s (let trim ((i 0))
             (if (and (< i (string-length line))
                      (memv (string-ref line i) '(#\space #\tab)))
                 (trim (+ i 1))
                 (substring line i (string-length line))))))
    (and (>= (string-length s) 7)
         (string=? (substring s 0 6) "(load ")
         (let* ((q1 (let scan ((i 6)) (if (char=? (string-ref s i) #\") i (scan (+ i 1)))))
                (q2 (let scan ((i (+ q1 1))) (if (char=? (string-ref s i) #\") i (scan (+ i 1))))))
           (substring s (+ q1 1) q2)))))

;; runtime source for PATH: from the binary's embedded store if present (a
;; self-contained joltc building an app, with no jolt checkout on disk), else read
;; from disk (running from a source checkout). build-joltc embeds every runtime
;; .ss the manifest inlines, so `build` never touches the filesystem for them.
(define (bld-source-string path)
  (let ((emb (hashtable-ref embedded-resources path #f)))
    (if (string? emb) emb (read-file-string path))))

(define (bld-string-lines s)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring s start i) acc) acc)))
            ((char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (bld-file-lines path) (bld-string-lines (bld-source-string path)))

;; Emit one line to OUT, recursively inlining a `(load ...)` of a repo file.
(define (bld-inline-line line out depth)
  (when (> depth 50) (error 'jolt-build "load nesting too deep"))
  (let ((p (bld-load-path line)))
    (if p
        (for-each (lambda (l) (bld-inline-line l out (+ depth 1))) (bld-file-lines p))
        (begin (put-string out line) (put-string out "\n")))))

;; Inline the runtime manifest, dispatching on the manifest tags. core-strs (the
;; shaken clojure.core defs, or #f) replaces the 'prelude blob; drop-compiler? (a
;; closed AOT app that never compiles from source) omits 'image + 'compile-eval —
;; the analyzer/back end are dead weight in the binary (~0.8MB).
(define (bld-emit-runtime out drop-compiler? core-strs)
  (for-each
    (lambda (entry)
      (cond
        ((eq? entry 'prelude)
         (if core-strs
             (for-each (lambda (s) (put-string out s) (put-string out "\n")) core-strs)
             (bld-inline-line (cdr (assq 'prelude bld-tagged-loads)) out 0)))
        ((memq entry '(image compile-eval))
         (unless drop-compiler? (bld-inline-line (cdr (assq entry bld-tagged-loads)) out 0)))
        (else (bld-inline-line entry out 0))))
    bld-runtime-manifest))

;; --- app emission -----------------------------------------------------------
;; Re-emit one app namespace to a list of Scheme strings: run-passes (const-fold +
;; numeric-annotate in every mode; inference also in release/optimized; inline +
;; scalar-replace additionally with direct-link) and stay strict — a form that
;; fails to emit must fail the build, not vanish.
;; The loop itself is emit-image's ei-emit-ns* (optimize? #t, guard? #f).
(define (bld-emit-ns ns-name src) (ei-emit-ns* ns-name src #t #f))

;; --- whole-program inference pre-pass ---------------------------------------
;; Analyze every app form (all namespaces, deps-first) to IR and run the
;; closed-world param-type fixpoint, so each fn's param types pick up the record
;; types its callers pass. The per-ns emit below then bare-indexes field reads and
;; devirtualizes protocol calls at those sites (the back end reads the resulting
;; :hint/:devirt annotations). Optimized builds only; registries come from the
;; runtime tables populated as the app loaded.
(define jolt-wp-infer!             (var-deref "jolt.passes.types" "wp-infer!"))
(define jolt-wp-set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define jolt-wp-set-proto-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define jolt-wp-host-record-shapes (var-deref "jolt.host" "record-shapes"))
(define jolt-wp-host-proto-methods (var-deref "jolt.host" "protocol-methods"))
(define jolt-contagion-prepass!      (var-deref "jolt.backend-scheme" "contagion-prepass!"))
(define jolt-contagion-prepass-done! (var-deref "jolt.backend-scheme" "contagion-prepass-done!"))
(define jolt-reset-clone-prepass!    (var-deref "jolt.backend-scheme" "reset-clone-prepass!"))

(define (bld-wp-infer! ordered)
  (jolt-wp-set-record-shapes! (jolt-wp-host-record-shapes #f))
  (jolt-wp-set-proto-methods! (jolt-wp-host-proto-methods #f))
  (let ((nodes '()) (ns-nodes '()))
    (for-each
      (lambda (nf)
        (set-chez-ns! (car nf))
        (let ((src (ldr-read-source (cdr nf))) (per-ns '()))
          (parameterize ((rdr-source-file (cdr nf)))
            (for-each
              (lambda (f)
                (ce-scan-requires! f (car nf))
                (unless (or (ei-ns-form? f) (ce-macro-form? f))
                  ;; a form the analyzer rejects here only loses whole-program
                  ;; type info (per-form emit still errors the build if it's
                  ;; truly broken) — but say so, or an optimized build silently
                  ;; loses inference for the namespace.
                  (guard (e (#t (display (string-append
                                          "jolt build: note: whole-program inference skipped a form in "
                                          (car nf) "\n")
                                         (current-error-port))
                                #f))
                    (let ((n (jolt-ce-analyze (make-analyze-ctx (car nf)) f)))
                      (set! nodes (cons n nodes))
                      (set! per-ns (cons n per-ns))))))
              (ei-read-all src)))
          (set! ns-nodes (cons (cons (car nf) (reverse per-ns)) ns-nodes))))
      ordered)
    (jolt-wp-infer! (apply jolt-vector (reverse nodes)))
    ;; contagion clone-site pre-pass: an impl worth a specialized clone is one that is
    ;; BOTH contagion-eligible (:num field beside a proven :double) AND reached by a
    ;; devirtualized call site. Run per-ns after wp-infer! (rich field types must be
    ;; live) so a devirt site can resolve the clone regardless of emit order.
    (jolt-reset-clone-prepass!)
    (for-each (lambda (p) (jolt-contagion-prepass! (apply jolt-vector (cdr p)) (car p))) ns-nodes)
    (jolt-contagion-prepass-done!)))

;; Strings emitted before each app ns's forms, replaying what the source loader
;; does per file: (1) set chez-current-ns so runtime ns-sensitive setup forms
;; (defmulti/defmethod resolve their target var through it) land in the right ns;
;; (2) register the ns's :as aliases so a quoted alias resolves at runtime — a
;; (defmethod ig/foo …) passes 'ig/foo to defmethod-setup, which needs ig -> the
;; real ns, but the build strips the (ns …) form that would register it.
(define (bld-scan-spec! ns-name spec emit!)
  (let ((items (cond ((pvec? spec) (seq->list spec))
                     ((and (cseq? spec) (cseq-list? spec)) (seq->list spec))
                     (else '()))))
    (when (and (pair? items) (symbol-t? (car items)))
      (let ((target (symbol-t-name (car items))))
        (let loop ((xs (cdr items)))
          (when (and (pair? xs) (pair? (cdr xs)))
            (let ((k (car xs)) (v (cadr xs)))
              (when (keyword? k)
                (cond
                  ((and (string=? (keyword-t-name k) "as") (symbol-t? v))
                   (emit! (string-append "(chez-register-alias! " (ei-str-lit ns-name)
                                         " " (ei-str-lit (symbol-t-name v))
                                         " " (ei-str-lit target) ")")))
                  ;; :refer [a b] / :refer :all — a defmethod on a referred multifn
                  ;; resolves the bare name through the refer table at runtime.
                  ((or (string=? (keyword-t-name k) "refer") (string=? (keyword-t-name k) "only"))
                   (cond
                     ((and (keyword? v) (string=? (keyword-t-name v) "all"))
                      (emit! (string-append "(chez-register-refer-all! " (ei-str-lit ns-name)
                                            " " (ei-str-lit target) ")")))
                     ((or (pvec? v) (and (cseq? v) (cseq-list? v)))
                      (for-each (lambda (n)
                                  (when (symbol-t? n)
                                    (emit! (string-append "(chez-register-refer! " (ei-str-lit ns-name)
                                                          " " (ei-str-lit (symbol-t-name n))
                                                          " " (ei-str-lit target) ")"))))
                                (seq->list v))))))))
            (loop (cddr xs))))))))

(define (bld-ns-prelude ns-name src)
  (let ((acc (list (string-append "(set-chez-ns! " (ei-str-lit ns-name) ")")))
        (nsf (let loop ((fs (ei-read-all src)))
               (cond ((null? fs) #f)
                     ((ei-ns-form? (car fs)) (car fs))
                     (else (loop (cdr fs)))))))
    (when nsf
      (for-each
        (lambda (clause)
          (when (and (cseq? clause) (cseq-list? clause))
            (let ((citems (seq->list clause)))
              (when (and (pair? citems) (keyword? (car citems))
                         (let ((kn (keyword-t-name (car citems))))
                           (or (string=? kn "require") (string=? kn "use"))))
                (for-each (lambda (spec)
                            (bld-scan-spec! ns-name spec
                                            (lambda (s) (set! acc (cons s acc)))))
                          (cdr citems))))))
        (seq->list nsf)))
    (reverse acc)))

;; --- bundling: native libs + resources --------------------------------------
;; A jolt seq of jolt strings -> a Scheme list of Scheme strings.
(define (bld-strs x) (map jolt-str-render-one (seq->list x)))

;; Emit native-library loads. `natives` is the encoded jolt seq jolt.main/
;; encode-natives produced: each entry is ["process"] | ["static" form…] |
;; ["req" cand…] | ["opt" cand…]. `which` selects 'required (process + static +
;; req) or 'optional. Required loads are emitted before the app forms (the app's
;; defcfn foreign-procedures are now lazily resolved on first call, so they can
;; be emitted before the library is loaded — the binding only becomes callable
;; after the lib loads); a load-shared-object failure there is fatal — correct
;; for a required lib. A "static" lib is cc-linked into the binary (see
;; bld-native-link-flags), so its symbols are already in the process: it loads
;; them the same way a "process" lib does. Optional loads run in the scheme-start
;; launcher, where guard catches a missing lib (the defcfn's foreign-procedure is
;; only resolved when the closure is first called, so the defining form can
;; evaluate before the library is loaded).
(define (bld-emit-natives out natives which)
  (for-each
    (lambda (entry)
      (let* ((parts (bld-strs entry)) (kind (car parts)) (cands (cdr parts))
             (cand-lits (fold-left (lambda (s c) (string-append s (ei-str-lit c) " ")) "" cands)))
        (cond
          ((and (eq? which 'required) (or (string=? kind "process") (string=? kind "static")))
           (put-string out "(jolt-build-load-native '() #f #t)\n"))
          ((and (eq? which 'required) (string=? kind "req"))
           (put-string out (string-append "(jolt-build-load-native (list " cand-lits ") #f #f)\n")))
          ((and (eq? which 'optional) (string=? kind "opt"))
           (put-string out (string-append "(jolt-build-load-native (list " cand-lits ") #t #f)\n"))))))
    (seq->list natives)))

;; The cc link fragment for the "static" natives: each archive must be FORCE-loaded
;; (the linker would otherwise drop an archive member main.c never references) and,
;; on Linux, the executable's symbols exported into the dynamic table so the
;; startup (load-shared-object #f) + foreign-procedure can resolve them (-rdynamic,
;; added by build-with-cc when this fragment is non-empty). Returns "" when no lib
;; is statically linked. Entry forms: ["static" "archive" path] | ["static" "lib"
;; name libdir].
(define (bld-native-link-flags natives)
  (fold-left
    (lambda (acc entry)
      (let ((parts (bld-strs entry)))
        (if (string=? (car parts) "static")
            (string-append acc " " (bld-one-static-link (cdr parts)))
            acc)))
    "" (seq->list natives)))

;; A statically-linked native is only in the OUTPUT binary, but build step 1
;; evaluates the app's `foreign-procedure` forms in THIS process (to register its
;; macros/vars), and Chez resolves a foreign entry eagerly. So make the archive's
;; symbols resolvable here: build a throwaway shared object from it (force-loading
;; every member) and load it. The output binary still cc-links the static archive;
;; this temp .so is build-time only. Only the "archive" form is preloaded — the
;; "lib" form names a system library the OS loader already finds by soname.
(define (bld-preload-static-natives! natives builddir)
  (let ((n 0))
    (for-each
      (lambda (entry)
        (let ((parts (bld-strs entry)))
          (when (and (string=? (car parts) "static") (string=? (cadr parts) "archive"))
            (let* ((archive (caddr parts))
                   (so (string-append builddir "/native-" (number->string n)
                                      (if bld-osx? ".dylib" ".so"))))
              (set! n (+ n 1))
              (bld-system
                (if bld-osx?
                    (string-append "cc -dynamiclib -undefined dynamic_lookup -Wl,-all_load '"
                                   archive "' -o '" so "'")
                    (string-append "cc -shared -Wl,--whole-archive '" archive
                                   "' -Wl,--no-whole-archive -Wl,--unresolved-symbols=ignore-all -o '" so "'")))
              (load-shared-object so)))))
      (seq->list natives))))

(define (bld-one-static-link form)
  (let ((kind (car form)))
    (cond
      ((string=? kind "archive")
       (let ((path (cadr form)))
         (if bld-osx?
             (string-append "-Wl,-force_load," (bld-sh-quote path))
             (string-append "-Wl,--whole-archive " (bld-sh-quote path) " -Wl,--no-whole-archive"))))
      ((string=? kind "lib")
       (let* ((lib (cadr form)) (dir (caddr form))
              (L (if (> (string-length dir) 0) (string-append "-L" dir " ") "")))
         ;; -Bstatic forces the .a over a .so of the same -l name (GNU ld). macOS's
         ;; ld64 has no -Bstatic; there an :archive path is the reliable form.
         (if bld-osx?
             (string-append (if (> (string-length dir) 0) (string-append "-L" (bld-sh-quote dir) " ") "") "-l" lib)
             (string-append (if (> (string-length dir) 0) (string-append "-L" (bld-sh-quote dir) " ") "") "-Wl,-Bstatic -l" lib " -Wl,-Bdynamic"))))
      (else ""))))

;; Walk an embed root recursively; return (resource-name . abspath) pairs, where
;; resource-name is the "/"-joined path under the root (what io/resource is asked for).
(define (bld-walk-files root rel acc)
  (let ((dir (if (string=? rel "") root (string-append root "/" rel))))
    (fold-left
      (lambda (acc name)
        (let* ((relpath (if (string=? rel "") name (string-append rel "/" name)))
               (full (string-append root "/" relpath)))
          (if (file-directory? full)
              (bld-walk-files root relpath acc)
              (cons (cons relpath full) acc))))
      acc
      (directory-list dir))))

;; Emit register-embedded-resource! per file under each embed dir. Emitted BEFORE
;; the app forms. File contents are read at BUILD time and emitted as string
;; literals — flat.ss top-level forms run at every startup with no source on disk,
;; so read-file-string at runtime would fail. The bytes are baked into the binary.
(define (bld-emit-embeds out embed-dirs)
  (for-each
    (lambda (root)
      (when (file-directory? root)
        (for-each
          (lambda (rp)
            (put-string out (string-append
                              "(register-embedded-resource! " (ei-str-lit (car rp))
                              " " (ei-str-lit (read-file-string (cdr rp))) ")\n")))
          (bld-walk-files root "" '()))))
    (bld-strs embed-dirs)))

;; --- the build --------------------------------------------------------------
;; entry-ns: the app's main namespace (a string). out-path: the binary to write.
;; mode: "dev" | "release" | "optimized". Every form runs through jolt.passes/
;; run-passes (const-fold always; inline + type inference when optimized turns on
;; direct-linking). Deps + source roots are already applied by the caller.
;; natives: encoded :jolt/native libs to load at startup. embed-dirs: dirs whose
;; files bake into the binary (single-file). ext-roots: project-relative io/resource
;; roots resolved at runtime against JOLT_PWD (ship-alongside resources).
;; direct-link?: opt-in closed-world direct-linking (app->app calls bind directly,
;; no runtime redefinition). Off by default in every mode — release stays
;; dynamically linked.
(define (bld-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))
;; --- derive namespace roots from the require graph ---------------------------
;; Data-reader namespaces load during project setup, before build-binary arms its
;; ns-loaded hook, so the entry walk records nothing for them. Collect their ns
;; names (symbol ns parts) from *data-readers*; build-binary runs the require
;; closure over them to pull in their transitive deps too.
(define (bld-data-reader-ns-names)
  (let ((tbl (var-deref "clojure.core" "*data-readers*")) (acc '()))
    (when (pmap? tbl)
      (pmap-fold tbl
        (lambda (k v a)
          (when (and (symbol-t? v) (symbol-t-ns v) (not (jolt-nil? (symbol-t-ns v))))
            (let ((nm (symbol-t-ns v)))
              (unless (member nm acc)
                (set! acc (cons nm acc)))))
          a)
        #f))
    (reverse acc)))

;; Walk top-level forms in a source file and return the list of namespace name
;; STRINGS that this file requires (via ns :require/:use clauses and top-level
;; require/use forms). Only top-level forms are inspected — no recursion into
;; subforms (the quoted-data bug ce-scan-requires! has). Specs are parsed through
;; the shared expand-spec + parse-libspec (loader.ss / ns.ss), matching the
;; loader's semantics exactly.
(define (bld-ns-requires file)
  (let ((src (ldr-read-source file)) (reqs '()))
    (for-each
      (lambda (form)
        (when (and (cseq? form) (cseq-list? form))
          (let* ((items (seq->list form))
                 (h (and (pair? items) (car items)))
                 (hn (and (symbol-t? h) (symbol-t-name h))))
            (cond
              ;; (ns name (:require spec...) ...)
              ((and hn (string=? hn "ns"))
               (for-each
                 (lambda (clause)
                   (when (and (cseq? clause) (cseq-list? clause))
                     (let ((cl (seq->list clause)))
                       (when (and (pair? cl) (keyword? (car cl))
                                  (let ((kn (keyword-t-name (car cl))))
                                    (or (string=? kn "require") (string=? kn "use"))))
                         (for-each
                           (lambda (spec)
                             (for-each
                               (lambda (s)
                                 (let ((parsed (parse-libspec s)))
                                   (when parsed
                                     (set! reqs (cons (car parsed) reqs)))))
                               (expand-spec spec)))
                           (cdr cl))))))
                 (if (pair? (cdr items)) (cddr items) '())))
              ;; (require spec...) / (use spec...) — specs are quoted
              ((and hn (or (string=? hn "require") (string=? hn "use")))
               (for-each
                 (lambda (a)
                   (let ((unquoted (ce-unquote a)))
                     (for-each
                       (lambda (s)
                         (let ((parsed (parse-libspec s)))
                           (when parsed
                             (set! reqs (cons (car parsed) reqs)))))
                       (expand-spec unquoted))))
                  (cdr items)))))))
      (map rdr-form->data (ei-read-all src)))
    (reverse reqs)))

;; Post-order DFS from a list of root namespace names: for each name, find its
;; file, recurse into its requires, then append (name . file). Already-visited
;; names are skipped (cycles terminate). Names whose source file can't be found
;; (stdlib/AOT/in-memory) are skipped — they resolve elsewhere.
;; IMPORTANT: namespaces whose resolved file is jolt-runtime-owned (embedded
;; resource or under ldr-install-roots) are also skipped — they are either
;; preloaded at joltc boot or loaded via the hook flow, and emitting them into
;; the app section would bloat the binary and break direct-link bindings.
;; Result: deps first, roots last.
(define (bld-require-closure names)
  (let ((visited (make-hashtable string-hash string=?))
        (order '()))
    (let dfs ((ns names))
      (unless (null? ns)
        (let ((name (car ns)))
          (unless (hashtable-ref visited name #f)
            (hashtable-set! visited name #t)
            (let ((file (find-ns-file name)))
              (when (and file (not (ldr-install-file? file)))
                (dfs (bld-ns-requires file))
                (set! order (cons (cons name file) order)))))
          (dfs (cdr ns)))))
    (reverse order)))

;; Bake the *data-readers* table into the binary so a runtime (read-string
;; "#my/tag …") resolves its reader fn like it does under joltc run. Tag and
;; reader are symbols; the reader path var-derefs the fn at use time.
(define (bld-sym-lit s)
  (let ((ns (symbol-t-ns s)))
    (if (and ns (not (jolt-nil? ns)))
        (string-append "(jolt-symbol " (ei-str-lit ns) " " (ei-str-lit (symbol-t-name s)) ")")
        (string-append "(jolt-symbol #f " (ei-str-lit (symbol-t-name s)) ")"))))
(define (bld-emit-data-readers out)
  (let ((tbl (var-deref "clojure.core" "*data-readers*")))
    (when (and (pmap? tbl) (> (pmap-cnt tbl) 0))
      (put-string out "\n;; === data readers ===\n")
      (put-string out "(def-var! \"clojure.core\" \"*data-readers*\"\n  (jolt-assoc empty-pmap")
      (pmap-fold tbl
        (lambda (k v a)
          (put-string out (string-append "\n    " (bld-sym-lit k) " " (bld-sym-lit v)))
          a)
        #f)
      (put-string out "))\n"))))

(define (build-binary entry-ns out-path mode natives embed-dirs ext-roots direct-link? tree-shake? library?)
  ;; Windows executables carry .exe; normalize here so the append-payload and
  ;; cc paths agree and the shell can run the result. A library keeps its own
  ;; suffix (.dll/.so/.dylib) — never rewrite it to .exe.
  (let ((out-path (if (and bld-nt? (not library?) (not (bld-suffix? out-path ".exe")))
                      (string-append out-path ".exe")
                      out-path)))
  ;; The self-contained path (jolt-embedded-bytes "stub/launcher") needs no csv
  ;; kernel files, no Chez, no cc — only the legacy cc path does. A --library build
  ;; ALWAYS takes the cc path (build-shared), so it needs the toolchain even from
  ;; the self-contained joltc.
  (when (or library? (not (jolt-embedded-bytes "stub/launcher"))) (bld-check-toolchain))
  (when (> (string-length (bld-native-link-flags natives)) 0)
    ;; :static natives are cc-linked into the binary, so a C compiler must be on
    ;; PATH — the self-contained joltc bundles the Chez kernel (libkernel.a +
    ;; scheme.h) and relinks a custom stub (see build-self-contained), but still
    ;; needs the system cc for that link. Fail early (before the app's foreign-
    ;; procedure forms eval below) with an actionable message.
    (unless (bld-have-cc?)
      (error 'jolt-build
        "static native linking needs a C compiler (cc) on PATH; install one, or pass --dynamic to load the library at runtime."))
    ;; Preload static archives' symbols into this process so step 1's foreign-
    ;; procedure evals resolve; the .build dir must exist first.
    (bld-mkdir-p (string-append out-path ".build"))
    (bld-preload-static-natives! natives (string-append out-path ".build")))
  ;; 1. record app namespaces in dependency order as they finish loading.
  (let ((app-order '()))
    (set-ns-loaded-hook!
      (lambda (name file) (set! app-order (cons (cons name file) app-order))))
    (load-namespace entry-ns)
    (set-ns-loaded-hook! (lambda (name file) #f))
    ;; Build ordered ns list from the require graph (static scan of source files)
    ;; merged with the hook's load order. The graph gives post-order deps; the
    ;; hook captures dynamic requires the static scan can't see.
    (let* ((graph (bld-require-closure (list entry-ns)))
           (walked (reverse app-order))
           ;; graph without the entry-ns pair (it goes last)
           (graph-rest (if (and (pair? graph)
                                (string=? (caar (reverse graph)) entry-ns))
                           (reverse (cdr (reverse graph)))
                           graph))
           ;; reader namespaces with transitive closure
           (reader-ns-names (bld-data-reader-ns-names))
           (reader-pairs (bld-require-closure reader-ns-names))
           ;; only keep reader pairs not already in graph-rest or walked
           (reader-pairs
             (filter (lambda (p)
                       (not (or (assoc (car p) graph-rest)
                                (assoc (car p) walked))))
                     reader-pairs))
           ;; merge: reader pairs + graph-rest + walked novelties (preserving
           ;; walked order for dynamic requires the scan missed)
           (merged (append reader-pairs graph-rest))
           (merged
             (let loop ((w walked) (m merged))
               (if (null? w)
                   m
                   (if (assoc (caar w) m)
                       (loop (cdr w) m)
                       (loop (cdr w) (append m (list (car w))))))))
           ;; ensure entry-ns is last
           (entry-pair (or (assoc entry-ns merged)
                           (assoc entry-ns walked)
                           (cons entry-ns (find-ns-file entry-ns))))
           (ordered (append (remp (lambda (p) (string=? (car p) entry-ns)) merged)
                            (list entry-pair))))
      (when (null? ordered)
        (error 'jolt-build (string-append "no source namespace loaded for " entry-ns
                                          " — is it on the source roots?")))
      ;; 2. emit each app namespace. Release and optimized modes enable the
      ;; inference + record-shape setup passes (inference-enabled?); optimized
      ;; mode with direct-link additionally runs the inline + flatten +
      ;; scalar-replace fixpoint (inline-enabled?). Dev mode gets const-fold +
      ;; numeric-annotate only.
      ;; direct-link? (opt-in) commits to a closed world: app->app calls bind
      ;; directly, giving up runtime redefinition of those vars. Off by default in
      ;; every mode. The defined-set accumulates across the dependency-ordered
      ;; namespaces, so a dep's defs are direct-linkable by the time the entry that
      ;; calls them is emitted.
      ;; set-optimize!/set-direct-link! are process-global flags in the back end;
      ;; dynamic-wind guarantees they revert even if a strict form errors mid-emit
      ;; (a failing form errors the build by design), so the compiler isn't left in
      ;; optimize/direct-link mode for a later caller.
      (let*-values
          (((core-strs app-strs drop-compiler?)
            (dynamic-wind
              (lambda ()
                (set-optimize! (string=? mode "optimized"))
                (set-release! (string=? mode "release"))
                (when direct-link?
                  ((var-deref "jolt.backend-scheme" "set-direct-link!") #t)
                  ((var-deref "jolt.backend-scheme" "direct-link-reset!"))
                  (set-direct-link-flag! #t))
                ;; Cache resolved var cells per reference site in the APP forms
                ;; (bld-emit-ns / ei-emit-ns-records). A user build is a single
                ;; compile of fixed source, so the gensym-numbered cell names are
                ;; deterministic — the byte-fixpoint concern (the compiler re-
                ;; compiling itself) does NOT apply here, only to the seed mint,
                ;; which keeps var-cache OFF (emit-image.ss). ON in both modes.
                ((var-deref "jolt.backend-scheme" "set-var-cache!") #t)
                ;; whole-program param-type fixpoint before per-form emit
                (when (string=? mode "optimized") (bld-wp-infer! ordered)))
              (lambda ()
                ;; A #tag data-reader literal must compile in the binary the same as
                ;; it loads interpreted — apply the reader rewrite to each emitted
                ;; form too (no-op unless the app registered data readers).
                (parameterize ((ei-emit-form-hook
                                (lambda (form) (if data-readers-active (ldr-apply-readers form) form))))
                (if tree-shake?
                    (dce-shake
                      (dce-blob-records "host/chez/seed/prelude.ss")
                      (apply append
                        (map (lambda (nf)
                               ;; ns-prelude forms (always kept, no fqn/refs) set the
                               ;; ns + register aliases before this ns's forms; dce
                               ;; keeps original order.
                               (let ((src (ldr-read-source (cdr nf))))
                                 (parameterize ((rdr-source-file (cdr nf)))
                                   (append
                                     (map (lambda (s) (dce-rec #t #f '() s))
                                          (bld-ns-prelude (car nf) src))
                                     (ei-emit-ns-records (car nf) src)))))
                             ordered))
                      (string-append entry-ns "/-main"))
                    (values #f
                            (apply append
                              (map (lambda (nf)
                                     (let ((src (ldr-read-source (cdr nf))))
                                       (parameterize ((rdr-source-file (cdr nf)))
                                         (append (bld-ns-prelude (car nf) src)
                                                 (bld-emit-ns (car nf) src)))))
                                   ordered))
                            #f))))
              (lambda ()
                (set-optimize! #f)
                (set-release! #f)
                (set-direct-link-flag! #f)
                ((var-deref "jolt.backend-scheme" "set-direct-link!") #f)
                ;; drop the accumulated direct-link fqn set too — a later
                ;; in-process build would otherwise bind calls against defs
                ;; recorded for THIS one. (bld-wp-infer!'s record/protocol
                ;; seeds self-heal: the next build replaces them wholesale.)
                ((var-deref "jolt.backend-scheme" "direct-link-reset!"))
                ((var-deref "jolt.backend-scheme" "set-var-cache!") #f)))))
        (when drop-compiler? (display "jolt build: dropping compiler image (no runtime eval)\n"))
      (let* ((builddir (string-append out-path ".build"))
             (flat-ss  (string-append builddir "/flat.ss"))
             (flat-so  (string-append builddir "/flat.so"))
             (boot     (string-append builddir "/jolt.boot"))
             (boot-h   (string-append builddir "/boot_data.h"))
             (main-c   (string-append builddir "/main.c")))
        (bld-mkdir-p builddir)
        ;; 3. flat source = runtime + app + launcher.
        (let ((out (open-output-file flat-ss 'replace)))
          (bld-emit-runtime out drop-compiler? core-strs)
          ;; Load native libs, bake embedded resources, and point source roots at
          ;; the build-time app roots — all BEFORE the app forms. The app's
          ;; top-level forms run at binary startup (Sbuild_heap), and they include
          ;; foreign-procedure evals (a library's defcfn) and (slurp (io/resource …))
          ;; reads. So the libraries must be loaded and resources resolvable by the
          ;; time those forms run, not later in the scheme-start launcher.
          (put-string out "\n;; === native libraries (required) ===\n")
          (bld-emit-natives out natives 'required)
          (put-string out "\n;; === embedded resources ===\n")
          (bld-emit-embeds out embed-dirs)
           (bld-emit-data-readers out)
           ;; set-source-roots!* (not the scanning set-source-roots!): data readers
           ;; are baked just above, and re-scanning would eagerly reload reader
           ;; namespaces via jolt-compile-eval-form — dropped by a tree-shaken binary.
           (put-string out (string-append
                             "(set-source-roots!* (list "
                             (fold-left (lambda (s r) (string-append s (ei-str-lit r) " ")) ""
                                        (get-source-roots))
                             "))\n"))
          (put-string out "\n;; === app ===\n")
          (for-each (lambda (s) (put-string out s) (put-string out "\n")) app-strs)
          ;; The launcher runs as Chez's scheme-start (so argv reaches -main —
          ;; top-level boot forms run during heap build, before args are set), and
          ;; suppresses the interactive greeting. It resets source roots to the
          ;; app's resource dirs resolved against JOLT_PWD (or cwd) so a runtime
          ;; io/resource that wasn't embedded still resolves next to the binary.
          (put-string out "\n;; === launcher ===\n")
          (put-string out "(suppress-greeting #t)\n")
          ;; GC tuning: larger nursery for allocation-heavy workloads (binary-trees,
          ;; ray tracer, etc.). Default 16 MB; override via JOLT_GC_TRIP_BYTES
          ;; environment variable (integer bytes, e.g. \"33554432\" for 32 MB).
          (put-string out
            (string-append
              "(collect-trip-bytes\n"
              "  (let ((trip (getenv \"JOLT_GC_TRIP_BYTES\"))\n"
              "        (default (* 16 1024 1024)))\n"
              "    (if trip (or (string->number trip) default) default)))\n"))
          (put-string out "(scheme-start\n  (lambda args\n")
          ;; The prologue (optional native loads + source-root setup) and the -main
          ;; call (or library export publish) run under one guard so a throw in
          ;; either surfaces as jolt-report-throwable + a non-zero exit/return
          ;; instead of Chez's opaque dump — the prologue previously ran before any
          ;; guard. A library returns 1 (so Sscheme_start returns non-zero to its
          ;; caller); an executable exits 1.
          (put-string out
            (string-append
              "    (guard (v (#t (jolt-report-throwable v (current-error-port))"
              (if library? " 1))\n" " (exit 1)))\n")))
          (bld-emit-natives out natives 'optional)
           (put-string out (string-append
                              "      (let ((base (or (getenv \"JOLT_PWD\") \".\")))\n"
                              "        (set-source-roots!*\n"
                              "          (append (map (lambda (r) (string-append base \"/\" r)) (list "
                             (fold-left (lambda (s r) (string-append s (ei-str-lit r) " ")) "" (bld-strs ext-roots))
                             "))\n"
                             "                  " (ldr-install-roots-str) "))))\n"))
          (if library?
              (put-string out (bld-library-launcher-body))
              (put-string out (string-append
                            ;; Call -main only if the entry namespace defines one;
                            ;; a script ns (top-level side effects, no -main) has
                            ;; already run its forms at heap build, so invoking a nil
                            ;; -main would crash ("nil cannot be cast to IFn") — just
                            ;; exit cleanly instead.
                            "      (let ((maincell (var-cell-lookup " (ei-str-lit entry-ns) " \"-main\")))\n"
                            ;; Loading the app left the current ns at the entry ns; reset
                            ;; it to `user` before -main, matching clojure.main (*ns* is
                            ;; `user` when a `-m` -main runs, so a runtime resolve of an
                            ;; aliased symbol behaves the same as on the JVM / interpreted
                            ;; joltc, not off the entry ns's alias table).
                            "        (set-chez-ns! \"user\")\n"
                            "        (when (and maincell (var-cell-defined? maincell))\n"
                            "          (apply jolt-invoke (var-cell-root maincell) args))))\n"
                            "    (exit 0)))\n")))
          (close-port out))
        ;; 4. compile -> boot -> link. Two paths, chosen by whether this process
        ;; carries the bundled Chez boots + launcher stub:
        ;;  - SELF-CONTAINED (the distributed joltc, jolt-eaj): compile-file +
        ;;    make-boot-file run IN PROCESS (the compiler is resident — joltc is
        ;;    built from scheme.boot), then the boot is appended to a copy of the
        ;;    embedded stub. No external Chez, no cc.
        ;;  - LEGACY (dev bin/joltc): spawn a fresh Chez for compile-file/
        ;;    make-boot-file, then xxd the boot into a C array and cc-link against
        ;;    libkernel.a. Kept so `make buildsmoke` still exercises the cc path.
        (cond
          (library?
           (build-shared entry-ns out-path mode builddir flat-ss flat-so boot boot-h
                         (bld-native-link-flags natives)))
          ;; petite-only is POSIX-only: on Windows jolt-foreign-proc-safe still
          ;; evals its foreign-procedure forms (fasl relocations abort the boot
          ;; there), and eval needs the compiler boot resident.
          ((jolt-embedded-bytes "stub/launcher")
           (build-self-contained entry-ns out-path mode builddir flat-ss flat-so boot
                                 (bld-native-link-flags natives)
                                 (and drop-compiler? (not bld-nt?))))
          (else
           (build-with-cc entry-ns out-path mode builddir flat-ss flat-so boot boot-h main-c
                          (bld-native-link-flags natives)
                          (and drop-compiler? (not bld-nt?)))))))))))

;; --- self-contained link (in-process compile + append the boot to the stub) ---
;; compile-file runs against the DEFAULT interaction environment, so the boot's
;; top-level defines land in the real symbol cells — the runtime compiler's
;; eval'd code must resolve them (var-deref, jolt-invoke, the jolt-n* macros)
;; when the built binary dynamically requires a namespace. Compiling in a clean
;; copy-environment instead orphans every define in locations eval can't see,
;; and the binary dies with "variable var-deref is not bound" the moment a
;; runtime require compiles source.
;;
;; The default env has a wrinkle the legacy fresh-Chez path doesn't: THIS
;; process's cells hold jolt's redefinitions of some kernel names (`error`,
;; regex.ss), so references to them compile as cell reads — and a read that
;; runs before the redefining form would find the fresh binary's cell unbound.
;; The prologue closes that: it first binds each redefined kernel name's cell
;; to its kernel value, making the boot's earliest reads identical to the
;; legacy path's primitive references.

;; every top-level (define nm …)/(define (nm …) …) name in the flat file that
;; shadows a scheme-environment VARIABLE (syntax names don't eval; skip them).
(define (bld-kernel-prologue flat-ss)
  (let ((seen (make-eq-hashtable))
        (kenv (scheme-environment))
        (names '()))
    (let ((ip (open-input-file flat-ss)))
      (let loop ()
        (let ((f (read ip)))
          (unless (eof-object? f)
            (when (and (pair? f) (eq? (car f) 'define) (pair? (cdr f)))
              (let* ((h (cadr f))
                     (nm (if (pair? h) (car h) h)))
                (when (and (symbol? nm)
                           (not (hashtable-ref seen nm #f))
                           (guard (e (#t #f)) (begin (eval nm kenv) #t)))
                  (hashtable-set! seen nm #t)
                  (set! names (cons nm names)))))
            (loop))))
      (close-port ip))
    (apply string-append
           (map (lambda (nm)
                  (let ((s (symbol->string nm)))
                    (string-append "(define " s " (eval '" s " (scheme-environment)))\n")))
                (reverse names)))))

;; prepend the prologue to the flat file in place.
(define (bld-prepend-prologue! flat-ss)
  (let ((prologue (bld-kernel-prologue flat-ss))
        (body (read-file-string flat-ss)))
    (let ((out (open-output-file flat-ss 'replace)))
      (put-string out ";; kernel-name cells pre-bound so early reads match the kernel primitives\n")
      (put-string out prologue)
      (put-string out body)
      (close-port out))))

;; Per-mode Chez compile parameters for app binaries. Mirrors the pattern in
;; build-joltc.ss (optimize-level 2, fasl-compressed #t for release/optimized).
;; "release" keeps inspector + proc-source ON so Clojure backtraces (via
;; inspect/object walking the continuation) survive. "optimized" turns them OFF
;; for max speed. "dev" leaves Chez defaults (optimize-level 2, inspector ON,
;; proc-source ON, fasl uncompressed — full debuggability).
(define (bld-chez-param-forms mode)
  ;; optimize-level 2, not 3: level 3 is Chez's UNSAFE mode — fx/fl/car/vector
  ;; ops skip their type checks, and jolt's error semantics depend on those
  ;; raising ((take nil coll) must throw, not walk off a nil count). Level 2
  ;; keeps every check with nearly all of the optimization.
  (cond
    ((string=? mode "optimized")
     (string-append
       "(optimize-level 2)\n"
       "(generate-inspector-information #f)\n"
       "(generate-procedure-source-information #f)\n"
       "(fasl-compressed #t)\n"))
    ((string=? mode "release")
     (string-append
       "(optimize-level 2)\n"
       "(generate-inspector-information #t)\n"
       "(generate-procedure-source-information #t)\n"
       "(fasl-compressed #t)\n"))
    (else "")))

(define (build-self-contained entry-ns out-path mode builddir flat-ss flat-so boot native-link petite-only?)
  (let ((petite (string-append builddir "/petite.boot"))
        (scheme (string-append builddir "/scheme.boot")))
    (jolt-spill-embedded! "csv/petite.boot" petite)
    (unless petite-only? (jolt-spill-embedded! "csv/scheme.boot" scheme))
    (display (string-append "jolt build: compiling " entry-ns " (" mode " mode, self-contained)\n"))
    (bld-prepend-prologue! flat-ss)
    (cond
      ((string=? mode "optimized")
       (parameterize ((optimize-level 2) (generate-inspector-information #f)
                       (generate-procedure-source-information #f) (fasl-compressed #t))
         (compile-file flat-ss flat-so)))
      ((string=? mode "release")
       (parameterize ((optimize-level 2) (generate-inspector-information #t)
                       (generate-procedure-source-information #t) (fasl-compressed #t))
         (compile-file flat-ss flat-so)))
      (else
       (compile-file flat-ss flat-so)))
    ;; A compiler-dropped binary (no runtime eval) boots from petite alone —
    ;; scheme.boot is the Chez compiler, ~5 MB of heap and ~1 MB of binary it
    ;; would never call. Chez's interpreter (petite) can't create a
    ;; foreign-procedure at runtime, but every defcfn in the image was
    ;; AOT-compiled, so the FFI is unaffected.
    (if petite-only?
        (make-boot-file boot '() petite flat-so)
        (make-boot-file boot '() petite scheme flat-so))
    ;; The stub is the native launcher the boot is appended to. With no :static
    ;; natives it's the prebuilt one bundled in joltc (no cc needed); with :static
    ;; natives it's re-linked here from the bundled kernel + launcher source so the
    ;; archives are baked in and their symbols resolve in the running binary.
    (if (> (string-length native-link) 0)
        (bld-relink-stub builddir native-link out-path)
        (jolt-spill-embedded! "stub/launcher" out-path))
    ;; link: stub bytes ++ boot ++ frame, then make it executable.
    (jolt-append-payload! out-path (read-file-bytes boot))
    (jolt-chmod-755 out-path)
    (display (string-append "jolt build: wrote " out-path "\n"))
    (when bld-osx?
      (display (string-append
                 "jolt build: note — on macOS this binary is unsigned; to share it,\n"
                 "  `xattr -d com.apple.quarantine " out-path "` on the target, or sign it.\n")))))

;; Re-link the launcher stub with the app's static native archives baked in, to
;; OUT-PATH. The self-contained joltc bundles the Chez kernel (libkernel.a),
;; header, and launcher source; spill them and drive the system cc — the same link
;; build-joltc.ss ran once at joltc-build time, plus the force-load archive flags
;; (native-link) and, on Linux, -rdynamic so the baked-in symbols stay dlsym-
;; visible for (load-shared-object #f) + foreign-procedure at startup.
(define (bld-relink-stub builddir native-link out-path)
  (let ((h  (string-append builddir "/scheme.h"))
        (lk (string-append builddir "/libkernel.a"))
        (lc (string-append builddir "/launcher.c")))
    (jolt-spill-embedded! "csv/scheme.h" h)
    (jolt-spill-embedded! "csv/libkernel.a" lk)
    (jolt-spill-embedded! "stub/launcher.c" lc)
    (display "jolt build: relinking launcher stub with static native libraries\n")
    (bld-system (string-append
      "cc -O2 " (bld-export-symbols-flag)
      "-I'" builddir "' '" lc "' '" lk "' -o '" out-path "' "
      native-link " " (bld-link-libs)))))

;; --- legacy cc link (dev bin/joltc): fresh Chez compile + xxd + cc ------------
(define (build-with-cc entry-ns out-path mode builddir flat-ss flat-so boot boot-h main-c native-link petite-only?)
  (display (string-append "jolt build: compiling " entry-ns " (" mode " mode)\n"))
  (let ((cs (string-append builddir "/compile.ss")))
    (let ((p (open-output-file cs 'replace)))
      (put-string p
        (string-append
          "(import (chezscheme))\n"
          (bld-chez-param-forms mode)
          "(compile-file " (ei-str-lit flat-ss) " " (ei-str-lit flat-so) ")\n"
          ;; petite-only boot when the compiler image was dropped (see
          ;; build-self-contained).
          "(make-boot-file " (ei-str-lit boot) " '()\n  "
          (ei-str-lit (string-append bld-csv-dir "/petite.boot")) "\n  "
          (if petite-only?
              ""
              (string-append (ei-str-lit (string-append bld-csv-dir "/scheme.boot")) "\n  "))
          (ei-str-lit flat-so) ")\n"))
      (close-port p))
    (bld-system (string-append bld-chez " --script '" cs "'")))
  (bld-system (string-append "xxd -i '" boot "' > '" boot-h "'"))
  ;; The xxd symbol is derived from the path; normalize to jolt_boot.
  (bld-system (string-append
    "sed -i.bak -E 's/unsigned char [A-Za-z0-9_]+\\[\\]/unsigned char jolt_boot[]/; "
    "s/unsigned int [A-Za-z0-9_]+_len/unsigned int jolt_boot_len/' '" boot-h "'"))
  (let ((mc (open-output-file main-c 'replace)))
    (put-string mc
      (string-append
        "#include \"scheme.h\"\n#include \"boot_data.h\"\n"
        "int main(int argc, char *argv[]) {\n"
        "  Sscheme_init(0);\n"
        "  Sregister_boot_file_bytes(\"jolt\", jolt_boot, jolt_boot_len);\n"
        "  Sbuild_heap(0, 0);\n"
        "  int status = Sscheme_start(argc, (const char **)argv);\n"
        "  Sscheme_deinit();\n  return status;\n}\n"))
    (close-port mc))
  ;; -rdynamic (Linux) exports the executable's symbols into the dynamic table so
  ;; a statically-linked native lib's symbols resolve via (load-shared-object #f)
  ;; at startup. macOS keeps unstripped executable symbols dlsym-visible already.
  (bld-system (string-append
    "cc -O2 " (if (> (string-length native-link) 0) (bld-export-symbols-flag) "")
    "-I'" bld-csv-dir "' '" main-c "' '" bld-csv-dir "/libkernel.a' "
    "-o '" out-path "' " native-link " " (bld-link-libs)))
  (display (string-append "jolt build: wrote " out-path "\n")))

;; --- shared-library link (jolt build --library) -----------------------------
;; The cc path adapted to emit a shared object instead of an executable: the same
;; compile-file + make-boot-file + xxd boot embedding, but a library.c stub
;; (jolt_library_init / jolt_lookup / jolt_library_shutdown instead of main) and
;; a -shared/-dynamiclib link. Only the cc path supports libraries today — the
;; self-contained append-to-prebuilt-stub path would need a library stub variant
;; baked into the distributed joltc (a follow-up).
;; last path segment of p (after the final '/'), for a dylib's -install_name.
(define (bld-basename p)
  (let loop ((i (fx- (string-length p) 1)))
    (cond ((fx<? i 0) p)
          ((char=? (string-ref p i) #\/) (substring p (fx+ i 1) (string-length p)))
          (else (loop (fx- i 1))))))

(define (bld-library-stub)
  (string-append
    "#include \"scheme.h\"\n"
    "#include <string.h>\n"
    "#include \"boot_data.h\"\n"
    "/* jolt_set_lookup_addr is called from the built library's scheme-start\n"
    "   handler (registered via Sforeign_symbol after Sbuild_heap) to hand the\n"
    "   stub the Scheme lookup callable's address. */\n"
    "static void* (*jolt_lookup_fn)(const char*) = 0;\n"
    "void jolt_set_lookup_addr(void* fn) { jolt_lookup_fn = (void*(*)(const char*))fn; }\n"
    "void* jolt_lookup(const char* name) { return jolt_lookup_fn ? jolt_lookup_fn(name) : 0; }\n"
    "int jolt_library_init(int argc, char** argv) {\n"
    "  if (!argv) argc = 0;  /* Sscheme_start reads argv[0..argc-1]; a NULL argv means no args */\n"
    "  Sscheme_init(0);\n"
    "  Sregister_boot_file_bytes(\"jolt\", jolt_boot, (iptr)jolt_boot_len);\n"
    "  Sbuild_heap(0, 0);\n"
    "  Sforeign_symbol(\"jolt_set_lookup_addr\", (void*)jolt_set_lookup_addr);\n"
    "  return Sscheme_start(argc, (const char**)argv); }\n"
    "void jolt_library_shutdown(void) { Sscheme_deinit(); }\n"))

;; The library scheme-start tail BODY: publish the export table to the embedder,
;; then return 0 so Sscheme_start returns to jolt_library_init's caller. The guard
;; (returning 1 on failure) is emitted by build-binary around the whole launcher —
;; prologue + this body — so an init failure anywhere reports and returns non-zero;
;; otherwise jolt_set_lookup_addr never runs and jolt_lookup silently returns NULL.
(define (bld-library-launcher-body)
  (string-append
    "      ;; publish the export table to the embedder\n"
    "      (let* ((lk (foreign-callable jolt-ffi-lookup-export (string) uptr))\n"
    "             (lk-addr (jolt-ffi-register-callable! lk)))\n"
    "        ((foreign-procedure \"jolt_set_lookup_addr\" (void*) void) lk-addr))\n"
    "      0)))\n"))

(define (build-shared entry-ns out-path mode builddir flat-ss flat-so boot boot-h native-link)
  (display (string-append "jolt build: compiling " entry-ns " (" mode " mode, shared library)\n"))
  (let ((cs (string-append builddir "/compile.ss")))
    (let ((p (open-output-file cs 'replace)))
      (put-string p
        (string-append
          "(import (chezscheme))\n"
          (bld-chez-param-forms mode)
          "(compile-file " (ei-str-lit flat-ss) " " (ei-str-lit flat-so) ")\n"
          "(make-boot-file " (ei-str-lit boot) " '()\n  "
          (ei-str-lit (string-append bld-csv-dir "/petite.boot")) "\n  "
          (ei-str-lit (string-append bld-csv-dir "/scheme.boot")) "\n  "
          (ei-str-lit flat-so) ")\n"))
      (close-port p))
    (bld-system (string-append bld-chez " --script '" cs "'")))
  (bld-system (string-append "xxd -i '" boot "' > '" boot-h "'"))
  (bld-system (string-append
    "sed -i.bak -E 's/unsigned char [A-Za-z0-9_]+\\[\\]/unsigned char jolt_boot[]/; "
    "s/unsigned int [A-Za-z0-9_]+_len/unsigned int jolt_boot_len/' '" boot-h "'"))
  (let ((lc (string-append builddir "/library.c")))
    (let ((p (open-output-file lc 'replace)))
      (put-string p (bld-library-stub))
      (close-port p))
    (bld-system (string-append
      "cc -O2 -fPIC "
      ;; -install_name @rpath/<base> so a binary that link-edits against the dylib
      ;; (rather than dlopen'ing it) can locate it via its rpath, not a build-dir path.
      (if bld-osx?
          (string-append "-dynamiclib -install_name '@rpath/" (bld-basename out-path) "' ")
          "-shared ")
      "-I'" bld-csv-dir "' '" lc "' '" bld-csv-dir "/libkernel.a' "
      "-o '" out-path "' " native-link " " (bld-link-libs))))
  (display (string-append "jolt build: wrote " out-path "\n")))

(def-var! "jolt.host" "build-binary"
  (lambda (entry out mode natives embed-dirs ext-roots direct-link? tree-shake?)
    (build-binary (jolt-str-render-one entry)
                  (jolt-str-render-one out)
                  (jolt-str-render-one mode)
                  natives embed-dirs ext-roots (jolt-truthy? direct-link?) (jolt-truthy? tree-shake?) #f)
    jolt-nil))
(def-var! "jolt.host" "build-library"
  (lambda (entry out mode natives embed-dirs ext-roots direct-link? tree-shake?)
    (build-binary (jolt-str-render-one entry)
                  (jolt-str-render-one out)
                  (jolt-str-render-one mode)
                  natives embed-dirs ext-roots (jolt-truthy? direct-link?) (jolt-truthy? tree-shake?) #t)
    jolt-nil))
