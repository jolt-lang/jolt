;; fn-form-registry.ss — map unique fn-literal names (jfn$<ns>/<def>$<n>) back
;; to their source form, defining ns, and free local names, so a closure
;; captured in a state image can be reconstructed as code (the R2/R3 write/read
;; sides of the image work). Registered at load time by emitted code, one
;; (image-register-fn-form! …) sibling per anon literal in a non-system
;; namespace; re-registering the same name overwrites (a re-required file re-emits
;; the same deterministic names).

;; The FORM arrives as source text: (image-fn-form-src "...") in the emitted
;; code, a UTF-8 bytevector constant once compiled. The conversion runs when the
;; form is EXPANDED (compile-file, or a source load), so a built runtime carries
;; one byte per character where a Chez string is four, and nothing runs at load.
;; The quoted construction this replaced was a let* of symbol/list/vector
;; allocations per literal, executed at every process start: for a hello-world
;; binary, 4.3 of the 8.6 MB allocated while the prelude loads, 2 of its 7 ms,
;; 7 MB of resident set, and 290 KB of the binary. The bytes are parsed on the
;; first lookup (below), and only the image writer ever looks.
(define-syntax image-fn-form-src
  (lambda (x)
    (syntax-case x ()
      ((k s) (string? (syntax->datum #'s))
       (with-syntax ((bv (datum->syntax #'k (string->utf8 (syntax->datum #'s)))))
         #''bv)))))

;; Source text -> the form, read as the compile path reads a file, minus
;; positions: the text is not that file, and the constructed form it stands in
;; for carried none. The back end checks every rendering against this same
;; parse before it emits text (backend fnsrc-row-src), through the jolt.host
;; seam, so a form that would read back differently is emitted constructed.
;; Every mode switch is pinned, not only the two positions need: the first
;; lookup runs on whichever thread dumps, and one inside an edn read would
;; otherwise parse (fn* [x] 'x) as an edn error -- which image-fnsrc-probe's
;; guard turns into "unregistered", refusing a closure that was registered.
(define (image-fn-form-parse s)
  (parameterize ((rdr-source-file #f) (rdr-suppress-pos #t) (rdr-data-read #f)
                 (rdr-edn-mode #f) (rdr-scan-mode #f) (rdr-discard-cb #f))
    (let-values (((form j) (rdr-read-top s 0 (string-length s))))
      form)))
(def-var! "jolt.host" "fn-form-parse" image-fn-form-parse)

;; Registered by emitted code at load, one call per anon literal, and namespaces
;; now load in parallel — so this runs on several threads at once. A strong
;; hashtable does not corrupt under that, but concurrent inserts do LOSE each
;; other (var-table measured 8.6k of 240k dropped), and a lost registration is a
;; closure the image writer can no longer reconstruct as code. Writes take the
;; mutex; the single-key lookup below stays unlocked, which is safe here for the
;; reasons set out at var-table in rt.ss.
(define fn-form-tbl (make-hashtable string-hash string=?))
(define fn-form-tbl-mu (make-mutex))


;; LIVE-NAMES is the optional fifth argument, and only a SPLICED copy of a
;; literal has one. free-names are the names the source form uses, so they are
;; what the restore wrapper binds; in a copy the inline pass made, those names no
;; longer describe what the compiled closure holds — a binder was renamed, a
;; caller local was substituted, or a constant argument was folded in and there
;; is no capture left at all. live-names says, per free name and in the same
;; order, either the variable name to recover from the live closure (a string) or
;; a one-element vector holding the constant value. Defaults to free-names, which
;; is what every un-spliced registration means.
;; MAKER is the optional sixth argument: (lambda (free…) <the literal>), the one
;; code object every instance of that site comes from. The dump side calls it
;; once with distinct sentinels to learn which closure slot holds which free
;; name, because Chez hands the captures back by POSITION and the names that say
;; which is which are inspector information a release build does not generate.
;; Slot 5 caches that permutation once derived; #f until then, and 'none when the
;; site has no maker or the probe could not be read.
;;
;; A #f in the live-names position means "no live-names" — a caller passing a
;; maker has to fill the fifth argument to reach the sixth.
;; CLASS is the optional seventh: the JVM class the literal compiles into, as
;; the analyzer names it (ns$f$fn__N), which (class f) and the trace report for
;; the closures the site makes (slot 6; fn-literal-class).
(define (image-register-fn-form! name form ns free-names . rest)
  (let* ((lv (if (null? rest) #f (car rest)))
         (mk (if (or (null? rest) (null? (cdr rest))) #f (cadr rest)))
         (cls (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest))) #f (caddr rest))))
    (jolt-with-mutex fn-form-tbl-mu
      (fn-name-note! name)
      (fn-flags-note! name)
      (hashtable-set! fn-form-tbl name
                      (vector form ns free-names
                              (if (or (not lv) (jolt-nil? lv)) free-names lv)
                              mk
                              #f
                              cls))))
  jolt-nil)
;; The reader positions of a top-level literal's list forms, which its
;; registered text reads back without: name -> (file . table), the table the
;; back end's "line:column ..." (fnsrc.clj fnsrc-pos-table) as UTF-8 bytes until
;; first asked for, a vector of (line . column) after. Only the runtime
;; specializer asks (compile-eval.ss), to recompile a literal with the lines a
;; backtrace reports.
(define fn-pos-tbl (make-hashtable string-hash string=?))
(define (image-register-fn-pos! name file table)
  (jolt-with-mutex fn-form-tbl-mu (hashtable-set! fn-pos-tbl name (cons file table)))
  jolt-nil)
(define (fn-pos-parse s)
  (let ((p (open-input-string s)))
    (let loop ((acc '()))
      (let ((tok (let read-tok ((cs '()))
                   (let ((c (read-char p)))
                     (cond ((eof-object? c) (if (null? cs) #f (list->string (reverse cs))))
                           ((char=? c #\space) (if (null? cs) (read-tok '()) (list->string (reverse cs))))
                           (else (read-tok (cons c cs))))))))
        (if (not tok)
            (list->vector (reverse acc))
            (let* ((k (let find ((i 0)) (if (char=? (string-ref tok i) #\:) i (find (fx+ i 1)))))
                   (l (string->number (substring tok 0 k)))
                   (c (string->number (substring tok (fx+ k 1) (string-length tok)))))
              (loop (cons (cons l c) acc))))))))
;; (file . #((line . column) ...)) or #f
(define (fn-pos-lookup name)
  (let ((e (hashtable-ref fn-pos-tbl name #f)))
    (and e
         (begin
           (when (bytevector? (cdr e))
             (jolt-with-mutex fn-form-tbl-mu
               (when (bytevector? (cdr e))
                 (set-cdr! e (guard (x (#t #f)) (fn-pos-parse (utf8->string (cdr e))))))))
           (and (vector? (cdr e)) e)))))

;; A literal whose form could not be registered (a macro spliced a live value
;; into it) still has its class -- kept apart, so the image writer never meets a
;; registration with no form.
(define fn-class-only-tbl (make-hashtable string-hash string=?))
(define (image-register-fn-class! name cls)
  (jolt-with-mutex fn-form-tbl-mu
    (fn-name-note! name)
    (hashtable-set! fn-class-only-tbl name cls))
  jolt-nil)

;; Names registered more than once. A literal inside a def takes the same name
;; each time the def is evaluated, and a namespace loaded from the AOT cache
;; never advanced this process's literal counter, so a later literal in it can
;; take a cached one's name. The closures made under the earlier registration
;; carry the same code name as the later ones, so a name seen twice no longer
;; says which source a procedure came from: the runtime specializer
;; (specialize.ss spec-owner-reg) leaves such a literal alone. Under
;; fn-form-tbl-mu.
(define fn-name-reused (make-hashtable string-hash string=?))
(define (fn-name-note! name)
  (when (or (hashtable-contains? fn-form-tbl name) (hashtable-contains? fn-class-only-tbl name))
    (hashtable-set! fn-name-reused name #t)))
(define (fn-name-reused? name)
  (jolt-with-mutex fn-form-tbl-mu (hashtable-contains? fn-name-reused name)))

;; The compile flags in force where a literal was compiled, which a recompile of
;; its source (specialize.ss) must compile under too: the loader binds
;; *unchecked-math* and *assert* per file, and a registration runs as its form
;; loads, inside that binding. Kept only when either differs from its default,
;; so a plain registration costs two derefs. name -> (unchecked-math . assert).
(define fn-flags-tbl (make-hashtable string-hash string=?))
(define fn-flag-cells #f)
(define (fn-flag-cells!)
  (or fn-flag-cells
      (let ((u (var-cell-lookup "clojure.core" "*unchecked-math*"))
            (a (var-cell-lookup "clojure.core" "*assert*")))
        (and u a (begin (set! fn-flag-cells (cons u a)) fn-flag-cells)))))
(define fn-flags-default '(#f . #t))
(define (fn-flags-note! name)
  (let ((cs (fn-flag-cells!)))
    (when cs
      (let ((um (jolt-truthy? (var-cell-deref (car cs))))
            (as (jolt-truthy? (var-cell-deref (cdr cs)))))
        (if (or um (not as))
            (hashtable-set! fn-flags-tbl name (cons um as))
            (hashtable-delete! fn-flags-tbl name))))))
(define (fn-flags-of name)
  (jolt-with-mutex fn-form-tbl-mu (hashtable-ref fn-flags-tbl name fn-flags-default)))
;; The class a registered literal's closures report, from the name its code
;; object carries (the literal's unique letrec name), or #f.
(define (fn-literal-class-of-name nm)
  (and nm
       (let ((reg (hashtable-ref fn-form-tbl nm #f)))
         (if reg
             (and (fx>? (vector-length reg) 6) (vector-ref reg 6))
             (hashtable-ref fn-class-only-tbl nm #f)))))
(define (fn-literal-class p)
  (and (procedure? p) (fn-literal-class-of-name (sa-procedure-code-name p))))
(set! procedure-class-name-hook fn-literal-class)
;; ...and its Class token. Asked per (class f), so the answer is memoized on the
;; code name OBJECT -- the one string a code object carries, so an eq?-probe
;; with no string hashing -- negative answers included (a def'd fn's name, which
;; the class arm answers). Copy-on-write, so the read is unlocked; bounded, so a
;; program asking the class of endlessly many distinct fns stops copying and
;; takes the uncached path past the bound.
(define fn-class-token-box (box (make-weak-eq-hashtable)))
(define fn-class-token-mu (make-mutex))
(define (fn-literal-class-token-slow nm)
  (let ((c (fn-literal-class-of-name nm)))
    (if c (jolt-class-for c) 'none)))
(define (fn-literal-class-token p)
  (let ((nm (sa-procedure-code-name p)))
    (and nm
         (let ((hit (hashtable-ref (unbox fn-class-token-box) nm #f)))
           (if hit
               (and (not (eq? hit 'none)) hit)
               (let ((tok (fn-literal-class-token-slow nm)))
                 (jolt-with-mutex fn-class-token-mu
                   (let ((t (unbox fn-class-token-box)))
                     (when (fx<? (hashtable-size t) 4096)
                       (let ((t2 (hashtable-copy t #t)))
                         (hashtable-set! t2 nm tok)
                         (set-box! fn-class-token-box t2)))))
                 (and (not (eq? tok 'none)) tok)))))))
(set! procedure-class-token-hook fn-literal-class-token)
;; The runtime's own closures named after the reference's classes
;; (natives-seq.ss jvm-class-named): the name IS the class.
(for-each (lambda (c) (hashtable-set! fn-class-only-tbl c c))
  '("clojure.core$map$fn__5950" "clojure.core$map$fn__5950$fn__5951"
    "clojure.core$filter$fn__5977" "clojure.core$filter$fn__5977$fn__5978"
    "clojure.core$take$fn__5993" "clojure.core$take$fn__5993$fn__5994"
    "clojure.core$drop$fn__6009" "clojure.core$drop$fn__6009$fn__6010"
    "clojure.core$take_while$fn__6001" "clojure.core$take_while$fn__6001$fn__6002"
    "clojure.core$drop_while$fn__6024" "clojure.core$drop_while$fn__6024$fn__6025"
    "clojure.core$comp$fn__5895" "clojure.core$cat$fn__8893"))

;; Attach a site's maker to its registration. Called from inside the form's own
;; cache-cell scope, after image-register-fn-form! has created the entry at the
;; top of the form — the maker closes over that scope's cells, so it cannot be
;; built where the registration is.
(define (image-fn-form-maker! name mk)
  (jolt-with-mutex fn-form-tbl-mu
    (let ((reg (hashtable-ref fn-form-tbl name #f)))
      (when (and reg (fx>? (vector-length reg) 4))
        (vector-set! reg 4 mk))))
  jolt-nil)

(define (image-fn-form-maker reg) (and (fx>? (vector-length reg) 4) (vector-ref reg 4)))
(define (image-fn-form-layout reg) (and (fx>? (vector-length reg) 5) (vector-ref reg 5)))
(define (image-fn-form-layout-set! reg v)
  (when (fx>? (vector-length reg) 5) (vector-set! reg 5 v)))

;; The registration vector (form ns free-names live-names ...) or #f when unknown —
;; the R2 dump-side lookup. A form registered as text is parsed here, once, and
;; the parse replaces the bytes; under the write mutex, so two dumps racing on
;; one site parse it once and see one object.
(define (image-fn-form-lookup name)
  (let ((reg (hashtable-ref fn-form-tbl name #f)))
    (when (and reg (bytevector? (vector-ref reg 0)))
      (jolt-with-mutex fn-form-tbl-mu
        (when (bytevector? (vector-ref reg 0))
          (vector-set! reg 0 (image-fn-form-parse (utf8->string (vector-ref reg 0)))))))
    reg))
