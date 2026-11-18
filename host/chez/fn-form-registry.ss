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

;; JOLT_LAZY_META=1's table of a form's rows, #("name" "text" #("free" ...) ...)
;; (backend fnsrc.clj fnsrc-tables): each text becomes its bytevector the same
;; way, when the table is expanded, and the table is quoted.
(define-syntax image-fn-form-rows
  (lambda (x)
    (syntax-case x ()
      ((k v) (vector? (syntax->datum #'v))
       (let* ((d (syntax->datum #'v)) (out (vector-copy d)) (n (vector-length d)))
         (do ((i 1 (fx+ i 3))) ((fx>= i n))
           (vector-set! out i (string->utf8 (vector-ref d i))))
         (with-syntax ((t (datum->syntax #'k out)))
           #''t))))))

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
;; mutex, and so do lookups, which may first have to file pending rows (below).
;;
;; The table, its lock and the pending queue are PRIVATE to this file (the
;; %-names; fnform-test.ss checks no other file names them). Everything else goes
;; through the public API: image-register-fn-form!, image-register-fn-forms!,
;; image-fn-form-maker! to write, image-fn-form-lookup and image-fn-form-entry to
;; read. Every read files the pending rows first under the lock, so no reader
;; can see a table missing a row registered before it looked.
(define %fn-form-tbl (make-hashtable string-hash string=?))
(define %fn-form-mu (make-mutex))


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
(define (image-register-fn-form! name form ns free-names . rest)
  (let* ((lv (if (null? rest) #f (car rest)))
         (mk (if (or (null? rest) (null? (cdr rest))) #f (cadr rest))))
    (jolt-with-mutex %fn-form-mu
      (%fn-form-file-pending!)
      (hashtable-set! %fn-form-tbl name
                      (vector form ns free-names
                              (if (or (not lv) (jolt-nil? lv)) free-names lv)
                              mk
                              #f))))
  jolt-nil)

;; Attach a site's maker to its registration. Called from inside the form's own
;; cache-cell scope, after image-register-fn-form! has created the entry at the
;; top of the form — the maker closes over that scope's cells, so it cannot be
;; built where the registration is.
;; A row still in a pending table (below) has no entry yet: its maker waits in
;; %fn-form-makers until the table is filed.
(define (image-fn-form-maker! name mk)
  (jolt-with-mutex %fn-form-mu
    (let ((reg (hashtable-ref %fn-form-tbl name #f)))
      ;; Both, when tables are pending: a name registered before and now
      ;; pending again (a def evaluated twice) has an entry the next filing
      ;; replaces, and the replacement must get this maker, not none.
      (when (and reg (fx>? (vector-length reg) 4)) (vector-set! reg 4 mk))
      (when (pair? %fn-form-pending) (hashtable-set! %fn-form-makers name mk))))
  jolt-nil)

;; --- JOLT_LAZY_META=1: rows as quoted tables, filed on first lookup ----------
;; (image-register-fn-forms! ns table) keeps a form's table as it is; the rows go
;; into the table, in registration order, the first time anything needs them:
;; a lookup, or an eager registration, which files the pending ones first so a
;; later registration of a name still replaces an earlier one. Only the image
;; writer looks, so a process that writes no image never files a row.
(define %fn-form-pending '())                      ; (ns . table), newest first
(define %fn-form-makers (make-hashtable string-hash string=?))
(define (image-register-fn-forms! ns table)
  (jolt-with-mutex %fn-form-mu
    (set! %fn-form-pending (cons (cons ns table) %fn-form-pending)))
  jolt-nil)
;; How many tables wait unfiled; reads under the lock and files nothing. For
;; the gates, which check that registration alone leaves a table pending.
(define (image-fn-form-pending-count)
  (jolt-with-mutex %fn-form-mu (length %fn-form-pending)))
(define (%fn-form-file-pending!)                   ; caller holds %fn-form-mu
  (unless (null? %fn-form-pending)
    (let ((ps (reverse %fn-form-pending)))
      (set! %fn-form-pending '())
      (for-each
        (lambda (p)
          (let ((ns (car p)) (t (cdr p)))
            (do ((i 0 (fx+ i 3))) ((fx>= i (vector-length t)))
              (let* ((name (vector-ref t i))
                     (frees (apply jolt-vector (vector->list (vector-ref t (fx+ i 2)))))
                     (mk (hashtable-ref %fn-form-makers name #f)))
                (when mk (hashtable-delete! %fn-form-makers name))
                (hashtable-set! %fn-form-tbl name
                                (vector (vector-ref t (fx+ i 1)) ns frees frees mk #f))))))
        ps))))

(define (image-fn-form-maker reg) (and (fx>? (vector-length reg) 4) (vector-ref reg 4)))
(define (image-fn-form-layout reg) (and (fx>? (vector-length reg) 5) (vector-ref reg 5)))
(define (image-fn-form-layout-set! reg v)
  (when (fx>? (vector-length reg) 5) (vector-set! reg 5 v)))

;; The registration vector (form ns free-names live-names ...) or #f when unknown —
;; the R2 dump-side lookup. A form registered as text is parsed here, once, and
;; the parse replaces the bytes; under the mutex, so two dumps racing on one
;; site parse it once and see one object.
(define (image-fn-form-lookup name)
  (jolt-with-mutex %fn-form-mu
    (%fn-form-file-pending!)
    (let ((reg (hashtable-ref %fn-form-tbl name #f)))
      (when (and reg (bytevector? (vector-ref reg 0)))
        (vector-set! reg 0 (image-fn-form-parse (utf8->string (vector-ref reg 0)))))
      reg)))
;; The same vector without parsing its form (slot 0 may still be the text's
;; bytes), or #f: for diagnostics and the gates.
(define (image-fn-form-entry name)
  (jolt-with-mutex %fn-form-mu
    (%fn-form-file-pending!)
    (hashtable-ref %fn-form-tbl name #f)))
