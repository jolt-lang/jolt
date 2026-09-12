;; fn-form-registry.ss — map unique anonymous-fn names (jfn$<ns>$<def>$<n>) back
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
(define (image-register-fn-form! name form ns free-names . rest)
  (let* ((lv (if (null? rest) #f (car rest)))
         (mk (if (or (null? rest) (null? (cdr rest))) #f (cadr rest))))
    (jolt-with-mutex fn-form-tbl-mu
      (hashtable-set! fn-form-tbl name
                      (vector form ns free-names
                              (if (or (not lv) (jolt-nil? lv)) free-names lv)
                              mk
                              #f))))
  jolt-nil)

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
