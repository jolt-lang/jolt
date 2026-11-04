;; readable printer + output seams — the __pr-str1 / __write / __with-out-str
;; host seams the overlay's pr-str/pr/prn/print/println/*-str family is built on
;; (jolt-core/clojure/core/20-coll.clj).
;;
;; jolt-pr-str (rt.ss) is STR-style: strings render raw. pr-str needs READABLE
;; (pr) style: strings quoted+escaped at every nesting level. This adds the
;; readable renderer; it mirrors jolt-pr-str but quotes strings and recurses into
;; itself, delegating scalars (nil/bool/number/keyword/symbol/char/regex) to
;; jolt-pr-str (already readable for those). The canonical ORDERED printer is
;; still future work — unordered colls render in HAMT order, compared via `=`.

;; inner string escape (no surrounding quotes): " \ newline tab return backspace formfeed.
(define (jolt-str-escape s)
  ;; most strings need nothing escaped: answer them as they are after one scan,
  ;; and build a copy only from the first character that does
  (let ((n (string-length s)))
    (let scan ((i 0))
      (cond ((fx=? i n) s)
            ((str-escape-for (string-ref s i)) (str-escape-from s i n))
            (else (scan (fx+ i 1)))))))
(define (str-escape-for c)
  (case c
    ((#\") "\\\"") ((#\\) "\\\\") ((#\newline) "\\n") ((#\tab) "\\t")
    ((#\return) "\\r") ((#\backspace) "\\b") ((#\page) "\\f")
    (else #f)))
(define (str-escape-from s start n)
  (let ((p (open-output-string)))
    (put-string p (substring s 0 start))
    (let loop ((i start))
      (if (fx=? i n)
          (get-output-string p)
          (let* ((c (string-ref s i)) (e (str-escape-for c)))
            (if e (put-string p e) (write-char c p))
            (loop (fx+ i 1)))))))

;; A host shim registers a type's readable rendering via register-pr-readable-arm!,
;; or register-pr-arm! for types whose str and readable forms match (most host types:
;; inst, uuid, record, var, …). Disjoint types, checked before the base cases.
(define jolt-pr-readable-arms '())
(define (register-pr-readable-arm! pred render)
  (pr-arm-reject-fast-type! 'register-pr-readable-arm! pred)
  (set! jolt-pr-readable-arms (cons (cons pred render) jolt-pr-readable-arms)))
(define (register-pr-arm! pred render)
  (register-pr-str-arm! pred render)
  (register-pr-readable-arm! pred render))

;; An IDeref value prints as the JVM's print-tagged-object over deref-as-map:
;; #object[cls 0xHASH {:status :ready, :val v}], the value printed the way the
;; enclosing pr or print prints. state answers (status . val), status one of
;; "ready", "pending" or "failed".
;;
;; One port takes the whole of a chain of them (an atom holding an atom …): each
;; level writes its prefix, the next level, then its suffix, so the text is built
;; once rather than copied at every level.
;;
;; A value that holds itself, directly or through collections and other IDeref
;; values, recurses forever, and the JVM raises StackOverflowError for it. Here
;; the stack limit is sized from the heap, so the heap would fill first; instead
;; the nesting is checked for a cycle as it goes, with Brent's method: vreg 14
;; holds #(saved depth power) for this thread's innermost IDeref being printed.
;; Each level compares itself with saved, and when its depth reaches power it
;; becomes saved and power doubles. Printing walks one path at a time and takes
;; the same step from the same value, so a cycle on that path repeats and meets
;; saved within about twice its length; a value seen again by eq? is the only
;; trigger, so a deep chain without a cycle never raises, and the check is one
;; comparison and three words a level. The state is restored in a masked
;; winder, so a throw or an interrupt cannot leave a stale saved value behind.
(define jolt-vreg-deref-printing 14)   ; #(saved depth power), or 0 outside a print
(define deref-print-arms '())
(define (deref-print-state x)
  (let loop ((as deref-print-arms))
    (cond ((null? as) #f)
          (((caar as) x) (cdar as))
          (else (loop (cdr as))))))
(define (display-to port s) (display s port))
(define (deref-print-to! port x state readable?)
  (let* ((st (virtual-register jolt-vreg-deref-printing))
         (saved (if (vector? st) (vector-ref st 0) #f))
         (depth (if (vector? st) (fx+ (vector-ref st 1) 1) 1))
         (power (if (vector? st) (vector-ref st 2) 1))
         (next (if (fx=? depth power)
                   (vector x depth (fx* power 2))
                   (vector saved depth power)))
         (lv (jolt-locks-held)))
    (when (eq? x saved)
      (jolt-throw (jolt-host-throwable "java.lang.StackOverflowError" jolt-nil)))
    (jolt-locks-enter!)
    (dynamic-wind
      (lambda () (set-virtual-register! jolt-vreg-deref-printing next))
      (jolt-masked-body (lambda () (deref-print-body! port x state readable?)))
      (jolt-masked-after lv (set-virtual-register! jolt-vreg-deref-printing st)))))
(define (deref-print-body! port x state readable?)
  (let ((sv (state x)))
    (display-to port "#object[")
    (display-to port (jolt-class-name x))
    (display-to port " 0x")
    (display-to port (jolt-identity-hex x))
    (display-to port " {:status :")
    (display-to port (car sv))
    (display-to port ", :val ")
    (let* ((v (cdr sv))
           (user (and readable? pr-user-method-render (pr-user-method-render v)))
           (inner (and (not user) (deref-print-state v))))
      (cond (user (display-to port user))
            (inner (deref-print-to! port v inner readable?))
            (else (display-to port (if readable? (jolt-pr-readable v) (jolt-pr-str v))))))
    (display-to port "}]")))
(define (register-deref-print! pred state)
  (define (form readable?)
    (lambda (x)
      (let ((port (open-output-string)))
        (deref-print-to! port x state readable?)
        (get-output-string port))))
  (set! deref-print-arms (cons (cons pred state) deref-print-arms))
  (register-pr-str-arm! pred (form #f))
  (register-pr-readable-arm! pred (form #t))
  ;; str is toString, which an IDeref does not override: Class@hash, the value
  ;; never visited
  (register-str-render! pred
    (lambda (x) (string-append (jolt-class-name x) "@" (jolt-identity-hex x)))))
(register-deref-print! jolt-atom? (lambda (a) (cons "ready" (jolt-atom-val a))))
;; inside a transaction a ref prints its in-transaction value
(register-deref-print! jolt-ref? (lambda (r) (cons "ready" (txn-read r))))
;; reduced's box is a clojure.lang.Reduced, an IDeref like the rest
(register-deref-print! jolt-reduced? (lambda (r) (cons "ready" (jolt-reduced-val r))))

;; A user's (defmethod print-method T …) IS the printer for T, as on the JVM — it
;; outranks every built-in rendering, including an arm a library registered for
;; its own host class. So the user method is consulted BEFORE the arm list rather
;; than being one more entry in it (where whichever arm was registered last won).
;; The java layer installs this: resolving the method needs (class x), which is
;; its concern. It returns the rendered string, or #f when there is no method.
(define pr-user-method-render #f)
(define (set-pr-user-method-render! f) (set! pr-user-method-render f))

;; *print-namespace-maps* shared-ns detector: returns the namespace string when
;; the map is non-empty, the var is true, every key is a namespace-qualified
;; keyword, and they all share the same (non-nil) namespace. Otherwise #f.
;; resolved once — see pr-readably-cell (rt.ss) for why the cached cell is safe
(define ns-maps-cell #f)
(define (pr-ns-maps-shared-ns pairs)
  ;; clojure.core/lift-ns: every key a qualified ident — keyword OR symbol — of
  ;; one namespace
  (define (ident-ns k)
    (let ((n (cond ((keyword? k) (keyword-t-ns k))
                   ((symbol-t? k) (symbol-t-ns k))
                   (else #f))))
      (and (string? n) (not (string=? n "")) n)))
  (and (pair? pairs)
       (begin
         (unless ns-maps-cell
           (set! ns-maps-cell (jolt-var "clojure.core" "*print-namespace-maps*")))
         (jolt-truthy? (jolt-var-get ns-maps-cell)))
       (let ((ns (ident-ns (caar pairs))))
         (and ns
              (let all-ns? ((rest (cdr pairs)))
                (if (null? rest)
                    ns   ;; all keys checked → return the shared namespace
                    (let ((n (ident-ns (caar rest))))
                      (and n (string=? n ns) (all-ns? (cdr rest))))))))))
;; lift-ns's strip-ns: the key without its namespace, keeping its kind
(define (pr-strip-ns k)
  (if (keyword? k) (keyword #f (keyword-t-name k)) (jolt-symbol #f (symbol-t-name k))))

(define (jolt-pr-readable-base x)
  (cond
    ;; *print-readably* nil makes pr print a string like print does — bare, no
    ;; quotes or escapes — at every nesting level. Chars already honored it
    ;; (jolt-char->string); strings were escaping unconditionally. The print
    ;; family's override (jolt-pr-readable?, rt.ss) is checked before the var,
    ;; so its per-value path skips this lookup entirely.
    ((string? x)
     (if (jolt-pr-readable?)
         (string-append "\"" (jolt-str-escape x) "\"")
         x))
    ;; pr renders the infinities / NaN in READABLE form (##Inf reads back), unlike
    ;; str's "Infinity"/"-Infinity"/"NaN". Applies at every nesting level.
    ((and (flonum? x) (fl= x +inf.0)) "##Inf")
    ((and (flonum? x) (fl= x -inf.0)) "##-Inf")
    ((and (flonum? x) (not (fl= x x))) "##NaN")
    ;; pr of an exact integer outside long range carries the BigInt N suffix
    ;; ((pr-str 12345678901234567890) => "12345678901234567890N"); str doesn't.
    ((jolt-bigint-print? x) (string-append (number->string x) "N"))
    ;; transients print as a cold tagged type (print-method routes this through a
    ;; multimethod; the readable fallback renders it directly).
    ;; forward refs to transients.ss (loaded later) — resolved at call time.
    ((jolt-transient? x)
     (case (jolt-transient-kind x)
       ((vec) "#<transient vector>") ((set) "#<transient set>") (else "#<transient map>")))
    ((pvec? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "[" (jolt-str-join (jolt-limited-vec-strs x jolt-pr-readable)) "]"))))
    ((pset? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "#{" (jolt-str-join (jolt-limited-list-strs
                       (pset-fold x (lambda (e a) (cons (jolt-pr-readable e) a)) '()))) "}"))))
    ((pmap? x)
     (if (jolt-print-hash?) "#"
         (with-deeper-print
           (let* ((pairs (pmap-fold x (lambda (k v a) (cons (cons k v) a)) '()))
                  (ns (pr-ns-maps-shared-ns pairs)))
             (if ns
                 ;; #:ns{:key val, …} — emit name-only keys in sorted order
                 (string-append "#:" ns "{"
                                (jolt-str-join-comma
                                 (jolt-limited-list-strs
                                  (map (lambda (pr)
                                         (string-append (jolt-pr-readable (pr-strip-ns (car pr)))
                                                        " " (jolt-pr-readable (cdr pr))))
                                       pairs)))
                                "}")
                 ;; standard {k v, …}
                 (string-append "{"
                                (jolt-str-join-comma
                                 (jolt-limited-list-strs
                                  (map (lambda (pr)
                                         (string-append (jolt-pr-readable (car pr)) " "
                                                        (jolt-pr-readable (cdr pr))))
                                       pairs)))
                                "}"))))))
    ((empty-list-t? x) (if (jolt-print-hash?) "#" "()"))
    ((cseq? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "(" (jolt-str-join (jolt-limited-seq-strs x jolt-pr-readable)) ")"))))
    ;; Defer to the str-side printer, whose arms and base cases these share —
    ;; passing readable? so that if it reaches the #object[…] fallback the content
    ;; is QUOTED, which is the (pr x) vs (print x) difference on the JVM.
    (else (jolt-pr-str/readable x #t))))
;; A user print-method still outranks everything, including for a fast-path type —
;; (defmethod print-method String …) is legal on the JVM — so the fast path skips
;; only the arm walk, never the user-method check.
(define (jolt-pr-readable-dispatch x)
  (or (and pr-user-method-render (pr-user-method-render x))
      (if (pr-fast-type? x)
          (jolt-pr-readable-base x)
          (let loop ((as (print-arms-for 1 jolt-pr-readable-arms x)))
            (cond ((null? as) (jolt-pr-readable-base x))
                  (((caar as) x) ((cdar as) x))
                  (else (loop (cdr as))))))))

;; *print-meta* support. The var is def'd after this file loads, so capture its
;; cell lazily; jolt-var-get (patched by dyn-binding.ss) honors a `binding`.
(define pr-meta-cell #f)
(define (pr-print-meta?)
  (unless pr-meta-cell (set! pr-meta-cell (jolt-var "clojure.core" "*print-meta*")))
  (jolt-truthy? (jolt-var-get pr-meta-cell)))
;; The metadata to print before x, or jolt-nil. A var prints as #'ns/name (its
;; {:ns :name} is derived, not user metadata) and a procedure is opaque — skip both.
(define (pr-user-meta x)
  (if (or (var-cell? x) (procedure? x)) jolt-nil (jolt-meta x)))

(define (jolt-pr-readable x)
  (if (pr-print-meta?)
      (let ((m (pr-user-meta x)))
        (if (jolt-nil? m)
            (jolt-pr-readable-dispatch x)
            (string-append "^" (jolt-pr-readable-dispatch m) " " (jolt-pr-readable-dispatch x))))
      (jolt-pr-readable-dispatch x)))

;; __pr-str1: render ONE value readably (the overlay's pr-str joins these).
(define (jolt-pr-str1 x) (jolt-pr-readable x))

;; The value `jolt -e` echoes, and what the unit gate compares against. READABLE,
;; like Clojure's REPL, which prints with pr: a result reads back as the value it
;; names, so a nested string keeps its quotes, a char keeps its \c syntax, and a
;; host object prints #object[cls "content"]. This used to go through
;; jolt-final-str, the str-style renderer, which printed ["hi" \c 1] as [hi \c 1].
;; nil still renders as the empty string, so `-e` of a side-effecting form prints
;; nothing — which is what `clojure -M -e nil` does too.
(define (jolt-repl-str x) (if (jolt-nil? x) "" (jolt-pr-readable x)))

;; __write: push a string to output. Normally this goes to the current Chez port
;; (so __with-out-str's redirect captures it). When clojure.pprint is active it
;; installs __pprint-write-hook; jolt-write then offers each string to the hook,
;; which routes it column-aware into a clojure.pprint pretty-writer if *out* is
;; bound to one (returns truthy) and otherwise declines (returns nil) so the
;; string falls through to the port. This is the JVM behaviour where core print
;; honours *out*; jolt only needs it for the pretty-printer.
(define jolt-pprint-write-hook jolt-nil)
;; suppressed while __with-out-str captures output to a string port: there the
;; redirect, not *out*, defines where text goes (pr-str / print-str rely on it).
(define jolt-pprint-hook-suppressed (make-thread-parameter #f))
(define out-cell #f)
;; What a write to the plain output port puts out. A surrogate unit (values.ss
;; jsurr) printed on its own is half a character ((print (char 0xD83D)), or
;; print over the elements of a char[] of units). The port has nowhere to hold
;; it, so a host with a pairing sink installs one here (host-static-classes.ss
;; sink-text, keyed by the port as (.write *out* ...) is) that waits for the low
;; half. Without one, a unit is U+FFFD, what finished jolt text holds for it.
(define jolt-port-text (lambda (key p) (if (jsurr? p) jolt-replacement-string p)))
(define (set-jolt-port-text! f) (set! jolt-port-text f))
;; How many sinks hold a high unit waiting for its low one (the pairing sink
;; keeps it, host-static-classes.ss). 0 is the "nothing waits anywhere" test, so
;; a plain string write is a fixnum compare and the port, nothing more.
(define sink-pending-live 0)
(define-syntax port-text
  (syntax-rules ()
    ((_ s*) (let ((s s*))
              (if (and (string? s) (fx=? sink-pending-live 0))
                  s
                  (jolt-port-text (current-output-port) s))))))
(define (jolt-write s)
  (cond ((and (not (jolt-nil? jolt-pprint-write-hook))
              (not (jolt-pprint-hook-suppressed))
              (jolt-truthy? (jolt-invoke jolt-pprint-write-hook s)))
         jolt-nil)
        ;; *out* rebound to a real writer: write through it, like the JVM's
        ;; (.write *out* s). Any value that carries a write method qualifies — a
        ;; host writer (StringWriter, PrintWriter) OR a deftype/reify/proxy one,
        ;; which is why this tests for a write METHOD rather than for jhost:
        ;; keying on the representation silently skipped every reify Writer.
        ;; The default port-writer over stdout keeps the fast port path below.
        ;; resolved once — see pr-readably-cell (rt.ss). var-cell-deref, NOT
        ;; var-cell-root: *out* is what with-out-str rebinds, so the read has to
        ;; stay on the binding stack.
        ((let ((w (begin
                    (unless out-cell
                      (set! out-cell (jolt-var "clojure.core" "*out*")))
                    (var-cell-deref out-cell))))
           (and (or (iface-method w "write" #f)
                    (and (jhost? w)
                         (not (and (string=? (jhost-tag w) "port-writer")
                                   (eq? (vector-ref (jhost-state w) 0) 'out)))))
                w))
         => (lambda (w) (record-method-dispatch w "write" (jolt-list s)) jolt-nil))
        (else (display (port-text s)) jolt-nil)))
;; (.append *out* x) — what the reference's print family uses for the space
;; between arguments, the newline, and a printed character, so a Writer under
;; *out* sees those as appends, separately from each value's write. X is a char or
;; a string. Routed exactly like jolt-write; a writer with no append method of
;; its own gets write, which is what java.io.Writer.append's default does.
;; A surrogate unit ((char 0xD83D)) goes to a host writer as itself, for
;; it to pair with its partner, and to the port through jolt-port-text. A port's
;; waiting half is keyed by the port itself (what (.write *out* ...) keys by too),
;; so the capture of a with-out-str and stdout never pair with each other.
(define (jolt-append x)
  (let ((s (if (char? x) (string x) x)))
    (cond ((and (not (jolt-nil? jolt-pprint-write-hook))
                (not (jolt-pprint-hook-suppressed))
                (jolt-truthy? (jolt-invoke jolt-pprint-write-hook
                                           (if (jsurr? s) jolt-replacement-string s))))
           jolt-nil)
          ((let ((w (begin
                      (unless out-cell
                        (set! out-cell (jolt-var "clojure.core" "*out*")))
                      (var-cell-deref out-cell))))
             (and (or (iface-method w "write" #f)
                      (and (jhost? w)
                           (not (and (string=? (jhost-tag w) "port-writer")
                                     (eq? (vector-ref (jhost-state w) 0) 'out)))))
                  w))
           => (lambda (w)
                (if (if (jhost? w)
                        (host-method-ref (jhost-tag w) "append")
                        (iface-method w "append" #f))
                    (record-method-dispatch w "append" (jolt-list x))
                    (record-method-dispatch w "write"
                                            (jolt-list (if (and (jsurr? s) (not (jhost? w)))
                                                           jolt-replacement-string
                                                           s))))
                jolt-nil))
          (else (display (port-text s)) jolt-nil))))
(def-var! "clojure.core" "__set-pprint-write-hook!"
  (lambda (f) (set! jolt-pprint-write-hook f) jolt-nil))
;; clojure.pprint wraps its writing in this so core print routes into the active
;; pretty-writer even under an outer with-out-str (which sets suppressed). A
;; pr-str/print-str nested inside then re-suppresses, so its capture still works.
(def-var! "clojure.core" "__with-pprint-routing"
  (lambda (thunk)
    (parameterize ((jolt-pprint-hook-suppressed #f)) (jolt-invoke thunk))))

;; __with-out-str: run a jolt thunk with output captured to a string, and return
;; the captured text.
;;
;; Two redirects have to agree here. with-output-to-string moves the CHEZ port,
;; which catches everything that reaches `display`; jolt-write separately honours
;; *out* when it holds a real writer. Redirecting only the port leaves an OUTER
;; (binding [*out* some-writer] …) in force, and jolt-write then sends the text to
;; that writer instead of the capture:
;;   (binding [*out* sw] (with-out-str (print "x")))  ; JVM: "x", sw empty
;; So push a *out* binding of our own, to the var's ROOT — the default stdout
;; port-writer, whose port resolves live to (current-output-port) and therefore
;; follows the redirect. Both mechanisms then point at the string port.
;;
;; Pushing a real binding rather than suppressing the *out* branch outright is
;; what keeps an INNER rebinding working: (with-out-str (binding [*out* sw] …))
;; pushes over this frame and still wins, exactly as on the JVM. The binding stack
;; lives in dyn-binding.ss, loaded after this file — resolved at call time.
(define (jolt-with-out-str thunk)
  (with-output-to-string
    (lambda ()
      (let ((cell (var-cell-lookup "clojure.core" "*out*")))
        ;; masked at its edges (jolt-wind*, locks.ss): an interrupt's escape
        ;; before the pop left *out* bound to this dead string port
        (jolt-wind*
          (lambda ()
            (when cell
              (jolt-push-thread-bindings (jolt-hash-map cell (var-cell-root cell)))))
          #f
          (lambda () (parameterize ((jolt-pprint-hook-suppressed #t)) (jolt-invoke thunk)))
          ;; a high unit still waiting for its low one ends the capture as the
          ;; lone half it is (U+FFFD) rather than staying pending for a port
          ;; that is gone, whichever way the body leaves
          (begin (unless (fx=? sink-pending-live 0)
                   (display (jolt-port-text (current-output-port) "")))
                 (when cell (jolt-pop-thread-bindings))))))))

;; __eprint / __eprintf: stderr seams. Flush each write — like the JVM's
;; auto-flushing System.err — so a long-running process (a server that never
;; returns from -main) shows its log lines instead of leaving them in a buffer
;; that only drains at exit.
(define (jolt-eprint s)
  (display s (current-error-port))
  (flush-output-port (current-error-port))
  jolt-nil)
(define (jolt-eprintf fmt . args)
  (apply fprintf (current-error-port) fmt args)
  (flush-output-port (current-error-port))
  jolt-nil)

(def-var! "clojure.core" "__pr-str1" jolt-pr-str1)
(def-var! "clojure.core" "__write" jolt-write)
(def-var! "clojure.core" "__append" jolt-append)
(def-var! "clojure.core" "__with-out-str" jolt-with-out-str)
(def-var! "clojure.core" "__eprint" jolt-eprint)
(def-var! "clojure.core" "__eprintf" jolt-eprintf)
