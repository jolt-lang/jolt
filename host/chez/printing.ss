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

;; inner string escape (no surrounding quotes): " \ newline tab return.
(define (jolt-str-escape s)
  (let loop ((cs (string->list s)) (acc '()))
    (if (null? cs)
        (list->string (reverse acc))
        (loop (cdr cs)
              (let ((c (car cs)))
                (case c
                  ((#\") (cons #\" (cons #\\ acc)))
                  ((#\\) (cons #\\ (cons #\\ acc)))
                  ((#\newline) (cons #\n (cons #\\ acc)))
                  ((#\tab) (cons #\t (cons #\\ acc)))
                  ((#\return) (cons #\r (cons #\\ acc)))
                  (else (cons c acc))))))))

;; A host shim registers a type's readable rendering via register-pr-readable-arm!,
;; or register-pr-arm! for types whose str and readable forms match (most host types:
;; inst, uuid, record, var, …). Disjoint types, checked before the base cases.
(define jolt-pr-readable-arms '())
(define (register-pr-readable-arm! pred render)
  (set! jolt-pr-readable-arms (cons (cons pred render) jolt-pr-readable-arms)))
(define (register-pr-arm! pred render)
  (register-pr-str-arm! pred render)
  (register-pr-readable-arm! pred render))
(define (jolt-pr-readable-base x)
  (cond
    ((string? x) (string-append "\"" (jolt-str-escape x) "\""))
    ;; pr renders the infinities / NaN in READABLE form (##Inf reads back), unlike
    ;; str's "Infinity"/"-Infinity"/"NaN". Applies at every nesting level.
    ((and (flonum? x) (fl= x +inf.0)) "##Inf")
    ((and (flonum? x) (fl= x -inf.0)) "##-Inf")
    ((and (flonum? x) (not (fl= x x))) "##NaN")
    ;; transients print as a cold tagged type (print-method routes this through a
    ;; multimethod; the readable fallback renders it directly).
    ;; forward refs to transients.ss (loaded later) — resolved at call time.
    ((jolt-transient? x)
     (case (jolt-transient-kind x)
       ((vec) "#<transient vector>") ((set) "#<transient set>") (else "#<transient map>")))
    ((pvec? x)
     (let ((acc '()))
       (let loop ((i (fx- (pvec-count x) 1)))
         (when (fx>=? i 0)
           (set! acc (cons (jolt-pr-readable (pvec-nth-d x i jolt-nil)) acc))
           (loop (fx- i 1))))
       (string-append "[" (jolt-str-join acc) "]")))
    ((pset? x)
     (string-append "#{" (jolt-str-join (pset-fold x (lambda (e a) (cons (jolt-pr-readable e) a)) '())) "}"))
    ((pmap? x)
     (string-append "{" (jolt-str-join
       (pmap-fold x (lambda (k v a)
                      (cons (string-append (jolt-pr-readable k) " " (jolt-pr-readable v)) a)) '())) "}"))
    ((empty-list-t? x) "()")
    ((cseq? x)
     (string-append "(" (jolt-str-join
       (let loop ((s x) (acc '()))
         (if (jolt-nil? s) (reverse acc)
             (loop (jolt-seq (seq-more s)) (cons (jolt-pr-readable (seq-first s)) acc))))) ")"))
    (else (jolt-pr-str x))))
(define (jolt-pr-readable-dispatch x)
  (let loop ((as jolt-pr-readable-arms))
    (cond ((null? as) (jolt-pr-readable-base x))
          (((caar as) x) ((cdar as) x))
          (else (loop (cdr as))))))

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

;; __write: push a string to *out* (current-output-port, so __with-out-str's
;; redirect captures it). Returns nil.
(define (jolt-write s) (display s) jolt-nil)

;; __with-out-str: run a jolt thunk with *out* rebound to a string port, return
;; the captured text.
(define (jolt-with-out-str thunk)
  (with-output-to-string (lambda () (jolt-invoke thunk))))

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
(def-var! "clojure.core" "__with-out-str" jolt-with-out-str)
(def-var! "clojure.core" "__eprint" jolt-eprint)
(def-var! "clojure.core" "__eprintf" jolt-eprintf)
