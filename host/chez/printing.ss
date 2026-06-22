;; readable printer + output seams (jolt-cf1q.3 Phase 2 inc B) — the __pr-str1 /
;; __write / __with-out-str host seams the overlay's pr-str/pr/prn/print/println/
;; *-str family is built on (jolt-core/clojure/core/20-coll.clj). They resolved to
;; jolt-nil, so the whole print family hit the apply-jolt-nil crash bucket.
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

(define (jolt-pr-readable x)
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

;; __pr-str1: render ONE value readably (the overlay's pr-str joins these).
(define (jolt-pr-str1 x) (jolt-pr-readable x))

;; __write: push a string to *out* (current-output-port, so __with-out-str's
;; redirect captures it). Returns nil.
(define (jolt-write s) (display s) jolt-nil)

;; __with-out-str: run a jolt thunk with *out* rebound to a string port, return
;; the captured text.
(define (jolt-with-out-str thunk)
  (with-output-to-string (lambda () (jolt-invoke thunk))))

;; __eprint / __eprintf: stderr seams.
(define (jolt-eprint s) (display s (current-error-port)) jolt-nil)
(define (jolt-eprintf fmt . args)
  (apply fprintf (current-error-port) fmt args) jolt-nil)

(def-var! "clojure.core" "__pr-str1" jolt-pr-str1)
(def-var! "clojure.core" "__write" jolt-write)
(def-var! "clojure.core" "__with-out-str" jolt-with-out-str)
(def-var! "clojure.core" "__eprint" jolt-eprint)
(def-var! "clojure.core" "__eprintf" jolt-eprintf)
