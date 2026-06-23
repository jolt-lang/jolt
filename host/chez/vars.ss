;; vars as first-class objects — (var x) / #'x.
;;
;; The emitter lowers :the-var to (jolt-var ns name) — the rt.ss var-cell, which
;; is now also a Clojure VAR value. var? / var-get / deref-of-var / var-as-IFn /
;; var equality / pr-str(#'ns/name) operate on it. bound? is overridden natively
;; in post-prelude.ss (the overlay reads (get v :root), nil on a record).
;;
;; Dynamic binding (binding / with-bindings* / var-set / thread-bound? /
;; with-redefs) lives in dyn-binding.ss, which chains the var-read paths set up
;; here.
;;
;; Loaded LAST (after natives-xform.ss): chains jolt-deref (atom/volatile arms)
;; and the printers.

(define (jolt-var-pred? x) (var-cell? x))

;; the var's current root; unbound is an error (Clojure throws on an unbound var).
(define (jolt-var-get v)
  (if (var-cell? v)
      (let ((r (var-cell-root v)))
        (if (eq? r jolt-unbound) (error #f "Unbound var" v) r))
      (error #f "var-get: not a var" v)))

;; deref of a var -> its root.
(define %v-deref jolt-deref)
(set! jolt-deref (lambda (x) (if (var-cell? x) (jolt-var-get x) (%v-deref x))))

;; a var is an IFn — invoking it invokes its root value ((var f) args -> (f args)).
(define %v-invoke jolt-invoke)
(set! jolt-invoke (lambda (f . args)
  (if (var-cell? f) (apply jolt-invoke (var-cell-root f) args) (apply %v-invoke f args))))

;; two var cells are = iff same ns/name (Clojure var identity).
(define %v-=2 jolt=2)
(set! jolt=2 (lambda (a b)
  (cond ((var-cell? a) (and (var-cell? b)
                            (string=? (var-cell-ns a) (var-cell-ns b))
                            (string=? (var-cell-name a) (var-cell-name b))))
        ((var-cell? b) #f)
        (else (%v-=2 a b)))))

;; pr-str / str of a var -> #'ns/name.
(define (var->str v) (string-append "#'" (var-cell-ns v) "/" (var-cell-name v)))
(define %v-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (var-cell? x) (var->str x) (%v-pr-str x))))
(define %v-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (var-cell? x) (var->str x) (%v-pr-readable x))))
(define %v-str-render-one jolt-str-render-one)
(set! jolt-str-render-one (lambda (x) (if (var-cell? x) (var->str x) (%v-str-render-one x))))

;; bound? — native (the overlay's (get v :root) is nil on a var-cell record).
(define (jolt-var-bound-one? v) (and (var-cell? v) (not (eq? (var-cell-root v) jolt-unbound))))
(define (jolt-bound? . vars) (if (for-all jolt-var-bound-one? vars) #t #f))

(def-var! "clojure.core" "var?" jolt-var-pred?)
(def-var! "clojure.core" "var-get" jolt-var-get)
(def-var! "clojure.core" "deref" jolt-deref)
