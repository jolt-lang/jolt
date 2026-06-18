;; Phase 1 (jolt-cf1q.2) — the minimal Chez RT the emitted Scheme rests on.
;;
;; Sits above the value model (values.ss) and below an emitted program. Adds the
;; two things the back end's output references that aren't in the value layer:
;;   1. the var-cell late-binding registry (Clojure vars — a global root that a
;;      reference reads at call time, so redefinition / mutual recursion work);
;;   2. the rt primitive shims the emitter names (jolt-inc/dec/not) and jolt's
;;      number printing (all jolt numbers model Clojure doubles; integer-valued
;;      print without a trailing ".0", matching the Janet host).
;;
;; Emitted programs do `(load "host/chez/rt.ss")`; this loads values.ss in turn.

(load "host/chez/values.ss")
(load "host/chez/collections.ss")
(load "host/chez/seq.ss")

;; --- rt arithmetic / logic shims (named in emit.janet's native-ops) ----------
(define (jolt-inc x) (+ x 1))
(define (jolt-dec x) (- x 1))
;; jolt `not`: only nil and false are falsey.
(define (jolt-not x) (if (jolt-truthy? x) #f #t))

;; --- exceptions (jolt-vcsl) --------------------------------------------------
;; throw raises the jolt value RAW (no envelope), like the Janet compiled back
;; end; catch (emitted as `guard`) binds it directly. Chez `raise` accepts any
;; object, so a thrown number/map/ex-info all work; uncaught -> non-zero exit.
(define (jolt-throw v) (raise v))
;; ex-info builds the tagged map {:jolt/type :jolt/ex-info :message :data :cause}
;; — a real jolt-hash-map, so the ex-data/ex-message/ex-cause tier fns read it
;; via jolt-get for free. Arity 2 (msg data) or 3 (msg data cause).
(define jolt-kw-ex-type (keyword "jolt" "type"))
(define jolt-kw-ex-info (keyword "jolt" "ex-info"))
(define jolt-kw-message (keyword #f "message"))
(define jolt-kw-data (keyword #f "data"))
(define jolt-kw-cause (keyword #f "cause"))
(define (jolt-ex-info msg data . more)
  (jolt-hash-map jolt-kw-ex-type jolt-kw-ex-info
                 jolt-kw-message msg
                 jolt-kw-data data
                 jolt-kw-cause (if (null? more) jolt-nil (car more))))

;; --- host interop (jolt-0kf5) ------------------------------------------------
;; (.method target arg*) lowers to (jolt-host-call "method" target arg*). JVM
;; interop has no general Chez analog, but the few methods jolt-core's io tier
;; calls map onto Chez equivalents: a writer's .write is a port display; a File's
;; .isDirectory / .listFiles work over a path string (Chez has no File type, so
;; file-seq's File branch is unreachable here — these keep the forms honest). An
;; unsupported method raises rather than silently returning nil.
(define (jolt-host-call method target . args)
  (cond
    ((string=? method "write") (display (car args) target) jolt-nil)
    ((string=? method "isDirectory") (if (file-directory? target) #t #f))
    ((string=? method "listFiles") (list->cseq (directory-list target)))
    (else (error 'jolt-host-call (string-append "unsupported host method: ." method)))))

;; --- var cells: late-bound global roots (Clojure vars) -----------------------
;; A var is a mutable cell keyed by "ns/name". A `:def` sets the root; a `:var`
;; reference reads it at use time (late binding), so a forward/mutually-recursive
;; reference resolves to whatever the cell holds when the call actually runs.
;; declare / (def name) with no init reserves a cell holding this placeholder
;; until the real def overwrites it (a forward reference resolves to the cell, and
;; correct code never reads it before the binding def runs).
(define jolt-unbound (string->symbol "#<jolt-unbound>"))
(define-record-type var-cell (fields ns name (mutable root)) (nongenerative var-cell-v1))
(define var-table (make-hashtable string-hash string=?))
(define (jolt-var ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name jolt-nil)))
          (hashtable-set! var-table k c)
          c))))
(define (var-deref ns name) (var-cell-root (jolt-var ns name)))
(define (def-var! ns name v) (var-cell-root-set! (jolt-var ns name) v) v)
;; declare / (def name) with no init: reserve the cell ONLY if absent. An
;; existing root is left intact — Clojure's (def x) with no init does not clobber
;; a prior binding (do (def x 7) (def x) x) => 7.
(define (declare-var! ns name)
  (let ((k (string-append ns "/" name)))
    (unless (hashtable-ref var-table k #f)
      (hashtable-set! var-table k (make-var-cell ns name jolt-unbound)))))

;; regex (jolt-i0s3): defines regex-t + the re-* fns (def-var!'d into
;; clojure.core), so it loads after def-var! and before the printer below (which
;; renders a regex-t as #"source").
(load "host/chez/regex.ss")

;; atoms (jolt-9ziu): host-coupled mutable cells; def-var!'d into clojure.core
;; (atom/deref/swap!/reset! + the compare/vals kernel). Loads after def-var! and
;; jolt-invoke (seq.ss) / jolt= (values.ss) / jolt-vector (collections.ss).
(load "host/chez/atoms.ss")

;; type predicates + simple accessors (jolt-9ziu): seed natives the overlay
;; assumes (map?/vector?/nil?/number?/.../name/namespace), def-var!'d into
;; clojure.core. Loads after the value-model record predicates they wrap.
(load "host/chez/predicates.ss")

;; --- jolt number printing ----------------------------------------------------
;; jolt models every number as a Clojure double: integer-valued values print
;; without a ".0" (the Janet host prints (* 1.0 5) as "5", (/ 1 2) as "0.5").
(define (jolt-num->string x)
  (cond
    ;; the -e / element printer renders the infinities and NaN as inf/-inf/nan
    ;; (Chez's number->string gives +inf.0 etc.); the str/print family uses the
    ;; long "Infinity"/"NaN" forms (see jolt-str-render-one in converters.ss).
    ((and (flonum? x) (fl= x +inf.0)) "inf")
    ((and (flonum? x) (fl= x -inf.0)) "-inf")
    ((and (flonum? x) (not (fl= x x))) "nan")
    ((and (rational? x) (integer? x)) (number->string (exact x)))
    (else (number->string x))))

;; Program-final-value printer. jolt's `-e` prints in str-style: strings raw (no
;; quotes), chars as `\c`/`\newline`, collections recursively. NOTE: maps/sets
;; render in HAMT-iteration order, which does NOT match the Janet host's order —
;; so unordered values are compared via `=` (true/false), not printed form.
;; The full canonical printer is Phase 2.
(define (jolt-str-join strs)
  (cond ((null? strs) "") ((null? (cdr strs)) (car strs))
        (else (string-append (car strs) " " (jolt-str-join (cdr strs))))))
(define (jolt-char->string c)
  (string-append "\\" (case c ((#\newline) "newline") ((#\space) "space") ((#\tab) "tab")
                        ((#\return) "return") (else (string c)))))
;; Program-final printer: jolt's `-e` is str-style at the top level, where a
;; bare nil renders as the empty string (a nil ELEMENT inside a collection still
;; prints "nil", which jolt-pr-str handles).
(define (jolt-final-str x) (if (jolt-nil? x) "" (jolt-pr-str x)))
(define (jolt-pr-str x)
  (cond
    ((jolt-nil? x) "nil")
    ((eq? x #t) "true")
    ((eq? x #f) "false")
    ((number? x) (jolt-num->string x))
    ((string? x) x)
    ((char? x) (jolt-char->string x))
    ((keyword? x) (let ((ns (keyword-t-ns x)))
                    (if ns (string-append ":" ns "/" (keyword-t-name x)) (string-append ":" (keyword-t-name x)))))
    ((jolt-symbol? x) (let ((ns (symbol-t-ns x)))
                        (if (or (jolt-nil? ns) (not ns) (eq? ns '())) (symbol-t-name x)
                            (string-append ns "/" (symbol-t-name x)))))
    ((regex-t? x) (string-append "#\"" (regex-t-source x) "\""))
    ((pvec? x) (let ((acc '())) (let loop ((i (fx- (pvec-count x) 1)))
                 (when (fx>=? i 0) (set! acc (cons (jolt-pr-str (pvec-nth-d x i jolt-nil)) acc)) (loop (fx- i 1))))
                 (string-append "[" (jolt-str-join acc) "]")))
    ((pset? x) (string-append "#{" (jolt-str-join (pset-fold x (lambda (e a) (cons (jolt-pr-str e) a)) '())) "}"))
    ((pmap? x) (string-append "{" (jolt-str-join
                 (pmap-fold x (lambda (k v a) (cons (string-append (jolt-pr-str k) " " (jolt-pr-str v)) a)) '())) "}"))
    ;; lists / cons / lazy seqs all print as (...) — forces a finite seq.
    ((empty-list-t? x) "()")
    ((cseq? x) (string-append "(" (jolt-str-join
                 (let loop ((s x) (acc '()))
                   (if (jolt-nil? s) (reverse acc)
                       (loop (jolt-seq (seq-more s)) (cons (jolt-pr-str (seq-first s)) acc))))) ")"))
    (else (format "~a" x))))

;; converters + string ops (jolt-t6cr): str/subs/vec/keyword/symbol/compare/int/
;; double/gensym — host-coupled seed natives def-var!'d into clojure.core. Loaded
;; LAST because `str` reuses jolt-pr-str (defined just above).
(load "host/chez/converters.ss")

;; transients (jolt-kl2l): copy-on-write transient collections + persistent disj;
;; extends get/count/contains? to see through a transient. After collections.ss
;; (the persistent ops it delegates to).
(load "host/chez/transients.ss")
