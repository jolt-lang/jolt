;; syntax-quote form builders. A macro expander whose body was a syntax-quote
;; template (lowered by jolt.host/form-syntax-quote-lower) calls these at RUNTIME
;; to build the EXPANSION as READER forms (cseq list / pvec / pmap / tagged-set
;; pmap) so the on-Chez analyzer can re-analyze it. def-var!'d into clojure.core,
;; so the lowered body's
;; unqualified __sqcat/__sqvec/__sqmap/__sqset/__sq1 refs (which lower to var-deref
;; in prelude mode) resolve here.
;;
;; A list/vector/set template lowers to (__sqcat part ...) where each part is
;; either (__sq1 x) — a single non-spliced item — or a ~@ expr that evaluates to a
;; seqable spliced in place. A map template lowers to (__sqmap k v ...) with no
;; splicing (alternating key/value, already lowered).
;;
;; A LIST template is lazy past its first splice. The JVM reads `(a b ~@xs c) as
;; (seq (concat (list a) (list b) xs (list c))): building the form realizes the
;; head and nothing of xs, whose elements are produced only as the form is walked
;; past b. That order is observable wherever the walker interleaves evaluation
;; with walking — the top-level do, which evaluates each subform before it reads
;; the next one. A macro that expands to (do (create-type ..) ~@(map analyze ..))
;; and relies on create-type having run before the first method is analyzed
;; (SCI's deftype) got the map's whole output computed at quote time here, ahead
;; of every form in the do. So __sqcat builds the template's own items up to the
;; first splice as realized cells, and leaves the rest — that splice, and every
;; part after it — as a lazy tail forced when the walk reaches it. A template
;; with no splice is exactly the eager list it always was (one allocation per
;; item, no lazy cell), so the ordinary macro pays nothing for this.
;;
;; A vector or set template stays eager: the JVM applies vector / hash-set to the
;; concat, which realizes it all.
;;
;; Loaded by rt.ss after collections.ss/seq.ss (jolt-list/jolt-vector/jolt-hash-map/
;; jolt-seq, the lazy-src cells) and def-var!.

;; (__sq1 x): one template item, marked so the builders can tell it from a ~@
;; splice without touching the splice (any seq test on a splice would force it).
;; The marker never leaves this file: the lowering only ever hands an __sq1 form
;; to __sqcat/__sqvec/__sqset as a direct argument.
(define sq1-tag (list (quote sq1)))   ; a fresh pair, compared by eq?
(define (jolt-sq1 x) (cons sq1-tag x))
(define (sq1? p) (and (pair? p) (eq? (car p) sq1-tag)))

;; flatten the __sqvec/__sqset parts (a marked item or a spliced seqable) into a
;; Scheme list — eager, as those templates are.
(define (sq-flatten parts)
  (let loop ((ps parts) (acc '()))
    (if (null? ps)
        (reverse acc)
        (loop (cdr ps)
              (if (sq1? (car ps))
                  (cons (cdr (car ps)) acc)
                  (let inner ((s (jolt-seq (car ps))) (a acc))
                    (if (jolt-nil? s)
                        a
                        (inner (jolt-seq (seq-more s)) (cons (seq-first s) a)))))))))

;; The parts from the first splice on, each as the seqable lazy-concat-seq walks:
;; a marked item is a one-element list, a splice is itself. A jolt list of them,
;; not a Scheme list: the lazy tail's descriptor holds it, and a descriptor's
;; arguments are image data (seq.ss lazy-src).
(define (sq-tail-colls parts)
  (apply jolt-list (map (lambda (p) (if (sq1? p) (jolt-list (cdr p)) p)) parts)))
;; the lazy tail: forced when the walk moves past the last item before the first
;; splice. Registered so a form holding one still writes to an image, like every
;; other core-produced lazy cell.
(define lz-sqcat-tail
  (register-lazy-src! 'sqcat-tail
    (lambda (colls _b) (lazy-concat-seq colls))))
;; list FORM. The realized prefix is list-flavored (cseq-list), as an eager
;; template's cells are; the last prefix cell carries the lazy tail. With the
;; splice in head position there is no prefix, and the result is the concat's own
;; seq — nil when every part is empty, as (seq (concat ..)) is.
(define (jolt-sqcat . parts)
  (let loop ((ps parts) (acc '()))
    (cond
      ((null? ps) (apply jolt-list (reverse acc)))
      ((sq1? (car ps)) (loop (cdr ps) (cons (cdr (car ps)) acc)))
      ((null? acc) (jolt-seq (lazy-concat-seq (sq-tail-colls ps))))
      (else
       (let ((tail (make-lazy-src lz-sqcat-tail (sq-tail-colls ps) #f)))
         (let build ((items acc) (rest tail) (first? #t))
           ;; acc is reversed: its head is the item just before the splice, the
           ;; one cell whose tail is the descriptor rather than the next cell.
           (if (null? items)
               rest
               (build (cdr items)
                      (if first?
                          (cseq-lazy/k (car items) rest sk-list)
                          (cseq-list (car items) rest))
                      #f))))))))
;; vector FORM: pvec.
(define (jolt-sqvec . parts) (apply jolt-vector (sq-flatten parts)))
;; set: a REAL set value (pset). A syntax-quote builds VALUES, and the cseq/pvec/
;; pmap that __sqcat/__sqvec/__sqmap build double as their own form rep — but a set
;; value (pset) differs from the reader's set FORM ({:jolt/type :jolt/set :value
;; <pvec>}), so building the tagged form here would make a runtime `#{~@xs} a map,
;; not a set. Build the value; the analyzer's form-set?
;; (host-contract.ss) additionally recognizes a pset, so a macro template's #{...}
;; expansion still re-analyzes as a set literal.
(define (jolt-sqset . parts) (apply jolt-hash-set (sq-flatten parts)))
;; map FORM: a plain pmap (the analyzer's form-map? = pmap with no :jolt/type).
;; Clojure's syntaxQuote builds the map via `apply hash-map`, so a `{...} template
;; is a PersistentHashMap in hash order at any size (unlike a {...} literal, an
;; array map in insertion order up to 8 entries) -- (keys `{:z 1 :a 2 :m 3 :b 4})
;; is (:m :z :b :a) there, and the metadata `^:foo (a) carries is ordered the same
;; way. The parts are evaluated, in source order, as this call's arguments; an
;; expansion that splices the map back into code is then compiled in the map's
;; own order, as the reference's MapExpr compiles a hash map's.
(define (jolt-sqmap . parts) (jolt-hash-map-build parts))

(def-var! "clojure.core" "__sq1"   jolt-sq1)
(def-var! "clojure.core" "__sqcat" jolt-sqcat)
(def-var! "clojure.core" "__sqvec" jolt-sqvec)
(def-var! "clojure.core" "__sqset" jolt-sqset)
(def-var! "clojure.core" "__sqmap" jolt-sqmap)
