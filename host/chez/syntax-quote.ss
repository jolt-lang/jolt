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
;; seqable spliced in place. Both kinds are seqables, so __sqcat just flattens each
;; part's jolt-seq in order. A map template lowers to (__sqmap k v ...) with no
;; splicing (alternating key/value, already lowered).
;;
;; Loaded by rt.ss after collections.ss/seq.ss (jolt-list/jolt-vector/jolt-hash-map/
;; jolt-seq) and def-var!.

;; flatten the __sqcat/__sqvec/__sqset parts (each a seqable) into a Scheme list.
(define (sq-flatten parts)
  (let loop ((ps parts) (acc '()))
    (if (null? ps)
        (reverse acc)
        (loop (cdr ps)
              (let inner ((s (jolt-seq (car ps))) (a acc))
                (if (jolt-nil? s)
                    a
                    (inner (jolt-seq (seq-more s)) (cons (seq-first s) a))))))))

;; single non-spliced item -> a one-element seqable (__sqcat flattens it).
(define (jolt-sq1 x) (jolt-list x))
;; list FORM: cseq with list?=#t, so the analyzer's form-list? sees a list.
(define (jolt-sqcat . parts) (apply jolt-list (sq-flatten parts)))
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
;; is HASH-ordered (unlike a {...} literal, which keeps insertion order).
(define (jolt-sqmap . parts) (jolt-hash-map-build parts))

(def-var! "clojure.core" "__sq1"   jolt-sq1)
(def-var! "clojure.core" "__sqcat" jolt-sqcat)
(def-var! "clojure.core" "__sqvec" jolt-sqvec)
(def-var! "clojure.core" "__sqset" jolt-sqset)
(def-var! "clojure.core" "__sqmap" jolt-sqmap)
