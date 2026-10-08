;; converters + string ops — host-coupled natives def-var!'d into clojure.core,
;; resolved in prelude mode. Loaded last (after jolt-pr-str), since `str` reuses
;; the printer. int/long truncate toward zero to an exact integer; compare returns
;; an exact -1/0/1; double yields a flonum.

;; str rendering for the value types not handled by the fast arms below. A host
;; shim loaded later (records, host-table, inst-time, …) registers an arm with
;; register-str-render! instead of set!-wrapping jolt-str-render-one — the arms
;; are type-disjoint, so the full behavior is the base arms here plus the
;; registry, gathered in one place rather than scattered across a set! chain.
;; Newest registration is checked first (matches the old outermost-wins order).
(define str-render-registry '())   ; list of (pred . render), checked front-to-back
(define (register-str-render! pred render)
  (pr-arm-reject-fast-type! 'register-str-render! pred)
  (set! str-render-registry (cons (cons pred render) str-render-registry)))

;; str: nil -> "", string raw, char bare (not \c), regex -> raw source, a
;; registered host type via its arm, else the printer (which renders collections
;; with readable elements).
(define (jolt-str-render-one v)
  (cond
    ((jolt-nil? v) "")
    ((string? v) v)
    ((char? v) (string v))
    ((regex-t? v) (regex-t-source v))
    ;; str/print render the infinities and NaN long-form (Clojure .toString),
    ;; unlike the -e printer's inf/-inf/nan.
    ((and (flonum? v) (fl= v +inf.0)) "Infinity")
    ((and (flonum? v) (fl= v -inf.0)) "-Infinity")
    ((and (flonum? v) (not (fl= v v))) "NaN")
    ;; a symbol stringifies to its name (JVM Symbol.toString returns the interned
    ;; name), so (str sym) of a no-ns symbol is the SAME string object the symbol
    ;; holds — code that compares those by identity (core.logic's non-unique lvar
    ;; equality) depends on it.
    ((symbol-t? v)
     (let ((ns (symbol-t-ns v)))
       (if (or (not ns) (jolt-nil? ns))
           (symbol-t-name v)
           (string-append ns "/" (symbol-t-name v)))))
    ;; a keyword and a fixnum are the next most common str operands after
    ;; strings (a keyword's name in a key, a count in a message), and both
    ;; used to reach the full printer below: 78 ns for (str s :k). Rendered
    ;; here exactly as the printer does — the keyword form is the one
    ;; keyword-direct-emit (backend) spells for .toString.
    ((keyword-t? v)
     (let ((ns (keyword-t-ns v)))
       (if ns
           (string-append ":" ns "/" (keyword-t-name v))
           (string-append ":" (keyword-t-name v)))))
    ((fixnum? v) (jolt-fixnum->string v))
    ;; numbers and booleans reach the printer without an arm ever claiming them
    ;; (pr-fast-type?, rt.ss); the cases above already cover the rest of that set.
    ((pr-fast-type? v) (jolt-pr-str v))
    (else
     (let loop ((rs str-render-registry))
       (cond
         ((null? rs) (jolt-pr-str v))
         (((caar rs) v) ((cdar rs) v))
         (else (loop (cdr rs))))))))
;; print/println render via the SAME readable renderer as pr, with string/char
;; quoting off: the JVM's print is (binding [*print-readably* nil] (pr ...)).
;; jolt-pr-readable already consults *print-readably* for strings (printing.ss)
;; and chars (jolt-char->string) at every nesting depth, so overriding it here
;; makes (print ["s" \a 2M]) render [s a 2M] while numbers, regexes, uuids and
;; the infinities keep their readable form — one renderer, not a per-type print
;; registry. nil renders "nil" via the readable base. The override is a virtual
;; register (rt.ss), not a thread binding: two ~1ns vreg writes per value instead
;; of a pmap alloc + fold + two thread-parameter writes. Saved/restored so
;; nesting composes, inside dynamic-wind so an un-restored override cannot leak
;; non-readable rendering into every later pr in this thread if the renderer
;; throws.
(define (jolt-print-one v)
  ;; masked at its far edge (locks.ss), so an interrupt cannot leave the thread
  ;; printing non-readably. The restore is a store, so the plain after-thunk.
  (let ((prev (virtual-register jolt-vreg-print-readably)) (lv (jolt-locks-held)))
    (jolt-locks-enter!)
    (dynamic-wind
      (lambda () (set-virtual-register! jolt-vreg-print-readably #f))
      (jolt-masked-body (lambda () (jolt-pr-readable v)))
      (jolt-masked-after lv (set-virtual-register! jolt-vreg-print-readably prev)))))
(def-var! "clojure.core" "__print1" jolt-print-one)

;; str: a top-level string/scalar renders as jolt-str-render-one (raw string,
;; "Infinity"…), but a COLLECTION renders as its readable form — nested strings
;; are QUOTED ((str ["x"]) => "[\"x\"]"), matching the JVM (a collection's
;; toString is readable). jolt-pr-readable resolves at call time.
;; A host type whose own toString must win over the rendering below — set by
;; records.ss for a deftype/record that declares one. Returns the string, or #f to
;; fall through. Kept as one hook rather than a registry arm because the arms are
;; only reached for values no branch here claims, and a record is claimed.
(define str-tostring-hook #f)
(define (set-str-tostring-hook! f) (set! str-tostring-hook f))
(define (jolt-str-one v)
  (let ((own (and str-tostring-hook (str-tostring-hook v))))
    (cond
      ((string? own) own)
      ((or (pvec? v) (pmap? v) (pset? v) (cseq? v) (empty-list-t? v) (jolt-lazyseq? v))
       (jolt-pr-readable v))
      (else (jolt-str-render-one v)))))
;; A string renders as itself. jolt-str-one asks the toString hook first, but
;; that hook only ever answers for a record (protocols.ss), so a string can skip
;; it and the type cond behind it: strings are most of what str is handed.
(define (jolt-str-piece v) (if (string? v) v (jolt-str-one v)))
;; Fixed entries for two and three arguments — the arities library code
;; actually writes — build the result in one string-append with no rest list,
;; no accumulator and no reverse. The variadic entry keeps its shape. Measured
;; 56 ns against string-append's 1 ns for (str s "!") before this.
(define jolt-str
  (case-lambda
    (() "")
    ;; single arg returns its rendering directly (no string-append copy), so
    ;; (str sym) hands back the symbol's own name string — JVM (str x) is
    ;; x.toString(), and core.logic's non-unique lvar equality compares those by
    ;; identity.
    ((a) (jolt-str-one a))
    ((a b) (string-append (jolt-str-piece a) (jolt-str-piece b)))
    ((a b c) (string-append (jolt-str-piece a) (jolt-str-piece b) (jolt-str-piece c)))
    ((a b c . rest)
     (let loop ((xs rest) (acc (list (jolt-str-piece c) (jolt-str-piece b) (jolt-str-piece a))))
       (if (null? xs)
           (apply string-append (reverse acc))
           (loop (cdr xs) (cons (jolt-str-piece (car xs)) acc)))))))

;; jolt indices are flonums; substring etc. need exact ints. A fixnum is already
;; one — this sits on the proven-string .charAt/.substring path, so it must not
;; call anything for the common case.
(define (jolt->idx n) (if (fixnum? n) n (exact (truncate (jolt-need-num n)))))
;; String.charAt and every CharSequence charAt over a string's characters
;; (StringBuilder reads its buffer through the same check): an index outside
;; [0, length) is the JVM's StringIndexOutOfBoundsException with its message,
;; not Chez's string-ref error.
(define (char-index-oob i n)
  (throw-jvm 'StringIndexOutOfBoundsException
             (string-append "Index " (number->string i) " out of bounds for length "
                            (number->string n))))
(define (jolt-char-at s i)
  (let ((i (jolt->idx i)))
    (if (and (fixnum? i) (fx>=? i 0) (fx<? i (string-length s)))
        (string-ref s i)
        (char-index-oob i (string-length s)))))

;; Chez's `substring` walks the span a character at a time; allocating the result
;; and asking for the block move is ~3.7x faster for the spans a parser, a reader
;; or a splitter cuts. Measured on Chez 10.4.1 / x86_64, 2048 chars out of 4300:
;; `substring` 4.27us, `(make-string n)` + `string-copy!` 1.17us — and the
;; make-string alone is 1.29us of that, so the copy itself is free and what the
;; primitive spends is the per-character loop. Out-of-range falls through to
;; `substring` so the range error stays the one host-faults.ss classifies. The
;; copy goes through the adapter's sa-string-copy-range! rather than a raw
;; string-copy!: this file is shared with the Gambit host, whose R7RS
;; string-copy! takes the same five arguments in the opposite direction.
(define (jolt-substr s start end)
  (if (and (fixnum? start) (fixnum? end)
           (fx>=? start 0) (fx<=? start end) (fx<=? end (string-length s)))
      (let ((d (make-string (fx- end start))))
        (sa-string-copy-range! d 0 s start end)
        d)
      (substring s start end)))

;; case-lambda, not `. end`: a rest-arg list allocated on every call is most of
;; what a one-character (subs txt i (inc i)) costs, and a parser does that per
;; input position.
(define jolt-subs
  (case-lambda
    ((s start)
     (let ((s (jolt-need-string s)))
       (jolt-substr s (jolt->idx start) (string-length s))))
    ((s start end)
     (let ((s (jolt-need-string s)))
       (jolt-substr s (jolt->idx start) (jolt->idx end))))))

;; vec: a pvec from any seqable. A vector comes back as itself minus its
;; metadata — clojure.core/vec's (with-meta coll nil) — so it is a copy only when
;; it carried some. A map entry is the exception: MapEntry is not IObj, so vec
;; takes LazilyPersistentVector.create's path there and answers a plain
;; PersistentVector of the two slots. The entry is a pvec of kind #t here, and
;; used to come back as itself — still a MapEntry to `class` and `instance?`.
(define (jolt-vec coll)
  (cond
    ((jolt-nil? coll) (jolt-vector))
    ((pvec? coll)
     (cond ((eq? (pvec-ent coll) #t) (pvec-with-ent coll #f))
           ((eq? (pvec-meta coll) jolt-nil) coll)
           (else (coll-with-meta coll jolt-nil))))
    ((string? coll) (apply jolt-vector (string->list coll)))
    ;; a source that drives its own reduce (IReduce/IReduceInit deftype or
    ;; reify) builds the vector by reduction, like LazilyPersistentVector.
    ((iface-method coll "reduce" 3) (jolt-into (jolt-vector) coll))
    (else (apply jolt-vector (seq->list coll)))))

(define (jolt-keyword . args)
  (cond
    ((= (length args) 1)
     (let ((a (car args)))
       (cond
         ((jolt-nil? a) jolt-nil)
         ((keyword? a) a)
         ;; a 1-arg string splits on the FIRST "/" into ns/name:
         ;; (keyword "x/y") => :x/y with ns "x" — destructure's {:keys [x/y]} builds
         ;; the key this way, so without the split the namespaced key never matches.
         ((string? a)
          (let ((si (let loop ((i 0))
                      (cond ((>= i (string-length a)) #f)
                            ((char=? (string-ref a i) #\/) i)
                            (else (loop (+ i 1)))))))
            (if (and si (> si 0) (< si (- (string-length a) 1)))
                (keyword (substring a 0 si) (substring a (+ si 1) (string-length a)))
                (keyword #f a))))
         ((jolt-symbol? a)
          (let ((ns (symbol-t-ns a)))
            (keyword (if (or (jolt-nil? ns) (not ns) (eq? ns '())) #f ns) (symbol-t-name a))))
         (else jolt-nil))))
    ((= (length args) 2)
     (keyword (let ((ns (car args))) (if (jolt-nil? ns) #f ns))
              (jolt-need-string (cadr args))))
    (else (throw-jvm (quote ArityException) "Wrong number of args passed to: keyword"))))

(define (jolt-symbol-new . args)
  (cond
    ((= (length args) 1)
     (let ((a (car args)))
       (cond
         ((jolt-symbol? a) a)
         ;; (symbol "ns/name") splits the namespace at the FIRST "/" (JVM
         ;; Symbol.intern), so (namespace (symbol "foo/bar/baz")) => "foo" with
         ;; name "bar/baz". A lone "/" or a leading slash has no namespace. The
         ;; no-ns sentinel is #f — matches emit's quoted-symbol lowering
         ;; (jolt-symbol #f "x"), so (= 'x (symbol "x")) holds (jolt= compares
         ;; ns with strict equal?).
         ((string? a)
          (let ((slen (string-length a)))
            (if (string=? a "/")
                (jolt-symbol #f "/")
                (let loop ((i 1))
                  (cond ((>= i slen) (jolt-symbol #f a))
                        ((char=? (string-ref a i) #\/)
                         (jolt-symbol (substring a 0 i) (substring a (+ i 1) slen)))
                        (else (loop (+ i 1))))))))
         ((keyword? a) (jolt-symbol (keyword-t-ns a) (keyword-t-name a)))
         ;; (symbol a-var) -> the var's qualified symbol (clojure.spec.alpha/->sym).
         ((var-cell? a) (jolt-symbol (var-cell-ns a) (var-cell-name a)))
         (else (throw-jvm (quote IllegalArgumentException) (string-append "no conversion to symbol: " (jolt-final-str a)))))))
    ;; (symbol ns name): a nil namespace is the no-ns sentinel #f (NOT jolt-nil),
    ;; so (symbol nil "x") equals (symbol "x") and the reader literal 'x — jolt=
    ;; compares ns with strict equal?, so a jolt-nil ns would differ from #f.
    ((= (length args) 2)
     (let ((ns (car args)) (nm (cadr args)))
       (unless (or (jolt-nil? ns) (string? ns))
         (throw-jvm (quote ClassCastException)
                    (string-append (jolt-final-str ns) " cannot be cast to java.lang.String")))
       (unless (string? nm)
         (throw-jvm (quote ClassCastException)
                    (string-append (jolt-final-str nm) " cannot be cast to java.lang.String")))
       (jolt-symbol (if (jolt-nil? ns) #f ns) nm)))
    (else (throw-jvm (quote ArityException) "Wrong number of args passed to: symbol"))))

;; gensym: per-process counter. The bump and the read are ONE step — a bare
;; (set! c (+ c 1)) followed by a read of c is a read-modify-write, and two
;; threads that interleave in it draw the same number, which is the one thing
;; gensym promises they cannot. Macros expand on whatever thread is compiling and
;; namespaces load in parallel, so this is reachable, and it is the same defect
;; the analyzer's gen-name has an atomic swap! for.
(define jolt-gensym-counter 0)
(define jolt-gensym-mutex (make-mutex))
(define (jolt-gensym . prefix)
  (let ((p (if (null? prefix) "G__" (car prefix)))
        (n (jolt-with-mutex jolt-gensym-mutex
             (set! jolt-gensym-counter (+ jolt-gensym-counter 1))
             jolt-gensym-counter)))
    (jolt-symbol #f
                 (string-append (if (string? p) p (jolt-str-render-one p))
                                (number->string n)))))

;; a numeric type outside Chez's tower converts through this hook (bigdec).
(define (jolt-double-slow x) (jolt-num-cast-throw x))
(define (jolt-double x)
  (cond ((char? x) (exact->inexact (char->integer x)))
        ((number? x) (exact->inexact x))
        (else (jolt-double-slow x))))

;; compare: 3-way, returns an EXACT integer (= JVM compare -> int).
(define (jolt-cmp3 x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))
(define (jolt-strcmp a b) (cond ((string<? a b) -1) ((string>? a b) 1) (else 0)))
(define (jolt-sym-ns-string s)
  (let ((n (symbol-t-ns s))) (if (or (jolt-nil? n) (not n) (eq? n '())) "" n)))
;; compare returns an EXACT integer -1/0/1 (= JVM compare -> int).
;; A host shim registers a type's ordering via register-compare-arm! (cf.
;; register-eq-arm!): an arm is (pred . handler) on (a b); the arm applies when
;; pred holds (typically either arg is the type) and handler returns -1/0/1.
;; Arms are the last resort before the "cannot compare" error, so a library can
;; make its own values Comparable without editing this file.
;; Only the SAME-TYPE pairs are subject to the invariant. jolt-compare's nil
;; clauses are single-sided — (compare nil x) is -1 for every x — so the fast
;; path answering them is correct for any type, and the usual
;; either-arg-is-my-type predicate stays legal even though it matches them. Same
;; distinction register-eq-arm! makes for its identity clause.
(define (compare-fast-probes)
  (list (cons 0 1) (cons "a" "b") (cons (keyword #f "a") (keyword #f "b"))
        (cons (jolt-symbol #f "a") (jolt-symbol #f "b")) (cons #t #f)
        (cons #\a #\b) (cons (probe-pvec) (probe-pvec))))
(define (compare-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who
                           (lambda (probe) (pred (car probe) (cdr probe)))
                           (compare-fast-probes)
                           "the jolt-compare same-type fast path"))
(define jolt-compare-arms '())
(define (register-compare-arm! pred handler)
  (compare-arm-reject-fast-type! 'register-compare-arm! pred)
  (set! jolt-compare-arms (cons (cons pred handler) jolt-compare-arms)))
(define (jolt-compare a b)
  (cond
    ;; Util.compare answers 0 for two identical references FIRST, before it asks
    ;; for Comparable at all — so a value with no ordering still compares to
    ;; itself, and a collection holding one sorts. Ordering is only ever
    ;; consulted for two DISTINCT values, and comparing a value to itself under
    ;; any ordering is 0 anyway, so this decides nothing the arms below would
    ;; have decided differently. Numbers are not excluded the way jolt=2's
    ;; identity clause excludes them: 0 is the right answer for a number
    ;; compared to itself, NaN included (Double.compareTo says NaN equals
    ;; itself), so interning cannot make it wrong.
    ((eq? a b) 0)
    ((and (jolt-nil? a) (jolt-nil? b)) 0)
    ((jolt-nil? a) -1)
    ((jolt-nil? b) 1)
    ((and (number? a) (number? b)) (jolt-cmp3 a b))
    ((and (string? a) (string? b)) (jolt-strcmp a b))
    ;; keywords order like symbols: a nil namespace sorts before any namespace,
    ;; then by namespace, then by name (Keyword.compareTo -> Symbol.compareTo)
    ((and (keyword? a) (keyword? b))
     (let ((r (jolt-strcmp (or (keyword-t-ns a) "") (or (keyword-t-ns b) ""))))
       (if (= r 0) (jolt-strcmp (keyword-t-name a) (keyword-t-name b)) r)))
    ((and (jolt-symbol? a) (jolt-symbol? b))
     (let ((r (jolt-strcmp (jolt-sym-ns-string a) (jolt-sym-ns-string b))))
       (if (= r 0) (jolt-strcmp (symbol-t-name a) (symbol-t-name b)) r)))
    ((and (boolean? a) (boolean? b)) (cond ((eq? a b) 0) ((eq? a #f) -1) (else 1)))
    ((and (char? a) (char? b)) (jolt-cmp3 (char->integer a) (char->integer b)))
    ((and (pvec? a) (pvec? b))
     (let ((la (pvec-count a)) (lb (pvec-count b)))
       (if (not (= la lb))
           (jolt-cmp3 la lb)
           (let loop ((i 0))
             (if (>= i la)
                 0
                 (let ((r (jolt-compare (pvec-nth-d a i jolt-nil) (pvec-nth-d b i jolt-nil))))
                   (if (= r 0) (loop (+ i 1)) r)))))))
    (else (let loop ((as jolt-compare-arms))
            (cond ((null? as) (throw-jvm (quote ClassCastException) (string-append (jolt-final-str a) " cannot be compared to " (jolt-final-str b))))
                  (((caar as) a b) ((cdar as) a b))
                  (else (loop (cdr as))))))))

(def-var! "clojure.core" "str" jolt-str)
(def-var! "clojure.core" "subs" jolt-subs)
(def-var! "clojure.core" "vec" jolt-vec)
(def-var! "clojure.core" "keyword" jolt-keyword)
(def-var! "clojure.core" "symbol" jolt-symbol-new)
(def-var! "clojure.core" "gensym" jolt-gensym)
;; --- checked narrow casts (RT.byteCast/shortCast/intCast/longCast/charCast) --
;; One helper carries the JVM ranges: truncate toward zero, then range-check.
;; NaN casts to 0 (Java (long)NaN); an out-of-range value (including a float
;; infinity) is IllegalArgumentException "Value out of range for <type>: x".
;; A non-numeric operand is the usual ClassCastException. Numeric types outside
;; Chez's tower truncate through a hook the shim extends (BigDecimal).
(define (jolt-cast-range-throw name x)
  (jolt-throw (jolt-host-throwable
               "java.lang.IllegalArgumentException"
               (string-append "Value out of range for " name ": " (jolt-str x)))))
(define (jolt-cast-truncate-slow x) (jolt-num-cast-throw x))
(define (jolt-checked-cast name lo hi x)
  (let ((n (cond ((char? x) (char->integer x))
                 ((and (number? x) (exact? x)) (truncate x))
                 ;; a double range-checks ITSELF (before truncation): (byte
                 ;; 127.000001) throws, (byte 1.1) is 1; NaN casts to 0; an
                 ;; infinity always fails the compare.
                 ((flonum? x) (cond ((nan? x) 0)
                                    ((or (< x lo) (> x hi)) (+ hi 1))
                                    (else (exact (truncate x)))))
                 (else (jolt-cast-truncate-slow x)))))
    (if (and (>= n lo) (<= n hi)) n (jolt-cast-range-throw name x))))
;; A fixnum is what these casts are handed almost every time they are called —
;; (long (.length s)), (int i) in a loop — and it is already the answer, so each
;; cast checks for one before entering the generic path above. That path is not
;; slow by accident: `truncate` is a generic procedure call, and for the LONG
;; bounds in particular Chez's fixnums are 61-bit, so +-2^63 are BIGNUMS and the
;; two range compares are fixnum-vs-bignum generic arithmetic on every single call.
;; Measured 44.7 ns for one (unchecked-int i) against 2.7 ns on the JVM, which is
;; ~100 ns per character in a hinted .charAt loop — honeysql's alphanumeric? was
;; 30x the JVM almost entirely because of this.
;;
;; The bound tests use the fx forms, valid because `fixnum?` has already answered
;; and every bound here is itself a fixnum on a 61-bit tower. LONG needs no test
;; at all: every Chez fixnum fits a 64-bit long by construction.
(define (jolt-byte-cast x)
  (if (and (fixnum? x) (fx>=? x -128) (fx<=? x 127))
      x
      (jolt-checked-cast "byte" -128 127 x)))
(define (jolt-short-cast x)
  (if (and (fixnum? x) (fx>=? x -32768) (fx<=? x 32767))
      x
      (jolt-checked-cast "short" -32768 32767 x)))
;; `int` is the one checked cast whose overflow is NOT an
;; IllegalArgumentException. Clojure routes the integer case through
;; Numbers.throwIntOverflow, so (int 2147483648) is an ArithmeticException
;; "integer overflow" while (int 2.5e9) keeps the IAE that RT.intCast(double)
;; raises for itself. byte/short/long use the IAE for both kinds of input, which
;; is why only this one needs the split. jolt threw the IAE for both.
(define (jolt-int-overflow-throw)
  (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException"
                                   "integer overflow")))
(define (jolt-int-cast x)
  (cond
    ((and (fixnum? x) (fx>=? x -2147483648) (fx<=? x 2147483647)) x)
    ((flonum? x) (jolt-checked-cast "int" -2147483648 2147483647 x))
    (else
     (let ((n (cond ((char? x) (char->integer x))
                    ((and (number? x) (exact? x)) (truncate x))
                    (else (jolt-cast-truncate-slow x)))))
       (if (and (>= n -2147483648) (<= n 2147483647))
           n
           (jolt-int-overflow-throw))))))
(define (jolt-long-cast x)
  (if (fixnum? x)
      x
      (jolt-checked-cast "long" -9223372036854775808 9223372036854775807 x)))
(def-var! "clojure.core" "int" jolt-int-cast)
(def-var! "clojure.core" "long" jolt-long-cast)
(def-var! "clojure.core" "byte" jolt-byte-cast)
(def-var! "clojure.core" "short" jolt-short-cast)
;; char: pass a char through; a code point must be a Unicode SCALAR VALUE.
;; jolt's strings are code-point indexed — (first "\x1f603;") hands back one char
;; whose int is 128515 — so the cast has to accept the whole range or it cannot
;; rebuild a char the string layer just produced. That is wider than the JVM's
;; 16-bit char, which is the project's position when the two models differ: match
;; where possible, otherwise be a superset. Surrogates are excluded because they
;; are not scalar values (Chez's integer->char rejects them); they raise the same
;; IllegalArgumentException as an out-of-range cast rather than a raw Chez error.
(define (jolt-char x)
  (if (char? x)
      x
      (let ((n (jolt-checked-cast "char" 0 #x10FFFF x)))
        (if (and (>= n #xD800) (<= n #xDFFF))
            (jolt-cast-range-throw "char" x)
            (integer->char n)))))
(def-var! "clojure.core" "char" jolt-char)
;; unchecked-long: truncate + wrap to 64 bits (RT.uncheckedLongCast — a float
;; infinity saturates, NaN is 0). unchecked-int wraps and sign-folds to 32.
(define (jolt-cast-saturate n lo hi) (cond ((< n lo) lo) ((> n hi) hi) (else n)))
;; Same fixnum-first shape as the checked casts above, and for the same reason —
;; see their comment. A fixnum already IS its own 64-bit wrap, so the long form
;; returns it untouched; the int form still has to sign-fold a fixnum wider than
;; 32 bits, which fxand/fx- do without leaving the fixnum domain because
;; #xffffffff is itself a fixnum here.
(define (jolt-unchecked-long x)
  (cond ((fixnum? x) x)
        ;; RT.uncheckedLongCast(Object) is ((Number) x).longValue(): a Character
        ;; is not a Number there, so (unchecked-long \a) raises. Only the INT
        ;; cast has a char overload — (unchecked-long (unchecked-int c)) is the
        ;; idiom, and it still works. jolt used to answer 97 here (found by the
        ;; JVM certification of the new corpus rows).
        ((char? x) (jolt-num-cast-throw x))
        ;; an exact integer wraps (long narrowing); a double SATURATES (Java's
        ;; double->long conversion clamps at the bounds, NaN is 0).
        ((and (number? x) (exact? x)) (jolt-wrap64 (truncate x)))
        ((flonum? x) (if (nan? x) 0
                         (jolt-cast-saturate (if (infinite? x) (if (> x 0.0) unc-2^63 (- unc-2^63)) (exact (truncate x)))
                                             -9223372036854775808 9223372036854775807)))
        (else (jolt-wrap64 (jolt-cast-truncate-slow x)))))
(define (jolt-unchecked-int x)
  (cond
    ((fixnum? x)
     (if (and (fx>=? x -2147483648) (fx<=? x 2147483647))
         x
         (let ((i (fxand x #xffffffff)))
           (if (fx>=? i #x80000000) (fx- i #x100000000) i))))
    ;; a char's code point is a Unicode scalar value, so it is always inside the
    ;; int range and needs no fold — see jolt-char on why jolt's are up to
    ;; #x10FFFF rather than the JVM's 16 bits.
    ((char? x) (char->integer x))
    ((flonum? x)
     ;; double->int clamps like Java
     (if (nan? x) 0
         (jolt-cast-saturate (if (infinite? x) (if (> x 0.0) #x80000000 (- #x80000000)) (exact (truncate x)))
                             -2147483648 2147483647)))
    (else
     (let ((i (bitwise-and (jolt-unchecked-long x) #xffffffff)))
       (if (>= i #x80000000) (- i #x100000000) i)))))
(def-var! "clojure.core" "unchecked-long" jolt-unchecked-long)
(def-var! "clojure.core" "unchecked-int" jolt-unchecked-int)
(def-var! "clojure.core" "double" jolt-double)
;; float: Chez has no single-float type, so the value stays a flonum, but it is
;; the nearest single-precision value (flsingle), as the JVM's float holds:
;; (double (float 0.3)) is 0.30000001192092896 and (float Double/MIN_VALUE) is
;; 0.0. The cast range-checks against Float/MAX_VALUE first, like
;; RT.floatCast (an infinity is out of range; NaN passes).
(define fl-float-max 3.4028234663852886e38)
(define (jolt-float x)
  (let ((d (jolt-double x)))
    (cond
      ((not (flonum? d)) d)
      ((and (not (nan? d)) (or (< d (- fl-float-max)) (> d fl-float-max)))
       (jolt-cast-range-throw "float" x))
      (else (flsingle d)))))
(def-var! "clojure.core" "float" jolt-float)
;; numerator/denominator: jolt ratios are Chez exact rationals; a non-ratio is
;; the JVM's Ratio cast failure.
(define (jolt-ratio-part name f)
  (lambda (x)
    (if (and (number? x) (exact? x) (rational? x) (not (integer? x)))
        (f x)
        (jolt-throw (jolt-host-throwable
                     "java.lang.ClassCastException"
                     (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                                    " cannot be cast to class clojure.lang.Ratio"))))))
(def-var! "clojure.core" "numerator" (jolt-ratio-part "numerator" numerator))
(def-var! "clojure.core" "denominator" (jolt-ratio-part "denominator" denominator))
(def-var! "clojure.core" "compare" jolt-compare)
