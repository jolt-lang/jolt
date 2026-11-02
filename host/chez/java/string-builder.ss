;; string-builder.ss — java.lang.StringBuilder and StringBuffer over the jhost
;; record: the builder every (StringBuilder.) in core and the stdlib constructs
;; (pprint's column writer, cl-format, with-out-str's sink), its method table,
;; and the count / seq / str / class / instance? arms that make a builder a
;; CharSequence. Shared by every target; the emitter's proven-StringBuilder
;; fast path (backend_scheme.clj sb-direct-emit) calls sb-append! / sb-str /
;; sb-length / render-piece directly, so their bodies and the method table's
;; are one definition.
;;
;; Needs the jhost record, register-class-ctor! / register-host-methods!, the
;; value-model arm registries (records / records-interop / host-class) and
;; throw-jvm; loads after those and before anything that constructs a builder.

;; ---- StringBuilder ----------------------------------------------------------
;; state: #(buf len owner cache monitor pend flushed cap)
;;   buf     the flushed text, buf[0, flushed), edited in place
;;   len     the whole length, flushed and pending
;;   pend    the pieces appended since the last flush, newest first; they hold
;;           len - flushed chars
;;   cap     capacity() — java.lang.AbstractStringBuilder's number, by its rules
;;           (16 by default, length + 16 from a String, (max needed (+ (* 2 old) 2))
;;           when an edit outgrows it), kept as arithmetic
;;
;; An append conses its piece onto pend and copies nothing. Chez strings are four
;; bytes a char, and a buffer that copied each piece in, grew by copying, and
;; copied out again at toString moved every char three times; building 2000 times
;; a 4000-char string from 40-char pieces took 140ms that way against 58ms for a
;; join. So toString joins buf's text and the pending pieces in one copy, and only
;; a read or edit by index (charAt, insert, setCharAt ...) flushes pend into buf.
;; buf then grows to the logical capacity, so appends interleaved with charAt stay
;; amortized O(1) instead of copying the whole text per read.
;;
;; toString hands out a fresh string, so no String a caller holds ever aliases the
;; buffer, and it is cached until the next write clears it. Pending pieces are
;; strings jolt treats as immutable; nothing here writes to one.
;;
;; A StringBuilder is unsynchronized, as on the JDK, which makes no promise to
;; threads that race one (its own can lose a racing append outright). A
;; StringBuffer is synchronized: monitor is its own object monitor, resolved once
;; at construction, and each of its methods runs under it — `synchronized` on the
;; JDK's methods, so it composes with (locking sb ...) and is reentrant. A
;; StringWriter writes into a StringBuffer (owner, its getBuffer) and shares that
;; store and monitor, as the JDK's StringWriter synchronizes on its buf.
;;
;; The two are separate method tables expanded from ONE definition (sb-methods
;; below), so StringBuilder pays nothing for the lock and the bodies cannot drift.
;; The emitter's proven-type fast path calls the unsynchronized primitives
;; (sb-append! sb-str sb-length sb-char-at) for a StringBuilder and the locking
;; ones (sb-append*! sb-str* sb-length* sb-char-at*) for a StringBuffer.
(define (make-sb-state s cap)
  (let ((n (string-length s)))
    (vector "" n #f #f #f (if (fx=? n 0) '() (list s)) 0 cap)))
(define-syntax sb-wrote! (syntax-rules () ((_ st) (vector-set! st 3 #f))))

;; --- operations on the state vector: no locking here ---------------------------
;; Copy piece[0, pn) into buf at n. A short piece goes char by char: string-copy!
;; is a library call that checks its arguments, which is most of the cost of a
;; one-char piece; from about four chars on the call is the faster copy.
(define (%sb-copy-in! buf n piece pn)
  (if (fx<? pn 4)
      (let loop ((i 0))
        (when (fx<? i pn)
          (string-set! buf (fx+ n i) (string-ref piece i))
          (loop (fx+ i 1))))
      (sa-string-copy-range! buf n piece 0 pn)))
;; Copy the pending pieces into dst, ending at `end`: they are newest first, so
;; each one goes just below the one after it.
(define (%sb-copy-pending! dst end pend)
  (let loop ((ps pend) (at end))
    (unless (null? ps)
      (let* ((p (car ps)) (pn (string-length p)) (from (fx- at pn)))
        (%sb-copy-in! dst from p pn)
        (loop (cdr ps) from)))))
;; ensureCapacityInternal's arithmetic on the logical capacity.
(define (%sb-grow-cap! st min)
  (let ((cap (vector-ref st 7)))
    (when (fx>? min cap) (vector-set! st 7 (fxmax min (fx+ (fx* 2 cap) 2))))))
;; Move the pending pieces into buf, growing it to the logical capacity, so buf
;; holds the whole text. Every read or edit by index starts here.
(define (%sb-flush! st)
  (let ((pend (vector-ref st 5)) (n (vector-ref st 1)))
    (unless (null? pend)
      (when (fx>? n (string-length (vector-ref st 0)))
        (let ((nb (make-string (fxmax n (vector-ref st 7)))) (fl (vector-ref st 6)))
          (sa-string-copy-range! nb 0 (vector-ref st 0) 0 fl)
          (vector-set! st 0 nb)))
      (%sb-copy-pending! (vector-ref st 0) n pend)
      (vector-set! st 5 '())
      (vector-set! st 6 n))))
(define-syntax %sb-flushed
  (syntax-rules () ((_ st) (unless (null? (vector-ref st 5)) (%sb-flush! st)))))
;; The new length after an edit in place, which runs flushed: all of it is in buf.
(define (%sb-set-length! st n)
  (vector-set! st 1 n)
  (vector-set! st 6 n)
  (sb-wrote! st))
;; ensureCapacityInternal for an edit in place: flushed, with room for min.
(define (%sb-ensure! st min)
  (%sb-flushed st)
  (%sb-grow-cap! st min)
  (let ((buf (vector-ref st 0)))
    (when (fx>? min (string-length buf))
      (let ((nb (make-string (vector-ref st 7))))
        (sa-string-copy-range! nb 0 buf 0 (vector-ref st 1))
        (vector-set! st 0 nb)))))
;; toString, append and charAt are macros so the unsynchronized primitives below
;; carry their bodies inline (no second call per append) while the locked table
;; expands the very same text.
(define (%sb-materialize! st)
  (let* ((n (vector-ref st 1)) (pend (vector-ref st 5)) (out (make-string n))
         (fl (vector-ref st 6)))
    (sa-string-copy-range! out 0 (vector-ref st 0) 0 fl)
    (%sb-copy-pending! out n pend)
    (vector-set! st 3 out)
    ;; The joined text stands in for everything before it: nothing writes to out,
    ;; so it is a pending piece like any other, and the next toString copies it as
    ;; one block instead of walking every piece again.
    (vector-set! st 5 (list out))
    (vector-set! st 6 0)
    out))
(define-syntax %sb-str
  (syntax-rules () ((_ st) (let ((s st)) (or (vector-ref s 3) (%sb-materialize! s))))))
(define-syntax %sb-append!
  (syntax-rules ()
    ((_ st* piece*)
     (let* ((st st*) (piece piece*) (pn (string-length piece)))
       (unless (fx=? pn 0)
         (let ((nn (fx+ (vector-ref st 1) pn)))
           (when (fx>? nn (vector-ref st 7)) (%sb-grow-cap! st nn))
           (vector-set! st 5 (cons piece (vector-ref st 5)))
           (vector-set! st 1 nn)
           (sb-wrote! st)))))))
;; Replace buf[s, e) with piece, in place: the one primitive insert, delete,
;; replace and deleteCharAt are spelled in. The tail moves first (string-copy!
;; copies as if through a temporary, so the overlap is safe).
(define (%sb-splice! st s e piece)
  (%sb-flushed st)
  (let* ((n (vector-ref st 1)) (pn (string-length piece))
         (nn (fx+ (fx- n (fx- e s)) pn)))
    (%sb-ensure! st nn)
    (let ((buf (vector-ref st 0)))
      (sa-string-copy-range! buf (fx+ s pn) buf e n)
      (sa-string-copy-range! buf s piece 0 pn)
      (%sb-set-length! st nn))))
;; charAt over the live buffer, with String.charAt's check against the length.
(define-syntax %sb-char-at
  (syntax-rules ()
    ((_ st* i*)
     (let* ((st st*) (n (vector-ref st 1)) (i (jolt->idx i*)))
       (if (and (fixnum? i) (fx>=? i 0) (fx<? i n))
           (begin (%sb-flushed st) (string-ref (vector-ref st 0) i))
           (char-index-oob i n))))))

;; --- the unsynchronized primitives: StringBuilder and the emitter's fast path --
(define (sb-str self) (%sb-str (jhost-state self)))
(define (sb-append! self piece) (%sb-append! (jhost-state self) piece))
(define (sb-length self) (vector-ref (jhost-state self) 1))
(define (sb-char-at self i) (%sb-char-at (jhost-state self) i))

;; --- any builder: the seams handed a StringBuilder, StringBuffer or StringWriter
;; (a PrintWriter's target, str, the CharSequence arms) take the lock when the
;; store has one.
(define-syntax sb-guarded
  (syntax-rules ()
    ((_ (st self) body ...)
     (let ((st (jhost-state self)))
       (jolt-call-with-monitor (vector-ref st 4) (lambda () body ...))))))
(define-syntax sb-unguarded
  (syntax-rules () ((_ (st self) body ...) (let ((st (jhost-state self))) body ...))))
(define (sb-str* self)
  (if (vector-ref (jhost-state self) 4) (sb-guarded (st self) (%sb-str st)) (sb-str self)))
(define (sb-append*! self piece)
  (if (vector-ref (jhost-state self) 4) (sb-guarded (st self) (%sb-append! st piece)) (sb-append! self piece)))
(define (sb-length* self)
  (if (vector-ref (jhost-state self) 4) (sb-guarded (st self) (vector-ref st 1)) (sb-length self)))
(define (sb-char-at* self i)
  (if (vector-ref (jhost-state self) 4) (sb-guarded (st self) (%sb-char-at st i)) (sb-char-at self i)))

(define (render-piece x)
  (cond ((jolt-nil? x) "null") ((char? x) (string x)) ((string? x) x)
        (else (jolt-str-render-one x))))

;; Appendable.append text: append(x) renders x; append(csq,start,end) appends the
;; subsequence csq[start,end) (data.json's writer appends string runs this way).
(define (append-text x rest)
  (if (null? rest)
      (render-piece x)
      (substring (render-piece x) (jnum->exact (car rest)) (jnum->exact (cadr rest)))))

;; The bounds every (offset, len) region shares, reported the way the JVM reports
;; them: the half-open range that was asked for, against the length there was.
;; WHO is the exception class, which the JDK picks per call site and jolt follows
;; — StringBuilder.append(char[],off,len) raises the plain IndexOutOfBounds,
;; .insert and String.valueOf the StringIndexOutOfBounds that extends it.
(define (jvm-range-check who len off end)
  ;; generic comparisons, not fx: an offset a caller got wrong can be any integer,
  ;; and a bignum must report as the out-of-range index it is rather than fault
  ;; inside a fixnum primitive
  (when (or (< off 0) (> off end) (> end len))
    (throw-jvm who
               (string-append "Range [" (number->string off) ", " (number->string end)
                              ") out of bounds for length " (number->string len)))))
;; What StringBuilder.append / .insert put in the buffer. The char[] overloads are
;; the ones append-text cannot serve: their 3-arg form is (offset, len) over the
;; ARRAY, where the CharSequence one is (start, end) over the rendering.
;; char-array->string / char-array-chunk are natives-array.ss's (the char[]
;; backing lives there); the predicate is here because every target answers it —
;; one with no arrays answers #f from its jolt-array? and never enters the other
;; two.
(define (char-array-arg? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'char)))
(define (sb-piece x)
  ;; string first: this runs on the open-coded .append path (sb-direct-emit), where
  ;; a string is nearly every argument and a char the next most common, and
  ;; render-piece would reach its own arms for them only after failed tests.
  (cond ((string? x) x)
        ((char? x) (string x))
        ((char-array-arg? x) (char-array->string x))
        (else (render-piece x))))
(define (sb-piece-range x a b who)
  (if (char-array-arg? x)
      (char-array-chunk x (jnum->exact a) (jnum->exact b) who)
      (append-text x (list a b))))

;; The JDK's checks, with its messages: an index is checkIndex's, a span is
;; checkRangeSIOOBE's (jvm-range-check above), and an insert offset reports the
;; span [offset, length) it fell outside.
(define (sb-check-offset off n)
  (when (or (< off 0) (> off n))
    (throw-jvm 'StringIndexOutOfBoundsException
               (string-append "Range [" (number->string off) ", " (number->string n)
                              ") out of bounds for length " (number->string n)))))
(define (sb-check-index i n)
  (unless (and (fixnum? i) (fx>=? i 0) (fx<? i n)) (char-index-oob i n)))

;; A capacity argument that is negative is the array allocation's failure, as on
;; the JDK; content starts with room for 16 more.
(define (sb-new-state args)
  (cond ((null? args) (make-sb-state "" 16))
        ((jolt-nil? (car args)) (throw-jvm 'NullPointerException "str"))
        ((number? (car args))
         (let ((cap (jnum->exact (car args))))
           (when (< cap 0) (throw-jvm 'NegativeArraySizeException (number->string cap)))
           (make-sb-state "" cap)))
        (else (let ((s (render-piece (car args))))
                (make-sb-state s (fx+ (string-length s) 16))))))
;; A synchronized builder locks on itself, which only exists once it is made.
(define (make-locked-sb tag st)
  (let ((h (make-jhost tag st)))
    (vector-set! st 2 h)
    (vector-set! st 4 (object-monitor h))
    h))

;; The JDK compares builders char by char, then by length.
(define (sb-compare a b)
  (let ((na (string-length a)) (nb (string-length b)))
    (let loop ((i 0))
      (cond ((or (fx=? i na) (fx=? i nb)) (fx- na nb))
            ((char=? (string-ref a i) (string-ref b i)) (loop (fx+ i 1)))
            (else (fx- (char->integer (string-ref a i)) (char->integer (string-ref b i))))))))

;; The method table, once. GUARD is sb-unguarded (StringBuilder) or sb-guarded
;; (StringBuffer): each method body runs inside it with st bound to the store.
;; A piece to append is rendered BEFORE the guard, so a user toString never runs
;; under the buffer's lock.
;;
;; The overloaded members are case-lambdas, not one rest-taking arm: the arities
;; are the overload set the JVM resolves against, and host-arity-ok? reads them
;; off the procedure (host-static.ss). One arm that picked its extra arguments
;; out of a list is how (.append sb "x" 1) reached `cadr` on a one-element list
;; and reported Chez's "incorrect list structure" for what is an arity error.
(define-syntax sb-methods
  (syntax-rules ()
    ((_ guard)
     (list
      (cons "append"
            (case-lambda
              ((self x) (let ((p (sb-piece x))) (guard (st self) (%sb-append! st p))) self)
              ((self x a b)
               (let ((p (sb-piece-range x a b (quote IndexOutOfBoundsException))))
                 (guard (st self) (%sb-append! st p)))
               self)))
      (cons "appendCodePoint"
            (lambda (self cp)
              (let ((c (jnum->exact cp)))
                (unless (and (integer? c) (<= 0 c #x10FFFF) (not (<= #xD800 c #xDFFF)))
                  (throw-jvm 'IllegalArgumentException
                             (string-append "Not a valid Unicode code point: 0x"
                                            (string-upcase (number->string (bitwise-and c #xFFFFFFFF) 16)))))
                (let ((p (string (integer->char c)))) (guard (st self) (%sb-append! st p)))
                self)))
      (cons "toString" (lambda (self) (guard (st self) (%sb-str st))))
      (cons "length" (lambda (self) (guard (st self) (->num (vector-ref st 1)))))
      (cons "isEmpty" (lambda (self) (guard (st self) (fx=? 0 (vector-ref st 1)))))
      (cons "charAt" (lambda (self i) (guard (st self) (%sb-char-at st i))))
      (cons "capacity" (lambda (self) (guard (st self) (->num (vector-ref st 7)))))
      (cons "ensureCapacity"
            (lambda (self m)
              (let ((m (jnum->exact m)))
                (when (> m 0) (guard (st self) (%sb-ensure! st m))))
              jolt-nil))
      (cons "trimToSize"
            (lambda (self)
              (guard (st self)
                (%sb-flushed st)
                (let ((n (vector-ref st 1)))
                  (when (fx<? n (vector-ref st 7))
                    (let ((nb (make-string n)))
                      (sa-string-copy-range! nb 0 (vector-ref st 0) 0 n)
                      (vector-set! st 0 nb)
                      (vector-set! st 7 n)))))
              jolt-nil))
      (cons "setLength"
            (lambda (self n)
              (let ((n (jnum->exact n)))
                (when (< n 0)
                  (throw-jvm 'StringIndexOutOfBoundsException
                             (string-append "String index out of range: " (number->string n))))
                (guard (st self)
                  (let ((cur (vector-ref st 1)))
                    (%sb-ensure! st n)
                    (when (fx>? n cur)
                      (let ((buf (vector-ref st 0)))
                        (do ((i cur (fx+ i 1))) ((fx=? i n)) (string-set! buf i #\nul))))
                    (%sb-set-length! st n))))
              jolt-nil))
      (cons "substring"
            (case-lambda
              ((self start) (let ((cur (guard (st self) (%sb-str st))) (s (jnum->exact start)))
                              (jvm-range-check 'StringIndexOutOfBoundsException (string-length cur) s (string-length cur))
                              (substring cur s (string-length cur))))
              ((self start end) (let ((cur (guard (st self) (%sb-str st)))
                                      (s (jnum->exact start)) (e (jnum->exact end)))
                                  (jvm-range-check 'StringIndexOutOfBoundsException (string-length cur) s e)
                                  (substring cur s e)))))
      ;; CharSequence.subSequence — AbstractStringBuilder returns substring(a, b),
      ;; i.e. a String, which is itself a CharSequence.
      (cons "subSequence" (lambda (self a b)
                            (let ((cur (guard (st self) (%sb-str st)))
                                  (s (jnum->exact a)) (e (jnum->exact b)))
                              (jvm-range-check 'StringIndexOutOfBoundsException (string-length cur) s e)
                              (substring cur s e))))
      (cons "indexOf"
            (case-lambda
              ((self needle)
               (->num (str-index-of (guard (st self) (%sb-str st)) (render-piece needle) 0)))
              ((self needle from)
               (->num (str-index-of (guard (st self) (%sb-str st)) (render-piece needle)
                                    (max 0 (jnum->exact from)))))))
      (cons "lastIndexOf"
            (case-lambda
              ((self needle) (->num (str-last-index-of (guard (st self) (%sb-str st)) (render-piece needle))))
              ((self needle from)
               (let* ((cur (guard (st self) (%sb-str st)))
                      (i (str-last-find (render-piece needle) cur
                                        (max -1 (min (jnum->exact from) (string-length cur))))))
                 (->num (if (jolt-nil? i) -1 i))))))
      (cons "codePointAt" (lambda (self i)
                            (guard (st self)
                              (let ((i (jnum->exact i)))
                                (sb-check-index i (vector-ref st 1))
                                (%sb-flushed st)
                                (->num (char->integer (string-ref (vector-ref st 0) i)))))))
      (cons "codePointBefore" (lambda (self i)
                                (guard (st self)
                                  (let ((j (- (jnum->exact i) 1)))
                                    (sb-check-index j (vector-ref st 1))
                                    (%sb-flushed st)
                                    (->num (char->integer (string-ref (vector-ref st 0) j)))))))
      ;; jolt's strings hold code points, so a count or offset in code points is
      ;; the same count in chars; the bounds are still the JDK's.
      (cons "codePointCount" (lambda (self b e)
                               (let ((b (jnum->exact b)) (e (jnum->exact e))
                                     (n (guard (st self) (vector-ref st 1))))
                                 (jvm-range-check 'IndexOutOfBoundsException n b e)
                                 (->num (- e b)))))
      (cons "offsetByCodePoints" (lambda (self i k)
                                   (let* ((i (jnum->exact i)) (r (+ i (jnum->exact k)))
                                          (n (guard (st self) (vector-ref st 1))))
                                     (when (or (< i 0) (> i n) (< r 0) (> r n))
                                       (throw-jvm 'IndexOutOfBoundsException jolt-nil))
                                     (->num r))))
      (cons "compareTo" (lambda (self other)
                          (->num (sb-compare (guard (st self) (%sb-str st)) (sb-str* other)))))
      (cons "setCharAt" (lambda (self i ch)
                          (let ((c (string-ref (render-piece ch) 0)))
                            (guard (st self)
                              (let ((i (jnum->exact i)))
                                (sb-check-index i (vector-ref st 1))
                                (%sb-flushed st)
                                (string-set! (vector-ref st 0) i c)
                                (sb-wrote! st))))
                          jolt-nil))
      (cons "deleteCharAt" (lambda (self i)
                             (guard (st self)
                               (let ((i (jnum->exact i)))
                                 (sb-check-index i (vector-ref st 1))
                                 (%sb-splice! st i (fx+ i 1) "")))
                             self))
      ;; delete and replace clamp the end to the length, then check, as the JDK does.
      (cons "delete" (lambda (self start end)
                       (guard (st self)
                         (let* ((n (vector-ref st 1)) (s (jnum->exact start)) (e (min n (jnum->exact end))))
                           (jvm-range-check 'StringIndexOutOfBoundsException n s e)
                           (%sb-splice! st s e "")))
                       self))
      (cons "replace" (lambda (self start end txt)
                        (let ((p (render-piece txt)))
                          (guard (st self)
                            (let* ((n (vector-ref st 1)) (s (jnum->exact start)) (e (min n (jnum->exact end))))
                              (jvm-range-check 'StringIndexOutOfBoundsException n s e)
                              (%sb-splice! st s e p))))
                        self))
      (cons "insert"
            (let ((ins (lambda (self offset p)
                         (guard (st self)
                           (let ((o (jnum->exact offset)))
                             (sb-check-offset o (vector-ref st 1))
                             (%sb-splice! st o o p)))
                         self)))
              (case-lambda
                ((self offset x) (ins self offset (sb-piece x)))
                ((self offset x a b)
                 (ins self offset (sb-piece-range x a b (quote StringIndexOutOfBoundsException)))))))
      ;; .getChars srcBegin srcEnd dst dstBegin — the CharSequence copy-out the
      ;; String method already had (natives-str.ss). Both ends are checked before
      ;; anything is written, so a bad request does not leave the destination half
      ;; filled; the destination's own error is the plain IndexOutOfBounds.
      (cons "getChars"
            (lambda (self src-begin src-end dst dst-begin)
              (let* ((cur (guard (st self) (%sb-str st)))
                     (s (jnum->exact src-begin)) (e (jnum->exact src-end))
                     (d (jnum->exact dst-begin)))
                (jvm-range-check (quote StringIndexOutOfBoundsException) (string-length cur) s e)
                (jvm-range-check (quote IndexOutOfBoundsException) (ja-len dst) d (+ d (- e s)))
                (let ((v (jolt-array-vec dst)))
                  (if (string? v)
                      (sa-string-copy-range! v d cur s e)
                      (let loop ((i s) (j d))
                        (when (fx<? i e)
                          (ja-set! dst j (string-ref cur i))
                          (loop (fx+ i 1) (fx+ j 1)))))))
              jolt-nil))
      (cons "reverse" (lambda (self)
                        (guard (st self)
                          (%sb-flushed st)
                          (let ((buf (vector-ref st 0)) (n (vector-ref st 1)))
                            (let loop ((i 0) (j (fx- n 1)))
                              (when (fx<? i j)
                                (let ((c (string-ref buf i)))
                                  (string-set! buf i (string-ref buf j))
                                  (string-set! buf j c)
                                  (loop (fx+ i 1) (fx- j 1)))))
                            (sb-wrote! st)))
                        self))))))

(register-class-ctor! "StringBuilder"
  (lambda args (make-jhost "string-builder" (sb-new-state args))))
(register-host-methods! "string-builder" (sb-methods sb-unguarded))

;; StringBuffer — the JDK's synchronized builder: the same store and the same
;; method bodies, each under the buffer's own monitor, and a second tag so
;; (class …) and instance? answer the class the caller wrote. rewrite-clj's reader
;; (cljfmt's parser) builds one per token.
(register-class-ctor! "StringBuffer"
  (lambda args (make-locked-sb "string-buffer" (sb-new-state args))))
(register-host-methods! "string-buffer" (sb-methods sb-guarded))

;; (str sb) / print a StringBuilder -> its accumulated content, like the JVM
;; (str calls toString). Without this str renders the opaque host object.
;; Both tags answer here: every arm below is about the shared store, so asking
;; the tag literally at any one of them is how a StringBuffer silently stops
;; being countable, seqable or printable while a StringBuilder still is.
(define-jhost-tag-pred (sb-builder-jhost? t) (string=? t "string-builder"))
(define-jhost-tag-pred (sb-buffer-jhost? t) (string=? t "string-buffer"))
(define (sb-jhost? x) (or (sb-builder-jhost? x) (sb-buffer-jhost? x)))
(define (sb-class-name x)
  (if (sb-buffer-jhost? x) "java.lang.StringBuffer" "java.lang.StringBuilder"))
(register-str-render! sb-jhost? sb-str*)
;; A StringBuilder IS a java.lang.CharSequence, so it answers (class …),
;; instance? through the class graph, and the three RT entry points that name a
;; CharSequence — count is its length, seq walks its characters, nth reads one.
;; Without the class arm (class sb) leaked the :object placeholder.
(register-class-arm! sb-jhost? sb-class-name)
;; An array class reaches instance-check as a raw string ("[C"), not a symbol, and
;; this arm is newer than the base taxonomy so it is asked first — hence the
;; symbol-t? guard before reading the name.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (sb-jhost? val) (symbol-t? type-sym))
        (jch-isa? (sb-class-name val) (symbol-t-name type-sym))
        'pass)))
(register-count-arm! sb-jhost? (lambda (x) (sb-length* x)))
(register-seq-arm! sb-jhost? (lambda (x) (jolt-seq (sb-str* x))))
