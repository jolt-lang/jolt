;; Tests for the Jolt value model on Chez (nil/truthiness, interned keywords,
;; symbols, exactness-aware =, hashing). Run from repo root:
;;   chez --script test/chez/values-test.ss
(import (chezscheme))
(load "host/chez/rt.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

;; nil distinct from #f and '()
(ok "nil not #f"        (not (eq? jolt-nil #f)))
(ok "nil not '()"       (not (eq? jolt-nil '())))
(ok "nil? jolt-nil"     (jolt-nil? jolt-nil))
(ok "nil? not on #f"    (not (jolt-nil? #f)))

;; truthiness: only nil and false falsey
(ok "nil falsey"        (not (jolt-truthy? jolt-nil)))
(ok "false falsey"      (not (jolt-truthy? #f)))
(ok "true truthy"       (jolt-truthy? #t))
(ok "0 truthy"          (jolt-truthy? 0))
(ok "empty-str truthy"  (jolt-truthy? ""))
(ok "empty-list truthy" (jolt-truthy? '()))

;; keywords interned -> identity
(ok "kw eq"             (eq? (keyword #f "foo") (keyword #f "foo")))
(ok "kw ns eq"          (eq? (keyword "a" "foo") (keyword "a" "foo")))
(ok "kw diff ns"        (not (eq? (keyword "a" "foo") (keyword #f "foo"))))
(ok "kw?"               (keyword? (keyword #f "x")))
(ok "kw not sym"        (not (jolt-symbol? (keyword #f "x"))))

;; symbols NOT interned but jolt= by ns/name
(ok "sym not eq"        (not (eq? (jolt-symbol #f "x") (jolt-symbol #f "x"))))
(ok "sym jolt="         (jolt= (jolt-symbol #f "x") (jolt-symbol #f "x")))
(ok "sym diff name"     (not (jolt= (jolt-symbol #f "x") (jolt-symbol #f "y"))))
(ok "sym?"              (jolt-symbol? (jolt-symbol "ns" "n")))

;; intern-symbol-cell: one cell per name CONTENT, however the string arrives.
;; The identity front cache must stay invisible: the same string object twice,
;; two distinct-but-equal string objects, and a cross-thread build all hand back
;; the same pool cell, so eq?-based symbol equality and the ncell hash memo hold.
(ok "intern cell stable for same string object"
    (let ((s (string-append "ivt-" "one")))
      (eq? (intern-symbol-cell s) (intern-symbol-cell s))))
(ok "intern cell shared across equal string objects"
    (eq? (intern-symbol-cell (string-append "ivt-" "two"))
         (intern-symbol-cell (string-append "ivt-" "two"))))
(ok "syms from distinct equal strings share the ncell"
    (eq? (symbol-t-ncell (jolt-symbol #f (string-append "ivt-" "three")))
         (symbol-t-ncell (jolt-symbol #f (string-append "ivt-" "three")))))
(ok "syms from distinct equal strings are jolt="
    (jolt= (jolt-symbol #f (string-append "ivt-" "four"))
           (jolt-symbol #f (string-append "ivt-" "four"))))
;; procedure identity hasheq: distinct fns hash distinct (per-process ids, the
;; JVM's Object.hashCode shape), the id is stable for the object — including
;; ACROSS a collection, which a raw address hash would break — and equality is
;; identity answered ahead of the arm walk.
(ok "distinct procedures hash distinct"
    (not (= (jolt-hasheq car) (jolt-hasheq cdr))))
(ok "procedure hasheq stable"
    (= (jolt-hasheq car) (jolt-hasheq car)))
(ok "procedure hasheq survives a collection"
    (let ((h (jolt-hasheq vector->list)))
      (collect)
      (= h (jolt-hasheq vector->list))))
(ok "procedure hasheq is 32-bit"
    (let ((h (jolt-hasheq car)))
      (and (fixnum? h) (fx<=? -2147483648 h 2147483647))))
(ok "jolt=2 identical procedures" (jolt=2 car car))
(ok "jolt=2 distinct procedures" (not (jolt=2 car cdr)))

;; --- record/deftype hasheq rides an instance field (the JVM's __hasheq slot) --
;; a registered (defrecord) tag caches its STRUCTURAL hash in the slot
(define vt-hr-desc (make-jrdesc "user.VtHashRec" (list (keyword #f "a") (keyword #f "b"))))
(jolt-with-mutex rec-tbl-mu (hashtable-set! chez-record-type-tbl "user.VtHashRec" #t))
(define vt-hr (make-jrec2 vt-hr-desc jolt-nil 0 1 2))
(ok "record hasheq slot starts unset" (eqv? 0 (jrec-hasheq vt-hr)))
(define vt-hr-h (jolt-hash vt-hr))
(ok "record hash fills the slot" (eqv? vt-hr-h (jrec-hasheq vt-hr)))
(ok "slot answers the repeat hash" (eqv? vt-hr-h (jolt-hash vt-hr)))
(ok "content-equal record hashes equal (cache-invisible)"
    (eqv? vt-hr-h (jolt-hash (make-jrec2 vt-hr-desc jolt-nil 0 1 2))))
(ok "jolt-hasheq agrees with jolt-hash on a record" (eqv? vt-hr-h (jolt-hasheq vt-hr)))
;; an unregistered tag (plain deftype) caches its IDENTITY hash in the same slot
(define vt-dt-desc (make-jrdesc "user.VtHashDt" (list (keyword #f "a"))))
(define vt-dt1 (make-jrec1 vt-dt-desc jolt-nil 0 7))
(define vt-dt2 (make-jrec1 vt-dt-desc jolt-nil 0 7))
(define vt-dt1-h (jolt-hash vt-dt1))
(ok "deftype hash cached in the slot" (eqv? vt-dt1-h (jrec-hasheq vt-dt1)))
(ok "deftype hash is identity: equal fields still differ"
    (not (eqv? vt-dt1-h (jolt-hash vt-dt2))))
(ok "deftype hash stable across calls" (eqv? vt-dt1-h (jolt-hash vt-dt1)))
(ok "deftype hash is 32-bit"
    (and (fixnum? vt-dt1-h) (fx<=? -2147483648 vt-dt1-h 2147483647)))

;; pvec cached hasheq: the leaf-run compute must equal the seq walk (vectors
;; and lists hash EQUAL as ordered colls), repeat, and survive the cache; the
;; equality fast-reject must never change an answer.
(ok "pvec hash equals list hash of same elements"
    (= (jolt-hasheq (jolt-vector 1 "two" (keyword #f "three")))
       (hash-ordered (jolt-seq (jolt-list 1 "two" (keyword #f "three"))))))
(ok "pvec hash stable on repeat"
    (let ((v (jolt-vector 1 2 3 4 5)))
      (= (jolt-hasheq v) (jolt-hasheq v))))
(ok "equal vecs stay equal after both are hashed"
    (let ((a (jolt-vector 1 2 3)) (b (jolt-vector 1 2 3)))
      (jolt-hasheq a) (jolt-hasheq b)
      (jolt=2 a b)))
(ok "unequal vecs stay unequal after both are hashed (reject path)"
    (let ((a (jolt-vector 1 2 3)) (b (jolt-vector 1 2 4)))
      (jolt-hasheq a) (jolt-hasheq b)
      (not (jolt=2 a b))))
(ok "unequal vecs stay unequal when only one is hashed"
    (let ((a (jolt-vector 1 2 3)) (b (jolt-vector 1 2 4)))
      (jolt-hasheq a)
      (not (jolt=2 a b))))
(ok "equal maps stay equal after both are hashed"
    (let ((a (jolt-hash-map (keyword #f "k") 1)) (b (jolt-hash-map (keyword #f "k") 1)))
      (jolt-hasheq a) (jolt-hasheq b)
      (jolt=2 a b)))

;; seq hasheq: cached per head object (ASeq._hasheq), values pinned to the
;; ordered-coll hash — a seq, a list and a vector of the same elements all
;; hash equal, repeats are stable, and a suffix hashes as its own chain.
(ok "seq hash equals vector hash of same elements"
    (let ((l (jolt-list 1 "two" (keyword #f "three"))))
      (= (jolt-hasheq l) (jolt-hasheq (jolt-vector 1 "two" (keyword #f "three"))))))
(ok "seq hash stable on repeat"
    (let ((l (jolt-list 1 2 3 4 5)))
      (= (jolt-hasheq l) (jolt-hasheq l))))
(ok "seq suffix hashes as its own chain"
    (let ((l (jolt-list 1 2 3)))
      (= (jolt-hasheq (seq-more l)) (jolt-hasheq (jolt-list 2 3)))))

(ok "intern cell agrees across threads"
    (let* ((s (string-append "ivt-" "five"))
           (mine (intern-symbol-cell s))
           (theirs #f)
           (done (make-mutex))
           (cv (make-condition)))
      (mutex-acquire done)
      (fork-thread (lambda ()
                     (let ((c (intern-symbol-cell (string-append "ivt-" "five"))))
                       (mutex-acquire done)
                       (set! theirs c)
                       (condition-signal cv)
                       (mutex-release done))))
      (let loop ()
        (unless theirs (condition-wait cv done) (loop)))
      (mutex-release done)
      (eq? mine theirs)))

;; numbers: exactness-aware = (Clojure semantics)
(ok "1 = 1"             (jolt= 1 1))
(ok "1 not= 1.0"        (not (jolt= 1 1.0)))
(ok "1.0 = 1.0"         (jolt= 1.0 1.0))
(ok "ratio ="           (jolt= 1/2 1/2))
(ok "bigint=int exact"  (jolt= 2 (expt 2 1)))
(ok "= variadic"        (jolt= 3 3 3))
(ok "= variadic false"  (not (jolt= 3 3 4)))

;; strings / chars
(ok "str ="             (jolt= "ab" "ab"))
(ok "str !="            (not (jolt= "ab" "ac")))
(ok "char ="            (jolt= #\a #\a))

;; hashing consistent with =
(ok "hash kw stable"    (= (jolt-hash (keyword #f "k")) (jolt-hash (keyword #f "k"))))
(ok "hash sym stable"   (= (jolt-hash (jolt-symbol #f "k")) (jolt-hash (jolt-symbol #f "k"))))
(ok "hash 1 != 1.0"     (not (= (jolt-hash 1) (jolt-hash 1.0))))
(ok "hash str stable"   (= (jolt-hash "abc") (jolt-hash "abc")))

;; regression: keyword intern key must not collide across ns/name boundary
(ok "kw no boundary collide" (not (eq? (keyword "a" "b/c") (keyword "a/b" "c"))))
;; regression: jolt-hash must not throw on non-finite floats
(ok "hash +inf ok" (number? (jolt-hash +inf.0)))
(ok "hash +nan ok"  (number? (jolt-hash +nan.0)))
(ok "hash inf != exact" (not (= (jolt-hash +inf.0) (jolt-hash 0))))

;; --- arm registries reject arms their fast path would skip ------------------
;; jolt-hash answers nil/keyword/fixnum/string, and jolt=2 answers fixnum and
;; flonum pairs, before consulting the arms. An arm claiming one of those would
;; be silently skipped, so registration rejects it rather than leaving the
;; invariant to a comment. Each registry guards its OWN fast path — the
;; printer's is wider, and reusing it here would reject legitimate arms.
(define (raises? thunk) (guard (e (#t #t)) (thunk) #f))

(define-record-type armtest-t (fields v) (nongenerative armtest-v1))

(ok "hash arm rejects fixnum"  (raises? (lambda () (register-hash-arm! fixnum? (lambda (x) 0)))))
(ok "hash arm rejects string"  (raises? (lambda () (register-hash-arm! string? (lambda (x) 0)))))
(ok "hash arm rejects keyword" (raises? (lambda () (register-hash-arm! keyword-t? (lambda (x) 0)))))
(ok "hash arm rejects nil"     (raises? (lambda () (register-hash-arm! jolt-nil?-fn (lambda (x) 0)))))

;; an arm claiming BOTH sides of a fast pair is what jolt=2 would skip
(ok "eq arm rejects fixnum pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (and (fixnum? a) (fixnum? b)))
                                          (lambda (a b) #t)))))
(ok "eq arm rejects flonum pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (and (flonum? a) (flonum? b)))
                                          (lambda (a b) #t)))))

;; hash's fast path is NARROWER than the printer's: chars, flonums and bignums all
;; reach the arms, so the hash guard must let them through
(ok "hash guard allows char"   (not (raises? (lambda () (hash-arm-reject-fast-type! 'test char?)))))
(ok "hash guard allows flonum" (not (raises? (lambda () (hash-arm-reject-fast-type! 'test flonum?)))))
(ok "pr guard still rejects char" (raises? (lambda () (pr-arm-reject-fast-type! 'test char?))))

;; Symbols moved ONTO the hash fast path (values.ss jolt-hash, hasheq.ss
;; jolt-hasheq) when symbol-t gained its khash field, so a symbol-claiming hash arm
;; is now exactly the silent-skip the invariant exists to catch and the guard has
;; to reject it. This assertion is the inverse of the one it replaces; if symbols
;; ever come back off that fast path it has to flip back.
(ok "hash guard now rejects symbol"
    (raises? (lambda () (hash-arm-reject-fast-type! 'test symbol-t?))))

;; pmap-fast-get (collections.ss) answers keyword and string key pairs without
;; consulting the arms — same class of bypass as jolt=2's fixnum/flonum clauses
;; — so an arm claiming either type would be silently skipped by map lookups.
(ok "eq arm rejects keyword pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (or (keyword-t? a) (keyword-t? b)))
                                          (lambda (a b) #t)))))
(ok "eq arm rejects string pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (or (string? a) (string? b)))
                                          (lambda (a b) #t)))))
;; jolt=2 answers a symbol PAIR directly now (the pooled-string eq? compare), so
;; symbols joined keywords and strings above and an arm claiming them is rejected
;; on the same grounds. Records are the type left to make the either-arg point
;; with: eq's identity clause legitimately short-circuits every non-number type,
;; and matching that clause is not what the invariant is about.
(ok "eq arm rejects symbol pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (or (symbol-t? a) (symbol-t? b)))
                                          (lambda (a b) #t)))))
(ok "eq guard allows either-arg shape"
    (not (raises? (lambda () (eq-arm-reject-fast-type!
                              'test (lambda (a b) (or (armtest-t? a) (armtest-t? b))))))))

;; jolt's own vector, map and set are answered ahead of BOTH walks (jolt=2,
;; jolt-hash, jolt-hasheq), so all three have to be in the probe sets — a type
;; that is hoisted but unprobed is one whose arms register happily and are then
;; silently dead, which is the exact failure the guard exists to make loud.
;; Asserting each separately so a probe set losing one type names which.
(ok "hash arm rejects vector" (raises? (lambda () (hash-arm-reject-fast-type! 'test pvec?))))
(ok "hash arm rejects map"    (raises? (lambda () (hash-arm-reject-fast-type! 'test pmap?))))
(ok "hash arm rejects set"    (raises? (lambda () (hash-arm-reject-fast-type! 'test pset?))))
(ok "eq arm rejects vector pair"
    (raises? (lambda () (eq-arm-reject-fast-type!
                         'test (lambda (a b) (and (pvec? a) (pvec? b)))))))
(ok "eq arm rejects map pair"
    (raises? (lambda () (eq-arm-reject-fast-type!
                         'test (lambda (a b) (and (pmap? a) (pmap? b)))))))
(ok "eq arm rejects set pair"
    (raises? (lambda () (eq-arm-reject-fast-type!
                         'test (lambda (a b) (and (pset? a) (pset? b)))))))

;; and a real arm on a type off the fast path still registers and is consulted
(ok "hash arm on a plain type registers"
    (not (raises? (lambda () (register-hash-arm! armtest-t? (lambda (x) 4242))))))
(ok "registered hash arm is consulted" (= 4242 (jolt-hash (make-armtest-t 1))))
(ok "eq arm on a plain type registers"
    (not (raises? (lambda () (register-eq-arm! (lambda (a b) (or (armtest-t? a) (armtest-t? b)))
                                               (lambda (a b) #t))))))
(ok "registered eq arm is consulted" (jolt=2 (make-armtest-t 1) (make-armtest-t 2)))

;; --- the collection and compare registries guard their own fast paths --------
;; Same invariant as eq/hash, same trap: the sets are all different, so each
;; registry has to probe its OWN. jolt-get answers records but not strings;
;; count/empty/seq answer strings but not records; contains? throws for scalars
;; before ever reaching an arm.
(ok "get arm rejects pmap"    (raises? (lambda () (register-get-arm! pmap? (lambda (c k d) d)))))
(ok "get arm rejects jrec"    (raises? (lambda () (register-get-arm! jrec? (lambda (c k d) d)))))
(ok "count arm rejects string" (raises? (lambda () (register-count-arm! string? (lambda (c) 0)))))
(ok "count arm rejects cseq"  (raises? (lambda () (register-count-arm! cseq? (lambda (c) 0)))))
(ok "contains arm rejects number"
    (raises? (lambda () (register-contains-arm! number? (lambda (c k) #f)))))
(ok "contains arm rejects keyword"
    (raises? (lambda () (register-contains-arm! keyword? (lambda (c k) #f)))))
(ok "empty arm rejects string" (raises? (lambda () (register-empty-arm! string? (lambda (c) #t)))))
(ok "seq arm rejects pvec"    (raises? (lambda () (register-seq-arm! pvec? (lambda (x) x)))))
(ok "seq arm rejects string"  (raises? (lambda () (register-seq-arm! string? (lambda (x) x)))))
(ok "compare arm rejects string pair"
    (raises? (lambda () (register-compare-arm! (lambda (a b) (and (string? a) (string? b)))
                                               (lambda (a b) 0)))))
(ok "compare arm rejects char pair"
    (raises? (lambda () (register-compare-arm! (lambda (a b) (and (char? a) (char? b)))
                                               (lambda (a b) 0)))))

;; the sets really do differ — a guard wider than its own fast path would reject
;; arms that are perfectly legal
(ok "get guard allows cseq"     (not (raises? (lambda () (get-arm-reject-fast-type! 'test cseq?)))))
;; get answers a string ahead of its arms, so an arm claiming one is refused
(ok "get arm rejects string"    (raises? (lambda () (register-get-arm! string? (lambda (c k d) d)))))
(ok "count guard allows jrec"   (not (raises? (lambda () (count-arm-reject-fast-type! 'test jrec?)))))
(ok "seq guard allows jrec"     (not (raises? (lambda () (seq-arm-reject-fast-type! 'test jrec?)))))
;; compare's nil clauses are single-sided and answer correctly for EVERY type, so
;; the usual either-arg predicate must stay legal even though it matches them
(ok "compare guard allows either-arg shape"
    (not (raises? (lambda () (compare-arm-reject-fast-type!
                              'test (lambda (a b) (or (armtest-t? a) (armtest-t? b))))))))

;; and real arms on a type off the fast path still register and are consulted
(ok "seq arm on a plain type registers"
    (not (raises? (lambda () (register-seq-arm! armtest-t? (lambda (x) jolt-nil))))))
(ok "count arm on a plain type registers"
    (not (raises? (lambda () (register-count-arm! armtest-t? (lambda (x) 7))))))
(ok "registered count arm is consulted" (= 7 (jolt-count (make-armtest-t 1))))

;; --- a base-vs-base pair never reaches the arms -----------------------------
;; The JVM's Util.equiv has no extension point for two base scalars (nil, a
;; number, keyword, symbol, string, char, boolean): a Keyword is never equal to a
;; String, nil is equal only to nil. jolt=2 used to answer those pairs only after
;; running every registered arm predicate — 145-240 ns per miss with 17 arms in a
;; bare runtime, growing with each library — and `case` lowers to a chain of
;; exactly those compares. They are answered ahead of the walk now, so the
;; registry must refuse an arm that would claim one (the same invariant that
;; keeps the same-type fast pairs honest), and a registered arm must never be
;; consulted for one.
(define eq-arm-calls 0)
(define-record-type armcount-t (fields v) (nongenerative armcount-v1))
(register-eq-arm! (lambda (a b)
                    (set! eq-arm-calls (+ eq-arm-calls 1))
                    (or (armcount-t? a) (armcount-t? b)))
                  (lambda (a b) #t))
(set! eq-arm-calls 0)
(define k-a (keyword #f "a"))
(define mixed-base-pairs
  (list (cons k-a jolt-nil) (cons jolt-nil 5) (cons jolt-nil "s") (cons jolt-nil #f)
        (cons k-a (jolt-symbol #f "a")) (cons (jolt-symbol #f "a") k-a)
        (cons k-a "a") (cons "a" k-a) (cons 5 k-a) (cons 5 5.0) (cons 1/2 0.5)
        (cons #t k-a) (cons #f 0) (cons #\a "a") (cons "1" 1) (cons #\a 97)))
(for-each (lambda (p) (ok "mixed base pair is unequal" (not (jolt=2 (car p) (cdr p)))))
          mixed-base-pairs)
(ok "same-kind base pairs still answer"
    (and (jolt=2 jolt-nil jolt-nil) (jolt=2 #\a #\a) (jolt=2 #t #t) (not (jolt=2 #t #f))
         (jolt=2 1 1) (not (jolt=2 1 2)) (jolt=2 1/2 1/2) (jolt=2 2.5 2.5)))
(ok "no eq arm consulted for base-vs-base pairs" (= eq-arm-calls 0))
(ok "registered eq arm is still consulted for its own type"
    (jolt=2 (make-armcount-t 1) (make-armcount-t 2)))
(ok "eq arm rejects a cross-base pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (and (keyword-t? a) (string? b)))
                                          (lambda (a b) #t)))))
(ok "eq arm rejects a nil-vs-base pair"
    (raises? (lambda () (register-eq-arm! (lambda (a b) (or (jolt-nil? a) (jolt-nil? b)))
                                          (lambda (a b) #t)))))

;; --- first answers a cell / a vector ahead of its arms ----------------------
(define first-arm-calls 0)
(define-record-type firstarm-t (fields v) (nongenerative firstarm-v1))
(register-first-arm! (lambda (x) (set! first-arm-calls (+ first-arm-calls 1)) (firstarm-t? x))
                     (lambda (x) 'arm))
(set! first-arm-calls 0)
(ok "first of a vector is its element 0" (= 1 (jolt-first (jolt-vector 1 2))))
(ok "first of an empty vector is nil" (jolt-nil? (jolt-first (jolt-vector))))
(ok "first of a list cell is its head" (= 3 (jolt-first (jolt-list 3 4))))
(ok "first of nil is nil" (jolt-nil? (jolt-first jolt-nil)))
(ok "no first arm consulted for a vector, a cell or nil" (= first-arm-calls 0))
;; a string is neither, so it asks the arms on its way to its seq
(ok "first of a string reaches its seq" (char=? #\a (jolt-first "ab")))
(ok "registered first arm answers its type" (eq? 'arm (jolt-first (make-firstarm-t 1))))
(ok "first arm rejects vector" (raises? (lambda () (register-first-arm! pvec? (lambda (x) x)))))
(ok "first arm rejects cseq"   (raises? (lambda () (register-first-arm! cseq? (lambda (x) x)))))
(ok "first arm rejects nil"    (raises? (lambda () (register-first-arm! jolt-nil?-fn (lambda (x) x)))))

;; --- counted cells: count without a walk, = by count first ---------------------
;; A PersistentList knows its _count; a StringSeq, an ArraySeq and an RSeq their
;; index; a LongRange its bounds (seq.ss cseqn, range-rest-count). Pinned by
;; allocation, which is deterministic where wall time is not: walking any of these
;; allocates per element (a string's or an rseq's cells, a range's chunks, a
;; vector's seq cells for =), and the O(1) answers allocate nothing per element.
(define (bytes-of thunk)
  (collect)
  (let ((b0 (sstats-bytes (statistics))))
    (thunk)
    (- (sstats-bytes (statistics)) b0)))
(define big-n 100000)
(define big-list (apply jolt-list (iota big-n)))
(ok "a list is a counted cell" (cseqn? big-list))
(ok "count of a list" (= big-n (jolt-count big-list)))
(ok "a list's rest is counted" (and (cseqn? (jolt-rest big-list)) (= (- big-n 1) (jolt-count (jolt-rest big-list)))))
(ok "a list's next is counted" (cseqn? (jolt-next big-list)))
(ok "a list's pop is counted" (and (cseqn? (jolt-pop big-list)) (= (- big-n 1) (jolt-count (jolt-pop big-list)))))
(ok "conj onto a list is counted" (and (cseqn? (jolt-conj1 big-list 'x)) (= (+ big-n 1) (jolt-count (jolt-conj1 big-list 'x)))))
(ok "conj onto () is counted" (and (cseqn? (jolt-conj1 jolt-empty-list 1)) (= 1 (jolt-count (jolt-conj1 jolt-empty-list 1)))))
(ok "(cons x nil) is a counted list" (and (cseqn? (jolt-cons 1 jolt-nil)) (= 1 (jolt-count (jolt-cons 1 jolt-nil)))))
(ok "reverse is a counted list" (let ((r (jolt-reverse (jolt-vector 1 2 3)))) (and (cseqn? r) (= 3 (jolt-count r)))))
;; the count is read from the cell, not walked: a cell claiming 7 behind a tail
;; that throws when forced still counts 7
(ok "count reads a counted cell's field"
    (= 7 (jolt-count (make-cseqn 'x (lambda () (error 'test "walked")) sk-list jolt-nil 7))))
;; a Cons and a lazy cell know no count, so are not counted cells; count still walks them
(ok "cons onto a list is not a counted cell" (not (cseqn? (jolt-cons 0 big-list))))
(ok "count of a cons onto a list" (= (+ big-n 1) (jolt-count (jolt-cons 0 big-list))))
(ok "a lazy cell is not a counted cell" (not (cseqn? (cseq-lazy 1 (lambda () jolt-nil)))))
(ok "count of a list allocates nothing per element" (< (bytes-of (lambda () (jolt-count big-list))) 1024))
(define big-str (make-string big-n #\a))
(ok "count of a string seq" (= big-n (jolt-count (jolt-seq big-str))))
(ok "count of a string seq's next" (= (- big-n 1) (jolt-count (jolt-next (jolt-seq big-str)))))
(ok "count of a string seq allocates nothing per element"
    (< (bytes-of (lambda () (jolt-count (jolt-seq big-str)))) 1024))
(define big-vec (apply jolt-vector (iota big-n)))
(ok "count of an rseq" (= big-n (jolt-count (jolt-rseq big-vec))))
(ok "count of an rseq's next" (= (- big-n 1) (jolt-count (jolt-next (jolt-rseq big-vec)))))
(ok "count of an rseq allocates nothing per element"
    (< (bytes-of (lambda () (jolt-count (jolt-rseq big-vec)))) 1024))
(ok "count of a range" (= 1000000 (jolt-count (jolt-range 1000000))))
(ok "count of a stepped range" (= 14 (jolt-count (jolt-range 3 100 7))))
(ok "count of a descending range" (= 14 (jolt-count (jolt-range 100 3 -7))))
(ok "count of a ratio range" (= 29 (jolt-count (jolt-range 1/2 10 1/3))))
(ok "count of a flonum range walks it" (= 11 (jolt-count (jolt-range 0 1 0.1))))
(ok "count of a range's rest" (= 999999 (jolt-count (jolt-rest (jolt-range 1000000)))))
(ok "count of a range allocates nothing per element"
    (< (bytes-of (lambda () (jolt-count (jolt-range 1000000)))) 4096))
(let ((r (jolt-range 1000)))
  (jolt-count (jolt-vec r))           ; realize every chunk
  (ok "count of a realized range" (= 1000 (jolt-count r))))
;; = compares known counts before walking: a vector against a list one longer
;; answers without building the vector's seq cells
(define big-list+1 (jolt-conj1 big-list 'x))
(ok "= of differing known counts is false" (not (jolt=2 big-vec big-list+1)))
(ok "= of differing known counts does not walk"
    (< (bytes-of (lambda () (jolt=2 big-vec big-list+1))) 1024))
(ok "= of equal counts still walks" (jolt=2 big-vec big-list))
(ok "= of equal counts, differing element" (not (jolt=2 (jolt-vector 1 2 3) (jolt-list 1 2 4))))
(ok "= range against a shorter list" (not (jolt=2 (jolt-range 4) (jolt-list 0 1 2))))
(ok "= string seq against a vector" (jolt=2 (jolt-seq "ab") (jolt-vector #\a #\b)))
(ok "= with a lazy side walks" (not (jolt=2 (cseq-lazy 1 (lambda () jolt-nil)) (jolt-list 1 2))))

(printf "values-test: ~a/~a passed\n" (- total fails) total)
(exit (if (> fails 0) 1 0))
