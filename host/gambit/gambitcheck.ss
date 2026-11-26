;; gambitcheck.ss — the G1 gate for the Gambit adapter (jolt-mj95.2).
;;
;; Run via `make gambitcheck` (detection-gated: skips when gambit-scheme is
;; absent, and always uses $(brew --prefix gambit-scheme)/bin/gsi — NEVER bare
;; gsc/gsi, which is Ghostscript on this machine). Run from the repo root.
;;
;; Loads the three gambit target files (prelude-shims, scheme-adapter-runtime,
;; hasheq), then:
;;   (a) asserts every host/scheme-adapter/CONTRACT.txt name is bound
;;       (or shimmed — variables via eval, syntax shims via the explicit list,
;;       each ALSO verified behaviorally by the unit tests below),
;;   (b) unit-tests every shim group: record round-trip incl. the parent
;;       clause family, hashtable CRUD + entries + weak-eq, fx + bitwise
;;       aliases, condition accessors on a caught error, condvar wait/signal,
;;       parameter fork-inheritance, with-mutex, hasheq known-answer rows
;;       (captured from the Chez build via bin/jolt), and the sa-* raise
;;       surface (message-carrying conditions).
;;
;; The hasheq known-answer rows pin VALUE PARITY with the Chez build: each row
;; is a pure hasheq function whose output was captured from `bin/jolt` on the
;; Chez runtime (the jolt-level (hash ...) routes through exactly these
;; functions). Rows for the vector/map shapes compute the hash-ordered /
;; hash-unordered combinator math directly — the cseq/pmap iteration over them
;; is G2 scope (the full runtime), the murmur3 + combinator math is G1.
;;
;; NO import here: the prelude's (import (except (gambit) define-record-type))
;; is the single import point for the whole boot path. A file-level import in
;; THIS file would create a module boundary and the loaded shim macros
;; (define-record-type, fx aliases, with-mutex, the sa-foreign-* syntaxes)
;; would not resolve here — the (gambit) names come from the prelude's import,
;; which loads into the same top level.

;; ---- splice the three target files ------------------------------------------
;;
;; ##include (NOT load): each loaded file is a SEPARATE compilation unit, and a
;; define-syntax inside a loaded file registers only in its own unit's macro
;; environment — the loading file's LATER forms never see it (defines leak at
;; runtime into the shared global, macros don't). The prelude's record/fx/
;; with-mutex shims are macros, so gambitcheck must expand in the SAME unit as
;; the prelude: ##include textually splices the three files here, exactly the
;; "compile the boot files together" flow the G2 build uses.

(##include "prelude-shims.ss")
(##include "scheme-adapter-runtime.ss")

;; hasheq's seq/symbol arms test jolt-nil? — a values.ss kernel name that only
;; boots in G2. A sentinel stub stands in for this gate; the known-answer rows
;; below never construct jolt-nil, so the stub is only ever asked "is this
;; ordinary value nil" (answer: no). G2's boot manifest loads the real one.
(define %gambitcheck-nil-sentinel (list 'nil))
(define (jolt-nil? x) (eq? x %gambitcheck-nil-sentinel))

(##include "hasheq.ss")

;; ---- test harness ------------------------------------------------------------

(define failures 0)

(define (check label actual expected)
  (if (equal? actual expected)
      (begin (printf "  ok     ~a\n" label) #t)
      (begin (printf "  FAIL   ~a: got ~s expected ~s\n" label actual expected)
             (set! failures (+ failures 1))
             #f)))

(define (check-true label v)
  (if v
      (begin (printf "  ok     ~a\n" label) #t)
      (begin (printf "  FAIL   ~a: got #f\n" label)
             (set! failures (+ failures 1))
             #f)))

(define (check-raise-message label thunk expected-msg)
  (let ((m (guard (e (#t (condition-message e)))
             (thunk)
             "NO-RAISE")))
    (check (string-append label " -> " expected-msg)
           m expected-msg)))

;; ---- (a) contract-name assertion pass ----------------------------------------

;; minimal line parsing, self-contained (mirrors host/scheme-adapter/chez.ss)
(define (char-ws? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
      (char=? c #\return)))

(define (string->tokens s)
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (cond
        ((>= i n) (reverse acc))
        ((char-ws? (string-ref s i)) (loop (+ i 1) acc))
        (else
         (let scan ((j i))
           (if (and (< j n) (not (char-ws? (string-ref s j))))
               (scan (+ j 1))
               (loop j (cons (substring s i j) acc)))))))))

(define (strip-comment s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond
        ((>= i n) s)
        ((or (char=? (string-ref s i) #\#) (char=? (string-ref s i) #\;))
         (substring s 0 i))
        (else (loop (+ i 1)))))))

(define (contract-file-path)
  ;; #f when unreachable — the js-compiled gate has no repo cwd (Gambit-js
  ;; current-directory is not the node process cwd, and command-line carries
  ;; no script path), so the contract-name pass SKIPS there; the unit tests
  ;; are the behavioral proof. Under gsi via make, the repo-root path exists
  ;; and the pass is mandatory.
  (if (file-exists? "host/scheme-adapter/CONTRACT.txt")
      "host/scheme-adapter/CONTRACT.txt"
      (let* ((cl (command-line))
             (sp (and (pair? cl) (pair? (cdr cl)) (cadr cl))))
        (if (and sp (file-exists? sp))
            (let* ((dir (substring sp 0 (- (string-length sp)
                                           (string-length "gambitcheck.ss"))))
                   (root (substring dir 0 (- (string-length dir)
                                             (string-length "host/gambit/")))))
              (string-append root "host/scheme-adapter/CONTRACT.txt"))
            #f))))

(define (read-contract-names path)
  (let ((p (open-input-file path)))
    (let loop ((acc '()))
      (let ((l (get-line p)))
        (if (eof-object? l)
            (begin (close-port p) (reverse acc))
            (let ((toks (string->tokens (strip-comment l))))
              (if (or (null? toks)
                      (and (>= (length toks) 2)
                           (string=? (car toks) "#")
                           (string=? (cadr toks) "tier:")))
                  (loop acc)
                  (loop (cons (string->symbol (car toks)) acc)))))))))

;; Syntax shims the variable-probe below cannot see (eval of a macro name is
;; implementation-defined). Each of these is ALSO verified behaviorally by the
;; unit-test sections — this list is only the CONTRACT-name side of the check.
(define contract-syntax-shims
  '(with-mutex
    sa-foreign-procedure sa-foreign-procedure-native-error
    sa-foreign-procedure-blocking
    sa-foreign-callable sa-foreign-callable-collect-safe
    ;; capability-unchecked: expand to the checked primitives here
    sa-ufx+ sa-ufx- sa-ufx<? sa-ufx>=? sa-ufx=? sa-ufxand sa-ufxxor sa-uvector-ref sa-uvector-set! sa-ustring-ref
    sa-ufx* sa-ufxior sa-ufxsra sa-ufxsll sa-ustring-set!
    sa-uflvector-ref sa-uflvector-set!
    sa-ubytevector-ieee-single-native-ref sa-ubytevector-ieee-single-native-set!
    sa-ubytevector-ieee-double-native-ref sa-ubytevector-ieee-double-native-set!))

(define (bound? s)
  (or (guard (e (#t #f)) (eval s (interaction-environment)) #t)
      (memq s contract-syntax-shims)))

(define (assert-contract-names)
  (printf "== CONTRACT.txt names (bound or shimmed) ==\n")
  (let ((path (contract-file-path)))
    (if (not path)
        (printf "  skip   CONTRACT.txt not reachable (js target has no repo cwd); the unit tests below are the behavioral proof\n")
        (assert-contract-names* path))))

(define (assert-contract-names* path)
  (let ((names (read-contract-names path)))
    (let ((missing (filter (lambda (s) (not (bound? s))) names)))
      (for-each (lambda (s) (printf "  NOT-BOUND ~a\n" s)) missing)
      (printf "  ~a contract names checked against the combined env\n"
              (length names))
      (if (null? missing)
          (printf "  ok     all contract names bound-or-shimmed\n")
          (begin (printf "  FAIL   ~a contract name(s) not bound\n"
                         (length missing))
                 (set! failures (+ failures 1)))))))

;; ---- (b) unit tests ----------------------------------------------------------

(define (test-records)
  (define-record-type animal (fields name) (nongenerative gambitcheck-animal-v1))
  (define-record-type dog (parent animal) (fields (mutable age)) (nongenerative gambitcheck-dog-v1))
  (printf "== records: define-record-type round-trip incl. parent clause ==\n")
  (check "ctor + accessors" (let ((d (make-dog "rex" 3))) (list (animal-name d) (dog-age d)))
         '("rex" 3))
  (check "predicate on child" (dog? (make-dog "rex" 3)) #t)
  (check "base predicate inclusive of child" (animal? (make-dog "rex" 3)) #t)
  (check "child predicate excludes base instance" (dog? (make-animal "rex")) #f)
  (check "parent accessor on child" (animal-name (make-dog "rex" 3)) "rex")
  (check "mutable field set!" (let ((d (make-dog "rex" 3))) (dog-age-set! d 4) (dog-age d)) 4)
  (check "immutable field has no setter (unbound)" (not (guard (e (#t #f)) (eval 'animal-name-set! (interaction-environment)) #t)) #t)
  (check "wrong-arity ctor raises" (guard (e (#t (condition-message e))) (make-dog "rex") "no-raise") "wrong number of arguments"))

(define (test-hashtables)
  (printf "== hashtable shim over tables ==\n")
  (let ((h (make-eq-hashtable)))
    (hashtable-set! h 'a 1)
    (check "ref after set!" (hashtable-ref h 'a #f) 1)
    (check "missing ref -> default" (hashtable-ref h 'zzz #f) #f)
    (check "contains? after set!" (hashtable-contains? h 'a) #t)
    (check "size" (hashtable-size h) 1)
    (hashtable-delete! h 'a)
    (check "contains? after delete!" (hashtable-contains? h 'a) #f)
    (check "size after delete!" (hashtable-size h) 0))
  (let ((h (make-eq-hashtable)))
    (hashtable-set! h 'a 1)
    (hashtable-set! h 'b 2)
    (check "keys is a vector" (vector? (hashtable-keys h)) #t)
    ;; iteration order is unspecified on BOTH hosts — compare as a sorted set
    (check "keys contents" (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) (vector->list (hashtable-keys h))) '(a b))
    (check "entries -> two parallel vectors (order-independent, aligned)"
           (call-with-values (lambda () (hashtable-entries h))
             (lambda (ks vs)
               (list-sort (lambda (x y) (< (cdr x) (cdr y)))
                          (map cons (vector->list ks) (vector->list vs)))))
           '((a . 1) (b . 2))))
  (let ((h (make-eq-hashtable)))
    (hashtable-set! h 'a 1)
    (hashtable-set! h 'b 2)
    (check "CONTRACT misc hashtable-values" (list-sort < (vector->list (hashtable-values h))) '(1 2))
    (check "CONTRACT misc hashtable-cells" (length (hashtable-cells h)) 2))
  (let ((h (make-hashtable string-hash string=?)))
    (hashtable-set! h "k" 1)
    (check "make-hashtable (hash+equiv dropped, equal? test)" (hashtable-ref h "k" #f) 1))
  (let ((w (make-weak-eq-hashtable)))
    (let ((k (list 1)))
      (hashtable-set! w k 'v)
      (check "weak-eq set!/ref" (hashtable-ref w k #f) 'v))))

(define (test-fx-aliases)
  (printf "== fx + bitwise spelling aliases ==\n")
  (check "fx=? alias" (fx=? 1 1) #t)
  (check "fx<? alias" (fx<? 1 2) #t)
  (check "fx>? alias" (fx>? 2 1) #t)
  (check "fxsll alias" (fxsll 1 4) 16)
  (check "fxsra alias" (fxsra 8 1) 4)
  (check "native fx+ still bound" (fx+ 1 2) 3)
  ;; capability-unchecked: on this target each is the checked primitive
  (check "sa-ufx+" (sa-ufx+ 1 2) 3)
  (check "sa-ufx-" (sa-ufx- 5 2) 3)
  (check "sa-ufx<?" (sa-ufx<? 1 2) #t)
  (check "sa-ufx>=?" (sa-ufx>=? 2 2) #t)
  (check "sa-ufx=?" (sa-ufx=? 3 3) #t)
  (check "sa-ufxand" (sa-ufxand 12 10) 8)
  (check "sa-ufxxor" (sa-ufxxor 12 10) 6)
  (check "sa-ufx*" (sa-ufx* 6 7) 42)
  (check "sa-ufxior" (sa-ufxior 12 10) 14)
  (check "sa-ufxsra" (sa-ufxsra 40 3) 5)
  (check "sa-ufxsll" (sa-ufxsll 5 3) 40)
  (check "sa-ustring-set!" (let ((s (make-string 2 #\a))) (sa-ustring-set! s 1 #\b) s) "ab")
  (check "sa-uvector-ref" (sa-uvector-ref (vector 1 2 3) 1) 2)
  (check "sa-uvector-set!" (let ((v (vector 1 2 3))) (sa-uvector-set! v 1 9) (vector-ref v 1)) 9)
  (check "sa-ustring-ref" (sa-ustring-ref "abc" 1) #\b)
  (check "sa-uflvector-set!/-ref" (let ((v (make-f64vector 3 0.))) (sa-uflvector-set! v 1 2.5) (sa-uflvector-ref v 1)) 2.5)
  ;; native (little-endian) IEEE patterns: 1.5 is #x3fc00000 single, #x3ff8000000000000 double
  (check "sa-ubytevector-ieee-single-native-set!"
         (let ((bv (make-u8vector 6 0))) (sa-ubytevector-ieee-single-native-set! bv 2 1.5) bv)
         (u8vector 0 0 0 0 #xc0 #x3f))
  (check "sa-ubytevector-ieee-single-native-ref"
         (sa-ubytevector-ieee-single-native-ref (u8vector 9 0 0 #xc0 #x3f) 1) 1.5)
  (check "sa-ubytevector-ieee-double-native-set!/-ref round-trip"
         (let ((bv (make-u8vector 8 0))) (sa-ubytevector-ieee-double-native-set! bv 0 -0.1)
           (list (u8vector-ref bv 7) (sa-ubytevector-ieee-double-native-ref bv 0)))
         (list #xbf -0.1))
  (check "sa-vector-copy-range! (R7RS shape)"
         (let ((to (make-vector 5 0))) (sa-vector-copy-range! to 1 (vector 7 8 9) 1 3) to)
         (vector 0 8 9 0 0))
  ;; the string twin is the subs fast path (converters.ss, shared with this
  ;; host); the raw string-copy! it replaced has the OPPOSITE argument order
  ;; here, so a wrong adapter reads back "hello" untouched and a blank span.
  (check "sa-string-copy-range! (R7RS shape)"
         (let ((from (string-copy "hello world")) (to (make-string 5 #\-)))
           (sa-string-copy-range! to 0 from 6 11)
           (list to from))
         (list "world" "hello world"))
  (check "bitwise-arithmetic-shift-left (natives-num.ss spelling)" (bitwise-arithmetic-shift-left 1 40) 1099511627776)
  (check "bitwise-arithmetic-shift-right floor on negative" (bitwise-arithmetic-shift-right -7 1) -4))

(define (test-conditions)
  (printf "== Chez condition accessors over error objects ==\n")
  (let ((c (guard (e (#t e)) (error 'who "boom" 1 2))))
    (check "condition? on error object" (condition? c) #t)
    (check "message-condition? on error object" (message-condition? c) #t)
    (check "condition-message" (condition-message c) "boom")
    (check "condition-irritants" (condition-irritants c) '(1 2)))
  (check "condition? on non-error" (condition? 42) #f)
  (check "message-condition? on non-error" (message-condition? 42) #f)
  (check "condition-message on non-error -> #f" (condition-message 42) #f)
  (check "condition-irritants on non-error -> '()" (condition-irritants 42) '()))

(define (test-threads)
  (printf "== thread mappings ==\n")
  ;; condvar wait/signal round-trip. got is a SHARED mutable cell — a
  ;; thread-parameter would be wrong here: the worker's set lands in its own
  ;; dynamic environment (inheritance copies parent->child at fork, never
  ;; back), the main thread never observes it, and the wait deadlocks.
  (let ((m (make-mutex))
        (cv (make-condition))
        (got (vector #f)))
    (define w (fork-thread
               (lambda ()
                 (mutex-lock! m)
                 (vector-set! got 0 #t)
                 (condition-signal cv)
                 (mutex-unlock! m))))
    (mutex-lock! m)
    (if (not (vector-ref got 0)) (condition-wait cv m))
    (mutex-unlock! m)
    (thread-join! w)
    (check "condvar wait/signal round-trip" (vector-ref got 0) #t))
  ;; condition-wait with a future timeout returns (mutex re-acquired)
  (let ((m (make-mutex))
        (cv (make-condition))
        (t0 (sa-real-time-ms)))
    (mutex-lock! m)
    (condition-wait cv m (+ (time->seconds (current-time)) 0.05))
    (mutex-unlock! m)
    (check-true "condition-wait timeout returns, mutex re-acquired"
                (< (- (sa-real-time-ms) t0) 5000)))
  ;; with-mutex macro: body value + no deadlock
  (check "with-mutex returns body value"
         (with-mutex (make-mutex) 42) 42)
  ;; parameter fork-inheritance (the G0 pin)
  (let ((p (make-thread-parameter 1))
        (result (make-thread-parameter #f)))
    (p 2)
    (let ((t (fork-thread (lambda () (result (p))))))
      (thread-join! t)
      (check "parameter fork-inheritance: child sees parent's current value" (result) 2)))
  ;; virtual registers are PER THREAD and a fresh thread starts every slot at
  ;; fixnum 0 -- Chez's contract, which the runtime leans on for anything a
  ;; child must not inherit (an interrupt box, a per-thread cache). Probed on
  ;; Chez 10.x: the child of a thread that wrote slot 12 reads 0 there.
  (let ((seen (vector 'unset 'unset)))
    (set-virtual-register! 12 'parent)
    (let ((t (fork-thread (lambda ()
                            (vector-set! seen 0 (virtual-register 12))
                            (set-virtual-register! 12 'child)
                            (vector-set! seen 1 (virtual-register 12))))))
      (thread-join! t)
      (check "virtual-register: a fresh thread starts a written slot at 0" (vector-ref seen 0) 0)
      (check "virtual-register: the child's write is its own" (vector-ref seen 1) 'child)
      (check "virtual-register: the parent's slot is untouched" (virtual-register 12) 'parent)
      (set-virtual-register! 12 0)))
  ;; get-thread-id: distinct numbers for live threads
  (let ((ids (make-table test: eq?)))
    (let ((id1 (get-thread-id))
          (t2 (fork-thread (lambda () (table-set! ids 't2 (get-thread-id))))))
      (thread-join! t2)
      (check "get-thread-id is a number" (and (number? id1) (number? (table-ref ids 't2 #f))) #t)
      (check "get-thread-id distinct per thread" (not (= id1 (table-ref ids 't2 #f))) #t))))

(define (test-hasheq-known-answers)
  (printf "== hasheq known answers (captured from the Chez build via bin/jolt) ==\n")
  ;; fixnums -> murmur3-hash-long-flat (jolt-hasheq fixnum arm)
  (check "hash 42  (Chez 1871679806)" (murmur3-hash-long-flat 42) 1871679806)
  (check "hash -7  (Chez -1703207563)" (murmur3-hash-long-flat -7) -1703207563)
  (check "hash 1000000000000  (Chez -1510912948)" (murmur3-hash-long-flat 1000000000000) -1510912948)
  (check "murmur3-hash-long agrees with flat on fixnum" (murmur3-hash-long 1000000000000) -1510912948)
  ;; strings -> string-hasheq (java-string-hashcode + murmur3-hash-int)
  (check "hash \"hello\"  (Chez 1715862179)" (string-hasheq "hello") 1715862179)
  (check "hash long string  (Chez 237270814)" (string-hasheq "abcdefghijklmnopqrstuvwxyz0123456789") 237270814)
  (check "hash longer string  (Chez 1189256432)"
         (string-hasheq "a-really-long-string-that-is-definitely-more-than-fifteen-characters-long-for-sure")
         1189256432)
  ;; keywords -> compute-keyword-hasheq (the keyword-t khash field)
  (check "hash :kw  (Chez 1158308175)" (compute-keyword-hasheq #f "kw") 1158308175)
  (check "hash :a/b  (Chez 1482224565)" (compute-keyword-hasheq "a" "b") 1482224565)
  ;; symbols -> compute-symbol-hasheq
  (check "hash 'sym  (Chez 195671222)" (compute-symbol-hasheq '() "sym") 195671222)
  (check "hash 'foo/bar  (Chez 254379989)" (compute-symbol-hasheq "foo" "bar") 254379989)
  ;; doubles -> double-hasheq
  (check "hash 0.5  (Chez 1071644672)" (double-hasheq 0.5) 1071644672)
  (check "hash 2.5  (Chez 1074003968)" (double-hasheq 2.5) 1074003968)
  (check "hash 0.0  (Chez 0)" (double-hasheq 0.0) 0)
  (check "hash -0.0  (Chez 0)" (double-hasheq -0.0) 0)
  ;; bignum / ratio -> big-integer-hashcode
  (check "hash 123456789012345678901234567890  (Chez 1915528825)"
         (big-integer-hashcode 123456789012345678901234567890) 1915528825)
  (check "hash 1/3  (Chez 2: bigint(1) ^ bigint(3))"
         (bitwise-xor (big-integer-hashcode 1) (big-integer-hashcode 3)) 2)
  ;; collection combinators: the hash-ordered / hash-unordered math behind
  ;; [1 2 3] and {:a 1} (cseq/pmap iteration is G2; the murmur3 + combinator
  ;; math is exactly what this port owns)
  (check "hash [1 2 3]  (Chez 736442005)"
         (let* ((h1 (i32 (+ 31 (murmur3-hash-long-flat 1))))
                (h2 (i32 (+ (* 31 h1) (murmur3-hash-long-flat 2))))
                (h3 (i32 (+ (* 31 h2) (murmur3-hash-long-flat 3)))))
           (mix-coll-hash h3 3))
         736442005)
  (check "hash {:a 1}  (Chez 1772842048)"
         (let* ((ka (compute-keyword-hasheq #f "a"))
                (h1 (i32 (+ 31 ka)))
                (h2 (i32 (+ (* 31 h1) (murmur3-hash-long-flat 1))))
                (e (mix-coll-hash h2 2)))
           (mix-coll-hash e 1))
         1772842048))

(define (test-sa-surface)
  (printf "== sa-* runtime surface ==\n")
  (check "sa-host-tag" (sa-host-tag) "gambit")
  (check "sa-os-family (documented else-default)" (sa-os-family) 'linux)
  (check "sa-arch (degraded)" (sa-arch) 'other)
  (check "sa-endian" (sa-endian) 'little)
  (check "sa-real-time-ms is an exact integer" (exact-integer? (sa-real-time-ms)) #t)
  (check "sa-real-time-ms advances"
         (let ((a (sa-real-time-ms)))
           (thread-sleep! 0.01)
           (>= (sa-real-time-ms) a)) #t)
  ;; mtime needs a real file; under the js exe there is no repo cwd (same
  ;; detection as the contract pass), so these two rows skip there
  (if (contract-file-path)
      (begin
        (check "sa-file-mtime-ms on an existing file"
               (exact-integer? (sa-file-mtime-ms "host/scheme-adapter/CONTRACT.txt")) #t)
        (check "sa-file-mtime-ms is a plausible epoch-ms (> 1e12)"
               (> (sa-file-mtime-ms "host/scheme-adapter/CONTRACT.txt") 1000000000000) #t))
      (printf "  skip   sa-file-mtime-ms rows (no repo cwd under the js exe)\n"))
  (check "sa-introspect-enabled? fixed #f" (sa-introspect-enabled?) #f)
  (check "sa-continuation-frames -> '()" (sa-continuation-frames #f) '())
  (check "sa-procedure-info -> #f" (sa-procedure-info (lambda () 1)) #f)
  (check "sa-stats zero vector" (sa-stats) #(0 0 0 0 0 0 0))
  (check "sa-profile-instrument! refuses on" (sa-profile-instrument! #t) #f)
  (check "sa-profile-instrument! off succeeds" (sa-profile-instrument! #f) #t)
  (check "sa-profile-instrumenting? fixed #f" (sa-profile-instrumenting?) #f)
  (check "sa-profile-compile calls its thunk" (sa-profile-compile (lambda () 7)) 7)
  (check "sa-profile-counts -> '()" (sa-profile-counts) '())
  (check "sa-profile-release! -> '()" (sa-profile-release!) '())
  (check "sa-block-profile-dump! -> 0" (sa-block-profile-dump! "/nonexistent/p.jpd" "release") 0)
  (check "sa-block-profile-load-unit! -> #f" (sa-block-profile-load-unit! '() "x.ss") #f)
  (check "sa-stack-walk -> #f" (sa-stack-walk #f 1 (lambda (c o f) #t)) #f)
  (check "sa-frame-source-pair -> #f" (sa-frame-source-pair #f) #f)
  (check "sa-code-name -> #f" (sa-code-name #f) #f)
  (check "sa-thread-cpu-nanos -> 0" (sa-thread-cpu-nanos) 0)
  (check "sa-baked-global -> #f always" (sa-baked-global 'jolt-baked-version-early) #f)
  (check "sa-gc-collect no-ops" (sa-gc-collect) #f)
  (check "sa-gc-max-generation 0" (sa-gc-max-generation) 0)
  ;; the ffi tier: EVERY entry raises a message-carrying condition
  (check-raise-message "sa-foreign-alloc" (lambda () (sa-foreign-alloc 16))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-procedure (syntax)" (lambda () (sa-foreign-procedure "f" (int) int))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-procedure-native-error (syntax)"
                       (lambda ()
                         (sa-foreign-procedure-native-error unsupported-native-error
                                                            () "f" (int) int))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "jolt-ffi-native-error-procedure (target wrapper)"
                       (lambda ()
                         (jolt-ffi-native-error-procedure () "f" (int) int))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-procedure-blocking (syntax)" (lambda () (sa-foreign-procedure-blocking "f" (int) int))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-callable (syntax)" (lambda () (sa-foreign-callable (lambda () 1) (int) int))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-callable-collect-safe (syntax)" (lambda () (sa-foreign-callable-collect-safe (lambda () 1) (int) int))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-load-shared-object" (lambda () (sa-load-shared-object "libx"))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-lock-object" (lambda () (sa-lock-object (list 1)))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-entry?" (lambda () (sa-foreign-entry? "f"))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-foreign-entry-address" (lambda () (sa-foreign-entry-address "f"))
                       "ffi is unsupported on the gambit target")
  (check-raise-message "sa-compile-file" (lambda () (sa-compile-file "a.ss" "a.so" #f))
                       "native compilation is unsupported on the gambit target")
  (check-raise-message "sa-make-boot-file" (lambda () (sa-make-boot-file "out.boot" '()))
                       "native compilation is unsupported on the gambit target")
  (check-raise-message "sa-fasl-write" (lambda () (sa-fasl-write 42 (open-output-string)))
                       "fasl serialization is unsupported on the gambit target")
  (check-raise-message "sa-fasl-read" (lambda () (sa-fasl-read (open-input-string "")))
                       "fasl serialization is unsupported on the gambit target")
  (check-raise-message "sa-run-process" (lambda () (sa-run-process "echo hi" #f))
                       "subprocess support is unsupported on the gambit target"))

;; ---- continuations tier — the one-shot escape primitive ---------------------
;; The gambit target IMPLEMENTS this tier rather than degrading, so these rows
;; assert real behaviour, not an honest-failure message. The two refusals are
;; the whole reason the adapter wraps call/cc at all: call/cc is multi-shot and
;; would happily graft control back into a frame that already finished.
(define (test-continuations)
  (printf "== continuations: one-shot escape (call/cc + spent flag) ==\n")
  (check "escape returns its value"
         (sa-call-with-escape-continuation (lambda (k) (k 'escaped) 'not-reached))
         'escaped)
  (check "falling through returns the body value"
         (sa-call-with-escape-continuation (lambda (k) 'fell-through))
         'fell-through)
  (check "escape leaves a loop from depth"
         (sa-call-with-escape-continuation
          (lambda (k) (let loop ((i 0)) (if (= i 100) (k i) (loop (+ i 1))))))
         100)
  ;; An escape is a real exit: the dynamic-wind chain between the capture and
  ;; the escape unwinds, which is what makes a jolt `finally` run.
  (check "escape unwinds dynamic-wind"
         (let ((log '()))
           (sa-call-with-escape-continuation
            (lambda (k)
              (dynamic-wind
                (lambda () (set! log (cons 'in log)))
                (lambda () (k 'out))
                (lambda () (set! log (cons 'out log))))))
           (reverse log))
         '(in out))
  (check-raise-message "second invocation refused"
                       (lambda ()
                         (let ((saved #f))
                           (sa-call-with-escape-continuation
                            (lambda (k) (set! saved k) (k 'first)))
                           (saved 'again)))
                       "escape continuation is spent")
  (check-raise-message "invocation after a normal return refused"
                       (lambda ()
                         (let ((saved #f))
                           (sa-call-with-escape-continuation
                            (lambda (k) (set! saved k) 'fell-through))
                           (saved 'too-late)))
                       "escape continuation is spent")
  (check "a fresh capture after a spent one still escapes"
         (let ((saved #f))
           (sa-call-with-escape-continuation (lambda (k) (set! saved k) (k 'first)))
           (sa-call-with-escape-continuation (lambda (k) (k 'second))))
         'second))

;; ---- coroutines tier (fibers R1) — the call/cc-based fiber primitive --------
;; Mirrors the Chez gate's correctness set at small scale: round trip,
;; round-robin order, per-fiber raise isolation with a surviving scheduler,
;; raise-after-park (the guard must ride the continuation), deep-stack yield.
(define (test-coroutines)
  (define (all-done? ls)
    (or (null? ls) (and (eq? (jolt-fiber-state (car ls)) 'done) (all-done? (cdr ls)))))
  (define (deep-yield n)
    (if (= n 0) (sa-fiber-yield) (deep-yield (- n 1))))
  (printf "== coroutines: fiber primitive (call/cc, R1) ==\n")
  ;; round trip: body runs before/after a yield; fiber completes with its value
  (let ((log '()))
    (define f (sa-fiber-spawn (lambda ()
                                (set! log (cons 'a log))
                                (sa-fiber-yield)
                                (set! log (cons 'b log))
                                'rt-done)))
    (sa-fiber-run-all)
    (check "round trip: body order" (reverse log) '(a b))
    (check "round trip: state done" (jolt-fiber-state f) 'done)
    (check "round trip: result" (jolt-fiber-result f) 'rt-done))
  ;; round robin: N fibers each yield M times in strict rotation
  (let ((log '()) (N 4) (M 3))
    (define fs
      (map (lambda (i)
             (sa-fiber-spawn
               (lambda ()
                 (let loop ((m M))
                   (if (> m 0)
                       (begin (set! log (append log (list i)))
                              (sa-fiber-yield)
                              (loop (- m 1)))
                       #f)))))
           '(0 1 2 3)))
    (sa-fiber-run-all)
    (check "round-robin: strict rotation"
           (let loop ((k 0))
             (if (>= k (* N M))
                 #t
                 (and (= (list-ref log k) (modulo k N)) (loop (+ k 1)))))
           #t)
    (check "round-robin: all done" (all-done? fs) #t))
  ;; raise isolation: one fiber raises, the scheduler survives, others run
  (let ((log '()))
    (define fibers
      (map (lambda (i)
             (sa-fiber-spawn
               (lambda ()
                 (if (= i 1)
                     (error 'fiber-test "boom")
                     (set! log (cons i log))))))
           '(0 1 2 3)))
    (define f2 (sa-fiber-spawn (lambda () 'ok)))
    (sa-fiber-run-all)
    (check "raise: non-raising fibers ran" (length log) 3)
    (check "raise: raising fiber dead" (jolt-fiber-state (list-ref fibers 1)) 'dead)
    (check "raise: error recorded"
           (condition-message (jolt-fiber-error (list-ref fibers 1))) "boom")
    (sa-fiber-run-all)
    (check "raise: scheduler reusable" (jolt-fiber-result f2) 'ok))
  ;; raise AFTER a park: the resume-path guard must ride the continuation
  (let ((f (sa-fiber-spawn (lambda () (sa-fiber-yield) (error 'fiber-test "late")))))
    (sa-fiber-run-all)
    (check "raise after park: dead" (jolt-fiber-state f) 'dead))
  ;; yield from 20 frames down
  (let ((f (sa-fiber-spawn (lambda () (deep-yield 20) 'deep-ok))))
    (sa-fiber-run-all)
    (check "deep yield: result" (jolt-fiber-result f) 'deep-ok)))

;; ---- main --------------------------------------------------------------------

(assert-contract-names)
(test-records)
(test-hashtables)
(test-fx-aliases)
(test-conditions)
(test-threads)
(test-hasheq-known-answers)
(test-sa-surface)
(test-continuations)
(test-coroutines)

(printf "\ngambitcheck: ~a failure(s)\n" failures)
(if (= failures 0)
    (begin (printf "gambitcheck: PASS — gambit adapter + shims verified on native gsi\n")
           (exit 0))
    (begin (printf "gambitcheck: FAILED\n")
           (exit 1)))
