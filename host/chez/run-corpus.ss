;; run-corpus.ss — the standing correctness gate, pure Chez. NO Janet.
;;
;; Loads the checked-in seed (host/chez/seed/{prelude,image}.ss) + the zero-Janet
;; spine, reads test/chez/corpus.edn, and for each row evaluates :actual and
;; :expected through jolt-compile-eval and compares by value-equality (jolt=). The
;; corpus :expected is JVM-sourced (test/conformance/regen-corpus.clj), so this
;; measures jolt-on-Chez against reference Clojure.
;;
;; Each case runs as its own top-level program (a top-level do unrolls, so a macro
;; defined earlier in the program is usable later), and mutable global state is
;; reset between cases so there is no leakage — same isolation a fresh process gives.
;;
;;   chez --script host/chez/run-corpus.ss
;;   JOLT_CHEZ_ZJ_FLOOR=N   override the regression floor (default 2691)
;;   JOLT_CORPUS_LIMIT=N    every-Nth stride (fast iteration; floor drops to 0)
;;   JOLT_DUMP_CRASH_LABELS=1   list crash + allowlisted labels
(import (chezscheme))

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define (slurp path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((cs '()) (c (read-char p)))
        (if (eof-object? c) (list->string (reverse cs))
            (loop (cons c cs) (read-char p)))))))

(define corpus (jolt-read-string (slurp "test/chez/corpus.edn")))
(define kw-label    (keyword #f "label"))
(define kw-actual   (keyword #f "actual"))
(define kw-expected (keyword #f "expected"))
(define kw-throws   (keyword #f "throws"))

;; --- per-case isolation: snapshot the world after setup, restore it each case ----
;; (1) var-table keys a case ADDS (its defs) are removed; (2) a base cell whose ROOT
;; a case mutated (e.g. in-ns rebinds clojure.core/*ns*) is restored; (3) the ns +
;; type registries are pruned to their base keys; (4) global-hierarchy's contents
;; (mutated by derive) are reset to a fresh hierarchy.
(define zj-base (let ((h (make-hashtable string-hash string=?)))
  (vector-for-each (lambda (k) (hashtable-set! h k #t)) (hashtable-keys var-table)) h))
(define zj-roots '())
(vector-for-each (lambda (k) (let ((c (hashtable-ref var-table k #f)))
                   (when c (set! zj-roots (cons (cons c (var-cell-root c)) zj-roots)))))
                 (hashtable-keys var-table))
(define (zj-snap ht) (let ((h (make-hashtable string-hash string=?)))
  (vector-for-each (lambda (k) (hashtable-set! h k #t)) (hashtable-keys ht)) h))
(define (zj-prune! ht base) (vector-for-each
  (lambda (k) (unless (hashtable-ref base k #f) (hashtable-delete! ht k))) (hashtable-keys ht)))
(define zj-ns-base (zj-snap ns-registry))
(define zj-type-base (zj-snap type-registry))
(define zj-ghier (var-cell-lookup "clojure.core" "global-hierarchy"))
(define (zj-reset!)
  (vector-for-each (lambda (k) (unless (hashtable-ref zj-base k #f) (hashtable-delete! var-table k)))
                   (hashtable-keys var-table))
  (for-each (lambda (cr) (unless (eq? (var-cell-root (car cr)) (cdr cr))
                           (var-cell-root-set! (car cr) (cdr cr)))) zj-roots)
  (zj-prune! ns-registry zj-ns-base)
  (zj-prune! type-registry zj-type-base)
  (hashtable-clear! ns-alias-table)
  (hashtable-clear! ns-refer-table)
  (hashtable-clear! ns-refer-all-table)
  (when zj-ghier (jolt-invoke (var-deref "clojure.core" "reset!")
                   (var-cell-root zj-ghier) (jolt-invoke (var-deref "clojure.core" "make-hierarchy"))))
  (set-chez-ns! "user"))

(define kw-message (keyword #f "message"))
(define (zj-err->str e)
  (cond ((and (pmap? e) (string? (jolt-get e kw-message))) (jolt-get e kw-message))
        ((condition? e) (call-with-string-output-port (lambda (p) (display-condition e p))))
        ((string? e) e)
        (else (call-with-string-output-port (lambda (p) (write e p))))))
(define (zj-clean s)
  (list->string (map (lambda (c) (if (or (char=? c #\tab) (char=? c #\newline)) #\space c))
                     (string->list s))))

;; --- allowlist: conformance gaps vs the JVM spec (no JVM host on Chez) -----------
;; Keyed by label. jolt does not match because it has no Class objects / Java arrays
;; / BigDecimal, supports the :jolt reader-conditional, or prints its own forms for
;; transients/atoms/Infinity. These DIVERGE but are tolerated; the gate fails only on
;; a NEW (unlisted) divergence or a drop below the floor.
(define known-fail-labels
  '("class name evaluates to canonical string"
    "class number" "class string" "class keyword"
    "definterface defines" "getMessage on a thrown string"
    "type of record" "chunked-seq? always false"
    "^Type tag on var" "symbol hint -> :tag"
    "lists extended type" "seq of tags"
    "close on throw" "macroexpand-1" "ns-imports empty user"
    "bean is the map" "proxy resolves nil" "unchecked-char"
    "*in* is bound" "*in* bound"
    "bigdec" "bigdec int M" "bigdec suffix M"
    "transient vector" "transient map"
    "atom override fires nested" "inf inside coll" "pr-str Infinity"
    "defmethod overrides a record, top level"
    "defmethod fires nested in a map" "defmethod fires through prn"
    "direct builtin override" "methods table inspectable"
    "reader conditional" "reader cond :jolt" "reader cond no match"
    "reader cond splice" "reader cond splice no match"
    "nil nested" "bool nested" "source order through syntax-quote"
    "make-array" "into-array" "to-array" "aclone vec"
    "boolean-array" "int-array" "long-array" "double-array"
    "float-array" "short-array" "doubles" "floats" "reader over char[]"
    "char-array of string"
    "atom?" "instance? Atom"
    "cancel an in-flight future returns true" "future-cancelled? after cancel"
    "no param vector"))
(define known-fail (make-hashtable string-hash string=?))
(for-each (lambda (l) (hashtable-set! known-fail l #t)) known-fail-labels)

;; Cases that BLOCK forever on a shared-heap host (deref of an undelivered promise) —
;; skip like :throws so one hung case can't stall the run.
(define skip-blocking (make-hashtable string-hash string=?))
(hashtable-set! skip-blocking "promise undelivered" #t)

;; Coarse crash bucket for the punch-list (informational; not gate-critical).
(define (crash-reason m)
  (define (has? sub) (let loop ((i 0))
    (cond ((> (+ i (string-length sub)) (string-length m)) #f)
          ((string=? (substring m i (+ i (string-length sub))) sub) #t)
          (else (loop (+ i 1))))))
  (cond ((has? "unsupported stdlib") "emit: unsupported stdlib fn")
        ((has? "unsupported host") "emit: unsupported host call")
        ((has? "host-static") "emit: host-static")
        ((has? "uncompil") "analyzer: uncompilable")
        ((has? "Unknown class") "runtime: unknown class")
        ((has? "No constructor") "runtime: no constructor")
        ((has? "No method") "runtime: no method")
        ((has? "not a fn") "runtime: not a fn")
        ((has? "not seqable") "runtime: not seqable")
        (else (substring m 0 (min 56 (string-length m))))))

;; --- run ------------------------------------------------------------------------
(define limit (let ((s (getenv "JOLT_CORPUS_LIMIT")))
                (and s (string->number s))))
(define stride (if (and limit (> limit 0)) (max 1 (quotient (pvec-count corpus) limit)) 1))

(define pass 0) (define throws 0)
(define crashes '())       ; (label . reason)
(define diverged '())      ; (label . got)  — NEW divergence; gate fails
(define known-hit '())     ; label
(define crash-keys (make-hashtable string-hash string=?))
(define (bucket! ht k) (hashtable-set! ht k (+ 1 (hashtable-ref ht k 0))))

(define t0 (current-time))
(let loop ((i 0))
  (when (< i (pvec-count corpus))
    (let* ((row (pvec-nth-d corpus i jolt-nil))
           (label (jolt-get row kw-label))
           (ev-src (jolt-get row kw-expected))
           (av-src (jolt-get row kw-actual)))
      (cond
        ((or (eq? ev-src kw-throws) (hashtable-ref skip-blocking label #f))
         (set! throws (+ throws 1)))
        (else
         (guard (e (#t (let ((r (crash-reason (zj-clean (zj-err->str e)))))
                         (bucket! crash-keys r)
                         (set! crashes (cons (cons label r) crashes)))))
           ;; discard a case's own stdout (a (println ...) side effect) so it can't
           ;; pollute the gate report — as if the case ran in its own process.
           (let* ((sink (open-output-string))
                  (av (parameterize ((current-output-port sink)) (jolt-compile-eval av-src "user")))
                  (ev (parameterize ((current-output-port sink)) (jolt-compile-eval ev-src "user"))))
             (cond
               ((jolt= ev av) (set! pass (+ pass 1)))
               ((hashtable-ref known-fail label #f) (set! known-hit (cons label known-hit)))
               (else (set! diverged (cons (cons label (zj-clean (jolt-final-str av))) diverged))))))
         (zj-reset!))))
    (loop (+ i stride))))

(define n-eval (+ pass (length crashes) (length diverged) (length known-hit)))
(define secs (let ((d (time-difference (current-time) t0)))
               (+ (time-second d) (/ (time-nanosecond d) 1e9))))
(printf "\nZero-Janet corpus parity: ~a/~a evaluated cases pass  (~as)\n"
        pass n-eval (/ (round (* secs 10)) 10.0))
(printf "  crash: ~a   NEW divergence: ~a   known: ~a   (throws skipped: ~a)\n"
        (length crashes) (length diverged) (length known-hit) throws)

(when (> (hashtable-size crash-keys) 0)
  (printf "\ncrash reasons:\n")
  (let-values (((ks vs) (hashtable-entries crash-keys)))
    (for-each (lambda (pair) (printf "  ~a x  ~a\n" (cdr pair) (car pair)))
              (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                         (vector->list (vector-map cons ks vs))))))
(when (getenv "JOLT_DUMP_CRASH_LABELS")
  (printf "\nCRASH LABELS:\n")
  (for-each (lambda (p) (printf "  [~a] :: ~a\n" (cdr p) (car p)))
            (list-sort (lambda (a b) (string<? (cdr a) (cdr b))) crashes))
  (printf "\nKNOWN-HIT LABELS:\n")
  (for-each (lambda (l) (printf "  ~a\n" l)) (list-sort string<? known-hit)))
(when (> (length diverged) 0)
  (printf "\nNEW divergences (ran, wrong value) — gate FAILS:\n")
  (for-each (lambda (p) (printf "  [~a] got ~a\n" (car p) (cdr p)))
            (list-head diverged (min 40 (length diverged)))))
(when (> (length known-hit) 0)
  (printf "\n~a known (allowlisted) failures tolerated.\n" (length known-hit)))

;; Regression floor: fail on any NEW divergence or if pass drops below the floor.
(define base-floor (let ((s (getenv "JOLT_CHEZ_ZJ_FLOOR")))
                     (if s (string->number s) 2691)))
(define floor (if limit 0 base-floor))
(when (or (> (length diverged) 0) (< pass floor))
  (printf "REGRESSION: pass ~a < floor ~a or ~a new divergence(s)\n"
          pass floor (length diverged)))
(flush-output-port)
(exit (if (or (> (length diverged) 0) (< pass floor)) 1 0))
