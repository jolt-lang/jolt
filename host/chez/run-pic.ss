;; run-pic.ss — protocol-dispatch polymorphic inline cache gate (backend_scheme emit).
;;
;; WP-B: under --opt the inference tags every recognized protocol call with
;; :proto/:method; the back end emits a per-site inline cache keyed on the
;; receiver's descriptor identity (an eq? scan over <= jolt-pic-n cached descs +
;; a global epoch guard) instead of re-walking the protocol string tables each
;; call. This gate pins the emission and the runtime contract:
;;   * the emitted form carries the PIC machinery (jolt-pic-make/install/rebuild,
;;     jrec-pic-desc, the jolt-proto-epoch guard) and NOT the devirt cell — a
;;     monomorphic site keeps the faster devirt path;
;;   * evaluating the def and calling it across distinct record types returns each
;;     type's own impl (megamorphic correctness), and stays correct on a repeat
;;     (the warmed cache serves the hit);
;;   * after an extend-type re-registers an impl at runtime (bumping the epoch),
;;     a subsequent call returns the NEW impl — the cache invalidated and rebuilt.
;;
;;   chez --script host/chez/run-pic.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze         (var-deref "jolt.analyzer" "analyze"))
(define emit            (var-deref "jolt.backend-scheme" "emit"))
(define emit-top-form   (var-deref "jolt.backend-scheme" "emit-top-form"))
(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(define kw              (lambda (n) (keyword #f n)))

(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
;; one protocol, three inline impls (distinct per-type results), instances.
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [r] Shape (area [s] (:r s)))")
(evals "(defrecord Square [w] Shape (area [s] (* (:w s) (:w s))))")
(evals "(defrecord Triangle [b h] Shape (area [s] (/ (* (:b s) (:h s)) 2)))")
(evals "(def c  (->Circle 7))")
(evals "(def sq (->Square 5))")
(evals "(def tr (->Triangle 6 4))")

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))
(define (has-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0)) (cond ((> (+ i m) n) #f)
                            ((string=? (substring s i (+ i m)) sub) #t)
                            (else (loop (+ i 1)))))))
;; eval an emitted Scheme string in the loaded runtime.
(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))

;; analyze (def usearea (fn [x] (area x))), tag the (area x) invoke with :proto/
;; :method (what the inference does for every recognized protocol call under --opt),
;; and emit the top form under direct-linking so the cache cells are live.
(define (pic-emit)
  (let* ((dn  (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usearea (fn [x] (area x)))")))
         (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
         (inv (jolt-get ar0 (kw "body")))
         (inv2 (jolt-assoc inv (kw "proto") "Shape" (kw "method") "area"))
         (dn2 (jolt-assoc dn (kw "init")
                          (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                      (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
    (set-direct-link! #t)
    (let ((e (emit-top-form dn2)))
      (set-direct-link! #f)
      e)))

(let ((e (pic-emit)))
  ;; emission: the PIC machinery is present.
  (check "emit uses the PIC cache vector" (has-sub? e "jolt-pic-make") #t)
  (check "emit installs on miss"          (has-sub? e "jolt-pic-install") #t)
  (check "emit rebuilds on stale epoch"   (has-sub? e "jolt-pic-rebuild") #t)
  (check "emit reads the desc identity"   (has-sub? e "jrec-pic-desc") #t)
  (check "emit guards on the epoch"       (has-sub? e "jolt-proto-epoch") #t)
  ;; a polymorphic site is NOT the monomorphic devirt path.
  (check "PIC site is not devirt"         (has-sub? e "devirt-resolve") #f)
  ;; eval the def so usearea exists with its per-site cache cell.
  (run-emit e)
  ;; megamorphic correctness: each receiver type dispatches to its own impl, and a
  ;; repeat hit stays right (the warmed cache serves the cached desc).
  (check "Circle dispatch"   (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c"))  7)
  (check "Square dispatch"   (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "sq")) 25)
  (check "Triangle dispatch" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "tr")) 12)
  (check "Circle again (cache hit)" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c")) 7)
  ;; invalidation: extend-type re-registers Circle's impl at runtime (register-
  ;; protocol-method bumps jolt-proto-epoch), so the cached site must rebuild and
  ;; serve the NEW impl rather than the stale cached one.
  (evals "(extend-type Circle Shape (area [s] (* (:r s) 100)))")
  (check "PIC invalidates after extend-type" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c")) 700))

;; a monomorphic site (the inference proved one receiver type) keeps the devirt
;; path, not the PIC: annotate :devirt-type and confirm no PIC machinery emits.
(let* ((dn  (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usearea2 (fn [x] (area x)))")))
       (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
       (inv (jolt-get ar0 (kw "body")))
       (inv2 (jolt-assoc inv (kw "proto") "Shape" (kw "method") "area"
                         (kw "devirt-type") "user.Circle" (kw "devirt-proto") "Shape" (kw "devirt-method") "area"))
       (dn2 (jolt-assoc dn (kw "init")
                        (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                    (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
  (set-direct-link! #t)
  (let ((e (emit-top-form dn2)))
    (set-direct-link! #f)
    (check "monomorphic site uses devirt, not PIC" (has-sub? e "devirt-resolve") #t)
    (check "monomorphic site emits no PIC vector"   (has-sub? e "jolt-pic-make") #f)))

;; ---- per-descriptor fast path regression (perf/round1 fix) ----------------  
;; find-protocol-method-desc must return non-#f for ALL types registered in
;; sequence (not just the last one). Before the fix the global-epoch guard in
;; find-protocol-method-desc made every desc except the most recently
;; registered miss the (fx= pepoch epoch) check.
(let* ((cd (hashtable-ref chez-tag-desc "user.Circle" #f))
       (sd (hashtable-ref chez-tag-desc "user.Square" #f))
       (td (hashtable-ref chez-tag-desc "user.Triangle" #f))
       (k  (intern-pm-key "Shape" "area")))
  (check "Circle desc ptable resolves" (not (not (find-protocol-method-desc cd "Shape" "area"))) #t)
  (check "Square desc ptable resolves" (not (not (find-protocol-method-desc sd "Shape" "area"))) #t)
  (check "Triangle desc ptable resolves" (not (not (find-protocol-method-desc td "Shape" "area"))) #t))

;; re-def Circle via defrecord to test old-desc invalidation. The old desc's
;; ptable must be set to #f so pre-redef instances fall back to the string
;; registry; the new desc resolves via its own ptable.
(let* ((old-desc (hashtable-ref chez-tag-desc "user.Circle" #f))
       (_ (evals "(defrecord Circle [r] Shape (area [s] (:r s)))"))
       (new-desc (hashtable-ref chez-tag-desc "user.Circle" #f)))
  (check "old desc invalidated after redef"     (jrdesc-ptable old-desc) #f)
  (check "find-protocol-method-desc misses on old desc"
         (find-protocol-method-desc old-desc "Shape" "area") #f)
  (check "new desc ptable resolves after redef"
         (not (not (find-protocol-method-desc new-desc "Shape" "area"))) #t)
  ;; protocol-resolve on a pre-redef instance still works (falls back to string
  ;; registry when the desc's ptable is #f).
  (check "protocol-resolve old instance after redef"
         (not (not (protocol-resolve "Shape" "area" (var-deref "user" "c")))) #t))

(if (= fails 0)
    (begin (printf "pic gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "pic gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
