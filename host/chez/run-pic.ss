;; run-pic.ss — protocol-dispatch polymorphic inline cache gate (backend_scheme emit).
;;
;; WP-B: under --opt the inference tags every recognized protocol call with
;; :proto/:method; the back end emits a per-site inline cache keyed on the
;; receiver's descriptor identity (an eq? scan over <= jolt-pic-n cached descs +
;; a global epoch guard) instead of re-walking the protocol string tables each
;; call. This gate pins the emission and the runtime contract:
;;   * the emitted form carries the PIC machinery (jolt-pic-empty/add,
;;     jrec-pic-desc, the jolt-proto-epoch guard) and NOT the devirt cell — a
;;     monomorphic site keeps the faster devirt path;
;;   * evaluating the def and calling it across distinct record types returns each
;;     type's own impl (megamorphic correctness), and stays correct on a repeat
;;     (the warmed cache serves the hit);
;;   * after an extend-type re-registers an impl at runtime (bumping the epoch),
;;     a subsequent call returns the NEW impl — the cache invalidated and rebuilt;
;;   * threads sharing one site across more types than it caches each get their
;;     own receiver's impl, every call (jolt#1284).
;;
;;   chez --script host/chez/run-pic.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze         (var-deref "jolt.analyzer" "analyze"))
(define emit-top-form   (var-deref "jolt.backend-scheme" "emit-top-form"))
(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(define kw              (lambda (n) (keyword #f n)))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)

(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
;; one protocol, three inline impls (distinct per-type results), instances.
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [r] Shape (area [s] (:r s)))")
(evals "(defrecord Square [w] Shape (area [s] (* (:w s) (:w s))))")
(evals "(defrecord Triangle [b h] Shape (area [s] (/ (* (:b s) (:h s)) 2)))")
(evals "(def c  (->Circle 7))")
(evals "(def sq (->Square 5))")
(evals "(def tr (->Triangle 6 4))")

;; eval an emitted Scheme string in the loaded runtime.
(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))

;; analyze (def usearea (fn [x] (area x))), tag the (area x) invoke with :proto/
;; :method (what the inference does for every recognized protocol call under --opt),
;; and emit the top form under direct-linking so the cache cells are live.
(define (pic-emit)
  (let* ((dn  (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usearea (fn [x] (area x)))")))
         (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
         (inv (jolt-get ar0 (kw "body")))
         (inv2 (jolt-assoc inv (kw "proto") "user/Shape" (kw "method") "area"))
         (dn2 (jolt-assoc dn (kw "init")
                          (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                      (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
    (set-direct-link! #t)
    (let ((e (emit-top-form dn2)))
      (set-direct-link! #f)
      e)))

(let ((e (pic-emit)))
  ;; emission: the PIC machinery is present.
  (gate-check "emit uses the PIC cache vector" (gate-sub? e "jolt-pic-empty") #t)
  (gate-check "emit builds a new cache on miss" (gate-sub? e "jolt-pic-add") #t)
  (gate-check "emit reads the desc identity"   (gate-sub? e "jrec-pic-desc") #t)
  (gate-check "emit guards on the epoch"       (gate-sub? e "jolt-proto-epoch") #t)
  ;; a polymorphic site is NOT the monomorphic devirt path.
  (gate-check "PIC site is not devirt"         (gate-sub? e "devirt-resolve") #f)
  ;; eval the def so usearea exists with its per-site cache cell.
  (run-emit e)
  ;; megamorphic correctness: each receiver type dispatches to its own impl, and a
  ;; repeat hit stays right (the warmed cache serves the cached desc).
  (gate-check "Circle dispatch"   (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c"))  7)
  (gate-check "Square dispatch"   (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "sq")) 25)
  (gate-check "Triangle dispatch" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "tr")) 12)
  (gate-check "Circle again (cache hit)" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c")) 7)
  ;; invalidation: extend-type re-registers Circle's impl at runtime (register-
  ;; protocol-method bumps jolt-proto-epoch), so the cached site must rebuild and
  ;; serve the NEW impl rather than the stale cached one.
  (evals "(extend-type Circle Shape (area [s] (* (:r s) 100)))")
  (gate-check "PIC invalidates after extend-type" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c")) 700)
  ;; one site, eight receiver types (twice the cache width, so every thread keeps
  ;; missing), eight threads. Each call must get its own receiver's impl: a cache
  ;; written in place by several threads handed a desc another type's impl, or
  ;; overran the vector into its epoch slot.
  (evals "(defprotocol Tag (tag [x]))")
  (evals "(do (defrecord T0 [v]) (defrecord T1 [v]) (defrecord T2 [v]) (defrecord T3 [v])
              (defrecord T4 [v]) (defrecord T5 [v]) (defrecord T6 [v]) (defrecord T7 [v]))")
  (evals "(extend-protocol Tag T0 (tag [_] 0) T1 (tag [_] 1) T2 (tag [_] 2) T3 (tag [_] 3)
                               T4 (tag [_] 4) T5 (tag [_] 5) T6 (tag [_] 6) T7 (tag [_] 7))")
  (evals "(def tag-xs (vec (for [i (range 50) k (range 8)]
                            ((nth [->T0 ->T1 ->T2 ->T3 ->T4 ->T5 ->T6 ->T7] k) k))))")
  (let* ((dn  (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usetag (fn [x] (tag x)))")))
         (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
         (inv (jolt-get ar0 (kw "body")))
         (dn2 (jolt-assoc dn (kw "init")
                          (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                      (jolt-vector (jolt-assoc ar0 (kw "body")
                                                               (jolt-assoc inv (kw "proto") "user/Tag" (kw "method") "tag"))))))
         (_ (set-direct-link! #t))
         (e (emit-top-form dn2)))
    (set-direct-link! #f)
    (run-emit e)
    (jolt-mark-mt!)
    (let* ((usetag (var-deref "user" "usetag"))
           (xs (map (lambda (i) (jolt-nth (var-deref "user" "tag-xs") i))
                    (iota (jolt-count (var-deref "user" "tag-xs")))))
           (bad 0)
           (m (make-mutex))
           (left 8))
      (define (work)
        (let loop ((n 0) (wrong 0))
          (if (fx= n 200)
              wrong
              (loop (fx+ n 1)
                    (fold-left (lambda (w x)
                                 (if (guard (c (#t #f))
                                       (eqv? (jolt-invoke usetag x) (jolt-get x (kw "v"))))
                                     w (fx+ w 1)))
                               wrong xs)))))
      (do ((i 0 (fx+ i 1))) ((fx= i 8))
        (fork-thread
         (lambda ()
           (let ((w (work)))
             (jolt-with-mutex m
               (set! bad (fx+ bad w))
               (set! left (fx- left 1)))))))
      (let wait ()
        (unless (fx= 0 (jolt-with-mutex m left))
          (sleep (make-time 'time-duration 1000000 0))
          (wait)))
      (gate-check "PIC shared by 8 threads over 8 types: no wrong impl" bad 0))))

;; a monomorphic site (the inference proved one receiver type) keeps the devirt
;; path, not the PIC: annotate :devirt-type and confirm no PIC machinery emits.
(let* ((dn  (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usearea2 (fn [x] (area x)))")))
       (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
       (inv (jolt-get ar0 (kw "body")))
       (inv2 (jolt-assoc inv (kw "proto") "user/Shape" (kw "method") "area"
                         (kw "devirt-type") "user.Circle" (kw "devirt-proto") "user/Shape" (kw "devirt-method") "area"))
       (dn2 (jolt-assoc dn (kw "init")
                        (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                    (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
  (set-direct-link! #t)
  (let ((e (emit-top-form dn2)))
    (set-direct-link! #f)
    (gate-check "monomorphic site uses devirt, not PIC" (gate-sub? e "devirt-resolve") #t)
    (gate-check "monomorphic site emits no PIC vector"   (gate-sub? e "jolt-pic-empty") #f)))

;; ---- per-descriptor fast path regression (perf/round1 fix) ----------------  
;; find-protocol-method-desc must return non-#f for ALL types registered in
;; sequence (not just the last one). Before the fix the global-epoch guard in
;; find-protocol-method-desc made every desc except the most recently
;; registered miss the (fx= pepoch epoch) check.
(let* ((cd (hashtable-ref chez-tag-desc "user.Circle" #f))
       (sd (hashtable-ref chez-tag-desc "user.Square" #f))
       (td (hashtable-ref chez-tag-desc "user.Triangle" #f)))
  (gate-check "Circle desc ptable resolves" (not (not (find-protocol-method-desc cd "user/Shape" "area"))) #t)
  (gate-check "Square desc ptable resolves" (not (not (find-protocol-method-desc sd "user/Shape" "area"))) #t)
  (gate-check "Triangle desc ptable resolves" (not (not (find-protocol-method-desc td "user/Shape" "area"))) #t))

;; re-def Circle via defrecord to test old-desc invalidation. The old desc's
;; ptable must be set to #f so pre-redef instances fall back to the string
;; registry; the new desc resolves via its own ptable.
(let* ((old-desc (hashtable-ref chez-tag-desc "user.Circle" #f))
       (_ (evals "(defrecord Circle [r] Shape (area [s] (:r s)))"))
       (new-desc (hashtable-ref chez-tag-desc "user.Circle" #f)))
  (gate-check "old desc invalidated after redef"     (jrdesc-ptable old-desc) #f)
  (gate-check "find-protocol-method-desc misses on old desc"
         (find-protocol-method-desc old-desc "user/Shape" "area") #f)
  (gate-check "new desc ptable resolves after redef"
         (not (not (find-protocol-method-desc new-desc "user/Shape" "area"))) #t)
  ;; protocol-resolve on a pre-redef instance still works (falls back to string
  ;; registry when the desc's ptable is #f).
  (gate-check "protocol-resolve old instance after redef"
         (not (not (protocol-resolve "user/Shape" "area" (var-deref "user" "c")))) #t))

;; ---- contagion: a PIC (megamorphic) site never resolves a contagion clone ------
;; Contagion clones are gated to monomorphic devirt sites by construction — only the
;; :devirt-type emit clause consults clone-sites. Make Ring/Shape/area a clone-site
;; (a contagion-eligible impl reached by a devirt site), then a PIC site over the
;; same proto/method must still use the PIC path and never devirt-resolve-fl.
(let* ((set-record-shapes!   (var-deref "jolt.passes.types" "set-record-shapes!"))
       (set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
       (wp-infer!            (var-deref "jolt.passes.types" "wp-infer!"))
       (contagion-prepass!    (var-deref "jolt.backend-scheme" "contagion-prepass!"))
       (contagion-prepass-done! (var-deref "jolt.backend-scheme" "contagion-prepass-done!"))
       (reset-clone-prepass!  (var-deref "jolt.backend-scheme" "reset-clone-prepass!")))
  (evals "(defrecord Ring [n] Shape (area [s] (* 3.14159 (:n s))))")
  (set-record-shapes! U (chez-record-shapes-map))
  (set-protocol-methods! U (chez-protocol-methods-map))
  (let* ((ring-def  (analyze (make-analyze-ctx "user") (jolt-ce-read "(defrecord Ring [n] Shape (area [s] (* 3.14159 (:n s))))")))
         (ring-ctor (analyze (make-analyze-ctx "user") (jolt-ce-read "(def ring-use (fn [] (->Ring 7)))")))
         (ddn  (analyze (make-analyze-ctx "user") (jolt-ce-read "(def dvring (fn [x] (area x)))")))
         (ar0  (jolt-nth (jolt-get (jolt-get ddn (kw "init")) (kw "arities")) 0))
         (inv  (jolt-get ar0 (kw "body")))
         (inv2 (jolt-assoc inv (kw "devirt-type") "user.Ring" (kw "devirt-proto") "user/Shape" (kw "devirt-method") "area"))
         (ddn2 (jolt-assoc ddn (kw "init") (jolt-assoc (jolt-get ddn (kw "init")) (kw "arities")
                                                       (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
    (wp-infer! U (jolt-vector ring-def ring-ctor))
    (reset-clone-prepass! U)
    (contagion-prepass! U (jolt-vector ring-def ddn2) "user")
    (contagion-prepass-done! U)
    (let ((e (pic-emit)))
      (gate-check "PIC over eligible impl still uses the PIC path" (gate-sub? e "jolt-pic-empty") #t)
      (gate-check "PIC over eligible impl never emits devirt-resolve-fl" (gate-sub? e "devirt-resolve-fl") #f))))

;; ---- :extend-via-metadata: a call the receiver's type alone cannot answer -------
;; A protocol defined with :extend-via-metadata true consults the value's metadata
;; between the type's own impls and extend's, which only its method shim does
;; (protocol-resolve-meta). An optimizing compile must therefore leave the call to
;; the shim: neither a PIC nor a devirtualized resolve may stand in for it, or a
;; built binary answers the extended impl, or a dispatch miss, where the JVM (and
;; jolt run) answer the metadata's.
(let* ((set-record-shapes!    (var-deref "jolt.passes.types" "set-record-shapes!"))
       (set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
       (run-passes            (var-deref "jolt.passes" "run-passes")))
  (define (opt-emit src)
    (set-optimize! #t)
    (set-record-shapes! U (chez-record-shapes-map))
    (set-protocol-methods! U (chez-protocol-methods-map))
    (let ((r ((var-deref "jolt.backend-scheme" "emit")
              (run-passes (analyze (make-analyze-ctx "user") (jolt-ce-read src))
                          (make-analyze-ctx "user") U))))
      (set-optimize! #f)
      r))
  (evals "(defprotocol MP :extend-via-metadata true (mp [x]))")
  (evals "(defrecord ME [a])")
  (evals "(extend-protocol MP ME (mp [_] :extended))")
  (let ((e (opt-emit "(defn mp-any [x] (mp x))")))
    (gate-check "via-metadata call emits no PIC" (gate-sub? e "jolt-pic") #f)
    (run-emit e)
    (gate-check "...and answers a vector's metadata impl"
                (jolt-final-str (evals "(pr-str (mp-any (with-meta [1] {`mp (fn [_] :meta)})))")) ":meta")
    (gate-check "...and a record's metadata ahead of extend"
                (jolt-final-str (evals "(pr-str (mp-any (with-meta (->ME 1) {`mp (fn [_] :meta)})))")) ":meta"))
  (let ((e (opt-emit "(defn mp-rec [] (mp (with-meta (->ME 1) {`mp (fn [_] :meta)})))")))
    (gate-check "via-metadata call on a proven record does not devirtualize" (gate-sub? e "devirt-resolve") #f)
    (run-emit e)
    (gate-check "...and answers the metadata impl"
                (jolt-final-str (evals "(pr-str (mp-rec))")) ":meta")))

(gate-summary "pic")
