;; run-guarded-inline.ss -- guarded inlining in the open-world spine (#1713).
;;
;; `jolt run` splices a small fn's body into its callers behind a test that the
;; callee's var still holds the fn that body was compiled into
;; (host-contract.ss hc-guarded-inline?, jolt.passes.inline guarded-splice). The
;; rows pin both halves: the splice happens, and every way a var can change
;; afterwards -- a redefinition, with-redefs, alter-var-root -- reaches a caller
;; compiled before it, as the call through the var would. The replay row is the
;; AOT cache's case: a caller's code compiled against one def of its callee and
;; run after a different def of it has loaded must take the call.
;;
;;   chez --script host/chez/run-guarded-inline.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define (emit-src src) (jolt-analyze-emit-form (jolt-ce-read src) "user"))
(define (run-scm scm) (eval (read (open-input-string scm)) (interaction-environment)))
(define (evals src) (jolt-compile-eval src "user"))
(define (spliced? scm) (gate-sub? scm "jolt-inline-root"))

;; --- off: the spine as it was ------------------------------------------------
(set-guarded-inline-flag! #f)
(evals "(defn gi-off-callee [x] (+ x 2))")
(gate-check "flag off: a call stays a call"
            (spliced? (emit-src "(defn gi-off-caller [x] (gi-off-callee x))")) #f)

;; --- on ---------------------------------------------------------------------
(set-guarded-inline-flag! #t)
(evals "(defn gi-add2 [x] (+ x 2))")
(define caller-src "(defn gi-twice [x] (* 2 (gi-add2 x)))")
(define caller-scm (emit-src caller-src))
(gate-check "flag on: a small callee is spliced behind its guard" (spliced? caller-scm) #t)
(run-scm caller-scm)
(gate-check "the spliced body answers the callee's answer" (evals "(gi-twice 1)") 6)
(gate-check "with-redefs reaches the spliced caller"
            (evals "(with-redefs [gi-add2 (fn [x] (+ x 100))] (gi-twice 1))") 202)
(gate-check "...and the body is back after it" (evals "(gi-twice 1)") 6)
(evals "(alter-var-root #'gi-add2 (constantly (fn [x] 50)))")
(gate-check "alter-var-root reaches the spliced caller" (evals "(gi-twice 1)") 100)
(evals "(defn gi-add2 [x] (+ x 1000))")
(gate-check "a redefinition reaches the spliced caller" (evals "(gi-twice 1)") 2002)

;; --- the fn is read before the arguments, as a call reads it -----------------
(evals "(defn gi-o [x] (+ x 1))")
(let ((scm (emit-src "(defn gi-oc [] (gi-o (do (alter-var-root (var gi-o) (constantly (fn [x] :new))) 1)))")))
  (gate-check "eval order: the caller is spliced" (spliced? scm) #t)
  (run-scm scm))
(gate-check "eval order: an argument that redefines the var runs the fn read before it, then the new one"
            (evals "(= [(gi-oc) (gi-oc)] [2 :new])") #t)

;; --- replay: compiled against one def, loaded after another ------------------
(evals "(defn gi-r-callee [x] (+ x 2))")
(define replay-scm (emit-src "(defn gi-r-caller [x] (gi-r-callee x))"))
(gate-check "replay: the caller is spliced" (spliced? replay-scm) #t)
;; the same source defined again: a new def, so a new token
(evals "(defn gi-r-callee [x] (+ x 2000))")
(run-scm replay-scm)
(gate-check "replay: a caller loaded after its callee was redefined takes the call"
            (evals "(gi-r-caller 1)") 2001)

;; --- what is never spliced ---------------------------------------------------
(evals "(defn ^:dynamic gi-dyn [x] (inc x))")
(gate-check "a ^:dynamic callee stays a call"
            (spliced? (emit-src "(defn gi-dyn-caller [x] (gi-dyn x))")) #f)
(evals "(defprotocol GiP (gi-pm [x]))")
(evals "(deftype GiT [a] GiP (gi-pm [_] a))")
(let ((scm (emit-src "(defn gi-pcaller [t] (gi-pm t))")))
  (gate-check "a protocol method keeps its dispatch-vector call site"
              (and (not (spliced? scm)) (gate-sub? scm "jrdesc-vtable-ref")) #t)
  (run-scm scm))
(gate-check "...and answers" (evals "(gi-pcaller (GiT. 7))") 7)

;; a ^double/^long param binds its argument coerced for the spliced body only:
;; the fallback call passes each argument as given, evaluated once, in order
(evals "(defn gi-dbl [^double x] (+ x 1.0))")
(evals "(defn gi-lng [^long x y] (+ x y))")
(evals "(def gi-log (atom []))")
(let ((scm (emit-src "(defn gi-hcaller [v] [(gi-dbl v) (gi-dbl (do (swap! gi-log conj :a) v)) (gi-lng (do (swap! gi-log conj :b) v) (do (swap! gi-log conj :c) 1))])")))
  (gate-check "a hinted callee is spliced" (spliced? scm) #t)
  (run-scm scm))
(gate-check "...and coerces in the body" (evals "(pr-str (gi-hcaller 1))") "[2.0 2.0 2]")
(evals "(defn gi-dbl [x] [x (type x)])")
(evals "(defn gi-lng [x y] [x y])")
(gate-check "after a redefinition the call gets the arguments as passed"
            (evals "(do (reset! gi-log []) (pr-str [(gi-hcaller 1) (gi-hcaller \"s\") @gi-log]))")
            "[[[1 java.lang.Long] [1 java.lang.Long] [1 1]] [[\"s\" java.lang.String] [\"s\" java.lang.String] [\"s\" 1]] [:a :b :c :a :b :c]]")

(set-guarded-inline-flag! #f)
(gate-summary "guarded-inline")
