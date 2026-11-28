;; run-specialize.ss -- runtime specialization of hot protocol call sites (#1861).
;;
;; `jolt run` code counts its protocol calls on records and samples the impl
;; each site reaches (specialize.ss); a site whose samples agree is inlined into
;; a recompile of the fn literal that holds it, behind a guard that the var
;; still holds the protocol fn, the receiver is of the sampled type and that
;; type's dispatch vector still holds the sampled impl. The rows pin both
;; halves: a chain of deftype reads is specialized and answers as before, and
;; every way the dispatch can change afterwards -- a redefined type, a
;; redefined method, a with-redefs of the protocol fn -- takes the call.
;;
;;   chez --script host/chez/run-specialize.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define (evals src) (jolt-compile-eval src "user"))
(set-guarded-inline-flag! #t)
(set-specialize-flag! #t)

;; the impl a type's dispatch vector holds for SpRd/sp-read
(define (impl-of tag)
  (find-protocol-method-desc (hashtable-ref chez-tag-desc tag #f) "user/SpRd" "sp-read"))
(define (specialized? tag) (and (hashtable-ref spec-proc-owner (impl-of tag) #f) #t))
;; ...for any protocol's method
(define (specialized-in? tag proto method)
  (and (hashtable-ref spec-proc-owner
                      (find-protocol-method-desc (hashtable-ref chez-tag-desc tag #f) proto method) #f)
       #t))

(evals "(defprotocol SpRd (sp-read [r]) (sp-unread [r c]))")
(evals "(deftype SpStr [^String s ^:unsynchronized-mutable i] SpRd
          (sp-read [_] (if (< i (count s)) (let [c (.charAt s i)] (set! i (inc i)) c) nil))
          (sp-unread [_ c] (set! i (dec i)) nil))")
(evals "(deftype SpPb [rdr ^:unsynchronized-mutable buf ^:unsynchronized-mutable ^double last] SpRd
          (sp-read [_] (if (some? buf) (let [c buf] (set! buf nil) c) (let [c (sp-read rdr)] (when c (set! last 1)) c)))
          (sp-unread [_ c] (set! buf c) nil))")
(evals "(deftype SpIdx [rdr ^:unsynchronized-mutable n ^:unsynchronized-mutable col] SpRd
          (sp-read [_] (let [c (sp-read rdr)] (when c (set! n (inc n)) (set! col (inc col))) c))
          (sp-unread [_ c] (set! n (dec n)) (sp-unread rdr c)))")
(evals "(def sp-text (apply str (repeat 3000 \"abcdefghi\\n\")))")
(evals "(defn sp-drain [r] (loop [n 0 acc 0] (if-let [c (sp-read r)] (recur (inc n) (+ acc (int c))) [n acc])))")
(define (drain) (evals "(pr-str (sp-drain (SpIdx. (SpPb. (SpStr. sp-text 0) nil 0.0) 0 0)))"))

;; a namespace loaded from the AOT cache never advanced this process's
;; per-namespace literal counter, so a literal named off it now would take the
;; registry name of one already loaded: the recompile must not. Put back after
;; the chain: a literal defined under the reset would take a name already
;; registered, which the specializer refuses (fn-form-registry.ss fn-name-reused)
(define saved-counters ((var-deref "clojure.core" "deref") (var-deref "jolt.backend-scheme.fnsrc" "fnsrc-ns-counters")))
((var-deref "clojure.core" "reset!") (var-deref "jolt.backend-scheme.fnsrc" "fnsrc-ns-counters")
                                     (jolt-hash-map))

;; --- the chain ---------------------------------------------------------------
(gate-check "the chain answers before it is hot" (drain) "[30000 2757000]")
(gate-check "...and while it gets hot" (begin (drain) (drain) (drain)) "[30000 2757000]")
(gate-check "the outer layer's read is specialized" (specialized? "user.SpIdx") #t)
(gate-check "...and so is the middle one's" (specialized? "user.SpPb") #t)
(gate-check "the specialized chain answers the same" (drain) "[30000 2757000]")
;; a backtrace names a recompiled method as the method it is, not by its
;; recompile's internal name
(gate-check "the recompiled read reports as SpIdx.sp-read"
            (let ((rec (hashtable-ref source-registry (sa-procedure-code-name (impl-of "user.SpIdx")) #f)))
              (and (vector? rec) (vector-ref rec 1)))
            "SpIdx.sp-read")
;; every 1024th call samples; a guard that held never reaches its fallback's
;; counter, so the owner has taken no sample since its recompile
(define (fallback-samples tag)
  (spec-owner-since (spec-owner-of (spec-impl-owner (impl-of tag)))))
(gate-check "a ^double field set inside an inlined body is still a double"
            (evals "(let [pb (SpPb. (SpStr. sp-text 0) nil 0.0)] (sp-drain (SpIdx. pb 0 0)) (pr-str (.-last pb)))")
            "1.0")
(gate-check "the specialized reads take their inlined arm" (begin (drain) (drain) (fallback-samples "user.SpIdx")) 0)

((var-deref "clojure.core" "reset!") (var-deref "jolt.backend-scheme.fnsrc" "fnsrc-ns-counters") saved-counters)

;; --- a redefined type takes the call -----------------------------------------
;; a new SpStr is a new descriptor, which no guard sampled
(evals "(deftype SpStr [^String s ^:unsynchronized-mutable i] SpRd
          (sp-read [_] (if (< i (count s)) (let [c (Character/toUpperCase (.charAt s i))] (set! i (inc i)) c) nil))
          (sp-unread [_ c] (set! i (dec i)) nil))")
(gate-check "a redefined innermost type is read through its new impl" (drain) "[30000 1893000]")
(gate-check "...and keeps being read through it as it gets hot again"
            (begin (drain) (drain) (drain) (drain)) "[30000 1893000]")

;; --- with-redefs of the protocol fn ------------------------------------------
;; extended (not declared) types call what the var holds; a declared one is the
;; JVM's interface call and keeps its impl
(evals "(defrecord SpOuter [rdr])")
(evals "(defrecord SpInner [rdr])")
(evals "(extend-type SpOuter SpRd (sp-read [w] (sp-read (:rdr w))) (sp-unread [w c] nil))")
(evals "(extend-type SpInner SpRd (sp-read [w] (sp-read (:rdr w))) (sp-unread [w c] nil))")
(define (drain-wrapped)
  (evals "(pr-str (sp-drain (SpOuter. (SpInner. (SpStr. sp-text 0)))))"))
(gate-check "an extended chain answers" (drain-wrapped) "[30000 1893000]")
(drain-wrapped) (drain-wrapped) (drain-wrapped)
(gate-check "...and is specialized" (specialized? "user.SpOuter") #t)
(gate-check "with-redefs of the protocol fn reaches a site inside the specialized impl"
            (evals "(let [orig sp-read]
                      (with-redefs [sp-read (fn [r] (if (instance? SpOuter r) (orig r) :redef))]
                        (sp-read (SpOuter. (SpInner. (SpStr. \"ab\" 0))))))")
            (jolt-keyword #f "redef"))
(gate-check "...and the specialized impl is back after it" (drain-wrapped) "[30000 1893000]")

;; --- a redefined method takes the call -----------------------------------------
;; extending SpInner again replaces its impl in its dispatch vector, which the
;; guard in SpOuter's specialized read compares against
(evals "(extend-type SpInner SpRd (sp-read [w] (when (sp-read (:rdr w)) \\x)) (sp-unread [w c] nil))")
(gate-check "a method redefined by extend-type is called" (drain-wrapped) "[30000 3600000]")

;; --- the inlined body keeps its arity's coercions -----------------------------
;; a ^double param coerces on entry and a ^long return truncates; the copy
;; binds the site's arguments straight, so it must coerce them the same way
(evals "(defprotocol SpSc (sp-scale [v k]))")
(evals "(deftype SpV [x])")
(evals "(extend-type SpV SpSc (sp-scale [_ ^double k] (str k)))")
(evals "(deftype SpW [v] SpSc (sp-scale [_ k] (sp-scale v k)))")
(evals "(defn sp-scale-hot [] (let [w (SpW. (SpV. 1))] (dotimes [_ 6000] (sp-scale w 2)) (sp-scale w 2)))")
(evals "(sp-scale-hot)")
(gate-check "a chain into a ^double param is specialized" (specialized-in? "user.SpW" "user/SpSc" "sp-scale") #t)
(gate-check "a ^double param of an inlined impl is coerced" (evals "(sp-scale-hot)") "2.0")

;; --- a name registered twice is not specialized -------------------------------
;; a literal inside a def takes the same registry name each time the def is
;; evaluated; the old literal, still installed by an earlier extend, must not be
;; recompiled from the new one's source
(evals "(defprotocol SpP (sp-p [x]))")
(evals "(deftype SpLeaf [] SpP (sp-p [_] :leaf))")
(evals "(defrecord SpA [r])")
(evals "(def sp-impl {:sp-p (fn [x] (sp-p (:r x)))})")
(evals "(extend SpA SpP sp-impl)")
(evals "(def sp-impl {:sp-p (fn [x] (sp-p (:r x)) :new)})")
(evals "(defn sp-p-hot [] (let [a (SpA. (SpLeaf.))] (dotimes [_ 6000] (sp-p a)) (sp-p a)))")
(evals "(sp-p-hot)")
(gate-check "an extend whose map literal was redefined keeps its own code"
            (evals "(sp-p-hot)") (jolt-keyword #f "leaf"))

;; --- the recompile compiles as the load did ------------------------------------
;; *unchecked-math* in force where the literal was compiled still applies when
;; the specializer recompiles it
(evals "(defprotocol SpCnt (sp-next [c]))")
(evals "(deftype SpOne [] SpCnt (sp-next [_] 1))")
;; bound as the loader binds it around a file that set! it
(dyn-with-frame (list (cons (var-cell-lookup "clojure.core" "*unchecked-math*") #t))
  (lambda () (evals "(deftype SpWrap [inner] SpCnt (sp-next [_] (+ (sp-next inner) Long/MAX_VALUE)))")))
(evals "(defn sp-wrap-hot [] (let [w (SpWrap. (SpOne.))] (dotimes [_ 6000] (sp-next w)) (sp-next w)))")
(evals "(sp-wrap-hot)")
(gate-check "a chain compiled under *unchecked-math* is specialized" (specialized-in? "user.SpWrap" "user/SpCnt" "sp-next") #t)
(gate-check "*unchecked-math* of the original compile holds in the recompile"
            (evals "(sp-wrap-hot)") (- (expt 2 63)))

;; --- off ---------------------------------------------------------------------
(set-specialize-flag! #f)
(evals "(deftype SpOff [rdr] SpRd (sp-read [_] (sp-read rdr)) (sp-unread [_ c] nil))")
(evals "(defn sp-drain-off [] (sp-drain (SpOff. (SpStr. sp-text 0))))")
(evals "(dotimes [_ 5] (sp-drain-off))")
(gate-check "JOLT_SPECIALIZE=0: a hot site stays a call" (specialized? "user.SpOff") #f)

(set-guarded-inline-flag! #f)
(gate-summary "specialize")
