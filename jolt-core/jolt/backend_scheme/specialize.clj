(ns jolt.backend-scheme.specialize
  "What the back end emits for the runtime specializer (#1861): the counter a
  protocol site the specializer may recompile ticks, the guard in front of an
  inlined impl body, the bare slot accesses on the receiver that guard proves,
  and the fallback call through the protocol fn already read. Those that lower
  a child node take `emit` as an argument."
  (:require [jolt.host :refer [protocol-call-key]]
            [jolt.backend-scheme.names :refer [chez-str-lit munge-name]]
            [jolt.backend-scheme.session :as session
             :refer [fresh-label seed-mint? var-cache?]]
            [jolt.backend-scheme.hoist :refer [*const-pool* hoist-const hoist-const-per-site]]
            [jolt.backend-scheme.lambda :refer [self-receiver]]))

;; A protocol site the specializer may recompile: the call a record receiver's
;; own impl takes counts itself, and every so often samples the impl and the
;; receiver's descriptor (specialize.ss jolt-psite-tick!). Only on a site
;; jolt.passes.inline number-psites numbered, inside an owner (session
;; *psite-owner*); the counter box is the site's own, made as the owner's form
;; loads.
(defn psite-ticked [c f d call]
  (let [owner session/*psite-owner*
        path (:psite (:node c))]
    (if (and (string? owner) path)
      (let [b (hoist-const-per-site (str "(jolt-psite-make " (chez-str-lit owner) " " (chez-str-lit path) ")"))]
        (str "(begin (jolt-psite-tick! " b " " f " " d ") " call ")"))
      call)))

;; A specializer's fallback call (jolt.passes.inline spec-splice): the protocol
;; fn already read off its var into a local, called as the site would have
;; called it. [via-var protocol-key], or nil.
(defn fallback-site [c n chez?]
  (let [v (:proto-via (:node c))]
    (when (and v chez? *const-pool* (var-cache?) (not (seed-mint?)) (pos? n))
      (let [pk (protocol-call-key nil (:ns v) (:name v) n)]
        (when-not (nil? pk) [v pk])))))

;; A specialized protocol site's test: the var still holds the protocol fn, the
;; receiver is of the type the site saw, and that type's dispatch vector still
;; holds the impl whose body follows. DESC and the impl are the specializer's
;; (specialize.ss jolt-spec-const).
(defn emit-proto-guard [emit node]
  (let [d (fresh-label "_sd$")
        shim (hoist-const (str "(protocol-fn-of " (chez-str-lit (:ns node)) " " (chez-str-lit (:name node)) ")"))
        id (hoist-const (str "(intern-pm-key " (chez-str-lit (:proto node)) " " (chez-str-lit (:method node)) ")"))
        desc (hoist-const (str "(jolt-spec-const " (:desc node) ")"))
        k (hoist-const (str "(jolt-spec-const " (:k node) ")"))]
    (str "(let ((" d " (jrec-pic-desc/i " (emit (:recv node)) ")))"
         " (and (eq? " (emit (:fn node)) " " shim ") (eq? " d " " desc ")"
         " (eq? (jrdesc-vtable-ref " d " " id ") " k ")))")))

;; The body a specialized protocol site inlined (jolt.passes.inline
;; specialize-sites), under its guard. The guard proved the receiver -- the
;; site's argument local -- a record of the sampled type, whose layout it
;; carries: the body's reads and writes of the receiver's fields are bare slot
;; accesses wherever that layout agrees with the place the deftype macro wrote
;; (proven-slot). With no layout, the receiver is checked once as a deftype
;; method checks it (lambda.clj self-receiver).
(defn emit-specialized-then [emit test then]
  (let [recv (:recv test)
        self (when-not (:fields test) (self-receiver {:params [(:name recv)] :body then}))]
    (cond
      (:fields test)
      (binding [session/*proven-recs* (assoc session/*proven-recs* (munge-name (:name recv))
                                             {:fields (:fields test) :dbl (:dbl test)})]
        (emit then))
      self
      (str "(let (" (nth self 2) ") "
           (binding [session/*self-recs* (assoc session/*self-recs* (nth self 0) (nth self 1))] (emit then))
           ")")
      :else (emit then))))
