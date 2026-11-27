;; No :require of clojure.core.async. Its vars are installed natively at boot
;; and the names below are spelled in full; a require would only close a cycle,
;; since the overlay (stdlib/clojure/core/async.clj) requires THIS namespace.
;; Upstream's file has the same shape, minus the cycle.
(ns clojure.core.async.impl.protocols
  (:refer-clojure :exclude [take!])
  (:require [jolt.host :as host]))

(defprotocol Handler
  (active? [h])
  (blockable? [h])
  (lock-id [h])
  (commit [h]))

(defprotocol WritePort
  (put! [port val handler] "Derefable boolean if put immediately, nil if enqueued."))

(defprotocol ReadPort
  (take! [port handler] "derefable val if taken, nil if take was enqueued"))

(deftype FnHandler [f blockable]
  java.util.concurrent.locks.Lock
  (lock [_])
  (unlock [_])
  Handler
  (active? [_] true)
  (blockable? [_] blockable)
  (lock-id [_] 0)
  (commit [_] f))

;; A native registration has its own cancellation machinery. Do not consume
;; a value on behalf of a foreign handler whose claim can be withdrawn.
(defn- callback [h]
  (if (and (instance? FnHandler h) (blockable? h))
    (commit h)
    (throw (UnsupportedOperationException. "Native port delegation requires a blockable fn-handler"))))

;; A channel reports the class name upstream's ManyToManyChannel has — that is
;; the class-arm in host/chez/java/async.ss — but the class graph carries no row
;; for the name, and value-host-tags, the list protocol dispatch looks a value up
;; by, is derived from that graph. Without a row, the extend-type below files its
;; impl under no tag a channel has and (satisfies? ReadPort ch) stays false.
;;
;; The supers list is empty on purpose: the class name is the only tag that earns
;; a place here, since dispatch looks up that same name. Naming the interfaces
;; upstream's class implements would not answer (instance? ReadPort ch) — that
;; goes through the instance check below — and nothing else asks.
(host/register-class-supers!
 "clojure.core.async.impl.channels.ManyToManyChannel" [])

;; (:import (clojure.core.async.impl.protocols ReadPort)) does not load a
;; namespace, and instance-check does not consult the class graph, so the key
;; (instance? ReadPort ch) lowers to has to be answered directly. Both spellings
;; are registered: the ns/Name key an unqualified symbol resolves to, and the
;; dotted class spelling.
;;
;; `when`, not the `and` it resembles. The registry reads nil as "not mine, ask
;; the next check" and anything non-nil as final, so an `and` — which returns
;; false for every name it does not own — would stop the walk and veto the
;; instance checks registered after this one (jolt.socket, jolt.time and
;; java.time all register checks).
(clojure.core/__register-instance-check!
 (fn [class-name val]
   (when (and (contains? #{"clojure.core.async.impl.protocols.ReadPort"
                           "clojure.core.async.impl.protocols/ReadPort"
                           "clojure.core.async.impl.protocols.WritePort"
                           "clojure.core.async.impl.protocols/WritePort"}
                         class-name)
              (clojure.core.async/chan? val))
     true)))

(extend-type clojure.core.async.impl.channels.ManyToManyChannel
  ReadPort
  (take! [port handler]
    ;; Earlier Jolt releases also accepted a bare callback at this entry point.
    (if (fn? handler)
      (clojure.core.async/take! port handler)
      (clojure.core.async/__port-take! port (callback handler))))
  WritePort
  (put! [port val handler] (clojure.core.async/__port-put! port val (callback handler))))
