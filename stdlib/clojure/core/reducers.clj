;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)

;; Ported from clojure.core.reducers (Rich Hickey), Clojure 1.12's
;; src/clj/clojure/core/reducers.clj, with its docstrings.
;;
;; DIVERGENCE from the JVM: fold is sequential. The JVM's fold forks its halves
;; onto a ForkJoinPool; here the same halves are reduced one after the other. A
;; vector still splits exactly as on the JVM (halving until a piece holds n or
;; fewer elements, each piece reduced from (combinef) and the pieces joined with
;; combinef), so a combinef sees the same pieces and fold gives the same answer,
;; foldcat the same Cat shape. A map is reduced whole with its key and value,
;; which is what a PersistentHashMap's own fold gives for an associative combinef.
;; A collection that can't be folded is reduced, as on the JVM.
;;
;; A reducer, a folder and a Cat are CollReduce, as on the JVM, and also
;; clojure.lang.IReduce: core reduce, into and transduce drive a reducible
;; through IReduce/IReduceInit here, so that is what lets them take one.
(ns clojure.core.reducers
  (:refer-clojure :exclude [reduce map mapcat filter remove take take-while drop flatten cat])
  (:require [clojure.core.protocols :as p]))

(defn reduce
  "Like core/reduce except:
     When init is not provided, (f) is used.
     Maps are reduced with reduce-kv"
  ([f coll] (reduce f (f) coll))
  ([f init coll]
   (if (instance? java.util.Map coll)
     (clojure.core/reduce-kv f init coll)
     (clojure.core/reduce f init coll))))

(defprotocol CollFold
  (coll-fold [coll n combinef reducef]))

(defn fold
  "Reduces a collection using a (potentially parallel) reduce-combine
  strategy. The collection is partitioned into groups of approximately
  n (default 512), each of which is reduced with reducef (with a seed
  value obtained by calling (combinef) with no arguments). The results
  of these reductions are then reduced with combinef (default
  reducef). combinef must be associative, and, when called with no
  arguments, (combinef) must produce its identity element. These
  operations may be performed in parallel, but the results will
  preserve order."
  ([reducef coll] (fold reducef reducef coll))
  ([combinef reducef coll] (fold 512 combinef reducef coll))
  ([n combinef reducef coll]
   (coll-fold coll n combinef reducef)))

(defn reducer
  "Given a reducible collection, and a transformation function xf,
  returns a reducible collection, where any supplied reducing
  fn will be transformed by xf. xf is a function of reducing fn to
  reducing fn."
  [coll xf]
  (reify
    p/CollReduce
    (coll-reduce [this f1] (p/coll-reduce this f1 (f1)))
    (coll-reduce [_ f1 init] (clojure.core/reduce (xf f1) init coll))
    clojure.lang.IReduce
    (reduce [_ f1] (clojure.core/reduce (xf f1) (f1) coll))
    (reduce [_ f1 init] (clojure.core/reduce (xf f1) init coll))))

(defn folder
  "Given a foldable collection, and a transformation function xf,
  returns a foldable collection, where any supplied reducing
  fn will be transformed by xf. xf is a function of reducing fn to
  reducing fn."
  [coll xf]
  (reify
    p/CollReduce
    (coll-reduce [_ f1] (clojure.core/reduce (xf f1) (f1) coll))
    (coll-reduce [_ f1 init] (clojure.core/reduce (xf f1) init coll))
    clojure.lang.IReduce
    (reduce [_ f1] (clojure.core/reduce (xf f1) (f1) coll))
    (reduce [_ f1 init] (clojure.core/reduce (xf f1) init coll))
    CollFold
    (coll-fold [_ n combinef reducef] (coll-fold coll n combinef (xf reducef)))))

;; The JVM writes each transformer with defcurried (the last argument left off
;; gives back a fn of the collection) and rfn (a reducing fn with a [] arity, a
;; [ret v] arity, and the [ret k v] arity a map's kv reduction calls). They are
;; written out here.

(defn map
  "Applies f to every value in the reduction of coll. Foldable."
  ([f] (fn [coll] (map f coll)))
  ([f coll]
   (folder coll
           (fn [f1]
             (fn
               ([] (f1))
               ([ret v] (f1 ret (f v)))
               ([ret k v] (f1 ret (f k v))))))))

(defn mapcat
  "Applies f to every value in the reduction of coll, concatenating the result
  colls of (f val). Foldable."
  ([f] (fn [coll] (mapcat f coll)))
  ([f coll]
   (folder coll
           (fn [f1]
             ;; a reduced from the inner reduction must stop the outer one too,
             ;; so it is wrapped again to survive the inner reduce's unwrapping.
             ;; Clojure's source shadows f1 with this, so its [] arity calls a fn
             ;; with no 0-arity; here [] calls the f1 it was given.
             (let [f1* (fn
                         ([ret v] (let [x (f1 ret v)] (if (reduced? x) (reduced x) x)))
                         ([ret k v] (let [x (f1 ret k v)] (if (reduced? x) (reduced x) x))))]
               (fn
                 ([] (f1))
                 ([ret v] (reduce f1* ret (f v)))
                 ([ret k v] (reduce f1* ret (f k v)))))))))

(defn filter
  "Retains values in the reduction of coll for which (pred val)
  returns logical true. Foldable."
  ([pred] (fn [coll] (filter pred coll)))
  ([pred coll]
   (folder coll
           (fn [f1]
             (fn
               ([] (f1))
               ([ret v] (if (pred v) (f1 ret v) ret))
               ([ret k v] (if (pred k v) (f1 ret k v) ret)))))))

(defn remove
  "Removes values in the reduction of coll for which (pred val)
  returns logical true. Foldable."
  ([pred] (fn [coll] (remove pred coll)))
  ([pred coll] (filter (complement pred) coll)))

(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat foldable
  collection."
  ([] (fn [coll] (flatten coll)))
  ([coll]
   (folder coll
           (fn [f1]
             (fn
               ([] (f1))
               ([ret v]
                (if (sequential? v)
                  (clojure.core/reduce f1 ret (flatten v))
                  (f1 ret v))))))))

(defn take-while
  "Ends the reduction of coll when (pred val) returns logical false."
  ([pred] (fn [coll] (take-while pred coll)))
  ([pred coll]
   (reducer coll
            (fn [f1]
              (fn
                ([] (f1))
                ([ret v] (if (pred v) (f1 ret v) (reduced ret)))
                ([ret k v] (if (pred k v) (f1 ret k v) (reduced ret))))))))

(defn take
  "Ends the reduction of coll after consuming n values."
  ([n] (fn [coll] (take n coll)))
  ([n coll]
   (reducer coll
            (fn [f1]
              (let [cnt (atom n)]
                (fn
                  ([] (f1))
                  ([ret v] (swap! cnt dec) (if (neg? @cnt) (reduced ret) (f1 ret v)))
                  ([ret k v] (swap! cnt dec) (if (neg? @cnt) (reduced ret) (f1 ret k v)))))))))

(defn drop
  "Elides the first n values from the reduction of coll."
  ([n] (fn [coll] (drop n coll)))
  ([n coll]
   (reducer coll
            (fn [f1]
              (let [cnt (atom n)]
                (fn
                  ([] (f1))
                  ([ret v] (swap! cnt dec) (if (neg? @cnt) (f1 ret v) ret))
                  ([ret k v] (swap! cnt dec) (if (neg? @cnt) (f1 ret k v) ret))))))))

;; The JVM's foldvec, run in place of forking: halve until a piece holds n or
;; fewer elements, reduce each piece from (combinef), join the halves with combinef.
(defn- foldvec [v n combinef reducef]
  (cond
    (empty? v) (combinef)
    (<= (count v) n) (reduce reducef (combinef) v)
    :else (let [split (quot (count v) 2)
                v1 (subvec v 0 split)
                v2 (subvec v split (count v))]
            (combinef (foldvec v1 n combinef reducef)
                      (foldvec v2 n combinef reducef)))))

(extend-protocol CollFold
  nil
  (coll-fold [_coll _n combinef _reducef] (combinef))

  Object
  (coll-fold [coll n combinef reducef]
    ;; a vector is clojure.lang.IPersistentVector's arm on the JVM, a map its own
    ;; fold's; anything else can't fold and is one reduce
    (if (vector? coll)
      (foldvec coll n combinef reducef)
      (reduce reducef (combinef) coll))))

(deftype Cat [cnt left right]
  clojure.lang.Counted
  (count [_] cnt)

  clojure.lang.Seqable
  (seq [_] (concat (seq left) (seq right)))

  p/CollReduce
  (coll-reduce [this f1] (p/coll-reduce this f1 (f1)))
  (coll-reduce [_ f1 init]
    (clojure.core/reduce f1 (clojure.core/reduce f1 init left) right))

  clojure.lang.IReduce
  (reduce [this f1] (.reduce this f1 (f1)))
  (reduce [_ f1 init]
    (clojure.core/reduce f1 (clojure.core/reduce f1 init left) right))

  CollFold
  (coll-fold [_ n combinef reducef]
    (combinef (fold n combinef reducef left)
              (fold n combinef reducef right))))

(defn cat
  "A high-performance combining fn that yields the catenation of the
  reduced values. The result is reducible, foldable, seqable and
  counted, providing the identity collections are reducible, seqable
  and counted. The single argument version will build a combining fn
  with the supplied identity constructor. Tests for identity
  with (zero? (count x)). See also foldcat."
  ([] (java.util.ArrayList.))
  ([ctor]
   (fn
     ([] (ctor))
     ([left right] (cat left right))))
  ([left right]
   (cond
     (zero? (count left)) right
     (zero? (count right)) left
     :else
     (Cat. (+ (count left) (count right)) left right))))

(defn append!
  ".adds x to acc and returns acc"
  [acc x]
  (doto acc (.add x)))

(defn foldcat
  "Equivalent to (fold cat append! coll)"
  [coll]
  (fold cat append! coll))

(defn monoid
  "Builds a combining fn out of the supplied operator and identity
  constructor. op must be associative and ctor called with no args
  must return an identity value for it."
  [op ctor]
  (fn m
    ([] (ctor))
    ([a b] (op a b))))
