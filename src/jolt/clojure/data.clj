;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.

;; Ported from clojure.data (Stuart Halloway). The reference dispatches via the
;; EqualityPartition/Diff protocols extended over host types; Jolt uses a plain
;; equality-partition fn over its own predicates instead — same behaviour, no
;; host-type protocol plumbing.
(ns clojure.data
  "Non-core data functions."
  (:require [clojure.set :as set]))

(declare diff)

(defn- atom-diff [a b]
  (if (= a b) [nil nil a] [a b nil]))

;; Convert an associative-by-numeric-index collection into an equivalent vector,
;; with nil for any missing keys.
(defn- vectorize [m]
  (when (seq m)
    (reduce
     (fn [result [k v]] (assoc result k v))
     (vec (repeat (apply max (keys m)) nil))
     m)))

(defn- diff-associative-key
  "Diff associative things a and b, comparing only the key k."
  [a b k]
  (let [va (get a k)
        vb (get b k)
        [a* b* ab] (diff va vb)
        in-a (contains? a k)
        in-b (contains? b k)
        same (and in-a in-b
                  (or (not (nil? ab))
                      (and (nil? va) (nil? vb))))]
    [(when (and in-a (or (not (nil? a*)) (not same))) {k a*})
     (when (and in-b (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})]))

(defn- diff-associative
  "Diff associative things a and b, comparing only keys in ks."
  [a b ks]
  (reduce
   (fn [diff1 diff2] (doall (map merge diff1 diff2)))
   [nil nil nil]
   (map (partial diff-associative-key a b) ks)))

(defn- diff-sequential [a b]
  (vec (map vectorize (diff-associative
                       (if (vector? a) a (vec a))
                       (if (vector? b) b (vec b))
                       (range (max (count a) (count b)))))))

(defn- diff-set [a b]
  [(not-empty (set/difference a b))
   (not-empty (set/difference b a))
   (not-empty (set/intersection a b))])

(defn- equality-partition [x]
  (cond
    (nil? x) :atom
    (map? x) :map
    (set? x) :set
    (sequential? x) :sequential
    :else :atom))

(defn- diff-similar [a b]
  ((case (equality-partition a)
     :atom atom-diff
     :set diff-set
     :sequential diff-sequential
     :map (fn [a b] (diff-associative a b (set/union (keys a) (keys b)))))
   a b))

(defn diff
  "Recursively compares a and b, returning a tuple of
  [things-only-in-a things-only-in-b things-in-both].
  Comparison rules:

  * For equal a and b, return [nil nil a].
  * Maps are subdiffed where keys match and values differ.
  * Sets are never subdiffed.
  * All sequential things are treated as associative collections
    by their indexes, with results returned as vectors.
  * Everything else (including strings!) is treated as
    an atom and compared for equality."
  [a b]
  (if (= a b)
    [nil nil a]
    (if (= (equality-partition a) (equality-partition b))
      (diff-similar a b)
      (atom-diff a b))))
