;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)

;; Ported from clojure.zip (Rich Hickey). A loc is a vector [node path] carrying
;; the zipper fns (:zip/branch? :zip/children :zip/make-node) as metadata. The
;; reference indexes a loc with (loc 0)/(loc 1); Jolt uses (nth loc ...) because a
;; metadata-bearing vector is not currently invocable as a fn.
(ns clojure.zip
  "Functional hierarchical zipper, with navigation, editing, and enumeration.")

(defn zipper
  "Creates a new zipper structure. branch? is a fn that, given a node, returns
  true if it can have children. children returns a seq of a branch node's
  children. make-node, given an existing node and a seq of children, returns a
  new branch node. root is the root node."
  [branch? children make-node root]
  (with-meta [root nil]
    {:zip/branch? branch? :zip/children children :zip/make-node make-node}))

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  [root]
  (zipper seq? identity (fn [node children] (with-meta children (meta node))) root))

(defn vector-zip
  "Returns a zipper for nested vectors, given a root vector"
  [root]
  (zipper vector? seq (fn [node children] (with-meta (vec children) (meta node))) root))

(defn node "Returns the node at loc" [loc] (nth loc 0))

(defn branch? "Returns true if the node at loc is a branch"
  [loc] ((:zip/branch? (meta loc)) (node loc)))

(defn children "Returns a seq of the children of node at loc, which must be a branch"
  [loc]
  (if (branch? loc)
    ((:zip/children (meta loc)) (node loc))
    (throw "called children on a leaf node")))

(defn make-node "Returns a new branch node, given an existing node and new children."
  [loc node children] ((:zip/make-node (meta loc)) node children))

(defn path "Returns a seq of nodes leading to this loc" [loc] (:pnodes (nth loc 1)))
(defn lefts "Returns a seq of the left siblings of this loc" [loc] (seq (:l (nth loc 1))))
(defn rights "Returns a seq of the right siblings of this loc" [loc] (:r (nth loc 1)))

(defn down "Returns the loc of the leftmost child of the node at this loc, or nil"
  [loc]
  (when (branch? loc)
    (let [[node path] loc
          [c & cnext :as cs] (children loc)]
      (when cs
        (with-meta [c {:l []
                       :pnodes (if path (conj (:pnodes path) node) [node])
                       :ppath path
                       :r cnext}]
          (meta loc))))))

(defn up "Returns the loc of the parent of the node at this loc, or nil if at the top"
  [loc]
  (let [[node {l :l, ppath :ppath, pnodes :pnodes, r :r, changed? :changed?, :as path}] loc]
    (when pnodes
      (let [pnode (peek pnodes)]
        (with-meta (if changed?
                     [(make-node loc pnode (concat l (cons node r)))
                      (and ppath (assoc ppath :changed? true))]
                     [pnode ppath])
          (meta loc))))))

(defn root "Zips all the way up and returns the root node, reflecting any changes."
  [loc]
  (if (= :end (nth loc 1))
    (node loc)
    (let [p (up loc)]
      (if p (recur p) (node loc)))))

(defn right "Returns the loc of the right sibling of the node at this loc, or nil"
  [loc]
  (let [[node {l :l, [r & rnext :as rs] :r, :as path}] loc]
    (when (and path rs)
      (with-meta [r (assoc path :l (conj l node) :r rnext)] (meta loc)))))

(defn rightmost "Returns the loc of the rightmost sibling of the node at this loc, or self"
  [loc]
  (let [[node {l :l r :r :as path}] loc]
    (if (and path r)
      (with-meta [(last r) (assoc path :l (apply conj l node (butlast r)) :r nil)] (meta loc))
      loc)))

(defn left "Returns the loc of the left sibling of the node at this loc, or nil"
  [loc]
  (let [[node {l :l r :r :as path}] loc]
    (when (and path (seq l))
      (with-meta [(peek l) (assoc path :l (pop l) :r (cons node r))] (meta loc)))))

(defn leftmost "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
  (let [[node {l :l r :r :as path}] loc]
    (if (and path (seq l))
      (with-meta [(first l) (assoc path :l [] :r (concat (rest l) [node] r))] (meta loc))
      loc)))

(defn insert-left "Inserts the item as the left sibling of the node at this loc, without moving"
  [loc item]
  (let [[node {l :l :as path}] loc]
    (if (nil? path)
      (throw "Insert at top")
      (with-meta [node (assoc path :l (conj l item) :changed? true)] (meta loc)))))

(defn insert-right "Inserts the item as the right sibling of the node at this loc, without moving"
  [loc item]
  (let [[node {r :r :as path}] loc]
    (if (nil? path)
      (throw "Insert at top")
      (with-meta [node (assoc path :r (cons item r) :changed? true)] (meta loc)))))

(defn replace "Replaces the node at this loc, without moving"
  [loc node]
  (let [[_ path] loc]
    (with-meta [node (assoc path :changed? true)] (meta loc))))

(defn edit "Replaces the node at this loc with the value of (f node args)"
  [loc f & args]
  (replace loc (apply f (node loc) args)))

(defn insert-child "Inserts the item as the leftmost child of the node at this loc, without moving"
  [loc item]
  (replace loc (make-node loc (node loc) (cons item (children loc)))))

(defn append-child "Inserts the item as the rightmost child of the node at this loc, without moving"
  [loc item]
  (replace loc (make-node loc (node loc) (concat (children loc) [item]))))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. At the end, returns a
  distinguished loc detectable via end?; if already at the end, stays there."
  [loc]
  (if (= :end (nth loc 1))
    loc
    (or
     (and (branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if (up p)
         (or (right (up p)) (recur (up p)))
         [(node p) :end])))))

(defn prev
  "Moves to the previous loc in the hierarchy, depth-first. At the root, returns nil."
  [loc]
  (if-let [lloc (left loc)]
    (loop [loc lloc]
      (if-let [child (and (branch? loc) (down loc))]
        (recur (rightmost child))
        loc))
    (up loc)))

(defn end? "Returns true if loc represents the end of a depth-first walk"
  [loc] (= :end (nth loc 1)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded it in a
  depth-first walk."
  [loc]
  (let [[node {l :l, ppath :ppath, pnodes :pnodes, rs :r, :as path}] loc]
    (if (nil? path)
      (throw "Remove at top")
      (if (pos? (count l))
        (loop [loc (with-meta [(peek l) (assoc path :l (pop l) :changed? true)] (meta loc))]
          (if-let [child (and (branch? loc) (down loc))]
            (recur (rightmost child))
            loc))
        (with-meta [(make-node loc (peek pnodes) rs)
                    (and ppath (assoc ppath :changed? true))]
          (meta loc))))))
