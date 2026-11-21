;;   Portions of this file are adapted from Carp (https://github.com/carp-lang/Carp),
;;   src/Memory.hs, Copyright (c) Erik Svedäng and contributors.
;;   Apache License 2.0 (licenses/Apache-2.0.txt).

(ns jolt.native.memory
  "Ownership checking for native regions, ported from Carp's Memory.hs.

  The state is the set of live owned values (Carp's memStateDeleters), each
  named by the id of the binding or IR node that produced it. manage adds a
  value, unmanage consumes it (a move) and fails if it is already gone, and
  transfer is the two together. The walk annotates the tree with where each
  value is freed:

    :let / :loop   :delete  -- freed when the scope ends (Carp's LetPat diff)
    :if            :delete-then / :delete-else  (Carp's IfPat reconciliation)
    :recur         :delete  -- freed before jumping back to the loop head
    region root    :delete  -- freed before the region returns
    fallible node  :live    -- everything to free if it raises

  The arrays a region borrows (its parameters) are never managed: they are the
  caller's. aget/aset/alength/aclone borrow their array argument, which must be
  alive; everything else that takes an owned value consumes it.

  loop/recur is the one rule Carp has no counterpart for (Carp has while). A
  recur frees everything created since the loop head, and every value that was
  live on entry to the loop must still be live at the recur: an iteration that
  continues may not consume an outer value, because the next one, or the exit,
  would use it again. An exit path may consume outer values once. The proof is
  test/native/proofs/loop-recur.smt2; the if rule's is if-reconcile.smt2."
  (:require [clojure.set :as set]
            [jolt.native.ast :as ast]))

(def ^:dynamic *state* nil)

(defn- owned? [node] (ast/owned? (:type node)))
(defn- var-of [node] (if (= :local (:op node)) (:binding node) (:id node)))
(defn- deleters [] (:deleters @*state*))
(defn- set-deleters! [s] (swap! *state* assoc :deleters s))

(defn- manage! [node]
  (when (owned? node)
    (swap! *state* update :deleters conj (var-of node))))

(defn- manage-id! [id] (swap! *state* update :deleters conj id))

(defn- unmanage!
  "Consume node's value. Carp: UsingUnownedValue when it is no longer live."
  [node]
  (when (owned? node)
    (let [v (var-of node)]
      (if (contains? (deleters) v)
        (swap! *state* update :deleters disj v)
        (ast/fail (:form node)
                  (str "uses " (pr-str (:form node))
                       " after it was given away (moved); use (aclone x) to keep a copy"))))))

(defn- transfer! [from to-id]
  (unmanage! from)
  (when (owned? from) (manage-id! to-id)))

(defn- can-be-referenced!
  "Carp's canBeReferenced: a borrow needs a live owner."
  [node]
  (when (and (owned? node) (not (contains? (deleters) (var-of node))))
    (ast/fail (:form node)
              (str "borrows " (pr-str (:form node)) " after it was given away (moved)"))))

(defn- snapshot [node] (assoc node :live (deleters)))

(declare visit)

(defn- diverges? [node] (= :never (:type node)))

(defn- visit-borrowing
  "Visit the array an op borrows. The borrow is checked where the op USES it,
  after its other arguments ran (borrow-checked!): an index expression that
  moves the array, (aset a (let [b a] 0) 1.0), would otherwise free it between
  the check and the store."
  [node k]
  (visit (k node)))

(defn- borrow-checked! [arr] (can-be-referenced! arr) arr)

(defn- visit-let [node]
  (let [pre (deleters)
        bs (mapv (fn [b]
                   (let [init (visit (:init b))]
                     ;; Carp's exclusiveTransferOwnership: the binding's id is
                     ;; fresh, so a shadowed outer name keeps its own deleter
                     (transfer! init (:id b))
                     (assoc b :init init)))
                 (:bindings node))
        body (visit (:body node))]
    (if (diverges? body)
      ;; control never reaches the let's end: the recur inside freed it all
      (do (set-deleters! pre)
          (assoc node :bindings bs :body body :delete #{}))
      (do (unmanage! body)
          (let [post (deleters)
                diff (set/difference post pre)]
            (set-deleters! (set/difference post diff))
            (manage! node)
            (assoc node :bindings bs :body body :delete diff))))))

(defn- run-branch
  "Visit a branch from state s, transferring its value to the if node.
  Answers [visited-branch post-deleters]."
  [s branch if-node]
  (reset! *state* s)
  (let [b (visit branch)]
    (when-not (diverges? b)
      (unmanage! b)
      (when (owned? if-node) (manage-id! (:id if-node))))
    [b (deleters)]))

(defn- visit-if [node]
  (let [test (visit (:test node))
        pre-state @*state*
        pre (:deleters pre-state)
        [then post-t] (run-branch pre-state (:then node) node)
        [else post-f] (run-branch pre-state (:else node) node)
        self (if (owned? node) #{(:id node)} #{})
        t-div (diverges? then)
        f-div (diverges? else)]
    (cond
      (and t-div f-div)
      (do (reset! *state* pre-state)
          (assoc node :test test :then then :else else :delete-then #{} :delete-else #{}))

      (or t-div f-div)
      ;; only the non-diverging branch reaches the join; the recur in the other
      ;; one frees what it must itself
      (let [post (if t-div post-f post-t)
            created (set/difference (set/difference post pre) self)]
        (reset! *state* (assoc pre-state :deleters (set/difference post created)))
        (assoc node :test test :then then :else else
               :delete-then (if t-div #{} created)
               :delete-else (if f-div #{} created)))

      :else
      ;; Carp's IfPat reconciliation, verbatim
      (let [deleted-t (set/difference pre post-t)
            deleted-f (set/difference pre post-f)
            both (set/intersection deleted-t deleted-f)
            created-t (set/difference (set/difference post-t pre) self)
            created-f (set/difference (set/difference post-f pre) self)
            dels-t (set/union (set/difference deleted-f both) created-t)
            dels-f (set/union (set/difference deleted-t both) created-f)
            alive (set/difference pre (set/union deleted-t deleted-f))]
        (reset! *state* (assoc pre-state :deleters (set/union alive self)))
        (assoc node :test test :then then :else else
               :delete-then dels-t :delete-else dels-f)))))

(defn- visit-loop [node]
  (let [pre (deleters)
        bs (mapv (fn [b]
                   (let [init (visit (:init b))]
                     (assoc b :init init)))
                 (:bindings node))
        ;; values live at the loop head, other than the loop vars themselves
        outer (do (doseq [b bs] (unmanage! (:init b))) (deleters))
        _ (doseq [b bs] (when (ast/owned? (:type (:init b))) (manage-id! (:id b))))
        loops (:loops @*state*)
        _ (swap! *state* assoc :loops (assoc loops (:loop-id node) outer))
        body (visit (:body node))
        _ (swap! *state* assoc :loops loops)]
    (if (diverges? body)
      (do (set-deleters! pre) (assoc node :bindings bs :body body :delete #{}))
      (do (unmanage! body)
          (let [post (deleters)
                diff (set/difference post pre)]
            (set-deleters! (set/difference post diff))
            (manage! node)
            (assoc node :bindings bs :body body :delete diff))))))

(defn- visit-recur [node]
  (let [args (mapv visit (:args node))
        outer (get-in @*state* [:loops (:loop-id node)])]
    (doseq [a args] (unmanage! a))
    (let [live (deleters)
          gone (set/difference outer live)]
      (when (seq gone)
        (ast/fail (:form node)
                  (str "a loop iteration that continues gives away "
                       (pr-str (get-in @*state* [:names (first gone)] (first gone)))
                       ", which the next iteration or the loop's exit would use again;"
                       " give it away only on the path that leaves the loop")))
      (set-deleters! outer)
      (assoc node :args args :delete (set/difference live outer)))))

(defn- visit-args [node]
  (assoc node :args (mapv (fn [a] (let [v (visit a)] (unmanage! v) v)) (:args node))))

(def ^:private fallible-prims
  #{:+ :- :* :neg :inc :dec :quot :rem :mod :to-long})

(defn visit [node]
  (case (:op node)
    :const node
    :local (do (when (ast/owned? (:type node))
                 (swap! *state* update :names assoc (:binding node) (:name node)))
               node)
    :let (visit-let node)
    :loop (visit-loop node)
    :recur (visit-recur node)
    :if (visit-if node)
    :do (let [stmts (mapv visit (:statements node))
              ret (visit (:ret node))]
          (when-not (diverges? ret) (transfer! ret (:id node)))
          (assoc node :statements stmts :ret ret))
    :prim (let [n (visit-args node)
                long-arith (and (= :long (:type node)) (fallible-prims (:prim node)))]
            (if (or long-arith (= :to-long (:prim node))) (snapshot n) n))
    :math (visit-args node)
    :alength (assoc node :arr (borrow-checked! (visit-borrowing node :arr)))
    :aget (let [arr (visit-borrowing node :arr)
                idx (visit (:idx node))]
            (borrow-checked! arr)
            (snapshot (assoc node :arr arr :idx idx)))
    ;; :live-idx is what is live when the index has run: the index cast fails
    ;; there, before the value runs (emit-index-cast)
    :aset (let [arr (visit-borrowing node :arr)
                idx (visit (:idx node))
                live-idx (deleters)
                v (visit (:val node))]
            (borrow-checked! arr)
            (snapshot (assoc node :arr arr :idx idx :val v :live-idx live-idx)))
    :aclone (let [arr (borrow-checked! (visit-borrowing node :arr))
                  n (snapshot (assoc node :arr arr))]
              (manage! node)
              n)
    :new-array (let [len (visit (:len node))
                     n (snapshot (assoc node :len len))]
                 (manage! node)
                 n)))

(defn check-region
  "Run the ownership pass over an analyzed region. Answers the region with
  :body annotated and :delete, the owned values to free before returning."
  [region]
  (binding [*state* (atom {:deleters #{} :names {} :loops {}})]
    (let [body (visit (:body region))]
      (when-not (diverges? body) (unmanage! body))
      (assoc region :body body :delete (deleters)))))
