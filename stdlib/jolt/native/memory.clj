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
;; helper instance iid -> {:positions #{arg index} :why {index text}}: which
;; arguments each native helper takes ownership of (inferred, check-region)
(def ^:dynamic *takes* {})
;; how the value being consumed is given away, for the reason a helper takes a
;; parameter: "returns it", "binds it to a new name", ...
(def ^:dynamic *why* "gives it away")
;; Facts about the whole body, kept outside *state* because the if and loop
;; rules snapshot and restore *state* per branch: a parameter given away on ANY
;; path is taken (id -> how), and why each moved value went (id -> text).
(def ^:dynamic *consumed* nil)
(def ^:dynamic *moved* nil)

(defn- owned? [node] (ast/owned? (:type node)))
(defn- var-of [node] (if (= :local (:op node)) (:binding node) (:id node)))
(defn- deleters [] (:deleters @*state*))
(defn- set-deleters! [s] (swap! *state* assoc :deleters s))

(defn- manage! [node]
  (when (owned? node)
    (swap! *state* update :deleters conj (var-of node))))

(defn- manage-id! [id] (swap! *state* update :deleters conj id))

(defn- ordinal [n] (case n 1 "1st" 2 "2nd" 3 "3rd" (str n "th")))

(defn- moved-note
  "Why a value is gone, when a helper took it: the explanatory half of the error."
  [v]
  (when-let [m (get @*moved* v)] (str "; " m)))

(defn- unmanage!
  "Consume node's value. Carp: UsingUnownedValue when it is no longer live."
  [node]
  (when (owned? node)
    (let [v (var-of node)]
      (if (contains? (deleters) v)
        (do (swap! *state* update :deleters disj v)
            ;; a helper parameter given away here is one the helper takes
            (when (and (contains? (:candidates @*state*) v)
                       (not (contains? @*consumed* v)))
              (swap! *consumed* assoc v *why*)))
        (ast/fail (:form node)
                  (str "uses " (pr-str (:form node))
                       " after it was given away (moved)" (moved-note v)
                       "; use (aclone x) to keep a copy"))))))

(defn- transfer! [from to-id]
  (unmanage! from)
  (when (owned? from) (manage-id! to-id)))

(defn- can-be-referenced!
  "Carp's canBeReferenced: a borrow needs a live owner."
  [node]
  (when (and (owned? node) (not (contains? (deleters) (var-of node))))
    (ast/fail (:form node)
              (str "borrows " (pr-str (:form node)) " after it was given away (moved)"
                   (moved-note (var-of node)) "; use (aclone x) to keep a copy"))))

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
                     (binding [*why* "binds it to a new name"] (transfer! init (:id b)))
                     (assoc b :init init)))
                 (:bindings node))
        body (visit (:body node))]
    (if (diverges? body)
      ;; control never reaches the let's end: the recur inside freed it all
      (do (set-deleters! pre)
          (assoc node :bindings bs :body body :delete #{}))
      (do (binding [*why* "returns it"] (unmanage! body))
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
    ;; only an owned if takes its branch's value; a unit or scalar if leaves an
    ;; owned branch value in the branch's own deleters, freed at its end
    (when (and (not (diverges? b)) (owned? if-node))
      (binding [*why* "returns it"] (unmanage! b))
      (manage-id! (:id if-node)))
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
    (binding [*why* "passes it to recur"] (doseq [a args] (unmanage! a)))
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
          (when-not (diverges? ret) (binding [*why* "returns it"] (transfer! ret (:id node))))
          (assoc node :statements stmts :ret ret))
    :prim (let [n (visit-args node)
                long-arith (and (= :long (:type node)) (fallible-prims (:prim node)))]
            (if (or long-arith (= :to-long (:prim node))) (snapshot n) n))
    :math (visit-args node)
    ;; a C function borrows the arrays it is passed, for the call
    :extern (let [args (mapv visit (:args node))]
              (doseq [a args] (if (ast/array-type? (:type a)) (borrow-checked! a) (unmanage! a)))
              (assoc node :args args))
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
    ;; a native helper borrows every array it is passed (it can neither free nor
    ;; return one), so each owned argument must be live at the call, after all
    ;; the arguments ran; it can fail, and an owned result is the caller's
    ;; a helper takes the arguments its instance gives away (inferred: *takes*)
    ;; and borrows the rest. A taken argument is moved, like binding it to a new
    ;; name; a later use explains which helper took it and why.
    :call (let [args (mapv visit (:args node))
                {:keys [positions why]} (get *takes* (:instance node))]
            ;; Move every taken argument before checking the borrowed ones: the
            ;; helper may free a taken array before it reads a borrowed one, so
            ;; (h x x) with h borrowing its first and taking its second must be
            ;; refused whichever order the parameters come in.
            (doseq [[i a] (map-indexed vector args)
                    :when (contains? positions i)]
              (binding [*why* (str "passes it to " (:helper-sym node) ", which takes it")]
                (unmanage! a))
              (when (owned? a)
                (swap! *moved* assoc (var-of a)
                       (str (:helper-sym node) " took ownership of its "
                            (ordinal (inc i)) " argument (it " (get why i) ")"))))
            (doseq [[i a] (map-indexed vector args)
                    :when (not (contains? positions i))]
              (borrow-checked! a))
            (let [n (snapshot (assoc node :args args))]
              (manage! node)
              n))
    :new-array (let [len (visit (:len node))
                     n (snapshot (assoc node :len len))]
                 (manage! node)
                 n)))

(defn- strip-ids
  "Remove ids from every :live set in a checked body: parameters a helper only
  borrowed are its caller's to free, even on the helper's error paths."
  [node ids]
  (cond
    (map? node) (into {} (map (fn [[k v]] [k (if (= k :live) (set/difference v ids) (strip-ids v ids))]) node))
    (vector? node) (mapv #(strip-ids % ids) node)
    :else node))

(defn- check-body
  "Check one function body. A region's parameters are borrowed or scalar, so
  nothing starts managed; a helper instance's candidate parameters (arrays the
  region owns) start managed, and the ones the body never gives away are then
  stripped back out: borrowed, not freed. Answers [body delete consumed]."
  [body candidates]
  (binding [*state* (atom {:deleters (set candidates) :names {} :loops {}
                           :candidates (set candidates)})
            *consumed* (atom {})
            *moved* (atom {})]
    (let [body (visit body)]
      (when-not (diverges? body) (binding [*why* "returns it"] (unmanage! body)))
      (let [consumed @*consumed*
            borrowed (set/difference (set candidates) (set (keys consumed)))]
        [(strip-ids body borrowed) (set/difference (deleters) borrowed) consumed]))))

(defn- check-variant
  "Check one body of a helper instance: [checked-body delete takes]."
  [inst body]
  (let [cands (keep (fn [p] (when (:candidate p) (:id p))) (:params inst))
        [b d consumed] (check-body body cands)
        idx (into {} (map-indexed (fn [i p] [(:id p) i]) (:params inst)))]
    [b d {:positions (set (map idx (keys consumed)))
          :why (into {} (map (fn [[id why]] [(idx id) why]) consumed))}]))

(defn- check-instance
  "A helper instance checked: each platform body (defnative's on) separately,
  and they must take the same arguments, or a caller could not know whether it
  still owns what it passed."
  [inst]
  (if-let [vs (:variants inst)]
    (let [checked (mapv #(check-variant inst (:body %)) vs)
          positions (distinct (map #(:positions (nth % 2)) checked))
          vs (mapv (fn [v [b d _]] (assoc v :body b :delete d)) vs checked)
          default (or (first (filter #(= :default (:guard %)) vs)) (first vs))]
      (when (< 1 (count positions))
        (ast/fail (:form (:body inst))
              (str "native helper " (:sym inst) "'s platform bodies take different arguments;"
                   " each must give away (or keep) the same ones")))
      [(assoc inst :variants vs :body (:body default) :delete (:delete default))
       (nth (first checked) 2)])
    (let [[b d takes] (check-variant inst (:body inst))]
      [(assoc inst :body b :delete d) takes])))

(defn check-region
  "Run the ownership pass over an analyzed region and each native helper
  instance it calls. Which arguments a helper takes is inferred, to a fixpoint
  since helpers call helpers (a parameter passed on to one that takes it is
  taken too). Answers the region with every :body annotated and a :delete per
  body, the owned values to free before it returns."
  [region]
  (let [[insts takes]
        (loop [takes {} n 0]
          (let [checked (binding [*takes* takes] (mapv check-instance (:instances region)))
                takes' (into {} (map (fn [[i t]] [(:iid i) t]) checked))]
            (cond
              (= takes' takes) [(mapv first checked) takes']
              (> n 64) (throw (ex-info "native: which arguments the helpers take did not settle after 64 rounds"
                                       {:instances (count (:instances region))}))
              :else (recur takes' (inc n)))))
        [body delete] (binding [*takes* takes] (check-body (:body region) nil))]
    (assoc region :body body :delete delete :instances insts :takes takes)))
