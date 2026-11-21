(ns jolt.native.stack
  "Which arrays a function can keep on the C stack instead of the heap: a
  (double-array n) or (long-array n) with a small constant n whose value never
  leaves the function and never outlives the iteration that made it. Such an
  array needs no calloc, no out-of-memory check and no free.

  An array's value is followed through every name and node that can hold it
  (its sources, as allocation sites). A site stays on the heap when its value
  can be the function's result, be taken by a helper (which may free it), be
  carried to the next iteration by recur or out of a loop, or share a holder
  with a heap array (one free, one decision). What remains is freed by the
  frame: the emitter declares a buffer per site and skips the frees of names
  that only ever hold those. The idea is Ferret's escape analysis
  (github.com/nakkaya/ferret), which stack-allocates what never leaves a
  function."
  (:require [jolt.native.ast :as ast]))

;; elements; 256 doubles is 2KB of stack per site
(def max-elems 256)

;; {holder-id #{site-id}}: what each node id and binding id can hold
(def ^:dynamic *srcs* nil)
;; site ids that leave the function, an iteration, or reach a taking helper
(def ^:dynamic *escapes* nil)
;; loop-id -> [binding-id]
(def ^:dynamic *loops* nil)
(def ^:dynamic *takes* nil)

(defn- hold! [id s] (when (seq s) (swap! *srcs* update id (fnil into #{}) s)) s)
(defn- escape! [s] (swap! *escapes* into s))

(declare walk)

(defn- walk-call [node]
  (let [taken (get-in *takes* [(:instance node) :positions] #{})]
    (doseq [[i a] (map-indexed vector (:args node))]
      (let [s (walk a)] (when (taken i) (escape! s))))
    ;; an owned result is the helper's allocation (or one it took)
    (when (ast/owned? (:type node)) (hold! (:id node) #{(:id node)}))))

(defn- walk-loop [node]
  (swap! *loops* assoc (:loop-id node) (mapv :id (:bindings node)))
  (doseq [b (:bindings node)] (hold! (:id b) (walk (:init b))))
  (let [s (walk (:body node))]
    ;; the value a loop answers survives the iteration that made it
    (escape! s)
    (hold! (:id node) s)))

(defn walk
  "The allocation sites node's value can be, recording holders as it goes."
  [node]
  (case (:op node)
    :new-array (do (walk (:len node)) (hold! (:id node) #{(:id node)}))
    :aclone (do (walk (:arr node)) (hold! (:id node) #{(:id node)}))
    :local (get @*srcs* (:binding node) #{})
    :let (do (doseq [b (:bindings node)] (hold! (:id b) (walk (:init b))))
             (hold! (:id node) (walk (:body node))))
    :do (do (run! walk (:statements node)) (hold! (:id node) (walk (:ret node))))
    :if (do (walk (:test node))
            (hold! (:id node) (into (walk (:then node)) (walk (:else node)))))
    :loop (walk-loop node)
    :recur (let [ids (get @*loops* (:loop-id node))]
             (doseq [[id a] (map vector ids (:args node))]
               (let [s (walk a)] (escape! s) (hold! id s)))
             #{})
    :call (walk-call node)
    (do (doseq [k [:args :arr :idx :val :len :test]]
          (let [v (get node k)]
            (cond (map? v) (walk v) (sequential? v) (run! walk v))))
        #{})))

(defn- candidate? [node]
  (and (= :new-array (:op node))
       (= :const (:op (:len node)))
       (<= 0 (:val (:len node)) max-elems)))

(defn- nodes [node]
  (tree-seq map? (fn [n] (filter map? (concat (vals n) (mapcat #(when (sequential? %) %) (vals n))
                                              (keep :init (:bindings n)))))
            node))

(defn- settle
  "Drop a stack site from every holder that also holds a heap one, until none
  does: a name is freed or not as a whole."
  [stack srcs]
  (let [mixed (fn [s] (some (fn [h] (when (and (some s h) (not (every? s h))) h)) (vals srcs)))]
    (loop [s stack]
      (if-let [h (mixed s)] (recur (apply disj s h)) s))))

(defn analyze-fn
  "For one function body: {:sites {site-id elems} :holders #{id}}, the stack
  arrays and the names (node and binding ids) whose frees the frame does."
  [body takes]
  (binding [*srcs* (atom {}) *escapes* (atom #{}) *loops* (atom {}) *takes* takes]
    ;; twice: a loop variable's sources include what its recurs pass, which the
    ;; first walk only learns after the uses that read it
    (dotimes [_ 3] (escape! (walk body)))
    (let [cands (into {} (map (fn [n] [(:id n) (:val (:len n))])) (filter candidate? (nodes body)))
          stack (settle (apply disj (set (keys cands)) @*escapes*) @*srcs*)]
      {:sites (select-keys cands stack)
       :holders (set (for [[id s] @*srcs* :when (and (seq s) (every? stack s))] id))})))

(defn analyze-region
  "{:entry analysis, iid analysis ...} for a checked region."
  [region]
  (into {:entry (analyze-fn (:body region) (:takes region))}
        (mapcat (fn [inst]
                  (cons [(:iid inst) (analyze-fn (:body inst) (:takes region))]
                        (map-indexed (fn [i v] [[(:iid inst) i] (analyze-fn (:body v) (:takes region))])
                                     (:variants inst)))))
        (:instances region)))
