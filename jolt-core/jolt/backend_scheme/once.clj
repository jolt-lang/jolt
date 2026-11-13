(ns jolt.backend-scheme.once
  "^:once captures for the Scheme back end: the liveness walk that marks where
  a once fn's body reads each capture for the last time, and the stores that
  release it there."
  (:require [clojure.string :as str]
            [jolt.ir :as ir]
            [jolt.backend-scheme.session :refer [fresh-label]]))

;; ^:once captures released at their last use (see once-last-uses, near emit-fn).
;; While a once fn's body emits, *once-clears* maps each capture's name to the
;; Scheme store that empties its slot in the closure's box; a node marked
;; :clear-caps gets those stores after it is evaluated.
(def ^:dynamic *once-clears* nil)
(defn once-clear-stmts [names]
  (when *once-clears*
    (let [ss (keep (fn [n] (get *once-clears* n)) names)]
      (when (seq ss) (str/join " " ss)))))
;; S (a node's emitted text) followed by the stores its :clear-caps call for.
(defn with-once-clears [node s]
  (let [st (when-let [cc (:clear-caps node)] (once-clear-stmts cc))]
    (cond
      (nil? st) s
      (= :local (:op node)) (str "(begin " st " " s ")")
      :else (let [v (fresh-label "_cv$")]
              (str "(let ((" v " " s ")) " st " " v ")")))))

;; ---------------------------------------------------------------------------
;; ^:once captures, released at their last use.
;;
;; The reference's compiler nulls a ^:once fn's closed-over field at the point of
;; its LAST USE on the path the body takes, so a body that walks a source does not
;; pin it, and a body that throws and is forced again finds every capture it had
;; not yet finished with (LazySeq reruns fn after a failure). Clearing them all on
;; entry released as much and diverged on the rerun (jolt-4s12.3).
;;
;; once-last-uses marks, in a once fn's body, the nodes after whose evaluation a
;; capture is dead: a :local read of it that nothing on the path reads again, or
;; a nested :fn that captures it last. The mark is :clear-caps, a vector of names,
;; which emit turns into the stores that let the captures go. The body reads each
;; capture into a local on entry, so the store changes no value the body sees --
;; only when the fn's own slot lets go.
;;
;; Backward liveness in evaluation order: `live` is the set of captures read
;; later on the path. A capture read inside a loop is live across the whole loop
;; (the next iteration reads it), and one read by a catch or finally is live
;; across the try's body (a throw anywhere in it reaches them). A rebinding of a
;; capture's name hides it for the rebinding's scope.

;; The captures in CAPS a node reads, outside any rebinding of them, including
;; through nested fns (whose own params hide names too).
(declare once-cap-uses)

;; A nested fn: each arity's params, rest param and the fn's own name hide the
;; captures they rebind, for that arity's body.
(defn- once-fn-cap-uses [node caps]
  (reduce (fn [acc a]
            (let [hidden (cond-> (set (:params a)) (:rest a) (conj (:rest a))
                           (:name node) (conj (:name node)))]
              (into acc (once-cap-uses (:body a) (reduce disj caps hidden)))))
          #{} (:arities node)))

;; let/loop: each init sees the captures the names bound before it leave visible
;; (a letrec's init, none of the group's names), and the body sees what all of
;; them leave.
(defn- once-binding-cap-uses [node caps]
  (let [letrec? (:letrec node)
        names (map first (:bindings node))
        all-caps (reduce disj caps names)]
    (loop [bs (:bindings node) cs (if letrec? all-caps caps) acc #{}]
      (if (empty? bs)
        (into acc (once-cap-uses (:body node) all-caps))
        (let [[nm init] (first bs)]
          (recur (rest bs) (if letrec? cs (disj cs nm))
                 (into acc (once-cap-uses init cs))))))))

(defn- once-cap-uses [node caps]
  (if (empty? caps)
    #{}
    (case (:op node)
      :local (if (contains? caps (:name node)) #{(:name node)} #{})
      :fn (once-fn-cap-uses node caps)
      (:let :loop) (once-binding-cap-uses node caps)
      (ir/reduce-ir-children (fn [acc c] (into acc (once-cap-uses c caps))) #{} node))))

;; Does the body loop back to the fn's own head (a recur outside any loop or
;; nested fn)? Then the whole body is a loop.
(defn- once-self-recur? [node]
  (let [op (:op node)]
    (cond
      (= op :recur) true
      (or (= op :loop) (= op :fn)) false
      :else (ir/reduce-ir-children (fn [acc c] (or acc (once-self-recur? c))) false node))))

(defn- mark-clear [node dead]
  (if (empty? dead) node (assoc node :clear-caps (vec (sort dead)))))

;; [node' live-before]
(declare once-mark)
(defn- once-mark-seq [nodes caps live]
  ;; nodes in evaluation order; walk them backward
  (loop [i (dec (count nodes)) live live acc ()]
    (if (neg? i)
      [(vec acc) live]
      (let [[n l] (once-mark (nth nodes i) caps live)]
        (recur (dec i) l (cons n acc))))))

(defn- once-mark-local [node caps live]
  (let [nm (:name node)]
    (if (contains? caps nm)
      [(if (contains? live nm) node (mark-clear node #{nm})) (conj live nm)]
      [node live])))

;; Both branches start from what is live after the if; the test sees the union.
(defn- once-mark-if [node caps live]
  (let [[t lt] (once-mark (:then node) caps live)
        [e le] (once-mark (:else node) caps live)
        [c lc] (once-mark (:test node) caps (into lt le))]
    [(assoc node :test c :then t :else e) lc]))

;; The scope each init of BS sees: the names bound before it are hidden (all of
;; them, for a letrec, whose inits see BODY-CAPS).
(defn- once-init-scopes [bs caps body-caps letrec?]
  (if letrec?
    (vec (repeat (count bs) body-caps))
    (loop [bs bs cs caps acc []]
      (if (empty? bs) acc
          (recur (rest bs) (disj cs (first (first bs))) (conj acc cs))))))

(defn- once-mark-binding [node caps live]
  (let [bs (:bindings node)
        names (map first bs)
        body-caps (reduce disj caps names)
        ;; a loop's body runs again: what it reads is live all through it
        body-live (if (= (:op node) :loop) (into live (once-cap-uses (:body node) body-caps)) live)
        [body lb] (once-mark (:body node) body-caps body-live)
        scopes (once-init-scopes bs caps body-caps (:letrec node))
        [inits l] (loop [i (dec (count bs)) live lb acc ()]
                    (if (neg? i)
                      [(vec acc) live]
                      (let [[n l] (once-mark (second (nth bs i)) (nth scopes i) live)]
                        (recur (dec i) l (cons n acc)))))]
    [(assoc node :bindings (mapv (fn [b init] [(first b) init]) bs inits) :body body) l]))

;; A catch or finally may run after a throw anywhere in the body, so what they
;; read is live all through it.
(defn- once-mark-try [node caps live]
  (let [[f lf] (if (:finally node) (once-mark (:finally node) caps live) [nil live])
        [c lc] (if (:catch-body node) (once-mark (:catch-body node) caps lf) [nil lf])
        [b lb] (once-mark (:body node) caps (into lf lc))
        n (assoc node :body b)
        n (if f (assoc n :finally f) n)
        n (if c (assoc n :catch-body c) n)]
    [n lb]))

;; Any other node evaluates its children in order.
(defn- once-mark-children [node caps live]
  (let [kids (ir/reduce-ir-children conj [] node)
        [kids' l] (once-mark-seq kids caps live)
        i (atom -1)]
    [(ir/map-ir-children (fn [_] (nth kids' (swap! i inc))) node) l]))

(defn- once-mark [node caps live]
  (if (empty? caps)
    [node live]
    (case (:op node)
      :local (once-mark-local node caps live)
      :fn (let [used (once-cap-uses node caps)]
            [(mark-clear node (remove live used)) (into live used)])
      :if (once-mark-if node caps live)
      (:let :loop) (once-mark-binding node caps live)
      :try (once-mark-try node caps live)
      (once-mark-children node caps live))))

(defn once-last-uses [body cap-names]
  (let [caps (set cap-names)]
    (if (once-self-recur? body)
      ;; the whole body loops: nothing is read for the last time inside it, and
      ;; the box goes when the closure does
      body
      (first (once-mark body caps #{})))))

