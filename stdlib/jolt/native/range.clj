(ns jolt.native.range
  "Range facts for a checked region: which long arithmetic cannot overflow, which
  index already fits an int, and which array access is in bounds, so the C
  emitter can leave those checks out of a fast copy of the function.

  The facts hold under assumptions about the function's array parameters (each
  no longer than Integer/MAX_VALUE, and longer than the largest index proven
  for it). The emitter turns those into one guard at the function's entry: when
  it holds, the fast copy runs; when not, the fully checked body runs, as
  before. So a fact never changes what a program does, only which checks run.

  Intervals are over exact integers (+' and *'): [lo hi], or nil for a value
  that is not a long. Loop variables reach a fixpoint, widened to the long
  range after a few rounds; a comparison narrows its locals in each branch.
  The rules are proved in test/native/proofs/range-*.smt2."
  (:require [jolt.native.ast :as ast]))

(def lmin Long/MIN_VALUE)
(def lmax Long/MAX_VALUE)
(def imin Integer/MIN_VALUE)
(def imax Integer/MAX_VALUE)
(def full [lmin lmax])
(def fixnum [-1152921504606846976 1152921504606846975])
(def int-len [0 imax])

(defn fits? [[lo hi] [a b]] (and lo (<= a lo) (<= hi b)))
(defn- clamp [[lo hi]] [(max lo lmin) (min hi lmax)])
(defn- join [x y] (cond (nil? x) y (nil? y) x
                        :else [(min (x 0) (y 0)) (max (x 1) (y 1))]))
(defn- r-add [[a b] [c d]] [(+' a c) (+' b d)])
(defn- r-sub [[a b] [c d]] [(-' a d) (-' b c)])
(defn- r-mul [[a b] [c d]]
  (let [ps [(*' a c) (*' a d) (*' b c) (*' b d)]] [(apply min ps) (apply max ps)]))

;; --- analysis state ----------------------------------------------------------

;; facts: {node-id fact}; the last visit of a node wins, and the last visit is
;; the one made with every enclosing loop at its fixpoint
(def ^:dynamic *facts* nil)
;; recur ranges per loop id, for the round under way
(def ^:dynamic *recurs* nil)
;; {param-id max-index}: the length each array parameter must exceed
(def ^:dynamic *needs* nil)
;; array parameter ids, whose lengths the guard bounds
(def ^:dynamic *params* nil)

(defn- fact! [id f] (swap! *facts* assoc id f))

;; env: {:r {local-id interval} :lt {local-id #{array-local-id}}
;;       :len-of {local-id array-local-id} :alen {array-local-id interval}}

(defn- local-id [node] (when (= :local (:op node)) (:binding node)))

(defn- alen
  "The length interval of the array an expression names."
  [env node]
  (or (when-let [id (local-id node)] (get-in env [:alen id])) int-len))

(declare rng long-prim-rng)

(defn- fold-arith
  "A left fold of + - *: each pair's interval, and whether it stays a long."
  [node env]
  (let [rs (mapv #(rng % env) (:args node))]
    (if (some nil? rs)
      nil
      (let [op (case (:prim node) :+ r-add :- r-sub :* r-mul)
            [acc oks] (reduce (fn [[acc oks] r]
                                (let [x (op acc r)]
                                  [(clamp x) (conj oks (fits? x full))]))
                              [(first rs) []] (rest rs))]
        (fact! (:id node) {:no-ov oks})
        acc))))

(defn- one-step [node env f]
  (when-let [r (rng (first (:args node)) env)]
    (let [x (f r)]
      (fact! (:id node) {:no-ov [(fits? x full)]})
      (clamp x))))

(defn- divide [node env]
  (let [[a b] (map #(rng % env) (:args node))]
    (when (and a b)
      (let [am (max (-' (a 0)) (a 1))]
        ;; rem and mod only over a divisor proven positive (range-rem.smt2,
        ;; range-mod.smt2); |quot| <= |a| (range-quot.smt2)
        (case (:prim node)
          :quot (clamp [(-' am) am])
          :rem (if (pos? (b 0)) [(if (neg? (a 0)) (- 1 (b 1)) 0) (dec (b 1))] full)
          :mod (if (pos? (b 0)) [0 (dec (b 1))] full))))))

(defn- prim-rng [node env]
  (if (and (not= :long (:type node)) (#{:+ :- :* :neg :inc :dec} (:prim node)))
    ;; double arithmetic: no interval, but its operands still carry facts
    (do (run! #(rng % env) (:args node)) nil)
    (long-prim-rng node env)))

(defn- long-prim-rng [node env]
  (case (:prim node)
    (:+ :- :*) (fold-arith node env)
    :neg (one-step node env (fn [[a b]] [(-' b) (-' a)]))
    :inc (one-step node env (fn [r] (r-add r [1 1])))
    :dec (one-step node env (fn [r] (r-sub r [1 1])))
    (:quot :rem :mod) (do (run! #(rng % env) (:args node)) (divide node env))
    :to-long (let [r (rng (first (:args node)) env)]
               (if (= :long (:type (first (:args node)))) r full))
    (do (run! #(rng % env) (:args node)) nil)))

;; --- comparisons narrow their locals ------------------------------------------

(defn- simple-rng
  "The interval of a local, a constant or an alength, without recording facts."
  [env node]
  (case (:op node)
    :const (when (= :long (:type node)) [(:val node) (:val node)])
    :local (when (= :long (:type node)) (get-in env [:r (:binding node)] full))
    :alength (alen env (:arr node))
    nil))

(defn- len-of
  "The array local whose length node is: (alength a), or a local bound to it."
  [env node]
  (case (:op node)
    :alength (local-id (:arr node))
    :local (get-in env [:len-of (:binding node)])
    nil))

(defn- relate
  "Narrow env by x < y (strict) or x <= y."
  [env x y strict]
  (let [rx (simple-rng env x) ry (simple-rng env y) k (if strict 1 0)
        env (if (and rx ry (local-id x))
              (assoc-in env [:r (local-id x)] [(rx 0) (min (rx 1) (- (ry 1) k))])
              env)
        env (if (and rx ry (local-id y))
              (assoc-in env [:r (local-id y)] [(max (ry 0) (+ (rx 0) k)) (ry 1)])
              env)]
    (if-let [a (and strict (local-id x) (len-of env y))]
      (update-in env [:lt (local-id x)] (fnil conj #{}) a)
      env)))

(defn- narrow
  "env as it stands where test answered truth."
  [env test truth]
  (case (:op test)
    :prim
    (let [[x y :as args] (:args test)
          longs? (and (= 2 (count args)) (every? #(= :long (:type %)) args))]
      (case (:prim test)
        :not (narrow env x (not truth))
        (:< :<= :> :>=)
        (if-not longs?
          env
          (let [[x y strict] (case (:prim test) :< [x y true] :<= [x y false]
                                   :> [y x true] :>= [y x false])]
            ;; not (x < y) is y <= x; not (x <= y) is y < x
            (if truth (relate env x y strict) (relate env y x (not strict)))))
        env))
    ;; (and a b) is (if a b false); (or a b) is (if a true b)
    :if
    (let [{:keys [test then else]} test
          const? (fn [n v] (and (= :const (:op n)) (= v (:val n))))]
      (cond
        (and truth (const? else false)) (narrow (narrow env test true) then true)
        (and (not truth) (const? then true)) (narrow (narrow env test false) else false)
        :else env))
    env))

;; --- arrays -------------------------------------------------------------------

(defn- new-len [r]
  (cond (nil? r) int-len
        (fits? r [imin imax]) [(max 0 (r 0)) (max 0 (r 1))]
        :else int-len))

(defn- value-alen
  "The length interval of an array value bound to a name."
  [env node]
  (case (:op node)
    :new-array (new-len (simple-rng env (:len node)))
    :aclone (alen env (:arr node))
    :local (alen env node)
    int-len))

(defn- index-facts!
  "Facts for aget/aset: the index fits an int, and the access is in bounds,
  statically or under the guard on an array parameter's length."
  [node env ri]
  (let [arr (local-id (:arr node))
        idx (local-id (:idx node))
        nonneg (and ri (>= (ri 0) 0))
        static (and nonneg
                    (or (and arr idx (contains? (get-in env [:lt idx]) arr))
                        (< (ri 1) ((alen env (:arr node)) 0))))
        guarded (and nonneg (not static) arr (contains? *params* arr) (< (ri 1) imax))]
    (when guarded (swap! *needs* update arr (fnil max -1) (ri 1)))
    (fact! (:id node) {:idx-int (boolean (and ri (fits? ri [imin imax])))
                       :in-bounds (boolean (or static guarded))})))

(defn- elem-rng [node]
  (when (= :long (:type node))
    (if (= [:ref :long] (:type (:arr node))) fixnum full)))

;; --- the walk -----------------------------------------------------------------

(defn- bind
  "env with a let or loop binding of id to init, whose interval is r."
  [env id init r]
  (let [env (-> env
                (update :r assoc id r)
                (update :lt dissoc id)
                (update :len-of dissoc id)
                (update :alen dissoc id))
        env (if-let [a (len-of env init)] (assoc-in env [:len-of id] a) env)]
    (if (ast/array-type? (:type init))
      (assoc-in env [:alen id] (value-alen env init))
      env)))

(defn- let-rng [node env]
  (let [env (reduce (fn [env {:keys [id init]}] (bind env id init (rng init env)))
                    env (:bindings node))]
    (rng (:body node) env)))

(defn- widen [round cur new]
  (if (or (< round 3) (nil? cur) (nil? new))
    new
    [(if (< (new 0) (cur 0)) lmin (new 0)) (if (> (new 1) (cur 1)) lmax (new 1))]))

(defn- loop-rng
  "Loop variables start at their inits and take every recur's values: iterate
  the body until their intervals stop changing. The round that changes nothing
  ran with the final intervals, so the facts it recorded are the ones kept.
  A loop variable stays \"the length of a\" only while every recur passes the
  length of that same array a, and a is not itself a loop variable that recur
  can rebind."
  [node env]
  (let [bs (:bindings node)
        ids (set (map :id bs))
        long? (fn [b] (= :long (:type (:init b))))
        [env inits] (reduce (fn [[env rs] {:keys [id init] :as b}]
                              (let [r (when (long? b) (rng init env))]
                                [(bind env id init r) (conj rs r)]))
                            [env []] bs)
        lens0 (mapv (fn [b] (let [a (get-in env [:len-of (:id b)])]
                              (when-not (contains? ids a) a)))
                    bs)
        lid (:loop-id node)]
    (loop [round 0 cur inits lens lens0]
      (swap! *recurs* assoc lid [])
      (let [env' (reduce (fn [e [b r a]]
                           (-> e (assoc-in [:r (:id b)] r) (update :alen dissoc (:id b))
                               (update :len-of #(if a (assoc % (:id b) a) (dissoc % (:id b))))))
                         env (map vector bs cur lens))
            out (rng (:body node) env')
            seen (get @*recurs* lid)
            new (vec (map-indexed
                      (fn [j b]
                        (when (long? b)
                          (widen round (cur j)
                                 (reduce join (inits j) (map #(nth (:r %) j) seen)))))
                      bs))
            lens' (vec (map-indexed
                        (fn [j a] (when (and a (every? #(= a (nth (:len %) j)) seen)) a))
                        lens))]
        (if (and (= new cur) (= lens' lens)) out (recur (inc round) new lens'))))))

(defn rng
  "The interval of a long-valued node in env (nil otherwise), recording facts."
  [node env]
  (case (:op node)
    :const (simple-rng env node)
    :local (simple-rng env node)
    :let (let-rng node env)
    :do (do (run! #(rng % env) (:statements node)) (rng (:ret node) env))
    :if (let [{:keys [test then else]} node
              _ (rng test env)
              rt (rng then (narrow env test true))
              re (rng else (narrow env test false))]
          (when (= :long (:type node))
            (join (when-not (= :never (:type then)) rt) (when-not (= :never (:type else)) re))))
    :loop (loop-rng node env)
    :recur (do (swap! *recurs* update (:loop-id node) conj
                      {:r (mapv #(rng % env) (:args node))
                       :len (mapv #(len-of env %) (:args node))})
               nil)
    :prim (prim-rng node env)
    :math (do (run! #(rng % env) (:args node)) nil)
    :extern (do (run! #(rng % env) (:args node)) (when (= :long (:type node)) full))
    :alength (do (rng (:arr node) env) (alen env (:arr node)))
    :aget (do (rng (:arr node) env)
              (index-facts! node env (rng (:idx node) env))
              (elem-rng node))
    :aset (let [_ (rng (:arr node) env)
                ri (rng (:idx node) env)
                rv (rng (:val node) env)]
            (index-facts! node env ri)
            (when (= :long (:type node)) rv))
    :call (do (run! #(rng % env) (:args node)) (when (= :long (:type node)) full))
    :new-array (do (rng (:len node) env) nil)
    :aclone (do (rng (:arr node) env) nil)
    nil))

(defn analyze-fn
  "Facts for one function (the region's entry or a helper instance) with these
  parameters: {:facts {node-id fact} :needs {param-id max-index}
  :arrays [param-id]}, needs being the indexes the guard must cover."
  [params body]
  (let [arrays (mapv :id (filter #(ast/array-type? (:type %)) params))]
    (binding [*facts* (atom {}) *recurs* (atom {}) *needs* (atom {})
              *params* (set arrays)]
      (rng body {:r {} :lt {} :len-of {} :alen {}})
      {:facts @*facts* :needs @*needs* :arrays arrays})))

(defn analyze-region
  "Facts for a checked region: {:entry analysis, iid analysis ...}."
  [region]
  (into {:entry (analyze-fn (:params region) (:body region))}
        (mapcat (fn [inst]
                  ;; a platform body (defnative's on) is its own function in C
                  (cons [(:iid inst) (analyze-fn (:params inst) (:body inst))]
                        (map-indexed (fn [i v] [[(:iid inst) i] (analyze-fn (:params inst) (:body v))])
                                     (:variants inst)))))
        (:instances region)))
