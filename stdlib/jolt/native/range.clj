(ns jolt.native.range
  "Range facts for a checked region: which long arithmetic cannot overflow, which
  index already fits an int, and which array access is in bounds, so the C
  emitter can leave those checks out of a fast copy of the function.

  The facts hold under assumptions about the function's array parameters (each
  no longer than Integer/MAX_VALUE, longer than the largest index proven for
  it, and longer than each symbolic bound an index was proven under: a long
  parameter or another array parameter's length, plus a constant). The emitter
  turns those into one guard at the function's entry: when it holds, the fast
  copy runs; when not, the fully checked body runs, as before. So a fact never
  changes what a program does, only which checks run.

  Besides intervals, a long local carries symbolic upper bounds, [base k] for
  local <= base + k, base being [:len a] (array a's length) or [:p p] (a long
  parameter, which nothing rebinds). (< i (dec n)) with n bound to (alength
  xs) gives i the bound [[:len xs] -2], so (aget xs (inc i)) is in bounds;
  (< i m) gives [[:p m] -1], and the guard m < xs.len covers (aget xs i). The
  idea is scriptc's affine loop versions (bytes-loop-versions.ts), hoisted to
  the function's entry because a parameter cannot change.

  An accumulator that every recur advances by a bounded step, in a loop with a
  counter that every recur increments, stays within init + trips * step, where
  trips is what the counter's range allows; so a sum of bytes cannot overflow
  (scriptc's lanes.ts reasons the same way about exact partial sums).

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
;; long parameter ids: a symbolic bound may name one
(def ^:dynamic *scalars* nil)
;; #{[array-id base k]}: base + k must be below the array's length
(def ^:dynamic *rels* nil)
;; loop-id -> [binding-id]: the variables a recur rebinds
(def ^:dynamic *loopvars* nil)

;; a symbolic bound's constant stays small, so base + k never leaves the long
;; range in the guard's C
(def max-offset 1048576)

(defn- fact! [id f] (swap! *facts* assoc id f))

;; env: {:r {local-id interval} :ub {local-id #{[base k]}}
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
    ;; a mask with a known non-negative operand bounds the result by it, which
    ;; is what lets (aget xs (bit-and i 255)) be proven in bounds
    :bit-and (let [rs (mapv #(rng % env) (:args node))
                   nonneg (keep (fn [r] (when (and r (>= (r 0) 0)) (r 1))) rs)]
               (if (seq nonneg) [0 (apply min nonneg)] full))
    :unsigned-bit-shift-right
    (let [[_ rk] (mapv #(rng % env) (:args node))]
      (if (and rk (= (rk 0) (rk 1)) (< 0 (rk 0) 64))
        ;; 2^(64-k) - 1, without forming 2^63 (k = 1), which would overflow
        [0 (bit-shift-right Long/MAX_VALUE (dec (rk 0)))]
        full))
    (:bit-or :bit-xor :bit-not :bit-shift-left :bit-shift-right)
    (do (run! #(rng % env) (:args node)) full)
    (do (run! #(rng % env) (:args node)) nil)))

;; --- comparisons narrow their locals ------------------------------------------

(defn- simple-rng
  "The interval of a local, a constant or an alength, without recording facts."
  [env node]
  (case (:op node)
    :const (when (= :long (:type node)) [(:val node) (:val node)])
    :local (when (= :long (:type node)) (get-in env [:r (:binding node)] full))
    :alength (alen env (:arr node))
    :prim (when (= :long (:type node))
            (let [[x y] (:args node)
                  c (fn [n] (when (and (= :const (:op n)) (= :long (:type n))) (:val n)))]
              (case (:prim node)
                :inc (some-> (simple-rng env x) (r-add [1 1]) clamp)
                :dec (some-> (simple-rng env x) (r-sub [1 1]) clamp)
                :+ (when (= 2 (count (:args node)))
                     (cond (c y) (some-> (simple-rng env x) (r-add [(c y) (c y)]) clamp)
                           (c x) (some-> (simple-rng env y) (r-add [(c x) (c x)]) clamp)))
                :- (when (and (= 2 (count (:args node))) (c y))
                     (some-> (simple-rng env x) (r-sub [(c y) (c y)]) clamp))
                nil)))
    nil))

(defn- len-of
  "The array local whose length node is: (alength a), or a local bound to it."
  [env node]
  (case (:op node)
    :alength (local-id (:arr node))
    :local (get-in env [:len-of (:binding node)])
    nil))

(defn- shift [ubs d]
  (set (keep (fn [[b k]] (let [k (+ k d)] (when (<= (- max-offset) k max-offset) [b k]))) ubs)))

(defn- ubs
  "The symbolic upper bounds of a long expression: #{[base k]}, each meaning
  value <= base + k."
  [env node]
  (case (:op node)
    :alength (if-let [a (local-id (:arr node))] #{[[:len a] 0]} #{})
    :local (let [id (:binding node)]
             (cond-> (get-in env [:ub id] #{})
               (contains? *scalars* id) (conj [[:p id] 0])
               (get-in env [:len-of id]) (conj [[:len (get-in env [:len-of id])] 0])))
    :prim (if-not (= :long (:type node))
            #{}
            (let [[x y] (:args node)
                  c (fn [n] (when (and (= :const (:op n)) (= :long (:type n))) (:val n)))
                  two (= 2 (count (:args node)))]
              (case (:prim node)
                :inc (shift (ubs env x) 1)
                :dec (shift (ubs env x) -1)
                :+ (cond (and two (c y)) (shift (ubs env x) (c y))
                         (and two (c x)) (shift (ubs env y) (c x))
                         :else #{})
                :- (if (and two (c y)) (shift (ubs env x) (- (c y))) #{})
                #{})))
    #{}))

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
    (if-let [xid (local-id x)]
      (update-in env [:ub xid] (fnil into #{}) (shift (ubs env y) (- k)))
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

(defn- guardable?
  "Whether the guard at the function's entry can read base."
  [[kind id]]
  (case kind :p (contains? *scalars* id) :len (contains? *params* id) false))

(defn- index-facts!
  "Facts for aget/aset: the index fits an int, and the access is in bounds,
  statically, under the guard on an array parameter's length, or under the
  guard that a symbolic bound of the index is below that length."
  [node env ri]
  (let [arr (local-id (:arr node))
        bounds (sort (ubs env (:idx node)))
        nonneg (and ri (>= (ri 0) 0))
        static (and nonneg
                    (or (and arr (some (fn [[b k]] (and (= b [:len arr]) (neg? k))) bounds))
                        (< (ri 1) ((alen env (:arr node)) 0))))
        ;; a symbolic bound first: the guard it needs (m <= len, or one length
        ;; below another) holds for real inputs, where the index's interval
        ;; alone can ask for a length no array has (an i below a long m is
        ;; only known below 2^31 - 1)
        rel (when (and nonneg (not static) arr (contains? *params* arr))
              (first (filter (fn [[b _]] (and (guardable? b) (not= b [:len arr]))) bounds)))
        guarded (and nonneg (not static) (not rel) arr (contains? *params* arr) (< (ri 1) imax))
        in-bounds (boolean (or static guarded rel))]
    (when guarded (swap! *needs* update arr (fnil max -1) (ri 1)))
    (when rel (swap! *rels* conj (into [arr] rel)))
    ;; an index in bounds is in 0..len-1, and every length fits an int
    (fact! (:id node) {:idx-int (boolean (or in-bounds (and ri (fits? ri [imin imax]))))
                       :in-bounds in-bounds})))

(defn- elem-rng [node]
  (when (= :long (:type node))
    (case (:type (:arr node)) [:ref :long] fixnum [:ref :byte] [-128 127] full)))

;; --- the walk -----------------------------------------------------------------

(defn- bind
  "env with a let or loop binding of id to init, whose interval is r."
  [env id init r]
  (let [env (-> env
                (update :r assoc id r)
                (assoc-in [:ub id] (if (= :long (:type init)) (ubs env init) #{}))
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

(defn- step
  "What a recur's argument adds to loop variable id: an interval, when the
  argument is id itself, (inc id), (dec id), (+ id e), (+ e id) or (- id e)
  for a long e; nil otherwise."
  [env arg id]
  (let [self? (fn [n] (= id (local-id n)))
        [x y :as args] (:args arg)]
    (cond
      (self? arg) [0 0]
      (not (and (= :prim (:op arg)) (= :long (:type arg)))) nil
      :else (case (:prim arg)
              :inc (when (self? x) [1 1])
              :dec (when (self? x) [-1 -1])
              :+ (when (= 2 (count args))
                   (cond (self? x) (rng y env) (self? y) (rng x env)))
              :- (when (and (= 2 (count args)) (self? x))
                   (when-let [[a b] (rng y env)] [(-' b) (-' a)]))
              nil))))

(defn- trip-bounds
  "Intervals for the loop variables an iteration advances by a bounded step,
  from how many iterations the loop can run: a counter that every recur
  increments, starting at or above lo and at most hi where it recurs, recurs at
  most hi - lo + 1 times, and each recur adds at most one step to the others.
  {j interval}, for the variables whose interval that bounds."
  [inits seen]
  (when (seq seen)
    (let [n (count inits)
          counter? (fn [j] (and (inits j) (every? #(= [1 1] (nth (:step %) j)) seen)))
          trips (->> (range n)
                     (keep (fn [j]
                             (let [at (map #(nth (:at %) j) seen)]
                               (when (and (counter? j) (every? some? at))
                                 (max 0 (inc (-' (apply max (map second at)) ((inits j) 0))))))))
                     seq)]
      (when trips
        (let [t (apply min trips)]
          (into {}
                (keep (fn [j]
                        (let [ss (map #(nth (:step %) j) seen)]
                          ;; not a counter: its trips come from its own range,
                          ;; which must widen first
                          (when (and (inits j) (not (counter? j)) (every? some? ss))
                            (let [[a b] (reduce join ss)
                                  r [(+' ((inits j) 0) (*' t (min a 0))) (+' ((inits j) 1) (*' t (max b 0)))]]
                              (when (fits? r full) [j r]))))))
                (range n)))))))

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
        lid (:loop-id node)
        needs0 @*needs*
        rels0 @*rels*]
    (swap! *loopvars* assoc lid (mapv :id bs))
    (loop [round 0 cur inits lens lens0]
      (swap! *recurs* assoc lid [])
      ;; the guard's needs and relations come from the final round only, as
      ;; the facts do: an early round's narrower intervals ask for less of
      ;; one check and more of another (x.len > 3 from i in 0..3)
      (reset! *needs* needs0)
      (reset! *rels* rels0)
      (let [env' (reduce (fn [e [b r a]]
                           (-> e (assoc-in [:r (:id b)] r) (assoc-in [:ub (:id b)] #{})
                               (update :alen dissoc (:id b))
                               (update :len-of #(if a (assoc % (:id b) a) (dissoc % (:id b))))))
                         env (map vector bs cur lens))
            out (rng (:body node) env')
            seen (get @*recurs* lid)
            ;; a trip bound can depend on a step that depends on the bound;
            ;; after a few rounds the plain widening decides, which ends
            tb (when (< round 8) (trip-bounds inits seen))
            new (vec (map-indexed
                      (fn [j b]
                        (when (long? b)
                          (or (get tb j)
                              (widen round (cur j)
                                     (reduce join (inits j) (map #(nth (:r %) j) seen))))))
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
    :recur (let [ids (get @*loopvars* (:loop-id node))]
             (swap! *recurs* update (:loop-id node) conj
                    {:r (mapv #(rng % env) (:args node))
                     :len (mapv #(len-of env %) (:args node))
                     :step (mapv #(step env %1 %2) (:args node) ids)
                     :at (mapv #(get-in env [:r %]) ids)})
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
  :rels #{[array-id base k]} :arrays [param-id]}, needs being the indexes the
  guard must cover and rels the symbolic bounds base + k it must keep below an
  array's length."
  [params body]
  (let [arrays (mapv :id (filter #(ast/array-type? (:type %)) params))]
    (binding [*facts* (atom {}) *recurs* (atom {}) *needs* (atom {}) *rels* (atom #{})
              *loopvars* (atom {})
              *params* (set arrays)
              *scalars* (set (map :id (filter #(= :long (:type %)) params)))]
      (rng body {:r {} :ub {} :len-of {} :alen {}})
      {:facts @*facts* :needs @*needs* :rels @*rels* :arrays arrays})))

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
