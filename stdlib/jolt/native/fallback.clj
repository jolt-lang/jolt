(ns jolt.native.fallback
  "The Jolt half of a native region: Clojure generated from the same typed IR
  the C comes from, so the REPL and a built binary agree. It differs from the
  body as written only where a region's rules differ from Jolt's: long
  arithmetic overflows instead of promoting, an index passes through
  RT.intCast, and a variadic op folds pair by pair exactly as the C does."
  (:require [clojure.string :as str]))

;; the prefix of the fallback's own names (locals, helper fns): one no region
;; parameter starts with, since parameters keep their own names
(def ^:private ^:dynamic *prefix* "nr__")
(defn- lname [id] (symbol (str *prefix* "l" id)))
(defn- hname [iid] (symbol (str *prefix* "h" iid)))

(defn- fresh-prefix [params]
  (let [names (map (comp str :name) params)]
    (first (remove (fn [pre] (some #(str/starts-with? % pre) names))
                   (cons "nr__" (map #(str "nr" % "__") (range)))))))

(defn- realize
  "A generated form with every lazy part realized, while *prefix* is bound."
  [form]
  (cond (seq? form) (doall (map realize form))
        (vector? form) (mapv realize form)
        :else form))

(declare clj)

(defn- value-of
  "A function body as its result: a nil-typed (:unit) body answers nil, as the
  C does, even where a branch beside the nil computed a value: (when c x)."
  [node]
  (if (= :unit (:type node)) (list 'do (clj node) nil) (clj node)))

(defn- dbl [form t] (if (= t :long) (list 'clojure.core/double form) form))

(defn- fold [node op-long op-double]
  (let [args (:args node)]
    (first
     (reduce (fn [[acc at] a]
               (let [bt (:type a) b (clj a)]
                 (if (and (= at :long) (= bt :long))
                   [(list op-long acc b) :long]
                   [(list op-double (dbl acc at) (dbl b bt)) :double])))
             [(clj (first args)) (:type (first args))]
             (rest args)))))

(defn- prim [node]
  (let [args (:args node) t (:type node) a0 (first args)]
    (case (:prim node)
      :+ (fold node 'jolt.native.rt/add 'clojure.core/+)
      :- (fold node 'jolt.native.rt/sub 'clojure.core/-)
      :* (fold node 'jolt.native.rt/mul 'clojure.core/*)
      :div (if (= 1 (count args))
             (list 'clojure.core// 1.0 (clj a0))
             (fold node 'clojure.core// 'clojure.core//))
      :neg (if (= t :long) (list 'jolt.native.rt/neg (clj a0)) (list 'clojure.core/- (clj a0)))
      :inc (if (= t :long) (list 'jolt.native.rt/add (clj a0) 1) (list 'clojure.core/+ (clj a0) 1.0))
      :dec (if (= t :long) (list 'jolt.native.rt/sub (clj a0) 1) (list 'clojure.core/- (clj a0) 1.0))
      :quot (cons 'jolt.native.rt/quot* (map clj args))
      :rem (cons 'jolt.native.rt/rem* (map clj args))
      :mod (cons 'jolt.native.rt/mod* (map clj args))
      :< (cons 'clojure.core/< (map clj args))
      :<= (cons 'clojure.core/<= (map clj args))
      :> (cons 'clojure.core/> (map clj args))
      :>= (cons 'clojure.core/>= (map clj args))
      :num= (cons 'clojure.core/== (map clj args))
      :eq (cons 'clojure.core/= (map clj args))
      :ne (cons 'clojure.core/not= (map clj args))
      :zero? (list 'clojure.core/zero? (clj a0))
      :pos? (list 'clojure.core/pos? (clj a0))
      :neg? (list 'clojure.core/neg? (clj a0))
      :not (list 'clojure.core/not (clj a0))
      :to-double (list 'clojure.core/double (clj a0))
      :to-long (list 'clojure.core/long (clj a0)))))

(defn clj
  "Clojure for an analyzed node."
  [node]
  (case (:op node)
    :const (:val node)
    :local (if-let [p (get-in node [:param-name])] p (lname (:binding node)))
    :let (list 'clojure.core/let
               (vec (mapcat (fn [b] [(lname (:id b)) (clj (:init b))]) (:bindings node)))
               (clj (:body node)))
    :if (list 'if (clj (:test node)) (clj (:then node)) (clj (:else node)))
    :do (concat ['do] (map clj (:statements node)) [(clj (:ret node))])
    :loop (list 'clojure.core/loop
                (vec (mapcat (fn [b] [(lname (:id b)) (clj (:init b))]) (:bindings node)))
                (clj (:body node)))
    :recur (cons 'recur
                 (map (fn [a want] (if (and (= want :double) (= :long (:type a)))
                                     (list 'clojure.core/double (clj a)) (clj a)))
                      (:args node) (:want node)))
    :prim (prim node)
    :math (cons (symbol "Math" (:fn node)) (map #(dbl (clj %) (:type %)) (:args node)))
    ;; the same C function, through its jolt.ffi binding
    :extern (cons (:sym node) (map (fn [a want] (if (= want :double) (dbl (clj a) (:type a)) (clj a)))
                                   (:args node) (:params node)))
    :alength (list 'clojure.core/alength (clj (:arr node)))
    :aget (list 'clojure.core/aget (clj (:arr node)) (list 'jolt.native.rt/index (clj (:idx node))))
    :aset (let [v (if (and (= :double (:type node)) (= :long (:type (:val node))))
                    (list 'clojure.core/double (clj (:val node)))
                    (clj (:val node)))]
            ;; a borrowed long-array keeps fixnum storage, as the C half must;
            ;; the index is checked first there, so it is here too
            (list (if (= [:ref :long] (:type (:arr node))) 'jolt.native.rt/fixnum-aset 'clojure.core/aset)
                  (clj (:arr node)) (list 'jolt.native.rt/index (clj (:idx node))) v))
    :new-array (list (if (= :double (:elem node)) 'clojure.core/double-array 'clojure.core/long-array)
                     (clj (:len node)))
    :aclone (list 'clojure.core/aclone (clj (:arr node)))
    :call (cons (hname (:instance node)) (map clj (:args node)))))

(defn region-form
  "The fallback expression for an analyzed region, evaluated where its
  parameters are bound under their own names."
  [region]
  (binding [*prefix* (fresh-prefix (:params region))]
    (let [params (into {} (map (fn [p] [(:id p) (:name p)]) (:params region)))
          mark (fn mark [node]
                 (cond
                   (and (map? node) (= :local (:op node)) (params (:binding node)))
                   (assoc node :param-name (params (:binding node)))
                   (map? node) (into {} (map (fn [[k v]] [k (mark v)]) node))
                   (vector? node) (mapv mark node)
                   :else node))
          body (value-of (mark (:body region)))
          insts (:instances region)]
      (realize
       (if (seq insts)
         ;; each native helper instance as a local fn, from its own checked IR
         (list 'clojure.core/letfn
               (mapv (fn [i] (list (hname (:iid i)) (mapv (comp lname :id) (:params i)) (value-of (:body i))))
                     insts)
               body)
         body)))))
