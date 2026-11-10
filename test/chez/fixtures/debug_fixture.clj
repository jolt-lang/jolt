(ns debug-fixture (:refer-clojure :exclude [destructure]))

(defn inner [x]
  (let [y (* x 2)]
    (/ y 0)))

(defn outer [a]
  (let [b (+ a 1)]
    (inner b)))

(defn looper [n]
  (loop [i 0 acc []]
    (if (< i n)
      (recur (inc i) (conj acc (if (= i 3) (throw (ex-info "at three" {})) i)))
      acc)))

(defn catcher [s]
  (try
    (Long/parseLong s)
    (catch NumberFormatException e
      (let [msg (.getMessage e)]
        (throw (ex-info "bad number" {:msg msg} e))))))

(defn bottom-out [n]
  (if (zero? n)
    (throw (ex-info "bottom" {}))
    (bottom-out (dec n))))

(defn sum-to [n]
  (loop [i 0 s 0]
    (if (< i n) (recur (inc i) (+ s i)) s)))

(defn shadow [x]
  (let [x (inc x)
        x (* x 10)]
    (throw (ex-info "shadow" {}))))

(defn variadic [a & more]
  (throw (ex-info "variadic" {})))

(defn destructure [{:keys [k]} [p q]]
  (throw (ex-info "destructure" {})))

(defn with-fn [x]
  (let [f (fn [y] (/ y 0))]
    (f (+ x 1))))

(defn make-preconstructed []
  (ex-info "preconstructed" {}))

(defn throw-preconstructed [x]
  (let [e (make-preconstructed)
        y (* x 2)]
    (throw e)))

(defn throw-preconstructed-in-loop [n]
  (let [e (ex-info "preconstructed in loop" {})]
    (loop [i 0]
      (if (= i n)
        (throw e)
        (recur (inc i))))))

(defprotocol Boom (boom [this n]))
(defrecord Bomb [k]
  Boom
  (boom [this n] (let [m (+ k n)] (/ m 0))))

(defmulti area :kind)
(defmethod area :sq [{:keys [w]}]
  (let [a (* w w)]
    (/ a 0)))

(defprotocol Sized (size-of [this]))
(defrecord Box [w])
(extend-type Box
  Sized
  (size-of [this] (let [w (:w this) n (* w w)] (/ n 0))))
(extend-protocol Sized
  String
  (size-of [this] (let [n (count this)] (/ n 0))))
