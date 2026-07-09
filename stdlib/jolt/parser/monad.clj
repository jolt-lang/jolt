;; Minimal monadic-parser core, adapted from rm-hull/jasentaa
;; (MIT). A parser is a fn from input to a seq of [value remaining] results;
;; do* threads them together.

(ns jolt.parser.monad)

(defn failure [& args]
  '())

(defn bind [v f]
  (f v))

(defn return [v]
  (fn [input]
    (list [v input])))

(defn >>= [m f]
  (fn [input]
    (->>
     m
     (bind input)
     (mapcat (fn [[v tail]] (bind tail (f v)))))))

(defn- merge-bind [body bind]
  (if (and (not= clojure.lang.Symbol (type bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

(defmacro do* [& forms]
  (reduce merge-bind (last forms) (reverse (butlast forms))))
