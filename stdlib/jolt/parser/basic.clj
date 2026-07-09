;; Primitive character parsers, adapted from rm-hull/jasentaa (MIT).

(ns jolt.parser.basic
  (:require
   [jolt.parser.monad :as m :refer [>>=]]))

(defn any [input]
  (if (empty? input)
    (m/failure input)
    (list [(first input) (rest input)])))

(defn eof
  "Succeeds only at the end of input, consuming nothing (dual of `any`)."
  [input]
  (if (empty? input)
    (list [nil input])
    (m/failure input)))

(defn sat
  "Satisfies a given predicate"
  [pred]
  (>>= any (fn [v]
             (if (pred (:char v))
               (m/return v)
               m/failure))))

(defn char-cmp
  "Does a character comparison using a specific function"
  [f]
  (fn [c] (sat (partial f (first c)))))

(def match
  "Recognises a given char"
  (char-cmp =))

(def none-of
  "Rejects a given char"
  (char-cmp not=))

(defn from-re [re]
  (sat (fn [v]
         (not (nil? (re-find re (str v)))))))

(defmacro fwd
  "Delays the evaluation of a parser that was forward (declare)d and
   it has not been defined yet. For use in (def)s of no-arg parsers,
   since the parser expression evaluates immediately."
  [p]
  (let [x (gensym)]
    `(fn [~x] (~p ~x))))
