;; The MIT License (MIT)
;;
;; Copyright (c) 2016 Richard Hull
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns jolt.infix.core
  (:require
   [jolt.infix.math.core]
   [jolt.infix.math.constants]
   [jolt.infix.math.bit-shuffling]
   [jolt.infix.math.trig]))

(def operator-alias
  (atom
   {'&&     'and
    '||     'or
    '!=     'not=
    '%      'mod
    '<<     'bit-shift-left
    '>>     'bit-shift-right
    '>>>    'unsigned-bit-shift-right
    '!      'not
    '&      'bit-and
    '|      'bit-or
    '.      '*
    'abs    'Math/abs
    'signum 'Math/signum
    '**     'Math/pow
    'sin    'Math/sin
    'cos    'Math/cos
    'tan    'Math/tan
    'asin   'Math/asin
    'acos   'Math/acos
    'atan   'Math/atan
    'sinh   'Math/sinh
    'cosh   'Math/cosh
    'tanh   'Math/tanh
    'sec    'jolt.infix.math.trig/sec
    'csc    'jolt.infix.math.trig/csc
    'cot    'jolt.infix.math.trig/cot
    'asec   'jolt.infix.math.trig/asec
    'acsc   'jolt.infix.math.trig/acsc
    'acot   'jolt.infix.math.trig/acot
    'exp    'Math/exp
    'log    'Math/log
    'e      'Math/E
    'π      'Math/PI
    'φ      'jolt.infix.math.constants/φ
    'sqrt   'Math/sqrt
    '√      'Math/sqrt
    '÷      'jolt.infix.math.core/divide
    'root   'jolt.infix.math.core/root
    'gcd    'jolt.infix.math.core/gcd
    'lcm    'jolt.infix.math.core/lcm
    'fact   'jolt.infix.math.core/fact
    'sum    'jolt.infix.math.core/sum
    '∑      'jolt.infix.math.core/sum
    'product 'jolt.infix.math.core/product
    '∏      'jolt.infix.math.core/product}))

(defn suppress! [sym]
  (swap! operator-alias dissoc sym))

(def operator-precedence
  ; From https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
  ; Lowest precedence first
  [;; binary operators
   'or 'and 'bit-or 'bit-xor 'bit-and 'not= '= '== '>= '> '<= '<
   'unsigned-bit-shift-right 'bit-shift-right 'bit-shift-left
   '+ '- '* '/ 'jolt.infix.math.core/divide 'Math/pow 'mod

   ;; unary operators
   'not
   'Math/sin  'Math/cos  'Math/tan
   'Math/asin 'Math/acos 'Math/atan
   'Math/sinh 'Math/cosh 'Math/tanh
   'Math/sqrt 'Math/exp  'Math/log
   'Math/abs  'Math/signum])

(def right-associative-operators
  #{'Math/pow})

(defn- bounded? [sym]
  (if-let [v (resolve sym)]
    (bound? v)
    false))

(defn resolve-alias
  "Attempt to resolve any aliases: if not found just return the original term"
  [term]
  (if (and (symbol? term) (bounded? term))
    term
    (get @operator-alias term term)))

(defn- empty-arglist? [xs]
  (let [elem (fnext xs)]
    (and (seq? elem) (empty? elem))))

(defn rewrite
  "Recursively rewrites the infix-expr as a prefix expression, according to
   the operator precedence rules"
  [infix-expr]
  (cond
    (not (seq? infix-expr))
    (resolve-alias infix-expr)

    (and (seq? (first infix-expr)) (= (count infix-expr) 1))
    (rewrite (first infix-expr))

    (empty? (rest infix-expr))
    (first infix-expr)

    :else
    (let [infix-expr (map resolve-alias infix-expr)]
      (loop [ops operator-precedence]
        (if-let [op (first ops)]
          (let [idx (if (right-associative-operators op)
                      (.indexOf ^java.util.List infix-expr op)
                      (.lastIndexOf ^java.util.List infix-expr op))]
            (if (pos? idx)
              (let [[expr1 [op & expr2]] (split-at idx infix-expr)]
                (list op (rewrite expr1) (rewrite expr2)))
              (recur (next ops))))

          (if (empty-arglist? infix-expr)
            (list (rewrite (first infix-expr)))
            (list (rewrite (first infix-expr)) (rewrite (next infix-expr)))))))))

(defn- logical-or [a b] (or a b))
(defn- logical-and [a b] (and a b))

(def base-env
  (merge
    ; wrapped java.lang.Math constants & functions
   (->>
    ['jolt.infix.math.core 'jolt.infix.math.constants
     'jolt.infix.math.trig 'jolt.infix.math.bit-shuffling]
    (mapcat ns-publics)
    (map (fn [[k v]] (vector (keyword k) v)))
    (into {}))

    ; Basic ops
   {:== ==
    := =
    :!= not=
    :+ +
    :- -
    :* *
    :/ /
    :% mod
    :&& logical-and
    :|| logical-or
    :> >
    :< <
    :>= >=
    :<= <=}))
