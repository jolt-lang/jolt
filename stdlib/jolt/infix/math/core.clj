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

(ns jolt.infix.math.core
  (:refer-clojure :exclude [rand])
  (:require
   [jolt.infix.math :refer [defunary defbinary]]))

(defunary abs)
(defunary signum)
(defunary sqrt)
(defunary sqrt √)
(defunary exp)
(defunary log)

(defbinary pow)
(defbinary pow **)

(def product *)
(def sum +)
(def rand clojure.core/rand)
(def randInt clojure.core/rand-int)

(defn divide [a b]
  (if (zero? b)
    (if (neg? a)
      Double/NEGATIVE_INFINITY
      Double/POSITIVE_INFINITY)
    (/ a b)))

(def ÷ divide)

(defn root [a b]
  (pow b (/ 1 a)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn fact [n]
  (if (zero? n)
    1
    (apply * (range 1 (inc n)))))
