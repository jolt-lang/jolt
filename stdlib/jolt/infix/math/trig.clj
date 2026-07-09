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

(ns jolt.infix.math.trig
  (:require
   [jolt.infix.math :refer [defunary defbinary]]
   [jolt.infix.math.core :refer [÷]]))

(defunary sin)
(defunary cos)
(defunary tan)

(defunary asin)
(defunary acos)
(defunary atan)
(defbinary atan2)

(defunary sinh)
(defunary cosh)
(defunary tanh)

;; Additional trig functions not found in JDK java.lang.Math
(defn sec
  "compute secant, given the angle in radians"
  [θ]
  (÷ 1 (Math/cos θ)))

(defn csc
  "compute cosecant, given the angle in radians"
  [θ]
  (÷ 1 (Math/sin θ)))

(defn cot
  "compute cotangent, given the angle in radians"
  [θ]
  (÷ 1 (Math/tan θ)))

(defn asec
  "compute arcsecant, given the number, returns the arcsecant in radians"
  [value]
  (Math/acos (÷ 1 value)))

(defn acsc
  "compute arccosecant, given the number, returns the arccosecant in radians"
  [value]
  (Math/asin (÷ 1 value)))

(defn acot
  "compute arccotangent, given the number, returns the arccotangent in radians"
  [value]
  (Math/atan (÷ 1 value)))

