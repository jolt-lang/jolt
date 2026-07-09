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

(ns jolt.infix.grammar
  (:refer-clojure :exclude [boolean])
  (:require
   [jolt.parser.monad :as m]
   [jolt.parser.position :refer [strip-location]]
   [jolt.parser.basic :refer :all]
   [jolt.parser.combinators :refer :all]))

; expression ::= term { addop term }.
; term ::= factor { mulop factor }.
; factor ::= base { expop base }
; base ::= "(" expression ")" | boolean | number | var | function | ternary.
; addop ::= "+" | "-".
; mulop ::= "*" | "/".
; expop ::= "**"
;
; function ::= envref expression | envref "(" <empty> | expression { "," expression } ")".
; ternary ::= expression "?" expression ":" expression.
; envref ::= letter | "_" { letter | digit | "_"  | "." }.
; var ::= envref.
; boolean :: = "true" | "false"
; number ::= integer | decimal | rational | binary | hex
; binary :: = [ "-" ] "0b" { "0" | "1" }.
; hex :: = [ "-" ] "0x" | "#" { "0" | ... | "9" | "A" | ... | "F" | "a" | ... | "f" }.
; integer :: = [ "-" ] digits.
; decimal :: = [ "-" ] digits "." digits.
; rational :: = integer "/" digits.
; letter ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z".
; digit ::= "0" | "1" | ... | "8" | "9".
; digits ::= digit { digit }.

(def digit (from-re #"[0-9]"))

; TODO: allow unicode/utf8 characters
(def letter (from-re #"[a-zA-Z]"))

(def alpha-num (any-of letter digit (match "_") (match ".")))

(def digits
  (m/do*
   (text <- (plus digit))
   (m/return (strip-location text))))

(def envref
  (m/do*
   (fst <- (any-of letter (match "_") (match ".")))
   (rst <- (token (many alpha-num)))
   (m/return (let [kw (keyword (strip-location (cons fst rst)))]
               (fn [env]
                 (if (contains? env kw)
                   (get env kw)
                   (throw (IllegalStateException.
                           (str (name kw) " is not bound in environment")))))))))

(def var envref)

(def binary
  (m/do*
   (sign <- (optional (match "-")))
   (string "0b")
   (value <- (m/do*
              (text <- (token (plus (from-re #"[01]"))))
              (m/return (strip-location text))))
   (m/return (constantly (Long/parseLong (str (strip-location sign) value) 2)))))

(def hex
  (m/do*
   (sign <- (optional (match "-")))
   (any-of
    (match "#")
    (string "0x"))
   (value <- (m/do*
              (text <- (token (plus (from-re #"[0-9A-Fa-f]"))))
              (m/return (strip-location text))))
   (m/return (constantly (Long/parseLong (str (strip-location sign) value) 16)))))

(def integer
  (m/do*
   (sign <- (optional (match "-")))
   (value <- (token digits))
   (m/return (constantly (Long/parseLong (str (strip-location sign) value))))))

(def rational
  (m/do*
   (dividend <- integer)
   (match "/")
   (divisor <- (token digits))
   (m/return (constantly (/ (dividend) (Long/parseLong divisor))))))

(def decimal
  (m/do*
   (sign <- (optional (match "-")))
   (i <- digits)
   (p <- (match "."))
   (d <- (token digits))
   (m/return (constantly (Double/parseDouble (str (strip-location sign) i "." d))))))

(def number
  (any-of integer decimal rational binary hex))

(def boolean
  (m/do*
   (value <- (any-of (string "true") (string "false")))
   (m/return (constantly (Boolean/parseBoolean (str (strip-location value)))))))

(defn list-of [parser]
  (optional (separated-by (token parser) (symb ","))))

(declare expression)

(def function
  (or-else
   (m/do*
    (f <- envref)
    (plus (match " "))
    (expr <- expression)
    (m/return (fn [env]
                ((f env) (expr env)))))
   (m/do*
    (f <- envref)
    (symb "(")
    (args <- (list-of expression))
    (symb ")")
    (m/return (fn [env]
                (apply
                 (f env)
                 (map #(% env) args)))))))

(def ternary-op
  (m/do*
   (symb "(")
   (condition <- expression)
   (symb ")")
   (symb "?")
   (yes-exp <- expression)
   (symb ":")
   (no-exp <- expression)
   (m/return (fn [env]
               (if (condition env) (yes-exp env) (no-exp env))))))

(def base
  (any-of
   (m/do*
    (symb "(")
    (e <- expression)
    (symb ")")
    (m/return e))
   boolean
   number
   var
   function
   ternary-op))

(defn binary-op [& ops]
  (m/do*
   (op <- (reduce or-else (map string ops)))
   (m/return
    (let [kw (keyword (str (strip-location op)))]
      (fn [env]
        (if (contains? env kw)
          (get env kw)
          (throw (IllegalStateException.
                  (str (name kw) " is not bound in environment")))))))))

(def expop (binary-op "**"))
(def mulop (binary-op "*" "/" "÷" "%" ">>" ">>>" "<<" "=" "==" "!=" ">" "<" ">=" "<="))
(def addop (binary-op "+" "-" "|" "&" "||" "&&"))

(defn- resolve-var [arg env]
  (let [v (arg env)]
    (if (var? v)
      (var-get v)
      v)))

(defn- left-associative-reducer [op-parser arg-parser]
  (m/do*
   (a1 <- arg-parser)
   (rst <- (many
            (m/do*
             spaces
             (op <- (token op-parser))
             (a2 <- arg-parser)
             (m/return [op a2]))))
   (m/return
    (fn [env]
      (reduce
       (fn [acc [op a2]] ((op env) acc (resolve-var a2 env)))
       (resolve-var a1 env)
       rst)))))

(defn- right-associative-reducer [op-parser arg-parser]
  (m/do*
   (rst <- (many
            (m/do*
             spaces
             (a <- arg-parser)
             (op <- (token op-parser))
             (m/return [a op]))))
   (an <- arg-parser)
   (m/return
    (fn [env]
      (reduce
       (fn [acc [a op]] ((op env) (resolve-var a env) acc))
       (resolve-var an env)
       (reverse rst))))))

(def factor (right-associative-reducer expop base))

(def term (left-associative-reducer mulop factor))

(def expression (left-associative-reducer addop term))
