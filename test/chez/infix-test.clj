;; jolt.infix gate — exercises jolt's built-in infix notation (jolt.infix +
;; jolt.parser) with the upstream rm-hull/infix suite (macros/grammar/core
;; tests). Self-checks and prints INFIX OK when all pass (smoke.sh greps for it).
;; Parse failures surface as a jolt ex-info rather than a host exception class,
;; so the error-path checks look for ExceptionInfo.
;; Run: bin/joltc run test/chez/infix-test.clj
(ns infix-test
  (:require
   [jolt.infix :refer [infix $= from-string]]
   [jolt.infix.core :refer [suppress! base-env]]
   [jolt.infix.grammar :as g]
   [jolt.parser :refer [parse-all]]))

(def failures (atom []))
(defn fail! [msg] (swap! failures conj msg))
(defn chk [label ok] (when-not ok (fail! label)))
(defn chk= [label got want]
  (when-not (= got want)
    (fail! (str label ": want " (pr-str want) " got " (pr-str got)))))

(defn float=
  ([x y] (float= x y 0.00001))
  ([x y epsilon]
   (let [scale (if (or (zero? x) (zero? y)) 1 (Math/abs x))]
     (<= (Math/abs (- x y)) (* scale epsilon)))))
(defn chk-near [label got want]
  (when-not (float= got want)
    (fail! (str label ": ~want " want " got " got))))

(defn threw [thunk] (try (thunk) ::none (catch Throwable e e)))
(defn chk-throws [label pred thunk]
  (let [e (threw thunk)]
    (cond
      (= e ::none) (fail! (str label ": expected throw, got none"))
      (not (pred e)) (fail! (str label ": wrong throw " (pr-str (type e))
                                 " / " (pr-str (.getMessage e)))))))
(defn ex? [e] (instance? clojure.lang.ExceptionInfo e))
(defn ise? [e] (instance? IllegalStateException e))
(defn arity? [e] (instance? clojure.lang.ArityException e))
(defn parse-msg [re] (fn [e] (and (ex? e) (boolean (re-find re (str (.getMessage e)))))))

(def ε 0.0000001)

;; ---------------------------------------------------------------- macros tests

;; basic-arithmetic
(chk= "ba1" (infix 3 + 4) (+ 3 4))
(chk= "ba2" (infix 3 + 5 * 8) 43)
(chk= "ba3" (infix (3 + 5) * 8) 64)
(chk= "ba4" (infix (3 - 2) - 1) 0)
(chk= "ba5" (infix 3 - 2 - 1) 0)
(chk= "ba6" (infix 3 + 2 % 3) 5)
(chk= "ba7" (infix (3 + 2) % 3) 2)
(chk= "ba8" (infix 1 - 1 + 1) 1)
(chk= "ba9" (infix 1 - 2 + 3) 2)

;; basic-arithmetic-$=
(chk= "$=1" ($= 3 + 4) (+ 3 4))
(chk= "$=2" ($= 3 + 5 * 8) 43)
(chk= "$=3" ($= (3 + 5) * 8) 64)
(chk= "$=4" ($= (3 - 2) - 1) 0)
(chk= "$=5" ($= 3 - 2 - 1) 0)
(chk= "$=6" ($= 3 + 2 % 3) 5)
(chk= "$=7" ($= (3 + 2) % 3) 2)
(chk= "$=8" ($= 1 - 1 + 1) 1)
(chk= "$=9" ($= 1 - 2 + 3) 2)

;; check-aliasing
(chk= "al1" (infix √ (5 * 5)) 5.0)
(chk= "al2" (infix 5 % 3) 2)
(let [t 0.324]
  (chk "al3" (> ε (Math/abs (- (infix sin (2 * t) + 3 * cos (4 * t)) 1.4176457261295824)))))

;; check-nested-aliasing
(chk= "nest" (infix abs (3 ** 6)) 729.0)

;; check-nullary-operators
(let [f (fn [] (infix 3 + 7))
      g #(infix 7 + 21)]
  (chk= "null1" (infix f () + 4) 14)
  (chk= "null2" (infix g () / 4) 7)
  (chk "null3" (<= 0 (infix rand () * 3) 3)))

;; check-unary-precedence
(let [x 4 y 3]
  (chk= "un1" (infix √ x) 2.0)
  (chk= "un2" (infix √ x + y) 5.0)
  (chk= "un3" (infix cos π) -1.0))

;; check-binary-precedence
(let [x 4 y 3]
  (chk= "bin1" (infix x . y) 12)
  (chk= "bin2" (infix x ** y) 64.0)
  (chk= "bin3" (infix 2 ** 2 ** 2 ** 2) 65536.0))

;; check-from-string
(chk= "fs1" ((from-string "5")) 5)
(chk= "fs2" ((from-string "5 * 2")) 10)
(chk= "fs3" ((from-string "5 + 2")) 7)
(chk= "fs4" ((from-string "1 - 1 + 1")) 1)
(chk= "fs5" ((from-string "1 - 2 + 3")) 2)
(chk= "fs6" ((from-string [x] "x + 3") 4) 7)
(chk= "fs7" ((from-string [_x] "_x + 3") 4) 7)
(chk= "fs8" ((from-string [x] {:+ -} "x + 3") 4) 1)
(chk= "fs9" ((from-string [] {:x 6 :+ +} "x + 1")) 7)
(chk= "fs10" ((from-string [] "3 + 5**2")) 28.0)
(chk= "fs11" ((from-string [] "3 * 5**2")) 75.0)
(chk= "fs12" ((from-string [t] "(t*(t>>5|t>>8))>>(t>>16)") 3425) 380175)
(chk= "fs13" ((from-string [t] "( t * (  t  >> 5 | t >>  8 ) ) >> ( t >> 16  )") 3425) 380175)
(chk= "fs14" ((from-string "(3-2)-1")) 0)
(chk= "fs15" ((from-string "3 - 2 - 1")) 0)
(chk= "fs16" ((from-string "2 ** 2 ** 2 ** 2")) 65536.0)
(chk= "fs17" ((from-string "divide(3, 0)")) Double/POSITIVE_INFINITY)
(chk= "fs18" ((from-string "-3 ÷ 0")) Double/NEGATIVE_INFINITY)
(chk= "fs19" ((from-string [t] "t - 2") 7) 5)
(chk= "fs20" ((from-string [t] "t-2") 7) 5)
(chk= "fs21" ((from-string [t] "t- 2") 7) 5)
(chk= "fs22" ((from-string [t] "t+2") 3) 5)
(chk= "fs23" ((from-string [f] "f() + 2") (fn [] 3)) 5)
(chk "fs24" (<= 0 ((from-string "rand() * 10")) 10))
(chk "fs25" (<= 0 ((from-string "randInt(10)")) 10))
(chk "fs26" (<= 0 ((from-string [n] "randInt(n)") 5) 5))
(chk= "fs27" ((from-string "pow(5,2)")) 25.0)
(chk= "fs28" ((from-string "sin(3)")) 0.1411200080598672)
(chk= "fs29" ((from-string "sin 3")) 0.1411200080598672)
(chk= "fs30" ((from-string "pi")) 3.1415926535897932)
(chk= "fs31" ((from-string "e")) 2.718281828459045)
(chk= "fs32" ((from-string [e] "e * 3") 5) 15)
(chk= "fs33" ((from-string [e] "e ** (3 + pi)") 5) 19624.068163608234)
(chk= "fs34" ((from-string [x] "product(e, pi, 3 * 3, x)") 5) 384.2880400203104)
(chk= "fs35" ((from-string "16 = 16")) true)
(chk= "fs36" ((from-string "16 == 16")) true)
(chk= "fs37" ((from-string "16 != 17")) true)
(chk= "fs38" ((from-string "16 | 32")) 48)
(chk= "fs39" ((from-string "false || true")) true)
(chk= "fs40" ((from-string "false || false")) false)
(chk= "fs41" ((from-string "16 & 48")) 16)
(chk= "fs42" ((from-string "true && false")) false)
(chk= "fs43" ((from-string "true && true")) true)
(chk-throws "fs44" (parse-msg #"Failed to parse text at line: 1, col: 3\nx \+ \n  \^")
            #((from-string [x] "x + ") 3))
(chk-throws "fs45" (parse-msg #"Failed to parse text at line: 1, col: 8\nx\+\(y\*7\)\)\n       \^")
            #((from-string [x y] "x+(y*7))") 3 2))
(chk-throws "fs46" arity? #((from-string [x] "x + 3") 2 3))
(chk-throws "fs47" (fn [e] (and (ise? e) (re-find #"x is not bound in environment" (str (.getMessage e)))))
            #((from-string "x + 3")))

;; check-math-namespace-aliases
(chk= "csc" (infix csc (32)) 1.813477718829676)
(chk= "sec" (infix sec (4)) -1.5298856564663974)

;; check-alias-expansion
(let [x 4 y 3]
  (chk-near "alx" (infix exp (sin x + cos y) - sin (exp (x + y))) 0.389947492069644965))

;; check-equivalence
(let [f (from-string [t] "t>>5 | t>>8")]
  (doseq [t (repeatedly 50 #(rand-int 1000000))]
    (chk (str "equiv t=" t) (= (f t) (infix (t >> 5 | t >> 8))))))

;; check-division-precedence
(let [x 0 y 1]
  (chk= "dp1" (infix sin (x ÷ y ** 2)) 0.0)
  (chk= "dp2" (infix 4 / 4 * 2) 2)
  (chk= "dp3" (infix (4 / 4) * 2) 2)
  (chk= "dp4" (infix 4 / (4 * 2)) 1/2)
  (chk= "dp5" (infix 4 / (4 * 2.0)) 0.5)
  (chk= "dp6" (infix 4 * 4 / 2) 8)
  (chk= "dp7" (infix (4 * 4) / 2) 8)
  (chk= "dp8" (infix 4 * (4 / 2)) 8))

;; check-equality
(chk "eq1" (true? (infix 5 = 5)))
(chk "eq2" (false? (infix 5 = 5.0)))
(chk "eq3" (true? (infix 5 == 5)))
(chk "eq4" (true? (infix 5 == 5.0)))
(chk "eq5" (true? (infix 5 != 3)))
(chk "eq6" (true? (infix 5 not= 4)))

;; check-comparison
(chk "cmp1" (true? (infix 3 < 5)))
(chk "cmp2" (false? (infix 3 > 5)))
(chk "cmp3" (true? (infix 3 <= 5)))
(chk "cmp4" (false? (infix 3 >= 5)))
(chk "cmp5" (true? (infix 3 >= 3)))
(chk "cmp6" (true? (infix 3 <= 3)))

;; check-meta
(let [hypot (from-string [x y] "sqrt(x**2 + y**2)")
      no-params (from-string "1 + 2")]
  (chk= "meta1" (meta hypot) {:params [:x :y] :doc "sqrt(x**2 + y**2)"})
  (chk= "meta2" (meta no-params) {:params [] :doc "1 + 2"}))

;; ------------------------------------------------------------------ core tests

(suppress! 'e)
(let [e 9]
  (chk= "suppress" (infix e * 3) 27))

;; --------------------------------------------------------------- grammar tests

;; check-var
(let [env {:x 32 :something_else 19 :dot.var 608}]
  (chk-throws "gvar-noparse" ex? #(parse-all g/var "54"))
  (chk= "gvar-x" ((parse-all g/var "x") env) 32)
  (chk= "gvar-se" ((parse-all g/var "something_else") env) 19)
  (chk= "gvar-dot" ((parse-all g/var "dot.var") env) 608)
  (chk-throws "gvar-fred" ise? #((parse-all g/var "fred") env)))

;; check-integer
(chk-throws "int-f" ex? #(parse-all g/integer "f"))
(chk-throws "int-rat" ex? #(parse-all g/integer "1/2"))
(chk-throws "int-dec" ex? #(parse-all g/integer "1.2"))
(chk-throws "int-bin" ex? #(parse-all g/integer "0b01011"))
(chk-throws "int-hex" ex? #(parse-all g/integer "0xDEADCAFE"))
(chk= "int-17" ((parse-all g/integer "17")) 17)
(chk= "int-neg17" ((parse-all g/integer "-17")) -17)
(chk= "int-big" ((parse-all g/integer "443243242444234217")) 443243242444234217)

;; check-binary
(chk-throws "bin-f" ex? #(parse-all g/binary "f"))
(chk-throws "bin-rat" ex? #(parse-all g/binary "1/2"))
(chk-throws "bin-dec" ex? #(parse-all g/binary "1.2"))
(chk= "bin-27" ((parse-all g/binary "0b011011")) 27)
(chk= "bin-neg27" ((parse-all g/binary "-0b011011")) -27)
(chk= "bin-big" ((parse-all g/binary "0b100000010011000001110001111011011")) 4334871515)

;; check-hex
(chk-throws "hex-f" ex? #(parse-all g/hex "f"))
(chk-throws "hex-rat" ex? #(parse-all g/hex "1/2"))
(chk-throws "hex-dec" ex? #(parse-all g/hex "1.2"))
(chk= "hex-cafe" ((parse-all g/hex "0xCAFEBABE")) 3405691582)
(chk= "hex-dead" ((parse-all g/hex "#DEADBEEF")) 3735928559)

;; check-rational
(chk-throws "rat-f" ex? #(parse-all g/rational "f"))
(chk-throws "rat-int" ex? #(parse-all g/rational "12"))
(chk-throws "rat-dec" ex? #(parse-all g/rational "1.2"))
(chk-throws "rat-bin" ex? #(parse-all g/rational "0b01011"))
(chk-throws "rat-hex" ex? #(parse-all g/rational "0xDEADCAFE"))
(chk= "rat-17" ((parse-all g/rational "1/7")) 1/7)
(chk= "rat-neg17" ((parse-all g/rational "-1/7")) -1/7)

;; check-decimal
(chk-throws "dec-f" ex? #(parse-all g/decimal "f"))
(chk-throws "dec-int" ex? #(parse-all g/decimal "12"))
(chk-throws "dec-rat" ex? #(parse-all g/decimal "1/2"))
(chk-throws "dec-bin" ex? #(parse-all g/decimal "0b01011"))
(chk-throws "dec-hex" ex? #(parse-all g/decimal "0xDEADCAFE"))
(chk= "dec-17" ((parse-all g/decimal "1.7")) 1.7)
(chk= "dec-neg17" ((parse-all g/decimal "-1.7")) -1.7)

;; check-number
(chk-throws "num-f" ex? #(parse-all g/number "f"))
(chk= "num-rat" ((parse-all g/number "6/72")) 1/12)
(chk= "num-17" ((parse-all g/number "17")) 17)
(chk= "num-dec" ((parse-all g/number "-1.7")) -1.7)
(chk= "num-bin" ((parse-all g/number "0b11110000")) 240)
(chk= "num-hex" ((parse-all g/number "#FFFE")) 65534)

;; check-list
(chk-throws "list-sp" ex? #(parse-all (g/list-of g/digits) " "))
(chk-throws "list-empty" ex? #(parse-all (g/list-of g/digits) ""))
(chk= "list-1" (parse-all (g/list-of g/digits) "1") ["1"])
(chk= "list-678" (parse-all (g/list-of g/digits) "6 , 7, 8 ") ["6" "7" "8"])
(chk= "list-fib" (parse-all (g/list-of g/digits) "1,1,2,3,5") ["1" "1" "2" "3" "5"])

;; check-function
(let [env {:x 81 :* * :sqrt (fn [x] (Math/sqrt x))}]
  (chk= "gfn-x" ((parse-all g/function "sqrt x") env) 9.0)
  (chk= "gfn-25" ((parse-all g/function "sqrt(25)") env) 5.0)
  (chk= "gfn-49" ((parse-all g/function "sqrt(7*7)") env) 7.0)
  (chk-throws "gfn-bargle" ise? #((parse-all g/function "bargle(7)") env))
  (chk-throws "gfn-arity" arity? #((parse-all g/function "sqrt(7, 5)") env)))

;; check-expression
(let [env (merge base-env {:t 0.324 :x_y_z 3 :_a 4 :_a.b 2})]
  (chk-near "gex-sin" ((parse-all g/expression "sin(2 * t) + 3 * cos(4 * t)") env) 1.4176457261295824)
  (chk-throws "gex-unbound" ise? #((parse-all g/expression "3 + 4") {}))
  (chk= "gex-43" ((parse-all g/expression "3 + 5 * 8") env) 43)
  (chk= "gex-64" ((parse-all g/expression "(3 + 5) * 8") env) 64)
  (chk= "gex-pow" ((parse-all g/expression "2 ** 2 ** 2 ** 2") env) 65536.0)
  (chk= "gex-xyz" ((parse-all g/expression "x_y_z * 2 + _a") env) 10)
  (chk= "gex-xyz2" ((parse-all g/expression "x_y_z * 2 + _a / _a.b") env) 8))

;; check-ternary-op
(let [env (merge base-env {:t 150 :x 10 :y 20})]
  (chk= "tern1" ((parse-all g/ternary-op "(t > 100) ? 1 : 0") env) 1)
  (chk= "tern2" ((parse-all g/ternary-op "(t < 100) ? x + y : x - y") env) -10)
  (chk= "tern3" ((parse-all g/ternary-op "(sum(x, y) >= 30) ? sum(x, y) : 0") env) 30))

;; check-baseenv-functions
(chk= "be-add" ((parse-all g/expression "9 + 7") base-env) 16)
(chk= "be-sub" ((parse-all g/expression "19 - 7") base-env) 12)
(chk= "be-mul" ((parse-all g/expression "9 * 7") base-env) 63)
(chk= "be-div" ((parse-all g/expression "19 / 75") base-env) 19/75)
(chk= "be-p1" ((parse-all g/expression "3 + 5 ** 2") base-env) 28.0)
(chk= "be-p2" ((parse-all g/expression "3 * 5 ** 2") base-env) 75.0)
(chk-near "be-pow" ((parse-all g/expression "pow(2.53, 3.1)") base-env) (Math/pow 2.53 3.1))
(chk-near "be-pow2" ((parse-all g/expression "7.01 ** 1.9") base-env) (Math/pow 7.01 1.9))
(chk-near "be-abs" ((parse-all g/expression "abs(-9.213)") base-env) (Math/abs -9.213))
(chk-near "be-signum" ((parse-all g/expression "signum(-9.213)") base-env) (Math/signum -9.213))
(chk-near "be-sqrt" ((parse-all g/expression "sqrt(24353)") base-env) (Math/sqrt 24353))
(chk-near "be-root" ((parse-all g/expression "root(3, 27)") base-env) 3)
(chk-near "be-exp" ((parse-all g/expression "exp(93)") base-env) (Math/exp 93))
(chk-near "be-log" ((parse-all g/expression "log(23.1)") base-env) (Math/log 23.1))
(chk-near "be-sin" ((parse-all g/expression "sin(1.91)") base-env) (Math/sin 1.91))
(chk-near "be-cos" ((parse-all g/expression "cos(2.791)") base-env) (Math/cos 2.791))
(chk-near "be-tan" ((parse-all g/expression "tan(44.3)") base-env) (Math/tan 44.3))
(chk-near "be-asin" ((parse-all g/expression "asin(0.99)") base-env) (Math/asin 0.99))
(chk-near "be-acos" ((parse-all g/expression "acos(0.04)") base-env) (Math/acos 0.04))
(chk-near "be-atan" ((parse-all g/expression "atan(0.11)") base-env) (Math/atan 0.11))
(chk-near "be-atan2" ((parse-all g/expression "atan2(0.1, 2)") base-env) (Math/atan2 0.1 2))
(chk-near "be-sinh" ((parse-all g/expression "sinh(60)") base-env) (Math/sinh 60))
(chk-near "be-cosh" ((parse-all g/expression "cosh(4)") base-env) (Math/cosh 4))
(chk-near "be-tanh" ((parse-all g/expression "tanh(1.9)") base-env) (Math/tanh 1.9))
(chk-near "be-sec" ((parse-all g/expression "sec(3.0)") base-env) (/ 1 (Math/cos 3)))
(chk-near "be-csc" ((parse-all g/expression "csc(2)") base-env) (/ 1 (Math/sin 2)))
(chk-near "be-cot" ((parse-all g/expression "cot(1.322)") base-env) (/ 1 (Math/tan 1.322)))
(chk-near "be-asec" ((parse-all g/expression "asec(3.0)") base-env) (Math/acos (/ 1 3)))
(chk-near "be-acsc" ((parse-all g/expression "acsc(33)") base-env) (Math/asin (/ 1 33)))
(chk-near "be-acot" ((parse-all g/expression "acot(0.21)") base-env) (Math/atan (/ 1 0.21)))
(chk-near "be-sum" ((parse-all g/expression "sum(1, 2, 5.7, 4)") base-env) (+ 1 2 5.7 4))
(chk-near "be-product" ((parse-all g/expression "product(1, 2, 5.7, 4)") base-env) (* 1 2 5.7 4))
(chk= "be-fact0" ((parse-all g/expression "fact 0") base-env) 1)
(chk= "be-fact5" ((parse-all g/expression "fact 5") base-env) 120)
(chk= "be-gcd" ((parse-all g/expression "gcd(8, 12)") base-env) 4)
(chk= "be-lcm" ((parse-all g/expression "lcm(8, 12)") base-env) 24)
(chk= "be-eq1" ((parse-all g/expression "3 = 3") base-env) true)
(chk= "be-eq2" ((parse-all g/expression "3 = (5 - 1)") base-env) false)
(chk= "be-neq" ((parse-all g/expression "3 != 5") base-env) true)
(chk= "be-eq3" ((parse-all g/expression "0 = 0.0") base-env) false)
(chk= "be-eqeq" ((parse-all g/expression "0 == 0.0") base-env) true)

;; ---------------------------------------------------------------------- report
(if (empty? @failures)
  (println "INFIX OK")
  (do
    (println "INFIX FAIL" (count @failures) "failures:")
    (doseq [f @failures] (println "  FAIL:" f))))
