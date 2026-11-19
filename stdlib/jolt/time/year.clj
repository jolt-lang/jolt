(ns jolt.time.year
  "Year and YearMonth."
  (:require [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]))

(defn- statics! [names members]
  (doseq [n names] (__register-class-statics! n members)))

;; --- Year --------------------------------------------------------------------

(defn year [y] (impl/value :jolt.time/year {:value y}))
(defn year-val [y] (impl/field y :value))
(defn year? [y] (= :jolt.time/year (impl/type-of y)))

(impl/register-type! :jolt.time/year
  {:eq   (fn [a b] (= (year-val a) (year-val b)))
   :hash (fn [y] (year-val y))
   :str  (fn [y] (str (year-val y)))
   :cmp  (fn [a b] (- (year-val a) (year-val b)))
   :classes #{"java.time.Year" "Year"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

(declare year-month)

(__register-class-methods! :jolt.time/year
  {"getValue"   year-val
   "isLeap"     (fn [y] (u/leap? (year-val y)))
   "length"     (fn [y] (if (u/leap? (year-val y)) 366 365))
   "plusYears"  (fn [y n] (year (+ (year-val y) (u/->long n))))
   "minusYears" (fn [y n] (year (- (year-val y) (u/->long n))))
   "atMonth"    (fn [y m] (year-month (year-val y) (e/month-num m)))
   "atDay"      (fn [y doy] (l/local-date (+ (days-from-civil (year-val y) 1 1) (dec (u/->long doy)))))
   "isBefore"   (fn [y o] (< (year-val y) (year-val o)))
   "isAfter"    (fn [y o] (> (year-val y) (year-val o)))
   "compareTo"  compare
   "equals"     (fn [y o] (boolean (and (impl/jt? o) (year? o) (= (year-val y) (year-val o)))))
   "hashCode"   year-val
   "toString"   (fn [y] (str (year-val y)))})

;; The parsers of Year, YearMonth and MonthDay, as strict as the JDK's: a text
;; that does not match names the index it failed at, and fields out of range
;; are the resolve failure each type's from() raises.
(defn parse-year
  "Year.parse: an optional sign and up to nine digits."
  [s]
  (let [sign (when (pos? (count s)) (#{\+ \-} (nth s 0)))
        d0 (if sign 1 0)
        d1 (u/digits-from s d0 9)]
    (when (= d0 d1) (u/parse-fail-at s d0))
    (u/parse-end! s d1)
    (let [y (parse-long (subs s d0 d1))] (if (= sign \-) (- y) y))))

(defn parse-year-month
  "YearMonth.parse: uuuu-MM. [year month]."
  [s]
  (let [[y e] (u/scan-iso-year s 0)
        _ (when-not (u/char-at? s e \-) (u/parse-fail-at s e))
        m (or (u/two-digits s (inc e)) (u/parse-fail-at s (inc e)))]
    (u/parse-end! s (+ e 3))
    ;; the JDK resolves both fields together, so a year out of range and a
    ;; month out of range fail with the same message
    (when-not (and (<= -999999999 y 999999999) (<= 1 m 12))
      (u/parse-fail-unobtainable s "YearMonth" (str "Year=" y ", MonthOfYear=" m)))
    [y m]))

(defn parse-month-day
  "MonthDay.parse: --MM-dd, February 29 included. [month day]."
  [s]
  (when-not (and (u/char-at? s 0 \-) (u/char-at? s 1 \-)) (u/parse-fail-at s 0))
  (let [m (or (u/two-digits s 2) (u/parse-fail-at s 2))
        _ (when-not (u/char-at? s 4 \-) (u/parse-fail-at s 4))
        d (or (u/two-digits s 5) (u/parse-fail-at s 5))]
    (u/parse-end! s 7)
    (when-not (and (<= 1 m 12) (<= 1 d (u/len-of-month 2000 m)))
      (u/parse-fail-unobtainable s "MonthDay" (str "DayOfMonth=" d ", MonthOfYear=" m)))
    [m d]))

(statics! ["Year" "java.time.Year"]
  {"of"    (fn [y] (year (u/->long y)))
   "now"   (fn [& args] (year (first (civil-from-days (u/floor-div (impl/clock-millis (first args)) 86400000)))))
   "isLeap" (fn [y] (u/leap? (u/->long y)))
   "parse" (fn [s & _] (year (parse-year (str s))))
   "MIN_VALUE" -999999999
   "MAX_VALUE" 999999999})

;; --- YearMonth ---------------------------------------------------------------

(defn year-month
  "Construct a normalized YearMonth (month overflow rolls into the year)."
  [y m]
  (let [ym (+ (* y 12) (dec m))]
    (impl/value :jolt.time/year-month {:year (u/floor-div ym 12) :month (inc (u/floor-mod ym 12))})))
(defn ym-year [x] (impl/field x :year))
(defn ym-month [x] (impl/field x :month))
(defn ym? [x] (= :jolt.time/year-month (impl/type-of x)))
(defn- ym-key [x] [(ym-year x) (ym-month x)])
(defn- ym-cmp [a b] (impl/first-nonzero (- (ym-year a) (ym-year b)) (- (ym-month a) (ym-month b))))
(defn- ym-plus-months [x n]
  (let [tm (+ (* (ym-year x) 12) (dec (ym-month x)) n)]
    (impl/value :jolt.time/year-month {:year (u/floor-div tm 12) :month (inc (u/floor-mod tm 12))})))
;; YearMonth.toString pads a year under four digits but, unlike LocalDate, never
;; signs one past four: 10000-01, not +10000-01.
(defn- ym->string [x]
  (let [y (ym-year x)]
    (str (if (< (abs y) 1000) (u/pad4 y) (str y)) "-" (u/pad2 (ym-month x)))))

(impl/register-type! :jolt.time/year-month
  {:eq   (fn [a b] (= (ym-key a) (ym-key b)))
   :hash (fn [x] (+ (* (ym-year x) 13) (ym-month x)))
   :str  ym->string
   :cmp  ym-cmp
   :classes #{"java.time.YearMonth" "YearMonth"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/year-month
  {"getYear"       ym-year
   "getMonthValue" ym-month
   "getMonth"      (fn [x] (e/month (ym-month x)))
   "lengthOfMonth" (fn [x] (u/len-of-month (ym-year x) (ym-month x)))
   "lengthOfYear"  (fn [x] (if (u/leap? (ym-year x)) 366 365))
   "isLeapYear"    (fn [x] (u/leap? (ym-year x)))
   "plusMonths"    (fn [x n] (ym-plus-months x (u/->long n)))
   "minusMonths"   (fn [x n] (ym-plus-months x (- (u/->long n))))
   "plusYears"     (fn [x n] (year-month (+ (ym-year x) (u/->long n)) (ym-month x)))
   "minusYears"    (fn [x n] (year-month (- (ym-year x) (u/->long n)) (ym-month x)))
   "withYear"      (fn [x v] (year-month (u/->long v) (ym-month x)))
   "withMonth"     (fn [x v] (year-month (ym-year x) (u/->long v)))
   "atDay"         (fn [x d] (l/local-date (days-from-civil (ym-year x) (ym-month x) (u/->long d))))
   "atEndOfMonth"  (fn [x] (l/local-date (days-from-civil (ym-year x) (ym-month x) (u/len-of-month (ym-year x) (ym-month x)))))
   "isValidDay"    (fn [x d] (let [dd (u/->long d)] (and (>= dd 1) (<= dd (u/len-of-month (ym-year x) (ym-month x))))))
   "isBefore"      (fn [x o] (neg? (ym-cmp x o)))
   "isAfter"       (fn [x o] (pos? (ym-cmp x o)))
   "compareTo"     compare
   "equals"        (fn [x o] (boolean (and (impl/jt? o) (ym? o) (= (ym-key x) (ym-key o)))))
   "hashCode"      (fn [x] (+ (* (ym-year x) 13) (ym-month x)))
   "toString"      ym->string})

(statics! ["YearMonth" "java.time.YearMonth"]
  {"of"    (fn [y m] (year-month (u/->long y) (e/month-num m)))
   "now"   (fn [& args] (let [[y m _] (civil-from-days (u/floor-div (impl/clock-millis (first args)) 86400000))] (year-month y m)))
   "parse" (fn [s & _] (let [[y m] (parse-year-month (str s))] (year-month y m)))})

;; --- MonthDay ----------------------------------------------------------------

(defn month-day [m d] (impl/value :jolt.time/month-day {:month m :day d}))
(defn md-month [x] (impl/field x :month))
(defn md-day [x] (impl/field x :day))
(defn md? [x] (= :jolt.time/month-day (impl/type-of x)))
(defn- md->string [x] (str "--" (u/pad2 (md-month x)) "-" (u/pad2 (md-day x))))

(impl/register-type! :jolt.time/month-day
  {:eq (fn [a b] (= [(md-month a) (md-day a)] [(md-month b) (md-day b)]))
   :hash (fn [x] (+ (* (md-month x) 100) (md-day x)))
   :str md->string :cmp (fn [a b] (impl/first-nonzero (- (md-month a) (md-month b)) (- (md-day a) (md-day b))))
   :classes #{"java.time.MonthDay" "MonthDay" "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster" "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/month-day
  {"getMonthValue" md-month "getDayOfMonth" md-day "getMonth" (fn [x] (e/month (md-month x)))
   "atYear" (fn [x y] (l/local-date (days-from-civil (u/->long y) (md-month x) (md-day x))))
   "isValidYear" (fn [x y] (<= (md-day x) (u/len-of-month (u/->long y) (md-month x))))
   "compareTo" compare
   "equals" (fn [x o] (boolean (and (impl/jt? o) (md? o) (= (md-month x) (md-month o)) (= (md-day x) (md-day o)))))
   "hashCode" (fn [x] (+ (* (md-month x) 100) (md-day x))) "toString" md->string})

(statics! ["MonthDay" "java.time.MonthDay"]
  {"of" (fn [m d] (month-day (e/month-num m) (u/->long d)))
   "parse" (fn [s & _] (let [[m d] (parse-month-day (str s))] (month-day m d)))
   "now" (fn [& args] (let [[_ m dd] (civil-from-days (u/floor-div (impl/clock-millis (first args)) 86400000))] (month-day m dd)))})
