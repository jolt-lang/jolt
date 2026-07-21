(ns jolt.time.local
  "LocalDate, LocalTime, and LocalDateTime — the timezone-free temporals. A date
  is an epoch-day count, a time a nano-of-day count, a date-time the pair.
  atZone/atOffset and toInstant are added by the zone and instant namespaces."
  (:require [jolt.time.impl :as impl :refer [days-from-civil civil-from-days]]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]))

(defn- statics! [names members]
  (doseq [n names] (__register-class-statics! n members)))

(def ^:private npd u/nanos-per-day)
(def ^:private nps u/nanos-per-sec)

;; --- shared date/time helpers ------------------------------------------------
(defn ld-dow [ed] (inc (u/floor-mod (+ ed 3) 7)))
(defn ld-day-of-year [ed] (let [[y _ _] (civil-from-days ed)] (inc (- ed (days-from-civil y 1 1)))))
(defn iso-date-str [ed] (let [[y m d] (civil-from-days ed)] (str (u/pad4 y) "-" (u/pad2 m) "-" (u/pad2 d))))
(defn iso-datetime-str [ed nod] (str (iso-date-str ed) "T" (u/iso-time-str nod)))

(defn parse-iso-date [s]
  ;; yyyy-MM-dd, plus proleptic/extended years like -999999999-01-01.
  (let [neg (= \- (nth s 0)) body (if neg (subs s 1) s) n (count body)
        d1 (loop [i 0] (if (or (>= i n) (= \- (nth body i))) i (recur (inc i))))
        d2 (loop [i (inc d1)] (if (or (>= i n) (= \- (nth body i))) i (recur (inc i))))
        y (parse-long (subs body 0 d1)) m (parse-long (subs body (inc d1) d2)) d (parse-long (subs body (inc d2)))]
    (if (and y m d)
      (days-from-civil (* (if neg -1 1) y) m d)
      (throw (ex-info (str "could not parse LocalDate: " s) {})))))

(defn lt-hour [nod] (quot nod (* 3600 nps)))
(defn lt-minute [nod] (mod (quot nod (* 60 nps)) 60))
(defn lt-second [nod] (mod (quot nod nps) 60))
(defn lt-nano [nod] (mod nod nps))

;; --- LocalDate ---------------------------------------------------------------

(defn local-date [ed] (impl/value :jolt.time/local-date {:epoch-day ed}))
(defn ld-epoch-day [d] (impl/field d :epoch-day))
(defn ld? [d] (= :jolt.time/local-date (impl/type-of d)))

(defn- ld-of [y m d] (local-date (days-from-civil y m d)))
(defn- ld-from-ms [ms] (local-date (u/floor-div ms 86400000)))

(defn ld-plus-months [d n]
  (let [[y m dom] (civil-from-days (ld-epoch-day d))
        ym (+ (* y 12) (dec m) n)
        y2 (u/floor-div ym 12) m2 (inc (u/floor-mod ym 12))]
    (local-date (days-from-civil y2 m2 (min dom (u/len-of-month y2 m2))))))
(defn ld-plus-years [d n]
  (let [[y m dom] (civil-from-days (ld-epoch-day d))]
    (local-date (days-from-civil (+ y n) m (min dom (u/len-of-month (+ y n) m))))))
(defn ld-with [d which v]
  (let [[y m dom] (civil-from-days (ld-epoch-day d))]
    (case which
      :year  (local-date (days-from-civil v m (min dom (u/len-of-month v m))))
      :month (local-date (days-from-civil y v (min dom (u/len-of-month y v))))
      :day   (local-date (days-from-civil y m v))
      :doy   (local-date (+ (days-from-civil y 1 1) (dec v))))))

(impl/register-type! :jolt.time/local-date
  {:eq   (fn [a b] (= (ld-epoch-day a) (ld-epoch-day b)))
   :hash (fn [d] (ld-epoch-day d))
   :str  (fn [d] (iso-date-str (ld-epoch-day d)))
   :cmp  (fn [a b] (compare (ld-epoch-day a) (ld-epoch-day b)))
   :classes #{"java.time.LocalDate" "LocalDate"
              "java.time.chrono.ChronoLocalDate" "ChronoLocalDate"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

;; forward decl of the LocalDateTime constructor (defined below)
(declare local-dt lt-nano-of-day local-time)

(__register-class-methods! :jolt.time/local-date
  {"getYear"        (fn [d] (first (civil-from-days (ld-epoch-day d))))
   "getMonthValue"  (fn [d] (second (civil-from-days (ld-epoch-day d))))
   "getDayOfMonth"  (fn [d] (nth (civil-from-days (ld-epoch-day d)) 2))
   "getMonth"       (fn [d] (e/month (second (civil-from-days (ld-epoch-day d)))))
   "getDayOfWeek"   (fn [d] (e/dow (ld-dow (ld-epoch-day d))))
   "getDayOfYear"   (fn [d] (ld-day-of-year (ld-epoch-day d)))
   "toEpochDay"     (fn [d] (ld-epoch-day d))
   "lengthOfMonth"  (fn [d] (let [[y m _] (civil-from-days (ld-epoch-day d))] (u/len-of-month y m)))
   "lengthOfYear"   (fn [d] (let [[y _ _] (civil-from-days (ld-epoch-day d))] (if (u/leap? y) 366 365)))
   "isLeapYear"     (fn [d] (u/leap? (first (civil-from-days (ld-epoch-day d)))))
   "plusDays"       (fn [d n] (local-date (+ (ld-epoch-day d) (u/->long n))))
   "minusDays"      (fn [d n] (local-date (- (ld-epoch-day d) (u/->long n))))
   "plusWeeks"      (fn [d n] (local-date (+ (ld-epoch-day d) (* 7 (u/->long n)))))
   "minusWeeks"     (fn [d n] (local-date (- (ld-epoch-day d) (* 7 (u/->long n)))))
   "plusMonths"     (fn [d n] (ld-plus-months d (u/->long n)))
   "minusMonths"    (fn [d n] (ld-plus-months d (- (u/->long n))))
   "plusYears"      (fn [d n] (ld-plus-years d (u/->long n)))
   "minusYears"     (fn [d n] (ld-plus-years d (- (u/->long n))))
   "withYear"       (fn [d v] (ld-with d :year (u/->long v)))
   "withMonth"      (fn [d v] (ld-with d :month (u/->long v)))
   "withDayOfMonth" (fn [d v] (ld-with d :day (u/->long v)))
   "withDayOfYear"  (fn [d v] (ld-with d :doy (u/->long v)))
   "isBefore"       (fn [d o] (< (ld-epoch-day d) (ld-epoch-day o)))
   "isAfter"        (fn [d o] (> (ld-epoch-day d) (ld-epoch-day o)))
   "isEqual"        (fn [d o] (= (ld-epoch-day d) (ld-epoch-day o)))
   "compareTo"      (fn [d o] (compare (ld-epoch-day d) (ld-epoch-day o)))
   "equals"         (fn [d o] (boolean (and (impl/jt? o) (ld? o) (= (ld-epoch-day d) (ld-epoch-day o)))))
   "hashCode"       (fn [d] (ld-epoch-day d))
   "atStartOfDay"   (fn [d & _] (local-dt (ld-epoch-day d) 0))
   "atTime"         (fn ([d t] (local-dt (ld-epoch-day d) (lt-nano-of-day t)))
                        ([d h m] (local-dt (ld-epoch-day d) (u/hmsn->nano (u/->long h) (u/->long m) 0 0)))
                        ([d h m s] (local-dt (ld-epoch-day d) (u/hmsn->nano (u/->long h) (u/->long m) (u/->long s) 0)))
                        ([d h m s nano] (local-dt (ld-epoch-day d) (u/hmsn->nano (u/->long h) (u/->long m) (u/->long s) (u/->long nano)))))
   "toString"       (fn [d] (iso-date-str (ld-epoch-day d)))})

(statics! ["LocalDate" "java.time.LocalDate"]
  {"of"         (fn [y m d] (ld-of (u/->long y) (u/->long m) (u/->long d)))
   "ofEpochDay" (fn [n] (local-date (u/->long n)))
   "ofYearDay"  (fn [y doy] (local-date (+ (days-from-civil (u/->long y) 1 1) (dec (u/->long doy)))))
   "parse"      (fn [s & _] (local-date (parse-iso-date (str s))))
   "now"        (fn [& args] (ld-from-ms (impl/clock-millis (first args))))
   "MIN"        (ld-of -999999999 1 1)
   "MAX"        (ld-of 999999999 12 31)})

;; --- LocalTime ---------------------------------------------------------------

(defn local-time [nod] (impl/value :jolt.time/local-time {:nod nod}))
(defn lt-nano-of-day [t] (impl/field t :nod))
(defn lt? [t] (= :jolt.time/local-time (impl/type-of t)))

(defn lt-plus [t nanos] (local-time (u/floor-mod (+ (lt-nano-of-day t) nanos) npd)))
(defn lt-with [t which v]
  (let [nod (lt-nano-of-day t) h (lt-hour nod) mi (lt-minute nod) s (lt-second nod) nano (lt-nano nod)]
    (case which
      :hour   (local-time (u/hmsn->nano v mi s nano))
      :minute (local-time (u/hmsn->nano h v s nano))
      :second (local-time (u/hmsn->nano h mi v nano))
      :nano   (local-time (u/hmsn->nano h mi s v)))))

(def ^:private trunc-div
  {"NANOS" 1 "MICROS" 1000 "MILLIS" 1000000 "SECONDS" u/nanos-per-sec
   "MINUTES" (* 60 u/nanos-per-sec) "HOURS" (* 3600 u/nanos-per-sec) "DAYS" u/nanos-per-day})

(defn unit-name [u]
  (cond (and (impl/jt? u) (= :jolt.time/chrono-unit (impl/type-of u))) (e/cu-name u)
        (string? u) u
        (keyword? u) (name u)))
(defn lt-truncate [nod u] (let [div (get trunc-div (unit-name u) 1)] (* (quot nod div) div)))

(impl/register-type! :jolt.time/local-time
  {:eq   (fn [a b] (= (lt-nano-of-day a) (lt-nano-of-day b)))
   :hash (fn [t] (lt-nano-of-day t))
   :str  (fn [t] (u/iso-time-str (lt-nano-of-day t)))
   :cmp  (fn [a b] (compare (lt-nano-of-day a) (lt-nano-of-day b)))
   :classes #{"java.time.LocalTime" "LocalTime"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/local-time
  {"getHour"     (fn [t] (lt-hour (lt-nano-of-day t)))
   "getMinute"   (fn [t] (lt-minute (lt-nano-of-day t)))
   "getSecond"   (fn [t] (lt-second (lt-nano-of-day t)))
   "getNano"     (fn [t] (lt-nano (lt-nano-of-day t)))
   "toNanoOfDay" (fn [t] (lt-nano-of-day t))
   "toSecondOfDay" (fn [t] (quot (lt-nano-of-day t) nps))
   "plusHours"   (fn [t n] (lt-plus t (* (u/->long n) 3600 nps)))
   "minusHours"  (fn [t n] (lt-plus t (- (* (u/->long n) 3600 nps))))
   "plusMinutes" (fn [t n] (lt-plus t (* (u/->long n) 60 nps)))
   "minusMinutes" (fn [t n] (lt-plus t (- (* (u/->long n) 60 nps))))
   "plusSeconds" (fn [t n] (lt-plus t (* (u/->long n) nps)))
   "minusSeconds" (fn [t n] (lt-plus t (- (* (u/->long n) nps))))
   "plusNanos"   (fn [t n] (lt-plus t (u/->long n)))
   "minusNanos"  (fn [t n] (lt-plus t (- (u/->long n))))
   "withHour"    (fn [t v] (lt-with t :hour (u/->long v)))
   "withMinute"  (fn [t v] (lt-with t :minute (u/->long v)))
   "withSecond"  (fn [t v] (lt-with t :second (u/->long v)))
   "withNano"    (fn [t v] (lt-with t :nano (u/->long v)))
   "truncatedTo" (fn [t u] (local-time (lt-truncate (lt-nano-of-day t) u)))
   "isBefore"    (fn [t o] (< (lt-nano-of-day t) (lt-nano-of-day o)))
   "isAfter"     (fn [t o] (> (lt-nano-of-day t) (lt-nano-of-day o)))
   "compareTo"   (fn [t o] (compare (lt-nano-of-day t) (lt-nano-of-day o)))
   "equals"      (fn [t o] (boolean (and (impl/jt? o) (lt? o) (= (lt-nano-of-day t) (lt-nano-of-day o)))))
   "hashCode"    (fn [t] (lt-nano-of-day t))
   "atDate"      (fn [t d] (local-dt (ld-epoch-day d) (lt-nano-of-day t)))
   "toString"    (fn [t] (u/iso-time-str (lt-nano-of-day t)))})

(statics! ["LocalTime" "java.time.LocalTime"]
  {"of"           (fn ([h m] (local-time (u/hmsn->nano (u/->long h) (u/->long m) 0 0)))
                      ([h m s] (local-time (u/hmsn->nano (u/->long h) (u/->long m) (u/->long s) 0)))
                      ([h m s nano] (local-time (u/hmsn->nano (u/->long h) (u/->long m) (u/->long s) (u/->long nano)))))
   "ofNanoOfDay"  (fn [n] (local-time (u/->long n)))
   "ofSecondOfDay" (fn [n] (local-time (* (u/->long n) nps)))
   "parse"        (fn [s & _] (local-time (u/parse-hms->nano (str s))))
   "now"          (fn [& args] (local-time (* (u/floor-mod (impl/clock-millis (first args)) 86400000) 1000000)))
   "MIN"          (local-time 0)
   "MAX"          (local-time (dec npd))
   "MIDNIGHT"     (local-time 0)
   "NOON"         (local-time (* 12 3600 nps))})

;; --- LocalDateTime -----------------------------------------------------------

(defn local-dt [ed nod] (impl/value :jolt.time/local-date-time {:epoch-day ed :nod nod}))
(defn ldt-epoch-day [x] (impl/field x :epoch-day))
(defn ldt-nod [x] (impl/field x :nod))
(defn ldt? [x] (= :jolt.time/local-date-time (impl/type-of x)))
(defn- ldt-key [x] [(ldt-epoch-day x) (ldt-nod x)])
(defn- ldt-cmp [a b] (compare (ldt-key a) (ldt-key b)))

(defn ldt-date [x] (local-date (ldt-epoch-day x)))
(defn ldt-time [x] (local-time (ldt-nod x)))
(defn ldt-combine [d t] (local-dt (ld-epoch-day d) (lt-nano-of-day t)))
(defn ldt-plus-nanos [x nanos]
  (let [total (+ (ldt-nod x) nanos)]
    (local-dt (+ (ldt-epoch-day x) (u/floor-div total npd)) (u/floor-mod total npd))))
(defn ldt->ms [x] (+ (* (ldt-epoch-day x) 86400000) (quot (ldt-nod x) 1000000)))
(defn ldt->epoch-second [x] (+ (* (ldt-epoch-day x) 86400) (quot (ldt-nod x) nps)))

(impl/register-type! :jolt.time/local-date-time
  {:eq   (fn [a b] (= (ldt-key a) (ldt-key b)))
   :hash (fn [x] (+ (* (ldt-epoch-day x) 31) (ldt-nod x)))
   :str  (fn [x] (iso-datetime-str (ldt-epoch-day x) (ldt-nod x)))
   :cmp  ldt-cmp
   :classes #{"java.time.LocalDateTime" "LocalDateTime"
              "java.time.chrono.ChronoLocalDateTime" "ChronoLocalDateTime"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/local-date-time
  {"getYear"       (fn [x] (first (civil-from-days (ldt-epoch-day x))))
   "getMonthValue" (fn [x] (second (civil-from-days (ldt-epoch-day x))))
   "getMonth"      (fn [x] (e/month (second (civil-from-days (ldt-epoch-day x)))))
   "getDayOfMonth" (fn [x] (nth (civil-from-days (ldt-epoch-day x)) 2))
   "getDayOfWeek"  (fn [x] (e/dow (ld-dow (ldt-epoch-day x))))
   "getDayOfYear"  (fn [x] (ld-day-of-year (ldt-epoch-day x)))
   "getHour"       (fn [x] (lt-hour (ldt-nod x)))
   "getMinute"     (fn [x] (lt-minute (ldt-nod x)))
   "getSecond"     (fn [x] (lt-second (ldt-nod x)))
   "getNano"       (fn [x] (lt-nano (ldt-nod x)))
   "toLocalDate"   (fn [x] (ldt-date x))
   "toLocalTime"   (fn [x] (ldt-time x))
   "toEpochSecond" (fn [x & _] (ldt->epoch-second x))
   "plusDays"      (fn [x n] (local-dt (+ (ldt-epoch-day x) (u/->long n)) (ldt-nod x)))
   "minusDays"     (fn [x n] (local-dt (- (ldt-epoch-day x) (u/->long n)) (ldt-nod x)))
   "plusWeeks"     (fn [x n] (local-dt (+ (ldt-epoch-day x) (* 7 (u/->long n))) (ldt-nod x)))
   "minusWeeks"    (fn [x n] (local-dt (- (ldt-epoch-day x) (* 7 (u/->long n))) (ldt-nod x)))
   "plusMonths"    (fn [x n] (ldt-combine (ld-plus-months (ldt-date x) (u/->long n)) (ldt-time x)))
   "minusMonths"   (fn [x n] (ldt-combine (ld-plus-months (ldt-date x) (- (u/->long n))) (ldt-time x)))
   "plusYears"     (fn [x n] (ldt-combine (ld-plus-years (ldt-date x) (u/->long n)) (ldt-time x)))
   "minusYears"    (fn [x n] (ldt-combine (ld-plus-years (ldt-date x) (- (u/->long n))) (ldt-time x)))
   "plusHours"     (fn [x n] (ldt-plus-nanos x (* (u/->long n) 3600 nps)))
   "minusHours"    (fn [x n] (ldt-plus-nanos x (- (* (u/->long n) 3600 nps))))
   "plusMinutes"   (fn [x n] (ldt-plus-nanos x (* (u/->long n) 60 nps)))
   "minusMinutes"  (fn [x n] (ldt-plus-nanos x (- (* (u/->long n) 60 nps))))
   "plusSeconds"   (fn [x n] (ldt-plus-nanos x (* (u/->long n) nps)))
   "minusSeconds"  (fn [x n] (ldt-plus-nanos x (- (* (u/->long n) nps))))
   "plusNanos"     (fn [x n] (ldt-plus-nanos x (u/->long n)))
   "minusNanos"    (fn [x n] (ldt-plus-nanos x (- (u/->long n))))
   "withYear"      (fn [x v] (ldt-combine (ld-with (ldt-date x) :year (u/->long v)) (ldt-time x)))
   "withMonth"     (fn [x v] (ldt-combine (ld-with (ldt-date x) :month (u/->long v)) (ldt-time x)))
   "withDayOfMonth" (fn [x v] (ldt-combine (ld-with (ldt-date x) :day (u/->long v)) (ldt-time x)))
   "withDayOfYear" (fn [x v] (ldt-combine (ld-with (ldt-date x) :doy (u/->long v)) (ldt-time x)))
   "withHour"      (fn [x v] (ldt-combine (ldt-date x) (lt-with (ldt-time x) :hour (u/->long v))))
   "withMinute"    (fn [x v] (ldt-combine (ldt-date x) (lt-with (ldt-time x) :minute (u/->long v))))
   "withSecond"    (fn [x v] (ldt-combine (ldt-date x) (lt-with (ldt-time x) :second (u/->long v))))
   "withNano"      (fn [x v] (ldt-combine (ldt-date x) (lt-with (ldt-time x) :nano (u/->long v))))
   "truncatedTo"   (fn [x u] (ldt-combine (ldt-date x) (local-time (lt-truncate (ldt-nod x) u))))
   "isBefore"      (fn [x o] (neg? (ldt-cmp x o)))
   "isAfter"       (fn [x o] (pos? (ldt-cmp x o)))
   "isEqual"       (fn [x o] (= (ldt-key x) (ldt-key o)))
   "compareTo"     ldt-cmp
   "equals"        (fn [x o] (boolean (and (impl/jt? o) (ldt? o) (= (ldt-key x) (ldt-key o)))))
   "hashCode"      (fn [x] (+ (* (ldt-epoch-day x) 31) (ldt-nod x)))
   "toString"      (fn [x] (iso-datetime-str (ldt-epoch-day x) (ldt-nod x)))})

(statics! ["LocalDateTime" "java.time.LocalDateTime"]
  {"of"      (fn ([d t] (local-dt (ld-epoch-day d) (lt-nano-of-day t)))
                ([y mo d h mi] (local-dt (days-from-civil (u/->long y) (u/->long mo) (u/->long d)) (u/hmsn->nano (u/->long h) (u/->long mi) 0 0)))
                ([y mo d h mi s] (local-dt (days-from-civil (u/->long y) (u/->long mo) (u/->long d)) (u/hmsn->nano (u/->long h) (u/->long mi) (u/->long s) 0)))
                ([y mo d h mi s nano] (local-dt (days-from-civil (u/->long y) (u/->long mo) (u/->long d)) (u/hmsn->nano (u/->long h) (u/->long mi) (u/->long s) (u/->long nano)))))
   "ofEpochSecond" (fn [secs nano _off]
                     (let [es (u/->long secs)]
                       (local-dt (u/floor-div es 86400) (+ (* (u/floor-mod es 86400) nps) (u/->long nano)))))
   "parse"   (fn [s & _] (let [d (str s) t (.indexOf d "T")]
                           (local-dt (parse-iso-date (subs d 0 t)) (u/parse-hms->nano (subs d (inc t))))))
   "now"     (fn [& args] (let [ms (impl/clock-millis (first args))] (local-dt (u/floor-div ms 86400000) (* (u/floor-mod ms 86400000) 1000000))))
   "MIN"     (local-dt (days-from-civil -999999999 1 1) 0)
   "MAX"     (local-dt (days-from-civil 999999999 12 31) (dec npd))})
