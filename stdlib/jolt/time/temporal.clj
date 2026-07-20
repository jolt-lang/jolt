(ns jolt.time.temporal
  "The generic temporal machinery: plus/minus/until/get/getLong/with/isSupported/
  range across the temporal types, TemporalAdjusters, and the TemporalAmount /
  ChronoUnit / ChronoField operations that reach into a temporal. Registered
  additively onto the types the local/amount/year namespaces define; the instant
  namespace extends these with its own branches."
  (:require [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]
            [jolt.time.amount :as a]
            [jolt.time.year :as y]))

;; --- name extraction ---------------------------------------------------------
(defn- unit-name [x]
  (cond (and (impl/jt? x) (= :jolt.time/chrono-unit (impl/type-of x))) (e/cu-name x)
        (string? x) x (keyword? x) (name x)))
(defn- field-name [x]
  (cond (and (impl/jt? x) (= :jolt.time/chrono-field (impl/type-of x))) (e/cf-name x)
        (string? x) x (keyword? x) (name x)))

(defn- tof [t] (impl/type-of t))
(def ^:private DATE :jolt.time/local-date)
(def ^:private TIME :jolt.time/local-time)
(def ^:private DT   :jolt.time/local-date-time)

;; --- plus by unit ------------------------------------------------------------
;; extensible per type: the instant namespace assoc's its own fn in.
(defonce plus-unit-fns (atom {}))
(defn register-plus-unit! [type-kw f] (swap! plus-unit-fns assoc type-kw f))

(defn plus-unit [t n unit]
  (let [u (u/upper unit)]
    (condp = (tof t)
      DATE (case u
             "DAYS" (l/local-date (+ (l/ld-epoch-day t) n))
             "WEEKS" (l/local-date (+ (l/ld-epoch-day t) (* 7 n)))
             "MONTHS" (l/ld-plus-months t n)
             "YEARS" (l/ld-plus-years t n)
             "DECADES" (l/ld-plus-years t (* 10 n))
             "CENTURIES" (l/ld-plus-years t (* 100 n))
             "MILLENNIA" (l/ld-plus-years t (* 1000 n))
             (throw (ex-info (str "LocalDate plus unsupported unit " u) {})))
      TIME (l/lt-plus t (* n (or (u/chrono-unit-nanos u) (throw (ex-info (str "no nanos for " u) {})))))
      DT (case u
           "DAYS" (l/local-dt (+ (l/ldt-epoch-day t) n) (l/ldt-nod t))
           "WEEKS" (l/local-dt (+ (l/ldt-epoch-day t) (* 7 n)) (l/ldt-nod t))
           "MONTHS" (l/ldt-combine (l/ld-plus-months (l/ldt-date t) n) (l/ldt-time t))
           "YEARS" (l/ldt-combine (l/ld-plus-years (l/ldt-date t) n) (l/ldt-time t))
           "DECADES" (l/ldt-combine (l/ld-plus-years (l/ldt-date t) (* 10 n)) (l/ldt-time t))
           "CENTURIES" (l/ldt-combine (l/ld-plus-years (l/ldt-date t) (* 100 n)) (l/ldt-time t))
           "MILLENNIA" (l/ldt-combine (l/ld-plus-years (l/ldt-date t) (* 1000 n)) (l/ldt-time t))
           (l/ldt-plus-nanos t (* n (u/chrono-unit-nanos u))))
      (if-let [f (get @plus-unit-fns (tof t))]
        (f t n u)
        (throw (ex-info "plus: unsupported temporal" {:t t}))))))

;; --- plus raw nanos (Duration.addTo) -----------------------------------------
(defonce plus-nanos-fns (atom {}))
(defn register-plus-nanos! [type-kw f] (swap! plus-nanos-fns assoc type-kw f))
(defn plus-nanos [t nanos]
  (condp = (tof t)
    TIME (l/lt-plus t nanos)
    DT (l/ldt-plus-nanos t nanos)
    DATE (l/local-date (+ (l/ld-epoch-day t) (quot nanos (* 86400 u/nanos-per-sec))))
    (if-let [f (get @plus-nanos-fns (tof t))] (f t nanos)
      (throw (ex-info "plus(Duration): unsupported temporal" {:t t})))))

;; --- plus a Period -----------------------------------------------------------
(defn plus-period [t p sign]
  (let [yy (* sign (a/per-years p)) mm (* sign (a/per-months p)) dd (* sign (a/per-days p))]
    (condp = (tof t)
      DATE (l/local-date (+ (l/ld-epoch-day (l/ld-plus-months (l/ld-plus-years t yy) mm)) dd))
      DT (let [nd (l/ld-plus-months (l/ld-plus-years (l/ldt-date t) yy) mm)]
           (l/local-dt (+ (l/ld-epoch-day nd) dd) (l/ldt-nod t)))
      (throw (ex-info "plus(Period): unsupported temporal" {:t t})))))

;; --- unit-between ------------------------------------------------------------
(defn date-months-between [da db]
  (let [[y1 m1 d1] (civil-from-days da) [y2 m2 d2] (civil-from-days db)
        months (- (+ (* y2 12) m2) (+ (* y1 12) m1))]
    (cond (and (pos? months) (< d2 d1)) (dec months)
          (and (neg? months) (> d2 d1)) (inc months)
          :else months)))

(defonce between-fns (atom {}))
(defn register-between! [type-kw f] (swap! between-fns assoc type-kw f))

(defn unit-between [unit a b]
  (let [u (u/upper unit) ta (tof a)]
    (cond
      (and (= DATE ta) (= DATE (tof b)))
      (let [da (l/ld-epoch-day a) db (l/ld-epoch-day b)]
        (case u "DAYS" (- db da) "WEEKS" (quot (- db da) 7)
             "MONTHS" (date-months-between da db)
             "YEARS" (quot (date-months-between da db) 12)
             "DECADES" (quot (date-months-between da db) 120)
             "CENTURIES" (quot (date-months-between da db) 1200)
             (throw (ex-info (str "between unsupported unit " u) {}))))
      (and (= TIME ta) (= TIME (tof b)))
      (quot (- (l/lt-nano-of-day b) (l/lt-nano-of-day a)) (u/chrono-unit-nanos u))
      (and (= DT ta) (= DT (tof b)))
      (if (#{"YEARS" "MONTHS" "DECADES" "CENTURIES" "MILLENNIA"} u)
        (unit-between u (l/ldt-date a) (l/ldt-date b))
        (quot (- (+ (* (l/ldt-epoch-day b) u/nanos-per-day) (l/ldt-nod b))
                 (+ (* (l/ldt-epoch-day a) u/nanos-per-day) (l/ldt-nod a)))
              (u/chrono-unit-nanos u)))
      :else (if-let [f (get @between-fns ta)] (f u a b)
              (throw (ex-info "between: unsupported temporals" {:a a :b b}))))))

;; --- get / with field --------------------------------------------------------
(declare get-field supports-field?)
(defonce get-field-fns (atom {}))
(defn register-get-field! [type-kw f] (swap! get-field-fns assoc type-kw f))

(defn- date-field [t f]
  (let [ed (l/ld-epoch-day t) [yr m d] (civil-from-days ed)]
    (case f "YEAR" yr "MONTH_OF_YEAR" m "DAY_OF_MONTH" d
         "DAY_OF_WEEK" (l/ld-dow ed) "DAY_OF_YEAR" (l/ld-day-of-year ed) "EPOCH_DAY" ed
         "PROLEPTIC_MONTH" (+ (* yr 12) (dec m))
         "YEAR_OF_ERA" (if (>= yr 1) yr (- 1 yr)) "ERA" (if (>= yr 1) 1 0)
         "ALIGNED_DAY_OF_WEEK_IN_MONTH" (inc (mod (dec d) 7))
         "ALIGNED_WEEK_OF_MONTH" (inc (quot (dec d) 7))
         "ALIGNED_DAY_OF_WEEK_IN_YEAR" (inc (mod (dec (l/ld-day-of-year ed)) 7))
         "ALIGNED_WEEK_OF_YEAR" (inc (quot (dec (l/ld-day-of-year ed)) 7))
         (throw (ex-info (str "LocalDate has no field " f) {})))))

(defn- time-field [nod f]
  (let [h (l/lt-hour nod) mi (l/lt-minute nod) s (l/lt-second nod) na (l/lt-nano nod)]
    (case f "HOUR_OF_DAY" h "MINUTE_OF_HOUR" mi "SECOND_OF_MINUTE" s "NANO_OF_SECOND" na
         "NANO_OF_DAY" nod "MILLI_OF_DAY" (quot nod 1000000) "MICRO_OF_DAY" (quot nod 1000)
         "SECOND_OF_DAY" (quot nod u/nanos-per-sec) "MINUTE_OF_DAY" (quot nod (* 60 u/nanos-per-sec))
         "MILLI_OF_SECOND" (quot na 1000000) "MICRO_OF_SECOND" (quot na 1000)
         "CLOCK_HOUR_OF_DAY" (if (zero? h) 24 h) "HOUR_OF_AMPM" (mod h 12)
         "CLOCK_HOUR_OF_AMPM" (let [x (mod h 12)] (if (zero? x) 12 x)) "AMPM_OF_DAY" (quot h 12)
         (throw (ex-info (str "LocalTime has no field " f) {})))))

(defn get-field [t field]
  (let [f (u/upper field)]
    (condp = (tof t)
      DATE (date-field t f)
      TIME (time-field (l/lt-nano-of-day t) f)
      DT (if (supports-field? (l/ldt-date t) f) (date-field (l/ldt-date t) f) (time-field (l/ldt-nod t) f))
      :jolt.time/year (let [yr (y/year-val t)] (case f "YEAR" yr "YEAR_OF_ERA" (if (>= yr 1) yr (- 1 yr)) "ERA" (if (>= yr 1) 1 0)
                                                     (throw (ex-info (str "Year has no field " f) {}))))
      :jolt.time/year-month (let [yr (y/ym-year t) m (y/ym-month t)]
                              (case f "YEAR" yr "MONTH_OF_YEAR" m "PROLEPTIC_MONTH" (+ (* yr 12) (dec m))
                                   "YEAR_OF_ERA" (if (>= yr 1) yr (- 1 yr)) "ERA" (if (>= yr 1) 1 0)
                                   (throw (ex-info (str "YearMonth has no field " f) {}))))
      (if-let [g (get @get-field-fns (tof t))] (g t f)
        (throw (ex-info "get(field): unsupported temporal" {:t t}))))))

(defn with-field [t field v]
  (let [f (u/upper field)]
    (condp = (tof t)
      DATE (case f "YEAR" (l/ld-with t :year v) "MONTH_OF_YEAR" (l/ld-with t :month v)
                 "DAY_OF_MONTH" (l/ld-with t :day v) "DAY_OF_YEAR" (l/ld-with t :doy v)
                 "EPOCH_DAY" (l/local-date v) (throw (ex-info (str "LocalDate.with " f) {})))
      TIME (case f "HOUR_OF_DAY" (l/lt-with t :hour v) "MINUTE_OF_HOUR" (l/lt-with t :minute v)
                 "SECOND_OF_MINUTE" (l/lt-with t :second v) "NANO_OF_SECOND" (l/lt-with t :nano v)
                 "NANO_OF_DAY" (l/local-time v) (throw (ex-info (str "LocalTime.with " f) {})))
      DT (if (#{"YEAR" "MONTH_OF_YEAR" "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"} f)
           (l/ldt-combine (with-field (l/ldt-date t) f v) (l/ldt-time t))
           (l/ldt-combine (l/ldt-date t) (with-field (l/ldt-time t) f v)))
      (throw (ex-info "with(field): unsupported temporal" {:t t})))))

;; --- isSupported -------------------------------------------------------------
(def ^:private date-fields
  #{"YEAR" "MONTH_OF_YEAR" "DAY_OF_MONTH" "DAY_OF_WEEK" "DAY_OF_YEAR" "EPOCH_DAY"
    "PROLEPTIC_MONTH" "YEAR_OF_ERA" "ERA" "ALIGNED_DAY_OF_WEEK_IN_MONTH"
    "ALIGNED_DAY_OF_WEEK_IN_YEAR" "ALIGNED_WEEK_OF_MONTH" "ALIGNED_WEEK_OF_YEAR"})
(def ^:private time-fields
  #{"HOUR_OF_DAY" "CLOCK_HOUR_OF_DAY" "HOUR_OF_AMPM" "CLOCK_HOUR_OF_AMPM" "AMPM_OF_DAY"
    "MINUTE_OF_HOUR" "MINUTE_OF_DAY" "SECOND_OF_MINUTE" "SECOND_OF_DAY"
    "MILLI_OF_SECOND" "MILLI_OF_DAY" "MICRO_OF_SECOND" "MICRO_OF_DAY" "NANO_OF_SECOND" "NANO_OF_DAY"})
(def ^:private date-units #{"DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES" "CENTURIES" "MILLENNIA" "ERAS"})
(def ^:private time-units #{"NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS"})

(defn supports-field? [t field]
  (let [f (u/upper field)]
    (condp = (tof t)
      DATE (contains? date-fields f)
      TIME (contains? time-fields f)
      DT (or (contains? date-fields f) (contains? time-fields f))
      :jolt.time/year (contains? #{"YEAR" "YEAR_OF_ERA" "ERA"} f)
      :jolt.time/year-month (contains? #{"YEAR" "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "ERA"} f)
      :jolt.time/instant (contains? #{"INSTANT_SECONDS" "NANO_OF_SECOND" "MICRO_OF_SECOND" "MILLI_OF_SECOND"} f)
      false)))
(defn supports-unit? [t unit]
  (let [u (u/upper unit)]
    (condp = (tof t)
      DATE (contains? date-units u) TIME (contains? time-units u)
      DT (not= u "FOREVER") false)))

;; --- range -------------------------------------------------------------------
(defn temporal-range [t field]
  (let [f (u/upper field)]
    (cond
      (and (= DATE (tof t)) (= f "DAY_OF_MONTH"))
      (let [[yr m _] (civil-from-days (l/ld-epoch-day t))] (e/value-range 1 1 28 (u/len-of-month yr m)))
      (= f "MONTH_OF_YEAR") (e/value-range 1 1 12 12)
      (= f "DAY_OF_WEEK") (e/value-range 1 1 7 7)
      (= f "HOUR_OF_DAY") (e/value-range 0 0 23 23)
      (= f "MINUTE_OF_HOUR") (e/value-range 0 0 59 59)
      (= f "SECOND_OF_MINUTE") (e/value-range 0 0 59 59)
      (= f "NANO_OF_SECOND") (e/value-range 0 0 999999999 999999999)
      :else (e/value-range 0 0 999999999999 999999999999))))

;; --- TemporalAdjusters -------------------------------------------------------
(defn- adjuster [proc] (impl/value :jolt.time/temporal-adjuster {:proc proc}))
(defn- adjuster-proc [adj] (impl/field adj :proc))
(defn apply-adjuster [t adj]
  (let [proc (cond (and (impl/jt? adj) (= :jolt.time/temporal-adjuster (impl/type-of adj))) (adjuster-proc adj)
                   (fn? adj) adj
                   :else (throw (ex-info "with: expected a TemporalAdjuster" {})))]
    (condp = (tof t)
      DATE (proc t)
      DT (l/ldt-combine (proc (l/ldt-date t)) (l/ldt-time t))
      (proc t))))

(defn- next-dow [d target same?]
  (let [cur (l/ld-dow (l/ld-epoch-day d)) delta (u/floor-mod (- target cur) 7)
        step (if (and same? (zero? delta)) 0 (if (zero? delta) 7 delta))]
    (l/local-date (+ (l/ld-epoch-day d) step))))
(defn- prev-dow [d target same?]
  (let [cur (l/ld-dow (l/ld-epoch-day d)) delta (u/floor-mod (- cur target) 7)
        step (if (and same? (zero? delta)) 0 (if (zero? delta) 7 delta))]
    (l/local-date (- (l/ld-epoch-day d) step))))
(defn- dow-target [dow]
  (if (and (impl/jt? dow) (= :jolt.time/day-of-week (impl/type-of dow))) (e/dow-val dow) (u/->long dow)))
(defn- first-day-of-month [d] (let [[yr m _] (civil-from-days (l/ld-epoch-day d))] (l/local-date (days-from-civil yr m 1))))
(defn- last-day-of-month [d] (let [[yr m _] (civil-from-days (l/ld-epoch-day d))] (l/local-date (days-from-civil yr m (u/len-of-month yr m)))))
(defn- dow-in-month [d ordinal target]
  (let [[yr m _] (civil-from-days (l/ld-epoch-day d))]
    (if (>= ordinal 0)
      (let [fm (next-dow (l/local-date (days-from-civil yr m 1)) target true)]
        (l/local-date (+ (l/ld-epoch-day fm) (* 7 (dec (max 1 ordinal))))))
      (let [lm (prev-dow (last-day-of-month d) target true)]
        (l/local-date (- (l/ld-epoch-day lm) (* 7 (dec (- ordinal)))))))))

(doseq [n ["TemporalAdjusters" "java.time.temporal.TemporalAdjusters"]]
  (__register-class-statics! n
    {"firstDayOfMonth" (fn [] (adjuster first-day-of-month))
     "lastDayOfMonth" (fn [] (adjuster last-day-of-month))
     "firstDayOfNextMonth" (fn [] (adjuster (fn [d] (l/local-date (inc (l/ld-epoch-day (last-day-of-month d)))))))
     "firstDayOfYear" (fn [] (adjuster (fn [d] (let [[yr _ _] (civil-from-days (l/ld-epoch-day d))] (l/local-date (days-from-civil yr 1 1))))))
     "lastDayOfYear" (fn [] (adjuster (fn [d] (let [[yr _ _] (civil-from-days (l/ld-epoch-day d))] (l/local-date (days-from-civil yr 12 31))))))
     "firstDayOfNextYear" (fn [] (adjuster (fn [d] (let [[yr _ _] (civil-from-days (l/ld-epoch-day d))] (l/local-date (days-from-civil (inc yr) 1 1))))))
     "dayOfWeekInMonth" (fn [ordinal dow] (adjuster (fn [d] (dow-in-month d (u/->long ordinal) (dow-target dow)))))
     "firstInMonth" (fn [dow] (adjuster (fn [d] (dow-in-month d 1 (dow-target dow)))))
     "lastInMonth" (fn [dow] (adjuster (fn [d] (dow-in-month d -1 (dow-target dow)))))
     "next" (fn [dow] (adjuster (fn [d] (next-dow d (dow-target dow) false))))
     "nextOrSame" (fn [dow] (adjuster (fn [d] (next-dow d (dow-target dow) true))))
     "previous" (fn [dow] (adjuster (fn [d] (prev-dow d (dow-target dow) false))))
     "previousOrSame" (fn [dow] (adjuster (fn [d] (prev-dow d (dow-target dow) true))))
     "ofDateAdjuster" (fn [f] (adjuster (fn [d] (f d))))}))

(impl/register-type! :jolt.time/temporal-adjuster
  {:eq (fn [a b] (identical? a b)) :hash (fn [_] 0) :str (fn [_] "TemporalAdjuster") :cmp nil
   :classes #{"java.time.temporal.TemporalAdjuster" "TemporalAdjuster"}})
(__register-class-methods! :jolt.time/temporal-adjuster
  {"adjustInto" (fn [adj t] (apply-adjuster t adj))})

;; --- register the generic methods onto the temporal types --------------------
(def generic-methods
  {"plus" (fn ([t amt] (if (a/dur? amt) (plus-nanos t (a/dur-total-nanos amt)) (plus-period t amt 1)))
             ([t n unit] (plus-unit t (u/->long n) (unit-name unit))))
   "minus" (fn ([t amt] (if (a/dur? amt) (plus-nanos t (- (a/dur-total-nanos amt))) (plus-period t amt -1)))
              ([t n unit] (plus-unit t (- (u/->long n)) (unit-name unit))))
   "until" (fn ([t o] (a/per-between t o))
              ([t o unit] (unit-between (unit-name unit) t o)))
   "get" (fn [t f] (get-field t (field-name f)))
   "getLong" (fn [t f] (get-field t (field-name f)))
   "with" (fn ([t adj] (apply-adjuster t adj))
             ([t f v] (with-field t (field-name f) (u/->long v))))
   "isSupported" (fn [t x] (cond (and (impl/jt? x) (= :jolt.time/chrono-unit (impl/type-of x))) (supports-unit? t (e/cu-name x))
                                 (and (impl/jt? x) (= :jolt.time/chrono-field (impl/type-of x))) (supports-field? t (e/cf-name x))
                                 :else false))
   "range" (fn [t f] (temporal-range t (field-name f)))})

(def ^:private field-only
  (select-keys generic-methods ["get" "getLong" "isSupported"]))

(doseq [tk [:jolt.time/local-date :jolt.time/local-time :jolt.time/local-date-time]]
  (__register-class-methods! tk generic-methods))
(doseq [tk [:jolt.time/year :jolt.time/year-month]]
  (__register-class-methods! tk field-only))

;; --- wire the field/unit/amount types into the machinery ---------------------
(__register-class-methods! :jolt.time/chrono-unit
  {"between" (fn [u a b] (unit-between (e/cu-name u) a b))
   "addTo"   (fn [u t n] (plus-unit t (u/->long n) (e/cu-name u)))})
(__register-class-methods! :jolt.time/chrono-field
  {"getFrom" (fn [f t] (get-field t (e/cf-name f)))})
(__register-class-methods! :jolt.time/duration
  {"addTo" (fn [d t] (plus-nanos t (a/dur-total-nanos d)))
   "subtractFrom" (fn [d t] (plus-nanos t (- (a/dur-total-nanos d))))})
(__register-class-methods! :jolt.time/period
  {"addTo" (fn [p t] (plus-period t p 1))
   "subtractFrom" (fn [p t] (plus-period t p -1))})

;; TemporalAmount.getUnits/get (tick's t/units iterates these)
(__register-class-methods! :jolt.time/duration
  {"getUnits" (fn [_] [(e/chrono-unit "SECONDS") (e/chrono-unit "NANOS")])
   "get" (fn [d u] (case (e/cu-name u) "SECONDS" (a/dur-secs d) "NANOS" (a/dur-nanos d)
                        (throw (ex-info (str "Duration has no unit " (e/cu-name u)) {}))))})
(__register-class-methods! :jolt.time/period
  {"getUnits" (fn [_] [(e/chrono-unit "YEARS") (e/chrono-unit "MONTHS") (e/chrono-unit "DAYS")])
   "get" (fn [p u] (case (e/cu-name u) "YEARS" (a/per-years p) "MONTHS" (a/per-months p) "DAYS" (a/per-days p)
                        (throw (ex-info (str "Period has no unit " (e/cu-name u)) {}))))})
