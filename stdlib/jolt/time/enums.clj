(ns jolt.time.enums
  "java.time enum + field types: Month, DayOfWeek, ChronoUnit, ChronoField, and
  ValueRange. Requiring the namespace registers the classes. The temporal-coupled
  operations (Month/from, ChronoUnit.between, ChronoField.getFrom, …) are added by
  the temporal namespaces, which own the receiver types."
  (:require [jolt.time.impl :as impl]
            [jolt.time.util :as u]))

(defn- statics! [names members]
  (doseq [n names] (__register-class-statics! n members)))

;; ---- Month ------------------------------------------------------------------

(defn month [n] (impl/value :jolt.time/month {:value n}))
(defn month-val [e] (impl/field e :value))
(defn- month-name [e] (nth u/month-names (dec (month-val e))))

(impl/register-type! :jolt.time/month
  {:eq   (fn [a b] (= (month-val a) (month-val b)))
   :hash (fn [e] (month-val e))
   :str  month-name
   :cmp  (fn [a b] (compare (month-val a) (month-val b)))
   :classes #{"java.time.Month" "Month"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(__register-class-methods! :jolt.time/month
  {"getValue"  (fn [e] (month-val e))
   "ordinal"   (fn [e] (dec (month-val e)))
   "name"      month-name
   "toString"  month-name
   "getDisplayName" (fn [e & _] (month-name e))
   "plus"      (fn [e n] (month (inc (u/floor-mod (+ (dec (month-val e)) (u/->long n)) 12))))
   "minus"     (fn [e n] (month (inc (u/floor-mod (- (dec (month-val e)) (u/->long n)) 12))))
   "length"    (fn [e leap] (u/len-of-month (if leap 4 1) (month-val e)))
   "minLength" (fn [e] (if (= 2 (month-val e)) 28 (u/len-of-month 1 (month-val e))))
   "maxLength" (fn [e] (if (= 2 (month-val e)) 29 (u/len-of-month 1 (month-val e))))
   "firstMonthOfQuarter" (fn [e] (month (inc (* 3 (quot (dec (month-val e)) 3)))))
   "compareTo" (fn [e o] (- (month-val e) (month-val o)))
   "equals"    (fn [e o] (boolean (and (impl/jt? o) (= :jolt.time/month (impl/type-of o))
                                        (= (month-val e) (month-val o)))))
   "hashCode"  (fn [e] (month-val e))})

(defn- month-valueOf [s]
  (or (some (fn [i] (when (= (nth u/month-names i) s) (month (inc i)))) (range 12))
      (throw (ex-info (str "No enum constant Month." s) {}))))

(statics! ["Month" "java.time.Month"]
  (merge {"of"      (fn [n] (month (u/->long n)))
          "valueOf" month-valueOf
          "values"  (fn [] (mapv month (range 1 13)))}
         (zipmap u/month-names (map month (range 1 13)))))

;; ---- DayOfWeek --------------------------------------------------------------

(defn dow [n] (impl/value :jolt.time/day-of-week {:value n}))
(defn dow-val [e] (impl/field e :value))
(defn- dow-name [e] (nth u/day-names (dec (dow-val e))))

(impl/register-type! :jolt.time/day-of-week
  {:eq   (fn [a b] (= (dow-val a) (dow-val b)))
   :hash (fn [e] (dow-val e))
   :str  dow-name
   :cmp  (fn [a b] (compare (dow-val a) (dow-val b)))
   :classes #{"java.time.DayOfWeek" "DayOfWeek"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(__register-class-methods! :jolt.time/day-of-week
  {"getValue"  (fn [e] (dow-val e))
   "ordinal"   (fn [e] (dec (dow-val e)))
   "name"      dow-name
   "toString"  dow-name
   "getDisplayName" (fn [e & _] (dow-name e))
   "plus"      (fn [e n] (dow (inc (u/floor-mod (+ (dec (dow-val e)) (u/->long n)) 7))))
   "minus"     (fn [e n] (dow (inc (u/floor-mod (- (dec (dow-val e)) (u/->long n)) 7))))
   "compareTo" (fn [e o] (- (dow-val e) (dow-val o)))
   "equals"    (fn [e o] (boolean (and (impl/jt? o) (= :jolt.time/day-of-week (impl/type-of o))
                                        (= (dow-val e) (dow-val o)))))
   "hashCode"  (fn [e] (dow-val e))})

(defn- dow-valueOf [s]
  (or (some (fn [i] (when (= (nth u/day-names i) s) (dow (inc i)))) (range 7))
      (throw (ex-info (str "No enum constant DayOfWeek." s) {}))))

(statics! ["DayOfWeek" "java.time.DayOfWeek"]
  (merge {"of"      (fn [n] (dow (u/->long n)))
          "valueOf" dow-valueOf
          "values"  (fn [] (mapv dow (range 1 8)))}
         (zipmap u/day-names (map dow (range 1 8)))))

;; ---- ChronoUnit -------------------------------------------------------------

(defn chrono-unit [name] (impl/value :jolt.time/chrono-unit {:name name}))
(defn cu-name [u] (impl/field u :name))

(impl/register-type! :jolt.time/chrono-unit
  {:eq   (fn [a b] (= (cu-name a) (cu-name b)))
   :hash (fn [u] (hash (cu-name u)))
   :str  cu-name
   :cmp  (fn [a b] (compare (u/chrono-unit-index (cu-name a))
                            (u/chrono-unit-index (cu-name b))))
   :classes #{"java.time.temporal.ChronoUnit" "ChronoUnit"
              "java.time.temporal.TemporalUnit" "TemporalUnit"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(def ^:private time-based-units
  #{"NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS"})

(__register-class-methods! :jolt.time/chrono-unit
  {"name"       cu-name
   "toString"   cu-name
   "ordinal"    (fn [u] (u/chrono-unit-index (cu-name u)))
   "isTimeBased" (fn [u] (contains? time-based-units (cu-name u)))
   "isDateBased" (fn [u] (contains? #{"DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES"
                                      "CENTURIES" "MILLENNIA" "ERAS"} (cu-name u)))
   "isDurationEstimated" (fn [u] (not (contains? time-based-units (cu-name u))))
   "equals"     (fn [u o] (boolean (and (impl/jt? o) (= :jolt.time/chrono-unit (impl/type-of o))
                                         (= (cu-name u) (cu-name o)))))
   "hashCode"   (fn [u] (hash (cu-name u)))})

(statics! ["ChronoUnit" "java.time.temporal.ChronoUnit"]
  (merge {"valueOf" (fn [s] (chrono-unit s))
          "values"  (fn [] (mapv chrono-unit u/chrono-units))}
         (zipmap u/chrono-units (map chrono-unit u/chrono-units))))

;; ---- ChronoField ------------------------------------------------------------

(defn chrono-field [name] (impl/value :jolt.time/chrono-field {:name name}))
(defn cf-name [f] (impl/field f :name))

(impl/register-type! :jolt.time/chrono-field
  {:eq   (fn [a b] (= (cf-name a) (cf-name b)))
   :hash (fn [f] (hash (cf-name f)))
   :str  cf-name
   :cmp  (fn [a b] (compare (u/chrono-field-index (cf-name a))
                            (u/chrono-field-index (cf-name b))))
   :classes #{"java.time.temporal.ChronoField" "ChronoField"
              "java.time.temporal.TemporalField" "TemporalField"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})

(__register-class-methods! :jolt.time/chrono-field
  {"name"      cf-name
   "toString"  cf-name
   "getDisplayName" (fn [f & _] (cf-name f))
   "isDateBased" (fn [f] (contains? u/date-based-fields (cf-name f)))
   "isTimeBased" (fn [f] (not (or (contains? u/date-based-fields (cf-name f))
                                  (contains? #{"INSTANT_SECONDS" "OFFSET_SECONDS"} (cf-name f)))))
   "equals"    (fn [f o] (boolean (and (impl/jt? o) (= :jolt.time/chrono-field (impl/type-of o))
                                        (= (cf-name f) (cf-name o)))))
   "hashCode"  (fn [f] (hash (cf-name f)))})

(statics! ["ChronoField" "java.time.temporal.ChronoField"]
  (merge {"valueOf" (fn [s] (chrono-field s))
          "values"  (fn [] (mapv chrono-field u/chrono-fields))}
         (zipmap u/chrono-fields (map chrono-field u/chrono-fields))))

;; ---- ValueRange -------------------------------------------------------------

(defn value-range [smin lmin smax lmax]
  (impl/value :jolt.time/value-range {:smin smin :lmin lmin :smax smax :lmax lmax}))

(impl/register-type! :jolt.time/value-range
  {:eq   (fn [a b] (and (= (impl/field a :lmin) (impl/field b :lmin))
                        (= (impl/field a :lmax) (impl/field b :lmax))))
   :hash (fn [r] (hash [(impl/field r :lmin) (impl/field r :lmax)]))
   :str  (fn [r] (str (impl/field r :lmin) " - " (impl/field r :lmax)))
   :cmp  nil
   :classes #{"java.time.temporal.ValueRange" "ValueRange" "java.io.Serializable" "Serializable"}})

(__register-class-methods! :jolt.time/value-range
  {"getMinimum" (fn [r] (impl/field r :lmin))
   "getLargestMinimum" (fn [r] (impl/field r :lmin))
   "getSmallestMaximum" (fn [r] (impl/field r :smax))
   "getMaximum" (fn [r] (impl/field r :lmax))
   "isFixed" (fn [r] (and (= (impl/field r :smin) (impl/field r :lmin))
                          (= (impl/field r :smax) (impl/field r :lmax))))
   "isValidValue" (fn [r v] (let [x (u/->long v)] (and (>= x (impl/field r :lmin)) (<= x (impl/field r :lmax)))))
   "toString" (fn [r] (str (impl/field r :lmin) " - " (impl/field r :lmax)))})

(statics! ["ValueRange" "java.time.temporal.ValueRange"]
  {"of" (fn ([mn mx] (value-range (u/->long mn) (u/->long mn) (u/->long mx) (u/->long mx)))
            ([mn smx lmx] (value-range (u/->long mn) (u/->long mn) (u/->long smx) (u/->long lmx)))
            ([smn lmn smx lmx] (value-range (u/->long smn) (u/->long lmn) (u/->long smx) (u/->long lmx))))})
