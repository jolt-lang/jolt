(ns jolt.time.amount
  "Duration (seconds + nanos) and Period (years/months/days). The TemporalAmount
  operations that apply an amount to a temporal (addTo/subtractFrom, get, getUnits)
  are added with the generic temporal machinery."
  (:require [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]))

(defn- statics! [names members]
  (doseq [n names] (__register-class-statics! n members)))

(def ^:private nps u/nanos-per-sec)

;; --- Duration ----------------------------------------------------------------

(defn- duration [secs nanos] (impl/value :jolt.time/duration {:secs secs :nanos nanos}))
(defn dur-secs [d] (impl/field d :secs))
(defn dur-nanos [d] (impl/field d :nanos))
(defn dur? [d] (= :jolt.time/duration (impl/type-of d)))

;; normalize so nanos are in [0, 1e9); a carry rolls into seconds.
(defn- normalize [secs nanos]
  (duration (+ secs (u/floor-div nanos nps)) (u/floor-mod nanos nps)))
(defn dur-total-nanos [d] (+ (* (dur-secs d) nps) (dur-nanos d)))
(defn- of-total-nanos [tn] (normalize (u/floor-div tn nps) (u/floor-mod tn nps)))

;; nanos of a temporal on its own timeline (local layer: date=day, time=nano-of-day).
;; instant/zoned/offset register their own nanos via register-nanos! (they load later).
(defonce nanos-fns (atom {}))
(defn register-nanos! [type-kw f] (swap! nanos-fns assoc type-kw f))
(defn temporal-nanos [t]
  (condp = (impl/type-of t)
    :jolt.time/local-date      (* (l/ld-epoch-day t) u/nanos-per-day)
    :jolt.time/local-date-time (+ (* (l/ldt-epoch-day t) u/nanos-per-day) (l/ldt-nod t))
    :jolt.time/local-time      (l/lt-nano-of-day t)
    (if-let [f (get @nanos-fns (impl/type-of t))] (f t)
      (throw (ex-info "Duration/between: unsupported temporal" {:t t})))))

(defn- dur-frac-digits [nano]
  (let [s9 (u/pad-left (str nano) 9)]
    (loop [i 9] (cond (<= i 1) (subs s9 0 1)
                      (= \0 (nth s9 (dec i))) (recur (dec i))
                      :else (subs s9 0 i)))))

(defn- dur->string [secs nanos]
  (if (and (zero? secs) (zero? nanos)) "PT0S"
    (let [hours (quot secs 3600) mins (quot (rem secs 3600) 60) rem-secs (rem secs 60)]
      (str "PT"
           (when-not (zero? hours) (str hours "H"))
           (when-not (zero? mins) (str mins "M"))
           (when (or (not (zero? rem-secs)) (not (zero? nanos)) (and (zero? hours) (zero? mins)))
             (str (if (zero? nanos)
                    (str rem-secs)
                    (let [neg (neg? rem-secs) whole (if neg (inc rem-secs) rem-secs)
                          frac (if neg (- nps nanos) nanos)]
                      (str (when (and neg (zero? whole)) "-") whole "." (dur-frac-digits frac))))
                  "S"))))))

(defn- unit-nanos [unit]
  (let [nm (cond (and (impl/jt? unit) (= :jolt.time/chrono-unit (impl/type-of unit))) (e/cu-name unit)
                 (string? unit) unit :else (str unit))]
    (or (u/chrono-unit-nanos nm) (throw (ex-info (str "no fixed duration for unit " nm) {})))))

(defn- digit? [c] (and (>= (int c) 48) (<= (int c) 57)))
(defn- dval [c] (- (int c) 48))
(defn- up-char [c] (if (and (>= (int c) 97) (<= (int c) 122)) (char (- (int c) 32)) c))

;; read an integer[.fraction] from index i; -> [int-part frac-part frac-digits next-i]
(defn- read-num [s i len]
  (loop [k i acc 0 frac 0 fdig 0 in-frac false]
    (cond
      (and (< k len) (digit? (nth s k)))
      (if in-frac (recur (inc k) acc (+ (* frac 10) (dval (nth s k))) (inc fdig) true)
                  (recur (inc k) (+ (* acc 10) (dval (nth s k))) frac fdig false))
      (and (< k len) (= \. (nth s k))) (recur (inc k) acc frac fdig true)
      :else [acc frac fdig k])))

(defn- parse-iso-duration [s]
  (let [len (count s)
        neg (and (pos? len) (= \- (nth s 0)))
        i0 (if neg 1 0)]
    (when-not (and (< i0 len) (#{\P \p} (nth s i0)))
      (throw (ex-info (str "could not parse Duration: " s) {})))
    (loop [i (inc i0) in-time false total 0]
      (if (>= i len)
        (of-total-nanos (if neg (- total) total))
        (let [c (nth s i)]
          (if (#{\T \t} c)
            (recur (inc i) true total)
            (let [[sg i] (cond (= \- c) [-1 (inc i)] (= \+ c) [1 (inc i)] :else [1 i])
                  [acc frac fdig k] (read-num s i len)
                  _ (when (>= k len) (throw (ex-info (str "could not parse Duration: " s) {})))
                  unit (up-char (nth s k))
                  nanos (* (+ (* acc nps) (* frac (u/pow10 (max 0 (- 9 fdig))))) sg)
                  mult (case unit \D 86400 \H 3600
                             \M (if in-time 60 (throw (ex-info "Duration months unsupported" {})))
                             \S 1 (throw (ex-info (str "bad Duration unit: " s) {})))]
              (recur (inc k) in-time (+ total (* nanos mult))))))))))

(impl/register-type! :jolt.time/duration
  {:eq   (fn [a b] (= (dur-total-nanos a) (dur-total-nanos b)))
   :hash (fn [d] (hash (dur-total-nanos d)))
   :str  (fn [d] (dur->string (dur-secs d) (dur-nanos d)))
   :cmp  (fn [a b] (compare (dur-total-nanos a) (dur-total-nanos b)))
   :classes #{"java.time.Duration" "Duration"
              "java.time.temporal.TemporalAmount" "TemporalAmount"
              "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/duration
  {"getSeconds" dur-secs "getNano" dur-nanos
   "toDays" (fn [d] (quot (dur-secs d) 86400))
   "toHours" (fn [d] (quot (dur-secs d) 3600))
   "toMinutes" (fn [d] (quot (dur-secs d) 60))
   "toMillis" (fn [d] (quot (dur-total-nanos d) 1000000))
   "toNanos" dur-total-nanos
   "plus" (fn [d o] (of-total-nanos (+ (dur-total-nanos d) (dur-total-nanos o))))
   "minus" (fn [d o] (of-total-nanos (- (dur-total-nanos d) (dur-total-nanos o))))
   "plusDays" (fn [d n] (of-total-nanos (+ (dur-total-nanos d) (* (u/->long n) 86400 nps))))
   "plusHours" (fn [d n] (of-total-nanos (+ (dur-total-nanos d) (* (u/->long n) 3600 nps))))
   "plusMinutes" (fn [d n] (of-total-nanos (+ (dur-total-nanos d) (* (u/->long n) 60 nps))))
   "plusSeconds" (fn [d n] (of-total-nanos (+ (dur-total-nanos d) (* (u/->long n) nps))))
   "plusMillis" (fn [d n] (of-total-nanos (+ (dur-total-nanos d) (* (u/->long n) 1000000))))
   "plusNanos" (fn [d n] (of-total-nanos (+ (dur-total-nanos d) (u/->long n))))
   "minusDays" (fn [d n] (of-total-nanos (- (dur-total-nanos d) (* (u/->long n) 86400 nps))))
   "minusHours" (fn [d n] (of-total-nanos (- (dur-total-nanos d) (* (u/->long n) 3600 nps))))
   "minusMinutes" (fn [d n] (of-total-nanos (- (dur-total-nanos d) (* (u/->long n) 60 nps))))
   "minusSeconds" (fn [d n] (of-total-nanos (- (dur-total-nanos d) (* (u/->long n) nps))))
   "minusMillis" (fn [d n] (of-total-nanos (- (dur-total-nanos d) (* (u/->long n) 1000000))))
   "minusNanos" (fn [d n] (of-total-nanos (- (dur-total-nanos d) (u/->long n))))
   "multipliedBy" (fn [d n] (of-total-nanos (* (dur-total-nanos d) (u/->long n))))
   "dividedBy" (fn [d n] (of-total-nanos (quot (dur-total-nanos d) (u/->long n))))
   "negated" (fn [d] (of-total-nanos (- (dur-total-nanos d))))
   "abs" (fn [d] (of-total-nanos (abs (dur-total-nanos d))))
   "withSeconds" (fn [d s] (normalize (u/->long s) (dur-nanos d)))
   "withNanos" (fn [d na] (normalize (dur-secs d) (u/->long na)))
   "isZero" (fn [d] (and (zero? (dur-secs d)) (zero? (dur-nanos d))))
   "isNegative" (fn [d] (neg? (dur-total-nanos d)))
   "compareTo" (fn [d o] (compare (dur-total-nanos d) (dur-total-nanos o)))
   "equals" (fn [d o] (boolean (and (impl/jt? o) (dur? o) (= (dur-total-nanos d) (dur-total-nanos o)))))
   "hashCode" (fn [d] (hash (dur-total-nanos d)))
   "toString" (fn [d] (dur->string (dur-secs d) (dur-nanos d)))})

(statics! ["Duration" "java.time.Duration"]
  {"ZERO" (duration 0 0)
   "of" (fn [n unit] (of-total-nanos (* (u/->long n) (unit-nanos unit))))
   "ofDays" (fn [n] (normalize (* (u/->long n) 86400) 0))
   "ofHours" (fn [n] (normalize (* (u/->long n) 3600) 0))
   "ofMinutes" (fn [n] (normalize (* (u/->long n) 60) 0))
   "ofSeconds" (fn ([s] (normalize (u/->long s) 0)) ([s na] (normalize (u/->long s) (u/->long na))))
   "ofMillis" (fn [n] (of-total-nanos (* (u/->long n) 1000000)))
   "ofNanos" (fn [n] (of-total-nanos (u/->long n)))
   "between" (fn [a b] (of-total-nanos (- (temporal-nanos b) (temporal-nanos a))))
   "parse" (fn [s] (parse-iso-duration (str s)))})

;; --- Period ------------------------------------------------------------------

(defn- period [y m d] (impl/value :jolt.time/period {:years y :months m :days d}))
(defn per-years [p] (impl/field p :years))
(defn per-months [p] (impl/field p :months))
(defn per-days [p] (impl/field p :days))
(defn per? [p] (= :jolt.time/period (impl/type-of p)))

(defn- per->string [p]
  (let [y (per-years p) m (per-months p) d (per-days p)]
    (if (and (zero? y) (zero? m) (zero? d)) "P0D"
      (str "P" (when-not (zero? y) (str y "Y")) (when-not (zero? m) (str m "M"))
           (when-not (zero? d) (str d "D"))))))

(defn per-between [a b]
  (let [[y1 m1 d1] (civil-from-days (l/ld-epoch-day a))
        [y2 m2 d2] (civil-from-days (l/ld-epoch-day b))
        total-months (- (+ (* y2 12) m2) (+ (* y1 12) m1))
        days (- d2 d1)
        [tm dd] (cond
                  (and (pos? total-months) (neg? days))
                  (let [tm (dec total-months) bm (+ (* y1 12) (dec m1) tm)
                        by (u/floor-div bm 12) bmo (inc (u/floor-mod bm 12))]
                    [tm (+ days (u/len-of-month by bmo))])
                  (and (neg? total-months) (pos? days))
                  [(inc total-months) (- days (u/len-of-month y2 m2))]
                  :else [total-months days])]
    (period (u/floor-div tm 12) (u/floor-mod tm 12) dd)))

(defn- parse-iso-period [s]
  (let [len (count s) sign (if (and (pos? len) (= \- (nth s 0))) -1 1)
        start (if (= -1 sign) 1 0)]
    (when-not (and (< start len) (#{\P \p} (nth s start)))
      (throw (ex-info (str "could not parse Period: " s) {})))
    (loop [i (inc start) y 0 m 0 d 0]
      (if (>= i len)
        (period (* sign y) (* sign m) (* sign d))
        (let [[vsign i] (cond (= \- (nth s i)) [-1 (inc i)] (= \+ (nth s i)) [1 (inc i)] :else [1 i])
              [acc _ _ k] (read-num s i len)
              _ (when (>= k len) (throw (ex-info (str "could not parse Period: " s) {})))
              uc (up-char (nth s k)) val (* vsign acc)]
          (case uc
            \Y (recur (inc k) val m d)
            \M (recur (inc k) y val d)
            \W (recur (inc k) y m (+ d (* 7 val)))
            \D (recur (inc k) y m (+ d val))
            (throw (ex-info (str "bad Period unit: " s) {}))))))))

(impl/register-type! :jolt.time/period
  {:eq   (fn [a b] (and (= (per-years a) (per-years b)) (= (per-months a) (per-months b)) (= (per-days a) (per-days b))))
   :hash (fn [p] (+ (per-years p) (bit-shift-left (per-months p) 8) (bit-shift-left (per-days p) 16)))
   :str  per->string
   :cmp  nil
   :classes #{"java.time.Period" "Period"
              "java.time.chrono.ChronoPeriod" "ChronoPeriod"
              "java.time.temporal.TemporalAmount" "TemporalAmount"}})

(__register-class-methods! :jolt.time/period
  {"getYears" per-years "getMonths" per-months "getDays" per-days
   "toTotalMonths" (fn [p] (+ (* (per-years p) 12) (per-months p)))
   "plusYears" (fn [p n] (period (+ (per-years p) (u/->long n)) (per-months p) (per-days p)))
   "plusMonths" (fn [p n] (period (per-years p) (+ (per-months p) (u/->long n)) (per-days p)))
   "plusDays" (fn [p n] (period (per-years p) (per-months p) (+ (per-days p) (u/->long n))))
   "minusYears" (fn [p n] (period (- (per-years p) (u/->long n)) (per-months p) (per-days p)))
   "minusMonths" (fn [p n] (period (per-years p) (- (per-months p) (u/->long n)) (per-days p)))
   "minusDays" (fn [p n] (period (per-years p) (per-months p) (- (per-days p) (u/->long n))))
   "withYears" (fn [p n] (period (u/->long n) (per-months p) (per-days p)))
   "withMonths" (fn [p n] (period (per-years p) (u/->long n) (per-days p)))
   "withDays" (fn [p n] (period (per-years p) (per-months p) (u/->long n)))
   "plus" (fn [p o] (period (+ (per-years p) (per-years o)) (+ (per-months p) (per-months o)) (+ (per-days p) (per-days o))))
   "minus" (fn [p o] (period (- (per-years p) (per-years o)) (- (per-months p) (per-months o)) (- (per-days p) (per-days o))))
   "multipliedBy" (fn [p n] (period (* (per-years p) (u/->long n)) (* (per-months p) (u/->long n)) (* (per-days p) (u/->long n))))
   "negated" (fn [p] (period (- (per-years p)) (- (per-months p)) (- (per-days p))))
   "normalized" (fn [p] (let [tm (+ (* (per-years p) 12) (per-months p))]
                          (period (u/floor-div tm 12) (u/floor-mod tm 12) (per-days p))))
   "isZero" (fn [p] (and (zero? (per-years p)) (zero? (per-months p)) (zero? (per-days p))))
   "isNegative" (fn [p] (or (neg? (per-years p)) (neg? (per-months p)) (neg? (per-days p))))
   "equals" (fn [p o] (boolean (and (impl/jt? o) (per? o) (= (per-years p) (per-years o))
                                     (= (per-months p) (per-months o)) (= (per-days p) (per-days o)))))
   "hashCode" (fn [p] (+ (per-years p) (bit-shift-left (per-months p) 8) (bit-shift-left (per-days p) 16)))
   "toString" per->string})

(statics! ["Period" "java.time.Period"]
  {"ZERO" (period 0 0 0)
   "of" (fn [y m d] (period (u/->long y) (u/->long m) (u/->long d)))
   "ofYears" (fn [y] (period (u/->long y) 0 0))
   "ofMonths" (fn [m] (period 0 (u/->long m) 0))
   "ofWeeks" (fn [w] (period 0 0 (* 7 (u/->long w))))
   "ofDays" (fn [d] (period 0 0 (u/->long d)))
   "between" (fn [a b] (per-between a b))
   "parse" (fn [s] (parse-iso-period (str s)))})
