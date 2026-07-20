(ns jolt.time.util
  "Pure helpers shared across the java.time value types: calendar math, name
  tables, and the fixed unit-duration table.")

(def nanos-per-sec 1000000000)
(def nanos-per-day 86400000000000)
;; java.time's average year length (365.2425 days) for the estimated durations.
(def seconds-per-year 31556952)

(def month-names
  ["JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE"
   "JULY" "AUGUST" "SEPTEMBER" "OCTOBER" "NOVEMBER" "DECEMBER"])
(def day-names
  ["MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY" "SUNDAY"])

(defn upper [s]
  (apply str (map (fn [c] (if (and (>= (int c) 97) (<= (int c) 122)) (char (- (int c) 32)) c)) s)))

(defn leap? [y]
  (and (zero? (mod y 4)) (or (not (zero? (mod y 100))) (zero? (mod y 400)))))

(defn floor-div [a b]
  (let [q (quot a b) r (rem a b)]
    (if (and (not (zero? r)) (neg? (* a b))) (dec q) q)))

(defn floor-mod [a b] (- a (* (floor-div a b) b)))

;; java.time coerces its numeric args toward zero (truncating); a value already
;; an exact integer is left as-is so large counts keep arbitrary precision.
(defn ->long [n] (if (integer? n) n (long n)))

(defn len-of-month [y m]
  (cond (= m 2) (if (leap? y) 29 28)
        (#{4 6 9 11} m) 30
        :else 31))

;; unit -> fixed nanosecond length; FOREVER has none. month/year and up use the
;; average-year estimate, exactly as java.time's ChronoUnit does.
(def chrono-unit-nanos
  (let [s seconds-per-year n nanos-per-sec]
    {"NANOS" 1 "MICROS" 1000 "MILLIS" 1000000
     "SECONDS" n "MINUTES" (* 60 n) "HOURS" (* 3600 n) "HALF_DAYS" (* 43200 n)
     "DAYS" (* 86400 n) "WEEKS" (* 7 86400 n)
     "MONTHS" (long (Math/round (double (* (/ s 12) n))))
     "YEARS" (* s n)
     "DECADES" (* 10 s n) "CENTURIES" (* 100 s n) "MILLENNIA" (* 1000 s n)
     "ERAS" (* 1000000000 s n) "FOREVER" nil}))

;; ChronoUnit order (for ordinal / values).
(def chrono-units
  ["NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS"
   "DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES" "CENTURIES" "MILLENNIA" "ERAS" "FOREVER"])

(def chrono-unit-index (zipmap chrono-units (range)))

(def chrono-fields
  ["NANO_OF_SECOND" "NANO_OF_DAY" "MICRO_OF_SECOND" "MICRO_OF_DAY" "MILLI_OF_SECOND" "MILLI_OF_DAY"
   "SECOND_OF_MINUTE" "SECOND_OF_DAY" "MINUTE_OF_HOUR" "MINUTE_OF_DAY"
   "HOUR_OF_AMPM" "CLOCK_HOUR_OF_AMPM" "HOUR_OF_DAY" "CLOCK_HOUR_OF_DAY" "AMPM_OF_DAY"
   "DAY_OF_WEEK" "ALIGNED_DAY_OF_WEEK_IN_MONTH" "ALIGNED_DAY_OF_WEEK_IN_YEAR"
   "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"
   "ALIGNED_WEEK_OF_MONTH" "ALIGNED_WEEK_OF_YEAR"
   "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA"
   "INSTANT_SECONDS" "OFFSET_SECONDS"])

(def chrono-field-index (zipmap chrono-fields (range)))

(def date-based-fields
  #{"DAY_OF_WEEK" "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"
    "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA"})

;; --- nano-of-day <-> h/m/s/nano ----------------------------------------------
(defn hmsn->nano [h m s nano] (+ (* (+ (* h 3600) (* m 60) s) nanos-per-sec) nano))

;; --- integer power of ten (no float rounding) --------------------------------
(defn pow10 [n] (reduce * 1 (repeat n 10)))

;; --- zero-padding + ISO rendering --------------------------------------------
(defn pad-left [s n] (str (apply str (repeat (max 0 (- n (count s))) "0")) s))
(defn pad2 [n] (pad-left (str n) 2))
(defn pad4
  "java.time's proleptic-year rendering: 4 digits for 0..9999, a sign otherwise."
  [y]
  (cond (neg? y)   (str "-" (pad-left (str (- y)) 4))
        (> y 9999) (str "+" y)
        :else      (pad-left (str y) 4)))

;; nano fraction as 3/6/9 digits (LocalTime style: shortest group that fits).
(defn frac-digits [nano]
  (cond (zero? (mod nano 1000000)) (pad-left (str (quot nano 1000000)) 3)
        (zero? (mod nano 1000))    (pad-left (str (quot nano 1000)) 6)
        :else                      (pad-left (str nano) 9)))

;; leading `digits` of nano's 9-digit zero-padded form (fixed-width fraction).
(defn frac-fixed [nano digits] (subs (pad-left (str nano) 9) 0 digits))

(defn iso-time-str [nod]
  (let [h    (quot nod (* 3600 nanos-per-sec))
        mi   (mod (quot nod (* 60 nanos-per-sec)) 60)
        s    (mod (quot nod nanos-per-sec) 60)
        nano (mod nod nanos-per-sec)]
    (str (pad2 h) ":" (pad2 mi)
         (if (and (zero? s) (zero? nano)) ""
             (str ":" (pad2 s) (if (zero? nano) "" (str "." (frac-digits nano))))))))

;; --- ISO parsing -------------------------------------------------------------
(defn- digit? [c] (and (>= (int c) 48) (<= (int c) 57)))

(defn digits-at
  "n digits from index i as an integer, or nil."
  [s i n]
  (when (<= (+ i n) (count s))
    (loop [j i acc 0]
      (if (= j (+ i n)) acc
          (let [c (nth s j)]
            (when (digit? c) (recur (inc j) (+ (* acc 10) (- (int c) 48)))))))))

(defn parse-hms->nano
  "Parse HH:mm[:ss[.fraction]] to nano-of-day."
  [s]
  (let [len (count s) h (digits-at s 0 2) mi (digits-at s 3 2)]
    (when-not (and h mi (= \: (nth s 2)))
      (throw (ex-info (str "could not parse LocalTime: " s) {})))
    (let [s2 (and (> len 5) (= \: (nth s 5)) (digits-at s 6 2))]
      (cond
        (not s2) (hmsn->nano h mi 0 0)
        (and (< 8 len) (= \. (nth s 8)))
        (loop [j 9 k 0 acc 0]
          (if (and (< j len) (digit? (nth s j)))
            (recur (inc j) (inc k) (+ (* acc 10) (- (int (nth s j)) 48)))
            (hmsn->nano h mi s2 (* acc (pow10 (max 0 (- 9 k)))))))
        :else (hmsn->nano h mi s2 0)))))
