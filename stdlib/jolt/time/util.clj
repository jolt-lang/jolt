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

;; ChronoField.checkValidValue: a value outside the field's range is a
;; DateTimeException naming the field and the range, as the JDK words it.
(defn- date-time-exception [msg]
  (jolt.host/throwable "java.time.DateTimeException" msg))
(defn check-field [field lo hi v]
  (when-not (and (<= lo v) (<= v hi))
    (throw (date-time-exception (str "Invalid value for " field " (valid values " lo " - " hi "): " v))))
  v)

;; LocalDate.of's checks: the year, the month and the day each against its
;; field, then the day against the month (LocalDate.create).
(defn check-date [y m d]
  (check-field "Year" -999999999 999999999 y)
  (check-field "MonthOfYear" 1 12 m)
  (when-not (and (<= 1 d) (<= d 31))
    (throw (date-time-exception (str "Invalid value for DayOfMonth (valid values 1 - 28/31): " d))))
  (when (and (> d 28) (> d (len-of-month y m)))
    (throw (date-time-exception
            (if (= d 29)
              (str "Invalid date 'February 29' as '" y "' is not a leap year")
              (str "Invalid date '" (nth month-names (dec m)) " " d "'"))))))

;; LocalTime.of's checks, field by field.
(defn check-time [h m s n]
  (check-field "HourOfDay" 0 23 h)
  (check-field "MinuteOfHour" 0 59 m)
  (check-field "SecondOfMinute" 0 59 s)
  (check-field "NanoOfSecond" 0 999999999 n))

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

;; The ISO formatters' parsing, as the JDK does it: every scanner reads from
;; index i of the whole text and answers the index past what it read, so a
;; failure names the index in the text the caller was given.
(defn parse-exception [msg]
  (jolt.host/throwable "java.time.format.DateTimeParseException" msg))
(defn parse-fail-at [text i]
  (throw (parse-exception (str "Text '" text "' could not be parsed at index " i))))
(defn parse-fail-unparsed [text i]
  (throw (parse-exception (str "Text '" text "' could not be parsed, unparsed text found at index " i))))
(defn parse-fail-resolve [text e]
  (throw (parse-exception (str "Text '" text "' could not be parsed: " (ex-message e)))))
(defn parse-end!
  "Text past index i was not read by any field."
  [text i]
  (when (< i (count text)) (parse-fail-unparsed text i)))

(defn char-at?
  "The char at index i is c, ignoring case (the ISO formatters parse
  case-insensitively, so 't' and 'z' read as 'T' and 'Z')."
  [s i c]
  (and (< i (count s)) (= (upper (str (nth s i))) (upper (str c)))))

(defn digits-from
  "The index past the run of digits starting at i, reading at most n."
  [s i n]
  (loop [j i] (if (and (< (- j i) n) (< j (count s)) (digit? (nth s j))) (recur (inc j)) j)))

(defn two-digits
  "The two-digit field at i, or nil."
  [s i]
  (when (= (+ i 2) (digits-from s i 2)) (parse-long (subs s i (+ i 2)))))

(defn scan-iso-date
  "ISO_LOCAL_DATE from index i: [year month day end]. A year past four digits
  carries its sign (+10000-01-01, -999999999-01-01); '+' only past four."
  [s i]
  (let [sign (when (< i (count s)) (#{\+ \-} (nth s i)))
        y0 (if sign (inc i) i)
        y1 (digits-from s y0 10)
        ny (- y1 y0)]
    (when (or (< ny 4) (and (nil? sign) (> ny 4)) (and (= sign \+) (<= ny 4))) (parse-fail-at s i))
    (when-not (char-at? s y1 \-) (parse-fail-at s y1))
    (let [m0 (inc y1) m (or (two-digits s m0) (parse-fail-at s m0))
          _ (when-not (char-at? s (+ m0 2) \-) (parse-fail-at s (+ m0 2)))
          d0 (+ m0 3) d (or (two-digits s d0) (parse-fail-at s d0))
          y (parse-long (subs s y0 y1))]
      [(if (= sign \-) (- y) y) m d (+ d0 2)])))

(defn scan-iso-time
  "HH:mm[:ss[.fraction]] from index i: [hour minute second nano end]. With
  need-secs (ISO_INSTANT) the seconds are required; otherwise ':' without two
  digits after it is left unread, as the JDK's optional section leaves it."
  [s i need-secs]
  (let [h (or (two-digits s i) (parse-fail-at s i))
        _ (when-not (char-at? s (+ i 2) \:) (parse-fail-at s (+ i 2)))
        mi (or (two-digits s (+ i 3)) (parse-fail-at s (+ i 3)))
        j (+ i 5)
        sec (if need-secs
              (do (when-not (char-at? s j \:) (parse-fail-at s j))
                  (or (two-digits s (inc j)) (parse-fail-at s (inc j))))
              (when (char-at? s j \:) (two-digits s (inc j))))]
    (if (nil? sec)
      [h mi 0 0 j]
      (let [k (+ j 3)]
        (if (char-at? s k \.)
          (let [f1 (digits-from s (inc k) 9) fd (subs s (inc k) f1)]
            [h mi sec (* (if (= "" fd) 0 (parse-long fd)) (pow10 (- 9 (count fd)))) f1])
          [h mi sec 0 k])))))
