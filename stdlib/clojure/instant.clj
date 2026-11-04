;;   Portions of this file are from Clojure's src/clj/clojure/instant.clj,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; clojure.instant — RFC3339 timestamp parsing and the `#inst` reader
;; constructors. The parser and the range validation are the reference
;; implementation's, unchanged (they are pure Clojure); only the constructors are
;; jolt's, because they build host values.
;;
;; Host-model notes:
;;
;;   - `java.sql.Timestamp` has millisecond resolution on jolt, as every
;;     instant does, so `read-instant-timestamp` truncates the nanosecond
;;     fraction to milliseconds instead of preserving it. It is kept
;;     as a distinct entry point because libraries bind it into *data-readers*.
;;   - `java.util.Calendar` on jolt holds an instant, not an instant plus a zone,
;;     so `read-instant-calendar` yields the right point in time but does not
;;     preserve the literal's offset zone the way the JVM's does.
;;   - print-method / print-dup for Date are NOT installed here: jolt's printer
;;     already emits `#inst "…"` in RFC3339 natively for both, so the reference's
;;     SimpleDateFormat-backed methods would only duplicate it.
(ns clojure.instant
  "Reading and constructing instants from RFC3339-like timestamp strings.")

(defn- fail [msg]
  (throw (new RuntimeException msg)))

(defn- divisible? [num div] (zero? (mod num div)))
(defn- indivisible? [num div] (not (divisible? num div)))

(defn- parse-int [s] (Long/parseLong s))

(defn- zero-fill-right [s width]
  (let [n (count s)]
    (cond (= width n) s
          (< width n) (subs s 0 width)
          :else (str s (apply str (repeat (- width n) \0))))))

(def parse-timestamp
  "Parse a string containing an RFC3339-like timestamp.

  The function new-instant is called with the following arguments.

                  min  max           default
                  ---  ------------  -------
    years          0           9999      N/A (s must provide years)
    months         1             12        1
    days           1             31        1 (actual max days depends
    hours          0             23        0  on month and year)
    minutes        0             59        0
    seconds        0             60        0 (though 60 is only valid
    nanoseconds    0      999999999        0  when minutes is 59)
    offset-sign   -1              1        0
    offset-hours   0             23        0
    offset-minutes 0             59        0

  These are all integers and will be non-nil. (The listed defaults will be passed
  if the corresponding field is not present in s.)

  Unlike RFC3339: only the timestamp format is parsed, trailing components may be
  elided, and a missing time-offset is treated as +00:00."
  (let [timestamp #"(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?)?)?)?(?:[Z]|([-+])(\d\d):(\d\d))?"]
    (fn [new-instant cs]
      (if-let [[_ years months days hours minutes seconds fraction
                offset-sign offset-hours offset-minutes]
               (re-matches timestamp cs)]
        (new-instant
         (parse-int years)
         (if-not months 1 (parse-int months))
         (if-not days 1 (parse-int days))
         (if-not hours 0 (parse-int hours))
         (if-not minutes 0 (parse-int minutes))
         (if-not seconds 0 (parse-int seconds))
         (if-not fraction 0 (parse-int (zero-fill-right fraction 9)))
         (cond (= "-" offset-sign) -1
               (= "+" offset-sign) 1
               :else 0)
         (if-not offset-hours 0 (parse-int offset-hours))
         (if-not offset-minutes 0 (parse-int offset-minutes)))
        (fail (str "Unrecognized date/time syntax: " cs))))))

;;; Verification of the extra-grammatical restrictions from RFC3339.

(defn- leap-year? [year]
  (and (divisible? year 4)
       (or (indivisible? year 100)
           (divisible? year 400))))

(def ^:private days-in-month
  (let [dim-norm [nil 31 28 31 30 31 30 31 31 30 31 30 31]
        dim-leap [nil 31 29 31 30 31 30 31 31 30 31 30 31]]
    (fn [month leap?]
      ((if leap? dim-leap dim-norm) month))))

;; The reference builds the failure message from the unevaluated test form, and
;; its exact text ("failed: (<= 1 months 12)") is what a caller sees, so keep it.
(defmacro ^:private verify [test]
  `(when-not ~test (fail ~(str "failed: " (pr-str test)))))

(defn validated
  "Return a function which constructs an instant by calling constructor after
  first validating that those arguments are in range and otherwise plausible.
  The resulting function will throw an exception if called with invalid
  arguments."
  [new-instance]
  (fn [years months days hours minutes seconds nanoseconds
       offset-sign offset-hours offset-minutes]
    (verify (<= 1 months 12))
    (verify (<= 1 days (days-in-month months (leap-year? years))))
    (verify (<= 0 hours 23))
    (verify (<= 0 minutes 59))
    (verify (<= 0 seconds (if (= minutes 59) 60 59)))
    (verify (<= 0 nanoseconds 999999999))
    (verify (<= -1 offset-sign 1))
    (verify (<= 0 offset-hours 23))
    (verify (<= 0 offset-minutes 59))
    (new-instance years months days hours minutes seconds nanoseconds
                  offset-sign offset-hours offset-minutes)))

;;; Reader integration.

(defn- days-from-civil
  "Days from the Unix epoch to the given proleptic-Gregorian y-m-d (Hinnant's
  algorithm, the same one the host's inst formatter inverts)."
  [y m d]
  (let [y (if (<= m 2) (dec y) y)
        era (quot (if (neg? y) (- y 399) y) 400)
        yoe (- y (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- instant-ms
  "Milliseconds since the epoch for the parsed fields, with the zone offset
  applied — the fields name a local time in that offset, so the offset is
  subtracted to reach UTC."
  [years months days hours minutes seconds millis
   offset-sign offset-hours offset-minutes]
  (let [day-secs (+ (* (days-from-civil years months days) 86400)
                    (* hours 3600) (* minutes 60) seconds)
        offset-secs (* offset-sign (+ (* offset-hours 3600) (* offset-minutes 60)))]
    (+ (* 1000 (- day-secs offset-secs)) millis)))

(defn- construct-date
  "Construct a java.util.Date, which expresses the original instant as
  milliseconds since the epoch, UTC."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (new java.util.Date
       (instant-ms years months days hours minutes seconds (quot nanoseconds 1000000)
                   offset-sign offset-hours offset-minutes)))

(defn- construct-calendar
  "Construct a java.util.Calendar preserving the timezone offset, but truncating
  the subsecond fraction to milliseconds. The fields are set first and the zone
  last, exactly as the reference does: a calendar resolves its fields in whatever
  zone is current when the instant is finally asked for."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (doto (java.util.GregorianCalendar. years (dec months) days hours minutes seconds)
    (.set java.util.Calendar/MILLISECOND (quot nanoseconds 1000000))
    (.setTimeZone (java.util.TimeZone/getTimeZone
                   (format "GMT%s%02d:%02d"
                           (if (neg? offset-sign) "-" "+")
                           offset-hours offset-minutes)))))

(defn- construct-timestamp
  "Construct a java.sql.Timestamp. On jolt it has millisecond resolution, so
  the fraction is truncated to milliseconds rather than kept to nanoseconds."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (new java.sql.Timestamp
       (instant-ms years months days hours minutes seconds (quot nanoseconds 1000000)
                   offset-sign offset-hours offset-minutes)))

(def read-instant-date
  "To read an instant as a java.util.Date, bind *data-readers* to a map with this
  var as the value for the 'inst key. The timezone offset will be used to convert
  into UTC."
  (partial parse-timestamp (validated construct-date)))

(def read-instant-calendar
  "To read an instant as a java.util.Calendar, bind *data-readers* to a map with
  this var as the value for the 'inst key."
  (partial parse-timestamp (validated construct-calendar)))

(def read-instant-timestamp
  "To read an instant as a java.sql.Timestamp, bind *data-readers* to a map with
  this var as the value for the 'inst key. The timezone offset will be used to
  convert into UTC."
  (partial parse-timestamp (validated construct-timestamp)))
