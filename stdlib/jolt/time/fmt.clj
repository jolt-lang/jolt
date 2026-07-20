(ns jolt.time.fmt
  "The base DateTimeFormatter: a pattern/ISO engine over the base temporals, with
  English month/day names. This is the core-owned base (RFC 0008); it computes
  from epoch arithmetic alone and needs no locale data.

  Localized display names (through jolt.host/locale-name / strftime) and the
  zone-aware parse/format for ZonedDateTime / OffsetDateTime live in the
  jolt-lang/time library's fuller jolt.time.fmt, which shadows this namespace
  when that library is on the load path."
  (:require [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.local :as l]
            [jolt.time.year :as y]
            [jolt.time.instant :as inst]
            [jolt.time.zones :as z]))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))

;; --- display names (English base) --------------------------------------------
(def ^:private en-months
  ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"])
(def ^:private en-days ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(defn- month-name [_locale mo full?]
  (let [n (nth en-months (dec mo))] (if full? n (subs n 0 3))))
(defn- day-name [_locale dow full?]
  (let [n (nth en-days (dec dow))] (if full? n (subs n 0 3))))

;; --- field extraction: [y mo d hh mi se nano dow off zid] --------------------
(defn- parts [v]
  (let [tk (impl/type-of v)
        from-ldt (fn [ed nod off zid]
                   (let [[y mo d] (civil-from-days ed)]
                     [y mo d (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod)
                      (inc (u/floor-mod (+ ed 3) 7)) off zid]))]
    (if (and (not tk) (instance? java.util.Date v))
      ;; the #inst/Date layer's value (UTC), so its .format renders like an instant
      (let [ms (.getTime v) secs (u/floor-div ms 1000)
            ed (u/floor-div secs 86400) nod (* (u/floor-mod secs 86400) u/nanos-per-sec)]
        (from-ldt ed nod 0 nil))
    (condp = tk
      :jolt.time/local-date (from-ldt (l/ld-epoch-day v) 0 0 nil)
      :jolt.time/local-time (let [nod (l/lt-nano-of-day v)] [1970 1 1 (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod) 4 0 nil])
      :jolt.time/local-date-time (from-ldt (l/ldt-epoch-day v) (l/ldt-nod v) 0 nil)
      :jolt.time/instant (let [n (inst/inst-nanos v) secs (u/floor-div n u/nanos-per-sec) ed (u/floor-div secs 86400) nod (+ (* (u/floor-mod secs 86400) u/nanos-per-sec) (u/floor-mod n u/nanos-per-sec))]
                           (from-ldt ed nod 0 nil))
      nil))))

(defn- off-iso [secs colon allow-z]
  (if (and allow-z (zero? secs)) "Z"
    (let [neg (neg? secs) a (abs secs) h (quot a 3600) m (quot (mod a 3600) 60) s (mod a 60)]
      (str (if neg "-" "+") (u/pad2 h) (if colon ":" "") (u/pad2 m)
           (if (zero? s) "" (str (if colon ":" "") (u/pad2 s)))))))

(defn- scan-quote
  "Scan a '…' literal starting at the opening quote i; -> [next-i appended-out]."
  [pattern i n out]
  (if (and (< (inc i) n) (= \' (nth pattern (inc i))))
    [(+ i 2) (str out "'")]                       ; '' -> a literal quote
    (loop [j (inc i) acc out]
      (cond (>= j n) [j acc]
            (= \' (nth pattern j)) [(inc j) acc]
            :else (recur (inc j) (str acc (nth pattern j)))))))

(defn format-pattern [pattern v locale]
  (let [p (parts v)]
    (if-not p (str v)
      (let [[y mo d hh mi se nano dow off zid] p n (count pattern)]
        (letfn [(run-len [i c] (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i))))]
          (loop [i 0 out ""]
            (if (>= i n) out
              (let [c (nth pattern i) k (run-len i c)]
                (cond
                  (= c \') (let [[j o] (scan-quote pattern i n out)] (recur j o))
                  (or (= c \y) (= c \Y)) (recur (+ i k) (str out (if (>= k 4) (u/pad4 y) (u/pad2 (mod y 100)))))
                  (= c \M) (recur (+ i k) (str out (cond (= k 1) (str mo) (= k 2) (u/pad2 mo) (= k 3) (month-name locale mo false) :else (month-name locale mo true))))
                  (= c \d) (recur (+ i k) (str out (if (= k 1) (str d) (u/pad2 d))))
                  (= c \E) (recur (+ i k) (str out (day-name locale dow (>= k 4))))
                  (= c \H) (recur (+ i k) (str out (if (= k 1) (str hh) (u/pad2 hh))))
                  (= c \h) (recur (+ i k) (str out (let [h12 (let [h (mod hh 12)] (if (zero? h) 12 h))] (if (= k 1) (str h12) (u/pad2 h12)))))
                  (= c \m) (recur (+ i k) (str out (if (= k 1) (str mi) (u/pad2 mi))))
                  (= c \s) (recur (+ i k) (str out (if (= k 1) (str se) (u/pad2 se))))
                  (= c \S) (recur (+ i k) (str out (u/pad-left (str (quot nano (u/pow10 (max 0 (- 9 k))))) k)))
                  (= c \a) (recur (+ i k) (str out (if (< hh 12) "AM" "PM")))
                  (= c \X) (recur (+ i k) (str out (off-iso off (>= k 3) true)))
                  (= c \x) (recur (+ i k) (str out (off-iso off (>= k 3) false)))
                  (= c \Z) (recur (+ i k) (str out (off-iso off (>= k 3) false)))
                  (or (= c \V) (= c \z)) (recur (+ i k) (str out (or zid (off-iso off true true))))
                  :else (recur (inc i) (str out c)))))))))))

;; --- DateTimeFormatter -------------------------------------------------------
(defn formatter [pattern locale] (impl/value :jolt.time/dt-formatter {:pattern pattern :locale (or locale "en")}))
(defn- fmt-pattern [f] (impl/field f :pattern))
(defn- fmt-locale [f] (impl/field f :locale))
(defn fmt? [f] (= :jolt.time/dt-formatter (impl/type-of f)))

(defn do-format [f v]
  (cond (and (impl/jt? f) (fmt? f)) (format-pattern (fmt-pattern f) v (fmt-locale f))
        (string? f) (format-pattern f v "en")
        :else (str v)))

(impl/register-type! :jolt.time/dt-formatter
  {:eq (fn [a b] (= (fmt-pattern a) (fmt-pattern b))) :hash (fn [f] (hash (fmt-pattern f)))
   :str fmt-pattern :cmp nil :classes #{"java.time.format.DateTimeFormatter" "DateTimeFormatter"}})

(defn- locale-id [l]
  (cond (and (impl/jt? l) (= :jolt.time/locale (impl/type-of l))) (impl/field l :id)
        (string? l) l :else "en"))

(__register-class-methods! :jolt.time/dt-formatter
  {"format" (fn [self v] (format-pattern (fmt-pattern self) v (fmt-locale self)))
   "withLocale" (fn [self l] (formatter (fmt-pattern self) (locale-id l)))
   "withZone" (fn [self _z] self)
   "parse" (fn [self s] (let [f (parse-with-pattern (fmt-pattern self) (str s))]
                          (l/local-dt (days-from-civil (:year f) (:month f) (:day f))
                                      (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f)))))
   "toString" fmt-pattern})

;; register .format on every base temporal
(doseq [tk [:jolt.time/local-date :jolt.time/local-time :jolt.time/local-date-time :jolt.time/instant]]
  (__register-class-methods! tk {"format" (fn [self f] (do-format f self))}))

;; java.util.Locale stays in core's always-available java.util layer (the rich
;; Scheme registration in host/chez/java/inst-time.ss). The base formatter is
;; English-only, so it treats any locale opaquely (locale-id falls to "en"); the
;; jolt-lang/time library adds its own Locale and the localized-name path.

;; java.time.format.FormatStyle
(def ^:private style-patterns
  {:date {"SHORT" "M/d/yy" "MEDIUM" "MMM d, yyyy" "LONG" "MMMM d, yyyy" "FULL" "EEEE, MMMM d, yyyy"}
   :time {"SHORT" "h:mm a" "MEDIUM" "h:mm:ss a" "LONG" "h:mm:ss a" "FULL" "h:mm:ss a"}
   :datetime {"SHORT" "M/d/yy, h:mm a" "MEDIUM" "MMM d, yyyy, h:mm:ss a" "LONG" "MMMM d, yyyy, h:mm:ss a" "FULL" "EEEE, MMMM d, yyyy, h:mm:ss a"}})
(statics! ["FormatStyle" "java.time.format.FormatStyle"]
  (into {} (map (fn [s] [s (impl/value :jolt.time/format-style {:style s})]) ["SHORT" "MEDIUM" "LONG" "FULL"])))
(impl/register-type! :jolt.time/format-style
  {:eq (fn [a b] (= (impl/field a :style) (impl/field b :style))) :hash (fn [s] (hash (impl/field s :style)))
   :str (fn [s] (impl/field s :style)) :cmp nil :classes #{"java.time.format.FormatStyle" "FormatStyle"}})
(defn- style-of [fs] (impl/field fs :style))
(defn- style-fmt [kind fs] (formatter (get-in style-patterns [kind (style-of fs)] "yyyy-MM-dd HH:mm:ss") "en"))

(statics! ["DateTimeFormatter" "java.time.format.DateTimeFormatter"]
  {"ofPattern" (fn [p & r] (formatter (str p) (if (seq r) (locale-id (first r)) "en")))
   "ISO_LOCAL_DATE" (formatter "yyyy-MM-dd" "en") "ISO_LOCAL_TIME" (formatter "HH:mm:ss" "en")
   "ISO_LOCAL_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ss" "en")
   "ISO_DATE" (formatter "yyyy-MM-dd" "en") "ISO_TIME" (formatter "HH:mm:ss" "en")
   "ISO_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ss" "en")
   "ISO_INSTANT" (formatter "yyyy-MM-dd'T'HH:mm:ssX" "en")
   "ISO_OFFSET_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ssXXX" "en")
   "ISO_OFFSET_TIME" (formatter "HH:mm:ssXXX" "en")
   "ISO_OFFSET_DATE" (formatter "yyyy-MM-ddXXX" "en")
   "ISO_ORDINAL_DATE" (formatter "yyyy-DDD" "en")
   "ISO_WEEK_DATE" (formatter "yyyy-'W'ww-e" "en")
   "BASIC_ISO_DATE" (formatter "yyyyMMdd" "en")
   "RFC_1123_DATE_TIME" (formatter "EEE, dd MMM yyyy HH:mm:ss Z" "en")
   "ofLocalizedDate" (fn [fs] (style-fmt :date fs))
   "ofLocalizedTime" (fn [fs] (style-fmt :time fs))
   "ofLocalizedDateTime" (fn [fs] (style-fmt :datetime fs))})

;; --- DateTimeFormatterBuilder (minimal) --------------------------------------
(defn- builder [] (impl/value :jolt.time/dtf-builder {:pattern (atom "")}))
(defn- b-append! [b s] (swap! (impl/field b :pattern) str s) b)
(__register-class-ctor! "DateTimeFormatterBuilder" (fn [& _] (builder)))
(__register-class-ctor! "java.time.format.DateTimeFormatterBuilder" (fn [& _] (builder)))
(impl/register-type! :jolt.time/dtf-builder
  {:eq (fn [a b] (identical? a b)) :hash (fn [_] 0) :str (fn [_] "DateTimeFormatterBuilder") :cmp nil
   :classes #{"java.time.format.DateTimeFormatterBuilder" "DateTimeFormatterBuilder"}})
(__register-class-methods! :jolt.time/dtf-builder
  {"appendPattern" (fn [b p] (b-append! b (str p)))
   "appendLiteral" (fn [b s] (b-append! b (str "'" s "'")))
   "parseCaseInsensitive" (fn [b] b) "parseLenient" (fn [b] b)
   "optionalStart" (fn [b] b) "optionalEnd" (fn [b] b)
   "toFormatter" (fn [b & _] (formatter @(impl/field b :pattern) "en"))})

;; --- pattern-based parse -----------------------------------------------------
;; Walk the pattern and input together: pattern letters consume digits into fields;
;; literals (quoted or plain) must match. Returns {:year :month :day :hour :min
;; :sec :nano :off}.
(defn parse-with-pattern [pattern s]
  (let [n (count pattern) sl (count s)]
    (letfn [(run-len [i c] (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i))))
            (digits [si k] (loop [j si acc 0 got 0]
                             (if (and (< j sl) (>= (int (nth s j)) 48) (<= (int (nth s j)) 57) (< got k))
                               (recur (inc j) (+ (* acc 10) (- (int (nth s j)) 48)) (inc got))
                               [acc j])))]
      (loop [i 0 si 0 f {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0 :nano 0 :off 0}]
        (if (>= i n) f
          (let [c (nth pattern i) k (run-len i c)]
            (cond
              (= c \') (let [[j o] (scan-quote pattern i n "")]  ; the literal to match
                         (recur j (+ si (count o)) f))
              (= c \y) (let [[v si'] (digits si (max k 4))] (recur (+ i k) si' (assoc f :year (if (<= k 2) (+ 2000 v) v))))
              (= c \M) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :month v)))
              (= c \d) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :day v)))
              (= c \H) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :hour v)))
              (= c \m) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :min v)))
              (= c \s) (let [[v si'] (digits si 2)
                             ;; ISO patterns (…ss XXX) don't spell out the fraction; consume an
                             ;; optional ".fffffffff" so 10:59:13.417Z keeps its millis.
                             [nano si2] (if (and (< si' sl) (= \. (nth s si')))
                                          (let [[fv fend] (digits (inc si') 9)]
                                            [(* fv (u/pow10 (max 0 (- 9 (dec (- fend si')))))) fend])
                                          [(:nano f) si'])]
                         (recur (+ i k) si2 (assoc f :sec v :nano nano)))
              (= c \S) (let [[v si'] (digits si k)] (recur (+ i k) si' (assoc f :nano (* v (u/pow10 (max 0 (- 9 k)))))))
              (or (= c \V) (= c \X) (= c \Z) (= c \z))
                (if (and (< si sl) (#{\Z \z} (nth s si)))
                  (recur (+ i k) (inc si) (assoc f :off 0))
                  (let [end (loop [j si] (if (and (< j sl) (or (#{\+ \- \:} (nth s j)) (and (>= (int (nth s j)) 48) (<= (int (nth s j)) 57)))) (recur (inc j)) j))]
                    (recur (+ i k) end (assoc f :off (if (> end si) (z/parse-zone-offset (subs s si end)) 0)))))
              :else (recur (inc i) (inc si) f))))))))

(defn- fmt-of [f] (if (and (impl/jt? f) (fmt? f)) (fmt-pattern f) (str f)))
(defn- pfields [fmt s] (parse-with-pattern (fmt-of fmt) (str s)))

;; formatter-aware parse for the base temporals, overriding the ISO-only statics
;; for the 2-arg case.
(defn- ldt-of [f] (l/local-dt (days-from-civil (:year f) (:month f) (:day f))
                              (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f))))
(statics! ["LocalDate" "java.time.LocalDate"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (l/local-date (days-from-civil (:year f) (:month f) (:day f)))) (l/local-date (l/parse-iso-date (str s)))))})
(statics! ["LocalTime" "java.time.LocalTime"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (l/local-time (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f)))) (l/local-time (u/parse-hms->nano (str s)))))})
(statics! ["LocalDateTime" "java.time.LocalDateTime"] {"parse" (fn [s & fmt] (if (seq fmt) (ldt-of (pfields (first fmt) s)) (let [d (str s) ti (.indexOf d "T")] (l/local-dt (l/parse-iso-date (subs d 0 ti)) (u/parse-hms->nano (subs d (inc ti)))))))})
(statics! ["YearMonth" "java.time.YearMonth"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (y/year-month (:year f) (:month f))) (y/year-month (u/digits-at (str s) 0 4) (u/digits-at (str s) 5 2))))})
