# java.time shims (jolt-ea7): the surface Selmer's date filters use, backed
# by epoch milliseconds (the same representation as :jolt/inst). Local time
# means the HOST's local time (os/date with local=true); zones beyond the
# system default are not modeled. Registered through the evaluator's
# class-statics / tagged-methods registries, so this module is data plus an
# install call — adding another java.* shim follows the same shape.

(use ./evaluator)

(defn- chr [s] (get s 0))

# --- values -------------------------------------------------------------------

(defn- instant [ms] @{:jolt/type :jolt/instant :ms ms})
(defn- zoned [ms zone] @{:jolt/type :jolt/zoned-dt :ms ms :zone zone})
(defn- local-dt [ms] @{:jolt/type :jolt/local-dt :ms ms})
(defn- formatter [pattern &opt locale] @{:jolt/type :jolt/dt-formatter :pattern pattern :locale locale})

(def- zone-default @{:jolt/type :jolt/zone-id :id "system"})

# ms of any date-ish shim value (or a :jolt/inst)
(defn- ms-of [d]
  (cond
    (number? d) d
    (and (or (table? d) (struct? d))
         (or (= :jolt/inst (get d :jolt/type))
             (= :jolt/instant (get d :jolt/type))
             (= :jolt/zoned-dt (get d :jolt/type))
             (= :jolt/local-dt (get d :jolt/type))))
      (get d :ms)
    (error (string "not a date value: " (type d)))))

# --- formatting ----------------------------------------------------------------

(def- month-names ["January" "February" "March" "April" "May" "June" "July"
                   "August" "September" "October" "November" "December"])
(def- day-names ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(defn- pad2 [n] (if (< n 10) (string "0" n) (string n)))

# Format epoch-ms with a (subset of the) JVM DateTimeFormatter pattern:
# yyyy yy MMMM MMM MM M dd d EEEE EEE HH H hh h mm m ss s a, quoted literals
# with '...'. Unknown letters pass through.
(defn- format-ms [pattern ms]
  (def d (os/date (math/floor (/ ms 1000)) true))
  (def out @"")
  (var i 0)
  (def n (length pattern))
  (defn run-len [c]
    (var j i)
    (while (and (< j n) (= (pattern j) c)) (++ j))
    (- j i))
  (while (< i n)
    (def c (pattern i))
    (def k (run-len c))
    (cond
      (= c (chr "'"))
        # quoted literal up to the closing quote ('' = literal quote)
        (if (and (< (+ i 1) n) (= (pattern (+ i 1)) (chr "'")))
          (do (buffer/push out "'") (+= i 2))
          (let [close (string/find "'" pattern (+ i 1))]
            (buffer/push out (string/slice pattern (+ i 1) close))
            (set i (+ close 1))))
      (= c (chr "y"))
        (do (buffer/push out (if (>= k 4) (string (d :year))
                               (pad2 (mod (d :year) 100))))
            (+= i k))
      (= c (chr "M"))
        (do (buffer/push out (case k
                               1 (string (+ 1 (d :month)))
                               2 (pad2 (+ 1 (d :month)))
                               3 (string/slice (in month-names (d :month)) 0 3)
                               (in month-names (d :month))))
            (+= i k))
      (= c (chr "d"))
        (do (buffer/push out (if (= k 1) (string (+ 1 (d :month-day))) (pad2 (+ 1 (d :month-day)))))
            (+= i k))
      (= c (chr "E"))
        (do (buffer/push out (if (>= k 4) (in day-names (d :week-day))
                               (string/slice (in day-names (d :week-day)) 0 3)))
            (+= i k))
      (= c (chr "H"))
        (do (buffer/push out (if (= k 1) (string (d :hours)) (pad2 (d :hours)))) (+= i k))
      (= c (chr "h"))
        (let [h12 (let [h (mod (d :hours) 12)] (if (= h 0) 12 h))]
          (buffer/push out (if (= k 1) (string h12) (pad2 h12))) (+= i k))
      (= c (chr "m"))
        (do (buffer/push out (if (= k 1) (string (d :minutes)) (pad2 (d :minutes)))) (+= i k))
      (= c (chr "s"))
        (do (buffer/push out (if (= k 1) (string (d :seconds)) (pad2 (d :seconds)))) (+= i k))
      (= c (chr "a"))
        (do (buffer/push out (if (< (d :hours) 12) "AM" "PM")) (+= i k))
      (do (buffer/push out (string/from-bytes c)) (++ i))))
  (string out))

# Localized FormatStyle approximations (no locale database on this host).
(def- style-patterns
  {[:date :short] "M/d/yy"          [:date :medium] "MMM d, yyyy"
   [:date :long] "MMMM d, yyyy"     [:date :full] "EEEE, MMMM d, yyyy"
   [:time :short] "h:mm a"          [:time :medium] "h:mm:ss a"
   [:time :long] "h:mm:ss a"        [:time :full] "h:mm:ss a"
   [:datetime :short] "M/d/yy, h:mm a"
   [:datetime :medium] "MMM d, yyyy, h:mm:ss a"
   [:datetime :long] "MMMM d, yyyy, h:mm:ss a"
   [:datetime :full] "EEEE, MMMM d, yyyy, h:mm:ss a"})

(defn- style-fmt [kind style]
  (formatter (get style-patterns [kind (get style :style)] "yyyy-MM-dd HH:mm:ss")))

# --- registration --------------------------------------------------------------

(defn install! []
  (def fs (fn [style] @{:jolt/type :jolt/format-style :style style}))
  (register-class-statics! "FormatStyle"
    @{"SHORT" (fs :short) "MEDIUM" (fs :medium) "LONG" (fs :long) "FULL" (fs :full)})
  (register-class-statics! "DateTimeFormatter"
    @{"ofPattern" (fn [p &opt locale] (formatter p locale))
      "ISO_LOCAL_DATE" (formatter "yyyy-MM-dd")
      "ISO_LOCAL_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ss")
      "ofLocalizedDate" (fn [style] (style-fmt :date style))
      "ofLocalizedTime" (fn [style] (style-fmt :time style))
      "ofLocalizedDateTime" (fn [style] (style-fmt :datetime style))})
  (register-class-statics! "Instant"
    @{"ofEpochMilli" (fn [ms] (instant ms))
      "now" (fn [] (instant (math/floor (* 1000 (os/clock :realtime)))))})
  (register-class-statics! "ZoneId"
    @{"systemDefault" (fn [] zone-default)})
  (register-class-statics! "LocalDateTime"
    @{"ofInstant" (fn [inst zone] (local-dt (ms-of inst)))
      "now" (fn [] (local-dt (math/floor (* 1000 (os/clock :realtime)))))})
  (register-class-statics! "Locale"
    @{"getDefault" (fn [] @{:jolt/type :jolt/locale :id "default"})
      "ENGLISH" @{:jolt/type :jolt/locale :id "en"}
      "US" @{:jolt/type :jolt/locale :id "en-US"}})
  (register-tagged-methods! :jolt/instant
    @{"atZone" (fn [self zone] (zoned (self :ms) zone))
      "toEpochMilli" (fn [self] (self :ms))})
  (register-tagged-methods! :jolt/zoned-dt
    @{"toLocalDateTime" (fn [self] (local-dt (self :ms)))
      "toInstant" (fn [self] (instant (self :ms)))})
  (register-tagged-methods! :jolt/local-dt
    @{"atZone" (fn [self zone] (zoned (self :ms) zone))})
  # a :jolt/inst (#inst — Clojure's java.util.Date) supports the Date methods
  # Selmer's fix-date path calls
  (register-tagged-methods! :jolt/inst
    @{"toInstant" (fn [self] (instant (self :ms)))
      "getTime" (fn [self] (self :ms))})
  (register-tagged-methods! :jolt/dt-formatter
    @{"withLocale" (fn [self locale] (formatter (self :pattern) locale))
      "format" (fn [self d] (format-ms (self :pattern) (ms-of d)))}))

# --- java.io / java.lang shims (Selmer's template reader) ---------------------

(defn- string-reader [src]
  # :close makes with-open's __close happy (it calls (get x :close) when
  # present); :read-line-fn matches the 50-io reader convention so line-seq
  # works over readers io/reader hands back
  (def self @{:jolt/type :jolt/jio-string-reader :s (string src) :pos 0
              :close (fn [] nil)})
  (put self :read-line-fn
    (fn []
      (def {:s s :pos pos} self)
      (when (< pos (length s))
        (def i (string/find "\n" s pos))
        (if (nil? i)
          (do (put self :pos (length s)) (string/slice s pos))
          (do (put self :pos (+ i 1)) (string/slice s pos i))))))
  self)
(defn- string-builder [&opt init]
  # a numeric arg is a CAPACITY (java.lang.StringBuilder(int)), not content
  @{:jolt/type :jolt/string-builder
    :buf (cond (nil? init) @"" (number? init) (buffer/new init) (buffer init))})

(defn- render-piece [x]
  (cond
    (nil? x) "null"
    (and (struct? x) (= :jolt/char (get x :jolt/type))) (string/from-bytes (x :ch))
    (string x)))

(defn install-io! []
  (register-tagged-methods! :jolt/jio-string-reader
    @{"read" (fn [self]
               (if (>= (self :pos) (length (self :s)))
                 -1
                 (let [b ((self :s) (self :pos))]
                   (put self :pos (+ 1 (self :pos)))
                   b)))
      "mark" (fn [self &opt limit] (put self :marked (self :pos)) nil)
      "reset" (fn [self] (put self :pos (or (self :marked) 0)) nil)
      "skip" (fn [self n] (put self :pos (min (length (self :s)) (+ (self :pos) n))) n)
      "close" (fn [self] nil)})
  (register-tagged-methods! :jolt/string-builder
    @{"append" (fn [self x] (buffer/push (self :buf) (render-piece x)) self)
      "toString" (fn [self] (string (self :buf)))
      "length" (fn [self] (length (self :buf)))
      "setLength" (fn [self n]
                    (def buf (self :buf))
                    (if (< n (length buf))
                      (buffer/popn buf (- (length buf) n))
                      (while (< (length buf) n) (buffer/push buf "\0")))
                    nil)
      "charAt" (fn [self i] {:jolt/type :jolt/char :ch ((self :buf) i)})})
  (each nm ["File" "java.io.File"]
    (register-class-statics! nm @{"separator" "/" "separatorChar" {:jolt/type :jolt/char :ch 47}}))
  (register-class-statics! "Class"
    @{"forName" (fn [nm] @{:jolt/type :jolt/class :name nm})})
  (each nm ["StringReader" "java.io.StringReader"]
    (register-class-ctor! nm string-reader))
  (each nm ["StringBuilder" "java.lang.StringBuilder"]
    (register-class-ctor! nm string-builder))
  # java.net.URL: enough for selmer's template cache — file: URLs only.
  # A protocol-less spec throws (selmer catches MalformedURLException and
  # prepends file:///), and getPath hands back a stat-able filesystem path.
  (defn url-path [spec]
    (var p (if (string/has-prefix? "file:" spec) (string/slice spec 5) spec))
    (while (and (> (length p) 1) (string/has-prefix? "//" p))
      (set p (string/slice p 1)))
    p)
  (register-tagged-methods! :jolt/url
    @{"getPath" (fn [self] (url-path (self :spec)))
      "getFile" (fn [self] (url-path (self :spec)))
      "toString" (fn [self] (self :spec))
      "toExternalForm" (fn [self] (self :spec))})
  (each nm ["URL" "java.net.URL"]
    (register-class-ctor! nm
      (fn [spec & _]
        (def s (string spec))
        (def colon (string/find ":" s))
        (if (or (nil? colon) (= colon 0)
                (string/find "/" (string/slice s 0 colon)))
          (error (string "MalformedURLException: no protocol: " s))
          @{:jolt/type :jolt/url :spec s}))))
  (each nm ["Locale" "java.util.Locale"]
    (register-class-ctor! nm (fn [id &opt _country] @{:jolt/type :jolt/locale :id (string id)}))))

(install!)
(install-io!)
