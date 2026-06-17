# Host interop — java.lang static surfaces (Math/Thread/System/Long) + the
# java.time shim (epoch-ms values + a DateTimeFormatter pattern subset). Split
# from host_interop.janet (jolt-jx5l, phase 1). Everything registers through the
# evaluator's class-statics / tagged-methods registries via install!. Also the
# home of the shared coercion helpers (chr/pad2) that host_io reuses.
(use ../evaluator)
(use ../regex)
(use ../core)
(use ../pv)
(use ../plist)
(use ../types)
(use ../lazyseq)
(import ../phm)

(defn chr [s] (get s 0))

# --- java.lang static surfaces (Math/Thread/System/Long) ----------------------
# Registered through the generic class-statics registry below, same as every
# other class — there is no special-case dispatch (jolt-jx5l).
(def- math-statics
  @{"sqrt" math/sqrt "pow" math/pow "floor" math/floor "ceil" math/ceil
    "abs" (fn [x] (if (< x 0) (- x) x))
    "round" (fn [x] (math/round x))
    "sin" math/sin "cos" math/cos "tan" math/tan
    "asin" math/asin "acos" math/acos "atan" math/atan
    "log" math/log "log10" math/log10 "exp" math/exp
    "max" (fn [a b] (if (> a b) a b)) "min" (fn [a b] (if (< a b) a b))
    "signum" (fn [x] (cond (< x 0) -1.0 (> x 0) 1.0 0.0))
    "PI" math/pi "E" math/e
    "random" (fn [&] (math/random))})

# sleep parks the CURRENT thread's event loop — inside a future body that's the
# worker OS thread (ev/spawn-thread gives each worker its own loop), so a
# sleeping future doesn't block the parent.
(def- thread-statics
  {"sleep" (fn [ms] (ev/sleep (/ ms 1000)) nil)
   "yield" (fn [] (ev/sleep 0) nil)
   "interrupted" (fn [] false)
   "currentThread" (fn [] @{:jolt/type :jolt/thread :id "main"})})

# wall/monotonic clocks + properties/env (what portable timing/config code uses).
(def- system-statics
  # realtime clock (sub-ms float epoch seconds) — os/time is whole seconds,
  # which quantized every elapsed-time measurement to 1000ms.
  {"currentTimeMillis" (fn [] (math/floor (* 1000 (os/clock :realtime))))
   "nanoTime" (fn [] (math/floor (* 1e9 (os/clock :monotonic))))
   "getProperty" (fn [k &opt dflt]
                   (case k
                     "os.name" (case (os/which)
                                 :windows "Windows" :macos "Mac OS X" "Linux")
                     "line.separator" "\n"
                     "file.separator" "/"
                     "user.dir" (os/cwd)
                     "user.home" (os/getenv "HOME")
                     "java.io.tmpdir" (or (os/getenv "TMPDIR") "/tmp")
                     dflt))
   # JOLT_BAKE_ENV_ALLOWLIST (jolt-s3j): during an image bake (jpm build of a
   # native executable, set by the project's build.sh) the env snapshot that
   # libraries like config.core capture at load gets MARSHALED INTO THE BINARY
   # — GitHub push protection once flagged real API tokens inside an example's
   # build output. With the var set, System/getenv serves only the listed
   # comma-separated names (single-var reads of unlisted names return nil), so
   # nothing secret can bake. Unset (the normal runtime case), reads are live
   # and unfiltered.
   "getenv" (fn [&opt k]
              (def allow (os/getenv "JOLT_BAKE_ENV_ALLOWLIST"))
              (if (nil? allow)
                (if k (os/getenv k) (os/environ))
                (let [names (string/split "," allow)
                      ok @{}]
                  (each n names (put ok (string/trim n) true))
                  (if k
                    (when (get ok k) (os/getenv k))
                    (let [e (os/environ) out @{}]
                      (eachp [ek ev] e (when (get ok ek) (put out ek ev)))
                      out)))))
   # the property subset getProperty serves, as an iterable map
   "getProperties" (fn []
                     {"os.name" (case (os/which)
                                  :windows "Windows" :macos "Mac OS X" "Linux")
                      "line.separator" "\n"
                      "file.separator" "/"
                      "user.dir" (os/cwd)
                      "user.home" (or (os/getenv "HOME") "")
                      "java.io.tmpdir" (or (os/getenv "TMPDIR") "/tmp")})
   # terminate the process with the given status code
   "exit" (fn [&opt status] (os/exit (if (nil? status) 0 status)))})

# sentinels portable code compares against. jolt numbers are doubles, so these
# are the f64 approximations.
(def- long-statics
  {"MAX_VALUE" 9223372036854775807
   "MIN_VALUE" -9223372036854775808
   "parseLong" (fn [s &opt radix]
                 (def n (scan-number (string/trim (string s)) (or radix 10)))
                 (if (and n (= n (math/floor n)))
                   n
                   (error (string "NumberFormatException: For input string: \"" s "\""))))
   "valueOf" (fn [s &opt radix]
               (def n (scan-number (string/trim (string s)) (or radix 10)))
               (if (and n (= n (math/floor n)))
                 n
                 (error (string "NumberFormatException: For input string: \"" s "\""))))})

# --- values -------------------------------------------------------------------

(defn- instant [ms] @{:jolt/type :jolt/instant :ms ms})
(defn- zoned [ms zone] @{:jolt/type :jolt/zoned-dt :ms ms :zone zone})
(defn- local-dt [ms] @{:jolt/type :jolt/local-dt :ms ms})
(defn formatter [pattern &opt locale] @{:jolt/type :jolt/dt-formatter :pattern pattern :locale locale})

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

(defn pad2 [n] (if (< n 10) (string "0" n) (string n)))

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
  # java.lang statics through the generic registry (no resolve-sym special-case).
  (register-class-statics! "Math" math-statics)
  (register-class-statics! "Thread" thread-statics)
  (register-class-statics! "System" system-statics)
  (register-class-statics! "Long" long-statics)
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
  (let [locale-statics @{"getDefault" (fn [] @{:jolt/type :jolt/locale :id "default"})
                         "ENGLISH" @{:jolt/type :jolt/locale :id "en"}
                         "US" @{:jolt/type :jolt/locale :id "en-US"}
                         "ROOT" @{:jolt/type :jolt/locale :id "root"}}]
    (each nm ["Locale" "java.util.Locale"]
      (register-class-statics! nm locale-statics)))
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
