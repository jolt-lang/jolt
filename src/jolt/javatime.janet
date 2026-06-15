# java.time shims (jolt-ea7): the surface Selmer's date filters use, backed
# by epoch milliseconds (the same representation as :jolt/inst). Local time
# means the HOST's local time (os/date with local=true); zones beyond the
# system default are not modeled. Registered through the evaluator's
# class-statics / tagged-methods registries, so this module is data plus an
# install call — adding another java.* shim follows the same shape.

(use ./evaluator)
(use ./regex)
(import ./phm)

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

(defn make-string-writer []
  # :close lets with-open close the writer (core-close-resource calls :close);
  # it's a no-op so .toString after with-open still sees the buffer.
  @{:jolt/type :jolt/writer :buf @"" :sink nil :close (fn [] nil)})
(defn make-out-writer []
  @{:jolt/type :jolt/writer :buf nil :sink prin})

(defn- render-piece [x]
  (cond
    (nil? x) "null"
    (and (struct? x) (= :jolt/char (get x :jolt/type))) (string/from-bytes (x :ch))
    (string x)))

# Writer.write(int) writes the CHAR for that code (unlike StringBuilder.append(int),
# which appends the int's digits). jolt chars are bytes, so this round-trips UTF-8
# byte-for-byte with readLine.
(defn- writer-piece [x]
  (if (number? x) (string/from-bytes (math/trunc x)) (render-piece x)))

# Read one unit from any reader-ish value: our shims dispatch through their
# tagged "read"; a janet core/file reads one byte. -1 at EOF.
(defn- read-unit [r]
  (cond
    (and (or (table? r) (struct? r)) (get r :jolt/type))
      (((get tagged-methods (r :jolt/type)) "read") r)
    (= :core/file (type r))
      (let [b (file/read r 1)] (if (or (nil? b) (= 0 (length b))) -1 (b 0)))
    (error (string "not a reader: " (type r)))))

(defn- pushback-reader [rdr]
  # java.io.PushbackReader: read delegates to the wrapped reader unless
  # something was unread; unread takes a char (or char code) and pushes it back
  (def self @{:jolt/type :jolt/pushback-reader :rdr rdr :pushed @[]
              :close (fn [] nil)})
  self)

(defn install-io! []
  (register-tagged-methods! :jolt/pushback-reader
    @{"read" (fn [self]
               (if (> (length (self :pushed)) 0)
                 (array/pop (self :pushed))
                 (read-unit (self :rdr))))
      "unread" (fn [self ch]
                 (array/push (self :pushed)
                             (if (number? ch) ch (get ch :ch)))
                 nil)
      "close" (fn [self] nil)})
  (register-tagged-methods! :jolt/jio-string-reader
    @{"read" (fn [self]
               (if (>= (self :pos) (length (self :s)))
                 -1
                 (let [b ((self :s) (self :pos))]
                   (put self :pos (+ 1 (self :pos)))
                   b)))
      "readLine" (fn [self] ((self :read-line-fn)))
      "mark" (fn [self &opt limit] (put self :marked (self :pos)) nil)
      "reset" (fn [self] (put self :pos (or (self :marked) 0)) nil)
      "skip" (fn [self n] (put self :pos (min (length (self :s)) (+ (self :pos) n))) n)
      "close" (fn [self] nil)})
  # java.io.Writer / StringWriter — the print-method protocol surface
  # (jolt-g1r). A writer either pushes to a sink fn (stdout/custom) or
  # accumulates in a buffer (StringWriter). write/append coerce chars the
  # same way StringBuilder does.
  (register-tagged-methods! :jolt/writer
    @{"write"    (fn [self x]
                   (if (self :sink)
                     ((self :sink) (writer-piece x))
                     (buffer/push-string (self :buf) (writer-piece x)))
                   nil)
      "append"   (fn [self x]
                   (if (self :sink)
                     ((self :sink) (render-piece x))
                     (buffer/push-string (self :buf) (render-piece x)))
                   self)
      "flush"    (fn [self] nil)
      "close"    (fn [self] nil)
      "toString" (fn [self] (string (or (self :buf) "")))})
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
  (register-class-statics! "Boolean"
    @{"parseBoolean" (fn [s] (= "true" (string/ascii-lower (string s))))
      "TRUE" true "FALSE" false})
  (register-class-statics! "Class"
    @{"forName" (fn [nm] @{:jolt/type :jolt/class :name nm})})
  (each nm ["StringReader" "java.io.StringReader"]
    (register-class-ctor! nm string-reader))
  (each nm ["StringBuilder" "java.lang.StringBuilder"]
    (register-class-ctor! nm string-builder))
  (each nm ["StringWriter" "java.io.StringWriter"]
    (register-class-ctor! nm make-string-writer))
  # --- java.net / java.util surface for ring-codec (ring-core enablement) ---
  # URLEncoder/URLDecoder: www-form-urlencoded (space <-> +, %XX bytes,
  # [A-Za-z0-9.*_-] literal). Charset args are accepted and ignored beyond
  # the name (everything is UTF-8 bytes here).
  (defn- url-unreserved? [b]
    (or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90))
        (and (>= b 97) (<= b 122)) (= b 46) (= b 42) (= b 95) (= b 45)))
  (defn- url-encode-www [s & _]
    (def out @"")
    (each b (string/bytes (string s))
      (cond
        (url-unreserved? b) (buffer/push-byte out b)
        (= b 32) (buffer/push-string out "+")
        (buffer/push-string out (string/format "%%%02X" b))))
    (string out))
  (defn- hexv [b]
    (cond (and (>= b 48) (<= b 57)) (- b 48)
          (and (>= b 65) (<= b 70)) (- b 55)
          (and (>= b 97) (<= b 102)) (- b 87)
          (error "URLDecoder: malformed escape")))
  (defn- url-decode-www [s & _]
    (def bytes (string/bytes (string s)))
    (def n (length bytes))
    (def out @"")
    (var i 0)
    (while (< i n)
      (def b (in bytes i))
      (cond
        (= b 43) (do (buffer/push-string out " ") (++ i))
        (= b 37) (if (< (+ i 2) n)
                   (do (buffer/push-byte out (+ (* 16 (hexv (in bytes (+ i 1)))) (hexv (in bytes (+ i 2)))))
                       (+= i 3))
                   (error "URLDecoder: incomplete escape"))
        (do (buffer/push-byte out b) (++ i))))
    (string out))
  (each nm ["URLEncoder" "java.net.URLEncoder"]
    (register-class-statics! nm @{"encode" url-encode-www}))
  (each nm ["URLDecoder" "java.net.URLDecoder"]
    (register-class-statics! nm @{"decode" url-decode-www}))
  (each nm ["Charset" "java.nio.charset.Charset"]
    (register-class-statics! nm @{"forName" (fn [nm*] @{:jolt/type :jolt/charset :name nm*})}))
  # Base64 (RFC 4648): encoder/decoder singletons with encode/decode methods.
  (def- b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (defn- b64-encode [bs]
    (def bytes (if (bytes? bs) bs (string bs)))
    (def n (length bytes))
    (def out @"")
    (var i 0)
    (while (< i n)
      (def b0 (in bytes i))
      (def b1 (if (< (+ i 1) n) (in bytes (+ i 1))))
      (def b2 (if (< (+ i 2) n) (in bytes (+ i 2))))
      (buffer/push-byte out (in b64-alphabet (brshift b0 2)))
      (buffer/push-byte out (in b64-alphabet (bor (blshift (band b0 3) 4) (brshift (or b1 0) 4))))
      (buffer/push-string out (if (nil? b1) "=" (string/from-bytes (in b64-alphabet (bor (blshift (band b1 0xf) 2) (brshift (or b2 0) 6))))))
      (buffer/push-string out (if (nil? b2) "=" (string/from-bytes (in b64-alphabet (band b2 0x3f)))))
      (+= i 3))
    (string out))
  (def- b64-rev (do (def t @{}) (eachp [i c] b64-alphabet (put t c i)) t))
  (defn- b64-decode [s]
    (def cleaned (string/replace-all "=" "" (string s)))
    (def out @"")
    (var acc 0) (var bits 0)
    (each c (string/bytes cleaned)
      (def v (get b64-rev c))
      (when (nil? v) (error "Base64: illegal character"))
      (set acc (bor (blshift acc 6) v))
      (+= bits 6)
      (when (>= bits 8)
        (-= bits 8)
        (buffer/push-byte out (band (brshift acc bits) 0xff))))
    out)
  (register-tagged-methods! :jolt/base64-encoder @{"encode" (fn [self bs] (b64-encode bs)) "encodeToString" (fn [self bs] (b64-encode bs))})
  (register-tagged-methods! :jolt/base64-decoder @{"decode" (fn [self s] (b64-decode s))})
  (each nm ["Base64" "java.util.Base64"]
    (register-class-statics! nm
      @{"getEncoder" (fn [] @{:jolt/type :jolt/base64-encoder})
        "getDecoder" (fn [] @{:jolt/type :jolt/base64-decoder})}))
  # Integer statics: valueOf with optional radix. Returns a plain number —
  # byteValue/intValue live on the number method surface in the evaluator.
  (register-class-statics! "Integer"
    @{"valueOf" (fn [x &opt radix]
                  (cond
                    (number? x) x
                    (nil? radix) (or (scan-number (string x)) (error (string "NumberFormatException: " x)))
                    (= radix 16) (or (scan-number (string "16r" x)) (error (string "NumberFormatException: " x)))
                    (= radix 8) (or (scan-number (string "8r" x)) (error (string "NumberFormatException: " x)))
                    (= radix 2) (or (scan-number (string "2r" x)) (error (string "NumberFormatException: " x)))
                    (error (string "Integer/valueOf: unsupported radix " radix))))
      "parseInt" (fn [x &opt radix]
                   (or (scan-number (string (case radix 16 "16r" 8 "8r" 2 "2r" "") x))
                       (error (string "NumberFormatException: " x))))
      "MAX_VALUE" 2147483647
      "MIN_VALUE" -2147483648})
  # StringTokenizer: eager split on any delimiter char, empty tokens skipped.
  (defn- tokenize [s delims]
    (def dset @{})
    (each b (string/bytes delims) (put dset b true))
    (def toks @[])
    (def cur @"")
    (each b (string/bytes (string s))
      (if (get dset b)
        (when (> (length cur) 0) (array/push toks (string cur)) (buffer/clear cur))
        (buffer/push-byte cur b)))
    (when (> (length cur) 0) (array/push toks (string cur)))
    toks)
  (register-tagged-methods! :jolt/string-tokenizer
    @{"hasMoreTokens" (fn [self] (< (self :pos) (length (self :toks))))
      "countTokens"   (fn [self] (- (length (self :toks)) (self :pos)))
      "nextToken"     (fn [self]
                        (if (< (self :pos) (length (self :toks)))
                          (let [t (in (self :toks) (self :pos))]
                            (put self :pos (+ 1 (self :pos))) t)
                          (error "NoSuchElementException")))})
  (each nm ["StringTokenizer" "java.util.StringTokenizer"]
    (register-class-ctor! nm (fn [s &opt delims]
                               @{:jolt/type :jolt/string-tokenizer
                                 :toks (tokenize s (or delims " \t\n\r\f"))
                                 :pos 0})))
  # clojure.lang.MapEntry: a 2-tuple, jolt's map-entry representation.
  # java.util.HashMap: a mutable wrapper over a janet table, keyed by canonical
  # key (so jolt collection keys compare by value). reitit uses it as a fast
  # read cache: (HashMap. m) copies a map's entries, (.get hm k) reads.
  # raw value-keys: reitit's HashMap keys are strings/keywords/tuples, all of
  # which janet tables key by value — no canonicalization needed here.
  (defn- hm-entries [m]
    (cond (phm/phm? m) (phm/phm-entries m)
          (struct? m) (pairs m)
          (table? m) (pairs m)
          @[]))
  (register-tagged-methods! :jolt/hashmap
    @{"get"         (fn [self k] (get (self :tbl) k))
      "put"         (fn [self k v] (put (self :tbl) k v) v)
      "containsKey" (fn [self k] (not (nil? (get (self :tbl) k))))
      "size"        (fn [self] (length (self :tbl)))})
  (each nm ["HashMap" "java.util.HashMap"]
    (register-class-ctor! nm
      (fn [&opt init]
        (def tbl @{})
        (when init (each pair (hm-entries init) (put tbl (in pair 0) (in pair 1))))
        @{:jolt/type :jolt/hashmap :tbl tbl})))
  (each nm ["MapEntry" "clojure.lang.MapEntry"]
    (register-class-ctor! nm (fn [k v] [k v])))
  # (String. bytes) / (String. bytes charset): UTF-8 bytes to string.
  (each nm ["String" "java.lang.String"]
    (register-class-ctor! nm (fn [x &opt charset] (string x))))
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
  (each nm ["PushbackReader" "java.io.PushbackReader"]
    (register-class-ctor! nm (fn [rdr &opt size] (pushback-reader rdr))))
  (each nm ["BigInteger" "java.math.BigInteger"]
    (register-class-ctor! nm
      (fn [v]
        (or (scan-number (string/trim (string v)))
            (error (string "NumberFormatException: For input string: \"" v "\""))))))
  (each nm ["Locale" "java.util.Locale"]
    (register-class-ctor! nm (fn [id &opt _country] @{:jolt/type :jolt/locale :id (string id)})))
  # java.util.regex.Pattern statics: Pattern/compile, Pattern/quote, Pattern/MULTILINE.
  # Pattern/compile returns jolt's native :jolt/regex compiled value so that
  # str/replace, re-matches, .split etc accept it transparently.
  (defn- pattern-quote [s]
    (def meta "\\.[]{}()*+-?^$|&")
    (def buf @"")
    (var i 0)
    (while (< i (length s))
      (def c (s i))
      (if (string/find (string/from-bytes c) meta)
        (buffer/push buf (chr "\\")))
      (buffer/push buf (string/from-bytes c))
      (++ i))
    (string buf))
  (def pattern-multiline 8)
  (each nm ["Pattern" "java.util.regex.Pattern"]
    (register-class-statics! nm
      @{"compile" (fn [s &opt flags]
                    (if (and flags (= (band flags pattern-multiline) pattern-multiline))
                      (re-pattern (string "(?m)" s))
                      (re-pattern s)))
        "quote" (fn [s] (pattern-quote s))
        "MULTILINE" pattern-multiline}))
  # .split on compiled regex values: delegates to re-split, drops trailing empties
  (register-tagged-methods! :jolt/regex
    @{"split" (fn [self s &opt limit]
                (def parts (re-split self s))
                (while (and (> (length parts) 0) (= "" (last parts)))
                  (array/pop parts))
                parts)})
  # JVM exception constructors: (Exception. msg), (IllegalArgumentException. msg),
  # (InterruptedException. msg). Return the message string so getMessage works.
  (each nm ["Exception" "java.lang.Exception"]
    (register-class-ctor! nm (fn [msg] (string msg))))
  (each nm ["IllegalArgumentException" "java.lang.IllegalArgumentException"]
    (register-class-ctor! nm (fn [msg] (string msg))))
  (each nm ["InterruptedException" "java.lang.InterruptedException"]
    (register-class-ctor! nm (fn [msg] (string msg))))
  # Character class statics (ASCII — the engine is byte-based).
  (register-class-statics! "Character"
    @{"isUpperCase" (fn [ch] (and (>= (ch :ch) 65) (<= (ch :ch) 90)))
      "isLowerCase" (fn [ch] (and (>= (ch :ch) 97) (<= (ch :ch) 122)))})
  # java.net.URI constructor: stores the spec string, rounds-trips through str.
  (each nm ["URI" "java.net.URI"]
    (register-class-ctor! nm (fn [spec] (string spec))))
  # java.util.Date: millis-valued instants; no-arg = now.
  (defn- make-date [&opt ms]
    @{:jolt/type :jolt/date :ms (or ms (math/floor (* 1000 (os/clock :realtime))))})
  (each nm ["Date" "java.util.Date"]
    (register-class-ctor! nm make-date))
  (register-tagged-methods! :jolt/date
    @{"getTime" (fn [self] (self :ms))
      "toString" (fn [self] (string (self :ms)))})
  # java.util.TimeZone: getTimeZone(id) -> a tz value.
  (defn- make-tz [id] @{:jolt/type :jolt/tz :id (string id)})
  (register-class-statics! "TimeZone"
    @{"getTimeZone" (fn [id] (make-tz id))})
  (register-class-statics! "java.util.TimeZone"
    @{"getTimeZone" (fn [id] (make-tz id))})
  # java.text.SimpleDateFormat: minimal formatter supporting y M d H m s tokens.
  (defn- pad2 [n] (if (< n 10) (string "0" n) (string n)))
  (defn- sdf-format [pattern ms utc?]
    (def d (os/date (math/floor (/ ms 1000)) (not utc?)))
    (def out @"")
    (var i 0)
    (while (< i (length pattern))
      (def c (pattern i))
      (def k (do (var j i)
                 (while (and (< j (length pattern)) (= (pattern j) c)) (++ j))
                 (- j i)))
      (cond
        (= c (chr "y")) (do (buffer/push out (if (>= k 4) (string (d :year)) (pad2 (mod (d :year) 100)))) (+= i k))
        (= c (chr "M")) (do (buffer/push out (if (= k 1) (string (+ 1 (d :month))) (pad2 (+ 1 (d :month))))) (+= i k))
        (= c (chr "d")) (do (buffer/push out (if (= k 1) (string (+ 1 (d :month-day))) (pad2 (+ 1 (d :month-day))))) (+= i k))
        (= c (chr "H")) (do (buffer/push out (if (= k 1) (string (d :hours)) (pad2 (d :hours)))) (+= i k))
        (= c (chr "m")) (do (buffer/push out (if (= k 1) (string (d :minutes)) (pad2 (d :minutes)))) (+= i k))
        (= c (chr "s")) (do (buffer/push out (if (= k 1) (string (d :seconds)) (pad2 (d :seconds)))) (+= i k))
        (do (buffer/push out (string/from-bytes c)) (++ i))))
    (string out))
  (defn- make-sdf [pattern]
    @{:jolt/type :jolt/sdf :pattern pattern :utc true})
  (each nm ["SimpleDateFormat" "java.text.SimpleDateFormat"]
    (register-class-ctor! nm make-sdf))
  (register-tagged-methods! :jolt/sdf
    @{"setTimeZone" (fn [self tz]
                      (put self :utc (= "UTC" (get tz :id)))
                      self)
      "format" (fn [self date]
                 (sdf-format (self :pattern) (date :ms) (self :utc)))})
  # Thread stub: getContextClassLoader returns a stub so migratus jar/create code
  # that walks Thread/currentThread doesn't crash.
  (register-tagged-methods! :jolt/thread
    @{"getContextClassLoader" (fn [self] @{:jolt/type :jolt/classloader})})
  # ClassLoader degrade (jolt-hjw): there is no classpath, so getResource returns
  # nil and migratus's find-migration-dir falls through to the filesystem branch
  # (resources/<dir>). getSystemClassLoader yields the same stub.
  (each nm ["ClassLoader" "java.lang.ClassLoader"]
    (register-class-statics! nm @{"getSystemClassLoader" (fn [] @{:jolt/type :jolt/classloader})}))
  # No STM on jolt: a transaction is never running, so logging libraries that
  # gate agent-vs-direct on it (clojure.tools.logging/log*) always log directly.
  (register-class-statics! "clojure.lang.LockingTransaction" @{"isRunning" (fn [] false)})
  (register-tagged-methods! :jolt/classloader
    @{"getResource" (fn [self path] nil)
      "getResources" (fn [self path] nil)
      "getResourceAsStream" (fn [self path] nil)})
  # next.jdbc host shims (paired with the __jdbc-* builtins in core.janet and the
  # instance? Connection case in evaluator.janet). The wrapped connection carries
  # a clj :exec callback (run one SQL string) and a :close callback.
  # java.sql.Timestamp: migratus builds (Timestamp. millis) for the applied
  # column; represent it as the millis number so it stores and sorts directly.
  (each nm ["Timestamp" "java.sql.Timestamp"]
    (register-class-ctor! nm (fn [ms] ms)))
  (register-tagged-methods! :jolt/jdbc-conn
    @{"setAutoCommit" (fn [self _] self)
      "isClosed" (fn [self] (get (get self :closed) 0))
      "close" (fn [self]
                (unless (get (get self :closed) 0)
                  ((get self :close))
                  (put (get self :closed) 0 true))
                nil)
      "getMetaData" (fn [self] @{:jolt/type :jolt/jdbc-meta :product (get self :product)})})
  (register-tagged-methods! :jolt/jdbc-meta
    @{"getDatabaseProductName" (fn [self] (get self :product))})
  # Statement batch: addBatch accumulates SQL strings, executeBatch runs each via
  # the connection's clj :exec callback (which executes inside the transaction).
  (register-tagged-methods! :jolt/jdbc-stmt
    @{"addBatch" (fn [self sql] (array/push (get self :cmds) sql) nil)
      "executeBatch" (fn [self]
                       (def out @[])
                       (each c (get self :cmds) (array/push out ((get self :exec) c)))
                       out)
      "close" (fn [self] nil)})
  # java.io.File model (jolt-hjw). A :jolt/file carries its path; io/file and the
  # File. ctor build it (see core.janet's __make-file). The method surface below
  # is backed by os/ and file/. listFiles returns child :jolt/file values so
  # file-seq (File-aware) yields :jolt/file leaves migratus can call methods on.
  (defn- jfile-path [x]
    (if (and (table? x) (= :jolt/file (get x :jolt/type))) (get x :path) (string x)))
  (defn- make-jfile [path &opt child]
    @{:jolt/type :jolt/file
      :path (if child (string (jfile-path path) "/" (jfile-path child)) (jfile-path path))})
  (defn- last-slash [p]
    (var idx nil) (var i 0)
    (while (< i (length p)) (when (= (p i) 47) (set idx i)) (++ i))
    idx)
  (defn- jfile-name [p]
    (if-let [i (last-slash p)] (string/slice p (+ i 1)) p))
  (defn- jfile-abs [p]
    (if (string/has-prefix? "/" p) p (string (os/cwd) "/" p)))
  (each nm ["File" "java.io.File"]
    (register-class-ctor! nm make-jfile))
  (register-tagged-methods! :jolt/file
    @{"getPath" (fn [self] (get self :path))
      "toString" (fn [self] (get self :path))
      "getName" (fn [self] (jfile-name (get self :path)))
      "getParent" (fn [self] (let [p (get self :path)]
                               (if-let [i (last-slash p)] (string/slice p 0 i) nil)))
      "getAbsolutePath" (fn [self] (jfile-abs (get self :path)))
      "getCanonicalPath" (fn [self] (jfile-abs (get self :path)))
      "getAbsoluteFile" (fn [self] (make-jfile (jfile-abs (get self :path))))
      "exists" (fn [self] (not (nil? (os/stat (get self :path)))))
      "isFile" (fn [self] (= :file (os/stat (get self :path) :mode)))
      "isDirectory" (fn [self] (= :directory (os/stat (get self :path) :mode)))
      "canRead" (fn [self] (not (nil? (os/stat (get self :path)))))
      # listFiles: child File values, or nil when not a directory (Clojure null)
      "listFiles" (fn [self]
                    (let [p (get self :path)]
                      (when (= :directory (os/stat p :mode))
                        (map (fn [e] (make-jfile p e)) (os/dir p)))))
      "list" (fn [self]
               (let [p (get self :path)]
                 (when (= :directory (os/stat p :mode)) (os/dir p))))
      "toPath" (fn [self] @{:jolt/type :jolt/nio-path :s (get self :path)})
      "toURI" (fn [self] (string "file:" (jfile-abs (get self :path))))
      "toURL" (fn [self] @{:jolt/type :jolt/url :url (string "file:" (jfile-abs (get self :path)))})
      "delete" (fn [self] (let [r (protect (os/rm (get self :path)))] (truthy? (r 0))))
      "mkdir" (fn [self] (truthy? ((protect (os/mkdir (get self :path))) 0)))
      "mkdirs" (fn [self] (truthy? ((protect (os/mkdir (get self :path))) 0)))
      "createNewFile" (fn [self]
                        (let [p (get self :path)]
                          (if (os/stat p) false
                            (do (def f (file/open p :w)) (file/close f) true))))
      "equals" (fn [self o] (and (table? o) (= (get self :path) (get o :path))))
      "hashCode" (fn [self] (hash (get self :path)))})
  # java.nio.file degrade for migratus's script-excluded? glob check: just enough
  # of Path / FileSystem / PathMatcher to match a filename against a glob, with a
  # simple recursive * / ? matcher (no path-segment semantics — filenames only).
  (defn- glob-matches? [glob s]
    (defn m [gi si]
      (cond
        (= gi (length glob)) (= si (length s))
        (= (glob gi) 42) # *
          (or (m (+ gi 1) si)
              (and (< si (length s)) (m gi (+ si 1))))
        (and (< si (length s))
             (or (= (glob gi) 63) (= (glob gi) (s si)))) # ? or literal
          (m (+ gi 1) (+ si 1))
        false))
    (m 0 0))
  (register-tagged-methods! :jolt/nio-path
    @{"getFileSystem" (fn [self] @{:jolt/type :jolt/nio-fs})
      "toString" (fn [self] (get self :s))})
  (register-tagged-methods! :jolt/nio-fs
    @{"getPath" (fn [self s & _] @{:jolt/type :jolt/nio-path :s s})
      "getPathMatcher" (fn [self spec]
                         @{:jolt/type :jolt/nio-matcher
                           :glob (if (string/has-prefix? "glob:" spec)
                                   (string/slice spec 5) spec)})})
  (register-tagged-methods! :jolt/nio-matcher
    @{"matches" (fn [self path] (glob-matches? (get self :glob) (get path :s)))}))

(install!)
(install-io!)
