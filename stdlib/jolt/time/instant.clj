(ns jolt.time.instant
  "Instant — a nanosecond count from the epoch on the UTC timeline. atZone/atOffset
  are added by the zone namespace."
  (:require [jolt.time.impl :as impl]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]
            [jolt.time.amount :as a]
            [jolt.time.temporal :as t])
  (:refer-clojure :exclude [inst?]))

(def ^:private nps u/nanos-per-sec)

(defn instant [nanos] (impl/value :jolt.time/instant {:nanos nanos}))
(defn inst-nanos [x] (impl/field x :nanos))
(defn inst? [x] (= :jolt.time/instant (impl/type-of x)))

;; epoch nanos of ANY instant-like value. The #inst/Date layer keeps its own
;; instant representation, and tick converts a java.util.Date to one; comparing a
;; library instant against it must still work, at millisecond granularity.
(defn- o-nanos [o]
  (if (inst? o) (inst-nanos o)
    (try (* (.toEpochMilli o) 1000000) (catch Throwable _ (inst-nanos o)))))

(defn- iso-instant-str [en]
  (let [secs (u/floor-div en nps) nano (u/floor-mod en nps)
        ed (u/floor-div secs 86400) sod (u/floor-mod secs 86400)]
    (str (l/iso-date-str ed) "T"
         (u/pad2 (quot sod 3600)) ":" (u/pad2 (mod (quot sod 60) 60)) ":" (u/pad2 (mod sod 60))
         (cond (zero? nano) ""
               (zero? (mod nano 1000000)) (str "." (u/frac-fixed nano 3))
               (zero? (mod nano 1000)) (str "." (u/frac-fixed nano 6))
               :else (str "." (u/frac-fixed nano 9)))
         "Z")))

(defn- scan-offset
  "ISO_INSTANT's offset from index i, read leniently as the JDK reads it: Z, or
  +HH then optional minutes then optional seconds, all with ':' or all without.
  A minute or second past 59 is left unread. [offset-seconds end]."
  [s i]
  (cond
    (u/char-at? s i \Z) [0 (inc i)]
    (not (and (< i (count s)) (#{\+ \-} (nth s i)))) (u/parse-fail-at s i)
    :else
    (let [h (or (u/two-digits s (inc i)) (u/parse-fail-at s i))
          colon (u/char-at? s (+ i 3) \:)
          sep (if colon 1 0)
          field (fn [j] (when (or (not colon) (u/char-at? s j \:))
                          (when-let [v (u/two-digits s (+ j sep))] (when (<= v 59) v))))
          mpos (+ i 3)
          m (field mpos)
          spos (+ mpos sep 2)
          sec (when m (field spos))]
      (when (> h 23)
        (throw (u/parse-exception (str "Text '" s "' could not be parsed: Value out of range: Hour[0-23], Minute[0-59], Second[0-59]"))))
      [(* (if (= \- (nth s i)) -1 1) (+ (* h 3600) (* (or m 0) 60) (or sec 0)))
       (cond sec (+ spos sep 2) m spos :else mpos)])))

(defn parse-iso-instant [s]
  (let [[y mo d e1] (u/scan-iso-date s 0)
        _ (when-not (u/char-at? s e1 \T) (u/parse-fail-at s e1))
        [h mi sec nano e2] (u/scan-iso-time s (inc e1) true)
        [off e3] (scan-offset s e2)]
    (u/parse-end! s e3)
    ;; ISO_INSTANT reads 24:00:00 as the next midnight and 23:59:60 as :59; any
    ;; other field out of range, offset past 18 hours included, fails at index 0
    (let [[h sec day] (cond (and (= h 24) (= mi 0) (= sec 0) (= nano 0)) [0 0 1]
                            (and (= h 23) (= mi 59) (= sec 60)) [23 59 0]
                            :else [h sec 0])]
      (try (u/check-date y mo d) (u/check-time h mi sec nano)
           (catch java.time.DateTimeException _ (u/parse-fail-at s 0)))
      (when-not (<= (* -18 3600) off (* 18 3600)) (u/parse-fail-at s 0))
      (- (+ (* (+ (impl/days-from-civil y mo d) day) u/nanos-per-day) (u/hmsn->nano h mi sec nano))
         (* off nps)))))

(defn- unit-nanos [unit]
  (let [nm (u/upper (if (and (impl/jt? unit) (= :jolt.time/chrono-unit (impl/type-of unit)))
                      (e/cu-name unit) (str unit)))]
    (or (u/chrono-unit-nanos nm) (throw (ex-info (str "no nanos for unit " nm) {})))))

(impl/register-type! :jolt.time/instant
  {:eq   (fn [a b] (= (inst-nanos a) (inst-nanos b)))
   :hash (fn [x] (inst-nanos x))
   :str  (fn [x] (iso-instant-str (inst-nanos x)))
   :cmp  (fn [a b] (impl/secs-nanos-cmp (inst-nanos a) (inst-nanos b)))
   :classes #{"java.time.Instant" "Instant"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

;; clojure.core's Inst protocol, as core_instant18.clj extends it on the JVM once
;; java.time.Instant exists: inst? and inst-ms answer for an Instant from here.
(extend-protocol Inst
  java.time.Instant
  (inst-ms* [i] (u/floor-div (inst-nanos i) 1000000)))

(__register-class-methods! :jolt.time/instant
  (merge t/generic-methods
    {"getEpochSecond" (fn [x] (u/floor-div (inst-nanos x) nps))
     "getNano"     (fn [x] (u/floor-mod (inst-nanos x) nps))
     "toEpochMilli" (fn [x] (u/floor-div (inst-nanos x) 1000000))
     "plusMillis"  (fn [x n] (instant (+ (inst-nanos x) (* (u/->long n) 1000000))))
     "minusMillis" (fn [x n] (instant (- (inst-nanos x) (* (u/->long n) 1000000))))
     "plusSeconds" (fn [x n] (instant (+ (inst-nanos x) (* (u/->long n) nps))))
     "minusSeconds" (fn [x n] (instant (- (inst-nanos x) (* (u/->long n) nps))))
     "plusNanos"   (fn [x n] (instant (+ (inst-nanos x) (u/->long n))))
     "minusNanos"  (fn [x n] (instant (- (inst-nanos x) (u/->long n))))
     "isBefore"    (fn [x o] (< (inst-nanos x) (o-nanos o)))
     "isAfter"     (fn [x o] (> (inst-nanos x) (o-nanos o)))
     "compareTo"   (fn [x o] (impl/secs-nanos-cmp (inst-nanos x) (o-nanos o)))
     "equals"      (fn [x o] (= (inst-nanos x) (o-nanos o)))
     "hashCode"    (fn [x] (inst-nanos x))
     "truncatedTo" (fn [x unit] (let [d (unit-nanos unit)] (instant (* (u/floor-div (inst-nanos x) d) d))))
     "toString"    (fn [x] (iso-instant-str (inst-nanos x)))}))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))
(statics! ["Instant" "java.time.Instant"]
  {"now"          (fn [& args] (instant (* (impl/clock-millis (first args)) 1000000)))
   "ofEpochMilli" (fn [ms] (instant (* (u/->long ms) 1000000)))
   "ofEpochSecond" (fn ([s] (instant (* (u/->long s) nps)))
                       ([s nano] (instant (+ (* (u/->long s) nps) (u/->long nano)))))
   "parse"        (fn [s & _] (instant (parse-iso-instant (str s))))
   ;; the JDK reads INSTANT_SECONDS and NANO_OF_SECOND: a local date/time has
   ;; no instant, so it is the DateTimeException with get's miss as the cause
   "from"         (fn [t] (cond
                            (nil? t) (l/from-dte! "Instant" t)
                            (= :jolt.time/instant (impl/type-of t)) t
                            :else
                            (try (instant (+ (* (.getLong t java.time.temporal.ChronoField/INSTANT_SECONDS) nps)
                                             (.get t java.time.temporal.ChronoField/NANO_OF_SECOND)))
                                 (catch java.time.DateTimeException e (l/from-dte! "Instant" t e)))))
   "EPOCH"        (instant 0)
   "MIN"          (instant (* (impl/days-from-civil -999999999 1 1) 86400 nps))
   "MAX"          (instant (+ (* (impl/days-from-civil 999999999 12 31) 86400 nps) (* 86399 nps) 999999999))})

;; wire Instant into the generic temporal machinery
(t/register-plus-nanos! :jolt.time/instant (fn [x nanos] (instant (+ (inst-nanos x) nanos))))
(t/register-plus-unit! :jolt.time/instant
  (fn [x n u] (instant (+ (inst-nanos x) (if (= u "DAYS") (* n 86400 nps) (* n (u/chrono-unit-nanos u)))))))
(t/register-between! :jolt.time/instant
  (fn [u a b] (quot (- (inst-nanos b) (inst-nanos a)) (u/chrono-unit-nanos u))))
(t/register-get-field! :jolt.time/instant
  (fn [x f] (case f
              "INSTANT_SECONDS" (u/floor-div (inst-nanos x) nps)
              "NANO_OF_SECOND"  (u/floor-mod (inst-nanos x) nps)
              "MILLI_OF_SECOND" (u/floor-div (u/floor-mod (inst-nanos x) nps) 1000000)
              "MICRO_OF_SECOND" (u/floor-div (u/floor-mod (inst-nanos x) nps) 1000)
              (t/unsupported-field! f))))

;; the deferred LocalDateTime.toInstant (UTC): now that Instant exists
(__register-class-methods! :jolt.time/local-date-time
  {"toInstant" (fn [x & _] (instant (* (l/ldt->ms x) 1000000)))})

(a/register-nanos! :jolt.time/instant inst-nanos)

;; java.util.Date/from a library Instant (tick's t/inst goes through this). Builds
;; a Date via its core (#inst-layer) constructor from the instant's epoch millis.
(statics! ["Date" "java.util.Date"]
  {"from" (fn [i] (java.util.Date. (u/floor-div (inst-nanos i) 1000000)))})

;; make the #inst/Date layer's Date.toInstant etc. yield THIS instant, so a Date
;; and a library instant compare and print as one representation. The hook is
;; a static reference: jolt.host/set-instant-ctor! is defined by the host that
;; autoloads this namespace (inst-time.ss), and looking it up with `resolve` at
;; load time made every app that reaches java.time resolve a var by name — the
;; one thing `jolt build --tree-shake` cannot follow, so each kept every def and
;; the compiler image for this one form.
(jolt.host/set-instant-ctor! (fn [nanos] (instant nanos)))
