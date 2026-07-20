(ns jolt.time.instant
  "Instant — a nanosecond count from the epoch on the UTC timeline. atZone/atOffset
  are added by the zone namespace."
  (:require [jolt.time.impl :as impl]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]
            [jolt.time.amount :as a]
            [jolt.time.temporal :as t]))

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

(defn- parse-offset [s]
  (if (or (= s "Z") (= s "z")) 0
    (let [sign (if (= \- (nth s 0)) -1 1) h (u/digits-at s 1 2) m (or (u/digits-at s 4 2) 0)]
      (* sign (+ (* h 3600) (* m 60))))))
(defn- offset-pos [s]
  (let [tpos (or (some (fn [i] (when (#{\T \t} (nth s i)) i)) (range (count s))) 0)]
    (some (fn [i] (when (#{\Z \z \+ \-} (nth s i)) i)) (range (inc tpos) (count s)))))
(defn parse-iso-instant [s]
  (let [op (offset-pos s) local (if op (subs s 0 op) s)
        off (parse-offset (if op (subs s op) "Z"))
        ti (.indexOf local "T")
        ed (l/parse-iso-date (subs local 0 ti))
        nod (u/parse-hms->nano (subs local (inc ti)))]
    (- (+ (* ed u/nanos-per-day) nod) (* off nps))))

(defn- unit-nanos [unit]
  (let [nm (u/upper (if (and (impl/jt? unit) (= :jolt.time/chrono-unit (impl/type-of unit)))
                      (e/cu-name unit) (str unit)))]
    (or (u/chrono-unit-nanos nm) (throw (ex-info (str "no nanos for unit " nm) {})))))

(impl/register-type! :jolt.time/instant
  {:eq   (fn [a b] (= (inst-nanos a) (inst-nanos b)))
   :hash (fn [x] (inst-nanos x))
   :str  (fn [x] (iso-instant-str (inst-nanos x)))
   :cmp  (fn [a b] (compare (inst-nanos a) (inst-nanos b)))
   :classes #{"java.time.Instant" "Instant"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

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
     "compareTo"   (fn [x o] (compare (inst-nanos x) (o-nanos o)))
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
   "from"         (fn [t] (condp = (impl/type-of t)
                            :jolt.time/instant t
                            :jolt.time/local-date-time (instant (* (l/ldt->ms t) 1000000))
                            (instant (* (a/temporal-nanos t) 1) )))
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
              (throw (ex-info (str "Instant has no field " f) {})))))

;; the deferred LocalDateTime.toInstant (UTC): now that Instant exists
(__register-class-methods! :jolt.time/local-date-time
  {"toInstant" (fn [x & _] (instant (* (l/ldt->ms x) 1000000)))})

(a/register-nanos! :jolt.time/instant inst-nanos)

;; java.util.Date/from a library Instant (tick's t/inst goes through this). Builds
;; a Date via its core (#inst-layer) constructor from the instant's epoch millis.
(statics! ["Date" "java.util.Date"]
  {"from" (fn [i] (java.util.Date. (u/floor-div (inst-nanos i) 1000000)))})

;; make the #inst/Date layer's Date.toInstant etc. yield THIS instant, so a Date
;; and a library instant compare and print as one representation.
(when-let [set-ctor (resolve 'jolt.host/set-instant-ctor!)]
  ((deref set-ctor) (fn [nanos] (instant nanos))))
