(ns jolt.time.zones
  "The base ZoneOffset / ZoneId surface: fixed offsets and ZoneId construction,
  which compute from epoch arithmetic alone and need no OS timezone database.
  This is the core-owned base (RFC 0008).

  Named-zone offset resolution and DST — anything that reads the OS tz database
  through jolt.host/tz-offset-seconds — lives in the jolt-lang/time library's
  fuller jolt.time.zones, which shadows this namespace when that library is on
  the load path. Without it, ZoneId/of still constructs a zone holding its id,
  but a named zone does not resolve to a real offset (it degrades to UTC)."
  (:require [clojure.string :as str]
            [jolt.time.impl :as impl]
            [jolt.time.util :as u]))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))

;; --- ZoneOffset --------------------------------------------------------------
(defn zone-offset [secs] (impl/value :jolt.time/zone-offset {:secs secs}))
(defn zo-secs [z] (impl/field z :secs))
(defn zo? [z] (= :jolt.time/zone-offset (impl/type-of z)))

(defn zo-id [secs]
  (if (zero? secs) "Z"
    (let [neg (neg? secs) a (abs secs) h (quot a 3600) m (quot (mod a 3600) 60) s (mod a 60)]
      (str (if neg "-" "+") (u/pad2 h) ":" (u/pad2 m) (if (zero? s) "" (str ":" (u/pad2 s)))))))

(defn parse-zone-offset [s]
  (let [s (str s)]
    (if (contains? #{"Z" "z" "UTC" "GMT" "+00:00"} s) 0
      (let [sign (if (= \- (nth s 0)) -1 1)
            body (if (#{\+ \-} (nth s 0)) (subs s 1) s)
            parts (str/split body #":")]
        (if (and (= 1 (count parts)) (> (count (first parts)) 2))
          (let [b (first parts)]
            (* sign (+ (* (or (parse-long (subs b 0 2)) 0) 3600)
                       (* (if (>= (count b) 4) (or (parse-long (subs b 2 4)) 0) 0) 60)
                       (if (>= (count b) 6) (or (parse-long (subs b 4 6)) 0) 0))))
          (* sign (+ (* (or (parse-long (nth parts 0)) 0) 3600)
                     (* (if (> (count parts) 1) (or (parse-long (nth parts 1)) 0) 0) 60)
                     (if (> (count parts) 2) (or (parse-long (nth parts 2)) 0) 0))))))))

(declare zone-rules)
(impl/register-type! :jolt.time/zone-offset
  {:eq (fn [a b] (= (zo-secs a) (zo-secs b))) :hash zo-secs :str (fn [z] (zo-id (zo-secs z)))
   :cmp (fn [a b] (compare (zo-secs a) (zo-secs b)))
   :classes #{"java.time.ZoneOffset" "ZoneOffset" "java.time.ZoneId" "ZoneId"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor" "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/zone-offset
  {"getId" (fn [z] (zo-id (zo-secs z))) "getTotalSeconds" zo-secs
   "getRules" (fn [z] (zone-rules (zo-id (zo-secs z)) (zo-secs z)))
   "normalized" (fn [z] z)
   "compareTo" (fn [z o] (compare (zo-secs z) (zo-secs o)))
   "equals" (fn [z o] (boolean (and (impl/jt? o) (zo? o) (= (zo-secs z) (zo-secs o)))))
   "hashCode" zo-secs "toString" (fn [z] (zo-id (zo-secs z)))})

(statics! ["ZoneOffset" "java.time.ZoneOffset"]
  {"of" (fn [s] (zone-offset (parse-zone-offset s)))
   "ofTotalSeconds" (fn [n] (zone-offset (u/->long n)))
   "ofHours" (fn [h] (zone-offset (* (u/->long h) 3600)))
   "ofHoursMinutes" (fn [h m] (zone-offset (+ (* (u/->long h) 3600) (* (u/->long m) 60))))
   "ofHoursMinutesSeconds" (fn [h m s] (zone-offset (+ (* (u/->long h) 3600) (* (u/->long m) 60) (u/->long s))))
   "UTC" (zone-offset 0) "MIN" (zone-offset (* -18 3600)) "MAX" (zone-offset (* 18 3600))})

;; --- ZoneId (construction only) ----------------------------------------------
;; A ZoneId holds its id and a fixed offset (0 for a named zone, which the base
;; cannot resolve). The library's zones.clj replaces resolve-zone with the
;; tz-database-backed version.
(defn zone-id [id off] (impl/value :jolt.time/zone-id {:id id :off off}))
(defn zid-id [z] (impl/field z :id))
(defn zid-off [z] (impl/field z :off))
(defn zid? [z] (= :jolt.time/zone-id (impl/type-of z)))

(defn- fixed-offset-zone? [id] (and (pos? (count id)) (#{\+ \-} (nth id 0))))

(defn resolve-zone
  "Any zone designator (string / ZoneId / ZoneOffset) -> [id offset]. The base
  resolves fixed offsets and UTC; a named zone holds its id at offset 0 (the
  library resolves it against the OS tz database)."
  [z]
  (cond
    (and (impl/jt? z) (zo? z)) [(zo-id (zo-secs z)) (zo-secs z)]
    (and (impl/jt? z) (zid? z)) [(zid-id z) (zid-off z)]
    :else (let [id (str z)]
            (cond
              (contains? #{"Z" "UTC" "GMT" "Etc/UTC" "Etc/GMT" "system"} id) ["Z" 0]
              (fixed-offset-zone? id) (let [s (parse-zone-offset id)] [(zo-id s) s])
              :else [id 0]))))
(defn zone-id-of [z] (let [[id off] (resolve-zone z)] (zone-id id off)))

(impl/register-type! :jolt.time/zone-id
  {:eq (fn [a b] (= (zid-id a) (zid-id b))) :hash (fn [z] (hash (zid-id z))) :str zid-id :cmp nil
   :classes #{"java.time.ZoneId" "ZoneId" "java.io.Serializable" "Serializable"}})

(__register-class-methods! :jolt.time/zone-id
  {"getId" zid-id
   "getRules" (fn [z] (zone-rules (zid-id z) (zid-off z)))
   "normalized" (fn [z] (if (and (pos? (count (zid-id z))) (#{\+ \- \Z} (nth (zid-id z) 0))) (zone-offset (zid-off z)) z))
   "getDisplayName" (fn [z & _] (zid-id z))
   "equals" (fn [z o] (boolean (and (impl/jt? o) (zid? o) (= (zid-id z) (zid-id o)))))
   "hashCode" (fn [z] (hash (zid-id z))) "toString" zid-id})

(statics! ["ZoneId" "java.time.ZoneId"]
  {"of" (fn [id & _] (zone-id-of id))
   "systemDefault" (fn [] (zone-id "Z" 0))
   "getAvailableZoneIds" (fn [] #{})})

;; --- ZoneRules (fixed offset only) -------------------------------------------
;; The base rules always report their construction offset. The library's rules
;; resolve named-zone offsets (and DST) against the tz database.
(defn zone-rules [id std] (impl/value :jolt.time/zone-rules {:id id :std std}))
(defn- zr-id [r] (impl/field r :id))
(defn- zr-std [r] (impl/field r :std))

(impl/register-type! :jolt.time/zone-rules
  {:eq (fn [a b] (= (zr-id a) (zr-id b))) :hash (fn [r] (hash (zr-id r))) :str (fn [r] (str "ZoneRules[" (zr-id r) "]")) :cmp nil
   :classes #{"java.time.zone.ZoneRules" "ZoneRules"}})

(__register-class-methods! :jolt.time/zone-rules
  {"getOffset" (fn [r & _] (zone-offset (zr-std r)))
   "isFixedOffset" (fn [r] true)
   "toString" (fn [r] (str "ZoneRules[" (zr-id r) "]"))})
