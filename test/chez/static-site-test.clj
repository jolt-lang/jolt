;; Static member sites cache what `Class/member` resolved to, and must still see
;; every change to the registry: a member a library adds or replaces after the
;; site first ran, and a mutable static set after a site read it. Also that what
;; the uncached path raises — an unknown member, a wrong arity, arguments to a
;; field — is still raised, with the same message, from a warm site.
;; Run: bin/jolt run test/chez/static-site-test.clj (smoke.sh greps "STATIC SITES OK").
(ns static-site-test)

(def failures (atom []))
(defn chk [label ok] (when-not ok (swap! failures conj label)))
(defn msg [f] (try (f) :no-throw (catch Throwable e (ex-message e))))

;; each site is its own defn, so a cached entry is what a second call reads
(defn read-answer [] Long/SITE_ANSWER)
(defn call-twice [x] (Long/siteTwice x))
(defn call-twice-2 [] (Long/siteTwice 1 2))
(defn call-field [] (Long/MAX_VALUE))
(defn call-field-args [] (Long/MAX_VALUE 1))
(defn read-min [] Long/MIN_VALUE)
(defn read-flag [] clojure.lang.RT/checkSpecAsserts)

;; --- a member added after the site first missed --------------------------------
(chk "an unknown member raises from the site"
     (string? (msg read-answer)))
(jolt.host/extend-class! "java.lang.Long" {:statics {:SITE_ANSWER 42}})
(chk "a member added later is read by the site that missed" (= 42 (read-answer)))
(chk "...and again once the site is warm" (= 42 (read-answer)))

;; --- a member replaced after the site cached it --------------------------------
(jolt.host/extend-class! "java.lang.Long" {:statics {:SITE_ANSWER 43}})
(chk "a replaced value is seen by a warm site" (= 43 (read-answer)))

;; --- call sites ------------------------------------------------------------------
(jolt.host/extend-class! "java.lang.Long" {:statics {:siteTwice (fn [x] (* 2 x))}})
(chk "a static call through a site" (= 6 (call-twice 3)))
(chk "...warm" (= 8 (call-twice 4)))
(jolt.host/extend-class! "java.lang.Long" {:statics {:siteTwice (fn [x] (* 3 x))}})
(chk "a replaced static method is called by a warm site" (= 9 (call-twice 3)))
(chk "a wrong arity still names the method and the count"
     (= "No matching method siteTwice found taking 2 args for class java.lang.Long"
        (msg call-twice-2)))
(chk "...from a warm site too"
     (= "No matching method siteTwice found taking 2 args for class java.lang.Long"
        (msg call-twice-2)))

;; --- fields through the call form ---------------------------------------------------
(chk "(Long/MAX_VALUE) reads the field" (= 9223372036854775807 (call-field)))
(chk "...warm" (= 9223372036854775807 (call-field)))
(chk "a field given arguments still raises"
     (string? (msg call-field-args)))
(chk "a plain field read" (= -9223372036854775808 (read-min) (read-min)))

;; --- a mutable static ------------------------------------------------------------
(let [before (read-flag)]
  (jolt.host/set-static-field! "clojure.lang.RT" "checkSpecAsserts" true)
  (chk "a mutable static set after the site read it is seen" (true? (read-flag)))
  (jolt.host/set-static-field! "clojure.lang.RT" "checkSpecAsserts" before)
  (chk "...and set back" (= before (read-flag))))

(if (empty? @failures)
  (println "STATIC SITES OK")
  (doseq [f @failures] (println "FAIL:" f)))
