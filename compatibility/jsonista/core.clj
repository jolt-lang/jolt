;; jsonista.core for jolt, implementing the public API of jsonista
;; (github.com/metosin/jsonista): its names, arities and object-mapper options,
;; with output checked against jsonista 0.3.8 on the JVM. Written over
;; clojure.data.json, without Jackson.
;;
;; jsonista is Copyright (c) Metosin Oy, under the Eclipse Public License 2.0
;; (licenses/EPL-2.0.txt).

(ns jsonista.core
  "jsonista's public API, for libraries written against it (muuntaja's JSON
  format is the main one). Real jsonista is a thin layer over Jackson
  Databind and its own compiled Java classes, neither of which jolt can load.
  Reading goes through jolt's built-in clojure.data.json, and writing is done
  here, so the text matches what jsonista writes: keyword keys and values keep
  their namespace, a ratio is written as a double, NaN and the infinities as
  strings, a java.util.Date in UTC (:date-format), a java.time value in its ISO
  form, a byte array as base64, and :pretty in Jackson's default layout. A
  decoded object is a persistent hash map and an array a vector, as jsonista
  builds them.

  The mappers object-mapper returns are instances of
  com.fasterxml.jackson.databind.ObjectMapper, so code that tests one with
  instance? (muuntaja does) takes it. They are not Jackson, though, so the
  options that are Jackson's own (:factory, :encoders, a :mapper that is not
  one of these, a module other than java-collection-module) throw rather than
  be ignored. A value with no JSON form throws, as it does in jsonista."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (com.fasterxml.jackson.databind ObjectMapper)
           (java.io ByteArrayInputStream File InputStream InputStreamReader
                    IOException OutputStream OutputStreamWriter PushbackReader
                    Reader StringReader StringWriter Writer)
           (java.net URL)
           (java.text SimpleDateFormat)
           (java.util ArrayList Base64 Date HashMap TimeZone)))

;; --- mappers ------------------------------------------------------------------

;; A mapper is its options. Declaring ObjectMapper makes (instance? ObjectMapper
;; m) true for it, under the name jsonista's callers import.
(deftype JoltObjectMapper [options] ObjectMapper)

(def ^:private java-collection-module-marker ::java-collection-module)

(defn ^:no-doc java-collection-module
  "The module that decodes objects to java.util.HashMap and arrays to
  java.util.ArrayList, for object-mapper's :modules."
  []
  java-collection-module-marker)

(defn- unsupported [what]
  (throw (UnsupportedOperationException.
          (str "jsonista.core on jolt has no Jackson: " what " is not supported"))))

(defn object-mapper
  "A mapper with options, as jsonista's object-mapper takes them.

  Encoding: :pretty (Jackson's default pretty printer), :escape-non-ascii,
  :strip-nils (leave out a map entry whose value is nil or an empty string,
  collection or array, as Jackson's NON_EMPTY does), :order-by-keys,
  :date-format (a SimpleDateFormat pattern for java.util.Date, written in
  UTC; default yyyy-MM-dd'T'HH:mm:ss'Z'), :encode-key-fn (true, the default,
  writes a keyword key without its colon; false writes it as its str, colon
  and all; a fn is applied to each keyword key).

  Decoding: :decode-key-fn (true makes the keys keywords, a fn is applied to
  each key string; default false), :bigdecimals (a number with a fraction or
  an exponent as a BigDecimal).

  :mapper takes another of these mappers to start from, and :modules takes
  java-collection-module. :factory, :encoders, and any other mapper or module
  are Jackson's and throw."
  ([] (object-mapper {}))
  ([options]
   (let [{:keys [mapper modules factory encoders]} options]
     (when factory (unsupported ":factory"))
     (when (seq encoders) (unsupported ":encoders"))
     (when (and mapper (not (instance? JoltObjectMapper mapper)))
       (unsupported (str "a :mapper that is a " (.getName (class mapper)))))
     (doseq [m modules]
       (when-not (= m java-collection-module-marker)
         (unsupported (str "the module " (pr-str m)))))
     (let [base (if mapper (.-options ^JoltObjectMapper mapper) {})
           opts (merge base (dissoc options :mapper :modules))]
       (->JoltObjectMapper
        (cond-> opts
          (or (:java-collections base) (seq modules)) (assoc :java-collections true)))))))

(def ^:deprecated +default-mapper+
  "DEPRECATED: The default ObjectMapper instance."
  (object-mapper {}))

(def default-object-mapper
  "The default ObjectMapper instance."
  (object-mapper {}))

(def keyword-keys-object-mapper
  "ObjectMapper instance that uses keyword keys for maps"
  (object-mapper {:encode-key-fn true, :decode-key-fn true}))

(defn- options-of [mapper]
  (if (instance? JoltObjectMapper mapper)
    (.-options ^JoltObjectMapper mapper)
    (throw (ClassCastException.
            (str (.getName (class mapper)) " cannot be cast to com.fasterxml.jackson.databind.ObjectMapper")))))

;; --- reading ------------------------------------------------------------------

(defn- key-fn [decode-key-fn]
  (cond (true? decode-key-fn) keyword
        (fn? decode-key-fn) decode-key-fn
        :else nil))

(defn- build
  "A value data.json read, rebuilt the way jsonista's deserializers build it:
  objects as persistent hash maps (so they print in the order jsonista's do)
  and arrays as vectors, or as java.util.HashMap and ArrayList under
  java-collection-module."
  [v kf java?]
  (cond
    (map? v)
    (if java?
      (let [m (HashMap.)]
        (doseq [[k x] v] (.put m (if kf (kf k) k) (build x kf java?)))
        m)
      (persistent!
       (reduce-kv (fn [m k x] (assoc! m (if kf (kf k) k) (build x kf java?)))
                  (transient (hash-map)) v)))
    (vector? v)
    (if java?
      (ArrayList. ^java.util.Collection (mapv #(build % kf java?) v))
      (mapv #(build % kf java?) v))
    :else v))

(defn- read-json
  "The first JSON value on rdr, as jsonista reads it; anything after it is left
  unread. Jackson's parse errors are IOExceptions, and so are these."
  [^Reader rdr options]
  (let [v (try
            (json/read (PushbackReader. rdr 64) :eof-error? true :bigdec (boolean (:bigdecimals options)))
            (catch IOException e (throw e))
            (catch Exception e (throw (IOException. (ex-message e) e))))]
    (build v (key-fn (:decode-key-fn options)) (:java-collections options))))

(defn- read-closing
  "read-json, closing rdr afterwards: Jackson closes the source it read."
  [^Reader rdr options]
  (try (read-json rdr options) (finally (.close rdr))))

(defprotocol ReadValue
  (-read-value [this mapper]))

(extend-protocol ReadValue
  (Class/forName "[B")
  (-read-value [this mapper]
    (read-closing (InputStreamReader. (ByteArrayInputStream. ^bytes this) "UTF-8") (options-of mapper)))

  nil
  (-read-value [_ _])

  File
  (-read-value [this mapper]
    (read-closing (io/reader this :encoding "UTF-8") (options-of mapper)))

  URL
  (-read-value [this mapper]
    (read-closing (io/reader this :encoding "UTF-8") (options-of mapper)))

  String
  (-read-value [this mapper]
    (read-json (StringReader. this) (options-of mapper)))

  Reader
  (-read-value [this mapper]
    (read-closing this (options-of mapper)))

  InputStream
  (-read-value [this mapper]
    (read-closing (InputStreamReader. ^InputStream this "UTF-8") (options-of mapper))))

;; --- writing ------------------------------------------------------------------

(defn- hex4 [n] (format "%04X" n))

(defn- write-escaped
  "s as a JSON string, escaped as Jackson does: the two-character escapes for
  \\\" \\\\ \\b \\f \\n \\r \\t, \\uXXXX for any other control character, and
  with escape-non-ascii? every character above U+007F as \\uXXXX, one above
  U+FFFF as its UTF-16 surrogate pair."
  [^Writer w ^String s escape-non-ascii?]
  (.write w "\"")
  (let [n (.length s)
        flush (fn [from to] (when (< from to) (.write w (subs s from to))))]
    (loop [i 0 run 0]
      (if (< i n)
        (let [c (int (.charAt s i))
              esc (cond
                    (= c 34) "\\\""
                    (= c 92) "\\\\"
                    (= c 8) "\\b"
                    (= c 12) "\\f"
                    (= c 10) "\\n"
                    (= c 13) "\\r"
                    (= c 9) "\\t"
                    (< c 32) (str "\\u" (hex4 c))
                    (and escape-non-ascii? (> c 0xFFFF))
                    (let [v (- c 0x10000)]
                      (str "\\u" (hex4 (+ 0xD800 (bit-shift-right v 10)))
                           "\\u" (hex4 (+ 0xDC00 (bit-and v 0x3FF)))))
                    (and escape-non-ascii? (> c 127)) (str "\\u" (hex4 c))
                    :else nil)]
          (if esc
            (do (flush run i) (.write w ^String esc) (recur (inc i) (inc i)))
            (recur (inc i) run)))
        (flush run n))))
  (.write w "\""))

(defn- named-str
  "A keyword's text without its colon, namespace kept (:a/b is \"a/b\")."
  [k]
  (subs (str k) 1))

(defn- key-str
  "A map key as jsonista writes it. A keyword goes by :encode-key-fn: true
  drops the colon, false keeps it, a fn is applied. Anything else is its str."
  [k opts]
  (if (keyword? k)
    (let [f (:encode-key-fn opts true)]
      (cond (true? f) (named-str k)
            (fn? f) (str (f k))
            :else (str k)))
    (str k)))

(defn- date-str [^Date d date-format]
  (let [f (SimpleDateFormat. (or date-format "yyyy-MM-dd'T'HH:mm:ss'Z'"))]
    (.setTimeZone f (TimeZone/getTimeZone "UTC"))
    (.format f d)))

;; java.time values Jackson's JavaTimeModule writes as ISO strings. Matched by
;; class name, since jolt provides only some of these classes without a
;; library. A ZonedDateTime is written without its [zone] suffix, as
;; ISO_OFFSET_DATE_TIME does.
(def ^:private java-time-strings
  #{"java.time.Instant" "java.time.LocalDate" "java.time.LocalDateTime"
    "java.time.LocalTime" "java.time.OffsetDateTime" "java.time.OffsetTime"
    "java.time.ZonedDateTime" "java.time.Year" "java.time.YearMonth"
    "java.time.MonthDay" "java.time.ZoneOffset"})

(defn- java-time-str [x]
  (let [c (class x)
        n (if (class? c) (.getName ^Class c) (str c))]
    (when (contains? java-time-strings n)
      (let [s (str x)]
        (if (= n "java.time.ZonedDateTime") (str/replace s #"\[[^\]]*\]$" "") s)))))

(defn- unencodable [x]
  (IOException. (str "No serializer found for class " (.getName ^Class (class x))
                     " (jsonista.core on jolt writes Clojure data, java.util collections,"
                     " Java arrays, dates and java.time values)")))

(defn- write-double [^Writer w d]
  (cond (Double/isNaN d) (.write w "\"NaN\"")
        (Double/isInfinite d) (.write w (if (pos? d) "\"Infinity\"" "\"-Infinity\""))
        :else (.write w (str d))))

(defn- indent [level]
  (str "\n" (apply str (repeat level "  "))))

(defn- empty-value?
  "Jackson's NON_EMPTY, which :strip-nils turns on: nil, or an empty string,
  collection or array, or a map whose every value is itself empty, since
  Jackson asks a map whether anything would be left of it once its own
  entries are stripped ({:a {:b nil}} is written as {})."
  [v]
  (or (nil? v)
      (and (string? v) (= 0 (count v)))
      (and (or (map? v) (instance? java.util.Map v))
           (every? empty-value? (vals v)))
      (and (or (coll? v) (instance? java.util.Collection v))
           (empty? v))
      (and (.isArray (class v)) (= 0 (count v)))))

(declare write-value*)

(defn- write-object [^Writer w m opts pretty? level]
  (let [entries (cond->> (seq m)
                  (:strip-nils opts) (remove (fn [[_ v]] (empty-value? v)))
                  ;; keys that do not compare (:a and "a") fail as Jackson's
                  ;; sort does, with an IOException
                  (:order-by-keys opts)
                  (#(try (doall (sort-by key %))
                         (catch ClassCastException e
                           (throw (IOException. (str "Cannot order map entries by key: " (ex-message e)) e))))))
        n (inc level)]
    (when (some #(nil? (key %)) entries)
      (throw (IOException. "Null key for a Map not allowed in JSON")))
    (.write w "{")
    (if (empty? entries)
      (when pretty? (.write w " "))
      (do (reduce (fn [first? [k v]]
                    (when-not first? (.write w ","))
                    (when pretty? (.write w ^String (indent n)))
                    (write-escaped w (key-str k opts) (:escape-non-ascii opts))
                    (.write w (if pretty? " : " ":"))
                    (write-value* w v opts pretty? n)
                    false)
                  true entries)
          (when pretty? (.write w ^String (indent level)))))
    (.write w "}")))

;; Jackson's default pretty printer keeps an array's values on one line, so an
;; object inside one indents from the array's own level.
(defn- write-array [^Writer w xs opts pretty? level]
  (let [xs (seq xs)]
    (.write w "[")
    (if-not xs
      (when pretty? (.write w " "))
      (do (when pretty? (.write w " "))
          (reduce (fn [first? v]
                    (when-not first? (.write w (if pretty? ", " ",")))
                    (write-value* w v opts pretty? level)
                    false)
                  true xs)
          (when pretty? (.write w " "))))
    (.write w "]")))

(defn- write-value* [^Writer w x opts pretty? level]
  (let [esc? (:escape-non-ascii opts)]
    (cond
      (nil? x) (.write w "null")
      (true? x) (.write w "true")
      (false? x) (.write w "false")
      (string? x) (write-escaped w x esc?)
      (keyword? x) (write-escaped w (named-str x) esc?)
      (symbol? x) (write-escaped w (str x) esc?)
      (char? x) (write-escaped w (str x) esc?)
      (ratio? x) (write-double w (double x))
      (float? x) (write-double w (double x))
      (number? x) (.write w (str x))
      ;; Jackson walks a record through java.util.Map's entrySet, which a
      ;; defrecord answers with (set this): its fields in hash-set order, not
      ;; declaration order ({"b":2,"a":1} for (->R 1 2) with fields [a b]).
      (record? x) (write-object w (set x) opts pretty? level)
      (map? x) (write-object w x opts pretty? level)
      (instance? java.util.Map x) (write-object w (into {} x) opts pretty? level)
      (bytes? x) (write-escaped w (.encodeToString (Base64/getEncoder) ^bytes x) false)
      (.isArray (class x)) (write-array w (seq x) opts pretty? level)
      (or (sequential? x) (set? x) (instance? java.util.Collection x)) (write-array w x opts pretty? level)
      (instance? Date x) (write-escaped w (date-str x (:date-format opts)) esc?)
      (uuid? x) (write-escaped w (str x) false)
      :else (if-let [s (java-time-str x)]
              (write-escaped w s esc?)
              (throw (unencodable x))))))

(defn- write-to [^Writer w value mapper]
  (let [opts (options-of mapper)]
    (write-value* w value opts (boolean (:pretty opts)) 0)
    w))

(defn- write-closing
  "write-to, then flush and close w: Jackson closes the target it wrote."
  [^Writer w value mapper]
  (try (write-to w value mapper) (.flush w) (finally (.close w))))

(defprotocol WriteValue
  (-write-value [this value mapper]))

(extend-protocol WriteValue
  File
  (-write-value [this value mapper]
    (write-closing (io/writer this :encoding "UTF-8") value mapper))

  OutputStream
  (-write-value [this value mapper]
    (write-closing (OutputStreamWriter. ^OutputStream this "UTF-8") value mapper))

  Writer
  (-write-value [this value mapper]
    (write-closing this value mapper)))

;; --- public api ---------------------------------------------------------------

(defn read-value
  "Decodes a value from a JSON from anything that
  satisfies [[ReadValue]] protocol. By default,
  File, URL, String, Reader and InputStream are supported.

  To configure, pass in an ObjectMapper created with [[object-mapper]],
  see [[object-mapper]] docstring for the available options."
  ([object]
   (-read-value object default-object-mapper))
  ([object mapper]
   (-read-value object mapper)))

(defn ^String write-value-as-string
  "Encode a value as a JSON string.

  To configure, pass in an ObjectMapper created with [[object-mapper]]."
  ([object]
   (write-value-as-string object default-object-mapper))
  ([object mapper]
   (str (write-to (StringWriter.) object mapper))))

(defn write-value-as-bytes
  "Encode a value as a JSON byte-array.

  To configure, pass in an ObjectMapper created with [[object-mapper]]."
  {:tag 'bytes}
  ([object]
   (write-value-as-bytes object default-object-mapper))
  ([object mapper]
   (.getBytes ^String (write-value-as-string object mapper) "UTF-8")))

(defn write-value
  "Encode a value as JSON and write using the provided [[WriteValue]] instance.
  By default, File, OutputStream and Writer are supported.

  To configure, pass in an ObjectMapper created with [[object-mapper]],
  see [[object-mapper]] docstring for the available options."
  ([to object]
   (-write-value to object default-object-mapper))
  ([to object mapper]
   (-write-value to object mapper)))
