(ns cheshire.core
  "cheshire's public API, for scripts written for babashka, which bundles
  cheshire. Parsing goes through jolt's built-in clojure.data.json, and
  writing is done here, so the text matches what cheshire (Jackson) writes:
  keyword keys keep their namespace, a ratio is written as a double, NaN and
  the infinities as strings, a java.util.Date with :date-format (UTC), a byte
  array as base64, and :pretty in Jackson's default layout.

  The Jackson-only parts of cheshire are left out: custom encoders
  (cheshire.generate), factory options (cheshire.factory) and the binary
  Smile and CBOR formats. A value with no JSON form throws, as cheshire does."
  (:require [clojure.data.json :as json])
  (:import (java.io PushbackReader Reader StringReader StringWriter Writer)
           (java.text SimpleDateFormat)
           (java.util Base64 Date TimeZone)))

;; --- parsing ------------------------------------------------------------------

(defn- key-fn->f
  "cheshire's key-fn argument: true keywordizes, a fn is applied to each key
  string, anything else leaves the keys as strings."
  [key-fn]
  (cond (true? key-fn) keyword
        (fn? key-fn) key-fn
        (ifn? key-fn) key-fn
        :else nil))

(defn- convert
  "Applies key-fn and array-coerce-fn to a value data.json read. field is the
  key (as read, before key-fn) of the map entry v is the value of, or nil.
  array-coerce-fn is cheshire's: called with that field name, it returns the
  collection an array's elements go into."
  [v field kf coerce]
  (cond
    (map? v) (persistent!
              (reduce-kv (fn [m k x] (assoc! m (if kf (kf k) k) (convert x k kf coerce)))
                         (transient {}) v))
    (vector? v) (let [to (when coerce (coerce field))
                      xs (mapv (fn [x] (convert x nil kf coerce)) v)]
                  (if coerce (into to xs) xs))
    :else v))

(def ^:private eof (Object.))

(defn- read-one
  "The next JSON value from a PushbackReader, or eof when only whitespace is
  left. Anything after the value stays unread, as cheshire leaves it."
  [^PushbackReader r]
  (json/read r :eof-error? false :eof-value eof))

(defn- finish
  "A value read at the top level, made the way cheshire returns it: a top-level
  array is a lazy seq unless strict? (its own elements' arrays are still
  converted), and array-coerce-fn applies to a strict top-level array."
  [v kf coerce strict?]
  (cond
    (identical? v eof) nil
    (and (vector? v) (not strict?)) (let [xs (mapv (fn [x] (convert x nil kf coerce)) v)]
                                      (lazy-seq (seq xs)))
    :else (convert v nil kf coerce)))

(defn- pushback ^PushbackReader [rdr]
  (if (instance? PushbackReader rdr) rdr (PushbackReader. ^Reader rdr 64)))

(defn- string-reader
  "A reader over s, which must be a string: cheshire casts it, so a number or a
  keyword throws rather than being read as its printed form."
  [s]
  (when-not (string? s)
    (throw (ClassCastException. (str (.getName (class s)) " cannot be cast to java.lang.String"))))
  (StringReader. ^String s))

(defn- parse* [rdr key-fn array-coerce-fn strict?]
  (finish (read-one (pushback rdr)) (key-fn->f key-fn) array-coerce-fn strict?))

(defn parse-string
  "Returns the Clojure value of the JSON in string, nil for nil or blank.
  key-fn true makes the keys keywords, a fn is applied to each key.
  array-coerce-fn, called with an array's field name, returns the collection
  its elements go into. A top-level array comes back as a lazy seq."
  ([string] (parse-string string nil nil))
  ([string key-fn] (parse-string string key-fn nil))
  ([^String string key-fn array-coerce-fn]
   (when string
     (parse* (string-reader string) key-fn array-coerce-fn false))))

(defn parse-string-strict
  "parse-string, with a top-level array as a vector."
  ([string] (parse-string-strict string nil nil))
  ([string key-fn] (parse-string-strict string key-fn nil))
  ([^String string key-fn array-coerce-fn]
   (when string
     (parse* (string-reader string) key-fn array-coerce-fn true))))

(defn parse-stream
  "parse-string over a java.io.Reader: the next JSON value read from it, or nil
  at its end."
  ([rdr] (parse-stream rdr nil nil))
  ([rdr key-fn] (parse-stream rdr key-fn nil))
  ([rdr key-fn array-coerce-fn]
   (parse* rdr key-fn array-coerce-fn false)))

(defn parse-stream-strict
  "parse-stream, with a top-level array as a vector."
  ([rdr] (parse-stream-strict rdr nil nil))
  ([rdr key-fn] (parse-stream-strict rdr key-fn nil))
  ([rdr key-fn array-coerce-fn]
   (parse* rdr key-fn array-coerce-fn true)))

(defn parsed-seq
  "A lazy seq of the JSON values read one after another from a
  java.io.Reader, until its end."
  ([reader] (parsed-seq reader nil nil))
  ([reader key-fn] (parsed-seq reader key-fn nil))
  ([reader key-fn array-coerce-fn]
   (let [r (pushback reader)
         kf (key-fn->f key-fn)]
     ((fn step []
        (lazy-seq
         (let [v (read-one r)]
           (when-not (identical? v eof)
             (cons (finish v kf array-coerce-fn true) (step))))))))))

;; --- pretty printing ----------------------------------------------------------

(def default-pretty-print-options
  "cheshire's options for :pretty, the layout of Jackson's default pretty
  printer."
  {:indentation "  "
   :line-break "\n"
   :indent-arrays? false
   :indent-objects? true
   :before-array-values nil
   :after-array-values nil
   :object-field-value-separator nil})

(defn create-pretty-printer
  "A pretty printer for generate-string's :pretty, from options merged over
  default-pretty-print-options. :indentation is a string or a count of spaces."
  [options]
  (let [o (merge default-pretty-print-options options)
        ind (:indentation o)]
    (assoc o
           ::pretty-printer true
           :indentation (if (number? ind) (apply str (repeat ind \space)) (str ind)))))

(defn- pretty-printer [pretty]
  (cond (and (map? pretty) (::pretty-printer pretty)) pretty
        (map? pretty) (create-pretty-printer pretty)
        pretty (create-pretty-printer {})
        :else nil))

;; --- writing ------------------------------------------------------------------

(defn- hex4 [n] (format "%04X" n))

(defn- write-escaped
  "s as a JSON string, escaped as Jackson does: the two-character escapes for
  \\b \\f \\n \\r \\t, \\uXXXX for any other control character, and with
  escape-non-ascii? every character above U+007F as \\uXXXX, one above U+FFFF
  as its UTF-16 surrogate pair."
  [^Writer w ^String s escape-non-ascii?]
  (.write w "\"")
  ;; Characters that need no escape are written as whole runs of s, so a
  ;; surrogate pair is never split into two writes.
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
  "A map key as cheshire writes it: a keyword through key-fn when one is given,
  otherwise without its colon; anything else as its str."
  [k key-fn]
  (cond (keyword? k) (if key-fn (str (key-fn k)) (named-str k))
        (string? k) k
        :else (str k)))

(defn- date-str [^Date d date-format]
  (let [f (SimpleDateFormat. (or date-format "yyyy-MM-dd'T'HH:mm:ss'Z'"))]
    (.setTimeZone f (TimeZone/getTimeZone "UTC"))
    (.format f d)))

(defn- unencodable [x]
  (ex-info (str "Cannot JSON encode object of class: " (class x) ": " (.toString ^Object x))
           {:value x}))

(defn- write-double [^Writer w d]
  (cond (Double/isNaN d) (.write w "\"NaN\"")
        (Double/isInfinite d) (.write w (if (pos? d) "\"Infinity\"" "\"-Infinity\""))
        :else (.write w (str d))))

(defn- indent [pp level]
  (str (:line-break pp) (apply str (repeat level (:indentation pp)))))

(declare write-value)

;; level is Jackson's nesting count outside the container being written. An
;; object always adds one; an array adds one only when :indent-arrays? puts its
;; values on their own lines, so an object inside an inline array indents from
;; the array's own level.

(defn- write-object [^Writer w m opts pp level]
  (let [sep (if pp (or (:object-field-value-separator pp) " : ") ":")
        gap (fn [l] (when pp (if (:indent-objects? pp) (indent pp l) " ")))
        n (inc level)]
    (.write w "{")
    (if (empty? m)
      (when pp (.write w " "))
      (do (reduce (fn [first? [k v]]
                    (when-not first? (.write w ","))
                    (when-let [g (gap n)] (.write w ^String g))
                    (write-escaped w (key-str k (:key-fn opts)) (:escape-non-ascii opts))
                    (.write w ^String sep)
                    (write-value w v opts pp n)
                    false)
                  true m)
          (when-let [g (gap level)] (.write w ^String g))))
    (.write w "}")))

(defn- write-array [^Writer w xs opts pp level]
  (let [own-lines? (and pp (:indent-arrays? pp))
        gap (fn [l] (when pp (if own-lines? (indent pp l) " ")))
        n (if own-lines? (inc level) level)
        xs (seq xs)]
    (.write w "[")
    (if-not xs
      (when pp (.write w ^String (or (:after-array-values pp) " ")))
      (do (.write w (str (or (and pp (:before-array-values pp)) (gap n))))
          (reduce (fn [first? v]
                    (when-not first?
                      (.write w ",")
                      (when-let [g (gap n)] (.write w ^String g)))
                    (write-value w v opts pp n)
                    false)
                  true xs)
          (.write w (str (or (and pp (:after-array-values pp)) (gap level))))))
    (.write w "]")))

(defn- write-value [^Writer w x opts pp level]
  (cond
    (nil? x) (.write w "null")
    (true? x) (.write w "true")
    (false? x) (.write w "false")
    (string? x) (write-escaped w x (:escape-non-ascii opts))
    (keyword? x) (write-escaped w (named-str x) (:escape-non-ascii opts))
    (symbol? x) (write-escaped w (str x) (:escape-non-ascii opts))
    (char? x) (write-escaped w (str x) (:escape-non-ascii opts))
    (ratio? x) (write-double w (double x))
    (float? x) (write-double w (double x))
    (number? x) (.write w (str x))
    (map? x) (write-object w x opts pp level)
    (instance? java.util.Map x) (write-object w (into {} x) opts pp level)
    (bytes? x) (write-escaped w (.encodeToString (Base64/getEncoder) ^bytes x) false)
    (.isArray (class x)) (throw (unencodable x))
    (or (sequential? x) (set? x) (instance? java.util.Collection x)) (write-array w x opts pp level)
    (instance? Date x) (write-escaped w (date-str x (:date-format opts)) (:escape-non-ascii opts))
    (uuid? x) (write-escaped w (str x) false)
    :else (throw (unencodable x))))

(defn generate-stream
  "Writes obj as JSON to a java.io.Writer and returns the writer. opts as for
  generate-string."
  ([obj writer] (generate-stream obj writer nil))
  ([obj ^Writer writer opts]
   (write-value writer obj opts (pretty-printer (:pretty opts)) 0)
   (.flush writer)
   writer))

(defn generate-string
  "obj as a JSON string. opts: :pretty (true, an options map, or
  create-pretty-printer's result), :escape-non-ascii, :key-fn (applied to
  keyword keys), :date-format (a SimpleDateFormat pattern, written in UTC)."
  ([obj] (generate-string obj nil))
  ([obj opts]
   (str (generate-stream obj (StringWriter.) opts))))

(def ^{:arglists '([string] [string key-fn] [string key-fn array-coerce-fn])}
  decode
  "parse-string."
  parse-string)

(def ^{:arglists '([rdr] [rdr key-fn] [rdr key-fn array-coerce-fn])}
  decode-stream
  "parse-stream."
  parse-stream)

(def ^{:arglists '([obj] [obj opts])}
  encode
  "generate-string."
  generate-string)

(def ^{:arglists '([obj writer] [obj writer opts])}
  encode-stream
  "generate-stream."
  generate-stream)
