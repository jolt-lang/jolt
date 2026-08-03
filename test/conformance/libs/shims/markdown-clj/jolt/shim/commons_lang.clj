;; Stand-in for org.apache.commons.lang.StringEscapeUtils, the Apache Commons
;; Lang JVM jar that markdown-clj's suite uses as an ORACLE: the autourl tests
;; assert that unescaping markdown's own HTML output yields the raw link. The
;; jar does not exist on jolt, so the class is registered here with unescapeHtml,
;; implemented independently of markdown's escaping.
;;
;; A shim rather than an edit: changing the suite to drop its oracle would
;; silently turn the recorded tally into a measure of our own source. The
;; library under test is unmodified.
(ns jolt.shim.commons-lang)

;; The named entities commons-lang's unescapeHtml knows that are worth
;; carrying here. Markdown's autolink output exercises the hex form; the named
;; set covers what ordinary HTML escaping emits. Anything not in this map (and
;; not numeric) is left untouched, as the JVM does.
(def ^:private named
  {"amp" 38 "lt" 60 "gt" 62 "quot" 34 "apos" 39 "nbsp" 160
   "copy" 169 "reg" 174 "trade" 8482 "hellip" 8230 "bull" 8226
   "ndash" 8211 "mdash" 8212 "lsquo" 8216 "rsquo" 8217
   "ldquo" 8220 "rdquo" 8221 "middot" 183 "laquo" 171 "raquo" 187
   "deg" 176 "plusmn" 177 "frac14" 188 "frac12" 189 "frac34" 190
   "times" 215 "divide" 247})

(defn- digit [c]
  (cond (and (<= 48 c) (<= c 57)) (- c 48)
        (and (<= 97 c) (<= c 102)) (+ (- c 97) 10)
        (and (<= 65 c) (<= c 70)) (+ (- c 65) 10)
        :else nil))

(defn- parse-radix [s radix]
  (when (pos? (count s))
    (loop [i 0 v 0]
      (if (>= i (count s))
        v
        (let [d (digit (int (nth s i)))]
          ;; nil, not (reduced nil): reduced only means anything to reduce, and a
          ;; Reduced is truthy, so the caller would take it for a code point. nil
          ;; sends it down the leave-the-token-alone path, which is what
          ;; commons-lang does with an entity it cannot parse.
          (if (or (nil? d) (>= d radix))
            nil
            (recur (inc i) (+ (* v radix) d))))))))

;; Token is &…; inclusive. The body between the delimiters starts with the
;; numeric marker: &#xNN; / &#XNN; hex, &#NN; decimal, else a named entity.
(defn- entity-code [token]
  (let [body (subs token 1 (dec (count token)))]
    (cond
      (clojure.string/starts-with? body "#x") (parse-radix (subs body 2) 16)
      (clojure.string/starts-with? body "#X") (parse-radix (subs body 2) 16)
      (clojure.string/starts-with? body "#") (parse-radix (subs body 1) 10)
      :else (get named body))))

;; One pass, left to right: copy non-entity runs through, decode &…; tokens,
;; leave a lone or unrecognized & alone.
(defn- unescape* [s]
  (let [n (count s)]
    (loop [i 0 acc []]
      (if (>= i n)
        (apply str acc)
        (let [c (nth s i)]
          (if (not= c \&)
            (recur (inc i) (conj acc c))
            (let [semi (clojure.string/index-of s ";" i)]
              (if (or (nil? semi) (> (- semi i) 12))
                (recur (inc i) (conj acc c))
                (let [cp (entity-code (subs s i (inc semi)))]
                  (if cp
                    (recur (inc semi) (conj acc (char cp)))
                    (recur (inc i) (conj acc c))))))))))))

;; Registering the statics is also what makes the class name resolve, so the
;; suite's qualified call org.apache.commons.lang.StringEscapeUtils/unescapeHtml
;; compiles.
(__register-class-statics! "org.apache.commons.lang.StringEscapeUtils"
  {"unescapeHtml" (fn [s] (unescape* (str s)))})
