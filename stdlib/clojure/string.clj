; Jolt Standard Library: clojure.string
; String manipulation functions using Jolt core string interop.

(defn blank?
  [s]
  (if (nil? s) true
    (= 0 (count (str-trim s)))))

;; The case fns and the searches take any Object s through its toString, like
;; the reference ((upper-case :kw) is ":KW", (capitalize 1) is "1"); nil throws
;; like calling a method on null.
;; A string is already its own toString, and it is what essentially every caller
;; passes. Taking the .toString anyway sent it through the whole method-dispatch
;; arm chain to reach the String surface — ~550ns on top of a ~40ns
;; string-upcase, so (upper-case "sel") spent more than 90% of its time deciding
;; how to dispatch. Every fn in this namespace coerces through here.
(defn- to-str [s]
  (cond
    (string? s) s
    (nil? s) (throw (new NullPointerException "s"))
    :else (.toString s)))
(defn capitalize
  [s]
  (let [s (to-str s)]
    (if (< 1 (count s))
      (str (str-upper (subs s 0 1))
           (str-lower (subs s 1)))
      (str-upper s))))

(defn lower-case
  [s]
  (str-lower (to-str s)))

(defn upper-case
  [s]
  (str-upper (to-str s)))

(defn includes?
  [s substr]
  (not (nil? (str-find substr (to-str s)))))

(defn join
  
  ([coll] (str-join coll))
  ([separator coll] (str-join coll separator)))

;; The reference dispatches on the match's type: a char, a CharSequence or a
;; Pattern, anything else "Invalid match arg"; a string match casts its
;; replacement to CharSequence (nil is a NullPointerException, a number a
;; ClassCastException), a regex match with a nil replacement dies applying it.
(defn- check-match [match replacement]
  (cond
    (nil? match) (throw (IllegalArgumentException. (str "Invalid match arg: " match)))
    (or (char? match) (string? match))
    (cond (nil? replacement) (throw (NullPointerException. "replacement"))
          (not (or (string? replacement) (char? replacement)))
          (throw (ClassCastException. (str "class " (.getName (class replacement)) " cannot be cast to class java.lang.CharSequence"))))
    (and (instance? java.util.regex.Pattern match) (nil? replacement)) (throw (NullPointerException. "replacement"))))

(defn replace
  [s match replacement]
  (check-match match replacement)
  (str-replace-all match replacement (to-str s)))

(defn replace-first
  [s match replacement]
  (check-match match replacement)
  (str-replace match replacement (to-str s)))

(defn reverse
  [s]
  (str-reverse-b s))

(defn split
  ([s re] (split s re 0))
  ([s re limit]
   ;; Java Pattern.split semantics: limit > 0 caps the parts (trailing empties
   ;; kept); limit < 0 splits fully and keeps trailing empties; limit 0 (the
   ;; default) splits fully then drops trailing empty strings — but a no-match
   ;; result ([input], the only 1-element case) is returned as-is.
   (let [parts (vec (str-split re s (if (pos? limit) limit nil)))]
     (if (and (zero? limit) (> (count parts) 1))
       (loop [v parts] (if (and (seq v) (= "" (peek v))) (recur (pop v)) v))
       parts))))

(defn split-lines
  "Split s on \\n or \\r\\n, returning a vector of lines."
  [s]
  ;; through split, not the raw native, for the limit-0 semantics: trailing
  ;; empty strings drop, like the reference (split-lines "a\nb\n") => ["a" "b"].
  (split s #"\r?\n"))

(defn starts-with?
  [s substr]
  (when (nil? substr) (throw (new NullPointerException "substr")))
  (let [s (to-str s)
        slen (count s) slen2 (count substr)]
    (and (>= slen slen2)
         (= (subs s 0 slen2) substr))))

(defn ends-with?
  [s substr]
  (when (nil? substr) (throw (new NullPointerException "substr")))
  (let [s (to-str s)
        slen (count s) slen2 (count substr)]
    (and (>= slen slen2)
         (= (subs s (- slen slen2)) substr))))

(defn ^String trim
  
  [s]
  (str-trim s))

(defn triml
  
  [s]
  (str-triml s))

(defn trimr
  
  [s]
  (str-trimr s))

(defn escape
  [s cmap]
  (when (nil? s) (throw (new NullPointerException "s")))
  (apply str
    (map (fn [ch]
           (if-let [rep (cmap ch)] rep (str ch)))
         s)))

(defn index-of
  "0-based index of the first occurrence of value in s, or nil."
  ([s value]
   (str-find value (to-str s)))
  ([s value from]
   ;; JVM String.indexOf clamps: negative from -> 0, from past the end -> nil
   (let [st (to-str s)
         from (min (max 0 (long from)) (count st))
         idx (str-find value (subs st from))]
     (when idx (+ from idx)))))

(defn last-index-of
  
  ([s value]
   (let [r (str-reverse-b s) sval (str-reverse-b value)
         idx (str-find sval r)]
     (when idx (- (count s) (+ idx (count value))))))
  ([s value from]
   ;; JVM lastIndexOf: largest k <= from where the match STARTS (the match may
   ;; extend past from), negative from -> nil, from past the end clamps
   (let [from (min (max -1 (long from)) (dec (count s)))
         sub (if (neg? from) "" (subs s 0 (min (count s) (+ from (count value)))))
         r (str-reverse-b sub) sval (str-reverse-b value)
         idx (str-find sval r)]
     (when idx (- (count sub) (+ idx (count value)))))))

(defn re-quote-replacement
  "Escape special characters (backslash and dollar) in a regex replacement
  string so it is used literally rather than interpreted."
  [replacement]
  (when (nil? replacement) (throw (NullPointerException. "replacement")))
  ;; str first: the JVM takes any CharSequence-able value — a regex passed as
  ;; the replacement quotes its pattern source (yamlscript's re templates).
  (apply str
    (map (fn [ch]
           (let [c (str ch)]
             (if (or (= c "\\") (= c "$")) (str "\\" c) c)))
         (seq (str replacement)))))

;; Ported from clojure.string/trim-newline (CharSequence interop replaced with
;; portable count/subs). Removes all trailing \n or \r characters.
(defn trim-newline
  "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
  [s]
  (when (nil? s) (throw (NullPointerException. "s")))
  (loop [index (count s)]
    (if (zero? index)
      ""
      (let [c (subs s (dec index) index)]
        (if (or (= c "\n") (= c "\r"))
          (recur (dec index))
          (subs s 0 index))))))
